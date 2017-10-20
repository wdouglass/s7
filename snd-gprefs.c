#include "snd.h"
#include "sndlib-strings.h"

static GtkWidget *preferences_dialog = NULL, *load_path_text_widget = NULL;

static bool prefs_unsaved = false;
static char *prefs_saved_filename = NULL;
static char *include_load_path = NULL;

#define POWER_WAIT_TIME ((guint32)100)
#define POWER_INITIAL_WAIT_TIME ((guint32)500)
#define ERROR_WAIT_TIME ((guint32)5000)

#if (!HAVE_GTK_WIDGET_GET_VISIBLE)
  #define Widget_Is_Sensitive(Wid) GTK_WIDGET_SENSITIVE(Wid)
#else
  #define Widget_Is_Sensitive(Wid) gtk_widget_is_sensitive(Wid)
#endif

#define STARTUP_WIDTH 750
#define STARTUP_HEIGHT 800


typedef struct prefs_info {
  GtkWidget *label, *text, *arrow_up, *arrow_down, *arrow_right, *error, *toggle, *scale, *toggle2, *toggle3;
  GtkWidget *color, *rscl, *gscl, *bscl, *rtxt, *gtxt, *btxt, *list_menu, *radio_button;
  GtkAdjustment *adj, *radj, *gadj, *badj;
  GtkWidget **radio_buttons;
  bool got_error;
  timeout_result_t help_id, power_id, erase_id;
  const char *var_name, *saved_label;
  const char **values;
  int num_values, num_buttons;
  mus_float_t scale_max;
  GtkSizeGroup *color_texts, *color_scales;
  void (*toggle_func)(struct prefs_info *prf);
  void (*toggle2_func)(struct prefs_info *prf);
  void (*toggle3_func)(struct prefs_info *prf);
  void (*scale_func)(struct prefs_info *prf);
  void (*arrow_up_func)(struct prefs_info *prf);
  void (*arrow_down_func)(struct prefs_info *prf);
  void (*text_func)(struct prefs_info *prf);
  void (*color_func)(struct prefs_info *prf, double r, double g, double b);
  void (*reflect_func)(struct prefs_info *prf);
  void (*save_func)(struct prefs_info *prf, FILE *fd);
  const char *(*help_func)(struct prefs_info *prf);
  void (*clear_func)(struct prefs_info *prf);
  void (*revert_func)(struct prefs_info *prf);
} prefs_info;


static void prefs_set_dialog_title(const char *filename);
static void reflect_key(prefs_info *prf, const char *key_name);
static void save_key(prefs_info *prf, FILE *fd, char *(*binder)(char *key, bool c, bool m, bool x));
static void key_bind(prefs_info *prf, char *(*binder)(char *key, bool c, bool m, bool x));
static void clear_prefs_dialog_error(void);
static void scale_set_color(prefs_info *prf, color_t pixel);
static char *get_text(GtkWidget *w);
static void set_text(GtkWidget *w, const char *value);
static void post_prefs_error(const char *msg, prefs_info *data);
#ifdef __GNUC__
  static void va_post_prefs_error(const char *msg, prefs_info *data, ...) __attribute__ ((format (printf, 1, 0)));
#else
  static void va_post_prefs_error(const char *msg, prefs_info *data, ...);
#endif

/* used in snd-prefs.c */
#define GET_TOGGLE(Toggle)        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(Toggle))
#define SET_TOGGLE(Toggle, Value) set_toggle_button(Toggle, Value, false, (void *)prf)
#define GET_TEXT(Text)            get_text(Text)
#define SET_TEXT(Text, Val)       set_text(Text, Val)
#define free_TEXT(Val)            
#define TIMEOUT(Func)             g_timeout_add_full(0, ERROR_WAIT_TIME, Func, (gpointer)prf, NULL)
#define SET_SCALE(Value)          ADJUSTMENT_SET_VALUE(prf->adj, Value)
#define GET_SCALE()               (ADJUSTMENT_VALUE(prf->adj) * prf->scale_max)
#define SET_SENSITIVE(Wid, Val)   gtk_widget_set_sensitive(Wid, Val)
#define black_text(Prf)
#define red_text(Prf)
/* gdk_gc_set_foreground(Prf->label->style->black_gc, ss->red) no effect? */


static GtkWidget *make_basic_row(GtkWidget *box)
{
#if GTK_CHECK_VERSION(3, 0, 0)
  GtkWidget *row, *r;
  r = gtk_event_box_new(); /* not button! */
  gtk_widget_set_hexpand(GTK_WIDGET(r), true);
  sg_box_pack_start(GTK_BOX(box), r, false, false, 0);
  add_highlight_button_style(r);
  gtk_widget_show(r);
  
  row = gtk_hbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(r), row);
  gtk_widget_set_hexpand(GTK_WIDGET(row), true);
  gtk_widget_show(row);
#else
  GtkWidget *row;
  row = gtk_hbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);
#endif

  return(row);
}

static void set_radio_button(prefs_info *prf, int which)
{
  if ((which >= 0) && (which < prf->num_buttons))
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}


#define which_radio_button(Prf)   get_user_int_data(G_OBJECT(Prf->radio_button))

#include "snd-prefs.c"


static void sg_entry_set_text(GtkEntry* entry, const char *text)
{
  if (text)
    gtk_entry_set_text(entry, (gchar *)text);
  else gtk_entry_set_text(entry, " ");
}


static void set_text(GtkWidget *w, const char *value)
{
  if (GTK_IS_ENTRY(w))
    sg_entry_set_text(GTK_ENTRY(w), value);
  else
    {
      if (GTK_IS_ENTRY(BIN_CHILD(w)))
	sg_entry_set_text(GTK_ENTRY(BIN_CHILD(w)), value);
    }
}


static char *get_text(GtkWidget *w)
{
  if (GTK_IS_ENTRY(w))
    return((char *)gtk_entry_get_text(GTK_ENTRY(w)));
  if (GTK_IS_ENTRY(BIN_CHILD(w)))
    return((char *)gtk_entry_get_text(GTK_ENTRY(BIN_CHILD(w))));
  return(NULL);
}

  


/* ---------------- help strings ---------------- */

static bool prefs_dialog_error_is_posted = false;

static void post_prefs_dialog_error(const char *message, void *data)
{
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), (char *)message);
  prefs_dialog_error_is_posted = (bool)message;
}


static void clear_prefs_dialog_error(void)
{
  if (prefs_dialog_error_is_posted)
    {
      prefs_dialog_error_is_posted = false;
      post_prefs_dialog_error(NULL, NULL);
    }
}


static void prefs_change_callback(GtkWidget *w, gpointer context)
{
  prefs_unsaved = true;
  prefs_set_dialog_title(NULL);
  clear_prefs_dialog_error();
}


static GtkSizeGroup *label_group;
static GtkSizeGroup *widgets_group;


static color_info *rscl_color, *gscl_color, *bscl_color;

#define PACK_1 true
#define PACK_2 false


/* ---------------- row (main) label widget ---------------- */

static GtkWidget *make_row_label(prefs_info *prf, const char *label, GtkWidget *box)
{
  GtkWidget *w;

  w = gtk_label_new(label);
#if (!GTK_CHECK_VERSION(3, 0, 0))
  gtk_misc_set_alignment(GTK_MISC(w), 1.0, 0.0);
#else
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_widget_set_halign(GTK_WIDGET(w), GTK_ALIGN_END);
#else
  gtk_misc_set_alignment(GTK_MISC(w), 1.0, 0.5);
#endif
#endif
  gtk_size_group_add_widget(label_group, w);
  sg_box_pack_start(GTK_BOX(box), w, PACK_1, PACK_2, 0);
  gtk_widget_show(w);
  prf->saved_label = label;

  return(w);
}


/* ---------------- row inner label widget ---------------- */

static void make_row_inner_label(prefs_info *prf, const char *label, GtkWidget *box)
{
  GtkWidget *w;
  w = gtk_label_new(label);
  sg_box_pack_start(GTK_BOX(box), w, false, false, 4);
  gtk_widget_show(w);
}


/* ---------------- row middle separator widget ---------------- */

static void make_row_middle_separator(GtkWidget *box)
{
  GtkWidget *w;
  w = gtk_vseparator_new();
  sg_box_pack_start(GTK_BOX(box), w, false, false, 10);
  gtk_widget_show(w);
}


/* ---------------- row inner separator widget ---------------- */

static void make_row_inner_separator(int width, GtkWidget *box)
{
  GtkWidget *w;
  w = gtk_hseparator_new();
  sg_box_pack_start(GTK_BOX(box), w, false, false, width);
  gtk_widget_show(w);
}


/* ---------------- row toggle widget ---------------- */

static GtkWidget *make_row_toggle_with_label(prefs_info *prf, bool current_value, GtkWidget *box, const char *label)
{
  GtkWidget *w;
  if (label)
    w = gtk_check_button_new_with_label(label);
  else w = gtk_check_button_new();
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), current_value);
  sg_box_pack_start(GTK_BOX(box), w, false, false, 0); /* was 10 */
  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(w, "toggled", prefs_change_callback, NULL);
  return(w);
}


static GtkWidget *make_row_toggle(prefs_info *prf, bool current_value, GtkWidget *box)
{
  return(make_row_toggle_with_label(prf, current_value, box, NULL));
}


/* ---------------- error widget ---------------- */

static GtkWidget *make_row_error(prefs_info *prf, GtkWidget *box)
{
  GtkWidget *w;

  w = gtk_label_new("");
  sg_box_pack_end(GTK_BOX(box), w, true, false, 0);
  gtk_widget_show(w);
  return(w);
}


/* ---------------- row arrows ---------------- */

static gboolean remove_arrow_func(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->power_id != 0)
    {
      g_source_remove(prf->power_id);
      prf->power_id = 0;
    }
  return(false);
}


static gint arrow_func_up(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (Widget_Is_Sensitive(prf->arrow_up))
    {
      if (prf->arrow_up_func)
	{
	  (*(prf->arrow_up_func))(prf);
	  prf->power_id = g_timeout_add_full(0,
					     POWER_WAIT_TIME,
					     arrow_func_up,
					     (gpointer)prf, NULL);
	}
      else prf->power_id = 0;
    }
  return(0);
}


static gint arrow_func_down(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (Widget_Is_Sensitive(prf->arrow_down))
    {
      if (prf->arrow_down_func)
	{
	  (*(prf->arrow_down_func))(prf);
	  prf->power_id = g_timeout_add_full(0,
					     POWER_WAIT_TIME,
					     arrow_func_down,
					     (gpointer)prf, NULL);
	}
      else prf->power_id = 0;
    }
  return(0);
}


static gboolean call_arrow_down_press(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->arrow_down_func))
    {
      (*(prf->arrow_down_func))(prf);
      if (Widget_Is_Sensitive(w))
	prf->power_id = g_timeout_add_full(0,
					   POWER_INITIAL_WAIT_TIME,
					   arrow_func_down,
					   (gpointer)prf, NULL);
      else prf->power_id = 0;
    }
  return(false);
}


static gboolean call_arrow_up_press(GtkWidget *w, GdkEventButton *ev, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->arrow_up_func))
    {
      (*(prf->arrow_up_func))(prf);
      if (Widget_Is_Sensitive(w))
	prf->power_id = g_timeout_add_full(0,
					   POWER_INITIAL_WAIT_TIME,
					   arrow_func_up,
					   (gpointer)prf, NULL);
      else prf->power_id = 0;
    }
  return(false);
}


static GtkWidget *make_row_arrows(prefs_info *prf, GtkWidget *box)
{
  GtkWidget *ev_up, *ev_down, *up, *down;
#if GTK_CHECK_VERSION(3, 14, 0)
  GtkIconTheme *icon_theme; 
  icon_theme = gtk_icon_theme_get_default();
#endif

  ev_down = gtk_event_box_new();
  sg_box_pack_start(GTK_BOX(box), ev_down, false, false, 0);
  gtk_widget_show(ev_down);

#if GTK_CHECK_VERSION(3, 14, 0)
  down = gtk_image_new_from_pixbuf(gtk_icon_theme_load_icon(icon_theme, "pan-down-symbolic", 16, (GtkIconLookupFlags)0, NULL)); 
#else
  down = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_ETCHED_OUT);
#endif
  gtk_container_add(GTK_CONTAINER(ev_down), down);
  gtk_widget_show(down);

  ev_up = gtk_event_box_new();
  sg_box_pack_start(GTK_BOX(box), ev_up, false, false, 0);
  gtk_widget_show(ev_up);

#if GTK_CHECK_VERSION(3, 14, 0)
  up = gtk_image_new_from_pixbuf(gtk_icon_theme_load_icon(icon_theme, "pan-up-symbolic", 16, (GtkIconLookupFlags)0, NULL)); 
#else
  up = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_ETCHED_OUT);
#endif
  gtk_container_add(GTK_CONTAINER(ev_up), up);
  gtk_widget_show(up);

  prf->arrow_up = up;
  prf->arrow_down = down;

  SG_SIGNAL_CONNECT(ev_down, "button_press_event", call_arrow_down_press, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_down, "button_release_event", remove_arrow_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_up, "button_press_event", call_arrow_up_press, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_up, "button_release_event", remove_arrow_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_down, "button_press_event", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(ev_up, "button_press_event", prefs_change_callback, NULL);

  return(up);
}


/* ---------------- bool row ---------------- */

static void call_toggle_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle_func))
    (*(prf->toggle_func))(prf);
}


static prefs_info *prefs_row_with_toggle(const char *label, const char *varname, bool current_value,
					 GtkWidget *box,
					 void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);

  return(prf);
}


/* ---------------- two toggles ---------------- */

static void call_toggle2_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle2_func))
    (*(prf->toggle2_func))(prf);
}


static prefs_info *prefs_row_with_two_toggles(const char *label, const char *varname, 
					      const char *label1, bool value1,
					      const char *label2, bool value2,
					      GtkWidget *box,
					      void (*toggle_func)(prefs_info *prf),
					      void (*toggle2_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *row, *hb;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->toggle2_func = toggle2_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->toggle = make_row_toggle_with_label(prf, value1, hb, label1);
  make_row_inner_separator(20, hb);
  prf->toggle2 = make_row_toggle_with_label(prf, value2, hb, label2);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle2, "toggled", call_toggle2_func, (gpointer)prf);

  return(prf);
}



/* ---------------- toggle with text ---------------- */

/* see commentary in snd-xprefs.c */


typedef struct {
  bool text_focussed, text_changed;
  prefs_info *prf;
} text_info;


static void text_change_callback(GtkWidget *w, gpointer context)
{
  text_info *data = (text_info *)context;
  if (data->text_focussed) /* try to omit non-user actions that change the value */
    data->text_changed = true;
}


static void text_activate_callback(GtkWidget *w, gpointer context)
{
  text_info *data = (text_info *)context;
  data->text_changed = false;
}


static void text_grab_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer context)
{
  text_info *data = (text_info *)context;
  if (with_pointer_focus(ss))
    goto_window(w);
  data->text_focussed = true;
}


static void text_lose_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer context)
{
  text_info *data = (text_info *)context;
  if ((data->text_focussed) &&
      (data->text_changed) &&
      (data->prf) &&
      (data->prf->text_func))
    {
      (*(data->prf->text_func))(data->prf);
      data->text_changed = false;
    }
}


static void call_text_func(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}


static GtkWidget *make_row_text(prefs_info *prf, const char *text_value, int cols, GtkWidget *box)
{
  int len;
  GtkWidget *w;
  GtkSettings *settings;
  text_info *info;

  len = mus_strlen(text_value);
  w = gtk_entry_new();
  gtk_entry_set_has_frame(GTK_ENTRY(w), true);
  if (text_value) sg_entry_set_text(GTK_ENTRY(w), text_value);
  gtk_entry_set_has_frame(GTK_ENTRY(w), false);
  if (cols > 0)
    gtk_entry_set_width_chars(GTK_ENTRY(w), cols);
  else
    {
      if (len > 24) /* sigh... */
	gtk_entry_set_width_chars(GTK_ENTRY(w), len);
    }
  gtk_editable_set_editable(GTK_EDITABLE(w), true);
  settings = gtk_widget_get_settings(w);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);
  sg_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);

  info = (text_info *)calloc(1, sizeof(text_info));
  info->prf = prf;

  SG_SIGNAL_CONNECT(w, "enter_notify_event", text_grab_focus_callback, (gpointer)info);
  SG_SIGNAL_CONNECT(w, "leave_notify_event", text_lose_focus_callback, (gpointer)info);
  SG_SIGNAL_CONNECT(w, "changed", text_change_callback, (gpointer)info);
  SG_SIGNAL_CONNECT(w, "activate", text_activate_callback, (gpointer)info);

  return(w);
}


static prefs_info *prefs_row_with_toggle_with_text(const char *label, const char *varname, bool current_value,
						   const char *text_label, const char *text_value, int cols,
						   GtkWidget *box,
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);
  make_row_inner_separator(16, hb);
  make_row_inner_label(prf, text_label, hb);
  prf->text= make_row_text(prf, text_value, cols, hb);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}


static prefs_info *prefs_row_with_toggle_with_two_texts(const char *label, const char *varname, bool current_value,
							const char *label1, const char *text1, 
							const char *label2, const char *text2, int cols,
							GtkWidget *box,
							void (*toggle_func)(prefs_info *prf),
							void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;
  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);
  make_row_inner_separator(16, hb);
  make_row_inner_label(prf, label1, hb);
  prf->text= make_row_text(prf, text1, cols, hb);
  make_row_inner_label(prf, label2, hb);
  prf->rtxt= make_row_text(prf, text2, cols, hb);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "activate", call_text_func, (gpointer)prf);

  return(prf);
}



/* ---------------- text with toggle ---------------- */

static prefs_info *prefs_row_with_text_with_toggle(const char *label, const char *varname, bool current_value,
						   const char *toggle_label, const char *text_value, int cols,
						   GtkWidget *box,
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->text = make_row_text(prf, text_value, cols, hb);
  make_row_inner_separator(8, hb);
  make_row_inner_label(prf, toggle_label, hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);  
  
  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}


/* ---------------- text with three toggle ---------------- */

static prefs_info *prefs_row_with_text_and_three_toggles(const char *label, const char *varname,
							 const char *text_label, int cols,
							 const char *toggle1_label, const char *toggle2_label, const char *toggle3_label,
							 const char *text_value, 
							 bool toggle1_value, bool toggle2_value, bool toggle3_value,
							 GtkWidget *box,
							 void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->text_func = text_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  make_row_inner_label(prf, text_label, hb);
  prf->text = make_row_text(prf, text_value, cols, hb);
  make_row_inner_separator(12, hb);
  make_row_inner_label(prf, toggle1_label, hb);
  prf->toggle = make_row_toggle(prf, toggle1_value, hb);  
  make_row_inner_separator(4, hb);
  make_row_inner_label(prf, toggle2_label, hb);
  prf->toggle2 = make_row_toggle(prf, toggle2_value, hb);  
  make_row_inner_separator(4, hb);
  make_row_inner_label(prf, toggle3_label, hb);
  prf->toggle3 = make_row_toggle(prf, toggle3_value, hb);  
  
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  return(prf);
}


/* ---------------- radio row ---------------- */

static void call_radio_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle_func))
    {
      prf->radio_button = w;
      (*(prf->toggle_func))(prf);
    }
}


static GtkWidget *make_row_radio_box(prefs_info *prf,
				     const char **labels, int num_labels, int current_value,
				     GtkWidget *box)
{
  GtkWidget *w, *current_button;
  int i;

  w = gtk_hbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);

  prf->radio_buttons = (GtkWidget **)calloc(num_labels, sizeof(GtkWidget *));
  prf->num_buttons = num_labels;

  for (i = 0; i < num_labels; i++)
    {
      if (i == 0)
	current_button = gtk_radio_button_new_with_label(NULL, labels[i]);
      else current_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(prf->radio_buttons[0])), labels[i]);
      prf->radio_buttons[i] = current_button;
      sg_box_pack_start(GTK_BOX(w), current_button, false, false, 0);
      set_user_int_data(G_OBJECT(current_button), i);
      gtk_widget_show(current_button);
      SG_SIGNAL_CONNECT(current_button, "clicked", call_radio_func, (gpointer)prf);
      SG_SIGNAL_CONNECT(current_button, "clicked", prefs_change_callback, NULL);
    }

  if (current_value != -1)
    set_toggle_button(prf->radio_buttons[current_value], true, false, (void *)prf);

  return(w);
}
  

static prefs_info *prefs_row_with_radio_box(const char *label, const char *varname, 
					    const char **labels, int num_labels, int current_value,
					    GtkWidget *box,
					    void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;
  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);

  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->toggle = make_row_radio_box(prf, labels, num_labels, current_value, hb);

  return(prf);
}


static prefs_info *prefs_row_with_radio_box_and_number(const char *label, const char *varname, 
						       const char **labels, int num_labels, int current_value,
						       const char *text_value, int text_cols,
						       GtkWidget *box,
						       void (*toggle_func)(prefs_info *prf),
						       void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
						       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *row, *hb;
  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);

  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->toggle = make_row_radio_box(prf, labels, num_labels, current_value, hb);
  prf->text = make_row_text(prf, text_value, text_cols, hb);
  prf->arrow_up = make_row_arrows(prf, hb);

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}



/* ---------------- scale row ---------------- */

static void call_scale_func(GtkAdjustment *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->scale_func))
    (*(prf->scale_func))(prf);
}


static void call_scale_text_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}


static void prefs_scale_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  float_to_textfield(prf->text, ADJUSTMENT_VALUE(prf->adj) * prf->scale_max);
}


static prefs_info *prefs_row_with_scale(const char *label, const char *varname, 
					mus_float_t max_val, mus_float_t current_value,
					GtkWidget *box,
					void (*scale_func)(prefs_info *prf),
					void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;
  char *str;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->scale_max = max_val;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  
  str = (char *)calloc(12, sizeof(char));
  snprintf(str, 12, "%.3f", current_value);
  prf->text = make_row_text(prf, str, 6, hb);
  free(str);

  prf->adj = (GtkAdjustment *)gtk_adjustment_new(current_value /max_val, 0.0, 1.01, 0.001, 0.01, .01);
  prf->scale = gtk_hscale_new(GTK_ADJUSTMENT(prf->adj));
  sg_box_pack_start(GTK_BOX(hb), prf->scale, true, true, 4);
  gtk_widget_show(prf->scale);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->scale)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->scale), false);

  prf->scale_func = scale_func;
  prf->text_func = text_func;

  SG_SIGNAL_CONNECT(prf->scale, "value_changed", call_scale_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->scale, "value_changed", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(prf->scale, "value_changed", prefs_scale_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_scale_text_func, (gpointer)prf);
  return(prf);
}


/* ---------------- text row ---------------- */

static prefs_info *prefs_row_with_text(const char *label, const char *varname, const char *value,
				       GtkWidget *box,
				       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->text = make_row_text(prf, value, 0, hb);

  prf->text_func = text_func;
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}


/* ---------------- two texts in a row ---------------- */

static prefs_info *prefs_row_with_two_texts(const char *label, const char *varname,
					    const char *label1, const char *text1, const char *label2, const char *text2, int cols,
					    GtkWidget *box,
					    void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;
  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  make_row_inner_label(prf, label1, hb);
  prf->text = make_row_text(prf, text1, cols, hb);
  make_row_inner_label(prf, label2, hb);  
  prf->rtxt = make_row_text(prf, text2, cols, hb);

  prf->text_func = text_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "activate", call_text_func, (gpointer)prf);

  return(prf);
}


/* ---------------- number row ---------------- */

static prefs_info *prefs_row_with_number(const char *label, const char *varname, const char *value, int cols,
					 GtkWidget *box,
 					 void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
					 void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);

  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);
  prf->text = make_row_text(prf, value, cols, hb);
  prf->arrow_up = make_row_arrows(prf, hb);
  prf->error = make_row_error(prf, hb);

  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  return(prf);
}


/* ---------------- list row ---------------- */


static prefs_info *prefs_row_with_list(const char *label, const char *varname, const char *value,
				       const char **values, int num_values,
				       GtkWidget *box,
				       void (*text_func)(prefs_info *prf),
				       char *(*completion_func)(widget_t w, const char *text, void *context), void *completion_context)
{
  int i;
  prefs_info *prf = NULL;
  GtkWidget *hb, *row;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);  
  
#if GTK_CHECK_VERSION(3, 0, 0)
  prf->text = gtk_combo_box_text_new_with_entry();
  for (i = 0; i < num_values; i++)
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(prf->text), (values[i]) ? values[i] : " ");
#else
  prf->text = gtk_combo_box_entry_new_text();
  for (i = 0; i < num_values; i++)
    gtk_combo_box_append_text(GTK_COMBO_BOX(prf->text), (values[i]) ? values[i] : " ");
#endif
  sg_entry_set_text(GTK_ENTRY(BIN_CHILD(prf->text)), value);
  sg_box_pack_start(GTK_BOX(hb), prf->text, false, false, 4);
  gtk_widget_show(prf->text);

  prf->error = make_row_error(prf, hb);

  prf->text_func = text_func;
  SG_SIGNAL_CONNECT(prf->text, "changed", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "changed", prefs_change_callback, NULL);
  return(prf);
}



/* ---------------- color selector row(s) ---------------- */

static void pixel_to_rgb(color_t pix, double *r, double *g, double *b)
{
  (*r) = rgb_to_float(pix->red);
  (*g) = rgb_to_float(pix->green);
  (*b) = rgb_to_float(pix->blue);
}


#if GTK_CHECK_VERSION(3, 0, 0)
static void display_color(GtkWidget *w, color_t pixel)
{
#if GTK_CHECK_VERSION(3, 92, 1)
  return;
#else
  cairo_t *cr;
#if GTK_CHECK_VERSION(3, 22, 0)
  GdkWindow *window;
  GdkDrawingContext *context;
  window = WIDGET_TO_WINDOW(w);
  context = gdk_window_begin_draw_frame(window, gdk_window_get_visible_region(window));
  cr = gdk_drawing_context_get_cairo_context(context);
#else
  cr = gdk_cairo_create(WIDGET_TO_WINDOW(w));
#endif
  cairo_set_source_rgba(cr, pixel->red, pixel->green, pixel->blue, pixel->alpha);
  cairo_rectangle(cr, 0, 0, widget_width(w), widget_height(w));
  cairo_fill(cr);
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame(window, context);
#else
  cairo_destroy(cr);
#endif
#endif
}
#endif


static void scale_set_color(prefs_info *prf, color_t pixel)
{
  double r = 0.0, g = 0.0, b = 0.0;
  pixel_to_rgb(pixel, &r, &g, &b);
  float_to_textfield(prf->rtxt, r);
  ADJUSTMENT_SET_VALUE(prf->radj, r);
  float_to_textfield(prf->gtxt, g);
  ADJUSTMENT_SET_VALUE(prf->gadj, g);
  float_to_textfield(prf->btxt, b);
  ADJUSTMENT_SET_VALUE(prf->badj, b);
#if GTK_CHECK_VERSION(3, 0, 0)
  display_color(prf->color, pixel);
#else
  widget_modify_bg(prf->color, GTK_STATE_NORMAL, pixel);
#endif
}


static void reflect_color(prefs_info *prf)
{
  mus_float_t r, g, b;
  color_info *current_color;

  r = ADJUSTMENT_VALUE(prf->radj);
  g = ADJUSTMENT_VALUE(prf->gadj);
  b = ADJUSTMENT_VALUE(prf->badj);

  current_color = rgb_to_color(r, g, b);
#if GTK_CHECK_VERSION(3, 0, 0)
  display_color(prf->color, current_color);
#else
  widget_modify_bg(prf->color, GTK_STATE_NORMAL, current_color); 
#endif

  float_to_textfield(prf->rtxt, r);
  float_to_textfield(prf->gtxt, g);
  float_to_textfield(prf->btxt, b);
}


static void prefs_color_callback(GtkWidget *w, gpointer context)
{
  reflect_color((prefs_info *)context);
}


static gint unpost_color_error(gpointer data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = false;
  gtk_label_set_text(GTK_LABEL(prf->label), prf->saved_label);
  reflect_color(prf);
  return(0);
}


static void errors_to_color_text(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  gtk_label_set_text(GTK_LABEL(prf->label), msg);
  TIMEOUT(unpost_color_error);
}


static void prefs_r_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  double r;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  redirect_errors_to(errors_to_color_text, context);
  r = (double)string_to_mus_float_t(str, 0.0, "red amount");
  redirect_errors_to(NULL, NULL);
  if (!(prf->got_error))
    {
      ADJUSTMENT_SET_VALUE(prf->radj, (double)mus_fclamp(0.0, r, 1.0));
      reflect_color(prf);
    }
}


static void prefs_g_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  double r;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  redirect_errors_to(errors_to_color_text, context);
  r = (double)string_to_mus_float_t(str, 0.0, "green amount");
  redirect_errors_to(NULL, NULL);
  if (!(prf->got_error))
    {
      ADJUSTMENT_SET_VALUE(prf->gadj, (double)mus_fclamp(0.0, r, 1.0));
      reflect_color(prf);
    }
}


static void prefs_b_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  double r;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  redirect_errors_to(errors_to_color_text, context);
  r = (double)string_to_mus_float_t(str, 0.0, "blue amount");
  redirect_errors_to(NULL, NULL);
  if (!(prf->got_error))
    {
      ADJUSTMENT_SET_VALUE(prf->badj, (double)mus_fclamp(0.0, r, 1.0));
      reflect_color(prf);
    }
}


static void prefs_call_color_func_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->color_func))
    {
      double r, g, b;
      r = ADJUSTMENT_VALUE(prf->radj);
      g = ADJUSTMENT_VALUE(prf->gadj);
      b = ADJUSTMENT_VALUE(prf->badj);
      (*(prf->color_func))(prf, r, g, b);
    }
}


#if GTK_CHECK_VERSION(3, 0, 0)
static gboolean drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  prefs_info *prf = (prefs_info *)data;
  mus_float_t r, g, b;
  color_info *current_color;

  r = ADJUSTMENT_VALUE(prf->radj);
  g = ADJUSTMENT_VALUE(prf->gadj);
  b = ADJUSTMENT_VALUE(prf->badj);

  current_color = rgb_to_color(r, g, b);
  display_color(prf->color, current_color);
  return(false);
}
#endif


static prefs_info *prefs_color_selector_row(const char *label, const char *varname, 
					    color_t current_pixel,
					    GtkWidget *box,
					    void (*color_func)(prefs_info *prf, double r, double g, double b))
{
  prefs_info *prf = NULL;
  GtkWidget *hb, *row, *row2, *sep3;
  double r = 0.0, g = 0.0, b = 0.0;

  prf = (prefs_info *)calloc(1, sizeof(prefs_info));
  prf->var_name = varname;
  pixel_to_rgb(current_pixel, &r, &g, &b);

  /* first row */
  row = make_basic_row(box);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  sg_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  make_row_middle_separator(hb);    

  {
    GtkWidget *frame;
    frame = gtk_frame_new(NULL);
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
    prf->color_texts = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
    sg_box_pack_start(GTK_BOX(hb), frame, false, false, 4);
    gtk_size_group_add_widget(prf->color_texts, frame);

    prf->color = gtk_drawing_area_new();
    gtk_container_add(GTK_CONTAINER(frame), prf->color);

#if GTK_CHECK_VERSION(3, 0, 0)
    SG_SIGNAL_CONNECT(prf->color, DRAW_SIGNAL, drawer_expose, prf);
    gtk_widget_set_hexpand(GTK_WIDGET(prf->color), true);
    gtk_widget_set_vexpand(GTK_WIDGET(prf->color), true);
#else
    widget_modify_bg(prf->color, GTK_STATE_NORMAL, current_pixel);
#endif
    gtk_widget_show(prf->color);
    gtk_widget_show(frame);
  }
  
  make_row_inner_separator(8, hb);
  
  prf->rtxt = make_row_text(prf, NULL, 6, hb);
  gtk_size_group_add_widget(prf->color_texts, prf->rtxt);
  float_to_textfield(prf->rtxt, r);

  prf->gtxt = make_row_text(prf, NULL, 6, hb);
  gtk_size_group_add_widget(prf->color_texts, prf->gtxt);
  float_to_textfield(prf->gtxt, g);

  prf->btxt = make_row_text(prf, NULL, 6, hb);
  gtk_size_group_add_widget(prf->color_texts, prf->btxt);
  float_to_textfield(prf->btxt, b);

  /* second row */

  row2 = gtk_hbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(box), row2, false, false, 0);
  gtk_widget_show(row2);

  make_row_inner_separator(20, row2);

  prf->radj = (GtkAdjustment *)gtk_adjustment_new(r, 0.0, 1.01, 0.001, 0.01, .01);
  prf->rscl = gtk_hscale_new(GTK_ADJUSTMENT(prf->radj));
  gtk_widget_set_name(prf->rscl, "prefs_color_scale");
  sg_box_pack_start(GTK_BOX(row2), prf->rscl, true, true, 4);
#if (!GTK_CHECK_VERSION(3, 0, 0))
  widget_modify_bg(prf->rscl, GTK_STATE_NORMAL, rscl_color);   /* this is the slider except when clicked */
  widget_modify_bg(prf->rscl, GTK_STATE_PRELIGHT, rscl_color); /* this is the slider when clicked */
#else
  add_red_scale_style(prf->rscl);
#endif
  gtk_widget_show(prf->rscl);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->rscl)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->rscl), false);

  prf->gadj = (GtkAdjustment *)gtk_adjustment_new(g, 0.0, 1.01, 0.001, 0.01, .01);
  prf->gscl = gtk_hscale_new(GTK_ADJUSTMENT(prf->gadj));
  gtk_widget_set_name(prf->gscl, "prefs_color_scale");
  sg_box_pack_start(GTK_BOX(row2), prf->gscl, true, true, 4);
#if (!GTK_CHECK_VERSION(3, 0, 0))
  widget_modify_bg(prf->gscl, GTK_STATE_NORMAL, gscl_color);
  widget_modify_bg(prf->gscl, GTK_STATE_PRELIGHT, gscl_color);
#else
  add_green_scale_style(prf->gscl);
#endif
  gtk_widget_show(prf->gscl);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->gscl)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->gscl), false);

  prf->badj = (GtkAdjustment *)gtk_adjustment_new(b, 0.0, 1.01, 0.001, 0.01, .01);
  prf->bscl = gtk_hscale_new(GTK_ADJUSTMENT(prf->badj));
  gtk_widget_set_name(prf->bscl, "prefs_color_scale");
  sg_box_pack_start(GTK_BOX(row2), prf->bscl, true, true, 4);
#if (!GTK_CHECK_VERSION(3, 0, 0))
  widget_modify_bg(prf->bscl, GTK_STATE_NORMAL, bscl_color);
  widget_modify_bg(prf->bscl, GTK_STATE_PRELIGHT, bscl_color);
#else
  add_blue_scale_style(prf->bscl);
#endif
  gtk_widget_show(prf->bscl);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->bscl)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->bscl), false);

  sep3 = gtk_hseparator_new();
  sg_box_pack_end(GTK_BOX(row2), sep3, false, false, 20);
  gtk_widget_show(sep3);

  SG_SIGNAL_CONNECT(prf->rtxt, "activate", prefs_r_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gtxt, "activate", prefs_g_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->btxt, "activate", prefs_b_callback, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->radj, "value_changed", prefs_call_color_func_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gadj, "value_changed", prefs_call_color_func_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->badj, "value_changed", prefs_call_color_func_callback, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->radj, "value_changed", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(prf->gadj, "value_changed", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(prf->badj, "value_changed", prefs_change_callback, NULL);

  SG_SIGNAL_CONNECT(prf->radj, "value_changed", prefs_color_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gadj, "value_changed", prefs_color_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->badj, "value_changed", prefs_color_callback, (gpointer)prf);
  prf->color_func = color_func;

  return(prf);
}


/* ---------------- topic separator ---------------- */

static void make_inter_topic_separator(GtkWidget *topics)
{
  GtkWidget *w;
  w = gtk_hseparator_new();
  sg_box_pack_start(GTK_BOX(topics), w, false, false, 0);
  gtk_widget_show(w);
  /* height = INTER_TOPIC_SPACE no line */
}


/* ---------------- variable separator ---------------- */

static void make_inter_variable_separator(GtkWidget *topics)
{
  GtkWidget *w;
  w = gtk_hseparator_new();
  sg_box_pack_start(GTK_BOX(topics), w, false, false, 0);
  gtk_widget_show(w);
  /* height = INTER_VARIABLE_SPACE no line */
}


/* ---------------- top-level contents label ---------------- */

static void make_top_level_label(const char *label, GtkWidget *parent)
{
  GtkWidget *w1, *w2, *w3;
  char *str;

  w1 = gtk_vseparator_new();
  sg_box_pack_start(GTK_BOX(parent), w1, false, false, 6);
  gtk_widget_show(w1);

  str = mus_format("<b>%s</b>", label);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  w2 = gtk_label_new(label);
  gtk_label_set_markup(GTK_LABEL(w2), str);
  gtk_label_set_use_markup(GTK_LABEL(w2), true);
  gtk_misc_set_alignment(GTK_MISC(w2), 0.01, 0.5);
#else
  w2 = gtk_button_new_with_label(label);
  add_highlight_button_style(w2);
  gtk_label_set_markup(GTK_LABEL(BIN_CHILD(w2)), str);
  gtk_label_set_use_markup(GTK_LABEL(BIN_CHILD(w2)), true);
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_widget_set_halign(GTK_WIDGET(w2), GTK_ALIGN_START);
#else
  gtk_misc_set_alignment(GTK_MISC(BIN_CHILD(w2)), 0.01, 0.5);
#endif
#endif

  free(str);

  sg_box_pack_start(GTK_BOX(parent), w2, false, false, 0);
  gtk_widget_show(w2);

  w3 = gtk_vseparator_new();
  sg_box_pack_start(GTK_BOX(parent), w3, false, false, 6);
  gtk_widget_show(w3);

  make_inter_variable_separator(parent);
}


static GtkWidget *make_top_level_box(GtkWidget *topics)
{
  GtkWidget *w, *frame;

  frame = gtk_frame_new(NULL);
  sg_box_pack_start(GTK_BOX(topics), frame, true, true, 0);
  gtk_widget_show(frame);

  w = gtk_vbox_new(false, 2);
  gtk_container_add(GTK_CONTAINER(frame), w);
  gtk_widget_show(w);
  return(w);
}


static void make_inner_label(const char *label, GtkWidget *parent)
{
  GtkWidget *w, *w1, *w2;
  char *str;

  w1 = gtk_vseparator_new();
  sg_box_pack_start(GTK_BOX(parent), w1, false, false, 4);
  gtk_widget_show(w1);

  str = mus_format("<b>%s</b>", label);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  w = gtk_label_new(label);
  gtk_label_set_markup(GTK_LABEL(w), str);
  gtk_label_set_use_markup(GTK_LABEL(w), true);
  gtk_misc_set_alignment(GTK_MISC(w), 0.0, 0.5);
#else
  w = gtk_button_new_with_label(label);
  add_highlight_button_style(w);
  gtk_label_set_markup(GTK_LABEL(BIN_CHILD(w)), str);
  gtk_label_set_use_markup(GTK_LABEL(BIN_CHILD(w)), true);
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_widget_set_halign(GTK_WIDGET(w), GTK_ALIGN_START);
#else
  gtk_misc_set_alignment(GTK_MISC(BIN_CHILD(w)), 0.0, 0.5);
#endif
#endif

  free(str);

  sg_box_pack_start(GTK_BOX(parent), w, false, false, 0);
  gtk_widget_show(w);

  w2 = gtk_vseparator_new();
  sg_box_pack_start(GTK_BOX(parent), w2, false, false, 4);
  gtk_widget_show(w2);

  make_inter_variable_separator(parent);
}


/* ---------------- base buttons ---------------- */

static gint preferences_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  clear_prefs_dialog_error();
  gtk_widget_hide(preferences_dialog);
  return(true);
}


static void preferences_dismiss_callback(GtkWidget *w, gpointer context) 
{
  clear_prefs_dialog_error();
  gtk_widget_hide(preferences_dialog);
}


static void preferences_help_callback(GtkWidget *w, gpointer context) 
{
  snd_help("preferences",
	   "This dialog sets various global variables. 'Save' then writes the new values \
to ~/.snd_prefs_ruby|forth|s7 so that they take effect the next time you start Snd.  'Revert' resets each variable either to \
its value when the Preferences dialog was started, or to the last saved value.  'Clear' resets each variable to its default value (its \
value when Snd starts, before loading initialization files). 'Help' starts this dialog, and as long as it's active, it will post helpful \
information if the mouse lingers over some variable -- sort of a tooltip that stays out of your way. \
You can also request help on a given topic by clicking the variable name on the far right.",
	   WITH_WORD_WRAP);
}


static void prefs_set_dialog_title(const char *filename)
{
  char *str;
  if (filename)
    {
      if (prefs_saved_filename) free(prefs_saved_filename);
      prefs_saved_filename = mus_strdup(filename);
    }
  if (prefs_saved_filename)
    str = mus_format("Preferences%s (saved in %s)\n",
		     (prefs_unsaved) ? "*" : "",
		     prefs_saved_filename);
  else str = mus_format("Preferences%s",
			(prefs_unsaved) ? "*" : "");
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), str);
  free(str);
}


static void preferences_revert_callback(GtkWidget *w, gpointer context) 
{
  preferences_revert_or_clear(true);
}


static void preferences_clear_callback(GtkWidget *w, gpointer context) 
{
  preferences_revert_or_clear(false);
}


#if HAVE_EXTENSION_LANGUAGE
static void preferences_save_callback(GtkWidget *w, gpointer context) 
{
  clear_prefs_dialog_error();
  redirect_snd_error_to(post_prefs_dialog_error, NULL);
  redirect_snd_warning_to(post_prefs_dialog_error, NULL);
  save_prefs();
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
}
#endif


/* ---------------- errors ---------------- */

static void clear_prefs_error(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  g_signal_handler_disconnect(prf->text, prf->erase_id);
  prf->erase_id = 0;
  set_label(prf->error, "");
}


static void post_prefs_error(const char *msg, prefs_info *prf)
{
  prf->got_error = true;
  set_label(prf->error, msg);
  if (prf->erase_id != 0)
    g_signal_handler_disconnect(prf->text, prf->erase_id);
  prf->erase_id = SG_SIGNAL_CONNECT(prf->text, "changed", clear_prefs_error, (gpointer)prf);
}


static void va_post_prefs_error(const char *msg, prefs_info *data, ...)
{
  char *buf;
  va_list ap;
  va_start(ap, data);
  buf = vstr(msg, ap);
  va_end(ap);
  post_prefs_error(buf, data);
  free(buf);
}


/* ---------------- preferences dialog ---------------- */

widget_t make_preferences_dialog(void)
{
  GtkWidget *save_button, *revert_button, *clear_button, *help_button, *dismiss_button, *topics, *scroller;
  prefs_info *prf;
  char *str;

  if (preferences_dialog)
    {
      gtk_widget_show(preferences_dialog);
      return(preferences_dialog);
    }

  preferences_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(preferences_dialog), GTK_WINDOW(main_shell(ss)));
#endif
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), "Preferences");
  sg_make_resizable(preferences_dialog);
  /* sg_container_set_border_width (GTK_CONTAINER(preferences_dialog), 10); */
  gtk_widget_realize(preferences_dialog);

#if (!GTK_CHECK_VERSION(3, 22, 0))
  if ((STARTUP_WIDTH < gdk_screen_width()) &&
      (STARTUP_HEIGHT < gdk_screen_height()))
#endif
    gtk_window_resize(GTK_WINDOW(preferences_dialog), STARTUP_WIDTH, STARTUP_HEIGHT);

  help_button = gtk_dialog_add_button(GTK_DIALOG(preferences_dialog), "Help", GTK_RESPONSE_NONE);
  revert_button = gtk_dialog_add_button(GTK_DIALOG(preferences_dialog), "Revert", GTK_RESPONSE_NONE);
  clear_button = gtk_dialog_add_button(GTK_DIALOG(preferences_dialog), "Clear", GTK_RESPONSE_NONE);
  dismiss_button = gtk_dialog_add_button(GTK_DIALOG(preferences_dialog), "Go away", GTK_RESPONSE_NONE);
#if HAVE_EXTENSION_LANGUAGE
  save_button = gtk_dialog_add_button(GTK_DIALOG(preferences_dialog), "Save", GTK_RESPONSE_NONE);
  gtk_widget_set_name(save_button, "dialog_button");
#endif

  gtk_widget_set_name(help_button, "dialog_button");
  gtk_widget_set_name(revert_button, "dialog_button");
  gtk_widget_set_name(clear_button, "dialog_button");
  gtk_widget_set_name(dismiss_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(dismiss_button);
  add_highlight_button_style(revert_button);
  add_highlight_button_style(clear_button);
#if HAVE_EXTENSION_LANGUAGE
  add_highlight_button_style(save_button);
#endif
  add_highlight_button_style(help_button);
#endif

  SG_SIGNAL_CONNECT(preferences_dialog, "delete_event", preferences_delete_callback, NULL);
  SG_SIGNAL_CONNECT(dismiss_button, "clicked", preferences_dismiss_callback, NULL);
  SG_SIGNAL_CONNECT(revert_button, "clicked", preferences_revert_callback, NULL);
  SG_SIGNAL_CONNECT(clear_button, "clicked", preferences_clear_callback, NULL);
#if HAVE_EXTENSION_LANGUAGE
  SG_SIGNAL_CONNECT(save_button, "clicked", preferences_save_callback, NULL);
#endif
  SG_SIGNAL_CONNECT(help_button, "clicked", preferences_help_callback, NULL);

  gtk_widget_show(dismiss_button);
#if HAVE_EXTENSION_LANGUAGE
  gtk_widget_show(save_button);
#endif
  gtk_widget_show(revert_button);
  gtk_widget_show(clear_button);
  gtk_widget_show(help_button);

  topics = gtk_vbox_new(false, 0);
  scroller = gtk_scrolled_window_new(NULL, NULL);
  sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(preferences_dialog)), scroller, true, true, 0);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#if HAVE_GTK_HEADER_BAR_NEW
  gtk_container_add(GTK_CONTAINER(scroller), topics);
#else
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scroller), topics);
#endif

  gtk_widget_show(topics);
  gtk_widget_show(scroller);

  label_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  widgets_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

#if 0
  /* these are really garish */
  rscl_color = rgb_to_color(1.0, 0.0, 0.0);
  gscl_color = rgb_to_color(0.0, 1.0, 0.0);
  bscl_color = rgb_to_color(0.0, 0.0, 1.0);
#else
  rscl_color = rgb_to_color(1.0, 0.4, 0.4);
  gscl_color = rgb_to_color(0.56, 1.0, 0.56);
  bscl_color = rgb_to_color(0.68, 0.84, 1.0);
#endif

  /* ---------------- overall behavior ---------------- */

  {
    GtkWidget *dpy_box;
    char *str1, *str2;

    /* ---------------- overall behavior ----------------*/

    dpy_box = make_top_level_box(topics);
    make_top_level_label("overall behavior choices", dpy_box);

    str1 = mus_format("%d", ss->init_window_width);
    str2 = mus_format("%d", ss->init_window_height);
    rts_init_window_width = ss->init_window_width;
    rts_init_window_height = ss->init_window_height;
    prf = prefs_row_with_two_texts("start up size", S_window_width, 
				   "width:", str1, "height:", str2, 6,
				   dpy_box,
				   startup_size_text);
    remember_pref(prf, reflect_init_window_size, save_init_window_size, help_init_window_size, clear_init_window_size, revert_init_window_size);  
    free(str2);
    free(str1);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("ask before overwriting anything", S_ask_before_overwrite,
				rts_ask_before_overwrite = ask_before_overwrite(ss), 
				dpy_box,
				overwrite_toggle);
    remember_pref(prf, reflect_ask_before_overwrite, save_ask_before_overwrite, help_ask_before_overwrite, NULL, revert_ask_before_overwrite);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("ask about unsaved edits before exiting", S_ask_about_unsaved_edits,
				rts_unsaved_edits = ask_about_unsaved_edits(ss), 
				dpy_box,
				unsaved_edits_toggle);
    remember_pref(prf, reflect_unsaved_edits, save_unsaved_edits, help_unsaved_edits, clear_unsaved_edits, revert_unsaved_edits);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("include thumbnail graph in upper right corner", S_with_inset_graph,
				rts_with_inset_graph = with_inset_graph(ss),
				dpy_box,
				with_inset_graph_toggle);
    remember_pref(prf, reflect_with_inset_graph, save_with_inset_graph, help_inset_graph, 
		  clear_with_inset_graph, revert_with_inset_graph);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("resize main window as sounds open and close", S_auto_resize,
				rts_auto_resize = auto_resize(ss), 
				dpy_box, 
				resize_toggle);
    remember_pref(prf, reflect_auto_resize, save_auto_resize, help_auto_resize, NULL, revert_auto_resize);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("pointer focus", S_with_pointer_focus,
				rts_with_pointer_focus = with_pointer_focus(ss),
				dpy_box,
				with_pointer_focus_toggle);
    remember_pref(prf, reflect_with_pointer_focus, save_with_pointer_focus, help_pointer_focus, 
		  clear_with_pointer_focus, revert_with_pointer_focus);

    make_inter_variable_separator(dpy_box);
    rts_sync_style = sync_style(ss);
    prf = prefs_row_with_two_toggles("operate on all channels together", S_sync,
				     "within each sound", rts_sync_style == SYNC_BY_SOUND,
				     "across all sounds", rts_sync_style == SYNC_ALL,
				     dpy_box,
				     sync1_choice, sync2_choice);
    remember_pref(prf, reflect_sync_style, save_sync_style, help_sync_style, clear_sync_style, revert_sync_style);

    make_inter_variable_separator(dpy_box);
    rts_remember_sound_state = remember_sound_state(ss);
    prf = prefs_row_with_toggle("restore a sound's state if reopened later", S_remember_sound_state,
				rts_remember_sound_state,
				dpy_box,
				toggle_remember_sound_state);
    remember_pref(prf, reflect_remember_sound_state, save_remember_sound_state, help_remember_sound_state, 
		  clear_remember_sound_state, revert_remember_sound_state);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("show the control panel upon opening a sound", S_show_controls,
				rts_show_controls = in_show_controls(ss), 
				dpy_box, 
				controls_toggle);
    remember_pref(prf, reflect_show_controls, save_show_controls, help_show_controls, NULL, revert_show_controls);

    make_inter_variable_separator(dpy_box);
    include_peak_env_directory = mus_strdup(peak_env_dir(ss));
    rts_peak_env_directory = mus_strdup(include_peak_env_directory);
    include_peak_envs = find_peak_envs();
    rts_peak_envs = include_peak_envs;
    prf = prefs_row_with_toggle_with_text("save peak envs to speed up initial display", S_peak_env_dir,
					  include_peak_envs,
					  "directory:", include_peak_env_directory, 25,
					  dpy_box,
					  peak_envs_toggle, peak_envs_text);
    remember_pref(prf, reflect_peak_envs, save_peak_envs, help_peak_envs, clear_peak_envs, revert_peak_envs);

    make_inter_variable_separator(dpy_box);
    str = mus_format("%d", rts_max_regions = max_regions(ss));
    prf = prefs_row_with_toggle_with_text("selection creates an associated region", S_selection_creates_region,
					  rts_selection_creates_region = selection_creates_region(ss),
					  "max regions:", str, 5,
					  dpy_box,
					  selection_creates_region_toggle, max_regions_text);
    remember_pref(prf, reflect_selection_creates_region, save_selection_creates_region, help_selection_creates_region, NULL, revert_selection_creates_region);
    free(str);


    make_inter_variable_separator(dpy_box);
    rts_with_toolbar = with_toolbar(ss);
    prf = prefs_row_with_toggle("include a toolbar", S_with_toolbar, 
				rts_with_toolbar,
				dpy_box,
				toggle_with_toolbar);
    remember_pref(prf, reflect_with_toolbar, save_with_toolbar, help_with_toolbar, clear_with_toolbar, revert_with_toolbar);

    make_inter_variable_separator(dpy_box);
    rts_with_tooltips = with_tooltips(ss);
    prf = prefs_row_with_toggle("enable tooltips", S_with_tooltips, 
				rts_with_tooltips,
				dpy_box,
				toggle_with_tooltips);
    remember_pref(prf, reflect_with_tooltips, save_with_tooltips, help_with_tooltips, clear_with_tooltips, revert_with_tooltips);


    make_inter_variable_separator(dpy_box);
    rts_with_menu_icons = with_menu_icons(ss);
    prf = prefs_row_with_toggle("enable menu icons (gtk only)", S_with_menu_icons, 
				rts_with_menu_icons,
				dpy_box,
				toggle_with_menu_icons);
    remember_pref(prf, reflect_with_menu_icons, save_with_menu_icons, help_with_menu_icons, clear_with_menu_icons, revert_with_menu_icons);



    /* ---------------- file options ---------------- */

    make_inter_variable_separator(dpy_box);
    make_inner_label("  file options", dpy_box);

    rts_load_path = find_sources();
    prf = prefs_row_with_text("directory containing Snd's " Xen_language " files", "load path", 
			      rts_load_path,
			      dpy_box,
			      load_path_text);
    remember_pref(prf, reflect_load_path, NULL, help_load_path, clear_load_path, revert_load_path);
    load_path_text_widget = prf->text;

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("display only sound files in various file lists", S_just_sounds,
				rts_just_sounds = just_sounds(ss), 
				dpy_box,
				just_sounds_toggle);
    remember_pref(prf, prefs_reflect_just_sounds, save_just_sounds, help_just_sounds, NULL, revert_just_sounds);

    make_inter_variable_separator(dpy_box);
    rts_temp_dir = mus_strdup(temp_dir(ss));
    prf = prefs_row_with_text("directory for temporary files", S_temp_dir, 
			      temp_dir(ss), 
			      dpy_box,
			      temp_dir_text);
    remember_pref(prf, reflect_temp_dir, save_temp_dir, help_temp_dir, NULL, revert_temp_dir);

    make_inter_variable_separator(dpy_box);
    rts_save_dir = mus_strdup(save_dir(ss));
    prf = prefs_row_with_text("directory for save-state files", S_save_dir, 
			      save_dir(ss), 
			      dpy_box,
			      save_dir_text);
    remember_pref(prf, reflect_save_dir, save_save_dir, help_save_dir, NULL, revert_save_dir);

    make_inter_variable_separator(dpy_box);
    rts_save_state_file = mus_strdup(save_state_file(ss));
    prf = prefs_row_with_text("default save-state filename", S_save_state_file, 
			      save_state_file(ss), 
			      dpy_box,
			      save_state_file_text);
    remember_pref(prf, reflect_save_state_file, save_save_state_file, help_save_state_file, NULL, revert_save_state_file);

#if HAVE_LADSPA
    make_inter_variable_separator(dpy_box);
    rts_ladspa_dir = mus_strdup(ladspa_dir(ss));
    prf = prefs_row_with_text("directory for ladspa plugins", S_ladspa_dir, 
			      ladspa_dir(ss), 
			      dpy_box,
			      ladspa_dir_text);
    remember_pref(prf, reflect_ladspa_dir, save_ladspa_dir, help_ladspa_dir, NULL, revert_ladspa_dir);
#endif

    make_inter_variable_separator(dpy_box);
    rts_html_program = mus_strdup(html_program(ss));
    prf = prefs_row_with_text("external program to read HTML files via snd-help", S_html_program,
			      html_program(ss),
			      dpy_box,
			      html_program_text);
    remember_pref(prf, reflect_html_program, save_html_program, help_html_program, NULL, revert_html_program);
    make_inter_variable_separator(dpy_box);

    rts_default_output_chans = default_output_chans(ss);
    prf = prefs_row_with_radio_box("default new sound attributes: chans", S_default_output_chans,
				   output_chan_choices, NUM_OUTPUT_CHAN_CHOICES, -1,
				   dpy_box,
				   default_output_chans_choice);
    remember_pref(prf, reflect_default_output_chans, save_default_output_chans, help_default_output_chans, NULL, revert_default_output_chans);
    reflect_default_output_chans(prf);

    rts_default_output_srate = default_output_srate(ss);
    prf = prefs_row_with_radio_box("srate", S_default_output_srate,
				   output_srate_choices, NUM_OUTPUT_SRATE_CHOICES, -1,
				   dpy_box,
				   default_output_srate_choice);
    remember_pref(prf, reflect_default_output_srate, save_default_output_srate, help_default_output_srate, NULL, revert_default_output_srate);
    reflect_default_output_srate(prf);

    rts_default_output_header_type = default_output_header_type(ss);
    prf = prefs_row_with_radio_box("header type", S_default_output_header_type,
				   output_header_type_choices, NUM_OUTPUT_HEADER_TYPE_CHOICES, -1,
				   dpy_box,
				   default_output_header_type_choice);
    output_header_type_prf = prf;
    remember_pref(prf, reflect_default_output_header_type, save_default_output_header_type, help_default_output_header_type, NULL, revert_default_output_header_type);

    rts_default_output_sample_type = default_output_sample_type(ss);
    prf = prefs_row_with_radio_box("sample type", S_default_output_sample_type,
				   output_sample_type_choices, NUM_OUTPUT_SAMPLE_TYPE_CHOICES, -1,
				   dpy_box,
				   default_output_sample_type_choice);
    output_sample_type_prf = prf;
    remember_pref(prf, reflect_default_output_sample_type, save_default_output_sample_type, help_default_output_sample_type, NULL, revert_default_output_sample_type);
    reflect_default_output_header_type(output_header_type_prf);
    reflect_default_output_sample_type(output_sample_type_prf);

    make_inter_variable_separator(dpy_box);
    {
      int i, srate = 0, chans = 0;
      mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
      mus_header_raw_defaults(&srate, &chans, &samp_type);
      str = mus_format("%d", chans);
      str1 = mus_format("%d", srate);
      rts_raw_chans = chans;
      rts_raw_srate = srate;
      rts_raw_sample_type = samp_type;
      raw_sample_type_choices = (char **)calloc(MUS_NUM_SAMPLES - 1, sizeof(char *));
      for (i = 1; i < MUS_NUM_SAMPLES; i++)
	raw_sample_type_choices[i - 1] = raw_sample_type_to_string((mus_sample_t)i); /* skip MUS_UNKNOWN_SAMPLE = 0 */
      prf = prefs_row_with_text("default raw sound attributes: chans", S_mus_header_raw_defaults, str,
				dpy_box, 
				raw_chans_choice);
      remember_pref(prf, reflect_raw_chans, save_raw_chans, help_raw_chans, NULL, revert_raw_chans);

      prf = prefs_row_with_text("srate", S_mus_header_raw_defaults, str1,
				dpy_box, 
				raw_srate_choice);
      remember_pref(prf, reflect_raw_srate, save_raw_srate, help_raw_srate, NULL, revert_raw_srate);
      free(str);
      free(str1);


      prf = prefs_row_with_list("sample type", S_mus_header_raw_defaults, raw_sample_type_choices[samp_type - 1],
				(const char **)raw_sample_type_choices, MUS_NUM_SAMPLES - 1,
				dpy_box, 
				raw_sample_type_from_text,
				NULL, NULL);
      remember_pref(prf, reflect_raw_sample_type, save_raw_sample_type, help_raw_sample_type, NULL, revert_raw_sample_type);

    }
    make_inter_variable_separator(dpy_box);


    /* ---------------- additional keys ---------------- */

    {
      key_info *ki;

      make_inner_label("  additional keys", dpy_box);
      ki = find_prefs_key("play-from-cursor");
      prf = prefs_row_with_text_and_three_toggles("play all chans from cursor", S_play, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,						
						  dpy_box,
						  bind_play_from_cursor);
      remember_pref(prf, reflect_play_from_cursor, save_pfc, help_play_from_cursor, clear_play_from_cursor, NULL);
      free(ki);

      make_inter_variable_separator(dpy_box);
      ki = find_prefs_key("show-all");
      prf = prefs_row_with_text_and_three_toggles("show entire sound", S_x_bounds, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_show_all);
      remember_pref(prf, reflect_show_all, save_show_all, help_show_all, clear_show_all, NULL);
      free(ki);

      make_inter_variable_separator(dpy_box);
      ki = find_prefs_key("select-all");
      prf = prefs_row_with_text_and_three_toggles("select entire sound", S_select_all, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_select_all);
      remember_pref(prf, reflect_select_all, save_select_all, help_select_all, clear_select_all, NULL);
      free(ki);

      make_inter_variable_separator(dpy_box);
      ki = find_prefs_key("show-selection");
      prf = prefs_row_with_text_and_three_toggles("show current selection", "show-selection", 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_show_selection);
      remember_pref(prf, reflect_show_selection, save_show_selection, help_show_selection, clear_show_selection, NULL);
      free(ki);

      make_inter_variable_separator(dpy_box);
      ki = find_prefs_key("revert-sound");
      prf = prefs_row_with_text_and_three_toggles("undo all edits (revert)", S_revert_sound, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_revert);
      remember_pref(prf, reflect_revert, save_revert, help_revert, clear_revert_sound, NULL);
      free(ki);

      make_inter_variable_separator(dpy_box);
      ki = find_prefs_key("exit");
      prf = prefs_row_with_text_and_three_toggles("exit from Snd", S_exit, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_exit);
      remember_pref(prf, reflect_exit, save_exit, help_exit, clear_exit, NULL);
      free(ki);

      make_inter_variable_separator(dpy_box);
      ki = find_prefs_key("goto-maxamp");
      prf = prefs_row_with_text_and_three_toggles("move cursor to channel's maximum sample", S_maxamp_position, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_goto_maxamp);
      remember_pref(prf, reflect_goto_maxamp, save_goto_maxamp, help_goto_maxamp, clear_goto_maxamp, NULL);
      free(ki);

    }

    /* ---------------- cursor options ---------------- */

    make_inter_variable_separator(dpy_box);
    make_inner_label("  cursor options", dpy_box);

    prf = prefs_row_with_toggle("report cursor location as it moves", S_with_verbose_cursor,
				rts_with_verbose_cursor = with_verbose_cursor(ss), 
				dpy_box,
				with_verbose_cursor_toggle);
    remember_pref(prf, reflect_with_verbose_cursor, save_with_verbose_cursor, help_with_verbose_cursor, NULL, revert_with_verbose_cursor);

    make_inter_variable_separator(dpy_box);
    {
      char *str1;
      str = mus_format("%.2f", rts_cursor_update_interval = cursor_update_interval(ss));
      str1 = mus_format("%d", rts_cursor_location_offset = cursor_location_offset(ss));
      prf = prefs_row_with_toggle_with_two_texts("track current location while playing", S_with_tracking_cursor,
						 (rts_with_tracking_cursor = with_tracking_cursor(ss)), 
						 "update:", str,
						 "offset:", str1, 8, 
						 dpy_box,
						 with_tracking_cursor_toggle,
						 cursor_location_text);
      remember_pref(prf, reflect_with_tracking_cursor, save_with_tracking_cursor, help_with_tracking_cursor, NULL, revert_with_tracking_cursor);
      free(str);
      free(str1);
    }

    make_inter_variable_separator(dpy_box);
    str = mus_format("%d", rts_cursor_size = cursor_size(ss));
    prf = prefs_row_with_number("size", S_cursor_size,
				str, 4, 
				dpy_box,
				cursor_size_up, cursor_size_down, cursor_size_from_text);
    remember_pref(prf, reflect_cursor_size, save_cursor_size, help_cursor_size, NULL, revert_cursor_size);
    free(str);
    if (cursor_size(ss) <= 0) gtk_widget_set_sensitive(prf->arrow_down, false);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_radio_box("shape", S_cursor_style,
				   cursor_styles, NUM_CURSOR_STYLES, 
				   rts_cursor_style = cursor_style(ss),
				   dpy_box, 
				   cursor_style_choice);
    remember_pref(prf, reflect_cursor_style, save_cursor_style, help_cursor_style, NULL, revert_cursor_style);

    make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_radio_box("tracking cursor shape", S_tracking_cursor_style,
				   cursor_styles, NUM_CURSOR_STYLES, 
				   rts_tracking_cursor_style = tracking_cursor_style(ss),
				   dpy_box, 
				   tracking_cursor_style_choice);
    remember_pref(prf, reflect_tracking_cursor_style, save_tracking_cursor_style, help_tracking_cursor_style, NULL, revert_tracking_cursor_style);

    make_inter_variable_separator(dpy_box);
    saved_cursor_color = ss->cursor_color;
    prf = prefs_color_selector_row("color", S_cursor_color, ss->cursor_color,
				   dpy_box,
				   cursor_color_func);
    remember_pref(prf, NULL, save_cursor_color, help_cursor_color, clear_cursor_color, revert_cursor_color);

    /* ---------------- (overall) colors ---------------- */

    make_inter_variable_separator(dpy_box);
    make_inner_label("  colors", dpy_box);
    
    saved_basic_color = ss->basic_color;
    prf = prefs_color_selector_row("main background color", S_basic_color, ss->basic_color,
				   dpy_box,
				   basic_color_func);
    remember_pref(prf, NULL, save_basic_color, help_basic_color, clear_basic_color, revert_basic_color);

    make_inter_variable_separator(dpy_box);
    saved_highlight_color = ss->highlight_color;
    prf = prefs_color_selector_row("main highlight color", S_highlight_color, ss->highlight_color,
				   dpy_box,
				   highlight_color_func);
    remember_pref(prf, NULL, save_highlight_color, help_highlight_color, clear_highlight_color, revert_highlight_color);

    make_inter_variable_separator(dpy_box);
    saved_position_color = ss->position_color;
    prf = prefs_color_selector_row("second highlight color", S_position_color, ss->position_color,
				   dpy_box,
				   position_color_func);
    remember_pref(prf, NULL, save_position_color, help_position_color, clear_position_color, revert_position_color);

    make_inter_variable_separator(dpy_box);
    saved_zoom_color = ss->zoom_color;
    prf = prefs_color_selector_row("third highlight color", S_zoom_color, ss->zoom_color,
				   dpy_box,
				   zoom_color_func);
    remember_pref(prf, NULL, save_zoom_color, help_zoom_color, clear_zoom_color, revert_zoom_color);
  }

  make_inter_topic_separator(topics);

  /* -------- graphs -------- */
  {
    GtkWidget *grf_box;

    /* ---------------- graph options ---------------- */

    grf_box = make_top_level_box(topics);
    make_top_level_label("graph options", grf_box);

    prf = prefs_row_with_radio_box("how to connect the dots", S_graph_style,
				   graph_styles, NUM_GRAPH_STYLES, 
				   rts_graph_style = graph_style(ss),
				   grf_box,
				   graph_style_choice);
    remember_pref(prf, reflect_graph_style, save_graph_style, help_graph_style, NULL, revert_graph_style);

    make_inter_variable_separator(grf_box);
    str = mus_format("%d", rts_dot_size = dot_size(ss));
    prf = prefs_row_with_number("dot size", S_dot_size,
				str, 4, 
				grf_box,
				dot_size_up, dot_size_down, dot_size_from_text);
    remember_pref(prf, reflect_dot_size, save_dot_size, help_dot_size, NULL, revert_dot_size);
    free(str);
    if (dot_size(ss) <= 0) gtk_widget_set_sensitive(prf->arrow_down, false);

    make_inter_variable_separator(grf_box);
    rts_initial_beg = initial_beg(ss);
    rts_initial_dur = initial_dur(ss);
    str = mus_format("%.2f : %.2f", rts_initial_beg, rts_initial_dur);
    prf = prefs_row_with_text_with_toggle("initial graph x bounds", S_initial_graph_hook, 
					  (rts_full_duration = show_full_duration(ss)),
					  "show full duration", str, 16,
					  grf_box, 
					  initial_bounds_toggle,
					  initial_bounds_text);
    free(str);
    remember_pref(prf, reflect_initial_bounds, save_initial_bounds, help_initial_bounds, clear_initial_bounds, revert_initial_bounds);

    make_inter_variable_separator(grf_box);
    prf = prefs_row_with_radio_box("how to layout multichannel graphs", S_channel_style,
				   channel_styles, NUM_CHANNEL_STYLES, 
				   rts_channel_style = channel_style(ss),
				   grf_box,
				   channel_style_choice);
    remember_pref(prf, reflect_channel_style, save_channel_style, help_channel_style, NULL, revert_channel_style);

    make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("layout wave and fft graphs horizontally", S_graphs_horizontal,
				rts_graphs_horizontal = graphs_horizontal(ss),
				grf_box,
				graphs_horizontal_toggle);
    remember_pref(prf, reflect_graphs_horizontal, save_graphs_horizontal, help_graphs_horizontal, NULL, revert_graphs_horizontal);

    make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("include y=0 line in sound graphs", S_show_y_zero,
				rts_show_y_zero = show_y_zero(ss),
				grf_box,
				y_zero_toggle);
    remember_pref(prf, reflect_show_y_zero, save_show_y_zero, help_show_y_zero, NULL, revert_show_y_zero);

    make_inter_variable_separator(grf_box);
    rts_show_grid = show_grid(ss);
    prf = prefs_row_with_toggle("include a grid in sound graphs", S_show_grid,
				rts_show_grid == WITH_GRID,
				grf_box,
				grid_toggle);
    remember_pref(prf, reflect_show_grid, save_show_grid, help_show_grid, NULL, revert_show_grid);

    make_inter_variable_separator(grf_box);
    prf = prefs_row_with_scale("grid density", S_grid_density, 
			       2.0, rts_grid_density = grid_density(ss),
			       grf_box,
			       grid_density_scale_callback, grid_density_text_callback);
    remember_pref(prf, reflect_grid_density, save_grid_density, help_grid_density, NULL, revert_grid_density);


    make_inter_variable_separator(grf_box);
    rts_show_axes = show_axes(ss);
    prf = prefs_row_with_list("what axes to display", S_show_axes, show_axes_choices[(int)rts_show_axes],
			      show_axes_choices, NUM_SHOW_AXES,
			      grf_box,
			      show_axes_from_text,
			      NULL, NULL);
    remember_pref(prf, reflect_show_axes, save_show_axes, help_show_axes, clear_show_axes, revert_show_axes);

    make_inter_variable_separator(grf_box);
    rts_x_axis_style = x_axis_style(ss);
    prf = prefs_row_with_list("time division", S_x_axis_style, x_axis_styles[(int)rts_x_axis_style],
			      x_axis_styles, NUM_X_AXIS_STYLES,
			      grf_box,
			      x_axis_style_from_text,
			      NULL, NULL);
    remember_pref(prf, reflect_x_axis_style, save_x_axis_style, help_x_axis_style, clear_x_axis_style, revert_x_axis_style);

    make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("include smpte info", "show-smpte-label",
				rts_with_smpte_label = with_smpte_label(ss),
				grf_box,
				smpte_toggle);
    remember_pref(prf, reflect_smpte, save_smpte, help_smpte, clear_smpte, revert_smpte);


    /* ---------------- (graph) colors ---------------- */

    make_inter_variable_separator(grf_box); 
    make_inner_label("  colors", grf_box);

    saved_data_color = ss->data_color;    
    prf = prefs_color_selector_row("unselected data (waveform) color", S_data_color, ss->data_color,
				   grf_box, 
				   data_color_func);
    remember_pref(prf, NULL, save_data_color, help_data_color, clear_data_color, revert_data_color);

    make_inter_variable_separator(grf_box);
    saved_graph_color = ss->graph_color;
    prf = prefs_color_selector_row("unselected graph (background) color", S_graph_color, ss->graph_color,
				   grf_box,
				   graph_color_func);
    remember_pref(prf, NULL, save_graph_color, help_graph_color, clear_graph_color, revert_graph_color);

    make_inter_variable_separator(grf_box);
    saved_selected_data_color = ss->selected_data_color;
    prf = prefs_color_selector_row("selected channel data (waveform) color", S_selected_data_color, ss->selected_data_color,
				   grf_box,
				   selected_data_color_func);
    remember_pref(prf, NULL, save_selected_data_color, help_selected_data_color, clear_selected_data_color, revert_selected_data_color);

    make_inter_variable_separator(grf_box);
    saved_selected_graph_color = ss->selected_graph_color;
    prf = prefs_color_selector_row("selected channel graph (background) color", S_selected_graph_color, ss->selected_graph_color,
				   grf_box,
				   selected_graph_color_func);
    remember_pref(prf, NULL, save_selected_graph_color, help_selected_graph_color, clear_selected_graph_color, revert_selected_graph_color);

    make_inter_variable_separator(grf_box);
    saved_selection_color = ss->selection_color;
    prf = prefs_color_selector_row("selection color", S_selection_color, ss->selection_color,
				   grf_box,
				   selection_color_func);
    remember_pref(prf, NULL, save_selection_color, help_selection_color, clear_selection_color, revert_selection_color);

    /* ---------------- (graph) fonts ---------------- */

    make_inter_variable_separator(grf_box);
    make_inner_label("  fonts", grf_box);

    rts_axis_label_font = mus_strdup(axis_label_font(ss));
    prf = prefs_row_with_text("axis label font", S_axis_label_font, 
			      axis_label_font(ss), 
			      grf_box, 
			      axis_label_font_text);
    remember_pref(prf, reflect_axis_label_font, save_axis_label_font, help_axis_label_font, clear_axis_label_font, revert_axis_label_font);

    make_inter_variable_separator(grf_box);    
    rts_axis_numbers_font = mus_strdup(axis_numbers_font(ss)); 
    prf = prefs_row_with_text("axis number font", S_axis_numbers_font, 
			      axis_numbers_font(ss), 
			      grf_box,
			      axis_numbers_font_text);
    remember_pref(prf, reflect_axis_numbers_font, save_axis_numbers_font, help_axis_numbers_font, clear_axis_numbers_font, revert_axis_numbers_font);

    make_inter_variable_separator(grf_box);  
    rts_peaks_font = mus_strdup(peaks_font(ss));    
    prf = prefs_row_with_text("fft peaks font", S_peaks_font, 
			      peaks_font(ss), 
			      grf_box,
			      peaks_font_text);
    remember_pref(prf, reflect_peaks_font, save_peaks_font, help_peaks_font, clear_peaks_font, revert_peaks_font);

    make_inter_variable_separator(grf_box);    
    rts_bold_peaks_font = mus_strdup(bold_peaks_font(ss));     
    prf = prefs_row_with_text("fft peaks bold font (for main peaks)", S_bold_peaks_font, 
			      bold_peaks_font(ss), 
			      grf_box,
			      bold_peaks_font_text);
    remember_pref(prf, reflect_bold_peaks_font, save_bold_peaks_font, help_bold_peaks_font, clear_bold_peaks_font, revert_bold_peaks_font);

    make_inter_variable_separator(grf_box);  
    rts_tiny_font = mus_strdup(tiny_font(ss));        
    prf = prefs_row_with_text("tiny font (for various annotations)", S_peaks_font, 
			      tiny_font(ss), 
			      grf_box,
			      tiny_font_text);
    remember_pref(prf, reflect_tiny_font, save_tiny_font, help_tiny_font, clear_tiny_font, revert_tiny_font);
  }

  make_inter_topic_separator(topics);

  /* -------- transform -------- */
  {
    GtkWidget *fft_box;

    /* ---------------- transform options ---------------- */

    fft_box = make_top_level_box(topics);
    make_top_level_label("transform options", fft_box);

    rts_fft_size = transform_size(ss);
    str = mus_format("%" PRId64, rts_fft_size);
    prf = prefs_row_with_number("size", S_transform_size,
				str, 12, 
				fft_box,
				fft_size_up, fft_size_down, fft_size_from_text);
    remember_pref(prf, reflect_fft_size, save_fft_size, help_fft_size, NULL, revert_fft_size);
    free(str);
    if (transform_size(ss) <= 2) gtk_widget_set_sensitive(prf->arrow_down, false);

    make_inter_variable_separator(fft_box);
    prf = prefs_row_with_radio_box("transform graph choice", S_transform_graph_type,
				   transform_graph_types, NUM_TRANSFORM_GRAPH_TYPES, 
				   rts_transform_graph_type = transform_graph_type(ss),
				   fft_box,
				   transform_graph_type_choice);
    remember_pref(prf, reflect_transform_graph_type, save_transform_graph_type, help_transform_graph_type, NULL, revert_transform_graph_type);


    make_inter_variable_separator(fft_box);
    rts_transform_type = transform_type(ss);
    prf = prefs_row_with_list("transform", S_transform_type, transform_types[rts_transform_type],
			      transform_types, NUM_BUILTIN_TRANSFORM_TYPES,
			      fft_box,
			      transform_type_from_text,
			      transform_type_completer, NULL);
    remember_pref(prf, reflect_transform_type, save_transform_type, help_transform_type, clear_transform_type, revert_transform_type);

    make_inter_variable_separator(fft_box);
    rts_fft_window = fft_window(ss);
    prf = prefs_row_with_list("data window", S_fft_window, mus_fft_window_name(rts_fft_window),
			      mus_fft_window_names(), MUS_NUM_FFT_WINDOWS,
			      fft_box,
			      fft_window_from_text,
			      fft_window_completer, NULL);
    remember_pref(prf, reflect_fft_window, save_fft_window, help_fft_window, clear_fft_window, revert_fft_window);


    make_inter_variable_separator(fft_box);
    prf = prefs_row_with_scale("data window family parameter", S_fft_window_beta, 
			       1.0, rts_fft_window_beta = fft_window_beta(ss),
			       fft_box,
			       fft_window_beta_scale_callback, fft_window_beta_text_callback);
    remember_pref(prf, reflect_fft_window_beta, save_fft_window_beta, help_fft_window_beta, NULL, revert_fft_window_beta);

    make_inter_variable_separator(fft_box);
    str = mus_format("%d", rts_max_transform_peaks = max_transform_peaks(ss));
    prf = prefs_row_with_toggle_with_text("show fft peak data", S_show_transform_peaks,
					  rts_show_transform_peaks = show_transform_peaks(ss),
					  "max peaks:", str, 5,
					  fft_box,
					  transform_peaks_toggle, max_peaks_text);
    remember_pref(prf, reflect_transform_peaks, save_transform_peaks, help_transform_peaks, NULL, revert_transform_peaks);
    free(str);


    make_inter_variable_separator(fft_box);
    {
      const char **cmaps;
      int i, len;
      len = num_colormaps();
      cmaps = (const char **)calloc(len, sizeof(const char *));
      for (i = 0; i < len; i++)
	cmaps[i] = (const char *)colormap_name(i);
      rts_colormap = color_map(ss);
      prf = prefs_row_with_list("sonogram colormap", S_colormap, cmaps[rts_colormap],
				cmaps, len,
				fft_box,
				colormap_from_text,
				colormap_completer, NULL);
      remember_pref(prf, reflect_colormap, save_colormap, help_colormap, clear_colormap, revert_colormap);
      free(cmaps);
    }


    make_inter_variable_separator(fft_box);
    prf = prefs_row_with_toggle("y axis as log magnitude (dB)", S_fft_log_magnitude,
				rts_fft_log_magnitude = fft_log_magnitude(ss),
				fft_box,
				log_magnitude_toggle);
    remember_pref(prf, reflect_fft_log_magnitude, save_fft_log_magnitude, help_fft_log_magnitude, NULL, revert_fft_log_magnitude);

    make_inter_variable_separator(fft_box);
    str = mus_format("%.1f", rts_min_dB = min_dB(ss));
    prf = prefs_row_with_text("minimum y-axis dB value", S_min_dB, str,
			      fft_box,
			      min_dB_text);
    remember_pref(prf, reflect_min_dB, save_min_dB, help_min_dB, NULL, revert_min_dB);
    free(str);

    make_inter_variable_separator(fft_box);
    prf = prefs_row_with_toggle("x axis as log freq", S_fft_log_frequency,
				rts_fft_log_frequency = fft_log_frequency(ss),
				fft_box,
				log_frequency_toggle);
    remember_pref(prf, reflect_fft_log_frequency, save_fft_log_frequency, help_fft_log_frequency, NULL, revert_fft_log_frequency);

    make_inter_variable_separator(fft_box);
    prf = prefs_row_with_radio_box("normalization", S_transform_normalization,
				   transform_normalizations, NUM_TRANSFORM_NORMALIZATIONS, 
				   rts_transform_normalization = transform_normalization(ss),
				   fft_box,
				   transform_normalization_choice);
    remember_pref(prf, reflect_transform_normalization, save_transform_normalization, help_transform_normalization, NULL, revert_transform_normalization);
  }

  make_inter_topic_separator(topics);

  /* -------- marks, mixes, and regions -------- */
  {
    GtkWidget *mmr_box;
    char *str1, *str2;

    /* ---------------- marks and mixes ---------------- */

    mmr_box = make_top_level_box(topics);
    make_top_level_label("marks and mixes", mmr_box);

    saved_mark_color = ss->mark_color;
    prf = prefs_color_selector_row("mark and mix tag color", S_mark_color, ss->mark_color,
				   mmr_box,
				   mark_color_func);
    remember_pref(prf, NULL, save_mark_color, help_mark_color, clear_mark_color, revert_mark_color);

    make_inter_variable_separator(mmr_box);

    str1 = mus_format("%d", rts_mark_tag_width = mark_tag_width(ss));
    str2 = mus_format("%d", rts_mark_tag_height = mark_tag_height(ss));
    prf = prefs_row_with_two_texts("mark tag size", S_mark_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box,
				   mark_tag_size_text);
    remember_pref(prf, reflect_mark_tag_size, save_mark_tag_size, help_mark_tag_size, NULL, revert_mark_tag_size);
    free(str2);
    free(str1);

    make_inter_variable_separator(mmr_box);
    str1 = mus_format("%d", rts_mix_tag_width = mix_tag_width(ss));
    str2 = mus_format("%d", rts_mix_tag_height = mix_tag_height(ss));
    prf = prefs_row_with_two_texts("mix tag size", S_mix_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box,
				   mix_tag_size_text);
    remember_pref(prf, reflect_mix_tag_size, save_mix_tag_size, help_mix_tag_size, NULL, revert_mix_tag_size);
    free(str2);
    free(str1);

    make_inter_variable_separator(mmr_box);
    saved_mix_color = ss->mix_color;
    prf = prefs_color_selector_row("mix waveform color", S_mix_color, ss->mix_color,
				   mmr_box,
				   mix_color_func);
    remember_pref(prf, NULL, save_mix_color, help_mix_color, clear_mix_color, revert_mix_color);

    make_inter_variable_separator(mmr_box);
    str = mus_format("%d", rts_mix_waveform_height = mix_waveform_height(ss));
    prf = prefs_row_with_toggle_with_text("show mix waveforms (attached to the mix tag)", S_show_mix_waveforms,
					  rts_show_mix_waveforms = show_mix_waveforms(ss),
					  "max waveform height:", str, 5,
					  mmr_box,
					  show_mix_waveforms_toggle, mix_waveform_height_text);
    remember_pref(prf, reflect_mix_waveforms, save_mix_waveforms, help_mix_waveforms, NULL, revert_mix_waveforms);
    free(str);
  }
  

  make_inter_topic_separator(topics);

  /* -------- clm -------- */
  {
    GtkWidget *clm_box;

    /* ---------------- clm options ---------------- */

    clm_box = make_top_level_box(topics);
    make_top_level_label("clm", clm_box);

    str = mus_format("%d", rts_speed_control_tones = speed_control_tones(ss));
    rts_speed_control_style = speed_control_style(ss);
    prf = prefs_row_with_radio_box_and_number("speed control choice", S_speed_control_style,
					      speed_control_styles, NUM_SPEED_CONTROL_STYLES, (int)speed_control_style(ss),
					      str, 6,
					      clm_box,
					      speed_control_choice, speed_control_up, speed_control_down, speed_control_text);
    remember_pref(prf, reflect_speed_control, save_speed_control, help_speed_control, NULL, revert_speed_control);
    free(str);

    make_inter_variable_separator(clm_box);
    str = mus_format("%d", rts_sinc_width = sinc_width(ss));
    prf = prefs_row_with_text("sinc interpolation width in srate converter", S_sinc_width, str,
			      clm_box,
			      sinc_width_text);
    remember_pref(prf, reflect_sinc_width, save_sinc_width, help_sinc_width, NULL, revert_sinc_width);
    free(str);
  }

  make_inter_topic_separator(topics);

  /* -------- programming -------- */
  {
    GtkWidget *prg_box;

    /* ---------------- listener options ---------------- */

    prg_box = make_top_level_box(topics);
    make_top_level_label("listener options", prg_box);

    prf = prefs_row_with_toggle("show listener at start up", S_show_listener,
				rts_show_listener = listener_is_visible(),
				prg_box,
				show_listener_toggle);
    remember_pref(prf, reflect_show_listener, save_show_listener, help_show_listener, clear_show_listener, revert_show_listener);

    make_inter_variable_separator(prg_box);
    rts_listener_prompt = mus_strdup(listener_prompt(ss));
    prf = prefs_row_with_text("prompt", S_listener_prompt, 
			      listener_prompt(ss), 
			      prg_box,
			      listener_prompt_text);
    remember_pref(prf, reflect_listener_prompt, save_listener_prompt, help_listener_prompt, NULL, revert_listener_prompt);

    make_inter_variable_separator(prg_box);
    str = mus_format("%d", rts_print_length = print_length(ss));
    prf = prefs_row_with_text("number of vector elements to display", S_print_length, str,
			      prg_box,
			      print_length_text);
    remember_pref(prf, reflect_print_length, save_print_length, help_print_length, NULL, revert_print_length);
    free(str);

    make_inter_variable_separator(prg_box);
    rts_listener_font = mus_strdup(listener_font(ss));
    prf = prefs_row_with_text("font", S_listener_font, 
			      listener_font(ss), 
			      prg_box,
			      listener_font_text);
    remember_pref(prf, reflect_listener_font, save_listener_font, help_listener_font, clear_listener_font, revert_listener_font);

    make_inter_variable_separator(prg_box);
    saved_listener_color = ss->listener_color;
    prf = prefs_color_selector_row("background color", S_listener_color, ss->listener_color,
				   prg_box,
				   listener_color_func);
    remember_pref(prf, NULL, save_listener_color, help_listener_color, clear_listener_color, revert_listener_color);

    make_inter_variable_separator(prg_box);
    saved_listener_text_color = ss->listener_text_color;
    prf = prefs_color_selector_row("text color", S_listener_text_color, ss->listener_text_color,
				   prg_box,
				   listener_text_color_func);
    remember_pref(prf, NULL, save_listener_text_color, help_listener_text_color, clear_listener_text_color, revert_listener_text_color);
  }

  /* -------- audio -------- */
  {
    GtkWidget *aud_box;

    /* ---------------- audio options ---------------- */

    aud_box = make_top_level_box(topics);
    make_top_level_label("audio options", aud_box);

    str = mus_format("%d", rts_dac_size = dac_size(ss));
    prf = prefs_row_with_text("dac buffer size", S_dac_size, 
			      str,
			      aud_box,
			      dac_size_text);
    remember_pref(prf, reflect_dac_size, save_dac_size, help_dac_size, NULL, revert_dac_size);
    free(str);

    make_inter_variable_separator(aud_box);
    prf = prefs_row_with_toggle("fold in otherwise unplayable channels", S_dac_combines_channels,
				rts_dac_combines_channels = dac_combines_channels(ss),
				aud_box,
				dac_combines_channels_toggle);
    remember_pref(prf, reflect_dac_combines_channels, save_dac_combines_channels, help_dac_combines_channels, NULL, revert_dac_combines_channels);

  }

  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);
  gtk_widget_show(preferences_dialog);
  prefs_unsaved = false;
  prefs_set_dialog_title(NULL);

  return(preferences_dialog);
}
