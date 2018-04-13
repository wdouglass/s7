#include "snd.h"

/* envelope editor and viewer */

static GtkWidget *enved_dialog = NULL;
static GtkWidget *enved_apply_button, *enved_apply2_button, *enved_cancel_button, *enved_drawer, *show_button;
static GtkWidget *enved_save_button = NULL, *enved_reset_button, *fir_button = NULL;
static GtkWidget *enved_revert_button, *enved_undo_button, *enved_redo_button, *brktxtL, *enved_graph_button;
static GtkWidget *flt_button, *amp_button, *src_button, *rbrow, *clip_button;
static GtkWidget *enved_name_label, *enved_text_label, *enved_dB_button, *enved_order_label;
static GtkWidget *lin_button, *lerow, *baseScale, *baseLabel, *baseValue, *enved_selection_button, *selrow, *unrow;
static GtkAdjustment *baseAdj, *orderAdj;
static gc_t *gc, *rgc, *ggc;
static slist *env_list = NULL;
#if (GTK_CHECK_VERSION(3, 89, 0))
  static cairo_t *enved_cr;
#endif

static const char *env_names[3] = {"amp env:", "flt env:", "src env:"};

static bool showing_all_envs = false; /* edit one env (0), or view all currently defined envs (1) */
static bool apply_to_selection = false, we_turned_selection_off = false;

static int env_window_width = 0;
static int env_window_height = 0;

static chan_info *active_channel = NULL, *last_active_channel = NULL;

static env* selected_env = NULL; /* if during view, one env is clicked, it is "selected" and can be pasted elsewhere */
static env* active_env = NULL;   /* env currently being edited */

static axis_info *axis = NULL;
static axis_info *gray_ap = NULL;

static bool is_FIR = true;
static bool old_clipping = false;
static bool ignore_button_release = false;


axis_info *enved_make_axis(const char *name, graphics_context *ax, 
			   int ex0, int ey0, int width, int height, 
			   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax,
			   printing_t printing)
{
  init_env_axes(axis, name, ex0, ey0, width, height, xmin, xmax, ymin, ymax, printing);
  return(axis);
}


static void display_env(env *e, const char *name, gc_t *cur_gc, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  ss->enved->with_dots = dots;
  axis->ax->gc = cur_gc;
  env_editor_display_env(ss->enved, e, axis->ax, name, x0, y0, width, height, printing);
}


void display_enved_env_with_selection(env *e, const char *name, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  /* called in a loop through all envs during view_envs (called only from env_redisplay_1 below) */
  display_env(e, name, (selected_env == e) ? rgc : gc, x0, y0, width, height, dots, printing);
}


static void reflect_segment_state(void);

static void prepare_env_edit(env *new_env)
{
  prepare_enved_edit(new_env);
  if (new_env->base == 1.0)
    set_enved_style(ENVELOPE_LINEAR);
  else
    {
      set_enved_style(ENVELOPE_EXPONENTIAL);
      set_enved_base(new_env->base);
    }
  reflect_segment_state();
}


void set_enved_redo_sensitive(bool val) {set_sensitive(enved_redo_button, val);}
void set_enved_revert_sensitive(bool val) {set_sensitive(enved_revert_button, val);}
void set_enved_undo_sensitive(bool val) {set_sensitive(enved_undo_button, val);}
void set_enved_save_sensitive(bool val) {set_sensitive(enved_save_button, val);}
void set_enved_show_sensitive(bool val) {set_sensitive(show_button, val);}


void make_scrolled_env_list(void)
{
  int n, size;
  size = enved_all_envs_top();
  slist_clear(env_list);
  for (n = 0; n < size; n++) 
    slist_append(env_list, enved_all_names(n));
  gtk_widget_show(env_list->scroller);
}


void enved_reflect_peak_env_completion(snd_info *sp)
{
  if ((enved_dialog) && (active_channel) && (enved_with_wave(ss)))
    {
      if (active_channel->sound == sp) 
	env_redisplay();
    }
}


void new_active_channel_alert(void)
{
  if (enved_dialog)
    {
      /* if showing current active channel in gray, update */
      active_channel = current_channel();
      env_redisplay();
    }
}


static void dismiss_enved_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(enved_dialog);
}


static gint delete_enved_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(enved_dialog);
  return(true);
}


static void help_enved_callback(GtkWidget *w, gpointer context)
{
  envelope_editor_dialog_help();
}


static void force_update(GtkWidget *wid)
{
  if ((wid) && (WIDGET_TO_WINDOW(wid)))
    gdk_window_invalidate_rect(GDK_WINDOW(WIDGET_TO_WINDOW(wid)), NULL, true);
}


static bool within_selection_src = false;

static void apply_enved(void)
{
  if (active_env)
    {
      active_channel = current_channel();
      if (active_channel)
	{
	  char *origin = NULL, *estr = NULL;
	  set_sensitive(enved_apply_button, false);
	  set_sensitive(enved_apply2_button, false);
	  set_stock_button_label(enved_cancel_button, I_STOP);
	  force_update(enved_cancel_button);
	  switch (enved_target(ss))
	    {
	    case ENVED_AMPLITUDE:
#if HAVE_FORTH
	      origin = mus_format("%s%s %s drop", 
				  estr = env_to_string(active_env),
				  (apply_to_selection) ? "" : " 0 " PROC_FALSE,
				  (apply_to_selection) ? S_env_selection : S_env_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%s%s", 
				  to_proc_name((apply_to_selection) ? S_env_selection : S_env_channel),
				  estr = env_to_string(active_env),
				  (apply_to_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	      apply_env(active_channel, active_env, 0, 
			current_samples(active_channel),
			apply_to_selection, 
			origin, NULL,
			C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0);
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      if (enved_with_wave(ss)) env_redisplay();
	      if (estr) free(estr);
	      if (origin) free(origin);
	      break;
	    case ENVED_SPECTRUM: 
#if HAVE_FORTH
	      origin = mus_format("%s %d%s %s drop",
				  estr = env_to_string(active_env), 
				  enved_filter_order(ss),
				  (apply_to_selection) ? "" : " 0 " PROC_FALSE,
				  (apply_to_selection) ? S_filter_selection : S_filter_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d%s",
				  to_proc_name((apply_to_selection) ? S_filter_selection : S_filter_channel),
				  estr = env_to_string(active_env), 
				  enved_filter_order(ss),
				  (apply_to_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	      apply_filter(active_channel, 
			   (is_FIR) ? enved_filter_order(ss) : 0,
			   active_env, 
			   origin, NULL,
			   apply_to_selection, NULL, NULL,
			   C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0, false);
	      if (estr) free(estr);
	      if (origin) free(origin);
	      break;
	    case ENVED_SRATE:
	      {
		int i, j;
		env *max_env;
		max_env = copy_env(active_env);
		for (i = 0, j = 1; i < max_env->pts; i++, j += 2)
		  if (max_env->data[j] < .01) max_env->data[j] = .01;
		within_selection_src = true;
		src_env_or_num(active_channel, max_env, 0.0, 
			       false, "Enved: src", apply_to_selection, NULL,
			       C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0);
		within_selection_src = false;
		max_env = free_env(max_env);
		if (enved_with_wave(ss)) env_redisplay();
	      }
	      break;
	    }
	  set_sensitive(enved_apply_button, true);
	  set_sensitive(enved_apply2_button, true);
	  set_stock_button_label(enved_cancel_button, I_GO_AWAY);
	}
    }
}


static void env_redisplay_1(printing_t printing)
{
  cairo_t *cr;
  bool clear_cr = false;

  if (!enved_dialog_is_active()) return;
#if (GTK_CHECK_VERSION(3, 89, 0))
  cr = enved_cr;
  if (!cr) return;
  ss->cr = cr; /* used in make_axes etc */
#else
  if ((printing == NOT_PRINTING) && 
      (!(ss->cr)))
    {
      /* we can get here from display_channel_data_with_size with an existing ss->cr */
      ss->cr = make_cairo(WIDGET_TO_WINDOW(enved_drawer));
      clear_cr = true;
    }
  if (!(ss->cr)) return;
  cr = ss->cr;
#endif
      
  cairo_push_group(cr);
  cairo_set_source_rgba(cr, gc->bg_color->red, gc->bg_color->green, gc->bg_color->blue, gc->bg_color->alpha);
  cairo_rectangle(cr, 0, 0, env_window_width, env_window_height);
  cairo_fill(cr);
  
  if (showing_all_envs) 
    {
      int x0, x1, y0, y1;
      x0 = axis->x_axis_x0;
      x1 = axis->x_axis_x1;
      y0 = axis->y_axis_y0;
      y1 = axis->y_axis_y1;
      view_envs(env_window_width, env_window_height, NOT_PRINTING); /* NOT_PRINTING because we're not using the eps stuff here */
      axis->x_axis_x0 = x0;
      axis->x_axis_x1 = x1;
      axis->y_axis_y0 = y0;
      axis->y_axis_y1 = y1;
    }
  else 
    {
      char *name;
      name = (char *)gtk_entry_get_text(GTK_ENTRY(enved_text_label));
      if (!name) name = (char *)"noname";
      
      if ((enved_with_wave(ss)) &&
	  (active_channel) &&
	  (!(active_channel->squelch_update)))
	{
	  if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env) && (is_FIR) && (printing == NOT_PRINTING))
	    display_frequency_response(active_env, axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	  enved_show_background_waveform(axis, gray_ap, apply_to_selection, (enved_target(ss) == ENVED_SPECTRUM), NOT_PRINTING);
	}
      display_env(active_env, name, gc, 0, 0, env_window_width, env_window_height, true, NOT_PRINTING);
    }
  
  cairo_pop_group_to_source(cr);
  cairo_paint(cr);

#if (!GTK_CHECK_VERSION(3, 89, 0))
  if ((printing == NOT_PRINTING) &&
      (clear_cr))
    {
      free_cairo(ss->cr);
      ss->cr = NULL;
    }
#endif
}


void env_redisplay(void) 
{
  env_redisplay_1(NOT_PRINTING);
}


void env_redisplay_with_print(void) 
{
  env_redisplay_1(PRINTING);
}


void update_enved_background_waveform(chan_info *cp)
{
  if ((enved_dialog_is_active()) &&
      (enved_with_wave(ss)) &&
      (enved_target(ss) == ENVED_AMPLITUDE) &&
      (cp == active_channel) &&
      ((!apply_to_selection) || (selection_is_active_in_channel(cp))))
    env_redisplay();
}



static void enved_filter_order_callback(GtkWidget *w, gpointer data)
{
  set_enved_filter_order(gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(enved_order_label)));
}


static void clear_point_label(void);

static void clear_genv_error(void)
{
  if (brktxtL) 
    clear_point_label();
}


static gint unpost_genv_error(gpointer data)
{
  clear_genv_error();
  return(0);
}


static void errors_to_genv_text(const char *msg, void *data)
{
  gtk_label_set_text(GTK_LABEL(brktxtL), msg);
  g_timeout_add_full(0, (guint32)5000, unpost_genv_error, NULL, NULL);
}


static void text_field_activated(GtkWidget *w, gpointer context)
{ 
  /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if ((str) && (*str))
    {
      env *e = NULL;
      while (isspace((int)(*str))) str++;
      e = name_to_env(str);
      if (!e)
	{
	  if (isalpha((int)(str[0])))
	    {
	      alert_envelope_editor(str, copy_env(active_env));
	      add_or_edit_symbol(str, active_env);
	      set_sensitive(enved_save_button, false);
	      env_redisplay(); /* updates label */
	    }
	  else 
	    {
	      redirect_errors_to(errors_to_genv_text, NULL);
	      e = string_to_env(str);
	      redirect_errors_to(NULL, NULL);
	    }
	}
      if (e) 
	{
	  if (active_env)
	    {
	      #define ENVED_TEMP_NAME "enved-backup"
	      /* save current under a temp name!  -- user might have mistakenly reused a name */
	      alert_envelope_editor((char *)ENVED_TEMP_NAME, copy_env(active_env));
	      add_or_edit_symbol(ENVED_TEMP_NAME, active_env);
	      active_env = free_env(active_env);
	    }
	  active_env = copy_env(e);
	  set_enved_env_list_top(0);
	  prepare_env_edit(active_env);
	  set_sensitive(enved_save_button, true);
	  set_sensitive(enved_undo_button, false);
	  set_sensitive(enved_revert_button, false);
	  env_redisplay();
	  free_env(e);
	}
    }
}


static void enved_save_button_pressed(GtkWidget *w, gpointer context)
{
  char *name = NULL;
  if (!active_env) return;
  name = (char *)gtk_entry_get_text(GTK_ENTRY(enved_text_label));
  if ((!name) || (!(*name))) 
    name = (char *)"unnamed";
  alert_envelope_editor(name, copy_env(active_env));
  add_or_edit_symbol(name, active_env);
  set_sensitive(enved_save_button, false);
  env_redisplay();
}


static void apply_enved_callback(GtkWidget *w, gpointer context)
{
  /* apply current envs to currently sync'd channels */
  apply_enved();
  last_active_channel = active_channel;
}


static void undo_and_apply_enved_callback(GtkWidget *w, gpointer context)
{
  /* undo upto previous amp env, then apply */
  /* this blindly undoes the previous edit (assumed to be an envelope) -- if the user made some other change in the meantime, too bad */
  if ((active_channel) && (active_channel == last_active_channel))
    {
      active_channel->squelch_update = true;
      undo_edit_with_sync(active_channel, 1);
      active_channel->squelch_update = false;
      clear_status_area(active_channel->sound);
    }
  apply_enved();
  last_active_channel = active_channel;
}


static void reflect_segment_state(void)
{
  if ((enved_dialog) &&
      (active_env) && 
      (!(showing_all_envs)))
    env_redisplay();
}


static void select_or_edit_env(int pos)
{
  if (showing_all_envs)
    {
      showing_all_envs = false;
      set_button_label(show_button, "view envs");
    }
  if (active_env) active_env = free_env(active_env);
  selected_env = position_to_env(pos);
  if (!selected_env) return;
  active_env = selected_env;
  gtk_entry_set_text(GTK_ENTRY(enved_text_label), enved_all_names(pos));
  set_enved_env_list_top(0);
  prepare_env_edit(active_env);
  set_sensitive(enved_undo_button, false);
  set_sensitive(enved_revert_button, false);
  set_sensitive(enved_save_button, false);
  env_redisplay();
}


#define BLANK_LABEL "              "

static void clear_point_label(void)
{
  gtk_label_set_text(GTK_LABEL(brktxtL), BLANK_LABEL);
}


static char brkpt_buf[LABEL_BUFFER_SIZE];

static void enved_display_point_label(mus_float_t x, mus_float_t y)
{
  if ((enved_in_dB(ss)) && (min_dB(ss) < -60))
    snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.5f", x, y);
  else snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.3f", x, y);
  gtk_label_set_text(GTK_LABEL(brktxtL), brkpt_buf);
}


static gboolean enved_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  int evx, evy;
  GdkModifierType state;
  oclock_t motion_time = 0;
  ignore_button_release = false;

#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_state((GdkEvent *)ev, &state);
#else
  state = ev->state;
#endif
  if (BUTTON1_PRESSED(state))
    {
      if (EVENT_IS_HINT(ev))
	window_get_pointer(ev, &evx, &evy, &state);
      else
	{
#if (GTK_CHECK_VERSION(3, 0, 0))
          gdouble x, y;
	  gdk_event_get_coords((GdkEvent *)ev, &x, &y);
	  evx = (int)x;
	  evy = (int)y;
#else
	  evx = (int)(ev->x);
	  evy = (int)(ev->y);
#endif
	  motion_time = event_time(ev);
	  if ((motion_time - ss->enved->down_time) < 100) return(false);
	}
    }
  else return(false);

  if (!showing_all_envs)
    {
      mus_float_t x, y;
      env_editor_button_motion_with_xy(ss->enved, evx, evy, motion_time, active_env, &x, &y);
      enved_display_point_label(x, y);
      env_redisplay();
    }

  return(false);
}


static gboolean enved_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  gdouble x, y;
  ss->enved->down_time = event_time(ev);
  ss->enved->env_dragged = false;

#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_coords((GdkEvent *)ev, &x, &y);
#else
  x = ev->x;
  y = ev->y;
#endif

  if (showing_all_envs)
    {
      int pos;
      pos = hit_env((int)x, (int)y, env_window_width, env_window_height);
      slist_select(env_list, pos);
      if ((pos >= 0) && 
	  (pos < enved_all_envs_top())) 
	{
	  select_or_edit_env(pos);
	  ignore_button_release = true;
	}
    }
  else
    {
      if (!active_env)
	{
	  active_env = default_env(1.0, 0.0);
	  active_env->base = enved_base(ss);
	  env_redisplay(); /* needed to get current_xs set up correctly */
	}
      if (env_editor_button_press(ss->enved, (int)x, (int)y, event_time(ev), active_env))
	env_redisplay();
      enved_display_point_label(ungrf_x(ss->enved->axis, x), env_editor_ungrf_y_dB(ss->enved, y)); /* was (int)y? */
      set_sensitive(enved_save_button, true);
      set_sensitive(enved_undo_button, true);
      set_sensitive(enved_revert_button, true);
    }
  return(false);
}


static gboolean enved_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (ignore_button_release)
    ignore_button_release = false;
  else
    {
      if (!showing_all_envs)
	{
	  env_editor_button_release(ss->enved, active_env);
	  env_redisplay();
	  clear_point_label();
	}
    }
  return(false);
}

#if GTK_CHECK_VERSION(3, 89, 0)
static void enved_drawer_expose(GtkDrawingArea *w, cairo_t *cr, int width, int height, gpointer data)
{
  enved_cr = cr;
  cairo_set_line_width(cr, 1.0);
  env_window_width = widget_width(GTK_WIDGET(w));
  env_window_height = widget_height(GTK_WIDGET(w));
  env_redisplay();
}
#else
static gboolean enved_drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay();
  return(false);
}

static gboolean enved_drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  /* update display, can be either view of all envs or sequence of current envs */
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay();
  return(false);
}
#endif


static void show_button_pressed(GtkWidget *w, gpointer context)
{
  /* if show all (as opposed to show current), loop through loaded LV_LISTs */
  showing_all_envs = (!showing_all_envs);
  set_button_label(show_button, (showing_all_envs) ? "edit env" : "view envs");
  env_redisplay();
}


static void enved_selection_button_pressed(GtkWidget *w, gpointer context)
{
  apply_to_selection = (!apply_to_selection);
  widget_modify_bg(enved_selection_button, GTK_STATE_NORMAL, (apply_to_selection) ? ss->yellow : ss->basic_color);
  set_sensitive(enved_apply2_button, true);
  if ((enved_with_wave(ss)) && 
      (!showing_all_envs)) 
    env_redisplay();
}


static void enved_revert_button_pressed(GtkWidget *w, gpointer context)
{
  revert_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  if (!active_env)
    text_field_activated(enved_text_label, NULL);
  env_redisplay();
}


static void enved_undo_button_pressed(GtkWidget *w, gpointer context)
{
  undo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}


static void enved_redo_button_pressed(GtkWidget *w, gpointer context)
{
  redo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}


static void reflect_apply_state(void)
{
  gtk_label_set_text(GTK_LABEL(enved_name_label), env_names[enved_target(ss)]);
  widget_modify_bg(amp_button, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_AMPLITUDE) ? ss->yellow : ss->basic_color);
  widget_modify_bg(flt_button, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_SPECTRUM) ? ss->yellow : ss->basic_color);
  widget_modify_bg(src_button, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_SRATE) ? ss->yellow : ss->basic_color);
  if ((!showing_all_envs) && (enved_with_wave(ss))) env_redisplay();
}


static void flt_button_pressed(GtkWidget *w, gpointer context)
{
  in_set_enved_target(ENVED_SPECTRUM);
  old_clipping = enved_clipping(ss);
  set_enved_clipping(true);
  reflect_apply_state();
}


static void amp_button_pressed(GtkWidget *w, gpointer context)
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clipping(old_clipping);
  in_set_enved_target(ENVED_AMPLITUDE);
  reflect_apply_state();
}


static void src_button_pressed(GtkWidget *w, gpointer context)
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clipping(old_clipping);
  in_set_enved_target(ENVED_SRATE);
  reflect_apply_state();
}


static void enved_reset(void)
{
  set_enved_clipping(DEFAULT_ENVED_CLIPPING);
  set_enved_style(ENVELOPE_LINEAR);
  set_enved_power(DEFAULT_ENVED_POWER);
  set_enved_base(DEFAULT_ENVED_BASE);
  set_enved_target(DEFAULT_ENVED_TARGET);
  set_enved_with_wave(DEFAULT_ENVED_WITH_WAVE);
  set_enved_in_dB(DEFAULT_ENVED_IN_DB);
  set_enved_filter_order(DEFAULT_ENVED_FILTER_ORDER);
  if (active_env) active_env = free_env(active_env);
#if HAVE_SCHEME
  active_env = string_to_env("'(0 0 1 0)");
#endif
#if HAVE_FORTH
  active_env = string_to_env("'( 0 0 1 0 )");
#endif
#if HAVE_RUBY
  active_env = string_to_env("[0, 0, 1, 0]");
#endif
  set_enved_env_list_top(0);
  prepare_env_edit(active_env);
  set_sensitive(enved_save_button, true);
  reflect_enved_style();
  env_redisplay();
}


static void enved_reset_button_pressed(GtkWidget *w, gpointer context)
{
  enved_reset();
}


static void env_browse_callback(const char *name, int row, void *data)
{
  select_or_edit_env(row);
}


static void enved_graph_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_with_wave(TOGGLE_BUTTON_ACTIVE(w));
  env_redisplay();
}


static void enved_dB_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_in_dB(TOGGLE_BUTTON_ACTIVE(w));
  env_redisplay();
}


static void clip_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_clipping(TOGGLE_BUTTON_ACTIVE(w));
}


static void make_base_label(mus_float_t bval)
{
  char *sfs, *buf;
  int i, len, scale_len;

  len = (int)(enved_power(ss) * 4);
  if (len < 32) len = 32;

  sfs = (char *)calloc(len, sizeof(char));
  snprintf(sfs, len, "%.3f", bval);

  scale_len = (int)(enved_power(ss) + 3);
  if (scale_len < 32) scale_len = 32;
  buf = (char *)calloc(scale_len, sizeof(char));

  for (i = 0; i < scale_len - 1; i++) buf[i] = sfs[i];
  gtk_label_set_text(GTK_LABEL(baseValue), buf);

  free(sfs);
  free(buf);

  in_set_enved_base(bval);

  if ((active_env) && 
      (!(showing_all_envs))) 
    {
      if (enved_save_button) set_sensitive(enved_save_button, true); /* what about undo/redo here? */

      active_env->base = enved_base(ss);
      if (active_env->base == 1.0)
	set_enved_style(ENVELOPE_LINEAR);
      else set_enved_style(ENVELOPE_EXPONENTIAL);

      env_redisplay();
    }
}


static void base_changed(mus_float_t val)
{
  mus_float_t bval;
  if (val == 0) 
    bval = 0.0;
  else 
    {
      if (val == 0.5)
	bval = 1.0;
      else
	{
	  if (val > 0.5)
	    bval = pow(1.0 + (10.0 * ((val - 0.5) * 2)), enved_power(ss));  
	  else 
	    bval = pow((val * 2), enved_power(ss) - 1.0);
	}
    }
  make_base_label(bval);
}


static void reflect_changed_base(mus_float_t val)
{
  mus_float_t ival;
  if (val <= 0.0) 
    ival = 0.0;
  else
    {
      if (val == 1.0)
	ival = 0.5;
      else
	{
	  if (val <= 1.0)
	    ival = 0.5 * pow(val, 1.0 / (enved_power(ss) - 1.0));
	  else 
	    {
	      if (enved_power(ss) != 0.0)
		ival = (0.5 + 0.05 * (pow(val, (1.0 / (enved_power(ss)))) - 1));
	      else ival = 0.5; /* ?? maybe 5.0? */
	    }
	}
    }
  if (baseAdj)
    {
      ADJUSTMENT_SET_VALUE(baseAdj, ival);
      make_base_label(val);
    }
}


static void make_linear(GtkWidget *w, gpointer context)
{
  reflect_changed_base(1.0);
  set_enved_style(ENVELOPE_LINEAR);
}


static void base_changed_callback(GtkAdjustment *adj, gpointer context)
{
  base_changed(ADJUSTMENT_VALUE(adj));
}


static gboolean fir_button_pressed(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  is_FIR = (!is_FIR);
  gtk_label_set_text(GTK_LABEL(fir_button), (is_FIR) ? "fir" : "fft");
  if (enved_with_wave(ss)) env_redisplay();
  return(false);
}


static void reflect_sound_state(void)
{
  bool file_on;
  file_on = (bool)(any_selected_sound());
  set_sensitive(enved_apply_button, file_on);
  set_sensitive(enved_apply2_button, file_on);
}


static Xen reflect_file_in_enved(Xen hook_or_reason)
{
  if (enved_dialog) reflect_sound_state();
  return(Xen_false);
}

Xen_wrap_1_arg(reflect_file_in_enved_w, reflect_file_in_enved)

#define BB_MARGIN 3

GtkWidget *create_envelope_editor(void)
{
  if (!enved_dialog)
    {
      GtkWidget *mainform, *enved_help_button, *leftbox, *bottombox, *leftframe, *toprow, *bottomrow;

      enved_dialog = gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(enved_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      add_dialog_style(enved_dialog);
      SG_SIGNAL_CONNECT(enved_dialog, "delete_event", delete_enved_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(enved_dialog), "Edit Envelope");
      sg_make_resizable(enved_dialog);
      sg_container_set_border_width(GTK_CONTAINER(enved_dialog), 4);
      gtk_widget_realize(enved_dialog);
      gtk_window_resize(GTK_WINDOW(enved_dialog), 500, 500);
      widget_modify_bg(enved_dialog, GTK_STATE_NORMAL, ss->basic_color);

      gc = gc_new();
      gc_set_background(gc, ss->white);
      gc_set_foreground(gc, ss->black);

      rgc = gc_new();
      gc_set_background(rgc, ss->white);
      gc_set_foreground(rgc, ss->red);

      ggc = gc_new();
      gc_set_background(ggc, ss->white);
      gc_set_foreground(ggc, ss->enved_waveform_color);

      enved_help_button = gtk_dialog_add_button(GTK_DIALOG(enved_dialog), "Help", GTK_RESPONSE_NONE);
      enved_reset_button = gtk_dialog_add_button(GTK_DIALOG(enved_dialog), "Clear graph", GTK_RESPONSE_NONE);
      enved_cancel_button = gtk_dialog_add_button(GTK_DIALOG(enved_dialog), I_GO_AWAY, GTK_RESPONSE_NONE);
      enved_apply2_button = gtk_dialog_add_button(GTK_DIALOG(enved_dialog), "Undo&Apply", GTK_RESPONSE_NONE);
      enved_apply_button = gtk_dialog_add_button(GTK_DIALOG(enved_dialog), "Apply", GTK_RESPONSE_NONE);

      gtk_widget_set_name(enved_help_button, "dialog_button");
      gtk_widget_set_name(enved_cancel_button, "dialog_button");
      gtk_widget_set_name(enved_apply_button, "dialog_button");
      gtk_widget_set_name(enved_apply2_button, "dialog_button");
      gtk_widget_set_name(enved_reset_button, "dialog_button");

      SG_SIGNAL_CONNECT(enved_cancel_button, "clicked", dismiss_enved_callback, NULL);
      SG_SIGNAL_CONNECT(enved_reset_button, "clicked", enved_reset_button_pressed, NULL);
      SG_SIGNAL_CONNECT(enved_help_button, "clicked", help_enved_callback, NULL);
      SG_SIGNAL_CONNECT(enved_apply2_button, "clicked", undo_and_apply_enved_callback, NULL);
      SG_SIGNAL_CONNECT(enved_apply_button, "clicked", apply_enved_callback, NULL);

#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(enved_cancel_button);
      add_highlight_button_style(enved_apply_button);
      add_highlight_button_style(enved_apply2_button);
      add_highlight_button_style(enved_reset_button);
      add_highlight_button_style(enved_help_button);
#endif
      gtk_widget_show(enved_cancel_button);
      gtk_widget_show(enved_apply_button);
      gtk_widget_show(enved_apply2_button);
      gtk_widget_show(enved_reset_button);
      gtk_widget_show(enved_help_button);

      mainform = gtk_hbox_new(false, 0); /* buttons + graph */
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(enved_dialog)), mainform, true, true, 0);

      leftframe = gtk_frame_new(NULL);
      sg_box_pack_start(GTK_BOX(mainform), leftframe, false, false, 0);
      gtk_frame_set_shadow_type(GTK_FRAME(leftframe), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(leftframe);
      widget_modify_bg(leftframe, GTK_STATE_NORMAL, ss->black);

      leftbox = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(leftframe), leftbox);
      gtk_widget_show(leftbox);

      bottombox = gtk_vbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(enved_dialog)), bottombox, false, false, 4);
      gtk_widget_show(bottombox);


      enved_drawer = gtk_drawing_area_new();
      sg_box_pack_start(GTK_BOX(mainform), enved_drawer, true, true, 0);
#if (!GTK_CHECK_VERSION(3, 89, 0))
      sg_widget_set_events(enved_drawer, GDK_ALL_EVENTS_MASK);
#endif
      widget_modify_bg(enved_drawer, GTK_STATE_NORMAL, ss->white);
      widget_modify_fg(enved_drawer, GTK_STATE_NORMAL, ss->black);
      gtk_widget_show(enved_drawer);

      show_button = gtk_button_new_with_label("view envs");
      gtk_button_set_relief(GTK_BUTTON(show_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(leftbox), show_button, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(show_button, "clicked", show_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(show_button);
#endif
      gtk_widget_show(show_button);

      enved_save_button = gtk_button_new_with_label("define it");
      gtk_button_set_relief(GTK_BUTTON(enved_save_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(leftbox), enved_save_button, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(enved_save_button, "clicked", enved_save_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(enved_save_button);
#endif
      gtk_widget_show(enved_save_button);

      enved_revert_button = gtk_button_new_with_label("revert ");
      gtk_button_set_relief(GTK_BUTTON(enved_revert_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(leftbox), enved_revert_button, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(enved_revert_button, "clicked", enved_revert_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(enved_revert_button);
#endif
      gtk_widget_show(enved_revert_button);

      unrow = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(leftbox), unrow, false, false, BB_MARGIN);
      gtk_widget_show(unrow);

      enved_undo_button = gtk_button_new_with_label(" undo ");
      gtk_button_set_relief(GTK_BUTTON(enved_undo_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(unrow), enved_undo_button, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(enved_undo_button, "clicked", enved_undo_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(enved_undo_button);
#endif
      gtk_widget_show(enved_undo_button);

      enved_redo_button = gtk_button_new_with_label(" redo ");
      gtk_button_set_relief(GTK_BUTTON(enved_redo_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(unrow), enved_redo_button, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(enved_redo_button, "clicked", enved_redo_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(enved_redo_button);
#endif
      gtk_widget_show(enved_redo_button);

      rbrow = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(leftbox), rbrow, false, false, BB_MARGIN);
      gtk_widget_show(rbrow);

      amp_button = gtk_button_new_with_label("amp");
      gtk_button_set_relief(GTK_BUTTON(amp_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(rbrow), amp_button, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(amp_button, "clicked", amp_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(amp_button);
#endif
      gtk_widget_show(amp_button);

      flt_button = gtk_button_new_with_label("flt");
      gtk_button_set_relief(GTK_BUTTON(flt_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(rbrow), flt_button, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(flt_button, "clicked", flt_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(flt_button);
#endif
      gtk_widget_show(flt_button);

      src_button = gtk_button_new_with_label("src");
      gtk_button_set_relief(GTK_BUTTON(src_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(rbrow), src_button, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(src_button, "clicked", src_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(src_button);
#endif
      gtk_widget_show(src_button);


      selrow = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(leftbox), selrow, false, false, BB_MARGIN);
      gtk_widget_show(selrow);

      enved_selection_button = gtk_button_new_with_label("selection");
      gtk_button_set_relief(GTK_BUTTON(enved_selection_button), SG_RELIEF_HALF);
      sg_box_pack_start(GTK_BOX(selrow), enved_selection_button, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(enved_selection_button, "clicked", enved_selection_button_pressed, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(enved_selection_button);
#endif
      gtk_widget_show(enved_selection_button);

      env_list = slist_new_with_title("envs:", leftbox, NULL, 0, BOX_PACK);
      env_list->select_callback = env_browse_callback;
      if (enved_all_envs_top() > 0) make_scrolled_env_list();


      toprow = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(bottombox), toprow, false, false, 0);
      gtk_widget_show(toprow);

      bottomrow = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(bottombox), bottomrow, false, false, 0);
      gtk_widget_show(bottomrow);

      {
	GtkWidget* sep;
	sep = gtk_hseparator_new();
	sg_box_pack_end(GTK_BOX(DIALOG_CONTENT_AREA(enved_dialog)), sep, false, false, 8);
	gtk_widget_show(sep);
      }

      #define LEFT_MARGIN 8

      enved_name_label = gtk_label_new("amp env:");
      widget_set_margin_left(enved_name_label, LEFT_MARGIN);
      sg_box_pack_start(GTK_BOX(toprow), enved_name_label, false, false, 0);
      gtk_widget_show(enved_name_label);

      enved_text_label = snd_entry_new_with_size(toprow, 28);
      SG_SIGNAL_CONNECT(enved_text_label, "activate", text_field_activated, NULL);

      brktxtL = gtk_label_new(BLANK_LABEL);
      sg_box_pack_start(GTK_BOX(toprow), brktxtL, false, false, 6);
      gtk_widget_show(brktxtL);


      enved_dB_button = gtk_check_button_new_with_label("dB");
      SG_SIGNAL_CONNECT(enved_dB_button, "toggled", enved_dB_button_callback, NULL);
      sg_box_pack_end(GTK_BOX(toprow), enved_dB_button, false, false, 4);
      gtk_widget_show(enved_dB_button);

      enved_graph_button = gtk_check_button_new_with_label("wave");
      SG_SIGNAL_CONNECT(enved_graph_button, "toggled", enved_graph_button_callback, NULL);
      sg_box_pack_end(GTK_BOX(toprow), enved_graph_button, false, false, 4);
      gtk_widget_show(enved_graph_button);

      clip_button = gtk_check_button_new_with_label("clip");
      SG_SIGNAL_CONNECT(clip_button, "toggled", clip_button_callback, NULL);
      sg_box_pack_end(GTK_BOX(toprow), clip_button, false, false, 4);
      gtk_widget_show(clip_button);


      baseLabel = gtk_label_new("exp:");
      widget_set_margin_left(baseLabel, LEFT_MARGIN);
      sg_box_pack_start(GTK_BOX(bottomrow), baseLabel, false, false, 4);
      gtk_widget_show(baseLabel);

      baseValue = gtk_label_new("1.000");
      sg_box_pack_start(GTK_BOX(bottomrow), baseValue, false, false, 4);
      gtk_widget_show(baseValue);


      lerow = gtk_vbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(bottomrow), lerow, true, true, 8);
      gtk_widget_show(lerow);

      {
	GtkWidget* sep;
	sep = gtk_vseparator_new();
	sg_box_pack_start(GTK_BOX(lerow), sep, false, false, 8);
	gtk_widget_show(sep);
      }

      baseAdj = (GtkAdjustment *)gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
      baseScale = gtk_hscrollbar_new(GTK_ADJUSTMENT(baseAdj));
      widget_modify_bg(baseScale, GTK_STATE_NORMAL, ss->position_color);
      SG_SIGNAL_CONNECT(baseAdj, "value_changed", base_changed_callback, NULL);
      /* sg_box_pack_start(GTK_BOX(bottomrow), baseScale, true, true, 4); */
      sg_box_pack_start(GTK_BOX(lerow), baseScale, true, true, 0);
      gtk_widget_show(baseScale);


      {
	/* try to center the linear button */
	GtkWidget *hr, *rb, *lb;

	hr = gtk_hbox_new(false, 0);
	sg_box_pack_start(GTK_BOX(lerow), hr, false, false, 4);
	gtk_widget_show(hr);

	rb = gtk_label_new("");
	sg_box_pack_start(GTK_BOX(hr), rb, true, true, 2);
	gtk_widget_show(rb);

	lb = gtk_label_new("");
	sg_box_pack_end(GTK_BOX(hr), lb, true, true, 2);
	gtk_widget_show(lb);

	lin_button = gtk_button_new_with_label("1.0");
	sg_box_pack_start(GTK_BOX(hr), lin_button, false, false, 4);
	SG_SIGNAL_CONNECT(lin_button, "clicked", make_linear, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
	add_center_button_style(lin_button); 
#endif
	gtk_widget_show(lin_button);
      }

      orderAdj = (GtkAdjustment *)gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      enved_order_label = gtk_spin_button_new(GTK_ADJUSTMENT(orderAdj), 0.0, 0);
      sg_box_pack_end(GTK_BOX(bottomrow), enved_order_label, false, false, 4);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(enved_order_label), true);
      SG_SIGNAL_CONNECT(orderAdj, "value_changed", enved_filter_order_callback, NULL);
      SG_SIGNAL_CONNECT(enved_order_label, "enter_notify_event", spin_button_focus_callback, NULL);
      SG_SIGNAL_CONNECT(enved_order_label, "leave_notify_event", spin_button_unfocus_callback, NULL);
      gtk_widget_show(enved_order_label);

      /* fir_button = gtk_button_new_with_label((is_FIR) ? "fir" : "fft"); */
      /* SG_SIGNAL_CONNECT(eb, "clicked", fir_button_pressed, NULL); */
      {
#if (!GTK_CHECK_VERSION(3, 92, 1))
	GtkWidget *eb;
	eb = gtk_event_box_new();
	sg_box_pack_end(GTK_BOX(bottomrow), eb, false, false, 4);
	widget_set_margin_left(eb, 8);
	widget_modify_bg(eb, GTK_STATE_NORMAL, ss->basic_color);
	sg_widget_set_events(eb, GDK_BUTTON_PRESS_MASK);
	SG_SIGNAL_CONNECT(eb, "button_press_event", fir_button_pressed, NULL);
	gtk_widget_show(eb);

	fir_button = gtk_label_new("fir");
	gtk_container_add(GTK_CONTAINER(eb), fir_button);
	gtk_widget_show(fir_button);
#else
	fir_button = gtk_label_new("fir");
	sg_box_pack_end(GTK_BOX(bottomrow), fir_button, false, false, 4);
	SG_SIGNAL_CONNECT(fir_button, "button_press_event", fir_button_pressed, NULL);
	gtk_widget_show(fir_button);
#endif
      }

      gtk_widget_show(mainform);
      gtk_widget_show(enved_dialog);

      axis = (axis_info *)calloc(1, sizeof(axis_info));
      axis->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      axis->ax->wn = WIDGET_TO_WINDOW(enved_drawer);
      axis->ax->w = enved_drawer;
      axis->ax->gc = gc;
      axis->ax->current_font = AXIS_NUMBERS_FONT(ss);

      gray_ap = (axis_info *)calloc(1, sizeof(axis_info));
      gray_ap->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      gray_ap->graph_active = true;
      gray_ap->ax->wn = WIDGET_TO_WINDOW(enved_drawer);
      gray_ap->ax->w = enved_drawer;
      gray_ap->ax->gc = ggc;
      gray_ap->ax->current_font = AXIS_NUMBERS_FONT(ss);

#if GTK_CHECK_VERSION(3, 89, 0)
      gtk_drawing_area_set_content_width(GTK_DRAWING_AREA(enved_drawer), gtk_widget_get_allocated_width(enved_drawer));
      gtk_drawing_area_set_content_height(GTK_DRAWING_AREA(enved_drawer), gtk_widget_get_allocated_height(enved_drawer));
      gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(enved_drawer), enved_drawer_expose, NULL, NULL);
#else
      SG_SIGNAL_CONNECT(enved_drawer, DRAW_SIGNAL, enved_drawer_expose, NULL);
      SG_SIGNAL_CONNECT(enved_drawer, "configure_event", enved_drawer_resize, NULL);
#endif
      SG_SIGNAL_CONNECT(enved_drawer, "button_press_event", enved_drawer_button_press, NULL);
      SG_SIGNAL_CONNECT(enved_drawer, "button_release_event", enved_drawer_button_release, NULL);
      SG_SIGNAL_CONNECT(enved_drawer, "motion_notify_event", enved_drawer_button_motion, NULL);

      if (enved_all_envs_top() == 0)
	set_sensitive(show_button, false);
      set_sensitive(enved_revert_button, false);
      set_sensitive(enved_undo_button, false);
      set_sensitive(enved_redo_button, false);
      set_sensitive(enved_save_button, false);
      if (!(selection_is_active())) 
	set_sensitive(enved_selection_button, false);

      set_toggle_button(clip_button, enved_clipping(ss), false, NULL);
      set_toggle_button(enved_graph_button, enved_with_wave(ss), false, NULL);
      set_toggle_button(enved_dB_button, enved_in_dB(ss), false, NULL);

      reflect_apply_state();
      reflect_segment_state();
      reflect_sound_state();

      set_dialog_widget(ENVED_DIALOG, enved_dialog);

      Xen_add_to_hook_list(ss->snd_open_file_hook, reflect_file_in_enved_w, "enved-file-open-handler", "enved dialog's file-open-hook handler");
    }
  else raise_dialog(enved_dialog);

  env_window_width = widget_width(enved_drawer);
  env_window_height = widget_height(enved_drawer);
  active_channel = current_channel();
#if (!GTK_CHECK_VERSION(3, 89, 0))
  env_redisplay();
#endif
  return(enved_dialog);
}


void set_enved_clipping(bool val) 
{
  in_set_enved_clipping(val); 
  if (enved_dialog) set_toggle_button(clip_button, val, false, NULL);
}


void reflect_enved_style(void)
{
  reflect_segment_state();
}


void set_enved_target(enved_target_t val) 
{
  in_set_enved_target(val); 
  if (enved_dialog) reflect_apply_state();
}


void set_enved_with_wave(bool val) 
{
  in_set_enved_with_wave(val); 
  if (enved_dialog) set_toggle_button(enved_graph_button, val, false, NULL);
}


void set_enved_in_dB(bool val) 
{
  in_set_enved_in_dB(val); 
  if (enved_dialog) set_toggle_button(enved_dB_button, val, false, NULL);
}


void set_enved_base(mus_float_t val) 
{
  in_set_enved_base(val); 
  if (enved_dialog) reflect_changed_base(val);
}


bool enved_dialog_is_active(void)
{
  return((enved_dialog) && (widget_is_active(enved_dialog)));
}


void set_enved_filter_order(int order)
{
  if ((order > 0) && (order < 2000))
    {
      if (order & 1) 
	{in_set_enved_filter_order(order + 1);}
      else {in_set_enved_filter_order(order);}
      if (enved_dialog)
	{
	  widget_int_to_text(enved_order_label, enved_filter_order(ss));
	  if ((enved_target(ss) == ENVED_SPECTRUM) && 
	      (enved_with_wave(ss)) && 
	      (!showing_all_envs)) 
	    env_redisplay();
	}
    }
}


void enved_reflect_selection(bool on)
{
  if ((enved_dialog) && (!within_selection_src))
    {
      set_sensitive(enved_selection_button, on);
      if ((apply_to_selection) && (!on))
	{
	  apply_to_selection = false;
	  we_turned_selection_off = true;
	}
      if ((on) && (we_turned_selection_off))
	{
	  apply_to_selection = true;
	}
      widget_modify_bg(enved_selection_button, GTK_STATE_NORMAL, (apply_to_selection) ? ss->yellow : ss->basic_color);
      if ((enved_target(ss) != ENVED_SPECTRUM) && 
	  (enved_with_wave(ss)) && 
	  (!showing_all_envs)) 
	env_redisplay();
    }
}


void color_enved_waveform(color_info *pix)
{
  if (enved_dialog)
    {
      gc_set_foreground(ggc, pix);
      if ((enved_with_wave(ss)) && (enved_dialog)) env_redisplay();
    }
}


static Xen g_enved_envelope(void)
{
  #define H_enved_envelope "(" S_enved_envelope "): current envelope editor displayed (active) envelope"
  return(env_to_xen(active_env));
}


static Xen g_set_enved_envelope(Xen e)
{
  Xen_check_type(Xen_is_list(e) || Xen_is_string(e) || Xen_is_symbol(e), e, 1, S_set S_enved_envelope, "a list, symbol, or string");
  if (active_env) active_env = free_env(active_env);
  if ((Xen_is_string(e)) || (Xen_is_symbol(e)))
    active_env = name_to_env((Xen_is_string(e)) ? Xen_string_to_C_string(e) : Xen_symbol_to_C_string(e));
  else active_env = xen_to_env(e);
  if ((!active_env) && (!(Xen_is_list(e))))
    Xen_error(Xen_make_error_type("no-such-envelope"),
	      Xen_list_2(C_string_to_Xen_string(S_set S_enved_envelope ": bad envelope arg: ~A"),
			 e));
  if (enved_dialog) 
    env_redisplay();
  return(e);
}


static Xen g_enved_filter(void)
{
  #define H_enved_filter "(" S_enved_filter "): envelope editor FIR/FFT filter choice (" PROC_TRUE ": FIR)"
  return(C_bool_to_Xen_boolean(is_FIR));
}


static Xen g_set_enved_filter(Xen type)
{
  Xen_check_type(Xen_is_boolean(type), type, 1, S_set S_enved_filter, "boolean");
  is_FIR = Xen_boolean_to_C_bool(type);
  if (fir_button)
    gtk_label_set_text(GTK_LABEL(fir_button), (is_FIR) ? "fir" : "fft");
  return(type);
}


Xen_wrap_no_args(g_enved_filter_w, g_enved_filter)
Xen_wrap_1_arg(g_set_enved_filter_w, g_set_enved_filter)
Xen_wrap_no_args(g_enved_envelope_w, g_enved_envelope)
Xen_wrap_1_arg(g_set_enved_envelope_w, g_set_enved_envelope)

void g_init_gxenv(void)
{
#if HAVE_SCHEME
  s7_pointer pcl_b, pcl_t;
  pcl_b = s7_make_circular_signature(s7, 0, 1, s7_make_symbol(s7, "boolean?"));
  pcl_t = s7_make_circular_signature(s7, 0, 1, s7_t(s7));
#endif

  Xen_define_typed_dilambda(S_enved_filter, g_enved_filter_w, H_enved_filter,
			    S_set S_enved_filter, g_set_enved_filter_w,  0, 0, 1, 0, pcl_b, pcl_b);

  Xen_define_typed_dilambda(S_enved_envelope, g_enved_envelope_w, H_enved_envelope,
			    S_set S_enved_envelope, g_set_enved_envelope_w,  0, 0, 1, 0, pcl_t, pcl_t);
}

