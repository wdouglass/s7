#include "snd.h"

/* ---------------- HELP MONOLOG ---------------- */

static GtkWidget *help_dialog = NULL;

static void dismiss_help_dialog(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(help_dialog);
}


static gint delete_help_dialog(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(help_dialog);
  return(true);
}


#define HELP_ROWS 12
#define HELP_COLUMNS 72
/* these set the initial size of the help dialog text area */

static GtkWidget *help_text = NULL;
slist *related_items = NULL;
static char *original_help_text = NULL;
static const char **help_urls = NULL;

static void add_help_text(GtkWidget *text, const char *message)
{
  sg_text_insert(text, (char *)message);
}


int help_text_width(const char *txt, int start, int end)
{
#if 0
  char *buf;
  int len;
  buf = (char *)calloc(end - start + 2, sizeof(char));
  strncpy(buf, txt, end - start);
  len = sg_text_width(buf, LISTENER_FONT(ss));
  free(buf);
  if (len > 0) return(len);
#endif
  return((end - start) * 8);
}


static with_word_wrap_t outer_with_wrap = WITHOUT_WORD_WRAP;
static int old_help_text_width = 0;

static gboolean help_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  int curwid;
  curwid = widget_width(help_dialog); /* was help_text, but I'm getting shivering in expose events */
  if (old_help_text_width == 0)
    old_help_text_width = curwid;
  else
    {
      if ((outer_with_wrap == WITH_WORD_WRAP) && 
	  (abs(curwid - old_help_text_width) > 50))
	{
	  char *cur_help_str, *new_help_str;
	  cur_help_str = sg_get_text(help_text, 0, -1);
	  new_help_str = word_wrap(original_help_text, curwid);
	  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);
	  sg_text_insert(help_text, new_help_str);
	  if (new_help_str) free(new_help_str);
	  if (cur_help_str) g_free(cur_help_str);
	  old_help_text_width = curwid;
	}
    }
  return(false);
}


static bool new_help(const char *pattern, bool complain)
{
  const char *url;
  const char **xrefs;
  url = snd_url(pattern);
  if (url)
    {
      /* given name, find doc string, if any */
      Xen xstr;
      xstr = g_snd_help(C_string_to_Xen_string(pattern), 0);
      if (Xen_is_string(xstr))
	{
	  int gc_loc;
	  gc_loc = snd_protect(xstr);
	  xrefs = help_name_to_xrefs(pattern);
	  snd_help_with_xrefs(pattern, Xen_string_to_C_string(xstr), WITH_WORD_WRAP, xrefs, NULL);
	  snd_unprotect_at(gc_loc);
	  if (xrefs) free(xrefs);
	  return(true);
	}
      url_to_html_viewer(url);
      return(true);
    }
  if ((!(snd_topic_help(pattern))) && (complain))
    {
      xrefs = help_name_to_xrefs(pattern);
      if (xrefs)
	{
	  snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, xrefs, NULL);
	  free(xrefs);
	  return(true);
	}
      else snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, NULL, NULL);
    }
  return(false);
}


static char *find_highlighted_text(const char *value)
{
  int i, len, start = -1;
  len = mus_strlen(value);
  for (i = 0; i < len; i++)
    if (value[i] == '{')
      start = i + 1;
    else 
      {
	if (value[i] == '}')
	  {
	    int end;
	    end = i;
	    if ((start > 0) && ((end - start) > 0))
	      {
		int k;
		char *topic;
		topic = (char *)calloc(end - start + 1, sizeof(char));
		for (i = start, k = 0; i < end; i++, k++)
		  topic[k] = value[i];
		return(topic);
	      }
	  }
      }
  return(NULL);
}


static void help_browse_callback(const char *name, int row, void *data)
{
  if ((help_urls) && (help_urls[row]))
    url_to_html_viewer(help_urls[row]);
  else
    {
      char *topic;
      topic = find_highlighted_text(name);
      if (topic)
	{
	  name_to_html_viewer(topic);
	  free(topic);
	}
      else
	{
	  if (name)
	    new_help(name, true);
	}
    }
}


static gboolean text_release_callback(GtkTreeSelection *selection, gpointer *gp)
{
  /* this needs to be bool return false -- otherwise, apparently, the mouse-drag->selection never gets turned off! */
  GtkTextIter start, end;
  #define HELP_BUFFER gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text))

  if (gtk_text_buffer_get_selection_bounds(HELP_BUFFER, &start, &end))
    {
      char *txt;
      txt = gtk_text_buffer_get_text(HELP_BUFFER, &start, &end, true);
      /* only change help text display if it's one word in the selection, and we can find new help */
      if (txt)
	{
	  int i, len;
	  bool one_word = true;
	  len = mus_strlen(txt);
	  for (i = 0; i < len; i++)
	    if (isspace((int)txt[i]))
	      {
		one_word = false;
		break;
	      }
	  if (one_word) new_help(txt, false);
	  g_free(txt);
	}
    }

  return(false);
}


static void search_activated(GtkWidget *w, gpointer context)
{ 
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w)); /* no free, I think */
  if (new_help(str, true))
    gtk_entry_set_text(GTK_ENTRY(w), "");
}


static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button, *search, *frame;
  help_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(help_dialog), GTK_WINDOW(main_shell(ss)));
#endif
  SG_SIGNAL_CONNECT(help_dialog, "delete_event", delete_help_dialog, NULL);

  gtk_window_set_title(GTK_WINDOW(help_dialog), I_HELP);
  sg_make_resizable(help_dialog);
  sg_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_window_resize(GTK_WINDOW(help_dialog), HELP_COLUMNS * 9, HELP_ROWS * 40);
  gtk_widget_realize(help_dialog);

  ok_button = gtk_dialog_add_button(GTK_DIALOG(help_dialog), "Go Away", GTK_RESPONSE_NONE);
  gtk_widget_set_name(ok_button, "dialog_button");
  SG_SIGNAL_CONNECT(ok_button, "clicked", dismiss_help_dialog, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(ok_button);
#endif
  gtk_widget_show(ok_button);

  frame = gtk_frame_new(NULL);
  sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(help_dialog)), frame, true, true, 10); 
  gtk_widget_show(frame);

  help_text = make_scrolled_text(frame, false, CONTAINER_ADD, false); /* last arg ignored */
#if (!GTK_CHECK_VERSION(3, 92, 1))
  gtk_widget_add_events(help_text, GDK_BUTTON_RELEASE);
#endif
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(help_text), 10);
  SG_SIGNAL_CONNECT(help_text, "button_release_event", text_release_callback, NULL);

  related_items = slist_new_with_title("related topics", DIALOG_CONTENT_AREA(help_dialog), NULL, 0, BOX_PACK);
  related_items->select_callback = help_browse_callback;

  {
    GtkWidget *label, *hbox;
#if (!HAVE_GTK_GRID_NEW)
    hbox = gtk_hbox_new(false, 0);
    sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(help_dialog)), hbox, false, false, 10); 
    gtk_widget_show(hbox);

    label = gtk_label_new("help topic:");
    sg_box_pack_start(GTK_BOX(hbox), label, false, false, 0); 
    gtk_widget_show(label);
    
    search = snd_entry_new(hbox, NULL, WITH_WHITE_BACKGROUND);
#else
    GtkWidget *content_area;
    content_area = gtk_dialog_get_content_area(GTK_DIALOG(help_dialog));

    hbox = gtk_grid_new();
    gtk_container_add(GTK_CONTAINER(content_area), hbox);
    gtk_widget_show(hbox);

    label = gtk_label_new("help topic:");
    gtk_widget_set_halign(label, GTK_ALIGN_CENTER);
    gtk_widget_set_hexpand(label, false);
    g_object_set(label, "margin", 10, NULL);
    gtk_grid_attach(GTK_GRID(hbox), label, 0, 0, 1, 1);
    gtk_widget_show(label);

    search = snd_entry_new(hbox, label, WITH_WHITE_BACKGROUND);
#endif
  }

  SG_SIGNAL_CONNECT(search, "activate", search_activated, NULL);
  gtk_widget_show(help_dialog);
  SG_SIGNAL_CONNECT((gpointer)help_dialog, DRAW_SIGNAL, help_expose_callback, NULL);
  set_dialog_widget(HELP_DIALOG, help_dialog);
}


GtkWidget *snd_help(const char *subject, const char *helpstr, with_word_wrap_t with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */

  outer_with_wrap = with_wrap;
  if (!(help_dialog)) 
    create_help_monolog(); 
  else raise_dialog(help_dialog);

  gtk_window_set_title(GTK_WINDOW(help_dialog), subject);
  original_help_text = (char *)helpstr;
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);

  if (with_wrap == WITH_WORD_WRAP)
    {
      char *new_help;
      new_help = word_wrap(helpstr, (int)(widget_width(help_text) * 1.3));
      add_help_text(help_text, new_help);
      if (new_help) free(new_help);
    }
  else add_help_text(help_text, helpstr);

  slist_clear(related_items); /* this can clobber "subject"! */
  return(help_dialog);
}


GtkWidget *snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, const char **xrefs, const char **urls)
{
  GtkWidget *w;
  help_urls = urls;
  w = snd_help(subject, helpstr, with_wrap);
  if (xrefs)
    {
      int i;
      for (i = 0; (xrefs[i]); i++)
	slist_append(related_items, xrefs[i]);
    }
  return(w);
}


void snd_help_append(const char *text)
{
  if (help_text) 
    sg_text_insert(help_text, text);
}


void snd_help_back_to_top(void)
{
  if (help_text)
    {
      GtkTextIter pos;
      GtkTextBuffer *buf;
      buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));
      gtk_text_buffer_get_iter_at_offset(buf, &pos, 0);
      gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(help_text), &pos, 0.0, true, 0.0, 0.0);
    }
}


/* -------------------------------- FIND DIALOG -------------------------------- */

static GtkWidget *edit_find_dialog, *edit_find_text, *find_cancel_button, *edit_find_label, *find_next_button, *find_previous_button;
static GtkWidget *find_error_frame = NULL, *find_error_label = NULL;
static chan_info *find_channel = NULL;
static gulong find_key_press_handler_id = 0;


static void clear_find_error(void)
{
  if ((find_error_frame) && (widget_is_active(find_error_frame)))
    set_label(find_error_label, "");
  if (find_key_press_handler_id)
    {
      g_signal_handler_disconnect(edit_find_text, find_key_press_handler_id);
      find_key_press_handler_id = 0;
    }
}


static gboolean find_modify_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  clear_find_error();
  return(false);
}


void errors_to_find_text(const char *msg, void *data)
{
  find_dialog_set_label(msg);
  gtk_widget_show(find_error_frame);
  find_key_press_handler_id = SG_SIGNAL_CONNECT(edit_find_text, "key_press_event", find_modify_key_press, NULL);
}


void stop_search_if_error(const char *msg, void *data)
{
  errors_to_find_text(msg, data);
  ss->stopped_explicitly = true; /* should be noticed in global_search in snd-find.c */
}


static void edit_find_dismiss(GtkWidget *w, gpointer context) 
{ 
  if (ss->checking_explicitly)
    ss->stopped_explicitly = true;
  else 
    {
      gtk_widget_hide(edit_find_dialog);
      clear_find_error();
    }
} 


static gint edit_find_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  clear_find_error();
  gtk_widget_hide(edit_find_dialog);
  return(true);
}


static void edit_find_help(GtkWidget *w, gpointer context) 
{
  find_dialog_help();
} 


static void edit_find_find(read_direction_t direction, GtkWidget *w, gpointer context) 
{
#if HAVE_EXTENSION_LANGUAGE
  find_dialog_find((char *)gtk_entry_get_text(GTK_ENTRY(edit_find_text)), direction, find_channel);
#endif
}


void find_dialog_stop_label(bool show_stop)
{
  if (show_stop)
    set_stock_button_label(find_cancel_button, I_STOP);
  else set_stock_button_label(find_cancel_button, I_GO_AWAY); 
}


void find_dialog_set_label(const char *str) 
{
  if (edit_find_label) 
    set_label(edit_find_label, str);
}


static void edit_find_next(GtkWidget *w, gpointer context) 
{
  edit_find_find(READ_FORWARD, w, context);
}


static void edit_find_previous(GtkWidget *w, gpointer context) 
{
  edit_find_find(READ_BACKWARD, w, context);
}


static void make_edit_find_dialog(bool managed, chan_info *cp)
{
  if (!edit_find_dialog)
    {
      GtkWidget *dl, *rc;
      GtkWidget *help_button;
      edit_find_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(edit_find_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      SG_SIGNAL_CONNECT(edit_find_dialog, "delete_event", edit_find_delete, NULL);
      gtk_window_set_title(GTK_WINDOW(edit_find_dialog), I_FIND);
      sg_make_resizable(edit_find_dialog);
      sg_container_set_border_width (GTK_CONTAINER(edit_find_dialog), 10);
      gtk_window_resize(GTK_WINDOW(edit_find_dialog), 350, 120);
      gtk_widget_realize(edit_find_dialog);

      find_next_button = gtk_dialog_add_button(GTK_DIALOG(edit_find_dialog), "Forward", GTK_RESPONSE_NONE);
      find_previous_button = gtk_dialog_add_button(GTK_DIALOG(edit_find_dialog), "Back", GTK_RESPONSE_NONE);
      find_cancel_button = gtk_dialog_add_button(GTK_DIALOG(edit_find_dialog), "Go Away", GTK_RESPONSE_NONE);
      help_button = gtk_dialog_add_button(GTK_DIALOG(edit_find_dialog), "Help", GTK_RESPONSE_NONE);

      gtk_widget_set_name(help_button, "dialog_button");
      gtk_widget_set_name(find_cancel_button, "dialog_button");
      gtk_widget_set_name(find_previous_button, "dialog_button");
      gtk_widget_set_name(find_next_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(find_cancel_button);
      add_highlight_button_style(help_button);
      add_highlight_button_style(find_previous_button);
      add_highlight_button_style(find_next_button);
#endif

      SG_SIGNAL_CONNECT(find_cancel_button, "clicked", edit_find_dismiss, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", edit_find_help, NULL);
      SG_SIGNAL_CONNECT(find_next_button, "clicked", edit_find_next, NULL);
      SG_SIGNAL_CONNECT(find_previous_button, "clicked", edit_find_previous, NULL);

      gtk_widget_show(find_cancel_button);
      gtk_widget_show(find_next_button);
      gtk_widget_show(find_previous_button);
      gtk_widget_show(help_button);
      

      rc = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), rc, true, true, 4);
      gtk_widget_show(rc);

      dl = gtk_label_new(I_find);
      sg_box_pack_start(GTK_BOX(rc), dl, false, false, 4);
      gtk_widget_show(dl);

      edit_find_text = snd_entry_new(rc, NULL, WITH_WHITE_BACKGROUND);
      SG_SIGNAL_CONNECT(edit_find_text, "activate", edit_find_next, NULL);
      
      edit_find_label = gtk_label_new("");
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), edit_find_label, false, false, 4);
      gtk_widget_show(edit_find_label);


      find_error_frame = gtk_frame_new(NULL);
      sg_box_pack_end(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), find_error_frame, false, false, 4);

      find_error_label = gtk_label_new("");
      gtk_container_add(GTK_CONTAINER(find_error_frame), find_error_label);
      gtk_widget_show(find_error_label);


      if (managed) gtk_widget_show(edit_find_dialog);
      set_dialog_widget(FIND_DIALOG, edit_find_dialog);
    }
  else 
    {
      if (managed) raise_dialog(edit_find_dialog);
    }
}


void edit_find_callback(GtkWidget *w, gpointer context)
{
  make_edit_find_dialog(true, NULL);
}


void find_dialog(chan_info *cp)
{
  /* used in snd-kbd.c */
  make_edit_find_dialog(true, cp);
}


bool find_dialog_is_active(void)
{
  return((edit_find_dialog) && (widget_is_active(edit_find_dialog)));
}


void save_find_dialog_state(FILE *fd)
{
  if (find_dialog_is_active())
    {
      const char *text;
      /* text = sg_get_text(edit_find_text, 0, -1); -- this is for text_view widgets, but edit_find_text is a gtk_entry */
      text = gtk_entry_get_text(GTK_ENTRY(edit_find_text));
#if HAVE_SCHEME
      if (text)
	fprintf(fd, "(%s #t \"%s\")\n", S_find_dialog, text);
      else
	{
	  if (ss->search_expr)
	    fprintf(fd, "(%s #t \"%s\")\n", S_find_dialog, ss->search_expr);
	  else fprintf(fd, "(%s #t)\n", S_find_dialog);
	}
#endif
#if HAVE_RUBY
      if (text)
	fprintf(fd, "%s(true, \"%s\")\n", to_proc_name(S_find_dialog), text);
      else
	{
	  if (ss->search_expr)
	    fprintf(fd, "%s(true, \"%s\")\n", to_proc_name(S_find_dialog), ss->search_expr);
	  else fprintf(fd, "%s(true)\n", to_proc_name(S_find_dialog));
	}
#endif
#if HAVE_FORTH
      if (text)
	fprintf(fd, "#t \"%s\" %s drop\n", text, S_find_dialog);
      else
	{
	  if (ss->search_expr)
	    fprintf(fd, "#t \"%s\" %s drop\n", ss->search_expr, S_find_dialog);
	  else fprintf(fd, "#t %s drop\n", S_find_dialog);
	}
#endif
      /* if (text) g_free(text); */
    }
}


static Xen g_find_dialog(Xen managed, Xen text)
{
  #define H_find_dialog "(" S_find_dialog " :optional managed text): create and activate the Edit:Find dialog, return the dialog widget"

  Xen_check_type(Xen_is_boolean_or_unbound(managed), managed, 1, S_find_dialog, "a boolean");
  Xen_check_type(Xen_is_string_or_unbound(text), text, 2, S_find_dialog, "a string");

  make_edit_find_dialog(Xen_boolean_to_C_bool(managed), NULL);
  if ((edit_find_text) && (Xen_is_string(text)))
    gtk_entry_get_text(GTK_ENTRY(edit_find_text));

  return(Xen_wrap_widget(edit_find_dialog));
}


static Xen g_find_dialog_widgets(void)
{
  if (edit_find_dialog)
    return(Xen_cons(Xen_wrap_widget(edit_find_dialog),
	     Xen_cons(Xen_wrap_widget(edit_find_text),
  	       Xen_cons(Xen_wrap_widget(find_next_button),
		 Xen_cons(Xen_wrap_widget(find_previous_button),
		   Xen_cons(Xen_wrap_widget(find_cancel_button),
		     Xen_empty_list))))));
  return(Xen_empty_list);
}


Xen_wrap_2_optional_args(g_find_dialog_w, g_find_dialog)
Xen_wrap_no_args(g_find_dialog_widgets_w, g_find_dialog_widgets)

void g_init_gxfind(void)
{
  Xen_define_typed_procedure(S_find_dialog, g_find_dialog_w, 0, 2, 0, H_find_dialog,
			     s7_make_signature(s7, 3, s7_make_symbol(s7, "pair?"), s7_make_symbol(s7, "boolean?"), s7_make_symbol(s7, "string?")));

  Xen_define_typed_procedure("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "internal auto-test function", 
			     s7_make_signature(s7, 1, s7_make_symbol(s7, "list?")));
}

