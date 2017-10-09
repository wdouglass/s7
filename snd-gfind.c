#include "snd.h"

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
      gtk_container_set_border_width (GTK_CONTAINER(edit_find_dialog), 10);
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
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), rc, true, true, 4);
      gtk_widget_show(rc);

      dl = gtk_label_new(I_find);
      gtk_box_pack_start(GTK_BOX(rc), dl, false, false, 4);
      gtk_widget_show(dl);

      edit_find_text = snd_entry_new(rc, NULL, WITH_WHITE_BACKGROUND);
      SG_SIGNAL_CONNECT(edit_find_text, "activate", edit_find_next, NULL);
      
      edit_find_label = gtk_label_new("");
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), edit_find_label, false, false, 4);
      gtk_widget_show(edit_find_label);


      find_error_frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), find_error_frame, false, false, 4);

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
      char *text = NULL;
      text = sg_get_text(edit_find_text, 0, -1);
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
      if (text) g_free(text);
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

