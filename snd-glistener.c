#include "snd.h"
#include "snd-menu.h"

static GtkWidget *listener_text = NULL;
static GtkTextBuffer *listener_buffer = NULL;

void listener_append(const char *msg)
{
  if (ss->listener)
    glistener_append_text(ss->listener, msg);
  if (listener_text)
    ss->graph_is_active = false;
}


void listener_append_and_prompt(const char *msg)
{
  if (msg)
    {
      glistener_append_text(ss->listener, msg);
      glistener_append_prompt(ss->listener);
    }
  else 
    {
      if (ss->listener)
	glistener_append_text(ss->listener, "\n");
    }
}


static Xen listener_click_hook; 
static Xen mouse_enter_listener_hook;
static Xen mouse_leave_listener_hook;

static gboolean listener_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (with_pointer_focus(ss))
    goto_window(listener_text);

  if (Xen_hook_has_list(mouse_enter_listener_hook))
    run_hook(mouse_enter_listener_hook,
	     Xen_list_1(Xen_wrap_widget(listener_text)),
	     S_mouse_enter_listener_hook);
  cursor_set_blinks(w, true);
  return(false);
}


static gboolean listener_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (Xen_hook_has_list(mouse_leave_listener_hook))
    run_hook(mouse_leave_listener_hook,
	     Xen_list_1(Xen_wrap_widget(listener_text)),
	     S_mouse_leave_listener_hook);
  cursor_set_blinks(w, false);
  return(false);
}


static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->graph_is_active = false;
  return(false);
}


static gboolean listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  guint key;
  GdkModifierType state;

  /* clear possible warning */
  /* glistener_clear_status(ss->listener); */

  key = EVENT_KEYVAL(event);
#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_state((GdkEvent *)event, &state);
#else
  state = event->state;
#endif

  if ((state & snd_ControlMask) &&
      ((key == snd_K_g) || (key == snd_K_G)))
    ss->C_g_typed = true; 

  if (ss->graph_is_active) 
    {
      chan_info *cp;
      cp = current_channel();
      if (!cp) 
	ss->graph_is_active = false;
      else
	{
	  graph_key_press(channel_graph(cp), event, (gpointer)cp); 
	  return(true); /* don't repeat the keystroke?? */
	}
    }
  return(false);
}


#if HAVE_SCHEME
static s7_pointer g_listener_load_hook(s7_scheme *sc, s7_pointer args)
{
  /* arg is the hook, (hook 'name) is the file */
  s7_pointer hook, file;
  char msg[128];
  if (!(ss->listener)) return(args);
  hook = s7_car(args);
  file = s7_let_ref(s7, hook, s7_make_symbol(s7, "name"));
  if (!s7_is_string(file))
    s7_wrong_type_arg_error(sc, "glistener *load-hook*", 1, file, "a hook with a 'name field, the file being loaded");
  snprintf(msg, 128, "loading %s", s7_string(file));
  glistener_post_status(ss->listener, msg);
  return(args);
}
#endif


static void listener_init(glistener *g, GtkWidget *w)
{
  listener_text = w;
  listener_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));

  set_listener_text_font();
  add_listener_style(w);
  SG_SIGNAL_CONNECT(w, "key_press_event", listener_key_press, NULL); /* Snd's should run first */
  SG_SIGNAL_CONNECT(w, "button_press_event", listener_button_press, NULL);
  SG_SIGNAL_CONNECT(w, "enter_notify_event", listener_focus_callback, NULL);
  SG_SIGNAL_CONNECT(w, "leave_notify_event", listener_unfocus_callback, NULL);
  
  glistener_set_prompt_tag(ss->listener, gtk_text_buffer_create_tag(listener_buffer, "glistener_prompt_tag", 
								    "weight", PANGO_WEIGHT_BOLD, 
								    /* "foreground", "red", */
								    NULL));
  ss->listener_pane = w;
}


#if HAVE_SCHEME
static const char *helper(glistener *g, const char *text)
{
  s7_pointer sym;
  if (!text) return(NULL);

  sym = s7_symbol_table_find_name(s7, text);
  if (sym)
    return(s7_help(s7, sym));

  glistener_clear_status(g);
  return(NULL);
}


#if HAVE_CHECKER
static const char *checker(glistener *g, const char *text)
{
  s7_int gc_loc, err_loc;
  s7_pointer port, err_port, result;
  const char *errmsg;
  bool err = false;
  result = s7_f(s7);

  if (s7_begin_hook(s7)) return(NULL); 
  err_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
  err_loc = s7_gc_protect(s7, err_port);
  port = s7_open_input_string(s7, text);
  gc_loc = s7_gc_protect(s7, port);
  result = s7_read(s7, port);
  errmsg = s7_get_output_string(s7, err_port);
  if (errmsg) {err = true; glistener_post_status(g, errmsg);}
  s7_close_input_port(s7, port);
  s7_gc_unprotect_at(s7, gc_loc);
  s7_close_output_port(s7, s7_current_error_port(s7));
  s7_set_current_error_port(s7, err_port);
  s7_gc_unprotect_at(s7, err_loc);

  if ((!err) &&
      (s7_is_pair(result)))
    {
      s7_pointer sym;
      sym = s7_car(result);
      if (s7_is_symbol(sym))
	{
	  s7_pointer val;
	  val = s7_symbol_value(s7, sym);
	  if ((val != s7_undefined(s7)) &&
	      (!s7_is_aritable(s7, val, s7_list_length(s7, result) - 1)))
	    {
	      glistener_post_status(g, "wrong number of args");
	    }
	}
    }
  return(NULL);
}
#endif


static GtkTextTag *string_tag = NULL, *comment_tag = NULL, *comment3_tag = NULL, *block_comment_tag = NULL;
static GtkTextTag *syntax_tag = NULL, *macro_tag = NULL, *procedure_tag = NULL, *constant_tag = NULL;
static GtkTextTag *undefined_tag = NULL;
/* also bracket and character
 */

static s7_pointer g_colorizer_colors(s7_scheme *sc, s7_pointer args)
{
  #define H_colorizer_colors "(colorizer-colors comment comment3 block-comment string constant syntax macro procedure undefined)"
  /* set the tag colors from scheme -- tags are owned by the buffer, so I assume we don't free the old ones
   *
   * default: (colorizer-colors "red" "brown" "orangered" "gray40" "gray20" "blue" "seagreen" "steelblue4" "black")
   */
  int i;
  s7_pointer p;

  for (p = args, i = 1; s7_is_pair(p); p = s7_cdr(p), i++)
    {
      if (!s7_is_string(s7_car(p)))
	s7_wrong_type_arg_error(sc, "colorizer-colors", i, s7_car(p), "a color name (a string)");
    }
  p = args;

  if (s7_is_pair(p)) {comment_tag =       gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {comment3_tag =      gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {block_comment_tag = gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {string_tag =        gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {constant_tag =      gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {syntax_tag =        gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {macro_tag =         gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {procedure_tag =     gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); p = s7_cdr(p);}
  if (s7_is_pair(p)) {undefined_tag =     gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", s7_string(s7_car(p)), NULL); /* p = s7_cdr(p); */}

  return(s7_t(sc));
}


static void colorizer(glistener *g, glistener_colorizer_t type, int start, int end)
{
  GtkTextIter s1, s2;
  GtkTextTag *tag;

  if (!comment_tag)
    {
      comment_tag =       gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "red", NULL);
      comment3_tag =      gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "brown", NULL);
      block_comment_tag = gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "orangered", NULL);
      string_tag =        gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "gray40", NULL);
      constant_tag =      gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "gray20", NULL);
      syntax_tag =        gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "blue", NULL);
      macro_tag =         gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "seagreen", NULL);
      procedure_tag =     gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "steelblue4", NULL);
      undefined_tag =     gtk_text_buffer_create_tag(listener_buffer, NULL, "foreground", "black", NULL);
    }
  tag = NULL;

  switch (type)
    {
    case GLISTENER_COMMENT:        
      tag = comment_tag; 
      if (comment3_tag)
	{
	  /* presumably ;;; ... is handled differently in this case */
	  GtkTextIter iter;
	  gtk_text_buffer_get_iter_at_offset(listener_buffer, &iter, start);
	  /* is start the start of a line, and are the first 3 chars semicolons */
	  if ((gtk_text_iter_starts_line(&iter)) &&
	      ((end - start) > 2))
	    {
	      char *text;
	      text = glistener_text(ss->listener, start, start + 3);
	      if (text)
		{
		  if (strcmp(text, ";;;") == 0)
		    tag = comment3_tag;
		  free(text);
		}
	    }
	}
      break;

    case GLISTENER_BLOCK_COMMENT:  
      tag = block_comment_tag; 
      break;

    case GLISTENER_STRING:         
      tag = string_tag;
      break;

    case GLISTENER_ATOM: 
      {
	char *text;
	tag = NULL;

	text = glistener_text(ss->listener, start, end);
	if (text[0] == '#')
	  {
	    tag = constant_tag;
	  }
	else
	  {
	    s7_pointer sym;
	    sym = s7_symbol_table_find_name(s7, text);
	    
	    if (sym)
	      {
		if (s7_is_syntax(s7_symbol_value(s7, sym)))
		  tag = syntax_tag;
		else
		  {
		    if (s7_is_procedure(s7_symbol_value(s7, sym)))
		      tag = procedure_tag;
		    else
		      {
			if (s7_is_macro(s7, sym))
			  tag = macro_tag;
			else
			  {
			    if (!s7_is_defined(s7, text))
			      {
				tag = undefined_tag;
			      }
			  }
		      }
		  }
	      }
	    else
	      {
	      }
	  }
	if (text) free(text);
      }
      break;

    case GLISTENER_LIST:
    case GLISTENER_BRACKET:
    case GLISTENER_CHARACTER:
      break;
    }
  if (!tag) return;

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, start);
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &s2, end);
  gtk_text_buffer_apply_tag(listener_buffer, tag, &s1, &s2);
}


static void completer(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data)
{
  s7_for_each_symbol_name(s7, symbol_func, data);
}


static s7_pointer top_level_let = NULL;
static s7_pointer g_top_level_let(s7_scheme *sc, s7_pointer args)
{
  return(top_level_let);
}

static s7_pointer g_set_top_level_let(s7_scheme *sc, s7_pointer args)
{
  top_level_let = s7_car(args);
  return(top_level_let);
}


static void evaluator(glistener *g, const char *text)
{
  s7_int gc_loc;
  s7_pointer old_port, result;
  char *errmsg = NULL;
  
  if (s7_begin_hook(s7)) return;      /* s7 is already running (user typed <cr> during computation) */
  
  old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
  gc_loc = s7_gc_protect(s7, old_port);
  
  if (with_interrupts(ss))
    s7_set_begin_hook(s7, listener_begin_hook);
  
  result = s7_eval_c_string_with_environment(s7, text, top_level_let);
  
  s7_set_begin_hook(s7, NULL);
  errmsg = mus_strdup(s7_get_output_string(s7, s7_current_error_port(s7)));
  
  s7_close_output_port(s7, s7_current_error_port(s7));
  s7_set_current_error_port(s7, old_port);
  s7_gc_unprotect_at(s7, gc_loc);
  
  if (errmsg)
    {
      if (*errmsg)
	snd_display_result(errmsg, NULL);
      free(errmsg);
    }
  else snd_report_listener_result(result);
}


static s7_pointer wrap_glistener(glistener *g)
{
  return(s7_make_c_pointer(s7, (void *)g));
}

static glistener *unwrap_glistener(s7_scheme *sc, const char *caller, s7_pointer p)
{
  glistener *g;
  if (!s7_is_c_pointer(p))
    s7_wrong_type_arg_error(sc, caller, 1, p, "a c-pointer");
  g = (glistener *)s7_c_pointer(p);
  if (!g)
    s7_wrong_type_arg_error(sc, caller, 1, p, "a non-null c-pointer");
  return(g);
}

static s7_pointer g_evaluate(s7_scheme *sc, s7_pointer args)
{
  char *str;
  s7_pointer result;
  str = glistener_evaluate(unwrap_glistener(sc, "listener eval", s7_car(args)));
  result = s7_make_string(s7, str);
  if (str) g_free(str);
  return(result);
}

static s7_pointer g_complete(s7_scheme *sc, s7_pointer args)
{
  char *str;
  s7_pointer result;
  str = glistener_complete(unwrap_glistener(sc, "listener completion", s7_car(args)));
  result = s7_make_string(s7, str);
  if (str) g_free(str);
  return(result);
}

static s7_pointer g_append_text(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_string(s7_cadr(args)))
    glistener_append_text(unwrap_glistener(sc, "listener append_text", s7_car(args)), s7_string(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_insert_text(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_string(s7_cadr(args)))
    glistener_insert_text(unwrap_glistener(sc, "listener insert_text", s7_car(args)), s7_string(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_scroll_to_end(s7_scheme *sc, s7_pointer args)
{
  glistener_scroll_to_end(unwrap_glistener(sc, "listener scroll_to_end", s7_car(args)));
  return(s7_car(args));
}

static s7_pointer g_append_prompt(s7_scheme *sc, s7_pointer args)
{
  glistener_append_prompt(unwrap_glistener(sc, "listener append prompt", s7_car(args)));
  return(s7_car(args));
}

static s7_pointer g_prompt_position(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(s7, glistener_prompt_position(unwrap_glistener(sc, "listener prompt position", s7_car(args)))));
}

static s7_pointer g_cursor_position(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(s7, glistener_cursor_position(unwrap_glistener(sc, "listener cursor position", s7_car(args)))));
}

static s7_pointer g_set_cursor_position(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_integer(s7_cadr(args)))
    glistener_set_cursor_position(unwrap_glistener(sc, "set listener cursor-position", s7_car(args)), s7_integer(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_text(s7_scheme *sc, s7_pointer args)
{
  if ((s7_is_integer(s7_cadr(args))) &&
      (s7_is_integer(s7_caddr(args))))
    {
      char *str;
      s7_pointer result;
      str = glistener_text(unwrap_glistener(sc, "listener text", s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)));
      result = s7_make_string(s7, str);
      if (str) g_free(str);
      return(result);
    }
  return(s7_f(sc));
}

static s7_pointer g_clear(s7_scheme *sc, s7_pointer args)
{
  glistener_clear(unwrap_glistener(sc, "listener clear", s7_car(args)));
  return(s7_car(args));
}

static s7_pointer g_text_widget(s7_scheme *sc, s7_pointer args)
{
  return(s7_list(sc, 2, s7_make_symbol(sc, "GtkTextView*"), s7_make_c_pointer(sc, (void *)listener_text)));
}

static s7_pointer g_set_prompt(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_string(s7_cadr(args)))
    glistener_set_prompt(unwrap_glistener(sc, "set listener prompt", s7_car(args)), s7_string(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_set_prompt_tag(s7_scheme *sc, s7_pointer args)
{
  /* args: (#<c_pointer 0x2dfbed0> (GtkTextTag_ #<c_pointer 0x2e8f180>))
   */
  GtkTextTag *new_tag = NULL;
  if ((s7_is_pair(s7_cadr(args))) &&
      (s7_is_pair(s7_cdadr(args))))
    new_tag = (GtkTextTag *)(s7_c_pointer(s7_cadadr(args)));

  glistener_set_prompt_tag(unwrap_glistener(sc, "set listener prompt tag", s7_car(args)), new_tag);
  return(s7_cadr(args));
}

/*  (listener-set-prompt-tag *listener* 
      (gtk_text_buffer_create_tag 
        (GTK_TEXT_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (listener-text-widget *listener*))))
	#f (list "weight" PANGO_WEIGHT_BOLD "foreground" "red")))
*/


static void glistener_init(glistener *g1)
{
  s7_pointer cp, t, s, p, i, b;
  cp = s7_make_symbol(s7, "c-pointer?");
  s = s7_make_symbol(s7, "string?");
  p = s7_make_symbol(s7, "pair?");
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  t = s7_t(s7);

  s7_define_typed_function(s7, "listener-append-text",     g_append_text,     2, 0, false, "(listener-append-text g txt)",    s7_make_signature(s7, 3, s, cp, s));
  s7_define_typed_function(s7, "listener-insert-text",     g_insert_text,     2, 0, false, "(listener-insert-text g txt)",    s7_make_signature(s7, 3, s, cp, s));
  s7_define_typed_function(s7, "listener-cursor-position", g_cursor_position, 1, 0, false, "(listener-cursor-position g)",    s7_make_signature(s7, 2, i, cp));
  s7_define_typed_function(s7, "listener-set-cursor-position", g_set_cursor_position, 2, 0, false, "(listener-set-cursor-position g pos)",
			   s7_make_signature(s7, 3, i, cp, i));
  s7_define_typed_function(s7, "listener-text",            g_text,            3, 0, false, "(listener-text g start end)",
			   s7_make_signature(s7, 4, s7_make_signature(s7, 2, b, s), cp, i, i));
  s7_define_typed_function(s7, "listener-append-prompt",   g_append_prompt,   1, 0, false, "(listener-append-prompt g)",      s7_make_signature(s7, 2, cp, cp));
  s7_define_typed_function(s7, "listener-prompt-position", g_prompt_position, 1, 0, false, "(listener-prompt-position g)",    s7_make_signature(s7, 2, i, cp));
  s7_define_typed_function(s7, "listener-set-prompt",      g_set_prompt,      2, 0, false, "(listener-set-prompt g str)",     s7_make_signature(s7, 3, s, cp, s));
  s7_define_typed_function(s7, "listener-set-prompt-tag",  g_set_prompt_tag,  2, 0, false, "(listener-set-prompt-tag g tag)", s7_make_signature(s7, 3, t, cp, t));
  s7_define_typed_function(s7, "listener-evaluate",        g_evaluate,        1, 0, false, "(listener-evaluate g)",           s7_make_signature(s7, 2, s, cp));
  s7_define_typed_function(s7, "listener-complete",        g_complete,        1, 0, false, "(listener-complete g)",           s7_make_signature(s7, 2, s, cp));
  s7_define_typed_function(s7, "listener-scroll",          g_scroll_to_end,   1, 0, false, "(listener-scroll g)",             s7_make_signature(s7, 2, cp, cp));
  s7_define_typed_function(s7, "listener-clear",           g_clear,           1, 0, false, "(listener-clear g)",              s7_make_signature(s7, 2, cp, cp));
  s7_define_typed_function(s7, "listener-text-widget",     g_text_widget,     1, 0, false, "(listener-text-widget g)",        s7_make_signature(s7, 2, p, cp));
  s7_define_variable(s7, "*listener*", wrap_glistener(g1));
}
#endif


static bool listener_colorizing = false;
bool listener_colorized(void) {return(listener_colorizing);}
bool listener_set_colorized(bool val) 
{
#if HAVE_SCHEME
  listener_colorizing = val;
  s7_symbol_set_value(s7, ss->listener_colorized_symbol, s7_make_boolean(s7, val));
  if (ss->listener)
    {
      if (val)
	glistener_set_colorizer(ss->listener, colorizer);
      else glistener_set_colorizer(ss->listener, NULL);
    }
#endif
  return(val);
}


#if HAVE_FORTH || HAVE_RUBY
static void evaluator(glistener *g, const char *text)
{
  call_read_hook_or_eval(text); /* snd-listener.c */
}
#endif

#if (GTK_CHECK_VERSION(3, 92, 1))
static bool keyer(glistener *g, GtkWidget *w, GdkEvent *e)
#else
static bool keyer(glistener *g, GtkWidget *w, GdkEventKey *e)
#endif
{
  /* add C-g handling */
  guint key;
  GdkModifierType state;

  key = EVENT_KEYVAL(e);
#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_state((GdkEvent *)e, &state);
#else
  state = e->state;
#endif
  if (((key == snd_K_g) || (key == snd_K_G)) &&
      (state & snd_ControlMask))
    {
      ss->C_g_typed = true;
      control_g(any_selected_sound());
    }

  return(false);
}


static void make_listener_widget(int height)
{
#if HAVE_EXTENSION_LANGUAGE
  if (!listener_text)
    {
      GtkWidget *frame;

      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);

      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_pack2(GTK_PANED(sound_pane(ss)), frame, false, true); /* add2 but resize=false */
      else gtk_container_add(GTK_CONTAINER(main_pane(ss)), frame);

      ss->listener = glistener_new(frame, listener_init);
      glistener_set_evaluator(ss->listener, evaluator);
      glistener_set_keyer(ss->listener, keyer);

#if HAVE_SCHEME
      glistener_set_helper(ss->listener, helper);
#if HAVE_CHECKER
      glistener_set_checker(ss->listener, checker);
#endif
      glistener_set_completer(ss->listener, completer);
      if (listener_colorizing)
	glistener_set_colorizer(ss->listener, colorizer);
      glistener_init(ss->listener);
#endif
#if HAVE_FORTH || HAVE_RUBY
      glistener_is_schemish(ss->listener, false);
#endif
    }
#endif
}


void goto_listener(void) 
{
  goto_window(listener_text);
}


void color_listener(color_info *pix)
{
  ss->listener_color = pix;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->listener_color_symbol, Xen_wrap_pixel(pix));
#endif
#if (!GTK_CHECK_VERSION(3, 0, 0))
  glistener_set_background_color(ss->listener, rgb_to_gdk_color(ss->listener_color));
#else
  glistener_set_background_color(ss->listener, (GdkRGBA *)(ss->listener_color));
#endif
}


void color_listener_text(color_info *pix)
{
  ss->listener_text_color = pix;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->listener_text_color_symbol, Xen_wrap_pixel(pix));
#endif
#if (!GTK_CHECK_VERSION(3, 0, 0))
  glistener_set_text_color(ss->listener, rgb_to_gdk_color(ss->listener_text_color));
#else
  glistener_set_text_color(ss->listener, (GdkRGBA *)(ss->listener_text_color));
#endif
}


void handle_listener(bool open)
{
  if (!open) return;
  if (!listener_text)
    {
      make_listener_widget(100);

      color_listener(ss->listener_color);
      color_listener_text(ss->listener_text_color);

      gtk_widget_hide(view_listener_menu);
    }

  if ((sound_pane(ss)) && /* might be run -separate with no sound open */
      (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS))
    {
      int hgt;
      hgt = widget_height(sound_pane(ss));
      if (hgt > 100) /* we can get here before the sound window has opened, but with one pending.
		      *   the position is in terms of current size, which is useless in this case.
		      */
	gtk_paned_set_position(GTK_PANED(sound_pane(ss)), (gint)(hgt * .75));
    }
}


bool listener_exists(void)
{
  return((bool)listener_text);
}


int listener_height(void) 
{
  if (!listener_text) 
    return(0);
  return(100);
}


int listener_width(void) 
{
  if (!listener_text)
    return(0);
  return(500);
}


static Xen g_listener_selection(void)
{
  #define H_listener_selection "(" S_listener_selection "): currently selected text in listener or " PROC_FALSE
  Xen res = Xen_false;
  if (listener_text)
    {
      GtkTextIter start, end;
      if (gtk_text_buffer_get_selection_bounds(listener_buffer, &start, &end))
	{
	  char *txt;
	  txt = gtk_text_buffer_get_text(listener_buffer, &start, &end, true);
	  if (txt) 
	    {
	      res = C_string_to_Xen_string(txt);
	      g_free(txt);
	    }
	}
    }
  return(res);
}


void set_listener_text_font(void)
{
  /* (set! (listener-font) "Monospace 12") */
  glistener_set_font(ss->listener, LISTENER_FONT(ss));
}


static Xen g_reset_listener_cursor(void)
{
  #define H_reset_listener_cursor "(" S_reset_listener_cursor "): reset listener cursor to the default pointer"
  if (listener_text)
    glistener_set_cursor_shape(ss->listener, ss->arrow_cursor);
  return(Xen_false);
}


void clear_listener(void)
{
  if (ss->listener)
    glistener_clear(ss->listener);
}


void append_listener_text(int end, const char *msg)
{
  /* "end" arg needed in Motif */
  if (ss->listener)
    glistener_append_text(ss->listener, msg);
}


int save_listener_text(FILE *fp)
{
  if (ss->listener)
    {
      if ((!listener_text) ||
	  (glistener_write(ss->listener, fp)))
	return(0);
    }
  return(-1);
}


static Xen g_goto_listener_end(void)
{
  #define H_goto_listener_end "(" S_goto_listener_end "): move cursor and scroll to bottom of listener pane"
  if (ss->listener)
    glistener_scroll_to_end(ss->listener);
  return(Xen_false);
}



Xen_wrap_no_args(g_listener_selection_w, g_listener_selection)
Xen_wrap_no_args(g_reset_listener_cursor_w, g_reset_listener_cursor)
Xen_wrap_no_args(g_goto_listener_end_w, g_goto_listener_end)

void g_init_gxlistener(void)
{
#if HAVE_SCHEME
  s7_pointer s, b;
  s = s7_make_symbol(s7, "string?");
  b = s7_make_symbol(s7, "boolean?");

  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (widget): called when the mouse \
enters the lisp listener pane:\n\
  (hook-push " S_mouse_enter_listener_hook "\n\
    (lambda (hook)\n\
      (" S_focus_widget " (hook 'widget))))"
#endif
#if HAVE_RUBY
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
  $mouse_enter_listener_hook.add-hook!(\"enter\") do |widget|\n\
    focus_widget(widget)\n\
  end"
#endif
#if HAVE_FORTH
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
" S_mouse_enter_listener_hook " lambda: <{ wid }> wid " S_focus_widget " ; add-hook!"
#endif

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (widget): called when the mouse \
leaves the lisp listener pane"

  mouse_enter_listener_hook = Xen_define_hook(S_mouse_enter_listener_hook, "(make-hook 'widget)", 1, H_mouse_enter_listener_hook);
  mouse_leave_listener_hook = Xen_define_hook(S_mouse_leave_listener_hook, "(make-hook 'widget)", 1, H_mouse_leave_listener_hook);

  Xen_define_typed_procedure(S_listener_selection, g_listener_selection_w,       0, 0, 0, H_listener_selection,    s7_make_signature(s7, 1, s7_make_signature(s7, 2, s, b)));
  Xen_define_typed_procedure(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor, s7_make_signature(s7, 1, b));
  Xen_define_typed_procedure(S_goto_listener_end, g_goto_listener_end_w,         0, 0, 0, H_goto_listener_end,     s7_make_signature(s7, 1, b));

  #define H_listener_click_hook S_listener_click_hook " (position): called when listener clicked; position is text pos of click in listener"
  listener_click_hook = Xen_define_hook(S_listener_click_hook, "(make-hook 'position)", 1,   H_listener_click_hook); 

#if HAVE_SCHEME
  top_level_let = s7_nil(s7);
  s7_define_variable(s7, "top-level-let", s7_dilambda(s7, "top-level-let", g_top_level_let, 0, 0, g_set_top_level_let, 1, 0, "listener environment"));

  s7_define_typed_function(s7, "colorizer-colors", g_colorizer_colors, 0, 0, true, H_colorizer_colors, s7_make_circular_signature(s7, 1, 2, b, s));

  s7_hook_set_functions(s7, s7_name_to_value(s7, "*load-hook*"),
    s7_cons(s7, 
      s7_make_function(s7, "listener-load-hook", g_listener_load_hook, 1, 0, false, "listener *load-hook* function"), 
      s7_hook_functions(s7, s7_name_to_value(s7, "*load-hook*"))));
#endif
}


/* to get rid of 
 *    Fontconfig error: Cannot load default config file
 * and the consequent ridiculous fonts, since I think I built fontconfig from scratch,
 * copy (as root) /etc/fonts/fonts.conf to /usr/local/etc/fonts/fonts.conf
 *
 * to disable the goddamn beep put
 *   gtk-error-bell = 0
 * in /etc/gtk-2.0/gtkrc
 *
 * to build fontconfig, use --disable-docs (there's no way to make the docbook chain happy)
 * atk-bridge-2.0 needed, glib needs automake 1.13.1, at-spi2-atk needs at-spi2-code which
 * is incompatible with glib 2.37.0, my FC18 machine is dead, so I'm stuck.  Wait for FC19...
 *
 * to implement stuff like find, the prompt might be in the status area?
 */
