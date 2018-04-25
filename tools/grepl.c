/* example main program that calls glistener/s7
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include <gtk/gtk.h>

#include "s7.h"
#include "glistener.h"

static s7_scheme *s7;

static s7_pointer wrap_glistener(glistener *g)
{
  return(s7_make_c_pointer_with_type(s7, (void *)g, s7_make_symbol(s7, "glistener*"), s7_f(s7)));
}

static glistener *unwrap_glistener(s7_pointer p)
{
  return((glistener *)s7_c_pointer(p));
}


static s7_pointer g_glistener_text_widget(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer_with_type(sc, glistener_text_widget(unwrap_glistener(s7_car(args))), s7_make_symbol(sc, "GtkWidget*"), s7_f(sc)));
}

static s7_pointer g_glistener_text_buffer(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer_with_type(sc, glistener_text_buffer(unwrap_glistener(s7_car(args))), s7_make_symbol(sc, "GtkTextBuffer*"), s7_f(sc)));
}

static s7_pointer g_glistener_status_widget(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer_with_type(sc, glistener_status_widget(unwrap_glistener(s7_car(args))), s7_make_symbol(sc, "GtkWidget*"), s7_f(sc)));
}


static s7_pointer g_evaluate(s7_scheme *sc, s7_pointer args)
{
  char *str;
  s7_pointer result;
  str = glistener_evaluate(unwrap_glistener(s7_car(args)));
  result = s7_make_string(s7, str);
  if (str) g_free(str);
  return(result);
}

static s7_pointer g_complete(s7_scheme *sc, s7_pointer args)
{
  char *str;
  s7_pointer result;
  str = glistener_complete(unwrap_glistener(s7_car(args)));
  result = s7_make_string(s7, str);
  if (str) g_free(str);
  return(result);
}

static s7_pointer g_append_text(s7_scheme *sc, s7_pointer args)
{
  glistener_append_text(unwrap_glistener(s7_car(args)), s7_string(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_insert_text(s7_scheme *sc, s7_pointer args)
{
  glistener_insert_text(unwrap_glistener(s7_car(args)), s7_string(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_scroll_to_end(s7_scheme *sc, s7_pointer args)
{
  glistener_scroll_to_end(unwrap_glistener(s7_car(args)));
  return(s7_car(args));
}

static s7_pointer g_append_prompt(s7_scheme *sc, s7_pointer args)
{
  glistener_append_prompt(unwrap_glistener(s7_car(args)));
  return(s7_car(args));
}

static s7_pointer g_prompt_position(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(s7, glistener_prompt_position(unwrap_glistener(s7_car(args)))));
}

static s7_pointer g_cursor_position(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(s7, glistener_cursor_position(unwrap_glistener(s7_car(args)))));
}

static s7_pointer g_set_cursor_position(s7_scheme *sc, s7_pointer args)
{
  glistener_set_cursor_position(unwrap_glistener(s7_car(args)), s7_integer(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_text(s7_scheme *sc, s7_pointer args)
{
  char *str;
  s7_pointer result;
  str = glistener_text(unwrap_glistener(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)));
  result = s7_make_string(s7, str);
  if (str) g_free(str);
  return(result);
}

static s7_pointer g_clear(s7_scheme *sc, s7_pointer args)
{
  glistener_clear(unwrap_glistener(s7_car(args)));
  return(s7_car(args));
}

static s7_pointer g_set_prompt(s7_scheme *sc, s7_pointer args)
{
  glistener_set_prompt(unwrap_glistener(s7_car(args)), s7_string(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_glistener_set_font(s7_scheme *sc, s7_pointer args)
{
  glistener_set_font(unwrap_glistener(s7_car(args)), pango_font_description_from_string(s7_string(s7_cadr(args))));
  return(s7_cadr(args));
}

static void glistener_init(glistener *g1)
{
  s7_define_function(s7, "glistener-text-widget", g_glistener_text_widget, 1, 0, false, "(glistener-text-widget g)");
  s7_define_function(s7, "glistener-text-buffer", g_glistener_text_buffer, 1, 0, false, "(glistener-text-buffer g)");
  s7_define_function(s7, "glistener-status-widget", g_glistener_status_widget, 1, 0, false, "(glistener-status-widget g)");
  s7_define_function(s7, "glistener-set-font", g_glistener_set_font, 2, 0, false, "(glistener-set-font g font)");
  s7_define_function(s7, "append-text", g_append_text, 2, 0, false, "(append-text g txt)");
  s7_define_function(s7, "insert-text", g_insert_text, 2, 0, false, "(insert-text g txt)");
  s7_define_function(s7, "cursor-position", g_cursor_position, 1, 0, false, "(cursor-position g)");
  s7_define_function(s7, "set-cursor-position", g_set_cursor_position, 2, 0, false, "(set-cursor-position g pos)");
  s7_define_function(s7, "text", g_text, 3, 0, false, "(text g start end)");
  s7_define_function(s7, "append-prompt", g_append_prompt, 1, 0, false, "(append-prompt g)");
  s7_define_function(s7, "prompt-position", g_prompt_position, 1, 0, false, "(prompt-position g)");
  s7_define_function(s7, "set-prompt", g_set_prompt, 2, 0, false, "(set-prompt g str)");
  s7_define_function(s7, "evaluate", g_evaluate, 1, 0, false, "(evaluate g)");
  s7_define_function(s7, "complete", g_complete, 1, 0, false, "(complete g)");
  s7_define_function(s7, "scroll", g_scroll_to_end, 1, 0, false, "(scroll g)");
  s7_define_function(s7, "clear", g_clear, 1, 0, false, "(clear g)");
  s7_define_variable(s7, "*g*", wrap_glistener(g1));
}

static GdkCursor *arrow_cursor;
static glistener *g1;

static gint quit_repl(GtkWidget *w, GdkEvent *event, gpointer context) {exit(0);}

static void evaluator(glistener *g, const char *text)
{
  int gc_loc;
  s7_pointer old_port, result;
  const char *errmsg = NULL;
  char *msg = NULL;
  
  old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
  gc_loc = s7_gc_protect(s7, old_port);
  
  result = s7_eval_c_string(s7, text);
  errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
  if ((errmsg) && (*errmsg))
    {
      msg = (char *)calloc(strlen(errmsg) + 1, sizeof(char));
      strcpy(msg, errmsg);
      glistener_set_cursor_shape(g1, arrow_cursor); /* make sure we undo the wait cursor if an error occurred */
    }
  
  s7_close_output_port(s7, s7_current_error_port(s7));
  s7_set_current_error_port(s7, old_port);
  s7_gc_unprotect_at(s7, gc_loc);
  
  glistener_append_text(g, "\n");
  if (msg)
    glistener_append_text(g, msg);
  else 
    {
      msg = s7_object_to_c_string(s7, result);
      glistener_append_text(g, msg);
    }
  if (msg) free(msg);
  glistener_append_prompt(g);
}

static void listener_init(glistener *g, GtkWidget *w)
{
  unsigned char prompt[4] = {0xce, 0xbb, '>', '\0'}; /* lambda as prompt */
  GtkTextBuffer *buffer;

  buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  glistener_set_font(g, pango_font_description_from_string("Monospace 10"));
  glistener_set_prompt_tag(g, gtk_text_buffer_create_tag(buffer, "glistener_prompt_tag", 
							 "weight", PANGO_WEIGHT_BOLD, 
							 "foreground", "red",
							 NULL));
  glistener_set_prompt(g, prompt);
}

static const char *helper(glistener *g, const char *text)
{
  s7_pointer sym;
  sym = s7_symbol_table_find_name(s7, text);
  if (sym)
    return(s7_help(s7, sym));
  glistener_clear_status(g);
  return(NULL);
}

static void completer(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data)
{
  s7_for_each_symbol_name(s7, symbol_func, data);
}


int main(int argc, char **argv) 
{
  GtkWidget *shell, *frame, *vb, *hb;

  s7 = s7_init();  

#if (GTK_CHECK_VERSION(3, 90, 0))
  gtk_init();
#else
  gtk_init(&argc, &argv);
#endif 

  shell = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  g_signal_connect(G_OBJECT(shell), "delete_event", G_CALLBACK(quit_repl), NULL);

  hb = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
  gtk_container_add(GTK_CONTAINER(shell), hb);
  gtk_widget_show(hb);

  vb = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_container_add(GTK_CONTAINER(hb), vb);
  gtk_widget_show(vb);

  frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
  gtk_widget_show(frame);
  gtk_container_add(GTK_CONTAINER(vb), frame);

  s7_define_variable(s7, "grepl:hbox", s7_make_c_pointer_with_type(s7, hb, s7_make_symbol(s7, "GtkBox*"), s7_f(s7)));
  s7_define_variable(s7, "grepl:vbox", s7_make_c_pointer_with_type(s7, vb, s7_make_symbol(s7, "GtkBox*"), s7_f(s7)));
  s7_define_variable(s7, "grepl:shell", s7_make_c_pointer_with_type(s7, shell, s7_make_symbol(s7, "GtkWindow*"), s7_f(s7)));
  s7_define_variable(s7, "grepl:frame", s7_make_c_pointer_with_type(s7, frame, s7_make_symbol(s7, "GtkFrame*"), s7_f(s7)));

  g1 = glistener_new(frame, listener_init);
  glistener_set_evaluator(g1, evaluator);
  glistener_set_helper(g1, helper);
  glistener_set_completer(g1, completer);

  glistener_init(g1);
  arrow_cursor = GDK_CURSOR_NEW(GDK_LEFT_PTR);

  gtk_widget_show(shell);
  gtk_window_resize(GTK_WINDOW(shell), 600, 400);

  gtk_main();
}

/* to build glistener.o: gcc glistener.c -c `pkg-config --libs gtk+-3.0 --cflags` -lm -ldl
 */

/* in gtk-2: gcc grepl.c -o grepl s7.o glistener.o -Wl,-export-dynamic `pkg-config --libs gtk+-2.0 --cflags` -lm -ldl
 * in gtk-3: gcc grepl.c -o grepl s7.o glistener.o -Wl,-export-dynamic `pkg-config --libs gtk+-3.0 --cflags` -lm -ldl
 * in gtk-4: 
 *    gcc glistener.c -c `pkg-config --libs gtk+-4.0 --cflags` -lm -ldl
 *    gcc grepl.c -o grepl s7.o glistener.o -Wl,-export-dynamic `pkg-config --libs gtk+-4.0 --cflags` -lm -ldl
 */

/* old:
 * (load "libgtk_s7.so" (define *gtk* (inlet 'init_func 'libgtk_s7_init)))
 * (load "/home/bil/cl/libgtk_s7.so" (define *gtk* (inlet 'init_func 'libgtk_s7_init)))
 * (glistener-set-font *g* "Monospace 14")
 *
 * gcc -c libgtk_s7.c -o libgtk_s7.o -I. -fPIC `pkg-config --libs gtk+-3.0 --cflags` -lm -ldl
 * gcc libgtk_s7.o -shared -o libgtk_s7.so
 * (load "libgtk_s7.so" (define *gtk* (inlet 'init_func 'libgtk_s7_init)))
 * (gtk_window_resize (GTK_WINDOW grepl:shell) 600 600)
 */
