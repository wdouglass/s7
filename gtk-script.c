#include <gtk/gtk.h>
#include "s7.h"

static void activate(GtkApplication* app, gpointer user_data)
{
  s7_scheme *s7;
  s7_pointer gtk_env;
  s7 = s7_init();                                  /* fire up s7 */
#if S7_GTK3
  s7_provide(s7, "gtk3");
#else
  s7_provide(s7, "gtk4");
#endif
  gtk_env = s7_inlet(s7, s7_list(s7, 2,            /* create an environment */
	      s7_make_symbol(s7, "*gtk-app*"),     /*   with '*gtk-app* as the GtkApplication pointer */
		s7_make_c_pointer_with_type(s7, (void *)app, 
		  s7_make_symbol(s7, "GtkApplication*"), s7_f(s7))));
  s7_load_with_environment(s7, "coretemp.scm", gtk_env);
}

int main (int argc, char **argv)
{
  GtkApplication *app;
  int status;
  app = gtk_application_new("s7gtk.script", G_APPLICATION_FLAGS_NONE);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
  status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);
  return(status);
}
/* gtk3: gcc `pkg-config --cflags gtk+-3.0` -DS7_GTK3 -o gtk-script gtk-script.c s7.o -Wl,-export-dynamic -lm -ldl `pkg-config --libs gtk+-3.0` 
 * gtk4: gcc `pkg-config --cflags gtk4` -DS7_GTK4 -o gtk-script gtk-script.c s7.o -Wl,-export-dynamic -lm -ldl `pkg-config --libs gtk4` 
 */
