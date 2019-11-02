#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <libgen.h>
#endif

#include "s7.h"

static void dumb_repl(s7_scheme *sc)
{
  while (true)
    {
      char buffer[512];
      char response[1024];
      fprintf(stdout, "\n> ");
      fgets(buffer, 512, stdin);
      if ((buffer[0] != '\n') || (strlen(buffer) > 1))
	{
	  snprintf(response, 1024, "(write %s)", buffer);
	  s7_eval_c_string(sc, response);
	}
    }
}

int main(int argc, char **argv)
{
  s7_scheme *sc;
  sc = s7_init();

  if (argc == 2)
    {
      fprintf(stderr, "load %s\n", argv[1]);
      if (!s7_load(sc, argv[1]))
	fprintf(stderr, "can't find %s\n", argv[1]);  /* it could also be a directory */
    }
  else 
    {
#ifdef _MSC_VER
      dumb_repl(sc);
#else
      s7_pointer old_e, e, val;
      s7_int gc_loc;
      /* try to get lib_s7.so from the repl's directory, and set *libc*.
       *   otherwise repl.scm will try to load libc.scm which will try to build libc_s7.so locally, but that requires s7.h
       */
      s7_add_to_load_path(sc, dirname(argv[0]));
      e = s7_inlet(sc, s7_list(sc, 2, s7_make_symbol(sc, "init_func"), s7_make_symbol(sc, "libc_s7_init")));
      gc_loc = s7_gc_protect(sc, e);
      old_e = s7_set_curlet(sc, e);   /* e is now (curlet) so loaded names from libc will be placed there, not in (rootlet) */
      val = s7_load_with_environment(sc, "libc_s7.so", e);
      s7_define_variable(sc, "*libc*", e);
      s7_eval_c_string(sc, "(set! *libraries* (cons (cons \"libc.scm\" *libc*) *libraries*))");
      s7_gc_unprotect_at(sc, gc_loc);
      s7_set_curlet(sc, old_e);       /* restore incoming (curlet) */
      if (!val) dumb_repl(sc);
      s7_load(sc, "repl.scm");
      s7_eval_c_string(sc, "((*repl* 'run))");
#endif
    }
  return(0);
}

/* gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl
 */
