#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <libgen.h>
#endif

#include "s7.h"

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
      s7_add_to_load_path(sc, dirname(argv[0]));
      s7_repl(sc);
#endif
    }
  return(0);
}

/* gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl
 */
