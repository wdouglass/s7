#include <stdio.h>
#include <stdlib.h>

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
      s7_load(sc, "repl.scm");
      s7_eval_c_string(sc, "((*repl* 'run))");
    }
  return(0);
}

/* gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl
 */
