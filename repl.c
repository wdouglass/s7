#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "s7.h"

static char *realdir(const char *filename)
{
  char *path;
  char *p;

  if (!(path = realpath(filename, NULL)))
    return(NULL);
  if ((p = strrchr(path, '/')) > path)
    *p = '\0';
  return(path);
}

int main(int argc, char **argv)
{
  s7_scheme *sc;

  sc = s7_init();
  /* fprintf(stderr, "s7: %s\n", S7_DATE); */

  if (argc == 2)
    {
      fprintf(stderr, "load %s\n", argv[1]);
      s7_load(sc, argv[1]);
    }
  else
    {
#ifdef _MSC_VER
      dumb_repl(sc);
#else
      char *dir;
      if (!(dir = realdir(argv[0])))
        {
          fprintf(stderr, "%s: %s\n", strerror(errno), argv[0]);
          return(2);
        }
      s7_add_to_load_path(sc, dir);
      free(dir);
      s7_repl(sc);
#endif
    }
  return(0);
}

/* gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl
 */
