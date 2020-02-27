#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <errno.h>
  #include <unistd.h>
#endif

#include "s7.h"

#ifndef _MSC_VER
static char *realdir(const char *filename)
{
  char *path;
  char *p;

  if (!strchr(filename, '/'))
    {
      char *pwd;
      if (access("libc_s7.so", F_OK) != 0)
	{
	  fprintf(stderr, "%s needs libc_s7.so (give the explicit pathname)\n", filename); /* env PATH=/home/bil/cl repl */
	  exit(2);
	}
      return(NULL);  /* we're in the libc_s7.so directory, I hope (user could start a version of s7 that does not match the local libc_s7.so...) */
    }
  if (!(path = realpath(filename, NULL)))
    {
      fprintf(stderr, "%s: %s\n", strerror(errno), filename);
      exit(2);
    }
  if (!(p = strrchr(path, '/')))
    {
      free(path);
      fprintf(stderr, "please provide the full pathname for %s\n", filename);
      exit(2);
    }
  if (p > path) *p = '\0'; else p[1] = 0;
  return(path);
}
#endif

int main(int argc, char **argv)
{
  s7_scheme *sc;

  sc = s7_init();
  /* fprintf(stderr, "s7: %s\n", S7_DATE); */

  if (argc == 2)
    {
      fprintf(stderr, "load %s\n", argv[1]);
      if (!s7_load(sc, argv[1]))
	{
	  fprintf(stderr, "can't load %s\n", argv[1]);
	  return(2);
	}
    }
  else
    {
#ifdef _MSC_VER
      dumb_repl(sc);
#else
#ifdef S7_LOAD_PATH
      s7_add_to_load_path(sc, S7_LOAD_PATH);
#else
      char *dir; 
      dir = realdir(argv[0]);
      if (dir)
	{
	  s7_add_to_load_path(sc, dir);
	  free(dir);
	}
#endif
      s7_repl(sc);
#endif
    }
  return(0);
}

/* gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl
 */
