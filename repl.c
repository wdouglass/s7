#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <errno.h>
  #include <unistd.h>
#endif

#include "s7.h"

#ifndef _MSC_VER
static char *realdir(s7_scheme *sc, const char *filename)
{
  char *path;
  char *p;

  if (!strchr(filename, '/'))
    {
      if (access("libc_s7.so", F_OK) != 0)
	{
	  if ((access("libc.scm", F_OK) == 0) &&
	      (access("cload.scm", F_OK) == 0))
	    {
	      s7_load(sc, "cload.scm");
	      s7_load(sc, "libc.scm");
	      return(NULL);
	    }
	  fprintf(stderr, "%s needs libc_s7.so (give the explicit repl pathname or build it by running: repl libc.scm)\n", filename); /* env PATH=/home/bil/cl repl */
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

  if (argc >= 2)
    {
      if (strcmp(argv[1], "-e") == 0)         /* repl -e '(+ 1 2)' */
	{
	  s7_pointer x;
	  x = s7_eval_c_string(sc, argv[2]);
	  fprintf(stdout, "%s\n", s7_object_to_c_string(sc, x));
	  return(0);
	}
      fprintf(stderr, "load %s\n", argv[1]);  /* repl test.scm */
      if (!s7_load(sc, argv[1]))
	{
	  fprintf(stderr, "can't load %s\n", argv[1]);
	  return(2);
	}
    }
  else
    {
#ifdef _MSC_VER
  while (true)
    {
      char buffer[512];
      fprintf(stdout, "\n> ");
      if (!fgets(buffer, 512, stdin)) break;  /* error or ctrl-D */
      if (((buffer[0] != '\n') || (strlen(buffer) > 1)))
	{
	  char response[1024];
	  snprintf(response, 1024, "(write %s)", buffer);
	  s7_eval_c_string(sc, response);
	}
    }
  fprintf(stdout, "\n");
  if (ferror(stdin))
    fprintf(stderr, "read error on stdin\n");
#else
#ifdef S7_LOAD_PATH
      s7_add_to_load_path(sc, S7_LOAD_PATH);
#else
      char *dir; 
      dir = realdir(sc, argv[0]);
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
