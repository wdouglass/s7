/* nrepl, notcurses-base repl
 *
 * gcc -o nrepl nrepl.c s7.o -Wl,-export-dynamic -lnotcurses -lm -I. -ldl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <errno.h>
  #include <unistd.h>
#endif

#include "s7.h"

#include "notcurses_s7.c"

/* libc stuff used in nrepl.scm (this is extracted from libc_s7.c created by cload.scm from libc.scm) */

#include <time.h>
#include <sys/time.h>
#include <glob.h>

static s7_pointer s7__getenv(s7_scheme *sc, s7_pointer arg)
{
  char* s7__getenv_0;
  if (s7_is_string(s7_car(arg)))
    s7__getenv_0 = (char*)s7_string(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 0, s7_car(arg), "string"));
  return(s7_make_string(sc, (char*)getenv(s7__getenv_0)));
}

static s7_pointer s7__time(s7_scheme *sc, s7_pointer arg)
{
  time_t* s7__time_0;
    s7__time_0 = (time_t*)s7_c_pointer_with_type(sc, s7_car(arg), s7_make_symbol(sc, "time_t*"), __func__, 0);
  return(s7_make_integer(sc, (s7_int)time(s7__time_0)));
}

static s7_pointer s7__localtime(s7_scheme *sc, s7_pointer arg)
{
  time_t* s7__localtime_0;
    s7__localtime_0 = (time_t*)s7_c_pointer_with_type(sc, s7_car(arg), s7_make_symbol(sc, "time_t*"), __func__, 0);
  return(s7_make_c_pointer_with_type(sc, (void*)localtime(s7__localtime_0), s7_make_symbol(sc, "tm*"), s7_f(sc)));
}

static s7_pointer g_time_make(s7_scheme *sc, s7_pointer args) 
{
  time_t *tm;
  tm = (time_t *)calloc(1, sizeof(time_t));
  (*tm) = (time_t)s7_integer(s7_car(args));
  return(s7_make_c_pointer_with_type(sc, (void *)tm, s7_make_symbol(sc, "time_t*"), s7_f(sc)));
}

static s7_pointer g_strftime(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, (s7_int)strftime((char *)s7_string(s7_car(args)), 
					      (size_t)s7_integer(s7_cadr(args)), 
					      s7_string(s7_caddr(args)), 
					      (const struct tm *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer s7__isatty(s7_scheme *sc, s7_pointer arg)
{
  int s7__isatty_0;
  if (s7_is_integer(s7_car(arg)))
    s7__isatty_0 = (int)s7_integer(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 0, s7_car(arg), "integer"));
  return(s7_make_integer(sc, (s7_int)isatty(s7__isatty_0)));
}

static s7_pointer s7__fileno(s7_scheme *sc, s7_pointer arg)
{
  FILE* s7__fileno_0;
    s7__fileno_0 = (FILE*)s7_c_pointer_with_type(sc, s7_car(arg), s7_make_symbol(sc, "FILE*"), __func__, 0);
  return(s7_make_integer(sc, (s7_int)fileno(s7__fileno_0)));
}

static s7_pointer s7__fgets(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg;
  char* s7__fgets_0;
  int s7__fgets_1;
  FILE* s7__fgets_2;
  arg = args;
  if (s7_is_string(s7_car(arg)))
    s7__fgets_0 = (char*)s7_string(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 1, s7_car(arg), "string"));
  arg = s7_cdr(arg);
  if (s7_is_integer(s7_car(arg)))
    s7__fgets_1 = (int)s7_integer(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 2, s7_car(arg), "integer"));
  arg = s7_cdr(arg);
    s7__fgets_2 = (FILE*)s7_c_pointer_with_type(sc, s7_car(arg), s7_make_symbol(sc, "FILE*"), __func__, 3);
  return(s7_make_string(sc, (char*)fgets(s7__fgets_0, s7__fgets_1, s7__fgets_2)));
}

static s7_pointer s7__strlen(s7_scheme *sc, s7_pointer arg)
{
  char* s7__strlen_0;
  if (s7_is_string(s7_car(arg)))
    s7__strlen_0 = (char*)s7_string(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 0, s7_car(arg), "string"));
  return(s7_make_integer(sc, (s7_int)strlen(s7__strlen_0)));
}

static s7_pointer g_c_pointer_to_string(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string_with_length(sc, (const char *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args))));
}

static s7_pointer s7__calloc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg;
  size_t s7__calloc_0;
  size_t s7__calloc_1;
  arg = args;
  if (s7_is_integer(s7_car(arg)))
    s7__calloc_0 = (size_t)s7_integer(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 1, s7_car(arg), "integer"));
  arg = s7_cdr(arg);
  if (s7_is_integer(s7_car(arg)))
    s7__calloc_1 = (size_t)s7_integer(s7_car(arg));
  else return(s7_wrong_type_arg_error(sc, __func__, 2, s7_car(arg), "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)calloc(s7__calloc_0, s7__calloc_1), s7_make_symbol(sc, "void*"), s7_f(sc)));
}

static s7_pointer s7__globfree(s7_scheme *sc, s7_pointer arg)
{
  glob_t* s7__globfree_0;
  s7__globfree_0 = (glob_t*)s7_c_pointer_with_type(sc, s7_car(arg), s7_make_symbol(sc, "glob_t*"), __func__, 0);
  globfree(s7__globfree_0);
  return(s7_unspecified(sc));
}

static s7_pointer g_glob_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(glob_t)), s7_make_symbol(sc, "glob_t*"), s7_f(sc)));
}

static s7_pointer g_glob(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, glob(s7_string(s7_car(args)), s7_integer(s7_cadr(args)), NULL, (glob_t *)s7_c_pointer(s7_caddr(args)))));
}

static s7_pointer g_glob_gl_pathv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p;
  int i;
  glob_t *g;
  g = (glob_t *)s7_c_pointer(s7_car(args));
  p = s7_nil(sc);
  for (i = 0; i < g->gl_pathc; i++)
    p = s7_cons(sc, s7_make_string(sc, g->gl_pathv[i]), p);
  return(p);
}

/* catch sigint */
#include <signal.h>

static struct sigaction new_action, old_action;
#if (!USE_SND)
  static s7_scheme *s7;
#endif
static struct notcurses *nc;

void eval_sigint_handler(int signum)
{
  s7_error(s7, s7_t(s7), s7_list(s7, 1, s7_make_string(s7, "interrupted")));
}

static s7_pointer set_sigint_handler(s7_scheme *sc, s7_pointer args)
{
  s7 = sc;
  new_action.sa_handler = eval_sigint_handler;
  sigemptyset(&new_action.sa_mask);
  new_action.sa_flags = SA_RESTART;
  sigaction(SIGINT, &new_action, NULL);
}

void exit_sigint_handler(int signum)
{
  s7_quit(s7);
  notcurses_stop(nc); /* using the actual old_action ("fatal_handler") does not clean up completely -- mouse chatter */
  exit(0);
}

static s7_pointer unset_sigint_handler(s7_scheme *sc, s7_pointer args)
{
  s7 = sc;
  nc = (struct notcurses *)s7_c_pointer(s7_car(args));
  old_action.sa_handler = exit_sigint_handler;
  sigemptyset(&old_action.sa_mask);
  old_action.sa_flags = SA_RESTART;
  sigaction(SIGINT, &old_action, NULL);
}


static void init_nlibc(s7_scheme *sc)
{
  s7_pointer cur_env, pl_tx, pcl_s, pl_ix, pcl_x, pcl_i, pl_ssix, pl_is, pl_xi, pcl_xi;
  s7_int gc_loc;

  cur_env = s7_inlet(sc, s7_nil(sc));
  gc_loc = s7_gc_protect(sc, cur_env);

  {
    s7_pointer t, x, s, d, i;
    t = s7_t(sc);
    x = s7_make_symbol(sc, "c-pointer?");
    s = s7_make_symbol(sc, "string?");
    d = s7_make_symbol(sc, "float?");
    i = s7_make_symbol(sc, "integer?");

    pcl_xi = s7_make_circular_signature(sc, 1, 2, x, i);
    pl_tx = s7_make_signature(sc, 2, t, x);
    pl_xi = s7_make_signature(sc, 2, x, i);
    pl_ix = s7_make_signature(sc, 2, i, x);
    pcl_s = s7_make_circular_signature(sc, 0, 1, s);
    pcl_x = s7_make_circular_signature(sc, 0, 1, x);
    pcl_i = s7_make_circular_signature(sc, 0, 1, i);
    pl_ssix = s7_make_signature(sc, 4, s, s, i, x);
    pl_is = s7_make_signature(sc, 2, i, s);
  }

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getenv"),
            s7_make_typed_function(sc, "getenv", s7__getenv, 1, 0, false, "char* getenv(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "time"),
            s7_make_typed_function(sc, "time", s7__time, 1, 0, false, "int time(time_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "time.make"),
            s7_make_typed_function(sc, "time.make", g_time_make, 1, 0, false, "time.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "localtime"),
            s7_make_typed_function(sc, "localtime", s7__localtime, 1, 0, false, "tm* localtime(time_t*)", pcl_x));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strftime"),
            s7_make_typed_function(sc, "strftime", g_strftime, 4, 0, false, "strftime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isatty"),
            s7_make_typed_function(sc, "isatty", s7__isatty, 1, 0, false, "int isatty(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fileno"),
            s7_make_typed_function(sc, "fileno", s7__fileno, 1, 0, false, "int fileno(FILE*)", pl_ix));

  s7_define(sc, cur_env, s7_make_symbol(sc, "stdin"), s7_make_c_pointer_with_type(sc, (void*)stdin, s7_make_symbol(sc, "FILE*"), s7_f(sc)));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fgets"),
            s7_make_typed_function(sc, "fgets", s7__fgets, 3, 0, false, "char* fgets(char* int FILE*)", pl_ssix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strlen"),
            s7_make_typed_function(sc, "strlen", s7__strlen, 1, 0, false, "size_t strlen(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "c-pointer->string"),
            s7_make_typed_function(sc, "c-pointer->string", g_c_pointer_to_string, 2, 0, false, "c-pointer->string", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "calloc"),
            s7_make_typed_function(sc, "calloc", s7__calloc, 2, 0, false, "void* calloc(size_t size_t)", pcl_xi));

  s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_MARK"), s7_make_integer(sc, (s7_int)GLOB_MARK));
  s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_TILDE"), s7_make_integer(sc, (s7_int)GLOB_TILDE));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob.make"),
            s7_make_typed_function(sc, "glob.make", g_glob_make, 0, 0, false, "glob.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "globfree"),
            s7_make_typed_function(sc, "globfree", s7__globfree, 1, 0, false, "void globfree(glob_t*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob"),
            s7_make_typed_function(sc, "glob", g_glob, 3, 0, false, "glob", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob.gl_pathv"),
            s7_make_typed_function(sc, "glob.gl_pathv", g_glob_gl_pathv, 1, 0, false, "glob.gl_pathv", NULL));
  
  s7_define_constant(sc, "*nlibc*", cur_env);
  s7_gc_unprotect_at(sc, gc_loc);
}


#if (!USE_SND)
int main(int argc, char **argv)
{
  s7_scheme *sc;
  sc = s7_init();
#else
static int nrepl(s7_scheme *sc)
{
#endif
  init_nlibc(sc);

  s7_define_function(sc, "set-sigint-handler", set_sigint_handler, 0, 0, false, "");
  s7_define_function(sc, "unset-sigint-handler", unset_sigint_handler, 1, 0, false, "");

  notcurses_s7_init(sc);

#if (!USE_SND)
  if (argc >= 2)
    {
      if (strcmp(argv[1], "-e") == 0)
	{
	  s7_pointer x;
	  x = s7_eval_c_string(sc, argv[2]);
	  fprintf(stdout, "%s\n", s7_object_to_c_string(sc, x));
	  return(0);
	}
      fprintf(stderr, "load %s\n", argv[1]);
      if (!s7_load(sc, argv[1]))
	{
	  fprintf(stderr, "can't load %s\n", argv[1]);
	  return(2);
	}
    }
  else
#endif
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
#endif
      if (!s7_load(sc, "nrepl.scm"))
	return(1);
      s7_eval_c_string(sc, "((*nrepl* 'run))");
#endif
    }
  return(0);
}

