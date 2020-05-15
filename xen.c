/* xen support procedures */

#include "mus-config.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <math.h>
#include <time.h>

#ifdef _MSC_VER
  #include <io.h>
  #include <process.h>
  #pragma warning(disable: 4244)
#endif

#include "xen.h"

#define S_gc_off "gc-off"
#define S_gc_on  "gc-on"


char *xen_strdup(const char *str)
{
  char *newstr = NULL;
  if ((!str) || (!(*str))) return(NULL);
  newstr = (char *)malloc(strlen(str) + 1);
  if (newstr) strcpy(newstr, str);
  return(newstr);
}



/* ------------------------------ RUBY ------------------------------ */

#if HAVE_RUBY

#define HAVE_RB_PROC_NEW 1
/* As the README says, only versions of ruby 1.8 or later will work */

#if USE_SND
void snd_rb_raise(Xen type, Xen info); /* XEN_ERROR */
#endif

#define S_add_help "add_help"
#define S_get_help "get_help"

Xen rb_documentation(Xen name)
{
  Xen_check_type((Xen_is_string(name) || Xen_is_symbol(name)), name, 1, S_get_help, "a char* or symbol");
  if (Xen_is_symbol(name))
    return(rb_property(XEN_SYMBOL_TO_STRING(name), Xen_documentation_symbol));
  else
    return(rb_property(name, Xen_documentation_symbol));
}


Xen rb_set_documentation(Xen name, Xen help)
{
  Xen_check_type((Xen_is_string(name) || Xen_is_symbol(name)), name, 1, S_add_help, "a char* or symbol");
  Xen_check_type(Xen_is_string(help), help, 2, S_add_help, "a char*");
  if (Xen_is_symbol(name))
    rb_set_property(XEN_SYMBOL_TO_STRING(name), Xen_documentation_symbol, help);
  else
    rb_set_property(name, Xen_documentation_symbol, help);
  return(name);
}


static Xen g_add_help(Xen name, Xen help)
{
#define H_add_help S_add_help "(name, help)  add help to topic or function name (String or Symbol)"
  return(rb_set_documentation(name, help));
}


static Xen g_get_help(Xen name)
{
#define H_get_help S_get_help "([name=:" S_get_help "])  \
return help associated with name (String or Symbol) or false"
  if (!Xen_is_bound(name))
    return(C_string_to_Xen_string(H_get_help));
  else
    return(rb_documentation(name));
}


void xen_initialize(void)
{
#ifdef RUBY_INIT_STACK
  RUBY_INIT_STACK;
#endif

  ruby_init();
  ruby_init_loadpath();
  ruby_script("xen");        /* necessary in ruby 1.9 (else segfault in rb_raise -- is this the rb GC bug (see snd-xen.c)?) */

  Init_Hook();
}


void xen_gc_mark(Xen val)
{
  rb_gc_mark(val);
}


Xen xen_rb_cdr(Xen val)
{
  if (Xen_is_cons(val))
    {
      Xen new_list;
      new_list = Xen_copy_arg(val);
      rb_ary_delete_at(new_list, 0);
      return(new_list);
    }
  return(val);
}


Xen xen_rb_cons(Xen arg1, Xen arg2)
{
  if (Xen_is_null(arg2))
    return(rb_ary_new3(1, arg1));
  if (!(Xen_is_cons(arg2)))
    return(rb_ary_new3(2, arg1, arg2));
  return(rb_ary_unshift(arg2, arg1)); /* arg2 assumed to be array here in Ruby */
}


Xen xen_rb_cons2(Xen arg1, Xen arg2, Xen arg3)
{
  return(rb_ary_unshift(xen_rb_cons(arg2, arg3), arg1));
}


Xen xen_rb_ary_new_with_initial_element(long num, Xen element)
{
  Xen arr;
  int i;
  arr = rb_ary_new2(num);
  for (i = 0; i < num; i++)
    rb_ary_store(arr, i, element);
  return(arr);
}


Xen xen_set_assoc(Xen key, Xen val, Xen alist)
{
  /* assoc key val in alist so later rb_ary_assoc will find val given key in alist */
  /*
    if array?(alist)
      if array?(item = alist.assoc(key))
        item[1] = val
      else
        alist.push([key, val])
      end
    else
      [[key, val]]
    end
  */
  if (Xen_is_cons(alist))
    {
      Xen pair;
      pair = rb_ary_assoc(alist, key);
      if (Xen_is_cons(pair))
	rb_ary_store(pair, 1, val);
      else rb_ary_push(alist, rb_assoc_new(key, val));
      return(alist);
    }
  return(rb_ary_new3(1, rb_assoc_new(key, val)));
}


Xen xen_assoc(Xen key, Xen alist) 
{ 
  if (Xen_is_cons(alist)) 
    { 
      Xen val; 
      val = rb_ary_assoc(alist, key); 
      if (val != Qnil) 
	return(rb_ary_entry(val, 1)); 
    } 
  return(Qfalse); 
} 


static char *scheme_to_ruby(const char *name)
{
  /* replace any non-alphanumeric except "?" with "_". "?" -> "_p". '->" -> "2" drop "!" */
  char *new_name = NULL;
  int len;
  len = strlen(name);
  if (len > 0)
    {
      int i, j;
      new_name = (char *)calloc(len + 3, sizeof(char)); /* +1 for possible _p, +1 for possible $ */
      for (i = 0, j = 0; i < len; i++)
	{
	  if (isalnum(name[i]))
	    new_name[j++] = name[i];
	  else 
	    {
	      if (name[i] != '!')
		{
		  if ((name[i] == '-') &&
		      (name[i + 1] == '>'))
		    {
		      new_name[j++] = '2';
		      i++;
		    }
		  else
		    {
		      new_name[j++] = '_';
		      if (name[i] == '?')
			new_name[j++] = 'p';
		    }
		}
	    }
	}
    }
  return(new_name);
}


char *xen_scheme_constant_to_ruby(const char *name)
{
  /* upcase first char */
  char *new_name;
  new_name = scheme_to_ruby(name);
  new_name[0] = toupper(new_name[0]);
  return(new_name);
}


char *xen_scheme_procedure_to_ruby(const char *name)
{
  char *new_name = NULL;
  int len;
  len = name ? strlen(name) : 0;
  if (len > 0)
    {
      int i, j;
      new_name = (char *)calloc(len + 1, sizeof(char));
      for (i = 0, j = 0; i < len; i++)
	{
	  if ((isalnum(name[i])) || (name[i] == '!') || (name[i] == '?'))
	    new_name[j++] = name[i];
	  else 
	    {
	      if ((name[i] == '-') &&
		  (name[i + 1] == '>'))
		{
		  new_name[j++] = '2';
		  i++;
		}
	      else new_name[j++] = '_';
	    }
	}
    }
  return(new_name);
}


char *xen_scheme_global_variable_to_ruby(const char *name)
{
  /* prepend $ */
  char *new_name;
  new_name = scheme_to_ruby(name);
  if (new_name[0] == '_')
    new_name[0] = '$';
  else
    {
      int i, len;
      len = strlen(new_name);
      for (i = len; i > 0; i--)
	new_name[i] = new_name[i - 1];
      new_name[0] = '$';
    }
  return(new_name);
}


/* looks for global variables and constants (functions too?) */

bool xen_rb_defined_p(const char *name)
{
  char *var_name = scheme_to_ruby(name);
  char buf[128];

  if (var_name[0] == '$')
    snprintf(buf, 128, "defined? %s", var_name);
  else snprintf(buf, 128, "defined? $%s", var_name);

  if (Xen_eval_C_string(buf) != Qnil)
    {
      free(var_name);
      return(true);
    }
  else
    {
      bool val;
      var_name[0] = toupper(var_name[0]);
      val = rb_const_defined(rb_cObject, rb_intern(var_name));
      free(var_name);
      return(val);
    }
}


Xen xen_rb_gv_get(const char *name)
{
  char *temp;
  Xen val;
  temp = xen_scheme_global_variable_to_ruby(name);
  val = rb_gv_get(temp);
  if (temp) free(temp);
  return(val);
}


Xen xen_rb_gv_set(const char *name, Xen new_val)
{
  char *temp;
  Xen val;
  temp = xen_scheme_global_variable_to_ruby(name);
  val = rb_gv_set(temp, new_val);
  if (temp) free(temp);
  return(val);
}


Xen xen_rb_intern(const char *name)
{
  char *temp;
  Xen val;
  temp = xen_scheme_constant_to_ruby(name);
  val = rb_intern(temp);
  if (temp) free(temp);
  return(val);
}


Xen xen_rb_make_keyword(const char *name)
{
  char *temp;
  Xen val;
  temp = xen_scheme_procedure_to_ruby(name);
  val = C_string_to_Xen_symbol(temp);
  if (temp) free(temp);
  return(val);
}


void xen_rb_define(const char *name, Xen value)
{
  char *temp;
  temp = xen_scheme_constant_to_ruby(name);
  rb_define_global_const(temp, value);
  if (temp) free(temp);
}


Xen xen_rb_define_class(const char *name)
{
  char *temp;
  Xen val;
  temp = xen_scheme_constant_to_ruby(name);
  val = rb_define_class(temp, rb_cObject);
  if (temp) free(temp);
  return(val);
}




#ifndef RARRAY_PTR 
  #define RB_ARRAY_PTR(Ary) RARRAY(Ary)->ptr 
  #define RB_ARRAY_LEN(Ary) RARRAY(Ary)->len 
#else 
  #define RB_ARRAY_PTR(Ary) RARRAY_PTR(Ary) 
  #define RB_ARRAY_LEN(Ary) RARRAY_LEN(Ary) 
#endif 
 

int xen_rb_list_length(Xen obj) 
{ 
  if (Xen_is_vector(obj)) 
     return((int)RB_ARRAY_LEN(obj)); 
  if (obj == Xen_empty_list) 
    return(0); 
  return(-1); 
} 


Xen xen_rb_list_ref(Xen obj, int index)
{
  if (Xen_is_vector(obj))
    return(rb_ary_entry(obj, (long)index));
  return(Xen_empty_list);
}


Xen xen_rb_list_set(Xen obj, int index, Xen value)
{
  if (Xen_is_vector(obj))
    rb_ary_store(obj, (long)index, value);
  return(value);
}


char *xen_version(void)
{
  /* there is no macro we can depend on for the version number (its name changes unpredictably),
   *   and ruby/version.h tries to be funny about how unreliable their semi-functional access is.
   *   Maybe use <ruby/version.h> and ruby_version here (a const char*).
   * No, even that doesn't work because there's no way to tell whether version.h exists.
   *   Humph!
   */
  char *buf;
  buf = (char *)calloc(128, sizeof(char));
  snprintf(buf, 128, "%s", "Ruby");
  return(buf);
}


static Xen xen_rb_report_error(Xen nada, Xen err_info)
{
  /* backtrace info: */
  /*    return rb_funcall(err_info, rb_intern("backtrace"), 0); */
  /* which can be an array of strings */

  fprintf(stderr,"error: %s\n", Xen_object_to_C_string(err_info));
  return(Xen_false);
}


static char *rb_prompt = NULL;

static Xen xen_rb_rep(Xen ig)
{
  Xen val;
  char *str, *res;
  size_t size = 512;
  char **buffer;
  buffer = (char **)calloc(1, sizeof(char *));
  buffer[0] = (char *)calloc(size, sizeof(char));
  fprintf(stdout, "%s", rb_prompt);
  res = fgets(buffer[0], size, stdin); /* check result to make compiler happy */
  if (!res) fprintf(stderr, "fgets returns null\n");
  val = xen_rb_eval_string_with_error(buffer[0]);
  str = Xen_object_to_C_string(val);
  fprintf(stdout, "%s\n", (str) ? str : "nil");
  free(buffer[0]);
  free(buffer);
  return(ig);
}


void xen_rb_repl_set_prompt(const char *prompt)
{
  if (rb_prompt) free(rb_prompt);
  rb_prompt = xen_strdup(prompt);
}


static Xen xen_rb_rescue(Xen val)
{
  if (!rb_prompt) rb_prompt = xen_strdup(">");
  return(rb_rescue(Xen_procedure_cast xen_rb_rep,
		   Xen_false,
		   Xen_procedure_cast xen_rb_report_error,
		   Xen_false));
}


void xen_repl(int argc, char **argv)
{
  while (true)
    {
      int status = 0;
      rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_rescue,
		 Xen_false,
		 &status);
      if (status != 0)
	{
	  fprintf(stderr, "%s\n", Xen_object_to_C_string(rb_gv_get("$!")));
	  status = 0;
	}
    }
}


Xen xen_rb_eval_string_with_error(const char *str)
{
  int status = 0;
  Xen res;
  res = rb_eval_string_protect(str, &status);
  if (status != 0)
    return(xen_rb_obj_as_string(rb_gv_get("$!")));
  return(res);
}


void xen_rb_load_file_with_error(const char *file)
{
  int status = 0, i;
  Xen err, info;
  rb_load_protect(C_string_to_Xen_string(file), 0, &status);
  if (status == 0)
    return;
  fprintf(stderr, "Can't load %s", file);
  err = rb_gv_get("$!");
  if (err != Qnil)
    fprintf(stderr, ": %s", Xen_object_to_C_string(err));
  fprintf(stderr, "\n");
  info = rb_gv_get("$@");
  if (info == Qnil)
    return;
  for (i = 0; i < Xen_vector_length(info); i++)
    fprintf(stderr, "%s\n", Xen_string_to_C_string(Xen_vector_ref(info, i)));
}


Xen xen_rb_add_to_load_path(char *path) 
{ 
 Xen rpath, load_path; 
 rpath = rb_str_new2(path); 
 load_path = rb_gv_get("$:");
 if (Xen_is_false(rb_ary_includes(load_path, rpath))) 
   rb_ary_unshift(load_path, rpath); 
 return(Xen_false); 
} 


static char *lstbuf = NULL;

static char *xen_rb_list_to_s(Xen lst)
{
  int i, len;
  if (!lstbuf) 
    lstbuf = (char *)calloc(512, sizeof(char));
  else lstbuf[0] = '\0';
  len = Xen_list_length(lst);
  for (i = 0; i < len; i++)
    {
      strcat(lstbuf, Xen_object_to_C_string(Xen_list_ref(lst, i)));
      strcat(lstbuf, " ");
    }
  return(lstbuf);
}


void xen_rb_raise(Xen type, Xen info)
{
  rb_raise(rb_eStandardError, "%s: %s\n", 
	   rb_id2name(type), 
	   xen_rb_list_to_s(info));
}


int xen_rb_required_args(Xen val)
{
  int args;
  args = Xen_integer_to_C_int(val);
  if (args == -1) return(1); 
  if (args < 0) return(abs(args + 1));
  return(args);
}


Xen xen_rb_obj_as_string(Xen obj)
{
  int status = 0;
  Xen result;
  result = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST rb_obj_as_string,
		      obj,
		      &status);
  if (status != 0)
    return(C_string_to_Xen_string("<invalid object>"));
  return(result);
}


#if HAVE_RB_PROC_NEW

static Xen xen_rb_apply_1(Xen args) 
{ 
  return(rb_apply(Xen_car(args), rb_intern("call"), Xen_cadr(args))); 
} 

#else

static Xen xen_rb_apply_1(Xen args)
{
  if (Xen_is_procedure(Xen_car(args))) 
    return(rb_apply(Xen_car(args), rb_intern("call"), Xen_cadr(args))); 
  return(rb_apply(rb_mKernel, Xen_car(args), Xen_cadr(args))); 
}

#endif


Xen xen_rb_apply(Xen func, Xen args)
{
  Xen val;
  int status = 0;
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_apply_1,
		   Xen_list_2(func, args),
		   &status);
  if (status != 0)
    return(xen_rb_obj_as_string(rb_gv_get("$!")));
  return(val);
}


static Xen xen_rb_funcall_0_inner(Xen args)
{
  return(rb_funcall(args, rb_intern("call"), 0));
}


Xen xen_rb_funcall_0(Xen func)
{
  Xen val;
  int status = 0;
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_funcall_0_inner,
		   func,
		   &status);
  if (status != 0)
    return(xen_rb_obj_as_string(rb_gv_get("$!")));
  return(val);
}


Xen xen_rb_copy_list(Xen val) 
{ 
  if ((val == Xen_empty_list) || (!Xen_is_cons(val)))
    return Xen_empty_list; 
  return rb_ary_dup(val); 
} 


Xen xen_rb_str_new2(char *arg)
{
  return(rb_str_new2((arg) ? arg : ""));
}


/* class Hook */
 
static Xen xen_rb_cHook;

static Xen hook_alloc(Xen klass)
{
  return(Data_Wrap_Struct(klass, 0, 0, 0));
}


#define Xen_is_class_hook(Arg)              rb_obj_is_kind_of(Arg, xen_rb_cHook)

bool xen_rb_hook_p(Xen obj)
{
  return(Xen_is_class_hook(obj));
}


bool xen_rb_hook_empty_p(Xen obj) 
{ 
  if (Xen_is_class_hook(obj)) 
    return(RB_ARRAY_LEN(rb_iv_get(obj, "@procs")) == 0); 
  return(true); 
} 


/*
 * @name = "$name_of_hook"
 * @arity = arity of procedure(s),         default 0
 * @procs = [["named proc1", proc1], ...]
 */

static Xen xen_rb_hook_initialize(int argc, Xen *argv, Xen hook)
{
  Xen name, arity, help;
  rb_scan_args(argc, argv, "12", &name, &arity, &help);
  Xen_check_type(Xen_is_string(name) || Xen_is_symbol(name), name, 1, __func__, "a char* or symbol");
  if (Xen_is_symbol(name))
    name = XEN_SYMBOL_TO_STRING(name);
  if (arity != Qnil)
    {
      Xen_check_type(Xen_is_integer(arity), arity, 2, __func__, "an integer");
    }
  else arity = INT2NUM(0);
  if (help != Qnil)
    {
      Xen_check_type(Xen_is_string(help), help, 3, __func__, "a char*");
      XEN_SET_OBJECT_HELP(name, help);
    }
  rb_iv_set(hook, "@name", name);
  rb_iv_set(hook, "@arity", arity);
  rb_iv_set(hook, "@procs", rb_ary_new());
  return(hook);
}


/*
 * To create a simple hook in C, see xen.h, XEN_DEFINE_SIMPLE_HOOK.
 * To create a global hook variables, see xen_rb_create_hook() below.
 */

Xen xen_rb_hook_c_new(char *name, int arity, char *help)
{
  Xen args[3];
  args[0] = C_string_to_Xen_string(name);
  args[1] = C_int_to_Xen_integer(arity);
  args[2] = C_string_to_Xen_string(help);
  return(xen_rb_hook_initialize(3, args, hook_alloc(xen_rb_cHook)));
}


/*
  RUBY_RELEASE_DATE < "2004-03-18" ? old : new

  lambda do         end.arity 	    -1     0 !!!
  lambda do ||      end.arity 	     0     0
  lambda do |a|     end.arity 	    -1     1 !!!
  lambda do |*a|    end.arity 	    -1    -1
  lambda do |a, b|  end.arity 	     2     2
  lambda do |a, *b| end.arity 	    -2    -2
  etc.
*/

#ifdef RUBY_VERSION
  #define XEN_RUBY_RELEASE_DATE  RUBY_RELEASE_DATE
#else
  #define XEN_RUBY_RELEASE_DATE  Xen_string_to_C_string(Xen_eval_C_string("RUBY_RELEASE_DATE"))
#endif

#define RUBY_NEW_ARITY_DATE   "2004-03-18"
#define OLD_RUBY_ARITY()      (strcmp(XEN_RUBY_RELEASE_DATE, RUBY_NEW_ARITY_DATE) < 0)
/* #define NEW_RUBY_ARITY()      (strcmp(XEN_RUBY_RELEASE_DATE, RUBY_NEW_ARITY_DATE) >= 0) */

bool xen_rb_arity_ok(int rargs, int args)
{
  if (OLD_RUBY_ARITY())
    {
      if ((rargs >= 2) || (rargs == 0))
	return(rargs == args);
      else if (rargs <= -2)
	return(abs(rargs) <= args);
      else			/* rargs -1 remains (no 1 exists) */
	return((args == 1) || (args == 0) || (args == -1));
    }
  else /* NEW_RUBY_ARITY */
    return((rargs >= 0) ? (rargs == args) : (abs(rargs) <= args));
}

 
static Xen xen_rb_hook_add_hook(int argc, Xen *argv, Xen hook)
{
  Xen name, func;
  int args;
  args = Xen_integer_to_C_int(rb_iv_get(hook, "@arity"));
  rb_scan_args(argc, argv, "1&", &name, &func);
  Xen_check_type(Xen_is_string(name), name, 1, __func__, "a char*");
  Xen_check_type(Xen_is_procedure(func) && xen_rb_arity_ok(Xen_integer_to_C_int(Xen_arity(func)), args),
		  func, 2, __func__, "a procedure");
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, name, func));
  return(hook);
}


#if HAVE_RB_PROC_NEW

static Xen xen_proc_call(Xen args, Xen id) 
{ 
  return(rb_apply(rb_mKernel, (ID)id, Xen_is_cons(args) ? args : Xen_list_1(args))); 
} 

#if 0
  VALUE rb_proc_new((VALUE (*)(ANYARGS/* VALUE yieldarg[, VALUE procarg] */), VALUE)); 
  void rb_define_module_function(VALUE,const char*,VALUE(*)(ANYARGS),int);
#endif

static Xen xen_rb_proc_new(const char *name, VALUE (*func)(ANYARGS), int arity, const char* doc) 
{ 
  switch (arity) /* g++ 10 insists that arity arg must be a constant! */
    {
    case 0: rb_define_module_function(rb_mKernel, name, func, 0); break;
    case 1: rb_define_module_function(rb_mKernel, name, func, 1); break;
    case 2: rb_define_module_function(rb_mKernel, name, func, 2); break;
    case 3: rb_define_module_function(rb_mKernel, name, func, 3); break;
    case 4: rb_define_module_function(rb_mKernel, name, func, 4); break;
    case 5: rb_define_module_function(rb_mKernel, name, func, 5); break;
    case 6: rb_define_module_function(rb_mKernel, name, func, 6); break;
    case 7: rb_define_module_function(rb_mKernel, name, func, 7); break;
    case 8: rb_define_module_function(rb_mKernel, name, func, 8); break;
    default: fprintf(stderr, "arity: %d\n", arity);
    }
  if (doc) C_SET_OBJECT_HELP(name, doc); 
  return(rb_proc_new(Xen_procedure_cast xen_proc_call, rb_intern(name))); 
} 


static Xen xen_rb_hook_arity(Xen hook); 

Xen xen_rb_add_hook(Xen hook, VALUE (*func)(ANYARGS), const char *name, const char* doc) 
{ 
  /* called from C, not Ruby, to add a function to a Ruby-side hook */ 
  char *temp; 
  temp = xen_scheme_procedure_to_ruby(name); 
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, C_string_to_Xen_string(temp), xen_rb_proc_new(temp, func, Xen_integer_to_C_int(xen_rb_hook_arity(hook)), doc))); 
  if (temp) free(temp); 
  return(hook); 
} 

#else

Xen xen_rb_add_hook(Xen hook, VALUE (*func)(), const char *name, const char* doc) 
{
  /* called from C, not Ruby, to add a function to a Ruby-side hook 
   *   this doesn't work in g++ because it thinks the funcs are invalid:
   *   "error: invalid conversion from 'VALUE (*)(VALUE, VALUE)' to 'VALUE (*)(...)'" (snd-file.c etc)
   */ 
  Xen var, avar; 
  char *temp; 
  temp = xen_scheme_procedure_to_ruby(name); 
  avar = rb_iv_get(hook, "@arity");
  rb_define_module_function(rb_mKernel, temp, Xen_procedure_cast func, (Xen_is_integer(avar)) ? Xen_integer_to_C_int(avar) : 0); 
  if (doc) C_SET_OBJECT_HELP(temp, doc); 
  var = rb_intern(temp); 
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, C_string_to_Xen_string(temp), var)); 
  if (temp) free(temp); 
  return(hook); 
}

#endif


static Xen xen_rb_hook_remove_hook(Xen hook, Xen name)
{
  Xen ary;
  ary = rb_iv_get(hook, "@procs");
  return(rb_ary_delete(ary, rb_ary_assoc(ary, name)));
}


Xen xen_rb_hook_reset_hook(Xen hook)
{
  if (Xen_is_class_hook(hook))
    rb_ary_clear(rb_iv_get(hook, "@procs"));
  return(hook);
}


static Xen xen_rb_hook_names(Xen hook)
{
  Xen ary, ret = Qnil;
  long len;
  ary = rb_iv_get(hook, "@procs");
  len = RB_ARRAY_LEN(ary); 
  if (len > 0)
    {
      long i;
      ret = rb_ary_new2(len);
      for (i = 0; i < len; i++)
	rb_ary_store(ret, i, Xen_vector_ref(Xen_vector_ref(ary, i), 0));
    }
  return(ret);
}


Xen xen_rb_hook_to_a(Xen hook)
{
  Xen ret = Qnil;
  if (Xen_is_class_hook(hook))
    {
      Xen ary;
      long len;
      ary = rb_iv_get(hook, "@procs");
      len = Xen_list_length(ary);
      if (len > 0)
	{
	  long i;
	  ret = rb_ary_new2(len);
	  for (i = 0; i < len; i++)
	    rb_ary_store(ret, i, Xen_vector_ref(Xen_vector_ref(ary, i), 1));
	}
    }
  return(ret);
}


static Xen xen_rb_hook_run_hook(Xen hook)
{
  if (RB_ARRAY_LEN(rb_iv_get(hook, "@procs"))) 
    rb_ary_each(xen_rb_hook_to_a(hook));
  return(hook);
}


/*
 * Calls all hook-procedures but returns only the last result; use
 * $var_hook.run_hook { |prc| ret << prc.call(*args) } for collecting
 * results.
 */

static Xen xen_rb_hook_call(int argc, Xen *argv, Xen hook)
{
  Xen result = Qnil, rest, procs;
  rb_scan_args(argc, argv, "*", &rest);
  procs = xen_rb_hook_to_a(hook);
  if (procs != Qnil)
    {
      long i;
      for (i = 0; i < RB_ARRAY_LEN(procs); i++) 
	result = xen_rb_apply(rb_ary_entry(procs, i), rest);
    }
  return(result);
}


static Xen xen_rb_hook_is_empty_p(Xen hook)
{
  return(C_bool_to_Xen_boolean(RB_ARRAY_LEN(rb_iv_get(hook, "@procs")) == 0)); 
}

 
static Xen xen_rb_hook_length(Xen hook)
{
  return(C_int_to_Xen_integer(RB_ARRAY_LEN(rb_iv_get(hook, "@procs")))); 
}


static Xen xen_rb_hook_name(Xen hook)
{
  return(rb_iv_get(hook, "@name"));
}


static Xen xen_rb_hook_describe(Xen hook)
{
  return(Xen_documentation(xen_rb_hook_name(hook)));
}


static Xen xen_rb_hook_arity(Xen hook)
{
  return(rb_iv_get(hook, "@arity"));
}


static Xen xen_rb_hook_inspect(Xen hook)
{
  Xen str = rb_str_new2("#<Hook name: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@name")));
  rb_str_cat2(str, ", arity: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@arity")));
  rb_str_cat2(str, ", procs[");
  rb_str_append(str, rb_inspect(xen_rb_hook_length(hook)));
  rb_str_cat2(str, "]: ");
  rb_str_append(str, rb_inspect(xen_rb_hook_names(hook)));
  rb_str_cat2(str, ">");
  return(str);
}    


/* bil -- added xen_rb_create_hook for Xen_define_hook in xen.h, 13-Jun-05 --
 *   seems to work, but I'm guessing, especially the rb_gv_set line.
 *   I can't use rb_define_variable here, as in the old version, because it takes a pointer
 *   to the new variable, which in this case is a local variable => segfault.
 */

Xen xen_rb_create_hook(char *name, int arity, char *help)
{
  Xen var, hook_name;
  char *temp;
  var = xen_rb_hook_c_new(temp = xen_scheme_global_variable_to_ruby(name), arity, help);
  hook_name = xen_rb_hook_name(var);
  rb_gv_set(Xen_string_to_C_string(hook_name), var);
  if (temp) free(temp);
  return(var);
}

static int simple_hook_number = 0; 
 

Xen xen_rb_create_simple_hook(int arity) 
{ 
  char *name; 
  Xen hook; 
  name = (char *)calloc(20, sizeof(char)); 
  snprintf(name, 20, "simple_%02d_hook", simple_hook_number++); 
  hook = xen_rb_create_hook(name, arity, NULL); 
  free(name); 
  return(hook); 
} 


/*
 * make_hook(name, arity = 0, help = "", hook_name = nil, &func)
 *
 * make_hook("var_hook")
 *   == $var_hook = Hook.new("var_hook")
 * make_hook("var_hook", 1)
 *   == $var_hook = Hook.new("var_hook", 1)
 * make_hook("var_hook", 1, "help $var_hook")
 *   == $var_hook = Hook.new("var_hook", 1, "help $var_hook")
 * 
 * make_hook("var_hook", 1, "help $var_hook", "1st proc") do |a| ... end
 *   == $var_hook = Hook.new("var_hook", 1, "help $var_hook")
 *      $var_hook.add_hook!("1st proc") do |a| ... end
 */

#ifndef RSTRING_LEN 
  #define RB_STR_LEN(str)                RSTRING(str)->len 
#else 
  #define RB_STR_LEN(str)                RSTRING_LEN(str) 
#endif 

static Xen xen_rb_make_hook(int argc, Xen *argv, Xen klass)
{
  Xen hook = Xen_false, name;
  if (argc > 0 && argc < 4)
    {
      hook = xen_rb_hook_initialize(argc, argv, hook_alloc(xen_rb_cHook));
      if (rb_block_given_p())
	{
	  argv[0] = rb_str_new2("");
	  xen_rb_hook_add_hook(1, argv, hook);
	}
    }
  else if (argc == 4 && rb_block_given_p())
    {
      hook = xen_rb_hook_initialize(3, argv, hook_alloc(xen_rb_cHook));
      argv[0] = argv[3];
      xen_rb_hook_add_hook(1, argv, hook);
    }
  else Xen_error(Xen_make_error_type("wrong-number-of-args"),
		 Xen_list_1(C_string_to_Xen_string("make_hook(name, arity=0, help=\"\", hook_name=\"\", &func)")));
  name = xen_rb_hook_name(hook);
  if (Xen_char_to_C_char(name) != '$') 
    {
      char *temp;
      temp = xen_scheme_global_variable_to_ruby(Xen_string_to_C_string(name)); 
      name = C_string_to_Xen_string(temp);
      if (temp) free(temp);
    }
  Xen_check_type(RB_STR_LEN(name) >= 2, name, 1, __func__, "a char*, len >= 2"); 
  return(rb_gv_set(Xen_string_to_C_string(name), hook)); 
}


static Xen xen_rb_is_hook_p(Xen klass, Xen obj)
{
  return(C_bool_to_Xen_boolean(Xen_is_class_hook(obj)));
}


/*
 * Hook.new(name, arity = 0, help = "")
 *
 * $my_hook = Hook.new("my_hook", 2, "info of my_hook")
 * $my_hook.add_hook!("1st proc") do |a, b| ... end
 *     or make_hook("my_hook", 2, "info of my_hook", "1st proc") do |a, b| ... end
 * 
 * $my_hook.add_hook!("2nd proc") do |a, b| ... end
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[2]: ["1st proc", "2nd proc"]>
 *
 * ret = 0
 * $my_hook.run_hook do |prc| ret = prc.call(ret, 2) end
 *
 * $my_hook.help      --> info of my_hook
 * $my_hook.remove_hook!("1st proc")
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[1]: ["2nd proc"]>
 * 
 * $my_hook.remove_hook!("2nd proc")
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[0]: nil>
 */

#if (!HAVE_RB_DEFINE_ALLOC_FUNC)
static Xen xen_rb_new(int argc, Xen *argv, Xen klass)
{
  Xen hook = hook_alloc(klass);
  rb_obj_call_init(hook, argc, argv);
  return(hook);
}
#endif


static Xen rb_object_properties = Xen_false;

#define S_property       "property"
#define S_set_property   "set_property"
#define S_properties     "properties"


Xen rb_property(Xen obj, Xen key)
{
#define H_property S_property "(obj, key)  \
if key exists, return obj's value (maybe nil) associated with key otherwise false"
  Xen props = Xen_false;
  
  if (Xen_is_false(rb_object_properties))
    return(Xen_false);

  props = rb_hash_aref(rb_object_properties, obj);

  if (Xen_is_false(props) || props == Qnil)
    return(Xen_false);
  else
    return(rb_hash_aref(props, key));
}


Xen rb_set_property(Xen obj, Xen key, Xen value)
{
#define H_set_property S_set_property "(obj, key, value)  \
set key-value pair for obj and return value"
  Xen props = Xen_false;

  if (Xen_is_false(rb_object_properties))
    {
      rb_object_properties = rb_hash_new();
      Xen_GC_protect(rb_object_properties);
    }
  else
    props = rb_hash_aref(rb_object_properties, obj);
  
  if (Xen_is_false(props) || props == Qnil)
    props = rb_hash_new();
  
  rb_hash_aset(props, key, value);
  rb_hash_aset(rb_object_properties, obj, props);
  return(value);
}


Xen rb_properties(void)
{
#define H_properties S_properties "()  return all properties of rb_object_properties (a hash)"
  return(rb_object_properties);
}


static Xen g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
  rb_gc_disable();
  return(Xen_false);
}


static Xen g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
  rb_gc_enable();
  return(Xen_false);
}


Xen_wrap_1_optional_arg(g_get_help_w, g_get_help);
Xen_wrap_2_args(g_add_help_w, g_add_help);
Xen_wrap_3_args(g_set_property_w, rb_set_property);
Xen_wrap_2_args(g_property_w, rb_property);
Xen_wrap_no_args(g_properties_w, rb_properties);

Xen_wrap_no_args(g_gc_off_w, g_gc_off)
Xen_wrap_no_args(g_gc_on_w, g_gc_on)


static bool hook_inited = false;

void Init_Hook(void)
{
  if (hook_inited) return;
  hook_inited = true;
 
  xen_rb_cHook = rb_define_class("Hook", rb_cObject);
  rb_include_module(xen_rb_cHook, rb_mEnumerable);
#if HAVE_RB_DEFINE_ALLOC_FUNC
  rb_define_alloc_func(xen_rb_cHook, hook_alloc);
#else
  rb_define_singleton_method(xen_rb_cHook, "new", Xen_procedure_cast xen_rb_new, -1);
#endif
    
  rb_define_method(xen_rb_cHook, "initialize", Xen_procedure_cast xen_rb_hook_initialize, -1);
  rb_define_method(xen_rb_cHook, "add_hook!", Xen_procedure_cast xen_rb_hook_add_hook, -1);
  rb_define_method(xen_rb_cHook, "remove_hook!", Xen_procedure_cast xen_rb_hook_remove_hook, 1);
  rb_define_method(xen_rb_cHook, "reset_hook!", Xen_procedure_cast xen_rb_hook_reset_hook, 0);
  rb_define_alias(xen_rb_cHook, "clear", "reset_hook!");
  rb_define_method(xen_rb_cHook, "to_a", Xen_procedure_cast xen_rb_hook_to_a, 0);
  rb_define_method(xen_rb_cHook, "run_hook", Xen_procedure_cast xen_rb_hook_run_hook, 0);
  rb_define_alias(xen_rb_cHook, "each", "run_hook");
  rb_define_method(xen_rb_cHook, "call", Xen_procedure_cast xen_rb_hook_call, -1);
  rb_define_method(xen_rb_cHook, "length", Xen_procedure_cast xen_rb_hook_length, 0);
  rb_define_alias(xen_rb_cHook, "size", "length");
  rb_define_method(xen_rb_cHook, "empty?", Xen_procedure_cast xen_rb_hook_is_empty_p, 0);
  rb_define_method(xen_rb_cHook, "name", Xen_procedure_cast xen_rb_hook_name, 0);
  rb_define_method(xen_rb_cHook, "arity", Xen_procedure_cast xen_rb_hook_arity, 0);
  rb_define_method(xen_rb_cHook, "describe", Xen_procedure_cast xen_rb_hook_describe, 0);
  rb_define_alias(xen_rb_cHook, "help", "describe");
  rb_define_alias(xen_rb_cHook, "documentation", "describe");
  rb_define_method(xen_rb_cHook, "inspect", Xen_procedure_cast xen_rb_hook_inspect, 0);
  
  rb_define_global_function("make_hook", Xen_procedure_cast xen_rb_make_hook, -1);
  rb_define_global_function("hook?", Xen_procedure_cast xen_rb_is_hook_p, 1);

  Xen_define_procedure(S_get_help,             g_get_help_w,             0, 1, 0, H_get_help);
  Xen_define_procedure(S_add_help,             g_add_help_w,             2, 0, 0, H_add_help);

  Xen_define_procedure(S_set_property,         g_set_property_w,         3, 0, 0, H_set_property);
  Xen_define_procedure(S_property,             g_property_w,             2, 0, 0, H_property);
  Xen_define_procedure(S_properties,           g_properties_w,           0, 0, 0, H_properties);

  Xen_define_procedure(S_gc_off,               g_gc_off_w,               0, 0, 0, H_gc_off);
  Xen_define_procedure(S_gc_on,                g_gc_on_w,                0, 0, 0, H_gc_on);
}

/* end of class Hook */

#endif



/* ------------------------------ FORTH ------------------------------ */

#if HAVE_FORTH

char *xen_version(void)
{
  return(fth_format("Fth: %s, Xen: " XEN_VERSION, FTH_VERSION));
}


void xen_gc_mark(Xen val)
{
  fth_gc_mark(val);
}


/*
 * A simple interpreter:
 *
 *  #include <xen.h>
 *  
 *  int main(int argc, char **argv)
 *  {
 *    xen_repl(argc, argv);
 *    return(0);
 *  }
 *
 * linking requires xen.o and -lfth -lm
 */

void xen_repl(int argc, char **argv)
{
  fth_repl(argc, argv);
}


static ficlWord *snd_exit_xt; 

static void fth_snd_exit(int n) 
{ 
  if (!snd_exit_xt) 
    snd_exit_xt = ficlSystemLookup(FTH_FICL_SYSTEM(), (char *)"snd-exit"); 
  ficlStackPushInteger(FTH_FICL_STACK(), n); 
  ficlVmExecuteXT(FTH_FICL_VM(), snd_exit_xt); 
  ficlStackDrop(FTH_FICL_STACK(), 1); 
} 
 

static Xen g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
  fth_gc_on();
  return(Xen_false);
}


static Xen g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
  fth_gc_on();
  return(Xen_false);
}


void xen_initialize(void)
{
  fth_init();
  fth_exit_hook = fth_snd_exit; 

  Xen_define_procedure(S_gc_off, g_gc_off, 0, 0, 0, H_gc_off);
  Xen_define_procedure(S_gc_on,  g_gc_on,  0, 0, 0, H_gc_on);
}

#endif 	/* HAVE_FORTH */



/* ------------------------------ s7 ------------------------------ */

#if HAVE_SCHEME
#include "s7.h"

#if ENABLE_WEBSERVER
  #include "s7webserver/s7webserver.h"
#endif

s7_scheme *s7;
Xen xen_false, xen_true, xen_nil, xen_undefined, xen_zero;

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
  snprintf(buf, 64, "s7: %s (%s), Xen: %s", S7_VERSION, S7_DATE, XEN_VERSION);
  return(buf);
}


static char *xen_s7_repl_prompt = NULL;

void xen_s7_set_repl_prompt(const char *new_prompt)
{
  if (xen_s7_repl_prompt) free(xen_s7_repl_prompt);
  xen_s7_repl_prompt = xen_strdup(new_prompt);
}


#if USE_SND
char *stdin_check_for_full_expression(const char *newstr);
void stdin_free_str(void);
#endif

void xen_repl(int argc, char **argv)
{
  int size = 512;
  bool expr_ok = true;
  char *buffer;
  buffer = (char *)calloc(size, sizeof(char));

  while (true)
    {
      if (expr_ok)
	{
	  fprintf(stdout, "\n%s", xen_s7_repl_prompt);
	  expr_ok = false; /* don't get into an infinite loop if running in the background! */
	}
      if (fgets(buffer, size, stdin))
	{
	  /* also, it's possible to get a string of spaces or nulls (? -- not sure what is coming in) if stdin is /dev/null */
	  /*   then if (as in condor) stdout is being saved in a file, we get in an infinite loop storing "snd>" until the disk fills up */
	  int i, len;

	  expr_ok = false;
	  len = strlen(buffer);
	  for (i = 0; i < len; i++)
	    {
	      if (buffer[i] == 0)
		break;
	      if (!isspace((int)buffer[i]))
		{
		  expr_ok = true;
		  break;
		}
	    }
	  if (expr_ok)
	    {
	      char *temp;
#if USE_SND
	      char *str;
	      str = stdin_check_for_full_expression(buffer); /* "str" here is actually stdin_str, so we need to clear it explicitly */
	      if (!str) {expr_ok = false; continue;}
	      len = strlen(str) + 16;
	      temp = (char *)malloc(len * sizeof(char));
	      snprintf(temp, len, "(write %s)", str);  
	      Xen_eval_C_string(temp);
	      free(temp);
	      stdin_free_str();
#else
	      temp = (char *)malloc(len + 16);
	      snprintf(temp, len + 16, "(write %s)", buffer);    /* use write, not display so that strings are in double quotes */
	      Xen_eval_C_string(temp);
	      free(temp);
#endif
	    }
	}
    }
  /* unreachable */
  free(buffer);
}


void xen_gc_mark(Xen val)
{
  s7_mark(val);
}


Xen xen_set_assoc(s7_scheme *sc, s7_pointer key, s7_pointer val, s7_pointer alist)
{
  /* fixup alist, return it (caller has to make sure it is reflected in its object) */
  /*
     (let ((old-val (assoc key alist)))
       (if old-val
           (progn
              (set-cdr! old-val new-val)
              alist)
	   (cons (cons key new-val) alist)))
  */
  Xen old_val;
  old_val = s7_assoc(sc, key, alist); /* returns #f if nothing found */
  if (old_val == s7_f(sc))
    return(s7_cons(sc, s7_cons(sc, key, val), alist));
  s7_set_cdr(old_val, val);
  return(alist);
}


Xen xen_assoc(s7_scheme *sc, Xen key, Xen alist)
{
  Xen val;
  val = s7_assoc(sc, key, alist);
  if (val != s7_f(sc))
    return(s7_cdr(val));
  return(s7_f(sc));
}


/* add various file functions that everyone else implements */

#ifndef _MSC_VER
  #include <unistd.h>
  #include <sys/time.h>
#endif

#include <sys/stat.h>
#include <fcntl.h>


static Xen g_getpid(void)
{
  #define H_getpid "(getpid) returns the current job's process id"
  return(C_int_to_Xen_integer((int)getpid()));
}


#if (!WITH_SYSTEM_EXTRAS)
static bool file_probe(const char *arg)
{
#ifndef _MSC_VER
  return(access(arg, F_OK) == 0);
#else
  int fd;
#ifdef O_NONBLOCK
  fd = open(arg, O_RDONLY, O_NONBLOCK);
#else
  fd = open(arg, O_RDONLY, 0);
#endif
  if (fd == -1) return(false);
  close(fd);
  return(true);
#endif
}


static Xen g_file_exists_p(Xen name)
{
  #define H_file_exists_p "(file-exists? filename): #t if the file exists"
  Xen_check_type(Xen_is_string(name), name, 1, "file-exists?", "a string");
  return(C_bool_to_Xen_boolean(file_probe(Xen_string_to_C_string(name))));
}


static bool is_directory(const char *filename)
{
#if (defined(_MSC_VER) || __CYGWIN__)
  return(false);
#else
#ifdef S_ISDIR
  struct stat statbuf;
  return((stat(filename, &statbuf) >= 0) &&
	 (S_ISDIR(statbuf.st_mode)));
  return(false);
#endif
#endif
}

static Xen g_is_directory(Xen name)
{
  #define H_is_directory "(directory? filename): #t if filename names a directory"
  Xen_check_type(Xen_is_string(name), name, 1, "directory?", "a string");
  return(C_bool_to_Xen_boolean(is_directory(Xen_string_to_C_string(name)))); /* snd-file.c l 84 */
}

static Xen g_delete_file(Xen name)
{
  #define H_delete_file "(delete-file filename): deletes the file"
  Xen_check_type(Xen_is_string(name), name, 1, "delete-file", "a string");
  return(C_bool_to_Xen_boolean(unlink(Xen_string_to_C_string(name))));
}


static Xen g_system(Xen command)
{
  #define H_system "(system command): execute command"
  Xen_check_type(Xen_is_string(command), command, 1, "system", "a string");
  return(C_int_to_Xen_integer(system(Xen_string_to_C_string(command))));
}


static Xen g_s7_getenv(Xen var) /* "g_getenv" is in use in glib! */
{
  #define H_getenv "(getenv var): return value of environment variable var"
  Xen_check_type(Xen_is_string(var), var, 1, "getenv", "a string");
  return(C_string_to_Xen_string(getenv(Xen_string_to_C_string(var))));
}
#endif



#ifdef _MSC_VER
  #include <direct.h>
#endif

static Xen g_getcwd(void)
{
  #define H_getcwd "(getcwd) returns the name of the current working directory"
  char *buf;
  Xen result = Xen_false;
  buf = (char *)calloc(1024, sizeof(char));
#ifdef _MSC_VER
  if (_getcwd(buf, 1024))
#else
  if (getcwd(buf, 1024))
#endif
    result = C_string_to_Xen_string(buf);
  free(buf);
  return(result);
}


static Xen g_strftime(Xen format, Xen tm)
{
  #define H_strftime "(strftime format time) returns a string describing the time: (strftime \"%d-%b %H:%M %Z\" (localtime (current-time)))"
  char *buf;
  Xen result;
  const struct tm *p;

  Xen_check_type(Xen_is_string(format), format, 1, "strftime", "a string");
  Xen_check_type(Xen_is_wrapped_c_pointer(tm), tm, 2, "strftime", "a localtime struct");

  p = (const struct tm *)Xen_unwrap_C_pointer(tm);
  Xen_check_type(p, tm, 2, "strftime", "a localtime struct");

  buf = (char *)calloc(1024, sizeof(char));
  strftime(buf, 1024, Xen_string_to_C_string(format), p);
  result = C_string_to_Xen_string(buf);
  free(buf);

  return(result);
}


/* (format #f ";~A~%" (strftime "%d-%b %H:%M %Z" (localtime (current-time)))) */
/* these two need to be compatible with g_file_write_date in snd-file.c */

static Xen g_localtime(Xen tm)
{
  #define H_localtime "(localtime tm) breaks up tm into something suitable for strftime"
  time_t rtime;
  Xen_check_type(Xen_is_integer(tm), tm, 1, "localtime", "an integer");
  rtime = (time_t)Xen_ulong_to_C_ulong(tm);
  return(Xen_wrap_C_pointer(localtime((time_t *)(&rtime))));
}


static Xen g_current_time(void)
{
  time_t curtime;
  #define H_current_time "(current-time) returns the current time (for localtime and strftime)"
  curtime = time(NULL);
  return(C_ulong_to_Xen_ulong(curtime));
}


static Xen g_ftell(Xen fd)
{
  Xen_check_type(Xen_is_integer(fd), fd, 1, "ftell", "an integer");
  return(C_int_to_Xen_integer(lseek(Xen_integer_to_C_int(fd), 0, SEEK_CUR)));
}


static Xen g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
  s7_gc_on(s7, false);
  return(Xen_false);
}


static Xen g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
  s7_gc_on(s7, true);
  return(Xen_false);
}



Xen_wrap_no_args(g_getpid_w, g_getpid)
#if (!WITH_SYSTEM_EXTRAS)
  Xen_wrap_1_arg(g_file_exists_p_w, g_file_exists_p)
  Xen_wrap_1_arg(g_is_directory_w, g_is_directory)
  Xen_wrap_1_arg(g_delete_file_w, g_delete_file)
  Xen_wrap_1_arg(g_s7_getenv_w, g_s7_getenv)
  Xen_wrap_1_arg(g_system_w, g_system)
#endif
Xen_wrap_no_args(g_getcwd_w, g_getcwd)
Xen_wrap_2_args(g_strftime_w, g_strftime)
Xen_wrap_1_arg(g_localtime_w, g_localtime)
Xen_wrap_no_args(g_current_time_w, g_current_time)
Xen_wrap_1_arg(g_ftell_w, g_ftell)
Xen_wrap_no_args(g_gc_off_w, g_gc_off)
Xen_wrap_no_args(g_gc_on_w, g_gc_on)

#if ENABLE_WEBSERVER
  #if USE_MOTIF
  #include "snd.h"
  static idle_func_t called_periodically(any_pointer_t pet)
  {
    s7webserver_call_very_often();
    return(BACKGROUND_CONTINUE);
  }
  #endif
#endif


s7_scheme *s7_xen_initialize(s7_scheme *sc)
{
  s7_pointer i, b, p, s;

  xen_s7_repl_prompt = xen_strdup("> ");
  if (!sc)
    {
      s7 = s7_init();
      if (!s7) 
	{
	  fprintf(stderr, "Can't initialize s7!\n");
	  return(NULL);
	}
#if ENABLE_WEBSERVER
      {
	s7webserver_t *s7webserver;
	s7webserver = s7webserver_create(s7, 6080, true);
	if (!s7webserver)
	  fprintf(stderr, "Unable to start web server. Port 6080 may be in use\n");
	else fprintf(stdout, "Started s7 webserver at port %d\n", s7webserver_get_portnumber(s7webserver));
#if USE_MOTIF
	BACKGROUND_ADD(called_periodically, NULL);
#endif
      }
#endif
    }
  else s7 = sc;

  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "pair?");
  s = s7_make_symbol(s7, "string?");

  xen_false = s7_f(s7);
  xen_true = s7_t(s7);
  xen_nil = s7_nil(s7);
  xen_undefined = s7_undefined(s7);
  xen_zero = s7_make_integer(s7, 0);
  s7_gc_protect(s7, xen_zero);

  Xen_define_typed_procedure("getpid",       g_getpid_w,        0, 0, 0, H_getpid,        s7_make_signature(s7, 1, i));
#if (!WITH_SYSTEM_EXTRAS)
  Xen_define_typed_procedure("file-exists?", g_file_exists_p_w, 1, 0, 0, H_file_exists_p, s7_make_signature(s7, 2, b, s));
  Xen_define_typed_procedure("directory?",   g_is_directory_w,  1, 0, 0, H_is_directory,  s7_make_signature(s7, 2, b, s));
  Xen_define_typed_procedure("delete-file",  g_delete_file_w,   1, 0, 0, H_delete_file,   s7_make_signature(s7, 2, b, s));
  Xen_define_typed_procedure("getenv",       g_s7_getenv_w,     1, 0, 0, H_getenv,        s7_make_signature(s7, 2, s, s));
  Xen_define_typed_procedure("system",       g_system_w,        1, 0, 0, H_system,        s7_make_signature(s7, 2, i, s));
#endif
  Xen_define_typed_procedure("getcwd",       g_getcwd_w,        0, 0, 0, H_getcwd,        s7_make_signature(s7, 1, s));
  Xen_define_typed_procedure("strftime",     g_strftime_w,      2, 0, 0, H_strftime,      s7_make_signature(s7, 3, s, s, p));
  Xen_define_typed_procedure("localtime",    g_localtime_w,     1, 0, 0, H_localtime,     s7_make_signature(s7, 2, p, i));
  Xen_define_typed_procedure("current-time", g_current_time_w,  0, 0, 0, H_current_time,  s7_make_signature(s7, 1, i));
  Xen_define_typed_procedure("ftell",        g_ftell_w,         1, 0, 0, "(ftell fd): lseek", s7_make_signature(s7, 2, i, i));
  Xen_define_typed_procedure(S_gc_off,       g_gc_off_w,        0, 0, 0, H_gc_off,        s7_make_signature(s7, 1, b));
  Xen_define_typed_procedure(S_gc_on,        g_gc_on_w,         0, 0, 0, H_gc_on,         s7_make_signature(s7, 1, b));

  Xen_eval_C_string("(define (hook-push hook func) \n\
                       \"(hook-push hook func) adds func to hook's function list\" \n\
                       (if (not (member func (hook-functions hook) eq?)) (set! (hook-functions hook) (cons func (hook-functions hook)))))");
  Xen_eval_C_string("(define (hook-append hook func) \n\
                       \"(hook-append hook func) adds func to the end of hook's function list\" \n\
                       (set! (hook-functions hook) (append (hook-functions hook) (list func))))");
  Xen_eval_C_string("(define (hook-clear hook) (set! (hook-functions hook) ()))");
  Xen_eval_C_string("(define (hook-remove hook func) \n\
                       (set! (hook-functions hook)\n\
	                     (let loop ((l (hook-functions hook))\n\
		                        (result ()))\n\
	                       (cond ((null? l) (reverse! result))\n\
		                     ((eq? func (car l)) (loop (cdr l) result))\n\
		                     (else (loop (cdr l) (cons (car l) result)))))))");

  Xen_eval_C_string("(define-macro (while whether . body) `(do () ((not ,whether)) ,@body))");
  Xen_eval_C_string("(define (identity x) \"return arg\" x)");

  return(s7);
}


void xen_initialize(void)
{
  s7_xen_initialize(NULL);
}
#endif




/* ------------------------------ NONE OF THE ABOVE ------------------------------ */

#if (!HAVE_EXTENSION_LANGUAGE)

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
  snprintf(buf, 64, "no extension language");
  return(buf);
}


void xen_repl(int argc, char **argv)
{
}


void xen_initialize(void)
{
}


void xen_gc_mark(Xen val)
{
}


void xen_no_ext_lang_check_args(const char *name, int args, int req_args, int opt_args, int rst_args)
{
  if (args > 0) /* nargify -- all are required */
    {
      if (req_args != args)
	fprintf(stderr, "%s: %d required args, but req: %d (opt: %d, rst: %d)\n", name, args, req_args, opt_args, rst_args);
      if (opt_args != 0)
	fprintf(stderr, "%s: all args required, but opt: %d (rst: %d)\n", name, opt_args, rst_args);
      if (rst_args != 0)
	fprintf(stderr, "%s: all args required, but rst: %d\n", name, rst_args);
    }
  else
    {
      if (args != -100) /* vargify -- any ok */
	{
	  args = -args;
	  if (rst_args == 0)
	    {
	      if (req_args + opt_args != args)
		fprintf(stderr, "%s: total args: %d, but req: %d and opt: %d\n", name, args, req_args, opt_args);
	    }
	  else
	    {
	      if (req_args + opt_args > args)
		fprintf(stderr, "%s: has :rest, but req: %d and opt: %d , whereas total: %d\n", name, req_args, opt_args, args);
	    }
	}
    }
}

#endif
