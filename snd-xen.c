#if (defined(__GNUC__))
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <string.h>
#endif

#include "snd.h"
#include "clm2xen.h"

#define HAVE_SPECIAL_FUNCTIONS (!_MSC_VER)

/* Snd defines its own exit and delay
 *
 *   In Ruby, rand is kernel_rand.
 *
 *   In Forth, Snd's exit is named snd-exit.
 */
/* error handlers */

static const char *io_error_names[IO_ERROR_NUM] = {"no error", "save-hook cancellation", "bad channel",
						   "can't reopen file", "too many open files", "unknown sndlib error", 
						   "no memory", "can't open", "no filename", "bad sample type", "bad header type", "sndlib uninitialized", 
						   "not a sound file", "file closed", "write error", "interrupted", "can't close", 
						   "bad header", "disk full", "write protected", "can't read selection file",
						   "need write confirmation", "no changes", "io edit-hook cancellation", "can't create file"
};

const char *io_error_name(io_error_t err)
{
  if (err < IO_ERROR_NUM)
    return(io_error_names[(int)err]);
  return(mus_format("unknown io_error: %d", err));
}


/* this is needed as a C int below */
#ifndef USE_NO_GUI
  #define USE_NO_GUI 0
#endif


static bool run_snd_error_hook(const char *msg)
{
  return((Xen_hook_has_list(ss->snd_error_hook)) &&
	 (Xen_is_true(run_or_hook(ss->snd_error_hook, 
				 Xen_list_1(C_string_to_Xen_string(msg)),
				 S_snd_error_hook))));
}


void redirect_snd_warning_to(void (*handler)(const char *warning_msg, void *ufd), void *data)
{
  ss->snd_warning_handler = handler;
  ss->snd_warning_data = data;
}


void redirect_snd_error_to(void (*handler)(const char *error_msg, void *ufd), void *data)
{
  ss->snd_error_handler = handler;
  ss->snd_error_data = data;
}


static void snd_error_1(const char *msg, bool with_redirection_and_hook)
{
  if (with_redirection_and_hook)
    {
      if (ss->snd_error_handler)
	{
	  /* make sure it doesn't call itself recursively */
	  void (*old_snd_error_handler)(const char *error_msg, void *data);
	  void *old_snd_error_data;
	  old_snd_error_handler = ss->snd_error_handler;
	  old_snd_error_data = ss->snd_error_data;
	  ss->snd_error_handler = NULL;
	  ss->snd_error_data = NULL;
	  (*(old_snd_error_handler))(msg, old_snd_error_data);
	  ss->snd_error_handler = old_snd_error_handler;
	  ss->snd_error_data = old_snd_error_data;
	  return;
	}
      
      if (run_snd_error_hook(msg))
	return;
    }
#if (USE_NO_GUI)
  fprintf(stderr, "%s", msg);
#else
  if (ss)
    {
      if (ss->batch_mode)
	fprintf(stderr, "%s", msg);
#if ((!HAVE_EXTENSION_LANGUAGE) && (!USE_NO_GUI))
      {
	snd_info *sp;
	sp = any_selected_sound();
	if ((sp) && (sp->active))
	  status_report(sp, "%s", msg);
	else post_it("Error", msg);
      }
#endif
    }
  else 
    {
      fprintf(stderr, "%s", msg);
      fputc('\n', stderr);
    }
#endif
  /* end USE_NO_GUI */
}


static void snd_warning_1(const char *msg)
{
  if (ss->snd_warning_handler)
    {
      /* make sure it doesn't call itself recursively */
      void (*old_snd_warning_handler)(const char *msg, void *data);
      void *old_snd_warning_data;
      old_snd_warning_handler = ss->snd_warning_handler;
      old_snd_warning_data = ss->snd_warning_data;
      ss->snd_warning_handler = NULL;
      ss->snd_warning_data = NULL;
      (*(old_snd_warning_handler))(msg, old_snd_warning_data);
      ss->snd_warning_handler = old_snd_warning_handler;
      ss->snd_warning_data = old_snd_warning_data;
      return;
    }

  if ((Xen_hook_has_list(ss->snd_warning_hook)) &&
      (Xen_is_true(run_or_hook(ss->snd_warning_hook, 
			      Xen_list_1(C_string_to_Xen_string(msg)),
			      S_snd_warning_hook))))
    return;

  if ((ss) && (!(ss->batch_mode)) && (ss->max_sounds > 0))
    {
      snd_info *sp;
      sp = any_selected_sound();
      if ((sp) && (sp->active))
	status_report(sp, "%s", msg); /* make the Mac C compiler happy */
      else 
	{
	  listener_append(msg);
	  fprintf(stderr, "%s", msg); 
	}
    }
  else fprintf(stderr, "%s", msg);
}


static int snd_error_buffer_size = 1024;
static char *snd_error_buffer = NULL;

void snd_warning(const char *format, ...)
{
  int bytes_needed = 0;
  va_list ap;

  if (!snd_error_buffer) 
    snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));
  va_start(ap, format);

  /* can't use vasprintf here -- we may jump anywhere leaving unclaimed memory behind 
   */
  bytes_needed = vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
  va_end(ap);

  if (bytes_needed >= snd_error_buffer_size)
    {
      snd_error_buffer_size = bytes_needed * 2;
      free(snd_error_buffer);
      snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));

      va_start(ap, format);
      vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
      va_end(ap);
    }
  snd_warning_1(snd_error_buffer);
}


void snd_warning_without_format(const char *msg)
{
  snd_warning_1(msg);
}


void snd_error(const char *format, ...)
{
  int bytes_needed = 0;
  va_list ap;
  if (!snd_error_buffer) 
    snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));

  va_start(ap, format);
  bytes_needed = vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
  va_end(ap);

  if (bytes_needed > snd_error_buffer_size)
    {
      snd_error_buffer_size = bytes_needed * 2;
      free(snd_error_buffer);
      snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));

      va_start(ap, format);
      vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
      va_end(ap);
    }
  snd_error_1(snd_error_buffer, true);
}


void snd_error_without_format(const char *msg)
{
  snd_error_1(msg, true);
}


static Xen g_snd_error(Xen msg)
{
  /* this throws a 'snd-error error; it does not call snd_error_1 or friends above */
  #define H_snd_error "(" S_snd_error " str): throws a 'snd-error error"
  Xen_check_type(Xen_is_string(msg), msg, 1, S_snd_error, "a string");

  if (!(run_snd_error_hook(Xen_string_to_C_string(msg)))) /* have to call this before the throw, else we end up at top level */
    Xen_error(Xen_make_error_type("snd-error"),
	      Xen_list_2(C_string_to_Xen_string(S_snd_error ": ~A"),
			 msg));
  return(msg);
}

  
static Xen g_snd_warning(Xen msg)
{
  #define H_snd_warning "(" S_snd_warning " str): reports warning message str (normally in the status area)"
  Xen_check_type(Xen_is_string(msg), msg, 1, S_snd_warning, "a string");
  snd_warning("%s", Xen_string_to_C_string(msg));
  return(msg);
}


static Xen clip_hook;

static mus_float_t run_clip_hook(mus_float_t val)
{
  if (Xen_hook_has_list(clip_hook))
    {
      Xen result;
      result = run_progn_hook(clip_hook,
			      Xen_list_1(C_double_to_Xen_real(val)),
			      S_clip_hook);
      if (Xen_is_number(result))
	return(Xen_real_to_C_double(result));
    }
  /* otherwise mimic the built-in default in io.c */
  if (val >= 0.99999)
    return(0.99999);
  return(-1.0);
}

static bool clip_hook_checker(void)
{
  bool result;
  result = Xen_hook_has_list(clip_hook);
  if (result)
    mus_clip_set_handler(run_clip_hook);
  else mus_clip_set_handler(NULL);
  return(result);
}


 

/* -------- protect Xen vars from GC -------- */

#if HAVE_SCHEME

int snd_protect(Xen obj) {return(s7_gc_protect(s7, obj));}
void snd_unprotect_at(int loc) {s7_gc_unprotect_at(s7, loc);}

#else
static Xen gc_protection;
static int gc_protection_size = 0;
#define DEFAULT_GC_VALUE Xen_undefined
static int gc_last_cleared = NOT_A_GC_LOC;
static int gc_last_set = NOT_A_GC_LOC;

int snd_protect(Xen obj)
{
  int i, old_size;
  Xen tmp;
  
  if (gc_protection_size == 0)
    {
      gc_protection_size = 512;
      gc_protection = Xen_make_vector(gc_protection_size, DEFAULT_GC_VALUE);
      Xen_GC_protect(gc_protection);
      Xen_vector_set(gc_protection, 0, obj);
      gc_last_set = 0;
    }
  else
    {
      if ((gc_last_cleared >= 0) && 
	  Xen_is_eq(Xen_vector_ref(gc_protection, gc_last_cleared), DEFAULT_GC_VALUE))
	{
	  /* we hit this branch about 2/3 of the time */
	  Xen_vector_set(gc_protection, gc_last_cleared, obj);
	  gc_last_set = gc_last_cleared;
	  gc_last_cleared = NOT_A_GC_LOC;

	  return(gc_last_set);
	}

      for (i = gc_last_set; i < gc_protection_size; i++)
	if (Xen_is_eq(Xen_vector_ref(gc_protection, i), DEFAULT_GC_VALUE))
	  {
	    Xen_vector_set(gc_protection, i, obj);
	    gc_last_set = i;
	    
	    return(gc_last_set);
	  }

      for (i = 0; i < gc_last_set; i++)
	if (Xen_is_eq(Xen_vector_ref(gc_protection, i), DEFAULT_GC_VALUE))
	  {
	    /* here we average 3 checks before a hit, so this isn't as bad as it looks */
	    Xen_vector_set(gc_protection, i, obj);
	    gc_last_set = i;

	    return(gc_last_set);
	  }

      tmp = gc_protection;
      old_size = gc_protection_size;
      gc_protection_size *= 2;
      gc_protection = Xen_make_vector(gc_protection_size, DEFAULT_GC_VALUE);
      Xen_GC_protect(gc_protection);

      for (i = 0; i < old_size; i++)
	{
	  Xen_vector_set(gc_protection, i, Xen_vector_ref(tmp, i));
	  Xen_vector_set(tmp, i, DEFAULT_GC_VALUE);
	}

      Xen_vector_set(gc_protection, old_size, obj);

      /*   in Ruby, I think we can unprotect it */
#if HAVE_RUBY || HAVE_FORTH
      Xen_GC_unprotect(tmp);
#endif
      gc_last_set = old_size;
    }
  return(gc_last_set);
}


void snd_unprotect_at(int loc)
{
  if (loc >= 0)
    {
      Xen_vector_set(gc_protection, loc, DEFAULT_GC_VALUE);
      gc_last_cleared = loc;
    }
}
#endif


/* -------- error handling -------- */

static char *last_file_loaded = NULL;

#if HAVE_SCHEME
static Xen g_snd_s7_error_handler(Xen args)
{
  s7_pointer msg;
  if (s7_is_pair(args))
    msg = s7_car(args);
  else msg = args;
  Xen_check_type(Xen_is_string(msg), msg, 1, "_snd_s7_error_handler_", "a string");

  if (ss->xen_error_handler)
    (*(ss->xen_error_handler))(s7_string(msg), (void *)any_selected_sound()); /* not NULL! */
  return(s7_f(s7));
}
#endif


void redirect_xen_error_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  ss->xen_error_handler = handler;
  ss->xen_error_data = data;

#if HAVE_SCHEME
  if (!handler)
    s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) ())");
  else s7_eval_c_string(s7, "(set! (hook-functions *error-hook*) (list  \n\
                               (lambda (hook)                           \n\
                                 (let ((args (hook 'data)))             \n\
                                 (_snd_s7_error_handler_                \n\
                                   (string-append                       \n\
                                     (if (string? args)                 \n\
                                         args                           \n\
                                         (if (pair? args)               \n\
                                             (apply format #f args)     \n\
                                             \"\"))                     \n\
                                     (with-let (owlet)                  \n\
                                       (if (and error-code              \n\
                                                (string? error-file)    \n\
                                                (number? error-line))   \n\
                                           (format #f \"~%~S[~D]: ~A~%\" error-file error-line error-code) \n\
                                           \"\"))))))))");
#endif
}


static void redirect_snd_print_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  ss->snd_print_handler = handler;
  ss->snd_print_data = data;
}


void redirect_everything_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  redirect_snd_error_to(handler, data);
  redirect_xen_error_to(handler, data);
  redirect_snd_warning_to(handler, data);
  redirect_snd_print_to(handler, data);
}


void redirect_errors_to(void (*handler)(const char *msg, void *ufd), void *data)
{
  redirect_snd_error_to(handler, data);
  redirect_xen_error_to(handler, data);
  redirect_snd_warning_to(handler, data);
}


static char *gl_print(Xen result);



/* ---------------- RUBY error handler ---------------- */ 

#if HAVE_RUBY 
static Xen snd_format_if_needed(Xen args) 
{ 
  /* if car has formatting info, use next arg as arg list for it */ 
  Xen format_args = Xen_empty_list, cur_arg, result; 
  int i, start = 0, num_args, format_info_len, err_size = 8192; 
  bool got_tilde = false, was_formatted = false; 
  char *format_info = NULL, *errmsg = NULL; 

  num_args = Xen_list_length(args); 
  if (num_args == 1) return(Xen_car(args)); 

  format_info = mus_strdup(Xen_string_to_C_string(Xen_car(args))); 
  format_info_len = mus_strlen(format_info); 

  if (Xen_is_cons(Xen_cadr(args))) 
    format_args = Xen_copy_arg(Xen_cadr(args)); /* protect Ruby case */ 
  else format_args = Xen_cdr(args); 

  errmsg = (char *)calloc(err_size, sizeof(char)); 

  for (i = 0; i < format_info_len; i++) 
    { 
      if (format_info[i] == '~') 
    { 
      strncat(errmsg, (char *)(format_info + start), i - start); 
      start = i + 2; 
      got_tilde = true; 
    } 
      else 
    { 
      if (got_tilde) 
        { 
          was_formatted = true; 
          got_tilde = false; 
          switch (format_info[i]) 
         { 
         case '~': errmsg = mus_strcat(errmsg, "~", &err_size); break; 
         case '%': errmsg = mus_strcat(errmsg, "\n", &err_size); break; 
         case 'S': 
         case 'A': 
           if (!Xen_is_null(format_args)) 
             { 
               cur_arg = Xen_car(format_args); 
               format_args = Xen_cdr(format_args); 
               if (Xen_is_vector(cur_arg)) 
              { 
                char *vstr; 
                vstr = gl_print(cur_arg); 
                errmsg = mus_strcat(errmsg, vstr, &err_size); 
                free(vstr); 
              } 
               else 
              { 
                char *temp = NULL; 
                errmsg = mus_strcat(errmsg, temp = (char *)Xen_object_to_C_string(cur_arg), &err_size); 
              } 
             } 
           /* else ignore it */ 
           break; 
         default: start = i - 1; break; 
         } 
        } 
    } 
    } 
  if (i > start) 
    strncat(errmsg, (char *)(format_info + start), i - start); 
  if (format_info) free(format_info); 
  if (!was_formatted) 
    { 
      char *temp = NULL; 
      errmsg = mus_strcat(errmsg, " ", &err_size); 
      errmsg = mus_strcat(errmsg, temp = (char *)Xen_object_to_C_string(Xen_cadr(args)), &err_size); 

      if (num_args > 2) 
    { 
      if (!Xen_is_false(Xen_caddr(args))) start = 2; else start = 3; 
      for (i = start; i < num_args; i++) 
        { 
          char *temp = NULL; 
          errmsg = mus_strcat(errmsg, " ", &err_size); 
          errmsg = mus_strcat(errmsg, temp = (char *)Xen_object_to_C_string(Xen_list_ref(args, i)), &err_size); 
        } 
    } 
    } 
  result = C_string_to_Xen_string(errmsg); 
  free(errmsg); 
  return(result); 
} 

void snd_rb_raise(Xen tag, Xen throw_args) 
{ 
  static char *msg = NULL; 
  Xen err = rb_eStandardError, bt; 
  int size = 2048; 
  char *idname; 

  if (msg) free(msg); 
  msg = (char *)calloc(size, sizeof(char)); 

  idname = (char *)rb_id2name(tag); 
  if (strcmp(idname, "Out_of_range") == 0) 
    err = rb_eRangeError; 
  else 
    if (strcmp(idname, "Wrong_type_arg") == 0) 
      err = rb_eTypeError; 

  msg = mus_strcat(msg, idname, &size); 
  if (strcmp(idname, "Mus_error") == 0) 
    msg = mus_strcat(msg, ": ", &size); 
  else msg = mus_strcat(msg, " in ", &size); 
  msg = mus_strcat(msg, Xen_string_to_C_string(snd_format_if_needed(throw_args)), &size); 

  bt = rb_funcall(err, rb_intern("caller"), 0); 

  if (Xen_vector_length(bt) > 0) 
    { 
      int i; 
      msg = mus_strcat(msg, "\n", &size); 
      for (i = 0; i < Xen_vector_length(bt); i++) 
    { 
      msg = mus_strcat(msg, Xen_string_to_C_string(Xen_vector_ref(bt, i)), &size); 
      msg = mus_strcat(msg, "\n", &size); 
    } 
    } 

  if (strcmp(idname, "Snd_error") != 0) 
    { 
      if (!(run_snd_error_hook(msg))) 
    { 
      if (ss->xen_error_handler) 
        { 
          /* make sure it doesn't call itself recursively */ 
          void (*old_xen_error_handler)(const char *msg, void *data); 
          void *old_xen_error_data; 
          old_xen_error_handler = ss->xen_error_handler; 
          old_xen_error_data = ss->xen_error_data; 
          ss->xen_error_handler = NULL; 
          ss->xen_error_data = NULL; 
          (*(old_xen_error_handler))(msg, old_xen_error_data); 
          ss->xen_error_handler = old_xen_error_handler; 
          ss->xen_error_data = old_xen_error_data; 
        } 
    } 
    } 

  rb_raise(err, "%s", msg); 
} 
#endif 
/* end HAVE_RUBY */ 



#if HAVE_EXTENSION_LANGUAGE

Xen snd_catch_any(Xen_catch_t body, void *body_data, const char *caller)
{
  return((*body)(body_data));
}

#else

/* no extension language but user managed to try to evaluate something
 *   can this happen?
 */
Xen snd_catch_any(Xen_catch_t body, void *body_data, const char *caller)
{
  snd_error("This version of Snd has no extension language, so there's no way for %s to evaluate anything", caller);
  return(Xen_false);
}
#endif


bool procedure_arity_ok(Xen proc, int args)
{
#if HAVE_SCHEME
  return(s7_is_aritable(s7, proc, args));
#else
  Xen arity;
  int rargs;
  arity = Xen_arity(proc);
  rargs = Xen_integer_to_C_int(arity);

#if HAVE_RUBY
  return(xen_rb_arity_ok(rargs, args));
#endif

#if HAVE_FORTH
  return(rargs == args);
#endif
#endif
  return(true);
}


char *procedure_ok(Xen proc, int args, const char *caller, const char *arg_name, int argn)
{
  /* if string returned, needs to be freed */

  if (!(Xen_is_procedure(proc)))
    {
      if (!Xen_is_false(proc)) /* #f as explicit arg to clear */
	return(mus_format(" %s is not a procedure!", (arg_name) ? arg_name : caller));
    }
  else
    {
      int rargs;
      Xen arity;
      arity = Xen_arity(proc);

#if HAVE_RUBY
      rargs = Xen_integer_to_C_int(arity);
      if (!xen_rb_arity_ok(rargs, args))
 	return(mus_format("  %s function should take %d args, not %d", (arg_name) ? arg_name : caller, args, (rargs < 0) ? (-rargs) : rargs));
#endif

#if HAVE_SCHEME
      {
	int oargs, loc;

	loc = snd_protect(arity);
	rargs = Xen_integer_to_C_int(Xen_car(arity));
	oargs = Xen_integer_to_C_int(Xen_cdr(arity));
	snd_unprotect_at(loc);

	if (rargs > args)
	  return(mus_format(" %s function should take %d argument%s, but instead requires %d",
			    (arg_name) ? arg_name : caller, args, (args != 1) ? "s" : "", rargs));

	if ((rargs + oargs) < args)
	  return(mus_format(" %s function should accept at least %d argument%s, but instead accepts only %d",
			    (arg_name) ? arg_name : caller, args, (args != 1) ? "s" : "", rargs + oargs));
      }
#endif

#if HAVE_FORTH
      rargs = Xen_integer_to_C_int(arity);
      if (rargs != args)
	return(mus_format(" %s function should take %d args, not %d", (arg_name) ? arg_name : caller, args, rargs));
#endif
    }
  return(NULL);
}


Xen snd_no_such_file_error(const char *caller, Xen filename)
{
  Xen_error(NO_SUCH_FILE,
	    Xen_list_4(C_string_to_Xen_string("no-such-file: ~A ~S: ~A"),
		       C_string_to_Xen_string(caller),
		       filename,
		       C_string_to_Xen_string(snd_open_strerror())));
  return(Xen_false);
}


Xen snd_no_such_channel_error(const char *caller, Xen snd, Xen chn)
{
  int index = NOT_A_SOUND;

  if (Xen_is_integer(snd))
    index = Xen_integer_to_C_int(snd);
  else
    {
      if (xen_is_sound(snd))
	index = Xen_sound_to_C_int(snd);
    }

  if ((index >= 0) &&
      (index < ss->max_sounds) && 
      (snd_ok(ss->sounds[index]))) /* good grief... */
    {
      snd_info *sp;
      sp = ss->sounds[index];
      Xen_error(NO_SUCH_CHANNEL,
		Xen_list_6(C_string_to_Xen_string("no-such-channel: (~A: sound: ~A, chan: ~A) (~S, chans: ~A))"),
			   C_string_to_Xen_string(caller),
			   snd, 
			   chn, 
			   C_string_to_Xen_string(sp->short_filename), 
			   C_int_to_Xen_integer(sp->nchans)));
    }
  Xen_error(NO_SUCH_CHANNEL,
	    Xen_list_4(C_string_to_Xen_string("no-such-channel: (~A: sound: ~A, chan: ~A)"),
		       C_string_to_Xen_string(caller),
		       snd,
		       chn));
  return(Xen_false);
}


Xen snd_no_active_selection_error(const char *caller)
{
  Xen_error(Xen_make_error_type("no-active-selection"),
	    Xen_list_2(C_string_to_Xen_string("~A: no active selection"),
		       C_string_to_Xen_string(caller)));
  return(Xen_false);
}


Xen snd_bad_arity_error(const char *caller, Xen errstr, Xen proc)
{
  Xen_error(Xen_make_error_type("bad-arity"),
            Xen_list_3(C_string_to_Xen_string("~A,~A"),
		       C_string_to_Xen_string(caller),
                       errstr));
  return(Xen_false);
}



/* -------- various evaluators (within our error handler) -------- */

Xen eval_str_wrapper(void *data)
{
  return(Xen_eval_C_string((char *)data));
}


static Xen eval_file_wrapper(void *data)
{
  last_file_loaded = (char *)data;
  Xen_load((char *)data);
  last_file_loaded = NULL;
  return(Xen_true);
}


static char *g_print_1(Xen obj) /* free return val */
{
#if HAVE_SCHEME
  return(Xen_object_to_C_string(obj)); 
#endif

#if HAVE_FORTH || HAVE_RUBY
  return(mus_strdup(Xen_object_to_C_string(obj))); 
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
  return(NULL);
#endif
}


static char *gl_print(Xen result)
{
  char *newbuf, *str = NULL;
  int i, ilen, savelen;

#if HAVE_SCHEME
  /* expand \t first (neither gtk nor motif handles this automatically)
   *   but... "#\\t" is the character t not a tab indication!
   *   (object->string #\t) or worse #\tab
   */
  #define TAB_SPACES 4
  int tabs = 0, len, j = 0;

  newbuf = g_print_1(result);
  len = mus_strlen(newbuf);

  for (i = 0; i < len - 1; i++)
    if (((i == 0) || ((newbuf[i - 1] != '\\') && (newbuf[i - 1] != '#'))) &&
	(newbuf[i] == '\\') && 
	(newbuf[i + 1] == 't'))
      tabs++;

  if (tabs == 0)
    return(newbuf);

  ilen = len + tabs * TAB_SPACES;
  str = (char *)calloc(ilen, sizeof(char));

  for (i = 0; i < len - 1; i++)
    {
      if (((i == 0) || (newbuf[i - 1] != '\\')) && 
	  (newbuf[i] == '\\') && 
	  (newbuf[i + 1] == 't'))
	{
	  int k;
	  for (k = 0; k < TAB_SPACES; k++)
	    str[j + k] = ' ';
	  j += TAB_SPACES;
	  i++;
	}
      else str[j++] = newbuf[i];
    }
  str[j] = newbuf[len - 1];

  free(newbuf);
  return(str);
#endif

  /* specialize vectors which can be enormous in this context */
  if ((!(Xen_is_vector(result))) || 
      ((int)(Xen_vector_length(result)) <= print_length(ss)))
    return(g_print_1(result));

  ilen = print_length(ss); 
  newbuf = (char *)calloc(128, sizeof(char));
  savelen = 128;

#if HAVE_FORTH
  snprintf(newbuf, 128, "#("); 
#endif

#if HAVE_RUBY
  snprintf(newbuf, 128, "[");
#endif

  for (i = 0; i < ilen; i++)
    {
      str = g_print_1(Xen_vector_ref(result, i));
      if ((str) && (*str)) 
	{
	  if (i != 0) 
	    {
#if HAVE_RUBY
	      newbuf = mus_strcat(newbuf, ",", &savelen);
#endif
	      newbuf = mus_strcat(newbuf, " ", &savelen); 
	    }
	  newbuf = mus_strcat(newbuf, str, &savelen);
	  free(str);
	}
    }

#if HAVE_FORTH
  newbuf = mus_strcat(newbuf, " ...)", &savelen);
#endif

#if HAVE_RUBY
  newbuf = mus_strcat(newbuf, " ...]", &savelen);
#endif

  return(newbuf);
}


void snd_display_result(const char *str, const char *endstr)
{
  if (ss->snd_print_handler)
    {
      /* make sure it doesn't call itself recursively */
      void (*old_snd_print_handler)(const char *msg, void *data);
      void *old_snd_print_data;
      old_snd_print_handler = ss->snd_print_handler;
      old_snd_print_data = ss->snd_print_data;
      ss->snd_print_handler = NULL;
      ss->snd_print_data = NULL;
      (*(old_snd_print_handler))(str, old_snd_print_data);
      ss->snd_print_handler = old_snd_print_handler;
      ss->snd_print_data = old_snd_print_data;
    }
  else
    {
      if (endstr) listener_append(endstr);
      listener_append_and_prompt(str);
    }
}


void snd_report_result(Xen result, const char *buf)
{
  char *str;
  str = gl_print(result);
  snd_display_result(str, buf);
  if (str) free(str);
}


void snd_report_listener_result(Xen form)
{
  snd_report_result(form, "\n");
}


static char *stdin_str = NULL;

void clear_stdin(void)
{
  if (stdin_str) free(stdin_str);
  stdin_str = NULL;
}


#if HAVE_SCHEME
static int check_balance(const char *expr, int start, int end) 
{
  int i;
  bool not_whitespace = false;
  int paren_count = 0;
  bool prev_separator = true;
  bool quote_wait = false;

  i = start;
  while (i < end) 
    {
      switch (expr[i]) 
	{
	case ';' :
	  /* skip till newline. */
	  do {
	    i++;
	  } while ((i < end) && (expr[i] != '\n'));
	  break;

	case ' ':
	case '\n':
	case '\t':
	case '\r':
	  if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      prev_separator = true;
	      i++;
	    }
	  break;

	case '\"' :
	  if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      /* skip past ", ignoring \", some cases:
	       *  "\"\"" '("\"\"") "\\" "#\\(" "'(\"#\\\")"
	       */
	      while (i < end)
		{
		  i++;
		  if (expr[i] == '\\') 
		    i++;
		  else
		    {
		      if (expr[i] == '\"')
			break;
		    }
		}
	      i++;
	      if (paren_count == 0) 
		{
		  if (i < end) 
		    return(i);
		  else return(0);
		} 
	      else 
		{
		  prev_separator = true;
		  not_whitespace = true;
		  quote_wait = false;
		}
	    }
	  break;

	case '#':
	  if ((i < end - 1) &&
	      (expr[i + 1] == '|'))
	    {
	      /* (+ #| a comment |# 2 1) */
	      i++;
	      do {
		i++;
	      } while (((expr[i] != '|') || (expr[i + 1] != '#')) && (i < end));
	      i++;
	      break;
	    }
	  else
	    {
	      /* (set! *#readers* (cons (cons #\c (lambda (str) (apply make-rectangular (read)))) *#readers*))
	       */
	      if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
		return(i);
	      else 
		{
		  bool found_it = false;
		  if (prev_separator)
		    {
		      int k, incr = 0;
		      for (k = i + 1; k < end; k++)
			{
			  if (expr[k] == '(')
			    {
			      /* should we look at the readers here? I want to support #c(1 2) for example */
			      not_whitespace = false;
			      prev_separator = false;
			      incr = k - i;
			      break;
			    }
			  else
			    {
			      if ((!isdigit((int)expr[k])) && /* #2d(...)? */
				  (!isalpha((int)expr[k])) && /* #c(1 2)? */
				  (expr[k] != 'D') && 
				  (expr[k] != 'd') &&
				  (expr[k] != '=') &&   /* what is this for? */
				  (expr[k] != '#'))     /* perhaps #1d(#(1 2) 3) ? */
				break;
			    }
			}
		      if (incr > 0)
			{
			  i += incr;
			  found_it = true;
			}
		    }
		  if (!found_it)
		    {
		      if ((i + 2 < end) && (expr[i + 1] == '\\') && 
			  ((expr[i + 2] == ')') || (expr[i + 2] == ';') || (expr[i + 2] == '\"') || (expr[i + 2] == '(')))
			i += 3;
		      else
			{
			  prev_separator = false;
			  quote_wait = false;
			  not_whitespace = true;
			  i++;
			}
		    }
		}
	    }
	  break;

	case '(' :
	  if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
	    return(i - 1); /* 'a(...) -- ignore the (...) */
	  else 
	    {
	      i++;
	      paren_count++;
	      not_whitespace = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;

	case ')' :
	  paren_count--;
	  if ((not_whitespace) && (paren_count == 0))
	    return(i + 1);
	  else 
	    {
	      i++;
	      not_whitespace = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;

	case '\'' :
	case '`' :                  /* `(1 2) */
	  if (prev_separator) 
	    quote_wait = true;
	  not_whitespace = true;
	  i++;
	  break;

	case ',':                   /* `,(+ 1 2) */
	case '@':                   /* `,@(list 1 2) */
	  prev_separator = false;
	  not_whitespace = true;
	  i++;
	  break;

	default:
	  prev_separator = false;
	  quote_wait = false;
	  not_whitespace = true;
	  i++;
	  break;
	}
    }

  return(0);
}
#endif


char *stdin_check_for_full_expression(const char *newstr)
{
#if HAVE_SCHEME
  int end_of_text;
#endif
  if (stdin_str)
    {
      char *str;
      str = stdin_str;
      stdin_str = (char *)calloc(mus_strlen(str) + mus_strlen(newstr) + 2, sizeof(char));
      strcat(stdin_str, str);
      strcat(stdin_str, newstr);
      free(str);
    }
  else stdin_str = mus_strdup(newstr);
#if HAVE_SCHEME
  end_of_text = check_balance(stdin_str, 0, mus_strlen(stdin_str));
  if (end_of_text > 0)
    {
      if (end_of_text + 1 < mus_strlen(stdin_str))
	stdin_str[end_of_text + 1] = 0;
      return(stdin_str);
    }
  return(NULL);
#endif
  return(stdin_str);
}

void stdin_free_str(void)
{
  if (stdin_str) free(stdin_str);
  stdin_str = NULL;
}


static void string_to_stdout(const char *msg, void *ignored)
{
  fprintf(stdout, "%s\n", msg);
}


void snd_eval_stdin_str(const char *buf)
{
  /* we may get incomplete expressions here */
  /*   (Ilisp always sends a complete expression, but it may be broken into two or more pieces from read's point of view) */

  char *str = NULL;
  if (mus_strlen(buf) == 0) return;

  str = stdin_check_for_full_expression(buf);
  if (str)
    {
      Xen result;
      int loc;

      redirect_everything_to(string_to_stdout, NULL);
      result = snd_catch_any(eval_str_wrapper, (void *)str, str);
      redirect_everything_to(NULL, NULL);

      loc = snd_protect(result);
      stdin_free_str();

      str = gl_print(result);
      string_to_stdout(str, NULL);

      if (mus_strlen(stdin_prompt(ss)) > 0)
	{
	  fprintf(stdout, "%s", stdin_prompt(ss));
	  fflush(stdout);
	}

      if (str) free(str);
      snd_unprotect_at(loc);
    }
}


static void string_to_stderr_and_listener(const char *msg, void *ignore)
{
  fprintf(stderr, "%s\n", msg);
  if (listener_exists()) /* the idea here is to save startup errors until we can post them */
    {
      listener_append((char *)msg);
      listener_append("\n");
    }
  else 
    {
      if (ss->startup_errors)
	{
	  char *temp;
	  temp = ss->startup_errors;
	  ss->startup_errors = mus_format("%s\n%s %s\n", ss->startup_errors, listener_prompt(ss), msg);
	  free(temp);
	}
      else ss->startup_errors = mus_strdup(msg); /* initial prompt is already there */
    }
}


static bool snd_load_init_file_1(const char *filename)
{
  char *fullname;
  bool happy = false;
  fullname = mus_expand_filename(filename);
  if (mus_file_probe(fullname))
    {
      char *expr;
      happy = true;
#if HAVE_SCHEME
      expr = mus_format("(load %s)", fullname);
#endif

#if HAVE_RUBY || HAVE_FORTH
      expr = mus_format("load(%s)", fullname);
#endif
      snd_catch_any(eval_file_wrapper, (void *)fullname, expr);
      free(expr);
    }

  if (fullname) free(fullname);
  return(happy);
}


void snd_load_init_file(bool no_global, bool no_init)
{
  /* look for ".snd" on the home directory; return true if an error occurred (to try to get that info to the user's attention) */
  /* called only in snd-g|xmain.c at initialization time */

  /* changed Oct-05 because the Scheme/Ruby/Forth choices are becoming a hassle --
   *   now save-options has its own file ~/.snd_prefs_ruby|forth|s7 which is loaded first, if present
   *     then ~/.snd_ruby|forth|s7, if present
   *     then ~/.snd for backwards compatibility
   * snd_options does not write ~/.snd anymore, but overwrites the .snd_prefs_* file
   * use set init files only change the ~/.snd choice
   *
   * there are parallel choices for the global configuration file: /etc/snd_ruby|forth|s7.conf
   */

#if HAVE_EXTENSION_LANGUAGE
#if HAVE_RUBY
  #define SND_EXT_CONF "/etc/snd_ruby.conf"
  #define SND_PREFS "~/.snd_prefs_ruby"
  #define SND_INIT "~/.snd_ruby"
#endif

#if HAVE_FORTH
  #define SND_EXT_CONF "/etc/snd_forth.conf"
  #define SND_PREFS "~/.snd_prefs_forth"
  #define SND_INIT "~/.snd_forth"
#endif

#if HAVE_SCHEME
  #define SND_EXT_CONF "/etc/snd_s7.conf"
  #define SND_PREFS "~/.snd_prefs_s7"
  #define SND_INIT "~/.snd_s7"
#endif

#define SND_INIT_FILE_ENVIRONMENT_NAME "SND_INIT_FILE"
#if (defined(_MSC_VER) || __CYGWIN__)
  #define INIT_FILE_NAME "snd-init"
#else
  #define INIT_FILE_NAME "~/.snd"
#endif

  #define SND_CONF "/etc/snd.conf"
  redirect_snd_print_to(string_to_stdout, NULL);
  redirect_errors_to(string_to_stderr_and_listener, NULL);

  /* check for global configuration files (/etc/snd*) */
  if (!no_global)
    {
      snd_load_init_file_1(SND_EXT_CONF);
      snd_load_init_file_1(SND_CONF);
    }

  /* now load local init file(s) */
  if (!no_init)
    {
      char *temp;
      snd_load_init_file_1(SND_PREFS);  /* check for possible prefs dialog output */
      snd_load_init_file_1(SND_INIT);
      temp = getenv(SND_INIT_FILE_ENVIRONMENT_NAME);
      if (temp)
	snd_load_init_file_1(temp);
      else snd_load_init_file_1(INIT_FILE_NAME);
    }

  redirect_everything_to(NULL, NULL);
#endif
}


static char *find_source_file(const char *orig);

void snd_load_file(const char *filename)
{
  char *str, *str2 = NULL;

  str = mus_expand_filename(filename);
  if (!(mus_file_probe(str)))
    {
      char *temp;
      temp = find_source_file(str); 
      free(str);
      str = temp;
    }
  if (!str)
    {
      snd_error("can't load %s: %s", filename, snd_open_strerror());
      return;
    }

  str2 = mus_format("(load \"%s\")", filename);   /* currently unused in Forth and Ruby */
  snd_catch_any(eval_file_wrapper, (void *)str, str2);
  if (str) free(str);
  if (str2) free(str2);
}


static Xen g_snd_print(Xen msg)
{
  #define H_snd_print "(" S_snd_print " str): display str in the listener window"
  char *str = NULL;

  if (Xen_is_string(msg))
    str = mus_strdup(Xen_string_to_C_string(msg));
  else
    {
      if (Xen_is_char(msg))
	{
	  str = (char *)calloc(2, sizeof(char));
	  str[0] = Xen_char_to_C_char(msg);
	}
      else str = gl_print(msg);
    }

  if (str)
    {
#if USE_GTK
      if (ss->listener)
#endif
      listener_append(str);
      free(str);
    }
  /* used to check for event in Motif case, but that is very dangerous -- check for infinite loop C-c needs to be somewhere else */
  return(msg);
}


void check_features_list(const char *features)
{
  /* check for list of features, report any missing, exit (for compsnd) */
  /*  this can't be in snd.c because we haven't fully initialized the extension language and so on at that point */
  if (!features) return;

#if HAVE_SCHEME
  Xen_eval_C_string(mus_format("(for-each \
                                  (lambda (f)	\
                                    (if (not (provided? f)) \
                                        (display (format #f \"~%%no ~A!~%%~%%\" f)))) \
                                  (list %s))", features));
#endif

#if HAVE_RUBY
  /* provided? is defined in examp.rb */
  Xen_eval_C_string(mus_format("[%s].each do |f|\n\
                                  unless $LOADED_FEATURES.map do |ff| File.basename(ff) end.member?(f.to_s.tr(\"_\", \"-\"))\n\
                                    $stderr.printf(\"~\\nno %%s!\\n\\n\", f.id2name)\n\
                                  end\n\
                                end\n", features));
#endif

#if HAVE_FORTH
  Xen_eval_C_string(mus_format("'( %s ) [each] dup \
                                          provided? [if] \
                                            drop \
                                          [else] \
                                            1 >list \"\\nno %%s!\\n\\n\" swap format .stderr \
                                          [then] \
                                        [end-each]\n", 
			       features)); 
#endif
  snd_exit(0);
}


mus_float_t string_to_mus_float_t(const char *str, mus_float_t lo, const char *field_name)
{
#if HAVE_EXTENSION_LANGUAGE
  Xen res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->float");
  if (Xen_is_number(res))
    {
      mus_float_t f;
      f = Xen_real_to_C_double(res);
      if (f < lo)
	snd_error("%s: %.3f is invalid", field_name, f);
      else return(f);
    }
  else snd_error("%s is not a number", str);
  return(0.0);
#else
  float res = 0.0;
  if (str) 
    {
      if (!(sscanf(str, "%f", &res)))
	snd_error("%s is not a number", str);
      else
	{
	  if (res < lo)
	    snd_error("%s: %.3f is invalid", field_name, res);
	}
    }
  return((mus_float_t)res);
#endif
}


int string_to_int(const char *str, int lo, const char *field_name) 
{
#if HAVE_EXTENSION_LANGUAGE
  Xen res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->int");
  if (Xen_is_number(res))
    {
      int val;
      val = Xen_integer_to_C_int(res);
      if (val < lo)
	snd_error("%s: %d is invalid", field_name, val);
      else return(val);
    }
  else snd_error("%s: %s is not a number", field_name, str);
  return(0);
#else
  int res = 0;
  if (str) 
    {
      if (!(sscanf(str, "%12d", &res)))
	snd_error("%s: %s is not a number", field_name, str);
      else
	{
	  if (res < lo)
	    snd_error("%s: %d is invalid", field_name, res);
	}
    }
  return(res);
#endif
}


mus_long_t string_to_mus_long_t(const char *str, mus_long_t lo, const char *field_name)
{
#if HAVE_EXTENSION_LANGUAGE
  Xen res;

  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->mus_long_t");
  if (Xen_is_number(res))
    {
      mus_long_t val;
      val = Xen_llong_to_C_llong(res);
      if (val < lo)
	snd_error("%s: %" PRId64 " is invalid", field_name, val);
      else return(val);
    }
  else snd_error("%s: %s is not a number", field_name, str);
  return(0);
#else
  mus_long_t res = 0;
  if (str) 
    {
      if (!(sscanf(str, "%" PRId64, &res)))
	snd_error("%s: %s is not a number", field_name, str);
      else
	{
	  if (res < lo)
	    snd_error("%s: %" PRId64 " is invalid", field_name, res);
	}
    }
  return(res);
#endif
}


Xen run_progn_hook(Xen hook, Xen args, const char *caller)
{
#if HAVE_SCHEME
  return(s7_call(s7, hook, args));
#else
  Xen result = Xen_false;
  Xen procs = Xen_hook_list(hook);

  while (!Xen_is_null(procs))
    {
      result = Xen_apply(Xen_car(procs), args, caller);
      procs = Xen_cdr(procs);
    }

  return(result);
#endif
}


Xen run_hook(Xen hook, Xen args, const char *caller)
{
#if HAVE_SCHEME
  return(s7_call(s7, hook, args));
#else
  Xen procs = Xen_hook_list(hook);

  while (!Xen_is_null(procs))
    {
      if (!(Xen_is_eq(args, Xen_empty_list)))
	Xen_apply(Xen_car(procs), args, caller);
      else Xen_call_with_no_args(Xen_car(procs), caller);
      procs = Xen_cdr(procs);
    }

  return(Xen_false);
#endif
}


Xen run_or_hook(Xen hook, Xen args, const char *caller)
{
#if HAVE_SCHEME
  return(s7_call(s7, hook, args));
#else
  Xen result = Xen_false; /* (or): #f */
  Xen hook_result = Xen_false;
  Xen procs = Xen_hook_list(hook);

  while (!Xen_is_null(procs))
    {
      if (!(Xen_is_eq(args, Xen_empty_list)))
	result = Xen_apply(Xen_car(procs), args, caller);
      else result = Xen_call_with_no_args(Xen_car(procs), caller);
      if (!Xen_is_false(result)) 
        hook_result = result;
      procs = Xen_cdr(procs);
    }

  return(hook_result);
#endif
}



#if HAVE_SCHEME && (!_MSC_VER)
#include <dlfcn.h>
/* these are included because libtool's dlopen is incredibly stupid */

/* apparently netBSD does not have dlerror? 
    #ifdef __NetBSD__
      #define dlerror() g_strerror(errno)
    #endif

    to get symbols from current program: handle = dlopen(NULL, RTLD_GLOBAL | RTLD_LAZY);
 */

static Xen g_dlopen(Xen name, Xen flags)
{
  #define H_dlopen "(dlopen lib (flags RTLD_LAZY)) loads the dynamic library 'lib' and returns a handle for it (for dlinit and dlclose)"
  const char *cname;
  Xen_check_type(Xen_is_string(name), name, 1, "dlopen", "a string (filename)");
  cname = Xen_string_to_C_string(name);
  if (cname)
    {
      void *handle;
      handle = dlopen(cname, RTLD_LAZY);
      if (!handle)
	{
	  char *longname;

	  longname = mus_expand_filename(cname);
	  if (Xen_is_integer(flags))
	    handle = dlopen(longname, Xen_integer_to_C_int(flags));
	  else handle = dlopen(longname, RTLD_LAZY);
	  free(longname);

	  if (!handle)
	    {
	      char *err;
	      err = (char *)dlerror();
	      if ((err) && (*err))
		return(C_string_to_Xen_string(err));
	      return(Xen_false);
	    }
	}
      return(Xen_wrap_C_pointer(handle));
    }
  return(Xen_false);
}


static Xen g_dlclose(Xen handle)
{
  #define H_dlclose "(dlclose handle) may close the library referred to by 'handle'."
  Xen_check_type(Xen_is_wrapped_c_pointer(handle), handle, 1, "dlclose", "a library handle");
  return(C_int_to_Xen_integer(dlclose((void *)(Xen_unwrap_C_pointer(handle)))));
}


static Xen g_dlerror(void)
{
  #define H_dlerror "(dlerror) returns a string describing the last dlopen/dlinit/dlclose error"
  return(C_string_to_Xen_string(dlerror()));
}


static Xen g_dlsym(Xen handle, Xen func)
{
  #define H_dlsym "(dlsym library function-name) returns a pointer to function in library, or #f."
  void *proc;

  Xen_check_type(Xen_is_wrapped_c_pointer(handle), handle, 1, "dlsym", "a library handle");
  Xen_check_type(Xen_is_string(func), func, 2, "dlsym", "a string (function name)");

  proc = dlsym((void *)(Xen_unwrap_C_pointer(handle)), Xen_string_to_C_string(func));
  if (!proc) return(Xen_false);
  return(Xen_wrap_C_pointer(func));
}


static Xen g_dlinit(Xen handle, Xen func)
{
  #define H_dlinit "(dlinit handle func) calls 'func' from the library referred to by 'handle'."
  typedef void *(*snd_dl_func)(void);
  void *proc;

  Xen_check_type(Xen_is_wrapped_c_pointer(handle), handle, 1, "dlinit", "a library handle");
  Xen_check_type(Xen_is_string(func), func, 2, "dlinit", "a string (init func name)");

  proc = dlsym((void *)(Xen_unwrap_C_pointer(handle)), Xen_string_to_C_string(func));
  if (!proc) return(C_string_to_Xen_string(dlerror()));
  ((snd_dl_func)proc)();
  return(Xen_true);
}
#endif

static Xen g_little_endian(void)
{
#if MUS_LITTLE_ENDIAN
  return(Xen_true);
#else
  return(Xen_false);
#endif
}


static Xen g_snd_global_state(void)
{
  return(Xen_wrap_C_pointer(ss));
}


#if (!HAVE_SCHEME)
/* fmod is the same as modulo in s7:
   (do ((i 0 (+ i 1))) 
       ((= i 100)) 
     (let ((val1 (- (random 1.0) 2.0)) 
           (val2 (- (random 1.0) 2.0)))
       (let ((f (fmod val1 val2)) 
             (m (modulo val1 val2))) 
         (if (> (abs (- f m)) 1e-9) 
             (format *stderr* "~A ~A -> ~A ~A~%" val1 val2 f m)))))
*/

static Xen g_fmod(Xen a, Xen b)
{
  double val, x, y;
  Xen_check_type(Xen_is_number(a), a, 1, "fmod", " a number");
  Xen_check_type(Xen_is_number(b), b, 2, "fmod", " a number");
  x = Xen_real_to_C_double(a);
  y = Xen_real_to_C_double(b);
  val = fmod(x, y);
  if (((y > 0.0) && (val < 0.0)) ||
      ((y < 0.0) && (val > 0.0)))
    return(C_double_to_Xen_real(val + y));
  return(C_double_to_Xen_real(val));
}
#endif


#if HAVE_SPECIAL_FUNCTIONS || HAVE_GSL
#define S_bes_j0 "bes-j0"
#define S_bes_j1 "bes-j1"
#define S_bes_jn "bes-jn"
#define S_bes_y0 "bes-y0"
#define S_bes_y1 "bes-y1"
#define S_bes_yn "bes-yn"
#endif


#if HAVE_SCHEME && WITH_GMP && HAVE_SPECIAL_FUNCTIONS

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>

static Xen big_math_1(Xen x, 
		      int (*mpfr_math)(mpfr_ptr, mpfr_srcptr, mpfr_rnd_t))
{
  s7_pointer val;
  mpfr_t y;
  mpfr_init_set(y, *s7_big_real(x), GMP_RNDN);
  mpfr_math(y, y, GMP_RNDN);
  val = s7_make_big_real(s7, &y);
  mpfr_clear(y);
  return(val);
}


static Xen big_j0(Xen x) {return(big_math_1(x, mpfr_j0));}
static Xen big_j1(Xen x) {return(big_math_1(x, mpfr_j1));}
static Xen big_y0(Xen x) {return(big_math_1(x, mpfr_y0));}
static Xen big_y1(Xen x) {return(big_math_1(x, mpfr_y1));}

static Xen big_erf(Xen x) {return(big_math_1(x, mpfr_erf));}
static Xen big_erfc(Xen x) {return(big_math_1(x, mpfr_erfc));}


static Xen big_math_2(Xen n, Xen x, 
		      int (*mpfr_math)(mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t))
{
  s7_pointer val;
  mpfr_t y;
  mpfr_init_set(y, *s7_big_real(x), GMP_RNDN);
  mpfr_math(y, Xen_integer_to_C_int(n), y, GMP_RNDN);
  val = s7_make_big_real(s7, &y);
  mpfr_clear(y);
  return(val);
}


static Xen big_jn(Xen n, Xen x) {return(big_math_2(n, x, mpfr_jn));}
static Xen big_yn(Xen n, Xen x) {return(big_math_2(n, x, mpfr_yn));}


/* bes-i0 from G&R 8.447, 8.451, A&S 9.6.12, 9.7.1, arprec bessel.cpp */

static Xen big_i0(Xen ux)
{
  int k;
  mpfr_t sum, x, x1, x2, eps;
  mpfr_init_set_ui(sum, 0, GMP_RNDN);
  mpfr_init_set(x, *s7_big_real(ux), GMP_RNDN);
  mpfr_init_set_ui(sum, 1, GMP_RNDN);
  mpfr_init_set_ui(x1, 1, GMP_RNDN);
  mpfr_init_set_ui(eps, 2, GMP_RNDN);
  mpfr_pow_si(eps, eps, -mpfr_get_default_prec(), GMP_RNDN);
  mpfr_init_set_ui(x2, mpfr_get_default_prec(), GMP_RNDN);
  mpfr_div_ui(x2, x2, 2, GMP_RNDN);
  if (mpfr_cmpabs(x, x2) < 0)
    {
      mpfr_mul(x, x, x, GMP_RNDN);           /* x = ux^2 */
      for (k = 1; k < 10000; k++)
	{
	  mpfr_set_ui(x2, k, GMP_RNDN);      /* x2 = k */
	  mpfr_mul(x2, x2, x2, GMP_RNDN);    /* x2 = k^2 */
	  mpfr_div(x1, x1, x2, GMP_RNDN);    /* x1 = x1/x2 */
	  mpfr_mul(x1, x1, x, GMP_RNDN);     /* x1 = x1*x */
	  mpfr_div_ui(x1, x1, 4, GMP_RNDN);  /* x1 = x1/4 */
	  if (mpfr_cmp(x1, eps) < 0)
	    break;
	  mpfr_add(sum, sum, x1, GMP_RNDN);  /* sum += x1 */
	}
      /* takes usually ca 10 to 40 iterations */
    }
  else
    {
      mpfr_t den, num;
      mpfr_init(den);
      mpfr_init(num);
      mpfr_abs(x, x, GMP_RNDN);
      for (k = 1; k < 10000; k++)
	{
	  mpfr_set(x2, x1, GMP_RNDN);
	  mpfr_set_ui(den, k, GMP_RNDN);
	  mpfr_mul_ui(den, den, 8, GMP_RNDN);
	  mpfr_mul(den, den, x, GMP_RNDN);
	  mpfr_set_ui(num, k, GMP_RNDN);
	  mpfr_mul_ui(num, num, 2, GMP_RNDN);
	  mpfr_sub_ui(num, num, 1, GMP_RNDN);
	  mpfr_mul(num, num, num, GMP_RNDN);
	  mpfr_div(num, num, den, GMP_RNDN);
	  mpfr_mul(x1, x1, num, GMP_RNDN);
	  mpfr_add(sum, sum, x1, GMP_RNDN);  
	  if (mpfr_cmp(x1, eps) < 0)
	    {
	      mpfr_const_pi(x2, GMP_RNDN);
	      mpfr_mul_ui(x2, x2, 2, GMP_RNDN);
	      mpfr_mul(x2, x2, x, GMP_RNDN);
	      mpfr_sqrt(x2, x2, GMP_RNDN);           /* sqrt(2*pi*x) */
	      mpfr_div(sum, sum, x2, GMP_RNDN);
	      mpfr_exp(x1, x, GMP_RNDN);
	      mpfr_mul(sum, sum, x1, GMP_RNDN);      /* sum * e^x / sqrt(2*pi*x) */
	      break;
	    }
	  if (mpfr_cmp(x1, x2) > 0)
	    {
	      fprintf(stderr, "bes-i0 has screwed up");
	      break;
	    }
	}
      mpfr_clear(den);
      mpfr_clear(num);
    }
  mpfr_clear(x1);
  mpfr_clear(x2);
  mpfr_clear(x);
  mpfr_clear(eps);
  return(s7_make_big_real(s7, &sum));
}


/* fft
 *     (define hi (make-vector 8))
 *     (define ho (make-vector 8))
 *     (do ((i 0 (+ i 1))) ((= i 8)) (vector-set! hi i (bignum "0.0")) (vector-set! ho i (bignum "0.0")))
 *     (vector-set! ho 1 (bignum "-1.0"))
 *     (vector-set! ho 1 (bignum "-1.0"))
 *     (bignum-fft hi ho 8)
 *
 * this is tricky -- perhaps a bad idea.  vector elements are changed in place which means
 *   they better be unique!  and there are no checks that each element actually is a bignum
 *   which means we'll segfault if a normal real leaks through.
 *
 * bignum_fft is say 200 times slower than the same size fftw call, and takes more space than
 *   I can account for: 2^20 29 secs ~.5 Gb, 2^24 11 mins ~5Gb.  I think there should be
 *   the vector element (8), the mpfr_t space (16 or 32), the s7_cell (28 or 32), and the value pointer (8),
 *   and the heap pointer loc (8) so 2^24 should be (* 2 (expt 2 24) (+ 8 8 8 8 32 32)) = 3 Gb, not 5.  2^25 25 min 10.6?
 *   I think the extra is in the free space in the heap -- it can be adding 1/4 of the total.
 */

static s7_pointer bignum_fft(s7_scheme *sc, s7_pointer args)
{
  #define H_bignum_fft "(bignum-fft rl im n (sign 1)) performs a multiprecision fft on the vectors of bigfloats rl and im"

  int n, sign = 1;
  s7_pointer *rl, *im;

  int m, j, mh, ldm, lg, i, i2, j2, imh;
  mpfr_t ur, ui, u, vr, vi, angle, c, s, temp;

  #define big_rl(n) (*(s7_big_real(rl[n])))
  #define big_im(n) (*(s7_big_real(im[n])))

  n = s7_integer(s7_list_ref(sc, args, 2));
  if (s7_list_length(sc, args) > 3)
    sign = s7_integer(s7_list_ref(sc, args, 3));

  rl = s7_vector_elements(s7_list_ref(sc, args, 0));
  im = s7_vector_elements(s7_list_ref(sc, args, 1));

  /* scramble(rl, im, n); */
  {
    int i, m, j;
    s7_pointer vr, vi;
    j = 0;
    for (i = 0; i < n; i++)
      {
	if (j > i)
	  {
	    vr = rl[j];
	    vi = im[j];
	    rl[j] = rl[i];
	    im[j] = im[i];
	    rl[i] = vr;
	    im[i] = vi;
	  }
	m = n >> 1;
	while ((m >= 2) && (j >= m))
	  {
	    j -= m;
	    m = m >> 1;
	  }
	j += m;
      }
  }

  imh = (int)(log(n + 1) / log(2.0));
  m = 2;
  ldm = 1;
  mh = n >> 1;

  mpfr_init(angle);                        /* angle = (M_PI * sign) */
  mpfr_const_pi(angle, GMP_RNDN);
  if (sign == -1)
    mpfr_neg(angle, angle, GMP_RNDN);

  mpfr_init(c);
  mpfr_init(s);
  mpfr_init(ur);
  mpfr_init(ui);
  mpfr_init(u);
  mpfr_init(vr);
  mpfr_init(vi);
  mpfr_init(temp);

  for (lg = 0; lg < imh; lg++)
    {
      mpfr_cos(c, angle, GMP_RNDN);         /* c = cos(angle) */
      mpfr_sin(s, angle, GMP_RNDN);         /* s = sin(angle) */
      mpfr_set_ui(ur, 1, GMP_RNDN);         /* ur = 1.0 */
      mpfr_set_ui(ui, 0, GMP_RNDN);         /* ui = 0.0 */
      for (i2 = 0; i2 < ldm; i2++)
	{
	  i = i2;
	  j = i2 + ldm;
	  for (j2 = 0; j2 < mh; j2++)
	    {
	      mpfr_set(temp, big_im(j), GMP_RNDN);          /* vr = ur * rl[j] - ui * im[j] */
	      mpfr_mul(temp, temp, ui, GMP_RNDN);
	      mpfr_set(vr, big_rl(j), GMP_RNDN);
	      mpfr_mul(vr, vr, ur, GMP_RNDN);
	      mpfr_sub(vr, vr, temp, GMP_RNDN);
	      
	      mpfr_set(temp, big_rl(j), GMP_RNDN);          /* vi = ur * im[j] + ui * rl[j] */
	      mpfr_mul(temp, temp, ui, GMP_RNDN);
	      mpfr_set(vi, big_im(j), GMP_RNDN);
	      mpfr_mul(vi, vi, ur, GMP_RNDN);
	      mpfr_add(vi, vi, temp, GMP_RNDN);
	      
	      mpfr_set(big_rl(j), big_rl(i), GMP_RNDN);     /* rl[j] = rl[i] - vr */
	      mpfr_sub(big_rl(j), big_rl(j), vr, GMP_RNDN);

	      mpfr_set(big_im(j), big_im(i), GMP_RNDN);     /* im[j] = im[i] - vi */
	      mpfr_sub(big_im(j), big_im(j), vi, GMP_RNDN);
	      
	      mpfr_add(big_rl(i), big_rl(i), vr, GMP_RNDN); /* rl[i] += vr */
	      mpfr_add(big_im(i), big_im(i), vi, GMP_RNDN); /* im[i] += vi */
	      
	      i += m;
	      j += m;
	    }

	  mpfr_set(u, ur, GMP_RNDN);             /* u = ur */
	  mpfr_set(temp, ui, GMP_RNDN);          /* ur = (ur * c) - (ui * s) */
	  mpfr_mul(temp, temp, s, GMP_RNDN);
	  mpfr_mul(ur, ur, c, GMP_RNDN);
	  mpfr_sub(ur, ur, temp, GMP_RNDN);
	  
	  mpfr_set(temp, u, GMP_RNDN);           /* ui = (ui * c) + (u * s) */
	  mpfr_mul(temp, temp, s, GMP_RNDN);
	  mpfr_mul(ui, ui, c, GMP_RNDN);
	  mpfr_add(ui, ui, temp, GMP_RNDN);
	}
      mh >>= 1;
      ldm = m;

      mpfr_div_ui(angle, angle, 2, GMP_RNDN);   /* angle *= 0.5 */
      m <<= 1;
    }
  return(s7_f(sc));
}

#endif


#if HAVE_SPECIAL_FUNCTIONS && (!HAVE_GSL)
static Xen g_j0(Xen x)
{
  #define H_j0 "(" S_bes_j0 " x): returns the regular cylindrical bessel function value J0(x)"
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_j0, " a number");
#endif

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j0(x));
#endif
  return(C_double_to_Xen_real(j0(Xen_real_to_C_double(x))));
}


static Xen g_j1(Xen x)
{
  #define H_j1 "(" S_bes_j1 " x): returns the regular cylindrical bessel function value J1(x)"
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_j1, " a number");
#endif

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j1(x));
#endif
  return(C_double_to_Xen_real(j1(Xen_real_to_C_double(x))));
}


static Xen g_jn(Xen order, Xen x)
{
  #define H_jn "(" S_bes_jn " n x): returns the regular cylindrical bessel function value Jn(x)"
  Xen_check_type(Xen_is_integer(order), x, 1, S_bes_jn, " an int");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(x), x, 2, S_bes_jn, " a number");
#endif

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_jn(order, x));
#endif
  return(C_double_to_Xen_real(jn(Xen_integer_to_C_int(order), Xen_real_to_C_double(x))));
}


static Xen g_y0(Xen x)
{
  #define H_y0 "(" S_bes_y0 " x): returns the irregular cylindrical bessel function value Y0(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_y0, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y0(x));
#endif
  return(C_double_to_Xen_real(y0(Xen_real_to_C_double(x))));
}


static Xen g_y1(Xen x)
{
  #define H_y1 "(" S_bes_y1 " x): returns the irregular cylindrical bessel function value Y1(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_y1, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y1(x));
#endif
  return(C_double_to_Xen_real(y1(Xen_real_to_C_double(x))));
}


static Xen g_yn(Xen order, Xen x)
{
  #define H_yn "(" S_bes_yn " n x): returns the irregular cylindrical bessel function value Yn(x)"
  Xen_check_type(Xen_is_integer(order), x, 1, S_bes_yn, " an int");
  Xen_check_type(Xen_is_number(x), x, 2, S_bes_yn, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_yn(order, x));
#endif
  return(C_double_to_Xen_real(yn(Xen_integer_to_C_int(order), Xen_real_to_C_double(x))));
}


static Xen g_erf(Xen x)
{
  #define H_erf "(erf x): returns the error function erf(x)"
  Xen_check_type(Xen_is_number(x), x, 1, "erf", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erf(x));
#endif
  return(C_double_to_Xen_real(erf(Xen_real_to_C_double(x))));
}


static Xen g_erfc(Xen x)
{
  #define H_erfc "(erfc x): returns the complementary error function erfc(x)"
  Xen_check_type(Xen_is_number(x), x, 1, "erfc", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erfc(x));
#endif
  return(C_double_to_Xen_real(erfc(Xen_real_to_C_double(x))));
}


static Xen g_lgamma(Xen x)
{
  #define H_lgamma "(lgamma x): returns the log of the gamma function at x"
  Xen_check_type(Xen_is_number(x), x, 1, "lgamma", " a number");
  return(C_double_to_Xen_real(lgamma(Xen_real_to_C_double(x))));
}
#endif


#define S_bes_i0 "bes-i0"

static Xen g_i0(Xen x)
{
  #define H_i0 "(" S_bes_i0 " x): returns the modified cylindrical bessel function value I0(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_i0, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_i0(x));
#endif
  return(C_double_to_Xen_real(mus_bessi0(Xen_real_to_C_double(x)))); /* uses GSL if possible */
}


/* ---------------------------------------- use GSL ---------------------------------------- */
#if HAVE_GSL

/* include all the bessel functions, etc */
#include <gsl/gsl_sf_bessel.h>

static Xen g_j0(Xen x)
{
  #define H_j0 "(" S_bes_j0 " x): returns the regular cylindrical bessel function value J0(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_j0, " a number");

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j0(x));
#endif
  return(C_double_to_Xen_real(gsl_sf_bessel_J0(Xen_real_to_C_double(x))));
}


static Xen g_j1(Xen x)
{
  #define H_j1 "(" S_bes_j1 " x): returns the regular cylindrical bessel function value J1(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_j1, " a number");

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_j1(x));
#endif
  return(C_double_to_Xen_real(gsl_sf_bessel_J1(Xen_real_to_C_double(x))));
}


static Xen g_jn(Xen order, Xen x)
{
  #define H_jn "(" S_bes_jn " n x): returns the regular cylindrical bessel function value Jn(x)"
  Xen_check_type(Xen_is_integer(order), x, 1, S_bes_jn, " an int");
  Xen_check_type(Xen_is_number(x), x, 2, S_bes_jn, " a number");

#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_jn(order, x));
#endif
  return(C_double_to_Xen_real(gsl_sf_bessel_Jn(Xen_integer_to_C_int(order), Xen_real_to_C_double(x))));
}


static Xen g_y0(Xen x)
{
  #define H_y0 "(" S_bes_y0 " x): returns the irregular cylindrical bessel function value Y0(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_y0, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y0(x));
#endif
  return(C_double_to_Xen_real(gsl_sf_bessel_Y0(Xen_real_to_C_double(x))));
}


static Xen g_y1(Xen x)
{
  #define H_y1 "(" S_bes_y1 " x): returns the irregular cylindrical bessel function value Y1(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_y1, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_y1(x));
#endif
  return(C_double_to_Xen_real(gsl_sf_bessel_Y1(Xen_real_to_C_double(x))));
}


static Xen g_yn(Xen order, Xen x)
{
  #define H_yn "(" S_bes_yn " n x): returns the irregular cylindrical bessel function value Yn(x)"
  Xen_check_type(Xen_is_integer(order), x, 1, S_bes_yn, " an int");
  Xen_check_type(Xen_is_number(x), x, 2, S_bes_yn, " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_yn(order, x));
#endif
  return(C_double_to_Xen_real(gsl_sf_bessel_Yn(Xen_integer_to_C_int(order), Xen_real_to_C_double(x))));
}

#define S_bes_i1 "bes-i1"
#define S_bes_in "bes-in"
#define S_bes_k0 "bes-k0"
#define S_bes_k1 "bes-k1"
#define S_bes_kn "bes-kn"

static Xen g_i1(Xen x)
{
  #define H_i1 "(" S_bes_i1 " x): returns the regular cylindrical bessel function value I1(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_i1, " a number");
  return(C_double_to_Xen_real(gsl_sf_bessel_I1(Xen_real_to_C_double(x))));
}


static Xen g_in(Xen order, Xen x)
{
  #define H_in "(" S_bes_in " n x): returns the regular cylindrical bessel function value In(x)"
  Xen_check_type(Xen_is_integer(order), x, 1, S_bes_in, " an int");
  Xen_check_type(Xen_is_number(x), x, 2, S_bes_in, " a number");
  return(C_double_to_Xen_real(gsl_sf_bessel_In(Xen_integer_to_C_int(order), Xen_real_to_C_double(x))));
}


static Xen g_k0(Xen x)
{
  #define H_k0 "(" S_bes_k0 " x): returns the irregular cylindrical bessel function value K0(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_k0, " a number");
  return(C_double_to_Xen_real(gsl_sf_bessel_K0(Xen_real_to_C_double(x))));
}


static Xen g_k1(Xen x)
{
  #define H_k1 "(" S_bes_k1 " x): returns the irregular cylindrical bessel function value K1(x)"
  Xen_check_type(Xen_is_number(x), x, 1, S_bes_k1, " a number");
  return(C_double_to_Xen_real(gsl_sf_bessel_K1(Xen_real_to_C_double(x))));
}


static Xen g_kn(Xen order, Xen x)
{
  #define H_kn "(" S_bes_kn " n x): returns the irregular cylindrical bessel function value Kn(x)"
  Xen_check_type(Xen_is_integer(order), x, 1, S_bes_kn, " an int");
  Xen_check_type(Xen_is_number(x), x, 2, S_bes_kn, " a number");
  return(C_double_to_Xen_real(gsl_sf_bessel_Kn(Xen_integer_to_C_int(order), Xen_real_to_C_double(x))));
}


#include <gsl/gsl_sf_erf.h>
static Xen g_erf(Xen x)
{
  #define H_erf "(erf x): returns the error function erf(x)"
  Xen_check_type(Xen_is_number(x), x, 1, "erf", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erf(x));
#endif
  return(C_double_to_Xen_real(gsl_sf_erf(Xen_real_to_C_double(x))));
}


static Xen g_erfc(Xen x)
{
  #define H_erfc "(erfc x): returns the complementary error function value erfc(x)"
  Xen_check_type(Xen_is_number(x), x, 1, "erfc", " a number");
#if HAVE_SCHEME && WITH_GMP
  if ((s7_is_bignum(x)) &&
      (s7_is_real(x)) &&
      (!(s7_is_rational(x))))
    return(big_erfc(x));
#endif
  return(C_double_to_Xen_real(gsl_sf_erfc(Xen_real_to_C_double(x))));
}


#include <gsl/gsl_sf_gamma.h>
static Xen g_lgamma(Xen x)
{
  #define H_lgamma "(lgamma x): returns the log of the gamma function at x"
  Xen_check_type(Xen_is_number(x), x, 1, "lgamma", " a number");
  return(C_double_to_Xen_real(gsl_sf_lngamma(Xen_real_to_C_double(x))));
}



#include <gsl/gsl_sf_ellint.h>
static Xen g_gsl_ellipk(Xen k)
{
  double f;
  #define H_gsl_ellipk "(gsl-ellipk k): returns the complete elliptic integral k"
  Xen_check_type(Xen_is_number(k), k, 1, "gsl-ellipk", "a number");
  f = Xen_real_to_C_double(k);
  Xen_check_type(f >= 0.0, k, 1, "gsl-ellipk", "a non-negative number");
  return(C_double_to_Xen_real(gsl_sf_ellint_Kcomp(sqrt(Xen_real_to_C_double(k)), GSL_PREC_APPROX)));
}


#include <gsl/gsl_sf_elljac.h>
static Xen g_gsl_ellipj(Xen u, Xen m)
{
  #define H_gsl_ellipj "(gsl-ellipj u m): returns the Jacobian elliptic functions sn, cn, and dn of u and m"
  double sn = 0.0, cn = 0.0, dn = 0.0;
  Xen_check_type(Xen_is_number(u), u, 1, "gsl-ellipj", "a number");
  Xen_check_type(Xen_is_number(m), m, 2, "gsl-ellipj", "a number");
  gsl_sf_elljac_e(Xen_real_to_C_double(u),
		  Xen_real_to_C_double(m),
		  &sn, &cn, &dn);
  return(Xen_list_3(C_double_to_Xen_real(sn),
		    C_double_to_Xen_real(cn),
		    C_double_to_Xen_real(dn)));
}


#include <gsl/gsl_version.h>
#if ((GSL_MAJOR_VERSION >= 1) && (GSL_MINOR_VERSION >= 9))
  #define HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE 1
#endif

#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE

/* eignevector/values, from gsl/doc/examples/eigen_nonsymm.c */

#include <gsl/gsl_math.h>
#include <gsl/gsl_eigen.h>

static Xen g_gsl_eigenvectors(Xen matrix)
{
  double *data;
  int i, j, len;
  Xen values = Xen_false, vectors = Xen_false;

#if HAVE_SCHEME
  Xen_check_type(s7_is_float_vector(matrix), matrix, 1, "gsl-eigenvectors", "a float vector");
  len = (int)sqrt(s7_vector_length(matrix));
  data = (double *)s7_float_vector_elements(matrix);
#else
  vct *v;
  Xen_check_type(mus_is_vct(matrix), matrix, 1, "gsl-eigenvectors", "a vct");
  v = Xen_to_vct(matrix);
  len = (int)sqrt(mus_vct_length(v));
  data = mus_vct_data(v);
#endif

  {
    gsl_matrix_view m = gsl_matrix_view_array(data, len, len);
    gsl_vector_complex *eval = gsl_vector_complex_alloc(len);
    gsl_matrix_complex *evec = gsl_matrix_complex_alloc(len, len);
    gsl_eigen_nonsymmv_workspace *w = gsl_eigen_nonsymmv_alloc(len);
    gsl_eigen_nonsymmv(&m.matrix, eval, evec, w);
    gsl_eigen_nonsymmv_free(w);
    gsl_eigen_nonsymmv_sort(eval, evec, GSL_EIGEN_SORT_ABS_DESC);
  
    {
      int values_loc, vectors_loc;

      values = Xen_make_vector(len, Xen_integer_zero);
      values_loc = snd_protect(values);
      vectors = Xen_make_vector(len, Xen_false);
      vectors_loc = snd_protect(vectors);

      for (i = 0; i < len; i++)
	{
	  Xen vect;
#if HAVE_SCHEME
	  s7_double *fv_data;
#endif
	  gsl_complex eval_i = gsl_vector_complex_get(eval, i);
	  gsl_vector_complex_view evec_i = gsl_matrix_complex_column(evec, i);
	  Xen_vector_set(values, i, C_double_to_Xen_real(GSL_REAL(eval_i)));
	
#if HAVE_SCHEME
	  vect = s7_make_float_vector(s7, len, 1, NULL);
	  fv_data = s7_float_vector_elements(vect);
#else
	  vect = Xen_make_vector(len, Xen_integer_zero);
#endif
	  Xen_vector_set(vectors, i, vect);

	  for (j = 0; j < len; j++)
	    {
	      gsl_complex z = gsl_vector_complex_get(&evec_i.vector, j);
#if HAVE_SCHEME
	      fv_data[j] = GSL_REAL(z);
#else
	      Xen_vector_set(vect, j, C_double_to_Xen_real(GSL_REAL(z)));
#endif
	    }
	}
      snd_unprotect_at(values_loc);
      snd_unprotect_at(vectors_loc);
    }

    gsl_vector_complex_free(eval);
    gsl_matrix_complex_free(evec);
  }

#if (!HAVE_SCHEME)
  free(data);
#endif
  return(Xen_list_2(values, vectors));
}
#endif


#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS
#include <gsl/gsl_poly.h>
#include <complex.h>

static Xen g_gsl_roots(Xen poly)
{
  #define H_gsl_roots "(gsl-roots poly): roots of poly"
  int i, n, loc;
  double *p;
  double complex *z;
  gsl_poly_complex_workspace *w;
  Xen result;

  /* gsl_roots: balance_companion_matrix gets hung if the vector is multidimensional */
  Xen_check_type((Xen_is_vector(poly)) && (Xen_vector_rank(poly) == 1), poly, 1, "gsl-roots", "a vector");

  n = Xen_vector_length(poly);
  w = gsl_poly_complex_workspace_alloc(n);
  z = (double complex *)calloc(n, sizeof(double complex));
  p = (double *)calloc(n, sizeof(double));

#if HAVE_SCHEME
  if (s7_is_float_vector(poly))
    {
      s7_double *e;
      e = s7_float_vector_elements(poly);
      for (i = 0; i < n; i++)
	p[i] = e[i];
    }
  else
    {
      for (i = 0; i < n; i++)
	p[i] = Xen_real_to_C_double(Xen_vector_ref(poly, i));
    }
#else
  for (i = 0; i < n; i++)
    p[i] = Xen_real_to_C_double(Xen_vector_ref(poly, i));
#endif

  gsl_poly_complex_solve(p, n, w, (gsl_complex_packed_ptr)z);
  gsl_poly_complex_workspace_free (w);

  result = Xen_make_vector(n - 1, Xen_integer_zero);
  loc = snd_protect(result);
  for (i = 0; i < n - 1; i++)
    if (__imag__(z[i]) != 0.0)
      Xen_vector_set(result, i, C_complex_to_Xen_complex(z[i]));
    else Xen_vector_set(result, i, C_double_to_Xen_real(__real__(z[i])));

  free(z);
  free(p);
  snd_unprotect_at(loc);
  return(result);
}
#endif
#endif



/* -------- source file extensions list -------- */

static char **source_file_extensions = NULL;
static int source_file_extensions_size = 0;
static int source_file_extensions_end = 0;
static int default_source_file_extensions = 0;

static void add_source_file_extension(const char *ext)
{
  int i;
  for (i = 0; i < source_file_extensions_end; i++)
    if (mus_strcmp(ext, source_file_extensions[i]))
      return;
  if (source_file_extensions_end == source_file_extensions_size)
    {
      source_file_extensions_size += 8;
      if (!source_file_extensions)
	source_file_extensions = (char **)calloc(source_file_extensions_size, sizeof(char *));
      else source_file_extensions = (char **)realloc(source_file_extensions, source_file_extensions_size * sizeof(char *));
    }
  source_file_extensions[source_file_extensions_end] = mus_strdup(ext);
  source_file_extensions_end++;
}


bool is_source_file(const char *name)
{
  if (!name) return(false);
  if (source_file_extensions)
    {
      int i, dot_loc = -1, len;
      len = strlen(name);

      for (i = 0; i < len; i++)
	if (name[i] == '.')
	  dot_loc = i;
      /* dot_loc is last dot in the name */

      if ((dot_loc > 0) &&
	  (dot_loc < len - 1))
	{
	  const char *ext;

	  ext = (const char *)(name + dot_loc + 1);
	  for (i = 0; i < source_file_extensions_end; i++)
	    if (mus_strcmp(ext, source_file_extensions[i]))
	      return(true);
	}
    }
  return(false);
}


void save_added_source_file_extensions(FILE *fd)
{
  if (source_file_extensions_end > default_source_file_extensions)
    {
      int i;
      for (i = default_source_file_extensions; i < source_file_extensions_end; i++)
	{
#if HAVE_SCHEME
	  fprintf(fd, "(%s \"%s\")\n", S_add_source_file_extension, source_file_extensions[i]);
#endif
	  
#if HAVE_RUBY
	  fprintf(fd, "%s(\"%s\")\n", to_proc_name(S_add_source_file_extension), source_file_extensions[i]);
#endif
	  
#if HAVE_FORTH
	  fprintf(fd, "\"%s\" %s drop\n", source_file_extensions[i], S_add_source_file_extension);
#endif
	}
    }
}


static Xen g_add_source_file_extension(Xen ext)
{
  #define H_add_source_file_extension "(" S_add_source_file_extension " ext):  add the file extension 'ext' to the list of source file extensions"
  Xen_check_type(Xen_is_string(ext), ext, 1, S_add_source_file_extension, "a string");
  add_source_file_extension(Xen_string_to_C_string(ext));
  return(ext);
}


static char *find_source_file(const char *orig)
{
  int i;
  for (i = 0; i < source_file_extensions_end; i++)
    {
      char *str;
      str = mus_format("%s.%s", orig, source_file_extensions[i]);
      if (mus_file_probe(str))
	return(str);
      free(str);
    }
  return(NULL);
}


/* list-in-vector|list, vector-in-list|vector, cobj-in-vector|list obj-in-cobj
 *   string-ci-in-vector? hash-table cases?
 *   most of this could be done via for-each
 */

#if HAVE_SCHEME && (!_MSC_VER)
  Xen_wrap_2_optional_args(g_dlopen_w, g_dlopen)
  Xen_wrap_1_arg(g_dlclose_w, g_dlclose)
  Xen_wrap_no_args(g_dlerror_w, g_dlerror)
  Xen_wrap_2_args(g_dlinit_w, g_dlinit)
  Xen_wrap_2_args(g_dlsym_w, g_dlsym)
#endif
#if HAVE_SCHEME
  Xen_wrap_1_arg(g_snd_s7_error_handler_w, g_snd_s7_error_handler);
#endif

Xen_wrap_1_arg(g_snd_print_w, g_snd_print)
Xen_wrap_no_args(g_little_endian_w, g_little_endian)
Xen_wrap_no_args(g_snd_global_state_w, g_snd_global_state)
Xen_wrap_1_arg(g_add_source_file_extension_w, g_add_source_file_extension)

#if (!HAVE_SCHEME)
Xen_wrap_2_args(g_fmod_w, g_fmod)
#endif

#if HAVE_SPECIAL_FUNCTIONS || HAVE_GSL
  Xen_wrap_1_arg(g_j0_w, g_j0)
  Xen_wrap_1_arg(g_j1_w, g_j1)
  Xen_wrap_2_args(g_jn_w, g_jn)
  Xen_wrap_1_arg(g_y0_w, g_y0)
  Xen_wrap_1_arg(g_y1_w, g_y1)
  Xen_wrap_2_args(g_yn_w, g_yn)
  Xen_wrap_1_arg(g_erf_w, g_erf)
  Xen_wrap_1_arg(g_erfc_w, g_erfc)
  Xen_wrap_1_arg(g_lgamma_w, g_lgamma)
#endif

Xen_wrap_1_arg(g_i0_w, g_i0)

#if HAVE_GSL
  Xen_wrap_1_arg(g_i1_w, g_i1)
  Xen_wrap_2_args(g_in_w, g_in)
  Xen_wrap_1_arg(g_k0_w, g_k0)
  Xen_wrap_1_arg(g_k1_w, g_k1)
  Xen_wrap_2_args(g_kn_w, g_kn)

  Xen_wrap_1_arg(g_gsl_ellipk_w, g_gsl_ellipk)
  Xen_wrap_2_args(g_gsl_ellipj_w, g_gsl_ellipj)
#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE
  Xen_wrap_1_arg(g_gsl_eigenvectors_w, g_gsl_eigenvectors)
#endif

  #if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS
    Xen_wrap_1_arg(g_gsl_roots_w, g_gsl_roots)
  #endif
#endif

#if HAVE_EXTENSION_LANGUAGE
#if HAVE_SCHEME
#if USE_MOTIF
  void Init_libxm(s7_scheme *sc);
#else
  void libgtk_s7_init(s7_scheme *sc);
#endif
#if HAVE_GL
 void Init_libgl(s7_scheme *sc);
#endif
#else /* not s7 */
#if USE_MOTIF
  void Init_libxm(void);
#else
#if USE_GTK
  void Init_libxg(void);
#endif
#endif
#if HAVE_GL
 void Init_libgl(void);
#endif
#endif
#endif

static char *legalize_path(const char *in_str)
{ 
  int inlen;
  char *out_str;
  int inpos, outpos = 0; 

  inlen = mus_strlen(in_str); 
  out_str = (char *)calloc(inlen * 2, sizeof(char)); 

  for (inpos = 0; inpos < inlen; inpos++)
    { 
      if (in_str[inpos] == '\\')
	out_str[outpos++] = '\\';
      out_str[outpos++] = in_str[inpos]; 
    } 

  return(out_str); 
} 


#if HAVE_GL
static Xen g_snd_gl_context(void)
{
#if USE_GTK
  /* return(Xen_list_2(C_string_to_Xen_symbol("GLContext"), Xen_wrap_C_pointer(ss->cx))); */
  return(XEN_FALSE);
#else
#if USE_MOTIF
  return(Xen_list_2(C_string_to_Xen_symbol("GLXContext"), Xen_wrap_C_pointer(ss->cx)));
#else
  return(XEN_FALSE);
#endif
#endif
} 

Xen_wrap_no_args(g_snd_gl_context_w, g_snd_gl_context)
#endif



/* -------------------------------------------------------------------------------- */

Xen_wrap_1_arg(g_snd_error_w, g_snd_error)
Xen_wrap_1_arg(g_snd_warning_w, g_snd_warning)

void g_xen_initialize(void)
{
#if HAVE_SCHEME
  s7_pointer pl_dr, pl_dir, pl_ss, pl_b, s, i, b, r, d, t;
#if WITH_GMP
  s7_pointer v;
#endif
#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE
  s7_pointer pl_pf;
#endif
#if HAVE_GSL || HAVE_GL
  s7_pointer pl_prr, p;
  p = s7_make_symbol(s7, "pair?");
#endif
  s = s7_make_symbol(s7, "string?");
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  r = s7_make_symbol(s7, "real?");
  d = s7_make_symbol(s7, "float?");
#if WITH_GMP
  v = s7_make_symbol(s7, "vector?");
#endif
  t = s7_t(s7);
  pl_ss = s7_make_signature(s7, 2, s, s);
  pl_dr = s7_make_circular_signature(s7, 1, 2, d, r);
#if HAVE_GSL
  pl_prr = s7_make_signature(s7, 3, p, r, r);
#endif
  pl_dir = s7_make_signature(s7, 3, d, i, r);
  pl_b = s7_make_signature(s7, 1, b);
#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE
  pl_pf = s7_make_signature(s7, 2, s7_make_symbol(s7, "pair?"), s7_make_symbol(s7, "float-vector?"));
#endif
#endif

#if HAVE_RUBY
  rb_gc_disable();
#endif

  Xen_define_typed_procedure(S_snd_error,   g_snd_error_w,   1, 0, 0, H_snd_error,   pl_ss);
  Xen_define_typed_procedure(S_snd_warning, g_snd_warning_w, 1, 0, 0, H_snd_warning, pl_ss);

#if HAVE_SCHEME
  #define H_snd_error_hook S_snd_error_hook " (message): called upon snd_error. \
If it returns " PROC_TRUE ", Snd flushes the error (it assumes you've reported it via the hook):\n\
  (hook-push " S_snd_error_hook "\n\
    (lambda (hook) (" S_play " \"bong.snd\")))"

  #define H_snd_warning_hook S_snd_warning_hook " (message): called upon snd_warning. \
If it returns " PROC_TRUE ", Snd flushes the warning (it assumes you've reported it via the hook):\n\
  (define without-warnings\n\
    (lambda (thunk)\n\
      (define no-warning (lambda (hook) (set! (hook 'result) #t)))\n\
      (hook-push snd-warning-hook no-warning) \n\
      (thunk)\n\
      (hook-remove snd-warning-hook no-warning)))"
#endif
#if HAVE_RUBY
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns true, Snd flushes the error (it assumes you've reported it via the hook):\n\
  $snd_error_hook.add-hook!(\"error\") do |msg|\n\
    play(\"bong.snd\")\n\
    false\n\
  end"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns true, Snd flushes the warning (it assumes you've reported it via the hook)\n\
  def without_warning(&body)\n\
    $snd_warning_hook.add_hook!(\"no_warning\") do |msg| true end\n\
    ret = body.call\n\
    $snd_warning_hook.remove_hook!(\"no_warning\")\n\
    ret\n\
  end\n\
  # without_warning do " S_snd_warning "(\"not shown\") end"
#endif
#if HAVE_FORTH
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns " PROC_TRUE ", Snd flushes the error (it assumes you've reported it via the hook):\n\
" S_snd_error_hook " lambda: <{ msg }>\n\
  \"bong.snd\" " S_play " drop\n\
  #f\n\
; add-hook!"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns " PROC_TRUE ", Snd flushes the warning (it assumes you've reported it via the hook)\n\
  : no-warning <{ msg -- f }> #t ;\n\
  : without-warnings <{ xt -- }>\n\
    " S_snd_warning_hook " <'> no-warning add-hook!\n\
    xt execute\n\
    " S_snd_warning_hook " <'> no-warning remove-hook! drop\n\
  ;\n\
  \\ lambda: ( -- ) \"not shown\" " S_snd_warning " ; without-warning\n\
"
#endif

  ss->snd_error_hook =   Xen_define_hook(S_snd_error_hook,   "(make-hook 'message)", 1, H_snd_error_hook);
  ss->snd_warning_hook = Xen_define_hook(S_snd_warning_hook, "(make-hook 'message)", 1, H_snd_warning_hook);

  #define H_clip_hook S_clip_hook " (val) is called each time a sample is about to \
be clipped upon being written to a sound file.  The hook function can return the new value to \
be written, or rely on the default (-1.0 or 1.0 depending on the sign of 'val')."

  clip_hook = Xen_define_hook(S_clip_hook, "(make-hook 'val)", 1, H_clip_hook); 
  mus_clip_set_handler_and_checker(NULL, clip_hook_checker);

  add_source_file_extension(Xen_file_extension);
#if HAVE_SCHEME
  add_source_file_extension("cl");
  add_source_file_extension("lisp");
  add_source_file_extension("init");  /* for slib */
#endif

#if HAVE_FORTH
  add_source_file_extension("fth");
  add_source_file_extension("fsm");
#endif
  add_source_file_extension("marks"); /* from save-marks */
  default_source_file_extensions = source_file_extensions_end;

  Xen_define_typed_procedure("snd-global-state", g_snd_global_state_w, 0, 0, 0, "internal testing function", s7_make_signature(s7, 1, t));
  Xen_define_typed_procedure(S_add_source_file_extension, g_add_source_file_extension_w, 1, 0, 0, H_add_source_file_extension,
			     s7_make_signature(s7, 2, s, s));

  ss->snd_open_file_hook = Xen_define_simple_hook("(make-hook 'reason)", 1);
  Xen_GC_protect(ss->snd_open_file_hook);

  ss->effects_hook = Xen_define_hook(S_effects_hook, "(make-hook)", 0, "called when something changes that the effects dialogs care about");

  Init_sndlib();

#if HAVE_FORTH
  fth_add_loaded_files("sndlib.so");
#endif

#if (!HAVE_SCHEME)
  gc_protection = Xen_false;
#endif

  Xen_define_typed_procedure(S_snd_print,      g_snd_print_w,     1, 0, 0, H_snd_print, pl_ss);
  Xen_define_typed_procedure("little-endian?", g_little_endian_w, 0, 0, 0, "return " PROC_TRUE " if host is little endian", pl_b);

#if HAVE_SCHEME
  Xen_eval_C_string("(define fmod modulo)");
#else
  Xen_define_procedure("fmod",           g_fmod_w,          2, 0, 0, "C's fmod");
#endif

#if HAVE_SPECIAL_FUNCTIONS || HAVE_GSL
  Xen_define_typed_procedure(S_bes_j0, g_j0_w,     1, 0, 0, H_j0,	pl_dr);
  Xen_define_typed_procedure(S_bes_j1, g_j1_w,     1, 0, 0, H_j1,	pl_dr);
  Xen_define_typed_procedure(S_bes_jn, g_jn_w,     2, 0, 0, H_jn,	pl_dir);
  Xen_define_typed_procedure(S_bes_y0, g_y0_w,     1, 0, 0, H_y0,	pl_dr);
  Xen_define_typed_procedure(S_bes_y1, g_y1_w,     1, 0, 0, H_y1,	pl_dr);
  Xen_define_typed_procedure(S_bes_yn, g_yn_w,     2, 0, 0, H_yn,	pl_dir);
  Xen_define_typed_procedure("erf",    g_erf_w,    1, 0, 0, H_erf,	pl_dr);
  Xen_define_typed_procedure("erfc",   g_erfc_w,   1, 0, 0, H_erfc,	pl_dr);
  Xen_define_typed_procedure("lgamma", g_lgamma_w, 1, 0, 0, H_lgamma,	pl_dr);
#endif

  Xen_define_typed_procedure(S_bes_i0, g_i0_w,     1, 0, 0, H_i0,       pl_dr);

#if HAVE_GSL
  Xen_define_typed_procedure(S_bes_i1, g_i1_w,     1, 0, 0, H_i1,	pl_dr);
  Xen_define_typed_procedure(S_bes_in, g_in_w,     2, 0, 0, H_in,	pl_dir);
  Xen_define_typed_procedure(S_bes_k0, g_k0_w,     1, 0, 0, H_k0,	pl_dr);
  Xen_define_typed_procedure(S_bes_k1, g_k1_w,     1, 0, 0, H_k1,	pl_dr);
  Xen_define_typed_procedure(S_bes_kn, g_kn_w,     2, 0, 0, H_kn,	pl_dir);

  Xen_define_typed_procedure("gsl-ellipk", g_gsl_ellipk_w, 1, 0, 0, H_gsl_ellipk, pl_dr);
  Xen_define_typed_procedure("gsl-ellipj", g_gsl_ellipj_w, 2, 0, 0, H_gsl_ellipj, pl_prr);

#if HAVE_GSL_EIGEN_NONSYMMV_WORKSPACE
  Xen_define_typed_procedure("gsl-eigenvectors", g_gsl_eigenvectors_w, 1, 0, 0, "returns eigenvalues and eigenvectors", pl_pf);
#endif

#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS
  Xen_define_typed_procedure("gsl-roots",  g_gsl_roots_w,  1, 0, 0, H_gsl_roots, NULL);
#endif
#endif

#if HAVE_SCHEME && WITH_GMP
  s7_define_typed_function(s7, "bignum-fft", bignum_fft, 3, 1, false, H_bignum_fft, s7_make_signature(s7, 5, b, v, v, i, i));
#endif

  g_init_base();
  g_init_utils();
  g_init_marks();
  g_init_regions();
  g_init_selection();
  g_init_mix();
  g_init_fft(); /* needs to precede snd-chn init */
  g_init_chn();
  g_init_kbd();
  g_init_sig();
  g_init_print();
  g_init_edits();
  g_init_listener();
  g_init_help();
  g_init_menu();
  g_init_main();
  g_init_snd();
  g_init_dac(); /* needs to follow snd and mix */
  g_init_file();
  g_init_data();
  g_init_env();
  g_init_find();
#if (!USE_NO_GUI)
  g_init_gxcolormaps();
  g_init_draw();
  g_init_axis();
#if USE_MOTIF
  g_init_motif();
#else
  g_init_gxfile();
  g_init_gxdraw();
  g_init_gxenv();
  g_init_gxmenu();
  g_init_gxlistener();
  g_init_gxchn();
  g_init_gxregion();
  g_init_gxsnd();
  g_init_gxfind();
#endif
#endif

#if HAVE_SCHEME && (!_MSC_VER)
  Xen_define_typed_procedure("dlopen",  g_dlopen_w,  1, 1 ,0, H_dlopen,   s7_make_signature(s7, 3, t, s, i));
  Xen_define_typed_procedure("dlclose", g_dlclose_w, 1, 0 ,0, H_dlclose,  s7_make_signature(s7, 2, i, t));
  Xen_define_typed_procedure("dlerror", g_dlerror_w, 0, 0 ,0, H_dlerror,  s7_make_signature(s7, 1, s));
  Xen_define_typed_procedure("dlinit",  g_dlinit_w,  2, 0 ,0, H_dlinit,   s7_make_signature(s7, 3, b, t, s));
  Xen_define_typed_procedure("dlsym",   g_dlsym_w,   2, 0 ,0, H_dlsym,    s7_make_signature(s7, 3, t, t, s));

  Xen_define_constant("RTLD_LAZY", RTLD_LAZY, "dlopen flag");
  Xen_define_constant("RTLD_NOW", RTLD_NOW, "dlopen flag");
  Xen_define_constant("RTLD_GLOBAL", RTLD_GLOBAL, "dlopen flag");
#endif

#if HAVE_LADSPA && HAVE_EXTENSION_LANGUAGE
  g_ladspa_to_snd();
#endif

#ifdef SCRIPTS_DIR
  Xen_add_to_load_path((char *)SCRIPTS_DIR);
#endif

  { 
    char *pwd, *legal_pwd; 
    pwd = mus_getcwd(); 
    legal_pwd = legalize_path(pwd);
    Xen_add_to_load_path(legal_pwd); 
    free(legal_pwd); 
  } 

#if HAVE_SCHEME
  Xen_define_typed_procedure("_snd_s7_error_handler_", g_snd_s7_error_handler_w,  1, 0, 0, "internal error redirection for snd/s7",
			     s7_make_signature(s7, 2, b, s));

  Xen_eval_C_string("(define redo-edit redo)");        /* consistency with Ruby */
  Xen_eval_C_string("(define undo-edit undo)");
  
  /* Xen_eval_C_string("(define (procedure-name proc) (if (procedure? proc) (format #f \"~A\" proc) #f))"); */
  /* needed in snd-test.scm and hooks.scm */

  Xen_eval_C_string("\
    (define* (apropos name (port #f) (e (rootlet)))  \
      \"(apropos name (port *stdout*) (env (rootlet))) looks for 'name' as a part of any symbol name, and sends matches to 'port'\"  \
      (let ((ap-name (if (string? name)   \
		         name   \
		         (if (symbol? name)   \
			     (symbol->string name)  \
			     (error 'wrong-type-arg \"apropos argument 1 should be a string or a symbol\"))))  \
	    (ap-env (if (let? e)   \
		        e   \
		        (error 'wrong-type-arg \"apropos argument 3 should be an environment\")))  \
	    (ap-port (if (or (not port) (output-port? port))   \
		         port  \
                         (error 'wrong-type-arg \"apropos argument 2 should be an output port\"))))  \
        (for-each  \
         (lambda (binding)  \
           (if (and (pair? binding)  \
		    (string-position ap-name (symbol->string (car binding))))  \
	       (snd-print \
                 (format ap-port \"~%~A: ~A\"   \
		         (car binding)   \
		         (if (procedure? (cdr binding))  \
		             (documentation (cdr binding))  \
		             (cdr binding))))))  \
         ap-env) \
         #f))");

  Xen_eval_C_string("\
(define break-ok #f)\
(define break-exit #f)  ; a kludge to get 2 funcs to share a local variable\n\
(define break-enter #f)\
\
(let ((saved-listener-prompt (listener-prompt)))\
  (set! break-exit (lambda ()\
		     (hook-clear read-hook)\
		     (set! (listener-prompt) saved-listener-prompt)\
		     #f))\
  (set! break-enter (lambda ()\
		      (set! saved-listener-prompt (listener-prompt)))))\
\
(define-macro (break)\
  `(let ((__break__ (curlet)))\
     (break-enter)\
     (set! (listener-prompt) (format #f \"~A>\" (if (defined? __func__) __func__ 'break)))\
     (call/cc\
      (lambda (return)\
	(set! break-ok return)      ; save current program loc so (break-ok) continues from the break\n\
	(hook-push read-hook        ; anything typed in the listener is evaluated in the environment of the break call\n\
		   (lambda (str)\
		     (eval-string str __break__)))\
	(error 'snd-top-level)))    ; jump back to the top level\n\
     (break-exit)))                 ; we get here if break-ok is called\n\
");

#endif

#if HAVE_FORTH
  Xen_eval_C_string("<'> redo alias redo-edit");        /* consistency with Ruby */ 
  Xen_eval_C_string("<'> undo alias undo-edit"); 
  Xen_eval_C_string(": clm-print ( fmt :optional args -- ) fth-format snd-print drop ;"); 
#endif

#if HAVE_RUBY
  Xen_eval_C_string("def clm_print(str, *args)\n\
                      snd_print format(str, *args)\n\
                      end");
#endif

#if HAVE_GL
  Xen_define_typed_procedure("snd-gl-context", g_snd_gl_context_w, 0, 0, 0, "GL Context", s7_make_signature(s7, 1, p));
#endif

#if HAVE_EXTENSION_LANGUAGE
#if USE_MOTIF
#if HAVE_SCHEME
  {
    s7_pointer motif, old_shadow;
    s7_define_constant(s7, "*motif*", motif = s7_inlet(s7, s7_nil(s7)));
    old_shadow = s7_shadow_rootlet(s7);
    s7_set_shadow_rootlet(s7, motif);
    Init_libxm(s7);
    s7_set_shadow_rootlet(s7, old_shadow);
  }
#else
  Init_libxm();
#endif
#if HAVE_FORTH
  fth_add_loaded_files("libxm.so");
#endif
#endif

#if USE_GTK
  #if HAVE_SCHEME
  {
    s7_pointer gtk, old_curlet;
    s7_define_constant(s7, "*gtk*", gtk = s7_inlet(s7, s7_nil(s7)));
    old_curlet = s7_set_curlet(s7, gtk);
    libgtk_s7_init(s7);
    s7_set_curlet(s7, old_curlet);
  }
  #else
    Init_libxg();
  #endif
#if HAVE_FORTH
  fth_add_loaded_files("libxg.so");
#endif
#endif

#if HAVE_GL
#if HAVE_SCHEME
  {
    s7_pointer gl, old_shadow;
    s7_define_constant(s7, "*gl*", gl = s7_inlet(s7, s7_nil(s7)));
    old_shadow = s7_shadow_rootlet(s7);
    s7_set_shadow_rootlet(s7, gl);
    Init_libgl(s7);
    s7_set_shadow_rootlet(s7, old_shadow);
  }
#else
  Init_libgl();
#endif /* s7 */
#endif /* gl */
#endif /* extension language */

#if HAVE_ALSA
  Xen_provide_feature("alsa");
#endif

#if HAVE_OSS
  Xen_provide_feature("oss");
#endif

#if MUS_PULSEAUDIO
  Xen_provide_feature("pulse-audio");
#endif

#if MUS_JACK
  Xen_provide_feature("jack");
#endif

#if HAVE_GSL
  Xen_provide_feature("gsl");
#endif

#if USE_MOTIF
  Xen_provide_feature("snd-motif");
#endif

#if USE_GTK
  Xen_provide_feature("snd-gtk");
#if GTK_CHECK_VERSION(3, 91, 0)
  Xen_provide_feature("gtk4");
#else
  #if GTK_CHECK_VERSION(3, 0, 0)
    Xen_provide_feature("gtk3");
  #else
    Xen_provide_feature("gtk2");
  #endif
#endif
#endif

#if USE_NO_GUI
  Xen_provide_feature("snd-nogui");
#endif

#if HAVE_FORTH
  Xen_provide_feature("snd-forth");
#endif

#if HAVE_SCHEME
  Xen_provide_feature("snd-s7");
#endif

#if WITH_AUDIO
  Xen_provide_feature("audio");
#endif

#if ENABLE_WEBSERVER
  Xen_provide_feature("webserver");
#endif

#if HAVE_RUBY
  Xen_provide_feature("snd-ruby");
  /* we need to set up the search path so that load and require will work as in the program irb */
  {
    Xen paths;
    int i, len;
    paths = rb_gv_get("$:");
    /* this is printed as 
     *   ["/home/bil/ruby-snd", "/usr/local/share/snd", "/usr/local/lib/ruby/site_ruby/2.0.0", ...]
     */
    len = Xen_vector_length(paths);
    for (i = 0; i < len; i++)
      Xen_add_to_load_path(Xen_string_to_C_string(Xen_vector_ref(paths, i)));
  }
#endif

  Xen_provide_feature("snd");
  Xen_provide_feature("snd" SND_MAJOR_VERSION);
  Xen_provide_feature("snd-" SND_MAJOR_VERSION "." SND_MINOR_VERSION);

#if HAVE_RUBY
  rb_gc_enable();
#endif

}

