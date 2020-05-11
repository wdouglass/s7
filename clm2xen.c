/* tie CLM module into Scheme, Ruby, or Forth */

/* if the optimizer stops working inexplicably, look for any symbols used before this that
 *    might shadow a generator name; one such case was (make-hook 'env...) in snd-env.c
 *
 * (env env) is accepted by the optimizer in error
 */

#include "mus-config.h"

#if USE_SND
  #include "snd.h"
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdarg.h>

#ifndef _MSC_VER
  #include <unistd.h>
#else
  #include <io.h>
  #pragma warning(disable: 4244)
#endif

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"
#include "clm2xen.h"
#include "clm-strings.h"

#ifndef TWO_PI
  #define TWO_PI (2.0 * M_PI)
#endif

#ifndef PROC_FALSE
  #if HAVE_RUBY
    #define PROC_FALSE "false"
    #define PROC_TRUE "true"
  #else
    #define PROC_FALSE "#f"
    #define PROC_TRUE  "#t"
  #endif
#endif

static Xen xen_float_zero;


/* -------------------------------------------------------------------------------- */
#if HAVE_SCHEME
static bool mus_simple_out_any_to_file(mus_long_t samp, mus_float_t val, int chan, mus_any *IO)
{
  rdout *gen = (rdout *)IO;
  if ((chan < gen->chans) &&
      (samp <= gen->data_end) &&
      (samp >= gen->data_start))
    {
      gen->obufs[chan][samp - gen->data_start] += val;
      if (samp > gen->out_end) 
	gen->out_end = samp;
      return(true);
    }
  return(false);
}
#endif

/* -------------------------------------------------------------------------------- */

struct mus_xen {
  mus_any *gen;
  int nvcts;
  Xen *vcts; /* one for each accessible mus_float_t array (wrapped up here in a vct) */
  struct mus_xen *next;
};


enum {MUS_DATA_WRAPPER, MUS_INPUT_FUNCTION, MUS_ANALYZE_FUNCTION, MUS_EDIT_FUNCTION, MUS_SYNTHESIZE_FUNCTION,
      MUS_SELF_WRAPPER, MUS_INPUT_DATA, MUS_MAX_VCTS}; /* order matters, stuff before self_wrapper is GC marked */

static mus_xen *mx_free_lists[9] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

static mus_xen *mx_alloc(int vcts)
{
  mus_xen *p;
  if (mx_free_lists[vcts])
    {
      p = mx_free_lists[vcts];
      mx_free_lists[vcts] = p->next;
      return(p);
    }
  p = (mus_xen *)malloc(sizeof(mus_xen));
  p->nvcts = vcts;
  if (vcts > 0)
    p->vcts = (Xen *)malloc(vcts * sizeof(Xen));
  else p->vcts = NULL;
  return(p);
}


static void mx_free(mus_xen *p)
{
  p->next = mx_free_lists[p->nvcts];
  mx_free_lists[p->nvcts] = p;
}


mus_any *mus_xen_gen(mus_xen *x) {return(x->gen);}
#define mus_xen_to_mus_any(Gn) (((mus_xen *)Gn)->gen)

#if (!HAVE_SCHEME)
#define XEN_NULL 0

#define Xen_real_to_C_double_if_bound(Xen_Arg, C_Val, Caller, ArgNum) \
  if (Xen_is_bound(Xen_Arg)) {if (Xen_is_number(Xen_Arg)) C_Val = Xen_real_to_C_double(Xen_Arg); else Xen_check_type(false, Xen_Arg, ArgNum, Caller, "a number");}

#define Xen_to_C_double_or_error(Xen_Arg, C_Val, Caller, ArgNum) \
  do {C_Val = 0.0; if (Xen_is_number(Xen_Arg)) C_Val = Xen_real_to_C_double(Xen_Arg); else Xen_check_type(false, Xen_Arg, ArgNum, Caller, "a number");} while (0)

#define Xen_real_to_C_double_with_caller(Xen_Arg, Caller) Xen_real_to_C_double(Xen_Arg)

#define Xen_to_C_integer_or_error(Xen_Arg, C_Val, Caller, ArgNum) \
  do {if (Xen_is_integer(Xen_Arg)) C_Val = Xen_integer_to_C_int(Xen_Arg); else {C_Val = 0.0; Xen_check_type(false, Xen_Arg, ArgNum, Caller, "an integer");}} while (0)

#if (HAVE_FORTH) || (HAVE_RUBY)
  #define Xen_object_ref_checked(Obj, Type) (Xen_c_object_is_type(Obj, Type) ? Xen_object_ref(Obj) : NULL)
#else
  #define Xen_object_ref_checked(Obj, Type) NULL
#endif

#else
#define Xen_real_to_C_double_if_bound(Xen_Arg, C_Val, Caller, ArgNum) if (Xen_is_bound(Xen_Arg)) C_Val = (double)s7_number_to_real_with_caller(s7, Xen_Arg, Caller)
#define Xen_to_C_double_or_error(Xen_Arg, C_Val, Caller, ArgNum) C_Val = (double)s7_number_to_real_with_caller(s7, Xen_Arg, Caller)
#define Xen_real_to_C_double_with_caller(Xen_Arg, Caller) s7_number_to_real_with_caller(s7, Xen_Arg, Caller)

#define Xen_to_C_integer_or_error(Xen_Arg, C_Val, Caller, ArgNum) \
  do {if (s7_is_integer(Xen_Arg)) C_Val = s7_integer(Xen_Arg); else {C_Val = 0.0; Xen_check_type(false, Xen_Arg, ArgNum, Caller, "an integer");}} while (0)

#define Xen_object_ref_checked(Obj, Type) s7_c_object_value_checked(Obj, Type)
#define XEN_NULL NULL
#endif


static int local_error_type = MUS_NO_ERROR;
static char *local_error_msg = NULL;

static void local_mus_error(int type, char *msg)
{
  local_error_type = type;
  if (local_error_msg) free(local_error_msg);
  local_error_msg = mus_strdup(msg);
}


static Xen clm_mus_error(int type, const char *msg, const char *caller)
{
  /* mus_error returns an int, which is a bother in this context */
  mus_error(type, "%s: %s", caller, msg);
  return(Xen_false);
}


#if HAVE_SCHEME
static s7_pointer mus_error_symbol, clm_error_info, clm_err1, clm_err2, clm_err3;
#define CLM_ERROR mus_error_symbol
static void clm_error(const char *caller, const char *msg, Xen val)
{
  s7_set_car(clm_err1, s7_make_string_wrapper(s7, caller));
  s7_set_car(clm_err2, s7_make_string_wrapper(s7, msg));
  s7_set_car(clm_err3, val);
  s7_error(s7, mus_error_symbol, clm_error_info);
}
#else

#define CLM_ERROR Xen_make_error_type("mus-error")

static void clm_error(const char *caller, const char *msg, Xen val)
{
  Xen_error(CLM_ERROR,
	    Xen_list_4(C_string_to_Xen_string("~A: ~A ~A"),
		       C_string_to_Xen_string(caller),
		       C_string_to_Xen_string(msg),
		       val));
}
#endif



/* ---------------- optional-key ---------------- */
#if HAVE_SCHEME
  static s7_pointer extra_args_string;
#endif

int mus_optkey_unscramble(const char *caller, int num_args, int nkeys, Xen *keys, Xen *args, int *orig)
{
  /* implement the &optional-key notion in CLM */
  /* "keys" holds the keywords the calling function accepts, 
   *   upon return, if a key was given in the arglist or its position had a value, the corresponding value is in its keys location
   * "nkeys is the size of "keys"
   * "args" contains the original arguments passed to the function in order
   *   it should be of size nkeys * 2, and any trailing (unspecified) args should be Xen_undefined
   * "orig" should be of size nkeys, and will contain upon return the 1-based location of the original keyword value argument
   *  (it is intended for error reports)
   */
  int arg_ctr = 0, key_start = 0, rtn_ctr = 0, end;
  bool keying = false;
  end = num_args - 1;

  while (arg_ctr < num_args)
    {
      Xen key;
      key = args[arg_ctr];
      if (!(Xen_is_keyword(key)))
	{
	  if (arg_ctr >= nkeys) /* we aren't handling a keyword arg, so the underlying args should only take nkeys args */
#if HAVE_SCHEME
	    {
	      s7_set_car(clm_err1, s7_make_string_wrapper(s7, caller));
	      s7_set_car(clm_err2, extra_args_string);
	      s7_set_car(clm_err3, key);
	      s7_error(s7, mus_error_symbol, clm_error_info);
	    }
#else
	    clm_error(caller, "extra trailing args?", key);
#endif
	  if (keying) 
	    clm_error(caller, "unmatched value within keyword section?", key);
	  /* type checking on the actual values has to be the caller's problem */

	  keys[arg_ctr] = key;
	  orig[arg_ctr] = arg_ctr + 1;
	  arg_ctr++;
	  key_start = arg_ctr;
	  rtn_ctr++;
	}
      else
	{
	  int i;
	  Xen val;
	  bool key_found;
	  if (arg_ctr >= end)
	    clm_error(caller, "keyword without value?", key);

	  val = args[arg_ctr + 1];
	  if (Xen_is_keyword(val))
	    clm_error(caller, "two keywords in a row?", key);

	  keying = true;
	  key_found = false;
	  for (i = key_start; i < nkeys; i++)
	    {
	      if (Xen_keyword_is_eq(keys[i], key))
		{
		  keys[i] = val;
		  arg_ctr += 2;
		  orig[i] = arg_ctr;
		  rtn_ctr++;
		  key_found = true;
		  break;
		}
	    }

	  if (!key_found)
	    {
	      /* either there's a redundant keyword pair or a keyword that 'caller' doesn't recognize */
	      clm_error(caller, "redundant or invalid key found", key);
	      /* normally (all local cases) the error returns */
	      arg_ctr += 2;
	    }
	}
    }
  return(rtn_ctr);
}

#if (!HAVE_SCHEME)
static mus_float_t optkey_float_error(Xen key, int n, const char *caller)
{
  Xen_check_type(false, key, n, caller, "a number");
  return(0.0);
}

#define Xen_optkey_to_float(Original_key, Key, Caller, N, Def) \
  ((Xen_keyword_is_eq(Original_key, Key)) ? Def : ((Xen_is_number(Key)) ? Xen_real_to_C_double(Key) : optkey_float_error(Key, N, Caller)))
#endif

mus_float_t mus_optkey_to_float(Xen key, const char *caller, int n, mus_float_t def)
{
  if (Xen_is_number(key))
    return(Xen_real_to_C_double(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "a number");
  return(def);
}

#if (!HAVE_SCHEME)
static int optkey_int_error(Xen key, int n, const char *caller)
{
  Xen_check_type(false, key, n, caller, "an integer");
  return(0);
}

#define Xen_optkey_to_int(Original_key, Key, Caller, N, Def) \
  ((Xen_keyword_is_eq(Original_key, Key)) ? Def : ((Xen_is_integer(Key)) ? Xen_integer_to_C_int(Key) : optkey_int_error(Key, N, Caller)))
#endif

int mus_optkey_to_int(Xen key, const char *caller, int n, int def)
{
  if (Xen_is_integer(key))
    return(Xen_integer_to_C_int(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "an integer");
  return(def);
}


bool mus_optkey_to_bool(Xen key, const char *caller, int n, bool def)
{
  if (Xen_is_boolean(key))
    return(Xen_boolean_to_C_bool(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "#f or #t");
  return(def);
}

#if (!HAVE_SCHEME)
static mus_long_t optkey_llong_error(Xen key, int n, const char *caller)
{
  Xen_check_type(false, key, n, caller, "an integer");
  return(0);
}
#endif

#define Xen_optkey_to_mus_long_t(Original_key, Key, Caller, N, Def) \
  ((Xen_keyword_is_eq(Original_key, Key)) ? Def : ((Xen_is_integer(Key)) ? Xen_llong_to_C_llong(Key) : optkey_llong_error(Key, N, Caller)))

mus_long_t mus_optkey_to_mus_long_t(Xen key, const char *caller, int n, mus_long_t def)
{
  if (Xen_is_integer(key))
    return(Xen_llong_to_C_llong(key));
  if (!(Xen_is_keyword(key)))
    Xen_check_type(false, key, n, caller, "a sample number or size");
  return(def);
}


const char *mus_optkey_to_string(Xen key, const char *caller, int n, char *def)
{
  if (Xen_is_string(key))
    return(Xen_string_to_C_string(key));
  if ((!(Xen_is_keyword(key))) && (!(Xen_is_false(key))))
    Xen_check_type(false, key, n, caller, "a string");
  return(def);
}

#if (!HAVE_SCHEME)
static vct *mus_optkey_to_vct(Xen key, const char *caller, int n, vct *def)
{
  if (mus_is_vct(key))
    return(Xen_to_vct(key));
  if ((!(Xen_is_keyword(key))) && (!(Xen_is_false(key))))
    Xen_check_type(false, key, n, caller, "a " S_vct);
  return(def);
}
#endif

static bool local_arity_ok(Xen proc, int args) /* from snd-xen.c minus (inconvenient) gc protection */
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


Xen mus_optkey_to_procedure(Xen key, const char *caller, int n, Xen def, int required_args, const char *err)
{
  /* in this case, it's faster to look for the keyword first */
  if ((!(Xen_is_keyword(key))) && 
      (!(Xen_is_false(key))))
    {
      Xen_check_type(Xen_is_procedure(key), key, n, caller, "a procedure");
      if (!(local_arity_ok(key, required_args)))
	Xen_bad_arity_error(caller, n, key, err);
      return(key);
    }
  return(def);
}



/* ---------------- clm keywords ---------------- */
#if HAVE_SCHEME
static s7_pointer kw_frequency, kw_radius, kw_readable;
static void init_keywords(void)
{
  kw_frequency = Xen_make_keyword("frequency");
  kw_radius = Xen_make_keyword("radius");
  kw_readable = Xen_make_keyword("readable");
}
#else

static Xen kw_frequency, kw_initial_phase, kw_wave, kw_amplitude,
  kw_r, kw_ratio, kw_size, kw_a0, kw_a1, kw_a2, kw_b1, kw_b2, kw_max_size,
  kw_input, kw_srate, kw_file, kw_channel, kw_start,
  kw_initial_contents, kw_initial_element, kw_scaler, kw_feedforward, kw_feedback,
  kw_radius, kw_partials, kw_a, kw_n,
  kw_order, kw_x_coeffs, kw_y_coeffs, kw_envelope, kw_base, kw_duration, kw_offset, kw_end,
  kw_direction, kw_degree, kw_distance, kw_reverb, kw_output, kw_fft_size,
  kw_expansion, kw_length, kw_hop, kw_ramp, kw_jitter,
  kw_type, kw_channels, kw_filter, kw_revout, kw_width,
  kw_edit, kw_synthesize, kw_analyze, kw_interp, kw_overlap, kw_pitch,
  kw_distribution, kw_coeffs, kw_kind;


static void init_keywords(void)
{
  /* in Ruby there's rb_intern of the symbol -- is it safe? */
  kw_frequency =        Xen_make_keyword("frequency");
  kw_initial_phase =    Xen_make_keyword("initial-phase");
  kw_wave =             Xen_make_keyword("wave");
  kw_amplitude =        Xen_make_keyword("amplitude");
  kw_r =                Xen_make_keyword("r");
  kw_ratio =            Xen_make_keyword("ratio");
  kw_size =             Xen_make_keyword("size");
  kw_a0 =               Xen_make_keyword("a0");
  kw_a1 =               Xen_make_keyword("a1");
  kw_a2 =               Xen_make_keyword("a2");
  kw_b1 =               Xen_make_keyword("b1");
  kw_b2 =               Xen_make_keyword("b2");
  kw_max_size =         Xen_make_keyword("max-size");
  kw_input =            Xen_make_keyword("input");
  kw_srate =            Xen_make_keyword("srate");
  kw_file =             Xen_make_keyword("file");
  kw_channel =          Xen_make_keyword("channel");
  kw_start =            Xen_make_keyword("start");  /* make-readin */
  kw_initial_contents = Xen_make_keyword("initial-contents");
  kw_initial_element =  Xen_make_keyword("initial-element");
  kw_scaler =           Xen_make_keyword("scaler");
  kw_feedforward =      Xen_make_keyword("feedforward");
  kw_feedback =         Xen_make_keyword("feedback");
  kw_radius =           Xen_make_keyword("radius");
  kw_partials =         Xen_make_keyword("partials");
  kw_a =                Xen_make_keyword("a");
  kw_n =                Xen_make_keyword("n");
  kw_order =            Xen_make_keyword("order");
  kw_x_coeffs =         Xen_make_keyword("xcoeffs");
  kw_y_coeffs =         Xen_make_keyword("ycoeffs");
  kw_envelope =         Xen_make_keyword("envelope");
  kw_base =             Xen_make_keyword("base");
  kw_duration =         Xen_make_keyword("duration");
  kw_offset =           Xen_make_keyword("offset");
  kw_end =              Xen_make_keyword("end");
  kw_direction =        Xen_make_keyword("direction");
  kw_degree =           Xen_make_keyword("degree");
  kw_distance =         Xen_make_keyword("distance");
  kw_reverb =           Xen_make_keyword("reverb");
  kw_output =           Xen_make_keyword("output");
  kw_fft_size =         Xen_make_keyword("fft-size");
  kw_expansion =        Xen_make_keyword("expansion");
  kw_length =           Xen_make_keyword("length");
  kw_hop =              Xen_make_keyword("hop");
  kw_ramp =             Xen_make_keyword("ramp");
  kw_jitter =           Xen_make_keyword("jitter");
  kw_type =             Xen_make_keyword("type");
  kw_channels =         Xen_make_keyword("channels");
  kw_filter =           Xen_make_keyword("filter");
  kw_revout =           Xen_make_keyword("revout");
  kw_width =            Xen_make_keyword("width");
  kw_edit =             Xen_make_keyword("edit");
  kw_synthesize =       Xen_make_keyword("synthesize");
  kw_analyze =          Xen_make_keyword("analyze");
  kw_interp =           Xen_make_keyword("interp");
  kw_overlap =          Xen_make_keyword("overlap");
  kw_pitch =            Xen_make_keyword("pitch");
  kw_distribution =     Xen_make_keyword("distribution");
  kw_coeffs =           Xen_make_keyword("coeffs");
  kw_kind =             Xen_make_keyword("kind");
}
#endif


/* ---------------- *clm-table-size* ---------------- */

static mus_long_t clm_table_size = MUS_CLM_DEFAULT_TABLE_SIZE;
#if HAVE_SCHEME
  static s7_pointer clm_table_size_symbol;
#endif

mus_long_t clm_default_table_size_c(void) {return(clm_table_size);}

static Xen g_clm_table_size(void) {return(C_llong_to_Xen_llong(clm_table_size));}

static Xen g_set_clm_table_size(Xen val) 
{
  mus_long_t size;
  #define H_clm_table_size "(" S_clm_table_size "): the default table size for most generators (512)"
  Xen_check_type(Xen_is_llong(val), val, 1, S_set S_clm_table_size, "an integer");
  size = Xen_llong_to_C_llong(val);
  if ((size <= 0) || 
      (size > mus_max_table_size()))
    Xen_out_of_range_error(S_set S_clm_table_size, 1, val, "invalid size (see mus-max-table-size)");
  clm_table_size = size;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, clm_table_size_symbol, s7_make_integer(s7, clm_table_size));
#endif
  return(C_llong_to_Xen_llong(clm_table_size));
}


/* ---------------- AM and simple stuff ---------------- */

static const char *fft_window_xen_names[MUS_NUM_FFT_WINDOWS] = 
    {S_rectangular_window, S_hann_window, S_welch_window, S_parzen_window, S_bartlett_window,
     S_hamming_window, S_blackman2_window, S_blackman3_window, S_blackman4_window,
     S_exponential_window, S_riemann_window, S_kaiser_window, S_cauchy_window,
     S_poisson_window, S_gaussian_window, S_tukey_window, S_dolph_chebyshev_window,
     S_hann_poisson_window, S_connes_window, S_samaraki_window, S_ultraspherical_window,
     S_bartlett_hann_window, S_bohman_window, S_flat_top_window,
     S_blackman5_window, S_blackman6_window, S_blackman7_window, S_blackman8_window, S_blackman9_window, S_blackman10_window,
     S_rv2_window, S_rv3_window, S_rv4_window, S_mlt_sine_window, S_papoulis_window, S_dpss_window, S_sinc_window
};


const char *mus_fft_window_xen_name(mus_fft_window_t i) {return(fft_window_xen_names[(int)i]);}


static Xen g_mus_file_buffer_size(void)
{
  #define H_mus_file_buffer_size "(" S_mus_file_buffer_size "): current CLM IO buffer size (default is 8192)"
  return(C_llong_to_Xen_llong(mus_file_buffer_size()));
}


#if HAVE_SCHEME
  static s7_pointer mus_file_buffer_size_symbol;
#endif

static Xen g_mus_set_file_buffer_size(Xen val)
{
  mus_long_t len;
  Xen_check_type(Xen_is_llong(val), val, 1, S_set S_mus_file_buffer_size, "an integer");
  len = Xen_llong_to_C_llong(val);
  if (len <= 0) 
    Xen_out_of_range_error(S_set S_mus_file_buffer_size, 1, val, "must be > 0");
  mus_set_file_buffer_size(len);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, mus_file_buffer_size_symbol, s7_make_integer(s7, len));
#endif
  return(val);
}


static Xen g_radians_to_hz(Xen val) 
{
  #define H_radians_to_hz "(" S_radians_to_hz " rads): convert radians per sample to frequency in Hz: rads * srate / (2 * pi)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_radians_to_hz, 1);
  return(C_double_to_Xen_real(mus_radians_to_hz(x)));
}


static Xen g_hz_to_radians(Xen val) 
{
  #define H_hz_to_radians "(" S_hz_to_radians " hz): convert frequency in Hz to radians per sample: hz * 2 * pi / srate"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_hz_to_radians, 1);
  return(C_double_to_Xen_real(mus_hz_to_radians(x)));
}


static Xen g_radians_to_degrees(Xen val) 
{
  #define H_radians_to_degrees "(" S_radians_to_degrees " rads): convert radians to degrees: rads * 360 / (2 * pi)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_radians_to_degrees, 1);
  return(C_double_to_Xen_real(mus_radians_to_degrees(x)));
}


static Xen g_degrees_to_radians(Xen val) 
{
  #define H_degrees_to_radians "(" S_degrees_to_radians " deg): convert degrees to radians: deg * 2 * pi / 360"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_degrees_to_radians, 1);
  return(C_double_to_Xen_real(mus_degrees_to_radians(x)));
}


static Xen g_db_to_linear(Xen val) 
{
  #define H_db_to_linear "(" S_db_to_linear " db): convert decibel value db to linear value: pow(10, db / 20)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_db_to_linear, 1);
  return(C_double_to_Xen_real(mus_db_to_linear(x)));
}


static Xen g_linear_to_db(Xen val) 
{
  #define H_linear_to_db "(" S_linear_to_db " lin): convert linear value to decibels: 20 * log10(lin)"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_linear_to_db, 1);
  return(C_double_to_Xen_real(mus_linear_to_db(x)));
}


static Xen g_even_weight(Xen val) 
{
  #define H_even_weight "(" S_even_weight " x): return the even weight of x"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_even_weight, 1);
  return(C_double_to_Xen_real(mus_even_weight(x)));
}


static Xen g_odd_weight(Xen val) 
{
  #define H_odd_weight "(" S_odd_weight " x): return the odd weight of x"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_odd_weight, 1);
  return(C_double_to_Xen_real(mus_odd_weight(x)));
}


static Xen g_even_multiple(Xen val1, Xen val2) 
{
  #define H_even_multiple "(" S_even_multiple " x y): return the even multiple of x and y"
  mus_float_t x, y;
  Xen_to_C_double_or_error(val1, x, S_even_multiple, 1);
  Xen_to_C_double_or_error(val2, y, S_even_multiple, 2);
  return(C_double_to_Xen_real(mus_even_multiple(x, y)));
}


static Xen g_odd_multiple(Xen val1, Xen val2) 
{
  #define H_odd_multiple "(" S_odd_multiple " x y): return the odd multiple of x and y"
  mus_float_t x, y;
  Xen_to_C_double_or_error(val1, x, S_odd_multiple, 1);
  Xen_to_C_double_or_error(val2, y, S_odd_multiple, 2);
  return(C_double_to_Xen_real(mus_odd_multiple(x, y)));
}


static Xen g_seconds_to_samples(Xen val) 
{
  #define H_seconds_to_samples "(" S_seconds_to_samples " secs): use " S_mus_srate " to convert seconds to samples"
  mus_float_t x;
  Xen_to_C_double_or_error(val, x, S_seconds_to_samples, 1);
  return(C_llong_to_Xen_llong(mus_seconds_to_samples(x)));
}


static Xen g_samples_to_seconds(Xen val) 
{
  #define H_samples_to_seconds "(" S_samples_to_seconds " samps): use " S_mus_srate " to convert samples to seconds"
  Xen_check_type(Xen_is_llong(val), val, 1, S_samples_to_seconds, "a number");
  return(C_double_to_Xen_real(mus_samples_to_seconds(Xen_llong_to_C_llong(val))));
}


#if HAVE_SCHEME
  static s7_pointer clm_srate_symbol;
#endif

static Xen g_mus_srate(void) 
{
  #define H_mus_srate "(" S_mus_srate "): current sampling rate"
  return(C_double_to_Xen_real(mus_srate()));
}


static Xen g_mus_set_srate(Xen val) 
{
  mus_float_t sr;
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_mus_srate, "a number");
  sr = Xen_real_to_C_double(val);
  if (sr != mus_srate())
    {
      if (sr <= 0.0) 
	Xen_out_of_range_error(S_set S_mus_srate, 1, val, "must be > 0.0");
      mus_set_srate(sr);
#if HAVE_SCHEME
      s7_symbol_set_value(s7, clm_srate_symbol, s7_make_real(s7, sr));
#endif
    }
  return(val);
}


#if HAVE_SCHEME
  static s7_pointer mus_float_equal_fudge_factor_symbol;
#endif

static Xen g_mus_float_equal_fudge_factor(void) 
{
  #define H_mus_float_equal_fudge_factor "(" S_mus_float_equal_fudge_factor "): floating point equality fudge factor"
  return(C_double_to_Xen_real(mus_float_equal_fudge_factor()));
}


static Xen g_mus_set_float_equal_fudge_factor(Xen val) 
{
  mus_float_t factor;
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_mus_float_equal_fudge_factor, "a number");
  factor = Xen_real_to_C_double(val);
  if (factor != mus_float_equal_fudge_factor())
    {
      mus_set_float_equal_fudge_factor(factor);
#if HAVE_SCHEME
      s7_symbol_set_value(s7, mus_float_equal_fudge_factor_symbol, s7_make_real(s7, factor));
#endif
    }
  return(val);
}


#if HAVE_SCHEME
  static s7_pointer mus_array_print_length_symbol;
#endif

static Xen g_mus_array_print_length(void) 
{
  #define H_mus_array_print_length "(" S_mus_array_print_length "): current clm array print length (default is 8).  This \
affects error reporting and generator descriptions.  Array (" S_vct ") elements beyond this length are represented by '...'"
  return(C_int_to_Xen_integer(mus_array_print_length()));
}


static Xen g_mus_set_array_print_length(Xen val) 
{
  int len;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mus_array_print_length, "an integer");
  len = Xen_integer_to_C_int(val);
  if (len != mus_array_print_length())
    {
      if (len < 0)
	Xen_out_of_range_error(S_set S_mus_array_print_length, 1, val, "must be >= 0");
      mus_set_array_print_length(len);
#if HAVE_SCHEME
      s7_symbol_set_value(s7, mus_array_print_length_symbol, s7_make_integer(s7, len));
#endif
    }
  return(val);
}


static Xen g_ring_modulate(Xen val1, Xen val2) 
{
  #define H_ring_modulate "(" S_ring_modulate " s1 s2): s1 * s2 (sample by sample multiply)"
  Xen_check_type(Xen_is_number(val1), val1, 1, S_ring_modulate, "a number");
  Xen_check_type(Xen_is_number(val2), val2, 2, S_ring_modulate, "a number");
  return(C_double_to_Xen_real(mus_ring_modulate(Xen_real_to_C_double(val1), Xen_real_to_C_double(val2))));
}


static Xen g_amplitude_modulate(Xen val1, Xen val2, Xen val3) 
{
  #define H_amplitude_modulate "(" S_amplitude_modulate " carrier in1 in2): in1 * (carrier + in2)"
  Xen_check_type(Xen_is_number(val1), val1, 1, S_amplitude_modulate, "a number");
  Xen_check_type(Xen_is_number(val2), val2, 2, S_amplitude_modulate, "a number");
  Xen_check_type(Xen_is_number(val3), val3, 3, S_amplitude_modulate, "a number");
  return(C_double_to_Xen_real(mus_amplitude_modulate(Xen_real_to_C_double(val1), Xen_real_to_C_double(val2), Xen_real_to_C_double(val3))));
}


static Xen g_contrast_enhancement(Xen val1, Xen val2) 
{
  mus_float_t index = 1.0; /* this is the default in clm.html and mus.lisp */
  #define H_contrast_enhancement "(" S_contrast_enhancement " sig (index 1.0)): sin(sig * pi / 2 + index * sin(sig * 2 * pi))"
  Xen_check_type(Xen_is_number(val1), val1, 1, S_contrast_enhancement, "a number");
  if (Xen_is_bound(val2))
    {
      Xen_check_type(Xen_is_number(val2), val2, 2, S_contrast_enhancement, "a number");
      index = Xen_real_to_C_double(val2);
    }
  return(C_double_to_Xen_real(mus_contrast_enhancement(Xen_real_to_C_double(val1), index)));
}


static Xen g_dot_product(Xen val1, Xen val2, Xen size) 
{
  #define H_dot_product "(" S_dot_product " v1 v2 (size)): sum of v1[i] * v2[i] (also named scalar product)"
  vct *v1, *v2;
  mus_long_t len;  

  Xen_check_type(mus_is_vct(val1), val1, 1, S_dot_product, "a " S_vct);
  Xen_check_type(mus_is_vct(val2), val2, 2, S_dot_product, "a " S_vct);
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 3, S_dot_product, "an integer");

  v1 = Xen_to_vct(val1);
  v2 = Xen_to_vct(val2);
  if (Xen_is_llong(size))
    {
      len = Xen_llong_to_C_llong(size);
      if (len == 0) return(C_double_to_Xen_real(0.0));
      if (len < 0)
	Xen_out_of_range_error(S_dot_product, 3, size, "size < 0?");
      if (len > mus_vct_length(v1)) len = mus_vct_length(v1);
    }
  else len = mus_vct_length(v1); 
  if (len > mus_vct_length(v2)) len = mus_vct_length(v2);

  return(C_double_to_Xen_real(mus_dot_product(mus_vct_data(v1), mus_vct_data(v2), len)));
}


#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS && (!HAVE_RUBY)

#if defined(__sun) && defined(__SVR4)
  #undef _Complex_I
  #define _Complex_I 1.0fi
#endif

#define S_edot_product "edot-product"

static Xen g_edot_product(Xen val1, Xen val2) 
{
  #define H_edot_product "(" S_edot_product " freq data): sum of (e^freq*i) * data[i]"
  mus_long_t i, len;
  vct *v = NULL;
  complex double freq;
  complex double *vals;
  Xen result;

  Xen_check_type(Xen_is_complex(val1), val1, 1, S_edot_product, "complex");
  Xen_check_type((mus_is_vct(val2)) || (Xen_is_vector(val2)), val2, 2, S_edot_product, "a " S_vct);

  freq = Xen_complex_to_C_complex(val1);
  if (mus_is_vct(val2))
    {
      v = Xen_to_vct(val2);
      len = mus_vct_length(v);
    }
  else
    {
      len = Xen_vector_length(val2);
    }
  vals = (complex double *)calloc(len, sizeof(complex double));
  if (mus_is_vct(val2))
    {
      mus_float_t *vdata;
      vdata = mus_vct_data(v);
      for (i = 0; i < len; i++)
	vals[i] = vdata[i];
    }
  else
    {
      for (i = 0; i < len; i++)
	vals[i] = Xen_complex_to_C_complex(Xen_vector_ref(val2, i));
    }
  result = C_complex_to_Xen_complex(mus_edot_product(freq, vals, len));
  free(vals);
  return(result);
}
#endif

typedef enum {G_RECTANGULAR_POLAR, G_POLAR_RECTANGULAR, G_RECTANGULAR_MAGNITUDES} xclm_window_t;

static Xen g_fft_window_1(xclm_window_t choice, Xen val1, Xen val2, Xen ulen, const char *caller) 
{
  vct *v1, *v2;
  mus_long_t len;

  Xen_check_type(mus_is_vct(val1), val1, 1, caller, "a " S_vct);
  Xen_check_type(mus_is_vct(val2), val2, 2, caller, "a " S_vct);
  Xen_check_type(Xen_is_llong_or_unbound(ulen), ulen, 3, caller, "an integer");

  v1 = Xen_to_vct(val1);
  v2 = Xen_to_vct(val2);
  if (Xen_is_llong(ulen))
    {
      len = Xen_llong_to_C_llong(ulen);
      if (len == 0) return(Xen_false);
      if (len < 0)
	Xen_out_of_range_error(caller, 3, ulen, "size < 0?");
      if (len > mus_vct_length(v1)) len = mus_vct_length(v1);
    }
  else len = mus_vct_length(v1); 
  if (len > mus_vct_length(v2)) len = mus_vct_length(v2);
  switch (choice)
    {
    case G_RECTANGULAR_POLAR:      mus_rectangular_to_polar(mus_vct_data(v1), mus_vct_data(v2), len);      break;
    case G_RECTANGULAR_MAGNITUDES: mus_rectangular_to_magnitudes(mus_vct_data(v1), mus_vct_data(v2), len); break;
    case G_POLAR_RECTANGULAR:      mus_polar_to_rectangular(mus_vct_data(v1), mus_vct_data(v2), len);      break;
    }
  return(val1);
}


static Xen g_rectangular_to_polar(Xen val1, Xen val2) 
{
  #define H_rectangular_to_polar "(" S_rectangular_to_polar " rl im): convert real/imaginary \
data in " S_vct "s rl and im from rectangular form (fft output) to polar form (a spectrum)"

  return(g_fft_window_1(G_RECTANGULAR_POLAR, val1, val2, Xen_undefined, S_rectangular_to_polar));
}


static Xen g_rectangular_to_magnitudes(Xen val1, Xen val2) 
{
  #define H_rectangular_to_magnitudes "(" S_rectangular_to_magnitudes " rl im): convert real/imaginary \
data in " S_vct "s rl and im from rectangular form (fft output) to polar form, but ignore the phases"

  return(g_fft_window_1(G_RECTANGULAR_MAGNITUDES, val1, val2, Xen_undefined, S_rectangular_to_magnitudes));
}


static Xen g_polar_to_rectangular(Xen val1, Xen val2) 
{
  #define H_polar_to_rectangular "(" S_polar_to_rectangular " rl im): convert real/imaginary \
data in " S_vct "s rl and im from polar (spectrum) to rectangular (fft)"

  return(g_fft_window_1(G_POLAR_RECTANGULAR, val1, val2, Xen_undefined, S_polar_to_rectangular));
}

static Xen g_mus_fft(Xen url, Xen uim, Xen len, Xen usign)
{
  #define H_mus_fft "(" S_mus_fft " rl im (len) (dir 1)): return the fft of " S_vct "s rl and im which contain \
the real and imaginary parts of the data; len should be a power of 2, dir = 1 for fft, -1 for inverse-fft"

  int sign;
  mus_long_t n;
  vct *v1, *v2;

  Xen_check_type((mus_is_vct(url)), url, 1, S_mus_fft, "a " S_vct);
  Xen_check_type((mus_is_vct(uim)), uim, 2, S_mus_fft, "a " S_vct);

  v1 = Xen_to_vct(url);
  v2 = Xen_to_vct(uim);

  if (Xen_is_integer(usign)) 
    sign = Xen_integer_to_C_int(usign); 
  else sign = 1;

  if (Xen_is_llong(len)) 
    {
      n = Xen_llong_to_C_llong(len); 
      if (n <= 0)
	Xen_out_of_range_error(S_mus_fft, 3, len, "size <= 0?");
      if (n > mus_max_malloc())
	Xen_out_of_range_error(S_mus_fft, 3, len, "size too large (see mus-max-malloc)");
      if (n > mus_vct_length(v1))
	n = mus_vct_length(v1);
    }
  else n = mus_vct_length(v1);

  if (n > mus_vct_length(v2))
    n = mus_vct_length(v2);

  if (!(is_power_of_2(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (mus_long_t)pow(2.0, np);
    }

  if (n > 0)
    mus_fft(mus_vct_data(v1), mus_vct_data(v2), n, sign);
  /*
   * in fftw, there's the extra complex array allocation, so for n = 2^29
   *   (and doubles for vcts as well as fftw), we need 24.6 Gbytes, and the FFT
   *   takes 144 secs on a 2.4 GHz machine.  (Similarly, 2^28 needs 12.6 Gb
   *   and takes 61 secs).  
   */

  return(url);
}


static Xen g_make_fft_window(Xen type, Xen size, Xen ubeta, Xen ualpha)
{
  #if HAVE_SCHEME
    #define make_window_example "(" S_make_fft_window " " S_hamming_window " 256)"
  #endif
  #if HAVE_RUBY
    #define make_window_example "make_fft_window(Hamming_window, 256)"
  #endif
  #if HAVE_FORTH
    #define make_window_example "hamming-window 256 make-fft-window"
  #endif

  #define H_make_fft_window "(" S_make_fft_window " type size (beta 0.0) (alpha 0.0)): -> fft data window (a " S_vct "). \
type is one of the sndlib fft window identifiers such as " S_kaiser_window ", beta \
is the window family parameter, if any:\n  " make_window_example

  mus_float_t beta = 0.0, alpha = 0.0;
  mus_long_t n;
  int fft_window;
  mus_float_t *data;

  Xen_check_type(Xen_is_integer(type), type, 1, S_make_fft_window, "an integer (window type)");
  Xen_check_type(Xen_is_llong(size), size, 2, S_make_fft_window, "an integer");

  if (Xen_is_number(ubeta)) beta = Xen_real_to_C_double(ubeta);
  if (Xen_is_number(ualpha)) alpha = Xen_real_to_C_double(ualpha);

  n = Xen_llong_to_C_llong(size);
  if (n <= 0)
    Xen_out_of_range_error(S_make_fft_window, 2, size, "size <= 0?");
  if (n > mus_max_malloc())
    Xen_out_of_range_error(S_make_fft_window, 2, size, "size too large (see mus-max-malloc)");

  fft_window = Xen_integer_to_C_int(type);
  if (!(mus_is_fft_window(fft_window)))
    Xen_out_of_range_error(S_make_fft_window, 1, type, "unknown fft window");

  data = (mus_float_t *)malloc(n * sizeof(mus_float_t));
  mus_make_fft_window_with_window((mus_fft_window_t)fft_window, n, beta, alpha, data);
  return(xen_make_vct(n, data));
}


static Xen g_spectrum(Xen url, Xen uim, Xen uwin, Xen utype)
{
  #define H_mus_spectrum "(" S_spectrum " rl im window (type 1)): \
real and imaginary data in " S_vct "s rl and im, returns (in rl) the spectrum thereof; \
window is the fft data window (a " S_vct " as returned by " S_make_fft_window "), \
and type determines how the spectral data is scaled:\n\
  0 = data in dB,\n\
  1 (default) = linear and normalized\n\
  2 = linear and un-normalized."

  int type;
  mus_long_t n;
  vct *v1, *v2, *v3 = NULL;

  Xen_check_type((mus_is_vct(url)), url, 1, S_spectrum, "a " S_vct);
  Xen_check_type((mus_is_vct(uim)), uim, 2, S_spectrum, "a " S_vct);
  if (!Xen_is_false(uwin)) Xen_check_type((mus_is_vct(uwin)), uwin, 3, S_spectrum, "a " S_vct " or " PROC_FALSE);

  v1 = Xen_to_vct(url);
  v2 = Xen_to_vct(uim);
  if (!Xen_is_false(uwin)) v3 = Xen_to_vct(uwin);

  n = mus_vct_length(v1);
  if (n > mus_vct_length(v2))
    n = mus_vct_length(v2);
  if ((v3) && (n > mus_vct_length(v3)))
    n = mus_vct_length(v3);

  if (!(is_power_of_2(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }

  if (Xen_is_integer(utype)) 
    type = Xen_integer_to_C_int(utype);
  else type = 1; /* linear normalized */
  if ((type < 0) || (type > 2))
    Xen_out_of_range_error(S_spectrum, 4, utype, "type must be 0..2");
  
  if (n > 0)
    mus_spectrum(mus_vct_data(v1), mus_vct_data(v2), (v3) ? (mus_vct_data(v3)) : NULL, n, (mus_spectrum_t)type);
  return(url);
}


static Xen g_autocorrelate(Xen reals)
{
  #define H_autocorrelate "(" S_autocorrelate " data): in place autocorrelation of data (a " S_vct ")"
  /* assumes length is power of 2 */
  vct *v1 = NULL;
  Xen_check_type(mus_is_vct(reals), reals, 1, S_autocorrelate, "a " S_vct);
  v1 = Xen_to_vct(reals);
  if (mus_vct_length(v1) > 0)
    mus_autocorrelate(mus_vct_data(v1), mus_vct_length(v1));
  return(reals);
}


static Xen g_correlate(Xen data1, Xen data2)
{
  #define H_correlate "(" S_correlate " data1 data2): in place cross-correlation of data1 and data2 (both " S_vct "s)"
  mus_long_t size;
  vct *v1 = NULL, *v2 = NULL;

  Xen_check_type(mus_is_vct(data1), data1, 1, S_correlate, "a " S_vct);
  Xen_check_type(mus_is_vct(data2), data2, 2, S_correlate, "a " S_vct);

  v1 = Xen_to_vct(data1);
  v2 = Xen_to_vct(data2);
  if (mus_vct_length(v1) < mus_vct_length(v2))
    size = mus_vct_length(v1);
  else size = mus_vct_length(v2);

  if (size > 0)
    mus_correlate(mus_vct_data(v1), mus_vct_data(v2), size);
  return(data1);
}


static Xen g_convolution(Xen url1, Xen url2, Xen un)
{
  #define H_mus_convolution "(" S_convolution " v1 v2 (len)): convolution \
of " S_vct "s v1 with v2, using fft of size len (a power of 2), result in v1"

  mus_long_t n;
  vct *v1, *v2;

  Xen_check_type((mus_is_vct(url1)), url1, 1, S_convolution, "a " S_vct);
  Xen_check_type((mus_is_vct(url2)), url2, 2, S_convolution, "a " S_vct);

  v1 = Xen_to_vct(url1);
  v2 = Xen_to_vct(url2);

  if (Xen_is_integer(un)) 
    {
      n = Xen_llong_to_C_llong(un); 
      if (n <= 0)
	Xen_out_of_range_error(S_convolution, 3, un, "size <= 0?");
      if (n > mus_max_malloc())
	Xen_out_of_range_error(S_convolution, 3, un, "size too large (see mus-max-malloc)");
      if (n > mus_vct_length(v1))
	n = mus_vct_length(v1);
    }
  else n = mus_vct_length(v1);
  if (n > mus_vct_length(v2))
    n = mus_vct_length(v2);
  if (!(is_power_of_2(n)))
    {
      mus_float_t nf;
      int np;
      nf = (log(n) / log(2.0));
      np = (int)nf;
      n = (int)pow(2.0, np);
    }
  if (n > 0)
    mus_convolution(mus_vct_data(v1), mus_vct_data(v2), n);
  return(url1);
}


static Xen g_polynomial(Xen arr, Xen x)
{
  #define H_polynomial "(" S_polynomial " coeffs x): evaluate a polynomial at x.  coeffs are in order \
of degree, so coeff[0] is the constant term."

#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(x), x, 2, S_polynomial, "a number");
#endif
  if (mus_is_vct(arr))
    {
      vct *v;
      v = Xen_to_vct(arr);
      return(C_double_to_Xen_real(mus_polynomial(mus_vct_data(v), Xen_real_to_C_double_with_caller(x, S_polynomial), mus_vct_length(v))));
    }

  Xen_check_type(Xen_is_vector(arr), arr, 1, S_polynomial, "a vector or " S_vct);
  {
    mus_float_t sum, cx;
    int i, ncoeffs;

    ncoeffs = Xen_vector_length(arr);
    if (ncoeffs <= 0) return(C_double_to_Xen_real(0.0));
    if (ncoeffs == 1) return(Xen_vector_ref(arr, 0)); /* just a constant term */

    cx = Xen_real_to_C_double_with_caller(x, S_polynomial);
    sum = Xen_real_to_C_double_with_caller(Xen_vector_ref(arr, ncoeffs - 1), S_polynomial);
    for (i = ncoeffs - 2; i >= 0; i--) 
      sum = (sum * cx) + Xen_real_to_C_double_with_caller(Xen_vector_ref(arr, i), S_polynomial);
    return(C_double_to_Xen_real(sum));
  }
}


static Xen g_array_interp(Xen obj, Xen phase, Xen size) /* opt size */
{
  #define H_array_interp "(" S_array_interp " v phase (size)): v[phase] \
taking into account wrap-around (size is size of data), with linear interpolation if phase is not an integer."

  mus_long_t len;
  vct *v;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_array_interp, "a " S_vct);
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(phase), phase, 2, S_array_interp, "a number");
#endif
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 3, S_array_interp, "an integer");

  v = Xen_to_vct(obj);
  if (Xen_is_bound(size)) 
    {
      len = Xen_llong_to_C_llong(size); 
      if (len <= 0)
	Xen_out_of_range_error(S_array_interp, 3, size, "size <= 0?");
      if (len > mus_vct_length(v)) 
	len = mus_vct_length(v);
    }
  else len = mus_vct_length(v);
  if (len == 0)
    return(C_double_to_Xen_real(0.0));
  return(C_double_to_Xen_real(mus_array_interp(mus_vct_data(v), Xen_real_to_C_double_with_caller(phase, S_array_interp), len)));
}


static Xen g_mus_interpolate(Xen type, Xen x, Xen obj, Xen size, Xen yn1)
{
  #define H_mus_interpolate "(" S_mus_interpolate " type x v (size) (yn1 0.0)): interpolate in \
data ('v' is a " S_vct ") using interpolation 'type', such as " S_mus_interp_linear "."

  mus_long_t len;
  int itype;
  vct *v;
  mus_float_t y = 0.0;

  Xen_check_type(Xen_is_integer(type), type, 1, S_mus_interpolate, "an integer (interp type such as " S_mus_interp_all_pass ")");
  Xen_check_type(Xen_is_number(x), x, 2, S_mus_interpolate, "a number");
  Xen_check_type(mus_is_vct(obj), obj, 3, S_mus_interpolate, "a " S_vct);
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 4, S_mus_interpolate, "an integer");
  Xen_check_type(Xen_is_number_or_unbound(yn1), yn1, 5, S_mus_interpolate, "a number");

  itype = Xen_integer_to_C_int(type);
  if (!(mus_is_interp_type(itype)))
    Xen_out_of_range_error(S_mus_interpolate, 1, type, "unknown interp type");

  v = Xen_to_vct(obj);

  if (Xen_is_bound(size)) 
    {
      len = Xen_llong_to_C_llong(size); 
      if (len <= 0)
	Xen_out_of_range_error(S_mus_interpolate, 4, size, "size <= 0?");
      if (len > mus_vct_length(v)) 
	len = mus_vct_length(v);
    }
  else len = mus_vct_length(v);
  if (len == 0)
    return(C_double_to_Xen_real(0.0));

  if (Xen_is_number(yn1))
    y = Xen_real_to_C_double(yn1);

  return(C_double_to_Xen_real(mus_interpolate((mus_interp_t)itype, Xen_real_to_C_double(x), mus_vct_data(v), len, y)));
}



/* ---------------- mus-xen struct ---------------- */

static Xen_object_type_t mus_xen_tag;

bool mus_is_xen(Xen obj) {return(Xen_c_object_is_type(obj, mus_xen_tag));}

#define Xen_to_C_generator(Xen_Arg, X_Val, C_Val, Checker, Caller, Descr) \
  Xen_check_type((X_Val = (mus_xen *)Xen_object_ref_checked(Xen_Arg, mus_xen_tag)) && (Checker(C_Val = (mus_any *)mus_xen_to_mus_any(X_Val))), Xen_Arg, 1, Caller, Descr)

#define Xen_to_C_any_generator(Xen_Arg, X_Val, C_Val, Caller, Descr) \
  Xen_check_type((X_Val = (mus_xen *)Xen_object_ref_checked(Xen_Arg, mus_xen_tag)) && (C_Val = (mus_any *)mus_xen_to_mus_any(X_Val)), Xen_Arg, 1, Caller, Descr)



static Xen g_is_mus_generator(Xen obj) 
{
  #define H_is_mus_generator "(" S_is_mus_generator " obj): " PROC_TRUE " if 'obj' is a CLM generator."
  return(C_bool_to_Xen_boolean(mus_is_xen(obj)));
}


#if HAVE_SCHEME
static s7_pointer s7_mus_xen_mark(s7_scheme *sc, s7_pointer obj)
#else
static Xen_object_mark_t mark_mus_xen(Xen obj) 
#endif
{
  mus_xen *ms;
#if HAVE_RUBY
  /* rb_gc_mark passes us the actual value, not the Xen wrapper */
  ms = (mus_xen *)obj;
#endif
#if HAVE_FORTH
  ms = Xen_to_mus_xen(obj);
#endif
#if HAVE_SCHEME
  ms = (mus_xen *)s7_c_object_value(obj);
#endif
  if (ms->vcts) 
    {
      int i, lim;
      lim = MUS_SELF_WRAPPER;
      if (ms->nvcts < lim) lim = ms->nvcts;
      for (i = 0; i < lim; i++) 
	if (Xen_is_bound(ms->vcts[i]))
	  xen_gc_mark(ms->vcts[i]);
    }
#if HAVE_RUBY || HAVE_SCHEME
  return(NULL);
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  return(0);
#endif
}


#if (!HAVE_SCHEME)
static void mus_xen_free(mus_xen *ms)
{
  mus_free(ms->gen);
  ms->gen = NULL;
  mx_free(ms);
}

Xen_wrap_free(mus_xen, free_mus_xen, mus_xen_free)

#else

static s7_pointer s7_mus_xen_free(s7_scheme *sc, s7_pointer obj)
{
  mus_xen *ms;
  ms = (mus_xen *)s7_c_object_value(obj);
  mus_free(ms->gen);
  ms->gen = NULL;
  mx_free(ms);  
  return(NULL);
}

static s7_pointer mus_generator_to_string(s7_scheme *sc, s7_pointer args)
{
  s7_pointer g;
  g = s7_car(args);
  if (s7_is_pair(s7_cdr(args)))
    {
      s7_pointer choice;
      choice = s7_cadr(args);
      if (choice == kw_readable)
	s7_error(sc, s7_make_symbol(sc, "out-of-range"), s7_list(sc, 1, s7_make_string(sc, "can't write a clm generator readably")));
    }
  return(s7_make_string(sc, mus_describe(((mus_xen *)s7_c_object_value(g))->gen)));
}

static s7_pointer s7_mus_xen_is_equal(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p1, p2;
  p1 = s7_car(args);
  p2 = s7_cadr(args);
  if (p1 == p2) return(s7_t(sc));

  if (s7_c_object_type(p2) == mus_xen_tag)
    return(s7_make_boolean(sc, mus_equalp(((mus_xen *)s7_c_object_value(p1))->gen, ((mus_xen *)s7_c_object_value(p2))->gen)));
  return(s7_f(sc));
}
#endif


enum {G_FILTER_STATE, G_FILTER_XCOEFFS, G_FILTER_YCOEFFS};
/* G_FILTER_STATE must = MUS_DATA_WRAPPER = 0 */
enum {G_LOCSIG_DATA, G_LOCSIG_REVDATA, G_LOCSIG_OUT, G_LOCSIG_REVOUT};

static Xen mus_xen_copy(mus_xen *ms)
{
  /* return an object -> copied mus_xen -> copied mus_any gen */
  mus_xen *np;

  np = mx_alloc(ms->nvcts);
  np->gen = mus_copy(ms->gen);
  if (ms->nvcts > 0)
    {
      if (ms->nvcts == 1)
	{
	  if ((mus_is_env(np->gen)) || /* do the most common case first */
	      (mus_is_formant_bank(np->gen)))
	    np->vcts[MUS_DATA_WRAPPER] = ms->vcts[MUS_DATA_WRAPPER];
	  else
	    {
	      if ((mus_is_comb_bank(np->gen)) ||
		  (mus_is_all_pass_bank(np->gen)) ||
		  (mus_is_filtered_comb_bank(np->gen)))
		{
		  /* set up objects for new gens so that they will eventually be GC'd */
		  Xen v;
		  int i, len;
		  len = Xen_vector_length(ms->vcts[MUS_DATA_WRAPPER]);
		  v = Xen_make_vector(len, Xen_false);
		  np->vcts[MUS_DATA_WRAPPER] = v;
		  for (i = 0; i < len; i++)
		    Xen_vector_set(v, i, mus_xen_to_object(mus_any_to_mus_xen(mus_bank_generator(np->gen, i))));
		}
	      else np->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
	    }
	}
      else
	{
	  if (ms->nvcts == 2)
	    {
	      if (mus_is_pulsed_env(np->gen))
		{
		  /* mus_free taken care of by copied pulsed_env gen */
		  np->vcts[0] = Xen_false;
		  np->vcts[1] = Xen_false;
		}
	      else
		{
		  if (mus_is_filtered_comb(np->gen))
		    {
		      np->vcts[0] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
		      np->vcts[1] = Xen_false; /* filt gen but it's not wrapped */
		    }
		  else
		    {
		      np->vcts[0] = ms->vcts[0];
		      np->vcts[1] = ms->vcts[1];
		    }
		}
	    }
	  else
	    {
	      if (ms->nvcts == 3)
		{
		  if (mus_is_oscil_bank(np->gen))
		    {
		      np->vcts[0] = ms->vcts[0];
		      np->vcts[1] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
		      np->vcts[2] = ms->vcts[2];
		    }
		  else
		    {
		      np->vcts[G_FILTER_STATE] = xen_make_vct_wrapper(mus_length(np->gen), mus_data(np->gen));
		      np->vcts[G_FILTER_XCOEFFS] = ms->vcts[G_FILTER_XCOEFFS];
		      np->vcts[G_FILTER_YCOEFFS] = ms->vcts[G_FILTER_YCOEFFS];
		    }
		}
	      else
		{
		  int i;
		  for (i = 0; i < ms->nvcts; i++)
		    np->vcts[i] = ms->vcts[i];
		  
		  if (mus_is_granulate(np->gen))
		    np->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_granulate_grain_max_length(np->gen), mus_data(np->gen));
		  
		  if ((mus_is_convolve(np->gen)) ||
		      (mus_is_src(np->gen)) ||
		      (mus_is_granulate(np->gen)) ||
		      (mus_is_phase_vocoder(np->gen)))
		    {
		      Xen c_obj;
		      c_obj = mus_xen_to_object(np);
		      np->vcts[MUS_SELF_WRAPPER] = c_obj;
		      mus_generator_copy_feeders(np->gen, ms->gen);
		      return(c_obj);
		    }
		}
	    }
	}
    }
  return(mus_xen_to_object(np));
}


#if HAVE_RUBY
static Xen mus_xen_to_s(Xen obj)
{
  char *str;
  Xen result;
  str = mus_describe(Xen_to_mus_any(obj));
  result = C_string_to_Xen_string(str);
  if (str) free(str);
  return(result);
}
#endif


#if HAVE_FORTH
static Xen print_mus_xen(Xen obj)
{
  char *str;
  Xen result;
  str = mus_describe(Xen_to_mus_any(obj));
  result = fth_make_string_format("#<%s>", str);
  if (str) free(str);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static Xen equalp_mus_xen(Xen obj1, Xen obj2) 
{
  if ((!(mus_is_xen(obj1))) || (!(mus_is_xen(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(mus_equalp(Xen_to_mus_any(obj1), Xen_to_mus_any(obj2))));
}
#endif


#if HAVE_RUBY || HAVE_FORTH
static Xen mus_xen_apply(Xen gen, Xen arg1, Xen arg2)
{
#if HAVE_FORTH
  Xen_check_type(mus_is_xen(gen), gen, 1, S_mus_apply, "a generator");
#endif
  return(C_double_to_Xen_real(mus_run(Xen_to_mus_any(gen),
				 (Xen_is_number(arg1)) ? Xen_real_to_C_double(arg1) : 0.0,
				 (Xen_is_number(arg2)) ? Xen_real_to_C_double(arg2) : 0.0)));
}
#endif

#if HAVE_SCHEME

/* these are for mus_xen_tag, so need not handle float-vectors */

static Xen mus_xen_apply(s7_scheme *sc, Xen args1)
{
  s7_pointer gen, args;
  gen = s7_car(args1);
  args = s7_cdr(args1);
  if (s7_is_pair(args))
    {
      mus_float_t arg1, arg2;
      arg1 = s7_number_to_real_with_caller(sc, s7_car(args), "mus-apply");
      args = s7_cdr(args);
      if (s7_is_pair(args))
	arg2 = s7_number_to_real_with_caller(sc, s7_car(args), "mus-apply");
      else arg2 = 0.0;
      return(s7_make_real(s7, mus_run(Xen_to_mus_any(gen), arg1, arg2)));
    }
  return(s7_make_real(s7, mus_run(Xen_to_mus_any(gen), 0.0, 0.0)));
}

static Xen s7_mus_length(s7_scheme *sc, Xen args)
{
  s7_pointer obj;
  obj = s7_car(args);
  if (mus_length_exists(Xen_to_mus_any(obj)))
    return(g_mus_length(obj));
  return(s7_make_integer(sc, 1));
}

static Xen g_mus_copy(Xen gen);
static Xen s7_mus_copy(s7_scheme *sc, Xen args)
{
  return(g_mus_copy(s7_car(args)));
}

static s7_pointer g_clm_let;
#endif


Xen mus_xen_to_object(mus_xen *gn) /* global for user-defined gens */
{
#if HAVE_SCHEME
  return(s7_make_c_object_with_let(s7, mus_xen_tag, gn, g_clm_let));
#else
  return(Xen_make_object(mus_xen_tag, gn, mark_mus_xen, free_mus_xen));
#endif
}


Xen mus_xen_to_object_with_vct(mus_xen *gn, Xen v) /* global for user-defined gens */
{
  gn->vcts[MUS_DATA_WRAPPER] = v;
  return(Xen_make_object(mus_xen_tag, gn, mark_mus_xen, free_mus_xen));
}


mus_any *mus_optkey_to_mus_any(Xen key, const char *caller, int n, mus_any *def)
{
  /* from Michael Scholz's sndins.c */
  if (!(Xen_is_keyword(key))) 
    {
      Xen_check_type(mus_is_xen(key), key, n, caller, "a clm generator or keyword");
      return(Xen_to_mus_any(key));
    }
  return(def);
}

#if (!HAVE_SCHEME)
static Xen mus_optkey_to_input_procedure(Xen key, const char *caller, int n, Xen def, int required_args, const char *err)
{
  if (Xen_is_procedure(key))
    {
      if (!(local_arity_ok(key, required_args)))
	Xen_bad_arity_error(caller, n, key, err);
      return(key);
    }

  if (mus_is_xen(key))
    {
      if (!(mus_is_input(Xen_to_mus_any(key))))
	Xen_wrong_type_arg_error(caller, n, key, "an input generator");
      return(key);
    }

  if ((!(Xen_is_keyword(key))) && 
      (!(Xen_is_false(key))))
    Xen_check_type(false, key, n, caller, "a procedure or input generator");
  return(def);
}
#endif


/* ---------------- wrappers ---------------- */

mus_xen *mus_any_to_mus_xen(mus_any *ge)
{
  mus_xen *gn;
  gn = mx_alloc(0);
  gn->gen = ge;
  return(gn);
}


mus_xen *mus_any_to_mus_xen_with_vct(mus_any *ge, Xen v)
{
  mus_xen *gn;
  gn = mx_alloc(1);
  gn->gen = ge;
  gn->vcts[MUS_DATA_WRAPPER] = v;
  return(gn);
}


mus_xen *mus_any_to_mus_xen_with_two_vcts(mus_any *ge, Xen v1, Xen v2)
{
  mus_xen *gn;
  gn = mx_alloc(2);
  gn->gen = ge;
  gn->vcts[MUS_DATA_WRAPPER] = v1;
  gn->vcts[MUS_INPUT_FUNCTION] = v2;
  return(gn);
}



/* ---------------- generic functions ---------------- */


static Xen g_mus_reset(Xen gen) 
{
  #define H_mus_reset "(" S_mus_reset " gen): clear out gen, setting it to its default starting state"
  mus_xen *ms;
  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_reset(ms->gen);
      return(gen);
    }
#if HAVE_SCHEME
  if (s7_is_float_vector(gen))
    {
      s7_int len;
      s7_double *dst;
      dst = s7_float_vector_elements(gen);
      /* memset((void *)s7_float_vector_elements(gen), 0, len * sizeof(s7_double)); */
      for (len = s7_vector_length(gen); len > 0; len--)
	*dst++ = 0.0;
      return(gen);
    }
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-reset"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  } 
#endif
  Xen_check_type(false, gen, 1, S_mus_reset, "a generator");
  return(gen);
}

#if HAVE_SCHEME
  static s7_pointer mus_copy_symbol, copy_function;
#endif

static Xen g_mus_copy(Xen gen) 
{
  #define H_mus_copy "(" S_mus_copy " gen): return a copy of gen"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    return(mus_xen_copy(ms));
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, mus_copy_symbol);
    if (func == copy_function)
      return(s7_copy(s7, s7_list(s7, 1, gen)));
    if (func != Xen_undefined) 
      return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_copy, "a generator");
  return(gen);
}


static Xen g_mus_run(Xen gen, Xen arg1, Xen arg2) 
{
  #define H_mus_run "(" S_mus_run " gen (arg1 0.0) (arg2 0.0)): apply gen to arg1 and arg2"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_float_t a1 = 0.0, a2 = 0.0;
      Xen_real_to_C_double_if_bound(arg1, a1, S_mus_run, 2);
      Xen_real_to_C_double_if_bound(arg2, a2, S_mus_run, 3);
      return(C_double_to_Xen_real(mus_run(ms->gen, a1, a2)));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-run"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 3, gen, arg1, arg2))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_run, "a generator");
  return(C_double_to_Xen_real(0.0));
}


static Xen g_mus_apply(Xen arglist)
{
  #define H_mus_apply "(" S_mus_apply " gen args...): apply gen to args"
  mus_xen *ms;
  Xen gen;
  int arglist_len;

  arglist_len = Xen_list_length(arglist);
  if ((arglist_len > 3) || (arglist_len == 0)) 
    return(C_double_to_Xen_real(0.0));

  gen = Xen_car(arglist);
  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_any *g;
      g = ms->gen;

      if (arglist_len == 1) 
	return(C_double_to_Xen_real(mus_apply(g, 0.0, 0.0)));

      if (arglist_len == 2)
	return(C_double_to_Xen_real(mus_apply(g, Xen_real_to_C_double(Xen_cadr(arglist)), 0.0)));

      return(C_double_to_Xen_real(mus_apply(g, 
					    Xen_real_to_C_double_with_caller(Xen_cadr(arglist), "mus-apply"), 
					    Xen_real_to_C_double_with_caller(Xen_caddr(arglist), "mus-apply"))));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-apply"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, arglist));
  }
#endif
  Xen_check_type(false, Xen_car(arglist), 1, S_mus_apply, "a generator");
  return(C_double_to_Xen_real(0.0));
}


static Xen g_mus_describe(Xen gen) 
{
  #define H_mus_describe "(" S_mus_describe " gen): return a string describing the state of CLM generator generator"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      Xen result;
      char *str;
      str = mus_describe(ms->gen);
      result = C_string_to_Xen_string(str);
      if (str) free(str);
      return(result);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-describe"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_describe, "a generator");
  return(gen);
}


#if HAVE_SCHEME
#define mus_double_generic(Caller, CLM_case, Symbol)				\
  mus_xen *gn;                                                                \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (gn) return(C_double_to_Xen_real(CLM_case(gn->gen))); \
  func = s7_method(s7, gen, Symbol);				\
  if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(gen);

#define mus_set_double_generic(Caller, CLM_case)                   \
  mus_xen *gn;                                                             \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  Xen_check_type(Xen_is_double(val), val, 2, S_set Caller, "a float");   \
  if (gn) {CLM_case(gn->gen, Xen_real_to_C_double(val)); return(val);}	\
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if ((func != Xen_undefined) && (s7_setter(s7, func)))	\
    return(s7_apply_function(s7, s7_setter(s7, func), s7_list(s7, 2, gen, val))); \
  Xen_check_type(false, gen, 1, S_set Caller, "a generator");  \
  return(val);

#define mus_long_long_generic(Caller, CLM_case)                       \
  mus_xen *gn;                                                                \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (gn) return(C_llong_to_Xen_llong(CLM_case(gn->gen))); \
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(gen);

#define mus_set_long_long_generic(Caller, CLM_case)                \
  mus_xen *gn;                                                             \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  Xen_check_type(Xen_is_integer(val), val, 2, Caller, "an integer"); \
  if (gn) {CLM_case(gn->gen, Xen_llong_to_C_llong(val)); return(val);}	\
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if ((func != Xen_undefined) && (s7_setter(s7, func)))	\
    return(s7_apply_function(s7, s7_setter(s7, func), s7_list(s7, 2, gen, val))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(val);

#define mus_int_generic(Caller, CLM_case)                             \
  mus_xen *gn;                                                                \
  s7_pointer func; \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (gn) return(C_int_to_Xen_integer(CLM_case(gn->gen)));			\
  func = s7_method(s7, gen, s7_make_symbol(s7, Caller));		\
  if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); \
  Xen_check_type(false, gen, 1, Caller, "a generator");	\
  return(gen);

#else

#define mus_double_generic(Caller, CLM_case, Symbol)				\
  mus_xen *gn;                                                                \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(C_double_to_Xen_real(CLM_case(gn->gen)));

#define mus_set_double_generic(Caller, CLM_case)                   \
  mus_xen *gn;                                                             \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, S_set Caller, "a generator");  \
  Xen_check_type(Xen_is_double(val), val, 2, S_set Caller, "a float");   \
  CLM_case(gn->gen, Xen_real_to_C_double(val));				   \
  return(val);

#define mus_long_long_generic(Caller, CLM_case)                          \
  mus_xen *gn;                                                                \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(C_llong_to_Xen_llong(CLM_case(gn->gen)));

#define mus_set_long_long_generic(Caller, CLM_case)                   \
  mus_xen *gn;                                                             \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, S_set Caller, "a generator");  \
  Xen_check_type(Xen_is_integer(val), val, 2, S_set Caller, "an integer");   \
  CLM_case(gn->gen, Xen_llong_to_C_llong(val));				   \
  return(val);

#define mus_int_generic(Caller, CLM_case)                             \
  mus_xen *gn;                                                                \
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);		\
  if (!gn) Xen_check_type(false, gen, 1, Caller, "a generator");  \
  return(C_int_to_Xen_integer(CLM_case(gn->gen))); 

#endif

#if HAVE_SCHEME
static Xen sym_frequency, sym_phase, sym_scaler, sym_increment, sym_width, sym_offset, sym_feedforward, sym_feedback;
#endif

static Xen g_mus_frequency(Xen gen) 
{
  #define H_mus_frequency "(" S_mus_frequency " gen): gen's frequency (Hz)"
  mus_double_generic(S_mus_frequency, mus_frequency, sym_frequency);
}

static Xen g_mus_set_frequency(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_frequency, mus_set_frequency);
}


static Xen g_mus_phase(Xen gen) 
{
  #define H_mus_phase "(" S_mus_phase " gen): gen's current phase (radians)"
  mus_double_generic(S_mus_phase, mus_phase, sym_phase);
}

static Xen g_mus_set_phase(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_phase, mus_set_phase);
}


static Xen g_mus_scaler(Xen gen) 
{
  #define H_mus_scaler "(" S_mus_scaler " gen): gen's scaler, if any.  This is often an amplitude adjustment of some sort."
  mus_double_generic(S_mus_scaler, mus_scaler, sym_scaler);
}

static Xen g_mus_set_scaler(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_scaler, mus_set_scaler);
}


static Xen g_mus_feedforward(Xen gen) 
{
  #define H_mus_feedforward "(" S_mus_feedforward " gen): gen's feedforward field"
  mus_double_generic(S_mus_feedforward, mus_scaler, sym_feedforward);
}

static Xen g_mus_set_feedforward(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_feedforward, mus_set_scaler);
}


static Xen g_mus_feedback(Xen gen)
{
  #define H_mus_feedback "(" S_mus_feedback " gen): gen's " S_mus_feedback " field"
  mus_double_generic(S_mus_feedback, mus_increment, sym_feedback);
}

static Xen g_mus_set_feedback(Xen gen, Xen val)
{
  mus_set_double_generic(S_mus_feedback, mus_set_increment);
}


static Xen g_mus_width(Xen gen) 
{
  #define H_mus_width "(" S_mus_width " gen): gen's width, if any.  This is usually a table size."
  mus_double_generic(S_mus_width, mus_width, sym_width);
}

static Xen g_mus_set_width(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_width, mus_set_width);
}


static Xen g_mus_offset(Xen gen) 
{
  #define H_mus_offset "(" S_mus_offset " gen): gen's offset, if any"
  mus_double_generic(S_mus_offset, mus_offset, sym_offset);
}

static Xen g_mus_set_offset(Xen gen, Xen val) 
{
  mus_set_double_generic(S_mus_offset, mus_set_offset);
}


static Xen g_mus_increment(Xen gen)
{
  #define H_mus_increment "(" S_mus_increment " gen): gen's " S_mus_increment " field, if any"
  mus_double_generic(S_mus_increment, mus_increment, sym_increment);
}

static Xen g_mus_set_increment(Xen gen, Xen val)
{
  mus_set_double_generic(S_mus_increment, mus_set_increment);
}


static Xen g_mus_hop(Xen gen)
{
  #define H_mus_hop "(" S_mus_hop " gen): gen's " S_mus_hop " field"
  mus_long_long_generic(S_mus_hop, mus_hop);
}

static Xen g_mus_set_hop(Xen gen, Xen val)
{
  mus_set_long_long_generic(S_mus_hop, mus_set_hop);
}


static Xen g_mus_ramp(Xen gen)
{
  #define H_mus_ramp "(" S_mus_ramp " gen): granulate generator's " S_mus_ramp " field"
  mus_long_long_generic(S_mus_ramp, mus_ramp);
}

static Xen g_mus_set_ramp(Xen gen, Xen val)
{
  mus_set_long_long_generic(S_mus_ramp, mus_set_ramp);
}


static Xen g_mus_location(Xen gen)
{
  #define H_mus_location "(" S_mus_location " gen): gen's " S_mus_location " field, if any"
  mus_long_long_generic(S_mus_location, mus_location);
}

static Xen g_mus_set_location(Xen gen, Xen val)
{
  mus_set_long_long_generic(S_mus_location, mus_set_location);
}


static Xen g_mus_order(Xen gen) 
{
  #define H_mus_order "(" S_mus_order " gen): gen's order, if any"
  mus_long_long_generic(S_mus_order, mus_length);
}


static Xen g_mus_channel(Xen gen)
{
  #define H_mus_channel "(" S_mus_channel " gen): gen's " S_mus_channel " field, if any"
  mus_int_generic(S_mus_channel, mus_channel);
}


static Xen g_mus_interp_type(Xen gen)
{
  #define H_mus_interp_type "(" S_mus_interp_type " gen): gen's " S_mus_interp_type " field, if any"
  mus_int_generic(S_mus_interp_type, mus_channels);
}


static Xen g_mus_type(Xen gen) 
{
  #define H_mus_type "(" S_mus_type " gen): gen's type"
  mus_int_generic(S_mus_type, mus_type);
}


static Xen g_mus_name(Xen gen) 
{
  #define H_mus_name "(" S_mus_name " gen): gen's (type) name, if any"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    return(C_string_to_Xen_string(mus_name(mus_xen_to_mus_any(ms))));
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-name"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_name, "a generator");
  return(gen);
}


Xen g_mus_file_name(Xen gen) 
{
  #define H_mus_file_name "(" S_mus_file_name " gen): file associated with gen, if any"
  mus_xen *gn;             
  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (gn) 
    return(C_string_to_Xen_string(mus_file_name(gn->gen)));
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, S_mus_file_name));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_file_name, "a generator"); 
  return(gen);
}


Xen g_mus_data(Xen gen) 
{
  #define H_mus_data "(" S_mus_data " gen): gen's internal data (a " S_vct "), if any"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      if (ms->vcts)
	return(ms->vcts[MUS_DATA_WRAPPER]); 
      else return(Xen_false);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, S_mus_data));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_data, "a generator");
  return(gen);
}


static Xen g_mus_set_data(Xen gen, Xen val) 
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      Xen_check_type((mus_is_vct(val)), val, 2, S_set S_mus_data, "a " S_vct);
      if (ms->vcts)
	{
	  vct *v;
	  mus_any *ma;
	  v = Xen_to_vct(val);
	  ma = ms->gen;
	  mus_set_data(ma, mus_vct_data(v));  /* TO REMEMBER: if allocated, should have freed, and set to not allocated */
	  ms->vcts[MUS_DATA_WRAPPER] = val;
	  return(val);
	}
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-data"));
    if ((func != Xen_undefined) && (s7_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_setter(s7, func), s7_list(s7, 2, gen, val)));
  }
#endif

  Xen_check_type(false, gen, 1, S_set S_mus_data, "a generator with a data field");
  return(Xen_false);
}


static Xen c_xcoeffs(mus_xen *ms)
{
  mus_any *g;
  g = ms->gen;
  if (ms->vcts)
    {
      if (mus_is_polywave(g))
	return(ms->vcts[0]);
      if (ms->nvcts > G_FILTER_XCOEFFS)
	return(ms->vcts[G_FILTER_XCOEFFS]); 
    }
  if ((mus_is_one_zero(g)) ||
      (mus_is_one_pole(g)) ||
      (mus_is_two_zero(g)) ||
      (mus_is_two_pole(g)))
    return(xen_make_vct_wrapper(3, mus_xcoeffs(g)));
  return(Xen_false);
}

static Xen g_mus_xcoeffs(Xen gen) 
{
  #define H_mus_xcoeffs "(" S_mus_xcoeffs " gen): gen's filter xcoeffs (" S_vct " of coefficients on inputs)"
  mus_xen *ms;
  
  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) return(c_xcoeffs(ms));

#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-xcoeffs"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_xcoeffs, "a generator");
  return(gen);
}


static Xen g_mus_ycoeffs(Xen gen) 
{
  #define H_mus_ycoeffs "(" S_mus_ycoeffs " gen): gen's filter ycoeffs (" S_vct " of coefficients on outputs)"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      mus_any *g;
      g = ms->gen;
      if (ms->vcts)
	{
	  if ((mus_is_polywave(Xen_to_mus_any(gen))) && (ms->nvcts == 2))
	    return(ms->vcts[1]);
	  if (ms->nvcts > G_FILTER_YCOEFFS)
	    return(ms->vcts[G_FILTER_YCOEFFS]);
	}
      if ((mus_is_one_zero(g)) ||
	  (mus_is_one_pole(g)) ||
	  (mus_is_two_zero(g)) ||
	  (mus_is_two_pole(g)))
	return(xen_make_vct_wrapper(3, mus_ycoeffs(g)));
      return(Xen_false);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-ycoeffs"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_ycoeffs, "a generator");
  return(gen);
}


static Xen g_mus_xcoeff(Xen gen, Xen index)
{
  #define H_mus_xcoeff "(" S_mus_xcoeff " gen index): gen's filter xcoeff value at index (0-based)"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      Xen_to_C_integer_or_error(index, ind, S_mus_xcoeff, 2);
      if (ind < 0)
	Xen_out_of_range_error(S_mus_xcoeff, 2, index, "index must be non-negative");
      return(C_double_to_Xen_real(mus_xcoeff(mus_xen_to_mus_any(ms), ind)));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-xcoeff"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 2, gen, index))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_xcoeff, "a generator");
  return(index);
}


static Xen g_mus_set_xcoeff(Xen gen, Xen index, Xen val)
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      mus_float_t x;
      Xen_to_C_integer_or_error(index, ind, S_set S_mus_xcoeff, 2);
      Xen_to_C_double_or_error(val, x, S_set S_mus_xcoeff, 3);
      if (ind < 0)
	Xen_out_of_range_error(S_set S_mus_xcoeff, 2, index, "index must be non-negative");
      mus_set_xcoeff(mus_xen_to_mus_any(ms), ind, x);
      return(val);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-xcoeff"));
    if ((func != Xen_undefined) && (s7_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_setter(s7, func), s7_list(s7, 3, gen, index, val)));
  }
#endif
  Xen_check_type(false, gen, 1, S_set S_mus_xcoeff, "a generator");
  return(val);
}


static Xen g_mus_ycoeff(Xen gen, Xen index)
{
  #define H_mus_ycoeff "(" S_mus_ycoeff " gen index): gen's filter ycoeff value at index (0-based)"
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      Xen_to_C_integer_or_error(index, ind, S_mus_ycoeff, 2);
      if (ind < 0)
	Xen_out_of_range_error(S_mus_ycoeff, 2, index, "index must be non-negative");
      return(C_double_to_Xen_real(mus_ycoeff(mus_xen_to_mus_any(ms), ind)));
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-ycoeff"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 2, gen, index))); 
  }
#endif
  Xen_check_type(false, gen, 1, S_mus_ycoeff, "a generator");
  return(index);
}


static Xen g_mus_set_ycoeff(Xen gen, Xen index, Xen val)
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms) 
    {
      int ind = 0;
      mus_float_t x;
      Xen_to_C_integer_or_error(index, ind, S_set S_mus_ycoeff, 2);
      Xen_to_C_double_or_error(val, x, S_set S_mus_ycoeff, 3);
      if (ind < 0)
	Xen_out_of_range_error(S_set S_mus_ycoeff, 2, index, "index must be non-negative");
      mus_set_ycoeff(mus_xen_to_mus_any(ms), ind, x);
      return(val);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-ycoeff"));
    if ((func != Xen_undefined) && (s7_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_setter(s7, func), s7_list(s7, 3, gen, index, val)));
  }
#endif
  Xen_check_type(false, gen, 1, S_set S_mus_ycoeff, "a generator");
  return(val);
}

Xen g_mus_channels(Xen gen)
{
  #define H_mus_channels "(" S_mus_channels " gen): gen's " S_mus_channels " field, if any"
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (gn)
    return(C_int_to_Xen_integer(mus_channels(gn->gen)));

#if HAVE_SCHEME
  if (mus_is_vct(gen))
    {
      if (Xen_vector_rank(gen) > 1)
	return(C_int_to_Xen_integer(s7_vector_dimension(gen, 0)));
      else return(C_int_to_Xen_integer(1));
    }
#else
  if (mus_is_vct(gen))
    return(C_int_to_Xen_integer(1));
#endif

#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-channels"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif

  Xen_check_type(false, gen, 1, S_mus_channels, "an output generator, " S_vct ", or sound-data object");
  return(Xen_false); /* make compiler happy */
}


Xen g_mus_length(Xen gen)
{
  #define H_mus_length "(" S_mus_length " gen): gen's length, if any"
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (gn)
    return(C_llong_to_Xen_llong(mus_length(gn->gen)));

  if (mus_is_vct(gen))
    return(C_int_to_Xen_integer(mus_vct_length(Xen_to_vct(gen))));

#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-length"));
    if (func != Xen_undefined) return(s7_apply_function(s7, func, s7_list(s7, 1, gen))); 
  }
#endif

  Xen_check_type(false, gen, 1, S_mus_length, "a generator, " S_vct ", or sound-data object");
  return(Xen_false); /* make compiler happy */
}


static Xen g_mus_set_length(Xen gen, Xen val) 
{
  mus_xen *ms;

  ms = (mus_xen *)Xen_object_ref_checked(gen, mus_xen_tag);
  if (ms)
    {
      mus_long_t len = 0;
      mus_any *ptr = NULL;

      Xen_to_C_integer_or_error(val, len, S_set S_mus_length, 2);
      if (len <= 0)
	Xen_out_of_range_error(S_set S_mus_length, 1, val, "must be > 0");

      ptr = ms->gen;
      if ((!mus_is_env(ptr)) && (!mus_is_src(ptr))) /* set length doesn't refer to data vct here */
	{
	  if ((ms->vcts) && (!(Xen_is_eq(ms->vcts[MUS_DATA_WRAPPER], Xen_undefined))))
	    {
	      vct *v;
	      v = Xen_to_vct(ms->vcts[MUS_DATA_WRAPPER]);
	      if ((v) && (len > mus_vct_length(v)))
		Xen_out_of_range_error(S_set S_mus_length, 1, val, "must be <= current data size");
	      /* set_offset refers only to env, set_width only to square_wave et al, set_location only readin */
	      /* filters are protected by keeping allocated_size and not allowing arrays to be set */
	    }
	}
      mus_set_length(ptr, len);
      return(val);
    }
#if HAVE_SCHEME
  {
    s7_pointer func; 
    func = s7_method(s7, gen, s7_make_symbol(s7, "mus-length"));
    if ((func != Xen_undefined) && (s7_setter(s7, func)))		      
      return(s7_apply_function(s7, s7_setter(s7, func), s7_list(s7, 2, gen, val)));
  }
#endif
  Xen_check_type(false, gen, 1, S_set S_mus_length, "a generator");
  return(val);
}

#if HAVE_SCHEME
#define D_METHOD(Func)					\
  static s7_double mus_ ## Func ## _dp(s7_pointer o)	\
  {							\
   return(mus_ ## Func(Xen_to_mus_any(o)));		\
  }
D_METHOD(scaler)
D_METHOD(phase)
D_METHOD(frequency)
D_METHOD(offset)
D_METHOD(width)
D_METHOD(increment)
D_METHOD(feedforward)
D_METHOD(feedback)

/* these two accept float-vectors and other non-generator arguments */
#define I_METHOD(Func)					\
  static s7_int mus_ ## Func ## _ip(s7_scheme *sc, s7_pointer o)	\
  {							\
   return(s7_integer(g_mus_ ## Func(o)));		\
  }
I_METHOD(length)
I_METHOD(channels)

#define I2_METHOD(Func)					\
  static s7_int mus_ ## Func ## _ip(s7_scheme *sc, s7_pointer o)	\
  {							\
   return(mus_ ## Func(Xen_to_mus_any(o)));		\
  }
I2_METHOD(order)
I2_METHOD(location)
I2_METHOD(channel)
I2_METHOD(ramp)
I2_METHOD(hop)
#endif


#define Aok(a) Xen_is_bound(a) 

/* ---------------- oscil ---------------- */

#if HAVE_SCHEME
static s7_pointer g_make_oscil(s7_scheme *sc, s7_pointer args)
{
  #define H_make_oscil "(" S_make_oscil " (frequency 0.0) (initial-phase 0.0)): return a new " S_oscil " (sinewave) generator"
  mus_any *ge;
  mus_float_t freq, phase;
  
  freq = s7_number_to_real(s7, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_oscil, 1, s7_car(args), "freq > srate/2?");

  phase = s7_number_to_real(s7, s7_cadr(args));

  ge = mus_make_oscil(freq, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_oscil(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_oscil "(" S_make_oscil " (frequency 0.0) (initial-phase 0.0)): return a new " S_oscil " (sinewave) generator"
  mus_any *ge;
  mus_float_t freq = 0.0, phase = 0.0;

  if (Xen_is_bound(arg1))
    {
      if (!Xen_is_bound(arg2))
	{
	  Xen_check_type(Xen_is_number(arg1), arg1, 1, S_make_oscil, "a number");
	  freq = Xen_real_to_C_double(arg1);
	  if (freq > (0.5 * mus_srate()))
	    Xen_out_of_range_error(S_make_oscil, 1, arg1, "freq > srate/2?");
	}
      else
	{
	  int vals, pr = 0;
	  Xen args[4]; 
	  Xen keys[2];
	  int orig_arg[2] = {0, 0};
	  keys[0] = kw_frequency;
	  keys[1] = kw_initial_phase;

	  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; if (Aok(arg4)) {args[pr++] = arg4;}}}}
	  vals = mus_optkey_unscramble(S_make_oscil, pr, 2, keys, args, orig_arg);
	  if (vals > 0)
	    {
	      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_oscil, orig_arg[0], freq);
	      if (freq > (0.5 * mus_srate()))
		Xen_out_of_range_error(S_make_oscil, orig_arg[0], keys[0], "freq > srate/2?");
	      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_oscil, orig_arg[1], phase);
	    }
	}
    }
 
  ge = mus_make_oscil(freq, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif

static Xen g_oscil(Xen osc, Xen fm, Xen pm)
{
  #define H_oscil "(" S_oscil " gen (fm 0.0) (pm 0.0)): next sample from " S_oscil " gen: val = sin(phase + pm); phase += (freq + fm)"

  mus_float_t fm1;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(osc, gn, g, mus_is_oscil, S_oscil, "an oscil");
  if (!Xen_is_bound(fm))
    return(C_double_to_Xen_real(mus_oscil_unmodulated(g)));
  if (!Xen_is_bound(pm))
    return(C_double_to_Xen_real(mus_oscil_fm(g, Xen_real_to_C_double(fm))));
  fm1 = Xen_real_to_C_double(fm);
  if (fm1 == 0.0)
    return(C_double_to_Xen_real(mus_oscil_pm(g, Xen_real_to_C_double(pm))));
  return(C_double_to_Xen_real(mus_oscil(g, fm1, Xen_real_to_C_double(pm))));
}


static Xen g_is_oscil(Xen os) 
{
  #define H_is_oscil "(" S_is_oscil " gen): " PROC_TRUE " if gen is an " S_oscil
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_oscil(Xen_to_mus_any(os)))));
}


static Xen g_make_oscil_bank(Xen freqs, Xen phases, Xen amps, XEN stable)
{
  #define H_make_oscil_bank "(" S_make_oscil_bank " freqs phases amps stable): return a new oscil-bank generator. (freqs in radians)"

  mus_any *ge = NULL;
  vct *f, *p, *a = NULL;
  mus_xen *gn;

  Xen_check_type(mus_is_vct(freqs), freqs, 1, S_make_oscil_bank, "a " S_vct);
  Xen_check_type(mus_is_vct(phases), phases, 2, S_make_oscil_bank, "a " S_vct);
  Xen_check_type(Xen_is_boolean_or_unbound(stable), stable, 3, S_make_oscil_bank, "a boolean");

  f = Xen_to_vct(freqs);
  p = Xen_to_vct(phases);
  if (mus_is_vct(amps)) a = Xen_to_vct(amps);
  
  ge = mus_make_oscil_bank(mus_vct_length(f), mus_vct_data(f), mus_vct_data(p), (a) ? mus_vct_data(a) : NULL, Xen_is_true(stable));
  /* Xen_is_true looks specifically for #t */

  gn = mx_alloc(3);
  gn->gen = ge;
  gn->vcts[0] = freqs;
  gn->vcts[1] = phases;
  gn->vcts[2] = amps;

  return(mus_xen_to_object(gn));
}


static Xen g_is_oscil_bank(Xen os) 
{
  #define H_is_oscil_bank "(" S_is_oscil_bank " gen): " PROC_TRUE " if gen is an " S_oscil_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_oscil_bank(Xen_to_mus_any(os)))));
}


static Xen g_oscil_bank(Xen g)
{
  #define H_oscil_bank "(" S_oscil_bank " bank): sum an array of oscils"
  mus_any *ob = NULL;
  mus_xen *gn;

  Xen_to_C_generator(g, gn, ob, mus_is_oscil_bank, S_oscil_bank, "an oscil-bank generator");
  return(C_double_to_Xen_real(mus_oscil_bank(ob)));
}




/* ---------------- delay ---------------- */

#define H_make_delay "(" S_make_delay " size initial-contents initial-element max-size type): \
return a new delay line of size elements. \
If the delay length will be changing at run-time, max-size sets its maximum length, so\n\
   (" S_make_delay " len :max-size (+ len 10))\n\
provides 10 extra elements of delay for subsequent phasing or flanging. \
initial-contents can be either a list or a " S_vct "."

#define H_make_comb "(" S_make_comb " scaler size initial-contents initial-element max-size type): \
return a new comb filter (a delay line with a scaler on the feedback) of size elements. \
If the comb length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

#define H_make_filtered_comb "(" S_make_filtered_comb " scaler size initial-contents initial-element max-size type filter): \
return a new filtered comb filter (a delay line with a scaler and a filter on the feedback) of size elements. \
If the comb length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

#define H_make_notch "(" S_make_notch " scaler size initial-contents initial-element max-size type)): \
return a new notch filter (a delay line with a scaler on the feedforward) of size elements. \
If the notch length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

#define H_make_all_pass "(" S_make_all_pass " feedback feedforward size initial-contents initial-element max-size type): \
return a new allpass filter (a delay line with a scalers on both the feedback and the feedforward). \
If the " S_all_pass " length will be changing at run-time, max-size sets its maximum length. \
initial-contents can be either a list or a " S_vct "."

#define H_make_moving_average "(" S_make_moving_average " size initial-contents initial-element): \
return a new moving_average generator. initial-contents can be either a list or a " S_vct "."

#define H_make_moving_max "(" S_make_moving_max " size initial-contents initial-element): \
return a new moving-max generator. initial-contents can be either a list or a " S_vct "."

#define H_make_moving_norm "(" S_make_moving_norm " size (scaler 1.0)): return a new moving-norm generator."


#if HAVE_SCHEME

static mus_long_t d_size(s7_scheme *sc, s7_pointer size, int arg_n, const char *caller)
{
  mus_long_t x;
  if (size == Xen_false)
    return(-1);
  if (!s7_is_integer(size))
    s7_wrong_type_arg_error(sc, caller, arg_n, size, "an integer");    
  x = s7_integer(size);
  if (x < 0)
    Xen_out_of_range_error(caller, arg_n, size, "size < 0?");
  if (x > mus_max_table_size())
    Xen_out_of_range_error(caller, arg_n, size, "size too large (see mus-max-table-size)");
  return(x);
}

static mus_long_t d_max_size(s7_scheme *sc, s7_pointer msize, int arg_n, const char *caller)
{
  mus_long_t max_size;
  if (msize == Xen_false)
    return(-1);
  if (!s7_is_integer(msize))
    s7_wrong_type_arg_error(sc, caller, arg_n, msize, "an integer");    
  max_size = s7_integer(msize);
  if (max_size <= 0)
    Xen_out_of_range_error(caller, arg_n, msize, "max-size <= 0?");
  if (max_size > mus_max_table_size())
    Xen_out_of_range_error(caller, arg_n, msize, "max-size too large (see mus-max-table-size)");
  return(max_size);
}

static int d_interp(s7_scheme *sc, s7_pointer interp, bool maxed, int arg_n, const char *caller)
{
  if (s7_is_integer(interp))
    {
      int interp_type;
      interp_type = s7_integer(interp);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(caller, arg_n, interp, "no such interp-type");
      return(interp_type);
    }
  return((maxed) ? MUS_INTERP_LINEAR : MUS_INTERP_NONE);
}

static s7_pointer d_contents(s7_scheme *sc, s7_pointer contents, s7_pointer init, mus_long_t max_size, int arg_n, const char *caller)
{
  if (contents == Xen_false)
    {
      if (max_size == -1)
	max_size = 1;
      contents = s7_make_float_vector(sc, max_size, 1, NULL);
      if (init != Xen_false)
	{
	  mus_float_t initial_element;
	  initial_element = s7_number_to_real(sc, init);
	  if (initial_element != 0.0)
	    {
	      mus_long_t i;
	      s7_double *line;
	      line = s7_float_vector_elements(contents);
	      for (i = 0; i < max_size; i++) 
		line[i] = initial_element;
	    }
	}
      return(contents);
    }

  if (s7_is_number(init))
    s7_wrong_type_arg_error(sc, caller, arg_n, contents, "initial-contents and initial-element in the same call?");

  if (s7_is_float_vector(contents))
    {
      if (max_size > s7_vector_length(contents))
	Xen_out_of_range_error(caller, arg_n, contents, "size > initial-contents length");
      return(contents);
    }

  if (s7_is_pair(contents))
    {
      mus_long_t len;
      len = s7_list_length(sc, contents);
      if (len < 0)
	s7_wrong_type_arg_error(sc, caller, arg_n, contents, "a proper list");
      if (max_size > len)
	Xen_out_of_range_error(caller, arg_n, contents, "size > initial-contents length");
      return(xen_list_to_vct(contents));
    }
  return(s7_wrong_type_arg_error(sc, caller, arg_n, contents, "a float-vector or a proper list"));
}
  
/* I don't think any of these mus_make* funcs call clm_error, so the local mus_error handlers are not needed */

static s7_pointer g_make_delay(s7_scheme *sc, s7_pointer args)
{
  /* size initial-contents initial-element max-size interp-type */
  mus_any *ge;
  mus_long_t size, max_size;
  int interp_type;
  s7_pointer p;
  s7_pointer initial_contents;
  
  p = s7_cdddr(args);
  size = d_size(sc, s7_car(args), 1, S_make_delay);        /* -1 if no size key */
  max_size = d_max_size(sc, s7_car(p), 4, S_make_delay);   /* -1 if no max-size key */
  if (max_size < size) max_size = (size == 0) ? 1 : size;

  initial_contents = d_contents(sc, s7_cadr(args), s7_caddr(args), max_size, 2, S_make_delay);
  if (max_size == -1)
    max_size = s7_vector_length(initial_contents);
  if (size == -1)
    size = max_size;

  interp_type = d_interp(sc, s7_cadr(p), max_size != size, 5, S_make_delay);
  ge = mus_make_delay(size, s7_float_vector_elements(initial_contents), max_size, (mus_interp_t)interp_type);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

static s7_pointer g_make_comb(s7_scheme *sc, s7_pointer args)
{
  /* scaler size initial-contents initial-element max-size interp-type */
  mus_any *ge;
  mus_long_t size, max_size;
  int interp_type;
  s7_pointer p;
  s7_pointer initial_contents;
  mus_float_t scl;
  
  size = d_size(sc, s7_cadr(args), 2, S_make_comb);        /* -1 if no size key */
  p = s7_cdddr(args);
  max_size = d_max_size(sc, s7_cadr(p), 5, S_make_comb);   /* -1 if no max-size key */
  if (max_size < size) max_size = (size == 0) ? 1 : size;

  initial_contents = d_contents(sc, s7_caddr(args), s7_car(p), max_size, 3, S_make_comb);
  if (max_size == -1)
    max_size = s7_vector_length(initial_contents);
  if (size == -1)
    size = max_size;

  scl = s7_number_to_real(sc, s7_car(args));
  interp_type = d_interp(sc, s7_caddr(p), max_size != size, 6, S_make_comb);
  ge = mus_make_comb(scl, size, s7_float_vector_elements(initial_contents), max_size, (mus_interp_t)interp_type);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

static s7_pointer g_make_filtered_comb(s7_scheme *sc, s7_pointer args)
{
  /* scaler size initial-contents initial-element max-size interp-type filter */
  mus_any *ge, *filt;
  mus_long_t size, max_size;
  int interp_type;
  s7_pointer p, flt;
  s7_pointer initial_contents;
  mus_float_t scl;
  
  size = d_size(sc, s7_cadr(args), 2, S_make_filtered_comb);        /* -1 if no size key */
  p = s7_cdddr(args);
  max_size = d_max_size(sc, s7_cadr(p), 5, S_make_filtered_comb);   /* -1 if no max-size key */
  if (max_size < size) max_size = (size == 0) ? 1 : size;

  initial_contents = d_contents(sc, s7_caddr(args), s7_car(p), max_size, 3, S_make_filtered_comb);
  if (max_size == -1)
    max_size = s7_vector_length(initial_contents);
  if (size == -1)
    size = max_size;

  scl = s7_number_to_real(sc, s7_car(args));
  interp_type = d_interp(sc, s7_caddr(p), max_size != size, 6, S_make_filtered_comb);

  flt = s7_cadddr(p);
  if (flt == Xen_false)
    filt = NULL;
  else
    {
      if (!mus_is_xen(flt))
	s7_wrong_type_arg_error(sc, S_make_filtered_comb, 7, flt, "a generator");
      filt = Xen_to_mus_any(flt);
    }

  ge = mus_make_filtered_comb(scl, size, s7_float_vector_elements(initial_contents), max_size, (mus_interp_t)interp_type, filt);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, initial_contents, flt)));
  return(Xen_false);
}

static s7_pointer g_make_notch(s7_scheme *sc, s7_pointer args)
{
  /* scaler size initial-contents initial-element max-size interp-type */
  mus_any *ge;
  mus_long_t size, max_size;
  int interp_type;
  s7_pointer p;
  s7_pointer initial_contents;
  mus_float_t scl;
  
  size = d_size(sc, s7_cadr(args), 2, S_make_notch);        /* -1 if no size key */
  p = s7_cdddr(args);
  max_size = d_max_size(sc, s7_cadr(p), 5, S_make_notch);   /* -1 if no max-size key */
  if (max_size < size) max_size = (size == 0) ? 1 : size;

  initial_contents = d_contents(sc, s7_caddr(args), s7_car(p), max_size, 3, S_make_notch);
  if (max_size == -1)
    max_size = s7_vector_length(initial_contents);
  if (size == -1)
    size = max_size;

  scl = s7_number_to_real(sc, s7_car(args));
  interp_type = d_interp(sc, s7_caddr(p), max_size != size, 6, S_make_notch);
  ge = mus_make_notch(scl, size, s7_float_vector_elements(initial_contents), max_size, (mus_interp_t)interp_type);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

static s7_pointer g_make_all_pass(s7_scheme *sc, s7_pointer args)
{
  /* feedback feedforward size initial-contents initial-element max-size interp-type */
  mus_any *ge;
  mus_long_t size, max_size;
  int interp_type;
  s7_pointer p;
  s7_pointer initial_contents;
  mus_float_t feedback, feedforward;
  
  size = d_size(sc, s7_caddr(args), 3, S_make_all_pass);        /* -1 if no size key */
  p = s7_cdddr(args);
  max_size = d_max_size(sc, s7_caddr(p), 6, S_make_all_pass);   /* -1 if no max-size key */
  if (max_size < size) max_size = (size == 0) ? 1 : size;

  initial_contents = d_contents(sc, s7_car(p), s7_cadr(p), max_size, 4, S_make_all_pass);
  if (max_size == -1)
    max_size = s7_vector_length(initial_contents);
  if (size == -1)
    size = max_size;

  feedback = s7_number_to_real(sc, s7_car(args));
  feedforward = s7_number_to_real(sc, s7_cadr(args));
  interp_type = d_interp(sc, s7_cadddr(p), max_size != size, 7, S_make_all_pass);
  ge = mus_make_all_pass(feedback, feedforward, size, s7_float_vector_elements(initial_contents), max_size, (mus_interp_t)interp_type);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

static s7_pointer g_make_moving_average(s7_scheme *sc, s7_pointer args)
{
  /* size initial-contents initial-element */
  mus_any *ge;
  mus_long_t size;
  s7_pointer initial_contents;
  mus_float_t *line;
  mus_float_t sum;
  
  size = d_size(sc, s7_car(args), 1, S_make_moving_average);        /* -1 if no size key */
  initial_contents = d_contents(sc, s7_cadr(args), s7_caddr(args), size, 2, S_make_moving_average);
  size = s7_vector_length(initial_contents);
  if (size == 0)
    Xen_out_of_range_error(S_make_moving_average, 1, s7_car(args), "size = 0?");

  line = s7_float_vector_elements(initial_contents);
  sum = line[0];
  if (s7_cadr(args) == Xen_false)
    {
      if (s7_caddr(args) != Xen_false)
	sum *= size;
    }
  else
    {
      mus_long_t i;
      for (i = 1; i < size; i++)
	sum += line[i];
    }
  ge = mus_make_moving_average_with_initial_sum(size, line, sum);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

static s7_pointer g_make_moving_max(s7_scheme *sc, s7_pointer args)
{
  /* size initial-contents initial-element */
  mus_any *ge;
  mus_long_t size;
  s7_pointer initial_contents;
  mus_float_t *line;
  
  size = d_size(sc, s7_car(args), 1, S_make_moving_max);        /* -1 if no size key */
  initial_contents = d_contents(sc, s7_cadr(args), s7_caddr(args), size, 2, S_make_moving_max);
  size = s7_vector_length(initial_contents);
  if (size == 0)
    Xen_out_of_range_error(S_make_moving_max, 1, s7_car(args), "size = 0?");

  line = s7_float_vector_elements(initial_contents);
  ge = mus_make_moving_max(size, line);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

static s7_pointer g_make_moving_norm(s7_scheme *sc, s7_pointer args)
{
  /* size scaler initial_contents initial-element */
  mus_any *ge;
  mus_float_t scl;
  mus_long_t size;
  mus_float_t *line;
  s7_pointer initial_contents;

  size = d_size(sc, s7_car(args), 1, S_make_moving_norm);        /* -1 if no size key */
  initial_contents = d_contents(sc, s7_caddr(args), s7_cadddr(args), size, 3, S_make_moving_norm);
  size = s7_vector_length(initial_contents);
  if (size == 0)
    Xen_out_of_range_error(S_make_moving_norm, 1, s7_car(args), "size = 0?");

  scl = s7_number_to_real(sc, s7_cadr(args));
  line = s7_float_vector_elements(initial_contents);
  ge = mus_make_moving_norm(size, line, scl);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, initial_contents)));
  return(Xen_false);
}

#else
typedef enum {G_DELAY, G_COMB, G_NOTCH, G_ALL_PASS, G_FCOMB} xclm_delay_t;

static Xen g_make_delay_1(xclm_delay_t choice, Xen arglist)
{
  mus_any *ge = NULL, *filt = NULL;
  const char *caller = NULL;
  Xen args[18];
  Xen keys[9];
  Xen xen_filt = Xen_false;
  int orig_arg[9] = {0, 0, 0, 0, 0, 0, 0, (int)MUS_INTERP_NONE, 0};
  int vals, i, argn = 0;
  mus_long_t max_size = -1, size = -1;
  int interp_type = (int)MUS_INTERP_NONE;
  mus_float_t *line = NULL;
  mus_float_t scaler = 0.0, feedback = 0.0, feedforward = 0.0;
  vct *initial_contents = NULL;
  Xen orig_v = Xen_false;            /* initial-contents can be a vct */
  mus_float_t initial_element = 0.0;
  int scaler_key = -1, feedback_key = -1, feedforward_key = -1, size_key = -1, initial_contents_key = -1;
  int initial_element_key = -1, max_size_key = -1, interp_type_key = -1, filter_key = -1;

  switch (choice)
    {
    case G_DELAY:          caller = S_make_delay;                                                       break;
    case G_COMB:           caller = S_make_comb;           scaler_key = argn; keys[argn++] = kw_scaler; break;
    case G_FCOMB:          caller = S_make_filtered_comb;  scaler_key = argn; keys[argn++] = kw_scaler; break;
    case G_NOTCH:          caller = S_make_notch;          scaler_key = argn; keys[argn++] = kw_scaler; break;
    case G_ALL_PASS: 
      caller = S_make_all_pass; 
      feedback_key = argn;
      keys[argn++] = kw_feedback; 
      feedforward_key = argn;
      keys[argn++] = kw_feedforward; 
      break;
    }

  size_key = argn;             keys[argn++] = kw_size;
  initial_contents_key = argn; keys[argn++] = kw_initial_contents;
  initial_element_key = argn;  keys[argn++] = kw_initial_element;
  max_size_key = argn;         keys[argn++] = kw_max_size;
  interp_type_key = argn;      keys[argn++] = kw_type;
  filter_key = argn;           keys[argn++] = kw_filter;

  {
    Xen p;
    int a2, arglist_len;
    a2 = argn * 2;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > a2) 
      clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(caller, arglist_len, argn, keys, args, orig_arg);
  }

  if (vals > 0)
    {
      bool size_set = false, max_size_set = false;
      /* try to catch obvious type/range errors before allocations 
       *   a major complication here is that size can be 0
       */

      if (!(Xen_is_keyword(keys[size_key])))
	{
	  size = Xen_optkey_to_mus_long_t(kw_size, keys[size_key], caller, orig_arg[size_key], size); /* size can  be 0? -- surely we need a line in any case? */
	  if (size < 0)
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size < 0?");
	  if (size > mus_max_table_size())
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size too large (see mus-max-table-size)");
	  size_set = true;
	}

      if (!(Xen_is_keyword(keys[max_size_key])))
	{
	  max_size = Xen_optkey_to_mus_long_t(kw_max_size, keys[max_size_key], caller, orig_arg[max_size_key], max_size); /* -1 = unset */
	  if (max_size <= 0)
	    Xen_out_of_range_error(caller, orig_arg[max_size_key], keys[max_size_key], "max-size <= 0?");
	  if (max_size > mus_max_table_size())
	    Xen_out_of_range_error(caller, orig_arg[max_size_key], keys[max_size_key], "max-size too large (see mus-max-table-size)");
	  max_size_set = true;
	}

      if (Xen_is_keyword(keys[interp_type_key]))
	{
	  /* if type not given, if max_size, assume linear interp (for possible tap), else no interp */
	  if ((max_size_set) && (max_size != size))
	    interp_type = (int)MUS_INTERP_LINEAR;
	  else interp_type = (int)MUS_INTERP_NONE;
	}
      else
	{
	  interp_type = Xen_optkey_to_int(kw_type, keys[interp_type_key], caller, orig_arg[interp_type_key], (int)MUS_INTERP_LINEAR);
	  if (!(mus_is_interp_type(interp_type)))
	    Xen_out_of_range_error(caller, orig_arg[interp_type_key], keys[interp_type_key], "no such interp-type");
	}

      initial_element = Xen_optkey_to_float(kw_initial_element, keys[initial_element_key], caller, orig_arg[initial_element_key], initial_element);

      switch (choice)
	{
	case G_DELAY: 
	  break;

	case G_COMB: case G_NOTCH: case G_FCOMB:
	  scaler = Xen_optkey_to_float(kw_scaler, keys[scaler_key], caller, orig_arg[scaler_key], scaler);
	  break;

	case G_ALL_PASS:
	  feedback = Xen_optkey_to_float(kw_feedback, keys[feedback_key], caller, orig_arg[feedback_key], feedback);
	  feedforward = Xen_optkey_to_float(kw_feedforward, keys[feedforward_key], caller, orig_arg[feedforward_key], feedforward);
	  break;
	}

      if (!(Xen_is_keyword(keys[filter_key])))
	{
	  if (choice != G_FCOMB)
	    clm_error(caller, "filter arg passed??", keys[filter_key]);
				 
	  Xen_check_type(mus_is_xen(keys[filter_key]), keys[filter_key], orig_arg[filter_key], caller, "filter arg must be a generator");
	  xen_filt = keys[filter_key];
	  filt = Xen_to_mus_any(xen_filt);
	}

      if (!(Xen_is_keyword(keys[initial_contents_key])))
	{
	  if (!(Xen_is_keyword(keys[initial_element_key])))
	    Xen_out_of_range_error(caller, 
				   orig_arg[initial_contents_key], 
				   keys[initial_contents_key], 
				   "initial-contents and initial-element in same call?");
	  if (mus_is_vct(keys[initial_contents_key]))
	    {
	      initial_contents = Xen_to_vct(keys[initial_contents_key]);
	      orig_v = keys[initial_contents_key];
	    }
	  else
	    {
	      if (Xen_is_list(keys[initial_contents_key]))
		{
		  int len;
		  len = Xen_list_length(keys[initial_contents_key]);
		  if (len <= 0) 
		    Xen_error(NO_DATA,
			      Xen_list_2(C_string_to_Xen_string("~A: initial-contents not a proper list?"),
					 C_string_to_Xen_string(caller)));

		  orig_v = xen_list_to_vct(keys[initial_contents_key]);
		  initial_contents = Xen_to_vct(orig_v);
		  /* do I need to protect this until we read its contents? -- no extlang stuff except error returns */
		}
	      else Xen_check_type(Xen_is_false(keys[initial_contents_key]), 
				   keys[initial_contents_key], 
				   orig_arg[initial_contents_key], 
				   caller, "a " S_vct " or a list");
	    }
	  if (initial_contents)
	    {
	      if (size_set)
		{
		  if (size > mus_vct_length(initial_contents))
		    Xen_out_of_range_error(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "size > initial-contents length");
		}
	      else size = mus_vct_length(initial_contents);
	      if (max_size_set)
		{
		  if (max_size > mus_vct_length(initial_contents))
		    Xen_out_of_range_error(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "max-size > initial-contents length");
		}
	      else max_size = mus_vct_length(initial_contents);
	    }
	}
    } 
  /* here size can be (user-set to) 0, but max_size needs to be a reasonable allocation size */
  if (size < 0) size = 1;
  if (max_size < size) 
    {
      if (size == 0)
	max_size = 1;
      else max_size = size;
    }

  if (!initial_contents)
    {
      line = (mus_float_t *)malloc(max_size * sizeof(mus_float_t));
      if (!line)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate delay line", caller));
      orig_v = xen_make_vct(max_size, line);
      if (initial_element != 0.0)
	{
	  for (i = 0; i < max_size; i++) 
	    line[i] = initial_element;
	}
      else mus_clear_floats(line, max_size);
    }
  else
    {
      line = mus_vct_data(initial_contents);
    }

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    switch (choice)
      {
      case G_DELAY:           ge = mus_make_delay(size, line, max_size, (mus_interp_t)interp_type);                           break;
      case G_COMB:            ge = mus_make_comb(scaler, size, line, max_size, (mus_interp_t)interp_type);                    break;
      case G_NOTCH:           ge = mus_make_notch(scaler, size, line, max_size, (mus_interp_t)interp_type);                   break;
      case G_ALL_PASS:        ge = mus_make_all_pass(feedback, feedforward, size, line, max_size, (mus_interp_t)interp_type); break;
      case G_FCOMB:           ge = mus_make_filtered_comb(scaler, size, line, max_size, (mus_interp_t)interp_type, filt);     break;
      }
    mus_error_set_handler(old_error_handler);
  }

  if (ge) 
    {
      if (choice != G_FCOMB)
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
      return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, orig_v, xen_filt)));
    }
  return(clm_mus_error(local_error_type, local_error_msg, caller));
}


static Xen g_make_delay(Xen args) 
{
  if ((Xen_is_pair(args)) && (!Xen_is_pair(Xen_cdr(args))))
    {
      Xen val, v;
      mus_any *ge;
      mus_long_t size, max_size;
      mus_float_t *line;

      val = Xen_car(args);
      Xen_check_type(Xen_is_integer(val), val, 1, S_make_delay, "an integer");
      size = Xen_integer_to_C_int(val);
      if (size < 0)
	Xen_out_of_range_error(S_make_delay, 1, val, "size < 0?");
      if (size > mus_max_table_size())
	Xen_out_of_range_error(S_make_delay, 1, val, "size too large (see mus-max-table-size)");
      if (size == 0) max_size = 1; else max_size = size;

      line = (mus_float_t *)calloc(max_size, sizeof(mus_float_t));
      v = xen_make_vct(max_size, line); /* we need this for mus-data */

      ge = mus_make_delay(size, line, max_size, MUS_INTERP_NONE);
      if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
      return(Xen_false);
    }

  return(g_make_delay_1(G_DELAY, args));
}


static Xen g_make_comb(Xen args)          {return(g_make_delay_1(G_COMB, args));}
static Xen g_make_filtered_comb(Xen args) {return(g_make_delay_1(G_FCOMB, args));}
static Xen g_make_notch(Xen args)         {return(g_make_delay_1(G_NOTCH, args));}
static Xen g_make_all_pass(Xen args)      {return(g_make_delay_1(G_ALL_PASS, args));}


typedef enum {G_MOVING_AVERAGE, G_MOVING_MAX, G_MOVING_NORM} xclm_moving_t;

static Xen g_make_moving_any(xclm_moving_t choice, const char *caller, Xen arglist)
{
  mus_any *ge = NULL;
  Xen args[8];
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, i, argn = 0, arglist_len;
  mus_long_t size = -1;
  mus_float_t scaler = 1.0, sum = 0.0;
  vct *initial_contents = NULL;
  Xen orig_v = Xen_false, p; 
  mus_float_t initial_element = 0.0;
  mus_float_t *line = NULL;
  int scaler_key = -1, size_key, initial_contents_key, initial_element_key;
  mus_error_handler_t *old_error_handler;

  size_key = argn;                 
  keys[argn++] = kw_size;
  if (choice == G_MOVING_NORM) 
    {
      scaler_key = argn;       
      keys[argn++] = kw_scaler;
    }
  initial_contents_key = argn; 
  keys[argn++] = kw_initial_contents;
  initial_element_key = argn;  
  keys[argn++] = kw_initial_element;

  arglist_len = Xen_list_length(arglist);
  if (arglist_len > 8) 
    clm_error(caller, "too many arguments!", arglist);
  for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
  vals = mus_optkey_unscramble(caller, arglist_len, argn, keys, args, orig_arg);

  if (vals > 0)
    {
      bool size_set = false;
      if (!(Xen_is_keyword(keys[size_key])))
	{
	  size = Xen_optkey_to_mus_long_t(kw_size, keys[size_key], caller, orig_arg[size_key], size); /* size can  be 0? -- surely we need a line in any case? */
	  if (size < 0)
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size < 0?");
	  if (size > mus_max_table_size())
	    Xen_out_of_range_error(caller, orig_arg[size_key], keys[size_key], "size too large (see mus-max-table-size)");
	  size_set = true;
	}

      if (choice == G_MOVING_NORM)
	scaler = Xen_optkey_to_float(kw_scaler, keys[scaler_key], caller, orig_arg[scaler_key], scaler);

      initial_element = Xen_optkey_to_float(kw_initial_element, keys[initial_element_key], caller, orig_arg[initial_element_key], initial_element);
      if (!(Xen_is_keyword(keys[initial_contents_key])))
	{
	  if (!(Xen_is_keyword(keys[initial_element_key])))
	    Xen_out_of_range_error(caller, 
				   orig_arg[initial_contents_key], 
				   keys[initial_contents_key], 
				   "initial-contents and initial-element in same call?");
	  if (mus_is_vct(keys[initial_contents_key]))
	    {
	      initial_contents = Xen_to_vct(keys[initial_contents_key]);
	      orig_v = keys[initial_contents_key];
	    }
	  else
	    {
	      if (Xen_is_list(keys[initial_contents_key]))
		{
		  int len;
		  len = Xen_list_length(keys[initial_contents_key]);
		  if (len <= 0) 
		    Xen_error(NO_DATA,
			      Xen_list_2(C_string_to_Xen_string("~A: initial-contents not a proper list?"),
					 C_string_to_Xen_string(caller)));
		  
		  orig_v = xen_list_to_vct(keys[initial_contents_key]);
		  initial_contents = Xen_to_vct(orig_v);
		  /* do I need to protect this until we read its contents? -- no extlang stuff except error returns */
		}
	      else Xen_check_type(Xen_is_false(keys[initial_contents_key]), 
				  keys[initial_contents_key], 
				  orig_arg[initial_contents_key], 
				  caller, "a " S_vct " or a list");
	    }
	  if (initial_contents)
	    {
	      if (size_set)
		{
		  if (size > mus_vct_length(initial_contents))
		    Xen_out_of_range_error(caller, orig_arg[initial_contents_key], keys[initial_contents_key], "size > initial-contents length");
		}
	      else size = mus_vct_length(initial_contents);
	    }
	}
    }

  if (size < 0) size = 1;
  if (size == 0)
    Xen_out_of_range_error(caller, 0, C_llong_to_Xen_llong(size), "size = 0?");

  if (!initial_contents)
    {
      line = (mus_float_t *)malloc(size * sizeof(mus_float_t));
      if (!line)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate delay line", caller));
      orig_v = xen_make_vct(size, line);
      if (initial_element != 0.0)
	{
	  for (i = 0; i < size; i++) 
	    line[i] = initial_element;
	  sum = initial_element * size;
	}
      else mus_clear_floats(line, size);
    }
  else
    {
      line = mus_vct_data(initial_contents);
      if ((line) && (choice == G_MOVING_AVERAGE))
	{
	  sum = line[0];
	  for (i = 1; i < size; i++)
	    sum += line[i];
	}
    }

  old_error_handler = mus_error_set_handler(local_mus_error);
  switch (choice)
    {
    case G_MOVING_AVERAGE:  ge = mus_make_moving_average_with_initial_sum(size, line, sum); break;
    case G_MOVING_MAX:      ge = mus_make_moving_max(size, line);                           break;
    case G_MOVING_NORM:     ge = mus_make_moving_norm(size, line, scaler);                  break;
    }
  mus_error_set_handler(old_error_handler);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
  return(clm_mus_error(local_error_type, local_error_msg, caller));
}

static Xen g_make_moving_average(Xen args) {return(g_make_moving_any(G_MOVING_AVERAGE, S_make_moving_average, args));}
static Xen g_make_moving_max(Xen args)     {return(g_make_moving_any(G_MOVING_MAX, S_make_moving_max, args));}
static Xen g_make_moving_norm(Xen args)    {return(g_make_moving_any(G_MOVING_NORM, S_make_moving_norm, args));}
#endif


static Xen g_delay(Xen obj, Xen input, Xen pm)
{
  #define H_delay "(" S_delay " gen (val 0.0) (pm 0.0)): \
delay val according to the delay line's length and pm ('phase-modulation'). \
If pm is greater than 0.0, the max-size argument used to create gen should have accommodated its maximum value."

  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_delay, S_delay, "a delay line");
  if (!Xen_is_bound(input))
    return(C_double_to_Xen_real(mus_delay_unmodulated(g, 0.0)));
  if (!Xen_is_bound(pm))
    return(C_double_to_Xen_real(mus_delay_unmodulated(g, Xen_real_to_C_double(input))));
  return(C_double_to_Xen_real(mus_delay(g, Xen_real_to_C_double(input), Xen_real_to_C_double(pm))));
}


static Xen g_delay_tick(Xen obj, Xen input)
{
  #define H_delay_tick "(" S_delay_tick " gen (val 0.0)): \
delay val according to the delay line's length. This merely 'ticks' the delay line forward.\
The argument 'val' is returned."

  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_delay, S_delay_tick, "a delay line");
  Xen_real_to_C_double_if_bound(input, in1, S_delay_tick, 2);

  return(C_double_to_Xen_real(mus_delay_tick(g, in1)));
}


static Xen g_notch(Xen obj, Xen input, Xen pm)
{
  #define H_notch "(" S_notch " gen (val 0.0) (pm 0.0)): notch filter val, pm changes the delay length."

  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_notch, S_notch, "a notch filter");
  Xen_real_to_C_double_if_bound(input, in1, S_notch, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_notch, 3);

  return(C_double_to_Xen_real(mus_notch(g, in1, pm1)));
}


static Xen g_comb(Xen obj, Xen input, Xen pm)
{
  #define H_comb "(" S_comb " gen (val 0.0) (pm 0.0)): comb filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_comb, S_comb, "a comb generator");
  Xen_real_to_C_double_if_bound(input, in1, S_comb, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_comb, 3);

  return(C_double_to_Xen_real(mus_comb(g, in1, pm1)));
}


static Xen g_make_comb_bank(Xen arg)
{
  #define H_make_comb_bank "(" S_make_comb_bank " gens): return a new comb-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;

  Xen_check_type(Xen_is_vector(arg), arg, 1, S_make_comb_bank, "a vector of comb generators");

  size = Xen_vector_length(arg);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(arg, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_comb(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_comb_bank(j, gens);
  free(gens);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, arg)));
  return(Xen_false);
}


static Xen g_is_comb_bank(Xen os) 
{
  #define H_is_comb_bank "(" S_is_comb_bank " gen): " PROC_TRUE " if gen is a " S_comb_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_comb_bank(Xen_to_mus_any(os)))));
}


static Xen g_comb_bank(Xen gens, Xen inp)
{
  #define H_comb_bank "(" S_comb_bank " bank inval): sum an array of " S_comb " filters."
  mus_any *bank = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(gens, gn, bank, mus_is_comb_bank, S_comb_bank, "a comb-bank generator");
  Xen_real_to_C_double_if_bound(inp, x, S_comb_bank, 2);

  return(C_double_to_Xen_real(mus_comb_bank(bank, x)));
}



static Xen g_filtered_comb(Xen obj, Xen input, Xen pm)
{
  #define H_filtered_comb "(" S_filtered_comb " gen (val 0.0) (pm 0.0)): filtered comb filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_filtered_comb, S_filtered_comb, "a filtered-comb generator");
  Xen_real_to_C_double_if_bound(input, in1, S_filtered_comb, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_filtered_comb, 3);

  return(C_double_to_Xen_real(mus_filtered_comb(g, in1, pm1)));
}


static Xen g_make_filtered_comb_bank(Xen arg)
{
  #define H_make_filtered_comb_bank "(" S_make_filtered_comb_bank " gens): return a new filtered_comb-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;

  Xen_check_type(Xen_is_vector(arg), arg, 1, S_make_filtered_comb_bank, "a vector of filtered_comb generators");

  size = Xen_vector_length(arg);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(arg, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_filtered_comb(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_filtered_comb_bank(j, gens);
  free(gens);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, arg)));
  return(Xen_false);
}


static Xen g_is_filtered_comb_bank(Xen os) 
{
  #define H_is_filtered_comb_bank "(" S_is_filtered_comb_bank " gen): " PROC_TRUE " if gen is a " S_filtered_comb_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_filtered_comb_bank(Xen_to_mus_any(os)))));
}


static Xen g_filtered_comb_bank(Xen gens, Xen inp)
{
  #define H_filtered_comb_bank "(" S_filtered_comb_bank " bank inval): sum an array of " S_filtered_comb " filters."
  mus_any *bank = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(gens, gn, bank, mus_is_filtered_comb_bank, S_filtered_comb_bank, "a filtered-comb-bank generator");
  Xen_real_to_C_double_if_bound(inp, x, S_filtered_comb_bank, 2);

  return(C_double_to_Xen_real(mus_filtered_comb_bank(bank, x)));
}



static Xen g_all_pass(Xen obj, Xen input, Xen pm)
{
  #define H_all_pass "(" S_all_pass " gen (val 0.0) (pm 0.0)): all-pass filter val, pm changes the delay length."
  mus_float_t in1 = 0.0, pm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_all_pass, S_all_pass, "an all-pass filter");
  Xen_real_to_C_double_if_bound(input, in1, S_all_pass, 2);
  Xen_real_to_C_double_if_bound(pm, pm1, S_all_pass, 3);

  return(C_double_to_Xen_real(mus_all_pass(g, in1, pm1)));
}


static Xen g_make_all_pass_bank(Xen arg)
{
  #define H_make_all_pass_bank "(" S_make_all_pass_bank " gens): return a new all_pass-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;

  Xen_check_type(Xen_is_vector(arg), arg, 1, S_make_all_pass_bank, "a vector of all_pass generators");

  size = Xen_vector_length(arg);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(arg, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_all_pass(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_all_pass_bank(j, gens);
  free(gens);

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, arg)));
  return(Xen_false);
}


static Xen g_is_all_pass_bank(Xen os) 
{
  #define H_is_all_pass_bank "(" S_is_all_pass_bank " gen): " PROC_TRUE " if gen is a " S_all_pass_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_all_pass_bank(Xen_to_mus_any(os)))));
}


static Xen g_all_pass_bank(Xen gens, Xen inp)
{
  #define H_all_pass_bank "(" S_all_pass_bank " bank inval): sum an array of " S_all_pass " filters."
  mus_any *bank = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(gens, gn, bank, mus_is_all_pass_bank, S_all_pass_bank, "an all-pass-bank generator");
  Xen_real_to_C_double_if_bound(inp, x, S_all_pass_bank, 2);

  return(C_double_to_Xen_real(mus_all_pass_bank(bank, x)));
}



static Xen g_moving_average(Xen obj, Xen input)
{
  #define H_moving_average "(" S_moving_average " gen (val 0.0)): moving window average."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_moving_average, S_moving_average, "a moving-average generator");
  Xen_real_to_C_double_if_bound(input, in1, S_moving_average, 2);

  return(C_double_to_Xen_real(mus_moving_average(g, in1)));
}


static Xen g_moving_max(Xen obj, Xen input)
{
  #define H_moving_max "(" S_moving_max " gen (val 0.0)): moving window max."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_moving_max, S_moving_max, "a moving-max generator");
  Xen_real_to_C_double_if_bound(input, in1, S_moving_max, 2);

  return(C_double_to_Xen_real(mus_moving_max(g, in1)));
}


static Xen g_moving_norm(Xen obj, Xen input)
{
  #define H_moving_norm "(" S_moving_norm " gen (val 0.0)): moving window norm."
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_moving_norm, S_moving_norm, "a moving-norm generator");
  Xen_real_to_C_double_if_bound(input, in1, S_moving_norm, 2);

  return(C_double_to_Xen_real(mus_moving_norm(g, in1)));
}


static Xen g_tap(Xen obj, Xen loc)
{
  #define H_tap "(" S_tap " gen (pm 0.0)): tap the " S_delay " generator offset by pm"
  mus_float_t dloc = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_tap, S_tap, "a delay line tap");
  Xen_real_to_C_double_if_bound(loc, dloc, S_tap, 3);

  return(C_double_to_Xen_real(mus_tap(g, dloc)));
}


static Xen g_is_tap(Xen obj) 
{
  #define H_is_tap "(" S_is_tap " gen): " PROC_TRUE " if gen is a delay line tap"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_tap(Xen_to_mus_any(obj)))));
}


static Xen g_is_delay(Xen obj) 
{
  #define H_is_delay "(" S_is_delay " gen): " PROC_TRUE " if gen is a delay line"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_delay(Xen_to_mus_any(obj)))));
}


static Xen g_is_comb(Xen obj)
{
  #define H_is_comb "(" S_is_comb " gen): " PROC_TRUE " if gen is a comb filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_comb(Xen_to_mus_any(obj)))));
}


static Xen g_is_filtered_comb(Xen obj)
{
  #define H_is_filtered_comb "(" S_is_filtered_comb " gen): " PROC_TRUE " if gen is a filtered-comb filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_filtered_comb(Xen_to_mus_any(obj)))));
}


static Xen g_is_notch(Xen obj) 
{
  #define H_is_notch "(" S_is_notch " gen): " PROC_TRUE " if gen is a notch filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_notch(Xen_to_mus_any(obj)))));
}


static Xen g_is_all_pass(Xen obj) 
{
  #define H_is_all_pass "(" S_is_all_pass " gen): " PROC_TRUE " if gen is an all-pass filter"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_all_pass(Xen_to_mus_any(obj)))));
}


static Xen g_is_moving_average(Xen obj) 
{
  #define H_is_moving_average "(" S_is_moving_average " gen): " PROC_TRUE " if gen is a moving-average generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_moving_average(Xen_to_mus_any(obj)))));
}


static Xen g_is_moving_max(Xen obj) 
{
  #define H_is_moving_max "(" S_is_moving_max " gen): " PROC_TRUE " if gen is a moving-max generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_moving_max(Xen_to_mus_any(obj)))));
}


static Xen g_is_moving_norm(Xen obj) 
{
  #define H_is_moving_norm "(" S_is_moving_norm " gen): " PROC_TRUE " if gen is a moving-norm generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_moving_norm(Xen_to_mus_any(obj)))));
}


/* -------- ncos -------- */

static Xen g_is_ncos(Xen obj) 
{
  #define H_is_ncos "(" S_is_ncos " gen): " PROC_TRUE " if gen is an " S_ncos " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_ncos(Xen_to_mus_any(obj)))));
}

#if HAVE_SCHEME
static s7_pointer g_make_ncos(s7_scheme *sc, s7_pointer args)
{
  #define H_make_ncos "(" S_make_ncos " (frequency 0.0) (n 1)): return a new " S_ncos " generator, producing a sum of 'n' equal amplitude cosines."
  mus_any *ge;
  mus_float_t freq;
  int n;
  s7_pointer un;

  freq = s7_number_to_real(s7, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_ncos, 1, s7_car(args), "freq > srate/2?");

  un = s7_cadr(args);
  if (!s7_is_integer(un))
    return(s7_wrong_type_arg_error(s7, S_make_ncos, 2, un, "an integer"));
  n = s7_integer(un);
  if (n <= 0)
    Xen_out_of_range_error(S_make_ncos, 2, un, "n <= 0?");

  ge = mus_make_ncos(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_ncos(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_ncos "(" S_make_ncos " (frequency 0.0) (n 1)): return a new " S_ncos " generator, producing a sum of 'n' equal amplitude cosines."

  mus_any *ge;
  Xen args[4]; 
  Xen keys[2];
  int orig_arg[2] = {0, 0};
  int vals, n = 1, pr = 0;
  mus_float_t freq = 0.0;

  keys[0] = kw_frequency;
  keys[1] = kw_n;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; if (Aok(arg4)) {args[pr++] = arg4;}}}}
  vals = mus_optkey_unscramble(S_make_ncos, pr, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_ncos, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_ncos, orig_arg[0], keys[0], "freq > srate/2?");

      n = Xen_optkey_to_int(kw_n, keys[1], S_make_ncos, orig_arg[1], n);
      if (n <= 0)
	Xen_out_of_range_error(S_make_ncos, orig_arg[1], keys[1], "n <= 0?");
    }

  ge = mus_make_ncos(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif

static Xen g_ncos(Xen obj, Xen fm)
{
  #define H_ncos "(" S_ncos " gen (fm 0.0)): get the next sample from 'gen', an " S_ncos " generator"

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_ncos, S_ncos, "an ncos generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_ncos, 2);

  return(C_double_to_Xen_real(mus_ncos(g, fm1)));
}



/* -------- nsin -------- */

static Xen g_is_nsin(Xen obj) 
{
  #define H_is_nsin "(" S_is_nsin " gen): " PROC_TRUE " if gen is an " S_nsin " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_nsin(Xen_to_mus_any(obj)))));
}

#if HAVE_SCHEME
static s7_pointer g_make_nsin(s7_scheme *sc, s7_pointer args)
{
  #define H_make_nsin "(" S_make_nsin " (frequency 0.0) (n 1)): return a new " S_nsin " generator, producing a sum of 'n' equal amplitude sines"
  mus_any *ge;
  mus_float_t freq;
  int n;
  s7_pointer un;

  freq = s7_number_to_real(s7, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_nsin, 1, s7_car(args), "freq > srate/2?");

  un = s7_cadr(args);
  if (!s7_is_integer(un))
    return(s7_wrong_type_arg_error(s7, S_make_nsin, 2, un, "an integer"));
  n = s7_integer(un);
  if (n <= 0)
    Xen_out_of_range_error(S_make_nsin, 2, un, "n <= 0?");

  ge = mus_make_nsin(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_nsin(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_nsin "(" S_make_nsin " (frequency 0.0) (n 1)): return a new " S_nsin " generator, producing a sum of 'n' equal amplitude sines"

  mus_any *ge;
  Xen args[4]; 
  Xen keys[2];
  int orig_arg[2] = {0, 0};
  int vals, n = 1, pr = 0;
  mus_float_t freq = 0.0;

  keys[0] = kw_frequency;
  keys[1] = kw_n;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; if (Aok(arg4)) {args[pr++] = arg4;}}}}
  vals = mus_optkey_unscramble(S_make_nsin, pr, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_nsin, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_nsin, orig_arg[0], keys[0], "freq > srate/2?");

      n = Xen_optkey_to_int(kw_n, keys[1], S_make_nsin, orig_arg[1], n);
      if (n <= 0)
	Xen_out_of_range_error(S_make_nsin, orig_arg[1], keys[1], "n <= 0?");
    }

  ge = mus_make_nsin(freq, n);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif

static Xen g_nsin(Xen obj, Xen fm)
{
  #define H_nsin "(" S_nsin " gen (fm 0.0)): get the next sample from 'gen', an " S_nsin " generator"

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_nsin, S_nsin, "an nsin generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_nsin, 2);

  return(C_double_to_Xen_real(mus_nsin(g, fm1)));
}



/* ---------------- rand, rand_interp ---------------- */

#define RANDOM_DISTRIBUTION_TABLE_SIZE 512
#define RANDOM_DISTRIBUTION_ENVELOPE_SIZE 50

static mus_float_t *inverse_integrate(Xen dist, int data_size)
{
  /* e = env possibly starting < 0 */
  int e_size = RANDOM_DISTRIBUTION_ENVELOPE_SIZE;
  mus_float_t *e, *data;
  int i, e_len, lim, e_loc = 2;
  Xen ex0, ex1, ey0, ey1;
  mus_float_t x, x0, x1, xincr, y0, y1, sum, first_sum, last_sum = 0.0;

  lim = (e_size + 1) * 2;
  e = (mus_float_t *)calloc(lim, sizeof(mus_float_t));

  e_len = Xen_list_length(dist);
  ex0 = Xen_list_ref(dist, 0);
  ex1 = Xen_list_ref(dist, e_len - 2);
  x0 = Xen_real_to_C_double(ex0);
  /* get x range first */
  x1 = Xen_real_to_C_double(ex1);
  xincr = (x1 - x0) / (mus_float_t)e_size;
  /* now true x1 */
  ex1 = Xen_list_ref(dist, 2);
  x1 = Xen_real_to_C_double(ex1);
  ey0 = Xen_list_ref(dist, 1);
  ey1 = Xen_list_ref(dist, 3);
  y0 = Xen_real_to_C_double(ey0);
  y1 = Xen_real_to_C_double(ey1);
  sum = y0;
  first_sum = sum;

  for (i = 0,  x = x0; i < lim; i += 2, x += xincr)
    {
      e[i] = sum;
      last_sum = sum;
      e[i + 1] = x;
      while ((x >= x1) && ((e_loc + 2) < e_len))
	{
	  x0 = x1;
	  y0 = y1;
	  e_loc += 2;
	  ex1 = Xen_list_ref(dist, e_loc);
	  ey1 = Xen_list_ref(dist, e_loc + 1);
	  x1 = Xen_real_to_C_double(ex1);
	  y1 = Xen_real_to_C_double(ey1);
	}
      if ((x == x0) || (x0 == x1))
	sum += y0;
      else sum += (y0 + (y1 - y0) * (x - x0) / (x1 - x0));
    }

  xincr = (last_sum - first_sum) / (mus_float_t)(data_size - 1);
  data = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
  x0 = e[0];
  x1 = e[2];
  y0 = e[1];
  y1 = e[3];
  e_len = lim;
  e_loc = 2;

  for (i = 0, x = first_sum; i < data_size; i++, x += xincr)
    {
      while ((x >= x1) && ((e_loc + 2) < e_len))
	{
	  x0 = x1;
	  y0 = y1;
	  e_loc += 2;
	  x1 = e[e_loc];
	  y1 = e[e_loc + 1];
	}
      if ((x == x0) || (x0 == x1))
	data[i] = y0;
      else data[i] = (y0 + (y1 - y0) * (x - x0) / (x1 - x0));
    }
  free(e);
  return(data);
}


#define H_make_rand_interp "(" S_make_rand_interp " (frequency 0.0) (amplitude 1.0) envelope distribution size): \
return a new " S_rand_interp " generator, producing linearly interpolated random numbers. \
frequency is the rate at which new end-points are chosen."

#define H_make_rand "(" S_make_rand " (frequency 0.0) (amplitude 1.0) envelope distribution size): \
return a new " S_rand " generator, producing a sequence of random numbers (a step  function). \
frequency is the rate at which new numbers are chosen."

#if HAVE_SCHEME
static s7_pointer g_make_noi(s7_scheme *sc, s7_pointer args, bool rand_case, const char *caller)
{
  mus_any *ge;
  mus_float_t freq, base;
  mus_float_t *distribution = NULL;
  s7_pointer v = NULL, p, fp;
  int distribution_size = RANDOM_DISTRIBUTION_TABLE_SIZE;
  
  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > mus_srate())
    Xen_out_of_range_error(caller, 1, s7_car(args), "freq > srate?");
  
  p = s7_cddr(args);
  fp = s7_caddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, caller, 5, fp, "an integer"));
      distribution_size = s7_integer(fp);
      if (distribution_size <= 0)
	Xen_out_of_range_error(caller, 5, fp, "distribution size <= 0?");
      if (distribution_size > mus_max_table_size())
	Xen_out_of_range_error(caller, 5, fp, "distribution size too large (see mus-max-table-size)");
    }

  fp = s7_car(p);
  if (fp != Xen_false)
    {
      int len;
      len = s7_list_length(sc, fp);
      if ((len < 4) || (len & 1))
	return(s7_wrong_type_arg_error(sc, caller, 3, fp, "a distribution envelope"));
      if (s7_cadr(p) != Xen_false)
	clm_error(caller, ":envelope and :distribution in same call?", args);
      distribution = inverse_integrate(fp, distribution_size);
      v = xen_make_vct(distribution_size, distribution);
    }
  else
    {
      fp = s7_cadr(p);
      if (fp != Xen_false)
	{
	  if (!s7_is_float_vector(fp))
	    return(s7_wrong_type_arg_error(sc, caller, 4, fp, "a float-vector"));
	  v = fp;
	  distribution_size = s7_vector_length(v);
	  distribution = s7_float_vector_elements(v);
	}
    }

  base = s7_number_to_real(sc, s7_cadr(args));

  if (!distribution)
    {
      if (rand_case)
	ge = mus_make_rand(freq, base);
      else ge = mus_make_rand_interp(freq, base);
    }
  else
    {
      if (rand_case)
	ge = mus_make_rand_with_distribution(freq, base, distribution, distribution_size);
      else ge = mus_make_rand_interp_with_distribution(freq, base, distribution, distribution_size);
    }
  if (ge)
    {
      if (v)
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
      return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
    }
  return(Xen_false);
}

static s7_pointer g_make_rand_interp(s7_scheme *sc, s7_pointer args) {return(g_make_noi(sc, args, false, S_make_rand_interp));}
static s7_pointer g_make_rand(s7_scheme *sc, s7_pointer args)        {return(g_make_noi(sc, args, true, S_make_rand));}
#else
static Xen g_make_noi(bool rand_case, const char *caller, Xen arglist)
{
  mus_any *ge = NULL;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals;
  mus_float_t freq = 0.0, base = 1.0;
  mus_float_t *distribution = NULL;
  Xen orig_v = Xen_false;
  int distribution_size = RANDOM_DISTRIBUTION_TABLE_SIZE;

  keys[0] = kw_frequency;
  keys[1] = kw_amplitude;
  keys[2] = kw_envelope;
  keys[3] = kw_distribution;
  keys[4] = kw_size;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(caller, arglist_len, 5, keys, args, orig_arg);
  }

  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > mus_srate())
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate?");

      base = Xen_optkey_to_float(kw_amplitude, keys[1], caller, orig_arg[1], base);

      distribution_size = Xen_optkey_to_int(kw_size, keys[4], caller, orig_arg[4], distribution_size);
      if (distribution_size <= 0)
	Xen_out_of_range_error(caller, orig_arg[4], keys[4], "distribution size <= 0?");
      if (distribution_size > mus_max_table_size())
	Xen_out_of_range_error(caller, orig_arg[4], keys[4], "distribution size too large (see mus-max-table-size)");

      if (!(Xen_is_keyword(keys[2]))) /* i.e. envelope arg was specified */
        {
	  int len;
	  Xen_check_type(Xen_is_list(keys[2]), keys[2], orig_arg[2], caller, "an envelope");
	  len = Xen_list_length(keys[2]);
	  if ((len < 4) || (len & 1))
	    clm_error(caller, "bad distribution envelope", keys[2]);
	  /* envelope and distribution are incompatible */
	  if (!(Xen_is_keyword(keys[3])))
	    clm_error(caller, ":envelope and :distribution in same call?", keys[3]);
	  distribution = inverse_integrate(keys[2], distribution_size);
	  orig_v = xen_make_vct(distribution_size, distribution);
	}
      else
	{
	  if (!(Xen_is_keyword(keys[3]))) /* i.e. distribution arg was specified */
	    {
	      Xen_check_type(mus_is_vct(keys[3]) || Xen_is_false(keys[3]), keys[3], orig_arg[3], caller, "a " S_vct);
	      if (mus_is_vct(keys[3]))
		{
		  vct *v = NULL;
		  orig_v = keys[3];
		  v = mus_optkey_to_vct(orig_v, caller, orig_arg[3], NULL);
		  distribution_size = mus_vct_length(v);
		  distribution = mus_vct_data(v);
		}
	    }
	}
    }
  if (!distribution)
    {
      if (rand_case)
	ge = mus_make_rand(freq, base);
      else ge = mus_make_rand_interp(freq, base);
    }
  else
    {
      if (rand_case)
	ge = mus_make_rand_with_distribution(freq, base, distribution, distribution_size);
      else ge = mus_make_rand_interp_with_distribution(freq, base, distribution, distribution_size);
    }
  if (ge)
    {
      if (mus_is_vct(orig_v))
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
      return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
    }
  return(Xen_false);
}

static Xen g_make_rand_interp(Xen arglist) {return(g_make_noi(false, S_make_rand_interp, arglist));}
static Xen g_make_rand(Xen arglist)        {return(g_make_noi(true, S_make_rand, arglist));}
#endif

static Xen g_rand(Xen obj, Xen fm)
{
  #define H_rand "(" S_rand " gen (fm 0.0)): gen's current random number. \
fm modulates the rate at which the current number is changed."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rand, S_rand, "a rand generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rand, 2);

  return(C_double_to_Xen_real(mus_rand(g, fm1)));
}


static Xen g_is_rand(Xen obj) 
{
  #define H_is_rand "(" S_is_rand " gen): " PROC_TRUE " if gen is a " S_rand
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_rand(Xen_to_mus_any(obj)))));
}


static Xen g_rand_interp(Xen obj, Xen fm)
{
  #define H_rand_interp "(" S_rand_interp " gen (fm 0.0)): gen's current (interpolating) random number. \
fm modulates the rate at which new segment end-points are chosen."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rand_interp, S_rand_interp, "a rand-interp generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rand_interp, 2);

  return(C_double_to_Xen_real(mus_rand_interp(g, fm1)));
}


static Xen g_is_rand_interp(Xen obj) 
{
  #define H_is_rand_interp "(" S_is_rand_interp " gen): " PROC_TRUE " if gen is a " S_rand_interp
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_rand_interp(Xen_to_mus_any(obj)))));
}


static Xen g_mus_random(Xen a) 
{
  #define H_mus_random "(" S_mus_random " val): a random number between -val and val. \
the built-in 'random' function returns values between 0 and its argument"
  mus_float_t x;

  Xen_to_C_double_or_error(a, x, S_mus_random, 1);
  return(C_double_to_Xen_real(mus_random(x)));
}


static Xen g_mus_rand_seed(void) 
{
  #define H_mus_rand_seed "(" S_mus_rand_seed "): the random number seed; \
this can be used to re-run a particular random number sequence."

  return(C_int_to_Xen_integer(mus_rand_seed()));
}


static Xen g_mus_set_rand_seed(Xen a) 
{
  Xen_check_type(Xen_is_integer(a), a, 1, S_set S_mus_rand_seed, "an integer");
  mus_set_rand_seed((unsigned long)Xen_integer_to_C_int(a)); 
  return(a);
}



/* ---------------- table lookup ---------------- */

static Xen g_is_table_lookup(Xen obj) 
{
  #define H_is_table_lookup "(" S_is_table_lookup " gen): " PROC_TRUE " if gen is a " S_table_lookup
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_table_lookup(Xen_to_mus_any(obj)))));
}


static Xen g_partials_to_wave(Xen partials, Xen utable, Xen normalize)
{
  #define H_partials_to_wave "(" S_partials_to_wave " partials wave (normalize " PROC_FALSE ")): \
take a list or " S_vct " of partials (harmonic number and associated amplitude) and produce \
a waveform for use in " S_table_lookup ".  If wave (a " S_vct ") is not given, \
a new one is created.  If normalize is " PROC_TRUE ", the resulting waveform goes between -1.0 and 1.0.\n\
  (set! gen (" S_make_table_lookup " 440.0 :wave (" S_partials_to_wave " '(1 1.0 2 .5))))"

  vct *f;
  Xen table; 
  mus_float_t *partial_data = NULL;
  mus_long_t len = 0;
  bool partials_allocated = true;
#if HAVE_SCHEME
  uint32_t gc_loc;
#endif

  Xen_check_type(mus_is_vct(partials) || Xen_is_list(partials), partials, 1, S_partials_to_wave, "a list or a " S_vct);
  Xen_check_type(mus_is_vct(utable) || Xen_is_false(utable) || (!(Xen_is_bound(utable))), utable, 2, S_partials_to_wave, "a " S_vct " or " PROC_FALSE);
  Xen_check_type(Xen_is_boolean_or_unbound(normalize), normalize, 3, S_partials_to_wave, "a boolean");

  if (mus_is_vct(partials))
    {
      vct *v;
      v = Xen_to_vct(partials);
      partial_data = mus_vct_data(v);
      len = mus_vct_length(v);
      partials_allocated = false;
    }
  else
    {
      len = Xen_list_length(partials);
      if (len == 0)
	Xen_error(NO_DATA, 
		  Xen_list_2(C_string_to_Xen_string("~A: partials list empty?"), 
			     C_string_to_Xen_string(S_partials_to_wave)));

      if (!(Xen_is_number(Xen_car(partials))))
	Xen_check_type(false, partials, 1, S_partials_to_wave, "a list of numbers (partial numbers with amplitudes)");
    }
  if (len & 1)
    Xen_error(BAD_TYPE,
	      Xen_list_3(C_string_to_Xen_string("~A: odd length partials list? ~A"), 
			 C_string_to_Xen_string(S_partials_to_wave), 
			 partials));

  if ((!Xen_is_bound(utable)) || (!(mus_is_vct(utable))))
    {
      mus_float_t *wave;
      wave = (mus_float_t *)calloc(clm_table_size, sizeof(mus_float_t));
      if (!wave)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave table", S_partials_to_wave));
      table = xen_make_vct(clm_table_size, wave);
    }
  else table = utable;

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, table);
#endif

  f = Xen_to_vct(table);

  if (!partial_data)
    {
      Xen lst;
      int i;
      partial_data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      if (!partial_data)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate partials table", S_partials_to_wave));
      for (i = 0, lst = Xen_copy_arg(partials); i < len; i++, lst = Xen_cdr(lst)) 
	partial_data[i] = Xen_real_to_C_double(Xen_car(lst));
    }

  mus_partials_to_wave(partial_data, len / 2, mus_vct_data(f), mus_vct_length(f), (Xen_is_true(normalize)));

  if (partials_allocated)
    free(partial_data);

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif

  return(table);
}


static Xen g_phase_partials_to_wave(Xen partials, Xen utable, Xen normalize)
{
  vct *f;
  Xen table;
  mus_float_t *partial_data = NULL;
  mus_long_t len = 0;
  bool partials_allocated = true;
#if HAVE_SCHEME
  uint32_t gc_loc;
#endif

  #if HAVE_SCHEME
    #define pp2w_example "(" S_make_table_lookup " 440.0 :wave (" S_phase_partials_to_wave " (list  1 .75 0.0  2 .25 (* 3.14159 .5))))"
  #endif
  #if HAVE_RUBY
    #define pp2w_example "make_table_lookup(440.0, :wave, phase_partials2wave([1.0, 0.75, 0.0,  2.0, 0.25, 3.14159 * 0.5]))"
  #endif
  #if HAVE_FORTH
    #define pp2w_example "440.0 0.0 '( 1.0 0.75 0.0 2.0 0.25 3.14159 0.5 f* ) #f #f phase-partials->wave make-table-lookup"
  #endif

  #define H_phase_partials_to_wave "(" S_phase_partials_to_wave " partials wave (normalize " PROC_FALSE ")): \
take a list or " S_vct " of partials (harmonic number, amplitude, initial phase) and produce \
a waveform for use in " S_table_lookup ".  If wave (a " S_vct ") is not given, \
a new one is created.  If normalize is " PROC_TRUE ", the resulting waveform goes between -1.0 and 1.0.\n  " pp2w_example

  Xen_check_type(mus_is_vct(partials) || Xen_is_list(partials), partials, 1, S_phase_partials_to_wave, "a list or a " S_vct);
  Xen_check_type(mus_is_vct(utable) || Xen_is_false(utable) || (!(Xen_is_bound(utable))), utable, 2, S_phase_partials_to_wave, "a " S_vct " or " PROC_FALSE);
  Xen_check_type(Xen_is_boolean_or_unbound(normalize), normalize, 3, S_phase_partials_to_wave, "a boolean");

  if (mus_is_vct(partials))
    {
      vct *v;
      v = Xen_to_vct(partials);
      partial_data = mus_vct_data(v);
      len = mus_vct_length(v);
      partials_allocated = false;
    }
  else
    {
      len = Xen_list_length(partials);
      if (len == 0)
	Xen_error(NO_DATA,
		  Xen_list_2(C_string_to_Xen_string("~A: partials list empty?"),
			     C_string_to_Xen_string(S_phase_partials_to_wave)));

      if (!(Xen_is_number(Xen_car(partials))))
	Xen_check_type(false, partials, 1, S_phase_partials_to_wave, "a list of numbers (partial numbers with amplitudes and phases)");
    }
  if ((len % 3) != 0)
    Xen_error(Xen_make_error_type("wrong-type-arg"),
	      Xen_list_3(C_string_to_Xen_string("~A: partials list, ~A, should have 3 entries for each harmonic (number amp phase)"),
			 C_string_to_Xen_string(S_phase_partials_to_wave), 
			 partials));

  if ((!Xen_is_bound(utable)) || (!(mus_is_vct(utable))))
    {
      mus_float_t *wave;
      wave = (mus_float_t *)calloc(clm_table_size, sizeof(mus_float_t));
      if (!wave)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave table", S_phase_partials_to_wave));
      table = xen_make_vct(clm_table_size, wave);
    }
  else table = utable;

#if HAVE_SCHEME
  gc_loc = s7_gc_protect(s7, table);
#endif

  f = Xen_to_vct(table);

  if (!partial_data)
    {
      int i;
      Xen lst;
      partial_data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      if (!partial_data)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate partials table", S_phase_partials_to_wave));
      for (i = 0, lst = Xen_copy_arg(partials); i < len; i++, lst = Xen_cdr(lst)) 
	partial_data[i] = Xen_real_to_C_double(Xen_car(lst));
    }

  mus_phase_partials_to_wave(partial_data, len / 3, mus_vct_data(f), mus_vct_length(f), (Xen_is_true(normalize)));

  if (partials_allocated)
    free(partial_data);

#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
#endif

  return(table);
}

#define H_make_table_lookup "(" S_make_table_lookup " (frequency 0.0) (initial-phase 0.0) wave size type): \
return a new " S_table_lookup " generator.  \
The default table size is 512; use :size to set some other size, or pass your own " S_vct " as the 'wave'.\n\
   (set! gen (" S_make_table_lookup " 440.0 :wave (" S_partials_to_wave " '(1 1.0))))\n\
is the same in effect as " S_make_oscil ".  'type' sets the interpolation choice which defaults to " S_mus_interp_linear "."

#if HAVE_SCHEME
static s7_pointer g_make_table_lookup(s7_scheme *sc, s7_pointer args)
{
  mus_any *ge;
  mus_long_t table_size = clm_table_size;
  mus_float_t freq, phase;
  mus_float_t *table = NULL;
  s7_pointer v, p, fp;
  int interp_type = (int)MUS_INTERP_LINEAR;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_table_lookup, 1, s7_car(args), "freq > srate/2?");
  
  phase = s7_number_to_real(sc, s7_cadr(args));
  if (phase < 0.0)
    Xen_out_of_range_error(S_make_table_lookup, 2, s7_cadr(args), "initial phase <= 0.0?"); /* is this actually an error? */

  p = s7_cddr(args);
  v = s7_car(p);
  if (v != Xen_false)
    {
      if (!s7_is_float_vector(v))
	return(s7_wrong_type_arg_error(s7, S_make_table_lookup, 3, v, "a float-vector"));
      table = s7_float_vector_elements(v);
      table_size = s7_vector_length(v);
    }
  
  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(s7, S_make_table_lookup, 4, fp, "an integer"));
      table_size = s7_integer(fp);
      if (table_size <= 0)
	Xen_out_of_range_error(S_make_table_lookup, 4, fp, "size <= 0?");
      if (table_size > mus_max_table_size())
	Xen_out_of_range_error(S_make_table_lookup, 4, fp, "size too large (see mus-max-table-size)");
      if ((table) && (table_size > mus_vct_length(v)))
	Xen_out_of_range_error(S_make_table_lookup, 4, fp, "table size > wave size");
    }
  
  fp = s7_caddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(s7, S_make_table_lookup, 5, fp, "an integer"));
      interp_type = s7_integer(fp);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(S_make_table_lookup, 5, fp, "no such interp-type");
    }

  if (!table)
    {
      v = s7_make_float_vector(sc, table_size, 1, NULL);
      table = s7_float_vector_elements(v);
    }
  ge = mus_make_table_lookup(freq, phase, table, table_size, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
}
#else
static Xen g_make_table_lookup(Xen arglist)
{
  mus_any *ge;
  int vals;
  mus_long_t table_size = clm_table_size;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, MUS_INTERP_LINEAR};
  mus_float_t freq = 0.0, phase = 0.0;
  mus_float_t *table = NULL;
  Xen orig_v = Xen_false;
  int interp_type = (int)MUS_INTERP_LINEAR;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_wave;
  keys[3] = kw_size;
  keys[4] = kw_type;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_table_lookup, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_table_lookup, arglist_len, 5, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      vct *v = NULL;
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_table_lookup, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[0], keys[0], "freq > srate/2?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_table_lookup, orig_arg[1], phase);
      if (phase < 0.0)
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[1], keys[1], "initial phase <= 0.0?"); /* is this actually an error? */

      v = mus_optkey_to_vct(keys[2], S_make_table_lookup, orig_arg[2], NULL);
      if (v) 
	{
	  orig_v = keys[2];
	  table = mus_vct_data(v);
	  table_size = mus_vct_length(v);
	}

      table_size = Xen_optkey_to_mus_long_t(kw_size, keys[3], S_make_table_lookup, orig_arg[3], table_size);
      if (table_size <= 0)
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[3], keys[3], "size <= 0?");
      if (table_size > mus_max_table_size())
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[3], keys[3], "size too large (see mus-max-table-size)");
      if ((v) && (table_size > mus_vct_length(v)))
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[3], keys[3], "table size > wave size");

      interp_type = Xen_optkey_to_int(kw_type, keys[4], S_make_table_lookup, orig_arg[4], interp_type);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(S_make_table_lookup, orig_arg[4], keys[4], "no such interp-type");
    }

  if (!(mus_is_vct(orig_v)))
    {
      table = (mus_float_t *)calloc(table_size, sizeof(mus_float_t));
      if (!table)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate table-lookup table", S_make_table_lookup));
      orig_v = xen_make_vct(table_size, table);
    }
  ge = mus_make_table_lookup(freq, phase, table, table_size, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
}
#endif

static Xen g_table_lookup(Xen obj, Xen fm) 
{
  #define H_table_lookup "(" S_table_lookup " gen (fm 0.0)): interpolated table-lookup \
with 'wrap-around' when gen's phase marches off either end of its table."

  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_table_lookup, S_table_lookup, "a table-lookup generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_table_lookup, 2);

  return(C_double_to_Xen_real(mus_table_lookup(g, fm1)));
}



/* ---------------- sawtooth et al ---------------- */

#if HAVE_SCHEME
static s7_pointer g_make_sawtooth_wave(s7_scheme *sc, s7_pointer args)
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " (frequency 0.0) (amplitude 1.0) (initial-phase pi)): \
return a new " S_sawtooth_wave " generator."

  mus_any *ge;
  mus_float_t freq, base, phase;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > mus_srate())
    Xen_out_of_range_error(S_make_sawtooth_wave, 1, s7_car(args), "freq > srate/2?");
  base = s7_number_to_real(sc, s7_cadr(args));
  phase = s7_number_to_real(sc, s7_caddr(args));

  ge = mus_make_sawtooth_wave(freq, base, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

static s7_pointer g_make_square_wave(s7_scheme *sc, s7_pointer args)
{
  #define H_make_square_wave "(" S_make_square_wave " (frequency 0.0) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_square_wave " generator."

  mus_any *ge;
  mus_float_t freq, base, phase;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > mus_srate())
    Xen_out_of_range_error(S_make_square_wave, 1, s7_car(args), "freq > srate/2?");
  base = s7_number_to_real(sc, s7_cadr(args));
  phase = s7_number_to_real(sc, s7_caddr(args));

  ge = mus_make_square_wave(freq, base, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

static s7_pointer g_make_triangle_wave(s7_scheme *sc, s7_pointer args)
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " (frequency 0.0) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_triangle_wave " generator."

  mus_any *ge;
  mus_float_t freq, base, phase;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > mus_srate())
    Xen_out_of_range_error(S_make_triangle_wave, 1, s7_car(args), "freq > srate/2?");
  base = s7_number_to_real(sc, s7_cadr(args));
  phase = s7_number_to_real(sc, s7_caddr(args));

  ge = mus_make_triangle_wave(freq, base, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

static s7_pointer g_make_pulse_train(s7_scheme *sc, s7_pointer args)
{
  #define H_make_pulse_train "(" S_make_pulse_train " (frequency 0.0) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_pulse_train " generator.  This produces a sequence of impulses."

  mus_any *ge;
  mus_float_t freq, base, phase;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > mus_srate())
    Xen_out_of_range_error(S_make_pulse_train, 1, s7_car(args), "freq > srate/2?");
  base = s7_number_to_real(sc, s7_cadr(args));
  phase = s7_number_to_real(sc, s7_caddr(args));

  ge = mus_make_pulse_train(freq, base, phase);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

#else
typedef enum {G_SAWTOOTH_WAVE, G_SQUARE_WAVE, G_TRIANGLE_WAVE, G_PULSE_TRAIN} xclm_wave_t;

static Xen g_make_sw(xclm_wave_t type, mus_float_t def_phase, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  mus_any *ge = NULL;
  const char *caller = NULL;
  Xen args[6]; 
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, pr = 0;
  mus_float_t freq = 0.0, base = 1.0, phase;

  phase = def_phase;

  switch (type)
    {
      case G_SAWTOOTH_WAVE: caller = S_make_sawtooth_wave; break;
      case G_SQUARE_WAVE:   caller = S_make_square_wave;   break;
      case G_TRIANGLE_WAVE: caller = S_make_triangle_wave; break;
      case G_PULSE_TRAIN:   caller = S_make_pulse_train;   break;
    }

  keys[0] = kw_frequency;
  keys[1] = kw_amplitude;
  keys[2] = kw_initial_phase;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; 
	if (Aok(arg4)) {args[pr++] = arg4; if (Aok(arg5)) {args[pr++] = arg5; if (Aok(arg6)) {args[pr++] = arg6; }}}}}}
  vals = mus_optkey_unscramble(caller, pr, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > mus_srate())
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      base = Xen_optkey_to_float(kw_amplitude, keys[1], caller, orig_arg[1], base);
      phase = Xen_optkey_to_float(kw_initial_phase, keys[2], caller, orig_arg[2], phase);
    }

  switch (type)
    {
    case G_SAWTOOTH_WAVE: ge = mus_make_sawtooth_wave(freq, base, phase); break;
    case G_SQUARE_WAVE:   ge = mus_make_square_wave(freq, base, phase);   break;
    case G_TRIANGLE_WAVE: ge = mus_make_triangle_wave(freq, base, phase); break;
    case G_PULSE_TRAIN:   ge = mus_make_pulse_train(freq, base, phase);   break;
    }
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_make_sawtooth_wave(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_sawtooth_wave "(" S_make_sawtooth_wave " (frequency 0.0) (amplitude 1.0) (initial-phase pi)): \
return a new " S_sawtooth_wave " generator."

  return(g_make_sw(G_SAWTOOTH_WAVE, M_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_square_wave(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_square_wave "(" S_make_square_wave " (frequency 0.0) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_square_wave " generator."

  return(g_make_sw(G_SQUARE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_triangle_wave(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_triangle_wave "(" S_make_triangle_wave " (frequency 0.0) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_triangle_wave " generator."

  return(g_make_sw(G_TRIANGLE_WAVE, 0.0, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_pulse_train(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_pulse_train "(" S_make_pulse_train " (frequency 0.0) (amplitude 1.0) (initial-phase 0.0)): \
return a new " S_pulse_train " generator.  This produces a sequence of impulses."

  return(g_make_sw(G_PULSE_TRAIN, TWO_PI, arg1, arg2, arg3, arg4, arg5, arg6));
}
#endif

static Xen g_sawtooth_wave(Xen obj, Xen fm) 
{
  #define H_sawtooth_wave "(" S_sawtooth_wave " gen (fm 0.0)): next sawtooth sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_sawtooth_wave, S_sawtooth_wave, "a sawtooth-wave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_sawtooth_wave, 2);

  return(C_double_to_Xen_real(mus_sawtooth_wave(g, fm1)));
}


static Xen g_square_wave(Xen obj, Xen fm) 
{
  #define H_square_wave "(" S_square_wave " gen (fm 0.0)): next square wave sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_square_wave, S_square_wave, "a square-wave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_square_wave, 2);

  return(C_double_to_Xen_real(mus_square_wave(g, fm1)));
}


static Xen g_triangle_wave(Xen obj, Xen fm) 
{
  #define H_triangle_wave "(" S_triangle_wave " gen (fm 0.0)): next triangle wave sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_triangle_wave, S_triangle_wave, "a triangle-wave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_triangle_wave, 2);

  return(C_double_to_Xen_real(mus_triangle_wave(g, fm1)));
}


static Xen g_pulse_train(Xen obj, Xen fm) 
{
  #define H_pulse_train "(" S_pulse_train " gen (fm 0.0)): next pulse train sample from generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_pulse_train, S_pulse_train, "a pulse-train generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_pulse_train, 2);

  return(C_double_to_Xen_real(mus_pulse_train(g, fm1)));
}


static Xen g_is_sawtooth_wave(Xen obj) 
{
  #define H_is_sawtooth_wave "(" S_is_sawtooth_wave " gen): " PROC_TRUE " if gen is a " S_sawtooth_wave
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_sawtooth_wave(Xen_to_mus_any(obj)))));
}


static Xen g_is_square_wave(Xen obj) 
{
  #define H_is_square_wave "(" S_is_square_wave " gen): " PROC_TRUE " if gen is a " S_square_wave
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_square_wave(Xen_to_mus_any(obj)))));
}


static Xen g_is_triangle_wave(Xen obj) 
{
  #define H_is_triangle_wave "(" S_is_triangle_wave " gen): " PROC_TRUE " if gen is a " S_triangle_wave
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_triangle_wave(Xen_to_mus_any(obj)))));
}


static Xen g_is_pulse_train(Xen obj) 
{
  #define H_is_pulse_train "(" S_is_pulse_train " gen): " PROC_TRUE " if gen is a " S_pulse_train
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_pulse_train(Xen_to_mus_any(obj)))));
}



/* ---------------- asymmetric-fm ---------------- */

#if HAVE_SCHEME
static s7_pointer g_make_asymmetric_fm(s7_scheme *sc, s7_pointer args)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " (frequency 0.0) (initial-phase 0.0) (r 1.0) (ratio 1.0)): \
return a new " S_asymmetric_fm " generator."
  mus_any *ge;
  mus_float_t freq, phase, r, ratio;
  s7_pointer x;

  x = s7_car(args);
  freq = s7_number_to_real(s7, x);
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_asymmetric_fm, 1, x, "freq > srate/2?");

  args = s7_cdr(args);
  x = s7_car(args);
  phase = s7_number_to_real(s7, x);

  args = s7_cdr(args);
  x = s7_car(args);
  r = s7_number_to_real(s7, x);
  x = s7_cadr(args);
  ratio = s7_number_to_real(s7, x);
  
  ge = mus_make_asymmetric_fm(freq, phase, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_asymmetric_fm(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6, Xen arg7, Xen arg8)
{
  #define H_make_asymmetric_fm "(" S_make_asymmetric_fm " (frequency 0.0) (initial-phase 0.0) (r 1.0) (ratio 1.0)): \
return a new " S_asymmetric_fm " generator."

  mus_any *ge;
  Xen args[8]; 
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, pr = 0;
  mus_float_t freq = 0.0, phase = 0.0, r = 1.0, ratio = 1.0;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_r;
  keys[3] = kw_ratio;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; 
	if (Aok(arg4)) {args[pr++] = arg4; if (Aok(arg5)) {args[pr++] = arg5; if (Aok(arg6)) {args[pr++] = arg6;
	      if (Aok(arg7)) {args[pr++] = arg7; if (Aok(arg8)) {args[pr++] = arg8;}}}}}}}}
  vals = mus_optkey_unscramble(S_make_asymmetric_fm, pr, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_asymmetric_fm, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_asymmetric_fm, orig_arg[0], keys[0], "freq > srate/2?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_asymmetric_fm, orig_arg[1], phase);

      r = Xen_optkey_to_float(kw_r, keys[2], S_make_asymmetric_fm, orig_arg[2], r);

      ratio = Xen_optkey_to_float(kw_ratio, keys[3], S_make_asymmetric_fm, orig_arg[3], ratio);
    }

  ge = mus_make_asymmetric_fm(freq, phase, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif


static Xen g_asymmetric_fm(Xen obj, Xen index, Xen fm)
{
  #define H_asymmetric_fm "(" S_asymmetric_fm " gen (index 0.0) (fm 0.0)): next sample from asymmetric fm generator"
  mus_float_t fm1 = 0.0, index1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_asymmetric_fm, S_asymmetric_fm, "an asymmetric-fm generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_asymmetric_fm, 2);
  Xen_real_to_C_double_if_bound(index, index1, S_asymmetric_fm, 3);

  return(C_double_to_Xen_real(mus_asymmetric_fm(g, index1, fm1)));
}


static Xen g_is_asymmetric_fm(Xen obj) 
{
  #define H_is_asymmetric_fm "(" S_is_asymmetric_fm " gen): " PROC_TRUE " if gen is a " S_asymmetric_fm
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_asymmetric_fm(Xen_to_mus_any(obj)))));
}



/* ---------------- simple filters ---------------- */

#if HAVE_SCHEME
static s7_pointer g_make_one_zero(s7_scheme *sc, s7_pointer args)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1): return a new " S_one_zero " filter;  a0*x(n) + a1*x(n-1)"
  mus_any *gen;
  mus_float_t a0, a1;
  
  a0 = s7_number_to_real(sc, s7_car(args));
  a1 = s7_number_to_real(sc, s7_cadr(args));
  gen = mus_make_one_zero(a0, a1);

  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static s7_pointer g_make_one_pole(s7_scheme *sc, s7_pointer args)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1): return a new " S_one_pole " filter; a0*x(n) - b1*y(n-1)"
  mus_any *gen;
  mus_float_t a0, b1;

  a0 = s7_number_to_real(sc, s7_car(args));
  b1 = s7_number_to_real(sc, s7_cadr(args));
  gen = mus_make_one_pole(a0, b1);

  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

/* make-two-zero and make-two-pole are harder because they can accept two different sets of keyword arguments */
static s7_pointer g_make_two_zero_a(s7_scheme *sc, s7_pointer args)
{
  mus_any *gen;
  mus_float_t a0, a1;

  a0 = s7_number_to_real(sc, s7_car(args));
  args = s7_cdr(args);
  a1 = s7_number_to_real(sc, s7_car(args));
  if (a0 < 20.0)
    {
      mus_float_t a2;
      a2 = s7_number_to_real(sc, s7_cadr(args));
      gen = mus_make_two_zero(a0, a1, a2);
    }
  else gen = mus_make_two_zero_from_frequency_and_radius(a0, a1);

  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static s7_pointer g_make_two_zero_f(s7_scheme *sc, s7_pointer args)
{
  mus_any *gen;
  mus_float_t freq, radius;

  freq = s7_number_to_real(sc, s7_car(args));
  radius = s7_number_to_real(sc, s7_cadr(args));
  gen = mus_make_two_zero_from_frequency_and_radius(freq, radius);

  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static s7_pointer make_two_zero_a, make_two_zero_f;
static s7_pointer g_make_two_zero(s7_scheme *sc, s7_pointer args)
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) or (" S_make_two_zero " frequency radius): return a new " S_two_zero " filter; a0*x(n) + a1*x(n-1) + a2*x(n-2)"
  int n;
  s7_pointer p;
  for (n = 0, p = args; s7_is_pair(p); n++, p = s7_cdr(p))
    {
      s7_pointer arg;
      if (n > 2)
	s7_wrong_number_of_args_error(sc, S_make_two_zero, args);
      arg = s7_car(p);
      if (s7_is_keyword(arg))
	{
	  if ((arg == kw_frequency) || (arg == kw_radius))
	    return(s7_apply_function_star(sc, make_two_zero_f, args));
	  return(s7_apply_function_star(sc, make_two_zero_a, args));
	}
    }
  return(s7_apply_function_star(sc, make_two_zero_a, args));
}


static s7_pointer g_make_two_pole_f(s7_scheme *sc, s7_pointer args)
{
  mus_any *gen;
  mus_float_t freq, radius;

  freq = s7_number_to_real(sc, s7_car(args));
  radius = s7_number_to_real(sc, s7_cadr(args));
  gen = mus_make_two_pole_from_frequency_and_radius(freq, radius);

  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static s7_pointer g_make_two_pole_a(s7_scheme *sc, s7_pointer args)
{
  mus_any *gen;
  mus_float_t a0, b1;

  a0 = s7_number_to_real(sc, s7_car(args));
  args = s7_cdr(args);
  b1 = s7_number_to_real(sc, s7_car(args));
  if (a0 < 2.0)
    {
      mus_float_t b2;
      b2 = s7_number_to_real(sc, s7_cadr(args));
      gen = mus_make_two_pole(a0, b1, b2);
    }
  else gen = mus_make_two_pole_from_frequency_and_radius(a0, b1);

  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static s7_pointer make_two_pole_a, make_two_pole_f;
static s7_pointer g_make_two_pole(s7_scheme *sc, s7_pointer args)
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) or (" S_make_two_pole " frequency radius): return a new " S_two_pole " filter; a0*x(n) - b1*y(n-1) - b2*y(n-2)"
  int n;
  s7_pointer p;
  for (n = 0, p = args; s7_is_pair(p); n++, p = s7_cdr(p))
    {
      s7_pointer arg;
      if (n > 2)
	s7_wrong_number_of_args_error(sc, S_make_two_pole, args);
      arg = s7_car(p);
      if (s7_is_keyword(arg))
	{
	  if ((arg == kw_frequency) || (arg == kw_radius))
	    return(s7_apply_function_star(sc, make_two_pole_f, args));
	  return(s7_apply_function_star(sc, make_two_pole_a, args));
	}
    }
  return(s7_apply_function_star(sc, make_two_pole_a, args));
}
#else

typedef enum {G_ONE_POLE, G_ONE_ZERO, G_TWO_POLE, G_TWO_ZERO} xclm_filter_t;

static const char *smpflts[6] = {S_make_one_pole, S_make_one_zero, S_make_two_pole, S_make_two_zero};


static Xen g_make_smpflt_1(xclm_filter_t choice, Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  mus_any *gen = NULL;
  Xen args[4]; 
  Xen keys[2];
  int orig_arg[2] = {0, 0};
  int vals, pr = 0;
  mus_float_t a0 = 0.0;
  mus_float_t a1 = 0.0;

  switch (choice)
    {
    case G_ONE_ZERO: keys[0] = kw_a0;        keys[1] = kw_a1;     break;
    case G_ONE_POLE: keys[0] = kw_a0;        keys[1] = kw_b1;     break;
    default:         keys[0] = kw_frequency; keys[1] = kw_radius; break;
    }

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; if (Aok(arg4)) {args[pr++] = arg4;}}}}
  vals = mus_optkey_unscramble(smpflts[choice], pr, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      a0 = mus_optkey_to_float(keys[0], smpflts[choice], orig_arg[0], a0);
      a1 = mus_optkey_to_float(keys[1], smpflts[choice], orig_arg[1], a1);
    }

  switch (choice)
    {
    case G_ONE_ZERO: gen = mus_make_one_zero(a0, a1); break;
    case G_ONE_POLE: gen = mus_make_one_pole(a0, a1); break;
    case G_TWO_ZERO: gen = mus_make_two_zero_from_frequency_and_radius(a0, a1); break;
    case G_TWO_POLE: gen = mus_make_two_pole_from_frequency_and_radius(a0, a1); break;
    default: break;
    }
  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static Xen g_make_one_zero(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_one_zero "(" S_make_one_zero " a0 a1): return a new " S_one_zero " filter;  a0*x(n) + a1*x(n-1)"
  return(g_make_smpflt_1(G_ONE_ZERO, arg1, arg2, arg3, arg4));
}


static Xen g_make_one_pole(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_one_pole "(" S_make_one_pole " a0 b1): return a new " S_one_pole " filter; a0*x(n) - b1*y(n-1)"
  return(g_make_smpflt_1(G_ONE_POLE, arg1, arg2, arg3, arg4));
}


static Xen g_make_smpflt_2(xclm_filter_t choice, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  mus_any *gen = NULL;
  Xen args[6]; 
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals, pr = 0;
  mus_float_t a0 = 0.0;
  mus_float_t a1 = 0.0;
  mus_float_t a2 = 0.0;
  if (choice == G_TWO_ZERO)
    {
      keys[0] = kw_a0;
      keys[1] = kw_a1;
      keys[2] = kw_a2;
    }
  else
    {
      keys[0] = kw_a0;
      keys[1] = kw_b1;
      keys[2] = kw_b2;
    }

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; 
	if (Aok(arg4)) {args[pr++] = arg4; if (Aok(arg5)) {args[pr++] = arg5; if (Aok(arg6)) {args[pr++] = arg6; }}}}}}
  vals = mus_optkey_unscramble(smpflts[choice], pr, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      a0 = Xen_optkey_to_float(kw_a0, keys[0], smpflts[choice], orig_arg[0], a0);
      a1 = mus_optkey_to_float(keys[1], smpflts[choice], orig_arg[1], a1);
      a2 = mus_optkey_to_float(keys[2], smpflts[choice], orig_arg[2], a2);
    }
  if (choice == G_TWO_ZERO)
    gen = mus_make_two_zero(a0, a1, a2);
  else gen = mus_make_two_pole(a0, a1, a2);
  if (gen) return(mus_xen_to_object(mus_any_to_mus_xen(gen)));
  return(Xen_false);
}

static bool found_polar_key(Xen arg)
{
  return((Xen_is_keyword(arg)) && 
	 ((Xen_keyword_is_eq(arg, kw_radius)) ||
	  (Xen_keyword_is_eq(arg, kw_frequency))));
}


static bool found_coeff_key(Xen arg)
{
  return((Xen_is_keyword(arg)) && 
	 (!(Xen_keyword_is_eq(arg, kw_radius))) &&
	 (!(Xen_keyword_is_eq(arg, kw_frequency))));
}

static Xen g_make_two_zero(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_two_zero "(" S_make_two_zero " a0 a1 a2) or (" S_make_two_zero " frequency radius): return a new " S_two_zero " filter; \
a0*x(n) + a1*x(n-1) + a2*x(n-2)"

  if ((Xen_is_bound(arg2)) && /* 0 or 1 args -> coeffs */
      (!(Xen_is_bound(arg5)))) /* 5 or more args -> coeffs */
    {
      if ((found_polar_key(arg1)) || 
	  (found_polar_key(arg2)) ||    /* if arg1 is frequency as number, then arg2 is either key or number */
	  ((!(Xen_is_bound(arg3))) &&    /* make a guess that if 2 args, no keys, and a0 > 20, it is intended as a frequency */
	   (!(found_coeff_key(arg1))) &&
	   ((Xen_is_number(arg1)) && (Xen_real_to_C_double(arg1) >= 20.0))))
	return(g_make_smpflt_1(G_TWO_ZERO, arg1, arg2, arg3, arg4));
    }

  return(g_make_smpflt_2(G_TWO_ZERO, arg1, arg2, arg3, arg4, arg5, arg6));
}


static Xen g_make_two_pole(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) 
{
  #define H_make_two_pole "(" S_make_two_pole " a0 b1 b2) or (" S_make_two_pole " frequency radius): return a new " S_two_pole " filter; \
a0*x(n) - b1*y(n-1) - b2*y(n-2)"

  if ((Xen_is_bound(arg2)) && /* 0 or 1 args -> coeffs */
      (!(Xen_is_bound(arg5)))) /* 5 or more args -> coeffs */
    {
      if ((found_polar_key(arg1)) || 
	  (found_polar_key(arg2)) ||    /* if arg1 is frequency as number, then arg2 is either key or number */
	  ((!(Xen_is_bound(arg3))) &&
	   (!(found_coeff_key(arg1))) &&
	   ((Xen_is_number(arg1)) && (Xen_real_to_C_double(arg1) >= 2.0))))
	return(g_make_smpflt_1(G_TWO_POLE, arg1, arg2, arg3, arg4));
    }

  return(g_make_smpflt_2(G_TWO_POLE, arg1, arg2, arg3, arg4, arg5, arg6));
}
#endif


static Xen g_one_zero(Xen obj, Xen fm)
{
  #define H_one_zero "(" S_one_zero " gen (input 0.0)): one zero filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_one_zero, S_one_zero, "a one-zero filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_one_zero, 2);

  return(C_double_to_Xen_real(mus_one_zero(g, fm1)));
}


static Xen g_one_pole(Xen obj, Xen fm)
{
  #define H_one_pole "(" S_one_pole " gen (input 0.0)): one pole filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_one_pole, S_one_pole, "a one-pole filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_one_pole, 2);

  return(C_double_to_Xen_real(mus_one_pole(g, fm1)));
}


static Xen g_two_zero(Xen obj, Xen fm)
{
  #define H_two_zero "(" S_two_zero " gen (input 0.0)): two zero filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_two_zero, S_two_zero, "a two-zero filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_two_zero, 2);

  return(C_double_to_Xen_real(mus_two_zero(g, fm1)));
}


static Xen g_two_pole(Xen obj, Xen fm)
{
  #define H_two_pole "(" S_two_pole " gen (input 0.0)): two pole filter of input"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_two_pole, S_two_pole, "a two-pole filter");
  Xen_real_to_C_double_if_bound(fm, fm1, S_two_pole, 2);

  return(C_double_to_Xen_real(mus_two_pole(g, fm1)));
}


static Xen g_is_one_zero(Xen obj) 
{
  #define H_is_one_zero "(" S_is_one_zero " gen): " PROC_TRUE " if gen is a " S_one_zero
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_one_zero(Xen_to_mus_any(obj)))));
}


static Xen g_is_one_pole(Xen obj) 
{
  #define H_is_one_pole "(" S_is_one_pole " gen): " PROC_TRUE " if gen is a " S_one_pole
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_one_pole(Xen_to_mus_any(obj)))));
}


static Xen g_is_two_zero(Xen obj) 
{
  #define H_is_two_zero "(" S_is_two_zero " gen): " PROC_TRUE " if gen is a " S_two_zero
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_two_zero(Xen_to_mus_any(obj)))));
}


static Xen g_is_two_pole(Xen obj) 
{
  #define H_is_two_pole "(" S_is_two_pole " gen): " PROC_TRUE " if gen is a " S_two_pole
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_two_pole(Xen_to_mus_any(obj)))));
}




/* ---------------- formant ---------------- */

#if HAVE_SCHEME
static s7_pointer g_make_formant(s7_scheme *sc, s7_pointer args)
{
  #define H_make_formant "(" S_make_formant " frequency radius): \
return a new formant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  mus_any *ge;
  mus_float_t freq, radius;
  
  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_formant, 1, s7_car(args), "freq > srate/2?");

  radius = s7_number_to_real(sc, s7_cadr(args));

  ge = mus_make_formant(freq, radius);
  if (ge)
    return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

static s7_pointer g_make_firmant(s7_scheme *sc, s7_pointer args)
{
  #define H_make_firmant "(" S_make_firmant " frequency radius): \
return a new firmant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  mus_any *ge;
  mus_float_t freq, radius;
  
  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_firmant, 1, s7_car(args), "freq > srate/2?");

  radius = s7_number_to_real(sc, s7_cadr(args));

  ge = mus_make_firmant(freq, radius);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_frm(bool formant_case, const char *caller, Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  mus_any *ge;
  int vals, pr = 0;
  Xen args[4]; 
  Xen keys[2];
  int orig_arg[2] = {0, 0};
  mus_float_t freq = 0.0, radius = 0.0;

  keys[0] = kw_frequency;
  keys[1] = kw_radius;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; if (Aok(arg4)) {args[pr++] = arg4;}}}}
  vals = mus_optkey_unscramble(caller, pr, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      radius = Xen_optkey_to_float(kw_radius, keys[1], caller, orig_arg[1], radius);
    }

  if (formant_case)
    {
      ge = mus_make_formant(freq, radius);
      if (ge)
	{
	  mus_xen *gn;
	  gn = mus_any_to_mus_xen(ge);
	  return(mus_xen_to_object(gn));
	}
    }
  else 
    {
      ge = mus_make_firmant(freq, radius);
      if (ge) 
	return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
    }
  return(Xen_false);
}
#endif

static Xen g_formant(Xen gen, Xen input, Xen freq)
{
  #define H_formant "(" S_formant " gen (input 0.0) freq-in-radians): next sample from resonator generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_formant, S_formant, "a formant generator");

  Xen_real_to_C_double_if_bound(input, in1, S_formant, 2);
  if (Xen_is_bound(freq))
    return(C_double_to_Xen_real(mus_formant_with_frequency(g, in1, Xen_real_to_C_double(freq))));

  return(C_double_to_Xen_real(mus_formant(g, in1)));
}

#if (!HAVE_SCHEME)
static Xen g_make_formant(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_formant "(" S_make_formant " frequency radius): \
return a new formant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  return(g_make_frm(true, S_make_formant, arg1, arg2, arg3, arg4));
}
#endif

static Xen g_is_formant(Xen os) 
{
  #define H_is_formant "(" S_is_formant " gen): " PROC_TRUE " if gen is a " S_formant
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_formant(Xen_to_mus_any(os)))));
}


static Xen g_set_formant_radius_and_frequency(Xen gen, Xen rad, Xen frq)
{
  #define H_mus_set_formant_radius_and_frequency  "(" S_mus_set_formant_radius_and_frequency  " gen radius frequency): set " S_formant " \
generator gen's radius and frequency"
  mus_any *g = NULL;
  mus_float_t radius, frequency;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_formant, S_mus_set_formant_radius_and_frequency, "a formant generator");

  Xen_to_C_double_or_error(rad, radius, S_mus_set_formant_radius_and_frequency, 2);
  Xen_to_C_double_or_error(frq, frequency, S_mus_set_formant_radius_and_frequency, 3);

  mus_set_formant_radius_and_frequency(g, radius, frequency);
  return(rad);
}


static Xen g_set_formant_frequency(Xen gen, Xen frq)
{
  #define H_mus_set_formant_frequency  "(" S_mus_set_formant_frequency  " gen frequency): set " S_formant " generator gen's frequency"
  mus_any *g = NULL;
  mus_float_t frequency;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_formant, S_mus_set_formant_frequency, "a formant generator");
  Xen_to_C_double_or_error(frq, frequency, S_mus_set_formant_frequency, 2);

  mus_set_formant_frequency(g, frequency);
  return(frq);
}


static Xen g_make_formant_bank(Xen frms, Xen amps)
{
  #define H_make_formant_bank "(" S_make_formant_bank " gens amps): return a new formant-bank generator."

  mus_any *ge = NULL;
  mus_any **gens;
  int i, j, size;
  vct *v = NULL;

  Xen_check_type(Xen_is_vector(frms), frms, 1, S_make_formant_bank, "a vector of formant generators");
  /* need size and elements -> mus_any */

  size = Xen_vector_length(frms);
  if (size == 0) return(Xen_false);
  gens = (mus_any **)calloc(size, sizeof(mus_any *));

  if (Xen_is_bound(amps))
    {
      v = Xen_to_vct(amps);
      if (!v) Xen_check_type(false, amps, 2, S_make_formant_bank, "a " S_vct " if anything");
    }

  for (i = 0, j = 0; i < size; i++)
    {
      Xen g;
      g = Xen_vector_ref(frms, i);
      if (mus_is_xen(g))
	{
	  mus_any *fg;
	  fg = Xen_to_mus_any(g);
	  if (mus_is_formant(fg))
	    gens[j++] = fg;
	}
    }
  if (j > 0)
    ge = mus_make_formant_bank(j, gens, (v) ? mus_vct_data(v) : NULL);
  free(gens);

  if (ge) 
    {
      if (v)
	return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, frms, amps)));
      return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, frms)));
    }
  return(Xen_false);
}


static Xen g_is_formant_bank(Xen os) 
{
  #define H_is_formant_bank "(" S_is_formant_bank " gen): " PROC_TRUE " if gen is a " S_formant_bank
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_formant_bank(Xen_to_mus_any(os)))));
}


static Xen g_formant_bank(Xen gens, Xen inp)
{
  #define H_formant_bank "(" S_formant_bank " gens inval): sum a bank of " S_formant " generators"
  mus_any *bank = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gens, gn, bank, mus_is_formant_bank, S_formant_bank, "a formant-bank generator");
  if (mus_is_vct(inp))
    return(C_double_to_Xen_real(mus_formant_bank_with_inputs(bank, mus_vct_data(Xen_to_vct(inp)))));

  if (Xen_is_number(inp))
    return(C_double_to_Xen_real(mus_formant_bank(bank, Xen_real_to_C_double(inp))));

  if (!Xen_is_bound(inp))
    return(C_double_to_Xen_real(mus_formant_bank(bank, 0.0)));

  Xen_check_type(false, inp, 2, S_formant_bank, "a number or a " S_vct);
  return(Xen_false);
}




/* ---------------- one-pole-all-pass ---------------- */

static Xen g_make_one_pole_all_pass(Xen arg1, Xen arg2)
{
  #define H_make_one_pole_all_pass "(" S_make_one_pole_all_pass " size coeff): return a new one-pole-all-pass generator."

  mus_any *ge = NULL;
  int size;
  mus_float_t coeff;

  Xen_check_type(Xen_is_integer(arg1), arg1, 1, S_make_one_pole_all_pass, "an integer");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(arg2), arg2, 2, S_make_one_pole_all_pass, "a number");
#endif

  size = Xen_integer_to_C_int(arg1);
  if (size < 0)
    Xen_out_of_range_error(S_make_one_pole_all_pass, 1, arg1, "size < 0?");
  if (size == 0) return(Xen_false);
  coeff = Xen_real_to_C_double(arg2);

  ge = mus_make_one_pole_all_pass(size, coeff);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_is_one_pole_all_pass(Xen os) 
{
  #define H_is_one_pole_all_pass "(" S_is_one_pole_all_pass " gen): " PROC_TRUE " if gen is a " S_one_pole_all_pass
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_one_pole_all_pass(Xen_to_mus_any(os)))));
}


static Xen g_one_pole_all_pass(Xen gen, Xen fm)
{
  #define H_one_pole_all_pass "(" S_one_pole_all_pass " gen (input 0.0)): run a one-pole-all-pass generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_one_pole_all_pass, S_one_pole_all_pass, "a one-pole-all-pass generator");
  Xen_real_to_C_double_if_bound(fm, in1, S_one_pole_all_pass, 2);
  return(C_double_to_Xen_real(mus_one_pole_all_pass(g, in1)));
}




/* ---------------- firmant ---------------- */

#if (!HAVE_SCHEME)
static Xen g_make_firmant(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_firmant "(" S_make_firmant " frequency radius): \
return a new firmant generator (a resonator).  radius sets the pole radius (in terms of the 'unit circle'). \
frequency sets the resonance center frequency (Hz)."

  return(g_make_frm(false, S_make_firmant, arg1, arg2, arg3, arg4));
}
#endif

static Xen g_is_firmant(Xen os) 
{
  #define H_is_firmant "(" S_is_firmant " gen): " PROC_TRUE " if gen is a " S_firmant " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_firmant(Xen_to_mus_any(os)))));
}


static Xen g_firmant(Xen gen, Xen input, Xen freq)
{
  #define H_firmant "(" S_firmant " gen (input 0.0) freq-in-radians): next sample from resonator generator"
  mus_float_t in1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(gen, gn, g, mus_is_firmant, S_firmant, "a firmant generator");

  Xen_real_to_C_double_if_bound(input, in1, S_firmant, 2);
  if (Xen_is_bound(freq)) 
    return(C_double_to_Xen_real(mus_firmant_with_frequency(g, in1, Xen_real_to_C_double(freq))));

  return(C_double_to_Xen_real(mus_firmant(g, in1)));
}


static mus_float_t mus_pink_noise(vct *v)
{
  int i, size;
  mus_float_t sum = 0.0, amp, x;
  mus_float_t *data;

  size = mus_vct_length(v);
  data = mus_vct_data(v);
  amp = data[0];

  for (i = 2, x = 0.5; i < size; i += 2, x *= 0.5)
    {
      sum += data[i];
      data[i + 1] -= x;
      if (data[i + 1] < 0.0)
	{
	  data[i] = mus_random(amp);
	  data[i + 1] += 1.0;
	}
    }
  return(sum + mus_random(amp));
}

#define S_pink_noise "pink-noise"
static Xen g_pink_noise(Xen gens)
{
  #define H_pink_noise "(pink-noise gens) generates an approximation to pink noise."
  int size;
  vct *v;

  Xen_check_type((mus_is_vct(gens)) && (Xen_vector_rank(gens) == 1), gens, 1, S_pink_noise, "a " S_vct);
  v = Xen_to_vct(gens);
  size = mus_vct_length(v);
  if (size == 0)
    return(xen_float_zero);
  Xen_check_type((size & 1) == 0, gens, 1, S_pink_noise, "an even length " S_vct);

  return(C_double_to_Xen_real(mus_pink_noise(v)));
}

#if HAVE_SCHEME
static s7_double piano_noise(s7_int *g, s7_double noi)
{
  g[0] = ((g[0] * 1103515245) + 12345) & 0xffffffff;
  noi *= (((s7_double)g[0] * 4.6566128730774e-10) - 1.0);
  return(noi);
}

#define S_piano_noise "piano-noise"
static Xen g_piano_noise(Xen gen, XEN amp)
{
  #define H_piano_noise "(piano-noise gen amp) generates the noise used in the piano instrument."

  if (!s7_is_int_vector(gen)) s7_wrong_type_arg_error(s7, S_piano_noise, 1, gen, "an int-vector");
  if (!s7_is_real(amp)) s7_wrong_type_arg_error(s7, S_piano_noise, 2, amp, "a real");

  return(C_double_to_Xen_real(piano_noise(s7_int_vector_elements(gen), Xen_real_to_C_double(amp))));
}

static s7_double piano_noise_d_pd(s7_pointer v, s7_double x)
{
  return(piano_noise(s7_int_vector_elements(v), x));
}


#define S_singer_filter "singer-filter"
static Xen g_singer_filter(Xen start, Xen end, Xen tmp, Xen dline1, Xen dline2, Xen coeffs)
{
  #define H_singer_filter "this is an optimization for the singer instrument"
  int j, k, beg, lim;
  s7_double *d1, *d2, *cf;
  s7_double temp;

  if (!s7_is_integer(start)) s7_wrong_type_arg_error(s7, S_singer_filter, 1, start, "an integer");
  if (!s7_is_integer(end)) s7_wrong_type_arg_error(s7, S_singer_filter, 2, end, "an integer");
  if (!s7_is_real(tmp)) s7_wrong_type_arg_error(s7, S_singer_filter, 3, tmp, "a real");
  if (!s7_is_float_vector(dline1)) s7_wrong_type_arg_error(s7, S_singer_filter, 4, dline1, "a float-vector");
  if (!s7_is_float_vector(dline2)) s7_wrong_type_arg_error(s7, S_singer_filter, 5, dline2, "a float-vector");
  if (!s7_is_float_vector(coeffs)) s7_wrong_type_arg_error(s7, S_singer_filter, 6, coeffs, "a float-vector");
  
  beg = s7_integer(start);
  lim = s7_integer(end);
  d1 = s7_float_vector_elements(dline1);
  d2 = s7_float_vector_elements(dline2);
  cf = s7_float_vector_elements(coeffs);
  temp = s7_number_to_real(s7, tmp);

  for (k = beg, j = beg + 1; j < lim; k++, j++)
    {
      s7_double temp1, x;
      x = d2[j + 1];
      d2[j] = x + (cf[j] * (d1[k] - x));
      temp1 = temp;
      temp = d1[k] + d2[j] - x;
      d1[k] = temp1;
    }
  return(s7_make_real(s7, temp));
}


#define S_singer_nose_filter "singer-nose-filter"
static Xen g_singer_nose_filter(Xen end, Xen tmp, Xen dline1, Xen dline2, Xen coeffs)
{
  #define H_singer_nose_filter "this is an optimization for the singer instrument"
  int j, k, lim;
  s7_double *d1, *d2, *cf;
  s7_double temp;

  if (!s7_is_integer(end)) s7_wrong_type_arg_error(s7, S_singer_nose_filter, 1, end, "an integer");
  if (!s7_is_real(tmp)) s7_wrong_type_arg_error(s7, S_singer_nose_filter, 2, tmp, "a real");
  if (!s7_is_float_vector(dline1)) s7_wrong_type_arg_error(s7, S_singer_nose_filter, 3, dline1, "a float-vector");
  if (!s7_is_float_vector(dline2)) s7_wrong_type_arg_error(s7, S_singer_nose_filter, 4, dline2, "a float-vector");
  if (!s7_is_float_vector(coeffs)) s7_wrong_type_arg_error(s7, S_singer_nose_filter, 5, coeffs, "a float-vector");
  
  lim = s7_integer(end);
  d1 = s7_float_vector_elements(dline1);
  d2 = s7_float_vector_elements(dline2);
  cf = s7_float_vector_elements(coeffs);
  temp = s7_number_to_real(s7, tmp);

  for (k = 1, j = 2; j < lim; k++, j++)
    {
      s7_double t1, reftemp;
      reftemp = cf[j] * (d1[k] - d2[j + 1]);
      d2[j] = d2[j + 1] + reftemp;
      t1 = temp;
      temp = d1[k] + reftemp;
      d1[k] = t1;
    }

  return(s7_make_real(s7, temp));
}

#endif



/* ---------------- wave-train ---------------- */

#define H_make_wave_train "(" S_make_wave_train " (frequency 0.0) (initial-phase 0.0) wave size type): \
return a new wave-train generator (an extension of pulse-train).   Frequency is \
the repetition rate of the wave found in wave. Successive waves can overlap."

#if HAVE_SCHEME
static s7_pointer g_make_wave_train(s7_scheme *sc, s7_pointer args)
{
  /* almost the same as make-table-lookup */
  mus_any *ge;
  mus_long_t table_size = clm_table_size;
  mus_float_t freq, phase;
  mus_float_t *table = NULL;
  s7_pointer v, p, fp;
  int interp_type = (int)MUS_INTERP_LINEAR;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_wave_train, 1, s7_car(args), "freq > srate/2?");
  if (freq < 0.0)
    Xen_out_of_range_error(S_make_wave_train, 1, s7_car(args), "freq < 0.0?");
  
  phase = s7_number_to_real(sc, s7_cadr(args));
  if (phase < 0.0)
    Xen_out_of_range_error(S_make_wave_train, 2, s7_cadr(args), "initial phase <= 0.0?");

  p = s7_cddr(args);
  v = s7_car(p);
  if (v != Xen_false)
    {
      if (!s7_is_float_vector(v))
	return(s7_wrong_type_arg_error(s7, S_make_wave_train, 3, v, "a float-vector"));
      table = s7_float_vector_elements(v);
      table_size = s7_vector_length(v);
    }
  
  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(s7, S_make_wave_train, 4, fp, "an integer"));
      table_size = s7_integer(fp);
      if (table_size <= 0)
	Xen_out_of_range_error(S_make_wave_train, 4, fp, "size <= 0?");
      if (table_size > mus_max_table_size())
	Xen_out_of_range_error(S_make_wave_train, 4, fp, "size too large (see mus-max-table-size)");
      if ((table) && (table_size > mus_vct_length(v)))
	Xen_out_of_range_error(S_make_wave_train, 4, fp, "table size > wave size");
    }
  
  fp = s7_caddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(s7, S_make_wave_train, 5, fp, "an integer"));
      interp_type = s7_integer(fp);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(S_make_wave_train, 5, fp, "no such interp-type");
    }

  if (!table)
    {
      v = s7_make_float_vector(sc, table_size, 1, NULL);
      table = s7_float_vector_elements(v);
    }
  ge = mus_make_wave_train(freq, phase, table, table_size, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
}
#else
static Xen g_make_wave_train(Xen arglist)
{
  mus_any *ge;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, MUS_INTERP_LINEAR};
  int vals;
  mus_long_t wsize = clm_table_size;
  Xen orig_v = Xen_false;
  mus_float_t freq = 0.0, phase = 0.0;
  mus_float_t *wave = NULL;
  int interp_type = (int)MUS_INTERP_LINEAR;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_wave;
  keys[3] = kw_size;
  keys[4] = kw_type;

  {
    Xen p;
    int i, arglist_len;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_wave_train, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_wave_train, arglist_len, 5, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      vct *v = NULL;
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_wave_train, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_wave_train, orig_arg[0], keys[0], "freq > srate/2?");
      if (freq < 0.0)
	Xen_out_of_range_error(S_make_wave_train, orig_arg[0], keys[0], "freq < 0.0?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_wave_train, orig_arg[1], phase);
      if (phase < 0.0)
	Xen_out_of_range_error(S_make_wave_train, orig_arg[1], keys[1], "phase < 0.0?");

      v = mus_optkey_to_vct(keys[2], S_make_wave_train, orig_arg[2], NULL);
      if (v)
	{
	  orig_v = keys[2];
	  wave = mus_vct_data(v);
	  wsize = mus_vct_length(v);
	}

      wsize = Xen_optkey_to_mus_long_t(kw_size, keys[3], S_make_wave_train, orig_arg[3], wsize);
      if (wsize <= 0)
	Xen_out_of_range_error(S_make_wave_train, orig_arg[3], keys[3], "size <= 0?");
      if (wsize > mus_max_table_size())
	Xen_out_of_range_error(S_make_wave_train, orig_arg[3], keys[3], "size too large (see mus-max-table-size)");
      if ((v) && (wsize > mus_vct_length(v)))
	Xen_out_of_range_error(S_make_wave_train, orig_arg[3], keys[3], "table size > wave size");

      interp_type = Xen_optkey_to_int(kw_type, keys[4], S_make_wave_train, orig_arg[4], interp_type);
      if (!(mus_is_interp_type(interp_type)))
	Xen_out_of_range_error(S_make_wave_train, orig_arg[4], keys[4], "no such interp-type");
    }

  if (!wave) 
    {
      wave = (mus_float_t *)calloc(wsize, sizeof(mus_float_t));
      if (!wave)
	return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate wave-train table", S_make_wave_train));
      orig_v = xen_make_vct(wsize, wave);
    }
  ge = mus_make_wave_train(freq, phase, wave, wsize, (mus_interp_t)interp_type);
  return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
}
#endif

static Xen g_wave_train(Xen obj, Xen fm)
{
  #define H_wave_train "(" S_wave_train " gen (fm 0.0)): next sample of " S_wave_train
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_wave_train, S_wave_train, "a wave-train generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_wave_train, 2);

  return(C_double_to_Xen_real(mus_wave_train(g, fm1)));
}


static Xen g_is_wave_train(Xen obj) 
{
  #define H_is_wave_train "(" S_is_wave_train " gen): " PROC_TRUE " if gen is a " S_wave_train
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_wave_train(Xen_to_mus_any(obj)))));
}



/* ---------------- waveshaping ---------------- */

enum {NO_PROBLEM_IN_LIST, NULL_LIST, ODD_LENGTH_LIST, NON_NUMBER_IN_LIST, NON_INTEGER_IN_LIST, NEGATIVE_NUMBER_IN_LIST, HUGE_NUMBER_IN_LIST};

static const char *list_to_partials_error_to_string(int code)
{
  switch (code)
    {
    case NO_PROBLEM_IN_LIST:          return("~A: nothing wrong with partials list?? ~A");                    
    case NULL_LIST:                   return("~A: partials list is null, ~A");                                
    case ODD_LENGTH_LIST:             return("~A: partials list has an odd number of elements: ~A");          
    case NON_NUMBER_IN_LIST:          return("~A: partials list has a non-numerical element: ~A");            
    case NON_INTEGER_IN_LIST:         return("~A: partials list has a non-integral harmonic number: ~A");            
    case NEGATIVE_NUMBER_IN_LIST:     return("~A: partials list has a partial number that is negative: ~A");  
    case HUGE_NUMBER_IN_LIST:         return("~A: partials list has a partial number that is too large: ~A"); 
    }
  return("~A: unknown error, ~A");
}


static mus_float_t *list_to_partials(Xen harms, int *npartials, int *error_code)
{
  int listlen, i, maxpartial = 0, curpartial;
  mus_float_t *partials = NULL;
  Xen lst;

  listlen = Xen_list_length(harms);
  if (listlen == 0)
    {
      (*error_code) = NULL_LIST;
      return(NULL);
    }

  if (listlen & 1)
    {
      (*error_code) = ODD_LENGTH_LIST;
      return(NULL);
    }

  if (!(Xen_is_number(Xen_car(harms)))) 
    {
      (*error_code) = NON_NUMBER_IN_LIST;
      return(NULL);
    }
  /* the list is '(partial-number partial-amp ... ) */
  (*error_code) = NO_PROBLEM_IN_LIST;

  for (i = 0, lst = Xen_copy_arg(harms); i < listlen; i += 2, lst = Xen_cddr(lst))
    {
      if (!(Xen_is_integer(Xen_car(lst))))
	{
	  (*error_code) = NON_INTEGER_IN_LIST;
	  return(NULL);
	}
      if (!(Xen_is_number(Xen_cadr(lst))))
	{
	  (*error_code) = NON_NUMBER_IN_LIST;
	  return(NULL);
	}
      curpartial = Xen_integer_to_C_int(Xen_car(lst));
      if (curpartial < 0)
	{
	  (*error_code) = NEGATIVE_NUMBER_IN_LIST;
	  return(NULL);
	}
      if (curpartial > maxpartial) 
	maxpartial = curpartial;
    }

  if (maxpartial > 1000000)
    {
      (*error_code) = HUGE_NUMBER_IN_LIST;
      return(NULL);
    }

  partials = (mus_float_t *)calloc(maxpartial + 1, sizeof(mus_float_t));
  /* here and elsewhere? this won't be null until we touch it in linux, but that gloms up all our
   *   code with once-in-a-billion-years error checks.
   */
  if (!partials)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate waveshaping partials list");
  (*npartials) = maxpartial + 1;

  for (i = 0, lst = Xen_copy_arg(harms); i < listlen; i += 2, lst = Xen_cddr(lst))
    {
      curpartial = Xen_integer_to_C_int(Xen_car(lst));
      partials[curpartial] = (mus_float_t)Xen_real_to_C_double(Xen_cadr(lst));
    }
  return(partials);
}


static mus_float_t *mus_vct_to_partials(vct *v, int *npartials, int *error_code)
{
  int len, i, maxpartial, curpartial;
  mus_float_t *partials = NULL, *vdata;

  len = mus_vct_length(v);
  if (len == 0)
    {
      (*error_code) = NULL_LIST;
      return(NULL);
    }
  if (len & 1)
    {
      (*error_code) = ODD_LENGTH_LIST;
      return(NULL);
    }
  (*error_code) = NO_PROBLEM_IN_LIST;

  vdata = mus_vct_data(v);
  maxpartial = (int)(vdata[0]);
  if (maxpartial < 0)
    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
  else
    {
      for (i = 2; i < len; i += 2)
	{
	  curpartial = (int)(vdata[i]);
	  if (curpartial > maxpartial) 
	    maxpartial = curpartial;
	  if (curpartial < 0)
	    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
	}
    }
  if (maxpartial > 1000000)
    {
      (*error_code) = HUGE_NUMBER_IN_LIST;
      return(NULL);
    }
  if ((*error_code) != NO_PROBLEM_IN_LIST)
    return(NULL);

  partials = (mus_float_t *)calloc(maxpartial + 1, sizeof(mus_float_t));
  if (!partials)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate waveshaping partials list");
  (*npartials) = maxpartial + 1;

  for (i = 0; i < len; i += 2)
    {
      curpartial = (int)(vdata[i]);
      partials[curpartial] = vdata[i + 1];
    }
  return(partials);
}


static mus_float_t *mus_vector_to_partials(Xen v, int *npartials, int *error_code)
{
  int len, i, maxpartial, curpartial;
  mus_float_t *partials = NULL;

  len = Xen_vector_length(v);
  if (len == 0)
    {
      (*error_code) = NULL_LIST;
      return(NULL);
    }
  if (len & 1)
    {
      (*error_code) = ODD_LENGTH_LIST;
      return(NULL);
    }
  (*error_code) = NO_PROBLEM_IN_LIST;

  maxpartial = (int)(Xen_integer_to_C_int(Xen_vector_ref(v, 0)));
  if (maxpartial < 0)
    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
  else
    {
      for (i = 2; i < len; i += 2)
	{
	  curpartial = (int)(Xen_integer_to_C_int(Xen_vector_ref(v, i)));
	  if (curpartial > maxpartial) 
	    maxpartial = curpartial;
	  if (curpartial < 0)
	    (*error_code) = NEGATIVE_NUMBER_IN_LIST;
	}
    }
  if ((*error_code) != NO_PROBLEM_IN_LIST)
    return(NULL);

  partials = (mus_float_t *)calloc(maxpartial + 1, sizeof(mus_float_t));
  if (!partials)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate waveshaping partials list");
  (*npartials) = maxpartial + 1;

  for (i = 0; i < len; i += 2)
    {
      curpartial = (int)(Xen_integer_to_C_int(Xen_vector_ref(v, i)));
      partials[curpartial] = Xen_real_to_C_double(Xen_vector_ref(v, i + 1));
    }
  return(partials);
}


static Xen g_partials_to_polynomial(Xen amps, Xen ukind)
{
  #if HAVE_SCHEME
    #define p2p_example "(let ((v0 (partials->polynomial '(1 1.0 2 1.0)))\n        (os (make-oscil)))\n    (polynomial v0 (oscil os)))"
  #endif
  #if HAVE_RUBY
    #define p2p_example "v0 = partials2polynomial([1, 1.0, 2, 1.0])\n  os = make_oscil()\n  polynomial(v0, oscil(os))"
  #endif
  #if HAVE_FORTH
    #define p2p_example "'( 1 1.0 2 1.0 ) partials->polynomial value v0\n  make-oscil value os\n  v0 os 0.0 0.0 oscil polynomial"
  #endif

  #define H_partials_to_polynomial "(" S_partials_to_polynomial " partials (kind " S_mus_chebyshev_first_kind ")): \
produce a Chebyshev polynomial suitable for use with the " S_polynomial " generator \
to create (via waveshaping) the harmonic spectrum described by the partials argument:\n  " p2p_example

  int npartials = 0;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;
  mus_float_t *partials = NULL, *wave;
  int error = NO_PROBLEM_IN_LIST;

  Xen_check_type(mus_is_vct(amps) || Xen_is_list(amps), amps, 1, S_partials_to_polynomial, "a list or a " S_vct);
  Xen_check_type(Xen_is_integer_or_unbound(ukind), ukind, 2, S_partials_to_polynomial, "either " S_mus_chebyshev_first_kind " or " S_mus_chebyshev_second_kind);

  if (Xen_is_integer(ukind))
    {
      int ck;
      ck = Xen_integer_to_C_int(ukind);
      if ((ck >= MUS_CHEBYSHEV_EITHER_KIND) && (ck <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)ck;
      else Xen_out_of_range_error(S_partials_to_polynomial, 2, ukind, "unknown Chebyshev polynomial kind");
    }
  
  if (mus_is_vct(amps))
    partials = mus_vct_to_partials(Xen_to_vct(amps), &npartials, &error);
  else partials = list_to_partials(amps, &npartials, &error);

  if (!partials)
    Xen_error(NO_DATA, 
	      Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
			 C_string_to_Xen_string(S_partials_to_polynomial), 
			 amps));

  wave = mus_partials_to_polynomial(npartials, partials, kind); /* wave == partials; in both vct and list cases, partials is newly allocated */
  return(xen_make_vct(npartials, wave));
}


static Xen g_normalize_partials(Xen partials)
{
  #define H_normalize_partials "(" S_normalize_partials " partials) scales the \
partial amplitudes in the " S_vct " or list 'partials' by the inverse of their sum (so that they add to 1.0)."

  vct *v;
  Xen xv = Xen_false;

  Xen_check_type(((Xen_is_list(partials)) && (!Xen_is_null(partials))) || (mus_is_vct(partials)), partials, 1, S_normalize_partials, "a " S_vct " or (non-empty) list");

  if (mus_is_vct(partials))
    xv = partials;
  else xv = xen_list_to_vct(partials);
  v = Xen_to_vct(xv);

  if ((mus_vct_length(v) > 1) &&
      ((mus_vct_length(v) & 1) == 0))
    mus_normalize_partials(mus_vct_length(v) / 2, mus_vct_data(v));
  else Xen_error(BAD_TYPE,
		 Xen_list_3(C_string_to_Xen_string("~A: partials, ~A, must be a non-empty list or " S_vct " of even length (partial-number partial-amp ...)"),
			    C_string_to_Xen_string(S_normalize_partials),
			    partials));
  return(xv);
}


static mus_float_t *vector_to_float_array(Xen v)
{
  mus_float_t *data;
  mus_long_t i, len;
  len = Xen_vector_length(v);
  data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
  for (i = 0; i < len; i++)
    data[i] = Xen_real_to_C_double(Xen_vector_ref(v, i));
  return(data);
}

static Xen g_chebyshev_tu_sum(Xen x, Xen tn, Xen un)
{
  #define H_chebyshev_tu_sum "(" S_mus_chebyshev_tu_sum " x tn un) returns the sum of the weighted \
Chebyshev polynomials Tn and Un (vectors or " S_vct "s), with phase x."

  bool need_free = false;
  int len = 0;
  mus_float_t *tdata = NULL, *udata = NULL;
  Xen result;
  
  Xen_check_type(Xen_is_double(x), x, 1, S_mus_chebyshev_tu_sum, "a float");

  if ((mus_is_vct(tn)) &&
      (mus_is_vct(un)))
    {
      vct *Tn, *Un;
      Tn = Xen_to_vct(tn);
      tdata = mus_vct_data(Tn);
      Un = Xen_to_vct(un);
      udata = mus_vct_data(Un);
      len = mus_vct_length(Tn);
      if (len == 0) return(C_double_to_Xen_real(0.0));
      if (len != mus_vct_length(Un)) return(C_double_to_Xen_real(0.0));
    }
  else
    {
      if ((Xen_is_vector(tn)) && 
	  (Xen_is_vector(un)))
	{
	  len = Xen_vector_length(tn);
	  if (len == 0) return(C_double_to_Xen_real(0.0));
	  if (len != Xen_vector_length(un)) return(C_double_to_Xen_real(0.0));
	  tdata = vector_to_float_array(tn);
	  udata = vector_to_float_array(un);
	  need_free = true;
	}
      else
	{
	  Xen_check_type(false, tn, 1, S_mus_chebyshev_tu_sum, "both arrays should be either " S_vct "s or vectors");
	}
    }

  result = C_double_to_Xen_real(mus_chebyshev_tu_sum(Xen_real_to_C_double(x), len, tdata, udata));
  if (need_free)
    {
      free(tdata);
      free(udata);
    }

  return(result);
}


static Xen g_chebyshev_t_sum(Xen x, Xen tn)
{
  #define H_chebyshev_t_sum "(" S_mus_chebyshev_t_sum " x tn) returns the sum of the weighted \
Chebyshev polynomials Tn (a " S_vct ")."

  bool need_free = false;
  int len = 0;
  mus_float_t *data = NULL;
  Xen result;
  
  Xen_check_type(Xen_is_double(x), x, 1, S_mus_chebyshev_t_sum, "a float");
  if (mus_is_vct(tn))
    {
      vct *Tn;
      Tn = Xen_to_vct(tn);
      data = mus_vct_data(Tn);
      len = mus_vct_length(Tn);
      if (len == 0) return(C_double_to_Xen_real(0.0));
    }
  else
    {
      if (Xen_is_vector(tn))
	{
	  len = Xen_vector_length(tn);
	  if (len == 0) return(C_double_to_Xen_real(0.0));
	  data = vector_to_float_array(tn);
	  need_free = true;
	}
      else Xen_check_type(false, tn, 1, S_mus_chebyshev_t_sum, "a " S_vct " or a vector");
    }
  result = C_double_to_Xen_real(mus_chebyshev_t_sum(Xen_real_to_C_double(x), len, data));
  if (need_free)
    free(data);
  return(result);
}


static Xen g_chebyshev_u_sum(Xen x, Xen un)
{
  #define H_chebyshev_u_sum "(" S_mus_chebyshev_u_sum " x un) returns the sum of the weighted \
Chebyshev polynomials Un (a " S_vct ")."

  bool need_free = false;
  int len = 0;
  mus_float_t *data = NULL;
  Xen result;

  Xen_check_type(Xen_is_double(x), x, 1, S_mus_chebyshev_u_sum, "a float");

  if (mus_is_vct(un))
    {
      vct *Un;
      Un = Xen_to_vct(un);
      len = mus_vct_length(Un);
      if (len == 0) return(C_double_to_Xen_real(0.0));
      data = mus_vct_data(Un);
    }
  else
    {
      if (Xen_is_vector(un))
	{
	  len = Xen_vector_length(un);
	  if (len == 0) return(C_double_to_Xen_real(0.0));
	  data = vector_to_float_array(un);
	  need_free = true;
	}
      else Xen_check_type(false, un, 1, S_mus_chebyshev_u_sum, "a " S_vct " or a vector");
    }
  result = C_double_to_Xen_real(mus_chebyshev_u_sum(Xen_real_to_C_double(x), len, data));
  if (need_free)
    free(data);
  return(result);
}




/* ---------------- polyshape ---------------- */

static Xen g_polyshape(Xen obj, Xen index, Xen fm)
{
  #define H_polyshape "(" S_polyshape " gen (index 1.0) (fm 0.0)): next sample of polynomial-based waveshaper"
  mus_float_t fm1 = 0.0, index1 = 1.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_polyshape, S_polyshape, "a polyshape generator");
  Xen_real_to_C_double_if_bound(index, index1, S_polyshape, 2);
  Xen_real_to_C_double_if_bound(fm, fm1, S_polyshape, 3);

  return(C_double_to_Xen_real(mus_polyshape(g, index1, fm1)));
}


static Xen g_is_polyshape(Xen obj) 
{
  #define H_is_polyshape "(" S_is_polyshape " gen): " PROC_TRUE " if gen is a " S_polyshape
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_polyshape(Xen_to_mus_any(obj)))));
}

#define H_make_polyshape "(" S_make_polyshape " (frequency 0.0) (initial-phase 0.0) coeffs partials kind): \
return a new polynomial-based waveshaping generator:\n\
   (" S_make_polyshape " :coeffs (" S_partials_to_polynomial " '(1 1.0)))\n\
is the same in effect as " S_make_oscil


#if HAVE_SCHEME
static s7_pointer g_make_polyshape(s7_scheme *sc, s7_pointer args)
{
  mus_any *ge;
  int csize, npartials;
  s7_pointer v, p, fp;
  mus_float_t freq, phase;
  mus_float_t *coeffs = NULL;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_polyshape, 1, s7_car(args), "freq > srate/2?");

  p = s7_cddr(args);
  fp = s7_caddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(s7, S_make_polyshape, 5, fp, "an integer"));
      kind = (mus_polynomial_t)s7_integer(fp);
      if ((kind < MUS_CHEBYSHEV_EITHER_KIND) || (kind > MUS_CHEBYSHEV_SECOND_KIND))
	Xen_out_of_range_error(S_make_polyshape, 5, fp, "unknown Chebyshev polynomial kind");
    }

  v = s7_car(p);
  if (v != Xen_false)
    {
      if (!s7_is_float_vector(v))
	return(s7_wrong_type_arg_error(s7, S_make_polyshape, 3, fp, "a float-vector"));
      coeffs = s7_float_vector_elements(v);
      csize = s7_vector_length(v);
    }
  else
    {
      int error = NO_PROBLEM_IN_LIST;
      fp = s7_cadr(p);
      if (fp != Xen_false)
	{
	  if (s7_is_float_vector(fp))
	    coeffs = mus_vct_to_partials(Xen_to_vct(fp), &npartials, &error);
	  else
	    {
	      if (s7_is_pair(fp))
		coeffs = list_to_partials(fp, &npartials, &error);
	      else return(s7_wrong_type_arg_error(s7, S_make_polyshape, 4, fp, "a float-vector or a pair"));
	    }
	  if (!coeffs)
	    Xen_error(NO_DATA, 
		      Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
				 C_string_to_Xen_string(S_make_polyshape), fp));
	  coeffs = mus_partials_to_polynomial(npartials, coeffs, kind);
	  csize = npartials;
	  /* coeffs = partials here, so don't delete */ 
	}
    }

  if (!coeffs)
    {
      /* clm.html says '(1 1) is the default */
      v = s7_make_float_vector(sc, 2, 1, NULL);
      coeffs = s7_float_vector_elements(v);
      coeffs[0] = 0.0;
      coeffs[1] = 1.0;
      csize = 2;
    }
  if (!s7_is_float_vector(v)) v = xen_make_vct(csize, coeffs);
  phase = s7_number_to_real(sc, s7_cadr(args));

  ge = mus_make_polyshape(freq, phase, coeffs, csize, kind);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
  return(Xen_false);
}
#else
static Xen g_make_polyshape(Xen arglist)
{
  mus_any *ge;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, csize = 0, npartials = 0;
  Xen orig_v = Xen_false;
  mus_float_t freq = 0.0, phase = 0.0; 
  /* 
   * if we followed the definition directly, the initial phase default would be M_PI_2 (pi/2) so that
   *   we drive the Tn's with a cosine.  But I've always used sine instead, so I think I'll leave
   *   it that way.  There is no difference in the output waveform except an overall phase
   *   offset.  So, with sine, the phases rotate through cos sin -cos -sin... rather than being all cos,
   *   but these add to exactly the same actual wave -- what you'd expect since Tn doesn't know
   *   where we started.  This also does not affect "signification".
   */
  mus_float_t *coeffs = NULL;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;

  keys[0] = kw_frequency;
  keys[1] = kw_initial_phase;
  keys[2] = kw_coeffs;
  keys[3] = kw_partials;
  keys[4] = kw_kind;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_polyshape, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_polyshape, arglist_len, 5, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      vct *v = NULL;
      int ck;

      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_polyshape, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_polyshape, orig_arg[0], keys[0], "freq > srate/2?");

      phase = Xen_optkey_to_float(kw_initial_phase, keys[1], S_make_polyshape, orig_arg[2], phase);

      ck = Xen_optkey_to_int(kw_kind, keys[4], S_make_polyshape, orig_arg[4], (int)kind);
      if ((ck >= MUS_CHEBYSHEV_EITHER_KIND) && (ck <= MUS_CHEBYSHEV_SECOND_KIND))
	kind = (mus_polynomial_t)ck;
      else Xen_out_of_range_error(S_make_polyshape, orig_arg[4], keys[4], "unknown Chebyshev polynomial kind");

      v = mus_optkey_to_vct(keys[2], S_make_polyshape, orig_arg[2], NULL);
      if (v)
        {
	  orig_v = keys[2];
	  coeffs = mus_vct_data(v);
	  csize = mus_vct_length(v);
	}
      else
	{
	  if (!(Xen_is_keyword(keys[3])))
	    {
	      mus_float_t *partials = NULL;
	      int error = NO_PROBLEM_IN_LIST;
	      if (mus_is_vct(keys[3]))
		partials = mus_vct_to_partials(Xen_to_vct(keys[3]), &npartials, &error);
	      else
		{
		  Xen_check_type(Xen_is_list(keys[3]), keys[3], orig_arg[3], S_make_polyshape, "a list or a " S_vct);
		  partials = list_to_partials(keys[3], &npartials, &error);
		}
	      if (!partials)
		Xen_error(NO_DATA, 
			  Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
				     C_string_to_Xen_string(S_make_polyshape), 
				     keys[3]));
	      coeffs = mus_partials_to_polynomial(npartials, partials, kind);
	      csize = npartials;
	      /* coeffs = partials here, so don't delete */ 
	    }
	}
    }

  if (!coeffs)
    {
      /* clm.html says '(1 1) is the default */
      mus_float_t *data;
      data = (mus_float_t *)malloc(2 * sizeof(mus_float_t));
      data[0] = 0.0;
      data[1] = 1.0;
      coeffs = mus_partials_to_polynomial(2, data, kind);
      csize = 2;
    }

  if (Xen_is_false(orig_v))
    orig_v = xen_make_vct(csize, coeffs);

  ge = mus_make_polyshape(freq, phase, coeffs, csize, kind);
  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_v)));
  return(Xen_false);
}
#endif


/* ---------------- polywave ---------------- */

static Xen g_polywave(Xen obj, Xen fm)
{
  #define H_polywave "(" S_polywave " gen (fm 0.0)): next sample of polywave waveshaper"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_polywave, S_polywave, "a polywave generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_polywave, 3);

  return(C_double_to_Xen_real(mus_polywave(g, fm1)));
}


static Xen g_is_polywave(Xen obj) 
{
  #define H_is_polywave "(" S_is_polywave " gen): " PROC_TRUE " if gen is a " S_polywave " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_polywave(Xen_to_mus_any(obj)))));
}

#define H_make_polywave "(" S_make_polywave " (frequency 0.0) (partials '(1 1)) (type " S_mus_chebyshev_first_kind ") xcoeffs ycoeffs): \
return a new polynomial-based waveshaping generator.  (" S_make_polywave " :partials (float-vector 1 1.0)) is the same in effect as " S_make_oscil "."

#if HAVE_SCHEME
static s7_pointer g_make_polywave(s7_scheme *sc, s7_pointer args)
{
  mus_any *ge;
  int n = 0;
  mus_float_t freq;
  mus_float_t *xcoeffs = NULL, *ycoeffs = NULL;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND; /* 1 clm.h */
  s7_pointer vx = NULL, vy = NULL, p, px, py, pr, pf, pt;
  int error = NO_PROBLEM_IN_LIST;

  pr = s7_cadr(args);
  if (pr != Xen_false)
    {
      if (s7_is_float_vector(pr))
	xcoeffs = mus_vct_to_partials(pr, &n, &error);
      else
	{
	  if (s7_is_pair(pr))
	    xcoeffs = list_to_partials(pr, &n, &error);
	  else
	    {
	      if (s7_is_int_vector(pr))
		return(s7_wrong_type_arg_error(sc, S_make_polywave, 2, pr, "a float-vector or a list"));
	      else
		{
		  if (s7_is_vector(pr))
		    xcoeffs = mus_vector_to_partials(pr, &n, &error);
		  else return(s7_wrong_type_arg_error(sc, S_make_polywave, 2, pr, "a float-vector or a list"));
		}
	    }
	}
      if (!xcoeffs)
	Xen_error(NO_DATA, Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), C_string_to_Xen_string(S_make_polywave), pr));
      vx = xen_make_vct(n, xcoeffs);
    }
  else n = 0;

  p = s7_cdddr(args);
  px = s7_car(p);
  if (px != Xen_false)
    {
      if (!s7_is_float_vector(px))
	return(s7_wrong_type_arg_error(sc, S_make_polywave, 4, px, "a float-vector"));
      vx = px;
      xcoeffs = s7_float_vector_elements(px);
      n = s7_vector_length(px);
    }

  py = s7_cadr(p);
  if (py != Xen_false)
    {
      s7_int i;
      if (!s7_is_float_vector(py))
	return(s7_wrong_type_arg_error(sc, S_make_polywave, 5, py, "a float-vector"));
      vy = py;
      ycoeffs = s7_float_vector_elements(py);
      i = s7_vector_length(py);
      if ((n == 0) || (i < n))
	n = i;
    }

  pf = s7_car(args);
  freq = s7_number_to_real(sc, pf);
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_polywave, 1, pf, "freq > srate/2?");

  pt = s7_caddr(args);
  if (pt != Xen_false)
    {
      s7_int type;
      if (!s7_is_integer(pt))
	return(s7_wrong_type_arg_error(sc, S_make_polywave, 3, pt, "an integer"));
      type = s7_integer(pt);
      if ((type >= MUS_CHEBYSHEV_EITHER_KIND) && 
	  (type <= MUS_CHEBYSHEV_BOTH_KINDS))
	kind = (mus_polynomial_t)type;
      else Xen_out_of_range_error(S_make_polywave, 3, pt, "unknown Chebyshev polynomial kind");
    }

  if (!xcoeffs)
    {
      /* clm.html says '(1 1) is the default but table-lookup is 0? */
      vx = s7_make_float_vector(sc, 2, 1, NULL);
      xcoeffs = s7_float_vector_elements(vx);
      xcoeffs[0] = 0.0;
      xcoeffs[1] = 1.0;
      n = 2; 
    }

  if (ycoeffs)
    {
      ge = mus_make_polywave_tu(freq, xcoeffs, ycoeffs, n);
      if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, vx, vy)));
      return(Xen_false);
    }
  ge = mus_make_polywave(freq, xcoeffs, n, kind);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, vx)));
  return(Xen_false);
}
#else
static Xen g_make_polywave(Xen arglist)
{
  mus_any *ge;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, n = 0, npartials = 0;
  Xen orig_x = Xen_false, orig_y = Xen_false;
  mus_float_t freq = 0.0; 
  mus_float_t *xcoeffs = NULL, *ycoeffs = NULL, *partials = NULL;
  mus_polynomial_t kind = MUS_CHEBYSHEV_FIRST_KIND;
  int error = NO_PROBLEM_IN_LIST;

  keys[0] = kw_frequency;
  keys[1] = kw_partials;
  keys[2] = kw_type;
  keys[3] = kw_x_coeffs;
  keys[4] = kw_y_coeffs;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_polywave, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_polywave, arglist_len, 5, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      vct *v;
      int type;
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_polywave, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_polywave, orig_arg[0], keys[0], "freq > srate/2?");

      type = Xen_optkey_to_int(kw_type, keys[2], S_make_polywave, orig_arg[2], (int)kind);
      if ((type >= MUS_CHEBYSHEV_EITHER_KIND) && 
	  (type <= MUS_CHEBYSHEV_BOTH_KINDS))
	kind = (mus_polynomial_t)type;
      else Xen_out_of_range_error(S_make_polywave, orig_arg[2], keys[2], "unknown Chebyshev polynomial kind");

      if (!(Xen_is_keyword(keys[1]))) /* partials were supplied */
	{
	  if (mus_is_vct(keys[1]))
	    partials = mus_vct_to_partials(Xen_to_vct(keys[1]), &npartials, &error);
	  else
	    {
	      if (Xen_is_vector(keys[1]))
		partials = mus_vector_to_partials(keys[1], &npartials, &error);
	      else
		{
		  Xen_check_type(Xen_is_list(keys[1]), keys[1], orig_arg[1], S_make_polywave, "a list or a " S_vct);
		  partials = list_to_partials(keys[1], &npartials, &error);
		}
	    }
	  if (!partials) /* here if null, something went wrong in the translation functions */
	    Xen_error(NO_DATA, 
		      Xen_list_3(C_string_to_Xen_string(list_to_partials_error_to_string(error)), 
				 C_string_to_Xen_string(S_make_polywave), 
				 keys[1]));

	  xcoeffs = partials;
	  n = npartials;
	  orig_x = xen_make_vct(n, xcoeffs);
	  /* xcoeffs = partials here, so don't delete */ 
	}

      if (!(Xen_is_keyword(keys[3])))
        {
	  Xen_check_type(mus_is_vct(keys[3]), keys[3], orig_arg[3], S_make_polywave, "a " S_vct);
	  orig_x = keys[3];
	  v = Xen_to_vct(orig_x);
	  n = mus_vct_length(v);
	  xcoeffs = mus_vct_data(v);
        }
      
      if (!(Xen_is_keyword(keys[4])))
	{
	  /* make-polyoid in generators.scm */
	  int yn;
	  Xen_check_type(mus_is_vct(keys[4]), keys[4], orig_arg[4], S_make_polywave, "a " S_vct);
	  orig_y = keys[4];
	  v = Xen_to_vct(orig_y);
	  yn = mus_vct_length(v);
	  if ((n == 0) || (yn < n))
	    n = yn;
	  ycoeffs = mus_vct_data(v);
	}
    }

  if (!xcoeffs)
    {
      /* clm.html says '(1 1) is the default but table-lookup is 0? */
      mus_float_t *data;
      data = (mus_float_t *)malloc(2 * sizeof(mus_float_t));
      data[0] = 0.0;
      data[1] = 1.0;
      xcoeffs = data;
      n = 2; 
      orig_x = xen_make_vct(n, xcoeffs);
    }

  if (ycoeffs)
    {
      ge = mus_make_polywave_tu(freq, xcoeffs, ycoeffs, n);
      if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(ge, orig_x, orig_y)));
    }
  ge = mus_make_polywave(freq, xcoeffs, n, kind);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, orig_x)));
  return(Xen_false);
}
#endif


/* ---------------- nrxysin and nrxycos ---------------- */

static Xen g_is_nrxysin(Xen obj) 
{
  #define H_is_nrxysin "(" S_is_nrxysin " gen): " PROC_TRUE " if gen is an " S_nrxysin " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_nrxysin(Xen_to_mus_any(obj)))));
}


static Xen g_is_nrxycos(Xen obj) 
{
  #define H_is_nrxycos "(" S_is_nrxycos " gen): " PROC_TRUE " if gen is an " S_nrxycos " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_nrxycos(Xen_to_mus_any(obj)))));
}


static Xen g_nrxysin(Xen obj, Xen fm)
{
  #define H_nrxysin "(" S_nrxysin " gen (fm 0.0)): next sample of nrxysin generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_nrxysin, S_nrxysin, "an nrxysin generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_nrxysin, 2);

  return(C_double_to_Xen_real(mus_nrxysin(g, fm1)));
}

static Xen g_nrxycos(Xen obj, Xen fm)
{
  #define H_nrxycos "(" S_nrxycos " gen (fm 0.0)): next sample of nrxycos generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_nrxycos, S_nrxycos, "an nrxycos generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_nrxycos, 2);

  return(C_double_to_Xen_real(mus_nrxycos(g, fm1)));
}

#if HAVE_SCHEME
static s7_pointer g_make_nrxy(s7_scheme *sc, s7_pointer args, bool sin_case, const char *caller)
{
  mus_any *ge;
  mus_float_t freq, r, ratio;
  int n;
  s7_pointer un;

  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(caller, 1, s7_car(args), "freq > srate/2?");

  args = s7_cdr(args);
  ratio = s7_number_to_real(sc, s7_car(args));

  args = s7_cdr(args);
  un = s7_car(args);
  if (!s7_is_integer(un))
    return(s7_wrong_type_arg_error(s7, caller, 3, un, "an integer"));
  n = s7_integer(un);
  if (n < 0)
    Xen_out_of_range_error(caller, 3, un, "n (sidebands) < 0?");

  r = s7_number_to_real(sc, s7_cadr(args));
  if ((r >= 1.0) ||
    (r <= -1.0))
    Xen_out_of_range_error(caller, 4, s7_cadr(args), "r (sideband amp ratio) not within -1.0 to 1.0?");

  if (sin_case)
    ge = mus_make_nrxysin(freq, ratio, n, r);
  else ge = mus_make_nrxycos(freq, ratio, n, r);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

static s7_pointer g_make_nrxysin(s7_scheme *sc, s7_pointer args)
{
  #define H_make_nrxysin "(" S_make_nrxysin " (frequency 0.0) (ratio 1.0) (n 1) (r 0.5)): return a new nrxysin generator."
  return(g_make_nrxy(sc, args, true, S_make_nrxysin));
}

static s7_pointer g_make_nrxycos(s7_scheme *sc, s7_pointer args)
{
  #define H_make_nrxycos "(" S_make_nrxycos " (frequency 0.0) (ratio 1.0) (n 1) (r 0.5)): return a new nrxycos generator."
  return(g_make_nrxy(sc, args, false, S_make_nrxycos));
}
#else
static Xen g_make_nrxy(bool sin_case, const char *caller, Xen arglist)
{
  mus_any *ge;
  Xen args[8];
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;
  mus_float_t freq = 0.0, r = 0.5, ratio = 1.0;
  int n = 1;

  keys[0] = kw_frequency;
  keys[1] = kw_ratio;
  keys[2] = kw_n;
  keys[3] = kw_r;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 8) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(caller, arglist_len, 4, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      ratio = Xen_optkey_to_float(kw_ratio, keys[1], caller, orig_arg[1], ratio);

      n = Xen_optkey_to_int(kw_n, keys[2], caller, orig_arg[2], n);
      if (n < 0)
	Xen_out_of_range_error(caller, orig_arg[2], keys[2], "n (sidebands) < 0?");

      r = Xen_optkey_to_float(kw_r, keys[3], caller, orig_arg[3], r);
      if ((r >= 1.0) ||
	  (r <= -1.0))
	Xen_out_of_range_error(caller, orig_arg[3], keys[3], "r (sideband amp ratio) not within -1.0 to 1.0?");
      /* if not with doubles, this actually maxes out around .99999999 because mus_optkey_to_float (apparently) rounds up */
    }
  if (sin_case)
    ge = mus_make_nrxysin(freq, ratio, n, r);
  else ge = mus_make_nrxycos(freq, ratio, n, r);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_make_nrxysin(Xen arglist)
{
  #define H_make_nrxysin "(" S_make_nrxysin " (frequency 0.0) (ratio 1.0) (n 1) (r 0.5)): return a new nrxysin generator."

  return(g_make_nrxy(true, S_make_nrxysin, arglist));
}

static Xen g_make_nrxycos(Xen arglist)
{
  #define H_make_nrxycos "(" S_make_nrxycos " (frequency 0.0) (ratio 1.0) (n 1) (r 0.5)): return a new nrxycos generator."

  return(g_make_nrxy(false, S_make_nrxycos, arglist));
}
#endif


/* ---------------- rxyksin and rxykcos ---------------- */

static Xen g_is_rxyksin(Xen obj) 
{
  #define H_is_rxyksin "(" S_is_rxyksin " gen): " PROC_TRUE " if gen is an " S_rxyksin " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_rxyksin(Xen_to_mus_any(obj)))));
}


static Xen g_is_rxykcos(Xen obj) 
{
  #define H_is_rxykcos "(" S_is_rxykcos " gen): " PROC_TRUE " if gen is an " S_rxykcos " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && 
			  (mus_is_rxykcos(Xen_to_mus_any(obj)))));
}


static Xen g_rxyksin(Xen obj, Xen fm)
{
  #define H_rxyksin "(" S_rxyksin " gen (fm 0.0)): next sample of rxyksin generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rxyksin, S_rxyksin, "an rxyksin generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rxyksin, 2);

  return(C_double_to_Xen_real(mus_rxyksin(g, fm1)));
}

static Xen g_rxykcos(Xen obj, Xen fm)
{
  #define H_rxykcos "(" S_rxykcos " gen (fm 0.0)): next sample of rxykcos generator"
  mus_float_t fm1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_rxykcos, S_rxykcos, "an rxykcos generator");
  Xen_real_to_C_double_if_bound(fm, fm1, S_rxykcos, 2);

  return(C_double_to_Xen_real(mus_rxykcos(g, fm1)));
}

#if HAVE_SCHEME
static s7_pointer g_make_rxyk(s7_scheme *sc, s7_pointer args, bool sin_case, const char *caller)
{
  mus_any *ge;
  mus_float_t freq, r, ratio;
  
  freq = s7_number_to_real(sc, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(caller, 1, s7_car(args), "freq > srate/2?");
  
  ratio = s7_number_to_real(sc, s7_cadr(args));
  r = s7_number_to_real(sc, s7_caddr(args));

  if (sin_case)
    ge = mus_make_rxyksin(freq, 0.0, r, ratio);
  else ge = mus_make_rxykcos(freq, 0.0, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}

static s7_pointer g_make_rxyksin(s7_scheme *sc, s7_pointer args)
{
  #define H_make_rxyksin "(" S_make_rxyksin " (frequency 0.0) (ratio 1.0) (r 0.5)): return a new rxyksin generator."

  return(g_make_rxyk(sc, args, true, S_make_rxyksin));
}

static s7_pointer g_make_rxykcos(s7_scheme *sc, s7_pointer args)
{
  #define H_make_rxykcos "(" S_make_rxykcos " (frequency 0.0) (ratio 1.0) (r 0.5)): return a new rxykcos generator."

  return(g_make_rxyk(sc, args, false, S_make_rxykcos));
}
#else
static Xen g_make_rxyk(bool sin_case, const char *caller, Xen arglist)
{
  mus_any *ge;
  Xen args[6];
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  mus_float_t freq = 0.0, r = 0.5, ratio = 1.0; /* original in generators.scm assumes initial-phase = 0.0 */

  keys[0] = kw_frequency;
  keys[1] = kw_ratio;
  keys[2] = kw_r;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 6) clm_error(caller, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(caller, arglist_len, 3, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], caller, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(caller, orig_arg[0], keys[0], "freq > srate/2?");

      ratio = Xen_optkey_to_float(kw_ratio, keys[1], caller, orig_arg[1], ratio);
      r = Xen_optkey_to_float(kw_r, keys[2], caller, orig_arg[2], r);
    }
  if (sin_case)
    ge = mus_make_rxyksin(freq, 0.0, r, ratio);
  else ge = mus_make_rxykcos(freq, 0.0, r, ratio);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_make_rxyksin(Xen arglist)
{
  #define H_make_rxyksin "(" S_make_rxyksin " (frequency 0.0) (ratio 1.0) (r 0.5)): \
return a new rxyksin generator."

  return(g_make_rxyk(true, S_make_rxyksin, arglist));
}

static Xen g_make_rxykcos(Xen arglist)
{
  #define H_make_rxykcos "(" S_make_rxykcos " (frequency 0.0) (ratio 1.0) (r 0.5)): \
return a new rxykcos generator."

  return(g_make_rxyk(false, S_make_rxykcos, arglist));
}
#endif


/* ----------------  filter ---------------- */

typedef enum {G_FILTER, G_FIR_FILTER, G_IIR_FILTER} xclm_fir_t;

static Xen g_make_fir_coeffs(Xen order, Xen envl)
{
  #define H_make_fir_coeffs "(" S_make_fir_coeffs " order v): turn spectral envelope in " S_vct " v into coeffs for FIR filter"
  int size;
  mus_float_t *a;
  vct *v;

  Xen_check_type(Xen_is_integer(order), order, 1, S_make_fir_coeffs, "int");
  Xen_check_type(mus_is_vct(envl), envl, 2, S_make_fir_coeffs, "a " S_vct);

  v = Xen_to_vct(envl);

  size = Xen_integer_to_C_int(order);
  if (size != mus_vct_length(v))
    Xen_error(CLM_ERROR,
	      Xen_list_3(C_string_to_Xen_string(S_make_fir_coeffs ": order ~A != " S_vct " length ~A"),
			 order, 
			 envl));

  a = mus_make_fir_coeffs(Xen_integer_to_C_int(order), mus_vct_data(v), NULL);
  return(xen_make_vct(mus_vct_length(v), a));
}


static Xen g_is_filter(Xen obj) 
{
  #define H_is_filter "(" S_is_filter " gen): " PROC_TRUE " if gen is a " S_filter
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_filter(Xen_to_mus_any(obj)))));
}


static Xen g_is_fir_filter(Xen obj) 
{
  #define H_is_fir_filter "(" S_is_fir_filter " gen): " PROC_TRUE " if gen is an " S_fir_filter
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_fir_filter(Xen_to_mus_any(obj)))));
}


static Xen g_is_iir_filter(Xen obj) 
{
  #define H_is_iir_filter "(" S_is_iir_filter " gen): " PROC_TRUE " if gen is an " S_iir_filter
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_iir_filter(Xen_to_mus_any(obj)))));
}


static Xen g_filter(Xen obj, Xen input)
{
  #define H_filter "(" S_filter " gen (input 0.0)): next sample from filter"
  mus_any *g = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(obj, gn, g, mus_is_filter, S_filter, "a filter");
  Xen_real_to_C_double_if_bound(input, x, S_filter, 2);

  return(C_double_to_Xen_real(mus_filter(g, x)));
}


static Xen g_fir_filter(Xen obj, Xen input)
{
  #define H_fir_filter "(" S_fir_filter " gen (input 0.0)): next sample from FIR filter"
  mus_any *g = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(obj, gn, g, mus_is_fir_filter, S_fir_filter, "an FIR filter");
  Xen_real_to_C_double_if_bound(input, x, S_fir_filter, 2);

  return(C_double_to_Xen_real(mus_fir_filter(g, x)));
}


static Xen g_iir_filter(Xen obj, Xen input)
{
  #define H_iir_filter "(" S_iir_filter " gen (input 0.0)): next sample from IIR filter"
  mus_any *g = NULL;
  mus_xen *gn;
  mus_float_t x = 0.0;

  Xen_to_C_generator(obj, gn, g, mus_is_iir_filter, S_iir_filter, "an IIR filter");
  Xen_real_to_C_double_if_bound(input, x, S_iir_filter, 2);

  return(C_double_to_Xen_real(mus_iir_filter(g, x)));
}

#define H_make_filter "(" S_make_filter " order xcoeffs ycoeffs): return a new direct form FIR/IIR filter, coeff args are " S_vct "s"
#define H_make_fir_filter "(" S_make_fir_filter " order xcoeffs): return a new FIR filter, xcoeffs a " S_vct
#define H_make_iir_filter "(" S_make_iir_filter " order ycoeffs): return a new IIR filter, ycoeffs a " S_vct

#if HAVE_SCHEME
static s7_pointer g_make_filter_1(s7_scheme *sc, s7_pointer args, xclm_fir_t choice, const char *caller)
{
  s7_pointer x = NULL, y = NULL, fp;
  mus_any *fgen = NULL;
  int order = 0;
  
  fp = s7_car(args);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, caller, 1, fp, "an integer"));
      order = s7_integer(fp);
      if (order <= 0)
	Xen_out_of_range_error(caller, 1, fp, "order <= 0?");
    }
  
  fp = s7_cadr(args);
  if (fp != Xen_false)
    {
      int len;
      if (!s7_is_float_vector(fp))
	return(s7_wrong_type_arg_error(sc, caller, 2, fp, "a float-vector"));
      if (choice == G_IIR_FILTER)
	y = fp;
      else x = fp;
      len = s7_vector_length(fp);
      if ((order != 0) && (len != order))
	Xen_error(CLM_ERROR, Xen_list_4(C_string_to_Xen_string("~A: coeffs, ~A, must match order, ~A"),
					C_string_to_Xen_string(caller), fp, s7_car(args)));
      order = len;
    }
  
  if (choice == G_FILTER)
    {
      fp = s7_caddr(args);
      if (fp != Xen_false)
	{
	  int len;
	  y = fp;
	  if (!s7_is_float_vector(y))
	    return(s7_wrong_type_arg_error(sc, caller, 3, y, "a float-vector"));
	  len = s7_vector_length(y);
	  if ((order != 0) && (len != order))
	    {
	      if (x)
		Xen_error(CLM_ERROR,
			  Xen_list_4(C_string_to_Xen_string("~A: coeffs must be same length.  x len: ~A, y len: ~A"),
				     C_string_to_Xen_string(caller),
				     C_int_to_Xen_integer(mus_vct_length(x)),
				     C_int_to_Xen_integer(len)));
	      else 
		Xen_error(CLM_ERROR, Xen_list_4(C_string_to_Xen_string("~A: coeffs, ~A, must match order, ~A"),
						C_string_to_Xen_string(caller), y, s7_car(args)));
	    }
	  order = len;
	}
      if (!y) choice = G_FIR_FILTER; else {if (!x) choice = G_IIR_FILTER;}
    }
  if ((order == 0) || ((!x) && (!y)))
    Xen_error(NO_DATA, Xen_list_2(C_string_to_Xen_string("~A: no coeffs?"), C_string_to_Xen_string(caller)));

  switch (choice)
    {
    case G_FILTER: fgen = mus_make_filter(order, s7_float_vector_elements(x), s7_float_vector_elements(y), NULL); break;
    case G_FIR_FILTER: fgen = mus_make_fir_filter(order, s7_float_vector_elements(x), NULL); break;
    case G_IIR_FILTER: fgen = mus_make_iir_filter(order, s7_float_vector_elements(y), NULL); break;
    }
  if (fgen)
    {
      mus_xen *gn = NULL;
      gn = mx_alloc(3);
      gn->gen = fgen; 
      gn->vcts[G_FILTER_STATE] = xen_make_vct_wrapper(order, mus_data(fgen));
      gn->vcts[G_FILTER_XCOEFFS] = (x) ? x : Xen_undefined;
      gn->vcts[G_FILTER_YCOEFFS] = (y) ? y : Xen_undefined;
      return(mus_xen_to_object(gn));
    }
  return(Xen_false);
}

static s7_pointer g_make_filter(s7_scheme *sc, s7_pointer args)     {return(g_make_filter_1(sc, args, G_FILTER,     S_make_filter));}
static s7_pointer g_make_fir_filter(s7_scheme *sc, s7_pointer args) {return(g_make_filter_1(sc, args, G_FIR_FILTER, S_make_fir_filter));}
static s7_pointer g_make_iir_filter(s7_scheme *sc, s7_pointer args) {return(g_make_filter_1(sc, args, G_IIR_FILTER, S_make_iir_filter));}
#else
static Xen g_make_filter_1(xclm_fir_t choice, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  Xen xwave = Xen_undefined, ywave = Xen_undefined;
  mus_any *fgen = NULL;
  Xen args[8]; 
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  vct *x = NULL, *y = NULL;
  int vals, order = 0, pr = 0;
  const char *caller;
  if (choice == G_FILTER) caller = S_make_filter; else if (choice == G_FIR_FILTER) caller = S_make_fir_filter; else caller = S_make_iir_filter;

  keys[0] = kw_order;
  keys[1] = kw_x_coeffs;
  keys[2] = kw_y_coeffs;
  keys[3] = kw_coeffs;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; 
	if (Aok(arg4)) {args[pr++] = arg4; if (Aok(arg5)) {args[pr++] = arg5; if (Aok(arg6)) {args[pr++] = arg6;}}}}}}
  vals = mus_optkey_unscramble(caller, pr, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      if (!(Xen_is_keyword(keys[0])))
	{
	  order = Xen_optkey_to_int(kw_order, keys[0], caller, orig_arg[0], 0);
	  if (order <= 0)
	    Xen_out_of_range_error(caller, orig_arg[0], keys[0], "order <= 0?");
	}

      if (!(Xen_is_keyword(keys[1])))
        {
	  Xen_check_type(mus_is_vct(keys[1]), keys[1], orig_arg[1], caller, "a " S_vct);
	  if (choice == G_IIR_FILTER)
	    {
	      ywave = keys[1];
	      y = Xen_to_vct(ywave);
	    }
	  else
	    {
	      xwave = keys[1];
	      x = Xen_to_vct(xwave);
	    }
        }

      if (!(Xen_is_keyword(keys[2])))
	{
	  Xen_check_type(mus_is_vct(keys[2]), keys[2], orig_arg[2], caller, "a " S_vct);
	  ywave = keys[2];
	  y = Xen_to_vct(ywave);
	}

      if ((choice != G_FILTER) && (!(Xen_is_keyword(keys[3]))))
        {
	  if (choice == G_IIR_FILTER)
	    clm_error(caller, "redundant arg passed to " S_make_iir_filter "?", keys[3]);
	  else clm_error(caller, "redundant arg passed to " S_make_fir_filter "?", keys[3]);
        }
    }

  if (choice == G_FILTER)
    {
      if (!y)
	choice = G_FIR_FILTER;
      else 
	{
	  if (!x)
	    choice = G_IIR_FILTER;
	}
    }
  if (((!x) && (choice != G_IIR_FILTER)) ||
      ((!y) && (choice != G_FIR_FILTER)))
    Xen_error(NO_DATA,
	      Xen_list_2(C_string_to_Xen_string("~A: no coeffs?"),
			 C_string_to_Xen_string(caller)));
  if (order == 0)
    {
      if (x)
	order = mus_vct_length(x);
      else order = mus_vct_length(y);
    }
  else
    {
      if ((x) && (order > mus_vct_length(x)))
	{
	  Xen_error(CLM_ERROR,
		    Xen_list_4(C_string_to_Xen_string("~A: xcoeffs, ~A, must match order, ~A"),
			       C_string_to_Xen_string(caller),
			       keys[1],
			       keys[0]));
	}
      else
	{
	  if ((y) && (order > mus_vct_length(y)))
	    Xen_error(CLM_ERROR,
		      Xen_list_4(C_string_to_Xen_string("~A: ycoeffs, ~A, must match order, ~A"),
				 C_string_to_Xen_string(caller),
				 keys[2],
				 keys[0])); 
	  else
	    {
	      if ((x) && (y) && (mus_vct_length(x) != mus_vct_length(y)))
		Xen_error(CLM_ERROR,
			  Xen_list_4(C_string_to_Xen_string("~A: coeffs must be same length.  x len: ~A, y len: ~A"),
				     C_string_to_Xen_string(caller),
				     C_int_to_Xen_integer(mus_vct_length(x)),
				     C_int_to_Xen_integer(mus_vct_length(y))));
	    }
	}
    }
  switch (choice)
    {
    case G_FILTER: fgen = mus_make_filter(order, mus_vct_data(x), mus_vct_data(y), NULL); break;
    case G_FIR_FILTER: fgen = mus_make_fir_filter(order, mus_vct_data(x), NULL); break;
    case G_IIR_FILTER: fgen = mus_make_iir_filter(order, mus_vct_data(y), NULL); break;
    }
  if (fgen)
    {
      mus_xen *gn = NULL;
      gn = mx_alloc(3);
      gn->gen = fgen;                                    /* delay gn allocation since make_filter can throw an error */
      gn->vcts[G_FILTER_STATE] = xen_make_vct_wrapper(order, mus_data(fgen));
      gn->vcts[G_FILTER_XCOEFFS] = xwave;
      gn->vcts[G_FILTER_YCOEFFS] = ywave;
      return(mus_xen_to_object(gn));
    }
  return(Xen_false);
}

static Xen g_make_filter(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6) {return(g_make_filter_1(G_FILTER, arg1, arg2, arg3, arg4, arg5, arg6));}
static Xen g_make_fir_filter(Xen arg1, Xen arg2, Xen arg3, Xen arg4) {return(g_make_filter_1(G_FIR_FILTER, arg1, arg2, arg3, arg4, Xen_undefined, Xen_undefined));}
static Xen g_make_iir_filter(Xen arg1, Xen arg2, Xen arg3, Xen arg4) {return(g_make_filter_1(G_IIR_FILTER, arg1, arg2, arg3, arg4, Xen_undefined, Xen_undefined));}
#endif


/* ---------------- env ---------------- */

static Xen g_is_env(Xen obj) 
{
  #define H_is_env "(" S_is_env " gen): " PROC_TRUE " if gen is a " S_env
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_env(Xen_to_mus_any(obj)))));
}


static Xen g_env(Xen obj) 
{
  #define H_env "(" S_env " gen): next sample from envelope generator"
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_env, S_env, "an env generator");

  return(C_double_to_Xen_real(mus_env(g)));
}

#define H_make_env "(" S_make_env " envelope (scaler 1.0) duration (offset 0.0) (base 1.0) end length): \
return a new envelope generator.  'envelope' is a list, vector, or " S_vct " of break-point pairs. To create the envelope, \
these points are offset by 'offset', scaled by 'scaler', and mapped over the time interval defined by \
either 'duration' (seconds) or 'length' (samples).  If 'base' is 1.0, the connecting segments \
are linear, if 0.0 you get a step function, and anything else produces an exponential connecting segment."


#if HAVE_SCHEME
static s7_pointer make_env;
static s7_pointer g_make_env(s7_scheme *sc, s7_pointer args)
{
  mus_any *ge;
  mus_float_t base, scaler, offset, duration;
  mus_long_t end;
  int i, len = 0;
  mus_float_t *brkpts;
  s7_pointer v, p, durp, envp, endp, durationp;

  durationp = s7_caddr(args);
  if (durationp != Xen_false)
    {
      duration = s7_number_to_real(sc, durationp);
      if (duration <= 0.0)
	Xen_out_of_range_error(S_make_env, 3, durationp, "duration <= 0.0?");	
    }
  else duration = 0.0;
  
  envp = s7_car(args);
  if (s7_is_pair(envp))
    {
      s7_pointer ep;
      bool internal_lists = false;
      len = s7_list_length(sc, envp);
      if (s7_is_pair(s7_car(envp)))
	{
	  len *= 2;
	  internal_lists = true;
	}
      else
	{
	  if ((len < 2) || (len & 1))
	    return(s7_wrong_type_arg_error(sc, S_make_env, 1, envp, "a list with an even number of entries"));
	}
      v = s7_make_float_vector(sc, len, 1, NULL);
      brkpts = s7_float_vector_elements(v);
      if (internal_lists)
	{
	  for (i = 0, ep = envp; i < len; i += 2, ep = s7_cdr(ep))
	    {
	      brkpts[i] = s7_number_to_real(sc, s7_caar(ep));
	      brkpts[i + 1] = s7_number_to_real(sc, s7_cadar(ep));
	    }
	}
      else
	{
	  for (i = 0, ep = envp; i < len; i++, ep = s7_cdr(ep))
	    brkpts[i] = s7_number_to_real(sc, s7_car(ep));
	}
    }
  else
    {
      if (s7_is_float_vector(envp))
	{
	  v = envp;
	  len = s7_vector_length(envp);
	  if ((len < 2) || (len & 1))
	    return(s7_wrong_type_arg_error(sc, S_make_env, 1, envp, "a float-vector with an even number of entries"));
	  brkpts = s7_float_vector_elements(envp);
	}
      else
	{
	  if (s7_is_int_vector(envp))
	    return(s7_wrong_type_arg_error(sc, S_make_env, 1, envp, "a list, float-vector, or a normal vector"));
	  else
	    {
	      if (s7_is_vector(envp))
		{
		  s7_pointer *els;
		  len = s7_vector_length(envp);
		  if ((len < 2) || (len & 1))
		    return(s7_wrong_type_arg_error(sc, S_make_env, 1, envp, "a vector with an even number of entries"));
		  v = s7_make_float_vector(sc, len, 1, NULL);
		  brkpts = s7_float_vector_elements(v);	
		  els = s7_vector_elements(envp);
		  for (i = 0; i < len; i++)
		    brkpts[i] = s7_number_to_real(sc, els[i]);
		}
	      else return(s7_wrong_type_arg_error(sc, S_make_env, 1, envp, "a list, float-vector, or vector"));
	    }
	}
    }

  p = s7_cdddr(args);
  endp = s7_caddr(p);
  if (endp != Xen_false)
    {
      if (!s7_is_integer(endp))
	return(s7_wrong_type_arg_error(sc, S_make_env, 6, endp, "an integer"));
      end = s7_integer(endp);
      if (end <= 0)
	Xen_out_of_range_error(S_make_env, 5, endp, "end <= 0?");	
    }
  else end = 0;

  durp = s7_cadddr(p);
  if (durp != Xen_false)
    {
      mus_long_t dur;
      if (!s7_is_integer(durp))
	return(s7_wrong_type_arg_error(sc, S_make_env, 7, durp, "an integer"));
      dur = s7_integer(durp);
      if (dur <= 0)
	Xen_out_of_range_error(S_make_env, 6, durp, "dur <= 0?");
      if ((end > 0) && (end != dur - 1))
	Xen_error(CLM_ERROR,
		  Xen_list_3(C_string_to_Xen_string(S_make_env ": end, ~A, and dur, ~A, specified, but dur != end+1"), endp, durp));
      end = dur - 1;
    }

  if ((end == 0) && (duration == 0.0))
    Xen_out_of_range_error(S_make_env, 0, durationp, "duration <= 0.0?");

  base = s7_number_to_real(sc, s7_cadr(p));
  if (base < 0.0) 
    Xen_out_of_range_error(S_make_env, 5, s7_cadr(p), "base < 0.0?");

  scaler = s7_number_to_real(sc, s7_cadr(args));
  offset = s7_number_to_real(sc, s7_car(p));

  if (duration > (24 * 3600 * 365))
    Xen_out_of_range_error(S_make_env, 0, durationp, "duration > year?");

  /* mus_make_env can raise an error, so we need local redirects here */
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_env(brkpts, len / 2, scaler, offset, base, duration, end, NULL);
    mus_error_set_handler(old_error_handler);
  }

  if (ge) 
    return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, v)));
  return(clm_mus_error(local_error_type, local_error_msg, S_make_env));
}
#else
static Xen g_make_env(Xen arglist)
{
  mus_any *ge;
  Xen args[14];
  Xen keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i;
  mus_float_t base = 1.0, scaler = 1.0, offset = 0.0, duration = 0.0;
  mus_long_t end = 0, dur = -1;
  int npts = 0;
  mus_float_t *brkpts = NULL;
  vct *v = NULL;

  keys[0] = kw_envelope;
  keys[1] = kw_scaler;
  keys[2] = kw_duration;
  keys[3] = kw_offset;
  keys[4] = kw_base;
  keys[5] = kw_end;
  keys[6] = kw_length;

  {
    int arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 14) clm_error(S_make_env, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_env, arglist_len, 7, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      scaler = Xen_optkey_to_float(kw_scaler, keys[1], S_make_env, orig_arg[1], 1.0);

      duration = Xen_optkey_to_float(kw_duration, keys[2], S_make_env, orig_arg[2], 0.0);
      if ((duration < 0.0) || ((duration == 0.0) && (!Xen_is_keyword(keys[2]))))
	Xen_out_of_range_error(S_make_env, orig_arg[2], keys[2], "duration <= 0.0?");

      offset = Xen_optkey_to_float(kw_offset, keys[3], S_make_env, orig_arg[3], 0.0);

      base = Xen_optkey_to_float(kw_base, keys[4], S_make_env, orig_arg[4], 1.0);
      if (base < 0.0) 
	Xen_out_of_range_error(S_make_env, orig_arg[4], keys[4], "base < 0.0?");

      end = Xen_optkey_to_mus_long_t(kw_end, keys[5], S_make_env, orig_arg[5], 0);
      if (end < 0) 
	Xen_out_of_range_error(S_make_env, orig_arg[5], keys[5], "end < 0?");

      dur = Xen_optkey_to_mus_long_t(kw_length, keys[6], S_make_env, orig_arg[6], 0);
      if (dur < 0) 
	Xen_out_of_range_error(S_make_env, orig_arg[6], keys[6], "length < 0?");

      /* env data is a list, checked last to let the preceding throw wrong-type error before calloc  */
      if (!(Xen_is_keyword(keys[0])))
        {
	  int len;
	  Xen vect = XEN_NULL;
	  if (mus_is_vct(keys[0]))
	    {
	      v = Xen_to_vct(keys[0]);
	      len = mus_vct_length(v);
	      if ((len < 2) || (len & 1))
		Xen_error(BAD_TYPE,
			  Xen_list_2(C_string_to_Xen_string(S_make_env ": " S_vct " is a bogus breakpoints list, ~A"), 
				     keys[0]));
	    }
	  else
	    {
#if HAVE_SCHEME
	      /* in Ruby and Forth vectors and lists are the same, so stay with the old code */
	      if (Xen_is_vector(keys[0]))
		{
		  vect = keys[0];
		  len = Xen_vector_length(vect);
		  if ((len < 2) || (len & 1))
		    Xen_error(BAD_TYPE, Xen_list_2(C_string_to_Xen_string(S_make_env ": vector is a bogus breakpoints list, ~A"), vect));
		}
	      else
		{
#endif
		  Xen_check_type(Xen_is_list(keys[0]), keys[0], orig_arg[0], S_make_env, "a list, vector, or " S_vct);
		  len = Xen_list_length(keys[0]);
		  if (len == 0)
		    Xen_error(NO_DATA,
			      Xen_list_2(C_string_to_Xen_string(S_make_env ": null env? ~A"), 
					 keys[0]));
		  
		  if (Xen_is_list(Xen_car(keys[0])))
		    len *= 2;
		  else
		    {
		      if (len & 1)
			Xen_error(BAD_TYPE,
				  Xen_list_2(C_string_to_Xen_string(S_make_env ": odd length breakpoints list? ~A"), 
					     keys[0]));
		      
		      if (!(Xen_is_number(Xen_car(keys[0]))))
			Xen_check_type(false, keys[0], orig_arg[0], S_make_env, "a list of numbers (breakpoints)");
		    }
		}
#if HAVE_SCHEME
	    }
#endif
	  npts = len / 2;
	  if (v)
	    brkpts = mus_vct_data(v);
	  else
	    {
	      brkpts = (mus_float_t *)malloc(len * sizeof(mus_float_t));
	      if (!brkpts)
		return(clm_mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate env list", S_make_env));
	      if (vect)
		{
		  for (i = 0; i < len; i++)
		    brkpts[i] = Xen_real_to_C_double(Xen_vector_ref(vect, i));
		}
	      else
		{
		  Xen lst;
		  if (Xen_is_number(Xen_car(keys[0])))
		    {
		      for (i = 0, lst = Xen_copy_arg(keys[0]); (i < len) && (!Xen_is_null(lst)); i++, lst = Xen_cdr(lst))
			brkpts[i] = Xen_real_to_C_double(Xen_car(lst));
		    }
		  else
		    {
		      for (i = 0, lst = Xen_copy_arg(keys[0]); (i < len) && (!Xen_is_null(lst)); i += 2, lst = Xen_cdr(lst))
			{
			  Xen el;
			  el = Xen_car(lst);
			  if ((Xen_is_pair(el)) &&
			      (Xen_is_number(Xen_car(el))) &&
			      (Xen_is_pair(Xen_cdr(el))) &&
			      (Xen_is_number(Xen_cadr(el))))
			    {
			      brkpts[i] = Xen_real_to_C_double(Xen_car(el));
			      brkpts[i + 1] = Xen_real_to_C_double(Xen_cadr(el));
			    }
			  else 
			    {
			      Xen_error(BAD_TYPE, 
					Xen_list_2(C_string_to_Xen_string(S_make_env ": odd breakpoints list? ~A"), 
						    keys[0]));
			    }
			}
		    }
		}
	    }
        }
    }

  if (!brkpts) 
    {
      Xen_error(NO_DATA,
		Xen_list_1(C_string_to_Xen_string(S_make_env ": no envelope?"))); 
    }

  if (dur > 0)
    {
      if ((end > 0) && ((end + 1) != dur))
	{
	  if ((!v) && (brkpts)) {free(brkpts); brkpts = NULL;}
	  Xen_error(CLM_ERROR,
		    Xen_list_3(C_string_to_Xen_string(S_make_env ": end, ~A, and dur, ~A, specified, but dur != end+1"),
			       keys[5], 
			       keys[6]));
	}
      end = dur - 1;
    }

  /* (make-env '(0 1 1 0) :duration most-positive-fixnum) -> env linear, pass: 0 (dur: -9223372036854775808)...
   */
  if ((end <= 0) && (duration <= 0.0))
    Xen_out_of_range_error(S_make_env, 0, C_double_to_Xen_real(duration), "duration <= 0.0?");
  if (duration > (24 * 3600 * 365))
    Xen_out_of_range_error(S_make_env, 0, C_double_to_Xen_real(duration), "duration > year?");

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_env(brkpts, npts, scaler, offset, base, duration, end, NULL);
    mus_error_set_handler(old_error_handler);
  }
  
  if (ge) 
    {
      if (v) 
	return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, keys[0]))); /* in s7, keys[0] == v */
      return(mus_xen_to_object(mus_any_to_mus_xen_with_vct(ge, xen_make_vct(mus_env_breakpoints(ge) * 2, brkpts))));
    }
  return(clm_mus_error(local_error_type, local_error_msg, S_make_env));
}
#endif

static Xen g_env_interp(Xen x, Xen env1) /* "env" causes trouble in Objective-C!! */
{
  #define H_env_interp "(" S_env_interp " x env): value of envelope env at x"
  Xen_check_type(Xen_is_number(x), x, 1, S_env_interp, "a number");
  Xen_check_type((mus_is_xen(env1)) && (mus_is_env(Xen_to_mus_any(env1))), env1, 2, S_env_interp, "an env generator");
  return(C_double_to_Xen_real(mus_env_interp(Xen_real_to_C_double(x), Xen_to_mus_any(env1))));
}



/* mus_env_any calls the C function itself, so we pass it connect_func,
 *   connect_func uses the function passed as an argument to g_env_any.
 *   I can't think of a cleaner way to handle this except via nested functions.
 *   Both versions seem to work ok with recursive env-any calls.
 */

static Xen current_connect_func;

static mus_float_t connect_func(mus_float_t val)
{
  return(Xen_real_to_C_double(Xen_call_with_1_arg(current_connect_func,
				    C_double_to_Xen_real(val),
				    S_env_any " connect function")));
}

static Xen g_env_any(Xen e, Xen func)
{
  Xen val;
  Xen old_connect_func = Xen_false;

  #define H_env_any "(" S_env_any " e func) uses 'func' to connect the dots in the env 'e'"
  Xen_check_type((mus_is_xen(e)) && (mus_is_env(Xen_to_mus_any(e))), e, 1, S_env_any, "an env generator");
  Xen_check_type((Xen_is_procedure(func)) && (Xen_is_aritable(func, 1)), func, 2, S_env_any, "a function of one arg");
  
  old_connect_func = current_connect_func;
  current_connect_func = func;
  val = C_double_to_Xen_real(mus_env_any(Xen_to_mus_any(e), connect_func));
  current_connect_func = old_connect_func;

  return(val);
}



#define S_envelope_interp "envelope-interp"

static Xen g_envelope_interp(Xen ux, Xen e, Xen ubase)
{
  #define H_envelope_interp "(envelope-interp x e (base 1.0)) -> value of e at x; base controls connecting segment type: (envelope-interp .3 '(0 0 .5 1 1 0)) -> .6"
  mus_float_t x, base = 1.0, x0, y0, y1;
  Xen_check_type(Xen_is_number(ux), ux, 1, S_envelope_interp, "a number");
  Xen_check_type(Xen_is_list(e), e, 2, S_envelope_interp, "a list");

  if (Xen_is_null(e))
    return(xen_float_zero);

  x = Xen_real_to_C_double(ux);
  if (Xen_is_bound(ubase)) base = Xen_real_to_C_double(ubase);
  x0 = Xen_real_to_C_double(Xen_car(e));

  while (true)
    {
      mus_float_t x1;
      Xen ey;
      if (!Xen_is_pair(Xen_cdr(e)))
	Xen_check_type(false, e, 2, S_envelope_interp, "a list of breakpoint values");
      ey = Xen_cadr(e);
      if ((x <= x0) ||
	  (!Xen_is_pair(Xen_cddr(e))))
	return(ey);
      x1 = Xen_real_to_C_double(Xen_caddr(e));
      if (x < x1)
	{
	  if (base == 0.0)
	    return(ey);
	  y0 = Xen_real_to_C_double(ey);
	  y1 = Xen_real_to_C_double(Xen_cadddr(e));
	  if (y0 == y1)
	    return(ey);
	  if (base == 1.0)
	    return(C_double_to_Xen_real(y0 + ((x - x0) * (y1 - y0) / (x1 - x0))));
	  return(C_double_to_Xen_real(y0 + (((y1 - y0) / (base - 1.0)) * (pow(base, (x - x0) / (x1 - x0)) - 1.0))));
	}
      e = Xen_cddr(e);
      x0 = x1;
    }
  return(Xen_false);
}

/* -------------------------------- pulsed-env -------------------------------- */

static Xen g_make_pulsed_env(Xen e, Xen dur, Xen frq)
{
  #define H_make_pulsed_env "(" S_make_pulsed_env " envelope duration frequency) returns a pulsed-env generator."
  Xen gp, ge;
  mus_any *pl;

  pl = mus_make_pulse_train(Xen_real_to_C_double(frq), 1.0, 0.0);
  gp = mus_xen_to_object(mus_any_to_mus_xen(pl));

#if HAVE_SCHEME
  ge = s7_apply_function_star(s7, make_env, s7_list(s7, 3, e, s7_make_real(s7, 1.0), dur));
#else
  ge = g_make_env(Xen_list_3(e, C_double_to_Xen_real(1.0), dur));
#endif

  pl = mus_make_pulsed_env(Xen_to_mus_any(ge), Xen_to_mus_any(gp));
  return(mus_xen_to_object(mus_any_to_mus_xen_with_two_vcts(pl, ge, gp)));
}

static Xen g_is_pulsed_env(Xen os)
{
  #define H_is_pulsed_env "(" S_is_pulsed_env " gen) returns " PROC_TRUE " if gen is a pulsed-env generator."
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && (mus_is_pulsed_env(Xen_to_mus_any(os)))));
}

static Xen g_pulsed_env(Xen g, Xen fm)
{
  #define H_pulsed_env "(" S_pulsed_env " gen fm) runs a pulsed-env generator."
  mus_any *pl = NULL;

  Xen_check_type((mus_is_xen(g)) && (mus_is_pulsed_env(pl = Xen_to_mus_any(g))), g, 1, S_pulsed_env, "a pulsed-env object");

  if (Xen_is_number(fm))
    return(C_double_to_Xen_real(mus_pulsed_env(pl, Xen_real_to_C_double(fm))));
  return(C_double_to_Xen_real(mus_pulsed_env_unmodulated(pl)));
}




/* ---------------- io ---------------- */

#if (!HAVE_RUBY)
  #define S_output "*output*"
  #define S_reverb "*reverb*"
#else
  #define S_output "output"
  #define S_reverb "reverb"
#endif

static Xen clm_output, clm_reverb; /* *output* and *reverb* at extlang level -- these can be output streams, vct, sound-data objects etc */

#if (HAVE_SCHEME)
static Xen clm_output_slot = NULL, clm_reverb_slot = NULL;

#define CLM_OUTPUT s7_slot_value(clm_output_slot)
#define CLM_REVERB s7_slot_value(clm_reverb_slot)

#else

#define CLM_OUTPUT Xen_variable_ref(S_output)
#define CLM_REVERB Xen_variable_ref(S_reverb)

#endif


static Xen g_is_mus_input(Xen obj) 
{
  #define H_is_mus_input "(" S_is_mus_input " gen): " PROC_TRUE " if gen is an input generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_input(Xen_to_mus_any(obj)))));
}


static Xen g_is_mus_output(Xen obj) 
{
  #define H_is_mus_output "(" S_is_mus_output " gen): " PROC_TRUE " if gen is an output generator"

  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_output(Xen_to_mus_any(obj)))));
}


static Xen g_is_file_to_sample(Xen obj) 
{
  #define H_is_file_to_sample "(" S_is_file_to_sample " gen): " PROC_TRUE " if gen is a " S_file_to_sample " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_file_to_sample(Xen_to_mus_any(obj)))));
}

static Xen mus_clm_output(void) {return(CLM_OUTPUT);}
static Xen mus_clm_reverb(void) {return(CLM_REVERB);}

static Xen g_is_file_to_frample(Xen obj) 
{
  #define H_is_file_to_frample "(" S_is_file_to_frample " gen): " PROC_TRUE " if gen is a " S_file_to_frample " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_file_to_frample(Xen_to_mus_any(obj)))));
}


static Xen g_is_sample_to_file(Xen obj) 
{
  #define H_is_sample_to_file "(" S_is_sample_to_file " gen): " PROC_TRUE " if gen is a " S_sample_to_file " generator"

  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_sample_to_file(Xen_to_mus_any(obj)))));
}


static Xen g_is_frample_to_file(Xen obj) 
{
  #define H_is_frample_to_file "(" S_is_frample_to_file " gen): " PROC_TRUE " if gen is a " S_frample_to_file " generator"

  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_frample_to_file(Xen_to_mus_any(obj)))));
}


#if HAVE_SCHEME
static mus_float_t (*in_any_2)(mus_long_t pos, int chn);
#endif

static mus_float_t in_any_3(const char *caller, mus_long_t pos, int in_chan, Xen inp)
{
#if HAVE_SCHEME
  if (Xen_is_false(inp)) return(0.0); /* ws.scm default for *clm-reverb* is #f */
  if (inp == CLM_REVERB)
    return(in_any_2(pos, in_chan));
#endif

  if (mus_is_xen(inp))
    {
      Xen_check_type(mus_is_input(Xen_to_mus_any(inp)), inp, 3, caller, "an input generator");
      return(mus_in_any(pos, in_chan, (mus_any *)Xen_to_mus_any(inp)));
    }

  if (mus_is_vct(inp))
    {
#if HAVE_SCHEME
      if (pos < s7_vector_length(inp))
	{
	  s7_double *els;
	  s7_int rank;
	  els = s7_float_vector_elements(inp);
	  rank = s7_vector_rank(inp);
	  if (rank > 1)
	    {
	      s7_int *offsets;
	      s7_double x;
	      offsets = (s7_int *)malloc(rank * sizeof(s7_int));
	      s7_vector_offsets(inp, offsets, rank);
	      x = els[in_chan * offsets[0] + pos];
	      free(offsets);
	      return(x);
	    }
	  return(els[pos]);
	}
      return(0.0);
#else
      vct *v;
      mus_float_t *vdata;
      v = Xen_to_vct(inp);
      vdata = mus_vct_data(v);
      if (pos < mus_vct_length(v))
	return(vdata[pos]);
      return(0.0);
#endif
    }

  if (Xen_is_vector(inp))
    {
      if (pos < Xen_vector_length(inp))
	return(Xen_real_to_C_double(Xen_vector_ref(inp, pos)));
    }

  return(0.0);
}

static Xen g_in_any_1(const char *caller, Xen frample, int in_chan, Xen inp)
{
  mus_long_t pos;

  Xen_check_type(Xen_is_integer(frample), frample, 1, caller, "an integer");

  pos = Xen_llong_to_C_llong(frample);
  if (pos < 0) 
    Xen_out_of_range_error(caller, 1, frample, "location should be >= 0");    

  if (in_chan < 0) 
    Xen_out_of_range_error(caller, 2, C_int_to_Xen_integer(in_chan), "must be >= 0");    

  return(C_double_to_Xen_real(in_any_3(caller, pos, in_chan, inp)));
}


static Xen g_in_any(Xen frample, Xen chan, Xen inp) 
{
  #define H_in_any "(" S_in_any " frample chan stream): input stream sample at frample in channel chan"
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_in_any, "an integer");
  return(g_in_any_1(S_in_any, frample, Xen_integer_to_C_int(chan), inp));
}


static Xen g_ina(Xen frample, Xen inp) 
{
  #define H_ina "(" S_ina " frample stream): input stream sample in channel 0 at frample"
  return(g_in_any_1(S_ina, frample, 0, inp));
}


static Xen g_inb(Xen frample, Xen inp) 
{
  #define H_inb "(" S_inb " frample stream): input stream sample in channel 1 at frample"
  return(g_in_any_1(S_inb, frample, 1, inp));
}


#if (!HAVE_SCHEME)
static Xen out_any_2(Xen outp, mus_long_t pos, mus_float_t inv, int chn, const char *caller)
#else
static Xen fallback_out_any_2(Xen outp, mus_long_t pos, mus_float_t inv, int chn, const char *caller)
#endif
{
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(outp, mus_xen_tag);
  if (gn)
    {
      /* mus_out_any will check the writer so output_p is pointless */
      mus_out_any(pos, inv, chn, mus_xen_to_mus_any(gn));
      return(xen_float_zero);
    }

  if (mus_is_vct(outp))
    {
      mus_float_t *vdata;
      vct *v;
      v = xen_to_vct(outp);
      vdata = mus_vct_data(v);
      if (Xen_vector_rank(outp) == 1)
	{
	  if (chn == 0)
	    {
	      if (pos < mus_vct_length(v))
		vdata[pos] += inv;
	    }
	}
#if HAVE_SCHEME
      else
	{
	  s7_int *offsets;
	  s7_int rank;
	  rank = s7_vector_rank(outp);
	  offsets = (s7_int *)malloc(rank * sizeof(s7_int));
	  s7_vector_offsets(outp, offsets, rank);
	  pos += (chn * offsets[0]);
	  if (pos < mus_vct_length(v))
	    vdata[pos] += inv;
	  free(offsets);
	}
#endif
      return(xen_float_zero);
    }

  if (Xen_is_vector(outp))
    {
      if (pos < Xen_vector_length(outp))
	Xen_vector_set(outp, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(outp, pos)) + inv));
    }

  return(xen_float_zero);
}

#if HAVE_SCHEME

static Xen (*out_any_2)(mus_long_t pos, mus_float_t inv, int chn, const char *caller);

bool mus_simple_out_any_to_file(mus_long_t samp, mus_float_t val, int chan, mus_any *IO);
bool mus_simple_outa_to_file(mus_long_t samp, mus_float_t val, mus_any *IO);

static mus_xen *clm_output_gn = NULL;
static mus_any *clm_output_gen = NULL;
static vct *clm_output_vct;

static Xen out_any_2_to_mus_xen(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  mus_out_any(pos, inv, chn, clm_output_gen);
  return(xen_float_zero);
}

static Xen safe_out_any_2_to_mus_xen(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  if (!mus_simple_out_any_to_file(pos, inv, chn, clm_output_gen))
    mus_safe_out_any_to_file(pos, inv, chn, clm_output_gen);
  return(xen_float_zero);
}


static Xen out_any_2_to_vct(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  mus_float_t *vdata;
  vdata = mus_vct_data(clm_output_vct);

#if (!HAVE_SCHEME)
  if ((chn == 0) &&
      (pos < mus_vct_length(clm_output_vct)))
    vdata[pos] += inv;
#else
  if (Xen_vector_rank(clm_output_vct) == 1)
    {
      if ((chn == 0) &&
	  (pos < mus_vct_length(clm_output_vct)))
	vdata[pos] += inv;
    }
  else
    {
      s7_int chans;
      chans = s7_vector_dimension(clm_output_vct, 0);
      if (chn < chans)
	{
	  s7_int chan_len;
	  chan_len = s7_vector_dimension(clm_output_vct, 1);
	  if (pos < chan_len)
	    vdata[chn * chan_len + pos] += inv;
	}
    }
#endif
  return(xen_float_zero);
}


static Xen out_any_2_to_vector(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  if (pos < Xen_vector_length(CLM_OUTPUT))
    Xen_vector_set(CLM_OUTPUT, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(CLM_OUTPUT, pos)) + inv));
  return(xen_float_zero);
}

static Xen out_any_2_no_op(mus_long_t pos, mus_float_t inv, int chn, const char *caller)
{
  return(xen_float_zero);
}

static s7_pointer g_clm_output_set(s7_scheme *sc, s7_pointer args)
{
  s7_pointer new_output;
  new_output = s7_cadr(args);

  clm_output_gn = (mus_xen *)Xen_object_ref_checked(new_output, mus_xen_tag);
  if (clm_output_gn)
    {
      out_any_2 = out_any_2_to_mus_xen;
      clm_output_gen = clm_output_gn->gen;

      if (mus_out_any_is_safe(clm_output_gen))
	out_any_2 = safe_out_any_2_to_mus_xen;
    }
  else
    {
      clm_output_gen = NULL;
      if (mus_is_vct(new_output))
	{
	  out_any_2 = out_any_2_to_vct;
	  clm_output_vct = xen_to_vct(new_output);
	}
      else
	{
	  if (Xen_is_vector(new_output))
	    {
	      out_any_2 = out_any_2_to_vector;
	    }
	  else out_any_2 = out_any_2_no_op;
	}
    }
  return(new_output);
}


/* need in_any_2(pos, 0, caller) -> double + safe case + none-file cases
 */

static mus_xen *clm_input_gn;
static mus_any *clm_input_gen;
static vct *clm_input_vct;

static mus_float_t in_any_2_to_mus_xen(mus_long_t pos, int chn)
{
  return(mus_in_any(pos, chn, clm_input_gen));
}

static mus_float_t safe_in_any_2_to_mus_xen(mus_long_t pos, int chn)
{
  return(mus_file_to_sample(clm_input_gen, pos, chn));
}

static mus_float_t in_any_2_to_vct(mus_long_t pos, int chn)
{
  mus_float_t *vdata;
  vdata = mus_vct_data(clm_input_vct);
  if ((chn == 0) &&
      (pos < mus_vct_length(clm_input_vct)))
    return(vdata[pos]);
  return(0.0);
}

static mus_float_t in_any_2_to_vector(mus_long_t pos, int chn)
{
  if (pos < Xen_vector_length(CLM_REVERB))
    return(Xen_real_to_C_double(Xen_vector_ref(CLM_REVERB, pos)));
  return(0.0);
}

static mus_float_t in_any_2_no_op(mus_long_t pos, int chn)
{
  return(0.0);
}

static s7_pointer g_clm_reverb_set(s7_scheme *sc, s7_pointer args)
{
  s7_pointer new_input;
  new_input = s7_cadr(args);

  clm_input_gn = (mus_xen *)Xen_object_ref_checked(new_input, mus_xen_tag);
  if (clm_input_gn)
    {
      in_any_2 = in_any_2_to_mus_xen;
      clm_input_gen = clm_input_gn->gen;

      if (mus_in_any_is_safe(clm_input_gen))
	in_any_2 = safe_in_any_2_to_mus_xen;
    }
  else
    {
      if (mus_is_vct(new_input))
	{
	  in_any_2 = in_any_2_to_vct;
	  clm_input_vct = xen_to_vct(new_input);
	}
      else
	{
	  if (Xen_is_vector(new_input))
	    {
	      in_any_2 = in_any_2_to_vector;
	    }
	  else in_any_2 = in_any_2_no_op;
	}
    }
  return(new_input);
}


#endif


#define S_out_bank "out-bank"
static Xen g_out_bank(Xen gens, Xen loc, Xen inval)
{
  #define H_out_bank "(out-bank gens location val) calls each generator in the gens vector, passing it the argument val, then \
sends that output to the output channels in the vector order (the first generator writes to outa, the second to outb, etc)."

  mus_long_t pos;
  int i, size;
  mus_float_t x = 0.0;

  Xen_check_type(Xen_is_integer(loc), loc, 2, S_out_bank, "an integer");
  pos = Xen_llong_to_C_llong(loc);
  if (pos < 0) 
    Xen_out_of_range_error(S_out_bank, 2, loc, "must be >= 0");    

  Xen_check_type(Xen_is_vector(gens), gens, 1, S_out_bank, "a vector of generators");
  size = Xen_vector_length(gens);

  Xen_check_type(Xen_is_number(inval), inval, 3, S_out_bank, "a number");
  x = Xen_real_to_C_double(inval);

#if HAVE_SCHEME  
  for (i = 0; i < size; i++)
    {
      mus_any *g = NULL;
      mus_xen *gn;
      Xen_to_C_any_generator(Xen_vector_ref(gens, i), gn, g, S_out_bank, "an output generator");
      out_any_2(pos, mus_apply(g, x, 0.0), i, S_out_bank);
    }
#else
  for (i = 0; i < size; i++)
    {
      mus_any *g = NULL;
      mus_xen *gn;
      Xen_to_C_any_generator(Xen_vector_ref(gens, i), gn, g, S_out_bank, "an output generator");
      out_any_2(CLM_OUTPUT, pos, mus_apply(g, x, 0.0), i, S_out_bank);
    }
#endif

  return(inval);
}


static Xen g_out_any_1(const char *caller, Xen frample, int chn, Xen val, Xen outp)
{
  mus_long_t pos = 0;
  mus_float_t inv;

  if (chn < 0)
    Xen_out_of_range_error(caller, 3, C_int_to_Xen_integer(chn), "must be >= 0");    
  Xen_to_C_integer_or_error(frample, pos, caller, 1);
  if (pos < 0) 
    Xen_out_of_range_error(caller, 1, frample, "must be >= 0");    

  Xen_to_C_double_or_error(val, inv, caller, 2);

  if (!Xen_is_bound(outp))
#if (!HAVE_SCHEME)
    return(out_any_2(CLM_OUTPUT, pos, inv, chn, caller));
#else
    return(out_any_2(pos, inv, chn, caller));
#endif

#if (!HAVE_SCHEME)
  return(out_any_2(outp, pos, inv, chn, caller));
#else
  if (outp == CLM_OUTPUT)
    return(out_any_2(pos, inv, chn, caller));
  return(fallback_out_any_2(outp, pos, inv, chn, caller));
#endif
}

static Xen g_out_any(Xen frample, Xen val, Xen chan, Xen outp)
{
  #define H_out_any "(" S_out_any " frample val chan stream): add val to output stream at frample in channel chan"
  Xen_check_type(Xen_is_integer(chan), chan, 3, S_out_any, "an integer");
  return(g_out_any_1(S_out_any, frample, Xen_integer_to_C_int(chan), val, outp));
}


static Xen g_outa(Xen frample, Xen val, Xen outp)
{
  #define H_outa "(" S_outa " frample val stream): add val to output stream at frample in channel 0"
  return(g_out_any_1(S_outa, frample, 0, val, outp));
}

static Xen g_outb(Xen frample, Xen val, Xen outp)
{
  #define H_outb "(" S_outb " frample val stream): add val to output stream at frample in channel 1"
  return(g_out_any_1(S_outb, frample, 1, val, outp));
}


static Xen g_outc(Xen frample, Xen val, Xen outp)
{
  #define H_outc "(" S_outc " frample val stream): add val to output stream at frample in channel 2"
  return(g_out_any_1(S_outc, frample, 2, val, outp));
}


static Xen g_outd(Xen frample, Xen val, Xen outp)
{
  #define H_outd "(" S_outd " frample val stream): add val to output stream at frample in channel 3"
  return(g_out_any_1(S_outd, frample, 3, val, outp));
}


static Xen g_mus_close(Xen ptr)
{
  #define H_mus_close "(" S_mus_close " gen): close the IO stream managed by 'gen' (a sample->file generator, for example)"

  if (mus_is_xen(ptr))
    return(C_int_to_Xen_integer(mus_close_file((mus_any *)Xen_to_mus_any(ptr))));

  Xen_check_type(mus_is_vct(ptr) || Xen_is_false(ptr) || Xen_is_vector(ptr), 
		  ptr, 1, S_mus_close, "an IO gen or its outa equivalent");
  return(Xen_integer_zero);
}


static Xen g_make_file_to_sample(Xen name, Xen buffer_size)
{
  #define H_make_file_to_sample "(" S_make_file_to_sample " filename buffer-size): return an input generator reading 'filename' (a sound file)"

  mus_any *ge;
  mus_long_t size;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_file_to_sample, "a string");
  Xen_check_type(Xen_is_llong_or_unbound(buffer_size), buffer_size, 2, S_make_file_to_sample, "an integer");

  if (!(mus_file_probe(Xen_string_to_C_string(name))))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_file_to_sample ": ~S, ~A"),
			 name,
			 C_string_to_Xen_string(STRERROR(errno))));

  if (Xen_is_llong(buffer_size))
    {
      size = Xen_llong_to_C_llong(buffer_size);
      if (size <= 0)
	Xen_out_of_range_error(S_make_file_to_sample, 2, buffer_size, "must be > 0");
    }
  else size = mus_file_buffer_size();

  ge = mus_make_file_to_sample_with_buffer_size(Xen_string_to_C_string(name), size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_file_to_sample(Xen obj, Xen samp, Xen chan)
{
  #define H_file_to_sample "(" S_file_to_sample " obj frample chan): sample value in sound file read by 'obj' in channel chan at frample"
  int channel = 0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_input, S_file_to_sample, "an input generator");
  Xen_check_type(Xen_is_llong(samp), samp, 2, S_file_to_sample, "an integer");

  if (Xen_is_bound(chan))
    {
      Xen_check_type(Xen_is_integer(chan), chan, 3, S_file_to_sample, "an integer");
      channel = Xen_integer_to_C_int(chan);
    }
  return(C_double_to_Xen_real(mus_file_to_sample(g, Xen_llong_to_C_llong(samp), channel)));
}


static Xen g_make_sample_to_file(Xen name, Xen chans, Xen out_format, Xen out_type, Xen comment)
{
  #if HAVE_SCHEME
    #define make_sample_to_file_example "(" S_make_sample_to_file " \"test.snd\" 2 mus-lshort mus-riff)"
  #endif
  #if HAVE_RUBY
    #define make_sample_to_file_example "\"test.snd\" 2 Mus_lshort Mus_riff make_sample2file"
  #endif
  #if HAVE_FORTH
    #define make_sample_to_file_example "\"test.snd\" 2 mus-lshort mus-riff make-sample->file"
  #endif

  #define H_make_sample_to_file "(" S_make_sample_to_file " filename chans sample-type header-type comment): \
return an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'sample-type' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n  " make_sample_to_file_example

  mus_sample_t df = MUS_OUT_SAMPLE_TYPE;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_sample_to_file, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(chans), chans, 2, S_make_sample_to_file, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(out_format), out_format, 3, S_make_sample_to_file, "an integer (sample type)");
  Xen_check_type(Xen_is_integer_or_unbound(out_type), out_type, 4, S_make_sample_to_file, "an integer (header type)");

  if (Xen_is_integer(out_format)) df = (mus_sample_t)Xen_integer_to_C_int(out_format);
  if (mus_is_sample_type(df))
    {
      mus_header_t ht = MUS_NEXT;
      if (Xen_is_integer(out_type)) ht = (mus_header_t)Xen_integer_to_C_int(out_type);
      if (mus_is_header_type(ht))
	{
	  int chns = 1;
	  if (Xen_is_integer(chans)) chns = Xen_integer_to_C_int(chans);
	  if (chns > 0)
	    {
	      mus_any *rgen;
	      rgen = mus_make_sample_to_file_with_comment(Xen_string_to_C_string(name),
							  chns, df, ht,
							  (Xen_is_string(comment)) ? Xen_string_to_C_string(comment) : NULL);
	      if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
	    }
	  else Xen_out_of_range_error(S_make_sample_to_file, 2, chans, "chans <= 0?");
	}
      else Xen_out_of_range_error(S_make_sample_to_file, 4, out_type, "invalid header type");
    }
  else Xen_out_of_range_error(S_make_sample_to_file, 3, out_format, "invalid sample type");
  return(Xen_false);
}


static Xen g_continue_sample_to_file(Xen name)
{
  #define H_continue_sample_to_file "(" S_continue_sample_to_file " filename): return an output generator \
that reopens an existing sound file 'filename' ready for output via " S_sample_to_file

  mus_any *rgen = NULL;
  Xen_check_type(Xen_is_string(name), name, 1, S_continue_sample_to_file, "a string");
  rgen = mus_continue_sample_to_file(Xen_string_to_C_string(name));
  if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
  return(Xen_false);
}


static Xen g_sample_to_file(Xen obj, Xen samp, Xen chan, Xen val)
{
  #define H_sample_to_file "(" S_sample_to_file " obj samp chan val): add val to the output stream \
handled by the output generator 'obj', in channel 'chan' at frample 'samp'"

  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_any_generator(obj, gn, g, S_sample_to_file, "an output generator");
  Xen_check_type(mus_is_output(g), obj, 1, S_sample_to_file, "an output generator");

  Xen_check_type(Xen_is_integer(samp), samp, 2, S_sample_to_file, "an integer");
  Xen_check_type(Xen_is_integer(chan), chan, 3, S_sample_to_file, "an integer");
  Xen_check_type(Xen_is_number(val), val, 4, S_sample_to_file, "a number");

  mus_sample_to_file(g,
		     Xen_llong_to_C_llong(samp),
		     Xen_integer_to_C_int(chan),
		     Xen_real_to_C_double(val));
  return(val);
}


static Xen g_sample_to_file_add(Xen obj1, Xen obj2)
{
  #define H_sample_to_file_add "(" S_sample_to_file_add " obj1 obj2): mixes obj2 (an output generator) into obj1 (also an output generator)"
  mus_any *g1 = NULL, *g2 = NULL;
  mus_xen *gn1, *gn2;

  Xen_to_C_any_generator(obj1, gn1, g1, S_sample_to_file_add, "an output generator");
  Xen_to_C_any_generator(obj2, gn2, g2, S_sample_to_file_add, "an output generator");
  Xen_check_type(mus_is_output(g1), obj1, 1, S_sample_to_file_add, "an output generator");
  Xen_check_type(mus_is_output(g2), obj2, 2, S_sample_to_file_add, "an output generator");

  mus_sample_to_file_add(g1, g2);
  return(obj1);
}

static Xen g_make_file_to_frample(Xen name, Xen buffer_size)
{
  #define H_make_file_to_frample "(" S_make_file_to_frample " filename buffer-size): return an input generator reading 'filename' (a sound file)"

  mus_any *ge;
  mus_long_t size;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_file_to_frample, "a string");
  Xen_check_type(Xen_is_llong_or_unbound(buffer_size), buffer_size, 2, S_make_file_to_frample, "an integer");

  if (!(mus_file_probe(Xen_string_to_C_string(name))))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_file_to_frample ": ~S, ~A"),
			 name,
			 C_string_to_Xen_string(STRERROR(errno))));

  if (Xen_is_llong(buffer_size))
    {
      size = Xen_llong_to_C_llong(buffer_size);
      if (size <= 0)
	Xen_out_of_range_error(S_make_file_to_frample, 2, buffer_size, "must be > 0");
    }
  else size = mus_file_buffer_size();
  ge = mus_make_file_to_frample_with_buffer_size(Xen_string_to_C_string(name), size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_file_to_frample(Xen obj, Xen samp, Xen outfr)
{
  #define H_file_to_frample "(" S_file_to_frample " obj samp outf): frample of samples at frample 'samp' in sound file read by 'obj'"
  
  Xen_check_type((mus_is_xen(obj)) && (mus_is_input(Xen_to_mus_any(obj))), obj, 1, S_file_to_frample, "an input generator");
  Xen_check_type(Xen_is_integer(samp), samp, 2, S_file_to_frample, "an integer");

  mus_file_to_frample(Xen_to_mus_any(obj), Xen_llong_to_C_llong(samp), mus_vct_data(Xen_to_vct(outfr)));
  return(outfr);
}


static Xen g_make_frample_to_file(Xen name, Xen chans, Xen out_format, Xen out_type, Xen comment)
{
  #if HAVE_SCHEME
    #define make_frample_to_file_example "(" S_make_frample_to_file " \"test.snd\" 2 mus-lshort mus-riff)"
  #endif
  #if HAVE_RUBY
    #define make_frample_to_file_example "\"test.snd\" 2 Mus_lshort Mus_riff make_frample2file"
  #endif
  #if HAVE_FORTH
    #define make_frample_to_file_example "\"test.snd\" 2 mus-lshort mus-riff make-frample->file"
  #endif

  #define H_make_frample_to_file "(" S_make_frample_to_file " filename chans sample-type header-type comment): \
return an output generator writing the sound file 'filename' which is set up to have \
'chans' channels of 'sample-type' samples with a header of 'header-type'.  The latter \
should be sndlib identifiers:\n  " make_frample_to_file_example

  mus_any *fgen = NULL;

  Xen_check_type(Xen_is_string(name), name, 1, S_make_frample_to_file, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(chans), chans, 2, S_make_frample_to_file, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(out_format), out_format, 3, S_make_frample_to_file, "an integer (sample type id)");
  Xen_check_type(Xen_is_integer_or_unbound(out_type), out_type, 4, S_make_frample_to_file, "an integer (header-type id)");

  fgen = mus_make_frample_to_file_with_comment(Xen_string_to_C_string(name),
					     (Xen_is_integer(chans)) ? Xen_integer_to_C_int(chans) : 1,
					     (Xen_is_integer(out_format)) ? (mus_sample_t)Xen_integer_to_C_int(out_format) : MUS_OUT_SAMPLE_TYPE,
					     (Xen_is_integer(out_type)) ? (mus_header_t)Xen_integer_to_C_int(out_type) : MUS_NEXT,
					     (Xen_is_string(comment)) ? Xen_string_to_C_string(comment) : NULL);
  if (fgen) return(mus_xen_to_object(mus_any_to_mus_xen(fgen)));
  return(Xen_false);
}


static Xen g_continue_frample_to_file(Xen name)
{
  #define H_continue_frample_to_file "(" S_continue_frample_to_file " filename): return an output generator \
that reopens an existing sound file 'filename' ready for output via " S_frample_to_file

  mus_any *rgen = NULL;
  Xen_check_type(Xen_is_string(name), name, 1, S_continue_frample_to_file, "a string");
  rgen = mus_continue_frample_to_file(Xen_string_to_C_string(name));
  if (rgen) return(mus_xen_to_object(mus_any_to_mus_xen(rgen)));
  return(Xen_false);
}


static Xen g_frample_to_file(Xen obj, Xen samp, Xen val)
{
  #define H_frample_to_file "(" S_frample_to_file " obj samp val): add frample 'val' to the output stream \
handled by the output generator 'obj' at frample 'samp'"
  mus_xen *gn;

  gn = (mus_xen *)Xen_object_ref_checked(obj, mus_xen_tag);
  Xen_check_type(((gn) && (mus_is_output(gn->gen))), obj, 1, S_frample_to_file, "an output generator");
  Xen_check_type(Xen_is_integer(samp), samp, 2, S_frample_to_file, "an integer");

  mus_frample_to_file(gn->gen, Xen_llong_to_C_llong(samp), mus_vct_data(Xen_to_vct(val)));
  return(val);
}




/* ---------------- readin ---------------- */

static Xen g_is_readin(Xen obj) 
{
  #define H_is_readin "(" S_is_readin " gen): " PROC_TRUE " if gen is a " S_readin
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_readin(Xen_to_mus_any(obj)))));
}


static Xen g_readin(Xen obj)
{
  #define H_readin "(" S_readin " gen): next sample from readin generator (a sound file reader)"
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_readin, S_readin, "a readin generator");

  return(C_double_to_Xen_real(mus_readin(g)));
}

#define H_make_readin "(" S_make_readin " file (channel 0) (start 0) (direction 1) size): \
return a new readin (file input) generator reading the sound file 'file' starting at frample \
'start' in channel 'channel' and reading forward if 'direction' is not -1"

#if HAVE_SCHEME
static s7_pointer g_make_readin(s7_scheme *sc, s7_pointer args)
{
  mus_any *ge;
  const char *file = NULL;
  int chans, channel, direction;
  mus_long_t start, buffer_size;
  s7_pointer fp;
  
  fp = s7_car(args);
  if (!s7_is_string(fp))
    Xen_out_of_range_error(S_make_readin, 1, fp, "no file name given");
  file = s7_string(fp);
  if (!(mus_file_probe(file)))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_readin ": ~S, ~A"),
			 C_string_to_Xen_string(file),
			 C_string_to_Xen_string(STRERROR(errno))));
  chans = mus_sound_chans(file);
  if (chans <= 0)
    Xen_error(BAD_HEADER,
	      Xen_list_2(C_string_to_Xen_string(S_make_readin ": ~S chans <= 0?"),
			 C_string_to_Xen_string(file)));

  fp = s7_cadr(args);
  if (!s7_is_integer(fp))
    return(s7_wrong_type_arg_error(sc, S_make_readin, 2, fp, "an integer"));
  channel = s7_integer(fp);
  if (channel < 0)
    Xen_out_of_range_error(S_make_readin, 2, fp, "channel < 0?");
  if (channel >= chans)
    Xen_out_of_range_error(S_make_readin, 2, fp, "channel > available chans?");

  fp = s7_caddr(args);
  if (!s7_is_integer(fp))
    return(s7_wrong_type_arg_error(sc, S_make_readin, 3, fp, "an integer"));
  start = s7_integer(fp);
  if (start < 0)
    Xen_out_of_range_error(S_make_readin, 3, fp, "start < 0?");

  args = s7_cdddr(args);
  fp = s7_car(args);
  if (!s7_is_integer(fp))
    return(s7_wrong_type_arg_error(sc, S_make_readin, 4, fp, "an integer"));
  direction = s7_integer(fp);

  fp = s7_cadr(args);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_readin, 5, fp, "an integer"));
      buffer_size = s7_integer(fp);
      if (buffer_size <= 0)
	Xen_out_of_range_error(S_make_readin, 5, fp, "must be > 0");
    }
  else buffer_size = mus_file_buffer_size();

  ge = mus_make_readin_with_buffer_size(file, channel, start, direction, buffer_size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_readin(Xen arglist)
{
  /* optkey file channel start direction size */
  mus_any *ge;
  const char *file = NULL;
  Xen args[10];
  Xen keys[5];
  int orig_arg[5] = {0, 0, 0, 0, 0};
  int vals, chans;
  mus_long_t buffer_size;
  int channel = 0, direction = 1;
  mus_long_t start = 0;

  keys[0] = kw_file;
  keys[1] = kw_channel;
  keys[2] = kw_start;
  keys[3] = kw_direction;
  keys[4] = kw_size;

  buffer_size = mus_file_buffer_size();
  /* this is only 8192! (clm.h MUS_DEFAULT_FILE_BUFFER_SIZE) */

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 10) clm_error(S_make_readin, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_readin, arglist_len, 5, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_make_readin, orig_arg[0], NULL); /* not copied */

      channel = Xen_optkey_to_int(kw_channel, keys[1], S_make_readin, orig_arg[1], channel);
      if (channel < 0)
	Xen_out_of_range_error(S_make_readin, orig_arg[1], keys[1], "channel < 0?");

      start = Xen_optkey_to_mus_long_t(kw_start, keys[2], S_make_readin, orig_arg[2], start);

      direction = Xen_optkey_to_int(kw_direction, keys[3], S_make_readin, orig_arg[3], direction);

      buffer_size = Xen_optkey_to_mus_long_t(kw_size, keys[4], S_make_readin, orig_arg[4], buffer_size);
      if (buffer_size <= 0)
	Xen_out_of_range_error(S_make_readin, orig_arg[4], keys[4], "must be > 0");
    }

  if (!file)
    Xen_out_of_range_error(S_make_readin, orig_arg[0], keys[0], "no file name given");
  if (!(mus_file_probe(file)))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_make_readin ": ~S, ~A"),
			 C_string_to_Xen_string(file),
			 C_string_to_Xen_string(STRERROR(errno))));

  chans = mus_sound_chans(file);
  if (chans <= 0)
    Xen_error(BAD_HEADER,
	      Xen_list_2(C_string_to_Xen_string(S_make_readin ": ~S chans <= 0?"),
			 C_string_to_Xen_string(file)));

  if (channel >= chans)
    Xen_out_of_range_error(S_make_readin, orig_arg[1], keys[1], "channel > available chans?");

  ge = mus_make_readin_with_buffer_size(file, channel, start, direction, buffer_size);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif


/* ---------------- locsig ---------------- */

static Xen g_locsig_ref(Xen obj, Xen chan)
{
  #define H_locsig_ref "(" S_locsig_ref " gen chan): locsig 'gen' channel 'chan' scaler"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_ref, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_ref, "an integer");
  return(C_double_to_Xen_real(mus_locsig_ref(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan))));
}


static Xen g_locsig_set(Xen obj, Xen chan, Xen val)
{
  #define H_locsig_set "(" S_locsig_set " gen chan val): set the locsig generator's channel 'chan' scaler to 'val'"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_set, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_set, "an integer");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(val), val, 3, S_locsig_set, "a number");
  mus_locsig_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), Xen_real_to_C_double(val));
#else
  mus_locsig_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), s7_number_to_real_with_caller(s7, val, S_locsig_set));
#endif
  return(val);
}


static Xen g_locsig_reverb_ref(Xen obj, Xen chan)
{
  #define H_locsig_reverb_ref "(" S_locsig_reverb_ref " gen chan): locsig reverb channel 'chan' scaler"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_reverb_ref, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_reverb_ref, "an integer");
  return(C_double_to_Xen_real(mus_locsig_reverb_ref(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan))));
}


static Xen g_locsig_reverb_set(Xen obj, Xen chan, Xen val)
{
  #define H_locsig_reverb_set "(" S_locsig_reverb_set " gen chan val): set the locsig reverb channel 'chan' scaler to 'val'"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_locsig_reverb_set, "a locsig generator");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_locsig_reverb_set, "an integer");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(val), val, 3, S_locsig_reverb_set, "a number");
  mus_locsig_reverb_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), Xen_real_to_C_double(val));
#else
  mus_locsig_reverb_set(Xen_to_mus_any(obj), Xen_integer_to_C_int(chan), s7_number_to_real_with_caller(s7, val, S_locsig_reverb_set));
#endif
  return(val);
}


static Xen g_is_locsig(Xen obj)
{
  #define H_is_locsig "(" S_is_locsig " gen): " PROC_TRUE " if gen is a " S_locsig
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj)))));
}


static void mus_locsig_or_move_sound_to_vct_or_sound_data(mus_xen *ms, mus_any *loc_gen, mus_long_t pos, bool from_locsig)
{
  mus_float_t *outfr = NULL, *revfr = NULL;
  Xen output, reverb;
#if HAVE_SCHEME
  int chans, rev_chans;
#endif
  if (pos < 0) return;

  if (from_locsig)
    {
      outfr = mus_locsig_outf(loc_gen);
      revfr = mus_locsig_revf(loc_gen);
#if HAVE_SCHEME
      chans = mus_locsig_channels(loc_gen);
      rev_chans = mus_locsig_reverb_channels(loc_gen);
#endif
    }
  else
    {
      outfr = mus_move_sound_outf(loc_gen);
      revfr = mus_move_sound_revf(loc_gen);
#if HAVE_SCHEME
      chans = mus_move_sound_channels(loc_gen);
      rev_chans = mus_move_sound_reverb_channels(loc_gen);
#endif
    }
  output = ms->vcts[G_LOCSIG_OUT];

  if (outfr)
    {
      if (mus_is_vct(output))
	{
	  vct *v;
	  mus_float_t *vdata;
	  v = Xen_to_vct(output);
	  vdata = mus_vct_data(v);
	  if (Xen_vector_rank(output) == 1)
	    {
	      if (pos < mus_vct_length(v))
		vdata[pos] += outfr[0];
	    }
#if HAVE_SCHEME
	  else
	    {
	      s7_int chan_len;
	      chan_len = s7_vector_dimension(output, 1); 
	      if (pos < chan_len)
		{
		  int i;
		  for (i = 0; i < chans; i++)
		    vdata[i * chan_len + pos] += outfr[i];
		}
	    }
#endif
	}
      else 
	{
	  if ((Xen_is_vector(output)) &&
	      (pos < Xen_vector_length(output)))
	    Xen_vector_set(output, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(output, pos)) + outfr[0]));
	}
    }
  
  if ((revfr) && 
      (Xen_is_bound(ms->vcts[G_LOCSIG_REVOUT])))
    {
      reverb = ms->vcts[G_LOCSIG_REVOUT];
      if (mus_is_vct(reverb))
	{
	  vct *v;
	  mus_float_t *vdata;
	  v = Xen_to_vct(reverb);
	  vdata = mus_vct_data(v);
	  if (Xen_vector_rank(reverb) == 1)
	    {
	      if (pos < mus_vct_length(v))
		vdata[pos] += revfr[0];
	    }
#if HAVE_SCHEME
	  else
	    {
	      s7_int chan_len;
	      chan_len = s7_vector_dimension(reverb, 1);
	      if (pos < chan_len)
		{
		  int i;
		  for (i = 0; i < rev_chans; i++)
		    vdata[i * chan_len + pos] += revfr[i];
		}
	    }
#endif
	}
      else 
	{
	  if ((Xen_is_vector(reverb)) &&
	      (pos < Xen_vector_length(reverb)))
	    Xen_vector_set(reverb, pos, C_double_to_Xen_real(Xen_real_to_C_double(Xen_vector_ref(reverb, pos)) + revfr[0]));
	}
    }
}


static Xen g_locsig(Xen xobj, Xen xpos, Xen xval)
{
  #define H_locsig "(" S_locsig " gen loc val): add 'val' to the output of locsig at frample 'loc'"
  mus_any *loc_gen;
  mus_xen *ms;
  mus_long_t pos;
  mus_float_t fval;

  ms = (mus_xen *)Xen_object_ref_checked(xobj, mus_xen_tag);
  if (!ms) Xen_check_type(false, xobj, 1, S_locsig, "a locsig generator");
  loc_gen = ms->gen;
  Xen_check_type(mus_is_locsig(loc_gen), xobj, 1, S_locsig, "a locsig generator");

  Xen_check_type(Xen_is_integer(xpos), xpos, 2, S_locsig, "an integer");

  pos = Xen_llong_to_C_llong(xpos);
  if (pos < 0) 
    Xen_out_of_range_error(S_locsig, 2, xpos, "must be >= 0");    

#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(xval), xval, 3, S_locsig, "a number");
  fval = Xen_real_to_C_double(xval);
#else
  fval = s7_number_to_real_with_caller(s7, xval, S_locsig);
#endif

  mus_locsig(loc_gen, pos, fval);

  return(xval);  /* changed 30-June-06 to return val rather than a wrapped frample */
}

static mus_interp_t clm_locsig_type = MUS_INTERP_LINEAR;

static Xen g_locsig_type(void)
{
  #define H_locsig_type "(" S_locsig_type "): locsig interpolation type, either " S_mus_interp_linear " or " S_mus_interp_sinusoidal "."
  return(C_int_to_Xen_integer((int)clm_locsig_type));
}


static Xen g_set_locsig_type(Xen val)
{
  mus_interp_t newval;
  Xen_check_type(Xen_is_integer(val), val, 1, S_locsig_type, S_mus_interp_linear " or " S_mus_interp_sinusoidal);
  newval = (mus_interp_t)Xen_integer_to_C_int(val);
  if ((newval == MUS_INTERP_LINEAR) || (newval == MUS_INTERP_SINUSOIDAL))
    clm_locsig_type = newval;
  return(C_int_to_Xen_integer((int)clm_locsig_type));
}


static void clm_locsig_detour(mus_any *ptr, mus_long_t pos)
{
  mus_xen *ms;
  ms = (mus_xen *)mus_locsig_closure(ptr);
  /* now check for vct/sound-data special cases */
  if (ms->nvcts == 4) 
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, ms->gen, pos, true);
}

#define H_make_locsig "(" S_make_locsig " (degree 0.0) (distance 1.0) (reverb 0.0) output revout channels type): \
 return a new generator for signal placement in n channels.  Channel 0 corresponds to 0 degrees."

#if HAVE_SCHEME
static s7_pointer g_make_locsig(s7_scheme *sc, s7_pointer args)
{
  mus_any *ge, *outp = NULL, *revp = NULL;
  s7_pointer ov, rv, p, fp;
  int out_chans, rev_chans;
  mus_interp_t type;
  mus_float_t degree = 0.0, distance = 1.0, reverb = 0.0;
  type = clm_locsig_type;
  
  fp = s7_car(args);
  if (fp != Xen_false) degree = s7_number_to_real(sc, fp);
  fp = s7_cadr(args);
  if (fp != Xen_false) distance = s7_number_to_real(sc, fp);
  fp = s7_caddr(args);
  if (fp != Xen_false) reverb = s7_number_to_real(sc, fp);

  p = s7_cdddr(args);
  fp = s7_car(p);
  ov = (fp == Xen_false) ? CLM_OUTPUT : fp;
  if ((mus_is_xen(ov)) && 
      (mus_is_output(Xen_to_mus_any(ov))))
    {
      outp = (mus_any *)Xen_to_mus_any(ov);
      out_chans = mus_channels((mus_any *)outp);
    }
  else
    {
      if (s7_is_float_vector(ov))
	{
	  if (s7_vector_rank(ov) > 1)
	    out_chans = s7_vector_dimension(ov, 0);
	  else out_chans = 1;
	}
      else 
	{
	  if (ov != Xen_false)
	    return(s7_wrong_type_arg_error(sc, S_make_locsig, 4, ov, "an output generator or a float-vector"));
	  else ov = s7_undefined(sc);
	  out_chans = 1;
	}
    }

  fp = s7_cadr(p);
  rv = (fp == Xen_false) ? CLM_REVERB : fp;
  if ((mus_is_xen(rv)) && 
      (mus_is_output(Xen_to_mus_any(rv))))
    {
      revp = (mus_any *)Xen_to_mus_any(rv);
      rev_chans = mus_channels((mus_any *)revp);
    }
  else
    {
      if (s7_is_float_vector(rv))
	{
	  if (s7_vector_rank(rv) > 1)
	    rev_chans = s7_vector_dimension(rv, 0);
	  else rev_chans = 1;
	}
      else 
	{
	  if (rv != Xen_false)
	    return(s7_wrong_type_arg_error(sc, S_make_locsig, 5, rv, "an output generator or a float-vector"));
	  else rv = s7_undefined(sc);
	  rev_chans = 0;
	}
    }

  fp = s7_caddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_locsig, 6, fp, "an integer"));
      out_chans = s7_integer(fp);
      if (out_chans <= 0) 
	Xen_out_of_range_error(S_make_locsig, 6, fp, "chans <= 0?");
      if (out_chans > mus_max_table_size()) 
	Xen_out_of_range_error(S_make_locsig, 6, fp, "too many chans");
    }

  fp = s7_cadddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_locsig, 7, fp, "an integer"));
      type = (mus_interp_t)s7_integer(fp);
      if ((type != MUS_INTERP_LINEAR) && (type != MUS_INTERP_SINUSOIDAL))
	Xen_out_of_range_error(S_make_locsig, 7, fp, "type must be " S_mus_interp_linear " or " S_mus_interp_sinusoidal ".");
    }

  ge = mus_make_locsig(degree, distance, reverb, out_chans, outp, rev_chans, revp, type);
  if (ge)
    {
      mus_xen *gn;
      if ((ov != Xen_false) && (rv != Xen_false))
	gn = mx_alloc(4);
      else gn = mx_alloc(2);

      /* these two are for the mus-data and mus-xcoeffs methods = MUS_DATA_WRAPPER and G_FILTER_XCOEFFS */
      if (out_chans > 0)
	gn->vcts[G_LOCSIG_DATA] = xen_make_vct_wrapper(out_chans, mus_data(ge));
      else gn->vcts[G_LOCSIG_DATA] = Xen_undefined;
      if (rev_chans > 0)
	gn->vcts[G_LOCSIG_REVDATA] = xen_make_vct_wrapper(rev_chans, mus_xcoeffs(ge));
      else gn->vcts[G_LOCSIG_REVDATA] = Xen_undefined;

      if (gn->nvcts == 4)
	{
	  mus_locsig_set_detour(ge, clm_locsig_detour);
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  mus_set_environ(ge, (void *)gn);
	}
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(Xen_false);
}
#else
static Xen g_make_locsig(Xen arglist)
{
  mus_any *ge;
  mus_any *outp = NULL, *revp = NULL;
  Xen args[14];
  Xen keys[7];
  Xen ov = Xen_undefined, rv = Xen_undefined;
  Xen keys3 = Xen_undefined, keys4 = Xen_undefined;
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, out_chans = -1, rev_chans = -1;
  mus_interp_t type;
  mus_float_t degree = 0.0, distance = 1.0, reverb = 0.0;

  type = clm_locsig_type;

  keys[0] = kw_degree;
  keys[1] = kw_distance;
  keys[2] = kw_reverb;
  keys[3] = kw_output;  
  keys[4] = kw_revout;
  keys[5] = kw_channels;
  keys[6] = kw_type;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 14) clm_error(S_make_locsig, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_locsig, arglist_len, 7, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      degree = Xen_optkey_to_float(kw_degree, keys[0], S_make_locsig, orig_arg[0], degree);
      distance = Xen_optkey_to_float(kw_distance, keys[1], S_make_locsig, orig_arg[1], distance);
      reverb = Xen_optkey_to_float(kw_reverb, keys[2], S_make_locsig, orig_arg[2], reverb);

      if (!(Xen_is_keyword(keys[3])))
	keys3 = keys[3];

      if (!(Xen_is_keyword(keys[4])))
	keys4 = keys[4];

      if (!(Xen_is_keyword(keys[5])))
	{
	  Xen_check_type(Xen_is_integer(keys[5]), keys[5], orig_arg[5], S_make_locsig, "an integer");
	  out_chans = Xen_integer_to_C_int(keys[5]);
	  if (out_chans < 0) 
	    Xen_out_of_range_error(S_make_locsig, orig_arg[5], keys[5], "chans < 0?");
	  if (out_chans > mus_max_table_size()) 
	    Xen_out_of_range_error(S_make_locsig, orig_arg[5], keys[5], "too many chans");
	}

      type = (mus_interp_t)Xen_optkey_to_int(kw_type, keys[6], S_make_locsig, orig_arg[6], (int)type);
      if ((type != MUS_INTERP_LINEAR) && (type != MUS_INTERP_SINUSOIDAL))
	Xen_out_of_range_error(S_make_locsig, orig_arg[6], keys[6], "type must be " S_mus_interp_linear " or " S_mus_interp_sinusoidal ".");
    }

  if (!Xen_is_bound(keys3))
    keys3 = CLM_OUTPUT;

  if (!Xen_is_bound(keys4))
    keys4 = CLM_REVERB;

  /* try to default output to *output* and reverb to *reverb*, if they're currently set and not closed */
  /*   mus_close is actually mus_close_file = sample_to_file_end = free and nullify obufs so we're hoping dynamic-wind works... */

  if ((mus_is_xen(keys3)) && 
      (mus_is_output(Xen_to_mus_any(keys3))))
    {
      outp = (mus_any *)Xen_to_mus_any(keys3);
      if (out_chans < 0) 
	out_chans = mus_channels((mus_any *)outp);
    }
  else
    {
      if (mus_is_vct(keys3))
	ov = keys3;
      else Xen_check_type(Xen_is_keyword(keys[3]) || Xen_is_false(keys[3]), keys[3], orig_arg[3], S_make_locsig, "an output gen, " S_vct ", vector, or a sound-data object");
#if HAVE_SCHEME
      if ((out_chans < 0) &&
	  (s7_is_vector(ov)) &&
	  (s7_vector_rank(ov) > 1))
	out_chans = s7_vector_dimension(ov, 0);
#endif
    }

  if ((mus_is_xen(keys4)) && 
      (mus_is_output(Xen_to_mus_any(keys4))))
    {
      revp = (mus_any *)Xen_to_mus_any(keys4);
      if (rev_chans < 0)
	rev_chans = mus_channels((mus_any *)revp);
    }
  else
    {
      if (mus_is_vct(keys4))
	{
	  rv = keys4;
	  rev_chans = 1;
#if HAVE_SCHEME
	  if (Xen_vector_rank(rv) > 1)
	    rev_chans = s7_vector_dimension(rv, 0);
#endif
	}
      else Xen_check_type(Xen_is_keyword(keys[4]) || Xen_is_false(keys[4]), keys[4], orig_arg[4], S_make_locsig, "a reverb output generator");
    }

  if (out_chans < 0) out_chans = 1;
  if (rev_chans < 0) rev_chans = 0;

  ge = mus_make_locsig(degree, distance, reverb, out_chans, outp, rev_chans, revp, type);

  if (ge)
    {
      mus_xen *gn;
      if (((Xen_is_bound(ov)) && (!Xen_is_false(ov))) || 
	  ((Xen_is_bound(rv)) && (!Xen_is_false(rv))))
	gn = mx_alloc(4);
      else gn = mx_alloc(2);

      /* these two are for the mus-data and mus-xcoeffs methods in Scheme (etc) = MUS_DATA_WRAPPER and G_FILTER_XCOEFFS */
      if (out_chans > 0)
	gn->vcts[G_LOCSIG_DATA] = xen_make_vct_wrapper(out_chans, mus_data(ge));
      else gn->vcts[G_LOCSIG_DATA] = Xen_undefined;
      if (rev_chans > 0)
	gn->vcts[G_LOCSIG_REVDATA] = xen_make_vct_wrapper(rev_chans, mus_xcoeffs(ge));
      else gn->vcts[G_LOCSIG_REVDATA] = Xen_undefined;

      if (gn->nvcts == 4)
	{
	  mus_locsig_set_detour(ge, clm_locsig_detour);
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  mus_set_environ(ge, (void *)gn);
	}

      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }
  return(Xen_false);
}
#endif


static Xen g_move_locsig(Xen obj, Xen degree, Xen distance)
{
  #define H_move_locsig "(" S_move_locsig " gen degree distance): move locsig gen to reflect degree and distance"
  Xen_check_type((mus_is_xen(obj)) && (mus_is_locsig(Xen_to_mus_any(obj))), obj, 1, S_move_locsig, "a locsig generator");
#if (!HAVE_SCHEME)
  Xen_check_type(Xen_is_number(degree), degree, 2, S_move_locsig, "a number in degrees");
  Xen_check_type(Xen_is_number(distance), distance, 3, S_move_locsig, "a number > 1.0");
  mus_move_locsig(Xen_to_mus_any(obj), Xen_real_to_C_double(degree), Xen_real_to_C_double(distance));
#else
  mus_move_locsig(Xen_to_mus_any(obj), 
		  s7_number_to_real_with_caller(s7, degree, S_move_locsig), 
		  s7_number_to_real_with_caller(s7, distance, S_move_locsig));
#endif
  return(obj);
}




/* ---------------- move-sound ---------------- */

static Xen g_is_move_sound(Xen obj)
{
  #define H_is_move_sound "(" S_is_move_sound " gen): " PROC_TRUE " if gen is a " S_move_sound
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_move_sound(Xen_to_mus_any(obj)))));
}


static Xen g_move_sound(Xen obj, Xen loc, Xen val)
{
  #define H_move_sound "(" S_move_sound " gen loc val): dlocsig run-time generator handling 'val' at sample 'loc'"
  mus_any *move_gen;
  mus_xen *ms;
  mus_long_t pos;
  mus_float_t fval;

  Xen_check_type(mus_is_xen(obj), obj, 1, S_move_sound, "a move-sound generator");
  ms = Xen_to_mus_xen(obj);
  move_gen = (mus_any *)(ms->gen);

  Xen_check_type(mus_is_move_sound(move_gen), obj, 1, S_move_sound, "a move-sound generator");
  Xen_check_type(Xen_is_integer(loc), loc, 2, S_move_sound, "an integer");
  Xen_check_type(Xen_is_number(val), val, 3, S_move_sound, "a number");

  pos = Xen_llong_to_C_llong(loc);
  if (pos < 0) 
    Xen_out_of_range_error(S_move_sound, 2, loc, "must be >= 0");    
  fval = Xen_real_to_C_double(val);

  mus_move_sound(move_gen, pos, fval);
  return(val);
}


static mus_any **xen_vector_to_mus_any_array(Xen vect)
{
  mus_any **gens;
  mus_long_t i, len;

  if (!(Xen_is_vector(vect))) return(NULL);
  len = Xen_vector_length(vect);
  gens = (mus_any **)calloc(len, sizeof(mus_any *));

  for (i = 0; i < len; i++)
    if (mus_is_xen(Xen_vector_ref(vect, i)))
      gens[i] = Xen_to_mus_any(Xen_vector_ref(vect, i));
  return(gens);
}


static int *xen_vector_to_int_array(Xen vect)
{
  int *vals;
  mus_long_t i, len;

  len = Xen_vector_length(vect);
  vals = (int *)calloc(len, sizeof(int));

  for (i = 0; i < len; i++)
    vals[i] = Xen_integer_to_C_int(Xen_vector_ref(vect, i));
  return(vals);
}


static void clm_move_sound_detour(mus_any *ptr, mus_long_t pos)
{
  mus_xen *ms;
  ms = (mus_xen *)mus_move_sound_closure(ptr);
  /* now check for vct/sound-data special cases */
  if (ms->nvcts == 4) 
    mus_locsig_or_move_sound_to_vct_or_sound_data(ms, ms->gen, pos, false);
}


static Xen g_make_move_sound(Xen dloc_list, Xen outp, Xen revp)
{
  Xen ov = Xen_undefined, rv = Xen_undefined;
  mus_any *ge, *dopdly, *dopenv, *globrevenv = NULL, *output = NULL, *revput = NULL;
  mus_any **out_delays, **out_envs, **rev_envs;
  int *out_map;
  mus_long_t start, end;
  int outchans = 0, revchans = 0;
  Xen ref;

  #define H_make_move_sound "(" S_make_move_sound " dloc-list (out *output*) (rev *reverb*)): make a dlocsig run-time generator"

  /* dloc-list is (list start end outchans revchans dopdly dopenv revenv outdelays outenvs revenvs outmap) */
  /*   outdelays envs and revenvs are vectors */

  Xen_check_type(Xen_is_list(dloc_list) && (Xen_list_length(dloc_list) == 11), dloc_list, 1, S_make_move_sound, "a dlocsig list");

  if (!Xen_is_bound(outp))
    outp = CLM_OUTPUT;

  if (!Xen_is_bound(revp))
    revp = CLM_REVERB;

  if (mus_is_xen(outp))
    {
      output = Xen_to_mus_any(outp);
      Xen_check_type(mus_is_output(output), outp, 2, S_make_move_sound, "output stream");
    }
  else
    {
      if ((mus_is_vct(outp)) || 
	  (Xen_is_false(outp)) || 
	  (!Xen_is_bound(outp)))
	ov = outp;
      else Xen_check_type(false, outp, 2, S_make_move_sound, "output stream, " S_vct ", or a sound-data object");
    }

  if (mus_is_xen(revp))
    {
      revput = Xen_to_mus_any(revp);
      Xen_check_type(mus_is_output(revput), revp, 3, S_make_move_sound, "reverb stream");
    }
  else
    {
      if ((mus_is_vct(revp)) || 
	  (Xen_is_false(revp)) || 
	  (!Xen_is_bound(revp)))
	rv = revp;
      else Xen_check_type(false, revp, 3, S_make_move_sound, "reverb stream, " S_vct ", or a sound-data object");
    }

  ref = Xen_list_ref(dloc_list, 0);
  Xen_check_type(Xen_is_llong(ref), ref, 1, S_make_move_sound, "dlocsig list[0] (start): a sample number");
  start = Xen_llong_to_C_llong(ref);

  ref = Xen_list_ref(dloc_list, 1);
  Xen_check_type(Xen_is_llong(ref), ref, 1, S_make_move_sound, "dlocsig list[1] (end): a sample number");
  end = Xen_llong_to_C_llong(ref);

  ref = Xen_list_ref(dloc_list, 2);
  Xen_check_type(Xen_is_integer(ref), ref, 1, S_make_move_sound, "dlocsig list[2] (outchans): an integer");
  outchans = Xen_integer_to_C_int(ref);

  ref = Xen_list_ref(dloc_list, 3);
  Xen_check_type(Xen_is_integer(ref), ref, 1, S_make_move_sound, "dlocsig list[3] (revchans): an integer");
  revchans = Xen_integer_to_C_int(ref);

  ref = Xen_list_ref(dloc_list, 4);
  Xen_check_type(mus_is_xen(ref), ref, 1, S_make_move_sound, "dlocsig list[4] (doppler delay): a delay generator");
  dopdly = Xen_to_mus_any(ref);
  Xen_check_type(mus_is_delay(dopdly), ref, 1, S_make_move_sound, "dlocsig list[4] (doppler delay): a delay generator");

  ref = Xen_list_ref(dloc_list, 5);
  Xen_check_type(mus_is_xen(ref), ref, 1, S_make_move_sound, "dlocsig list[5] (doppler env): an env generator");
  dopenv = Xen_to_mus_any(ref);
  Xen_check_type(mus_is_env(dopenv), ref, 1, S_make_move_sound, "dlocsig list[5] (doppler env): an env generator");

  ref = Xen_list_ref(dloc_list, 6);
  Xen_check_type(Xen_is_false(ref) || mus_is_xen(ref), ref, 1, S_make_move_sound, "dlocsig list[6] (global rev env): an env generator");
  if (mus_is_xen(ref))
    {
      globrevenv = Xen_to_mus_any(ref);
      Xen_check_type(mus_is_env(globrevenv), ref, 1, S_make_move_sound, "dlocsig list[6] (global rev env): an env generator");
    }

  ref = Xen_list_ref(dloc_list, 7);
  Xen_check_type(Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= outchans), 
		  ref, 1, S_make_move_sound, "dlocsig list[7] (out delays): a vector of delay gens");

  ref = Xen_list_ref(dloc_list, 8);
  Xen_check_type(Xen_is_false(ref) || (Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= outchans)), 
		  ref, 1, S_make_move_sound, "dlocsig list[8] (out envs): " PROC_FALSE " or a vector of envs");

  ref = Xen_list_ref(dloc_list, 9);
  Xen_check_type(Xen_is_false(ref) || (Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= revchans)), 
		  ref, 1, S_make_move_sound, "dlocsig list[9] (rev envs): " PROC_FALSE " or a vector of envs");

  ref = Xen_list_ref(dloc_list, 10);
  Xen_check_type(Xen_is_vector(ref) && ((int)Xen_vector_length(ref) >= outchans), 
		  ref, 1, S_make_move_sound, "dlocsig list[10] (out map): vector of ints");

  /* put off allocation until all type error checks are done */

  out_delays = xen_vector_to_mus_any_array(Xen_list_ref(dloc_list, 7));
  out_envs = xen_vector_to_mus_any_array(Xen_list_ref(dloc_list, 8));
  rev_envs = xen_vector_to_mus_any_array(Xen_list_ref(dloc_list, 9));
  out_map = xen_vector_to_int_array(Xen_list_ref(dloc_list, 10));

  ge = mus_make_move_sound(start, end, outchans, revchans,
			   dopdly, dopenv, globrevenv,
			   out_delays, out_envs, rev_envs, out_map,
			   output, revput,
			   true, false);                  /* free outer arrays but not gens */
  if (ge)
    {
      mus_xen *gn;
      if (((Xen_is_bound(ov)) && (!Xen_is_false(ov))) || 
	  ((Xen_is_bound(rv)) && (!Xen_is_false(rv))))
	gn = mx_alloc(4);
      else gn = mx_alloc(1);
      gn->vcts[G_LOCSIG_DATA] = dloc_list; /* it is crucial that the list be gc-protected! */
      if (gn->nvcts == 4)
	{
	  mus_move_sound_set_detour(ge, clm_move_sound_detour);
	  gn->vcts[G_LOCSIG_OUT] = ov;
	  gn->vcts[G_LOCSIG_REVOUT] = rv;
	  gn->vcts[G_LOCSIG_REVDATA] = Xen_undefined;
	  mus_set_environ(ge, (void *)gn);
	}
      gn->gen = ge;
      return(mus_xen_to_object(gn));
    }

  return(Xen_false);
}




/* ---------------- src ---------------- */

static Xen xen_one, xen_minus_one;

#if HAVE_SCHEME
static Xen as_needed_arglist_one, as_needed_arglist_minus_one;

static mus_float_t as_needed_input_float(void *ptr, int direction)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(s7_real(gn->vcts[MUS_INPUT_DATA]));
}

static mus_float_t as_needed_block_input_float(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
  mus_xen *gn = (mus_xen *)ptr;
  mus_float_t val;
  mus_long_t i, lim4;

  lim4 = end - 4;
  val = (mus_float_t)s7_real(gn->vcts[MUS_INPUT_DATA]); /* set in the chooser below */

  for (i = start; i <= lim4;)
    {
      data[i++] = val;
      data[i++] = val;
      data[i++] = val;
      data[i++] = val;
    }
  for (;i < end; i++)
    data[i] = val;
  return(val);
}


static mus_float_t as_needed_input_any(void *ptr, int direction)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(s7_number_to_real(s7, s7_apply_function(s7, gn->vcts[MUS_INPUT_FUNCTION], (direction == 1) ? as_needed_arglist_one : as_needed_arglist_minus_one)));
}
#endif


static mus_float_t as_needed_input_generator(void *ptr, int direction)
{
#if HAVE_EXTENSION_LANGUAGE
  return(mus_apply((mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]), 0.0, 0.0));
#else
  return(0.0);
#endif
}

static mus_float_t as_needed_block_input_generator(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
#if HAVE_EXTENSION_LANGUAGE
  mus_any *g;
  mus_long_t i;
  g = (mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = mus_apply(g, 0.0, 0.0);
#endif
  return(0.0);
}


static mus_float_t as_needed_input_readin(void *ptr, int direction)
{
#if HAVE_EXTENSION_LANGUAGE
  return(mus_readin((mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA])));
#else
  return(0.0);
#endif
}

static mus_float_t as_needed_block_input_readin(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
#if HAVE_EXTENSION_LANGUAGE
  mus_any *g;
  mus_long_t i;
  g = (mus_any *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = mus_readin(g);
#endif
  return(0.0);
}


#if USE_SND && HAVE_SCHEME
static mus_float_t as_needed_input_sampler(void *ptr, int direction)
{
  return(read_sample((snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA])));
}

static mus_float_t as_needed_block_input_sampler(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
  snd_fd *p;
  mus_long_t i;
  p = (snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = read_sample(p);
  return(0.0);
}


mus_float_t read_sample_with_direction(void *p, int dir);
static mus_float_t as_needed_input_sampler_with_direction(void *ptr, int direction)
{
  return(read_sample_with_direction((snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]), direction));
}

static mus_float_t as_needed_block_input_sampler_with_direction(void *ptr, int direction, mus_float_t *data, mus_long_t start, mus_long_t end)
{
  snd_fd *p;
  mus_long_t i;
  p = (snd_fd *)(((mus_xen *)ptr)->vcts[MUS_INPUT_DATA]);
  for (i = start; i < end; i++)
    data[i] = read_sample_with_direction(p, direction);
  return(0.0);
}
#endif


static mus_float_t as_needed_input_func(void *ptr, int direction) /* intended for "as-needed" input funcs */
{
  mus_xen *gn = (mus_xen *)ptr;
  if (gn)
    {
      Xen in_obj;
      in_obj = gn->vcts[MUS_INPUT_FUNCTION];
      if (Xen_is_procedure(in_obj))
	return(Xen_real_to_C_double(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_INPUT_FUNCTION], (direction == 1) ? xen_one : xen_minus_one)));
    }
  return(0.0);
}

static void set_as_needed_input_choices(mus_any *gen, Xen obj, mus_xen *gn)
{
  /* fprintf(stderr, "set_as_needed_input for %s: %s\n", mus_name(gen), DISPLAY(obj)); */
  if (mus_is_xen(obj)) /* input function is a generator */
    {
      mus_any *p;
      p = Xen_to_mus_any(obj);
      if (p) 
	{
#if HAVE_EXTENSION_LANGUAGE
	  gn->vcts[MUS_INPUT_DATA] = (Xen)p; 
#endif
	  if (mus_is_readin(p))
	    mus_generator_set_feeders(gen, as_needed_input_readin, as_needed_block_input_readin);
	  else mus_generator_set_feeders(gen, as_needed_input_generator, as_needed_block_input_generator);
	  return;
	}
    }

#if HAVE_SCHEME
  if ((Xen_is_procedure(obj)) &&
      (!Xen_is_procedure(gn->vcts[MUS_ANALYZE_FUNCTION]))) /* this assumes scheme-ready input function at least in phase-vocoder case */
    {
      s7_pointer body;
      body = s7_closure_body(s7, obj);
      if (s7_is_pair(body))
	{
	  if (s7_is_null(s7, s7_cdr(body)))
	    {
	      s7_pointer res;
	      res = s7_car(body);
	      if (s7_is_real(res))
		{
		  gn->vcts[MUS_INPUT_DATA] = res;
		  mus_generator_set_feeders(gen, as_needed_input_float, as_needed_block_input_float);
		  return;
		}
	      if (s7_is_pair(res))
		{
#if USE_SND
		  {
		    s7_pointer arg;
		    arg = s7_car(s7_closure_args(s7, obj));
		    if ((s7_is_pair(s7_cddr(res))) &&
			(arg == s7_caddr(res)) &&
			(s7_car(res) == s7_make_symbol(s7, "read-sample-with-direction")))
		      {
			gn->vcts[MUS_INPUT_DATA] = (Xen)xen_to_sampler(s7_symbol_local_value(s7, s7_cadr(res), s7_closure_let(s7, obj)));
			mus_generator_set_feeders(gen, as_needed_input_sampler_with_direction, as_needed_block_input_sampler_with_direction);
			return;
		      }
		  }
#endif
		}
	    }
	}
#if USE_SND
      /* check for a sampler (snd-edits.c) */
      if (is_sampler(obj))
	{
	  gn->vcts[MUS_INPUT_DATA] = (Xen)xen_to_sampler(obj);
	  mus_generator_set_feeders(gen, as_needed_input_sampler, as_needed_block_input_sampler);
	  return;
	}
      mus_generator_set_feeders(gen, as_needed_input_any, NULL);
      return;
#endif	  
    }
#endif
  mus_generator_set_feeders(gen, as_needed_input_func, NULL);
}


static Xen g_mus_clear_sincs(void)
{
  mus_clear_sinc_tables();
  return(Xen_false);
}


static Xen g_is_src(Xen obj) 
{
  #define H_is_src "(" S_is_src " gen): " PROC_TRUE " if gen is an " S_src
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_src(Xen_to_mus_any(obj)))));
}


#define SRC_CHANGE_MAX 1000000.0

static Xen g_src(Xen obj, Xen pm, Xen func) 
{
  #define H_src "(" S_src " gen (pm 0.0) input-function): next sampling rate conversion sample. \
'pm' can be used to change the sampling rate on a sample-by-sample basis.  'input-function' \
is a function of one argument (the current input direction, normally ignored) that is called \
internally whenever a new sample of input data is needed.  If the associated " S_make_src " \
included an 'input' argument, input-function is ignored."

  mus_float_t pm1 = 0.0;
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_src, S_src, "an src generator");
  Xen_real_to_C_double_if_bound(pm, pm1, S_src, 2);

  /* if sr_change (pm1) is ridiculous, complain! */
  if ((pm1 > SRC_CHANGE_MAX) || (pm1 < -SRC_CHANGE_MAX))
    Xen_out_of_range_error(S_src, 2, pm, "src change too large");

  if (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA]))
    {
      if (Xen_is_procedure(func))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else Xen_bad_arity_error(S_src, 3, func, "src input function wants 1 arg");
	}
    }
  return(C_double_to_Xen_real(mus_src(g, pm1, NULL)));
}

static void set_gn_gen(void *p, mus_any *g)
{
  mus_xen *gn = (mus_xen *)p;
  gn->gen = g;
}

#define H_make_src "(" S_make_src " input (srate 1.0) (width 10)): \
return a new sampling-rate conversion generator (using 'warped sinc interpolation'). \
'srate' is the ratio between the new rate and the old. 'width' is the sine \
width (effectively the steepness of the low-pass filter), normally between 10 and 100. \
'input' if given is an open file stream."

#if HAVE_SCHEME
static s7_pointer g_make_src(s7_scheme *sc, s7_pointer args)
{
  s7_pointer in_obj, p;
  mus_xen *gn;
  mus_any *ge;
  mus_float_t srate;
  int wid = 10;
  
  in_obj = s7_car(args); /* input proc 1 arg */
  if (s7_is_procedure(in_obj))
    {
      if (!s7_is_aritable(sc, in_obj, 1))
	return(s7_wrong_type_arg_error(sc, S_make_src, 1, in_obj, "a function of one argument"));
    }
  else
    {
      if (mus_is_xen(in_obj))
	{
	  if (!mus_is_input(Xen_to_mus_any(in_obj)))
	    return(s7_wrong_type_arg_error(sc, S_make_src, 1, in_obj, "an input generator"));
	}
      else 
	{
	  if (in_obj == Xen_false)
	    in_obj = s7_undefined(sc);
	  else return(s7_wrong_type_arg_error(sc, S_make_src, 1, in_obj, "a procedure or an input generator"));
	}
    }

  p = s7_caddr(args);
  if (p != Xen_false)
    {
      if (!s7_is_integer(p))
	return(s7_wrong_type_arg_error(sc, S_make_src, 3, p, "an integer"));
      wid = s7_integer(p);
      if (wid < 0) 
	Xen_out_of_range_error(S_make_src, 3, p, "width < 0?");
      if (wid > 2000) 
	Xen_out_of_range_error(S_make_src, 3, p, "width > 2000?");
    }
  srate = s7_number_to_real(sc, s7_cadr(args));
  
  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  /* mus_make_src assumes it can invoke the input function! */
  gn->vcts[MUS_INPUT_FUNCTION] = in_obj;

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_src_with_init(NULL, srate, wid, gn, set_gn_gen);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      Xen src_obj;
#if HAVE_SCHEME
      uint32_t loc;
#endif
      gn->gen = ge;
      src_obj = mus_xen_to_object(gn);
#if HAVE_SCHEME
      loc = s7_gc_protect(s7, src_obj);
#endif
      /* src_init can call an input function which can trigger the GC, so we need to GC-protect the new object */
      gn->vcts[MUS_SELF_WRAPPER] = src_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      mus_src_init(ge);
#if HAVE_SCHEME
      s7_gc_unprotect_at(s7, loc);
#endif
      return(src_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_src));
}
#else
static Xen g_make_src(Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6)
{
  Xen in_obj = Xen_undefined;
  mus_xen *gn;
  mus_any *ge = NULL;
  int vals, pr = 0, wid = 0; /* 0 here picks up the current default width in clm.c */
  Xen args[6]; 
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  mus_float_t srate = 1.0;

  keys[0] = kw_input;
  keys[1] = kw_srate;
  keys[2] = kw_width;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; 
	if (Aok(arg4)) {args[pr++] = arg4; if (Aok(arg5)) {args[pr++] = arg5; if (Aok(arg6)) {args[pr++] = arg6; }}}}}}
  vals = mus_optkey_unscramble(S_make_src, pr, 3, keys, args, orig_arg);
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_src, orig_arg[0], Xen_undefined, 1, "src input procedure takes 1 arg");

      srate = Xen_optkey_to_float(kw_srate, keys[1], S_make_src, orig_arg[1], srate);
      /* srate can be negative => read in reverse */

      wid = Xen_optkey_to_int(kw_width, keys[2], S_make_src, orig_arg[2], wid);
      if (wid < 0) 
	Xen_out_of_range_error(S_make_src, orig_arg[2], keys[2], "width < 0?");
      if (wid > 2000) 
	Xen_out_of_range_error(S_make_src, orig_arg[2], keys[2], "width > 2000?");
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  /* mus_make_src assumes it can invoke the input function! */
  gn->vcts[MUS_INPUT_FUNCTION] = in_obj;

  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_src_with_init(NULL, srate, wid, gn, set_gn_gen);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      Xen src_obj;
#if HAVE_SCHEME
      s7_int loc;
#endif

      gn->gen = ge;
      src_obj = mus_xen_to_object(gn);
#if HAVE_SCHEME
      loc = s7_gc_protect(s7, src_obj);
#endif
      /* src_init can call an input function which can trigger the GC, so we need to GC-protect the new object */

      gn->vcts[MUS_SELF_WRAPPER] = src_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      mus_src_init(ge);
#if HAVE_SCHEME
      s7_gc_unprotect_at(s7, loc);
#endif
      return(src_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_src));
}
#endif



/* ---------------- granulate ---------------- */

static Xen g_is_granulate(Xen obj) 
{
  #define H_is_granulate "(" S_is_granulate " gen): " PROC_TRUE " if gen is a " S_granulate " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_granulate(Xen_to_mus_any(obj)))));
}


static int grnedit(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(Xen_integer_to_C_int(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_EDIT_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static Xen g_granulate(Xen obj, Xen func, Xen edit_func) 
{
  #define H_granulate "(" S_granulate " gen input-func edit-func): next sample from granular synthesis generator"
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_granulate, S_granulate, "a granulate generator");

  if ((Xen_is_bound(func)) &&
      (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA])))
    {
      if (Xen_is_procedure(func))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else Xen_bad_arity_error(S_granulate, 2, func, "granulate input function wants 1 arg");
	}
      if (Xen_is_procedure(edit_func))
	{
	  if (Xen_is_aritable(edit_func, 1))
	    {
	      if (!(Xen_is_bound(gn->vcts[MUS_EDIT_FUNCTION]))) /* default value is Xen_undefined */
		{
		  mus_granulate_set_edit_function(gn->gen, grnedit);
		  gn->vcts[MUS_EDIT_FUNCTION] = edit_func;
		}
	    }
	  else Xen_bad_arity_error(S_granulate, 3, edit_func, "granulate edit function wants 1 arg");
	}
    }
  return(C_double_to_Xen_real(mus_granulate(g, NULL)));
}


#define H_make_granulate "(" S_make_granulate " input (expansion 1.0) (length .15) (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size edit): \
 return a new granular synthesis generator.  'length' is the grain length (seconds), 'expansion' is the ratio in timing \
 between the new and old (expansion > 1.0 slows things down), 'scaler' scales the grains \
 to avoid overflows, 'hop' is the spacing (seconds) between successive grains upon output. \
 'jitter' controls the randomness in that spacing, 'input' can be a file pointer. 'edit' can \
 be a function of one arg, the current granulate generator.  It is called just before \
 a grain is added into the output buffer. The current grain is accessible via " S_mus_data ". \
 The edit function, if any, should return the length in samples of the grain, or 0."

#if HAVE_SCHEME
static s7_pointer g_make_granulate(s7_scheme *sc, s7_pointer args)
{
  s7_pointer in_obj, edit_obj, grn_obj, p, fp;
  mus_xen *gn;
  mus_any *ge;

  mus_float_t expansion = 1.0, segment_length = .15, segment_scaler = .6, ramp_time = .4, output_hop = .05, jitter = 1.0;
  int maxsize = 0;

  in_obj = s7_car(args); /* input proc 1 arg */
  if (s7_is_procedure(in_obj))
    {
      if (!s7_is_aritable(sc, in_obj, 1))
	return(s7_wrong_type_arg_error(sc, S_make_granulate, 1, in_obj, "a function of one argument"));
    }
  else
    {
      if (mus_is_xen(in_obj))
	{
	  if (!mus_is_input(Xen_to_mus_any(in_obj)))
	    return(s7_wrong_type_arg_error(sc, S_make_granulate, 1, in_obj, "an input generator"));
	}
      else 
	{
	  if (in_obj == Xen_false)
	    in_obj = s7_undefined(sc);
	  else return(s7_wrong_type_arg_error(sc, S_make_granulate, 1, in_obj, "a procedure or an input generator"));
	}
    }

  fp = s7_cadr(args);
  if (fp != Xen_false)
    {
      expansion = s7_number_to_real(sc, fp);
      if (expansion <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, 2, fp, "expansion <= 0.0?");
    }
  p = s7_cddr(args);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      segment_length = s7_number_to_real(sc, fp);
      if (segment_length <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, 3, fp, "segment-length <= 0.0?");
    }

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      segment_scaler = s7_number_to_real(sc, fp);
      if (segment_scaler == 0.0) 
	Xen_out_of_range_error(S_make_granulate, 4, fp, "segment-scaler should be greater than 0.0?");
    }

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      output_hop = s7_number_to_real(sc, fp);
      if (output_hop <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, 5, fp, "hop <= 0?");
      if (output_hop > 3600.0) 
	Xen_out_of_range_error(S_make_granulate, 5, fp, "hop > 3600?");
      if ((segment_length + output_hop) > 60.0) /* multiplied by srate in mus_make_granulate in array allocation */
	Xen_out_of_range_error(S_make_granulate, 5, fp, "segment_length + output_hop too large!");
    }

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      ramp_time = s7_number_to_real(sc, fp);
      if ((ramp_time < 0.0) || (ramp_time > 0.5))
	Xen_out_of_range_error(S_make_granulate, 6, fp, "ramp must be between 0.0 and 0.5");
    }

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      jitter = s7_number_to_real(sc, fp);
      if ((jitter < 0.0) || (jitter > 100.0))
	Xen_out_of_range_error(S_make_granulate, 7, fp, "jitter must be between 0.0 and 100.0");
    }

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_granulate, 8, fp, "an integer"));
      maxsize = s7_integer(fp);
      if ((maxsize > mus_max_malloc()) || (maxsize <= 0))
	Xen_out_of_range_error(S_make_granulate, 8, fp, "max-size invalid");
    }

  edit_obj = s7_caddr(p);
  if (edit_obj != Xen_false)
    {
      if (s7_is_procedure(edit_obj))
	{
	  if (!s7_is_aritable(sc, edit_obj, 1))
	    return(s7_wrong_type_arg_error(sc, S_make_granulate, 9, edit_obj, "a function of one argument"));
	}
      else return(s7_wrong_type_arg_error(sc, S_make_granulate, 9, edit_obj, "a procedure"));
    }
  else edit_obj = s7_undefined(sc);

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_granulate(NULL, 
			    expansion, segment_length, segment_scaler, output_hop, ramp_time, jitter, maxsize, 
			    (!Xen_is_bound(edit_obj) ? NULL : grnedit),
			    (void *)gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_granulate_grain_max_length(ge), mus_data(ge));
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->gen = ge;
      grn_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = grn_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(grn_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_granulate));
}
#else
static Xen g_make_granulate(Xen arglist)
{
  Xen in_obj = Xen_undefined;
  mus_xen *gn;
  mus_any *ge;
  Xen args[18];
  Xen keys[9];
  int orig_arg[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  int vals, maxsize = 0;
  mus_float_t expansion = 1.0, segment_length = .15, segment_scaler = .6, ramp_time = .4, output_hop = .05;
  mus_float_t jitter = 1.0;
  Xen edit_obj = Xen_undefined, grn_obj;

  keys[0] = kw_input;
  keys[1] = kw_expansion;
  keys[2] = kw_length;
  keys[3] = kw_scaler;
  keys[4] = kw_hop;
  keys[5] = kw_ramp;
  keys[6] = kw_jitter;
  keys[7] = kw_max_size;
  keys[8] = kw_edit;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 18) clm_error(S_make_granulate, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_granulate, arglist_len, 9, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_granulate, orig_arg[0], Xen_undefined, 1, "granulate input procedure takes 1 arg");

      expansion = Xen_optkey_to_float(kw_expansion, keys[1], S_make_granulate, orig_arg[1], expansion);
      if (expansion <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[1], keys[1], "expansion <= 0.0?");

      segment_length = Xen_optkey_to_float(kw_length, keys[2], S_make_granulate, orig_arg[2], segment_length);
      if (segment_length <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[2], keys[2], "segment-length <= 0.0?");

      segment_scaler = Xen_optkey_to_float(kw_scaler, keys[3], S_make_granulate, orig_arg[3], segment_scaler);
      if (segment_scaler == 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[3], keys[3], "segment-scaler should be greater than 0.0?");

      output_hop = Xen_optkey_to_float(kw_hop, keys[4], S_make_granulate, orig_arg[4], output_hop);
      if (output_hop <= 0.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[4], keys[4], "hop <= 0?");
      if (output_hop > 3600.0) 
	Xen_out_of_range_error(S_make_granulate, orig_arg[4], keys[4], "hop > 3600?");
      if ((segment_length + output_hop) > 60.0) /* multiplied by srate in mus_make_granulate in array allocation */
	Xen_out_of_range_error(S_make_granulate, orig_arg[2], Xen_list_2(keys[2], keys[4]), "segment_length + output_hop too large!");

      ramp_time = Xen_optkey_to_float(kw_ramp, keys[5], S_make_granulate, orig_arg[5], ramp_time);
      if ((ramp_time < 0.0) || (ramp_time > 0.5))
	Xen_out_of_range_error(S_make_granulate, orig_arg[5], keys[5], "ramp must be between 0.0 and 0.5");

      jitter = Xen_optkey_to_float(kw_jitter, keys[6], S_make_granulate, orig_arg[6], jitter);
      Xen_check_type((jitter >= 0.0) && (jitter < 100.0), keys[6], orig_arg[6], S_make_granulate, "0.0 .. 100.0");

      maxsize = Xen_optkey_to_int(kw_max_size, keys[7], S_make_granulate, orig_arg[7], maxsize);
      if ((maxsize > mus_max_malloc()) || 
	  (maxsize < 0) ||
	  ((maxsize == 0) && (!Xen_is_keyword(keys[7]))))
	Xen_out_of_range_error(S_make_granulate, orig_arg[7], keys[7], "max-size invalid");

      edit_obj = mus_optkey_to_procedure(keys[8], S_make_granulate, orig_arg[8], Xen_undefined, 1, "granulate edit procedure takes 1 arg");
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_granulate(NULL, 
			    expansion, segment_length, segment_scaler, output_hop, ramp_time, jitter, maxsize, 
			    (!Xen_is_bound(edit_obj) ? NULL : grnedit),
			    (void *)gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->vcts[MUS_DATA_WRAPPER] = xen_make_vct_wrapper(mus_granulate_grain_max_length(ge), mus_data(ge));
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->gen = ge;
      grn_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = grn_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(grn_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_granulate));
}
#endif


/* ---------------- convolve ---------------- */

static Xen g_is_convolve(Xen obj) 
{
  #define H_is_convolve "(" S_is_convolve " gen): " PROC_TRUE " if gen is a " S_convolve " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_convolve(Xen_to_mus_any(obj)))));
}


static Xen g_convolve(Xen obj, Xen func) 
{
  #define H_convolve_gen "(" S_convolve " gen input-func): next sample from convolution generator"
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_convolve, S_convolve, "a convolve generator");

  if (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA]))
    {
      if (Xen_is_procedure(func))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func;
	  else Xen_bad_arity_error(S_convolve, 2, func, "convolve input function wants 1 arg");
	}
    }
  return(C_double_to_Xen_real(mus_convolve(g, NULL)));
}


/* filter-size? */

#define H_make_convolve "(" S_make_convolve " input filter fft-size): \
return a new convolution generator which convolves its input with the impulse response 'filter'."

#if HAVE_SCHEME
static s7_pointer g_make_convolve(s7_scheme *sc, s7_pointer args)
{
  mus_xen *gn;
  mus_any *ge;
  s7_pointer filter, in_obj, fs;
  mus_long_t fft_size;
  
  filter = s7_cadr(args);
  if (!s7_is_float_vector(filter))
    return(s7_wrong_type_arg_error(sc, S_make_convolve, 2, filter, "a float-vector"));
  
  in_obj = s7_car(args); /* input proc 1 arg */
  if (s7_is_procedure(in_obj))
    {
      if (!s7_is_aritable(sc, in_obj, 1))
	return(s7_wrong_type_arg_error(sc, S_make_convolve, 1, in_obj, "a function of one argument"));
    }
  else
    {
      if (mus_is_xen(in_obj))
	{
	  if (!mus_is_input(Xen_to_mus_any(in_obj)))
	    return(s7_wrong_type_arg_error(sc, S_make_convolve, 1, in_obj, "an input generator"));
	}
      else 
	{
	  if (in_obj == Xen_false)
	    in_obj = s7_undefined(sc);
	  else return(s7_wrong_type_arg_error(sc, S_make_convolve, 1, in_obj, "a procedure or an input generator"));
	}
    }

  fs = s7_caddr(args);
  if (fs != Xen_false)
    {
      if (!s7_is_integer(fs))
	return(s7_wrong_type_arg_error(sc, S_make_convolve, 3, fs, "an integer"));
      fft_size = s7_integer(fs);
      if ((fft_size  <= 0) || 
	  (fft_size > mus_max_malloc()))
	Xen_out_of_range_error(S_make_convolve, 3, fs, "fft-size invalid (see mus-max-malloc))");
    }
  else 
    {
      fft_size = s7_vector_length(filter);
      if (is_power_of_2(fft_size))
	fft_size *= 2;
      else fft_size = (mus_long_t)pow(2.0, 1 + (int)(log((mus_float_t)(fft_size + 1)) / log(2.0)));
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  ge = mus_make_convolve(NULL, s7_float_vector_elements(filter), fft_size, s7_vector_length(filter), gn);
  if (ge)
    {
      Xen c_obj;
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = filter; /* why is this here? GC protection? (might be a locally-allocated vct as from file->vct) */
      gn->gen = ge;
      c_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = c_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(c_obj);
    }

  free(gn->vcts);
  free(gn);
  return(Xen_false);
}
#else
static Xen g_make_convolve(Xen arglist)
{
  mus_xen *gn;
  mus_any *ge;
  Xen args[6];
  Xen keys[3];
  int orig_arg[3] = {0, 0, 0};
  int vals;
  vct *filter = NULL;
  Xen filt = Xen_undefined, in_obj = Xen_undefined;
  mus_long_t fftlen, fft_size = 0;

  keys[0] = kw_input;
  keys[1] = kw_filter;
  keys[2] = kw_fft_size;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 6) clm_error(S_make_convolve, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_convolve, arglist_len, 3, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_convolve, orig_arg[0], Xen_undefined, 1, "convolve input procedure takes 1 arg");

      filter = mus_optkey_to_vct(keys[1], S_make_convolve, orig_arg[1], NULL);
      if (filter) filt = keys[1];

      fft_size = Xen_optkey_to_mus_long_t(kw_fft_size, keys[2], S_make_convolve, orig_arg[2], fft_size);
      if ((fft_size  < 0) || 
	  ((fft_size == 0) && (!Xen_is_keyword(keys[2]))) ||
	  (fft_size > mus_max_malloc()))
	Xen_out_of_range_error(S_make_convolve, orig_arg[2], keys[2], "fft-size invalid (see mus-max-malloc))");
    }

  if (!filter)
    Xen_error(NO_DATA,
	      Xen_list_1(C_string_to_Xen_string(S_make_convolve ": no impulse (filter)?")));

  if (is_power_of_2(mus_vct_length(filter)))
    fftlen = mus_vct_length(filter) * 2;
  else fftlen = (mus_long_t)pow(2.0, 1 + (int)(log((mus_float_t)(mus_vct_length(filter) + 1)) / log(2.0)));
  if (fft_size < fftlen) fft_size = fftlen;

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_convolve(NULL, mus_vct_data(filter), fft_size, mus_vct_length(filter), gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      Xen c_obj;
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = filt; /* why is this here? GC protection? (might be a locally-allocated vct as from file->vct) */
      gn->gen = ge;
      c_obj = mus_xen_to_object(gn);
      gn->vcts[MUS_SELF_WRAPPER] = c_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(c_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_convolve));
}
#endif


static Xen g_convolve_files(Xen file1, Xen file2, Xen maxamp, Xen outfile)
{
  #define H_convolve_files "(" S_convolve_files " file1 file2 maxamp output-file): convolve \
file1 and file2 writing outfile after scaling the convolution result to maxamp."

  const char *f1, *f2, *f3;
  mus_float_t maxval = 1.0;

  Xen_check_type(Xen_is_string(file1), file1, 1, S_convolve_files, "a string");
  Xen_check_type(Xen_is_string(file2), file2, 2, S_convolve_files, "a string");
  Xen_check_type(Xen_is_number_or_unbound(maxamp), maxamp, 3, S_convolve_files, "a number");
  Xen_check_type((!Xen_is_bound(outfile)) || (Xen_is_string(outfile)), outfile, 4, S_convolve_files, "a string");

  f1 = Xen_string_to_C_string(file1);
  f2 = Xen_string_to_C_string(file2);
  if (Xen_is_string(outfile)) 
    f3 = Xen_string_to_C_string(outfile); 
  else f3 = "tmp.snd";
  if (Xen_is_number(maxamp)) 
    maxval = Xen_real_to_C_double(maxamp);

  mus_convolve_files(f1, f2, maxval, f3);
  return(C_string_to_Xen_string(f3));
}




/* ---------------- phase-vocoder ---------------- */

/* pvedit pvanalyze pvsynthesize:
 * these three functions provide a path for the call (clm.c) (*(pv->edit))(pv->closure)
 *   which is calling a user-supplied edit function within the particular phase-vocoder
 *   generator's context.  "closure" is an uninterpreted void pointer passed in by the
 *   user, and passed here as the edit function argument.  In this file, pv->edit is
 *   &pvedit, and (void *)ptr is closure; in make_phase_vocoder we set closure to be
 *   the mus_xen object that shadows the phase-vocoder generator, with two special
 *   pointers in the vcts field: vcts[MUS_EDIT_FUNCTION] is the (Scheme-side) function
 *   passed by the user, and vcts[MUS_SELF_WRAPPER] is a pointer to the (Scheme-relevant)
 *   object that packages the mus_xen pointer for Scheme.  This way, the user's
 *    (make-phase-vocoder ... (lambda (v) (mus-length v)) ...)
 *   treats v as the current pv gen, vcts[MUS_SELF_WRAPPER] = v, vcts[MUS_EDIT_FUNCTION] = 
 *   the lambda form, mus_xen obj->gen is the C-side pv struct pointer.  See above
 *   under as_needed_input_func for more verbiage.  (All this complication arises because clm.c
 *   is pure C -- no notion that Scheme might be the caller, and the user's pv.scm
 *   or whatever is pure Scheme -- no notion that C is actually doing the work,
 *   and we have to tie everything together here including the Scheme-C-Scheme-C 
 *   call chains).
 */

static int pvedit(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(Xen_boolean_to_C_bool(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_EDIT_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static mus_float_t pvsynthesize(void *ptr)
{
  mus_xen *gn = (mus_xen *)ptr;
  return(Xen_real_to_C_double(Xen_unprotected_call_with_1_arg(gn->vcts[MUS_SYNTHESIZE_FUNCTION], gn->vcts[MUS_SELF_WRAPPER])));
}


static bool pvanalyze(void *ptr, mus_float_t (*input)(void *arg1, int direction))
{
  mus_xen *gn = (mus_xen *)ptr;
  /* we can only get input func if it's already set up by the outer gen call, so (?) we can use that function here.
   *   but the gc might be called during this call, and scan the args, so the input function should be
   *   in the arg list only if its a legit pointer?
   */
  return(Xen_boolean_to_C_bool(Xen_unprotected_call_with_2_args(gn->vcts[MUS_ANALYZE_FUNCTION], 
								gn->vcts[MUS_SELF_WRAPPER], 
								gn->vcts[MUS_INPUT_FUNCTION])));
}


static Xen g_is_phase_vocoder(Xen obj) 
{
  #define H_is_phase_vocoder "(" S_is_phase_vocoder " gen): " PROC_TRUE " if gen is an " S_phase_vocoder
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_phase_vocoder(Xen_to_mus_any(obj)))));
}


static Xen g_phase_vocoder(Xen obj, Xen func, Xen analyze_func, Xen edit_func, Xen synthesize_func)
{
  #define H_phase_vocoder "(" S_phase_vocoder " gen input-function analyze-func edit-func synthesize-func): next phase vocoder value"
  mus_xen *gn;
  mus_any *g = NULL;

  Xen_to_C_generator(obj, gn, g, mus_is_phase_vocoder, S_phase_vocoder, "a phase-vocoder generator");

  if (Xen_is_bound(func))
    {
      bool (*analyze)(void *arg, mus_float_t (*input)(void *arg1, int direction)) = NULL;
      int (*edit)(void *arg) = NULL;
      mus_float_t (*synthesize)(void *arg) = NULL;

      if ((Xen_is_procedure(func)) &&
	  (!Xen_is_bound(gn->vcts[MUS_INPUT_DATA])))
	{
	  if (Xen_is_aritable(func, 1))
	    gn->vcts[MUS_INPUT_FUNCTION] = func; /* as_needed_input_func set at make time will pick this up */
	  else Xen_bad_arity_error(S_phase_vocoder, 2, func, S_phase_vocoder " input function wants 1 arg");
	}
      if (Xen_is_procedure(analyze_func))
	{
	  if (Xen_is_aritable(analyze_func, 2))
	    {
	      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_func;
	      analyze = pvanalyze;
	    }
	  else Xen_bad_arity_error(S_phase_vocoder, 3, analyze_func, S_phase_vocoder " analyze function wants 2 args");
	}
      if (Xen_is_procedure(edit_func))
	{
	  if (Xen_is_aritable(edit_func, 1))
	    {
	      gn->vcts[MUS_EDIT_FUNCTION] = edit_func;
	      edit = pvedit;
	    }
	  else Xen_bad_arity_error(S_phase_vocoder, 4, edit_func, S_phase_vocoder " edit function wants 1 arg");
	}
      if (Xen_is_procedure(synthesize_func))
	{
	  if (Xen_is_aritable(synthesize_func, 1))
	    {
	      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synthesize_func;
	      synthesize = pvsynthesize;
	    }
	  else Xen_bad_arity_error(S_phase_vocoder, 5, synthesize_func, S_phase_vocoder " synthesize function wants 1 arg");
	}
      return(C_double_to_Xen_real(mus_phase_vocoder_with_editors(g, NULL, analyze, edit, synthesize)));
    }
  return(C_double_to_Xen_real(mus_phase_vocoder(g, NULL)));
}

#if HAVE_SCHEME
    #define pv_example "(" S_make_phase_vocoder " #f 512 4 256 1.0 #f #f #f)"
    #define pv_edit_example "(" S_make_phase_vocoder " #f 512 4 256 1.0\n\
    (lambda (v infunc) (snd-print \"analyzing\") #t)\n\
    (lambda (v) (snd-print \"editing\") #t)\n\
    (lambda (v) (snd-print \"resynthesizing\") 0.0))"
#endif
#if HAVE_RUBY
    #define pv_example "make_phase_vocoder(false, 512, 4, 256, 1.0, false, false, false)"
    #define pv_edit_example "make_phase_vocoder(false, 512, 4, 256, 1.0,\n\
        lambda do | v, infunc | snd_print(\"analyzing\"); true end,\n\
        lambda do | v | snd_print(\"editing\"); true end,\n\
        lambda do | v | snd_print(\"resynthesizing\"); 0.0 end)"
#endif
#if HAVE_FORTH
    #define pv_example "#f 512 4 256 1.0 #f #f #f " S_make_phase_vocoder
    #define pv_edit_example "#f 512 4 256 1.0\n\
    lambda: <{ v infunc -- f }> \"analyzing\" snd-print drop #t ;\n\
    lambda: <{ v -- n }> \"editing\" snd-print drop #t ;\n\
    lambda: <{ v -- r }> \"resynthesizing\" snd-print drop 0.0 ; " S_make_phase_vocoder
#endif

#define H_make_phase_vocoder "(" S_make_phase_vocoder " input fft-size overlap interp pitch analyze edit synthesize): \
return a new phase-vocoder generator; input is the input function (it can be set at run-time), analyze, edit, \
and synthesize are either " PROC_FALSE " or functions that replace the default innards of the generator, fft-size, overlap \
and interp set the fftsize, the amount of overlap between ffts, and the time between new analysis calls. \
'analyze', if given, takes 2 args, the generator and the input function; if it returns " PROC_TRUE ", the default analysis \
code is also called.  'edit', if given, takes 1 arg, the generator; if it returns " PROC_TRUE ", the default edit code \
is run.  'synthesize' is a function of 1 arg, the generator; it is called to get the current vocoder \
output. \n\n  " pv_example "\n\n  " pv_edit_example "."

#if HAVE_SCHEME
static s7_pointer g_make_phase_vocoder(s7_scheme *sc, s7_pointer args)
{
  mus_xen *gn;
  mus_any *ge;
  s7_pointer in_obj, edit_obj, synth_obj, analyze_obj, p, pv_obj, fp;
  mus_float_t pitch;
  int fft_size, overlap, interp;

  in_obj = s7_car(args); /* input proc 1 arg */
  if (s7_is_procedure(in_obj))
    {
      if (!s7_is_aritable(sc, in_obj, 1))
	return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 1, in_obj, "a function of one argument"));
    }
  else
    {
      if (mus_is_xen(in_obj))
	{
	  if (!mus_is_input(Xen_to_mus_any(in_obj)))
	    return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 1, in_obj, "an input generator"));
	}
      else 
	{
	  if (in_obj == Xen_false)
	    in_obj = s7_undefined(sc);
	  else return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 1, in_obj, "a procedure or an input generator"));
	}
    }

  fp = s7_cadr(args);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 2, fp, "an integer"));
      fft_size = s7_integer(fp);
      if (fft_size <= 1) 
	Xen_out_of_range_error(S_make_phase_vocoder, 2, fp, "fft size <= 1?");
      if (fft_size > mus_max_malloc())
	Xen_out_of_range_error(S_make_phase_vocoder, 2, fp, "fft size too large (see mus-max-malloc)");
      if (!is_power_of_2(fft_size))
	Xen_out_of_range_error(S_make_phase_vocoder, 2, fp, "fft size must be power of 2");
    }
  else fft_size = 512;
  
  p = s7_cddr(args);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 3, fp, "an integer"));
      overlap = s7_integer(fp);
      if (overlap <= 0)
	Xen_out_of_range_error(S_make_phase_vocoder, 3, fp, "overlap <= 0?");
    }
  else overlap = 4;

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 4, fp, "an integer"));
      interp = s7_integer(fp);
      if (interp <= 0)
	Xen_out_of_range_error(S_make_phase_vocoder, 4, fp, "interp <= 0?");
    }
  else interp = 128;

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp != Xen_false)
    pitch = s7_number_to_real(sc, fp);
  else pitch = 1.0;

  analyze_obj = s7_cadr(p);
  if (analyze_obj != Xen_false)
    {
      if (s7_is_procedure(analyze_obj))
	{
	  if (!s7_is_aritable(sc, analyze_obj, 2))
	    return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 6, analyze_obj, "a function of two arguments"));
	}
      else return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 6, analyze_obj, "a procedure"));
    }
  else analyze_obj = s7_undefined(sc);

  p = s7_cddr(p);
  edit_obj = s7_car(p);
  if (edit_obj != Xen_false)
    {
      if (s7_is_procedure(edit_obj))
	{
	  if (!s7_is_aritable(sc, edit_obj, 1))
	    return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 7, edit_obj, "a function of one argument"));
	}
      else return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 7, edit_obj, "a procedure"));
    }
  else edit_obj = s7_undefined(sc);
  
  synth_obj = s7_cadr(p);
  if (synth_obj != Xen_false)
    {
      if (s7_is_procedure(synth_obj))
	{
	  if (!s7_is_aritable(sc, synth_obj, 1))
	    return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 8, synth_obj, "a function of one argument"));
	}
      else return(s7_wrong_type_arg_error(sc, S_make_phase_vocoder, 8, synth_obj, "a procedure"));
    }
  else synth_obj = s7_undefined(sc);
  
  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_phase_vocoder(NULL,
				fft_size, overlap, interp, pitch,
				(!Xen_is_bound(analyze_obj) ? NULL : pvanalyze),
				(!Xen_is_bound(edit_obj) ? NULL : pvedit),
				(!Xen_is_bound(synth_obj) ? NULL : pvsynthesize),
				(void *)gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_obj;
      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synth_obj;
      gn->gen = ge;
      pv_obj = mus_xen_to_object(gn);
      /* need scheme-relative backpointer for possible function calls */
      gn->vcts[MUS_SELF_WRAPPER] = pv_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(pv_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_phase_vocoder));
}
#else
static Xen g_make_phase_vocoder(Xen arglist)
{
  Xen in_obj = Xen_undefined, edit_obj = Xen_undefined, synthesize_obj = Xen_undefined, analyze_obj = Xen_undefined;
  mus_xen *gn;
  mus_any *ge;
  Xen args[16];
  Xen keys[8];
  Xen pv_obj;
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int vals;
  int fft_size = 512, overlap = 4, interp = 128;
  mus_float_t pitch = 1.0;

  keys[0] = kw_input;
  keys[1] = kw_fft_size;
  keys[2] = kw_overlap;
  keys[3] = kw_interp;
  keys[4] = kw_pitch;
  keys[5] = kw_analyze;
  keys[6] = kw_edit;
  keys[7] = kw_synthesize;

  {
    int i, arglist_len;
    Xen p;
    arglist_len = Xen_list_length(arglist);
    if (arglist_len > 16) clm_error(S_make_phase_vocoder, "too many arguments!", arglist);
    for (i = 0, p = arglist; i < arglist_len; i++, p = Xen_cdr(p)) args[i] = Xen_car(p);
    vals = mus_optkey_unscramble(S_make_phase_vocoder, arglist_len, 8, keys, args, orig_arg);
  }
  if (vals > 0)
    {
      in_obj = mus_optkey_to_input_procedure(keys[0], S_make_phase_vocoder, orig_arg[0], Xen_undefined, 1, S_phase_vocoder " input procedure takes 1 arg");

      fft_size = Xen_optkey_to_int(kw_fft_size, keys[1], S_make_phase_vocoder, orig_arg[1], fft_size);
      if (fft_size <= 1) 
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size <= 1?");
      if (fft_size > mus_max_malloc())
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size too large (see mus-max-malloc)");
      if (!is_power_of_2(fft_size))
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[1], keys[1], "fft size must be power of 2");

      overlap = Xen_optkey_to_int(kw_overlap, keys[2], S_make_phase_vocoder, orig_arg[2], overlap);
      if (overlap <= 0)
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[2], keys[2], "overlap <= 0?");

      interp = Xen_optkey_to_int(kw_interp, keys[3], S_make_phase_vocoder, orig_arg[3], interp);
      if (interp <= 0)
	Xen_out_of_range_error(S_make_phase_vocoder, orig_arg[3], keys[3], "interp <= 0?");

      pitch = Xen_optkey_to_float(kw_pitch, keys[4], S_make_phase_vocoder, orig_arg[4], pitch);

      analyze_obj = mus_optkey_to_procedure(keys[5], S_make_phase_vocoder, orig_arg[5], Xen_undefined, 2, S_phase_vocoder " analyze procedure takes 2 args");
      edit_obj = mus_optkey_to_procedure(keys[6], S_make_phase_vocoder, orig_arg[6], Xen_undefined, 1, S_phase_vocoder " edit procedure takes 1 arg");
      synthesize_obj = mus_optkey_to_procedure(keys[7], S_make_phase_vocoder, orig_arg[7], Xen_undefined, 1, S_phase_vocoder " synthesize procedure takes 1 arg");
    }

  gn = mx_alloc(MUS_MAX_VCTS);
  {int i; for (i = 0; i < MUS_MAX_VCTS; i++) gn->vcts[i] = Xen_undefined;}
  {
    mus_error_handler_t *old_error_handler;
    old_error_handler = mus_error_set_handler(local_mus_error);
    ge = mus_make_phase_vocoder(NULL,
				fft_size, overlap, interp, pitch,
				(!Xen_is_bound(analyze_obj) ? NULL : pvanalyze),
				(!Xen_is_bound(edit_obj) ? NULL : pvedit),
				(!Xen_is_bound(synthesize_obj) ? NULL : pvsynthesize),
				(void *)gn);
    mus_error_set_handler(old_error_handler);
  }

  if (ge)
    {
      gn->vcts[MUS_INPUT_FUNCTION] = in_obj;
      gn->vcts[MUS_EDIT_FUNCTION] = edit_obj;
      gn->vcts[MUS_ANALYZE_FUNCTION] = analyze_obj;
      gn->vcts[MUS_SYNTHESIZE_FUNCTION] = synthesize_obj;
      gn->gen = ge;
      pv_obj = mus_xen_to_object(gn);
      /* need scheme-relative backpointer for possible function calls */
      gn->vcts[MUS_SELF_WRAPPER] = pv_obj;
      set_as_needed_input_choices(ge, in_obj, gn);
      return(pv_obj);
    }

  free(gn->vcts);
  free(gn);
  return(clm_mus_error(local_error_type, local_error_msg, S_make_phase_vocoder));
}
#endif


static Xen g_phase_vocoder_amps(Xen pv) 
{
  #define H_phase_vocoder_amps "(" S_phase_vocoder_amps " gen): " S_vct " containing the current output sinusoid amplitudes"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_amps, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_amps(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}

  
static Xen g_phase_vocoder_freqs(Xen pv) 
{
  #define H_phase_vocoder_freqs "(" S_phase_vocoder_freqs " gen): " S_vct " containing the current output sinusoid frequencies"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_freqs, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_freqs(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len, amps));
}

  
static Xen g_phase_vocoder_phases(Xen pv) 
{
  #define H_phase_vocoder_phases "(" S_phase_vocoder_phases " gen): " S_vct " containing the current output sinusoid phases"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_phases, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_phases(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}
  

static Xen g_phase_vocoder_amp_increments(Xen pv) 
{
  #define H_phase_vocoder_amp_increments "(" S_phase_vocoder_amp_increments " gen): " S_vct " containing the current output sinusoid amplitude increments per sample"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_amp_increments, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_amp_increments(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len, amps));
}
  

static Xen g_phase_vocoder_phase_increments(Xen pv) 
{
  #define H_phase_vocoder_phase_increments "(" S_phase_vocoder_phase_increments " gen): " S_vct " containing the current output sinusoid phase increments"
  mus_float_t *amps; 
  int len;
  mus_xen *gn;

  Xen_check_type((mus_is_xen(pv)) && (mus_is_phase_vocoder(Xen_to_mus_any(pv))), pv, 1, S_phase_vocoder_phase_increments, "a " S_phase_vocoder " generator");

  gn = Xen_to_mus_xen(pv);
  amps = mus_phase_vocoder_phase_increments(gn->gen); 
  len = (int)mus_length(gn->gen);
  return(xen_make_vct_wrapper(len / 2, amps));
}

  


/* -------- ssb-am -------- */

static Xen g_is_ssb_am(Xen obj) 
{
  #define H_is_ssb_am "(" S_is_ssb_am " gen): " PROC_TRUE " if gen is a " S_ssb_am
  return(C_bool_to_Xen_boolean((mus_is_xen(obj)) && (mus_is_ssb_am(Xen_to_mus_any(obj)))));
}


#if HAVE_SCHEME
static s7_pointer g_make_ssb_am(s7_scheme *sc, s7_pointer args)
{
  #define H_make_ssb_am "(" S_make_ssb_am " (frequency 0.0) (order 40)): return a new " S_ssb_am " generator."
  #define MUS_MAX_SSB_ORDER 65536
  mus_any *ge;
  int order;
  mus_float_t freq;
  s7_pointer un;

  freq = s7_number_to_real(s7, s7_car(args));
  if (freq > (0.5 * mus_srate()))
    Xen_out_of_range_error(S_make_ssb_am, 1, s7_car(args), "freq > srate/2?");
  
  un = s7_cadr(args);
  if (!s7_is_integer(un))
    return(s7_wrong_type_arg_error(s7, S_make_ssb_am, 2, un, "an integer"));
  order = s7_integer(un);
  if (order <= 0)
    Xen_out_of_range_error(S_make_ssb_am, 2, un, "order <= 0?");
  if (order > MUS_MAX_SSB_ORDER)
    Xen_out_of_range_error(S_make_ssb_am, 2, un, "order too large?");

  ge = mus_make_ssb_am(freq, order);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#else
static Xen g_make_ssb_am(Xen arg1, Xen arg2, Xen arg3, Xen arg4)
{
  #define H_make_ssb_am "(" S_make_ssb_am " (frequency 0.0) (order 40)): return a new " S_ssb_am " generator."
  #define MUS_MAX_SSB_ORDER 65536

  mus_any *ge;
  Xen args[4]; 
  Xen keys[2];
  int orig_arg[2] = {0, 0};
  int vals, pr = 0;
  int order = 40;
  mus_float_t freq = 0.0;

  keys[0] = kw_frequency;
  keys[1] = kw_order;

  if (Aok(arg1)) {args[pr++] = arg1; if (Aok(arg2)) {args[pr++] = arg2; if (Aok(arg3)) {args[pr++] = arg3; if (Aok(arg4)) {args[pr++] = arg4;}}}}
  vals = mus_optkey_unscramble(S_make_ssb_am, pr, 2, keys, args, orig_arg);
  if (vals > 0)
    {
      freq = Xen_optkey_to_float(kw_frequency, keys[0], S_make_ssb_am, orig_arg[0], freq);
      if (freq > (0.5 * mus_srate()))
	Xen_out_of_range_error(S_make_ssb_am, orig_arg[0], keys[0], "freq > srate/2?");

      order = Xen_optkey_to_int(kw_order, keys[1], S_make_ssb_am, orig_arg[1], order);
      if (order <= 0)
	Xen_out_of_range_error(S_make_ssb_am, orig_arg[1], keys[1], "order <= 0?");
      if (order > MUS_MAX_SSB_ORDER)
	Xen_out_of_range_error(S_make_ssb_am, orig_arg[1], keys[1], "order too large?");
    }

  ge = mus_make_ssb_am(freq, order);
  if (ge) return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}
#endif

static Xen g_ssb_am(Xen obj, Xen insig, Xen fm)
{
  #define H_ssb_am "(" S_ssb_am " gen (insig 0.0) (fm 0.0)): get the next sample from " S_ssb_am " generator"

  mus_float_t insig1 = 0.0;
  mus_any *g = NULL;
  mus_xen *gn;

  Xen_to_C_generator(obj, gn, g, mus_is_ssb_am, S_ssb_am, "an ssb-am generator");
  Xen_real_to_C_double_if_bound(insig, insig1, S_ssb_am, 2);

  if (Xen_is_bound(fm))
    {
      Xen_check_type(Xen_is_number(fm), fm, 3, S_ssb_am, "a number");
      return(C_double_to_Xen_real(mus_ssb_am(g, insig1, Xen_real_to_C_double(fm))));
    }
  return(C_double_to_Xen_real(mus_ssb_am_unmodulated(g, insig1)));
}


#define S_mus_frandom "mus-frandom"
#define S_mus_irandom "mus-irandom"

static Xen g_mus_frandom(Xen val) 
{
  return(C_double_to_Xen_real(mus_frandom(Xen_real_to_C_double_with_caller(val, S_mus_frandom))));
}

static Xen g_mus_irandom(Xen val) 
{
  mus_long_t ind;
  Xen_to_C_integer_or_error(val, ind, S_mus_irandom, 1);
  return(C_int_to_Xen_integer(mus_irandom(ind)));
}


static Xen mus_clm_output(void);
static Xen mus_clm_reverb(void);



/* Xen out, Xen in, Xen ost, Xen olen, Xen ist, Xen mx, Xen envs */
static Xen g_mus_file_mix(Xen args)
{
  #define H_mus_file_mix "(" S_mus_file_mix " outfile infile (outloc 0) (framples) (inloc 0) matrix envs): \
mix infile into outfile starting at outloc in outfile and inloc in infile \
mixing 'framples' framples into 'outfile'.  framples defaults to the length of infile. If matrix, \
use it to scale the various channels; if envs (an array of envelope generators), use \
it in conjunction with matrix to scale/envelope all the various ins and outs. \
'outfile' can also be a " S_frample_to_file " generator, and 'infile' can be a " S_file_to_frample " generator."

  Xen arg, out, in;
  mus_any *outf = NULL, *inf = NULL;
  mus_float_t *matrix = NULL;
  mus_any ***envs1 = NULL;
  int i;
  mus_long_t ostart = 0, istart = 0, osamps = 0;
  int in_chans = 0, out_chans = 0, mx_chans = 0, in_size = 0;  /* mus_mix in clm.c assumes the envs array is large enough */
  const char *outfile = NULL, *infile = NULL;

  /* -------- setup output gen -------- */
  Xen_check_type(Xen_is_pair(args), args, 0, S_mus_file_mix, "a filename or a " S_frample_to_file " generator");
  arg = args; 
  out = Xen_car(arg);
  Xen_check_type(Xen_is_string(out) || ((mus_is_xen(out)) && (mus_is_output(Xen_to_mus_any(out)))), 
		 out, 1, S_mus_file_mix, "a filename or a " S_frample_to_file " generator");
  if (Xen_is_string(out)) 
    {
      outfile = Xen_string_to_C_string(out);
      if (!mus_file_probe(outfile))
	Xen_error(NO_SUCH_FILE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": no such file, ~S"), out));
      out_chans = mus_sound_chans(outfile);
      if (out_chans <= 0)
	Xen_error(BAD_HEADER, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": ~S output chans <= 0"), out));
    }
  else 
    {
      outf = Xen_to_mus_any(out);
      out_chans = mus_channels(outf);
    }

  /* -------- setup input gen -------- */
  arg = Xen_cdr(arg); 
  if (!Xen_is_pair(arg))
    Xen_error(NO_SUCH_FILE, Xen_list_1(C_string_to_Xen_string(S_mus_file_mix ": input file or generator (second argument) missing")));
  in = Xen_car(arg);
  Xen_check_type(Xen_is_string(in) || ((mus_is_xen(in)) && (mus_is_input(Xen_to_mus_any(in)))), 
		 in, 2, S_mus_file_mix, "a filename or a " S_file_to_frample " generator");
  if (Xen_is_string(in)) 
    {
      infile = Xen_string_to_C_string(in);
      if (!mus_file_probe(infile))
	Xen_error(NO_SUCH_FILE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": no such file, ~S"), in));
      in_chans = mus_sound_chans(infile);
      if (in_chans <= 0)
	Xen_error(BAD_HEADER, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": ~S input chans <= 0"), in));
      osamps = mus_sound_framples(infile);
    }
  else 
    {
      inf = Xen_to_mus_any(in);
      in_chans = mus_channels(inf);
      osamps = mus_length(inf);
    }

  /* inf and outf only exist during the rest of the arglist scan if not infile or outfile.
   *    we need to delay making the inf/outf gens in this case to greatly simplify error handling.
   */

  /* rest of args are optional */
  arg = Xen_cdr(arg); 
  if (!Xen_is_null(arg))
    {
      Xen ost;
      ost = Xen_car(arg);
      Xen_check_type(Xen_is_integer(ost), ost, 3, S_mus_file_mix, "an integer");
      ostart = Xen_llong_to_C_llong(ost);

      arg = Xen_cdr(arg); 
      if (!Xen_is_null(arg))
	{
	  Xen olen;
	  olen = Xen_car(arg);
	  Xen_check_type(Xen_is_integer(olen), olen, 4, S_mus_file_mix, "an integer");
	  osamps = Xen_llong_to_C_llong(olen); 
	  if (osamps <= 0) return(Xen_false);
	  
	  arg = Xen_cdr(arg); 
	  if (!Xen_is_null(arg))
	    {
	      Xen ist;
	      ist = Xen_car(arg);
	      Xen_check_type(Xen_is_integer(ist), ist, 5, S_mus_file_mix, "an integer");
	      istart = Xen_llong_to_C_llong(ist);
	      
	      arg = Xen_cdr(arg); 
	      if (!Xen_is_null(arg))
		{
		  Xen mx;
		  mx = Xen_car(arg);
		  Xen_check_type((mus_is_vct(mx)) || (Xen_is_false(mx)), mx, 6, S_mus_file_mix, "a " S_vct);
		  if (mus_is_vct(mx))
		    {
		      matrix = mus_vct_data(Xen_to_vct(mx));
		      mx_chans = (int)sqrt(mus_vct_length(Xen_to_vct(mx)));
		    }

		  arg = Xen_cdr(arg); 
		  if (!Xen_is_null(arg))
		    {
		      Xen envs;
		      envs = Xen_car(arg);
		      Xen_check_type((Xen_is_false(envs)) || (Xen_is_vector(envs)), envs, 7, S_mus_file_mix, "a vector of envs");
		      if (Xen_is_vector(envs))
			{
			  int in_len = 0, out_len, j, out_size;
			  /* pack into a C-style array of arrays of env pointers */
			  in_len = Xen_vector_length(envs);
			  if (in_len == 0)
			    Xen_error(BAD_TYPE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": env vector, ~A, can't be empty"), envs));
			  
			  for (i = 0; i < in_len; i++)
			    {
			      Xen datum;
			      datum = Xen_vector_ref(envs, i);
			      if (!(Xen_is_vector(datum)))
				Xen_error(BAD_TYPE, Xen_list_2(C_string_to_Xen_string(S_mus_file_mix ": vector, ~A, must contain vectors of envelopes"), datum));
			    }
			  
			  out_len = Xen_vector_length(Xen_vector_ref(envs, 0));
			  if (in_len < in_chans) in_size = in_chans; else in_size = in_len;
			  if (out_len < out_chans) out_size = out_chans; else out_size = out_len;
			  
			  envs1 = (mus_any ***)malloc(in_size * sizeof(mus_any **));
			  for (i = 0; i < in_size; i++) 
			    envs1[i] = (mus_any **)calloc(out_size, sizeof(mus_any *));
			  
			  for (i = 0; i < in_len; i++)
			    for (j = 0; j < out_len; j++) 
			      {
				Xen datum1;
				datum1 = Xen_vector_ref(Xen_vector_ref(envs, i), j);
				if (mus_is_xen(datum1))
				  {
				    if (mus_is_env(Xen_to_mus_any(datum1)))
				      envs1[i][j] = Xen_to_mus_any(datum1);
				    else 
				      {
					for (i = 0; i < in_size; i++) if (envs1[i]) free(envs1[i]);
					free(envs1);
					Xen_error(BAD_TYPE, Xen_list_4(C_string_to_Xen_string(S_mus_file_mix ": vector, ~A at ~A ~A, must contain an envelope"), 
								       datum1,
								       C_int_to_Xen_integer(i),
								       C_int_to_Xen_integer(j)));
				      }
				  }
			    }
			}
		    }
		}
	    }
	}
    }

  if ((infile) && (outfile))
    mus_file_mix(outfile, infile, ostart, osamps, istart, matrix, mx_chans, envs1);
  else
    {
      if (infile)
	inf = mus_make_file_to_frample(infile);
      if (outfile)
	outf = mus_continue_sample_to_file(outfile);
      mus_file_mix_with_reader_and_writer(outf, inf, ostart, osamps, istart, matrix, mx_chans, envs1);
      if (infile)
	mus_free((mus_any *)inf);
      if (outfile)
	mus_free((mus_any *)outf);
    }
  if (envs1) 
    {
      for (i = 0; i < in_size; i++) if (envs1[i]) free(envs1[i]);
      free(envs1);
    }
  return(Xen_true);
}


/* Xen file, Xen beg, Xen dur, Xen mx, Xen revmx, Xen envs, Xen srcs, Xen srcenv, Xen outstream, Xen revstream */

static Xen g_mus_file_mix_with_envs(Xen args)
{
  #define H_mus_file_mix_with_envs "(" S_mus_file_mix_with_envs " file beg dur mx revmx envs srcs srcenv out rev) is an extension of " S_mus_file_mix ", primarily \
intended to speed up the fullmix instrument.  file is a vector of readin generators.  beg is the sample at which to start mixing \
output, dur is the number of samples to write. mx is a matrix, revmx is either #f or a matrix. "

  int i, in_chans, out_chans, mx_chans = 0, rev_chans = 0, rev_mix_chans = 0;
  mus_long_t st, nd;
  mus_any *s_env = NULL, *ostr, *rstr = NULL;
  mus_any **mix_envs, **mix_srcs, **mix_rds;
  mus_xen *gn;
  Xen ve, arg, file, beg, dur, mx, revmx, envs, srcs, srcenv, outstream, revstream;
  mus_float_t *mix = NULL, *rev_mix = NULL;

  i = Xen_list_length(args);
  if ((i < 8) || (i > 10)) /* no wrong-number-of-args error in xen.h, so I'll use out-of-range */
    Xen_out_of_range_error(S_mus_file_mix_with_envs, 0, args, "wrong number of args");
    
  arg = args;
  file = Xen_car(arg);
  Xen_check_type(Xen_is_vector(file), file, 1, S_mus_file_mix_with_envs, "a vector of readin generators");
  in_chans = Xen_vector_length(file);
  
  arg = Xen_cdr(arg);
  beg = Xen_car(arg);
  Xen_check_type(Xen_is_integer(beg), beg, 2, S_mus_file_mix_with_envs, "an integer");
  st = Xen_integer_to_C_int(beg);

  arg = Xen_cdr(arg);
  dur = Xen_car(arg);
  Xen_check_type(Xen_is_integer(dur), dur, 3, S_mus_file_mix_with_envs, "an integer");
  nd = st + Xen_integer_to_C_int(dur);

  arg = Xen_cdr(arg);
  mx = Xen_car(arg);
  if (mus_is_vct(mx))
    {
      mix = mus_vct_data(Xen_to_vct(mx));
      mx_chans = (int)sqrt(mus_vct_length(Xen_to_vct(mx)));
    }

  arg = Xen_cdr(arg);
  revmx = Xen_car(arg);
  if (mus_is_vct(revmx))
    {
      rev_mix = mus_vct_data(Xen_to_vct(revmx));
      rev_mix_chans = (int)sqrt(mus_vct_length(Xen_to_vct(revmx)));
    }

  arg = Xen_cdr(arg);
  envs = Xen_car(arg);
  if (!Xen_is_false(envs))
    Xen_check_type(Xen_is_vector(envs), envs, 6, S_mus_file_mix_with_envs, "a vector of env generators");

  arg = Xen_cdr(arg);
  srcs = Xen_car(arg);
  if (!Xen_is_false(srcs))
    Xen_check_type(Xen_is_vector(srcs), srcs, 7, S_mus_file_mix_with_envs, "a vector of src generators");

  arg = Xen_cdr(arg);
  srcenv = Xen_car(arg);
  if (!Xen_is_false(srcenv))
    {
      gn = (mus_xen *)Xen_object_ref_checked(srcenv, mus_xen_tag);
      if (!gn) Xen_check_type(false, srcenv, 8, S_mus_file_mix_with_envs, "an env generator");
      s_env = gn->gen;
      Xen_check_type(mus_is_env(s_env), srcenv, 8, S_mus_file_mix_with_envs, "an env generator");
    }

  revstream = Xen_false;
  arg = Xen_cdr(arg);
  if (!Xen_is_null(arg))
    {
      outstream = Xen_car(arg);
      gn = (mus_xen *)Xen_object_ref_checked(outstream, mus_xen_tag);
      if (!gn)
	Xen_check_type(false, outstream, 9, S_mus_file_mix_with_envs, "an output generator");
      ostr = gn->gen;

      arg = Xen_cdr(arg);
      if (!Xen_is_null(arg))
	revstream = Xen_car(arg);
    }
  else ostr = Xen_to_mus_any(mus_clm_output());
  out_chans = mus_channels(ostr);

  if (rev_mix)
    {
      if (!Xen_is_false(revstream))
	{
	  gn = (mus_xen *)Xen_object_ref_checked(revstream, mus_xen_tag);
	  if (!gn)
	    Xen_check_type(false, revstream, 10, S_mus_file_mix_with_envs, "an output generator");
	  rstr = gn->gen;
	}
      else rstr = Xen_to_mus_any(mus_clm_reverb());
      rev_chans = mus_channels(rstr);
    }

  mix_rds = (mus_any **)calloc(in_chans, sizeof(mus_any *));
  mix_srcs = (mus_any **)calloc(in_chans, sizeof(mus_any *));

  for (i = 0; i < in_chans; i++)
    mix_rds[i] = Xen_to_mus_any(Xen_vector_ref(file, i));
    
  if (Xen_is_vector(srcs))
    {
      for (i = 0; i < in_chans; i++)
	{
	  ve = Xen_vector_ref(srcs, i);
	  if (!Xen_is_false(ve)) mix_srcs[i] = Xen_to_mus_any(ve);
	}
    }

  mix_envs = (mus_any **)calloc(in_chans * out_chans, sizeof(mus_any *));
  if (Xen_is_vector(envs))
    for (i = 0; i < in_chans * out_chans; i++)
      {
	ve = Xen_vector_ref(envs, i);
	if (!Xen_is_false(ve)) mix_envs[i] = Xen_to_mus_any(ve);
      }

  {
    mus_long_t samp;
    int outp;
    mus_float_t src_env_val = 0.0;
    mus_float_t *infs, *out_frample, *rev_frample = NULL;

    infs = (mus_float_t *)calloc(in_chans, sizeof(mus_float_t));
    out_frample = (mus_float_t *)calloc(out_chans, sizeof(mus_float_t));
    if (rev_mix) rev_frample = (mus_float_t *)calloc(rev_chans, sizeof(mus_float_t));

    if (in_chans == 1)
      {
	mus_any *s = NULL, *r = NULL;
	s = mix_srcs[0];
	if (!s) r = mix_rds[0];

	for (samp = st; samp < nd; samp++)
	  {
	    for (outp = 0; outp < out_chans; outp++)
	      {
		mus_any *e;
		e = mix_envs[outp];
		if (e)
		  mix[outp] = mus_env(e);
	      }
	    if (s_env)
	      src_env_val = mus_env(s_env);
	    if (s)
	      infs[0] = mus_src(s, src_env_val, NULL);
	    else 
	      {
		if (r) 
		  infs[0] = mus_readin(r);
		else infs[0] = 0.0;
	      }
	    mus_frample_to_file(ostr, samp, mus_frample_to_frample(mix, mx_chans, infs, in_chans, out_frample, out_chans));
	    if (rev_mix) mus_frample_to_file(rstr, samp, mus_frample_to_frample(rev_mix, rev_mix_chans, infs, in_chans, rev_frample, rev_chans));
	  }
      }
    else
      {
	for (samp = st; samp < nd; samp++)
	  {
	    int inp, off;
	    for (inp = 0, off = 0; inp < in_chans; inp++, off += mx_chans)
	      for (outp = 0; outp < out_chans; outp++)
		{
		  mus_any *e;
		  e = mix_envs[inp * out_chans + outp]; /* this is different from the matrix setup -- I don't know why */
		  if (e)
		    mix[off + outp] = mus_env(e);
		}
	    if (s_env)
	      src_env_val = mus_env(s_env);
	    for (inp = 0; inp < in_chans; inp++)
	      {
		mus_any *s;
		s = mix_srcs[inp];
		if (s)
		  infs[inp] = mus_src(s, src_env_val, NULL);
		else 
		  {
		    s = mix_rds[inp];
		    if (s) 
		      infs[inp] = mus_readin(s);
		    else infs[inp] = 0.0;
		  }
	      }
	    mus_frample_to_file(ostr, samp, mus_frample_to_frample(mix, mx_chans, infs, in_chans, out_frample, out_chans));
	    if (rev_mix) mus_frample_to_file(rstr, samp, mus_frample_to_frample(rev_mix, rev_mix_chans, infs, in_chans, rev_frample, rev_chans));
	  }
      }
    free(infs);
    free(out_frample);
    if (rev_frample) free(rev_frample);
  }

  free(mix_rds);
  free(mix_srcs);
  free(mix_envs);
  return(Xen_false);
}


static Xen g_frample_to_frample(Xen mx, Xen infr, Xen inchans, Xen outfr, Xen outchans)
{
  #define H_frample_to_frample "(" S_frample_to_frample " matrix in-data in-chans out-data out-chans): pass frample in-data through matrix \
returning frample out-data; this is a matrix multiply of matrix and in-data"
  int ins, outs, mxs;
  vct *vin, *vout, *vmx;

  Xen_check_type(mus_is_vct(mx), mx, 1, S_frample_to_frample, "a " S_vct);
  Xen_check_type(mus_is_vct(infr), infr, 2, S_frample_to_frample, "a " S_vct);
  Xen_check_type(mus_is_vct(outfr), outfr, 4, S_frample_to_frample, "a " S_vct);
  Xen_check_type(Xen_is_integer(inchans), inchans, 3, S_frample_to_frample, "an integer");
  Xen_check_type(Xen_is_integer(outchans), outchans, 5, S_frample_to_frample, "an integer");

  ins = Xen_integer_to_C_int(inchans);
  vin = Xen_to_vct(infr);
  if (mus_vct_length(vin) < ins) ins = mus_vct_length(vin);
  if (ins <= 0) return(outfr);

  outs = Xen_integer_to_C_int(outchans);
  vout = Xen_to_vct(outfr);
  if (mus_vct_length(vout) < outs) outs = mus_vct_length(vout);
  if (outs <= 0) return(outfr);

  vmx = Xen_to_vct(mx);
  mxs = (int)sqrt(mus_vct_length(vmx));

  mus_frample_to_frample(mus_vct_data(vmx), mxs, mus_vct_data(vin), ins, mus_vct_data(vout), outs);
  return(outfr);
}




#if HAVE_SCHEME
#ifndef _MSC_VER

#include <time.h>
#include <sys/time.h>

static struct timeval overall_start_time;
#define S_get_internal_real_time "get-internal-real-time"
#define S_internal_time_units_per_second "internal-time-units-per-second"

static Xen g_get_internal_real_time(void) 
{
  #define H_get_internal_real_time "(" S_get_internal_real_time ") returns the number of seconds since \
the program started.  The number is in terms of " S_internal_time_units_per_second ", usually 1"
  struct timezone z0;
  struct timeval t0;
  mus_float_t secs;
  gettimeofday(&t0, &z0);
  secs = difftime(t0.tv_sec, overall_start_time.tv_sec);
  return(C_double_to_Xen_real(secs + 0.000001 * (t0.tv_usec - overall_start_time.tv_usec)));
}
#else
static Xen g_get_internal_real_time(void) {return(C_double_to_Xen_real(0.0));}
#endif

Xen_wrap_no_args(g_get_internal_real_time_w, g_get_internal_real_time)
#endif




/* -------------------------------- scheme-side optimization -------------------------------- */

#if HAVE_SCHEME

static mus_float_t mus_nsin_unmodulated(mus_any *p) {return(mus_nsin(p, 0.0));}
static mus_float_t mus_ncos_unmodulated(mus_any *p) {return(mus_ncos(p, 0.0));}
static mus_float_t mus_nrxysin_unmodulated(mus_any *p) {return(mus_nrxysin(p, 0.0));}
static mus_float_t mus_nrxycos_unmodulated(mus_any *p) {return(mus_nrxycos(p, 0.0));}
static mus_float_t mus_rxyksin_unmodulated(mus_any *p) {return(mus_rxyksin(p, 0.0));}
static mus_float_t mus_rxykcos_unmodulated(mus_any *p) {return(mus_rxykcos(p, 0.0));}
static mus_float_t mus_square_wave_unmodulated(mus_any *p) {return(mus_square_wave(p, 0.0));}
static mus_float_t mus_sawtooth_wave_unmodulated(mus_any *p) {return(mus_sawtooth_wave(p, 0.0));}

static mus_float_t mus_src_simple(mus_any *p) {return(mus_src(p, 0.0, NULL));}
static mus_float_t mus_src_two(mus_any *p, mus_float_t x) {return(mus_src(p, x, NULL));}
/* static mus_float_t mus_granulate_simple(mus_any *p) {return(mus_granulate_with_editor(p, NULL, NULL));} */
static mus_float_t mus_convolve_simple(mus_any *p) {return(mus_convolve(p, NULL));}
/* static mus_float_t mus_phase_vocoder_simple(mus_any *p) {return(mus_phase_vocoder(p, NULL));} */

/* almost no error checking here; for example all the s7_c_object_value calls should check that their argument is a c-object */

#define GEN_1(Type, Func)						\
  static bool is_ ## Type ## _b(s7_pointer p)				\
  {									\
    return((mus_is_xen(p)) && (mus_is_ ## Type(Xen_to_mus_any(p))));	\
  }									\
  static s7_double mus_ ## Type ## _dv(void *o)				\
  {									\
    mus_xen *gn = (mus_xen *)o;						\
    return(Func(gn->gen));						\
  }									\
  static s7_double mus_ ## Type ## _dp(s7_pointer p)			\
  {									\
    mus_xen *gn;							\
    gn = (mus_xen *)s7_c_object_value(p);				\
    return(Func(gn->gen));						\
  }

#define GEN_2(Type, Func1, Func2)					\
  static bool is_ ## Type ## _b(s7_pointer p)				\
  {									\
    return((mus_is_xen(p)) && (mus_is_ ## Type(Xen_to_mus_any(p))));	\
  }									\
  static s7_double mus_ ## Type ## _dv(void *o)				\
  {									\
    mus_xen *gn = (mus_xen *)o;						\
    return(Func1(gn->gen));						\
  }									\
  static s7_double mus_ ## Type ## _dp(s7_pointer p)			\
  {									\
    mus_xen *gn;							\
    gn = (mus_xen *)s7_c_object_value(p);				\
    return(Func1(gn->gen));						\
  }									\
  static s7_double mus_ ## Type ## _dvd(void *o, s7_double d)		\
  {									\
    mus_xen *gn = (mus_xen *)o;						\
    return(Func2(gn->gen, d));						\
  }									\
  static s7_double mus_ ## Type ## _dpd(s7_pointer p, s7_double d)	\
  {									\
    mus_xen *gn = (mus_xen *)s7_c_object_value(p);			\
    return(Func2(gn->gen, d));						\
  }

#define GEN_3(Type, Func1, Func2, Func3)				\
  static bool is_ ## Type ## _b(s7_pointer p)				\
  {									\
    return((mus_is_xen(p)) && (mus_is_ ## Type(Xen_to_mus_any(p))));	\
  }									\
  static s7_double mus_ ## Type ## _dv(void *o)				\
  {									\
    mus_xen *gn = (mus_xen *)o;						\
    return(Func1(gn->gen));						\
  }									\
  static s7_double mus_ ## Type ## _dp(s7_pointer p)			\
  {									\
    mus_xen *gn;							\
    gn = (mus_xen *)s7_c_object_value(p);				\
    return(Func1(gn->gen));						\
  }									\
  static s7_double mus_ ## Type ## _dvd(void *o, s7_double d)		\
  {									\
    mus_xen *gn = (mus_xen *)o;						\
    return(Func2(gn->gen, d));						\
  }									\
  static s7_double mus_ ## Type ## _dpd(s7_pointer p, s7_double d)	\
  {									\
    mus_xen *gn = (mus_xen *)s7_c_object_value(p);			\
    return(Func2(gn->gen, d));						\
  }									\
  static s7_double mus_ ## Type ## _dvdd(void *o, s7_double x1, s7_double x2)		\
  {									\
    mus_xen *gn = (mus_xen *)o;						\
    return(Func3(gn->gen, x1, x2));					\
  }

static mus_float_t mus_one_pole_0(mus_any *p) {return(mus_one_pole(p, 0.0));}
static mus_float_t mus_two_pole_0(mus_any *p) {return(mus_two_pole(p, 0.0));}
static mus_float_t mus_one_zero_0(mus_any *p) {return(mus_one_zero(p, 0.0));}
static mus_float_t mus_two_zero_0(mus_any *p) {return(mus_two_zero(p, 0.0));}
static mus_float_t mus_delay_0(mus_any *p) {return(mus_delay_unmodulated(p, 0.0));}
static mus_float_t mus_comb_0(mus_any *p) {return(mus_comb_unmodulated(p, 0.0));}
static mus_float_t mus_comb_bank_0(mus_any *p) {return(mus_comb_bank(p, 0.0));}
static mus_float_t mus_all_pass_bank_0(mus_any *p) {return(mus_all_pass_bank(p, 0.0));}
static mus_float_t mus_notch_0(mus_any *p) {return(mus_notch_unmodulated(p, 0.0));}
static mus_float_t mus_all_pass_0(mus_any *p) {return(mus_all_pass_unmodulated(p, 0.0));}
static mus_float_t mus_one_pole_all_pass_0(mus_any *p) {return(mus_one_pole_all_pass(p, 0.0));}
static mus_float_t mus_moving_average_0(mus_any *p) {return(mus_moving_average(p, 0.0));}
static mus_float_t mus_moving_max_0(mus_any *p) {return(mus_moving_max(p, 0.0));}
static mus_float_t mus_moving_norm_0(mus_any *p) {return(mus_moving_norm(p, 0.0));}
static mus_float_t mus_filter_0(mus_any *p) {return(mus_filter(p, 0.0));}
static mus_float_t mus_fir_filter_0(mus_any *p) {return(mus_fir_filter(p, 0.0));}
static mus_float_t mus_iir_filter_0(mus_any *p) {return(mus_iir_filter(p, 0.0));}
static mus_float_t mus_polyshape_0(mus_any *p) {return(mus_polyshape_unmodulated(p, 1.0));}
static mus_float_t mus_filtered_comb_0(mus_any *p) {return(mus_filtered_comb_unmodulated(p, 0.0));}
static mus_float_t mus_filtered_comb_bank_0(mus_any *p) {return(mus_filtered_comb_bank(p, 0.0));}
static mus_float_t mus_asymmetric_fm_0(mus_any *p) {return(mus_asymmetric_fm_unmodulated(p, 0.0));}
static mus_float_t mus_formant_0(mus_any *p) {return(mus_formant(p, 0.0));}
static mus_float_t mus_firmant_0(mus_any *p) {return(mus_firmant(p, 0.0));}
static mus_float_t mus_ssb_am_0(mus_any *p) {return(mus_ssb_am(p, 0.0, 0.0));}

GEN_3(all_pass, mus_all_pass_0, mus_all_pass_unmodulated, mus_all_pass)
GEN_2(asymmetric_fm, mus_asymmetric_fm_0, mus_asymmetric_fm_unmodulated)
GEN_3(comb, mus_comb_0, mus_comb_unmodulated, mus_comb)
GEN_2(comb_bank, mus_comb_bank_0, mus_comb_bank)
GEN_2(all_pass_bank, mus_all_pass_bank_0, mus_all_pass_bank)
GEN_1(convolve, mus_convolve_simple)
GEN_3(delay, mus_delay_0, mus_delay_unmodulated, mus_delay)
GEN_1(env, mus_env)
GEN_2(filter, mus_filter_0, mus_filter)
GEN_2(filtered_comb, mus_filtered_comb_0, mus_filtered_comb_unmodulated)
GEN_2(filtered_comb_bank, mus_filtered_comb_bank_0, mus_filtered_comb_bank)
GEN_2(fir_filter, mus_fir_filter_0, mus_fir_filter)
GEN_3(firmant, mus_firmant_0, mus_firmant, mus_firmant_with_frequency)
GEN_3(formant, mus_formant_0, mus_formant, mus_formant_with_frequency)
/* GEN_1(granulate, mus_granulate_simple) */
GEN_2(iir_filter, mus_iir_filter_0, mus_iir_filter)
GEN_2(moving_average, mus_moving_average_0, mus_moving_average)
GEN_2(moving_max, mus_moving_max_0, mus_moving_max)
GEN_2(moving_norm, mus_moving_norm_0, mus_moving_norm)
GEN_2(ncos, mus_ncos_unmodulated, mus_ncos)
GEN_3(notch, mus_notch_0, mus_notch_unmodulated, mus_notch)
GEN_2(nrxycos, mus_nrxycos_unmodulated, mus_nrxycos)
GEN_2(nrxysin, mus_nrxysin_unmodulated, mus_nrxysin)
GEN_2(nsin, mus_nsin_unmodulated, mus_nsin)
GEN_2(one_pole, mus_one_pole_0, mus_one_pole)
GEN_2(one_pole_all_pass, mus_one_pole_all_pass_0, mus_one_pole_all_pass)
GEN_2(one_zero, mus_one_zero_0, mus_one_zero)
GEN_3(oscil, mus_oscil_unmodulated, mus_oscil_fm, mus_oscil)
GEN_1(oscil_bank, mus_oscil_bank)
/* GEN_1(phase_vocoder, mus_phase_vocoder_simple) */
GEN_2(polyshape, mus_polyshape_0, mus_polyshape_unmodulated)
GEN_2(polywave, mus_polywave_unmodulated, mus_polywave)
GEN_2(pulse_train, mus_pulse_train_unmodulated, mus_pulse_train)
GEN_2(pulsed_env, mus_pulsed_env_unmodulated, mus_pulsed_env)
GEN_2(rand, mus_rand_unmodulated, mus_rand)
GEN_2(rand_interp, mus_rand_interp_unmodulated, mus_rand_interp)
GEN_1(readin, mus_readin)
GEN_2(rxykcos, mus_rxykcos_unmodulated, mus_rxykcos)
GEN_2(rxyksin, mus_rxyksin_unmodulated, mus_rxyksin)
GEN_2(sawtooth_wave, mus_sawtooth_wave_unmodulated, mus_sawtooth_wave)
GEN_2(square_wave, mus_square_wave_unmodulated, mus_square_wave)
GEN_2(src, mus_src_simple, mus_src_two)
GEN_2(table_lookup, mus_table_lookup_unmodulated, mus_table_lookup)
GEN_2(triangle_wave, mus_triangle_wave_unmodulated, mus_triangle_wave)
GEN_2(two_pole, mus_two_pole_0, mus_two_pole)
GEN_2(two_zero, mus_two_zero_0, mus_two_zero)
GEN_2(wave_train, mus_wave_train_unmodulated, mus_wave_train)
GEN_3(ssb_am, mus_ssb_am_0, mus_ssb_am_unmodulated, mus_ssb_am)
GEN_2(tap, mus_tap_unmodulated, mus_tap)

/* granulate and phase-vocoder are omitted because their editing functions can
 *   involve loops, causing the optimizer to step on itself.  Ideally, and
 *   maybe eventually, the optimized program would be sequestered, but for
 *   now these two generators will run slower.  One quick fix would be to 
 *   have a simple-granulate|phase-vocoder that had no internal lambdas.
 *   All we need is the name.  (Also I'm assuming that if src|convolve
 *   have input functions, they won't involve loops -- here also we really
 *   need a simple version of the generator).
 */

static s7_double file_to_sample_d7pi(s7_scheme *sc, s7_pointer p, s7_int index)
{
  mus_any *g = NULL;
  mus_xen *gn;
  Xen_to_C_generator(p, gn, g, mus_is_file_to_sample, S_file_to_sample, "a file->sample generator");
  return(mus_file_to_sample(g, index, 0));			
}

static s7_double outa_did(s7_int pos, s7_double x)
{
  out_any_2(pos, x, 0, S_outa);
  return(x);
}

static s7_double outb_did(s7_int pos, s7_double x)
{
  out_any_2(pos, x, 1, S_outb);
  return(x);
}

static s7_double outc_did(s7_int pos, s7_double x)
{
  out_any_2(pos, x, 2, S_outc);
  return(x);
}

static s7_double outd_did(s7_int pos, s7_double x)
{
  out_any_2(pos, x, 3, S_outd);
  return(x);
}

#if 0
/* need s7_d_idi_t */
static s7_double out_any_did(s7_int pos, s7_double x, s7_int i)
{
  out_any_2(pos, x, i, S_out_any);
  return(x);
}
#endif

static s7_double ina_dip(s7_int pos, s7_pointer p)
{
  return(in_any_3(S_ina, pos, 0, p));
}

static s7_double inb_dip(s7_int pos, s7_pointer p)
{
  return(in_any_3(S_inb, pos, 1, p));
}


static s7_double locsig_d_vid(void *obj, s7_int ind, s7_double x)
{
  mus_xen *gn = (mus_xen *)obj;
  mus_locsig(gn->gen, ind, x); /* clm.c's mus_locsig is a void func? */
  return(x);
}

static s7_double locsig_set_d_vid(void *obj, s7_int ind, s7_double x)
{
  mus_xen *gn = (mus_xen *)obj;
  mus_locsig_set(gn->gen, ind, x); /* clm.c's mus_locsig is a void func? */
  return(x);
}


static s7_double mus_formant_bank_dvd(void *o, s7_double x)
{
  mus_xen *gn = (mus_xen *)o;
  return(mus_formant_bank(gn->gen, x));
}

static s7_double mus_formant_bank_dpd(s7_pointer p, s7_double x)
{
  mus_xen *gn = (mus_xen *)s7_c_object_value(p);
  return(mus_formant_bank(gn->gen, x));
}

static s7_double mus_formant_bank_dv(void *o)
{
  mus_xen *gn = (mus_xen *)o;
  return(mus_formant_bank(gn->gen, 0.0));
}

static s7_double mus_formant_bank_dp(s7_pointer p)
{
  mus_xen *gn = (mus_xen *)s7_c_object_value(p);
  return(mus_formant_bank(gn->gen, 0.0));
}

static s7_double mus_set_formant_frequency_dvd(void *o, s7_double x)
{
  mus_xen *gn = (mus_xen *)o;
  return(mus_set_formant_frequency(gn->gen, x));
}

static s7_double mus_set_formant_frequency_dpd(s7_pointer p, s7_double x)
{
  mus_xen *gn = (mus_xen *)s7_c_object_value(p);
  return(mus_set_formant_frequency(gn->gen, x));
}

static s7_double mus_set_formant_radius_and_frequency_dvdd(void *o, s7_double x1, s7_double x2)
{
  mus_xen *gn = (mus_xen *)o;
  mus_set_formant_radius_and_frequency(gn->gen, x1, x2);
  return(x2);
}


static s7_double out_bank_d_7pid(s7_scheme *sc, s7_pointer gens, s7_int loc, s7_double x)
{
  int i, len;
  s7_pointer *els;
  els = s7_vector_elements(gens);
  len = s7_vector_length(gens);
  for (i = 0; i < len; i++)
    out_any_2(loc, mus_apply(((mus_xen *)(s7_c_object_value(els[i])))->gen, x, 0.0), i, S_out_bank);
  return(x);
}

static s7_double polynomial_d_pd(s7_pointer v, s7_double x)
{
  return(mus_polynomial(s7_float_vector_elements(v), x, s7_vector_length(v)));
}

static s7_double array_interp_d_pd(s7_pointer v, s7_double x)
{
  return(mus_array_interp(s7_float_vector_elements(v), x, s7_vector_length(v)));
}


#define DF_1(Call) static s7_double mus_ ## Call ## _d(s7_double x) {return((s7_double)mus_ ## Call((mus_float_t)x));}
#define DF_2(Call) static s7_double mus_ ## Call ## _d(s7_double x1, s7_double x2) {return((s7_double)mus_ ## Call((mus_float_t)x1, (mus_float_t)x2));}

DF_1(odd_weight)
DF_1(even_weight)
DF_1(hz_to_radians)
DF_1(radians_to_hz)
DF_1(db_to_linear)
DF_1(linear_to_db)
DF_1(radians_to_degrees)
DF_1(degrees_to_radians)
DF_1(random)

static s7_double mus_hz_to_radians_d_p(s7_pointer x) {return((s7_double)mus_hz_to_radians(s7_number_to_real(s7, x)));}

DF_2(contrast_enhancement)
DF_2(odd_multiple)
DF_2(even_multiple)
DF_2(ring_modulate)


static void init_choosers(s7_scheme *sc)
{
  sym_frequency = s7_make_symbol(sc, S_mus_frequency);
  sym_phase = s7_make_symbol(sc, S_mus_phase);
  sym_scaler = s7_make_symbol(sc, S_mus_scaler);
  sym_increment = s7_make_symbol(sc, S_mus_increment);
  sym_width = s7_make_symbol(sc, S_mus_width);
  sym_offset = s7_make_symbol(sc, S_mus_offset);
  sym_feedforward = s7_make_symbol(sc, S_mus_feedforward);
  sym_feedback = s7_make_symbol(sc, S_mus_feedback);

  mus_copy_symbol = s7_make_symbol(sc, "mus-copy");
  copy_function = s7_name_to_value(sc, "copy");

  s7_set_d_function(sc, s7_name_to_value(sc, S_mus_srate), mus_srate);
  s7_set_d_function(sc, s7_name_to_value(sc, S_mus_float_equal_fudge_factor), mus_float_equal_fudge_factor);

  s7_set_d_v_function(sc, s7_name_to_value(sc, S_all_pass), mus_all_pass_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_all_pass_bank), mus_all_pass_bank_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_asymmetric_fm), mus_asymmetric_fm_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_comb), mus_comb_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_comb_bank), mus_comb_bank_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_convolve), mus_convolve_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_delay), mus_delay_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_env), mus_env_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_filter), mus_filter_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_filtered_comb), mus_filtered_comb_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_filtered_comb_bank), mus_filtered_comb_bank_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_fir_filter), mus_fir_filter_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_firmant), mus_firmant_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_formant), mus_formant_dv);
  /* s7_set_d_v_function(sc, s7_name_to_value(sc, S_granulate), mus_granulate_dv); */
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_iir_filter), mus_iir_filter_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_moving_average), mus_moving_average_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_moving_max), mus_moving_max_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_moving_norm), mus_moving_norm_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_ncos), mus_ncos_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_notch), mus_notch_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_nrxycos), mus_nrxycos_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_nrxysin), mus_nrxysin_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_nsin), mus_nsin_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_one_pole), mus_one_pole_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_one_pole_all_pass), mus_one_pole_all_pass_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_one_zero), mus_one_zero_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_oscil), mus_oscil_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_oscil_bank), mus_oscil_bank_dv);
  /* s7_set_d_v_function(sc, s7_name_to_value(sc, S_phase_vocoder), mus_phase_vocoder_dv); */
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_polyshape), mus_polyshape_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_polywave), mus_polywave_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_pulse_train), mus_pulse_train_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_pulsed_env), mus_pulsed_env_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_rand), mus_rand_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_rand_interp), mus_rand_interp_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_readin), mus_readin_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_rxykcos), mus_rxykcos_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_rxyksin), mus_rxyksin_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_sawtooth_wave), mus_sawtooth_wave_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_square_wave), mus_square_wave_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_src), mus_src_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_ssb_am), mus_ssb_am_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_table_lookup), mus_table_lookup_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_tap), mus_tap_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_triangle_wave), mus_triangle_wave_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_two_pole), mus_two_pole_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_two_zero), mus_two_zero_dv);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_wave_train), mus_wave_train_dv);

  s7_set_d_p_function(sc, s7_name_to_value(sc, S_all_pass), mus_all_pass_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_all_pass_bank), mus_all_pass_bank_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_asymmetric_fm), mus_asymmetric_fm_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_comb), mus_comb_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_comb_bank), mus_comb_bank_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_convolve), mus_convolve_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_delay), mus_delay_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_env), mus_env_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_filter), mus_filter_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_filtered_comb), mus_filtered_comb_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_filtered_comb_bank), mus_filtered_comb_bank_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_fir_filter), mus_fir_filter_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_firmant), mus_firmant_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_formant), mus_formant_dp);
  /* s7_set_d_p_function(sc, s7_name_to_value(sc, S_granulate), mus_granulate_dp); */
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_iir_filter), mus_iir_filter_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_moving_average), mus_moving_average_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_moving_max), mus_moving_max_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_moving_norm), mus_moving_norm_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_ncos), mus_ncos_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_notch), mus_notch_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_nrxycos), mus_nrxycos_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_nrxysin), mus_nrxysin_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_nsin), mus_nsin_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_one_pole), mus_one_pole_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_one_pole_all_pass), mus_one_pole_all_pass_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_one_zero), mus_one_zero_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_oscil), mus_oscil_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_oscil_bank), mus_oscil_bank_dp);
  /* s7_set_d_p_function(sc, s7_name_to_value(sc, S_phase_vocoder), mus_phase_vocoder_dp); */
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_polyshape), mus_polyshape_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_polywave), mus_polywave_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_pulse_train), mus_pulse_train_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_pulsed_env), mus_pulsed_env_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_rand), mus_rand_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_rand_interp), mus_rand_interp_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_readin), mus_readin_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_rxykcos), mus_rxykcos_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_rxyksin), mus_rxyksin_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_sawtooth_wave), mus_sawtooth_wave_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_square_wave), mus_square_wave_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_src), mus_src_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_ssb_am), mus_ssb_am_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_table_lookup), mus_table_lookup_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_tap), mus_tap_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_triangle_wave), mus_triangle_wave_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_two_pole), mus_two_pole_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_two_zero), mus_two_zero_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_wave_train), mus_wave_train_dp);

  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_all_pass), mus_all_pass_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_all_pass_bank), mus_all_pass_bank_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_asymmetric_fm), mus_asymmetric_fm_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_comb), mus_comb_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_comb_bank), mus_comb_bank_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_delay), mus_delay_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_filter), mus_filter_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_filtered_comb), mus_filtered_comb_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_filtered_comb_bank), mus_filtered_comb_bank_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_fir_filter), mus_fir_filter_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_firmant), mus_firmant_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_formant), mus_formant_dvd);
  s7_set_d_v_function(sc, s7_name_to_value(sc, S_formant_bank), mus_formant_bank_dv);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_formant_bank), mus_formant_bank_dp);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_formant_bank), mus_formant_bank_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_iir_filter), mus_iir_filter_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_moving_average), mus_moving_average_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_moving_max), mus_moving_max_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_moving_norm), mus_moving_norm_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_ncos), mus_ncos_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_notch), mus_notch_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_nrxycos), mus_nrxycos_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_nrxysin), mus_nrxysin_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_nsin), mus_nsin_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_one_pole), mus_one_pole_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_one_pole_all_pass), mus_one_pole_all_pass_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_one_zero), mus_one_zero_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_oscil), mus_oscil_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_polyshape), mus_polyshape_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_polywave), mus_polywave_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_pulse_train), mus_pulse_train_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_pulsed_env), mus_pulsed_env_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_rand), mus_rand_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_rand_interp), mus_rand_interp_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_rxykcos), mus_rxykcos_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_rxyksin), mus_rxyksin_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_sawtooth_wave), mus_sawtooth_wave_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_square_wave), mus_square_wave_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_src), mus_src_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_ssb_am), mus_ssb_am_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_table_lookup), mus_table_lookup_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_tap), mus_tap_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_triangle_wave), mus_triangle_wave_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_two_pole), mus_two_pole_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_two_zero), mus_two_zero_dvd);
  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_wave_train), mus_wave_train_dvd);

  s7_set_d_vd_function(sc, s7_name_to_value(sc, S_mus_set_formant_frequency), mus_set_formant_frequency_dvd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_mus_set_formant_radius_and_frequency), mus_set_formant_radius_and_frequency_dvdd);

  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_all_pass), mus_all_pass_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_all_pass_bank), mus_all_pass_bank_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_asymmetric_fm), mus_asymmetric_fm_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_comb), mus_comb_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_comb_bank), mus_comb_bank_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_delay), mus_delay_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_filter), mus_filter_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_filtered_comb), mus_filtered_comb_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_filtered_comb_bank), mus_filtered_comb_bank_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_fir_filter), mus_fir_filter_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_firmant), mus_firmant_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_formant), mus_formant_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_formant_bank), mus_formant_bank_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_iir_filter), mus_iir_filter_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_moving_average), mus_moving_average_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_moving_max), mus_moving_max_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_moving_norm), mus_moving_norm_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_ncos), mus_ncos_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_notch), mus_notch_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_nrxycos), mus_nrxycos_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_nrxysin), mus_nrxysin_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_nsin), mus_nsin_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_one_pole), mus_one_pole_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_one_pole_all_pass), mus_one_pole_all_pass_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_one_zero), mus_one_zero_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_oscil), mus_oscil_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_polyshape), mus_polyshape_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_polywave), mus_polywave_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_pulse_train), mus_pulse_train_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_pulsed_env), mus_pulsed_env_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_rand), mus_rand_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_rand_interp), mus_rand_interp_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_rxykcos), mus_rxykcos_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_rxyksin), mus_rxyksin_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_sawtooth_wave), mus_sawtooth_wave_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_square_wave), mus_square_wave_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_src), mus_src_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_ssb_am), mus_ssb_am_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_table_lookup), mus_table_lookup_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_tap), mus_tap_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_triangle_wave), mus_triangle_wave_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_two_pole), mus_two_pole_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_two_zero), mus_two_zero_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_wave_train), mus_wave_train_dpd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_mus_set_formant_frequency), mus_set_formant_frequency_dpd);

  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_oscil), mus_oscil_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_all_pass), mus_all_pass_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_comb), mus_comb_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_notch), mus_notch_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_ssb_am), mus_ssb_am_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_formant), mus_formant_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_firmant), mus_firmant_dvdd);
  s7_set_d_vdd_function(sc, s7_name_to_value(sc, S_delay), mus_delay_dvdd);

  s7_set_d_d_function(sc, s7_name_to_value(sc, S_odd_weight), mus_odd_weight_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_even_weight), mus_even_weight_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_hz_to_radians), mus_hz_to_radians_d);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_hz_to_radians), mus_hz_to_radians_d_p);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_radians_to_hz), mus_radians_to_hz_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_db_to_linear), mus_db_to_linear_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_linear_to_db), mus_linear_to_db_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_radians_to_degrees), mus_radians_to_degrees_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_degrees_to_radians), mus_degrees_to_radians_d);
  s7_set_d_d_function(sc, s7_name_to_value(sc, S_mus_random), mus_random_d);

  s7_set_d_dd_function(sc, s7_name_to_value(sc, S_even_multiple), mus_even_multiple_d);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, S_odd_multiple), mus_odd_multiple_d);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, S_ring_modulate), mus_ring_modulate_d);
  s7_set_d_dd_function(sc, s7_name_to_value(sc, S_contrast_enhancement), mus_contrast_enhancement_d);

  s7_set_d_id_function(sc, s7_name_to_value(sc, S_outa), outa_did);
  s7_set_d_id_function(sc, s7_name_to_value(sc, S_outb), outb_did);
  s7_set_d_id_function(sc, s7_name_to_value(sc, S_outc), outc_did);
  s7_set_d_id_function(sc, s7_name_to_value(sc, S_outd), outd_did);

  s7_set_d_ip_function(sc, s7_name_to_value(sc, S_ina), ina_dip);
  s7_set_d_ip_function(sc, s7_name_to_value(sc, S_inb), inb_dip);

  s7_set_d_7pi_function(sc, s7_name_to_value(sc, S_file_to_sample), file_to_sample_d7pi);

  s7_set_d_p_function(sc, s7_name_to_value(sc, S_pink_noise), mus_pink_noise);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_piano_noise), piano_noise_d_pd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_polynomial), polynomial_d_pd);
  s7_set_d_pd_function(sc, s7_name_to_value(sc, S_array_interp), array_interp_d_pd);

  s7_set_d_vid_function(sc, s7_name_to_value(sc, S_locsig), locsig_d_vid);
  s7_set_d_vid_function(sc, s7_name_to_value(sc, S_locsig_set), locsig_set_d_vid);

  s7_set_d_7pid_function(sc, s7_name_to_value(sc, S_out_bank), out_bank_d_7pid);

  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_all_pass), is_all_pass_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_asymmetric_fm), is_asymmetric_fm_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_comb), is_comb_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_comb_bank), is_comb_bank_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_all_pass_bank), is_all_pass_bank_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_convolve), is_convolve_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_delay), is_delay_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_env), is_env_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_filter), is_filter_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_filtered_comb), is_filtered_comb_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_filtered_comb_bank), is_filtered_comb_bank_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_fir_filter), is_fir_filter_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_firmant), is_firmant_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_formant), is_formant_b);
  /* s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_granulate), is_granulate_b); */
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_iir_filter), is_iir_filter_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_moving_average), is_moving_average_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_moving_max), is_moving_max_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_moving_norm), is_moving_norm_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_ncos), is_ncos_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_notch), is_notch_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_nrxycos), is_nrxycos_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_nrxysin), is_nrxysin_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_nsin), is_nsin_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_one_pole), is_one_pole_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_one_pole_all_pass), is_one_pole_all_pass_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_one_zero), is_one_zero_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_oscil), is_oscil_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_oscil_bank), is_oscil_bank_b);
  /* s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_phase_vocoder), is_phase_vocoder_b); */
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_polyshape), is_polyshape_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_polywave), is_polywave_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_pulse_train), is_pulse_train_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_pulsed_env), is_pulsed_env_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_rand), is_rand_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_rand_interp), is_rand_interp_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_readin), is_readin_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_rxykcos), is_rxykcos_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_rxyksin), is_rxyksin_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_sawtooth_wave), is_sawtooth_wave_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_square_wave), is_square_wave_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_src), is_src_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_table_lookup), is_table_lookup_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_triangle_wave), is_triangle_wave_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_two_pole), is_two_pole_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_two_zero), is_two_zero_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_wave_train), is_wave_train_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_ssb_am), is_ssb_am_b);
  s7_set_b_p_function(sc, s7_name_to_value(sc, S_is_tap), is_tap_b);

  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_scaler), mus_scaler_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_phase), mus_phase_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_frequency), mus_frequency_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_offset), mus_offset_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_width), mus_width_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_increment), mus_increment_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_feedforward), mus_feedforward_dp);
  s7_set_d_p_function(sc, s7_name_to_value(sc, S_mus_feedback), mus_feedback_dp);

  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_length), mus_length_ip);
  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_order), mus_order_ip);
  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_location), mus_location_ip);
  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_channel), mus_channel_ip);
  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_channels), mus_channels_ip);
  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_ramp), mus_ramp_ip);
  s7_set_i_7p_function(sc, s7_name_to_value(sc, S_mus_hop), mus_hop_ip);
}
#endif /*s7 */


Xen_wrap_no_args(g_mus_srate_w, g_mus_srate)
Xen_wrap_1_arg(g_mus_set_srate_w, g_mus_set_srate)
Xen_wrap_no_args(g_mus_float_equal_fudge_factor_w, g_mus_float_equal_fudge_factor)
Xen_wrap_1_arg(g_mus_set_float_equal_fudge_factor_w, g_mus_set_float_equal_fudge_factor)
Xen_wrap_no_args(g_mus_array_print_length_w, g_mus_array_print_length)
Xen_wrap_1_arg(g_mus_set_array_print_length_w, g_mus_set_array_print_length)
Xen_wrap_1_arg(g_radians_to_hz_w, g_radians_to_hz)
Xen_wrap_1_arg(g_hz_to_radians_w, g_hz_to_radians)
Xen_wrap_1_arg(g_radians_to_degrees_w, g_radians_to_degrees)
Xen_wrap_1_arg(g_degrees_to_radians_w, g_degrees_to_radians)
Xen_wrap_1_arg(g_db_to_linear_w, g_db_to_linear)
Xen_wrap_1_arg(g_linear_to_db_w, g_linear_to_db)
Xen_wrap_1_arg(g_even_weight_w, g_even_weight)
Xen_wrap_1_arg(g_odd_weight_w, g_odd_weight)
Xen_wrap_2_args(g_even_multiple_w, g_even_multiple)
Xen_wrap_2_args(g_odd_multiple_w, g_odd_multiple)
Xen_wrap_1_arg(g_seconds_to_samples_w, g_seconds_to_samples)
Xen_wrap_1_arg(g_samples_to_seconds_w, g_samples_to_seconds)
Xen_wrap_2_args(g_ring_modulate_w, g_ring_modulate)
Xen_wrap_3_args(g_amplitude_modulate_w, g_amplitude_modulate)
Xen_wrap_2_optional_args(g_contrast_enhancement_w, g_contrast_enhancement)
Xen_wrap_3_optional_args(g_dot_product_w, g_dot_product)
#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS && (!HAVE_RUBY)
Xen_wrap_2_args(g_edot_product_w, g_edot_product)
#endif
Xen_wrap_2_args(g_polynomial_w, g_polynomial)
Xen_wrap_4_optional_args(g_make_fft_window_w, g_make_fft_window)
Xen_wrap_4_optional_args(g_mus_fft_w, g_mus_fft)
Xen_wrap_4_optional_args(g_spectrum_w, g_spectrum)
Xen_wrap_1_arg(g_autocorrelate_w, g_autocorrelate)
Xen_wrap_2_args(g_correlate_w, g_correlate)
Xen_wrap_3_optional_args(g_convolution_w, g_convolution)
Xen_wrap_2_args(g_rectangular_to_polar_w, g_rectangular_to_polar)
Xen_wrap_2_args(g_rectangular_to_magnitudes_w, g_rectangular_to_magnitudes)
Xen_wrap_2_args(g_polar_to_rectangular_w, g_polar_to_rectangular)
Xen_wrap_3_optional_args(g_array_interp_w, g_array_interp)
Xen_wrap_5_optional_args(g_mus_interpolate_w, g_mus_interpolate)
Xen_wrap_1_arg(g_mus_describe_w, g_mus_describe)
Xen_wrap_1_arg(g_mus_name_w, g_mus_name)
Xen_wrap_3_optional_args(g_mus_run_w, g_mus_run)
Xen_wrap_1_arg(g_mus_phase_w, g_mus_phase)
Xen_wrap_2_args(g_mus_set_phase_w, g_mus_set_phase)
Xen_wrap_1_arg(g_mus_width_w, g_mus_width)
Xen_wrap_2_args(g_mus_set_width_w, g_mus_set_width)
Xen_wrap_1_arg(g_mus_scaler_w, g_mus_scaler)
Xen_wrap_2_args(g_mus_set_scaler_w, g_mus_set_scaler)
Xen_wrap_1_arg(g_mus_feedforward_w, g_mus_feedforward)
Xen_wrap_2_args(g_mus_set_feedforward_w, g_mus_set_feedforward)
Xen_wrap_1_arg(g_mus_reset_w, g_mus_reset)
Xen_wrap_1_arg(g_mus_copy_w, g_mus_copy)
Xen_wrap_1_arg(g_mus_offset_w, g_mus_offset)
Xen_wrap_2_args(g_mus_set_offset_w, g_mus_set_offset)
Xen_wrap_1_arg(g_mus_frequency_w, g_mus_frequency)
Xen_wrap_2_args(g_mus_set_frequency_w, g_mus_set_frequency)
Xen_wrap_1_arg(g_mus_length_w, g_mus_length)
Xen_wrap_1_arg(g_mus_file_name_w, g_mus_file_name)
Xen_wrap_2_args(g_mus_set_length_w, g_mus_set_length)
Xen_wrap_1_arg(g_mus_type_w, g_mus_type)
Xen_wrap_1_arg(g_mus_order_w, g_mus_order)
Xen_wrap_1_arg(g_mus_data_w, g_mus_data)
Xen_wrap_2_args(g_mus_set_data_w, g_mus_set_data)
Xen_wrap_1_arg(g_is_oscil_w, g_is_oscil)
Xen_wrap_3_optional_args(g_oscil_w, g_oscil)
Xen_wrap_1_arg(g_is_oscil_bank_w, g_is_oscil_bank)
Xen_wrap_1_arg(g_oscil_bank_w, g_oscil_bank)
Xen_wrap_any_args(g_mus_apply_w, g_mus_apply)
Xen_wrap_3_optional_args(g_delay_w, g_delay)
Xen_wrap_2_optional_args(g_delay_tick_w, g_delay_tick)
Xen_wrap_2_optional_args(g_tap_w, g_tap)
Xen_wrap_3_optional_args(g_notch_w, g_notch)
Xen_wrap_3_optional_args(g_comb_w, g_comb)
Xen_wrap_3_optional_args(g_filtered_comb_w, g_filtered_comb)
Xen_wrap_3_optional_args(g_all_pass_w, g_all_pass)
Xen_wrap_2_optional_args(g_moving_average_w, g_moving_average)
Xen_wrap_2_optional_args(g_moving_max_w, g_moving_max)
Xen_wrap_2_optional_args(g_moving_norm_w, g_moving_norm)
Xen_wrap_1_arg(g_is_tap_w, g_is_tap)
Xen_wrap_1_arg(g_is_delay_w, g_is_delay)
Xen_wrap_1_arg(g_is_notch_w, g_is_notch)
Xen_wrap_1_arg(g_is_comb_w, g_is_comb)
Xen_wrap_1_arg(g_is_filtered_comb_w, g_is_filtered_comb)
Xen_wrap_1_arg(g_is_all_pass_w, g_is_all_pass)
Xen_wrap_1_arg(g_is_moving_average_w, g_is_moving_average)
Xen_wrap_1_arg(g_is_moving_max_w, g_is_moving_max)
Xen_wrap_1_arg(g_is_moving_norm_w, g_is_moving_norm)

Xen_wrap_2_optional_args(g_ncos_w, g_ncos)
Xen_wrap_1_arg(g_is_ncos_w, g_is_ncos)
Xen_wrap_2_optional_args(g_nsin_w, g_nsin)
Xen_wrap_1_arg(g_is_nsin_w, g_is_nsin)

Xen_wrap_2_optional_args(g_rand_w, g_rand)
Xen_wrap_2_optional_args(g_rand_interp_w, g_rand_interp)
Xen_wrap_1_arg(g_is_rand_w, g_is_rand)
Xen_wrap_1_arg(g_is_rand_interp_w, g_is_rand_interp)
Xen_wrap_1_arg(g_mus_random_w, g_mus_random)
Xen_wrap_no_args(g_mus_rand_seed_w, g_mus_rand_seed)
Xen_wrap_1_arg(g_mus_set_rand_seed_w, g_mus_set_rand_seed)
Xen_wrap_1_arg(g_is_table_lookup_w, g_is_table_lookup)
Xen_wrap_2_optional_args(g_table_lookup_w, g_table_lookup)
Xen_wrap_3_optional_args(g_partials_to_wave_w, g_partials_to_wave)
Xen_wrap_3_optional_args(g_phase_partials_to_wave_w, g_phase_partials_to_wave)
Xen_wrap_2_optional_args(g_sawtooth_wave_w, g_sawtooth_wave)
Xen_wrap_1_arg(g_is_sawtooth_wave_w, g_is_sawtooth_wave)
Xen_wrap_2_optional_args(g_triangle_wave_w, g_triangle_wave)
Xen_wrap_1_arg(g_is_triangle_wave_w, g_is_triangle_wave)
Xen_wrap_2_optional_args(g_square_wave_w, g_square_wave)
Xen_wrap_1_arg(g_is_square_wave_w, g_is_square_wave)
Xen_wrap_2_optional_args(g_pulse_train_w, g_pulse_train)
Xen_wrap_1_arg(g_is_pulse_train_w, g_is_pulse_train)

Xen_wrap_3_args(g_make_pulsed_env_w, g_make_pulsed_env)
Xen_wrap_2_optional_args(g_pulsed_env_w, g_pulsed_env)
Xen_wrap_1_arg(g_is_pulsed_env_w, g_is_pulsed_env)

Xen_wrap_3_optional_args(g_asymmetric_fm_w, g_asymmetric_fm)
Xen_wrap_1_arg(g_is_asymmetric_fm_w, g_is_asymmetric_fm)
Xen_wrap_2_optional_args(g_one_zero_w, g_one_zero)
Xen_wrap_1_arg(g_is_one_zero_w, g_is_one_zero)
Xen_wrap_2_optional_args(g_one_pole_w, g_one_pole)
Xen_wrap_1_arg(g_is_one_pole_w, g_is_one_pole)
Xen_wrap_2_optional_args(g_two_zero_w, g_two_zero)
Xen_wrap_1_arg(g_is_two_zero_w, g_is_two_zero)
Xen_wrap_2_optional_args(g_two_pole_w, g_two_pole)
Xen_wrap_1_arg(g_is_two_pole_w, g_is_two_pole)

Xen_wrap_1_arg(g_is_formant_w, g_is_formant)
Xen_wrap_3_optional_args(g_formant_w, g_formant)

Xen_wrap_2_optional_args(g_formant_bank_w, g_formant_bank)
Xen_wrap_1_arg(g_is_formant_bank_w, g_is_formant_bank)
Xen_wrap_2_optional_args(g_make_formant_bank_w, g_make_formant_bank)

Xen_wrap_1_arg(g_is_firmant_w, g_is_firmant)
Xen_wrap_3_optional_args(g_firmant_w, g_firmant)

Xen_wrap_1_arg(g_is_one_pole_all_pass_w, g_is_one_pole_all_pass)
Xen_wrap_2_args(g_make_one_pole_all_pass_w, g_make_one_pole_all_pass)
Xen_wrap_2_optional_args(g_one_pole_all_pass_w, g_one_pole_all_pass)

Xen_wrap_2_args(g_set_formant_frequency_w, g_set_formant_frequency)
Xen_wrap_3_args(g_set_formant_radius_and_frequency_w, g_set_formant_radius_and_frequency)

Xen_wrap_5_args(g_frample_to_frample_w, g_frample_to_frample)

Xen_wrap_2_optional_args(g_wave_train_w, g_wave_train)
Xen_wrap_1_arg(g_is_wave_train_w, g_is_wave_train)
Xen_wrap_3_optional_args(g_polyshape_w, g_polyshape)
Xen_wrap_1_arg(g_is_polyshape_w, g_is_polyshape)
Xen_wrap_2_optional_args(g_partials_to_polynomial_w, g_partials_to_polynomial)
Xen_wrap_1_arg(g_normalize_partials_w, g_normalize_partials)
Xen_wrap_2_args(g_chebyshev_t_sum_w, g_chebyshev_t_sum)
Xen_wrap_2_args(g_chebyshev_u_sum_w, g_chebyshev_u_sum)
Xen_wrap_3_args(g_chebyshev_tu_sum_w, g_chebyshev_tu_sum)
Xen_wrap_2_optional_args(g_polywave_w, g_polywave)
Xen_wrap_1_arg(g_is_polywave_w, g_is_polywave)

Xen_wrap_2_optional_args(g_nrxysin_w, g_nrxysin)
Xen_wrap_1_arg(g_is_nrxysin_w, g_is_nrxysin)
Xen_wrap_2_optional_args(g_nrxycos_w, g_nrxycos)
Xen_wrap_1_arg(g_is_nrxycos_w, g_is_nrxycos)

Xen_wrap_2_optional_args(g_rxyksin_w, g_rxyksin)
Xen_wrap_1_arg(g_is_rxyksin_w, g_is_rxyksin)
Xen_wrap_2_optional_args(g_rxykcos_w, g_rxykcos)
Xen_wrap_1_arg(g_is_rxykcos_w, g_is_rxykcos)

Xen_wrap_2_optional_args(g_filter_w, g_filter)
Xen_wrap_1_arg(g_is_filter_w, g_is_filter)
Xen_wrap_2_args(g_make_fir_coeffs_w, g_make_fir_coeffs)
Xen_wrap_2_optional_args(g_fir_filter_w, g_fir_filter)
Xen_wrap_1_arg(g_is_fir_filter_w, g_is_fir_filter)
Xen_wrap_2_optional_args(g_iir_filter_w, g_iir_filter)
Xen_wrap_1_arg(g_is_iir_filter_w, g_is_iir_filter)
Xen_wrap_1_arg(g_mus_xcoeffs_w, g_mus_xcoeffs)
Xen_wrap_1_arg(g_mus_ycoeffs_w, g_mus_ycoeffs)
Xen_wrap_2_args(g_mus_xcoeff_w, g_mus_xcoeff)
Xen_wrap_3_args(g_mus_set_xcoeff_w, g_mus_set_xcoeff)
Xen_wrap_2_args(g_mus_ycoeff_w, g_mus_ycoeff)
Xen_wrap_3_args(g_mus_set_ycoeff_w, g_mus_set_ycoeff)
Xen_wrap_1_arg(g_is_env_w, g_is_env)
Xen_wrap_1_arg(g_env_w, g_env)
Xen_wrap_2_args(g_env_interp_w, g_env_interp)
Xen_wrap_3_optional_args(g_envelope_interp_w, g_envelope_interp)
Xen_wrap_2_args(g_env_any_w, g_env_any)
Xen_wrap_1_arg(g_is_file_to_sample_w, g_is_file_to_sample)
Xen_wrap_2_optional_args(g_make_file_to_sample_w, g_make_file_to_sample)
Xen_wrap_3_optional_args(g_file_to_sample_w, g_file_to_sample)
Xen_wrap_1_arg(g_is_sample_to_file_w, g_is_sample_to_file)
Xen_wrap_5_optional_args(g_make_sample_to_file_w, g_make_sample_to_file)
Xen_wrap_1_arg(g_continue_sample_to_file_w, g_continue_sample_to_file)
Xen_wrap_4_args(g_sample_to_file_w, g_sample_to_file)
Xen_wrap_2_args(g_sample_to_file_add_w, g_sample_to_file_add)

Xen_wrap_1_arg(g_is_file_to_frample_w, g_is_file_to_frample)
Xen_wrap_2_optional_args(g_make_file_to_frample_w, g_make_file_to_frample)
Xen_wrap_3_optional_args(g_file_to_frample_w, g_file_to_frample)
Xen_wrap_1_arg(g_continue_frample_to_file_w, g_continue_frample_to_file)
Xen_wrap_1_arg(g_is_frample_to_file_w, g_is_frample_to_file)
Xen_wrap_3_args(g_frample_to_file_w, g_frample_to_file)
Xen_wrap_5_optional_args(g_make_frample_to_file_w, g_make_frample_to_file)


Xen_wrap_1_arg(g_is_mus_input_w, g_is_mus_input)
Xen_wrap_1_arg(g_is_mus_output_w, g_is_mus_output)
Xen_wrap_3_args(g_in_any_w, g_in_any)
Xen_wrap_2_args(g_ina_w, g_ina)
Xen_wrap_2_args(g_inb_w, g_inb)
Xen_wrap_4_optional_args(g_out_any_w, g_out_any)
Xen_wrap_3_optional_args(g_outa_w, g_outa)
Xen_wrap_3_optional_args(g_outb_w, g_outb)
Xen_wrap_3_optional_args(g_outc_w, g_outc)
Xen_wrap_3_optional_args(g_outd_w, g_outd)
Xen_wrap_1_arg(g_mus_close_w, g_mus_close)
Xen_wrap_no_args(g_mus_file_buffer_size_w, g_mus_file_buffer_size)
Xen_wrap_1_arg(g_mus_set_file_buffer_size_w, g_mus_set_file_buffer_size)
Xen_wrap_1_arg(g_is_readin_w, g_is_readin)
Xen_wrap_1_arg(g_readin_w, g_readin)
Xen_wrap_1_arg(g_mus_channel_w, g_mus_channel)
Xen_wrap_1_arg(g_mus_interp_type_w, g_mus_interp_type)
Xen_wrap_1_arg(g_mus_location_w, g_mus_location)
Xen_wrap_2_args(g_mus_set_location_w, g_mus_set_location)
Xen_wrap_1_arg(g_mus_increment_w, g_mus_increment)
Xen_wrap_2_args(g_mus_set_increment_w, g_mus_set_increment)
Xen_wrap_1_arg(g_mus_feedback_w, g_mus_feedback)
Xen_wrap_2_args(g_mus_set_feedback_w, g_mus_set_feedback)
Xen_wrap_1_arg(g_is_locsig_w, g_is_locsig)
Xen_wrap_3_args(g_locsig_w, g_locsig)
Xen_wrap_3_args(g_move_locsig_w, g_move_locsig)
Xen_wrap_no_args(g_locsig_type_w, g_locsig_type)
Xen_wrap_1_arg(g_set_locsig_type_w, g_set_locsig_type)
Xen_wrap_1_arg(g_mus_channels_w, g_mus_channels)
Xen_wrap_2_args(g_locsig_ref_w, g_locsig_ref)
Xen_wrap_2_args(g_locsig_reverb_ref_w, g_locsig_reverb_ref)
Xen_wrap_3_args(g_locsig_set_w, g_locsig_set)
Xen_wrap_3_args(g_locsig_reverb_set_w, g_locsig_reverb_set)
Xen_wrap_1_arg(g_is_move_sound_w, g_is_move_sound)
Xen_wrap_3_args(g_move_sound_w, g_move_sound)
Xen_wrap_3_optional_args(g_make_move_sound_w, g_make_move_sound)
Xen_wrap_no_args(g_mus_clear_sincs_w, g_mus_clear_sincs)
Xen_wrap_1_arg(g_is_src_w, g_is_src)
Xen_wrap_3_optional_args(g_src_w, g_src)
Xen_wrap_1_arg(g_is_granulate_w, g_is_granulate)
Xen_wrap_3_optional_args(g_granulate_w, g_granulate)
Xen_wrap_1_arg(g_mus_ramp_w, g_mus_ramp)
Xen_wrap_2_args(g_mus_set_ramp_w, g_mus_set_ramp)
Xen_wrap_1_arg(g_is_convolve_w, g_is_convolve)
Xen_wrap_2_optional_args(g_convolve_w, g_convolve)
Xen_wrap_4_optional_args(g_convolve_files_w, g_convolve_files)
Xen_wrap_1_arg(g_is_phase_vocoder_w, g_is_phase_vocoder)
Xen_wrap_5_optional_args(g_phase_vocoder_w, g_phase_vocoder)
Xen_wrap_1_arg(g_phase_vocoder_amp_increments_w, g_phase_vocoder_amp_increments)
Xen_wrap_1_arg(g_phase_vocoder_amps_w, g_phase_vocoder_amps)
Xen_wrap_1_arg(g_phase_vocoder_freqs_w, g_phase_vocoder_freqs)
Xen_wrap_1_arg(g_phase_vocoder_phases_w, g_phase_vocoder_phases)
Xen_wrap_1_arg(g_phase_vocoder_phase_increments_w, g_phase_vocoder_phase_increments)
Xen_wrap_1_arg(g_mus_hop_w, g_mus_hop)
Xen_wrap_2_args(g_mus_set_hop_w, g_mus_set_hop)
Xen_wrap_3_optional_args(g_ssb_am_w, g_ssb_am)
Xen_wrap_1_arg(g_is_ssb_am_w, g_is_ssb_am)
Xen_wrap_no_args(g_clm_table_size_w, g_clm_table_size)
Xen_wrap_1_arg(g_set_clm_table_size_w, g_set_clm_table_size)
Xen_wrap_1_arg(g_is_mus_generator_w, g_is_mus_generator)
Xen_wrap_1_arg(g_mus_frandom_w, g_mus_frandom)
Xen_wrap_1_arg(g_mus_irandom_w, g_mus_irandom)
#if (!HAVE_SCHEME)
Xen_wrap_4_optional_args(g_make_oscil_w, g_make_oscil)
Xen_wrap_4_optional_args(g_make_ssb_am_w, g_make_ssb_am)
Xen_wrap_4_optional_args(g_make_ncos_w, g_make_ncos)
Xen_wrap_4_optional_args(g_make_nsin_w, g_make_nsin)
Xen_wrap_8_optional_args(g_make_asymmetric_fm_w, g_make_asymmetric_fm)
Xen_wrap_any_args(g_make_nrxysin_w, g_make_nrxysin)
Xen_wrap_any_args(g_make_nrxycos_w, g_make_nrxycos)
Xen_wrap_any_args(g_make_rxykcos_w, g_make_rxykcos)
Xen_wrap_any_args(g_make_rxyksin_w, g_make_rxyksin)
Xen_wrap_4_optional_args(g_make_one_pole_w, g_make_one_pole)
Xen_wrap_4_optional_args(g_make_one_zero_w, g_make_one_zero)
Xen_wrap_6_optional_args(g_make_two_zero_w, g_make_two_zero)
Xen_wrap_6_optional_args(g_make_two_pole_w, g_make_two_pole)
Xen_wrap_6_optional_args(g_make_filter_w, g_make_filter)
Xen_wrap_4_optional_args(g_make_fir_filter_w, g_make_fir_filter)
Xen_wrap_4_optional_args(g_make_iir_filter_w, g_make_iir_filter)
Xen_wrap_4_optional_args(g_make_formant_w, g_make_formant)
Xen_wrap_4_optional_args(g_make_firmant_w, g_make_firmant)
Xen_wrap_6_optional_args(g_make_sawtooth_wave_w, g_make_sawtooth_wave)
Xen_wrap_6_optional_args(g_make_triangle_wave_w, g_make_triangle_wave)
Xen_wrap_6_optional_args(g_make_square_wave_w, g_make_square_wave)
Xen_wrap_6_optional_args(g_make_pulse_train_w, g_make_pulse_train)
Xen_wrap_any_args(g_make_delay_w, g_make_delay)
Xen_wrap_any_args(g_make_comb_w, g_make_comb)
Xen_wrap_any_args(g_make_filtered_comb_w, g_make_filtered_comb)
Xen_wrap_any_args(g_make_notch_w, g_make_notch)
Xen_wrap_any_args(g_make_all_pass_w, g_make_all_pass)
Xen_wrap_any_args(g_make_moving_average_w, g_make_moving_average)
Xen_wrap_any_args(g_make_moving_max_w, g_make_moving_max)
Xen_wrap_any_args(g_make_moving_norm_w, g_make_moving_norm)
Xen_wrap_any_args(g_make_env_w, g_make_env)
Xen_wrap_any_args(g_make_polywave_w, g_make_polywave)
Xen_wrap_any_args(g_make_polyshape_w, g_make_polyshape)
Xen_wrap_any_args(g_make_readin_w, g_make_readin)
Xen_wrap_any_args(g_make_convolve_w, g_make_convolve)
Xen_wrap_any_args(g_make_rand_w, g_make_rand)
Xen_wrap_any_args(g_make_rand_interp_w, g_make_rand_interp)
Xen_wrap_6_optional_args(g_make_src_w, g_make_src)
Xen_wrap_any_args(g_make_table_lookup_w, g_make_table_lookup)
Xen_wrap_any_args(g_make_wave_train_w, g_make_wave_train)
Xen_wrap_any_args(g_make_phase_vocoder_w, g_make_phase_vocoder)
Xen_wrap_any_args(g_make_granulate_w, g_make_granulate)
Xen_wrap_any_args(g_make_locsig_w, g_make_locsig)
#endif

Xen_wrap_4_optional_args(g_make_oscil_bank_w, g_make_oscil_bank)
Xen_wrap_any_args(g_mus_file_mix_w, g_mus_file_mix)
Xen_wrap_any_args(g_mus_file_mix_with_envs_w, g_mus_file_mix_with_envs)
Xen_wrap_2_optional_args(g_comb_bank_w, g_comb_bank)
Xen_wrap_1_arg(g_is_comb_bank_w, g_is_comb_bank)
Xen_wrap_1_arg(g_make_comb_bank_w, g_make_comb_bank)
Xen_wrap_2_optional_args(g_filtered_comb_bank_w, g_filtered_comb_bank)
Xen_wrap_1_arg(g_is_filtered_comb_bank_w, g_is_filtered_comb_bank)
Xen_wrap_1_arg(g_make_filtered_comb_bank_w, g_make_filtered_comb_bank)
Xen_wrap_2_optional_args(g_all_pass_bank_w, g_all_pass_bank)
Xen_wrap_1_arg(g_is_all_pass_bank_w, g_is_all_pass_bank)
Xen_wrap_1_arg(g_make_all_pass_bank_w, g_make_all_pass_bank)
Xen_wrap_1_arg(g_pink_noise_w, g_pink_noise)
Xen_wrap_3_args(g_out_bank_w, g_out_bank)

#if HAVE_SCHEME
Xen_wrap_2_args(g_piano_noise_w, g_piano_noise)
Xen_wrap_6_args(g_singer_filter_w, g_singer_filter)
Xen_wrap_5_args(g_singer_nose_filter_w, g_singer_nose_filter)

static s7_pointer acc_clm_srate(s7_scheme *sc, s7_pointer args) {return(g_mus_set_srate(s7_cadr(args)));}  
static s7_pointer acc_clm_table_size(s7_scheme *sc, s7_pointer args) {return(g_set_clm_table_size(s7_cadr(args)));}  
static s7_pointer acc_mus_file_buffer_size(s7_scheme *sc, s7_pointer args) {return(g_mus_set_file_buffer_size(s7_cadr(args)));}  
static s7_pointer acc_mus_float_equal_fudge_factor(s7_scheme *sc, s7_pointer args) {return(g_mus_set_float_equal_fudge_factor(s7_cadr(args)));}  
static s7_pointer acc_mus_array_print_length(s7_scheme *sc, s7_pointer args) {return(g_mus_set_array_print_length(s7_cadr(args)));}  

static s7_pointer generator_to_let(s7_scheme *sc, s7_pointer args)
{
  /* this is called upon (object->let <gen>)
   *   ideally it would be in clm.c, local to each generator like other methods, but we depend on the mus_xen vcts
   */
  s7_pointer gen, let;
  mus_any *g;
  mus_xen *gn;

  gen = s7_car(args);
  let = s7_cadr(args);
  gn = (mus_xen *)s7_c_object_value(gen);
  g = gn->gen;  /* gn->nvcts and gn->vcts hold the arrays and functions */

  if (mus_name_exists(g)) 
    s7_varlet(sc, let, s7_make_symbol(sc, "name"), s7_make_string(sc, mus_name(g)));
  if (mus_file_name_exists(g)) 
    s7_varlet(sc, let, s7_make_symbol(sc, "file-name"), s7_make_string(sc, mus_file_name(g)));
  if ((!mus_is_granulate(g)) && (mus_frequency_exists(g)))
    s7_varlet(sc, let, s7_make_symbol(sc, "frequency"), s7_make_real(sc, mus_frequency(g)));
  if ((!mus_is_env(g)) && (mus_phase_exists(g))) 
    s7_varlet(sc, let, s7_make_symbol(sc, "phase"), s7_make_real(sc, mus_phase(g)));

  if (mus_is_oscil_bank(g))
    {
      if (mus_length_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "size"), s7_make_integer(sc, mus_length(g)));
      if (gn->nvcts == 3)  /* let includes gen (the s7 c_object) which includes gn (the clm2xen mus_xen object), so vcts should be safe (GC protected) */
	{
	  s7_varlet(sc, let, s7_make_symbol(sc, "freqs"), gn->vcts[0]);
	  s7_varlet(sc, let, s7_make_symbol(sc, "phases"), gn->vcts[1]);
	  s7_varlet(sc, let, s7_make_symbol(sc, "amps"), gn->vcts[2]);
	}
    }

  if ((mus_is_nsin(g)) || (mus_is_ncos(g)))
    {
      if (mus_length_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "n"), s7_make_integer(sc, mus_length(g)));
    }

  if ((mus_is_nrxysin(g)) || (mus_is_nrxycos(g)) || (mus_is_asymmetric_fm(g)))
    {
      if (mus_length_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "n"), s7_make_integer(sc, mus_length(g)));
      if (mus_scaler_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "r"), s7_make_real(sc, mus_scaler(g)));
      if (mus_offset_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "ratio"), s7_make_real(sc, mus_offset(g)));
    }
  
  if ((mus_is_rxyksin(g)) || (mus_is_rxykcos(g)))
    {
      if (mus_scaler_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "r"), s7_make_real(sc, mus_scaler(g)));
      if (mus_offset_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "ratio"), s7_make_real(sc, mus_offset(g)));
    }

  if ((mus_is_square_wave(g)) || (mus_is_sawtooth_wave(g)) || 
      (mus_is_pulse_train(g)) || (mus_is_triangle_wave(g)))
    {
      if (mus_scaler_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "amplitude"), s7_make_real(sc, mus_scaler(g)));
    }
  
  if (mus_is_fir_filter(g))
    {
      if (mus_length_exists(g)) 
	s7_varlet(sc, let, s7_make_symbol(sc, "order"), s7_make_integer(sc, mus_length(g)));
      if (gn->nvcts == 3)
	s7_varlet(sc, let, s7_make_symbol(sc, "xcoeffs"), gn->vcts[G_FILTER_XCOEFFS]);
    }
  else
    {
      if (mus_is_iir_filter(g))
	{
	  if (mus_length_exists(g)) 
	    s7_varlet(sc, let, s7_make_symbol(sc, "order"), s7_make_integer(sc, mus_length(g)));
	  if (gn->nvcts == 3)
	    s7_varlet(sc, let, s7_make_symbol(sc, "ycoeffs"), gn->vcts[G_FILTER_YCOEFFS]);
	}
      else
	{
	  if (mus_is_filter(g))
	    {
	      if (mus_length_exists(g)) 
		s7_varlet(sc, let, s7_make_symbol(sc, "order"), s7_make_integer(sc, mus_length(g)));
	      if (gn->nvcts == 3)
		{
		  s7_varlet(sc, let, s7_make_symbol(sc, "xcoeffs"), gn->vcts[G_FILTER_XCOEFFS]);
		  s7_varlet(sc, let, s7_make_symbol(sc, "ycoeffs"), gn->vcts[G_FILTER_YCOEFFS]);
		}
	    }
	}
    }

  if ((mus_is_one_zero(g)) && (mus_xcoeffs_exists(g)))
    {
      mus_float_t *xs;
      xs = mus_xcoeffs(g);
      s7_varlet(sc, let, s7_make_symbol(sc, "a0"), s7_make_real(sc, xs[0]));
      s7_varlet(sc, let, s7_make_symbol(sc, "a1"), s7_make_real(sc, xs[1]));
    }
  if (mus_is_two_zero(g))
    {
      if (mus_scaler_exists(g))
	s7_varlet(sc, let, s7_make_symbol(sc, "radius"), s7_make_real(sc, mus_scaler(g)));
      if (mus_xcoeffs_exists(g))
	{
	  mus_float_t *xs;
	  xs = mus_xcoeffs(g);
	  s7_varlet(sc, let, s7_make_symbol(sc, "a0"), s7_make_real(sc, xs[0]));
	  s7_varlet(sc, let, s7_make_symbol(sc, "a1"), s7_make_real(sc, xs[1]));
	  s7_varlet(sc, let, s7_make_symbol(sc, "a2"), s7_make_real(sc, xs[2]));
	}
    }
  if ((mus_is_one_pole(g)) && (mus_xcoeffs_exists(g)) && (mus_ycoeffs_exists(g)))
    {
      mus_float_t *xs, *ys;
      xs = mus_xcoeffs(g);
      ys = mus_ycoeffs(g);
      s7_varlet(sc, let, s7_make_symbol(sc, "a0"), s7_make_real(sc, xs[0]));
      s7_varlet(sc, let, s7_make_symbol(sc, "b1"), s7_make_real(sc, ys[1]));
    }
  if (mus_is_two_pole(g))
    {
      if (mus_scaler_exists(g))
	s7_varlet(sc, let, s7_make_symbol(sc, "radius"), s7_make_real(sc, mus_scaler(g)));
      if ((mus_xcoeffs_exists(g)) && (mus_ycoeffs_exists(g)))
	{
	  mus_float_t *xs, *ys;
	  xs = mus_xcoeffs(g);
	  ys = mus_ycoeffs(g);
	  s7_varlet(sc, let, s7_make_symbol(sc, "a0"), s7_make_real(sc, xs[0]));
	  s7_varlet(sc, let, s7_make_symbol(sc, "b1"), s7_make_real(sc, ys[1]));
	  s7_varlet(sc, let, s7_make_symbol(sc, "b2"), s7_make_real(sc, ys[2]));
	}
    }

  if ((mus_is_formant(g)) || (mus_is_firmant(g)))
    {
      if (mus_scaler_exists(g))
	s7_varlet(sc, let, s7_make_symbol(sc, "radius"), s7_make_real(sc, mus_scaler(g)));
    }
  if (mus_is_formant_bank(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "gens"), gn->vcts[0]);
      if (gn->nvcts == 2)
	s7_varlet(sc, let, s7_make_symbol(sc, "amps"), gn->vcts[1]);
    }

  if ((mus_is_comb(g)) || (mus_is_notch(g)) || (mus_is_filtered_comb(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "scaler"), s7_make_real(sc, (mus_is_notch(g)) ? mus_scaler(g) : mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "length"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "interp"), s7_make_symbol(sc, mus_interp_type_to_string(mus_channels(g))));
      s7_varlet(sc, let, s7_make_symbol(sc, "line"), gn->vcts[0]);
      if ((mus_is_filtered_comb(g)) &&
	  (gn->vcts[1]))
	s7_varlet(sc, let, s7_make_symbol(sc, "filter"), gn->vcts[1]);
    }
  if (mus_is_all_pass(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "feedforward"), s7_make_real(sc, mus_scaler(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "feedback"), s7_make_real(sc, mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "length"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "interp"), s7_make_symbol(sc, mus_interp_type_to_string(mus_channels(g))));
      s7_varlet(sc, let, s7_make_symbol(sc, "line"), gn->vcts[0]);
    }
  if (mus_is_one_pole_all_pass(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "size"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "coeff"), s7_make_real(sc, mus_scaler(g)));
    }
  if ((mus_is_delay(g)) || (mus_is_moving_max(g)) ||
      (mus_is_moving_average(g)) || (mus_is_moving_norm(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "length"), s7_make_integer(sc, mus_length(g)));
      if (mus_is_delay(g))
	s7_varlet(sc, let, s7_make_symbol(sc, "interp"), s7_make_symbol(sc, mus_interp_type_to_string(mus_channels(g))));
      s7_varlet(sc, let, s7_make_symbol(sc, "line"), gn->vcts[0]);
      if ((mus_is_moving_norm(g)) || (mus_is_moving_max(g)))
	{
	  if ((mus_is_moving_norm(g)) && (mus_increment_exists(g)))
	    s7_varlet(sc, let, s7_make_symbol(sc, "norm"), s7_make_real(sc, mus_offset(g)));
	  if (mus_scaler_exists(g))
	    s7_varlet(sc, let, s7_make_symbol(sc, "max"), s7_make_real(sc, mus_scaler(g)));
	}
    }
  
  if ((mus_is_table_lookup(g)) || (mus_is_wave_train(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "size"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "wave"), gn->vcts[0]);
      s7_varlet(sc, let, s7_make_symbol(sc, "interp"), s7_make_symbol(sc, mus_interp_type_to_string(mus_channels(g))));
    }
  if ((mus_is_comb_bank(g)) || (mus_is_all_pass_bank(g)) || (mus_is_filtered_comb_bank(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "size"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "gens"), gn->vcts[0]);
    }

  if (mus_is_readin(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "channel"), s7_make_integer(sc, mus_channel(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "location"), s7_make_integer(sc, mus_location(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "direction"), s7_make_integer(sc, (s7_int)mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "file-size"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "file-channels"), s7_make_integer(sc, mus_channels(g)));
    }
  if ((mus_is_file_to_sample(g)) || (mus_is_file_to_frample(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "file-size"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "file-channels"), s7_make_integer(sc, mus_channels(g)));
    }

  if (mus_is_src(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "input"), gn->vcts[MUS_INPUT_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "srate"), s7_make_real(sc, mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "width"), s7_make_integer(sc, mus_length(g)));
    }

  if (mus_is_env(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "envelope"), gn->vcts[0]);
      s7_varlet(sc, let, s7_make_symbol(sc, "scaler"), s7_make_real(sc, mus_scaler(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "offset"), s7_make_real(sc, mus_offset(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "base"), s7_make_real(sc, mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "length"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "current-value"), s7_make_real(sc, mus_phase(g)));
    }
  if (mus_is_pulsed_env(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "env"), gn->vcts[0]);
      s7_varlet(sc, let, s7_make_symbol(sc, "pulse-train"), gn->vcts[1]);
    }

  if (mus_is_convolve(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "input"), gn->vcts[MUS_INPUT_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "filter"), gn->vcts[MUS_ANALYZE_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "fft-size"), s7_make_integer(sc, mus_length(g)));
    }

  if ((mus_is_rand(g)) || (mus_is_rand_interp(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "amplitude"), s7_make_real(sc, mus_scaler(g)));
      if (gn->nvcts > 0) s7_varlet(sc, let, s7_make_symbol(sc, "distribution"), gn->vcts[0]);
    }

  if (mus_is_granulate(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "input"), gn->vcts[MUS_INPUT_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "expansion"), s7_make_real(sc, mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "scaler"), s7_make_real(sc, mus_scaler(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "hop"), s7_make_real(sc, mus_frequency(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "ramp"), s7_make_real(sc, mus_ramp(g) / (mus_float_t)mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "jitter"), s7_make_real(sc, mus_offset(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "length"), s7_make_real(sc, mus_length(g) / mus_srate()));
      s7_varlet(sc, let, s7_make_symbol(sc, "edit"), gn->vcts[MUS_EDIT_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "grain"), gn->vcts[MUS_DATA_WRAPPER]);
    } 

  if (mus_is_locsig(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "degree"), s7_make_real(sc, mus_scaler(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "distance"), s7_make_real(sc, mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "reverb"), s7_make_real(sc, mus_offset(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "channels"), s7_make_integer(sc, mus_channels(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "type"), s7_make_symbol(sc, mus_interp_type_to_string(mus_hop(g))));
      s7_varlet(sc, let, s7_make_symbol(sc, "out-coeffs"), gn->vcts[G_LOCSIG_DATA]);
      s7_varlet(sc, let, s7_make_symbol(sc, "rev-coeffs"), gn->vcts[G_LOCSIG_REVDATA]);
      if (gn->nvcts == 4)
	{
	  s7_varlet(sc, let, s7_make_symbol(sc, "output"), gn->vcts[G_LOCSIG_OUT]);
	  s7_varlet(sc, let, s7_make_symbol(sc, "revout"), gn->vcts[G_LOCSIG_REVOUT]);
	}
    }
  
  if ((mus_is_sample_to_file(g)) || (mus_is_frample_to_file(g)))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "channels"), s7_make_integer(sc, mus_channels(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "sample-type"), s7_make_symbol(sc, mus_sample_type_to_string((mus_sample_t)mus_location(g))));
      s7_varlet(sc, let, s7_make_symbol(sc, "header-type"), s7_make_symbol(sc, mus_header_type_to_string((mus_header_t)mus_channel(g))));
      s7_varlet(sc, let, s7_make_symbol(sc, "buffer-length"), s7_make_integer(sc, mus_length(g)));
    }

  if (mus_is_ssb_am(g))
    s7_varlet(sc, let, s7_make_symbol(sc, "order"), s7_make_integer(sc, mus_length(g)));

  if (mus_is_phase_vocoder(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "input"), gn->vcts[MUS_INPUT_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "fft-size"), s7_make_integer(sc, mus_length(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "overlap"), s7_make_integer(sc, mus_length(g) / mus_hop(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "interp"), s7_make_integer(sc, (s7_int)mus_increment(g)));
      s7_varlet(sc, let, s7_make_symbol(sc, "analyze"), gn->vcts[MUS_ANALYZE_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "edit"), gn->vcts[MUS_EDIT_FUNCTION]);
      s7_varlet(sc, let, s7_make_symbol(sc, "synthesize"), gn->vcts[MUS_SYNTHESIZE_FUNCTION]);
    }

  if (mus_is_move_sound(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "pars"), gn->vcts[G_LOCSIG_DATA]);
      if (gn->nvcts == 4)
	{
	  s7_varlet(sc, let, s7_make_symbol(sc, "output"), gn->vcts[G_LOCSIG_OUT]);
	  s7_varlet(sc, let, s7_make_symbol(sc, "revout"), gn->vcts[G_LOCSIG_REVOUT]);
	  s7_varlet(sc, let, s7_make_symbol(sc, "rev-coeffs"), gn->vcts[G_LOCSIG_REVDATA]);
	}
    }

  if (mus_is_polyshape(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "coeffs"), gn->vcts[0]);
      s7_varlet(sc, let, s7_make_symbol(sc, "choice"), s7_make_integer(sc, mus_channel(g)));
    }

  if (mus_is_polywave(g))
    {
      s7_varlet(sc, let, s7_make_symbol(sc, "xcoeffs"), gn->vcts[0]);
      if (gn->nvcts > 1)
	s7_varlet(sc, let, s7_make_symbol(sc, "ycoeffs"), gn->vcts[1]);
      s7_varlet(sc, let, s7_make_symbol(sc, "choice"), s7_make_integer(sc, mus_channel(g)));
    }
  return(let);
}

#endif

static void mus_xen_init(void)
{
#if HAVE_SCHEME
  s7_pointer s, i, p, t, r, c, f, v, b, d, j;

  s7_pointer pl_bt, pl_ir, pl_cc, pl_ccic, pl_ccrr, pl_fc, pl_fcif, pl_cs, pl_ff, pl_tt, pl_fffifi, pl_ffftii, pl_fffi, 
    pl_fti, pl_fif, pl_fiir, pl_fttb, pl_ic, pl_rciir, pl_rcir, pl_ririt, pl_dirt, pl_riirfff, pl_rirfff, pl_rrpr, 
    pl_sc, pl_sssrs, pl_tc, pl_ici, pl_i, pl_fcf, pl_dcr, pl_dr, pl_dffi, pl_dfri, pl_dirfir, pl_dc, pl_dci, pl_dcir, pl_dv, 
    pl_dvir, pl_drf, pl_drc, pl_diit, pl_dit, pl_dct, pl_d, pl_djr, pl_it, pl_iti;
#endif

  mus_initialize();
  current_connect_func = Xen_false;

#if HAVE_SCHEME
  mus_xen_tag = s7_make_c_type(s7, "<generator>");
  s7_c_type_set_gc_free(s7, mus_xen_tag, s7_mus_xen_free);
  s7_c_type_set_is_equal(s7, mus_xen_tag, s7_mus_xen_is_equal);
  s7_c_type_set_gc_mark(s7, mus_xen_tag, s7_mus_xen_mark);
  s7_c_type_set_ref(s7, mus_xen_tag, mus_xen_apply);
  s7_c_type_set_length(s7, mus_xen_tag, s7_mus_length);
  s7_c_type_set_copy(s7, mus_xen_tag, s7_mus_copy);
  s7_c_type_set_to_string(s7, mus_xen_tag, mus_generator_to_string);

  mus_error_symbol = s7_make_symbol(s7, "mus-error");
  clm_error_info = s7_list(s7, 4, s7_make_string(s7, "~A: ~A ~A"), s7_nil(s7), s7_nil(s7), s7_nil(s7));
  s7_gc_protect(s7, clm_error_info);
  clm_err1 = s7_cdr(clm_error_info);
  clm_err2 = s7_cddr(clm_error_info);
  clm_err3 = s7_cdddr(clm_error_info);

  extra_args_string = s7_make_string(s7, "extra trailing args?");
  s7_gc_protect(s7, extra_args_string);

  s = s7_make_symbol(s7, "string?");
  i = s7_make_symbol(s7, "integer?");
  p = s7_make_symbol(s7, "pair?");
  t = s7_t(s7);
  r = s7_make_symbol(s7, "real?");
  c = s7_t(s7); /* s7_make_symbol(s7, "c-object?"): this should be mus-generator which should match against oscil? etc -- maybe someday... */
  f = s7_make_symbol(s7, "float-vector?");
  j = s7_make_symbol(s7, "int-vector?");
  v = s7_make_symbol(s7, "vector?");
  b = s7_make_symbol(s7, "boolean?");
  d = s7_make_symbol(s7, "float?");
  
  pl_bt = s7_make_signature(s7, 2, b, t);
  
  pl_d = s7_make_signature(s7, 1, d);
  pl_dcr = s7_make_circular_signature(s7, 2, 3, d, c, r);
  pl_djr = s7_make_circular_signature(s7, 2, 3, d, j, r);
  pl_dct = s7_make_signature(s7, 3, d, c, t);
  pl_dci = s7_make_circular_signature(s7, 2, 3, d, c, i);
  pl_dcir = s7_make_signature(s7, 4, d, c, i, r);
  pl_dr = s7_make_circular_signature(s7, 1, 2, d, r);
  pl_dffi = s7_make_signature(s7, 4, d, f, f, i);
  pl_dfri = s7_make_signature(s7, 4, d, f, r, i);
  pl_dirfir = s7_make_signature(s7, 6, d, i, r, f, i, r);
  pl_dc = s7_make_signature(s7, 2, d, c);
  pl_dv = s7_make_signature(s7, 2, d, v);
  pl_dvir = s7_make_signature(s7, 4, d, v, i, r);
  pl_drf = s7_make_circular_signature(s7, 2, 3, d, r, f);
  pl_drc = s7_make_signature(s7, 3, d, r, c);
  pl_diit = s7_make_signature(s7, 4, d, i, i, t);
  pl_dit = s7_make_signature(s7, 3, d, i, t);
  
  pl_ir = s7_make_signature(s7, 2, i, r);
  pl_i = s7_make_circular_signature(s7, 0, 1, i);
  pl_cc = s7_make_circular_signature(s7, 1, 2, c, c);
  pl_ici = s7_make_signature(s7, 3, i, c, i);
  pl_iti = s7_make_signature(s7, 3, i, t, i);
  pl_ccic = s7_make_signature(s7, 4, c, c, i, c);
  pl_ccrr = s7_make_signature(s7, 4, c, c, r, r);
  pl_fc = s7_make_signature(s7, 2, s7_make_signature(s7, 2, f, b), c);
  pl_cs = s7_make_signature(s7, 2, c, s);
  pl_ff = s7_make_circular_signature(s7, 1, 2, f, f);
  pl_tt = s7_make_signature(s7, 2, t, t);
  pl_fcf = s7_make_signature(s7, 3, f, c, f);
  pl_fffifi = s7_make_signature(s7, 6, f, f, f, i, f, i);
  pl_ffftii = s7_make_signature(s7, 6, f, f, f, t, i, i);
  pl_fffi = s7_make_circular_signature(s7, 3, 4, f, f, f, i);
  pl_fti = s7_make_signature(s7, 3, f, t, i);
  pl_fif = s7_make_signature(s7, 3, f, i, f);
  pl_fiir = s7_make_circular_signature(s7, 3, 4, f, i, i, r);
  pl_fttb = s7_make_signature(s7, 4, f, t, t, b);
  pl_ic = s7_make_signature(s7, 2, i, c);
  pl_it = s7_make_signature(s7, 2, i, t);
  pl_rciir = s7_make_signature(s7, 5, r, c, i, i, r);
  pl_rcir = s7_make_signature(s7, 4, r, c, i, r);
  pl_ririt = s7_make_signature(s7,5, r, i, r, i, t);
  pl_dirt = s7_make_signature(s7, 4, d, i, r, t);
  pl_riirfff = s7_make_signature(s7, 7, r, i, i, r, f, f, f);
  pl_rirfff = s7_make_signature(s7, 6, r, i, r, f, f, f);
  pl_rrpr = s7_make_signature(s7, 4, r, r, p, r);
  pl_sc = s7_make_signature(s7, 2, s, c);
  pl_sssrs = s7_make_signature(s7, 5, s, s, s, r, s);
  pl_tc = s7_make_signature(s7, 2, t, c);
  pl_fcif = s7_make_signature(s7, 4, f, c, i, f);
#else
  mus_xen_tag = Xen_make_object_type("Mus", sizeof(mus_xen));
#endif

  xen_one = C_int_to_Xen_integer(1);
  Xen_GC_protect(xen_one);
  xen_minus_one = C_int_to_Xen_integer(-1);
  Xen_GC_protect(xen_minus_one);
  xen_float_zero = C_double_to_Xen_real(0.0);
  Xen_GC_protect(xen_float_zero);

#if HAVE_SCHEME
  as_needed_arglist_one = Xen_list_1(xen_one);
  as_needed_arglist_minus_one = Xen_list_1(xen_minus_one);
  Xen_GC_protect(as_needed_arglist_one);
  Xen_GC_protect(as_needed_arglist_minus_one);
#endif

#if HAVE_FORTH
  fth_set_object_inspect(mus_xen_tag, print_mus_xen);
  fth_set_object_equal(mus_xen_tag, equalp_mus_xen);
  fth_set_object_mark(mus_xen_tag, mark_mus_xen);
  fth_set_object_free(mus_xen_tag, free_mus_xen);
  fth_set_object_apply(mus_xen_tag, Xen_procedure_cast mus_xen_apply, 0, 2, 0);
#endif

#if HAVE_RUBY
  rb_define_method(mus_xen_tag, "to_s", Xen_procedure_cast mus_xen_to_s, 0);
  rb_define_method(mus_xen_tag, "eql?", Xen_procedure_cast equalp_mus_xen, 1);
  rb_define_method(mus_xen_tag, "frequency", Xen_procedure_cast g_mus_frequency, 0);
  rb_define_method(mus_xen_tag, "frequency=", Xen_procedure_cast g_mus_set_frequency, 1);
  rb_define_method(mus_xen_tag, "phase", Xen_procedure_cast g_mus_phase, 0);
  rb_define_method(mus_xen_tag, "phase=", Xen_procedure_cast g_mus_set_phase, 1);
  rb_define_method(mus_xen_tag, "scaler", Xen_procedure_cast g_mus_scaler, 0);
  rb_define_method(mus_xen_tag, "scaler=", Xen_procedure_cast g_mus_set_scaler, 1);
  rb_define_method(mus_xen_tag, "width", Xen_procedure_cast g_mus_width, 0);
  rb_define_method(mus_xen_tag, "width=", Xen_procedure_cast g_mus_set_width, 1);
  rb_define_method(mus_xen_tag, "offset", Xen_procedure_cast g_mus_offset, 0);
  rb_define_method(mus_xen_tag, "offset=", Xen_procedure_cast g_mus_set_offset, 1);
  rb_define_method(mus_xen_tag, "reset", Xen_procedure_cast g_mus_reset, 0);
  /* rb_define_method(mus_xen_tag, "copy", Xen_procedure_cast g_mus_copy, 0); */
  rb_define_method(mus_xen_tag, "length", Xen_procedure_cast g_mus_length, 0);
  rb_define_method(mus_xen_tag, "length=", Xen_procedure_cast g_mus_set_length, 1);
  rb_define_method(mus_xen_tag, "data", Xen_procedure_cast g_mus_data, 0);
  rb_define_method(mus_xen_tag, "data=", Xen_procedure_cast g_mus_set_data, 1);
  rb_define_method(mus_xen_tag, "feedforward", Xen_procedure_cast g_mus_feedforward, 0);
  rb_define_method(mus_xen_tag, "feedforward=", Xen_procedure_cast g_mus_set_feedforward, 1);
  rb_define_method(mus_xen_tag, "feedback", Xen_procedure_cast g_mus_feedback, 0);
  rb_define_method(mus_xen_tag, "feedback=", Xen_procedure_cast g_mus_set_increment, 1);
  rb_define_method(mus_xen_tag, "order", Xen_procedure_cast g_mus_order, 0);
  rb_define_method(mus_xen_tag, "type", Xen_procedure_cast g_mus_type, 0);
  rb_define_method(mus_xen_tag, "order=", Xen_procedure_cast g_mus_set_length, 1);
  rb_define_method(mus_xen_tag, "call", Xen_procedure_cast mus_xen_apply, 2);
  rb_define_method(mus_xen_tag, "location", Xen_procedure_cast g_mus_location, 0);
  rb_define_method(mus_xen_tag, "location=", Xen_procedure_cast g_mus_set_location, 1);
  rb_define_method(mus_xen_tag, "increment", Xen_procedure_cast g_mus_increment, 0);
  rb_define_method(mus_xen_tag, "increment=", Xen_procedure_cast g_mus_set_increment, 1);
  rb_define_method(mus_xen_tag, "channels", Xen_procedure_cast g_mus_channels, 0);
  rb_define_method(mus_xen_tag, "channel", Xen_procedure_cast g_mus_channel, 0);
  rb_define_method(mus_xen_tag, "interp_type", Xen_procedure_cast g_mus_interp_type, 0);
  rb_define_method(mus_xen_tag, "xcoeffs", Xen_procedure_cast g_mus_xcoeffs, 0);
  rb_define_method(mus_xen_tag, "ycoeffs", Xen_procedure_cast g_mus_ycoeffs, 0);
  rb_define_method(mus_xen_tag, "xcoeff", Xen_procedure_cast g_mus_xcoeff, 1);
  rb_define_method(mus_xen_tag, "ycoeff", Xen_procedure_cast g_mus_ycoeff, 1);
  /*
  rb_define_method(mus_xen_tag, "xcoeff=", Xen_procedure_cast g_mus_set_xcoeff, 1);
  rb_define_method(mus_xen_tag, "ycoeff=", Xen_procedure_cast g_mus_set_ycoeff, 1);
  */
  rb_define_method(mus_xen_tag, "ramp", Xen_procedure_cast g_mus_ramp, 0);
  rb_define_method(mus_xen_tag, "ramp=", Xen_procedure_cast g_mus_set_ramp, 1);
  rb_define_method(mus_xen_tag, "hop", Xen_procedure_cast g_mus_hop, 0);
  rb_define_method(mus_xen_tag, "hop=", Xen_procedure_cast g_mus_set_hop, 1);
  rb_define_method(mus_xen_tag, "name", Xen_procedure_cast g_mus_name, 0);
  rb_define_method(mus_xen_tag, "file_name", Xen_procedure_cast g_mus_file_name, 0);
#endif  

  init_keywords();

  Xen_define_typed_dilambda(S_mus_srate, g_mus_srate_w, H_mus_srate, 
			    S_set S_mus_srate, g_mus_set_srate_w, 0, 0, 1, 0, pl_d, pl_dr);
  Xen_define_typed_dilambda(S_mus_float_equal_fudge_factor, g_mus_float_equal_fudge_factor_w, H_mus_float_equal_fudge_factor,
			    S_set S_mus_float_equal_fudge_factor, g_mus_set_float_equal_fudge_factor_w, 0, 0, 1, 0, pl_d, pl_dr);
  Xen_define_typed_dilambda(S_mus_array_print_length, g_mus_array_print_length_w, H_mus_array_print_length,
			    S_set S_mus_array_print_length, g_mus_set_array_print_length_w, 0, 0, 1, 0, pl_i, pl_i);
  Xen_define_typed_dilambda(S_clm_table_size, g_clm_table_size_w, H_clm_table_size, 
			    S_set S_clm_table_size, g_set_clm_table_size_w, 0, 0, 1, 0, pl_i, pl_i);

#if HAVE_SCHEME
  clm_srate_symbol = s7_define_variable(s7, "*clm-srate*", s7_make_real(s7, MUS_DEFAULT_SAMPLING_RATE));
  s7_set_setter(s7, clm_srate_symbol, s7_make_function(s7, "[acc-clm-srate]", acc_clm_srate, 2, 0, false, "accessor"));

  clm_table_size_symbol = s7_define_variable(s7, "*" S_clm_table_size "*", s7_make_integer(s7, MUS_CLM_DEFAULT_TABLE_SIZE));
  s7_set_documentation(s7, clm_table_size_symbol, "*clm-table-size*: the default table size for most generators (512)");
  s7_set_setter(s7, clm_table_size_symbol, s7_make_function(s7, "[acc-clm-table-size]", acc_clm_table_size, 2, 0, false, "accessor"));

  mus_file_buffer_size_symbol = s7_define_variable(s7, "*clm-file-buffer-size*", s7_make_integer(s7, MUS_DEFAULT_FILE_BUFFER_SIZE));
  s7_set_setter(s7, mus_file_buffer_size_symbol, s7_make_function(s7, "[acc-mus-file-buffer-size]", acc_mus_file_buffer_size, 2, 0, false, "accessor"));

  mus_float_equal_fudge_factor_symbol = s7_define_variable(s7, "*" S_mus_float_equal_fudge_factor "*", s7_make_real(s7, 0.0000001)); /* clm.c */
  s7_set_documentation(s7, mus_float_equal_fudge_factor_symbol, "*mus-float-equal-fudge-factor*: floating point equality fudge factor");
  s7_set_setter(s7, mus_float_equal_fudge_factor_symbol, s7_make_function(s7, "[acc-mus-float-equal-fudge-factor]", acc_mus_float_equal_fudge_factor, 2, 0, false, "accessor"));

  mus_array_print_length_symbol = s7_define_variable(s7, "*" S_mus_array_print_length "*", s7_make_integer(s7, MUS_DEFAULT_ARRAY_PRINT_LENGTH));
  s7_set_documentation(s7, mus_array_print_length_symbol, "*mus-array-print-length*: current clm array print length (default is 8).");
  s7_set_setter(s7, mus_array_print_length_symbol, s7_make_function(s7, "[acc-mus-array-print-length]", acc_mus_array_print_length, 2, 0, false, "accessor"));

  g_clm_let = s7_openlet(s7, s7_inlet(s7, s7_list(s7, 2, s7_make_symbol(s7, "object->let"), 
						  s7_make_function(s7, "generator->let", generator_to_let, 2, 0, false, "clm generator object->let method"))));
  s7_gc_protect(s7, g_clm_let);

#endif

  Xen_define_typed_procedure(S_radians_to_hz,		g_radians_to_hz_w,		1, 0, 0, H_radians_to_hz,	pl_dr);
  Xen_define_typed_procedure(S_hz_to_radians,		g_hz_to_radians_w,		1, 0, 0, H_hz_to_radians,	pl_dr);
  Xen_define_typed_procedure(S_radians_to_degrees,	g_radians_to_degrees_w,		1, 0, 0, H_radians_to_degrees,	pl_dr);
  Xen_define_typed_procedure(S_degrees_to_radians,	g_degrees_to_radians_w,		1, 0, 0, H_degrees_to_radians,	pl_dr);
  Xen_define_typed_procedure(S_db_to_linear,		g_db_to_linear_w,		1, 0, 0, H_db_to_linear,	pl_dr);
  Xen_define_typed_procedure(S_linear_to_db,		g_linear_to_db_w,		1, 0, 0, H_linear_to_db,	pl_dr);
  Xen_define_typed_procedure(S_even_weight,		g_even_weight_w,		1, 0, 0, H_even_weight,		pl_dr);
  Xen_define_typed_procedure(S_odd_weight,		g_odd_weight_w,			1, 0, 0, H_odd_weight,		pl_dr);
  Xen_define_typed_procedure(S_even_multiple,		g_even_multiple_w,		2, 0, 0, H_even_multiple,	pl_dr);
  Xen_define_typed_procedure(S_odd_multiple,		g_odd_multiple_w,		2, 0, 0, H_odd_multiple,	pl_dr);
  Xen_define_typed_procedure(S_seconds_to_samples,	g_seconds_to_samples_w,		1, 0, 0, H_seconds_to_samples,	pl_ir);
  Xen_define_typed_procedure(S_samples_to_seconds,	g_samples_to_seconds_w,		1, 0, 0, H_samples_to_seconds,	pl_dr);
  Xen_define_typed_procedure(S_ring_modulate,		g_ring_modulate_w,		2, 0, 0, H_ring_modulate,	pl_dr);
  Xen_define_typed_procedure(S_amplitude_modulate,	g_amplitude_modulate_w,		3, 0, 0, H_amplitude_modulate,	pl_dr);
  Xen_define_typed_procedure(S_contrast_enhancement,	g_contrast_enhancement_w,	1, 1, 0, H_contrast_enhancement, pl_dr);
  Xen_define_typed_procedure(S_dot_product,		g_dot_product_w,		2, 1, 0, H_dot_product,		pl_dffi);
#if HAVE_COMPLEX_TRIG && HAVE_COMPLEX_NUMBERS && (!HAVE_RUBY)
  Xen_define_typed_procedure(S_edot_product,		g_edot_product_w,		2, 0, 0, H_edot_product,	NULL);
#endif
  Xen_define_typed_procedure(S_polynomial,		g_polynomial_w,			2, 0, 0, H_polynomial,		pl_dfri);
  Xen_define_typed_procedure(S_make_fft_window,		g_make_fft_window_w,		2, 2, 0, H_make_fft_window,	pl_fiir);
  Xen_define_typed_procedure(S_mus_fft,			g_mus_fft_w,			2, 2, 0, H_mus_fft,		pl_fffi);
  Xen_define_typed_procedure(S_spectrum,		g_spectrum_w,			3, 1, 0, H_mus_spectrum,	pl_ffftii); 
  Xen_define_typed_procedure(S_autocorrelate,		g_autocorrelate_w,		1, 0, 0, H_autocorrelate,	pl_ff);
  Xen_define_typed_procedure(S_correlate,		g_correlate_w,			2, 0, 0, H_correlate,		pl_ff);
  Xen_define_typed_procedure(S_convolution,		g_convolution_w,		2, 1, 0, H_mus_convolution,	pl_fffi);
  Xen_define_typed_procedure(S_rectangular_to_polar,	g_rectangular_to_polar_w,	2, 0, 0, H_rectangular_to_polar, pl_ff);
  Xen_define_typed_procedure(S_rectangular_to_magnitudes, g_rectangular_to_magnitudes_w, 2, 0, 0, H_rectangular_to_magnitudes, pl_ff);
  Xen_define_typed_procedure(S_polar_to_rectangular,	g_polar_to_rectangular_w,	2, 0, 0, H_polar_to_rectangular, pl_ff);
  Xen_define_typed_procedure(S_array_interp,		g_array_interp_w,		2, 1, 0, H_array_interp,	pl_dfri);
  Xen_define_typed_procedure(S_mus_interpolate,		g_mus_interpolate_w,		3, 2, 0, H_mus_interpolate,	pl_dirfir);
  Xen_define_typed_procedure(S_mus_frandom,		g_mus_frandom_w,		1, 0, 0, "random reals",	pl_dr);
  Xen_define_typed_procedure(S_mus_irandom,		g_mus_irandom_w,		1, 0, 0, "random integers",	pl_i);

  Xen_define_constant(S_rectangular_window,		MUS_RECTANGULAR_WINDOW,		"The un-window, so to speak");
  Xen_define_constant(S_hann_window,			MUS_HANN_WINDOW,		"A simple raised cosine window");
  Xen_define_constant(S_welch_window,			MUS_WELCH_WINDOW,		"A triangular window squared");
  Xen_define_constant(S_parzen_window,			MUS_PARZEN_WINDOW,		"A triangular window");
  Xen_define_constant(S_bartlett_window,		MUS_BARTLETT_WINDOW,		"A triangular window");
  Xen_define_constant(S_bartlett_hann_window,		MUS_BARTLETT_HANN_WINDOW,	"A combination of the bartlett and hann windows");
  Xen_define_constant(S_bohman_window,			MUS_BOHMAN_WINDOW,		"A weighted cosine window");
  Xen_define_constant(S_flat_top_window,		MUS_FLAT_TOP_WINDOW,		"A sum of cosines window");
  Xen_define_constant(S_hamming_window,			MUS_HAMMING_WINDOW,		"A raised cosine");
  Xen_define_constant(S_blackman2_window,		MUS_BLACKMAN2_WINDOW,		"second order cosine window");
  Xen_define_constant(S_blackman3_window,		MUS_BLACKMAN3_WINDOW,		"third order cosine window");
  Xen_define_constant(S_blackman4_window,		MUS_BLACKMAN4_WINDOW,		"4th order cosine window");
  Xen_define_constant(S_blackman5_window,		MUS_BLACKMAN5_WINDOW,		"5th order cosine window");
  Xen_define_constant(S_blackman6_window,		MUS_BLACKMAN6_WINDOW,		"6th order cosine window");
  Xen_define_constant(S_blackman7_window,		MUS_BLACKMAN7_WINDOW,		"7th order cosine window");
  Xen_define_constant(S_blackman8_window,		MUS_BLACKMAN8_WINDOW,		"8th order cosine window");
  Xen_define_constant(S_blackman9_window,		MUS_BLACKMAN9_WINDOW,		"9th order cosine window");
  Xen_define_constant(S_blackman10_window,		MUS_BLACKMAN10_WINDOW,		"10th order cosine window");
  Xen_define_constant(S_exponential_window,		MUS_EXPONENTIAL_WINDOW,		"An inverted triangle from exp");
  Xen_define_constant(S_riemann_window,			MUS_RIEMANN_WINDOW,		"sinc-based window");
  Xen_define_constant(S_kaiser_window,			MUS_KAISER_WINDOW,		"Bessel I0 based window");
  Xen_define_constant(S_cauchy_window,			MUS_CAUCHY_WINDOW,		"window based on 1/(1+sqr(angle)");
  Xen_define_constant(S_poisson_window,			MUS_POISSON_WINDOW,		"window based on exp(-angle)");
  Xen_define_constant(S_gaussian_window,		MUS_GAUSSIAN_WINDOW,		"window based on exp(-sqr(angle))");
  Xen_define_constant(S_tukey_window,			MUS_TUKEY_WINDOW,		"window based on truncated cosine");
  Xen_define_constant(S_dolph_chebyshev_window,		MUS_DOLPH_CHEBYSHEV_WINDOW,	"window from inverse fft (using Chebyshev Tn)");
  Xen_define_constant(S_connes_window,			MUS_CONNES_WINDOW,		"triangle window squared twice");
  Xen_define_constant(S_hann_poisson_window,		MUS_HANN_POISSON_WINDOW,	"poisson window * hann window");
  Xen_define_constant(S_samaraki_window,		MUS_SAMARAKI_WINDOW,		"window from inverse fft (using Chebyshev Un)");
  Xen_define_constant(S_ultraspherical_window,		MUS_ULTRASPHERICAL_WINDOW,	"window from inverse fft (using Ultraspherical Cn)");
  Xen_define_constant(S_rv2_window,			MUS_RV2_WINDOW,			"Rife-Vincent second order window (Hann extension)");
  Xen_define_constant(S_rv3_window,			MUS_RV3_WINDOW,			"Rife-Vincent third order window (Hann extension)");
  Xen_define_constant(S_rv4_window,			MUS_RV4_WINDOW,			"Rife-Vincent 4th order window (Hann extension)");
  Xen_define_constant(S_mlt_sine_window,		MUS_MLT_SINE_WINDOW,		"modulated lapped transform sine window");
  Xen_define_constant(S_papoulis_window,		MUS_PAPOULIS_WINDOW,		"papoulise window");
  Xen_define_constant(S_dpss_window,			MUS_DPSS_WINDOW,		"proplate spheroidal (slepian) window");
  Xen_define_constant(S_sinc_window,			MUS_SINC_WINDOW,		"sinc (Lanczos) window");

  Xen_define_constant(S_mus_interp_linear,		MUS_INTERP_LINEAR,		"locsig/delay linear interpolation");
  Xen_define_constant(S_mus_interp_sinusoidal,		MUS_INTERP_SINUSOIDAL,		"locsig sinusoidal interpolation");
  Xen_define_constant(S_mus_interp_all_pass,		MUS_INTERP_ALL_PASS,		"delay interpolation");
  Xen_define_constant(S_mus_interp_lagrange,		MUS_INTERP_LAGRANGE,		"second order lagrange interpolation");
  Xen_define_constant(S_mus_interp_hermite,		MUS_INTERP_HERMITE,		"third order hermite interpolation");
  Xen_define_constant(S_mus_interp_none,		MUS_INTERP_NONE,		"no interpolation -- step func");
  Xen_define_constant(S_mus_interp_bezier,		MUS_INTERP_BEZIER,		"bezier interpolation");

  Xen_define_constant(S_mus_chebyshev_first_kind,	MUS_CHEBYSHEV_FIRST_KIND,	"Chebyshev polynomial of first kind, for " S_partials_to_polynomial);
  Xen_define_constant(S_mus_chebyshev_second_kind,	MUS_CHEBYSHEV_SECOND_KIND,	"Chebyshev polynomial of second kind, for " S_partials_to_polynomial);
  Xen_define_constant(S_mus_chebyshev_both_kinds,	MUS_CHEBYSHEV_BOTH_KINDS,	"use both Chebyshev polynomials in polywave");

  Xen_define_typed_procedure(S_mus_describe,		g_mus_describe_w,		1, 0, 0,  H_mus_describe,	pl_sc);
  Xen_define_typed_procedure(S_mus_file_name,		g_mus_file_name_w,		1, 0, 0,  H_mus_file_name,	pl_sc);
  Xen_define_typed_procedure(S_mus_reset,		g_mus_reset_w,			1, 0, 0,  H_mus_reset,		pl_tc);
  Xen_define_typed_procedure(S_mus_copy,		g_mus_copy_w,			1, 0, 0,  H_mus_copy,		pl_cc);
  Xen_define_typed_procedure(S_mus_run,			g_mus_run_w,			1, 2, 0,  H_mus_run,            pl_dcr);

  Xen_define_typed_procedure(S_mus_name,     g_mus_name_w, 1, 0, 0,  H_mus_name, pl_sc);
  Xen_define_typed_dilambda(S_mus_phase,     g_mus_phase_w,     H_mus_phase,     S_set S_mus_phase,     g_mus_set_phase_w,      1, 0, 2, 0, pl_dc, pl_dcr);
  Xen_define_typed_dilambda(S_mus_scaler,    g_mus_scaler_w,    H_mus_scaler,    S_set S_mus_scaler,    g_mus_set_scaler_w,     1, 0, 2, 0, pl_dc, pl_dcr);
  Xen_define_typed_dilambda(S_mus_width,     g_mus_width_w,     H_mus_width,     S_set S_mus_width,     g_mus_set_width_w,      1, 0, 2, 0, pl_ic, pl_ici);
  Xen_define_typed_dilambda(S_mus_frequency, g_mus_frequency_w, H_mus_frequency, S_set S_mus_frequency, g_mus_set_frequency_w,  1, 0, 2, 0, pl_dc, pl_dcr);
  Xen_define_typed_dilambda(S_mus_length,    g_mus_length_w,    H_mus_length,    S_set S_mus_length,    g_mus_set_length_w,     1, 0, 2, 0, pl_it, pl_iti);
  Xen_define_typed_dilambda(S_mus_data,      g_mus_data_w,      H_mus_data,      S_set S_mus_data,      g_mus_set_data_w,       1, 0, 2, 0, pl_fc, pl_fcf);
  Xen_define_typed_dilambda(S_mus_xcoeff,    g_mus_xcoeff_w,    H_mus_xcoeff,    S_set S_mus_xcoeff,    g_mus_set_xcoeff_w,     2, 0, 3, 0, pl_dci, pl_dcir);
  Xen_define_typed_dilambda(S_mus_ycoeff,    g_mus_ycoeff_w,    H_mus_ycoeff,    S_set S_mus_ycoeff,    g_mus_set_ycoeff_w,     2, 0, 3, 0, pl_dci, pl_dcir);
  Xen_define_typed_dilambda(S_mus_offset,    g_mus_offset_w,    H_mus_offset,    S_set S_mus_offset,    g_mus_set_offset_w,     1, 0, 2, 0, pl_dc, pl_dcr);

  Xen_define_typed_procedure(S_mus_xcoeffs,		g_mus_xcoeffs_w,		1, 0, 0, H_mus_xcoeffs,		pl_fc);
  Xen_define_typed_procedure(S_mus_ycoeffs,		g_mus_ycoeffs_w,		1, 0, 0, H_mus_ycoeffs,		pl_fc);

  Xen_define_typed_procedure(S_is_oscil,		g_is_oscil_w,			1, 0, 0, H_is_oscil,		pl_bt);
  Xen_define_typed_procedure(S_oscil,			g_oscil_w,			1, 2, 0, H_oscil,
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_oscil), r));

  Xen_define_typed_procedure(S_is_oscil_bank,		g_is_oscil_bank_w,		1, 0, 0, H_is_oscil_bank,	pl_bt);
  Xen_define_typed_procedure(S_oscil_bank,		g_oscil_bank_w,			1, 0, 0, H_oscil_bank,
			     s7_make_signature(s7, 2, d, s7_make_symbol(s7, S_is_oscil_bank)));

  Xen_define_typed_procedure(S_mus_apply,		g_mus_apply_w,			0, 0, 1, H_mus_apply,           pl_dcr);

  Xen_define_typed_procedure(S_delay,			g_delay_w,                 1, 2, 0, H_delay,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_delay), r));
  Xen_define_typed_procedure(S_delay_tick,		g_delay_tick_w,            1, 1, 0, H_delay_tick,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_delay), r)); 
  Xen_define_typed_procedure(S_tap,			g_tap_w,                   1, 1, 0, H_tap,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_tap), r));
  Xen_define_typed_procedure(S_notch,			g_notch_w,                 1, 2, 0, H_notch,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_notch), r));
  Xen_define_typed_procedure(S_comb,			g_comb_w,                  1, 2, 0, H_comb,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_comb), r));
  Xen_define_typed_procedure(S_filtered_comb,		g_filtered_comb_w,         1, 2, 0, H_filtered_comb,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_filtered_comb), r));
  Xen_define_typed_procedure(S_all_pass,		g_all_pass_w,              1, 2, 0, H_all_pass,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_all_pass), r));
  Xen_define_typed_procedure(S_moving_average,		g_moving_average_w,        1, 1, 0, H_moving_average,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_moving_average), r));
  Xen_define_typed_procedure(S_moving_max,		g_moving_max_w,            1, 1, 0, H_moving_max,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_moving_max), r));
  Xen_define_typed_procedure(S_moving_norm,		g_moving_norm_w,           1, 1, 0, H_moving_norm,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_moving_norm), r));
  Xen_define_typed_procedure(S_is_tap,			g_is_tap_w,                1, 0, 0, H_is_tap,			pl_bt);
  Xen_define_typed_procedure(S_is_delay,		g_is_delay_w,              1, 0, 0, H_is_delay,			pl_bt);
  Xen_define_typed_procedure(S_is_notch,		g_is_notch_w,              1, 0, 0, H_is_notch,			pl_bt);
  Xen_define_typed_procedure(S_is_comb,			g_is_comb_w,               1, 0, 0, H_is_comb,			pl_bt);
  Xen_define_typed_procedure(S_is_filtered_comb,	g_is_filtered_comb_w,      1, 0, 0, H_is_filtered_comb,		pl_bt);
  Xen_define_typed_procedure(S_is_all_pass,		g_is_all_pass_w,           1, 0, 0, H_is_all_pass,		pl_bt);
  Xen_define_typed_procedure(S_is_moving_average,	g_is_moving_average_w,     1, 0, 0, H_is_moving_average,	pl_bt);
  Xen_define_typed_procedure(S_is_moving_max,		g_is_moving_max_w,         1, 0, 0, H_is_moving_max,		pl_bt);
  Xen_define_typed_procedure(S_is_moving_norm,		g_is_moving_norm_w,        1, 0, 0, H_is_moving_norm,		pl_bt);

  Xen_define_typed_procedure(S_comb_bank,		g_comb_bank_w,             1, 1, 0, H_comb_bank,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_comb_bank), r));
  Xen_define_typed_procedure(S_is_comb_bank,		g_is_comb_bank_w,          1, 0, 0, H_is_comb_bank,		pl_bt);
  Xen_define_typed_procedure(S_make_comb_bank,		g_make_comb_bank_w,        1, 0, 0, H_make_comb_bank,		
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_comb_bank), t));

  Xen_define_typed_procedure(S_filtered_comb_bank,	g_filtered_comb_bank_w,    1, 1, 0, H_filtered_comb_bank,	
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_filtered_comb_bank), r));
  Xen_define_typed_procedure(S_is_filtered_comb_bank,	g_is_filtered_comb_bank_w,  1, 0, 0, H_is_filtered_comb_bank,	pl_bt);
  Xen_define_typed_procedure(S_make_filtered_comb_bank, g_make_filtered_comb_bank_w, 1, 0, 0, H_make_filtered_comb_bank, 
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_filtered_comb_bank), t));

  Xen_define_typed_procedure(S_all_pass_bank,		g_all_pass_bank_w,         1, 1, 0, H_all_pass_bank,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_all_pass_bank), r));
  Xen_define_typed_procedure(S_is_all_pass_bank,	g_is_all_pass_bank_w,      1, 0, 0, H_is_all_pass_bank,		pl_bt);
  Xen_define_typed_procedure(S_make_all_pass_bank,	g_make_all_pass_bank_w,    1, 0, 0, H_make_all_pass_bank,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_all_pass_bank), t));

  Xen_define_typed_procedure(S_pink_noise,		g_pink_noise_w,            1, 0, 0, H_pink_noise,		pl_dv);

  Xen_define_typed_procedure(S_out_bank,		g_out_bank_w,              3, 0, 0, H_out_bank,			pl_dvir);

  Xen_define_typed_dilambda(S_mus_feedback,  g_mus_feedback_w, H_mus_feedback, 
			    S_set S_mus_feedback, g_mus_set_feedback_w,  1, 0, 2, 0, pl_dc, pl_dcr);
  Xen_define_typed_dilambda(S_mus_feedforward, g_mus_feedforward_w, H_mus_feedforward, 
			    S_set S_mus_feedforward, g_mus_set_feedforward_w,  1, 0, 2, 0, pl_dc, pl_dcr);

#if HAVE_RUBY
  rb_define_alias(rb_mKernel, "kernel_rand", "rand");
#endif
  Xen_define_typed_procedure(S_rand,			g_rand_w,                  1, 1, 0, H_rand,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_rand), r));
  Xen_define_typed_procedure(S_rand_interp,		g_rand_interp_w,           1, 1, 0, H_rand_interp,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_rand_interp), r));
  Xen_define_typed_procedure(S_is_rand,			g_is_rand_w,               1, 0, 0, H_is_rand,			pl_bt);
  Xen_define_typed_procedure(S_is_rand_interp,		g_is_rand_interp_w,        1, 0, 0, H_is_rand_interp,		pl_bt);
  Xen_define_typed_procedure(S_mus_random,		g_mus_random_w,            1, 0, 0, H_mus_random,		pl_dr);

  Xen_define_typed_dilambda(S_mus_rand_seed, g_mus_rand_seed_w, H_mus_rand_seed, 
			    S_set S_mus_rand_seed, g_mus_set_rand_seed_w, 0, 0, 1, 0, pl_i, pl_i);

  Xen_define_typed_procedure(S_ncos,			g_ncos_w,                  1, 1, 0, H_ncos,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_ncos), r));
  Xen_define_typed_procedure(S_is_ncos,			g_is_ncos_w,               1, 0, 0, H_is_ncos,			pl_bt);
  Xen_define_typed_procedure(S_nsin,			g_nsin_w,                  1, 1, 0, H_nsin,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_nsin), r));
  Xen_define_typed_procedure(S_is_nsin,			g_is_nsin_w,               1, 0, 0, H_is_nsin,			pl_bt);

  Xen_define_typed_procedure(S_is_table_lookup,		g_is_table_lookup_w,       1, 0, 0, H_is_table_lookup,		pl_bt);
  Xen_define_typed_procedure(S_table_lookup,		g_table_lookup_w,          1, 1, 0, H_table_lookup,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_table_lookup), r));
  Xen_define_typed_procedure(S_partials_to_wave,	g_partials_to_wave_w,      1, 2, 0, H_partials_to_wave,		pl_fttb);
  Xen_define_typed_procedure(S_phase_partials_to_wave,	g_phase_partials_to_wave_w, 1, 2, 0, H_phase_partials_to_wave,	pl_fttb);

  Xen_define_typed_procedure(S_sawtooth_wave,		g_sawtooth_wave_w,         1, 1, 0, H_sawtooth_wave,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_sawtooth_wave), r));
  Xen_define_typed_procedure(S_is_sawtooth_wave,	g_is_sawtooth_wave_w,      1, 0, 0, H_is_sawtooth_wave,		pl_bt);
  Xen_define_typed_procedure(S_triangle_wave,		g_triangle_wave_w,         1, 1, 0, H_triangle_wave,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_triangle_wave), r));
  Xen_define_typed_procedure(S_is_triangle_wave,	g_is_triangle_wave_w,      1, 0, 0, H_is_triangle_wave,		pl_bt);
  Xen_define_typed_procedure(S_square_wave,		g_square_wave_w,           1, 1, 0, H_square_wave,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_square_wave), r));
  Xen_define_typed_procedure(S_is_square_wave,		g_is_square_wave_w,        1, 0, 0, H_is_square_wave,		pl_bt);
  Xen_define_typed_procedure(S_pulse_train,		g_pulse_train_w,           1, 1, 0, H_pulse_train,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_pulse_train), r));
  Xen_define_typed_procedure(S_is_pulse_train,		g_is_pulse_train_w,        1, 0, 0, H_is_pulse_train,		pl_bt);


  Xen_define_typed_procedure(S_make_pulsed_env,		g_make_pulsed_env_w,       3, 0, 0, H_make_pulsed_env,		
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_pulsed_env), t));
  Xen_define_typed_procedure(S_pulsed_env,		g_pulsed_env_w,            1, 1, 0, H_pulsed_env,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_pulsed_env), r));
  Xen_define_typed_procedure(S_is_pulsed_env,		g_is_pulsed_env_w,         1, 0, 0, H_is_pulsed_env,		pl_bt);


  Xen_define_typed_procedure(S_asymmetric_fm,		g_asymmetric_fm_w,         1, 2, 0, H_asymmetric_fm,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_asymmetric_fm), r));
  Xen_define_typed_procedure(S_is_asymmetric_fm,	g_is_asymmetric_fm_w,      1, 0, 0, H_is_asymmetric_fm,		pl_bt);

  Xen_define_typed_procedure(S_one_zero,		g_one_zero_w,              1, 1, 0, H_one_zero,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_one_zero), r));
  Xen_define_typed_procedure(S_is_one_zero,		g_is_one_zero_w,           1, 0, 0, H_is_one_zero,		pl_bt);
  Xen_define_typed_procedure(S_one_pole,		g_one_pole_w,              1, 1, 0, H_one_pole,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_one_pole), r));
  Xen_define_typed_procedure(S_is_one_pole,		g_is_one_pole_w,           1, 0, 0, H_is_one_pole,		pl_bt);
  Xen_define_typed_procedure(S_two_zero,		g_two_zero_w,              1, 1, 0, H_two_zero,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_two_zero), r));
  Xen_define_typed_procedure(S_is_two_zero,		g_is_two_zero_w,           1, 0, 0, H_is_two_zero,		pl_bt);
  Xen_define_typed_procedure(S_two_pole,		g_two_pole_w,              1, 1, 0, H_two_pole,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_two_pole), r));
  Xen_define_typed_procedure(S_is_two_pole,		g_is_two_pole_w,           1, 0, 0, H_is_two_pole,		pl_bt);

  Xen_define_typed_procedure(S_wave_train,		g_wave_train_w,            1, 1, 0, H_wave_train,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_wave_train), r));
  Xen_define_typed_procedure(S_is_wave_train,		g_is_wave_train_w,         1, 0, 0, H_is_wave_train,		pl_bt);

  Xen_define_typed_procedure(S_is_formant,		g_is_formant_w,            1, 0, 0, H_is_formant,		pl_bt);
  Xen_define_typed_procedure(S_formant,			g_formant_w,               1, 2, 0, H_formant,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_formant), r));

  Xen_define_typed_procedure(S_formant_bank,		g_formant_bank_w,          1, 1, 0, H_formant_bank,		
			     s7_make_signature(s7, 3, d, s7_make_symbol(s7, S_is_formant_bank), s7_make_signature(s7, 2, r, f)));
  Xen_define_typed_procedure(S_is_formant_bank,		g_is_formant_bank_w,       1, 0, 0, H_is_formant_bank,		pl_bt);
  Xen_define_typed_procedure(S_make_formant_bank,	g_make_formant_bank_w,     1, 1, 0, H_make_formant_bank,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_formant_bank), t));

  Xen_define_typed_procedure(S_is_firmant,		g_is_firmant_w,            1, 0, 0, H_is_firmant,		pl_bt);
  Xen_define_typed_procedure(S_firmant,			g_firmant_w,               1, 2, 0, H_firmant,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_firmant), r));

  Xen_define_typed_procedure(S_is_one_pole_all_pass,	g_is_one_pole_all_pass_w,  1, 0, 0, H_is_one_pole_all_pass,	pl_bt);
  Xen_define_typed_procedure(S_make_one_pole_all_pass,	g_make_one_pole_all_pass_w, 2, 0, 0, H_make_one_pole_all_pass,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_one_pole_all_pass), t));
  Xen_define_typed_procedure(S_one_pole_all_pass,	g_one_pole_all_pass_w,     1, 1, 0, H_one_pole_all_pass,	
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_one_pole_all_pass), r));

  Xen_define_typed_procedure(S_mus_set_formant_frequency, g_set_formant_frequency_w, 2, 0, 0, H_mus_set_formant_frequency,
			     s7_make_signature(s7, 3, d, s7_make_symbol(s7, S_is_formant), r));
  Xen_define_typed_procedure(S_mus_set_formant_radius_and_frequency, g_set_formant_radius_and_frequency_w, 3, 0, 0, H_mus_set_formant_radius_and_frequency, 
			     s7_make_signature(s7, 4, d, s7_make_symbol(s7, S_is_formant), r, r));

  Xen_define_typed_procedure(S_polyshape,		g_polyshape_w,             1, 2, 0, H_polyshape,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_polyshape), r));
  Xen_define_typed_procedure(S_is_polyshape,		g_is_polyshape_w,          1, 0, 0, H_is_polyshape,		pl_bt);
  Xen_define_typed_procedure(S_partials_to_polynomial,	g_partials_to_polynomial_w, 1, 1, 0, H_partials_to_polynomial,	pl_fti);
  Xen_define_typed_procedure(S_normalize_partials,	g_normalize_partials_w,    1, 0, 0, H_normalize_partials,	pl_tt);
  Xen_define_typed_procedure(S_mus_chebyshev_t_sum,	g_chebyshev_t_sum_w,       2, 0, 0, H_chebyshev_t_sum,		pl_drf);
  Xen_define_typed_procedure(S_mus_chebyshev_u_sum,	g_chebyshev_u_sum_w,       2, 0, 0, H_chebyshev_u_sum,		pl_drf);
  Xen_define_typed_procedure(S_mus_chebyshev_tu_sum,	g_chebyshev_tu_sum_w,      3, 0, 0, H_chebyshev_tu_sum,		pl_drf);
  Xen_define_typed_procedure(S_polywave,		g_polywave_w,              1, 1, 0, H_polywave,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_polywave), r));
  Xen_define_typed_procedure(S_is_polywave,		g_is_polywave_w,           1, 0, 0, H_is_polywave,		pl_bt);

  Xen_define_typed_procedure(S_nrxysin,			g_nrxysin_w,               1, 1, 0, H_nrxysin,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_nrxysin), r));
  Xen_define_typed_procedure(S_is_nrxysin,		g_is_nrxysin_w,            1, 0, 0, H_is_nrxysin,		pl_bt);
  Xen_define_typed_procedure(S_nrxycos,			g_nrxycos_w,               1, 1, 0, H_nrxycos,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_nrxycos), r));
  Xen_define_typed_procedure(S_is_nrxycos,		g_is_nrxycos_w,            1, 0, 0, H_is_nrxycos,		pl_bt);

  Xen_define_typed_procedure(S_rxyksin,			g_rxyksin_w,               1, 1, 0, H_rxyksin,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_rxyksin), r));
  Xen_define_typed_procedure(S_is_rxyksin,		g_is_rxyksin_w,            1, 0, 0, H_is_rxyksin,		pl_bt);
  Xen_define_typed_procedure(S_rxykcos,			g_rxykcos_w,               1, 1, 0, H_rxykcos,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_rxykcos), r));
  Xen_define_typed_procedure(S_is_rxykcos,		g_is_rxykcos_w,            1, 0, 0, H_is_rxykcos,		pl_bt);


  Xen_define_typed_procedure(S_filter,			g_filter_w,                1, 1, 0, H_filter,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_filter), r));
  Xen_define_typed_procedure(S_is_filter,		g_is_filter_w,             1, 0, 0, H_is_filter,		pl_bt);
  Xen_define_typed_procedure(S_make_fir_coeffs,		g_make_fir_coeffs_w,       2, 0, 0, H_make_fir_coeffs,		pl_fif);
  Xen_define_typed_procedure(S_fir_filter,		g_fir_filter_w,            1, 1, 0, H_fir_filter,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_fir_filter), r));
  Xen_define_typed_procedure(S_is_fir_filter,		g_is_fir_filter_w,         1, 0, 0, H_is_fir_filter,		pl_bt);
  Xen_define_typed_procedure(S_iir_filter,		g_iir_filter_w,            1, 1, 0, H_iir_filter,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_iir_filter), r));
  Xen_define_typed_procedure(S_is_iir_filter,		g_is_iir_filter_w,         1, 0, 0, H_is_iir_filter,		pl_bt);
  Xen_define_typed_procedure(S_mus_order,		g_mus_order_w,             1, 0, 0, H_mus_order,		pl_ic);
  Xen_define_typed_procedure(S_mus_type,		g_mus_type_w,              1, 0, 0, H_mus_type,			pl_ic);


  Xen_define_typed_procedure(S_is_env,			g_is_env_w,                1, 0, 0, H_is_env,			pl_bt);
  Xen_define_typed_procedure(S_env,			g_env_w,                   1, 0, 0, H_env,			
			     s7_make_signature(s7, 2, d, s7_make_symbol(s7, S_is_env)));
  Xen_define_typed_procedure(S_env_interp,		g_env_interp_w,            2, 0, 0, H_env_interp,		pl_drc);
  Xen_define_typed_procedure(S_envelope_interp,		g_envelope_interp_w,       2, 1, 0, H_envelope_interp,		pl_rrpr);
  Xen_define_unsafe_typed_procedure(S_env_any,		g_env_any_w,               2, 0, 0, H_env_any,                  pl_dct);

  Xen_define_typed_procedure(S_is_locsig,		g_is_locsig_w,             1, 0, 0, H_is_locsig,		pl_bt);
  Xen_define_typed_procedure(S_locsig,			g_locsig_w,                3, 0, 0, H_locsig,			
			     s7_make_signature(s7, 4, r, s7_make_symbol(s7, S_is_locsig), i, r));
  Xen_define_typed_procedure(S_move_locsig,		g_move_locsig_w,           3, 0, 0, H_move_locsig,		pl_ccrr);
  Xen_define_typed_procedure(S_mus_channels,		g_mus_channels_w,          1, 0, 0, H_mus_channels,		pl_it);

#if HAVE_RUBY
  Xen_define_procedure(S_locsig_ref,			g_locsig_ref_w,            2, 0, 0, H_locsig_ref);
  Xen_define_procedure(S_locsig_reverb_ref,		g_locsig_reverb_ref_w,     2, 0, 0, H_locsig_reverb_ref);
#endif

  Xen_define_typed_procedure(S_locsig_set,		g_locsig_set_w,            3, 0, 0, H_locsig_set,
			     s7_make_signature(s7, 4, d, s7_make_symbol(s7, S_is_locsig), i, r));

#if HAVE_SCHEME || HAVE_FORTH
  Xen_define_typed_dilambda(S_locsig_ref, g_locsig_ref_w, H_locsig_ref, 
			    S_set S_locsig_ref, g_locsig_set_w,  2, 0, 3, 0, pl_dci, pl_dcir);
  Xen_define_typed_dilambda(S_locsig_reverb_ref, g_locsig_reverb_ref_w, H_locsig_reverb_ref, 
			    S_locsig_reverb_set, g_locsig_reverb_set_w,  2, 0, 3, 0, pl_dci, pl_dcir);
#endif

  Xen_define_typed_procedure(S_locsig_reverb_set,	g_locsig_reverb_set_w,     3, 0, 0, H_locsig_reverb_set,	pl_rcir);
  Xen_define_typed_dilambda(S_locsig_type,   g_locsig_type_w, H_locsig_type, 
			    S_set S_locsig_type, g_set_locsig_type_w,  0, 0, 1, 0, pl_i, pl_i);

  Xen_define_typed_procedure(S_is_move_sound,		g_is_move_sound_w,         1, 0, 0, H_is_move_sound,		pl_bt);
  Xen_define_typed_procedure(S_move_sound,		g_move_sound_w,            3, 0, 0, H_move_sound,		
			     s7_make_circular_signature(s7, 2, 3, r, s7_make_symbol(s7, S_is_move_sound), r));
  Xen_define_typed_procedure(S_make_move_sound,		g_make_move_sound_w,       1, 2, 0, H_make_move_sound,		
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_move_sound), t));

  Xen_define_typed_procedure(S_is_file_to_sample,	g_is_file_to_sample_w,     1, 0, 0, H_is_file_to_sample,	pl_bt);
  Xen_define_typed_procedure(S_make_file_to_sample,	g_make_file_to_sample_w,   1, 1, 0, H_make_file_to_sample,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_file_to_sample), t));
  Xen_define_typed_procedure(S_file_to_sample,		g_file_to_sample_w,        2, 1, 0, H_file_to_sample,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_file_to_sample), i));
  Xen_define_typed_procedure(S_is_sample_to_file,	g_is_sample_to_file_w,     1, 0, 0, H_is_sample_to_file,	pl_bt);
  Xen_define_typed_procedure(S_make_sample_to_file,	g_make_sample_to_file_w,   1, 4, 0, H_make_sample_to_file,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_sample_to_file), t));
  Xen_define_typed_procedure(S_continue_sample_to_file, g_continue_sample_to_file_w, 1, 0, 0, H_continue_sample_to_file, pl_cs);
  Xen_define_typed_procedure(S_sample_to_file,		g_sample_to_file_w,        4, 0, 0, H_sample_to_file,		pl_rciir);
  Xen_define_typed_procedure(S_sample_to_file_add,	g_sample_to_file_add_w,    2, 0, 0, H_sample_to_file_add,	pl_cc);

  Xen_define_typed_procedure(S_is_file_to_frample,	g_is_file_to_frample_w,    1, 0, 0, H_is_file_to_frample,	pl_bt);
  Xen_define_typed_procedure(S_make_file_to_frample,	g_make_file_to_frample_w,  1, 1, 0, H_make_file_to_frample,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_file_to_frample), t));
  Xen_define_typed_procedure(S_file_to_frample,		g_file_to_frample_w,       2, 1, 0, H_file_to_frample,		pl_ccic);
  Xen_define_typed_procedure(S_continue_frample_to_file, g_continue_frample_to_file_w, 1, 0, 0, H_continue_frample_to_file, pl_cs);
  Xen_define_typed_procedure(S_is_frample_to_file,	g_is_frample_to_file_w,    1, 0, 0, H_is_frample_to_file,	pl_bt);
  Xen_define_typed_procedure(S_frample_to_file,		g_frample_to_file_w,       3, 0, 0, H_frample_to_file,		pl_fcif);
  Xen_define_typed_procedure(S_make_frample_to_file,	g_make_frample_to_file_w,  1, 4, 0, H_make_frample_to_file,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_frample_to_file), t));

  Xen_define_typed_procedure(S_is_mus_input,		g_is_mus_input_w,          1, 0, 0, H_is_mus_input,		pl_bt);
  Xen_define_typed_procedure(S_is_mus_output,		g_is_mus_output_w,         1, 0, 0, H_is_mus_output,		pl_bt);
  Xen_define_typed_procedure(S_in_any,			g_in_any_w,                3, 0, 0, H_in_any,			pl_diit);
  Xen_define_typed_procedure(S_ina,			g_ina_w,                   2, 0, 0, H_ina,			pl_dit);  
  Xen_define_typed_procedure(S_inb,			g_inb_w,                   2, 0, 0, H_inb,			pl_dit);
  Xen_define_typed_procedure(S_out_any,			g_out_any_w,               3, 1, 0, H_out_any,			pl_ririt);
  Xen_define_typed_procedure(S_outa,			g_outa_w,                  2, 1, 0, H_outa,			pl_dirt);
  Xen_define_typed_procedure(S_outb,			g_outb_w,                  2, 1, 0, H_outb,			pl_dirt);
  Xen_define_typed_procedure(S_outc,			g_outc_w,                  2, 1, 0, H_outc,			pl_dirt);
  Xen_define_typed_procedure(S_outd,			g_outd_w,                  2, 1, 0, H_outd,			pl_dirt);
  Xen_define_typed_procedure(S_mus_close,		g_mus_close_w,             1, 0, 0, H_mus_close,		pl_tc);

  Xen_define_typed_dilambda(S_mus_file_buffer_size, g_mus_file_buffer_size_w, H_mus_file_buffer_size,
			    S_set S_mus_file_buffer_size, g_mus_set_file_buffer_size_w,  0, 0, 1, 0, pl_i, pl_i);

  Xen_define_typed_procedure(S_is_readin,		g_is_readin_w,             1, 0, 0, H_is_readin,		pl_bt);
  Xen_define_typed_procedure(S_readin,			g_readin_w,                1, 0, 0, H_readin,			
			     s7_make_signature(s7, 2, d, s7_make_symbol(s7, S_is_readin)));
  Xen_define_typed_procedure(S_mus_channel,		g_mus_channel_w,           1, 0, 0, H_mus_channel,		pl_ic);
  Xen_define_typed_procedure(S_mus_interp_type,		g_mus_interp_type_w,       1, 0, 0, H_mus_interp_type,		pl_ic);

  Xen_define_typed_dilambda(S_mus_location,  g_mus_location_w, H_mus_location, 
			    S_set S_mus_location, g_mus_set_location_w,  1, 0, 2, 0, pl_ic, pl_ici);
  Xen_define_typed_dilambda(S_mus_increment, g_mus_increment_w, H_mus_increment, 
			    S_set S_mus_increment, g_mus_set_increment_w,  1, 0, 2, 0, pl_dc, pl_dcr);

  Xen_define_typed_procedure(S_is_granulate,		g_is_granulate_w,          1, 0, 0, H_is_granulate,		pl_bt);
  Xen_define_unsafe_typed_procedure(S_granulate,	g_granulate_w,             1, 2, 0, H_granulate,		
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_granulate), t));

  Xen_define_typed_dilambda(S_mus_ramp, g_mus_ramp_w, H_mus_ramp, 
			    S_set S_mus_ramp, g_mus_set_ramp_w,  1, 0, 2, 0, pl_dc, pl_dcr);

  Xen_define_typed_procedure(S_clear_sincs,		g_mus_clear_sincs_w,       0, 0, 0, "clears out any sinc tables", NULL);
  Xen_define_typed_procedure(S_is_src,			g_is_src_w,                1, 0, 0, H_is_src,			pl_bt);
  Xen_define_unsafe_typed_procedure(S_src,		g_src_w,                   1, 2, 0, H_src,			
				    s7_make_signature(s7, 4, d, s7_make_symbol(s7, S_is_src), r, t));

  Xen_define_typed_procedure(S_is_convolve,		g_is_convolve_w,           1, 0, 0, H_is_convolve,		pl_bt);
  Xen_define_unsafe_typed_procedure(S_convolve,		g_convolve_w,              1, 1, 0, H_convolve_gen,		
				    s7_make_signature(s7, 3, d, s7_make_symbol(s7, S_is_convolve), t));
  Xen_define_typed_procedure(S_convolve_files,		g_convolve_files_w,        2, 2, 0, H_convolve_files,		pl_sssrs);

  Xen_define_typed_procedure(S_is_phase_vocoder,	g_is_phase_vocoder_w,      1, 0, 0, H_is_phase_vocoder,		pl_bt);
  Xen_define_unsafe_typed_procedure(S_phase_vocoder,    g_phase_vocoder_w,         1, 4, 0, H_phase_vocoder,		
				    s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_phase_vocoder), t));
  Xen_define_typed_procedure(S_phase_vocoder_amp_increments, g_phase_vocoder_amp_increments_w, 1, 0, 0, H_phase_vocoder_amp_increments, pl_fc);
  Xen_define_typed_procedure(S_phase_vocoder_amps,	g_phase_vocoder_amps_w,    1, 0, 0, H_phase_vocoder_amps,	pl_fc);
  Xen_define_typed_procedure(S_phase_vocoder_freqs,	g_phase_vocoder_freqs_w,   1, 0, 0, H_phase_vocoder_freqs,	pl_fc);
  Xen_define_typed_procedure(S_phase_vocoder_phases,	g_phase_vocoder_phases_w,  1, 0, 0, H_phase_vocoder_phases,	pl_fc);
  Xen_define_typed_procedure(S_phase_vocoder_phase_increments, g_phase_vocoder_phase_increments_w, 1, 0, 0, H_phase_vocoder_phase_increments, pl_fc);

  Xen_define_typed_dilambda(S_mus_hop, g_mus_hop_w, H_mus_hop, 
			    S_set S_mus_hop, g_mus_set_hop_w,  1, 0, 2, 0, pl_dc, pl_dcr);

  Xen_define_typed_procedure(S_ssb_am,			g_ssb_am_w,                1, 2, 0, H_ssb_am,			
			     s7_make_circular_signature(s7, 2, 3, d, s7_make_symbol(s7, S_is_ssb_am), r));
  Xen_define_typed_procedure(S_is_ssb_am,		g_is_ssb_am_w,             1, 0, 0, H_is_ssb_am,		pl_bt);

  Xen_define_typed_procedure(S_is_mus_generator,	g_is_mus_generator_w,      1, 0, 0, H_is_mus_generator,		pl_bt);

  Xen_define_variable(S_output, clm_output, Xen_false);
  Xen_define_variable(S_reverb, clm_reverb, Xen_false);

#if HAVE_SCHEME
  {
    s7_pointer clm_output_accessor, clm_reverb_accessor;
    /* these are globals in s7, so they aren't going to move */
    clm_output_slot = s7_slot(s7, clm_output);
    clm_reverb_slot = s7_slot(s7, clm_reverb);
    out_any_2 = out_any_2_to_mus_xen;
    /* these can't be safe functions */
    clm_output_accessor = s7_make_function(s7, "(set " S_output ")", g_clm_output_set, 2, 0, false, "called if " S_output " is set");
    s7_set_setter(s7, s7_make_symbol(s7, S_output), clm_output_accessor);

    clm_reverb_accessor = s7_make_function(s7, "(set " S_reverb ")", g_clm_reverb_set, 2, 0, false, "called if " S_reverb " is set");
    s7_set_setter(s7, s7_make_symbol(s7, S_reverb), clm_reverb_accessor);
  }
#endif

#if HAVE_SCHEME && (!_MSC_VER)
  Xen_define_typed_procedure(S_get_internal_real_time, g_get_internal_real_time_w, 0, 0, 0, H_get_internal_real_time, NULL);
  Xen_define_constant(S_internal_time_units_per_second, 1, "units used by " S_get_internal_real_time);
#endif

  #define star_sig(Checker) s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, Checker), t)

#if HAVE_SCHEME
  Xen_define_typed_procedure(S_piano_noise,		g_piano_noise_w,            2, 0, 0, H_piano_noise,		pl_djr);
  Xen_define_typed_procedure(S_singer_filter,		g_singer_filter_w,          6, 0, 0, H_singer_filter,		pl_riirfff);
  Xen_define_typed_procedure(S_singer_nose_filter,	g_singer_nose_filter_w,     5, 0, 0, H_singer_nose_filter,	pl_rirfff);

  s7_define_typed_function_star(s7, S_make_env, g_make_env, 
				"envelope (scaler 1.0) duration (offset 0.0) (base 1.0) end length",
				H_make_env, 
				star_sig(S_is_env));
  make_env = s7_name_to_value(s7, S_make_env);

  s7_define_typed_function_star(s7, S_make_polywave, g_make_polywave, 
				"(frequency 0.0) (partials '(1 1)) (type 1) xcoeffs ycoeffs)",
				H_make_polywave, 
				star_sig(S_is_polywave));
  s7_define_typed_function_star(s7, S_make_polyshape, g_make_polyshape, 
				"(frequency 0.0) (initial-phase 0.0) coeffs partials kind",
				H_make_polyshape, 
				star_sig(S_is_polyshape));
  s7_define_typed_function_star(s7, S_make_oscil, g_make_oscil, 
				"(frequency 0.0) (initial-phase 0.0)", 
				H_make_oscil, 
				star_sig(S_is_oscil));
  s7_define_typed_function_star(s7, S_make_ssb_am, g_make_ssb_am, 
				"(frequency 0.0) (order 40)", 
				H_make_ssb_am,
				star_sig(S_is_ssb_am)); 
  s7_define_typed_function_star(s7, S_make_ncos, g_make_ncos, 
				"(frequency 0.0) (n 1)", 
				H_make_ncos,
				star_sig(S_is_ncos));
  s7_define_typed_function_star(s7, S_make_nsin, g_make_nsin, 
				"(frequency 0.0) (n 1)", 
				H_make_nsin,
				star_sig(S_is_nsin));
  s7_define_typed_function_star(s7, S_make_asymmetric_fm, g_make_asymmetric_fm, 
				"(frequency 0.0) (initial-phase 0.0) (r 1.0) (ratio 1.0)",
				H_make_asymmetric_fm, 
				star_sig(S_is_asymmetric_fm));
  s7_define_typed_function_star(s7, S_make_nrxysin, g_make_nrxysin, 
				"(frequency 0.0) (ratio 1.0) (n 1) (r 0.5))", 
				H_make_nrxysin,
				star_sig(S_is_nrxysin));
  s7_define_typed_function_star(s7, S_make_nrxycos, g_make_nrxycos, 
				"(frequency 0.0) (ratio 1.0) (n 1) (r 0.5))", 
				H_make_nrxycos,
				star_sig(S_is_nrxycos));
  s7_define_typed_function_star(s7, S_make_rxyksin, g_make_rxyksin, 
				"(frequency 0.0) (ratio 1.0) (r 0.5))", 
				H_make_rxyksin,
				star_sig(S_is_rxyksin));
  s7_define_typed_function_star(s7, S_make_rxykcos, g_make_rxykcos, 
				"(frequency 0.0) (ratio 1.0) (r 0.5))", 
				H_make_rxykcos,
				star_sig(S_is_rxykcos));

  s7_define_typed_function_star(s7, S_make_sawtooth_wave, g_make_sawtooth_wave, 
				"(frequency 0.0) (amplitude 1.0) (initial-phase 3.141592653589793)", 
				H_make_sawtooth_wave, 
				star_sig(S_is_sawtooth_wave));
  s7_define_typed_function_star(s7, S_make_square_wave, g_make_square_wave, 
				"(frequency 0.0) (amplitude 1.0) (initial-phase 0.0)", 
				H_make_square_wave,
				star_sig(S_is_square_wave));
  s7_define_typed_function_star(s7, S_make_triangle_wave, g_make_triangle_wave, 
				"(frequency 0.0) (amplitude 1.0) (initial-phase 0.0)", 
				H_make_triangle_wave, 
				star_sig(S_is_triangle_wave));
  s7_define_typed_function_star(s7, S_make_pulse_train, g_make_pulse_train, 
				"(frequency 0.0) (amplitude 1.0) (initial-phase 6.283185307179586)", 
				H_make_pulse_train, 
				star_sig(S_is_pulse_train));

  s7_define_typed_function_star(s7, S_make_one_zero, g_make_one_zero, 
				"(a0 0.0) (a1 0.0)", 
				H_make_one_zero,
                                star_sig(S_is_one_zero));
  s7_define_typed_function_star(s7, S_make_one_pole, g_make_one_pole, 
				"(a0 0.0) (b1 0.0)", 
				H_make_one_pole,
                                star_sig(S_is_one_pole));

  s7_define_typed_function(s7, S_make_two_zero, g_make_two_zero, 0, 6, 0, H_make_two_zero, star_sig(S_is_two_zero));
  make_two_zero_a = s7_make_safe_function_star(s7, S_make_two_zero, g_make_two_zero_a, "(a0 0.0) (a1 0.0) (a2 0.0)", H_make_two_zero);
  make_two_zero_f = s7_make_safe_function_star(s7, S_make_two_zero, g_make_two_zero_f, "(frequency 0.0) (radius 0.0)", H_make_two_zero);

  s7_define_typed_function(s7, S_make_two_pole, g_make_two_pole, 0, 6, 0, H_make_two_pole, star_sig(S_is_two_pole));
  make_two_pole_a = s7_make_safe_function_star(s7, S_make_two_pole, g_make_two_pole_a, "(a0 0.0) (b1 0.0) (b2 0.0)", H_make_two_pole);
  make_two_pole_f = s7_make_safe_function_star(s7, S_make_two_pole, g_make_two_pole_f, "(frequency 0.0) (radius 0.0)", H_make_two_pole);

  s7_define_typed_function_star(s7, S_make_filter, g_make_filter, 
				"order xcoeffs ycoeffs",
				H_make_filter,
                                star_sig(S_is_filter));
  s7_define_typed_function_star(s7, S_make_fir_filter, g_make_fir_filter, 
				"order xcoeffs",
				H_make_fir_filter,
                                star_sig(S_is_fir_filter));
  s7_define_typed_function_star(s7, S_make_iir_filter, g_make_iir_filter, 
				"order ycoeffs",
				H_make_iir_filter,
                                star_sig(S_is_iir_filter));

  s7_define_typed_function_star(s7, S_make_formant, g_make_formant, 
				"(frequency 0.0) (radius 0.0)", 
				H_make_formant,
				star_sig(S_is_formant));
  s7_define_typed_function_star(s7, S_make_firmant, g_make_firmant, 
				"(frequency 0.0) (radius 0.0)", 
				H_make_firmant,
				star_sig(S_is_firmant));

  s7_define_typed_function_star(s7, S_make_delay, g_make_delay, 
				"size initial-contents initial-element max-size type",
				H_make_delay,
				star_sig(S_is_delay));
  s7_define_typed_function_star(s7, S_make_comb, g_make_comb, 
				"(scaler 1.0) size initial-contents initial-element max-size type", 
				H_make_comb,
				star_sig(S_is_comb));
  s7_define_typed_function_star(s7, S_make_filtered_comb, g_make_filtered_comb, 
				"(scaler 1.0) size initial-contents initial-element max-size type filter", 
				H_make_filtered_comb,
				star_sig(S_is_filtered_comb));
  s7_define_typed_function_star(s7, S_make_notch, g_make_notch, 
				"(scaler 1.0) size initial-contents initial-element max-size type", 
				H_make_notch,
				star_sig(S_is_notch));
  s7_define_typed_function_star(s7, S_make_all_pass, g_make_all_pass, 
				"(feedback 0.0) (feedforward 0.0) size initial-contents initial-element max-size type", 
				H_make_all_pass,
				star_sig(S_is_all_pass));
  s7_define_typed_function_star(s7, S_make_moving_average, g_make_moving_average, 
				"size initial-contents initial-element", 
				H_make_moving_average,
				star_sig(S_is_moving_average));
  s7_define_typed_function_star(s7, S_make_moving_max, g_make_moving_max, 
				"size initial-contents initial-element", 
				H_make_moving_max,
				star_sig(S_is_moving_max));
  s7_define_typed_function_star(s7, S_make_moving_norm, g_make_moving_norm, 
				"size (scaler 1.0) initial-contents initial-element", 
				H_make_moving_norm,
				star_sig(S_is_moving_norm));
  s7_define_typed_function_star(s7, S_make_readin, g_make_readin, 
				"file (channel 0) (start 0) (direction 1) size",
				H_make_readin,
				star_sig(S_is_readin));
  s7_define_typed_function_star(s7, S_make_convolve, g_make_convolve, 
				"input filter fft-size",
				H_make_convolve,
				star_sig(S_is_convolve));
  s7_define_typed_function_star(s7, S_make_rand, g_make_rand, 
				"(frequency 0.0) (amplitude 1.0) envelope distribution size",
				H_make_rand,
				star_sig(S_is_rand));
  s7_define_typed_function_star(s7, S_make_rand_interp, g_make_rand_interp, 
				"(frequency 0.0) (amplitude 1.0) envelope distribution size",
				H_make_rand_interp,
				star_sig(S_is_rand_interp));
  s7_define_typed_function_star(s7, S_make_src, g_make_src, 
				"input (srate 1.0) (width 10)",
				H_make_src,
				star_sig(S_is_src));
  s7_define_typed_function_star(s7, S_make_table_lookup, g_make_table_lookup, 
				"(frequency 0.0) (initial-phase 0.0) wave size type)",
				H_make_table_lookup,
				star_sig(S_is_table_lookup));
  s7_define_typed_function_star(s7, S_make_wave_train, g_make_wave_train, 
				"(frequency 0.0) (initial-phase 0.0) wave size type",
				H_make_wave_train,
				star_sig(S_is_wave_train));
  s7_define_typed_function_star(s7, S_make_phase_vocoder, g_make_phase_vocoder, 
				"input fft-size overlap interp pitch analyze edit synthesize",
				H_make_phase_vocoder,
				star_sig(S_is_phase_vocoder));
  s7_define_typed_function_star(s7, S_make_granulate, g_make_granulate, 
				"input (expansion 1.0) (length .15) (scaler .6) (hop .05) (ramp .4) (jitter 1.0) max-size edit)",
				H_make_granulate,
				star_sig(S_is_granulate));
  s7_define_typed_function_star(s7, S_make_locsig, g_make_locsig, 
				"(degree 0.0) (distance 1.0) (reverb 0.0) output revout channels type",
				H_make_locsig,
				star_sig(S_is_locsig));
#else
  Xen_define_typed_procedure(S_make_env,          g_make_env_w,           0, 0, 1, H_make_env,         star_sig(S_is_env));
  Xen_define_typed_procedure(S_make_polywave,     g_make_polywave_w,      0, 0, 1, H_make_polywave,    star_sig(S_is_polywave));
  Xen_define_typed_procedure(S_make_polyshape,    g_make_polyshape_w,     0, 0, 1, H_make_polyshape,   star_sig(S_is_polyshape));
  Xen_define_typed_procedure(S_make_oscil,        g_make_oscil_w,         0, 4, 0, H_make_oscil,       star_sig(S_is_oscil));
  Xen_define_typed_procedure(S_make_ssb_am,       g_make_ssb_am_w,        0, 4, 0, H_make_ssb_am,      star_sig(S_is_ssb_am)); 
  Xen_define_typed_procedure(S_make_ncos,         g_make_ncos_w,          0, 4, 0, H_make_ncos,        star_sig(S_is_ncos)); 
  Xen_define_typed_procedure(S_make_nsin,         g_make_nsin_w,          0, 4, 0, H_make_nsin,        star_sig(S_is_nsin)); 
  Xen_define_typed_procedure(S_make_asymmetric_fm,g_make_asymmetric_fm_w, 0, 8, 0, H_make_asymmetric_fm, star_sig(S_is_asymmetric_fm));
  Xen_define_typed_procedure(S_make_nrxysin,      g_make_nrxysin_w,       0, 0, 1, H_make_nrxysin,     star_sig(S_is_nrxysin));
  Xen_define_typed_procedure(S_make_nrxycos,      g_make_nrxycos_w,       0, 0, 1, H_make_nrxycos,     star_sig(S_is_nrxycos));
  Xen_define_typed_procedure(S_make_rxyksin,      g_make_rxyksin_w,       0, 0, 1, H_make_rxyksin,     star_sig(S_is_rxyksin));
  Xen_define_typed_procedure(S_make_rxykcos,      g_make_rxykcos_w,       0, 0, 1, H_make_rxykcos,     star_sig(S_is_rxykcos));
  Xen_define_typed_procedure(S_make_one_zero,     g_make_one_zero_w,      0, 4, 0, H_make_one_zero,    star_sig(S_is_one_zero));
  Xen_define_typed_procedure(S_make_one_pole,     g_make_one_pole_w,      0, 4, 0, H_make_one_pole,    star_sig(S_is_one_pole));
  Xen_define_typed_procedure(S_make_two_zero,     g_make_two_zero_w,      0, 6, 0, H_make_two_zero,    star_sig(S_is_two_zero));
  Xen_define_typed_procedure(S_make_two_pole,     g_make_two_pole_w,      0, 6, 0, H_make_two_pole,    star_sig(S_is_two_pole));
  Xen_define_typed_procedure(S_make_formant,      g_make_formant_w,       0, 4, 0, H_make_formant,     star_sig(S_is_formant));
  Xen_define_typed_procedure(S_make_firmant,      g_make_firmant_w,       0, 4, 0, H_make_firmant,     star_sig(S_is_firmant));
  Xen_define_typed_procedure(S_make_pulse_train,  g_make_pulse_train_w,   0, 6, 0, H_make_pulse_train, star_sig(S_is_pulse_train));
  Xen_define_typed_procedure(S_make_sawtooth_wave,g_make_sawtooth_wave_w, 0, 6, 0, H_make_sawtooth_wave, star_sig(S_is_sawtooth_wave));
  Xen_define_typed_procedure(S_make_triangle_wave,g_make_triangle_wave_w, 0, 6, 0, H_make_triangle_wave, star_sig(S_is_triangle_wave));
  Xen_define_typed_procedure(S_make_square_wave,  g_make_square_wave_w,   0, 6, 0, H_make_square_wave,   star_sig(S_is_square_wave));
  Xen_define_typed_procedure(S_make_delay,        g_make_delay_w,         0, 0, 1, H_make_delay,       star_sig(S_is_delay));
  Xen_define_typed_procedure(S_make_comb,         g_make_comb_w,          0, 0, 1, H_make_comb,        star_sig(S_is_comb));
  Xen_define_typed_procedure(S_make_filtered_comb,g_make_filtered_comb_w, 0, 0, 1, H_make_filtered_comb, star_sig(S_is_filtered_comb));
  Xen_define_typed_procedure(S_make_notch,        g_make_notch_w,         0, 0, 1, H_make_notch,       star_sig(S_is_notch)); 
  Xen_define_typed_procedure(S_make_all_pass,     g_make_all_pass_w,      0, 0, 1, H_make_all_pass,    star_sig(S_is_all_pass));
  Xen_define_typed_procedure(S_make_moving_average,g_make_moving_average_w,0, 0, 1, H_make_moving_average, star_sig(S_is_moving_average));
  Xen_define_typed_procedure(S_make_moving_max,   g_make_moving_max_w,    0, 0, 1, H_make_moving_max,  star_sig(S_is_moving_max));
  Xen_define_typed_procedure(S_make_moving_norm,  g_make_moving_norm_w,   0, 0, 1, H_make_moving_norm, star_sig(S_is_moving_norm));
  Xen_define_typed_procedure(S_make_readin,       g_make_readin_w,        0, 0, 1, H_make_readin,      star_sig(S_is_readin));
  Xen_define_typed_procedure(S_make_convolve,     g_make_convolve_w,      0, 0, 1, H_make_convolve,    star_sig(S_is_convolve));
  Xen_define_typed_procedure(S_make_rand,         g_make_rand_w,          0, 0, 1, H_make_rand,	       star_sig(S_is_rand));
  Xen_define_typed_procedure(S_make_rand_interp,  g_make_rand_interp_w,   0, 0, 1, H_make_rand_interp, star_sig(S_is_rand_interp));
  Xen_define_typed_procedure(S_make_src,          g_make_src_w,           0, 6, 0, H_make_src,         star_sig(S_is_src));
  Xen_define_typed_procedure(S_make_table_lookup, g_make_table_lookup_w,  0, 0, 1, H_make_table_lookup,star_sig(S_is_table_lookup));
  Xen_define_typed_procedure(S_make_filter,       g_make_filter_w,        0, 6, 0, H_make_filter,      star_sig(S_is_filter));
  Xen_define_typed_procedure(S_make_fir_filter,   g_make_fir_filter_w,    0, 4, 0, H_make_fir_filter,  star_sig(S_is_fir_filter));
  Xen_define_typed_procedure(S_make_iir_filter,   g_make_iir_filter_w,    0, 4, 0, H_make_iir_filter,  star_sig(S_is_iir_filter));
  Xen_define_typed_procedure(S_make_wave_train,   g_make_wave_train_w,    0, 0, 1, H_make_wave_train,  star_sig(S_is_wave_train));
  Xen_define_typed_procedure(S_make_phase_vocoder,g_make_phase_vocoder_w, 0, 0, 1, H_make_phase_vocoder, star_sig(S_is_phase_vocoder));
  Xen_define_typed_procedure(S_make_granulate,    g_make_granulate_w,     0, 0, 1, H_make_granulate,   star_sig(S_is_granulate));
  Xen_define_typed_procedure(S_make_locsig,       g_make_locsig_w,        0, 0, 1, H_make_locsig,      star_sig(S_is_locsig));
#endif

  Xen_define_typed_procedure(S_make_oscil_bank,         g_make_oscil_bank_w,        2, 2, 0, H_make_oscil_bank,	
			     s7_make_circular_signature(s7, 1, 2, s7_make_symbol(s7, S_is_oscil_bank), t));

  Xen_define_typed_procedure(S_mus_file_mix, 	        g_mus_file_mix_w,           0, 0, 1, H_mus_file_mix,		NULL); /* actually 2 0 1? */
  Xen_define_typed_procedure(S_mus_file_mix_with_envs,	g_mus_file_mix_with_envs_w, 0, 0, 1, H_mus_file_mix_with_envs,	NULL); /* actually 8 2 0 I think */
  Xen_define_typed_procedure(S_frample_to_frample,	g_frample_to_frample_w,     5, 0, 0, H_frample_to_frample,	pl_fffifi);

#if HAVE_SCHEME
  init_choosers(s7);
#endif


  /* -------- clm-print (see also snd-xen.c) -------- */
#if (!USE_SND)

#if HAVE_FORTH
Xen_eval_C_string("<'> fth-print alias clm-print ( fmt args -- )"); 
#endif

#if HAVE_RUBY
  Xen_eval_C_string("def clm_print(str, *args)\n\
                      $stdout.print format(str, *args)\n\
                      end");
#endif
#endif

  Xen_provide_feature("clm");
  {
    char *clm_version;
    clm_version = mus_format("clm%d", MUS_VERSION);
    Xen_provide_feature(clm_version);
    free(clm_version);
  }

#if HAVE_SCHEME && (!_MSC_VER)
  {
    struct timezone z0;
    gettimeofday(&overall_start_time, &z0);
  }
#endif
}


void Init_sndlib(void)
{
  mus_sndlib_xen_initialize();
  mus_vct_init();
  mus_xen_init();

#if HAVE_SCHEME
  if (sizeof(mus_float_t) != sizeof(s7_double))
    fprintf(stderr, "in s7-clm, s7_double must match mus_float_t.  Currently s7_double has %d bytes, but mus_float_t has %d\n", 
	    (int)sizeof(s7_double), 
	    (int)sizeof(mus_float_t));
#endif
}

#if HAVE_SCHEME
void s7_init_sndlib(s7_scheme *sc)
{
  s7_xen_initialize(sc);
  Init_sndlib();
}
#endif
