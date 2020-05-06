/* vct support 
 *
 * a vct is an object containing a mus_float_t array and its size
 *
 * C side:
 *   void mus_vct_init(void)                    called to declare the various functions and the vct type
 *   bool mus_is_vct(Xen obj)                   is obj a vct
 *   Xen xen_make_vct(int len, mus_float_t *data)     make a new vct
 *   Xen xen_make_vct_wrapper(int len, mus_float_t *data) make a new vct that doesn't free data when garbage collector strikes
 *   vct *xen_to_vct(Xen arg)                   given Xen arg, return vct
 *   void mus_vct_set_print_length(int val)     set vct print length (default 10) (also mus_vct_print_length)
 *
 *   (make-vct len (filler 0.0))      make new vct
 *   (vct? obj)                       is obj a vct
 *   (vct-ref v index)                return v[index]
 *   (vct-set! v index val)           v[index] = val
 *   (vct-copy v)                     return a copy of v
 *   (vct-length v)                   return length of v
 *   (vct-add! v1 v2 (offset 0))      v1[i+offset] = v1[i+offset] + v2[i] -> v1
 *   (vct-subtract! v1 v2)            v1[i] = v1[i] - v2[i] -> v1
 *   (vct-offset! v1 scl)             v1[i] += scl -> v1
 *   (vct-multiply! v1 v2)            v1[i] *= v2[i] -> v1
 *   (vct-scale! v1 scl)              v1[i] *= scl -> v1
 *   (vct-abs! v)                     v[i] = abs(v[i])
 *   (vct-fill! v1 val)               v1[i] = val -> v1
 *   (vct-map! v1 proc)               set each element of v1 to value of function proc()
 *   (vct-peak v1)                    max val (abs) in v
 *   (vct-equal? v1 v2 diff)          is element-wise relative-difference of v1 and v2 ever greater than diff?
 *   (list->vct lst)                  return vct with elements of list lst
 *   (vct->list v1)                   return list with elements of vct v1
 *   (vector->vct vect)               return vct with elements of vector vect
 *   (vct->vector v)                  return vector of vct contents
 *   (vct-move! v new old)            v[new++] = v[old++] -> v
 *   (vct-subseq v start end vnew)    vnew = v[start..end]
 *   (vct-reverse! v (len #f))        reverse contents (using len as end point if given)
 *   (vct->string v)                  scheme-readable description of vct
 *
 *   (vct* obj1 obj2) combines vct-multiply and vct-scale
 *   (vct+ obj1 obj2) combines vct-add and vct-offset
 *
 * The intended use is a sort of latter-day array-processing system that handles huge
 * one-dimensional vectors -- fft's, etc.  Some of these functions can be found in
 * the Snd package; others can be found in the CLM package (clm2xen.c).
 */

#include "mus-config.h"

#if USE_SND
  #include "snd.h"
#endif

#include <stddef.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if _MSC_VER
  #pragma warning(disable: 4244)
#endif

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "clm2xen.h"
#include "vct.h"

#if (!HAVE_SCHEME)
struct vct {
  mus_long_t length;
  mus_float_t *data;
  bool dont_free;
};

mus_long_t mus_vct_length(vct *v) {return(v->length);}
mus_float_t *mus_vct_data(vct *v) {return(v->data);}
#endif

#if HAVE_SCHEME

#define S_make_vct       "make-float-vector"
#define S_vct_add        "float-vector-add!"
#define S_vct_subtract   "float-vector-subtract!"
#define S_vct_copy       "float-vector-copy"
#define S_vct_length     "float-vector-length"
#define S_vct_multiply   "float-vector-multiply!"
#define S_vct_offset     "float-vector-offset!"
#define S_vct_ref        "float-vector-ref"
#define S_vct_scale      "float-vector-scale!"
#define S_vct_abs        "float-vector-abs!"
#define S_vct_fill       "float-vector-fill!"
#define S_vct_set        "float-vector-set!"
#define S_vct_peak       "float-vector-peak"
#define S_vct_equal      "float-vector-equal?"
#define S_is_vct         "float-vector?"
#define S_list_to_vct    "list->float-vector"
#define S_vct_to_list    "float-vector->list"
#define S_vector_to_vct  "vector->float-vector"
#define S_vct_to_vector  "float-vector->vector"
#define S_vct_move       "float-vector-move!"
#define S_vct_subseq     "float-vector-subseq"
#define S_vct_reverse    "float-vector-reverse!"
#define S_vct_to_string  "float-vector->string"
#define S_vct_times      "float-vector*"
#define S_vct_plus       "float-vector+"
#define A_VCT            "a float-vector"
#else
#define S_make_vct       "make-vct"
#define S_vct_add        "vct-add!"
#define S_vct_subtract   "vct-subtract!"
#define S_vct_copy       "vct-copy"
#define S_vct_length     "vct-length"
#define S_vct_multiply   "vct-multiply!"
#define S_vct_offset     "vct-offset!"
#define S_vct_ref        "vct-ref"
#define S_vct_scale      "vct-scale!"
#define S_vct_abs        "vct-abs!"
#define S_vct_fill       "vct-fill!"
#define S_vct_set        "vct-set!"
#define S_vct_peak       "vct-peak"
#define S_vct_equal      "vct-equal?"
#define S_is_vct         "vct?"
#define S_list_to_vct    "list->vct"
#define S_vct_to_list    "vct->list"
#define S_vector_to_vct  "vector->vct"
#define S_vct_to_vector  "vct->vector"
#define S_vct_move       "vct-move!"
#define S_vct_subseq     "vct-subseq"
#define S_vct_reverse    "vct-reverse!"
#define S_vct_to_string  "vct->string"
#if HAVE_RUBY
  #define S_vct_times    "vct_multiply"
  #define S_vct_plus     "vct_add"
#else
  #define S_vct_times    "vct*"
  #define S_vct_plus     "vct+"
#endif
#define A_VCT            "a vct"
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

#if USE_SND
  #define VCT_PRINT_LENGTH DEFAULT_PRINT_LENGTH
#else
  #define VCT_PRINT_LENGTH 10
#endif


#if WITH_VECTORIZE
void mus_clear_floats(mus_float_t *dst, mus_long_t len)
{
  mus_long_t k;
  for (k = 0; k < len; k++) dst[k] = 0.0;
}

void mus_copy_floats(mus_float_t *dst, mus_float_t *src, mus_long_t len)
{
  mus_long_t k;
  for (k = 0; k < len; k++) dst[k] = src[k];
}

void mus_add_floats(mus_float_t *dst, mus_float_t *src, mus_long_t len)
{
  mus_long_t k;
  for (k = 0; k < len; k++) dst[k] += src[k];
}

void mus_abs_floats(mus_float_t *dst, mus_long_t len)
{
  mus_long_t k;
  for (k = 0; k < len; k++) dst[k] = fabs(dst[k]);
}
#endif


static int vct_print_length = VCT_PRINT_LENGTH;

void mus_vct_set_print_length(int len) 
{
  vct_print_length = len;
}

int mus_vct_print_length(void) 
{
  return(vct_print_length);
}


vct *xen_to_vct(Xen arg)
{
  if (mus_is_vct(arg))
    return((vct *)Xen_to_vct(arg));
  return(NULL);
}


#define VCT_PRINT_BUFFER_SIZE 64


#if (!HAVE_SCHEME)

static Xen_object_type_t vct_tag;

bool mus_is_vct(Xen obj)
{
  return(Xen_c_object_is_type(obj, vct_tag));
}


static void vct_free(vct *v)
{
  if (v)
    {
      if ((!(v->dont_free)) && 
	  (v->data)) 
	free(v->data);
      v->data = NULL;
      free(v);
    }
}

Xen_wrap_free(vct, free_vct, vct_free)

static char *mus_vct_to_string(vct *v)
{
  mus_long_t len, size;
  char *buf;
  char flt[VCT_PRINT_BUFFER_SIZE];
  mus_float_t *d;

  if (!v) return(NULL);
  len = vct_print_length;
  if (len > mus_vct_length(v)) len = mus_vct_length(v);
  d = mus_vct_data(v);
  size = (len + 1) * VCT_PRINT_BUFFER_SIZE;
  buf = (char *)calloc(size, sizeof(char));
  snprintf(buf, size, "#<vct[len=%" print_mus_long "]", mus_vct_length(v));

  if ((len > 0) && (d))
    {
      int i;
      strcat(buf, ":");
      for (i = 0; i < len; i++)
	{
	  snprintf(flt, VCT_PRINT_BUFFER_SIZE, " %.3f", d[i]);
	  strcat(buf, flt);
	}
      if (mus_vct_length(v) > vct_print_length)
	strcat(buf, " ...");
    }
  strcat(buf, ">");
  return(buf);
}
#endif


char *mus_vct_to_readable_string(vct *v)
{
  int i, len, size;
  char *buf;
  char flt[VCT_PRINT_BUFFER_SIZE];
  mus_float_t *d;

  if (!v) return(NULL);
  len = (int)(mus_vct_length(v));
  size = (len + 1) * VCT_PRINT_BUFFER_SIZE;
  buf = (char *)calloc(size, sizeof(char));
  d = mus_vct_data(v);

#if HAVE_SCHEME
  snprintf(buf, size, "(float-vector");
#endif
#if HAVE_RUBY || HAVE_FORTH
  snprintf(buf, size, "vct(");
#endif

  for (i = 0; i < len; i++)
    {
#if HAVE_SCHEME || HAVE_FORTH
      snprintf(flt, VCT_PRINT_BUFFER_SIZE, " %.3f", d[i]);
#endif
#if HAVE_RUBY
      snprintf(flt, VCT_PRINT_BUFFER_SIZE, "%.3f%s", d[i], i + 1 < len ? ", " : "");
#endif
      strcat(buf, flt);
    }

#if HAVE_FORTH
  strcat(buf, " ");
#endif
  strcat(buf, ")");

  return(buf);
}


static Xen g_vct_to_readable_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define H_vct_to_string "(" S_vct_to_string " v): readable description of v"

  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_to_string, A_VCT);

  vstr = mus_vct_to_readable_string(Xen_to_vct(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}

bool mus_vct_is_equal(vct *v1, vct *v2)
{
  if (v1 == v2) return(true);
  return((mus_vct_length(v1) == mus_vct_length(v2)) &&
	 (mus_arrays_are_equal(mus_vct_data(v1), mus_vct_data(v2), 
			       mus_float_equal_fudge_factor(),
			       mus_vct_length(v1))));
}


#if (!HAVE_SCHEME)

static Xen g_is_vct(Xen obj) 
{
  #define H_is_vct "(" S_is_vct " obj): is obj a " S_vct
  return(C_bool_to_Xen_boolean(mus_is_vct(obj)));
}

Xen_wrap_print(vct, print_vct, mus_vct_to_string)

static Xen equalp_vct(Xen obj1, Xen obj2)
{
  if ((!(mus_is_vct(obj1))) || (!(mus_is_vct(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(mus_vct_is_equal(Xen_to_vct(obj1), Xen_to_vct(obj2))));
}

vct *mus_vct_make(mus_long_t len)
{
  vct *new_vct;
  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  if (len > 0)
    new_vct->data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
  else new_vct->data = NULL;
  new_vct->dont_free = false;
  return(new_vct);
}


vct *mus_vct_wrap(mus_long_t len, mus_float_t *data)
{
  vct *new_vct;
  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = true;
  return(new_vct);
}


vct *mus_vct_free(vct *v) 
{
  vct_free(v);
  return(NULL);
}


Xen xen_make_vct(mus_long_t len, mus_float_t *data)
{
  vct *new_vct;

  if (len < 0) return(Xen_false);
  if ((len > 0) && 
      (!data))
    Xen_error(Xen_make_error_type("out-of-memory"),
	      Xen_list_2(C_string_to_Xen_string(S_make_vct ": can't allocate size ~A"),
			 C_int_to_Xen_integer(len)));

  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = false;
  return(Xen_make_object(vct_tag, new_vct, 0, free_vct));
}


Xen xen_make_vct_wrapper(mus_long_t len, mus_float_t *data)
{
  vct *new_vct;
  new_vct = (vct *)malloc(sizeof(vct));
  new_vct->length = len;
  new_vct->data = data;
  new_vct->dont_free = true;
  return(Xen_make_object(vct_tag, new_vct, 0, free_vct));
}


Xen vct_to_xen(vct *v)
{
  return(Xen_make_object(vct_tag, v, 0, free_vct));
}


static Xen g_vct_fill(Xen obj, Xen val);

static Xen g_make_vct(Xen len, Xen filler)
{
  #if HAVE_RUBY
    #define vct_make_example "v = make_vct(32, 1.0)"
  #endif
  #if HAVE_FORTH
    #define vct_make_example "32 1.0 make-vct value v"
  #endif
  #if HAVE_SCHEME
    #define vct_make_example "(make-float-vector 32 1.0)"
  #endif

  #define H_make_vct "(" S_make_vct " len :optional (initial-element 0)): returns a new " S_vct " of length len filled with \
initial-element: \n  " vct_make_example

  mus_long_t size;
  Xen_check_type(Xen_is_llong(len), len, 1, S_make_vct, "an integer");
  Xen_check_type(Xen_is_number(filler) || !Xen_is_bound(filler), filler, 2, S_make_vct, "a number");

  size = Xen_llong_to_C_llong(len);
  if (size < 0) 
    Xen_out_of_range_error(S_make_vct, 1, len, "new vct size < 0?");

  if ((size > mus_max_malloc()) ||
      (((mus_long_t)(size * sizeof(mus_float_t))) > mus_max_malloc()))
    Xen_out_of_range_error(S_make_vct, 1, len, "new vct size is too large (see mus-max-malloc)");

  if (Xen_is_number(filler))
    return(g_vct_fill(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))), filler));

  return(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))));
}


static Xen g_vct_length(Xen obj)
{
  #define H_vct_length "(" S_vct_length " v): length of " S_vct " v"
  vct *v;
  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_length, A_VCT);
  v = Xen_to_vct(obj);
  return(C_llong_to_Xen_llong(mus_vct_length(v)));
}


static Xen g_vct_copy(Xen obj)
{
  #define H_vct_copy "(" S_vct_copy " v): returns a copy of " S_vct " v"
  vct *v;
  mus_float_t *copied_data = NULL;
  mus_long_t len;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_copy, A_VCT);

  v = Xen_to_vct(obj);
  len = mus_vct_length(v);
  if (len > 0)
    {
      copied_data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      mus_copy_floats(copied_data, mus_vct_data(v), len);
    }
  return(xen_make_vct(len, copied_data));
}

#else /* HAVE_SCHEME */
vct *mus_vct_make(mus_long_t len)
{
  s7_int di[1];
  di[0] = len;
  return(s7_make_float_vector(s7, len, 1, di));
}

Xen xen_make_vct(mus_long_t len, mus_float_t *data)
{
  return(s7_make_float_vector_wrapper(s7, len, (s7_double *)data, 1, NULL, true));     /* freed by s7 */
}

Xen xen_make_vct_wrapper(mus_long_t len, mus_float_t *data)
{
  s7_int di[1];
  di[0] = len;
  return(s7_make_float_vector_wrapper(s7, len, (s7_double *)data, 1, di, false));     /* not freed by s7 */
}

vct *mus_vct_wrap(mus_long_t len, mus_float_t *data)
{
  return(xen_make_vct_wrapper(len, data));
}

static Xen g_vct_copy(Xen obj)
{
  #define H_vct_copy "(" S_vct_copy " v): returns a copy of " S_vct " v"
  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_copy, A_VCT);
  return(s7_vector_copy(s7, obj));
}
#endif


static Xen g_vct_move(Xen obj, Xen newi, Xen oldi, Xen backwards)
{
  #define H_vct_moveB "(" S_vct_move " obj new old :optional backwards): moves " S_vct " obj data from old to new: v[new++] = v[old++], or \
v[new--] = v[old--] if backwards is " PROC_FALSE "."
  vct *v;
  mus_long_t i, j, ni, nj;
  mus_float_t *d;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_move, A_VCT);
  Xen_check_type(Xen_is_llong(newi), newi, 2, S_vct_move, "an integer");
  Xen_check_type(Xen_is_llong(oldi), oldi, 3, S_vct_move, "an integer");
  Xen_check_type(Xen_is_boolean_or_unbound(backwards), backwards, 4, S_vct_move, "a boolean");

  v = Xen_to_vct(obj);
  d = mus_vct_data(v);
  ni = Xen_llong_to_C_llong(newi);
  nj = Xen_llong_to_C_llong(oldi);

  if ((Xen_is_boolean(backwards)) && 
      (!Xen_is_false(backwards)))
    {
      if (ni >= mus_vct_length(v)) 
	Xen_out_of_range_error(S_vct_move, 2, newi, "new-index too high");
      if (nj >= mus_vct_length(v))
	Xen_out_of_range_error(S_vct_move, 3, oldi, "old-index too high");
      for (i = ni, j = nj; (j >= 0) && (i >= 0); i--, j--) 
	d[i] = d[j];
    }
  else
    {
      mus_long_t len;
      if (ni < 0)
	Xen_out_of_range_error(S_vct_move, 2, newi, "new-index < 0?");
      if (nj < 0)
	Xen_out_of_range_error(S_vct_move, 3, oldi, "old-index < 0?");
      len = mus_vct_length(v);
      for (i = ni, j = nj; (j < len) && (i < len); i++, j++) 
	d[i] = d[j];
    }
  return(obj);
}

#if (!HAVE_SCHEME)
static Xen g_vct_ref(Xen obj, Xen pos)
{
  #define H_vct_ref "(" S_vct_ref " v n): element n of " S_vct " v, v[n]"
  vct *v;
  mus_long_t loc;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_ref, A_VCT);
  Xen_check_type(Xen_is_llong(pos), pos, 2, S_vct_ref, "an integer");

  v = Xen_to_vct(obj);
  loc = Xen_llong_to_C_llong(pos);

  if (loc < 0)
    Xen_out_of_range_error(S_vct_ref, 2, pos, "index < 0?");
  if (loc >= mus_vct_length(v))
    Xen_out_of_range_error(S_vct_ref, 2, pos, "index too high?");

  return(C_double_to_Xen_real(mus_vct_data(v)[loc]));
}


static Xen g_vct_set(Xen obj, Xen pos, Xen val)
{
  #define H_vct_setB "(" S_vct_set " v n val): sets element of " S_vct " v to val, v[n] = val"
  vct *v;
  mus_long_t loc;
  double x;
  mus_float_t *d;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_set, A_VCT);
  Xen_check_type(Xen_is_llong(pos), pos, 2, S_vct_set, "an integer");
  Xen_check_type(Xen_is_number(val), val, 3, S_vct_set, "a real number");

  x = Xen_real_to_C_double(val);
  v = Xen_to_vct(obj);
  loc = Xen_llong_to_C_llong(pos);

  if (loc < 0)
    Xen_out_of_range_error(S_vct_set, 2, pos, "index < 0?"); 
  if (loc >= mus_vct_length(v))
    Xen_out_of_range_error(S_vct_set, 2, pos, "index >= vct-length?");
  
  d = mus_vct_data(v);
  d[loc] = x;
  return(val);
}
#endif


static Xen g_vct_multiply(Xen obj1, Xen obj2)
{
  #define H_vct_multiplyB "(" S_vct_multiply " v1 v2): element-wise multiply of " S_vct "s v1 and v2: v1[i] *= v2[i], returns v1"
  mus_long_t i, lim, lim1;
  vct *v1, *v2;
  mus_float_t *d1, *d2;

  Xen_check_type(mus_is_vct(obj1), obj1, 1, S_vct_multiply, A_VCT);
  Xen_check_type(mus_is_vct(obj2), obj2, 2, S_vct_multiply, A_VCT);

  v1 = Xen_to_vct(obj1);
  v2 = Xen_to_vct(obj2);
  d1 = mus_vct_data(v1);
  d2 = mus_vct_data(v2);
  lim = mus_vct_length(v1);
  lim1 = mus_vct_length(v2);
  if (lim > lim1) lim = lim1;
  for (i = 0; i < lim; i++) d1[i] *= d2[i];
  return(obj1);
}

#if WITH_VECTORIZE
static void vct_add(mus_float_t *d1, mus_float_t *d2, mus_long_t lim) __attribute__((optimize("tree-vectorize")));
static void vct_add(mus_float_t *d1, mus_float_t *d2, mus_long_t lim)
{
  mus_add_floats(d1, d2, lim);
}
#else
static void vct_add(mus_float_t *d1, mus_float_t *d2, mus_long_t lim)
{
  mus_long_t i, lim8;
  lim8 = lim - 16;
  i = 0;
  while (i <= lim8)
    {
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
      d1[i] += d2[i]; i++;
    }
  for (; i < lim; i++) 
    d1[i] += d2[i];
}
#endif

static Xen g_vct_add(Xen obj1, Xen obj2, Xen offs)
{
  #define H_vct_addB "(" S_vct_add " v1 v2 :optional (offset 0)): element-wise add of " S_vct "s v1 and v2: v1[i + offset] += v2[i], returns v1"
  mus_long_t lim, len1;
  vct *v1, *v2;
  mus_float_t *d1, *d2;

  Xen_check_type(mus_is_vct(obj1), obj1, 1, S_vct_add, A_VCT);
  Xen_check_type(mus_is_vct(obj2), obj2, 2, S_vct_add, A_VCT);
  Xen_check_type(Xen_is_llong_or_unbound(offs), offs, 3, S_vct_add, "an integer");

  v1 = Xen_to_vct(obj1);
  v2 = Xen_to_vct(obj2);
  d1 = mus_vct_data(v1);
  d2 = mus_vct_data(v2);
  len1 = mus_vct_length(v1);
  lim = mus_vct_length(v2);
  if (lim > len1) lim = len1;
  if (lim == 0) return(obj1);

  if (Xen_is_llong(offs))
    {
      mus_long_t j;
      j = Xen_llong_to_C_llong(offs);
      if (j < 0) 
	Xen_out_of_range_error(S_vct_add, 3, offs, "offset < 0?");
      if (j > len1)
	Xen_out_of_range_error(S_vct_add, 3, offs, "offset > length of vct?");

      if ((j + lim) > len1)
	lim = (len1 - j);

      vct_add((mus_float_t *)(d1 + j), d2, lim);
    }
  else vct_add(d1, d2, lim);
  return(obj1);
}


static Xen g_vct_subtract(Xen obj1, Xen obj2)
{
  #define H_vct_subtractB "(" S_vct_subtract " v1 v2): element-wise subtract of " S_vct "s v1 and v2: v1[i] -= v2[i], returns v1"
  mus_long_t i, lim, lim1, lim4;
  vct *v1, *v2;
  mus_float_t *d1, *d2;

  Xen_check_type(mus_is_vct(obj1), obj1, 1, S_vct_subtract, A_VCT);
  Xen_check_type(mus_is_vct(obj2), obj2, 2, S_vct_subtract, A_VCT);

  v1 = Xen_to_vct(obj1);
  v2 = Xen_to_vct(obj2);
  d1 = mus_vct_data(v1);
  d2 = mus_vct_data(v2);
  lim = mus_vct_length(v1);
  lim1 = mus_vct_length(v2);
  if (lim > lim1) lim = lim1;
  lim4 = lim - 4;

  i = 0;
  while (i <= lim4)
    {
      d1[i] -= d2[i]; i++;
      d1[i] -= d2[i]; i++;
      d1[i] -= d2[i]; i++;
      d1[i] -= d2[i]; i++;
    }
  for (; i < lim; i++) 
    d1[i] -= d2[i];

  return(obj1);
}


static Xen g_vct_abs(Xen obj)
{
  #define H_vct_absB "(" S_vct_abs " v): v[i] = abs(v[i]), return v."
  mus_long_t lim;
  vct *v;
  mus_float_t *d;

  Xen_check_type(mus_is_vct(obj), obj, 0, S_vct_abs, A_VCT);

  v = Xen_to_vct(obj);
  d = mus_vct_data(v);
  lim = mus_vct_length(v);
  mus_abs_floats(d, lim);
  return(obj);
}


static Xen g_vct_equal(Xen uv1, Xen uv2, Xen udiff)
{
  #define H_vct_equal "(" S_vct_equal " v1 v2 diff): is element-wise relative-difference of v1 and v2 ever greater than diff?"
  mus_long_t i, lim;
  vct *v1, *v2;
  mus_float_t *d1, *d2;
  mus_float_t diff, max_diff = 0.0;

  Xen_check_type(mus_is_vct(uv1), uv1, 1, S_vct_equal, A_VCT);
  Xen_check_type(mus_is_vct(uv2), uv2, 2, S_vct_equal, A_VCT);
  Xen_check_type(Xen_is_number(udiff), udiff, 3, S_vct_equal, "a number");

  v1 = Xen_to_vct(uv1);
  d1 = mus_vct_data(v1);
  v2 = Xen_to_vct(uv2);
  d2 = mus_vct_data(v2);
  diff = Xen_real_to_C_double(udiff);

  lim = mus_vct_length(v1);
  if (mus_vct_length(v2) < lim) lim = mus_vct_length(v2);

  for (i = 0; i < lim; i++)
    {
      mus_float_t x1, x2, z;
      x1 = fabs(d1[i]);
      x2 = fabs(d2[i]);
      z = fabs(d1[i] - d2[i]);
      if (x1 > x2)
	z /= x1;
      else 
	{
	  if (x2 > 0.0)
	    z /= x2;
	}
      if (z > diff)
	return(Xen_false);
      if (z > max_diff) 
	max_diff = z;
    }

  return(C_double_to_Xen_real(max_diff));
}

#if WITH_VECTORIZE
static void vct_scale(mus_float_t *d, mus_float_t scl, mus_long_t len) __attribute__((optimize("tree-vectorize")));
#endif

static void vct_scale(mus_float_t *d, mus_float_t scl, mus_long_t len)
{
  if (scl == 0.0)
    mus_clear_floats(d, len);
  else
    {
      if (scl != 1.0)
	{
	  mus_long_t i, lim4;
	  lim4 = len - 4;
	  i = 0;
	  while (i <= lim4)
	    {
	      d[i++] *= scl;
	      d[i++] *= scl;
	      d[i++] *= scl;
	      d[i++] *= scl;
	    }
	  for (; i < len; i++) 
	    d[i] *= scl;
	}
    }
}

static Xen g_vct_scale(Xen obj1, Xen obj2)
{
  #define H_vct_scaleB "(" S_vct_scale " v val): scale each element of v by val: v[i] *= val, returns v"

  /* Xen_check_type(s7_is_float_vector(obj1), obj1, 1, "float-vector-scale!", "a float-vector");
   * return(s7_float_vector_scale(s7, obj1, obj2));
   */
  vct *v1;

  Xen_check_type(mus_is_vct(obj1), obj1, 1, S_vct_scale, A_VCT);
  Xen_check_type(Xen_is_number(obj2), obj2, 2, S_vct_scale, "a number");

  v1 = Xen_to_vct(obj1);
  if (mus_vct_length(v1) == 0) return(obj1);
  vct_scale(mus_vct_data(v1), Xen_real_to_C_double(obj2), mus_vct_length(v1));
  return(obj1);
}


static Xen g_vct_offset(Xen obj1, Xen obj2)
{
  #define H_vct_offsetB "(" S_vct_offset " v val): add val to each element of v: v[i] += val, returns v"
  vct *v1;
  mus_float_t scl;
  mus_float_t *d;
  mus_long_t len;

  Xen_check_type(mus_is_vct(obj1), obj1, 1, S_vct_offset, A_VCT);
  Xen_check_type(Xen_is_number(obj2), obj2, 2, S_vct_offset, "a number");

  v1 = Xen_to_vct(obj1);
  if (mus_vct_length(v1) == 0) return(obj1);
  d = mus_vct_data(v1);
  len = mus_vct_length(v1);

  scl = Xen_real_to_C_double(obj2);
  if (scl != 0.0)
    {
      mus_long_t i;
      for (i = 0; i < len; i++) 
	d[i] += scl;
    }
  return(obj1);
}

#if HAVE_SCHEME
#define S_vct_spatter "float-vector-spatter"
static Xen g_vct_spatter(Xen fv, XEN iv, XEN end, XEN val)
{
  #define H_vct_spatter "(" S_vct_spatter " fv iv end val) places val in fv at locations determined by iv"
  s7_double *fv_vals;
  s7_int *iv_vals;
  s7_double x;
  int i, len;

  if (!s7_is_float_vector(fv)) s7_wrong_type_arg_error(s7, S_vct_spatter, 1, fv, "a float-vector");
  if (!s7_is_int_vector(iv)) s7_wrong_type_arg_error(s7, S_vct_spatter, 2, iv, "an int-vector");
  if (!s7_is_integer(end)) s7_wrong_type_arg_error(s7, S_vct_spatter, 3, end, "an integer");
  if (!s7_is_real(val)) s7_wrong_type_arg_error(s7, S_vct_spatter, 4, val, "a real");

  fv_vals = s7_float_vector_elements(fv);
  iv_vals = s7_int_vector_elements(iv);
  len = s7_integer(end);
  x = s7_real(val);
  for (i = 0; i < len; i++)
    fv_vals[iv_vals[i]] = x;

  return(val);
}

#define S_vct_interpolate "float-vector-interpolate"
static Xen g_vct_interpolate(Xen fv, Xen start_index, Xen end_index, Xen start_x, XEN incr, XEN val1, XEN val2)
{
  #define H_vct_interpolate "(" S_vct_interpolate " fv index0 index1 x0 dx x1 x2) sets the values of fv between\
index0 and index1 interpolating between x2 and x1 by incrementing x0 by dx"
  s7_double x0, dx, x1, x2;
  int i, beg, lim;
  s7_double *fv_vals;
  fv_vals = s7_float_vector_elements(fv);

  if (!s7_is_float_vector(fv)) s7_wrong_type_arg_error(s7, S_vct_interpolate, 1, fv, "a float-vector");
  if (!s7_is_integer(start_index)) s7_wrong_type_arg_error(s7, S_vct_spatter, 2, start_index, "an integer");
  if (!s7_is_integer(end_index)) s7_wrong_type_arg_error(s7, S_vct_spatter, 3, end_index, "an integer");
  if (!s7_is_real(start_x)) s7_wrong_type_arg_error(s7, S_vct_spatter, 4, start_x, "a real");
  if (!s7_is_real(incr)) s7_wrong_type_arg_error(s7, S_vct_spatter, 5, incr, "a real");
  if (!s7_is_real(val1)) s7_wrong_type_arg_error(s7, S_vct_spatter, 6, val1, "a real");
  if (!s7_is_real(val2)) s7_wrong_type_arg_error(s7, S_vct_spatter, 7, val2, "a real");

  beg = s7_integer(start_index);
  lim = s7_integer(end_index);
  x0 = s7_real(start_x);
  dx = s7_real(incr);
  x1 = s7_real(val1);
  x2 = s7_real(val2);
  for (i = beg; i < lim; i++, x0 += dx)
    fv_vals[i] = (x0 * x1) + ((1.0 - x0) * x2);
  return(val1);
}
#endif


#if (!HAVE_SCHEME)
static Xen g_vct_fill(Xen obj1, Xen obj2)
{
  #define H_vct_fillB "(" S_vct_fill " v val): set each element of v to val: v[i] = val, returns v"
  mus_long_t i, len; /* unsigned int is much slower */
  vct *v1;
  mus_float_t scl;
  mus_float_t *d;

  Xen_check_type(mus_is_vct(obj1), obj1, 1, S_vct_fill, A_VCT);
  Xen_check_type(Xen_is_number(obj2), obj2, 2, S_vct_fill, "a number");

  v1 = Xen_to_vct(obj1);
  len = mus_vct_length(v1);
  if (len == 0) return(obj1);
  d = mus_vct_data(v1);

  scl = Xen_real_to_C_double(obj2);
  if (scl == 0.0)
    mus_clear_floats(d, len);
  else 
    {
      mus_long_t lim8;
      lim8 = len - 8;
      i = 0;
      while (i <= lim8)
	{
	  d[i++] = scl;
	  d[i++] = scl;
	  d[i++] = scl;
	  d[i++] = scl;
	  d[i++] = scl;
	  d[i++] = scl;
	  d[i++] = scl;
	  d[i++] = scl;
	}
      for (; i < len; i++) 
	d[i] = scl;
    }
  return(obj1);
}
#endif

double mus_vct_peak(vct *v)
{
  mus_float_t val = 0.0;
  mus_float_t *d;
  mus_long_t i, len;

  len = mus_vct_length(v);
  if (len == 0) return(0.0);
  d = mus_vct_data(v);

  for (i = 0; i < len; i++)
    val = (fabs(d[i]) > val) ? fabs(d[i]) : val;
  return(val);
}


Xen g_vct_peak(Xen obj)
{
  #define H_vct_peak "(" S_vct_peak " v): max of abs of elements of v"
  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_peak, A_VCT);
  return(C_double_to_Xen_real(mus_vct_peak(Xen_to_vct(obj))));
}


#if HAVE_SCHEME
#define S_vct_peak_and_location "float-vector-peak-and-location"
#else
#define S_vct_peak_and_location "vct-peak-and-location"
#endif


static Xen g_vct_peak_and_location(Xen obj)
{
  #define H_vct_peak_and_location "(" S_vct_peak_and_location " v): max of abs of elements of v and its position in v"
  mus_float_t val = 0.0;
  mus_long_t i, loc = 0, len;
  vct *v;
  mus_float_t *d;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_vct_peak_and_location, "a " S_vct);
  v = Xen_to_vct(obj);
  d = mus_vct_data(v);
  len = mus_vct_length(v);

  for (i = 0; i < len; i++)
    {
      mus_float_t absv;
      absv = fabs(d[i]);
      if (absv > val) 
	{
	  val = absv;
	  loc = i;
	}
    }
  return(Xen_list_2(C_double_to_Xen_real(val), C_int_to_Xen_integer(loc)));
}


static Xen g_vct_subseq(Xen vobj, Xen start, Xen end, Xen newv)
{
  #define H_vct_subseq "(" S_vct_subseq " v start :optional end vnew): v[start..end], placed in vnew if given or new " S_vct
  vct *vold, *vnew;
  mus_float_t *dnew, *dold;
  Xen res;
  mus_long_t i, old_len, new_len, j, istart;

  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_subseq, A_VCT);
  Xen_check_type(Xen_is_llong(start), start, 2, S_vct_subseq, "an integer");
  Xen_check_type(Xen_is_llong_or_unbound(end), end, 3, S_vct_subseq, "an integer");

  istart = Xen_llong_to_C_llong(start);
  if (istart < 0)
    Xen_out_of_range_error(S_vct_subseq, 2, start, "start < 0?");

  vold = Xen_to_vct(vobj);
  old_len = mus_vct_length(vold);

  if (Xen_is_llong(end))
    {
      mus_long_t iend;
      iend = Xen_llong_to_C_llong(end);
      if (iend < istart)
	Xen_out_of_range_error(S_vct_subseq, 3, end, "end < start?");
      if (iend > old_len)
	Xen_out_of_range_error(S_vct_subseq, 3, end, "end > vct length?");
      new_len = iend - istart + 1;
    }
  else new_len = old_len - istart;

  if (new_len <= 0) 
    return(Xen_false);

  if (mus_is_vct(newv))
    res = newv;
  else res = xen_make_vct(new_len, (mus_float_t *)calloc(new_len, sizeof(mus_float_t)));
  vnew = Xen_to_vct(res);

  if (new_len > mus_vct_length(vnew)) 
    new_len = mus_vct_length(vnew);
  dnew = mus_vct_data(vnew);
  dold = mus_vct_data(vold);

  for (i = istart, j = 0; (j < new_len) && (i < old_len); i++, j++)
    dnew[j] = dold[i];

  return(res);
}


Xen xen_list_to_vct(Xen lst)
{
  #define H_list_to_vct "(" S_list_to_vct " lst): returns a new " S_vct " filled with elements of list lst"
  mus_long_t len = 0, i;
  vct *v;
  mus_float_t *d;
  Xen scv, lst1;

  Xen_check_type(Xen_is_list(lst), lst, 1, S_list_to_vct, "a list");
  len = Xen_list_length(lst);
  if (len > 0)
    scv = xen_make_vct(len, (mus_float_t *)calloc(len, sizeof(mus_float_t)));
  else scv = xen_make_vct(0, NULL);

  v = Xen_to_vct(scv);
  d = mus_vct_data(v);
  for (i = 0, lst1 = Xen_copy_arg(lst); i < len; i++, lst1 = Xen_cdr(lst1)) 
    {
      if (Xen_is_number(Xen_car(lst1)))
	d[i] = (mus_float_t)Xen_real_to_C_double(Xen_car(lst1));
      else Xen_wrong_type_arg_error(S_list_to_vct, i, Xen_car(lst1), "a number");
    }

  return(scv);
}


Xen mus_array_to_list(mus_float_t *arr, mus_long_t i, mus_long_t len)
{
  if (i < (len - 1))
    return(Xen_cons(C_double_to_Xen_real(arr[i]), 
		    mus_array_to_list(arr, i + 1, len)));
  else return(Xen_cons(C_double_to_Xen_real(arr[i]), 
		       Xen_empty_list));
}


#if (!HAVE_SCHEME)
static Xen g_vct(Xen args) 
{
  #define H_vct "(" S_vct " args...): returns a new " S_vct " with args as contents; same as " S_list_to_vct ": (" S_vct " 1 2 3)"
  return(xen_list_to_vct(args));
}


static Xen g_vct_to_list(Xen vobj)
{
  #define H_vct_to_list "(" S_vct_to_list " v): returns a new list with elements of " S_vct " v"
  vct *v;
  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_to_list, A_VCT);

  v = Xen_to_vct(vobj);
  if (mus_vct_length(v) == 0)
    return(Xen_empty_list);

  return(mus_array_to_list(mus_vct_data(v), 0, mus_vct_length(v)));
}


static Xen g_vector_to_vct(Xen vect)
{
  #define H_vector_to_vct "(" S_vector_to_vct " vect): returns a new " S_vct " with the elements of vector vect"
  mus_long_t len, i;
  vct *v;
  mus_float_t *d;
  Xen scv;

  Xen_check_type(Xen_is_vector(vect), vect, 1, S_vector_to_vct, "a vector");

  len = (mus_long_t)Xen_vector_length(vect);
  if (len > 0) 
    scv = xen_make_vct(len, (mus_float_t *)calloc(len, sizeof(mus_float_t)));
  else scv = xen_make_vct(0, NULL);

  v = Xen_to_vct(scv);
  d = mus_vct_data(v);
  for (i = 0; i < len; i++) 
    d[i] = (mus_float_t)Xen_real_to_C_double(Xen_vector_ref(vect, i));

  return(scv);
}


static Xen g_vct_to_vector(Xen vobj)
{
  #define H_vct_to_vector "(" S_vct_to_vector " v): returns a new vector with the elements of " S_vct
  vct *v;
  mus_float_t *d;
  mus_long_t i, len;
  Xen new_vect;

  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_to_vector, A_VCT);
  v = Xen_to_vct(vobj);
  len = mus_vct_length(v);
  new_vect = Xen_make_vector(len, C_double_to_Xen_real(0.0));

#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_disable(); 
  /* uh oh -- gc is triggered by C_double_to_Xen_real causing segfault, even if we
   *   protect (via Xen_protect_from_gc) new_vect -- I guess the double currently
   *   being created is causing the trouble?
   */
#endif

  d = mus_vct_data(v);
  for (i = 0; i < len; i++) 
    Xen_vector_set(new_vect, i, C_double_to_Xen_real(d[i]));

#if HAVE_RUBY && HAVE_RB_GC_DISABLE
  rb_gc_enable();
#endif

  return(new_vect);
}


static Xen g_vct_reverse(Xen vobj, Xen size)
{
  #define H_vct_reverse "(" S_vct_reverse " v len): in-place reversal of " S_vct " contents"
  vct *v;
  mus_float_t *d;
  mus_long_t i, j, len = -1;

  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_reverse, A_VCT);
  Xen_check_type(Xen_is_llong_or_unbound(size), size, 2, S_vct_reverse, "an integer");

  v = Xen_to_vct(vobj);
  if (Xen_is_llong(size))
    len = Xen_llong_to_C_llong(size);
  if ((len <= 0) || (len > mus_vct_length(v)))
    len = mus_vct_length(v);
  if (len == 1) return(vobj);
  d = mus_vct_data(v);

  for (i = 0, j = len - 1; i < j; i++, j--)
    {
      mus_float_t temp;
      temp = d[i];
      d[i] = d[j];
      d[j] = temp;
    }
  return(vobj);
}
#endif


#if HAVE_SCHEME
#define S_vct_max "float-vector-max"
#define S_vct_min "float-vector-min"
#else
#define S_vct_max "vct-max"
#define S_vct_min "vct-min"
#endif

static mus_float_t vct_max(mus_float_t *d, mus_long_t len)
{
  mus_long_t i;
  mus_float_t mx;
  mx = d[0];
  for (i = 1; i < len; i++)
    if (d[i] > mx)
      mx = d[i];
  return(mx);
}

#if HAVE_SCHEME
static s7_double float_vector_max_d_p(s7_pointer v)
{
  return(vct_max(s7_float_vector_elements(v), s7_vector_length(v)));
}
#endif

static Xen g_vct_max(Xen vobj)
{
  #define H_vct_max "(" S_vct_max " v): returns the maximum element of " S_vct
  vct *v;
  mus_long_t len;

  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_max, A_VCT);
  v = Xen_to_vct(vobj);

  len = mus_vct_length(v);
  if (len > 0)
    return(C_double_to_Xen_real(vct_max(mus_vct_data(v), len)));
  return(C_double_to_Xen_real(0.0));
}


static mus_float_t vct_min(mus_float_t *d, mus_long_t len)
{
  mus_long_t i;
  mus_float_t mx;
  mx = d[0];
  for (i = 1; i < len; i++)
    if (d[i] < mx)
      mx = d[i];
  return(mx);
}

#if HAVE_SCHEME
static s7_double float_vector_min_d_p(s7_pointer v)
{
  return(vct_min(s7_float_vector_elements(v), s7_vector_length(v)));
}
#endif

static Xen g_vct_min(Xen vobj)
{
  #define H_vct_min "(" S_vct_min " v): returns the minimum element of " S_vct
  vct *v;
  mus_long_t len;

  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_min, A_VCT);
  v = Xen_to_vct(vobj);

  len = mus_vct_length(v);
  if (len > 0)
    return(C_double_to_Xen_real(vct_min(mus_vct_data(v), len)));
  return(C_double_to_Xen_real(0.0));
}


static Xen g_vct_times(Xen obj1, Xen obj2)
{
  #define H_vct_times "(" S_vct_times " obj1 obj2): either " S_vct_multiply " or " S_vct_scale ", depending on the types of its arguments"
  if (mus_is_vct(obj1))
    {
      if (mus_is_vct(obj2))
	return(g_vct_multiply(obj1, obj2));
      return(g_vct_scale(obj1, obj2));
    }
  return(g_vct_scale(obj2, obj1));
}


static Xen g_vct_plus(Xen obj1, Xen obj2)
{
  #define H_vct_plus "(" S_vct_plus " obj1 obj2): either " S_vct_add " or " S_vct_offset ", depending on the types of its arguments"
  if (mus_is_vct(obj1))
    {
      if (mus_is_vct(obj2))
	return(g_vct_add(obj1, obj2, Xen_undefined));
      return(g_vct_offset(obj1, obj2));
    }
  return(g_vct_offset(obj2, obj1));
}

#if HAVE_RUBY
static Xen g_vct_each(Xen obj)
{
  mus_long_t i;
  vct *v;
  mus_float_t *d;

  v = Xen_to_vct(obj);
  d = mus_vct_data(v);

  for (i = 0; i < mus_vct_length(v); i++)
    rb_yield(C_double_to_Xen_real(d[i]));
  return(obj);
}


static Xen g_vct_compare(Xen vr1, Xen vr2)
{
  if ((mus_is_vct(vr1)) && (mus_is_vct(vr2)))
    {
      mus_long_t i, len;
      vct *v1, *v2;
      mus_float_t *d1, *d2;

      v1 = Xen_to_vct(vr1);
      v2 = Xen_to_vct(vr2);
      d1 = mus_vct_data(v1);
      d2 = mus_vct_data(v2);

      len = mus_vct_length(v1);
      if (len > mus_vct_length(v2)) len = mus_vct_length(v2);
      for (i = 0; i < len; i++) 
	if (d1[i] < d2[i])
	  return(C_int_to_Xen_integer(-1));
	else
	  if (d1[i] > d2[i])
	    return(C_int_to_Xen_integer(1));
      len = mus_vct_length(v1) - mus_vct_length(v2);
      if (len == 0) return(C_int_to_Xen_integer(0));
      if (len > 0) return(C_int_to_Xen_integer(1));
    }
  return(C_int_to_Xen_integer(-1));
}


static Xen g_rb_make_vct(int argc, Xen *argv, Xen self)
{
  mus_long_t size;
  Xen len, filler;
  rb_scan_args(argc, argv, "11", &len, &filler);
  Xen_check_type(Xen_is_llong(len), len, 1, "Vct.new", "an integer");
  size = Xen_llong_to_C_llong(len);
  if (size <= 0) 
    Xen_out_of_range_error("Vct.new", 1, len, "len <= 0?");
  if (Xen_is_number(filler))
    return(g_vct_fill(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))), filler));
  if (rb_block_given_p()) {
    mus_long_t i;
    mus_float_t *buffer = (mus_float_t *)calloc(size, sizeof(mus_float_t));
    for (i = 0; i < size; i++) {
      buffer[i] = Xen_real_to_C_double(rb_yield(C_int_to_Xen_integer(i)));
    }
    return xen_make_vct(size, buffer);
  }
  return(xen_make_vct(size, (mus_float_t *)calloc(size, sizeof(mus_float_t))));
}


static Xen g_vct_map(Xen obj)
{
  if (rb_block_given_p()) 
    {
      mus_long_t i;
      vct *v;
      mus_float_t *d;
      
      v = Xen_to_vct(obj);
      d = mus_vct_data(v);
      mus_float_t *buffer = (mus_float_t *)calloc(mus_vct_length(v), sizeof(mus_float_t));

      for (i = 0; i < mus_vct_length(v); i++)
	buffer[i] = Xen_real_to_C_double(rb_yield(C_double_to_Xen_real(d[i])));
      return xen_make_vct(mus_vct_length(v), buffer);
    }
  return obj;
}


static Xen g_vct_map_store(Xen obj)
{
  if (rb_block_given_p()) 
    {
      mus_long_t i;
      vct *v;
      mus_float_t *d;

      v = Xen_to_vct(obj);
      d = mus_vct_data(v);

      for (i = 0; i < mus_vct_length(v); i++)
	d[i] = Xen_real_to_C_double(rb_yield(C_double_to_Xen_real(d[i])));
    }
  return obj;
}


/* v1.add!(v2[,offset=0]) destructive */

static Xen rb_vct_add(int argc, Xen *argv, Xen obj1)
{
  Xen obj2, offs;
  rb_scan_args(argc, argv, "11", &obj2, &offs);
  return g_vct_add(obj1, obj2, (argc == 2) ? offs : Xen_undefined);
}


/* v1.add(v2[,offset=0]) returns new vct */

static Xen rb_vct_add_cp(int argc, Xen *argv, Xen obj1)
{
  Xen obj2, offs;
  rb_scan_args(argc, argv, "11", &obj2, &offs);
  return g_vct_add(g_vct_copy(obj1), obj2, (argc == 2) ? offs : Xen_undefined);
}


/* v1.subtract(v2) returns new vct */

static Xen rb_vct_subtract_cp(Xen obj1, Xen obj2)
{
  return g_vct_subtract(g_vct_copy(obj1), obj2);
}


static Xen rb_vct_offset_cp(Xen obj, Xen scl)
{
  return g_vct_offset(g_vct_copy(obj), scl);
}


static Xen rb_vct_multiply_cp(Xen obj1, Xen obj2)
{
  return g_vct_multiply(g_vct_copy(obj1), obj2);
}


static Xen rb_vct_scale_cp(Xen obj, Xen scl)
{
  return g_vct_scale(g_vct_copy(obj), scl);
}


/* destructive */

static Xen rb_vct_move(int argc, Xen *argv, Xen obj)
{
  Xen vnew, old, backward;
  rb_scan_args(argc, argv, "21", &vnew, &old, &backward);
  return g_vct_move(obj, vnew, old, (argc == 3) ? backward : Xen_undefined);
}


/* returns new vct */

static Xen rb_vct_move_cp(int argc, Xen *argv, Xen obj)
{
  Xen vnew, old, backward;
  rb_scan_args(argc, argv, "21", &vnew, &old, &backward);
  return g_vct_move(g_vct_copy(obj), vnew, old, (argc == 3) ? backward : Xen_undefined);
}


static Xen rb_vct_subseq(int argc, Xen *argv, Xen obj)
{
  Xen start, end, vnew;
  rb_scan_args(argc, argv, "12", &start, &end, &vnew);
    return g_vct_subseq(obj, start, (argc > 1) ? end :Xen_undefined, (argc > 2) ? vnew : Xen_undefined);
}


/* destructive */

static Xen rb_vct_reverse(int argc, Xen *argv, Xen obj)
{
  Xen len;
  rb_scan_args(argc, argv, "01", &len);
  return g_vct_reverse(obj, (argc > 0) ? len : Xen_undefined);
}


/* returns new vct */

static Xen rb_vct_reverse_cp(int argc, Xen *argv, Xen obj)
{
  Xen len;
  rb_scan_args(argc, argv, "01", &len);
  return g_vct_reverse(g_vct_copy(obj), (argc > 0) ? len : Xen_undefined);
}


static Xen rb_vct_first(Xen obj)
{
  return g_vct_ref(obj, C_int_to_Xen_integer(0));
}


static Xen rb_set_vct_first(Xen obj, Xen val)
{
  return g_vct_set(obj, C_int_to_Xen_integer(0), val);
}


static Xen rb_vct_last(Xen obj)
{
  return g_vct_ref(obj, C_int_to_Xen_integer(mus_vct_length(Xen_to_vct(obj)) - 1));
}


static Xen rb_set_vct_last(Xen obj, Xen val)
{
  return g_vct_set(obj, C_int_to_Xen_integer(mus_vct_length(Xen_to_vct(obj)) - 1), val);
}
#endif


#if HAVE_FORTH
static void ficl_values_to_vct(ficlVm *vm)
{
#define h_values_to_vct "( len-floats len -- vct )  \
Returns a new vct of length LEN with len items found on stack.\n\
0.5 0.3 0.1  3  >vct  .g => #<vct[len=3]: 0.500 0.300 0.100>"
  long size;
  FICL_STACK_CHECK(vm->dataStack, 1, 0);
  size = ficlStackPopInteger(vm->dataStack);
  if (size > 0)
    {
      mus_float_t *data = (mus_float_t *)calloc(size, sizeof(mus_float_t));
      if (data)
	{
	  long i;
	  FICL_STACK_CHECK(vm->dataStack, size, 1);
	  for (i = size - 1; i >= 0; i--)
	    data[i] = ficlStackPop2Float(vm->dataStack);
	  ficlStackPushUnsigned(vm->dataStack, xen_make_vct(size, data));
	}
      else fth_throw(FTH_SYSTEM_ERROR, "cannot create Vct");
    }
  else ficlStackPushUnsigned(vm->dataStack, fth_false());
}


static void ficl_begin_vct(ficlVm *vm)
{
#define h_begin_vct "( -- )  \
Creates a vct with contents between `vct(' and closing paren `)'.\n\
vct( 0.5 0.3 0.1 ) .g => #<vct[len=3]: 0.500 0.300 0.100>"
  fth_begin_values_to_obj(vm, (char *)">vct", FTH_FALSE);
}
#endif


#if (!HAVE_SCHEME)
  Xen_wrap_2_optional_args(g_make_vct_w, g_make_vct)
  Xen_wrap_2_args(g_vct_fill_w, g_vct_fill)
  Xen_wrap_any_args(g_vct_w, g_vct)
  Xen_wrap_1_arg(g_vct_length_w, g_vct_length)
  Xen_wrap_2_optional_args(g_vct_reverse_w, g_vct_reverse)
  Xen_wrap_1_arg(g_vct_to_list_w, g_vct_to_list)
  Xen_wrap_1_arg(g_list_to_vct_w, xen_list_to_vct)
  Xen_wrap_1_arg(g_vector_to_vct_w, g_vector_to_vct)
  Xen_wrap_1_arg(g_vct_to_vector_w, g_vct_to_vector)
  Xen_wrap_1_arg(g_is_vct_w, g_is_vct)
  Xen_wrap_2_args(g_vct_ref_w, g_vct_ref)
  Xen_wrap_3_args(g_vct_set_w, g_vct_set)
#endif
Xen_wrap_1_arg(g_vct_copy_w, g_vct_copy)
Xen_wrap_2_args(g_vct_multiply_w, g_vct_multiply)
Xen_wrap_2_args(g_vct_scale_w, g_vct_scale)
Xen_wrap_1_arg(g_vct_abs_w, g_vct_abs)
Xen_wrap_3_optional_args(g_vct_add_w, g_vct_add)
Xen_wrap_2_args(g_vct_subtract_w, g_vct_subtract)
Xen_wrap_2_args(g_vct_offset_w, g_vct_offset)
Xen_wrap_1_arg(g_vct_peak_w, g_vct_peak)
Xen_wrap_3_args(g_vct_equal_w, g_vct_equal)
Xen_wrap_1_arg(g_vct_peak_and_location_w, g_vct_peak_and_location)
Xen_wrap_4_optional_args(g_vct_move_w, g_vct_move)
Xen_wrap_4_optional_args(g_vct_subseq_w, g_vct_subseq)
Xen_wrap_1_arg(g_vct_to_readable_string_w, g_vct_to_readable_string)
Xen_wrap_2_args(g_vct_times_w, g_vct_times)
Xen_wrap_2_args(g_vct_plus_w, g_vct_plus)
Xen_wrap_1_arg(g_vct_max_w, g_vct_max)
Xen_wrap_1_arg(g_vct_min_w, g_vct_min)
#if HAVE_SCHEME
Xen_wrap_4_args(g_vct_spatter_w, g_vct_spatter)
Xen_wrap_7_args(g_vct_interpolate_w, g_vct_interpolate)
#endif

void mus_vct_init(void)
{
#if HAVE_SCHEME
  s7_pointer pl_ff, pl_rf, pl_fff, pl_fffi, pl_ffr, pl_pf, pl_bffr, pl_ftt, pl_ffiib, pl_ffiif, pl_sf, pl_rfvir, pl_rfiir;
#else
  vct_tag = Xen_make_object_type("Vct", sizeof(vct));

  /* for ruby and forth, I think we can define Frame, SoundData, and Mixer to be Vct's with
   *   some handlers for the channel arg.  Then nothing in the *.rb|fs file has to change
   *   except all the deprecated names like "region-frames" -> framples.
   *
   *   Not sure how to do this -- is it "alias" in Ruby?
   */
#endif

#if HAVE_FORTH
  fth_set_object_inspect(vct_tag,   print_vct);
  fth_set_object_dump(vct_tag,      g_vct_to_readable_string);
  fth_set_object_to_array(vct_tag,  g_vct_to_vector);
  fth_set_object_copy(vct_tag,      g_vct_copy);
  fth_set_object_value_ref(vct_tag, g_vct_ref);
  fth_set_object_value_set(vct_tag, g_vct_set);
  fth_set_object_equal(vct_tag,     equalp_vct);
  fth_set_object_length(vct_tag,    g_vct_length);
  fth_set_object_free(vct_tag,      free_vct);
  fth_set_object_apply(vct_tag, Xen_procedure_cast g_vct_ref, 1, 0, 0);
  FTH_PRIM(FTH_FICL_DICT(), (char *)">vct",   ficl_values_to_vct, h_values_to_vct);
  FTH_PRIM(FTH_FICL_DICT(), (char *)"vct(",   ficl_begin_vct,     h_begin_vct);
  Xen_eval_C_string("start-prefixes : vct( vct( ; end-prefixes"); 
#endif

#if HAVE_RUBY
  rb_include_module(vct_tag, rb_mComparable);
  rb_include_module(vct_tag, rb_mEnumerable);

  rb_define_method(vct_tag, "to_s",     Xen_procedure_cast print_vct, 0);
  rb_define_method(vct_tag, "eql?",     Xen_procedure_cast equalp_vct, 1);
  rb_define_method(vct_tag, "[]",       Xen_procedure_cast g_vct_ref, 1);
  rb_define_method(vct_tag, "[]=",      Xen_procedure_cast g_vct_set, 2);
  rb_define_method(vct_tag, "length",   Xen_procedure_cast g_vct_length, 0);
  rb_define_method(vct_tag, "each",     Xen_procedure_cast g_vct_each, 0);
  rb_define_method(vct_tag, "<=>",      Xen_procedure_cast g_vct_compare, 1);
  rb_define_singleton_method(vct_tag, "new", Xen_procedure_cast g_rb_make_vct, -1);
  rb_define_method(vct_tag, "map",      Xen_procedure_cast g_vct_map, 0);
  rb_define_method(vct_tag, "map!",     Xen_procedure_cast g_vct_map_store, 0);
  rb_define_method(vct_tag, "to_a",     Xen_procedure_cast g_vct_to_vector, 0);
  rb_define_method(rb_cArray, "to_vct", Xen_procedure_cast g_vector_to_vct, 0);

  rb_define_method(vct_tag, "to_str",    Xen_procedure_cast g_vct_to_readable_string, 0);
  rb_define_method(vct_tag, "dup",       Xen_procedure_cast g_vct_copy, 0);
  rb_define_method(vct_tag, "peak",      Xen_procedure_cast g_vct_peak, 0);
  rb_define_method(vct_tag, "add",       Xen_procedure_cast rb_vct_add_cp, -1);
  rb_define_method(vct_tag, "add!",      Xen_procedure_cast rb_vct_add, -1);
  rb_define_method(vct_tag, "subtract",  Xen_procedure_cast rb_vct_subtract_cp, 1);
  rb_define_method(vct_tag, "subtract!", Xen_procedure_cast g_vct_subtract, 1);
  rb_define_method(vct_tag, "offset",    Xen_procedure_cast rb_vct_offset_cp, 1);
  rb_define_method(vct_tag, "offset!",   Xen_procedure_cast g_vct_offset, 1);
  rb_define_method(vct_tag, "multiply",  Xen_procedure_cast rb_vct_multiply_cp, 1);
  rb_define_method(vct_tag, "multiply!", Xen_procedure_cast g_vct_multiply, 1);
  rb_define_method(vct_tag, "scale",     Xen_procedure_cast rb_vct_scale_cp, 1);
  rb_define_method(vct_tag, "scale!",    Xen_procedure_cast g_vct_scale, 1);
  rb_define_method(vct_tag, "fill",      Xen_procedure_cast g_vct_fill, 1);
  rb_define_method(vct_tag, "move",      Xen_procedure_cast rb_vct_move_cp, -1);
  rb_define_method(vct_tag, "move!",     Xen_procedure_cast rb_vct_move, -1);
  rb_define_method(vct_tag, "subseq",    Xen_procedure_cast rb_vct_subseq, -1);
  rb_define_method(vct_tag, "reverse",   Xen_procedure_cast rb_vct_reverse_cp, -1);
  rb_define_method(vct_tag, "reverse!",  Xen_procedure_cast rb_vct_reverse, -1);
  rb_define_method(vct_tag, "first",     Xen_procedure_cast rb_vct_first, 0);
  rb_define_method(vct_tag, "first=",    Xen_procedure_cast rb_set_vct_first, 1);
  rb_define_method(vct_tag, "last",      Xen_procedure_cast rb_vct_last, 0);
  rb_define_method(vct_tag, "last=",     Xen_procedure_cast rb_set_vct_last, 1);
#endif

#if HAVE_SCHEME
  {
    s7_pointer s, i, p, b, r, f, t;
    s = s7_make_symbol(s7, "string?");
    i = s7_make_symbol(s7, "integer?");
    p = s7_make_symbol(s7, "pair?");
    r = s7_make_symbol(s7, "real?");
    b = s7_make_symbol(s7, "boolean?");
    f = s7_make_symbol(s7, "float-vector?");
    t = s7_t(s7);
    pl_rf = s7_make_signature(s7, 2, r, f);
    pl_ff = s7_make_signature(s7, 2, f, f);
    pl_sf = s7_make_signature(s7, 2, s, f);
    pl_pf = s7_make_signature(s7, 2, p, f);
    pl_ftt = s7_make_signature(s7, 3, f, t, t);
    pl_fff = s7_make_signature(s7, 3, f, f, f);
    pl_ffr = s7_make_signature(s7, 3, f, f, r);
    pl_bffr = s7_make_signature(s7, 4, b, f, f, r);
    pl_fffi = s7_make_signature(s7, 4, f, f, f, i);
    pl_ffiib = s7_make_signature(s7, 5, f, f, i, i, b);
    pl_ffiif = s7_make_signature(s7, 5, f, f, i, i, f);
    pl_rfvir = s7_make_signature(s7, 5, r, f, s7_make_symbol(s7, "int-vector?"), i, r);
    pl_rfiir = s7_make_circular_signature(s7, 4, 5, r, f, i, i, r);
  }
#endif

  Xen_define_typed_procedure(S_vct_multiply,      g_vct_multiply_w,  2, 0, 0, H_vct_multiplyB,		pl_fff);
  Xen_define_typed_procedure(S_vct_add,           g_vct_add_w,       2, 1, 0, H_vct_addB,		pl_fffi);
  Xen_define_typed_procedure(S_vct_subtract,      g_vct_subtract_w,  2, 0, 0, H_vct_subtractB,		pl_fff);
  Xen_define_typed_procedure(S_vct_offset,        g_vct_offset_w,    2, 0, 0, H_vct_offsetB,		pl_ffr);
  Xen_define_typed_procedure(S_vct_peak,          g_vct_peak_w,      1, 0, 0, H_vct_peak,		pl_rf);
  Xen_define_typed_procedure(S_vct_peak_and_location, g_vct_peak_and_location_w, 1, 0, 0, H_vct_peak_and_location, pl_pf);
  Xen_define_typed_procedure(S_vct_move,          g_vct_move_w,      3, 1, 0, H_vct_moveB,		pl_ffiib);
  Xen_define_typed_procedure(S_vct_subseq,        g_vct_subseq_w,    2, 2, 0, H_vct_subseq,		pl_ffiif);
  Xen_define_typed_procedure(S_vct_copy,          g_vct_copy_w,      1, 0, 0, H_vct_copy,		pl_ff);

#if HAVE_FORTH
  Xen_define_dilambda(S_vct_ref,                 g_vct_ref_w, H_vct_ref, "set-" S_vct_ref, g_vct_set_w,  2, 0, 3, 0);
#else
#if (!HAVE_SCHEME)
  Xen_define_procedure(S_vct_ref,                g_vct_ref_w,       2, 0, 0, H_vct_ref);
#endif
#endif

  Xen_define_typed_procedure(S_vct_to_string,     g_vct_to_readable_string_w, 1, 0, 0, H_vct_to_string, pl_sf);
  Xen_define_typed_procedure(S_vct_times,         g_vct_times_w,     2, 0, 0, H_vct_times,		pl_ftt);
  Xen_define_typed_procedure(S_vct_plus,          g_vct_plus_w,      2, 0, 0, H_vct_plus,		pl_ftt);
  Xen_define_typed_procedure(S_vct_max,           g_vct_max_w,       1, 0, 0, H_vct_max,		pl_rf);
  Xen_define_typed_procedure(S_vct_min,           g_vct_min_w,       1, 0, 0, H_vct_min,		pl_rf);
  Xen_define_typed_procedure(S_vct_scale,         g_vct_scale_w,     2, 0, 0, H_vct_scaleB,		pl_ftt);
  Xen_define_typed_procedure(S_vct_abs,           g_vct_abs_w,       1, 0, 0, H_vct_absB,		pl_ff);
  Xen_define_typed_procedure(S_vct_equal,         g_vct_equal_w,     3, 0, 0, H_vct_equal,		pl_bffr);

#if (!HAVE_SCHEME)
  Xen_define_procedure(S_vct_set,           g_vct_set_w,       3, 0, 0, H_vct_setB);
  Xen_define_procedure(S_is_vct,            g_is_vct_w,        1, 0, 0, H_is_vct);
  Xen_define_procedure(S_vct_fill,          g_vct_fill_w,      2, 0, 0, H_vct_fillB);
  Xen_define_procedure(S_vct,               g_vct_w,           0, 0, 1, H_vct);
  Xen_define_procedure(S_vct_length,        g_vct_length_w,    1, 0, 0, H_vct_length);
  Xen_define_procedure(S_vct_reverse,       g_vct_reverse_w,   1, 1, 0, H_vct_reverse);
  Xen_define_procedure(S_vct_to_list,       g_vct_to_list_w,   1, 0, 0, H_vct_to_list);
  Xen_define_procedure(S_list_to_vct,       g_list_to_vct_w,   1, 0, 0, H_list_to_vct);
  Xen_define_procedure(S_vector_to_vct,     g_vector_to_vct_w, 1, 0, 0, H_vector_to_vct);
  Xen_define_procedure(S_vct_to_vector,     g_vct_to_vector_w, 1, 0, 0, H_vct_to_vector);
  Xen_define_procedure(S_make_vct,          g_make_vct_w,      1, 1, 0, H_make_vct);
#else
  Xen_define_typed_procedure(S_vct_spatter,     g_vct_spatter_w,     4, 0, 0, H_vct_spatter,           pl_rfvir);
  Xen_define_typed_procedure(S_vct_interpolate, g_vct_interpolate_w, 7, 0, 0, H_vct_interpolate,       pl_rfiir);

  s7_set_d_p_function(s7, s7_name_to_value(s7, S_vct_min), float_vector_min_d_p);
  s7_set_d_p_function(s7, s7_name_to_value(s7, S_vct_max), float_vector_max_d_p);
#endif
}
