#ifndef VCT_H
#define VCT_H

#if HAVE_SCHEME
  typedef struct s7_cell vct;
#else
typedef struct vct vct;
#endif

#include "xen.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef WITH_VECTORIZE
#if (defined(__GNUC__) && __GNUC__ >= 5) && (!CLM)
  #define WITH_VECTORIZE 1
#else
  #define WITH_VECTORIZE 0
#endif
#endif

#if WITH_VECTORIZE
MUS_EXPORT void mus_clear_floats(mus_float_t *arr, mus_long_t len) __attribute__ ((optimize("tree-vectorize")));
MUS_EXPORT void mus_copy_floats(mus_float_t *dst, mus_float_t *src, mus_long_t len) __attribute__ ((optimize("tree-vectorize")));
MUS_EXPORT void mus_add_floats(mus_float_t *dst, mus_float_t *src, mus_long_t len) __attribute__ ((optimize("tree-vectorize")));
MUS_EXPORT void mus_abs_floats(mus_float_t *dst, mus_long_t len) __attribute__ ((optimize("tree-vectorize")));
#else
#define mus_clear_floats(Arr, Len)		\
  do {						\
    mus_long_t K;				\
    mus_float_t *dst;				\
    dst = Arr;					\
    for (K = Len; K > 0; K--)			\
      *dst++ = 0.0;				\
  } while (0)
#define mus_copy_floats(Dst, Src, Len)		\
  do {						\
    mus_long_t K;				\
    mus_float_t *dst, *src;			\
    dst = Dst;					\
    src = Src;					\
    for (K = Len; K > 0; K--)			\
      *dst++ = *src++;				\
    } while (0)
#define mus_add_floats(Dst, Src, Len)		\
  do {						\
    mus_long_t K;				\
    mus_float_t *dst, *src;			\
    dst = Dst;					\
    src = Src;					\
    for (K = Len; K > 0; K--)			\
      *dst++ += *src++;				\
    } while (0)
#define mus_abs_floats(Dst, Len)		\
  do {						\
    mus_long_t K;				\
    mus_float_t *dst;				\
    dst = Dst;					\
    for (K = 0; K < Len; K++)			\
      dst[K] = fabs(dst[K]);			\
    } while (0)
#endif

MUS_EXPORT void mus_vct_init(void);
MUS_EXPORT int mus_vct_print_length(void);
MUS_EXPORT void mus_vct_set_print_length(int len);
MUS_EXPORT Xen mus_array_to_list(mus_float_t *arr, mus_long_t i, mus_long_t len);
MUS_EXPORT bool mus_vct_is_equal(vct *v1, vct *v2);
MUS_EXPORT char *mus_vct_to_readable_string(vct *v);
MUS_EXPORT vct *mus_vct_make(mus_long_t len);
MUS_EXPORT vct *mus_vct_free(vct *v);
MUS_EXPORT double mus_vct_peak(vct *v);

MUS_EXPORT Xen xen_list_to_vct(Xen lst);
MUS_EXPORT vct *xen_to_vct(Xen arg);
MUS_EXPORT Xen xen_make_vct(mus_long_t len, mus_float_t *data);
MUS_EXPORT Xen xen_make_vct_wrapper(mus_long_t len, mus_float_t *data);
MUS_EXPORT Xen g_vct_peak(Xen obj);

MUS_EXPORT vct *mus_vct_wrap(mus_long_t len, mus_float_t *data);

#if HAVE_SCHEME
  #define S_vct             "float-vector"
  #define Xen_to_vct(Obj)   (vct *)Obj
  #define mus_vct_length(V) s7_vector_length((s7_pointer)V)
  #define mus_vct_data(V)   s7_float_vector_elements((s7_pointer)V)
  #define mus_is_vct(Obj)   s7_is_float_vector(Obj)
  #define vct_to_xen(V)     (Xen)V
#else
  #define S_vct "vct"
  #define Xen_to_vct(arg) ((vct *)Xen_object_ref(arg))
  MUS_EXPORT mus_long_t mus_vct_length(vct *v);
  MUS_EXPORT mus_float_t *mus_vct_data(vct *v);
  MUS_EXPORT bool mus_is_vct(Xen obj);
  MUS_EXPORT Xen vct_to_xen(vct *v);
#endif

#ifdef __cplusplus
}
#endif

#endif
