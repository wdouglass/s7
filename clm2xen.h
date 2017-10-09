#ifndef CLM2XEN_H
#define CLM2XEN_H

#include "vct.h"

typedef struct mus_xen mus_xen;

#define Xen_to_mus_xen(arg) ((mus_xen *)Xen_object_ref(arg))
#define Xen_to_mus_any(obj) mus_xen_gen(Xen_to_mus_xen(obj))
#define MUS_CLM_DEFAULT_TABLE_SIZE 512

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT mus_long_t clm_default_table_size_c(void);

MUS_EXPORT mus_any *mus_xen_gen(mus_xen *x);
MUS_EXPORT bool mus_is_xen(Xen obj);
MUS_EXPORT const char *mus_fft_window_xen_name(mus_fft_window_t i);
MUS_EXPORT Xen mus_xen_to_object(mus_xen *gn);
MUS_EXPORT Xen mus_xen_to_object_with_vct(mus_xen *gn, Xen v);
MUS_EXPORT mus_any *mus_optkey_to_mus_any(Xen key, const char *caller, int n, mus_any *def);
MUS_EXPORT int mus_optkey_unscramble(const char *caller, int num_args, int nkeys, Xen *keys, Xen *args, int *orig);
MUS_EXPORT mus_float_t mus_optkey_to_float(Xen key, const char *caller, int n, mus_float_t def);
MUS_EXPORT int mus_optkey_to_int(Xen key, const char *caller, int n, int def);
MUS_EXPORT bool mus_optkey_to_bool(Xen key, const char *caller, int n, bool def);
MUS_EXPORT mus_long_t mus_optkey_to_mus_long_t(Xen key, const char *caller, int n, mus_long_t def);
MUS_EXPORT const char *mus_optkey_to_string(Xen key, const char *caller, int n, char *def);
MUS_EXPORT Xen mus_optkey_to_procedure(Xen key, const char *caller, int n, Xen def, int required_args, const char *err);

MUS_EXPORT mus_xen *mus_any_to_mus_xen(mus_any *ge);
MUS_EXPORT mus_xen *mus_any_to_mus_xen_with_vct(mus_any *ge, Xen v);
MUS_EXPORT mus_xen *mus_any_to_mus_xen_with_two_vcts(mus_any *ge, Xen v1, Xen v2);

MUS_EXPORT Xen g_mus_channels(Xen obj);
MUS_EXPORT Xen g_mus_length(Xen gen);
MUS_EXPORT Xen g_mus_file_name(Xen gen);
MUS_EXPORT Xen g_mus_data(Xen gen);

#if HAVE_SCHEME
MUS_EXPORT void s7_init_sndlib(s7_scheme *sc);
#endif

MUS_EXPORT void Init_sndlib(void);
#ifdef __cplusplus
}
#endif

#endif
