#ifndef SNDLIB2XEN_H
#define SNDLIB2XEN_H

#include "xen.h"

/* error indications */

#define NO_SUCH_CHANNEL Xen_make_error_type("no-such-channel")
#define NO_SUCH_FILE    Xen_make_error_type("no-such-file")
#define BAD_TYPE        Xen_make_error_type("bad-type")
#define NO_DATA         Xen_make_error_type("no-data")
#define BAD_HEADER      Xen_make_error_type("bad-header")

#ifdef __cplusplus
extern "C" {
#endif

MUS_EXPORT void mus_sndlib_xen_initialize (void);
MUS_EXPORT Xen g_mus_sound_srate(Xen filename);    /* snd-snd.c */
MUS_EXPORT Xen g_mus_sound_chans(Xen filename);    /* snd-snd.c */
MUS_EXPORT Xen g_mus_sound_framples(Xen filename); /* snd-chn.c */
MUS_EXPORT Xen g_mus_expand_filename(Xen file);    /* snd-snd.c */
MUS_EXPORT Xen g_mus_sound_maxamp(Xen file);       /* snd-chn.c */

MUS_EXPORT Xen g_mus_sound_path(void);

#ifdef __cplusplus
}
#endif

#endif
