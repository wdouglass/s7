#ifndef SND_H
#define SND_H

#include <mus-config.h>

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <limits.h>
#include <errno.h>
#include <stdlib.h>
#ifndef _MSC_VER
  #include <unistd.h>
  #include <locale.h>
#endif
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdint.h>
#include <inttypes.h>

#include "_sndlib.h"
#include "xen.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "vct.h"
#include "snd-0.h"

#ifdef USE_MOTIF
  #include "snd-x0.h"
#else
  #if USE_GTK
    #include "snd-g0.h"
  #else
    #include "snd-nogui0.h"
  #endif
#endif

#include "snd-1.h"

#ifdef USE_MOTIF
  #include "snd-x1.h"
#else
  #if USE_GTK
    #include "snd-g1.h"
  #else
    #include "snd-nogui1.h"
  #endif
#endif

#include "snd-strings.h"

#define SND_DATE "26-June-19"
#ifndef SND_VERSION
#define SND_VERSION "19.5"
#endif
#define SND_MAJOR_VERSION "19"
#define SND_MINOR_VERSION "5"

#endif
