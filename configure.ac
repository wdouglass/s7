# Configuration script for Snd
#
# we now depend completely on pc (pkgconfig) files.
#   Motif apparently has none.
#   gmp, mpfr, and mpc deliberately have none!


AC_INIT(snd, 19.8, bil@ccrma.stanford.edu, ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-19.tar.gz)

AC_CONFIG_SRCDIR(snd.c)
AC_CANONICAL_HOST # needed by case $host below
AC_CONFIG_HEADERS(mus-config.h)
AC_CONFIG_FILES(makefile)
AC_PROG_CC

# AC_HEADER_STDC
AC_PROG_INSTALL

MAKE_TARGET="snd"
AUDIO_SYSTEM="None"
RANDOM_FEATURES=""
OPTIONAL_LIBRARIES=""
LOCAL_LANGUAGE="None"
GRAPHICS_TOOLKIT="None"

PACKAGE=Snd
VERSION=19.8

#--------------------------------------------------------------------------------
# configuration options
#   --with-motif          use Motif
#   --with-gtk            use Gtk (the default)
#   --with-alsa           use ALSA if possible (the default)
#   --with-oss            use OSS 
#   --with-jack           use Jack
#   --without-audio       stub out all audio
#   --with-gmp            include multiprecision arithmetic via gmp, mpfr, and mpc
#   --disable-deprecated  do not include any deprecated stuff (in gtk, motif, s7, sndlib, clm, etc)
#   --with-ladspa         include LADSPA plugin support (Linux)
#   --with-gui            make Snd with(out) any graphics support
#   --with-forth          use Forth as the extension language
#   --with-ruby           use Ruby as the extension language
#   --with-s7             use s7 as the extension language (default = yes)
#   --with-pure-s7        s7 WITH_PURE_S7
#   --with-s7-history     s7 WITH_HISTORY
#   --with-s7-profiling   s7 WITH_PROFILE
#   --with-extension-language use some extension language (default=yes)
#   --with-temp-dir       directory to use for temp files
#   --with-save-dir       directory to use for saved-state files
#   --with-doc-dir        directory to search for documentation
#   --with-gl             include OpenGL support (default=no, Motif only)
#   --with-gl2ps          include gl2ps (Motif only)
#   --with-editres 	  include EditRes in xm
#   --without-gsl         omit GSL even if it exists
#   --without-fftw        omit FFTW even if it exists
#   --with-pulseaudio     use PulseAudio
#   --with-portaudio      use portaudio
#   --with-webserver      use webserver


AC_ARG_WITH(gtk,         [  --with-gtk		  use Gtk to build Snd])
AC_ARG_WITH(gui,         [  --with-gui  	      	  make Snd with graphics support])
AC_ARG_WITH(gl,          [  --with-gl		  include OpenGL support, Motif only])
AC_ARG_WITH(gl2ps,       [  --with-gl2ps		  include gl2ps, Motif only])
AC_ARG_WITH(motif,	 [  --with-motif	  	  use libXm to build Snd])
AC_ARG_WITH(editres,     [  --with-editres	  include editres in xm])

AC_ARG_WITH(alsa,        [  --with-alsa		  use ALSA])
AC_ARG_WITH(oss,         [  --with-oss		  use OSS])
AC_ARG_WITH(jack,        [  --with-jack		  use JACK])
AC_ARG_WITH(ladspa,	 [  --with-ladspa  	  include support for LADSPA plugins, Linux only])
AC_ARG_WITH(pulseaudio,  [  --with-pulseaudio 	  use PulseAudio, default=no])
AC_ARG_WITH(portaudio,   [  --with-portaudio 	  use portaudio, default=no])

AC_ARG_WITH(extension-language, [  --with-extension-language use some extension language, default=yes])
AC_ARG_WITH(s7,          [  --with-s7  	          use S7, default=yes])
AC_ARG_WITH(forth,       [  --with-forth	  	  use Forth as the extension language])
AC_ARG_WITH(ruby,        [  --with-ruby             use Ruby as the extension language])

AC_ARG_WITH(pure-s7,     [  --with-pure-s7	  s7 WITH_PURE_S7])
AC_ARG_WITH(s7-history,  [  --with-s7-history	  s7 WITH_HISTORY])
AC_ARG_WITH(s7-profiling, [  --with-s7-profiling	  s7 WITH_PROFILE])

AC_ARG_WITH(gsl,         [  --with-gsl		  use GSL, default=yes])
AC_ARG_WITH(fftw,        [  --with-fftw		  use fftw, default=yes])
AC_ARG_WITH(gmp,         [  --with-gmp		  include multiprecision arithmetic via gmp, mpfr, and mpc, default=no])
AC_ARG_WITH(audio,       [  --without-audio         don't include any audio functionality])

AC_ARG_WITH(temp-dir,    [  --with-temp-dir	  directory to use for temp files], AC_DEFINE_UNQUOTED(MUS_DEFAULT_TEMP_DIR, "${withval}"))
AC_ARG_WITH(save-dir,    [  --with-save-dir	  directory to use for saved-state files], AC_DEFINE_UNQUOTED(MUS_DEFAULT_SAVE_DIR, "${withval}"))
AC_ARG_WITH(doc-dir,     [  --with-doc-dir	  directory to search for documentation], AC_DEFINE_UNQUOTED(MUS_DEFAULT_DOC_DIR, "${withval}"))
AC_ARG_WITH(webserver,   [  --with-webserver 	  use webserver, default=no])

AC_ARG_ENABLE(deprecated,[  --disable-deprecated	  do not include any deprecated stuff from gtk, s7, motif, clm, snd, or sndlib])


AC_C_BIGENDIAN
AC_CHECK_SIZEOF(void *)
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)



#--------------------------------------------------------------------------------
# fftw
#--------------------------------------------------------------------------------

FFTW_LIBS=""
FFTW_CFLAGS=""
if test "$with_fftw" != no; then
  AC_MSG_CHECKING(for fftw3)
  if test x$PKG_CONFIG != xno ; then
    if $PKG_CONFIG fftw3 --exists ; then
      FFTW_LIBS="`$PKG_CONFIG fftw3 --libs`"
      FFTW_CFLAGS="`$PKG_CONFIG fftw3 --cflags`"
      AC_DEFINE(HAVE_FFTW3)
      OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw-`$PKG_CONFIG fftw3 --modversion`"
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi
  fi
fi
AC_SUBST(FFTW_LIBS)
AC_SUBST(FFTW_CFLAGS)




#--------------------------------------------------------------------------------
# GMP, MPFR, MPC
#--------------------------------------------------------------------------------

# no pc files -- deliberately! We'll just add the libraries and let the chips fall...

GMP_LIBS=""

if test "$with_gmp" = yes ; then
  GMP_LIBS="-lgmp -lmpfr -lmpc -lm"  
  AC_DEFINE(WITH_GMP)
  OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gmp mpfr mpc"
fi

AC_SUBST(GMP_LIBS)



#--------------------------------------------------------------------------------
# GSL
#--------------------------------------------------------------------------------

GSL_LIBS=""
GSL_CFLAGS=""

if test "$with_gsl" != no; then
  AC_MSG_CHECKING(for gsl)
  if test x$PKG_CONFIG != xno ; then
    if $PKG_CONFIG gsl --exists ; then
      GSL_LIBS="`$PKG_CONFIG gsl --libs`"
      GSL_CFLAGS="`$PKG_CONFIG gsl --cflags`"
      AC_DEFINE(HAVE_GSL)
      OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gsl-`$PKG_CONFIG gsl --modversion`"
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi
  fi
fi

AC_SUBST(GSL_LIBS)
AC_SUBST(GSL_CFLAGS)



#--------------------------------------------------------------------------------
# Ladspa
#--------------------------------------------------------------------------------

if test "$with_ladspa" = yes ; then
  AC_DEFINE(HAVE_LADSPA)
  RANDOM_FEATURES="$RANDOM_FEATURES ladspa"
fi



#--------------------------------------------------------------------------------
# graphics
#--------------------------------------------------------------------------------

# graphics: motif chosen if --with-motif
#           no gui chosen if --without-gui
#           gtk chosen if neither of above and we can find gtk-2.0.pc or gtk-3.0.pc, or gtk4.pc?
#           else no gui

XLIBS=""
XFLAGS=""

GX_FILES=""
GX_HEADERS=""

GTK_FILES=""
GTK_HEADERS=""
GTK_CFLAGS=""
GTK_LIBS=""
GTK_LD_LIBS=""

ac_snd_gui_choice=none

if test "$with_motif" = no && test "$with_gtk" = no ; then
  with_gui=no
fi

if test "$with_gui" = no ; then
  AC_DEFINE(USE_NO_GUI)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"
  ac_snd_gui_choice=no
fi


#--------------------------------------------------------------------------------
# X/Motif
#--------------------------------------------------------------------------------

# here as in the gmp case, we simply set up the libs/flags and hope for the best

if test "$with_motif" = yes ; then
  AC_PATH_XTRA

  GTK_FLAGS="$X_CFLAGS"
  X_POST_LIBS="-lX11 $X_EXTRA_LIBS -lXext"
  case "$host" in
    *-apple-*)
      X_POST_LIBS="$X_POST_LIBS -lSM -lICE"
      ;;
    *-*-linux*) 
      X_POST_LIBS="$X_POST_LIBS -lSM -lICE -lXft"
      ;;
  esac
  X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt"

  GX_FILES="MOTIF_O_FILES"
  GX_HEADERS="SND_X_HEADERS"

  if test "$with_editres" = yes ; then
    AC_DEFINE(WITH_EDITRES)
    OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES editres"
    X_PRE_LIBS="$X_PRE_LIBS -lXmu"
  fi

  GTK_LIBS="$X_PRE_LIBS $X_POST_LIBS -lXpm"
  AC_DEFINE(USE_MOTIF)
  GRAPHICS_TOOLKIT="Motif"
  ac_snd_gui_choice=Motif
fi


#--------------------------------------------------------------------------------
# Gtk
#--------------------------------------------------------------------------------

if test "$ac_snd_gui_choice" = none ; then
  if test x$PKG_CONFIG != xno ; then

#    m4_foreach([gtk_version], [[gtk4], [gtk+-3.0], [gtk+-2.0]],
     m4_foreach([gtk_version], [[gtk+-3.0], [gtk+-2.0]],
       [
       if test "$ac_snd_gui_choice" = none ; then
         if $PKG_CONFIG gtk_version --exists ; then
           GTK_CFLAGS="`$PKG_CONFIG gtk_version --cflags`"
           GTK_LIBS="`$PKG_CONFIG gtk_version --libs`"
           GTK_LD_LIBS="`$PKG_CONFIG gtk_version --libs-only-L` `$PKG_CONFIG gtk_version --libs-only-l`"
           ac_snd_gui_choice=gtk
           GRAPHICS_TOOLKIT=Gtk-`$PKG_CONFIG gtk_version --modversion`
         fi
       fi
       ])
 
    if test "$ac_snd_gui_choice" = gtk ; then  
      GX_FILES="GTK_O_FILES"
      GX_HEADERS="SND_G_HEADERS"
      AC_DEFINE(USE_GTK)
 
      if test x$PKG_CONFIG != xno ; then
        CAIRO_CFLAGS="`$PKG_CONFIG cairo --cflags-only-I`"
        AC_SUBST(CAIRO_CFLAGS)
      fi
    fi
  fi
fi


#--------------------------------------------------------------------------------
# no GUI
#--------------------------------------------------------------------------------

if test "$ac_snd_gui_choice" = none ; then
  AC_DEFINE(USE_NO_GUI)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"
fi

# fallback on no-gui
if test "$with_gui" = no ; then
  AC_DEFINE(USE_NO_GUI)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"
  ac_snd_gui_choice=no
fi


AC_SUBST(XLIBS)
AC_SUBST(XFLAGS)

AC_SUBST(GX_FILES)
AC_SUBST(GX_HEADERS)

AC_SUBST(GTK_LIBS)
AC_SUBST(GTK_CFLAGS)
AC_SUBST(GTK_LD_LIBS)




#--------------------------------------------------------------------------------
# OpenGL
#--------------------------------------------------------------------------------

GL_LIBS=""
GL_FILES=""
GL_FLAGS=""

if test "$with_gl" = yes ; then
  if test x$PKG_CONFIG != xno ; then
    if $PKG_CONFIG gl --exists ; then

      GL_CFLAGS="`$PKG_CONFIG gl --cflags`"
      GL_LIBS="`$PKG_CONFIG gl --libs`"
      GL_FILES="gl.o"
      AC_DEFINE(HAVE_GL)	    
      OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES openGL"

      if $PKG_CONFIG glu --exists ; then
          AC_DEFINE(HAVE_GLU)
          GL_CFLAGS="$GL_CFLAGS `$PKG_CONFIG glu --cflags`"
          GL_LIBS="$GL_LIBS `$PKG_CONFIG glu --libs`"
      fi

      if test "$with_gl2ps" = yes ; then
        AC_DEFINE(WITH_GL2PS)
        RANDOM_FEATURES="$RANDOM_FEATURES gl2ps"
        GL_FILES="$GL_FILES gl2ps.o"
      fi
    fi
  fi
fi

AC_SUBST(GL_LIBS)
AC_SUBST(GL_FILES)
AC_SUBST(GL_FLAGS)



#--------------------------------------------------------------------------------
# language
#--------------------------------------------------------------------------------

# language choice: ruby if --with-ruby and we can find a ruby pc file
#                  forth if --with-forth
#                  none if --without-extension-language
#                  s7 otherwise

XEN_LIBS=""
XEN_CFLAGS=""
ac_snd_extension_language=none

if test "$with_extension_language" = no ; then
  ac_snd_extension_language=no
  LOCAL_LANGUAGE="None"
fi


#--------------------------------------------------------------------------------
# Ruby
#--------------------------------------------------------------------------------

# Yuri suggests: ruby -r rbconfig -e 'C = RbConfig::CONFIG' -e 'puts C["ruby_version"]' | sed -E 's|([[:digit:]]+\.[[:digit:]]+).*|\1|'

if test "$with_ruby" = yes ; then
  if test x$PKG_CONFIG != xno ; then
    m4_foreach([ruby_version], [[ruby-2.6], [ruby-2.5], [ruby-2.4], [ruby-2.3], [ruby-2.2], [ruby-2.1], [ruby-2.0], [ruby], [ruby-1.9.3], [ruby-1.9], [ruby-1.8]],
      [
      if test "$ac_snd_extension_language" = none ; then
        if $PKG_CONFIG ruby_version --exists ; then
          AC_DEFINE(HAVE_RUBY)
          XEN_CFLAGS="-fPIC `$PKG_CONFIG ruby_version --cflags`"
          # this depends on building ruby itself with the --enable-shared flag
          XEN_LIBS="`$PKG_CONFIG ruby_version --libs`"
          LOCAL_LANGUAGE=Ruby-`$PKG_CONFIG ruby_version --modversion`
          ac_snd_extension_language=Ruby
          if test "$ac_snd_gui_choice" = gtk ; then
            S7_LIB="xg.o"
          else
            S7_LIB=""
          fi
          AC_SUBST(S7_LIB)
        fi
      fi
      ])
  fi
fi


#--------------------------------------------------------------------------------
# Forth
#--------------------------------------------------------------------------------

if test "$with_forth" = yes ; then
  AC_PATH_PROG([FTH], [fth], [no])
  AC_MSG_CHECKING([for Forth])
  if test "${FTH}" != no ; then
    XEN_CFLAGS=`${FTH} --no-init-file --eval .cflags`
    XEN_LIBS=`${FTH} --no-init-file --eval .libs`
    AC_MSG_RESULT([yes])
    AC_DEFINE(HAVE_FORTH)
    LOCAL_LANGUAGE="Forth"
    ac_snd_extension_language=Forth
    if test "$ac_snd_gui_choice" = gtk ; then
      S7_LIB="xg.o"
    else
      S7_LIB=""
    fi
    AC_SUBST(S7_LIB)
  else
    AC_MSG_RESULT([no])
  fi
fi


#--------------------------------------------------------------------------------
# s7 (the default)
#--------------------------------------------------------------------------------

if test "$with_s7" != no && test "$ac_snd_extension_language" = none ; then
  AC_DEFINE(HAVE_SCHEME)	
  ac_snd_extension_language=s7
  LOCAL_LANGUAGE="s7"
  if test "$ac_snd_gui_choice" = gtk ; then
    S7_LIB="s7.o libgtk_s7.o"
  else
    S7_LIB="s7.o"
  fi
  AC_SUBST(S7_LIB)
fi

AC_SUBST(XEN_LIBS)
AC_SUBST(XEN_CFLAGS)




#--------------------------------------------------------------------------------
# OGG, Flac, Speex, Mpeg, Timidity, TTA, Wavpack
# --------------------------------------------------------------------------------

AC_PATH_PROG(PATH_OGGDEC, oggdec, no) # OGG read
AC_PATH_PROG(PATH_OGGENC, oggenc, no) # OGG write

if test "$PATH_OGGDEC" != "no" ; then
  if test "$PATH_OGGENC" != "no" ; then
    AC_DEFINE(HAVE_OGG)
    AC_DEFINE_UNQUOTED(PATH_OGGDEC, "${PATH_OGGDEC}")
    AC_DEFINE_UNQUOTED(PATH_OGGENC, "${PATH_OGGENC}")
  fi
fi


AC_PATH_PROG(PATH_MPG123, mpg123, no) # MPEG read/write?

if test "$PATH_MPG123" != "no" ; then
  AC_DEFINE(HAVE_MPEG)
  AC_DEFINE_UNQUOTED(PATH_MPG123, "${PATH_MPG123}")
fi


AC_PATH_PROG(PATH_MPG321, mpg321, no) # MPEG read/write?

if test "$PATH_MPG321" != "no" ; then
  AC_DEFINE(HAVE_MPEG)
  AC_DEFINE(HAVE_MPG321)
  AC_DEFINE_UNQUOTED(PATH_MPG321, "${PATH_MPG321}")
fi


AC_PATH_PROG(PATH_SPEEXDEC, speexdec, no) # Speex read
AC_PATH_PROG(PATH_SPEEXENC, speexenc, no) # Speex write

if test "$PATH_SPEEXDEC" != "no" ; then
  if test "$PATH_SPEEXENC" != "no" ; then
    AC_DEFINE(HAVE_SPEEX)
    AC_DEFINE_UNQUOTED(PATH_SPEEXDEC, "${PATH_SPEEXDEC}")
    AC_DEFINE_UNQUOTED(PATH_SPEEXENC, "${PATH_SPEEXENC}")
  fi
fi


AC_PATH_PROG(PATH_FLAC, flac, no) # Flac read/write

if test "$PATH_FLAC" != "no" ; then
  AC_DEFINE(HAVE_FLAC)
  AC_DEFINE_UNQUOTED(PATH_FLAC, "${PATH_FLAC}")
fi


AC_PATH_PROG(PATH_TIMIDITY, timidity, no) # Timidity for .mid -> .wav

if test "$PATH_TIMIDITY" != "no" ; then
  AC_DEFINE(HAVE_TIMIDITY)
  AC_DEFINE_UNQUOTED(PATH_TIMIDITY, "${PATH_TIMIDITY}")
fi


AC_PATH_PROG(PATH_WAVPACK, wavpack, no)
AC_PATH_PROG(PATH_WVUNPACK, wvunpack, no)

if test "$PATH_WAVPACK" != "no" ; then
  if test "$PATH_WVUNPACK" != "no" ; then
    AC_DEFINE(HAVE_WAVPACK)
    AC_DEFINE_UNQUOTED(PATH_WAVPACK, "${PATH_WAVPACK}")
    AC_DEFINE_UNQUOTED(PATH_WVUNPACK, "${PATH_WVUNPACK}")
  fi
fi




#--------------------------------------------------------------------------------
# Audio
#--------------------------------------------------------------------------------

AUDIO_LIB=""
JACK_LIBS=""
JACK_FLAGS=""

if test "$with_audio" != no ; then

  if test "$with_pulseaudio" = yes ; then
    AC_DEFINE(MUS_PULSEAUDIO)
    AUDIO_LIB="-lpulse-simple -lpulse"
    AUDIO_SYSTEM=pulseaudio
  fi
  
  if test "$with_portaudio" = yes ; then
    AC_DEFINE(MUS_PORTAUDIO)
    AUDIO_SYSTEM=portaudio
    AUDIO_LIB="-lportaudio"
  fi

  if test "$with_jack" = yes ; then
    if test "$with_alsa" = yes ; then 
      AUDIO_SYSTEM=ALSA+JACK
    else
      AUDIO_SYSTEM=JACK
    fi
    AC_DEFINE(MUS_JACK)
    if test x$PKG_CONFIG != xno ; then
      if $PKG_CONFIG jack --exists ; then    
        JACK_LIBS="`$PKG_CONFIG jack --libs`"
        JACK_FLAGS="`$PKG_CONFIG jack --cflags`"
        if $PKG_CONFIG samplerate --exists ; then    
  	  JACK_LIBS="$JACK_LIBS `$PKG_CONFIG samplerate --libs`"
  	  JACK_FLAGS="$JACK_FLAGS `$PKG_CONFIG samplerate --cflags`"
        else
          JACK_LIBS="$JACK_LIBS -lsamplerate"
        fi
      else
        JACK_LIBS="-ljack -lsamplerate"
      fi
    else
      JACK_LIBS="-ljack -lsamplerate"
    fi
  JACK_LIBS="$JACK_LIBS -lpthread"
  fi

  if test "$with_alsa" = yes ; then
    AC_DEFINE(HAVE_ALSA)
    AUDIO_LIB="-lasound"
    if test "$with_jack" = yes ; then
      AUDIO_SYSTEM=ALSA+JACK
    else
      AUDIO_SYSTEM=ALSA
    fi
  fi

  if test "$with_oss" = yes ; then
    AC_DEFINE(HAVE_OSS)
    AUDIO_SYSTEM=OSS
  fi

  if test "$AUDIO_SYSTEM" = None ; then
    case "$host" in
      *-*-linux*) 
        AUDIO_SYSTEM=ALSA
        AC_DEFINE(HAVE_ALSA)
        AUDIO_LIB="-lasound"
	;;
      *-*-sunos4*) 
	AUDIO_SYSTEM=Sun
        ;;
      *-*-solaris*) 
	AUDIO_SYSTEM=Sun
        ;;
      *-*-hpux*) 
	AUDIO_SYSTEM=Hpux
        ;;
      *-*-bsdi*) 
	AC_DEFINE(HAVE_OSS)
	AUDIO_SYSTEM=OSS
        ;;
      *-*-freebsd*)
	AC_DEFINE(HAVE_OSS)
	AUDIO_SYSTEM=OSS
	;;
      *-*-openbsd*) 
	AUDIO_SYSTEM=OpenBSD
	AUDIO_LIB="-lsndio"
        ;;
      *-*-netbsd*) 
	AUDIO_SYSTEM=NetBSD
        ;;
      *-*-cygwin*)
	if test "$with_jack" != yes ; then 
	  AUDIO_SYSTEM=Windows
	fi
	;;
      *-*-mingw*)
	audio_system=Windows
	;;
      *-apple-*)
	if test "$with_jack" != yes ; then 
	  AUDIO_SYSTEM=MacOSX
	  AUDIO_LIB="-framework CoreAudio -framework CoreFoundation -framework CoreMIDI"
        else
          AUDIO_SYSTEM=JACK
	  JACK_LIBS="-framework CoreAudio -framework CoreServices -framework AudioUnit -L/usr/local/lib -ljack -lsamplerate"
          JACK_FLAGS="-I/usr/local/include"
	fi
	;;
    esac
  fi
fi

AC_MSG_CHECKING([for audio system])
AC_MSG_RESULT($AUDIO_SYSTEM)

if test "$AUDIO_SYSTEM" != None ; then
  AC_DEFINE(WITH_AUDIO)
fi

AC_SUBST(AUDIO_LIB)
AC_SUBST(JACK_LIBS)
AC_SUBST(JACK_FLAGS)



#--------------------------------------------------------------------------------
# compiler/loader flags
#--------------------------------------------------------------------------------

LIBS=""
LDSO_FLAGS=""
SO_FLAGS=""
SO_LD="ld"
# the SO_* stuff here is for consistency with the sndlib configure script

case "$host" in
  *-*-linux*) 
    LDSO_FLAGS="-shared"
    LIBS="$LIBS -lm -ldl"
    if test "$GCC" = yes ; then
      SO_FLAGS="-fPIC $SO_FLAGS"
      SO_LD="$CC"
    fi
    ;;

  *-*-kfreebsd*|*-*-gnu*)
      LDSO_FLAGS="-shared"
      LIBS="$LIBS -lm -ldl"
      if test "$GCC" = yes ; then
        SO_FLAGS="-fPIC $SO_FLAGS"
      fi

      AUDIO_SYSTEM=OSS

      if test "$with_jack" = yes ; then
             if test "$with_oss" != yes ; then
               AUDIO_SYSTEM=JACK
             fi
      fi

      case $AUDIO_SYSTEM in
          JACK)
              AC_DEFINE(HAVE_JACK_IN_LINUX)
              AC_DEFINE(HAVE_OSS)
              AUDIO_LIB="-lsamplerate"
              ;;
          OSS)
              AC_DEFINE(HAVE_OSS)
              AUDIO_SYSTEM=OSS
              ;;
      esac
      ;;

  *-*-sunos4*) 
    LIBS="$LIBS -lm"
    ;;

  *-*-solaris*) 
    LIBS="$LIBS -lm"
    LDSO_FLAGS="-G"
    ;;

  *-*-hpux*) 
    LDSO_FLAGS="+z -Ae +DA1.1"
    if test "$GCC" = yes ; then
      SO_FLAGS="-fPIC $SO_FLAGS"
    fi
    ;;

  *-*-bsdi*) 
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_FLAGS="-fPIC $SO_FLAGS"
    fi
    ;;

  *-*-freebsd*)
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_LD="$CC"
      SO_FLAGS="-fPIC $SO_FLAGS"
      CFLAGS="-fPIC $CFLAGS"
      LDSO_FLAGS="-shared"
    fi
    ;;

  *-*-openbsd*) 
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_LD="$CC"
      SO_FLAGS="-fPIC $SO_FLAGS"
      CFLAGS="-ftrampolines $CFLAGS"
      LDSO_FLAGS="-shared"
    fi
    ;;

  *-*-netbsd*) 
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_LD="$CC"
      SO_FLAGS="-fPIC $SO_FLAGS"
      LDSO_FLAGS="-shared"
    fi
    ;;

  *-*-mingw*)
    LIBS="$LIBS -lwinmm -lwsock32"
    LDFLAGS="$LDFLAGS -mwindows"
    SO_INSTALL=":"
    SO_LD=":"
    ;;

  *-apple-*)
    SO_LD="$CC"	
    LDSO_FLAGS="-dynamic -bundle -undefined suppress -flat_namespace"
    ;;
esac

AC_SUBST(LDSO_FLAGS)
AC_SUBST(SO_FLAGS)
AC_SUBST(SO_LD)


#--------------------------------------------------------------------------------
# webserver
#--------------------------------------------------------------------------------

WEBSERVER_LIBS=""
WEBSERVER_FILES=""

if test "$with_webserver" = yes ; then
  AC_DEFINE(ENABLE_WEBSERVER)

  WEBSERVER_FILES="s7webserver/s7webserver.o s7webserver/qhttpserver-master/lib/libqhttpserver.a"
  WEBSERVER_LIBS="`pkg-config --libs QtNetwork` -lstdc++"
  
  RANDOM_FEATURES="$RANDOM_FEATURES webserver"
fi

AC_SUBST(WEBSERVER_LIBS)
AC_SUBST(WEBSERVER_FILES)



#--------------------------------------------------------------------------------
# export-dynamic
#--------------------------------------------------------------------------------

CFLAGS="-O2 -I. $CFLAGS"
ORIGINAL_LDFLAGS="$LDFLAGS"

case "$host" in
  *-*-linux* | *-*-bsdi* | *-*-freebsd* | *-*-openbsd* | *-*-netbsd*) 
    if test "$CC" = "gcc" || test "$CC" = "g++" || test "$CC" = "cc" ; then
      LDFLAGS="$LDFLAGS -Wl,-export-dynamic"
    fi
esac



#--------------------------------------------------------------------------------
# disable-deprecated
#--------------------------------------------------------------------------------

if test "$enable_deprecated" = no ; then
  CFLAGS="-DGTK_DISABLE_DEPRECATED -DG_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED -DPANGO_DISABLE_DEPRECATED -DCAIRO_DISABLE_DEPRECATED -DGSL_DISABLE_DEPRECATED -Wall $CFLAGS"
  AC_DEFINE(DISABLE_DEPRECATED)
fi


#--------------------------------------------------------------------------------
# pure-s7
#--------------------------------------------------------------------------------

if test "$with_pure_s7" = yes ; then
  CFLAGS="-DWITH_PURE_S7 $CFLAGS"
fi

#--------------------------------------------------------------------------------
# s7-history
#--------------------------------------------------------------------------------

if test "$with_s7_history" = yes ; then
  CFLAGS="-DWITH_HISTORY $CFLAGS"
fi

#--------------------------------------------------------------------------------
# s7-profiling
#--------------------------------------------------------------------------------

if test "$with_s7_profiling" = yes ; then
  CFLAGS="-DWITH_S7_PROFILE $CFLAGS"
fi



#--------------------------------------------------------------------------------
# output
#--------------------------------------------------------------------------------

AC_SUBST(INSTALL)
AC_SUBST(CFLAGS)
AC_SUBST(ORIGINAL_LDFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(MAKE_TARGET)
AC_OUTPUT

AC_MSG_RESULT([
  Options selected
  -------------------------
  Snd version ...........: $VERSION
  CFLAGS ................: $CFLAGS
  LDFLAGS ...............:$LDFLAGS
  LIBS...................: $LIBS
  prefix.................: ${prefix}
  extension language.....: $LOCAL_LANGUAGE
  audio system...........: $AUDIO_SYSTEM
  graphics toolkit.......: $GRAPHICS_TOOLKIT
  optional libraries.....:$OPTIONAL_LIBRARIES
  random features........:$RANDOM_FEATURES
  environs...............: $host $CC
])

