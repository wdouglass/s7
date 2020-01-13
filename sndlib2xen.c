/* Tie sndlib into Xen */

#include "mus-config.h"

#if USE_SND
  #include "snd.h"
#else
  #if HAVE_RUBY
    #define PROC_FALSE "false"
    #define PROC_TRUE "true"
  #endif
  #if HAVE_SCHEME || HAVE_FORTH
    #define PROC_FALSE "#f"
    #define PROC_TRUE "#t"
  #endif
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>


#ifdef _MSC_VER
  #pragma warning(disable: 4244)
#endif

#include "_sndlib.h"
#include "sndlib-strings.h"
#include "vct.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "clm2xen.h"

#ifndef S_set
  #if HAVE_RUBY
    #define S_set "set_"
  #endif
  #if HAVE_SCHEME
    #define S_set "set! "
  #endif
  #if HAVE_FORTH
    #define S_set "set-"
  #endif
#endif



/* originally I tried to simplify C GC by using global static strings that were 
 *   freed whenever the associated function was called again, on the assumption
 *   that the preceding value was now unused.  In a multithread context, that
 *   assumption is false, so I didn't use code like this:
 *
 *  static char *tmpstr = NULL;
 *
 *  static char *local_mus_expand_filename(char *name)
 *  {
 *    if (tmpstr) {free(tmpstr); tmpstr = NULL;}
 *       tmpstr = mus_expand_filename(name);
 *    return(tmpstr);
 *  }
 */

static Xen g_mus_sound_loop_info(Xen gfilename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename): synth loop info for sound as a list: (start1 \
end1 start2 end2 base-note base-detune mode1 mode2)"
  int *res;
  Xen sres = Xen_empty_list;
  char *str = NULL;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_loop_info, "a string"); 

  res = mus_sound_loop_info(str = mus_expand_filename(Xen_string_to_C_string(gfilename)));
  if (str) free(str);
  if (res)
    {
      sres = Xen_list_8(C_int_to_Xen_integer(res[0]), C_int_to_Xen_integer(res[1]), C_int_to_Xen_integer(res[2]),
			C_int_to_Xen_integer(res[3]), C_int_to_Xen_integer(res[4]), C_int_to_Xen_integer(res[5]),
			C_int_to_Xen_integer(res[6]), C_int_to_Xen_integer(res[7]));
      free(res);
    }
  return(sres);
}


static Xen g_mus_sound_mark_info(Xen gfilename)
{
  #define H_mus_sound_mark_info "(" S_mus_sound_mark_info " filename): aifc header mark info as a list of lists: ((id pos)...)"
  int *mark_ids, *mark_positions;
  int marks = 0;
  Xen sres = Xen_empty_list;
  char *str = NULL;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_mark_info, "a string"); 

  marks = mus_sound_mark_info(str = mus_expand_filename(Xen_string_to_C_string(gfilename)), &mark_ids, &mark_positions);
  if (str) free(str);
  if (marks > 0)
    {
      int i;
      for (i = 0; i < marks; i++)
	sres = Xen_cons(Xen_list_2(C_int_to_Xen_integer(mark_ids[i]),
				   C_int_to_Xen_integer(mark_positions[i])),
			sres);
    }
  return(sres);
}


static Xen gmus_sound(const char *caller, int (*func)(const char *file), Xen gfilename)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_int_to_Xen_integer((*func)(str));
  if (str) free(str);
  return(result);
}


static Xen gmus_sound_set(const char *caller, int (*func)(const char *file, int newval), Xen gfilename, Xen val)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  Xen_check_type(Xen_is_integer(val), val, 2, caller, "an integer");
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_int_to_Xen_integer((*func)(str, Xen_integer_to_C_int(val)));
  if (str) free(str);
  return(result);
}


static Xen glmus_sound(const char *caller, mus_long_t (*func)(const char *file), Xen gfilename)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_llong_to_Xen_llong((*func)(str));
  if (str) free(str);
  return(result);
}


static Xen glmus_sound_set(const char *caller, int (*func)(const char *file, mus_long_t newval), Xen gfilename, Xen val)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  Xen_check_type(Xen_is_number(val), val, 2, caller, "a number");
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_llong_to_Xen_llong((*func)(str, Xen_llong_to_C_llong(val)));
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_samples(Xen filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename): samples (number-of-framples * number-of-channels) in sound file"
  return(glmus_sound(S_mus_sound_samples, mus_sound_samples, filename));
}


static Xen g_mus_sound_set_samples(Xen filename, Xen val) 
{
  return(glmus_sound_set(S_set S_mus_sound_samples, mus_sound_set_samples, filename, val));
}


Xen g_mus_sound_framples(Xen filename) 
{
  #define H_mus_sound_framples "(" S_mus_sound_framples " filename): framples (number-of-samples / number-of-channels) in sound file"
  return(glmus_sound(S_mus_sound_framples, mus_sound_framples, filename));
}


static Xen g_mus_sound_datum_size(Xen filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename): bytes per sample used by the data in sound file (sample type dependent)"
  return(gmus_sound(S_mus_sound_datum_size, mus_sound_datum_size, filename));
}


static Xen g_mus_sound_data_location(Xen filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename): location (in bytes) of first sample of sound data"
  return(glmus_sound(S_mus_sound_data_location, mus_sound_data_location, filename));
}


static Xen g_mus_sound_set_data_location(Xen filename, Xen val) 
{
  return(glmus_sound_set(S_set S_mus_sound_data_location, mus_sound_set_data_location, filename, val));
}


Xen g_mus_sound_chans(Xen filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename): channels of data in sound file"
  return(gmus_sound(S_mus_sound_chans, mus_sound_chans, filename));
}


static Xen g_mus_sound_set_chans(Xen filename, Xen val) 
{
  return(gmus_sound_set(S_set S_mus_sound_chans, mus_sound_set_chans, filename, val));
}


Xen g_mus_sound_srate(Xen filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename): sampling rate of sound file"
  return(gmus_sound(S_mus_sound_srate, mus_sound_srate, filename));
}


static Xen g_mus_sound_set_srate(Xen filename, Xen val) 
{
  return(gmus_sound_set(S_set S_mus_sound_srate, mus_sound_set_srate, filename, val));
}


static Xen g_mus_sound_header_type(Xen filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename): header type (e.g. " S_mus_aifc ") of sound file"

  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_mus_sound_header_type, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(filename));
  result = C_int_to_Xen_integer((int)mus_sound_header_type(str));
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_set_header_type(Xen filename, Xen val) 
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_set S_mus_sound_header_type, "a string"); 
  Xen_check_type(Xen_is_integer(val), val, 2, S_set S_mus_sound_header_type, "an integer");
  str = mus_expand_filename(Xen_string_to_C_string(filename));
  result = C_int_to_Xen_integer((int)mus_sound_set_header_type(str, (mus_header_t)Xen_integer_to_C_int(val)));
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_sample_type(Xen filename) 
{
  #define H_mus_sound_sample_type "(" S_mus_sound_sample_type " filename): sample type (e.g. " S_mus_bshort ") of data in sound file"
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_mus_sound_sample_type, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(filename));
  result = C_int_to_Xen_integer((int)mus_sound_sample_type(str));
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_set_sample_type(Xen filename, Xen val) 
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_set S_mus_sound_sample_type, "a string"); 
  Xen_check_type(Xen_is_integer(val), val, 2, S_set S_mus_sound_sample_type, "an integer");
  str = mus_expand_filename(Xen_string_to_C_string(filename));
  result = C_int_to_Xen_integer((int)mus_sound_set_sample_type(str, (mus_sample_t)Xen_integer_to_C_int(val)));
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_length(Xen filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename): sound file length in bytes"
  return(glmus_sound(S_mus_sound_length, mus_sound_length, filename));
}


static Xen g_mus_sound_type_specifier(Xen filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename): original sound file header type identifier (e.g. 0x2e736e64)"
  return(gmus_sound(S_mus_sound_type_specifier, mus_sound_type_specifier, filename));
}


static Xen g_mus_sound_forget(Xen filename) 
{
  #define H_mus_sound_forget "(" S_mus_sound_forget " filename): remove 'filename' from sound cache.  If you create, then later \
delete a sound file, " S_mus_sound_forget " can be used to clear it from sndlib's cache of sound files"
  return(gmus_sound(S_mus_sound_forget, mus_sound_forget, filename));
}


static Xen g_mus_sound_prune(void) 
{
  #define H_mus_sound_prune "(" S_mus_sound_prune "): remove all defunct entries from sndlib's sound file cache."
  return(C_int_to_Xen_integer(mus_sound_prune()));
}


static Xen g_mus_sound_comment(Xen gfilename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename): comment (a string) found in sound file's header"
  char *res = NULL, *str = NULL; 
  Xen newstr;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_comment, "a string"); 

  res = mus_sound_comment(str = mus_expand_filename(Xen_string_to_C_string(gfilename)));
  if (str) free(str);
  newstr = C_string_to_Xen_string(res);
  if (res) free(res);
  return(newstr);
}


static Xen g_mus_sound_write_date(Xen filename) 
{
  char *str = NULL;
  Xen result;

  #define H_mus_sound_write_date "(" S_mus_sound_write_date " filename): write date of sound file"
  Xen_check_type(Xen_is_string(filename), filename, 1, S_mus_sound_write_date, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(filename));
  result = C_ulong_to_Xen_ulong((unsigned long)mus_sound_write_date(str)); /* actually time_t */
  if (str) free(str);
  return(result);
}

static Xen g_mus_header_writable(Xen head, Xen data)
{
  #define H_mus_header_writable "(" S_mus_header_writable " header-type sample-type) returns " PROC_TRUE " if the header can handle the sample type"
  Xen_check_type(Xen_is_integer(head), head, 1, S_mus_header_writable, "a header type");
  Xen_check_type(Xen_is_integer(data), data, 2, S_mus_header_writable, "a sample type");
  return(C_bool_to_Xen_boolean(mus_header_writable((mus_header_t)Xen_integer_to_C_int(head), (mus_sample_t)Xen_integer_to_C_int(data))));
}

static Xen g_mus_header_raw_defaults(void)
{
  #define H_mus_header_raw_defaults "(" S_mus_header_raw_defaults "): returns list '(srate chans sample-type) of current raw sound default attributes"
  int srate, chans;
  mus_sample_t sample_type;
  mus_header_raw_defaults(&srate, &chans, &sample_type);
  return(Xen_list_3(C_int_to_Xen_integer(srate),
		    C_int_to_Xen_integer(chans),
		    C_int_to_Xen_integer((int)sample_type)));
}


static Xen g_mus_header_set_raw_defaults(Xen lst)
{
  Xen_check_type((Xen_is_list(lst)) && (Xen_list_length(lst) == 3), lst, 1, S_mus_header_raw_defaults, "a list: '(srate chans sample-type)");
  Xen_check_type(Xen_is_integer(Xen_car(lst)), Xen_car(lst), 1, S_mus_header_raw_defaults, "an integer = srate");
  Xen_check_type(Xen_is_integer(Xen_cadr(lst)), Xen_cadr(lst), 2, S_mus_header_raw_defaults, "an integer = chans");
  Xen_check_type(Xen_is_integer(Xen_caddr(lst)), Xen_caddr(lst), 3, S_mus_header_raw_defaults, "an integer = sample-type");
  mus_header_set_raw_defaults(Xen_integer_to_C_int(Xen_car(lst)),
			      Xen_integer_to_C_int(Xen_cadr(lst)),
			      (mus_sample_t)Xen_integer_to_C_int(Xen_caddr(lst)));
  return(lst);
}


static Xen g_mus_header_type_name(Xen type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type): header type (e.g. " S_mus_aiff ") as a string"
  Xen_check_type(Xen_is_integer(type), type, 1, S_mus_header_type_name, "an integer (header-type id)"); 
  return(C_string_to_Xen_string(mus_header_type_name((mus_header_t)Xen_integer_to_C_int(type))));
}


static Xen g_mus_header_type_to_string(Xen type) 
{
  #define H_mus_header_type_to_string "(" S_mus_header_type_to_string " type): header type (e.g. " S_mus_aiff ") as a string"
  Xen_check_type(Xen_is_integer(type), type, 1, S_mus_header_type_to_string, "an integer (header-type id)"); 
  return(C_string_to_Xen_string(mus_header_type_to_string((mus_header_t)Xen_integer_to_C_int(type))));
}


static Xen g_mus_sample_type_name(Xen samp_type) 
{
  #define H_mus_sample_type_name "(" S_mus_sample_type_name " samp_type): sample type (e.g. " S_mus_bshort ") as a string"
  Xen_check_type(Xen_is_integer(samp_type), samp_type, 1, S_mus_sample_type_name, "an integer (sample-type id)"); 
  return(C_string_to_Xen_string(mus_sample_type_name((mus_sample_t)Xen_integer_to_C_int(samp_type))));
}


static Xen g_mus_sample_type_to_string(Xen samp_type) 
{
  #define H_mus_sample_type_to_string "(" S_mus_sample_type_to_string " samp_type): sample type (e.g. " S_mus_bshort ") as a string"
  Xen_check_type(Xen_is_integer(samp_type), samp_type, 1, S_mus_sample_type_to_string, "an integer (sample-type id)"); 
  return(C_string_to_Xen_string(mus_sample_type_to_string((mus_sample_t)Xen_integer_to_C_int(samp_type))));
}


static Xen g_mus_bytes_per_sample(Xen samp_type) 
{
  #define H_mus_bytes_per_sample "(" S_mus_bytes_per_sample " sample-type): number of bytes per sample in \
sample-type (e.g. " S_mus_bshort " = 2)"
  Xen_check_type(Xen_is_integer(samp_type), samp_type, 1, S_mus_bytes_per_sample, "an integer (sample-type id)"); 
  return(C_int_to_Xen_integer(mus_bytes_per_sample((mus_sample_t)Xen_integer_to_C_int(samp_type))));
}


static Xen g_mus_sound_duration(Xen gfilename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename): duration (in seconds) of sound file"
  float res;
  char *str = NULL;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_duration, "a string"); 
  res = mus_sound_duration(str = mus_expand_filename(Xen_string_to_C_string(gfilename)));
  if (str) free(str);
  return(C_double_to_Xen_real(res));

}


static Xen g_mus_oss_set_buffers(Xen num, Xen size)
{
  #define H_mus_oss_set_buffers "(" S_mus_oss_set_buffers " num size): set Linux OSS 'fragment' number and size. \
If Snd's controls seem sluggish, try (" S_mus_oss_set_buffers " 4 12) or even (" S_mus_oss_set_buffers " 2 12). \
This reduces the on-card buffering, but may introduce clicks."

#if (HAVE_OSS || HAVE_ALSA)
  Xen_check_type(Xen_is_integer(num), num, 1, S_mus_oss_set_buffers, "an integer");
  Xen_check_type(Xen_is_integer(size), size, 2, S_mus_oss_set_buffers, "an integer");
  mus_oss_set_buffers(Xen_integer_to_C_int(num),
			    Xen_integer_to_C_int(size));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_buffers(void)
{
  #define H_mus_alsa_buffers "(" S_mus_alsa_buffers "): current number of ALSA periods."
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_buffers()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_buffers(Xen val)
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mus_alsa_buffers, "an integer");
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_set_buffers(Xen_integer_to_C_int(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_buffer_size(void)
{
  #define H_mus_alsa_buffer_size "(" S_mus_alsa_buffer_size "): current size of ALSA buffers."
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_buffer_size()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_buffer_size(Xen val)
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mus_alsa_buffer_size, "an integer");
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_set_buffer_size(Xen_integer_to_C_int(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_device(void)
{
  #define H_mus_alsa_device "(" S_mus_alsa_device "): current ALSA device."
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_device()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_device(Xen val)
{
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_mus_alsa_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_set_device(Xen_string_to_C_string(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_playback_device(void)
{
  #define H_mus_alsa_playback_device "(" S_mus_alsa_playback_device "): current ALSA playback device."
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_playback_device()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_playback_device(Xen val)
{
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_mus_alsa_playback_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_set_playback_device(Xen_string_to_C_string(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_capture_device(void)
{
  #define H_mus_alsa_capture_device "(" S_mus_alsa_capture_device "): current ALSA capture device."
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_capture_device()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_capture_device(Xen val)
{
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_mus_alsa_capture_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_set_capture_device(Xen_string_to_C_string(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_squelch_warning(void)
{
  #define H_mus_alsa_squelch_warning "(" S_mus_alsa_squelch_warning "): whether to squelch ALSA srate mismatch warnings."
#if HAVE_ALSA
  return(C_bool_to_Xen_boolean(mus_alsa_squelch_warning()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_squelch_warning(Xen val)
{
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_mus_alsa_squelch_warning, "a boolean");
#if HAVE_ALSA
  return(C_bool_to_Xen_boolean(mus_alsa_set_squelch_warning(Xen_boolean_to_C_bool(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_sound_maxamp_exists(Xen file)
{
  #define H_mus_sound_maxamp_exists "(" S_mus_sound_maxamp_exists " filename): " PROC_TRUE " if sound's maxamp data is available \
in the sound cache; if it isn't, a call on " S_mus_sound_maxamp " has to open and read the data to get the maxamp."
  bool val;
  char *str = NULL;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_maxamp_exists, "a string");
  val = mus_sound_maxamp_exists(str = mus_expand_filename(Xen_string_to_C_string(file)));
  if (str) free(str);
  return(C_bool_to_Xen_boolean(val));
}


Xen g_mus_sound_maxamp(Xen file)
{
  #define H_mus_sound_maxamp "(" S_mus_sound_maxamp " filename): maxamps in sound (a list of paired amps (floats) and locations (samples))"
  int chans;
  char *filename;
  Xen res = Xen_empty_list;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_maxamp, "a string");

  filename = mus_expand_filename(Xen_string_to_C_string(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      mus_long_t rtn;
      mus_float_t *vals;
      mus_long_t *times;

      vals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

      rtn = mus_sound_maxamps(filename, chans, vals, times);
      if (rtn != MUS_ERROR)
	{
	  int i;
	  for (i = chans - 1; i >= 0; i--)
	    res = Xen_cons(C_llong_to_Xen_llong(times[i]), Xen_cons(C_double_to_Xen_real(vals[i]), res));
	}
      free(vals);
      free(times);
      if (filename) free(filename);
    }
  else 
    {
      if (filename) free(filename);
      Xen_error(BAD_HEADER,
		Xen_list_1(C_string_to_Xen_string(S_mus_sound_maxamp ": chans <= 0")));
    }
  return(res);
}


static Xen g_mus_sound_set_maxamp(Xen file, Xen vals)
{
  int chans;
  char *filename;

  Xen_check_type(Xen_is_string(file), file, 1, S_set S_mus_sound_maxamp, "a string");
  Xen_check_type(Xen_is_list(vals), vals, 2, S_set S_mus_sound_maxamp, "a list");

  filename = mus_expand_filename(Xen_string_to_C_string(file));
  chans = mus_sound_chans(filename);

  /* presumably any actual error here will be trapped via mus-error (raised in mus_header_read via read_sound_file_header),
   *   so chans <= 0 is actually in the header?
   */
  if (chans > 0)
    {
      Xen lst;
      int i, j, len;
      mus_float_t *mvals;
      mus_long_t *times;

      len = Xen_list_length(vals);
      if (len < (chans * 2))
	Xen_wrong_type_arg_error(S_set S_mus_sound_maxamp, 2, vals, "max amp list length must = 2 * chans");
      if (len > chans * 2) len = chans * 2;

      mvals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

      for (i = 0, j = 0, lst = Xen_copy_arg(vals); i < len; i += 2, j++, lst = Xen_cddr(lst))
	{
	  times[j] = Xen_llong_to_C_llong(Xen_car(lst));
	  mvals[j] = Xen_real_to_C_double(Xen_cadr(lst));
	}

      fprintf(stderr, "set in g_mus_sound_set_maxamp\n");
      mus_sound_set_maxamps(filename, chans, mvals, times);
      free(mvals);
      free(times);
      if (filename) free(filename);
    }
  else 
    {
      if (filename) free(filename);
      Xen_error(BAD_HEADER,
		Xen_list_1(C_string_to_Xen_string(S_set S_mus_sound_maxamp ": chans <= 0")));
    }
  return(vals);
}



#define S_mus_sound_preload "mus-sound-preload"
static Xen g_mus_sound_preload(Xen file)
{
  #define H_mus_sound_preload "(" S_mus_sound_preload " filename): save filename's data in memory (faster opens and so on)."
  char *str;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_preload, "a string");
  str = mus_expand_filename(Xen_string_to_C_string(file));

  if (str)
    {
      int ifd;
      ifd = mus_sound_open_input(str);
      if (ifd != MUS_ERROR)
	{
	  int i, chans;
	  mus_float_t **bufs;
	  mus_long_t framples;
	  chans = mus_sound_chans(str);
	  framples = mus_sound_framples(str) + 8; /* + 8 for readers that wander off the end */
	  bufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
	  for (i = 0; i < chans; i++)
	    bufs[i] = (mus_float_t *)malloc(framples * sizeof(mus_float_t));
      
	  mus_file_seek_frample(ifd, 0);
	  mus_file_read_file(ifd, 0, chans, framples, bufs);
	  mus_sound_set_saved_data(str, bufs);
	  mus_sound_close_input(ifd);
	}
      free(str);
    }
  return(file);
}


/* global default clipping values */

static Xen g_mus_clipping(void)
{
  #define H_mus_clipping "(" S_mus_clipping "): whether sound data written to a file should be clipped"
  return(C_bool_to_Xen_boolean(mus_clipping()));
}


static Xen g_mus_set_clipping(Xen clipped)
{
  Xen_check_type(Xen_is_boolean(clipped), clipped, 1, S_set S_mus_clipping, "a boolean");
  return(C_bool_to_Xen_boolean(mus_set_clipping(Xen_boolean_to_C_bool(clipped))));
}


/* file local clipping values */

static Xen g_mus_file_clipping(Xen fd)
{
  #define H_mus_file_clipping "(" S_mus_file_clipping " fd): whether sound data written to file 'fd' should be clipped"
  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_file_clipping, "an integer");
  return(C_bool_to_Xen_boolean(mus_file_clipping(Xen_integer_to_C_int(fd))));
}


static Xen g_mus_file_set_clipping(Xen fd, Xen clipped)
{
  Xen_check_type(Xen_is_integer(fd), fd, 1, S_set S_mus_file_clipping, "an integer");
  Xen_check_type(Xen_is_boolean(clipped), clipped, 2, S_set S_mus_file_clipping, "a boolean");
  return(C_bool_to_Xen_boolean(mus_file_set_clipping(Xen_integer_to_C_int(fd), Xen_boolean_to_C_bool(clipped))));
}



Xen g_mus_expand_filename(Xen file)
{
  #define H_mus_expand_filename "(" S_mus_expand_filename " name): expand 'name' into a canonical or absolute filename, that is, \
one in which all directories in the path are explicit."
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_expand_filename, "a string");
  str = mus_expand_filename(Xen_string_to_C_string(file));
  result = C_string_to_Xen_string(str);
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_report_cache(Xen file)
{
  #define H_mus_sound_report_cache "(" S_mus_sound_report_cache " (name)): print the current sound \
cache info to the file given or stdout"
  const char *name;

  if (!Xen_is_bound(file))
    {
      mus_sound_report_cache(stdout);
      return(Xen_false);
    }

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_report_cache, "a string");
  name = Xen_string_to_C_string(file);
  if (name)
    {
      char *str = NULL;
      str = mus_expand_filename(name);
      if (str)
	{
	  FILE *fd;
	  fd = FOPEN(str, "w");
	  free(str);
	  if (fd)
	    {
	      mus_sound_report_cache(fd);
	      FCLOSE(fd, name);
	      return(file);
	    }
	}
    }

  Xen_error(Xen_make_error_type("cannot-save"),
	    Xen_list_3(C_string_to_Xen_string(S_mus_sound_report_cache ": ~S ~A"),
		       file,
		       C_string_to_Xen_string(STRERROR(errno))));
  return(Xen_false);
}


static Xen g_mus_error_type_to_string(Xen err)
{
  #define H_mus_error_type_to_string "(" S_mus_error_type_to_string " err): string description of err (a sndlib error type)"
  Xen_check_type(Xen_is_integer(err), err, 1, S_mus_error_type_to_string, "an integer");
  return(C_string_to_Xen_string((char *)mus_error_type_to_string(Xen_integer_to_C_int(err))));
}


static Xen g_array_to_file(Xen filename, Xen data, Xen len, Xen srate, Xen channels)
{
  #define H_array_to_file "(" S_array_to_file " filename data len srate channels): write 'data', \
a " S_vct " of interleaved samples, to the sound file 'filename' set up to have the given \
srate and channels.  'len' samples are written."

  /* this exists for compatibility with the Common Lisp version of CLM. Ideally, we'd get rid
   *   of it and provide vct<->file and sound-data<->file instead so that the channels aren't
   *   handled through interleaving.  But that means extensive changes to the Lisp code...
   */

  mus_long_t olen, samps;
  vct *v;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_array_to_file, "a string");
  Xen_check_type(mus_is_vct(data), data, 2, S_array_to_file, "a " S_vct);
  Xen_check_type(Xen_is_llong(len), len, 3, S_array_to_file, "an integer");
  Xen_check_type(Xen_is_integer(srate), srate, 4, S_array_to_file, "an integer");
  Xen_check_type(Xen_is_integer(channels), channels, 5, S_array_to_file, "an integer");

  v = Xen_to_vct(data);
  samps = Xen_llong_to_C_llong(len);
  if (samps <= 0)
    Xen_out_of_range_error(S_array_to_file, 3, len, "samples <= 0?");
  if (samps > mus_vct_length(v))
    samps = mus_vct_length(v);

  olen = mus_float_array_to_file(Xen_string_to_C_string(filename),
				 mus_vct_data(v),
				 samps,
				 Xen_integer_to_C_int(srate),
				 Xen_integer_to_C_int(channels));
  return(C_llong_to_Xen_llong(olen));
}


static Xen g_file_to_array(Xen filename, Xen chan, Xen start, Xen samples, Xen data)
{
  #define H_file_to_array "(" S_file_to_array " filename chan start samples data): read the sound file \
'filename' placing samples from channel 'chan' into the " S_vct " 'data' starting in the file \
at frample 'start' and reading 'samples' samples altogether."

  int chn, chans;
  mus_long_t samps;
  vct *v;
  const char *name = NULL;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_file_to_array, "a string");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_file_to_array, "an integer");
  Xen_check_type(Xen_is_llong(start), start, 3, S_file_to_array, "an integer");
  Xen_check_type(Xen_is_llong(samples), samples, 4, S_file_to_array, "an integer");
  Xen_check_type((mus_is_vct(data)), data, 5, S_file_to_array, "a " S_vct);

  name = Xen_string_to_C_string(filename);
  if (!(mus_file_probe(name)))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_file_to_array ": ~S ~A"),
			 filename,
			 C_string_to_Xen_string(STRERROR(errno))));

  v = Xen_to_vct(data);

  samps = Xen_llong_to_C_llong(samples);
  if (samps <= 0) 
    Xen_out_of_range_error(S_file_to_array, 4, samples, "samples <= 0?");
  if (samps > mus_vct_length(v))
    samps = mus_vct_length(v);

  chn = Xen_integer_to_C_int(chan);
  chans = mus_sound_chans(name);
  if ((chn < 0) || (chn > chans))
    Xen_error(NO_SUCH_CHANNEL,
	      Xen_list_4(C_string_to_Xen_string(S_file_to_array ": invalid chan: ~A, ~S has ~A chans"),
			 chan,
			 filename,
			 C_int_to_Xen_integer(chans)));

  if (chans <= 0)
    Xen_error(BAD_HEADER,
	      Xen_list_2(C_string_to_Xen_string(S_file_to_array ": ~S chans <= 0"),
			 filename));

  mus_file_to_float_array(name, chn, Xen_llong_to_C_llong(start), samps, mus_vct_data(v));
  return(data);
}


static Xen new_sound_hook;

static void g_new_sound_hook(const char *filename)
{
  if (Xen_hook_has_list(new_sound_hook))
    {
#if HAVE_SCHEME
      s7_call(s7, new_sound_hook, s7_cons(s7, C_string_to_Xen_string(filename), Xen_empty_list));
#else
      Xen procs, fname;
      fname = C_string_to_Xen_string(filename);
      procs = Xen_hook_list(new_sound_hook);
      while (!Xen_is_null(procs))
	{
	  Xen_call_with_1_arg(Xen_car(procs), fname, S_new_sound_hook);
	  procs = Xen_cdr(procs);
	}
#endif
    }
}


static Xen sound_path;
Xen g_mus_sound_path(void)
{
  #define H_mus_sound_path "(" S_mus_sound_path "): a list of directories to search for sound files."
  return(sound_path);
}

#if HAVE_SCHEME
  static bool have_sound_path = false;
  static s7_int sound_path_loc = 0;
  static s7_pointer mus_sound_path_symbol;
#endif

static Xen g_mus_set_sound_path(Xen val)
{
  Xen_check_type(Xen_is_list(val), val, 1, S_set S_mus_sound_path, "a list");
#if HAVE_SCHEME
  if (have_sound_path)
    s7_gc_unprotect_at(s7, sound_path_loc);
  sound_path = val;
  sound_path_loc = s7_gc_protect(s7, sound_path);
  have_sound_path = true;
  s7_symbol_set_value(s7, mus_sound_path_symbol, val);
#else
  if (sound_path != Xen_empty_list)
    Xen_GC_unprotect(sound_path);
  Xen_GC_protect(val);
  sound_path = val;
#endif
  return(val);
}


static Xen g_mus_max_malloc(void)
{
  #define H_mus_max_malloc "(" S_mus_max_malloc "): maximum number of bytes we will try to malloc."
  return(C_llong_to_Xen_llong(mus_max_malloc()));
}

#if HAVE_SCHEME
  static s7_pointer mus_max_malloc_symbol;
#endif

static Xen g_mus_set_max_malloc(Xen val)
{
  mus_long_t size;
  Xen_check_type(Xen_is_llong(val), val, 1, S_set S_mus_max_malloc, "an integer");
  size = Xen_llong_to_C_llong(val);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, mus_max_malloc_symbol, s7_make_integer(s7, size));
#endif
  return(C_llong_to_Xen_llong(mus_set_max_malloc(size)));
}



static Xen g_mus_max_table_size(void)
{
  #define H_mus_max_table_size "(" S_mus_max_table_size "): maximum table size."
  return(C_llong_to_Xen_llong(mus_max_table_size()));
}


#if HAVE_SCHEME
static s7_pointer mus_max_table_size_symbol;
#endif

static Xen g_mus_set_max_table_size(Xen val)
{
  mus_long_t size;
  Xen_check_type(Xen_is_llong(val), val, 1, S_set S_mus_max_table_size, "an integer");
  size = Xen_llong_to_C_llong(val);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, mus_max_table_size_symbol, s7_make_integer(s7, size));
#endif
  return(C_llong_to_Xen_llong(mus_set_max_table_size(size)));
}


#if __APPLE__
#define S_mus_audio_output_properties_mutable "mus-audio-output-properties-mutable"
static Xen g_mus_audio_output_properties_mutable(Xen mut)
{
  #define H_mus_audio_output_properties_mutable "(" S_mus_audio_output_properties_mutable " val): can DAC settings be changed to match the current sound"
  Xen_check_type(Xen_is_boolean(mut), mut, 1, S_mus_audio_output_properties_mutable, "a boolean");
  return(C_bool_to_Xen_boolean(mus_audio_output_properties_mutable(Xen_boolean_to_C_bool(mut))));
}
#endif


Xen_wrap_1_arg(g_mus_sound_samples_w, g_mus_sound_samples)
Xen_wrap_2_args(g_mus_sound_set_samples_w, g_mus_sound_set_samples)
Xen_wrap_1_arg(g_mus_sound_framples_w, g_mus_sound_framples)
Xen_wrap_1_arg(g_mus_sound_duration_w, g_mus_sound_duration)
Xen_wrap_1_arg(g_mus_sound_datum_size_w, g_mus_sound_datum_size)
Xen_wrap_1_arg(g_mus_sound_data_location_w, g_mus_sound_data_location)
Xen_wrap_2_args(g_mus_sound_set_data_location_w, g_mus_sound_set_data_location)
Xen_wrap_1_arg(g_mus_sound_chans_w, g_mus_sound_chans)
Xen_wrap_2_args(g_mus_sound_set_chans_w, g_mus_sound_set_chans)
Xen_wrap_1_arg(g_mus_sound_srate_w, g_mus_sound_srate)
Xen_wrap_2_args(g_mus_sound_set_srate_w, g_mus_sound_set_srate)
Xen_wrap_1_arg(g_mus_sound_header_type_w, g_mus_sound_header_type)
Xen_wrap_2_args(g_mus_sound_set_header_type_w, g_mus_sound_set_header_type)
Xen_wrap_1_arg(g_mus_sound_sample_type_w, g_mus_sound_sample_type)
Xen_wrap_2_args(g_mus_sound_set_sample_type_w, g_mus_sound_set_sample_type)
Xen_wrap_1_arg(g_mus_sound_length_w, g_mus_sound_length)
Xen_wrap_1_arg(g_mus_sound_type_specifier_w, g_mus_sound_type_specifier)
Xen_wrap_1_arg(g_mus_header_type_name_w, g_mus_header_type_name)
Xen_wrap_1_arg(g_mus_header_type_to_string_w, g_mus_header_type_to_string)
Xen_wrap_1_arg(g_mus_sample_type_name_w, g_mus_sample_type_name)
Xen_wrap_1_arg(g_mus_sample_type_to_string_w, g_mus_sample_type_to_string)
Xen_wrap_1_arg(g_mus_sound_comment_w, g_mus_sound_comment)
Xen_wrap_1_arg(g_mus_sound_write_date_w, g_mus_sound_write_date)
Xen_wrap_1_arg(g_mus_bytes_per_sample_w, g_mus_bytes_per_sample)
Xen_wrap_1_arg(g_mus_sound_loop_info_w, g_mus_sound_loop_info)
Xen_wrap_1_arg(g_mus_sound_mark_info_w, g_mus_sound_mark_info)
Xen_wrap_1_arg(g_mus_sound_maxamp_w, g_mus_sound_maxamp)
Xen_wrap_2_args(g_mus_sound_set_maxamp_w, g_mus_sound_set_maxamp)
Xen_wrap_1_arg(g_mus_sound_maxamp_exists_w, g_mus_sound_maxamp_exists)
Xen_wrap_1_arg(g_mus_sound_preload_w, g_mus_sound_preload)

Xen_wrap_no_args(g_mus_clipping_w, g_mus_clipping)
Xen_wrap_1_arg(g_mus_set_clipping_w, g_mus_set_clipping)
Xen_wrap_1_arg(g_mus_file_clipping_w, g_mus_file_clipping)
Xen_wrap_2_args(g_mus_file_set_clipping_w, g_mus_file_set_clipping)
Xen_wrap_no_args(g_mus_header_raw_defaults_w, g_mus_header_raw_defaults)
Xen_wrap_1_arg(g_mus_header_set_raw_defaults_w, g_mus_header_set_raw_defaults)
Xen_wrap_2_args(g_mus_header_writable_w, g_mus_header_writable)
Xen_wrap_1_arg(g_mus_expand_filename_w, g_mus_expand_filename)
Xen_wrap_1_optional_arg(g_mus_sound_report_cache_w, g_mus_sound_report_cache)
Xen_wrap_1_arg(g_mus_sound_forget_w, g_mus_sound_forget)
Xen_wrap_no_args(g_mus_sound_prune_w, g_mus_sound_prune)
Xen_wrap_1_arg(g_mus_error_type_to_string_w, g_mus_error_type_to_string)
Xen_wrap_2_args(g_mus_oss_set_buffers_w, g_mus_oss_set_buffers)
Xen_wrap_5_args(g_array_to_file_w, g_array_to_file)
Xen_wrap_5_args(g_file_to_array_w, g_file_to_array)
Xen_wrap_no_args(g_mus_alsa_buffers_w, g_mus_alsa_buffers)
Xen_wrap_1_arg(g_mus_alsa_set_buffers_w, g_mus_alsa_set_buffers)
Xen_wrap_no_args(g_mus_alsa_buffer_size_w, g_mus_alsa_buffer_size)
Xen_wrap_1_arg(g_mus_alsa_set_buffer_size_w, g_mus_alsa_set_buffer_size)
Xen_wrap_no_args(g_mus_alsa_device_w, g_mus_alsa_device)
Xen_wrap_1_arg(g_mus_alsa_set_device_w, g_mus_alsa_set_device)
Xen_wrap_no_args(g_mus_alsa_playback_device_w, g_mus_alsa_playback_device)
Xen_wrap_1_arg(g_mus_alsa_set_playback_device_w, g_mus_alsa_set_playback_device)
Xen_wrap_no_args(g_mus_alsa_capture_device_w, g_mus_alsa_capture_device)
Xen_wrap_1_arg(g_mus_alsa_set_capture_device_w, g_mus_alsa_set_capture_device)
Xen_wrap_no_args(g_mus_alsa_squelch_warning_w, g_mus_alsa_squelch_warning)
Xen_wrap_1_arg(g_mus_alsa_set_squelch_warning_w, g_mus_alsa_set_squelch_warning)

#if __APPLE__
Xen_wrap_1_arg(g_mus_audio_output_properties_mutable_w, g_mus_audio_output_properties_mutable)
#endif

Xen_wrap_no_args(g_mus_max_malloc_w, g_mus_max_malloc)
Xen_wrap_1_arg(g_mus_set_max_malloc_w, g_mus_set_max_malloc)
Xen_wrap_no_args(g_mus_max_table_size_w, g_mus_max_table_size)
Xen_wrap_1_arg(g_mus_set_max_table_size_w, g_mus_set_max_table_size)
Xen_wrap_no_args(g_mus_sound_path_w, g_mus_sound_path)
Xen_wrap_1_arg(g_mus_set_sound_path_w, g_mus_set_sound_path)

#if HAVE_SCHEME
  static s7_pointer acc_mus_max_table_size(s7_scheme *sc, s7_pointer args) {return(g_mus_set_max_table_size(s7_cadr(args)));}  
  static s7_pointer acc_mus_max_malloc(s7_scheme *sc, s7_pointer args) {return(g_mus_set_max_malloc(s7_cadr(args)));}  
  static s7_pointer acc_mus_sound_path(s7_scheme *sc, s7_pointer args) {return(g_mus_set_sound_path(s7_cadr(args)));}  
#endif


void mus_sndlib_xen_initialize(void)
{
#if HAVE_SCHEME
  s7_pointer pl_is, pl_isi, pl_si, pl_ss, pl_ps, pl_psp, pl_i, pl_bii, pl_p, pl_rs, pl_bi, pl_bib, pl_b, pl_ls;
  s7_pointer pl_l, pl_isfiii, pl_fsiiif, pl_bs, pl_ts, pl_sh, pl_bhi;
#endif

  mus_sound_initialize();
  sound_path = Xen_empty_list;

#if HAVE_RUBY
  Init_Hook();
#endif

  Xen_define_constant(S_mus_out_format,           MUS_OUT_SAMPLE_TYPE,      "sample type for fastest IO");
  Xen_define_constant(S_mus_unknown_header,       MUS_UNKNOWN_HEADER,       "unknown header type");
  Xen_define_constant(S_mus_next,                 MUS_NEXT,                 "NeXT (Sun) sound header id");
  Xen_define_constant(S_mus_aifc,                 MUS_AIFC,                 "AIFC sound header id");
  Xen_define_constant(S_mus_rf64,                 MUS_RF64,                 "RF64 sound header id");
  Xen_define_constant(S_mus_riff,                 MUS_RIFF,                 "RIFF (MS wave) sound header id");
  Xen_define_constant(S_mus_nist,                 MUS_NIST,                 "NIST (Sphere) sound header id");
  Xen_define_constant(S_mus_raw,                  MUS_RAW,                  "raw (headerless) sound header id");
  Xen_define_constant(S_mus_ircam,                MUS_IRCAM,                "IRCAM sound header id");
  Xen_define_constant(S_mus_aiff,                 MUS_AIFF,                 "AIFF (old-style) sound header id");
  Xen_define_constant(S_mus_bicsf,                MUS_BICSF,                "BICSF header id");
  Xen_define_constant(S_mus_voc,                  MUS_VOC,                  "VOC header id");
  Xen_define_constant(S_mus_svx,                  MUS_SVX,                  "SVX (IFF) header id");
  Xen_define_constant(S_mus_soundfont,            MUS_SOUNDFONT,            "soundfont header id");
  Xen_define_constant(S_mus_caff,                 MUS_CAFF,                 "Apple Core Audio File Format header id");

  Xen_define_constant(S_mus_unknown_sample,       MUS_UNKNOWN_SAMPLE,       "unknown sample type");
  Xen_define_constant(S_mus_bshort,               MUS_BSHORT,               "big-endian short sample type id");
  Xen_define_constant(S_mus_lshort,               MUS_LSHORT,               "little-endian short sample type id");
  Xen_define_constant(S_mus_mulaw,                MUS_MULAW,                "mulaw (8-bit) sample type id");
  Xen_define_constant(S_mus_alaw,                 MUS_ALAW,                 "alaw (8-bit) sample type id");
  Xen_define_constant(S_mus_byte,                 MUS_BYTE,                 "signed byte sample type id");
  Xen_define_constant(S_mus_ubyte,                MUS_UBYTE,                "unsigned byte sample type id");
  Xen_define_constant(S_mus_bfloat,               MUS_BFLOAT,               "big-endian float sample type id");
  Xen_define_constant(S_mus_lfloat,               MUS_LFLOAT,               "little-endian float sample type id");
  Xen_define_constant(S_mus_bint,                 MUS_BINT,                 "big-endian int sample type id");
  Xen_define_constant(S_mus_lint,                 MUS_LINT,                 "little-endian int sample type id");
  Xen_define_constant(S_mus_bintn,                MUS_BINTN,                "normalized big-endian int sample type id");
  Xen_define_constant(S_mus_lintn,                MUS_LINTN,                "normalized little-endian int sample type id");
  Xen_define_constant(S_mus_b24int,               MUS_B24INT,               "big-endian 24-bit sample type id");
  Xen_define_constant(S_mus_l24int,               MUS_L24INT,               "little-endian 24-bit sample type id");
  Xen_define_constant(S_mus_bdouble,              MUS_BDOUBLE,              "big-endian double sample type id");
  Xen_define_constant(S_mus_ldouble,              MUS_LDOUBLE,              "little-endian double sample type id");
  Xen_define_constant(S_mus_ubshort,              MUS_UBSHORT,              "unsigned big-endian short sample type id");
  Xen_define_constant(S_mus_ulshort,              MUS_ULSHORT,              "unsigned little-endian short sample type id");
  Xen_define_constant(S_mus_bdouble_unscaled,     MUS_BDOUBLE_UNSCALED,     "unscaled big-endian double sample type id");
  Xen_define_constant(S_mus_ldouble_unscaled,     MUS_LDOUBLE_UNSCALED,     "unscaled little-endian double sample type id");
  Xen_define_constant(S_mus_bfloat_unscaled,      MUS_BFLOAT_UNSCALED,      "unscaled big-endian float sample type id");
  Xen_define_constant(S_mus_lfloat_unscaled,      MUS_LFLOAT_UNSCALED,      "unscaled little-endian float sample type id");

#if HAVE_SCHEME
  s7_eval_c_string(s7, 
    "(define (mus_header_t? form argn) \
       (let ((h (list-ref form argn))) \
         (if (not (memq h '(mus-next mus-aifc mus-riff mus-nist mus-raw mus-ircam mus-aiff mus-bicsf mus-voc mus-svx mus-soundfont mus-rf64 mus-caff))) \
             (if (not (integer? h)) \
                 'integer? \
                 (if (not (< 0 h 71)) \
                     \"an integer between 1 and 70\")))))");
  {
    s7_pointer s, i, p, b, r, f, t, l, h;
    s = s7_make_symbol(s7, "string?");
    i = s7_make_symbol(s7, "integer?");
    p = s7_make_symbol(s7, "pair?");
    l = s7_make_symbol(s7, "list?");
    b = s7_make_symbol(s7, "boolean?");
    r = s7_make_symbol(s7, "real?");
    f = s7_make_symbol(s7, "float-vector?");
    h = s7_make_symbol(s7, "mus_header_t?");
    t = s7_t(s7);
    pl_is = s7_make_signature(s7, 2, i, s);
    pl_isi = s7_make_signature(s7, 3, i, s, i);
    pl_si = s7_make_signature(s7, 2, s, i);
    pl_sh = s7_make_signature(s7, 2, s, h);
    pl_ss = s7_make_signature(s7, 2, s, s);
    pl_ts = s7_make_signature(s7, 2, t, s);
    pl_ls = s7_make_signature(s7, 2, l, s);
    pl_ps = s7_make_signature(s7, 2, p, s);
    pl_psp = s7_make_signature(s7, 3, p, s, p);
    pl_i = s7_make_circular_signature(s7, 0, 1, i);
    pl_bii = s7_make_signature(s7, 3, b, i, i);
    pl_bhi = s7_make_signature(s7, 3, b, h, i);
    pl_p = s7_make_circular_signature(s7, 0, 1, p);
    pl_l = s7_make_circular_signature(s7, 0, 1, l);
    pl_rs = s7_make_signature(s7, 2, r, s);
    pl_bi = s7_make_signature(s7, 2, b, i);
    pl_bib = s7_make_signature(s7, 3, b, i, b);
    pl_b = s7_make_circular_signature(s7, 0, 1, b);
    pl_bs = s7_make_signature(s7, 2, b, s);
    pl_isfiii = s7_make_signature(s7, 6, i, s, f, i, i, i);
    pl_fsiiif = s7_make_signature(s7, 6, f, s, i, i, i, f);
  }
#endif

  Xen_define_typed_dilambda(S_mus_sound_samples, g_mus_sound_samples_w, H_mus_sound_samples,  
			    S_set S_mus_sound_samples, g_mus_sound_set_samples_w, 1, 0, 2, 0, pl_is, pl_isi);
  Xen_define_typed_dilambda(S_mus_sound_data_location, g_mus_sound_data_location_w, H_mus_sound_data_location,
			    S_set S_mus_sound_data_location, g_mus_sound_set_data_location_w, 1, 0, 2, 0, pl_is, pl_isi);
  Xen_define_typed_dilambda(S_mus_sound_chans, g_mus_sound_chans_w, H_mus_sound_chans, 
			    S_set S_mus_sound_chans, g_mus_sound_set_chans_w, 1, 0, 2, 0, pl_is, pl_isi);
  Xen_define_typed_dilambda(S_mus_sound_srate, g_mus_sound_srate_w, H_mus_sound_srate,
			    S_set S_mus_sound_srate, g_mus_sound_set_srate_w, 1, 0, 2, 0, pl_is, pl_isi);
  Xen_define_typed_dilambda(S_mus_sound_header_type, g_mus_sound_header_type_w, H_mus_sound_header_type,
			    S_set S_mus_sound_header_type, g_mus_sound_set_header_type_w, 1, 0, 2, 0, pl_is, pl_isi);
  Xen_define_typed_dilambda(S_mus_sound_sample_type, g_mus_sound_sample_type_w, H_mus_sound_sample_type,
			    S_set S_mus_sound_sample_type, g_mus_sound_set_sample_type_w, 1, 0, 2, 0, pl_is, pl_isi);

  Xen_define_typed_procedure(S_mus_sound_framples,       g_mus_sound_framples_w,         1, 0, 0, H_mus_sound_framples,	       pl_is);
  Xen_define_typed_procedure("mus-sound-frames",         g_mus_sound_framples_w,         1, 0, 0, H_mus_sound_framples,        pl_is);
  Xen_define_typed_procedure(S_mus_sound_duration,       g_mus_sound_duration_w,         1, 0, 0, H_mus_sound_duration,        pl_rs);
  Xen_define_typed_procedure(S_mus_sound_datum_size,     g_mus_sound_datum_size_w,       1, 0, 0, H_mus_sound_datum_size,      pl_is);
  Xen_define_typed_procedure(S_mus_sound_length,         g_mus_sound_length_w,           1, 0, 0, H_mus_sound_length,          pl_is);
  Xen_define_typed_procedure(S_mus_sound_type_specifier, g_mus_sound_type_specifier_w,   1, 0, 0, H_mus_sound_type_specifier,  pl_is);
  Xen_define_typed_procedure(S_mus_header_type_name,     g_mus_header_type_name_w,       1, 0, 0, H_mus_header_type_name,      pl_sh);
  Xen_define_typed_procedure(S_mus_header_type_to_string,g_mus_header_type_to_string_w,  1, 0, 0, H_mus_header_type_to_string, pl_sh);
  Xen_define_typed_procedure(S_mus_header_writable,      g_mus_header_writable_w,        2, 0, 0, H_mus_header_writable,       pl_bhi);
  Xen_define_typed_procedure(S_mus_sample_type_name,     g_mus_sample_type_name_w,       1, 0, 0, H_mus_sample_type_name,      pl_si);
  Xen_define_typed_procedure(S_mus_sample_type_to_string,g_mus_sample_type_to_string_w,  1, 0, 0, H_mus_sample_type_to_string, pl_si);
  Xen_define_typed_procedure(S_mus_sound_comment,        g_mus_sound_comment_w,          1, 0, 0, H_mus_sound_comment,         pl_ts);
  Xen_define_typed_procedure(S_mus_sound_write_date,     g_mus_sound_write_date_w,       1, 0, 0, H_mus_sound_write_date,      pl_is);
  Xen_define_typed_procedure(S_mus_bytes_per_sample,     g_mus_bytes_per_sample_w,       1, 0, 0, H_mus_bytes_per_sample,      pl_i);
  Xen_define_typed_procedure(S_mus_sound_loop_info,      g_mus_sound_loop_info_w,        1, 0, 0, H_mus_sound_loop_info,       pl_ls);
  Xen_define_typed_procedure(S_mus_sound_mark_info,      g_mus_sound_mark_info_w,        1, 0, 0, H_mus_sound_mark_info,       pl_ps);
  Xen_define_typed_procedure(S_mus_sound_maxamp_exists,  g_mus_sound_maxamp_exists_w,    1, 0, 0, H_mus_sound_maxamp_exists,   pl_bs);
  Xen_define_typed_procedure(S_mus_sound_forget,         g_mus_sound_forget_w,           1, 0, 0, H_mus_sound_forget,          pl_is);
  Xen_define_typed_procedure(S_mus_sound_prune,          g_mus_sound_prune_w,            0, 0, 0, H_mus_sound_prune,           pl_i);

  Xen_define_typed_procedure(S_mus_expand_filename,      g_mus_expand_filename_w,        1, 0, 0, H_mus_expand_filename,       pl_ss);
  Xen_define_typed_procedure(S_mus_sound_report_cache,   g_mus_sound_report_cache_w,     0, 1, 0, H_mus_sound_report_cache,    NULL);
  Xen_define_typed_procedure(S_mus_error_type_to_string, g_mus_error_type_to_string_w,   1, 0, 0, H_mus_error_type_to_string,  pl_si);
  Xen_define_typed_procedure(S_mus_oss_set_buffers,      g_mus_oss_set_buffers_w,        2, 0, 0, H_mus_oss_set_buffers,       pl_bii);
  Xen_define_typed_procedure(S_array_to_file,            g_array_to_file_w,              5, 0, 0, H_array_to_file,             pl_isfiii);
  Xen_define_typed_procedure(S_file_to_array,            g_file_to_array_w,              5, 0, 0, H_file_to_array,             pl_fsiiif);

  Xen_define_typed_procedure(S_mus_sound_preload,        g_mus_sound_preload_w,          1, 0, 0, H_mus_sound_preload,         pl_ss);

  Xen_define_typed_dilambda(S_mus_header_raw_defaults, g_mus_header_raw_defaults_w, H_mus_header_raw_defaults,
			    S_set S_mus_header_raw_defaults, g_mus_header_set_raw_defaults_w, 0, 0, 1, 0, pl_p, pl_p);

  Xen_define_typed_dilambda(S_mus_clipping, g_mus_clipping_w, H_mus_clipping, 
			    S_set S_mus_clipping, g_mus_set_clipping_w, 0, 0, 1, 0, pl_b, pl_b);
  Xen_define_typed_dilambda(S_mus_file_clipping, g_mus_file_clipping_w, H_mus_file_clipping, 
			    S_set S_mus_file_clipping, g_mus_file_set_clipping_w, 1, 0, 2, 0, pl_bi, pl_bib);
  Xen_define_typed_dilambda(S_mus_sound_maxamp, g_mus_sound_maxamp_w, H_mus_sound_maxamp, 
			    S_set S_mus_sound_maxamp, g_mus_sound_set_maxamp_w, 1, 0, 2, 0, pl_ps, pl_psp);

  /* these are no-ops if not ALSA, but that makes it easier to maintain global initialization files */
  Xen_define_typed_dilambda(S_mus_alsa_buffers, g_mus_alsa_buffers_w, H_mus_alsa_buffers, S_set 
			    S_mus_alsa_buffers, g_mus_alsa_set_buffers_w, 0, 0, 1, 0, NULL, NULL);
  Xen_define_typed_dilambda(S_mus_alsa_buffer_size, g_mus_alsa_buffer_size_w, H_mus_alsa_buffer_size, 
			    S_set S_mus_alsa_buffer_size, g_mus_alsa_set_buffer_size_w, 0, 0, 1, 0, NULL, NULL);
  Xen_define_typed_dilambda(S_mus_alsa_device, g_mus_alsa_device_w, H_mus_alsa_device, 
			    S_set S_mus_alsa_device, g_mus_alsa_set_device_w, 0, 0, 1, 0, NULL, NULL);
  Xen_define_typed_dilambda(S_mus_alsa_playback_device, g_mus_alsa_playback_device_w, H_mus_alsa_playback_device, 
			    S_set S_mus_alsa_playback_device, g_mus_alsa_set_playback_device_w, 0, 0, 1, 0, NULL, NULL);
  Xen_define_typed_dilambda(S_mus_alsa_capture_device, g_mus_alsa_capture_device_w, H_mus_alsa_capture_device, 
			    S_set S_mus_alsa_capture_device, g_mus_alsa_set_capture_device_w, 0, 0, 1, 0, NULL, NULL);
  Xen_define_typed_dilambda(S_mus_alsa_squelch_warning, g_mus_alsa_squelch_warning_w, H_mus_alsa_squelch_warning,
			    S_set S_mus_alsa_squelch_warning, g_mus_alsa_set_squelch_warning_w, 0, 0, 1, 0, NULL, NULL);

  Xen_define_typed_dilambda(S_mus_max_malloc, g_mus_max_malloc_w, H_mus_max_malloc, 
			    S_set S_mus_max_malloc, g_mus_set_max_malloc_w, 0, 0, 1, 0, pl_i, pl_i);
  Xen_define_typed_dilambda(S_mus_max_table_size, g_mus_max_table_size_w, H_mus_max_table_size, 
			    S_set S_mus_max_table_size, g_mus_set_max_table_size_w, 0, 0, 1, 0, pl_i, pl_i);
  Xen_define_typed_dilambda(S_mus_sound_path, g_mus_sound_path_w, H_mus_sound_path, 
			    S_set S_mus_sound_path, g_mus_set_sound_path_w, 0, 0, 1, 0, pl_l, pl_l);

#if HAVE_SCHEME
  mus_max_table_size_symbol = s7_define_variable(s7, "*" S_mus_max_table_size "*", s7_make_integer(s7, MUS_MAX_TABLE_SIZE_DEFAULT));
  s7_set_documentation(s7, mus_max_table_size_symbol, "*mus-max-table-size*: maximum table size.");
  s7_set_setter(s7, mus_max_table_size_symbol, s7_make_function(s7, "[acc-mus-max-table-size]" "]", acc_mus_max_table_size, 2, 0, false, "accessor"));

  mus_max_malloc_symbol = s7_define_variable(s7, "*" S_mus_max_malloc "*", s7_make_integer(s7, MUS_MAX_MALLOC_DEFAULT));
  s7_set_documentation(s7, mus_max_malloc_symbol, "*mus-max-malloc*: maximum number of bytes we will try to malloc.");
  s7_set_setter(s7, mus_max_malloc_symbol, s7_make_function(s7, "[acc-mus-max-malloc]" "]", acc_mus_max_malloc, 2, 0, false, "accessor"));

  mus_sound_path_symbol = s7_define_variable(s7, "*" S_mus_sound_path "*", s7_nil(s7));
  s7_set_documentation(s7, mus_sound_path_symbol, "*" S_mus_sound_path "* is a list of directories to search for sound files");
  s7_set_setter(s7, mus_sound_path_symbol, s7_make_function(s7, "[acc-mus-sound-path]" "]", acc_mus_sound_path, 2, 0, false, "accessor"));
#endif

#if __APPLE__
  Xen_define_typed_procedure(S_mus_audio_output_properties_mutable, g_mus_audio_output_properties_mutable_w, 1, 0, 0, H_mus_audio_output_properties_mutable, pl_b);
#endif

  #define H_new_sound_hook S_new_sound_hook "(name): called when a new sound file is being created"
  new_sound_hook = Xen_define_hook(S_new_sound_hook, "(make-hook 'name)", 1, H_new_sound_hook);
  mus_header_write_set_hook(g_new_sound_hook);

  Xen_provide_feature("sndlib");
}
