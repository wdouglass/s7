#if (defined(__GNUC__)) && (!(defined(__cplusplus)))
  #define _GNU_SOURCE
  /* this is needed to get the vasprintf declaration */
#endif

#include "snd.h"


int snd_round(double x) /* needs to be double here (not mus_float_t) for x axis calcs */
{
  int i;
  i = (int)x;
  if ((x - i) > 0.5) return(i + 1);
  return(i);
}


mus_long_t snd_round_mus_long_t(double x)
{
  mus_long_t i;
  i = (mus_long_t)x;
  if ((x - i) > 0.5) return(i + 1);
  return(i);
}


mus_long_t snd_abs_mus_long_t(mus_long_t val)
{
  /* div is also limited to int */
  return((val < 0) ? -val : val);
}


#define POW2_SIZE 31
static int ipow2s[POW2_SIZE] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 
				512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 
				131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 
				33554432, 67108864, 134217728, 268435456, 536870912, 1073741824};
int snd_int_pow2(int n)
{
  return(ipow2s[n]);
}


mus_long_t snd_mus_long_t_pow2(int n)
{
  if ((n < 0) || (n > 46)) return(0);
  if (n < POW2_SIZE)
    return((mus_long_t)ipow2s[n]);
  return((mus_long_t)ipow2s[16] * (mus_long_t)ipow2s[n - 16]);  /* can't store an array as above -- compiler complaints */
}


int snd_to_int_pow2(int n)
{
  /* round up to next power of 2 */
  int i;
  for (i = 0; i < POW2_SIZE; i++)
    if (ipow2s[i] >= n)
      return(ipow2s[i]);
  return(0);
}


int snd_int_log2(int n)
{
  /* round down */
  int i;
  for (i = 1; i < POW2_SIZE; i++)
    if (ipow2s[i] > n)
      return(i - 1);
  return(0);
}


#define MAX_FLOAT_DIFF_FOR_EQUAL 0.0000001

bool snd_feq(mus_float_t val1, mus_float_t val2)
{
  /* if some float can be affected by a widget, we can easily get float inaccuracies that confuse "==" (in gtk in particular) */
  if (val1 == val2) return(true);
  if (fabs(val1 - val2) < MAX_FLOAT_DIFF_FOR_EQUAL) return(true);
  return(false);
}


#if !(defined(__GNUC__) && (!(defined(__cplusplus))))
mus_float_t in_dB(mus_float_t min_dB, mus_float_t lin_dB, mus_float_t val)
{
  return((val <= lin_dB) ? min_dB : (20.0 * log10(val)));
}
#endif


#define TIME_STR_SIZE 64
static char time_buf[TIME_STR_SIZE];

char *snd_local_time(void)
{
  time_t ts;
  time(&ts);
  strftime(time_buf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
  return(time_buf);
}


char *snd_strftime(const char *format, time_t date)
{
  strftime(time_buf, TIME_STR_SIZE, format, localtime(&date));
  return(time_buf);
}


char *snd_io_strerror(void) /* "snd_strerror" is exported by ALSA! */
{
  if (ss->local_errno != 0)
    return(strerror(ss->local_errno));
  if (errno != 0)
    return(strerror(errno));
  return(NULL);
}


char *snd_open_strerror(void)
{
  if (ss->local_open_errno != 0)
    return(strerror(ss->local_open_errno));
  return(snd_io_strerror());
}


char *string_to_colon(char *val)
{
  char *up_to_colon;
  int i, len;
  up_to_colon = (char *)calloc(strlen(val) + 1, sizeof(char));
  len = strlen(val);
  for (i = 0; i < len; i++)
    {
      if ((val[i] == ':') || (val[i] == ' '))
	{
	  up_to_colon[i] = 0;
	  return(up_to_colon);
	}
      up_to_colon[i] = val[i];
    }
  return(up_to_colon);
}


char *filename_without_directory(const char *name)
{
  /* since I don't want to mess with freeing these guys, I'll just return a pointer into the name */
  int i, len, last_slash;
  last_slash = 0;
  len = strlen(name);
  for (i = 0; i < len - 1; i++) 
    if (name[i] == '/') 
      last_slash = i + 1;
  return((char *)(name + last_slash));
}


char *just_filename(char *name)
{
  char *nodir;
  int i, len;
  nodir = mus_strdup(filename_without_directory(name));
  len = strlen(nodir);
  for (i = 0; i < len; i++) 
    if (nodir[i] == '.') 
      {
	nodir[i] = '\0'; 
	break;
      }
  return(nodir);
}


char *just_directory(const char *name)
{
  int i, len, last_slash = 0;
  char *dirname = NULL;
  len = strlen(name);
  dirname = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < len - 1; i++) 
    if (name[i] == '/') 
      last_slash = i + 1;
  if (last_slash > 0)
    strncpy(dirname, name, last_slash);
  return(dirname);
}


char *file_to_string(const char *filename)
{ 
  FILE *file;
  long size;
  char *content = NULL;
  file = fopen(filename, "r");
  if (!file) return(NULL);
  fseek(file, 0, SEEK_END);
  size = ftell(file);
  if (size > 0)
    {
      size_t bytes;
      rewind(file);
      content = (char *)calloc(size + 1, sizeof(char));
      bytes = fread(content, sizeof(char), size, file);
      if (bytes == 0)
	fprintf(stderr, "file->string read error");
    }
  fclose(file);
  return(content);
}


char *vstr(const char *format, va_list ap)
{
  char *buf;
#ifndef _MSC_VER
  if (vasprintf(&buf, format, ap) == -1)
    return(NULL);
#else
  int len;
  len = mus_strlen(format) + PRINT_BUFFER_SIZE;
  buf = (char *)calloc(len, sizeof(char));
  vsnprintf(buf, len, format, ap);
#endif
  return(buf);
}


disk_space_t disk_has_space(mus_long_t bytes, const char *filename)
{
  mus_long_t kfree, kneeded;
  kfree = disk_kspace(filename);
  if (kfree < 0) 
    {
      snd_error("can't access %s: %s", filename, snd_io_strerror()); 
      return(NO_DISK_SPACE);
    }
  kneeded = bytes >> 10;
  if (kfree < kneeded)
    {
      snd_error("not enough space left on disk: only %" PRId64 " kbytes available", kfree);
      return(NOT_ENOUGH_DISK_SPACE);
    }
  return(DISK_SPACE_OK);
}


char *prettyf(double num, int tens)
{ 
  /* try to prettify float display -- if tens <= 0, return int */
  static char prtbuf[256];

  if (tens <= 0)
    snprintf(prtbuf, 256, "%d", (int)snd_round(num));
  else 
    {
      int i, len;
      snprintf(prtbuf, 256, "%.*f", tens, num); /* %f assumes double arg */
      /* look for trailing 0's beyond the ddd.0 case */
      len = strlen(prtbuf);
      for (i = len - 1; (i > 0) && (prtbuf[i] == '0') && (prtbuf[i - 1] != '.'); i--)
	prtbuf[i] = '\0';
    }
  return(mus_strdup(prtbuf));
}


static char *get_tmpdir(void)
{
  char *tmpdir = NULL;
  int len;
  tmpdir = mus_strdup(getenv("TMPDIR"));
  if ((!tmpdir) && (MUS_DEFAULT_TEMP_DIR)) tmpdir = mus_strdup(MUS_DEFAULT_TEMP_DIR);
#ifdef P_tmpdir
  if (!tmpdir) tmpdir = mus_strdup(P_tmpdir); /* /usr/include/stdio.h */
#else
  if (!tmpdir) return(mus_strdup("/tmp"));
#endif
  if (!tmpdir) return(mus_strdup("."));
  len = strlen(tmpdir);
  if (tmpdir[len - 1] == '/') tmpdir[len - 1] = 0; /* this is what forces us to copy the string above (Sun segfaults otherwise) */
  return(tmpdir);
}


static int sect_ctr = 0;

char *shorter_tempnam(const char *udir, const char *prefix)
{
  /* tempnam turns out names that are inconveniently long (in this case the filename is user-visible) */
  char *str, *tmpdir = NULL;
  str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  if ((!udir) || (mus_strlen(udir) == 0)) 
    tmpdir = get_tmpdir(); /* incoming dir could be "" */
  else tmpdir = mus_strdup(udir);
  snprintf(str, PRINT_BUFFER_SIZE, "%s%s%s%d_%d.snd", 
	   tmpdir, 
	   (tmpdir[strlen(tmpdir) - 1] == '/') ? "" : "/",
	   (prefix) ? prefix : "snd_", 
	   (int)getpid(), 
	   sect_ctr++);
  if (tmpdir) free(tmpdir);
  tmpdir = mus_strdup(str);
  free(str);
  return(tmpdir);
}


char *snd_tempnam(void)
{
  char *udir;
  udir = temp_dir(ss);
  if ((udir) && (*udir))
    return(shorter_tempnam(udir, "snd_"));
  return(shorter_tempnam(NULL, "snd_"));
}


#if MUS_PORTAUDIO
#include <portaudio.h>
#endif

void snd_exit(int val)
{
#if MUS_PORTAUDIO
  Pa_Terminate();
#endif
  exit(val);
}


/* currently this is only used by the test suite (snd-test.scm)
 */
#if HAVE_SCHEME
#define S_file_to_string "file->string"

static Xen g_file_to_string(Xen name)
{ 
  char *contents;
  Xen val = Xen_false;
  Xen_check_type(Xen_is_string(name), name, 1, S_file_to_string, "a string");
  contents = file_to_string(Xen_string_to_C_string(name));
  val = C_string_to_Xen_string(contents);
  free(contents);
  return(val);
}
#endif


#if HAVE_SCHEME
  Xen_wrap_1_arg(g_file_to_string_w, g_file_to_string)
#endif

void g_init_utils(void)
{
#if HAVE_SCHEME
  Xen_define_typed_procedure(S_file_to_string, g_file_to_string_w, 1, 0, 0, "file contents as string", 
			     s7_make_circular_signature(s7, 0, 1, s7_make_symbol(s7, "string?")));
#endif
}
