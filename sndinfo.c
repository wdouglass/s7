/* sndinfo describes sounds */

#include "mus-config.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef _MSC_VER
  #include <unistd.h>
#endif
#include <string.h>
#include <errno.h>
#include <time.h>

#include "sndlib.h"

static char *display_maxamps(const char *filename, int chans)
{
  char *ampstr;
  char fstr[16];
  int i, len;
  mus_float_t *vals;
  mus_long_t *times;

  len = chans * 32;
  ampstr = (char *)calloc(len, sizeof(char));
  vals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
  times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

  snprintf(ampstr, len, "\n  max amp%s: ", (chans > 1) ? "s" : "");
  mus_sound_maxamps(filename, chans, vals, times);
  for (i = 0; i < chans; i++)
    {
      snprintf(fstr, 16, "%.3f ", vals[i]);
      strcat(ampstr, fstr);
    }
  free(vals);
  free(times);
  return(ampstr);
}

int main(int argc, char *argv[])
{
  int chans, srate, ctr;
  mus_sample_t samp_type;
  mus_header_t type;
  mus_long_t samples;
  float length = 0.0;
  time_t date;
  int *loops = NULL;
  char *comment, *header_name;
  char *samp_type_info = NULL, *samp_type_name, *ampstr = NULL;
  char timestr[64];
  if (argc == 1) {printf("usage: sndinfo file\n"); exit(0);}
  mus_sound_initialize();
  for (ctr = 1; ctr < argc; ctr++)
    {
      if (mus_file_probe(argv[ctr])) /* see if it exists */
	{
	  date = mus_sound_write_date(argv[ctr]);
	  srate = mus_sound_srate(argv[ctr]);
	  if (srate == MUS_ERROR)
	    {
	      fprintf(stdout, "%s: not a sound file?\n", argv[ctr]);
	      continue;
	    }
	  chans = mus_sound_chans(argv[ctr]);
	  samples = mus_sound_samples(argv[ctr]);
	  comment = mus_sound_comment(argv[ctr]); 
	  if ((chans > 0) && (srate > 0))
	    length = (float)((double)samples / (double)(chans * srate));
	  loops = mus_sound_loop_info(argv[ctr]);
	  type = mus_sound_header_type(argv[ctr]);
	  header_name = (char *)mus_header_type_name(type);
	  samp_type = mus_sound_sample_type(argv[ctr]);
	  if (samp_type != MUS_UNKNOWN_SAMPLE)
	    samp_type_info = (char *)mus_sample_type_name(samp_type);
	  else
	    {
	      int orig_type;
	      if (!samp_type_info) samp_type_info = (char *)calloc(64, sizeof(char));
	      orig_type = mus_sound_original_sample_type(argv[ctr]);
	      samp_type_name = (char *)mus_header_original_sample_type_name(orig_type, type);
	      if (samp_type_name)
		snprintf(samp_type_info, 64, "%d (%s)", orig_type, samp_type_name);
	      else snprintf(samp_type_info, 64, "%d", orig_type);
	    }
	  fprintf(stdout, "%s:\n  srate: %d\n  chans: %d\n  length: %f",
		  argv[ctr], srate, chans, length);
	  if (length < 10.0)
	    {
	      int samps;
	      samps = mus_sound_framples(argv[ctr]);
	      fprintf(stdout, " (%d sample%s)", samps, (samps != 1) ? "s" : "");
	    }
	  fprintf(stdout, "\n");
	  fprintf(stdout, "  header type: %s\n  sample type: %s\n  ",
		  header_name,
		  samp_type_info);

	  strftime(timestr, 64, "%a %d-%b-%Y %H:%M %Z", localtime(&date));
	  fprintf(stdout, "written: %s", timestr);

	  if ((chans > 0) && (mus_sound_maxamp_exists(argv[ctr])))
	    {
	      ampstr = display_maxamps(argv[ctr], chans);
	      if (ampstr) fprintf(stdout, "%s", ampstr);
	    }
	  fprintf(stdout, "\n");
	  if (comment) fprintf(stdout, "  comment: %s\n", comment);
	  if (loops)
	    {
	      fprintf(stdout, "  loop: %d to %d\n", loops[0], loops[1]);
	      if (loops[2] != 0)
		fprintf(stdout, "  loop: %d to %d\n", loops[2], loops[3]);
	      if (loops[0] != 0)
		fprintf(stdout, "    base: %d, detune: %d\n", loops[4], loops[5]);
	    }
	}
      else
	fprintf(stderr, "%s: %s\n", argv[ctr], strerror(errno));
      if (ctr < argc - 1) fprintf(stdout, "\n");
    }
  return(0);
}
