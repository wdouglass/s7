#include "snd.h"
#include "sndlib-strings.h"
#include "clm2xen.h"

static snd_info *get_sp_1(int index)
{
  if ((index >= 0) &&
      (index < ss->max_sounds) && 
      (snd_ok(ss->sounds[index])))
    return(ss->sounds[index]);

  return(NULL);
}

snd_info *get_sp(Xen snd)
{
  if (xen_is_sound(snd))
    return(get_sp_1(xen_sound_to_int(snd)));

#if (!HAVE_SCHEME)      
  if (Xen_is_integer(snd))
    return(get_sp_1(Xen_integer_to_C_int(snd)));
#else
  if (Xen_is_integer(snd))
    {
      s7_int p;
      p = s7_integer(snd);
      if ((p < 0) ||
	  (p > ss->max_sounds))
	return(NULL);
      return(get_sp_1((int)p));
    }
#endif

  if ((Xen_is_boolean(snd)) || (!Xen_is_bound(snd)))  /* use default sound, if any */
    return(any_selected_sound());

  return(NULL);
}


snd_info *snd_new_file(const char *newname, int chans, int srate, mus_sample_t sample_type, 
		       mus_header_t header_type, const char *new_comment, mus_long_t samples)
{
  /* caller checks newname != null, and runs overwrite hook */
  if (mus_header_writable(header_type, sample_type))
    {
      io_error_t err;
      err = snd_write_header(newname, header_type, srate, chans, samples * chans, sample_type, new_comment, NULL);
      if (err != IO_NO_ERROR)
	snd_error("%s %s: %s", 
		  io_error_name(err),
		  newname, 
		  snd_io_strerror());
      else
	{
	  int chan;
	  mus_long_t size;
	  /* send out the initial samples */
	  chan = snd_reopen_write(newname);
	  lseek(chan, mus_header_data_location(), SEEK_SET);
	  size = chans * mus_samples_to_bytes(sample_type, samples);
	  if (size > 0)
	    {
	      ssize_t bytes;
	      unsigned char *buf;
	      buf = (unsigned char *)calloc(size, sizeof(unsigned char));
	      bytes = write(chan, buf, size);
	      if (bytes == 0)
		fprintf(stderr, "%s: write error", newname);
	      free(buf);
	    }
	  snd_close(chan, newname);
	  ss->open_requestor = FROM_NEW_FILE_DIALOG;
	  return(sound_is_silence(snd_open_file(newname, FILE_READ_WRITE)));
	}
    }
  else 
    snd_error("%s: can't write %s header with %s sample type",
	      newname,
	      mus_header_type_name(header_type),
	      mus_sample_type_name(sample_type));
  return(NULL);
}


/* ---------------- peak amp envs ---------------- */

typedef struct env_state {
  int slice, edpos;
  mus_long_t samples, m;  
  peak_env_info *ep; 
  snd_fd *sf;

  unsigned char *direct_data;
  mus_sample_t format;
  int chans, bytes, fd;
  bool file_open;
} env_state;


static env_state *free_env_state(env_state *es)
{
  if (es)
    {
      if (es->sf)
	es->sf = free_snd_fd(es->sf);
      if (es->file_open)
	{
	  close(es->fd);
	  es->file_open = false;
	}
      if (es->direct_data)
	{
	  free(es->direct_data);
	  es->direct_data = NULL;
	}
      free(es);
    }
  return(NULL);
}


peak_env_info *free_peak_env_info(peak_env_info *ep)
{
  if (ep)
    {
      if (ep->data_max) {free(ep->data_max); ep->data_max = NULL;}
      if (ep->data_min) {free(ep->data_min); ep->data_min = NULL;}
      free(ep);
    }
  return(NULL);
}


peak_env_info *free_peak_env(chan_info *cp, int pos)
{
  /* can be either during channel close, or premature work proc removal */
  if ((cp) && 
      (cp->edits) &&
      (pos < cp->edit_size) &&
      (cp->edits[pos]->peak_env))
    {
      free_peak_env_info(cp->edits[pos]->peak_env);
      cp->edits[pos]->peak_env = NULL;
    }
  return(NULL);
}


/* during processing, cp->peak_env_state -> env_state for that channel
 *  cp->peak_env_in_progress is the associated X work proc
 */

void free_peak_env_state(chan_info *cp)
{
  /* env info is tied into cp edit list peak envs immediately upon env start, released via normal cp cleanups */
  /* this function just cleans up the current work proc stuff (amp_env in this case can be incomplete) */
  if (cp)
    {
      cp->peak_env_state = free_env_state(cp->peak_env_state);
      cp->peak_env_in_progress = 0;
    }
}

#define MIN_INIT 1000000.0
#define MAX_INIT -1000000.0
#define MAX_ENV_SIZE (1 << 30)

static env_state *make_env_state(chan_info *cp, mus_long_t samples)
{
  int pos, orig_pos;
  peak_env_info *ep;
  env_state *es;

  if (samples <= 0) return(NULL);
  if (samples > MAX_ENV_SIZE) return(NULL);
  stop_peak_env(cp);
  pos = cp->edit_ctr;
  orig_pos = cp->edits[pos]->edpos; /* don't assume we're editing the preceding state! */
  es = (env_state *)calloc(1, sizeof(env_state)); /* only creation point */
  es->file_open = false;
  es->samples = samples;
  es->slice = 0;
  es->edpos = pos;
  es->m = 0;
  
  es->direct_data = NULL;

  if (cp->edits[pos]->peak_env)
    {
      es->ep = cp->edits[pos]->peak_env;
      ep = es->ep;
    }
  else 
    {
      bool happy = false;
      es->ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      ep = es->ep;
      if (pos > 0)
	{
	  peak_env_info *old_ep;
	  old_ep = cp->edits[orig_pos]->peak_env;

	  if ((old_ep) && 
	      (old_ep->completed))
	    {
	      mus_long_t old_samples;

	      /* here in many cases, the underlying edit's amp env has most of the data we need.
	       * cp->edits[cp->edit_ctr] describes the current edit, with beg and end, so in the
	       * simplest case, we can just copy to the bin representing beg, and from the bin
	       * representing end (setting ep->top_bin and ep->bin); if the file's length has
	       * changed dramatically, we need to do it all.  fmin/fmax need to be set as we copy.
	       * as-one-edit can mess this up...
	       */

	      old_samples = cp->edits[orig_pos]->samples;
	      if (snd_abs_mus_long_t(samples - old_samples) < (samples / 2))
		{
		  mus_long_t start, end;
		  start = edit_changes_begin_at(cp, cp->edit_ctr);
		  end = edit_changes_end_at(cp, cp->edit_ctr);

		  if (snd_abs_mus_long_t(end - start) < (samples / 2))
		    {
		      int i, start_bin;

		      /* here we'll try to take advantage of an existing envelope */
		      old_ep = cp->edits[orig_pos]->peak_env;
		      ep->samps_per_bin = old_ep->samps_per_bin;
		      ep->peak_env_size = (int)(ceil((double)(es->samples) / (double)(ep->samps_per_bin)));
		      ep->data_max = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
		      ep->data_min = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
		      start_bin = (int)(start / ep->samps_per_bin);
		      ep->fmin = old_ep->data_min[0];
		      ep->fmax = old_ep->data_max[0];
		      for (i = 0; i < start_bin; i++) 
			{
			  ep->data_min[i] = old_ep->data_min[i];
			  ep->data_max[i] = old_ep->data_max[i];
			  if (ep->data_min[i] < ep->fmin) ep->fmin = ep->data_min[i];
			  if (ep->data_max[i] > ep->fmax) ep->fmax = ep->data_max[i];
			}
		      ep->bin = start_bin;
		      if (end != 0)
			{
			  int j, end_bin, old_end_bin;
			  old_end_bin = (int)(end / old_ep->samps_per_bin);
			  end += (samples - old_samples);
			  end_bin = (int)(end / ep->samps_per_bin);
			  if (end_bin <= 0)
			    {
			      old_end_bin += (1 - end_bin);
			      end_bin = 1;
			    }

			  for (i = end_bin, j = old_end_bin; (i < ep->peak_env_size) && (j < old_ep->peak_env_size); i++, j++)
			    {
			      ep->data_min[i] = old_ep->data_min[j];
			      ep->data_max[i] = old_ep->data_max[j];
			      if (ep->data_min[i] < ep->fmin) ep->fmin = ep->data_min[i];
			      if (ep->data_max[i] > ep->fmax) ep->fmax = ep->data_max[i];
			    }
			  ep->top_bin = end_bin;
			}
		      else ep->top_bin = 0;
		      happy = true;
		    }
		}
	    }
	}
      if (!happy)
	{
	  int val;
	  /* we want samps_per_bin to be useful over a wide range of file sizes */
	  /* 160e6 = about a hour at 44KHz */

	  val = (int)(log((double)(es->samples)));
	  if (val > 20) val = 20;
	  ep->peak_env_size = snd_int_pow2(val);
	  ep->samps_per_bin = (int)(ceil((double)(es->samples) / (double)(ep->peak_env_size)));
	  ep->data_max = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
	  ep->data_min = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
	  ep->bin = 0;
	  ep->top_bin = 0;
	  ep->fmin = 10000000.0;
	  ep->fmax = -10000000.0;
	  /* preset as much as possible of the envelope */
	}
      cp->edits[pos]->peak_env = ep;
      ep->completed = false;
    }
  es->sf = NULL;
  return(es);
}


void start_peak_env_state(chan_info *cp)
{
  cp->peak_env_state = make_env_state(cp, current_samples(cp));
}



static bool tick_peak_env(chan_info *cp, env_state *es)
{
  peak_env_info *ep;

  ep = es->ep;
  if (es->slice == 0)
    {
      int n, sb, lm;
      mus_long_t samps_to_read;

      if (ep->top_bin != 0)
	lm = (ep->top_bin - ep->bin + 1);
      else lm = (ep->peak_env_size - ep->bin);
      if (lm <= 0) lm = 1;

      samps_to_read = (mus_long_t)lm * (mus_long_t)(ep->samps_per_bin);
      if (samps_to_read > 1000000)
	{
	  lm = 1000000 / ep->samps_per_bin;
	  samps_to_read = (mus_long_t)lm * (mus_long_t)(ep->samps_per_bin);
	}

      sb = ep->bin;
      if (sb >= ep->peak_env_size)
	{
	  /* oops... */
	  es->slice++;
	  if (es->sf)
	    es->sf = free_snd_fd(es->sf);
	  if (es->direct_data)
	    {
	      free(es->direct_data);
	      es->direct_data = NULL;
	    }
	  ep->completed = true;
	  return(true);
	}

      if ((!es->sf) &&
	  (!es->direct_data))
	{
	  if ((cp->edit_ctr == 0) &&
	      (cp->sound) &&
	      (cp->sound->inuse == SOUND_NORMAL) &&
	      (cp->sound->hdr) &&
	      (cp->sound->nchans <= 4) &&
	      (cp->sounds) &&
	      (cp->sounds[0]) &&
	      (cp->sounds[0]->io))
	    {
	      es->fd = mus_file_open_read(cp->sound->filename);
	      if (es->fd == -1)
		{
		  snd_warning("%s no longer exists!", cp->sound->filename);
		  return(true);
		}
	      es->file_open = true;
	      lseek(es->fd, cp->sound->hdr->data_location, SEEK_SET);

	      es->format = cp->sound->hdr->sample_type;
	      es->chans = cp->sound->nchans;
	      es->bytes = ep->samps_per_bin * mus_bytes_per_sample(es->format) * es->chans;
	      es->direct_data = (unsigned char *)malloc(es->bytes * lm);
	    }
	  else es->sf = init_sample_read_any(ep->bin * ep->samps_per_bin, cp, READ_FORWARD, es->edpos);
	}
      
      if (!es->direct_data)
	{
	  snd_fd *sfd;
	  sfd = es->sf;
	  if (!sfd) return(false);

	  for (n = 0; (n < lm) && (sb < ep->peak_env_size); n++, sb++)
	    {
	      mus_float_t ymin, ymax, val;
	      int i, lim;
	      val = read_sample(sfd);
	      ymin = val;
	      ymax = val;
	      i = 1;
	      lim = ep->samps_per_bin - 4;
	      while (i <= lim)
		{
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  i += 4;
		}
	      for (; i < ep->samps_per_bin; i++)
		{
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		}
	      ep->data_max[sb] = ymax;
	      ep->data_min[sb] = ymin;
	      
	      if (ymin < ep->fmin) ep->fmin = ymin;
	      if (ymax > ep->fmax) ep->fmax = ymax;
	    }
	}
      else
	{
	  ssize_t bytes_read;
	  
	  /* there might be trailing chunks, so we have to keep track of es->samples (Tito Latini 2-Feb-17) */
	  bytes_read = es->samples * mus_bytes_per_sample(es->format) * es->chans;
	  if (bytes_read > (lm * es->bytes))
	    bytes_read = lm * es->bytes;
	  bytes_read = read(es->fd, (char *)(es->direct_data), bytes_read);
	  if (bytes_read < lm * es->bytes)
	    {
	      int zero_byte;
	      zero_byte = mus_sample_type_zero(es->format);
	      if ((zero_byte == 0) ||
		  ((es->format != MUS_UBSHORT) &&
		   (es->format != MUS_ULSHORT)))
		memset((void *)(es->direct_data + bytes_read), zero_byte, lm * es->bytes - bytes_read);
	      else /* MUS_UB|LSHORT 32768 or 128(as a short)=>0 */
		{
		  mus_long_t i, start, len;
		  unsigned short *buf;

		  /* (with-sound (:sample-type mus-ubshort) (fm-violin 0 2 440 .1)) */

		  buf = (unsigned short *)(es->direct_data);
		  start = bytes_read / 2;
		  len = lm * es->bytes / 2;
		  for (i = start; i < len; i++)
		    buf[i] = (unsigned short)zero_byte;
		}
	    }
	  
	  for (n = 0; (n < lm) && (sb < ep->peak_env_size); n++, sb++)
	    {
	      mus_float_t cur_min = 0.0, cur_max = 0.0;
	      mus_samples_bounds((unsigned char *)(es->direct_data + es->bytes * n), es->bytes, cp->chan, es->chans, es->format, &cur_min, &cur_max);
	      
	      ep->data_max[sb] = cur_max;
	      ep->data_min[sb] = cur_min;
	      
	      if (cur_min < ep->fmin) ep->fmin = cur_min;
	      if (cur_max > ep->fmax) ep->fmax = cur_max;
	    }
	}

      es->m += samps_to_read;
      ep->bin += lm;
      if ((es->m >= es->samples) || 
	  ((ep->top_bin > 0) && (ep->bin >= ep->top_bin))) /* this applies to partial amp envs */
	{
	  es->slice++;

	  if (es->sf)
	    es->sf = free_snd_fd(es->sf);

	  if (es->direct_data)
	    {
	      free(es->direct_data);
	      es->direct_data = NULL;
	    }
	  ep->completed = true;
	  return(true);
	}
      return(false);
    }
  else
    {
      ep->completed = true;
      return(true);
    }
}

void finish_peak_env(chan_info *cp)
{
  if ((cp->peak_env_in_progress) && 
      (cp->peak_env_state))
    {
      while (!(tick_peak_env(cp, cp->peak_env_state))) ; /* finish peak-env scan */
      enved_reflect_peak_env_completion(cp->sound);
      free_peak_env_state(cp);
    }
}


idle_func_t get_peak_env(any_pointer_t ptr)
{
  /* calculate the amp env of channel */
  chan_info *cp = (chan_info *)ptr;
  env_state *es;
  int pos;

  if (!cp) return(BACKGROUND_QUIT);

  pos = cp->edit_ctr;
  if ((pos == -1) || 
      (cp->active < CHANNEL_HAS_EDIT_LIST))
    {
      free_peak_env_state(cp);
      return(BACKGROUND_QUIT);
    }

  if (!(cp->peak_env_state)) 
    cp->peak_env_state = make_env_state(cp, current_samples(cp));

  es = cp->peak_env_state;
  if (es)
    {
      if (tick_peak_env(cp, es))
	{
	  free_peak_env_state(cp);
	  enved_reflect_peak_env_completion(cp->sound);
	  if (cp->waiting_to_make_graph) 
	    {
	      cp->waiting_to_make_graph = false;
	      cp->new_peaks = true;
	      update_graph(cp);
	      cp->new_peaks = false;
	    }
	  return(BACKGROUND_QUIT);
	}
      else return(BACKGROUND_CONTINUE);
    }
  return(BACKGROUND_QUIT);
}


bool peak_env_maxamp_ok(chan_info *cp, int edpos)
{
  if (cp)
    {
      peak_env_info *ep;
      ep = cp->edits[edpos]->peak_env;
      return((ep) && (ep->completed));
    }
  return(false);
}


mus_float_t peak_env_maxamp(chan_info *cp, int edpos)
{
  peak_env_info *ep;
  mus_float_t ymax;
  ep = cp->edits[edpos]->peak_env;
  ymax = -ep->fmin;
  if (ymax < ep->fmax) 
    return(ep->fmax);
  return(ymax);
}


bool peak_env_usable(chan_info *cp, mus_float_t samples_per_pixel, mus_long_t hisamp, bool start_new, int edit_pos, bool finish_env) 
{
  peak_env_info *ep;

#if USE_NO_GUI
  return(false);
#endif

  if (!(cp->edits)) return(false);

  ep = cp->edits[edit_pos]->peak_env;
  if (ep)
    {
      int bin;
      bin = (int)(hisamp / ep->samps_per_bin); 
      if ((ep->completed) || 
	  (bin < ep->bin) || 
	  ((ep->top_bin != 0) && (bin > ep->top_bin)))
	return(samples_per_pixel >= (mus_float_t)(ep->samps_per_bin));
    }

  if ((finish_env) && (cp->peak_env_in_progress) && (cp->peak_env_state))
    {
      /* caller wants data, but a read is in progress -- finish it as quickly as possible */
      finish_peak_env(cp);
      if (cp->waiting_to_make_graph) 
	{
	  cp->waiting_to_make_graph = false;
	  update_graph(cp);
	}
      return(peak_env_usable(cp, samples_per_pixel, hisamp, start_new, edit_pos, false));
    }

  if ((start_new) &&
      (!(cp->peak_env_in_progress)) && 
      (current_samples(cp) > PEAK_ENV_CUTOFF) &&
      (cp->sound->short_filename))             /* region browser jumped in too soon during autotest */
    start_peak_env(cp);
  return(false);
}


static short local_grf_y(mus_float_t val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((short)(ap->y_base + val * ap->y_scale));
}


int peak_env_graph(chan_info *cp, mus_float_t samples_per_pixel, int srate) 
{
  mus_float_t step, x, pinc = 0.0;
  double xf, xk;
  mus_float_t ymin, ymax;
  int xi;
  int j = 0;
  mus_long_t i;
  peak_env_info *ep;
  axis_info *ap;

  ap = cp->axis;
  ep = cp->edits[cp->edit_ctr]->peak_env;
  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
  xf = (double)(ap->losamp) / (double)(ep->samps_per_bin);
  x = ap->x0;
  xi = grf_x(x, ap);
  i = ap->losamp;
  xk = (double)i;
  if (cp->printing) pinc = (mus_float_t)samples_per_pixel / (mus_float_t)srate;
  ymin = ep->fmax;
  ymax = ep->fmin;

  while (i <= ap->hisamp)
    {
      int k, kk;
      k = (int)xf;
      xf += step;
      kk = (int)xf;
      if (kk >= ep->peak_env_size)
	{
	  kk = ep->peak_env_size - 1;
          if (k > kk) k = kk; /* make sure we get a value below */
	}
      for (; k <= kk; k++)
	{
	  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
	  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
	}
      xk += samples_per_pixel;
      i = (mus_long_t)xk;
      set_grf_points(xi, j,
		     local_grf_y(ymin, ap),
		     local_grf_y(ymax, ap));
      if (cp->printing) 
	{
	  x += pinc; 
	  ps_set_grf_points(x, j, ymin, ymax);
	}
      xi++;
      j++;
      if (j >= POINT_BUFFER_SIZE) break;
      ymin = ep->fmax;
      ymax = ep->fmin;
    }
  return(j);
}


int peak_env_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end, mus_float_t samples_per_pixel, int srate)
{
  mus_float_t step, x;
  double xf, xk;
  mus_float_t ymin, ymax;
  int xi;
  int j = 0;
  mus_long_t i;
  peak_env_info *ep;
  axis_info *ap;

  ap = cp->axis;
  ep = cp->edits[cp->edit_ctr]->peak_env;
  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
  xf = (double)(beg) / (double)(ep->samps_per_bin);
  x = beg / srate;
  xi = grf_x(x, ap);
  i = beg;
  xk = (double)i;

  ymin = ep->fmax;
  ymax = ep->fmin;

  while (i <= end)
    {
      int k, kk;
      k = (int)xf;
      xf += step;
      kk = (int)xf;
      if (kk >= ep->peak_env_size) 
	kk = ep->peak_env_size - 1;
      for (; k <= kk; k++)
	{
	  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
	  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
	}
      xk += samples_per_pixel;
      i = (mus_long_t)xk;
      set_grf_points(xi, j,
		     local_grf_y(ymin, ap),
		     local_grf_y(ymax, ap));
      xi++;
      j++;
      if (j >= POINT_BUFFER_SIZE) break;
      ymin = ep->fmax;
      ymax = ep->fmin;
    }
  return(j);
}


void peak_env_scale_by(chan_info *cp, mus_float_t scl, int pos)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      int i;
      peak_env_info *new_ep;
      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);
      if (!new_ep)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}
      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (scl >= 0.0)
	{
	  new_ep->fmin = old_ep->fmin * scl;
	  new_ep->fmax = old_ep->fmax * scl;
	  for (i = 0; i < new_ep->peak_env_size; i++) 
	    {
	      new_ep->data_min[i] = old_ep->data_min[i] * scl;
	      new_ep->data_max[i] = old_ep->data_max[i] * scl;
	    }
	}
      else
	{
	  new_ep->fmax = old_ep->fmin * scl;
	  new_ep->fmin = old_ep->fmax * scl;
	  for (i = 0; i < new_ep->peak_env_size; i++) 
	    {
	      new_ep->data_max[i] = old_ep->data_min[i] * scl;
	      new_ep->data_min[i] = old_ep->data_max[i] * scl;
	    }
	}
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
    }
}


void pick_one_bin(peak_env_info *ep, int bin, mus_long_t cursamp, chan_info *cp, int edpos)
{
  snd_fd *sf;
  int n;
  mus_float_t val, ymin, ymax;

  /* here we have to read the current bin using the current fragments */
  sf = init_sample_read_any(cursamp, cp, READ_FORWARD, edpos);
  if (!sf) return;

  val = read_sample(sf); 
  ymin = val;
  ymax = val;

  for (n = 1; n < ep->samps_per_bin; n++)
    {
      val = read_sample(sf); 
      if (ymin > val) ymin = val; 
      if (ymax < val) ymax = val;
    }

  ep->data_max[bin] = ymax;
  ep->data_min[bin] = ymin;
  free_snd_fd(sf);
}


void peak_env_scale_selection_by(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos)
{
  peak_env_info *old_ep;

  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      mus_float_t fmax = MAX_INIT, fmin = MIN_INIT;
      mus_long_t cursamp, start, end;
      int i;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);

      if (!new_ep)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}

      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;

      for (i = 0, cursamp = 0; i < new_ep->peak_env_size; i++, cursamp += new_ep->samps_per_bin) 
	{
	  if ((cursamp >= end) || (cursamp <= start))
	    {
	      new_ep->data_min[i] = old_ep->data_min[i];
	      new_ep->data_max[i] = old_ep->data_max[i];
	    }
	  else
	    {
	      /* if segment is entirely in scaled section, just scale it */
	      if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
		{
		  if (scl >= 0.0)
		    {
		      new_ep->data_max[i] = old_ep->data_max[i] * scl;
		      new_ep->data_min[i] = old_ep->data_min[i] * scl;
		    }
		  else
		    {
		      new_ep->data_max[i] = old_ep->data_min[i] * scl;
		      new_ep->data_min[i] = old_ep->data_max[i] * scl;
		    }
		}
	      else pick_one_bin(new_ep, i, cursamp, cp, cp->edit_ctr);
	    }
	  if (fmin > new_ep->data_min[i]) fmin = new_ep->data_min[i];
	  if (fmax < new_ep->data_max[i]) fmax = new_ep->data_max[i];
	}

      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
    }
}


peak_env_info *peak_env_section(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos)
{
  /* used in snd-region.c to create the region peak amp env */
  peak_env_info *old_ep, *new_ep = NULL;
  mus_float_t fmax = MAX_INIT, fmin = MIN_INIT;
  int i, j;
  mus_long_t cursamp, start, end;

  old_ep = cp->edits[edpos]->peak_env;
  if (!old_ep) return(NULL);

  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
  new_ep->peak_env_size = old_ep->peak_env_size;
  new_ep->samps_per_bin = old_ep->samps_per_bin;

  end = beg + num - 1;
  start = beg - new_ep->samps_per_bin;
  for (i = 0, j = 0, cursamp = 0; (i < new_ep->peak_env_size) && (cursamp < end); i++, cursamp += new_ep->samps_per_bin) 
    {
      if (cursamp > start)
	{
	  /* if segment is entirely in region, just copy it */
	  if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
	    {
	      new_ep->data_max[j] = old_ep->data_max[i];
	      new_ep->data_min[j] = old_ep->data_min[i];
	    }
	  else pick_one_bin(new_ep, j, cursamp, cp, edpos);
	  if (fmin > new_ep->data_min[j]) fmin = new_ep->data_min[j];
	  if (fmax < new_ep->data_max[j]) fmax = new_ep->data_max[j];
	  j++;
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
    }
  return(new_ep);
}


peak_env_info *copy_peak_env_info(peak_env_info *old_ep, bool reversed)
{
  peak_env_info *new_ep = NULL;
  if ((old_ep) && 
      (old_ep->completed))
    {
      new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
      new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      new_ep->fmin = old_ep->fmin;
      new_ep->fmax = old_ep->fmax;

      if (reversed)
	{
	  int i, j;
	  for (i = 0, j = new_ep->peak_env_size - 1; i < new_ep->peak_env_size; i++, j--) 
	    {
	      new_ep->data_min[j] = old_ep->data_min[i];
	      new_ep->data_max[j] = old_ep->data_max[i];
	    }
	}
      else
	{
	  mus_copy_floats(new_ep->data_min, old_ep->data_min, new_ep->peak_env_size);
	  mus_copy_floats(new_ep->data_max, old_ep->data_max, new_ep->peak_env_size);
	}

      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
    }
  return(new_ep);
}


peak_env_info *peak_env_copy(chan_info *cp, bool reversed, int edpos)
{
  return(copy_peak_env_info(cp->edits[edpos]->peak_env, reversed));
}


void amp_env_env(chan_info *cp, mus_float_t *brkpts, int npts, int pos, mus_float_t base, mus_float_t scaler, mus_float_t offset)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      int i;
      mus_any *e;
      mus_float_t fmin, fmax;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);

      if (!new_ep)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}

      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (base == 1.0)
	e = mus_make_env(brkpts, npts, scaler, offset, base, 0.0, new_ep->peak_env_size - 1, brkpts);
      else e = mus_make_env(brkpts, npts, 1.0, 0.0, base, 0.0, new_ep->peak_env_size - 1, brkpts);
      fmin = MIN_INIT;
      fmax = MAX_INIT;

      for (i = 0; i < new_ep->peak_env_size; i++) 
	{
	  mus_float_t val;
	  val = mus_env(e);
	  if (val >= 0.0)
	    {
	      new_ep->data_min[i] = old_ep->data_min[i] * val;
	      new_ep->data_max[i] = old_ep->data_max[i] * val;
	    }
	  else
	    {
	      new_ep->data_min[i] = old_ep->data_max[i] * val;
	      new_ep->data_max[i] = old_ep->data_min[i] * val;
	    }
	  if (new_ep->data_min[i] < fmin) fmin = new_ep->data_min[i];
	  if (new_ep->data_max[i] > fmax) fmax = new_ep->data_max[i];
	}

      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
      mus_free(e);
    }
}


void amp_env_env_selection_by(chan_info *cp, mus_any *e, mus_long_t beg, mus_long_t num, int pos)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      mus_float_t xmax = 1.0;
      mus_float_t *data;
      mus_float_t fmax = MAX_INIT, fmin = MIN_INIT;
      int i;
      mus_long_t cursamp, start, end;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);

      if (!new_ep)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}

      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;
      data = mus_data(e);
      xmax = data[mus_env_breakpoints(e) * 2 - 2];

      for (i = 0, cursamp = 0; i < new_ep->peak_env_size; i++, cursamp += new_ep->samps_per_bin) 
	{
	  if ((cursamp >= end) || (cursamp <= start))
	    {
	      new_ep->data_min[i] = old_ep->data_min[i];
	      new_ep->data_max[i] = old_ep->data_max[i];
	    }
	  else
	    {
	      /* if segment is entirely in scaled section, just scale it */
	      if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
		{
		  mus_float_t val;
		  val = mus_env_interp((double)(cursamp - beg) * xmax / (double)num, e);
		  if (val >= 0.0)
		    {
		      new_ep->data_max[i] = old_ep->data_max[i] * val;
		      new_ep->data_min[i] = old_ep->data_min[i] * val;
		    }
		  else
		    {
		      new_ep->data_max[i] = old_ep->data_min[i] * val;
		      new_ep->data_min[i] = old_ep->data_max[i] * val;
		    }

		}
	      else pick_one_bin(new_ep, i, cursamp, cp, cp->edit_ctr);
	    }
	  if (fmin > new_ep->data_min[i]) fmin = new_ep->data_min[i];
	  if (fmax < new_ep->data_max[i]) fmax = new_ep->data_max[i];
	}

      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
    }
}


void peak_env_insert_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int pos)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      mus_long_t end, old_samps, cur_samps;
      int i, j, subsamp, val, bins;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if (new_ep) new_ep = free_peak_env(cp, cp->edit_ctr);

      old_samps = cp->edits[pos]->samples;
      cur_samps = current_samples(cp);
      val = (int)(log((double)(cur_samps)));
      if (val > 20) val = 20;
      val = snd_int_pow2(val);
      subsamp = val / old_ep->peak_env_size;
      if (subsamp != 1) return;

      new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      new_ep->peak_env_size = (int)(ceil(cur_samps / new_ep->samps_per_bin));
      new_ep->completed = true;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
      new_ep->bin = new_ep->peak_env_size;
      new_ep->top_bin = new_ep->peak_env_size;
      new_ep->data_max = (mus_float_t *)calloc(new_ep->peak_env_size, sizeof(mus_float_t));
      new_ep->data_min = (mus_float_t *)calloc(new_ep->peak_env_size, sizeof(mus_float_t));
      new_ep->fmin = old_ep->fmin;
      if (new_ep->fmin > 0.0) new_ep->fmin = 0.0;
      new_ep->fmax = old_ep->fmax;
      if (new_ep->fmax < 0.0) new_ep->fmax = 0.0;
      end = beg + num - 1;

      if (beg == 0)
	{
	  /* insert at start, so copy to end */
	  i = (int)ceil(end / new_ep->samps_per_bin);
	  bins = new_ep->peak_env_size - i;
	  if (old_ep->peak_env_size < bins) bins = old_ep->peak_env_size;
	  mus_copy_floats(&(new_ep->data_min[i]), old_ep->data_min, bins);
	  mus_copy_floats(&(new_ep->data_max[i]), old_ep->data_max, bins);
	}
      else
	{
	  if (beg >= old_samps)
	    {
	      /* copy start */
	      bins = (int)floor(beg / old_ep->samps_per_bin);
	      if (bins > old_ep->peak_env_size) bins = old_ep->peak_env_size;
	      mus_copy_floats(new_ep->data_min, old_ep->data_min, bins);
	      mus_copy_floats(new_ep->data_max, old_ep->data_max, bins);
	    }
	  else
	    {
	      i = (int)floor(beg / old_ep->samps_per_bin);
	      if (i > 0)
		{
		  mus_copy_floats(new_ep->data_min, old_ep->data_min, i);
		  mus_copy_floats(new_ep->data_max, old_ep->data_max, i);
		}
	      if (i < new_ep->peak_env_size)
		{
		  pick_one_bin(new_ep, i, i * old_ep->samps_per_bin, cp, cp->edit_ctr);
		  i++;
		}
	      j = (int)floor(end / new_ep->samps_per_bin);
	      if (j < new_ep->peak_env_size)
		{
		  pick_one_bin(new_ep, j, j * new_ep->samps_per_bin, cp, cp->edit_ctr);
		  j++;
		}
	      if (i < old_ep->peak_env_size)
		{
		  bins = new_ep->peak_env_size - j;
		  if ((i + bins) >= old_ep->peak_env_size)
		    bins = old_ep->peak_env_size - i;
		  mus_copy_floats(&(new_ep->data_min[j]), &(old_ep->data_min[i]), bins);
		  mus_copy_floats(&(new_ep->data_max[j]), &(old_ep->data_max[i]), bins);
		}
	    }
	}
    }
}


#if XEN_HAVE_RATIOS
void snd_rationalize(mus_float_t a, int *num, int *den)
{
  Xen ratio;
  int gloc;
  ratio = Xen_rationalize(C_double_to_Xen_real(a), C_double_to_Xen_real(a * .04)); /* was .02 until 13-Dec-07 but that gives too many useless choices */
  gloc = snd_protect(ratio);
  (*num) = (int)Xen_numerator(ratio);
  (*den) = (int)Xen_denominator(ratio);
  snd_unprotect_at(gloc);
}
#endif


/* -------- control panel speed -------- */

#if (!XEN_HAVE_RATIOS)
#define TOTAL_RATS 123

static const char *rat_names[TOTAL_RATS] = {
  "1/20", "5/96", "7/128", "15/256", "31/512", "1/16", "1/15", "5/72", "9/128", "3/40", "5/64", "1/12", "11/128", "3/32", "1/10", "5/48", "7/64", "15/128", "31/256", "1/8", "2/15", "5/36", "9/64", "3/20", "5/32", "1/6", "11/64", "3/16", "1/5", "5/24", "7/32", "15/64", "31/128", "1/4", "4/15", "5/18", "9/32", "3/10", "5/16", "1/3", "11/32", "3/8", "2/5", "5/12", "7/16", "15/32", "31/64", "1/2", "8/15", "5/9", "9/16", "3/5", "5/8", "2/3", "11/16", "3/4", "4/5", "5/6", "7/8", "15/16", "31/32", "1/1", "16/15", "10/9", "9/8", "6/5", "5/4", "4/3", "11/8", "3/2", "8/5", "5/3", "7/4", "15/8", "31/16", "2/1", "32/15", "20/9", "9/4", "12/5", "5/2", "8/3", "11/4", "3/1", "16/5", "10/3", "7/2", "15/4", "31/8", "4/1", "64/15", "40/9", "9/2", "24/5", "5/1", "16/3", "11/2", "6/1", "32/5", "20/3", "7/1", "15/2", "31/4", "8/1", "128/15", "80/9", "9/1", "48/5", "10/1", "32/3", "11/1", "12/1", "64/5", "40/3", "14/1", "15/1", "31/2", "16/1", "256/15", "160/9", "18/1", "96/5", "20/1"};

static mus_float_t rat_values[TOTAL_RATS] = {
  0.050, 0.052, 0.055, 0.059, 0.061, 0.063, 0.067, 0.069, 0.070, 0.075, 0.078, 0.083, 0.086, 0.094, 0.100, 0.104, 0.109, 0.117, 0.121, 0.125, 0.133, 0.139, 0.141, 0.150, 0.156, 0.167, 0.172, 0.188, 0.200, 0.208, 0.219, 0.234, 0.242, 0.250, 0.267, 0.278, 0.281, 0.300, 0.313, 0.333, 0.344, 0.375, 0.400, 0.417, 0.438, 0.469, 0.484, 0.500, 0.533, 0.556, 0.563, 0.600, 0.625, 0.667, 0.688, 0.750, 0.800, 0.833, 0.875, 0.938, 0.969, 1.000, 1.067, 1.111, 1.125, 1.200, 1.250, 1.333, 1.375, 1.500, 1.600, 1.667, 1.750, 1.875, 1.938, 2.000, 2.133, 2.222, 2.250, 2.400, 2.500, 2.667, 2.750, 3.000, 3.200, 3.333, 3.500, 3.750, 3.875, 4.000, 4.267, 4.444, 4.500, 4.800, 5.000, 5.333, 5.500, 6.000, 6.400, 6.667, 7.000, 7.500, 7.750, 8.000, 8.533, 8.889, 9.000, 9.600, 10.000, 10.667, 11.000, 12.000, 12.800, 13.333, 14.000, 15.000, 15.500, 16.000, 17.067, 17.778, 18.000, 19.200, 20.000};
#endif


mus_float_t speed_changed(mus_float_t val, char *srcbuf, speed_style_t style, int tones, int srcbuf_size)
{
  char numbuf[16];
  int semi, i, j;
  switch (style)
    {
    case SPEED_CONTROL_AS_RATIO:
#if XEN_HAVE_RATIOS
      {
	int num, den;
	snd_rationalize(val, &num, &den);
	snprintf(srcbuf, srcbuf_size, "%d/%d", num, den);
	return((mus_float_t)num / (mus_float_t)den);
      }
#else
      for (i = 1; i < TOTAL_RATS; i++)
	if (rat_values[i] > val) 
	  break;
      if ((rat_values[i] - val) < (val - rat_values[i - 1]))
	{
	  snprintf(srcbuf, srcbuf_size, "%s", rat_names[i]);
	  return(rat_values[i]);
	}
      else
	{
	  snprintf(srcbuf, srcbuf_size, "%s", rat_names[i - 1]);
	  return(rat_values[i - 1]);
	}
#endif
      break;

    case SPEED_CONTROL_AS_SEMITONE: 
      /* find closest semitone to val */
      semi = snd_round(log(val) * ((mus_float_t)tones / log(2.0)));
      /* space until (-) num (-52 to 52 is its range if 12-tone) */
      for (i = 0; i < srcbuf_size; i++) srcbuf[i] = ' '; 
      snprintf(numbuf, 16, "%d", semi);
      j = strlen(numbuf) - 1;
      for (i = 3; (i >= 0) && (j >= 0); i--, j--) 
	srcbuf[i] = numbuf[j];
      srcbuf[srcbuf_size - 1] = 0;
      return(pow(2.0, ((mus_float_t)semi / (mus_float_t)tones)));

    default: 
      snprintf(srcbuf, srcbuf_size, "%.3f", val);
      return(val);
    }
}


/* -------- name click etc */

static char sname[PRINT_BUFFER_SIZE];

char *shortname(snd_info *sp)
{
  if (is_link_file(sp->filename))
    {
      snprintf(sname, PRINT_BUFFER_SIZE, "(%s)", sp->short_filename);
      return(sname);
    }
  return(sp->short_filename);
}


char *shortname_indexed(snd_info *sp)
{
  if (show_indices(ss))
    {
      if (is_link_file(sp->filename))
	snprintf(sname, PRINT_BUFFER_SIZE, "%d: (%s)", sp->index, sp->short_filename); /* don't try to share sname */
      else snprintf(sname, PRINT_BUFFER_SIZE, "%d: %s", sp->index, sp->short_filename);
      return(sname);
    }
  return(shortname(sp));
}


void add_sound_data(char *filename, snd_info *sp, channel_graph_t graphed)
{
  uint32_t i;
  for (i = 0; i < sp->nchans; i++) 
    add_channel_data(filename, sp->chans[i], graphed);
}


#ifndef _MSC_VER
static char *linked_file(const char *link_name)
{
  char *link_file;
  ssize_t bytes;
  #define READLINK_FILE_SIZE 256
  link_file = (char *)calloc(READLINK_FILE_SIZE, sizeof(char));
  bytes = readlink(link_name, link_file, READLINK_FILE_SIZE);
  link_file[bytes] = 0;
  return(link_file);
}
#endif


static Xen name_click_hook;

char *sp_name_click(snd_info *sp) /* caller should free returned string */
{
  if (sp)
    {
      file_info *hdr;

      /* call name-click-hook (if any) return #t = don't print info in the status area */
      if ((Xen_hook_has_list(name_click_hook)) &&
	  (Xen_is_true(run_or_hook(name_click_hook, 
				  Xen_list_1(C_int_to_Xen_sound(sp->index)),
				  S_name_click_hook))))
	return(NULL);

      hdr = sp->hdr;
      if (hdr)
	{
	  mus_float_t dur;
	  char *result, *str = NULL;

	  bool linked;
	  linked = is_link_file(sp->filename);
	  dur = (mus_float_t)((double)(hdr->samples) / (double)(hdr->chans * hdr->srate));
	  result = mus_format("%d, %d chan%s, %.3f sec%s, %s: %s, %s%s%s%s",
			       hdr->srate,
			       hdr->chans,
			       ((hdr->chans > 1) ? "s" : ""),
			       dur,
			       ((dur == 1.0) ? "" : "s"),
			       mus_header_type_to_string(hdr->type),
			       mus_sample_type_to_string(hdr->sample_type),
			       snd_strftime("%d-%b-%Y %H:%M", sp->write_date),
			       (linked) ? ", (link to " : "",
#ifndef _MSC_VER
			       (linked) ? str = linked_file(sp->filename) : "",
#else
			       (linked) ? "?" : "",
#endif
			       (linked) ? ")" : "");
	  if (str) free(str);
	  return(result);
	}
    }
  return(NULL);
}



/* ---------------- save and restore control panel buttons ----------------*/

typedef struct ctrl_state {
  mus_float_t amp, speed, contrast, expand, revscl, revlen;
  env *filter_env;
  bool expand_on, contrast_on, reverb_on, filter_on, reversed;
  int filter_order;
  mus_float_t contrast_amp, expand_ramp, expand_length, expand_hop, expand_jitter, reverb_feedback, reverb_decay, reverb_lowpass;
} ctrl_state;


static ctrl_state *free_control_settings(ctrl_state *cs)
{
  if (cs)
    {
      if (cs->filter_env) free_env(cs->filter_env);
      free(cs);
    }
  return(NULL);
}


void free_controls(snd_info *sp)
{
  sp->saved_controls = free_control_settings(sp->saved_controls);
}


static ctrl_state *current_control_settings(snd_info *sp, ctrl_state *cs)
{
  if (!cs) cs = (ctrl_state *)calloc(1, sizeof(ctrl_state));
  cs->amp = sp->amp_control;
  cs->speed = sp->speed_control;
  cs->expand = sp->expand_control;
  cs->revscl = sp->reverb_control_scale;
  cs->revlen = sp->reverb_control_length;
  cs->contrast = sp->contrast_control;
  cs->expand_on = sp->expand_control_on;
  cs->reverb_on = sp->reverb_control_on;
  cs->contrast_on = sp->contrast_control_on;
  cs->filter_on = sp->filter_control_on;
  cs->filter_order = sp->filter_control_order;
  if (sp->filter_control_envelope) 
    {
      if (cs->filter_env) cs->filter_env = free_env(cs->filter_env);
      cs->filter_env = copy_env(sp->filter_control_envelope);
    }
  if (sp->speed_control_direction == 1) 
    cs->reversed = false; 
  else cs->reversed = true;

  cs->contrast_amp = sp->contrast_control_amp;
  cs->expand_ramp = sp->expand_control_ramp;
  cs->expand_hop = sp->expand_control_hop;
  cs->expand_jitter = sp->expand_control_jitter;
  cs->expand_length = sp->expand_control_length;
  cs->reverb_feedback = sp->reverb_control_feedback;
  cs->reverb_decay = sp->reverb_control_decay;
  cs->reverb_lowpass = sp->reverb_control_lowpass;
  return(cs);
}


void save_controls(snd_info *sp) 
{
  sp->saved_controls = current_control_settings(sp, sp->saved_controls);
}


static ctrl_state *restore_control_settings(snd_info *sp, ctrl_state *cs)
{
  /* for use in controls->channel when the actual control panel is not in use */
  if (cs)
    {
      sp->amp_control = cs->amp;
      sp->speed_control = cs->speed;
      sp->expand_control = cs->expand;
      sp->reverb_control_scale = cs->revscl;
      sp->reverb_control_length = cs->revlen;
      sp->contrast_control = cs->contrast;
      sp->expand_control_on = cs->expand_on;
      sp->reverb_control_on = cs->reverb_on;
      sp->contrast_control_on = cs->contrast_on;
      sp->filter_control_on = cs->filter_on;
      sp->filter_control_order = cs->filter_order;
      if (cs->filter_env)
	{
	  sp->filter_control_envelope = free_env(sp->filter_control_envelope);
	  sp->filter_control_envelope = copy_env(cs->filter_env);
	}
      if (cs->reversed)
	sp->speed_control_direction = -1;
      else sp->speed_control_direction = 1;
      sp->contrast_control_amp = cs->contrast_amp;
      sp->expand_control_ramp = cs->expand_ramp;
      sp->expand_control_hop = cs->expand_hop;
      sp->expand_control_jitter = cs->expand_jitter;
      sp->expand_control_length = cs->expand_length;
      sp->reverb_control_feedback = cs->reverb_feedback;
      sp->reverb_control_decay = cs->reverb_decay;
      sp->reverb_control_lowpass = cs->reverb_lowpass;
    }
  return(cs);
}


void restore_controls(snd_info *sp) 
{
  ctrl_state *cs;
  char *tmpstr;
  cs = sp->saved_controls;
  if (!cs) 
    {
      sp->saved_controls = (ctrl_state *)calloc(1, sizeof(ctrl_state));
      cs = sp->saved_controls;
      cs->amp = DEFAULT_AMP_CONTROL;
      cs->speed = DEFAULT_SPEED_CONTROL;
      cs->reversed = false; /* (this is the button's view) */
      cs->expand = DEFAULT_EXPAND_CONTROL;
      cs->expand_on = DEFAULT_EXPAND_CONTROL_ON;
      cs->revscl = DEFAULT_REVERB_CONTROL_SCALE;
      cs->revlen = DEFAULT_REVERB_CONTROL_LENGTH;
      cs->reverb_on = DEFAULT_REVERB_CONTROL_ON;
      cs->contrast = DEFAULT_CONTRAST_CONTROL;
      cs->contrast_on = DEFAULT_CONTRAST_CONTROL_ON;
      cs->filter_on = DEFAULT_FILTER_CONTROL_ON;
      cs->filter_order = filter_control_order(ss);
      cs->filter_env = NULL;
    }
  toggle_expand_button(sp, cs->expand_on);
  toggle_contrast_button(sp, cs->contrast_on);
  toggle_reverb_button(sp, cs->reverb_on);
  toggle_filter_button(sp, cs->filter_on);
  toggle_direction_arrow(sp, cs->reversed);
  set_amp(sp, cs->amp);
  set_speed(sp, cs->speed);
  set_contrast(sp, cs->contrast);
  set_expand(sp, cs->expand);
  set_revscl(sp, cs->revscl);
  set_revlen(sp, cs->revlen);
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope); 
  if (cs->filter_env) 
    sp->filter_control_envelope = copy_env(cs->filter_env);
  else sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  set_filter_order(sp, cs->filter_order); /* causes redisplay */
  tmpstr = env_to_string(sp->filter_control_envelope);
  set_filter_text(sp, tmpstr);
  if (tmpstr) free(tmpstr);
}


void reset_controls(snd_info *sp) 
{
  char *tmpstr;
  toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_ON);
  toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_ON);
  toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_ON);
  toggle_filter_button(sp, DEFAULT_FILTER_CONTROL_ON);
  toggle_direction_arrow(sp, false);
  set_amp(sp, DEFAULT_AMP_CONTROL);
  set_speed(sp, DEFAULT_SPEED_CONTROL);
  set_contrast(sp, DEFAULT_CONTRAST_CONTROL);
  set_expand(sp, DEFAULT_EXPAND_CONTROL);
  set_revscl(sp, DEFAULT_REVERB_CONTROL_SCALE);
  set_revlen(sp, DEFAULT_REVERB_CONTROL_LENGTH);
  set_filter_order(sp, filter_control_order(ss));
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  tmpstr = env_to_string(sp->filter_control_envelope);
  set_filter_text(sp, tmpstr);
  display_filter_env(sp);
  if (tmpstr) free(tmpstr);

}


static void apply_unset_controls(snd_info *sp) 
{
  /* after apply_controls there's no need to clear everything! */
  toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_ON);
  toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_ON);
  toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_ON);
  toggle_filter_button(sp, DEFAULT_FILTER_CONTROL_ON);
  toggle_direction_arrow(sp, false);
  set_amp(sp, DEFAULT_AMP_CONTROL);
  set_speed(sp, DEFAULT_SPEED_CONTROL);
}


void set_show_controls(bool val)
{
  in_set_show_controls(ss, val);
#if (!USE_NO_GUI)
  if (in_show_controls(ss))
    show_all_controls();
  else hide_all_controls(); 
#endif
}



/* ---------------- control panel apply ---------------- */

void stop_applying(snd_info *sp)
{
  /* called if C-g during the apply process */
  sp->apply_ok = false;
}

typedef struct {
  int slice;
  snd_info *sp;
  mus_long_t i;
  int ofd;
  char *ofile;
  ctrl_state *cs;
  file_info *hdr;
  char *origin;
} apply_state;


static Xen after_apply_controls_hook;

static void *make_apply_state(snd_info *sp)
{
  /* set up initial state for apply_controls */
  apply_state *ap;
  ap = (apply_state *)calloc(1, sizeof(apply_state));
  ap->slice = 0;
  ap->hdr = NULL;
  ap->sp = sp;
  return((void *)ap);
}


static apply_state *free_apply_state(apply_state *ap)
{
  if (ap)
    {
      if (ap->ofile) {free(ap->ofile); ap->ofile = NULL;}
      if (ap->origin) {free(ap->origin); ap->origin = NULL;}
      ap->hdr = free_file_info(ap->hdr);
      free(ap);
    }
  return(NULL);
}


static mus_long_t apply_dur = 0, orig_dur, apply_beg = 0;

static bool apply_controls(apply_state *ap)
{
  snd_info *sp;
  chan_info *cp = NULL;
  sync_info *si;
  mus_float_t mult_dur;
  int i, added_dur = 0;

  if (!ap) return(false);
  sp = ap->sp;
  if ((!(sp->active)) || (sp->inuse != SOUND_NORMAL)) return(false);

  if (sp->filter_control_on) 
    added_dur = sp->filter_control_order;
  mult_dur = 1.0 / fabs(sp->speed_control);
  if (sp->expand_control_on) 
    mult_dur *= sp->expand_control;
  if (sp->reverb_control_on) 
    added_dur += (int)((snd_srate(sp) * sp->reverb_control_decay));

  if ((ss->apply_choice != APPLY_TO_SELECTION) &&
      (snd_feq(sp->speed_control, 1.0)) && 
      (apply_beg == 0) &&
      (sp->speed_control_direction == 1) &&
      (!(sp->filter_control_on)) && (!(sp->expand_control_on)) && (!(sp->reverb_control_on)) && (!(sp->contrast_control_on)))
    {
      int old_sync;
      bool need_scaling = false;
      mus_float_t *scalers = NULL;

      old_sync = sp->sync;
      /* get unused sync val */
      if (ss->apply_choice == APPLY_TO_SOUND)
	{
	  sp->sync = ss->sound_sync_max + 1;
	  ss->sound_sync_max++;
	}
      else sp->sync = 0;

      /* check for local amp_control vals */
      if (sp->selected_channel == NO_SELECTION) 
	cp = sp->chans[0];
      else cp = sp->chans[sp->selected_channel];
      si = sync_to_chan(cp);
      if (!si)
	{
	  sp->sync = old_sync;
	  return(false);
	}

      scalers = (mus_float_t *)calloc(si->chans, sizeof(mus_float_t));
      for (i = 0; i < si->chans; i++)
	{
	  chan_info *ncp;
	  ncp = si->cps[i];
	  if (ncp->amp_control)
	    scalers[i] = ncp->amp_control[0];
	  else scalers[i] = sp->amp_control;
	  if (!(snd_feq(scalers[i], 1.0))) need_scaling = true; /* could possibly check all edit_ctrs, but this seems easier */
	}

      if (need_scaling)
	scale_by(cp, scalers, si->chans, false);
      else snd_warning_without_format("apply controls: no changes to apply!");

      sp->sync = old_sync;
      free(scalers);
      free_sync_info(si);
    }
  else
    {
      mus_long_t orig_apply_dur;
      io_error_t io_err = IO_NO_ERROR;
      int curchan = 0;

      orig_apply_dur = apply_dur;

      switch (ap->slice)
	{
	case 0:
	  /* apply_beg = 0; */
	  ap->ofile = NULL;
	  ap->ofile = snd_tempnam();
	  ap->hdr = make_temp_header(ap->ofile, snd_srate(sp), sp->nchans, 0, (char *)__func__);

	  switch (ss->apply_choice)
	    {
	    case APPLY_TO_CHANNEL:   
	      ap->hdr->chans = 1; 
	      if (sp->selected_channel != NO_SELECTION) 
		curchan = sp->selected_channel;
	      if (apply_dur == 0)
		apply_dur = current_samples(sp->chans[curchan]) - apply_beg;
	      break;

	    case APPLY_TO_SOUND:     
	      ap->hdr->chans = sp->nchans; 
	      if (apply_dur == 0)
		apply_dur = current_samples(sp->chans[0]) - apply_beg;
	      break;

	    case APPLY_TO_SELECTION: 
	      ap->hdr->chans = selection_chans();
	      if (ap->hdr->chans <= 0) return(false);
	      if (apply_dur == 0)
		apply_dur = selection_len(); 
	      break;
	    }

	  if (!ap->origin)
	    {
	      /* from apply-controls */
	      /* to reproduce this on a channel-independent basis, we need to use controls->channel
	       *   and conjure up a list of settings that match the current ones.
	       */
	      char *ampstr, *speedstr, *contraststr, *expandstr, *filterstr, *reverbstr;
	      if (sp->amp_control != DEFAULT_AMP_CONTROL)
		ampstr = mus_format("%.4f", 
				    sp->amp_control);
	      else ampstr = mus_strdup(PROC_FALSE);
	      if ((!(snd_feq(sp->speed_control, DEFAULT_SPEED_CONTROL))) || 
		  (sp->speed_control_direction == -1))
		speedstr = mus_format("%.4f", 
				      sp->speed_control * sp->speed_control_direction);
	      else speedstr = mus_strdup(PROC_FALSE);
	      if (sp->contrast_control_on)
		contraststr = mus_format(LIST_OPEN "%.4f" PROC_SEP "%.4f" LIST_CLOSE, 
					 sp->contrast_control, sp->contrast_control_amp);
	      else contraststr = mus_strdup(PROC_FALSE);
	      if (sp->expand_control_on)
		expandstr = mus_format(LIST_OPEN "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" LIST_CLOSE,
				       sp->expand_control, sp->expand_control_length, sp->expand_control_ramp, 
				       sp->expand_control_hop, sp->expand_control_jitter);
	      else expandstr = mus_strdup(PROC_FALSE);
	      if (sp->reverb_control_on)
		reverbstr = mus_format(LIST_OPEN "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" LIST_CLOSE,
				       sp->reverb_control_scale, sp->reverb_control_length, sp->reverb_control_feedback, 
				       sp->reverb_control_lowpass, sp->reverb_control_decay);
	      else reverbstr = mus_strdup(PROC_FALSE);
	      if (sp->filter_control_on)
		{
		  char *envstr;
		  envstr = env_to_string(sp->filter_control_envelope);
		  filterstr = mus_format(LIST_OPEN "%d" PROC_SEP "%s" LIST_CLOSE, 
					 sp->filter_control_order, envstr);
		  free(envstr);
		}
	      else filterstr = mus_strdup(PROC_FALSE);
#if HAVE_FORTH
	      if (orig_apply_dur == 0)
	      ap->origin = mus_format(" '( %s %s %s %s %s %s ) %" print_mus_long PROC_SEP PROC_FALSE " %s", 
				      ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr, 
				      apply_beg, S_controls_to_channel);
	      else ap->origin = mus_format(" '( %s %s %s %s %s %s ) %" print_mus_long PROC_SEP "%" print_mus_long " %s",
					   ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr,
					   apply_beg, apply_dur, S_controls_to_channel);
#else
	      if (orig_apply_dur == 0)
	      ap->origin = mus_format("%s" PROC_OPEN LIST_OPEN "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" LIST_CLOSE PROC_SEP "%" print_mus_long PROC_SEP PROC_FALSE, 
				      to_proc_name(S_controls_to_channel),
				      ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr, 
				      apply_beg);
	      else ap->origin = mus_format("%s" PROC_OPEN LIST_OPEN "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" LIST_CLOSE PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long,
					   to_proc_name(S_controls_to_channel),
					   ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr,
					   apply_beg, apply_dur);
#endif
	      free(ampstr);
	      free(speedstr);
	      free(contraststr);
	      free(expandstr);
	      free(reverbstr);
	      free(filterstr);
	    }

	  orig_dur = apply_dur;
	  apply_dur = (mus_long_t)(mult_dur * (apply_dur + added_dur));
	  ap->ofd = open_temp_file(ap->ofile, ap->hdr->chans, ap->hdr, &io_err);

	  if (ap->ofd == -1)
	    {
	      snd_error("%s apply temp file %s: %s\n", 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			ap->ofile, 
			snd_open_strerror());
	      sp->applying = false;
	      free_apply_state(ap);
	      return(false);
	    }

	  sp->apply_ok = true;
	  initialize_apply(sp, ap->hdr->chans, apply_beg, orig_dur + added_dur); /* dur here is input dur */
	  ap->i = 0;
	  ap->slice++;
	  return(true);
	  
	case 1:
	  if (!(sp->apply_ok))
	    ap->slice++;
	  else
	    {
	      int len;
	      len = run_apply(ap->ofd); /* returns framples written (an int) */
	      if (len <= 0)
		{
		  ap->slice++;
		  return(true);
		}
	      ap->i += len;
	      if (ap->i >= apply_dur) ap->slice++;
	      /* check_for_event(); */
	      /* if C-G, stop_applying called which cancels and backs out */
	      if ((ss->stopped_explicitly) || (!(sp->active)))
		ap->slice++;
	    }
	  return(true);
	  
	case 2:
	  finalize_apply(sp);
	  close_temp_file(ap->ofile,
			  ap->ofd,
			  ap->hdr->type,
			  apply_dur * (ap->hdr->chans) * mus_bytes_per_sample((ap->hdr)->sample_type));
	  if ((sp->apply_ok) && (apply_dur > 0))
	    {
	      switch (ss->apply_choice)
		{
		case APPLY_TO_SOUND:
		  if (sp->nchans > 1) 
		    remember_temp(ap->ofile, sp->nchans);
		  if (apply_beg > 0)
		    {
		      for (i = 0; i < (int)sp->nchans; i++)
			{
			  if (file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[i], i,
						  (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  ap->origin, sp->chans[i]->edit_ctr))
			    update_graph(sp->chans[i]);
			}
		    }
		  else
		    {
		      for (i = 0; i < (int)sp->nchans; i++)
			{
			  if (file_override_samples(apply_dur, ap->ofile, sp->chans[i], i,
						    (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						    ap->origin))
			    update_graph(sp->chans[i]);
			}
		    }
		  break;

		case APPLY_TO_CHANNEL: 
		  if (sp->selected_channel != NO_SELECTION) 
		    curchan = sp->selected_channel;
		  if (apply_beg > 0)
		    file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[curchan], 0, 
					DELETE_ME, ap->origin, sp->chans[curchan]->edit_ctr);
		  else file_override_samples(apply_dur, ap->ofile, sp->chans[curchan], 0, 
					     DELETE_ME, ap->origin);
		  update_graph(sp->chans[curchan]);
		  break;

		case APPLY_TO_SELECTION:
		  if (selection_chans() > 1) 
		    remember_temp(ap->ofile, selection_chans());
		  si = selection_sync();
		  if (apply_dur == selection_len())
		    {
		      for (i = 0; i < si->chans; i++)
			{
			  if (file_change_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], i,
						  (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  ap->origin, si->cps[i]->edit_ctr))
			    update_graph(si->cps[i]);
			}
		    }
		  else
		    {
		      bool ok;
		      ok = delete_selection(DONT_UPDATE_DISPLAY);
		      if (apply_dur > 0)
			{
			  for (i = 0; i < si->chans; i++)
			    {
			      file_insert_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], 0, 
						  (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  ap->origin, si->cps[i]->edit_ctr);
			      reactivate_selection(si->cps[i], si->begs[i], si->begs[i] + apply_dur);
			      if (ok) backup_edit_list(si->cps[i]);
			    }
			}
		    }
		  free_sync_info(si); 
		  break;
		}
	      clear_status_area(sp);
	      sp->apply_ok = false;
	      
	      if ((sp->expand_control_on) || 
		  (sp->speed_control_direction != 1) || (!(snd_feq(sp->speed_control, 1.0))))
		{
		  for (i = 0; i < (int)sp->nchans; i++)
		    {
		      cp = sp->chans[i];
		      if (cp->edits[cp->edit_ctr]->marks)
			{
			  mus_float_t ratio;
			  if (!(sp->expand_control_on))
			    ratio = sp->speed_control;
			  else ratio = sp->speed_control / sp->expand_control;
			  if (ratio != 1.0)
			    {
			      bool over_selection;
			      over_selection = (ss->apply_choice == APPLY_TO_SELECTION);
			      src_marks(cp, ratio, orig_dur, apply_dur, 
					(over_selection) ? selection_beg(cp) : 0,
					over_selection);
			      update_graph(cp);
			    }
			}
		    }
		}
	    }
	  else
	    {
	      snd_remove(ap->ofile, REMOVE_FROM_CACHE);
	    }
	  break;
	}
    }

  apply_unset_controls(sp);

  if (Xen_hook_has_list(after_apply_controls_hook))
    run_hook(after_apply_controls_hook, 
	     Xen_list_1(C_int_to_Xen_sound(sp->index)),
	     S_after_apply_controls_hook);

  sp->applying = false;
  free_apply_state(ap);
  ss->stopped_explicitly = false;
  return(false);
}


void expand_control_set_hop(mus_float_t hop)
{
  int i;
  in_set_expand_control_hop(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_hop = expand_control_hop(ss);
	  if (sp->playing) dac_set_expand_hop(sp, expand_control_hop(ss));
	}
    }
}

void expand_control_set_length(mus_float_t hop)
{
  int i;
  in_set_expand_control_length(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_length = expand_control_length(ss);
	  if (sp->playing) dac_set_expand_length(sp, expand_control_length(ss));
	}
    }
}

void expand_control_set_ramp(mus_float_t hop)
{
  int i;
  in_set_expand_control_ramp(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_ramp = expand_control_ramp(ss);
	  if (sp->playing) dac_set_expand_ramp(sp, expand_control_ramp(ss));
	}
    }
}

void expand_control_set_jitter(mus_float_t hop)
{
  int i;
  in_set_expand_control_jitter(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_jitter = expand_control_jitter(ss);
	}
    }
}

void contrast_control_set_amp(mus_float_t hop)
{
  int i;
  in_set_contrast_control_amp(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->contrast_control_amp = contrast_control_amp(ss);
	  if (sp->playing) dac_set_contrast_amp(sp, contrast_control_amp(ss));
	}
    }
}

void reverb_control_set_lowpass(mus_float_t hop)
{
  int i;
  in_set_reverb_control_lowpass(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->reverb_control_lowpass = reverb_control_lowpass(ss);
	  if (sp->playing) dac_set_reverb_lowpass(sp, reverb_control_lowpass(ss));
	}
    }
}

void reverb_control_set_feedback(mus_float_t hop)
{
  int i;
  in_set_reverb_control_feedback(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->reverb_control_feedback = reverb_control_feedback(ss);
	  if (sp->playing) dac_set_reverb_feedback(sp, reverb_control_feedback(ss));
	}
    }
}



/* ---------------- status area ---------------- 
 */

void status_report(snd_info *sp, const char *format, ...)
{
#if (!USE_NO_GUI)
  char *buf;
  va_list ap;
  if ((!sp) || (!(sp->active)) || (sp->inuse != SOUND_NORMAL)) return;
  va_start(ap, format);
  buf = vstr(format, ap);
  va_end(ap);
  set_status(sp, buf, false);
  free(buf);
#endif
}


void clear_status_area(snd_info *sp)
{
  set_status(sp, NULL, true);
}


void errors_to_status_area(const char *msg, void *data)
{
  snd_info *sp;
  sp = (snd_info *)data;
  if (!(snd_ok(sp)))
    {
      sp = any_selected_sound();
      if (!snd_ok(sp)) return;
    }
  status_report((snd_info *)data, "%s", msg);
}


void printout_to_status_area(const char *msg, void *data)
{
  set_status((snd_info *)data, msg, false);
}





/* ---------------------------------------- sound objects ---------------------------------------- */

typedef struct {
  int n;
} xen_sound;


#define Xen_to_xen_sound(arg) ((xen_sound *)Xen_object_ref(arg))

int xen_sound_to_int(Xen n)
{
  xen_sound *mx;
  mx = Xen_to_xen_sound(n);
  return(mx->n);
}


static Xen_object_type_t xen_sound_tag;

bool xen_is_sound(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_sound_tag));
}

#if (!HAVE_SCHEME)
static void xen_sound_free(xen_sound *v) {if (v) free(v);}

Xen_wrap_free(xen_sound, free_xen_sound, xen_sound_free)
#else
static s7_pointer s7_xen_sound_free(s7_scheme *sc, s7_pointer obj)
{
  xen_sound *v;
  v = (xen_sound *)s7_c_object_value(obj);
  if (v) free(v);
  return(NULL);
}
#endif


static char *xen_sound_to_string(xen_sound *v)
{
  #define SOUND_PRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(SOUND_PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, SOUND_PRINT_BUFFER_SIZE, "#<sound %d>", v->n);
  return(buf);
}


#if HAVE_FORTH || HAVE_RUBY
Xen_wrap_print(xen_sound, print_xen_sound, xen_sound_to_string)

static Xen g_xen_sound_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_sound_to_string "sound->string"
  Xen_check_type(xen_is_sound(obj), obj, 1, S_xen_sound_to_string, "a sound");
  vstr = xen_sound_to_string(Xen_to_xen_sound(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#else
#if HAVE_SCHEME
static s7_pointer g_xen_sound_to_string(s7_scheme *sc, s7_pointer args)
{
  char *vstr;
  Xen result;
  vstr = xen_sound_to_string(Xen_to_xen_sound(s7_car(args)));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif
#endif


#if (!HAVE_SCHEME)
static bool xen_sound_equalp(xen_sound *v1, xen_sound *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static Xen equalp_xen_sound(Xen obj1, Xen obj2)
{
  if ((!(xen_is_sound(obj1))) || (!(xen_is_sound(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_sound_equalp(Xen_to_xen_sound(obj1), Xen_to_xen_sound(obj2))));
}
#endif


static xen_sound *xen_sound_make(int n)
{
  xen_sound *new_v;
  new_v = (xen_sound *)malloc(sizeof(xen_sound));
  new_v->n = n;
  return(new_v);
}


Xen new_xen_sound(int n)
{
  xen_sound *mx;
  if (n < 0)
    return(Xen_false);

  mx = xen_sound_make(n);
  return(Xen_make_object(xen_sound_tag, mx, 0, free_xen_sound));
}


#if HAVE_SCHEME
static s7_pointer s7_xen_sound_is_equal(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p1, p2;
  p1 = s7_car(args);
  p2 = s7_cadr(args);
  if (p1 == p2) return(s7_t(sc));
  if (s7_c_object_type(p2) == xen_sound_tag)
    return(s7_make_boolean(sc, (((xen_sound *)s7_c_object_value(p1))->n == ((xen_sound *)s7_c_object_value(p2))->n)));
  return(s7_f(sc));
}


static Xen s7_xen_sound_length(s7_scheme *sc, Xen args)
{
  return(g_framples(s7_car(args), Xen_integer_zero, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION)));
}


static Xen s7_xen_sound_copy(s7_scheme *sc, Xen args)
{
  snd_info *sp;
  s7_pointer obj;
  obj = s7_car(args);
  sp = get_sp(obj);
  if (sp)
    {
      io_error_t err;
      char *name;
      name = snd_tempnam();
      if (mus_header_writable(sp->hdr->type, sp->hdr->sample_type))
	err = save_edits_without_display(sp, name, sp->hdr->type, sp->hdr->sample_type, sp->hdr->srate, NULL, AT_CURRENT_EDIT_POSITION);
      else err = save_edits_without_display(sp, name, MUS_NEXT, MUS_OUT_SAMPLE_TYPE, sp->hdr->srate, NULL, AT_CURRENT_EDIT_POSITION);
      sp = snd_open_file(name, FILE_READ_WRITE);
      free(name);
      if (sp)
	return(new_xen_sound(sp->index));
      if (is_serious_io_error(err))
	Xen_error(Xen_make_error_type("IO-error"),
		  Xen_list_2(C_string_to_Xen_string("copy sound: can't save edits, ~A"),
			     C_string_to_Xen_string(io_error_name(err))));
    }
  return(Xen_false);
}


static Xen s7_xen_sound_fill(s7_scheme *sc, Xen args)
{
  snd_info *sp;
  s7_pointer obj;

  obj = s7_car(args);
  sp = get_sp(obj);
  if (sp)
    {
      mus_float_t valf;
      chan_info *cp;
      uint32_t i;
      s7_pointer val;
      
      val = s7_cadr(args);
      valf = Xen_real_to_C_double(val);
      if (valf == 0.0)
	{
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      scale_channel(cp, 0.0, 0, current_samples(cp), cp->edit_ctr, false);
	      update_graph(cp);
	    }
	}
      else
	{
	  /* this was #if (!HAVE_SCHEME) which makes no sense -- I think it meant (!HAVE_RUN)
	   *   but that means (fill! <sound>) fails if optimization is off.
	   */
	  mus_long_t len = -1, j;
	  mus_float_t *data = NULL;
	  
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      if ((!data) || (current_samples(cp) != len))
		{
		  len = current_samples(cp);
		  if (data) free(data);
		  data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
		  for (j = 0; j < len; j++)
		    data[j] = valf;
		}
	      if (change_samples(0, len, data, cp, "fill! sound", cp->edit_ctr, fabs(valf)))
		update_graph(cp);
	    }
	  free(data);
	}
    }
  return(Xen_false);
}
#endif


static void init_xen_sound(void)
{
#if HAVE_SCHEME
  xen_sound_tag = s7_make_c_type(s7, "<sound>");
  s7_c_type_set_gc_free(s7, xen_sound_tag, s7_xen_sound_free);
  s7_c_type_set_is_equal(s7, xen_sound_tag, s7_xen_sound_is_equal);
  s7_c_type_set_length(s7, xen_sound_tag, s7_xen_sound_length);
  s7_c_type_set_copy(s7, xen_sound_tag, s7_xen_sound_copy);
  s7_c_type_set_fill(s7, xen_sound_tag, s7_xen_sound_fill);
  s7_c_type_set_to_string(s7, xen_sound_tag, g_xen_sound_to_string);
#else
#if HAVE_RUBY
  xen_sound_tag = Xen_make_object_type("XenSound", sizeof(xen_sound));
#else
  xen_sound_tag = Xen_make_object_type("Sound", sizeof(xen_sound));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_sound_tag,   print_xen_sound);
  fth_set_object_dump(xen_sound_tag,      g_xen_sound_to_string);
  fth_set_object_equal(xen_sound_tag,     equalp_xen_sound);
  fth_set_object_free(xen_sound_tag,      free_xen_sound);
#endif

#if HAVE_RUBY
  rb_define_method(xen_sound_tag, "to_s",     Xen_procedure_cast print_xen_sound, 0);
  rb_define_method(xen_sound_tag, "eql?",     Xen_procedure_cast equalp_xen_sound, 1);
  rb_define_method(xen_sound_tag, "==",       Xen_procedure_cast equalp_xen_sound, 1);
  rb_define_method(xen_sound_tag, "to_str",   Xen_procedure_cast g_xen_sound_to_string, 0);
#endif
}

/* -------------------------------------------------------------------------------- */

static Xen g_integer_to_sound(Xen n)
{
  #define H_integer_to_sound "(" S_integer_to_sound " n) returns a sound object corresponding to the given integer"
  int index;
  Xen_check_type(Xen_is_integer(n), n, 1, S_integer_to_sound, "an integer");
  index = Xen_integer_to_C_int(n);
  if (get_sp_1(index))
    return(new_xen_sound(index));
  return(Xen_false);
}


static Xen g_sound_to_integer(Xen n)
{
  #define H_sound_to_integer "(" S_sound_to_integer " id) returns the integer corresponding to the given sound"
  Xen_check_type(xen_is_sound(n), n, 1, S_sound_to_integer, "a sound");
  return(C_int_to_Xen_integer(xen_sound_to_int(n)));
}


Xen snd_no_such_sound_error(const char *caller, Xen n)
{
  Xen_error(Xen_make_error_type("no-such-sound"),
	    Xen_list_3(C_string_to_Xen_string("~A: no such sound: ~A"),
		       C_string_to_Xen_string(caller),
		       n));
  return(Xen_false);
}


static Xen g_is_sound(Xen snd)
{
  #define H_is_sound "(" S_is_sound " snd): " PROC_TRUE " if 'snd' (a sound object or an integer) is an active (accessible) sound"

  if (Xen_is_integer(snd) || xen_is_sound(snd))
    {
      snd_info *sp;
      sp = get_sp(snd);
      return(C_bool_to_Xen_boolean((sp) && 
				   (snd_ok(sp)) &&
				   (sp->inuse == SOUND_NORMAL)));
    }
  return(Xen_false);
}


static Xen g_select_sound(Xen snd)
{
  #define H_select_sound "(" S_select_sound " snd): make sound 'snd' (a sound object or an index) the default sound for \
any editing operations."
  snd_info *sp;

  Xen_check_type(Xen_is_integer(snd) || xen_is_sound(snd), snd, 1, S_select_sound, "a sound object or index");

  sp = get_sp(snd);
  if (sp)
    {
      select_channel(sp, 0);
      return(snd);
    }

  return(snd_no_such_sound_error(S_select_sound, snd));
}


static Xen g_select_channel(Xen chn_n)
{
  #define H_select_channel "(" S_select_channel " :optional (chn 0)): make channel 'chn' of the currently selected sound the default \
channel for editing."
  snd_info *sp;
  int chan = 0;

  Snd_assert_sound(S_select_channel, chn_n, 1);
  if (Xen_is_integer(chn_n)) chan = Xen_integer_to_C_int(chn_n);

  sp = any_selected_sound();
  if ((sp) && 
      (chan >= 0) &&
      (chan < (int)sp->nchans)) 
    {
      select_channel(sp, chan);
      return(chn_n);
    }

  return(snd_no_such_channel_error(S_select_channel, C_string_to_Xen_string(S_selected_sound), chn_n));
}


static Xen g_find_sound(Xen filename, Xen which)
{
  #define H_find_sound "(" S_find_sound " name :optional (nth 0)): return the sound associated with file 'name'. \
If more than one such sound exists, 'nth' chooses which one to return."
  snd_info *sp;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_find_sound, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(which), which, 2, S_find_sound, "an integer");

  sp = find_sound(Xen_string_to_C_string(filename), (Xen_is_integer(which)) ? Xen_integer_to_C_int(which) : 0);
  if (sp) return(C_int_to_Xen_sound(sp->index));

  return(Xen_false);
}


typedef enum {SP_SYNC, SP_READ_ONLY, SP_NCHANS, SP_CONTRASTING, SP_EXPANDING, SP_REVERBING, SP_FILTERING, SP_FILTER_ORDER,
	      SP_SRATE, SP_SAMPLE_TYPE, SP_DATA_LOCATION, SP_HEADER_TYPE, SP_SAVE_CONTROLS, SP_RESTORE_CONTROLS, SP_SELECTED_CHANNEL,
	      SP_COMMENT, SP_FILE_NAME, SP_SHORT_FILE_NAME, SP_CLOSE, SP_UPDATE, SP_SHOW_CONTROLS,
	      SP_FILTER_DBING, SP_SPEED_TONES, SP_SPEED_STYLE, SP_RESET_CONTROLS,
	      SP_AMP, SP_CONTRAST, SP_CONTRAST_AMP, SP_EXPAND, SP_EXPAND_LENGTH, SP_EXPAND_RAMP, SP_EXPAND_HOP,
	      SP_SPEED, SP_REVERB_LENGTH, SP_REVERB_FEEDBACK, SP_REVERB_SCALE, SP_REVERB_LOW_PASS,
	      SP_REVERB_DECAY, SP_PROPERTIES, SP_FILTER_COEFFS, SP_DATA_SIZE, SP_FILTER_HZING, SP_EXPAND_JITTER,
	      SP_CONTRAST_BOUNDS, SP_AMP_BOUNDS, SP_SPEED_BOUNDS, SP_EXPAND_BOUNDS, SP_REVERB_LENGTH_BOUNDS, SP_REVERB_SCALE_BOUNDS,
	      SP_FILTER_ENVELOPE
} sp_field_t;


static Xen sound_get(Xen snd, sp_field_t fld, const char *caller)
{
  snd_info *sp;
  Xen res = Xen_empty_list;

  if (Xen_is_true(snd))
    {
      int i;
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL))
	    res = Xen_cons(sound_get(C_int_to_Xen_integer(i), fld, caller), res);
	}
      return(res);
    }

  if (xen_is_player(snd))
    {
      sp = get_player_sound(snd);
      if (!sp)
	return(no_such_player_error(caller, snd));
    }
  else
    {
      Snd_assert_sound(caller, snd, 1);
      sp = get_sp(snd);
      if (!sp)
	return(snd_no_such_sound_error(caller, snd));
    }
  if ((!sp) || 
      (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(caller, snd));

  switch (fld)
    {
    case SP_SYNC:                return(C_int_to_Xen_integer(sp->sync));                             
    case SP_READ_ONLY:           return(C_bool_to_Xen_boolean(sp->user_read_only == FILE_READ_ONLY));
    case SP_NCHANS:              return(C_int_to_Xen_integer(sp->nchans));                           
    case SP_EXPANDING:           return(C_bool_to_Xen_boolean(sp->expand_control_on));               
    case SP_CONTRASTING:         return(C_bool_to_Xen_boolean(sp->contrast_control_on));             
    case SP_REVERBING:           return(C_bool_to_Xen_boolean(sp->reverb_control_on));               
    case SP_FILTERING:           return(C_bool_to_Xen_boolean(sp->filter_control_on));               
    case SP_FILTER_DBING:        return(C_bool_to_Xen_boolean(sp->filter_control_in_dB));            
    case SP_FILTER_HZING:        return(C_bool_to_Xen_boolean(sp->filter_control_in_hz));            
    case SP_FILTER_ORDER:        return(C_int_to_Xen_integer(sp->filter_control_order));             
    case SP_SRATE:               return(C_int_to_Xen_integer(sp->hdr->srate));                       
    case SP_SAMPLE_TYPE:         return(C_int_to_Xen_integer(sp->hdr->sample_type));                 
    case SP_HEADER_TYPE:         return(C_int_to_Xen_integer(sp->hdr->type));                        
    case SP_DATA_LOCATION:       return(C_llong_to_Xen_llong(sp->hdr->data_location));               
    case SP_DATA_SIZE:           return(C_llong_to_Xen_llong(mus_samples_to_bytes(sp->hdr->sample_type, sp->hdr->samples)));
    case SP_SAVE_CONTROLS:       if (has_widgets(sp)) save_controls(sp);     break;
    case SP_RESTORE_CONTROLS:    if (has_widgets(sp)) restore_controls(sp);  break;
    case SP_RESET_CONTROLS:      if (has_widgets(sp)) reset_controls(sp);    break;
    case SP_FILE_NAME:           return(C_string_to_Xen_string(sp->filename));                    
    case SP_SHORT_FILE_NAME:     return(C_string_to_Xen_string(sp->short_filename));              
    case SP_CLOSE:               if (!(is_player_sound(sp))) snd_close_file(sp); break;
    case SP_SHOW_CONTROLS:       if (has_widgets(sp)) return(C_bool_to_Xen_boolean(showing_controls(sp))); break;
    case SP_SPEED_TONES:         return(C_int_to_Xen_integer(sp->speed_control_tones));           
    case SP_SPEED_STYLE:         return(C_int_to_Xen_integer((int)(sp->speed_control_style)));    
    case SP_COMMENT:             return(C_string_to_Xen_string(sp->hdr->comment));                
    case SP_AMP:                 return(C_double_to_Xen_real(sp->amp_control));                   
    case SP_CONTRAST:            return(C_double_to_Xen_real(sp->contrast_control));              
    case SP_CONTRAST_AMP:        return(C_double_to_Xen_real(sp->contrast_control_amp));          
    case SP_EXPAND:              return(C_double_to_Xen_real(sp->expand_control));                
    case SP_EXPAND_LENGTH:       return(C_double_to_Xen_real(sp->expand_control_length));         
    case SP_EXPAND_RAMP:         return(C_double_to_Xen_real(sp->expand_control_ramp));           
    case SP_EXPAND_HOP:          return(C_double_to_Xen_real(sp->expand_control_hop));            
    case SP_EXPAND_JITTER:       return(C_double_to_Xen_real(sp->expand_control_jitter));         
    case SP_REVERB_LENGTH:       return(C_double_to_Xen_real(sp->reverb_control_length));         
    case SP_REVERB_FEEDBACK:     return(C_double_to_Xen_real(sp->reverb_control_feedback));       
    case SP_REVERB_SCALE:        return(C_double_to_Xen_real(sp->reverb_control_scale));          
    case SP_REVERB_LOW_PASS:     return(C_double_to_Xen_real(sp->reverb_control_lowpass));        
    case SP_REVERB_DECAY:        return(C_double_to_Xen_real(sp->reverb_control_decay));          

    case SP_AMP_BOUNDS:          
      return(Xen_list_2(C_double_to_Xen_real(sp->amp_control_min), C_double_to_Xen_real(sp->amp_control_max))); 

    case SP_CONTRAST_BOUNDS:     
      return(Xen_list_2(C_double_to_Xen_real(sp->contrast_control_min), C_double_to_Xen_real(sp->contrast_control_max))); 

    case SP_EXPAND_BOUNDS:       
      return(Xen_list_2(C_double_to_Xen_real(sp->expand_control_min), C_double_to_Xen_real(sp->expand_control_max))); 

    case SP_SPEED_BOUNDS:        
      return(Xen_list_2(C_double_to_Xen_real(sp->speed_control_min), C_double_to_Xen_real(sp->speed_control_max)));

    case SP_REVERB_LENGTH_BOUNDS: 
      return(Xen_list_2(C_double_to_Xen_real(sp->reverb_control_length_min), C_double_to_Xen_real(sp->reverb_control_length_max))); 

    case SP_REVERB_SCALE_BOUNDS: 
      return(Xen_list_2(C_double_to_Xen_real(sp->reverb_control_scale_min), C_double_to_Xen_real(sp->reverb_control_scale_max))); 

    case SP_SELECTED_CHANNEL:    
      if (sp->selected_channel != NO_SELECTION) 
	return(C_int_to_Xen_integer(sp->selected_channel));
      return(Xen_false); 

    case SP_UPDATE:              
      if (!(is_player_sound(sp)))
	{
	  mus_sound_forget(sp->filename); /* old record must be out-of-date, so flush it (write date can be troublesome) */
	  sp = snd_update_within_xen(sp, caller); 
	  if (sp) 
	    return(C_int_to_Xen_sound(sp->index));
	} 
      break;

    case SP_PROPERTIES:
      if (!(is_player_sound(sp)))
	{
	  if (!(Xen_is_vector(sp->properties)))
	    {
	      sp->properties = Xen_make_vector(1, Xen_empty_list);
	      sp->properties_loc = snd_protect(sp->properties);
	    }
	  return(Xen_vector_ref(sp->properties, 0));
	}
      break;

    case SP_SPEED:
#if XEN_HAVE_RATIOS
      if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	{
	  if (sp->speed_control_direction == -1)
	    return(Xen_make_ratio(C_int_to_Xen_integer(-sp->speed_control_numerator), C_int_to_Xen_integer(sp->speed_control_denominator)));
	  return(Xen_make_ratio(C_int_to_Xen_integer(sp->speed_control_numerator), C_int_to_Xen_integer(sp->speed_control_denominator)));
	}
#endif
      if (sp->speed_control_direction == -1) 
	return(C_double_to_Xen_real((-(sp->speed_control)))); 
      return(C_double_to_Xen_real(sp->speed_control)); 

    case SP_FILTER_COEFFS: 
      if (sp->filter_control_envelope)
	{
	  int len;
	  mus_float_t *coeffs, *data;
	  len = sp->filter_control_order;
	  coeffs = (mus_float_t *)calloc(len, len * sizeof(mus_float_t));
	  data = sample_linear_env(sp->filter_control_envelope, len);
	  mus_make_fir_coeffs(len, data, coeffs);
	  free(data);
	  return(xen_make_vct(len, coeffs));
	}
      break;

    case SP_FILTER_ENVELOPE:
      if (sp->filter_control_envelope)
	return(env_to_xen(sp->filter_control_envelope));
      break;
    }
  return(Xen_false);
}


static Xen sound_get_global(Xen snd, sp_field_t fld, const char *caller)
{
  if (!Xen_is_bound(snd))
    switch (fld)
      {
      case SP_FILTER_DBING:         return(C_bool_to_Xen_boolean(filter_control_in_dB(ss)));    
      case SP_FILTER_HZING:         return(C_bool_to_Xen_boolean(filter_control_in_hz(ss)));    
      case SP_FILTER_ORDER:         return(C_int_to_Xen_integer(filter_control_order(ss)));     
      case SP_SHOW_CONTROLS:        return(C_bool_to_Xen_boolean(in_show_controls(ss)));        
      case SP_SPEED_TONES:          return(C_int_to_Xen_integer(speed_control_tones(ss)));      
      case SP_SPEED_STYLE:          return(C_int_to_Xen_integer((int)(speed_control_style(ss))));
      case SP_CONTRAST_AMP:         return(C_double_to_Xen_real(contrast_control_amp(ss)));     
      case SP_EXPAND_LENGTH:        return(C_double_to_Xen_real(expand_control_length(ss)));    
      case SP_EXPAND_RAMP:          return(C_double_to_Xen_real(expand_control_ramp(ss)));      
      case SP_EXPAND_HOP:           return(C_double_to_Xen_real(expand_control_hop(ss)));       
      case SP_EXPAND_JITTER:        return(C_double_to_Xen_real(expand_control_jitter(ss)));    
      case SP_REVERB_FEEDBACK:      return(C_double_to_Xen_real(reverb_control_feedback(ss)));  
      case SP_REVERB_LOW_PASS:      return(C_double_to_Xen_real(reverb_control_lowpass(ss)));   
      case SP_REVERB_DECAY:         return(C_double_to_Xen_real(reverb_control_decay(ss)));     

      case SP_AMP_BOUNDS:           
	return(Xen_list_2(C_double_to_Xen_real(amp_control_min(ss)), C_double_to_Xen_real(amp_control_max(ss)))); 

      case SP_CONTRAST_BOUNDS:     
	return(Xen_list_2(C_double_to_Xen_real(contrast_control_min(ss)), C_double_to_Xen_real(contrast_control_max(ss))));

      case SP_EXPAND_BOUNDS:        
	return(Xen_list_2(C_double_to_Xen_real(expand_control_min(ss)), C_double_to_Xen_real(expand_control_max(ss)))); 

      case SP_SPEED_BOUNDS:         
	return(Xen_list_2(C_double_to_Xen_real(speed_control_min(ss)), C_double_to_Xen_real(speed_control_max(ss)))); 

      case SP_REVERB_LENGTH_BOUNDS: 
	return(Xen_list_2(C_double_to_Xen_real(reverb_control_length_min(ss)), C_double_to_Xen_real(reverb_control_length_max(ss)))); 

      case SP_REVERB_SCALE_BOUNDS:  
	return(Xen_list_2(C_double_to_Xen_real(reverb_control_scale_min(ss)), C_double_to_Xen_real(reverb_control_scale_max(ss)))); 

      default: 
	break;
      }
  return(sound_get(snd, fld, caller));
}


static Xen sound_set(Xen snd, Xen val, sp_field_t fld, const char *caller)
{
  snd_info *sp;
  int i, ival;
  mus_float_t fval;

  if (Xen_is_true(snd))
    {
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL))
	    sound_set(C_int_to_Xen_integer(i), val, fld, caller);
	}
      return(val);
    }

  if (xen_is_player(snd))
    {
      sp = get_player_sound(snd);
      if (!sp)
	return(no_such_player_error(caller, snd));
    }
  else
    {
      Snd_assert_sound(caller, snd, 1);
      sp = get_sp(snd);
      if (!sp)
	return(snd_no_such_sound_error(caller, snd));
    }
  if ((!sp) || 
      (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(caller, snd));

  switch (fld)
    {
    case SP_SYNC:  
      if (Xen_is_integer(val))
	syncb(sp, Xen_integer_to_C_int(val));
      else syncb(sp, (int)Xen_boolean_to_C_bool(val));
      break;

    case SP_READ_ONLY:
      if (has_widgets(sp))
	{
	  sp->user_read_only = (Xen_boolean_to_C_bool(val) ? FILE_READ_ONLY : FILE_READ_WRITE);
	  if ((sp->user_read_only == FILE_READ_ONLY) || 
	      (sp->file_read_only == FILE_READ_ONLY))
	    show_lock(sp); 
	  else hide_lock(sp);
	}
      break;

    case SP_EXPANDING:
      toggle_expand_button(sp, Xen_boolean_to_C_bool(val));
      break;

    case SP_CONTRASTING:
      toggle_contrast_button(sp, Xen_boolean_to_C_bool(val));
      break;

    case SP_REVERBING:
      toggle_reverb_button(sp, Xen_boolean_to_C_bool(val));
      break;

    case SP_FILTERING:
      toggle_filter_button(sp, Xen_boolean_to_C_bool(val));
      break;

    case SP_FILTER_DBING:   
      set_filter_in_dB(sp, Xen_boolean_to_C_bool(val));
      break;

    case SP_FILTER_HZING:   
      set_filter_in_hz(sp, Xen_boolean_to_C_bool(val));
      break;

    case SP_FILTER_ORDER:
      set_filter_order(sp, Xen_integer_to_C_int(val));
      break;

    case SP_SHOW_CONTROLS:
      if (has_widgets(sp)) 
	{
	  if (Xen_boolean_to_C_bool(val))
	    show_controls(sp); 
	  else hide_controls(sp); 
	}
      break;

    case SP_SPEED_TONES:
      sp->speed_control_tones = Xen_integer_to_C_int(val);
      if (sp->speed_control_tones <= 0) 
	sp->speed_control_tones = DEFAULT_SPEED_CONTROL_TONES;
      set_speed(sp, sp->speed_control); /* update label etc */
      break;

    case SP_SPEED_STYLE:
      sp->speed_control_style = (speed_style_t)Xen_integer_to_C_int(val); /* range checked already */
#if XEN_HAVE_RATIOS
      if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
      set_speed(sp, sp->speed_control); /* update label etc */
      break;

    case SP_SRATE:
      if (!(is_player_sound(sp))) 
	{
	  if (Xen_is_integer(val))
	    ival = Xen_integer_to_C_int(val);
	  else
	    {
	      if (Xen_is_double(val))
		ival = snd_round(Xen_real_to_C_double(val));
	      else ival = 44100;
	    }
	  if ((ival <= 0) || (ival > 100000000))
	    Xen_out_of_range_error(S_set S_srate, 1, val, "impossible srate");
	  mus_sound_set_srate(sp->filename, ival);
	  sp->hdr->srate = ival;
	  /* if there are pending edits, we certainly don't want to flush them in this case! */
	  if (!(has_unsaved_edits(sp)))
	    snd_update_within_xen(sp, caller); 
	  else 
	    {
	      /* reset x axis bounds */
	      uint32_t i;
	      for (i = 0; i < sp->nchans; i++)
		set_x_axis_x0x1(sp->chans[i], 0.0, (double)(current_samples(sp->chans[i])) / (double)ival);
	    }
	}
      break;

    case SP_NCHANS: 
      if (!(is_player_sound(sp))) 
	{
	  ival = Xen_integer_to_C_int(val);
	  if ((ival <= 0) || (ival > MUS_MAX_CHANS))
	    Xen_out_of_range_error(S_set S_channels, 1, val, "highly unlikely number of channels");
	  mus_sound_set_chans(sp->filename, ival);
	  sp->hdr->chans = ival;
	  snd_update_within_xen(sp, caller); 
	}
      break;

    case SP_SAMPLE_TYPE:
      if (!(is_player_sound(sp))) 
	{
	  mus_sample_t ival;
	  ival = (mus_sample_t)Xen_integer_to_C_int(val);
	  if (mus_is_sample_type(ival))
	    {
	      mus_sample_t old_format;
	      old_format = sp->hdr->sample_type;
	      mus_sound_set_sample_type(sp->filename, ival);
	      sp->hdr->sample_type = ival;
	      if (mus_bytes_per_sample(old_format) != mus_bytes_per_sample(ival))
		{
		  sp->hdr->samples = (sp->hdr->samples * mus_bytes_per_sample(old_format)) / mus_bytes_per_sample(ival);
		  mus_sound_set_samples(sp->filename, sp->hdr->samples);
		}
	      /* clear peak amp envs, if any -- is this right?  (snd-update below...) */
	      for (i = 0; i < (int)sp->nchans; i++)
		{
		  chan_info *cp;
		  cp = sp->chans[i];
		  if ((cp) && (cp->edits[cp->edit_ctr]->peak_env))
		    cp->edits[cp->edit_ctr]->peak_env = free_peak_env(cp, cp->edit_ctr);
		}
	      snd_update_within_xen(sp, caller);
	    }
	  else Xen_out_of_range_error(S_set S_sample_type, 1, val, "unknown sample type");
	}
      break;

    case SP_HEADER_TYPE:
      if (!(is_player_sound(sp))) 
	{
	  mus_header_t typ;
	  typ = (mus_header_t)Xen_integer_to_C_int(val);
	  if (mus_is_header_type(typ))
	    {
	      mus_sound_set_header_type(sp->filename, typ);
	      snd_update_within_xen(sp, caller); 
	    }
	  else Xen_out_of_range_error(S_set S_header_type, 1, val, "unknown header type");
	}
      break;

    case SP_DATA_LOCATION:  
      if (!(is_player_sound(sp))) 
	{
	  mus_long_t loc;
	  loc = Xen_llong_to_C_llong(val);
	  if (loc >= 0)
	    {
	      mus_sound_set_data_location(sp->filename, loc);
	      snd_update_within_xen(sp, caller); 
	    }
	  else Xen_out_of_range_error(S_set S_data_location, 1, val, "data location < 0?");
	}
      break;

    case SP_DATA_SIZE:  
      if (!(is_player_sound(sp))) 
	{
	  mus_long_t size;
	  size = Xen_llong_to_C_llong(val);
	  if (size >= 0)
	    {
	      mus_sound_set_samples(sp->filename, mus_bytes_to_samples(sp->hdr->sample_type, size));
	      snd_update_within_xen(sp, caller); 
	    }
	  else Xen_out_of_range_error(S_set S_data_size, 1, val, "data size < 0?");
	}
      break;

    case SP_COMMENT:
      if (!(is_player_sound(sp))) 
	{
	  if (sp->hdr->comment) free(sp->hdr->comment);
	  if (Xen_is_false(val))
	    sp->hdr->comment = NULL;
	  else sp->hdr->comment = mus_strdup(Xen_string_to_C_string(val));
	}
      break;

    case SP_PROPERTIES:
      if (!(is_player_sound(sp)))
	{
	  if (!(Xen_is_vector(sp->properties)))
	    {
	      sp->properties = Xen_make_vector(1, Xen_empty_list);
	      sp->properties_loc = snd_protect(sp->properties);
	    }
	  Xen_vector_set(sp->properties, 0, val);
	  return(Xen_vector_ref(sp->properties, 0));
	}
      break;

    case SP_AMP:           
      fval = Xen_real_to_C_double(val);
      if (fval >= 0.0) set_amp(sp, fval); 
      return(C_double_to_Xen_real(sp->amp_control)); 

    case SP_AMP_BOUNDS:
      sp->amp_control_min = Xen_real_to_C_double(Xen_car(val));
      sp->amp_control_max = Xen_real_to_C_double(Xen_cadr(val));
      set_amp(sp, mus_fclamp(sp->amp_control_min, sp->amp_control, sp->amp_control_max));
      return(val);

    case SP_CONTRAST:      
      set_contrast(sp, Xen_real_to_C_double(val));
      return(C_double_to_Xen_real(sp->contrast_control)); 

    case SP_CONTRAST_BOUNDS:
      sp->contrast_control_min = Xen_real_to_C_double(Xen_car(val));
      sp->contrast_control_max = Xen_real_to_C_double(Xen_cadr(val));
      set_contrast(sp, mus_fclamp(sp->contrast_control_min, sp->contrast_control, sp->contrast_control_max));
      return(val);

    case SP_CONTRAST_AMP:  
      sp->contrast_control_amp = Xen_real_to_C_double(val);
      if (sp->playing) dac_set_contrast_amp(sp, sp->contrast_control_amp);
      break;

    case SP_EXPAND:        
      fval = Xen_real_to_C_double(val);
      if (fval > 0.0) set_expand(sp, fval); 
      return(C_double_to_Xen_real(sp->expand_control)); 

    case SP_EXPAND_BOUNDS:
      sp->expand_control_min = Xen_real_to_C_double(Xen_car(val));
      sp->expand_control_max = Xen_real_to_C_double(Xen_cadr(val));
      set_expand(sp, mus_fclamp(sp->expand_control_min, sp->expand_control, sp->expand_control_max));
      return(val);

    case SP_EXPAND_LENGTH: 
      fval = Xen_real_to_C_double(val);
      if (fval > 0.0) 
	{
	  sp->expand_control_length = fval; 
	  if (sp->playing) dac_set_expand_length(sp, sp->expand_control_length);
	}
      else Xen_out_of_range_error(S_set S_expand_control_length, 1, val, "length <= 0.0?");
      return(C_double_to_Xen_real(sp->expand_control_length));

    case SP_EXPAND_RAMP:   
      fval = Xen_real_to_C_double(val);
      if ((fval >= 0.0) && (fval < 0.5)) 
	{
	  sp->expand_control_ramp = fval; 
	  if (sp->playing) dac_set_expand_ramp(sp, fval); 
	}
      return(C_double_to_Xen_real(sp->expand_control_ramp));

    case SP_EXPAND_HOP:    
      fval = Xen_real_to_C_double(val);
      if (fval > 0.0) 
	{
	  sp->expand_control_hop = fval; 
	  if (sp->playing) dac_set_expand_hop(sp, fval); 
	}
      else Xen_out_of_range_error(S_set S_expand_control_hop, 1, val, "hop <= 0.0?");
      return(C_double_to_Xen_real(sp->expand_control_hop));

    case SP_EXPAND_JITTER:    
      fval = mus_fclamp(0.0, Xen_real_to_C_double(val), 100.0);
      sp->expand_control_jitter = fval; 
      return(C_double_to_Xen_real(sp->expand_control_jitter));

    case SP_SPEED: 
#if XEN_HAVE_RATIOS
      if ((sp->speed_control_style == SPEED_CONTROL_AS_RATIO) &&
	  (Xen_is_ratio(val)))
	{
	  sp->speed_control_numerator = (int)Xen_numerator(val);
	  sp->speed_control_denominator = (int)Xen_denominator(val);
	  fval = (mus_float_t)(sp->speed_control_numerator) / (mus_float_t)(sp->speed_control_denominator);
	  if (sp->speed_control_numerator < 0)
	    {
	      sp->speed_control_direction = -1;
	      sp->speed_control_numerator = -sp->speed_control_numerator;
	    }
	  else sp->speed_control_direction = 1;
	  set_speed(sp, fabs(fval));
	  sp->speed_control = fabs(fval); /* not redundant */
	  toggle_direction_arrow(sp, (sp->speed_control_direction == -1));
	  return(val);
	}
#endif
      fval = Xen_real_to_C_double(val);
      if (fval != 0.0)
	{
	  int direction;
	  if (fval > 0.0) direction = 1; else direction = -1;
	  set_speed(sp, fabs(fval)); 
#if XEN_HAVE_RATIOS
	  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
	  toggle_direction_arrow(sp, (direction == -1));
	  if (sp->speed_control_direction == -1) 
	    return(C_double_to_Xen_real((-(sp->speed_control)))); 
	  else return(C_double_to_Xen_real(sp->speed_control));
	}
      break;

    case SP_SPEED_BOUNDS:
      sp->speed_control_min = Xen_real_to_C_double(Xen_car(val));
      sp->speed_control_max = Xen_real_to_C_double(Xen_cadr(val));
      set_speed(sp, mus_fclamp(sp->speed_control_min, sp->speed_control, sp->speed_control_max));
      return(val);

    case SP_REVERB_LENGTH:    
      fval = Xen_real_to_C_double(val);
      if (fval >= 0.0) set_revlen(sp, fval); 
      return(C_double_to_Xen_real(sp->reverb_control_length)); 

    case SP_REVERB_LENGTH_BOUNDS:
      sp->reverb_control_length_min = Xen_real_to_C_double(Xen_car(val));
      sp->reverb_control_length_max = Xen_real_to_C_double(Xen_cadr(val));
      set_revlen(sp, mus_fclamp(sp->reverb_control_length_min, sp->reverb_control_length, sp->reverb_control_length_max));
      return(val);

    case SP_REVERB_FEEDBACK:  
      sp->reverb_control_feedback = mus_fclamp(0.0, Xen_real_to_C_double(val), 100.0);
      if (sp->playing) dac_set_reverb_feedback(sp, sp->reverb_control_feedback);
      break;

    case SP_REVERB_SCALE:     
      set_revscl(sp, Xen_real_to_C_double(val));
      return(C_double_to_Xen_real(sp->reverb_control_scale)); 

    case SP_REVERB_SCALE_BOUNDS:
      sp->reverb_control_scale_min = Xen_real_to_C_double(Xen_car(val));
      sp->reverb_control_scale_max = Xen_real_to_C_double(Xen_cadr(val));
      set_revscl(sp, mus_fclamp(sp->reverb_control_scale_min, sp->reverb_control_scale, sp->reverb_control_scale_max));
      return(val);

    case SP_REVERB_LOW_PASS:  
      sp->reverb_control_lowpass = mus_fclamp(0.0, Xen_real_to_C_double(val), 1.0);
      if (sp->playing) dac_set_reverb_lowpass(sp, sp->reverb_control_lowpass);
      break;

    case SP_REVERB_DECAY:     
      sp->reverb_control_decay = Xen_real_to_C_double(val);
      break;

    case SP_FILTER_ENVELOPE:
      {
	env *e = NULL;
	if (sp->filter_control_envelope) 
	  sp->filter_control_envelope = free_env(sp->filter_control_envelope);  /* set to null in case get_env throws error */
	if (!(Xen_is_false(val)))
	  e = get_env(val, caller); /* has some error checks -- val must be list, but we can be #f -- see "get" case above: null env (nogui) -> #f */
	if (e)
	  {
	    for (i = 0; i < e->pts; i++)
	      if ((e->data[i * 2 + 1] > 1.0) ||
		  (e->data[i * 2 + 1] < 0.0))
		{
		  free_env(e);
		  Xen_out_of_range_error(caller, 1, val, "y values < 0.0 or > 1.0");
		}
	    sp->filter_control_envelope = e;
	    filter_env_changed(sp, sp->filter_control_envelope);
	  }
      }
      break;

    default:
      break;
    }
  return(val);
}


static Xen sound_set_global(Xen snd, Xen val, sp_field_t fld, const char *caller)
{
  mus_float_t fval;
  if (!Xen_is_bound(snd))
    switch (fld)
      {
      case SP_FILTER_DBING:   
	in_set_filter_control_in_dB(ss, Xen_boolean_to_C_bool(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_FILTER_HZING:   
	in_set_filter_control_in_hz(ss, Xen_boolean_to_C_bool(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_FILTER_ORDER:
	Xen_check_type(Xen_is_integer(val), val, 0, caller, "an integer");
	if (Xen_integer_to_C_int(val) > 0)
	  in_set_filter_control_order(ss, Xen_integer_to_C_int(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_SHOW_CONTROLS:
	in_set_show_controls(ss, Xen_boolean_to_C_bool(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_SPEED_TONES:
	Xen_check_type(Xen_is_integer(val), val, 0, caller, "an integer");
	in_set_speed_control_tones(ss, Xen_integer_to_C_int(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_SPEED_STYLE:
	Xen_check_type(Xen_is_integer(val), val, 0, caller, "an integer");
	in_set_speed_control_style(ss, (speed_style_t)Xen_integer_to_C_int(val)); /* range checked already */
	return(sound_set(Xen_true, val, fld, caller));

      case SP_AMP_BOUNDS:
	in_set_amp_control_min(ss, Xen_real_to_C_double(Xen_car(val)));
	in_set_amp_control_max(ss, Xen_real_to_C_double(Xen_cadr(val)));
	reflect_mix_change(ANY_MIX_ID);
	return(sound_set(Xen_true, val, fld, caller));

      case SP_CONTRAST_BOUNDS:
	in_set_contrast_control_min(ss, Xen_real_to_C_double(Xen_car(val)));
	in_set_contrast_control_max(ss, Xen_real_to_C_double(Xen_cadr(val)));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_CONTRAST_AMP:  
	in_set_contrast_control_amp(ss, Xen_real_to_C_double(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_EXPAND_BOUNDS:
	in_set_expand_control_min(ss, Xen_real_to_C_double(Xen_car(val)));
	in_set_expand_control_max(ss, Xen_real_to_C_double(Xen_cadr(val)));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_EXPAND_LENGTH: 
	fval = Xen_real_to_C_double(val);
	if (fval > 0.0)
	  in_set_expand_control_length(ss, fval);
	return(sound_set(Xen_true, val, fld, caller));

      case SP_EXPAND_RAMP:
	fval = Xen_real_to_C_double(val);
	if ((fval >= 0.0) && (fval < 0.5)) 
	  in_set_expand_control_ramp(ss, fval);
	return(sound_set(Xen_true, val, fld, caller));

      case SP_EXPAND_HOP:
	fval = Xen_real_to_C_double(val);
	if (fval > 0.0)
	  in_set_expand_control_hop(ss, fval);
	return(sound_set(Xen_true, val, fld, caller));

      case SP_EXPAND_JITTER:    
	in_set_expand_control_jitter(ss, Xen_real_to_C_double(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_SPEED_BOUNDS:
	in_set_speed_control_min(ss, Xen_real_to_C_double(Xen_car(val)));
	in_set_speed_control_max(ss, Xen_real_to_C_double(Xen_cadr(val)));
	reflect_mix_change(ANY_MIX_ID);
	return(sound_set(Xen_true, val, fld, caller));

      case SP_REVERB_LENGTH_BOUNDS:
	in_set_reverb_control_length_min(ss, Xen_real_to_C_double(Xen_car(val)));
	in_set_reverb_control_length_max(ss, Xen_real_to_C_double(Xen_cadr(val)));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_REVERB_FEEDBACK:  
	in_set_reverb_control_feedback(ss, Xen_real_to_C_double(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_REVERB_SCALE_BOUNDS:
	in_set_reverb_control_scale_min(ss, Xen_real_to_C_double(Xen_car(val)));
	in_set_reverb_control_scale_max(ss, Xen_real_to_C_double(Xen_cadr(val)));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_REVERB_LOW_PASS:  
	in_set_reverb_control_lowpass(ss, Xen_real_to_C_double(val));
	return(sound_set(Xen_true, val, fld, caller));

      case SP_REVERB_DECAY:     
	in_set_reverb_control_decay(ss, Xen_real_to_C_double(val));
	return(sound_set(Xen_true, val, fld, caller));

      default: break;
      }
  return(sound_set(snd, val, fld, caller));
}


static Xen g_channels(Xen snd)
{
  #define H_channels "("  S_channels " :optional obj): how many channels the object obj has"

  if (Xen_is_string(snd))
    return(g_mus_sound_chans(snd));              /* mus-sound-chans */

  if ((mus_is_xen(snd)) ||
      (mus_is_vct(snd)) ||
      (Xen_is_list(snd)))
    return(g_mus_channels(snd));                 /* mus-channels */

  if (xen_is_mix(snd))                            /* mixes are always 1 chan */
    return(C_int_to_Xen_integer(1));

  if (xen_is_region(snd))                         /* region-chans */
    return(g_region_chans(snd));

  if (xen_is_selection(snd))                      /* selection-chans */
    return(g_selection_chans());

  if (Xen_is_vector(snd))                         /* vector as output in clm */
    return(C_int_to_Xen_integer(Xen_vector_rank(snd)));

  return(sound_get(snd, SP_NCHANS, S_channels));
}


static Xen check_number(Xen val, const char *caller)
{
  Xen_check_type(Xen_is_number(val), val, 1, caller, "a number");
  return(val);
}


static Xen check_non_negative_integer(Xen val, const char *caller)
{
  Xen_check_type(Xen_is_integer(val) && (Xen_integer_to_C_int(val) >= 0), val, 1, caller, "a non-negative integer");
  return(val);
}


static Xen g_set_channels(Xen snd, Xen val)
{
  if (!Xen_is_bound(val))
    return(sound_set(Xen_undefined, check_non_negative_integer(snd, S_set S_channels), SP_NCHANS, S_set S_channels));
  else return(sound_set(snd, check_non_negative_integer(val, S_set S_channels), SP_NCHANS, S_set S_channels));
}


static Xen g_srate(Xen snd) 
{
  #define H_srate "(" S_srate " :optional obj): obj's srate; obj can be a region, a string (sound file name), a sound, or an integer (sound index)"

  if (Xen_is_string(snd))
    return(g_mus_sound_srate(snd));

  if (xen_is_region(snd))
    return(g_region_srate(snd));

  if (xen_is_selection(snd))
    return(g_selection_srate());

  return(sound_get(snd, SP_SRATE, S_srate));
}


static Xen g_set_srate(Xen snd, Xen val) 
{
  if (!Xen_is_bound(val))
    return(sound_set(Xen_undefined, check_number(snd, S_set S_srate), SP_SRATE, S_set S_srate));
  else return(sound_set(snd, check_number(val, S_set S_srate), SP_SRATE, S_set S_srate));
}


static Xen g_data_location(Xen snd) 
{
  #define H_data_location "(" S_data_location " :optional snd): snd's data location (bytes)"
  return(sound_get(snd, SP_DATA_LOCATION, S_data_location));
}


static Xen g_set_data_location(Xen snd, Xen val) 
{
  if (!Xen_is_bound(val))
    return(sound_set(Xen_undefined, check_non_negative_integer(snd, S_set S_data_location), SP_DATA_LOCATION, S_set S_data_location));
  else return(sound_set(snd, check_non_negative_integer(val, S_set S_data_location), SP_DATA_LOCATION, S_set S_data_location));
}


static Xen g_data_size(Xen snd) 
{
  #define H_data_size "(" S_data_size " :optional snd): snd's data size (bytes)"
  return(sound_get(snd, SP_DATA_SIZE, S_data_size));
}


static Xen g_set_data_size(Xen snd, Xen val) 
{
  if (!Xen_is_bound(val))
    return(sound_set(Xen_undefined, check_non_negative_integer(snd, S_set S_data_size), SP_DATA_SIZE, S_set S_data_size));
  else return(sound_set(snd, check_non_negative_integer(val, S_set S_data_size), SP_DATA_SIZE, S_set S_data_size));
}


static Xen g_sample_type(Xen snd) 
{
  #define H_sample_type "(" S_sample_type " :optional snd): snd's sample type (e.g. " S_mus_bshort ")"
  return(sound_get(snd, SP_SAMPLE_TYPE, S_sample_type));
}


static Xen g_set_sample_type(Xen snd, Xen val) 
{
  if (!Xen_is_bound(val))
    return(sound_set(Xen_undefined, check_non_negative_integer(snd, S_set S_sample_type), SP_SAMPLE_TYPE, S_set S_sample_type));
  else return(sound_set(snd, check_non_negative_integer(val, S_set S_sample_type), SP_SAMPLE_TYPE, S_set S_sample_type));
}


static Xen g_header_type(Xen snd) 
{
  #define H_header_type "(" S_header_type " :optional snd): snd's header type (e.g. " S_mus_aiff ")"
  return(sound_get(snd, SP_HEADER_TYPE, S_header_type));
}


static Xen g_set_header_type(Xen snd, Xen val) 
{
  if (!Xen_is_bound(val))
    return(sound_set(Xen_undefined, check_non_negative_integer(snd, S_set S_header_type), SP_HEADER_TYPE, S_set S_header_type));
  else return(sound_set(snd, check_non_negative_integer(val, S_set S_header_type), SP_HEADER_TYPE, S_set S_header_type));
}


static Xen g_comment(Xen snd)
{
  #define H_comment "(" S_comment " :optional snd): snd's comment (in its header)"
  return(sound_get(snd, SP_COMMENT, S_comment));
}


static Xen g_set_comment(Xen snd, Xen val) 
{
  if (!Xen_is_bound(val))
    {
      Xen_check_type(Xen_is_string(snd) || Xen_is_false(snd), snd, 1, S_set S_comment, "a string");
      return(sound_set(Xen_undefined, snd, SP_COMMENT, S_set S_comment));
    }

  Xen_check_type(Xen_is_string(val) || Xen_is_false(val), val, 2, S_set S_comment, "a string");
  return(sound_set(snd, val, SP_COMMENT, S_set S_comment));
}


static Xen g_sync(Xen snd) 
{
  #define H_sync "(" S_sync " :optional snd): snd's sync value (0 = no sync).  Some editing operations \
are applied to all sounds sharing the sync value of the selected sound.  'snd' can also be a mix or mark object."

  if (xen_is_mix(snd))                            /* mix-sync */
    return(g_mix_sync(snd));

  if (xen_is_mark(snd))                           /* mark-sync */
    return(g_mark_sync(snd));

  return(sound_get(snd, SP_SYNC, S_sync));       /* sync */
}


static Xen g_set_sync(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_integer_or_boolean(on), on, 1, S_set S_sync, "an integer");

  if (xen_is_mix(snd))
    return(g_set_mix_sync(snd, on));

  if (xen_is_mark(snd))
    return(g_set_mark_sync(snd, on));

  return(sound_set(snd, on, SP_SYNC, S_set S_sync));
}

with_two_setter_args(g_set_sync_reversed, g_set_sync)


static Xen g_sync_max(void) 
{
  #define H_sync_max "(" S_sync_max "): max sound sync value seen so far"
  return(C_int_to_Xen_integer(ss->sound_sync_max));
}


static Xen g_sound_properties(Xen snd) 
{
  #define H_sound_properties "(" S_sound_properties " :optional snd): snd's property list"
  return(sound_get(snd, SP_PROPERTIES, S_sound_properties));
}


static Xen g_set_sound_properties(Xen on, Xen snd) 
{
  return(sound_set(snd, on, SP_PROPERTIES, S_set S_sound_properties));
}

with_two_setter_args(g_set_sound_properties_reversed, g_set_sound_properties)


static Xen g_sound_property(Xen key, Xen snd) 
{
  #define H_sound_property "(" S_sound_property " key snd) returns the value associated with 'key' in the given sound's\
property list, or " PROC_FALSE "."
  return(Xen_assoc_ref(key, g_sound_properties(snd)));
}

#if HAVE_SCHEME
static Xen g_set_sound_property(Xen val, Xen key, Xen snd) 
#else
static Xen g_set_sound_property(Xen key, Xen val, Xen snd) 
#endif
{
  g_set_sound_properties(Xen_assoc_set(key, val, g_sound_properties(snd)), snd);
  return(val);
}

with_three_setter_args(g_set_sound_property_reversed, g_set_sound_property)



static Xen g_channel_style(Xen snd) 
{
  snd_info *sp;

  if (!Xen_is_bound(snd))
    return(C_int_to_Xen_integer(channel_style(ss)));

  Snd_assert_sound(S_channel_style, snd, 1);
  sp = get_sp(snd);
  if (!sp) 
    return(snd_no_such_sound_error(S_channel_style, snd));

  return(C_int_to_Xen_integer((int)(sp->channel_style)));
}


static void update_sound(snd_info *sp)
{
  if (sp)
    {
      switch (channel_style(ss))
	{
	case CHANNELS_SEPARATE:     separate_sound(sp);    break;
	case CHANNELS_COMBINED:     combine_sound(sp);     break;
	case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
	default:
	  break;
	}
    }
}


void set_channel_style(channel_style_t val)
{
  in_set_channel_style(val);
  for_each_sound(update_sound);
  for_each_chan(update_graph);
}


static Xen g_set_channel_style(Xen style, Xen snd) 
{
  snd_info *sp;
  int in_style;
  channel_style_t new_style = CHANNELS_SEPARATE;

  #define H_channel_style "(" S_channel_style " :optional snd): how multichannel sounds lay out the channels. \
The default is " S_channels_combined "; other values are " S_channels_separate " and " S_channels_superimposed ". \
As a global (if the 'snd' arg is omitted), it is the default setting for each sound's 'unite' button."

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_channel_style, "an integer"); 
  in_style = Xen_integer_to_C_int(style);
  if ((in_style < 0) ||
      (in_style >= NUM_CHANNEL_STYLES))
    Xen_out_of_range_error(S_set S_channel_style, 1, style, S_channel_style " should be " S_channels_separate ", " S_channels_combined ", or " S_channels_superimposed);
  new_style = (channel_style_t)in_style;

  if (!Xen_is_bound(snd))
    {
      set_channel_style(new_style);
      return(C_int_to_Xen_integer(channel_style(ss)));
    }

  Snd_assert_sound(S_set S_channel_style, snd, 2);
  sp = get_sp(snd);
  if (!sp) 
    return(snd_no_such_sound_error(S_set S_channel_style, snd));

  set_sound_channel_style(sp, new_style);

  return(C_int_to_Xen_integer((int)(sp->channel_style)));
}

with_two_setter_args(g_set_channel_style_reversed, g_set_channel_style)


static Xen g_read_only(Xen snd) 
{
  #define H_read_only "(" S_read_only " :optional snd): whether snd is write-protected"
  return(sound_get(snd, SP_READ_ONLY, S_read_only));
}


static Xen g_set_read_only(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_read_only, "a boolean");
  return(sound_set(snd, on, SP_READ_ONLY, S_set S_read_only));
}

with_two_setter_args(g_set_read_only_reversed, g_set_read_only)


static Xen g_contrast_control_on(Xen snd) 
{
  #define H_contrast_control_on "(" S_contrast_control_on " :optional snd): snd's control panel constrast button state"
  return(sound_get(snd, SP_CONTRASTING, S_contrast_control_on));
}


static Xen g_set_contrast_control_on(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_contrast_control_on, "a boolean");
  return(sound_set(snd, on, SP_CONTRASTING, S_set S_contrast_control_on));
}

with_two_setter_args(g_set_contrast_control_on_reversed, g_set_contrast_control_on)


static Xen g_expand_control_on(Xen snd) 
{
  #define H_expand_control_on "(" S_expand_control_on " :optional snd): snd's control panel expand button state"
  return(sound_get(snd, SP_EXPANDING, S_expand_control_on));
}


static Xen g_set_expand_control_on(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_expand_control_on, "a boolean");
  return(sound_set(snd, on, SP_EXPANDING, S_set S_expand_control_on));
}

with_two_setter_args(g_set_expand_control_on_reversed, g_set_expand_control_on)


static Xen g_reverb_control_on(Xen snd) 
{
  #define H_reverb_control_on "(" S_reverb_control_on " :optional snd): snd's control panel reverb button state"
  return(sound_get(snd, SP_REVERBING, S_reverb_control_on));
}


static Xen g_set_reverb_control_on(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_reverb_control_on, "a boolean");
  return(sound_set(snd, on, SP_REVERBING, S_set S_reverb_control_on));
}

with_two_setter_args(g_set_reverb_control_on_reversed, g_set_reverb_control_on)


static Xen g_filter_control_on(Xen snd) 
{
  #define H_filter_control_on "(" S_filter_control_on " :optional snd): snd's control panel filter button state"
  return(sound_get(snd, SP_FILTERING, S_filter_control_on));
}


static Xen g_set_filter_control_on(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_filter_control_on, "a boolean");
  return(sound_set(snd, on, SP_FILTERING, S_set S_filter_control_on));
}

with_two_setter_args(g_set_filter_control_on_reversed, g_set_filter_control_on)


static Xen g_filter_control_in_dB(Xen snd) 
{
  #define H_filter_control_in_dB "(" S_filter_control_in_dB " :optional snd): " PROC_TRUE " if snd's filter envelope is displayed in dB in control panel"
  return(sound_get_global(snd, SP_FILTER_DBING, S_filter_control_in_dB));
}


static Xen g_set_filter_control_in_dB(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_filter_control_in_dB, "a boolean");
  return(sound_set_global(snd, on, SP_FILTER_DBING, S_set S_filter_control_in_dB));
}

with_two_setter_args(g_set_filter_control_in_dB_reversed, g_set_filter_control_in_dB)


static Xen g_filter_control_in_hz(Xen snd) 
{
  #define H_filter_control_in_hz "(" S_filter_control_in_hz " :optional snd): " PROC_TRUE " if snd's filter envelope x axis should be in hz (control panel filter)"
  return(sound_get_global(snd, SP_FILTER_HZING, S_filter_control_in_hz));
}


static Xen g_set_filter_control_in_hz(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_filter_control_in_hz, "a boolean");
  return(sound_set_global(snd, on, SP_FILTER_HZING, S_set S_filter_control_in_hz));
}

with_two_setter_args(g_set_filter_control_in_hz_reversed, g_set_filter_control_in_hz)


static Xen g_filter_control_coeffs(Xen snd) 
{
  #define H_filter_control_coeffs "(" S_filter_control_coeffs " :optional snd): control panel filter coeffs"
  return(sound_get(snd, SP_FILTER_COEFFS, S_filter_control_coeffs));
}


static Xen g_filter_control_order(Xen snd) 
{
  #define H_filter_control_order "(" S_filter_control_order " :optional snd): filter order (in control panel)"
  return(sound_get_global(snd, SP_FILTER_ORDER, S_filter_control_order));
}


static Xen g_set_filter_control_order(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_integer(on), on, 1, S_set S_filter_control_order, "an integer"); 
  return(sound_set_global(snd, on, SP_FILTER_ORDER, S_set S_filter_control_order));
}

with_two_setter_args(g_set_filter_control_order_reversed, g_set_filter_control_order)


static Xen g_show_controls(Xen snd) 
{
  #define H_show_controls "(" S_show_controls " :optional snd): " PROC_TRUE " if snd's control panel is known to be open"
  return(sound_get_global(snd, SP_SHOW_CONTROLS, S_show_controls));
}


static Xen g_set_show_controls(Xen on, Xen snd)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_show_controls, "a boolean");
  return(sound_set_global(snd, on, SP_SHOW_CONTROLS, S_set S_show_controls));
}

with_two_setter_args(g_set_show_controls_reversed, g_set_show_controls)


static Xen g_save_controls(Xen snd) 
{
  #define H_save_controls "(" S_save_controls " :optional snd): save the control panel settings for subsequent " S_restore_controls
  return(sound_get(snd, SP_SAVE_CONTROLS, S_save_controls));
}


static Xen g_restore_controls(Xen snd) 
{
  #define H_restore_controls "(" S_restore_controls " :optional snd): restore the previously saved control panel settings"
  return(sound_get(snd, SP_RESTORE_CONTROLS, S_restore_controls));
}


static Xen g_reset_controls(Xen snd) 
{
  #define H_reset_controls "(" S_reset_controls " :optional snd): reset (clear) the control panel settings"
  return(sound_get(snd, SP_RESET_CONTROLS, S_reset_controls));
}


static Xen g_selected_channel(Xen snd) 
{
  #define H_selected_channel "(" S_selected_channel " :optional snd): currently selected channel in snd (or " PROC_FALSE " if none)"
  return(sound_get(snd, SP_SELECTED_CHANNEL, S_selected_channel));
}


static Xen g_set_selected_channel(Xen snd, Xen chn_n) 
{
  snd_info *sp;

  if (!Xen_is_bound(chn_n))
    return(g_select_channel(snd));

  Snd_assert_sound(S_set S_selected_channel, snd, 1); 
  sp = get_sp(snd);
  if (!sp) 
    return(snd_no_such_sound_error(S_set S_selected_channel, snd));

  if (Xen_is_false(chn_n))
    sp->selected_channel = NO_SELECTION;
  else
    {
      mus_long_t chan = 0;
      if (Xen_is_integer(chn_n)) chan = Xen_integer_to_C_int(chn_n);
      if ((chan >= 0) && 
	  (chan < (int)sp->nchans)) 
	{
	  select_channel(sp, (int)chan);
	  return(chn_n);
	}
      return(snd_no_such_channel_error(S_set S_selected_channel, snd, chn_n));
    }

  return(Xen_false);
}


static Xen g_file_name(Xen snd) 
{
  #define H_file_name "(" S_file_name " :optional snd): snd's full filename; snd can be a sound, mix, region, string, or generator."

  if (xen_is_sound(snd))
    return(sound_get(snd, SP_FILE_NAME, S_file_name));

  if (mus_is_xen(snd))
    return(g_mus_file_name(snd));

  if (xen_is_mix(snd))
    return(C_string_to_Xen_string(mix_file_name(Xen_mix_to_C_int(snd))));

  if (xen_is_region(snd))
    return(C_string_to_Xen_string(region_file_name(Xen_region_to_C_int(snd))));

#if HAVE_SCHEME
  if ((s7_is_input_port(s7, snd)) || (s7_is_output_port(s7, snd)))
    return(C_string_to_Xen_string(s7_port_filename(s7, snd)));
#endif

  if (Xen_is_string(snd))
    return(g_mus_expand_filename(snd));

  if ((is_sampler(snd)) || (is_mix_sampler(snd)))
    return(g_sampler_file_name(snd));

  return(sound_get(snd, SP_FILE_NAME, S_file_name));
}


static Xen g_short_file_name(Xen snd) 
{
  #define H_short_file_name "(" S_short_file_name " :optional snd): short form of snd's file name (no directory)"
  return(sound_get(snd, SP_SHORT_FILE_NAME, S_short_file_name));
}


static Xen g_close_sound_1(int snd)
{
  if ((snd >= 0) &&
      (snd < ss->max_sounds))
    {
      snd_info *sp;
      sp = ss->sounds[snd];
      if (snd_ok(sp))
	{
	  if (sp->inuse == SOUND_WRAPPER) /* from make_simple_channel_display (variable-graph and the region graphs) */
	    {
	      /* not sure what to do in this case, but at least we can get it out of the various #t chan loops */
	      sp->inuse = SOUND_IDLE;
	      ss->sounds[sp->index] = NULL; /* a huge memory leak... */
	    }
	  else snd_close_file(sp);
	}
    }
  return(Xen_false);
}


static Xen g_close_sound(Xen snd) 
{
  #define H_close_sound "(" S_close_sound " :optional snd): close snd"

  if (Xen_is_integer(snd))
    return(g_close_sound_1(Xen_integer_to_C_int(snd)));

  if (xen_is_sound(snd))
    return(g_close_sound_1(Xen_sound_to_C_int(snd)));

  return(sound_get(snd, SP_CLOSE, S_close_sound));
}


static Xen g_update_sound(Xen snd) 
{
  #define H_update_sound "(" S_update_sound " :optional snd): update snd (re-read it from the disk after flushing pending edits)"
  return(sound_get(snd, SP_UPDATE, S_update_sound));
}


static void save_sound_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  Xen_error(Xen_make_error_type("cannot-save"),
	    Xen_list_3(C_string_to_Xen_string("~A: ~A"),
		       C_string_to_Xen_string((char *)data),
		       C_string_to_Xen_string(msg)));
}


static Xen g_save_sound(Xen index) 
{
  snd_info *sp;
  io_error_t err = IO_NO_ERROR;
  #define H_save_sound "(" S_save_sound " :optional snd): save snd (update the on-disk data to match Snd's current version)"

  Snd_assert_sound(S_save_sound, index, 1);

  sp = get_sp(index);
  if (!sp) 
    return(snd_no_such_sound_error(S_save_sound, index));

  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    {
      char *msg;
      Xen str;
      msg = mus_format("%s (index %d) is write-protected", 
		       sp->short_filename, 
		       sp->index);
      str = C_string_to_Xen_string(msg);
      free(msg);
      Xen_error(Xen_make_error_type("cannot-save"),
		Xen_list_2(C_string_to_Xen_string(S_save_sound ": can't save sound, ~A"),
			   str));
      return(Xen_false);
    }

  redirect_snd_error_to(save_sound_error_handler, (void *)S_save_sound);
  redirect_snd_warning_to(save_sound_error_handler, (void *)S_save_sound);
  err = save_edits_without_asking(sp);      
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);

  /* if err and we got here, report it */
  if (is_serious_io_error(err))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_2(C_string_to_Xen_string(S_save_sound ": IO error ~A"),
			 C_string_to_Xen_string(io_error_name(err))));
	      
  return(C_int_to_Xen_sound(sp->index));
}


static Xen g_revert_sound(Xen index)
{
  #define H_revert_sound "("  S_revert_sound " :optional snd): revert snd to its unedited state (undo all)"
  snd_info *sp;
  uint32_t i;

  Snd_assert_sound(S_revert_sound, index, 1);

  sp = get_sp(index);
  if (!sp) 
    return(snd_no_such_sound_error(S_revert_sound, index));

  for (i = 0; i < sp->nchans; i++) 
    {
      revert_edits(sp->chans[i]); 
      update_graph(sp->chans[i]);
    }
  reflect_file_revert_in_label(sp);

  return(index); /* was #t */
}


static Xen g_selected_sound(void)
{
  #define H_selected_sound "(" S_selected_sound "): currently selected sound (or " PROC_FALSE " if none)"
  if ((ss->selected_sound != NO_SELECTION) && 
      (snd_ok(ss->sounds[ss->selected_sound])))
    return(C_int_to_Xen_sound(ss->selected_sound));

  return(Xen_false);
}


static void open_sound_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  Xen_error(Xen_make_error_type("not-a-sound-file"),
	    Xen_list_3(C_string_to_Xen_string("~A: ~A"),
		       C_string_to_Xen_string((char *)data),
		       C_string_to_Xen_string(msg)));
}


static Xen g_open_sound(Xen filename)
{ 
  /* return new sound if successful */
  #define H_open_sound "(" S_open_sound " filename): \
open filename (as if opened from File:Open menu option), and return the new sound"

  const char *fname = NULL;
  snd_info *sp;
  bool file_exists;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_open_sound, "a string");

  fname = Xen_string_to_C_string(filename);
  {
    char *fullname;
    /* before probing, need to undo all the Unix-isms */
    fullname = mus_expand_filename(fname);
    file_exists = mus_file_probe(fullname);
    free(fullname);
  }

  if (!file_exists)
    return(snd_no_such_file_error(S_open_sound, filename));
  ss->open_requestor = FROM_OPEN_SOUND;

  redirect_snd_error_to(open_sound_error_handler, (void *)S_open_sound);
  sp = snd_open_file(fname, FILE_READ_WRITE); /* this will call mus_expand_filename */
  redirect_snd_error_to(NULL, NULL);

  if (sp) 
    return(C_int_to_Xen_sound(sp->index));

  /* sp NULL is not an error (open-hook func returned #t) */
  return(Xen_false);
}

#if (!HAVE_SCHEME)
static Xen kw_header_type, kw_file, kw_srate, kw_channel, kw_sound, kw_edit_position, kw_channels, kw_size, kw_comment, kw_sample_type;

static void init_sound_keywords(void)
{
  kw_header_type = Xen_make_keyword("header-type");
  kw_sample_type = Xen_make_keyword("sample-type");
  kw_file = Xen_make_keyword("file");
  kw_srate = Xen_make_keyword("srate");
  kw_channel = Xen_make_keyword("channel");
  kw_sound = Xen_make_keyword("sound");
  kw_edit_position = Xen_make_keyword("edit-position");
  kw_channels = Xen_make_keyword("channels");
  kw_size = Xen_make_keyword("size");
  kw_comment = Xen_make_keyword("comment");
}
#endif

#define H_open_raw_sound "(" S_open_raw_sound " file channels srate sample-type): \
open file assuming the data matches the attributes indicated unless the file actually has a header"

#if HAVE_SCHEME
static s7_pointer g_open_raw_sound(s7_scheme *sc, s7_pointer args)
{
  const char *file;
  char *fullname;
  snd_info *sp;
  bool file_exists;
  int os = 1, oc = 1;
  mus_sample_t ofr = MUS_BSHORT;
  s7_pointer p, fp;

  mus_header_raw_defaults(&os, &oc, &ofr);
  
  fp = s7_car(args);
  if (!s7_is_string(fp))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_1(C_string_to_Xen_string(S_open_raw_sound ": no output file?")));
  file = s7_string(fp);
  fullname = mus_expand_filename(file);
  file_exists = mus_file_probe(fullname);
  free(fullname);
  if (!file_exists)
    return(snd_no_such_file_error(S_open_raw_sound, fp));
  
  fp = s7_cadr(args);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_open_raw_sound, 2, fp, "an integer"));
      oc = s7_integer(fp);
      if ((oc < 0) ||
	  (oc > MUS_MAX_CHANS))
	Xen_out_of_range_error(S_open_raw_sound, 2, fp, "too many channels requested");
      set_fallback_chans(oc);
    }

  p = s7_cddr(args);

  fp = s7_car(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_open_raw_sound, 3, fp, "an integer"));
      os = s7_integer(fp);
      set_fallback_srate(os);
    }

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_open_raw_sound, 4, fp, "an integer"));
      ofr = (mus_sample_t)s7_integer(fp);
      set_fallback_sample_type(ofr);
    }

  mus_header_set_raw_defaults(os, oc, ofr);
  ss->reloading_updated_file = -1;
  ss->open_requestor = FROM_OPEN_RAW_SOUND;
  sp = snd_open_file(file, FILE_READ_WRITE);
  set_fallback_chans(0);
  set_fallback_srate(0);
  set_fallback_sample_type(MUS_UNKNOWN_SAMPLE);
  ss->reloading_updated_file = 0;
  if (sp) 
    return(C_int_to_Xen_sound(sp->index));
  return(Xen_false);
}
#else
static Xen g_open_raw_sound(Xen arglist)
{
  const char *file = NULL;
  char *fullname;
  snd_info *sp;
  bool file_exists;
  int os = 1, oc = 1;
  mus_sample_t ofr = MUS_BSHORT;
  Xen args[8]; 
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, i, arglist_len;

  keys[0] = kw_file;
  keys[1] = kw_channels;
  keys[2] = kw_srate;
  keys[3] = kw_sample_type;

  mus_header_raw_defaults(&os, &oc, &ofr);

  for (i = 0; i < 8; i++) args[i] = Xen_undefined;
  arglist_len = Xen_list_length(arglist);
  if (arglist_len > 8)
    Xen_out_of_range_error(S_open_raw_sound, 0, arglist, "too many arguments");

  for (i = 0; i < arglist_len; i++) args[i] = Xen_list_ref(arglist, i);
  vals = mus_optkey_unscramble(S_open_raw_sound, arglist_len, 4, keys, args, orig_arg);

  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_open_raw_sound, orig_arg[0], NULL);
      oc = mus_optkey_to_int(keys[1], S_open_raw_sound, orig_arg[1], oc);
      if ((oc < 0) ||
	  (oc > MUS_MAX_CHANS))
	Xen_out_of_range_error(S_open_raw_sound, 2, args[orig_arg[1]], "too many channels requested");
      if (!(Xen_is_keyword(keys[1]))) set_fallback_chans(oc);
      os = mus_optkey_to_int(keys[2], S_open_raw_sound, orig_arg[2], os);
      if (!(Xen_is_keyword(keys[2]))) set_fallback_srate(os);
      ofr = (mus_sample_t)mus_optkey_to_int(keys[3], S_open_raw_sound, orig_arg[3], (int)ofr);
      if (!(Xen_is_keyword(keys[3]))) set_fallback_sample_type(ofr);
    }

  if (!file) 
    Xen_error(NO_SUCH_FILE,
	      Xen_list_1(C_string_to_Xen_string(S_open_raw_sound ": no output file?")));

  fullname = mus_expand_filename(file);
  file_exists = mus_file_probe(fullname);
  free(fullname);
  if (!file_exists)
    return(snd_no_such_file_error(S_open_raw_sound, keys[0]));

  mus_header_set_raw_defaults(os, oc, ofr);
  ss->reloading_updated_file = -1;
  ss->open_requestor = FROM_OPEN_RAW_SOUND;

  sp = snd_open_file(file, FILE_READ_WRITE);

  set_fallback_chans(0);
  set_fallback_srate(0);
  set_fallback_sample_type(MUS_UNKNOWN_SAMPLE);
  ss->reloading_updated_file = 0;

  /* snd_open_file -> snd_open_file_1 -> add_sound_window -> make_file_info -> raw_data_dialog_to_file_info */
  /*   so here if hooked, we'd need to save the current hook, make it return the current args, open, then restore */

  if (sp) 
    return(C_int_to_Xen_sound(sp->index));
  return(Xen_false);
}
#endif

#if HAVE_SCHEME
  #define read_only_example "You can make it writable via: (set! (" S_read_only ") #f)"
#endif
#if HAVE_RUBY
  #define read_only_example "You can make it writable via: set_read_only(false)"
#endif
#if HAVE_FORTH
  #define read_only_example "You can make it writable via: #f set-read-only"
#endif

static Xen g_view_sound(Xen filename)
{
  #define H_view_sound "(" S_view_sound " filename): open a file in read-only mode. " read_only_example " at any time."

  const char *fname = NULL;
  char *fullname;
  snd_info *sp = NULL;
  bool file_exists;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_view_sound, "a string");

  fname = Xen_string_to_C_string(filename);
  fullname = mus_expand_filename(fname);
  file_exists = mus_file_probe(fullname);
  free(fullname);
  if (!file_exists)
    return(snd_no_such_file_error(S_view_sound, filename));

  ss->open_requestor = FROM_VIEW_SOUND;
  sp = snd_open_file(fname, FILE_READ_ONLY);

  if (sp) 
    return(C_int_to_Xen_sound(sp->index));
  return(Xen_false);
}

  #if HAVE_SCHEME
    #define save_as_example "(" S_save_sound_as " \"test.snd\" index 44100 " S_mus_bshort " " S_mus_next ")"
  #endif
  #if HAVE_RUBY
    #define save_as_example "save_sound_as(\"test.snd\", index, 44100, Mus_bshort, Mus_next)"
  #endif
  #if HAVE_FORTH
    #define save_as_example "\"test.snd\" index 44100 mus-bshort mus-next save-sound-as"
  #endif

  #define H_save_sound_as "("  S_save_sound_as " file sound srate sample-type header-type channel edit-position comment): \
save sound in file using the indicated attributes.  If channel is specified, only that channel is saved (extracted). \
Omitted arguments take their value from the sound being saved.\n  " save_as_example
  
#if HAVE_SCHEME
static s7_pointer g_save_sound_as(s7_scheme *sc, s7_pointer args)
{
  snd_info *sp = NULL; 
  file_info *hdr;
  mus_header_t ht;
  mus_sample_t df;
  char *fname = NULL;
  int sr, chan;
  const char *outcom, *file;
  io_error_t io_err = IO_NO_ERROR;
  s7_pointer p, fp, index, edpos, pchan, filep;
  bool free_outcom = false;
  int edit_position = AT_CURRENT_EDIT_POSITION;

  /* fprintf(stderr, "args: %s\n", s7_object_to_c_string(sc, args)); */

  fp = s7_car(args);
  filep = fp;
  if (fp != Xen_false)
    {
      if (!s7_is_string(fp))
	return(s7_wrong_type_arg_error(sc, S_save_sound_as, 1, fp, "a string (a filename)"));
      file = s7_string(fp);
    }
  else file = NULL;

  index = s7_cadr(args);
  Snd_assert_sound(S_save_sound_as, index, 2);
  sp = get_sp(index);
  if (!sp) 
    return(snd_no_such_sound_error(S_save_sound_as, index));
  hdr = sp->hdr;

  p = s7_cddr(args);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_sound_as, 3, fp, "an integer (srate)"));
      sr = s7_integer(fp);
      if (sr <= 0)
	Xen_error(Xen_make_error_type("cannot-save"),
		  Xen_list_2(C_string_to_Xen_string(S_save_sound_as ": srate (~A) can't be <= 0"), fp));
    }
  else sr = -1;

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_sound_as, 4, fp, "an integer (sample type)"));
      df = (mus_sample_t)s7_integer(fp);
    }
  else df = MUS_UNKNOWN_SAMPLE;

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_sound_as, 5, fp, "an integer (header type)"));
      ht = (mus_header_t)s7_integer(fp);
    }
  else ht = MUS_UNKNOWN_HEADER;

  fp = s7_cadr(p);
  pchan = fp;
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_sound_as, 6, fp, "an integer (channel)"));
      chan = s7_integer(fp);
    }
  else chan = -1;

  p = s7_cddr(p);
  edpos = s7_car(p);

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_string(fp))
	return(s7_wrong_type_arg_error(sc, S_save_sound_as, 8, fp, "a string"));
      outcom = s7_string(fp);
    }
  else outcom = NULL;

#else
static Xen g_save_sound_as(Xen arglist)
{
  snd_info *sp;
  file_info *hdr;
  mus_header_t ht = MUS_UNKNOWN_HEADER;
  mus_sample_t df = MUS_UNKNOWN_SAMPLE;
  int sr = -1, chan = -1, edit_position = AT_CURRENT_EDIT_POSITION;
  io_error_t io_err = IO_NO_ERROR;
  char *fname = NULL;
  const char *file = NULL, *outcom = NULL;
  Xen args[16]; 
  Xen keys[8];
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int i, vals, arglist_len;
  Xen edpos = Xen_undefined, index = Xen_undefined, pchan;
  bool free_outcom = false, filep;

  keys[0] = kw_file;
  keys[1] = kw_sound;
  keys[2] = kw_srate;
  keys[3] = kw_sample_type;
  keys[4] = kw_header_type;
  keys[5] = kw_channel;
  keys[6] = kw_edit_position;
  keys[7] = kw_comment;

  for (i = 0; i < 16; i++) args[i] = Xen_undefined;
  arglist_len = Xen_list_length(arglist);
  if (arglist_len > 16)
    Xen_out_of_range_error(S_save_sound_as, 0, arglist, "too many arguments");

  for (i = 0; i < arglist_len; i++) args[i] = Xen_list_ref(arglist, i);
  vals = mus_optkey_unscramble(S_save_sound_as, arglist_len, 8, keys, args, orig_arg);

  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_save_sound_as, orig_arg[0], NULL);
      if (!(Xen_is_keyword(keys[1]))) index = keys[1];
      ht = (mus_header_t)mus_optkey_to_int(keys[4], S_save_sound_as, orig_arg[4], (int)ht);
      df = (mus_sample_t)mus_optkey_to_int(keys[3], S_save_sound_as, orig_arg[3], (int)df);
      sr = mus_optkey_to_int(keys[2], S_save_sound_as, orig_arg[2], sr);

      if ((sr <= 0) && (!Xen_is_keyword(keys[2])))
	Xen_error(Xen_make_error_type("cannot-save"),
		  Xen_list_2(C_string_to_Xen_string(S_save_sound_as ": srate (~A) can't be <= 0"),
			     C_int_to_Xen_integer(sr)));

      chan = mus_optkey_to_int(keys[5], S_save_sound_as, orig_arg[5], chan);
      if (!(Xen_is_keyword(keys[6])))
	edpos = keys[6];
      outcom = mus_optkey_to_string(keys[7], S_save_sound_as, orig_arg[7], NULL);
    }
  pchan = keys[5];
  filep = keys[0];
#endif

  if ((!file) || 
      (is_directory(file)))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_1(C_string_to_Xen_string(S_save_sound_as ": no output file?")));

  Snd_assert_sound(S_save_sound_as, index, 2);

  sp = get_sp(index);
  if (!sp) 
    return(snd_no_such_sound_error(S_save_sound_as, index));
  hdr = sp->hdr;

  if (ht == MUS_UNKNOWN_HEADER) ht = hdr->type;
  if (!(mus_header_writable(ht, MUS_IGNORE_SAMPLE)))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_2(C_string_to_Xen_string(S_save_sound_as ": can't write ~A headers"),
			 C_string_to_Xen_string(mus_header_type_name(ht))));

  if (sr == -1) 
    sr = hdr->srate;

  if (df == MUS_UNKNOWN_SAMPLE) 
    {
      /* try to find some writable sample_type */
      df = hdr->sample_type;
      if (!mus_header_writable(ht, df)) 
	df = MUS_OUT_SAMPLE_TYPE;
      if (!mus_header_writable(ht, df))
	{
	  switch (df)
	    {
	    case MUS_BFLOAT:  df = MUS_LFLOAT;  break;
	    case MUS_BDOUBLE: df = MUS_LDOUBLE; break;
	    case MUS_BINT:    df = MUS_LINT;    break;
	    case MUS_LFLOAT:  df = MUS_BFLOAT;  break;
	    case MUS_LDOUBLE: df = MUS_BDOUBLE; break;
	    case MUS_LINT:    df = MUS_BINT;    break;
	    default: break;
	    }
	  if (!mus_header_writable(ht, df))
	    {
	      int i;
	      for (i = 1; i < MUS_NUM_SAMPLES; i++) /* MUS_UNKNOWN_SAMPLE is 0 */
		{
		  df = (mus_sample_t)i;
		  if (mus_header_writable(ht, df))
		    break;
		}
	    }
	}
    }

  if (!mus_header_writable(ht, df))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_3(C_string_to_Xen_string(S_save_sound_as ": can't write ~A data to ~A headers"),
			 C_string_to_Xen_string(mus_sample_type_name(df)),
			 C_string_to_Xen_string(mus_header_type_name(ht))));

  if (chan >= (int)(sp->nchans))
    return(snd_no_such_channel_error(S_save_sound_as, index, pchan));

  if (Xen_is_integer(edpos))
    {
      int i;
      edit_position = to_c_edit_position(sp->chans[(chan >= 0) ? chan : 0], edpos, S_save_sound_as, 7);
      for (i = 0; i < (int)sp->nchans; i++)
	if (edit_position > sp->chans[i]->edit_ctr)
	  Xen_error(Xen_make_error_type("no-such-edit"),
		    Xen_list_5(C_string_to_Xen_string(S_save_sound_as ": no such edit position: ~A (~S chan ~A has ~A edits)"),
			       C_int_to_Xen_integer(edit_position),
			       C_string_to_Xen_string(sp->short_filename),
			       C_int_to_Xen_integer(i),
			       C_int_to_Xen_integer(sp->chans[i]->edit_ctr)));
    }

  fname = mus_expand_filename(file);
  if (!outcom) 
    {
      outcom = output_comment(hdr);
      if (outcom) free_outcom = true;
    }

  if (!(run_before_save_as_hook(sp, fname, false, sr, df, ht, outcom)))
    {
      if (chan >= 0)
	io_err = channel_to_file_with_settings(sp->chans[chan], fname, sr, df, ht, outcom, edit_position);
      else io_err = save_edits_without_display(sp, fname, ht, df, sr, outcom, edit_position);
    }

  if (free_outcom) 
    {
      free((char *)outcom); 
      outcom = NULL;
    }

  if (io_err == IO_NO_ERROR) 
    run_after_save_as_hook(sp, fname, false); /* true => from dialog */
  else
    {
      if (io_err != IO_SAVE_HOOK_CANCELLATION)
	{
	  Xen errstr;
	  errstr = C_string_to_Xen_string(fname);
	  if (fname) {free(fname); fname = NULL;}
	  Xen_error(Xen_make_error_type("cannot-save"),
		    Xen_list_3(C_string_to_Xen_string(S_save_sound_as ": ~A (~A)"),
			       errstr,
			       C_string_to_Xen_string(snd_open_strerror())));
	}
    }

  if (fname) free(fname);
  return(filep);
}

  #if HAVE_SCHEME
    #define new_sound_example "(" S_new_sound " \"test.snd\" 1 22050 " S_mus_bshort " " S_mus_next " \"no comment\" 1000)"
  #endif
  #if HAVE_RUBY
    #define new_sound_example "new_sound(\"test.snd\", 1, 22050, Mus_bshort, Mus_next, \"no comment\", 1000)"
  #endif
  #if HAVE_FORTH
    #define new_sound_example "\"test.snd\" 1 22050 mus-bshort mus-next \"no comment\" 1000 new-sound"
  #endif

  #define H_new_sound "(" S_new_sound " file channels srate sample-type header-type comment size): \
creates a new sound file with the indicated attributes; if any are omitted, the corresponding default-output variable is used. \
The 'size' argument sets the number of samples (zeros) in the newly created sound. \n  " new_sound_example

#if HAVE_SCHEME
static s7_pointer g_new_sound(s7_scheme *sc, s7_pointer args)
{
  snd_info *sp = NULL; 
  mus_header_t ht;
  mus_sample_t df;
  char *str;
  int sr, ch, chan;
  mus_long_t size, len;
  const char *com;
  io_error_t io_err;
  s7_pointer p, fp;

  fp = s7_car(args);
  if (fp != Xen_false)
    {
      const char *file;
      if (!s7_is_string(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 1, fp, "a string (a filename)"));
      file = s7_string(fp);
      str = mus_expand_filename(file);
      if (!str)
	Xen_out_of_range_error(S_new_sound, 1, fp, "bad file name?");
    }
  else str = snd_tempnam();
  mus_sound_forget(str);

  fp = s7_cadr(args);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 2, fp, "an integer (channels)"));
      ch = s7_integer(fp);
      if (ch <= 0)
	Xen_out_of_range_error(S_new_sound, 2, fp, "channels <= 0?");
    }
  else ch = default_output_chans(ss);

  p = s7_cddr(args);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 3, fp, "an integer (srate)"));
      sr = s7_integer(fp);
      if (sr <= 0)
	Xen_out_of_range_error(S_new_sound, 3, fp, "srate <= 0?");
    }
  else sr = default_output_srate(ss);

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 4, fp, "an integer (sample type)"));
      df = (mus_sample_t)s7_integer(fp);
      if (!(mus_is_sample_type(df)))
	Xen_out_of_range_error(S_new_sound, 4, fp, "invalid sample type");
    }
  else df = default_output_sample_type(ss);

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 5, fp, "an integer (header type)"));
      ht = (mus_header_t)s7_integer(fp);
      if (!(mus_is_header_type(ht)))
	Xen_out_of_range_error(S_new_sound, 5, fp, "invalid header type");
      if (!(mus_header_writable(ht, df)))
	Xen_error(BAD_HEADER,
		  Xen_list_3(C_string_to_Xen_string(S_new_sound ": can't write ~A data to a ~A header"),
			     C_string_to_Xen_string(mus_sample_type_short_name(df)),
			     C_string_to_Xen_string(mus_header_type_name(ht))));
    }
  else ht = default_output_header_type(ss);

  fp = s7_cadr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_string(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 6, fp, "a string"));
      com = s7_string(fp);
    }
  else com = NULL;
  
  fp = s7_caddr(p);
  if (fp != Xen_false)
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_new_sound, 7, fp, "an integer (initial file size)"));
      len = s7_integer(fp);
      if (len < 0)
	Xen_out_of_range_error(S_new_sound, 7, fp, "size < 0?");
    }
  else len = 1;

  io_err = snd_write_header(str, ht, sr, ch, len * ch, df, com, NULL); /* last arg is loop info */
  if (io_err != IO_NO_ERROR)
    {
      s7_pointer filep;
      filep = s7_make_string(sc, str);
      if (str) {free(str); str = NULL;}
      Xen_error(Xen_make_error_type("IO-error"),
		Xen_list_3(C_string_to_Xen_string(S_new_sound ": ~S, ~A"),
			   filep,
			   C_string_to_Xen_string(snd_io_strerror())));
    }

  chan = snd_reopen_write(str);
  lseek(chan, mus_header_data_location(), SEEK_SET);

  size = ch * mus_samples_to_bytes(df, len);
  if (size > 0)
    {
      unsigned char *buf;
      buf = (unsigned char *)calloc(size, sizeof(unsigned char));
      if (write(chan, buf, size) != size) fprintf(stderr, "new-sound %s write error", str);
      free(buf);
    }

  snd_close(chan, str);
  ss->open_requestor = FROM_NEW_SOUND;

  sp = sound_is_silence(snd_open_file(str, FILE_READ_WRITE));

  if (str) free(str);
  if (sp) return(C_int_to_Xen_sound(sp->index));
  return(Xen_false);
}
#else
static Xen g_new_sound(Xen arglist)
{
  snd_info *sp = NULL; 
  mus_header_t ht;
  mus_sample_t df;
  int sr, ch, chan;
  mus_long_t size, len = 1;
  char *str = NULL;
  const char *com = NULL, *file = NULL;
  Xen args[14]; 
  Xen keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len;
  io_error_t io_err;

  keys[0] = kw_file;
  keys[1] = kw_channels;
  keys[2] = kw_srate;
  keys[3] = kw_sample_type;
  keys[4] = kw_header_type;
  keys[5] = kw_comment;
  keys[6] = kw_size;

  for (i = 0; i < 14; i++) args[i] = Xen_undefined;
  arglist_len = Xen_list_length(arglist);
  if (arglist_len > 14)
    Xen_out_of_range_error(S_open_raw_sound, 0, arglist, "too many arguments");

  for (i = 0; i < arglist_len; i++) args[i] = Xen_list_ref(arglist, i);
  vals = mus_optkey_unscramble(S_new_sound, arglist_len, 7, keys, args, orig_arg);

  ht = default_output_header_type(ss);
  df = default_output_sample_type(ss);
  sr = default_output_srate(ss);
  ch = default_output_chans(ss);

  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_new_sound, orig_arg[0], NULL);
      /* this can be null if :file is not passed as an arg (use temp name below) */
      ht = (mus_header_t)mus_optkey_to_int(keys[4], S_new_sound, orig_arg[4], (int)ht);
      df = (mus_sample_t)mus_optkey_to_int(keys[3], S_new_sound, orig_arg[3], (int)df);
      sr = mus_optkey_to_int(keys[2], S_new_sound, orig_arg[2], sr);
      ch = mus_optkey_to_int(keys[1], S_new_sound, orig_arg[1], ch);
      com = mus_optkey_to_string(keys[5], S_new_sound, orig_arg[5], NULL);
      len = mus_optkey_to_mus_long_t(keys[6], S_new_sound, orig_arg[6], len);
    }

  if (!(mus_is_header_type(ht)))
    Xen_out_of_range_error(S_new_sound, orig_arg[4], keys[4], "invalid header type");

  if (!(mus_is_sample_type(df)))
    Xen_out_of_range_error(S_new_sound, orig_arg[3], keys[3], "invalid sample type");

  if (!(mus_header_writable(ht, df)))
    Xen_error(BAD_HEADER,
	      Xen_list_3(C_string_to_Xen_string(S_new_sound ": can't write ~A data to a ~A header"),
			 C_string_to_Xen_string(mus_sample_type_short_name(df)),
			 C_string_to_Xen_string(mus_header_type_name(ht))));

  if (sr <= 0)
    Xen_out_of_range_error(S_new_sound, orig_arg[2], keys[2], "srate <= 0?");

  if (ch <= 0)
    Xen_out_of_range_error(S_new_sound, orig_arg[1], keys[1], "channels <= 0?");

  if (len < 0)
    Xen_out_of_range_error(S_new_sound, orig_arg[6], keys[6], "size < 0?");

  if (file)
    {
      str = mus_expand_filename(file);
      if (!str)
	Xen_out_of_range_error(S_new_sound, orig_arg[0], keys[0], "bad file name?");
    }
  else str = snd_tempnam();
  mus_sound_forget(str);

  io_err = snd_write_header(str, ht, sr, ch, len * ch, df, com, NULL); /* last arg is loop info */
  if (io_err != IO_NO_ERROR)
    {
      if (str) {free(str); str = NULL;}
      Xen_error(Xen_make_error_type("IO-error"),
		Xen_list_3(C_string_to_Xen_string(S_new_sound ": ~S, ~A"),
			   keys[0],
			   C_string_to_Xen_string(snd_io_strerror())));
    }

  chan = snd_reopen_write(str);
  lseek(chan, mus_header_data_location(), SEEK_SET);

  size = ch * mus_samples_to_bytes(df, len);
  if (size > 0)
    {
      unsigned char *buf;
      buf = (unsigned char *)calloc(size, sizeof(unsigned char));
      if (write(chan, buf, size) != size) fprintf(stderr, "new-sound %s write error", str);
      free(buf);
    }

  snd_close(chan, str);
  ss->open_requestor = FROM_NEW_SOUND;

  sp = sound_is_silence(snd_open_file(str, FILE_READ_WRITE));

  if (str) free(str);
  if (sp) return(C_int_to_Xen_sound(sp->index));
  return(Xen_false);
}
#endif

static Xen g_speed_control_style(Xen snd)
{
  #define H_speed_control_style "(" S_speed_control_style " :optional snd): speed control panel interpretation \
choice: " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone "."

  return(sound_get_global(snd, SP_SPEED_STYLE, S_speed_control_style));
}


static Xen g_set_speed_control_style(Xen speed, Xen snd) 
{
  int in_spd;
  speed_style_t spd;

  Xen_check_type(Xen_is_integer(speed), speed, 1, S_set S_speed_control_style, "an integer"); 

  in_spd = Xen_integer_to_C_int(speed);
  if (in_spd < 0)
    Xen_out_of_range_error(S_set S_speed_control_style, 1, speed, "invalid " S_speed_control_style);

  spd = (speed_style_t)in_spd;
  if (spd >= NUM_SPEED_CONTROL_STYLES)
    Xen_out_of_range_error(S_set S_speed_control_style, 1, speed, 
			   S_speed_control_style " should be " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone);

  return(sound_set_global(snd, speed, SP_SPEED_STYLE, S_set S_speed_control_style));
}

with_two_setter_args(g_set_speed_control_style_reversed, g_set_speed_control_style)


static Xen g_speed_control_tones(Xen snd)
{
  #define H_speed_control_tones "(" S_speed_control_tones " :optional snd): if " S_speed_control_style " is " S_speed_control_as_semitone ", this chooses the octave divisions (12)"
  return(sound_get_global(snd, SP_SPEED_TONES, S_speed_control_tones));
}


static Xen g_set_speed_control_tones(Xen val, Xen snd)
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_speed_control_tones, "a number"); 
  return(sound_set_global(snd, val, SP_SPEED_TONES, S_set S_speed_control_tones));
}

with_two_setter_args(g_set_speed_control_tones_reversed, g_set_speed_control_tones)


static Xen g_amp_control(Xen snd, Xen chn_n) 
{
  #define H_amp_control "(" S_amp_control " :optional snd chn): current amp slider setting"
  if (Xen_is_bound(chn_n))
    {
      chan_info *cp;
      Snd_assert_channel(S_amp_control, snd, chn_n, 1);
      cp = get_cp(snd, chn_n, S_amp_control);
      if (!cp) return(Xen_false);
      if (cp->amp_control)
	return(C_double_to_Xen_real(cp->amp_control[0]));
    }
  return(sound_get(snd, SP_AMP, S_amp_control));
}


static Xen g_set_amp_control(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_amp_control, "a number"); 

  if (Xen_is_bound(chn_n))
    {
      chan_info *cp;
      Snd_assert_channel(S_amp_control, snd, chn_n, 2);
      cp = get_cp(snd, chn_n, S_amp_control);
      if (!cp) return(Xen_false);
      if (!cp->amp_control)
	cp->amp_control = (mus_float_t *)calloc(1, sizeof(mus_float_t));
      cp->amp_control[0] = (mus_float_t)Xen_real_to_C_double(on);
      return(on);
    }

  return(sound_set(snd, on, SP_AMP, S_set S_amp_control));
}

with_three_setter_args(g_set_amp_control_reversed, g_set_amp_control)


static Xen g_amp_control_bounds(Xen snd) 
{
  #define H_amp_control_bounds "(" S_amp_control_bounds " :optional snd): current amp slider bounds (default: '(0.0 8.0))"
  return(sound_get_global(snd, SP_AMP_BOUNDS, S_amp_control_bounds));
}


static Xen g_set_amp_control_bounds(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_list(on), on, 1, S_set S_amp_control_bounds, "a list of the new min and max values"); 

  if ((Xen_list_length(on) != 2) ||
      (!(Xen_is_number(Xen_car(on)))) ||
      (!(Xen_is_number(Xen_cadr(on)))))
    Xen_wrong_type_arg_error(S_set S_amp_control_bounds, 1, on, "a list of 2 numbers");

  if (Xen_real_to_C_double(Xen_car(on)) >= Xen_real_to_C_double(Xen_cadr(on)))
    Xen_out_of_range_error(S_set S_amp_control_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_AMP_BOUNDS, S_set S_amp_control_bounds));
}

with_two_setter_args(g_set_amp_control_bounds_reversed, g_set_amp_control_bounds)


static Xen g_contrast_control(Xen snd) 
{
  #define H_contrast_control "(" S_contrast_control " :optional snd): current contrast slider setting"
  return(sound_get(snd, SP_CONTRAST, S_contrast_control));
}


static Xen g_set_contrast_control(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_contrast_control, "a number"); 
  return(sound_set(snd, on, SP_CONTRAST, S_set S_contrast_control));
}

with_two_setter_args(g_set_contrast_control_reversed, g_set_contrast_control)


static Xen g_contrast_control_bounds(Xen snd) 
{
  #define H_contrast_control_bounds "(" S_contrast_control_bounds " :optional snd): current contrast slider bounds (default: '(0.0 10.0))"
  return(sound_get_global(snd, SP_CONTRAST_BOUNDS, S_contrast_control_bounds));
}


static Xen g_set_contrast_control_bounds(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_list(on), on, 1, S_set S_contrast_control_bounds, "a list of the new min and max values"); 

  if ((Xen_list_length(on) != 2) ||
      (!(Xen_is_number(Xen_car(on)))) ||
      (!(Xen_is_number(Xen_cadr(on)))))
    Xen_wrong_type_arg_error(S_set S_contrast_control_bounds, 1, on, "a list of 2 numbers");

  if (Xen_real_to_C_double(Xen_car(on)) >= Xen_real_to_C_double(Xen_cadr(on)))
    Xen_out_of_range_error(S_set S_contrast_control_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_CONTRAST_BOUNDS, S_set S_contrast_control_bounds));
}

with_two_setter_args(g_set_contrast_control_bounds_reversed, g_set_contrast_control_bounds)


static Xen g_contrast_control_amp(Xen snd) 
{
  #define H_contrast_control_amp "(" S_contrast_control_amp " :optional snd): snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  return(sound_get_global(snd, SP_CONTRAST_AMP, S_contrast_control_amp));
}


static Xen g_set_contrast_control_amp(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_contrast_control_amp, "a number");
  return(sound_set_global(snd, on, SP_CONTRAST_AMP, S_set S_contrast_control_amp));
}

with_two_setter_args(g_set_contrast_control_amp_reversed, g_set_contrast_control_amp)


static Xen g_expand_control(Xen snd) 
{
  #define H_expand_control "(" S_expand_control " :optional snd): current expand slider setting"
  return(sound_get(snd, SP_EXPAND, S_expand_control));
}


static Xen g_set_expand_control(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_expand_control, "a number"); 
  return(sound_set(snd, on, SP_EXPAND, S_set S_expand_control));
}

with_two_setter_args(g_set_expand_control_reversed, g_set_expand_control)


static Xen g_expand_control_bounds(Xen snd) 
{
  #define H_expand_control_bounds "(" S_expand_control_bounds " :optional snd): current expand slider bounds (default: '(0.001 20.0))"
  return(sound_get_global(snd, SP_EXPAND_BOUNDS, S_expand_control_bounds));
}


static Xen g_set_expand_control_bounds(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_list(on), on, 1, S_set S_expand_control_bounds, "a list of the new min and max values"); 

  if ((Xen_list_length(on) != 2) ||
      (!(Xen_is_number(Xen_car(on)))) ||
      (!(Xen_is_number(Xen_cadr(on)))))
    Xen_wrong_type_arg_error(S_set S_expand_control_bounds, 1, on, "a list of 2 numbers");

  if (Xen_real_to_C_double(Xen_car(on)) >= Xen_real_to_C_double(Xen_cadr(on)))
    Xen_out_of_range_error(S_set S_expand_control_bounds, 1, on, "min >= max");

  if (Xen_real_to_C_double(Xen_car(on)) <= 0.0)
    Xen_out_of_range_error(S_set S_expand_control_bounds, 1, on, "min <= 0.0");

  return(sound_set_global(snd, on, SP_EXPAND_BOUNDS, S_set S_expand_control_bounds));
}

with_two_setter_args(g_set_expand_control_bounds_reversed, g_set_expand_control_bounds)


static Xen g_expand_control_length(Xen snd) 
{
  #define H_expand_control_length "(" S_expand_control_length " :optional snd): current expansion segment length in seconds (.15)"
  return(sound_get_global(snd, SP_EXPAND_LENGTH, S_expand_control_length));
}


static Xen g_set_expand_control_length(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_expand_control_length, "a number"); 
  return(sound_set_global(snd, on, SP_EXPAND_LENGTH, S_set S_expand_control_length));
}

with_two_setter_args(g_set_expand_control_length_reversed, g_set_expand_control_length)


static Xen g_expand_control_ramp(Xen snd) 
{
  #define H_expand_control_ramp "(" S_expand_control_ramp " :optional snd): current expansion ramp time (.4)"
  return(sound_get_global(snd, SP_EXPAND_RAMP, S_expand_control_ramp));
}


static Xen g_set_expand_control_ramp(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_expand_control_ramp, "a number");
  return(sound_set_global(snd, on, SP_EXPAND_RAMP, S_set S_expand_control_ramp));
}

with_two_setter_args(g_set_expand_control_ramp_reversed, g_set_expand_control_ramp)


static Xen g_expand_control_hop(Xen snd) 
{
  #define H_expand_control_hop "(" S_expand_control_hop " :optional snd): current expansion output grain spacing in seconds (0.05)"
  return(sound_get_global(snd, SP_EXPAND_HOP, S_expand_control_hop));
}


static Xen g_set_expand_control_hop(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_expand_control_hop, "a number"); 
  return(sound_set_global(snd, on, SP_EXPAND_HOP, S_set S_expand_control_hop));
}

with_two_setter_args(g_set_expand_control_hop_reversed, g_set_expand_control_hop)


static Xen g_expand_control_jitter(Xen snd) 
{
  #define H_expand_control_jitter "(" S_expand_control_jitter " :optional snd): current expansion output grain spacing jitter (0.1)"
  return(sound_get_global(snd, SP_EXPAND_JITTER, S_expand_control_jitter));
}


static Xen g_set_expand_control_jitter(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_expand_control_jitter, "a number"); 
  return(sound_set_global(snd, on, SP_EXPAND_JITTER, S_set S_expand_control_jitter));
}

with_two_setter_args(g_set_expand_control_jitter_reversed, g_set_expand_control_jitter)


static Xen g_speed_control(Xen snd) 
{
  #define H_speed_control "(" S_speed_control " :optional snd): current speed (srate) slider setting"
  return(sound_get(snd, SP_SPEED, S_speed_control));
}


static Xen g_set_speed_control(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_speed_control, "a number"); 
  return(sound_set(snd, on, SP_SPEED, S_set S_speed_control));
}

with_two_setter_args(g_set_speed_control_reversed, g_set_speed_control)


static Xen g_speed_control_bounds(Xen snd) 
{
  #define H_speed_control_bounds "(" S_speed_control_bounds " :optional snd): current speed slider bounds (default: '(0.05 20.0))"
  return(sound_get_global(snd, SP_SPEED_BOUNDS, S_speed_control_bounds));
}


static Xen g_set_speed_control_bounds(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_list(on), on, 1, S_set S_speed_control_bounds, "a list of the new min and max values"); 

  if ((Xen_list_length(on) != 2) ||
      (!(Xen_is_number(Xen_car(on)))) ||
      (!(Xen_is_number(Xen_cadr(on)))))
    Xen_wrong_type_arg_error(S_set S_speed_control_bounds, 1, on, "a list of 2 numbers");

  if (Xen_real_to_C_double(Xen_car(on)) >= Xen_real_to_C_double(Xen_cadr(on)))
    Xen_out_of_range_error(S_set S_speed_control_bounds, 1, on, "min >= max");

  if (Xen_real_to_C_double(Xen_car(on)) <= 0.0)
    Xen_out_of_range_error(S_set S_speed_control_bounds, 1, on, "min <= 0.0");

  return(sound_set_global(snd, on, SP_SPEED_BOUNDS, S_set S_speed_control_bounds));
}

with_two_setter_args(g_set_speed_control_bounds_reversed, g_set_speed_control_bounds)


static Xen g_reverb_control_length(Xen snd) 
{
  #define H_reverb_control_length "(" S_reverb_control_length " :optional snd): reverb decay length scaler"
  return(sound_get(snd, SP_REVERB_LENGTH, S_reverb_control_length));
}


static Xen g_set_reverb_control_length(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_reverb_control_length, "a number"); 
  return(sound_set(snd, on, SP_REVERB_LENGTH, S_set S_reverb_control_length));
}

with_two_setter_args(g_set_reverb_control_length_reversed, g_set_reverb_control_length)


static Xen g_reverb_control_length_bounds(Xen snd) 
{
  #define H_reverb_control_length_bounds "(" S_reverb_control_length_bounds " :optional snd): current reverb length slider bounds (default: '(0.0 5.0))"
  return(sound_get_global(snd, SP_REVERB_LENGTH_BOUNDS, S_reverb_control_length_bounds));
}


static Xen g_set_reverb_control_length_bounds(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_list(on), on, 1, S_set S_reverb_control_length_bounds, "a list of the new min and max values"); 

  if ((Xen_list_length(on) != 2) ||
      (!(Xen_is_number(Xen_car(on)))) ||
      (!(Xen_is_number(Xen_cadr(on)))))
    Xen_wrong_type_arg_error(S_set S_reverb_control_length_bounds, 1, on, "a list of 2 numbers");

  if (Xen_real_to_C_double(Xen_car(on)) >= Xen_real_to_C_double(Xen_cadr(on)))
    Xen_out_of_range_error(S_set S_reverb_control_length_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_REVERB_LENGTH_BOUNDS, S_set S_reverb_control_length_bounds));
}

with_two_setter_args(g_set_reverb_control_length_bounds_reversed, g_set_reverb_control_length_bounds)


static Xen g_reverb_control_feedback(Xen snd) 
{
  #define H_reverb_control_feedback "(" S_reverb_control_feedback " :optional snd): reverb feedback scaler"
  return(sound_get_global(snd, SP_REVERB_FEEDBACK, S_reverb_control_feedback));
}


static Xen g_set_reverb_control_feedback(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_reverb_control_feedback, "a number"); 
  return(sound_set_global(snd, on, SP_REVERB_FEEDBACK, S_set S_reverb_control_feedback));
}

with_two_setter_args(g_set_reverb_control_feedback_reversed, g_set_reverb_control_feedback)


static Xen g_reverb_control_scale(Xen snd) 
{
  #define H_reverb_control_scale "(" S_reverb_control_scale " :optional snd): reverb scaler (the amount of reverb)"
  return(sound_get(snd, SP_REVERB_SCALE, S_reverb_control_scale));
}


static Xen g_set_reverb_control_scale(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_reverb_control_scale, "a number"); 
  return(sound_set(snd, on, SP_REVERB_SCALE, S_set S_reverb_control_scale));
}

with_two_setter_args(g_set_reverb_control_scale_reversed, g_set_reverb_control_scale)


static Xen g_reverb_control_scale_bounds(Xen snd) 
{
  #define H_reverb_control_scale_bounds "(" S_reverb_control_scale_bounds " :optional snd): current reverb scale slider bounds (default: '(0.0 4.0))"
  return(sound_get_global(snd, SP_REVERB_SCALE_BOUNDS, S_reverb_control_scale_bounds));
}


static Xen g_set_reverb_control_scale_bounds(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_list(on), on, 1, S_set S_reverb_control_scale_bounds, "a list of the new min and max values"); 

  if ((Xen_list_length(on) != 2) ||
      (!(Xen_is_number(Xen_car(on)))) ||
      (!(Xen_is_number(Xen_cadr(on)))))
    Xen_wrong_type_arg_error(S_set S_reverb_control_scale_bounds, 1, on, "a list of 2 numbers");

  if (Xen_real_to_C_double(Xen_car(on)) >= Xen_real_to_C_double(Xen_cadr(on)))
    Xen_out_of_range_error(S_set S_reverb_control_scale_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_REVERB_SCALE_BOUNDS, S_set S_reverb_control_scale_bounds));
}

with_two_setter_args(g_set_reverb_control_scale_bounds_reversed, g_set_reverb_control_scale_bounds)


static Xen g_reverb_control_lowpass(Xen snd) 
{
  #define H_reverb_control_lowpass "(" S_reverb_control_lowpass " :optional snd): reverb lowpass filter coefficient"
  return(sound_get_global(snd, SP_REVERB_LOW_PASS, S_reverb_control_lowpass));
}


static Xen g_set_reverb_control_lowpass(Xen on, Xen snd) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_reverb_control_lowpass, "a number"); 
  return(sound_set_global(snd, on, SP_REVERB_LOW_PASS, S_set S_reverb_control_lowpass));
}

with_two_setter_args(g_set_reverb_control_lowpass_reversed, g_set_reverb_control_lowpass)


static Xen g_reverb_control_decay(Xen snd)
{
  #define H_reverb_control_decay "(" S_reverb_control_decay " :optional snd): " S_apply_controls " reverb decay time (1.0 seconds)"
  return(sound_get_global(snd, SP_REVERB_DECAY, S_reverb_control_decay));
}


static Xen g_set_reverb_control_decay(Xen val, Xen snd)
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_reverb_control_decay, "a number"); 
  return(sound_set_global(snd, val, SP_REVERB_DECAY, S_set S_reverb_control_decay));
}

with_two_setter_args(g_set_reverb_control_decay_reversed, g_set_reverb_control_decay)


static Xen g_filter_control_envelope(Xen snd)
{
  #define H_filter_control_envelope "(" S_filter_control_envelope " :optional snd): snd's filter envelope (in the control panel)"
  return(sound_get(snd, SP_FILTER_ENVELOPE, S_filter_control_envelope));
}


static Xen g_set_filter_control_envelope(Xen val, Xen snd)
{
  return(sound_set(snd, val, SP_FILTER_ENVELOPE, S_set S_filter_control_envelope));
}

with_two_setter_args(g_set_filter_control_envelope_reversed, g_set_filter_control_envelope)


static void squelch_printout(const char *msg, void *ignore)
{
}


static void apply_controls_error(const char *msg, void *data)
{
  redirect_snd_warning_to(NULL, NULL);
  redirect_snd_error_to(NULL, NULL);
  Xen_error(Xen_make_error_type("cannot-apply-controls"),
	    Xen_list_3(C_string_to_Xen_string("~A: ~A"),
		       C_string_to_Xen_string((char *)data),
		       C_string_to_Xen_string(msg)));
}


static Xen g_controls_to_channel(Xen settings, Xen beg, Xen dur, Xen snd, Xen chn, Xen origin)
{
  #define H_controls_to_channel "(" S_controls_to_channel " settings :optional beg dur snd chn origin) sets up \
snd's controls to reflect 'settings' (unspecified settings are not changed), then applies the controls as \
an edit of channel 'chn'. The 'settings' argument is a list:\n\
\n\
  (list amp speed\n\
    (list contrast contrast_amp)\n\
    (list expand expand_length expand_ramp expand_hop expand_jitter)\n\
    (list reverb_scale reverb_length reverb_feedback reverb_low_pass reverb_decay)\n\
    (list filter_order filter_env))\n\
\n\
where each inner list entry can also be " PROC_FALSE "."

  snd_info *sp;
  chan_info *cp;

  Xen_check_type(Xen_is_list(settings), settings, 1, S_controls_to_channel, "a list");
  Snd_assert_channel(S_controls_to_channel, snd, chn, 4);
  Xen_check_type(Xen_is_llong(beg) || Xen_is_false(beg) || !Xen_is_bound(beg), beg, 2, S_controls_to_channel, "an integer");
  Xen_check_type(Xen_is_llong(dur) || Xen_is_false(dur) || !Xen_is_bound(dur), dur, 3, S_controls_to_channel, "an integer");
  Xen_check_type(Xen_is_string_or_unbound(origin), origin, 7, S_controls_to_channel, "a string");

  sp = get_sp(snd); /* control changes make sense, but not 'apply' -- expecting just 'play' if a player */
  if (sp)
    {
      apply_state *ap;
      int old_selected_channel;
      ctrl_state *saved_settings;
      if (sp->applying)
	{
	  Xen_error(Xen_make_error_type("cannot-apply-controls"),
		    Xen_list_1(C_string_to_Xen_string(S_controls_to_channel ": already applying controls")));
	}
      if (Xen_is_llong(beg)) apply_beg = Xen_llong_to_C_llong(beg); else apply_beg = 0;
      if (Xen_is_llong(dur)) apply_dur = Xen_llong_to_C_llong(dur); else apply_dur = 0;
      cp = get_cp(snd, chn, S_controls_to_channel);
      old_selected_channel = sp->selected_channel;
      sp->selected_channel = cp->chan;
      saved_settings = current_control_settings(sp, NULL);

      /* now read the 'settings' list for any new settings */
      if ((Xen_is_list(settings)) && (!Xen_is_null(settings)))
	{
	  int i, len, elen;
	  Xen lst;
	  /* settings: 
	     (list amp speed
	       (list contrast contrast_amp)
	       (list expand expand_length expand_ramp expand_hop expand_jitter)
	       (list reverb_scale reverb_length reverb_feedback reverb_low_pass reverb_decay)
	       (list filter_order filter_env))
	     where any (outer) items can be #f
	  */
	  len = Xen_list_length(settings);
	  for (i = 0, lst = Xen_copy_arg(settings); i < len; i++, lst = Xen_cdr(lst))
	    {
	      Xen element;
	      element = Xen_car(lst);
	      switch (i)
		{
		case 0: 
		  if (Xen_is_number(element)) sp->amp_control = Xen_real_to_C_double(element);
		  break;

		case 1:
		  if (Xen_is_number(element)) sp->speed_control = Xen_real_to_C_double(element);
		  break;

		case 2:
		  if (Xen_is_list(element))
		    {
		      elen = Xen_list_length(element);
		      if (elen > 0) sp->contrast_control_on = true;
		      if (elen > 0) sp->contrast_control = Xen_real_to_C_double(Xen_car(element));
		      if (elen > 1) sp->contrast_control_amp = Xen_real_to_C_double(Xen_cadr(element));
		    }
		  break;

		case 3:
		  if (Xen_is_list(element))
		    {
		      elen = Xen_list_length(element);
		      if (elen > 0) sp->expand_control_on = true;
		      if (elen > 0) sp->expand_control = Xen_real_to_C_double(Xen_car(element));
		      if (elen > 1) sp->expand_control_length = Xen_real_to_C_double(Xen_cadr(element));
		      if (elen > 2) sp->expand_control_ramp = Xen_real_to_C_double(Xen_caddr(element));
		      if (elen > 3) sp->expand_control_hop = Xen_real_to_C_double(Xen_list_ref(element, 3));
		      if (elen > 4) sp->expand_control_jitter = Xen_real_to_C_double(Xen_list_ref(element, 4));
		    }
		  break;

		case 4:
		  if (Xen_is_list(element))
		    {
		      elen = Xen_list_length(element);
		      if (elen > 0) sp->reverb_control_on = true;
		      if (elen > 0) sp->reverb_control_scale = Xen_real_to_C_double(Xen_car(element));
		      if (elen > 1) sp->reverb_control_length = Xen_real_to_C_double(Xen_cadr(element));
		      if (elen > 2) sp->reverb_control_feedback = Xen_real_to_C_double(Xen_caddr(element));
		      if (elen > 3) sp->reverb_control_lowpass = Xen_real_to_C_double(Xen_list_ref(element, 3));
		      if (elen > 4) sp->reverb_control_decay = Xen_real_to_C_double(Xen_list_ref(element, 4));
		    }
		  break;

		case 5:
		  if (Xen_is_list(element))
		    {
		      elen = Xen_list_length(element);
		      if (elen > 0) sp->filter_control_on = true;
		      if (elen > 0) sp->filter_control_order = Xen_integer_to_C_int(Xen_car(element));
		      if (elen > 1) sp->filter_control_envelope = get_env(Xen_cadr(element), S_controls_to_channel);
		    }
		}
	    }
	}

      ss->apply_choice = APPLY_TO_CHANNEL;
      sp->applying = true;
      ap = (apply_state *)make_apply_state(sp);

#if HAVE_EXTENSION_LANGUAGE
#if HAVE_FORTH
      if (!(Xen_is_number(dur)))
	ap->origin = mus_format("%s %" print_mus_long PROC_SEP PROC_FALSE " %s", 
				Xen_object_to_C_string(settings), 
				apply_beg, S_controls_to_channel);
      else ap->origin = mus_format("%s " PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long " %s", 
				   Xen_object_to_C_string(settings), 
				   apply_beg, apply_dur, S_controls_to_channel);
#else
      {
	char *temp = NULL;
	if (!(Xen_is_number(dur)))
	  ap->origin = mus_format("%s" PROC_OPEN "%s%s" PROC_SEP "%" print_mus_long PROC_SEP PROC_FALSE, 
				  to_proc_name(S_controls_to_channel), 
				  PROC_QUOTE,
				  temp = Xen_object_to_C_string(settings), 
				  apply_beg);
	else ap->origin = mus_format("%s" PROC_OPEN "%s%s" PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long, 
				     to_proc_name(S_controls_to_channel), 
				     PROC_QUOTE,
				     temp = Xen_object_to_C_string(settings), 
				     apply_beg, apply_dur);
#if HAVE_SCHEME
	if (temp) free(temp);
#endif
      }
#endif
#endif

      if (ap)
	{
	  redirect_snd_error_to(apply_controls_error, (void *)S_controls_to_channel);
	  redirect_snd_warning_to(squelch_printout, NULL);
	  while (apply_controls(ap)) {};
	  redirect_snd_warning_to(NULL, NULL); /* no-op message pointless within xen */
	  redirect_snd_error_to(NULL, NULL);
	}
      sp->selected_channel = old_selected_channel;
      restore_control_settings(sp, saved_settings);
      free_control_settings(saved_settings);
    }

  return(settings);
}


static Xen g_apply_controls(Xen snd, Xen choice, Xen beg, Xen dur)
{
  #define H_apply_controls "(" S_apply_controls " :optional snd (choice 0) (beg 0) (dur len)): \
applies the current control panel state as an edit. \
The 'choices' are 0 (apply to sound), 1 (apply to channel), and 2 (apply to selection).  If 'beg' is given, the apply starts there."

  snd_info *sp;

  Snd_assert_sound(S_apply_controls, snd, 1);
  Xen_check_type(Xen_is_integer_or_unbound(choice), choice, 2, S_apply_controls, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(beg), beg, 3, S_apply_controls, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(dur), dur, 4, S_apply_controls, "an integer");

  sp = get_sp(snd); /* control changes make sense, but not 'apply' -- expecting just 'play' if a player */
  if (sp)
    {
      apply_state *ap;
      snd_apply_t cur_choice = APPLY_TO_SOUND;

      if (sp->applying)
	{
	  Xen_error(Xen_make_error_type("cannot-apply-controls"),
		    Xen_list_1(C_string_to_Xen_string(S_apply_controls ": already applying controls")));
	}

      if (Xen_is_llong(beg)) apply_beg = Xen_llong_to_C_llong(beg); else apply_beg = 0;
      if (Xen_is_llong(dur)) apply_dur = Xen_llong_to_C_llong(dur); else apply_dur = 0;

      if (Xen_is_integer(choice))
	cur_choice = (snd_apply_t)Xen_integer_to_C_int(choice);
      if (cur_choice > APPLY_TO_SELECTION)
	Xen_out_of_range_error(S_apply_controls, 2, choice, "choice must be 0=sound, 1=channel, or 2=selection");

      ss->apply_choice = cur_choice;
      sp->applying = true;
      ap = (apply_state *)make_apply_state(sp);

      if (ap)
	{
	  redirect_snd_error_to(apply_controls_error, (void *)S_apply_controls);
	  redirect_snd_warning_to(squelch_printout, NULL);
	  while (apply_controls(ap)) {};
	  redirect_snd_warning_to(NULL, NULL); /* no-op message pointless within xen */
	  redirect_snd_error_to(NULL, NULL);
	}
      return(snd);
    }
  return(snd_no_such_sound_error(S_apply_controls, snd));
}


/* ---------------------------------------- peak env files ---------------------------------------- */

static int pack_env_info_type(void)
{
  /* put data description in peak-env info file (in case user opens it from incompatible machine) */
  int val = 0;
#if MUS_LITTLE_ENDIAN
  val |= (1 << 8);
#endif
  val |= (1 << 9); /* always float now */
  val |= (sizeof(mus_float_t) << 10);
  return(val);
}


static char *peak_clean(const char *name)
{
  int len, i;
  char *peak_name;
  len = mus_strlen(name);
  peak_name = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    {
      if ((name[i] == '\\') ||
	  (name[i] == '/'))
	peak_name[i] = '_';
      else peak_name[i] = name[i];
    }
  return(peak_name);
}


static char *expanded_peak_name(const char *name, int chan)
{
  char *fullname, *peak_file_name, *clean_name;

  clean_name = peak_clean(name);
  peak_file_name = mus_format("%s/%s-peaks-%d", peak_env_dir(ss), clean_name, chan);
  fullname = mus_expand_filename(peak_file_name);

  if (clean_name) free(clean_name);
  if (peak_file_name) free(peak_file_name);
  return(fullname);
}


void delete_peak_env_info_file(chan_info *cp)
{
  char *fullname;
  fullname = expanded_peak_name(cp->sound->filename, cp->chan);

  if (mus_file_probe(fullname))
    remove(fullname);

  if (fullname) free(fullname);
}


#define PEAK_ENV_VERSION 0
#define PEAK_ENV_INTS 5
#define PEAK_ENV_SAMPS 2


bool write_peak_env_info_file(chan_info *cp)
{
  char *fullname;
  peak_env_info *ep;
  int fd;
  int ibuf[PEAK_ENV_INTS];
  mus_float_t mbuf[PEAK_ENV_SAMPS];
  ssize_t bytes;

  if (!(cp->edits)) return(true);
  ep = cp->edits[0]->peak_env;
  if (!ep) return(false);

  fullname = expanded_peak_name(cp->sound->filename, cp->chan);
  fd = mus_file_create(fullname);
  if (fd == -1)
    {
      if (fullname) free(fullname);
      return(false);
    }

  ibuf[0] = ((ep->completed) ? 1 : 0) | PEAK_ENV_VERSION | (pack_env_info_type() << 16);
  ibuf[1] = ep->peak_env_size;
  ibuf[2] = ep->samps_per_bin;
  ibuf[3] = ep->bin;
  ibuf[4] = ep->top_bin;
  mbuf[0] = ep->fmin;
  mbuf[1] = ep->fmax;

  bytes = write(fd, (char *)ibuf, (PEAK_ENV_INTS * sizeof(int)));
  if (bytes != 0) bytes = write(fd, (char *)mbuf, (PEAK_ENV_SAMPS * sizeof(mus_float_t)));
  if (bytes != 0) bytes = write(fd, (char *)(ep->data_min), (ep->peak_env_size * sizeof(mus_float_t)));
  if (bytes != 0) bytes = write(fd, (char *)(ep->data_max), (ep->peak_env_size * sizeof(mus_float_t)));
  if (bytes == 0) fprintf(stderr, "write error while writing peak env file");

  snd_close(fd, fullname);
  if (fullname) free(fullname);
  return(true);
}


typedef enum {PEAK_ENV_NO_ERROR, PEAK_ENV_BAD_HEADER, PEAK_ENV_BAD_FORMAT, PEAK_ENV_BAD_SIZE, PEAK_ENV_NO_FILE, PEAK_ENV_NO_DATA} peak_env_error_t;
static const char *peak_env_error[6] = {
  "no error", 
  "peak-env file has a bad header!", 
  "peak-env file is in the wrong sample type; will re-make it.", 
  "peak-env file size is messed up!", 
  "peak-env file has vanished!", 
  "peak-env file is empty!"};

static bool peak_env_info_type_ok(int val)
{
  return((val == 0) ||                            /* for backwards compatibility */
	 (val == pack_env_info_type()));
}


static peak_env_info *get_peak_env_info(const char *fullname, peak_env_error_t *error)
{
  peak_env_info *ep;
  int fd, hdr = 0;
  ssize_t bytes;
  int ibuf[PEAK_ENV_INTS];
  mus_float_t mbuf[PEAK_ENV_SAMPS];

  fd = mus_file_open_read(fullname);
  if (fd == -1) 
    {
      (*error) = PEAK_ENV_NO_FILE;
      return(NULL);
    }

  bytes = read(fd, (char *)ibuf, (PEAK_ENV_INTS * sizeof(int)));
  if (bytes != (PEAK_ENV_INTS * sizeof(int)))
    {
      snd_close(fd, fullname);
      (*error) = PEAK_ENV_NO_DATA;
      return(NULL);
    }

  hdr = ibuf[0];
  (*error) = PEAK_ENV_NO_ERROR;
  if (((hdr & 0xf) != 0) && ((hdr & 0xf) != 1)) 
    (*error) = PEAK_ENV_BAD_HEADER;
  else
    {
      if (!(peak_env_info_type_ok(hdr >> 16)))
	(*error) = PEAK_ENV_BAD_FORMAT;
      else
	{
	  if ((ibuf[1] <= 0) || (!(is_power_of_2(ibuf[1]))))
	    (*error) = PEAK_ENV_BAD_SIZE;
	  else
	    {
	      if ((ibuf[2] <= 0) || (ibuf[4] > ibuf[1]))
		(*error) = PEAK_ENV_BAD_HEADER;
	    }
	}
    }

  if ((*error) != PEAK_ENV_NO_ERROR) 
    {
      snd_close(fd, fullname);
      return(NULL);
    }

  ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
  ep->completed = (bool)(hdr & 0xf); /* version number in higher bits */
  ep->peak_env_size = ibuf[1];
  ep->samps_per_bin = ibuf[2];
  ep->bin = ibuf[3];
  ep->top_bin = ibuf[4];

  if (read(fd, (char *)mbuf, (PEAK_ENV_SAMPS * sizeof(mus_float_t))) == 0) fprintf(stderr, "%s: read error", fullname);

  ep->fmin = mbuf[0];
  ep->fmax = mbuf[1];

  ep->data_min = (mus_float_t *)malloc(ep->peak_env_size * sizeof(mus_float_t));
  ep->data_max = (mus_float_t *)malloc(ep->peak_env_size * sizeof(mus_float_t));

  if (read(fd, (char *)(ep->data_min), (ep->peak_env_size * sizeof(mus_float_t))) == 0) fprintf(stderr, "%s: read error", fullname);
  if (read(fd, (char *)(ep->data_max), (ep->peak_env_size * sizeof(mus_float_t))) == 0) fprintf(stderr, "%s: read error", fullname);

  snd_close(fd, fullname);
  return(ep);
}


const char *read_peak_env_info_file(chan_info *cp)
{
  peak_env_error_t err = PEAK_ENV_NO_ERROR;
  char *fullname;

  if (!(cp->edits)) return(NULL);

  fullname = expanded_peak_name(cp->sound->filename, cp->chan);
  if (mus_file_probe(fullname))
    {
      if (file_write_date(fullname) > cp->sound->write_date)
	cp->edits[0]->peak_env = get_peak_env_info(fullname, &err);
      else remove(fullname);
    }
  if (fullname) free(fullname);

  if ((!cp->edits[0]->peak_env) &&
      (err != PEAK_ENV_NO_ERROR))
    return(peak_env_error[(int)err]);

  return(NULL);
}


static Xen g_peak_env_info_to_vcts(peak_env_info *ep, int len)
{
  /* changed 5-Jan-03 to return vcts */
  /* in snd-test this causes unfreed memory because the sound-icon-box saves all the data for each icon (vcts unfreed) */
  Xen res;
  int i, lim;
  vct *vmax, *vmin;
  mus_float_t *maxdata, *mindata;
  int loc;

  if ((len == 0) || (len > ep->peak_env_size))
    lim = ep->peak_env_size;
  else lim = len;
  if (lim <= 0) return(Xen_empty_list);

  res = Xen_list_2(xen_make_vct(lim, (mus_float_t *)calloc(lim, sizeof(mus_float_t))),
		   xen_make_vct(lim, (mus_float_t *)calloc(lim, sizeof(mus_float_t))));
  loc = snd_protect(res);

  vmin = xen_to_vct(Xen_car(res));
  vmax = xen_to_vct(Xen_cadr(res));
  mindata = mus_vct_data(vmin);
  maxdata = mus_vct_data(vmax);

  if (ep->peak_env_size == lim)
    {
      for (i = 0; i < lim; i++)
	{
	  mindata[i] = ep->data_min[i];
	  maxdata[i] = ep->data_max[i];
	}
    }
  else
    {
      mus_float_t cmax, cmin, incr, x;
      int j;
      incr = (mus_float_t)(ep->peak_env_size - 1) / (mus_float_t)lim; /* make extra room on left */
      cmax = ep->fmin;
      cmin = ep->fmax;
      mindata[0] = ep->data_min[0];
      maxdata[0] = ep->data_max[0];
      for (i = 1, j = 1, x = 0.0; i < ep->peak_env_size; i++)
	{
	  if (ep->data_max[i] > cmax) cmax = ep->data_max[i];
	  if (ep->data_min[i] < cmin) cmin = ep->data_min[i];
	  x += 1.0;
	  if (x >= incr)
	    {
	      mindata[j] = cmin;
	      maxdata[j++] = cmax;
	      x -= incr;
	      cmax = ep->fmin;
	      cmin = ep->fmax;
	      if (j == lim) break;
	    }
	}
    }
  snd_unprotect_at(loc);
  return(res);
}


#if (!USE_NO_GUI)
typedef struct {
  chan_info *cp;
  env_state *es;
  int len;
  Xen filename;
  Xen func;
  int func_gc_loc;
} env_tick;

static idle_func_t tick_it(any_pointer_t pet)
{
  bool val;
  env_state *es;
  chan_info *cp;
  env_tick *et = (env_tick *)pet;
  es = et->es;
  cp = et->cp;
  val = tick_peak_env(cp, es);
  if (val)
    {
      es = free_env_state(es);
      if (Xen_is_procedure(et->func))
	{
	  int loc;
	  Xen peak;
	  peak = g_peak_env_info_to_vcts(cp->edits[0]->peak_env, et->len);
	  loc = snd_protect(peak);
	  Xen_call_with_3_args(et->func,
		     et->filename,
		     C_int_to_Xen_integer(cp->chan),
		     peak,
		     "amp env tick");
	  snd_unprotect_at(et->func_gc_loc);
	  snd_unprotect_at(loc);
	}
      completely_free_snd_info(cp->sound);
      free(et);
      return(BACKGROUND_QUIT);
    }
  return(BACKGROUND_CONTINUE);
}
#endif


static Xen g_channel_amp_envs(Xen filename, Xen chan, Xen pts, Xen peak_func, Xen done_func)
{
  /* return two vectors of size pts containing y vals (min and max) of amp env
   *   if peak_func, use it to get peak_env_info file if needed
   *   if done_func set workproc that calls it when done
   */
  #define H_channel_amp_envs "(" S_channel_amp_envs " :optional file (chan 0) size peak-file-func work-proc-func): \
return two " S_vct "s of length 'size' containing y vals (min and max) of file's channel chan's amp envs. \
'peak-file-func' is used to get the name of the associated peak_env_info file if the file is very large. \
'work-proc-func' is called when the amp envs are ready if the amp envs are gathered in the background. \
If 'filename' is a sound index or a sound object, 'size' is interpreted as an edit-position, and the current amp envs are returned."

  char *fullname = NULL;
  int len = 0, chn = 0;
  snd_info *sp = NULL;
  chan_info *cp = NULL;
  peak_env_error_t err = PEAK_ENV_NO_ERROR;

  Xen_check_type(Xen_is_string(filename) || Xen_is_integer(filename) || !Xen_is_bound(filename) || xen_is_sound(filename), 
		  filename, 1, S_channel_amp_envs, "a string or sound index");
  Xen_check_type(Xen_is_integer_or_unbound(chan), chan, 2, S_channel_amp_envs, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(pts), pts, 3, S_channel_amp_envs, "an integer");

  Xen_check_type(((Xen_is_procedure(peak_func)) && (procedure_arity_ok(peak_func, 2))) ||
		  (Xen_is_false(peak_func)) ||
		  (!Xen_is_bound(peak_func)), 
		  peak_func, 4, S_channel_amp_envs, "a procedure of 2 args");
  Xen_check_type(((Xen_is_procedure(done_func)) && (procedure_arity_ok(done_func, 3))) ||
		  (Xen_is_false(done_func)) ||
		  (!Xen_is_bound(done_func)), 
		  done_func, 5, S_channel_amp_envs, "a procedure of 3 args");

  if (!(Xen_is_string(filename)))
    {
      cp = get_cp(filename, chan, S_channel_amp_envs);
      if (cp)
	{
	  env_state *es;
	  peak_env_info *ep;
	  int pos;

	  pos = to_c_edit_position(cp, pts, S_channel_amp_envs, 3); /* here "pts" is edpos, not vector size */
	  if (!cp->edits)
	    return(Xen_empty_list);

	  ep = cp->edits[pos]->peak_env; /* this can be null -- we run the peak envs if necessary */
	  if ((ep) &&
	      (ep->completed))
	    return(g_peak_env_info_to_vcts(ep, ep->peak_env_size));

	  /* force amp env to completion */
	  stop_peak_env(cp);
	  es = make_env_state(cp, cp->edits[pos]->samples);
	  if (es)
	    {
	      while (!(tick_peak_env(cp, es))) {};
	      free_env_state(es);
	      ep = cp->edits[pos]->peak_env;
	      if (ep)
		return(g_peak_env_info_to_vcts(ep, ep->peak_env_size));
	    }
	  return(Xen_empty_list);
	}
      /* else get_cp threw an error */
    }

  /* filename is a string from here down */

  fullname = mus_expand_filename(Xen_string_to_C_string(filename));
  if (Xen_is_integer(chan)) chn = Xen_integer_to_C_int(chan);
  if (chn < 0)
    Xen_out_of_range_error(S_channel_amp_envs, 2, chan, "must be >= 0");
  if (Xen_is_integer(pts)) len = Xen_integer_to_C_int(pts);

  /* look for sp->filename = fullname
     then peak
     then read direct (via make_sound_readable)
  */

  sp = find_sound(fullname, 0);
  if (sp)
    {
      if (chn < (int)sp->nchans)
	{
	  cp = sp->chans[chn];
	  if (cp->edits[0]->peak_env)
	    {
	      if (fullname) free(fullname);
	      /* here len can be 0 */
	      return(g_peak_env_info_to_vcts(cp->edits[0]->peak_env, len));
	    }
	}
      else
	{
	  if (fullname) free(fullname);
	  Xen_error(NO_SUCH_CHANNEL, 
		    Xen_list_3(C_string_to_Xen_string(S_channel_amp_envs ": no such channel (~A in ~S)"),
			       chan,
			       filename));
	  return(Xen_false);
	}
    }

  if (!(mus_file_probe(fullname)))
    {
      if (fullname) free(fullname);
      Xen_error(NO_SUCH_FILE, 
		Xen_list_2(C_string_to_Xen_string(S_channel_amp_envs ": no such file: ~S"),
			   filename));
      return(Xen_false);
    }
  if (mus_sound_chans(fullname) < chn)
    {
      if (fullname) free(fullname);
      Xen_error(NO_SUCH_CHANNEL, 
		Xen_list_3(C_string_to_Xen_string(S_channel_amp_envs ": no such channel (~A in ~S)"),
			   chan,
			   filename));
      return(Xen_false);
    }

  if (Xen_is_procedure(peak_func))
    {
      Xen peak_filename;
      peak_filename = Xen_call_with_2_args(peak_func,
				 filename,
				 chan,
				 "peak env filename procedure");
      if (Xen_is_string(peak_filename))
	{
	  char *peakname;
	  peakname = mus_expand_filename(Xen_string_to_C_string(peak_filename));
	  if (mus_file_probe(peakname))
	    {
	      peak_env_info *ep;
	      ep = get_peak_env_info(peakname, &err);
	      if (ep)
		{
		  Xen vcts;
		  vcts = g_peak_env_info_to_vcts(ep, len);
		  free_peak_env_info(ep);
		  if (peakname) free(peakname);
		  if (fullname) free(fullname);
		  return(vcts);
		}
	    }
	  /* the else side (no such file) could be considered a request to make the peak env file (i.e. not necessarily an error) */
	  if (peakname) {free(peakname); peakname = NULL;}
	}
    }

  /* now set up to read direct... */
  sp = make_sound_readable(fullname, false);
  if (fullname) free(fullname);
  fullname = NULL;
  if ((sp) &&
      (chn < (int)sp->nchans))
    {
      cp = sp->chans[chn];
      if (cp)
	{
	  Xen peak = Xen_false;
	  env_state *es;
	  es = make_env_state(cp, cp->edits[0]->samples);
	  if (es)
	    {
#if (!USE_NO_GUI)
	      if (Xen_is_procedure(done_func))
		{
		  int id;
		  env_tick *et;

		  if (len <= 0)
		    Xen_out_of_range_error(S_channel_amp_envs, 3, pts, "must be > 0");

		  et = (env_tick *)calloc(1, sizeof(env_tick));
		  et->cp = cp;
		  et->es = es;
		  et->func = done_func;
		  et->func_gc_loc = snd_protect(done_func);
		  et->len = len;
		  et->filename = filename;
		  id = (int)BACKGROUND_ADD(tick_it, (any_pointer_t)et);
		  return(C_int_to_Xen_integer(id));
		}
#endif
	      while (!(tick_peak_env(cp, es))) {};
	      free_env_state(es);
	      peak = g_peak_env_info_to_vcts(cp->edits[0]->peak_env, len);
	    }
	  cp->active = CHANNEL_INACTIVE;
	  completely_free_snd_info(sp);
	  return(peak);
	}
    }
  return(Xen_false);
}

/* -------------------------------------------------------------------------------- */


static Xen g_start_progress_report(Xen snd, Xen chn)
{
  #define H_start_progress_report "(" S_start_progress_report " :optional snd chn): post the hour-glass icon"
  chan_info *cp;

  Snd_assert_channel(S_start_progress_report, snd, chn, 1);
  cp = get_cp(snd, chn, S_start_progress_report);
  if (!cp)
    return(snd_no_such_channel_error(S_start_progress_report, snd, chn));

  start_progress_report(cp);

  return(Xen_true);
}


static Xen g_finish_progress_report(Xen snd, Xen chn)
{
  #define H_finish_progress_report "(" S_finish_progress_report " :optional snd chn): remove the hour-glass icon"
  chan_info *cp;

  Snd_assert_channel(S_finish_progress_report, snd, chn, 1);
  cp = get_cp(snd, chn, S_finish_progress_report);
  if (!cp)
    return(snd_no_such_channel_error(S_finish_progress_report, snd, chn));

  finish_progress_report(cp);

  return(Xen_false);
}


static Xen g_progress_report(Xen pct, Xen snd, Xen chn)
{
  #define H_progress_report "(" S_progress_report " pct :optional snd chn): \
update an on-going 'progress report' (an animated hour-glass icon) in snd's channel chn using pct to indicate how far along we are"
  chan_info *cp;

  Snd_assert_channel(S_progress_report, snd, chn, 2);
  cp = get_cp(snd, chn, S_progress_report);
  if (!cp)
    return(snd_no_such_channel_error(S_progress_report, snd, chn));

  Xen_check_type(Xen_is_number(pct), pct, 1, S_progress_report, "a number");

  progress_report(cp, Xen_real_to_C_double(pct));
  return(pct);
}


static Xen g_sounds(void)
{
  #define H_sounds "(" S_sounds "): list of active sounds"
  int i;
  Xen result;
  result = Xen_empty_list;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	result = Xen_cons(C_int_to_Xen_sound(i),
			  result);
    }
  return(result);
}


static Xen g_status_report(Xen msg, Xen snd)
{
  #define H_status_report "(" S_status_report " message :optional snd) posts message in snd's status area.\
If 'snd' is not a currently open sound, the message is sent to the listener, if it is open. \
If there is no sound or listener, it is sent to stderr."

  snd_info *sp;
  const char *message;

  Xen_check_type(Xen_is_string(msg), msg, 1, S_status_report, "a string");
  Snd_assert_sound(S_status_report, snd, 2);

  message = Xen_string_to_C_string(msg);
  sp = get_sp(snd);

  if ((!sp) || 
      (sp->inuse != SOUND_NORMAL))
    {
      if ((message) && (*message))
	{
	  if (listener_exists())
	    append_listener_text(-1, message);
	  else fprintf(stderr, "%s", message);
	}
    }
  else
    {
      if ((message) && (*message))
	set_status(sp, message, false);
      else clear_status_area(sp);
    }
  return(msg);
}


Xen_wrap_1_arg(g_is_sound_w, g_is_sound)
Xen_wrap_2_optional_args(g_find_sound_w, g_find_sound)
Xen_wrap_1_optional_arg(g_channels_w, g_channels)
Xen_wrap_2_optional_args(g_set_channels_w, g_set_channels)
Xen_wrap_1_optional_arg(g_srate_w, g_srate)
Xen_wrap_2_optional_args(g_set_srate_w, g_set_srate)
Xen_wrap_1_optional_arg(g_data_location_w, g_data_location)
Xen_wrap_2_optional_args(g_set_data_location_w, g_set_data_location)
Xen_wrap_1_optional_arg(g_data_size_w, g_data_size)
Xen_wrap_2_optional_args(g_set_data_size_w, g_set_data_size)
Xen_wrap_1_optional_arg(g_sample_type_w, g_sample_type)
Xen_wrap_2_optional_args(g_set_sample_type_w, g_set_sample_type)
Xen_wrap_1_optional_arg(g_header_type_w, g_header_type)
Xen_wrap_2_optional_args(g_set_header_type_w, g_set_header_type)
Xen_wrap_1_optional_arg(g_comment_w, g_comment)
Xen_wrap_2_optional_args(g_set_comment_w, g_set_comment)
Xen_wrap_1_optional_arg(g_file_name_w, g_file_name)
Xen_wrap_1_optional_arg(g_short_file_name_w, g_short_file_name)
Xen_wrap_1_optional_arg(g_save_controls_w, g_save_controls)
Xen_wrap_1_optional_arg(g_restore_controls_w, g_restore_controls)
Xen_wrap_1_optional_arg(g_reset_controls_w, g_reset_controls)
Xen_wrap_no_args(g_selected_sound_w, g_selected_sound)
Xen_wrap_1_optional_arg(g_selected_channel_w, g_selected_channel)
Xen_wrap_2_optional_args(g_set_selected_channel_w, g_set_selected_channel)
Xen_wrap_1_arg(g_select_sound_w, g_select_sound)
Xen_wrap_1_optional_arg(g_select_channel_w, g_select_channel)
Xen_wrap_1_optional_arg(g_close_sound_w, g_close_sound)
Xen_wrap_1_optional_arg(g_update_sound_w, g_update_sound)
Xen_wrap_1_optional_arg(g_save_sound_w, g_save_sound)
Xen_wrap_1_arg(g_open_sound_w, g_open_sound)
Xen_wrap_1_arg(g_view_sound_w, g_view_sound)
Xen_wrap_1_optional_arg(g_revert_sound_w, g_revert_sound)
Xen_wrap_4_optional_args(g_apply_controls_w, g_apply_controls)
Xen_wrap_6_optional_args(g_controls_to_channel_w, g_controls_to_channel)
Xen_wrap_1_optional_arg(g_filter_control_envelope_w, g_filter_control_envelope)
Xen_wrap_1_optional_arg(g_show_controls_w, g_show_controls)
Xen_wrap_1_optional_arg(g_sync_w, g_sync)
Xen_wrap_no_args(g_sync_max_w, g_sync_max)
Xen_wrap_1_optional_arg(g_sound_properties_w, g_sound_properties)
Xen_wrap_2_optional_args(g_sound_property_w, g_sound_property)
Xen_wrap_1_optional_arg(g_channel_style_w, g_channel_style)
Xen_wrap_1_optional_arg(g_read_only_w, g_read_only)
Xen_wrap_1_optional_arg(g_expand_control_on_w, g_expand_control_on)
Xen_wrap_1_optional_arg(g_contrast_control_on_w, g_contrast_control_on)
Xen_wrap_1_optional_arg(g_reverb_control_on_w, g_reverb_control_on)
Xen_wrap_1_optional_arg(g_filter_control_on_w, g_filter_control_on)
Xen_wrap_1_optional_arg(g_filter_control_in_dB_w, g_filter_control_in_dB)
Xen_wrap_1_optional_arg(g_filter_control_in_hz_w, g_filter_control_in_hz)
Xen_wrap_1_optional_arg(g_filter_control_coeffs_w, g_filter_control_coeffs)
Xen_wrap_1_optional_arg(g_filter_control_order_w, g_filter_control_order)
Xen_wrap_1_optional_arg(g_contrast_control_w, g_contrast_control)
Xen_wrap_1_optional_arg(g_contrast_control_bounds_w, g_contrast_control_bounds)
Xen_wrap_1_optional_arg(g_contrast_control_amp_w, g_contrast_control_amp)
Xen_wrap_1_optional_arg(g_expand_control_w, g_expand_control)
Xen_wrap_1_optional_arg(g_expand_control_bounds_w, g_expand_control_bounds)
Xen_wrap_1_optional_arg(g_expand_control_length_w, g_expand_control_length)
Xen_wrap_1_optional_arg(g_expand_control_ramp_w, g_expand_control_ramp)
Xen_wrap_1_optional_arg(g_expand_control_hop_w, g_expand_control_hop)
Xen_wrap_1_optional_arg(g_expand_control_jitter_w, g_expand_control_jitter)
Xen_wrap_1_optional_arg(g_speed_control_w, g_speed_control)
Xen_wrap_1_optional_arg(g_speed_control_bounds_w, g_speed_control_bounds)
Xen_wrap_1_optional_arg(g_reverb_control_length_w, g_reverb_control_length)
Xen_wrap_1_optional_arg(g_reverb_control_length_bounds_w, g_reverb_control_length_bounds)
Xen_wrap_1_optional_arg(g_reverb_control_scale_w, g_reverb_control_scale)
Xen_wrap_1_optional_arg(g_reverb_control_scale_bounds_w, g_reverb_control_scale_bounds)
Xen_wrap_1_optional_arg(g_reverb_control_feedback_w, g_reverb_control_feedback)
Xen_wrap_1_optional_arg(g_reverb_control_lowpass_w, g_reverb_control_lowpass)
Xen_wrap_2_optional_args(g_amp_control_w, g_amp_control)
Xen_wrap_1_optional_arg(g_amp_control_bounds_w, g_amp_control_bounds)
Xen_wrap_1_optional_arg(g_reverb_control_decay_w, g_reverb_control_decay)
Xen_wrap_1_optional_arg(g_speed_control_style_w, g_speed_control_style)
Xen_wrap_1_optional_arg(g_speed_control_tones_w, g_speed_control_tones)
Xen_wrap_5_optional_args(g_channel_amp_envs_w, g_channel_amp_envs);
Xen_wrap_2_optional_args(g_start_progress_report_w, g_start_progress_report)
Xen_wrap_2_optional_args(g_finish_progress_report_w, g_finish_progress_report)
Xen_wrap_3_optional_args(g_progress_report_w, g_progress_report)
Xen_wrap_no_args(g_sounds_w, g_sounds)
Xen_wrap_1_arg(g_integer_to_sound_w, g_integer_to_sound)
Xen_wrap_1_arg(g_sound_to_integer_w, g_sound_to_integer)
Xen_wrap_2_optional_args(g_status_report_w, g_status_report)
#if HAVE_SCHEME
#define g_set_filter_control_envelope_w g_set_filter_control_envelope_reversed
#define g_set_read_only_w g_set_read_only_reversed
#define g_set_sound_properties_w g_set_sound_properties_reversed
#define g_set_sound_property_w g_set_sound_property_reversed
#define g_set_sync_w g_set_sync_reversed
#define g_set_channel_style_w g_set_channel_style_reversed
#define g_set_show_controls_w g_set_show_controls_reversed
#define g_set_expand_control_on_w g_set_expand_control_on_reversed
#define g_set_contrast_control_on_w g_set_contrast_control_on_reversed
#define g_set_reverb_control_on_w g_set_reverb_control_on_reversed
#define g_set_filter_control_on_w g_set_filter_control_on_reversed
#define g_set_filter_control_in_dB_w g_set_filter_control_in_dB_reversed
#define g_set_filter_control_in_hz_w g_set_filter_control_in_hz_reversed
#define g_set_filter_control_order_w g_set_filter_control_order_reversed
#define g_set_contrast_control_w g_set_contrast_control_reversed
#define g_set_contrast_control_bounds_w g_set_contrast_control_bounds_reversed
#define g_set_contrast_control_amp_w g_set_contrast_control_amp_reversed
#define g_set_expand_control_w g_set_expand_control_reversed
#define g_set_expand_control_bounds_w g_set_expand_control_bounds_reversed
#define g_set_expand_control_length_w g_set_expand_control_length_reversed
#define g_set_expand_control_ramp_w g_set_expand_control_ramp_reversed
#define g_set_expand_control_hop_w g_set_expand_control_hop_reversed
#define g_set_expand_control_jitter_w g_set_expand_control_jitter_reversed
#define g_set_speed_control_w g_set_speed_control_reversed
#define g_set_speed_control_bounds_w g_set_speed_control_bounds_reversed
#define g_set_reverb_control_length_w g_set_reverb_control_length_reversed
#define g_set_reverb_control_length_bounds_w g_set_reverb_control_length_bounds_reversed
#define g_set_reverb_control_scale_w g_set_reverb_control_scale_reversed
#define g_set_reverb_control_scale_bounds_w g_set_reverb_control_scale_bounds_reversed
#define g_set_reverb_control_feedback_w g_set_reverb_control_feedback_reversed
#define g_set_reverb_control_lowpass_w g_set_reverb_control_lowpass_reversed
#define g_set_amp_control_w g_set_amp_control_reversed
#define g_set_amp_control_bounds_w g_set_amp_control_bounds_reversed
#define g_set_reverb_control_decay_w g_set_reverb_control_decay_reversed
#define g_set_speed_control_style_w g_set_speed_control_style_reversed
#define g_set_speed_control_tones_w g_set_speed_control_tones_reversed
#else
Xen_wrap_any_args(g_save_sound_as_w, g_save_sound_as)
Xen_wrap_any_args(g_new_sound_w, g_new_sound)
Xen_wrap_any_args(g_open_raw_sound_w, g_open_raw_sound)
Xen_wrap_2_optional_args(g_set_filter_control_envelope_w, g_set_filter_control_envelope)
Xen_wrap_2_optional_args(g_set_read_only_w, g_set_read_only)
Xen_wrap_2_optional_args(g_set_sound_properties_w, g_set_sound_properties)
Xen_wrap_3_optional_args(g_set_sound_property_w, g_set_sound_property)
Xen_wrap_2_optional_args(g_set_sync_w, g_set_sync)
Xen_wrap_2_optional_args(g_set_channel_style_w, g_set_channel_style)
Xen_wrap_2_optional_args(g_set_show_controls_w, g_set_show_controls)
Xen_wrap_2_optional_args(g_set_expand_control_on_w, g_set_expand_control_on)
Xen_wrap_2_optional_args(g_set_contrast_control_on_w, g_set_contrast_control_on)
Xen_wrap_2_optional_args(g_set_reverb_control_on_w, g_set_reverb_control_on)
Xen_wrap_2_optional_args(g_set_filter_control_on_w, g_set_filter_control_on)
Xen_wrap_2_optional_args(g_set_filter_control_in_dB_w, g_set_filter_control_in_dB)
Xen_wrap_2_optional_args(g_set_filter_control_in_hz_w, g_set_filter_control_in_hz)
Xen_wrap_2_optional_args(g_set_filter_control_order_w, g_set_filter_control_order)
Xen_wrap_2_optional_args(g_set_contrast_control_w, g_set_contrast_control)
Xen_wrap_2_optional_args(g_set_contrast_control_bounds_w, g_set_contrast_control_bounds)
Xen_wrap_2_optional_args(g_set_contrast_control_amp_w, g_set_contrast_control_amp)
Xen_wrap_2_optional_args(g_set_expand_control_w, g_set_expand_control)
Xen_wrap_2_optional_args(g_set_expand_control_bounds_w, g_set_expand_control_bounds)
Xen_wrap_2_optional_args(g_set_expand_control_length_w, g_set_expand_control_length)
Xen_wrap_2_optional_args(g_set_expand_control_ramp_w, g_set_expand_control_ramp)
Xen_wrap_2_optional_args(g_set_expand_control_hop_w, g_set_expand_control_hop)
Xen_wrap_2_optional_args(g_set_expand_control_jitter_w, g_set_expand_control_jitter)
Xen_wrap_2_optional_args(g_set_speed_control_w, g_set_speed_control)
Xen_wrap_2_optional_args(g_set_speed_control_bounds_w, g_set_speed_control_bounds)
Xen_wrap_2_optional_args(g_set_reverb_control_length_w, g_set_reverb_control_length)
Xen_wrap_2_optional_args(g_set_reverb_control_length_bounds_w, g_set_reverb_control_length_bounds)
Xen_wrap_2_optional_args(g_set_reverb_control_scale_w, g_set_reverb_control_scale)
Xen_wrap_2_optional_args(g_set_reverb_control_scale_bounds_w, g_set_reverb_control_scale_bounds)
Xen_wrap_2_optional_args(g_set_reverb_control_feedback_w, g_set_reverb_control_feedback)
Xen_wrap_2_optional_args(g_set_reverb_control_lowpass_w, g_set_reverb_control_lowpass)
Xen_wrap_3_optional_args(g_set_amp_control_w, g_set_amp_control)
Xen_wrap_2_optional_args(g_set_amp_control_bounds_w, g_set_amp_control_bounds)
Xen_wrap_2_optional_args(g_set_reverb_control_decay_w, g_set_reverb_control_decay)
Xen_wrap_2_optional_args(g_set_speed_control_style_w, g_set_speed_control_style)
Xen_wrap_2_optional_args(g_set_speed_control_tones_w, g_set_speed_control_tones)
#endif

#if HAVE_SCHEME
static s7_pointer acc_channel_style(s7_scheme *sc, s7_pointer args) {return(g_set_channel_style(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_filter_control_in_dB(s7_scheme *sc, s7_pointer args) {return(g_set_filter_control_in_dB(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_filter_control_in_hz(s7_scheme *sc, s7_pointer args) {return(g_set_filter_control_in_hz(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_speed_control_tones(s7_scheme *sc, s7_pointer args) {return(g_set_speed_control_tones(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_speed_control_style(s7_scheme *sc, s7_pointer args) {return(g_set_speed_control_style(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_expand_control_length(s7_scheme *sc, s7_pointer args) {return(g_set_expand_control_length(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_expand_control_ramp(s7_scheme *sc, s7_pointer args) {return(g_set_expand_control_ramp(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_expand_control_hop(s7_scheme *sc, s7_pointer args) {return(g_set_expand_control_hop(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_expand_control_jitter(s7_scheme *sc, s7_pointer args) {return(g_set_expand_control_jitter(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_contrast_control_amp(s7_scheme *sc, s7_pointer args) {return(g_set_contrast_control_amp(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_reverb_control_feedback(s7_scheme *sc, s7_pointer args) {return(g_set_reverb_control_feedback(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_reverb_control_lowpass(s7_scheme *sc, s7_pointer args) {return(g_set_reverb_control_lowpass(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_reverb_control_decay(s7_scheme *sc, s7_pointer args) {return(g_set_reverb_control_decay(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_filter_control_order(s7_scheme *sc, s7_pointer args) {return(g_set_filter_control_order(s7_cadr(args), s7_undefined(sc)));}
static s7_pointer acc_show_controls(s7_scheme *sc, s7_pointer args) {return(g_set_show_controls(s7_cadr(args), s7_undefined(sc)));}
#endif


void g_init_snd(void)
{
#if HAVE_SCHEME
  s7_pointer pl_iq, pl_iqi, pl_sq, pl_sts, pl_i, pl_osi, pl_bt, pl_bo, pl_bob, pl_io, pl_ioi, pl_po, pl_pop, pl_ro, pl_ror, pl_oi, pl_ioz, pl_roo, pl_roor;
  s7_pointer i, t, s, b, o, q, p, r, z, sd, fv;
  i = s7_make_symbol(s7, "integer?");
  s = s7_make_symbol(s7, "string?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "pair?");
  r = s7_make_symbol(s7, "real?");
  sd = s7_make_symbol(s7, "sound?");
  fv = s7_make_symbol(s7, "float-vector?");
  t = s7_t(s7);
  q = t; /* sigh -- #t is legal here which is idiotic */
  o = t;
  z = s7_make_signature(s7, 2, i, b);
  pl_i = s7_make_signature(s7, 1, i);
  pl_iq = s7_make_signature(s7, 2, i, q);
  pl_iqi = s7_make_signature(s7, 3, i, q, i);
  pl_sts = s7_make_signature(s7, 3, s, t, s);
  pl_sq = s7_make_signature(s7, 2, s, q);
  pl_osi = s7_make_signature(s7, 3, o, s, i);
  pl_bt = s7_make_signature(s7, 2, b, t);
  pl_bo = s7_make_signature(s7, 2, b, o);
  pl_bob = s7_make_signature(s7, 3, b, o, b);
  pl_io = s7_make_signature(s7, 2, i, o);
  pl_oi = s7_make_signature(s7, 2, o, i);
  pl_ioi = s7_make_signature(s7, 3, i, o, i);
  pl_ioz = s7_make_signature(s7, 3, i, o, z);
  pl_po = s7_make_signature(s7, 2, p, o);
  pl_pop = s7_make_signature(s7, 3, p, o, p);
  pl_ro = s7_make_signature(s7, 2, r, o);
  pl_ror = s7_make_signature(s7, 3, r, o, r);
  pl_roo = s7_make_signature(s7, 3, r, o, o);
  pl_roor = s7_make_signature(s7, 4, r, o, o, r);
#endif

  init_xen_sound();
#if (!HAVE_SCHEME)
  init_sound_keywords();
#endif

  #define H_name_click_hook S_name_click_hook " (snd): called when sound name clicked. \
If it returns " PROC_TRUE ", the usual informative status babbling is squelched."

  #define H_after_apply_controls_hook S_after_apply_controls_hook " (snd): called when " S_apply_controls " finishes."

  name_click_hook =           Xen_define_hook(S_name_click_hook,           "(make-hook 'snd)",      1, H_name_click_hook);
  after_apply_controls_hook = Xen_define_hook(S_after_apply_controls_hook, "(make-hook 'snd)",      1, H_after_apply_controls_hook);

  #define H_channels_separate "The value for " S_channel_style " that causes channel graphs to occupy separate panes"
  #define H_channels_combined "The value for " S_channel_style " that causes channel graphs to occupy one pane (set by the 'unite' button)"
  #define H_channels_superimposed "The value for " S_channel_style " that causes channel graphs to occupy one pane and one axis"

  Xen_define_constant(S_channels_separate,     CHANNELS_SEPARATE,     H_channels_separate);
  Xen_define_constant(S_channels_combined,     CHANNELS_COMBINED,     H_channels_combined);
  Xen_define_constant(S_channels_superimposed, CHANNELS_SUPERIMPOSED, H_channels_superimposed);

  Xen_define_typed_procedure(S_status_report,          g_status_report_w,          1, 1, 0, H_status_report, pl_sts);

  Xen_define_typed_dilambda(S_channels,      g_channels_w,      H_channels,      S_set S_channels,      g_set_channels_w,       0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_chans,         g_channels_w,      H_channels,      S_set S_chans,         g_set_channels_w,       0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_srate,         g_srate_w,         H_srate,         S_set S_srate,         g_set_srate_w,          0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_data_location, g_data_location_w, H_data_location, S_set S_data_location, g_set_data_location_w,  0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_data_size,     g_data_size_w,     H_data_size,     S_set S_data_size,     g_set_data_size_w,      0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_sample_type,   g_sample_type_w,   H_sample_type,   S_set S_sample_type,   g_set_sample_type_w,    0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_header_type,   g_header_type_w,   H_header_type,   S_set S_header_type,   g_set_header_type_w,    0, 1, 1, 1, pl_iq, pl_iqi);
  Xen_define_typed_dilambda(S_comment,       g_comment_w,       H_comment,       S_set S_comment,       g_set_comment_w,        0, 1, 1, 1, NULL, NULL);

  Xen_define_typed_procedure(S_is_sound,             g_is_sound_w,         1, 0, 0, H_is_sound,		pl_bt);
  Xen_define_typed_procedure(S_find_sound,           g_find_sound_w,       1, 1, 0, H_find_sound,	pl_osi);
  Xen_define_typed_procedure(S_file_name,            g_file_name_w,        0, 1, 0, H_file_name,	pl_sq);
  Xen_define_typed_procedure(S_short_file_name,      g_short_file_name_w,  0, 1, 0, H_short_file_name,	pl_sq);
  Xen_define_typed_procedure(S_save_controls,        g_save_controls_w,    0, 1, 0, H_save_controls,	pl_bt);
  Xen_define_typed_procedure(S_restore_controls,     g_restore_controls_w, 0, 1, 0, H_restore_controls, pl_bt);
  Xen_define_typed_procedure(S_reset_controls,       g_reset_controls_w,   0, 1, 0, H_reset_controls,	pl_bt);
  Xen_define_typed_procedure(S_select_sound,         g_select_sound_w,     1, 0, 0, H_select_sound,     s7_make_signature(s7, 2, sd, i));
  Xen_define_typed_procedure(S_select_channel,       g_select_channel_w,   0, 1, 0, H_select_channel,   s7_make_signature(s7, 2, i, i));
  Xen_define_typed_procedure(S_sync_max,             g_sync_max_w,         0, 0, 0, H_sync_max,	        pl_i);
  Xen_define_typed_procedure(S_filter_control_coeffs, g_filter_control_coeffs_w, 0, 1, 0, H_filter_control_coeffs, s7_make_signature(s7, 2, fv, sd));

  Xen_define_typed_dilambda(S_selected_sound, g_selected_sound_w, H_selected_sound, 
			    S_set S_selected_sound, g_select_sound_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, s7_make_signature(s7, 2, sd, b)), s7_make_signature(s7, 2, sd, z));
  Xen_define_typed_dilambda(S_selected_channel, g_selected_channel_w, H_selected_channel, 
			    S_set S_selected_channel, g_set_selected_channel_w,  0, 1, 0, 2,
			    s7_make_signature(s7, 2, z, sd), s7_make_signature(s7, 3, i, t, z));

  Xen_define_typed_procedure(S_start_progress_report,  g_start_progress_report_w,   0, 2, 0, H_start_progress_report,  s7_make_signature(s7, 3, b, sd, i));
  Xen_define_typed_procedure(S_finish_progress_report, g_finish_progress_report_w,  0, 2, 0, H_finish_progress_report, s7_make_signature(s7, 3, b, sd, i));
  Xen_define_typed_procedure(S_progress_report,        g_progress_report_w,         1, 2, 0, H_progress_report,        s7_make_signature(s7, 4, r, r, sd, i));

  /* open-sound is definitely not a safe procedure; probably the rest of these are similar 
   *   [see snd-test 5 with tests=2 or more]
   */
  Xen_define_unsafe_typed_procedure(S_close_sound,    g_close_sound_w,        0, 1, 0, H_close_sound,       s7_make_signature(s7, 2, t, t));
  Xen_define_unsafe_typed_procedure(S_update_sound,   g_update_sound_w,       0, 1, 0, H_update_sound,      s7_make_signature(s7, 2, t, t));
  Xen_define_unsafe_typed_procedure(S_save_sound,     g_save_sound_w,         0, 1, 0, H_save_sound,        s7_make_signature(s7, 2, sd, t));
  Xen_define_unsafe_typed_procedure(S_open_sound,     g_open_sound_w,         1, 0, 0, H_open_sound,        s7_make_signature(s7, 2, sd, s));
  Xen_define_unsafe_typed_procedure(S_view_sound,     g_view_sound_w,         1, 0, 0, H_view_sound,        s7_make_signature(s7, 2, sd, s));
  Xen_define_unsafe_typed_procedure(S_revert_sound,   g_revert_sound_w,       0, 1, 0, H_revert_sound,      s7_make_signature(s7, 2, sd, sd));

#if HAVE_SCHEME
  s7_define_function_star(s7, S_new_sound, g_new_sound, "file channels srate sample-type header-type comment size", H_new_sound);
  s7_define_function_star(s7, S_save_sound_as, g_save_sound_as, "file sound srate sample-type header-type channel edit-position comment", H_save_sound_as);
  s7_define_function_star(s7, S_open_raw_sound, g_open_raw_sound, "file channels srate sample-type", H_open_raw_sound);
#else
  Xen_define_unsafe_typed_procedure(S_new_sound,      g_new_sound_w,          0, 0, 1, H_new_sound,         s7_make_circular_signature(s7, 0, 1, t));
  Xen_define_unsafe_typed_procedure(S_open_raw_sound, g_open_raw_sound_w,     0, 0, 1, H_open_raw_sound,    s7_make_circular_signature(s7, 0, 1, t));
  Xen_define_unsafe_typed_procedure(S_save_sound_as,  g_save_sound_as_w,      0, 0, 1, H_save_sound_as,     s7_make_circular_signature(s7, 0, 1, t));
#endif

  Xen_define_typed_procedure(S_apply_controls,         g_apply_controls_w,          0, 4, 0, H_apply_controls,         s7_make_signature(s7, 5, t, t, i, i, i));
  Xen_define_typed_procedure(S_controls_to_channel,    g_controls_to_channel_w,     0, 6, 0, H_controls_to_channel,    s7_make_signature(s7, 7, p, p, i, i, t, t, s));

  Xen_define_typed_dilambda(S_filter_control_envelope, g_filter_control_envelope_w, H_filter_control_envelope, 
			    S_set S_filter_control_envelope, g_set_filter_control_envelope_w, 0, 1, 1, 1,
			    s7_make_signature(s7, 2, p, t), s7_make_signature(s7, 3, p, t, p));
  Xen_define_typed_dilambda(S_sound_properties, g_sound_properties_w, H_sound_properties, 
			    S_set S_sound_properties, g_set_sound_properties_w, 0, 1, 1, 1,
			    s7_make_circular_signature(s7, 0, 1, t), s7_make_circular_signature(s7, 0, 1, t));
  Xen_define_typed_dilambda(S_sound_property, g_sound_property_w, H_sound_property, 
			    S_set S_sound_property, g_set_sound_property_w, 1, 1, 2, 1,
			    s7_make_circular_signature(s7, 0, 1, t), s7_make_circular_signature(s7, 0, 1, t));

  Xen_define_typed_dilambda(S_show_controls, g_show_controls_w, H_show_controls, 
			    S_set S_show_controls, g_set_show_controls_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_sync, g_sync_w, H_sync, 
			    S_set S_sync, g_set_sync_w, 0, 1, 1, 1, pl_io, pl_ioz);
  Xen_define_typed_dilambda(S_channel_style, g_channel_style_w, H_channel_style, 
			    S_set S_channel_style, g_set_channel_style_w, 0, 1, 1, 1, pl_io, pl_ioi);
  Xen_define_typed_dilambda(S_read_only, g_read_only_w, H_read_only, 
			    S_set S_read_only, g_set_read_only_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_expand_control_on, g_expand_control_on_w, H_expand_control_on, 
			    S_set S_expand_control_on, g_set_expand_control_on_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_contrast_control_on, g_contrast_control_on_w, H_contrast_control_on, 
			    S_set S_contrast_control_on, g_set_contrast_control_on_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_reverb_control_on, g_reverb_control_on_w, H_reverb_control_on, 
			    S_set S_reverb_control_on, g_set_reverb_control_on_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_filter_control_on, g_filter_control_on_w, H_filter_control_on, 
			    S_set S_filter_control_on, g_set_filter_control_on_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_filter_control_in_dB, g_filter_control_in_dB_w, H_filter_control_in_dB, 
			    S_set S_filter_control_in_dB, g_set_filter_control_in_dB_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_filter_control_in_hz, g_filter_control_in_hz_w, H_filter_control_in_hz, 
			    S_set S_filter_control_in_hz, g_set_filter_control_in_hz_w, 0, 1, 1, 1, pl_bo, pl_bob);
  Xen_define_typed_dilambda(S_filter_control_order, g_filter_control_order_w, H_filter_control_order, 
			    S_set S_filter_control_order, g_set_filter_control_order_w, 0, 1, 1, 1, pl_io, pl_ioi);
  Xen_define_typed_dilambda(S_contrast_control, g_contrast_control_w, H_contrast_control, 
			    S_set S_contrast_control, g_set_contrast_control_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_contrast_control_bounds, g_contrast_control_bounds_w, H_contrast_control_bounds, 
			    S_set S_contrast_control_bounds, g_set_contrast_control_bounds_w, 0, 1, 1, 1, pl_po, pl_pop);
  Xen_define_typed_dilambda(S_contrast_control_amp, g_contrast_control_amp_w, H_contrast_control_amp, 
			    S_set S_contrast_control_amp, g_set_contrast_control_amp_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_expand_control, g_expand_control_w, H_expand_control, 
			    S_set S_expand_control, g_set_expand_control_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_expand_control_bounds, g_expand_control_bounds_w, H_expand_control_bounds, 
			    S_set S_expand_control_bounds, g_set_expand_control_bounds_w, 0, 1, 1, 1, pl_po, pl_pop);
  Xen_define_typed_dilambda(S_expand_control_length, g_expand_control_length_w, H_expand_control_length, 
			    S_set S_expand_control_length, g_set_expand_control_length_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_expand_control_ramp, g_expand_control_ramp_w, H_expand_control_ramp, 
			    S_set S_expand_control_ramp, g_set_expand_control_ramp_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_expand_control_hop, g_expand_control_hop_w, H_expand_control_hop, 
			    S_set S_expand_control_hop, g_set_expand_control_hop_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_expand_control_jitter, g_expand_control_jitter_w, H_expand_control_jitter, 
			    S_set S_expand_control_jitter, g_set_expand_control_jitter_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_speed_control, g_speed_control_w, H_speed_control, 
			    S_set S_speed_control, g_set_speed_control_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_speed_control_bounds, g_speed_control_bounds_w, H_speed_control_bounds, 
			    S_set S_speed_control_bounds, g_set_speed_control_bounds_w, 0, 1, 1, 1, pl_po, pl_pop);
  Xen_define_typed_dilambda(S_reverb_control_length, g_reverb_control_length_w, H_reverb_control_length, 
			    S_set S_reverb_control_length, g_set_reverb_control_length_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_reverb_control_length_bounds, g_reverb_control_length_bounds_w, H_reverb_control_length_bounds, 
			    S_set S_reverb_control_length_bounds, g_set_reverb_control_length_bounds_w, 0, 1, 1, 1, pl_po, pl_pop);
  Xen_define_typed_dilambda(S_reverb_control_scale, g_reverb_control_scale_w, H_reverb_control_scale, 
			    S_set S_reverb_control_scale, g_set_reverb_control_scale_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_reverb_control_scale_bounds, g_reverb_control_scale_bounds_w, H_reverb_control_scale_bounds, 
			    S_set S_reverb_control_scale_bounds, g_set_reverb_control_scale_bounds_w, 0, 1, 1, 1, pl_po, pl_pop);
  Xen_define_typed_dilambda(S_reverb_control_feedback, g_reverb_control_feedback_w, H_reverb_control_feedback, 
			    S_set S_reverb_control_feedback, g_set_reverb_control_feedback_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_reverb_control_lowpass, g_reverb_control_lowpass_w, H_reverb_control_lowpass, 
			    S_set S_reverb_control_lowpass, g_set_reverb_control_lowpass_w, 0, 1, 1, 1, pl_ro, pl_ror);
  Xen_define_typed_dilambda(S_amp_control, g_amp_control_w, H_amp_control, 
			    S_set S_amp_control, g_set_amp_control_w, 0, 2, 1, 2, pl_roo,pl_roor);
  Xen_define_typed_dilambda(S_amp_control_bounds, g_amp_control_bounds_w, H_amp_control_bounds, 
			    S_set S_amp_control_bounds, g_set_amp_control_bounds_w, 0, 1, 1, 1, pl_po, pl_pop);
  Xen_define_typed_dilambda(S_reverb_control_decay, g_reverb_control_decay_w, H_reverb_control_decay, 
			    S_set S_reverb_control_decay, g_set_reverb_control_decay_w, 0, 1, 1, 1, pl_ro, pl_ror);
  
  #define H_speed_control_as_float "The value for " S_speed_control_style " that interprets the speed slider as a float"
  #define H_speed_control_as_ratio "The value for " S_speed_control_style " that interprets the speed slider as a just-intonation ratio"
  #define H_speed_control_as_semitone "The value for " S_speed_control_style " that interprets the speed slider as a microtone (via " S_speed_control_tones ")"
  
  Xen_define_constant(S_speed_control_as_float,        SPEED_CONTROL_AS_FLOAT,    H_speed_control_as_float);
  Xen_define_constant(S_speed_control_as_ratio,        SPEED_CONTROL_AS_RATIO,    H_speed_control_as_ratio);
  Xen_define_constant(S_speed_control_as_semitone,     SPEED_CONTROL_AS_SEMITONE, H_speed_control_as_semitone);
  
  Xen_define_typed_dilambda(S_speed_control_style, g_speed_control_style_w, H_speed_control_style, 
			    S_set S_speed_control_style, g_set_speed_control_style_w, 0, 1, 1, 1, pl_io, pl_ioi);
  Xen_define_typed_dilambda(S_speed_control_tones, g_speed_control_tones_w, H_speed_control_tones, 
			    S_set S_speed_control_tones, g_set_speed_control_tones_w, 0, 1, 1, 1, pl_io, pl_ioi);

  Xen_define_typed_procedure(S_channel_amp_envs,        g_channel_amp_envs_w,         0, 5, 0, H_channel_amp_envs, s7_make_signature(s7, 6, t, t, i, i, t, t));

  Xen_define_typed_procedure(S_sounds,                  g_sounds_w,                   0, 0, 0, H_sounds, s7_make_signature(s7, 1, s7_make_symbol(s7, "list?")));
  Xen_define_typed_procedure(S_integer_to_sound,        g_integer_to_sound_w,         1, 0, 0, H_integer_to_sound, pl_oi);
  Xen_define_typed_procedure(S_sound_to_integer,        g_sound_to_integer_w,         1, 0, 0, H_sound_to_integer, pl_io);

#if HAVE_SCHEME
  s7_set_documentation(s7, ss->channel_style_symbol, "*channel-style*: how multichannel sounds lay out the channels: channels-combined, channels-separate or channels-superimposed.");
  s7_set_documentation(s7, ss->filter_control_in_db_symbol, "*filter-control-in-dB*: #t if snd's filter envelope is displayed in dB in control panel");
  s7_set_documentation(s7, ss->filter_control_in_hz_symbol, "*filter-control-in-hz*: #t if snd's filter envelope x axis should be in hz (control panel filter)");
  s7_set_documentation(s7, ss->speed_control_tones_symbol, "*speed-control-tones*: the speed-control octave divisions (12)");
  s7_set_documentation(s7, ss->speed_control_style_symbol, "*speed-control-style*: speed control choice (speed-control-as-float etc)");
  s7_set_documentation(s7, ss->expand_control_length_symbol, "*expand-control-length*: current expansion segment length in seconds (.15)");
  s7_set_documentation(s7, ss->expand_control_ramp_symbol, "*expand-control-ramp*: current expansion ramp time (.4)");
  s7_set_documentation(s7, ss->expand_control_hop_symbol, "*expand-control-hop*: current expansion output grain spacing in seconds (0.05)");
  s7_set_documentation(s7, ss->expand_control_jitter_symbol, "*expand-control-jitter*: current expansion output grain spacing jitter (0.1)");
  s7_set_documentation(s7, ss->contrast_control_amp_symbol, "*contrast-control-amp*: contrast amp");
  s7_set_documentation(s7, ss->reverb_control_feedback_symbol, "*reverb-control-feedback*: control-panel reverb feedback scaler");
  s7_set_documentation(s7, ss->reverb_control_lowpass_symbol, "*reverb-control-lowpass*: control-panel reverb lowpass filter coefficient");
  s7_set_documentation(s7, ss->reverb_control_decay_symbol, "*reverb-control-decay*: control-panel reverb decay time (1.0 seconds)");
  s7_set_documentation(s7, ss->filter_control_order_symbol, "*filter-control-order*: control-panel filter order");
  s7_set_documentation(s7, ss->show_controls_symbol, "*show-controls*: #t if snd's control panel is known to be open");

  s7_set_setter(s7, ss->channel_style_symbol, s7_make_function(s7, "[acc-" S_channel_style "]", acc_channel_style, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->filter_control_in_db_symbol, s7_make_function(s7, "[acc-" S_filter_control_in_dB "]", acc_filter_control_in_dB, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->filter_control_in_hz_symbol, s7_make_function(s7, "[acc-" S_filter_control_in_hz "]", acc_filter_control_in_hz, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->speed_control_tones_symbol, s7_make_function(s7, "[acc-" S_speed_control_tones "]", acc_speed_control_tones, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->speed_control_style_symbol, s7_make_function(s7, "[acc-" S_speed_control_style "]", acc_speed_control_style, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->expand_control_length_symbol, s7_make_function(s7, "[acc-" S_expand_control_length "]", acc_expand_control_length, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->expand_control_ramp_symbol, s7_make_function(s7, "[acc-" S_expand_control_ramp "]", acc_expand_control_ramp, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->expand_control_hop_symbol, s7_make_function(s7, "[acc-" S_expand_control_hop "]", acc_expand_control_hop, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->expand_control_jitter_symbol, s7_make_function(s7, "[acc-" S_expand_control_jitter "]", acc_expand_control_jitter, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->contrast_control_amp_symbol, s7_make_function(s7, "[acc-" S_contrast_control_amp "]", acc_contrast_control_amp, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->reverb_control_feedback_symbol, s7_make_function(s7, "[acc-" S_reverb_control_feedback "]", acc_reverb_control_feedback, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->reverb_control_lowpass_symbol, s7_make_function(s7, "[acc-" S_reverb_control_lowpass "]", acc_reverb_control_lowpass, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->reverb_control_decay_symbol, s7_make_function(s7, "[acc-" S_reverb_control_decay "]", acc_reverb_control_decay, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->filter_control_order_symbol, s7_make_function(s7, "[acc-" S_filter_control_order "]", acc_filter_control_order, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->show_controls_symbol, s7_make_function(s7, "[acc-" S_show_controls "]", acc_show_controls, 2, 0, false, "accessor"));
#endif
}
