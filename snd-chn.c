#include "snd.h"
#include "clm2xen.h"
#include "clm-strings.h"

bool is_graph_style(int grf)
{
  switch (grf)
    {
    case GRAPH_LINES: case GRAPH_DOTS: case GRAPH_FILLED: case GRAPH_DOTS_AND_LINES: case GRAPH_LOLLIPOPS: 
      return(true);
    }
  return(false);
}


typedef struct lisp_grf {
  int *len;
  mus_float_t **data;
  int graphs;
  axis_info *axis;
  int env_data;
  show_axes_t show_axes;
} lisp_grf;


chan_info *get_cp(Xen snd, Xen x_chn_n, const char *caller)
{
  snd_info *sp;
  int chn_n;

  sp = get_sp(snd);

  if ((!sp) || (!(sp->active)) || (sp->inuse == SOUND_IDLE))
    {
      snd_no_such_sound_error(caller, snd); 
      return(NULL); /* just in case our catch has been clobbered */
    }

  if (Xen_is_integer(x_chn_n))
    chn_n = Xen_integer_to_C_int(x_chn_n);
  else
    if (sp->selected_channel != NO_SELECTION) 
      chn_n = sp->selected_channel;
    else chn_n = 0;

  if ((chn_n >= 0) && (chn_n < (int)sp->nchans) && (sp->chans[chn_n]))
    return(sp->chans[chn_n]);

  snd_no_such_channel_error(caller, snd, x_chn_n);
  return(NULL);
}


static Xen lisp_graph_hook;
static Xen mouse_press_hook; 
static Xen mark_click_hook; 
static Xen mix_click_hook; 
static Xen mouse_click_hook;
static Xen mouse_drag_hook; 
static Xen key_press_hook; 
static Xen after_transform_hook;
static Xen graph_hook;
static Xen after_graph_hook;
static Xen after_lisp_graph_hook;


static void after_transform(chan_info *cp, mus_float_t scaler)
{
  if (Xen_hook_has_list(after_transform_hook))
    run_hook(after_transform_hook,
	     Xen_list_3(C_int_to_Xen_sound(cp->sound->index),
			C_int_to_Xen_integer(cp->chan),
			C_double_to_Xen_real(scaler)),
	     S_after_transform_hook);
}


static void run_after_graph_hook(chan_info *cp)
{
  if ((cp->hookable == WITH_HOOK) &&
      (Xen_hook_has_list(after_graph_hook)))
    run_hook(after_graph_hook,
	     Xen_list_2(C_int_to_Xen_sound(cp->sound->index),
			C_int_to_Xen_integer(cp->chan)),
	     S_after_graph_hook);
  /* (hook-push after-graph-hook (lambda (hook) (snd-print (format #f "~A ~A~%" (hook 'snd) (hook 'chn))))) */
}


static void set_y_bounds(axis_info *ap);

static void chans_time_graph_type(chan_info *cp, int value)
{
  cp->time_graph_type = (graph_type_t)value;
  if (cp->time_graph_type == GRAPH_ONCE) 
    {
      set_y_bounds(cp->axis);
      resize_sy(cp);
      set_x_bounds(cp->axis);
      resize_sx_and_zx(cp);
    }
  update_graph(cp); 
}

static void set_time_graph_type(graph_type_t val) 
{
  in_set_time_graph_type(val); 
  for_each_chan_with_int(chans_time_graph_type, (int)val);
}


static void chans_wavo_hop(chan_info *cp, int hop)
{
  cp->wavo_hop = hop;
  update_graph(cp); 
}

static void set_wavo_hop(int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_hop(val); 
  for_each_chan_with_int(chans_wavo_hop, val);
}


static void chans_wavo_trace(chan_info *cp, int value)
{
  cp->wavo_trace = value;
  update_graph(cp);
}

void set_wavo_trace(int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_trace(val); 
  for_each_chan_with_int(chans_wavo_trace, val);
}


static void set_beats_per_minute(mus_float_t val) 
{
  if (val > 0.0) 
    {
      if (val > 10000.0) val = 10000.0;
      in_set_beats_per_minute(val); 
      chans_field(FCP_BEATS, val);
      if (!(ss->graph_hook_active)) 
	for_each_chan(update_graph);
    }
}

static void chans_beats_per_measure(chan_info *cp, int value)
{
  cp->beats_per_measure = value;
  update_graph(cp);
}

static void set_beats_per_measure(int val) 
{
  if (val > 0) 
    {
      if (val > 1000) val = 1000;
      in_set_beats_per_measure(val); 
      for_each_chan_with_int(chans_beats_per_measure, val);
    }
}


static void chans_max_transform_peaks(chan_info *cp, int value)
{
  cp->max_transform_peaks = value;
}

void set_max_transform_peaks(int val) 
{
  in_set_max_transform_peaks(val); 
  for_each_chan_with_int(chans_max_transform_peaks, val);
}


static void chans_zero_pad(chan_info *cp, int value)
{
  cp->zero_pad = value;
  calculate_fft(cp);
}

static void set_zero_pad(int val) 
{
  in_set_zero_pad(val); 
  for_each_chan_with_int(chans_zero_pad, val);
}


static void chans_show_grid(chan_info *cp, int value)
{
  cp->show_grid = (with_grid_t)value;
  update_graph(cp);
}

void set_show_grid(with_grid_t val)
{
  in_set_show_grid(val);
  for_each_chan_with_int(chans_show_grid, (int)val);
}


static void chans_grid_density(chan_info *cp, mus_float_t value)
{
  cp->grid_density = value;
  update_graph(cp);
}

void set_grid_density(mus_float_t val)
{
  in_set_grid_density(val);
  for_each_chan_with_float(chans_grid_density, val);
}


static void chans_show_sonogram_cursor(chan_info *cp, bool value)
{
  cp->show_sonogram_cursor = value;
  update_graph(cp);
}

static void set_show_sonogram_cursor(bool val)
{
  in_set_show_sonogram_cursor(val);
  for_each_chan_with_bool(chans_show_sonogram_cursor, val);
}


static void chans_transform_graph_type(chan_info *cp, int value)
{
  cp->transform_graph_type = (graph_type_t)value;
}

void in_set_transform_graph_type(graph_type_t uval) 
{
  graph_type_t val;
  val = (graph_type_t)mus_iclamp((int)GRAPH_ONCE, uval, (int)GRAPH_AS_SPECTROGRAM);
  in_set_transform_graph_type_1(val); 
  for_each_chan_with_int(chans_transform_graph_type, (int)val);
}


static void chans_show_mix_waveforms(chan_info *cp, bool value)
{
  cp->show_mix_waveforms = value;
  update_graph(cp); 
}

void set_show_mix_waveforms(bool val) 
{
  in_set_show_mix_waveforms(val); 
  for_each_chan_with_bool(chans_show_mix_waveforms, val);
}


static void chans_show_axes(chan_info *cp, int value)
{
  cp->show_axes = (show_axes_t)value;
  if (cp->lisp_info)
    cp->lisp_info->show_axes = cp->show_axes;
  update_graph(cp); 
}

void set_show_axes(show_axes_t val) 
{
  in_set_show_axes(val); 
  for_each_chan_with_int(chans_show_axes, (int)val);
}


static void chans_graphs_horizontal(chan_info *cp, bool value)
{
  cp->graphs_horizontal = value;
  update_graph(cp); 
}

static void set_graphs_horizontal(bool val) 
{
  in_set_graphs_horizontal(val);
  for_each_chan_with_bool(chans_graphs_horizontal, val);
}


static void chans_fft_window(chan_info *cp, int value)
{
  cp->fft_window = (mus_fft_window_t)value;
  if (cp->fft) (cp->fft)->window = (mus_fft_window_t)value;
}

void in_set_fft_window(mus_fft_window_t val) 
{
  in_set_fft_window_1(val); 
  for_each_chan_with_int(chans_fft_window, (int)val);
}


void chans_field(fcp_t field, mus_float_t val)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      uint32_t j;
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  switch (field)
	    {
	    case FCP_X_ANGLE:        sp->chans[j]->spectro_x_angle = val;                         break; /* these are in-coming from user interface */
	    case FCP_X_SCALE:        sp->chans[j]->spectro_x_scale = val;                         break;
	    case FCP_Y_ANGLE:        sp->chans[j]->spectro_y_angle = val;                         break;
	    case FCP_Y_SCALE:        sp->chans[j]->spectro_y_scale = val;                         break;
	    case FCP_Z_ANGLE:        sp->chans[j]->spectro_z_angle = val;                         break;
	    case FCP_Z_SCALE:        sp->chans[j]->spectro_z_scale = val;                         break;
	    case FCP_SPECTRUM_START: sp->chans[j]->spectrum_start = mus_fclamp(0.0, val, 1.0);    break; 
	    case FCP_SPECTRUM_END:   sp->chans[j]->spectrum_end = mus_fclamp(0.0, val, 1.0);      break;
	    case FCP_ALPHA:          sp->chans[j]->fft_window_alpha = mus_fclamp(0.0, val, 10.0); break;
	    case FCP_BETA:           sp->chans[j]->fft_window_beta = mus_fclamp(0.0, val, 1.0);   break;
	    case FCP_BEATS:          if (val > 0.0) sp->chans[j]->beats_per_minute = val;         break;
	    }
    }
}


void combine_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_COMBINED);}
void superimpose_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_SUPERIMPOSED);}
void separate_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_SEPARATE);}


void set_sound_channel_style(snd_info *sp, channel_style_t val)
{
  switch (val)
    {
    case CHANNELS_SEPARATE:     separate_sound(sp);    break; /* snd-xchn.c -> change_channel_style */
    case CHANNELS_COMBINED:     combine_sound(sp);     break;
    case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
    default:
      break;
    }
}


bool chan_fft_in_progress(chan_info *cp)
{
  /* fft_in_progress is a background process only if sonogram/spectrogram */
  return((bool)(cp->fft_in_progress));
}


void set_chan_fft_in_progress(chan_info *cp, idle_t fp) 
{
  cp->fft_in_progress = fp;
}


void stop_fft_in_progress(chan_info *cp)
{
  if (cp)
    {
      if (cp->fft_in_progress) 
	{
	  BACKGROUND_REMOVE(cp->fft_in_progress);
	  finish_progress_report(cp);
	  cp->fft_in_progress = 0;
	}
    }
}


void stop_peak_env(chan_info *cp)
{
  if (cp->peak_env_in_progress)
    {
      BACKGROUND_REMOVE(cp->peak_env_in_progress);
      free_peak_env_state(cp);
      cp->peak_env_in_progress = 0; 
    }
}


void force_fft_clear(chan_info *cp)
{
  if (cp->fft_in_progress)
    {
      BACKGROUND_REMOVE(cp->fft_in_progress);
      finish_progress_report(cp);
      cp->fft_in_progress = 0;
    }
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  cp_free_fft_state(cp);
}


void chan_info_cleanup(chan_info *cp)
{
  if (cp)
    {
      cp->selected = false;
      if (cp->fft_in_progress) 
	{
	  BACKGROUND_REMOVE(cp->fft_in_progress);
	  cp->fft_in_progress = 0;
	}
      stop_peak_env(cp);
      cleanup_cw(cp);
    }
}


static void chans_dot_size(chan_info *cp, int value)
{
  cp->dot_size = value;
  update_graph(cp);
}

void set_dot_size(int val)
{
  if (val > 0)  /* -1 here can crash X! */
    {
      in_set_dot_size(val);
      for_each_chan_with_int(chans_dot_size, val);
    }
}


static void chans_cursor_size(chan_info *cp, int value)
{
  cp->cursor_size = value;
  update_graph(cp);
}

void set_cursor_size(int val)
{
  if (val > 0)
    {
      in_set_cursor_size(val);
      for_each_chan_with_int(chans_cursor_size, val);
    }
}


static void chans_cursor_style(chan_info *cp, int value)
{
  cursor_style_t style;
  style = (cursor_style_t)value;
  if ((cp->cursor_style == CURSOR_PROC) && (Xen_is_procedure(cp->cursor_proc)))
    {
      snd_unprotect_at(cp->cursor_proc_loc);
      cp->cursor_proc = Xen_undefined;
      cp->cursor_proc_loc = NOT_A_GC_LOC;
    }
  cp->cursor_style = style;
  cp->just_zero = (style == CURSOR_LINE); /* no point in displaying y value in this case */
  update_graph(cp);
}

void set_cursor_style(cursor_style_t val)
{
  in_set_cursor_style(val);
  for_each_chan_with_int(chans_cursor_style, (int)val);
}


static void chans_tracking_cursor_style(chan_info *cp, int value)
{
  cp->tracking_cursor_style = (cursor_style_t)value;
}

static void set_tracking_cursor_style(cursor_style_t val)
{
  in_set_tracking_cursor_style(val);
  for_each_chan_with_int(chans_tracking_cursor_style, (int)val);
}


chan_info *virtual_selected_channel(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp->channel_style == CHANNELS_SEPARATE) || (sp->selected_channel == NO_SELECTION)) 
    return(cp);
  else return(sp->chans[sp->selected_channel]);
}


static int calculate_fft_1(chan_info *cp, bool update_display)
{
  if ((cp->graph_transform_on) &&
      (!(chan_fft_in_progress(cp))))
    {
#if (!USE_NO_GUI)
      if (cp->transform_graph_type == GRAPH_ONCE)
	single_fft(cp, update_display, DONT_FORCE_REFFT);
      else set_chan_fft_in_progress(cp,
				    BACKGROUND_ADD(sonogram_in_slices,
						   make_sonogram_state(cp, 
								       DONT_FORCE_REFFT)));
#else
      single_fft(cp, update_display, DONT_FORCE_REFFT);
#endif
    }
  return(0);
}

void calculate_fft(chan_info *cp)
{
  calculate_fft_1(cp, FORCE_REDISPLAY);
}


static void update_graph_1(chan_info *cp, bool warn)
{
  /* don't put display stuff here!  This is needed so that the fft display does not get caught in a loop */
  snd_info *sp;
  axis_info *ap;

  if ((cp->updating) || 
      (cp->active != CHANNEL_HAS_AXES) ||
      (!cp->sounds) || 
      (!cp->sounds[cp->sound_ctr])) 
    return;

  sp = cp->sound;
  if (cp->squelch_update)
    {
#if (!USE_NO_GUI)
      if ((warn) && (sp))
	set_status(sp, "(update squelched)", false); /* this has tripped me one too many times... */
#endif
      return;
    }

  cp->updating = true;
  /* next two are needed by fft and lisp displays, but if put off until make_graph cause
   * the display to happen twice in some cases 
   */
  ap = cp->axis;
  if (ap)
    {
      double cur_srate;
      cur_srate = (double)(snd_srate(sp));
      ap->losamp = snd_round_mus_long_t(ap->x0 * cur_srate); 
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (mus_long_t)(ap->x1 * cur_srate);
    }

  if ((cp->graph_transform_on) && 
      (!(chan_fft_in_progress(cp)))) 
    calculate_fft_1(cp, DONT_FORCE_REDISPLAY);
  display_channel_data(cp);
  cp->updating = false;
}


void update_graph(chan_info *cp)
{
  update_graph_1(cp, false);
}


void update_graph_or_warn(chan_info *cp)
{
  update_graph_1(cp, true);
}


#define INITIAL_EDIT_SIZE 8

static Xen initial_graph_hook;

bool add_channel_data_1(chan_info *cp, int srate, mus_long_t framples, channel_graph_t graphed)
{
  /* initialize channel, including edit/sound lists */
  axis_info *ap;
  mus_float_t ymin = 0.0, ymax = 0.0, y0, y1;
  double xmax, x0, x1, dur;
  const char *label = NULL;
  char *hook_label = NULL;
  bool ymin_set = false, ymax_set = false;

  cp->edit_size = INITIAL_EDIT_SIZE;
  cp->edits = (ed_list **)calloc(cp->edit_size, sizeof(ed_list *));
  cp->edit_ctr = 0;
  cp->edits[0] = initial_ed_list(0, framples - 1);
  cp->sound_size = INITIAL_EDIT_SIZE;
  cp->sound_ctr = 0;
  cp->sounds = (snd_data **)calloc(cp->sound_size, sizeof(snd_data *));

  cp->active = CHANNEL_HAS_EDIT_LIST;

  x0 = initial_beg(ss);
  x1 = initial_beg(ss) + initial_dur(ss);
  y0 = -1.0;
  y1 = 1.0;

  switch (cp->x_axis_style)
    {
    case X_AXIS_IN_BEATS:      label = "time (beats)";    break;
    case X_AXIS_IN_MEASURES:   label = "time (measures)"; break;
    case X_AXIS_IN_SAMPLES:    label = "time (samples)";  break;
    case X_AXIS_AS_PERCENTAGE: label = "time (percent)";  break;
    default:                   label = "time";            break;
    }

  dur = (double)framples / (double)(srate);
  if (show_full_duration(ss))
    {
      x0 = 0.0;
      x1 = dur;
    }

  if ((cp->hookable == WITH_HOOK) && 
      (graphed == WITH_GRAPH))
    {
      /* can also be WITHOUT_GRAPH and WITHOUT_INITIAL_GRAPH_HOOK
       *   the former is called in snd-nogui, and in the make_readable calls in snd-regions and snd-snd
       *   the latter is from snd-edits where we are updating an already displayed sound (and keeping its axis settings across the update)
       * this hook is replacing earlier "initial-x0" settings
       */

      /* peak-env setup.  This used to be handled by extension language code (peak-env.*) using
       *   the initial-graph-hook, close-hook, and exit-hook.
       */
      if (peak_env_dir(ss))
	{
	  /* look for peak env file.  If found, read in its contents. */
	  const char *error_string;
	  error_string = read_peak_env_info_file(cp);
	  if (error_string)
	    snd_warning("peak-env: %s\n", error_string);
	}
      /* there is no data yet in the channel, so channel_maxamp here will get 0.0 */

      /* initial-graph-hook */
      if (Xen_hook_has_list(initial_graph_hook))
	{
	  Xen res;
	  res = run_or_hook(initial_graph_hook,
			    Xen_list_3(C_int_to_Xen_sound(cp->sound->index),
				       C_int_to_Xen_integer(cp->chan),
				       C_double_to_Xen_real(dur)),
			    S_initial_graph_hook);

	  if (Xen_is_list(res))
	    {
	      int len;
	      len = Xen_list_length(res);
	      if (len > 0) x0 = Xen_real_to_C_double(Xen_car(res));
	      if (len > 1) x1 = Xen_real_to_C_double(Xen_cadr(res));
	      if (len > 2) y0 = Xen_real_to_C_double(Xen_caddr(res));
	      if (len > 3) y1 = Xen_real_to_C_double(Xen_cadddr(res));
	      if ((len > 4) && 
		  (Xen_is_string(Xen_list_ref(res, 4))))
		hook_label = mus_strdup(Xen_string_to_C_string(Xen_list_ref(res, 4)));
	      if (len > 5)
		{
		  ymin = Xen_real_to_C_double(Xen_list_ref(res, 5));
		  ymin_set = true;
		}
	      if (len > 6)
		{
		  ymax = Xen_real_to_C_double(Xen_list_ref(res, 6));
		  ymax_set = true;
		}
	      /* ymin/ymax for possible fit data hooks */
	    }
	}
    }

  if ((!ymax_set) && (!ymin_set) &&
      (peak_env_maxamp_ok(cp, 0)))
    {
      ymax = peak_env_maxamp(cp, 0);
      if (ymax < 1.0) ymax = 1.0;
      ymin = -ymax;
    }
  else
    {
      if (!ymax_set) 
	{
	  if (y1 > 1.0) 
	    ymax = y1; 
	  else ymax = 1.0;
	}
      if (!ymin_set) 
	{if (y0 < -1.0) 
	    ymin = y0; 
	  else ymin = -1.0;
	}
    }

  if (dur == 0.0) xmax = .001; else xmax = dur;
  if (dur <= 0.0)
    {
      /* empty sound */
      label = "(no data)";
      xmax = .001;
    }
  else
    {
      if (xmax > dur) xmax = dur;
      if (x0 >= x1) x0 = x1 - .01;
    }

  if (xmax <= 0.0) xmax = .001;
  if (ymin >= ymax) ymin = ymax - .01;
  if (y0 >= y1) y0 = y1 - .01;

  ap = make_axis_info(cp, 0.0, xmax, ymin, ymax, (hook_label) ? hook_label : label, x0, x1, y0, y1, NULL);

  if (hook_label) free(hook_label);

  if (dur == 0.0)
    {
      ap->zx = 1.0;
      ap->sx = 1.0;
      ap->no_data = true;
    }
  else
    {
      if (ap->x_ambit != 0.0)
	{
	  ap->zx = (ap->x1 - ap->x0) / ap->x_ambit;
	  ap->sx = (ap->x0 - ap->xmin) / ap->x_ambit;
	}
      ap->no_data = false;
    }

  if (ap->y_ambit != 0.0)
    {
      ap->zy = (ap->y1 - ap->y0) / ap->y_ambit;
      ap->sy = (ap->y0 - ap->ymin) / ap->y_ambit;
    }

  cp->axis = ap;
  if (graphed == WITH_GRAPH) 
    initialize_scrollbars(cp);

  cp->active = CHANNEL_HAS_AXES;
  return(ymax_set);
}


void start_peak_env(chan_info *cp)
{
  if (cp->peak_env_in_progress) stop_peak_env(cp);
  start_peak_env_state(cp);
  cp->peak_env_in_progress = BACKGROUND_ADD(get_peak_env, (any_pointer_t)cp);
}


void add_channel_data(char *filename, chan_info *cp, channel_graph_t graphed)
{
  int chn = 0;
  mus_long_t framples;
  snd_info *sp;
  file_info *chdr, *hdr;
#if (!USE_NO_GUI)
  bool ymax_set;
#endif

  sp = cp->sound;
  hdr = sp->hdr;
  framples = hdr->samples / hdr->chans;
#if (!USE_NO_GUI)
  ymax_set = add_channel_data_1(cp, hdr->srate, framples, graphed);
#else
  add_channel_data_1(cp, hdr->srate, framples, graphed);
#endif

  chdr = copy_header(filename, hdr); /* need one separate from snd_info case */
  chn = cp->chan;
  if (chdr)
    {
      int fd;
      fd = snd_open_read(filename);
      if (fd != -1)
	{
	  snd_io *io;
	  snd_file_open_descriptors(fd,
				    filename, chdr->sample_type,
				    chdr->data_location,
				    chdr->chans,
				    chdr->type);
	  during_open(fd, filename, SND_OPEN_CHANNEL);
	  io = make_file_state(fd, chdr, chn, 0, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename, io, chdr, DONT_DELETE_ME, cp->edit_ctr, chn);

	  /* if sound tables have the saved maxamp data, grab it */
	  if (mus_sound_channel_maxamp_exists(filename, cp->chan))
	    {
	      mus_long_t pos;
	      /* fprintf(stderr, "reading max for %s %d\n", filename, cp->chan); */
	      set_ed_maxamp(cp, 0, mus_sound_channel_maxamp(filename, cp->chan, &pos));
	      set_ed_maxamp_position(cp, 0, pos);
	    }
	  /* else fprintf(stderr, "can't read max for %s %d\n", filename, chn); */
	}
    }

#if (!USE_NO_GUI)
  if ((show_full_range(ss)) && 
      (!ymax_set) && 
      (graphed == WITH_GRAPH))
    {
      mus_float_t ymax;
      if ((sp->channel_style == CHANNELS_COMBINED) &&
	  (sp->nchans > 1))
	{
	  if (cp->chan == (int)sp->nchans - 1)
	    {
	      uint32_t i;
	      ymax = 0.0;
	      for (i = 0; i < sp->nchans; i++)
		{
		  mus_float_t local_max;
		  local_max = channel_maxamp(sp->chans[i], 0);
		  if (local_max > ymax)
		    ymax = local_max;
		}
	      if (ymax > 1.0)
		{
		  for (i = 0; i < sp->nchans; i++)
		    {
		      axis_info *ap;
		      chan_info *ncp;
		      ncp = sp->chans[i];
		      ap = ncp->axis;
		      ap->ymin = -ymax;
		      ap->ymax = ymax;
		      ap->y_ambit = 2 * ap->ymax;
		      ap->y0 = ap->ymin;
		      ap->y1 = ap->ymax;
		      ap->zy = 1.0;
		      ap->sy = 0.0;
		      resize_sy_and_zy(ncp);
		      apply_y_axis_change(ncp);
		    }
		}
	    }
	}
      else
	{
	  ymax = channel_maxamp(cp, 0);
	  if (ymax > 1.0)
	    {
	      axis_info *ap;
	      ap = cp->axis;
	      ap->ymin = -ymax;
	      ap->ymax = ymax;
	      ap->y_ambit = 2 * ap->ymax;
	      ap->y0 = ap->ymin;
	      ap->y1 = ap->ymax;
	      ap->zy = 1.0;
	      ap->sy = 0.0;
	      resize_sy_and_zy(cp);
	      apply_y_axis_change(cp);
	    }
	}
    }
  else
    {
      if ((current_samples(cp) > PEAK_ENV_CUTOFF) &&
	  (!cp->edits[0]->peak_env) &&              /* perhaps created at initial graph time */
	  (sp->short_filename))                    /* region browser jumped in too soon during autotest */
	start_peak_env(cp);
    }
#endif
}


static void set_y_bounds(axis_info *ap)
{
  mus_float_t range;
  range = ap->zy * ap->y_ambit;
  ap->y0 = ap->ymin + ap->sy * ap->y_ambit;
  ap->y1 = (ap->y0 + range);
  if (ap->y1 > ap->ymax)
    {
      ap->y1 = ap->ymax;
      ap->y0 = ap->y1 - range;
    }
  if (ap->y0 < ap->ymin) ap->y0 = ap->ymin;
}


static bool is_NaN(double x) {return(x != x);}

void set_x_bounds(axis_info *ap)
{
  double range;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  range = ap->zx * ap->x_ambit;
  ap->x0 = ap->xmin + ap->sx * ap->x_ambit;
  if (is_NaN(ap->x0)) ap->x0 = 0.0;
  ap->x1 = (ap->x0 + range);
  if (ap->x1 > ap->xmax)
    {
      ap->x1 = ap->xmax;
      ap->x0 = ap->x1 - range;
    }
  if (ap->x0 < ap->xmin) ap->x0 = ap->xmin;
  ap->changed = true;
}


void apply_y_axis_change(chan_info *cp)
{
  snd_info *sp;
  axis_info *ap;
  ap = cp->axis;
  sp = cp->sound;

  set_y_bounds(ap);
  update_graph_or_warn(cp);
  update_enved_background_waveform(cp);

  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      uint32_t i;
      mus_float_t zy, sy;
      sy = ap->sy;
      zy = ap->zy;
      for (i = 0; i < sp->nchans; i++)
	{
	  chan_info *ncp;
	  ncp = sp->chans[i];
	  if (ncp != cp)
	    {
	      axis_info *nap;
	      nap = ncp->axis;
	      if (nap)
		{
		  nap->y_ambit = ap->y_ambit;
		  nap->ymin = ap->ymin;
		  nap->ymax = ap->ymax;
		  nap->zy = zy;
		  nap->sy = sy;
		  set_y_bounds(nap);
		  update_graph_or_warn(ncp);
		}
	    }
	}
    }
}


void set_x_axis_x0x1(chan_info *cp, double x0, double x1) 
{
  /* all callers are explicit (non-gui), so it should be safe to reset the z scroller here as well */
  axis_info *ap;

  ap = cp->axis;
  if (x0 >= 0.0) ap->x0 = x0; else ap->x0 = 0.0;
  ap->x1 = x1;
  if (x1 > ap->xmax) ap->xmax = x1;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin + .001;
  ap->x_ambit = ap->xmax - ap->xmin;
  if (ap->x_ambit != 0.0)
    {
      ap->zx = (x1 - x0) / ap->x_ambit;
      ap->sx = (x0 - ap->xmin) / ap->x_ambit;
      resize_sx_and_zx(cp);
    }
  apply_x_axis_change(cp); /* this checks sync */
  ap->changed = true;
}


static void set_x_axis_x0(chan_info *cp, mus_long_t left)
{
  if (cp)
    {
      axis_info *ap;
      ap = cp->axis; 
      if (ap) 
	{
	  double x1x0;
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x0 = (double)left / (double)snd_srate(cp->sound); 
	  if (is_NaN(ap->x0)) ap->x0 = 0.0;
	  set_x_axis_x0x1(cp, ap->x0, ap->x0 + x1x0);
	}
    }
}


static void set_x_axis_x1(chan_info *cp, mus_long_t right)
{
  if (cp)
    {
      axis_info *ap;
      ap = cp->axis; 
      if (ap) 
	{
	  double x1x0;
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x1 = (double)right / (double)snd_srate(cp->sound); 
	  set_x_axis_x0x1(cp, ap->x1 - x1x0, ap->x1);
	}
    }
}


void reset_x_display(chan_info *cp, double sx, double zx)
{
  axis_info *ap;
  bool reset_zx;

  ap = cp->axis;
  ap->sx = sx;
  reset_zx = (zx != ap->zx);
  ap->zx = zx;
  set_x_bounds(ap);
  if (reset_zx)
    resize_sx_and_zx(cp);
  else resize_sx(cp);
  update_graph_or_warn(cp);
}


static void update_xs(chan_info *ncp, axis_info *ap)
{
  axis_info *nap;
  nap = ncp->axis;
  if ((nap) && (nap->xmax > 0.0))
    {
      double scl;
      /* ncp->axis can be NULL here and elsewhere if we're in initialize_scrollbars
       *   of the first channel of a (brand-new) multi-channel sound with sync set --
       *   chans after the first have not necessarily set up an axis_info struct yet.
       */
      scl = ap->xmax / nap->xmax;
      reset_x_display(ncp, ap->sx * scl, ap->zx * scl);
    }
}


void apply_x_axis_change(chan_info *cp)
{
  int i;
  snd_info *sp;
  axis_info *ap;

  ap = cp->axis;
  sp = cp->sound;
  sp->lacp = cp;

  set_x_bounds(ap);
  update_graph_or_warn(cp);

  if (sp->sync != 0)
    {
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++) 
	if (cp != si->cps[i]) 
	  update_xs(si->cps[i], ap);
      free_sync_info(si);
    }
  else 
    {
      if (sp->channel_style != CHANNELS_SEPARATE)
	for (i = 0; i < (int)sp->nchans; i++)  /* not 1 (25-Oct-07: 1 might be selected chan, but 0 needs to reflect changes as well */
	  update_xs(sp->chans[i], ap);
    }
}


#define NO_ZOOM_FOCUS_LOCATION -1

static mus_long_t zoom_focus_location(chan_info *cp)
{
  if (cp->cursor_visible)
    return(cursor_sample(cp));

  if (selection_is_visible_in_channel(cp)) 
    /* this is complicated!  We want the relative position of the focussed-upon thing
     *   to stay the same, so that the zoom is smooth, but if we focus on just (for example)
     *   the selection start, we end up jumping unexpectedly if C-x v centered the selection.
     * So, return something in the current window related to the current selection.
     */
    {
      mus_long_t beg, end, mid, left, right;
      beg = selection_beg(cp);
      end = beg + selection_len();
      mid = (mus_long_t)(0.5 * (beg + end));
      left = cp->axis->losamp;
      right = cp->axis->hisamp;
      if ((mid > left) &&
	  (mid < right))
	return(mid);
      if ((beg > left) &&
	  (beg < right))
	return(beg);
      if ((end > left) &&
	  (end < right))
	return(end);
      /* return((mus_long_t)(0.5 * (left + right))); */
    }

  {
    mus_long_t pos;
    pos = zoom_focus_mix_in_channel_to_position(cp);
    if (pos >= 0)
      return(pos);
  }

  if (active_mark(cp))
    return(mark_beg(cp));

  return(NO_ZOOM_FOCUS_LOCATION);
}


void focus_x_axis_change(chan_info *cp, int focus_style)
{
  axis_info *ap;
  /* prepare for set_x_bounds given desired focus point, then drop into default
   * we need to set ap->sx to reflect the new zx value and the focus type
   * if focus_left - go on (nothing to do)
   *    focus_right - get old right, set sx to keep it as is
   *    focus_middle - ditto mid window
   *    focus_active - find the currently active entity, if none use focus_middle 
   *    focus_proc -- call proc
   */
  ap = cp->axis;
  if (ap->xmax == 0.0) return;

  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }

  if (focus_style != ZOOM_FOCUS_LEFT)
    {
      chan_info *ncp;
      mus_long_t newf;
      switch (focus_style)
	{
	case ZOOM_FOCUS_PROC:
	  ap->x0 = Xen_real_to_C_double(
                     Xen_call_with_6_args(ss->zoom_focus_proc,
					  C_int_to_Xen_sound(cp->sound->index),
					  C_int_to_Xen_integer(cp->chan),
					  C_double_to_Xen_real(ap->zx),
					  C_double_to_Xen_real(ap->x0),
					  C_double_to_Xen_real(ap->x1),
					  C_double_to_Xen_real(ap->x_ambit),
					  S_zoom_focus_style " procedure"));
	  break;

	case ZOOM_FOCUS_RIGHT:   
	  ap->x0 = ap->x1 - ap->zx * ap->x_ambit; 
	  break;

	case ZOOM_FOCUS_MIDDLE:  
	  ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx * ap->x_ambit); 
	  break;

	case ZOOM_FOCUS_ACTIVE:
	  ncp = virtual_selected_channel(cp);
	  /* axes should be the same, since all move together in this mode */
	  newf = zoom_focus_location(ncp);

	  if (newf == NO_ZOOM_FOCUS_LOCATION)
	    {
	      snd_info *sp;
	      int sync;
	      sp = cp->sound;
	      sync = sp->sync;
	      if (sync != 0)
		{
		  uint32_t i;
		  for (i = 0; i < sp->nchans; i++)
		    if ((int)i != ncp->chan)
		      {
			newf = zoom_focus_location(sp->chans[i]);
			if (newf != NO_ZOOM_FOCUS_LOCATION)
			  break;
		      }
		  if (newf == NO_ZOOM_FOCUS_LOCATION)
		    {
		      int j;
		      /* geez, maybe it's in a separate syncd sound */
		      for (j = 0; j < ss->max_sounds; j++)
			{
			  sp = ss->sounds[j];
			  if ((sp) && (sp->inuse == SOUND_NORMAL) && (sp->sync == sync) && (sp != cp->sound))
			    for (i = 0; i < sp->nchans; i++)
			      {
				newf = zoom_focus_location(sp->chans[i]);
				if (newf != NO_ZOOM_FOCUS_LOCATION)
				  break;
			      }}}}}

	  if (newf != NO_ZOOM_FOCUS_LOCATION)
	    {
	      double loc;
	      loc = (double)newf / (double)snd_srate(ncp->sound);
	      if ((loc > ap->x0) && 
		  (loc < ap->x1) && 
		  (ap->x1 > ap->x0))
		/* try to maintain current relative position in window */
		{
		  double pos;
		  pos = (loc - ap->x0) / (ap->x1 - ap->x0);
		  ap->x0 = loc - pos * ap->zx * ap->x_ambit;
		}
	      else ap->x0 = loc - 0.5 * ap->zx * ap->x_ambit;
	    }
	  else ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx * ap->x_ambit); 
	  break;
	}

      if (is_NaN(ap->x0)) ap->x0 = 0.0;
      if (ap->x0 < 0.0) ap->x0 = 0.0;
      if (ap->x_ambit != 0.0)
	ap->sx = (double)(ap->x0 - ap->xmin) / (double)(ap->x_ambit);
    }
  apply_x_axis_change(cp);
}


void sx_incremented(chan_info *cp, double amount)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx += (ap->zx * amount);
  if (ap->sx < 0.0) ap->sx = 0.0;
  if (ap->sx > (1.0 - ap->zx)) ap->sx = 1.0 - ap->zx;
  apply_x_axis_change(cp);
  resize_sx(cp);
}


void zx_incremented(chan_info *cp, double amount)
{ 
  /* kbd arrows etc -- needs to be able to return to original */
  axis_info *ap;
  mus_long_t samps;
  samps = current_samples(cp);
  ap = cp->axis;
  if ((amount >= 1.0) || 
      ((samps > 0) && ((ap->zx * (double)samps) > (amount / 2.0))))
    {
      ap->zx *= amount;
      if (ap->zx > 1.0) ap->zx = 1.0;
      focus_x_axis_change(cp, zoom_focus_style(ss));
      /* resize_sx(cp); */
      resize_sx_and_zx(cp); /* need zx also -- this changes its position, not its size. 22-Aug-12 */
    }
}


int move_axis(chan_info *cp, int x)
{
  /* need to scroll axis forward or backward -- distance per call depends on x distance from end of axis */
  double off;
  int nx;
  axis_info *ap;

  ap = cp->axis;
  if (x > ap->x_axis_x1)
    {
      off = x - ap->x_axis_x1;
      ap->sx += (off * ap->zx / 4000.0); /* increase 4000.0 to make the axis move more slowly (and below) */
      if (ap->sx > (1.0 - ap->zx)) ap->sx = 1.0 - ap->zx;
      nx = ap->x_axis_x1;
    }
  else
    {
      off = ap->x_axis_x0 - x;
      ap->sx -= (off * ap->zx / 4000.0);
      if (ap->sx < 0.0) ap->sx = 0.0;
      nx = ap->x_axis_x0;
    }

  apply_x_axis_change(cp);
  resize_sx(cp);

  return(nx);
}


void set_axes(chan_info *cp, double x0, double x1, mus_float_t y0, mus_float_t y1)
{
  /* use to change channel_style to channels_separate, and restore axes upon update */
  axis_info *ap;

  ap = cp->axis;
  ap->x0 = x0;
  ap->x1 = x1;
  if (ap->x_ambit != 0.0)
    {
      ap->zx = (x1 - x0) / ap->x_ambit;
      ap->sx = (x0 - ap->xmin) / ap->x_ambit;
    }
  resize_sx(cp);

  ap->y0 = y0;
  ap->y1 = y1;
  if (ap->y_ambit != 0.0)
    {
      ap->zy = (y1 - y0) / ap->y_ambit;
      ap->sy = (y0 - ap->ymin) / ap->y_ambit;
    }
  resize_sy_and_zy(cp);
}


/* these are copies from snd-axis.c; didn't want to use macros here */

static int local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((int)(ap->x_base + val * ap->x_scale));
}

static int local_grf_y(mus_float_t val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((int)(ap->y_base + val * ap->y_scale));
}


static void display_y_zero(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if ((ap->y0 < 0.0) && (ap->y1 > 0.0))
    {
      int zero;
      zero = local_grf_y(0.0, ap);
      draw_line(copy_context(cp), ap->x_axis_x0, zero, ap->x_axis_x1, zero);
      if (cp->printing) 
	ps_draw_line(ap, ap->x_axis_x0, 0, ap->x_axis_x1, 0);
    }
}


#if USE_MOTIF
  #define CHN_LABEL_OFFSET -5
#else
  #define CHN_LABEL_OFFSET -18
#endif

static char chn_id_str[LABEL_BUFFER_SIZE];


static void display_channel_id(chan_info *cp, graphics_context *ax, int height, int chans)
{
  if (cp->show_axes == SHOW_NO_AXES) 
    return;

  if ((chans > 1) || 
      (cp->edit_ctr > 0))
    {
      int x0, y0;
      color_t old_color = 0;

      set_peak_numbers_font(cp, ax);
      if (cp->printing) ps_set_peak_numbers_font();

      x0 = 5;
      y0 = height + CHN_LABEL_OFFSET;
      if (cp->edit_ctr == 0)
	snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%d]", cp->chan + 1);
      else snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%d]: %d", cp->chan + 1, cp->edit_ctr);

      if (cp == selected_channel())
	{
	  old_color = get_foreground_color(copy_context(cp));
	  set_foreground_color(copy_context(cp), ss->red);
	}

      draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
      if (cp->printing) 
	ps_draw_string(cp->axis, x0, y0, chn_id_str);
      if (cp == selected_channel())
	set_foreground_color(copy_context(cp), old_color);
    }
}


#if USE_MOTIF
  #define SELECTION_FFT_LABEL_OFFSET -3
#else
  #define SELECTION_FFT_LABEL_OFFSET -15
#endif


static void display_selection_transform_size(chan_info *cp, axis_info *fap, graphics_context *ax)
{
  int x0, y0;
  if (fap->height < 60) return;
  set_tiny_numbers_font(cp, ax);
  if (cp->printing) ps_set_tiny_numbers_font();
  y0 = fap->height + fap->y_offset + SELECTION_FFT_LABEL_OFFSET;
  x0 = fap->x_axis_x0 + 20;
  snprintf(chn_id_str, LABEL_BUFFER_SIZE, 
	       "(len: %" print_mus_long "/%" print_mus_long ")", 
	       selection_len(), 
	       cp->selection_transform_size);
  draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
  if (cp->printing) ps_draw_string(fap, x0, y0, chn_id_str);
}


snd_info *make_simple_channel_display(int srate, int initial_length, fw_button_t with_arrows, graph_style_t grf_style, widget_t container, bool with_events)
{
  snd_info *sp;
  chan_info *cp;
  file_info *hdr;
  int err;

  sp = make_basic_snd_info(1); /* 1 chan */
  sp->nchans = 1;
  sp->inuse = SOUND_WRAPPER;
  sp->active = true;
  sp->hdr = (file_info *)calloc(1, sizeof(file_info));
  hdr = sp->hdr;
  hdr->samples = initial_length;
  hdr->srate = srate;
  hdr->comment = NULL;
  hdr->chans = 1;
  err = add_channel_window(sp, 0, 0, 0, container, with_arrows, with_events);
  if (err == -1)
    {
      free(sp->chans);
      free(sp->hdr);
      free(sp);
      return(NULL);
    }
  cp = sp->chans[0];
  cp->editable = false;
  cp->sound = sp;
  cp->hookable = WITHOUT_HOOK;
  add_channel_data_1(cp, srate, initial_length, WITH_GRAPH);
  cp->time_graph_style = grf_style;
  cp->dot_size = dot_size(ss);
  return(sp);
}

static graphics_context *combined_context(chan_info *cp);

static int make_wavogram(chan_info *cp);


typedef enum {NORMAL_GRAPH, ENVED_GRAPH, MARKS_GRAPH} graph_choice_t;

static int make_graph_1(chan_info *cp, double cur_srate, graph_choice_t graph_choice, bool *two_sided)
{
  snd_info *sp;
  int j = 0;
  mus_long_t samps;
  axis_info *ap;
  mus_float_t samples_per_pixel;
  double x;

  /* in long files with small windows we can run into floating point errors that accumulate
   * in the loop (incrementing by a truncated amount each time), making the x amounts smaller
   * than they should be (so the graph appears squeezed).
   *
   * There is a similar problem with long files (say 1 hour at 44KHz), in which 
   * the x increment gets quantized making the graphs step-like, so the
   * axis_info fields x0, x1, and x_scale are doubles as well. The y axis is less problematic
   * since we are assuming sound files here.
   *
   * And if the data window itself is small (in the sense that we're looking at individual samples)
   * we need to make decisions about in-between sample mouse clicks, and the cursor placement
   * must match the sample placement (which means that the initial x-axis offset from the
   * first sample (similarly the last) must be taken into account).  Currently, the selection
   * definition (by mouse drag) has a sloppy rounding mechanism so that
   * within reason only samples within the selection-rectangle are in the final selection,
   * and cursor placement (mouse click) rounds to the nearest sample (snd-xchn.c cursor_round).
   *
   * And lastly, the axis x_scale double can be enormous whereas the between-sample choice can
   * be tiny, so we're pushing the arithmetic into dangerous realms.  This stuff has been tested
   * on some extreme cases (week-long 44KHz stereo), but there's always another special case...
   */

  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  graphics_context *ax = NULL;

  sp = cp->sound;
  ap = cp->axis;

  /* check for no graph */
  if ((!ap) || 
      (!(ap->graph_active)) || 
      (ap->x0 == ap->x1)) 
    return(0);

  /* check for wavogram */
  if (cp->time_graph_type == GRAPH_AS_WAVOGRAM) 
    return(make_wavogram(cp)); 

  if (graph_choice != ENVED_GRAPH)
    {
      ap->losamp = snd_round_mus_long_t(ap->x0 * cur_srate); /* was ceil??? */
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (mus_long_t)((ap->x1 * cur_srate) + 0.5); /* + 0.5 for 1-sample case */
      if ((ap->losamp == 0) && (ap->hisamp == 0)) return(0);
    }

  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  if ((x_start == x_end) && (samps > 10)) return(0); /* must be too-tiny graph */

  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (double)pixels);

  if (cp->printing) ps_allocate_grf_points();

  if (graph_choice == NORMAL_GRAPH)
    {
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	ax = combined_context(cp); 
      else ax = copy_context(cp);

      if (cp->printing) ps_fg(cp, ax);
    }

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)) ||
      ((cp->time_graph_style == GRAPH_FILLED) && 
       (samples_per_pixel < 25.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      double incr;
      int grfpts;
      /* i.e. individual samples are widely spaced, so we have to be careful about placement
       *   mouse uses grf_x so in this case we have to also (to make the cursor hit the dots etc) 
       */
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
      if (!sf) return(0);

      incr = (double)1.0 / cur_srate;
      grfpts = (int)(ap->hisamp - ap->losamp + 1);
      if (cp->printing)
	{
	  for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	    {
	      mus_float_t fsamp;
	      fsamp = read_sample(sf);
	      set_grf_point(local_grf_x(x, ap), j, local_grf_y(fsamp, ap));
	      ps_set_grf_point(x, j, fsamp);
	    }
	}
      else
	{
	  for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	    set_grf_point(local_grf_x(x, ap), j, local_grf_y(read_sample(sf), ap));
	}
      if (graph_choice == NORMAL_GRAPH)
	{
	  if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	    {
	      color_t old_color;
	      old_color = get_foreground_color(ax);
	      set_foreground_color(ax, cp->combined_data_color);
	      draw_grf_points(cp->dot_size, ax, j, ap, 0.0, cp->time_graph_style);
	      set_foreground_color(ax, old_color);
	    }
	  else draw_grf_points(cp->dot_size, ax, j, ap, 0.0, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_grf_points(ap, j, 0.0, cp->time_graph_style, cp->dot_size);
	}
      else 
	{
	  if (graph_choice == ENVED_GRAPH)
	    (*two_sided) = false;
	}
    }
  else
    {
      /* take min, max */
      if (peak_env_usable(cp, samples_per_pixel, ap->hisamp, true, cp->edit_ctr, false)) /* true = start new background amp env process if needed */
	j = peak_env_graph(cp, samples_per_pixel, (graph_choice != ENVED_GRAPH) ? ((int)snd_srate(sp)) : 1);
      else
	{
	  mus_float_t ymin, ymax, xf, pinc = 0.0;
	  mus_long_t ioff;
	  int xi;

	  if ((ap->hisamp - ap->losamp) > (current_samples(cp) / 4))
	    {    
                                             /* we're trying to view a large portion of the (large) sound */
	      if (cp->peak_env_in_progress)
		{                            /* is peak-env background process is still working on it */
		  peak_env_info *ep;
		  ep = cp->edits[cp->edit_ctr]->peak_env;
		  if ((ep) && samples_per_pixel >= (mus_float_t)(ep->samps_per_bin))
		    {                        /* and it will be useful when it finishes */
		      cp->waiting_to_make_graph = true;
		      return(0);             /* so don't run two enormous data readers in parallel */
		    }
		}
	    }
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (!sf) return(0);

	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = MIN_INIT;
	  ymax = MAX_INIT;
	  if (cp->printing) pinc = samples_per_pixel / cur_srate;
	  ap->changed = false;
	  ss->stopped_explicitly = false;
	  for (ioff = ap->losamp; ioff <= ap->hisamp; ioff++)
	    {
	      mus_float_t samp;
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
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
		  xf -= samples_per_pixel;
		  ymin = MIN_INIT;
		  ymax = MAX_INIT;
		  if (samps > 10000000)
		    {
		      check_for_event();
		      if ((ap->changed) || (ss->stopped_explicitly) || (cp->active < CHANNEL_HAS_EDIT_LIST))
			{
			  ss->stopped_explicitly = false;
			  ap->changed = false;
			  break;
			}
		    }
		}
	    }
	}
      if (graph_choice == NORMAL_GRAPH)
	{
	  if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	    {
	      color_t old_color;
	      old_color = get_foreground_color(ax);
	      set_foreground_color(ax, cp->combined_data_color);
	      draw_both_grf_points(cp->dot_size, ax, j, cp->time_graph_style);
	      set_foreground_color(ax, old_color);
	    }
	  else draw_both_grf_points(cp->dot_size, ax, j, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_both_grf_points(ap, j, cp->time_graph_style, cp->dot_size);
	}
      else 
	{
	  if (graph_choice == ENVED_GRAPH)
	    (*two_sided) = true;
	}
    }
  free_snd_fd(sf);

  if (graph_choice == NORMAL_GRAPH)
    {
      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	{
	  copy_context(cp); /* reset for axes etc */
	  if (cp->printing)
	    ps_reset_color();
	}
      if ((cp->with_verbose_cursor) && 
	  (cp->cursor_on) &&
	  (cursor_sample(cp) >= ap->losamp) && 
	  (cursor_sample(cp) <= ap->hisamp))
	show_cursor_info(cp); 
    }

  return(j);
}


static int make_graph(chan_info *cp) 
{
  return(make_graph_1(cp, (double)(snd_srate(cp->sound)), NORMAL_GRAPH, NULL));
}


int make_dragged_marks_graph(chan_info *cp) 
{
  return(make_graph_1(cp, (double)(snd_srate(cp->sound)), MARKS_GRAPH, NULL));
}


int make_background_graph(chan_info *cp, int srate, bool *two_sided) /* (for envelope editor) */
{
  return(make_graph_1(cp, (double)srate, ENVED_GRAPH, two_sided));
}


void make_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end)
{
  /* assume here that everything is already checked and set up (from snd-mix.c, dragging) */
  snd_info *sp;
  int j = 0;
  mus_long_t samps;
  axis_info *ap;
  mus_float_t samples_per_pixel;
  double x, cur_srate, beg_in_seconds, end_in_seconds;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  graphics_context *ax = NULL;

  sp = cp->sound;
  cur_srate = (double)snd_srate(sp);
  ap = cp->axis;
  ax = copy_context(cp);
  if (!(ap->ax))
    ap->ax = cp->ax;
  if (beg > 0) beg--;

  /* get samps / pixel */
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (double)pixels);

  if (beg < ap->losamp) beg = ap->losamp;
  if (end > ap->hisamp) end = ap->hisamp;
  beg_in_seconds = (double)beg / cur_srate;
  end_in_seconds = (double)end / cur_srate;

#if USE_GTK
  cairo_push_group(ss->cr);
#endif

  erase_rectangle(cp, ap->ax, 
		  local_grf_x(beg_in_seconds, ap), 
		  ap->y_axis_y1,
		  local_grf_x(end_in_seconds, ap) - local_grf_x(beg_in_seconds, ap) + 1, 
		  ap->y_axis_y0 - ap->y_axis_y1);

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE)) ||
      ((cp->time_graph_style == GRAPH_FILLED) && (samples_per_pixel < 25.0) && (samps < POINT_BUFFER_SIZE)))
    {
      double incr;
      int grfpts;
      sf = init_sample_read(beg, cp, READ_FORWARD);

      incr = (double)1.0 / cur_srate;
      grfpts = (int)(end - beg + 1);

      for (j = 0, x = beg_in_seconds; j < grfpts; j++, x += incr)
	set_grf_point(local_grf_x(x, ap), j, local_grf_y(read_sample(sf), ap));

      draw_grf_points(cp->dot_size, ax, j, ap, 0.0, cp->time_graph_style);
    }
  else
    {
      if (peak_env_usable(cp, samples_per_pixel, ap->hisamp, true, cp->edit_ctr, false)) /* true = start new background amp env process if needed */
	j = peak_env_partial_graph(cp, beg, end, samples_per_pixel, (int)snd_srate(sp));
      else
	{
	  mus_float_t ymin, ymax, xf;
	  mus_long_t ioff;
	  int xi;
	  if ((end - beg) > (current_samples(cp) / 4))
	    {    
                                             /* we're trying to view a large portion of the (large) sound */
	      if (cp->peak_env_in_progress)
		{                            /* is peak-env background process is still working on it */
		  peak_env_info *ep;
		  ep = cp->edits[cp->edit_ctr]->peak_env;
		  if ((ep) && samples_per_pixel >= (mus_float_t)(ep->samps_per_bin))
		    {                        /* and it will be useful when it finishes */
		      cp->waiting_to_make_graph = true;
#if USE_GTK
		      cairo_pop_group_to_source(ss->cr);
#endif
		      return;               /* so don't run two enormous data readers in parallel */
		    }
		}
	    }
	  sf = init_sample_read(beg, cp, READ_FORWARD);

	  j = 0;      /* graph point counter */
	  x = beg_in_seconds;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = MIN_INIT;
	  ymax = MAX_INIT;
	  ap->changed = false;
	  for (ioff = beg; ioff <= end; ioff++)
	    {
	      mus_float_t samp;
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j, local_grf_y(ymin, ap), local_grf_y(ymax, ap));
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = MIN_INIT;
		  ymax = MAX_INIT;
		}
	    }
	}
      draw_both_grf_points(cp->dot_size, ax, j, cp->time_graph_style);
    }

  free_snd_fd(sf);

#if (!USE_GTK)
  display_selection(cp);
#endif
  if (cp->show_y_zero) display_y_zero(cp);
      
  if ((cp->with_verbose_cursor) && 
      (cp->cursor_on) &&
      (cursor_sample(cp) >= beg) && 
      (cursor_sample(cp) <= end))
    show_cursor_info(cp); 

#if USE_GTK
  cairo_pop_group_to_source(ss->cr);
  cairo_paint(ss->cr);
#endif

  display_channel_marks(cp);
}


/* these next two procedures split "make_graph" into two pieces; the first
 *   gets the data to be graphed, using the amp envs and so on, and
 *   the second displays it.  (The first cp+pos may have no relation
 *   to the second cp -- allow arbitrary overlapping etc).
 */

Xen make_graph_data(chan_info *cp, int edit_pos, mus_long_t losamp, mus_long_t hisamp)
{
  int j = 0;
  mus_long_t samps, ioff;
  axis_info *ap;
  mus_float_t samples_per_pixel, xf;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  mus_float_t *data = NULL, *data1 = NULL;
  int data_size = 0;

  ap = cp->axis;
  if (losamp == -1) losamp = ap->losamp;
  if (hisamp == -1) hisamp = ap->hisamp;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = hisamp - losamp + 1;
  if ((samps <= 0) || ((x_start == x_end) && (samps > 10))) return(Xen_false);
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01;
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (double)pixels);

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      int i;
      data_size = (int)samps;
      sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
      if (!sf) return(Xen_false); /* should this throw an error? (CHANNEL_BEING_DEALLOCATED) */

      data = (mus_float_t *)malloc(data_size * sizeof(mus_float_t));
      for (i = 0; i < data_size; i++)
	data[i] = read_sample(sf);
    }
  else
    {
      if (peak_env_usable(cp, samples_per_pixel, hisamp, false, edit_pos, true)) 
	{
	  double step, xk;
	  mus_float_t ymin, ymax;
	  peak_env_info *ep;

	  data_size = pixels + 1;
	  data = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  data1 = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  ep = cp->edits[edit_pos]->peak_env;
	  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
	  xf = (double)(losamp) / (double)(ep->samps_per_bin);
	  j = 0;
	  ioff = losamp;
	  xk = (double)ioff;
	  ymin = ep->fmax;
	  ymax = ep->fmin;

	  while (ioff <= hisamp)
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
	      ioff = (mus_long_t)xk;
	      data[j] = ymin;
	      data1[j] = ymax;
	      j++;
	      ymin = ep->fmax;
	      ymax = ep->fmin;
	    }
	}
      else
	{
	  mus_float_t ymin, ymax;

	  data_size = pixels + 1;
	  sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
	  if (!sf) return(Xen_false);

	  data = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  data1 = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  j = 0;      /* graph point counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  ss->stopped_explicitly = false;

	  for (ioff = losamp, xf = 0.0; ioff <= hisamp; ioff++)
	    {
	      mus_float_t samp;
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  data[j] = ymin;
		  data1[j] = ymax;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = 100.0;
		  ymax = -100.0;
		  if (samps > 10000000)
		    {
		      if ((ap->changed) || (ss->stopped_explicitly))
			{
			  ss->stopped_explicitly = false;
			  ap->changed = false;
			  break;
			}
		    }
		}
	    }
	}
    }

  free_snd_fd(sf); 
  if (data1)
    return(Xen_list_2(xen_make_vct(data_size, data),
		      xen_make_vct(data_size, data1)));
  else return(xen_make_vct(data_size, data));
}


void draw_graph_data(chan_info *cp, mus_long_t losamp, mus_long_t hisamp, int data_size, mus_float_t *data, mus_float_t *data1, graphics_context *ax, graph_style_t style)
{
  mus_long_t i, samps;
  axis_info *ap;
  snd_info *sp;

  ap = cp->axis;
  sp = cp->sound;
  if (!data1)
    {
      double start_time = 0.0, cur_srate, x, incr;

      cur_srate = (double)snd_srate(sp);
      if (losamp == -1)
	losamp = snd_round_mus_long_t(ap->x0 * cur_srate);
      start_time = (double)(losamp) / cur_srate;
      if (hisamp == -1)
	hisamp = (mus_long_t)(ap->x1 * cur_srate);
      samps = hisamp - losamp + 1;
      if (samps <= 0) return;
      if (samps > data_size) samps = data_size;
      incr = (double)1.0 / cur_srate;

      for (i = 0, x = start_time; i < samps; i++, x += incr)
	set_grf_point(local_grf_x(x, ap), 
		      i, 
		      local_grf_y(data[i], ap));
      draw_grf_points(cp->dot_size, ax, samps, ap, 0.0, style);
    }
  else
    {
      int xi;
      if (losamp == -1)
	losamp = 0;
      if (hisamp == -1)
	hisamp = data_size - 1;
      samps = hisamp - losamp + 1;
      if (samps <= 0) return;
      if (samps > data_size) samps = data_size;
      for (i = 0, xi = (ap->x_axis_x0 + losamp); i < samps; i++, xi++)
	set_grf_points(xi, i, 
		       local_grf_y(data[i], ap), 
		       local_grf_y(data1[i], ap));
      draw_both_grf_points(cp->dot_size, ax, samps, style);
    }
}


static int compare_peak_amps(const void *pk1, const void *pk2)
{
  if (((fft_peak *)pk1)->amp > ((fft_peak *)pk2)->amp) return(-1);
  else if (((fft_peak *)pk1)->amp == ((fft_peak *)pk2)->amp) return(0);
  return(1);
}


static char ampstr[LABEL_BUFFER_SIZE];
#define AMP_ROOM 45
#define AMP_ROOM_CUTOFF 3.0

static void display_peaks(chan_info *cp, axis_info *fap, mus_float_t *data, 
			  mus_long_t start, mus_long_t end, 
			  mus_long_t losamp, mus_long_t hisamp, 
			  mus_float_t samps_per_pixel, bool fft_data, mus_float_t fft_scale /* fourier scale factor or 0.0 */)
{
  int num_peaks, row, frq_col, frq_strlen, tens, i, amp_col, amp_strlen, row_height = 15;
  mus_long_t samps, fsamps;
  bool with_amps;
  mus_float_t amp0;
  graphics_context *ax;
  fft_peak *peak_freqs = NULL;
  fft_peak *peak_amps = NULL;
  color_t old_color = (color_t)0; /* None in Motif, NULL in Gtk, 0 in no-gui */

  /* "end" is the top displayed frequency in the FFT frequency axis (srate*spectrum_end/2)
   * "hisamp - losamp" is how many samples of data we have
   */

  /* "tens" is for prettyf in snd-utils, if -1 -> print int else sets decimals */
  samps = hisamp - losamp;
  fsamps = end - start;
  if (samps > (fsamps * 10)) 
    tens = 2; 
  else 
    if (samps > fsamps) 
      tens = 1; 
    else
      if (samps > (fsamps / 10)) 
	tens = 0; 
      else tens = -1;

#if (!USE_GTK)
  row_height = (int)(1.25 * number_height(PEAKS_FONT(ss)));
#else
  row_height = (int)(0.75 * number_height(PEAKS_FONT(ss)));
#endif
  if (row_height < 10) row_height = 10;

  num_peaks = (fap->y_axis_y0 - fap->y_axis_y1) / (row_height + 5);
  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    num_peaks /= cp->sound->nchans;
  if (num_peaks <= 0) return;
  if (num_peaks > cp->max_transform_peaks) num_peaks = cp->max_transform_peaks;

  peak_freqs = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));
  peak_amps = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));

  if (fft_data)
    num_peaks = find_and_sort_transform_peaks(data, peak_freqs, num_peaks, losamp, hisamp /* "fft size" */, samps_per_pixel, fft_scale); 
  else num_peaks = find_and_sort_peaks(data, peak_freqs, num_peaks, losamp, hisamp);

  if ((num_peaks == 0) || 
      ((num_peaks == 1) && 
       ((peak_freqs[0].freq == 1.0) || (peak_freqs[0].freq == 0.0))))
    {
      free(peak_freqs); 
      free(peak_amps); 
      return;
    }

  frq_strlen = (int)ceil(log10(peak_freqs[num_peaks - 1].freq * end)) + ((tens <= 0) ? 0 : (tens + 1)) + 1;
  if (frq_strlen < 1) frq_strlen = 1;

  with_amps = (fap->width > ((30 + 5 * frq_strlen + AMP_ROOM) * AMP_ROOM_CUTOFF));
  amp_strlen = 3;

  frq_col = fap->x_axis_x1 - frq_strlen * 5; 
  amp_col = fap->x_axis_x1 - AMP_ROOM + 15;

  if (with_amps) 
    {
      frq_col -= AMP_ROOM;
      if ((fft_data) && 
	  ((cp->transform_normalization == DONT_NORMALIZE) ||
	   (cp->min_dB < -60)))
	{
	  frq_col -= 5;
	  amp_col -= 5;
	  amp_strlen = 4;
	}
    }

  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    {
      ax = combined_context(cp);
      old_color = get_foreground_color(ax);
      set_foreground_color(ax, cp->combined_data_color);
    }
  else ax = copy_context(cp);

  if (num_peaks > 6)
    {
      for (i = 0; i < num_peaks; i++) peak_amps[i] = peak_freqs[i];
      qsort((void *)peak_amps, num_peaks, sizeof(fft_peak), compare_peak_amps);
      if (num_peaks < 12) amp0 = peak_amps[2].amp; else amp0 = peak_amps[5].amp;
      set_bold_peak_numbers_font(cp, ax); /* in snd-g|xchn.c */
      if (cp->printing) ps_set_bold_peak_numbers_font();

#if (!USE_GTK)
      row = fap->y_axis_y1 + row_height;
#else
      row = fap->y_axis_y1;
#endif
      if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
	row += row_height * num_peaks * cp->chan;

      for (i = 0; i < num_peaks; i++)
	{
	  if (peak_freqs[i].amp >= amp0)
	    {
	      char *fstr;
	      mus_float_t px;

	      px = peak_freqs[i].freq;
	      fstr = prettyf(px * end, tens);
	      draw_string(ax, frq_col, row, fstr, strlen(fstr));
	      if (cp->printing) ps_draw_string(fap, frq_col, row, fstr);
	      free(fstr);
	      fstr = NULL;

	      if (with_amps)
		{
		  if ((fft_data) && (cp->fft_log_magnitude))
		    snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", in_dB(cp->min_dB, cp->lin_dB, peak_freqs[i].amp));
		  else snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", amp_strlen, peak_freqs[i].amp);
		  draw_string(ax, amp_col, row, ampstr, strlen(ampstr));
		  if (cp->printing) 
		    ps_draw_string(fap, amp_col, row, ampstr);
		}
	    }
	  row += row_height;
	}
    }
  else amp0 = 100.0;

  set_peak_numbers_font(cp, ax);
  if (cp->printing) ps_set_peak_numbers_font();
#if (!USE_GTK)
  row = fap->y_axis_y1 + row_height;
#else
  row = fap->y_axis_y1;
#endif
  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    row += row_height * num_peaks * cp->chan;

  for (i = 0; i < num_peaks; i++)
    {
      if (peak_freqs[i].amp < amp0)
	{
	  mus_float_t px;
	  char *fstr;

	  px = peak_freqs[i].freq;
	  fstr = prettyf(px * end, tens);
	  draw_string(ax, frq_col, row, fstr, strlen(fstr));
	  if (cp->printing) ps_draw_string(fap, frq_col, row, fstr);
	  free(fstr);
	  fstr = NULL;

	  if (with_amps)
	    {
	      if ((fft_data) && (cp->fft_log_magnitude))
		snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", in_dB(cp->min_dB, cp->lin_dB, peak_freqs[i].amp));
	      else snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", amp_strlen, peak_freqs[i].amp);
	      draw_string(ax, amp_col, row, ampstr, strlen(ampstr));
	      if (cp->printing) 
		ps_draw_string(fap, amp_col, row, ampstr);
	    }
	}
      row += row_height;
    }

  /* superimposed context reset takes place in make_fft_graph */
  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    set_foreground_color(ax, old_color);

  if (peak_freqs) free(peak_freqs); 
  if (peak_amps) free(peak_amps);
}


static void make_fft_graph(chan_info *cp, axis_info *fap, graphics_context *ax, with_hook_t with_hook)
{
  /* axes are already set, data is in the fft_info struct -- don't reset here! */
  /* since the fft size menu callback can occur while we are calculating the next fft, we have to lock the current size until the graph goes out */
  fft_info *fp;
  snd_info *sp;
  mus_float_t *data;
  mus_float_t incr, x, scale;
  mus_long_t i, hisamp, losamp = 0;
  int lines_to_draw = 0;
  mus_float_t samples_per_pixel;
  int logx, logy;
  mus_float_t pslogx, pslogy, saved_data = 0.0;
  mus_float_t minlx = 0.0, curlx = 0.0, lscale = 1.0;
  mus_float_t *fft_phases = NULL;
  bool free_phases = false;

  sp = cp->sound;
  fp = cp->fft;
  if (chan_fft_in_progress(cp)) return;
  if (!fap->graph_active) return;
  data = fp->data;
  /* these lo..hi values are just for upcoming loops -- not axis info */

  if (cp->transform_type == FOURIER)
    {
      hisamp = fp->current_size * cp->spectrum_end / 2;
      if ((cp->fft_log_frequency) && 
	  ((snd_srate(sp) * 0.5 * cp->spectrum_start) < log_freq_start(ss)))
	losamp = (mus_long_t)(ceil(fp->current_size * log_freq_start(ss) / (mus_float_t)snd_srate(sp)));
      else losamp = fp->current_size * cp->spectrum_start / 2;
      incr = (mus_float_t)snd_srate(sp) / (mus_float_t)(fp->current_size);
    }
  else
    {
      /* hisamp here is in terms of transform values, not original sampled data values */
      hisamp = fp->current_size * cp->spectrum_end;
      losamp = fp->current_size * cp->spectrum_start;
      incr = 1.0;
    }

  if ((losamp < 0) || (losamp >= hisamp)) return;

  /* no scaling etc here!! see snd_display_fft in snd-fft.c */
  scale = fp->scale;
  if (cp->printing) ps_allocate_grf_points();
  samples_per_pixel = (mus_float_t)((double)(hisamp - losamp) / (mus_float_t)(fap->x_axis_x1 - fap->x_axis_x0));
  if (cp->printing) ps_fg(cp, ax);

  if (cp->fft_log_frequency)
    {
      mus_float_t maxlx, max_data, log_range, fap_range;

      fap_range = fap->x1 - fap->x0;
      if (fap->x0 > 1.0) minlx = log(fap->x0); else minlx = 0.0;
      maxlx = log(fap->x1);
      log_range = (maxlx - minlx);
      lscale = fap_range / log_range;

      /* I think this block is getting the max of all the DC to losamp freqs to represent the 0th point of the graph?? */
      max_data = data[0];
      if ((spectrum_start(ss) == 0.0) && (losamp > 0))
	{
	  for (i = 1; i <= losamp; i++)
	    if (data[i] > max_data)
	      max_data = data[i];
	}
      saved_data = data[losamp];
      data[losamp] = max_data;
    }

  if ((samples_per_pixel < 4.0) &&
      ((hisamp - losamp) < POINT_BUFFER_SIZE)) /* point buffer size in snd-draw.c:
						*     hisamp - losamp can be anything (huge fft), and x1 - x0 can be > 2400 (very large screen) 
						*     so we have to check hisamp or make the buffer bigger 
						*/
    {
      /* save phases if needed */
      if ((cp->fft_with_phases) &&
	  (fp->phases) &&
	  (cp->transform_type == FOURIER))
	{
	  if (losamp == 0)
	    {
	      fft_phases = fp->phases;
	      free_phases = false;
	    }
	  else
	    {
	      mus_long_t size;
	      size = hisamp - losamp + 1;
	      fft_phases = (mus_float_t *)malloc(size * sizeof(mus_float_t));
	      mus_copy_floats(fft_phases, &(fp->phases[losamp]), size);
	      free_phases = true;
	    }
	}

      /* get fft graph points */
      if ((!(cp->fft_log_magnitude)) && 
	  (!(cp->fft_log_frequency)))
	{
	  if (losamp == 0)
	    {
	      for (i = 0, x = 0.0; i < hisamp; i++, x += incr)
		{
		  mus_float_t scaled_data;
		  scaled_data = data[i] * scale;
		  set_grf_point(local_grf_x(x, fap), i, local_grf_y(scaled_data, fap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i, scaled_data);
		}
	    }
	  else
	    {
	      for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
		{
		  mus_float_t scaled_data;
		  scaled_data = data[i] * scale;
		  set_grf_point(local_grf_x(x, fap), 
				i - losamp, 
				local_grf_y(scaled_data, fap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i - losamp, scaled_data);
		}
	    }
	}
      else
	{
	  /* either log freq or log magnitude or both */
	  for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
	    {
	      mus_float_t scaled_data;
	      scaled_data = data[i] * scale;

	      if (cp->fft_log_frequency) 
		{
		  if (x > 1.0) curlx = log(x); else curlx = 0.0;
		  logx = local_grf_x(fap->x0 + lscale * (curlx - minlx), fap);
		}
	      else logx = local_grf_x(x, fap);

	      if (cp->fft_log_magnitude) 
		logy = local_grf_y(in_dB(cp->min_dB, cp->lin_dB, scaled_data), fap); 
	      else logy = local_grf_y(scaled_data, fap);

	      set_grf_point(logx, i - losamp, logy);
	      if (cp->printing) 
		{
		  if (cp->fft_log_frequency) 
		    pslogx = fap->x0 + lscale * (curlx - minlx);
		  else pslogx = x;
		  if (cp->fft_log_magnitude) 
		    pslogy = in_dB(cp->min_dB, cp->lin_dB, scaled_data); 
		  else pslogy = scaled_data;
		  ps_set_grf_point(pslogx, i - losamp, pslogy);
		}
	    }
	}

      lines_to_draw = i - losamp;
    }
  else
    {
      mus_long_t j;
      mus_float_t xf, ina, ymax;
      if ((cp->fft_with_phases) &&
	  (fp->phases) &&
	  (cp->transform_type == FOURIER))
	{
	  fft_phases = (mus_float_t *)malloc(POINT_BUFFER_SIZE * sizeof(mus_float_t));
	  free_phases = true;
	}

      j = 0;      /* graph point counter */
      i = losamp;
      if (losamp == 0) 
	x = 0.0; 
      else x = fap->x0;
      xf = 0.0;     /* samples per pixel counter */
      ymax = MAX_INIT;

      if ((!(cp->fft_log_magnitude)) && 
	  (!(cp->fft_log_frequency)))
	{
	  while (i < hisamp)
	    {
	      ina = data[i];
	      if (ina > ymax) 
		{
		  ymax = ina;
		  if (fft_phases) 
		    fft_phases[j] = fp->phases[i];
		}
	      xf += 1.0;
	      i++;
	      if (xf > samples_per_pixel)
		{
		  mus_float_t scaled_data;
		  scaled_data = ymax * scale;

		  set_grf_point(local_grf_x(x, fap), j, local_grf_y(scaled_data, fap));
		  x += (incr * samples_per_pixel); 

		  if (cp->printing) 
		    ps_set_grf_point(x, j, scaled_data);

		  j++;
		  xf -= samples_per_pixel;
		  ymax = MAX_INIT;
		}
	    }
	}
      else
	{
	  while (i < hisamp)
	    {
	      ina = data[i];
	      if (ina > ymax) 
		{
		  ymax = ina;
		  if (fft_phases) 
		    fft_phases[j] = fp->phases[i];
		}

	      xf += 1.0;
	      i++;
	      if (xf > samples_per_pixel)
		{
		  mus_float_t scaled_data;
		  scaled_data = ymax * scale;

		  if (cp->fft_log_frequency) 
		    {
		      if (x > 1.0) curlx = log(x); else curlx = 0.0;
		      logx = local_grf_x(fap->x0 + lscale * (curlx - minlx), fap);
		    }
		  else logx = local_grf_x(x, fap);

		  if (cp->fft_log_magnitude) 
		    logy = local_grf_y(in_dB(cp->min_dB, cp->lin_dB, scaled_data), fap); 
		  else logy = local_grf_y(scaled_data, fap);

		  set_grf_point(logx, j, logy);

		  if (cp->printing) 
		    {
		      if (cp->fft_log_frequency) 
			pslogx = fap->x0 + lscale * (curlx - minlx);
		      else pslogx = x;
		      if (cp->fft_log_magnitude) 
			pslogy = in_dB(cp->min_dB, cp->lin_dB, scaled_data); 
		      else pslogy = scaled_data;
		      ps_set_grf_point(pslogx, j, pslogy);
		    }

		  x += (incr * samples_per_pixel);
		  j++;
		  xf -= samples_per_pixel;
		  ymax = MAX_INIT;
		}
	    }
	}
      lines_to_draw = j;
    }
    
  if ((cp->fft_with_phases) &&
      (fft_phases) &&
      (cp->transform_type == FOURIER))
    {
      int k;
      int *colors;
      color_t default_color;
      
      if (cp == selected_channel())
	default_color = ss->selected_data_color;
      else default_color = ss->data_color;
      
      /* if value is close to 0, use [selected-]data-color, else use fft_phases[i] */
      allocate_color_map(PHASES_COLORMAP);
      colors = (int *)malloc(lines_to_draw * sizeof(int));
      
      for (k = 0; k < lines_to_draw; k++)
	{
	  if (fft_phases[k] < 0.0) fft_phases[k] += 2.0 * M_PI;                    /* we want 0..2pi, not -pi..pi */
	  colors[k] = (int)((fft_phases[k] * color_map_size(ss)) / (2.0 * M_PI));
	}
      
      draw_colored_lines(cp, ax, get_grf_points(), lines_to_draw, colors, fap->y_axis_y0, default_color);
      
      free(colors);
      if (free_phases)
	free(fft_phases);
    }
  else 
    {
      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	{
	  color_t old_color;
	  old_color = get_foreground_color(ax);
	  set_foreground_color(ax, cp->combined_data_color);
	  /* draw_grf_points(cp->dot_size, ax, lines_to_draw, fap, 0.0, cp->transform_graph_style); */
	  /*   ^ this messes up dB graph if line-style is lollipops or filled -- see also below */
	  draw_grf_points(cp->dot_size, ax, lines_to_draw, fap, fap->y0, cp->transform_graph_style);
	  set_foreground_color(ax, old_color);
	}
      else draw_grf_points(cp->dot_size, ax, lines_to_draw, fap, fap->y0, cp->transform_graph_style);
    }
  
  if (cp->printing) 
    ps_draw_grf_points(fap, lines_to_draw, 0.0, cp->transform_graph_style, cp->dot_size);

  if (cp->fft_log_frequency)
    data[losamp] = saved_data;

  if (cp->show_transform_peaks) 
    {
      if (cp->transform_type == FOURIER)
	display_peaks(cp, fap, data, 
		      snd_srate(sp) * cp->spectrum_start / 2,
		      snd_srate(sp) * cp->spectrum_end / 2, 
		      losamp, hisamp, 
		      samples_per_pixel, true, scale);
      else display_peaks(cp, fap, data, 
			 fp->current_size * cp->spectrum_start, 
			 fp->current_size * cp->spectrum_end, 
			 losamp, hisamp, 
			 samples_per_pixel, true, 0.0);
    }

  if (cp->selection_transform_size != 0) 
    display_selection_transform_size(cp, fap, ax);

  if (sp->channel_style == CHANNELS_SUPERIMPOSED)
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color();
    }

  if (with_hook == WITH_HOOK) after_transform(cp, scale);
}


static int skew_color(mus_float_t x)
{
  mus_float_t base, val;
  int pos;
  if (x < color_cutoff(ss)) return(NO_COLOR);
  if (color_inverted(ss))   
    val = 1.0 - x;
  else val = x;
  base = color_scale(ss);
  if ((base > 0.0) && (base != 1.0))
    val = (pow(base, val) - 1.0) / (base - 1.0);
  pos = (int)(val * color_map_size(ss));
  if (pos > color_map_size(ss)) return(color_map_size(ss) - 1);
  if (pos > 0)
    return(pos - 1);
  return(0);
}


void make_sonogram(chan_info *cp)
{ 
  #define SAVE_FFT_SIZE 4096
  sono_info *si;
  static uint32_t *sono_js = NULL;
  static uint32_t sono_js_size = 0;

  if (chan_fft_in_progress(cp)) return;
  si = cp->sonogram_data;

  if ((si) && (si->scale > 0.0))
    {
      int i, slice, fwidth, fheight, j, bins;
      int rectw, recth, cmap_size;
      axis_info *fap;
      mus_float_t xf, xfincr, yf, yfincr, frectw, frecth, xscl, scl = 1.0;
      mus_float_t *hfdata;
      int *hidata;
      graphics_context *ax;
      mus_float_t minlx = 0.0, lscale = 1.0;

      ax = copy_context(cp);
      fap = cp->fft->axis;

      fwidth = fap->x_axis_x1 - fap->x_axis_x0;        /* these are the corners */
      fheight = fap->y_axis_y0 - fap->y_axis_y1;
      if ((fwidth == 0) || (fheight == 0)) return;

      bins = (int)(si->target_bins * cp->spectrum_end);
      if (bins == 0) return;

#if USE_MOTIF
      if (cp->fft_pix)                            /* Motif None = 0 */
	{
	  if ((cp->fft_changed == FFT_UNCHANGED) &&
	      (cp->fft_pix_ready) &&
	      (cp->fft_pix_cutoff == cp->spectrum_end) &&
	      ((int)(cp->fft_pix_width) == fwidth) &&
	      ((int)(cp->fft_pix_height) == fheight) &&
	      (cp->fft_pix_x0 == fap->x_axis_x0) &&
	      (cp->fft_pix_y0 == fap->y_axis_y1)) /* X is upside down */
	    {
	      if (restore_fft_pix(cp, ax))             /* copy pix into drawing area and return */
		return;
	    }
	  else
	    {
	      free_fft_pix(cp);
	    }
	  cp->fft_pix_ready = false;
	}
#endif

      if (sono_js_size != (uint32_t)color_map_size(ss))
	{
	  if (sono_js) free(sono_js);
	  sono_js_size = (uint32_t)color_map_size(ss);
	  sono_js = (uint32_t *)calloc(sono_js_size, sizeof(uint32_t));
	}
      if (cp->printing) ps_allocate_grf_points();
      allocate_sono_rects(si->total_bins);
      allocate_color_map(color_map(ss));

      scl = si->scale; 
      frectw = (mus_float_t)fwidth / (mus_float_t)(si->target_slices);
      frecth = (mus_float_t)fheight / (mus_float_t)bins;
      xscl = (mus_float_t)(fap->x1 - fap->x0) / (mus_float_t)(si->target_slices);
      rectw = (int)(ceil(frectw));
      recth = (int)(ceil(frecth));
      if (rectw == 0) rectw = 1;
      if (recth == 0) recth = 1;

      hfdata = (mus_float_t *)malloc((bins + 1) * sizeof(mus_float_t));
      hidata = (int *)malloc((bins + 1) * sizeof(int));

      if (cp->transform_type == FOURIER)
	{
	  if (cp->fft_log_frequency)
	    {
	      mus_float_t maxlx, fap_range, log_range;
	      fap_range = fap->y1 - fap->y0;
	      if (fap->y0 > 1.0) minlx = log(fap->y0); else minlx = 0.0;
	      maxlx = log(fap->y1);
	      log_range = (maxlx - minlx);
	      lscale = fap_range / log_range;
	    }
	  yfincr = cp->spectrum_end * (mus_float_t)snd_srate(cp->sound) * 0.5 / (mus_float_t)bins;
	}
      else yfincr = 1.0;

      if (cp->fft_log_frequency)
	{
	  for (yf = 0.0, i = 0; i <= bins; i++, yf += yfincr)
	    {
	      mus_float_t curlx;
	      if (yf > 1.0) curlx = log(yf); else curlx = 0.0;
	      hfdata[i] = fap->y0 + lscale * (curlx - minlx);
	      hidata[i] = local_grf_y(hfdata[i], fap);
	    }
	}
      else
	{
	  for (yf = 0.0, i = 0; i <= bins; i++, yf += yfincr)
	    {
	      hfdata[i] = yf;
	      hidata[i] = local_grf_y(yf, fap);
	    }
	}

      xfincr = ((mus_float_t)fwidth / (mus_float_t)(si->target_slices));
      xf = 2 + fap->x_axis_x0;
      ss->stopped_explicitly = false;
      cmap_size = color_map_size(ss) - 4;

      for (slice = 0; slice < si->active_slices; slice++, xf += xfincr)
	{
	  mus_float_t *fdata;

	  memset((void *)sono_js, 0, color_map_size(ss) * sizeof(uint32_t));
	  fdata = si->data[slice];

	  for (i = 0; i < bins; i++)
	    {
	      mus_float_t binval;
	      /* above is fdata[i-1], left is si->data[slice-1][i] */
	      binval = fdata[i] / scl;
	      if (cp->fft_log_magnitude) binval = 1.0 - (in_dB(cp->min_dB, cp->lin_dB, binval)) / cp->min_dB;
	      j = skew_color(binval);
	      if (j != NO_COLOR)
		{
		  if (cp->fft_log_frequency)
		    set_sono_rectangle(sono_js[j], j, (int)xf, hidata[i + 1], rectw, (int)(hidata[i] - hidata[i + 1]));
		  else set_sono_rectangle(sono_js[j], j, (int)xf, hidata[i + 1], rectw, recth);
		  if (cp->printing)
		    {
		      if (cp->fft_log_frequency) 
			ps_draw_sono_rectangle(fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, hidata[i] - hidata[i + 1]);
		      else ps_draw_sono_rectangle(fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, -frecth);
		    }
		  sono_js[j]++;
		}
	    }

	  i = 0;
	  while (i <= cmap_size)
	    {
	      if (sono_js[i] != 0) draw_sono_rectangles(ax, i, sono_js[i]);
	      i++;
	      if (sono_js[i] != 0) draw_sono_rectangles(ax, i, sono_js[i]);
	      i++;
	      if (sono_js[i] != 0) draw_sono_rectangles(ax, i, sono_js[i]);
	      i++;
	      if (sono_js[i] != 0) draw_sono_rectangles(ax, i, sono_js[i]);
	      i++;
	    }
	  for (; i < color_map_size(ss); i++)
	    if (sono_js[i] != 0) draw_sono_rectangles(ax, i, sono_js[i]);

	}

#if USE_MOTIF
      /* if size was wrong, we've already released pix above */
      if ((bins >= SAVE_FFT_SIZE) ||      /* transform size of twice that (8192) */
	  ((enved_dialog_is_active()) &&  /*   or we want to see the sonogram as the enved background */
	   (enved_target(ss) == ENVED_SPECTRUM) &&
	   (enved_with_wave(ss))))
	save_fft_pix(cp, ax, fwidth, fheight, fap->x_axis_x0, fap->y_axis_y1);
#endif

      if (cp->printing) ps_reset_color();
      free(hfdata);
      free(hidata);
      if (cp->hookable == WITH_HOOK) after_transform(cp, 1.0 / scl);
    }
}


static void rotate_matrix(mus_float_t xangle, mus_float_t yangle, mus_float_t zangle, mus_float_t xscl, mus_float_t yscl, mus_float_t zscl, mus_float_t *mat)
{
  /* return rotation matrix for rotation through angles xangle, yangle, then zangle with scaling by xscl, yscl, zscl */
  mus_float_t sinx, siny, sinz, cosx, cosy, cosz, deg;
  mus_float_t x, y, z;
  deg = 2.0 * M_PI / 360.0;
  x = xangle * deg;
  y = yangle * deg;
  z = zangle * deg;
  sinx = sin(x);
  siny = sin(y);
  sinz = sin(z);
  cosx = cos(x);
  cosy = cos(y);
  cosz = cos(z);
  mat[0] = cosy * cosz * xscl;
  mat[1] = (sinx * siny * cosz - cosx * sinz) * yscl;
  mat[2] = (cosx * siny * cosz + sinx * sinz) * zscl;
  mat[3] = cosy * sinz * xscl;
  mat[4] = (sinx * siny * sinz + cosx * cosz) * yscl;
  mat[5] = (cosx * siny * sinz - sinx * cosz) * zscl;
  mat[6] = -siny * xscl;
  mat[7] = sinx * cosy * yscl;
  mat[8] = cosx * cosy * zscl;
}


static void rotate(mus_float_t *xyz, mus_float_t *mat)
{ /* use rotation/scaling matrix set up by rotate_matrix to rotate and scale the vector xyz */
  mus_float_t x, y, z;
  x = xyz[0];
  y = xyz[1];
  z = xyz[2];
  xyz[0] = mat[0] * x + mat[1] * y + mat[2] * z;
  xyz[1] = mat[3] * x + mat[4] * y + mat[5] * z;
  xyz[2] = mat[6] * x + mat[7] * y + mat[8] * z;
}


#if HAVE_GL

void reset_spectro(void)
{
  /* only used in orientation dialog button and Enter key binding */
  set_spectrum_end(DEFAULT_SPECTRUM_END);
  set_spectrum_start(DEFAULT_SPECTRUM_START);
  set_spectro_hop(DEFAULT_SPECTRO_HOP);
  set_spectro_x_angle((with_gl(ss)) ? DEFAULT_SPECTRO_X_ANGLE : 90.0);
  set_spectro_y_angle((with_gl(ss)) ? DEFAULT_SPECTRO_Y_ANGLE : 0.0);
  set_spectro_z_angle((with_gl(ss)) ? DEFAULT_SPECTRO_Z_ANGLE : 358.0);
  set_spectro_x_scale((with_gl(ss)) ? DEFAULT_SPECTRO_X_SCALE : 1.0);
  set_spectro_y_scale((with_gl(ss)) ? DEFAULT_SPECTRO_Y_SCALE : 1.0);
  set_spectro_z_scale((with_gl(ss)) ? DEFAULT_SPECTRO_Z_SCALE : 0.1);
}


#if HAVE_GLU
#include <GL/glu.h>
#endif

static GLdouble unproject_to_x(int x, int y)
{
#if HAVE_GLU
  /* taken from GL doc p152 */
  GLint viewport[4];
  GLdouble mv[16], proj[16];
  GLint realy;
  GLdouble wx = 0.0, wy, wz;
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, mv);
  glGetDoublev(GL_PROJECTION_MATRIX, proj);
  realy = viewport[3] - (GLint)y - 1;
  gluUnProject((GLdouble)x, (GLdouble)realy, 0.0, mv, proj, viewport, &wx, &wy, &wz);
  return(wx);
#else
  return(0.0);
#endif
}


static GLdouble unproject_to_y(int x, int y)
{
#if HAVE_GLU
  /* taken from GL doc p152 */
  GLint viewport[4];
  GLdouble mv[16], proj[16];
  GLint realy;
  GLdouble wx, wy = 0.0, wz;
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, mv);
  glGetDoublev(GL_PROJECTION_MATRIX, proj);
  realy = viewport[3] - (GLint)y - 1;
  gluUnProject((GLdouble)x, (GLdouble)realy, 0.0, mv, proj, viewport, &wx, &wy, &wz);
  return(wy);
#else
  return(0.0);
#endif
}

#else

void reset_spectro(void)
{
  /* only used in orientation dialog button and Enter key binding */
  set_spectrum_end(DEFAULT_SPECTRUM_END);
  set_spectrum_start(DEFAULT_SPECTRUM_START);
  set_spectro_hop(DEFAULT_SPECTRO_HOP);
  set_spectro_x_angle(DEFAULT_SPECTRO_X_ANGLE);
  set_spectro_y_angle(DEFAULT_SPECTRO_Y_ANGLE);
  set_spectro_z_angle(DEFAULT_SPECTRO_Z_ANGLE);
  set_spectro_x_scale(DEFAULT_SPECTRO_X_SCALE);
  set_spectro_y_scale(DEFAULT_SPECTRO_Y_SCALE);
  set_spectro_z_scale(DEFAULT_SPECTRO_Z_SCALE);
}
#endif


static void display_channel_lisp_data(chan_info *cp);
static void make_axes(chan_info *cp, axis_info *ap, x_axis_style_t x_style, bool erase_first, with_grid_t grid, log_axis_t log_axes, show_axes_t axes);

#define DONT_CLEAR_GRAPH false
#define CLEAR_GRAPH true

#if HAVE_GL
#if USE_MOTIF
static void set_up_for_gl(chan_info *cp)
{
  glXMakeCurrent(main_display(ss), XtWindow(channel_graph(cp)), ss->cx);
}


static void gl_display(chan_info *cp)
{
  if (ss->gl_has_double_buffer)
    glXSwapBuffers(main_display(ss), XtWindow(channel_graph(cp)));
  else glFlush();
}
#else
static void set_up_for_gl(chan_info *cp)
{
  /* probably gdk_gl_context_make_current */
}
static void gl_display(chan_info *cp)
{
  glFlush();
}
#endif


#define GL_COLOR_SET(R, G, B) glColor3us(R, G, B)

static void gl_spectrogram(sono_info *si, int gl_fft_list, mus_float_t cutoff, bool use_dB, mus_float_t min_dB,
			   rgb_t br, rgb_t bg, rgb_t bb)
{
  mus_float_t lin_dB = 0.0;
  mus_float_t xincr, yincr, y0, x1;
  int bins = 0, slice, i, j;
  float inv_scl;
  int **js = NULL;

  inv_scl = 1.0 / si->scale;
  if (use_dB) lin_dB = pow(10.0, min_dB * 0.05);

  glNewList((GLuint)gl_fft_list, GL_COMPILE);

  bins = (int)(si->target_bins * cutoff);
  if (bins <= 0) bins = 1;

  js = (int **)calloc(si->active_slices, sizeof(int *));
  for (i = 0; i < si->active_slices; i++)
    {
      js[i] = (int *)calloc(bins, sizeof(int));
      if (use_dB) 
	{
	  for (j = 0; j < bins; j++)
	    js[i][j] = skew_color(1.0 - (in_dB(min_dB, lin_dB, si->data[i][j] * inv_scl)) / min_dB);
	}
      else
	{
	  for (j = 0; j < bins; j++)
	    js[i][j] = skew_color(si->data[i][j] * inv_scl); /* can be NO_COLOR (-1) */
	}
    }
  xincr = 1.0 / (double)(si->active_slices);
  yincr = 1.0 / (double)bins;
  x1 = -0.5;

  for (slice = 0; slice < si->active_slices - 1; slice++)
    {
      mus_float_t x0, y1;
      x0 = x1;
      x1 += xincr;
      y1 = -0.5;

      for (i = 0; i < bins - 1; i++)
	{
	  rgb_t r, g, b;
	  mus_float_t val00, val01, val11, val10;

	  glBegin(GL_POLYGON);

	  y0 = y1;
	  y1 += yincr;

	  val00 = si->data[slice][i] * inv_scl;
	  val01 = si->data[slice][i + 1] * inv_scl;
	  val10 = si->data[slice + 1][i] * inv_scl;
	  val11 = si->data[slice + 1][i + 1] * inv_scl;

	  if (use_dB) 
	    {
	      val00 = 1.0 - (in_dB(min_dB, lin_dB, val00)) / min_dB;
	      val01 = 1.0 - (in_dB(min_dB, lin_dB, val01)) / min_dB;
	      val10 = 1.0 - (in_dB(min_dB, lin_dB, val10)) / min_dB;
	      val11 = 1.0 - (in_dB(min_dB, lin_dB, val11)) / min_dB;
	    }
	  if (js[slice][i] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice][i], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x0, val00, y0);
	  
	  if (js[slice + 1][i] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice + 1][i], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x1, val10, y0);
	  
	  if (js[slice + 1][i + 1] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice + 1][i + 1], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x1, val11, y1);
	  
	  if (js[slice][i + 1] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice][i + 1], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x0, val01, y1);
	  
	  glEnd();
	}
    }
  for (i = 0; i < si->active_slices; i++) free(js[i]);
  free(js);
  js = NULL;
  glEndList();
}


static bool gl_warned_already = false;

static bool make_gl_spectrogram(chan_info *cp)
{
  sono_info *si;
  axis_info *fap;
  snd_info *sp;
  bool need_relist = false;
  rgb_t br = RGB_MAX, bg = RGB_MAX, bb = RGB_MAX;

#if USE_MOTIF
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
#endif
  
  si = cp->sonogram_data;
  sp = cp->sound;

  /* experiments with lighting were a bust -- does not improve things (ditto fog, translucency, grid) */
  
  fap = cp->fft->axis; 
  if ((cp->printing) &&
      (!gl_warned_already))
    {
      gl_warned_already = true;
#if WITH_GL2PS
      snd_warning("use " S_gl_graph_to_ps " to print openGL graphs");
#else
      snd_warning("we need openGL and gl2ps to print openGL graphs");
#endif
    }
  set_up_for_gl(cp);
  if (cp->gl_fft_list == NO_LIST) 
    {
      need_relist = true;
      cp->gl_fft_list = (int)glGenLists(1);
    }
  else
    {
      if (cp->fft_changed == FFT_CHANGED)
	{
	  glDeleteLists((GLuint)(cp->gl_fft_list), 1);
	  cp->gl_fft_list = (int)glGenLists(1);
	  need_relist = true;
	}
    }

  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glClearDepth(1.0);

#if USE_MOTIF
  /* get the background color */
  dpy = XtDisplay(main_shell(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  if (cp == selected_channel())
    tmp_color.pixel = ss->selected_graph_color;
  else tmp_color.pixel = ss->graph_color;
  XQueryColor(dpy, cmap, &tmp_color);
  br = tmp_color.red;
  bg = tmp_color.green;
  bb = tmp_color.blue;
  glClearColor(rgb_to_float(tmp_color.red),
	       rgb_to_float(tmp_color.green),
	       rgb_to_float(tmp_color.blue),
	       0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
#endif
  
  if (need_relist)
    gl_spectrogram(si, cp->gl_fft_list, cp->spectrum_end, cp->fft_log_magnitude, cp->min_dB, br, bg, bb);

  glViewport(fap->graph_x0, 0, fap->width, fap->height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  /* glOrtho(-1.0, 1.0, -1.0, 1.0, 1.0, -1.0); */ /* this appears to be the default */
  glRotatef(cp->spectro_x_angle, 1.0, 0.0, 0.0);
  glRotatef(cp->spectro_y_angle, 0.0, 1.0, 0.0);
  glRotatef(cp->spectro_z_angle, 0.0, 0.0, 1.0);
  glScalef(cp->spectro_x_scale, cp->spectro_y_scale, cp->spectro_z_scale);

  glCallList((GLuint)(cp->gl_fft_list));

  fap->use_gl = true;

  {
    double frq0, frq1;
    frq0 = snd_srate(sp) * cp->spectrum_start / 2.0;
    frq1 = snd_srate(sp) * cp->spectrum_end / 2.0;
    make_axis_info(cp,
		   cp->axis->x0, cp->axis->x1,
		   frq0, frq1,
		   "time",
		   cp->axis->x0, cp->axis->x1,
		   frq0, frq1,
		   fap);
  }

  make_axes(cp, fap, X_AXIS_IN_SECONDS, DONT_CLEAR_GRAPH, NO_GRID, WITH_LINEAR_AXES, cp->show_axes);
  /* there's no support for GL log y axis labelling in snd-axis.c (line 1244), or in the axis bounds
   *   choice here (see snd-fft.c -- need to check spectrum_start etc), or in gl_spectrogram above, 
   *   so ignore the log_freq setting -- since log freq really doesn't add much (unlike dB for example),
   *   the extra work seems wasted.
   */
  fap->use_gl = false;
  gl_display(cp);
  
  /* a kludge to get the normal graph drawn (again...) */
  if (cp->graph_time_on)
    display_channel_time_data(cp); 
  if (cp->graph_lisp_on)
    display_channel_lisp_data(cp); 

#if USE_MOTIF
  return(XtAppPending(main_app(ss)) == 0); /* return true if there are no pending events to force current buffer to be displayed */
#else
  return(false); /* make the compiler happy */
#endif
}
#endif
/* HAVE_GL */


static bool make_spectrogram(chan_info *cp)
{
  sono_info *si;
  axis_info *fap;
  graphics_context *ax;
  mus_float_t *fdata;
  mus_float_t matrix[9];
  mus_float_t xyz[3];
  mus_float_t xoff, yoff, xincr, yincr, x0, y0, binval, scl = 1.0;
  mus_float_t fwidth, fheight, zscl, yval, xval;
  int bins = 0, slice, i, j, xx = 0, yy = 0;
  bool old_with_gl = false;

  mus_float_t minlx = 0.0, lscale = 0.0, fap_incr = 0.0;

  if (chan_fft_in_progress(cp)) return(false);
  si = cp->sonogram_data;
  if ((!si) || (si->scale <= 0.0)) return(false);

#if HAVE_GL
  if (((cp->sound->nchans == 1) || 
       (cp->sound->channel_style == CHANNELS_SEPARATE)) &&
      (color_map(ss) != BLACK_AND_WHITE_COLORMAP) &&
      (with_gl(ss)))
    return(make_gl_spectrogram(cp));
#endif
  
  old_with_gl = with_gl(ss);
  if (old_with_gl) set_with_gl(false, false); /* needed to fixup spectro angles/scales etc */
  if (cp->printing) ps_allocate_grf_points();

  scl = si->scale;                     /* unnormalized fft doesn't make much sense here (just washes out the graph) */
  fap = cp->fft->axis;
  bins = (int)(si->target_bins * cp->spectrum_end);
  fwidth = (fap->x_axis_x1 - fap->x_axis_x0);
  fheight = (fap->y_axis_y1 - fap->y_axis_y0); /* negative! */
  xincr = fwidth / (mus_float_t)bins;
  yincr = fheight / (mus_float_t)si->active_slices;
  x0 = (fap->x_axis_x0 + fap->x_axis_x1) * 0.5;
  y0 = (fap->y_axis_y0 + fap->y_axis_y1) * 0.5;

  if (!(cp->fft_log_magnitude))
    zscl = -(cp->spectro_z_scale * fheight / scl);
  else zscl = -(cp->spectro_z_scale * fheight);
  rotate_matrix(cp->spectro_x_angle, cp->spectro_y_angle, cp->spectro_z_angle,
		cp->spectro_x_scale, cp->spectro_y_scale, zscl,
		matrix);

  if (cp->fft_log_frequency) 
    {
      fap_incr = (fap->x1 - fap->x0) / bins;
      if (fap->x0 > 1.0) minlx = log(fap->x0); else minlx = 0.0;
      lscale = (fap->x_axis_x1 - fap->x_axis_x0) / (log(fap->x1) - minlx);
    }

  ax = copy_context(cp);

  if (color_map(ss) != BLACK_AND_WHITE_COLORMAP)
    allocate_color_map(color_map(ss));
  ss->stopped_explicitly = false;

  for (slice = 0, xoff = fap->x_axis_x0, yoff = fap->y_axis_y0; 
       slice < si->active_slices; 
       slice++, yoff += yincr)
    {
      mus_float_t x, y;
      fdata = si->data[slice];
      x = xoff;
      y = yoff;
      
      if (color_map(ss) != BLACK_AND_WHITE_COLORMAP)
	{
	  xyz[0] = x - x0; 
	  xyz[1] = y - y0; 
	  xyz[2] = fdata[0]; 
	  rotate(xyz, matrix);
	  xx = (int)(xyz[0] + x0); 
	  yy = (int)(xyz[1] + xyz[2] + y0);
	}
      
      for (i = 0; i < bins; i++, x += xincr)
	{
	  mus_float_t logx;
	  if (cp->fft_log_frequency) 
	    {
	      mus_float_t fx, curlx;
	      fx = fap->x0 +  fap_incr * i;
	      if (fx > 1.0) curlx = log(fx); else curlx = 0.0;
	      logx = fap->x_axis_x0 + (curlx - minlx) * lscale;
	    }
	  else logx = x;

	  xyz[0] = logx - x0; 
	  xyz[1] = y - y0;
	  binval = fdata[i] / scl;
	  if (!(cp->fft_log_magnitude)) 		  
	    xyz[2] = fdata[i];
	  else 
	    {
	      xyz[2] = 1.0 - (in_dB(cp->min_dB, cp->lin_dB, binval)) / cp->min_dB; 
	      binval = xyz[2];
	    }
	  rotate(xyz, matrix);
	  yval = xyz[1] + xyz[2];
	  xval = xyz[0];
	  
	  if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	    {
	      set_grf_point((int)(xval + x0), i, (int)(yval + y0));
	      if (cp->printing) 
		ps_set_grf_point(ungrf_x(fap, (int)(xval + x0)), i, 
				 ungrf_y(fap, (int)(yval + y0)));
	    }
	  else
	    {
	      j = skew_color(binval);
	      if (j != NO_COLOR)
		{
		  draw_spectro_line(ax, j, xx, yy, 
				    (int)(xval + x0), 
				    (int)(yval + y0));
		  if (cp->printing) 
		    ps_draw_spectro_line(fap, j, xx, yy, xval + x0, yval + y0);
		}
	      xx = (int)(xval + x0); 
	      yy = (int)(yval + y0);
	    }
	}
      
      if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	draw_grf_points(cp->dot_size, ax, bins, fap, 0.0, cp->transform_graph_style);
      
      if ((cp->printing) && (color_map(ss) == BLACK_AND_WHITE_COLORMAP))
	ps_draw_grf_points(fap, bins, 0.0, cp->transform_graph_style, cp->dot_size);
    }

  if ((cp->printing) && (color_map(ss) != BLACK_AND_WHITE_COLORMAP)) ps_reset_color();
  if (cp->hookable == WITH_HOOK) after_transform(cp, 1.0 / scl);
  if (old_with_gl) set_with_gl(true, false);
  return(false);
}


/* ---------------------------------------- wavograms ---------------------------------------- */

/* (with-sound () (do ((j 0 (+ j 1))) ((= j 500)) (do ((i 0 (+ i 1))) ((= i 100)) (outa (+ (* j 100) i) (* i j .002 .01)))))
 *
 * xdeg=45, ydeg=0, zdeg=0 is the "flat" display
 */

typedef struct wavogram_state {
  int hop, trace, width, height, graph_x0, cmap, cmap_size, edit_ctr;
  bool inverted;
  mus_float_t scale, cutoff;
  mus_long_t losamp;
  mus_float_t x_scale, y_scale, z_scale, z_angle, x_angle, y_angle; /* only used in non-GL case */
} wavogram_state;

   
static int make_wavogram(chan_info *cp)
{
  snd_info *sp;
  mus_float_t xoff, x0, y0, xincr;
  mus_float_t width, height, zscl, yval, xval, binval;
  int i, j, points = 0, yincr, yoff;
  mus_float_t matrix[9];
  mus_float_t xyz[3];
  axis_info *ap;
  graphics_context *ax;
  snd_fd *sf = NULL;
  wavogram_state *lw;
#if USE_MOTIF
  bool need_redraw = true;
#endif
#if HAVE_GL || USE_MOTIF
  bool use_gl = false;
  bool need_new_list = true;
#endif

  sp = cp->sound;
  ap = cp->axis;
  if (sp) ap->losamp = (mus_long_t)(ap->x0 * snd_srate(sp));

#if HAVE_GL
  if (((sp->nchans == 1) || (sp->channel_style == CHANNELS_SEPARATE)) &&
      (color_map(ss) != BLACK_AND_WHITE_COLORMAP) &&
      (with_gl(ss)))
    use_gl = true;
#endif

  lw = cp->last_wavogram;
  if (lw)
    {
#if HAVE_GL || USE_MOTIF
      need_new_list = (!((lw->hop == wavo_hop(ss)) &&
			 (lw->trace == wavo_trace(ss)) &&
			 (lw->losamp == ap->losamp) &&
			 (lw->width == ap->width) &&
			 (lw->height == ap->height) &&
			 (lw->graph_x0 == ap->graph_x0) && 
			 (lw->cmap == color_map(ss)) &&
			 (lw->cmap_size == color_map_size(ss)) &&
			 (lw->edit_ctr == cp->edit_ctr) &&
			 (lw->inverted == color_inverted(ss)) &&
			 (lw->scale == color_scale(ss)) &&
			 (lw->cutoff == color_cutoff(ss))));
#endif
#if HAVE_GL
      if (use_gl)
	{
	  if (cp->gl_wavo_list == NO_LIST)
	    need_new_list = true;
	}
#endif
#if USE_MOTIF
      need_redraw = (!((lw->x_scale == spectro_x_scale(ss)) &&
		       (lw->y_scale == spectro_y_scale(ss)) &&
		       (lw->z_scale == spectro_z_scale(ss)) &&
		       (lw->x_angle == spectro_x_angle(ss)) &&
		       (lw->y_angle == spectro_y_angle(ss)) &&
		       (lw->z_angle == spectro_z_angle(ss))));
#endif
    }

#if USE_MOTIF
  if ((!use_gl) && (!need_redraw) && (!need_new_list))
    return(-1); /* not 0 here because I think that would cancel fft graph */
#endif

  if (cp->last_wavogram) free(cp->last_wavogram);

  lw = (wavogram_state *)malloc(sizeof(wavogram_state));
  lw->hop = wavo_hop(ss);
  lw->trace = wavo_trace(ss);
  lw->losamp = ap->losamp;
  lw->width = ap->width;
  lw->height = ap->height;
  lw->graph_x0 = ap->graph_x0;
  lw->cmap = color_map(ss);
  lw->cmap_size = color_map_size(ss);
  lw->edit_ctr = cp->edit_ctr;
  lw->inverted = color_inverted(ss);
  lw->scale = color_scale(ss);
  lw->cutoff = color_cutoff(ss);

  lw->x_scale = spectro_x_scale(ss);
  lw->y_scale = spectro_y_scale(ss);
  lw->z_scale = spectro_z_scale(ss);
  lw->x_angle = spectro_x_angle(ss);
  lw->y_angle = spectro_y_angle(ss);
  lw->z_angle = spectro_z_angle(ss);

  cp->last_wavogram = lw;

#if HAVE_GL
  if (use_gl)
    {
      mus_float_t **samps = NULL;
      int **js = NULL;
      float x0, x1, y0, y1; /* x5inc, y5inc; */
      mus_float_t xinc, yinc;
      int lines = 0, len = 0;
      rgb_t *rs = NULL;
      
      if (need_new_list)
	{
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (!sf) return(0);

	  lines = (int)(ap->height / cp->wavo_hop);
	  if (lines == 0) return(0);
	  len = cp->wavo_trace;
	  samps = (mus_float_t **)calloc(lines, sizeof(mus_float_t *));
	  js = (int **)calloc(lines, sizeof(int *));
	  for (i = 0; i < lines; i++)
	    {
	      samps[i] = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	      js[i] = (int *)calloc(len, sizeof(int));
	      for (j = 0; j < len; j++)
		{
		  samps[i][j] = read_sample(sf);
		  js[i][j] = skew_color(fabs(samps[i][j]));
		  if (js[i][j] < 0) js[i][j] = 0;
		}
	    }
	  free_snd_fd(sf);
	}

      set_up_for_gl(cp);

      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LEQUAL); 
      glClearDepth(1.0);
      glClearColor(1.0, 1.0, 1.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glViewport(ap->graph_x0, 0, ap->width, ap->height);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glRotatef(cp->spectro_x_angle, 1.0, 0.0, 0.0);
      glRotatef(cp->spectro_y_angle, 0.0, 1.0, 0.0);
      glRotatef(cp->spectro_z_angle, 0.0, 0.0, 1.0);
      glScalef(cp->spectro_x_scale, cp->spectro_y_scale, cp->spectro_z_scale);

      if (!need_new_list)
	{
	  glCallList((GLuint)(cp->gl_wavo_list));
	  gl_display(cp);
	  return((int)(lw->height / lw->hop));
	}

      if (cp->gl_wavo_list == NO_LIST) 
	cp->gl_wavo_list = (int)glGenLists(1);
      else
	{
	  glDeleteLists((GLuint)(cp->gl_wavo_list), 1);
	  cp->gl_wavo_list = (int)glGenLists(1);
	}

      xinc = 2.0 / (mus_float_t)len;
      yinc = 2.0 / (mus_float_t)lines;
      /* x5inc = 1.25 * xinc; */
      /* y5inc = 1.25 * yinc; */
      
      rs = color_map_reds(color_map(ss));
      
      glNewList((GLuint)(cp->gl_wavo_list), GL_COMPILE);

      if (rs)
	{
	  int len1, lines1;
	  mus_float_t xf;
	  rgb_t *gs, *bs;
	  /* each line is wavo_trace samps, there are (height / wave_hop) of these? */

	  gs = color_map_greens(color_map(ss));
	  bs = color_map_blues(color_map(ss));
	  y1 = -1.0;
	  len1 = len - 1;
	  lines1 = lines - 1;

	  for (j = 0; j < lines1; j++)
	    {
	      mus_float_t yf;
	      x1 = -1.0;
	      y0 = y1;
	      y1 += yinc;
	      yf = y1 + yinc / 2.0;

	      for (i = 0; i < len1; i++)
		{
		  int c;
		  x0 = x1;
		  x1 += xinc;
		  xf = x1 + xinc / 2.0;
		  
		  glBegin(GL_QUADS);
		  c = js[j][i];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(x0, samps[j][i], y0);
		
		  c = js[j + 1][i];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(xf, samps[j+ 1][i], y0);
		
		  c = js[j + 1][i + 1];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(xf, samps[j + 1][i + 1], yf);
		
		  c = js[j][i + 1];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(x0, samps[j][i + 1], y1 + yf);
		  glEnd();
		}
	    }
	}
      else
	{
	  int len1, lines1;
	  y1 = -1.0;
	  len1 = len - 1;
	  lines1 = lines - 1;
	  for (j = 0; j < lines1; j++)
	    {
	      x1 = -1.0;
	      y0 = y1;
	      y1 += yinc;
	      for (i = 0; i < len1; i++)
		{
		  rgb_t r, g, b;
		  x0 = x1;
		  x1 += xinc;

		  glBegin(GL_QUADS);
		  get_current_color(color_map(ss), js[j][i], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x0, samps[j][i], y0);
		
		  get_current_color(color_map(ss), js[j + 1][i], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x1, samps[j+ 1][i], y0);
		
		  get_current_color(color_map(ss), js[j + 1][i + 1], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x1, samps[j + 1][i + 1], y1);
		
		  get_current_color(color_map(ss), js[j][i + 1], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x0, samps[j][i + 1], y1);
		  glEnd();
		}
	    }
	}

      glEndList();
      glCallList((GLuint)(cp->gl_wavo_list));
      gl_display(cp);

      for (i = 0; i < lines; i++) 
	{
	  free(samps[i]);
	  free(js[i]);
	}
      free(samps);
      free(js);
      return(j);
    }
#endif

  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
  if (!sf) return(0);

  /* set up the graph width and so on */
  make_axes_1(ap, cp->x_axis_style, snd_srate(sp), SHOW_NO_AXES, NOT_PRINTING, NO_X_AXIS, NO_GRID, WITH_LINEAR_AXES, 1.0);
  erase_rectangle(cp, ap->ax, ap->graph_x0, ap->y_offset, ap->width, ap->height); 

  if (cp->printing) ps_allocate_grf_points();

  width = (ap->x_axis_x1 - ap->x_axis_x0);
  height = (ap->y_axis_y1 - ap->y_axis_y0); /* negative! */

  xincr = width / (mus_float_t)(cp->wavo_trace);
  yincr = -(cp->wavo_hop);
  if (yincr > 0) yincr = -yincr;
  if (yincr == 0) yincr = -1;

  x0 = (ap->x_axis_x0 + ap->x_axis_x1) * 0.5;
  y0 = (ap->y_axis_y0 + ap->y_axis_y1) * 0.5;

  zscl = -(cp->spectro_z_scale * height);
  rotate_matrix(cp->spectro_x_angle, cp->spectro_y_angle, cp->spectro_z_angle,
		cp->spectro_x_scale, cp->spectro_y_scale, zscl,
		matrix);

  ax = copy_context(cp);
  if (color_map(ss) != BLACK_AND_WHITE_COLORMAP)
    allocate_color_map(color_map(ss));

  for (xoff = ap->x_axis_x0, yoff = ap->y_axis_y0; 
       yoff > ap->y_axis_y1; 
       yoff += yincr)
    {
      mus_float_t x, y, xx, yy;
      xx = -1;
      yy = (int)y0; /* ? */
      x = xoff;
      y = yoff;
      for (i = 0; i < cp->wavo_trace; i++, x += xincr)
	{
	  binval = read_sample(sf);
	  xyz[0] = x - x0; 
	  xyz[1] = y - y0; 
	  xyz[2] = binval;
	  rotate(xyz, matrix);
	  yval = xyz[1] + xyz[2];
	  xval = xyz[0];
	  if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	    {
	      set_grf_point((int)(xval + x0), i, 
			    (int)(yval + y0));
	      if (cp->printing) 
		ps_set_grf_point(ungrf_x(ap, (int)(xval + x0)), i, 
				 ungrf_y(ap, (int)(y0 + yval)));
	    }
	  else
	    {
	      /* for color decision here we need absolute value of data */
	      if (binval < 0.0) binval = -binval;
	      if (xx != -1)
		{
		  j = skew_color(binval);
		  if (j != NO_COLOR)
		    {
		      draw_spectro_line(ax, j, xx, yy, 
					(int)(xval + x0), 
					(int)(yval + y0));
		      if (cp->printing) 
			ps_draw_spectro_line(ap, j, xx, yy, xval + x0, yval + y0);
		    }
		}
	      xx = (int)(xval + x0); 
	      yy = (int)(yval + y0);
	    }
	}
      points += cp->wavo_trace;
      
      if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	{
	  draw_grf_points(cp->dot_size, ax, cp->wavo_trace, ap, 0.0, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_grf_points(ap, cp->wavo_trace, 0.0, cp->time_graph_style, cp->dot_size);
	}
    }
      
  if ((cp->printing) && (color_map(ss) != BLACK_AND_WHITE_COLORMAP))
    ps_reset_color();
  free_snd_fd(sf);
  return(points);
}


axis_info *lisp_info_axis(chan_info *cp) 
{
  return(cp->lisp_info->axis);
}


void free_lisp_info(chan_info *cp)
{
  if (cp)
    {
      lisp_grf *lg;
      lg = cp->lisp_info;
      if (lg)
	{
	  if (lg->axis) 
	    lg->axis = free_axis_info(lg->axis);
	  if (lg->data) 
	    {
	      int i;
	      for (i = 0; i < lg->graphs; i++) 
		if (lg->data[i]) 
		  free(lg->data[i]);
	      free(lg->data);
	    }
	  if (lg->len) 
	    free(lg->len);
	  free(lg);
	}
    }
}


static void make_lisp_graph(chan_info *cp, Xen pixel_list)
{
  snd_info *sp;
  lisp_grf *up;
  /* data can be evenly spaced data or an envelope (up->env_data) */
  axis_info *uap = NULL;
  int i;
  graphics_context *ax;

  sp = cp->sound;
  up = cp->lisp_info;
  if (up) uap = up->axis; else return;
  if ((!uap) || (!uap->graph_active) || (!up->len) || (up->len[0] <= 0)) return;

  if (cp->printing) ps_allocate_grf_points();

  if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
    ax = combined_context(cp); 
  else ax = copy_context(cp);

  if (cp->printing) ps_fg(cp, ax);

  if (up->env_data)
    {
      int x0, y0;
      int grf_len;
      grf_len = up->len[0];
      x0 = local_grf_x(up->data[0][0], uap);
      y0 = local_grf_y(up->data[0][1], uap);
      if (cp->dot_size > 0) 
	draw_dot(ax, x0, y0, cp->dot_size);

      for (i = 2; i < grf_len; i += 2)
	{
	  int x1, y1;
	  x1 = local_grf_x(up->data[0][i], uap);
	  y1 = local_grf_y(up->data[0][i + 1], uap);
	  draw_line(ax, x0, y0, x1, y1);
	  if (cp->dot_size > 0) 
	    draw_dot(ax, x1, y1, cp->dot_size);
	  x0 = x1;
	  y0 = y1;
	}
    }
  else
    {
      int j, graph, pixel_len = -1;
      color_t old_color = 0;
      mus_float_t x, y;
      if (Xen_is_list(pixel_list))
	pixel_len = Xen_list_length(pixel_list);
      if (up->graphs > 1) 
	old_color = get_foreground_color(ax);

      for (graph = 0; graph < up->graphs; graph++)
	{
	  mus_float_t samples_per_pixel;
	  if ((pixel_len <= graph) ||
	      (!foreground_color_ok(Xen_list_ref(pixel_list, graph), ax)))
	    {
	      switch (graph)
		{
		case 0:  break;
		case 1:  set_foreground_color(ax, ss->red);        break;
		case 2:  set_foreground_color(ax, ss->green);      break;
		case 3:  set_foreground_color(ax, ss->light_blue); break;
		case 4:  set_foreground_color(ax, ss->yellow);     break;
		default: set_foreground_color(ax, ss->black);      break;
		}
	    }
	  /* check for up->len[graph] > pixels available and use ymin ymax if needed */
	  samples_per_pixel = (mus_float_t)(up->len[graph]) / (mus_float_t)(uap->x_axis_x1 - uap->x_axis_x0);
	  if (samples_per_pixel < 4.0)
	    {
	      mus_float_t start_x, xinc;
	      int grf_len;
	      grf_len = up->len[graph];
	      start_x = uap->x0;
	      if (grf_len <= 1) 
		xinc = 1.0;
	      else xinc = (uap->x1 - uap->x0) / (mus_float_t)(grf_len - 1);
	      for (i = 0, x = start_x; i < grf_len; i++, x += xinc)
		{
		  y = up->data[graph][i];
		  set_grf_point(local_grf_x(x, uap), i, 
				local_grf_y(y, uap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i, y);
		}
	      draw_grf_points(cp->dot_size, ax, grf_len, uap, 0.0, cp->lisp_graph_style);
	      if (cp->printing) 
		ps_draw_grf_points(uap, grf_len, 0.0, cp->lisp_graph_style, cp->dot_size);
	    }
	  else
	    {
	      int xi;
	      mus_float_t ymin, ymax, pinc, xf;
	      j = 0;
	      i = 0;
	      xf = 0.0;
	      x = 0.0;
	      pinc = samples_per_pixel / (mus_float_t)(up->len[graph]);
	      xi = local_grf_x(0.0, uap);
	      ymin = 32768.0;
	      ymax = -32768.0;
	      while (i < up->len[graph])
		{
		  y = up->data[graph][i];
		  if (y > ymax) ymax = y;
		  if (y < ymin) ymin = y;
		  xf += 1.0;
		  i++;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
				     local_grf_y(ymin, uap), 
				     local_grf_y(ymax, uap));
		      if (cp->printing) 
			{
			  x += pinc; 
			  ps_set_grf_points(x, j, ymin, ymax);
			}
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = 32768.0;
		      ymax = -32768.0;
		    }
		}
	      draw_both_grf_points(cp->dot_size, ax, j, cp->lisp_graph_style);
	      if (cp->printing) 
		ps_draw_both_grf_points(uap, j, cp->lisp_graph_style, cp->dot_size);
	    }
	}
      if (up->graphs > 1) set_foreground_color(ax, old_color);
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	{
	  copy_context(cp); /* reset for axes etc */
	  if (cp->printing) ps_reset_color();
	}
    }
}


static void make_axes(chan_info *cp, axis_info *ap, x_axis_style_t x_style, bool erase_first, with_grid_t grid, log_axis_t log_axes, show_axes_t axes)
{
  snd_info *sp;
  color_t old_color = 0; /* make the compiler happy */

  if (!(ap->ax))
    ap->ax = cp->ax;
  sp = cp->sound;
  setup_graphics_context(cp, ap->ax);

  /* here is where the graph is cleared (see also make_partial_graph and make_wavogram) */
  if (erase_first == CLEAR_GRAPH)
    erase_rectangle(cp, ap->ax, ap->graph_x0, ap->y_offset, ap->width, ap->height); 

  if (ss->axis_color_set)
    {
      old_color = get_foreground_color(ap->ax);
      /* if axis color has been set, use it, else use data color from preceding make_graph call */
      set_foreground_color(ap->ax, ss->axis_color);
    }

  make_axes_1(ap, 
	      x_style, 
	      snd_srate(sp), 
	      axes, 
	      cp->printing,
	      (cp->show_axes != SHOW_NO_AXES) ? WITH_X_AXIS : NO_X_AXIS, /* C++ wants the with_x_axis_t result */
	      grid, 
	      log_axes,
	      cp->grid_density);

  if (ss->axis_color_set)
    set_foreground_color(ap->ax, old_color);
}


static void draw_sonogram_cursor(chan_info *cp);
static void draw_graph_cursor(chan_info *cp);
static void show_smpte_label(chan_info *cp, graphics_context *ax);
static void show_inset_graph(chan_info *cp, graphics_context *cur_ax);


static void display_channel_data_with_size(chan_info *cp, 
					   int width, int height, int offset, 
					   bool just_fft, bool just_lisp, bool just_time,
					   bool use_incoming_cr)
{
  /* this procedure is unnecessarily confusing! */
  snd_info *sp;
  bool with_fft = false, with_lisp = false, with_time = false;
  int displays = 0;
  bool grflsp = false;
  axis_info *ap = NULL;
  axis_info *fap = NULL;
  axis_info *uap = NULL;
  lisp_grf *up = NULL;

  sp = cp->sound;
  
  /* -------- graph-hook -------- */
  if ((cp->hookable == WITH_HOOK) && 
      (!(ss->graph_hook_active)) &&
      (Xen_hook_has_list(graph_hook)))
    {
      Xen res;
      ss->graph_hook_active = true;
      res = run_progn_hook(graph_hook,
			   Xen_list_4(C_int_to_Xen_sound(sp->index),
				      C_int_to_Xen_integer(cp->chan),
				      C_double_to_Xen_real(cp->axis->y0),
				      C_double_to_Xen_real(cp->axis->y1)),
			   S_graph_hook);
      /* (hook-push graph-hook (lambda (a b c d) (snd-print (format #f "~A ~A ~A ~A" a b c d)))) */
      ss->graph_hook_active = false;
      if (Xen_is_true(res)) return;
    }

  ap = cp->axis;
  if (!ap) return;

  /* -------- decide which graphs to draw -------- */
  ap->height = height;
  ap->window_width = width;
  ap->width = width;
  ap->y_offset = offset;
  if (cp->graph_time_on) 
    {
      displays++;
      with_time = true;
    }
  grflsp = ((cp->graph_lisp_on) || (Xen_hook_has_list(lisp_graph_hook)));
  if (grflsp)
    {
      displays++;
      up = cp->lisp_info;
      if (!up)
	{
	  /* this should only happen the first time such a graph is needed */
	  cp->lisp_info = (lisp_grf *)calloc(1, sizeof(lisp_grf));
	  up = cp->lisp_info;
	  up->show_axes = cp->show_axes;
	  up->axis = make_axis_info(cp, 0.0, 1.0, -1.0, 1.0, "dummy axis", 0.0, 1.0, -1.0, 1.0, NULL);
	}
      if (up)
	{
	  uap = up->axis;
	  if (uap)
	    {
	      with_lisp = true;
	      uap->height = height;
	      uap->y_offset = offset;
	      uap->width = width;
	      uap->window_width = width;
	    }
	}
    }

  if (cp->graph_transform_on) 
    {
      fft_info *fp;
      displays++;
      fp = cp->fft;
      if (fp)
	{
	  fap = fp->axis; 
	  if (fap)
	    {
	      with_fft = true;
	      fap->height = height;
	      fap->y_offset = offset;
	      fap->width = width;
	      fap->window_width = width;
	    }
	}
    }

  if (displays == 0)
    {
      clear_window(ap->ax);
      return;
    }

  /* -------- set graph positions and sizes -------- */
  if (cp->graphs_horizontal)
    {
      if (with_time) ap->width = width / displays;
      if (with_fft) fap->width =  width / displays;
      if (with_lisp) uap->width = width / displays;

      /* now the x axis offsets for fap and uap */
      if (with_fft) 
	{
	  if (with_time) 
	    fap->graph_x0 = ap->width;
	  else fap->graph_x0 = 0;
	}
      if (with_lisp) 
	uap->graph_x0 = uap->width * (displays - 1);
    }
  else
    {
      height /= displays;
      if (with_time) ap->height = height;
      if (with_fft) fap->height =  height;
      if (with_lisp) uap->height = height;

      /* now the y axis offsets for fap and uap */
      if (with_fft)
	{
	  if (with_time) fap->y_offset += height; 
	  fap->graph_x0 = 0;
	}
      if (with_lisp) 
	{
	  uap->y_offset += (height * (displays - 1));
	  uap->graph_x0 = 0;
	}
    }

  /* -------- time domain graph -------- */

  if ((!just_fft) && (!just_lisp))
    {
      marks_off(cp);
      channel_set_mix_tags_erased(cp);

      if (with_time)
	{
	  int points;
	  graphics_context *our_ax;

#if USE_GTK
	  our_ax = copy_context(cp);
	  if ((!our_ax->w) || (!our_ax->wn))
	    {
	      /* how can this happen? */
	      our_ax->w = channel_to_widget(cp);
	      our_ax->wn = WIDGET_TO_WINDOW(our_ax->w);
	    }
	  if (!use_incoming_cr)
	    {
#if (GTK_CHECK_VERSION(3, 89, 0))
	      if (!cp->graph_cr) return;
	      ss->cr = cp->graph_cr;
#else
	      ss->cr = make_cairo(our_ax->wn);
#endif
	    }
	  cairo_push_group(ss->cr);
#else	  
	  our_ax = ap->ax;
#endif

	  if (cp->time_graph_type == GRAPH_AS_WAVOGRAM)
	    {
	      if (ap->y_axis_y0 == ap->y_axis_y1) 
		make_axes(cp, ap, cp->x_axis_style, DONT_CLEAR_GRAPH, cp->show_grid, WITH_LINEAR_AXES, cp->show_axes); /* first time needs setup */
	      ap->y0 = ap->x0;
	      ap->y1 = ap->y0 + (mus_float_t)(cp->wavo_trace * (ap->y_axis_y0 - ap->y_axis_y1)) / ((mus_float_t)(cp->wavo_hop) * snd_srate(sp));
	      ap->x1 = ap->x0 + (double)(cp->wavo_trace) / (double)snd_srate(sp);
	      points = make_wavogram(cp);
	    }
	  else
	    {
	      make_axes(cp, ap,
			cp->x_axis_style,
			(((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH),
			cp->show_grid,
			WITH_LINEAR_AXES,
			cp->show_axes);
	      cp->cursor_visible = false;
	      cp->selection_visible = false;

#if (!USE_GTK)
	      points = make_graph(cp);
#endif

	      if ((cp->chan == 0) || 
		  (sp->channel_style != CHANNELS_SUPERIMPOSED))
		{
		  display_selection(cp);
		  copy_context(cp);
		}

#if USE_GTK
	      points = make_graph(cp);
#endif

	      if (cp->show_y_zero) 
		display_y_zero(cp);
	      if ((channel_has_active_mixes(cp))) 
		display_channel_mixes(cp);
#if USE_GTK
	      copy_context(cp);
#else
	      our_ax = copy_context(cp);
#endif
	      if (with_inset_graph(ss))
		show_inset_graph(cp, our_ax);
	      if (with_smpte_label(ss))
		show_smpte_label(cp, our_ax);
	    }

	  if ((sp->channel_style != CHANNELS_SUPERIMPOSED) && 
	      (height > 10))
	    display_channel_id(cp, our_ax, height + offset, sp->nchans);

#if USE_GTK
	  /* here in gtk3 we can get:
	   *    cairo-surface.c:385: _cairo_surface_begin_modification: Assertion `! surface->finished' failed.
	   * followed by a segfault.  
	   *
	   * But I've followed this thing in detail, and nothing unusual is happening.
	   * If I check the surface status via 
	   *  {
	   *     cairo_surface_t *surface;
	   *     surface = cairo_get_target(ss->cr);
	   *     fprintf(stderr, "status: %d ", cairo_surface_status(surface));
	   *  }
	   * if prints 0 (success), and then dies!  I can't trace it in gdb because the goddamn gtk idle
	   *   mechanism gets confused.  I can't ask cairo if the surface is finished.  valgrind is happy. 
	   *   cairo-trace shows nothing unusual.  cairo 1.8.0 is happy.
	   * I am stuck.
	   */

	  cairo_pop_group_to_source(ss->cr);
	  cairo_paint(ss->cr);	  
	  if (!use_incoming_cr)
	    {
#if (!GTK_CHECK_VERSION(3, 89, 0))
	      free_cairo(ss->cr);
#endif
	      ss->cr = NULL;
	    }
#endif
	  if (points == 0)
	    return;

	  /* the next two can be called outside this context, so they handle the cairo_t business themselves */

	  if ((cp->chan == 0) || 
	      (sp->channel_style != CHANNELS_SUPERIMPOSED)) 
	    display_channel_marks(cp);

	  if (cp->cursor_on) 
	    draw_graph_cursor(cp);
	}
    }

  /* -------- fft graph -------- */
  if ((with_fft) && 
      (!just_lisp) && (!just_time))
    {
#if USE_GTK
      if (!use_incoming_cr)
	{
#if (GTK_CHECK_VERSION(3, 89, 0))
	  if (!cp->graph_cr) return;
	  ss->cr = cp->graph_cr;
#else
	  ss->cr = make_cairo(cp->ax->wn);
#endif
	}
      cairo_push_group(ss->cr);
#endif

      if ((!(with_gl(ss))) || 
	  (cp->transform_graph_type != GRAPH_AS_SPECTROGRAM) ||
	  (color_map(ss) == BLACK_AND_WHITE_COLORMAP) ||
	  ((sp->nchans > 1) && 
	   (sp->channel_style != CHANNELS_SEPARATE)))

	make_axes(cp, fap, X_AXIS_IN_SECONDS, /* x-axis-style only affects the time domain graph */
		  (((cp->chan == 0) ||
		    (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH),
		  ((cp->show_grid) && 
		   (cp->transform_graph_type != GRAPH_AS_SPECTROGRAM)) ? WITH_GRID : NO_GRID,
		  (!(cp->fft_log_frequency)) ? WITH_LINEAR_AXES :
		   ((cp->transform_graph_type == GRAPH_AS_SONOGRAM) ? WITH_LOG_Y_AXIS : WITH_LOG_X_AXIS),
		  cp->show_axes);

      if ((!with_time) || (just_fft))
	{ /* make_graph does this -- sets losamp needed by fft to find its starting point */
	  ap->losamp = (mus_long_t)(ap->x0 * (double)snd_srate(sp));
	  ap->hisamp = (mus_long_t)(ap->x1 * (double)snd_srate(sp));
	}

      switch (cp->transform_graph_type)
	{
	case GRAPH_ONCE:
	  make_fft_graph(cp,
			 cp->fft->axis,
			 (sp->channel_style == CHANNELS_SUPERIMPOSED) ? combined_context(cp) : copy_context(cp),
			 cp->hookable);
	  break;

	case GRAPH_AS_SONOGRAM:
	  make_sonogram(cp);
	  break;

	case GRAPH_AS_WAVOGRAM:
	  break;

	case GRAPH_AS_SPECTROGRAM:
#if HAVE_GL
	  if (make_spectrogram(cp))
	    glDrawBuffer(GL_BACK);
#else
	  make_spectrogram(cp);
#endif
	  break;
	}

#if USE_GTK
      cairo_pop_group_to_source(ss->cr);
      cairo_paint(ss->cr);	  
      if (!use_incoming_cr)
	{
#if (!GTK_CHECK_VERSION(3, 89, 0))
	  free_cairo(ss->cr);
#endif
	  ss->cr = NULL;
	}
#endif
      /* this cairo_t can't be maintained across the fft functions -- they can be
       *   work procs that return (displaying) at any time in the future, and
       *   call check_for_event so that they can be interrupted. 
       */
	  
      if (cp->cursor_on) 
	{
	  cp->fft_cursor_visible = false; 
	  draw_sonogram_cursor(cp);
	}
    }


  /* -------- "lisp" (extlang) graph -------- */
  if ((with_lisp) && 
      (!just_fft) && (!just_time))
    {
      int pixel_loc = -1;
      Xen pixel_list = Xen_false;
      if ((just_lisp) || ((!with_time) && (!(with_fft))))
	{
	  ap->losamp = (mus_long_t)(ap->x0 * (double)snd_srate(sp));
	  ap->hisamp = (mus_long_t)(ap->x1 * (double)snd_srate(sp));
	}
      if ((cp->hookable == WITH_HOOK) &&
	  (!(ss->lisp_graph_hook_active)) &&
	  (Xen_hook_has_list(lisp_graph_hook)))
	{
	  ss->lisp_graph_hook_active = true;
	  /* inadvertent recursive call here can hang entire computer */
	  pixel_list = run_progn_hook(lisp_graph_hook,
				      Xen_list_2(C_int_to_Xen_sound(cp->sound->index),
						 C_int_to_Xen_integer(cp->chan)),
				      S_lisp_graph_hook);
	  ss->lisp_graph_hook_active = false;
	  if (!(Xen_is_false(pixel_list))) pixel_loc = snd_protect(pixel_list);
	}

      if (up != cp->lisp_info)
	up = cp->lisp_info;
      if (uap != up->axis)
	uap = up->axis;
      /* if these were changed in the hook function, the old fields should have been saved across the change (g_graph below) */

#if USE_GTK
      if (!(uap->ax))
	uap->ax = cp->ax;
      if (!use_incoming_cr)
	{
#if (GTK_CHECK_VERSION(3, 89, 0))
	  if (!cp->graph_cr) return;
	  ss->cr = cp->graph_cr;
#else
	  ss->cr = make_cairo(uap->ax->wn);
#endif
	}
      cairo_push_group(ss->cr);
#endif
      make_axes(cp, uap, /* defined in this file l 2293 */
		X_AXIS_IN_SECONDS,
		(((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH),
		cp->show_grid,
		WITH_LINEAR_AXES,
		up->show_axes);

      if (!(Xen_is_procedure(pixel_list)))
	make_lisp_graph(cp, pixel_list); /* this uses the cairo_t set up above, but possible pixel_list proc does not */

#if USE_GTK
      cairo_pop_group_to_source(ss->cr);
      cairo_paint(ss->cr);	  
      if (!use_incoming_cr)
	{
#if (!GTK_CHECK_VERSION(3, 89, 0))
	  free_cairo(ss->cr);
#endif
	  ss->cr = NULL;
	}
#endif

      if (Xen_is_procedure(pixel_list))
	Xen_call_with_no_args(pixel_list, S_lisp_graph);

      if (!(Xen_is_false(pixel_list))) snd_unprotect_at(pixel_loc);

      if ((cp->hookable == WITH_HOOK) &&
	  (Xen_hook_has_list(after_lisp_graph_hook)))
	run_hook(after_lisp_graph_hook,
		 Xen_list_2(C_int_to_Xen_sound(cp->sound->index),
			    C_int_to_Xen_integer(cp->chan)),
		 S_after_lisp_graph_hook);
    }
  
  if ((!just_lisp) && (!just_fft))
    run_after_graph_hook(cp);
}


static void display_channel_data_1(chan_info *cp, bool just_fft, bool just_lisp, bool just_time, bool use_incoming_cr)
{
  snd_info *sp;
  int width, height;

  sp = cp->sound;
  if ((sp->inuse == SOUND_IDLE) ||
      (cp->active < CHANNEL_HAS_AXES) ||
      (!(sp->active)) ||
      (!(channel_graph_is_visible(cp))))
    return;

  if ((sp->channel_style == CHANNELS_SEPARATE) || 
      (sp->nchans == 1))
    {
      width = widget_width(channel_graph(cp));
      height = widget_height(channel_graph(cp));

#if USE_GTK
      /* thanks to Tito Latini -- try to get reasonable size graphs even in a script (using --batch 1)
       */
      if (ss->batch_mode)
        {
          width = 600;
          height = 400;
        }
#endif

      if ((height > 5) && (width > 5))
	display_channel_data_with_size(cp, width, height, 0, just_fft, just_lisp, just_time, use_incoming_cr);
    }
  else
    {
      /* all chans in one graph widget, sy->scroll entire set, zy->zoom entire set etc */
      /* updates are asynchronous (dependent on background ffts etc), so we can't do the whole window at once */
      /* complication here is that we're growing down from the top, causing endless confusion */

#if USE_GTK
      width = widget_width(channel_graph(sp->chans[0])); /* don't fixup for gsy width -- those sliders are in different table slots */
#else
      width = widget_width(channel_graph(sp->chans[0])) - (2 * ss->position_slider_width);
#endif
      if (width <= 0) return;
      height = widget_height(channel_graph(sp->chans[0]));
      cp->height = height;
      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	display_channel_data_with_size(cp, width, height, 0, just_fft, just_lisp, just_time, use_incoming_cr);
      else
	{
	  int offset, full_height, y0, y1, bottom, top;
	  mus_float_t val, size, chan_height;

	  /* CHANNELS_COMBINED case */
	  val = gsy_value(sp->chans[0]);
	  size = gsy_size(sp->chans[0]);
	  full_height = (int)((mus_float_t)height / size);
	  chan_height = (mus_float_t)full_height / (mus_float_t)(sp->nchans);
	  bottom = (int)(full_height * val);
	  top = (int)(full_height * (val + size));
	  y1 = (int)((sp->nchans - cp->chan) * chan_height);
	  y0 = y1 - (int)chan_height;
	  offset = top - y1;

	  if ((cp->chan == 0) && 
	      (offset > 0))
	    {
	      /* round off trouble can lead to garbage collecting at the top of the window (similarly at the bottom I suppose) */
	      chan_height += offset;
	      offset = 0;
	    }
#if USE_GTK
	  /* this only needed because Gtk signal blocking is screwed up */
	  if ((cp->chan > 0) && 
	      (channel_has_active_mixes(cp)) && 
	      (cp->axis->height != height)) 
	    channel_set_mix_tags_erased(cp);
#endif

	  if ((cp->chan == (int)(sp->nchans - 1)) && 
	      ((offset + chan_height) < height))
	    chan_height = height - offset;
	  if (((y0 < top) && (y0 >= bottom)) || 
	      ((y1 > bottom) && (y1 <= top)))
	    display_channel_data_with_size(cp, width, (int)chan_height, offset, just_fft, just_lisp, just_time, use_incoming_cr);
	  else 
	    {
	      axis_info *ap;
	      ap = cp->axis;
	      ap->y_offset = offset; /* needed for mouse click channel determination */
	    }
	}
    }
}


void display_channel_fft_data(chan_info *cp)
{
  display_channel_data_1(cp, true, false, false, false);
}


static void display_channel_lisp_data(chan_info *cp)
{
  display_channel_data_1(cp, false, true, false, false);
}


void display_channel_time_data(chan_info *cp)
{
  display_channel_data_1(cp, false, false, true, false);
}


void display_channel_data(chan_info *cp)
{
  display_channel_data_1(cp, false, false, false, false);
}


void display_channel_data_for_print(chan_info *cp)
{
  display_channel_data_1(cp, false, false, false, true);
}



/* ---------------- CHANNEL CURSOR ---------------- */

static void erase_cursor(chan_info *cp)
{
#if USE_MOTIF
  draw_cursor(cp);
#endif
}


static void draw_graph_cursor(chan_info *cp)
{
  axis_info *ap;

  ap = cp->axis;
  if ((cursor_sample(cp) < ap->losamp) || 
      (cursor_sample(cp) > ap->hisamp) ||
      (!(ap->ax)))
    return;

  if ((cp->chan > 0) && 
      (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)) 
    return;

#if USE_GTK
  if (!(ap->ax->wn)) return;
#if (GTK_CHECK_VERSION(3, 89, 0))
  if (!cp->graph_cr) return;
  ss->cr = cp->graph_cr;
#else
  ss->cr = make_cairo(ap->ax->wn);
#endif
#endif

  if (cp->cursor_visible) 
    erase_cursor(cp);

  cp->cx = local_grf_x((double)(cursor_sample(cp)) / (double)snd_srate(cp->sound), ap); /* not float -- this matters in very long files (i.e. > 40 minutes) */
  if (cp->just_zero)
    {
      cp->cy = local_grf_y(0.0, ap);
      /* cp->old_cy = local_grf_y(chn_sample(cursor_sample(cp), cp, cp->edit_ctr), ap); */
    }
  else cp->cy = local_grf_y(chn_sample(cursor_sample(cp), cp, cp->edit_ctr), ap);
  
  draw_cursor(cp);
  cp->cursor_visible = true;

#if USE_GTK
#if (!GTK_CHECK_VERSION(3, 89, 0))
  free_cairo(ss->cr);
#endif
  ss->cr = NULL;
#endif
}


static void draw_sonogram_cursor_1(chan_info *cp)
{
  axis_info *fap;
  graphics_context *fax;

  fap = cp->fft->axis;
  fax = cursor_context(cp);

  if ((fap) && (fax)) 
#if (!USE_GTK)
    draw_line(fax, cp->fft_cx, fap->y_axis_y0, cp->fft_cx, fap->y_axis_y1);
#else
  {
    color_t old_color;
    fax = cp->ax; /* fap->ax does not work here?!? */
#if (GTK_CHECK_VERSION(3, 89, 0))
    if (!cp->graph_cr) return;
    ss->cr = cp->graph_cr;
#else
    ss->cr = make_cairo(fax->wn);
#endif
    /* y_axis_y0 > y_axis_y1 (upside down coordinates) */
    old_color = get_foreground_color(fax);
    set_foreground_color(fax, ss->cursor_color);
    draw_line(fax, cp->fft_cx, fap->y_axis_y0 - 1, cp->fft_cx, fap->y_axis_y1);
    set_foreground_color(fax, old_color);
    cp->fft_cursor_visible = (!(cp->fft_cursor_visible));
#if (!GTK_CHECK_VERSION(3, 89, 0))
    free_cairo(ss->cr);
#endif
    ss->cr = NULL;
  }
#endif
}


static void draw_sonogram_cursor(chan_info *cp)
{
  if ((cp->graph_transform_on) &&
      (cp->show_sonogram_cursor) &&
      (cp->fft) &&
      (cp->fft->axis) &&
      (cp->transform_graph_type == GRAPH_AS_SONOGRAM))
    {
      if (cp->fft_cursor_visible) draw_sonogram_cursor_1(cp);
      cp->fft_cx = local_grf_x((double)(cursor_sample(cp)) / (double)snd_srate(cp->sound), cp->fft->axis);
      draw_sonogram_cursor_1(cp);
      cp->fft_cursor_visible = true;
    }
}


kbd_cursor_t cursor_decision(chan_info *cp)
{
  mus_long_t len;
  len = current_samples(cp);
  if (cursor_sample(cp) >= len) cursor_sample(cp) = len - 1; /* zero based, but in 0-length files, len = 0 */
  if (cursor_sample(cp) < 0) cursor_sample(cp) = 0;          /* perhaps the cursor should be forced off in empty files? */
  if (cursor_sample(cp) < cp->axis->losamp)
    {
      if (cursor_sample(cp) == 0) return(CURSOR_ON_LEFT);
      else 
	{
	  if (cp->sound->playing)
	    return(CURSOR_ON_RIGHT);
	  return(CURSOR_IN_MIDDLE);
	}
    }
  if (cursor_sample(cp) > cp->axis->hisamp)
    {
      if (cursor_sample(cp) >= (len - 1)) return(CURSOR_ON_RIGHT);
      else 
	{
	  if (cp->sound->playing)
	    return(CURSOR_ON_LEFT);
	  return(CURSOR_IN_MIDDLE);
	}
    }
  return(CURSOR_IN_VIEW);
}


void handle_cursor(chan_info *cp, kbd_cursor_t redisplay)
{
  /* no sync here */
  if (!cp) return;
  if (redisplay != KEYBOARD_NO_ACTION)
    {
      snd_info *sp;

      sp = cp->sound;
      if (cp->with_verbose_cursor)
	show_cursor_info(cp); 

      if (redisplay != CURSOR_IN_VIEW)
	{
	  double gx = 0.0;
	  axis_info *ap;
	  ap = cp->axis;
	  switch (redisplay)
	    {
	    case CURSOR_ON_LEFT: 
	      gx = (double)(cursor_sample(cp)) / (double)snd_srate(sp); 
	      break;

	    case CURSOR_ON_RIGHT: 
	      gx = (double)(cursor_sample(cp)) / (double)snd_srate(sp) - ap->zx * ap->x_ambit; 
	      break;

	    case CURSOR_IN_MIDDLE: 
	      gx = (double)(cursor_sample(cp)) / (double)snd_srate(sp) - ap->zx * 0.5 * ap->x_ambit; 
	      break;

	    default:
	      break;
	    }
	  if (gx < 0.0) gx = 0.0;
	  if (ap->x_ambit != 0.0)
	    reset_x_display(cp, (gx - ap->xmin) / ap->x_ambit, ap->zx);
	}
      else 
	{
	  if (cp->cursor_on) 
	    {
#if USE_MOTIF
	      draw_graph_cursor(cp);
	      draw_sonogram_cursor(cp);
              if (!(sp->playing))
                update_graph(cp);
#endif
#if USE_GTK
	      update_graph(cp);
#endif
	    }
	}
    }
  {
    /* not sure about this */
    int i;
    for (i = 0; i < cp->edit_size; i++) 
      if (cp->edits[i]) 
	cp->edits[i]->cursor = cursor_sample(cp);
  }
  update_possible_selection_in_progress(cursor_sample(cp));
}


void handle_cursor_with_sync(chan_info *cp, kbd_cursor_t redisplay)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      int i;
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++)
	handle_cursor(si->cps[i], redisplay);
      free_sync_info(si);
    }
  else handle_cursor(cp, redisplay);
}


void cursor_moveto(chan_info *cp, mus_long_t samp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      int i;
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++)
	{
	  chan_info *ncp;
	  ncp = si->cps[i];
	  cursor_sample(ncp) = samp;
	  handle_cursor(ncp, cursor_decision(ncp)); /* checks len */
	}
      free_sync_info(si);
    }
  else 
    {
      cursor_sample(cp) = samp;
      handle_cursor(cp, cursor_decision(cp));
    }
}


void cursor_move(chan_info *cp, mus_long_t samps)
{
  cursor_moveto(cp, cursor_sample(cp) + samps);
}


void cursor_moveto_without_verbosity(chan_info *cp, mus_long_t samp)
{
  bool old_verbose;
  int old_sync;
  old_verbose = cp->with_verbose_cursor;
  old_sync = cp->sound->sync;
  cp->with_verbose_cursor = false;
  cp->sound->sync = 0;
  cursor_moveto(cp, samp);
  cp->with_verbose_cursor = old_verbose;
  cp->sound->sync = old_sync;
}


void cursor_moveto_with_window(chan_info *cp, mus_long_t samp, mus_long_t left_samp, mus_long_t window_size)
{
  /* restore old window and cursor as much as possible */
  double gx;
  snd_info *sp;
  mus_long_t current_window_size;
  axis_info *ap;

  sp = cp->sound;
  ap = cp->axis;
  if (cp->cursor_visible)
    {
      if (cp->graph_time_on) 
	{
#if USE_GTK
#if (GTK_CHECK_VERSION(3, 89, 0))
	  if (!cp->graph_cr) return;
	  ss->cr = cp->graph_cr;
#else
	  ss->cr = make_cairo(ap->ax->wn);
#endif
#endif
	  erase_cursor(cp);
#if USE_GTK
#if (!GTK_CHECK_VERSION(3, 89, 0))
	  free_cairo(ss->cr);
#endif
	  ss->cr = NULL;
#endif
	}
      cp->cursor_visible = false; /* don't redraw at old location */
    }
  if (cp->fft_cursor_visible)
    {
      if ((cp->fft) && (cp->graph_transform_on)) draw_sonogram_cursor_1(cp);
      cp->fft_cursor_visible = false; /* don't redraw at old location */
    }

  cursor_sample(cp) = samp;
  current_window_size = ap->hisamp - ap->losamp;
  if (snd_abs_mus_long_t(current_window_size - window_size) < (mus_long_t)(0.1 * (double)window_size))
    gx = (double)(left_samp) / (double)snd_srate(sp);
  else gx = (double)(samp) / (double)snd_srate(sp) - ap->zx * 0.5 * ap->x_ambit; 
  if (gx < 0.0) gx = 0.0;
  if (ap->x_ambit != 0.0)
    reset_x_display(cp, (gx - ap->xmin) / ap->x_ambit, ap->zx);
}


void sync_cursors(chan_info *cp, mus_long_t samp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      int i;
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++)
	cursor_sample(si->cps[i]) = samp;
      free_sync_info(si);
    }
  else cursor_sample(cp) = samp;
}



void show_cursor_info(chan_info *cp)
{
  char *expr_str;
  snd_info *sp;
  mus_float_t y, absy;
  int digits, i, len;
  mus_long_t samp;
  char *s1, *s2;

  sp = cp->sound;
  if ((sp->sync != 0) && (cp->chan != 0)) return;

  samp = cursor_sample(cp);
  y = chn_sample(samp, cp, cp->edit_ctr);
  absy = fabs(y);
  if (absy < .0001) digits = 5;
  else if (absy<.01) digits = 4;
  else if (absy<1.0) digits = 3;
  else digits = 2;
  len = PRINT_BUFFER_SIZE;
  expr_str = (char *)calloc(len, sizeof(char));

  if (sp->nchans == 1)
    snprintf(expr_str, PRINT_BUFFER_SIZE, "cursor at %s (sample %" print_mus_long ") = %s",
		 s1 = x_axis_location_to_string(cp, (double)samp / (double)snd_srate(sp)),
		 samp,
		 s2 = prettyf(y, digits));
  else
    {
      if (sp->sync == 0)
	snprintf(expr_str, PRINT_BUFFER_SIZE, "chan %d, cursor at %s (sample %" print_mus_long ") = %s",
		     cp->chan + 1,
		     s1 = x_axis_location_to_string(cp, (double)samp / (double)snd_srate(sp)),
		     samp,
		     s2 = prettyf(y, digits));
      else
	{
	  /* in this case, assume we show all on chan 0 and ignore the call otherwise (see above) */
	  /* "cursor at..." then list of values */
	  snprintf(expr_str, PRINT_BUFFER_SIZE, "cursor at %s (sample %" print_mus_long "): %s",
		       s1 = x_axis_location_to_string(cp, (double)samp / (double)snd_srate(sp)),
		       samp,
		       s2 = prettyf(y, digits));
	  for (i = 1; i < (int)sp->nchans; i++)
	    {
	      chan_info *ncp;

	      expr_str = mus_strcat(expr_str, ", ", &len);
	      free(s2);
	      ncp = sp->chans[i];
	      y = chn_sample(samp, ncp, ncp->edit_ctr);
	      absy = fabs(y);
	      if (absy < .0001) digits = 5;
	      else if (absy<.01) digits = 4;
	      else if (absy<1.0) digits = 3;
	      else digits = 2;
	      s2 = prettyf(y, digits);
	      expr_str = mus_strcat(expr_str, s2, &len);
	    }
	}
    }
  set_status(sp, expr_str, false);
  free(expr_str);
  free(s1);
  free(s2);
}


void goto_graph(chan_info *cp)
{
  if ((cp) && (!(cp->squelch_update)))
    {
      snd_info *sp;
      sp = cp->sound;
      if ((cp->chan == 0) || (sp->channel_style == CHANNELS_SEPARATE))
	goto_window(channel_graph(cp));
      else goto_window(channel_graph(sp->chans[0]));
    }
}



/* ---------------- graphics callbacks ---------------- */

#define HIT_SLOP 4

static bool hit_cursor_triangle(chan_info *cp, int x, int y)
{
  axis_info *ap;
  mus_long_t samp;
  int cx;

  ap = cp->axis;
  samp = cursor_sample(cp);

  if ((samp < ap->losamp) ||
      (samp > ap->hisamp))
    return(false);

  cx = grf_x((double)samp / (double)snd_srate(cp->sound), ap);
  if ((cx > (x + HIT_SLOP)) ||
      ((cx + play_arrow_size(ss) + HIT_SLOP) < x))
    return(false);

  y = y - ap->y_axis_y0 - play_arrow_size(ss);
  if (y < 0) y = -y;
  return((cx + play_arrow_size(ss) - y + HIT_SLOP) >= x);
}

#define SLOPPY_MOUSE 6

#if USE_NO_GUI
#define GUI_SET_CURSOR(w, cursor)
#else
#if USE_GTK
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(WIDGET_TO_WINDOW(w), cursor)
#else
#define GUI_SET_CURSOR(w, cursor) XUndefineCursor(XtDisplay(w), XtWindow(w)); XDefineCursor(XtDisplay(w), XtWindow(w), cursor)
#endif
#endif

typedef enum {CLICK_NOGRAPH, CLICK_WAVE, CLICK_FFT_AXIS, CLICK_LISP, CLICK_MIX, CLICK_MARK,
	      CLICK_FFT_MAIN, CLICK_SELECTION_LEFT, CLICK_SELECTION_RIGHT, CLICK_SELECTION_PLAY, 
	      CLICK_INSET_GRAPH, CLICK_MIX_PLAY, CLICK_CURSOR_PLAY, CLICK_MARK_PLAY,
              CLICK_SELECTION_LOOP_PLAY, CLICK_SELECTION_MAIN, CLICK_FFT_AUX} click_loc_t;

typedef struct inset_graph_info_t {  /* chan_info field is inset_graph, set to null in snd-data, but not cleared or freed */
  int width, height, edpos, y_offset, data_size, x0, x1, y0, y1;
  point_t *data0, *data1;
  bool graphing;
  double maxamp;
} inset_graph_info_t;

static int mix_tag = NO_MIX_TAG, mix_play_tag = NO_MIX_TAG;
static mark *play_mark = NULL;
static mark *mouse_mark = NULL;


static click_loc_t within_graph(chan_info *cp, int x, int y)
{
  int x0, x1, y0, y1;
  axis_info *ap;
  #define SELECTION_DRAG_HEIGHT 50

  x0 = x - SLOPPY_MOUSE;
  x1 = x + SLOPPY_MOUSE;
  y0 = y - SLOPPY_MOUSE;
  y1 = y + SLOPPY_MOUSE;

  if (cp->graph_time_on)
    {
      ap = cp->axis;
      /* does (x, y) fall within the current axis bounds x_axis_x0|x1, y_axis_y0|y1 */
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	{
	  /* here we are inside the graph (within the axes)
	   *   we need to check for marks and whatnot before the selection, else there's no way
	   *   to drag a mark that's within the selection.
	   */

	  if ((with_inset_graph(ss)) &&
	      (cp->inset_graph) &&
	      (cp->inset_graph->graphing) &&
	      (x1 > cp->inset_graph->x0) &&
	      (x0 < cp->inset_graph->x1) &&
	      (y1 > cp->inset_graph->y0) &&
	      (y0 < cp->inset_graph->y1))
	    return(CLICK_INSET_GRAPH);

	  mix_tag = hit_mix(cp, x, y);
	  if (mix_tag != NO_MIX_TAG)
	    return(CLICK_MIX);

	  mix_play_tag = hit_mix_triangle(cp, x, y);
	  if (mix_play_tag != NO_MIX_TAG)
	    return(CLICK_MIX_PLAY);

	  mouse_mark = hit_mark(cp, x, y);
	  if (mouse_mark)
	    return(CLICK_MARK);

	  if (selection_is_active_in_channel(cp))
	    {
	      mus_long_t bpos, epos, xpos;
	      double sr;
	      sr = (double)snd_srate(cp->sound);
	      bpos = selection_beg(cp);
	      epos = selection_end(cp);

	      if ((y < (ap->y_offset + SELECTION_DRAG_HEIGHT)) ||
		  (y > (ap->y_axis_y0 - SELECTION_DRAG_HEIGHT)))
		{
		  /* look for click at selection boundary */
		  if ((bpos >= ap->losamp) &&
		      (bpos <= ap->hisamp))
		    {
		      mus_long_t x0_pos, x1_pos;
		      x0_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x0) * sr);
		      x1_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x1) * sr);
		      if ((bpos > x0_pos) && (bpos < x1_pos))
			return(CLICK_SELECTION_LEFT); /* "click" is a misnomer -- we have moved to the portion where we can grab the selection and resize it */
		    }
		  
		  if ((epos >= ap->losamp) &&
		      (epos <= ap->hisamp))
		    {
		      mus_long_t x0_pos, x1_pos;
		      x0_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x0) * sr);
		      x1_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x1) * sr);
		      if ((epos > x0_pos) && (epos < x1_pos))
			return(CLICK_SELECTION_RIGHT);
		    }
		}
	      
	      xpos = snd_round_mus_long_t(ungrf_x(cp->axis, x) * sr); /* a sample number */
	      
	      if ((bpos <= xpos) &&
		  (epos >= xpos))
		return(CLICK_SELECTION_MAIN);
	    }

	  return(CLICK_WAVE);
	}

      /* possibly in time graph but outside axes */
      if (selection_is_active_in_channel(cp))
	{
	 if (hit_selection_triangle(cp, x, y))
	   return(CLICK_SELECTION_PLAY);

	 if (hit_selection_loop_triangle(cp, x, y))
	   return(CLICK_SELECTION_LOOP_PLAY);
	}
      
      play_mark = hit_mark_triangle(cp, x, y);
      if (play_mark)
	return(CLICK_MARK_PLAY);

      if (hit_cursor_triangle(cp, x, y))
	return(CLICK_CURSOR_PLAY);
    }

  if (((cp->graph_lisp_on) || 
       (Xen_hook_has_list(lisp_graph_hook))) && 
      (cp->lisp_info))
    {
      ap = cp->lisp_info->axis;
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	return(CLICK_LISP);
    }

  if ((cp->graph_transform_on) && (cp->fft))
    {
      ap = cp->fft->axis;
      if (!ap) return(CLICK_NOGRAPH); /* apparently can happen if fft is being redrawn when we click */
      /* look first for on-axis (axis drag) mouse */
#if HAVE_GL
      if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) && 
	  (ap->used_gl))
	{
	  GLdouble xx;
	  xx = unproject_to_x(x, y);
	  if ((xx > -0.7) && (xx < -0.49))
	    return(CLICK_FFT_AXIS);
	}
#endif
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
	  if (((x >= ap->x_axis_x0) && (x <= ap->x_axis_x1)) && 
	      ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y0)))
	    return(CLICK_FFT_AXIS);
	}
      else
	{
	  if (((x0 <= ap->x_axis_x0) && (x1 >= ap->x_axis_x0)) && 
	      ((y <= ap->y_axis_y0) && (y >= ap->y_axis_y1)))
	    return(CLICK_FFT_AXIS);
	}
      /* now check within fft graph */
      if ((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0))
	{
	  if ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1))
	    return(CLICK_FFT_MAIN);
	  return(CLICK_FFT_AUX);
	}
    }

  return(CLICK_NOGRAPH);
}


void check_cursor_shape(chan_info *cp, int x, int y)
{
  chan_info *ncp;
  
  if ((!cp) || (!cp->sound) || (cp->active == CHANNEL_INACTIVE) || (cp->squelch_update)) return;

  if (cp->sound->channel_style == CHANNELS_COMBINED) 
    ncp = which_channel(cp->sound, y);
  else ncp = cp;

  switch (within_graph(ncp, x, y))
    {
    case CLICK_SELECTION_LEFT:
    case CLICK_SELECTION_RIGHT:
    case CLICK_MIX:
    case CLICK_MARK:
      if (cp->current_cursor != ss->bounds_cursor)
	{
	  cp->current_cursor = ss->bounds_cursor;
	  GUI_SET_CURSOR(channel_graph(cp), ss->bounds_cursor);
	}
      break;

    case CLICK_FFT_AXIS:
      /* these all involve a drag if the mouse is pressed 
       *   but for fft axis, if sonogram, we want an up-and-down arrow, not left-to-right
       */
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
	  if (cp->current_cursor != ss->bounds_cursor)
	    {
	      cp->current_cursor = ss->bounds_cursor;
	      GUI_SET_CURSOR(channel_graph(cp), ss->bounds_cursor);
	    }
	}
      else
	{
	  if (cp->current_cursor != ss->yaxis_cursor)
	    {
	      cp->current_cursor = ss->yaxis_cursor;
	      GUI_SET_CURSOR(channel_graph(cp), ss->yaxis_cursor);
	    }
	}
      break;

    case CLICK_MIX_PLAY:
    case CLICK_SELECTION_PLAY:
    case CLICK_CURSOR_PLAY:
    case CLICK_MARK_PLAY:
      if (!(cp->sound->playing) && (cp->current_cursor != ss->play_cursor))
        {
          snd_info *sp;
          sp = cp->sound;
          if (sp->sync != 0)
            {
              int i;
              sync_info *si;
              si = snd_sync(sp->sync);
              for (i = 0; i < si->chans; i++)
                si->cps[i]->original_cursor = cursor_sample(ncp);
            }
          else ncp->original_cursor = cursor_sample(ncp);
	  cp->current_cursor = ss->play_cursor;
	  GUI_SET_CURSOR(channel_graph(cp), ss->play_cursor);
	}
      break;

    case CLICK_SELECTION_LOOP_PLAY:
      if (cp->current_cursor != ss->loop_play_cursor)
	{
	  cp->current_cursor = ss->loop_play_cursor;
	  GUI_SET_CURSOR(channel_graph(cp), ss->loop_play_cursor);
	}
      break;

    default:
      if (cp->current_cursor != ss->graph_cursor)
	{
	  cp->current_cursor = ss->graph_cursor;
	  GUI_SET_CURSOR(channel_graph(cp), ss->graph_cursor);
	}
      break;
    }
}


static char *describe_fft_point(chan_info *cp, int x, int y)
{
  mus_float_t xf, yf;
  axis_info *ap;
  fft_info *fp;
  sono_info *si;
  int ind, time, digits = 3;
  fp = cp->fft;
  ap = fp->axis;

  if ((ap->x_axis_x1 == ap->x_axis_x0) || (ap->y_axis_y1 == ap->y_axis_y0))
    return(mus_strdup("?"));

  x = mus_iclamp(ap->x_axis_x0, x, ap->x_axis_x1);
  xf = ap->x0 + (ap->x1 - ap->x0) * (mus_float_t)(x - ap->x_axis_x0) / (mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0);

  digits = (int)ceil(fabs(cp->min_dB) / 20);
  if (digits < 2) digits = 2;

  if (cp->transform_graph_type == GRAPH_ONCE)
    {
      if (cp->transform_type == FOURIER)
	{
	  if (cp->fft_log_frequency)
	    {
	      mus_float_t minlx = 0.0;
	      if (ap->x0 > 1.0) minlx = log(ap->x0); else minlx = 0.0;
	      xf = exp(minlx + ((xf - ap->x0) * (log(ap->x1) - minlx) / (ap->x1 - ap->x0)));
	      ind = (int)((fp->current_size * xf) / (mus_float_t)snd_srate(cp->sound));
	    }
	  else ind = (int)((fp->current_size * xf) / (mus_float_t)snd_srate(cp->sound));
	}
      else ind = (int)xf;
      if (ind >= fp->current_size) ind = fp->current_size - 1;

      if ((cp->fft_with_phases) && 
	  (fp->phases) &&
	  (cp->transform_type == FOURIER))
	return(mus_format("(%.1f Hz: %.*f%s, %.*f radians (unscaled: %.*f)",
			  xf,
			  digits,
			  (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, (fp->data[ind] * fp->scale)) : (fp->data[ind] * fp->scale),
			  (cp->fft_log_magnitude) ? "dB" : "",
			  digits,
			  fp->phases[ind],
			  digits,
			  fp->data[ind]));
      else
	return(mus_format("(%.1f%s: %.*f%s (unscaled: %.*f)",
			  xf,
			  (((cp->transform_type == AUTOCORRELATION) || (cp->transform_type == CEPSTRUM)) ? " samps" : " Hz"),
			  digits,
			  (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, (fp->data[ind] * fp->scale)) : (fp->data[ind] * fp->scale),
			  (cp->fft_log_magnitude) ? "dB" : "",
			  digits,
			  fp->data[ind]));
    }
  else 
    {
      if (cp->transform_graph_type == GRAPH_AS_SONOGRAM)
	{
	  y = mus_iclamp(ap->y_axis_y1, y, ap->y_axis_y0);
	  yf = ap->y0 + (ap->y1 - ap->y0) * (mus_float_t)(y - ap->y_axis_y0) / (mus_float_t)(ap->y_axis_y1 - ap->y_axis_y0);

	  si = cp->sonogram_data;
	  if (cp->transform_type == FOURIER)
	    {
	      if (cp->fft_log_frequency)
		{
		  mus_float_t minlx = 0.0;
		  if (ap->y0 > 1.0) minlx = log(ap->y0); else minlx = 0.0;
		  yf = exp(minlx + ((yf - ap->y0) * (log(ap->y1) - minlx) / (ap->y1 - ap->y0)));
		  ind = (int)((fp->current_size * yf) / (mus_float_t)snd_srate(cp->sound));
		}
	      else ind = (int)((fp->current_size * yf) / (mus_float_t)snd_srate(cp->sound));
	    }
	  else ind = (int)yf;
	  if (ind >= si->total_bins) ind = si->total_bins - 1;
	  time = (int)(si->target_slices * (mus_float_t)(x - ap->x_axis_x0) / (mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0));
	  if (time >= si->total_slices) time = si->total_slices - 1;
	  return(mus_format("(time: %.2f, freq: %.1f, val: %.*f%s (raw: %.*f))",
			    xf, yf,
			    digits,
			    (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, si->data[time][ind] / si->scale) : (si->data[time][ind] / si->scale),
			    (cp->fft_log_magnitude) ? "dB" : "",
			    digits,
			    si->data[time][ind]));
	}
    }
  return(mus_strdup("?"));
}


void fftb(chan_info *cp, bool on)
{
  cp->graph_transform_on = on;
  set_toggle_button(channel_f(cp), on, false, (void *)cp);
  calculate_fft(cp);
}


void waveb(chan_info *cp, bool on)
{
  cp->graph_time_on = on;
  set_toggle_button(channel_w(cp), on, false, (void *)cp);
  update_graph_or_warn(cp);
}


static void propagate_wf_state(snd_info *sp)
{
  uint32_t i;
  bool w, f;
  chan_info *cp;

  cp = sp->chans[0];
  w = cp->graph_time_on;
  f = cp->graph_transform_on;

  for (i = 1; i < sp->nchans; i++) 
    {
      cp = sp->chans[i];
      cp->graph_time_on = w;
      cp->graph_transform_on = f;
      set_toggle_button(channel_f(cp), f, false, (void *)cp);
      set_toggle_button(channel_w(cp), w, false, (void *)cp);
    }
  for_each_sound_chan(sp, update_graph_or_warn);
}


void f_button_callback(chan_info *cp, bool on, bool with_control)
{
  snd_info *sp;
  cp->graph_transform_on = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propagate_wf_state(sp);
  else
    {
      update_graph_or_warn(cp);
      if (with_control)
	{
	  uint32_t i;
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      chan_info *ncp;
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->graph_transform_on = on;
#if USE_GTK
		  set_toggle_button(channel_f(ncp), on, true, (void *)cp);
#else
		  set_toggle_button(channel_f(ncp), on, false, (void *)cp);
#endif
		  update_graph_or_warn(ncp);
		}
	    }
	}
      goto_graph(cp);
    }
}


void w_button_callback(chan_info *cp, bool on, bool with_control)
{
  snd_info *sp;
  cp->graph_time_on = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propagate_wf_state(sp);
  else
    {
      update_graph_or_warn(cp);
      if (with_control)
	{
	  uint32_t i;
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      chan_info *ncp;
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->graph_time_on = on;
#if USE_GTK
		  set_toggle_button(channel_w(ncp), on, true, (void *)cp);
#else
		  set_toggle_button(channel_w(ncp), on, false, (void *)cp);
#endif
		  update_graph_or_warn(ncp);
		}
	    }
	}
      goto_graph(cp);
    }
}


void key_press_callback(chan_info *ncp, int x, int y, int key_state, int keysym)
{
  /* called by every key-intercepting widget in the entire sound pane */
  chan_info *cp;
  snd_info *sp;

  if (!ncp) return;
  cp = virtual_selected_channel(ncp);
  sp = cp->sound;
  select_channel(sp, cp->chan);

  if (((cp->graph_lisp_on) || (Xen_hook_has_list(lisp_graph_hook))) &&
      (within_graph(cp, x, y) == CLICK_LISP) &&
      (Xen_hook_has_list(key_press_hook)))
    {
      Xen res;
      res = run_or_hook(key_press_hook,
			Xen_list_4(C_int_to_Xen_sound(sp->index),
				   C_int_to_Xen_integer(cp->chan),
				   C_int_to_Xen_integer(keysym),
				   C_int_to_Xen_integer(key_state)), /* this can have NumLock etc -- will be masked off in keyboard_command */
			S_key_press_hook);
      if (Xen_is_true(res))
	return;
    }
  keyboard_command(cp, keysym, key_state);
  /* if lisp graph has cursor? */
}


chan_info *which_channel(snd_info *sp, int y)
{
  uint32_t i;
  chan_info *ncp = NULL;

  if (y <= 0) /* this can happen if we drag the mouse over the top of the Snd window, then release it */
    return(sp->chans[0]);

  for (i = 0; i < sp->nchans; i++)
    {
      axis_info *ap;
      chan_info *cp;
      cp = sp->chans[i];
      ap = cp->axis;
      if (y < ap->y_offset) 
	{
	  if (ncp)
	    return(ncp);
	  else return(cp);
	}
      ncp = cp;
    }
  return(ncp);
}


static mus_float_t fft_axis_extent(chan_info *cp)
{
  axis_info *ap;
  ap = cp->fft->axis;
  if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
    return((mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0));
  else return((mus_float_t)(ap->y_axis_y0 - ap->y_axis_y1));
}


static void calculate_syncd_fft(chan_info *cp, int value)
{
  if (cp->sound->sync == value) calculate_fft(cp);
}


static bool dragged = false;
static oclock_t mouse_down_time;
static click_loc_t click_within_graph = CLICK_NOGRAPH;
static int fft_axis_start = 0;
#if HAVE_GL
  static mus_float_t fft_faxis_start = 0.0;
#endif
static chan_info *dragged_cp;

#ifdef __APPLE__
static int press_x, press_y;
#endif


void graph_button_press_callback(chan_info *cp, void *ev, int x, int y, int key_state, int button, oclock_t time)
{
  snd_info *sp;

  sp = cp->sound;
  if ((cp->active < CHANNEL_HAS_AXES) || (!sp)) return; /* autotest silliness */

  /* if combining, figure out which virtual channel the mouse is in */
  if (sp->channel_style == CHANNELS_COMBINED) cp = which_channel(sp, y); /* select this?? */

  click_within_graph = within_graph(cp, x, y);
  select_channel(sp, cp->chan);

  if (button == POPUP_BUTTON)
    {
      switch (click_within_graph)
	{
	case CLICK_NOGRAPH:
	case CLICK_WAVE:
	case CLICK_MIX:
	case CLICK_MARK:
	case CLICK_INSET_GRAPH:
	case CLICK_MIX_PLAY:
	case CLICK_CURSOR_PLAY:
	case CLICK_MARK_PLAY:
	  post_basic_popup_menu(ev);
	  break;
	  
	case CLICK_FFT_AXIS:
	case CLICK_FFT_MAIN:
	case CLICK_FFT_AUX:
	  post_fft_popup_menu(ev);
	  break;
	  
	case CLICK_SELECTION_LEFT:
	case CLICK_SELECTION_RIGHT:
	case CLICK_SELECTION_PLAY:
	case CLICK_SELECTION_LOOP_PLAY:
	case CLICK_SELECTION_MAIN:
	  post_selection_popup_menu(ev);
	  break;
	  
	case CLICK_LISP:
	  post_lisp_popup_menu(ev);
	  break;
	}
      return;
    }

  mouse_down_time = time;
#ifdef __APPLE__
  press_x = x;
  press_y = y;
#endif
  /* select_channel(sp, cp->chan); */
  dragged_cp = cp;
  dragged = false;
  finish_selection_creation();

  switch (click_within_graph)
    {
    case CLICK_FFT_AXIS:
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
#if HAVE_GL
	  if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) &&
	      (cp->fft->axis->used_gl))
	    fft_faxis_start = unproject_to_y(x, y);
#endif
	  fft_axis_start = x;
	}
      else fft_axis_start = y;
      break;

    case CLICK_LISP:
      if (Xen_hook_has_list(mouse_press_hook))
	run_hook(mouse_press_hook,
		 Xen_list_6(C_int_to_Xen_sound(sp->index),
			    C_int_to_Xen_integer(cp->chan),
			    C_int_to_Xen_integer(button),
			    C_int_to_Xen_integer(key_state),
			    C_double_to_Xen_real(ungrf_x(cp->lisp_info->axis, x)),
			    C_double_to_Xen_real(ungrf_y(cp->lisp_info->axis, y))),
		 S_mouse_press_hook);
      break;

    case CLICK_MARK:
      set_mark_control(cp, mouse_mark, key_state);
      break;

    case CLICK_WAVE:
    case CLICK_SELECTION_MAIN:
    case CLICK_MIX:
      break;

    case CLICK_SELECTION_LEFT:
    case CLICK_SELECTION_RIGHT:
      dragged = true;
      restart_selection_creation(cp, click_within_graph == CLICK_SELECTION_RIGHT);
      break;

    case CLICK_MARK_PLAY:
    case CLICK_MIX_PLAY:
    case CLICK_CURSOR_PLAY:
    case CLICK_SELECTION_LOOP_PLAY:
    case CLICK_SELECTION_PLAY:
      if ((sp->playing) ||
	  (ss->selection_play_stop))
	{
	  stop_playing_all_sounds(PLAY_BUTTON_UNSET);
	  set_play_button(sp, false);
	  reflect_play_selection_stop(); /* this sets ss->selection_play_stop = false; */
	}
      else
	{
	  switch (click_within_graph)
	    {
	    case CLICK_MARK_PLAY:
	      if (mark_sync(play_mark))
		play_syncd_mark(cp, play_mark);
	      else 
		{
		  if (key_state & snd_ControlMask)
		    play_sound(sp, mark_sample(play_mark), NO_END_SPECIFIED);
		  else play_channel(cp, mark_sample(play_mark), NO_END_SPECIFIED);
		}
	      break;

	    case CLICK_MIX_PLAY:
	      play_mix_from_id(mix_play_tag);
	      break;
	      
	    case CLICK_CURSOR_PLAY:
	      play_channel_with_sync(cp, cursor_sample(cp), NO_END_SPECIFIED);
	      break;
	      
	    case CLICK_SELECTION_LOOP_PLAY:
	      ss->selection_play_stop = true;
	      loop_play_selection();
	      break;
	      
	    case CLICK_SELECTION_PLAY:
	      ss->selection_play_stop = true;
	      play_selection(IN_BACKGROUND);
	      break;

	    default:
	      break;
	    }
	}
      break;

    case CLICK_INSET_GRAPH:
    case CLICK_NOGRAPH:
    case CLICK_FFT_MAIN:
    case CLICK_FFT_AUX:
      break;
    }
}


#if USE_MOTIF
  #define BUTTON_2 Button2
#else
  #define BUTTON_2 2
#endif


void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button)
{
  snd_info *sp;

  sp = cp->sound;
  if ((cp->active < CHANNEL_HAS_AXES) || (!sp)) return; /* autotest silliness */

  if (sp->channel_style == CHANNELS_COMBINED)
    {
      if ((dragged) && (dragged_cp))
	cp = dragged_cp;
      else cp = which_channel(sp, y);
    }

  dragged_cp = NULL;
  if (!dragged)
    {
      click_loc_t actax;
      actax = within_graph(cp, x, y);
      
      if ((Xen_hook_has_list(mouse_click_hook)) &&
	  (Xen_is_true(run_or_hook(mouse_click_hook,
				  Xen_list_7(C_int_to_Xen_sound(sp->index),
					     C_int_to_Xen_integer(cp->chan),
					     C_int_to_Xen_integer(button),
					     C_int_to_Xen_integer(key_state),
					     C_int_to_Xen_integer(x),
					     C_int_to_Xen_integer(y),
					     C_int_to_Xen_integer((int)(((actax == CLICK_FFT_AXIS) || (actax == CLICK_FFT_MAIN)) ? 
								TRANSFORM_AXIS_INFO : ((actax == CLICK_LISP) ? 
										       LISP_AXIS_INFO : TIME_AXIS_INFO)))),
				  S_mouse_click_hook))))
	return;
      
      switch (actax)
	{
	case CLICK_INSET_GRAPH:
	  {
	    mus_long_t samp;
	    samp = snd_round_mus_long_t(current_samples(cp) * ((double)(x - cp->inset_graph->x0) / (double)(cp->inset_graph->width)));
	    cursor_moveto(cp, samp);
	    if ((samp < cp->axis->losamp) ||
		(samp > cp->axis->hisamp))
	      {
		mus_long_t rsamp;
		rsamp = samp + snd_round_mus_long_t(0.5 * (cp->axis->hisamp - cp->axis->losamp));
		if (rsamp < 0) rsamp = 0;
		if (rsamp > current_samples(cp)) rsamp = current_samples(cp);
		set_x_axis_x1(cp, rsamp);
		update_graph(cp);
	      }
	  }
	  break;
	  
	case CLICK_SELECTION_LEFT:
	case CLICK_SELECTION_RIGHT:
	case CLICK_SELECTION_MAIN:
	case CLICK_MIX:
	case CLICK_MARK:
	case CLICK_WAVE:
	  if (button == BUTTON_2) /* the middle button */
	    {
	      cp->cursor_on = true;
	      cursor_moveto(cp, snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)snd_srate(sp)));
	      paste_region(region_list_position_to_id(0), cp);
	    }
	  else 
	    {
	      if (key_state & (snd_ShiftMask | snd_ControlMask | snd_MetaMask))
		{
		  mus_long_t samps;
		  axis_info *ap;
		  /* zoom request -> each added key zooms closer, as does each successive click */
		  ap = cp->axis;
		  samps = current_samples(cp);
		  if ((samps > 0) && ((ap->zx * (double)samps) > 1.0))
		    {
		      if (key_state & snd_ShiftMask) ap->zx *= .5;
		      if (key_state & snd_ControlMask) ap->zx *= .5;
		      if (key_state & snd_MetaMask) ap->zx *= .5;
		      if (ap->x_ambit != 0.0)
			ap->sx = (((double)(cursor_sample(cp)) / (double)snd_srate(sp) - 
				   ap->zx * 0.5 * (ap->xmax - ap->xmin)) - ap->xmin) / ap->x_ambit;
		      apply_x_axis_change(cp);
		      resize_sx_and_zx(cp);
		    }
		}
	      else
		{
		  cp->cursor_on = true;
		  cursor_moveto(cp, snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)snd_srate(sp)));
		  if (mouse_mark)
		    {
		      Xen res = Xen_false;
		      if (Xen_hook_has_list(mark_click_hook))
			res = run_progn_hook(mark_click_hook,
					     Xen_list_1(new_xen_mark(mark_to_int(mouse_mark))),
					     S_mark_click_hook);
		      if (!(Xen_is_true(res)))
			{
			  mus_long_t samp;
			  int sync;
			  samp = mark_sample(mouse_mark);
			  sync = mark_sync(mouse_mark);
			  if (sync == 0)
			    status_report(sp, "mark %d at sample %" print_mus_long " (%3f secs): %3f", 
						 mark_to_int(mouse_mark), 
						 samp,
						 (double)samp / (double)(snd_srate(sp)),
						 chn_sample(samp, cp, cp->edit_ctr));
			  else
			    status_report(sp, "mark %d at sample %" print_mus_long " (%3f secs): %3f, (sync: %d)", 
						 mark_to_int(mouse_mark), 
						 samp,
						 (double)samp / (double)(snd_srate(sp)),
						 chn_sample(samp, cp, cp->edit_ctr),
						 sync);
			}
		    }
		  else
		    {
		      if (mix_tag != NO_MIX_TAG)
			{
			  Xen res = Xen_false;
			  /* the mix has already been selected by hit-mix above (to prepare drag) */
			  if (Xen_hook_has_list(mix_click_hook))
			    res = run_progn_hook(mix_click_hook,
						 Xen_list_1(new_xen_mix(mix_tag)),
						 S_mix_click_hook);
			  if (!(Xen_is_true(res)))
			    {
			      make_mix_dialog();
			      reflect_mix_change(mix_tag);
			    }
			}
		    }
		}
	    }
	  break;
	  
	case CLICK_FFT_AXIS: /* not dragged, so must have clicked close to axis */
	case CLICK_FFT_MAIN:
	  {
	    char *str;
	    str = describe_fft_point(cp, x, y);
	    set_status(sp, str, false);
	    if (str) free(str);
	  }
	  break;

#if USE_MOTIF
        case CLICK_MARK_PLAY:
          if (sp->sync != 0)
            {
              int i;
              sync_info *si;
              si = snd_sync(sp->sync);
              for (i = 0; i < si->chans; i++)
                update_graph(si->cps[i]);
            }
          else update_graph(cp);
#endif
	  
	default:
	  break;
	}
    }
  else /* dragged */
    {
      if (mouse_mark)
	{
	  finish_moving_mark(cp, mouse_mark);
	  mouse_mark = NULL;
	  dragged = false;
	}
      else
	{
	  if (mix_tag != NO_MIX_TAG)
	    {
	      finish_moving_mix_tag(mix_tag, x);
	      dragged = false;
	      mix_tag = NO_MIX_TAG;
	    }
	  else
	    {
	      cancel_selection_watch();
	      finish_selection_creation();
	      dragged = false;
	      if (show_selection_transform(ss)) 
		{
		  if (sp->sync)
		    for_each_normal_chan_with_int(calculate_syncd_fft, sp->sync);
		  else calculate_fft(cp);
		}
	    }
	}
    }
}


void graph_button_motion_callback(chan_info *cp, int x, int y, oclock_t time)
{
  /* this refers to mouse drag, not just any motion */
  snd_info *sp;
  oclock_t mouse_time;
  /* this needs to be a little slow about deciding that we are dragging, as opposed to a slow click */

  mouse_time = time;
  if ((mouse_time - mouse_down_time) < ss->click_time) return;

#ifdef __APPLE__
  /* on the Mac, we seem to get motion events even without any motion, and the times seem very short */
  if ((x == press_x) && (y == press_y)) return;
#endif

  sp = cp->sound;
  if ((cp->active < CHANNEL_HAS_AXES) || (!sp)) return; /* autotest silliness */

  if (sp->channel_style == CHANNELS_COMBINED) /* in united chans, dragging mark shouldn't change channel */
    {
      if (dragged_cp)
	cp = dragged_cp;
      else cp = which_channel(sp, y);
    }

  if (mouse_mark)
    {
      move_mark(cp, mouse_mark, x);
      /* if mark_drag_hook has printed info to the status_area, we shouldn't erase it here! */
      if (!ss->squelch_mark_drag_info) 
	status_report(sp, "%.4f", ungrf_x(cp->axis, x));
      dragged = true;
      return;
    }

  switch (click_within_graph)
    {
    case CLICK_MIX:
      /* printing the new position in the status area here is distracting and unnecessary 
       *   and the documentation (extsnd.html) has a mix-drag-hook function to print the position.
       */
      move_mix_tag(mix_tag, x, y);
      dragged = true;
      break;

    case CLICK_MARK:
      if ((dragged) &&
	  (!ss->squelch_mark_drag_info))
	status_report(sp, "%.4f", ungrf_x(cp->axis, x));
      dragged = true;
      break;
      
    case CLICK_INSET_GRAPH:
    case CLICK_SELECTION_LEFT:
    case CLICK_SELECTION_RIGHT:
    case CLICK_SELECTION_MAIN:
    case CLICK_WAVE:
      if (dragged) 
	status_report(sp, "%.4f", ungrf_x(cp->axis, x));
      
      if (!dragged) 
	{
	  /* somehow... if there is already a selection, be reluctant to clobber it */
	  start_selection_creation(cp, snd_round_mus_long_t(ungrf_x(cp->axis, x) * snd_srate(sp)));
	}
      else 
	{
	  update_possible_selection_in_progress(snd_round_mus_long_t(ungrf_x(cp->axis, x) * snd_srate(sp)));
	  move_selection(cp, x);
	}

      dragged = true;
      break;

    case CLICK_FFT_AXIS:
      {
	/* change spectrum_end(ss) and redisplay fft */
	/*   changed 25-Oct-07 -- follow sync and separate chan */
	mus_float_t new_cutoff;
	if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	  {
#if HAVE_GL
	    if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) &&
		(cp->fft->axis->used_gl))
	      {
		mus_float_t ny;
		ny = unproject_to_y(x, y);
		new_cutoff = cp->spectrum_end + (fft_faxis_start - ny);
		fft_faxis_start = ny;
	      }
	    else
#endif 
	      new_cutoff = cp->spectrum_end + ((mus_float_t)(fft_axis_start - x) / fft_axis_extent(cp));
	    fft_axis_start = x;
	  }
	else 
	  {
	    new_cutoff = cp->spectrum_end + ((mus_float_t)(y - fft_axis_start) / fft_axis_extent(cp));
	    fft_axis_start = y;
	  }
	if (new_cutoff > 1.0) new_cutoff = 1.0;
	if (new_cutoff < 0.001) new_cutoff = 0.001;
	if (sp->sync != 0)
	  {
	    int i;
	    sync_info *si;
	    si = snd_sync(sp->sync);
	    for (i = 0; i < si->chans; i++) 
	      {
		si->cps[i]->spectrum_end = new_cutoff;
		if (cp->transform_graph_type != GRAPH_ONCE)
		  sono_update(si->cps[i]);
		else update_graph(si->cps[i]);
	      }
	    free_sync_info(si);
	  }
	else
	  {
	    cp->spectrum_end = new_cutoff;
	    if (cp->transform_graph_type != GRAPH_ONCE)
	      sono_update(cp);
	    else update_graph(cp);
	  }
      }
      break;
      
    case CLICK_LISP:
      if (Xen_hook_has_list(mouse_drag_hook))
	run_hook(mouse_drag_hook,
		 Xen_list_6(C_int_to_Xen_sound(cp->sound->index),
			    C_int_to_Xen_integer(cp->chan),
			    C_int_to_Xen_integer(-1),
			    C_int_to_Xen_integer(-1),
			    C_double_to_Xen_real(ungrf_x(cp->lisp_info->axis, x)),
			    C_double_to_Xen_real(ungrf_y(cp->lisp_info->axis, y))),
		 S_mouse_drag_hook);
      break;
      
    case CLICK_FFT_MAIN:
      if (cp->with_verbose_cursor)
	{
	  char *str;
	  str = describe_fft_point(cp, x, y);
	  set_status(cp->sound, str, false);
	  if (str) free(str);
	}
      break;
      
    default:
      break;
    }
}


void channel_resize(chan_info *cp)
{
  snd_info *sp;
  if ((!cp) || (cp->active < CHANNEL_HAS_AXES) || (!cp->sound)) return;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      if (cp->chan == 0)
	for_each_sound_chan(sp, update_graph_or_warn);
    }
  else update_graph_or_warn(cp);
}


void edit_history_select(chan_info *cp, int row)
{
#if WITH_RELATIVE_PANES || USE_GTK
  if (cp->sound->channel_style != CHANNELS_SEPARATE)
    {
      uint32_t k;
      snd_info *sp;
      chan_info *ncp = NULL;
      sp = cp->sound;
      for (k = 1; k < sp->nchans; k++)
	if (sp->chans[k]->edhist_base > row)
	  {
	    ncp = sp->chans[k - 1];
	    break;
	  }
      if (!ncp) ncp = sp->chans[sp->nchans - 1];
      undo_edit_with_sync(ncp, ncp->edit_ctr - row + ncp->edhist_base);
      goto_graph(ncp);
    }
  else 
#endif
    {
      undo_edit_with_sync(cp, cp->edit_ctr - row);
      goto_graph(cp);
    }
}


widget_t channel_to_widget(chan_info *cp)
{
  if ((cp->chan > 0) && 
      (cp->sound->channel_style != CHANNELS_SEPARATE))
    return(channel_graph(cp->sound->chans[0]));
  return(channel_graph(cp));
}


chan_info *channel_to_chan(chan_info *cp)
{
  if ((cp->chan > 0) && 
      (cp->sound->channel_style != CHANNELS_SEPARATE))
    return(cp->sound->chans[0]);
  return(cp);
}


graphics_context *set_context(chan_info *cp, chan_gc_t gc)
{
  graphics_context *ax;
  chan_info *draw_cp;

  draw_cp = channel_to_chan(cp);
  ax = draw_cp->ax;

  if (cp->selected)
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = ss->selected_basic_gc;        break;
	case CHAN_IGC: ax->gc = ss->selected_erase_gc;       break;
	case CHAN_SELGC: ax->gc = ss->selected_selection_gc; break;
	case CHAN_CGC: ax->gc = ss->selected_cursor_gc;      break;
	case CHAN_MGC: ax->gc = ss->selected_mark_gc;        break;
	case CHAN_MXGC: ax->gc = ss->mix_gc;                 break;
	case CHAN_TMPGC: ax->gc = ss->selected_basic_gc;     break;
	}
    }
  else
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = ss->basic_gc;             break;
	case CHAN_IGC: ax->gc = ss->erase_gc;            break;
	case CHAN_SELGC: ax->gc = ss->selection_gc;      break;
	case CHAN_CGC: ax->gc = ss->cursor_gc;           break;
	case CHAN_MGC: ax->gc = ss->mark_gc;             break;
	case CHAN_MXGC: ax->gc = ss->mix_gc;             break;
	case CHAN_TMPGC: ax->gc = ss->combined_basic_gc; break;
	}
    }
  return(ax);
}


graphics_context *copy_context(chan_info *cp)            {return(set_context(cp, CHAN_GC));}
graphics_context *erase_context(chan_info *cp)           {return(set_context(cp, CHAN_IGC));}
graphics_context *selection_context(chan_info *cp)       {return(set_context(cp, CHAN_SELGC));}
graphics_context *cursor_context(chan_info *cp)          {return(set_context(cp, CHAN_CGC));}
graphics_context *mark_tag_context(chan_info *cp)        {return(set_context(cp, CHAN_MGC));}
graphics_context *mix_waveform_context(chan_info *cp)    {return(set_context(cp, CHAN_MXGC));}
static graphics_context *combined_context(chan_info *cp) {return(set_context(cp, CHAN_TMPGC));}



/* ---------------------------------------- smpte label ---------------------------------------- */

static void show_smpte_label(chan_info *cp, graphics_context *cur_ax)
{
#if (!USE_NO_GUI)
  #define SMPTE_FRAMPLES_PER_SECOND 24.0
  if (cp->graph_time_on)
    {
      int grf_x, grf_y, grf_width, grf_height;
      /* is there room for a label? */

      grf_x = cp->axis->x_axis_x0;
      grf_y = cp->axis->y_axis_y1;
      grf_width = cp->axis->x_axis_x1 - grf_x;
      grf_height = cp->axis->y_axis_y0 - grf_y;

      if ((grf_width > 100) &&
	  (grf_height > 20))
	{
	  bool try_tiny_font;
	  int width, height, framples, seconds, minutes, hours;
	  double secs;
	  char num_buf[3];
	  static char label[12] = "00:00:00:00";

	  try_tiny_font = (grf_height < 50);
	  width = 8 + number_width(label, try_tiny_font);
	  height = 8 + number_height((try_tiny_font) ? TINY_FONT(ss) : AXIS_NUMBERS_FONT(ss));
	  
	  secs = cp->axis->x0;
	  hours = floor(secs / 3600.0);
	  secs -= hours * 3600;
	  minutes = floor(secs / 60.0);
	  secs -= minutes * 60;
	  seconds = floor(secs);
	  framples = (secs - seconds) * SMPTE_FRAMPLES_PER_SECOND;
	  
	  snprintf(num_buf, 3, "%d", hours);
	  if (hours > 9) {label[0] = num_buf[0]; label[1] = num_buf[1];} else {label[0] = '0'; label[1] = num_buf[0];}
	  snprintf(num_buf, 3, "%d", minutes);
	  if (minutes > 9) {label[3] = num_buf[0]; label[4] = num_buf[1];} else {label[3] = '0'; label[4] = num_buf[0];}
	  snprintf(num_buf, 3, "%d", seconds);
	  if (seconds > 9) {label[6] = num_buf[0]; label[7] = num_buf[1];} else {label[6] = '0'; label[7] = num_buf[0];}
	  snprintf(num_buf, 3, "%d", framples);
	  if (framples > 9) {label[9] = num_buf[0]; label[10] = num_buf[1];} else {label[9] = '0'; label[10] = num_buf[0];}

	  fill_rectangle(cur_ax, grf_x, grf_y, width, 2);
	  fill_rectangle(cur_ax, grf_x, grf_y + height, width, 2);
	  fill_rectangle(cur_ax, grf_x, grf_y, 2, height);
	  fill_rectangle(cur_ax, grf_x + width - 2, grf_y, 2, height);
	  
	  set_numbers_font(cur_ax, NOT_PRINTING, try_tiny_font);
#if USE_MOTIF
	  grf_y += number_height((try_tiny_font) ? TINY_FONT(ss) : AXIS_NUMBERS_FONT(ss));
#else
	  grf_y++;
#endif
	  draw_string(cur_ax, grf_x + 4, grf_y + 4, label, 11);
	 }
    }
#endif
}


/* ---------------------------------------- inset graph ---------------------------------------- */

#define INSET_WIDTH .2
#define INSET_HEIGHT .25

void clear_inset_graph(chan_info *cp)
{
  if (cp->inset_graph)
    cp->inset_graph->edpos = -2;
}


void free_inset_graph(chan_info *cp)
{
  if (cp->inset_graph)
    {
      if (cp->inset_graph->data0) free(cp->inset_graph->data0);
      if (cp->inset_graph->data1) free(cp->inset_graph->data1);
      free(cp->inset_graph);
      cp->inset_graph = NULL;
    }
}


#if (!USE_NO_GUI)
static void make_point_arrays(inset_graph_info_t *info, int size, vct *v1)
{
  if (size != info->data_size)
    {
      if (info->data0) free(info->data0);
      if (info->data1) free(info->data1);
      info->data1 = NULL;
      info->data0 = (point_t *)calloc(size, sizeof(point_t));
    }

  if ((v1) && (!(info->data1)))
    info->data1 = (point_t *)calloc(size, sizeof(point_t));

  info->data_size = size;
}


static void update_inset_axes(chan_info *cp, inset_graph_info_t *info, graphics_context *cur_ax)
{
  char *str;
  int len, num_hgt = -1;
  axis_info *ap;
  ap = cp->axis;
  str = prettyf(ap->xmax, 2);
  if (str)
    {
      num_hgt = number_height(TINY_FONT(ss));
      len = strlen(str);
      set_tiny_numbers_font(cp, cur_ax);
#if (!USE_GTK)
      draw_string(cur_ax, info->x1 - 6 * len + 10, info->y1 + num_hgt, str, len);
#else
      draw_string(cur_ax, info->x1 - 6 * len + 10, info->y1 + (num_hgt / 2) - 2, str, len);
#endif
      free(str);
    }
  
  if (info->maxamp > 0.0)
    {
      str = prettyf(info->maxamp, (info->maxamp > .1) ? 2 : ((info->maxamp > .01) ? 3 : 4));
      if (str)
	{
	  len = strlen(str);
	  set_tiny_numbers_font(cp, cur_ax);
#if (!USE_GTK)
	  if (num_hgt == -1) num_hgt = number_height(TINY_FONT(ss));
	  draw_string(cur_ax, info->x0 - 6 * len - 2, info->y0 + (num_hgt / 2), str, len);
#else
	  draw_string(cur_ax, info->x0 - 6 * len - 2, info->y0, str, len);
#endif
	  free(str);
	}
    }
}
#endif


static void show_inset_graph(chan_info *cp, graphics_context *cur_ax)
{
#if (!USE_NO_GUI)
  if (cp->graph_time_on)
    {
      int grf_width, width, x_offset, y_offset, grf_height, height, chan_offset;
      bool new_peaks;
      inset_graph_info_t *info;
      mus_long_t framples;

      if (!(cp->inset_graph))
	{
	  cp->inset_graph = (inset_graph_info_t *)calloc(1, sizeof(inset_graph_info_t));
	  cp->inset_graph->edpos = -2;
	}

      info = cp->inset_graph;
      grf_width = cp->axis->x_axis_x1;
      width = snd_round(INSET_WIDTH * grf_width);
      x_offset = grf_width - width;

      grf_height = cp->axis->y_axis_y0 - cp->axis->y_axis_y1;
      height = snd_round(INSET_HEIGHT * grf_height);
      chan_offset = cp->axis->y_axis_y1 - 6;
      if ((cp->show_axes == SHOW_X_AXIS) || (cp->show_axes == SHOW_X_AXIS_UNLABELLED))
	chan_offset += 10;
      y_offset = chan_offset + snd_round(height * 0.5);

      new_peaks = ((cp->axis->cp) && (cp->axis->cp->new_peaks));
      /* new_peaks is set during update_graph if we just finished a new peak-env */
      framples = current_samples(cp);

      if ((width > 10) &&
	  (height > 10) &&
	  (framples > 0) &&
	  ((cp->chan == 0) || (cp->sound->channel_style != CHANNELS_SUPERIMPOSED)))
	{
	  /* draw axes around the inset graph */
	  fill_rectangle(cur_ax, x_offset, chan_offset + height, width, 2);
	  fill_rectangle(cur_ax, x_offset, chan_offset, 2, height);

	  info->x0 = x_offset;
	  info->x1 = x_offset + width;
	  info->y0 = chan_offset;
	  info->y1 = chan_offset + height;
	  
	  /* show where the current window fits into the overall graph */
	  {
	    int rx, lx, wx;
	    rx = snd_round(width * (double)(cp->axis->hisamp) / (double)framples);
	    lx = snd_round(width * (double)(cp->axis->losamp) / (double)framples);
#if USE_GTK
	    if (lx < 2) lx = 2; /* don't erase the y axis */
#endif
	    wx = rx - lx;
	    if (wx <= 0) wx = 1;

	    if (cp->selected)
	      {
		cur_ax->gc = ss->selected_selection_gc;
		fill_rectangle(cur_ax, x_offset + lx, chan_offset, wx, height);
		cur_ax->gc = ss->selected_basic_gc;
	      }
	    else
	      {
		cur_ax->gc = ss->selection_gc;
		fill_rectangle(cur_ax, x_offset + lx, chan_offset, wx, height);
		cur_ax->gc = ss->basic_gc;
	      }
	  }

	  if ((!new_peaks) &&
	      (width == info->width) &&
	      (height == info->height) &&
	      (y_offset == info->y_offset) &&
	      (cp->edit_ctr == info->edpos))
	    {
	      /* use old env graph points if nothing has changed */
	      update_inset_axes(cp, info, cur_ax);
	    }
	  else
	    {
	      /* need to get new peak env graph points */
	      Xen data;
	      double data_max = 0.0, data_scaler, step;
	      vct *v0 = NULL, *v1 = NULL;
	      mus_float_t *v0data = NULL, *v1data = NULL;
	      int data_len;
#if HAVE_SCHEME
	      s7_int gc_loc;
#endif

	      data = make_graph_data(cp, cp->edit_ctr, 0, framples);
	      if (Xen_is_false(data)) return;
#if HAVE_SCHEME
	      gc_loc = s7_gc_protect(s7, data);
#endif
	      if (mus_is_vct(data))
		v0 = xen_to_vct(data);
	      else
		{
		  v0 = xen_to_vct(Xen_car(data));
		  v1 = xen_to_vct(Xen_cadr(data));
		}

	      v0data = mus_vct_data(v0);
	      if (v1) v1data = mus_vct_data(v1);

	      data_max = mus_vct_peak(v0);
	      if (v1)
		{
		  double data1_max;
		  data1_max = mus_vct_peak(v1);
		  if (data1_max > data_max) data_max = data1_max;
		}
	      
	      if (data_max > 0.0)
		data_scaler = (double)height / (2 * data_max);
	      else data_scaler = 0.0;
	      info->maxamp = data_max;

	      data_len = mus_vct_length(v0);
	      step = (double)data_len / (double)width;

	      if (data_scaler < 0.00000001)
		{
		  /* load up 0's */
		  int i, j;
		  
		  make_point_arrays(info, width, v1);

		  for (j = x_offset, i = 0; i < width; i++, j++)
		    {
		      info->data0[i].x = j;
		      info->data0[i].y = y_offset;
		      if (v1)
			{
			  info->data1[i].x = j;
			  info->data1[i].y = y_offset;
			}
		    }
		}
	      else
		{
		  if (data_len > width)
		    {
		      /* normal case: more samples to display than we have room for, so subsample */
		      int i, j;
		      double max_y, min_y, stepper = 0.0;
		      max_y = -data_max;
		      min_y = data_max;
		      
		      make_point_arrays(info, width, v1);
		      
		      for (i = 0, j = 0; (i < data_len) && (j < width); i++)
			{
			  if (v1)
			    {
			      if (v1data[i] > max_y) max_y = v1data[i];
			      if (v0data[i] < min_y) min_y = v0data[i];
			    }
			  else
			    {
			      if (v0data[i] > max_y) max_y = v0data[i];
			    }
			  stepper += 1.0;
			  if (stepper >= step)
			    {
			      info->data0[j].x = x_offset;
			      info->data0[j].y = snd_round(y_offset - max_y * data_scaler);
			      max_y = -data_max;
			      if (v1)
				{
				  info->data1[j].x = x_offset;
				  info->data1[j].y = snd_round(y_offset - min_y * data_scaler);
				  min_y = data_max;
				}
			      x_offset++;
			      stepper -= step;
			      j++;
			    }
			}

		      while (j < width)
			{
			  info->data0[j].x = info->data0[j - 1].x;
			  info->data0[j].y = info->data0[j - 1].y;
			  if (v1) 
			    {
			      info->data1[j].x = info->data1[j - 1].x;
			      info->data1[j].y = info->data1[j - 1].y;
			    }
			  j++;
			}
		    }
		  else
		    {
		      /* more pixels than samples */
		      double xstep, xj;
		      int i;
		      xstep = (double)width / (double)data_len;
		      xj = (double)x_offset;

		      make_point_arrays(info, data_len, v1);

		      for (i = 0; i < data_len; i++, xj += xstep)
			{
			  info->data0[i].x = snd_round(xj);
			  if (!v1)
			    info->data0[i].y = snd_round(y_offset - data_scaler * v0data[i]);
			  else
			    {
			      info->data0[i].y = snd_round(y_offset - data_scaler * v1data[i]);
			      info->data1[i].x = info->data0[i].x;
			      info->data1[i].x = snd_round(y_offset - data_scaler * v0data[i]);
			    }
			}
		    }
		}

	      info->width = width;
	      info->height = height;
	      info->edpos = cp->edit_ctr;
	      info->y_offset = y_offset;
	      update_inset_axes(cp, info, cur_ax);
#if HAVE_SCHEME
	      s7_gc_unprotect_at(s7, gc_loc);
#endif
	    }

	  draw_lines(cur_ax, info->data0, info->data_size);
	  if (info->data1) draw_lines(cur_ax, info->data1, info->data_size);
	  info->graphing = true;
	}
      else info->graphing = false;
    }
#endif
}


void draw_inset_line_cursor(chan_info *cp, graphics_context *ax)
{
  /* we've checked that with_inset_graph is #t and cp has the pointer */
  if ((cp->inset_graph->graphing) &&
      (cp->cx > cp->inset_graph->x0) &&
      (cp->cx < cp->inset_graph->x1))
    draw_line(ax, cp->cx, cp->axis->y_axis_y0 - 1, cp->cx, cp->inset_graph->y1 + 4);
  else draw_line(ax, cp->cx, cp->axis->y_axis_y0 - 1, cp->cx, cp->axis->y_axis_y1);
}


/* -------------------------------------------------------------------------------- */

typedef enum {CP_GRAPH_TRANSFORM_ON, CP_GRAPH_TIME_ON, CP_FRAMPLES, CP_CURSOR, CP_GRAPH_LISP_ON, CP_LOSAMP, CP_HISAMP, CP_SQUELCH_UPDATE,
	      CP_EDIT_CTR, CP_CURSOR_STYLE, CP_EDIT_HOOK, CP_UNDO_HOOK, CP_AFTER_EDIT_HOOK,
	      CP_SHOW_Y_ZERO, CP_SHOW_MARKS, CP_TIME_GRAPH_TYPE, CP_WAVO_HOP, CP_WAVO_TRACE, CP_MAX_TRANSFORM_PEAKS, 
	      CP_SHOW_TRANSFORM_PEAKS, CP_ZERO_PAD, CP_WITH_VERBOSE_CURSOR, CP_FFT_LOG_FREQUENCY, CP_FFT_LOG_MAGNITUDE,
	      CP_WAVELET_TYPE, CP_SPECTRO_HOP, CP_TRANSFORM_SIZE, CP_TRANSFORM_GRAPH_TYPE, CP_FFT_WINDOW, CP_TRANSFORM_TYPE,
	      CP_TRANSFORM_NORMALIZATION, CP_SHOW_MIX_WAVEFORMS, CP_TIME_GRAPH_STYLE, CP_LISP_GRAPH_STYLE, CP_TRANSFORM_GRAPH_STYLE, CP_DOT_SIZE,
	      CP_SHOW_AXES, CP_GRAPHS_HORIZONTAL, CP_CURSOR_SIZE, CP_CURSOR_POSITION,
	      CP_EDPOS_FRAMPLES, CP_X_AXIS_STYLE, CP_UPDATE_TIME, CP_UPDATE_TRANSFORM_GRAPH, CP_UPDATE_LISP, CP_PROPERTIES,
	      CP_MIN_DB, CP_SPECTRO_X_ANGLE, CP_SPECTRO_Y_ANGLE, CP_SPECTRO_Z_ANGLE, CP_SPECTRO_X_SCALE, CP_SPECTRO_Y_SCALE, CP_SPECTRO_Z_SCALE,
	      CP_SPECTRUM_END, CP_SPECTRUM_START, CP_FFT_WINDOW_BETA, CP_SX, CP_SY, CP_ZX, CP_ZY, CP_MAXAMP, CP_EDPOS_MAXAMP,
	      CP_BEATS_PER_MINUTE, CP_EDPOS_CURSOR, CP_SHOW_GRID, CP_SHOW_SONOGRAM_CURSOR, CP_GRID_DENSITY, CP_MAXAMP_POSITION,
	      CP_EDPOS_MAXAMP_POSITION, CP_BEATS_PER_MEASURE, CP_FFT_WINDOW_ALPHA, CP_TRACKING_CURSOR_STYLE, CP_FFT_WITH_PHASES
} cp_field_t;


static Xen cp_edpos;
static int cp_edpos_loc = NOT_A_GC_LOC;

static Xen channel_get(Xen snd, Xen chn_n, cp_field_t fld, const char *caller)
{
  chan_info *cp;
  snd_info *sp = NULL;
  int i;
  Xen res = Xen_empty_list;
  if (Xen_is_true(snd))
    {
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res = Xen_cons(channel_get(C_int_to_Xen_integer(i), chn_n, fld, caller), res);
	  /* I think not SOUND_WRAPPER here -- get/set would then be operating on sounds that
	   *   are not returned by 'sounds' and return #f from 'sound?', so it would almost
	   *   certainly cause confusion.  The globals fields should be reflected however.
	   */
	}
      return(res);
    }
  else
    {
      if (Xen_is_true(chn_n))
	{
	  sp = get_sp(snd);
	  if (!sp)
	    return(snd_no_such_sound_error(caller, snd));
	  for (i = (int)sp->nchans - 1; i >= 0; i--)
	    res = Xen_cons(channel_get(snd, C_int_to_Xen_integer(i), fld, caller), res);
	  return(res);
	}
      else
	{
	  Snd_assert_channel(caller, snd, chn_n, 1);
	  cp = get_cp(snd, chn_n, caller);
	  if (!cp) return(Xen_false); /* perhaps snd-error-hook cancelled the error? */
	  switch (fld)
	    {
	    case CP_EDIT_CTR:                return(C_int_to_Xen_integer(cp->edit_ctr));                
	    case CP_GRAPH_TRANSFORM_ON:      return(C_bool_to_Xen_boolean(cp->graph_transform_on));     
	    case CP_GRAPH_TIME_ON:           return(C_bool_to_Xen_boolean(cp->graph_time_on));          
	    case CP_CURSOR:                  return(C_llong_to_Xen_llong(cursor_sample(cp)));           
	    case CP_EDPOS_CURSOR:            return(C_llong_to_Xen_llong(cp->edits[to_c_edit_position(cp, cp_edpos, S_cursor, 3)]->cursor));
	    case CP_FRAMPLES:                return(C_llong_to_Xen_llong(current_samples(cp)));           
	    case CP_GRAPH_LISP_ON:           return(C_bool_to_Xen_boolean(cp->graph_lisp_on));            
	    case CP_LOSAMP:                  if (cp->axis) return(C_llong_to_Xen_llong(cp->axis->losamp)); break;
	    case CP_HISAMP:                  if (cp->axis) return(C_llong_to_Xen_llong(cp->axis->hisamp)); break;
	    case CP_SQUELCH_UPDATE:          return(C_bool_to_Xen_boolean(cp->squelch_update));           
	    case CP_CURSOR_SIZE:             return(C_int_to_Xen_integer(cp->cursor_size));               

	    case CP_CURSOR_STYLE:
	      if (cp->cursor_style != CURSOR_PROC)
		return(C_int_to_Xen_integer((int)(cp->cursor_style)));
	      if (Xen_is_procedure(cp->cursor_proc))
		return(cp->cursor_proc);
	      return(ss->cursor_proc);

	    case CP_TRACKING_CURSOR_STYLE:
	      return(C_int_to_Xen_integer((int)(cp->tracking_cursor_style)));

	    case CP_EDIT_HOOK:
	      if (!(Xen_is_hook(cp->edit_hook)))
		{
		  cp->edit_hook = Xen_define_simple_hook("(make-hook)", 0);
		  cp->edit_hook_loc = snd_protect(cp->edit_hook);
		}
	      return(cp->edit_hook);

	    case CP_AFTER_EDIT_HOOK:
	      if (!(Xen_is_hook(cp->after_edit_hook)))
		{
		  cp->after_edit_hook = Xen_define_simple_hook("(make-hook)", 0);
		  cp->after_edit_hook_loc = snd_protect(cp->after_edit_hook);
		}
	      return(cp->after_edit_hook);

	    case CP_UNDO_HOOK:               
	      if (!(Xen_is_hook(cp->undo_hook)))
		{
		  cp->undo_hook = Xen_define_simple_hook("(make-hook)", 0);
		  cp->undo_hook_loc = snd_protect(cp->undo_hook);
		}
	      return(cp->undo_hook);

	    case CP_SHOW_Y_ZERO:             return(C_bool_to_Xen_boolean(cp->show_y_zero));                          
	    case CP_SHOW_GRID:               return(C_bool_to_Xen_boolean((bool)(cp->show_grid)));                    
	    case CP_GRID_DENSITY:            return(C_double_to_Xen_real(cp->grid_density));                          
	    case CP_SHOW_SONOGRAM_CURSOR:    return(C_bool_to_Xen_boolean((bool)(cp->show_sonogram_cursor)));         
	    case CP_SHOW_MARKS:              return(C_bool_to_Xen_boolean(cp->show_marks));                           
	    case CP_TIME_GRAPH_TYPE:         return(C_int_to_Xen_integer((int)(cp->time_graph_type)));                
	    case CP_WAVO_HOP:                return(C_int_to_Xen_integer(cp->wavo_hop));                              
	    case CP_WAVO_TRACE:              return(C_int_to_Xen_integer(cp->wavo_trace));                            
	    case CP_MAX_TRANSFORM_PEAKS:     return(C_int_to_Xen_integer(cp->max_transform_peaks));                   
	    case CP_ZERO_PAD:                return(C_int_to_Xen_integer(cp->zero_pad));                              
	    case CP_WAVELET_TYPE:            return(C_int_to_Xen_integer(cp->wavelet_type));                          
	    case CP_SHOW_TRANSFORM_PEAKS:    return(C_bool_to_Xen_boolean(cp->show_transform_peaks));                 
	    case CP_WITH_VERBOSE_CURSOR:     return(C_bool_to_Xen_boolean(cp->with_verbose_cursor));                  
	    case CP_FFT_LOG_FREQUENCY:       return(C_bool_to_Xen_boolean(cp->fft_log_frequency));                    
	    case CP_FFT_LOG_MAGNITUDE:       return(C_bool_to_Xen_boolean(cp->fft_log_magnitude));                    
	    case CP_FFT_WITH_PHASES:         return(C_bool_to_Xen_boolean(cp->fft_with_phases));                      
	    case CP_SPECTRO_HOP:             return(C_int_to_Xen_integer(cp->spectro_hop));                           
	    case CP_TRANSFORM_SIZE:          return(C_llong_to_Xen_llong(cp->transform_size));                        
	    case CP_TRANSFORM_GRAPH_TYPE:    return(C_int_to_Xen_integer((int)(cp->transform_graph_type)));           
	    case CP_FFT_WINDOW:              return(C_int_to_Xen_integer((int)(cp->fft_window)));                     
	    case CP_TRANSFORM_TYPE:          return(C_int_to_Xen_transform(cp->transform_type));                      
	    case CP_TRANSFORM_NORMALIZATION: return(C_int_to_Xen_integer((int)(cp->transform_normalization)));        
	    case CP_SHOW_MIX_WAVEFORMS:      return(C_bool_to_Xen_boolean(cp->show_mix_waveforms));                   
	    case CP_TIME_GRAPH_STYLE:        return(C_int_to_Xen_integer(cp->time_graph_style));                      
	    case CP_LISP_GRAPH_STYLE:        return(C_int_to_Xen_integer(cp->lisp_graph_style));                      
	    case CP_TRANSFORM_GRAPH_STYLE:   return(C_int_to_Xen_integer(cp->transform_graph_style));                 
	    case CP_X_AXIS_STYLE:            return(C_int_to_Xen_integer((int)(cp->x_axis_style)));                   
	    case CP_DOT_SIZE:                return(C_int_to_Xen_integer(cp->dot_size));                              
	    case CP_SHOW_AXES:               return(C_int_to_Xen_integer((int)(cp->show_axes)));                      
	    case CP_GRAPHS_HORIZONTAL:       return(C_bool_to_Xen_boolean(cp->graphs_horizontal));                    
	    case CP_CURSOR_POSITION:         return(Xen_list_2(C_int_to_Xen_integer(cp->cx), C_int_to_Xen_integer(cp->cy)));
	    case CP_EDPOS_FRAMPLES:          return(C_llong_to_Xen_llong(to_c_edit_samples(cp, cp_edpos, caller, 3))); 

	    case CP_UPDATE_TIME:
	      /* any display-oriented background process must first be run to completion
	       *       display checks for waiting process and does not update display if one found!
	       */
	      finish_peak_env(cp);
	      cp->waiting_to_make_graph = false;
	      display_channel_time_data(cp);
	      break;

	    case CP_UPDATE_LISP:
	      display_channel_lisp_data(cp);
	      break;

	    case CP_UPDATE_TRANSFORM_GRAPH: 
	      if (cp->graph_transform_on)
		{
		  if (chan_fft_in_progress(cp)) 
		    force_fft_clear(cp);
		  
		  ss->checking_explicitly = true;  /* do not allow UI events to intervene here! */
		  if (cp->transform_graph_type == GRAPH_ONCE)
		    single_fft(cp, FORCE_REDISPLAY, FORCE_REFFT);
		  else
		    {
		      void *val;
		      val = (void *)make_sonogram_state(cp, FORCE_REFFT);
		      while (sonogram_in_slices(val) == BACKGROUND_CONTINUE) ;
		    }
		  ss->checking_explicitly = false;
		}
	      break;

	    case CP_PROPERTIES:
	      if (!(Xen_is_vector(cp->properties)))
		{
		  cp->properties = Xen_make_vector(1, Xen_empty_list);
		  cp->properties_loc = snd_protect(cp->properties);
		}
	      return(Xen_vector_ref(cp->properties, 0));

	    case CP_SX:               if (cp->axis) return(C_double_to_Xen_real(cp->axis->sx)); break;
	    case CP_SY:               if (cp->axis) return(C_double_to_Xen_real(cp->axis->sy)); break;
	    case CP_ZX:               if (cp->axis) return(C_double_to_Xen_real(cp->axis->zx)); break;
	    case CP_ZY:               if (cp->axis) return(C_double_to_Xen_real(cp->axis->zy)); break;
	    case CP_MIN_DB:           return(C_double_to_Xen_real(cp->min_dB));                 
	    case CP_SPECTRO_X_ANGLE:  return(C_double_to_Xen_real(cp->spectro_x_angle));        
	    case CP_SPECTRO_Y_ANGLE:  return(C_double_to_Xen_real(cp->spectro_y_angle));        
	    case CP_SPECTRO_Z_ANGLE:  return(C_double_to_Xen_real(cp->spectro_z_angle));        
	    case CP_SPECTRO_X_SCALE:  return(C_double_to_Xen_real(cp->spectro_x_scale));        
	    case CP_SPECTRO_Y_SCALE:  return(C_double_to_Xen_real(cp->spectro_y_scale));        
	    case CP_SPECTRO_Z_SCALE:  return(C_double_to_Xen_real(cp->spectro_z_scale));        
	    case CP_SPECTRUM_END:     return(C_double_to_Xen_real(cp->spectrum_end));           
	    case CP_SPECTRUM_START:   return(C_double_to_Xen_real(cp->spectrum_start));         
	    case CP_FFT_WINDOW_ALPHA: return(C_double_to_Xen_real(cp->fft_window_alpha));       
	    case CP_FFT_WINDOW_BETA:  return(C_double_to_Xen_real(cp->fft_window_beta));        
	    case CP_BEATS_PER_MINUTE: return(C_double_to_Xen_real(cp->beats_per_minute));       
	    case CP_BEATS_PER_MEASURE: return(C_int_to_Xen_integer(cp->beats_per_measure));     
	    case CP_MAXAMP:           return(C_double_to_Xen_real(channel_maxamp(cp, AT_CURRENT_EDIT_POSITION)));
	    case CP_EDPOS_MAXAMP:     return(C_double_to_Xen_real(channel_maxamp(cp, to_c_edit_position(cp, cp_edpos, S_maxamp, 3))));
	    case CP_MAXAMP_POSITION:  return(C_llong_to_Xen_llong(channel_maxamp_position(cp, AT_CURRENT_EDIT_POSITION))); 
	    case CP_EDPOS_MAXAMP_POSITION: return(C_llong_to_Xen_llong(channel_maxamp_position(cp, to_c_edit_position(cp, cp_edpos, S_maxamp_position, 3)))); 
	    }
	}
    }
  return(Xen_false);
}


static int g_imin(int mn, Xen val, int def)
{
  int nval;
  if (Xen_is_integer(val)) nval = Xen_integer_to_C_int(val); else nval = def;
  if (nval >= mn) return(nval);
  return(mn);
}


static int g_omin(mus_long_t mn, Xen val, mus_long_t def)
{
  mus_long_t nval;
  if (Xen_is_llong(val)) nval = Xen_llong_to_C_llong(val); else nval = def;
  if (nval >= mn) return(nval);
  return(mn);
}


static void reset_y_display(chan_info *cp, double sy, double zy)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sy = sy;
  ap->zy = zy;
  resize_sy(cp);
  apply_y_axis_change(cp);
}


static void chans_x_axis_style(chan_info *cp, int value);

static bool call_update_graph = true;
#define MAX_SPECTRO_SCALE 1000.0
#define MAX_SPECTRO_ANGLE 360.0
#define MIN_SPECTRO_ANGLE -360.0


static Xen channel_set(Xen snd, Xen chn_n, Xen on, cp_field_t fld, const char *caller)
{
  chan_info *cp;
  int val = 0;
  bool bval = false;
  snd_info *sp;
  int i;
  mus_float_t curamp, curf;
  Xen res = Xen_empty_list;
  if (Xen_is_true(snd))
    {
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res = Xen_cons(channel_set(C_int_to_Xen_integer(i), chn_n, on, fld, caller), res);
	}
      return(res);
    }
  if (Xen_is_true(chn_n))
    {
      sp = get_sp(snd);
      if (!sp) 
	return(snd_no_such_sound_error(caller, snd));
      for (i = (int)sp->nchans - 1; i >= 0; i--)
	res = Xen_cons(channel_set(snd, C_int_to_Xen_integer(i), on, fld, caller), res);
      return(res);
    }
  Snd_assert_channel(caller, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, caller);
  if (!cp) return(Xen_false);

  switch (fld)
    {
    case CP_EDIT_CTR:
      if (cp->editable)
	{
	  if (Xen_is_integer(on)) val = Xen_integer_to_C_int(on); else val = 0;
	  if (cp->edit_ctr < val)
	    redo_edit(cp, val - cp->edit_ctr);
	  else undo_edit(cp, cp->edit_ctr - val);
	}
      return(C_int_to_Xen_integer(cp->edit_ctr));

    case CP_GRAPH_TRANSFORM_ON:
      bval = Xen_boolean_to_C_bool(on); 
      fftb(cp, bval);
      update_graph(cp);
      return(on);

    case CP_GRAPH_TIME_ON:
      bval = Xen_boolean_to_C_bool(on);
      waveb(cp, bval);
      update_graph(cp);
      return(on);

    case CP_CURSOR:
      {
	mus_long_t samp = 0;
	if (Xen_is_llong(on)) samp = Xen_llong_to_C_llong(on);
	if (samp < 0)
	  {
	    cp->cursor_on = false;
	    update_graph(cp);
	  }
	else
	  {
	    cp->cursor_on = true; 
	    cursor_moveto(cp, samp);
	  }
	cp->original_cursor = samp; /* for snd-dac, track-and-return */
	return(C_llong_to_Xen_llong(samp));
      }

    case CP_EDPOS_CURSOR:
      {
	int pos;
	mus_long_t samp = 0;
	pos = to_c_edit_position(cp, cp_edpos, caller, 3);
	if (Xen_is_llong(on)) samp = Xen_llong_to_C_llong(on);
	if (samp < 0)
	  {
	    cp->cursor_on = false;
	    update_graph(cp);
	  }
	else
	  {
	    if (pos == cp->edit_ctr)
	      {
		cp->cursor_on = true; 
		cursor_moveto(cp, samp);
	      }
	    else cp->edits[pos]->cursor = samp;
	  }
	return(C_llong_to_Xen_llong(samp));
      }

    case CP_GRAPH_LISP_ON:
      cp->graph_lisp_on = Xen_boolean_to_C_bool(on); 
      update_graph(cp);
      return(on);

    case CP_LOSAMP:
      Xen_check_type(Xen_is_integer(on), on, 1, S_set S_left_sample, "an integer");
      set_x_axis_x0(cp, beg_to_sample(on, caller));
      return(on);

    case CP_HISAMP:
      Xen_check_type(Xen_is_integer(on), on, 1, S_set S_right_sample, "an integer");
      set_x_axis_x1(cp, beg_to_sample(on, caller));
      return(on);

    case CP_SQUELCH_UPDATE:
      cp->squelch_update = Xen_boolean_to_C_bool(on);
      if (!(cp->squelch_update))
	{
	  /* make sure everything that was squelched before is now correct */
	  reflect_edit_history_change(cp);
	  if (cp->edit_ctr == 0)
	    reflect_file_revert_in_label(cp->sound);
	  else reflect_file_change_in_label(cp);
	  clear_status_area(cp->sound);
	}
      break;

    case CP_CURSOR_SIZE:
      cp->cursor_size = Xen_integer_to_C_int(on);
      update_graph(cp); 
      return(C_int_to_Xen_integer(cp->cursor_size));

    case CP_CURSOR_STYLE:
      if (Xen_is_procedure(on))
	{
	  char *error = NULL;
	  error = procedure_ok(on, 3, S_cursor_style, "", 1);
	  if (!error)
	    {
	      if ((cp->cursor_style == CURSOR_PROC) &&
		  (Xen_is_procedure(cp->cursor_proc)))
		snd_unprotect_at(cp->cursor_proc_loc);
	      cp->cursor_proc_loc = snd_protect(on);
	      cp->cursor_proc = on;
	      cp->cursor_style = CURSOR_PROC;
	      return(on);
	    }
	  else 
	    {
	      Xen errstr;
	      errstr = C_string_to_Xen_string(error);
	      free(error);
	      return(snd_bad_arity_error(S_set S_cursor_style, errstr, on));
	    }
	}
      else
	{
	  if ((cp->cursor_style == CURSOR_PROC) &&
	      (Xen_is_procedure(cp->cursor_proc)))
	    {
	      snd_unprotect_at(cp->cursor_proc_loc);
	      cp->cursor_proc = Xen_undefined;
	      cp->cursor_proc_loc = NOT_A_GC_LOC;
	    }
	  cp->cursor_style = (cursor_style_t)Xen_integer_to_C_int(on); /* range already checked */
	}
      cp->just_zero = (cp->cursor_style == CURSOR_LINE); /* no point in displaying y value in this case */
      update_graph(cp); 
      return(C_int_to_Xen_integer((int)(cp->cursor_style)));

    case CP_TRACKING_CURSOR_STYLE:
      cp->tracking_cursor_style = (cursor_style_t)Xen_integer_to_C_int(on);
      return(C_int_to_Xen_integer((int)(cp->tracking_cursor_style)));

    case CP_SHOW_Y_ZERO:
      cp->show_y_zero = Xen_boolean_to_C_bool(on); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean(cp->show_y_zero));

    case CP_SHOW_GRID:
      cp->show_grid = (with_grid_t)(Xen_boolean_to_C_bool(on)); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean((bool)(cp->show_grid)));

    case CP_GRID_DENSITY:
      curf = Xen_real_to_C_double(on); 
      if (curf >= 0.0)
	cp->grid_density = curf;
      else Xen_out_of_range_error(S_set S_grid_density, 1, on, "density < 0.0?");
      update_graph(cp); 
      return(C_double_to_Xen_real(cp->grid_density));

    case CP_SHOW_SONOGRAM_CURSOR:
      cp->show_sonogram_cursor = Xen_boolean_to_C_bool(on); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean(cp->show_sonogram_cursor));

    case CP_SHOW_MARKS:
      cp->show_marks = Xen_boolean_to_C_bool(on); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean(cp->show_marks));

    case CP_TIME_GRAPH_TYPE:
      cp->time_graph_type = (graph_type_t)Xen_integer_to_C_int(on); /* checked already */
      update_graph(cp); 
      return(C_int_to_Xen_integer((int)(cp->time_graph_type)));

    case CP_WAVO_HOP:
      cp->wavo_hop = g_imin(1, on, DEFAULT_WAVO_HOP); 
      update_graph(cp); 
      return(C_int_to_Xen_integer(cp->wavo_hop));

    case CP_WAVO_TRACE:
      cp->wavo_trace = mus_iclamp(1, g_imin(1, on, DEFAULT_WAVO_TRACE), POINT_BUFFER_SIZE);
      update_graph(cp); 
      return(C_int_to_Xen_integer(cp->wavo_trace));

    case CP_MAX_TRANSFORM_PEAKS:
      cp->max_transform_peaks = g_imin(1, on, DEFAULT_MAX_TRANSFORM_PEAKS); 
      return(C_int_to_Xen_integer(cp->max_transform_peaks));

    case CP_ZERO_PAD:
      cp->zero_pad = mus_iclamp(0, g_imin(0, on, DEFAULT_ZERO_PAD), MAX_ZERO_PAD); 
      update_graph(cp);
      return(C_int_to_Xen_integer(cp->zero_pad));

    case CP_WAVELET_TYPE:
      cp->wavelet_type = Xen_integer_to_C_int(on); /* range checked already */
      update_graph(cp);
      return(C_int_to_Xen_integer(cp->wavelet_type));

    case CP_SHOW_TRANSFORM_PEAKS:
      cp->show_transform_peaks = Xen_boolean_to_C_bool(on); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean(cp->show_transform_peaks));

    case CP_WITH_VERBOSE_CURSOR:
      cp->with_verbose_cursor = Xen_boolean_to_C_bool(on); 
      return(C_bool_to_Xen_boolean(cp->with_verbose_cursor));

    case CP_FFT_LOG_FREQUENCY:
      cp->fft_log_frequency = Xen_boolean_to_C_bool(on); 
      if (cp->graph_transform_on) calculate_fft(cp); 
      return(C_bool_to_Xen_boolean(cp->fft_log_frequency));

    case CP_FFT_LOG_MAGNITUDE:
      cp->fft_log_magnitude = Xen_boolean_to_C_bool(on); 
      if (cp->graph_transform_on) calculate_fft(cp); 
      return(C_bool_to_Xen_boolean(cp->fft_log_magnitude));

    case CP_FFT_WITH_PHASES:
      cp->fft_with_phases = Xen_boolean_to_C_bool(on); 
      if (cp->graph_transform_on) calculate_fft(cp); 
      return(C_bool_to_Xen_boolean(cp->fft_with_phases));

    case CP_SPECTRO_HOP:
      cp->spectro_hop = g_imin(1, on, DEFAULT_SPECTRO_HOP); 
      if (cp->graph_transform_on) calculate_fft(cp); 
      return(C_int_to_Xen_integer(cp->spectro_hop));

    case CP_TRANSFORM_SIZE:
      cp->transform_size = g_omin(1, on, DEFAULT_TRANSFORM_SIZE); 
      calculate_fft(cp);
      return(C_llong_to_Xen_llong(cp->transform_size));

    case CP_TRANSFORM_GRAPH_TYPE: 
      cp->transform_graph_type = (graph_type_t)Xen_integer_to_C_int(on); /* checked already */
      calculate_fft(cp); 
      return(C_int_to_Xen_integer((int)(cp->transform_graph_type))); 

    case CP_FFT_WINDOW:
      cp->fft_window = (mus_fft_window_t)Xen_integer_to_C_int(on); /* checked */
      calculate_fft(cp); 
      return(C_int_to_Xen_integer((int)(cp->fft_window)));

    case CP_TRANSFORM_TYPE:
      cp->transform_type = Xen_transform_to_C_int(on);
      calculate_fft(cp); 
      return(C_int_to_Xen_transform(cp->transform_type));

    case CP_TRANSFORM_NORMALIZATION:      
      cp->transform_normalization = (fft_normalize_t)Xen_integer_to_C_int(on); /* range already checked */
      calculate_fft(cp); 
      return(C_int_to_Xen_integer((int)(cp->transform_normalization)));

    case CP_SHOW_MIX_WAVEFORMS: 
      cp->show_mix_waveforms = Xen_boolean_to_C_bool(on); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean(cp->show_mix_waveforms));

    case CP_TIME_GRAPH_STYLE:
      cp->time_graph_style = (Xen_is_integer(on)) ? (graph_style_t)Xen_integer_to_C_int(on) : DEFAULT_GRAPH_STYLE;
      if (call_update_graph) update_graph(cp);
      return(C_int_to_Xen_integer((int)(cp->time_graph_style)));

    case CP_LISP_GRAPH_STYLE:
      cp->lisp_graph_style = (Xen_is_integer(on)) ? (graph_style_t)Xen_integer_to_C_int(on) : DEFAULT_GRAPH_STYLE;
      if (call_update_graph) update_graph(cp);
      return(C_int_to_Xen_integer((int)(cp->lisp_graph_style)));

    case CP_TRANSFORM_GRAPH_STYLE:
      cp->transform_graph_style = (Xen_is_integer(on)) ? (graph_style_t)Xen_integer_to_C_int(on) : DEFAULT_GRAPH_STYLE;
      if (call_update_graph) update_graph(cp);
      return(C_int_to_Xen_integer((int)(cp->transform_graph_style)));

    case CP_X_AXIS_STYLE:
      val = Xen_integer_to_C_int(on); /* range already checked */
      chans_x_axis_style(cp, val);
      return(C_int_to_Xen_integer((int)(cp->x_axis_style)));

    case CP_DOT_SIZE:
      cp->dot_size = mus_iclamp(MIN_DOT_SIZE, 
				(Xen_is_integer(on)) ? Xen_integer_to_C_int(on) : DEFAULT_DOT_SIZE, 
				MAX_DOT_SIZE); /* size > 17000 -> X segfault! */
      update_graph(cp);
      return(C_int_to_Xen_integer(cp->dot_size));

    case CP_SHOW_AXES:
      cp->show_axes = (show_axes_t)Xen_integer_to_C_int(on); /* range checked already */
      update_graph(cp); 
      return(C_int_to_Xen_integer((int)(cp->show_axes)));

    case CP_GRAPHS_HORIZONTAL:
      cp->graphs_horizontal = Xen_boolean_to_C_bool(on); 
      update_graph(cp); 
      return(C_bool_to_Xen_boolean(cp->graphs_horizontal));

    case CP_FRAMPLES:
      if (cp->editable)
	{
	  mus_long_t curlen, newlen;
	  bool need_update = true;
	  /* if less than current, delete, else zero pad */
	  curlen = current_samples(cp);
	  newlen = (Xen_is_llong(on)) ? Xen_llong_to_C_llong(on) : curlen;
	  if (newlen < 0)
	    Xen_out_of_range_error(S_set S_framples, 1, on, "framples < 0?");
	  if (curlen > newlen)
	    {
	      if (newlen > 0)
		need_update = delete_samples(newlen - 1, curlen - newlen, cp, cp->edit_ctr);
	      else need_update = delete_samples(0, curlen, cp, cp->edit_ctr);
	    }
	  else
	    {
	      if (newlen > curlen)
		extend_with_zeros(cp, curlen, newlen - curlen, cp->edit_ctr, S_set S_framples);
	    }
	  if (need_update)
	    update_graph(cp);
	}
      break;

    case CP_PROPERTIES:
      if (!(Xen_is_vector(cp->properties)))
	{
	  cp->properties = Xen_make_vector(1, Xen_empty_list);
	  cp->properties_loc = snd_protect(cp->properties);
	}
      Xen_vector_set(cp->properties, 0, on);
      return(Xen_vector_ref(cp->properties, 0));

    case CP_SX:
      reset_x_display(cp, mus_fclamp(0.0, Xen_real_to_C_double(on), 1.0), cp->axis->zx);
      break;

    case CP_ZX:
      cp->axis->zx = mus_fclamp(0.0, Xen_real_to_C_double(on), 1.0);
      focus_x_axis_change(cp, zoom_focus_style(ss));
      resize_sx_and_zx(cp);
      break;

    case CP_SY:
      reset_y_display(cp, mus_fclamp(0.0, Xen_real_to_C_double(on), 1.0), cp->axis->zy);
      break;

    case CP_ZY:
      reset_y_display(cp, cp->axis->sy, mus_fclamp(0.0, Xen_real_to_C_double(on), 1.0)); 
      break;

    case CP_MIN_DB:
      curamp = Xen_real_to_C_double(on); 
      if (curamp < 0.0)
	{
	  cp->min_dB = curamp;
	  cp->lin_dB = pow(10.0, cp->min_dB * 0.05); 
	  calculate_fft(cp); 
	}
      else Xen_out_of_range_error(caller, 1, on, S_min_dB " should be < 0.0");
      break;

    case CP_SPECTRO_X_ANGLE: cp->spectro_x_angle = mus_fclamp(MIN_SPECTRO_ANGLE, Xen_real_to_C_double(on), MAX_SPECTRO_ANGLE); calculate_fft(cp); break;
    case CP_SPECTRO_Y_ANGLE: cp->spectro_y_angle = mus_fclamp(MIN_SPECTRO_ANGLE, Xen_real_to_C_double(on), MAX_SPECTRO_ANGLE); calculate_fft(cp); break;
    case CP_SPECTRO_Z_ANGLE: cp->spectro_z_angle = mus_fclamp(MIN_SPECTRO_ANGLE, Xen_real_to_C_double(on), MAX_SPECTRO_ANGLE); calculate_fft(cp); break;

    case CP_SPECTRO_X_SCALE: cp->spectro_x_scale = mus_fclamp(0.0, Xen_real_to_C_double(on), MAX_SPECTRO_SCALE); calculate_fft(cp); break;
    case CP_SPECTRO_Y_SCALE: cp->spectro_y_scale = mus_fclamp(0.0, Xen_real_to_C_double(on), MAX_SPECTRO_SCALE); calculate_fft(cp); break;
    case CP_SPECTRO_Z_SCALE: cp->spectro_z_scale = mus_fclamp(0.0, Xen_real_to_C_double(on), MAX_SPECTRO_SCALE); calculate_fft(cp); break;

    case CP_SPECTRUM_END:  
      cp->spectrum_end = Xen_real_to_C_double(on); /* range checked already */
      calculate_fft(cp); 
      return(C_double_to_Xen_real(cp->spectrum_end)); 

    case CP_SPECTRUM_START:   
      cp->spectrum_start = Xen_real_to_C_double(on); /* range checked already */
      calculate_fft(cp); 
      return(C_double_to_Xen_real(cp->spectrum_start));   

    case CP_FFT_WINDOW_ALPHA:        
      cp->fft_window_alpha = Xen_real_to_C_double(on);
      calculate_fft(cp); 
      return(C_double_to_Xen_real(cp->fft_window_alpha));             

    case CP_FFT_WINDOW_BETA:        
      cp->fft_window_beta = Xen_real_to_C_double(on); /* range checked already */
      calculate_fft(cp); 
      return(C_double_to_Xen_real(cp->fft_window_beta));             

    case CP_MAXAMP:
      if (cp->editable)
	{
	  mus_float_t newamp[1];
	  curamp = channel_maxamp(cp, AT_CURRENT_EDIT_POSITION);
	  newamp[0] = Xen_real_to_C_double(on);
	  if (curamp != newamp[0])
	    {
	      if (scale_to(cp->sound, cp, newamp, 1, false))
		update_graph(cp);
	    }
	}
      break;

    case CP_BEATS_PER_MINUTE:
      curamp = Xen_real_to_C_double(on);
      if (curamp > 0.0)
	{
	  cp->beats_per_minute = curamp;
	  update_graph(cp);
	}
      break;

    case CP_BEATS_PER_MEASURE:
      cp->beats_per_measure = Xen_integer_to_C_int(on);
      update_graph(cp);
      return(on);

    default:
      break;
    }
  return(C_bool_to_Xen_boolean(val));
}


static Xen g_update_time_graph(Xen snd, Xen chn) 
{
  #define H_update_time_graph "(" S_update_time_graph " :optional snd chn): redraw snd channel chn's graphs"
  return(channel_get(snd, chn, CP_UPDATE_TIME, S_update_time_graph));
}


static Xen g_update_transform_graph(Xen snd, Xen chn) 
{
  #define H_update_transform_graph "(" S_update_transform_graph " :optional snd chn): redraw snd channel chn's fft display"
  return(channel_get(snd, chn, CP_UPDATE_TRANSFORM_GRAPH, S_update_transform_graph));
}


static Xen g_update_lisp_graph(Xen snd, Xen chn) 
{
  #define H_update_lisp_graph "(" S_update_lisp_graph " :optional snd chn): redraw snd channel chn's lisp graph"
  return(channel_get(snd, chn, CP_UPDATE_LISP, S_update_lisp_graph));
}


static Xen g_edit_position(Xen snd, Xen chn_n) 
{
  #define H_edit_position "(" S_edit_position " :optional snd chn): current edit history position in snd's channel chn"
  return(channel_get(snd, chn_n, CP_EDIT_CTR, S_edit_position));
}

static Xen g_set_edit_position(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_integer(on), on, 1, S_set S_edit_position, "an integer");
  return(channel_set(snd, chn_n, on, CP_EDIT_CTR, S_set S_edit_position));
}

with_three_setter_args(g_set_edit_position_reversed, g_set_edit_position)



static Xen g_transform_graph_on(Xen snd, Xen chn_n) 
{
  #define H_transform_graph_on "(" S_transform_graph_on " :optional snd chn): " PROC_TRUE " if fft display is active in snd's channel chn"
  return(channel_get(snd, chn_n, CP_GRAPH_TRANSFORM_ON, S_transform_graph_on));
}

static Xen g_set_transform_graph_on(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_transform_graph_on, "a boolean");
  return(channel_set(snd, chn_n, on, CP_GRAPH_TRANSFORM_ON, S_set S_transform_graph_on));
}

with_three_setter_args(g_set_transform_graph_on_reversed, g_set_transform_graph_on)



static Xen g_timer_graph_on(Xen snd, Xen chn_n) 
{
  #define H_timer_graph_on "(" S_time_graph_on " :optional snd chn): " PROC_TRUE " if time domain display is active in snd's channel chn"
  return(channel_get(snd, chn_n, CP_GRAPH_TIME_ON, S_time_graph_on));
}

static Xen g_set_timer_graph_on(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_time_graph_on, "a boolean");
  return(channel_set(snd, chn_n, on, CP_GRAPH_TIME_ON, S_set S_time_graph_on));
}

with_three_setter_args(g_set_timer_graph_on_reversed, g_set_timer_graph_on)



static Xen g_lisp_graph_on(Xen snd, Xen chn_n) 
{
  #define H_lisp_graph_on "(" S_lisp_graph_on " :optional snd chn): " PROC_TRUE " if lisp-generated data display is active in snd's channel chn"
  return(channel_get(snd, chn_n, CP_GRAPH_LISP_ON, S_lisp_graph_on));
}

static Xen g_set_lisp_graph_on(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_lisp_graph_on, "a boolean");
  return(channel_set(snd, chn_n, on, CP_GRAPH_LISP_ON, S_set S_lisp_graph_on));
}

with_three_setter_args(g_set_lisp_graph_on_reversed, g_set_lisp_graph_on)



static Xen g_cursor(Xen snd, Xen chn_n, Xen edpos) 
{
  #define H_cursor "(" S_cursor " :optional snd chn edpos): current cursor location in snd's channel chn"
  if (Xen_is_bound(edpos))
    {
      Xen res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn_n, CP_EDPOS_CURSOR, S_cursor);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  return(channel_get(snd, chn_n, CP_CURSOR, S_cursor));
}

static Xen g_set_cursor(Xen on, Xen snd, Xen chn_n, Xen edpos) 
{
  Xen_check_type(Xen_is_llong(on) || !Xen_is_bound(on), on, 1, S_set S_cursor, "an integer");
  if (Xen_is_bound(edpos))
    {
      Xen res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_set(snd, chn_n, on, CP_EDPOS_CURSOR, S_set S_cursor);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  return(channel_set(snd, chn_n, on, CP_CURSOR, S_set S_cursor));
}

with_four_setter_args(g_set_cursor_reversed, g_set_cursor)



static Xen g_cursor_style(Xen snd, Xen chn_n) 
{
  #define H_cursor_style "(" S_cursor_style " :optional snd chn): current cursor style in snd's channel chn. \
Possible values are " S_cursor_cross " (default), " S_cursor_line " (a vertical line), or a procedure of three arguments, the \
sound index, channel number, and graph (always " S_time_graph ").  The procedure \
should draw the cursor at the current cursor position using the " S_cursor_context "."

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn_n, CP_CURSOR_STYLE, S_cursor_style));
  if (cursor_style(ss) == CURSOR_PROC)
    return(ss->cursor_proc);
  return(C_int_to_Xen_integer((int)(cursor_style(ss))));
}

static Xen g_set_cursor_style(Xen on, Xen snd, Xen chn_n) 
{
  cursor_style_t val = CURSOR_PROC;
  Xen_check_type(Xen_is_integer(on) || Xen_is_procedure(on), on, 1, S_set S_cursor_style, "an integer or a function");
  if (Xen_is_integer(on))
    {
      int choice;
      choice = Xen_integer_to_C_int(on);
      if (choice < 0)
	Xen_out_of_range_error(S_set S_cursor_style, 1, on, S_cursor_style " should be " S_cursor_cross " or " S_cursor_line ", or a procedure");
      val = (cursor_style_t)choice;
      if (val > CURSOR_LINE)
	Xen_out_of_range_error(S_set S_cursor_style, 1, on, S_cursor_style " should be " S_cursor_cross " or " S_cursor_line ", or a procedure");
    }
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn_n, on, CP_CURSOR_STYLE, S_set S_cursor_style));
  else
    {
      if (Xen_is_procedure(on))
	{
	  char *error;
	  error = procedure_ok(on, 3, S_cursor_style, "", 1);
	  if (!error)
	    {	  
	      if ((cursor_style(ss) == CURSOR_PROC) && (Xen_is_procedure(ss->cursor_proc)))
		snd_unprotect_at(ss->cursor_proc_loc);
	      ss->cursor_proc_loc = snd_protect(on);
	      ss->cursor_proc = on;
	      in_set_cursor_style(CURSOR_PROC);
	    }
	  else 
	    {
	      Xen errstr;
	      errstr = C_string_to_Xen_string(error);
	      free(error);
	      return(snd_bad_arity_error(S_set S_cursor_style, errstr, on));
	    }
	}
      set_cursor_style(val);
      return(on);
    }
}

with_three_setter_args(g_set_cursor_style_reversed, g_set_cursor_style)



static Xen g_tracking_cursor_style(Xen snd, Xen chn_n) 
{
  #define H_tracking_cursor_style "(" S_tracking_cursor_style " :optional snd chn): current tracking cursor style in snd's channel chn. \
Possible values are " S_cursor_cross " (default), and " S_cursor_line "."

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn_n, CP_TRACKING_CURSOR_STYLE, S_tracking_cursor_style));
  return(C_int_to_Xen_integer((int)(tracking_cursor_style(ss))));
}

static Xen g_set_tracking_cursor_style(Xen on, Xen snd, Xen chn_n) 
{
  int in_val;
  cursor_style_t val = CURSOR_CROSS;
  Xen_check_type(Xen_is_integer(on), on, 1, S_set S_tracking_cursor_style, "an integer");
  in_val = Xen_integer_to_C_int(on);
  if (in_val < 0)
    Xen_out_of_range_error(S_set S_tracking_cursor_style, 1, on, S_tracking_cursor_style " should be " S_cursor_cross " or " S_cursor_line);
  val = (cursor_style_t)in_val;
  if (val > CURSOR_LINE)
    Xen_out_of_range_error(S_set S_tracking_cursor_style, 1, on, S_tracking_cursor_style " should be " S_cursor_cross " or " S_cursor_line);
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn_n, on, CP_TRACKING_CURSOR_STYLE, S_set S_tracking_cursor_style));
  else set_tracking_cursor_style(val);
  return(on);
}

with_three_setter_args(g_set_tracking_cursor_style_reversed, g_set_tracking_cursor_style)



static Xen g_cursor_size(Xen snd, Xen chn_n) 
{
  #define H_cursor_size "(" S_cursor_size " :optional snd chn): current cursor size in snd's channel chn"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn_n, CP_CURSOR_SIZE, S_cursor_size));
  return(C_int_to_Xen_integer(cursor_size(ss)));
}

static Xen g_set_cursor_size(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_integer(on) && (Xen_integer_to_C_int(on) > 0), on, 1, S_set S_cursor_size, "an integer");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn_n, on, CP_CURSOR_SIZE, S_set S_cursor_size));
  set_cursor_size(Xen_integer_to_C_int(on));
  return(C_int_to_Xen_integer(cursor_size(ss)));
}

with_three_setter_args(g_set_cursor_size_reversed, g_set_cursor_size)



static Xen g_cursor_position(Xen snd, Xen chn)
{
  #define H_cursor_position "(" S_cursor_position " :optional snd chn): current cursor position (x y in pixels) in snd's channel chn"
  return(channel_get(snd, chn, CP_CURSOR_POSITION, S_cursor_position));
}


Xen g_framples(Xen snd, Xen chn, Xen edpos)
{
  #define H_framples "(" S_framples " :optional snd-or-object chn edpos): number of framples of data in the given object or channel"

  if (!(Xen_is_bound(chn)))
    {
      if ((xen_is_sound(snd)) || (!Xen_is_bound(snd)))
	return(channel_get(snd, chn, CP_FRAMPLES, S_framples));

      if (Xen_is_string(snd))
	return(g_mus_sound_framples(snd));         /* mus-sound-framples */
      
      if (mus_is_xen(snd))                        /* mus-length */
	return(g_mus_length(snd));

      if (mus_is_vct(snd))                        /* vct-length */
	return(C_llong_to_Xen_llong(mus_vct_length(Xen_to_vct(snd))));

      if (xen_is_mix(snd))                        /* mix-length */
	return(g_mix_length(snd));

      if (xen_is_region(snd))                     /* region-framples */
	return(g_region_framples(snd, Xen_integer_zero));

      if (xen_is_player(snd))
	{
	  snd_info *sp;
	  sp = get_player_sound(snd);
	  return(C_llong_to_Xen_llong(current_samples(sp->chans[0])));
	}
    }

  if (xen_is_selection(snd))
    return(g_selection_framples(chn, edpos));

  if (Xen_is_bound(edpos))
    {
      Xen res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn, CP_EDPOS_FRAMPLES, S_framples);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }

  return(channel_get(snd, chn, CP_FRAMPLES, S_framples));
}


static Xen g_set_framples(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_framples, "a number");
  return(channel_set(snd, chn_n, on, CP_FRAMPLES, S_set S_framples));
}

with_three_setter_args(g_set_framples_reversed, g_set_framples)


static Xen g_vector_maxamp(Xen obj)
{
  double mx = 0.0;
  int i, len;
  len = Xen_vector_length(obj);
  for (i = 0; i < len; i++)
    {
      Xen el;
      el = Xen_vector_ref(obj, i);
      if (Xen_is_number(el))
	{
	  double temp;
	  temp = fabs(Xen_real_to_C_double(el));
	  if (temp > mx) mx = temp;
	}
      /* should we trigger an error here? */
    }
  return(C_double_to_Xen_real(mx));
}

static Xen g_maxamp(Xen snd, Xen chn_n, Xen edpos) 
{
  #define H_maxamp "(" S_maxamp " :optional snd chn edpos): maxamp of data in the object 'snd', or in snd's channel chn if snd is a sound"

  if (!(Xen_is_bound(chn_n)))
    {
      if (Xen_is_string(snd))                     /* string => mus_sound_maxamp */
	return(Xen_cadr(g_mus_sound_maxamp(snd)));

      if (mus_is_xen(snd))                        /* maxamp of mus-data */
	{
	  Xen v;
	  v = g_mus_data(snd);
	  if (mus_is_vct(v))
	    return(g_vct_peak(v));
	}

      if (mus_is_vct(snd))                        /* vct-peak */
	return(g_vct_peak(snd));

      if (xen_is_mix(snd))                        /* mix => sound maxamp of the mix data */
	return(g_mix_maxamp(snd));

      if (xen_is_region(snd))                     /* region-maxamp */
	return(g_region_maxamp(snd));

      if (Xen_is_vector(snd))
	return(g_vector_maxamp(snd));
    }

  if (xen_is_selection(snd))                      /* selection-maxamp where chn=snd and edpos=chn */
    return(g_selection_maxamp(chn_n, edpos));

  if (Xen_is_bound(edpos))
    {
      Xen res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn_n, CP_EDPOS_MAXAMP, S_maxamp);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  
  if ((Xen_is_bound(chn_n)) ||
      (Xen_is_true(snd)))
    return(channel_get(snd, chn_n, CP_MAXAMP, S_maxamp)); /* calls channel_maxamp */

  Snd_assert_sound(S_maxamp, snd, 0);
  {
    snd_info *sp;
    sp = get_sp(snd);
    if (sp)
      {
	uint32_t i;
	mus_float_t mx = 0.0;
	mus_float_t *vals = NULL;
	bool save_maxamp = true;

	for (i = 0; i < sp->nchans; i++)
	  if (sp->chans[i]->edit_ctr != 0)
	    {
	      save_maxamp = false;
	      break;
	    }
	if (save_maxamp)
	  {
	    mus_long_t *times = NULL;
	    /* fprintf(stderr, "save g_maxamp for %s (%d %" print_mus_long ")\n", sp->filename, mus_sound_maxamp_exists(sp->filename), sp->chans[0]->edits[0]->samples); */
	    vals = (mus_float_t *)calloc(sp->nchans, sizeof(mus_float_t));
	    times = (mus_long_t *)calloc(sp->nchans, sizeof(mus_long_t));
	    for (i = 0; i < sp->nchans; i++)
	      {
		chan_info *ncp;
		mus_long_t pos = 0;
		ncp = sp->chans[i];
		vals[i] = channel_maxamp_and_position(ncp, 0, &pos);
		if (vals[i] > mx) mx = vals[i];
		times[i] = pos;
	      }
	    mus_sound_set_maxamps(sp->filename, sp->nchans, vals, times);
	    free(vals);
	    free(times);
	    return(C_double_to_Xen_real(mx));
	  }

	for (i = 0; i < sp->nchans; i++)
	  {
	    mus_float_t cur_mx;
	    
	    cur_mx = channel_maxamp(sp->chans[i], sp->chans[i]->edit_ctr);
	    if (cur_mx > mx)
	      mx = cur_mx;
	  }
	return(C_double_to_Xen_real(mx));
      }
    else snd_no_such_sound_error(S_maxamp, snd); 
  }
  return(Xen_integer_zero);
}

static Xen g_set_maxamp(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_maxamp, "a number");
  return(channel_set(snd, chn_n, on, CP_MAXAMP, S_set S_maxamp));
}

with_three_setter_args(g_set_maxamp_reversed, g_set_maxamp)



static Xen g_maxamp_position(Xen snd, Xen chn_n, Xen edpos) 
{
  #define H_maxamp_position "(" S_maxamp_position " :optional snd chn edpos): location of maxamp of data in snd's channel chn"
  if (Xen_is_bound(edpos))
    {
      Xen res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn_n, CP_EDPOS_MAXAMP_POSITION, S_maxamp_position);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  return(channel_get(snd, chn_n, CP_MAXAMP_POSITION, S_maxamp_position));
}


static Xen g_squelch_update(Xen snd, Xen chn_n) 
{
  #define H_squelch_update "(" S_squelch_update " :optional snd chn): " PROC_TRUE " if updates (redisplays) are turned off in snd's channel chn"
  return(channel_get(snd, chn_n, CP_SQUELCH_UPDATE, S_squelch_update));
}

static Xen g_set_squelch_update(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_squelch_update, "a boolean");
  return(channel_set(snd, chn_n, on, CP_SQUELCH_UPDATE, S_set S_squelch_update));
}

with_three_setter_args(g_set_squelch_update_reversed, g_set_squelch_update)



static Xen g_ap_sx(Xen snd, Xen chn_n) 
{
  #define H_x_position_slider "(" S_x_position_slider " :optional snd chn): current x axis position slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_SX, S_x_position_slider));
}

static Xen g_set_ap_sx(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_x_position_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_SX, S_set S_x_position_slider));
}

with_three_setter_args(g_set_ap_sx_reversed, g_set_ap_sx)



static Xen g_ap_sy(Xen snd, Xen chn_n) 
{
  #define H_y_position_slider "(" S_y_position_slider " :optional snd chn): current y axis position slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_SY, S_y_position_slider));
}

static Xen g_set_ap_sy(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_y_position_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_SY, S_set S_y_position_slider));
}

with_three_setter_args(g_set_ap_sy_reversed, g_set_ap_sy)



static Xen g_ap_zx(Xen snd, Xen chn_n) 
{
  #define H_x_zoom_slider "(" S_x_zoom_slider " :optional snd chn): current x axis zoom slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_ZX, S_x_zoom_slider));
}

static Xen g_set_ap_zx(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_x_zoom_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_ZX, S_set S_x_zoom_slider));
}

with_three_setter_args(g_set_ap_zx_reversed, g_set_ap_zx)



static Xen g_ap_zy(Xen snd, Xen chn_n) 
{
  #define H_y_zoom_slider "(" S_y_zoom_slider " :optional snd chn): current y axis zoom slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_ZY, S_y_zoom_slider));
}

static Xen g_set_ap_zy(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_y_zoom_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_ZY, S_set S_y_zoom_slider));
}

with_three_setter_args(g_set_ap_zy_reversed, g_set_ap_zy)



static Xen g_edit_hook(Xen snd, Xen chn_n) 
{
  #if HAVE_SCHEME
    #define edit_hook_example "(hook-push (" S_edit_hook " snd chn) (lambda () (" S_snd_print " \";about to edit\") #f))"
  #endif
  #if HAVE_RUBY
    #define edit_hook_example "edit_hook(snd, chn).add_hook!(\"hook-test\") do | | snd_print(\"about to edit\"); false end"
  #endif
  #if HAVE_FORTH
    #define edit_hook_example "snd chn edit-hook lambda: <{ }> \"about to edit\" snd-print drop #f ; add-hook!"
  #endif

  #define H_edit_hook "(" S_edit_hook " :optional snd chn): snd's channel chn's " S_edit_hook ". \
This is a channel-specific hook variable; " S_edit_hook " is called just before any attempt to edit the channel's data; if it returns " PROC_TRUE ", \
the edit is aborted. \n  " edit_hook_example

  return(channel_get(snd, chn_n, CP_EDIT_HOOK, S_edit_hook));
}


static Xen g_after_edit_hook(Xen snd, Xen chn_n) 
{
  #if HAVE_SCHEME
    #define after_edit_hook_example "(hook-push (" S_after_edit_hook " snd chn) (lambda () (" S_snd_print " \";just edited\")))"
  #endif
  #if HAVE_RUBY
    #define after_edit_hook_example "after_edit_hook(snd, chn).add_hook!(\"hook-test\") do | | snd_print(\"just edited\") end"
  #endif
  #if HAVE_FORTH
    #define after_edit_hook_example "snd chn after-edit-hook lambda: <{ }> \"just edited\" snd-print drop ; add-hook!"
  #endif

  #define H_after_edit_hook "(" S_after_edit_hook " :optional snd chn): snd's channel chn's " S_after_edit_hook ". \
This is a channel-specific hook variable; " S_after_edit_hook " is called after an edit, but before " S_after_graph_hook ". \n  " after_edit_hook_example

  return(channel_get(snd, chn_n, CP_AFTER_EDIT_HOOK, S_after_edit_hook));
}


static Xen g_undo_hook(Xen snd, Xen chn_n) 
{
  #define H_undo_hook "(" S_undo_hook " :optional snd chn): snd's channel chn's " S_undo_hook ". \
This is a channel-specific hook variable; " S_undo_hook " is called just after any undo, redo, or revert that affects the channel."

  return(channel_get(snd, chn_n, CP_UNDO_HOOK, S_undo_hook));
}


static Xen g_show_y_zero(Xen snd, Xen chn)
{
  #define H_show_y_zero "(" S_show_y_zero " :optional snd chn): " PROC_TRUE " if Snd should include a line at y = 0.0"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_Y_ZERO, S_show_y_zero));
  return(C_bool_to_Xen_boolean(show_y_zero(ss)));
}

static void chans_zero(chan_info *cp, bool value)
{
  cp->show_y_zero = value;
  update_graph(cp);
}

void set_show_y_zero(bool val)
{
  in_set_show_y_zero(val);
  for_each_chan_with_bool(chans_zero, val);
}

static Xen g_set_show_y_zero(Xen on, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_show_y_zero, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_SHOW_Y_ZERO, S_set S_show_y_zero));
  set_show_y_zero(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(show_y_zero(ss)));
}

with_three_setter_args(g_set_show_y_zero_reversed, g_set_show_y_zero)



static Xen g_show_grid(Xen snd, Xen chn)
{
  #define H_show_grid "(" S_show_grid " :optional snd chn): " PROC_TRUE " if Snd should display a background grid in the graphs"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_GRID, S_show_grid));
  return(C_bool_to_Xen_boolean((bool)show_grid(ss)));
}

static Xen g_set_show_grid(Xen on, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_show_grid, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_SHOW_GRID, S_set S_show_grid));
  set_show_grid((with_grid_t)(Xen_boolean_to_C_bool(on)));
  return(C_bool_to_Xen_boolean((bool)show_grid(ss)));
}

with_three_setter_args(g_set_show_grid_reversed, g_set_show_grid)



static Xen g_grid_density(Xen snd, Xen chn)
{
  #define H_grid_density "(" S_grid_density " :optional snd chn): sets how closely axis ticks are spaced, default=1.0"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_GRID_DENSITY, S_grid_density));
  return(C_double_to_Xen_real(grid_density(ss)));
}

static Xen g_set_grid_density(Xen on, Xen snd, Xen chn) 
{
  mus_float_t curf;
  Xen_check_type(Xen_is_number(on), on, 1, S_set S_grid_density, "a number");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_GRID_DENSITY, S_set S_grid_density));
  curf = Xen_real_to_C_double(on);
  if (curf >= 0.0)
    set_grid_density(curf);
  else Xen_out_of_range_error(S_set S_grid_density, 1, on, "density < 0.0?");
  return(C_double_to_Xen_real(grid_density(ss)));
}

with_three_setter_args(g_set_grid_density_reversed, g_set_grid_density)



static Xen g_show_sonogram_cursor(Xen snd, Xen chn)
{
  #define H_show_sonogram_cursor "(" S_show_sonogram_cursor " :optional snd chn): " PROC_TRUE " if Snd should display a cursor in the sonogram"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_SONOGRAM_CURSOR, S_show_sonogram_cursor));
  return(C_bool_to_Xen_boolean((bool)show_sonogram_cursor(ss)));
}

static Xen g_set_show_sonogram_cursor(Xen on, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_show_sonogram_cursor, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_SHOW_SONOGRAM_CURSOR, S_set S_show_sonogram_cursor));
  set_show_sonogram_cursor(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(show_sonogram_cursor(ss)));
}

with_three_setter_args(g_set_show_sonogram_cursor_reversed, g_set_show_sonogram_cursor)



static Xen g_min_dB(Xen snd, Xen chn) 
{
  #define H_min_dB "(" S_min_dB " :optional snd chn): min dB value displayed in fft graphs using dB scales (default: -60)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_MIN_DB, S_min_dB));
  return(C_double_to_Xen_real(min_dB(ss)));
}

static void update_db_graph(chan_info *cp, mus_float_t new_db)
{
  cp->min_dB = new_db;
  cp->lin_dB = pow(10.0, cp->min_dB * 0.05); 
  if ((cp->active < CHANNEL_HAS_AXES) ||
      (!cp->sounds) || 
      (!cp->sounds[cp->sound_ctr]) ||
      (!(cp->graph_transform_on)) ||
      (!(cp->fft_log_magnitude)) ||
      (chan_fft_in_progress(cp)))
    return;
  calculate_fft(cp);
}

void set_min_db(mus_float_t db)
{
  set_min_dB(db);
  ss->lin_dB = pow(10.0, db * 0.05);
  for_each_chan_with_float(update_db_graph, db);
}

static Xen g_set_min_dB(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_min_dB, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_MIN_DB, S_set S_min_dB));
  else
    {
      mus_float_t db;
      db = Xen_real_to_C_double(val);
      if (db < 0.0)
	{
	  set_min_db(db);
	  reflect_min_db_in_transform_dialog();
	}
      else Xen_out_of_range_error(S_set S_min_dB, 1, val, S_min_dB " should be < 0.0");
      return(C_double_to_Xen_real(min_dB(ss)));
    }
}

with_three_setter_args(g_set_min_dB_reversed, g_set_min_dB)



static Xen g_fft_window_beta(Xen snd, Xen chn) 
{
  #define H_fft_window_beta "(" S_fft_window_beta " :optional snd chn): fft window beta parameter value"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW_BETA, S_fft_window_beta));
  return(C_double_to_Xen_real(fft_window_beta(ss)));
}

static Xen g_set_fft_window_beta(Xen val, Xen snd, Xen chn) 
{
  mus_float_t beta;
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_fft_window_beta, "a number"); 
  beta = Xen_real_to_C_double(val);
  if ((beta < 0.0) || (beta > 1.0))
    Xen_out_of_range_error(S_set S_fft_window_beta, 1, val, S_fft_window_beta " should be between 0.0 and 1.0");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW_BETA, S_set S_fft_window_beta));
  set_fft_window_beta(beta);
  return(C_double_to_Xen_real(fft_window_beta(ss)));
}

with_three_setter_args(g_set_fft_window_beta_reversed, g_set_fft_window_beta)



static Xen g_fft_window_alpha(Xen snd, Xen chn) 
{
  #define H_fft_window_alpha "(" S_fft_window_alpha " :optional snd chn): fft window alpha parameter value"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW_ALPHA, S_fft_window_alpha));
  return(C_double_to_Xen_real(fft_window_alpha(ss)));
}

static Xen g_set_fft_window_alpha(Xen val, Xen snd, Xen chn) 
{
  mus_float_t alpha;
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_fft_window_alpha, "a number"); 
  alpha = Xen_real_to_C_double(val);
  if ((alpha < 0.0) || (alpha > 10.0))
    Xen_out_of_range_error(S_set S_fft_window_alpha, 1, val, S_fft_window_alpha " should be between 0.0 and 10.0");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW_ALPHA, S_set S_fft_window_alpha));
  set_fft_window_alpha(alpha);
  return(C_double_to_Xen_real(fft_window_alpha(ss)));
}

with_three_setter_args(g_set_fft_window_alpha_reversed, g_set_fft_window_alpha)



static Xen g_spectrum_end(Xen snd, Xen chn) 
{
  #define H_spectrum_end "(" S_spectrum_end " :optional snd chn): max frequency shown in spectra (1.0 = srate/2)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRUM_END, S_spectrum_end));
  return(C_double_to_Xen_real(spectrum_end(ss)));
}

static Xen g_set_spectrum_end(Xen val, Xen snd, Xen chn) 
{
  mus_float_t cutoff;
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectrum_end, "a number"); 
  cutoff = Xen_real_to_C_double(val);
  if ((cutoff < 0.0) || (cutoff > 1.0))
    Xen_out_of_range_error(S_set S_spectrum_end, 1, val, S_spectrum_end " should be between 0.0 and 1.0");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRUM_END, S_set S_spectrum_end));
  set_spectrum_end(cutoff);
  return(C_double_to_Xen_real(spectrum_end(ss)));
}

with_three_setter_args(g_set_spectrum_end_reversed, g_set_spectrum_end)



static Xen g_spectrum_start(Xen snd, Xen chn) 
{
  #define H_spectrum_start "(" S_spectrum_start " :optional snd chn): lower bound of frequency in spectral displays (0.0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRUM_START, S_spectrum_start));
  return(C_double_to_Xen_real(spectrum_start(ss)));
}

static Xen g_set_spectrum_start(Xen val, Xen snd, Xen chn) 
{
  mus_float_t start;
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectrum_start, "a number"); 
  start = Xen_real_to_C_double(val);
  if ((start < 0.0) || (start > 1.0))
    Xen_out_of_range_error(S_set S_spectrum_start, 1, val, S_spectrum_start " should be between 0.0 and 1.0");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRUM_START, S_set S_spectrum_start));
  set_spectrum_start(start);
  return(C_double_to_Xen_real(spectrum_start(ss)));
}

with_three_setter_args(g_set_spectrum_start_reversed, g_set_spectrum_start)



static Xen g_spectro_x_angle(Xen snd, Xen chn) 
{
  #define H_spectro_x_angle "(" S_spectro_x_angle " :optional snd chn): spectrogram x-axis viewing angle (90.0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_X_ANGLE, S_spectro_x_angle));
  return(C_double_to_Xen_real(spectro_x_angle(ss)));
}

static Xen g_set_spectro_x_angle(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_x_angle, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_X_ANGLE, S_set S_spectro_x_angle));
  set_spectro_x_angle(mus_fclamp(MIN_SPECTRO_ANGLE, Xen_real_to_C_double(val), MAX_SPECTRO_ANGLE));
  return(C_double_to_Xen_real(spectro_x_angle(ss)));
}

with_three_setter_args(g_set_spectro_x_angle_reversed, g_set_spectro_x_angle)



static Xen g_spectro_x_scale(Xen snd, Xen chn) 
{
  #define H_spectro_x_scale "(" S_spectro_x_scale " :optional snd chn): scaler (stretch) along the spectrogram x axis (1.0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_X_SCALE, S_spectro_x_scale));
  return(C_double_to_Xen_real(spectro_x_scale(ss)));
}

static Xen g_set_spectro_x_scale(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_x_scale, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_X_SCALE, S_set S_spectro_x_scale));
  set_spectro_x_scale(mus_fclamp(0.0, Xen_real_to_C_double(val), MAX_SPECTRO_SCALE));
  return(C_double_to_Xen_real(spectro_x_scale(ss)));
}

with_three_setter_args(g_set_spectro_x_scale_reversed, g_set_spectro_x_scale)



static Xen g_spectro_y_angle(Xen snd, Xen chn) 
{
  #define H_spectro_y_angle "(" S_spectro_y_angle " :optional snd chn): spectrogram y-axis viewing angle (0.0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Y_ANGLE, S_spectro_y_angle));
  return(C_double_to_Xen_real(spectro_y_angle(ss)));
}

static Xen g_set_spectro_y_angle(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_y_angle, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Y_ANGLE, S_set S_spectro_y_angle));
  set_spectro_y_angle(mus_fclamp(MIN_SPECTRO_ANGLE, Xen_real_to_C_double(val), MAX_SPECTRO_ANGLE));
  return(C_double_to_Xen_real(spectro_y_angle(ss)));
}

with_three_setter_args(g_set_spectro_y_angle_reversed, g_set_spectro_y_angle)



static Xen g_spectro_y_scale(Xen snd, Xen chn) 
{
  #define H_spectro_y_scale "(" S_spectro_y_scale " :optional snd chn): scaler (stretch) along the spectrogram y axis (1.0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Y_SCALE, S_spectro_y_scale));
  return(C_double_to_Xen_real(spectro_y_scale(ss)));
}

static Xen g_set_spectro_y_scale(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_y_scale, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Y_SCALE, S_set S_spectro_y_scale));
  set_spectro_y_scale(mus_fclamp(0.0, Xen_real_to_C_double(val), MAX_SPECTRO_SCALE));
  return(C_double_to_Xen_real(spectro_y_scale(ss)));
}

with_three_setter_args(g_set_spectro_y_scale_reversed, g_set_spectro_y_scale)



static Xen g_spectro_z_angle(Xen snd, Xen chn) 
{
  #define H_spectro_z_angle "(" S_spectro_z_angle " :optional snd chn): spectrogram z-axis viewing angle (-2.0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Z_ANGLE, S_spectro_z_angle));
  return(C_double_to_Xen_real(spectro_z_angle(ss)));
}

static Xen g_set_spectro_z_angle(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_z_angle, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Z_ANGLE, S_set S_spectro_z_angle));
  set_spectro_z_angle(mus_fclamp(MIN_SPECTRO_ANGLE, Xen_real_to_C_double(val), MAX_SPECTRO_ANGLE));
  return(C_double_to_Xen_real(spectro_z_angle(ss)));
}

with_three_setter_args(g_set_spectro_z_angle_reversed, g_set_spectro_z_angle)



static Xen g_spectro_z_scale(Xen snd, Xen chn) 
{
  #define H_spectro_z_scale "(" S_spectro_z_scale " :optional snd chn): scaler (stretch) along the spectrogram z axis (0.1)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Z_SCALE, S_spectro_z_scale));
  return(C_double_to_Xen_real(spectro_z_scale(ss)));
}

static Xen g_set_spectro_z_scale(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_z_scale, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Z_SCALE, S_set S_spectro_z_scale));
  set_spectro_z_scale(mus_fclamp(0.0, Xen_real_to_C_double(val), MAX_SPECTRO_SCALE));
  return(C_double_to_Xen_real(spectro_z_scale(ss)));
}

with_three_setter_args(g_set_spectro_z_scale_reversed, g_set_spectro_z_scale)



static Xen g_spectro_hop(Xen snd, Xen chn)
{
  #define H_spectro_hop "(" S_spectro_hop " :optional snd chn): hop amount (pixels) in spectral displays"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SPECTRO_HOP, S_spectro_hop));
  return(C_int_to_Xen_integer(spectro_hop(ss)));
}

static Xen g_set_spectro_hop(Xen val, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_spectro_hop, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_HOP, S_set S_spectro_hop));
  set_spectro_hop((Xen_is_integer(val)) ? Xen_integer_to_C_int(val) : 0);
  return(C_int_to_Xen_integer(spectro_hop(ss)));
}

with_three_setter_args(g_set_spectro_hop_reversed, g_set_spectro_hop)



static Xen g_show_marks(Xen snd, Xen chn)
{
  #define H_show_marks "(" S_show_marks " :optional snd chn): " PROC_TRUE " if Snd should show marks"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_MARKS, S_show_marks));
  return(C_bool_to_Xen_boolean(show_marks(ss)));
}

static void chans_marks(chan_info *cp, bool value)
{
  cp->show_marks = value;
  update_graph(cp);
}

void set_show_marks(bool val)
{
  in_set_show_marks(val);
  for_each_chan_with_bool(chans_marks, val);
}

static Xen g_set_show_marks(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_show_marks, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_SHOW_MARKS, S_set S_show_marks));
  set_show_marks(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(show_marks(ss)));
}

with_three_setter_args(g_set_show_marks_reversed, g_set_show_marks)



static Xen g_show_transform_peaks(Xen snd, Xen chn)
{
  #define H_show_transform_peaks "(" S_show_transform_peaks " :optional snd chn): " PROC_TRUE " if fft display should include peak list"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_TRANSFORM_PEAKS, S_show_transform_peaks));
  return(C_bool_to_Xen_boolean(show_transform_peaks(ss)));
}

static Xen g_set_show_transform_peaks(Xen val, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_show_transform_peaks, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_SHOW_TRANSFORM_PEAKS, S_set S_show_transform_peaks));
  set_show_transform_peaks(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(show_transform_peaks(ss)));
}

with_three_setter_args(g_set_show_transform_peaks_reversed, g_set_show_transform_peaks)



static Xen g_zero_pad(Xen snd, Xen chn)
{
  #define H_zero_pad "(" S_zero_pad " :optional snd chn): zero padding used in fft as a multiple of fft size (0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_ZERO_PAD, S_zero_pad));
  return(C_int_to_Xen_integer(zero_pad(ss)));
}

static Xen g_set_zero_pad(Xen val, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_zero_pad, "an integer"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_ZERO_PAD, S_set S_zero_pad));
  set_zero_pad(mus_iclamp(0, g_imin(0, val, DEFAULT_ZERO_PAD), MAX_ZERO_PAD));
  return(C_int_to_Xen_integer(zero_pad(ss)));
}

with_three_setter_args(g_set_zero_pad_reversed, g_set_zero_pad)



static Xen g_wavelet_type(Xen snd, Xen chn)
{
  #define H_wavelet_type "(" S_wavelet_type " :optional snd chn): wavelet used in wavelet-transform (0)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_WAVELET_TYPE, S_wavelet_type));
  return(C_int_to_Xen_integer(wavelet_type(ss)));
}

static Xen g_set_wavelet_type(Xen val, Xen snd, Xen chn)
{
  int wave;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_wavelet_type, "an integer"); 
  wave = Xen_integer_to_C_int(val);
  if ((wave < 0) || (wave >= NUM_WAVELETS))
    Xen_out_of_range_error(S_set S_wavelet_type, 1, val, "unknown wavelet type");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_WAVELET_TYPE, S_set S_wavelet_type));
  set_wavelet_type(wave);
  return(C_int_to_Xen_integer(wavelet_type(ss)));
}

with_three_setter_args(g_set_wavelet_type_reversed, g_set_wavelet_type)



static Xen g_fft_log_frequency(Xen snd, Xen chn)
{
  #define H_fft_log_frequency "(" S_fft_log_frequency " :optional snd chn): " PROC_TRUE " if fft displays use log on the frequency axis"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_FFT_LOG_FREQUENCY, S_fft_log_frequency));
  return(C_bool_to_Xen_boolean(fft_log_frequency(ss)));
}

static Xen g_set_fft_log_frequency(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_fft_log_frequency, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_FFT_LOG_FREQUENCY, S_set S_fft_log_frequency));
  set_fft_log_frequency(Xen_boolean_to_C_bool(on)); 
  return(C_bool_to_Xen_boolean(fft_log_frequency(ss)));
}

with_three_setter_args(g_set_fft_log_frequency_reversed, g_set_fft_log_frequency)



static Xen g_fft_log_magnitude(Xen snd, Xen chn)
{
  #define H_fft_log_magnitude "(" S_fft_log_magnitude " :optional snd chn): " PROC_TRUE " if fft displays use dB"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_FFT_LOG_MAGNITUDE, S_fft_log_magnitude));
  return(C_bool_to_Xen_boolean(fft_log_magnitude(ss)));
}

static Xen g_set_fft_log_magnitude(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_fft_log_magnitude, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_FFT_LOG_MAGNITUDE, S_set S_fft_log_magnitude));
  set_fft_log_magnitude(Xen_boolean_to_C_bool(on)); 
  return(C_bool_to_Xen_boolean(fft_log_magnitude(ss)));
}

with_three_setter_args(g_set_fft_log_magnitude_reversed, g_set_fft_log_magnitude)



static Xen g_fft_with_phases(Xen snd, Xen chn)
{
  #define H_fft_with_phases "(" S_fft_with_phases " :optional snd chn): " PROC_TRUE " if fft displays include phase info"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_FFT_WITH_PHASES, S_fft_with_phases));
  return(C_bool_to_Xen_boolean(fft_with_phases(ss)));
}

static Xen g_set_fft_with_phases(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_fft_with_phases, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_FFT_WITH_PHASES, S_set S_fft_with_phases));
  set_fft_with_phases(Xen_boolean_to_C_bool(on)); 
  return(C_bool_to_Xen_boolean(fft_with_phases(ss)));
}

with_three_setter_args(g_set_fft_with_phases_reversed, g_set_fft_with_phases)



static Xen g_show_mix_waveforms(Xen snd, Xen chn)
{
  #define H_show_mix_waveforms "(" S_show_mix_waveforms " :optional snd chn): " PROC_TRUE " if Snd should display mix waveforms (above the main waveform)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_MIX_WAVEFORMS, S_show_mix_waveforms));
  return(C_bool_to_Xen_boolean(show_mix_waveforms(ss)));
}

static Xen g_set_show_mix_waveforms(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_show_mix_waveforms, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_SHOW_MIX_WAVEFORMS, S_set S_show_mix_waveforms));
  set_show_mix_waveforms(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(show_mix_waveforms(ss)));
}

with_three_setter_args(g_set_show_mix_waveforms_reversed, g_set_show_mix_waveforms)



static Xen g_with_verbose_cursor(Xen snd, Xen chn)
{
  #define H_with_verbose_cursor "(" S_with_verbose_cursor " :optional snd chn): " PROC_TRUE " if the cursor's position and so on is displayed in the status area"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_WITH_VERBOSE_CURSOR, S_with_verbose_cursor));
  return(C_bool_to_Xen_boolean(with_verbose_cursor(ss)));
}

static void chans_with_verbose_cursor(chan_info *cp, bool value)
{
  cp->with_verbose_cursor = value;
  update_graph(cp);
}

void set_with_verbose_cursor(bool val)
{
  in_set_with_verbose_cursor(val);
  if (val == 0) for_each_sound(clear_status_area);
  for_each_chan_with_bool(chans_with_verbose_cursor, val);
}

static Xen g_set_with_verbose_cursor(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_with_verbose_cursor, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_WITH_VERBOSE_CURSOR, S_set S_with_verbose_cursor));
  set_with_verbose_cursor(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(with_verbose_cursor(ss)));
}

with_three_setter_args(g_set_with_verbose_cursor_reversed, g_set_with_verbose_cursor)



static Xen g_time_graph_type(Xen snd, Xen chn)
{
  #define H_time_graph_type "(" S_time_graph_type " :optional snd chn): " S_graph_as_wavogram " if Snd's time domain display is a 'wavogram',\
otherwise " S_graph_once "."
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_TIME_GRAPH_TYPE, S_time_graph_type));
  return(C_int_to_Xen_integer((int)time_graph_type(ss)));
}

static Xen g_set_time_graph_type(Xen val, Xen snd, Xen chn) 
{
  graph_type_t on;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_time_graph_type, "an integer");
  on = (graph_type_t)Xen_integer_to_C_int(val);
  if ((on != GRAPH_ONCE) && (on != GRAPH_AS_WAVOGRAM))
    Xen_out_of_range_error(S_set S_time_graph_type, 1, val, S_time_graph_type " should be " S_graph_once ", or " S_graph_as_wavogram);
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_TIME_GRAPH_TYPE, S_set S_time_graph_type));
  set_time_graph_type(on);
  return(C_int_to_Xen_integer((int)time_graph_type(ss)));
}

with_three_setter_args(g_set_time_graph_type_reversed, g_set_time_graph_type)



static Xen g_wavo_hop(Xen snd, Xen chn)
{
  #define H_wavo_hop "(" S_wavo_hop " :optional snd chn): wavogram spacing between successive traces"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_WAVO_HOP, S_wavo_hop));
  return(C_int_to_Xen_integer(wavo_hop(ss)));
}

static Xen g_set_wavo_hop(Xen val, Xen snd, Xen chn) 
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_wavo_hop, "an integer"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_WAVO_HOP, S_set S_wavo_hop));
  set_wavo_hop(Xen_integer_to_C_int(val));
  return(val);
}

with_three_setter_args(g_set_wavo_hop_reversed, g_set_wavo_hop)



static Xen g_wavo_trace(Xen snd, Xen chn)
{
  #define H_wavo_trace "(" S_wavo_trace " :optional snd chn): length (samples) of each trace in the wavogram (64)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_WAVO_TRACE, S_wavo_trace));
  return(C_int_to_Xen_integer(wavo_trace(ss)));
}

static Xen g_set_wavo_trace(Xen val, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_wavo_trace, "an integer"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_WAVO_TRACE, S_set S_wavo_trace));
  set_wavo_trace(mus_iclamp(1, Xen_integer_to_C_int(val), POINT_BUFFER_SIZE));
  return(C_int_to_Xen_integer(wavo_trace(ss)));
}

with_three_setter_args(g_set_wavo_trace_reversed, g_set_wavo_trace)



static Xen g_transform_size(Xen snd, Xen chn)
{
  #define H_transform_size "(" S_transform_size " :optional snd chn): current fft size (512)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_SIZE, S_transform_size));
  return(C_llong_to_Xen_llong(transform_size(ss)));
}

static Xen g_set_transform_size(Xen val, Xen snd, Xen chn)
{
  mus_long_t len;

  Xen_check_type(Xen_is_llong(val), val, 1, S_set S_transform_size, "an integer"); 
  len = Xen_llong_to_C_llong(val);
  if (len <= 0)
    Xen_out_of_range_error(S_set S_transform_size, 1, val, "size must be > 0");
  if (!(is_power_of_2(len)))
    len = snd_mus_long_t_pow2((int)(log(len + 1) / log(2.0))); /* actually rounds down, despite appearances */
  if (len <= 0) return(Xen_false);
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_SIZE, S_set S_transform_size));
  set_transform_size(len);
  return(C_llong_to_Xen_llong(transform_size(ss)));
}

with_three_setter_args(g_set_transform_size_reversed, g_set_transform_size)



static Xen g_transform_graph_type(Xen snd, Xen chn)
{
  #define H_transform_graph_type "(" S_transform_graph_type " :optional snd chn) can \
be " S_graph_once ", " S_graph_as_sonogram ", or " S_graph_as_spectrogram "."
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_GRAPH_TYPE, S_transform_graph_type));
  return(C_int_to_Xen_integer((int)transform_graph_type(ss)));
}

static Xen g_set_transform_graph_type(Xen val, Xen snd, Xen chn)
{
  graph_type_t style;
  int gt;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_transform_graph_type, "an integer"); 
  gt = Xen_integer_to_C_int(val);
  style = (graph_type_t)gt;
  if ((gt < 0) || (style > GRAPH_AS_SPECTROGRAM))
    Xen_out_of_range_error(S_set S_transform_graph_type, 1, val, S_transform_graph_type " should be " S_graph_once ", " S_graph_as_sonogram ", or " S_graph_as_spectrogram);
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_GRAPH_TYPE, S_set S_transform_graph_type));
  set_transform_graph_type(style);
  return(C_int_to_Xen_integer((int)transform_graph_type(ss)));
}

with_three_setter_args(g_set_transform_graph_type_reversed, g_set_transform_graph_type)



static Xen g_fft_window(Xen snd, Xen chn)
{
  #define H_fft_window "(" S_fft_window " :optional snd chn): fft data window choice (e.g. " S_blackman2_window ").  The \
choices are: " S_rectangular_window ", " S_hann_window ", " S_welch_window ", " S_parzen_window ", \
" S_bartlett_window ", " S_hamming_window ", " S_blackman2_window ", " S_blackman3_window ", \
" S_blackman4_window ", " S_exponential_window ", " S_riemann_window ", " S_kaiser_window ", \
" S_cauchy_window ", " S_poisson_window ", " S_gaussian_window ", " S_tukey_window ", \
" S_dolph_chebyshev_window ", " S_hann_poisson_window ", " S_connes_window ", \
" S_samaraki_window ", " S_ultraspherical_window ", " S_bartlett_hann_window ", " S_bohman_window ", " S_flat_top_window ", and " S_blackman5_window " et al."

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW, S_fft_window));
  return(C_int_to_Xen_integer(fft_window(ss)));
}

static Xen g_set_fft_window(Xen val, Xen snd, Xen chn)
{
  int in_win;

  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_fft_window, "an integer"); 

  in_win = Xen_integer_to_C_int(val);
  if (!(mus_is_fft_window(in_win)))
    Xen_out_of_range_error(S_set S_fft_window, 1, val, "unknown fft data window");

  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW, S_set S_fft_window));

  set_fft_window((mus_fft_window_t)in_win);
  return(C_int_to_Xen_integer((int)fft_window(ss)));
}

with_three_setter_args(g_set_fft_window_reversed, g_set_fft_window)



static Xen g_transform_type(Xen snd, Xen chn)
{
  #define H_transform_type "(" S_transform_type " :optional snd chn): transform type; can be one of " S_fourier_transform ", \
" S_wavelet_transform ", " S_haar_transform ", " S_autocorrelation ", " S_walsh_transform ", \
" S_cepstrum ", or an added transform (see " S_add_transform ")."

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_TYPE, S_transform_type));
  return(C_int_to_Xen_transform(transform_type(ss)));
}

static Xen g_set_transform_type(Xen val, Xen snd, Xen chn)
{
  int type;
  Xen_check_type(xen_is_transform(val), val, 1, S_set S_transform_type, "a transform object"); 
  type = Xen_transform_to_C_int(val);
  if (!is_transform(type))
    Xen_out_of_range_error(S_set S_transform_type, 1, val, "unknown transform");

  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_TYPE, S_set S_transform_type));

  set_transform_type(type);
  return(C_int_to_Xen_transform(transform_type(ss)));
}

with_three_setter_args(g_set_transform_type_reversed, g_set_transform_type)



static Xen g_transform_normalization(Xen snd, Xen chn)
{
  #define H_transform_normalization "(" S_transform_normalization " :optional snd chn): one of " S_dont_normalize ", " S_normalize_by_channel \
", " S_normalize_by_sound ", or " S_normalize_globally ". \
decides whether spectral data is normalized before display (default: " S_normalize_by_channel ")"

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_NORMALIZATION, S_transform_normalization));
  return(C_int_to_Xen_integer((int)transform_normalization(ss)));
}

static Xen g_set_transform_normalization(Xen val, Xen snd, Xen chn)
{
  fft_normalize_t norm;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_transform_normalization, "an integer");
  norm = (fft_normalize_t)Xen_integer_to_C_int(val);
  if (norm >= NUM_TRANSFORM_NORMALIZATIONS)
    Xen_out_of_range_error(S_set S_transform_normalization, 1, val, 
			   S_transform_normalization " should be " S_dont_normalize ", " S_normalize_by_channel ", " S_normalize_by_sound ", or " S_normalize_globally);
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_NORMALIZATION, S_set S_transform_normalization));
  set_transform_normalization(norm);
  return(C_int_to_Xen_integer((int)transform_normalization(ss)));
}

with_three_setter_args(g_set_transform_normalization_reversed, g_set_transform_normalization)



static Xen g_max_transform_peaks(Xen snd, Xen chn)
{
  #define H_max_transform_peaks "(" S_max_transform_peaks " :optional snd chn): max number of fft peaks reported in fft display"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_MAX_TRANSFORM_PEAKS, S_max_transform_peaks));
  return(C_int_to_Xen_integer(max_transform_peaks(ss)));
}

static Xen g_set_max_transform_peaks(Xen n, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_integer(n), n, 1, S_set S_max_transform_peaks, "an integer"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, n, CP_MAX_TRANSFORM_PEAKS, S_set S_max_transform_peaks));
  else
    {
      int lim;
      lim = Xen_integer_to_C_int(n);
      if (lim >= 1)
	{
	  set_max_transform_peaks(lim);
	  reflect_peaks_in_transform_dialog();
	}
      return(C_int_to_Xen_integer(max_transform_peaks(ss)));
    }
}

with_three_setter_args(g_set_max_transform_peaks_reversed, g_set_max_transform_peaks)



static Xen g_graph_style(Xen snd, Xen chn)
{
  #define H_graph_style "(" S_graph_style " :optional snd chn): graph style, one \
of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_TIME_GRAPH_STYLE, S_time_graph_style));
  return(C_int_to_Xen_integer(graph_style(ss)));
}

static void chans_graph_style(chan_info *cp, int value)
{
  graph_style_t style = (graph_style_t)value;
  cp->time_graph_style = style;
  cp->lisp_graph_style = style;
  cp->transform_graph_style = style;
  update_graph(cp);
}

void set_graph_style(graph_style_t val)
{
  in_set_graph_style(val);
  for_each_chan_with_int(chans_graph_style, (int)val);
}

static Xen g_set_graph_style(Xen style, Xen snd, Xen chn)
{
  int val;

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_graph_style, "an integer"); 

  val = Xen_integer_to_C_int(style);
  if (!(is_graph_style(val)))
    Xen_out_of_range_error(S_set S_graph_style, 1, style, "unknown graph-style");

  if (Xen_is_bound(snd))
    {
      Xen xval;
      call_update_graph = false;
      xval = channel_set(snd, chn, style, CP_TIME_GRAPH_STYLE, S_set S_graph_style);
      channel_set(snd, chn, style, CP_LISP_GRAPH_STYLE, S_set S_graph_style);
      call_update_graph = true;
      channel_set(snd, chn, style, CP_TRANSFORM_GRAPH_STYLE, S_set S_graph_style);
      return(xval);
    }
  set_graph_style((graph_style_t)val);
  return(C_int_to_Xen_integer((int)(graph_style(ss))));
}

with_three_setter_args(g_set_graph_style_reversed, g_set_graph_style)



static Xen g_time_graph_style(Xen snd, Xen chn)
{
  #define H_time_graph_style "(" S_time_graph_style " snd chn): time domain graph drawing style. \
one of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"
  Snd_assert_sound(S_time_graph_style, snd, 0);
  return(channel_get(snd, chn, CP_TIME_GRAPH_STYLE, S_time_graph_style));
}

static Xen g_set_time_graph_style(Xen style, Xen snd, Xen chn)
{
  int val;

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_time_graph_style, "an integer"); 
  Snd_assert_sound(S_time_graph_style, snd, 0);

  val = Xen_integer_to_C_int(style);
  if (!(is_graph_style(val)))
    Xen_out_of_range_error(S_set S_time_graph_style, 1, style, "unknown " S_time_graph_style);

  return(channel_set(snd, chn, style, CP_TIME_GRAPH_STYLE, S_set S_time_graph_style));
}

with_three_setter_args(g_set_time_graph_style_reversed, g_set_time_graph_style)



static Xen g_lisp_graph_style(Xen snd, Xen chn)
{
  #define H_lisp_graph_style "(" S_lisp_graph_style " snd chn): lisp graph drawing style. \
one of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"
  Snd_assert_sound(S_lisp_graph_style, snd, 0);
  return(channel_get(snd, chn, CP_LISP_GRAPH_STYLE, S_lisp_graph_style));
}

static Xen g_set_lisp_graph_style(Xen style, Xen snd, Xen chn)
{
  int val;

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_lisp_graph_style, "an integer"); 
  Snd_assert_sound(S_lisp_graph_style, snd, 0);

  val = Xen_integer_to_C_int(style);
  if (!(is_graph_style(val)))
    Xen_out_of_range_error(S_set S_lisp_graph_style, 1, style, "unknown " S_lisp_graph_style);

  return(channel_set(snd, chn, style, CP_LISP_GRAPH_STYLE, S_set S_lisp_graph_style));
}

with_three_setter_args(g_set_lisp_graph_style_reversed, g_set_lisp_graph_style)



static Xen g_transform_graph_style(Xen snd, Xen chn)
{
  #define H_transform_graph_style "(" S_transform_graph_style " snd chn): fft graph drawing style, one \
of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"
  Snd_assert_sound(S_transform_graph_style, snd, 0);
  return(channel_get(snd, chn, CP_TRANSFORM_GRAPH_STYLE, S_transform_graph_style));
}

static Xen g_set_transform_graph_style(Xen style, Xen snd, Xen chn)
{
  int val;

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_transform_graph_style, "an integer"); 
  Snd_assert_sound(S_transform_graph_style, snd, 0);

  val = Xen_integer_to_C_int(style);

  if (!(is_graph_style(val)))
    Xen_out_of_range_error(S_set S_transform_graph_style, 1, style, "unknown " S_transform_graph_style);

  return(channel_set(snd, chn, style, CP_TRANSFORM_GRAPH_STYLE, S_set S_transform_graph_style));
}

with_three_setter_args(g_set_transform_graph_style_reversed, g_set_transform_graph_style)



static Xen g_dot_size(Xen snd, Xen chn)
{
  #define H_dot_size "(" S_dot_size " :optional snd chn): size in pixels of dots when graphing with dots (1)"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_DOT_SIZE, S_dot_size));
  return(C_int_to_Xen_integer(dot_size(ss)));
}

static Xen g_set_dot_size(Xen size, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_integer(size), size, 1, S_set S_dot_size, "an integer"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, size, CP_DOT_SIZE, S_set S_dot_size));
  set_dot_size(mus_iclamp(MIN_DOT_SIZE, Xen_integer_to_C_int(size), MAX_DOT_SIZE));
  return(C_int_to_Xen_integer(dot_size(ss)));
}

with_three_setter_args(g_set_dot_size_reversed, g_set_dot_size)



static Xen g_x_axis_style(Xen snd, Xen chn)
{
  #define H_x_axis_style "(" S_x_axis_style " :optional snd chn): \
The x axis labelling of the time domain waveform can be in seconds (" S_x_axis_in_seconds "), in samples (" S_x_axis_in_samples "), expressed as a \
percentage of the overall duration (" S_x_axis_as_percentage "), as a beat number (" S_x_axis_in_beats "), as a measure \
number (" S_x_axis_in_measures "), or clock-style (dd:hh:mm:ss) (" S_x_axis_as_clock ")."

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_X_AXIS_STYLE, S_x_axis_style));
  return(C_int_to_Xen_integer((int)x_axis_style(ss)));
}

static void chans_x_axis_style(chan_info *cp, int value)
{
  axis_info *ap;
  x_axis_style_t new_style = (x_axis_style_t)value;
  ap = cp->axis;
  cp->x_axis_style = new_style;
  if (ap)
    {
      if (ap->xlabel) free(ap->xlabel);
      /* if user set x-axis-label (snd-axis.c) ap->default_xlabel is not null, so use it rather than normal choices */
      if (ap->default_xlabel)
	ap->xlabel = mus_strdup(ap->default_xlabel);
      else
	{
	  switch (new_style)
	    {
	    case X_AXIS_IN_BEATS:      ap->xlabel = mus_strdup("time (beats)");    break;
	    case X_AXIS_IN_MEASURES:   ap->xlabel = mus_strdup("time (measures)"); break;
	    case X_AXIS_IN_SAMPLES:    ap->xlabel = mus_strdup("time (samples)");  break;
	    case X_AXIS_AS_PERCENTAGE: ap->xlabel = mus_strdup("time (percent)");  break;
	    default:                   ap->xlabel = mus_strdup("time");            break;
	    }
	}
      update_graph(cp);
    }
} 

void set_x_axis_style(x_axis_style_t val)
{
  in_set_x_axis_style(val);
  for_each_chan_with_int(chans_x_axis_style, (int)val);
}

static Xen g_set_x_axis_style(Xen style, Xen snd, Xen chn)
{
  int val;

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_x_axis_style, "an integer"); 

  val = Xen_integer_to_C_int(style);
  if (!(is_x_axis_style(val)))
    Xen_out_of_range_error(S_set S_x_axis_style, 1, style, "unknown " S_x_axis_style);

  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, style, CP_X_AXIS_STYLE, S_set S_x_axis_style));

  set_x_axis_style((x_axis_style_t)val);
  /* snd-menu.c -- maps over chans */
  return(C_int_to_Xen_integer((int)x_axis_style(ss)));
}

with_three_setter_args(g_set_x_axis_style_reversed, g_set_x_axis_style)



static Xen g_beats_per_minute(Xen snd, Xen chn)
{
  #define H_beats_per_minute "(" S_beats_per_minute " :optional snd chn): beats per minute if " S_x_axis_style " is " S_x_axis_in_beats
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_BEATS_PER_MINUTE, S_beats_per_minute));
  return(C_double_to_Xen_real(beats_per_minute(ss)));
}

static Xen g_set_beats_per_minute(Xen beats, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_number(beats), beats, 1, S_set S_beats_per_minute, "a number"); 
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, beats, CP_BEATS_PER_MINUTE, S_set S_beats_per_minute));
  else
    {
      mus_float_t val;
      val = Xen_real_to_C_double(beats);
      if (val > 0.0)
	set_beats_per_minute(val);
      return(C_double_to_Xen_real(beats_per_minute(ss)));
    }
}

with_three_setter_args(g_set_beats_per_minute_reversed, g_set_beats_per_minute)



static Xen g_beats_per_measure(Xen snd, Xen chn)
{
  #define H_beats_per_measure "(" S_beats_per_measure " :optional snd chn): beats per measure if " S_x_axis_style " is " S_x_axis_in_measures
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_BEATS_PER_MEASURE, S_beats_per_measure));
  return(C_int_to_Xen_integer(beats_per_measure(ss)));
}

static Xen g_set_beats_per_measure(Xen beats, Xen snd, Xen chn)
{
  int val;
  Xen_check_type(Xen_is_integer(beats), beats, 1, S_set S_beats_per_measure, "an int"); 
  val = Xen_integer_to_C_int(beats);
  if ((val <= 0) || (val > 1000))
    Xen_out_of_range_error(S_set S_beats_per_measure, 1, beats, S_beats_per_measure " should be between 1 and 1000");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, beats, CP_BEATS_PER_MEASURE, S_set S_beats_per_measure));
  set_beats_per_measure(val);
  return(C_int_to_Xen_integer(beats_per_measure(ss)));
}

with_three_setter_args(g_set_beats_per_measure_reversed, g_set_beats_per_measure)



static Xen g_show_axes(Xen snd, Xen chn)
{
  #define H_show_axes "(" S_show_axes " :optional snd chn) \
If " S_show_all_axes ", display x and y axes; if " S_show_x_axis ", just one axis (the x axis) is displayed. \
The other choices are " S_show_no_axes ", " S_show_all_axes_unlabelled ", " S_show_x_axis_unlabelled ", and " S_show_bare_x_axis "."

  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_SHOW_AXES, S_show_axes));
  return(C_int_to_Xen_integer((int)show_axes(ss)));
}

static Xen g_set_show_axes(Xen on, Xen snd, Xen chn)
{
  int val;

  Xen_check_type(Xen_is_integer(on), on, 1, S_set S_show_axes, "an integer");

  val = Xen_integer_to_C_int(on);
  if (!(shows_axes(val)))
    Xen_out_of_range_error(S_set S_show_axes, 1, on, "unknown " S_show_axes " choice");

  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, on, CP_SHOW_AXES, S_set S_show_axes));

  set_show_axes((show_axes_t)val);
  return(C_int_to_Xen_integer((int)show_axes(ss)));
}

with_three_setter_args(g_set_show_axes_reversed, g_set_show_axes)



static Xen g_graphs_horizontal(Xen snd, Xen chn)
{
  #define H_graphs_horizontal "(" S_graphs_horizontal " :optional snd chn): " PROC_TRUE " if the time domain, fft, and lisp graphs are layed out horizontally"
  if (Xen_is_bound(snd))
    return(channel_get(snd, chn, CP_GRAPHS_HORIZONTAL, S_graphs_horizontal));
  return(C_bool_to_Xen_boolean(graphs_horizontal(ss)));
}

static Xen g_set_graphs_horizontal(Xen val, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_graphs_horizontal, "a boolean");
  if (Xen_is_bound(snd))
    return(channel_set(snd, chn, val, CP_GRAPHS_HORIZONTAL, S_set S_graphs_horizontal));
  set_graphs_horizontal(Xen_boolean_to_C_bool(val)); 
  return(C_bool_to_Xen_boolean(graphs_horizontal(ss)));
}

with_three_setter_args(g_set_graphs_horizontal_reversed, g_set_graphs_horizontal)




void write_transform_peaks(FILE *fd, chan_info *ucp)
{
  /* put (sync'd) peak info in (possibly temporary) file.
   *   this used to follow the displays, but I think I'd rather get all the data at high precision
   */
  int i, chn;
  sync_info *si = NULL;

  fprintf(fd, "Snd: fft peaks (%s)\n\n", snd_local_time());

  si = sync_to_chan(ucp);
  for (chn = 0; chn < si->chans; chn++)
    {
      chan_info *cp;
      fft_info *fp;

      cp = si->cps[chn];
      fp = cp->fft;
      if (fp)
	{
	  axis_info *fap;

	  fap = fp->axis;
	  if ((fap) && (fap->graph_active))
	    {
	      snd_info *sp;
	      mus_float_t srate2;
	      axis_info *ap;
	      fft_peak *peak_freqs = NULL;
	      fft_peak *peak_amps = NULL;
	      mus_float_t *data;
	      int num_peaks, samps, srate;

	      ap = cp->axis;
	      sp = cp->sound;
	      data = fp->data;
	      srate = snd_srate(sp);
	      srate2 = (mus_float_t)srate * .5;
	      samps = (int)(fp->current_size * 0.5);

	      peak_freqs = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));
	      peak_amps = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));
	      num_peaks = find_and_sort_transform_peaks(data, peak_freqs, cp->max_transform_peaks, 0, samps, 0.5, fp->scale);

	      if ((num_peaks != 1) || 
		  (peak_freqs[0].freq != 0.0))
		{
		  fprintf(fd, "%s", sp->short_filename);
		  if (sp->nchans > 1) fprintf(fd, ": chan %d", cp->chan);
		  fprintf(fd, ", fft %" print_mus_long " points beginning at sample %" print_mus_long " (%.3f secs), %s\n\n",
			  fp->current_size, 
			  ap->losamp, 
			  ((double)(ap->losamp) / (double)srate),
			  mus_fft_window_name(cp->fft_window)); /* was Xen name */
		  for (i = 0; i < num_peaks; i++)
		    fprintf(fd, "  %.*f  %.*f\n",
			    6, 
			    peak_freqs[i].freq * srate2, 
			    6,
			    peak_freqs[i].amp); 
		  fprintf(fd, "\n");
		}
	      if (peak_freqs) {free(peak_freqs); peak_freqs = NULL;}
	      if (peak_amps) {free(peak_amps); peak_amps = NULL;}
	    }
	}
    }
  if (si) free_sync_info(si);
}


static Xen g_peaks(Xen filename, Xen snd, Xen chn_n)
{
  #define H_peaks "(" S_peaks " :optional filename snd chn): write current fft peaks data to filename, or \
to the info dialog if filename is omitted"

  char *name = NULL;
  chan_info *cp;
  bool post_to_dialog = true;
  FILE *fd = NULL;

  Xen_check_type((Xen_is_string(filename) || (Xen_is_false(filename)) || (!Xen_is_bound(filename))), filename, 1, S_peaks, "a string or " PROC_FALSE);

  Snd_assert_channel(S_peaks, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_peaks);
  if (!cp) return(Xen_false);

  if (Xen_is_string(filename))
    {
      name = mus_expand_filename(Xen_string_to_C_string(filename));
      if ((name) && (mus_strlen(name) > 0))
	{
	  fd = FOPEN(name, "w");
	  post_to_dialog = false;
	}
    }
  else
    {
      name = snd_tempnam();
      fd = FOPEN(name, "w");
    }

  if (!fd)
    Xen_error(Xen_make_error_type("cant-open-file"),
	      Xen_list_3(C_string_to_Xen_string(S_peaks ": ~S ~A"),
			 C_string_to_Xen_string(name),
			 C_string_to_Xen_string(snd_io_strerror())));

  write_transform_peaks(fd, cp);
  snd_fclose(fd, name);

  if (post_to_dialog)
    {
      char *str;
      str = file_to_string(name);
      if (str)
	{
	  post_it("fft peaks", str);
	  free(str);
	}
      snd_remove(name, IGNORE_CACHE);
    }
  free(name);
  return(filename);
}


static Xen g_left_sample(Xen snd, Xen chn_n) 
{
  #define H_left_sample "(" S_left_sample " :optional snd chn): left sample number in time domain window"
  return(channel_get(snd, chn_n, CP_LOSAMP, S_left_sample));
}

static Xen g_set_left_sample(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_llong(on) || !Xen_is_bound(on), on, 1, S_set S_left_sample, "an integer");
  return(channel_set(snd, chn_n, on, CP_LOSAMP, S_set S_left_sample));
}

with_three_setter_args(g_set_left_sample_reversed, g_set_left_sample)



static Xen g_right_sample(Xen snd, Xen chn_n) 
{
  #define H_right_sample "(" S_right_sample " :optional snd chn): right sample number in time domain window"
  return(channel_get(snd, chn_n, CP_HISAMP, S_right_sample));
}

static Xen g_set_right_sample(Xen on, Xen snd, Xen chn_n) 
{
  Xen_check_type(Xen_is_llong(on) || !Xen_is_bound(on), on, 1, S_set S_right_sample, "an integer");
  return(channel_set(snd, chn_n, on, CP_HISAMP, S_set S_right_sample));
}

with_three_setter_args(g_set_right_sample_reversed, g_set_right_sample)



static Xen g_channel_properties(Xen snd, Xen chn_n) 
{
  #define H_channel_properties "(" S_channel_properties " :optional snd chn): \
A property list associated with the given channel. It is set to () at the time a sound is opened."

  return(channel_get(snd, chn_n, CP_PROPERTIES, S_channel_properties));
}

static Xen g_set_channel_properties(Xen on, Xen snd, Xen chn_n) 
{
  /* Xen_check_type(Xen_is_list(on), on, 1, S_set S_channel_properties, "a property list"); */
  return(channel_set(snd, chn_n, on, CP_PROPERTIES, S_set S_channel_properties));
}

with_three_setter_args(g_set_channel_properties_reversed, g_set_channel_properties)


static Xen g_channel_property(Xen key, Xen snd, Xen chn) 
{
  #define H_channel_property "(" S_channel_property " key snd chn) returns the value associated with 'key' in \
the given channel's property list, or " PROC_FALSE "."
  return(Xen_assoc_ref(key, g_channel_properties(snd, chn)));
}

#if HAVE_SCHEME
static Xen g_set_channel_property(Xen val, Xen key, Xen snd, Xen chn) 
#else
static Xen g_set_channel_property(Xen key, Xen val, Xen snd, Xen chn) 
#endif
{
  g_set_channel_properties(Xen_assoc_set(key, val, g_channel_properties(snd, chn)), snd, chn);
  return(val);
}

with_four_setter_args(g_set_channel_property_reversed, g_set_channel_property)



static Xen g_edit_properties(Xen snd, Xen chn_n, Xen pos) 
{
  #define H_edit_properties "(" S_edit_properties " :optional snd chn edpos): \
A property list associated with the given edit. It is set to () at the time an edit is performed and cleared when that edit is no longer accessible."

  chan_info *cp;
  int edpos;

  Snd_assert_channel(S_edit_properties, snd, chn_n, 1);

  cp = get_cp(snd, chn_n, S_edit_properties);
  if (!cp) return(Xen_false);

  edpos = to_c_edit_position(cp, pos, S_edit_properties, 3);
  if (!(Xen_is_vector(cp->edits[edpos]->properties)))
    {
      cp->edits[edpos]->properties = Xen_make_vector(1, Xen_empty_list);
      cp->edits[edpos]->properties_gc_loc = snd_protect(cp->edits[edpos]->properties);
    }
  return(Xen_vector_ref(cp->edits[edpos]->properties, 0));
}

static Xen g_set_edit_properties(Xen on, Xen snd, Xen chn_n, Xen pos) 
{
  chan_info *cp;
  int edpos;

  Snd_assert_channel(S_set S_edit_properties, snd, chn_n, 1);

  cp = get_cp(snd, chn_n, S_set S_edit_properties);
  if (!cp) return(Xen_false);

  edpos = to_c_edit_position(cp, pos, S_set S_edit_properties, 3);
  if (!(Xen_is_vector(cp->edits[edpos]->properties)))
    {
      cp->edits[edpos]->properties = Xen_make_vector(1, Xen_empty_list);
      cp->edits[edpos]->properties_gc_loc = snd_protect(cp->edits[edpos]->properties);
    }
  Xen_vector_set(cp->edits[edpos]->properties, 0, on);
  return(Xen_vector_ref(cp->edits[edpos]->properties, 0));
}

with_four_setter_args(g_set_edit_properties_reversed, g_set_edit_properties)


static Xen g_edit_property(Xen key, Xen snd, Xen chn, Xen pos) 
{
  #define H_edit_property "(" S_edit_property " key snd chn pos) returns the value associated with 'key' in the \
given edit's property list, or " PROC_FALSE "."
  Snd_assert_channel(S_edit_property, snd, chn, 2);
  return(Xen_assoc_ref(key, g_edit_properties(snd, chn, pos)));
}

static Xen g_set_edit_property(Xen key, Xen val, Xen snd, Xen chn, Xen pos) 
{
  Snd_assert_channel(S_edit_property, snd, chn, 3);
  g_set_edit_properties(Xen_assoc_set(key, val, g_edit_properties(snd, chn, pos)), snd, chn, pos);
  return(val);
}


#if HAVE_SCHEME
static Xen g_set_edit_property_reversed(s7_scheme *sc, s7_pointer args)
{
  int len;
  len = Xen_list_length(args);

  if (len == 2)
    return(g_set_edit_property(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_undefined, Xen_undefined, Xen_undefined));

  if (len == 3)
    return(g_set_edit_property(Xen_list_ref(args, 0), Xen_list_ref(args, 2), Xen_list_ref(args, 1), Xen_undefined, Xen_undefined)); 

  if (len == 4)
    return(g_set_edit_property(Xen_list_ref(args, 0), Xen_list_ref(args, 3), Xen_list_ref(args, 1), Xen_list_ref(args, 2), Xen_undefined)); 

  return(g_set_edit_property(Xen_list_ref(args, 0), Xen_list_ref(args, 4), Xen_list_ref(args, 1), Xen_list_ref(args, 2), Xen_list_ref(args, 3)));
}
#endif


static Xen g_edits(Xen snd, Xen chn_n)
{
  #define H_edits "(" S_edits " :optional snd chn): -> (list undoable-edits redoable-edits) in snd's channel chn"
  chan_info *cp;
  int i;
  Snd_assert_channel(S_edits, snd, chn_n, 1);
  cp = get_cp(snd, chn_n, S_edits);
  if (!cp) return(Xen_false);
  for (i = cp->edit_ctr + 1; i < cp->edit_size; i++)
    if (!(cp->edits[i])) break;
  return(Xen_list_2(C_int_to_Xen_integer(cp->edit_ctr),
		    C_int_to_Xen_integer(i - cp->edit_ctr - 1)));
}

/* Xen ldata, Xen xlabel, Xen x0, Xen x1, Xen y0, Xen y1, Xen snd, Xen chn_n, Xen force_display, Xen show_axes */
static Xen g_graph(Xen args)
{
  #define H_graph "(" S_graph " data :optional xlabel (x0 0.0) (x1 1.0) y0 y1 snd chn (force-display " PROC_TRUE ") show-axes): \
displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list or a " S_vct ". \
If 'data' is a list of numbers, it is treated as an envelope."

  chan_info *cp;
  lisp_grf *lg;
  Xen data = Xen_undefined;
  char *label = NULL;
  int i, len = 0, graphs;
  bool need_update = false;
  mus_float_t ymin = 32768.0, ymax = -32768.0, val;
  double nominal_x0 = 0.0, nominal_x1 = 1.0;
  int old_height = 0, old_width = 0, ww = 0;
  int old_y_offset = 0, gx0 = 0;
  Xen arg, ldata, x0 = Xen_undefined, x1 = Xen_undefined;
  Xen snd = Xen_undefined, chn_n = Xen_undefined, force_display = Xen_undefined, show_axes = Xen_undefined;

  /* ldata can be a vct or a list of numbers or vcts */
  Xen_check_type(Xen_is_pair(args), args, 1, S_graph, "a " S_vct " or a list");
  arg = args;
  ldata = Xen_car(arg);
  Xen_check_type(((mus_is_vct(ldata)) || 
		   ((Xen_is_list(ldata)) && (Xen_list_length(ldata) > 0) && 
		    ((Xen_is_number(Xen_car(ldata))) || (mus_is_vct(Xen_car(ldata)))))),
		  ldata, 1, S_graph, "a " S_vct " or a list");

  if ((!(Xen_is_list(ldata))) || 
      (Xen_is_number(Xen_car(ldata))))
    graphs = 1; 
  else graphs = Xen_list_length(ldata);
  if (graphs == 0) return(Xen_false);

  arg = Xen_cdr(arg);
  if (!Xen_is_null(arg))
    {
      Xen xlabel;
      xlabel = Xen_car(arg);
      Xen_check_type(Xen_is_string(xlabel), xlabel, 2, S_graph, "a string (x axis label)");
      label = mus_strdup(Xen_string_to_C_string(xlabel)); 
      
      arg = Xen_cdr(arg);
      if (!Xen_is_null(arg))
	{
	  x0 = Xen_car(arg);
	  Xen_check_type(Xen_is_number(x0), x0, 3, S_graph, "a number (x0)");
	  nominal_x0 = Xen_real_to_C_double(x0);

	  arg = Xen_cdr(arg);
	  if (!Xen_is_null(arg))
	    {
	      x1 = Xen_car(arg);
	      Xen_check_type(Xen_is_number(x1), x1, 4, S_graph, "a number (x1)");
	      nominal_x1 = Xen_real_to_C_double(x1); 

	      arg = Xen_cdr(arg);
	      if (!Xen_is_null(arg))
		{
		  Xen y0;
		  y0 = Xen_car(arg);
		  Xen_check_type(Xen_is_number(y0), y0, 5, S_graph, "a number (y0)");
		  ymin = Xen_real_to_C_double(y0);

		  arg = Xen_cdr(arg);
		  if (!Xen_is_null(arg))
		    {
		      Xen y1;
		      y1 = Xen_car(arg);
		      Xen_check_type(Xen_is_number(y1), y1, 6, S_graph, "a number (y1)");
		      ymax = Xen_real_to_C_double(y1);

		      arg = Xen_cdr(arg);
		      if (!Xen_is_null(arg))
			{
			  snd = Xen_car(arg);

			  arg = Xen_cdr(arg);
			  if (!Xen_is_null(arg))
			    {
			      chn_n = Xen_car(arg);

			      arg = Xen_cdr(arg);
			      if (!Xen_is_null(arg))
				{
				  force_display = Xen_car(arg);
				  Xen_check_type(Xen_is_boolean(force_display), force_display, 9, S_graph, "a boolean (force-display)");

				  arg = Xen_cdr(arg);
				  if (!Xen_is_null(arg))
				    {
				      show_axes = Xen_car(arg);
				      Xen_check_type(Xen_is_integer(show_axes), show_axes, 10, S_graph, "an integer (show-axes choice)");
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  Snd_assert_channel(S_graph, snd, chn_n, 7);
  cp = get_cp(snd, chn_n, S_graph);
  if (!cp) return(Xen_false);

  if ((cp->sound_ctr == NOT_A_SOUND) || 
      (!cp->sounds) || 
      (!cp->sounds[cp->sound_ctr]) ||
      (!cp->axis))
    return(Xen_false);

  lg = cp->lisp_info;
  if (lg)
    {
      axis_info *uap = NULL;
      uap = cp->lisp_info->axis;
      if (uap)
	{
	  old_height = uap->height;
	  old_width = uap->width;
	  ww = uap->window_width;
	  gx0 = uap->graph_x0;
	  old_y_offset = uap->y_offset;
	}
      if (graphs != lg->graphs)
	{
	  free_lisp_info(cp);
	  cp->lisp_info = NULL;
	}
    }

  if (!(cp->lisp_info))
    {
      lg = (lisp_grf *)calloc(graphs, sizeof(lisp_grf));
      cp->lisp_info = lg;
      lg->len = (int *)calloc(graphs, sizeof(int));
      lg->graphs = graphs;
      lg->data = (mus_float_t **)calloc(graphs, sizeof(mus_float_t *));
      need_update = true;
    }
  
  cp->lisp_info->show_axes = cp->show_axes;
  if (Xen_is_integer(show_axes))
    {
      show_axes_t aval;
      aval = (show_axes_t)Xen_integer_to_C_int(show_axes);
      if (aval < NUM_SHOW_AXES)
	cp->lisp_info->show_axes = aval;
    }

  if ((Xen_is_list(ldata)) &&
      (Xen_is_number(Xen_car(ldata))))
    {
      Xen lst;
      len = Xen_list_length(ldata);
      /* just one graph to display */
      lg = cp->lisp_info;
      lg->env_data = 1;
      if (lg->len[0] != len)
	{
	  if (lg->data[0]) free(lg->data[0]);
	  lg->data[0] = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	  lg->len[0] = len;
	}
      for (i = 0, lst = Xen_copy_arg(ldata); i < len; i++, lst = Xen_cdr(lst))
	lg->data[0][i] = Xen_real_to_C_double(Xen_car(lst));
      if (ymin > ymax)
	{
	  for (i = 1; i < len; i += 2)
	    {
	      val = lg->data[0][i];
	      if (ymin > val) ymin = val;
	      if (ymax < val) ymax = val;
	    }
	}
      if (!Xen_is_number(x0)) nominal_x0 = lg->data[0][0];
      if (!Xen_is_number(x1)) nominal_x1 = lg->data[0][len - 2];
    }
  else
    {
      int graph;
      lg = cp->lisp_info;
      lg->env_data = 0;
      for (graph = 0; graph < graphs; graph++)
	{
	  vct *v = NULL;
	  if (Xen_is_list(ldata))
	    data = Xen_list_ref(ldata, graph);
	  else data = ldata;
	  v = Xen_to_vct(data);
	  len = mus_vct_length(v);
	  if (lg->len[graph] != len)
	    {
	      if (lg->data[graph]) free(lg->data[graph]);
	      lg->data[graph] = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	      lg->len[graph] = len;
	    }
	  mus_copy_floats(lg->data[graph], mus_vct_data(v), len);
	  if (ymin > ymax)
	    {
	      for (i = 0; i < len; i++)
		{
		  val = lg->data[graph][i];
		  if (ymin > val) ymin = val;
		  if (ymax < val) ymax = val;
		}
	    }
	}
    }

  lg->axis = make_axis_info(cp, nominal_x0, nominal_x1, ymin, ymax, label, nominal_x0, nominal_x1, ymin, ymax, lg->axis);
  lg->axis->y_offset = old_y_offset;

  if (label) free(label);
  if (need_update)
    {
      axis_info *uap = NULL;
      uap = lg->axis;
      uap->height = old_height;
      uap->window_width = ww;
      uap->width = old_width;
      uap->graph_x0 = gx0;
    }
  cp->graph_lisp_on = true;
  if ((!Xen_is_bound(force_display)) || 
      (!Xen_is_false(force_display)))
    {
      if (need_update)
	update_graph(cp);
      else display_channel_lisp_data(cp);
    }
  return(Xen_false);
  /* returning #f here because graph might be last thing in lisp-graph-hook, and we
   *   don't want its return value mistaken for the "pixel_list" that that hook function
   *   can return.
   */
}


#if HAVE_GL
#define S_glSpectrogram "glSpectrogram"
static Xen g_gl_spectrogram(Xen data, Xen gl_list, Xen cutoff, Xen use_dB, Xen min_dB, Xen scale, Xen br, Xen bg, Xen bb)
{
  #define H_glSpectrogram "(" S_glSpectrogram " data gl-list cutoff use-dB min-dB scale br bg bb) takes spectrogram \
data and passes it to openGL.  See snd-gl.scm for an example."
  sono_info *si;
  int i;
  vct *v;

  Xen_check_type(Xen_is_vector(data), data, 1, S_glSpectrogram, "a vector of " S_vct "s");
  Xen_check_type(Xen_is_integer(gl_list), gl_list, 2, S_glSpectrogram, "an integer");
  Xen_check_type(Xen_is_number(cutoff), cutoff, 3, S_glSpectrogram, "a number");
  Xen_check_type(Xen_is_boolean(use_dB), use_dB, 4, S_glSpectrogram, "a boolean");
  Xen_check_type(Xen_is_number(min_dB), min_dB, 5, S_glSpectrogram, "a number");
  Xen_check_type(Xen_is_number(scale), scale, 6, S_glSpectrogram, "a number");
  Xen_check_type(Xen_is_number(br), br, 7, S_glSpectrogram, "a number (red value)");
  Xen_check_type(Xen_is_number(bg), bg, 8, S_glSpectrogram, "a number (green value)");
  Xen_check_type(Xen_is_number(bb), bb, 9, S_glSpectrogram, "a number (blue value)");

  si = (sono_info *)calloc(1, sizeof(sono_info));
  si->active_slices = Xen_vector_length(data);
  si->data = (mus_float_t **)calloc(si->active_slices, sizeof(mus_float_t *));
  v = xen_to_vct(Xen_vector_ref(data, 0));
  si->target_bins = mus_vct_length(v);
  si->scale = Xen_real_to_C_double(scale);
  for (i = 0; i < si->active_slices; i++)
    {
      v = xen_to_vct(Xen_vector_ref(data, i));
      si->data[i] = mus_vct_data(v);
    }
  gl_spectrogram(si, 
		 Xen_integer_to_C_int(gl_list),
		 Xen_real_to_C_double(cutoff),
		 Xen_boolean_to_C_bool(use_dB),
		 Xen_real_to_C_double(min_dB),
		 float_to_rgb(Xen_real_to_C_double(br)),
		 float_to_rgb(Xen_real_to_C_double(bg)),
		 float_to_rgb(Xen_real_to_C_double(bb)));
  free(si->data);
  free(si);
  return(Xen_false);
}
#endif


static Xen g_channel_data(Xen snd, Xen chn)
{
  #define H_channel_data "(" S_channel_data " :optional snd chn) returns the in-core samples associated with the \
given channel."
  chan_info *cp;

  Snd_assert_channel(S_channel_data, snd, chn, 1);

  cp = get_cp(snd, chn, S_channel_data);
  if ((cp) && 
      (cp->sound) && 
      (cp->sound->inuse == SOUND_WRAPPER) &&
      (cp->sounds) &&
      (cp->sounds[0]) &&
      (cp->sounds[0]->buffered_data))
    return(xen_make_vct_wrapper(cp->edits[0]->samples, cp->sounds[0]->buffered_data));
  return(Xen_false);
}


static Xen g_is_variable_graph(Xen index)
{
  #define H_is_variable_graph "(" S_is_variable_graph " :optional snd): " PROC_TRUE " if snd is a variable graph (from " S_make_variable_graph ")."
  snd_info *sp;

  Xen_check_type(Xen_is_integer(index) || xen_is_sound(index), index, 1, S_is_variable_graph, "a sound");

  sp = get_sp(index);
  if (sp)
    return(C_bool_to_Xen_boolean(sp->inuse == SOUND_WRAPPER));
  return(Xen_false);
}


/* free-variable-graph is much too tricky because callbacks can be invoked after the
 *   controlling snd_info struct has been freed.  In Motif it appears to work to call
 *   XtHasCallbacks/XtRemoveAllCallbacks on all children of the parent widget, but
 *   I didn't try the gtk equivalent -- the whole thing is too messy to be trustworthy.
 */

static Xen g_make_variable_graph(Xen container, Xen name, Xen length, Xen srate)
{
  #define H_make_variable_graph "(" S_make_variable_graph " container :optional name length srate) returns a sound index referring \
to a standard Snd channel graph placed in the widget 'container'."

  snd_info *sp;
  chan_info *cp;
  int rate, initial_length;

  Xen_check_type(Xen_is_widget(container), container, 1, S_make_variable_graph, "a widget");
  Xen_check_type(Xen_is_string_or_unbound(name), name, 2, S_make_variable_graph, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(length), length, 3, S_make_variable_graph, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(srate), srate, 4, S_make_variable_graph, "an integer");

  rate = (Xen_is_integer(srate)) ? Xen_integer_to_C_int(srate) : (int)mus_srate();
  initial_length = (Xen_is_integer(length)) ? Xen_integer_to_C_int(length) : 8192;

  sp = make_simple_channel_display(rate, initial_length, WITH_FW_BUTTONS, graph_style(ss), 
				   (widget_t)(Xen_unwrap_widget(container)), WITH_EVENTS);

  if (!sp) /* can only happen if "container" is not a form widget (or perhaps no container XtWindow) */
    Xen_error(Xen_make_error_type("wrong-type-arg"),
	      Xen_list_2(C_string_to_Xen_string(S_make_variable_graph ": container, ~A, must be a Form widget with a legitimate window"),
			 container));

  sp->user_read_only = FILE_READ_ONLY;
  sp->index = find_free_sound_slot_for_channel_display();
  ss->sounds[sp->index] = sp;
  cp = sp->chans[0];
  if (Xen_is_string(name))
    {
      axis_info *ap;
      sp->filename = mus_strdup(Xen_string_to_C_string(name));
      sp->short_filename = mus_strdup(sp->filename);
      ap = cp->axis;
      if (ap->xlabel) free(ap->xlabel);
      ap->xlabel = mus_strdup(sp->filename);
      if (ap->default_xlabel) free(ap->default_xlabel); 
      ap->default_xlabel = NULL;
      if (ap->ylabel) free(ap->ylabel);
      ap->ylabel = NULL;
    }
  else
    {
      sp->filename = mus_strdup("variable");
      sp->short_filename = mus_strdup("var");
    }

  cp->sounds[0] = make_snd_data_buffer_for_simple_channel(initial_length);
  cp->edits[0] = initial_ed_list(0, initial_length - 1);
  cp->edits[0]->origin = mus_strdup(S_make_variable_graph);

  return(C_int_to_Xen_sound(sp->index));
}


static Xen g_zoom_focus_style(void) 
{
  if (zoom_focus_style(ss) != ZOOM_FOCUS_PROC)
    return(C_int_to_Xen_integer((int)zoom_focus_style(ss)));
  return(ss->zoom_focus_proc);
}


static Xen g_set_zoom_focus_style(Xen focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style "): one of " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle \
", or " S_zoom_focus_active ". This determines what zooming centers on (default: " S_zoom_focus_active ").  It can also \
be a function of 6 args (snd chan zx x0 x1 range) that returns the new window left edge as a float."

  Xen_check_type((Xen_is_integer(focus)) || (Xen_is_procedure(focus)), focus, 1, S_set S_zoom_focus_style, "an integer or a function");
  if ((Xen_is_procedure(focus)) && (!(procedure_arity_ok(focus, 6))))
    return(snd_bad_arity_error(S_set S_zoom_focus_style, 
			       C_string_to_Xen_string("zoom focus func should take 4 args"), 
			       focus));
  if (zoom_focus_style(ss) == ZOOM_FOCUS_PROC)
    {
      snd_unprotect_at(ss->zoom_focus_proc_loc);
      ss->zoom_focus_proc = Xen_undefined;
    }
  if (Xen_is_integer(focus))
    {
      int in_choice;
      zoom_focus_t choice;
      in_choice = Xen_integer_to_C_int(focus);
      if (in_choice < 0)
	Xen_out_of_range_error(S_set S_zoom_focus_style, 1, focus,
			       S_zoom_focus_style " should be " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle ", or " S_zoom_focus_active);
      choice = (zoom_focus_t)in_choice;
      if (choice > ZOOM_FOCUS_MIDDLE)
	Xen_out_of_range_error(S_set S_zoom_focus_style, 1, focus, 
			       S_zoom_focus_style " should be " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle ", or " S_zoom_focus_active);
      set_zoom_focus_style(choice);
      return(C_int_to_Xen_integer((int)zoom_focus_style(ss)));
    }
  set_zoom_focus_style(ZOOM_FOCUS_PROC);
  ss->zoom_focus_proc = focus;
  ss->zoom_focus_proc_loc = snd_protect(focus);
  return(focus);
}


static Xen g_with_gl(void) {return(C_bool_to_Xen_boolean(with_gl(ss)));}

static Xen g_set_with_gl(Xen val) 
{
  #define H_with_gl "(" S_with_gl "): " PROC_TRUE " if Snd should use GL graphics"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_with_gl, "a boolean");
#if HAVE_GL
  set_with_gl(Xen_boolean_to_C_bool(val), true);
  for_each_chan(update_graph);
#endif
  return(C_bool_to_Xen_boolean(with_gl(ss)));
}


static Xen g_sync_style(void) 
{
  return(C_int_to_Xen_integer((int)sync_style(ss)));
}


static Xen g_set_sync_style(Xen style) 
{
  int choice;
  #define H_sync_style "(" S_sync_style "): one of " S_sync_none ", " S_sync_all ", " \
", or " S_sync_by_sound ". This determines how channels are grouped when a sound is opened."

  Xen_check_type(Xen_is_integer(style), style, 1, S_set S_sync_style, "a sync choice");
  choice = Xen_integer_to_C_int(style);
  if ((choice < 0) || (choice >= NUM_SYNC_STYLES))
    Xen_out_of_range_error(S_set S_sync_style, 0, style, "style should be one of " S_sync_none ", " S_sync_all ", or " S_sync_by_sound);
  set_sync_style((sync_style_t)choice);
  return(C_int_to_Xen_integer((int)sync_style(ss)));
}



Xen_wrap_1_arg(g_is_variable_graph_w, g_is_variable_graph)
Xen_wrap_4_optional_args(g_make_variable_graph_w, g_make_variable_graph)
Xen_wrap_2_optional_args(g_channel_data_w, g_channel_data)

Xen_wrap_any_args(g_graph_w, g_graph)
Xen_wrap_2_optional_args(g_edits_w, g_edits)
Xen_wrap_3_optional_args(g_peaks_w, g_peaks)
Xen_wrap_2_optional_args(g_edit_hook_w, g_edit_hook)
Xen_wrap_2_optional_args(g_after_edit_hook_w, g_after_edit_hook)
Xen_wrap_2_optional_args(g_undo_hook_w, g_undo_hook)
Xen_wrap_2_optional_args(g_ap_sx_w, g_ap_sx)
Xen_wrap_2_optional_args(g_ap_sy_w, g_ap_sy)
Xen_wrap_2_optional_args(g_ap_zx_w, g_ap_zx)
Xen_wrap_2_optional_args(g_ap_zy_w, g_ap_zy)
Xen_wrap_3_optional_args(g_framples_w, g_framples)
Xen_wrap_3_optional_args(g_maxamp_position_w, g_maxamp_position)
Xen_wrap_3_optional_args(g_maxamp_w, g_maxamp)
Xen_wrap_2_optional_args(g_cursor_position_w, g_cursor_position)
Xen_wrap_2_optional_args(g_edit_position_w, g_edit_position)
Xen_wrap_2_optional_args(g_transform_graph_on_w, g_transform_graph_on)
Xen_wrap_2_optional_args(g_timer_graph_on_w, g_timer_graph_on)
Xen_wrap_2_optional_args(g_lisp_graph_on_w, g_lisp_graph_on)
Xen_wrap_2_optional_args(g_squelch_update_w, g_squelch_update)
Xen_wrap_3_optional_args(g_cursor_w, g_cursor)
Xen_wrap_2_optional_args(g_cursor_style_w, g_cursor_style)
Xen_wrap_2_optional_args(g_tracking_cursor_style_w, g_tracking_cursor_style)
Xen_wrap_2_optional_args(g_cursor_size_w, g_cursor_size)
Xen_wrap_2_optional_args(g_left_sample_w, g_left_sample)
Xen_wrap_2_optional_args(g_right_sample_w, g_right_sample)
Xen_wrap_2_optional_args(g_channel_properties_w, g_channel_properties)
Xen_wrap_3_optional_args(g_channel_property_w, g_channel_property)
Xen_wrap_3_optional_args(g_edit_properties_w, g_edit_properties)
Xen_wrap_4_optional_args(g_edit_property_w, g_edit_property)
Xen_wrap_2_optional_args(g_max_transform_peaks_w, g_max_transform_peaks)
Xen_wrap_2_optional_args(g_show_y_zero_w, g_show_y_zero)
Xen_wrap_2_optional_args(g_show_grid_w, g_show_grid)
Xen_wrap_2_optional_args(g_grid_density_w, g_grid_density)
Xen_wrap_2_optional_args(g_show_sonogram_cursor_w, g_show_sonogram_cursor)
Xen_wrap_2_optional_args(g_show_marks_w, g_show_marks)
Xen_wrap_2_optional_args(g_time_graph_type_w, g_time_graph_type)
Xen_wrap_2_optional_args(g_wavo_hop_w, g_wavo_hop)
Xen_wrap_2_optional_args(g_wavo_trace_w, g_wavo_trace)
Xen_wrap_2_optional_args(g_show_transform_peaks_w, g_show_transform_peaks)
Xen_wrap_2_optional_args(g_zero_pad_w, g_zero_pad)
Xen_wrap_2_optional_args(g_with_verbose_cursor_w, g_with_verbose_cursor)
Xen_wrap_2_optional_args(g_fft_log_frequency_w, g_fft_log_frequency)
Xen_wrap_2_optional_args(g_fft_log_magnitude_w, g_fft_log_magnitude)
Xen_wrap_2_optional_args(g_fft_with_phases_w, g_fft_with_phases)
Xen_wrap_2_optional_args(g_min_dB_w, g_min_dB)
Xen_wrap_2_optional_args(g_wavelet_type_w, g_wavelet_type)
Xen_wrap_2_optional_args(g_spectrum_end_w, g_spectrum_end)
Xen_wrap_2_optional_args(g_spectrum_start_w, g_spectrum_start)
Xen_wrap_2_optional_args(g_spectro_x_angle_w, g_spectro_x_angle)
Xen_wrap_2_optional_args(g_spectro_x_scale_w, g_spectro_x_scale)
Xen_wrap_2_optional_args(g_spectro_y_angle_w, g_spectro_y_angle)
Xen_wrap_2_optional_args(g_spectro_y_scale_w, g_spectro_y_scale)
Xen_wrap_2_optional_args(g_spectro_z_angle_w, g_spectro_z_angle)
Xen_wrap_2_optional_args(g_spectro_z_scale_w, g_spectro_z_scale)
Xen_wrap_2_optional_args(g_fft_window_alpha_w, g_fft_window_alpha)
Xen_wrap_2_optional_args(g_fft_window_beta_w, g_fft_window_beta)
Xen_wrap_2_optional_args(g_spectro_hop_w, g_spectro_hop)
Xen_wrap_2_optional_args(g_transform_size_w, g_transform_size)
Xen_wrap_2_optional_args(g_transform_graph_type_w, g_transform_graph_type)
Xen_wrap_2_optional_args(g_fft_window_w, g_fft_window)
Xen_wrap_2_optional_args(g_transform_type_w, g_transform_type)
Xen_wrap_2_optional_args(g_transform_normalization_w, g_transform_normalization)
Xen_wrap_2_optional_args(g_show_mix_waveforms_w, g_show_mix_waveforms)
Xen_wrap_2_optional_args(g_graph_style_w, g_graph_style)
Xen_wrap_2_optional_args(g_time_graph_style_w, g_time_graph_style)
Xen_wrap_2_optional_args(g_lisp_graph_style_w, g_lisp_graph_style)
Xen_wrap_2_optional_args(g_transform_graph_style_w, g_transform_graph_style)
Xen_wrap_2_optional_args(g_dot_size_w, g_dot_size)
Xen_wrap_2_optional_args(g_x_axis_style_w, g_x_axis_style)
Xen_wrap_2_optional_args(g_beats_per_measure_w, g_beats_per_measure)
Xen_wrap_2_optional_args(g_beats_per_minute_w, g_beats_per_minute)
Xen_wrap_2_optional_args(g_show_axes_w, g_show_axes)
Xen_wrap_2_optional_args(g_graphs_horizontal_w, g_graphs_horizontal)
Xen_wrap_2_optional_args(g_update_time_graph_w, g_update_time_graph)
Xen_wrap_2_optional_args(g_update_lisp_graph_w, g_update_lisp_graph)
Xen_wrap_2_optional_args(g_update_transform_graph_w, g_update_transform_graph)
Xen_wrap_no_args(g_zoom_focus_style_w, g_zoom_focus_style)
Xen_wrap_1_arg(g_set_zoom_focus_style_w, g_set_zoom_focus_style)
Xen_wrap_no_args(g_sync_style_w, g_sync_style)
Xen_wrap_1_arg(g_set_sync_style_w, g_set_sync_style)
Xen_wrap_no_args(g_with_gl_w, g_with_gl)
Xen_wrap_1_arg(g_set_with_gl_w, g_set_with_gl)
#if HAVE_SCHEME
#define g_set_ap_sx_w g_set_ap_sx_reversed
#define g_set_ap_sy_w g_set_ap_sy_reversed
#define g_set_ap_zx_w g_set_ap_zx_reversed
#define g_set_ap_zy_w g_set_ap_zy_reversed
#define g_set_framples_w g_set_framples_reversed
#define g_set_maxamp_w g_set_maxamp_reversed
#define g_set_edit_position_w g_set_edit_position_reversed
#define g_set_transform_graph_on_w g_set_transform_graph_on_reversed
#define g_set_timer_graph_on_w g_set_timer_graph_on_reversed
#define g_set_lisp_graph_on_w g_set_lisp_graph_on_reversed
#define g_set_squelch_update_w g_set_squelch_update_reversed
#define g_set_cursor_w g_set_cursor_reversed
#define g_set_cursor_style_w g_set_cursor_style_reversed
#define g_set_tracking_cursor_style_w g_set_tracking_cursor_style_reversed
#define g_set_cursor_size_w g_set_cursor_size_reversed
#define g_set_left_sample_w g_set_left_sample_reversed
#define g_set_right_sample_w g_set_right_sample_reversed
#define g_set_channel_properties_w g_set_channel_properties_reversed
#define g_set_channel_property_w g_set_channel_property_reversed
#define g_set_edit_properties_w g_set_edit_properties_reversed
#define g_set_edit_property_w g_set_edit_property_reversed
#define g_set_max_transform_peaks_w g_set_max_transform_peaks_reversed
#define g_set_show_y_zero_w g_set_show_y_zero_reversed
#define g_set_show_grid_w g_set_show_grid_reversed
#define g_set_grid_density_w g_set_grid_density_reversed
#define g_set_show_sonogram_cursor_w g_set_show_sonogram_cursor_reversed
#define g_set_show_marks_w g_set_show_marks_reversed
#define g_set_time_graph_type_w g_set_time_graph_type_reversed
#define g_set_wavo_hop_w g_set_wavo_hop_reversed
#define g_set_wavo_trace_w g_set_wavo_trace_reversed
#define g_set_show_transform_peaks_w g_set_show_transform_peaks_reversed
#define g_set_zero_pad_w g_set_zero_pad_reversed
#define g_set_with_verbose_cursor_w g_set_with_verbose_cursor_reversed
#define g_set_fft_log_frequency_w g_set_fft_log_frequency_reversed
#define g_set_fft_log_magnitude_w g_set_fft_log_magnitude_reversed
#define g_set_fft_with_phases_w g_set_fft_with_phases_reversed
#define g_set_min_dB_w g_set_min_dB_reversed
#define g_set_wavelet_type_w g_set_wavelet_type_reversed
#define g_set_spectrum_end_w g_set_spectrum_end_reversed
#define g_set_spectrum_start_w g_set_spectrum_start_reversed
#define g_set_spectro_x_angle_w g_set_spectro_x_angle_reversed
#define g_set_spectro_x_scale_w g_set_spectro_x_scale_reversed
#define g_set_spectro_y_angle_w g_set_spectro_y_angle_reversed
#define g_set_spectro_y_scale_w g_set_spectro_y_scale_reversed
#define g_set_spectro_z_angle_w g_set_spectro_z_angle_reversed
#define g_set_spectro_z_scale_w g_set_spectro_z_scale_reversed
#define g_set_fft_window_alpha_w g_set_fft_window_alpha_reversed
#define g_set_fft_window_beta_w g_set_fft_window_beta_reversed
#define g_set_spectro_hop_w g_set_spectro_hop_reversed
#define g_set_transform_size_w g_set_transform_size_reversed
#define g_set_transform_graph_type_w g_set_transform_graph_type_reversed
#define g_set_fft_window_w g_set_fft_window_reversed
#define g_set_transform_type_w g_set_transform_type_reversed
#define g_set_transform_normalization_w g_set_transform_normalization_reversed
#define g_set_show_mix_waveforms_w g_set_show_mix_waveforms_reversed
#define g_set_graph_style_w g_set_graph_style_reversed
#define g_set_time_graph_style_w g_set_time_graph_style_reversed
#define g_set_lisp_graph_style_w g_set_lisp_graph_style_reversed
#define g_set_transform_graph_style_w g_set_transform_graph_style_reversed
#define g_set_dot_size_w g_set_dot_size_reversed
#define g_set_x_axis_style_w g_set_x_axis_style_reversed
#define g_set_beats_per_measure_w g_set_beats_per_measure_reversed
#define g_set_show_axes_w g_set_show_axes_reversed
#define g_set_beats_per_minute_w g_set_beats_per_minute_reversed
#define g_set_graphs_horizontal_w g_set_graphs_horizontal_reversed
#else
Xen_wrap_3_optional_args(g_set_ap_sx_w, g_set_ap_sx)
Xen_wrap_3_optional_args(g_set_ap_sy_w, g_set_ap_sy)
Xen_wrap_3_optional_args(g_set_ap_zx_w, g_set_ap_zx)
Xen_wrap_3_optional_args(g_set_ap_zy_w, g_set_ap_zy)
Xen_wrap_3_optional_args(g_set_framples_w, g_set_framples)
Xen_wrap_3_optional_args(g_set_maxamp_w, g_set_maxamp)
Xen_wrap_3_optional_args(g_set_edit_position_w, g_set_edit_position)
Xen_wrap_3_optional_args(g_set_transform_graph_on_w, g_set_transform_graph_on)
Xen_wrap_3_optional_args(g_set_timer_graph_on_w, g_set_timer_graph_on)
Xen_wrap_3_optional_args(g_set_lisp_graph_on_w, g_set_lisp_graph_on)
Xen_wrap_3_optional_args(g_set_squelch_update_w, g_set_squelch_update)
Xen_wrap_4_optional_args(g_set_cursor_w, g_set_cursor)
Xen_wrap_3_optional_args(g_set_cursor_style_w, g_set_cursor_style)
Xen_wrap_3_optional_args(g_set_tracking_cursor_style_w, g_set_tracking_cursor_style)
Xen_wrap_3_optional_args(g_set_cursor_size_w, g_set_cursor_size)
Xen_wrap_3_optional_args(g_set_left_sample_w, g_set_left_sample)
Xen_wrap_3_optional_args(g_set_right_sample_w, g_set_right_sample)
Xen_wrap_3_optional_args(g_set_channel_properties_w, g_set_channel_properties)
Xen_wrap_4_optional_args(g_set_channel_property_w, g_set_channel_property)
Xen_wrap_4_optional_args(g_set_edit_properties_w, g_set_edit_properties)
Xen_wrap_5_optional_args(g_set_edit_property_w, g_set_edit_property)
Xen_wrap_3_optional_args(g_set_max_transform_peaks_w, g_set_max_transform_peaks)
Xen_wrap_3_optional_args(g_set_show_y_zero_w, g_set_show_y_zero)
Xen_wrap_3_optional_args(g_set_show_grid_w, g_set_show_grid)
Xen_wrap_3_optional_args(g_set_grid_density_w, g_set_grid_density)
Xen_wrap_3_optional_args(g_set_show_sonogram_cursor_w, g_set_show_sonogram_cursor)
Xen_wrap_3_optional_args(g_set_show_marks_w, g_set_show_marks)
Xen_wrap_3_optional_args(g_set_time_graph_type_w, g_set_time_graph_type)
Xen_wrap_3_optional_args(g_set_wavo_hop_w, g_set_wavo_hop)
Xen_wrap_3_optional_args(g_set_wavo_trace_w, g_set_wavo_trace)
Xen_wrap_3_optional_args(g_set_show_transform_peaks_w, g_set_show_transform_peaks)
Xen_wrap_3_optional_args(g_set_zero_pad_w, g_set_zero_pad)
Xen_wrap_3_optional_args(g_set_with_verbose_cursor_w, g_set_with_verbose_cursor)
Xen_wrap_3_optional_args(g_set_fft_log_frequency_w, g_set_fft_log_frequency)
Xen_wrap_3_optional_args(g_set_fft_log_magnitude_w, g_set_fft_log_magnitude)
Xen_wrap_3_optional_args(g_set_fft_with_phases_w, g_set_fft_with_phases)
Xen_wrap_3_optional_args(g_set_min_dB_w, g_set_min_dB)
Xen_wrap_3_optional_args(g_set_wavelet_type_w, g_set_wavelet_type)
Xen_wrap_3_optional_args(g_set_spectrum_end_w, g_set_spectrum_end)
Xen_wrap_3_optional_args(g_set_spectrum_start_w, g_set_spectrum_start)
Xen_wrap_3_optional_args(g_set_spectro_x_angle_w, g_set_spectro_x_angle)
Xen_wrap_3_optional_args(g_set_spectro_x_scale_w, g_set_spectro_x_scale)
Xen_wrap_3_optional_args(g_set_spectro_y_angle_w, g_set_spectro_y_angle)
Xen_wrap_3_optional_args(g_set_spectro_y_scale_w, g_set_spectro_y_scale)
Xen_wrap_3_optional_args(g_set_spectro_z_angle_w, g_set_spectro_z_angle)
Xen_wrap_3_optional_args(g_set_spectro_z_scale_w, g_set_spectro_z_scale)
Xen_wrap_3_optional_args(g_set_fft_window_alpha_w, g_set_fft_window_alpha)
Xen_wrap_3_optional_args(g_set_fft_window_beta_w, g_set_fft_window_beta)
Xen_wrap_3_optional_args(g_set_spectro_hop_w, g_set_spectro_hop)
Xen_wrap_3_optional_args(g_set_transform_size_w, g_set_transform_size)
Xen_wrap_3_optional_args(g_set_transform_graph_type_w, g_set_transform_graph_type)
Xen_wrap_3_optional_args(g_set_fft_window_w, g_set_fft_window)
Xen_wrap_3_optional_args(g_set_transform_type_w, g_set_transform_type)
Xen_wrap_3_optional_args(g_set_transform_normalization_w, g_set_transform_normalization)
Xen_wrap_3_optional_args(g_set_show_mix_waveforms_w, g_set_show_mix_waveforms)
Xen_wrap_3_optional_args(g_set_graph_style_w, g_set_graph_style)
Xen_wrap_3_optional_args(g_set_time_graph_style_w, g_set_time_graph_style)
Xen_wrap_3_optional_args(g_set_lisp_graph_style_w, g_set_lisp_graph_style)
Xen_wrap_3_optional_args(g_set_transform_graph_style_w, g_set_transform_graph_style)
Xen_wrap_3_optional_args(g_set_dot_size_w, g_set_dot_size)
Xen_wrap_3_optional_args(g_set_x_axis_style_w, g_set_x_axis_style)
Xen_wrap_3_optional_args(g_set_beats_per_measure_w, g_set_beats_per_measure)
Xen_wrap_3_optional_args(g_set_show_axes_w, g_set_show_axes)
Xen_wrap_3_optional_args(g_set_beats_per_minute_w, g_set_beats_per_minute)
Xen_wrap_3_optional_args(g_set_graphs_horizontal_w, g_set_graphs_horizontal)
#endif
#if HAVE_GL
  Xen_wrap_9_args(g_gl_spectrogram_w, g_gl_spectrogram)
#endif

#if HAVE_SCHEME
static s7_pointer acc_transform_type(s7_scheme *sc, s7_pointer args) {return(g_set_transform_type(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_show_transform_peaks(s7_scheme *sc, s7_pointer args) {return(g_set_show_transform_peaks(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_show_y_zero(s7_scheme *sc, s7_pointer args) {return(g_set_show_y_zero(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_show_marks(s7_scheme *sc, s7_pointer args) {return(g_set_show_marks(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_show_grid(s7_scheme *sc, s7_pointer args) {return(g_set_show_grid(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_fft_log_frequency(s7_scheme *sc, s7_pointer args) {return(g_set_fft_log_frequency(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_fft_log_magnitude(s7_scheme *sc, s7_pointer args) {return(g_set_fft_log_magnitude(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_fft_with_phases(s7_scheme *sc, s7_pointer args) {return(g_set_fft_with_phases(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_sync_style(s7_scheme *sc, s7_pointer args) {return(g_set_sync_style(s7_cadr(args)));}
static s7_pointer acc_show_axes(s7_scheme *sc, s7_pointer args) {return(g_set_show_axes(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_with_verbose_cursor(s7_scheme *sc, s7_pointer args) {return(g_set_with_verbose_cursor(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_x_scale(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_x_scale(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_y_scale(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_y_scale(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_z_scale(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_z_scale(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_z_angle(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_z_angle(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_x_angle(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_x_angle(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_y_angle(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_y_angle(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectrum_end(s7_scheme *sc, s7_pointer args) {return(g_set_spectrum_end(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectrum_start(s7_scheme *sc, s7_pointer args) {return(g_set_spectrum_start(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_spectro_hop(s7_scheme *sc, s7_pointer args) {return(g_set_spectro_hop(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_wavelet_type(s7_scheme *sc, s7_pointer args) {return(g_set_wavelet_type(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_dot_size(s7_scheme *sc, s7_pointer args) {return(g_set_dot_size(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_zero_pad(s7_scheme *sc, s7_pointer args) {return(g_set_zero_pad(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_wavo_hop(s7_scheme *sc, s7_pointer args) {return(g_set_wavo_hop(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_wavo_trace(s7_scheme *sc, s7_pointer args) {return(g_set_wavo_trace(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_transform_size(s7_scheme *sc, s7_pointer args) {return(g_set_transform_size(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_fft_window(s7_scheme *sc, s7_pointer args) {return(g_set_fft_window(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_transform_graph_type(s7_scheme *sc, s7_pointer args) {return(g_set_transform_graph_type(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_time_graph_type(s7_scheme *sc, s7_pointer args) {return(g_set_time_graph_type(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_fft_window_alpha(s7_scheme *sc, s7_pointer args) {return(g_set_fft_window_alpha(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_fft_window_beta(s7_scheme *sc, s7_pointer args) {return(g_set_fft_window_beta(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_grid_density(s7_scheme *sc, s7_pointer args) {return(g_set_grid_density(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_beats_per_minute(s7_scheme *sc, s7_pointer args) {return(g_set_beats_per_minute(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_show_mix_waveforms(s7_scheme *sc, s7_pointer args) {return(g_set_show_mix_waveforms(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_beats_per_measure(s7_scheme *sc, s7_pointer args) {return(g_set_beats_per_measure(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_transform_normalization(s7_scheme *sc, s7_pointer args) {return(g_set_transform_normalization(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_x_axis_style(s7_scheme *sc, s7_pointer args) {return(g_set_x_axis_style(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_zoom_focus_style(s7_scheme *sc, s7_pointer args) {return(g_set_zoom_focus_style(s7_cadr(args)));}
static s7_pointer acc_graph_style(s7_scheme *sc, s7_pointer args) {return(g_set_graph_style(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_max_transform_peaks(s7_scheme *sc, s7_pointer args) {return(g_set_max_transform_peaks(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_graphs_horizontal(s7_scheme *sc, s7_pointer args) {return(g_set_graphs_horizontal(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_cursor_size(s7_scheme *sc, s7_pointer args) {return(g_set_cursor_size(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_cursor_style(s7_scheme *sc, s7_pointer args) {return(g_set_cursor_style(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_tracking_cursor_style(s7_scheme *sc, s7_pointer args) {return(g_set_tracking_cursor_style(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_show_sonogram_cursor(s7_scheme *sc, s7_pointer args) {return(g_set_show_sonogram_cursor(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_min_dB(s7_scheme *sc, s7_pointer args) {return(g_set_min_dB(s7_cadr(args), s7_undefined(sc), s7_undefined(sc)));}
static s7_pointer acc_with_gl(s7_scheme *sc, s7_pointer args) {return(g_set_with_gl(s7_cadr(args)));}
#endif



void g_init_chn(void)
{
#if HAVE_SCHEME
  s7_pointer i, b, p, t, h, r, s, fv, pl_itt, pl_itti, pl_rtt, pl_rttr, pl_btt, pl_bttb, pl_ptt, pl_pttp, plc_t, pl_pttt, pl_ptttp, pl_bitt, pl_bitti;
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "list?");
  h = s7_make_symbol(s7, "procedure?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  fv = s7_make_symbol(s7, "float-vector?");
  t = s7_t(s7);
  pl_itt = s7_make_signature(s7, 3, i, t, t);
  pl_itti = s7_make_signature(s7, 4, i, t, t, i);
  pl_bitt = s7_make_signature(s7, 3, s7_make_signature(s7, 2, i, b), t, t);
  pl_bitti = s7_make_signature(s7, 4, s7_make_signature(s7, 2, i, b), t, t, i);
  pl_rtt = s7_make_signature(s7, 3, r, t, t);
  pl_rttr = s7_make_signature(s7, 4, r, t, t, r);
  pl_btt = s7_make_signature(s7, 3, b, t, t);
  pl_bttb = s7_make_signature(s7, 4, b, t, t, b);
  pl_ptt = s7_make_signature(s7, 3, p, t, t);
  pl_pttp = s7_make_signature(s7, 4, p, t, t, p);
  pl_pttt = s7_make_signature(s7, 4, p, t, t, t);
  pl_ptttp = s7_make_signature(s7, 5, p, t, t, t, p);
  plc_t = s7_make_circular_signature(s7, 0, 1, t);
#endif

  cp_edpos = Xen_undefined;

  Xen_define_typed_procedure(S_is_variable_graph,       g_is_variable_graph_w,      1, 0, 0, H_is_variable_graph,   s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_make_variable_graph,     g_make_variable_graph_w,    1, 3, 0, H_make_variable_graph, s7_make_signature(s7, 5, t, t, s, i, i));
  Xen_define_typed_procedure(S_graph,                   g_graph_w,                  0, 0, 1, H_graph, s7_make_signature(s7, 11, t, t, s, r, r, r, r, t, t, b, i));

  Xen_define_typed_procedure(S_channel_data,            g_channel_data_w,           0, 2, 0, H_channel_data,    s7_make_signature(s7, 3, fv, t, t));
  Xen_define_typed_procedure(S_edits,                   g_edits_w,                  0, 2, 0, H_edits,           pl_ptt);
  Xen_define_typed_procedure(S_peaks,                   g_peaks_w,                  0, 3, 0, H_peaks,           s7_make_signature(s7, 4, s, s, t, t));

  Xen_define_unsafe_typed_procedure(S_edit_hook,        g_edit_hook_w,              0, 2, 0, H_edit_hook,       s7_make_signature(s7, 3, h, t, t));
  Xen_define_unsafe_typed_procedure(S_after_edit_hook,  g_after_edit_hook_w,        0, 2, 0, H_after_edit_hook, s7_make_signature(s7, 3, h, t, t));
  Xen_define_unsafe_typed_procedure(S_undo_hook,        g_undo_hook_w,              0, 2, 0, H_undo_hook,       s7_make_signature(s7, 3, h, t, t));

  Xen_define_typed_procedure(S_update_time_graph,       g_update_time_graph_w,      0, 2, 0, H_update_time_graph,      pl_btt);
  Xen_define_typed_procedure(S_update_lisp_graph,       g_update_lisp_graph_w,      0, 2, 0, H_update_lisp_graph,      pl_btt);
  Xen_define_typed_procedure(S_update_transform_graph,  g_update_transform_graph_w, 0, 2, 0, H_update_transform_graph, pl_btt);

  Xen_define_typed_dilambda(S_x_position_slider, g_ap_sx_w, H_x_position_slider, 
			    S_set S_x_position_slider, g_set_ap_sx_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_y_position_slider, g_ap_sy_w, H_y_position_slider, 
			    S_set S_y_position_slider, g_set_ap_sy_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_x_zoom_slider, g_ap_zx_w, H_x_zoom_slider, 
			    S_set S_x_zoom_slider, g_set_ap_zx_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_y_zoom_slider, g_ap_zy_w, H_y_zoom_slider, 
			    S_set S_y_zoom_slider, g_set_ap_zy_w, 0, 2, 1, 2, pl_rtt, pl_rttr);

  Xen_define_typed_dilambda(S_framples, g_framples_w, H_framples, S_set S_framples, g_set_framples_w, 0, 3, 1, 2,
			    s7_make_signature(s7, 4, i, t, t, t), s7_make_signature(s7, 4, i, t, t, i));
  Xen_define_typed_dilambda(S_maxamp, g_maxamp_w, H_maxamp, S_set S_maxamp, g_set_maxamp_w, 0, 3, 1, 2, plc_t, plc_t);

  Xen_define_typed_procedure(S_maxamp_position,   g_maxamp_position_w, 0, 3, 0,   H_maxamp_position, s7_make_signature(s7, 4, i, t, t, t));
  Xen_define_typed_procedure(S_cursor_position,   g_cursor_position_w, 0, 2, 0,   H_cursor_position, pl_ptt);

  Xen_define_typed_dilambda(S_edit_position, g_edit_position_w, H_edit_position, 
			    S_set S_edit_position, g_set_edit_position_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_transform_graph_on, g_transform_graph_on_w, H_transform_graph_on, 
			    S_set S_transform_graph_on, g_set_transform_graph_on_w, 0, 2, 1, 2, pl_btt, pl_bttb);

  #define H_graph_once "The value for the various graph-type variables that displays the standard waveform"
  #define H_graph_as_wavogram "The value for " S_time_graph_type " to make a spectrogram-like form of the time-domain data"

  Xen_define_constant(S_graph_once,        GRAPH_ONCE,        H_graph_once);
  Xen_define_constant(S_graph_as_wavogram, GRAPH_AS_WAVOGRAM, H_graph_as_wavogram);

  #define H_graph_as_sonogram "The value for " S_transform_graph_type " that causes a sonogram to be displayed"
  #define H_graph_as_spectrogram "The value for " S_transform_graph_type " that causes a spectrogram to be displayed"
  /* #define H_graph_as_complex "The value for " S_transform_graph_type " to display FFT data in the complex plane" */

  Xen_define_constant(S_graph_as_sonogram,    GRAPH_AS_SONOGRAM,    H_graph_as_sonogram);
  Xen_define_constant(S_graph_as_spectrogram, GRAPH_AS_SPECTROGRAM, H_graph_as_spectrogram);
  /* Xen_define_constant(S_graph_as_complex,     GRAPH_AS_COMPLEX,     H_graph_as_complex); */

  Xen_define_typed_dilambda(S_time_graph_on, g_timer_graph_on_w, H_timer_graph_on, 
			    S_set S_time_graph_on, g_set_timer_graph_on_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_lisp_graph_on, g_lisp_graph_on_w, H_lisp_graph_on, 
			    S_set S_lisp_graph_on, g_set_lisp_graph_on_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_squelch_update, g_squelch_update_w, H_squelch_update, 
			    S_set S_squelch_update, g_set_squelch_update_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_cursor, g_cursor_w, H_cursor, 
			    S_set S_cursor, g_set_cursor_w, 0, 3, 1, 3, s7_make_signature(s7, 4, i, t, t, t), s7_make_signature(s7, 5, i, t, t, t, i));
  
  #define H_cursor_cross "The value for " S_cursor_style " that causes is to be a cross (the default)"
  #define H_cursor_line "The value for " S_cursor_style " that causes is to be a full vertical line"

  Xen_define_constant(S_cursor_cross,          CURSOR_CROSS, H_cursor_cross);
  Xen_define_constant(S_cursor_line,           CURSOR_LINE,  H_cursor_line);

  Xen_define_typed_dilambda(S_cursor_style, g_cursor_style_w, H_cursor_style, 
			    S_set S_cursor_style, g_set_cursor_style_w, 0, 2, 1, 2, plc_t, plc_t);
  Xen_define_typed_dilambda(S_tracking_cursor_style, g_tracking_cursor_style_w, H_tracking_cursor_style, 
			    S_set S_tracking_cursor_style, g_set_tracking_cursor_style_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_cursor_size, g_cursor_size_w, H_cursor_size, 
			    S_set S_cursor_size, g_set_cursor_size_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_left_sample, g_left_sample_w, H_left_sample, 
			    S_set S_left_sample, g_set_left_sample_w, 0, 2, 1, 2, pl_bitt, pl_bitti);
  Xen_define_typed_dilambda(S_right_sample, g_right_sample_w, H_right_sample, 
			    S_set S_right_sample, g_set_right_sample_w, 0, 2, 1, 2, pl_bitt, pl_bitti);
  Xen_define_typed_dilambda(S_channel_properties, g_channel_properties_w, H_channel_properties, 
			    S_set S_channel_properties, g_set_channel_properties_w, 0, 2, 1, 2, pl_ptt, pl_pttp);
  Xen_define_typed_dilambda(S_channel_property, g_channel_property_w, H_channel_property, 
			    S_set S_channel_property, g_set_channel_property_w, 1, 2, 2, 2, plc_t, plc_t);
  Xen_define_typed_dilambda(S_edit_properties, g_edit_properties_w, H_edit_properties, 
			    S_set S_edit_properties, g_set_edit_properties_w, 0, 3, 1, 3, pl_pttt, pl_ptttp);
  Xen_define_typed_dilambda(S_edit_property, g_edit_property_w, H_edit_property, 
			    S_set S_edit_property, g_set_edit_property_w, 1, 3, 2, 3, plc_t, plc_t);
  Xen_define_typed_dilambda(S_max_transform_peaks, g_max_transform_peaks_w, H_max_transform_peaks, 
			    S_set S_max_transform_peaks, g_set_max_transform_peaks_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_show_y_zero, g_show_y_zero_w, H_show_y_zero, 
			    S_set S_show_y_zero, g_set_show_y_zero_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_show_grid, g_show_grid_w, H_show_grid, 
			    S_set S_show_grid, g_set_show_grid_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_grid_density, g_grid_density_w, H_grid_density, 
			    S_set S_grid_density, g_set_grid_density_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_show_sonogram_cursor, g_show_sonogram_cursor_w, H_show_sonogram_cursor, 
			    S_set S_show_sonogram_cursor, g_set_show_sonogram_cursor_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_show_marks, g_show_marks_w, H_show_marks, 
			    S_set S_show_marks, g_set_show_marks_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_time_graph_type, g_time_graph_type_w, H_time_graph_type, 
			    S_set S_time_graph_type, g_set_time_graph_type_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_wavo_hop, g_wavo_hop_w, H_wavo_hop, 
			    S_set S_wavo_hop, g_set_wavo_hop_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_wavo_trace, g_wavo_trace_w, H_wavo_trace, 
			    S_set S_wavo_trace, g_set_wavo_trace_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_show_transform_peaks, g_show_transform_peaks_w, H_show_transform_peaks, 
			    S_set S_show_transform_peaks, g_set_show_transform_peaks_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_zero_pad, g_zero_pad_w, H_zero_pad, 
			    S_set S_zero_pad, g_set_zero_pad_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_with_verbose_cursor, g_with_verbose_cursor_w, H_with_verbose_cursor, 
			    S_set S_with_verbose_cursor, g_set_with_verbose_cursor_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_fft_log_frequency, g_fft_log_frequency_w, H_fft_log_frequency, 
			    S_set S_fft_log_frequency, g_set_fft_log_frequency_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_fft_log_magnitude, g_fft_log_magnitude_w, H_fft_log_magnitude, 
			    S_set S_fft_log_magnitude, g_set_fft_log_magnitude_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_fft_with_phases, g_fft_with_phases_w, H_fft_with_phases, 
			    S_set S_fft_with_phases, g_set_fft_with_phases_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  Xen_define_typed_dilambda(S_min_dB, g_min_dB_w, H_min_dB, 
			    S_set S_min_dB, g_set_min_dB_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_wavelet_type, g_wavelet_type_w, H_wavelet_type, 
			    S_set S_wavelet_type, g_set_wavelet_type_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_spectrum_end, g_spectrum_end_w, H_spectrum_end, 
			    S_set S_spectrum_end, g_set_spectrum_end_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectrum_start, g_spectrum_start_w, H_spectrum_start, 
			    S_set S_spectrum_start, g_set_spectrum_start_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_x_angle, g_spectro_x_angle_w, H_spectro_x_angle, 
			    S_set S_spectro_x_angle, g_set_spectro_x_angle_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_x_scale, g_spectro_x_scale_w, H_spectro_x_scale, 
			    S_set S_spectro_x_scale, g_set_spectro_x_scale_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_y_angle, g_spectro_y_angle_w, H_spectro_y_angle, 
			    S_set S_spectro_y_angle, g_set_spectro_y_angle_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_y_scale, g_spectro_y_scale_w, H_spectro_y_scale, 
			    S_set S_spectro_y_scale, g_set_spectro_y_scale_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_z_angle, g_spectro_z_angle_w, H_spectro_z_angle, 
			    S_set S_spectro_z_angle, g_set_spectro_z_angle_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_z_scale, g_spectro_z_scale_w, H_spectro_z_scale, 
			    S_set S_spectro_z_scale, g_set_spectro_z_scale_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_fft_window_beta, g_fft_window_beta_w, H_fft_window_beta, 
			    S_set S_fft_window_beta, g_set_fft_window_beta_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_fft_window_alpha, g_fft_window_alpha_w, H_fft_window_alpha, 
			    S_set S_fft_window_alpha, g_set_fft_window_alpha_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_spectro_hop, g_spectro_hop_w, H_spectro_hop, 
			    S_set S_spectro_hop, g_set_spectro_hop_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_transform_size, g_transform_size_w, H_transform_size, 
			    S_set S_transform_size, g_set_transform_size_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_transform_graph_type, g_transform_graph_type_w, H_transform_graph_type, 
			    S_set S_transform_graph_type, g_set_transform_graph_type_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_fft_window, g_fft_window_w, H_fft_window, 
			    S_set S_fft_window, g_set_fft_window_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_transform_type, g_transform_type_w, H_transform_type, 
			    S_set S_transform_type, g_set_transform_type_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_transform_normalization, g_transform_normalization_w, H_transform_normalization, 
			    S_set S_transform_normalization, g_set_transform_normalization_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_show_mix_waveforms, g_show_mix_waveforms_w, H_show_mix_waveforms, 
			    S_set S_show_mix_waveforms, g_set_show_mix_waveforms_w, 0, 2, 1, 2, pl_btt, pl_bttb);
  
  /* should these be named "graph-with-lines" etc? */
  #define H_graph_lines "The value for " S_graph_style " that causes graphs to use line-segments"
  #define H_graph_dots "The value for " S_graph_style " that causes graphs to use dots"
  #define H_graph_filled "The value for " S_graph_style " that causes graphs to use filled polygons"
  #define H_graph_dots_and_lines "The value for " S_graph_style " that causes graphs to use dots connected by lines"
  #define H_graph_lollipops "The value for " S_graph_style " that makes DSP engineers happy"
  
  Xen_define_constant(S_graph_lines,           GRAPH_LINES,          H_graph_lines);
  Xen_define_constant(S_graph_dots,            GRAPH_DOTS,           H_graph_dots);
  Xen_define_constant(S_graph_filled,          GRAPH_FILLED,         H_graph_filled);
  Xen_define_constant(S_graph_dots_and_lines,  GRAPH_DOTS_AND_LINES, H_graph_dots_and_lines);
  Xen_define_constant(S_graph_lollipops,       GRAPH_LOLLIPOPS,      H_graph_lollipops);
  
  Xen_define_typed_dilambda(S_time_graph_style, g_time_graph_style_w, H_time_graph_style, 
			    S_set S_time_graph_style, g_set_time_graph_style_w, 1, 1, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_lisp_graph_style, g_lisp_graph_style_w, H_lisp_graph_style, 
			    S_set S_lisp_graph_style, g_set_lisp_graph_style_w, 1, 1, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_transform_graph_style, g_transform_graph_style_w, H_transform_graph_style, 
			    S_set S_transform_graph_style, g_set_transform_graph_style_w, 1, 1, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_graph_style, g_graph_style_w, H_graph_style, 
			    S_set S_graph_style, g_set_graph_style_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_dot_size, g_dot_size_w, H_dot_size, 
			    S_set S_dot_size, g_set_dot_size_w, 0, 2, 1, 2, pl_itt, pl_itti);

  #define H_x_axis_in_seconds    "The value for " S_x_axis_style " that displays the x axis using seconds"
  #define H_x_axis_in_samples    "The value for " S_x_axis_style " that displays the x axis using sample numbers"
  #define H_x_axis_in_beats      "The value for " S_x_axis_style " that displays the x axis using beats (also " S_beats_per_minute ")"
  #define H_x_axis_in_measures   "The value for " S_x_axis_style " that displays the x axis using measure numbers"
  #define H_x_axis_as_percentage "The value for " S_x_axis_style " that displays the x axis using percentages"
  #define H_x_axis_as_clock      "The value for " S_x_axis_style " that displays the x axis using clock-like DD:HH:MM:SS syntax"

  Xen_define_constant(S_x_axis_in_seconds,     X_AXIS_IN_SECONDS,    H_x_axis_in_seconds);
  Xen_define_constant(S_x_axis_in_samples,     X_AXIS_IN_SAMPLES,    H_x_axis_in_samples);
  Xen_define_constant(S_x_axis_in_beats,       X_AXIS_IN_BEATS,      H_x_axis_in_beats);
  Xen_define_constant(S_x_axis_in_measures,    X_AXIS_IN_MEASURES,   H_x_axis_in_measures);
  Xen_define_constant(S_x_axis_as_percentage,  X_AXIS_AS_PERCENTAGE, H_x_axis_as_percentage);
  Xen_define_constant(S_x_axis_as_clock,       X_AXIS_AS_CLOCK,      H_x_axis_as_clock);

  Xen_define_typed_dilambda(S_x_axis_style, g_x_axis_style_w, H_x_axis_style, 
			    S_set S_x_axis_style, g_set_x_axis_style_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_beats_per_minute, g_beats_per_minute_w, H_beats_per_minute, 
			    S_set S_beats_per_minute, g_set_beats_per_minute_w, 0, 2, 1, 2, pl_rtt, pl_rttr);
  Xen_define_typed_dilambda(S_beats_per_measure, g_beats_per_measure_w, H_beats_per_measure, 
			    S_set S_beats_per_measure, g_set_beats_per_measure_w, 0, 2, 1, 2, pl_itt, pl_itti);

  #define H_show_all_axes "The value for " S_show_axes " that causes both the x and y axes to be displayed"
  #define H_show_all_axes_unlabelled "The value for " S_show_axes " that causes both the x and y axes to be displayed, but without any label"
  #define H_show_no_axes "The value for " S_show_axes " that causes neither the x or y axes to be displayed"
  #define H_show_x_axis "The value for " S_show_axes " that causes only the x axis to be displayed"
  #define H_show_x_axis_unlabelled "The value for " S_show_axes " that causes only the x axis to be displayed, but without any label"
  #define H_show_bare_x_axis "The value for " S_show_axes " that causes x axis to be displayed without a label or tick marks"

  Xen_define_constant(S_show_all_axes,           SHOW_ALL_AXES,            H_show_all_axes);
  Xen_define_constant(S_show_all_axes_unlabelled,SHOW_ALL_AXES_UNLABELLED, H_show_all_axes_unlabelled);
  Xen_define_constant(S_show_no_axes,            SHOW_NO_AXES,             H_show_no_axes);
  Xen_define_constant(S_show_x_axis,             SHOW_X_AXIS,              H_show_x_axis);
  Xen_define_constant(S_show_x_axis_unlabelled,  SHOW_X_AXIS_UNLABELLED,   H_show_x_axis_unlabelled);
  Xen_define_constant(S_show_bare_x_axis,        SHOW_BARE_X_AXIS,         H_show_bare_x_axis);

  Xen_define_typed_dilambda(S_show_axes, g_show_axes_w, H_show_axes, 
			    S_set S_show_axes, g_set_show_axes_w, 0, 2, 1, 2, pl_itt, pl_itti);
  Xen_define_typed_dilambda(S_graphs_horizontal, g_graphs_horizontal_w, H_graphs_horizontal, 
			    S_set S_graphs_horizontal, g_set_graphs_horizontal_w, 0, 2, 1, 2, pl_btt, pl_bttb);

  #define H_zoom_focus_left "The value for " S_zoom_focus_style " that causes zooming to maintain the left edge steady"
  #define H_zoom_focus_right "The value for " S_zoom_focus_style " that causes zooming to maintain the right edge steady"
  #define H_zoom_focus_middle "The value for " S_zoom_focus_style " that causes zooming to focus on the middle sample"
  #define H_zoom_focus_active "The value for " S_zoom_focus_style " that causes zooming to focus on the currently active object"

  Xen_define_constant(S_zoom_focus_left,       ZOOM_FOCUS_LEFT,   H_zoom_focus_left);
  Xen_define_constant(S_zoom_focus_right,      ZOOM_FOCUS_RIGHT,  H_zoom_focus_right);
  Xen_define_constant(S_zoom_focus_active,     ZOOM_FOCUS_ACTIVE, H_zoom_focus_active);
  Xen_define_constant(S_zoom_focus_middle,     ZOOM_FOCUS_MIDDLE, H_zoom_focus_middle);

  Xen_define_typed_dilambda(S_zoom_focus_style, g_zoom_focus_style_w, H_zoom_focus_style, 
			    S_set S_zoom_focus_style, g_set_zoom_focus_style_w,  0, 0, 1, 0, s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));
  Xen_define_typed_dilambda(S_with_gl, g_with_gl_w, H_with_gl, 
			    S_set S_with_gl, g_set_with_gl_w,  0, 0, 1, 0, s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));

  #define H_sync_none     "The " S_sync_style " choice that leaves every sound and channel unsync'd at the start"
  #define H_sync_all      "The " S_sync_style " choice that syncs together every sound and channel at the start"
  #define H_sync_by_sound "The " S_sync_style " choice that syncs all channels in a sound, but each sound is separate"

  Xen_define_constant(S_sync_none,     SYNC_NONE,     H_sync_none);
  Xen_define_constant(S_sync_all,      SYNC_ALL,      H_sync_all);
  Xen_define_constant(S_sync_by_sound, SYNC_BY_SOUND, H_sync_by_sound);

  Xen_define_typed_dilambda(S_sync_style, g_sync_style_w, H_sync_style, 
			    S_set S_sync_style, g_set_sync_style_w,  0, 0, 1, 0, s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

#if HAVE_GL
  Xen_define_typed_procedure(S_glSpectrogram, g_gl_spectrogram_w, 9, 0, 0, H_glSpectrogram,
			     s7_make_signature(s7, 10, b, s7_make_symbol(s7, "vector?"), i, r, b, r, r, r, r, r));
#endif

  #define H_after_transform_hook S_after_transform_hook " (snd chn scaler): called just after a spectrum is calculated."
  #define H_graph_hook S_graph_hook " (snd chn y0 y1): called each time a graph is about to be updated. If it returns " PROC_TRUE ", the display is not updated."
  #define H_after_graph_hook S_after_graph_hook " (snd chn): called after a graph is updated."
  #define H_lisp_graph_hook S_lisp_graph_hook " (snd chn): called just before the lisp graph is updated. If it returns a list \
of pixels, these are used in order by the list of graphs (if any), rather than Snd's default set; \
this makes it possible to use different colors for the various graphs. \
If it returns a function (of no arguments), that function is called rather than the standard graph routine."
  #define H_after_lisp_graph_hook S_after_lisp_graph_hook " (snd chn): called after a lisp graph is updated."
  #define H_mouse_press_hook S_mouse_press_hook " (snd chn button state x y): called upon mouse button press within the lisp graph."
  #define H_mouse_click_hook S_mouse_click_hook " (snd chn button state x y axis): called upon button click."
  #define H_mouse_drag_hook S_mouse_drag_hook " (snd chn button state x y): called upon mouse drag within the lisp graph."
  #define H_mark_click_hook S_mark_click_hook " (id): called when a mark is clicked; return " PROC_TRUE " to squelch the default message."
  #define H_mix_click_hook S_mix_click_hook " (id): called when a mix is clicked; return " PROC_TRUE " to squelch the default message."
  #define H_key_press_hook S_key_press_hook " (snd chn key state): called upon a key press if the mouse is in the lisp graph. \
If it returns " PROC_TRUE ", the key press is not passed to the main handler. 'state' refers to the control, meta, and shift keys."
  #define H_initial_graph_hook S_initial_graph_hook " (snd chn duration): called when a sound is displayed for the first time"
  
  after_transform_hook =  Xen_define_hook(S_after_transform_hook,   "(make-hook 'snd 'chn 'scaler)",                    3, H_after_transform_hook);
  graph_hook =            Xen_define_hook(S_graph_hook,             "(make-hook 'snd 'chn 'y0 'y1)",                    4, H_graph_hook);
  after_graph_hook =      Xen_define_hook(S_after_graph_hook,       "(make-hook 'snd 'chn)",                            2, H_after_graph_hook); 
  after_lisp_graph_hook = Xen_define_hook(S_after_lisp_graph_hook,  "(make-hook 'snd 'chn)",                            2, H_after_lisp_graph_hook);
  initial_graph_hook =    Xen_define_hook(S_initial_graph_hook,     "(make-hook 'snd 'chn 'duration)",                  3, H_initial_graph_hook);
  lisp_graph_hook =       Xen_define_hook(S_lisp_graph_hook,        "(make-hook 'snd 'chn)",                            2, H_lisp_graph_hook);
  mouse_press_hook =      Xen_define_hook(S_mouse_press_hook,       "(make-hook 'snd 'chn 'button 'state 'x 'y)",       6, H_mouse_press_hook);
  mouse_click_hook =      Xen_define_hook(S_mouse_click_hook,       "(make-hook 'snd 'chn 'button 'state 'x 'y 'axis)", 7, H_mouse_click_hook);
  mouse_drag_hook =       Xen_define_hook(S_mouse_drag_hook,        "(make-hook 'snd 'chn 'button 'state 'x 'y)",       6, H_mouse_drag_hook);
  key_press_hook =        Xen_define_hook(S_key_press_hook,         "(make-hook 'snd 'chn 'key 'state)",                4, H_key_press_hook);
  mark_click_hook =       Xen_define_hook(S_mark_click_hook,        "(make-hook 'id)",                                  1, H_mark_click_hook); 
  mix_click_hook =        Xen_define_hook(S_mix_click_hook,         "(make-hook 'id)",                                  1, H_mix_click_hook);  

#if HAVE_SCHEME
  s7_symbol_set_documentation(s7, ss->show_transform_peaks_symbol, "*" S_show_transform_peaks "* determines whether fft displays include a peak list");
  s7_symbol_set_documentation(s7, ss->show_y_zero_symbol, "*show-y-zero*: #t if Snd should include a line at y = 0.0");
  s7_symbol_set_documentation(s7, ss->show_marks_symbol, "*show-marks*: #t if Snd should show marks");
  s7_symbol_set_documentation(s7, ss->show_grid_symbol, "*show-grid*: #t if Snd should display a background grid in the graphs");
  s7_symbol_set_documentation(s7, ss->fft_log_frequency_symbol, "*fft-log-frequency*: #t if fft displays use log on the frequency axis");
  s7_symbol_set_documentation(s7, ss->fft_log_magnitude_symbol, "*fft-log-magnitude*: #t if fft displays use dB");
  s7_symbol_set_documentation(s7, ss->fft_with_phases_symbol, "*fft-with-phases*: #t if fft displays include phase info");
  s7_symbol_set_documentation(s7, ss->sync_style_symbol, "*sync-style*: determines how channels are grouped when a sound is opened.");
  s7_symbol_set_documentation(s7, ss->show_axes_symbol, "*show-axes*: If show-all-axes, display x and y axes; if show-x-axis, just one axis (the x axis) is displayed. The other choices are show-no-axes, show-all-axes-unlabelled, show-x-axis-unlabelled, and show-bare-x-axis.");
  s7_symbol_set_documentation(s7, ss->with_verbose_cursor_symbol, "*with-verbose-cursor*: #t if the cursor's position and so on is displayed in the status area");
  s7_symbol_set_documentation(s7, ss->spectro_x_scale_symbol, "*spectro-x-scale*: scaler (stretch) along the spectrogram x axis (1.0)");
  s7_symbol_set_documentation(s7, ss->spectro_y_scale_symbol, "*spectro-y-scale*: scaler (stretch) along the spectrogram y axis (1.0)");
  s7_symbol_set_documentation(s7, ss->spectro_z_scale_symbol, "*spectro-z-scale*: scaler (stretch) along the spectrogram z axis (0.1)");
  s7_symbol_set_documentation(s7, ss->spectro_z_angle_symbol, "*spectro-z-angle*: spectrogram z-axis viewing angle (-2.0)");
  s7_symbol_set_documentation(s7, ss->spectro_x_angle_symbol, "*spectro-x-angle*: spectrogram x-axis viewing angle (90.0)");
  s7_symbol_set_documentation(s7, ss->spectro_y_angle_symbol, "*spectro-y-angle*: spectrogram y-axis viewing angle (0.0)");
  s7_symbol_set_documentation(s7, ss->spectrum_end_symbol, "*spectrum-end*: max frequency shown in spectra (1.0 = srate/2)");
  s7_symbol_set_documentation(s7, ss->spectrum_start_symbol, "*spectrum-start*: lower bound of frequency in spectral displays (0.0)");
  s7_symbol_set_documentation(s7, ss->spectro_hop_symbol, "*spectro-hop*: hop amount (pixels) in spectral displays");
  s7_symbol_set_documentation(s7, ss->wavelet_type_symbol, "*wavelet-type*: wavelet used in wavelet-transform (0)");
  s7_symbol_set_documentation(s7, ss->dot_size_symbol, "*dot-size*: size in pixels of dots when graphing with dots (1)");
  s7_symbol_set_documentation(s7, ss->zero_pad_symbol, "*zero-pad*: zero padding used in fft as a multiple of fft size (0)");
  s7_symbol_set_documentation(s7, ss->wavo_hop_symbol, "*wavo-hop*: wavogram spacing between successive traces");
  s7_symbol_set_documentation(s7, ss->wavo_trace_symbol, "*wavo-trace*: length (samples) of each trace in the wavogram (64)");
  s7_symbol_set_documentation(s7, ss->transform_size_symbol, "*transform-size*: current fft size (512)");
  s7_symbol_set_documentation(s7, ss->fft_window_symbol, "*fft-window*: fft data window choice (blackman2-window etc)");
  s7_symbol_set_documentation(s7, ss->transform_graph_type_symbol, "*transform-graph-type* can be graph-once, graph-as-sonogram, or graph-as-spectrogram.");
  s7_symbol_set_documentation(s7, ss->time_graph_type_symbol, "*time-graph-type*: graph-once or graph-as-wavogram");
  s7_symbol_set_documentation(s7, ss->fft_window_alpha_symbol, "*fft-window-alpha*: fft window alpha parameter value");
  s7_symbol_set_documentation(s7, ss->fft_window_beta_symbol, "*fft-window-beta*: fft window beta parameter value");
  s7_symbol_set_documentation(s7, ss->grid_density_symbol, "*grid-density*: sets how closely axis ticks are spaced, default=1.0");
  s7_symbol_set_documentation(s7, ss->beats_per_minute_symbol, "*beats-per-minute*: beats per minute if x-axis-style is x-axis-in-beats");
  s7_symbol_set_documentation(s7, ss->show_mix_waveforms_symbol, "*show-mix-waveforms*: #t if Snd should display mix waveforms (above the main waveform)");
  s7_symbol_set_documentation(s7, ss->beats_per_measure_symbol, "*beats-per-measure*: beats per measure if x-axis-style is x-axis-in-measures");
  s7_symbol_set_documentation(s7, ss->transform_normalization_symbol, "*transform-normalization*: dont-normalize, normalize-by-channel, normalize-by-sound, or normalize-globally.");
  s7_symbol_set_documentation(s7, ss->x_axis_style_symbol, "*x-axis-style*: The x axis labelling of the time domain waveform (x-axis-in-seconds etc)");
  s7_symbol_set_documentation(s7, ss->zoom_focus_style_symbol, "*zoom-focus-style*: determines what zooming centers on (zoom-focus-active etc).");
  s7_symbol_set_documentation(s7, ss->graph_style_symbol, "*graph-style*: graph style (graph-lines etc)");
  s7_symbol_set_documentation(s7, ss->max_transform_peaks_symbol, "*max-transform-peaks*: max number of fft peaks reported in fft display");
  s7_symbol_set_documentation(s7, ss->graphs_horizontal_symbol, "*graphs-horizontal*: #t if the time domain, fft, and lisp graphs are layed out horizontally");
  s7_symbol_set_documentation(s7, ss->cursor_size_symbol, "*cursor-size*: current cursor size");
  s7_symbol_set_documentation(s7, ss->cursor_style_symbol, "*cursor-style*: current cursor shape (cursor-cross etc)");
  s7_symbol_set_documentation(s7, ss->tracking_cursor_style_symbol, "*tracking-cursor-style*: current tracking cursor shape (cursor-cross, cursor-line)");
  s7_symbol_set_documentation(s7, ss->show_sonogram_cursor_symbol, "*show-sonogram-cursor*: #t if Snd should display a cursor in the sonogram");
  s7_symbol_set_documentation(s7, ss->min_db_symbol, "*min-dB*: min dB value displayed in fft graphs using dB scales (-60)");
  s7_symbol_set_documentation(s7, ss->transform_type_symbol, "*transform-type*: transform type (fourier-transform etc)");
  s7_symbol_set_documentation(s7, ss->with_gl_symbol, "*with-gl*: #t if Snd should use GL graphics");

  s7_symbol_set_setter(s7, ss->transform_type_symbol, s7_make_function(s7, "[acc-" S_transform_type "]", acc_transform_type, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_transform_peaks_symbol, s7_make_function(s7, "[acc-" S_show_transform_peaks "]", acc_show_transform_peaks, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_y_zero_symbol, s7_make_function(s7, "[acc-" S_show_y_zero "]", acc_show_y_zero, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_marks_symbol, s7_make_function(s7, "[acc-" S_show_marks "]", acc_show_marks, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_grid_symbol, s7_make_function(s7, "[acc-" S_show_grid "]", acc_show_grid, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->fft_log_frequency_symbol, s7_make_function(s7, "[acc-" S_fft_log_frequency "]", acc_fft_log_frequency, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->fft_log_magnitude_symbol, s7_make_function(s7, "[acc-" S_fft_log_magnitude "]", acc_fft_log_magnitude, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->fft_with_phases_symbol, s7_make_function(s7, "[acc-" S_fft_with_phases "]", acc_fft_with_phases, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->sync_style_symbol, s7_make_function(s7, "[acc-" S_sync_style "]", acc_sync_style, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_axes_symbol, s7_make_function(s7, "[acc-" S_show_axes "]", acc_show_axes, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_verbose_cursor_symbol, s7_make_function(s7, "[acc-" S_with_verbose_cursor "]", acc_with_verbose_cursor, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_x_scale_symbol, s7_make_function(s7, "[acc-" S_spectro_x_scale "]", acc_spectro_x_scale, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_y_scale_symbol, s7_make_function(s7, "[acc-" S_spectro_y_scale "]", acc_spectro_y_scale, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_z_scale_symbol, s7_make_function(s7, "[acc-" S_spectro_z_scale "]", acc_spectro_z_scale, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_z_angle_symbol, s7_make_function(s7, "[acc-" S_spectro_z_angle "]", acc_spectro_z_angle, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_x_angle_symbol, s7_make_function(s7, "[acc-" S_spectro_x_angle "]", acc_spectro_x_angle, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_y_angle_symbol, s7_make_function(s7, "[acc-" S_spectro_y_angle "]", acc_spectro_y_angle, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectrum_end_symbol, s7_make_function(s7, "[acc-" S_spectrum_end "]", acc_spectrum_end, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectrum_start_symbol, s7_make_function(s7, "[acc-" S_spectrum_start "]", acc_spectrum_start, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->spectro_hop_symbol, s7_make_function(s7, "[acc-" S_spectro_hop "]", acc_spectro_hop, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->wavelet_type_symbol, s7_make_function(s7, "[acc-" S_wavelet_type "]", acc_wavelet_type, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->dot_size_symbol, s7_make_function(s7, "[acc-" S_dot_size "]", acc_dot_size, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->zero_pad_symbol, s7_make_function(s7, "[acc-" S_zero_pad "]", acc_zero_pad, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->wavo_hop_symbol, s7_make_function(s7, "[acc-" S_wavo_hop "]", acc_wavo_hop, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->wavo_trace_symbol, s7_make_function(s7, "[acc-" S_wavo_trace "]", acc_wavo_trace, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->transform_size_symbol, s7_make_function(s7, "[acc-" S_transform_size "]", acc_transform_size, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->fft_window_symbol, s7_make_function(s7, "[acc-" S_fft_window "]", acc_fft_window, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->transform_graph_type_symbol, s7_make_function(s7, "[acc-" S_transform_graph_type "]", acc_transform_graph_type, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->time_graph_type_symbol, s7_make_function(s7, "[acc-" S_time_graph_type "]", acc_time_graph_type, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->fft_window_alpha_symbol, s7_make_function(s7, "[acc-" S_fft_window_alpha "]", acc_fft_window_alpha, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->fft_window_beta_symbol, s7_make_function(s7, "[acc-" S_fft_window_beta "]", acc_fft_window_beta, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->grid_density_symbol, s7_make_function(s7, "[acc-" S_grid_density "]", acc_grid_density, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->beats_per_minute_symbol, s7_make_function(s7, "[acc-" S_beats_per_minute "]", acc_beats_per_minute, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_mix_waveforms_symbol, s7_make_function(s7, "[acc-" S_show_mix_waveforms "]", acc_show_mix_waveforms, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->beats_per_measure_symbol, s7_make_function(s7, "[acc-" S_beats_per_measure "]", acc_beats_per_measure, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->transform_normalization_symbol, s7_make_function(s7, "[acc-" S_transform_normalization "]", acc_transform_normalization, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->x_axis_style_symbol, s7_make_function(s7, "[acc-" S_x_axis_style "]", acc_x_axis_style, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->zoom_focus_style_symbol, s7_make_function(s7, "[acc-" S_zoom_focus_style "]", acc_zoom_focus_style, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->graph_style_symbol, s7_make_function(s7, "[acc-" S_graph_style "]", acc_graph_style, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->max_transform_peaks_symbol, s7_make_function(s7, "[acc-" S_max_transform_peaks "]", acc_max_transform_peaks, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->graphs_horizontal_symbol, s7_make_function(s7, "[acc-" S_graphs_horizontal "]", acc_graphs_horizontal, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->cursor_size_symbol, s7_make_function(s7, "[acc-" S_cursor_size "]", acc_cursor_size, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->cursor_style_symbol, s7_make_function(s7, "[acc-" S_cursor_style "]", acc_cursor_style, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->tracking_cursor_style_symbol, s7_make_function(s7, "[acc-" S_tracking_cursor_style "]", acc_tracking_cursor_style, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_sonogram_cursor_symbol, s7_make_function(s7, "[acc-" S_show_sonogram_cursor "]", acc_show_sonogram_cursor, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->min_db_symbol, s7_make_function(s7, "[acc-" S_min_dB "]", acc_min_dB, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_gl_symbol, s7_make_function(s7, "[acc-" S_with_gl "]", acc_with_gl, 2, 0, false, "accessor"));

  s7_eval_c_string(s7, "(set! *transform-type* fourier-transform)");
#endif
}
