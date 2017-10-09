#include "snd.h"


chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound)
{
  chan_info *cp; /* may be re-used */
  if (!cip)
    {
      cp = (chan_info *)calloc(1, sizeof(chan_info)); 
      cp->ax = (graphics_context *)calloc(1, sizeof(graphics_context));

      switch (chan % 4)
	{
	case 0: cp->combined_data_color = ss->black;      break;
	case 1: cp->combined_data_color = ss->red;        break;
	case 2: cp->combined_data_color = ss->green;      break;
	case 3: cp->combined_data_color = ss->blue;       break;
	}
#if USE_GTK
      cp->progress_pct = -1.0;
#endif
      cp->last_sonogram = NULL;
      cp->last_wavogram = NULL;
      cp->temp_sonogram = NULL;
      cp->inset_graph = NULL;
#if HAVE_GL
      cp->gl_fft_list = NO_LIST;
      cp->gl_wavo_list = NO_LIST;
#endif
      cp->edit_hook = Xen_false;
      cp->edit_hook_loc = NOT_A_GC_LOC;
      cp->after_edit_hook = Xen_false;
      cp->after_edit_hook_loc = NOT_A_GC_LOC;
      cp->undo_hook = Xen_false;
      cp->undo_hook_loc = NOT_A_GC_LOC;
      cp->properties = Xen_false; /* will be a vector of 1 element if it's ever used */
      cp->properties_loc = NOT_A_GC_LOC;
      cp->active = CHANNEL_INACTIVE;
    }
  else cp = cip;
  cp->chan = chan;
  cp->sound = sound;
  cp->sound_ctr = NOT_A_SOUND;
  cp->edit_ctr = -1;
  cp->sound_size = 0;
  cp->edit_size = 0;
  cp->cursor_on = false;
  cp->cursor_visible = false;
  cp->fft_cursor_visible = false;
  cp->show_sonogram_cursor = show_sonogram_cursor(ss);
  cp->selection_visible = false;
  cp->editable = true; /* must be unset (region display, variable display, etc) */
  cp->cursor_style = cursor_style(ss);
  cp->tracking_cursor_style = tracking_cursor_style(ss);
  cp->cursor_size = cursor_size(ss);
  cp->cursor_proc = Xen_undefined;
  cp->cursor_proc_loc = NOT_A_GC_LOC;
  cp->squelch_update = false;
  cp->show_y_zero = show_y_zero(ss);
  cp->show_grid = show_grid(ss);
  cp->show_marks = show_marks(ss);
  cp->time_graph_type = time_graph_type(ss);
  cp->wavo_hop = wavo_hop(ss);
  cp->wavo_trace = wavo_trace(ss);
  cp->max_transform_peaks = max_transform_peaks(ss);
  cp->show_transform_peaks = show_transform_peaks(ss);
  cp->zero_pad = zero_pad(ss);
  cp->with_verbose_cursor = with_verbose_cursor(ss);
  cp->fft_log_frequency = fft_log_frequency(ss);
  cp->fft_log_magnitude = fft_log_magnitude(ss);
  cp->fft_with_phases = fft_with_phases(ss);
  cp->min_dB = min_dB(ss);
  cp->lin_dB = ss->lin_dB;
  cp->in_as_one_edit = 0;
  cp->wavelet_type = wavelet_type(ss);
  cp->spectro_x_angle = spectro_x_angle(ss);
  cp->spectro_y_angle = spectro_y_angle(ss);
  cp->spectro_z_angle = spectro_z_angle(ss);
  cp->spectro_x_scale = spectro_x_scale(ss);
  cp->spectro_y_scale = spectro_y_scale(ss);
  cp->spectro_z_scale = spectro_z_scale(ss);
  cp->spectrum_end = spectrum_end(ss);
  cp->spectrum_start = spectrum_start(ss);
  cp->spectro_hop = spectro_hop(ss);
  cp->fft_window_alpha = fft_window_alpha(ss);
  cp->fft_window_beta = fft_window_beta(ss);
  cp->transform_size = transform_size(ss);
  cp->transform_graph_type = transform_graph_type(ss);
  cp->fft_window = fft_window(ss);
  cp->transform_type = transform_type(ss);
  cp->transform_normalization = transform_normalization(ss);
  cp->show_mix_waveforms = show_mix_waveforms(ss);
  cp->time_graph_style = graph_style(ss);
  cp->lisp_graph_style = graph_style(ss);
  cp->transform_graph_style = graph_style(ss);
  cp->graphs_horizontal = graphs_horizontal(ss);
  cp->dot_size = dot_size(ss);
  cp->grid_density = grid_density(ss);
  cp->x_axis_style = x_axis_style(ss);
  cp->beats_per_minute = beats_per_minute(ss);
  cp->beats_per_measure = beats_per_measure(ss);
  cp->show_axes = show_axes(ss);
  cp->graph_time_on = true; /* the default state (button is set when we start) */
  cp->graph_transform_on = false;
  cp->printing = NOT_PRINTING;
  cp->waiting_to_make_graph = false;
  cp->new_peaks = false;
  cp->sonogram_data = NULL;
  cp->lisp_info = NULL;
  cp->amp_control = NULL;
  cp->hookable = WITH_HOOK;
  cp->cx = 0;
  cp->cy = 0;
  cp->fft_cx = 0;
  cp->selection_transform_size = 0;
  if (cp->last_sonogram) 
    {
      free(cp->last_sonogram); 
      cp->last_sonogram = NULL;
    }
  if (cp->last_wavogram) 
    {
      free(cp->last_wavogram); 
      cp->last_wavogram = NULL;
    }
  if (cp->inset_graph)
    clear_inset_graph(cp);
  cp->active = CHANNEL_INITIALIZED;
  return(cp);
}


static chan_info *free_chan_info(chan_info *cp)
{
  /* this does not free the associated widgets -- they are merely unmanaged */

  cp->active = CHANNEL_INACTIVE;
  /* need an indication right away that this channel is being deleted -- during free_snd_info (close-sound),
   *   an error may occur (an edit list temp file might have vanished for example), and normally Snd
   *   attempts to post an error message in the sound's status area.  To force this out, we have to
   *   call XmUpdate or equivalent, which can cause all the sound's channels to attempt to redisplay;
   *   since the one causing the error is half-deallocated, trouble can ensue.  So both the channel
   *   and the sound have "active" flags that are true only when everything is ship-shape.
   */
  chan_info_cleanup(cp);
  cp->squelch_update = true;
  cp->axis = free_axis_info(cp->axis);
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  cp_free_fft_state(cp);
  cp->graph_transform_on = false;
  cp->printing = NOT_PRINTING;
  cp->graph_time_on = true;
  if (cp->edits) free_edit_list(cp);
  if (cp->sounds) free_sound_list(cp);
  free_channel_mixes(cp);
  cp->sound = NULL;  /* a backpointer */
  cp->cursor_on = false;
  cp->cursor_visible = false;
  cp->fft_cursor_visible = false;
  cp->show_sonogram_cursor = false;
  cp->selection_visible = false;
  if (cp->amp_control)
    {
      /* not sure this is the right thing */
      free(cp->amp_control);
      cp->amp_control = NULL;
    }
  if (Xen_is_procedure(cp->cursor_proc))
    {
      snd_unprotect_at(cp->cursor_proc_loc);
      cp->cursor_proc = Xen_undefined;
      cp->cursor_proc_loc = NOT_A_GC_LOC;
    }
  if (Xen_is_vector(cp->properties)) /* using vector as node for GC */
    Xen_vector_set(cp->properties, 0, Xen_empty_list);
  cp->waiting_to_make_graph = false;
  if (cp->sonogram_data) free_sono_info(cp);
  if (cp->temp_sonogram) 
    {
      /* special case -- background fft process never got a chance to run */
      if (cp->temp_sonogram == cp->last_sonogram) cp->last_sonogram = NULL;
      free_sonogram_fft_state(cp->temp_sonogram);
      free(cp->temp_sonogram); 
      cp->temp_sonogram = NULL;
    } 
  if (cp->last_sonogram) 
    {
      free_sonogram_fft_state(cp->last_sonogram);
      free(cp->last_sonogram); 
      cp->last_sonogram = NULL;
    }
  if (cp->last_wavogram) 
    {
      free(cp->last_wavogram); 
      cp->last_wavogram = NULL;
    }
  if (cp->lisp_info) 
    {
      free_lisp_info(cp);
      cp->lisp_info = NULL;
    }
  cp->graph_lisp_on = false;
  cp->selection_transform_size = 0;

  if (cp->as_one_edit_positions)
    {
      free(cp->as_one_edit_positions);
      cp->as_one_edit_positions = NULL;
      cp->as_one_edit_positions_size = 0;
    }

  if (Xen_is_hook(cp->edit_hook))
    {
      Xen_clear_hook_list(cp->edit_hook);
      snd_unprotect_at(cp->edit_hook_loc);
      cp->edit_hook = Xen_false;
      cp->edit_hook_loc = NOT_A_GC_LOC;
    }
  if (Xen_is_hook(cp->after_edit_hook))
    {
      Xen_clear_hook_list(cp->after_edit_hook);
      snd_unprotect_at(cp->after_edit_hook_loc);
      cp->after_edit_hook = Xen_false;
      cp->after_edit_hook_loc = NOT_A_GC_LOC;
    }
  if (Xen_is_hook(cp->undo_hook))
    {
      Xen_clear_hook_list(cp->undo_hook);
      snd_unprotect_at(cp->undo_hook_loc);
      cp->undo_hook = Xen_false;
      cp->undo_hook_loc = NOT_A_GC_LOC;
    }
  if (cp->inset_graph)
    clear_inset_graph(cp);

  return(cp);  /* pointer is left for possible future re-use */
}


snd_info *make_basic_snd_info(int chans)
{
  snd_info *sp = NULL;
  sp = (snd_info *)calloc(1, sizeof(snd_info));
  sp->chans = (chan_info **)calloc(chans, sizeof(chan_info *));
  sp->allocated_chans = chans;
  sp->properties = Xen_false; /* will be a vector of 1 element if it's ever used */
  sp->properties_loc = NOT_A_GC_LOC;
#if USE_NO_GUI
  sp->snd_widgets = false; /* it's a bool if no gui */
#endif
  return(sp);
}


void initialize_control_panel(snd_info *sp)
{
  sp->expand_control = DEFAULT_EXPAND_CONTROL;
  sp->expand_control_min = expand_control_min(ss);
  sp->expand_control_max = expand_control_max(ss);
  sp->last_expand_control = 0.0;
  sp->saved_expand_control = 0.0;
  sp->expand_control_on = DEFAULT_EXPAND_CONTROL_ON;
  sp->amp_control = DEFAULT_AMP_CONTROL;
  sp->amp_control_min = amp_control_min(ss);
  sp->amp_control_max = amp_control_max(ss);
  sp->last_amp_control = 1.0;
  sp->saved_amp_control = 1.0;
  sp->speed_control = fabs(DEFAULT_SPEED_CONTROL);
  sp->speed_control_min = speed_control_min(ss);
  sp->speed_control_max = speed_control_max(ss);
  sp->last_speed_control = 1.0;
  sp->saved_speed_control = 1.0;
  if (DEFAULT_SPEED_CONTROL > 0.0) sp->speed_control_direction = 1; else sp->speed_control_direction = -1;
  sp->contrast_control_on = DEFAULT_CONTRAST_CONTROL_ON;
  sp->contrast_control = DEFAULT_CONTRAST_CONTROL;
  sp->contrast_control_min = contrast_control_min(ss);
  sp->contrast_control_max = contrast_control_max(ss);
  sp->contrast_control_amp = contrast_control_amp(ss);
  sp->last_contrast_control = contrast_control_min(ss);
  sp->saved_contrast_control = contrast_control_min(ss);
  sp->reverb_control_on = DEFAULT_REVERB_CONTROL_ON;
  sp->filter_control_on = DEFAULT_FILTER_CONTROL_ON;
  sp->expand_control_length = expand_control_length(ss);
  sp->expand_control_ramp = expand_control_ramp(ss);
  sp->expand_control_hop = expand_control_hop(ss);
  sp->expand_control_jitter = expand_control_jitter(ss);
  sp->reverb_control_feedback = reverb_control_feedback(ss);
  sp->reverb_control_lowpass = reverb_control_lowpass(ss);
  sp->reverb_control_scale = DEFAULT_REVERB_CONTROL_SCALE;
  sp->reverb_control_scale_min = reverb_control_scale_min(ss);
  sp->reverb_control_scale_max = reverb_control_scale_max(ss);
  sp->reverb_control_decay = reverb_control_decay(ss);
  sp->speed_control_tones = speed_control_tones(ss);
  sp->speed_control_style = speed_control_style(ss);
  sp->speed_control_numerator = 1;
  sp->speed_control_denominator = 1;
  sp->last_reverb_control_scale = 0.0;
  sp->saved_reverb_control_scale = 0.0;
  sp->reverb_control_length = DEFAULT_REVERB_CONTROL_LENGTH;
  sp->reverb_control_length_min = reverb_control_length_min(ss);
  sp->reverb_control_length_max = reverb_control_length_max(ss);
  sp->last_reverb_control_length = 0.0;
  sp->saved_reverb_control_length = 0.0;
  sp->filter_control_order = filter_control_order(ss);
  sp->filter_control_in_dB = filter_control_in_dB(ss);
  sp->filter_control_in_hz = filter_control_in_hz(ss);
  if (sp->filter_control_in_hz)
    sp->filter_control_xmax = (mus_float_t)(snd_srate(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  sp->filter_control_changed = false;
  sp->saved_controls = NULL;
}


snd_info *make_snd_info(snd_info *sip, const char *filename, file_info *hdr, int snd_slot, read_only_t read_only)
{
  snd_info *sp = NULL;
  int chans, i;
  /* assume file has been found and header read before reaching us */
  /* if a reused pointer, may need to extend current chans array */
  chans = hdr->chans;
  if (!sip)
    {
      sp = make_basic_snd_info(chans);
    }
  else 
    {
      sp = sip;
      if (sp->allocated_chans < chans) 
	{
	  sp->chans = (chan_info **)realloc(sp->chans, chans * sizeof(chan_info *));
	  for (i = sp->allocated_chans; i < chans; i++) sp->chans[i] = NULL;
	  sp->allocated_chans = chans;
	}
    }
  sp->user_read_only = read_only;  /* need to be sure this is set before any hooks run */
  sp->bomb_in_progress = false;
  sp->index = snd_slot;
  sp->nchans = chans;
  sp->hdr = hdr;
  sp->inuse = SOUND_NORMAL;
  sp->filename = mus_strdup(filename);
  sp->short_filename = filename_without_directory(sp->filename); /* a pointer into filename, not a new string */
  sp->sync = DEFAULT_SYNC;
  sp->previous_sync = sp->sync;
  initialize_control_panel(sp);
  sp->selectpos = -1;

  if (chans > 1)
    {
      if (ss->update_sound_channel_style != NOT_A_CHANNEL_STYLE)
	{
	  sp->channel_style = ss->update_sound_channel_style;
	  ss->update_sound_channel_style = NOT_A_CHANNEL_STYLE;
	}
      else sp->channel_style = channel_style(ss);
    }
  else sp->channel_style = CHANNELS_SEPARATE;

  sp->selected_channel = NO_SELECTION;
  sp->playing = 0;
  sp->applying = false;
  sp->lacp = NULL;
  sp->delete_me = NULL;
  sp->name_string = NULL;
  sp->active = true;
  return(sp);
}


void free_snd_info(snd_info *sp)
{
  unsigned int i;

#if (!USE_NO_GUI)
  env_editor *edp;

  /* make sure trough colors are ok upon reuse */
  if (sp->reverb_control_on != DEFAULT_REVERB_CONTROL_ON)
    toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_ON);
  if (sp->expand_control_on != DEFAULT_EXPAND_CONTROL_ON)
    toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_ON);
  if (sp->contrast_control_on != DEFAULT_CONTRAST_CONTROL_ON)
    toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_ON);

  edp = sp->flt;
  if (edp)
    {
      edp->edited = false;
      edp->env_dragged = false;
      edp->env_pos = 0;
      edp->click_to_delete = false;
    }
  set_filter_text(sp, "");
#endif

  /* leave most for reuse as in free_chan_info */
  sp->active = false;
  sp->selectpos = -1;
  sp->sync = 0;
  snd_info_cleanup(sp);
  sp->previous_sync = sp->sync;
  for (i = 0; i < sp->nchans; i++)
    if (sp->chans[i]) 
      sp->chans[i] = free_chan_info(sp->chans[i]);
  sp->inuse = SOUND_IDLE;
  sp->playing = 0;
  sp->bomb_in_progress = false;
  sp->applying = false;
  sp->channel_style = CHANNELS_SEPARATE;
  sp->user_read_only = FILE_READ_WRITE;
  sp->file_read_only = FILE_READ_WRITE;
  sp->need_update = false;
  sp->file_unreadable = false;
  if (Xen_is_vector(sp->properties)) /* using vector as node for GC */
    Xen_vector_set(sp->properties, 0, Xen_empty_list);
  sp->selected_channel = NO_SELECTION;
  sp->short_filename = NULL;                      /* was a pointer into filename */
  if (sp->filename) free(sp->filename);
  sp->filename = NULL;
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  if (sp->saved_controls) free_controls(sp);
  sp->delete_me = NULL;
  if (sp->name_string) free(sp->name_string);
  sp->name_string = NULL;
  sp->lacp = NULL;
  sp->hdr = free_file_info(sp->hdr);
  if (sp->edited_region) clear_region_backpointer(sp);
  clear_players();
  reflect_mix_change(ANY_MIX_ID);

  if (Xen_hook_has_list(ss->effects_hook))
    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);
}


snd_info *completely_free_snd_info(snd_info *sp)
{
  int i;

  free_snd_info(sp);
  for (i = 0; i < sp->allocated_chans; i++) 
    {
      chan_info *cp;
      cp = sp->chans[i];
      if (cp)
	{
	  if (Xen_is_vector(cp->properties))
	    {
	      snd_unprotect_at(cp->properties_loc);
	      cp->properties_loc = NOT_A_GC_LOC;
	    }
	  if (cp->inset_graph)
	    free_inset_graph(cp);

	  cp->active = CHANNEL_FREED;
	  free(cp);
	  /* it's possible to have dangling readers (snd_fd) with pointers to this channel (see note in snd-edits.c under unlist_reader) */
	}
    }
  free(sp->chans);
  if (Xen_is_vector(sp->properties))
    {
      snd_unprotect_at(sp->properties_loc);
      sp->properties_loc = NOT_A_GC_LOC;
    }
  free(sp);
  return(NULL);
}


void for_each_chan_with_int(void (*func)(chan_info *ncp, int val), int value)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, value);
    }
}


void for_each_chan_with_mus_long_t(void (*func)(chan_info *ncp, mus_long_t val), mus_long_t value)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, value);
    }
}


void for_each_chan_with_bool(void (*func)(chan_info *ncp, bool val), bool value)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, value);
    }
}


void for_each_chan_with_float(void (*func)(chan_info *ncp, mus_float_t val), mus_float_t value)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, value);
    }
}


void for_each_chan(void (*func)(chan_info *ncp))
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp);
    }
}


void for_each_normal_chan_with_void(void (*func)(chan_info *ncp, void *ptr), void *userptr)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, userptr);
    }
}


void for_each_normal_chan_with_int(void (*func)(chan_info *ncp, int val), int value)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, value);
    }
}


void for_each_normal_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, value);
    }
}


void for_each_normal_chan(void (*func)(chan_info *ncp))
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      unsigned int j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp);
    }
}


void for_each_sound_chan_with_int(snd_info *sp, void (*func)(chan_info *ncp, int val1), int value)
{
  unsigned int j;
  chan_info *cp;
  for (j = 0; j < sp->nchans; j++)
    if ((cp = sp->chans[j]))
      (*func)(cp, value);
}


bool map_over_sound_chans(snd_info *sp, bool (*func)(chan_info *ncp))
{
  unsigned int j;
  bool val = false;
  chan_info *cp;
  for (j = 0; j < sp->nchans; j++)
    if ((cp = sp->chans[j]))
      {
	val = (*func)(cp);
	if (val) return(val);
      }
  return(val);
}


void for_each_sound_chan(snd_info *sp, void (*func)(chan_info *ncp))
{
  unsigned int j;
  chan_info *cp;
  for (j = 0; j < sp->nchans; j++)
    if ((cp = sp->chans[j]))
      (*func)(cp);
}


void for_each_sound(void (*func)(snd_info *usp))
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	(*func)(sp);
    }
}


void for_each_sound_with_void(void (*func)(snd_info *usp, void *ptr), void *userptr)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	(*func)(sp, userptr);
    }
}


void for_each_separate_chan(void (*func)(chan_info *ncp))
{
  /* used only to lock/unlock panes */
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	{
	  if (sp->channel_style != CHANNELS_SEPARATE)
	    (*func)(sp->chans[0]);
	  else for_each_sound_chan(sp, func);
	}
    }
}


bool snd_ok(snd_info *sp) 
{
  return((sp) && 
	 (sp->inuse != SOUND_IDLE) && 
	 (sp->active));
}


int active_channels(virtual_channels_t count_virtual_channels)
{
  int chans = 0, i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if (snd_ok(sp) && 
	  (sp->inuse != SOUND_WRAPPER))
	{
	  if ((count_virtual_channels == WITH_VIRTUAL_CHANNELS) ||
	      (sp->channel_style == CHANNELS_SEPARATE))
	    chans += sp->nchans;
	  else chans++;
	}
    }
  return(chans);
}


#define SOUNDS_ALLOC_SIZE 4

int find_free_sound_slot(int desired_chans)
{
  int i, j;
  snd_info *sp;
  /* first try to find an unused slot that can accommodate desired_chans (to reduce Widget creation) */
  if (ss->reloading_updated_file > 0)
    {
      /* snd_update should change the underlying slot only when it has to (user increased chans) */
      sp = ss->sounds[ss->reloading_updated_file - 1];
      if ((!sp) ||
	  ((sp->inuse == SOUND_IDLE) && (sp->allocated_chans >= desired_chans)))
	return(ss->reloading_updated_file - 1);
    }
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_IDLE) && (sp->allocated_chans == desired_chans)) return(i);
    }
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_IDLE) && (sp->allocated_chans > desired_chans)) return(i);
    }
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if (!sp) return(i);
      if (sp->inuse == SOUND_IDLE) return(i);
    }
  j = ss->max_sounds;
  ss->max_sounds += SOUNDS_ALLOC_SIZE;
  ss->sounds = (snd_info **)realloc(ss->sounds, ss->max_sounds * sizeof(snd_info *));
  for (i = j; i < ss->max_sounds; i++) ss->sounds[i] = NULL;
  return(j);
}


int find_free_sound_slot_for_channel_display(void)
{
  int i, j;
  for (i = 0; i < ss->max_sounds; i++)
    if (!ss->sounds[i]) return(i);
  j = ss->max_sounds;
  ss->max_sounds += SOUNDS_ALLOC_SIZE;
  ss->sounds = (snd_info **)realloc(ss->sounds, ss->max_sounds * sizeof(snd_info *));
  for (i = j; i < ss->max_sounds; i++) ss->sounds[i] = NULL;
  return(j);
}


static snd_info *any_active_sound(void)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	return(sp);
    }
  return(NULL);
}


snd_info *selected_sound(void)
{
  if ((ss->selected_sound != NO_SELECTION) &&
      (snd_ok(ss->sounds[ss->selected_sound])))
    return(ss->sounds[ss->selected_sound]);
  ss->selected_sound = NO_SELECTION;
  return(NULL);
}


snd_info *any_selected_sound(void)
{
  snd_info *sp;
  sp = selected_sound();
  if (!sp) sp = any_active_sound();
  return(sp);
}


chan_info *any_selected_channel(snd_info *sp)
{
  int chan = 0;
  if (sp->selected_channel != NO_SELECTION) 
    chan = sp->selected_channel;
  if ((sp->chans[chan]) && 
      (sp->chans[chan]->active > CHANNEL_INITIALIZED))
    return(sp->chans[chan]);
  return(NULL);
}


chan_info *selected_channel(void)
{
  if (ss->selected_sound != NO_SELECTION)
    {
      snd_info *sp;
      sp = ss->sounds[ss->selected_sound];
      if ((sp->inuse == SOUND_NORMAL) && (sp->selected_channel != NO_SELECTION))
	return(sp->chans[sp->selected_channel]);
    }
  return(NULL);
}


static Xen select_sound_hook;
static Xen select_channel_hook;
static int current_selectpos = 0;

static void select_sound(snd_info *sp)
{
  if ((!sp) || (sp->inuse != SOUND_NORMAL)) return;

  if (Xen_hook_has_list(select_sound_hook))
    run_hook(select_sound_hook,
	     Xen_list_1(C_int_to_Xen_sound(sp->index)),
	     S_select_sound_hook);

  if (ss->selected_sound != sp->index)
    {
      reflect_sound_selection(sp);
      ss->selected_sound = sp->index;
      new_active_channel_alert();
      reflect_save_as_sound_selection(sp->short_filename);
    }
  sp->selectpos = current_selectpos++;

  if (Xen_hook_has_list(ss->effects_hook))
    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);
}


chan_info *color_selected_channel(snd_info *sp)
{
  chan_info *ncp;
  ncp = sp->chans[sp->selected_channel];
  recolor_graph(ncp, true);
  ncp->selected = true;
  update_graph(ncp); /* update any indication of change in selected channel (channel id for example) */
  return(ncp);
}


void select_channel(snd_info *sp, int chan)
{
  chan_info *cp;

  if ((!sp) || (sp->inuse != SOUND_NORMAL)) return;
  cp = selected_channel();
  if (cp != sp->chans[chan])
    {
      chan_info *ncp;
      sp->selected_channel = chan;
      select_sound(sp);
      if (cp) 
	{
	  recolor_graph(cp, false);
	  cp->selected = false;
	  if (sp != cp->sound) cp->sound->selected_channel = NO_SELECTION;
	  update_graph(cp);
	}
      if (Xen_hook_has_list(select_channel_hook))
	run_hook(select_channel_hook,
		 Xen_list_2(C_int_to_Xen_sound(sp->index),
			    C_int_to_Xen_integer(chan)),
		 S_select_channel_hook);
      ncp = color_selected_channel(sp);
      goto_graph(ncp);
    }
}


chan_info *current_channel(void)
{
  snd_info *sp = NULL;
  if (ss->selected_sound != NO_SELECTION)
    sp = ss->sounds[ss->selected_sound];
  else sp = any_active_sound();
  if (sp) return(any_selected_channel(sp));
  return(NULL);
}


sync_info *free_sync_info(sync_info *si)
{
  if (si)
    {
      if (si->begs) free(si->begs);
      si->begs = NULL;
      if (si->cps) free(si->cps);
      si->cps = NULL;
      free(si);
    }
  return(NULL);
}


int syncd_channels(int sync)
{
  if (sync != 0)
    {
      int i, chans = 0;
      snd_info *sp;
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL) && 
	      (sp->sync == sync)) 
	    chans += sp->nchans;
	}
      return(chans);
    }
  return(0);
}


sync_info *snd_sync(int sync)
{
  int chans;
  snd_info *sp;
  chans = syncd_channels(sync);
  if (chans > 0)
    {
      int i, j;
      sync_info *si;
      si = (sync_info *)calloc(1, sizeof(sync_info));
      si->begs = (mus_long_t *)calloc(chans, sizeof(mus_long_t));
      si->cps = (chan_info **)calloc(chans, sizeof(chan_info *));
      si->chans = chans;
      for (i = 0, j = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL) && 
	      (sp->sync == sync))
	    {
	      unsigned int k;
	      for (k = 0; k < sp->nchans; k++, j++)
		si->cps[j] = sp->chans[k];
	    }
	}
      return(si);
    }
  return(NULL);
}


sync_info *make_simple_sync(chan_info *cp, mus_long_t beg)
{
  sync_info *si;
  si = (sync_info *)calloc(1, sizeof(sync_info));
  si->chans = 1;
  si->cps = (chan_info **)calloc(1, sizeof(chan_info *));
  si->cps[0] = cp;
  si->begs = (mus_long_t *)calloc(1, sizeof(mus_long_t));
  si->begs[0] = beg;
  return(si);
}


sync_info *sync_to_chan(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if (sp->sync != 0)
    return(snd_sync(sp->sync));
  return(make_simple_sync(cp, 0));
}


snd_info *find_sound(const char *name, int nth)
{
  char *sname;
  int i, which = 0;
  if (!name) return(NULL);
  sname = filename_without_directory(name);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	if ((mus_strcmp(name, sp->short_filename)) || 
	    (mus_strcmp(name, sp->filename)) ||
	    (mus_strcmp(sname, sp->short_filename)))
	  {
	    if (which == nth) return(sp);
	    which++;
	  }
    }
  return(NULL);
}


void g_init_data(void)
{
  #define H_select_sound_hook S_select_sound_hook " (snd): called whenever a sound is selected."

  #define H_select_channel_hook S_select_channel_hook " (snd chn): called whenever a channel is selected. \
Its arguments are the sound index and the channel number."

  select_sound_hook =   Xen_define_hook(S_select_sound_hook,   "(make-hook 'snd)",      1, H_select_sound_hook);
  select_channel_hook = Xen_define_hook(S_select_channel_hook, "(make-hook 'snd 'chn)", 2, H_select_channel_hook);
}

