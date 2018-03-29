#include "snd.h"
#include "clm-strings.h"
#include "sndlib-strings.h"
#include "clm2xen.h"

 
#if HAVE_RUBY
  #define TO_GVAR_NAME(Str) xen_scheme_global_variable_to_ruby(Str)
#else
  #define TO_GVAR_NAME(Str) Str
#endif


static void remove_temp_files(chan_info *cp)
{
  free_sound_list(cp);
  delete_any_remaining_mix_temp_files_at_exit(cp);
}


static void save_peak_env_info(chan_info *cp)
{
  write_peak_env_info_file(cp); /* this returns bool, but for_each_chan wants void */
}


static Xen exit_hook;
static Xen before_exit_hook;

bool snd_exit_cleanly(bool force_exit)
{  
  Xen res = Xen_false;
  ss->exiting = true; /* if segfault during exit code, don't try to restart at event loop! */

  /* before-exit-hook can cancel the exit, whereas exit-hook can't */
  if (Xen_hook_has_list(before_exit_hook))
    res = run_or_hook(before_exit_hook, 
		      Xen_empty_list,
		      S_before_exit_hook);
  if ((Xen_is_true(res)) && (!force_exit)) return(false); /* does it make any sense to call this hook if we're forced to exit anyway? */

#if (!USE_NO_GUI)
  if (ask_about_unsaved_edits(ss))
    {
      int i;
      bool found_saver = false;
      for (i = 0; i < ss->max_sounds; i++)
	{
	  snd_info *sp;
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL) &&
	      (has_unsaved_edits(sp)))
	    {
	      found_saver = true;
	      save_edits_now(sp);
	    }
	}
      if (found_saver) return(false);
    }
#endif

  if (peak_env_dir(ss))
    for_each_chan(save_peak_env_info);

  if (Xen_hook_has_list(exit_hook))
    run_hook(exit_hook, 
	     Xen_empty_list,
	     S_exit_hook);

  if (ss->file_monitor_ok)
    cleanup_file_monitor();

  cleanup_dac();
  for_each_normal_chan(remove_temp_files);
  cleanup_region_temp_files();
  forget_temps();
  return(true);
}


void sound_not_current(snd_info *sp)
{
  /* check for change in update status */
  bool needs_update;
  if (ss->file_monitor_ok) return;
  needs_update = (file_write_date(sp->filename) != sp->write_date);
  if (needs_update != sp->need_update)
    {
      sp->need_update = needs_update;
      if (needs_update)
	{
	  if (auto_update(ss))
	    snd_update(sp); /* will stop bomb via need_update flag */
	  else start_bomb(sp);
	}
      else stop_bomb(sp);
    }
}




/* ---------------- save sound state (options, or entire state) ---------------- */

static bool fneq(mus_float_t a, mus_float_t b)
{
  /* floating point != replacement */
  return(fabs(a - b) > .00001);
}


static const char *cursor_style_name(cursor_style_t style)
{
  switch (style)
    {
    case CURSOR_CROSS: return(TO_VAR_NAME(S_cursor_cross)); 
    case CURSOR_LINE:  return(TO_VAR_NAME(S_cursor_line));  
    default: /* proc?? */ return(TO_VAR_NAME(S_cursor_cross));
    }
}


static const char *show_axes2string(show_axes_t ax)
{
  switch (ax)
    {
    case SHOW_NO_AXES:           return(TO_VAR_NAME(S_show_no_axes));             
    case SHOW_X_AXIS:            return(TO_VAR_NAME(S_show_x_axis));              
    case SHOW_X_AXIS_UNLABELLED: return(TO_VAR_NAME(S_show_x_axis_unlabelled));   
    case SHOW_ALL_AXES:          return(TO_VAR_NAME(S_show_all_axes));            
    case SHOW_BARE_X_AXIS:       return(TO_VAR_NAME(S_show_bare_x_axis));         
    default:                     return(TO_VAR_NAME(S_show_all_axes_unlabelled)); 
    }
}


static const char *zoom_focus_style_name(zoom_focus_t choice)
{
  switch (choice)
    {
    case ZOOM_FOCUS_LEFT:   return(TO_VAR_NAME(S_zoom_focus_left));   
    case ZOOM_FOCUS_RIGHT:  return(TO_VAR_NAME(S_zoom_focus_right));  
    case ZOOM_FOCUS_MIDDLE: return(TO_VAR_NAME(S_zoom_focus_middle)); 
      /* proc?? */
    default:                return(TO_VAR_NAME(S_zoom_focus_active)); 
    }
}


static const char *transform_normalization_name(fft_normalize_t choice)
{
  switch (choice)
    {
    case DONT_NORMALIZE:      return(TO_VAR_NAME(S_dont_normalize));       
    case NORMALIZE_BY_CHANNEL:return(TO_VAR_NAME(S_normalize_by_channel)); 
    case NORMALIZE_BY_SOUND:  return(TO_VAR_NAME(S_normalize_by_sound));   
    case NORMALIZE_GLOBALLY:  return(TO_VAR_NAME(S_normalize_globally));   
    default:                  return(TO_VAR_NAME(S_normalize_by_channel)); 
    }
}


static const char *graph_style_name(graph_style_t choice)
{
  switch (choice)
    {
    case GRAPH_DOTS:           return(TO_VAR_NAME(S_graph_dots));           
    case GRAPH_DOTS_AND_LINES: return(TO_VAR_NAME(S_graph_dots_and_lines)); 
    case GRAPH_LOLLIPOPS:      return(TO_VAR_NAME(S_graph_lollipops));      
    case GRAPH_FILLED:         return(TO_VAR_NAME(S_graph_filled));         
    case GRAPH_LINES: 
    default:                   return(TO_VAR_NAME(S_graph_lines));          
    }
}


static const char *transform_graph_type_name(graph_type_t choice)
{
  switch (choice)
    {
    case GRAPH_AS_SONOGRAM:    return(TO_VAR_NAME(S_graph_as_sonogram));    
    case GRAPH_AS_SPECTROGRAM: return(TO_VAR_NAME(S_graph_as_spectrogram)); 
    default:                   return(TO_VAR_NAME(S_graph_once));           
    }
}


static const char *time_graph_type_name(graph_type_t choice)
{
  switch (choice)
    {
    case GRAPH_AS_WAVOGRAM: return(TO_VAR_NAME(S_graph_as_wavogram)); 
    default:                return(TO_VAR_NAME(S_graph_once));        
    }
}


static const char *x_axis_style_name(x_axis_style_t choice)
{
  switch (choice)
    {
    case X_AXIS_AS_CLOCK:      return(TO_VAR_NAME(S_x_axis_as_clock));      
    case X_AXIS_IN_SAMPLES:    return(TO_VAR_NAME(S_x_axis_in_samples));    
    case X_AXIS_AS_PERCENTAGE: return(TO_VAR_NAME(S_x_axis_as_percentage)); 
    case X_AXIS_IN_BEATS:      return(TO_VAR_NAME(S_x_axis_in_beats));      
    case X_AXIS_IN_MEASURES:   return(TO_VAR_NAME(S_x_axis_in_measures));   
    default:                   return(TO_VAR_NAME(S_x_axis_in_seconds));    
    }
}


static const char *speed_control_style_name(speed_style_t choice)
{
  switch (choice)
    {
    case SPEED_CONTROL_AS_RATIO:    return(TO_VAR_NAME(S_speed_control_as_ratio));    
    case SPEED_CONTROL_AS_SEMITONE: return(TO_VAR_NAME(S_speed_control_as_semitone)); 
    default:                        return(TO_VAR_NAME(S_speed_control_as_float));    
    }
}


static const char *channel_style_name(channel_style_t choice)
{
  switch (choice)
    {
    case CHANNELS_COMBINED:     return(TO_VAR_NAME(S_channels_combined));     
    case CHANNELS_SUPERIMPOSED: return(TO_VAR_NAME(S_channels_superimposed)); 
    default:                    return(TO_VAR_NAME(S_channels_separate));     
    }
}


static const char *sync_style_name(sync_style_t choice)
{
  switch (choice)
    {
    case SYNC_NONE: return(TO_VAR_NAME(S_sync_none));     
    case SYNC_ALL:  return(TO_VAR_NAME(S_sync_all)); 
    default:        return(TO_VAR_NAME(S_sync_by_sound)); 
    }
}


static const char *enved_target_name(enved_target_t choice)
{
  switch (choice)
    {
    case ENVED_SPECTRUM: return(TO_VAR_NAME(S_enved_spectrum));  
    case ENVED_SRATE:    return(TO_VAR_NAME(S_enved_srate));     
    default:             return(TO_VAR_NAME(S_enved_amplitude)); 
    }
}


static const char *b2s(bool val) {return((val) ? (char *)PROC_TRUE : (char *)PROC_FALSE);} /* cast needed by g++ > 3.4 */

#if (!USE_NO_GUI)
static char *colvarname = NULL;
static char *colormap_variable_name(int col)
{
  if (colvarname) free(colvarname);
  colvarname = (char *)calloc(64, sizeof(char));
  snprintf(colvarname, 64, "%s-colormap", colormap_name(col));
  return(colvarname);
}
#endif


#define white_space "      "
static bool b_ok = false;


#if HAVE_RUBY
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "set_%s(%s)\n", to_proc_name(name), val);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "set_%s(\"%s\")\n", to_proc_name(name), val);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "set_%s(%d)\n", to_proc_name(name), val);}
static void pss_sod(FILE *fd, const char *name, mus_long_t val)   {fprintf(fd, "set_%s(%" PRId64 ")\n", to_proc_name(name), val);}
static void pss_sf(FILE *fd, const char *name, mus_float_t val) {fprintf(fd, "set_%s(%.4f)\n", to_proc_name(name), val);}

static void pss_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2) 
  {fprintf(fd, "set_%s([%f, %f])\n", to_proc_name(name), val1, val2);}

static void psp_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%sset_%s(%s, sfile)\n", white_space, to_proc_name(name), val);}
static void psp_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%sset_%s(%d, sfile)\n", white_space, to_proc_name(name), val);}
static void psp_sf(FILE *fd, const char *name, mus_float_t val) {fprintf(fd, "%sset_%s(%.4f, sfile)\n", white_space, to_proc_name(name), val);}

static void psp_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2) 
  {fprintf(fd, "%sset_%s([%f, %f], sfile)\n", white_space, to_proc_name(name), val1, val2);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) 
  {fprintf(fd, "%sset_%s(%s, sfile, %d)\n", white_space, to_proc_name(name), val, chan);}

static void pcp_sss(FILE *fd, const char *name, const char *val, int chan, const char *grf) 
  {fprintf(fd, "%sset_%s(\"%s\", sfile, %d, %s)\n", white_space, to_proc_name(name), val, chan, TO_VAR_NAME(grf));}

static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {fprintf(fd, "%sset_%s(%d, sfile, %d)\n", white_space, to_proc_name(name), val, chan);}

static void pcp_sod(FILE *fd, const char *name, mus_long_t val, int chan)   
  {fprintf(fd, "%sset_%s(%" PRId64 ", sfile, %d)\n", white_space, to_proc_name(name), val, chan);}

static void pcp_sf(FILE *fd, const char *name, mus_float_t val, int chan) 
  {fprintf(fd, "%sset_%s(%.4f, sfile, %d)\n", white_space, to_proc_name(name), val, chan);}

static void pcp_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2, int chan) 
  {fprintf(fd, "%sset_%s([%f, %f], sfile, %d)\n", white_space, to_proc_name(name), val1, val2, chan);}
#endif


#if HAVE_FORTH
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%s set-%s drop\n", val, name);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "\"%s\" set-%s drop\n", val, name);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%d set-%s drop\n", val, name);}
static void pss_sod(FILE *fd, const char *name, mus_long_t val)   {fprintf(fd, "%" PRId64 " set-%s drop\n", val, name);}
static void pss_sf(FILE *fd, const char *name, mus_float_t val) {fprintf(fd, "%.4f set-%s drop\n", val, name);}
static void pss_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2) 
{fprintf(fd, "%s'( %f %f ) set-%s drop\n", white_space, val1, val2, name);}

static void psp_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%s%s sfile set-%s drop\n", white_space, val, name);}
static void psp_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%s%d sfile set-%s drop\n", white_space, val, name);}
static void psp_sf(FILE *fd, const char *name, mus_float_t val) {fprintf(fd, "%s%.4f sfile set-%s drop\n", white_space, val, name);}

static void psp_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2) 
  {fprintf(fd, "%s'( %f %f ) sfile set-%s drop\n", white_space, val1, val2, name);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) 
  {fprintf(fd, "%s%s sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sss(FILE *fd, const char *name, const char *val, int chan, const char *grf) 
  {fprintf(fd, "%s\"%s\" sfile %d %s set-%s drop\n", white_space, val, chan, grf, name);}

static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {fprintf(fd, "%s%d sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sod(FILE *fd, const char *name, mus_long_t val, int chan)   
  {fprintf(fd, "%s%" PRId64 " sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sf(FILE *fd, const char *name, mus_float_t val, int chan) 
  {fprintf(fd, "%s%.4f sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2, int chan) 
  {fprintf(fd, "%s'( %f %f ) sfile %d set-%s drop\n", white_space, val1, val2, chan, name);}
#endif


#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "(set! (%s) %s)\n", name, val);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "(set! (%s) \"%s\")\n", name, val);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "(set! (%s) %d)\n", name, val);}
static void pss_sod(FILE *fd, const char *name, mus_long_t val)   {fprintf(fd, "(set! (%s) %" PRId64 ")\n", name, val);}
static void pss_sf(FILE *fd, const char *name, mus_float_t val) {fprintf(fd, "(set! (%s) %.4f)\n", name, val);}
static void pss_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2) {fprintf(fd, "(set! (%s) (list %f %f))\n", name, val1, val2);}

static void psp_ss(FILE *fd, const char *name, const char *val) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) %s)\n", white_space, name, val);}

static void psp_sd(FILE *fd, const char *name, int val)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) %d)\n", white_space, name, val);}

static void psp_sf(FILE *fd, const char *name, mus_float_t val) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) %.4f)\n", white_space, name, val);}

static void psp_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) (list %f %f))\n", white_space, name, val1, val2);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %s)\n", white_space, name, chan, val);}

static void pcp_sss(FILE *fd, const char *name, const char *val, int chan, const char *grf) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d %s) \"%s\")\n", white_space, name, chan, grf, val);}

static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %d)\n", white_space, name, chan, val);}

static void pcp_sod(FILE *fd, const char *name, mus_long_t val, int chan)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %" PRId64 ")\n", white_space, name, chan, val);}

static void pcp_sf(FILE *fd, const char *name, mus_float_t val, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %.4f)\n", white_space, name, chan, val);}

static void pcp_sl(FILE *fd, const char *name, mus_float_t val1, mus_float_t val2, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) (list %f %f))\n", white_space, name, chan, val1, val2);}
#endif


static void save_options(FILE *fd)
{
  fprintf(fd, "\n%s Snd %s (%s) options saved %s\n", Xen_comment_mark, SND_VERSION, SND_DATE, snd_local_time());

  if (transform_size(ss) != DEFAULT_TRANSFORM_SIZE) pss_sod(fd, S_transform_size, transform_size(ss));
  if (fft_window(ss) != DEFAULT_FFT_WINDOW) pss_ss(fd, S_fft_window, TO_VAR_NAME(mus_fft_window_xen_name(fft_window(ss))));
  if (transform_graph_type(ss) != DEFAULT_TRANSFORM_GRAPH_TYPE) pss_ss(fd, S_transform_graph_type, transform_graph_type_name(transform_graph_type(ss)));
  if (time_graph_type(ss) != DEFAULT_TIME_GRAPH_TYPE) pss_ss(fd, S_time_graph_type, time_graph_type_name(time_graph_type(ss)));
  if (x_axis_style(ss) != DEFAULT_X_AXIS_STYLE) pss_ss(fd, S_x_axis_style, x_axis_style_name(x_axis_style(ss)));
  if (fneq(beats_per_minute(ss), DEFAULT_BEATS_PER_MINUTE)) pss_sf(fd, S_beats_per_minute, beats_per_minute(ss));
  if (beats_per_measure(ss) != DEFAULT_BEATS_PER_MEASURE) pss_sd(fd, S_beats_per_measure, beats_per_measure(ss));
  if (graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_graph_style, graph_style_name(graph_style(ss)));
  if (region_graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_region_graph_style, graph_style_name(region_graph_style(ss)));
  if (channel_style(ss) != DEFAULT_CHANNEL_STYLE) pss_ss(fd, S_channel_style, channel_style_name(channel_style(ss)));
  if (sync_style(ss) != DEFAULT_SYNC_STYLE) pss_ss(fd, S_sync_style, sync_style_name(sync_style(ss)));
  if (enved_target(ss) != DEFAULT_ENVED_TARGET) pss_ss(fd, S_enved_target, enved_target_name(enved_target(ss)));
  if (transform_type(ss) != DEFAULT_TRANSFORM_TYPE) pss_ss(fd, S_transform_type, TO_VAR_NAME(transform_program_name(transform_type(ss))));
  if (zoom_focus_style(ss) != ZOOM_FOCUS_ACTIVE) pss_ss(fd, S_zoom_focus_style, zoom_focus_style_name(zoom_focus_style(ss)));
  if (transform_normalization(ss) != DEFAULT_TRANSFORM_NORMALIZATION) pss_ss(fd, S_transform_normalization, transform_normalization_name(transform_normalization(ss)));
  if (with_file_monitor(ss) != DEFAULT_WITH_FILE_MONITOR) pss_ss(fd, S_with_file_monitor, b2s(with_file_monitor(ss)));
  if (just_sounds(ss) != DEFAULT_JUST_SOUNDS) pss_ss(fd, S_just_sounds, b2s(just_sounds(ss)));
  if (play_arrow_size(ss) != DEFAULT_PLAY_ARROW_SIZE) pss_sd(fd, S_play_arrow_size, play_arrow_size(ss));
  if (show_selection_transform(ss) != DEFAULT_SHOW_SELECTION_TRANSFORM) pss_ss(fd, S_show_selection_transform, b2s(show_selection_transform(ss)));
  if (with_gl(ss) != DEFAULT_WITH_GL) pss_ss(fd, S_with_gl, b2s(with_gl(ss)));
  if (with_mix_tags(ss) != DEFAULT_WITH_MIX_TAGS) pss_ss(fd, S_with_mix_tags, b2s(with_mix_tags(ss)));
  if (with_relative_panes(ss) != DEFAULT_WITH_RELATIVE_PANES) pss_ss(fd, S_with_relative_panes, b2s(with_relative_panes(ss)));
  if (sinc_width(ss) != DEFAULT_SINC_WIDTH) pss_sd(fd, S_sinc_width, sinc_width(ss));
  if (ss->init_window_width != DEFAULT_INIT_WINDOW_WIDTH) pss_sd(fd, S_window_width, ss->init_window_width);
  if (ss->init_window_height != DEFAULT_INIT_WINDOW_HEIGHT) pss_sd(fd, S_window_height, ss->init_window_height);
  if (ss->init_window_x != DEFAULT_INIT_WINDOW_X) pss_sd(fd, S_window_x, ss->init_window_x);
  if (ss->init_window_y != DEFAULT_INIT_WINDOW_Y) pss_sd(fd, S_window_y, ss->init_window_y);
  if (default_output_chans(ss) != DEFAULT_OUTPUT_CHANS) pss_sd(fd, S_default_output_chans, default_output_chans(ss));
  if (default_output_srate(ss) != DEFAULT_OUTPUT_SRATE) pss_sd(fd, S_default_output_srate, default_output_srate(ss));
  if (default_output_header_type(ss) != DEFAULT_OUTPUT_HEADER_TYPE) 
    pss_ss(fd, S_default_output_header_type, mus_header_type_to_string(default_output_header_type(ss)));
  if (default_output_sample_type(ss) != DEFAULT_OUTPUT_SAMPLE_TYPE) 
    pss_ss(fd, S_default_output_sample_type, mus_sample_type_to_string(default_output_sample_type(ss)));
  if (auto_resize(ss) != DEFAULT_AUTO_RESIZE) pss_ss(fd, S_auto_resize, b2s(auto_resize(ss)));
  if (graphs_horizontal(ss) != DEFAULT_GRAPHS_HORIZONTAL) pss_ss(fd, S_graphs_horizontal, b2s(graphs_horizontal(ss)));
  if (auto_update(ss) != DEFAULT_AUTO_UPDATE) pss_ss(fd, S_auto_update, b2s(auto_update(ss)));
  if (color_inverted(ss) != DEFAULT_COLOR_INVERTED) pss_ss(fd, S_color_inverted, b2s(color_inverted(ss)));
  if (zero_pad(ss) != DEFAULT_ZERO_PAD) pss_sd(fd, S_zero_pad, zero_pad(ss));
  if (ask_before_overwrite(ss) != DEFAULT_ASK_BEFORE_OVERWRITE) pss_ss(fd, S_ask_before_overwrite, b2s(ask_before_overwrite(ss)));
  if (with_toolbar(ss) != DEFAULT_WITH_TOOLBAR) pss_ss(fd, S_with_toolbar, b2s(with_toolbar(ss)));
  if (with_tooltips(ss) != DEFAULT_WITH_TOOLTIPS) pss_ss(fd, S_with_tooltips, b2s(with_tooltips(ss)));
  if (with_menu_icons(ss) != DEFAULT_WITH_MENU_ICONS) pss_ss(fd, S_with_menu_icons, b2s(with_menu_icons(ss)));
  if (remember_sound_state(ss) != DEFAULT_REMEMBER_SOUND_STATE) pss_ss(fd, S_remember_sound_state, b2s(remember_sound_state(ss)));
  if (ask_about_unsaved_edits(ss) != DEFAULT_ASK_ABOUT_UNSAVED_EDITS) pss_ss(fd, S_ask_about_unsaved_edits, b2s(ask_about_unsaved_edits(ss)));
  if (save_as_dialog_src(ss) != DEFAULT_SAVE_AS_DIALOG_SRC) pss_ss(fd, S_save_as_dialog_src, b2s(save_as_dialog_src(ss)));
  if (save_as_dialog_auto_comment(ss) != DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT) pss_ss(fd, S_save_as_dialog_auto_comment, b2s(save_as_dialog_auto_comment(ss)));
  if (show_full_duration(ss) != DEFAULT_SHOW_FULL_DURATION) pss_ss(fd, S_show_full_duration, b2s(show_full_duration(ss)));
  if (show_full_range(ss) != DEFAULT_SHOW_FULL_RANGE) pss_ss(fd, S_show_full_range, b2s(show_full_range(ss)));
  if (fneq(initial_beg(ss), DEFAULT_INITIAL_BEG)) pss_sf(fd, S_initial_beg, initial_beg(ss));
  if (fneq(initial_dur(ss), DEFAULT_INITIAL_DUR)) pss_sf(fd, S_initial_dur, initial_dur(ss));
  if (dac_combines_channels(ss) != DEFAULT_DAC_COMBINES_CHANNELS) pss_ss(fd, S_dac_combines_channels, b2s(dac_combines_channels(ss)));
  if (wavo_hop(ss) != DEFAULT_WAVO_HOP) pss_sd(fd, S_wavo_hop, wavo_hop(ss));
  if (wavo_trace(ss) != DEFAULT_WAVO_TRACE) pss_sd(fd, S_wavo_trace, wavo_trace(ss));
  if (spectro_hop(ss) != DEFAULT_SPECTRO_HOP) pss_sd(fd, S_spectro_hop, spectro_hop(ss));
#if (!USE_NO_GUI)
  if ((color_map(ss) != DEFAULT_COLOR_MAP) && (color_map(ss) <= 15)) pss_ss(fd, S_colormap, TO_GVAR_NAME(colormap_variable_name(color_map(ss))));
  if (color_map_size(ss) != DEFAULT_COLOR_MAP_SIZE) pss_sd(fd, S_colormap_size, color_map_size(ss));
#endif
  if (wavelet_type(ss) != DEFAULT_WAVELET_TYPE) pss_sd(fd, S_wavelet_type, wavelet_type(ss));
  if (cursor_style(ss) != DEFAULT_CURSOR_STYLE) pss_ss(fd, S_cursor_style, cursor_style_name(cursor_style(ss)));
  if (tracking_cursor_style(ss) != DEFAULT_TRACKING_CURSOR_STYLE) pss_ss(fd, S_tracking_cursor_style, cursor_style_name(tracking_cursor_style(ss)));
  if (cursor_size(ss) != DEFAULT_CURSOR_SIZE) pss_sd(fd, S_cursor_size, cursor_size(ss));
  if (dot_size(ss) != DEFAULT_DOT_SIZE) pss_sd(fd, S_dot_size, dot_size(ss));
  if (dac_size(ss) != DEFAULT_DAC_SIZE) pss_sd(fd, S_dac_size, dac_size(ss));
  if (selection_creates_region(ss) != DEFAULT_SELECTION_CREATES_REGION) pss_ss(fd, S_selection_creates_region, b2s(selection_creates_region(ss)));
  if (enved_filter_order(ss) != DEFAULT_ENVED_FILTER_ORDER) pss_sd(fd, S_enved_filter_order, enved_filter_order(ss));
  if (max_transform_peaks(ss) != DEFAULT_MAX_TRANSFORM_PEAKS) pss_sd(fd, S_max_transform_peaks, max_transform_peaks(ss));
  if (max_regions(ss) != DEFAULT_MAX_REGIONS) pss_sd(fd, S_max_regions, max_regions(ss));
  if (fneq(auto_update_interval(ss), DEFAULT_AUTO_UPDATE_INTERVAL)) pss_sf(fd, S_auto_update_interval, auto_update_interval(ss));
  if (fneq(cursor_update_interval(ss), DEFAULT_CURSOR_UPDATE_INTERVAL)) pss_sf(fd, S_cursor_update_interval, cursor_update_interval(ss));
  if (cursor_location_offset(ss) != DEFAULT_CURSOR_LOCATION_OFFSET) pss_sd(fd, S_cursor_location_offset, cursor_location_offset(ss));
  if (with_verbose_cursor(ss) != DEFAULT_WITH_VERBOSE_CURSOR) pss_ss(fd, S_with_verbose_cursor, b2s(with_verbose_cursor(ss)));
  if (with_inset_graph(ss) != DEFAULT_WITH_INSET_GRAPH) pss_ss(fd, S_with_inset_graph, b2s(with_inset_graph(ss)));
  if (with_interrupts(ss) != DEFAULT_WITH_INTERRUPTS) pss_ss(fd, S_with_interrupts, b2s(with_interrupts(ss)));
  if (with_smpte_label(ss) != DEFAULT_WITH_SMPTE_LABEL) pss_ss(fd, S_with_smpte_label, b2s(with_smpte_label(ss)));
  if (with_pointer_focus(ss) != DEFAULT_WITH_POINTER_FOCUS) pss_ss(fd, S_with_pointer_focus, b2s(with_pointer_focus(ss)));
  if (show_indices(ss) != DEFAULT_SHOW_INDICES) pss_ss(fd, S_show_indices, b2s(show_indices(ss)));
  if (show_transform_peaks(ss) != DEFAULT_SHOW_TRANSFORM_PEAKS) pss_ss(fd, S_show_transform_peaks, b2s(show_transform_peaks(ss)));
  if (show_y_zero(ss) != DEFAULT_SHOW_Y_ZERO) pss_ss(fd, S_show_y_zero, b2s(show_y_zero(ss)));
  if (show_grid(ss) != DEFAULT_SHOW_GRID) pss_ss(fd, S_show_grid, b2s((bool)show_grid(ss)));
  if (fneq(grid_density(ss), DEFAULT_GRID_DENSITY)) pss_sf(fd, S_grid_density, grid_density(ss));
  if (show_sonogram_cursor(ss) != DEFAULT_SHOW_SONOGRAM_CURSOR) pss_ss(fd, S_show_sonogram_cursor, b2s(show_sonogram_cursor(ss)));
  if (show_axes(ss) != DEFAULT_SHOW_AXES) pss_ss(fd, S_show_axes, show_axes2string(show_axes(ss)));
  if (show_marks(ss) != DEFAULT_SHOW_MARKS) pss_ss(fd, S_show_marks, b2s(show_marks(ss)));
  if (clipping(ss) != DEFAULT_CLIPPING) pss_ss(fd, S_clipping, b2s(clipping(ss)));
#if USE_MOTIF
  if (view_files_sort(ss) != DEFAULT_VIEW_FILES_SORT) pss_sd(fd, S_view_files_sort, view_files_sort(ss));
#endif
  if (fft_log_magnitude(ss) != DEFAULT_FFT_LOG_MAGNITUDE) pss_ss(fd, S_fft_log_magnitude, b2s(fft_log_magnitude(ss)));
  if (fft_log_frequency(ss) != DEFAULT_FFT_LOG_FREQUENCY) pss_ss(fd, S_fft_log_frequency, b2s(fft_log_frequency(ss)));
  if (fft_with_phases(ss) != DEFAULT_FFT_WITH_PHASES) pss_ss(fd, S_fft_with_phases, b2s(fft_with_phases(ss)));
  if (print_length(ss) != DEFAULT_PRINT_LENGTH) pss_sd(fd, S_print_length, print_length(ss));
  if (show_mix_waveforms(ss) != DEFAULT_SHOW_MIX_WAVEFORMS) pss_ss(fd, S_show_mix_waveforms, b2s(show_mix_waveforms(ss)));
  if (mix_waveform_height(ss) != DEFAULT_MIX_WAVEFORM_HEIGHT) pss_sd(fd, S_mix_waveform_height, mix_waveform_height(ss));
  if (mix_tag_height(ss) != DEFAULT_MIX_TAG_HEIGHT) pss_sd(fd, S_mix_tag_height, mix_tag_height(ss));
  if (mix_tag_width(ss) != DEFAULT_MIX_TAG_WIDTH) pss_sd(fd, S_mix_tag_width, mix_tag_width(ss));
  if (mark_tag_height(ss) != DEFAULT_MARK_TAG_HEIGHT) pss_sd(fd, S_mark_tag_height, mark_tag_height(ss));
  if (mark_tag_width(ss) != DEFAULT_MARK_TAG_WIDTH) pss_sd(fd, S_mark_tag_width, mark_tag_width(ss));
  if (enved_with_wave(ss) != DEFAULT_ENVED_WITH_WAVE) pss_ss(fd, S_enved_with_wave, b2s(enved_with_wave(ss)));
  if (enved_in_dB(ss) != DEFAULT_ENVED_IN_DB) pss_ss(fd, S_enved_in_dB, b2s(enved_in_dB(ss)));
  if (enved_clipping(ss) != DEFAULT_ENVED_CLIPPING) pss_ss(fd, S_enved_clipping, b2s(enved_clipping(ss)));
  if (enved_style(ss) == ENVELOPE_EXPONENTIAL) pss_ss(fd, S_enved_style, TO_VAR_NAME(S_envelope_exponential));

  if ((!tiny_font(ss)) || (!(mus_strcmp(tiny_font(ss), DEFAULT_TINY_FONT)))) pss_sq(fd, S_tiny_font, tiny_font(ss));
  if ((!peaks_font(ss)) || (!(mus_strcmp(peaks_font(ss), DEFAULT_PEAKS_FONT)))) pss_sq(fd, S_peaks_font, peaks_font(ss));
  if ((!bold_peaks_font(ss)) || (!(mus_strcmp(bold_peaks_font(ss), DEFAULT_BOLD_PEAKS_FONT)))) pss_sq(fd, S_bold_peaks_font, bold_peaks_font(ss));
  if ((!axis_label_font(ss)) || (!(mus_strcmp(axis_label_font(ss), DEFAULT_AXIS_LABEL_FONT)))) pss_sq(fd, S_axis_label_font, axis_label_font(ss));
  if ((!axis_numbers_font(ss)) || (!(mus_strcmp(axis_numbers_font(ss), DEFAULT_AXIS_NUMBERS_FONT)))) pss_sq(fd, S_axis_numbers_font, axis_numbers_font(ss));
  if (listener_font(ss))
    pss_sq(fd, S_listener_font, listener_font(ss));
  if (listener_is_visible())
    pss_ss(fd, S_show_listener, b2s(true));

#if USE_MOTIF || USE_GTK
  if (in_graph_cursor(ss) != DEFAULT_GRAPH_CURSOR)
    pss_sd(fd, S_graph_cursor, in_graph_cursor(ss));
#endif

  save_added_sound_file_extensions(fd);
  save_added_source_file_extensions(fd);

  if (save_state_file(ss))
    pss_sq(fd, S_save_state_file, save_state_file(ss));
  if (peak_env_dir(ss)) pss_sq(fd, S_peak_env_dir, peak_env_dir(ss));
  if (temp_dir(ss)) pss_sq(fd, S_temp_dir, temp_dir(ss));
  if (save_dir(ss)) pss_sq(fd, S_save_dir, save_dir(ss));
  if (open_file_dialog_directory(ss)) pss_sq(fd, S_open_file_dialog_directory, open_file_dialog_directory(ss));
  if (ladspa_dir(ss)) pss_sq(fd, S_ladspa_dir, ladspa_dir(ss));
  if ((eps_file(ss)) && (!(mus_strcmp(eps_file(ss), DEFAULT_EPS_FILE)))) pss_sq(fd, S_eps_file, eps_file(ss));
  if ((listener_prompt(ss)) && (!(mus_strcmp(listener_prompt(ss), DEFAULT_LISTENER_PROMPT)))) pss_sq(fd, S_listener_prompt, listener_prompt(ss));
  if ((html_program(ss)) && (!(mus_strcmp(html_program(ss), DEFAULT_HTML_PROGRAM)))) pss_sq(fd, S_html_program, html_program(ss));
  if (html_dir(ss)) pss_sq(fd, S_html_dir, html_dir(ss));

  if (fneq(fft_window_alpha(ss), DEFAULT_FFT_WINDOW_ALPHA)) pss_sf(fd, S_fft_window_alpha, fft_window_alpha(ss));
  if (fneq(fft_window_beta(ss), DEFAULT_FFT_WINDOW_BETA)) pss_sf(fd, S_fft_window_beta, fft_window_beta(ss));
  if (fneq(min_dB(ss), DEFAULT_MIN_DB)) pss_sf(fd, S_min_dB, min_dB(ss));
  if (fneq(log_freq_start(ss), DEFAULT_LOG_FREQ_START)) pss_sf(fd, S_log_freq_start, log_freq_start(ss));
  if (fneq(color_cutoff(ss), DEFAULT_COLOR_CUTOFF)) pss_sf(fd, S_color_cutoff, color_cutoff(ss));
  if (fneq(color_scale(ss), DEFAULT_COLOR_SCALE)) pss_sf(fd, S_color_scale, color_scale(ss));
  if (fneq(spectro_x_scale(ss), DEFAULT_SPECTRO_X_SCALE)) pss_sf(fd, S_spectro_x_scale, spectro_x_scale(ss));
  if (fneq(spectro_y_scale(ss), DEFAULT_SPECTRO_Y_SCALE)) pss_sf(fd, S_spectro_y_scale, spectro_y_scale(ss));
  if (fneq(spectro_z_scale(ss), DEFAULT_SPECTRO_Z_SCALE)) pss_sf(fd, S_spectro_z_scale, spectro_z_scale(ss));
  if (fneq(spectro_z_angle(ss), DEFAULT_SPECTRO_Z_ANGLE)) pss_sf(fd, S_spectro_z_angle, spectro_z_angle(ss));
  if (fneq(spectro_x_angle(ss), DEFAULT_SPECTRO_X_ANGLE)) pss_sf(fd, S_spectro_x_angle, spectro_x_angle(ss));
  if (fneq(spectro_y_angle(ss), DEFAULT_SPECTRO_Y_ANGLE)) pss_sf(fd, S_spectro_y_angle, spectro_y_angle(ss));
  if (fneq(spectrum_end(ss), DEFAULT_SPECTRUM_END)) pss_sf(fd, S_spectrum_end, spectrum_end(ss));
  if (fneq(spectrum_start(ss), DEFAULT_SPECTRUM_START)) pss_sf(fd, S_spectrum_start, spectrum_start(ss));
  if (fneq(enved_base(ss), DEFAULT_ENVED_BASE)) pss_sf(fd, S_enved_base, enved_base(ss));
  if (fneq(enved_power(ss), DEFAULT_ENVED_POWER)) pss_sf(fd, S_enved_power, enved_power(ss));
  if (fneq(eps_bottom_margin(ss), DEFAULT_EPS_BOTTOM_MARGIN)) pss_sf(fd, S_eps_bottom_margin, eps_bottom_margin(ss));
  if (fneq(eps_left_margin(ss), DEFAULT_EPS_LEFT_MARGIN)) pss_sf(fd, S_eps_left_margin, eps_left_margin(ss));
  if (fneq(eps_size(ss), DEFAULT_EPS_SIZE)) pss_sf(fd, S_eps_size, eps_size(ss));

  if ((fneq(contrast_control_min(ss), DEFAULT_CONTRAST_CONTROL_MIN)) ||
      (fneq(contrast_control_max(ss), DEFAULT_CONTRAST_CONTROL_MAX)))
    pss_sl(fd, S_contrast_control_bounds, contrast_control_min(ss), contrast_control_max(ss));
  if (fneq(contrast_control_amp(ss), DEFAULT_CONTRAST_CONTROL_AMP)) pss_sf(fd, S_contrast_control_amp, contrast_control_amp(ss));
  if ((fneq(expand_control_min(ss), DEFAULT_EXPAND_CONTROL_MIN)) ||
      (fneq(expand_control_max(ss), DEFAULT_EXPAND_CONTROL_MAX)))
    pss_sl(fd, S_expand_control_bounds, expand_control_min(ss), expand_control_max(ss));
  if (fneq(expand_control_ramp(ss), DEFAULT_EXPAND_CONTROL_RAMP)) pss_sf(fd, S_expand_control_ramp, expand_control_ramp(ss));
  if (fneq(expand_control_hop(ss), DEFAULT_EXPAND_CONTROL_HOP)) pss_sf(fd, S_expand_control_hop, expand_control_hop(ss));
  if (fneq(expand_control_jitter(ss), DEFAULT_EXPAND_CONTROL_JITTER)) pss_sf(fd, S_expand_control_jitter, expand_control_jitter(ss));
  if (fneq(expand_control_length(ss), DEFAULT_EXPAND_CONTROL_LENGTH)) pss_sf(fd, S_expand_control_length, expand_control_length(ss));
  if (speed_control_tones(ss) != DEFAULT_SPEED_CONTROL_TONES) pss_sd(fd, S_speed_control_tones, speed_control_tones(ss));
  if (speed_control_style(ss) != DEFAULT_SPEED_CONTROL_STYLE) pss_ss(fd, S_speed_control_style, speed_control_style_name(speed_control_style(ss)));
  if ((fneq(speed_control_min(ss), DEFAULT_SPEED_CONTROL_MIN)) ||
      (fneq(speed_control_max(ss), DEFAULT_SPEED_CONTROL_MAX)))
    pss_sl(fd, S_speed_control_bounds, speed_control_min(ss), speed_control_max(ss));
  if ((fneq(reverb_control_scale_min(ss), DEFAULT_REVERB_CONTROL_SCALE_MIN)) ||
      (fneq(reverb_control_scale_max(ss), DEFAULT_REVERB_CONTROL_SCALE_MAX)))
    pss_sl(fd, S_reverb_control_scale_bounds, reverb_control_scale_min(ss), reverb_control_scale_max(ss));
  if ((fneq(reverb_control_length_min(ss), DEFAULT_REVERB_CONTROL_LENGTH_MIN)) ||
      (fneq(reverb_control_length_max(ss), DEFAULT_REVERB_CONTROL_LENGTH_MAX)))
    pss_sl(fd, S_reverb_control_length_bounds, reverb_control_length_min(ss), reverb_control_length_max(ss));
  if (fneq(reverb_control_feedback(ss), DEFAULT_REVERB_CONTROL_FEEDBACK)) pss_sf(fd, S_reverb_control_feedback, reverb_control_feedback(ss));
  if (fneq(reverb_control_lowpass(ss), DEFAULT_REVERB_CONTROL_LOWPASS)) pss_sf(fd, S_reverb_control_lowpass, reverb_control_lowpass(ss));
  if (fneq(reverb_control_decay(ss), DEFAULT_REVERB_CONTROL_DECAY)) pss_sf(fd, S_reverb_control_decay, reverb_control_decay(ss));
  if ((fneq(amp_control_min(ss), DEFAULT_AMP_CONTROL_MIN)) ||
      (fneq(amp_control_max(ss), DEFAULT_AMP_CONTROL_MAX)))
    pss_sl(fd, S_amp_control_bounds, amp_control_min(ss), amp_control_max(ss));
  if (filter_control_order(ss) != DEFAULT_FILTER_CONTROL_ORDER) pss_sd(fd, S_filter_control_order, filter_control_order(ss));
  if (filter_control_in_dB(ss) != DEFAULT_FILTER_CONTROL_IN_DB) pss_ss(fd, S_filter_control_in_dB, b2s(filter_control_in_dB(ss)));
  if (filter_control_in_hz(ss) != DEFAULT_FILTER_CONTROL_IN_HZ) pss_ss(fd, S_filter_control_in_hz, b2s(filter_control_in_hz(ss)));
  if (with_tracking_cursor(ss) != DEFAULT_WITH_TRACKING_CURSOR) pss_sd(fd, S_with_tracking_cursor, (int)with_tracking_cursor(ss));
  if (in_show_controls(ss) != DEFAULT_SHOW_CONTROLS) pss_ss(fd, S_show_controls, b2s(in_show_controls(ss)));

  save_colors(fd);

  if (fneq(mus_srate(), MUS_DEFAULT_SAMPLING_RATE)) pss_sf(fd, S_mus_srate, mus_srate());
  if (mus_file_buffer_size() != MUS_DEFAULT_FILE_BUFFER_SIZE) pss_sd(fd, S_mus_file_buffer_size, mus_file_buffer_size());
  if (mus_array_print_length() != MUS_DEFAULT_ARRAY_PRINT_LENGTH) pss_sd(fd, S_mus_array_print_length, mus_array_print_length());
  if (clm_default_table_size_c() != MUS_CLM_DEFAULT_TABLE_SIZE) pss_sod(fd, S_clm_table_size, clm_default_table_size_c());

  {
    int srate = 0, chans = 0;
    mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
    mus_header_raw_defaults(&srate, &chans, &samp_type);
    if ((chans != 2) ||
	(srate != 44100) ||
	(samp_type != MUS_BSHORT))
      {
#if HAVE_SCHEME
	fprintf(fd, "(set! (mus-header-raw-defaults) (list %d %d %s))\n",
		srate,
		chans,
		mus_sample_type_to_string(samp_type));
#endif
#if HAVE_RUBY
	fprintf(fd, "set_mus_header_raw_defaults([%d, %d, %s])\n",
		srate,
		chans,
		mus_sample_type_to_string(samp_type));
#endif
#if HAVE_FORTH
	fprintf(fd, "'( %d %d %s ) set-mus-header-raw-defaults drop\n",
		srate,
		chans,
		mus_sample_type_to_string(samp_type));
#endif
      }
  }
  
  fprintf(fd, "%s end of snd options\n", Xen_comment_mark);
}


/* next two are for the help menu */

void global_control_panel_state(void)
{
  char *buf;
  snd_help_append("\n\nCurrent control panel defaults:\n\n");
  buf = (char *)calloc(1024, sizeof(char));
  snprintf(buf, 1024, "amp bounds: %.3f to %.3f\n", 
	       amp_control_min(ss), amp_control_max(ss));
  snd_help_append(buf);
  snprintf(buf, 1024, "speed bounds: %.3f to %.3f, tones: %d, style: %s\n",
	       speed_control_min(ss), speed_control_max(ss),
	       speed_control_tones(ss),
	       speed_control_style_name(speed_control_style(ss)));
  snd_help_append(buf);
  snprintf(buf, 1024, "expand bounds: %.3f to %.3f, ramp: %.3f, hop: %.3f, length: %.3f, jitter: %.3f\n",
	       expand_control_min(ss), expand_control_max(ss),
	       expand_control_ramp(ss), expand_control_hop(ss), expand_control_length(ss), expand_control_jitter(ss));
  snd_help_append(buf);
  snprintf(buf, 1024, "contrast bounds: %.3f to %.3f, amp: %.3f\n",
	       contrast_control_min(ss), contrast_control_max(ss),
	       contrast_control_amp(ss));
  snd_help_append(buf);
  snprintf(buf, 1024, "reverb scale: %.3f to %.3f, length: %.3f to %.3f, feedbacl: %.3f, lowpass: %.3f, decay: %.3f\n",
	       reverb_control_scale_min(ss), reverb_control_scale_max(ss),
	       reverb_control_length_min(ss), reverb_control_length_max(ss),
	       reverb_control_feedback(ss), reverb_control_lowpass(ss), reverb_control_decay(ss));
  snd_help_append(buf);
  snprintf(buf, 1024, "filter order: %d, in dB: %s, in Hz: %s\n",
	       filter_control_order(ss),
	       b2s(filter_control_in_dB(ss)),
	       b2s(filter_control_in_hz(ss)));
  snd_help_append(buf);
  snd_help_back_to_top();
  free(buf);
}


void global_fft_state(void)
{
  char *buf;
  snd_help_append("\n\nCurrent FFT defaults:\n\n");
  buf = (char *)calloc(1024, sizeof(char));
  snprintf(buf, 1024, "fft size: %" PRId64 "\n    type: %s\n    window: %s (alpha: %.3f, beta: %.3f)\n",
	       transform_size(ss), 
	       TO_VAR_NAME(transform_program_name(transform_type(ss))),
	       TO_VAR_NAME(mus_fft_window_xen_name(fft_window(ss))),
	       fft_window_alpha(ss),
	       fft_window_beta(ss));
  snd_help_append(buf);
  snprintf(buf, 1024, "    graph-type: %s\n    show-peaks: %s (max: %d)\n    show-selection-fft: %s\n",
	       transform_graph_type_name(transform_graph_type(ss)),
	       b2s(show_transform_peaks(ss)),
	       max_transform_peaks(ss),
	       b2s(show_selection_transform(ss)));
  snd_help_append(buf);
  snprintf(buf, 1024, "    log freq: %s (start: %.3f)\n    dB: %s, min-dB: %.3f\n    normalization: %s\n",
	       b2s(fft_log_frequency(ss)),
	       log_freq_start(ss),
	       b2s(fft_log_magnitude(ss)),	       
	       min_dB(ss),
	       transform_normalization_name(transform_normalization(ss)));
  snd_help_append(buf);
  snd_help_back_to_top();
  free(buf);
}


#if HAVE_SCHEME

static void set_print_lengths(int len); 

static void save_property_list(FILE *fd, Xen property_list, int chan, int edpos)
{
  Xen ignore_list;
  int old_print_length, old_vct_print_length;
  int old_s7_print_length;

  old_s7_print_length = s7_print_length(s7);
  old_vct_print_length = mus_vct_print_length();
  old_print_length = print_length(ss);

  /* make sure we don't truncate vector output with "..." */
  set_print_lengths(1000000); /* this sets all three lengths */  

  ignore_list = Xen_assoc(C_string_to_Xen_symbol("save-state-ignore"), property_list);

  if (!(Xen_is_list(ignore_list)))
    {
      char *temp = NULL;
      if (chan == -1)
	fprintf(fd, "%s(set! (%s sfile) \'%s)\n", white_space, S_sound_properties, temp = Xen_object_to_C_string(property_list));
      else 
	{
	  if (edpos == -1)
	    fprintf(fd, "%s(set! (%s sfile %d) \'%s)\n", white_space, S_channel_properties, chan, temp = Xen_object_to_C_string(property_list));
	  else fprintf(fd, "%s(set! (%s sfile %d %d) \'%s)\n", white_space, S_edit_properties, chan, edpos, temp = Xen_object_to_C_string(property_list));
	}
      if (temp) free(temp);
    }
  else
    {
      Xen new_properties = Xen_empty_list;
      int i, property_len, gc_loc;
      gc_loc = snd_protect(new_properties);
      property_len = Xen_list_length(property_list);
      for (i = 0; i < property_len; i++)
	{
	  Xen property;
	  property = Xen_list_ref(property_list, i);
	  if (Xen_is_false(Xen_member(Xen_car(property), ignore_list)))
	    new_properties = Xen_cons(property, new_properties);
	}
      if (!(Xen_is_null(new_properties)))
	{
	  char *temp = NULL;
	  if (chan == -1)
	    fprintf(fd, "%s(set! (%s sfile) \'%s)\n", white_space, S_sound_properties, temp = Xen_object_to_C_string(new_properties));
	  else 
	    {
	      if (edpos == -1)
		fprintf(fd, "%s(set! (%s sfile %d) \'%s)\n", white_space, S_channel_properties, chan, temp = Xen_object_to_C_string(new_properties));
	      else fprintf(fd, "%s(set! (%s sfile %d %d) \'%s)\n", white_space, S_edit_properties, chan, edpos, temp = Xen_object_to_C_string(new_properties));
	    }
	  if (temp) free(temp);
	}
      snd_unprotect_at(gc_loc);
    }

  /* restore the various print lengths */
  set_print_length(old_print_length);
  mus_vct_set_print_length(old_vct_print_length);
  s7_set_print_length(s7, old_s7_print_length);
}
#endif


#if HAVE_RUBY
static void save_property_list(FILE *fd, Xen property_list, int chan, int edpos)
{
  Xen ignore_list;
  ignore_list = rb_ary_assoc(property_list, C_string_to_Xen_symbol("save_state_ignore"));
  if (!(Xen_is_vector(ignore_list)))
    {
      if (chan == -1)
	fprintf(fd, "%sset_%s(%s, sfile)\n", white_space, to_proc_name(S_sound_properties), Xen_object_to_C_string(property_list));
      else 
	{
	  if (edpos == -1)
	    fprintf(fd, "%sset_%s(%s, sfile, %d)\n", white_space, to_proc_name(S_channel_properties), Xen_object_to_C_string(property_list), chan);
	  else fprintf(fd, "%sset_%s(%s, sfile, %d, %d)\n", white_space, to_proc_name(S_edit_properties), Xen_object_to_C_string(property_list), chan, edpos);
	}
    }
  else
    {
      Xen ignore_vec, new_properties = Xen_empty_list;
      int i, property_len, gc_loc;
      gc_loc = snd_protect(new_properties);
      property_len = Xen_list_length(property_list);
      ignore_vec = Xen_vector_ref(ignore_list, 1);
      if (Xen_is_vector(ignore_vec))
	{
	  for (i = 0; i < property_len; i++)
	    {
	      Xen property;
	      property = Xen_list_ref(property_list, i);
	      if (Xen_is_false(rb_ary_includes(ignore_vec, Xen_car(property))))
		new_properties = Xen_cons(property, new_properties);
	    }
	}
      else
	{
	  for (i = 0; i < property_len; i++)
	  new_properties = Xen_cons(Xen_list_ref(property_list, i), new_properties);
	}
      if (!(Xen_is_null(new_properties)))
	{
	  if (chan == -1)
	    fprintf(fd, "%sset_%s(%s, sfile)\n", white_space, to_proc_name(S_sound_properties), Xen_object_to_C_string(new_properties));
	  else 
	    {
	      if (edpos == -1)
		fprintf(fd, "%sset_%s(%s, sfile, %d)\n", white_space, to_proc_name(S_channel_properties), Xen_object_to_C_string(new_properties), chan);
	      else fprintf(fd, "%sset_%s(%s, sfile, %d, %d)\n", white_space, to_proc_name(S_edit_properties), Xen_object_to_C_string(new_properties), chan, edpos);
	    }
	}
      snd_unprotect_at(gc_loc);
    }
}
#endif


#if HAVE_FORTH
static void save_property_list(FILE *fd, Xen property_list, int chan, int edpos)
{
  Xen ignore_list;
  ignore_list = Xen_assoc(C_string_to_Xen_symbol("save-state-ignore"), property_list);
  if (!(Xen_is_list(ignore_list)))
    {
      if (chan == -1)
	fprintf(fd, "%s%s sfile set-%s drop\n", white_space, fth_to_c_dump(property_list), S_sound_properties);
      else 
	{
	  if (edpos == -1)
	    fprintf(fd, "%s%s sfile %d set-%s drop\n", white_space, fth_to_c_dump(property_list), chan, S_channel_properties);
	  else fprintf(fd, "%s%s sfile %d %d set-%s drop\n", white_space, fth_to_c_dump(property_list), chan, edpos, S_edit_properties);
	}
    }
  else
    {
      Xen new_properties = Xen_empty_list;
      int i, property_len, gc_loc;
      gc_loc = snd_protect(new_properties);
      property_len = Xen_list_length(property_list);
      for (i = 0; i < property_len; i++)
	{
	  Xen property;
	  property = Xen_list_ref(property_list, i);
	  if (Xen_is_false(Xen_member(Xen_car(property), ignore_list)))
	    new_properties = Xen_cons(property, new_properties);
	}
      if (!(Xen_is_null(new_properties)))
	{
	  if (chan == -1)
	    fprintf(fd, "%s%s sfile set-%s drop\n", white_space, fth_to_c_dump(new_properties), S_sound_properties);
	  else 
	    {
	      if (edpos == -1)
		fprintf(fd, "%s%s sfile %d set-%s drop\n", white_space, fth_to_c_dump(new_properties), chan, S_channel_properties);
	      else fprintf(fd, "%s%s sfile %d %d set-%s drop\n", white_space, fth_to_c_dump(new_properties), chan, edpos, S_edit_properties);
	    }
	}
      snd_unprotect_at(gc_loc);
    }
}
#endif


#if (!HAVE_EXTENSION_LANGUAGE)
static void save_property_list(FILE *fd, Xen property_list, int chan, int edpos) {}
#endif


static void check_selection(FILE *fd, chan_info *cp)
{
  if (selection_is_active_in_channel(cp))
    {
      mus_long_t beg, end;
      beg = selection_beg(cp);
      end = selection_end(cp);
      pcp_ss(fd, S_selection_member, b2s(true), cp->chan);
      pcp_sod(fd, S_selection_position, beg, cp->chan);
      pcp_sod(fd, S_selection_framples, end - beg + 1, cp->chan);     
    }
}


static int find_sound_nth(snd_info *nsp)
{
  int i, which = 0;
  for (i = 0; i < nsp->index; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	if ((mus_strcmp(nsp->short_filename, sp->short_filename)) || 
	    (mus_strcmp(nsp->filename, sp->filename)))
	  which++;
    }
  return(which);
}


void open_save_sound_block(snd_info *sp, FILE *fd, bool with_nth)
{
  /* here we have to use the 'nth' arg to find_sound -- it should return #f if such an 'nth' case is not found,
   *   so that we can tell when to open another view on a given file
   */
#if HAVE_RUBY
  fprintf(fd, "begin\n  sfile = %s(\"%s\", %d)\n  if (sfile == false)\n    sfile = %s(\"%s\")\n  end\n",
	  to_proc_name(S_find_sound),
	  sp->short_filename,
	  (with_nth) ? find_sound_nth(sp) : 0,
	  to_proc_name((sp->user_read_only == FILE_READ_ONLY) ? S_view_sound : S_open_sound),
	  sp->filename);
  
#endif
#if HAVE_SCHEME
  fprintf(fd, "(let* ((sfile (or (%s \"%s\" %d) (%s \"%s\")))\n"
              "       (-env- (curlet)))\n"
              "  (if sfile\n"
              "    (begin\n"
              "      (define (-mix-selection- beg pos len snd chn sel-chn start-chn)\n"
              "        (do ((ch start-chn (+ ch 1)))\n"
              "            ((> ch sel-chn))\n"
              "          (set! (selection-member? snd ch) #t)\n"
              "          (set! (selection-position snd ch) pos)\n"
              "          (set! (selection-framples snd ch) len))\n"
              "        (mix-selection beg snd chn sel-chn))\n",
	  S_find_sound,
	  sp->short_filename, /* short filename ok because find-sound searches for that name as well as the full filename */
	  (with_nth) ? find_sound_nth(sp) : 0,
	  (sp->user_read_only == FILE_READ_ONLY) ? S_view_sound : S_open_sound,
	  sp->filename);
#endif
#if HAVE_FORTH
  fprintf(fd, "\"%s\" %d %s to sfile\nsfile false? [if] \"%s\" %s to sfile [then]\n",
	  sp->short_filename,
	  (with_nth) ? find_sound_nth(sp) : 0,
	  S_find_sound,
	  sp->filename,
	  (sp->user_read_only == FILE_READ_ONLY) ? S_view_sound : S_open_sound);
#endif
}


void close_save_sound_block(FILE *fd, bool need_f)
{
#if HAVE_RUBY
  fprintf(fd, "end\n");
#endif
#if HAVE_SCHEME
  if (need_f)
    fprintf(fd, "      #f)))\n"); /* avoid empty begin if no field was output */
  else fprintf(fd, "      )))\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "\n");
#endif
}


static bool is_default_envelope(env *e)
{
  return((e) &&
	 (e->pts == 2) &&
	 (e->base == 1.0) &&
	 (e->data[0] == 0.0) &&
	 (e->data[1] == 1.0) &&
	 (e->data[2] == 1.0) &&
	 (e->data[3] == 1.0));
}


void save_sound_state(snd_info *sp, void *ptr) 
{
  /* called only after the global settings have been established, so here we can't use the DEFAULT_* macros that are ambiguous */
  int chan;
  FILE *fd;
  chan_info *cp;
  fd = (FILE *)ptr;
  open_save_sound_block(sp, fd, true);
  b_ok = false; 
  if (sp->sync != DEFAULT_SYNC) psp_sd(fd, S_sync, sp->sync);
  if (sp->contrast_control_on != DEFAULT_CONTRAST_CONTROL_ON) psp_ss(fd, S_contrast_control_on, b2s(sp->contrast_control_on));
  if (fneq(sp->contrast_control, DEFAULT_CONTRAST_CONTROL)) psp_sf(fd, S_contrast_control, sp->contrast_control);
  if ((fneq(sp->contrast_control_min, DEFAULT_CONTRAST_CONTROL_MIN)) ||
      (fneq(sp->contrast_control_max, DEFAULT_CONTRAST_CONTROL_MAX)))
    psp_sl(fd, S_contrast_control_bounds, sp->contrast_control_min, sp->contrast_control_max);
  if (fneq(sp->contrast_control_amp, DEFAULT_CONTRAST_CONTROL_AMP)) psp_sf(fd, S_contrast_control_amp, sp->contrast_control_amp);
  if (sp->expand_control_on != DEFAULT_EXPAND_CONTROL_ON) psp_ss(fd, S_expand_control_on, b2s(sp->expand_control_on));
  if (fneq(sp->expand_control, DEFAULT_EXPAND_CONTROL)) psp_sf(fd, S_expand_control, sp->expand_control);
  if ((fneq(sp->expand_control_min, DEFAULT_EXPAND_CONTROL_MIN)) ||
      (fneq(sp->expand_control_max, DEFAULT_EXPAND_CONTROL_MAX)))
    psp_sl(fd, S_expand_control_bounds, sp->expand_control_min, sp->expand_control_max);
  if (fneq(sp->expand_control_ramp, DEFAULT_EXPAND_CONTROL_RAMP)) psp_sf(fd, S_expand_control_ramp, sp->expand_control_ramp);
  if (fneq(sp->expand_control_hop, DEFAULT_EXPAND_CONTROL_HOP)) psp_sf(fd, S_expand_control_hop, sp->expand_control_hop);
  if (fneq(sp->expand_control_jitter, DEFAULT_EXPAND_CONTROL_JITTER)) psp_sf(fd, S_expand_control_jitter, sp->expand_control_jitter);
  if (fneq(sp->expand_control_length, DEFAULT_EXPAND_CONTROL_LENGTH)) psp_sf(fd, S_expand_control_length, sp->expand_control_length);
  if (sp->speed_control_tones != DEFAULT_SPEED_CONTROL_TONES) psp_sd(fd, S_speed_control_tones, sp->speed_control_tones);
  if (sp->speed_control_style != DEFAULT_SPEED_CONTROL_STYLE) psp_ss(fd, S_speed_control_style, speed_control_style_name(sp->speed_control_style));
  if (fneq(sp->speed_control, DEFAULT_SPEED_CONTROL)) 
    {
#if XEN_HAVE_RATIOS
      if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	{
	  /* no ratios in Ruby */
#if HAVE_FORTH
	  fprintf(fd, "%s%d/%d set-%s drop\n", white_space,
                  sp->speed_control_numerator * sp->speed_control_direction,
                  sp->speed_control_denominator, S_speed_control);
#else
	  fprintf(fd, "%s(set! (%s sfile) %d/%d)\n", white_space, S_speed_control,
                  sp->speed_control_numerator * sp->speed_control_direction,
                  sp->speed_control_denominator);
#endif
	}
      else
#endif
      psp_sf(fd, S_speed_control, sp->speed_control * sp->speed_control_direction);
    }
  if ((fneq(sp->speed_control_min, DEFAULT_SPEED_CONTROL_MIN)) ||
      (fneq(sp->speed_control_max, DEFAULT_SPEED_CONTROL_MAX)))
    psp_sl(fd, S_speed_control_bounds, sp->speed_control_min, sp->speed_control_max);
  if (sp->reverb_control_on != DEFAULT_REVERB_CONTROL_ON) psp_ss(fd, S_reverb_control_on, b2s(sp->reverb_control_on));
  if (fneq(sp->reverb_control_scale, DEFAULT_REVERB_CONTROL_SCALE)) psp_sf(fd, S_reverb_control_scale, sp->reverb_control_scale);
  if ((fneq(sp->reverb_control_scale_min, DEFAULT_REVERB_CONTROL_SCALE_MIN)) ||
      (fneq(sp->reverb_control_scale_max, DEFAULT_REVERB_CONTROL_SCALE_MAX)))
    psp_sl(fd, S_reverb_control_scale_bounds, sp->reverb_control_scale_min, sp->reverb_control_scale_max);
  if (fneq(sp->reverb_control_length, DEFAULT_REVERB_CONTROL_LENGTH)) psp_sf(fd, S_reverb_control_length, sp->reverb_control_length);
  if ((fneq(sp->reverb_control_length_min, DEFAULT_REVERB_CONTROL_LENGTH_MIN)) ||
      (fneq(sp->reverb_control_length_max, DEFAULT_REVERB_CONTROL_LENGTH_MAX)))
    psp_sl(fd, S_reverb_control_length_bounds, sp->reverb_control_length_min, sp->reverb_control_length_max);
  if (fneq(sp->reverb_control_feedback, DEFAULT_REVERB_CONTROL_FEEDBACK)) psp_sf(fd, S_reverb_control_feedback, sp->reverb_control_feedback);
  if (fneq(sp->reverb_control_lowpass, DEFAULT_REVERB_CONTROL_LOWPASS)) psp_sf(fd, S_reverb_control_lowpass, sp->reverb_control_lowpass);
  if (fneq(sp->reverb_control_decay, DEFAULT_REVERB_CONTROL_DECAY)) psp_sf(fd, S_reverb_control_decay, sp->reverb_control_decay);
  if (fneq(sp->amp_control, DEFAULT_AMP_CONTROL)) psp_sf(fd, S_amp_control, sp->amp_control);
  if ((fneq(sp->amp_control_min, DEFAULT_AMP_CONTROL_MIN)) ||
      (fneq(sp->amp_control_max, DEFAULT_AMP_CONTROL_MAX)))
    psp_sl(fd, S_amp_control_bounds, sp->amp_control_min, sp->amp_control_max);
  if (sp->filter_control_on != DEFAULT_FILTER_CONTROL_ON) psp_ss(fd, S_filter_control_on, b2s(sp->filter_control_on));
  if (sp->filter_control_order != DEFAULT_FILTER_CONTROL_ORDER) psp_sd(fd, S_filter_control_order, sp->filter_control_order);
  if (sp->filter_control_in_dB != DEFAULT_FILTER_CONTROL_IN_DB) psp_ss(fd, S_filter_control_in_dB, b2s(sp->filter_control_in_dB));
  if (sp->filter_control_in_hz != DEFAULT_FILTER_CONTROL_IN_HZ) psp_ss(fd, S_filter_control_in_hz, b2s(sp->filter_control_in_hz));
  if ((sp->filter_control_envelope) && (!(is_default_envelope(sp->filter_control_envelope))))
    {
      char *tmpstr = NULL;
      psp_ss(fd, S_filter_control_envelope, tmpstr = env_to_string(sp->filter_control_envelope));
      if (tmpstr) free(tmpstr);
    }

  if ((Xen_is_vector(sp->properties)) &&
      (Xen_is_list(Xen_vector_ref(sp->properties, 0))) &&
      (!(Xen_is_null(Xen_vector_ref(sp->properties, 0)))))
    {
      save_property_list(fd, Xen_vector_ref(sp->properties, 0), -1, -1); /* sound-properties */
    }
  for (chan = 0; chan < (int)sp->nchans; chan++)
    {
      axis_info *ap;

      cp = sp->chans[chan];
      if ((!cp) || (!cp->edits) || (!cp->sounds)) break;
      ap = cp->axis;
      if (!(cp->graph_time_on)) pcp_ss(fd, S_time_graph_on, b2s(cp->graph_time_on), chan);
      if (cp->graph_transform_on) pcp_ss(fd, S_transform_graph_on, b2s(cp->graph_transform_on), chan);
      if (cp->graph_lisp_on) pcp_ss(fd, S_lisp_graph_on, b2s(cp->graph_lisp_on), chan);
      if (ap)
	{
	  if (((ap->x0 != 0.0) || (ap->x1 != 0.1)) && (ap->x1 > .0005)) pcp_sl(fd, S_x_bounds, ap->x0, ap->x1, chan);
	  if ((ap->y0 != -1.0) || (ap->y1 != 1.0)) pcp_sl(fd, S_y_bounds, ap->y0, ap->y1, chan);
	}
      if (cp->cursor_size != DEFAULT_CURSOR_SIZE) pcp_sd(fd, S_cursor_size, cp->cursor_size, chan);
      if (cp->cursor_style != DEFAULT_CURSOR_STYLE) pcp_ss(fd, S_cursor_style, cursor_style_name(cp->cursor_style), chan);
      if (cp->tracking_cursor_style != DEFAULT_TRACKING_CURSOR_STYLE) pcp_ss(fd, S_tracking_cursor_style, cursor_style_name(cp->tracking_cursor_style), chan);
      if (cp->show_marks != show_marks(ss)) pcp_ss(fd, S_show_marks, b2s(cp->show_marks), chan);
      if (cp->show_y_zero != show_y_zero(ss)) pcp_ss(fd, S_show_y_zero, b2s(cp->show_y_zero), chan);
      if (cp->show_grid != show_grid(ss)) pcp_ss(fd, S_show_grid, b2s((bool)(cp->show_grid)), chan);
      if (cp->show_sonogram_cursor != show_sonogram_cursor(ss)) pcp_ss(fd, S_show_sonogram_cursor, b2s(cp->show_sonogram_cursor), chan);
      if (cp->wavo_hop != wavo_hop(ss)) pcp_sd(fd, S_wavo_hop, cp->wavo_hop, chan);
      if (cp->wavo_trace != wavo_trace(ss)) pcp_sd(fd, S_wavo_trace, cp->wavo_trace, chan);
      if (cp->max_transform_peaks != max_transform_peaks(ss)) pcp_sd(fd, S_max_transform_peaks, cp->max_transform_peaks, chan);
      if (cp->show_transform_peaks != show_transform_peaks(ss)) pcp_ss(fd, S_show_transform_peaks, b2s(cp->show_transform_peaks), chan);
      if (cp->fft_log_frequency != fft_log_frequency(ss)) pcp_ss(fd, S_fft_log_frequency, b2s(cp->fft_log_frequency), chan);
      if (cp->fft_log_magnitude != fft_log_magnitude(ss)) pcp_ss(fd, S_fft_log_magnitude, b2s(cp->fft_log_magnitude), chan);
      if (cp->fft_with_phases != fft_with_phases(ss)) pcp_ss(fd, S_fft_with_phases, b2s(cp->fft_with_phases), chan);
      if (cp->with_verbose_cursor != with_verbose_cursor(ss)) pcp_ss(fd, S_with_verbose_cursor, b2s(cp->with_verbose_cursor), chan);
      if (cp->zero_pad != zero_pad(ss)) pcp_sd(fd, S_zero_pad, cp->zero_pad, chan);
      if (cp->wavelet_type != wavelet_type(ss)) pcp_sd(fd, S_wavelet_type, cp->wavelet_type, chan);
      if (fneq(cp->min_dB, min_dB(ss))) pcp_sf(fd, S_min_dB, cp->min_dB, chan);
      if (fneq(cp->spectro_x_angle, spectro_x_angle(ss))) pcp_sf(fd, S_spectro_x_angle, cp->spectro_x_angle, chan);
      if (fneq(cp->spectro_y_angle, spectro_y_angle(ss))) pcp_sf(fd, S_spectro_y_angle, cp->spectro_y_angle, chan);
      if (fneq(cp->spectro_z_angle, spectro_z_angle(ss))) pcp_sf(fd, S_spectro_z_angle, cp->spectro_z_angle, chan);
      if (fneq(cp->spectro_x_scale, spectro_x_scale(ss))) pcp_sf(fd, S_spectro_x_scale, cp->spectro_x_scale, chan);
      if (fneq(cp->spectro_y_scale, spectro_y_scale(ss))) pcp_sf(fd, S_spectro_y_scale, cp->spectro_y_scale, chan);
      if (fneq(cp->spectro_z_scale, spectro_z_scale(ss))) pcp_sf(fd, S_spectro_z_scale, cp->spectro_z_scale, chan);
      if (fneq(cp->spectrum_end, spectrum_end(ss))) pcp_sf(fd, S_spectrum_end, cp->spectrum_end, chan);
      if (fneq(cp->spectrum_start, spectrum_start(ss))) pcp_sf(fd, S_spectrum_start, cp->spectrum_start, chan);
      if (fneq(cp->fft_window_alpha, fft_window_alpha(ss))) pcp_sf(fd, S_fft_window_alpha, cp->fft_window_alpha, chan);
      if (fneq(cp->fft_window_beta, fft_window_beta(ss))) pcp_sf(fd, S_fft_window_beta, cp->fft_window_beta, chan);
      if (cp->spectro_hop != spectro_hop(ss)) pcp_sd(fd, S_spectro_hop, cp->spectro_hop, chan);
      if (cp->transform_size != transform_size(ss)) pcp_sod(fd, S_transform_size, cp->transform_size, chan);
      if (cp->transform_graph_type != transform_graph_type(ss)) 
	pcp_ss(fd, S_transform_graph_type, transform_graph_type_name(cp->transform_graph_type), chan);
      if (cp->time_graph_type != time_graph_type(ss)) pcp_ss(fd, S_time_graph_type, time_graph_type_name(cp->time_graph_type), chan);
      if (cp->fft_window != fft_window(ss)) pcp_ss(fd, S_fft_window, TO_VAR_NAME(mus_fft_window_xen_name(cp->fft_window)), chan);
      if (cp->transform_type != transform_type(ss)) pcp_ss(fd, S_transform_type, TO_VAR_NAME(transform_program_name(cp->transform_type)), chan);
      /* this is assuming the added transform definition (if any) can be found -- maybe not a good idea */
      if (cp->transform_normalization != transform_normalization(ss)) 
	pcp_ss(fd, S_transform_normalization, transform_normalization_name(cp->transform_normalization), chan);
      if (cp->time_graph_style != graph_style(ss)) pcp_ss(fd, S_time_graph_style, graph_style_name(cp->time_graph_style), chan);
      if (cp->lisp_graph_style != graph_style(ss)) pcp_ss(fd, S_lisp_graph_style, graph_style_name(cp->lisp_graph_style), chan);
      if (cp->transform_graph_style != graph_style(ss)) pcp_ss(fd, S_transform_graph_style, graph_style_name(cp->transform_graph_style), chan);
      if (cp->show_mix_waveforms != show_mix_waveforms(ss)) pcp_ss(fd, S_show_mix_waveforms, b2s(cp->show_mix_waveforms), chan);
      if (cp->dot_size != dot_size(ss)) pcp_sd(fd, S_dot_size, cp->dot_size, chan);
      if (fneq(cp->grid_density, grid_density(ss))) pcp_sf(fd, S_grid_density, cp->grid_density, chan);
      if (cp->x_axis_style != x_axis_style(ss)) pcp_ss(fd, S_x_axis_style, x_axis_style_name(cp->x_axis_style), chan);
      if (fneq(cp->beats_per_minute, beats_per_minute(ss))) pcp_sf(fd, S_beats_per_minute, cp->beats_per_minute, chan);
      if (cp->beats_per_measure != beats_per_measure(ss)) pcp_sd(fd, S_beats_per_measure, cp->beats_per_measure, chan);
      if (cp->show_axes != show_axes(ss)) pcp_ss(fd, S_show_axes, show_axes2string(cp->show_axes), chan);
      if (cp->graphs_horizontal != graphs_horizontal(ss)) pcp_ss(fd, S_graphs_horizontal, b2s(cp->graphs_horizontal), chan);
      if ((Xen_is_vector(cp->properties)) &&
	  (Xen_is_list(Xen_vector_ref(cp->properties, 0))) &&
	  (!(Xen_is_null(Xen_vector_ref(cp->properties, 0)))))
	{
	  save_property_list(fd, Xen_vector_ref(cp->properties, 0), chan, -1); /* channel-properties */
	}

      /* ap->default_xlabel if not null, user explicitly set it */
      /* ylabel can only be a user choice -- never set by Snd */
      if (cp->axis)
	{
	  if (cp->axis->default_xlabel) pcp_sss(fd, S_x_axis_label, cp->axis->default_xlabel, chan, "time-graph");
	  if (cp->axis->ylabel)         pcp_sss(fd, S_y_axis_label, cp->axis->ylabel,         chan, "time-graph");
	}
      if ((cp->fft) &&
	  (cp->fft->axis))
	{
	  if (cp->fft->axis->default_xlabel) pcp_sss(fd, S_x_axis_label, cp->fft->axis->default_xlabel, chan, "transform-graph");
	  if (cp->fft->axis->ylabel)         pcp_sss(fd, S_y_axis_label, cp->fft->axis->ylabel,         chan, "transform-graph");
	}
      /* lisp_info is hidden in snd-chn.c */

      edit_history_to_file(fd, cp, true);
      {
	int i;
	for (i = 0; i <= cp->edit_ctr; i++)
	  {
	    ed_list *ed;
	    ed = cp->edits[i];
	    if ((Xen_is_vector(ed->properties)) &&
		(Xen_is_list(Xen_vector_ref(ed->properties, 0))) &&
		(!(Xen_is_null(Xen_vector_ref(ed->properties, 0)))))
	      {
		save_property_list(fd, Xen_vector_ref(ed->properties, 0), chan, i); /* edit-properties */
	      }
	  }
      }

#if HAVE_SCHEME
      mix_info_to_file(fd, cp);
#endif
      if (cursor_sample(cp) != 0) pcp_sod(fd, S_cursor, cursor_sample(cp), chan);
      check_selection(fd, cp);
      if ((!sp->remembering) &&
	  (selected_channel() == cp))
	{
#if HAVE_SCHEME
	  fprintf(fd, "%s(set! _saved_snd_selected_sound_ sfile)\n", white_space);
	  fprintf(fd, "%s(set! _saved_snd_selected_channel_ %d)\n", white_space, cp->chan);
#endif
#if HAVE_RUBY
	  fprintf(fd, "%ssaved_snd_selected_sound = sfile\n", white_space);
	  fprintf(fd, "%ssaved_snd_selected_channel = %d\n", white_space, cp->chan);
#endif
#if HAVE_FORTH
	  fprintf(fd, "%ssfile to saved_snd_selected_sound\n", white_space);
	  fprintf(fd, "%s%d to saved_snd_selected_channel\n", white_space, cp->chan);
#endif
	}
    }
  close_save_sound_block(fd, !b_ok);
}


static Xen after_save_state_hook;
static Xen before_save_state_hook;

void save_state(const char *save_state_name)
{
  FILE *save_fd;
  char *fullname;
  bool append_new_state = false;
  if (!save_state_name)
    {
      snd_error("no save state file name?");
      return;
    }
  fullname = mus_expand_filename(save_state_name);
  if (Xen_hook_has_list(before_save_state_hook))
    {
      Xen res;
      res = run_or_hook(before_save_state_hook, 
			Xen_list_1(C_string_to_Xen_string(fullname)),
			S_before_save_state_hook);
      append_new_state = Xen_boolean_to_C_bool(res);
    }
  if (append_new_state)
    save_fd = FOPEN(fullname, "a");
  else save_fd = FOPEN(fullname, "w");
  if (fullname) {free(fullname); fullname = NULL;}
  if (!save_fd)
    {
      snd_error("can't write %s: %s", save_state_name, snd_io_strerror());
      return;
    }

  save_options(save_fd);                            /* options = user-settable global state variables */
  /* the global settings need to precede possible local settings */

  if (ss->active_sounds > 0)
    {
      if (ss->selected_sound != NO_SELECTION)
	{
#if HAVE_SCHEME
	  fprintf(save_fd, "\n(define _saved_snd_selected_sound_ #f)\n");
	  fprintf(save_fd, "(define _saved_snd_selected_channel_ #f)\n");
#endif
#if HAVE_RUBY
	  fprintf(save_fd, "\nsaved_snd_selected_sound = -1\n");
	  fprintf(save_fd, "saved_snd_selected_channel = -1\n");
#endif
#if HAVE_FORTH
	  fprintf(save_fd, "\n#f value saved_snd_selected_sound\n");
	  fprintf(save_fd, "#f value saved_snd_selected_channel\n");
	  fprintf(save_fd, "#f value sfile\n");
#endif
	}
      for_each_sound_with_void(save_sound_state, (void *)save_fd);      /* current sound state -- will traverse chans */
      if (ss->selected_sound != NO_SELECTION)
	{
#if HAVE_SCHEME
	  fprintf(save_fd, "(if _saved_snd_selected_sound_\n");
	  fprintf(save_fd, "  (begin\n");
	  fprintf(save_fd, "    (%s _saved_snd_selected_sound_)\n", S_select_sound);
	  fprintf(save_fd, "    (%s _saved_snd_selected_channel_)))\n", S_select_channel);
#endif
#if HAVE_RUBY
	  fprintf(save_fd, "if saved_snd_selected_sound != -1\n");
	  fprintf(save_fd, "  select_sound(saved_snd_selected_sound)\n");
	  fprintf(save_fd, "  select_channel(saved_snd_selected_channel)\n");
	  fprintf(save_fd, "end\n");
#endif
#if HAVE_FORTH
	  fprintf(save_fd, "saved_snd_selected_sound false? not [if]\n");
	  fprintf(save_fd, "  saved_snd_selected_sound   %s drop\n", S_select_sound);
	  fprintf(save_fd, "  saved_snd_selected_channel %s drop\n", S_select_channel);
	  fprintf(save_fd, "[then]\n\n");
#endif
	}
    }
  fprintf(save_fd, "\n");
  save_envelope_editor_state(save_fd);                    /* current envelope editor window state */
  save_regions(save_fd);                                  /* regions */
  
  if (transform_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", to_proc_name(S_transform_dialog));
  if (enved_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", to_proc_name(S_enved_dialog));
  if (color_orientation_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", to_proc_name(S_color_orientation_dialog));
  if (region_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", to_proc_name(S_view_regions_dialog));
  save_post_it_dialog_state(save_fd);
  save_find_dialog_state(save_fd);
  save_edit_header_dialog_state(save_fd);
#if USE_MOTIF
  save_print_dialog_state(save_fd);
  save_view_files_dialogs(save_fd);
#endif
  save_file_dialog_state(save_fd);
  
  /* saving mix/track state is problematic.  For example, if we make a selection, mix it,
   *   then make another selection and mix it, we get an edit list:
   *   
   *   (change-samples-with-origin 266 451 "set! -mix-0 (mix-selection 266)" "/home/bil/zap/snd/snd_3309_9.snd" sfile 0 #f (list 1145009982 1848))
   *   (change-samples-with-origin 1655 480 "set! -mix-1 (mix-selection 1655)" "/home/bil/zap/snd/snd_3309_10.snd" sfile 0 #f (list 1145009982 1964))
   *   (set! (selection-member? sfile 0) #t)
   *   (set! (selection-position sfile 0) 816)
   *   (set! (selection-framples sfile 0) 480)
   *
   *  which won't even work for the current selection case!  If we mix some piece of a sound
   *    being edited in another window, we'd need to keep all edits in sync during the restore!
   *  But each mix has a temp file of its data, so perhaps an internal function to fake a mix
   *    using it?
   *
   *  :(edit-list->function)
   *    #<procedure #f ((snd chn) 
   *       (let ((-mix-0 0) (-mix-1 1)) 
   *         (set! -mix-0 (mix-selection 266)) 
   *         ;; selection can change here!
   *         (set! -mix-1 (mix-selection 1655))))>
   *
   *  which is also wrong!  This has to use the shadowing temp files.
   *    so edit-list->function fails if selections involved (mix-selection) [or is this what is expected?]
   *
   *  To keep the mix/track dialogs in sync with the newly restored mixes would require keeping the
   *     old mix/track numbers and mapping to the new ones.
   */
  
  /* the problem here (with saving hooks) is that it is not straightforward to save the function source
   *   (with the current print-set! source option, or with an earlier procedure->string function using
   *   funclet etc); many types print in this case in ways that are not readable.
   *   The functions may depend on globals that are not in loaded files, or that were changed since
   *   loading, and trying to map over the current module's obarray, saving each such variable in
   *   its current form, is a major undertaking (although this can be done for simple vars); additionally, 
   *   what if the user has changed these before restoring -- should the old forms be restored?
   * (this comment dates back to Guile days -- I think hook functions could be saved now)
   */
  
  snd_fclose(save_fd, save_state_name);
  if (Xen_hook_has_list(after_save_state_hook))
    run_hook(after_save_state_hook, 
	     Xen_list_1(C_string_to_Xen_string(save_state_name)),
	     S_after_save_state_hook);
}


const char *save_options_in_prefs(void)
{
#if HAVE_EXTENSION_LANGUAGE
  FILE *fd;
  char *fullname;

#if HAVE_RUBY
  #define SND_PREFS "~/.snd_prefs_ruby"
#endif
#if HAVE_FORTH
  #define SND_PREFS "~/.snd_prefs_forth"
#endif
#if HAVE_SCHEME
  #define SND_PREFS "~/.snd_prefs_s7"
#endif

  fullname = mus_expand_filename(SND_PREFS);
  fd = FOPEN(fullname, "w");
  if (!fd)
    {
      snd_error("can't write %s: %s", SND_PREFS, snd_io_strerror());
      return(NULL);
    }
  save_options(fd);
  snd_fclose(fd, SND_PREFS);
  free(fullname);
  return(SND_PREFS);
#else
  return(NULL);
#endif
}


#if 0
static char *file_extension(char *arg)
{
  char *dot = NULL, *sp;
  if (arg) 
    for (sp = arg; (*sp) != '\0'; sp++) 
      if ((*sp) == '.') 
	dot = (++sp);
  return(dot);
}
#endif


static char *startup_filename = NULL;
static int script_arg = 0, script_argn = 0;
static char **script_args;

static Xen g_script_arg(void) 
{
  #define H_script_arg "(" S_script_arg "): where we are in the startup arg list"
  return(C_int_to_Xen_integer(script_arg));
}


static Xen g_set_script_arg(Xen arg) 
{
  script_arg = Xen_integer_to_C_int(arg); 
  return(arg);
}


static Xen g_script_args(void)
{
  #define H_script_args "(" S_script_args "): the args passed to Snd at startup as a list of strings"
  Xen lst = Xen_empty_list;
  int i;
  for (i = script_argn - 1; i >= 0; i--)
    lst = Xen_cons(C_string_to_Xen_string(script_args[i]), lst);
  return(lst);
}


static void printout_to_stdout(const char *msg, void *ignore)
{
  puts(msg);
}


int handle_next_startup_arg(int auto_open_ctr, char **auto_open_file_names, bool with_title, int args)
{
  char *argname;
  argname = auto_open_file_names[auto_open_ctr];
  if (argname)
    { /* wanted to use "-d" and "-i" but they're in use */
      if ((mus_strcmp("-h", argname)) || 
	  (mus_strcmp("-horizontal", argname)) ||
	  (mus_strcmp("--horizontal", argname)) ||
	  (mus_strcmp("-v", argname)) || 
	  (mus_strcmp("-vertical", argname)) ||
	  (mus_strcmp("--vertical", argname)) ||
	  (mus_strcmp("-notebook", argname)) ||
	  (mus_strcmp("--notebook", argname)) ||
	  (mus_strcmp("-separate", argname)) ||
	  (mus_strcmp("--separate", argname)) ||
	  (mus_strcmp("-nostdin", argname)) ||
	  (mus_strcmp("-noglob", argname)) ||
	  (mus_strcmp("-noinit", argname)) ||
	  (mus_strcmp("--noinit", argname)))
	return(auto_open_ctr + 1);
      else
	{
	  if (mus_strcmp("-init", argname))
	    return(auto_open_ctr + 2);
	  else
	    {
#if (!USE_NO_GUI)
	      if ((mus_strcmp("-p", argname)) ||
		  (mus_strcmp("-preload", argname)) ||
		  (mus_strcmp("--preload", argname)))
		{
		  /* preload sound files in dir (can be ., should be unquoted) */
		  auto_open_ctr++;
		  if ((auto_open_ctr >= args) ||
		      (!auto_open_file_names[auto_open_ctr]))
		    snd_error("%s but no directory to add?", argname);
		  else view_files_add_directory(NULL_WIDGET, auto_open_file_names[auto_open_ctr]);
		}
	      else
#endif
		{
		  if ((mus_strcmp("-l", argname)) ||
		      (mus_strcmp("-load", argname)) ||
		      (mus_strcmp("--load", argname)) ||
		      (mus_strcmp("-b", argname)) ||
		      (mus_strcmp("-batch", argname)) ||
		      (mus_strcmp("--batch", argname)) ||
		      (is_source_file(argname)))
		    {
		      if ((mus_strcmp("-l", argname)) || 
			  (mus_strcmp("-load", argname)) ||
			  (mus_strcmp("--load", argname)) ||
			  (mus_strcmp("-b", argname)) || 
			  (mus_strcmp("-batch", argname)) ||
			  (mus_strcmp("--batch", argname)))
			auto_open_ctr++;
		      if ((auto_open_ctr >= args) ||
			  (!auto_open_file_names[auto_open_ctr]))
			snd_error("%s but no file to load?", argname);
		      else 
			{
			  script_arg = auto_open_ctr;
			  script_argn = args;
			  script_args = auto_open_file_names;
			  redirect_everything_to(printout_to_stdout, NULL);
			  snd_load_file(auto_open_file_names[auto_open_ctr]);
			  redirect_everything_to(NULL, NULL);
			  if (script_arg > auto_open_ctr)
			    auto_open_ctr = script_arg;
			}
		    }
		  else
		    {
		      if ((mus_strcmp("-e", argname)) ||
			  (mus_strcmp("-eval", argname)) ||
			  (mus_strcmp("--eval", argname)))
			{
			  /* evaluate expression */
			  auto_open_ctr++;
			  if ((auto_open_ctr >= args) ||
			      (!auto_open_file_names[auto_open_ctr]))
			    snd_error("%s but no form to evaluate?", argname);
			  else 
			    {
			      char *buf;
			      buf = auto_open_file_names[auto_open_ctr];
			      redirect_everything_to(printout_to_stdout, NULL);
			      snd_report_result(snd_catch_any(eval_str_wrapper, (void *)buf, buf), buf);
			      redirect_everything_to(NULL, NULL);
			    }
			}
		      else
			{
			  if ((with_title) && 
			      (mus_strcmp("-title", argname)))
			    {
			      auto_open_ctr++;
			      if ((auto_open_ctr >= args) ||
				  (!auto_open_file_names[auto_open_ctr]))
				snd_error_without_format("-title but no title?"); /* for gtk -- Xt handles the Motif case */
			      else ss->startup_title = mus_strdup(auto_open_file_names[auto_open_ctr]);
			    }
			  else
			    {
			      if (mus_strcmp("-I", argname))
				{
				  /* added 24-Oct-02: add to load path in either extension language */
				  auto_open_ctr++;
				  if ((auto_open_ctr >= args) ||
				      (!auto_open_file_names[auto_open_ctr]))
				    snd_error_without_format("-I but no path?");
				  else 
				    {
				      Xen_add_to_load_path(auto_open_file_names[auto_open_ctr]);
				    }
				}
			      else
				{
				  if (!startup_filename)
				    startup_filename = mus_strdup(argname);
				  ss->open_requestor = FROM_STARTUP;
				  if (!snd_open_file(argname, FILE_READ_WRITE))
				    {
				      /* non-existent file at startup */
				      if (argname[0] == '-')
					{
					  /* probably a bad option */
					  fprintf(stdout, "bad option: %s\n", argname);
					}
				      else
					{
					  fprintf(stdout, "can't open %s\n", argname);
					  if (ss->startup_errors)
					    ss->startup_errors = mus_format("%s\n%s ;%s\"%s\"\n", ss->startup_errors, listener_prompt(ss), "can't open ", argname);
					  else ss->startup_errors = mus_format(";%s\"%s\"\n", "can't open ", argname);
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  return(auto_open_ctr + 1);
}


static void save_state_error_handler(const char *msg, void *data)
{
  char *filename = (char *)data;
  Xen fname;
  if (filename)
    {
      fname = C_string_to_Xen_string(filename);
      free(filename); /* this is "name" below */
    }
  else fname = C_string_to_Xen_string(save_state_file(ss));
  redirect_snd_error_to(NULL, NULL);
  Xen_error(Xen_make_error_type("cannot-save"),
	    Xen_list_2(C_string_to_Xen_string(S_save_state ": can't save ~S"),
		       fname));
}


static Xen g_save_state(Xen filename) 
{
  char *name = NULL;
  Xen res;
  #define H_save_state "(" S_save_state " :optional filename): save the current Snd state in filename; (load filename) restores it.  The \
default " S_save_state " filename is " DEFAULT_SAVE_STATE_FILE ". It can be changed via " S_save_state_file "."

  Xen_check_type(Xen_is_string_or_unbound(filename), filename, 1, S_save_state, "a string");

  if (Xen_is_bound(filename))
    name = mus_strdup(Xen_string_to_C_string(filename));
  else name = mus_strdup(save_state_file(ss));

  redirect_snd_error_to(save_state_error_handler, (void *)name);
  save_state(name);
  redirect_snd_error_to(NULL, NULL);

  res = C_string_to_Xen_string(name);
  free(name);
  return(res);
}


static Xen g_exit(Xen val) 
{
  #define H_exit "(" S_exit " :optional val): exit Snd"
  if (snd_exit_cleanly(EXIT_NOT_FORCED))
    snd_exit((Xen_is_integer(val)) ? Xen_integer_to_C_int(val) : 1); 
  return(Xen_false);
}


static int snd_access(const char *dir, const char *caller)
{
  int err;
  char *temp;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err == -1)
    {
      Xen res;
      free(temp);
      temp = mus_format("%s: directory %s is not writable: %s", caller, dir, snd_open_strerror());
      res = C_string_to_Xen_string(temp);
      free(temp);
      Xen_error(NO_SUCH_FILE, Xen_list_1(res));
      return(0);
    }
  else snd_close(err, temp);
  snd_remove(temp, IGNORE_CACHE);
  free(temp);
  return(1);
}


static Xen g_temp_dir(void) {return(C_string_to_Xen_string(temp_dir(ss)));}

static Xen g_set_temp_dir(Xen val) 
{
  #define H_temp_dir "(" S_temp_dir "): name of directory for temp files (or " PROC_FALSE "=null)"
  const char *dir = MUS_DEFAULT_TEMP_DIR;
  Xen_check_type(Xen_is_string(val) || Xen_is_false(val), val, 1, S_set S_temp_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (Xen_is_string(val)) dir = Xen_string_to_C_string(val);
  if (snd_access(dir, S_temp_dir))
    {
      if (temp_dir(ss)) free(temp_dir(ss));
      set_temp_dir(mus_strdup(dir));
    }
  return(C_string_to_Xen_string(temp_dir(ss)));
}


static Xen g_peak_env_dir(void) {return(C_string_to_Xen_string(peak_env_dir(ss)));}

static Xen g_set_peak_env_dir(Xen val) 
{
  #define H_peak_env_dir "(" S_peak_env_dir "): name of directory for peak env files (or " PROC_FALSE "=null)"
  Xen_check_type(Xen_is_string(val) || Xen_is_false(val), val, 1, S_set S_peak_env_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (Xen_is_string(val)) 
    {
      const char *dir = NULL;
      dir = Xen_string_to_C_string(val);
      if (snd_access(dir, S_peak_env_dir))
	{
	  if (peak_env_dir(ss)) free(peak_env_dir(ss));
	  set_peak_env_dir(mus_strdup(dir));
	}
    }
  else
    {
      if (peak_env_dir(ss)) free(peak_env_dir(ss));
      set_peak_env_dir(NULL);
    }
  return(C_string_to_Xen_string(peak_env_dir(ss)));
}


static Xen g_ladspa_dir(void) {return(C_string_to_Xen_string(ladspa_dir(ss)));}

static Xen g_set_ladspa_dir(Xen val) 
{
  #define H_ladspa_dir "(" S_ladspa_dir "): name of directory for ladspa plugin libraries"
  Xen_check_type(Xen_is_string(val) || Xen_is_false(val), val, 1, S_set S_ladspa_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (ladspa_dir(ss)) free(ladspa_dir(ss));
  if (Xen_is_false(val))
    set_ladspa_dir(mus_strdup(DEFAULT_LADSPA_DIR));
  else set_ladspa_dir(mus_strdup(Xen_string_to_C_string(val)));
  return(C_string_to_Xen_string(ladspa_dir(ss)));
}


static Xen g_save_state_file(void) {return(C_string_to_Xen_string(save_state_file(ss)));}

static Xen g_set_save_state_file(Xen val) 
{
  const char *filename;
  #define H_save_state_file "(" S_save_state_file "): the name of the saved state file (\"saved-snd." Xen_file_extension "\")"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_save_state_file, "a string"); 
  filename = Xen_string_to_C_string(val);
  if (save_state_file(ss)) free(save_state_file(ss));
  in_set_save_state_file(mus_strdup(filename));
  return(C_string_to_Xen_string(save_state_file(ss)));
}


static Xen g_save_dir(void) {return(C_string_to_Xen_string(save_dir(ss)));}

static Xen g_set_save_dir(Xen val) 
{
  #define H_save_dir "(" S_save_dir "): name of directory for saved state data (or " PROC_FALSE "=null)"
  const char *dir = MUS_DEFAULT_SAVE_DIR;
  Xen_check_type(Xen_is_string(val) || Xen_is_false(val), val, 1, S_set S_save_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (Xen_is_string(val)) dir = Xen_string_to_C_string(val);
  if (snd_access(dir, S_save_dir))
    {
      if (save_dir(ss)) free(save_dir(ss));
      set_save_dir(mus_strdup(dir));
    }
  return(C_string_to_Xen_string(save_dir(ss)));
}


static Xen g_open_file_dialog_directory(void) {return(C_string_to_Xen_string(open_file_dialog_directory(ss)));}

static Xen g_set_open_file_dialog_directory(Xen val) 
{
  #define H_open_file_dialog_directory "(" S_open_file_dialog_directory "): name of directory for initial open file dialog search"
  const char *dir;
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_open_file_dialog_directory, "a string"); 
  dir = Xen_string_to_C_string(val);
  if (snd_access(dir, S_open_file_dialog_directory))
    {
      if (open_file_dialog_directory(ss)) free(open_file_dialog_directory(ss));
      set_open_file_dialog_directory(mus_strdup(dir));
    }
  return(C_string_to_Xen_string(open_file_dialog_directory(ss)));
}


static int snd_screen_height(void)
{
#if USE_MOTIF
  return(HeightOfScreen(ScreenOfDisplay(main_display(ss), 0)));
#else
#if USE_GTK
#if (!GTK_CHECK_VERSION(3, 22, 0)) /* don't combine with USE_GTK above! */
   return(gdk_screen_height());
 #else
   return(4000);
 #endif
#else
  return(4000);
#endif
#endif
}


static int snd_screen_width(void)
{
#if USE_MOTIF
  return(WidthOfScreen(ScreenOfDisplay(main_display(ss), 0)));
#else
#if USE_GTK
  #if (!GTK_CHECK_VERSION(3, 22, 0))
    return(gdk_screen_width());
  #else
    return(4000);
  #endif
#else
  return(4000);
#endif
#endif
}


static Xen g_window_height(void) 
{
  #define H_window_height "(" S_window_height "): current Snd window height in pixels"
  return(C_int_to_Xen_integer(widget_height(main_shell(ss))));
}


static Xen g_set_window_height(Xen height) 
{
  int val;
  Xen_check_type(Xen_is_integer(height), height, 1, S_set S_window_height, "an integer"); 
  val = Xen_integer_to_C_int(height);
  if ((val > 0) && (val < snd_screen_height()))
    {
#if (!USE_NO_GUI)
      set_widget_height(main_shell(ss), val);
#endif
      ss->init_window_height = val;
    }
  return(height);
}


static Xen g_window_width(void) 
{
  #define H_window_width "(" S_window_width "): current Snd window width in pixels"
  return(C_int_to_Xen_integer(widget_width(main_shell(ss))));
}


static Xen g_set_window_width(Xen width) 
{
  int val;
  Xen_check_type(Xen_is_integer(width), width, 1, S_set S_window_width, "an integer"); 
  val = Xen_integer_to_C_int(width);
  if ((val > 0) && (val < snd_screen_width()))
    {
#if (!USE_NO_GUI)
      set_widget_width(main_shell(ss), val);
#endif
      ss->init_window_width = val;
    }
  return(width);
}


static Xen g_window_x(void) 
{
  #define H_window_x "(" S_window_x "): current Snd window x position in pixels"
  return(C_int_to_Xen_integer(widget_x(main_shell(ss))));
}


static Xen g_set_window_x(Xen val) 
{
  int x;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_window_x, "an integer"); 
  x = Xen_integer_to_C_int(val);
  if ((x >= 0) && (x < snd_screen_width()))
    {
      set_widget_x(main_shell(ss), x);
      ss->init_window_x = x;
    }
  return(val);
}


static Xen g_window_y(void) 
{
  #define H_window_y "(" S_window_y "): current Snd window y position in pixels"
  return(C_int_to_Xen_integer(widget_y(main_shell(ss))));
}


static Xen g_set_window_y(Xen val) 
{
  int y;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_window_y, "an integer"); 
  y = Xen_integer_to_C_int(val);
  if ((y >= 0) && (y < snd_screen_height()))
    {
      set_widget_y(main_shell(ss), y);
      ss->init_window_y = y;
    }
  return(val);
}


static Xen g_just_sounds(void)
{
  #define H_just_sounds "(" S_just_sounds "): the 'just sounds' choice in the file chooser dialog"
  return(C_bool_to_Xen_boolean(just_sounds(ss)));
}


static Xen g_set_just_sounds(Xen on) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_just_sounds, "a boolean");
  set_just_sounds(Xen_boolean_to_C_bool(on));
  reflect_just_sounds();
  return(C_bool_to_Xen_boolean(just_sounds(ss)));
}


static Xen g_play_arrow_size(void)
{
  #define H_play_arrow_size "(" S_play_arrow_size "): the size of the play triangles"
  return(C_int_to_Xen_integer(play_arrow_size(ss)));
}


static Xen g_set_play_arrow_size(Xen size) 
{
  int arrow_size;
  Xen_check_type(Xen_is_integer(size), size, 1, S_set S_play_arrow_size, "an integer");

  arrow_size = Xen_integer_to_C_int(size);
  if (arrow_size >= 0)
    set_play_arrow_size(arrow_size);
  else Xen_out_of_range_error(S_set S_play_arrow_size, 1, size, "must be >= 0");

  for_each_chan(update_graph);
  return(size);
}


static Xen g_with_inset_graph(void)
{
  #define H_with_inset_graph "(" S_with_inset_graph "): if " PROC_TRUE " (default is " PROC_FALSE "), display the inset graph in the time domain section."
  return(C_bool_to_Xen_boolean(with_inset_graph(ss)));
}


static Xen g_set_with_inset_graph(Xen on) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_with_inset_graph, "a boolean");
  set_with_inset_graph(Xen_boolean_to_C_bool(on));
  for_each_chan(update_graph);
  return(C_bool_to_Xen_boolean(with_inset_graph(ss)));
}


static Xen g_with_interrupts(void)
{
  #define H_with_interrupts "(" S_with_interrupts "): if " PROC_TRUE ", check for GUI events during computations."
  return(C_bool_to_Xen_boolean(with_interrupts(ss)));
}


static Xen g_set_with_interrupts(Xen on) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_with_interrupts, "a boolean");
  set_with_interrupts(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(with_interrupts(ss)));
}


static Xen g_with_smpte_label(void)
{
  #define H_with_smpte_label "(" S_with_smpte_label "): if " PROC_TRUE " (default is " PROC_FALSE "), display the SMPTE data in the time domain section."
  return(C_bool_to_Xen_boolean(with_smpte_label(ss)));
}


static Xen g_set_with_smpte_label(Xen on) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_with_smpte_label, "a boolean");
  set_with_smpte_label(Xen_boolean_to_C_bool(on));
  for_each_chan(update_graph);
  return(C_bool_to_Xen_boolean(with_smpte_label(ss)));
}


static Xen g_with_pointer_focus(void)
{
  #define H_with_pointer_focus "(" S_with_pointer_focus "): if " PROC_TRUE " (default is " PROC_FALSE "), activate the text or graph widget beneath the mouse."
  return(C_bool_to_Xen_boolean(with_pointer_focus(ss)));
}


static Xen g_set_with_pointer_focus(Xen on) 
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_with_pointer_focus, "a boolean");
  set_with_pointer_focus(Xen_boolean_to_C_bool(on));
  return(C_bool_to_Xen_boolean(with_pointer_focus(ss)));
}


static Xen g_tiny_font(void) {return(C_string_to_Xen_string(tiny_font(ss)));}

static Xen g_set_tiny_font(Xen val) 
{
  #define H_tiny_font "(" S_tiny_font "): font use for some info in the graphs"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_tiny_font, "a string"); 
  set_tiny_font(Xen_string_to_C_string(val)); 
  return(C_string_to_Xen_string(tiny_font(ss)));
}


static Xen g_axis_label_font(void) {return(C_string_to_Xen_string(axis_label_font(ss)));}

static Xen g_set_axis_label_font(Xen val) 
{
  #define H_axis_label_font "(" S_axis_label_font "): font used for axis labels"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_axis_label_font, "a string"); 
  set_axis_label_font(Xen_string_to_C_string(val)); 
  return(C_string_to_Xen_string(axis_label_font(ss)));
}


static Xen g_axis_numbers_font(void) {return(C_string_to_Xen_string(axis_numbers_font(ss)));}

static Xen g_set_axis_numbers_font(Xen val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font "): font used for axis numbers"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_axis_numbers_font, "a string"); 
  set_axis_numbers_font(Xen_string_to_C_string(val)); 
  return(C_string_to_Xen_string(axis_numbers_font(ss)));
}


static Xen g_listener_font(void) {return(C_string_to_Xen_string(listener_font(ss)));}

static Xen g_set_listener_font(Xen val) 
{
  #define H_listener_font "(" S_listener_font "): font used by the lisp listener"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_listener_font, "a string");
  set_listener_font(Xen_string_to_C_string(val)); 
  return(C_string_to_Xen_string(listener_font(ss)));
}


static Xen g_bold_peaks_font(void) {return(C_string_to_Xen_string(bold_peaks_font(ss)));}

static Xen g_set_bold_peaks_font(Xen val) 
{
  #define H_bold_peaks_font "(" S_bold_peaks_font "): bold font used by fft peak display"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_bold_peaks_font, "a string"); 
  set_bold_peaks_font(Xen_string_to_C_string(val)); 
  return(C_string_to_Xen_string(bold_peaks_font(ss)));
}


static Xen g_peaks_font(void) {return(C_string_to_Xen_string(peaks_font(ss)));}

static Xen g_set_peaks_font(Xen val) 
{
  #define H_peaks_font "(" S_peaks_font "): normal font used by fft peak display"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_peaks_font, "a string"); 
  set_peaks_font(Xen_string_to_C_string(val)); 
  return(C_string_to_Xen_string(peaks_font(ss)));
}


static Xen g_auto_resize(void) {return(C_bool_to_Xen_boolean(auto_resize(ss)));}

static Xen g_set_auto_resize(Xen val) 
{
  #define H_auto_resize "(" S_auto_resize "): " PROC_TRUE " if Snd can change its main window size as it pleases (default: " PROC_TRUE ")"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_auto_resize, "a boolean");
  set_auto_resize(Xen_boolean_to_C_bool(val)); 
#if USE_MOTIF
  XtVaSetValues(main_shell(ss), XmNallowShellResize, auto_resize(ss), NULL);
#endif
  return(C_bool_to_Xen_boolean(auto_resize(ss)));
}


static Xen g_color_cutoff(void) {return(C_double_to_Xen_real(color_cutoff(ss)));}

static Xen g_set_color_cutoff(Xen val) 
{
  #define H_color_cutoff "(" S_color_cutoff "): color map cutoff point (default .003).  Any values \
below the cutoff are displayed in the background color"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_color_cutoff, "a number");
  set_color_cutoff(mus_fclamp(0.0,
			      Xen_real_to_C_double(val),
#if USE_MOTIF
			      0.25
#else
			      1.0
#endif
			      )); 
  return(C_double_to_Xen_real(color_cutoff(ss)));
}


static Xen g_color_inverted(void) {return(C_bool_to_Xen_boolean(color_inverted(ss)));}

static Xen g_set_color_inverted(Xen val) 
{
  #define H_color_inverted "(" S_color_inverted "): whether the colormap in operation should be inverted"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_color_inverted, "a boolean");
  set_color_inverted(Xen_boolean_to_C_bool(val)); 
  return(C_bool_to_Xen_boolean(color_inverted(ss)));
}


static Xen g_color_scale(void) {return(C_double_to_Xen_real(color_scale(ss)));}

static Xen g_set_color_scale(Xen val) 
{
  #define H_color_scale "(" S_color_scale "): darkness setting for colormaps (0.5)"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_color_scale, "a number"); 
  set_color_scale(mus_fclamp(0.0,
			     Xen_real_to_C_double(val),
			     1000.0)); 
  return(C_double_to_Xen_real(color_scale(ss)));
}


static Xen g_selection_creates_region(void) {return(C_bool_to_Xen_boolean(selection_creates_region(ss)));}

static Xen g_set_selection_creates_region(Xen val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region "): " PROC_TRUE " if a region should be created each time a selection is made. \
The default is currently " PROC_TRUE ", but that may change.  If you're dealing with large selections, and have no need of \
regions (saved selections), you can speed up many operations by setting this flag to " PROC_FALSE
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_selection_creates_region, "a boolean");
  set_selection_creates_region(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(selection_creates_region(ss)));
}


static void set_print_lengths(int len)
{
  set_print_length(len);
  mus_vct_set_print_length(len);
#if HAVE_SCHEME
  s7_set_print_length(s7, len);
#endif
}

static Xen g_print_length(void) {return(C_int_to_Xen_integer(print_length(ss)));}

static Xen g_set_print_length(Xen val) 
{
  int len;
  #define H_print_length "(" S_print_length "): number of vector elements to print in the listener (default: 12)"
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_print_length, "an integer"); 
  len = Xen_integer_to_C_int(val);
  if (len < 0)
    Xen_out_of_range_error(S_set S_print_length, 1, val, "must be >= 0");
  set_print_lengths(len);
  return(C_int_to_Xen_integer(print_length(ss)));
}


static Xen g_show_indices(void) {return(C_bool_to_Xen_boolean(show_indices(ss)));}

static Xen g_set_show_indices(Xen val) 
{
  #define H_show_indices "(" S_show_indices "): " PROC_TRUE " if sound name should be preceded by its index in the sound display."
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_show_indices, "a boolean");
  set_show_indices(Xen_boolean_to_C_bool(val));
  for_each_sound(update_sound_label);
  return(C_bool_to_Xen_boolean(show_indices(ss)));
}

static Xen g_with_relative_panes(void) {return(C_bool_to_Xen_boolean(with_relative_panes(ss)));}

static Xen g_set_with_relative_panes(Xen val) 
{
  #define H_with_relative_panes "(" S_with_relative_panes "): " PROC_TRUE " if multichannel sounds should try to maintain relative pane sizes"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_with_relative_panes, "a boolean");
  set_with_relative_panes(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(with_relative_panes(ss)));
}


static Xen g_with_background_processes(void) {return(C_bool_to_Xen_boolean(with_background_processes(ss)));}

static Xen g_set_with_background_processes(Xen val) 
{
  #define H_with_background_processes "(" S_with_background_processes "): " PROC_TRUE " if Snd should use background (idle time) processing"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_with_background_processes, "a boolean");
  set_with_background_processes(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(with_background_processes(ss)));
}


static Xen g_with_file_monitor(void) {return(C_bool_to_Xen_boolean(with_file_monitor(ss)));}

static Xen g_set_with_file_monitor(Xen val) 
{
  #define H_with_file_monitor "(" S_with_file_monitor "): " PROC_TRUE " if the file alteration monitor is active"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_with_file_monitor, "a boolean");
  set_with_file_monitor(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(with_file_monitor(ss)));
}


static Xen g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version "): current Snd version (a string)"
  return(C_string_to_Xen_string("Snd " SND_VERSION ", " SND_DATE)); /* make it look like s7-version's output */
}


static Xen g_color_orientation_dialog(Xen managed) 
{
  #define H_color_orientation_dialog "(" S_color_orientation_dialog " :optional managed): start the Color/Orientation dialog"
  Xen_check_type(Xen_is_boolean_or_unbound(managed), managed, 1, S_color_orientation_dialog, "a boolean");
  return(Xen_wrap_widget(make_color_orientation_dialog(Xen_boolean_to_C_bool(managed))));
}


static Xen g_transform_dialog(Xen managed) 
{
  #define H_transform_dialog "(" S_transform_dialog " :optional (managed " PROC_TRUE ")): start the Transforms dialog"
  Xen_check_type(Xen_is_boolean_or_unbound(managed), managed, 1, S_transform_dialog, "a boolean");
  return(Xen_wrap_widget(make_transform_dialog(Xen_boolean_to_C_bool(managed))));
}


static Xen g_print_dialog(Xen managed, Xen direct_to_printer) 
{
  #define H_print_dialog "(" S_print_dialog " :optional managed direct): start the File Print dialog"
  Xen_check_type(Xen_is_boolean_or_unbound(managed), managed, 1, S_print_dialog, "a boolean");
  Xen_check_type(Xen_is_boolean_or_unbound(direct_to_printer), direct_to_printer, 2, S_print_dialog, "a boolean");
  return(Xen_wrap_widget(make_file_print_dialog(!(Xen_is_false(managed)), Xen_is_true(direct_to_printer))));
}


static Xen g_preferences_dialog(void)
{
  #define H_preferences_dialog "(" S_preferences_dialog "): start the Options:Preferences dialog"
  return(Xen_wrap_widget(make_preferences_dialog()));
}


static Xen g_abortt(void) /* glib now exports g_abort (8-Sep-16) -- perhaps use some other prefix? (g=guile originally) */
{
  #define H_abort "(" S_abort "): exit Snd via \"abort\", presumably to land in the debugger"
  abort();
  return(Xen_false);
}


#if (!HAVE_SCHEME)
static Xen g_abortq(void)
{
  #define H_abortQ "(" S_c_g "): allow pending user interface events to occur, returning " PROC_TRUE " if C-g was typed"
  check_for_event();
  if ((ss->C_g_typed) || 
      (ss->stopped_explicitly))
    {
      ss->stopped_explicitly = false;
      ss->C_g_typed = false;
      return(Xen_true);
    }
  return(Xen_false);
}
#endif


Xen_wrap_no_args(g_save_state_file_w, g_save_state_file)
Xen_wrap_1_arg(g_set_save_state_file_w, g_set_save_state_file)
Xen_wrap_no_args(g_save_dir_w, g_save_dir)
Xen_wrap_1_arg(g_set_save_dir_w, g_set_save_dir)
Xen_wrap_no_args(g_open_file_dialog_directory_w, g_open_file_dialog_directory)
Xen_wrap_1_arg(g_set_open_file_dialog_directory_w, g_set_open_file_dialog_directory)
Xen_wrap_no_args(g_peak_env_dir_w, g_peak_env_dir)
Xen_wrap_1_arg(g_set_peak_env_dir_w, g_set_peak_env_dir)
Xen_wrap_no_args(g_temp_dir_w, g_temp_dir)
Xen_wrap_1_arg(g_set_temp_dir_w, g_set_temp_dir)
Xen_wrap_no_args(g_ladspa_dir_w, g_ladspa_dir)
Xen_wrap_1_arg(g_set_ladspa_dir_w, g_set_ladspa_dir)
Xen_wrap_1_optional_arg(g_save_state_w, g_save_state)
Xen_wrap_1_optional_arg(g_exit_w, g_exit)
Xen_wrap_no_args(g_script_arg_w, g_script_arg)
Xen_wrap_1_arg(g_set_script_arg_w, g_set_script_arg)
Xen_wrap_no_args(g_script_args_w, g_script_args)
Xen_wrap_no_args(g_window_x_w, g_window_x)
Xen_wrap_1_arg(g_set_window_x_w, g_set_window_x)
Xen_wrap_no_args(g_window_y_w, g_window_y)
Xen_wrap_1_arg(g_set_window_y_w, g_set_window_y)
Xen_wrap_no_args(g_window_width_w, g_window_width)
Xen_wrap_1_arg(g_set_window_width_w, g_set_window_width)
Xen_wrap_no_args(g_window_height_w, g_window_height)
Xen_wrap_1_arg(g_set_window_height_w, g_set_window_height)
Xen_wrap_no_args(g_just_sounds_w, g_just_sounds)
Xen_wrap_1_arg(g_set_just_sounds_w, g_set_just_sounds)
Xen_wrap_no_args(g_play_arrow_size_w, g_play_arrow_size)
Xen_wrap_1_arg(g_set_play_arrow_size_w, g_set_play_arrow_size)
Xen_wrap_no_args(g_with_inset_graph_w, g_with_inset_graph)
Xen_wrap_1_arg(g_set_with_inset_graph_w, g_set_with_inset_graph)
Xen_wrap_no_args(g_with_interrupts_w, g_with_interrupts)
Xen_wrap_1_arg(g_set_with_interrupts_w, g_set_with_interrupts)
Xen_wrap_no_args(g_with_smpte_label_w, g_with_smpte_label)
Xen_wrap_1_arg(g_set_with_smpte_label_w, g_set_with_smpte_label)
Xen_wrap_no_args(g_with_pointer_focus_w, g_with_pointer_focus)
Xen_wrap_1_arg(g_set_with_pointer_focus_w, g_set_with_pointer_focus)
Xen_wrap_no_args(g_auto_resize_w, g_auto_resize)
Xen_wrap_1_arg(g_set_auto_resize_w, g_set_auto_resize)
Xen_wrap_no_args(g_color_cutoff_w, g_color_cutoff)
Xen_wrap_1_arg(g_set_color_cutoff_w, g_set_color_cutoff)
Xen_wrap_no_args(g_color_inverted_w, g_color_inverted)
Xen_wrap_1_arg(g_set_color_inverted_w, g_set_color_inverted)
Xen_wrap_no_args(g_color_scale_w, g_color_scale)
Xen_wrap_1_arg(g_set_color_scale_w, g_set_color_scale)
Xen_wrap_no_args(g_selection_creates_region_w, g_selection_creates_region)
Xen_wrap_1_arg(g_set_selection_creates_region_w, g_set_selection_creates_region)
Xen_wrap_no_args(g_print_length_w, g_print_length)
Xen_wrap_1_arg(g_set_print_length_w, g_set_print_length)
Xen_wrap_no_args(g_show_indices_w, g_show_indices)
Xen_wrap_1_arg(g_set_show_indices_w, g_set_show_indices)
Xen_wrap_no_args(g_with_relative_panes_w, g_with_relative_panes)
Xen_wrap_1_arg(g_set_with_relative_panes_w, g_set_with_relative_panes)
Xen_wrap_no_args(g_with_background_processes_w, g_with_background_processes)
Xen_wrap_1_arg(g_set_with_background_processes_w, g_set_with_background_processes)
Xen_wrap_no_args(g_with_file_monitor_w, g_with_file_monitor)
Xen_wrap_1_arg(g_set_with_file_monitor_w, g_set_with_file_monitor)
Xen_wrap_no_args(g_tiny_font_w, g_tiny_font)
Xen_wrap_1_arg(g_set_tiny_font_w, g_set_tiny_font)
Xen_wrap_no_args(g_peaks_font_w, g_peaks_font)
Xen_wrap_1_arg(g_set_peaks_font_w, g_set_peaks_font)
Xen_wrap_no_args(g_bold_peaks_font_w, g_bold_peaks_font)
Xen_wrap_1_arg(g_set_bold_peaks_font_w, g_set_bold_peaks_font)
Xen_wrap_no_args(g_axis_label_font_w, g_axis_label_font)
Xen_wrap_1_arg(g_set_axis_label_font_w, g_set_axis_label_font)
Xen_wrap_no_args(g_axis_numbers_font_w, g_axis_numbers_font)
Xen_wrap_1_arg(g_set_axis_numbers_font_w, g_set_axis_numbers_font)
Xen_wrap_no_args(g_listener_font_w, g_listener_font)
Xen_wrap_1_arg(g_set_listener_font_w, g_set_listener_font)
Xen_wrap_no_args(g_snd_version_w, g_snd_version)
Xen_wrap_1_optional_arg(g_color_orientation_dialog_w, g_color_orientation_dialog)
Xen_wrap_1_optional_arg(g_transform_dialog_w, g_transform_dialog)
Xen_wrap_2_optional_args(g_print_dialog_w, g_print_dialog)
Xen_wrap_no_args(g_preferences_dialog_w, g_preferences_dialog)
Xen_wrap_no_args(g_abort_w, g_abortt)
#if (!HAVE_SCHEME)
Xen_wrap_no_args(g_abortq_w, g_abortq)
#endif

#if HAVE_SCHEME
static s7_pointer acc_temp_dir(s7_scheme *sc, s7_pointer args) {return(g_set_temp_dir(s7_cadr(args)));}
static s7_pointer acc_save_dir(s7_scheme *sc, s7_pointer args) {return(g_set_save_dir(s7_cadr(args)));}
static s7_pointer acc_ladspa_dir(s7_scheme *sc, s7_pointer args) {return(g_set_ladspa_dir(s7_cadr(args)));}
static s7_pointer acc_peak_env_dir(s7_scheme *sc, s7_pointer args) {return(g_set_peak_env_dir(s7_cadr(args)));}
static s7_pointer acc_listener_font(s7_scheme *sc, s7_pointer args) {return(g_set_listener_font(s7_cadr(args)));}
static s7_pointer acc_axis_label_font(s7_scheme *sc, s7_pointer args) {return(g_set_axis_label_font(s7_cadr(args)));}
static s7_pointer acc_axis_numbers_font(s7_scheme *sc, s7_pointer args) {return(g_set_axis_numbers_font(s7_cadr(args)));}
static s7_pointer acc_tiny_font(s7_scheme *sc, s7_pointer args) {return(g_set_tiny_font(s7_cadr(args)));}
static s7_pointer acc_peaks_font(s7_scheme *sc, s7_pointer args) {return(g_set_peaks_font(s7_cadr(args)));}
static s7_pointer acc_bold_peaks_font(s7_scheme *sc, s7_pointer args) {return(g_set_bold_peaks_font(s7_cadr(args)));}
static s7_pointer acc_with_inset_graph(s7_scheme *sc, s7_pointer args) {return(g_set_with_inset_graph(s7_cadr(args)));}
static s7_pointer acc_with_pointer_focus(s7_scheme *sc, s7_pointer args) {return(g_set_with_pointer_focus(s7_cadr(args)));}
static s7_pointer acc_with_smpte_label(s7_scheme *sc, s7_pointer args) {return(g_set_with_smpte_label(s7_cadr(args)));}
static s7_pointer acc_with_interrupts(s7_scheme *sc, s7_pointer args) {return(g_set_with_interrupts(s7_cadr(args)));}
static s7_pointer acc_color_scale(s7_scheme *sc, s7_pointer args) {return(g_set_color_scale(s7_cadr(args)));}
static s7_pointer acc_color_cutoff(s7_scheme *sc, s7_pointer args) {return(g_set_color_cutoff(s7_cadr(args)));}
static s7_pointer acc_color_inverted(s7_scheme *sc, s7_pointer args) {return(g_set_color_inverted(s7_cadr(args)));}
static s7_pointer acc_auto_resize(s7_scheme *sc, s7_pointer args) {return(g_set_auto_resize(s7_cadr(args)));}
static s7_pointer acc_print_length(s7_scheme *sc, s7_pointer args) {return(g_set_print_length(s7_cadr(args)));}
static s7_pointer acc_selection_creates_region(s7_scheme *sc, s7_pointer args) {return(g_set_selection_creates_region(s7_cadr(args)));}
static s7_pointer acc_save_state_file(s7_scheme *sc, s7_pointer args) {return(g_set_save_state_file(s7_cadr(args)));}
static s7_pointer acc_with_background_processes(s7_scheme *sc, s7_pointer args) {return(g_set_with_background_processes(s7_cadr(args)));}
static s7_pointer acc_with_file_monitor(s7_scheme *sc, s7_pointer args) {return(g_set_with_file_monitor(s7_cadr(args)));}
static s7_pointer acc_show_indices(s7_scheme *sc, s7_pointer args) {return(g_set_show_indices(s7_cadr(args)));}
static s7_pointer acc_just_sounds(s7_scheme *sc, s7_pointer args) {return(g_set_just_sounds(s7_cadr(args)));}
static s7_pointer acc_play_arrow_size(s7_scheme *sc, s7_pointer args) {return(g_set_play_arrow_size(s7_cadr(args)));}
static s7_pointer acc_with_relative_panes(s7_scheme *sc, s7_pointer args) {return(g_set_with_relative_panes(s7_cadr(args)));}
static s7_pointer acc_open_file_dialog_directory(s7_scheme *sc, s7_pointer args) {return(g_set_open_file_dialog_directory(s7_cadr(args)));}
#endif


void g_init_main(void)
{
#if HAVE_SCHEME
  s7_pointer pl_b, pl_bb, pl_i, pl_ii, pl_bi, pl_s, pl_ss, pl_d, pl_dr, pl_p, pl_z, pl_zb, pl_zbb;
  {
    s7_pointer i, b, d, r, s, p, z;
    i = s7_make_symbol(s7, "integer?");
    b = s7_make_symbol(s7, "boolean?");
    d = s7_make_symbol(s7, "float?");
    r = s7_make_symbol(s7, "real?");
    s = s7_make_symbol(s7, "string?");
    p = s7_make_symbol(s7, "pair?");

    z = s7_make_signature(s7, 2, p, b);
    pl_b = s7_make_signature(s7, 1, b);
    pl_bb = s7_make_signature(s7, 2, b, b);
    pl_i = s7_make_signature(s7, 1, i);
    pl_ii = s7_make_signature(s7, 2, i, i);
    pl_bi = s7_make_signature(s7, 2, b, i);
    pl_s = s7_make_signature(s7, 1, s);
    pl_ss = s7_make_signature(s7, 2, s, s);
    pl_d = s7_make_signature(s7, 1, d);
    pl_dr = s7_make_signature(s7, 2, d, r);
    pl_p = s7_make_signature(s7, 1, p);
    pl_z = s7_make_signature(s7, 1, z);
    pl_zb = s7_make_signature(s7, 2, z, b);
    pl_zbb = s7_make_signature(s7, 3, z, b, b);
  }
#endif
  Xen_define_typed_procedure(S_save_state,   g_save_state_w,   0, 1, 0, H_save_state, pl_ss);
#if HAVE_FORTH			/* exit is an existing word */
  Xen_define_typed_procedure("snd-" S_exit,  g_exit_w,         0, 1, 0, H_exit, pl_bi);
#else
  Xen_define_typed_procedure(S_exit,         g_exit_w,         0, 1, 0, H_exit, pl_bi);
#endif
  
  Xen_define_typed_dilambda(S_save_state_file, g_save_state_file_w, H_save_state_file, 
			    S_set S_save_state_file, g_set_save_state_file_w, 0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_save_dir, g_save_dir_w, H_save_dir,
			    S_set S_save_dir, g_set_save_dir_w,  0, 0, 1, 0, pl_s, pl_ss);
  
  Xen_define_typed_dilambda(S_open_file_dialog_directory, g_open_file_dialog_directory_w, H_open_file_dialog_directory,
			    S_set S_open_file_dialog_directory, g_set_open_file_dialog_directory_w,  0, 0, 1, 0, pl_s, pl_ss);
  
  Xen_define_typed_dilambda(S_peak_env_dir, g_peak_env_dir_w, H_peak_env_dir, 
			    S_set S_peak_env_dir, g_set_peak_env_dir_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_temp_dir, g_temp_dir_w, H_temp_dir, 
			    S_set S_temp_dir, g_set_temp_dir_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_ladspa_dir, g_ladspa_dir_w, H_ladspa_dir, 
			    S_set S_ladspa_dir, g_set_ladspa_dir_w,  0, 0, 1, 0, pl_s, pl_ss);
  
  #define H_before_exit_hook S_before_exit_hook " (): called upon exit. If it returns " PROC_TRUE ", Snd does not exit."
  
  before_exit_hook = Xen_define_hook(S_before_exit_hook, "(make-hook)", 0, H_before_exit_hook);
  
  #define H_exit_hook S_exit_hook " (): called upon exit.  This can be used to perform cleanup activities."
  
  exit_hook = Xen_define_hook(S_exit_hook, "(make-hook)", 0, H_exit_hook);
  
  #define H_after_save_state_hook S_after_save_state_hook " (name): called after Snd state has been saved; filename is the save state file."
  after_save_state_hook = Xen_define_hook(S_after_save_state_hook, "(make-hook 'name)", 1, H_after_save_state_hook);
  
  #define H_before_save_state_hook S_before_save_state_hook " (name): called before Snd state is saved. If \
the hook functions return " PROC_TRUE ", the save state process opens the file 'name' for appending, rather than truncating."
  before_save_state_hook = Xen_define_hook(S_before_save_state_hook, "(make-hook 'name)", 1, H_before_save_state_hook);
  
  Xen_define_typed_dilambda(S_script_arg, g_script_arg_w, H_script_arg, 
			    S_set S_script_arg, g_set_script_arg_w,  0, 0, 1, 0, pl_i, pl_ii);
  Xen_define_typed_procedure(S_script_args, g_script_args_w, 0, 0, 0, H_script_args, pl_p);
  
  Xen_define_typed_dilambda(S_window_x, g_window_x_w, H_window_x, 
			    S_set S_window_x, g_set_window_x_w,  0, 0, 1, 0, pl_i, pl_ii);
  Xen_define_typed_dilambda(S_window_y, g_window_y_w, H_window_y, 
			    S_set S_window_y, g_set_window_y_w,  0, 0, 1, 0, pl_i, pl_ii);
  Xen_define_typed_dilambda(S_window_width, g_window_width_w, H_window_width, 
			    S_set S_window_width, g_set_window_width_w,  0, 0, 1, 0, pl_i, pl_ii);  
  Xen_define_typed_dilambda(S_window_height, g_window_height_w, H_window_height, 
			    S_set S_window_height, g_set_window_height_w,  0, 0, 1, 0, pl_i, pl_ii);
  
  Xen_define_typed_dilambda(S_auto_resize, g_auto_resize_w, H_auto_resize, 
			    S_set S_auto_resize, g_set_auto_resize_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_color_cutoff, g_color_cutoff_w, H_color_cutoff, 
			    S_set S_color_cutoff, g_set_color_cutoff_w,  0, 0, 1, 0, pl_d, pl_dr);
  Xen_define_typed_dilambda(S_color_inverted, g_color_inverted_w, H_color_inverted, 
			    S_set S_color_inverted, g_set_color_inverted_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_color_scale, g_color_scale_w, H_color_scale, 
			    S_set S_color_scale, g_set_color_scale_w,  0, 0, 1, 0, pl_d, pl_dr);
  
  Xen_define_typed_dilambda(S_selection_creates_region, g_selection_creates_region_w, H_selection_creates_region,
			    S_set S_selection_creates_region, g_set_selection_creates_region_w,  0, 0, 1, 0, pl_b, pl_bb);
  
  Xen_define_typed_dilambda(S_print_length, g_print_length_w, H_print_length, 
			    S_set S_print_length, g_set_print_length_w,  0, 0, 1, 0, pl_i, pl_ii);
  Xen_define_typed_dilambda(S_show_indices, g_show_indices_w, H_show_indices, 
			    S_set S_show_indices, g_set_show_indices_w,  0, 0, 1, 0, pl_b, pl_bb);
  
  Xen_define_typed_dilambda(S_with_relative_panes, g_with_relative_panes_w, H_with_relative_panes,
			    S_set S_with_relative_panes, g_set_with_relative_panes_w,  0, 0, 1, 0, pl_b, pl_bb);
  
  Xen_define_typed_dilambda(S_with_background_processes, g_with_background_processes_w, H_with_background_processes,
			    S_set S_with_background_processes, g_set_with_background_processes_w,  0, 0, 1, 0, pl_b, pl_bb);
  
  Xen_define_typed_dilambda(S_with_file_monitor, g_with_file_monitor_w, H_with_file_monitor, 
			    S_set S_with_file_monitor, g_set_with_file_monitor_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_tiny_font, g_tiny_font_w, H_tiny_font, 
			    S_set S_tiny_font, g_set_tiny_font_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_peaks_font, g_peaks_font_w, H_peaks_font, 
			    S_set S_peaks_font, g_set_peaks_font_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_bold_peaks_font, g_bold_peaks_font_w, H_bold_peaks_font, 
			    S_set S_bold_peaks_font, g_set_bold_peaks_font_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_axis_label_font, g_axis_label_font_w, H_axis_label_font, 
			    S_set S_axis_label_font, g_set_axis_label_font_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_axis_numbers_font, g_axis_numbers_font_w, H_axis_numbers_font, 
			    S_set S_axis_numbers_font, g_set_axis_numbers_font_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_listener_font, g_listener_font_w, H_listener_font, 
			    S_set S_listener_font, g_set_listener_font_w,  0, 0, 1, 0, pl_s, pl_ss);
  Xen_define_typed_dilambda(S_just_sounds, g_just_sounds_w, H_just_sounds, 
			    S_set S_just_sounds, g_set_just_sounds_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_play_arrow_size, g_play_arrow_size_w, H_play_arrow_size, 
			    S_set S_play_arrow_size, g_set_play_arrow_size_w,  0, 0, 1, 0, pl_i, pl_ii);
  Xen_define_typed_dilambda(S_with_inset_graph, g_with_inset_graph_w, H_with_inset_graph, 
			    S_set S_with_inset_graph, g_set_with_inset_graph_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_with_interrupts, g_with_interrupts_w, H_with_interrupts, 
			    S_set S_with_interrupts, g_set_with_interrupts_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_with_smpte_label, g_with_smpte_label_w, H_with_smpte_label, 
			    S_set S_with_smpte_label, g_set_with_smpte_label_w,  0, 0, 1, 0, pl_b, pl_bb);
  Xen_define_typed_dilambda(S_with_pointer_focus, g_with_pointer_focus_w, H_with_pointer_focus, 
			    S_set S_with_pointer_focus, g_set_with_pointer_focus_w,  0, 0, 1, 0, pl_b, pl_bb);
  
  Xen_define_typed_procedure(S_snd_version,		g_snd_version_w,              0, 0, 0, H_snd_version,		   pl_s);
  Xen_define_typed_procedure(S_color_orientation_dialog,g_color_orientation_dialog_w, 0, 1, 0, H_color_orientation_dialog, pl_zb);
  Xen_define_typed_procedure(S_transform_dialog,        g_transform_dialog_w,         0, 1, 0, H_transform_dialog,	   pl_zb);
  Xen_define_typed_procedure(S_print_dialog,            g_print_dialog_w,             0, 2, 0, H_print_dialog,		   pl_zbb);
  Xen_define_typed_procedure(S_preferences_dialog,      g_preferences_dialog_w,       0, 0, 0, H_preferences_dialog,	   pl_z);
  Xen_define_typed_procedure(S_abort,			g_abort_w,                    0, 0, 0, H_abort,                    pl_b);
#if (!HAVE_SCHEME)
  Xen_define_typed_procedure(S_c_g,			g_abortq_w,                   0, 0, 0, H_abortQ,                   pl_b);
#endif
  
#if HAVE_SCHEME
  s7_symbol_set_documentation(s7, ss->temp_dir_symbol, "*temp-dir*: name of directory for temp files (or #f=null)"); 
  s7_symbol_set_documentation(s7, ss->save_dir_symbol, "*save-dir*: name of directory for saved state data (or #f=null)");
  s7_symbol_set_documentation(s7, ss->ladspa_dir_symbol, "*ladspa-dir*: name of directory for ladspa plugin libraries");
  s7_symbol_set_documentation(s7, ss->peak_env_dir_symbol, "*peak-env-dir*: name of directory for peak env files (or #f=null)");
  s7_symbol_set_documentation(s7, ss->listener_font_symbol, "*listener-font*: font used by the lisp listener");
  s7_symbol_set_documentation(s7, ss->axis_label_font_symbol, "*axis-label-font*: font used for axis labels");
  s7_symbol_set_documentation(s7, ss->axis_numbers_font_symbol, "*axis-numbers-font*: font used for axis numbers");
  s7_symbol_set_documentation(s7, ss->tiny_font_symbol, "*tiny-font*: font use for some info in the graphs");
  s7_symbol_set_documentation(s7, ss->peaks_font_symbol, "*peaks-font*: normal font used by fft peak display");
  s7_symbol_set_documentation(s7, ss->bold_peaks_font_symbol, "*bold-peaks-font*: bold font used by fft peak display");
  s7_symbol_set_documentation(s7, ss->with_inset_graph_symbol, "*with-inset-graph*: if #t, display the inset graph in the time domain section.");
  s7_symbol_set_documentation(s7, ss->with_pointer_focus_symbol, "*with-pointer-focus*: if #t, activate the text or graph widget beneath the mouse.");
  s7_symbol_set_documentation(s7, ss->with_smpte_label_symbol, "*with-smpte-label*: if #t, display the SMPTE data in the time domain section.");
  s7_symbol_set_documentation(s7, ss->with_interrupts_symbol, "*with-interrupts*: if #t, check for GUI events during computations.");
  s7_symbol_set_documentation(s7, ss->color_scale_symbol, "*color-scale*: darkness setting for colormaps (0.5)");
  s7_symbol_set_documentation(s7, ss->color_cutoff_symbol, "*color-cutoff*: color map cutoff point (default .003).");
  s7_symbol_set_documentation(s7, ss->color_inverted_symbol, "*color-inverted*: whether the colormap in operation should be inverted");
  s7_symbol_set_documentation(s7, ss->auto_resize_symbol, "*auto-resize*: #t if Snd can change its main window size as it pleases");
  s7_symbol_set_documentation(s7, ss->print_length_symbol, "*print-length*: number of vector elements to print in the listener (12)");
  s7_symbol_set_documentation(s7, ss->selection_creates_region_symbol, "*selection-creates-region*: #t if a region should be created each time a selection is made.");
  s7_symbol_set_documentation(s7, ss->save_state_file_symbol, "*save-state-file*: the name of the saved state file (\"saved-snd.scm\")");
  s7_symbol_set_documentation(s7, ss->with_background_processes_symbol, "*with-background-processes*: #t if Snd should use background (idle time) processing");
  s7_symbol_set_documentation(s7, ss->with_file_monitor_symbol, "*with-file-monitor*: #t if the file alteration monitor is active");
  s7_symbol_set_documentation(s7, ss->show_indices_symbol, "*show-indices*: #t if sound name should be preceded by its index in the sound display.");
  s7_symbol_set_documentation(s7, ss->just_sounds_symbol, "*just-sounds*: the 'just sounds' choice in the file chooser dialog");
  s7_symbol_set_documentation(s7, ss->play_arrow_size_symbol, "*play-arrow-size*: the size of the play triangles");
  s7_symbol_set_documentation(s7, ss->with_relative_panes_symbol, "*with-relative-panes*: #t if multichannel sounds should try to maintain relative pane sizes");
  s7_symbol_set_documentation(s7, ss->open_file_dialog_directory_symbol, "*open-file-dialog-directory*: name of directory for initial open file dialog search");

  s7_symbol_set_setter(s7, ss->temp_dir_symbol, s7_make_function(s7, "[acc-" S_temp_dir "]", acc_temp_dir, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->save_dir_symbol, s7_make_function(s7, "[acc-" S_save_dir "]", acc_save_dir, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->ladspa_dir_symbol, s7_make_function(s7, "[acc-" S_ladspa_dir "]", acc_ladspa_dir, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->peak_env_dir_symbol, s7_make_function(s7, "[acc-" S_peak_env_dir "]", acc_peak_env_dir, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->listener_font_symbol, s7_make_function(s7, "[acc-" S_listener_font "]", acc_listener_font, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->axis_label_font_symbol, s7_make_function(s7, "[acc-" S_axis_label_font "]", acc_axis_label_font, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->axis_numbers_font_symbol, s7_make_function(s7, "[acc-" S_axis_numbers_font "]", acc_axis_numbers_font, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->tiny_font_symbol, s7_make_function(s7, "[acc-" S_tiny_font "]", acc_tiny_font, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->peaks_font_symbol, s7_make_function(s7, "[acc-" S_peaks_font "]", acc_peaks_font, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->bold_peaks_font_symbol, s7_make_function(s7, "[acc-" S_bold_peaks_font "]", acc_bold_peaks_font, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_inset_graph_symbol, s7_make_function(s7, "[acc-" S_with_inset_graph "]", acc_with_inset_graph, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_pointer_focus_symbol, s7_make_function(s7, "[acc-" S_with_pointer_focus "]", acc_with_pointer_focus, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_smpte_label_symbol, s7_make_function(s7, "[acc-" S_with_smpte_label "]", acc_with_smpte_label, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_interrupts_symbol, s7_make_function(s7, "[acc-" S_with_interrupts "]", acc_with_interrupts, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->color_scale_symbol, s7_make_function(s7, "[acc-" S_color_scale "]", acc_color_scale, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->color_cutoff_symbol, s7_make_function(s7, "[acc-" S_color_cutoff "]", acc_color_cutoff, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->color_inverted_symbol, s7_make_function(s7, "[acc-" S_color_inverted "]", acc_color_inverted, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->auto_resize_symbol, s7_make_function(s7, "[acc-" S_auto_resize "]", acc_auto_resize, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->print_length_symbol, s7_make_function(s7, "[acc-" S_print_length "]", acc_print_length, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->selection_creates_region_symbol, s7_make_function(s7, "[acc-" S_selection_creates_region "]", acc_selection_creates_region, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->save_state_file_symbol, s7_make_function(s7, "[acc-" S_save_state_file "]", acc_save_state_file, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_background_processes_symbol, s7_make_function(s7, "[acc-" S_with_background_processes "]", acc_with_background_processes, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_file_monitor_symbol, s7_make_function(s7, "[acc-" S_with_file_monitor "]", acc_with_file_monitor, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->show_indices_symbol, s7_make_function(s7, "[acc-" S_show_indices "]", acc_show_indices, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->just_sounds_symbol, s7_make_function(s7, "[acc-" S_just_sounds "]", acc_just_sounds, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->play_arrow_size_symbol, s7_make_function(s7, "[acc-" S_play_arrow_size "]", acc_play_arrow_size, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_relative_panes_symbol, s7_make_function(s7, "[acc-" S_with_relative_panes "]", acc_with_relative_panes, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->open_file_dialog_directory_symbol, s7_make_function(s7, "[acc-" S_open_file_dialog_directory "]", acc_open_file_dialog_directory, 2, 0, false, "accessor"));
#endif
}
