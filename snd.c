/* Sound display/edit/etc
 *
 * originally intended as a re-implementation of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 */

#include "snd.h"

snd_state *ss = NULL;

static bool ignore_mus_error(int type, char *msg)
{
  Xen result = Xen_false;

  if (Xen_hook_has_list(ss->mus_error_hook))
    result = run_or_hook(ss->mus_error_hook, 
			 Xen_list_2(C_int_to_Xen_integer(type), 
				    C_string_to_Xen_string(msg)),
			 S_mus_error_hook);
  return(Xen_is_true(result));
}

#ifndef _MSC_VER
  void top_level_catch(int ignore);
#endif


static void mus_error_to_snd(int type, char *msg)
{
  if (!ss)
    {
      fprintf(stderr, "%s", msg);
      return;
    }

  if (!(ignore_mus_error(type, msg)))
    {
#if HAVE_EXTENSION_LANGUAGE
      if (!msg)
	Xen_error(Xen_make_error_type("mus-error"),
		  Xen_list_1(C_string_to_Xen_string((char *)mus_error_type_to_string(type))));
      else Xen_error(Xen_make_error_type("mus-error"),
		     Xen_list_1(C_string_to_Xen_string(msg)));
#endif
      snd_error("%s: %s", mus_error_type_to_string(type), msg);
#ifndef _MSC_VER
      ss->jump_ok = true;
      top_level_catch(1); /* sigh -- try to keep going */
#endif
    }
}


static void mus_print_to_snd(char *msg)
{
  if (!ss)
    {
      fprintf(stderr, "%s", msg);
      return;
    }
  if (!(ignore_mus_error(MUS_NO_ERROR, msg)))
    if (msg)
      {
	int i, len;
	listener_append(";");
	len = strlen(msg);

	for (i = 1; i < len - 1; i++)
	  if ((msg[i] == '\n') && (msg[i + 1] == ' '))
	    msg[i + 1] = ';';

	if (msg[0] == '\n')
	  listener_append((char *)(msg + 1));
	else listener_append(msg);

	if (msg[strlen(msg) - 1] != '\n')
	  listener_append("\n");
      }
}


static void initialize_load_path(void)
{
  /* look for SND_PATH env var, add dirs to %load-path or load_path */
  char *path;
  path = getenv("SND_PATH");
  if (path)
    {
      /* colon-separated list of directory names, pushed on load-path in reverse order (hopefully = search order) */
      int i, len, dirs = 1, curdir = 0, start = 0;
      char **dirnames;

      len = strlen(path);
      for (i = 0; i < len; i++)
	if (path[i] == ':')
	  dirs++;

      dirnames = (char **)calloc(dirs, sizeof(char *));
      for (i = 0; i < len; i++)
	{
	  if ((path[i] == ':') ||
	      (i == len - 1))
	    {
	      if (i > start)
		{
		  int j, lim;
		  char *tmp;

		  if (i == (len - 1))
		    lim = i + 1;
		  else lim = i;

		  tmp = (char *)calloc(lim - start + 1, sizeof(char));
		  for (j = start; j < lim; j++)
		    tmp[j - start] = path[j];

		  dirnames[curdir++] = mus_expand_filename(tmp);
		  start = i + 1;
		  free(tmp);
		}
	    }
	}

      for (i = curdir - 1; i >= 0; i--)
	{
	  Xen_add_to_load_path(dirnames[i]);
	  free(dirnames[i]);
	}
      free(dirnames);
    }
}


void snd_set_global_defaults(bool need_cleanup)
{
  if (need_cleanup)
    {
      if (ss->HTML_Program) {free(ss->HTML_Program); ss->HTML_Program = NULL;}
      if (ss->HTML_Dir) {free(ss->HTML_Dir); ss->HTML_Dir = NULL;}
      if (ss->Temp_Dir) {free(ss->Temp_Dir); ss->Temp_Dir = NULL;}
      if (ss->Save_Dir) {free(ss->Save_Dir); ss->Save_Dir = NULL;}
      if (ss->Ladspa_Dir) {free(ss->Ladspa_Dir); ss->Ladspa_Dir = NULL;}
      if (ss->Save_State_File) {free(ss->Save_State_File); ss->Save_State_File = NULL;}
      if (ss->Eps_File) {free(ss->Eps_File); ss->Eps_File = NULL;}
      if (ss->Listener_Prompt) {free(ss->Listener_Prompt); ss->Listener_Prompt = NULL;}
      if (ss->Stdin_Prompt) {free(ss->Stdin_Prompt); ss->Stdin_Prompt = NULL;}
      if (ss->Open_File_Dialog_Directory) {free(ss->Open_File_Dialog_Directory); ss->Open_File_Dialog_Directory = NULL;}
      
      /* not sure about the next two... */
      if ((cursor_style(ss) == CURSOR_PROC) && (Xen_is_procedure(ss->cursor_proc)))
	snd_unprotect_at(ss->cursor_proc_loc);
      if ((zoom_focus_style(ss) == ZOOM_FOCUS_PROC) && (Xen_is_procedure(ss->zoom_focus_proc)))
	snd_unprotect_at(ss->zoom_focus_proc_loc);
    }

  ss->Transform_Size =              DEFAULT_TRANSFORM_SIZE;
  ss->Fft_Window =                  DEFAULT_FFT_WINDOW;
  ss->Fft_Window_Alpha =            DEFAULT_FFT_WINDOW_ALPHA;
  ss->Fft_Window_Beta =             DEFAULT_FFT_WINDOW_BETA;
  ss->Transform_Graph_Type =        DEFAULT_TRANSFORM_GRAPH_TYPE;
  ss->Sinc_Width =                  DEFAULT_SINC_WIDTH;
  ss->Zero_Pad =                    DEFAULT_ZERO_PAD;
  ss->Wavelet_Type =                DEFAULT_WAVELET_TYPE;
  ss->Transform_Type =              DEFAULT_TRANSFORM_TYPE;
  ss->Transform_Normalization =     DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Show_Transform_Peaks =        DEFAULT_SHOW_TRANSFORM_PEAKS;
  ss->Show_Sonogram_Cursor =        DEFAULT_SHOW_SONOGRAM_CURSOR;
  ss->Fft_Log_Magnitude =           DEFAULT_FFT_LOG_MAGNITUDE;
  ss->Fft_Log_Frequency =           DEFAULT_FFT_LOG_FREQUENCY;
  ss->Fft_With_Phases =             DEFAULT_FFT_WITH_PHASES;
  ss->Max_Transform_Peaks =         DEFAULT_MAX_TRANSFORM_PEAKS;
  ss->Log_Freq_Start =              DEFAULT_LOG_FREQ_START;
  ss->Min_dB =                      DEFAULT_MIN_DB;
  ss->lin_dB =                      pow(10.0, DEFAULT_MIN_DB * 0.05);
  ss->Show_Selection_Transform =    DEFAULT_SHOW_SELECTION_TRANSFORM;
  ss->Default_Output_Chans =        DEFAULT_OUTPUT_CHANS;
  ss->Default_Output_Srate =        DEFAULT_OUTPUT_SRATE;
  ss->Default_Output_Header_Type =  DEFAULT_OUTPUT_HEADER_TYPE;
  ss->Default_Output_Sample_Type =  DEFAULT_OUTPUT_SAMPLE_TYPE;
  ss->Dac_Size =                    DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels =       DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Auto_Resize =                 DEFAULT_AUTO_RESIZE; 
  ss->Auto_Update =                 DEFAULT_AUTO_UPDATE; 
  ss->Auto_Update_Interval =        DEFAULT_AUTO_UPDATE_INTERVAL;
  ss->Ask_Before_Overwrite =        DEFAULT_ASK_BEFORE_OVERWRITE;
  ss->With_Toolbar =                DEFAULT_WITH_TOOLBAR;
  ss->With_Tooltips =               DEFAULT_WITH_TOOLTIPS;
  ss->Remember_Sound_State =        DEFAULT_REMEMBER_SOUND_STATE;
  ss->Ask_About_Unsaved_Edits =     DEFAULT_ASK_ABOUT_UNSAVED_EDITS;
  ss->Save_As_Dialog_Src =          DEFAULT_SAVE_AS_DIALOG_SRC;
  ss->Save_As_Dialog_Auto_Comment = DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT;
  ss->Show_Full_Duration =          DEFAULT_SHOW_FULL_DURATION;
  ss->Show_Full_Range =             DEFAULT_SHOW_FULL_RANGE;
  ss->Initial_Beg =                 DEFAULT_INITIAL_BEG;
  ss->Initial_Dur =                 DEFAULT_INITIAL_DUR;
  ss->With_Background_Processes =   DEFAULT_WITH_BACKGROUND_PROCESSES;
  ss->With_File_Monitor =           DEFAULT_WITH_FILE_MONITOR;
  ss->Selection_Creates_Region =    DEFAULT_SELECTION_CREATES_REGION;
  ss->Channel_Style =               DEFAULT_CHANNEL_STYLE;
  ss->Sound_Style =                 DEFAULT_SOUND_STYLE;
  ss->Graphs_Horizontal =           DEFAULT_GRAPHS_HORIZONTAL;
  ss->Graph_Style =                 DEFAULT_GRAPH_STYLE;
  ss->Region_Graph_Style =          DEFAULT_GRAPH_STYLE;
  ss->Time_Graph_Type =             DEFAULT_TIME_GRAPH_TYPE;
  ss->X_Axis_Style =                DEFAULT_X_AXIS_STYLE;
  ss->Beats_Per_Minute =            DEFAULT_BEATS_PER_MINUTE;
  ss->Beats_Per_Measure =           DEFAULT_BEATS_PER_MEASURE;
  ss->With_Relative_Panes =         DEFAULT_WITH_RELATIVE_PANES;
  ss->With_GL =                     DEFAULT_WITH_GL;
  ss->Dot_Size =                    DEFAULT_DOT_SIZE;
  ss->Grid_Density =                DEFAULT_GRID_DENSITY;
  ss->Zoom_Focus_Style =            DEFAULT_ZOOM_FOCUS_STYLE;
  ss->zoom_focus_proc =             Xen_undefined;
  ss->zoom_focus_proc_loc =         NOT_A_GC_LOC;
  ss->Max_Regions =                 DEFAULT_MAX_REGIONS;
  ss->Show_Y_Zero =                 DEFAULT_SHOW_Y_ZERO;
  ss->Show_Grid =                   DEFAULT_SHOW_GRID;
  ss->Show_Axes =                   DEFAULT_SHOW_AXES;
  ss->Show_Indices =                DEFAULT_SHOW_INDICES;
  ss->With_Inset_Graph =            DEFAULT_WITH_INSET_GRAPH;
  ss->With_Interrupts =             DEFAULT_WITH_INTERRUPTS;
  ss->With_Menu_Icons =             DEFAULT_WITH_MENU_ICONS;
  ss->With_Smpte_Label =            DEFAULT_WITH_SMPTE_LABEL;
  ss->With_Pointer_Focus =          DEFAULT_WITH_POINTER_FOCUS;
  ss->Play_Arrow_Size =             DEFAULT_PLAY_ARROW_SIZE;
  ss->Sync_Style =                  DEFAULT_SYNC_STYLE;
  ss->Stdin_Prompt =                mus_strdup(DEFAULT_STDIN_PROMPT);
  ss->Listener_Prompt =             mus_strdup(DEFAULT_LISTENER_PROMPT);
  ss->listener_prompt_length =      mus_strlen(ss->Listener_Prompt);
  ss->Clipping =                    DEFAULT_CLIPPING;
  ss->Print_Length =                DEFAULT_PRINT_LENGTH;
  ss->View_Files_Sort =             DEFAULT_VIEW_FILES_SORT;
  ss->Just_Sounds =                 DEFAULT_JUST_SOUNDS;
  ss->Open_File_Dialog_Directory =  NULL;
  ss->HTML_Dir =                    mus_strdup(DEFAULT_HTML_DIR);
  ss->HTML_Program =                mus_strdup(DEFAULT_HTML_PROGRAM);
  ss->Cursor_Size =                 DEFAULT_CURSOR_SIZE;
  ss->Cursor_Style =                DEFAULT_CURSOR_STYLE;
  ss->Tracking_Cursor_Style =       DEFAULT_TRACKING_CURSOR_STYLE;
  ss->With_Tracking_Cursor =        DEFAULT_WITH_TRACKING_CURSOR;
  ss->cursor_proc =                 Xen_undefined;
  ss->cursor_proc_loc =             NOT_A_GC_LOC;
  ss->With_Verbose_Cursor =         DEFAULT_WITH_VERBOSE_CURSOR;
  ss->Cursor_Update_Interval =      DEFAULT_CURSOR_UPDATE_INTERVAL;
  ss->Cursor_Location_Offset =      DEFAULT_CURSOR_LOCATION_OFFSET;
  ss->Show_Mix_Waveforms =          DEFAULT_SHOW_MIX_WAVEFORMS;
  ss->Mix_Waveform_Height =         DEFAULT_MIX_WAVEFORM_HEIGHT;
  ss->Mix_Tag_Width =               DEFAULT_MIX_TAG_WIDTH;
  ss->Mix_Tag_Height =              DEFAULT_MIX_TAG_HEIGHT;
  ss->With_Mix_Tags =               DEFAULT_WITH_MIX_TAGS;
  ss->Mark_Tag_Width =              DEFAULT_MARK_TAG_WIDTH;
  ss->Mark_Tag_Height =             DEFAULT_MARK_TAG_HEIGHT;
  ss->Show_Marks =                  DEFAULT_SHOW_MARKS;
  ss->Color_Map =                   DEFAULT_COLOR_MAP;
  ss->Color_Map_Size =              DEFAULT_COLOR_MAP_SIZE;
  ss->Color_Cutoff =                DEFAULT_COLOR_CUTOFF;
  ss->Color_Scale =                 DEFAULT_COLOR_SCALE;
  ss->Color_Inverted =              DEFAULT_COLOR_INVERTED;
  ss->Color_Map =                   DEFAULT_COLOR_MAP;
  ss->Wavo_Hop =                    DEFAULT_WAVO_HOP;
  ss->Wavo_Trace =                  DEFAULT_WAVO_TRACE;
  ss->Spectro_Hop =                 DEFAULT_SPECTRO_HOP;
  ss->Spectro_X_Scale =             DEFAULT_SPECTRO_X_SCALE;
  ss->Spectro_Y_Scale =             DEFAULT_SPECTRO_Y_SCALE;
  ss->Spectro_Z_Scale =             DEFAULT_SPECTRO_Z_SCALE;
  ss->Spectro_Z_Angle =             DEFAULT_SPECTRO_Z_ANGLE;
  ss->Spectro_X_Angle =             DEFAULT_SPECTRO_X_ANGLE;
  ss->Spectro_Y_Angle =             DEFAULT_SPECTRO_Y_ANGLE;
  ss->Spectrum_End =                DEFAULT_SPECTRUM_END;
  ss->Spectrum_Start =              DEFAULT_SPECTRUM_START;
  ss->Enved_Base =                  DEFAULT_ENVED_BASE;
  ss->Enved_Power =                 DEFAULT_ENVED_POWER;
  ss->Enved_With_Wave =             DEFAULT_ENVED_WITH_WAVE;
  ss->Enved_Style =                 DEFAULT_ENVED_STYLE;
  ss->Enved_Target =                DEFAULT_ENVED_TARGET;
  ss->Enved_Filter_Order =          DEFAULT_ENVED_FILTER_ORDER;
  ss->Eps_Bottom_Margin =           DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin =             DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size =                    DEFAULT_EPS_SIZE;
  ss->Expand_Control_Min =          DEFAULT_EXPAND_CONTROL_MIN;
  ss->Expand_Control_Max =          DEFAULT_EXPAND_CONTROL_MAX;
  ss->Amp_Control_Min =             DEFAULT_AMP_CONTROL_MIN;
  ss->Amp_Control_Max =             DEFAULT_AMP_CONTROL_MAX;
  ss->Speed_Control_Min =           DEFAULT_SPEED_CONTROL_MIN;
  ss->Speed_Control_Max =           DEFAULT_SPEED_CONTROL_MAX;
  ss->Contrast_Control_Min =        DEFAULT_CONTRAST_CONTROL_MIN;
  ss->Contrast_Control_Max =        DEFAULT_CONTRAST_CONTROL_MAX;
  ss->Contrast_Control_Amp =        DEFAULT_CONTRAST_CONTROL_AMP;
  ss->Expand_Control_Length =       DEFAULT_EXPAND_CONTROL_LENGTH;
  ss->Expand_Control_Ramp =         DEFAULT_EXPAND_CONTROL_RAMP;
  ss->Expand_Control_Hop =          DEFAULT_EXPAND_CONTROL_HOP;
  ss->Expand_Control_Jitter =       DEFAULT_EXPAND_CONTROL_JITTER;
  ss->Reverb_Control_Feedback =     DEFAULT_REVERB_CONTROL_FEEDBACK;
  ss->Reverb_Control_Lowpass =      DEFAULT_REVERB_CONTROL_LOWPASS;
  ss->Reverb_Control_Scale_Min =    DEFAULT_REVERB_CONTROL_SCALE_MIN;
  ss->Reverb_Control_Scale_Max =    DEFAULT_REVERB_CONTROL_SCALE_MAX;
  ss->Reverb_Control_Decay =        DEFAULT_REVERB_CONTROL_DECAY;
  ss->Speed_Control_Tones =         DEFAULT_SPEED_CONTROL_TONES;
  ss->Speed_Control_Style =         DEFAULT_SPEED_CONTROL_STYLE;
  ss->Reverb_Control_Length_Min =   DEFAULT_REVERB_CONTROL_LENGTH_MIN;
  ss->Reverb_Control_Length_Max =   DEFAULT_REVERB_CONTROL_LENGTH_MAX;
  ss->Filter_Control_Order =        DEFAULT_FILTER_CONTROL_ORDER;
  ss->Filter_Control_In_Db =        DEFAULT_FILTER_CONTROL_IN_DB;
  ss->Filter_Control_In_Hz =        DEFAULT_FILTER_CONTROL_IN_HZ;
  ss->Show_Controls =               DEFAULT_SHOW_CONTROLS;

  if (MUS_DEFAULT_TEMP_DIR != (char *)NULL) 
    ss->Temp_Dir = mus_strdup(MUS_DEFAULT_TEMP_DIR); 
  else ss->Temp_Dir = NULL;
  
  if (MUS_DEFAULT_SAVE_DIR != (char *)NULL) 
    ss->Save_Dir = mus_strdup(MUS_DEFAULT_SAVE_DIR); 
  else ss->Save_Dir = NULL;

  if (DEFAULT_LADSPA_DIR != (char *)NULL) 
    ss->Ladspa_Dir = mus_strdup(DEFAULT_LADSPA_DIR); 
  else ss->Ladspa_Dir = NULL;

  if (DEFAULT_SAVE_STATE_FILE != (char *)NULL) 
    ss->Save_State_File = mus_strdup(DEFAULT_SAVE_STATE_FILE); 
  else ss->Save_State_File = NULL;

  if (DEFAULT_PEAK_ENV_DIR != (char *)NULL) 
    ss->Peak_Env_Dir = mus_strdup(DEFAULT_PEAK_ENV_DIR); 
  else ss->Peak_Env_Dir = NULL;
  
  if (DEFAULT_EPS_FILE != (char *)NULL) 
    ss->Eps_File = mus_strdup(DEFAULT_EPS_FILE);
  else ss->Eps_File = NULL;

#if HAVE_SCHEME
  ss->eps_file_symbol =                s7_define_variable(s7, "*" S_eps_file "*",               s7_make_string(s7, DEFAULT_EPS_FILE));
  ss->enved_filter_order_symbol =      s7_define_variable(s7, "*" S_enved_filter_order "*",     s7_make_integer(s7, DEFAULT_ENVED_FILTER_ORDER));
  ss->eps_left_margin_symbol =         s7_define_variable(s7, "*" S_eps_left_margin "*",        s7_make_real(s7, DEFAULT_EPS_LEFT_MARGIN));
  ss->eps_bottom_margin_symbol =       s7_define_variable(s7, "*" S_eps_bottom_margin "*",      s7_make_real(s7, DEFAULT_EPS_BOTTOM_MARGIN));
  ss->eps_size_symbol =                s7_define_variable(s7, "*" S_eps_size "*",               s7_make_real(s7, DEFAULT_EPS_SIZE));
  ss->log_freq_start_symbol =          s7_define_variable(s7, "*" S_log_freq_start "*",         s7_make_real(s7, DEFAULT_LOG_FREQ_START));
  ss->color_map_symbol =               s7_define_variable(s7, "*" S_colormap "*",               s7_make_integer(s7, DEFAULT_COLOR_MAP));
  ss->color_map_size_symbol =          s7_define_variable(s7, "*" S_colormap_size "*",          s7_make_integer(s7, DEFAULT_COLOR_MAP_SIZE));
  ss->mix_waveform_height_symbol =     s7_define_variable(s7, "*" S_mix_waveform_height "*",    s7_make_integer(s7, DEFAULT_MIX_WAVEFORM_HEIGHT));
  ss->sinc_width_symbol =              s7_define_variable(s7, "*" S_sinc_width "*",             s7_make_integer(s7, DEFAULT_SINC_WIDTH));
  ss->region_graph_style_symbol =      s7_define_variable(s7, "*" S_region_graph_style "*",     s7_make_integer(s7, ss->Region_Graph_Style));
  ss->max_regions_symbol =             s7_define_variable(s7, "*" S_max_regions "*",            s7_make_integer(s7, DEFAULT_MAX_REGIONS));
  ss->with_gl_symbol =                 s7_define_variable(s7, "*" S_with_gl "*",                s7_make_boolean(s7, DEFAULT_WITH_GL));
  ss->with_relative_panes_symbol =     s7_define_variable(s7, "*" S_with_relative_panes "*",    s7_make_boolean(s7, DEFAULT_WITH_RELATIVE_PANES));
  ss->dac_size_symbol =                s7_define_variable(s7, "*" S_dac_size "*",               s7_make_integer(s7, DEFAULT_DAC_SIZE));
  ss->view_files_sort_symbol =         s7_define_variable(s7, "*" S_view_files_sort "*",        s7_make_integer(s7, DEFAULT_VIEW_FILES_SORT));
  ss->dac_combines_channels_symbol =   s7_define_variable(s7, "*" S_dac_combines_channels "*",  s7_make_boolean(s7, DEFAULT_DAC_COMBINES_CHANNELS));
  ss->show_selection_transform_symbol = s7_define_variable(s7, "*" S_show_selection_transform "*", s7_make_boolean(s7, DEFAULT_SHOW_SELECTION_TRANSFORM));
  ss->with_mix_tags_symbol =           s7_define_variable(s7, "*" S_with_mix_tags "*",          s7_make_boolean(s7, DEFAULT_WITH_MIX_TAGS));
  ss->listener_prompt_symbol =         s7_define_variable(s7, "*" S_listener_prompt "*",        s7_make_string(s7, DEFAULT_LISTENER_PROMPT));
  ss->stdin_prompt_symbol =            s7_define_variable(s7, "*" S_stdin_prompt "*",           s7_make_string(s7, DEFAULT_STDIN_PROMPT));
  ss->enved_base_symbol =              s7_define_variable(s7, "*" S_enved_base "*",             s7_make_real(s7, DEFAULT_ENVED_BASE));
  ss->enved_power_symbol =             s7_define_variable(s7, "*" S_enved_power "*",            s7_make_real(s7, DEFAULT_ENVED_POWER));
  ss->enved_with_wave_symbol =         s7_define_variable(s7, "*" S_enved_with_wave "*",        s7_make_boolean(s7, DEFAULT_ENVED_WITH_WAVE));
  ss->enved_style_symbol =             s7_define_variable(s7, "*" S_enved_style "*",            s7_make_integer(s7, DEFAULT_ENVED_STYLE));
  ss->graph_cursor_symbol =            s7_define_variable(s7, "*" S_graph_cursor "*",           s7_make_integer(s7, DEFAULT_GRAPH_CURSOR));
  ss->mix_tag_width_symbol =           s7_define_variable(s7, "*" S_mix_tag_width "*",          s7_make_integer(s7, DEFAULT_MIX_TAG_WIDTH));
  ss->mix_tag_height_symbol =          s7_define_variable(s7, "*" S_mix_tag_height "*",         s7_make_integer(s7, DEFAULT_MIX_TAG_HEIGHT));
  ss->mark_tag_height_symbol =         s7_define_variable(s7, "*" S_mark_tag_height "*",        s7_make_integer(s7, DEFAULT_MARK_TAG_HEIGHT));
  ss->mark_tag_width_symbol =          s7_define_variable(s7, "*" S_mark_tag_width "*",         s7_make_integer(s7, DEFAULT_MARK_TAG_WIDTH));
  ss->enved_target_symbol =            s7_define_variable(s7, "*" S_enved_target "*",           s7_make_integer(s7, DEFAULT_ENVED_TARGET));
  ss->cursor_update_interval_symbol =  s7_define_variable(s7, "*" S_cursor_update_interval "*", s7_make_real(s7, DEFAULT_CURSOR_UPDATE_INTERVAL));
  ss->cursor_location_offset_symbol =  s7_define_variable(s7, "*" S_cursor_location_offset "*", s7_make_integer(s7, DEFAULT_CURSOR_LOCATION_OFFSET));
  ss->show_controls_symbol =           s7_define_variable(s7, "*" S_show_controls "*",          s7_make_boolean(s7, DEFAULT_SHOW_CONTROLS));
  ss->with_tracking_cursor_symbol =    s7_define_variable(s7, "*" S_with_tracking_cursor "*",   s7_make_integer(s7, (int)DEFAULT_WITH_TRACKING_CURSOR));
  ss->html_dir_symbol =                s7_define_variable(s7, "*" S_html_dir "*",               s7_make_string(s7, DEFAULT_HTML_DIR));
  ss->html_program_symbol =            s7_define_variable(s7, "*" S_html_program "*",           s7_make_string(s7, DEFAULT_HTML_PROGRAM));
  ss->open_file_dialog_directory_symbol = s7_define_variable(s7, "*" S_open_file_dialog_directory "*", s7_make_string(s7, ss->Open_File_Dialog_Directory));

  /* snd-file.c */
  ss->auto_update_symbol =             s7_define_variable(s7, "*" S_auto_update "*",          s7_make_boolean(s7, DEFAULT_AUTO_UPDATE));
  ss->auto_update_interval_symbol =    s7_define_variable(s7, "*" S_auto_update_interval "*", s7_make_real(s7, DEFAULT_AUTO_UPDATE_INTERVAL));
  ss->clipping_symbol =                s7_define_variable(s7, "*" S_clipping "*",             s7_make_boolean(s7, DEFAULT_CLIPPING));
  ss->default_output_header_type_symbol = s7_define_variable(s7, "*" S_default_output_header_type "*", s7_make_integer(s7, DEFAULT_OUTPUT_HEADER_TYPE));
  ss->default_output_sample_type_symbol = s7_define_variable(s7, "*" S_default_output_sample_type "*", s7_make_integer(s7, DEFAULT_OUTPUT_SAMPLE_TYPE));
  ss->default_output_chans_symbol =    s7_define_variable(s7, "*" S_default_output_chans "*", s7_make_integer(s7, DEFAULT_OUTPUT_CHANS));
  ss->default_output_srate_symbol =    s7_define_variable(s7, "*" S_default_output_srate "*", s7_make_integer(s7, DEFAULT_OUTPUT_SRATE));
  ss->ask_before_overwrite_symbol =    s7_define_variable(s7, "*" S_ask_before_overwrite "*", s7_make_boolean(s7, DEFAULT_ASK_BEFORE_OVERWRITE));
  ss->ask_about_unsaved_edits_symbol = s7_define_variable(s7, "*" S_ask_about_unsaved_edits "*", s7_make_boolean(s7, DEFAULT_ASK_ABOUT_UNSAVED_EDITS));
  ss->show_full_duration_symbol =      s7_define_variable(s7, "*" S_show_full_duration "*",   s7_make_boolean(s7, DEFAULT_SHOW_FULL_DURATION));
  ss->show_full_range_symbol =         s7_define_variable(s7, "*" S_show_full_range "*",      s7_make_boolean(s7, DEFAULT_SHOW_FULL_RANGE));
  ss->remember_sound_state_symbol =    s7_define_variable(s7, "*" S_remember_sound_state "*", s7_make_boolean(s7, DEFAULT_REMEMBER_SOUND_STATE));
  ss->save_as_dialog_src_symbol =      s7_define_variable(s7, "*" S_save_as_dialog_src "*",   s7_make_boolean(s7, DEFAULT_SAVE_AS_DIALOG_SRC));
  ss->save_as_dialog_auto_comment_symbol = s7_define_variable(s7, "*" S_save_as_dialog_auto_comment "*", s7_make_boolean(s7, DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT));
  ss->with_toolbar_symbol =            s7_define_variable(s7, "*" S_with_toolbar "*",         s7_make_boolean(s7, DEFAULT_WITH_TOOLBAR));
  ss->with_tooltips_symbol =           s7_define_variable(s7, "*" S_with_tooltips "*",        s7_make_boolean(s7, DEFAULT_WITH_TOOLTIPS));
  ss->with_menu_icons_symbol =         s7_define_variable(s7, "*" S_with_menu_icons "*",      s7_make_boolean(s7, DEFAULT_WITH_MENU_ICONS));
  ss->initial_beg_symbol =             s7_define_variable(s7, "*" S_initial_beg "*",          s7_make_real(s7, DEFAULT_INITIAL_BEG));
  ss->initial_dur_symbol =             s7_define_variable(s7, "*" S_initial_dur "*",          s7_make_real(s7, DEFAULT_INITIAL_DUR));

  /* snd-main.c */
  ss->show_indices_symbol =          s7_define_variable(s7, "*" S_show_indices "*",           s7_make_boolean(s7, DEFAULT_SHOW_INDICES));
  ss->just_sounds_symbol =           s7_define_variable(s7, "*" S_just_sounds "*",            s7_make_boolean(s7, DEFAULT_JUST_SOUNDS));
  ss->play_arrow_size_symbol =       s7_define_variable(s7, "*" S_play_arrow_size "*",        s7_make_integer(s7, DEFAULT_PLAY_ARROW_SIZE));
  ss->print_length_symbol =          s7_define_variable(s7, "*" S_print_length "*",           s7_make_integer(s7, DEFAULT_PRINT_LENGTH));
  ss->selection_creates_region_symbol = s7_define_variable(s7, "*" S_selection_creates_region "*", s7_make_boolean(s7, DEFAULT_SELECTION_CREATES_REGION));
  ss->save_state_file_symbol =       s7_define_variable(s7, "*" S_save_state_file "*",        s7_make_string(s7, DEFAULT_SAVE_STATE_FILE));
  ss->with_background_processes_symbol = s7_define_variable(s7, "*" S_with_background_processes "*", s7_make_boolean(s7, DEFAULT_WITH_BACKGROUND_PROCESSES));
  ss->with_file_monitor_symbol =     s7_define_variable(s7, "*" S_with_file_monitor "*",      s7_make_boolean(s7, DEFAULT_WITH_FILE_MONITOR));
  ss->temp_dir_symbol =              s7_define_variable(s7, "*" S_temp_dir "*",               s7_make_string(s7, ss->Temp_Dir));
  ss->save_dir_symbol =              s7_define_variable(s7, "*" S_save_dir "*",               s7_make_string(s7, ss->Save_Dir));
  ss->ladspa_dir_symbol =            s7_define_variable(s7, "*" S_ladspa_dir "*",             s7_make_string(s7, DEFAULT_LADSPA_DIR));
  ss->peak_env_dir_symbol =          s7_define_variable(s7, "*" S_peak_env_dir "*",           s7_make_string(s7, DEFAULT_PEAK_ENV_DIR));
  ss->axis_label_font_symbol =       s7_define_variable(s7, "*" S_axis_label_font "*",        s7_make_string(s7, DEFAULT_AXIS_LABEL_FONT));
  ss->axis_numbers_font_symbol =     s7_define_variable(s7, "*" S_axis_numbers_font "*",      s7_make_string(s7, DEFAULT_AXIS_NUMBERS_FONT));
  ss->tiny_font_symbol =             s7_define_variable(s7, "*" S_tiny_font "*",              s7_make_string(s7, DEFAULT_TINY_FONT));
  ss->peaks_font_symbol =            s7_define_variable(s7, "*" S_peaks_font "*",             s7_make_string(s7, DEFAULT_PEAKS_FONT));
  ss->bold_peaks_font_symbol =       s7_define_variable(s7, "*" S_bold_peaks_font "*",        s7_make_string(s7, DEFAULT_BOLD_PEAKS_FONT));
  ss->with_inset_graph_symbol =      s7_define_variable(s7, "*" S_with_inset_graph "*",       s7_make_boolean(s7, DEFAULT_WITH_INSET_GRAPH));
  ss->with_pointer_focus_symbol =    s7_define_variable(s7, "*" S_with_pointer_focus "*",     s7_make_boolean(s7, DEFAULT_WITH_POINTER_FOCUS));
  ss->with_smpte_label_symbol =      s7_define_variable(s7, "*" S_with_smpte_label "*",       s7_make_boolean(s7, DEFAULT_WITH_SMPTE_LABEL));
  ss->with_interrupts_symbol =       s7_define_variable(s7, "*" S_with_interrupts "*",        s7_make_boolean(s7, DEFAULT_WITH_INTERRUPTS));
  ss->color_scale_symbol =           s7_define_variable(s7, "*" S_color_scale "*",            s7_make_real(s7, DEFAULT_COLOR_SCALE));
  ss->color_cutoff_symbol =          s7_define_variable(s7, "*" S_color_cutoff "*",           s7_make_real(s7, DEFAULT_COLOR_CUTOFF));
  ss->color_inverted_symbol =        s7_define_variable(s7, "*" S_color_inverted "*",         s7_make_boolean(s7, DEFAULT_COLOR_INVERTED));
  ss->auto_resize_symbol =           s7_define_variable(s7, "*" S_auto_resize "*",            s7_make_boolean(s7, DEFAULT_AUTO_RESIZE));
#if USE_MOTIF
  #define DEFAULT_LISTENER_FONT "9x15"
#endif
#if (!USE_NO_GUI)
  ss->listener_font_symbol =         s7_define_variable(s7, "*" S_listener_font "*",          s7_make_string(s7, DEFAULT_LISTENER_FONT));
#else
  ss->listener_font_symbol =         s7_define_variable(s7, "*" S_listener_font "*",          s7_make_string(s7, ""));
#endif

  /* snd-snd.c */
  ss->channel_style_symbol =           s7_define_variable(s7, "*" S_channel_style "*",           s7_make_integer(s7, DEFAULT_CHANNEL_STYLE));
  ss->filter_control_in_db_symbol =    s7_define_variable(s7, "*" S_filter_control_in_dB "*",    s7_make_boolean(s7, DEFAULT_FILTER_CONTROL_IN_DB));
  ss->filter_control_in_hz_symbol =    s7_define_variable(s7, "*" S_filter_control_in_hz "*",    s7_make_boolean(s7, DEFAULT_FILTER_CONTROL_IN_HZ));
  ss->speed_control_tones_symbol =     s7_define_variable(s7, "*" S_speed_control_tones "*",     s7_make_integer(s7, DEFAULT_SPEED_CONTROL_TONES));
  ss->speed_control_style_symbol =     s7_define_variable(s7, "*" S_speed_control_style "*",     s7_make_integer(s7, DEFAULT_SPEED_CONTROL_STYLE));
  ss->expand_control_length_symbol =   s7_define_variable(s7, "*" S_expand_control_length "*",   s7_make_real(s7, DEFAULT_EXPAND_CONTROL_LENGTH));
  ss->expand_control_ramp_symbol =     s7_define_variable(s7, "*" S_expand_control_ramp "*",     s7_make_real(s7, DEFAULT_EXPAND_CONTROL_RAMP));
  ss->expand_control_hop_symbol =      s7_define_variable(s7, "*" S_expand_control_hop "*",      s7_make_real(s7, DEFAULT_EXPAND_CONTROL_HOP));
  ss->expand_control_jitter_symbol =   s7_define_variable(s7, "*" S_expand_control_jitter "*",   s7_make_real(s7, DEFAULT_EXPAND_CONTROL_JITTER));
  ss->contrast_control_amp_symbol =    s7_define_variable(s7, "*" S_contrast_control_amp "*",    s7_make_real(s7, DEFAULT_CONTRAST_CONTROL_AMP));
  ss->reverb_control_feedback_symbol = s7_define_variable(s7, "*" S_reverb_control_feedback "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_FEEDBACK));
  ss->reverb_control_lowpass_symbol =  s7_define_variable(s7, "*" S_reverb_control_lowpass "*",  s7_make_real(s7, DEFAULT_REVERB_CONTROL_LOWPASS));
  ss->reverb_control_decay_symbol =    s7_define_variable(s7, "*" S_reverb_control_decay "*",    s7_make_real(s7, DEFAULT_REVERB_CONTROL_DECAY));
  ss->filter_control_order_symbol =    s7_define_variable(s7, "*" S_filter_control_order "*",    s7_make_integer(s7, DEFAULT_FILTER_CONTROL_ORDER));

  /* snd-chn.c */
  ss->show_transform_peaks_symbol = s7_define_variable(s7, "*" S_show_transform_peaks "*", s7_make_boolean(s7, DEFAULT_SHOW_TRANSFORM_PEAKS));
  ss->show_y_zero_symbol =          s7_define_variable(s7, "*" S_show_y_zero "*",          s7_make_boolean(s7, DEFAULT_SHOW_Y_ZERO));
  ss->show_marks_symbol =           s7_define_variable(s7, "*" S_show_marks "*",           s7_make_boolean(s7, DEFAULT_SHOW_MARKS));
  ss->show_grid_symbol =            s7_define_variable(s7, "*" S_show_grid "*",            s7_make_boolean(s7, DEFAULT_SHOW_GRID));
  ss->fft_log_frequency_symbol =    s7_define_variable(s7, "*" S_fft_log_frequency "*",    s7_make_boolean(s7, DEFAULT_FFT_LOG_FREQUENCY));
  ss->fft_log_magnitude_symbol =    s7_define_variable(s7, "*" S_fft_log_magnitude "*",    s7_make_boolean(s7, DEFAULT_FFT_LOG_MAGNITUDE));
  ss->fft_with_phases_symbol =      s7_define_variable(s7, "*" S_fft_with_phases "*",      s7_make_boolean(s7, DEFAULT_FFT_WITH_PHASES));
  ss->sync_style_symbol =           s7_define_variable(s7, "*" S_sync_style "*",           s7_make_integer(s7, DEFAULT_SYNC_STYLE));
  ss->show_axes_symbol =            s7_define_variable(s7, "*" S_show_axes "*",            s7_make_integer(s7, DEFAULT_SHOW_AXES));
  ss->min_db_symbol =               s7_define_variable(s7, "*" S_min_dB "*",               s7_make_real(s7, DEFAULT_MIN_DB));
  ss->cursor_size_symbol =          s7_define_variable(s7, "*" S_cursor_size "*",          s7_make_integer(s7, DEFAULT_CURSOR_SIZE));
  ss->cursor_style_symbol =         s7_define_variable(s7, "*" S_cursor_style "*",         s7_make_integer(s7, DEFAULT_CURSOR_STYLE));
  ss->tracking_cursor_style_symbol = s7_define_variable(s7, "*" S_tracking_cursor_style "*", s7_make_integer(s7, DEFAULT_TRACKING_CURSOR_STYLE));
  ss->show_sonogram_cursor_symbol = s7_define_variable(s7, "*" S_show_sonogram_cursor "*", s7_make_boolean(s7, DEFAULT_SHOW_SONOGRAM_CURSOR));
  ss->with_verbose_cursor_symbol =  s7_define_variable(s7, "*" S_with_verbose_cursor "*",  s7_make_boolean(s7, DEFAULT_WITH_VERBOSE_CURSOR));
  ss->spectro_x_scale_symbol =      s7_define_variable(s7, "*" S_spectro_x_scale "*",      s7_make_real(s7, DEFAULT_SPECTRO_X_SCALE));
  ss->spectro_y_scale_symbol =      s7_define_variable(s7, "*" S_spectro_y_scale "*",      s7_make_real(s7, DEFAULT_SPECTRO_Y_SCALE));
  ss->spectro_z_scale_symbol =      s7_define_variable(s7, "*" S_spectro_z_scale "*",      s7_make_real(s7, DEFAULT_SPECTRO_Z_SCALE));
  ss->spectro_z_angle_symbol =      s7_define_variable(s7, "*" S_spectro_z_angle "*",      s7_make_real(s7, DEFAULT_SPECTRO_Z_ANGLE));
  ss->spectro_x_angle_symbol =      s7_define_variable(s7, "*" S_spectro_x_angle "*",      s7_make_real(s7, DEFAULT_SPECTRO_X_ANGLE));
  ss->spectro_y_angle_symbol =      s7_define_variable(s7, "*" S_spectro_y_angle "*",      s7_make_real(s7, DEFAULT_SPECTRO_Y_ANGLE));
  ss->spectrum_end_symbol =         s7_define_variable(s7, "*" S_spectrum_end "*",         s7_make_real(s7, DEFAULT_SPECTRUM_END));
  ss->spectrum_start_symbol =       s7_define_variable(s7, "*" S_spectrum_start "*",       s7_make_real(s7, DEFAULT_SPECTRUM_START));
  ss->spectro_hop_symbol =          s7_define_variable(s7, "*" S_spectro_hop "*",          s7_make_integer(s7, DEFAULT_SPECTRO_HOP));
  ss->graphs_horizontal_symbol =    s7_define_variable(s7, "*" S_graphs_horizontal "*",    s7_make_boolean(s7, DEFAULT_GRAPHS_HORIZONTAL));
  ss->max_transform_peaks_symbol =  s7_define_variable(s7, "*" S_max_transform_peaks "*",  s7_make_integer(s7, DEFAULT_MAX_TRANSFORM_PEAKS));
  ss->fft_window_alpha_symbol =     s7_define_variable(s7, "*" S_fft_window_alpha "*",     s7_make_real(s7, DEFAULT_FFT_WINDOW_ALPHA));
  ss->fft_window_beta_symbol =      s7_define_variable(s7, "*" S_fft_window_beta "*",      s7_make_real(s7, DEFAULT_FFT_WINDOW_BETA));
  ss->grid_density_symbol =         s7_define_variable(s7, "*" S_grid_density "*",         s7_make_real(s7, DEFAULT_GRID_DENSITY));
  ss->beats_per_minute_symbol =     s7_define_variable(s7, "*" S_beats_per_minute "*",     s7_make_real(s7, DEFAULT_BEATS_PER_MINUTE));
  ss->show_mix_waveforms_symbol =   s7_define_variable(s7, "*" S_show_mix_waveforms "*",   s7_make_boolean(s7, DEFAULT_SHOW_MIX_WAVEFORMS));
  ss->beats_per_measure_symbol =    s7_define_variable(s7, "*" S_beats_per_measure "*",    s7_make_integer(s7, DEFAULT_BEATS_PER_MEASURE));
  ss->transform_normalization_symbol = s7_define_variable(s7, "*" S_transform_normalization "*", s7_make_integer(s7, DEFAULT_TRANSFORM_NORMALIZATION));
  ss->x_axis_style_symbol =         s7_define_variable(s7, "*" S_x_axis_style "*",         s7_make_integer(s7, DEFAULT_X_AXIS_STYLE));
  ss->zoom_focus_style_symbol =     s7_define_variable(s7, "*" S_zoom_focus_style "*",     s7_make_integer(s7, DEFAULT_ZOOM_FOCUS_STYLE));
  ss->graph_style_symbol =          s7_define_variable(s7, "*" S_graph_style "*",          s7_make_integer(s7, DEFAULT_GRAPH_STYLE));
  ss->wavelet_type_symbol =         s7_define_variable(s7, "*" S_wavelet_type "*",         s7_make_integer(s7, DEFAULT_WAVELET_TYPE));
  ss->dot_size_symbol =             s7_define_variable(s7, "*" S_dot_size "*",             s7_make_integer(s7, DEFAULT_DOT_SIZE));
  ss->zero_pad_symbol =             s7_define_variable(s7, "*" S_zero_pad "*",             s7_make_integer(s7, DEFAULT_ZERO_PAD));
  ss->wavo_hop_symbol =             s7_define_variable(s7, "*" S_wavo_hop "*",             s7_make_integer(s7, DEFAULT_WAVO_HOP));
  ss->wavo_trace_symbol =           s7_define_variable(s7, "*" S_wavo_trace "*",           s7_make_integer(s7, DEFAULT_WAVO_TRACE));
  ss->transform_size_symbol =       s7_define_variable(s7, "*" S_transform_size "*",       s7_make_integer(s7, DEFAULT_TRANSFORM_SIZE));
  ss->fft_window_symbol =           s7_define_variable(s7, "*" S_fft_window "*",           s7_make_integer(s7, DEFAULT_FFT_WINDOW));
  ss->transform_graph_type_symbol = s7_define_variable(s7, "*" S_transform_graph_type "*", s7_make_integer(s7, DEFAULT_TRANSFORM_GRAPH_TYPE));
  ss->time_graph_type_symbol =      s7_define_variable(s7, "*" S_time_graph_type "*",      s7_make_integer(s7, DEFAULT_TIME_GRAPH_TYPE));

  /* snd-draw.c */
  ss->data_color_symbol           = s7_define_variable(s7, "*" S_data_color "*",           s7_f(s7));
  ss->selected_data_color_symbol  = s7_define_variable(s7, "*" S_selected_data_color "*",  s7_f(s7));
  ss->mark_color_symbol           = s7_define_variable(s7, "*" S_mark_color "*",           s7_f(s7));
  ss->graph_color_symbol          = s7_define_variable(s7, "*" S_graph_color "*",          s7_f(s7));
  ss->selected_graph_color_symbol = s7_define_variable(s7, "*" S_selected_graph_color "*", s7_f(s7));
  ss->listener_color_symbol       = s7_define_variable(s7, "*" S_listener_color "*",       s7_f(s7));
  ss->listener_text_color_symbol  = s7_define_variable(s7, "*" S_listener_text_color "*",  s7_f(s7));
  ss->basic_color_symbol          = s7_define_variable(s7, "*" S_basic_color "*",          s7_f(s7));
  ss->selection_color_symbol      = s7_define_variable(s7, "*" S_selection_color "*",      s7_f(s7));
  ss->zoom_color_symbol           = s7_define_variable(s7, "*" S_zoom_color "*",           s7_f(s7));
  ss->position_color_symbol       = s7_define_variable(s7, "*" S_position_color "*",       s7_f(s7));
  ss->highlight_color_symbol      = s7_define_variable(s7, "*" S_highlight_color "*",      s7_f(s7));
  ss->enved_waveform_color_symbol = s7_define_variable(s7, "*" S_enved_waveform_color "*", s7_f(s7));
  ss->cursor_color_symbol         = s7_define_variable(s7, "*" S_cursor_color "*",         s7_f(s7));
  ss->text_focus_color_symbol     = s7_define_variable(s7, "*" S_text_focus_color "*",     s7_f(s7));
  ss->filter_control_waveform_color_symbol = s7_define_variable(s7, "*" S_filter_control_waveform_color "*", s7_f(s7));
  ss->mix_color_symbol            = s7_define_variable(s7, "*" S_mix_color "*",            s7_f(s7));
  ss->sash_color_symbol           = s7_define_variable(s7, "*" S_sash_color "*",           s7_f(s7));
  ss->axis_color_symbol           = s7_define_variable(s7, "*" S_axis_color "*",           s7_f(s7));

  ss->transform_type_symbol = s7_define_variable(s7, "*" S_transform_type "*", s7_f(s7)); /* set in snd-chn.c(!) */
#if USE_GTK
  ss->listener_colorized_symbol = s7_define_variable(s7, "*listener-colorized*", s7_make_boolean(s7, s7_f(s7)));
#endif
#endif
}


#if HAVE_GSL
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_errno.h>

/* default gsl error handler apparently aborts main program! */

static void snd_gsl_error(const char *reason, const char *file, int line, int gsl_errno)
{
  Xen_error(Xen_make_error_type("gsl-error"),
	    Xen_list_6(C_string_to_Xen_string("GSL: ~A, ~A in ~A line ~A, gsl err: ~A"),
		       C_string_to_Xen_string(gsl_strerror(gsl_errno)),
		       C_string_to_Xen_string(reason),
		       C_string_to_Xen_string(file),
		       C_int_to_Xen_integer(line),
		       C_int_to_Xen_integer(gsl_errno)));
}
#endif


int main(int argc, char **argv)
{
  int i;

#if (!_MSC_VER)
  setlocale(LC_NUMERIC, "C"); /* use decimal point in floats */
#endif

#if HAVE_GSL
  /* if HAVE_GSL and the environment variable GSL_IEEE_MODE exists, use it */
  /* GSL_IEEE_MODE=double-precision,mask-underflow,mask-denormalized */
  if (getenv("GSL_IEEE_MODE")) 
    gsl_ieee_env_setup();
  gsl_set_error_handler(snd_gsl_error);
#endif

  ss = (snd_state *)calloc(1, sizeof(snd_state)); /* not calloc! */
  ss->startup_errors = NULL;

#if USE_GTK
#if GTK_CHECK_VERSION(3, 0, 0) && (!GLIB_CHECK_VERSION(2,35,0))
  g_type_init();
#endif
#endif

  mus_sound_initialize(); /* has to precede version check (mus_audio_moniker needs to be setup in Alsa/Oss) */
  xen_initialize();

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "--version") == 0)
	{
	  fprintf(stdout, "%s", version_info());
	  snd_exit(0);
	}
      else
	{
	  if (strcmp(argv[i], "--help") == 0)
	    {
	      fprintf(stdout, "%s", "Snd is a sound editor; see http://ccrma.stanford.edu/software/snd/.\n");
	      fprintf(stdout, "%s", version_info());
	      snd_exit(0);
	    }
	}
    }

  initialize_sample_type_lists();
  snd_set_global_defaults(false);

  ss->jump_ok = false;
  ss->file_monitor_ok = false;
  allocate_regions(max_regions(ss));
  ss->init_window_x = DEFAULT_INIT_WINDOW_X; 
  ss->init_window_y = DEFAULT_INIT_WINDOW_Y; 
  ss->init_window_width = DEFAULT_INIT_WINDOW_WIDTH; 
  ss->init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;
  ss->click_time = 100;
  init_sound_file_extensions();

  ss->max_sounds = 4;                 /* expands to accommodate any number of files */
  ss->sound_sync_max = 0;
  ss->stopped_explicitly = false;     /* C-g sets this flag so that we can interrupt various loops */
  ss->checking_explicitly = false;
  ss->selection_play_stop = false;
  ss->reloading_updated_file = 0;
  ss->selected_sound = NO_SELECTION;
  ss->sounds = (snd_info **)calloc(ss->max_sounds, sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->graph_hook_active = false;
  ss->lisp_graph_hook_active = false;
  ss->exiting = false;
  ss->deferred_regions = 0;
  ss->snd_error_data = NULL;
  ss->snd_error_handler = NULL;
  ss->snd_warning_data = NULL;
  ss->snd_warning_handler = NULL;
  ss->xen_error_data = NULL;
  ss->xen_error_handler = NULL;
  ss->update_sound_channel_style = NOT_A_CHANNEL_STYLE;
  ss->squelch_mark_drag_info = false;

#if HAVE_GL && WITH_GL2PS
  ss->gl_printing = false;
#endif
  g_xen_initialize();
  ss->search_proc = Xen_undefined;
  ss->search_expr = NULL;
  mus_error_set_handler(mus_error_to_snd);
  mus_print_set_handler(mus_print_to_snd);

  initialize_load_path(); /* merge SND_PATH entries into the load-path */

  snd_doit(argc, argv);
  return(0);
}


void g_init_base(void)
{
  #define H_mus_error_hook S_mus_error_hook " (type message):  called upon mus_error. \
If it returns " PROC_TRUE ", Snd ignores the error (it assumes you've handled it via the hook)."

  ss->mus_error_hook = Xen_define_hook(S_mus_error_hook, "(make-hook 'type 'message)", 2, H_mus_error_hook);
}
