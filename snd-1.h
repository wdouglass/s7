#ifndef SND_1_H
#define SND_1_H

#define WITH_RELATIVE_PANES (USE_MOTIF && (XmVERSION > 1))

#define Snd_assert_sound(Origin, Snd, Offset) \
  if (!((Xen_is_integer(Snd)) || (xen_is_sound(Snd)) || (Xen_is_false(Snd)) || (!Xen_is_bound(Snd)))) \
    Xen_wrong_type_arg_error(Origin, Offset, Snd, "a sound object, an integer (sound index), or " PROC_FALSE);

#define Snd_assert_channel(Origin, Snd, Chn, Offset) \
  if (!((Xen_is_integer(Snd)) || (xen_is_sound(Snd)) || (Xen_is_false(Snd)) || (!Xen_is_bound(Snd)))) \
    Xen_wrong_type_arg_error(Origin, Offset, Snd, "a sound object, an integer (sound index), or " PROC_FALSE); \
  else \
    if (!((Xen_is_integer(Chn)) || (Xen_is_false(Chn)) || (!Xen_is_bound(Chn)))) \
      Xen_wrong_type_arg_error(Origin, Offset + 1, Chn, "an integer (0-based channel number) or " PROC_FALSE);


#if HAVE_SCHEME
/* these macros fix up argument order for setter procs in Scheme: (set! (proc a b) c) */

#define with_two_setter_args(name_reversed, name)	   \
  static s7_pointer name_reversed(s7_scheme *sc, s7_pointer args)   \
  {                                                        \
    if (Xen_is_null(Xen_cdr(args)))		   \
      return(name(Xen_car(args), Xen_undefined));	   \
    return(name(Xen_cadr(args), Xen_car(args)));	   \
  }

#define with_three_setter_args(name_reversed, name)                      \
  static s7_pointer name_reversed(s7_scheme *sc, s7_pointer args)                 \
  {							                 \
    if (Xen_is_null(Xen_cdr(args)))		                 \
      return(name(Xen_car(args), Xen_undefined, Xen_undefined));         \
    else {					 		         \
      if (Xen_is_null(Xen_cddr(args)))				 \
	return(name(Xen_cadr(args), Xen_car(args), Xen_undefined));	\
      else return(name(Xen_caddr(args), Xen_car(args), Xen_cadr(args))); \
  }}

#define with_four_setter_args(name_reversed, name)                                         \
  static s7_pointer name_reversed(s7_scheme *sc, s7_pointer args)                                   \
{							                                   \
  if (Xen_is_null(Xen_cdr(args)))					                   \
    return(name(Xen_car(args), Xen_undefined, Xen_undefined, Xen_undefined));              \
  else {								                   \
    if (Xen_is_null(Xen_cddr(args)))				                   \
      return(name(Xen_cadr(args), Xen_car(args), Xen_undefined, Xen_undefined));           \
    else {								                   \
      if (Xen_is_null(Xen_cdddr(args)))				                   \
	return(name(Xen_caddr(args), Xen_car(args), Xen_cadr(args), Xen_undefined));       \
      else return(name(Xen_cadddr(args), Xen_car(args), Xen_cadr(args), Xen_caddr(args))); \
  }}}

#else

#define with_two_setter_args(name_reversed, name)
#define with_three_setter_args(name_reversed, name)
#define with_four_setter_args(name_reversed, name)
#endif

#define Snd_assert_sample_type(Origin, Beg, Offset) \
  Xen_check_type(Xen_is_integer(Beg) || Xen_is_false(Beg) || (!Xen_is_bound(Beg)), Beg, Offset, Origin, "an integer or " PROC_FALSE)

typedef struct {
  char **values;
  int num_values, values_size;
  bool exact_match;
} list_completer_info;

typedef struct {
  int samps_per_bin, peak_env_size;
  mus_float_t fmax, fmin;
  mus_float_t *data_max, *data_min;
  bool completed;
  int bin, top_bin;
} peak_env_info;

typedef struct snd_io snd_io;

typedef struct {
  char *name;                  /* full name */
  mus_long_t samples;          /* total samples = chans * framples */
  mus_long_t data_location;    /* bytes */
  int srate;
  int chans;
  mus_sample_t sample_type;    /* sample type (mus_bshort etc) */
  mus_header_t type;           /* header type (mus_aifc etc) */
  char *comment;               /* output case, not input */
  int *loops;
} file_info;

typedef struct {
  snd_data_file_t type;
  mus_float_t *buffered_data;    
  snd_io *io;      
  char *filename;
  file_info *hdr;
  file_delete_t temporary;
  int edit_ctr;
  fd_open_t open;
  bool inuse;
  bool copy;
  int chan;
  mus_long_t data_bytes;        /* used only for edit-history descriptions (snd-edits.c display_ed_list) */
  bool free_me;
} snd_data;

typedef struct mark mark;
typedef struct enved_fft enved_fft;

typedef struct {
  int size, allocated_size;
  struct ed_fragment **fragments;
  mus_long_t beg, len;                      /* beg and len of changed portion */
  char *origin;
  edit_t edit_type;
  int sound_location;
  mus_long_t selection_beg, selection_end;  /* selection needs to follow edit list */
  mus_float_t maxamp, selection_maxamp;
  mus_long_t maxamp_position, selection_maxamp_position;
  int edpos;
  bool backed_up;
  mus_long_t samples, cursor;
  int mark_size, mark_ctr;
  mark **marks;                        /* mark positions */
  peak_env_info *peak_env; 
  enved_fft *fft;                      /* overall fft data for envelope editor */
  void *readers;                       /* current readers of this edit (g++ stupidity forces us to use void* here -- type is sf_info, snd-edits.c) */
  void *mixes;
  Xen properties;
  int properties_gc_loc;
} ed_list;

typedef struct snd_fd {
  mus_float_t (*runf)(struct snd_fd *sf);
  mus_float_t (*rev_runf)(struct snd_fd *sf);
  ed_list *current_state;
  struct ed_fragment *cb;
  mus_long_t loc, first, last;
  int cbi;
  read_direction_t direction;
  bool at_eof, freed;
  mus_float_t *data;
  snd_data *current_sound;
  mus_long_t initial_samp;
  struct chan_info *cp;
  struct snd_info *local_sp;
  mus_float_t fscaler;
  mus_long_t frag_pos;
  int edit_ctr, region;
  reader_t type;
  void *ramps, *mixes;
} snd_fd;

typedef struct {mus_float_t freq; mus_float_t amp;} fft_peak;

typedef struct {
  mus_float_t y0, y1;                         /* scroller-dependent axis bounds */
  double x0, x1;
  double xmin, xmax;
  mus_float_t ymin, ymax;                     /* data-dependent absolute limits */
  mus_float_t y_scale, y_base, y_ambit;
  double x_scale, x_base, x_ambit;
  char *xlabel, *ylabel, *default_xlabel;
  int y_axis_x0, x_axis_x0, y_axis_y0, x_axis_y0, x_axis_x1, y_axis_y1, x_label_x, x_label_y;
  bool graph_active;
  mus_long_t losamp, hisamp;                 /* displayed x-axis bounds in terms of sound sample numbers */
  int graph_x0;                         /* x axis offset relative to window (for double graphs) */
  struct tick_descriptor *x_ticks, *y_ticks; 
  graphics_context *ax;
  int width, height;
  struct chan_info *cp;
  mus_float_t sy, zy;                         /* as set by user, 0.0 - 1.0 */
  double sx, zx;
  int y_offset;
  int window_width;
  bool no_data, changed;
#if HAVE_GL
  bool use_gl, used_gl;
#endif
} axis_info;

typedef struct {
  mus_float_t *data;
  int pts, data_size; /* data_size is independent of actual number of points of data (can be much larger) */
  mus_float_t base;
} env;

typedef struct {
  int mix_id;
  mus_long_t beg, len;
  mus_float_t scaler, speed;
  env *amp_env;
  int index;           /* cp->sounds index (src writes a new temp file) */
} mix_state;

typedef struct env_editor {
  int *current_xs;
  int *current_ys;
  int current_size;
  axis_info *axis;
  oclock_t down_time;
  bool env_dragged;
  int env_pos;
  bool click_to_delete, in_dB, with_dots, clipping;
  bool edited;
} env_editor;

typedef struct {
  mus_long_t size;
  mus_long_t current_size;
  mus_fft_window_t window;
  mus_float_t alpha, beta;
  mus_float_t scale;
  axis_info *axis;
  mus_float_t *data, *phases;
  char *xlabel;
  struct chan_info *chan;
} fft_info;

typedef struct {
  int total_slices;        /* size of the data array (max for allocation checks) */
  int total_bins;          /* size other axis data array */
  int active_slices;       /* how many slices contain current data */
  int target_bins;         /* this many bins Y-side */
  int target_slices;       /* how many slices in full display (current) */
  mus_float_t **data;            /* data[total_slices][bins] -> each is a spectral magnitude */
  mus_long_t *begs;             /* associated begin times (for data reuse) */
  struct chan_info *cp;
  mus_float_t scale;
} sono_info;

typedef struct {
  int chans, fields;
  double *axis_data;
  bool *fftp, *wavep;
} axes_data;

typedef struct chan_info {
  int chan;                /* which chan are we */
  bool graph_transform_on;  /* f button state */
  bool graph_time_on;       /* w button state */
  bool graph_lisp_on;       /* is lisp graph active */
  struct lisp_grf *lisp_info; /* defined in snd-chn.c */
  bool cursor_on;          /* channel's cursor */
  bool cursor_visible, fft_cursor_visible;     /* for XOR decisions */
  int cursor_size;
  cursor_style_t cursor_style, tracking_cursor_style;
  int cx, cy, fft_cx; /* , old_cy; */ /* graph-relative cursor loc (for XOR in Motif, erase-via-overwrite in cairo) */
  int edit_ctr;            /* channel's edit history */
  int edit_size;           /* current edit list size */
  ed_list **edits;         /* the edit list */
  int sound_size;          /* edit_list associated temp sound buffers */
  int sound_ctr;           /* current location in sounds list */
  snd_data **sounds;       /* the associated temp buffer/file/struct list */
  fft_info *fft;           /* possibly null fft data */
  struct snd_info *sound;  /* containing sound */
  axis_info *axis;         /* time domain axis */

  idle_t fft_in_progress;
  idle_t peak_env_in_progress;
  struct env_state *peak_env_state;
  graphics_context *ax;
  bool selected;
  mus_float_t progress_pct;

#if USE_GTK
  GtkWidget **widgets;
  GtkAdjustment **adjs;
  GdkCursor *current_cursor;
  slist *edhist_list;
  color_info *combined_data_color;
#if GTK_CHECK_VERSION(3, 89, 0)
  cairo_t *clock_pix_cr, *graph_cr;
#endif
#endif

#if USE_MOTIF
  Widget *widgets;
  Pixmap fft_pix;
  uint32_t fft_pix_width, fft_pix_height;
  int fft_pix_x0, fft_pix_y0;
  bool fft_pix_ready;
  mus_float_t fft_pix_cutoff;
  Cursor current_cursor;
  Pixel combined_data_color;
#endif
#if USE_NO_GUI
  int current_cursor, combined_data_color;
#endif

  sono_info *sonogram_data;
  struct sonogram_state *last_sonogram, *temp_sonogram; /* defined in snd-fft.c */
  struct wavogram_state *last_wavogram;                 /* defined in snd-chn.c */
  bool show_sonogram_cursor;
  struct fft_state *fft_data;          /* parallels sonogram -- try to avoid repeating large ffts needlessly */
  printing_t printing;
  fft_change_t fft_changed;
  mus_float_t gsy, gzy;
  int height;
  mus_long_t original_cursor, original_left_sample, original_window_size;   /* for cursor reset after cursor-moving play */
  with_hook_t hookable;
  mus_long_t selection_transform_size;
  bool squelch_update, previous_squelch_update, waiting_to_make_graph;
  int in_as_one_edit, as_one_edit_positions_size;
  int *as_one_edit_positions;
  /* moved from global to channel-local 4-Aug-00 */
  mus_float_t spectro_x_scale, spectro_y_scale, spectro_z_scale, spectro_z_angle, spectro_x_angle, spectro_y_angle;
  mus_float_t spectrum_end, spectrum_start;
  mus_float_t lin_dB, min_dB, fft_window_alpha, fft_window_beta, beats_per_minute, grid_density;
  bool show_y_zero, show_marks, with_verbose_cursor;
  with_grid_t show_grid;
  int wavo_hop, wavo_trace, zero_pad, wavelet_type, max_transform_peaks, beats_per_measure;
  x_axis_style_t x_axis_style;
  show_axes_t show_axes;
  mus_long_t transform_size;
  mus_fft_window_t fft_window;
  graph_type_t transform_graph_type, time_graph_type;
  bool show_transform_peaks, fft_log_frequency, fft_log_magnitude, fft_with_phases;
  graph_style_t time_graph_style, lisp_graph_style, transform_graph_style;
  int dot_size;
  fft_normalize_t transform_normalization;
  int transform_type, spectro_hop, edhist_base;
  bool show_mix_waveforms, graphs_horizontal;
  Xen edit_hook;
  Xen undo_hook;
  Xen cursor_proc;
  Xen after_edit_hook;
  Xen properties;
  int cursor_proc_loc, edit_hook_loc, undo_hook_loc, after_edit_hook_loc, properties_loc;
  bool selection_visible;
  channel_state_t active;
  int old_x0, old_x1;
  mus_float_t *amp_control; /* local amp controls in snd-dac; should it be extended to other controls? */
  bool just_zero, new_peaks, editable, updating;
  struct inset_graph_info_t *inset_graph; /* defined in snd-chn.c */
#if HAVE_GL
  int gl_fft_list, gl_wavo_list;
#endif
} chan_info;

#define current_samples(Cp) (Cp)->edits[(Cp)->edit_ctr]->samples
#define cursor_sample(Cp) (Cp)->edits[(Cp)->edit_ctr]->cursor

typedef struct snd_info {
  sound_inuse_t inuse;
  int index;
  int playing;
  int sync, previous_sync;
  bool expand_control_on;
  bool contrast_control_on;
  bool reverb_control_on;
  bool filter_control_on, filter_control_in_dB, filter_control_in_hz;
  mus_float_t amp_control;
  mus_float_t speed_control;
  int speed_control_direction, speed_control_tones, speed_control_numerator, speed_control_denominator;
  speed_style_t speed_control_style;
  mus_float_t last_speed_control, last_amp_control, last_expand_control, last_contrast_control;
  mus_float_t last_reverb_control_length, last_reverb_control_scale;
  mus_float_t saved_speed_control, saved_amp_control, saved_expand_control, saved_contrast_control;
  mus_float_t saved_reverb_control_length, saved_reverb_control_scale;
  mus_float_t expand_control, expand_control_length, expand_control_ramp, expand_control_hop, expand_control_jitter;
  mus_float_t contrast_control, contrast_control_amp;
  mus_float_t reverb_control_length, reverb_control_scale, reverb_control_feedback, reverb_control_lowpass;
  mus_float_t reverb_control_decay, filter_control_xmax;
  mus_float_t contrast_control_min, contrast_control_max, expand_control_min, expand_control_max, speed_control_min, speed_control_max;
  mus_float_t amp_control_min, amp_control_max, reverb_control_scale_min, reverb_control_scale_max;
  mus_float_t reverb_control_length_min, reverb_control_length_max;
  int filter_control_order;
  bool filter_control_changed;
  env *filter_control_envelope;
  int selected_channel;
  char *filename;
  char *short_filename;
  uint32_t nchans;  /* "unsigned" to make gcc 7.1 happy */
  Xen properties;
  int properties_loc;
  bool remembering;
  read_only_t user_read_only, file_read_only;
  chan_info **chans;

  struct env_editor *flt;
#if USE_MOTIF
  Widget *widgets;
  Widget *progress_widgets;
  int num_progress_widgets;
  Widget tab;
  Widget dialog;
  int bomb_ctr;
#endif
#if USE_GTK
  GtkWidget **widgets;
  GtkAdjustment **snd_adjs;
  GtkWidget *dialog;
  int page;
  graphics_context *name_pix_ax, *stop_pix_ax, *speed_arrow_ax, *filter_ax;
  graphics_context **clock_pix_ax;
  GtkWidget **clock_widgets;
  int num_clock_widgets;
#if GTK_CHECK_VERSION(3, 89, 0)
  cairo_t *stop_pix_cr, *name_pix_cr, *speed_arrow_cr, *filter_drawer_cr;
#endif
#endif
#if USE_NO_GUI
  bool widgets;
#endif

  file_info *hdr;             /* header of file that would be affected if we were to save current edits */
  time_t write_date, update_warning_write_date;   /* check for change behind back while editing */
  bool need_update, file_unreadable; /* current in-core data does not match actual file (someone wrote it behind our back) */
  channel_style_t channel_style;
  int allocated_chans, selectpos; 
  struct region *edited_region;
  void *delete_me;
  chan_info *lacp;
  struct ctrl_state *saved_controls;
  bool apply_ok, applying;
  bool active;
  char *name_string;
  void *file_watcher;
  bool writing, bomb_in_progress;
} snd_info;

#define snd_srate(Sp) (((Sp)->hdr)->srate)
#define has_widgets(Sp) ((Sp) && ((Sp)->widgets))

typedef struct snd_state {
  int selected_sound;         /* NO_SELECTION = none selected = which sound is currently receiving user's attention */
  int active_sounds;
  int channel_min_height;
  snd_info **sounds;
  char *search_expr, *startup_title, *startup_errors;
  Xen search_proc;
  int file_sorters_size, file_filters_size;
  Xen file_sorters, file_filters;
  int search_proc_loc, local_errno, local_open_errno;
  int position_slider_width, zoom_slider_width, toggle_size, channel_sash_indent, sash_size, channel_sash_size, sash_indent;
  int max_sounds, sound_sync_max;
  char *translated_filename;
  print_choice_t print_choice;
  snd_apply_t apply_choice;
  bool gl_has_double_buffer;
  bool stopped_explicitly, checking_explicitly, selection_play_stop;
  int reloading_updated_file;
  int init_window_width, init_window_height;
  int init_window_x, init_window_y;
  bool graph_hook_active, lisp_graph_hook_active;

  bool Show_Transform_Peaks, Show_Y_Zero, Show_Marks;
  with_grid_t Show_Grid;
  bool Fft_Log_Frequency, Fft_Log_Magnitude, Fft_With_Phases;
  channel_style_t Channel_Style;
  sync_style_t Sync_Style;
  sound_style_t Sound_Style;
  show_axes_t Show_Axes;
  char *Eps_File, *Temp_Dir, *Save_Dir, *Ladspa_Dir, *Peak_Env_Dir;
  char *Listener_Font, *Axis_Label_Font, *Axis_Numbers_Font, *Tiny_Font, *Peaks_Font, *Bold_Peaks_Font;
  char *orig_listener_font, *orig_axis_label_font, *orig_axis_numbers_font, *orig_tiny_font, *orig_peaks_font, *orig_bold_peaks_font;
  bool With_Verbose_Cursor, With_Inset_Graph, With_Pointer_Focus, With_Smpte_Label, With_Interrupts;
  int Enved_Filter_Order;
  mus_float_t Eps_Left_Margin, Eps_Bottom_Margin, Eps_Size, Log_Freq_Start;
  mus_float_t Spectro_X_Scale, Spectro_Y_Scale, Spectro_Z_Scale, Spectro_Z_Angle, Spectro_X_Angle, Spectro_Y_Angle;
  mus_float_t Spectrum_End, Spectrum_Start;
  mus_header_t Default_Output_Header_Type;
  mus_sample_t Default_Output_Sample_Type;
  int Default_Output_Chans, Default_Output_Srate;
  int Spectro_Hop, Color_Map, Color_Map_Size, Wavelet_Type, Transform_Type;
  int Dot_Size;
  int Zero_Pad, Wavo_Hop, Wavo_Trace;
  mus_long_t Transform_Size;
  mus_fft_window_t Fft_Window;
  graph_type_t Transform_Graph_Type, Time_Graph_Type;
  bool Ask_Before_Overwrite, Ask_About_Unsaved_Edits, Show_Full_Duration, Show_Full_Range, Remember_Sound_State;
  bool Save_As_Dialog_Src, Save_As_Dialog_Auto_Comment, With_Toolbar, With_Tooltips, With_Menu_Icons;
  mus_float_t Fft_Window_Alpha, Fft_Window_Beta, Grid_Density, Initial_Beg, Initial_Dur;
  mus_float_t Color_Scale, Color_Cutoff, Beats_Per_Minute;
  bool Color_Inverted, Show_Mix_Waveforms;
  int Mix_Waveform_Height, Beats_Per_Measure;
  fft_normalize_t Transform_Normalization;
  int Sinc_Width;
  x_axis_style_t X_Axis_Style;
  zoom_focus_t Zoom_Focus_Style;
  graph_style_t Graph_Style, Region_Graph_Style;
  bool Auto_Resize, Auto_Update;
  int Max_Regions, Max_Transform_Peaks;
  bool With_GL, With_Relative_Panes;
  int Print_Length, Dac_Size, View_Files_Sort;
  bool Dac_Combines_Channels, Show_Selection_Transform, With_Mix_Tags, Selection_Creates_Region;
  char *Save_State_File, *Listener_Prompt, *Stdin_Prompt;
  mus_float_t Enved_Base, Enved_Power, Auto_Update_Interval;
  bool Enved_With_Wave, Graphs_Horizontal, With_Background_Processes, With_File_Monitor;
  env_type_t Enved_Style;
  int Mix_Tag_Width, Mix_Tag_Height, Mark_Tag_Height, Mark_Tag_Width;
  enved_target_t Enved_Target;
  bool Clipping, Show_Indices, Just_Sounds;
  int Cursor_Size;
  cursor_style_t Cursor_Style, Tracking_Cursor_Style;
  bool Filter_Control_In_Db, Filter_Control_In_Hz, Show_Sonogram_Cursor;
  int Speed_Control_Tones;
  speed_style_t Speed_Control_Style;
  mus_float_t Expand_Control_Length, Expand_Control_Ramp, Expand_Control_Hop, Expand_Control_Jitter;
  mus_float_t Contrast_Control_Amp;
  mus_float_t Reverb_Control_Feedback, Reverb_Control_Lowpass;
  mus_float_t Reverb_Control_Decay, Cursor_Update_Interval;
  mus_float_t Contrast_Control_Min, Contrast_Control_Max, Expand_Control_Min, Expand_Control_Max, Speed_Control_Min, Speed_Control_Max;
  mus_float_t Amp_Control_Min, Amp_Control_Max, Reverb_Control_Scale_Min, Reverb_Control_Scale_Max;
  mus_float_t Reverb_Control_Length_Min, Reverb_Control_Length_Max;
  int Filter_Control_Order, Cursor_Location_Offset, Play_Arrow_Size;
  mus_float_t Min_dB;
  bool Show_Controls;
  tracking_cursor_t With_Tracking_Cursor;
  char *HTML_Dir, *HTML_Program, *Open_File_Dialog_Directory;

#if HAVE_SCHEME
  s7_pointer show_transform_peaks_symbol, show_y_zero_symbol, show_marks_symbol,
    show_grid_symbol,
    fft_log_frequency_symbol, fft_log_magnitude_symbol, fft_with_phases_symbol,
    channel_style_symbol,
    sync_style_symbol,
    show_axes_symbol,
    eps_file_symbol, temp_dir_symbol, save_dir_symbol, ladspa_dir_symbol, peak_env_dir_symbol,
    listener_font_symbol, axis_label_font_symbol, axis_numbers_font_symbol, tiny_font_symbol, peaks_font_symbol, bold_peaks_font_symbol,
    with_verbose_cursor_symbol, with_inset_graph_symbol, with_pointer_focus_symbol, with_smpte_label_symbol, with_interrupts_symbol,
    enved_filter_order_symbol,
    eps_left_margin_symbol, eps_bottom_margin_symbol, eps_size_symbol, log_freq_start_symbol,
    spectro_x_scale_symbol, spectro_y_scale_symbol, spectro_z_scale_symbol, spectro_z_angle_symbol, spectro_x_angle_symbol, spectro_y_angle_symbol,
    spectrum_end_symbol, spectrum_start_symbol,
    default_output_header_type_symbol, default_output_sample_type_symbol, default_output_chans_symbol, default_output_srate_symbol,
    spectro_hop_symbol, color_map_symbol, color_map_size_symbol, wavelet_type_symbol, transform_type_symbol,
    dot_size_symbol,
    zero_pad_symbol, wavo_hop_symbol, wavo_trace_symbol,
    transform_size_symbol,
    fft_window_symbol,
    transform_graph_type_symbol, time_graph_type_symbol,
    ask_before_overwrite_symbol, ask_about_unsaved_edits_symbol, show_full_duration_symbol, show_full_range_symbol, remember_sound_state_symbol,
    save_as_dialog_src_symbol, save_as_dialog_auto_comment_symbol, with_toolbar_symbol, with_tooltips_symbol, with_menu_icons_symbol,
    fft_window_alpha_symbol, fft_window_beta_symbol, grid_density_symbol, initial_beg_symbol, initial_dur_symbol,
    color_scale_symbol, color_cutoff_symbol, beats_per_minute_symbol,
    color_inverted_symbol, show_mix_waveforms_symbol,
    mix_waveform_height_symbol, beats_per_measure_symbol,
    transform_normalization_symbol,
    sinc_width_symbol,
    x_axis_style_symbol,
    zoom_focus_style_symbol,
    graph_style_symbol, region_graph_style_symbol,
    auto_resize_symbol, auto_update_symbol,
    max_regions_symbol, max_transform_peaks_symbol,
    with_gl_symbol, with_relative_panes_symbol,
    print_length_symbol, dac_size_symbol, view_files_sort_symbol,
    dac_combines_channels_symbol, show_selection_transform_symbol, with_mix_tags_symbol, selection_creates_region_symbol,
    save_state_file_symbol, listener_prompt_symbol, stdin_prompt_symbol,
    enved_base_symbol, enved_power_symbol, auto_update_interval_symbol,
    enved_with_wave_symbol, graphs_horizontal_symbol, with_background_processes_symbol, with_file_monitor_symbol,
    enved_style_symbol,
    graph_cursor_symbol, mix_tag_width_symbol, mix_tag_height_symbol, mark_tag_height_symbol, mark_tag_width_symbol,
    enved_target_symbol,
    clipping_symbol, show_indices_symbol, just_sounds_symbol,
    cursor_size_symbol,
    cursor_style_symbol, tracking_cursor_style_symbol,
    filter_control_in_db_symbol, filter_control_in_hz_symbol, show_sonogram_cursor_symbol,
    speed_control_tones_symbol,
    speed_control_style_symbol,
    expand_control_length_symbol, expand_control_ramp_symbol, expand_control_hop_symbol, expand_control_jitter_symbol,
    contrast_control_amp_symbol,
    reverb_control_feedback_symbol, reverb_control_lowpass_symbol,
    reverb_control_decay_symbol, cursor_update_interval_symbol,
    filter_control_order_symbol, cursor_location_offset_symbol, play_arrow_size_symbol,
    min_db_symbol,
    show_controls_symbol,
    with_tracking_cursor_symbol,
#if USE_GTK
    listener_colorized_symbol,
#endif
    html_dir_symbol, html_program_symbol, open_file_dialog_directory_symbol;
#endif

#if USE_GTK
#if GTK_CHECK_VERSION(3, 94, 0)
  char* Graph_Cursor;
#else
  GdkCursorType Graph_Cursor;
#endif
#else
  int Graph_Cursor;
#endif

  bool tracking;
  Xen cursor_proc;
  int cursor_proc_loc, listener_prompt_length;
  Xen zoom_focus_proc;
  int zoom_focus_proc_loc;
  mus_float_t lin_dB;
  char *io_error_info;
  int deferred_regions;
  open_requestor_t open_requestor;
  void *open_requestor_data;
  bool batch_mode;
  bool jump_ok, exiting;
  env_editor *enved;
  oclock_t click_time;
  bool file_monitor_ok, C_g_typed, squelch_mark_drag_info;
  void (*snd_error_handler)(const char *error_msg, void *data);
  void *snd_error_data;
  void (*snd_warning_handler)(const char *warning_msg, void *data);
  void *snd_warning_data;
  void (*xen_error_handler)(const char *error_msg, void *data);
  void *xen_error_data;
  void (*snd_print_handler)(const char *msg, void *data);
  void *snd_print_data;
  channel_style_t update_sound_channel_style;
#if HAVE_GL && WITH_GL2PS
  bool gl_printing;
#endif
  Xen mus_error_hook;
  Xen snd_error_hook; 
  Xen snd_warning_hook; 
  Xen snd_open_file_hook;
  Xen effects_hook;

#if USE_MOTIF
  XtAppContext mainapp;     
  Widget mainshell;
  Widget mainpane;
  Widget soundpane;
  Widget soundpanebox;
  Widget toolbar;
  Display *mdpy;
  xm_font_t peaks_fontlist;
  XFontStruct *peaks_fontstruct;
  xm_font_t bold_peaks_fontlist;
  XFontStruct *bold_peaks_fontstruct; 
  xm_font_t listener_fontlist;
  XFontStruct *listener_fontstruct;
  XFontStruct *axis_label_fontstruct;
  XFontStruct *axis_numbers_fontstruct;
  xm_font_t tiny_fontlist;
  XFontStruct *tiny_fontstruct;

  Pixel white, black, red, yellow, green, blue, light_blue, lighter_blue;
  Pixel data_color, selected_data_color, mark_color, graph_color, selected_graph_color, listener_color, listener_text_color;
  Pixel basic_color, selection_color, zoom_color, position_color, highlight_color, enved_waveform_color, cursor_color;
  Pixel text_focus_color, filter_control_waveform_color, mix_color, sash_color;
  Pixel selected_grid_color, grid_color, axis_color;
  Pixel orig_data_color, orig_selected_data_color, orig_mark_color, orig_mix_color;
  Pixel orig_graph_color, orig_selected_graph_color, orig_listener_color, orig_listener_text_color, orig_cursor_color;
  Pixel orig_basic_color, orig_selection_color, orig_zoom_color, orig_position_color, orig_highlight_color;

  GC basic_gc, selected_basic_gc, combined_basic_gc;        
  GC cursor_gc, selected_cursor_gc;      
  GC selection_gc, selected_selection_gc;
  GC erase_gc, selected_erase_gc;        
  GC mark_gc, selected_mark_gc;          
  GC mix_gc;
  GC fltenv_basic_gc, fltenv_data_gc;
  Widget listener_pane;
  Widget *dialogs;
  int num_dialogs, dialogs_size;
  Cursor graph_cursor, wait_cursor, bounds_cursor, play_cursor, loop_play_cursor, yaxis_cursor;
  Widget requestor_dialog;
#if HAVE_GL
#if USE_MOTIF
  GLXContext cx;
#endif
#endif
  Widget *mw;
  bool axis_color_set;
#endif

#if USE_GTK
  cairo_t *cr;
  double line_width;
  GtkWidget *mainshell;
  GtkWidget *mainpane;
  GtkWidget *soundpane;
  GtkWidget *soundpanebox;
  GtkWidget *listener_pane;
  GdkWindow *mainwindow;

  PangoFontDescription *listener_fnt;
  PangoFontDescription *axis_label_fnt;
  PangoFontDescription *axis_numbers_fnt;
  PangoFontDescription *tiny_fnt;
  PangoFontDescription *peaks_fnt;
  PangoFontDescription *bold_peaks_fnt; 

  color_info *white, *black, *red, *yellow, *green, *blue, *light_blue, *lighter_blue;
  color_info *data_color, *selected_data_color, *mark_color, *graph_color, *selected_graph_color, *listener_color, *listener_text_color, *cursor_color;
  color_info *basic_color, *selection_color, *zoom_color, *position_color, *highlight_color, *enved_waveform_color;
  color_info *text_focus_color, *filter_control_waveform_color, *mix_color, *sash_color;
  color_info *selected_grid_color, *grid_color, *axis_color;
  color_info *orig_data_color, *orig_selected_data_color, *orig_mark_color, *orig_mix_color;
  color_info *orig_graph_color, *orig_selected_graph_color, *orig_listener_color, *orig_listener_text_color, *orig_cursor_color;
  color_info *orig_basic_color, *orig_selection_color, *orig_zoom_color, *orig_position_color, *orig_highlight_color;

  gc_t *basic_gc, *selected_basic_gc, *combined_basic_gc;        
  gc_t *cursor_gc, *selected_cursor_gc;      
  gc_t *selection_gc, *selected_selection_gc;
  gc_t *erase_gc, *selected_erase_gc;        
  gc_t *mark_gc, *selected_mark_gc;          
  gc_t *mix_gc;
  gc_t *fltenv_basic_gc, *fltenv_data_gc;

  GtkWidget **dialogs;
  int num_dialogs, dialogs_size;
  bool graph_is_active;
  GtkWidget *requestor_dialog;
  mus_float_t bg_gradient;
  
  GdkCursor *arrow_cursor, *wait_cursor, *graph_cursor, *bounds_cursor, *play_cursor, *loop_play_cursor, *yaxis_cursor;
  GtkWidget **mw;
  bool axis_color_set;

  glistener *listener;
#endif

#if USE_NO_GUI
  int data_color, selected_data_color, mix_color, basic_color, grid_color, selected_grid_color, mark_color, axis_color;
  int white, black, red, yellow, green, blue, light_blue;
  int fltenv_basic_gc, fltenv_data_gc;
  int basic_gc, selected_basic_gc, combined_basic_gc;        
  int cursor_gc, selected_cursor_gc;      
  int selection_gc, selected_selection_gc;
  int erase_gc, selected_erase_gc;        
  int mark_gc, selected_mark_gc;          
  int mix_gc;           
  void *ignore_me;
  int requestor_dialog;
  bool axis_color_set;
  int bounds_cursor, graph_cursor, play_cursor, loop_play_cursor, yaxis_cursor;
#endif
#if HAVE_SCHEME
  s7_pointer data_color_symbol, selected_data_color_symbol, mark_color_symbol, graph_color_symbol, 
    selected_graph_color_symbol, listener_color_symbol, listener_text_color_symbol,
    basic_color_symbol, selection_color_symbol, zoom_color_symbol, position_color_symbol, 
    highlight_color_symbol, enved_waveform_color_symbol, cursor_color_symbol,
    text_focus_color_symbol, filter_control_waveform_color_symbol, mix_color_symbol, 
    sash_color_symbol, axis_color_symbol;
#endif
} snd_state;

extern snd_state *ss;

typedef struct {
  int chans;
  mus_long_t *begs;
  chan_info **cps;
} sync_info;

typedef struct {
  int len;
  char **name;
} region_state;

typedef struct {
  char *key;
  bool c, m, x;
} key_info;



/* -------- snd-io.c -------- */

int snd_creat(const char *filename, mode_t mode);
FILE *snd_fopen(const char *filename, const char *modes);
int snd_open(const char *filename, int flags, mode_t mode);

void snd_remove(const char *name, cache_remove_t forget);
void snd_close(int fd, const char *name);
void snd_fclose(FILE *fd, const char *name);
io_error_t copy_file(const char *oldname, const char *newname);
io_error_t move_file(const char *oldfile, const char *newfile);

int snd_open_read(const char *arg);
int snd_reopen_write(const char *arg);
io_error_t snd_write_header(const char *name, mus_header_t type, int srate, int chans, mus_long_t samples, 
			    mus_sample_t sample_type, const char *comment, int *loops);
io_error_t sndlib_error_to_snd(int sndlib_err);
int snd_file_open_descriptors(int tfd, const char *name, mus_sample_t sample_type, mus_long_t location, int chans, mus_header_t type);
snd_io *make_file_state(int fd, file_info *hdr, int chan, mus_long_t beg, int suggested_bufsize);
void file_buffers_forward(mus_long_t ind0, mus_long_t ind1, mus_long_t indx, snd_fd *sf, snd_data *cur_snd);
void file_buffers_back(mus_long_t ind0, mus_long_t ind1, mus_long_t indx, snd_fd *sf, snd_data *cur_snd);

void remember_temp(const char *filename, int chans);
void forget_temp(const char *filename, int chan);
void forget_temps(void);

snd_data *make_snd_data_file(const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int ctr, int temp_chan);
snd_data *copy_snd_data(snd_data *sd, mus_long_t beg, int bufsize);
snd_data *free_snd_data(snd_data *sf);
snd_data *make_snd_data_buffer(mus_float_t *data, int len, int ctr);
snd_data *make_snd_data_buffer_for_simple_channel(int len);
int open_temp_file(const char *ofile, int chans, file_info *hdr, io_error_t *err);
io_error_t close_temp_file(const char *filename, int ofd, mus_header_t type, mus_long_t bytes);

void set_up_snd_io(chan_info *cp, int i, int fd, const char *filename, file_info *hdr, bool post_close);
mus_long_t io_beg(snd_io *io);
mus_long_t io_end(snd_io *io);



/* -------- snd-help.c -------- */

void about_snd_help(void);
void controls_help(void);
void fft_help(void);
void find_help(void);
void undo_help(void);
void sync_help(void);
void debug_help(void);
void env_help(void);
void marks_help(void);
void mix_help(void);
void sound_files_help(void);
void init_file_help(void);
void region_help(void);
void selection_help(void);
void colors_help(void);
char *version_info(void);
void transform_dialog_help(void);
void color_orientation_dialog_help(void);
void envelope_editor_dialog_help(void);
void region_dialog_help(void);
void raw_data_dialog_help(const char *info);
void new_file_dialog_help(void);
void edit_header_dialog_help(void);
void print_dialog_help(void);
void view_files_dialog_help(void);
void mix_dialog_help(void);
void find_dialog_help(void);
void open_file_dialog_help(void);
void mix_file_dialog_help(void);
void insert_file_dialog_help(void);
void save_as_dialog_help(void);
char* word_wrap(const char *text, int widget_len);
void g_init_help(void);
Xen g_snd_help_with_search(Xen text, int widget_wid, bool search);
Xen g_snd_help(Xen text, int widget_wid);
const char *snd_url(const char *name);
void set_html_dir(char *new_dir);
void key_help(void);
void play_help(void);
void save_help(void);
void reverb_help(void);
void resample_help(void);
void filter_help(void);
void insert_help(void);
void delete_help(void);
void name_to_html_viewer(const char *red_text);
void url_to_html_viewer(const char *url);
bool snd_topic_help(const char *topic);
const char **help_name_to_xrefs(const char *name);



/* -------- snd-menu.c -------- */

void reflect_file_revert_in_label(snd_info *sp);
void set_menu_label(widget_t w, const char *label);
void g_init_menu(void);


/* -------- snd-main.c -------- */

const char *save_options_in_prefs(void);
void open_save_sound_block(snd_info *sp, FILE *fd, bool with_nth);
void close_save_sound_block(FILE *fd, bool need_f);
void save_sound_state(snd_info *sp, void *ptr);
bool snd_exit_cleanly(bool force_exit);
void sound_not_current(snd_info *sp);
void save_state(const char *save_state_name);
void global_control_panel_state(void);
void global_fft_state(void);
int handle_next_startup_arg(int auto_open_ctr, char **auto_open_file_names, bool with_title, int args);

void g_init_main(void);


/* -------- snd-completion.c -------- */

char *expression_completer(widget_t w, const char *text, void *data);
void preload_best_completions(void);
void save_completion_choice(const char *selection);
int find_best_completion(char **choices, int num_choices);
int add_completer_func(char *(*func)(widget_t w, const char *text, void *context), void *data);
int add_completer_func_with_multicompleter(char *(*func)(widget_t w, const char *text, void *context), void *data, void (*multi_func)(widget_t w, void *data));
int get_completion_matches(void);
void set_completion_matches(int matches);
void set_save_completions(bool save);
void add_possible_completion(const char *text);
int get_possible_completions_size(void);
char **get_possible_completions(void);
void clear_possible_completions(void);
void handle_completions(widget_t w, int completer);
char *complete_text(widget_t w, const char *text, int func);
char *filename_completer(widget_t w, const char *text, void *data);
char *sound_filename_completer(widget_t w, const char *text, void *data);
char *srate_completer(widget_t w, const char *text, void *data);
char *list_completer(widget_t w, const char *text, void *data);
char *complete_listener_text(char *old_text, int end, bool *try_completion, char **to_file_text);
void add_srate_to_completion_list(int srate);
char *direct_completions(const char *str);
#if HAVE_FORTH || HAVE_RUBY
  void call_read_hook_or_eval(const char *text);
#endif



/* -------- snd-print.c -------- */

void ps_set_grf_points(double x, int j, mus_float_t ymin, mus_float_t ymax);
void ps_set_grf_point(double x, int j, mus_float_t y);
void ps_allocate_grf_points(void);
void ps_draw_grf_points(axis_info *ap, int j, mus_float_t y0, graph_style_t graph_style, int dot_size);
void ps_draw_both_grf_points(axis_info *ap, int j, graph_style_t graph_style, int dot_size);
void ps_draw_sono_rectangle(axis_info *ap, int color, mus_float_t x, mus_float_t y, mus_float_t width, mus_float_t height);
void ps_reset_color(void);
void ps_bg(axis_info *ap, graphics_context *ax);
void ps_fg(chan_info *cp, graphics_context *ax);
void ps_draw_line(axis_info *ap, int x0, int y0, int x1, int y1);
void ps_draw_spectro_line(axis_info *ap, int color, mus_float_t x0, mus_float_t y0, mus_float_t x1, mus_float_t y1);
void ps_fill_rectangle(axis_info *ap, int x0, int y0, int width, int height);
void ps_draw_string(axis_info *ap, int x0, int y0, const char *str);
void ps_set_number_font(void);
void ps_set_label_font(void);
void ps_set_bold_peak_numbers_font(void);
void ps_set_peak_numbers_font(void);
void ps_set_tiny_numbers_font(void);
bool snd_print(const char *output);
void print_enved(const char *output, int y0);
void g_init_print(void);



/* -------- snd-marks.c -------- */

int mark_sync_max(void);
void set_mark_sync(mark *m, int val);
int mark_to_int(mark *m);
int mark_sync(mark *m);
mus_long_t mark_sample(mark *m);
void marks_off(chan_info *cp);
mark *hit_mark(chan_info *cp, int x, int y);
void set_mark_control(chan_info *cp, mark *mp, int key_state);
mark *hit_mark_triangle(chan_info *cp, int x, int y);
void move_mark(chan_info *cp, mark *mp, int x);
void play_syncd_mark(chan_info *cp, mark *mp);
void finish_moving_mark(chan_info *cp, mark *m);
mark *add_mark(mus_long_t samp, const char *name, chan_info *cp);
bool delete_mark_samp(mus_long_t samp, chan_info *cp);
void free_mark_list(ed_list *ed);
bool goto_mark(chan_info *cp, int count);
mark *active_mark(chan_info *cp);
mus_long_t mark_beg(chan_info *cp);
void display_channel_marks(chan_info *cp);
void ripple_marks(chan_info *cp, mus_long_t beg, mus_long_t change);
bool mark_define_region(chan_info *cp, int count);
void save_mark_list(FILE *fd, chan_info *cp, bool all_chans);
void reverse_marks(chan_info *cp, mus_long_t beg, mus_long_t dur);
void src_marks(chan_info *cp, mus_float_t ratio, mus_long_t old_samps, mus_long_t new_samps, mus_long_t beg, bool over_selection);
void reset_marks(chan_info *cp, int cur_marks, mus_long_t *samps, mus_long_t end, mus_long_t extension, bool over_selection);
void ripple_trailing_marks(chan_info *cp, mus_long_t beg, mus_long_t old_len, mus_long_t new_len);
void swap_marks(chan_info *cp0, chan_info *cp1);
void g_init_marks(void);
void *sound_store_marks(snd_info *sp);
void sound_restore_marks(snd_info *sp, void *marks);
mus_long_t mark_id_to_sample(int id);

Xen new_xen_mark(int n);
bool xen_is_mark(Xen obj);
int xen_mark_to_int(Xen n);
#define Xen_mark_to_C_int(n) xen_mark_to_int(n)
Xen g_mark_sync(Xen mark_n);
Xen g_set_mark_sync(Xen mark_n, Xen sync_n);
 


/* -------- snd-data.c -------- */

chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound);
snd_info *make_snd_info(snd_info *sip, const char *filename, file_info *hdr, int snd_slot, read_only_t read_only);
snd_info *make_basic_snd_info(int chans);
void initialize_control_panel(snd_info *sp);
void free_snd_info(snd_info *sp);
snd_info *completely_free_snd_info(snd_info *sp);
void for_each_chan_with_int(void (*func)(chan_info *ncp, int val), int value);
void for_each_chan_with_mus_long_t(void (*func)(chan_info *ncp, mus_long_t val), mus_long_t value);
void for_each_chan_with_bool(void (*func)(chan_info *ncp, bool val), bool value);
void for_each_chan_with_float(void (*func)(chan_info *ncp, mus_float_t val), mus_float_t value);
void for_each_chan(void (*func)(chan_info *ncp));
void for_each_normal_chan(void (*func)(chan_info *ncp));
void for_each_normal_chan_with_void(void (*func)(chan_info *ncp, void *ptr), void *userptr);
void for_each_normal_chan_with_int(void (*func)(chan_info *ncp, int val), int value);
void for_each_normal_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value);
void for_each_sound_chan(snd_info *sp, void (*func)(chan_info *ncp));
void for_each_sound_chan_with_int(snd_info *sp, void (*func)(chan_info *ncp, int val1), int value);
void for_each_sound(void (*func)(snd_info *usp));
void for_each_sound_with_void(void (*func)(snd_info *usp, void *ptr), void *userptr);
void for_each_separate_chan(void (*func)(chan_info *ncp));
bool map_over_sound_chans(snd_info *sp, bool (*func)(chan_info *ncp));
bool snd_ok(snd_info *sp);
int active_channels(virtual_channels_t count_virtual_channels);
int syncd_channels(int sync);
int find_free_sound_slot(int desired_chans);
int find_free_sound_slot_for_channel_display(void);
snd_info *selected_sound(void);
chan_info *selected_channel(void);
chan_info *color_selected_channel(snd_info *sp);
snd_info *any_selected_sound(void);
chan_info *any_selected_channel(snd_info *sp);
void select_channel(snd_info *sp, int chan);
chan_info *current_channel(void);
sync_info *free_sync_info(sync_info *si);
sync_info *snd_sync(int sync);
sync_info *sync_to_chan(chan_info *cp);
sync_info *make_simple_sync(chan_info *cp, mus_long_t beg);
snd_info *find_sound(const char *name, int nth);
void mix_display_during_drag(int mix_id, mus_long_t drag_beg, mus_long_t drag_end);
void g_init_data(void);



/* -------- snd-edits.c -------- */

mus_float_t channel_maxamp(chan_info *cp, int edpos);
mus_long_t channel_maxamp_position(chan_info *cp, int edpos);
mus_float_t channel_maxamp_and_position(chan_info *cp, int edpos, mus_long_t *maxpos);
ed_list *initial_ed_list(mus_long_t beg, mus_long_t end);
snd_info *sound_is_silence(snd_info *sp);
mus_long_t edit_changes_begin_at(chan_info *cp, int edpos);
mus_long_t edit_changes_end_at(chan_info *cp, int edpos);
bool has_unsaved_edits(snd_info *sp);
char *run_save_state_hook(const char *filename);
void edit_history_to_file(FILE *fd, chan_info *cp, bool with_save_state_hook);
char *edit_to_string(chan_info *cp, int edit);
void free_edit_list(chan_info *cp);
void backup_edit_list(chan_info *cp);
void as_one_edit(chan_info *cp, int one_edit);
void free_sound_list(chan_info *cp);
void after_edit(chan_info *cp);
bool extend_with_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos, const char *origin);
bool insert_samples(mus_long_t beg, mus_long_t num, mus_float_t *vals, chan_info *cp, const char *origin, int edpos);
bool file_insert_samples(mus_long_t beg, mus_long_t num, const char *tempfile, chan_info *cp, int chan, 
			 file_delete_t auto_delete, const char *origin, int edpos);
bool insert_complete_file_at_cursor(snd_info *sp, const char *filename);
bool insert_complete_file(snd_info *sp, const char *str, mus_long_t chan_beg, file_delete_t auto_delete);
bool delete_samples(mus_long_t beg, mus_long_t num, chan_info *cp, int edpos);
bool change_samples(mus_long_t beg, mus_long_t num, mus_float_t *vals, chan_info *cp, const char *origin, int edpos, mus_float_t mx);
bool file_change_samples(mus_long_t beg, mus_long_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos);
bool file_override_samples(mus_long_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin);
mus_float_t chn_sample(mus_long_t samp, chan_info *cp, int pos);
void check_saved_temp_file(const char *type, Xen filename, Xen date_and_length);
bool is_editable(chan_info *cp);
file_delete_t xen_to_file_delete_t(Xen auto_delete, const char *caller);
snd_fd *free_snd_fd(snd_fd *sf);
char *sampler_to_string(snd_fd *fd);
bool is_sampler(Xen obj);
snd_fd *xen_to_sampler(Xen obj);
snd_fd *free_snd_fd_almost(snd_fd *sf);
bool scale_channel(chan_info *cp, mus_float_t scaler, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit);
bool scale_channel_with_origin(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, const char *origin);
bool ramp_channel(chan_info *cp, double start, double incr, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit);
bool xramp_channel(chan_info *cp, double start, double incr, double scaler, double offset, 
		   mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, mus_any *e, int xramp_seg_loc);
snd_fd *init_sample_read(mus_long_t samp, chan_info *cp, read_direction_t direction);
snd_fd *init_sample_read_any(mus_long_t samp, chan_info *cp, read_direction_t direction, int edit_position);
snd_fd *init_sample_read_any_with_bufsize(mus_long_t samp, chan_info *cp, read_direction_t direction, int edit_position, int bufsize);
void read_sample_change_direction(snd_fd *sf, read_direction_t dir);
void sampler_set_safe(snd_fd *sf, mus_long_t dur); 
bool unrampable(chan_info *cp, mus_long_t beg, mus_long_t dur, int pos, bool is_xramp);
bool sound_fragments_in_use(chan_info *cp, int pos);
#define read_sample(Sf) (*((Sf)->runf))(Sf)
mus_float_t channel_local_maxamp(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos, mus_long_t *maxpos);
bool undo_edit_with_sync(chan_info *cp, int count);
bool redo_edit_with_sync(chan_info *cp, int count);
bool undo_edit(chan_info *cp, int count);
bool redo_edit(chan_info *cp, int count);
io_error_t save_channel_edits(chan_info *cp, const char *ofile, int pos);
io_error_t channel_to_file_with_settings(chan_info *cp, const char *new_name, int srate, 
					 mus_sample_t samp_type, mus_header_t hd_type, const char *comment, int pos);
io_error_t save_edits(snd_info *sp);
io_error_t save_edits_without_asking(snd_info *sp);
io_error_t save_edits_and_update_display(snd_info *sp);
io_error_t save_edits_without_display(snd_info *sp, const char *new_name, mus_header_t type, 
				      mus_sample_t sample_type, int srate, const char *comment, int pos);
void revert_edits(chan_info *cp);
mus_long_t current_location(snd_fd *sf);
void g_init_edits(void);
void set_ed_maxamp(chan_info *cp, int edpos, mus_float_t val);
mus_float_t ed_maxamp(chan_info *cp, int edpos);
void set_ed_maxamp_position(chan_info *cp, int edpos, mus_long_t val);
mus_long_t ed_maxamp_position(chan_info *cp, int edpos);
void set_ed_selection_maxamp(chan_info *cp, mus_float_t val);
mus_float_t ed_selection_maxamp(chan_info *cp);
void set_ed_selection_maxamp_position(chan_info *cp, mus_long_t val);
mus_long_t ed_selection_maxamp_position(chan_info *cp);
void copy_then_swap_channels(chan_info *cp0, chan_info *cp1, int pos0, int pos1);
void reflect_file_change_in_label(chan_info *cp);
void reflect_file_change_in_title(void);
int mix_buffer_with_tag(chan_info *cp, mus_float_t *data, mus_long_t beg, mus_long_t num, const char *origin);

int mix_file_with_tag(chan_info *cp, const char *filename, int chan, mus_long_t beg, file_delete_t auto_delete, const char *origin);
void unmix(chan_info *cp, mix_state *ms);
void remix(chan_info *cp, mix_state *ms);
snd_fd *make_virtual_mix_reader(chan_info *cp, mus_long_t beg, mus_long_t len, int index, mus_float_t scl, read_direction_t direction);
bool virtual_mix_ok(chan_info *cp, int edpos);
bool begin_mix_op(chan_info *cp, mus_long_t old_beg, mus_long_t old_len, mus_long_t new_beg, mus_long_t new_len, int edpos, const char *caller);
void end_mix_op(chan_info *cp, mus_long_t old_beg, mus_long_t old_len);
void prepare_sound_list(chan_info *cp);
Xen g_sampler_file_name(Xen obj);
char *edit_list_to_function(chan_info *cp, int start_pos, int end_pos);
vct *samples_to_vct(mus_long_t beg, mus_long_t len, chan_info *cp, int pos, mus_float_t *buf, snd_fd *reader);
vct *samples_to_vct_with_reader(mus_long_t len, mus_float_t *buf, snd_fd *reader);



/* -------- snd-fft.c -------- */

int find_and_sort_transform_peaks(mus_float_t *buf, fft_peak *found, int num_peaks, mus_long_t losamp, mus_long_t hisamp, mus_float_t samps_per_pixel, mus_float_t fft_scale);
int find_and_sort_peaks(mus_float_t *buf, fft_peak *found, int num_peaks, mus_long_t losamp, mus_long_t hisamp);
fft_info *free_fft_info(fft_info *fp);
void free_sonogram_fft_state(void *ptr);
bool fft_window_beta_in_use(mus_fft_window_t win);
bool fft_window_alpha_in_use(mus_fft_window_t win);
void free_sono_info(chan_info *cp);
void sono_update(chan_info *cp);
void c_convolve(const char *fname, mus_float_t amp, int filec, mus_long_t filehdr, int filterc, mus_long_t filterhdr, mus_long_t filtersize,
		mus_long_t fftsize, int filter_chans, int filter_chan, mus_long_t data_size, snd_info *gsp);
void *make_sonogram_state(chan_info *cp, bool force_recalc);
void single_fft(chan_info *cp, bool update_display, bool force_recalc);
idle_func_t sonogram_in_slices(void *sono);
void clear_transform_edit_ctrs(chan_info *cp);
void g_init_fft(void);
mus_float_t fft_beta_max(mus_fft_window_t win);
void cp_free_fft_state(chan_info *cp);
void set_fft_info_xlabel(chan_info *cp, const char *new_label);
void fourier_spectrum(snd_fd *sf, mus_float_t *data, mus_long_t fft_size, mus_long_t data_len, mus_float_t *window, chan_info *cp);
const char *wavelet_name(int i);
const char **wavelet_names(void);
void set_log_freq_start(mus_float_t base);

const char *transform_name(int type);
const char *transform_program_name(int type);
int transform_position_to_type(int pos);
int transform_type_to_position(int type);
int max_transform_type(void);
void set_transform_position(int i, int j);
bool is_transform(int type);

Xen new_xen_transform(int n);
bool xen_is_transform(Xen obj);
int xen_transform_to_int(Xen n);
#define C_int_to_Xen_transform(Val) new_xen_transform(Val)
#define Xen_transform_to_C_int(n) xen_transform_to_int(n)



/* -------- snd-xen.c -------- */

const char *io_error_name(io_error_t err);
#ifdef __GNUC__
  void snd_error(const char *format, ...)  __attribute__ ((format (printf, 1, 2)));
  void snd_warning(const char *format, ...)  __attribute__ ((format (printf, 1, 2)));
#else
  void snd_error(const char *format, ...);
  void snd_warning(const char *format, ...);
#endif
void snd_error_without_format(const char *msg);
void snd_warning_without_format(const char *msg);
void redirect_snd_error_to(void (*handler)(const char *error_msg, void *ufd), void *data);
void redirect_snd_warning_to(void (*handler)(const char *warning_msg, void *ufd), void *data);

char *stdin_check_for_full_expression(const char *newstr);
void stdin_free_str(void);

void redirect_xen_error_to(void (*handler)(const char *msg, void *ufd), void *data);
void redirect_errors_to(void (*handler)(const char *msg, void *ufd), void *data);
void redirect_everything_to(void (*handler)(const char *msg, void *ufd), void *data);
Xen snd_catch_any(Xen_catch_t body, void *body_data, const char *caller);
Xen snd_no_such_file_error(const char *caller, Xen filename);
Xen snd_no_such_channel_error(const char *caller, Xen snd, Xen chn);
Xen snd_bad_arity_error(const char *caller, Xen errstr, Xen proc);
Xen snd_no_active_selection_error(const char *caller);
void g_xen_initialize(void);
Xen eval_str_wrapper(void *data);
Xen g_c_make_sampler(snd_fd *fd);
char *procedure_ok(Xen proc, int args, const char *caller, const char *arg_name, int argn);
bool procedure_arity_ok(Xen proc, int args);
int snd_protect(Xen obj);
void snd_unprotect_at(int loc);

Xen run_or_hook(Xen hook, Xen args, const char *caller);
Xen run_progn_hook(Xen hook, Xen args, const char *caller);
Xen run_hook(Xen hook, Xen args, const char *caller);
void check_features_list(const char *features);
#if (!USE_NO_GUI)
  mus_float_t check_color_range(const char *caller, Xen val);
#endif
void set_basic_color(color_t color);
void set_highlight_color(color_t color);
void set_position_color(color_t color);
void set_zoom_color(color_t color);
void set_data_color(color_t color);
void set_selected_data_color(color_t color);
void set_graph_color(color_t color);
void set_selected_graph_color(color_t color);
mus_float_t string_to_mus_float_t(const char *str, mus_float_t lo, const char *file_name);
int string_to_int(const char *str, int lo, const char *field_name);
mus_long_t string_to_mus_long_t(const char *str, mus_long_t lo, const char *field_name);
char *output_comment(file_info *hdr);
void snd_load_init_file(bool nog, bool noi);
void snd_load_file(const char *filename);
void snd_display_result(const char *str, const char *endstr);
void snd_report_result(Xen result, const char *buf);
void snd_report_listener_result(Xen form);
void snd_eval_stdin_str(const char *buf);
void clear_stdin(void);
#if HAVE_RUBY
  void snd_rb_raise(Xen type, Xen info);
#endif
bool is_source_file(const char *name);
void save_added_source_file_extensions(FILE *fd);



/* -------- snd-select.c -------- */

bool selection_is_active(void);
bool selection_is_active_in_channel(chan_info *cp);
bool selection_is_visible_in_channel(chan_info *cp);
mus_long_t selection_beg(chan_info *cp);
mus_long_t selection_end(chan_info *cp);
mus_long_t selection_len(void);
int selection_chans(void);
int selection_srate(void);
mus_float_t selection_maxamp(chan_info *cp);
void deactivate_selection(void);
void reactivate_selection(chan_info *cp, mus_long_t beg, mus_long_t end);
void ripple_selection(ed_list *new_ed, mus_long_t beg, mus_long_t num);
sync_info *selection_sync(void);
void start_selection_creation(chan_info *cp, mus_long_t samp);
void update_possible_selection_in_progress(mus_long_t samp);
void restart_selection_creation(chan_info *cp, bool right);
bool hit_selection_triangle(chan_info *cp, int x, int y);
bool hit_selection_loop_triangle(chan_info *cp, int x, int y);
void cp_delete_selection(chan_info *cp);

int make_region_from_selection(void);
void display_selection(chan_info *cp);
bool delete_selection(cut_selection_regraph_t regraph);
void move_selection(chan_info *cp, int x);
void finish_selection_creation(void);
int select_all(chan_info *cp);
io_error_t save_selection(const char *ofile, int srate, mus_sample_t samp_type, mus_header_t head_type, const char *comment, int chan);
bool selection_creation_in_progress(void);
void add_selection_or_region(int reg, chan_info *cp);
void insert_selection_from_menu(void);
void cancel_selection_watch(void);
void show_selection(void);
bool xen_is_selection(Xen obj);
Xen g_selection_chans(void);
Xen g_selection_srate(void);
Xen g_selection_maxamp(Xen snd, Xen chn);
Xen g_selection_framples(Xen snd, Xen chn);

void g_init_selection(void);
  

/* -------- snd-region.c -------- */

void allocate_regions(int numreg);
bool region_ok(int n);
int region_chans(int n);
int region_srate(int n);
const char *region_file_name(int n);
mus_long_t region_len(int n);
mus_float_t region_maxamp(int n);
int region_list_position_to_id(int n);
int region_id_to_list_position(int id);
file_info *fixup_region_data(chan_info *cp, int chan, int n);
region_state *region_report(void);
void free_region_state(region_state *r);
int remove_region_from_list(int pos);
io_error_t paste_region(int n, chan_info *cp);
io_error_t add_region(int n, chan_info *cp);
int define_region(sync_info *si, mus_long_t *ends);
snd_fd *init_region_read(mus_long_t beg, int n, int chan, read_direction_t direction);
void cleanup_region_temp_files(void);
int snd_regions(void);
void save_regions(FILE *fd);
io_error_t save_region(int rg, const char *name, mus_sample_t samp_type, mus_header_t head_type, const char *comment);
void region_edit(int reg);
void clear_region_backpointer(snd_info *sp);
void save_region_backpointer(snd_info *sp);
void sequester_deferred_regions(chan_info *cp, int edit_top);
void g_init_regions(void);
void for_each_region_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value);
mus_long_t region_current_location(snd_fd *fd);
char *region_description(int rg);

Xen new_xen_region(int n);
bool xen_is_region(Xen obj);
int xen_region_to_int(Xen n);
#define C_int_to_Xen_region(Val) new_xen_region(Val)
#define Xen_region_to_C_int(n) xen_region_to_int(n)
Xen g_region_srate(Xen n);
Xen g_region_chans(Xen n);
Xen g_region_framples(Xen n, Xen chan);
Xen g_region_maxamp(Xen n);
Xen g_play_region(Xen n, play_process_t back, Xen stop_proc);
Xen g_region_to_vct(Xen reg_n, Xen beg_n, Xen num, Xen chn_n, Xen v);



/* -------- snd-env.c -------- */

env *copy_env(env *e);
env *free_env(env *e);
char *env_to_string(env *e);
env *make_envelope_with_offset_and_scaler(mus_float_t *env_buffer, int len, mus_float_t offset, mus_float_t scaler);
env *default_env(mus_float_t x1, mus_float_t y);
bool is_default_env(env *e);
bool envs_equal(env *e1, env *e2);
env_editor *new_env_editor(void);
void env_editor_button_motion_with_xy(env_editor *edp, int evx, int evy, oclock_t motion_time, env *e, mus_float_t *new_x, mus_float_t *new_y);
void env_editor_button_motion(env_editor *edp, int evx, int evy, oclock_t motion_time, env *e);
bool env_editor_button_press(env_editor *edp, int evx, int evy, oclock_t time, env *e);
void env_editor_button_release(env_editor *edp, env *e);
double env_editor_ungrf_y_dB(env_editor *edp, int y);
void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax, printing_t printing);
void env_editor_display_env(env_editor *edp, env *e, graphics_context *ax, const char *name, 
			    int x0, int y0, int width, int height, printing_t printing);
void view_envs(int env_window_width, int env_window_height, printing_t printing);
int hit_env(int xe, int ye, int env_window_width, int env_window_height);
void prepare_enved_edit(env *new_env);
void redo_env_edit(void);
void undo_env_edit(void);
void revert_env_edit(void);
int enved_all_envs_top(void);
char *enved_all_names(int n);
void set_enved_env_list_top(int n);
/* env *enved_all_envs(int pos); */
void alert_envelope_editor(const char *name, env *val);
void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, printing_t printing);
void save_envelope_editor_state(FILE *fd);
char *env_name_completer(widget_t w, const char *text, void *data);
env *enved_next_env(void);
env *string_to_env(const char *str);
void add_or_edit_symbol(const char *name, env *val);
env* name_to_env(const char *str);
env *position_to_env(int pos);
/* void delete_envelope(const char *name); */
enved_fft *free_enved_fft(enved_fft *ef);
void reflect_enved_fft_change(chan_info *cp);

Xen env_to_xen(env *e);
env *xen_to_env(Xen res);
env *get_env(Xen e, const char *origin);
void g_init_env(void);


/* -------- snd-dac.c -------- */

void cleanup_dac(void);
void stop_playing_sound(snd_info *sp, play_stop_t reason);
void stop_playing_sound_without_hook(snd_info *sp, play_stop_t reason);
void stop_playing_sound_no_toggle(snd_info *sp, play_stop_t reason);
void stop_playing_all_sounds(play_stop_t reason);
void stop_playing_region(int n, play_stop_t reason);
void play_region(int n, play_process_t background);
void play_region_1(int region, play_process_t background, Xen stop_proc);
void play_channel(chan_info *cp, mus_long_t start, mus_long_t end);
void play_channel_with_sync(chan_info *cp, mus_long_t start, mus_long_t end);
void play_sound(snd_info *sp, mus_long_t start, mus_long_t end);
void play_channels(chan_info **cps, int chans, mus_long_t *starts, mus_long_t *ur_ends, 
		   play_process_t background, Xen edpos, bool selection, const char *caller, int arg_pos);
void play_selection(play_process_t background);
void loop_play_selection(void);
bool add_mix_to_play_list(mix_state *ms, chan_info *cp, mus_long_t beg_within_mix, bool start_playing);
void toggle_dac_pausing(void); /* snd-dac.c */
bool play_in_progress(void);
void initialize_apply(snd_info *sp, int chans, mus_long_t beg, mus_long_t framples);
void finalize_apply(snd_info *sp);
int run_apply(int ofd);
mus_float_t *sample_linear_env(env *e, int order);

void g_init_dac(void);
void clear_players(void);

bool xen_is_player(Xen obj);
#define is_player_sound(Sp) ((Sp) && ((Sp)->index < 0))
snd_info *get_player_sound(Xen player);
Xen no_such_player_error(const char *caller, Xen player);

void dac_set_expand(snd_info *sp, mus_float_t newval);
void dac_set_expand_length(snd_info *sp, mus_float_t newval);
void dac_set_expand_ramp(snd_info *sp, mus_float_t newval);
void dac_set_expand_hop(snd_info *sp, mus_float_t newval);
void dac_set_contrast_amp(snd_info *sp, mus_float_t newval);
void dac_set_reverb_feedback(snd_info *sp, mus_float_t newval);
void dac_set_reverb_lowpass(snd_info *sp, mus_float_t newval);



/* -------- snd-chn.c -------- */

bool is_graph_style(int grf);
chan_info *get_cp(Xen snd_n, Xen chn_n, const char *caller);
snd_info *make_simple_channel_display(int srate, int initial_length, fw_button_t with_arrows, graph_style_t grf_style, widget_t container, bool with_events);
axis_info *lisp_info_axis(chan_info *cp);
void free_lisp_info(chan_info *cp);
void zx_incremented(chan_info *cp, double amount);
kbd_cursor_t cursor_decision(chan_info *cp);
void reset_x_display(chan_info *cp, double sx, double zx);
void set_x_axis_x0x1(chan_info *cp, double x0, double x1);
void cursor_move(chan_info *cp, mus_long_t samps);
void cursor_moveto_without_verbosity(chan_info *cp, mus_long_t samp);
void cursor_moveto_with_window(chan_info *cp, mus_long_t samp, mus_long_t left_samp, mus_long_t window_size);
void sync_cursors(chan_info *cp, mus_long_t samp);
void set_wavo_trace(int uval);
void set_dot_size(int val);
chan_info *virtual_selected_channel(chan_info *cp);
void handle_cursor(chan_info *cp, kbd_cursor_t redisplay);
void handle_cursor_with_sync(chan_info *cp, kbd_cursor_t redisplay);
void chans_field(fcp_t field, mus_float_t val);
void in_set_transform_graph_type(graph_type_t val);
void in_set_fft_window(mus_fft_window_t val);
void set_max_transform_peaks(int val);
void combine_sound(snd_info *sp);
void separate_sound(snd_info *sp);
void superimpose_sound(snd_info *sp);
void set_sound_channel_style(snd_info *sp, channel_style_t val);
void set_chan_fft_in_progress(chan_info *cp, idle_t fp);
void stop_fft_in_progress(chan_info *cp);
void goto_graph(chan_info *cp);
void start_peak_env(chan_info *cp);
void stop_peak_env(chan_info *cp);
void write_transform_peaks(FILE *fd, chan_info *ucp);
bool chan_fft_in_progress(chan_info *cp);
void force_fft_clear(chan_info *cp);
void chan_info_cleanup(chan_info *cp);
void display_channel_data_for_print(chan_info *cp);
void update_graph(chan_info *cp);
void update_graph_or_warn(chan_info *cp);
void make_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end);
void add_channel_data(char *filename, chan_info *cp, channel_graph_t graphed);
bool add_channel_data_1(chan_info *cp, int srate, mus_long_t framples, channel_graph_t graphed);
void set_x_bounds(axis_info *ap);
void set_show_axes(show_axes_t val);
void display_channel_data(chan_info *cp);
void display_channel_fft_data(chan_info *cp);
void display_channel_time_data(chan_info *cp);
void show_cursor_info(chan_info *cp);
void apply_x_axis_change(chan_info *cp);
void apply_y_axis_change(chan_info *cp);
void sx_incremented(chan_info *cp, double amount);
int move_axis(chan_info *cp, int x);
void set_axes(chan_info *cp, double x0, double x1, mus_float_t y0, mus_float_t y1);
void focus_x_axis_change(chan_info *cp, int focus_style);
void key_press_callback(chan_info *ur_cp, int x, int y, int key_state, int keysym);
void graph_button_press_callback(chan_info *cp, void *ev, int x, int y, int key_state, int button, oclock_t time);
void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button);
void graph_button_motion_callback(chan_info *cp, int x, int y, oclock_t time);
void channel_resize(chan_info *cp);
void edit_history_select(chan_info *cp, int row);
int make_background_graph(chan_info *cp, int srate, bool *two_sided);
int make_dragged_marks_graph(chan_info *cp);
void reset_spectro(void);
void cursor_moveto(chan_info *cp, mus_long_t samp);
chan_info *which_channel(snd_info *sp, int y);
void set_show_grid(with_grid_t val);
void set_grid_density(mus_float_t val);
void set_cursor_size(int val);
void set_cursor_style(cursor_style_t val);
void set_show_mix_waveforms(bool val);
void clear_inset_graph(chan_info *cp);
void free_inset_graph(chan_info *cp);
void draw_inset_line_cursor(chan_info *cp, graphics_context *ax);
void make_sonogram(chan_info *cp);

void g_init_chn(void);
Xen make_graph_data(chan_info *cp, int edit_pos, mus_long_t losamp, mus_long_t hisamp);
void draw_graph_data(chan_info *cp, mus_long_t losamp, mus_long_t hisamp, int data_size, mus_float_t *data, mus_float_t *data1, graphics_context *ax, graph_style_t style);

void fftb(chan_info *cp, bool on);
void waveb(chan_info *cp, bool on);
void f_button_callback(chan_info *cp, bool on, bool with_control);
void w_button_callback(chan_info *cp, bool on, bool with_control);
graphics_context *set_context(chan_info *cp, chan_gc_t gc);
graphics_context *copy_context(chan_info *cp);
graphics_context *erase_context(chan_info *cp);
graphics_context *selection_context(chan_info *cp);
graphics_context *mark_tag_context(chan_info *cp);
graphics_context *mix_waveform_context(chan_info *cp);
graphics_context *cursor_context(chan_info *cp);
void calculate_fft(chan_info *cp);
void set_min_db(mus_float_t db);
void set_x_axis_style(x_axis_style_t val);
void set_with_verbose_cursor(bool val);
void set_graph_style(graph_style_t val);
void set_show_marks(bool val);
void set_show_y_zero(bool val);

Xen g_framples(Xen snd_n, Xen chn_n, Xen edpos);
void check_cursor_shape(chan_info *cp, int x, int y);
widget_t channel_to_widget(chan_info *cp);
chan_info *channel_to_chan(chan_info *cp);



/* -------- snd-axis.c -------- */

bool is_x_axis_style(int n);
bool shows_axes(int n);
axis_info *free_axis_info(axis_info *ap);
char *x_axis_location_to_string(chan_info *cp, double loc);
int grf_x(double val, axis_info *ap);
int grf_y(mus_float_t val, axis_info *ap);
void init_axis_scales(axis_info *ap);
void make_axes_1(axis_info *ap, x_axis_style_t x_style, int srate, show_axes_t axes, printing_t printing, 
		 with_x_axis_t show_x_axis, with_grid_t grid, log_axis_t log_axes, mus_float_t grid_scale);

#define ungrf_x(AP, X) (((X) - (AP)->x_base) / (AP)->x_scale)
#define ungrf_y(AP, Y) (((Y) - (AP)->y_base) / (AP)->y_scale)

axis_info *make_axis_info(chan_info *cp, double xmin, double xmax, mus_float_t ymin, mus_float_t ymax, 
			  const char *xlabel, double x0, double x1, mus_float_t y0, mus_float_t y1,
			  axis_info *old_ap);
void set_numbers_font(graphics_context *ax, printing_t printing, bool use_tiny_font);

#if (!USE_NO_GUI)
  void g_init_axis(void);
#endif
#if HAVE_GL
  void reload_label_font(void);
  void reload_number_font(void);
#endif



/* -------- snd-snd.c -------- */

snd_info *get_sp(Xen snd_n);
peak_env_info *free_peak_env(chan_info *cp, int pos);
void free_peak_env_state(chan_info *cp);
peak_env_info *free_peak_env_info(peak_env_info *ep);
void start_peak_env_state(chan_info *cp);
idle_func_t get_peak_env(any_pointer_t ptr);
void finish_peak_env(chan_info *cp);
bool peak_env_maxamp_ok(chan_info *cp, int edpos);
mus_float_t peak_env_maxamp(chan_info *cp, int edpos);
bool peak_env_usable(chan_info *cp, mus_float_t samples_per_pixel, mus_long_t hisamp, bool start_new, int edit_pos, bool finish_env);
int peak_env_graph(chan_info *cp, mus_float_t samples_per_pixel, int srate);
int peak_env_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end, mus_float_t samples_per_pixel, int srate);
char *shortname(snd_info *sp);
char *shortname_indexed(snd_info *sp);
void add_sound_data(char *filename, snd_info *sp, channel_graph_t graphed);
mus_float_t speed_changed(mus_float_t ival, char *srcbuf, speed_style_t style, int tones, int srcbuf_size);
char *sp_name_click(snd_info *sp);
void free_controls(snd_info *sp);
void save_controls(snd_info *sp);
void restore_controls(snd_info *sp);
void reset_controls(snd_info *sp);
void set_show_controls(bool val);
void stop_applying(snd_info *sp);
void expand_control_set_hop(mus_float_t hop);
void expand_control_set_length(mus_float_t hop);
void expand_control_set_ramp(mus_float_t hop);
void expand_control_set_jitter(mus_float_t hop);
void contrast_control_set_amp(mus_float_t hop);
void reverb_control_set_lowpass(mus_float_t hop);
void reverb_control_set_feedback(mus_float_t hop);
void amp_env_env(chan_info *cp, mus_float_t *brkpts, int npts, int pos, mus_float_t base, mus_float_t scaler, mus_float_t offset);
peak_env_info *copy_peak_env_info(peak_env_info *old_ep, bool reversed);
void amp_env_env_selection_by(chan_info *cp, mus_any *e, mus_long_t beg, mus_long_t num, int pos);
void peak_env_insert_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int pos);
snd_info *snd_new_file(const char *newname, int chans, int srate, mus_sample_t sample_type, 
		       mus_header_t header_type, const char *new_comment, mus_long_t samples);
#if XEN_HAVE_RATIOS
  void snd_rationalize(mus_float_t a, int *num, int *den);
#endif
#ifdef __GNUC__
  void status_report(snd_info *sp, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
#else
  void status_report(snd_info *sp, const char *format, ...);
#endif
void errors_to_status_area(const char *msg, void *data);
void printout_to_status_area(const char *msg, void *data);
void clear_status_area(snd_info *sp);;

void g_init_snd(void);
Xen snd_no_such_sound_error(const char *caller, Xen n);

void peak_env_scale_by(chan_info *cp, mus_float_t scl, int pos);
void peak_env_scale_selection_by(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos);
peak_env_info *peak_env_copy(chan_info *cp, bool reversed, int edpos);
peak_env_info *peak_env_section(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos);
void pick_one_bin(peak_env_info *ep, int bin, mus_long_t cursamp, chan_info *cp, int edpos);
void set_channel_style(channel_style_t val);

Xen new_xen_sound(int n);
bool xen_is_sound(Xen obj);
int xen_sound_to_int(Xen n);
#define C_int_to_Xen_sound(Val) new_xen_sound(Val)
#define Xen_sound_to_C_int(n) xen_sound_to_int(n)

const char *read_peak_env_info_file(chan_info *cp);
bool write_peak_env_info_file(chan_info *cp);
void delete_peak_env_info_file(chan_info *cp);



/* -------- snd-file -------- */

axes_data *free_axes_data(axes_data *sa);
axes_data *make_axes_data(snd_info *sp);
void restore_axes_data(snd_info *sp, axes_data *sa, mus_float_t new_duration, bool need_edit_history_update);
mus_long_t disk_kspace(const char *filename);
time_t file_write_date(const char *filename);
bool is_link_file(const char *filename);
int recent_files_size(void);
char **recent_files(void);
bool is_directory(const char *filename);
file_info *make_file_info(const char *fullname, read_only_t read_only, bool selected);
file_info *free_file_info(file_info *hdr);
file_info *copy_header(const char *fullname, file_info *ohdr);
file_info *make_temp_header(const char *fullname, int srate, int chans, mus_long_t samples, const char *caller);
bool is_sound_file(const char *name);
void init_sound_file_extensions(void);
void save_added_sound_file_extensions(FILE *fd);
const char **get_sound_file_extensions(void);
int sound_file_extensions_length(void);
snd_info *snd_open_file(const char *filename, read_only_t read_only);
void snd_close_file(snd_info *sp);
snd_info *make_sound_readable(const char *filename, bool post_close);
snd_info *snd_update(snd_info *sp);
snd_info *snd_update_within_xen(snd_info *sp, const char *caller);
int snd_decode(mus_header_t type, const char *input_filename, const char *output_filename);
void set_fallback_srate(int sr);
void set_fallback_chans(int ch);
void set_fallback_sample_type(mus_sample_t fr);
void set_with_tooltips(bool val);
void run_after_save_as_hook(snd_info *sp, const char *already_saved_as_name, bool from_save_as_dialog);
bool run_before_save_as_hook(snd_info *sp, const char *save_as_filename, bool selection, int srate, 
			     mus_sample_t smp_type, mus_header_t hd_type, const char *comment);
void during_open(int fd, const char *file, open_reason_t reason);
void after_open(snd_info *sp);
void set_with_toolbar_and_display(bool val);
#if (!USE_NO_GUI)
  void display_info(snd_info *sp);
#endif
void g_init_file(void);
void initialize_sample_type_lists(void);
void set_with_menu_icons(bool val);
Xen g_expand_vector(Xen vector, int new_size);



/* -------- snd-utils -------- */

int snd_round(double x);
mus_long_t snd_round_mus_long_t(double x);
mus_long_t snd_abs_mus_long_t(mus_long_t val);
int snd_int_pow2(int n);
mus_long_t snd_mus_long_t_pow2(int n);
int snd_to_int_pow2(int n);
int snd_int_log2(int n);
bool snd_feq(mus_float_t val1, mus_float_t val2);

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define in_dB(Min_Db, Lin_Db, Val)  ({ mus_float_t _snd_1_h_1 = Val; (_snd_1_h_1 <= Lin_Db) ? Min_Db : (20.0 * log10(_snd_1_h_1)); })
#else
  mus_float_t in_dB(mus_float_t min_dB, mus_float_t lin_dB, mus_float_t val);
#endif

char *snd_local_time(void);
char *snd_io_strerror(void);
char *snd_open_strerror(void);
char *string_to_colon(char *val);
char *filename_without_directory(const char *name);
char *just_filename(char *name);
char *just_directory(const char *name);
char *file_to_string(const char *filename);

#ifdef __GNUC__
  char *vstr(const char *format, va_list ap)  __attribute__ ((format (printf, 1, 0)));
  char *snd_strftime(const char *format, time_t date) __attribute__ ((format (strftime, 1, 0)));
#else
  char *vstr(const char *format, va_list ap);
  char *snd_strftime(const char *format, time_t date);
#endif

disk_space_t disk_has_space(mus_long_t bytes, const char *filename);
char *prettyf(double num, int tens);
char *shorter_tempnam(const char *dir, const char *prefix);
char *snd_tempnam(void);
void snd_exit(int val);
void g_init_utils(void);



/* -------- snd-listener -------- */

void g_init_listener(void);
#if HAVE_SCHEME
void listener_begin_hook(s7_scheme *sc, bool *val);
#endif
void set_listener_prompt(const char *new_prompt);
bool listener_is_visible(void);
Xen run_read_hook(char *str);
bool have_read_hook(void);



/* -------- snd-mix.c -------- */

void free_ed_mixes(void *ptr);
bool mix_exists(int n);
bool mix_is_active(int n);
bool channel_has_mixes(chan_info *cp);
bool channel_has_active_mixes(chan_info *cp);
const char *mix_name(int id);
const char *mix_file_name(int id);
int mix_name_to_id(const char *name);
void goto_mix(chan_info *cp, int count);
mus_long_t zoom_focus_mix_in_channel_to_position(chan_info *cp);
int any_mix_id(void);
int next_mix_id(int id);
int previous_mix_id(int id);
int lowest_mix_id(void);
int highest_mix_id(void);
void reset_mix_ctr(void);
void preload_mixes(mix_state **mixes, int low_id, ed_list *ed);
void free_channel_mixes(chan_info *cp);
void delete_any_remaining_mix_temp_files_at_exit(chan_info *cp);
void mix_info_to_file(FILE *fd, chan_info *cp);
int mix_sync_from_id(int id);
int mix_set_sync_from_id(int id, int new_sync);
void set_mix_waveform_height(int new_val);
Xen new_xen_mix(int n);
Xen g_make_mix_sampler(Xen mix_id, Xen ubeg);
bool xen_is_mix(Xen obj);
snd_fd *xen_mix_to_snd_fd(Xen obj);
int xen_mix_to_int(Xen n);
#define Xen_mix_to_C_int(n) xen_mix_to_int(n)
Xen g_mix_length(Xen n);
Xen g_mix_sync(Xen n);
Xen g_set_mix_sync(Xen n, Xen val);
Xen g_mix_maxamp(Xen mix_id);
Xen g_mix_to_vct(Xen mix_n, Xen beg_n, Xen num);
 
mus_long_t mix_position_from_id(int id);
mus_long_t mix_length_from_id(int id);
mus_float_t mix_amp_from_id(int id);
mus_float_t mix_speed_from_id(int id);
env *mix_amp_env_from_id(int id);
chan_info *mix_chan_info_from_id(int id);
int copy_mix(int id);

mix_state *prepare_mix_state_for_channel(chan_info *cp, int mix_loc, mus_long_t beg, mus_long_t len);
void add_ed_mix(ed_list *ed, mix_state *ms);
mix_state *copy_mix_state(mix_state *old_ms);

void g_init_mix(void);

bool mix_set_position_edit(int id, mus_long_t pos);
bool mix_set_amp_env_edit(int id, env *e);
bool mix_set_amp_edit(int id, mus_float_t amp);
bool mix_set_speed_edit(int id, mus_float_t spd);
void after_mix_edit(int id);

void syncd_mix_set_color(int id, color_t col);
void syncd_mix_unset_color(int id);
void syncd_mix_set_amp(int id, mus_float_t amp);
void syncd_mix_set_speed(int id, mus_float_t amp);
void syncd_mix_set_amp_env(int id, env *e);
void syncd_mix_play(int id);
void mix_unset_color_from_id(int id);
color_t mix_color_from_id(int mix_id);
color_t mix_set_color_from_id(int id, color_t new_color);
void start_dragging_syncd_mixes(int mix_id);
void keep_dragging_syncd_mixes(int mix_id);
void stop_dragging_syncd_mixes(int mix_id);
void after_syncd_mix_edit(int id);
void syncd_mix_change_position(int mix_id, mus_long_t change);

int mix_complete_file(snd_info *sp, mus_long_t beg, const char *fullname, bool with_tag, file_delete_t auto_delete, mix_sync_t all_chans, int *out_chans);
int mix_complete_file_at_cursor(snd_info *sp, const char *str);
int mix_file(mus_long_t beg, mus_long_t num, int chans, chan_info **cps, const char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int start_chan);

bool is_mix_sampler(Xen obj);
Xen g_copy_mix_sampler(Xen obj);
Xen g_mix_sampler_home(Xen obj);
Xen g_mix_sampler_is_at_end(Xen obj);
Xen g_mix_sampler_position(Xen obj);
Xen g_free_mix_sampler(Xen obj);
char *edit_list_mix_init(chan_info *cp);
void channel_set_mix_tags_erased(chan_info *cp);
void color_mixes(color_t color);
void move_mix_tag(int mix_tag, int x, int y);
void finish_moving_mix_tag(int mix_tag, int x);
int hit_mix(chan_info *cp, int x, int y);
int hit_mix_triangle(chan_info *cp, int x, int y);
int prepare_mix_dialog_waveform(int mix_id, axis_info *ap, bool *two_sided);
void display_channel_mixes(chan_info *cp);

bool play_mix_from_id(int mix_id);
Xen g_play_mix(Xen num, mus_long_t beg);
void drag_and_drop_mix_at_x_y(int data, const char *filename, int x, int y);

snd_fd *mf_to_snd_fd(void *p);


/* -------- snd-find.c -------- */

#if HAVE_EXTENSION_LANGUAGE && (!USE_NO_GUI)
  void find_dialog_find(char *str, read_direction_t direction, chan_info *cp);
#endif
void g_init_find(void);



/* -------- snd-trans.c -------- */

int snd_translate(const char *oldname, const char *newname, mus_header_t type);


/* -------- snd.c -------- */

void snd_set_global_defaults(bool need_cleanup);
void g_init_base(void);


/* -------- snd-kbd.c -------- */

int in_keymap(int key, int state, bool cx_extended);
void set_keymap_entry(int key, int state, int args, Xen func, bool cx_extended, const char *origin, const char *prefs_info);
char *key_description(int key, int state, bool cx_extended);
char *make_key_name(char *buf, int buf_size, int key, int state, bool extended);
void map_over_keys(bool (*func)(int key, int state, bool cx, Xen xf));
key_info *find_prefs_key(const char *prefs_name);

void save_edits_from_kbd(snd_info *sp);
void keyboard_command(chan_info *cp, int keysym, int state);
void control_g(snd_info *sp);
void g_init_kbd(void);


/* -------- snd-sig.c -------- */

void scale_by(chan_info *cp, mus_float_t *scalers, int len, bool selection);
bool scale_to(snd_info *sp, chan_info *cp, mus_float_t *scalers, int len, bool selection);
void src_env_or_num(chan_info *cp, env *e, mus_float_t ratio, bool just_num, 
		    const char *origin, bool over_selection, mus_any *gen, Xen edpos, int arg_pos);
void apply_filter(chan_info *ncp, int order, env *e, const char *caller, const char *origin, 
		  bool over_selection, mus_float_t *ur_a, mus_any *gen, Xen edpos, int arg_pos, bool truncate);
void apply_env(chan_info *cp, env *e, mus_long_t beg, mus_long_t dur, bool over_selection, 
	       const char *origin, mus_any *gen, Xen edpos, int arg_pos);
void cos_smooth(chan_info *cp, mus_long_t beg, mus_long_t num, bool over_selection);
void display_frequency_response(env *e, axis_info *ap, graphics_context *gax, int order, bool dBing);
void cursor_delete(chan_info *cp, mus_long_t count);
void cursor_zeros(chan_info *cp, mus_long_t count, bool over_selection);
void cursor_insert(chan_info *cp, mus_long_t beg, mus_long_t count);
void cut_and_smooth(chan_info *cp);
void src_file(const char *file, double ratio);
mus_long_t scan_channel(chan_info *cp, mus_long_t start, mus_long_t end, Xen proc);

void g_init_sig(void);
int to_c_edit_position(chan_info *cp, Xen edpos, const char *caller, int arg_pos);
mus_long_t to_c_edit_samples(chan_info *cp, Xen edpos, const char *caller, int arg_pos);
mus_long_t beg_to_sample(Xen beg, const char *caller);
mus_long_t dur_to_samples(Xen dur, mus_long_t beg, chan_info *cp, int edpos, int argn, const char *caller);
#if USE_MOTIF
char *scale_and_src(char **files, int len, int max_chans, mus_float_t amp, mus_float_t speed, env *amp_env, bool *err);
#endif
Xen g_scale_selection_by(Xen scalers);
void reverse_sound(chan_info *ncp, bool over_selection, Xen edpos, int arg_pos);


/* -------- snd-draw.c -------- */

point_t *get_grf_points(void);
point_t *get_grf_points1(void);
void draw_cursor(chan_info *cp);
void set_grf_points(int xi, int j, int ymin, int ymax);
void set_grf_point(int xi, int j, int yi);
void draw_grf_points(int dot_size, graphics_context *ax, int j, axis_info *ap, mus_float_t y0, graph_style_t graph_style);
void draw_both_grf_points(int dot_size, graphics_context *ax, int j, graph_style_t graph_style);
void g_init_draw(void);
void set_dialog_widget(snd_dialog_t which, widget_t wid);
void run_new_widget_hook(widget_t w);
bool foreground_color_ok(Xen color, graphics_context *ax);

#if HAVE_GL
  void sgl_save_currents(void);
  void sgl_set_currents(bool with_dialogs);
#endif


/* -------- snd-ladspa.c -------- */
#if HAVE_LADSPA
void g_ladspa_to_snd(void);
#endif

#endif

