#ifndef SND_0_H
#define SND_0_H

#if HAVE_RUBY
  #undef _
#endif

#if HAVE_SCHEME
  #define DISPLAY(Obj) s7_object_to_c_string(s7, Obj)
#endif

#ifndef STRFTIME_FORMAT
  #define STRFTIME_FORMAT "%a %d-%b-%Y %H:%M %Z"
#endif

#ifndef _MSC_VER
  #define STRCMP(a, b) strcasecmp(a, b)
  #define STRNCMP(a, b, c) strncasecmp(a, b, c)
#else
  #define STRCMP(a, b) strcmp(a, b)
  #define STRNCMP(a, b, c) strncmp(a, b, c)
#endif

#define strcopy(Dest, Src, Len) snprintf(Dest, Len, "%s", Src)

#define unpack_sound(a) ((int)a >> 16)
#define unpack_channel(a) ((int)a & 0xff)
#define pack_sound_and_channel(a, b) ((a << 16) | b)

#define POINT_BUFFER_SIZE 8192

#define FILE_BUFFER_SIZE 8192
#define MAX_BUFFER_SIZE 65536
#define PRINT_BUFFER_SIZE 512
#define LABEL_BUFFER_SIZE 128
#define REPORTING_SIZE (MAX_BUFFER_SIZE * 32)
/* progress bar (hourglass icon) is displayed if more than this many samples are being processed */

#if (!USE_NO_GUI)
  #define NUM_HOURGLASSES 15
#else
  #define NUM_HOURGLASSES 1
#endif

#define PEAK_ENV_CUTOFF 50000
#define MIN_INIT 1000000.0
#define MAX_INIT -1000000.0

#define DEFAULT_OUTPUT_CHANS 1
#define DEFAULT_OUTPUT_SRATE 44100
#define DEFAULT_OUTPUT_HEADER_TYPE MUS_NEXT
/* mus-next is probably best here since intermediate/temp files can be any length (> 2^32 bytes)
 *   and the next header-specified size, although 32 bits, is explicitly "advisory".
 */

#if MUS_LITTLE_ENDIAN
  #define DEFAULT_OUTPUT_SAMPLE_TYPE MUS_LFLOAT
#else
  #define DEFAULT_OUTPUT_SAMPLE_TYPE MUS_BFLOAT
#endif

#define NO_COMPLETER -1
#define NO_SELECTION -1
#define NO_END_SPECIFIED -1
#define NO_MIX_TAG -1
#define INVALID_MIX_ID -1
#define SAVE_ALL_CHANS -1
#define ANY_MIX_ID -2
#define NO_COLOR -1
#define NOT_A_GC_LOC -1
#define NOT_A_SOUND -1

#define MIX_FILE_NO_MIX -1
#define MIX_FILE_NO_FILE -2
#define MIX_FILE_NO_SP -3
#define MIX_FILE_NO_TEMP_FILE -4

#ifndef POPUP_BUTTON
  #define POPUP_BUTTON 3
#endif

#define I_HELP "Help"
#define I_GO_AWAY "Go away"
#define I_NEXT "Next"
#define I_PREVIOUS "Previous"
#define I_FIND "Find"
#define I_find "find:"
#define I_STOP "Stop"

typedef enum {SOUND_SAVE_AS, SELECTION_SAVE_AS, REGION_SAVE_AS} save_dialog_t;
typedef enum {NOT_IN_BACKGROUND, IN_BACKGROUND} play_process_t;
typedef enum {WITHOUT_VIRTUAL_CHANNELS, WITH_VIRTUAL_CHANNELS} virtual_channels_t;
typedef enum {WITH_FW_BUTTONS, WITH_ARROWS} fw_button_t;
typedef enum {WITHOUT_HOOK, WITH_HOOK} with_hook_t;
typedef enum {WITHOUT_WORD_WRAP, WITH_WORD_WRAP} with_word_wrap_t;
typedef enum {DRAG_ENTER, DRAG_LEAVE, DRAG_MOTION} drag_style_t;
typedef enum {ENVED_AMPLITUDE, ENVED_SPECTRUM, ENVED_SRATE} enved_target_t;
typedef enum {ENVELOPE_LINEAR, ENVELOPE_EXPONENTIAL} env_type_t;
typedef enum {GRAPH_LINES, GRAPH_DOTS, GRAPH_FILLED, GRAPH_DOTS_AND_LINES, GRAPH_LOLLIPOPS, NUM_GRAPH_STYLES} graph_style_t;
typedef enum {APPLY_TO_SOUND, APPLY_TO_CHANNEL, APPLY_TO_SELECTION} snd_apply_t;
typedef enum {GRAPH_ONCE, GRAPH_AS_SONOGRAM, GRAPH_AS_SPECTROGRAM, GRAPH_AS_WAVOGRAM} graph_type_t;
typedef enum {ZOOM_FOCUS_LEFT, ZOOM_FOCUS_RIGHT, ZOOM_FOCUS_ACTIVE, ZOOM_FOCUS_MIDDLE, ZOOM_FOCUS_PROC} zoom_focus_t;
typedef enum {DONT_DELETE_ME, DELETE_ME, ALREADY_DELETED, MULTICHANNEL_DELETION, MULTICHANNEL_DELETION_IF_FILE} file_delete_t;
typedef enum {SND_REOPEN_CLOSED_FILE, SND_OPEN_CHANNEL, SND_COPY_READER, SND_INSERT_FILE, SND_CHANGE_FILE, SND_OVERRIDE_FILE, SND_MIX_FILE} open_reason_t;
typedef enum {CURSOR_CROSS, CURSOR_LINE, CURSOR_PROC} cursor_style_t;
typedef enum {SHOW_NO_AXES, SHOW_ALL_AXES, SHOW_X_AXIS, SHOW_ALL_AXES_UNLABELLED, SHOW_X_AXIS_UNLABELLED, SHOW_BARE_X_AXIS, NUM_SHOW_AXES} show_axes_t;
typedef enum {DONT_NORMALIZE, NORMALIZE_BY_CHANNEL, NORMALIZE_BY_SOUND, NORMALIZE_GLOBALLY, NUM_TRANSFORM_NORMALIZATIONS} fft_normalize_t;
typedef enum {NO_DISK_SPACE, NOT_ENOUGH_DISK_SPACE, DISK_SPACE_OK} disk_space_t;
typedef enum {CHAN_GC, CHAN_IGC, CHAN_SELGC, CHAN_CGC, CHAN_MGC, CHAN_MXGC, CHAN_TMPGC} chan_gc_t;
typedef enum {NOT_A_VIEWER, FILE_VIEWER, REGION_VIEWER} file_viewer_t; /* 1 and 2, according to docs for mouse-enter-label-hook */
typedef enum {COLOR_ORIENTATION_DIALOG, ENVED_DIALOG, TRANSFORM_DIALOG,
	      FILE_OPEN_DIALOG, SOUND_SAVE_AS_DIALOG, VIEW_FILES_DIALOG, RAW_DATA_DIALOG, NEW_FILE_DIALOG,
	      FILE_MIX_DIALOG, EDIT_HEADER_DIALOG, FIND_DIALOG, HELP_DIALOG, MIX_DIALOG,
	      PRINT_DIALOG, REGION_DIALOG, POST_IT_DIALOG, CONTROLS_DIALOG, SELECTION_SAVE_AS_DIALOG,
              FILE_INSERT_DIALOG, REGION_SAVE_AS_DIALOG, PREFERENCES_DIALOG, NUM_DIALOGS} snd_dialog_t;
typedef enum {SOUND_IDLE, SOUND_NORMAL, SOUND_WRAPPER, SOUND_REGION, SOUND_READER} sound_inuse_t;
typedef enum {IO_NO_ERROR, IO_SAVE_HOOK_CANCELLATION, IO_BAD_CHANNEL, IO_CANT_REOPEN_FILE, IO_TOO_MANY_OPEN_FILES, 
	      IO_UNKNOWN_SNDLIB_ERROR, IO_NO_MEMORY, IO_CANT_OPEN_FILE, IO_NO_FILENAME, IO_BAD_SAMPLE_TYPE, IO_BAD_HEADER_TYPE,
	      IO_SNDLIB_UNINITIALIZED, IO_NOT_A_SOUND_FILE, IO_FILE_CLOSED, IO_WRITE_ERROR, IO_INTERRUPTED, IO_CANT_CLOSE_FILE, 
	      IO_BAD_HEADER, IO_DISK_FULL, IO_WRITE_PROTECTED, IO_CANT_READ_SELECTION_FILE, IO_NEED_WRITE_CONFIRMATION, 
	      IO_NO_CHANGES, IO_EDIT_HOOK_CANCELLATION, IO_CANT_CREATE_FILE, IO_ERROR_NUM} io_error_t;
typedef enum {INSERTION_EDIT, DELETION_EDIT, CHANGE_EDIT, INITIALIZE_EDIT, SCALED_EDIT, ZERO_EDIT, RAMP_EDIT, 
	      EXTEND_EDIT, MIX_EDIT, CHANGE_MIX_EDIT,
              NUM_EDIT_TYPES} edit_t;
typedef enum {SAMPLER, REGION_READER, MIX_READER} reader_t;
typedef enum {MIX_FOLLOWS_SYNC, MIX_SETS_SYNC_LOCALLY} mix_sync_t;
typedef enum {CHANNEL_FREED, CHANNEL_INACTIVE, CHANNEL_INITIALIZED, CHANNEL_HAS_EDIT_LIST, CHANNEL_HAS_AXES} channel_state_t;

enum {BLACK_AND_WHITE_COLORMAP, GRAY_COLORMAP, HOT_COLORMAP, COOL_COLORMAP, BONE_COLORMAP, COPPER_COLORMAP, PINK_COLORMAP, JET_COLORMAP, PRISM_COLORMAP,
      AUTUMN_COLORMAP, WINTER_COLORMAP, SPRING_COLORMAP, SUMMER_COLORMAP, RAINBOW_COLORMAP, FLAG_COLORMAP, PHASES_COLORMAP, NUM_BUILTIN_COLORMAPS};


#define is_serious_io_error(Err) ((Err != IO_NO_ERROR) && \
                                  (Err != IO_EDIT_HOOK_CANCELLATION) && \
                                  (Err != IO_SAVE_HOOK_CANCELLATION) && \
                                  (Err != IO_INTERRUPTED) && \
                                  (Err != IO_NEED_WRITE_CONFIRMATION) && \
                                  (Err != IO_NO_CHANGES))

enum {FILE_OPENED, FILE_CLOSED};
typedef enum {WITH_READABLE_HEADERS, WITH_WRITABLE_HEADERS, WITH_BUILTIN_HEADERS} header_choice_t;
enum {SORT_A_TO_Z, SORT_Z_TO_A, SORT_NEW_TO_OLD, SORT_OLD_TO_NEW, SORT_SMALL_TO_BIG, SORT_BIG_TO_SMALL, SORT_XEN};

typedef enum {FILE_READ_WRITE, FILE_READ_ONLY} read_only_t;
#define FORCE_REFFT true
#define DONT_FORCE_REFFT false
#define FORCE_REDISPLAY true
#define DONT_FORCE_REDISPLAY false
#define WITH_EVENTS true
#define WITHOUT_EVENTS false
#define NOT_IN_AS_ONE_EDIT false
#define IN_AS_ONE_EDIT true
#define FILE_SELECTED true
#define FILE_NOT_SELECTED false
#define EXIT_FORCED true
#define EXIT_NOT_FORCED false

#if HAVE_RUBY
  #define to_proc_name(Str) xen_scheme_procedure_to_ruby(Str)
  #define TO_VAR_NAME(Str) xen_scheme_constant_to_ruby(Str)
  #define PROC_OPEN "("
  #define PROC_SEP ", "
  #define LIST_OPEN "["
  #define LIST_CLOSE "]"
  #define BPAREN ""
  #define EPAREN ""
  #define PROC_FALSE "false"
  #define PROC_TRUE "true"
  #define PROC_QUOTE ""
#endif
#if HAVE_FORTH
  #define to_proc_name(Str) Str
  #define TO_VAR_NAME(Str) Str
  #define PROC_OPEN " "
  #define PROC_SEP " "
  #define LIST_OPEN " '( "
  #define LIST_CLOSE " ) "
  #define BPAREN ""
  #define EPAREN " drop"
  #define PROC_FALSE "#f"
  #define PROC_TRUE  "#t"
  #define PROC_QUOTE ""
#endif
#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
  #define to_proc_name(Str) Str
  #define TO_VAR_NAME(Str) Str
  #define PROC_OPEN " "
  #define PROC_SEP " "
  #define LIST_OPEN "(list "
  #define LIST_CLOSE ")"
  #define BPAREN "("
  #define EPAREN ")"
  #define PROC_FALSE "#f"
  #define PROC_TRUE  "#t"
  #define PROC_QUOTE "'"
#endif

#define MAX_MAIN_MENUS 64
#define NO_REGIONS -2
#define INVALID_REGION -1
#define NO_LIST -1

typedef enum {READ_FORWARD, READ_BACKWARD} read_direction_t;
typedef enum {DONT_TRACK, TRACK_AND_RETURN, TRACK_AND_STAY} tracking_cursor_t;
typedef enum {DONT_UPDATE_DISPLAY, UPDATE_DISPLAY} cut_selection_regraph_t;
typedef enum {IGNORE_CACHE, REMOVE_FROM_CACHE} cache_remove_t;
typedef enum {FFT_UNCHANGED, FFT_CHANGED, FFT_CHANGE_LOCKED} fft_change_t;
typedef enum {WITHOUT_GRAPH, WITH_GRAPH, WITHOUT_INITIAL_GRAPH_HOOK} channel_graph_t;
typedef enum {NOT_PRINTING, PRINTING} printing_t;
typedef enum {NO_X_AXIS, WITH_X_AXIS} with_x_axis_t;
typedef enum {NO_GRID, WITH_GRID} with_grid_t;
typedef enum {WITH_LINEAR_AXES, WITH_LOG_X_AXIS, WITH_LOG_Y_AXIS} log_axis_t;

#define GET_NEW_SYNC -1
#define GET_ORIGINAL_SYNC -2

#define OVER_SELECTION true
#define OVER_SOUND false

#define AT_CURRENT_EDIT_POSITION -1

typedef enum {X_AXIS_IN_SECONDS, X_AXIS_IN_SAMPLES, X_AXIS_AS_PERCENTAGE, X_AXIS_IN_BEATS, 
	      X_AXIS_IN_MEASURES, X_AXIS_AS_CLOCK, NUM_X_AXIS_STYLES} x_axis_style_t;
typedef enum {SPEED_CONTROL_AS_FLOAT, SPEED_CONTROL_AS_RATIO, SPEED_CONTROL_AS_SEMITONE, NUM_SPEED_CONTROL_STYLES} speed_style_t;
typedef enum {CURSOR_IN_VIEW, CURSOR_ON_LEFT, CURSOR_ON_RIGHT, CURSOR_IN_MIDDLE, KEYBOARD_NO_ACTION} kbd_cursor_t;
typedef enum {CHANNELS_SEPARATE, CHANNELS_COMBINED, CHANNELS_SUPERIMPOSED, NOT_A_CHANNEL_STYLE} channel_style_t;
#define NUM_CHANNEL_STYLES 3
typedef enum {SYNC_NONE, SYNC_ALL, SYNC_BY_SOUND} sync_style_t;
#define NUM_SYNC_STYLES 3
typedef enum {FD_CLOSED, FD_OPEN} fd_open_t;
typedef enum {PRINT_SND, PRINT_ENV} print_choice_t;
typedef enum {SND_DATA_NO_DATA, SND_DATA_FILE, SND_DATA_BUFFER} snd_data_file_t;
typedef enum {SOUNDS_VERTICAL, SOUNDS_HORIZONTAL, SOUNDS_IN_NOTEBOOK, SOUNDS_IN_SEPARATE_WINDOWS} sound_style_t;
enum {FOURIER, WAVELET, WALSH, AUTOCORRELATION, CEPSTRUM, HAAR, NUM_BUILTIN_TRANSFORM_TYPES}; /* not typedef'd -- grows as new ones are added */
#define NUM_WAVELETS 48

typedef enum {FCP_X_ANGLE, FCP_X_SCALE, FCP_Y_ANGLE, FCP_Y_SCALE, FCP_Z_ANGLE, FCP_Z_SCALE, 
	      FCP_SPECTRUM_END, FCP_SPECTRUM_START, FCP_ALPHA, FCP_BETA, FCP_BEATS} fcp_t;
typedef enum {TIME_AXIS_INFO, TRANSFORM_AXIS_INFO, LISP_AXIS_INFO} axis_info_t;
typedef enum {COLOR_POSITION, COLOR_ZOOM} slider_choice_t;

typedef enum {PLAY_COMPLETE, PLAY_CLOSE, PLAY_BUTTON_UNSET, PLAY_STOP_CALLED, PLAY_C_G, PLAY_NO_CHANNEL,
	      PLAY_ERROR, PLAY_APPLY, PLAY_EDIT, PLAY_C_T} play_stop_t;

typedef enum {NO_REQUESTOR, FROM_UPDATE, FROM_VIEW_FILES, FROM_DRAG_AND_DROP, FROM_OPEN_DIALOG,
	      FROM_KEYBOARD, FROM_STARTUP, FROM_REGION_EDIT, FROM_NEW_FILE_DIALOG, FROM_OPEN_SOUND, 
	      FROM_OPEN_RAW_SOUND, FROM_VIEW_SOUND, FROM_NEW_SOUND, FROM_RAW_DATA_DIALOG, FROM_MIX_DIALOG,
              FROM_INSERT_DIALOG, FROM_VIEW_FILES_MIX_DIALOG, FROM_VIEW_FILES_INSERT_DIALOG, 
	      FROM_OPEN_RECENT_MENU, FROM_OPEN_DIALOG_POPUP, FROM_POPUP_CUT_TO_NEW} open_requestor_t;

#define DEFAULT_AMP_CONTROL 1.0
#define DEFAULT_CONTRAST_CONTROL 0.0
#define DEFAULT_EXPAND_CONTROL 1.0
#define DEFAULT_REVERB_CONTROL_LENGTH 1.0
#define DEFAULT_REVERB_CONTROL_SCALE 0.0
#define DEFAULT_SPEED_CONTROL 1.0
#define DEFAULT_CONTRAST_CONTROL_ON false
#define DEFAULT_EXPAND_CONTROL_ON false
#define DEFAULT_FILTER_CONTROL_ON false
#define DEFAULT_REVERB_CONTROL_ON false


#define filter_control_in_dB(ss) ss->Filter_Control_In_Db
#if HAVE_SCHEME
  #define in_set_filter_control_in_dB(ss, val) {ss->Filter_Control_In_Db = val; s7_symbol_set_value(s7, ss->filter_control_in_db_symbol, s7_make_boolean(s7, val));}
#else
  #define in_set_filter_control_in_dB(ss, val) ss->Filter_Control_In_Db = val
#endif
#define DEFAULT_FILTER_CONTROL_IN_DB false

#define filter_control_in_hz(ss) ss->Filter_Control_In_Hz
#if HAVE_SCHEME
  #define in_set_filter_control_in_hz(ss, val) {ss->Filter_Control_In_Hz = val; s7_symbol_set_value(s7, ss->filter_control_in_hz_symbol, s7_make_boolean(s7, val));}
#else
  #define in_set_filter_control_in_hz(ss, val) ss->Filter_Control_In_Hz = val
#endif
#define DEFAULT_FILTER_CONTROL_IN_HZ false

#define speed_control_tones(ss) ss->Speed_Control_Tones
#if HAVE_SCHEME
  #define in_set_speed_control_tones(ss, val) {ss->Speed_Control_Tones = val; s7_symbol_set_value(s7, ss->speed_control_tones_symbol, s7_make_integer(s7, val));}
#else
  #define in_set_speed_control_tones(ss, val) ss->Speed_Control_Tones = val
#endif
#define DEFAULT_SPEED_CONTROL_TONES 12

#define speed_control_style(ss) ss->Speed_Control_Style
#if HAVE_SCHEME
  #define in_set_speed_control_style(ss, val) {ss->Speed_Control_Style = val; s7_symbol_set_value(s7, ss->speed_control_style_symbol, s7_make_integer(s7, val));}
#else
  #define in_set_speed_control_style(ss, val) ss->Speed_Control_Style = val
#endif
#define DEFAULT_SPEED_CONTROL_STYLE SPEED_CONTROL_AS_FLOAT

#define expand_control_length(ss) ss->Expand_Control_Length
#if HAVE_SCHEME
  #define in_set_expand_control_length(ss, val) {ss->Expand_Control_Length = val; s7_symbol_set_value(s7, ss->expand_control_length_symbol, s7_make_real(s7, val));}
#else
  #define in_set_expand_control_length(ss, val) ss->Expand_Control_Length = val
#endif
#define DEFAULT_EXPAND_CONTROL_LENGTH 0.15

#define expand_control_ramp(ss) ss->Expand_Control_Ramp
#if HAVE_SCHEME
  #define in_set_expand_control_ramp(ss, val) {ss->Expand_Control_Ramp = val; s7_symbol_set_value(s7, ss->expand_control_ramp_symbol, s7_make_real(s7, val));}
#else
  #define in_set_expand_control_ramp(ss, val) ss->Expand_Control_Ramp = val
#endif
#define DEFAULT_EXPAND_CONTROL_RAMP 0.4

#define expand_control_hop(ss) ss->Expand_Control_Hop
#if HAVE_SCHEME
  #define in_set_expand_control_hop(ss, val) {ss->Expand_Control_Hop = val; s7_symbol_set_value(s7, ss->expand_control_hop_symbol, s7_make_real(s7, val));}
#else
  #define in_set_expand_control_hop(ss, val) ss->Expand_Control_Hop = val
#endif
#define DEFAULT_EXPAND_CONTROL_HOP 0.05

#define expand_control_jitter(ss) ss->Expand_Control_Jitter
#if HAVE_SCHEME
  #define in_set_expand_control_jitter(ss, val) {ss->Expand_Control_Jitter = val; s7_symbol_set_value(s7, ss->expand_control_jitter_symbol, s7_make_real(s7, val));}
#else
  #define in_set_expand_control_jitter(ss, val) ss->Expand_Control_Jitter = val
#endif
#define DEFAULT_EXPAND_CONTROL_JITTER 0.1

#define contrast_control_amp(ss) ss->Contrast_Control_Amp
#if HAVE_SCHEME
  #define in_set_contrast_control_amp(ss, val) {ss->Contrast_Control_Amp = val; s7_symbol_set_value(s7, ss->contrast_control_amp_symbol, s7_make_real(s7, val));}
#else
  #define in_set_contrast_control_amp(ss, val) ss->Contrast_Control_Amp = val
#endif
#define DEFAULT_CONTRAST_CONTROL_AMP 1.0

#define reverb_control_feedback(ss) ss->Reverb_Control_Feedback
#if HAVE_SCHEME
  #define in_set_reverb_control_feedback(ss, val) {ss->Reverb_Control_Feedback = val; s7_symbol_set_value(s7, ss->reverb_control_feedback_symbol, s7_make_real(s7, val));}
#else
  #define in_set_reverb_control_feedback(ss, val) ss->Reverb_Control_Feedback = val
#endif
#define DEFAULT_REVERB_CONTROL_FEEDBACK 1.09

#define reverb_control_lowpass(ss) ss->Reverb_Control_Lowpass
#if HAVE_SCHEME
  #define in_set_reverb_control_lowpass(ss, val) {ss->Reverb_Control_Lowpass = val; s7_symbol_set_value(s7, ss->reverb_control_lowpass_symbol, s7_make_real(s7, val));}
#else
  #define in_set_reverb_control_lowpass(ss, val) ss->Reverb_Control_Lowpass = val
#endif
#define DEFAULT_REVERB_CONTROL_LOWPASS 0.7

#define reverb_control_decay(ss) ss->Reverb_Control_Decay
#if HAVE_SCHEME
  #define in_set_reverb_control_decay(ss, val) {ss->Reverb_Control_Decay = val; s7_symbol_set_value(s7, ss->reverb_control_decay_symbol, s7_make_real(s7, val));}
#else
  #define in_set_reverb_control_decay(ss, val) ss->Reverb_Control_Decay = val
#endif
#define DEFAULT_REVERB_CONTROL_DECAY 1.0

#define contrast_control_min(ss) ss->Contrast_Control_Min
#define in_set_contrast_control_min(ss, val) ss->Contrast_Control_Min = val
#define DEFAULT_CONTRAST_CONTROL_MIN 0.0

#define contrast_control_max(ss) ss->Contrast_Control_Max
#define in_set_contrast_control_max(ss, val) ss->Contrast_Control_Max = val
#define DEFAULT_CONTRAST_CONTROL_MAX 10.0

#define expand_control_min(ss) ss->Expand_Control_Min
#define in_set_expand_control_min(ss, val) ss->Expand_Control_Min = val
#define DEFAULT_EXPAND_CONTROL_MIN 0.05

#define expand_control_max(ss) ss->Expand_Control_Max
#define in_set_expand_control_max(ss, val) ss->Expand_Control_Max = val
#define DEFAULT_EXPAND_CONTROL_MAX 20.0

#define speed_control_min(ss) ss->Speed_Control_Min
#define in_set_speed_control_min(ss, val) ss->Speed_Control_Min = val
#define DEFAULT_SPEED_CONTROL_MIN 0.05

#define speed_control_max(ss) ss->Speed_Control_Max
#define in_set_speed_control_max(ss, val) ss->Speed_Control_Max = val
#define DEFAULT_SPEED_CONTROL_MAX 20.0

#define amp_control_min(ss) ss->Amp_Control_Min
#define in_set_amp_control_min(ss, val) ss->Amp_Control_Min = val
#define DEFAULT_AMP_CONTROL_MIN 0.0

#define amp_control_max(ss) ss->Amp_Control_Max
#define in_set_amp_control_max(ss, val) ss->Amp_Control_Max = val
#define DEFAULT_AMP_CONTROL_MAX 8.0

#define reverb_control_scale_min(ss) ss->Reverb_Control_Scale_Min
#define in_set_reverb_control_scale_min(ss, val) ss->Reverb_Control_Scale_Min = val
#define DEFAULT_REVERB_CONTROL_SCALE_MIN 0.0

#define reverb_control_scale_max(ss) ss->Reverb_Control_Scale_Max
#define in_set_reverb_control_scale_max(ss, val) ss->Reverb_Control_Scale_Max = val
#define DEFAULT_REVERB_CONTROL_SCALE_MAX 4.0

#define reverb_control_length_min(ss) ss->Reverb_Control_Length_Min
#define in_set_reverb_control_length_min(ss, val) ss->Reverb_Control_Length_Min = val
#define DEFAULT_REVERB_CONTROL_LENGTH_MIN 0.0

#define reverb_control_length_max(ss) ss->Reverb_Control_Length_Max
#define in_set_reverb_control_length_max(ss, val) ss->Reverb_Control_Length_Max = val
#define DEFAULT_REVERB_CONTROL_LENGTH_MAX 5.0

#define filter_control_order(ss) ss->Filter_Control_Order
#if HAVE_SCHEME
  #define in_set_filter_control_order(ss, val) {ss->Filter_Control_Order = val; s7_symbol_set_value(s7, ss->filter_control_order_symbol, s7_make_integer(s7, val));}
#else
  #define in_set_filter_control_order(ss, val) ss->Filter_Control_Order = val
#endif
#define DEFAULT_FILTER_CONTROL_ORDER 20

#define in_show_controls(ss) ss->Show_Controls
#if HAVE_SCHEME
  #define in_set_show_controls(ss, val) \
    do {\
        ss->Show_Controls = val; \
        s7_symbol_set_value(s7, ss->show_controls_symbol, s7_make_boolean(s7, ss->Show_Controls));\
    } while (0)
#else
  #define in_set_show_controls(ss, val) ss->Show_Controls = val
#endif
#define DEFAULT_SHOW_CONTROLS false

#define with_tracking_cursor(ss) ss->With_Tracking_Cursor
#if HAVE_SCHEME
#define set_with_tracking_cursor(ss, val) \
  do {\
      ss->With_Tracking_Cursor = val; \
      s7_symbol_set_value(s7, ss->with_tracking_cursor_symbol, s7_make_integer(s7, (int)(ss->With_Tracking_Cursor))); \
     } while (0)
#else
  #define set_with_tracking_cursor(ss, val) ss->With_Tracking_Cursor = val
#endif
#define DEFAULT_WITH_TRACKING_CURSOR DONT_TRACK

#define just_sounds(ss) ss->Just_Sounds
#if HAVE_SCHEME
  #define set_just_sounds(val) \
    do {\
        ss->Just_Sounds = val; \
        s7_symbol_set_value(s7, ss->just_sounds_symbol, s7_make_boolean(s7, ss->Just_Sounds));\
    } while (0)
#else
  #define set_just_sounds(val) ss->Just_Sounds = val
#endif
#define DEFAULT_JUST_SOUNDS true

#define DEFAULT_SYNC 0
#define DEFAULT_INIT_WINDOW_X -1
#define DEFAULT_INIT_WINDOW_Y -1
#define DEFAULT_INIT_WINDOW_WIDTH 0
#define DEFAULT_INIT_WINDOW_HEIGHT 0

#define default_output_header_type(ss) ss->Default_Output_Header_Type
#if HAVE_SCHEME
  #define set_default_output_header_type(a) \
    do {\
      ss->Default_Output_Header_Type = (mus_header_t)a;			\
        s7_symbol_set_value(s7, ss->default_output_header_type_symbol, s7_make_integer(s7, (s7_int)(ss->Default_Output_Header_Type))); \
    } while (0)
#else
#define set_default_output_header_type(a) ss->Default_Output_Header_Type = (mus_header_t)a
#endif

#define default_output_chans(ss) ss->Default_Output_Chans
#if HAVE_SCHEME
  #define set_default_output_chans(a) \
    do {\
        ss->Default_Output_Chans = a; \
        s7_symbol_set_value(s7, ss->default_output_chans_symbol, s7_make_integer(s7, ss->Default_Output_Chans));\
    } while (0)
#else
  #define set_default_output_chans(a) ss->Default_Output_Chans = a
#endif

#define default_output_srate(ss) ss->Default_Output_Srate
#if HAVE_SCHEME
  #define set_default_output_srate(a) \
    do {\
        ss->Default_Output_Srate = a; \
        s7_symbol_set_value(s7, ss->default_output_srate_symbol, s7_make_integer(s7, ss->Default_Output_Srate));\
    } while (0)
#else
  #define set_default_output_srate(a) ss->Default_Output_Srate = a
#endif

#define default_output_sample_type(ss) ss->Default_Output_Sample_Type
#if HAVE_SCHEME
  #define set_default_output_sample_type(a) \
    do {\
        ss->Default_Output_Sample_Type = (mus_sample_t)a;			\
        s7_symbol_set_value(s7, ss->default_output_sample_type_symbol, s7_make_integer(s7, (s7_int)ss->Default_Output_Sample_Type)); \
       } while (0)
#else
#define set_default_output_sample_type(a) ss->Default_Output_Sample_Type = (mus_sample_t)a
#endif

#define dac_size(ss) ss->Dac_Size
#if HAVE_SCHEME
  #define set_dac_size(a) \
    do {\
        ss->Dac_Size = a; \
        s7_symbol_set_value(s7, ss->dac_size_symbol, s7_make_integer(s7, ss->Dac_Size));\
    } while (0)
#else
  #define set_dac_size(a) ss->Dac_Size = a
#endif
#if (HAVE_OSS || HAVE_ALSA)
  #define DEFAULT_DAC_SIZE 256
#else
  #if __APPLE__
    #define DEFAULT_DAC_SIZE 64
  #else
    #define DEFAULT_DAC_SIZE 1024
  #endif
#endif

#define dac_combines_channels(ss) ss->Dac_Combines_Channels
#if HAVE_SCHEME
  #define set_dac_combines_channels(a) \
    do {\
        ss->Dac_Combines_Channels = a; \
        s7_symbol_set_value(s7, ss->dac_combines_channels_symbol, s7_make_boolean(s7, ss->Dac_Combines_Channels));\
    } while (0)
#else
  #define set_dac_combines_channels(a) ss->Dac_Combines_Channels = a
#endif
#define DEFAULT_DAC_COMBINES_CHANNELS true

#define max_regions(ss) ss->Max_Regions
#if HAVE_SCHEME
  #define in_set_max_regions(a) \
    do {\
        ss->Max_Regions = a; \
        s7_symbol_set_value(s7, ss->max_regions_symbol, s7_make_integer(s7, ss->Max_Regions));\
       } while (0)
#else
  #define in_set_max_regions(a) ss->Max_Regions = a
#endif
#define DEFAULT_MAX_REGIONS 16

#define max_transform_peaks(ss) ss->Max_Transform_Peaks
#if HAVE_SCHEME
  #define in_set_max_transform_peaks(a) \
    do {\
        ss->Max_Transform_Peaks = a; \
        s7_symbol_set_value(s7, ss->max_transform_peaks_symbol, s7_make_integer(s7, ss->Max_Transform_Peaks));\
    } while (0)
#else
  #define in_set_max_transform_peaks(a) ss->Max_Transform_Peaks = a
#endif
#define DEFAULT_MAX_TRANSFORM_PEAKS 100

#define auto_resize(ss) ss->Auto_Resize
#if HAVE_SCHEME
  #define set_auto_resize(a) \
    do {\
        ss->Auto_Resize = a; \
        s7_symbol_set_value(s7, ss->auto_resize_symbol, s7_make_boolean(s7, ss->Auto_Resize));\
    } while (0)
#else
  #define set_auto_resize(a) ss->Auto_Resize = a
#endif
#if HAVE_GTK
  #define DEFAULT_AUTO_RESIZE false
#else
  #define DEFAULT_AUTO_RESIZE true
#endif

#define auto_update(ss) ss->Auto_Update
#if HAVE_SCHEME
  #define set_auto_update(a) \
    do {\
        ss->Auto_Update = a; \
        s7_symbol_set_value(s7, ss->auto_update_symbol, s7_make_boolean(s7, ss->Auto_Update));\
    } while (0)
#else
  #define set_auto_update(a) ss->Auto_Update = a
#endif
#define DEFAULT_AUTO_UPDATE false

#define auto_update_interval(ss) ss->Auto_Update_Interval
#if HAVE_SCHEME
  #define set_auto_update_interval(a) \
    do {\
        ss->Auto_Update_Interval = a; \
        s7_symbol_set_value(s7, ss->auto_update_interval_symbol, s7_make_real(s7, ss->Auto_Update_Interval));\
    } while (0)
#else
  #define set_auto_update_interval(a) ss->Auto_Update_Interval = a
#endif
#define DEFAULT_AUTO_UPDATE_INTERVAL 60.0

#define cursor_update_interval(ss) ss->Cursor_Update_Interval
#if HAVE_SCHEME
  #define set_cursor_update_interval(a) \
    do {\
        ss->Cursor_Update_Interval = a; \
        s7_symbol_set_value(s7, ss->cursor_update_interval_symbol, s7_make_real(s7, ss->Cursor_Update_Interval));\
    } while (0)
#else
  #define set_cursor_update_interval(a) ss->Cursor_Update_Interval = a
#endif
#define DEFAULT_CURSOR_UPDATE_INTERVAL 0.05

#define cursor_location_offset(ss) ss->Cursor_Location_Offset
#if HAVE_SCHEME
  #define set_cursor_location_offset(a) \
    do {\
        ss->Cursor_Location_Offset = a; \
        s7_symbol_set_value(s7, ss->cursor_location_offset_symbol, s7_make_integer(s7, ss->Cursor_Location_Offset));\
    } while (0)
#else
  #define set_cursor_location_offset(a) ss->Cursor_Location_Offset = a
#endif
#define DEFAULT_CURSOR_LOCATION_OFFSET 0

#define color_cutoff(ss) ss->Color_Cutoff
#if HAVE_SCHEME
  #define in_set_color_cutoff(a) \
    do {\
        ss->Color_Cutoff = a; \
        s7_symbol_set_value(s7, ss->color_cutoff_symbol, s7_make_real(s7, ss->Color_Cutoff));\
    } while (0)
#else
  #define in_set_color_cutoff(a) ss->Color_Cutoff = a
#endif
#define DEFAULT_COLOR_CUTOFF 0.003

#define color_inverted(ss) ss->Color_Inverted
#if HAVE_SCHEME
  #define in_set_color_inverted(a) \
    do {\
        ss->Color_Inverted = a; \
        s7_symbol_set_value(s7, ss->color_inverted_symbol, s7_make_boolean(s7, ss->Color_Inverted));\
    } while (0)
#else
  #define in_set_color_inverted(a) ss->Color_Inverted = a
#endif
#define DEFAULT_COLOR_INVERTED true

#define color_scale(ss) ss->Color_Scale
#if HAVE_SCHEME
  #define in_set_color_scale(a) \
    do {\
        ss->Color_Scale = a; \
        s7_symbol_set_value(s7, ss->color_scale_symbol, s7_make_real(s7, ss->Color_Scale));\
    } while (0)
#else
  #define in_set_color_scale(a) ss->Color_Scale = a
#endif
#define DEFAULT_COLOR_SCALE 1.0

#define fft_window_alpha(ss) ss->Fft_Window_Alpha
#if HAVE_SCHEME
  #define in_set_fft_window_alpha(a) \
    do {\
        ss->Fft_Window_Alpha = a; \
        s7_symbol_set_value(s7, ss->fft_window_alpha_symbol, s7_make_real(s7, ss->Fft_Window_Alpha));\
    } while (0)
#else
  #define in_set_fft_window_alpha(a) ss->Fft_Window_Alpha = a
#endif
#define DEFAULT_FFT_WINDOW_ALPHA 0.0

#define fft_window_beta(ss) ss->Fft_Window_Beta
#if HAVE_SCHEME
  #define in_set_fft_window_beta(a) \
    do {\
        ss->Fft_Window_Beta = a; \
        s7_symbol_set_value(s7, ss->fft_window_beta_symbol, s7_make_real(s7, ss->Fft_Window_Beta));\
    } while (0)
#else
  #define in_set_fft_window_beta(a) ss->Fft_Window_Beta = a
#endif
#define DEFAULT_FFT_WINDOW_BETA 0.0

#define transform_size(ss) ss->Transform_Size
#if HAVE_SCHEME
  #define in_set_transform_size(a) \
    do {\
        ss->Transform_Size = a; \
        s7_symbol_set_value(s7, ss->transform_size_symbol, s7_make_integer(s7, ss->Transform_Size));\
    } while (0)
#else
  #define in_set_transform_size(a) ss->Transform_Size = a
#endif
#define DEFAULT_TRANSFORM_SIZE 512

#define transform_graph_type(ss) ss->Transform_Graph_Type
#if HAVE_SCHEME
  #define in_set_transform_graph_type_1(a) \
    do {\
        ss->Transform_Graph_Type = a; \
        s7_symbol_set_value(s7, ss->transform_graph_type_symbol, s7_make_integer(s7, ss->Transform_Graph_Type));\
    } while (0)
#else
  #define in_set_transform_graph_type_1(a) ss->Transform_Graph_Type = a
#endif
#define DEFAULT_TRANSFORM_GRAPH_TYPE GRAPH_ONCE

#define time_graph_type(ss) ss->Time_Graph_Type
#if HAVE_SCHEME
  #define in_set_time_graph_type(a) \
    do {\
        ss->Time_Graph_Type = a; \
        s7_symbol_set_value(s7, ss->time_graph_type_symbol, s7_make_integer(s7, ss->Time_Graph_Type));\
    } while (0)
#else
  #define in_set_time_graph_type(a) ss->Time_Graph_Type = a
#endif
#define DEFAULT_TIME_GRAPH_TYPE GRAPH_ONCE

#define fft_window(ss) ss->Fft_Window
#if HAVE_SCHEME
  #define in_set_fft_window_1(a) \
    do {\
        ss->Fft_Window = a; \
        s7_symbol_set_value(s7, ss->fft_window_symbol, s7_make_integer(s7, ss->Fft_Window));\
    } while (0)
#else
  #define in_set_fft_window_1(a) ss->Fft_Window = a
#endif
#define DEFAULT_FFT_WINDOW MUS_BLACKMAN2_WINDOW

#define log_freq_start(ss) ss->Log_Freq_Start
#if HAVE_SCHEME
  #define in_set_log_freq_start(a) \
    do {\
        ss->Log_Freq_Start = a; \
        s7_symbol_set_value(s7, ss->log_freq_start_symbol, s7_make_real(s7, ss->Log_Freq_Start));\
    } while (0)
#else
  #define in_set_log_freq_start(a) ss->Log_Freq_Start = a
#endif
#define DEFAULT_LOG_FREQ_START 32.0

#define dot_size(ss) ss->Dot_Size
#if HAVE_SCHEME
  #define in_set_dot_size(a) \
    do {\
        ss->Dot_Size = a; \
        s7_symbol_set_value(s7, ss->dot_size_symbol, s7_make_integer(s7, ss->Dot_Size));\
    } while (0)
#else
  #define in_set_dot_size(a) ss->Dot_Size = a
#endif
#define DEFAULT_DOT_SIZE 1
#define MIN_DOT_SIZE 0
#define MAX_DOT_SIZE 100

#define grid_density(ss) ss->Grid_Density
#if HAVE_SCHEME
  #define in_set_grid_density(a) \
    do {\
        ss->Grid_Density = a; \
        s7_symbol_set_value(s7, ss->grid_density_symbol, s7_make_real(s7, ss->Grid_Density));\
    } while (0)
#else
  #define in_set_grid_density(a) ss->Grid_Density = a
#endif
#define DEFAULT_GRID_DENSITY 1.0

#define transform_normalization(ss) ss->Transform_Normalization
#if HAVE_SCHEME
  #define in_set_transform_normalization(a) \
    do {\
        ss->Transform_Normalization = a; \
        s7_symbol_set_value(s7, ss->transform_normalization_symbol, s7_make_integer(s7, ss->Transform_Normalization));\
    } while (0)
#else
  #define in_set_transform_normalization(a) ss->Transform_Normalization = a
#endif
#define DEFAULT_TRANSFORM_NORMALIZATION NORMALIZE_BY_CHANNEL

#define ask_before_overwrite(ss) ss->Ask_Before_Overwrite
#if HAVE_SCHEME
  #define set_ask_before_overwrite(a) \
    do {\
        ss->Ask_Before_Overwrite = a; \
        s7_symbol_set_value(s7, ss->ask_before_overwrite_symbol, s7_make_boolean(s7, ss->Ask_Before_Overwrite));\
    } while (0)
#else
  #define set_ask_before_overwrite(a) ss->Ask_Before_Overwrite = a
#endif
#define DEFAULT_ASK_BEFORE_OVERWRITE false

#define with_toolbar(ss) ss->With_Toolbar
#if HAVE_SCHEME
  #define set_with_toolbar(a) \
    do {\
        ss->With_Toolbar = a; \
        s7_symbol_set_value(s7, ss->with_toolbar_symbol, s7_make_boolean(s7, ss->With_Toolbar));\
    } while (0)
#else
  #define set_with_toolbar(a) ss->With_Toolbar = a
#endif
#if USE_GTK
#define DEFAULT_WITH_TOOLBAR true
#else
#define DEFAULT_WITH_TOOLBAR false
#endif

#define with_tooltips(ss) ss->With_Tooltips
#if HAVE_SCHEME
  #define in_set_with_tooltips(a) \
    do {\
        ss->With_Tooltips = a; \
        s7_symbol_set_value(s7, ss->with_tooltips_symbol, s7_make_boolean(s7, ss->With_Tooltips));\
    } while (0)
#else
  #define in_set_with_tooltips(a) ss->With_Tooltips = a
#endif
#define DEFAULT_WITH_TOOLTIPS true

#define with_menu_icons(ss) ss->With_Menu_Icons
#if HAVE_SCHEME
  #define in_set_with_menu_icons(a) \
    do {\
        ss->With_Menu_Icons = a; \
        s7_symbol_set_value(s7, ss->with_menu_icons_symbol, s7_make_boolean(s7, ss->With_Menu_Icons));\
    } while (0)
#else
  #define in_set_with_menu_icons(a) ss->With_Menu_Icons = a
#endif
#define DEFAULT_WITH_MENU_ICONS true

#define save_as_dialog_src(ss) ss->Save_As_Dialog_Src
#if HAVE_SCHEME
  #define in_set_save_as_dialog_src(a) \
    do {\
        ss->Save_As_Dialog_Src = a; \
        s7_symbol_set_value(s7, ss->save_as_dialog_src_symbol, s7_make_boolean(s7, ss->Save_As_Dialog_Src));\
    } while (0)
#else
  #define in_set_save_as_dialog_src(a) ss->Save_As_Dialog_Src = a
#endif
#define DEFAULT_SAVE_AS_DIALOG_SRC false

#define save_as_dialog_auto_comment(ss) ss->Save_As_Dialog_Auto_Comment
#if HAVE_SCHEME
  #define in_set_save_as_dialog_auto_comment(a) \
    do {\
        ss->Save_As_Dialog_Auto_Comment = a; \
        s7_symbol_set_value(s7, ss->save_as_dialog_auto_comment_symbol, s7_make_boolean(s7, ss->Save_As_Dialog_Auto_Comment));\
    } while (0)
#else
  #define in_set_save_as_dialog_auto_comment(a) ss->Save_As_Dialog_Auto_Comment = a
#endif
#define DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT false

#define remember_sound_state(ss) ss->Remember_Sound_State
#if HAVE_SCHEME
  #define set_remember_sound_state(a) \
    do {\
        ss->Remember_Sound_State = a; \
        s7_symbol_set_value(s7, ss->remember_sound_state_symbol, s7_make_boolean(s7, ss->Remember_Sound_State));\
    } while (0)
#else
  #define set_remember_sound_state(a) ss->Remember_Sound_State = a
#endif
#define DEFAULT_REMEMBER_SOUND_STATE false

#define ask_about_unsaved_edits(ss) ss->Ask_About_Unsaved_Edits
#if HAVE_SCHEME
  #define set_ask_about_unsaved_edits(a) \
    do {\
        ss->Ask_About_Unsaved_Edits = a; \
        s7_symbol_set_value(s7, ss->ask_about_unsaved_edits_symbol, s7_make_boolean(s7, ss->Ask_About_Unsaved_Edits));\
    } while (0)
#else
  #define set_ask_about_unsaved_edits(a) ss->Ask_About_Unsaved_Edits = a
#endif
#define DEFAULT_ASK_ABOUT_UNSAVED_EDITS false

#define show_full_duration(ss) ss->Show_Full_Duration
#if HAVE_SCHEME
  #define set_show_full_duration(a) \
    do {\
        ss->Show_Full_Duration = a; \
        s7_symbol_set_value(s7, ss->show_full_duration_symbol, s7_make_boolean(s7, ss->Show_Full_Duration));\
    } while (0)
#else
  #define set_show_full_duration(a) ss->Show_Full_Duration = a
#endif
#define DEFAULT_SHOW_FULL_DURATION false

#define initial_beg(ss) ss->Initial_Beg
#if HAVE_SCHEME
  #define set_initial_beg(a) \
    do {\
        ss->Initial_Beg = a; \
        s7_symbol_set_value(s7, ss->initial_beg_symbol, s7_make_real(s7, ss->Initial_Beg));\
    } while (0)
#else
  #define set_initial_beg(a) ss->Initial_Beg = a
#endif
#define DEFAULT_INITIAL_BEG 0.0

#define initial_dur(ss) ss->Initial_Dur
#if HAVE_SCHEME
  #define set_initial_dur(a) \
    do {\
        ss->Initial_Dur = a; \
        s7_symbol_set_value(s7, ss->initial_dur_symbol, s7_make_real(s7, ss->Initial_Dur));\
    } while (0)
#else
  #define set_initial_dur(a) ss->Initial_Dur = a
#endif
#define DEFAULT_INITIAL_DUR 0.1

#define show_full_range(ss) ss->Show_Full_Range
#if HAVE_SCHEME
  #define set_show_full_range(a) \
    do {\
        ss->Show_Full_Range = a; \
        s7_symbol_set_value(s7, ss->show_full_range_symbol, s7_make_boolean(s7, ss->Show_Full_Range));\
    } while (0)
#else
  #define set_show_full_range(a) ss->Show_Full_Range = a
#endif
#define DEFAULT_SHOW_FULL_RANGE false

#define spectrum_end(ss) ss->Spectrum_End
#if HAVE_SCHEME
  #define in_set_spectrum_end(a) \
    do {\
        ss->Spectrum_End = a; \
        s7_symbol_set_value(s7, ss->spectrum_end_symbol, s7_make_real(s7, ss->Spectrum_End));\
    } while (0)
#else
  #define in_set_spectrum_end(a) ss->Spectrum_End = a
#endif
#define DEFAULT_SPECTRUM_END 1.0

#define spectrum_start(ss) ss->Spectrum_Start
#if HAVE_SCHEME
  #define in_set_spectrum_start(a) \
    do {\
        ss->Spectrum_Start = a; \
        s7_symbol_set_value(s7, ss->spectrum_start_symbol, s7_make_real(s7, ss->Spectrum_Start));\
    } while (0)
#else
  #define in_set_spectrum_start(a) ss->Spectrum_Start = a
#endif
#define DEFAULT_SPECTRUM_START 0.0

#define spectro_x_angle(ss) ss->Spectro_X_Angle
#if HAVE_SCHEME
  #define in_set_spectro_x_angle(a) \
    do {\
        ss->Spectro_X_Angle = a; \
        s7_symbol_set_value(s7, ss->spectro_x_angle_symbol, s7_make_real(s7, ss->Spectro_X_Angle));\
    } while (0)
#else
  #define in_set_spectro_x_angle(a) ss->Spectro_X_Angle = a
#endif
#if HAVE_GL
  #define DEFAULT_SPECTRO_X_ANGLE 300.0
#else
  #define DEFAULT_SPECTRO_X_ANGLE 90.0
#endif

#define spectro_y_angle(ss) ss->Spectro_Y_Angle
#if HAVE_SCHEME
  #define in_set_spectro_y_angle(a) \
    do {\
        ss->Spectro_Y_Angle = a; \
        s7_symbol_set_value(s7, ss->spectro_y_angle_symbol, s7_make_real(s7, ss->Spectro_Y_Angle));\
    } while (0)
#else
  #define in_set_spectro_y_angle(a) ss->Spectro_Y_Angle = a
#endif
#if HAVE_GL
  #define DEFAULT_SPECTRO_Y_ANGLE 320.0
#else
  #define DEFAULT_SPECTRO_Y_ANGLE 0.0
#endif

#define spectro_z_angle(ss) ss->Spectro_Z_Angle
#if HAVE_SCHEME
  #define in_set_spectro_z_angle(a) \
    do {\
        ss->Spectro_Z_Angle = a; \
        s7_symbol_set_value(s7, ss->spectro_z_angle_symbol, s7_make_real(s7, ss->Spectro_Z_Angle));\
    } while (0)
#else
  #define in_set_spectro_z_angle(a) ss->Spectro_Z_Angle = a
#endif
#if HAVE_GL
  #define DEFAULT_SPECTRO_Z_ANGLE 0.0
#else
  #define DEFAULT_SPECTRO_Z_ANGLE 358.0
#endif

#define spectro_x_scale(ss) ss->Spectro_X_Scale
#if HAVE_SCHEME
  #define in_set_spectro_x_scale(a) \
    do {\
        ss->Spectro_X_Scale = a; \
        s7_symbol_set_value(s7, ss->spectro_x_scale_symbol, s7_make_real(s7, ss->Spectro_X_Scale));\
    } while (0)
#else
  #define in_set_spectro_x_scale(a) ss->Spectro_X_Scale = a
#endif
#if HAVE_GL
  #define DEFAULT_SPECTRO_X_SCALE 1.5
  #define SPECTRO_X_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_X_SCALE 1.0
  #define SPECTRO_X_SCALE_MAX 2.0
#endif

#define spectro_y_scale(ss) ss->Spectro_Y_Scale
#if HAVE_SCHEME
  #define in_set_spectro_y_scale(a) \
    do {\
        ss->Spectro_Y_Scale = a; \
        s7_symbol_set_value(s7, ss->spectro_y_scale_symbol, s7_make_real(s7, ss->Spectro_Y_Scale));\
    } while (0)
#else
  #define in_set_spectro_y_scale(a) ss->Spectro_Y_Scale = a
#endif
#if HAVE_GL
  #define DEFAULT_SPECTRO_Y_SCALE 1.0
  #define SPECTRO_Y_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_Y_SCALE 1.0
  #define SPECTRO_Y_SCALE_MAX 2.0
#endif

#define spectro_z_scale(ss) ss->Spectro_Z_Scale
#if HAVE_SCHEME
  #define in_set_spectro_z_scale(a) \
    do {\
        ss->Spectro_Z_Scale = a; \
        s7_symbol_set_value(s7, ss->spectro_z_scale_symbol, s7_make_real(s7, ss->Spectro_Z_Scale));\
    } while (0)
#else
  #define in_set_spectro_z_scale(a) ss->Spectro_Z_Scale = a
#endif
#if HAVE_GL
  #define DEFAULT_SPECTRO_Z_SCALE 1.0
  #define SPECTRO_Z_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_Z_SCALE 0.1
  #define SPECTRO_Z_SCALE_MAX 1.0
#endif

#define spectro_hop(ss) ss->Spectro_Hop
#if HAVE_SCHEME
  #define in_set_spectro_hop(a) \
    do {\
        ss->Spectro_Hop = a; \
        s7_symbol_set_value(s7, ss->spectro_hop_symbol, s7_make_integer(s7, ss->Spectro_Hop));\
    } while (0)
#else
  #define in_set_spectro_hop(a) ss->Spectro_Hop = a
#endif
#define DEFAULT_SPECTRO_HOP 4

#define color_map(ss) ss->Color_Map
#if HAVE_SCHEME
  #define in_set_color_map(a) \
    do {\
        ss->Color_Map = a; \
        s7_symbol_set_value(s7, ss->color_map_symbol, s7_make_integer(s7, ss->Color_Map));\
    } while (0)
#else
  #define in_set_color_map(a) ss->Color_Map = a
#endif
#define DEFAULT_COLOR_MAP 2

#define color_map_size(ss) ss->Color_Map_Size
#if HAVE_SCHEME
  #define set_color_map_size(a) \
    do {\
        ss->Color_Map_Size = a; \
        s7_symbol_set_value(s7, ss->color_map_size_symbol, s7_make_integer(s7, ss->Color_Map_Size));\
    } while (0)
#else
  #define set_color_map_size(a) ss->Color_Map_Size = a
#endif
#define DEFAULT_COLOR_MAP_SIZE 512

#define graph_style(ss) ss->Graph_Style
#if HAVE_SCHEME
  #define in_set_graph_style(a) \
    do {\
        ss->Graph_Style = a; \
        s7_symbol_set_value(s7, ss->graph_style_symbol, s7_make_integer(s7, ss->Graph_Style));\
    } while (0)
#else
  #define in_set_graph_style(a) ss->Graph_Style = a
#endif
#define DEFAULT_GRAPH_STYLE GRAPH_LINES

#define region_graph_style(ss) ss->Region_Graph_Style
#if HAVE_SCHEME
  #define set_region_graph_style(a) \
    do {\
        ss->Region_Graph_Style = a; \
        s7_symbol_set_value(s7, ss->region_graph_style_symbol, s7_make_integer(s7, ss->Region_Graph_Style));\
    } while (0)
#else
  #define set_region_graph_style(a) ss->Region_Graph_Style = a
#endif

#define sinc_width(ss) ss->Sinc_Width
#if HAVE_SCHEME
  #define set_sinc_width(a) \
    do {\
        ss->Sinc_Width = a; \
        s7_symbol_set_value(s7, ss->sinc_width_symbol, s7_make_integer(s7, ss->Sinc_Width));\
    } while (0)
#else
  #define set_sinc_width(a) ss->Sinc_Width = a
#endif
#define DEFAULT_SINC_WIDTH 10

#define with_verbose_cursor(ss) ss->With_Verbose_Cursor
#if HAVE_SCHEME
  #define in_set_with_verbose_cursor(a) \
    do {\
        ss->With_Verbose_Cursor = a; \
        s7_symbol_set_value(s7, ss->with_verbose_cursor_symbol, s7_make_boolean(s7, ss->With_Verbose_Cursor));\
    } while (0)
#else
  #define in_set_with_verbose_cursor(a) ss->With_Verbose_Cursor = a
#endif
#define DEFAULT_WITH_VERBOSE_CURSOR false

#define with_inset_graph(ss) ss->With_Inset_Graph
#if HAVE_SCHEME
  #define set_with_inset_graph(a) \
    do {\
        ss->With_Inset_Graph = a; \
        s7_symbol_set_value(s7, ss->with_inset_graph_symbol, s7_make_boolean(s7, ss->With_Inset_Graph));\
    } while (0)
#else
  #define set_with_inset_graph(a) ss->With_Inset_Graph = a
#endif
#define DEFAULT_WITH_INSET_GRAPH false

#define with_interrupts(ss) ss->With_Interrupts
#if HAVE_SCHEME
  #define set_with_interrupts(a) \
    do {\
        ss->With_Interrupts = a; \
        s7_symbol_set_value(s7, ss->with_interrupts_symbol, s7_make_boolean(s7, ss->With_Interrupts));\
    } while (0)
#else
  #define set_with_interrupts(a) ss->With_Interrupts = a
#endif
#define DEFAULT_WITH_INTERRUPTS true

#define with_smpte_label(ss) ss->With_Smpte_Label
#if HAVE_SCHEME
  #define set_with_smpte_label(a) \
    do {\
        ss->With_Smpte_Label = a; \
        s7_symbol_set_value(s7, ss->with_smpte_label_symbol, s7_make_boolean(s7, ss->With_Smpte_Label));\
    } while (0)
#else
  #define set_with_smpte_label(a) ss->With_Smpte_Label = a
#endif
#define DEFAULT_WITH_SMPTE_LABEL false

#define with_pointer_focus(ss) ss->With_Pointer_Focus
#if HAVE_SCHEME
  #define set_with_pointer_focus(a) \
    do {\
        ss->With_Pointer_Focus = a; \
        s7_symbol_set_value(s7, ss->with_pointer_focus_symbol, s7_make_boolean(s7, ss->With_Pointer_Focus));\
    } while (0)
#else
  #define set_with_pointer_focus(a) ss->With_Pointer_Focus = a
#endif
#define DEFAULT_WITH_POINTER_FOCUS false

#define selection_creates_region(ss) ss->Selection_Creates_Region
#if HAVE_SCHEME
  #define set_selection_creates_region(a) \
    do {\
        ss->Selection_Creates_Region = a; \
        s7_symbol_set_value(s7, ss->selection_creates_region_symbol, s7_make_boolean(s7, ss->Selection_Creates_Region));\
    } while (0)
#else
  #define set_selection_creates_region(a) ss->Selection_Creates_Region = a
#endif
#define DEFAULT_SELECTION_CREATES_REGION true

#define zoom_focus_style(ss) ss->Zoom_Focus_Style
#if HAVE_SCHEME
  #define set_zoom_focus_style(a) \
    do {\
        ss->Zoom_Focus_Style = a; \
        s7_symbol_set_value(s7, ss->zoom_focus_style_symbol, s7_make_integer(s7, ss->Zoom_Focus_Style));\
    } while (0)
#else
  #define set_zoom_focus_style(a) ss->Zoom_Focus_Style = a
#endif
#define DEFAULT_ZOOM_FOCUS_STYLE ZOOM_FOCUS_ACTIVE

#define eps_file(ss) ss->Eps_File
#if HAVE_SCHEME
  #define set_eps_file(a) \
    do {\
        ss->Eps_File = a; \
        s7_symbol_set_value(s7, ss->eps_file_symbol, s7_make_string(s7, ss->Eps_File));\
    } while (0)
#else
  #define set_eps_file(a) ss->Eps_File = a
#endif
#define DEFAULT_EPS_FILE "snd.eps"

#define eps_left_margin(ss) ss->Eps_Left_Margin
#if HAVE_SCHEME
  #define set_eps_left_margin(a) \
    do {\
        ss->Eps_Left_Margin = a; \
        s7_symbol_set_value(s7, ss->eps_left_margin_symbol, s7_make_real(s7, ss->Eps_Left_Margin));\
    } while (0)
#else
  #define set_eps_left_margin(a) ss->Eps_Left_Margin = a
#endif
#define DEFAULT_EPS_LEFT_MARGIN 0.0

#define eps_bottom_margin(ss) ss->Eps_Bottom_Margin
#if HAVE_SCHEME
  #define set_eps_bottom_margin(a) \
    do {\
        ss->Eps_Bottom_Margin = a; \
        s7_symbol_set_value(s7, ss->eps_bottom_margin_symbol, s7_make_real(s7, ss->Eps_Bottom_Margin));\
    } while (0)
#else
  #define set_eps_bottom_margin(a) ss->Eps_Bottom_Margin = a
#endif
#define DEFAULT_EPS_BOTTOM_MARGIN 0.0

#define eps_size(ss) ss->Eps_Size
#if HAVE_SCHEME
  #define set_eps_size(a) \
    do {\
        ss->Eps_Size = a; \
        s7_symbol_set_value(s7, ss->eps_size_symbol, s7_make_real(s7, ss->Eps_Size));\
    } while (0)
#else
  #define set_eps_size(a) ss->Eps_Size = a
#endif
#define DEFAULT_EPS_SIZE 1.0

#define tiny_font(ss) ss->Tiny_Font
#if HAVE_SCHEME
  #define in_set_tiny_font(a) \
    do {\
        ss->Tiny_Font = a; \
        s7_symbol_set_value(s7, ss->tiny_font_symbol, s7_make_string(s7, ss->Tiny_Font));\
    } while (0)
#else
  #define in_set_tiny_font(a) ss->Tiny_Font = a
#endif

#define peaks_font(ss) ss->Peaks_Font
#if HAVE_SCHEME
  #define in_set_peaks_font(a) \
    do {\
        ss->Peaks_Font = a; \
        s7_symbol_set_value(s7, ss->peaks_font_symbol, s7_make_string(s7, ss->Peaks_Font));\
    } while (0)
#else
  #define in_set_peaks_font(a) ss->Peaks_Font = a
#endif

#define bold_peaks_font(ss) ss->Bold_Peaks_Font
#if HAVE_SCHEME
  #define in_set_bold_peaks_font(a) \
    do {\
        ss->Bold_Peaks_Font = a; \
        s7_symbol_set_value(s7, ss->bold_peaks_font_symbol, s7_make_string(s7, ss->Bold_Peaks_Font));\
    } while (0)
#else
  #define in_set_bold_peaks_font(a) ss->Bold_Peaks_Font = a
#endif

#define axis_label_font(ss) ss->Axis_Label_Font
#if HAVE_SCHEME
  #define in_set_axis_label_font(a) \
    do {\
        ss->Axis_Label_Font = a; \
        s7_symbol_set_value(s7, ss->axis_label_font_symbol, s7_make_string(s7, ss->Axis_Label_Font));\
    } while (0)
#else
  #define in_set_axis_label_font(a) ss->Axis_Label_Font = a
#endif

#define axis_numbers_font(ss) ss->Axis_Numbers_Font
#if HAVE_SCHEME
  #define in_set_axis_numbers_font(a) \
    do {\
        ss->Axis_Numbers_Font = a; \
        s7_symbol_set_value(s7, ss->axis_numbers_font_symbol, s7_make_string(s7, ss->Axis_Numbers_Font));\
    } while (0)
#else
  #define in_set_axis_numbers_font(a) ss->Axis_Numbers_Font = a
#endif

#define listener_font(ss) ss->Listener_Font
#if HAVE_SCHEME
  #define in_set_listener_font(a) \
    do {\
        ss->Listener_Font = a; \
        s7_symbol_set_value(s7, ss->listener_font_symbol, s7_make_string(s7, ss->Listener_Font));\
    } while (0)
#else
  #define in_set_listener_font(a) ss->Listener_Font = a
#endif

#define save_state_file(ss) ss->Save_State_File
#if HAVE_SCHEME
  #define in_set_save_state_file(a) \
    do {\
        ss->Save_State_File = a; \
        s7_symbol_set_value(s7, ss->save_state_file_symbol, s7_make_string(s7, ss->Save_State_File));\
    } while (0)
#else
  #define in_set_save_state_file(a) ss->Save_State_File = a
#endif
#define DEFAULT_SAVE_STATE_FILE "saved-snd." Xen_file_extension

#define temp_dir(ss) ss->Temp_Dir
#if HAVE_SCHEME
  #define set_temp_dir(a) \
    do {\
        ss->Temp_Dir = a; \
        s7_symbol_set_value(s7, ss->temp_dir_symbol, s7_make_string(s7, ss->Temp_Dir));\
    } while (0)
#else
  #define set_temp_dir(a) ss->Temp_Dir = a
#endif
#ifndef MUS_DEFAULT_TEMP_DIR
  #define MUS_DEFAULT_TEMP_DIR NULL
#endif

#define save_dir(ss) ss->Save_Dir
#if HAVE_SCHEME
  #define set_save_dir(a) \
    do {\
        ss->Save_Dir = a; \
        s7_symbol_set_value(s7, ss->save_dir_symbol, s7_make_string(s7, ss->Save_Dir));\
    } while (0)
#else
  #define set_save_dir(a) ss->Save_Dir = a
#endif
#ifndef MUS_DEFAULT_SAVE_DIR
  #define MUS_DEFAULT_SAVE_DIR NULL
#endif

#define ladspa_dir(ss) ss->Ladspa_Dir
#if HAVE_SCHEME
  #define set_ladspa_dir(a) \
    do {\
        ss->Ladspa_Dir = a; \
        s7_symbol_set_value(s7, ss->ladspa_dir_symbol, s7_make_string(s7, ss->Ladspa_Dir));\
    } while (0)
#else
  #define set_ladspa_dir(a) ss->Ladspa_Dir = a
#endif
#ifndef DEFAULT_LADSPA_DIR
  #define DEFAULT_LADSPA_DIR NULL
#endif

#define peak_env_dir(ss) ss->Peak_Env_Dir
#if HAVE_SCHEME
  #define set_peak_env_dir(a) \
    do {\
        ss->Peak_Env_Dir = a; \
        s7_symbol_set_value(s7, ss->peak_env_dir_symbol, s7_make_string(s7, ss->Peak_Env_Dir));\
    } while (0)
#else
  #define set_peak_env_dir(a) ss->Peak_Env_Dir = a
#endif
#ifndef DEFAULT_PEAK_ENV_DIR
  #define DEFAULT_PEAK_ENV_DIR NULL
#endif

#define open_file_dialog_directory(ss) ss->Open_File_Dialog_Directory
#if HAVE_SCHEME
  #define set_open_file_dialog_directory(a) \
    do {\
        ss->Open_File_Dialog_Directory = a; \
        s7_symbol_set_value(s7, ss->open_file_dialog_directory_symbol, s7_make_string(s7, ss->Open_File_Dialog_Directory));\
    } while (0)
#else
  #define set_open_file_dialog_directory(a) ss->Open_File_Dialog_Directory = a
#endif

#define wavelet_type(ss) ss->Wavelet_Type
#if HAVE_SCHEME
  #define in_set_wavelet_type(a) \
    do {\
        ss->Wavelet_Type = a; \
        s7_symbol_set_value(s7, ss->wavelet_type_symbol, s7_make_integer(s7, ss->Wavelet_Type));\
    } while (0)
#else
  #define in_set_wavelet_type(a) ss->Wavelet_Type = a
#endif
#define DEFAULT_WAVELET_TYPE 0

#define transform_type(ss) ss->Transform_Type
#if HAVE_SCHEME
  #define in_set_transform_type(a) \
    do {\
        ss->Transform_Type = a; \
        s7_symbol_set_value(s7, ss->transform_type_symbol, C_int_to_Xen_transform(ss->Transform_Type));	\
    } while (0)
#else
  #define in_set_transform_type(a) ss->Transform_Type = a
#endif
#define DEFAULT_TRANSFORM_TYPE FOURIER

#define show_selection_transform(ss) ss->Show_Selection_Transform
#if HAVE_SCHEME
  #define in_set_show_selection_transform(a) \
    do {\
        ss->Show_Selection_Transform = a; \
        s7_symbol_set_value(s7, ss->show_selection_transform_symbol, s7_make_boolean(s7, ss->Show_Selection_Transform));\
    } while (0)
#else
  #define in_set_show_selection_transform(a) ss->Show_Selection_Transform = a
#endif
#define DEFAULT_SHOW_SELECTION_TRANSFORM false

#define with_mix_tags(ss) ss->With_Mix_Tags
#if HAVE_SCHEME
  #define set_with_mix_tags(a) \
    do {\
        ss->With_Mix_Tags = a; \
        s7_symbol_set_value(s7, ss->with_mix_tags_symbol, s7_make_boolean(s7, ss->With_Mix_Tags));\
    } while (0)
#else
  #define set_with_mix_tags(a) ss->With_Mix_Tags = a
#endif
#if USE_NO_GUI
  #define DEFAULT_WITH_MIX_TAGS false
#else
  #define DEFAULT_WITH_MIX_TAGS true
#endif

#define with_relative_panes(ss) ss->With_Relative_Panes
#if HAVE_SCHEME
  #define set_with_relative_panes(a) \
    do {\
        ss->With_Relative_Panes = a; \
        s7_symbol_set_value(s7, ss->with_relative_panes_symbol, s7_make_boolean(s7, ss->With_Relative_Panes));\
    } while (0)
#else
  #define set_with_relative_panes(a) ss->With_Relative_Panes = a
#endif
#define DEFAULT_WITH_RELATIVE_PANES true

#define with_gl(ss) ss->With_GL
#if HAVE_SCHEME
  #define in_set_with_gl(a) \
    do {\
        ss->With_GL = a; \
        s7_symbol_set_value(s7, ss->with_gl_symbol, s7_make_boolean(s7, ss->With_GL));\
    } while (0)
#else
  #define in_set_with_gl(a) ss->With_GL = a
#endif
#if HAVE_GL
  #define DEFAULT_WITH_GL true
#else
  #define DEFAULT_WITH_GL false
#endif

#define with_background_processes(ss) ss->With_Background_Processes
#if HAVE_SCHEME
  #define set_with_background_processes(a) \
    do {\
        ss->With_Background_Processes = a; \
        s7_symbol_set_value(s7, ss->with_background_processes_symbol, s7_make_boolean(s7, ss->With_Background_Processes));\
    } while (0)
#else
  #define set_with_background_processes(a) ss->With_Background_Processes = a
#endif
#define DEFAULT_WITH_BACKGROUND_PROCESSES true

#define with_file_monitor(ss) ss->With_File_Monitor
#if HAVE_SCHEME
  #define set_with_file_monitor(a) \
    do {\
        ss->With_File_Monitor = a; \
        s7_symbol_set_value(s7, ss->with_file_monitor_symbol, s7_make_boolean(s7, ss->With_File_Monitor));\
    } while (0)
#else
  #define set_with_file_monitor(a) ss->With_File_Monitor = a
#endif
#define DEFAULT_WITH_FILE_MONITOR true

#define wavo_hop(ss) ss->Wavo_Hop
#if HAVE_SCHEME
  #define in_set_wavo_hop(a) \
    do {\
        ss->Wavo_Hop = a; \
        s7_symbol_set_value(s7, ss->wavo_hop_symbol, s7_make_integer(s7, ss->Wavo_Hop));\
    } while (0)
#else
  #define in_set_wavo_hop(a) ss->Wavo_Hop = a
#endif
#define DEFAULT_WAVO_HOP 3

#define wavo_trace(ss) ss->Wavo_Trace
#if HAVE_SCHEME
  #define in_set_wavo_trace(a) \
    do {\
        ss->Wavo_Trace = a; \
        s7_symbol_set_value(s7, ss->wavo_trace_symbol, s7_make_integer(s7, ss->Wavo_Trace));\
    } while (0)
#else
  #define in_set_wavo_trace(a) ss->Wavo_Trace = a
#endif
#define DEFAULT_WAVO_TRACE 64

#define x_axis_style(ss) ss->X_Axis_Style
#if HAVE_SCHEME
  #define in_set_x_axis_style(a) \
    do {\
        ss->X_Axis_Style = a; \
        s7_symbol_set_value(s7, ss->x_axis_style_symbol, s7_make_integer(s7, ss->X_Axis_Style));\
    } while (0)
#else
  #define in_set_x_axis_style(a) ss->X_Axis_Style = a
#endif
#define DEFAULT_X_AXIS_STYLE X_AXIS_IN_SECONDS

#define beats_per_minute(ss) ss->Beats_Per_Minute
#if HAVE_SCHEME
  #define in_set_beats_per_minute(a) \
    do {\
        ss->Beats_Per_Minute = a; \
        s7_symbol_set_value(s7, ss->beats_per_minute_symbol, s7_make_real(s7, ss->Beats_Per_Minute));\
    } while (0)
#else
  #define in_set_beats_per_minute(a) ss->Beats_Per_Minute = a
#endif
#define DEFAULT_BEATS_PER_MINUTE 60.0

#define beats_per_measure(ss) ss->Beats_Per_Measure
#if HAVE_SCHEME
  #define in_set_beats_per_measure(a) \
    do {\
        ss->Beats_Per_Measure = a; \
        s7_symbol_set_value(s7, ss->beats_per_measure_symbol, s7_make_integer(s7, ss->Beats_Per_Measure));\
    } while (0)
#else
  #define in_set_beats_per_measure(a) ss->Beats_Per_Measure = a
#endif
#define DEFAULT_BEATS_PER_MEASURE 4

#define zero_pad(ss) ss->Zero_Pad
#if HAVE_SCHEME
  #define in_set_zero_pad(a) \
    do {\
        ss->Zero_Pad = a; \
        s7_symbol_set_value(s7, ss->zero_pad_symbol, s7_make_integer(s7, ss->Zero_Pad));\
    } while (0)
#else
  #define in_set_zero_pad(a) ss->Zero_Pad = a
#endif
#define DEFAULT_ZERO_PAD 0
#define MAX_ZERO_PAD 1000

#define show_transform_peaks(ss) ss->Show_Transform_Peaks
#if HAVE_SCHEME
  #define in_set_show_transform_peaks(a) \
    do {\
        ss->Show_Transform_Peaks = a; \
        s7_symbol_set_value(s7, ss->show_transform_peaks_symbol, s7_make_boolean(s7, ss->Show_Transform_Peaks));\
    } while (0)
#else
  #define in_set_show_transform_peaks(a) ss->Show_Transform_Peaks = a
#endif
#define DEFAULT_SHOW_TRANSFORM_PEAKS false

#define show_indices(ss) ss->Show_Indices
#if HAVE_SCHEME
  #define set_show_indices(a) \
    do {\
        ss->Show_Indices = a; \
        s7_symbol_set_value(s7, ss->show_indices_symbol, s7_make_boolean(s7, ss->Show_Indices));\
    } while (0)
#else
  #define set_show_indices(a) ss->Show_Indices = a
#endif
#define DEFAULT_SHOW_INDICES false

#define show_y_zero(ss) ss->Show_Y_Zero
#if HAVE_SCHEME
  #define in_set_show_y_zero(a) \
    do {\
        ss->Show_Y_Zero = a; \
        s7_symbol_set_value(s7, ss->show_y_zero_symbol, s7_make_boolean(s7, ss->Show_Y_Zero));\
    } while (0)
#else
  #define in_set_show_y_zero(a) ss->Show_Y_Zero = a
#endif
#define DEFAULT_SHOW_Y_ZERO false

#define show_grid(ss) ss->Show_Grid
#if HAVE_SCHEME
  #define in_set_show_grid(a) \
    do {\
        ss->Show_Grid = a; \
        s7_symbol_set_value(s7, ss->show_grid_symbol, s7_make_boolean(s7, ss->Show_Grid));\
    } while (0)
#else
  #define in_set_show_grid(a) ss->Show_Grid = a
#endif
#define DEFAULT_SHOW_GRID NO_GRID

#define show_sonogram_cursor(ss) ss->Show_Sonogram_Cursor
#if HAVE_SCHEME
  #define in_set_show_sonogram_cursor(a) \
    do {\
        ss->Show_Sonogram_Cursor = a; \
        s7_symbol_set_value(s7, ss->show_sonogram_cursor_symbol, s7_make_boolean(s7, ss->Show_Sonogram_Cursor));\
    } while (0)
#else
  #define in_set_show_sonogram_cursor(a) ss->Show_Sonogram_Cursor = a
#endif
#define DEFAULT_SHOW_SONOGRAM_CURSOR false

#define show_axes(ss) ss->Show_Axes
#if HAVE_SCHEME
  #define in_set_show_axes(a) \
    do {\
        ss->Show_Axes = a; \
        s7_symbol_set_value(s7, ss->show_axes_symbol, s7_make_integer(s7, ss->Show_Axes));\
    } while (0)
#else
  #define in_set_show_axes(a) ss->Show_Axes = a
#endif
#define DEFAULT_SHOW_AXES SHOW_ALL_AXES

#define show_mix_waveforms(ss) ss->Show_Mix_Waveforms
#if HAVE_SCHEME
  #define in_set_show_mix_waveforms(a) \
    do {\
        ss->Show_Mix_Waveforms = a; \
        s7_symbol_set_value(s7, ss->show_mix_waveforms_symbol, s7_make_boolean(s7, ss->Show_Mix_Waveforms));\
    } while (0)
#else
  #define in_set_show_mix_waveforms(a) ss->Show_Mix_Waveforms = a
#endif
#define DEFAULT_SHOW_MIX_WAVEFORMS true

#define mix_waveform_height(ss) ss->Mix_Waveform_Height
#if HAVE_SCHEME
  #define in_set_mix_waveform_height(a) \
    do {\
        ss->Mix_Waveform_Height = a; \
        s7_symbol_set_value(s7, ss->mix_waveform_height_symbol, s7_make_integer(s7, ss->Mix_Waveform_Height));\
    } while (0)
#else
  #define in_set_mix_waveform_height(a) ss->Mix_Waveform_Height = a
#endif
#define DEFAULT_MIX_WAVEFORM_HEIGHT 20

#define show_marks(ss) ss->Show_Marks
#if HAVE_SCHEME
  #define in_set_show_marks(a) \
    do {\
        ss->Show_Marks = a; \
        s7_symbol_set_value(s7, ss->show_marks_symbol, s7_make_boolean(s7, ss->Show_Marks));\
    } while (0)
#else
  #define in_set_show_marks(a) ss->Show_Marks = a
#endif
#define DEFAULT_SHOW_MARKS true

#define fft_log_magnitude(ss) ss->Fft_Log_Magnitude
#if HAVE_SCHEME
  #define in_set_fft_log_magnitude(a) \
    do {\
        ss->Fft_Log_Magnitude = a; \
        s7_symbol_set_value(s7, ss->fft_log_magnitude_symbol, s7_make_boolean(s7, ss->Fft_Log_Magnitude));\
    } while (0)
#else
  #define in_set_fft_log_magnitude(a) ss->Fft_Log_Magnitude = a
#endif
#define DEFAULT_FFT_LOG_MAGNITUDE false

#define fft_log_frequency(ss) ss->Fft_Log_Frequency
#if HAVE_SCHEME
  #define in_set_fft_log_frequency(a) \
    do {\
        ss->Fft_Log_Frequency = a; \
        s7_symbol_set_value(s7, ss->fft_log_frequency_symbol, s7_make_boolean(s7, ss->Fft_Log_Frequency));\
    } while (0)
#else
  #define in_set_fft_log_frequency(a) ss->Fft_Log_Frequency = a
#endif
#define DEFAULT_FFT_LOG_FREQUENCY false

#define fft_with_phases(ss) ss->Fft_With_Phases
#if HAVE_SCHEME
  #define in_set_fft_with_phases(a) \
    do {\
        ss->Fft_With_Phases = a; \
        s7_symbol_set_value(s7, ss->fft_with_phases_symbol, s7_make_boolean(s7, ss->Fft_With_Phases));\
    } while (0)
#else
  #define in_set_fft_with_phases(a) ss->Fft_With_Phases = a
#endif
#define DEFAULT_FFT_WITH_PHASES false

#define cursor_style(ss) ss->Cursor_Style
#if HAVE_SCHEME
  #define in_set_cursor_style(a) \
    do {\
        ss->Cursor_Style = a; \
        s7_symbol_set_value(s7, ss->cursor_style_symbol, s7_make_integer(s7, ss->Cursor_Style));\
    } while (0)
#else
  #define in_set_cursor_style(a) ss->Cursor_Style = a
#endif
#define DEFAULT_CURSOR_STYLE CURSOR_CROSS

#define tracking_cursor_style(ss) ss->Tracking_Cursor_Style
#if HAVE_SCHEME
  #define in_set_tracking_cursor_style(a) \
    do {\
        ss->Tracking_Cursor_Style = a; \
        s7_symbol_set_value(s7, ss->tracking_cursor_style_symbol, s7_make_integer(s7, ss->Tracking_Cursor_Style));\
    } while (0)
#else
  #define in_set_tracking_cursor_style(a) ss->Tracking_Cursor_Style = a
#endif
#define DEFAULT_TRACKING_CURSOR_STYLE CURSOR_LINE

#define cursor_size(ss) ss->Cursor_Size
#if HAVE_SCHEME
  #define in_set_cursor_size(a) \
    do {\
        ss->Cursor_Size = a; \
        s7_symbol_set_value(s7, ss->cursor_size_symbol, s7_make_integer(s7, ss->Cursor_Size));\
    } while (0)
#else
  #define in_set_cursor_size(a) ss->Cursor_Size = a
#endif
#define DEFAULT_CURSOR_SIZE 15

#define channel_style(ss) ss->Channel_Style
#if HAVE_SCHEME
  #define in_set_channel_style(a) \
    do {\
        ss->Channel_Style = a; \
        s7_symbol_set_value(s7, ss->channel_style_symbol, s7_make_integer(s7, ss->Channel_Style));\
    } while (0)
#else
  #define in_set_channel_style(a) ss->Channel_Style = a
#endif
#define DEFAULT_CHANNEL_STYLE CHANNELS_COMBINED

#define sync_style(ss) ss->Sync_Style
#if HAVE_SCHEME
  #define set_sync_style(a) \
    do {\
        ss->Sync_Style = a; \
        s7_symbol_set_value(s7, ss->sync_style_symbol, s7_make_integer(s7, ss->Sync_Style));\
    } while (0)
#else
  #define set_sync_style(a) ss->Sync_Style = a
#endif
#define DEFAULT_SYNC_STYLE SYNC_BY_SOUND

#define sound_style(ss) ss->Sound_Style
#define set_sound_style(a) ss->Sound_Style = a
#define DEFAULT_SOUND_STYLE SOUNDS_VERTICAL

#define listener_prompt(ss) ss->Listener_Prompt
#if HAVE_SCHEME
  #define in_set_listener_prompt(a) \
    do {\
        ss->Listener_Prompt = a; \
        s7_symbol_set_value(s7, ss->listener_prompt_symbol, s7_make_string(s7, ss->Listener_Prompt));\
    } while (0)
#else
  #define in_set_listener_prompt(a) ss->Listener_Prompt = a
#endif
#define DEFAULT_LISTENER_PROMPT ">"

#define stdin_prompt(ss) ss->Stdin_Prompt
#if HAVE_SCHEME
  #define set_stdin_prompt(a) \
    do {\
        ss->Stdin_Prompt = a; \
        s7_symbol_set_value(s7, ss->stdin_prompt_symbol, s7_make_string(s7, ss->Stdin_Prompt));\
    } while (0)
#else
  #define set_stdin_prompt(a) ss->Stdin_Prompt = a
#endif
#define DEFAULT_STDIN_PROMPT ""

#define print_length(ss) ss->Print_Length
#if HAVE_SCHEME
  #define set_print_length(a) \
    do {\
        ss->Print_Length = a; \
        s7_symbol_set_value(s7, ss->print_length_symbol, s7_make_integer(s7, ss->Print_Length));\
    } while (0)
#else
  #define set_print_length(a) ss->Print_Length = a
#endif
#define DEFAULT_PRINT_LENGTH 12

#define view_files_sort(ss) ss->View_Files_Sort
#if HAVE_SCHEME
  #define set_view_files_sort(a) \
    do {\
        ss->View_Files_Sort = a; \
        s7_symbol_set_value(s7, ss->view_files_sort_symbol, s7_make_integer(s7, ss->View_Files_Sort));\
    } while (0)
#else
  #define set_view_files_sort(a) ss->View_Files_Sort = a
#endif
#define DEFAULT_VIEW_FILES_SORT SORT_A_TO_Z

#define enved_clipping(ss) ss->enved->clipping
#define in_set_enved_clipping(a) ss->enved->clipping = a
#define DEFAULT_ENVED_CLIPPING true

#define enved_with_wave(ss) ss->Enved_With_Wave
#if HAVE_SCHEME
  #define in_set_enved_with_wave(a) \
    do {\
        ss->Enved_With_Wave = a; \
        s7_symbol_set_value(s7, ss->enved_with_wave_symbol, s7_make_boolean(s7, ss->Enved_With_Wave));\
    } while (0)
#else
  #define in_set_enved_with_wave(a) ss->Enved_With_Wave = a
#endif
#define DEFAULT_ENVED_WITH_WAVE false

#define enved_filter_order(ss) ss->Enved_Filter_Order
#if HAVE_SCHEME
  #define in_set_enved_filter_order(a) \
    do {\
        ss->Enved_Filter_Order = a; \
        s7_symbol_set_value(s7, ss->enved_filter_order_symbol, s7_make_integer(s7, ss->Enved_Filter_Order));\
    } while (0)
#else
  #define in_set_enved_filter_order(a) ss->Enved_Filter_Order = a
#endif
#define DEFAULT_ENVED_FILTER_ORDER 40

#define enved_in_dB(ss) ss->enved->in_dB
#define in_set_enved_in_dB(a) ss->enved->in_dB = a
#define DEFAULT_ENVED_IN_DB false

#define enved_target(ss) ss->Enved_Target
#if HAVE_SCHEME
  #define in_set_enved_target(a) \
    do {\
        ss->Enved_Target = a; \
        s7_symbol_set_value(s7, ss->enved_target_symbol, s7_make_integer(s7, ss->Enved_Target));\
    } while (0)
#else
  #define in_set_enved_target(a) ss->Enved_Target = a
#endif
#define DEFAULT_ENVED_TARGET ENVED_AMPLITUDE

#define enved_base(ss) ss->Enved_Base
#if HAVE_SCHEME
  #define in_set_enved_base(a) \
    do {\
        ss->Enved_Base = a; \
        s7_symbol_set_value(s7, ss->enved_base_symbol, s7_make_real(s7, ss->Enved_Base));\
    } while (0)
#else
  #define in_set_enved_base(a) ss->Enved_Base = a
#endif
#define DEFAULT_ENVED_BASE 1.0

#define enved_power(ss) ss->Enved_Power
#if HAVE_SCHEME
  #define set_enved_power(a) \
    do {\
        ss->Enved_Power = a; \
        s7_symbol_set_value(s7, ss->enved_power_symbol, s7_make_real(s7, ss->Enved_Power));\
    } while (0)
#else
  #define set_enved_power(a) ss->Enved_Power = a
#endif
#define DEFAULT_ENVED_POWER 3.0

#define enved_style(ss) ss->Enved_Style
#if HAVE_SCHEME
  #define set_enved_style(a) \
    do {\
        ss->Enved_Style = a; \
        s7_symbol_set_value(s7, ss->enved_style_symbol, s7_make_integer(s7, ss->Enved_Style));\
    } while (0)
#else
  #define set_enved_style(a) ss->Enved_Style = a
#endif
#define DEFAULT_ENVED_STYLE ENVELOPE_LINEAR

#define in_graph_cursor(ss) ss->Graph_Cursor

#define clipping(ss) ss->Clipping
#if HAVE_SCHEME
  #define set_clipping(a) \
    do {\
        ss->Clipping = a; \
        s7_symbol_set_value(s7, ss->clipping_symbol, s7_make_boolean(s7, ss->Clipping));\
    } while (0)
#else
  #define set_clipping(a) ss->Clipping = a
#endif
#define DEFAULT_CLIPPING false

#define html_dir(ss) ss->HTML_Dir
#if HAVE_SCHEME
  #define set_html_dir_1(a) \
    do {\
        ss->HTML_Dir = a; \
        s7_symbol_set_value(s7, ss->html_dir_symbol, s7_make_string(s7, ss->HTML_Dir));\
    } while (0)
#else
  #define set_html_dir_1(a) ss->HTML_Dir = a
#endif
#define DEFAULT_HTML_DIR "."

#define html_program(ss) ss->HTML_Program
#if HAVE_SCHEME
  #define set_html_program(a) \
    do {\
        ss->HTML_Program = a; \
        s7_symbol_set_value(s7, ss->html_program_symbol, s7_make_string(s7, ss->HTML_Program));\
    } while (0)
#else
  #define set_html_program(a) ss->HTML_Program = a
#endif
#define DEFAULT_HTML_PROGRAM "firefox"

#define graphs_horizontal(ss) ss->Graphs_Horizontal
#if HAVE_SCHEME
  #define in_set_graphs_horizontal(a) \
    do {\
        ss->Graphs_Horizontal = a; \
        s7_symbol_set_value(s7, ss->graphs_horizontal_symbol, s7_make_boolean(s7, ss->Graphs_Horizontal));\
    } while (0)
#else
  #define in_set_graphs_horizontal(a) ss->Graphs_Horizontal = a
#endif
#define DEFAULT_GRAPHS_HORIZONTAL true

#define mix_tag_width(ss) ss->Mix_Tag_Width
#if HAVE_SCHEME
  #define set_mix_tag_width(a) \
    do {\
        ss->Mix_Tag_Width = a; \
        s7_symbol_set_value(s7, ss->mix_tag_width_symbol, s7_make_integer(s7, ss->Mix_Tag_Width));\
    } while (0)
#else
  #define set_mix_tag_width(a) ss->Mix_Tag_Width = a
#endif
#define DEFAULT_MIX_TAG_WIDTH 6

#define mix_tag_height(ss) ss->Mix_Tag_Height
#if HAVE_SCHEME
  #define set_mix_tag_height(a) \
    do {\
        ss->Mix_Tag_Height = a; \
        s7_symbol_set_value(s7, ss->mix_tag_height_symbol, s7_make_integer(s7, ss->Mix_Tag_Height));\
    } while (0)
#else
  #define set_mix_tag_height(a) ss->Mix_Tag_Height = a
#endif
#define DEFAULT_MIX_TAG_HEIGHT 14

#define mark_tag_width(ss) ss->Mark_Tag_Width
#if HAVE_SCHEME
  #define set_mark_tag_width(a) \
    do {\
        ss->Mark_Tag_Width = a; \
        s7_symbol_set_value(s7, ss->mark_tag_width_symbol, s7_make_integer(s7, ss->Mark_Tag_Width));\
    } while (0)
#else
  #define set_mark_tag_width(a) ss->Mark_Tag_Width = a
#endif
#define DEFAULT_MARK_TAG_WIDTH 10

#define mark_tag_height(ss) ss->Mark_Tag_Height
#if HAVE_SCHEME
  #define set_mark_tag_height(a) \
    do {\
        ss->Mark_Tag_Height = a; \
        s7_symbol_set_value(s7, ss->mark_tag_height_symbol, s7_make_integer(s7, ss->Mark_Tag_Height));\
    } while (0)
#else
  #define set_mark_tag_height(a) ss->Mark_Tag_Height = a
#endif
#define DEFAULT_MARK_TAG_HEIGHT 4

#define min_dB(ss) ss->Min_dB
#if HAVE_SCHEME
  #define set_min_dB(a) \
    do {\
        ss->Min_dB = a; \
        s7_symbol_set_value(s7, ss->min_db_symbol, s7_make_real(s7, ss->Min_dB));\
    } while (0)
#else
  #define set_min_dB(a) ss->Min_dB = a
#endif
#define DEFAULT_MIN_DB -60.0

#define play_arrow_size(ss) ss->Play_Arrow_Size
#if HAVE_SCHEME
  #define set_play_arrow_size(a) \
    do {\
        ss->Play_Arrow_Size = a; \
        s7_symbol_set_value(s7, ss->play_arrow_size_symbol, s7_make_integer(s7, ss->Play_Arrow_Size));\
    } while (0)
#else
  #define set_play_arrow_size(a) ss->Play_Arrow_Size = a
#endif
#define DEFAULT_PLAY_ARROW_SIZE 10


#define HAVE_GTK use USE_GTK not HAVE!
#define HAVE_MOTIF use USE_MOTIF not HAVE!
/* I keep using these HAVE_* forms by mistake */

#endif
