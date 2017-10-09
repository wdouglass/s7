#ifndef SND_X1_H
#define SND_X1_H


/* -------- snd-xhelp.c -------- */

Widget snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap);
Widget snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, const char **xrefs, const char **urls);
int help_text_width(const char *txt, int start, int end);
void snd_help_append(const char *text);
void snd_help_back_to_top(void);



/* -------- snd-xdraw.c -------- */

void draw_line(graphics_context *ax, int x0, int y0, int x1, int y1);
void draw_lines(graphics_context *ax, point_t *points, int num);
void draw_points(graphics_context *ax, point_t *points, int num, int size);
void fill_rectangle(graphics_context *ax, int x0, int y0, int width, int height);
void erase_rectangle(chan_info *cp, graphics_context *ax, int x0, int y0, int width, int height);
void fill_polygon(graphics_context *ax, int points, ...);
void fill_polygons(graphics_context *ax, point_t *points, int num, int y0);
void fill_two_sided_polygons(graphics_context *ax, point_t *points, point_t *points1, int num);
void draw_string(graphics_context *ax, int x0, int y0, const char *str, int len);
void gtk_style_draw_string(graphics_context *ax, int x0, int y0, const char *str, int len);
void draw_dot(graphics_context *ax, int x, int y, int size);
void setup_graphics_context(chan_info *cp, graphics_context *ax);
void draw_colored_lines(chan_info *cp, graphics_context *ax, point_t *points, int num, int *colors, int axis_y0, color_t default_color);
void draw_spectro_line(graphics_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void check_colormap_sizes(int size);
void reflect_color_list(bool setup_time);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, int x, int y, int width, int height);
void draw_sono_rectangles(graphics_context *ax, int color, int jmax);
void set_color_scale(mus_float_t val);
void set_color_inverted(bool val);
void set_color_cutoff(mus_float_t val);
void set_color_map(int val);
void set_spectro_hop(int val);
void set_spectro_x_angle(mus_float_t val);
void set_spectro_y_angle(mus_float_t val);
void set_spectro_z_angle(mus_float_t val);
void set_spectro_x_scale(mus_float_t val);
void set_spectro_y_scale(mus_float_t val);
void set_spectro_z_scale(mus_float_t val);
bool color_orientation_dialog_is_active(void);
Widget make_color_orientation_dialog(bool managed);
void reflect_spectro(void);
void set_with_gl(bool val, bool with_dialogs);



/* -------- snd-xlistener.c -------- */

void add_completer_to_builtin_textfield(Widget w, int completer);
void textfield_focus_callback(Widget w, XtPointer context, XtPointer info);
void textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info);
void mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag);
void mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag);
void listener_delete_text(int new_end);
void listener_append_and_prompt(const char *msg);
int save_listener_text(FILE *fp);
void append_listener_text(int end, const char *msg);
void goto_listener(void);
void color_listener(Pixel pix);
void color_listener_text(Pixel pix);
void listener_append(const char *msg);
void handle_listener(bool new_state);
bool listener_exists(void);
int listener_height(void);
int listener_width(void);
Widget make_togglebutton_widget(const char *name, Widget parent, Arg *args, int n);
Widget make_pushbutton_widget(const char *name, Widget parent, Arg *args, int n);
Widget make_text_widget(const char *name, Widget parent, Arg *args, int n);
Widget make_textfield_widget(const char *name, Widget parent, Arg *args, int n, text_cr_t activatable, int completer);
void clear_listener(void);
void set_listener_text_font(void);


/* -------- snd-xmenu.c -------- */

Widget add_menu(void);
void add_menu_drop(void);
Widget menu_widget(int which_menu);
void check_menu_labels(int key, int state, bool extended);
void reflect_play_selection_stop(void);
void set_button_label(Widget label, const char *str);
void add_tooltip(Widget w, const char *tip);
void post_basic_popup_menu(void *ev);
void post_lisp_popup_menu(void *ev);
void post_fft_popup_menu(void *ev);
void post_selection_popup_menu(void *ev);
widget_t make_file_print_dialog(bool managed, bool direct_to_printer);
void save_print_dialog_state(FILE *fd);


/* -------- snd-xmain.c -------- */

void snd_doit(int argc, char **argv);
color_t get_in_between_color(color_t fg, color_t bg);
void auto_update_restart(void);
void save_colors(FILE *Fp);



/* -------- snd-xfft.c -------- */

void set_fft_window_beta(mus_float_t val);
void set_fft_window_alpha(mus_float_t val);
void set_transform_size(mus_long_t val);
void set_fft_window(mus_fft_window_t val);
void set_wavelet_type(int val);
Widget make_transform_dialog(bool managed);
bool transform_dialog_is_active(void);

void set_spectrum_start(mus_float_t val);
void set_spectrum_end(mus_float_t val);
void set_transform_type(int val);
void make_transform_type_list(void);

void set_show_transform_peaks(bool val);
void set_fft_log_magnitude(bool val);
void set_fft_with_phases(bool val);
void set_fft_log_frequency(bool val);
void set_transform_normalization(fft_normalize_t val);
void set_show_selection_transform(bool show);
void set_transform_graph_type(graph_type_t val);
void reflect_peaks_in_transform_dialog(void);
void reflect_log_freq_start_in_transform_dialog(void);
void reflect_min_db_in_transform_dialog(void);



/* -------- snd-xregion.c -------- */

int update_region_browser(bool grf_too);
bool region_browser_is_active(void);
void delete_region_and_update_browser(int n);
void reflect_play_region_stop(int n);
bool region_dialog_is_active(void);
void allocate_region_rows(int n);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(void);


/* -------- snd-gxbitmaps.c -------- */

const char **mini_bomb_bits(int n);
const char **mini_glass_bits(int n);
const char **mini_lock_bits(void);
const char **close_icon_bits(void);
const char **snd_icon_bits(void);
const char **blank_bits(void);
void make_icons_transparent(const char *basic_color);
const char **stop_sign_bits(void);

void make_toolbar_icons(Widget w);
enum {SND_XPM_BACK_ARROW, SND_XPM_FORWARD_ARROW, SND_XPM_ZOOM_IN, SND_XPM_ZOOM_OUT, SND_XPM_CUT, SND_XPM_PASTE, SND_XPM_PREFERENCES, SND_XPM_CLOSE,
      SND_XPM_REDO, SND_XPM_UNDO, SND_XPM_SAVE, SND_XPM_SAVE_AS, SND_XPM_NEW, SND_XPM_OPEN, SND_XPM_NEXT, SND_XPM_BACK, SND_XPM_EXIT, SND_XPM_SEPARATOR, SND_XPM_UP, 
      SND_XPM_STOP, SND_XPM_REVERT, SND_XPM_PLAY, SND_XPM_CURSOR_PLAY, SND_XPM_STOP_PLAY,
      NUM_TOOLBAR_PIXMAPS};
Pixmap toolbar_icon(int which);
void show_toolbar(void);
void hide_toolbar(void);



/* -------- snd-gxcolormaps.c -------- */

char *colormap_name(int n);
bool is_colormap(int n);
int num_colormaps(void);
void get_current_color(int colormap, int n, rgb_t *r, rgb_t *g, rgb_t *b);
#if HAVE_GL
rgb_t *color_map_reds(int index);
rgb_t *color_map_greens(int index);
rgb_t *color_map_blues(int index);
#endif
void g_init_gxcolormaps(void);


/* -------- snd-xfind.c -------- */

void find_dialog(chan_info *cp);
void find_dialog_set_label(const char *str);
void stop_search_if_error(const char *msg, void *data);
void errors_to_find_text(const char *msg, void *data);
void find_dialog_stop_label(bool show_stop);
bool find_dialog_is_active(void);
void save_find_dialog_state(FILE *fd);


/* -------- snd-xutils.c -------- */

bool set_tiny_font(const char *font);
bool set_listener_font(const char *font);
bool set_peaks_font(const char *font);
bool set_bold_peaks_font(const char *font);
bool set_axis_label_font(const char *font);
bool set_axis_numbers_font(const char *font);
int label_width(const char *txt, bool use_tiny_font);
int number_width(const char *num, bool use_tiny_font);
int number_height(XFontStruct *numbers_font);
int label_height(bool use_tiny_font);
int mark_name_width(const char *txt);
void map_over_children_with_color(Widget w, void (*func)(Widget uw, color_t color), color_t color);
void clear_window(graphics_context *ax);
void set_label(Widget label, const char *str);
void set_title(const char *title);
void goto_window(Widget text);
void check_for_event(void);
void color_cursor(Pixel color);
void color_marks(Pixel color);
void color_selection(Pixel color);
void color_data(Pixel color);
void color_selected_data(Pixel color);
void color_graph(Pixel color);
void color_selected_graph(Pixel color);
void set_mix_color(Pixel color);
void recolor_graph(chan_info *cp, bool selected);
void set_sensitive(Widget wid, bool val);
void set_toggle_button(Widget wid, bool val, bool passed, void *data);
Dimension widget_height(Widget w);
Dimension widget_width(Widget w);
void set_widget_height(Widget w, Dimension height);
void set_widget_width(Widget w, Dimension width);
Position widget_x(Widget w);
Position widget_y(Widget w);
void set_widget_x(Widget w, Position x);
void set_widget_y(Widget w, Position y);
void set_widget_size(Widget w, Dimension width, Dimension height);
void set_widget_position(Widget w, Position x, Position y);
idle_t add_work_proc(XtWorkProc func, XtPointer data);
void draw_rotated_axis_label(chan_info *cp, graphics_context *ax, const char *text, int x0, int y0);


/* -------- snd-xchn.c -------- */

Widget channel_main_pane(chan_info *cp);
Widget channel_w(chan_info *cp);
Widget channel_f(chan_info *cp);
Widget channel_graph(chan_info *cp);
bool channel_graph_is_visible(chan_info *cp);
void change_gzy(mus_float_t val, chan_info *cp);
mus_float_t gsy_value(chan_info *cp);
mus_float_t gsy_size(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
void set_z_scrollbars(chan_info *cp, axis_info *ap);
void resize_sx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_sy_and_zy(chan_info *cp);
void resize_sx_and_zx(chan_info *cp);
void channel_open_pane(chan_info *cp);
void channel_unlock_pane(chan_info *cp);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, Widget main, fw_button_t arrows, bool with_events);
void set_peak_numbers_font(chan_info *cp, graphics_context *ax);
void set_bold_peak_numbers_font(chan_info *cp, graphics_context *ax);
void set_tiny_numbers_font(chan_info *cp, graphics_context *ax);
color_t get_foreground_color(graphics_context *ax);
void set_foreground_color(graphics_context *ax, Pixel color);
GC copy_GC(chan_info *cp);
GC erase_GC(chan_info *cp);
void graph_key_press(Widget w, XtPointer context, XEvent *event, Boolean *cont);
void cleanup_cw(chan_info *cp);
void free_fft_pix(chan_info *cp);
bool restore_fft_pix(chan_info *cp, graphics_context *ax);
void save_fft_pix(chan_info *cp, graphics_context *ax, int fwidth, int fheight, int x0, int y1);
void change_channel_style(snd_info *sp, channel_style_t new_style);
void color_chan_components(color_t color, slider_choice_t which_component);
void color_unselected_graphs(color_t color);


/* -------- snd-xsnd.c -------- */

int snd_pane_height(snd_info *sp);
int control_panel_height(snd_info *sp);
Widget w_snd_pane(snd_info *sp);
Widget unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp);
void set_status(snd_info *sp, const char *str, bool update);
void snd_info_cleanup(snd_info *sp);
void set_amp(snd_info *sp, mus_float_t val);
int amp_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval);
void set_expand(snd_info *sp, mus_float_t val);
void set_contrast(snd_info *sp, mus_float_t val);
void set_speed(snd_info *sp, mus_float_t val);
void set_revlen(snd_info *sp, mus_float_t val);
void set_revscl(snd_info *sp, mus_float_t val);
void set_filter_order(snd_info *sp, int val);
void set_filter_text(snd_info *sp, const char *str);
void display_filter_env(snd_info *sp);
void toggle_expand_button(snd_info *sp, bool state);
void toggle_contrast_button(snd_info *sp, bool state);
void toggle_reverb_button(snd_info *sp, bool state);
void toggle_filter_button(snd_info *sp, bool state);
void toggle_direction_arrow(snd_info *sp, bool state);
void set_filter_in_dB(snd_info *sp, bool val);
void set_filter_in_hz(snd_info *sp, bool val);
void filter_env_changed(snd_info *sp, env *e);
void set_play_button(snd_info *sp, bool val);
void play_button_pause(bool pausing);
void syncb(snd_info *sp, int on);
void show_lock(snd_info *sp);
void hide_lock(snd_info *sp);
void start_bomb(snd_info *sp);
void stop_bomb(snd_info *sp);
snd_info *add_sound_window(char *filename, read_only_t read_only, file_info *hdr);
void set_sound_pane_file_label(snd_info *sp, const char *str);
void color_filter_waveform(Pixel color);
void show_controls(snd_info *sp);
void hide_controls(snd_info *sp);
bool showing_controls(snd_info *sp);
void show_all_controls(void);
void hide_all_controls(void);
void start_progress_report(chan_info *cp);
void finish_progress_report(chan_info *cp);
void progress_report(chan_info *cp, mus_float_t pct);
XmString initial_speed_label(speed_style_t style);
void make_sound_icons_transparent_again(Pixel old_color, Pixel new_color);
void reflect_sound_selection(snd_info *sp);
void make_controls_dialog(void);
void update_sound_label(snd_info *sp);


/* -------- snd-xfile.c -------- */

void cleanup_file_monitor(void);
void *unmonitor_file(void *watcher);
void monitor_sound(snd_info *sp);

void save_view_files_dialogs(FILE *fd);
widget_t make_view_files_dialog(bool managed, bool make_new);
void view_files_unplay(void);
void view_files_add_directory(widget_t dialog, const char *dirname);
char *view_files_find_any_directory(void);
int view_files_dialog_list_length(void);
char **view_files_dialog_titles(void);
void view_files_start_dialog_with_title(const char *title);

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, mus_header_t *header_type, 
				       mus_sample_t *sample_type, mus_long_t *location, mus_long_t *samples, int min_chan);
void alert_new_file(void);
widget_t make_open_file_dialog(read_only_t read_only, bool managed);
widget_t make_sound_save_as_dialog(bool managed);
void make_channel_extract_dialog(int chan);
widget_t make_selection_save_as_dialog(bool managed);
widget_t make_region_save_as_dialog(bool managed);
widget_t make_new_file_dialog(bool managed);
void mouse_enter_label(void *r, int type);
void mouse_leave_label(void *r, int type);
Widget edit_header(snd_info *sp);
void save_edit_header_dialog_state(FILE *fd);
void set_open_file_play_button(bool val);
widget_t make_mix_file_dialog(bool managed);
widget_t make_insert_file_dialog(bool managed);
void clear_deleted_snd_info(void *dp);
void reflect_just_sounds(void);
void save_file_dialog_state(FILE *fd);
widget_t post_it(const char *subject, const char *str);
void post_it_append(const char *str);
void save_post_it_dialog_state(FILE *fd);
void reflect_region_in_save_as_dialog(void);
void reflect_selection_in_save_as_dialog(bool on);
void save_edits_now(snd_info *sp);
void changed_file_dialog(snd_info *sp);
void unpost_file_has_changed_if_any(snd_info *sp);
void unpost_unsaved_edits_if_any(snd_info *sp);
void reflect_save_as_src(bool val);
void reflect_save_as_auto_comment(bool val);
void reflect_save_as_sound_selection(const char *sound_name);
void add_drag_and_drop(Widget w, 
		       void (*drop_watcher)(Widget w, const char *message, Position x, Position y, void *data), 
		       void (*drag_watcher)(Widget w, const char *message, Position x, Position y, drag_style_t dtype, void *data), 
		       void *context);



/* -------- snd-xenv.c -------- */

axis_info *enved_make_axis(const char *name, graphics_context *ax, int ex0, int ey0, int width, int height, 
			   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax, printing_t printing);
void display_enved_env_with_selection(env *e, const char *name, int x0, int y0, int width, int height, bool dots, printing_t printing);
void set_enved_redo_sensitive(bool val);
void set_enved_revert_sensitive(bool val);
void set_enved_undo_sensitive(bool val);
void set_enved_save_sensitive(bool val);
void set_enved_show_sensitive(bool val);
void enved_reflect_selection(bool on);
void make_scrolled_env_list(void);
void enved_reflect_peak_env_completion(snd_info *sp);
void new_active_channel_alert(void);
void env_redisplay(void);
void env_redisplay_with_print(void);
void update_enved_background_waveform(chan_info *cp);
Widget create_envelope_editor(void);
void set_enved_clipping(bool val);
void reflect_enved_style(void);
void set_enved_base(mus_float_t val);
void set_enved_target(enved_target_t val);
void set_enved_with_wave(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void set_enved_filter_order(int order);
void color_enved_waveform(Pixel pix);



/* -------- snd-xmix.c -------- */

void reflect_mix_change(int mix_id);
Widget make_mix_dialog(void);
void reflect_mix_play_stop(void);
void make_mixer_icons_transparent_again(Pixel old_color, Pixel new_color);
int mix_dialog_mix(void);
void mix_dialog_set_mix(int id);


/* -------- snd-xprefs.c -------- */

widget_t make_preferences_dialog(void);

void g_init_motif(void);

#endif


