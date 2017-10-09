#ifndef SND_NOGUI1_H
#define SND_NOGUI1_H

/* -------- snd-xhelp.c -------- */

int snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap);
int snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, const char **xrefs, const char **urls);
int help_text_width(const char *txt, int start, int end);
void snd_help_append(const char *text);
void snd_help_back_to_top(void);


/* -------- snd-xdraw.c -------- */

void draw_line(graphics_context *ax, int x0, int y0, int x1, int y1);
void fill_rectangle(graphics_context *ax, int x0, int y0, int width, int height);
void erase_rectangle(chan_info *cp, graphics_context *ax, int x0, int y0, int width, int height);
void fill_polygon(graphics_context *ax, int points, ...);
void fill_polygons(graphics_context *ax, point_t *points, int num, int y0);
void fill_two_sided_polygons(graphics_context *ax, point_t *points, point_t *points1, int num);
void draw_string(graphics_context *ax, int x0, int y0, const char *str, int len);
void draw_dot(graphics_context *ax, int x, int y, int size);
void setup_graphics_context(chan_info *cp, graphics_context *ax);
void draw_spectro_line(graphics_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, int x, int y, int width, int height);
void draw_sono_rectangles(graphics_context *ax, int color, int jmax);
void draw_colored_lines(chan_info *cp, graphics_context *ax, point_t *points, int num, int *colors, int axis_y0, color_t default_color);
widget_t make_color_orientation_dialog(bool managed);
void set_color_scale(mus_float_t val);
void set_color_inverted(bool val);
void set_color_cutoff(mus_float_t val);
void set_spectro_hop(int val);
void set_spectro_x_angle(mus_float_t val);
void set_spectro_y_angle(mus_float_t val);
void set_spectro_z_angle(mus_float_t val);
void set_spectro_x_scale(mus_float_t val);
void set_spectro_y_scale(mus_float_t val);
void set_spectro_z_scale(mus_float_t val);
bool color_orientation_dialog_is_active(void);
void reflect_spectro(void);
void set_with_gl(bool val, bool dpys);


/* -------- snd-xfind.c -------- */

bool find_dialog_is_active(void);
void save_find_dialog_state(FILE *fd);


/* -------- snd-xlistener.c -------- */

void append_listener_text(int end, const char *msg);
int save_listener_text(FILE *fp);
void listener_append_and_prompt(const char *msg);
void goto_listener(void);
void listener_append(const char *msg);
void handle_listener(bool new_state);
bool listener_exists(void);
int listener_height(void);
int listener_width(void);
void clear_listener(void);


/* -------- snd-xmenu.c -------- */

void check_menu_labels(int key, int state, bool extended);
void reflect_play_selection_stop(void);
int menu_widget(int which_menu);
void set_button_label(int label, const char *str);
void post_basic_popup_menu(void *ev);
void post_lisp_popup_menu(void *ev);
void post_fft_popup_menu(void *ev);
void post_selection_popup_menu(void *ev);



/* -------- snd-xmain.c -------- */

color_t get_in_between_color(color_t fg, color_t bg);
void snd_doit(int argc, char **argv);
void auto_update_restart(void);
void save_colors(FILE *Fp);


/* -------- snd-xfft.c -------- */

void set_fft_window_beta(mus_float_t val);
void set_fft_window_alpha(mus_float_t val);
void set_transform_size(mus_long_t val);
void set_fft_window(mus_fft_window_t val);
void set_wavelet_type(int val);
int make_transform_dialog(bool managed);
bool transform_dialog_is_active(void);
void set_transform_type(int val);
void set_show_transform_peaks(bool val);
void set_fft_log_magnitude(bool val);
void set_fft_with_phases(bool val);
void set_fft_log_frequency(bool val);
void set_transform_normalization(fft_normalize_t val);
void set_show_selection_transform(bool show);
void set_transform_graph_type(graph_type_t val);

void set_spectrum_start(mus_float_t val);
void set_spectrum_end(mus_float_t val);

void reflect_peaks_in_transform_dialog(void);
void reflect_log_freq_start_in_transform_dialog(void);
void reflect_min_db_in_transform_dialog(void);
void make_transform_type_list(void);


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


/* -------- snd-xutils.c -------- */

void goto_window(int text);
bool set_tiny_font(const char *font);
bool set_listener_font(const char *font);
bool set_peaks_font(const char *font);
bool set_bold_peaks_font(const char *font);
bool set_axis_label_font(const char *font);
bool set_axis_numbers_font(const char *font);
int label_width(const char *txt, bool use_tiny_font);
int number_width(const char *num, bool use_tiny_font);
int number_height(int fnt);
int label_height(bool use_tiny_font);
int mark_name_width(const char *txt);
void clear_window(graphics_context *ax);
void set_title(const char *title);
void check_for_event(void);
void recolor_graph(chan_info *cp, bool selected);
void set_sensitive(int wid, bool val);
void set_toggle_button(int wid, bool val, bool passed, void *data);
int widget_height(int w);
int widget_width(int w);
void set_widget_size(int w, int width, int height);
int widget_x(int w);
int widget_y(int w);
void set_widget_x(int w, int x);
void set_widget_y(int w, int y);
void set_mix_color(int color);
void ensure_scrolled_window_row_visible(widget_t list, int pos, int num_rows);


/* -------- snd-xchn.c -------- */

int channel_w(chan_info *cp);
int channel_f(chan_info *cp);
int channel_graph(chan_info *cp);
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
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
void set_peak_numbers_font(chan_info *cp, graphics_context *ax);
void set_bold_peak_numbers_font(chan_info *cp, graphics_context *ax);
void set_tiny_numbers_font(chan_info *cp, graphics_context *ax);
color_t get_foreground_color(graphics_context *ax);
void set_foreground_color(graphics_context *ax, int color);
void cleanup_cw(chan_info *cp);
void get_current_color(int colormap, int j, rgb_t *r, rgb_t *g, rgb_t *b);
void change_channel_style(snd_info *sp, channel_style_t new_style);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, widget_t main, fw_button_t arrows, bool with_events);


/* -------- snd-xsnd.c -------- */

void set_status(snd_info *sp, const char *str, bool update);
void snd_info_cleanup(snd_info *sp);
void set_amp(snd_info *sp, mus_float_t val);
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
void show_controls(snd_info *sp);
void hide_controls(snd_info *sp);
bool showing_controls(snd_info *sp);
void start_progress_report(chan_info *cp);
void finish_progress_report(chan_info *cp);
void progress_report(chan_info *cp, mus_float_t pct);
void reflect_sound_selection(snd_info *sp);
void update_sound_label(snd_info *sp);

/* -------- snd-xfile.c -------- */

void cleanup_file_monitor(void);
void *unmonitor_file(void *watcher);
void monitor_sound(snd_info *sp);

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, mus_header_t *header_type, 
				       mus_sample_t *sample_type, mus_long_t *location, mus_long_t *samples, int min_chan);
widget_t make_new_file_dialog(bool managed);
int edit_header(snd_info *sp);
void save_edit_header_dialog_state(FILE *fd);
widget_t make_open_file_dialog(read_only_t read_only, bool managed);
void set_open_file_play_button(bool val);
widget_t make_selection_save_as_dialog(bool managed);
widget_t make_region_save_as_dialog(bool managed);
widget_t make_sound_save_as_dialog(bool managed);
widget_t make_mix_file_dialog(bool managed);
widget_t make_insert_file_dialog(bool managed);
widget_t post_it(const char *subject, const char *str);
void save_post_it_dialog_state(FILE *fd);
void reflect_just_sounds(void);
void save_file_dialog_state(FILE *fd);
void reflect_selection_in_save_as_dialog(bool on);
void reflect_save_as_src(bool val);
void reflect_save_as_auto_comment(bool val);
void reflect_save_as_sound_selection(const char *sound_name);


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
int create_envelope_editor(void);
void set_enved_clipping(bool val);
void reflect_enved_style(void);
void set_enved_base(mus_float_t val);
void set_enved_target(enved_target_t val);
void set_enved_with_wave(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void set_enved_filter_order(int order);



/* -------- snd-xmix.c -------- */

void reflect_mix_change(int mix_id);
int make_mix_dialog(void);
void reflect_mix_play_stop(void);
int mix_dialog_mix(void);
void mix_dialog_set_mix(int id);



/* -------- snd-xprint.c -------- */

widget_t make_file_print_dialog(bool managed, bool direct_to_printer);


/* -------- snd-xprefs.c -------- */

widget_t make_preferences_dialog(void);

#endif


