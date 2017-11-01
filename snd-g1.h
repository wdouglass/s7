#ifndef SND_G1_H
#define SND_G1_H

void recolor_everything(widget_t w, gpointer color);


/* -------- snd-ghelp.c -------- */

GtkWidget *snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap);
GtkWidget *snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, const char **xrefs, const char **urls);
int help_text_width(const char *txt, int start, int end);
void snd_help_append(const char *text);
void snd_help_back_to_top(void);



/* -------- snd-gdraw.c -------- */

void draw_line(graphics_context *ax, int x0, int y0, int x1, int y1);
void draw_lines(graphics_context *ax, point_t *points, int num);
void draw_points(graphics_context *ax, point_t *points, int num, int size);
void fill_rectangle(graphics_context *ax, int x0, int y0, int width, int height);
void erase_rectangle(chan_info *cp, graphics_context *ax, int x0, int y0, int width, int height);
void fill_polygon(graphics_context *ax, int points, ...);
void fill_polygons(graphics_context *ax, point_t *points, int num, int y0);
void fill_two_sided_polygons(graphics_context *ax, point_t *points, point_t *points1, int num);
void fill_polygon_from_array(graphics_context *ax, point_t *points, int npoints);
void draw_picture(graphics_context *ax, picture_t *src, gint xsrc, gint ysrc, gint xdest, gint ydest, gint width, gint height);
void draw_string(graphics_context *ax, int x0, int y0, const char *str, int len);
void draw_rotated_axis_label(chan_info *cp, graphics_context *ax, const char *text, gint x0, gint y0);
void draw_dot(graphics_context *ax, int x, int y, int size);
void draw_colored_lines(chan_info *cp, graphics_context *ax, point_t *points, int num, int *colors, int axis_y0, color_t default_color);
void setup_graphics_context(chan_info *cp, graphics_context *ax);
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
void view_color_orientation_callback(GtkWidget * w, gpointer info);
bool color_orientation_dialog_is_active(void);
GtkWidget *make_color_orientation_dialog(bool managed);
void reflect_spectro(void);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, int x, int y, int width, int height);
void draw_sono_rectangles(graphics_context *ax, int color, int jmax);
void draw_spectro_line(graphics_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void check_colormap_sizes(int size);
void initialize_colormap(void);
void reflect_color_list(bool setup_time);
void set_with_gl(bool val, bool with_dialogs);
void g_init_gxdraw(void);
gchar* scale_double_format_callback(GtkScale *w, gdouble val, gpointer data);



/* -------- snd-glistener.c -------- */

void color_listener(color_info *pix);
void color_listener_text(color_info *pix);
void handle_listener(bool new_state);
bool listener_exists(void);
int listener_height(void);
int listener_width(void);
void goto_listener(void);
int save_listener_text(FILE *fp);
void append_listener_text(int end, const char *msg);
void listener_append(const char *msg);
void listener_append_and_prompt(const char *msg);
void clear_listener(void);
void set_listener_text_font(void);
bool listener_colorized(void);
bool listener_set_colorized(bool val);
void g_init_gxlistener(void);



/* -------- snd-gmain.c -------- */

color_t get_in_between_color(color_t fg, color_t bg);
void snd_doit(int argc, char **argv);
void auto_update_restart(void);
void save_colors(FILE *Fp);



/* -------- snd-gmenu.c -------- */

GtkWidget *add_menu(void);
void reflect_play_selection_stop(void);
void g_init_gxmenu(void);
GtkWidget *menu_widget(int which_menu);
void check_menu_labels(int key, int state, bool extended);
GtkWidget *get_help_menu_widget(void);
void show_toolbar(void);
void hide_toolbar(void);
void add_tooltip(GtkWidget *w, const char *tip);
void post_basic_popup_menu(void *ev);
void post_lisp_popup_menu(void *ev);
void post_selection_popup_menu(void *ev);
widget_t make_file_print_dialog(bool managed, bool direct_to_printer);



/* -------- snd-gfft.c -------- */

void set_fft_window_beta(mus_float_t val);
void set_fft_window_alpha(mus_float_t val);
void set_transform_size(mus_long_t val);
void set_fft_window(mus_fft_window_t val);
void set_wavelet_type(int val);
GtkWidget *make_transform_dialog(bool managed);
bool transform_dialog_is_active(void);

void set_transform_type(int val);
void make_transform_type_list(void);
void set_spectrum_start(mus_float_t val);
void set_spectrum_end(mus_float_t val);

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
gboolean spin_button_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown);
gboolean spin_button_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown);
void post_fft_popup_menu(void *ev);



/* -------- snd-gregion.c -------- */

int update_region_browser(bool grf_too);
void reflect_play_region_stop(int n);
bool region_browser_is_active(void);
void delete_region_and_update_browser(int n);
void view_region_callback(GtkWidget *w, gpointer info);
void allocate_region_rows(int n);
bool region_dialog_is_active(void);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(void);
int region_dialog_region(void);
char *regrow_get_label(void *ur);
int regrow_get_pos(void *ur);
void g_init_gxregion(void);
void view_files_add_directory(widget_t dialog, const char *dirname);
bool view_files_has_files(void);
void view_files_callback(GtkWidget *w, gpointer info);
void view_files_unplay(void);


/* -------- snd-gxbitmaps.c -------- */

const char **snd_icon_bits(void);
enum {SND_PNG_LOCK, SND_PNG_STOP, SND_PNG_BOMB, SND_PNG_RIGHT_ARROW, SND_PNG_LEFT_ARROW, SND_PNG_BLANK, SND_PNG_SPEAKER};
cairo_surface_t *snd_icon(int which);



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
void phases_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b);



/* -------- snd-gchn.c -------- */

GtkWidget *channel_w(chan_info *cp);
GtkWidget *channel_f(chan_info *cp);
GtkWidget *channel_graph(chan_info *cp);
bool channel_graph_is_visible(chan_info *cp);
GtkWidget *channel_up_arrow(chan_info *cp);
GtkWidget *channel_down_arrow(chan_info *cp);
void channel_open_pane(chan_info *cp);
void resize_sx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_sy_and_zy(chan_info *cp);
void resize_sx_and_zx(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
void set_z_scrollbars(chan_info *cp, axis_info *ap);
void change_gzy(mus_float_t val, chan_info *cp);
mus_float_t gsy_value(chan_info *cp);
mus_float_t gsy_size(chan_info *cp);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
gboolean graph_key_press(GtkWidget *w, GdkEventKey *event, gpointer data);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, GtkWidget *main, fw_button_t arrows, bool with_events);
void set_peak_numbers_font(chan_info *cp, graphics_context *ax);
void set_bold_peak_numbers_font(chan_info *cp, graphics_context *ax);
void set_tiny_numbers_font(chan_info *cp, graphics_context *ax);
color_t get_foreground_color(graphics_context *ax);
void set_foreground_color(graphics_context *ax, color_info *color);
gc_t *copy_GC(chan_info *cp);
gc_t *erase_GC(chan_info *cp);
void cleanup_cw(chan_info *cp);
void change_channel_style(snd_info *sp, channel_style_t new_style);
void color_chan_components(color_t color, slider_choice_t which_component);
void color_unselected_graphs(color_t color);

void g_init_gxchn(void);



/* -------- snd-gfind.c -------- */

void edit_find_callback(GtkWidget *w, gpointer info);
void find_dialog(chan_info *cp);
void find_dialog_set_label(const char *str);
void stop_search_if_error(const char *msg, void *data);
void errors_to_find_text(const char *msg, void *data);
void find_dialog_stop_label(bool show_stop);
bool find_dialog_is_active(void);

void save_find_dialog_state(FILE *fd);
void g_init_gxfind(void);



/* -------- snd-gutils.c -------- */

bool set_tiny_font(const char *font);
bool set_listener_font(const char *font);
bool set_peaks_font(const char *font);
bool set_bold_peaks_font(const char *font);
bool set_axis_label_font(const char *font);
bool set_axis_numbers_font(const char *font);
int label_width(const char *txt, bool use_tiny_font);
int number_width(const char *num, bool use_tiny_font);
int number_height(PangoFontDescription *font);
int label_height(bool use_tiny_font);
int sg_text_width(const char *txt, PangoFontDescription *font);
int mark_name_width(const char *txt);
void clear_window(graphics_context *ax);
void highlight_color(GtkWidget *w);
void raise_dialog(GtkWidget *w);
void set_button_label(GtkWidget *label, const char *str);
void set_label(GtkWidget *label, const char *str);
void check_for_event(void);
void set_title(const char *title);
void goto_window(GtkWidget *text);

void gc_set_foreground(gc_t *gp, color_info *color);
void gc_set_background(gc_t *gp, color_info *color);
void gc_set_colors(gc_t *gp, color_info *col1, color_info *col2);
gc_t *gc_new(void);

void color_cursor(color_info *color);
void color_marks(color_info *color);
void color_selection(color_info *color);
void color_data(color_info *color);
void color_selected_data(color_info *color);
void color_graph(color_info *color);
void color_selected_graph(color_info *color);
void set_mix_color(color_info *color);
#if (!GTK_CHECK_VERSION(3, 92, 0))
  void sg_widget_modify_bg(GtkWidget *w, GtkStateType type, color_t color);
  void sg_widget_modify_fg(GtkWidget *w, GtkStateType type, color_t color);
  void sg_widget_modify_base(GtkWidget *w, GtkStateType type, color_t color);
  #define widget_modify_bg(W, T, C) sg_widget_modify_bg(W, T, C)
  #define widget_modify_fg(W, T, C) sg_widget_modify_fg(W, T, C)
  #define widget_modify_base(W, T, C) sg_widget_modify_base(W, T, C)
  #define sg_box_pack_start(Parent, Child, Expand, Fill, Pad) gtk_box_pack_start(Parent, Child, Expand, Fill, Pad)
  #define sg_box_pack_end(Parent, Child, Expand, Fill, Pad) gtk_box_pack_end(Parent, Child, Expand, Fill, Pad)
  #define sg_widget_set_events(Wid, Ev) gtk_widget_set_events(Wid, Ev)
  #define SG_RELIEF_HALF GTK_RELIEF_HALF
  #define sg_container_set_border_width(Container, Width) gtk_container_set_border_width(Container, Width)
#else
  #define widget_modify_bg(W, T, C)
  #define widget_modify_fg(W, T, C)
  #define widget_modify_base(W, T, C)
  #define sg_box_pack_start(Parent, Child, Expand, Fill, Pad) gtk_box_pack_start(Parent, Child)
  #define sg_box_pack_end(Parent, Child, Expand, Fill, Pad) gtk_box_pack_end(Parent, Child)
  #define sg_widget_set_events(Wid, Ev)
  #define SG_RELIEF_HALF GTK_RELIEF_NORMAL
  #define sg_container_set_border_width(Container, Width)
  guint sg_event_get_keyval(GdkEvent *e);
  guint sg_event_get_button(const GdkEvent *e);
#endif
color_t rgb_to_color(mus_float_t r, mus_float_t g, mus_float_t b);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  GdkColor *rgb_to_gdk_color(color_t col);
#endif
void add_highlight_button_style(GtkWidget *w);
void add_center_button_style(GtkWidget *w);
void add_toolbar_style(GtkWidget *w);
void add_menu_style(GtkWidget *w);
void add_paned_style(GtkWidget *w);
void add_red_scale_style(GtkWidget *w);
void add_green_scale_style(GtkWidget *w);
void add_blue_scale_style(GtkWidget *w);
void add_white_button_style(GtkWidget *w);
void add_listener_style(GtkWidget *w);
void add_dialog_style(GtkWidget *w);
void add_check_button_style(GtkWidget *w);
void add_entry_style(GtkWidget *w);

void recolor_graph(chan_info *cp, bool selected);
void set_sensitive(GtkWidget *wid, bool val);
void set_toggle_button(GtkWidget *wid, bool val, bool passed, void *data);
#if GTK_CHECK_VERSION(3, 0, 0)
int widget_height(GtkWidget *w);
int widget_width(GtkWidget *w);
#else
guint16 widget_height(GtkWidget *w);
guint16 widget_width(GtkWidget *w);
#endif
void set_widget_height(GtkWidget *w, guint16 height);
void set_widget_width(GtkWidget *w, guint16 width);
gint16 widget_x(GtkWidget *w);
gint16 widget_y(GtkWidget *w);
void set_widget_x(GtkWidget *w, gint16 x);
void set_widget_y(GtkWidget *w, gint16 y);
void set_widget_size(GtkWidget *w, guint16 width, guint16 height);
void set_widget_position(GtkWidget *w, gint16 x, gint16 y);
void set_user_data(GObject *obj, gpointer data);
void set_user_int_data(GObject *obj, int data);
void reset_user_int_data(GObject *obj, int data);
gpointer get_user_data(GObject *obj);
int get_user_int_data(GObject *obj);
void set_stock_button_label(GtkWidget *w, const char *new_label);
bool cursor_set_blinks(GtkWidget *w, bool blinks);

char *sg_get_text(GtkWidget *w, int start, int end);
void sg_text_insert(GtkWidget *w, const char *text);
GtkWidget *make_scrolled_text(GtkWidget *parent, bool editable, int add_choice, bool resize);
void sg_make_resizable(GtkWidget *w);
idle_t add_work_proc(GSourceFunc func, gpointer data);
GtkWidget *snd_gtk_dialog_new(void);
GtkWidget *snd_gtk_highlight_label_new(const char *label);
void widget_int_to_text(GtkWidget *w, int val);
void widget_mus_long_t_to_text(GtkWidget *w, mus_long_t val);
void ensure_scrolled_window_row_visible(widget_t list, int pos, int num_rows);

slist *slist_new_with_title_and_table_data(const char *title,
					   GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned,
					   int t1, int t2, int t3, int t4);
slist *slist_new(GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned);
slist *slist_new_with_title(const char *title, GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned);
void slist_clear(slist *lst);
void slist_append(slist *lst, const char *name);
void slist_moveto(slist *lst, int row);
void slist_select(slist *lst, int row);

#if GTK_CHECK_VERSION(3, 0, 0)
cairo_t *make_cairo(GdkWindow *win);
#else
cairo_t *make_cairo(GdkDrawable *win);
#endif
void free_cairo(cairo_t *cr);
void init_gtk(void);




/* -------- snd-gsnd.c -------- */

int control_panel_height(snd_info *sp);
GtkWidget *w_snd_pane(snd_info *sp);
GtkWidget *w_snd_pane_box(snd_info *sp);
GtkWidget *unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp);
void show_lock(snd_info *sp);
void hide_lock(snd_info *sp);
void start_bomb(snd_info *sp);
void stop_bomb(snd_info *sp);
void set_status(snd_info *sp, const char *str, bool update);
void set_play_button(snd_info *sp, bool val);
void play_button_pause(bool pausing);
void syncb(snd_info *sp, int on);
void set_amp(snd_info *sp, mus_float_t val);
mus_float_t amp_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval);
void set_expand(snd_info *sp, mus_float_t val);
void set_contrast(snd_info *sp, mus_float_t val);
void set_speed(snd_info *sp, mus_float_t val);
void set_revlen(snd_info *sp, mus_float_t val);
void set_revscl(snd_info *sp, mus_float_t val);
void set_filter_order(snd_info *sp, int order);
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
void color_filter_waveform(color_info *color);
snd_info *add_sound_window(char *filename, read_only_t read_only, file_info *hdr);
void set_sound_pane_file_label(snd_info *sp, const char *str);
void snd_info_cleanup(snd_info *sp);
void show_controls(snd_info *sp);
void hide_controls(snd_info *sp);
bool showing_controls(snd_info *sp);
void show_all_controls(void);
void hide_all_controls(void);
void start_progress_report(chan_info *cp);
void finish_progress_report(chan_info *cp);
void progress_report(chan_info *cp, mus_float_t pct);
void g_init_gxsnd(void);
void reflect_sound_selection(snd_info *sp);
void make_controls_dialog(void);
void update_sound_label(snd_info *sp);

GtkWidget *snd_entry_new(GtkWidget *container, GtkWidget *prev, snd_entry_bg_t with_white_background);
GtkWidget *snd_entry_new_with_size(GtkWidget *container, int size);
void connect_mouse_to_text(GtkWidget *text);



/* -------- snd-gmix.c -------- */

void reflect_mix_change(int mix_id);
GtkWidget *make_mix_dialog(void);
void reflect_mix_play_stop(void);
int mix_dialog_mix(void);
void mix_dialog_set_mix(int id);



/* -------- snd-genv.c -------- */

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
GtkWidget *create_envelope_editor(void);
void set_enved_clipping(bool val);
void reflect_enved_style(void);
void set_enved_base(mus_float_t val);
void set_enved_target(enved_target_t val);
void set_enved_with_wave(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void set_enved_filter_order(int order);
void color_enved_waveform(color_info *pix);
void g_init_gxenv(void);



/* -------- snd-gfile.c -------- */

void cleanup_file_monitor(void);
void *unmonitor_file(void *watcher);
void monitor_sound(snd_info *sp);

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, mus_header_t *header_type, 
				       mus_sample_t *sample_type, mus_long_t *location, mus_long_t *samples, int min_chan);
widget_t make_open_file_dialog(read_only_t read_only, bool managed);
widget_t make_sound_save_as_dialog(bool managed);
void make_channel_extract_dialog(int chan);
widget_t make_selection_save_as_dialog(bool managed);
widget_t make_region_save_as_dialog(bool managed);
widget_t make_new_file_dialog(bool managed);
widget_t make_mix_file_dialog(bool managed);
widget_t make_insert_file_dialog(bool managed);
GtkWidget *edit_header(snd_info *sp);
void save_edit_header_dialog_state(FILE *fd);
void set_open_file_play_button(bool val);
void g_init_gxfile(void);
void clear_deleted_snd_info(void *fd);
void reflect_just_sounds(void);
void save_file_dialog_state(FILE *fd);
widget_t post_it(const char *subject, const char *str);
void post_it_append(const char *str);
void save_post_it_dialog_state(FILE *fd);
void reflect_region_in_save_as_dialog(void);
void reflect_selection_in_save_as_dialog(bool on);
void save_edits_now(snd_info *sp);
void unpost_unsaved_edits_if_any(snd_info *sp);
void unpost_file_has_changed_if_any(snd_info *sp);
void changed_file_dialog(snd_info *sp);
void reflect_save_as_src(bool val);
void reflect_save_as_auto_comment(bool val);
void reflect_save_as_sound_selection(const char *sound_name);
void add_drag_and_drop(GtkWidget *w, 
		       void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data), 
		       void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data), 
		       void *context);


/* -------- snd-gprefs.c -------- */

widget_t make_preferences_dialog(void);

#endif

