#ifndef SND_MENU_H
#define SND_MENU_H

#define I_LINES_OR_DOTS "Dots or lines"
#define I_CHANNEL_LAYOUT "Channel layout"
#define I_ZOOM_CENTERS_ON "Zoom centers on"
#define I_AXIS_LAYOUT "Axis layout"

enum {m_menu,
        f_menu, f_cascade_menu,
          f_open_menu, f_open_recent_menu, f_open_recent_cascade_menu, f_close_menu, f_close_all_menu, f_save_menu, f_save_as_menu, f_revert_menu, f_exit_menu, f_new_menu,
          f_view_menu, f_print_menu, f_mix_menu, f_insert_menu, f_update_menu, f_sep_menu,
        e_menu, e_cascade_menu,
          e_cut_menu, e_paste_menu, e_mix_menu, e_play_menu, e_save_as_menu, e_undo_menu,
          e_redo_menu, e_find_menu, e_env_menu, e_header_menu, e_select_all_menu, e_unselect_menu,
          e_select_sep_menu, e_edit_sep_menu,
        h_menu, h_cascade_menu,
          h_about_snd_menu, h_fft_menu, h_find_menu, h_undo_menu, h_sync_menu, h_controls_menu,
          h_env_menu, h_marks_menu, h_sound_files_menu, h_init_file_menu,
          h_mix_menu, h_keys_menu, 
          h_play_menu, h_save_menu, h_resample_menu, h_filter_menu, h_insert_menu, 
          h_delete_menu, h_reverb_menu, h_debug_menu, h_region_menu, h_selection_menu, h_colors_menu,
        o_menu, o_cascade_menu,
          o_transform_menu, o_controls_menu,
          o_save_state_menu, o_sep_menu, o_preferences_menu,
        v_menu, v_cascade_menu,
          v_graph_style_menu, v_graph_style_cascade_menu,
            v_lines_menu, v_dots_menu, v_filled_menu, v_dots_and_lines_menu, v_lollipops_menu,
            v_zero_menu, v_cursor_menu, v_inset_menu, v_controls_menu, v_listener_menu,
          v_region_menu,
          v_combine_menu, v_combine_cascade_menu,
            v_combine_separate_menu, v_combine_combined_menu, v_combine_superimposed_menu,
          v_color_orientation_menu, 
          v_files_menu, v_mix_dialog_menu, 
          v_x_axis_menu, v_x_axis_cascade_menu,
            v_x_axis_seconds_menu, v_x_axis_samples_menu, v_x_axis_percentage_menu, v_x_axis_beats_menu, v_x_axis_measures_menu, v_x_axis_clock_menu,
          v_axes_menu, v_axes_cascade_menu,
          v_no_axes_menu, v_all_axes_menu, v_just_x_axis_menu, v_all_axes_unlabelled_menu, v_just_x_axis_unlabelled_menu, v_bare_x_axis_menu,
          v_focus_style_menu, v_focus_cascade_menu,
            v_focus_right_menu, v_focus_left_menu, v_focus_middle_menu, v_focus_active_menu,
          v_grid_menu,
        v_sep2_menu,
      NUM_MENU_WIDGETS
};

#define main_menu (ss->mw[m_menu])

#define file_menu (ss->mw[f_menu])
#define file_cascade_menu (ss->mw[f_cascade_menu])
#define file_open_menu (ss->mw[f_open_menu])
#define file_open_recent_menu (ss->mw[f_open_recent_menu])
#define file_open_recent_cascade_menu (ss->mw[f_open_recent_cascade_menu])
#define file_close_menu (ss->mw[f_close_menu])
#define file_close_all_menu (ss->mw[f_close_all_menu])
#define file_save_menu (ss->mw[f_save_menu])
#define file_save_as_menu (ss->mw[f_save_as_menu])
#define file_revert_menu (ss->mw[f_revert_menu])
#define file_exit_menu (ss->mw[f_exit_menu])
#define file_new_menu (ss->mw[f_new_menu])
#define file_view_menu (ss->mw[f_view_menu])
#define file_print_menu (ss->mw[f_print_menu])
#define file_mix_menu (ss->mw[f_mix_menu])
#define file_insert_menu (ss->mw[f_insert_menu])
#define file_update_menu (ss->mw[f_update_menu])
#define file_sep_menu (ss->mw[f_sep_menu])

#define edit_menu (ss->mw[e_menu])
#define edit_cascade_menu (ss->mw[e_cascade_menu])
#define edit_cut_menu (ss->mw[e_cut_menu])
#define edit_paste_menu (ss->mw[e_paste_menu])
#define edit_mix_menu (ss->mw[e_mix_menu])
#define edit_play_menu (ss->mw[e_play_menu])
#define edit_save_as_menu (ss->mw[e_save_as_menu])
#define edit_undo_menu (ss->mw[e_undo_menu])
#define edit_redo_menu (ss->mw[e_redo_menu])
#define edit_find_menu (ss->mw[e_find_menu])
#define edit_env_menu (ss->mw[e_env_menu])
#define edit_header_menu (ss->mw[e_header_menu])
#define edit_select_all_menu (ss->mw[e_select_all_menu])
#define edit_unselect_menu (ss->mw[e_unselect_menu])
#define edit_select_sep_menu (ss->mw[e_select_sep_menu])
#define edit_edit_sep_menu (ss->mw[e_edit_sep_menu])

#define help_menu (ss->mw[h_menu])
#define help_cascade_menu (ss->mw[h_cascade_menu])
#define help_about_snd_menu (ss->mw[h_about_snd_menu])
#define help_fft_menu (ss->mw[h_fft_menu])
#define help_find_menu (ss->mw[h_find_menu])
#define help_undo_menu (ss->mw[h_undo_menu])
#define help_sync_menu (ss->mw[h_sync_menu])
#define help_controls_menu (ss->mw[h_controls_menu])
#define help_env_menu (ss->mw[h_env_menu])
#define help_marks_menu (ss->mw[h_marks_menu])
#define help_sound_files_menu (ss->mw[h_sound_files_menu])
#define help_init_file_menu (ss->mw[h_init_file_menu])
#define help_mix_menu (ss->mw[h_mix_menu])
#define help_keys_menu (ss->mw[h_keys_menu])
#define help_play_menu (ss->mw[h_play_menu])
#define help_save_menu (ss->mw[h_save_menu])
#define help_resample_menu (ss->mw[h_resample_menu])
#define help_filter_menu (ss->mw[h_filter_menu])
#define help_insert_menu (ss->mw[h_insert_menu])
#define help_delete_menu (ss->mw[h_delete_menu])
#define help_reverb_menu (ss->mw[h_reverb_menu])
#define help_debug_menu (ss->mw[h_debug_menu])
#define help_region_menu (ss->mw[h_region_menu])
#define help_selection_menu (ss->mw[h_selection_menu])
#define help_colors_menu (ss->mw[h_colors_menu])

#define options_menu (ss->mw[o_menu])
#define options_cascade_menu (ss->mw[o_cascade_menu])
#define options_transform_menu (ss->mw[o_transform_menu])
#define options_save_state_menu (ss->mw[o_save_state_menu])
#define options_sep_menu (ss->mw[o_sep_menu])
#define options_preferences_menu (ss->mw[o_preferences_menu])
#define options_controls_menu (ss->mw[o_controls_menu])

#define view_menu (ss->mw[v_menu])
#define view_cascade_menu (ss->mw[v_cascade_menu])
#define view_graph_style_menu (ss->mw[v_graph_style_menu])
#define view_graph_style_cascade_menu (ss->mw[v_graph_style_cascade_menu])
#define view_lines_menu (ss->mw[v_lines_menu])
#define view_dots_menu (ss->mw[v_dots_menu])
#define view_filled_menu (ss->mw[v_filled_menu])
#define view_dots_and_lines_menu (ss->mw[v_dots_and_lines_menu])
#define view_lollipops_menu (ss->mw[v_lollipops_menu])
#define view_zero_menu (ss->mw[v_zero_menu])
#define view_cursor_menu (ss->mw[v_cursor_menu])
#define view_inset_menu (ss->mw[v_inset_menu])
#define view_controls_menu (ss->mw[v_controls_menu])
#define view_listener_menu (ss->mw[v_listener_menu])
#define view_region_menu (ss->mw[v_region_menu])
#define view_combine_menu (ss->mw[v_combine_menu])
#define view_combine_cascade_menu (ss->mw[v_combine_cascade_menu])
#define view_combine_separate_menu (ss->mw[v_combine_separate_menu])
#define view_combine_combined_menu (ss->mw[v_combine_combined_menu])
#define view_combine_superimposed_menu (ss->mw[v_combine_superimposed_menu])
#define view_color_orientation_menu (ss->mw[v_color_orientation_menu])
#define view_files_menu (ss->mw[v_files_menu])
#define view_mix_dialog_menu (ss->mw[v_mix_dialog_menu])
#define view_x_axis_menu (ss->mw[v_x_axis_menu])
#define view_x_axis_cascade_menu (ss->mw[v_x_axis_cascade_menu])
#define view_x_axis_seconds_menu (ss->mw[v_x_axis_seconds_menu])
#define view_x_axis_clock_menu (ss->mw[v_x_axis_clock_menu])
#define view_x_axis_samples_menu (ss->mw[v_x_axis_samples_menu])
#define view_x_axis_percentage_menu (ss->mw[v_x_axis_percentage_menu])
#define view_x_axis_beats_menu (ss->mw[v_x_axis_beats_menu])
#define view_x_axis_measures_menu (ss->mw[v_x_axis_measures_menu])
#define view_axes_menu (ss->mw[v_axes_menu])
#define view_axes_cascade_menu (ss->mw[v_axes_cascade_menu])
#define view_no_axes_menu (ss->mw[v_no_axes_menu])
#define view_all_axes_menu (ss->mw[v_all_axes_menu])
#define view_just_x_axis_menu (ss->mw[v_just_x_axis_menu])
#define view_all_axes_unlabelled_menu (ss->mw[v_all_axes_unlabelled_menu])
#define view_just_x_axis_unlabelled_menu (ss->mw[v_just_x_axis_unlabelled_menu])
#define view_bare_x_axis_menu (ss->mw[v_bare_x_axis_menu])
#define view_sep2_menu (ss->mw[v_sep2_menu])
#define view_focus_style_menu (ss->mw[v_focus_style_menu])
#define view_focus_cascade_menu (ss->mw[v_focus_cascade_menu])
#define view_focus_right_menu (ss->mw[v_focus_right_menu])
#define view_focus_left_menu (ss->mw[v_focus_left_menu])
#define view_focus_middle_menu (ss->mw[v_focus_middle_menu])
#define view_focus_active_menu (ss->mw[v_focus_active_menu])
#define view_grid_menu (ss->mw[v_grid_menu])

void edit_menu_update(void);
void view_menu_update(void);
void file_menu_update(void);
void update_file_from_menu(void);
void revert_file_from_menu(void);
void save_state_from_menu(void);
void unprotect_callback(int slot);

int g_add_to_main_menu(const char *label, int slot);
widget_t g_add_to_menu(int which_menu, const char *label, int callb, int position);
int g_remove_from_menu(int which_menu, const char *label);
void g_menu_callback(int callb);

#endif
