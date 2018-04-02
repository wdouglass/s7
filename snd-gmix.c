#include "snd.h"

/* ---------------- mix dialog ---------------- */

static GtkWidget *mix_dialog = NULL;
static int mix_dialog_id = INVALID_MIX_ID, old_mix_dialog_id = INVALID_MIX_ID;
static env *dialog_env = NULL;

static bool dragging = false;
static int edpos_before_drag;
static with_hook_t hookable_before_drag;
static mus_long_t drag_beg = 0, drag_end = 0;

static void start_dragging(int mix_id) 
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);
  edpos_before_drag = cp->edit_ctr;
  hookable_before_drag = cp->hookable;
  cp->hookable = WITHOUT_HOOK;
  dragging = true;
  drag_beg = mix_position_from_id(mix_id);
  drag_end = drag_beg + mix_length_from_id(mix_id);
  start_dragging_syncd_mixes(mix_id);
}


static void keep_dragging(int mix_id) 
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);
  cp->edit_ctr = edpos_before_drag;
  keep_dragging_syncd_mixes(mix_id);
}


static void stop_dragging(int mix_id) 
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);
  undo_edit(cp, 1);
  cp->hookable = hookable_before_drag;
  dragging = false;
  stop_dragging_syncd_mixes(mix_id);
}



/* -------- speed -------- */

static GtkWidget *w_speed, *w_speed_label, *w_speed_number, *w_speed_form;
#if (!GTK_CHECK_VERSION(3, 92, 1))
static GtkWidget *w_speed_event, *w_speed_label_event;
#endif
static GtkAdjustment *w_speed_adj;
static bool speed_pressed = false, speed_dragged = false;
/* can't use value_changed on adjustment and motion event happens even when the mouse merely moves across the slider without dragging */

static speed_style_t gmix_speed_control_style = SPEED_CONTROL_AS_FLOAT;
#if WITH_AUDIO
  static graphics_context *mix_play_ax = NULL;
#endif

static mus_float_t speed_to_scrollbar(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}


static mus_float_t scrollbar_to_speed(mus_float_t scroll)
{
  return(exp((scroll * (log(speed_control_max(ss)) - log(speed_control_min(ss))) / 0.9) + log(speed_control_min(ss))));
}


static mus_float_t set_speed_label(GtkWidget *label, mus_float_t in_speed)
{
  mus_float_t speed;
  char speed_number_buffer[6];
  speed = speed_changed(in_speed,
			speed_number_buffer,
			gmix_speed_control_style,
			speed_control_tones(ss),
			6);
  gtk_label_set_text(GTK_LABEL(label), speed_number_buffer);
  return(speed);
}


static void reflect_mix_speed(mus_float_t speed)
{
  ADJUSTMENT_SET_VALUE(w_speed_adj, speed_to_scrollbar(speed_control_min(ss), set_speed_label(w_speed_number, speed), speed_control_max(ss)));
  /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_speed_adj)); */
}


static gboolean mix_speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* label click -- not part of slider stuff */
  speed_pressed = false;
  speed_dragged = false;
  if (!(mix_is_active(mix_dialog_id))) return(false);
  mix_set_speed_edit(mix_dialog_id, 1.0);
  syncd_mix_set_speed(mix_dialog_id, 1.0);
  after_mix_edit(mix_dialog_id);
  after_syncd_mix_edit(mix_dialog_id);
  reflect_mix_speed(1.0);
  return(false);
}


static gboolean speed_label_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* (number) label click -- not part of slider stuff */
  speed_pressed = false;
  speed_dragged = false;
  if (!(mix_is_active(mix_dialog_id))) return(false);
  switch (gmix_speed_control_style)
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    gmix_speed_control_style = SPEED_CONTROL_AS_RATIO;    break;
    case SPEED_CONTROL_AS_RATIO:    gmix_speed_control_style = SPEED_CONTROL_AS_SEMITONE; break;
    case SPEED_CONTROL_AS_SEMITONE: gmix_speed_control_style = SPEED_CONTROL_AS_FLOAT;    break;
    }
  set_speed_label(w_speed_number, mix_speed_from_id(mix_dialog_id));
  return(false);
}


static gboolean speed_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  mus_float_t speed;
  mus_long_t beg, end;

  if (!speed_pressed) {speed_dragged = false; return(false);}
  speed_dragged = true;

  if (!(mix_is_active(mix_dialog_id))) return(false);
  if (!dragging) 
    start_dragging(mix_dialog_id);
  else keep_dragging(mix_dialog_id);

  speed = set_speed_label(w_speed_number, scrollbar_to_speed(ADJUSTMENT_VALUE(w_speed_adj)));
  mix_set_speed_edit(mix_dialog_id, speed);

  beg = mix_position_from_id(mix_dialog_id);
  end = beg + mix_length_from_id(mix_dialog_id);
  if (drag_beg > beg) drag_beg = beg;
  if (drag_end < end) drag_end = end;

  mix_display_during_drag(mix_dialog_id, drag_beg, drag_end);
  syncd_mix_set_speed(mix_dialog_id, speed);
  return(false);
}


static gboolean mix_speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data) 
{
  mus_float_t speed;

  speed_pressed = false;
  speed_dragged = false;

  if (!(mix_is_active(mix_dialog_id))) return(false);
  if (dragging)
    stop_dragging(mix_dialog_id);

  speed = set_speed_label(w_speed_number, scrollbar_to_speed(ADJUSTMENT_VALUE(w_speed_adj)));
  mix_set_speed_edit(mix_dialog_id, speed);
  syncd_mix_set_speed(mix_dialog_id, speed);
  after_mix_edit(mix_dialog_id);
  after_syncd_mix_edit(mix_dialog_id);
  return(false);
}


static gboolean speed_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data) 
{
  speed_dragged = false;
  speed_pressed = true;
  return(false);
}


/* -------- amp -------- */

static GtkWidget *w_amp, *w_amp_label, *w_amp_number, *w_amp_form;
#if (!GTK_CHECK_VERSION(3, 92, 1))
static GtkWidget *w_amp_event;
#endif
static GtkAdjustment *w_amp_adj;

static mus_float_t scrollbar_to_amp(mus_float_t val)
{
  if (val <= 0.0) 
    return(amp_control_min(ss));
  if (val >= 0.9) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9))
    return((((val / (0.5 * 0.9)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9)) + amp_control_min(ss));
}


static bool amp_pressed = false, amp_dragged = false;

static void reflect_mix_amp(mus_float_t val)
{
  char sfs[6];
  ADJUSTMENT_SET_VALUE(w_amp_adj, amp_to_scroll(amp_control_min(ss), val, amp_control_max(ss)));
  /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adj)); */
  snprintf(sfs, 6, "%.2f", val);
  gtk_label_set_text(GTK_LABEL(w_amp_number), sfs);
}


static gboolean mix_amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  amp_dragged = false;
  amp_pressed = false;
  if (!(mix_is_active(mix_dialog_id))) return(false);
  reflect_mix_amp(1.0);
  mix_set_amp_edit(mix_dialog_id, 1.0);
  syncd_mix_set_amp(mix_dialog_id, 1.0);
  after_mix_edit(mix_dialog_id);
  after_syncd_mix_edit(mix_dialog_id);
  ADJUSTMENT_SET_VALUE(w_amp_adj, amp_to_scroll(amp_control_min(ss), 1.0, amp_control_max(ss)));
  /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adj)); */
  return(false);
}


static gboolean amp_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  mus_float_t amp;
  if (!amp_pressed) {amp_dragged = false; return(false);}
  amp_dragged = true;
  if (!(mix_is_active(mix_dialog_id))) return(false);
  if (!dragging) 
    start_dragging(mix_dialog_id);
  else keep_dragging(mix_dialog_id);
  amp = scrollbar_to_amp(ADJUSTMENT_VALUE(w_amp_adj));
  reflect_mix_amp(amp);
  mix_set_amp_edit(mix_dialog_id, amp);
  mix_display_during_drag(mix_dialog_id, drag_beg, drag_end);
  syncd_mix_set_amp(mix_dialog_id, amp);
  return(false);
}


static gboolean mix_amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  mus_float_t amp;
  dragging = false;
  amp_pressed = false;
  amp_dragged = false;
  if (!(mix_is_active(mix_dialog_id))) return(false);
  if (dragging)
    stop_dragging(mix_dialog_id);
  amp = scrollbar_to_amp(ADJUSTMENT_VALUE(w_amp_adj));
  reflect_mix_amp(amp);
  mix_set_amp_edit(mix_dialog_id, amp);
  syncd_mix_set_amp(mix_dialog_id, amp);
  after_mix_edit(mix_dialog_id);
  after_syncd_mix_edit(mix_dialog_id);
  return(false);
}


static gboolean amp_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  amp_pressed = true;
  amp_dragged = false;
  return(false);
}



/* -------- amp-env -------- */

static GtkWidget *w_env_frame, *w_env;
static graphics_context *ax = NULL;
static gc_t *cur_gc;
static env_editor *spf = NULL;
static bool with_mix_background_wave = false;

static void show_mix_background_wave(int mix_id)
{
  env_editor *e;
  int pts;
  bool two_sided = false;
  e = spf;
  if (!e) return;
  pts = prepare_mix_dialog_waveform(mix_id, e->axis, &two_sided);
  if (pts > 0)
    {
      gc_set_foreground(ax->gc, ss->enved_waveform_color);
      if (two_sided)
	draw_both_grf_points(1, ax, pts, GRAPH_LINES);
      else draw_grf_points(1, ax, pts, e->axis, ungrf_y(e->axis, 0.0), GRAPH_LINES);
      gc_set_foreground(ax->gc, ss->black);
    }
}


static void mix_amp_env_resize(GtkWidget *w)
{
  if (!(mix_is_active(mix_dialog_id))) return;
  if (!ax)
    {
      cur_gc = gc_new();
      gc_set_background(cur_gc, ss->graph_color);
      gc_set_foreground(cur_gc, ss->data_color);
      ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      ax->wn = WIDGET_TO_WINDOW(w_env);
      ax->w = w_env;
      ax->gc = cur_gc;
    }
  ss->cr = make_cairo(ax->wn);
  cairo_push_group(ss->cr);

  /* erase previous */
  cairo_set_source_rgba(ss->cr, cur_gc->bg_color->red, cur_gc->bg_color->green, cur_gc->bg_color->blue, cur_gc->bg_color->alpha);
  cairo_rectangle(ss->cr, 0, 0, widget_width(w), widget_height(w));
  cairo_fill(ss->cr);

  spf->with_dots = true;
  env_editor_display_env(spf, dialog_env, ax, "mix env", 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);
  if (with_mix_background_wave)
    show_mix_background_wave(mix_dialog_id);

  cairo_pop_group_to_source(ss->cr);
  cairo_paint(ss->cr);
  free_cairo(ss->cr);
  ss->cr = NULL;
}


static gboolean mix_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (!(mix_is_active(mix_dialog_id))) return(false);
  spf->with_dots = false;
  if (env_editor_button_press(spf, (int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), EVENT_TIME(ev), dialog_env))
    mix_amp_env_resize(w);
  return(false);
}


static gboolean mix_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (!(mix_is_active(mix_dialog_id))) return(false);
  env_editor_button_release(spf, dialog_env);
  mix_amp_env_resize(w);
  return(false);
}


static gboolean mix_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  if (!(mix_is_active(mix_dialog_id))) return(false);
  if (BUTTON1_PRESSED(EVENT_STATE(ev)))
    {
      int x, y;
      GdkModifierType state;
      if (EVENT_IS_HINT(ev))
	window_get_pointer(ev, &x, &y, &state);
      else
	{
	  x = (int)(EVENT_X(ev));
	  y = (int)(EVENT_Y(ev));
	}
      spf->with_dots = false;
      env_editor_button_motion(spf, x, y, EVENT_TIME(ev), dialog_env);
      mix_amp_env_resize(w);
    }
  return(false);
}


static gboolean mix_amp_env_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  mix_amp_env_resize(w);
  return(false);
}


static gboolean mix_amp_env_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  if (!(mix_is_active(mix_dialog_id))) return(false);
  mix_amp_env_resize(w);
  return(false);
}


static GtkWidget *w_id = NULL, *w_beg = NULL, *w_id_label = NULL;
#if WITH_AUDIO
  static GtkWidget *mix_play = NULL;
#endif

static bool id_changed = false;

static GtkWidget *error_frame = NULL, *error_label = NULL;

static void clear_mix_error(void)
{
  if ((error_frame) && (widget_is_active(error_frame)))
    gtk_widget_hide(error_frame);
}


static gint unpost_mix_error(gpointer data)
{
  clear_mix_error();
  return(0);
}


static void errors_to_mix_text(const char *msg, void *data)
{
  gtk_label_set_text(GTK_LABEL(error_label), msg);
  gtk_widget_show(error_frame);
  g_timeout_add_full(0, (guint32)5000, unpost_mix_error, NULL, NULL);
}


static void id_activated(GtkWidget *w, gpointer context)
{
  char *val;
  id_changed = false;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_id));
  if (val)
    {
      int id;
      /* look for a mix name first, then a number */
      id = mix_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_mix_text, NULL);
	  id = string_to_int(val, 0, "id");
	  redirect_errors_to(NULL, NULL);
	}
      if (mix_is_active(id)) 
	{
	  mix_dialog_id = id;
	  reflect_mix_change(id);
	}
    }
}


static gboolean id_check_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (id_changed) id_activated(w_id, NULL);
  return(false);
}


static gboolean id_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  id_changed = true;
  return(false);
}


static void beg_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  if (!(mix_is_active(mix_dialog_id))) return;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_beg));
  if (val)
    {
      chan_info *cp;
      mus_float_t beg;
      char *up_to_colon;
      up_to_colon = string_to_colon(val);
      cp = mix_chan_info_from_id(mix_dialog_id);
      redirect_errors_to(errors_to_mix_text, NULL);
      beg = string_to_mus_float_t(up_to_colon, 0.0, "begin time");
      redirect_errors_to(NULL, NULL);
      if (beg >= 0.0)
	{
	  mus_long_t pos, old_pos;
	  old_pos = mix_position_from_id(mix_dialog_id);
	  pos = (mus_long_t)(beg * snd_srate(cp->sound));
	  mix_set_position_edit(mix_dialog_id, pos);
	  syncd_mix_change_position(mix_dialog_id, pos - old_pos);
	}
      after_mix_edit(mix_dialog_id);
      free(up_to_colon);
    }
}


static void widget_mix_to_text(GtkWidget *w, int id)
{
  if (mix_name(id))
    gtk_entry_set_text(GTK_ENTRY(w), mix_name(id));
  else widget_int_to_text(w, id);
}


static gboolean copy_mix_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  copy_mix(mix_dialog_id);
  after_mix_edit(mix_dialog_id);
  return(false);
}


/* -------- mix play -------- */

static bool mix_playing = false;

void reflect_mix_play_stop(void)
{
  /* called in snd-dac.c */
  mix_playing = false;
}


#if WITH_AUDIO
static void mix_play_callback(GtkWidget *w, gpointer context) 
{
  if (mix_playing)
    mix_playing = false;
  else
    {
      if (!(mix_exists(mix_dialog_id))) return;
      syncd_mix_play(mix_dialog_id);
      mix_playing = true;
      play_mix_from_id(mix_dialog_id);
    }
}


static gboolean mix_play_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  draw_picture(mix_play_ax, snd_icon(SND_PNG_SPEAKER), 0, 0, 0, 0, 16, 16); /* in gtk2 this looks better if y-dest is 2 */
  return(false);
}
#endif


static void mix_dB_callback(GtkWidget *w, gpointer context) 
{
  spf->in_dB = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  mix_amp_env_resize(w_env);
}


static void mix_sync_callback(GtkWidget *w, gpointer context) 
{
  bool cb_set;
  cb_set = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  if ((cb_set) &&
      (mix_sync_from_id(mix_dialog_id) == 0))
    {
      mix_set_sync_from_id(mix_dialog_id, GET_ORIGINAL_SYNC);  /* choose a new sync val or return to previous */
      /* check for resync */
      syncd_mix_set_color(mix_dialog_id, ss->red);
    }
  else
    {
      if ((!(cb_set)) &&
	  (mix_sync_from_id(mix_dialog_id) != 0))
	{
	  syncd_mix_unset_color(mix_dialog_id); /* unset colors of any syncd mixes */
	  mix_set_sync_from_id(mix_dialog_id, 0);
	}
    }
  for_each_normal_chan(display_channel_data);
}


static void mix_clip_callback(GtkWidget *w, gpointer context) 
{
  spf->clipping = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  mix_amp_env_resize(w_env);
}


static void mix_wave_callback(GtkWidget *w, gpointer context) 
{
  with_mix_background_wave = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  mix_amp_env_resize(w_env);
}


static void apply_env_callback(GtkWidget *w, gpointer context)
{
  /* set all mix amp envs, last one should remix */
  if (!(mix_is_active(mix_dialog_id))) return;

  if ((dialog_env) && 
      (!(is_default_env(dialog_env))))
    {
      mix_set_amp_env_edit(mix_dialog_id, dialog_env);
      syncd_mix_set_amp_env(mix_dialog_id, dialog_env);  
    }
  else 
    {
      mix_set_amp_env_edit(mix_dialog_id, NULL);
      syncd_mix_set_amp_env(mix_dialog_id, NULL);  
    }
  after_mix_edit(mix_dialog_id);
  mix_amp_env_resize(w_env);
}


static void dismiss_mix_dialog(GtkWidget *w, gpointer context)
{
  clear_mix_error();
  gtk_widget_hide(mix_dialog);
}


static gint delete_mix_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  clear_mix_error();
  gtk_widget_hide(mix_dialog);
  return(true);
}


static GtkWidget *mix_next_button, *mix_previous_button, *mix_apply_button;

static void mix_next_callback(GtkWidget *w, gpointer context)
{
  int id;
  clear_mix_error();
  id = next_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      reflect_mix_change(id);
      if (next_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(mix_next_button, false);
    }
}


static void mix_previous_callback(GtkWidget *w, gpointer context)
{
  int id;
  clear_mix_error();
  id = previous_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      reflect_mix_change(id);
      if (previous_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(mix_previous_button, false);
    }
}


static void mix_dialog_help_callback(GtkWidget *w, gpointer context) 
{
  mix_dialog_help();
}


static GtkWidget *w_sync;
#define LEFT_MARGIN 6

GtkWidget *make_mix_dialog(void)
{
  if (!mix_dialog)
    {
      GtkWidget *dismiss_button, *help_button, *rc, *mix_frame, *rc_top, *copy_button;
      GtkWidget *lo_hbox, *w_dB_frame, *w_dB, *w_clip, *w_wave, *w_dB_row;
#if WITH_AUDIO
      GtkWidget *mix_play_pix;
#endif
      char amplab[LABEL_BUFFER_SIZE];

      gmix_speed_control_style = speed_control_style(ss);

      mix_dialog_id = any_mix_id();
      mix_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(mix_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      SG_SIGNAL_CONNECT(mix_dialog, "delete_event", delete_mix_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(mix_dialog), "Mixes");
      sg_make_resizable(mix_dialog);
      sg_container_set_border_width (GTK_CONTAINER(mix_dialog), 6);
      gtk_window_resize(GTK_WINDOW(mix_dialog), 560, 280);
      gtk_widget_realize(mix_dialog);
      
      help_button = gtk_dialog_add_button(GTK_DIALOG(mix_dialog), "Help", GTK_RESPONSE_NONE);
      gtk_widget_set_name(help_button, "dialog_button");
      SG_SIGNAL_CONNECT(help_button, "clicked", mix_dialog_help_callback, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(help_button);
#endif
      gtk_widget_show(help_button);

      copy_button = gtk_dialog_add_button(GTK_DIALOG(mix_dialog), "Copy mix", GTK_RESPONSE_NONE);
      gtk_widget_set_name(copy_button, "dialog_button");
      SG_SIGNAL_CONNECT(copy_button, "clicked", copy_mix_callback, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(copy_button);
#endif
      gtk_widget_show(copy_button);

      dismiss_button = gtk_dialog_add_button(GTK_DIALOG(mix_dialog), I_GO_AWAY, GTK_RESPONSE_NONE);
      gtk_widget_set_name(dismiss_button, "dialog_button");
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_mix_dialog, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(dismiss_button);
#endif
      gtk_widget_show(dismiss_button);

      mix_apply_button = gtk_dialog_add_button(GTK_DIALOG(mix_dialog), "Apply env", GTK_RESPONSE_NONE);
      gtk_widget_set_name(mix_apply_button, "dialog_button");
      SG_SIGNAL_CONNECT(mix_apply_button, "clicked", apply_env_callback, NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(mix_apply_button);
#endif
      gtk_widget_show(mix_apply_button);

      /* normally hidden error indication at top */
      error_frame = gtk_frame_new(NULL);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(mix_dialog)), error_frame, false, false, 4);

      error_label = gtk_label_new(NULL);
      gtk_container_add(GTK_CONTAINER(error_frame), error_label);
      gtk_widget_show(error_label);


      /* top row of mix id name position */

      rc_top = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(mix_dialog)), rc_top, false, false, 4);
      gtk_widget_show(rc_top);

      mix_frame = gtk_frame_new(NULL);
      sg_box_pack_start(GTK_BOX(rc_top), mix_frame, false, false, 4);
      gtk_widget_show(mix_frame);

      rc = gtk_hbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(mix_frame), rc);
      gtk_widget_show(rc);

      w_id_label = gtk_label_new("mix:");
      sg_box_pack_start(GTK_BOX(rc), w_id_label, false, false, 4);
      gtk_widget_show(w_id_label);

      w_id = snd_entry_new(rc, NULL, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_id, "activate", id_activated, NULL);
      SG_SIGNAL_CONNECT(w_id, "leave_notify_event", id_check_callback, NULL);
      SG_SIGNAL_CONNECT(w_id, "key_press_event", id_modify_callback, NULL);

      w_beg = snd_entry_new(rc, NULL, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_beg, "activate", beg_activated, NULL);

#if WITH_AUDIO
      mix_play = gtk_button_new();
      sg_box_pack_start(GTK_BOX(rc), mix_play, false, false, 2);
      SG_SIGNAL_CONNECT(mix_play, "clicked", mix_play_callback, NULL);
      gtk_widget_show(mix_play);
      widget_modify_bg(mix_play, GTK_STATE_ACTIVE, ss->basic_color);
      widget_modify_bg(mix_play, GTK_STATE_SELECTED, ss->basic_color);
      
      mix_play_pix = gtk_drawing_area_new();
      sg_widget_set_events(mix_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(mix_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(mix_play), mix_play_pix);
      gtk_widget_show(mix_play_pix);
      SG_SIGNAL_CONNECT(mix_play_pix, DRAW_SIGNAL, mix_play_pix_expose, NULL);
#endif

      mix_next_button = button_new_with_icon(ICON_GO_FORWARD);
      gtk_widget_set_name(mix_next_button, "dialog_button");
      sg_box_pack_end(GTK_BOX(rc), mix_next_button, false, true, 6);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(mix_next_button);
#endif
      SG_SIGNAL_CONNECT(mix_next_button, "clicked", mix_next_callback, NULL);
      gtk_widget_show(mix_next_button);
      set_stock_button_label(mix_next_button, I_NEXT);

      mix_previous_button = button_new_with_icon(ICON_GO_BACK);
      gtk_widget_set_name(mix_previous_button, "dialog_button");
      sg_box_pack_end(GTK_BOX(rc), mix_previous_button, false, true, 6);
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(mix_previous_button);
#endif
      SG_SIGNAL_CONNECT(mix_previous_button, "clicked", mix_previous_callback, NULL);
      gtk_widget_show(mix_previous_button);
      set_stock_button_label(mix_previous_button, I_PREVIOUS);


      /* SPEED */
      w_speed_form = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(mix_dialog)), w_speed_form, false, false, 4);
      
#if (!GTK_CHECK_VERSION(3, 92, 1))
      w_speed_event = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(w_speed_form), w_speed_event, false, false, 4);
      gtk_widget_show(w_speed_event);
      SG_SIGNAL_CONNECT(w_speed_event, "button_press_event", mix_speed_click_callback, NULL);
#endif      
#if (!GTK_CHECK_VERSION(3, 0, 0))
      w_speed_label = gtk_label_new("speed:");
#else
      w_speed_label = gtk_button_new_with_label("speed:");
      add_highlight_button_style(w_speed_label);
#endif
#if (!GTK_CHECK_VERSION(3, 92, 1))
      gtk_container_add(GTK_CONTAINER(w_speed_event), w_speed_label);
#else
      sg_box_pack_start(GTK_BOX(w_speed_form), w_speed_label, false, false, 4);
      SG_SIGNAL_CONNECT(w_speed_label, "button_press_event", mix_speed_click_callback, NULL);
#endif
      gtk_widget_show(w_speed_label);

#if (!GTK_CHECK_VERSION(3, 92, 1))
      w_speed_label_event = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(w_speed_form), w_speed_label_event, false, false, 4);
      gtk_widget_show(w_speed_label_event);
      SG_SIGNAL_CONNECT(w_speed_label_event, "button_press_event", speed_label_click_callback, NULL);
#endif
      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO:    w_speed_number = gtk_label_new("1/1");  break;
	case SPEED_CONTROL_AS_SEMITONE: w_speed_number = gtk_label_new("1");    break;
	default:                        w_speed_number = gtk_label_new("1.00"); break;
	}
#if (!GTK_CHECK_VERSION(3, 92, 1))
      gtk_container_add(GTK_CONTAINER(w_speed_label_event), w_speed_number);
#else
      sg_box_pack_start(GTK_BOX(w_speed_form), w_speed_number, false, false, 4);
      SG_SIGNAL_CONNECT(w_speed_number, "button_press_event", speed_label_click_callback, NULL);
#endif
      gtk_widget_show(w_speed_number);
      
      w_speed_adj = (GtkAdjustment *)gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      w_speed = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_speed_adj));
      sg_box_pack_start(GTK_BOX(w_speed_form), w_speed, true, true, 4);
      SG_SIGNAL_CONNECT(w_speed, "button_release_event", mix_speed_release_callback, NULL);
      SG_SIGNAL_CONNECT(w_speed, "motion_notify_event", speed_motion_callback, NULL);
      SG_SIGNAL_CONNECT(w_speed, "button_press_event", speed_press_callback, NULL);
      gtk_widget_show(w_speed);
      gtk_widget_show(w_speed_form);


      /* AMP */
      spf = new_env_editor();

      w_amp_form = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(mix_dialog)), w_amp_form, false, false, 0);

#if (!GTK_CHECK_VERSION(3, 92, 1))      
      w_amp_event = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(w_amp_form), w_amp_event, false, false, 4);
      gtk_widget_show(w_amp_event);
      SG_SIGNAL_CONNECT(w_amp_event, "button_press_event", mix_amp_click_callback, NULL);
#endif      
      snprintf(amplab, LABEL_BUFFER_SIZE, "%s", "amp:");
#if (!GTK_CHECK_VERSION(3, 0, 0))
      w_amp_label = gtk_label_new(amplab);
#else
      w_amp_label = gtk_button_new_with_label("amp:");
      add_highlight_button_style(w_amp_label);
#endif
#if (!GTK_CHECK_VERSION(3, 92, 1))      
      gtk_container_add(GTK_CONTAINER(w_amp_event), w_amp_label);
#else
      sg_box_pack_start(GTK_BOX(w_amp_form), w_amp_label, false, false, 4);
      SG_SIGNAL_CONNECT(w_amp_label, "button_press_event", mix_amp_click_callback, NULL);
#endif
      gtk_widget_show(w_amp_label);
      
      w_amp_number = gtk_label_new("1.00");
      sg_box_pack_start(GTK_BOX(w_amp_form), w_amp_number, false, false, 0);
      gtk_widget_show(w_amp_number);
	  
      w_amp_adj = (GtkAdjustment *)gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
      w_amp = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_amp_adj));
      sg_box_pack_start(GTK_BOX(w_amp_form), w_amp, true, true, 4);

      SG_SIGNAL_CONNECT(w_amp, "motion_notify_event", amp_motion_callback, NULL);
      SG_SIGNAL_CONNECT(w_amp, "button_release_event", mix_amp_release_callback, NULL);
      SG_SIGNAL_CONNECT(w_amp, "button_press_event", amp_press_callback, NULL);
      gtk_widget_show(w_amp);
      
      gtk_widget_show(w_amp_form);

      lo_hbox = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(mix_dialog)), lo_hbox, true, true, 5);
      gtk_widget_show(lo_hbox);

      /* GRAPH (frame) */
      w_env_frame = gtk_frame_new(NULL);
      sg_box_pack_start(GTK_BOX(lo_hbox), w_env_frame, true, true, 10);

      /* GRAPH (buttons) */
      w_dB_frame = gtk_frame_new(NULL);
      sg_box_pack_end(GTK_BOX(lo_hbox), w_dB_frame, false, false, 2);
      gtk_widget_show(w_dB_frame);

      w_dB_row = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(w_dB_frame), w_dB_row);
      gtk_widget_show(w_dB_row);	

      w_clip = gtk_check_button_new_with_label("clip");
      widget_set_margin_left(w_clip, LEFT_MARGIN);
      SG_SIGNAL_CONNECT(w_clip, "toggled", mix_clip_callback, NULL);
      sg_box_pack_start(GTK_BOX(w_dB_row), w_clip, false, false, 0);
      gtk_widget_show(w_clip);

      w_wave = gtk_check_button_new_with_label("wave");
      widget_set_margin_left(w_wave, LEFT_MARGIN);
      SG_SIGNAL_CONNECT(w_wave, "toggled", mix_wave_callback, NULL);
      sg_box_pack_start(GTK_BOX(w_dB_row), w_wave, false, false, 0);
      gtk_widget_show(w_wave);

      w_dB = gtk_check_button_new_with_label("dB");
      widget_set_margin_left(w_dB, LEFT_MARGIN);
      SG_SIGNAL_CONNECT(w_dB, "toggled", mix_dB_callback, NULL);
      sg_box_pack_start(GTK_BOX(w_dB_row), w_dB, false, false, 0);
      gtk_widget_show(w_dB);

      w_sync = gtk_check_button_new_with_label("sync");
      widget_set_margin_left(w_sync, LEFT_MARGIN);
      SG_SIGNAL_CONNECT(w_sync, "toggled", mix_sync_callback, NULL);
      sg_box_pack_start(GTK_BOX(w_dB_row), w_sync, false, false, 0);
      gtk_widget_show(w_sync);

      /* GRAPH (drawing area) */
      w_env = gtk_drawing_area_new();
      sg_widget_set_events(w_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_env_frame), w_env);
      widget_modify_bg(w_env, GTK_STATE_NORMAL, ss->highlight_color);
      gtk_widget_show(w_env);
      SG_SIGNAL_CONNECT(w_env, DRAW_SIGNAL, mix_amp_env_expose_callback, NULL);
      SG_SIGNAL_CONNECT(w_env, "configure_event", mix_amp_env_resize_callback, NULL);
      SG_SIGNAL_CONNECT(w_env, "button_press_event", mix_drawer_button_press, NULL);
      SG_SIGNAL_CONNECT(w_env, "button_release_event", mix_drawer_button_release, NULL);
      SG_SIGNAL_CONNECT(w_env, "motion_notify_event", mix_drawer_button_motion, NULL);
      gtk_widget_show(w_env_frame);

      gtk_widget_show(mix_dialog);
      set_dialog_widget(MIX_DIALOG, mix_dialog);

      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w_clip), true);
      if (mix_sync_from_id(mix_dialog_id) != 0)
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w_sync), true);

#if WITH_AUDIO
      mix_play_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      mix_play_ax->wn = WIDGET_TO_WINDOW(mix_play_pix);
      mix_play_ax->gc = ss->basic_gc;
#endif

      gtk_widget_hide(error_frame);
    }
  else 
    {
      raise_dialog(mix_dialog);
    }
  reflect_mix_change(mix_dialog_id);  
  return(mix_dialog);
}


void reflect_mix_change(int mix_id)
{
  if ((mix_dialog) && 
      (widget_is_active(mix_dialog)))
    {
      if (mix_id != ANY_MIX_ID)
	mix_dialog_id = mix_id;
      if (!(mix_exists(mix_dialog_id))) 
	{
	  mix_dialog_id = any_mix_id(); 
	  mix_id = mix_dialog_id;
	}

      if ((mix_id == mix_dialog_id) || (mix_id == ANY_MIX_ID))
	{
	  mus_float_t val;
	  set_sensitive(mix_next_button, (next_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	  set_sensitive(mix_previous_button, (previous_mix_id(mix_dialog_id) != INVALID_MIX_ID));

	  /* now reflect current mix state in mix dialog controls */
	  if (mix_exists(mix_dialog_id))
	    {
	      char lab[LABEL_BUFFER_SIZE];
	      chan_info *cp;
	      mus_long_t beg, len;

	      cp = mix_chan_info_from_id(mix_dialog_id);
	      if (old_mix_dialog_id != INVALID_MIX_ID)
		{
		  mix_unset_color_from_id(old_mix_dialog_id);
		  syncd_mix_unset_color(old_mix_dialog_id);
		}
	      old_mix_dialog_id = mix_dialog_id;
	      mix_set_color_from_id(mix_dialog_id, ss->red);
	      syncd_mix_set_color(mix_dialog_id, ss->red);

	      for_each_normal_chan(display_channel_data);

	      if (!dragging)
		{
		  val = mix_speed_from_id(mix_dialog_id);
		  reflect_mix_speed(val);
		}

	      beg = mix_position_from_id(mix_dialog_id);
	      len = mix_length_from_id(mix_dialog_id);
	      snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f%s",
			   ((double)beg / (double)snd_srate(cp->sound)),
			   ((double)(beg + len) / (double)snd_srate(cp->sound)),
			   (mix_is_active(mix_dialog_id)) ? "" : " (locked)");
	      gtk_entry_set_text(GTK_ENTRY(w_beg), lab);

	      widget_mix_to_text(w_id, mix_dialog_id);

	      set_sensitive(mix_apply_button, true);
	    }
	  else
	    {
	      gtk_entry_set_text(GTK_ENTRY(w_id), "-1");
	      gtk_entry_set_text(GTK_ENTRY(w_beg), "no active mixes");
	      set_sensitive(mix_apply_button, false);
	    }
	  if (!dragging)
	    {
	      if (mix_is_active(mix_dialog_id))
		val = mix_amp_from_id(mix_dialog_id);
	      else val = 1.0;
	      reflect_mix_amp(val);
	    }
	  if (mix_amp_env_from_id(mix_dialog_id))
	    {
	      if (dialog_env) free_env(dialog_env);
	      dialog_env = copy_env(mix_amp_env_from_id(mix_dialog_id));
	    }
	  /* copy here else we're editing it directly afterwards (and we free old in mix_set_amp_env_edit) */
	  if (!dialog_env) 
	    dialog_env = default_env(1.0, 1.0);
	  mix_amp_env_resize(w_env);

	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w_sync), (mix_sync_from_id(mix_dialog_id) != 0));
	}
    }
}


int mix_dialog_mix(void) 
{
  return(mix_dialog_id);
}


void mix_dialog_set_mix(int id) 
{
  mix_dialog_id = id; 
  reflect_mix_change(mix_dialog_id);
}
