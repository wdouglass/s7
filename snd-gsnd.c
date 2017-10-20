#include "snd.h"

/* gtk's paned window is so buggy that perhaps I should simply replace all of them
 *   with boxes.  Especially the control panel.
 */

#define sound_env_editor(Sp) ((env_editor *)(sp->flt))

enum {W_pane, W_pane_box, W_control_panel,
      W_name_form, W_name, W_name_event, W_name_pix, W_stop_pix, W_info,
      W_play, W_sync, W_unite, W_close,
      W_amp_form, W_amp_event, W_amp, W_amp_label, W_amp_number, W_amp_sep,
      W_speed_form, W_speed, W_speed_event, W_speed_label, W_speed_label_event, W_speed_number, W_speed_pix,
      W_expand_form, W_expand, W_expand_event, W_expand_label, W_expand_number, W_expand_button,
      W_contrast_form, W_contrast, W_contrast_event, W_contrast_label, W_contrast_number, W_contrast_button,
      W_reverb_form, W_revscl, W_revscl_event, W_revscl_label, W_revscl_number,
      W_revlen, W_revlen_event, W_revlen_label, W_revlen_number, W_reverb_button,
      W_filter_form, W_filter_label, W_filter_order, W_filter_env, W_filter, W_filter_button, 
      W_filter_dB, W_filter_hz, W_filter_frame,
      NUM_SND_WIDGETS
};

enum {W_amp_adj, W_speed_adj, W_contrast_adj, W_expand_adj, W_revscl_adj, 
      W_revlen_adj, W_filter_adj,
      NUM_SND_ADJS
};


GtkWidget *unite_button(snd_info *sp)   {return(sp->snd_widgets[W_unite]);}

GtkWidget *w_snd_pane(snd_info *sp)     {return(sp->snd_widgets[W_pane]);}
GtkWidget *w_snd_pane_box(snd_info *sp) {return(sp->snd_widgets[W_pane_box]);}

#define SND_PANE(Sp)             Sp->snd_widgets[W_pane]
#define PANE_BOX(Sp)             Sp->snd_widgets[W_pane_box]

#define NAME_HBOX(Sp)            Sp->snd_widgets[W_name_form]
#define NAME_BUTTON(Sp)          Sp->snd_widgets[W_name]
#define NAME_EVENT_BOX(Sp)       Sp->snd_widgets[W_name_event]
#define NAME_SEPARATOR(Sp)       Sp->snd_widgets[W_amp_sep]

#define CLOSE_BUTTON(Sp)         Sp->snd_widgets[W_close]

#define STATUS_AREA(Sp)          Sp->snd_widgets[W_info]
#define NAME_PIX(Sp)             Sp->snd_widgets[W_name_pix]
#define STOP_PIX(Sp)             Sp->snd_widgets[W_stop_pix]
#define SYNC_BUTTON(Sp)          Sp->snd_widgets[W_sync]
#define PLAY_BUTTON(Sp)          Sp->snd_widgets[W_play]
#define UNITE_BUTTON(Sp)         Sp->snd_widgets[W_unite]
#define CLOCK_PIX(Sp, Chan)      Sp->clock_widgets[Chan]

#define CONTROL_PANEL(Sp)        Sp->snd_widgets[W_control_panel]

#define AMP_HBOX(Sp)             Sp->snd_widgets[W_amp_form]
#define AMP_LABEL(Sp)            Sp->snd_widgets[W_amp_number]
#define AMP_BUTTON(Sp)           Sp->snd_widgets[W_amp_label]
#define AMP_EVENT_BOX(Sp)        Sp->snd_widgets[W_amp_event]
#define AMP_SCROLLBAR(Sp)        Sp->snd_widgets[W_amp]

#define SPEED_HBOX(Sp)           Sp->snd_widgets[W_speed_form]
#define SPEED_ARROW(Sp)          Sp->snd_widgets[W_speed_pix]
#define SPEED_LABEL(Sp)          Sp->snd_widgets[W_speed_number]
#define SPEED_EVENT_BOX(Sp)      Sp->snd_widgets[W_speed_event]
#define SPEED_LABEL_EVENT_BOX(Sp) Sp->snd_widgets[W_speed_label_event]
#define SPEED_BUTTON(Sp)         Sp->snd_widgets[W_speed_label]
#define SPEED_SCROLLBAR(Sp)      Sp->snd_widgets[W_speed]

#define EXPAND_HBOX(Sp)          Sp->snd_widgets[W_expand_form]
#define EXPAND_LEFT_BUTTON(Sp)   Sp->snd_widgets[W_expand_label]
#define EXPAND_EVENT_BOX(Sp)     Sp->snd_widgets[W_expand_event]
#define EXPAND_SCROLLBAR(Sp)     Sp->snd_widgets[W_expand]
#define EXPAND_LABEL(Sp)         Sp->snd_widgets[W_expand_number]
#define EXPAND_RIGHT_BUTTON(Sp)  Sp->snd_widgets[W_expand_button]

#define CONTRAST_HBOX(Sp)        Sp->snd_widgets[W_contrast_form]
#define CONTRAST_LEFT_BUTTON(Sp) Sp->snd_widgets[W_contrast_label]
#define CONTRAST_EVENT_BOX(Sp)   Sp->snd_widgets[W_contrast_event]
#define CONTRAST_SCROLLBAR(Sp)   Sp->snd_widgets[W_contrast]
#define CONTRAST_LABEL(Sp)       Sp->snd_widgets[W_contrast_number]
#define CONTRAST_RIGHT_BUTTON(Sp) Sp->snd_widgets[W_contrast_button]

#define REVSCL_EVENT_BOX(Sp)     Sp->snd_widgets[W_revscl_event]
#define REVSCL_SCROLLBAR(Sp)     Sp->snd_widgets[W_revscl]
#define REVSCL_BUTTON(Sp)        Sp->snd_widgets[W_revscl_label]
#define REVSCL_LABEL(Sp)         Sp->snd_widgets[W_revscl_number]

#define REVLEN_EVENT_BOX(Sp)     Sp->snd_widgets[W_revlen_event]
#define REVLEN_BUTTON(Sp)        Sp->snd_widgets[W_revlen_label]
#define REVLEN_SCROLLBAR(Sp)     Sp->snd_widgets[W_revlen]
#define REVLEN_LABEL(Sp)         Sp->snd_widgets[W_revlen_number]

#define REVERB_RIGHT_BUTTON(Sp)  Sp->snd_widgets[W_reverb_button]
#define REVERB_HBOX(Sp)          Sp->snd_widgets[W_reverb_form]

#define FILTER_HBOX(Sp)          Sp->snd_widgets[W_filter_form]
#define FILTER_LEFT_BUTTON(Sp)   Sp->snd_widgets[W_filter_label]
#define FILTER_ORDER_TEXT(Sp)    Sp->snd_widgets[W_filter_order]
#define FILTER_COEFFS_TEXT(Sp)   Sp->snd_widgets[W_filter]
#define FILTER_RIGHT_BUTTON(Sp)  Sp->snd_widgets[W_filter_button]
#define FILTER_DB_BUTTON(Sp)     Sp->snd_widgets[W_filter_dB]
#define FILTER_HZ_BUTTON(Sp)     Sp->snd_widgets[W_filter_hz]
#define FILTER_FRAME(Sp)         Sp->snd_widgets[W_filter_frame]
#define FILTER_ENV(Sp)           Sp->snd_widgets[W_filter_env]

#define AMP_ADJUSTMENT(Sp)       Sp->snd_adjs[W_amp_adj]
#define SPEED_ADJUSTMENT(Sp)     Sp->snd_adjs[W_speed_adj]
#define EXPAND_ADJUSTMENT(Sp)    Sp->snd_adjs[W_expand_adj]
#define CONTRAST_ADJUSTMENT(Sp)  Sp->snd_adjs[W_contrast_adj]
#define REVSCL_ADJUSTMENT(Sp)    Sp->snd_adjs[W_revscl_adj]
#define REVLEN_ADJUSTMENT(Sp)    Sp->snd_adjs[W_revlen_adj]
#define FILTER_ADJUSTMENT(Sp)    Sp->snd_adjs[W_filter_adj]



static bool mini_lock_allocated = false;
static picture_t *mini_lock = NULL, *speed_r = NULL, *speed_l = NULL, *blank = NULL, *stop_sign = NULL;
static picture_t *bomb = NULL;

void show_lock(snd_info *sp)
{
  if (mini_lock)
    {
      draw_picture(sp->name_pix_ax, mini_lock, 0, 0, 0, 0, 16, 16);
      gtk_widget_show(NAME_PIX(sp));
    }
}


void hide_lock(snd_info *sp)
{
  if (mini_lock)
    gtk_widget_hide(NAME_PIX(sp));
}


static void show_stop_sign(snd_info *sp)
{
  if (!has_widgets(sp)) return;
  if (stop_sign)
    draw_picture(sp->stop_pix_ax, stop_sign, 0, 0, 0, 0, 16, 16);
  gtk_widget_show(STOP_PIX(sp));
}


static void hide_stop_sign(snd_info *sp)
{
  if (!has_widgets(sp)) return;
  gtk_widget_hide(STOP_PIX(sp));
}


static void show_bomb(snd_info *sp)
{
  if (!has_widgets(sp)) return;
  draw_picture(sp->name_pix_ax, bomb, 0, 0, 0, 0, 16, 16);
  gtk_widget_show(NAME_PIX(sp));
}


static void hide_bomb(snd_info *sp)
{
  if (!has_widgets(sp)) return;
  gtk_widget_hide(NAME_PIX(sp));
}


#define BOMB_TIME 200

static gint tick_bomb(gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (!has_widgets(sp)) return(0);
  if (sp->need_update || sp->file_unreadable)
    {
      show_bomb(sp);
      g_timeout_add_full(0, (guint32)BOMB_TIME, tick_bomb, data, NULL);
    }
  else 
    {
      hide_bomb(sp);
      sp->bomb_in_progress = false;
    }
  return(0);
}


void start_bomb(snd_info *sp)
{
  if (!has_widgets(sp)) return;
  if (!(sp->bomb_in_progress))
    {
      sp->bomb_in_progress = true;
      g_timeout_add_full(0, (guint32)BOMB_TIME, tick_bomb, (gpointer)sp, NULL);
    }
}


void stop_bomb(snd_info *sp)
{
  if (!has_widgets(sp)) return;
  hide_bomb(sp);
  sp->bomb_in_progress = false;
}



static Drawable *sound_pix_wn(chan_info *cp)
{
  snd_info *sp = NULL;
  if (cp) sp = cp->sound;
  if (!has_widgets(sp)) return(NULL);

  if ((cp->chan < sp->num_clock_widgets) &&
      (CLOCK_PIX(sp, cp->chan)) &&
      (sp->clock_pix_ax[cp->chan]))
    return(DRAWABLE(WIDGET_TO_WINDOW(CLOCK_PIX(sp, cp->chan))));
  return(DRAWABLE(WIDGET_TO_WINDOW(CLOCK_PIX(sp, 0))));
}


static void show_happy_face(Drawable *wn, mus_float_t pct)
{
  cairo_t *cr;
  cr = make_cairo(wn);

  /* overall background */
  cairo_translate(cr, 0, 0);

  cairo_push_group(cr);  

  cairo_set_source_rgba(cr, ss->basic_color->red, ss->basic_color->green, ss->basic_color->blue, ss->basic_color->alpha); 
  cairo_rectangle(cr, 0, 0, 16, 16);
  cairo_fill(cr);
  
  /* round face */
  cairo_set_source_rgb(cr, 1.0, pct, 0.0);
  cairo_arc(cr, 8, 8, 8, 0.0, 2 * M_PI);
  cairo_fill(cr);
  
  /* eyes */
  cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
  cairo_arc(cr, 5, 6, 1.5, 0, 2 * M_PI);
  cairo_fill(cr);

  cairo_arc(cr, 11, 6, 1.5, 0, 2 * M_PI);
  cairo_fill(cr);

  /* mouth */
  cairo_set_line_width(cr, 1.0);

  if (pct < 0.2)
    cairo_arc(cr, 8, 14, 4, 17.0/16.0 * M_PI, -1.0/16.0 * M_PI);
  else
    if (pct < 0.35)
      cairo_arc(cr, 8, 14, 4, 18.3/16.0 * M_PI, -2.3/16.0 * M_PI);
    else
      if (pct < 0.5)
	cairo_arc(cr, 8, 14, 4, 19.0/16.0 * M_PI, -3.0/16.0 * M_PI);
      else
	if (pct < 0.6)
	  {
	    cairo_move_to(cr, 4, 12);
	    cairo_rel_line_to(cr, 8, 0);
	  }
	else 
	  if (pct < 0.75)
	    cairo_arc(cr, 8, 7, 5, 4.00/16.0 * M_PI, 12.0/16.0 * M_PI);
	  else
	    if (pct < 0.9)
	      cairo_arc(cr, 8, 7, 5, 3.0/16.0 * M_PI, 13.0/16.0 * M_PI);
	    else cairo_arc(cr, 8, 8, 5, 1.0/16.0 * M_PI, 15.0/16.0 * M_PI);
  cairo_stroke(cr);
  
  cairo_pop_group_to_source(cr);
  cairo_paint(cr);
  free_cairo(cr);
}


static void hide_happy_face(Drawable *wn)
{
  cairo_t *cr;
  cr = make_cairo(wn);
  cairo_set_source_rgba(cr, ss->basic_color->red, ss->basic_color->green, ss->basic_color->blue, ss->basic_color->alpha); 
  cairo_rectangle(cr, 0, 0, 24, 24);
  cairo_fill(cr);
  free_cairo(cr);
}


static void make_pixmaps(void)
{
  if (!mini_lock_allocated)
    { 
      mini_lock = snd_icon(SND_PNG_LOCK);
      stop_sign = snd_icon(SND_PNG_STOP);
      blank = snd_icon(SND_PNG_BLANK);
      speed_r = snd_icon(SND_PNG_RIGHT_ARROW);
      speed_l = snd_icon(SND_PNG_LEFT_ARROW);
      bomb = snd_icon(SND_PNG_BOMB);
      mini_lock_allocated = true;
    }
}

/* lock stop blank bomb as 16x16
 *    bomb (warning) is also 22x22 24x24 32x32
 *    lock (changes-prevent) and stop (stop) also
 *  but 24x24 doesn't look larger?
 */


static gboolean name_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if ((has_widgets(sp)) &&
      (NAME_PIX(sp)) &&
      (sp->name_pix_ax))
    {
      if ((sp->user_read_only == FILE_READ_ONLY) || 
	  (sp->file_read_only == FILE_READ_ONLY))
	draw_picture(sp->name_pix_ax, mini_lock, 0, 0, 0, 0, 16, 16);
      else draw_picture(sp->name_pix_ax, blank, 0, 0, 0, 0, 16, 16);
    }
  return(false);
}


static gboolean clock_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  snd_info *sp = NULL;
  if (cp) sp = cp->sound;

  if ((has_widgets(sp)) &&
      (cp->chan < sp->num_clock_widgets) &&
      (CLOCK_PIX(sp, cp->chan)) &&
      (sp->clock_pix_ax[cp->chan]))
    {
      if (cp->progress_pct >= 0.0)
	show_happy_face(sound_pix_wn(cp), cp->progress_pct);
      else draw_picture(sp->clock_pix_ax[cp->chan], blank, 0, 0, 0, 0, 16, 16);
    }
  return(false);
}



/* -------- STATUS AREA CALLBACKS -------- */

static char stupid[1] = {'\0'};

void set_status(snd_info *sp, const char *str, bool update) 
{
  if ((sp->inuse != SOUND_NORMAL) || (!has_widgets(sp))) return;
  if (str)
    set_label(STATUS_AREA(sp), str);
  else set_label(STATUS_AREA(sp), stupid);
}



/* -------- PLAY BUTTON -------- */

static void set_button_base(GtkWidget *w, color_info *col)
{
  widget_modify_base(w, GTK_STATE_NORMAL, col);
  widget_modify_base(w, GTK_STATE_PRELIGHT, col);
}


void set_play_button(snd_info *sp, bool val)
{
#if WITH_AUDIO
  if (has_widgets(sp))
    set_toggle_button(PLAY_BUTTON(sp), val, false, (void *)sp);
#endif
}


void set_control_panel_play_button(snd_info *sp)
{
#if WITH_AUDIO
  if (has_widgets(sp))
    {
      set_toggle_button(PLAY_BUTTON(sp), false, false, sp);
      set_button_base(PLAY_BUTTON(sp), ss->white);
    }
#endif
}


#if WITH_AUDIO
static int last_play_state = 0;
/* these "last-*-state" variables are trying to catch C-M-click info which is then used
 *   presumably by the immediately following value-changed callback for the given button.
 */

static gboolean play_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_play_state = EVENT_STATE(ev);
  return(false);
}


static void play_button_click_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  chan_info *cp;
  bool on;

  on = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  if (sp->playing) 
    stop_playing_sound_no_toggle(sp, PLAY_BUTTON_UNSET);

  ss->tracking = ((with_tracking_cursor(ss) != DONT_TRACK) ||
		  ((on) && (last_play_state & (snd_ControlMask | snd_MetaMask))));

  cp = any_selected_channel(sp);
  goto_graph(cp);
  if (on) 
    {
      if (ss->tracking) 
	set_button_base(w, ss->green);
      else set_button_base(w, ss->white);
      play_sound(sp, 0, NO_END_SPECIFIED);
    }
  else set_button_base(w, ss->white);
}


typedef struct {bool pausing; } pause_data;

static void set_play_button_pause(snd_info *sp, void *ptr)
{
  if (sp->playing)
    {
      pause_data *pd = (pause_data *)ptr;
      GtkWidget *w;
      w = PLAY_BUTTON(sp);
      if (pd->pausing)
	set_button_base(w, ss->red);
      else 
	if (ss->tracking)
	  set_button_base(w, ss->green); 
	else set_button_base(w, ss->white);
    }
}
#endif


void play_button_pause(bool pausing)
{
#if WITH_AUDIO
  pause_data *pd;
  pd = (pause_data *)calloc(1, sizeof(pause_data));
  pd->pausing = pausing;
  for_each_sound_with_void(set_play_button_pause, (void *)pd);
  free(pd);
#endif
}


static void set_sync_color(snd_info *sp)
{
  GtkWidget *syb;
  syb = SYNC_BUTTON(sp);
  switch (sp->sync)
    {
    case 0:  set_button_base(syb, ss->white);               break;
    case 1:  set_button_base(syb, ss->selection_color);     break;
    case 2:  set_button_base(syb, ss->green);               break;
    case 3:  set_button_base(syb, ss->yellow);              break;
    case 4:  set_button_base(syb, ss->red);                 break;
    default: set_button_base(syb, ss->black);               break;
    }
}


void syncb(snd_info *sp, int on)
{
  sp->sync = on;
  if (on > ss->sound_sync_max) ss->sound_sync_max = on;
  if (has_widgets(sp))
    {
      set_sync_color(sp);
      set_toggle_button(SYNC_BUTTON(sp), (on != 0), false, (void *)sp);
    }
}


static int last_sync_state = 0;

static gboolean sync_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_sync_state = EVENT_STATE(ev);
  return(false);
}


static void sync_button_click(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  bool on;

  on = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  if (on)
    if (last_sync_state & snd_ControlMask) 
      if (last_sync_state & snd_MetaMask)
	if (last_sync_state & snd_ShiftMask)
	  sp->sync = 4;
	else sp->sync = 3;
      else sp->sync = 2;
    else sp->sync = 1;
  else sp->sync = 0;

  set_sync_color(sp);

  if (sp->sync != 0) 
    {
      chan_info *cp;
      if (sp->sync > ss->sound_sync_max) ss->sound_sync_max = sp->sync;
      cp = sp->lacp;
      if (!cp) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) sync_cursors(cp, cursor_sample(cp));
      apply_x_axis_change(cp);
    }
}


static int last_combine_state = 0;

static gboolean unite_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* click if set unsets, click if unset->combine, ctrl-click->superimpose */
  last_combine_state = EVENT_STATE(ev);
  return(false);
}


static void unite_button_click(GtkWidget *w, gpointer data)
{
  channel_style_t val;
  bool on;
  snd_info *sp = (snd_info *)data;

  on = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  if (on)
    {
      if (last_combine_state & (snd_ControlMask | snd_MetaMask)) 
	val = CHANNELS_SUPERIMPOSED;
      else val = CHANNELS_COMBINED;
    }
  else val = CHANNELS_SEPARATE;

  set_sound_channel_style(sp, val);
}


static gboolean name_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  char *str;
  snd_info *sp = (snd_info *)data;
  str = sp_name_click(sp);
  if (str)
    {
      status_report(sp, "%s", str);
      free(str);
    }
  return(false);
}


static gboolean name_button_tooltip(GtkWidget *w, gint x, gint y, gboolean keyboard_tip, GtkTooltip *tooltip, gpointer data)
{
  char *str;
  snd_info *sp = (snd_info *)data;
  str = sp_name_click(sp);
  if (str)
    {
      gtk_tooltip_set_text(tooltip, str);
      free(str);
    }
  else gtk_tooltip_set_text(tooltip, "nothing known about this sound!");
  return(true);
}



/* -------- AMP CALLBACKS -------- */

mus_float_t amp_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  if (val >= 1.0)
    return(0.9 * 0.5 * (1.0 + (val - 1.0) / (maxval - 1.0)));
  return(0.9 * 0.5 * ((val - minval) / (1.0 - minval)));
}


static mus_float_t scroll_to_amp(snd_info *sp, mus_float_t val)
{
  char amp_number_buffer[6];
  if (val <= 0.0) 
    sp->amp_control = sp->amp_control_min;
  else
    {
      if (val >= 0.9)
	sp->amp_control = sp->amp_control_max;
      else
	{
	  if (val > (0.5 * 0.9))
	    sp->amp_control = (((val / (0.5 * 0.9)) - 1.0) * (sp->amp_control_max - 1.0)) + 1.0;
	  else sp->amp_control = (val * (1.0 - sp->amp_control_min) / (0.5 * 0.9)) + sp->amp_control_min;
	}
    }
  snprintf(amp_number_buffer, 6, "%.3f", sp->amp_control);
  gtk_label_set_text(GTK_LABEL(AMP_LABEL(sp)), amp_number_buffer);
  return(val);
}


void set_amp(snd_info *sp, mus_float_t amp) 
{
  if (!has_widgets(sp))
    sp->amp_control = amp;
  else 
    {
      mus_float_t scrollval;
      GtkAdjustment *adj;
      scroll_to_amp(sp, scrollval = amp_to_scroll(sp->amp_control_min, amp, sp->amp_control_max));
      adj = AMP_ADJUSTMENT(sp);
      ADJUSTMENT_SET_VALUE(adj, scrollval);
      /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj)); */
    }
}


static gboolean amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if (EVENT_STATE(ev) & (snd_ControlMask | snd_MetaMask)) 
    set_amp(sp, sp->last_amp_control);
  else set_amp(sp, 1.0);
  return(false);
}


static void amp_changed_callback(GtkAdjustment *adj, gpointer data)
{
  scroll_to_amp((snd_info *)data, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
}


static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_amp_control = sp->saved_amp_control;
  sp->saved_amp_control = sp->amp_control;
  return(false);
}


/* -------- SPEED CALLBACKS -------- */

static mus_float_t speed_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}


static mus_float_t scroll_to_speed(snd_info *sp, mus_float_t ival)
{
  char speed_number_buffer[6];
  sp->speed_control = speed_changed(exp((ival * (log(sp->speed_control_max) - log(sp->speed_control_min)) / 0.9) + log(sp->speed_control_min)),
				    speed_number_buffer,
				    sp->speed_control_style,
				    sp->speed_control_tones,
				    6);
  gtk_label_set_text(GTK_LABEL(SPEED_LABEL(sp)), speed_number_buffer);
  return(ival);
}


static bool ignore_callback = false;

void set_speed(snd_info *sp, mus_float_t val)
{
  if (!has_widgets(sp))
    sp->speed_control = val;
  else
    {
      GtkAdjustment *adj;
      adj = SPEED_ADJUSTMENT(sp);
      ignore_callback = true;
      ADJUSTMENT_SET_VALUE(adj, scroll_to_speed(sp, speed_to_scroll(sp->speed_control_min, val, sp->speed_control_max)));
      /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj)); */
      ignore_callback = false;
    }
}


static gboolean speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if (EVENT_STATE(ev) & (snd_ControlMask | snd_MetaMask)) 
    set_speed(sp, sp->last_speed_control);
  else set_speed(sp, 1.0);

#if XEN_HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif

  return(false);
}


static gboolean speed_label_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  switch (sp->speed_control_style)
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    sp->speed_control_style = SPEED_CONTROL_AS_RATIO;    break;
    case SPEED_CONTROL_AS_RATIO:    sp->speed_control_style = SPEED_CONTROL_AS_SEMITONE; break;
    case SPEED_CONTROL_AS_SEMITONE: sp->speed_control_style = SPEED_CONTROL_AS_FLOAT;    break;
    }
  set_speed(sp, sp->speed_control);  /* remake label */
  return(false);
}


static void speed_changed_callback(GtkAdjustment *adj, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (ignore_callback) return;
  scroll_to_speed(sp, ADJUSTMENT_VALUE(adj));
#if XEN_HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
}


static gboolean speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_speed_control = sp->saved_speed_control;
  sp->saved_speed_control = sp->speed_control;
  return(false);
}


static void draw_speed_arrow(snd_info *sp)
{
  if (sp->speed_control_direction == 1)
    draw_picture(sp->speed_arrow_ax, speed_r, 0, 0, 0, 0, 16, 16);
  else draw_picture(sp->speed_arrow_ax, speed_l, 0, 0, 0, 0, 16, 16);
}


static gboolean speed_arrow_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (sp->speed_control_direction == 1)
    sp->speed_control_direction = -1;
  else sp->speed_control_direction = 1;
  gtk_widget_hide(SPEED_ARROW(sp));
  gtk_widget_show(SPEED_ARROW(sp));
  /* draw_speed_arrow(sp); */
  return(false);
}


static gboolean speed_arrow_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  draw_speed_arrow((snd_info *)data);
  return(false);
}


void toggle_direction_arrow(snd_info *sp, bool state)
{
  /* this is part of the apply-controls junk */
  int dir = 1;
  if (state) dir = -1;
  sp->speed_control_direction = dir;
  if (has_widgets(sp)) draw_speed_arrow(sp);
}


/* -------- EXPAND CALLBACKS -------- */

static mus_float_t expand_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}


static mus_float_t scroll_to_expand(snd_info *sp, mus_float_t val)
{
  char expand_number_buffer[6];
  if (val <= 0.0) 
    sp->expand_control = sp->expand_control_min;
  else
    {
      if (val >= 0.9)
	sp->expand_control = sp->expand_control_max;
      else sp->expand_control = exp((val * (log(sp->expand_control_max) - log(sp->expand_control_min)) / 0.9) + log(sp->expand_control_min));
    }
  if (sp->playing) dac_set_expand(sp, sp->expand_control);
  snprintf(expand_number_buffer, 6, "%.3f", sp->expand_control);
  gtk_label_set_text(GTK_LABEL(EXPAND_LABEL(sp)), expand_number_buffer);
  return(val);
}


void set_expand(snd_info *sp, mus_float_t val) 
{
  if (!has_widgets(sp))
    sp->expand_control = val;
  else
    {
      GtkAdjustment *adj;
      adj = EXPAND_ADJUSTMENT(sp);
      ignore_callback = true;
      ADJUSTMENT_SET_VALUE(adj, scroll_to_expand(sp, expand_to_scroll(sp->expand_control_min, val, sp->expand_control_max)));
      /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj)); */
      ignore_callback = false;
    }
}


static void expand_changed_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  scroll_to_expand((snd_info *)data, ADJUSTMENT_VALUE(adj));
}


static gboolean expand_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_expand_control = sp->saved_expand_control;
  sp->saved_expand_control = sp->expand_control;
  return(false);
}


static gboolean expand_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if (EVENT_STATE(ev) & (snd_ControlMask | snd_MetaMask)) 
    set_expand(sp, sp->last_expand_control);
  else set_expand(sp, 1.0);
  return(false);
}


static void expand_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->expand_control_on = TOGGLE_BUTTON_ACTIVE(w);
  /* to change the trough color: (widget_modify_bg (list-ref (channel-widgets) 3) GTK_STATE_ACTIVE (zoom-color)) */
  /*   and the slider color:     (widget_modify_bg (list-ref (channel-widgets) 3) GTK_STATE_PRELIGHT (highlight-color)) */
}


void toggle_expand_button(snd_info *sp, bool state)
{
  if (!has_widgets(sp))
    sp->expand_control_on = state;
  else set_toggle_button(EXPAND_RIGHT_BUTTON(sp), state, true, (void *)sp);
}


/* -------- CONTRAST CALLBACKS -------- */

static mus_float_t contrast_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return((val - minval) / (maxval - minval) * 0.9);
}


static mus_float_t scroll_to_contrast(snd_info *sp, mus_float_t val)
{
  char contrast_number_buffer[6];
  sp->contrast_control = sp->contrast_control_min + val * (sp->contrast_control_max - sp->contrast_control_min) / 0.9;
  snprintf(contrast_number_buffer, 6, "%.3f", sp->contrast_control);
  gtk_label_set_text(GTK_LABEL(CONTRAST_LABEL(sp)), contrast_number_buffer);
  return(val);
}


void set_contrast(snd_info *sp, mus_float_t val) 
{
  if (!has_widgets(sp))
    sp->contrast_control = val;
  else
    {
      GtkAdjustment *adj;
      adj = CONTRAST_ADJUSTMENT(sp);
      ignore_callback = true;
      ADJUSTMENT_SET_VALUE(adj, scroll_to_contrast(sp, contrast_to_scroll(sp->contrast_control_min, val, sp->contrast_control_max)));
      /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj)); */
      ignore_callback = false;
    }
}


static gboolean contrast_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if (EVENT_STATE(ev) & (snd_ControlMask | snd_MetaMask)) 
    set_contrast(sp, sp->last_contrast_control);
  else set_contrast(sp, 0.0);
  return(false);
}


static void contrast_changed_callback(GtkAdjustment *adj, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (ignore_callback) return;
  scroll_to_contrast(sp, ADJUSTMENT_VALUE(adj));
}


static gboolean contrast_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_contrast_control = sp->saved_contrast_control;
  sp->saved_contrast_control = sp->contrast_control;
  return(false);
}


static void contrast_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->contrast_control_on = TOGGLE_BUTTON_ACTIVE(w);
}


void toggle_contrast_button(snd_info *sp, bool state)
{
  if (!has_widgets(sp))
    sp->contrast_control_on = state;
  else set_toggle_button(CONTRAST_RIGHT_BUTTON(sp), state, true, (void *)sp);
}


/* -------- REVERB CALLBACKS -------- */

static mus_float_t revscl_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * (pow(val, 0.333) - pow(minval, 0.333)) / (pow(maxval, 0.333) - pow(minval, 0.333)));
}


static mus_float_t cube(mus_float_t a) {return(a*a*a);}


static mus_float_t scroll_to_revscl(snd_info *sp, mus_float_t val)
{
  char revscl_number_buffer[7];
  if (val <= 0.0) 
    sp->reverb_control_scale = sp->reverb_control_scale_min;
  else
    {
      if (val >= 0.9)
	sp->reverb_control_scale = sp->reverb_control_scale_max;
      else sp->reverb_control_scale = cube((val * (pow(sp->reverb_control_scale_max, 0.333) - pow(sp->reverb_control_scale_min, 0.333)) / 0.9) + 
					   pow(sp->reverb_control_scale_min, 0.333));
    }
  snprintf(revscl_number_buffer, 7, "%.4f", sp->reverb_control_scale);
  gtk_label_set_text(GTK_LABEL(REVSCL_LABEL(sp)), revscl_number_buffer);
  return(val);
}


void set_revscl(snd_info *sp, mus_float_t val) 
{
  if (!has_widgets(sp))
    sp->reverb_control_scale = val;
  else
    {
      GtkAdjustment *adj;
      adj = REVSCL_ADJUSTMENT(sp);
      ignore_callback = true;
      ADJUSTMENT_SET_VALUE(adj, scroll_to_revscl(sp, revscl_to_scroll(sp->reverb_control_scale_min, val, sp->reverb_control_scale_max)));
      /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj)); */
      ignore_callback = false;
    }
}


static gboolean revscl_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if (EVENT_STATE(ev) & (snd_ControlMask | snd_MetaMask)) 
    set_revscl(sp, sp->last_reverb_control_scale);
  else set_revscl(sp, 0.0);
  return(false);
}


static void revscl_changed_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  scroll_to_revscl((snd_info *)data, ADJUSTMENT_VALUE(adj));
}


static gboolean revscl_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_reverb_control_scale = sp->saved_reverb_control_scale;
  sp->saved_reverb_control_scale = sp->reverb_control_scale;
  return(false);
}


static mus_float_t revlen_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return((val - minval) / (maxval - minval) * 0.9);
}


static mus_float_t scroll_to_revlen(snd_info *sp, mus_float_t val)
{
  char revlen_number_buffer[5];
  sp->reverb_control_length = sp->reverb_control_length_min + 
    (sp->reverb_control_length_max - sp->reverb_control_length_min) * (mus_float_t)val / 0.9;
  snprintf(revlen_number_buffer, 5, "%.2f", sp->reverb_control_length);
  gtk_label_set_text(GTK_LABEL(REVLEN_LABEL(sp)), revlen_number_buffer);
  return(val);
}


void set_revlen(snd_info *sp, mus_float_t val)
{
  if (!has_widgets(sp))
    sp->reverb_control_length = val;
  else
    {
      GtkAdjustment *adj;
      adj = REVLEN_ADJUSTMENT(sp);
      ignore_callback = true;
      ADJUSTMENT_SET_VALUE(adj, scroll_to_revlen(sp, revlen_to_scroll(sp->reverb_control_length_min, val, sp->reverb_control_length_max)));
      /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj)); */
      ignore_callback = false;
    }
}


static gboolean revlen_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;

  if (EVENT_STATE(ev) & (snd_ControlMask | snd_MetaMask)) 
    set_revlen(sp, sp->last_reverb_control_length);
  else set_revlen(sp, 1.0);
  return(false);
}


static void revlen_changed_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  scroll_to_revlen((snd_info *)data, ADJUSTMENT_VALUE(adj));
}


static gboolean revlen_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_reverb_control_length = sp->saved_reverb_control_length;
  sp->saved_reverb_control_length = sp->reverb_control_length;
  return(false);
}


static void reverb_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->reverb_control_on = TOGGLE_BUTTON_ACTIVE(w);
}


void toggle_reverb_button(snd_info *sp, bool state)
{
  if (!has_widgets(sp))
    sp->reverb_control_on = state;
  else set_toggle_button(REVERB_RIGHT_BUTTON(sp), state, true, (void *)sp);
}


/* -------- FILTER CALLBACKS -------- */

#define MIN_FILTER_GRAPH_HEIGHT 20

void display_filter_env(snd_info *sp)
{
  graphics_context *ax;
  int height, width;
  GtkWidget *drawer;
  env_editor *edp;

  if (!has_widgets(sp)) return;

  edp = sp->flt;
  drawer = FILTER_ENV(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);

  if (!sp->filter_ax)
    {
      ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      ax->wn = WIDGET_TO_WINDOW(drawer);
      ax->w = drawer;
      sp->filter_ax = ax;
    }
  else 
    {
      ax = sp->filter_ax;
    }

  ax->gc = ss->fltenv_basic_gc;
  ss->cr = make_cairo(ax->wn);
  cairo_push_group(ss->cr);

  /* erase previous */
  cairo_set_source_rgba(ss->cr, ax->gc->bg_color->red, ax->gc->bg_color->green, ax->gc->bg_color->blue, ax->gc->bg_color->alpha);
  cairo_rectangle(ss->cr, 0, 0, width, height);
  cairo_fill(ss->cr);

  edp->in_dB = sp->filter_control_in_dB;
  edp->with_dots = true;
  if (!sp->filter_control_envelope) 
    sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);

  
  env_editor_display_env(edp, sp->filter_control_envelope, ax, "frequency response", 0, 0, width, height, NOT_PRINTING);

  if (edp->edited)
    {
      ax->gc = ss->fltenv_data_gc;
      display_frequency_response(sp->filter_control_envelope, 
				 (sound_env_editor(sp))->axis, ax, 
				 sp->filter_control_order, 
				 sp->filter_control_in_dB);
    }

  cairo_pop_group_to_source(ss->cr);
  cairo_paint(ss->cr);
  free_cairo(ss->cr);
  ss->cr = NULL;
}


static gboolean filter_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  if (BUTTON1_PRESSED(EVENT_STATE(ev)))
    {
      snd_info *sp = (snd_info *)data;
      int evx, evy;
      GdkModifierType state;
      env_editor *edp;
      if (EVENT_IS_HINT(ev))
	window_get_pointer(ev, &evx, &evy, &state);
      else
	{
	  evx = (int)(EVENT_X(ev));
	  evy = (int)(EVENT_Y(ev));
	}
      edp = sp->flt;
      edp->in_dB = sp->filter_control_in_dB;
      env_editor_button_motion(edp, evx, evy, EVENT_TIME(ev), sp->filter_control_envelope);
      display_filter_env(sp);
      sp->filter_control_changed = true;
    }
  return(false);
}


static gboolean filter_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  env_editor *edp;
  edp = sp->flt;
  edp->in_dB = sp->filter_control_in_dB;
  if (env_editor_button_press(edp, (int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), EVENT_TIME(ev), sp->filter_control_envelope))
    display_filter_env(sp);
  return(false);
}


static gboolean filter_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  char *tmpstr = NULL;
  snd_info *sp = (snd_info *)data;
  env_editor_button_release(sound_env_editor(sp), sp->filter_control_envelope);
  display_filter_env(sp);
  set_filter_text(sp, tmpstr = env_to_string(sp->filter_control_envelope));
  if (tmpstr) free(tmpstr);
  sp->filter_control_changed = true;
  return(false);
}


void set_filter_text(snd_info *sp, const char *str)
{
  if (has_widgets(sp))
    {
      if (str)
	gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), str);
      else gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), stupid);
    }
}


static gboolean filter_drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  display_filter_env(sp);
  return(false);
}


static gboolean filter_drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  display_filter_env(sp);
  return(false);
}


static void filter_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->filter_control_on = TOGGLE_BUTTON_ACTIVE(w);
}


void toggle_filter_button(snd_info *sp, bool state)
{
  if (!has_widgets(sp))
    sp->filter_control_on = state;
  else set_toggle_button(FILTER_RIGHT_BUTTON(sp), state, true, (void *)sp);
}


static void filter_db_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->filter_control_in_dB = TOGGLE_BUTTON_ACTIVE(w);
  display_filter_env(sp);
}


void set_filter_in_dB(snd_info *sp, bool val)
{
  sp->filter_control_in_dB = val;
  if (has_widgets(sp))
    {
      set_toggle_button(FILTER_DB_BUTTON(sp), val, false, (void *)sp);
      display_filter_env(sp);
    }
}


static void new_in_hz(snd_info *sp, bool val)
{
  sp->filter_control_in_hz = val;
  if (val)
    sp->filter_control_xmax = (mus_float_t)(snd_srate(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  if (sp->filter_control_envelope) free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
}


static void filter_hz_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  new_in_hz(sp, TOGGLE_BUTTON_ACTIVE(w));
  display_filter_env(sp);
}


void set_filter_in_hz(snd_info *sp, bool val)
{
  new_in_hz(sp, val);
  if (has_widgets(sp))
    {
      set_toggle_button(FILTER_HZ_BUTTON(sp), val, false, (void *)sp);
      display_filter_env(sp);
    }
}


static void set_filter_order_1(snd_info *sp, int order, bool setadj)
{
  sp->filter_control_order = order;
  if (setadj)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)), (gfloat)order);
  display_filter_env(sp);
  sp->filter_control_changed = true;
}  


void set_filter_order(snd_info *sp, int order) 
{
  if (!has_widgets(sp))
    sp->filter_control_order = order;
  else set_filter_order_1(sp, order, true);
}


static void filter_order_callback(GtkWidget *w, gpointer data)
{
  int order;
  snd_info *sp = (snd_info *)data;
  order = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)));
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_control_order = order;
  set_filter_order_1(sp, order, false);
}


static void filter_activate_callback(GtkWidget *w, gpointer context)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)context;
  char *str = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  redirect_errors_to(errors_to_status_area, (void *)sp);
  sp->filter_control_envelope = string_to_env((const char *)str);
  redirect_errors_to(NULL, NULL);
  if (!(sp->filter_control_envelope)) /* maybe user cleared text field? */
    sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  (sound_env_editor(sp))->edited = true;
  display_filter_env(sp);
  sp->filter_control_changed = true;
}


void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  if (has_widgets(sp))
    {
      char *tmpstr;
      tmpstr = env_to_string(e);
      if (tmpstr)
	{
	  gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), tmpstr);
	  free(tmpstr);
	}
      else gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), stupid);
      (sound_env_editor(sp))->edited = true;
      display_filter_env(sp);
      /* this is called also from snd-scm.c */
    }
  sp->filter_control_changed = true;
}


void color_filter_waveform(color_info *color)
{
  int i;
  gc_set_foreground(ss->fltenv_data_gc, color);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	display_filter_env(sp);
    }
}


/* -------- AMP ENVS ETC -------- */

static int cant_write(char *name)
{
#ifndef _MSC_VER
  return((access(name, W_OK)) != 0);
#else
  return(0);
#endif
}


static gint close_sound_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  if (sp) 
    {
      snd_close_file(sp);
      gtk_widget_hide(sp->dialog); 
    }
  return(true);
} 


static gboolean stop_sign_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if ((ss->checking_explicitly) || (play_in_progress())) ss->stopped_explicitly = true; 
  stop_playing_all_sounds(PLAY_C_G);
  if (sp->applying) stop_applying(sp);
  for_each_sound_chan(sp, stop_fft_in_progress);
  return(false);
}


static void show_sync_button(snd_info *sp)
{
  gtk_widget_show(SYNC_BUTTON(sp));
}


static Xen reflect_file_close_in_sync(Xen hook_or_reason)
{
  int reason;
#if HAVE_SCHEME
  reason = Xen_integer_to_C_int(s7_let_ref(s7, hook_or_reason, s7_make_symbol(s7, "reason")));
#else
  reason = Xen_integer_to_C_int(hook_or_reason);
#endif
  if ((reason == FILE_CLOSED) && /* snd-file.c */
      (ss->active_sounds == 1))
    {
      snd_info *sp;
      sp = any_selected_sound();
      if ((sp) && (sp->nchans == 1))
	gtk_widget_hide(SYNC_BUTTON(sp));
    }
  return(Xen_false);
}

Xen_wrap_1_arg(reflect_file_close_in_sync_w, reflect_file_close_in_sync)


static void close_button_callback(GtkWidget *w, gpointer context)
{
  snd_close_file((snd_info *)context);
}


static gboolean close_button_tooltip(GtkWidget *w, gint x, gint y, gboolean keyboard_tip, GtkTooltip *tooltip, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  char *tip;
  tip = mus_format("close %s", sp->short_filename);
  gtk_tooltip_set_text(tooltip, tip);
  free(tip);
  return(true);
}




/* -------- SOUND PANE -------- */

#define BUTTON_SPACE 6

static bool currently_showing_controls = false;

snd_info *add_sound_window(char *filename, read_only_t read_only, file_info *hdr)
{
  snd_info *sp, *osp;
  int snd_slot, nchans, i, old_chans;
  bool free_filename = false, make_widgets;
  char *old_name = NULL;
  int app_y, app_dy, chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */

  if (ss->translated_filename) 
    {
      old_name = filename;
      filename = ss->translated_filename;
      ss->translated_filename = NULL;
      free_filename = true;
    }

  nchans = hdr->chans;
  if (nchans <= 0) nchans = 1;

  app_y = widget_y(main_shell(ss));
  app_dy = widget_height(main_shell(ss));

  if (auto_resize(ss))
    {
      int screen_y;
#if GTK_CHECK_VERSION(3, 22, 0)
      screen_y = 600; /* who knows? */
#else
      screen_y = gdk_screen_height();
#endif
      app_dy = (screen_y - app_y - app_dy - 20 * nchans);
    }
  else app_dy -= listener_height();

  chan_min_y = app_dy / nchans;
  if (chan_min_y > ss->channel_min_height)
    chan_min_y = ss->channel_min_height; 
  else 
    if (chan_min_y < 5) 
      chan_min_y = 5;

  snd_slot = find_free_sound_slot(nchans); /* expands sound list if needed */
  if (ss->sounds[snd_slot])                    /* we're trying to re-use an old, inactive set of widgets and whatnot */
    {
      osp = ss->sounds[snd_slot];
      old_chans = osp->allocated_chans;
    }
  else old_chans = 0;

  make_widgets = (!ss->sounds[snd_slot]);
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], filename, hdr, snd_slot, read_only);
  sp = ss->sounds[snd_slot];
  sp->inuse = SOUND_NORMAL;

  sp->write_date = file_write_date(filename); /* needed early in this process by the peak-env handlers */
  make_pixmaps();

  if (!sp->snd_widgets)
    {
      sp->snd_widgets = (GtkWidget **)calloc(NUM_SND_WIDGETS, sizeof(GtkWidget *));
      sp->snd_adjs = (GtkAdjustment **)calloc(NUM_SND_ADJS, sizeof(GtkAdjustment *));
    }

  if (!(auto_resize(ss))) gtk_window_set_resizable(GTK_WINDOW(main_shell(ss)), false);
  if ((!make_widgets) && (old_chans < nchans))
    {
      for (i = old_chans; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 1, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
    }

  if (make_widgets)
    {
      SND_PANE(sp) = gtk_vpaned_new();
      add_paned_style(SND_PANE(sp));
      set_user_int_data(G_OBJECT(SND_PANE(sp)), sp->index);
      sg_container_set_border_width(GTK_CONTAINER(SND_PANE(sp)), 4); /* this is the outer margin of each sound's box -- 6 seems slightly large */
      /* I tried putting a frame around the entire pane, but it looked fussy, and the frame bottom cut into the filter widget! */

      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  sp->dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	  sg_make_resizable(sp->dialog);
	  gtk_container_add(GTK_CONTAINER(sp->dialog), SND_PANE(sp));
	  gtk_widget_show(sp->dialog);
	  SG_SIGNAL_CONNECT(sp->dialog, "delete_event", close_sound_dialog, sp);
	}
      else
	{
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
	      GtkWidget *tablab;
	      tablab = gtk_label_new(sp->short_filename);
	      gtk_widget_show(tablab);
	      gtk_notebook_append_page(GTK_NOTEBOOK(sound_pane_box(ss)), SND_PANE(sp), tablab);
	    }
	  else sg_box_pack_start(GTK_BOX(sound_pane_box(ss)), SND_PANE(sp), true, true, 0);
	  /* child2 is listener */
	}

      PANE_BOX(sp) = gtk_vbox_new(false, 0);
      gtk_paned_pack1(GTK_PANED(SND_PANE(sp)), PANE_BOX(sp), false, false); /* not add1 as of gtk 3.18.9 (Tito Latini) */
      gtk_widget_show(PANE_BOX(sp));

      NAME_HBOX(sp) = gtk_hbox_new(false, 0);
      sg_box_pack_end(GTK_BOX(PANE_BOX(sp)), NAME_HBOX(sp), false, false, 0);
      
      for (i = 0; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);

      /* controls etc */

      CONTROL_PANEL(sp) = gtk_vbox_new(false, 0);
      gtk_paned_pack2(GTK_PANED(SND_PANE(sp)), CONTROL_PANEL(sp), false, true); /* add2 but resize=false */
  

      /* -------- NAME FIELDS -------- */

      CLOSE_BUTTON(sp) = gtk_button_new();
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(CLOSE_BUTTON(sp));
#endif
      add_tooltip(CLOSE_BUTTON(sp), "close current sound");
      gtk_button_set_relief(GTK_BUTTON(CLOSE_BUTTON(sp)), GTK_RELIEF_NONE);
      gtk_button_set_image(GTK_BUTTON(CLOSE_BUTTON(sp)), image_new_with_icon(ICON_CLOSE, GTK_ICON_SIZE_MENU));
      sg_box_pack_start(GTK_BOX(NAME_HBOX(sp)), CLOSE_BUTTON(sp), false, false, 8);
      SG_SIGNAL_CONNECT(CLOSE_BUTTON(sp), "clicked", close_button_callback, sp);
      g_signal_connect(CLOSE_BUTTON(sp), "query-tooltip", G_CALLBACK(close_button_tooltip), (gpointer)sp);
      gtk_widget_show(CLOSE_BUTTON(sp));


      NAME_EVENT_BOX(sp) = gtk_event_box_new();
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(NAME_EVENT_BOX(sp));
#endif
      add_tooltip(NAME_EVENT_BOX(sp), "name of current sound"); /* just a placeholder */
      sg_box_pack_start(GTK_BOX(NAME_HBOX(sp)), NAME_EVENT_BOX(sp), false, false, 5);
      gtk_widget_show(NAME_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(NAME_EVENT_BOX(sp), "button_press_event", name_click_callback, sp);
      g_signal_connect(NAME_EVENT_BOX(sp), "query-tooltip", G_CALLBACK(name_button_tooltip), (gpointer)sp);
      
      NAME_BUTTON(sp) = gtk_label_new(shortname_indexed(sp));
      gtk_container_add(GTK_CONTAINER(NAME_EVENT_BOX(sp)), NAME_BUTTON(sp));
      gtk_widget_show(NAME_BUTTON(sp));
      
      NAME_PIX(sp) = gtk_drawing_area_new();
      sg_widget_set_events(NAME_PIX(sp), GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(NAME_PIX(sp), 16, 16);
      sg_box_pack_start(GTK_BOX(NAME_HBOX(sp)), NAME_PIX(sp), false, false, 2);
      gtk_widget_show(NAME_PIX(sp));
      sp->name_pix_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      sp->name_pix_ax->wn = WIDGET_TO_WINDOW(NAME_PIX(sp));
      sp->name_pix_ax->gc = ss->basic_gc;
      SG_SIGNAL_CONNECT(NAME_PIX(sp), DRAW_SIGNAL, name_pix_expose, sp);

      STOP_PIX(sp) = gtk_drawing_area_new();
      sg_widget_set_events(STOP_PIX(sp), GDK_BUTTON_PRESS_MASK);
      gtk_widget_set_size_request(STOP_PIX(sp), 16, 16);
      sg_box_pack_start(GTK_BOX(NAME_HBOX(sp)), STOP_PIX(sp), false, false, 2);
      gtk_widget_show(STOP_PIX(sp));
      sp->stop_pix_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      sp->stop_pix_ax->wn = WIDGET_TO_WINDOW(STOP_PIX(sp));
      sp->stop_pix_ax->gc = ss->basic_gc;
      SG_SIGNAL_CONNECT(STOP_PIX(sp), "button_press_event", stop_sign_press, sp);

      {
	unsigned int i;

	sp->clock_widgets = (GtkWidget **)calloc(sp->nchans, sizeof(GtkWidget *));
	sp->clock_pix_ax = (graphics_context **)calloc(sp->nchans, sizeof(graphics_context *));
	sp->num_clock_widgets = sp->nchans;

	for (i = 0; i < sp->nchans; i++)
	  {
	    CLOCK_PIX(sp, i) = gtk_drawing_area_new();
	    gtk_widget_set_size_request(CLOCK_PIX(sp, i), 16, 16);
	    sg_box_pack_start(GTK_BOX(NAME_HBOX(sp)), CLOCK_PIX(sp, i), false, false, 2);
	    gtk_widget_show(CLOCK_PIX(sp, i));
	    sp->clock_pix_ax[i] = (graphics_context *)calloc(1, sizeof(graphics_context));
	    sp->clock_pix_ax[i]->wn = WIDGET_TO_WINDOW(CLOCK_PIX(sp, i));
	    sp->clock_pix_ax[i]->gc = ss->basic_gc;
	    SG_SIGNAL_CONNECT(CLOCK_PIX(sp, i), DRAW_SIGNAL, clock_pix_expose, sp->chans[i]);
	  }
      }

      STATUS_AREA(sp) = gtk_label_new(NULL);
#if GTK_CHECK_VERSION(3, 0, 0)
      gtk_widget_set_halign(STATUS_AREA(sp), GTK_ALIGN_FILL);
      gtk_widget_set_hexpand(STATUS_AREA(sp), true);
#endif
      sg_box_pack_start(GTK_BOX(NAME_HBOX(sp)), STATUS_AREA(sp), true, true, 2);
      gtk_widget_show(STATUS_AREA(sp));

      /* now fill from other end */
      
#if WITH_AUDIO
      PLAY_BUTTON(sp) = gtk_check_button_new_with_label("play");
      sg_box_pack_end(GTK_BOX(NAME_HBOX(sp)), PLAY_BUTTON(sp), false, false, BUTTON_SPACE); /* need space here or "play" hits window edge */
      SG_SIGNAL_CONNECT(PLAY_BUTTON(sp), "button_press_event", play_button_callback, sp);
      SG_SIGNAL_CONNECT(PLAY_BUTTON(sp), "toggled", play_button_click_callback, sp);
      gtk_widget_show(PLAY_BUTTON(sp));
#endif
      
      SYNC_BUTTON(sp) = gtk_check_button_new_with_label("sync");
      add_tooltip(SYNC_BUTTON(sp), "group this sound with anything sharing its sync value");
      sg_box_pack_end(GTK_BOX(NAME_HBOX(sp)), SYNC_BUTTON(sp), false, false, BUTTON_SPACE);
      SG_SIGNAL_CONNECT(SYNC_BUTTON(sp), "button_press_event", sync_button_callback, sp);
      SG_SIGNAL_CONNECT(SYNC_BUTTON(sp), "toggled", sync_button_click, sp);
      gtk_widget_show(SYNC_BUTTON(sp));
      
      UNITE_BUTTON(sp) = gtk_check_button_new_with_label("unite");
      add_tooltip(UNITE_BUTTON(sp), "combine channel graphs in one window");
      sg_box_pack_end(GTK_BOX(NAME_HBOX(sp)), UNITE_BUTTON(sp), false, false, BUTTON_SPACE);
      SG_SIGNAL_CONNECT(UNITE_BUTTON(sp), "button_press_event", unite_button_callback, sp);
      SG_SIGNAL_CONNECT(UNITE_BUTTON(sp), "toggled", unite_button_click, sp);
      gtk_widget_show(UNITE_BUTTON(sp));
      
      gtk_widget_show(NAME_HBOX(sp));


      /* if control-panel */
      
      /* -------- AMP FIELDS -------- */
      
      NAME_SEPARATOR(sp) = gtk_hseparator_new();
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), NAME_SEPARATOR(sp), false, false, 4);
      gtk_widget_show(NAME_SEPARATOR(sp));
      
      AMP_HBOX(sp) = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), AMP_HBOX(sp), false, false, 0);
      
      AMP_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(AMP_HBOX(sp)), AMP_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(AMP_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(AMP_EVENT_BOX(sp), "button_press_event", amp_click_callback, sp);
      
      AMP_BUTTON(sp) = gtk_label_new("amp:");
      gtk_container_add(GTK_CONTAINER(AMP_EVENT_BOX(sp)), AMP_BUTTON(sp));
      gtk_widget_show(AMP_BUTTON(sp));
      
      AMP_LABEL(sp) = gtk_label_new("1.00 ");
      sg_box_pack_start(GTK_BOX(AMP_HBOX(sp)), AMP_LABEL(sp), false, false, 0);
      gtk_widget_show(AMP_LABEL(sp));
      
      AMP_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(amp_to_scroll(sp->amp_control_min, 1.0, sp->amp_control_max), 0.0, 1.0, 0.001, 0.01, .1);
      AMP_SCROLLBAR(sp) = gtk_hscrollbar_new(GTK_ADJUSTMENT(AMP_ADJUSTMENT(sp)));
      sg_box_pack_start(GTK_BOX(AMP_HBOX(sp)), AMP_SCROLLBAR(sp), true, true, 4);
      SG_SIGNAL_CONNECT(AMP_ADJUSTMENT(sp), "value_changed", amp_changed_callback, sp);
      SG_SIGNAL_CONNECT(AMP_SCROLLBAR(sp), "button_release_event", amp_release_callback, sp);
      gtk_widget_show(AMP_SCROLLBAR(sp));
      
      gtk_widget_show(AMP_HBOX(sp));
      
      
      /* -------- SPEED FIELDS -------- */
      
      SPEED_HBOX(sp) = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), SPEED_HBOX(sp), false, false, 0);
      
      SPEED_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(SPEED_HBOX(sp)), SPEED_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(SPEED_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(SPEED_EVENT_BOX(sp), "button_press_event", speed_click_callback, sp);
      
      SPEED_BUTTON(sp) = gtk_label_new("speed:");
      gtk_container_add(GTK_CONTAINER(SPEED_EVENT_BOX(sp)), SPEED_BUTTON(sp));
      gtk_widget_show(SPEED_BUTTON(sp));
      
      SPEED_LABEL_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(SPEED_HBOX(sp)), SPEED_LABEL_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(SPEED_LABEL_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(SPEED_LABEL_EVENT_BOX(sp), "button_press_event", speed_label_click_callback, sp);

      switch (sp->speed_control_style)
	{
	case SPEED_CONTROL_AS_RATIO:    SPEED_LABEL(sp) = gtk_label_new("  1/1"); break;
	case SPEED_CONTROL_AS_SEMITONE: SPEED_LABEL(sp) = gtk_label_new("    0"); break;
	default:                        SPEED_LABEL(sp) = gtk_label_new("1.00 "); break;
	}
      gtk_container_add(GTK_CONTAINER(SPEED_LABEL_EVENT_BOX(sp)), SPEED_LABEL(sp));
      gtk_widget_show(SPEED_LABEL(sp));
      
      SPEED_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(speed_to_scroll(sp->speed_control_min, 1.0, sp->speed_control_max), 0.0, 1.0, 0.001, 0.01, .1);
      SPEED_SCROLLBAR(sp) = gtk_hscrollbar_new(GTK_ADJUSTMENT(SPEED_ADJUSTMENT(sp)));
      sg_box_pack_start(GTK_BOX(SPEED_HBOX(sp)), SPEED_SCROLLBAR(sp), true, true, 4);
      SG_SIGNAL_CONNECT(SPEED_ADJUSTMENT(sp), "value_changed", speed_changed_callback, sp);
      SG_SIGNAL_CONNECT(SPEED_SCROLLBAR(sp), "button_release_event", speed_release_callback, sp);
      gtk_widget_show(SPEED_SCROLLBAR(sp));

      SPEED_ARROW(sp) = gtk_drawing_area_new();
      sg_widget_set_events(SPEED_ARROW(sp), GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK);
      sg_box_pack_start(GTK_BOX(SPEED_HBOX(sp)), SPEED_ARROW(sp), false, false, 2);
      gtk_widget_set_size_request(SPEED_ARROW(sp), 16, 16);
      gtk_widget_show(SPEED_ARROW(sp));
      sp->speed_arrow_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      sp->speed_arrow_ax->wn = WIDGET_TO_WINDOW(SPEED_ARROW(sp));
      sp->speed_arrow_ax->gc = ss->basic_gc;
      SG_SIGNAL_CONNECT(SPEED_ARROW(sp), DRAW_SIGNAL, speed_arrow_expose, sp);
      SG_SIGNAL_CONNECT(SPEED_ARROW(sp), "button_press_event", speed_arrow_press, sp);

      gtk_widget_show(SPEED_HBOX(sp));
      
      
      /* -------- EXPAND FIELDS -------- */
      
      EXPAND_HBOX(sp) = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), EXPAND_HBOX(sp), false, false, 0);
      
      EXPAND_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(EXPAND_HBOX(sp)), EXPAND_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(EXPAND_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(EXPAND_EVENT_BOX(sp), "button_press_event", expand_click_callback, sp);
      
      EXPAND_LEFT_BUTTON(sp) = gtk_label_new("expand:");
      gtk_container_add(GTK_CONTAINER(EXPAND_EVENT_BOX(sp)), EXPAND_LEFT_BUTTON(sp));
      gtk_widget_show(EXPAND_LEFT_BUTTON(sp));
      
      EXPAND_LABEL(sp) = gtk_label_new("1.00 ");
      sg_box_pack_start(GTK_BOX(EXPAND_HBOX(sp)), EXPAND_LABEL(sp), false, false, 0);
      gtk_widget_show(EXPAND_LABEL(sp));
      
      EXPAND_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(expand_to_scroll(sp->expand_control_min, 1.0, sp->expand_control_max), 0.0, 1.0, 0.001, 0.01, .1);
      EXPAND_SCROLLBAR(sp) = gtk_hscrollbar_new(GTK_ADJUSTMENT(EXPAND_ADJUSTMENT(sp)));
      sg_box_pack_start(GTK_BOX(EXPAND_HBOX(sp)), EXPAND_SCROLLBAR(sp), true, true, 4);
      SG_SIGNAL_CONNECT(EXPAND_ADJUSTMENT(sp), "value_changed", expand_changed_callback, sp);
      SG_SIGNAL_CONNECT(EXPAND_SCROLLBAR(sp), "button_release_event", expand_release_callback, sp);
      gtk_widget_show(EXPAND_SCROLLBAR(sp));
      
      EXPAND_RIGHT_BUTTON(sp) = gtk_check_button_new();
      sg_box_pack_start(GTK_BOX(EXPAND_HBOX(sp)), EXPAND_RIGHT_BUTTON(sp), false, false, 3);
      SG_SIGNAL_CONNECT(EXPAND_RIGHT_BUTTON(sp), "clicked", expand_button_callback, sp);
      gtk_widget_show(EXPAND_RIGHT_BUTTON(sp));
      
      gtk_widget_show(EXPAND_HBOX(sp));
      
      
      /* -------- CONTRAST FIELDS -------- */
      
      CONTRAST_HBOX(sp) = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), CONTRAST_HBOX(sp), false, false, 0);
      
      CONTRAST_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(CONTRAST_HBOX(sp)), CONTRAST_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(CONTRAST_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(CONTRAST_EVENT_BOX(sp), "button_press_event", contrast_click_callback, sp);
      
      CONTRAST_LEFT_BUTTON(sp) = gtk_label_new("contrast:");
      gtk_container_add(GTK_CONTAINER(CONTRAST_EVENT_BOX(sp)), CONTRAST_LEFT_BUTTON(sp));
      gtk_widget_show(CONTRAST_LEFT_BUTTON(sp));
      
      CONTRAST_LABEL(sp) = gtk_label_new("0.00 ");
      sg_box_pack_start(GTK_BOX(CONTRAST_HBOX(sp)), CONTRAST_LABEL(sp), false, false, 0);
      gtk_widget_show(CONTRAST_LABEL(sp));
      
      CONTRAST_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      CONTRAST_SCROLLBAR(sp) = gtk_hscrollbar_new(GTK_ADJUSTMENT(CONTRAST_ADJUSTMENT(sp)));
      sg_box_pack_start(GTK_BOX(CONTRAST_HBOX(sp)), CONTRAST_SCROLLBAR(sp), true, true, 4);
      SG_SIGNAL_CONNECT(CONTRAST_ADJUSTMENT(sp), "value_changed", contrast_changed_callback, sp);
      SG_SIGNAL_CONNECT(CONTRAST_SCROLLBAR(sp), "button_release_event", contrast_release_callback, sp);
      gtk_widget_show(CONTRAST_SCROLLBAR(sp));
      
      CONTRAST_RIGHT_BUTTON(sp) = gtk_check_button_new();
      sg_box_pack_start(GTK_BOX(CONTRAST_HBOX(sp)), CONTRAST_RIGHT_BUTTON(sp), false, false, 3);
      SG_SIGNAL_CONNECT(CONTRAST_RIGHT_BUTTON(sp), "clicked", contrast_button_callback, sp);
      gtk_widget_show(CONTRAST_RIGHT_BUTTON(sp));
      
      gtk_widget_show(CONTRAST_HBOX(sp));
      
      
      /* -------- REVERB FIELDS -------- */
      
      REVERB_HBOX(sp) = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), REVERB_HBOX(sp), false, false, 0);
      
      REVSCL_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVSCL_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(REVSCL_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(REVSCL_EVENT_BOX(sp), "button_press_event", revscl_click_callback, sp);
      
      REVSCL_BUTTON(sp) = gtk_label_new("reverb:");
      gtk_container_add(GTK_CONTAINER(REVSCL_EVENT_BOX(sp)), REVSCL_BUTTON(sp));
      gtk_widget_show(REVSCL_BUTTON(sp));
      
      REVSCL_LABEL(sp) = gtk_label_new("0.000 ");
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVSCL_LABEL(sp), false, false, 0);
      gtk_widget_show(REVSCL_LABEL(sp));
      
      REVSCL_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      REVSCL_SCROLLBAR(sp) = gtk_hscrollbar_new(GTK_ADJUSTMENT(REVSCL_ADJUSTMENT(sp)));
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVSCL_SCROLLBAR(sp), true, true, 4);
      SG_SIGNAL_CONNECT(REVSCL_ADJUSTMENT(sp), "value_changed", revscl_changed_callback, sp);
      SG_SIGNAL_CONNECT(REVSCL_SCROLLBAR(sp), "button_release_event", revscl_release_callback, sp);
      gtk_widget_show(REVSCL_SCROLLBAR(sp));
      
      REVLEN_EVENT_BOX(sp) = gtk_event_box_new();
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVLEN_EVENT_BOX(sp), false, false, 4);
      gtk_widget_show(REVLEN_EVENT_BOX(sp));
      SG_SIGNAL_CONNECT(REVLEN_EVENT_BOX(sp), "button_press_event", revlen_click_callback, sp);
      
      REVLEN_BUTTON(sp) = gtk_label_new("len:");
      gtk_container_add(GTK_CONTAINER(REVLEN_EVENT_BOX(sp)), REVLEN_BUTTON(sp));
      gtk_widget_show(REVLEN_BUTTON(sp));
      
      REVLEN_LABEL(sp) = gtk_label_new("1.0 ");
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVLEN_LABEL(sp), false, false, 0);
      gtk_widget_show(REVLEN_LABEL(sp));
      
      REVLEN_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(revlen_to_scroll(sp->reverb_control_length_min, 
							       sp->reverb_control_length, 
							       sp->reverb_control_length_max), 
						 0.0, 1.0, 0.001, 0.01, .1);
      REVLEN_SCROLLBAR(sp) = gtk_hscrollbar_new(GTK_ADJUSTMENT(REVLEN_ADJUSTMENT(sp)));
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVLEN_SCROLLBAR(sp), true, true, 4);
      SG_SIGNAL_CONNECT(REVLEN_ADJUSTMENT(sp), "value_changed", revlen_changed_callback, sp);
      SG_SIGNAL_CONNECT(REVLEN_SCROLLBAR(sp), "button_release_event", revlen_release_callback, sp);
      gtk_widget_show(REVLEN_SCROLLBAR(sp));
      
      REVERB_RIGHT_BUTTON(sp) = gtk_check_button_new();
      sg_box_pack_start(GTK_BOX(REVERB_HBOX(sp)), REVERB_RIGHT_BUTTON(sp), false, false, 3);
      SG_SIGNAL_CONNECT(REVERB_RIGHT_BUTTON(sp), "clicked", reverb_button_callback, sp);
      gtk_widget_show(REVERB_RIGHT_BUTTON(sp));
      
      gtk_widget_show(REVERB_HBOX(sp));
      
      
      /* -------- FILTER FIELDS -------- */
      
      FILTER_HBOX(sp) = gtk_hbox_new(false, 2);
      sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), FILTER_HBOX(sp), false, false, 0);
      
      FILTER_LEFT_BUTTON(sp) = gtk_label_new("filter:");
      sg_box_pack_start(GTK_BOX(FILTER_HBOX(sp)), FILTER_LEFT_BUTTON(sp), false, false, 4);
      gtk_widget_show(FILTER_LEFT_BUTTON(sp));
      
      FILTER_ADJUSTMENT(sp) = (GtkAdjustment *)gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      FILTER_ORDER_TEXT(sp) = gtk_spin_button_new(GTK_ADJUSTMENT(FILTER_ADJUSTMENT(sp)), 0.0, 0);
      sg_box_pack_start(GTK_BOX(FILTER_HBOX(sp)), FILTER_ORDER_TEXT(sp), false, false, 2);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)), true);
      SG_SIGNAL_CONNECT(FILTER_ADJUSTMENT(sp), "value_changed", filter_order_callback, sp);
      SG_SIGNAL_CONNECT(FILTER_ORDER_TEXT(sp), "enter_notify_event", spin_button_focus_callback, NULL);
      SG_SIGNAL_CONNECT(FILTER_ORDER_TEXT(sp), "leave_notify_event", spin_button_unfocus_callback, NULL);
      gtk_widget_show(FILTER_ORDER_TEXT(sp));
      
      FILTER_COEFFS_TEXT(sp) = snd_entry_new(FILTER_HBOX(sp), NULL, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(FILTER_COEFFS_TEXT(sp), "activate", filter_activate_callback, sp);

      FILTER_HZ_BUTTON(sp) = gtk_check_button_new_with_label("hz");
      sg_box_pack_start(GTK_BOX(FILTER_HBOX(sp)), FILTER_HZ_BUTTON(sp), false, false, 2);
      SG_SIGNAL_CONNECT(FILTER_HZ_BUTTON(sp), "clicked", filter_hz_callback, sp);
      gtk_widget_show(FILTER_HZ_BUTTON(sp));
      
      FILTER_DB_BUTTON(sp) = gtk_check_button_new_with_label("dB");
      sg_box_pack_start(GTK_BOX(FILTER_HBOX(sp)), FILTER_DB_BUTTON(sp), false, false, 2);
      SG_SIGNAL_CONNECT(FILTER_DB_BUTTON(sp), "clicked", filter_db_callback, sp);
      gtk_widget_show(FILTER_DB_BUTTON(sp));
      
      FILTER_RIGHT_BUTTON(sp) = gtk_check_button_new();
      sg_box_pack_start(GTK_BOX(FILTER_HBOX(sp)), FILTER_RIGHT_BUTTON(sp), false, false, 3);
      SG_SIGNAL_CONNECT(FILTER_RIGHT_BUTTON(sp), "clicked", filter_button_callback, sp);
      gtk_widget_show(FILTER_RIGHT_BUTTON(sp));
      
      gtk_widget_show(FILTER_HBOX(sp));
      
      {
	GtkWidget *fbox;
	fbox = gtk_hbox_new(false, 4);
	sg_box_pack_start(GTK_BOX(CONTROL_PANEL(sp)), fbox, true, true, 0);
	gtk_widget_show(fbox);
	
	/* -------- FILTER GRAPH -------- */
      
	FILTER_FRAME(sp) = gtk_frame_new(NULL);
	sg_box_pack_start(GTK_BOX(fbox), FILTER_FRAME(sp), true, true, 30);
      
	FILTER_ENV(sp) = gtk_drawing_area_new();
	sg_widget_set_events(FILTER_ENV(sp), GDK_ALL_EVENTS_MASK);
	widget_modify_bg(FILTER_ENV(sp), GTK_STATE_NORMAL, ss->highlight_color);
	gtk_container_add(GTK_CONTAINER(FILTER_FRAME(sp)), FILTER_ENV(sp));
	gtk_widget_show(FILTER_ENV(sp));
	SG_SIGNAL_CONNECT(FILTER_ENV(sp), DRAW_SIGNAL, filter_drawer_expose, sp);
	SG_SIGNAL_CONNECT(FILTER_ENV(sp), "configure_event", filter_drawer_resize, sp);
	SG_SIGNAL_CONNECT(FILTER_ENV(sp), "button_press_event", filter_drawer_button_press, sp);
	SG_SIGNAL_CONNECT(FILTER_ENV(sp), "button_release_event", filter_drawer_button_release, sp);
	SG_SIGNAL_CONNECT(FILTER_ENV(sp), "motion_notify_event", filter_drawer_button_motion, sp);
	
	gtk_widget_show(FILTER_FRAME(sp));
	sp->flt = new_env_editor();
      }
      
      /* end if control-panel */
      gtk_widget_show(CONTROL_PANEL(sp));
      gtk_widget_show(SND_PANE(sp));

#if GTK_CHECK_VERSION(3, 0, 0)
      set_basic_color(ss->basic_color);
#endif
    } /* new sound ss */
  else
    { 
      int k;
      /* re-manage currently inactive chan */
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS) 
	raise_dialog(sp->dialog);
      gtk_widget_show(UNITE_BUTTON(sp));
      gtk_widget_show(SND_PANE(sp));
      for (k = 0; k < nchans; k++) 
	add_channel_window(sp, k, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
      gtk_label_set_text(GTK_LABEL(NAME_BUTTON(sp)), shortname_indexed(sp));
      reset_user_int_data(G_OBJECT(SND_PANE(sp)), sp->index); /* is this necessary? */
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(sound_pane_box(ss)), SND_PANE(sp), sp->short_filename);
      else reset_controls(sp); /* segfault here in notebook case! */
    }

  gtk_window_set_resizable(GTK_WINDOW(main_shell(ss)), true);
  if (currently_showing_controls) show_controls(sp); else hide_controls(sp);

  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
    {
      char *title;
      title = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
      gtk_window_set_title(GTK_WINDOW(sp->dialog), title);
      free(title);
    }

  if (sp->nchans == 1) 
    {
      gtk_widget_hide(UNITE_BUTTON(sp));
      if (ss->active_sounds == 0)
	gtk_widget_hide(SYNC_BUTTON(sp));
      else
	{
	  for_each_sound(show_sync_button); 
	}
    }
  else
    {
      for_each_sound(show_sync_button); 
    }

  add_sound_data(filename, sp, WITH_GRAPH);

  if (cant_write(sp->filename)) sp->file_read_only = FILE_READ_ONLY;
  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    show_lock(sp); 
  else hide_lock(sp);

  if (old_name)
    status_report(sp, "(translated %s)", old_name);
  after_open(sp);
  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
    {
      sp->page = gtk_notebook_page_num(GTK_NOTEBOOK(sound_pane_box(ss)), SND_PANE(sp));
      reset_controls(sp);
    }
  if (free_filename) free(filename);
  
#if GTK_CHECK_VERSION(3, 0, 0)
  if (listener_exists())
    gtk_paned_set_position(GTK_PANED(sound_pane(ss)), 50);
  /* actually we haven't reached full size here at start-up */
#endif
  return(sp);
}


void set_sound_pane_file_label(snd_info *sp, const char *str)
{
  if ((!sp->name_string) || 
      (strcmp(sp->name_string, str) != 0))
    {
      if (sp->name_string) free(sp->name_string);
      sp->name_string = mus_strdup(str);
      set_label(NAME_BUTTON(sp), str);
    }
}

void update_sound_label(snd_info *sp)
{
  if (has_widgets(sp))
    gtk_label_set_text(GTK_LABEL(NAME_BUTTON(sp)), shortname_indexed(sp));
}

void snd_info_cleanup(snd_info *sp)
{
  if (has_widgets(sp))
    {
      clear_status_area(sp);
      if (SYNC_BUTTON(sp))
	{
	  set_toggle_button(SYNC_BUTTON(sp), false, false, (void *)sp);
	  set_sync_color(sp);
	  set_toggle_button(EXPAND_RIGHT_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(CONTRAST_RIGHT_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(FILTER_RIGHT_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(REVERB_RIGHT_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(UNITE_BUTTON(sp), false, false, (void *)sp);
	}
      if ((sp->dialog) && (widget_is_active(sp->dialog)))
	gtk_widget_hide(sp->dialog);

      if ((sp->snd_widgets) && (SND_PANE(sp))) gtk_widget_hide(SND_PANE(sp));
      sp->channel_style = CHANNELS_SEPARATE; 
    }
}



/* ---------------- normalize sounds ---------------- */

void show_controls(snd_info *sp)
{
#if (!GTK_CHECK_VERSION(3, 90, 0))
  gtk_widget_show_all(CONTROL_PANEL(sp));
#endif
  /* control panel is pane 2 of SND_PANE(sp); PANE_BOX is pane 1 */
  /* gtk_paned_set_position(GTK_PANED(sound_pane(ss)), (gint)(widget_height(sound_pane(ss)) * .75)); (glistener) */
}


void hide_controls(snd_info *sp)
{
#if GTK_CHECK_VERSION(3, 0, 0)
  gtk_widget_hide(CONTROL_PANEL(sp));
#else
  gtk_widget_hide_all(CONTROL_PANEL(sp));
#endif
}


bool showing_controls(snd_info *sp)
{
#if ((!HAVE_GTK_WIDGET_GET_VISIBLE) || (!HAVE_GTK_WIDGET_GET_MAPPED))
  return((GTK_WIDGET_MAPPED(CONTROL_PANEL(sp))) && 
	 (widget_is_active(CONTROL_PANEL(sp))));
#else
  return((gtk_widget_get_mapped(CONTROL_PANEL(sp))) && 
	 (gtk_widget_get_visible(CONTROL_PANEL(sp))));
#endif
}


void show_all_controls(void)
{
  int i;
  currently_showing_controls = true;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	show_controls(sp);
    }
}


void hide_all_controls(void)
{
  int i;
  currently_showing_controls = false;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	hide_controls(sp);
    }
}


int control_panel_height(snd_info *sp)
{
  return(widget_height(CONTROL_PANEL(sp)));
}



/* -------- PROGRESS REPORT -------- */

void progress_report(chan_info *cp, mus_float_t pct)
{
  snd_info *sp;
  sp = cp->sound;

  if ((!sp) || 
      (sp->inuse != SOUND_NORMAL) ||
      (cp->chan != 0) ||
      (sp->channel_style == CHANNELS_SUPERIMPOSED))
    return;

  {
    cp->progress_pct = pct;
    show_happy_face(sound_pix_wn(cp), pct);
  }

  check_for_event();
}


void finish_progress_report(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;

  if ((!sp) || 
      (sp->inuse != SOUND_NORMAL) ||
      (cp->chan != 0) ||
      (sp->channel_style == CHANNELS_SUPERIMPOSED))
    return;

  {
    cp->progress_pct = -1.0;
    hide_happy_face(sound_pix_wn(cp));
  }
  hide_stop_sign(sp);
}


void start_progress_report(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;

  if ((!sp) || 
      (sp->inuse != SOUND_NORMAL) ||
      (cp->chan != 0) ||
      (sp->channel_style == CHANNELS_SUPERIMPOSED))
    return;

  {
    cp->progress_pct = 0.0;
    show_happy_face(sound_pix_wn(cp), 0.0);
  }

  show_stop_sign(sp);
}


void reflect_sound_selection(snd_info *sp)
{
  snd_info *osp = NULL;
  if (ss->selected_sound != NO_SELECTION) osp = ss->sounds[ss->selected_sound];
  if ((osp) && (sp != osp) && (osp->inuse == SOUND_NORMAL)) 
    widget_modify_fg(NAME_BUTTON(osp), GTK_STATE_NORMAL, ss->black);
  if ((NAME_BUTTON(sp)) && (sp->selected_channel != NO_SELECTION))
    widget_modify_fg(NAME_BUTTON(sp), GTK_STATE_NORMAL, ss->red);
  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
    {
      int page, current_page;
      current_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(sound_pane_box(ss)));
      page = sp->page;
      if ((page != current_page) && (current_page >= 0))
	{
	  ss->selected_sound = sp->index; /* break infinite recursion here */
	  gtk_notebook_set_current_page(GTK_NOTEBOOK(sound_pane_box(ss)), page);
	}
    }
}


/* -------- controls dialog -------- */

static GtkWidget *controls_dialog = NULL;

enum {EXPAND_HOP, EXPAND_LENGTH, EXPAND_RAMP, EXPAND_JITTER, CONTRAST_AMP, REVERB_LOWPASS, REVERB_FEEDBACK};
static GtkAdjustment *controls[7];

static void reset_all_sliders(void)
{
  expand_control_set_hop(DEFAULT_EXPAND_CONTROL_HOP);
  expand_control_set_length(DEFAULT_EXPAND_CONTROL_LENGTH);
  expand_control_set_ramp(DEFAULT_EXPAND_CONTROL_RAMP);
  expand_control_set_jitter(DEFAULT_EXPAND_CONTROL_JITTER);
  contrast_control_set_amp(DEFAULT_CONTRAST_CONTROL_AMP);
  reverb_control_set_lowpass(DEFAULT_REVERB_CONTROL_LOWPASS);
  reverb_control_set_feedback(DEFAULT_REVERB_CONTROL_FEEDBACK);

  ADJUSTMENT_SET_VALUE(controls[EXPAND_HOP], expand_control_hop(ss));
  ADJUSTMENT_SET_VALUE(controls[EXPAND_LENGTH], expand_control_length(ss));
  ADJUSTMENT_SET_VALUE(controls[EXPAND_RAMP], expand_control_ramp(ss));
  ADJUSTMENT_SET_VALUE(controls[EXPAND_JITTER], expand_control_jitter(ss));
  ADJUSTMENT_SET_VALUE(controls[CONTRAST_AMP], contrast_control_amp(ss));
  ADJUSTMENT_SET_VALUE(controls[REVERB_LOWPASS], reverb_control_lowpass(ss));
  ADJUSTMENT_SET_VALUE(controls[REVERB_FEEDBACK], reverb_control_feedback(ss));
}

static void controls_reset_callback(GtkWidget *w, gpointer context)
{
  reset_all_sliders();
}

static void controls_help_callback(GtkWidget *w, gpointer context)
{
  snd_help("More controls", 
"This dialog controls all the otherwise hidden control-panel variables.\n\
Expand-hop sets the time in seconds between successive grains.\n\
Expand-length sets the length of each grain.\n\
Expand-ramp sets the ramp-time in the grain envelope.\n\
Expand-jitter sets the grain timing jitter.\n\
Contrast-amp sets the prescaler for contrast-enhancement.\n\
Reverb-lowpass sets the feedback lowpass filter coeficient.\n\
Reverb-feedback sets the scaler on the feedback.",
	   WITHOUT_WORD_WRAP);
}

static void controls_dismiss_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(controls_dialog);
}

static gint delete_controls_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(controls_dialog);
  return(true);
}

static void expand_hop_callback(GtkAdjustment *adj, gpointer context)
{
  expand_control_set_hop(ADJUSTMENT_VALUE(adj));
}

static void expand_length_callback(GtkAdjustment *adj, gpointer context)
{
  expand_control_set_length(ADJUSTMENT_VALUE(adj));
}

static void expand_ramp_callback(GtkAdjustment *adj, gpointer context)
{
  expand_control_set_ramp(ADJUSTMENT_VALUE(adj));
}

static void expand_jitter_callback(GtkAdjustment *adj, gpointer context)
{
  expand_control_set_jitter(ADJUSTMENT_VALUE(adj));
}

static void contrast_amp_callback(GtkAdjustment *adj, gpointer context)
{
  contrast_control_set_amp(ADJUSTMENT_VALUE(adj));
}

static void reverb_lowpass_callback(GtkAdjustment *adj, gpointer context)
{
  reverb_control_set_lowpass(ADJUSTMENT_VALUE(adj));
}

static void reverb_feedback_callback(GtkAdjustment *adj, gpointer context)
{
  reverb_control_set_feedback(ADJUSTMENT_VALUE(adj));
}

static void add_control(GtkWidget *main, const char *name, int loc, 
			mus_float_t init, mus_float_t low, mus_float_t high, 
			void (*callback)(GtkAdjustment *adj, gpointer context))
{
  GtkWidget *scale, *frame;
  GtkAdjustment *scale_adj;

  frame = gtk_frame_new(name);
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
  sg_box_pack_start(GTK_BOX(main), frame, true, true, 10); 
  gtk_widget_show(frame);

  scale_adj = (GtkAdjustment *)gtk_adjustment_new(init, low, high, 0.001, 0.001, .1);
  scale = gtk_hscale_new(GTK_ADJUSTMENT(scale_adj));
  UNSET_CAN_FOCUS(scale);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(scale)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_digits(GTK_SCALE(scale), 3);
  gtk_scale_set_value_pos(GTK_SCALE(scale), GTK_POS_TOP);
  gtk_scale_set_draw_value(GTK_SCALE(scale), true);
  gtk_container_add(GTK_CONTAINER(frame), scale);
  SG_SIGNAL_CONNECT(scale_adj, "value_changed", callback, NULL);
  gtk_widget_show(scale);
  controls[loc] = scale_adj;
}

void make_controls_dialog(void)
{
  if (!controls_dialog)
    {
      GtkWidget* mainbox, *help_button, *dismiss_button, *reset_button;

      controls_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(controls_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      SG_SIGNAL_CONNECT(controls_dialog, "delete_event", delete_controls_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(controls_dialog), "Controls");
      sg_make_resizable(controls_dialog);
      sg_container_set_border_width (GTK_CONTAINER(controls_dialog), 4);
      gtk_widget_realize(controls_dialog);
      gtk_window_resize(GTK_WINDOW(controls_dialog), 400, 500);

      help_button = gtk_dialog_add_button(GTK_DIALOG(controls_dialog), "Help", GTK_RESPONSE_NONE);
      reset_button = gtk_dialog_add_button(GTK_DIALOG(controls_dialog), "Revert", GTK_RESPONSE_NONE);
      dismiss_button = gtk_dialog_add_button(GTK_DIALOG(controls_dialog), "Go away", GTK_RESPONSE_NONE);

      gtk_widget_set_name(help_button, "dialog_button");
      gtk_widget_set_name(dismiss_button, "dialog_button");
      gtk_widget_set_name(reset_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(dismiss_button);
  add_highlight_button_style(reset_button);
  add_highlight_button_style(help_button);
#endif

      SG_SIGNAL_CONNECT(dismiss_button, "clicked", controls_dismiss_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", controls_help_callback, NULL);
      SG_SIGNAL_CONNECT(reset_button, "clicked", controls_reset_callback, NULL);

      gtk_widget_show(dismiss_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(help_button);

      mainbox = DIALOG_CONTENT_AREA(GTK_DIALOG(controls_dialog));
      add_control(mainbox, "expand hop", EXPAND_HOP, expand_control_hop(ss), .001, .3, expand_hop_callback);
      add_control(mainbox, "expand length", EXPAND_LENGTH, expand_control_length(ss), .01, .5, expand_length_callback);
      add_control(mainbox, "expand ramp", EXPAND_RAMP, expand_control_ramp(ss), .01, .5, expand_ramp_callback);
      add_control(mainbox, "expand jitter", EXPAND_JITTER, expand_control_jitter(ss), 0.0, 2.0, expand_jitter_callback);
      add_control(mainbox, "contrast amp", CONTRAST_AMP, contrast_control_amp(ss), 0.0, 2.0, contrast_amp_callback);
      add_control(mainbox, "reverb lowpass", REVERB_LOWPASS, reverb_control_lowpass(ss), 0.0, 1.0, reverb_lowpass_callback);
      add_control(mainbox, "reverb feedback", REVERB_FEEDBACK, reverb_control_feedback(ss), 0.0, 1.25, reverb_feedback_callback);

      set_dialog_widget(CONTROLS_DIALOG, controls_dialog);      
    }
  else raise_dialog(controls_dialog);
  gtk_widget_show(controls_dialog);
}



/* ---------------------------------------- */

static Xen g_sound_widgets(Xen snd)
{
  #define H_sound_widgets "(" S_sound_widgets " :optional snd): a list of \
widgets: ((0)pane (1)name (2)control-panel (3)status area (4)play-button (5)filter-env (6)unite-button (7)name-label (8)name-icon) (9)\
pane-box (10)name-form"
  snd_info *sp;

  Snd_assert_sound(S_sound_widgets, snd, 1);

  sp = get_sp(snd);
  if (!sp)
    return(snd_no_such_sound_error(S_sound_widgets, snd));
  if (!has_widgets(sp))
    return(Xen_empty_list);

  return(Xen_cons(Xen_wrap_widget(SND_PANE(sp)),
	  Xen_cons(Xen_wrap_widget(NAME_BUTTON(sp)),
           Xen_cons(Xen_wrap_widget(CONTROL_PANEL(sp)),
	    Xen_cons(Xen_wrap_widget(STATUS_AREA(sp)),
#if WITH_AUDIO
	     Xen_cons(Xen_wrap_widget(PLAY_BUTTON(sp)),
#else
             Xen_cons(Xen_false,
#endif
	      Xen_cons(Xen_wrap_widget(FILTER_ENV(sp)), /* this is the (filter) drawingarea widget */
	       Xen_cons(Xen_wrap_widget(UNITE_BUTTON(sp)),
		Xen_cons(Xen_false,
	         Xen_cons(Xen_wrap_widget(NAME_PIX(sp)),
		  Xen_cons(Xen_wrap_widget(PANE_BOX(sp)),
		   Xen_cons(Xen_wrap_widget(NAME_HBOX(sp)),
	            Xen_empty_list))))))))))));
}


Xen_wrap_1_optional_arg(g_sound_widgets_w, g_sound_widgets)


/* -------------------------------------------------------------------------------- */


static Xen mouse_enter_text_hook;
static Xen mouse_leave_text_hook;

static gboolean mouse_enter_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (with_pointer_focus(ss))
    goto_window(w);
  widget_modify_base(w, GTK_STATE_NORMAL, ss->white);
  if (Xen_hook_has_list(mouse_enter_text_hook))
    run_hook(mouse_enter_text_hook,
	     Xen_list_1(Xen_wrap_widget(w)),
	     S_mouse_enter_text_hook);
  cursor_set_blinks(w, true);
  return(false);
}


static gboolean mouse_leave_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  widget_modify_base(w, GTK_STATE_NORMAL, ss->basic_color);
  if (Xen_hook_has_list(mouse_leave_text_hook))
    run_hook(mouse_leave_text_hook,
	     Xen_list_1(Xen_wrap_widget(w)),
	     S_mouse_leave_text_hook);
  cursor_set_blinks(w, false);
  return(false);
}


void connect_mouse_to_text(GtkWidget *text)
{
  SG_SIGNAL_CONNECT(text, "enter_notify_event", mouse_enter_text_callback, NULL);
  SG_SIGNAL_CONNECT(text, "leave_notify_event", mouse_leave_text_callback, NULL);
}


static bool bindings_ok = false;

GtkWidget *snd_entry_new(GtkWidget *container, GtkWidget *prev, snd_entry_bg_t with_white_background)
{
  GtkWidget *text;
  GtkSettings *settings;

  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), true);
  add_entry_style(text);

  settings = gtk_widget_get_settings(text);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);

#if HAVE_GTK_GRID_NEW
  if (prev)
    {
      g_object_set(text, "margin", 2, NULL);
      gtk_widget_set_halign(text, GTK_ALIGN_FILL);
      gtk_widget_set_hexpand(text, true);
      gtk_grid_attach_next_to(GTK_GRID(container), text, prev, GTK_POS_RIGHT, 1, 1);
    }
  else sg_box_pack_start(GTK_BOX(container), text, true, true, 2);
#else
  sg_box_pack_start(GTK_BOX(container), text, true, true, 2);
#endif

  if (!bindings_ok)
    {
      bindings_ok = true;
      glistener_key_bindings(ss->listener, GTK_ENTRY_GET_CLASS(GTK_ENTRY(text)));
    }
  gtk_widget_show(text);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  if (with_white_background == WITH_WHITE_BACKGROUND) 
    {
      widget_modify_bg(text, GTK_STATE_NORMAL, ss->white);
      widget_modify_base(text, GTK_STATE_SELECTED, ss->white); 
    }
#endif
  connect_mouse_to_text(text);
  return(text);
}


GtkWidget *snd_entry_new_with_size(GtkWidget *container, int size)
{
  GtkWidget *text;
  GtkSettings *settings;

  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), true);
  gtk_entry_set_width_chars(GTK_ENTRY(text), size);
  add_entry_style(text);

  settings = gtk_widget_get_settings(text);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);

  sg_box_pack_start(GTK_BOX(container), text, false, false, 4);

  if (!bindings_ok)
    {
      bindings_ok = true;
      glistener_key_bindings(ss->listener, GTK_ENTRY_GET_CLASS(GTK_ENTRY(text)));
    }
  gtk_widget_show(text);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  widget_modify_bg(text, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(text, GTK_STATE_SELECTED, ss->white); 
#endif
  connect_mouse_to_text(text);
  return(text);
}




void g_init_gxsnd(void) 
{
  Xen_add_to_hook_list(ss->snd_open_file_hook, reflect_file_close_in_sync_w, "sync-open-file-watcher", "sound sync open-file-hook handler");

  Xen_define_typed_procedure(S_sound_widgets, g_sound_widgets_w, 0, 1, 0, H_sound_widgets, 
			     s7_make_signature(s7, 2, s7_make_symbol(s7, "list?"), s7_t(s7)));

#if HAVE_SCHEME
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
(hook-push " S_mouse_enter_text_hook "\n\
  (lambda (w)\n\
    (" S_focus_widget " w)))"
#endif
#if HAVE_RUBY
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
$mouse_enter_text_hook.add_hook!(\"enter\") do |w|\n\
    focus_widget(w)\n\
  end"
#endif
#if HAVE_FORTH
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
" S_mouse_enter_text_hook " lambda: <{ wid }> wid " S_focus_widget " ; add-hook!"
#endif

  #define H_mouse_leave_text_hook S_mouse_leave_text_hook " (widget): called when the mouse leaves a text widget"

  mouse_enter_text_hook = Xen_define_hook(S_mouse_enter_text_hook, "(make-hook 'widget)", 1, H_mouse_enter_text_hook);
  mouse_leave_text_hook = Xen_define_hook(S_mouse_leave_text_hook, "(make-hook 'widget)", 1, H_mouse_leave_text_hook);
}
