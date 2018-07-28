#include "snd.h"

enum {
    W_main_window,
    W_graph_window,
    W_wf_buttons,
      W_f, W_w,
    W_zy, W_sy,
    W_bottom_scrollers,
      W_sx, W_zx,
    W_graph,
    W_gzy, W_gsy,
#if (!GTK_CHECK_VERSION(3, 92, 1))
    W_up_ev, W_down_ev,
#endif
    CP_NUM_WIDGETS
};

enum {W_zy_adj, W_zx_adj, W_sy_adj, W_sx_adj, W_gzy_adj, W_gsy_adj, NUM_ADJS};


GtkWidget *channel_graph(chan_info *cp)      {return(cp->widgets[W_graph]);}
static GtkWidget *channel_sx(chan_info *cp)  {return(cp->widgets[W_sx]);}
static GtkWidget *channel_sy(chan_info *cp)  {return(cp->widgets[W_sy]);}
static GtkWidget *channel_zx(chan_info *cp)  {return(cp->widgets[W_zx]);}
static GtkWidget *channel_zy(chan_info *cp)  {return(cp->widgets[W_zy]);}
static GtkWidget *channel_gsy(chan_info *cp) {return(cp->widgets[W_gsy]);}
static GtkWidget *channel_gzy(chan_info *cp) {return(cp->widgets[W_gzy]);}
GtkWidget *channel_w(chan_info *cp)          {return(cp->widgets[W_w]);}
GtkWidget *channel_f(chan_info *cp)          {return(cp->widgets[W_f]);}
#if (GTK_CHECK_VERSION(3, 92, 1))
GtkWidget *channel_up_arrow(chan_info *cp)   {return(cp->widgets[W_w]);}
GtkWidget *channel_down_arrow(chan_info *cp) {return(cp->widgets[W_f]);}
#else
GtkWidget *channel_up_arrow(chan_info *cp)   {return(cp->widgets[W_up_ev]);}
GtkWidget *channel_down_arrow(chan_info *cp) {return(cp->widgets[W_down_ev]);}
#endif

#define EDIT_HISTORY_LIST(Cp) (Cp)->edhist_list
#if GTK_CHECK_VERSION(3, 0, 0)
  #define EDIT_HISTORY_CLOSED 2
#else
  #define EDIT_HISTORY_CLOSED 1
#endif
/* in gtk2, 0 is a no-op here, leaving the edit history pane open, 
 * but in gtk 3 if less than about 30, we get 
 * "Gtk-CRITICAL **: gtk_paint_slider: assertion `width >= 0' failed"
 *   (which means the damned thing has to be open always)
 * or if we hide the scrolled text widget, the pane separator goes away!
 * So, in gtk3 the scrolled window does not display a horizontal scrollbar
 */

static GtkWidget *channel_main_pane(chan_info *cp) {return(cp->widgets[W_main_window]);}
static GtkAdjustment *gsy_adj(chan_info *cp)           {return(cp->adjs[W_gsy_adj]);}
static GtkAdjustment *gzy_adj(chan_info *cp)           {return(cp->adjs[W_gzy_adj]);}
static GtkAdjustment *sy_adj(chan_info *cp)            {return(cp->adjs[W_sy_adj]);}
static GtkAdjustment *sx_adj(chan_info *cp)            {return(cp->adjs[W_sx_adj]);}
static GtkAdjustment *zy_adj(chan_info *cp)            {return(cp->adjs[W_zy_adj]);}
static GtkAdjustment *zx_adj(chan_info *cp)            {return(cp->adjs[W_zx_adj]);}


static mus_float_t sqr(mus_float_t a) {return(a * a);}

static mus_float_t cube(mus_float_t a) {return(a * a * a);}


bool channel_graph_is_visible(chan_info *cp)
{
  return((cp) &&
	 (cp->widgets) &&
	 (channel_graph(cp)) &&
	 (widget_is_active(channel_graph(cp))) &&
	 (cp->sound) &&
	 /* here we may have a sound wrapper for variable display in which case the sound widgets are not implemented */
	 (((cp->sound->inuse == SOUND_WRAPPER) || (cp->sound->inuse == SOUND_REGION)) ||
	  ((cp->sound->inuse == SOUND_NORMAL) &&
	   /* other choice: SOUND_IDLE -> no display */
	   (w_snd_pane(cp->sound)) &&
	   (widget_is_active(w_snd_pane(cp->sound))))));
}


void channel_open_pane(chan_info *cp)
{
  gtk_widget_show(channel_main_pane(cp));
}


static void sy_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sy = value - ap->zy;
  if (ap->sy < 0.0) ap->sy = 0.0;
  apply_y_axis_change(cp);
}


static void sx_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx = value;
  apply_x_axis_change(cp);
}


static void zy_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  mus_float_t old_zy;
  ap = cp->axis;
  if (value < .01) value = .01;
  old_zy = ap->zy;
  ap->zy = sqr(value);
  if (ap->zy < 1e-5) ap->zy = 1e-5;
  ap->sy += (.5 * (old_zy - ap->zy)); /* try to keep wave centered */
  if (ap->sy < 0) ap->sy = 0;
  apply_y_axis_change(cp);
  resize_sy(cp);
}


#define X_RANGE_CHANGEOVER 20.0

static void zx_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  double old_zx = 0.0;
  ap = cp->axis;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  if (value < .01) value = .01;
  old_zx = ap->zx;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    ap->zx = sqr(value);
  else ap->zx = cube(value);
  if (fabs(old_zx - ap->zx) > .00001) /* click on zoom is moving the window */
    {
      /* if cursor visible, focus on that, else selection, else mark, else left side */
      focus_x_axis_change(cp, zoom_focus_style(ss));

      /* focus_x_axis_change has already displayed the new graph, but in gtk the scrollbar setting
       *   in resize_sx will try to display everything again. So try to squelch it...
       */
      if (!(cp->squelch_update))
	{
	  cp->squelch_update = true;
	  resize_sx(cp);
	  cp->squelch_update = false;
	  clear_status_area(cp->sound); /* erase the "(update squelched)" message */
	}
      else resize_sx(cp);
    }
}


static void set_scrollbar(GtkAdjustment *adj, mus_float_t position, mus_float_t range) /* position and range 0 to 1.0 */
{
  ADJUSTMENT_SET_PAGE_SIZE(adj, range);
  ADJUSTMENT_SET_VALUE(adj, position);
}


/* restore_axes_data (snd-file.c) assumes change_gzy also fixes gsy */

void change_gzy(mus_float_t val, chan_info *cp)
{
  /* from snd_update */
  ADJUSTMENT_SET_PAGE_SIZE(gsy_adj(cp), 1.0 - val); 
  ADJUSTMENT_SET_VALUE(gsy_adj(cp), cp->gsy);
  ADJUSTMENT_SET_VALUE(gzy_adj(cp), val);
}


mus_float_t gsy_value(chan_info *cp)
{
  return(1.0 - (cp->gsy + ADJUSTMENT_PAGE_SIZE(gsy_adj(cp))));
}


mus_float_t gsy_size(chan_info *cp)
{
  return(ADJUSTMENT_PAGE_SIZE(gsy_adj(cp)));
}


static void set_zx(chan_info *cp, axis_info *ap)
{
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(zx_adj(cp), sqrt(ap->zx), .1);  /* assume size is 10% of scrollbar length */
  else set_scrollbar(zx_adj(cp), pow(ap->zx, .333), .1);
}


void set_z_scrollbars(chan_info *cp, axis_info *ap)
{
  set_zx(cp, ap);
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(ap->zy), .1);
}


void initialize_scrollbars(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;

  cp->gzy = 0.0;
  cp->gsy = 0.0;

  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(sx_adj(cp), ap->sx, ap->zx);
  set_scrollbar(sy_adj(cp), 1.0 - ap->sy, ap->zy);
  set_z_scrollbars(cp, ap);
  if ((sp->nchans > 1) && (cp->chan == 0) && (gsy_adj(cp)))
    {
      set_scrollbar(gsy_adj(cp), 0.0, 1.0);
      set_scrollbar(gzy_adj(cp), 0.0, 1.0 / (mus_float_t)(sp->nchans));
    }
}


void resize_sy(chan_info *cp)
{
  /* something changed the y axis view, so the scale scroller needs to reflect that change (in size and position) */
  axis_info *ap;
  ap = cp->axis;
  if (ap->y_ambit != 0.0)
    {
      mus_float_t size;
      size = (ap->y1 - ap->y0) / ap->y_ambit;
      set_scrollbar(sy_adj(cp), 
		    1.0 - ((ap->y0 - ap->ymin) / ap->y_ambit + size), 
		    size);
    }
}


void resize_sy_and_zy(chan_info *cp)
{
  resize_sy(cp);
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(cp->axis->zy), .1);
}


void resize_sx(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit != 0.0)
    set_scrollbar(sx_adj(cp),
		  (ap->x0 - ap->xmin) / ap->x_ambit,
		  (ap->x1 - ap->x0) / ap->x_ambit);
}


void resize_sx_and_zx(chan_info *cp)
{
  resize_sx(cp);
  set_zx(cp, cp->axis);
}


static void sy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  /* see note above -- context may be garbage!! -- this is a huge bug in gtk */
  static bool ignore_sy_valuechanged_callback = false;
  chan_info *cp;

  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_sy_valuechanged_callback))
    {
      ignore_sy_valuechanged_callback = true;
      sy_changed(1.0 - ADJUSTMENT_VALUE(adj), cp);
      ignore_sy_valuechanged_callback = false;
    }
}


static void sx_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  static bool ignore_sx_valuechanged_callback = false;
  chan_info *cp;

  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_sx_valuechanged_callback))
    {
      ignore_sx_valuechanged_callback = true;
      sx_changed(ADJUSTMENT_VALUE(adj), cp);
      ignore_sx_valuechanged_callback = false;
    }
}


static void zy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  static bool ignore_zy_valuechanged_callback = false;
  chan_info *cp;

  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_zy_valuechanged_callback))
    {
      ignore_zy_valuechanged_callback = true;
      zy_changed(1.0 - ADJUSTMENT_VALUE(adj), cp);
      ignore_zy_valuechanged_callback = false;
    }
}

static void zx_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  static bool ignore_zx_valuechanged_callback = false; /*see below */
  
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_zx_valuechanged_callback))
    {
      ignore_zx_valuechanged_callback = true;
      zx_changed(ADJUSTMENT_VALUE(adj), cp);
      ignore_zx_valuechanged_callback = false;
    }

  /* it's easy to get infinite recursion here: 
   *    zx_changed -> focus_x_axis_change -> apply_x_axis_change -> update_xs -> reset_x_display -> resize_sx -> set_scrollbar ->
   *    sx_valuechanged_callback -> apply_x_axis_change -> update_xs -> reset_x_display -> resize_sx_and_zx -> resize_sx
   *    and we're looping!  This loop happens now because the gtk 3 changes (hiding the adjustment struct) mean that
   *    we're using gtk_adjustment_set_value which triggers the valuechanged callback, whereas earlier we were
   *    simply setting the field, and not triggering the callback.  Good Grief!
   *
   * I added the guards, but now the sliders seem glitchy -- certainly not smooth!
   */
}


static void gzy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  cp->gzy = ADJUSTMENT_VALUE(adj);
  if (cp->active == CHANNEL_HAS_AXES)
    {
      ADJUSTMENT_SET_PAGE_SIZE(gsy_adj(cp), 1.0 - ADJUSTMENT_VALUE(adj)); 
      if (cp->gsy > cp->gzy)
	{
	  cp->gsy = ADJUSTMENT_VALUE(adj);
	  ADJUSTMENT_SET_VALUE(gsy_adj(cp), cp->gsy);
	}
#if (!(GTK_CHECK_VERSION(3, 18, 0)))
      else gtk_adjustment_changed(GTK_ADJUSTMENT(gsy_adj(cp)));
#endif
      for_each_sound_chan(cp->sound, update_graph_or_warn);
    }
}


static void gsy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  cp->gsy = ADJUSTMENT_VALUE(adj);
  if (cp->active == CHANNEL_HAS_AXES)
    for_each_sound_chan(cp->sound, update_graph_or_warn);
}


static GdkModifierType last_f_state;

static gboolean f_toggle_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_state((GdkEvent *)ev, &last_f_state);
#else
  last_f_state = ev->state;
#endif
  return(false);
}


static void f_toggle_click_callback(GtkWidget *w, gpointer data)
{
  f_button_callback((chan_info *)data, 
		    TOGGLE_BUTTON_ACTIVE(w), 
		    (last_f_state & snd_ControlMask));
}


static GdkModifierType last_w_state;

static gboolean w_toggle_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_state((GdkEvent *)ev, &last_w_state);
#else
  last_w_state = ev->state;
#endif
  return(false);
}


static void w_toggle_click_callback(GtkWidget *w, gpointer data)
{
  w_button_callback((chan_info *)data, 
		    TOGGLE_BUTTON_ACTIVE(w), 
		    (last_w_state & snd_ControlMask));
}


#define MIN_REGRAPH_X 12
#define MIN_REGRAPH_Y 7

#if (GTK_CHECK_VERSION(3, 89, 0))
static void channel_expose_callback(GtkDrawingArea *w, cairo_t *cr, int width, int height, gpointer data)
{
  chan_info *cp;
  cp = (chan_info *)data;
  if (!cp) return;
  cp->graph_cr = cr;
  cairo_set_line_width(cr, 1.0);
  if ((cp->active >= CHANNEL_HAS_AXES) && (cp->sound))
    { 
      snd_info *sp;
      sp = cp->sound;
      if (sp->channel_style != CHANNELS_SEPARATE)
	for_each_sound_chan(sp, update_graph_or_warn);
      else update_graph_or_warn(cp);
    }
}
#else
static gboolean channel_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  chan_info *cp;
  snd_info *sp;
  cp = (chan_info *)data;
  if ((!cp) || (cp->active < CHANNEL_HAS_AXES) || (!cp->sound)) return(false);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  if ((EVENT_AREA_HEIGHT(ev) < MIN_REGRAPH_Y) || 
      (EVENT_AREA_WIDTH(ev) < MIN_REGRAPH_X)) 
    return(false);
  /* these are 0 in gtk 3 */
#endif

  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    for_each_sound_chan(sp, update_graph_or_warn);
  else update_graph_or_warn(cp);
  return(false);
}
#endif

#if (!GTK_CHECK_VERSION(3, 89, 0))
static gboolean channel_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  channel_resize((chan_info *)data);
  return(false);
}
#endif

static Xen mouse_enter_graph_hook;
static Xen mouse_leave_graph_hook;

static gboolean graph_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  /* how many args does this thing take?  does it return an int?  what does the return value mean? */

  if (with_pointer_focus(ss))
    goto_window(w);

  if (Xen_hook_has_list(mouse_enter_graph_hook))
    {
      int pdata;
      pdata = get_user_int_data(G_OBJECT(w));
      run_hook(mouse_enter_graph_hook,
	       Xen_list_2(C_int_to_Xen_sound(unpack_sound(pdata)),
			  C_int_to_Xen_integer(unpack_channel(pdata))),
	       S_mouse_enter_graph_hook);
    }

  gdk_window_set_cursor(WIDGET_TO_WINDOW(w), ss->graph_cursor);
  return(false);
}


static gboolean graph_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  if (Xen_hook_has_list(mouse_leave_graph_hook))
    {
      int pdata;
      pdata = get_user_int_data(G_OBJECT(w));
      run_hook(mouse_leave_graph_hook,
	       Xen_list_2(C_int_to_Xen_sound(unpack_sound(pdata)),
			  C_int_to_Xen_integer(unpack_channel(pdata))),
	       S_mouse_leave_graph_hook);
    }

  gdk_window_set_cursor(WIDGET_TO_WINDOW(w), ss->arrow_cursor);
  return(false);
}


static void hide_gz_scrollbars(snd_info *sp)
{
  chan_info *cp;
  cp = sp->chans[0];
  if (channel_gsy(cp))
    {
      gtk_widget_hide(channel_gsy(cp));
      gtk_widget_hide(channel_gzy(cp));
    }
}


static void show_gz_scrollbars(snd_info *sp)
{
  chan_info *cp;
  cp = sp->chans[0];
  if (channel_gsy(cp))
    {
      gtk_widget_show(channel_gsy(cp));
      gtk_widget_show(channel_gzy(cp));
    }
}


/* edit history support */

static void history_select_callback(const char *name, int row, void *data)
{
  edit_history_select((chan_info *)data, row);
}


static void remake_edit_history(chan_info *cp)
{
  snd_info *sp;
  int i, eds;
  slist *lst;

  if ((!cp) || (cp->active < CHANNEL_HAS_AXES)) return;
  if (cp->squelch_update) return;
  lst = EDIT_HISTORY_LIST(cp);
  if (!lst) return;

  /* if you try to update something in a closed pane, gtk grinds to a halt */
  if (gtk_paned_get_position(GTK_PANED(cp->widgets[W_main_window])) < 10) return;

  slist_clear(lst);
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      uint32_t k;
      int ed, filelen;
      char *title;
      chan_info *ncp;
      filelen = 16 + strlen(sp->filename);
      title = (char *)calloc(filelen, sizeof(char));
      for (k = 0, ed = 0; k < sp->nchans; k++)
	{
	  ncp = sp->chans[k];
	  if ((ncp) && (ncp->sound))
	    {
	      ncp->edhist_base = ed++;
	      snprintf(title, filelen, "chan %u: %s", k + 1, sp->filename);
	      slist_append(lst, title);
	      eds = ncp->edit_ctr;
	      while ((eds < (ncp->edit_size - 1)) && (ncp->edits[eds + 1])) eds++;
	      for (i = 1; i <= eds; i++, ed++)
		{
		  char *str;
		  str = edit_to_string(ncp, i);
		  slist_append(lst, str);
		  free(str);
		}
	      if (k < sp->nchans - 1)
		{
		  slist_append(lst, "______________________________"); 
		  ed++; 
		} 
	    }
	}
      if (sp->selected_channel == NO_SELECTION)
	slist_select(lst, cp->edhist_base + cp->edit_ctr);
      else slist_select(lst, sp->chans[sp->selected_channel]->edhist_base + sp->chans[sp->selected_channel]->edit_ctr);
      free(title);
    }
  else
    {
      eds = cp->edit_ctr;
      while ((eds < (cp->edit_size - 1)) && (cp->edits[eds + 1])) eds++;
      if (eds >= 0)
	{
	  slist_append(lst, sp->filename);
	  for (i = 1; i <= eds; i++) 
	    {
	      char *str;
	      str = edit_to_string(cp, i);
	      slist_append(lst, str);
	      free(str);
	    }
	}
      slist_select(lst, cp->edit_ctr);
    }
  goto_graph(cp);
}


void reflect_edit_history_change(chan_info *cp)
{
  /* new edit so it is added, and any trailing lines removed */
  snd_info *sp;
  if (cp->squelch_update) return;
  if (cp->in_as_one_edit > 0) return;
  sp = cp->sound;
  if ((cp->chan > 0) && (sp->channel_style != CHANNELS_SEPARATE))
    {
      chan_info *ncp;
      ncp = sp->chans[0];
      if ((ncp) && (ncp->sound))
	remake_edit_history(ncp);
    }
  else remake_edit_history(cp);
}


void reflect_edit_counter_change(chan_info *cp)
{
  /* undo/redo/revert -- change which line is highlighted */
  snd_info *sp;
  slist *lst;

  if (cp->squelch_update) return;
  sp = cp->sound;

  if ((cp->chan > 0) && (sp->channel_style != CHANNELS_SEPARATE))
    {
      chan_info *ncp;
      ncp = sp->chans[0];

      lst = EDIT_HISTORY_LIST(ncp);
      if ((!lst) || (!(lst->items))) return;
      if (gtk_paned_get_position(GTK_PANED(cp->widgets[W_main_window])) < 10) return;

      slist_select(lst, cp->edit_ctr + cp->edhist_base);
    }
  else
    {
      lst = EDIT_HISTORY_LIST(cp);
      if ((!lst) || (!(lst->items))) return;
      if (widget_width(lst->scroller) < 10) return;

      slist_select(lst, cp->edit_ctr);
      slist_moveto(lst, cp->edit_ctr);
      goto_graph(cp);
    }
}


/* for combined cases, the incoming chan_info pointer is always chan[0], 
 * but the actual channel depends on placement if mouse oriented.
 * virtual_selected_channel(cp) (snd-chn.c) retains the current selected channel
 */

static gboolean real_graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym;
  GdkModifierType key_state;

  /* window_get_pointer(ev, &x, &y, &key_state); 
   * the x and y coordinates apparently refer to the mouse, 
   *   and are only used to recognize "lisp-graph" oriented keystrokes.
   *   These are then used only if there is also a key_press_hook.
   */
  keysym = EVENT_KEYVAL(ev);

#if (GTK_CHECK_VERSION(3, 0, 0))
  gdk_event_get_state((GdkEvent *)ev, &key_state);
  key_press_callback(cp, 0, 0, key_state, keysym);
#else
  {
    int x, y;
    key_state = ev->state;
    window_get_pointer(ev, &x, &y, &key_state);
    key_press_callback(cp, x, y, key_state, keysym);
  }
#endif

  g_signal_stop_emission((gpointer)w, g_signal_lookup("key_press_event", G_OBJECT_TYPE((gpointer)w)), 0);
  return(true);
}


gboolean graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym;
  GdkModifierType key_state;

  keysym = EVENT_KEYVAL(ev);

#if GTK_CHECK_VERSION(3, 0, 0)
  gdk_event_get_state((GdkEvent *)ev, &key_state);
  key_press_callback(cp, 0, 0, key_state, keysym);
#else
  {
    int x, y;
    key_state = ev->state;
    window_get_pointer(ev, &x, &y, &key_state);
    key_press_callback(cp, x, y, key_state, keysym);
  }
#endif
  return(true);
}
 

static gboolean graph_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  GdkModifierType state;
  chan_info *cp = (chan_info *)data;
  gdouble x, y;
  ss->graph_is_active = true;
  gtk_widget_grab_focus(w);

#if GTK_CHECK_VERSION(3, 0, 0)
  gdk_event_get_state((GdkEvent *)ev, &state);
  gdk_event_get_coords((GdkEvent *)ev, &x, &y);
#else
  state = ev->state;
  x = ev->x; /* these were gdouble in gtk2 */
  y = ev->y;
#endif

  graph_button_press_callback(cp, (void *)ev, (int)x, (int)y, state, EVENT_BUTTON(ev), event_time(ev));
  return(false);
}


static gboolean graph_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  GdkModifierType state;
  gdouble x, y;
#if GTK_CHECK_VERSION(3, 0, 0)
  gdk_event_get_state((GdkEvent *)ev, &state);
  gdk_event_get_coords((GdkEvent *)ev, &x, &y);
#else
  state = ev->state;
  x = ev->x;
  y = ev->y;
#endif
  graph_button_release_callback((chan_info *)data, (int)x, (int)y, state, EVENT_BUTTON(ev));
  return(false);
}


static gboolean graph_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  gint x, y;
  GdkModifierType state;

  if (EVENT_IS_HINT(ev))
    window_get_pointer(ev, &x, &y, &state); /* gdk_window_get_device_position gint x and y */
  else
    {
#if GTK_CHECK_VERSION(3, 0, 0)
      gdouble ex, ey;
      gdk_event_get_coords((GdkEvent *)ev, &ex, &ey);
      x = (gint)ex;
      y = (gint)ey;
#else
      x = (gint)(ev->x);
      y = (gint)(ev->y);      
#endif
    }

#if GTK_CHECK_VERSION(3, 0, 0)
  gdk_event_get_state((GdkEvent *)ev, &state);
#else
  state = ev->state;
#endif
  if (BUTTON1_PRESSED(state))
    graph_button_motion_callback((chan_info *)data, x, y, event_time(ev));
  else check_cursor_shape((chan_info *)data, x, y);

  return(false);
}


static void channel_drop_watcher(GtkWidget *w, const char *filename, int x, int y, void *data)
{
  drag_and_drop_mix_at_x_y(get_user_int_data(G_OBJECT(w)), filename, x, y);
}


static void channel_drag_watcher(GtkWidget *w, const char *filename, int x, int y, drag_style_t dtype, void *context)
{
  int snd, chn, data;
  snd_info *sp;

  data = get_user_int_data(G_OBJECT(w));
  chn = unpack_channel(data);
  snd = unpack_sound(data);
  sp = ss->sounds[snd];

  if (snd_ok(sp))
    {
      chan_info *cp;
      float seconds;
      switch (dtype)
	{
	case DRAG_ENTER:
	case DRAG_MOTION:
	  cp = sp->chans[chn];
	  if ((sp->nchans > 1) && (sp->channel_style == CHANNELS_COMBINED))
	    cp = which_channel(sp, y);    
	  seconds = (float)(ungrf_x(cp->axis, x));
	  if (seconds < 0.0) seconds = 0.0;
	  if (sp->nchans > 1)
	    status_report(sp, "drop to mix file in chan %d at %.4f", cp->chan + 1, seconds);
	  else status_report(sp, "drop to mix file at %.4f", seconds);
	  break;

	case DRAG_LEAVE:
	  set_status(sp, " ", false); /* not clear_status_area here! => segfault */
	  break;
	}
    }
}


int add_channel_window(snd_info *sp, int channel, int chan_y, int insertion, GtkWidget *main, fw_button_t button_style, bool with_events)
{
  GtkWidget **cw;
  GtkAdjustment **adjs;
  chan_info *cp;
  graphics_context *cax;
  bool make_widgets, need_extra_scrollbars;

  make_widgets = !sp->chans[channel];
  sp->chans[channel] = make_chan_info(sp->chans[channel], channel, sp);
  cp = sp->chans[channel];

  if (!cp->widgets) 
    {
      cw = (GtkWidget **)calloc(CP_NUM_WIDGETS, sizeof(GtkWidget *));
      adjs = (GtkAdjustment **)calloc(NUM_ADJS, sizeof(GtkAdjustment *));
      cp->widgets = cw;
      cp->adjs = adjs;
    }
  else
    {
      cw = cp->widgets;
      adjs = cp->adjs;
    }

  need_extra_scrollbars = ((!main) && (channel == 0));
  if (make_widgets)
    {
      if (!main)
	{
	  cw[W_main_window] = gtk_hpaned_new();
	  add_paned_style(cw[W_main_window]);
	  sg_container_set_border_width(GTK_CONTAINER(cw[W_main_window]), 2);
	  sg_box_pack_start(GTK_BOX(w_snd_pane_box(sp)), cw[W_main_window], true, true, 0);
	  cp->edhist_list = slist_new(cw[W_main_window], NULL, 0, PANED_ADD1);
#if GTK_CHECK_VERSION(3, 0, 0)
	  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cp->edhist_list->scroller), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
#else
	  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cp->edhist_list->scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#endif
	  cp->edhist_list->select_callback = history_select_callback;
	  cp->edhist_list->select_callback_data = (void *)cp;
	}
      else cw[W_main_window] = main;

      cw[W_graph_window] = gtk_table_new(3, 5, false);
      if ((GTK_IS_VPANED(cw[W_main_window])) || (GTK_IS_HPANED(cw[W_main_window])))
	gtk_paned_add2(GTK_PANED(cw[W_main_window]), cw[W_graph_window]);
      else
	{
	  if ((GTK_IS_VBOX(cw[W_main_window])) || (GTK_IS_HBOX(cw[W_main_window])))
	    sg_box_pack_start(GTK_BOX(cw[W_main_window]), cw[W_graph_window], true, true, 4);
	  else gtk_container_add(GTK_CONTAINER(cw[W_main_window]), cw[W_graph_window]);
	}
      gtk_widget_set_size_request(cw[W_graph_window], -1, chan_y);

      cw[W_graph] = gtk_drawing_area_new();
      add_drag_and_drop(cw[W_graph], channel_drop_watcher, channel_drag_watcher, NULL);
      set_user_int_data(G_OBJECT(cw[W_graph]), pack_sound_and_channel(sp->index, cp->chan));
#if (!GTK_CHECK_VERSION(3, 89, 0))
      sg_widget_set_events(cw[W_graph], GDK_ALL_EVENTS_MASK);
      SET_CAN_FOCUS(cw[W_graph]);
#endif
      widget_set_hexpand(cw[W_graph], true);
      widget_set_vexpand(cw[W_graph], true);

      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_graph], 2, 3, 0, 2, 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      gtk_widget_show(cw[W_graph]);
      if (with_events)
	{
#if (GTK_CHECK_VERSION(3, 89, 0))
	  gtk_drawing_area_set_content_width(GTK_DRAWING_AREA(cw[W_graph]), gtk_widget_get_allocated_width(cw[W_graph]));
	  gtk_drawing_area_set_content_height(GTK_DRAWING_AREA(cw[W_graph]), gtk_widget_get_allocated_height(cw[W_graph]));
	  gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(cw[W_graph]), channel_expose_callback, (void *)cp, NULL);
#else
	  SG_SIGNAL_CONNECT(cw[W_graph], DRAW_SIGNAL, channel_expose_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_graph], "configure_event", channel_resize_callback, cp);
#endif
	}
      SG_SIGNAL_CONNECT(cw[W_graph], "button_press_event", graph_button_press, cp);
      SG_SIGNAL_CONNECT(cw[W_graph], "button_release_event", graph_button_release, cp);
      SG_SIGNAL_CONNECT(cw[W_graph], "motion_notify_event", graph_button_motion, cp);
#if (!GTK_CHECK_VERSION(3, 89, 0))
      if (!main)
	{
	  SG_SIGNAL_CONNECT(cw[W_graph], "enter_notify_event", graph_mouse_enter, NULL);
	  SG_SIGNAL_CONNECT(cw[W_graph], "leave_notify_event", graph_mouse_leave, NULL);
	  SG_SIGNAL_CONNECT(cw[W_graph], "key_press_event", real_graph_key_press, cp);
	}
#endif

      cw[W_bottom_scrollers] = gtk_vbox_new(true, 0);
      gtk_box_set_spacing(GTK_BOX(cw[W_bottom_scrollers]), 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_bottom_scrollers], 2, 3, 2, 3, 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);
      gtk_widget_show(cw[W_bottom_scrollers]);

      adjs[W_sx_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.00, 0.001, 0.01, .01);
      cw[W_sx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_sx_adj]));
      sg_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_sx], true, true, 0);
      set_user_data(G_OBJECT(adjs[W_sx_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_sx_adj], "value_changed", sx_valuechanged_callback, cp);
      gtk_widget_show(cw[W_sx]);
      gtk_widget_set_name(cw[W_sx], "sx_slider");

      adjs[W_zx_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);
      cw[W_zx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_zx_adj]));
      sg_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_zx], true, true, 0);
      set_user_data(G_OBJECT(adjs[W_zx_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_zx_adj], "value_changed", zx_valuechanged_callback, cp);
      gtk_widget_set_name(cw[W_zx], "zx_slider");
      gtk_widget_show(cw[W_zx]);


      cw[W_wf_buttons] = gtk_vbox_new(true, 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_wf_buttons], 0, 2, 1, 3, GTK_SHRINK, GTK_SHRINK, 0, 0);
      gtk_widget_show(cw[W_wf_buttons]);
      
      if (button_style == WITH_FW_BUTTONS)
	{
	  cw[W_f] = gtk_check_button_new_with_label("f");
	  add_tooltip(cw[W_f], "show fft");
	  sg_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_f], true, true, 0);
	  gtk_widget_show(cw[W_f]);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_f]), false);
	  SG_SIGNAL_CONNECT(cw[W_f], "button_press_event", f_toggle_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_f], "toggled", f_toggle_click_callback, cp);
  
	  cw[W_w] = gtk_check_button_new_with_label("w");
	  add_tooltip(cw[W_f], "show wave");
	  sg_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_w], true, true, 0);
	  gtk_widget_show(cw[W_w]);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_w]), true);
	  SG_SIGNAL_CONNECT(cw[W_w], "button_press_event", w_toggle_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_w], "toggled", w_toggle_click_callback, cp);
	}
      else
	{
#if GTK_CHECK_VERSION(3, 14, 0)
	  GtkIconTheme *icon_theme; 
	  icon_theme = gtk_icon_theme_get_default();
#endif
#if (!GTK_CHECK_VERSION(3, 92, 1))
	  cw[W_up_ev] = gtk_event_box_new();
	  sg_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_up_ev], true, true, 0);
	  gtk_widget_show(cw[W_up_ev]);
#endif
	  /* gtk_arrow is deprecated -- docs say: use GtkImage with a suitable icon
	   *   but the damned "suitable icon" is specific to some ugly Gnome theme,
	   *   so I'll conjure up some "^" and "v" images I guess -- insert flame here.
	   */
#if GTK_CHECK_VERSION(3, 14, 0)
	  cw[W_f] = gtk_image_new_from_pixbuf(gtk_icon_theme_load_icon(icon_theme, "pan-up-symbolic", 16, (GtkIconLookupFlags)0, NULL)); 
#else
	  cw[W_f] = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_ETCHED_OUT);
#endif
#if (!GTK_CHECK_VERSION(3, 92, 1))
	  gtk_container_add(GTK_CONTAINER(cw[W_up_ev]), GTK_WIDGET(cw[W_f]));
#else
	  sg_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_f], true, true, 0);
#endif
	  gtk_widget_show(cw[W_f]);

#if (!GTK_CHECK_VERSION(3, 92, 1))
	  cw[W_down_ev] = gtk_event_box_new();
	  sg_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_down_ev], true, true, 0);
	  gtk_widget_show(cw[W_down_ev]);
#endif

#if GTK_CHECK_VERSION(3, 14, 0)
	  cw[W_w] = gtk_image_new_from_pixbuf(gtk_icon_theme_load_icon(icon_theme, "pan-down-symbolic", 16, (GtkIconLookupFlags)0, NULL)); 
#else
	  cw[W_w] = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_ETCHED_OUT);
#endif
#if (!GTK_CHECK_VERSION(3, 92, 1))
	  gtk_container_add(GTK_CONTAINER(cw[W_down_ev]), GTK_WIDGET(cw[W_w]));
#else
	  sg_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_w], true, true, 0);
#endif
	  gtk_widget_show(cw[W_w]);
	}

      adjs[W_zy_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);   /* 0 -> 1 (upside down) */
      cw[W_zy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_zy_adj]));
      widget_set_vexpand(cw[W_zy], true);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_zy], 0, 1, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       2, 0);
      set_user_data(G_OBJECT(adjs[W_zy_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_zy_adj], "value_changed", zy_valuechanged_callback, cp);
      gtk_widget_show(cw[W_zy]);
      gtk_widget_set_name(cw[W_zy], "zy_slider");

      adjs[W_sy_adj] = (GtkAdjustment *)gtk_adjustment_new(0.5, 0.0, 1.01, 0.001, 0.01, .01);
      cw[W_sy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_sy_adj]));
      widget_set_vexpand(cw[W_sy], true);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_sy], 1, 2, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       2, 0);
      set_user_data(G_OBJECT(adjs[W_sy_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_sy_adj], "value_changed", sy_valuechanged_callback, cp);
      gtk_widget_show(cw[W_sy]);
      gtk_widget_set_name(cw[W_sy], "sy_slider");

      if (need_extra_scrollbars)
	{
	  adjs[W_gsy_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .01);
	  cw[W_gsy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gsy_adj]));
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gsy], 3, 4, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  set_user_data(G_OBJECT(adjs[W_gsy_adj]), (gpointer)cp);
	  SG_SIGNAL_CONNECT(adjs[W_gsy_adj], "value_changed", gsy_valuechanged_callback, cp);
	  gtk_widget_show(cw[W_gsy]);
	  gtk_widget_set_name(cw[W_gsy], "gsy_slider");

	  adjs[W_gzy_adj] = (GtkAdjustment *)gtk_adjustment_new(1.0, 0.0, 1.00, 0.001, 0.01, .01);
	  cw[W_gzy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gzy_adj]));
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gzy], 4, 5, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  set_user_data(G_OBJECT(adjs[W_gzy_adj]), (gpointer)cp);
	  SG_SIGNAL_CONNECT(adjs[W_gzy_adj], "value_changed", gzy_valuechanged_callback, cp);
	  gtk_widget_show(cw[W_gzy]);
	  gtk_widget_set_name(cw[W_gzy], "gzy_slider");
	  
	  gtk_widget_hide(cw[W_gsy]);
	  gtk_widget_hide(cw[W_gzy]);
	}
      else
	{
	  cw[W_gsy] = NULL;
	  cw[W_gzy] = NULL;
	}

      if ((GTK_IS_VPANED(cw[W_main_window])) || (GTK_IS_HPANED(cw[W_main_window])))
	gtk_paned_set_position(GTK_PANED(cw[W_main_window]), EDIT_HISTORY_CLOSED);  
      gtk_widget_show(cw[W_graph_window]);

    }
  else recolor_graph(cp, false); /* in case selection color left over from previous use */
#if (!GTK_CHECK_VERSION(3, 90, 0))
  if ((sp->channel_style != CHANNELS_COMBINED) || (channel == 0))
    gtk_widget_show_all(cw[W_main_window]);
#endif

  if ((need_extra_scrollbars) && (sp->channel_style != CHANNELS_COMBINED))
    hide_gz_scrollbars(sp); /* default is on in this case */  

  reflect_edit_history_change(cp);

  cax = cp->ax;
  cax->gc = ss->basic_gc;
  /* cax->wn has to wait until update_graph */

  {
    GtkWidget *w; 
    w = channel_to_widget(cp);
    cax->wn = WIDGET_TO_WINDOW(w);
    cax->w = w;
  }
  return(0);
}


static void set_graph_font(chan_info *cp, graphics_context *ax, PangoFontDescription *fnt)
{
  ax->current_font = fnt;
}


void set_peak_numbers_font(chan_info *cp, graphics_context *ax) {set_graph_font(cp, ax, PEAKS_FONT(ss));}

void set_tiny_numbers_font(chan_info *cp, graphics_context *ax) {set_graph_font(cp, ax, TINY_FONT(ss));}

void set_bold_peak_numbers_font(chan_info *cp, graphics_context *ax) {set_graph_font(cp, ax, BOLD_PEAKS_FONT(ss));}


color_t get_foreground_color(graphics_context *ax)
{
  return(ax->gc->fg_color);
}


void set_foreground_color(graphics_context *ax, color_info *color)
{
  gc_set_foreground(ax->gc, color);
}


gc_t *copy_GC(chan_info *cp)
{
  if (cp->selected) return(ss->selected_basic_gc);
  return(ss->basic_gc);
}


gc_t *erase_GC(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((cp->selected) ||
      ((sp) && (sp->channel_style == CHANNELS_SUPERIMPOSED) && (sp->index == ss->selected_sound)))
    return(ss->selected_erase_gc);
  return(ss->erase_gc);
}


void cleanup_cw(chan_info *cp)
{
  if (cp)
    {
      GtkWidget **cw;
      cp->progress_pct = -1.0;

      if (EDIT_HISTORY_LIST(cp)) 
	{
	  slist_clear(EDIT_HISTORY_LIST(cp));
	  gtk_paned_set_position(GTK_PANED(cp->widgets[W_main_window]), EDIT_HISTORY_CLOSED);
	}

      cp->selected = false;
      cw = cp->widgets;

      if (cw)
	{
	  if (cw[W_w])
	    {
	      set_toggle_button(cw[W_w], true, false, (void *)cp);
	      set_toggle_button(cw[W_f], false, false, (void *)cp);
	    }
	  gtk_widget_hide(channel_main_pane(cp));
	}
    }
}


void change_channel_style(snd_info *sp, channel_style_t new_style)
{
  if ((sp) && 
      (sp->nchans > 1))
    {
      channel_style_t old_style;
      chan_info *selected_cp;

      selected_cp = any_selected_channel(sp); /* chan 0 is none is selected */
      old_style = sp->channel_style;
      sp->channel_style = new_style;

      if (new_style != old_style)
	{
	  uint32_t i;
	  int height[1];
	  if ((new_style == CHANNELS_SEPARATE) || (old_style == CHANNELS_SEPARATE))
	    remake_edit_history(sp->chans[0]);

	  if (old_style == CHANNELS_COMBINED)
	    {
	      hide_gz_scrollbars(sp);
	      for (i = 1; i < sp->nchans; i++) 
		channel_set_mix_tags_erased(sp->chans[i]);
	    }
	  else 
	    {
	      if (new_style == CHANNELS_COMBINED)
		{
		  show_gz_scrollbars(sp);
		  for (i = 1; i < sp->nchans; i++) 
		    channel_set_mix_tags_erased(sp->chans[i]);
		}
	    }

	  if (old_style == CHANNELS_SUPERIMPOSED)
	    {
	      syncb(sp, sp->previous_sync);
	      /* set to blue? */
	    }
	  else
	    {
	      if (new_style == CHANNELS_SUPERIMPOSED)
		{
		  sp->previous_sync = sp->sync;
		  if (sp->sync == 0) syncb(sp, 1);

		  apply_y_axis_change(selected_cp);
		  apply_x_axis_change(selected_cp);

		  for (i = 0; i < sp->nchans; i++) 
		    {
		      if ((int)i != selected_cp->chan)
			{
			  chan_info *ncp;
			  ncp = sp->chans[i];
			  cursor_sample(ncp) = cursor_sample(selected_cp);
			  if (selected_cp->graph_transform_on != ncp->graph_transform_on)
			    {
			      ncp->graph_transform_on = selected_cp->graph_transform_on;
			      set_toggle_button(channel_f(ncp), selected_cp->graph_transform_on, false, (void *)ncp);
			    }
			  if (selected_cp->graph_time_on != ncp->graph_time_on)
			    {
			      ncp->graph_time_on = selected_cp->graph_time_on;
			      set_toggle_button(channel_w(ncp), selected_cp->graph_time_on, false, (void *)ncp);
			    }
			}
		    }
		}
	    }

	  height[0] = widget_height(w_snd_pane(sp)) - control_panel_height(sp);
	  if (old_style == CHANNELS_SEPARATE)
	    {
	      axis_info *ap;
	      ap = selected_cp->axis;

	      for (i = 0; i < sp->nchans; i++) 
		{
		  if ((int)i != selected_cp->chan)
		    set_axes(sp->chans[i], ap->x0, ap->x1, ap->y0, ap->y1);

		  if (i > 0)
		    {
		      chan_info *ncp;
		      ncp = sp->chans[i];
		      cleanup_cw(ncp);
		      ncp->ax->w = channel_to_widget(ncp);
		      ncp->ax->wn = WIDGET_TO_WINDOW(ncp->ax->w);
		    }
		}
	      channel_open_pane(sp->chans[0]);
	      set_toggle_button(unite_button(sp), true, false, (void *)sp);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SEPARATE)
		{
		  /* height[0] = total space available */
		  height[0] /= (int)sp->nchans;

		  for_each_sound_chan(sp, channel_open_pane);
		  /* for (i = 0; i < sp->nchans; i++) reset_mix_graph_parent(sp->chans[i]); */

		  for (i = 1; i < sp->nchans; i++)
		    {
#if (!GTK_CHECK_VERSION(3, 90, 0))
		      GtkWidget **cw;
#endif
		      chan_info *cp;
		      cp = sp->chans[i];
		      cp->ax->w = channel_to_widget(cp);
		      cp->ax->wn = WIDGET_TO_WINDOW(cp->ax->w);
#if (!GTK_CHECK_VERSION(3, 90, 0))
		      cw = cp->widgets;
		      gtk_widget_show_all(cw[W_main_window]);
#endif
		    }
		  set_toggle_button(unite_button(sp), false, false, (void *)sp);
		  if (sp->selected_channel > 0) color_selected_channel(sp);
		}
	    }

	  if ((new_style == CHANNELS_COMBINED) && 
	      (sp->selected_channel > 0)) 
	    color_selected_channel(sp);
	}
    }
}



static Xen g_channel_widgets(Xen snd, Xen chn)
{
  #define H_channel_widgets "(" S_channel_widgets " :optional snd chn): a list of widgets: ((0)graph (1)w (2)f (3)sx (4)sy (5)zx (6)zy\
(7)edhist (8)gsy (9)gzy (10)main (11)sx_adj (12)sy_adj (13)zx_adj (14)zy_adj (15)gsy_adj (16)gzy_adj)"

  #define Xen_wrap_adj(Value) ((Value) ? Xen_list_2(C_string_to_Xen_symbol("GtkAdjustment_"), Xen_wrap_C_pointer(Value)) : Xen_false)

  chan_info *cp;
  Snd_assert_channel(S_channel_widgets, snd, chn, 1);
  cp = get_cp(snd, chn, S_channel_widgets);
  if (!cp) return(Xen_false);
  return(Xen_cons(Xen_wrap_widget(channel_graph(cp)),
	  Xen_cons(Xen_wrap_widget(channel_w(cp)),
	   Xen_cons(Xen_wrap_widget(channel_f(cp)),
	    Xen_cons(Xen_wrap_widget(channel_sx(cp)),
	     Xen_cons(Xen_wrap_widget(channel_sy(cp)),
	      Xen_cons(Xen_wrap_widget(channel_zx(cp)),
	       Xen_cons(Xen_wrap_widget(channel_zy(cp)),
		Xen_cons((EDIT_HISTORY_LIST(cp)) ? Xen_wrap_widget(EDIT_HISTORY_LIST(cp)->topics) : Xen_false,
		 Xen_cons(Xen_wrap_widget(channel_gsy(cp)),
		  Xen_cons(Xen_wrap_widget(channel_gzy(cp)),
		   Xen_cons(Xen_wrap_widget(channel_main_pane(cp)),
		    Xen_cons(Xen_wrap_adj(sx_adj(cp)),
		     Xen_cons(Xen_wrap_adj(sy_adj(cp)),
		      Xen_cons(Xen_wrap_adj(zx_adj(cp)),
		       Xen_cons(Xen_wrap_adj(zy_adj(cp)),
		        Xen_cons(Xen_wrap_adj(gsy_adj(cp)),
			 Xen_cons(Xen_wrap_adj(gzy_adj(cp)),
#if (GTK_CHECK_VERSION(3, 89, 0))
#if HAVE_SCHEME
			  Xen_cons(s7_make_c_pointer_with_type(s7, (void *)(cp->graph_cr), s7_make_symbol(s7, "cairo_t_"), Xen_false),
#else
		          Xen_cons(Xen_wrap_widget(cp->graph_cr),
#endif
#else
			  Xen_cons(Xen_false,
#endif
                           Xen_empty_list)))))))))))))))))));
}

static gint timed_eval(gpointer in_code)
{
#if HAVE_EXTENSION_LANGUAGE
  /* #if needed on 64-bit machines */
  Xen lst = (Xen)in_code;
  Xen_call_with_no_args(Xen_cadr(lst), "timed callback func");
  snd_unprotect_at(Xen_integer_to_C_int(Xen_car(lst)));
#endif
  return(0);
}


static Xen g_in(Xen ms, Xen code)
{
  #define H_in "(" S_in " msecs thunk): invoke thunk in msecs milliseconds (named call_in in Ruby)"

#if HAVE_EXTENSION_LANGUAGE
  Xen_check_type(Xen_is_number(ms), ms, 1, S_in, "a number");
  Xen_check_type(Xen_is_procedure(code), code, 2, S_in, "a procedure");
  if (Xen_is_aritable(code, 0))
    {
      int secs;
      Xen_check_type(Xen_is_integer(ms), ms, 3, S_in, "an integer");
      secs = Xen_integer_to_C_int(ms);
      if (secs < 0) 
	Xen_out_of_range_error(S_in, 1, ms, "a positive integer");
      else
	{
	  Xen lst;
	  lst = Xen_list_2(Xen_false, code);
	  Xen_list_set(lst, 0, C_int_to_Xen_integer(snd_protect(lst)));
	  g_timeout_add_full(0, (guint32)secs, timed_eval, (gpointer)lst, NULL);
	}
    }
  else Xen_bad_arity_error(S_in, 2, code, "should take no args");
#endif

  return(ms);
}


void color_unselected_graphs(color_t color)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse != SOUND_WRAPPER))
	{
	  int j;
	  for (j = 0; j < sp->allocated_chans; j++)
	    {
	      chan_info *cp;
	      cp = sp->chans[j];
	      update_graph(cp);
	    }
	}
    }
}


void color_chan_components(color_t color, slider_choice_t which_component)
{
#if (!GTK_CHECK_VERSION(3, 0, 0))
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      int j;
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse != SOUND_WRAPPER))
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    chan_info *cp;
	    cp = sp->chans[j];
	    if (cp)
	      {
		if (which_component == COLOR_POSITION)
		  {
		    /* in gtk3 this sets the slider color when dragged, and it looks bad
		     * in gtk2 it sets the background (the trough, not the slider)
		     */
		    widget_modify_bg(channel_sx(cp), GTK_STATE_ACTIVE, color);
		    widget_modify_bg(channel_sy(cp), GTK_STATE_ACTIVE, color);
		  }
		else
		  {
		    widget_modify_bg(channel_zx(cp), GTK_STATE_ACTIVE, color);
		    widget_modify_bg(channel_zy(cp), GTK_STATE_ACTIVE, color);
		  }
	      }
	  }
    }
#endif
}


static Xen g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor "): current graph cursor shape"
  return(C_int_to_Xen_integer(in_graph_cursor(ss)));
}


static Xen g_set_graph_cursor(Xen curs)
{
  int val;
  Xen_check_type(Xen_is_integer(curs), curs, 1, S_set S_graph_cursor, "an integer");
  val = Xen_integer_to_C_int(curs);
  if ((val >= 0) && ((val & 1) == 0) && (val <= GDK_XTERM)) /* these are even numbers up to about 152 (gdkcursor.h) */
    {
      ss->Graph_Cursor = val;
      ss->graph_cursor = GDK_CURSOR_NEW((GdkCursorType)in_graph_cursor(ss));
      /* the gtk examples ignore g_object_ref|unref in this regard, so I will also */
    }
  else Xen_out_of_range_error(S_set S_graph_cursor, 1, curs, "invalid cursor");
  return(curs);
}


Xen_wrap_2_args(g_in_w, g_in)
Xen_wrap_no_args(g_graph_cursor_w, g_graph_cursor)
Xen_wrap_1_arg(g_set_graph_cursor_w, g_set_graph_cursor)
Xen_wrap_2_optional_args(g_channel_widgets_w, g_channel_widgets)

#if HAVE_SCHEME
static s7_pointer acc_graph_cursor(s7_scheme *sc, s7_pointer args) {return(g_set_graph_cursor(s7_cadr(args)));}
#endif

void g_init_gxchn(void)
{
#if HAVE_SCHEME
  s7_pointer i, t, r, p, fnc;
  r = s7_make_symbol(s7, "real?");
  fnc = s7_make_symbol(s7, "procedure?");
  i = s7_make_symbol(s7, "integer?");
  p = s7_make_symbol(s7, "pair?");
  t = s7_t(s7);
#endif

  Xen_define_typed_procedure(S_in, g_in_w, 2, 0, 0, H_in, s7_make_signature(s7, 3, r, r, fnc));

  Xen_define_typed_dilambda(S_graph_cursor, g_graph_cursor_w, H_graph_cursor,
			    S_set S_graph_cursor, g_set_graph_cursor_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

  Xen_define_typed_procedure(S_channel_widgets, g_channel_widgets_w, 0, 2, 0, H_channel_widgets, s7_make_signature(s7, 3, p, t, t));

#if HAVE_SCHEME
  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn): called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
  (hook-push " S_mouse_enter_graph_hook "\n\
    (lambda (hook)\n\
      (" S_focus_widget " (car (" S_channel_widgets " (hook 'snd) (hook 'chn))))))"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn): called when the mouse \
leaves the drawing area (graph pane) of the given channel."

#endif
#if HAVE_RUBY

  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn): called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
  $mouse_enter_graph_hook.add-hook!(\"focus\") do |snd chn|\n\
    focus_widget(channel_widgets(snd, chn)[0])\n\
    end"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn): called when the mouse \
leaves the drawing area (graph pane) of the given channel."
#endif
#if HAVE_FORTH
  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn): called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
" S_mouse_enter_graph_hook " lambda: <{ snd chn }>\n\
  snd chn " S_channel_widgets " car " S_focus_widget "\n\
; add-hook!"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn): called when the mouse \
leaves the drawing area (graph pane) of the given channel."
#endif

  mouse_enter_graph_hook = Xen_define_hook(S_mouse_enter_graph_hook, "(make-hook 'snd 'chn)", 2, H_mouse_enter_graph_hook);
  mouse_leave_graph_hook = Xen_define_hook(S_mouse_leave_graph_hook, "(make-hook 'snd 'chn)", 2, H_mouse_leave_graph_hook);

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->graph_cursor_symbol, s7_make_function(s7, "[acc-" S_graph_cursor "]", acc_graph_cursor, 2, 0, false, "accessor"));
  s7_symbol_set_documentation(s7, ss->graph_cursor_symbol, "*graph-cursor*: current graph cursor shape");
#endif
}

/* apparently in gtk 3.8.n the sliders are invisible until you try to move them.
 *   I can't find any info about this, and can't find any way to fix it. 
 *   Nothing works.
 */
