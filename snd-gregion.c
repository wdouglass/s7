#include "snd.h"

static void sg_left_justify_button(GtkWidget *button)
{
#if GTK_CHECK_VERSION(3, 0, 0)
  gtk_widget_set_halign(GTK_WIDGET(button), GTK_ALIGN_START);
#else
  gfloat x, y;
  gtk_misc_get_alignment(GTK_MISC(GTK_LABEL(BIN_CHILD(button))), &x, &y);
  gtk_misc_set_alignment(GTK_MISC(GTK_LABEL(BIN_CHILD(button))), 0.05, y);
#endif
}


#if (!GTK_CHECK_VERSION(3, 0, 0))
static void sg_left_justify_label(GtkWidget *label)
{
  /* the label justify function in Gtk refers to the text of the lines after the first! */
  gfloat x, y;
  gtk_misc_get_alignment(GTK_MISC(GTK_LABEL(label)), &x, &y);
  gtk_misc_set_alignment(GTK_MISC(GTK_LABEL(label)), 0.05, y);
}
#endif


/* -------- region browser -------- */

typedef struct {
  GtkWidget *rw, *nm, *pl;
  int pos;
} regrow;

static GtkWidget *region_dialog = NULL, *region_list, *region_grf;
static regrow **region_rows = NULL;
static int region_rows_size = 0;
static snd_info *rsp = NULL;
static int current_region = -1;
static GtkWidget *srate_text, *length_text, *chans_text, *maxamp_text;
static GtkWidget *save_as_button = NULL, *insert_button = NULL, *mix_button = NULL;
static regrow *region_row(int n);


static void set_current_region(int rg)
{
  bool reg_ok = false;
  current_region = rg;
  reflect_region_in_save_as_dialog();
  if (rg >= 0)
    reg_ok = region_ok(region_list_position_to_id(rg));
  if (save_as_button) gtk_widget_set_sensitive(save_as_button, reg_ok);
  if (mix_button) gtk_widget_set_sensitive(mix_button, reg_ok);
  if (insert_button) gtk_widget_set_sensitive(insert_button, reg_ok);
}


void reflect_regions_in_region_browser(void)
{
  if (rsp)
    {
      uint32_t i;
      rsp->active = true;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = CHANNEL_HAS_AXES;
    }
}


void reflect_no_regions_in_region_browser(void)
{
  if (rsp)
    {
      uint32_t i;
      rsp->active = false;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = CHANNEL_INACTIVE;
    }
}


static void region_update_graph(chan_info *cp)
{
  if (current_region == -1) return;
  rsp->nchans = region_chans(region_list_position_to_id(current_region));
  if (rsp->nchans == 0) return;
  update_graph(cp);
  rsp->nchans = 1;
}


void reflect_region_graph_style(void)
{
  if (current_region == -1) return;
  if ((rsp) &&
      (rsp->chans) &&
      (rsp->chans[0]) &&
      (region_dialog_is_active()))
    {
      rsp->chans[0]->time_graph_style = region_graph_style(ss);
      rsp->chans[0]->dot_size = dot_size(ss);
      /* update_graph(rsp->chans[0]); */
      update_region_browser(true);
    }
}


static void unhighlight_region(void)
{
#if (!GTK_CHECK_VERSION(3, 92, 1))
  if (current_region != -1)
    {
      regrow *oldr;
      oldr = region_row(current_region);
      widget_modify_bg(oldr->nm, GTK_STATE_NORMAL, ss->white);
      widget_modify_base(oldr->nm, GTK_STATE_NORMAL, ss->white);
      widget_modify_bg(oldr->rw, GTK_STATE_NORMAL, ss->white);
      widget_modify_base(oldr->rw, GTK_STATE_NORMAL, ss->white);
    }
#endif
}


static void highlight_region(void)
{
#if (!GTK_CHECK_VERSION(3, 92, 1))
  if (current_region != -1)
    {
      regrow *oldr;
      oldr = region_row(current_region);
      widget_modify_bg(oldr->nm, GTK_STATE_NORMAL, ss->light_blue);
      widget_modify_base(oldr->nm, GTK_STATE_NORMAL, ss->light_blue);
      widget_modify_bg(oldr->rw, GTK_STATE_NORMAL, ss->light_blue);
      widget_modify_base(oldr->rw, GTK_STATE_NORMAL, ss->light_blue);
    }
#endif
}


static void make_region_labels(file_info *hdr)
{
  char *str;
  if (!hdr) return;
  str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(str, PRINT_BUFFER_SIZE, "srate: %d", hdr->srate);
#if (!GTK_CHECK_VERSION(3, 0, 0))
  set_label(srate_text, str);
#else
  set_button_label(srate_text, str);
#endif
  snprintf(str, PRINT_BUFFER_SIZE, "chans: %d", hdr->chans);
#if (!GTK_CHECK_VERSION(3, 0, 0))
  set_label(chans_text, str);
#else
  set_button_label(chans_text, str);
#endif
  snprintf(str, PRINT_BUFFER_SIZE, "length: %.3f", ((double)(hdr->samples) / (double)(hdr->chans * hdr->srate)));
#if (!GTK_CHECK_VERSION(3, 0, 0))
  set_label(length_text, str);
#else
  set_button_label(length_text, str);
#endif
  snprintf(str, PRINT_BUFFER_SIZE, "maxamp: %.3f", region_maxamp(region_list_position_to_id(current_region)));
#if (!GTK_CHECK_VERSION(3, 0, 0))
  set_label(maxamp_text, str);
#else
  set_button_label(maxamp_text, str);
#endif
  free(str);
}


int update_region_browser(bool grf_too)
{
  int i, len;
  region_state *rs;

  rs = region_report();
  len = rs->len;

  for (i = 0; i < len; i++) 
    {
      regrow *r;
      r = region_row(i);
      set_button_label(r->nm, rs->name[i]);
#if WITH_AUDIO
      set_toggle_button(r->pl, false, false, (void *)r);
#endif
      gtk_widget_show(r->rw);
    }

  for (i = len; i < max_regions(ss); i++) 
    if (region_rows[i])
      gtk_widget_hide(region_rows[i]->rw);

  free_region_state(rs);
  if (len == 0) return(0);

  gtk_widget_show(region_list);
  if (grf_too)
    {
      chan_info *cp;

      unhighlight_region();
      set_current_region(0);
      highlight_region();
      goto_window(region_rows[0]->nm);

      cp = rsp->chans[0];
      if (cp) 
	{
	  cp->sound = rsp;
	  cp->chan = 0;
	  set_sensitive(channel_f(cp), false);
	  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(0)) > 1));
	  rsp->hdr = fixup_region_data(cp, 0, 0);
	  make_region_labels(rsp->hdr);
	  region_update_graph(cp);
	}
    }
  return(len);
}


static gint region_browser_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(region_dialog);
  return(true);
}


static void region_ok_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(region_dialog);
}


bool region_browser_is_active(void)
{
  return((region_dialog) && 
	 (widget_is_active(region_dialog)));
}


static gboolean region_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
  return(false);
}

#if (!GTK_CHECK_VERSION(3, 96, 0))
static gboolean region_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  region_update_graph((chan_info *)data);
  return(false);
}
#endif

void delete_region_and_update_browser(int pos)
{
  int act;
  unhighlight_region();
  act = remove_region_from_list(pos);
  if (act == INVALID_REGION) return;
  if (region_dialog)
    {
      if (act != NO_REGIONS)
	{
	  set_current_region(0);
	  highlight_region();
	  goto_window(region_rows[0]->nm);
	}
      else set_current_region(-1);
      update_region_browser(true);
    }
}


static void region_insert_callback(GtkWidget *w, gpointer context)
{
  if ((current_region != -1) &&
      (selected_channel()))
    paste_region(region_list_position_to_id(current_region), selected_channel());
}


static void region_mix_callback(GtkWidget *w, gpointer context)
{
  if ((current_region != -1) &&
      (selected_channel()))
    add_region(region_list_position_to_id(current_region), selected_channel());
}


static void region_save_callback(GtkWidget *w, gpointer context)
{
  if (current_region != -1)
    make_region_save_as_dialog(true);
}


static void region_help_callback(GtkWidget *w, gpointer context)
{
  region_dialog_help();
}


static gboolean region_up_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if (cp->chan > 0)
    {
      cp->chan--;
      set_sensitive(channel_f(cp), (cp->chan > 0));
      set_sensitive(channel_w(cp), true);
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
  return(false);
}


static gboolean region_down_arrow_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if ((cp->chan + 1) < region_chans(region_list_position_to_id(current_region)))
    {
      cp->chan++;
      set_sensitive(channel_f(cp), true);
      set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(current_region)) > (cp->chan + 1)));
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
  return(false);
}



static oclock_t mouse_down_time = 0, ev_mouse_down_time = 0;

static gboolean select_event_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ev_mouse_down_time = event_time(ev);
  return(false);
}


static void region_focus_callback(GtkWidget *w, gpointer context)  /* button clicked callback */
{
  chan_info *cp;
  regrow *r = (regrow *)context;

  if (mouse_down_time != 0)
    {
      if ((ev_mouse_down_time - mouse_down_time) < 200)
	{
	  mouse_down_time = ev_mouse_down_time;
	  if (current_region != -1) 
	    region_edit(current_region);
	  return;
	}
    }
  mouse_down_time = ev_mouse_down_time;

  unhighlight_region();
  if (region_list_position_to_id(r->pos) == INVALID_REGION) return; /* needed by auto-tester */
  set_current_region(r->pos);
  cp = rsp->chans[0];
  cp->sound = rsp;
  cp->chan  = 0;
  highlight_region();
  set_sensitive(channel_f(cp), false);
  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(current_region)) > 1));
  rsp->hdr = fixup_region_data(cp, 0, current_region);
  make_region_labels(rsp->hdr);
  region_update_graph(cp);
}


void reflect_play_region_stop(int n)
{
#if WITH_AUDIO
  if (region_rows)
    {
      regrow *rg;
      rg = region_row(region_id_to_list_position(n));
      if (rg) set_toggle_button(rg->pl, false, false, (void *)rg);
    }
#endif
}


static void region_play_callback(GtkWidget *w, gpointer context)
{
#if WITH_AUDIO
  regrow *r = (regrow *)context;
  if (TOGGLE_BUTTON_ACTIVE(r->pl))
    play_region(region_list_position_to_id(r->pos), IN_BACKGROUND);
  else stop_playing_region(region_list_position_to_id(r->pos), PLAY_BUTTON_UNSET);
#endif
}


static Xen reflect_file_in_region_browser(Xen hook_or_reason)
{
  if (region_dialog)
    {
      bool file_on;
      file_on = (bool)(any_selected_sound());
      set_sensitive(mix_button, file_on);
      set_sensitive(insert_button, file_on);
    }
  return(Xen_false);
}

Xen_wrap_1_arg(reflect_file_in_region_browser_w, reflect_file_in_region_browser)

char *regrow_get_label(void *ur)
{
  regrow *r = (regrow *)ur;
  return((char *)gtk_label_get_text(GTK_LABEL(BIN_CHILD(r->nm))));
}


int regrow_get_pos(void *ur)
{
  regrow *r = (regrow *)ur;
  return(r->pos);
}


static Xen mouse_enter_label_hook;
static Xen mouse_leave_label_hook;

static void mouse_enter_or_leave_label(void *r, int type, Xen hook, const char *caller)
{
  if ((r) &&
      (Xen_hook_has_list(hook)))
    {
      char *label;
      label = regrow_get_label(r);
      if (label)
	run_hook(hook,
		 Xen_list_3(C_int_to_Xen_integer(type),
			    C_int_to_Xen_integer(regrow_get_pos(r)),
			    C_string_to_Xen_string(label)),
		 caller);

    }
}


static void mouse_leave_label(void *r, int type)
{
  mouse_enter_or_leave_label(r, type, mouse_leave_label_hook, S_mouse_leave_label_hook);
}


static void mouse_enter_label(void *r, int type)
{
  mouse_enter_or_leave_label(r, type, mouse_enter_label_hook, S_mouse_enter_label_hook);
}


static gboolean regrow_mouse_enter_label(GtkWidget *w, GdkEventCrossing *ev, gpointer gp)
{
  mouse_enter_label((void *)gp, REGION_VIEWER);
  return(false);
}


static gboolean regrow_mouse_leave_label(GtkWidget *w, GdkEventCrossing *ev, gpointer gp)
{
  mouse_leave_label((void *)gp, REGION_VIEWER);
  return(false);
}


static regrow *make_regrow(GtkWidget *ww, GCallback play_callback, GCallback name_callback)
{
  regrow *r;
  r = (regrow *)calloc(1, sizeof(regrow));

  /* assume "ww" is a vbox widget in this case */
  r->rw = gtk_hbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(ww), r->rw, false, false, 0);
  widget_modify_bg(r->rw, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(r->rw, GTK_STATE_NORMAL, ss->white);
  gtk_widget_show(r->rw);

#if WITH_AUDIO
  r->pl = gtk_check_button_new();
  sg_box_pack_start(GTK_BOX(r->rw), r->pl, false, false, 2);
  SG_SIGNAL_CONNECT(r->pl, "toggled", play_callback, r);
  gtk_widget_show(r->pl);
#endif

  r->nm = gtk_button_new_with_label("");
  widget_modify_bg(r->nm, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(r->nm, GTK_STATE_NORMAL, ss->white);
  gtk_button_set_relief(GTK_BUTTON(r->nm), SG_RELIEF_HALF);
  sg_left_justify_button(r->nm);
  sg_box_pack_start(GTK_BOX(r->rw), r->nm, true, true, 2);
  add_white_button_style(r->nm);

  SG_SIGNAL_CONNECT(r->nm, "clicked", name_callback, r);
  SG_SIGNAL_CONNECT(r->nm, "enter_notify_event", regrow_mouse_enter_label, r);
  SG_SIGNAL_CONNECT(r->nm, "leave_notify_event", regrow_mouse_leave_label, r);
  SG_SIGNAL_CONNECT(r->nm, "button_press_event", select_event_callback, NULL);

  set_user_data(G_OBJECT(r->nm), (gpointer)r);
  gtk_widget_show(r->nm);

  return(r);
}


static bool query_callback(GtkTooltip *tooltip, const char *which)
{
  snd_info *sp;

  sp = any_selected_sound();
  if (sp)
    {
      chan_info *cp;
      cp = any_selected_channel(sp);
      if (cp)
	{
	  char *tip;
	  tip = mus_format("%s the selected region at the cursor (at time %.3f) in %s",
			   which,
			   ((double)cursor_sample(cp)) / ((double)(snd_srate(sp))),
			   sp->short_filename);
	  gtk_tooltip_set_text(tooltip, tip);
	  free(tip);
	  return(true);
	}
    }

  if (strcmp(which, "insert") == 0)
    gtk_tooltip_set_text(tooltip, "if there is an active sound, this inserts the selected region into it at the cursor");
  else gtk_tooltip_set_text(tooltip, "if there is an active sound, this mixes the selected region with it starting at the cursor");
  return(true);
}


static gboolean insert_region_tooltip(GtkWidget *w, gint x, gint y, gboolean keyboard_tip, GtkTooltip *tooltip, gpointer data)
{
  return(query_callback(tooltip, "insert"));
}


static gboolean mix_region_tooltip(GtkWidget *w, gint x, gint y, gboolean keyboard_tip, GtkTooltip *tooltip, gpointer data)
{
  return(query_callback(tooltip, "mix"));
}


static void make_region_dialog(void)
{
  int i, id, rows;
  regrow *r;
  chan_info *cp;
  GtkWidget *infobox, *labbox, *dismiss_button, *help_button;
  GtkWidget *sep1, *cww, *toppane, *tophbox, *formw;
#if (!GTK_CHECK_VERSION(3, 92, 1))
  GtkWidget *labels;
#endif
#if WITH_AUDIO
  GtkWidget *plw;
#endif

  region_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(region_dialog), GTK_WINDOW(main_shell(ss)));
#endif
  SG_SIGNAL_CONNECT(region_dialog, "delete_event", region_browser_delete_callback, NULL);
  gtk_window_set_title(GTK_WINDOW(region_dialog), "Regions");
  sg_make_resizable(region_dialog);
  sg_container_set_border_width(GTK_CONTAINER(region_dialog), 10);
  gtk_window_resize(GTK_WINDOW(region_dialog), 400, 500);
  gtk_widget_realize(region_dialog);

  insert_button = gtk_dialog_add_button(GTK_DIALOG(region_dialog), "Insert", GTK_RESPONSE_NONE);
  mix_button = gtk_dialog_add_button(GTK_DIALOG(region_dialog), "Mix", GTK_RESPONSE_NONE);
  save_as_button = gtk_dialog_add_button(GTK_DIALOG(region_dialog), "Save as", GTK_RESPONSE_NONE);
  dismiss_button = gtk_dialog_add_button(GTK_DIALOG(region_dialog), "Go away", GTK_RESPONSE_NONE);
  help_button = gtk_dialog_add_button(GTK_DIALOG(region_dialog), "Help", GTK_RESPONSE_NONE);

  gtk_widget_set_name(help_button, "dialog_button");
  gtk_widget_set_name(dismiss_button, "dialog_button");
  gtk_widget_set_name(insert_button, "dialog_button");
  gtk_widget_set_name(mix_button, "dialog_button");
  gtk_widget_set_name(save_as_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(help_button);
  add_highlight_button_style(dismiss_button);
  add_highlight_button_style(insert_button);
  add_highlight_button_style(save_as_button);
  add_highlight_button_style(mix_button);
#endif

  SG_SIGNAL_CONNECT(insert_button, "clicked", region_insert_callback, NULL);
  SG_SIGNAL_CONNECT(mix_button, "clicked", region_mix_callback, NULL);
  SG_SIGNAL_CONNECT(help_button, "clicked", region_help_callback, NULL);
  SG_SIGNAL_CONNECT(dismiss_button, "clicked", region_ok_callback, NULL);
  SG_SIGNAL_CONNECT(save_as_button, "clicked", region_save_callback, NULL);

  add_tooltip(insert_button,  "insert the selected region");
  add_tooltip(mix_button,     "mix the selected region");
  add_tooltip(save_as_button, "save the selected region to a file");

  g_signal_connect(insert_button, "query-tooltip", G_CALLBACK(insert_region_tooltip), NULL);
  g_signal_connect(mix_button,    "query-tooltip", G_CALLBACK(mix_region_tooltip), NULL);
  
  gtk_widget_show(insert_button);
  gtk_widget_show(mix_button);
  gtk_widget_show(help_button);
  gtk_widget_show(dismiss_button);
  gtk_widget_show(save_as_button);

  region_grf = gtk_vpaned_new();
  add_paned_style(region_grf);
  sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(region_dialog)), region_grf, true, true, 0);
  gtk_widget_show(region_grf);


  toppane = gtk_hbox_new(false, 0);
  gtk_paned_add1(GTK_PANED(region_grf), toppane);
  gtk_widget_show(toppane);

  formw = gtk_vbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(toppane), formw, true, true, 4);
  gtk_widget_show(formw);

  sep1 = gtk_vseparator_new(); /* not hsep -- damned thing insists on drawing a line */
  sg_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
  gtk_widget_show(sep1);

  tophbox = gtk_hbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(formw), tophbox, false, false, 4);
  gtk_widget_show(tophbox);

#if WITH_AUDIO
#if (!GTK_CHECK_VERSION(3, 0, 0))
  plw = gtk_label_new("play"); 
#else
  plw = gtk_button_new_with_label("play"); 
  add_highlight_button_style(plw);
#endif
  sg_box_pack_start(GTK_BOX(tophbox), plw, false, false, 2);
  gtk_widget_show(plw);
#endif

  sep1 = gtk_vseparator_new();
  sg_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
  gtk_widget_show(sep1);


  region_list = gtk_vbox_new(false, 0);

  cww = gtk_scrolled_window_new(NULL, NULL);
  sg_box_pack_start(GTK_BOX(formw), cww, true, true, 0);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cww), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#if HAVE_GTK_HEADER_BAR_NEW
  gtk_container_add(GTK_CONTAINER(cww), region_list);
#else
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(cww), region_list);
#endif

  gtk_widget_show(region_list);
  gtk_widget_show(cww);

  sep1 = gtk_hseparator_new();
  sg_box_pack_end(GTK_BOX(formw), sep1, false, false, 2);
  gtk_widget_show(sep1);


  infobox = gtk_vbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(toppane), infobox, false, false, 2);
  gtk_widget_show(infobox);
  
  region_rows = (regrow **)calloc(max_regions(ss), sizeof(regrow *));
  region_rows_size = max_regions(ss);
  for (i = 0; i < max_regions(ss); i++)
    {
      r = make_regrow(region_list, (void (*)())region_play_callback, (void (*)())region_focus_callback);
      region_rows[i] = r;
      r->pos = i;
    }

  rows = update_region_browser(false);
  if (rows > 10) rows = 10;

  /* in Gtk, apparently, labels are just the text, not the background (i.e. they're transparent) */
  /* we need a button simply to get the background color, then a vbox to put four labels on the button */
  /* but we get a button which flashes whenever the mouse comes near it and has "relief" */
  /* if we turn off the relief, the colors go away */
  /* all I want is an opaque label with a background color */

#if (!GTK_CHECK_VERSION(3, 92, 1))
  labels = gtk_event_box_new();
  sg_box_pack_start(GTK_BOX(infobox), labels, true, true, 2);
  gtk_widget_show(labels);
  widget_modify_bg(labels, GTK_STATE_NORMAL, ss->highlight_color);
  /* SG_SIGNAL_CONNECT(labels, "enter_notify_event", region_labels_mouse_enter, NULL); */
#endif

  labbox = gtk_vbox_new(true, 0);
#if (!GTK_CHECK_VERSION(3, 92, 1))
  gtk_container_add(GTK_CONTAINER(labels), labbox);
#else
  sg_box_pack_start(GTK_BOX(infobox), labbox, true, true, 2);
#endif
  gtk_widget_show(labbox);
  widget_modify_bg(labbox, GTK_STATE_NORMAL, ss->highlight_color);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  srate_text = gtk_label_new("srate:");
  sg_left_justify_label(srate_text);
#else
  srate_text = gtk_button_new_with_label("srate:");
  add_highlight_button_style(srate_text);
  sg_left_justify_button(srate_text);
#endif
  sg_box_pack_start(GTK_BOX(labbox), srate_text, false, false, 2);
  gtk_widget_show(srate_text);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  chans_text = gtk_label_new("chans:");
  sg_left_justify_label(chans_text);
#else
  chans_text = gtk_button_new_with_label("chans:");
  add_highlight_button_style(chans_text);
  sg_left_justify_button(chans_text);
#endif
  sg_box_pack_start(GTK_BOX(labbox), chans_text, false, false, 2);
  gtk_widget_show(chans_text);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  length_text = gtk_label_new("length:");
  sg_left_justify_label(length_text);
#else
  length_text = gtk_button_new_with_label("length:");
  add_highlight_button_style(length_text);
  sg_left_justify_button(length_text);
#endif
  sg_box_pack_start(GTK_BOX(labbox), length_text, false, false, 2);
  gtk_widget_show(length_text);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  maxamp_text = gtk_label_new("maxamp:");
  sg_left_justify_label(maxamp_text);
#else
  maxamp_text = gtk_button_new_with_label("maxamp:");
  add_highlight_button_style(maxamp_text);
  sg_left_justify_button(maxamp_text);
#endif
  sg_box_pack_start(GTK_BOX(labbox), maxamp_text, false, false, 2);
  gtk_widget_show(maxamp_text);

  gtk_widget_show(region_dialog);

  id = region_list_position_to_id(0);
  rsp = make_simple_channel_display(region_srate(id), region_len(id), WITH_ARROWS, region_graph_style(ss), region_grf, WITHOUT_EVENTS);
  rsp->inuse = SOUND_REGION;
  set_current_region(0);
  cp = rsp->chans[0];

  gtk_paned_set_position(GTK_PANED(region_grf), (gint)floor(100 + rows * 14));

  SG_SIGNAL_CONNECT(channel_graph(cp), DRAW_SIGNAL, region_resize_callback, cp);
#if (!GTK_CHECK_VERSION(3, 96, 0))
  SG_SIGNAL_CONNECT(channel_graph(cp), "configure_event", region_expose_callback, cp);
#endif
  SG_SIGNAL_CONNECT(channel_up_arrow(cp), "button_press_event", region_up_arrow_callback, NULL);
  SG_SIGNAL_CONNECT(channel_down_arrow(cp), "button_press_event", region_down_arrow_callback, NULL);

  set_sensitive(channel_f(cp), false);
  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(0)) > 1));

  add_tooltip(channel_f(cp), "move the graph to the previous channel");
  add_tooltip(channel_w(cp), "move the graph to the next channel");

  cp->chan = 0;
  rsp->hdr = fixup_region_data(cp, 0, 0);
  make_region_labels(rsp->hdr);
  highlight_region();
  region_update_graph(cp);

  Xen_add_to_hook_list(ss->snd_open_file_hook, reflect_file_in_region_browser_w, "region-dialog-open-file-watcher", "region dialog open-file-hook handler");

  set_dialog_widget(REGION_DIALOG, region_dialog);
}


void view_region_callback(GtkWidget *w, gpointer context)
{
  /* put up scrollable dialog describing/playing/editing the region list */
  if (!region_dialog)
    make_region_dialog();
  else 
    {
      update_region_browser(true);
      raise_dialog(region_dialog);
      set_current_region(0);
    }
}


bool region_dialog_is_active(void)
{
  return((region_dialog) && 
	 (widget_is_active(region_dialog)));
}


void allocate_region_rows(int n)
{
  if ((region_dialog) && 
      (n > region_rows_size))
    {
      int i;
      region_rows = (regrow **)realloc(region_rows, n * sizeof(regrow *));
      for (i = region_rows_size; i < n; i++) region_rows[i] = NULL;
      region_rows_size = n;
    }
}


static regrow *region_row(int n)
{
  if (n < region_rows_size)
    {
      if (!region_rows[n])
	{
	  regrow *r;
	  r = make_regrow(region_list, (void (*)())region_play_callback, (void (*)())region_focus_callback);
	  region_rows[n] = r;
	  r->pos = n;
	}
      return(region_rows[n]);
    }
  return(NULL);
}


int region_dialog_region(void)
{
  return(region_list_position_to_id(current_region));
}


static Xen g_view_regions_dialog(void) 
{
  #define H_view_regions_dialog "(" S_view_regions_dialog "): start the region dialog"
  if (snd_regions() > 0) 
    view_region_callback(main_pane(ss), NULL); 
  return(Xen_wrap_widget(region_dialog));
}


Xen_wrap_no_args(g_view_regions_dialog_w, g_view_regions_dialog)

void g_init_gxregion(void)
{
  Xen_define_typed_procedure(S_view_regions_dialog, g_view_regions_dialog_w, 0, 0, 0, H_view_regions_dialog, 
			     s7_make_signature(s7, 1, s7_make_symbol(s7, "pair?")));

#if HAVE_SCHEME
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.scm \
to popup file info as follows: \n\
(hook-push " S_mouse_enter_label_hook "\n\
  (lambda (type position name)\n\
    (if (not (= type 2))\n\
        (" S_info_dialog " name (finfo name)))))\n\
See also nb.scm."
#endif
#if HAVE_RUBY
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.rb \
to popup file info as follows: \n\
$mouse_enter_label_hook.add_hook!(\"finfo\") do |type, position, name|\n\
  if type != 2\n\
    " S_info_dialog "(name, finfo(name))\n\
  end\n\
end\n\
See also nb.rb."
#endif
#if HAVE_FORTH
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.fs \
to popup file info as follows: \n\
" S_mouse_enter_label_hook " lambda: <{ type position name }>\n\
  type 2 <> if\n\
    name name finfo info-dialog\n\
  else\n\
    #f\n\
  then\n\
; add-hook!"
#endif

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  mouse_enter_label_hook = Xen_define_hook(S_mouse_enter_label_hook, "(make-hook 'type 'position 'label)", 3, H_mouse_enter_label_hook);
  mouse_leave_label_hook = Xen_define_hook(S_mouse_leave_label_hook, "(make-hook 'type 'position 'label)", 3, H_mouse_leave_label_hook);
}


/* simple view-files replacement */

#include "snd-file.h"

GtkWidget *view_files_dialog = NULL;

static gint vf_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(view_files_dialog);
  return(true);
}

static void vf_ok_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(view_files_dialog);
}

static void vf_help_callback(GtkWidget *w, gpointer context)
{
  view_files_dialog_help();
}


static char **vf_names, **vf_full_names;
static int vf_names_size = 0;
static regrow **vf_rows = NULL;
static snd_info *vf_play_sp = NULL;

static bool view_files_play(int pos, bool play)
{
  if (play)
    {
      if (vf_play_sp)
	{
	  if (vf_play_sp->playing) return(true); /* can't play two of these at once */
	  if (strcmp(vf_play_sp->short_filename, vf_names[pos]) != 0)
	    {
	      completely_free_snd_info(vf_play_sp);
	      vf_play_sp = NULL;
	    }
	}
      if ((!vf_play_sp) && 
	  (vf_full_names[pos]))
	vf_play_sp = make_sound_readable(vf_full_names[pos], false);
      if (vf_play_sp)
	{
	  vf_play_sp->short_filename = vf_names[pos];
	  vf_play_sp->filename = NULL;
	  play_sound(vf_play_sp, 0, NO_END_SPECIFIED);
	}
      else return(true); /* can't find or setup file */
    }
  else
    { /* play toggled off */
      if ((vf_play_sp) && 
	  (vf_play_sp->playing)) 
	{
	  stop_playing_sound(vf_play_sp, PLAY_BUTTON_UNSET);
	}
    }
  return(false);
}

void view_files_unplay(void)
{
  int i;
  for (i = 0; i < vf_names_size; i++)
    if (TOGGLE_BUTTON_ACTIVE(vf_rows[i]->pl))
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(vf_rows[i]->pl), false);
}

static void vf_focus_callback(GtkWidget *w, gpointer context) {}

static void vf_play_callback(GtkWidget *w, gpointer context)
{
  regrow *r = (regrow *)context;
  view_files_play(r->pos, TOGGLE_BUTTON_ACTIVE(r->pl));
}

static int sort_a_to_z(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  return(strcmp(d1->filename, d2->filename));
}

static void vf_sort(void)
{
  sort_info **data;
  int i;

  data = (sort_info **)calloc(vf_names_size, sizeof(sort_info *));
  for (i = 0; i < vf_names_size; i++)
    {
      data[i] = (sort_info *)calloc(1, sizeof(sort_info));
      data[i]->filename = vf_names[i];
      data[i]->full_filename = vf_full_names[i];
    }
  qsort((void *)data, vf_names_size, sizeof(sort_info *), sort_a_to_z);
  for (i = 0; i < vf_names_size; i++)
    {
      vf_names[i] = data[i]->filename;
      vf_full_names[i] = data[i]->full_filename;
      free(data[i]);
    }
  free(data);
}

void view_files_add_directory(widget_t dialog, const char *dirname) 
{
  dir_info *sound_files = NULL;
  if ((dirname) && (dirname[strlen(dirname) - 1] != '/'))
    {
      char *add_slash;
      add_slash = mus_format("%s/", dirname);
      sound_files = find_sound_files_in_dir(add_slash);
      free(add_slash);
    }
  else sound_files = find_sound_files_in_dir(dirname);

  if ((sound_files) && 
      (sound_files->len > 0))
    {
      int i;

      vf_names_size = sound_files->len;
      vf_names = (char **)calloc(vf_names_size, sizeof(char *));
      vf_full_names = (char **)calloc(vf_names_size, sizeof(char *));
      vf_rows = (regrow **)calloc(vf_names_size, sizeof(regrow *));

      for (i = 0; i < sound_files->len; i++) 
	{
	  vf_names[i] = mus_strdup(sound_files->files[i]->filename);
	  vf_full_names[i] = mus_strdup(sound_files->files[i]->full_filename);
	}
      sound_files = free_dir_info(sound_files);
      vf_sort();
    }
}

bool view_files_has_files(void)
{
  return(vf_names_size > 0);
}

void view_files_callback(GtkWidget *w, gpointer info)
{
  if (!view_files_dialog)
    {
      int i;
      regrow *r;
      GtkWidget *dismiss_button, *help_button, *vf_list;
      GtkWidget *sep1, *cww, *tophbox, *formw;
#if WITH_AUDIO
      GtkWidget *plw;
#endif      
      view_files_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(view_files_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      SG_SIGNAL_CONNECT(view_files_dialog, "delete_event", vf_delete_callback, NULL);
      gtk_window_set_title(GTK_WINDOW(view_files_dialog), "Files");
      sg_make_resizable(view_files_dialog);
      sg_container_set_border_width(GTK_CONTAINER(view_files_dialog), 10);
      gtk_window_resize(GTK_WINDOW(view_files_dialog), 300, 500);
      gtk_widget_realize(view_files_dialog);
      
      dismiss_button = gtk_dialog_add_button(GTK_DIALOG(view_files_dialog), "Go away", GTK_RESPONSE_NONE);
      help_button = gtk_dialog_add_button(GTK_DIALOG(view_files_dialog), "Help", GTK_RESPONSE_NONE);
      
      gtk_widget_set_name(help_button, "dialog_button");
      gtk_widget_set_name(dismiss_button, "dialog_button");
      
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(help_button);
      add_highlight_button_style(dismiss_button);
#endif
      
      SG_SIGNAL_CONNECT(help_button, "clicked", vf_help_callback, NULL);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", vf_ok_callback, NULL);
      
      gtk_widget_show(help_button);
      gtk_widget_show(dismiss_button);
      
      formw = gtk_vbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(view_files_dialog)), formw, true, true, 0);
      gtk_widget_show(formw);
      
      sep1 = gtk_vseparator_new(); /* not hsep -- damned thing insists on drawing a line */
      sg_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
      gtk_widget_show(sep1);
      
      tophbox = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(formw), tophbox, false, false, 4);
      gtk_widget_show(tophbox);
      
#if WITH_AUDIO
#if (!GTK_CHECK_VERSION(3, 0, 0))
      plw = gtk_label_new("play"); 
#else
      plw = gtk_button_new_with_label("play"); 
      add_highlight_button_style(plw);
#endif
      sg_box_pack_start(GTK_BOX(tophbox), plw, false, false, 2);
      gtk_widget_show(plw);
#endif
      
      sep1 = gtk_vseparator_new();
      sg_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
      gtk_widget_show(sep1);
      
      vf_list = gtk_vbox_new(false, 0);
      
      cww = gtk_scrolled_window_new(NULL, NULL);
      sg_box_pack_start(GTK_BOX(formw), cww, true, true, 0);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cww), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#if HAVE_GTK_HEADER_BAR_NEW
      gtk_container_add(GTK_CONTAINER(cww), vf_list);
#else
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(cww), vf_list);
#endif
      
      gtk_widget_show(vf_list);
      gtk_widget_show(cww);
      
      for (i = 0; i < vf_names_size; i++)
	{
	  r = make_regrow(vf_list, (void (*)())vf_play_callback, (void (*)())vf_focus_callback);
	  vf_rows[i] = r;
	  r->pos = i;
	  set_button_label(r->nm, vf_names[i]);
	}

      gtk_widget_show(view_files_dialog);
      set_dialog_widget(VIEW_FILES_DIALOG, view_files_dialog);
    }
  else
    {
      raise_dialog(view_files_dialog);
    }

}
