#include "snd.h"
#include "clm2xen.h"


static bool cp_has_selection(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return((ed) && (ed->selection_beg != NO_SELECTION));
}


static bool map_over_chans(bool (*func)(chan_info *ncp))
{
  /* non-zero = abort map, skips inactive sounds */
  int i;
  bool val = false;

  for (i = 0; i < ss->max_sounds; i++)
    {
      uint32_t j;
      snd_info *sp;
      chan_info *cp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    if (cp)
	      {
		val = (*func)(cp);
		if (val) return(val);
	      }
      }
  return(val);
}


bool selection_is_active(void)
{
  /* is selection active in any channel */
  return(map_over_chans(cp_has_selection));
}


bool selection_is_active_in_channel(chan_info *cp)
{
  return((cp) && (cp_has_selection(cp)));
}


static bool selection_is_visible(chan_info *cp)
{
  ed_list *ed;
  axis_info *ap;

  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg == NO_SELECTION) return(false);

  ap = cp->axis;
  return((ed) && 
	 (ap->losamp < ed->selection_end) && 
	 (ap->hisamp > ed->selection_beg));
}


bool selection_is_visible_in_channel(chan_info *cp)
{
  return((cp_has_selection(cp)) && 
	 (selection_is_visible(cp)));
}


static mus_long_t mus_long_t_map_over_chans(mus_long_t (*func)(chan_info *, mus_long_t *), mus_long_t *userptr)
{
  int i;
  mus_long_t val = 0;

  for (i = 0; i < ss->max_sounds; i++)
    {
      uint32_t j;
      snd_info *sp;
      chan_info *cp;

      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    {
	      val = (*func)(cp, userptr);
	      if (val) return(val);
	    }
    }
  return(val);
}


static mus_long_t cp_selection_beg(chan_info *cp, mus_long_t *beg) 
{
  ed_list *ed;

  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg != NO_SELECTION)
    {
      beg[0] = ed->selection_beg;
      return(1); /* i.e. stop map_over_chans */
    }

  return(0);
}


mus_long_t selection_beg(chan_info *cp)
{
  mus_long_t beg[1];
  beg[0] = 0;
  if (cp)
    cp_selection_beg(cp, beg);
  else mus_long_t_map_over_chans(cp_selection_beg, beg);
  return(beg[0]);
}


static int xen_selection_counter = 0;

static void cp_set_selection_beg(chan_info *cp, mus_long_t beg)
{
  ed_list *ed;
  mus_long_t len;

  ed = cp->edits[cp->edit_ctr];
  len = current_samples(cp);
  if (beg < len)
    ed->selection_beg = beg;
  else ed->selection_beg = len - 1;
  xen_selection_counter++;

  ed->selection_maxamp = -1.0;
  ed->selection_maxamp_position = -1;
}


mus_long_t selection_end(chan_info *cp) /* never called without selection_member check in advance */
{
  return(cp->edits[cp->edit_ctr]->selection_end);
}


static mus_long_t cp_selection_len(chan_info *cp, mus_long_t *ptr)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    return(ed->selection_end - ed->selection_beg + 1); 
  return(0);
}


mus_long_t selection_len(void)
{
  return(mus_long_t_map_over_chans(cp_selection_len, NULL));
}


static void cp_set_selection_len(chan_info *cp, mus_long_t len)
{
  ed_list *ed;
  mus_long_t cplen;
  ed = cp->edits[cp->edit_ctr];
  cplen = current_samples(cp);
  ed->selection_end = ed->selection_beg + len - 1;
  if (ed->selection_end >= cplen) ed->selection_end = cplen - 1;
  ed->selection_maxamp = -1.0;
  ed->selection_maxamp_position = -1;
}


static void selection_chans_1(chan_info *cp, int *counter)
{
  if (cp_has_selection(cp)) counter[0]++;
}


int selection_chans(void)
{
  int count[1];
  count[0] = 0;
  for_each_normal_chan_with_refint(selection_chans_1, count);
  return(count[0]);
}


static mus_long_t selection_srate_1(chan_info *cp, mus_long_t *ignored)
{
  if (cp_has_selection(cp)) 
    return((mus_long_t)snd_srate(cp->sound));
  return(0);
}


int selection_srate(void)
{
  if (selection_is_active())
    return((int)mus_long_t_map_over_chans(selection_srate_1, NULL));
  return(0);
}


mus_float_t selection_maxamp(chan_info *cp)
{
  mus_float_t val = 0.0;
  if (selection_is_active_in_channel(cp))
    {
      mus_long_t maxpos = 0;
      val = ed_selection_maxamp(cp);
      if (val >= 0.0) return(val);
      val = channel_local_maxamp(cp, 
				 selection_beg(cp), 
				 selection_end(cp) - selection_beg(cp) + 1,
				 cp->edit_ctr,
				 &maxpos);
      set_ed_selection_maxamp(cp, val);
      set_ed_selection_maxamp_position(cp, maxpos);
    }
  return(val);
}


static mus_long_t selection_maxamp_position(chan_info *cp)
{
  selection_maxamp(cp);
  return(ed_selection_maxamp_position(cp));
}


void cp_delete_selection(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    {
      delete_samples(ed->selection_beg, cp_selection_len(cp, NULL), cp, cp->edit_ctr);
      ed = cp->edits[cp->edit_ctr];
      ed->selection_beg = NO_SELECTION;
    }
}


bool delete_selection(cut_selection_regraph_t regraph)
{
  if (selection_is_active())
    {
      for_each_normal_chan(cp_delete_selection);
      if (regraph == UPDATE_DISPLAY) 
	for_each_normal_chan(update_graph);
      enved_reflect_selection(false);

      if (Xen_hook_has_list(ss->effects_hook))
	run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

      return(true);
    }
  return(false);
}


static void cp_deactivate_selection(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed) ed->selection_beg = NO_SELECTION;
}


static sync_info *syncd_chans = NULL;

void deactivate_selection(void)
{
  for_each_normal_chan(cp_deactivate_selection);
  for_each_normal_chan(update_graph);
  enved_reflect_selection(false);

  if (Xen_hook_has_list(ss->effects_hook))
    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

  if (syncd_chans) 
    syncd_chans = free_sync_info(syncd_chans);
}


void reactivate_selection(chan_info *cp, mus_long_t beg, mus_long_t end)
{
  ed_list *ed;
  mus_long_t len;

  ed = cp->edits[cp->edit_ctr];
  len = current_samples(cp) - 1;
  if (beg < 0) beg = 0;
  if (end < 0) end = 0;
  if (beg > len) beg = len;
  if (end > len) end = len;
  if (beg > end) end = beg;

  ed->selection_beg = beg;
  ed->selection_end = end;
  cp->selection_visible = false;
  ed->selection_maxamp = -1.0;
  ed->selection_maxamp_position = -1;
  xen_selection_counter++;
  enved_reflect_selection(true);
  reflect_selection_in_save_as_dialog(true);

  if (Xen_hook_has_list(ss->effects_hook))
    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);
}


void ripple_selection(ed_list *ed, mus_long_t beg, mus_long_t num)
{
  /* beg = insert or delete begin point (snd-edits.c), num = samps inserted (num positive) or deleted (num negative) at beg */
  if (ed->selection_beg != NO_SELECTION)
    {
      if (beg < ed->selection_beg) 
	{
	  ed->selection_beg += num;
	  if (beg >= ed->selection_beg) 
	    ed->selection_beg = NO_SELECTION; /* deletion included some of current selection from outside */
	  else ed->selection_end += num;
	}
      else
	{
	  if (beg < ed->selection_end)
	    {
	      ed->selection_end += num;
	      if (ed->selection_end < beg)
		ed->selection_beg = NO_SELECTION; /* same as above but from end */
	    }
	}
    }
}


static void next_selection_chan(chan_info *cp, void *sidata)
{
  if (cp_has_selection(cp))
    {
      sync_info *si = (sync_info *)sidata;
      int chan;
      chan = si->chans;
      si->chans++;
      si->begs[chan] = selection_beg(cp);
      si->cps[chan] = cp;
    }
}


sync_info *selection_sync(void)
{
  sync_info *si;
  if (!(selection_is_active())) return(NULL);
  si = (sync_info *)calloc(1, sizeof(sync_info));
  si->chans = selection_chans();
  si->cps = (chan_info **)calloc(si->chans, sizeof(chan_info *));
  si->begs = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
  si->chans = 0;
  for_each_normal_chan_with_void(next_selection_chan, (void *)si);
  return(si);
}


static int mix_selection(chan_info *cp, sync_info *si_out, mus_long_t beg, io_error_t *err, int start_chan)
{
  char *tempfile;
  int id = INVALID_MIX_ID;
  io_error_t io_err;

  tempfile = snd_tempnam();
  io_err = save_selection(tempfile, snd_srate(cp->sound), MUS_OUT_SAMPLE_TYPE, MUS_NEXT, NULL, SAVE_ALL_CHANS);
  if (io_err == IO_NO_ERROR)
    {
      char *origin = NULL;
#if HAVE_FORTH
      origin = mus_format("%" PRId64 " snd chn %s", beg, S_mix_selection);
#else
  #if HAVE_SCHEME
      origin = mus_format("-mix-selection- %" PRId64 " %" PRId64 " %" PRId64 " snd chn",
                          beg, selection_beg(cp), selection_len());
  #else
      origin = mus_format("%s" PROC_OPEN "%" PRId64, to_proc_name(S_mix_selection), beg);
  #endif
#endif
      if (si_out->chans > 1)
	remember_temp(tempfile, si_out->chans);

      id = mix_file(beg, selection_len(), si_out->chans, si_out->cps, tempfile, 
		    (si_out->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME, 
		    origin, with_mix_tags(ss), 
		    start_chan);
      free(origin);
    }
  if (tempfile) free(tempfile);
  (*err) = io_err;

  return(id);
}


void add_selection_or_region(int reg, chan_info *cp)
{
  /* in all cases, this has a local sound to report in (kbd, xmenu) */
  if (cp) 
    {
      if (is_editable(cp))
	{
	  io_error_t io_err = IO_NO_ERROR;
	  bool got_selection;
	  got_selection = ((reg == 0) && (selection_is_active()));
	  if (got_selection)
	    {
	      sync_info *si_out;
	      si_out = sync_to_chan(cp);
	      mix_selection(cp, si_out, cursor_sample(cp), &io_err, 0);
	      free_sync_info(si_out);
	    }
	  else io_err = add_region(reg, cp);
	  if (io_err != IO_NO_ERROR)
	    status_report(cp->sound, "can't mix %s: %s",
				 (got_selection) ? "selection" : "region",
				 io_error_name(io_err));
	}
    }
  else snd_error_without_format("no channel to mix into?");
}


static io_error_t insert_selection(chan_info *cp, sync_info *si_out, mus_long_t beg)
{
  char *tempfile = NULL;
  mus_sample_t out_format = MUS_OUT_SAMPLE_TYPE;
  io_error_t io_err = IO_NO_ERROR;

  if (mus_header_writable(MUS_NEXT, cp->sound->hdr->sample_type))
    out_format = cp->sound->hdr->sample_type;
  tempfile = snd_tempnam();

  io_err = save_selection(tempfile, snd_srate(cp->sound), out_format, MUS_NEXT, NULL, SAVE_ALL_CHANS);
  if (io_err == IO_NO_ERROR)
    {
      int i;
      sync_info *si_in;
      si_in = selection_sync();
      if (si_in)
	{
	  if (si_in->chans > 1) 
	    remember_temp(tempfile, si_in->chans);
	  for (i = 0; ((i < si_in->chans) && (i < si_out->chans)); i++)
	    {
	      char *origin;
	      chan_info *cp_in, *cp_out;
	      mus_long_t len;
	      cp_out = si_out->cps[i]; /* currently syncd chan that we might paste to */
	      cp_in = si_in->cps[i];   /* selection chan to paste in (no wrap-around here) */
	      len = cp_selection_len(cp_in, NULL);
#if HAVE_FORTH
	      origin = mus_format("%" PRId64 " %s", beg, S_insert_selection);
#else
	      origin = mus_format("%s" PROC_OPEN "%" PRId64, to_proc_name(S_insert_selection), beg);
#endif
	      if (file_insert_samples(beg, len,
				      tempfile, cp_out, i,
				      (si_in->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				      origin, cp_out->edit_ctr))
		update_graph(cp_out);
	      free(origin);
	    }
	  free_sync_info(si_in);
	}
    }
  if (tempfile) free(tempfile);
  return(io_err);
}


static void insert_selection_or_region(int reg, chan_info *cp)
{
  if (cp) 
    {
      io_error_t err = IO_NO_ERROR;
      bool got_selection;
      got_selection = ((reg == 0) && (selection_is_active()));
      if (got_selection)
	{
	  sync_info *si_out;
	  si_out = sync_to_chan(cp);
	  err = insert_selection(cp, si_out, cursor_sample(cp));
	  free_sync_info(si_out);
	}
      else err = paste_region(reg, cp);
      if (err != IO_NO_ERROR)
	status_report(cp->sound, "can't insert %s: %s",
			     (got_selection) ? "selection" : "region",
			     io_error_name(err));
    }
}


void insert_selection_from_menu(void)
{
  insert_selection_or_region(0, selected_channel());
}


/* we're drawing the selection in one channel, but others may be sync'd to it */

void start_selection_creation(chan_info *cp, mus_long_t samp)
{  
  int i;
  if ((syncd_chans) && 
      (selection_creates_region(ss)))
    /* hmmm -- if keyboard selection in progress, then mouse press? */
    make_region_from_selection();
  deactivate_selection();
  syncd_chans = sync_to_chan(cp);
  syncd_chans->begs[0] = samp;           /* begs not otherwise used here, so treat as pivot point */
  for (i = 0; i < syncd_chans->chans; i++)
    reactivate_selection(syncd_chans->cps[i], samp, samp);
}


void restart_selection_creation(chan_info *cp, bool right)
{
  syncd_chans = sync_to_chan(cp);
  if (right)
    syncd_chans->begs[0] = selection_beg(cp);
  else syncd_chans->begs[0] = selection_end(cp);
}


bool selection_creation_in_progress(void) {return(syncd_chans != NULL);}


void finish_selection_creation(void)
{
  if (syncd_chans)
    {
      if (selection_creates_region(ss)) 
	make_region_from_selection();
      enved_reflect_selection(true);
      reflect_selection_in_save_as_dialog(true);

      if (Xen_hook_has_list(ss->effects_hook))
	run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

      syncd_chans = free_sync_info(syncd_chans);      
    }
}


static void show_selection_triangle(chan_info *cp, graphics_context *ax, int x0, int x1, mus_long_t beg, mus_long_t end);

static void cp_redraw_selection(chan_info *cp)
{
  int x0, x1;
  mus_long_t beg, end;
  axis_info *ap;
  double sp_srate;
  graphics_context *ax;

  ap = cp->axis;
  beg = selection_beg(cp);
  end = selection_end(cp);
  sp_srate = (double)snd_srate(cp->sound);

  if (ap->losamp < beg)
    x0 = grf_x((double)beg / sp_srate, ap);
  else x0 = ap->x_axis_x0;

  if (ap->hisamp > end)
    x1 = grf_x((double)end / sp_srate, ap);
  else x1 = ap->x_axis_x1;

  ax = selection_context(cp);

#if USE_GTK
  if (x0 <= ap->x_axis_x0) 
    x0 += 2;                       /* dont' erase the y axis */
#else
  if (cp->selection_visible)
    {
      fill_rectangle(ax,
		     cp->old_x0,
		     ap->y_axis_y1,
		     cp->old_x1 - cp->old_x0,
		     (int)(ap->y_axis_y0 - ap->y_axis_y1));
      show_selection_triangle(cp, ax, cp->old_x0, cp->old_x1, beg, end);
    }
#endif

  fill_rectangle(ax,
		 x0,
		 ap->y_axis_y1,
		 x1 - x0,
		 (int)(ap->y_axis_y0 - ap->y_axis_y1));
  show_selection_triangle(cp, ax, x0, x1, beg, end);

  cp->old_x0 = x0;
  cp->old_x1 = x1;
  cp->selection_visible = true;
}


static void redraw_selection(void)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if (sp)
	{
	  if (sp->inuse == SOUND_NORMAL)
	    {
	      uint32_t j;
	      for (j = 0; j < sp->nchans; j++)
		{
		  chan_info *cp;
		  cp = sp->chans[j];
		  if ((cp) && (selection_is_visible(cp)))
		    {
#if (!USE_GTK)
		      cp_redraw_selection(cp);
		      if ((cp->graph_transform_on) && 
			  (!(chan_fft_in_progress(cp))) &&
			  (show_selection_transform(ss)))
			calculate_fft(cp);
#else
		      if ((cp->graph_transform_on) && 
			  (!(chan_fft_in_progress(cp))) &&
			  (show_selection_transform(ss)))
			update_graph(cp);
		      else display_channel_time_data(cp);
#endif
		      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
			break;
		    }
		}
	    }
	}
    }
}


void display_selection(chan_info *cp)
{ 
  if (selection_is_visible(cp))
    cp_redraw_selection(cp); /* draw just this chan */
}


void update_possible_selection_in_progress(mus_long_t samp)
{
  if (syncd_chans)
    {
      int i;
      mus_long_t original_beg;

      if (samp < 0) samp = 0;
      original_beg = syncd_chans->begs[0];
      for (i = 0; i < syncd_chans->chans; i++)
	{
	  chan_info *cp;
	  ed_list *ed;
	  mus_long_t new_end;

	  cp = syncd_chans->cps[i];
	  ed = cp->edits[cp->edit_ctr];
	  ed->selection_maxamp = -1.0;
	  ed->selection_maxamp_position = -1;
	  if (samp > current_samples(cp))
	    new_end = current_samples(cp);
	  else new_end = samp;

	  if (new_end < original_beg)
	    {
	      ed->selection_beg = new_end;
	      ed->selection_end = original_beg;
	    }
	  else
	    {
	      ed->selection_beg = original_beg;
	      ed->selection_end = new_end;
	    }
	}
      redraw_selection();
    }
}


int make_region_from_selection(void)
{
  mus_long_t *ends = NULL;
  int i, id = -1;
  bool happy = false;
  sync_info *si;

  if (!(selection_is_active())) return(-1);
  if (max_regions(ss) == 0) return(-1);

  si = selection_sync();
  ends = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
  for (i = 0; i < si->chans; i++) 
    {
      ends[i] = selection_end(si->cps[i]);
      if (ends[i] > si->begs[i]) happy = true;
      /* C-space followed by mouse click causes a bogus selection-creation event */
    }

  if (happy) id = define_region(si, ends);
  free_sync_info(si);
  if (ends) free(ends);
  return(id);
}


int select_all(chan_info *cp)
{
  if (cp) 
    {
      sync_info *si;
      int i;
      deactivate_selection();
      si = sync_to_chan(cp);
      for (i = 0; i < si->chans; i++)
	{
	  if (current_samples(si->cps[i]) > 0)
	    {
	      reactivate_selection(si->cps[i], 0, current_samples(si->cps[i]));
	      update_graph(si->cps[i]);
	    }
	}
      free_sync_info(si);
      if ((selection_is_active()) && (selection_creates_region(ss)))
	return(make_region_from_selection());
    }
  return(-1);
}


/* ---------------- selection mouse motion ---------------- */

static int last_selection_x = 0;
static timeout_result_t watch_selection_button = 0;

void cancel_selection_watch(void)
{
#if (!USE_NO_GUI)
  if (watch_selection_button)
    TIMEOUT_REMOVE(watch_selection_button);
#endif
  watch_selection_button = 0;
}

static void move_selection_1(chan_info *cp, int x);

#if (!USE_NO_GUI)
static TIMEOUT_TYPE watch_selection(TIMEOUT_ARGS)
{
  chan_info *cp = (chan_info *)context;
  if (watch_selection_button)
    {
      move_selection_1(cp, last_selection_x);
      watch_selection_button = CALL_TIMEOUT(watch_selection, 50, cp);
    }
  TIMEOUT_RESULT
}
#endif

static void move_selection_1(chan_info *cp, int x)
{
  axis_info *ap;
  ap = cp->axis;
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) ||
	  ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)))
	return;
      move_axis(cp, x);
      if (!watch_selection_button) 
	watch_selection_button = CALL_TIMEOUT(watch_selection, 50, cp);
    }
  else 
    if (watch_selection_button) 
      cancel_selection_watch();
  redraw_selection();
}


void move_selection(chan_info *cp, int x)
{
  last_selection_x = x; /* called in snd-xchn -- sets last_selection_x */
  move_selection_1(cp, x);
}


static void show_selection_triangle(chan_info *cp, graphics_context *ax, int x0, int x1, mus_long_t beg, mus_long_t end)
{
  int y0;

  y0 = ((axis_info *)(cp->axis))->y_axis_y0;
  if ((cp->axis->losamp <= beg) &&
      (cp->axis->hisamp > beg))
    {
      fill_polygon(ax, 4,
		   x0, y0,
		   x0 + play_arrow_size(ss), y0 + play_arrow_size(ss),
		   x0, y0 + 2 * play_arrow_size(ss),
		   x0, y0);
    }

  if ((cp->axis->losamp < end) &&
      (cp->axis->hisamp >= end))
    {
      fill_polygon(ax, 4,
		   x1, y0,
		   x1 - play_arrow_size(ss), y0 + play_arrow_size(ss),
		   x1, y0 + 2 * play_arrow_size(ss),
		   x1, y0);
    }
}

#define HIT_SLOP 4

bool hit_selection_triangle(chan_info *cp, int x, int y)
{
  axis_info *ap;
  mus_long_t beg;
  int mx;

  beg = selection_beg(cp);
  ap = cp->axis;
  if (beg < ap->losamp) return(false);
  if (beg > ap->hisamp) return(false);

  mx = grf_x((double)beg / (double)snd_srate(cp->sound), ap);

  if (mx > (x + HIT_SLOP)) return(false);                                /* click point is to the left of the triangle */
  if ((mx + play_arrow_size(ss) + HIT_SLOP) < x) return(false);  /* click point is to the right of the triangle */

  y = y - ap->y_axis_y0 - play_arrow_size(ss);
  if (y < 0) y = -y;
  return((mx + play_arrow_size(ss) - y + HIT_SLOP) >= x);
}


bool hit_selection_loop_triangle(chan_info *cp, int x, int y)
{
  axis_info *ap;
  mus_long_t end;
  int mx;

  end = selection_end(cp);
  ap = cp->axis;
  if (end < ap->losamp) return(false);
  if (end > ap->hisamp) return(false);

  mx = grf_x((double)end / (double)snd_srate(cp->sound), ap);

  if ((mx - play_arrow_size(ss) - HIT_SLOP) > x) return(false); 
  if (mx < (x - HIT_SLOP)) return(false);

  y = y - ap->y_axis_y0 - play_arrow_size(ss);
  if (y < 0) y = -y;

  return((mx - play_arrow_size(ss) - y - HIT_SLOP) <= x);
}




/* ---------------------------------------- selection object ---------------------------------------- */

typedef struct {
  int n;
} xen_selection;


#define Xen_to_xen_selection(arg) ((xen_selection *)Xen_object_ref(arg))

static Xen_object_type_t xen_selection_tag;

bool xen_is_selection(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_selection_tag));
}


static void xen_selection_free(xen_selection *v) {if (v) free(v);}

Xen_wrap_free(xen_selection, free_xen_selection, xen_selection_free)


static char *xen_selection_to_string(xen_selection *v)
{
  #define xen_is_selectionRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(xen_is_selectionRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, xen_is_selectionRINT_BUFFER_SIZE, "#<selection %d>", v->n);
  return(buf);
}

Xen_wrap_print(xen_selection, print_xen_selection, xen_selection_to_string)


#if HAVE_FORTH || HAVE_RUBY
static Xen g_xen_selection_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_selection_to_string "selection->string"

  Xen_check_type(xen_is_selection(obj), obj, 1, S_xen_selection_to_string, "a selection");

  vstr = xen_selection_to_string(Xen_to_xen_selection(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static bool xen_selection_equalp(xen_selection *v1, xen_selection *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}


static Xen equalp_xen_selection(Xen obj1, Xen obj2)
{
  if ((!(xen_is_selection(obj1))) || (!(xen_is_selection(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_selection_equalp(Xen_to_xen_selection(obj1), Xen_to_xen_selection(obj2))));
}
#endif


static xen_selection *xen_selection_make(int n)
{
  xen_selection *new_v;
  new_v = (xen_selection *)malloc(sizeof(xen_selection));
  new_v->n = n;
  return(new_v);
}


static Xen g_selection(void)
{
  #define H_selection "(" S_selection" ) returns an object representing the current selection, or " PROC_FALSE " if there is no active selection"
  if (selection_is_active())
    {
      xen_selection *mx;
      mx = xen_selection_make(xen_selection_counter);
      return(Xen_make_object(xen_selection_tag, mx, 0, free_xen_selection));
    }
  return(Xen_false);
}


#if HAVE_SCHEME
static Xen s7_xen_selection_length(s7_scheme *sc, Xen args)
{
  return(g_selection_framples(Xen_undefined, Xen_undefined));
}


static bool s7_xen_selection_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_selection *)obj1)->n == ((xen_selection *)obj2)->n));
}


static Xen s7_xen_selection_copy(s7_scheme *sc, Xen args)
{
  if (selection_is_active())
    {
      snd_info *sp;
      char *name;
      name = snd_tempnam();
      save_selection(name, selection_srate(), MUS_OUT_SAMPLE_TYPE, MUS_NEXT, NULL, SAVE_ALL_CHANS);
      sp = snd_open_file(name, FILE_READ_WRITE);
      free(name);
      return(new_xen_sound(sp->index));
    }
  return(Xen_false);
}


static Xen s7_xen_selection_fill(s7_scheme *sc, Xen args)
{
  sync_info *si;
  mus_float_t valf;
  s7_pointer val;

  val = s7_cadr(args);
  valf = Xen_real_to_C_double(val);
  if (valf == 0.0)
    {
      mus_float_t vals[1] = {0.0};
      scale_by(NULL, vals, 1, true); /* 1 entry in vals array, true = over selection */
      return(Xen_false);
    }

  si = selection_sync();
  if (si)
    {
      int i;
      for (i = 0; i < si->chans; i++)
	{
	  mus_long_t beg, end, len, j;
	  mus_float_t *data;
	  beg = selection_beg(si->cps[i]);
	  end = selection_end(si->cps[i]);
	  len = end - beg + 1;
	  data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
	  for (j = 0; j < len; j++)
	    data[j] = valf;
	  if (change_samples(beg, len, data, si->cps[i], "fill! selection", si->cps[i]->edit_ctr, fabs(valf)))
	    update_graph(si->cps[i]);
	  free(data);
	}
    }
  return(Xen_false);
}
#endif


static void init_xen_selection(void)
{
#if HAVE_SCHEME
  xen_selection_tag = s7_make_c_type(s7, "<selection>");
  s7_c_type_set_print(s7, xen_selection_tag, print_xen_selection);
  s7_c_type_set_free(s7, xen_selection_tag, free_xen_selection);
  s7_c_type_set_equal(s7, xen_selection_tag, s7_xen_selection_equalp);
  s7_c_type_set_length(s7, xen_selection_tag, s7_xen_selection_length);
  s7_c_type_set_copy(s7, xen_selection_tag, s7_xen_selection_copy);
  s7_c_type_set_fill(s7, xen_selection_tag, s7_xen_selection_fill);
#else
#if HAVE_RUBY
  xen_selection_tag = Xen_make_object_type("XenSelection", sizeof(xen_selection));
#else
  xen_selection_tag = Xen_make_object_type("selection", sizeof(xen_selection));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_selection_tag,   print_xen_selection);
  fth_set_object_dump(xen_selection_tag,      g_xen_selection_to_string);
  fth_set_object_equal(xen_selection_tag,     equalp_xen_selection);
  fth_set_object_free(xen_selection_tag,      free_xen_selection);
#endif

#if HAVE_RUBY
  rb_define_method(xen_selection_tag, "to_s",     Xen_procedure_cast print_xen_selection, 0);
  rb_define_method(xen_selection_tag, "eql?",     Xen_procedure_cast equalp_xen_selection, 1);
  rb_define_method(xen_selection_tag, "==",       Xen_procedure_cast equalp_xen_selection, 1);
  rb_define_method(xen_selection_tag, "to_str",   Xen_procedure_cast g_xen_selection_to_string, 0);
#endif
}
/* -------------------------------------------------------------------------------- */



io_error_t save_selection(const char *ofile, int srate, mus_sample_t samp_type, mus_header_t head_type, const char *comment, int chan)
{
  /* type and format have already been checked */
  int ofd;
  io_error_t io_err = IO_NO_ERROR;
  mus_long_t oloc, alloc_len;
  sync_info *si;
  mus_long_t *ends;
  int i, j, k, chans;
  mus_long_t dur;
  snd_fd **sfs;
  snd_info *sp = NULL;
  mus_float_t **data;

  si = selection_sync();
  if ((si) && (si->cps) && (si->cps[0])) sp = si->cps[0]->sound;

  if (head_type == MUS_UNKNOWN_HEADER)
    {
      if ((sp) && (mus_header_writable(sp->hdr->type, MUS_IGNORE_SAMPLE)))
	head_type = sp->hdr->type;
      else head_type = MUS_NEXT;
    }
  if (samp_type == MUS_UNKNOWN_SAMPLE)
    {
      if ((sp) && (mus_header_writable(head_type, sp->hdr->sample_type)))
	samp_type = sp->hdr->sample_type;
      else samp_type = MUS_OUT_SAMPLE_TYPE;
    }
  if (!mus_header_writable(head_type, samp_type))
    {
      head_type = MUS_NEXT;
      samp_type = MUS_OUT_SAMPLE_TYPE;
    }
  if (srate == -1)
    srate = selection_srate();

  dur = selection_len();
  if (chan == SAVE_ALL_CHANS)
    chans = si->chans;
  else chans = 1;

  io_err = snd_write_header(ofile, head_type, srate, chans, chans * dur, samp_type, comment, NULL);
  if (io_err != IO_NO_ERROR)
    {
      free_sync_info(si);
      return(io_err);
    }

  oloc = mus_header_data_location();
  ofd = snd_reopen_write(ofile);

  if (sp)
    {
      int bps;
      mus_long_t num;
      disk_space_t no_space;
      bool copy_ok = false;

      bps = mus_bytes_per_sample(samp_type);
      num = dur * bps * chans;

      no_space = disk_has_space(num, ofile);
      if (no_space != DISK_SPACE_OK)
	{
	  snd_close(ofd, ofile);
	  free_sync_info(si);
	  return(IO_DISK_FULL);
	}

      copy_ok = ((samp_type == sp->hdr->sample_type) && 
		 (chans == (int)sp->nchans) &&
		 (chan == SAVE_ALL_CHANS));
      if (copy_ok)
	for (i = 0; i < chans; i++)
	  if ((sp->chans[i]->edit_ctr != 0) || 
	      (si->cps[i]->sound != sp) ||
	      (si->begs[i] != si->begs[0]))
	    {
	      copy_ok = false;
	      break;
	    }
      if (copy_ok)
	{
	  /* write next header with correct len
	   * seek loc in sp->filename
	   * copy len*data-size bytes
	   * get max from amp envs
	   */
	  int fdi;
	  lseek(ofd, oloc, SEEK_SET);
	  fdi = mus_file_open_read(sp->filename); /* this does not read the header */
	  if (fdi == -1)
	    {
	      snd_close(ofd, ofile);
	      free_sync_info(si);
	      return(IO_CANT_READ_SELECTION_FILE);
	    }
	  /* snd_error("can't read selection's original sound? %s: %s", sp->filename, snd_io_strerror()); */
	  else
	    {
	      mus_long_t iloc;
	      char *buffer;
	  
	      iloc = mus_sound_data_location(sp->filename);
	      lseek(fdi, iloc + chans * bps * si->begs[0], SEEK_SET);
	      buffer = (char *)malloc(MAX_BUFFER_SIZE * sizeof(char));
	      for (j = 0; j < num; j += MAX_BUFFER_SIZE)
		{
		  ssize_t n;
		  mus_long_t bytes;
		  bytes = num - j;
		  if (bytes > MAX_BUFFER_SIZE) bytes = MAX_BUFFER_SIZE;
		  n = read(fdi, buffer, bytes);
		  if (n != 0)
		    n = write(ofd, buffer, bytes);
		  if (n == 0)
		    fprintf(stderr, "IO error while saving selection");
		}
	      free(buffer);
	      snd_close(fdi, sp->filename);
	    }
	  snd_close(ofd, ofile);
	  free_sync_info(si);
#if USE_MOTIF
	  if (!(ss->file_monitor_ok))
	    alert_new_file();
#endif
	  return(IO_NO_ERROR);
	}
    }

  ends = (mus_long_t *)calloc(chans, sizeof(mus_long_t));
  sfs = (snd_fd **)calloc(chans, sizeof(snd_fd *));
  if (chan == SAVE_ALL_CHANS)
    {
      for (i = 0; i < chans; i++) 
	{
	  ends[i] = selection_end(si->cps[i]);
	  sfs[i] = init_sample_read(selection_beg(si->cps[i]), si->cps[i], READ_FORWARD);
	}
    }
  else
    {
      ends[0] = selection_end(si->cps[chan]);
      sfs[0] = init_sample_read(selection_beg(si->cps[chan]), si->cps[chan], READ_FORWARD);
    }

  snd_file_open_descriptors(ofd, ofile, samp_type, oloc, chans, head_type);
  mus_file_set_clipping(ofd, clipping(ss));
  lseek(ofd, oloc, SEEK_SET);
  data = (mus_float_t **)calloc(chans, sizeof(mus_float_t *));

  if (dur > REPORTING_SIZE)
    alloc_len = REPORTING_SIZE;
  else alloc_len = dur;

  for (i = 0; i < chans; i++) 
    data[i] = (mus_float_t *)calloc(alloc_len, sizeof(mus_float_t)); 

  if (alloc_len == dur)
    {
      for (k = 0; k < chans; k++)
	samples_to_vct_with_reader(dur, data[k], sfs[k]);
      mus_file_write(ofd, 0, dur - 1, chans, data);
    }
  else
    {
      mus_long_t ioff;
      ss->stopped_explicitly = false;
      for (k = 0; k < chans; k++)
	sampler_set_safe(sfs[k], ends[k]);

      for (ioff = 0; ioff < dur; ioff += alloc_len)
	{
	  mus_long_t kdur;

	  kdur = dur - ioff;
	  if (kdur > alloc_len) kdur = alloc_len;

	  for (j = 0; j < kdur; j++)
	    {
	      for (k = 0; k < chans; k++)
		{
		  if ((ioff + j) <= ends[k]) 
		    data[k][j] = read_sample(sfs[k]);
		  else data[k][j] = 0.0;
		}
	    }
	  io_err = sndlib_error_to_snd(mus_file_write(ofd, 0, j - 1, chans, data));
	  if (io_err != IO_NO_ERROR)
	    {
	      snd_warning("%s %s: %s",
			  io_error_name(io_err),
			  ofile,
			  snd_io_strerror());
	      break; 
	    }
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = false;
	      snd_warning_without_format("save selection stopped");
	      io_err = IO_INTERRUPTED;
	      break;
	    }
	}
    }

  for (i = 0; i < chans; i++)
    {
      free_snd_fd(sfs[i]);
      free(data[i]);
    }
  free(sfs);
  free(data);
  free_sync_info(si);
  free(ends);

  if (mus_file_close(ofd) != 0)
    return(IO_CANT_CLOSE_FILE);
#if USE_MOTIF
  if (!(ss->file_monitor_ok))
    alert_new_file();
#endif

  return(io_err);
}


static Xen g_delete_selection(void)
{
  #define H_delete_selection "(" S_delete_selection "): delete the currently selected portion"
  if (selection_is_active())
    {
      delete_selection(UPDATE_DISPLAY);
      return(Xen_true);
    }
  return(snd_no_active_selection_error(S_delete_selection));
}


static Xen g_insert_selection(Xen beg, Xen snd, Xen chn)
{
  #define H_insert_selection "(" S_insert_selection " :optional (beg 0) snd chn): insert the currently selected portion starting at beg"
  if (selection_is_active())
    {
      chan_info *cp;
      mus_long_t samp;
      io_error_t io_err = IO_NO_ERROR;
      sync_info *si_out;

      Snd_assert_channel(S_insert_selection, snd, chn, 2);
      Xen_check_type(Xen_is_integer_or_unbound(beg), beg, 1, S_insert_selection, "an integer");

      cp = get_cp(snd, chn, S_insert_selection);
      if ((!cp) || (!(is_editable(cp)))) return(Xen_false);

      samp = beg_to_sample(beg, S_insert_selection);
      if (Xen_is_integer(chn))
	si_out = make_simple_sync(cp, samp); /* ignore sync */
      else si_out = sync_to_chan(cp);

      io_err = insert_selection(cp, si_out, samp);
      free_sync_info(si_out);

      if (is_serious_io_error(io_err))
	Xen_error(Xen_make_error_type("IO-error"),
		  Xen_list_2(C_string_to_Xen_string(S_insert_selection ": IO error ~A"),
			     C_string_to_Xen_string(io_error_name(io_err))));
      return(Xen_false);
    }
  return(snd_no_active_selection_error(S_insert_selection));
}


static Xen g_mix_selection(Xen beg, Xen snd, Xen chn, Xen sel_chan)
{
  #define H_mix_selection "(" S_mix_selection " :optional (beg 0) snd chn (selection-channel " PROC_TRUE ")): mix the currently selected portion starting at beg"
  if (selection_is_active())
    {
      chan_info *cp;
      mus_long_t obeg;
      io_error_t io_err = IO_NO_ERROR;
      int i, selection_chan = 0, id = -1, chans = 0;
      sync_info *si_out;
      Xen result = Xen_empty_list;

      Snd_assert_channel(S_mix_selection, snd, chn, 2);
      Xen_check_type(Xen_is_integer_or_unbound(beg), beg, 1, S_mix_selection, "an integer");
      Xen_check_type(Xen_is_integer_boolean_or_unbound(sel_chan), sel_chan, 4, S_mix_selection, "an integer or " PROC_TRUE);

      cp = get_cp(snd, chn, S_mix_selection);
      if ((!cp) || (!(is_editable(cp)))) return(Xen_false);

      obeg = beg_to_sample(beg, S_mix_selection);
      if (Xen_is_integer(sel_chan))
	selection_chan = Xen_integer_to_C_int(sel_chan);
      if (Xen_is_integer(chn))
	si_out = make_simple_sync(cp, obeg); /* ignore sync */
      else si_out = sync_to_chan(cp);

      id = mix_selection(cp, si_out, obeg, &io_err, selection_chan);
      chans = si_out->chans;                 /* save for loop below */
      free_sync_info(si_out);

      if (is_serious_io_error(io_err))
	Xen_error(Xen_make_error_type("IO-error"),
		  Xen_list_2(C_string_to_Xen_string(S_mix_selection ": IO error ~A"),
			     C_string_to_Xen_string(io_error_name(io_err))));

      if (id == -1) return(Xen_false);
      for (i = 0; i < chans; i++)
	result = Xen_cons(new_xen_mix(id + i), result);
      return(Xen_list_reverse(result));
    }
  return(snd_no_active_selection_error(S_mix_selection));
}


static Xen g_selection_to_mix(void)
{
  #define H_selection_to_mix "(" S_selection_to_mix "): turns the current selection into a mix"
  if (selection_is_active())
    {
      chan_info *cp;
      io_error_t io_err;
      int i, id = INVALID_MIX_ID, chans = 0, sync = GET_NEW_SYNC;
      sync_info *si_out;
      Xen result = Xen_empty_list;
      char *tempfile, *origin = NULL;

      si_out = selection_sync();
      cp = si_out->cps[0];

      tempfile = snd_tempnam();
      io_err = save_selection(tempfile, snd_srate(cp->sound), MUS_OUT_SAMPLE_TYPE, MUS_NEXT, NULL, SAVE_ALL_CHANS);
      if (is_serious_io_error(io_err))
	{
	  if (tempfile) free(tempfile);
	  free_sync_info(si_out);
	  Xen_error(Xen_make_error_type("IO-error"),
		    Xen_list_2(C_string_to_Xen_string(S_selection_to_mix ": IO error ~A"),
			       C_string_to_Xen_string(io_error_name(io_err))));
	}

      origin = mus_format("%s", S_selection_to_mix);
      if (si_out->chans > 1)
	remember_temp(tempfile, si_out->chans);

      g_scale_selection_by(C_double_to_Xen_real(0.0));

      id = mix_file(selection_beg(NULL), selection_len(), si_out->chans, si_out->cps, tempfile, 
		    (si_out->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME, 
		    origin, true,
		    0);

      deactivate_selection();
      free(origin);
      if (tempfile) free(tempfile);
      chans = si_out->chans;                 /* save for loop below */
      free_sync_info(si_out);

      if (id == -1) return(Xen_false);
      if (chans == 1)
	return(Xen_cons(new_xen_mix(id), Xen_empty_list)); /* no sync */

      for (i = 0; i < chans; i++)
	{
	  sync = mix_set_sync_from_id(id + i, sync);
	  result = Xen_cons(new_xen_mix(id + i), result);
	}

      if ((mix_dialog_mix() >= id) &&
	  (mix_dialog_mix() < (id + chans)))
	reflect_mix_change(id);
       /* this update is needed in a case like: file close (closing old mix), open new, mix -- this mix can now have old mix's id */
      return(Xen_list_reverse(result));
    }
  return(snd_no_active_selection_error(S_selection_to_mix));
}


static Xen g_is_selection(Xen sel)
{
  #define H_is_selection "(" S_is_selection " :optional obj): " PROC_TRUE " if selection is currently active, visible, etc. \
If 'obj' is passed, " S_is_selection " returns " PROC_TRUE " if obj is a selection object and there is a current selection."

  if ((Xen_is_bound(sel)) &&
      (!(xen_is_selection(sel))))
    return(Xen_false);

  return(C_bool_to_Xen_boolean(selection_is_active()));
}


static Xen g_selection_position(Xen snd, Xen chn)
{
  #define H_selection_position "(" S_selection_position " :optional snd chn): selection start samp"
  if (selection_is_active())
    {
      if (!Xen_is_bound(snd))
	return(C_llong_to_Xen_llong(selection_beg(NULL)));
      else
	{
	  chan_info *cp;
	  Snd_assert_channel(S_selection_position, snd, chn, 1);
	  cp = get_cp(snd, chn, S_selection_position);
	  if (!cp) return(Xen_false);
	  return(C_llong_to_Xen_llong(selection_beg(cp)));
	}
    }
  return(snd_no_active_selection_error(S_selection_position));
}


static Xen g_set_selection_position(Xen pos, Xen snd, Xen chn)
{
  chan_info *cp;
  mus_long_t beg;

  Snd_assert_channel(S_set S_selection_position, snd, chn, 2);
  Xen_check_type(Xen_is_integer(pos), pos, 1, S_selection_position, "an integer");

  beg = beg_to_sample(pos, S_set S_selection_position);
  if (!Xen_is_bound(snd))
    {
      sync_info *si = NULL;
      if (selection_is_active())
	si = selection_sync();
      else 
	{
	  cp = current_channel();
	  if (cp) si = sync_to_chan(cp);
	}
      if (si)
	{
	  int i;
	  for (i = 0; i < si->chans; i++) 
	    cp_set_selection_beg(si->cps[i], beg);
	  free_sync_info(si);
	}
    }
  else 
    {
      cp = get_cp(snd, chn, S_set S_selection_position);
      if (!cp) return(Xen_false);
      cp_set_selection_beg(cp, beg);
    }
  redraw_selection();
  return(pos);
}

with_three_setter_args(g_set_selection_position_reversed, g_set_selection_position)


Xen g_selection_framples(Xen snd, Xen chn)
{
  #define H_selection_framples "(" S_selection_framples " :optional snd chn): selection length"
  if (selection_is_active())
    {
      if (!Xen_is_bound(snd))
	return(C_llong_to_Xen_llong(selection_len()));
      else
	{
	  chan_info *cp;
	  Snd_assert_channel(S_selection_framples, snd, chn, 1);
	  cp = get_cp(snd, chn, S_selection_framples);
	  if (!cp) return(Xen_false);
	  return(C_llong_to_Xen_llong(cp_selection_len(cp, NULL)));
	}
    }
  return(snd_no_active_selection_error(S_selection_framples));
}


static Xen g_set_selection_framples(Xen samps, Xen snd, Xen chn)
{
  chan_info *cp;
  mus_long_t len;

  Xen_check_type(Xen_is_llong(samps), samps, 1, S_set S_selection_framples, "an integer");
  len = Xen_llong_to_C_llong(samps);
  if (len <= 0)
    Xen_wrong_type_arg_error(S_set S_selection_framples, 1, samps, "a positive integer");
  if (!Xen_is_bound(snd))
    {
      sync_info *si = NULL;
      if (selection_is_active())
	si = selection_sync();
      else 
	{
	  cp = current_channel();
	  if (cp) si = sync_to_chan(cp);
	}
      if (si)
	{
	  int i;
	  for (i = 0; i < si->chans; i++)
	    cp_set_selection_len(si->cps[i], len);
	  free_sync_info(si);
	}
    }
  else 
    {
      Snd_assert_channel(S_set S_selection_framples, snd, chn, 2);
      cp = get_cp(snd, chn, S_set S_selection_framples);
      if (!cp) return(Xen_false);
      cp_set_selection_len(cp, len);
    }
  redraw_selection();
  return(samps);
}

with_three_setter_args(g_set_selection_framples_reversed, g_set_selection_framples)


static Xen g_selection_member(Xen snd, Xen chn)
{
  #define H_selection_member "(" S_selection_member " :optional snd chn): " PROC_TRUE " if snd's channel chn is a member of the current selection"
  chan_info *cp;
  Snd_assert_channel(S_selection_member, snd, chn, 1);
  cp = get_cp(snd, chn, S_selection_member);
  if (!cp) return(Xen_false);
  return(C_bool_to_Xen_boolean(selection_is_active_in_channel(cp)));
}


static Xen g_set_selection_member(Xen on, Xen snd, Xen chn)
{
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_selection_member, "a boolean");
  if ((Xen_is_true(snd)) && (Xen_is_false(on)))
    deactivate_selection();
  else
    {
      chan_info *cp;
      Snd_assert_channel(S_set S_selection_member, snd, chn, 2);
      cp = get_cp(snd, chn, S_set S_selection_member);
      if (!cp) return(Xen_false);
      if (Xen_is_true(on))
	{
	  if (selection_is_active())
	    cp_set_selection_beg(cp, selection_beg(NULL));
	  else cp_set_selection_beg(cp, 0);
	}
      else cp_deactivate_selection(cp);
      enved_reflect_selection(selection_is_active());
      reflect_selection_in_save_as_dialog(selection_is_active());

      if (Xen_hook_has_list(ss->effects_hook))
	run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

      if (selection_is_active())
	redraw_selection();
    }
  return(on);
}

with_three_setter_args(g_set_selection_member_reversed, g_set_selection_member)


static Xen g_select_all(Xen snd_n, Xen chn_n)
{
  #define H_select_all "(" S_select_all " :optional snd chn): make a new selection containing all of snd's channel chn. \
If sync is set, all chans are included.  The new region id is returned (if " S_selection_creates_region " is " PROC_TRUE ")."
  chan_info *cp;
  int id;

  Snd_assert_channel(S_select_all, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_select_all);
  if (!cp) return(Xen_false);

  id = select_all(cp);
  if (selection_creates_region(ss)) 
    return(C_int_to_Xen_region(id));
  else return(Xen_false);
}


#define H_save_selection "(" S_save_selection " file srate sample-type header-type comment channel): \
save the current selection in file using the indicated file attributes.  If channel is given, save only that channel."

#if HAVE_SCHEME
static s7_pointer g_save_selection(s7_scheme *sc, s7_pointer args)
{
  mus_header_t head_type;
  mus_sample_t samp_type;
  int sr, chn;
  io_error_t io_err;
  const char *com, *file;
  char *fname;
  s7_pointer p, fp, res;

  if (!(selection_is_active()))
    return(snd_no_active_selection_error(S_save_selection));

  fp = s7_car(args);
  if (fp == Xen_false)
    Xen_error(Xen_make_error_type("IO-error"), Xen_list_1(C_string_to_Xen_string(S_save_selection ": no output file?")));
  if (!s7_is_string(fp))
    return(s7_wrong_type_arg_error(sc, S_save_selection, 1, fp, "a string"));
  file = s7_string(fp);
  res = fp;

  fp = s7_cadr(args);
  if (fp == Xen_false)
    sr = selection_srate();
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 2, fp, "an integer"));
      sr = s7_integer(fp);
      if (sr <= 0)
	Xen_error(Xen_make_error_type("cannot-save"),
		  Xen_list_2(C_string_to_Xen_string(S_save_selection ": srate (~A) can't be <= 0"), fp));
    }

  p = s7_cddr(args);
  fp = s7_car(p);
  if (fp == Xen_false)
    samp_type = MUS_UNKNOWN_SAMPLE;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 3, fp, "an integer"));
      samp_type = (mus_sample_t)s7_integer(fp);
    }

  fp = s7_cadr(p);
  if (fp == Xen_false)
    head_type = MUS_UNKNOWN_HEADER;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 4, fp, "an integer"));
      head_type = (mus_header_t)s7_integer(fp);
    }
  if ((head_type != MUS_UNKNOWN_HEADER) && (!(mus_header_writable(head_type, MUS_IGNORE_SAMPLE))))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_2(C_string_to_Xen_string(S_save_selection ": can't write a ~A header"),
			 C_string_to_Xen_string(mus_header_type_name(head_type))));

  if ((head_type != MUS_UNKNOWN_HEADER) && (samp_type != MUS_UNKNOWN_SAMPLE) && (!(mus_header_writable(head_type, samp_type))))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_3(C_string_to_Xen_string(S_save_selection ": can't write ~A data to a ~A header"),
			 C_string_to_Xen_string(mus_sample_type_name(samp_type)),
			 C_string_to_Xen_string(mus_header_type_name(head_type))));

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp == Xen_false)
    com = NULL;
  else
    {
      if (!s7_is_string(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 5, fp, "a string"));
      com = s7_string(fp);
    }
  
  fp = s7_cadr(p);
  if (fp == Xen_false)
    chn = SAVE_ALL_CHANS;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 6, fp, "an integer"));
      chn = s7_integer(fp);
    }

  fname = mus_expand_filename(file);
  io_err = save_selection(fname, sr, samp_type, head_type, com, chn);

  if (fname) free(fname);
  if ((io_err != IO_NO_ERROR) &&
      (io_err != IO_INTERRUPTED) &&
      (io_err != IO_SAVE_HOOK_CANCELLATION))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_3(C_string_to_Xen_string(S_save_selection ": can't save ~S, ~A"), res, C_string_to_Xen_string(snd_open_strerror())));
  return(res);
}
#else
static Xen kw_header_type, kw_comment, kw_file, kw_srate, kw_channel, kw_sample_type;

static void init_selection_keywords(void)
{
  kw_header_type = Xen_make_keyword("header-type");
  kw_sample_type = Xen_make_keyword("sample-type");
  kw_comment = Xen_make_keyword("comment");
  kw_file = Xen_make_keyword("file");
  kw_srate = Xen_make_keyword("srate");
  kw_channel = Xen_make_keyword("channel");
}

static Xen g_save_selection(Xen arglist)
{
  mus_header_t head_type = MUS_UNKNOWN_HEADER;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  int sr = -1, chn = 0;
  io_error_t io_err = IO_NO_ERROR;
  const char *com = NULL, *file = NULL;
  char *fname = NULL;
  Xen args[12]; 
  Xen keys[6];
  int orig_arg[6] = {0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len;
  if (!(selection_is_active()))
    return(snd_no_active_selection_error(S_save_selection));

  /* changed 7-Dec-08 to be more like save-sound-as in default values -- if just one
   *   sound involved, or all sounds same, use current choices rather than MUS_NEXT etc:
   *   hdr->type|srate|format
   */

  keys[0] = kw_file;
  keys[1] = kw_srate;
  keys[2] = kw_sample_type;
  keys[3] = kw_header_type;
  keys[4] = kw_comment;
  keys[5] = kw_channel;

  for (i = 0; i < 12; i++) args[i] = Xen_undefined;
  arglist_len = Xen_list_length(arglist);
  if (arglist_len > 12)
    Xen_out_of_range_error(S_save_selection, 0, arglist, "too many arguments");
  for (i = 0; i < arglist_len; i++) args[i] = Xen_list_ref(arglist, i);

  vals = mus_optkey_unscramble(S_save_selection, arglist_len, 6, keys, args, orig_arg);
  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_save_selection, orig_arg[0], NULL);
      sr = mus_optkey_to_int(keys[1], S_save_selection, orig_arg[1], selection_srate());
      samp_type = (mus_sample_t)mus_optkey_to_int(keys[2], S_save_selection, orig_arg[2], (int)samp_type);
      head_type = (mus_header_t)mus_optkey_to_int(keys[3], S_save_selection, orig_arg[3], (int)head_type);
      com = mus_optkey_to_string(keys[4], S_save_selection, orig_arg[4], NULL);
      chn = mus_optkey_to_int(keys[5], S_save_selection, orig_arg[5], SAVE_ALL_CHANS);
    }

  if (!file) 
    Xen_error(Xen_make_error_type("IO-error"),
	      Xen_list_1(C_string_to_Xen_string(S_save_selection ": no output file?")));

  if ((head_type != MUS_UNKNOWN_HEADER) && (!(mus_header_writable(head_type, MUS_IGNORE_SAMPLE))))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_2(C_string_to_Xen_string(S_save_selection ": can't write a ~A header"),
			 C_string_to_Xen_string(mus_header_type_name(head_type))));

  if ((head_type != MUS_UNKNOWN_HEADER) && (samp_type != MUS_UNKNOWN_SAMPLE) && (!(mus_header_writable(head_type, samp_type))))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_3(C_string_to_Xen_string(S_save_selection ": can't write ~A data to a ~A header"),
			 C_string_to_Xen_string(mus_sample_type_name(samp_type)),
			 C_string_to_Xen_string(mus_header_type_name(head_type))));

  if ((sr != -1) && (sr <= 0))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_2(C_string_to_Xen_string(S_save_selection ": srate (~A) can't be <= 0"),
			 C_int_to_Xen_integer(sr)));

  fname = mus_expand_filename(file);
  io_err = save_selection(fname, sr, samp_type, head_type, com, chn);

  if (fname) free(fname);
  if ((io_err != IO_NO_ERROR) &&
      (io_err != IO_INTERRUPTED) &&
      (io_err != IO_SAVE_HOOK_CANCELLATION))
    Xen_error(Xen_make_error_type("cannot-save"),
	      Xen_list_3(C_string_to_Xen_string(S_save_selection ": can't save ~S, ~A"),
			 keys[0],
			 C_string_to_Xen_string(snd_open_strerror())));
  return(args[orig_arg[0] - 1]);
}
#endif

Xen g_selection_chans(void)
{
  #define H_selection_chans "(" S_selection_chans "): chans in active selection"
  return(C_int_to_Xen_integer(selection_chans()));
}


Xen g_selection_srate(void)
{
  #define H_selection_srate "(" S_selection_srate "): selection srate"
  return(C_int_to_Xen_integer(selection_srate()));
}


Xen g_selection_maxamp(Xen snd, Xen chn)
{
  #define H_selection_maxamp "(" S_selection_maxamp " :optional snd chn): selection maxamp in given channel, or overall maxamp if no args passed."
  if (Xen_is_bound(snd))
    {
      chan_info *cp;
      Snd_assert_channel(S_selection_maxamp, snd, chn, 1);
      cp = get_cp(snd, chn, S_selection_maxamp);
      if (!cp) return(Xen_false);
      return(C_double_to_Xen_real(selection_maxamp(cp)));
    }
  else
    {
      mus_float_t mx = 0.0;
      int i;
      sync_info *si;
      si = selection_sync();
      if (!si)
	return(C_double_to_Xen_real(0.0)); /* no selection -- error? */
      for (i = 0; i < si->chans; i++)
	{
	  mus_float_t cur_mx;
	  cur_mx = selection_maxamp(si->cps[i]);
	  if (cur_mx > mx)
	    mx = cur_mx;
	}
      free_sync_info(si);
      return(C_double_to_Xen_real(mx));
    }
}


static Xen g_selection_maxamp_position(Xen snd, Xen chn)
{
  #define H_selection_maxamp_position "(" S_selection_maxamp_position " :optional snd chn): location of selection maxamp (0 = start of selection)"
  chan_info *cp;
  Snd_assert_channel(S_selection_maxamp_position, snd, chn, 1);
  cp = get_cp(snd, chn, S_selection_maxamp_position);
  if (!cp) return(Xen_false);
  return(C_llong_to_Xen_llong(selection_maxamp_position(cp)));
}


static double sel_beg, sel_end;

static bool get_selection_bounds(chan_info *cp)
{
  if (selection_is_active_in_channel(cp))
    {
      mus_long_t samp;
      double x;
      samp = selection_beg(cp);
      x = (double)samp / snd_srate(cp->sound);
      if ((sel_beg < 0.0) || (x < sel_beg))
	sel_beg = x;
      samp = selection_end(cp);
      x = (double)samp / snd_srate(cp->sound);
      if ((sel_end < 0.0) || (x > sel_end))
	sel_end = x;
    }
  return(false);
}

static bool update_bounds(chan_info *cp)
{
  set_x_axis_x0x1(cp, sel_beg, sel_end);
  return(false);
}


void show_selection(void)
{
  sel_beg = -1.0;
  sel_end = -1.0;
  map_over_chans(get_selection_bounds);
  map_over_chans(update_bounds);
}


static Xen g_show_selection(void)
{
  #define H_show_selection "(" S_show_selection ") adjusts graph bounds to display the current selection in full"
  if (selection_is_active())
    show_selection();
  return(Xen_false);
}


static Xen g_unselect_all(void)
{
  #define H_unselect_all "(" S_unselect_all ") deactivates (unselects) the current selection."
  deactivate_selection();
  return(Xen_false);
}


Xen_wrap_2_optional_args(g_selection_position_w, g_selection_position)
Xen_wrap_2_optional_args(g_selection_framples_w, g_selection_framples)
Xen_wrap_2_optional_args(g_selection_member_w, g_selection_member)
Xen_wrap_no_args(g_selection_w, g_selection)
Xen_wrap_1_optional_arg(g_is_selection_w, g_is_selection)
Xen_wrap_no_args(g_selection_chans_w, g_selection_chans)
Xen_wrap_no_args(g_selection_srate_w, g_selection_srate)
Xen_wrap_2_optional_args(g_selection_maxamp_w, g_selection_maxamp)
Xen_wrap_2_optional_args(g_selection_maxamp_position_w, g_selection_maxamp_position)
Xen_wrap_no_args(g_delete_selection_w, g_delete_selection)
Xen_wrap_3_optional_args(g_insert_selection_w, g_insert_selection)
Xen_wrap_4_optional_args(g_mix_selection_w, g_mix_selection)
Xen_wrap_no_args(g_selection_to_mix_w, g_selection_to_mix)
Xen_wrap_2_optional_args(g_select_all_w, g_select_all)
Xen_wrap_no_args(g_show_selection_w, g_show_selection)
Xen_wrap_no_args(g_unselect_all_w, g_unselect_all)
#if HAVE_SCHEME
#define g_set_selection_position_w g_set_selection_position_reversed
#define g_set_selection_framples_w g_set_selection_framples_reversed
#define g_set_selection_member_w g_set_selection_member_reversed
#else
Xen_wrap_any_args(g_save_selection_w, g_save_selection)
Xen_wrap_3_optional_args(g_set_selection_position_w, g_set_selection_position)
Xen_wrap_3_optional_args(g_set_selection_framples_w, g_set_selection_framples)
Xen_wrap_3_optional_args(g_set_selection_member_w, g_set_selection_member)
#endif

void g_init_selection(void)
{
#if HAVE_SCHEME
  s7_pointer i, b, t, f, sel, p;
  i = s7_make_symbol(s7, "integer?");
  sel = s7_make_symbol(s7, "selection?");
  b = s7_make_symbol(s7, "boolean?");
  f = s7_make_symbol(s7, "float?");
  p = s7_make_symbol(s7, "pair?");
  t = s7_t(s7);
#endif

#if (!HAVE_SCHEME)
  init_selection_keywords();
#endif
  init_xen_selection();

  Xen_define_typed_dilambda(S_selection_position, g_selection_position_w, H_selection_position, S_set S_selection_position, g_set_selection_position_w, 0, 2, 1, 2,
			    s7_make_signature(s7, 3, i, t, t), s7_make_signature(s7, 4, i, t, t, i));
  Xen_define_typed_dilambda(S_selection_framples, g_selection_framples_w, H_selection_framples, S_set S_selection_framples, g_set_selection_framples_w, 0, 2, 1, 2,
			    s7_make_signature(s7, 3, i, t, t), s7_make_signature(s7, 4, i, t, t, i));
  Xen_define_typed_dilambda(S_selection_member, g_selection_member_w, H_selection_member, S_set S_selection_member, g_set_selection_member_w, 0, 2, 1, 2,
			    s7_make_signature(s7, 3, b, t, t), s7_make_signature(s7, 4, b, t, t, b));

  Xen_define_typed_procedure(S_selection,        g_selection_w,        0, 0, 0, H_selection,         s7_make_signature(s7, 1, s7_make_signature(s7, 2, sel, b)));
  Xen_define_typed_procedure(S_is_selection,     g_is_selection_w,     0, 1, 0, H_is_selection,      s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_selection_chans,  g_selection_chans_w,  0, 0, 0, H_selection_chans,   s7_make_signature(s7, 1, i));
  Xen_define_typed_procedure(S_selection_srate,  g_selection_srate_w,  0, 0, 0, H_selection_srate,   s7_make_signature(s7, 1, i));
  Xen_define_typed_procedure(S_selection_maxamp, g_selection_maxamp_w, 0, 2, 0, H_selection_maxamp,  s7_make_signature(s7, 3, f, t, t));
  Xen_define_typed_procedure(S_selection_maxamp_position, g_selection_maxamp_position_w, 0, 2, 0, H_selection_maxamp_position, s7_make_signature(s7, 3, i, t, t));
  Xen_define_typed_procedure(S_select_all,       g_select_all_w,       0, 2, 0, H_select_all,        s7_make_signature(s7, 3, b, t, t));
  Xen_define_typed_procedure(S_unselect_all,     g_unselect_all_w,     0, 0, 0, H_unselect_all,      s7_make_signature(s7, 1, b));
  Xen_define_typed_procedure(S_delete_selection, g_delete_selection_w, 0, 0, 0, H_delete_selection,  s7_make_signature(s7, 1, b));
  Xen_define_typed_procedure(S_insert_selection, g_insert_selection_w, 0, 3, 0, H_insert_selection,  s7_make_signature(s7, 4, b, i, t, t));
  Xen_define_typed_procedure(S_mix_selection,    g_mix_selection_w,    0, 4, 0, H_mix_selection,     s7_make_signature(s7, 5, t, i, t, t, i));
  Xen_define_typed_procedure(S_selection_to_mix, g_selection_to_mix_w, 0, 0, 0, H_selection_to_mix,  s7_make_signature(s7, 1, p));
  Xen_define_typed_procedure(S_show_selection,   g_show_selection_w,   0, 0, 0, H_show_selection,    s7_make_signature(s7, 1, b));
#if HAVE_SCHEME
  s7_define_safe_function_star(s7, S_save_selection, g_save_selection, "file srate sample-type header-type comment channel", H_save_selection);
#else
  Xen_define_typed_procedure(S_save_selection,   g_save_selection_w,   0, 0, 1, H_save_selection,    s7_make_circular_signature(s7, 0, 1, t));
#endif
}
