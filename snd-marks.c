#include "snd.h"

struct mark {
  mus_long_t samp;
  char *name;
  int id, sync;
  bool visible;
  Xen properties;
  int properties_gc_loc;
};

static int mark_id_counter = 0;

int mark_to_int(mark *m) {return(m->id);}


mus_long_t mark_sample(mark *m) {return(m->samp);}


static int sync_max = 0;

int mark_sync_max(void) 
{
  return(sync_max);
}


int mark_sync(mark *m) {return(m->sync);}

void set_mark_sync(mark *m, int val) 
{
  m->sync = val; 
  if (val > sync_max) 
    sync_max = val; 
}


static mark *make_mark_1(mus_long_t samp, const char *name, int id, int sc)
{
  mark *mp;
  mp = (mark *)calloc(1, sizeof(mark));
  if (name) mp->name = mus_strdup(name); else mp->name = NULL;
  mp->samp = samp;
  mp->id = id;
  set_mark_sync(mp, sc);
  mp->properties_gc_loc = NOT_A_GC_LOC;
  mp->properties = Xen_false;
  return(mp);
}


static mark *make_mark(mus_long_t samp, const char *name) 
{
  return(make_mark_1(samp, name, mark_id_counter++, 0));
}


static mark *copy_mark(mark *m) 
{
  return(make_mark_1(m->samp, m->name, m->id, m->sync));
}


static mark *free_mark(mark *mp)
{
  if (mp)
    {
      if (mp->name) free(mp->name);
      if (mp->properties_gc_loc != NOT_A_GC_LOC)
	{
	  snd_unprotect_at(mp->properties_gc_loc);
	  mp->properties_gc_loc = NOT_A_GC_LOC;
	  mp->properties = Xen_false;
	}
      free(mp);
    }
  return(NULL);
}


static mark *map_over_marks(chan_info *cp, mark *(*func)(chan_info *ncp, mark *mp1, void *p1), void *m, read_direction_t direction)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->marks))
    {
      int marks;
      mark **mps;
      mps = ed->marks;
      marks = ed->mark_ctr;
      if (mps)
	{
	  mark *mp;
	  int i;
	  if (direction == READ_FORWARD)
	    {
	      for (i = 0; i <= marks; i++) 
		if (mps[i]) /* can be null if we're running delete_marks at a higher level and draw-mark-hook is active */
		  {
		    mp = (*func)(cp, mps[i], m);
		    if (mp) return(mp);
		  }
	    }
	  else
	    {
	      for (i = marks; i >= 0; i--) 
		if (mps[i])
		  {
		    mp = (*func)(cp, mps[i], m);
		    if (mp) return(mp);
		  }
	    }
	}
    }
  return(NULL);
}


static mark *map_over_marks_with_int(chan_info *cp, mark *(*func)(chan_info *ncp, mark *mp1, int val1), int value, read_direction_t direction)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->marks))
    {
      int marks;
      mark **mps;
      mps = ed->marks;
      marks = ed->mark_ctr;
      if (mps)
	{
	  mark *mp;
	  int i;
	  if (direction == READ_FORWARD)
	    {
	      for (i = 0; i <= marks; i++) 
		if (mps[i]) /* can be null if we're running delete_marks at a higher level and draw-mark-hook is active */
		  {
		    mp = (*func)(cp, mps[i], value);
		    if (mp) return(mp);
		  }
	    }
	  else
	    {
	      for (i = marks; i >= 0; i--) 
		if (mps[i])
		  {
		    mp = (*func)(cp, mps[i], value);
		    if (mp) return(mp);
		  }
	    }
	}
    }
  return(NULL);
}


static mark *find_mark_id_1(chan_info *cp, mark *mp, int id)
{
  if (mp->id == id)
    return(mp); 
  return(NULL);
}


static mark *find_mark_from_id(int id, chan_info **cps, int pos)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      chan_info *cp;
      snd_info *sp;
      uint32_t j;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    {
	      if (pos < cp->edit_size) /* pos can be -1 */
		{
		  int old_pos;
		  mark *mp;
		  old_pos = cp->edit_ctr;
		  if (pos >= 0) cp->edit_ctr = pos;
		  /* memoization would have to be done here where we know cp->edit_ctr */
		  mp = map_over_marks_with_int(cp, find_mark_id_1, id, READ_FORWARD);
		  cp->edit_ctr = old_pos;
		  if (mp) 
		    {
		      if (cps) cps[0] = cp; 
		      return(mp);
		    }
		}
	      }
	  }
  return(NULL);
}


mus_long_t mark_id_to_sample(int id)
{
  mark *m;
  m = find_mark_from_id(id, NULL, AT_CURRENT_EDIT_POSITION);
  if (m)
    return(m->samp);
  return(-1);
}


static mark *find_previous_mark_1(chan_info *cp, mark *mp, void *m)
{
  if (mp->samp < (*((mus_long_t *)m))) 
    return(mp); 
  return(NULL);
}


static mark *find_previous_mark(mus_long_t current_sample, chan_info *cp)
{
  return(map_over_marks(cp, find_previous_mark_1, (void *)(&current_sample), READ_BACKWARD));
}


static mark *find_next_mark_1(chan_info *cp, mark *mp, void *m)
{
  if (mp->samp > (*((mus_long_t *)m))) 
    return(mp); 
  return(NULL);
}


static mark *find_next_mark(mus_long_t current_sample, chan_info *cp)
{
  return(map_over_marks(cp, find_next_mark_1, (void *)(&current_sample), READ_FORWARD));
}


static mark* marks_off_1(chan_info *cp, mark *mp, void *ignore)
{
  mp->visible = false;
  return(NULL);
}


void marks_off(chan_info *cp)
{
  map_over_marks(cp, marks_off_1, NULL, READ_FORWARD);
}


static void show_mark(chan_info *cp, mark *mp, bool show);

static Xen draw_mark_hook;

static void draw_mark_1(chan_info *cp, mark *mp, bool show)
{
  /* fields are samp and name */
  if (!(cp->graph_time_on)) return;
  if (Xen_hook_has_list(draw_mark_hook))
    {
      Xen res;
      res = run_progn_hook(draw_mark_hook,
			   Xen_list_1(new_xen_mark(mp->id)),
			   S_draw_mark_hook);
      if (Xen_is_true(res))
	{
	  mp->visible = show;
	  return;
	}
    }
  show_mark(cp, mp, show);
}


static void draw_mark(chan_info *cp, mark *mp)
{
  if (!(mp->visible)) draw_mark_1(cp, mp, true);
}


static void erase_mark(chan_info *cp, mark *mp)
{
#if (!USE_GTK)
  if (mp->visible) draw_mark_1(cp, mp, false);
#endif
}


typedef struct {
  int x, y;
  mark *all_done;
} mdata;

static mark *hit_mark_1(chan_info *cp, mark *mp, void *m)
{
  int mx;
  mdata *md = (mdata *)m;
  axis_info *ap;

  ap = cp->axis;
  if (mp->samp < ap->losamp) return(NULL);
  if (mp->samp > ap->hisamp) return(md->all_done); /* grf_x clips so we can be confused by off-screen marks */

  mx = grf_x((double)(mp->samp) / (double)snd_srate(cp->sound), cp->axis);
  if (mx > (md->x + mark_tag_width(ss))) return(md->all_done); /* past it */
  if (mx < (md->x - mark_tag_width(ss))) return(NULL);         /* before it */
  if (!mp->name)                                    /* check y if unnamed */
    {
      if ((md->y >= ap->y_axis_y1) && 
	  (md->y <= (ap->y_axis_y1 + mark_tag_height(ss)))) 
	return(mp); 
      else return(NULL);
    }
  else return(mp);
}


#define HIT_SLOP 4

static mark *hit_mark_triangle_1(chan_info *cp, mark *mp, void *m)
{
  /* m->samp = raw x mouse position */
  /* we're going left to right, so after passing x, give up */
  int mx, y;
  mdata *md = (mdata *)m;
  axis_info *ap;

  ap = cp->axis;
  if (mp->samp < ap->losamp) return(NULL);
  if (mp->samp > ap->hisamp) return(md->all_done); /* grf_x clips so we can be confused by off-screen marks */

  mx = grf_x((double)(mp->samp) / (double)snd_srate(cp->sound), cp->axis);
  if (mx > (md->x + HIT_SLOP)) return(md->all_done);
  if ((mx + play_arrow_size(ss) + HIT_SLOP) < md->x) return(NULL);
  y = md->y - ap->y_axis_y0 - play_arrow_size(ss);
  if (y < 0) y = -y;
  if ((mx + play_arrow_size(ss) - y + HIT_SLOP) >= md->x) return(mp);
  /* the last is assuming the triangle shape for hit detection */
  return(NULL);
}


mark *hit_mark_triangle(chan_info *cp, int x, int y)
{
  if (cp->edits[cp->edit_ctr]->marks)
    {
      axis_info *ap;
      ap = cp->axis;
      /* first check that we're in the bottom portion of the graph where the mark triangles are */
      if ((y >= ap->y_axis_y0) && 
	  (y <= (ap->y_axis_y0 + 2 * play_arrow_size(ss))))
	{
	  mark *mp;
	  mdata *md;
	  md = (mdata *)calloc(2, sizeof(mdata));
	  md->x = x;
	  md->y = y;
	  md->all_done = (mark *)1;
	  mp = map_over_marks(cp, hit_mark_triangle_1, (void *)md, READ_FORWARD);
	  if (mp == (mark *)1) mp = NULL;
	  free(md);
	  return(mp);
	}
    }
  return(NULL);
}


static Xen mark_drag_hook;
static Xen mark_hook; /* add, delete, move */

static bool watching_mouse = false; /* this is tracking axis moves */
static int last_mouse_x = 0;
static mark *moving_mark = NULL; /* used only while "off-screen" during axis moves */

static timeout_result_t watch_mouse_button = 0;

#if (!USE_NO_GUI)
static void move_axis_to_track_mark(chan_info *cp);

static TIMEOUT_TYPE watch_mouse(TIMEOUT_ARGS)
{
  chan_info *cp = (chan_info *)context;
  if (watch_mouse_button)
    {
      move_axis_to_track_mark(cp);
      watch_mouse_button = CALL_TIMEOUT(watch_mouse, 50, cp);
    }
  TIMEOUT_RESULT
}
#endif


static void start_mark_watching(chan_info *cp, mark *mp)
{
  moving_mark = mp;
  watch_mouse_button = CALL_TIMEOUT(watch_mouse, 50, cp);
  watching_mouse = true;
}


static void cancel_mark_watch(chan_info *cp)
{
#if (!USE_NO_GUI)
  if (watch_mouse_button) TIMEOUT_REMOVE(watch_mouse_button);
#endif
  watch_mouse_button = 0;
  watching_mouse = false;
  moving_mark = NULL;
}


static bool move_mark_1(chan_info *cp, mark *mp, int x)
{
  axis_info *ap;
  int nx;
  mus_long_t samps;
  bool redraw;

  ap = cp->axis;
  redraw = (!watching_mouse);

  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watching_mouse)
	{
	  if (((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) ||
	      ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)))
	    return(false);
	}
      nx = move_axis(cp, x);
      if (!watching_mouse) start_mark_watching(cp, mp);
    }
  else 
    {
      erase_mark(cp, mp);
      nx = x;
      if (watching_mouse) 
	{
	  cancel_mark_watch(cp); 
	  redraw = false;
	}
    }

  mp->samp = (mus_long_t)(ungrf_x(ap, nx) * snd_srate(cp->sound));
  if (mp->samp < 0) mp->samp = 0;

  samps = current_samples(cp);
  if (mp->samp > samps) mp->samp = samps;

  if (Xen_hook_has_list(mark_drag_hook))
    ss->squelch_mark_drag_info = Xen_is_true(run_progn_hook(mark_drag_hook,
							   Xen_list_1(new_xen_mark(mp->id)),
							   S_mark_drag_hook));
  else ss->squelch_mark_drag_info = false;
  return(redraw);
}


static int compare_mark_samps(const void *mp1, const void *mp2)
{
  mark *m1, *m2;
  m1 = (mark *)(*((mark **)mp1));
  m2 = (mark *)(*((mark **)mp2));
  if (m1->samp < m2->samp) return(-1);
  if (m1->samp == m2->samp) return(0);
  return(1);
}


static void sort_marks(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed)
    qsort((void *)(ed->marks), ed->mark_ctr + 1, sizeof(mark *), compare_mark_samps);
}


typedef enum {MARK_ADD, MARK_DELETE, MARK_MOVE, MARKS_DELETE, MARK_RELEASE} mark_hook_reason_t;

static void run_mark_hook(chan_info *cp, int id, mark_hook_reason_t reason)
{
  /* called after the mark list has been made consistent */
  if (Xen_hook_has_list(mark_hook))
    run_hook(mark_hook,
	     Xen_list_4(new_xen_mark(id),
			C_int_to_Xen_sound(cp->sound->index),
			C_int_to_Xen_integer(cp->chan),
			C_int_to_Xen_integer((int)reason)),
	     S_mark_hook);

  if (Xen_hook_has_list(ss->effects_hook))
    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);
}


#define MARKS_ALLOC_SIZE 4

mark *add_mark(mus_long_t samp, const char *name, chan_info *cp)
{
  int i, med;
  mark **mps;
  ed_list *ed;

  ed = cp->edits[cp->edit_ctr];
  if (!(ed->marks))
    {
      ed->mark_size = MARKS_ALLOC_SIZE;
      ed->mark_ctr = -1;
      ed->marks = (mark **)calloc(MARKS_ALLOC_SIZE, sizeof(mark *));
    }

  ed->mark_ctr++;
  if (ed->mark_ctr >= ed->mark_size)
    {
      ed->mark_size += MARKS_ALLOC_SIZE;
      ed->marks = (mark **)realloc(ed->marks, ed->mark_size * sizeof(mark *));
      for (i = ed->mark_size - MARKS_ALLOC_SIZE; i < ed->mark_size; i++) ed->marks[i] = NULL;
    }

  mps = ed->marks;
  med = ed->mark_ctr;

  if (med == 0)
    {
      if (mps[0]) free_mark(mps[0]);
      mps[0] = make_mark(samp, name);
      run_mark_hook(cp, mps[0]->id, MARK_ADD);
      return(mps[0]);
    }
  else
    {
      for (i = 0; i < med; i++) /* not <= because we pre-incremented above */
	{
	  mark *mp;
	  mp = mps[i];
	  if (samp < mp->samp)
	    {
	      int j;
	      if (mps[med]) free_mark(mps[med]);
	      for (j = med; j > i; j--)
		mps[j] = mps[j - 1];
	      mps[i] = make_mark(samp, name);
	      run_mark_hook(cp, mps[i]->id, MARK_ADD);
	      return(mps[i]);
	    }
	}
      /* insert at end */
      if (mps[med]) free_mark(mps[med]);
      mps[med] = make_mark(samp, name);
      run_mark_hook(cp, mps[med]->id, MARK_ADD);
      return(mps[med]);
    }
}


bool delete_mark_samp(mus_long_t samp, chan_info *cp)
{
  if (cp)
    {
      ed_list *ed;
      ed = cp->edits[cp->edit_ctr];
      if (ed->marks)
	{
	  mark **mps;
	  mps = ed->marks;
	  if (mps)
	    {
	      int i, edm;
	      edm = ed->mark_ctr;
	      for (i = 0; i <= edm; i++)
		{
		  mark *mp;
		  mp = mps[i];
		  if (mp->samp == samp)
		    {
		      axis_info *ap;
		      int id = -1;
		      ap = cp->axis;
		      if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, mp); 
		      id = mp->id;
		      free_mark(mp);
		      mps[i] = NULL;
		      if (i < edm)
			{
			  int j;
			  for (j = i; j < edm; j++) mps[j] = mps[j + 1];
			  mps[edm] = NULL;
			}
		      ed->mark_ctr--;
		      run_mark_hook(cp, id, MARK_DELETE);
		      return(true);
		    }
		}
	    }
	}
    }
  return(false);
}


static bool delete_mark_id(int id, chan_info *cp)
{
  if (cp)
    {
      ed_list *ed;
      ed = cp->edits[cp->edit_ctr];
      if (ed->marks)
	{
	  mark **mps;
	  mps = ed->marks;
	  if (mps)
	    {
	      int i, edm;
	      edm = ed->mark_ctr;
	      for (i = 0; i <= edm; i++)
		{
		  mark *mp;
		  mp = mps[i];
		  if (mp->id == id)
		    {
		      axis_info *ap;
		      ap = cp->axis;
		      if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, mp); 
		      free_mark(mp);
		      mps[i] = NULL;
		      if (i < edm)
			{
			  int j;
			  for (j = i; j < edm; j++) mps[j] = mps[j + 1];
			  mps[edm] = NULL;
			}
		      ed->mark_ctr--;
		      run_mark_hook(cp, id, MARK_DELETE);
		      return(true);
		    }
		}
	    }
	}
    }
  return(false);
}


static void delete_marks(chan_info *cp)
{
  if (cp)
    {
      ed_list *ed;
      ed = cp->edits[cp->edit_ctr];
      if (ed->marks)
	{
	  mark **mps;
	  mps = ed->marks;
	  if (mps)
	    {
	      int i;
	      for (i = 0; i < ed->mark_size; i++)
		{
		  mark *mp;
		  mp = mps[i];
		  if (mp) 
		    {
		      axis_info *ap;
		      ap = cp->axis;
		      if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, mp); 
		      free_mark(mp);
		      mps[i] = NULL;
		    }
		}
	      ed->mark_ctr = -1;
	      run_mark_hook(cp, -1, MARKS_DELETE);
	    }
	}
    }
}


void free_mark_list(ed_list *ed)
{
  if (ed->marks)
    {
      int j;
      for (j = 0; j < ed->mark_size; j++)
	{
	  mark *mp;
	  mp = ed->marks[j];
	  if (mp) free_mark(mp);
	  ed->marks[j] = NULL;
	}
      free(ed->marks);
      ed->marks = NULL;
      ed->mark_size = 0;
    }
}


/* save and restore across update-sound */

typedef struct {
  mark **marks;
  int ctr;
  int size;
} mark_info;

typedef struct {
  mark_info **ms;
  int size;
} marks_info;

void *sound_store_marks(snd_info *sp)
{
  uint32_t i;
  mark_info **res;
  marks_info *rtn;
  res = (mark_info **)calloc(sp->nchans, sizeof(mark_info *));
  rtn = (marks_info *)calloc(1, sizeof(marks_info));
  rtn->ms = res;
  rtn->size = sp->nchans;
  for (i = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      cp = sp->chans[i];
      if (cp)
	{
	  ed_list *ed;
	  ed = cp->edits[cp->edit_ctr];
	  if (ed)
	    {
	      res[i] = (mark_info *)calloc(1, sizeof(mark_info));
	      res[i]->marks = ed->marks;
	      res[i]->ctr = ed->mark_ctr;
	      res[i]->size = ed->mark_size;
	      ed->marks = NULL;
	      ed->mark_size = 0;
	      ed->mark_ctr = -1;
	    }
	}
    }
  return((void *)rtn);
}


void sound_restore_marks(snd_info *sp, void *mrk)
{
  marks_info *mrks = (marks_info *)mrk;
  if (mrks)
    {
      int i, lim;
      mark_info **marks;
      marks = mrks->ms;
      lim = mrks->size;
      if ((int)sp->nchans < lim) lim = sp->nchans; /* update can change channel number either way */
      for (i = 0; i < lim; i++)
	{
	  if (marks[i])
	    {
	      ed_list *ed;
	      chan_info *cp;
	      cp = sp->chans[i];
	      ed = cp->edits[0];
	      if (ed)
		{
		  ed->marks = marks[i]->marks;
		  ed->mark_ctr = marks[i]->ctr;
		  ed->mark_size = marks[i]->size;
		}
	    }
	}
      for (i = 0; i < mrks->size; i++)
	if (marks[i]) free(marks[i]);
      /* possible memleak here if chan num has lessened */
      free(marks);
      free(mrks);
    }
}


static mark *find_nth_mark(chan_info *cp, int count)
{
  int i, c;
  mus_long_t samp;
  mark *mp = NULL;
  if ((!cp) || (!cp->edits[cp->edit_ctr]->marks)) return(NULL);
  if (count > 0) c = count; else c = -count;
  samp = cursor_sample(cp);
  for (i = 0; i < c; i++)
    {
      if (count > 0) mp = find_next_mark(samp, cp);
      else mp = find_previous_mark(samp, cp);
      if (!mp) break;
      samp = mp->samp;
    }
  return(mp);
}


bool goto_mark(chan_info *cp, int count)
{
  mark *mp;
  if ((!cp) || (!cp->edits[cp->edit_ctr]->marks)) return(false);
  mp = find_nth_mark(cp, count);
  if (!mp) return(false);
  cursor_moveto(cp, mp->samp);
  return(true);
}


#if 0
static mark *find_named_mark_1(chan_info *cp, mark *mp, void *uname)
{
  char *name = (char *)uname;
  if ((mp->name) && (mus_strcmp(mp->name, name))) return(mp);
  else return(NULL);
}


static mark *find_named_mark(chan_info *cp, const char *name)
{
  return(map_over_marks(cp, find_named_mark_1, (void *)name, READ_FORWARD));
}


void goto_named_mark(chan_info *cp, const char *name)
{
  mark *mp;
  mp = find_named_mark(cp, name);
  if (mp) cursor_moveto(cp, mp->samp);
}
#endif


static mark *active_mark_1(chan_info *cp, mark *mp, void *ignore)
{
  axis_info *ap;
  ap = cp->axis;
  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) return(mp); 
  return(NULL);
}


mark *active_mark(chan_info *cp)
{
  return(map_over_marks(cp, active_mark_1, NULL, READ_FORWARD));
}


mus_long_t mark_beg(chan_info *cp)
{
  /* called only in snd-chn.c for active zoom */
  mark *mp;
  mp = active_mark(cp);
  if (mp) return(mp->samp);
  return(-1);
}


typedef struct {
  mus_long_t last_samp;
} dpy_last;

static mark *display_channel_marks_1(chan_info *cp, mark *mp, void *m)
{
  axis_info *ap;
  dpy_last *ls = (dpy_last *)m;
  ap = cp->axis;
  if (mp->samp > ap->hisamp) return(mp); /* terminates loop */
  if (mp->samp == ls->last_samp)
    return(NULL); 
  /* actually this should notice how wide in samples the mark stem is and avoid any redraw until we get to the next clear pixel */
  else ls->last_samp = mp->samp;         /* avoid drawing twice at same point == erase */
  if ((mp->samp >= ap->losamp) && 
      (mp->samp <= ap->hisamp) && 
      (mp != moving_mark))
    draw_mark(cp, mp);
  return(NULL);
}


void display_channel_marks(chan_info *cp)
{
  if ((cp->edits[cp->edit_ctr]->marks) && (cp->show_marks))
    {
      dpy_last ls;
      ls.last_samp = -1;
      map_over_marks(cp, display_channel_marks_1, (void *)(&ls), READ_FORWARD);
    }
}


void ripple_marks(chan_info *cp, mus_long_t beg, mus_long_t change)
{
  /* if change = 0, just set ptr, else copy and fixup with deletions */
  /* this is called after the tree has been pushed forward, so edit_ctr is ahead of us */
  /* but we don't do anything if no marks */

  if ((cp) && (cp->edit_ctr > 0))
    {
      ed_list *old_ed, *new_ed;
      new_ed = cp->edits[cp->edit_ctr];
      old_ed = cp->edits[cp->edit_ctr - 1];
      
      if (new_ed->marks)
	{
	  /* release current */
	  free_mark_list(new_ed);
	}

      /* copy old with position change */
      new_ed->mark_ctr = old_ed->mark_ctr;
      new_ed->mark_size = old_ed->mark_size;
      if (new_ed->mark_size > 0)
	{
	  new_ed->marks = (mark **)calloc(new_ed->mark_size, sizeof(mark *));
	  if (new_ed->mark_ctr >= 0)
	    {
	      int i;
	      mark **mps, **mpo;
	      mark *mp;
	      mps = new_ed->marks;
	      mpo = old_ed->marks;
	      for (i = 0; i <= new_ed->mark_ctr; i++)
		mps[i] = copy_mark(mpo[i]);
	      if (change < 0)
		{
		  mus_long_t end;
		  /* if (change < 0) and any marks are between beg and beg+change, they must be deleted */
		  end = beg - change - 1;
		  i = 0;
		  while (i <= new_ed->mark_ctr)
		    {
		      mp = mps[i];
		      if ((mp->samp >= beg) && (mp->samp <= end)) /* was mp->samp > beg, ditto end, beg can = end */
			delete_mark_samp(mp->samp, cp);           /* changes cp->mark_ctr, hence the while loop?  */
		      else 
			{
			  if (mp->samp > beg)                     /* don't change marks that precede the point of the change */
			    mp->samp += change;
			  i++;
			}
		    }
		}
	      else
		{
		  if (change > 0)
		    for (i = 0; i <= new_ed->mark_ctr; i++)
		      {
			mp = mps[i];
			if (mp->samp > beg) mp->samp += change;
		      }
		}
	    }
	}
    }
}


bool mark_define_region(chan_info *cp, int count)
{
  if ((cp) && (max_regions(ss) > 0))
    {
      ed_list *ed;
      ed = cp->edits[cp->edit_ctr];
      if (ed->marks)
	{
	  mus_long_t beg;
	  mark *mp;
	  beg = cursor_sample(cp);
	  mp = find_nth_mark(cp, count);
	  if (mp)
	    {
	      mus_long_t end;
	      end = mp->samp;
	      if (end != beg)
		{
		  mus_long_t ends[1];
		  sync_info *si;
		  int i;
		  ends[0] = end;
		  if (end < beg) 
		    {
		      ends[0] = beg;
		      beg = end;
		    }
		  deactivate_selection();
		  si = sync_to_chan(cp);
		  si->begs[0] = beg;
		  define_region(si, ends);
		  for (i = 0; i < si->chans; i++)
		    {
		      reactivate_selection(si->cps[i], beg, ends[0]);
		      update_graph(si->cps[i]);
		    }
		  free_sync_info(si);
		  return(true);
		}
	    }
	}
    }
  return(false);
}


static mark *reverse_mark_1(chan_info *cp, mark *mp, void *um)
{
  mark *m = (mark *)um;
  mp->samp = m->samp - mp->samp;
  return(NULL);
}


void reverse_marks(chan_info *cp, mus_long_t beg, mus_long_t dur) /* beg -1 for full sound */
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed->marks)
    {
      mark *m;
      mark **mps;
      mps = ed->marks;
      if (beg == -1)
	{
	  m = make_mark_1(current_samples(cp) - 1, NULL, 0, 0);
	  map_over_marks(cp, reverse_mark_1, (void *)m, READ_FORWARD);
	  free_mark(m);
	}
      else
	{
	  mus_long_t end;
	  int i, marks;
	  end = beg + dur - 1;
	  marks = ed->mark_ctr;
	  for (i = 0; i <= marks; i++) 
	    {
	      m = mps[i];
	      if ((m->samp >= beg) && (m->samp <= end))
		m->samp = end - (m->samp - beg);
	    }
	}
      if (ed->mark_ctr >= 0)
	qsort((void *)mps, ed->mark_ctr + 1, sizeof(mark *), compare_mark_samps);
    }
}


void src_marks(chan_info *cp, mus_float_t ratio, mus_long_t old_samps, mus_long_t new_samps, mus_long_t beg, bool over_selection)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed->marks) && (ed->mark_ctr >= 0))
    {		
      int i, marks;
      mark *m;
      mark **mps;
      mps = ed->marks;
      marks = ed->mark_ctr;
      if (!over_selection)
	{
	  for (i = 0; i <= marks; i++) 
	    {
	      m = mps[i];
	      if (ratio > 0.0)
		m->samp = (mus_long_t)(m->samp / ratio);
	      else m->samp = (mus_long_t)((old_samps - 1 - m->samp) / (-ratio)); /* ratio < 0 here */
	    }
	}
      else
	{
	  mus_long_t end;
	  end = beg + old_samps - 1;
	  for (i = 0; i <= marks; i++) 
	    {
	      m = mps[i];
	      if ((m->samp >= beg) && (m->samp <= end))
		{
		  if (ratio > 0.0)
		    m->samp = beg + (mus_long_t)((m->samp - beg) / ratio);
		  else m->samp = beg + (mus_long_t)((old_samps - 1 - (m->samp - beg)) / (-ratio));
		}
	      else
		{
		  if (m->samp > end)
		    m->samp += (new_samps - old_samps);
		}
	    }
	}
      if (ratio < 0.0) qsort((void *)mps, marks + 1, sizeof(mark *), compare_mark_samps);
    }
}


void reset_marks(chan_info *cp, int cur_marks, mus_long_t *samps, mus_long_t end, mus_long_t extension, bool over_selection)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed->marks) && (ed->mark_ctr >= 0))
    {		
      int i, marks;
      mark *m;
      mark **mps;
      mps = ed->marks;
      marks = ed->mark_ctr;
      if (over_selection)
	for (i = 0; i <= marks; i++) 
	  {
	    m = mps[i];
	    if (m->samp > end) m->samp += extension;
	  }
      for (i = 0; (i <= marks) && (i < cur_marks); i++) 
	{
	  m = mps[i];
	  if (samps[i] >= 0) m->samp = samps[i];
	}
      qsort((void *)mps, marks + 1, sizeof(mark *), compare_mark_samps);
    }
}


void ripple_trailing_marks(chan_info *cp, mus_long_t beg, mus_long_t old_len, mus_long_t new_len)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed->marks) && (ed->mark_ctr >= 0))
    {		
      int i, marks;
      mark **mps;
      ripple_marks(cp, 0, 0);
      mps = ed->marks;
      marks = ed->mark_ctr;
      for (i = 0; i <= marks; i++) 
	{
	  mark *m;
	  m = mps[i];
	  if (m->samp > (beg + old_len)) m->samp += (new_len - old_len);
	}
    }
}


void swap_marks(chan_info *cp0, chan_info *cp1)
{
  ed_list *ed0, *ed1;
  ed0 = cp0->edits[cp0->edit_ctr];
  ed1 = cp1->edits[cp1->edit_ctr];
  if ((ed0->marks) || (ed1->marks))
    {
      mark **mps0 = NULL, **mps1 = NULL;
      int ctr0 = -1, ctr1 = -1;
      int size0 = 0, size1 = 0;
      if (ed0->marks)
	{
	  mps0 = ed0->marks;
	  ctr0 = ed0->mark_ctr;
	  size0 = ed0->mark_size;
	}
      if (ed1->marks)
	{
	  mps1 = ed1->marks;
	  ctr1 = ed1->mark_ctr;
	  size1 = ed1->mark_size;
	}
      ed0->marks = mps1;
      ed0->mark_ctr = ctr1;
      ed0->mark_size = size1;
      ed1->marks = mps0;
      ed1->mark_ctr = ctr0;
      ed1->mark_size = size0;
    }
}


/* -------------------------------- SYNCD AND DRAGGED MARKS -------------------------------- */

typedef struct {
  mark **marks;
  chan_info **chans;
  int marks_size;
  int mark_ctr;
  int sync;
  mus_long_t *initial_samples;
} syncdata;

static syncdata *make_syncdata(int sync)
{
  syncdata *sd;
  sd = (syncdata *)calloc(1, sizeof(syncdata));
  sd->sync = sync;
  sd->mark_ctr = 0;
  sd->marks_size = 8;
  sd->marks = (mark **)calloc(sd->marks_size, sizeof(mark *));
  sd->chans = (chan_info **)calloc(sd->marks_size, sizeof(chan_info *));
  sd->initial_samples = (mus_long_t *)calloc(sd->marks_size, sizeof(mus_long_t));
  return(sd);
}


static void add_syncd_mark(syncdata *sd, mark *mp, chan_info *cp)
{
  sd->marks[sd->mark_ctr] = mp;
  sd->initial_samples[sd->mark_ctr] = mp->samp;
  sd->chans[sd->mark_ctr++] = cp;
  if (sd->mark_ctr == sd->marks_size)
    {
      int i;
      sd->marks = (mark **)realloc(sd->marks, sd->marks_size * 2 * sizeof(mark *));
      sd->chans = (chan_info **)realloc(sd->chans, sd->marks_size * 2 * sizeof(chan_info *));
      /* why was initial_samples missing? 2-May-02 */
      sd->initial_samples = (mus_long_t *)realloc(sd->initial_samples, sd->marks_size * 2 * sizeof(mus_long_t));
      for (i = sd->marks_size; i < sd->marks_size * 2; i++) {sd->marks[i] = NULL; sd->chans[i] = NULL;}
      sd->marks_size *= 2;
    }
}


static mark *gather_local_syncd_marks(chan_info *cp, mark *mp, void *usd)
{
  syncdata *sd = (syncdata *)usd;
  if (sd->sync == mp->sync)
    add_syncd_mark(sd, mp, cp);
  return(NULL);
}


static void gather_chan_syncd_marks(chan_info *cp, void *sd)
{
  map_over_marks(cp, gather_local_syncd_marks, sd, READ_FORWARD);
}


static syncdata *gather_syncd_marks(int sync)
{
  syncdata *sd;
  sd = make_syncdata(sync);
  for_each_normal_chan_with_void(gather_chan_syncd_marks, (void *)sd);
  return(sd);
}


static syncdata *free_syncdata(syncdata *sd)
{
  if (sd)
    {
      if (sd->marks) free(sd->marks);
      if (sd->initial_samples) free(sd->initial_samples);
      if (sd->chans) free(sd->chans);
      free(sd);
    }
  return(NULL);
}


static bool mark_control_clicked = false; /* C-click of mark -> drag data as mark is dragged */
static mus_long_t mark_initial_sample = 0;
static syncdata *mark_sd = NULL;

typedef struct {
  widget_t graph;
  point_t *p0, *p1;
  int lastpj;
  color_t color;
} mark_context;

static mark_context **mark_movers = NULL;

static mark_context *make_mark_context(chan_info *cp)
{
  mark_context *g;
  g = (mark_context *)calloc(1, sizeof(mark_context));
  g->graph = channel_graph(cp);
  g->color = ss->mark_color;
  return(g);
}


static mark_context *free_mark_context(mark_context *ms)
{
  if (ms->p0) {free(ms->p0); ms->p0 = NULL;}
  if (ms->p1) {free(ms->p1); ms->p1 = NULL;}
  free(ms);
  return(NULL);
}


static void mark_save_graph(mark_context *ms, int j);
static void make_mark_graph(chan_info *cp, mus_long_t initial_sample, mus_long_t current_sample, int which);

static void initialize_md_context(int size, chan_info **cps)
{
  int i;
  mark_movers = (mark_context **)calloc(size, sizeof(mark_context *));
  for (i = 0; i < size; i++)
    {
      mark_context *ms;
      mark_movers[i] = make_mark_context(cps[i]);
      ms = mark_movers[i];
      ms->lastpj = make_dragged_marks_graph(cps[i]);
      mark_save_graph(ms, ms->lastpj);
    }
}


static void finalize_md_context(int size)
{
  if (mark_movers)
    {
      int i;
      for (i = 0; i < size; i++) 
	if (mark_movers[i]) 
	  free_mark_context(mark_movers[i]);
      free(mark_movers);
      mark_movers = NULL;
    }
}


void set_mark_control(chan_info *cp, mark *mp, int key_state)
{
  mark_control_clicked = (key_state & snd_ControlMask);
  
  if (mark_control_clicked)
    {
      mark_initial_sample = mp->samp;
      if (mark_sd) 
	{
	  if ((mark_sd->mark_ctr > 1) &&
	      (mark_sd->marks[0] != mp))
	    {
	      int loc;
	      for (loc = 1; loc < mark_sd->mark_ctr; loc++)
		if (mark_sd->marks[loc] == mp) break;
	      if (loc < mark_sd->mark_ctr)
		{
		  chan_info *tc;
		  mus_long_t ts;
		  mark *tm;
		  tm = mark_sd->marks[0];
		  ts = mark_sd->initial_samples[0];
		  tc = mark_sd->chans[0];
		  mark_sd->marks[0] = mark_sd->marks[loc];
		  mark_sd->initial_samples[0] = mark_sd->initial_samples[loc];
		  mark_sd->chans[0] = mark_sd->chans[loc];
		  mark_sd->marks[loc] = tm;
		  mark_sd->initial_samples[loc] = ts;
		  mark_sd->chans[loc] = tc;
		}
	    }
	  initialize_md_context(mark_sd->mark_ctr, mark_sd->chans);
	}
      else initialize_md_context(1, &cp);
    }
}


mark *hit_mark(chan_info *cp, int x, int y)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed->marks) && (ed->mark_ctr >= 0))
    {
      axis_info *ap;
      ap = cp->axis;

      /* first check that we're in the top portion of the graph where the mark tabs are */
      if ((y >= ap->y_axis_y1) && 
	  (y <= (ap->y_axis_y1 + mark_tag_height(ss) + 10)))               /*  + 10 for named marks -- checked again later */
	{
	  mark *mp;
	  mdata *md;

	  md = (mdata *)calloc(1, sizeof(mdata));
	  md->x = x;
	  md->y = y;
	  md->all_done = (mark *)1;
	  mp = map_over_marks(cp, hit_mark_1, (void *)md, READ_FORWARD);
	  if (mp == (mark *)1) mp = NULL;
	  free(md);

	  if (mp)
	    {
	      if (mp->sync != 0) 
		{
		  if (mark_sd) mark_sd = free_syncdata(mark_sd);
		  mark_sd = gather_syncd_marks(mp->sync);
		}
	    }
	  return(mp);
	}
    }
  return(NULL);
}


#if (!USE_NO_GUI)

static void allocate_erase_grf_points(mark_context *ms)
{
  if (!ms->p0)
    {
      ms->p0 = (point_t *)calloc(POINT_BUFFER_SIZE, sizeof(point_t));
      ms->p1 = (point_t *)calloc(POINT_BUFFER_SIZE, sizeof(point_t));
    }
}


static void backup_erase_grf_points(mark_context *ms, int nj)
{
  ms->lastpj = nj;
  memcpy((void *)(ms->p0), (void *)get_grf_points(), nj * sizeof(point_t));
  memcpy((void *)(ms->p1), (void *)get_grf_points1(), nj * sizeof(point_t));
}


static void mark_save_graph(mark_context *ms, int j)
{
  allocate_erase_grf_points(ms);
  backup_erase_grf_points(ms, j);
}


static void erase_and_draw_grf_points(mark_context *ms, chan_info *cp, int nj)
{
  graphics_context *ax;
  point_t *points;
  chan_info *draw_cp;
#if USE_MOTIF
  GC draw_gc, undraw_gc;
#else
  gc_t *draw_gc, *undraw_gc;
#endif

  points = get_grf_points();
  draw_cp = channel_to_chan(cp);
  ax = draw_cp->ax;

#if USE_GTK
#if (GTK_CHECK_VERSION(3, 89, 0))
  ss->cr = cp->graph_cr;
#else
  ss->cr = make_cairo(ax->wn);
#endif
#endif

  undraw_gc = erase_GC(draw_cp);
  draw_gc = copy_GC(draw_cp);
  if (draw_cp->time_graph_style == GRAPH_LINES)
    {
      ax->gc = undraw_gc;
      draw_lines(ax, ms->p0, ms->lastpj);
      ax->gc = draw_gc;
      draw_lines(ax, points, nj);
    }
  else 
    {
      ax->gc = undraw_gc;
      draw_points(ax, ms->p0, ms->lastpj, draw_cp->dot_size);
      ax->gc = draw_gc;
      draw_points(ax, points, nj, draw_cp->dot_size);
    }
  backup_erase_grf_points(ms, nj);
  ax->gc = draw_gc;

#if USE_GTK
#if (!GTK_CHECK_VERSION(3, 89, 0))
  free_cairo(ss->cr);
#endif
  ss->cr = NULL;
#endif
}


static void erase_and_draw_both_grf_points(mark_context *ms, chan_info *cp, int nj)
{
  graphics_context *ax;
  point_t *points, *points1;
  chan_info *draw_cp;
#if USE_MOTIF
  GC draw_gc, undraw_gc;
#else
  gc_t *draw_gc, *undraw_gc;
#endif

  points = get_grf_points();
  points1 = get_grf_points1();

  draw_cp = channel_to_chan(cp);
  ax = draw_cp->ax;

#if USE_GTK
#if (GTK_CHECK_VERSION(3, 89, 0))
  ss->cr = cp->graph_cr;
#else
  ss->cr = make_cairo(ax->wn);
#endif
#endif

  undraw_gc = erase_GC(draw_cp);
  draw_gc = copy_GC(draw_cp);
  if (draw_cp->time_graph_style == GRAPH_LINES)
    {
      ax->gc = undraw_gc;
      draw_lines(ax, ms->p0, ms->lastpj);
      draw_lines(ax, ms->p1, ms->lastpj);
      ax->gc = draw_gc;
      draw_lines(ax, points, nj);
      draw_lines(ax, points1, nj);
    }
  else 
    {
      ax->gc = undraw_gc;
      draw_points(ax, ms->p0, ms->lastpj, draw_cp->dot_size);
      draw_points(ax, ms->p1, ms->lastpj, draw_cp->dot_size);
      ax->gc = draw_gc;
      draw_points(ax, points, nj, draw_cp->dot_size);
      draw_points(ax, points1, nj, draw_cp->dot_size);
    }
  backup_erase_grf_points(ms, nj);
  ax->gc = draw_gc;

#if USE_GTK
#if (!GTK_CHECK_VERSION(3, 89, 0))
  free_cairo(ss->cr);
#endif
  ss->cr = NULL;
#endif
}
#else
static void mark_save_graph(mark_context *ms, int j) {}
#endif


static bool move_syncd_mark(chan_info *cp, mark *m, int x)
{
  mus_long_t old_samp, diff;
  bool redraw;
  old_samp = m->samp;
  redraw = move_mark_1(cp, m, x);
  diff = m->samp - old_samp;
  if (diff != 0)
    {
      if ((mark_sd) && (mark_sd->mark_ctr > 1))
	{
	  int i;
	  for (i = 0; i < mark_sd->mark_ctr; i++)
	    {
	      mark *mp;
	      mp = mark_sd->marks[i];
	      if (mp != m)
		{
		  axis_info *ap;
		  mus_long_t samps;
		  chan_info *ncp;
		  ncp = mark_sd->chans[i];
		  ap = ncp->axis;
		  if ((mp->samp >= ap->losamp) && 
		      (mp->samp <= ap->hisamp)) 
		    erase_mark(ncp, mp);
		  mp->samp += diff;
		  if (mp->samp < 0) mp->samp = 0;
		  samps = current_samples(ncp);
		  if (mp->samp > samps) mp->samp = samps;
		  if (mark_control_clicked)
		    make_mark_graph(ncp, mark_sd->initial_samples[i], mp->samp, i);
		  if ((mp->samp >= ap->losamp) && 
		      (mp->samp <= ap->hisamp)) 
		    draw_mark(ncp, mp);
		}
	    }
	}
    }
  return(redraw);
}


#if (!USE_NO_GUI)
static void move_axis_to_track_mark(chan_info *cp)
{
  if (moving_mark)
    {
      bool redraw;
      if (moving_mark->sync)
	redraw = move_syncd_mark(cp, moving_mark, last_mouse_x);
      else redraw = move_mark_1(cp, moving_mark, last_mouse_x);
      if (redraw) draw_mark(cp, moving_mark);
    }
}
#endif


void move_mark(chan_info *cp, mark *mp, int x) /* from mouse drag callback in snd-chn.c, called whenever mark is visible */
{
  bool redraw;
  last_mouse_x = x;

  if (mp->sync)
    redraw = move_syncd_mark(cp, mp, x);
  else redraw = move_mark_1(cp, mp, x);

  if (mark_control_clicked)
    make_mark_graph(cp, mark_initial_sample, mp->samp, 0);
      
 #if (!USE_GTK)
  if (redraw) draw_mark(cp, mp);
 #else
  if ((redraw) && (!mark_control_clicked))
    display_channel_time_data(cp);
 #endif
}


static void edit_dragged_mark(chan_info *cp, mark *m, mus_long_t initial_sample)
{
  /* edit -- initial_sample is where we were when the drag started, ended at m->samp */
  mus_long_t num, mark_final_sample;
  int id;
  mark *new_m;

  mark_final_sample = m->samp;
  num = mark_final_sample - initial_sample;
  m->samp = initial_sample;
  id = m->id;

  if (num > 0)
    extend_with_zeros(cp, initial_sample, num, cp->edit_ctr, "drag mark");
      /* at this point, old mark pointer is irrelevant (it lives in the previous edit history list) */
      /*   but since the ripple didn't touch it, we need to move it forward to reflect the insertion */
  else 
    if (num < 0)
      {
	new_m = map_over_marks_with_int(cp, find_mark_id_1, id, READ_FORWARD);
	new_m->samp = initial_sample;
	delete_samples(mark_final_sample, -num, cp, cp->edit_ctr);
      }

  if (num != 0) 
    {
      new_m = map_over_marks_with_int(cp, find_mark_id_1, id, READ_FORWARD);
      new_m->samp = mark_final_sample;
      update_graph(cp);
    }
}


void finish_moving_mark(chan_info *cp, mark *m) /* button release called from snd-chn.c */
{
  if (watching_mouse) cancel_mark_watch(cp);
  if ((m->sync != 0) && (mark_sd))
    {
      int i;
      if (Xen_hook_has_list(mark_hook))
	for (i = 0; i < mark_sd->mark_ctr; i++)
	  run_mark_hook(mark_sd->chans[i], mark_sd->marks[i]->id, MARK_RELEASE);
      if (mark_control_clicked)
	{
	  for (i = mark_sd->mark_ctr - 1; i >= 0; i--)
	    {
	      /* do the edits in reverse order on the assumption that marks sharing a channel were ordered to begin with,
	       *   so they'll happen in reverse order here, so the lower sample edits in rippling won't affect the higher
	       */
	      mark *sdm;
	      sdm = mark_sd->marks[i];
	      edit_dragged_mark(mark_sd->chans[i], sdm, mark_sd->initial_samples[i]);
	    }
	  finalize_md_context(mark_sd->mark_ctr);
	}
      for (i = 0; i < mark_sd->mark_ctr; i++)
	if (mark_sd->chans[i])
	  {
	    int j;
	    sort_marks(mark_sd->chans[i]); /* resort marks in case movement scrambled them */
	    for (j = i + 1; j < mark_sd->mark_ctr; j++)    /* only sort each channel once */
	      if (mark_sd->chans[j] == mark_sd->chans[i])
		mark_sd->chans[j] = NULL;
	  }
    }
  else 
    {
      run_mark_hook(cp, m->id, MARK_RELEASE);
      if (mark_control_clicked) 
	{
	  edit_dragged_mark(cp, m, mark_initial_sample);
	  finalize_md_context(1);
	}
      sort_marks(cp);
    }
  if (mark_sd) mark_sd = free_syncdata(mark_sd);
}


void play_syncd_mark(chan_info *cp, mark *m)
{
  syncdata *sd;
  sd = gather_syncd_marks(m->sync);
  if ((sd) && (sd->mark_ctr > 0))
    play_channels(sd->chans, sd->mark_ctr, sd->initial_samples, NULL, IN_BACKGROUND, 
		  C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), false, "play sync'd mark", 0);
  if (sd) free_syncdata(sd);
}


static void make_mark_graph(chan_info *cp, mus_long_t initial_sample, mus_long_t current_sample, int which)
{
#if (!USE_NO_GUI)
  snd_info *sp;
  int j = 0;
  mus_long_t i, k, samps;
  axis_info *ap;
  double samples_per_pixel, x;  
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time = 0.0, cur_srate;

  sp = cp->sound;
  ap = cp->axis;
  cur_srate = (double)snd_srate(sp);
  ap->losamp = (mus_long_t)(ap->x0 * cur_srate);
  if (ap->losamp < 0) ap->losamp = 0;
  if (ap->x0 != ((double)(ap->losamp) / cur_srate)) ap->losamp++;
  start_time = (double)(ap->losamp) / cur_srate;
  ap->hisamp = (mus_long_t)(ap->x1 * cur_srate);
  if ((ap->losamp == 0) && (ap->hisamp == 0)) return;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  if ((x_start == x_end) && (samps > 10)) return; /* must be too-tiny graph */
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (mus_float_t)pixels);

  /* this is assuming one dragged mark per channel */
  if ((samples_per_pixel < 5.0) && 
      (samps < POINT_BUFFER_SIZE))
    {
      double incr;
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
      if (!sf) return;
      incr = (double)1.0 / cur_srate;
      if (current_sample < initial_sample)
	{
	  for (j = 0, i = ap->losamp, x = start_time; i <= ap->hisamp; i++, j++, x += incr)
	    {
	      if (i == current_sample) 
		for (k = current_sample; k < initial_sample; k++) 
		  read_sample(sf);
	      set_grf_point(grf_x(x, ap), j, grf_y(read_sample(sf), ap));
	    }
	}
      else
	{
	  for (j = 0, i = ap->losamp, x = start_time; i <= ap->hisamp; i++, j++, x += incr)
	    {
	      double samp;
	      if ((i < initial_sample) || (i >= current_sample)) 
		samp = read_sample(sf);
	      else samp = 0.0;
	      set_grf_point(grf_x(x, ap), j, grf_y(samp, ap));
	    }
	}
      erase_and_draw_grf_points(mark_movers[which], cp, j);
    }
  else
    {
      mus_float_t ymin, ymax;
      int xi;
      double xf;
      if (peak_env_usable(cp, samples_per_pixel, ap->hisamp, false, cp->edit_ctr, (samps > PEAK_ENV_CUTOFF)))
	{
	  /* needs two sets of pointers and a frame within the amp env:
	   *   sample given mark edit: i and xk
	   *   sample within (original, unedited) amp env: ii and xki (xf)
	   *   frame bounds within amp env if relevant: k and kk
	   * this is confusing code!
	   */
	  double step, xk, xki;
	  mus_long_t ii;
	  peak_env_info *ep;
	  ep = cp->edits[cp->edit_ctr]->peak_env;
	  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
	  xf = (double)(ap->losamp) / (double)(ep->samps_per_bin);
	  j = 0;
	  x = ap->x0;
	  xi = grf_x(x, ap);
	  i = ap->losamp;
	  ii = ap->losamp;
	  xk = i;
	  xki = (double)(ap->losamp);
	  while (i <= ap->hisamp)
	    {
	      mus_long_t kk;
	      k = (mus_long_t)xf;
	      kk = (mus_long_t)(xf + step);
	      if (((current_sample >= initial_sample) && 
		   (i >= initial_sample) && 
		   (i < current_sample)) ||
		  (kk >= ep->peak_env_size))
		{
		  ymin = 0.0;
		  ymax = 0.0;
		}
	      else
		{
		  ymin = ep->fmax;
		  ymax = ep->fmin;
		  for (; k <= kk; k++)
		    {
		      if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
		      if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
		    }
		}
	      set_grf_points(xi++, j++,
			     grf_y(ymin, ap),
			     grf_y(ymax, ap));
	      xk += samples_per_pixel;
	      i = (mus_long_t)xk;
	      if ((current_sample < initial_sample) && 
		  (ii >= current_sample) && 
		  (ii < initial_sample))
		{
		  xf = (mus_float_t)((double)initial_sample / (mus_float_t)ep->samps_per_bin);
		  ii = initial_sample;
		  xki = (double)initial_sample;
		}
	      else 
		{
		  if ((current_sample < initial_sample) ||
		      (i >= current_sample) ||
		      (i < initial_sample))
		    {
		      xf += step;
		      xki += samples_per_pixel;
		      ii = (mus_long_t)xki;
		    }
		}
	    }
	  erase_and_draw_both_grf_points(mark_movers[which], cp, j);
	}
      else
	{
	  mus_float_t msamp;
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (!sf) return;
	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = MIN_INIT;
	  ymax = MAX_INIT;
	  if (current_sample < initial_sample) 
	    {
	      for (i = ap->losamp; i <= ap->hisamp; i++)
		{
		  if (i == current_sample) 
		    for (k = current_sample; k < initial_sample; k++) 
		      read_sample(sf);
		  msamp = read_sample(sf);
		  if (msamp > ymax) ymax = msamp;
		  if (msamp < ymin) ymin = msamp;
		  xf += 1.0;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
				     grf_y(ymin, ap), 
				     grf_y(ymax, ap));
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = MIN_INIT;
		      ymax = MAX_INIT;
		    }
		}
	    }
	  else
	    {
	      for (i = ap->losamp, xf = 0.0; i <= ap->hisamp; i++)
		{
		  if ((i < initial_sample) || (i >= current_sample))
		    msamp = read_sample(sf);
		  else msamp = 0.0;
		  if (msamp > ymax) ymax = msamp;
		  if (msamp < ymin) ymin = msamp;
		  xf += 1.0;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
				     grf_y(ymin, ap), 
				     grf_y(ymax, ap));
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = MIN_INIT;
		      ymax = MAX_INIT;
		    }
		}
	    }
	  erase_and_draw_both_grf_points(mark_movers[which], cp, j);
	}
    }
  free_snd_fd(sf);
#endif
}


#if (!USE_NO_GUI)

/* -------------------------------- display mark -------------------------------- */

static void show_mark(chan_info *cp, mark *mp, bool show)
{
  int top, cx, y0, y1;
  axis_info *ap;
  graphics_context *ax;

#if USE_MOTIF
  #define STRING_Y_OFFSET 6
#else
  #define STRING_Y_OFFSET -6
#endif

  ap = cp->axis;
  top = ap->y_axis_y1;
  y1 = top;
  y0 = ap->y_axis_y0;
  if (mp->name) top += 10;
  cx = grf_x((double)(mp->samp) / (double)snd_srate(cp->sound), ap);

  ax = mark_tag_context(cp);
#if USE_GTK
#if (GTK_CHECK_VERSION(3, 89, 0))
  ss->cr = cp->graph_cr;
#else
  ss->cr = make_cairo(ax->wn);
#endif
#endif

  if (mp->name)
    {
      int len;
#if USE_MOTIF
      ax->current_font = ss->peaks_fontstruct->fid;
      XSetFont(ax->dp, ax->gc, ss->peaks_fontstruct->fid);
#else
      ax->current_font = PEAKS_FONT(ss);
#endif
      len = mark_name_width(mp->name);
      draw_string(ax, (int)(cx - 0.5 * len), y1 + STRING_Y_OFFSET, mp->name, strlen(mp->name));
    }

  fill_rectangle(ax,
		 cx - mark_tag_width(ss), top,
		 2 * mark_tag_width(ss), mark_tag_height(ss));
  draw_line(ax, cx, top + 4, cx, y0);

  if (mp->samp != cursor_sample(cp))
    fill_polygon(ax, 4,
		 cx, y0,
		 cx + play_arrow_size(ss), y0 + play_arrow_size(ss),
		 cx, y0 + 2 * play_arrow_size(ss),
		 cx, y0);
  mp->visible = show;
#if USE_GTK
#if (!GTK_CHECK_VERSION(3, 89, 0))
  free_cairo(ss->cr);
#endif
  ss->cr = NULL;
  copy_context(cp);
#endif
}

#else
/* no gui */
static void show_mark(chan_info *cp, mark *mp, bool show) {}
#endif



/* ---------------------------------------- mark object ---------------------------------------- */

typedef struct {
  int n;
} xen_mark;


#define Xen_to_xen_mark(arg) ((xen_mark *)Xen_object_ref(arg))

int xen_mark_to_int(Xen n)
{
  xen_mark *mx;
  mx = Xen_to_xen_mark(n);
  return(mx->n);
}


static Xen_object_type_t xen_mark_tag;

bool xen_is_mark(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_mark_tag));
}


static void xen_mark_free(xen_mark *v) {if (v) free(v);}

Xen_wrap_free(xen_mark, free_xen_mark, xen_mark_free)


static char *xen_mark_to_string(xen_mark *v)
{
  #define MARK_PRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(MARK_PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, MARK_PRINT_BUFFER_SIZE, "#<mark %d>", v->n);
  return(buf);
}


#if HAVE_FORTH || HAVE_RUBY
Xen_wrap_print(xen_mark, print_xen_mark, xen_mark_to_string)

static Xen g_xen_mark_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_mark_to_string "mark->string"
  Xen_check_type(xen_is_mark(obj), obj, 1, S_xen_mark_to_string, "a mark");
  vstr = xen_mark_to_string(Xen_to_xen_mark(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#else
#if HAVE_SCHEME
static s7_pointer g_xen_mark_to_string(s7_scheme *sc, s7_pointer args)
{
  char *vstr;
  s7_pointer result;
  vstr = xen_mark_to_string(Xen_to_xen_mark(s7_car(args)));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif
#endif


#if (!HAVE_SCHEME)
static bool xen_mark_equalp(xen_mark *v1, xen_mark *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static Xen equalp_xen_mark(Xen obj1, Xen obj2)
{
  if ((!(xen_is_mark(obj1))) || (!(xen_is_mark(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_mark_equalp(Xen_to_xen_mark(obj1), Xen_to_xen_mark(obj2))));
}
#endif


static xen_mark *xen_mark_make(int n)
{
  xen_mark *new_v;
  new_v = (xen_mark *)malloc(sizeof(xen_mark));
  new_v->n = n;
  return(new_v);
}

#if HAVE_SCHEME
  static s7_pointer g_mark_methods = NULL;
#endif

Xen new_xen_mark(int n)
{
  xen_mark *mx;
  if (n < 0)
    return(Xen_false);

  mx = xen_mark_make(n);
#if HAVE_SCHEME
  {
    s7_pointer m;
    m = Xen_make_object(xen_mark_tag, mx, 0, free_xen_mark);
    s7_c_object_set_let(m, g_mark_methods);
    return(m);
  }
#else
  return(Xen_make_object(xen_mark_tag, mx, 0, free_xen_mark));
#endif
}


#if HAVE_SCHEME
static bool s7_xen_mark_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_mark *)obj1)->n == ((xen_mark *)obj2)->n));
}


static Xen s7_xen_mark_copy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj;
  int id;
  mark *m;
  chan_info *cps[1] = {NULL};
  obj = s7_car(args);
  id = xen_mark_to_int(obj);
  m = find_mark_from_id(id, cps, AT_CURRENT_EDIT_POSITION);
  if (m)
    {
      mark *new_m;
      new_m = add_mark(m->samp, m->name, cps[0]);
      new_m->sync = m->sync;
      return(new_xen_mark(new_m->id));
    }
  return(obj);
}

static s7_pointer mark_to_let_func = NULL;
#endif


static void init_xen_mark(void)
{
#if HAVE_SCHEME
  g_mark_methods = s7_openlet(s7, s7_inlet(s7, s7_list(s7, 2, s7_make_symbol(s7, "object->let"), mark_to_let_func)));
  s7_gc_protect(s7, g_mark_methods);
  xen_mark_tag = s7_make_c_type(s7, "<mark>");
  s7_c_type_set_free(s7, xen_mark_tag, free_xen_mark);
  s7_c_type_set_equal(s7, xen_mark_tag, s7_xen_mark_equalp);
  s7_c_type_set_copy(s7, xen_mark_tag, s7_xen_mark_copy);
  s7_c_type_set_to_string(s7, xen_mark_tag, g_xen_mark_to_string);
#else
#if HAVE_RUBY
  xen_mark_tag = Xen_make_object_type("XenMark", sizeof(xen_mark));
#else
  xen_mark_tag = Xen_make_object_type("Mark", sizeof(xen_mark));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_mark_tag,   print_xen_mark);
  fth_set_object_dump(xen_mark_tag,      g_xen_mark_to_string);
  fth_set_object_equal(xen_mark_tag,     equalp_xen_mark);
  fth_set_object_free(xen_mark_tag,      free_xen_mark);
#endif

#if HAVE_RUBY
  rb_define_method(xen_mark_tag, "to_s",     Xen_procedure_cast print_xen_mark, 0);
  rb_define_method(xen_mark_tag, "eql?",     Xen_procedure_cast equalp_xen_mark, 1);
  rb_define_method(xen_mark_tag, "==",       Xen_procedure_cast equalp_xen_mark, 1);
  rb_define_method(xen_mark_tag, "to_str",   Xen_procedure_cast g_xen_mark_to_string, 0);
#endif
}
/* -------------------------------------------------------------------------------- */


static Xen g_integer_to_mark(Xen n)
{
  #define H_integer_to_mark "(" S_integer_to_mark " n) returns a mark object corresponding to the given integer"
  mark *m;
  Xen_check_type(Xen_is_integer(n), n, 1, S_integer_to_mark, "an integer");
  m = find_mark_from_id(Xen_integer_to_C_int(n), NULL, AT_CURRENT_EDIT_POSITION);
  if (m)
    return(new_xen_mark(m->id));
  return(Xen_false);
}


static Xen g_mark_to_integer(Xen n)
{
  #define H_mark_to_integer "(" S_mark_to_integer " id) returns the integer corresponding to the given mark object"
  Xen_check_type(xen_is_mark(n), n, 1, S_mark_to_integer, "a mark");
  return(C_int_to_Xen_integer(xen_mark_to_int(n)));
}


static Xen snd_no_such_mark_error(const char *caller, Xen id)
{
  Xen_error(Xen_make_error_type("no-such-mark"),
	    Xen_list_3(C_string_to_Xen_string("~A: no such mark ~A"),
		       C_string_to_Xen_string(caller),
		       id));
  return(Xen_false);
}


typedef enum {MARK_SAMPLE, MARK_NAME, MARK_SYNC, MARK_HOME} mark_field_t;

static Xen mark_get(Xen n, mark_field_t fld, Xen pos_n, const char *caller)
{
  int pos;
  chan_info *ncp[1];
  mark *m = NULL;

  pos = (Xen_is_integer(pos_n)) ? Xen_integer_to_C_int(pos_n) : AT_CURRENT_EDIT_POSITION;

  m = find_mark_from_id(Xen_mark_to_C_int(n), ncp, pos);
  if (!m) 
    return(snd_no_such_mark_error(caller, n));

  switch (fld)
    {
    case MARK_SAMPLE: 
      return(C_llong_to_Xen_llong(m->samp)); 

    case MARK_SYNC:   
      return(C_int_to_Xen_integer(m->sync)); 

    case MARK_NAME:   
      if (m->name) 
	return(C_string_to_Xen_string(m->name)); 
      return(C_string_to_Xen_string("")); 

    case MARK_HOME:   
      return(Xen_list_2(C_int_to_Xen_sound((ncp[0]->sound)->index),
			C_int_to_Xen_integer(ncp[0]->chan))); 
    }
  return(Xen_false);
}


static Xen mark_set(Xen mark_n, Xen val, mark_field_t fld, const char *caller)
{
  chan_info *cp[1];
  mark *m;

  m = find_mark_from_id(Xen_mark_to_C_int(mark_n), cp, AT_CURRENT_EDIT_POSITION);
  if (!m) 
    return(snd_no_such_mark_error(caller, mark_n));

  switch (fld)
    {
    case MARK_SAMPLE: 
      m->samp = mus_oclamp(0, 
			   (Xen_is_llong(val)) ? Xen_llong_to_C_llong(val) : 0,
			   current_samples(cp[0]));
      sort_marks(cp[0]); /* update and re-sort current mark list */
      run_mark_hook(cp[0], m->id, MARK_MOVE);
      update_graph(cp[0]);
      break;

    case MARK_SYNC: 
      if (Xen_is_integer(val))
	set_mark_sync(m, Xen_integer_to_C_int(val));
      else set_mark_sync(m, (int)Xen_boolean_to_C_bool(val));
      break;

    case MARK_NAME:
      if (m->name) free(m->name);
      if (Xen_is_false(val))
	m->name = NULL;
      else m->name = mus_strdup(Xen_string_to_C_string(val));
      update_graph(cp[0]);
      break;

    default:
      break;
    }
  return(val);
}


static Xen g_is_mark(Xen id_n)
{
  #define H_is_mark "(" S_is_mark " id): " PROC_TRUE " if mark is active"
  if (xen_is_mark(id_n))
    return(C_bool_to_Xen_boolean(find_mark_from_id(Xen_mark_to_C_int(id_n), NULL, AT_CURRENT_EDIT_POSITION)));
  return(Xen_false);
}


static Xen g_mark_sample(Xen mark_n, Xen pos_n) 
{
  #define H_mark_sample "(" S_mark_sample " id :optional pos): mark's location (sample number) at edit history pos"
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_mark_sample, "a mark");
  Xen_check_type(Xen_is_integer_or_unbound(pos_n), pos_n, 2, S_mark_sample, "an integer");
  return(mark_get(mark_n, MARK_SAMPLE, pos_n, S_mark_sample));
}

static Xen g_set_mark_sample(Xen mark_n, Xen samp_n) 
{
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_set S_mark_sample, "a mark");
  Xen_check_type(Xen_is_llong(samp_n) || !Xen_is_bound(samp_n), samp_n, 2, S_set S_mark_sample, "an integer");
  return(mark_set(mark_n, samp_n, MARK_SAMPLE, S_set S_mark_sample));
}


Xen g_mark_sync(Xen mark_n)
{
  #define H_mark_sync "(" S_mark_sync " id): mark's sync value (default: 0)"
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_mark_sync, "a mark");
  return(mark_get(mark_n, MARK_SYNC, Xen_undefined, S_mark_sync));
}

Xen g_set_mark_sync(Xen mark_n, Xen sync_n)
{
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_set S_mark_sync, "a mark");
  Xen_check_type(Xen_is_integer_or_boolean(sync_n), sync_n, 2, S_set S_mark_sync, "an integer");
  return(mark_set(mark_n, sync_n, MARK_SYNC, S_set S_mark_sync));
}


static Xen g_mark_name(Xen mark_n) 
{
  #define H_mark_name "(" S_mark_name " id): mark's name"
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_mark_name, "a mark");
  return(mark_get(mark_n, MARK_NAME, Xen_undefined, S_mark_name));
}

static Xen g_set_mark_name(Xen mark_n, Xen name) 
{
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_set S_mark_name, "a mark");
  Xen_check_type(Xen_is_string(name) || Xen_is_false(name), name, 2, S_set S_mark_name, "a string");
  return(mark_set(mark_n, name, MARK_NAME, S_set S_mark_name));
}


static Xen g_mark_sync_max(void) 
{
  #define H_mark_sync_max "(" S_mark_sync_max "): max mark sync value seen so far"
  return(C_int_to_Xen_integer(mark_sync_max()));
}


static Xen g_mark_home(Xen mark_n)
{
  #define H_mark_home "(" S_mark_home " id): the sound (index) and channel that hold mark id"
  Xen_check_type(xen_is_mark(mark_n), mark_n, 1, S_mark_home, "a mark");
  return(mark_get(mark_n, MARK_HOME, Xen_undefined, S_mark_home));
}


static Xen g_find_mark(Xen samp_n, Xen snd, Xen chn_n, Xen edpos) 
{
  #define H_find_mark "(" S_find_mark " samp-or-name :optional snd chn edpos): \
find the mark in snd's channel chn at samp (if a number) or with the given name (if a string); return the mark or " PROC_FALSE " if no mark found."

  mark **mps;
  int pos;
  chan_info *cp = NULL;

  Xen_check_type((Xen_is_llong(samp_n) || Xen_is_string(samp_n) || (Xen_is_false(samp_n))), samp_n, 1, S_find_mark, "an integer or string or " PROC_FALSE);
  Snd_assert_channel(S_find_mark, snd, chn_n, 2); 

  cp = get_cp(snd, chn_n, S_find_mark);
  if (!cp) return(Xen_false);
  pos = to_c_edit_position(cp, edpos, S_find_mark, 4);

  mps = cp->edits[pos]->marks;
  if (mps)
    {
      int i;
      mus_long_t samp = 0;
      const char *name = NULL;
      if (Xen_is_string(samp_n))
	name = Xen_string_to_C_string(samp_n);
      else 
	{
	  if (Xen_is_llong(samp_n))
	    samp = Xen_llong_to_C_llong(samp_n);
	}
      if (name)
	{
	  for (i = 0; i <= cp->edits[pos]->mark_ctr; i++) 
	    if ((mps[i]) && 
		(mus_strcmp(name, mps[i]->name)))
	      return(new_xen_mark(mps[i]->id));
	}
      else
	{
	  for (i = 0; i <= cp->edits[pos]->mark_ctr; i++)
	    if ((mps[i]) && 
		(mps[i]->samp == samp)) 
	      return(new_xen_mark(mps[i]->id));
	}
    }
  return(Xen_false);
}

static Xen g_add_mark_1(Xen samp_n, Xen snd, Xen chn_n, Xen name, Xen sync, bool check_sample, const char *caller) 
{
  #define H_add_mark "(" S_add_mark " samp :optional snd chn name (sync 0)): add a mark at sample samp returning the mark."
  mark *m = NULL;
  chan_info *cp;
  mus_long_t loc = 0;
  int msync = 0;
  const char *mname = NULL;

  Xen_check_type(Xen_is_llong(samp_n) || !Xen_is_bound(samp_n), samp_n, 1, caller, "an integer");
  Xen_check_type(Xen_is_string_or_unbound(name) || Xen_is_false(name), name, 4, caller, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(sync), sync, 5, caller, "an integer");
  Snd_assert_channel(caller, snd, chn_n, 2);

  cp = get_cp(snd, chn_n, caller);
  if (!cp) return(Xen_false);

  if (Xen_is_llong(samp_n)) loc = Xen_llong_to_C_llong(samp_n);

  if ((!check_sample) &&
      (loc >= current_samples(cp)))
    return(Xen_false);

  if ((loc < 0) || 
      (loc >= current_samples(cp)))
    Xen_error(Xen_make_error_type("no-such-sample"),
	      Xen_list_2(C_string_to_Xen_string(S_add_mark ": no such sample, ~A"), 
			 samp_n));

  if (Xen_is_string(name)) mname = Xen_string_to_C_string(name);
  if (Xen_is_integer(sync)) msync = Xen_integer_to_C_int(sync);

  m = add_mark(loc, mname, cp);
  if (m)
    {
      if (msync != 0) set_mark_sync(m, msync);
      update_graph(cp);
      return(new_xen_mark(m->id));
    }
  return(Xen_false);
}


static Xen g_add_mark(Xen samp_n, Xen snd, Xen chn_n, Xen name, Xen sync)
{
  return(g_add_mark_1(samp_n, snd, chn_n, name, sync, true, S_add_mark));
}


static Xen g_add_mark_unchecked(Xen samp_n, Xen snd, Xen chn_n, Xen name, Xen sync)
{
  #define H_add_mark_unchecked "(add-mark! samp :optional snd chn name (sync 0)): add a mark at sample samp returning the mark.\
Unlike add-mark, add-mark! does not check for an invalid sample number."

  return(g_add_mark_1(samp_n, snd, chn_n, name, sync, false, S_add_mark "!"));
}


static Xen g_delete_mark(Xen id_n) 
{
  #define H_delete_mark "(" S_delete_mark " id): delete mark id"
  chan_info *cp[1];
  mark *m;
  int id;

  Xen_check_type(xen_is_mark(id_n), id_n, 1, S_delete_mark, "a mark");
  id = Xen_mark_to_C_int(id_n);

  m = find_mark_from_id(id, cp, AT_CURRENT_EDIT_POSITION);
  if (!m) 
    return(snd_no_such_mark_error(S_delete_mark, id_n));

  if (delete_mark_id(id, cp[0]))
    update_graph(cp[0]);
  else return(snd_no_such_mark_error(S_delete_mark, id_n));

  return(id_n);
}


static Xen g_delete_marks(Xen snd, Xen chn_n) 
{
  #define H_delete_marks "(" S_delete_marks " :optional snd chn): delete all marks in snd's channel chn"
  chan_info *cp;
  Snd_assert_channel(S_delete_marks, snd, chn_n, 1);
  cp = get_cp(snd, chn_n, S_delete_marks);
  if (!cp) return(Xen_false);
  delete_marks(cp);
  return(Xen_false);
}


static Xen int_array_to_mark_list(int *arr, int i, int len)
{
  if (i < len)
    return(Xen_cons(new_xen_mark(arr[i]), int_array_to_mark_list(arr, i + 1, len)));
  return(Xen_cons(new_xen_mark(arr[i]), Xen_empty_list));
}


static int *syncd_marks(int sync)
{
  syncdata *sd;
  int *ids;
  int i;
  sd = make_syncdata(sync);
  for_each_normal_chan_with_void(gather_chan_syncd_marks, (void *)sd);
  ids = (int *)calloc(1 + sd->mark_ctr, sizeof(int));
  ids[0] = sd->mark_ctr;
  for (i = 0; i < sd->mark_ctr; i++) ids[i + 1] = sd->marks[i]->id;
  free_syncdata(sd);
  return(ids);
}


static Xen g_syncd_marks(Xen sync)
{
  #define H_syncd_marks "(" S_syncd_marks " sync): list of marks that share a given sync value (" S_mark_sync ")"
  int *ids;
  Xen res;

  Xen_check_type(Xen_is_integer(sync), sync, 1, S_syncd_marks, "an integer");
  ids = syncd_marks(Xen_integer_to_C_int(sync));

  if (!ids) return(Xen_empty_list);
  if (ids[0] == 0) {free(ids); return(Xen_empty_list);}

  res = int_array_to_mark_list(ids, 1, ids[0]);

  free(ids);
  return(res);
}


static Xen g_mark_tag_width(void) {return(C_int_to_Xen_integer(mark_tag_width(ss)));}

static Xen g_set_mark_tag_width(Xen val) 
{
  #define H_mark_tag_width "(" S_mark_tag_width "): width (pixels) of mark tags (10)"
  int width;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mark_tag_width, "an integer"); 
  width = mus_iclamp(0, Xen_integer_to_C_int(val), LOTSA_PIXELS);
  set_mark_tag_width(width);
  for_each_normal_chan(update_graph);
  return(C_int_to_Xen_integer(mark_tag_width(ss)));
}


static Xen g_mark_tag_height(void) {return(C_int_to_Xen_integer(mark_tag_height(ss)));}

static Xen g_set_mark_tag_height(Xen val) 
{
  #define H_mark_tag_height "(" S_mark_tag_height "): height (pixels) of mark tags (4)"
  int height;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mark_tag_height, "an integer"); 
  height = mus_iclamp(0, Xen_integer_to_C_int(val), LOTSA_PIXELS);
  set_mark_tag_height(height);
  for_each_normal_chan(update_graph);
  return(C_int_to_Xen_integer(mark_tag_height(ss)));
}


static int *channel_marks(chan_info *cp, int pos)
{
  int *ids = NULL;
  ed_list *ed;
  ed = cp->edits[pos];
  if (ed->marks)
    {
      mark **mps;
      int marks;
      mps = ed->marks;
      marks = ed->mark_ctr;
      if (mps)
	{
	  int i;
	  ids = (int *)calloc(marks + 2, sizeof(int)); /* 1 for size, 1 because mark_ctr is current count */
	  ids[0] = marks + 1;
	  for (i = 0; i <= marks; i++) 
	    ids[i + 1] = mps[i]->id;
	}
    }
  return(ids);
}


static Xen g_marks(Xen snd, Xen chn_n, Xen pos_n) 
{
  #define H_marks "(" S_marks " :optional snd chn edpos): list of marks in snd/chn at edit history position pos. \
mark list is: channel given: (id id ...), snd given: ((id id) (id id ...)), neither given: (((id ...) ...) ...)."

  chan_info *cp;
  snd_info *sp;
  Xen res1 = Xen_empty_list;

  Snd_assert_channel(S_marks, snd, chn_n, 0);

  if (Xen_is_integer(snd) || xen_is_sound(snd))
      {
	int *ids;
	if (Xen_is_integer(chn_n))
	  {
	    int pos;
	    Xen res;
	    cp = get_cp(snd, chn_n, S_marks);
	    if (!cp) return(Xen_false);
	    if (Xen_is_integer(pos_n)) 
	      {
		pos = Xen_integer_to_C_int(pos_n);
		if (pos == AT_CURRENT_EDIT_POSITION)
		  pos = cp->edit_ctr;
		if ((pos < 0) || (pos >= cp->edit_size) || (!cp->edits[pos]))
		  Xen_error(Xen_make_error_type("no-such-edit"),
			    Xen_list_2(C_string_to_Xen_string(S_marks ": no such edit, ~A"),
				       pos_n));
	      }
	    else pos = cp->edit_ctr;
	    ids = channel_marks(cp, pos);
	    if (!ids) return(Xen_empty_list);
	    if (ids[0] == 0) 
	      {
		free(ids); 
		return(Xen_empty_list);
	      }
	    res = int_array_to_mark_list(ids, 1, ids[0]);
	    free(ids);
	    return(res);
	  }
	else
	  {
	    int i;
	    sp = get_sp(snd);
	    if (!sp) 
	      return(snd_no_such_sound_error(S_marks, snd));
	    for (i = (int)sp->nchans - 1; i >= 0; i--)
	      {
		cp = sp->chans[i];
		ids = channel_marks(cp, cp->edit_ctr);
		if ((!ids) || (ids[0] == 0))
		  res1 = Xen_cons(Xen_empty_list, res1);
		else res1 = Xen_cons(int_array_to_mark_list(ids, 1, ids[0]), 
				     res1);
		if (ids) free(ids);
	      }
	  }
      }
  else
    {
      int j;
      /* all marks */
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res1 = Xen_cons(g_marks(C_int_to_Xen_integer(j), Xen_undefined, Xen_undefined), 
			    res1);
	}
    }
  return(res1);
}


/* ---------------- saved marks ----------------
 *
 * upon restoration, pre-existing marks can collide both in mark-id and mark-sync
 *   with the saved marks, so these need to be fixed-up as they are encountered.
 *   Also mark-sync-max needs to reflect the newly chosen sync values.
 *
 * We used to try to save the entire mark history, but I can't see how that can work
 *   in general -- the save-marks output can be loaded at any time, so we can't
 *   rely on anything in regard to the edit history.
 */

static bool find_any_marks(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return((ed->marks) && (ed->mark_ctr >= 0)); /* initialized to -1 -- 0 is first mark */
}

typedef struct {
  FILE *fd;
  int size;
  int *syncs;
} save_mark_info;

#define SYNC_BASE "_sync_"
#define SYNC_NAME_SIZE 32

static char *mark_sync_name(int cur_sync)
{
  char *result;
  result = (char *)calloc(SYNC_NAME_SIZE, sizeof(char));
  snprintf(result, SYNC_NAME_SIZE, "%s%d", SYNC_BASE, cur_sync);
  return(result);
}


static char *map_mark_sync(chan_info *cp, mark *m, save_mark_info *sv)
{
  /* if sync 0 just return "0",  else if already in syncs array, use _sync_n_ as name,
   *   else open a let, declare _sync_n_ with new safe value,
   *   add current int value to syncs array (can't assume unshared here will not collide later)
   */

  int i, cur_sync;
  cur_sync = m->sync;
  if (cur_sync == 0)
    return(mus_strdup("0"));

  if (sv->size > 0)
    for (i = 0; i < sv->size; i++)
      if (cur_sync == sv->syncs[i])
	return(mark_sync_name(cur_sync));

  /* add sync to current set (protect against later collisions, take current shared-syncs into account) */
  if (sv->size == 0)
    sv->syncs = (int *)calloc(1, sizeof(int));
  else sv->syncs = (int *)realloc(sv->syncs, (sv->size + 1) * sizeof(int));
  sv->syncs[sv->size++] = cur_sync;

#if (!HAVE_FORTH)
  fprintf(sv->fd, "      ");
  for (i = 0; i < sv->size - 1; i++) fprintf(sv->fd, "  "); /* previous lets */
#endif

#if HAVE_SCHEME
  fprintf(sv->fd, "(let ((%s%d (+ (mark-sync-max) 1)))\n", SYNC_BASE, cur_sync);
#endif

#if HAVE_RUBY
  fprintf(sv->fd, "begin\n        %s%d = mark_sync_max + 1\n", SYNC_BASE, cur_sync);
#endif

#if HAVE_FORTH
  fprintf(sv->fd, "mark-sync-max 1 + value %s%d\n", SYNC_BASE, cur_sync);
#endif

  return(mark_sync_name(cur_sync));
}


static mark *save_mark(chan_info *cp, mark *m, void *info)
{
  /* we're called within "sound_block" where "sfile" = current sound index */

  save_mark_info *sv = (save_mark_info *)info;
  char *mapped_sync;
  int i;

  mapped_sync = map_mark_sync(cp, m, sv);

  fprintf(sv->fd, "      ");
  for (i = 0; i < sv->size; i++) fprintf(sv->fd, "  "); /* lets */

  /* here we need to use the "!" form of add-mark to ignore bad sample numbers -- these can come about during
   *   the save-state process when redoable edits causing extensions are undone before marks are added,
   *   possibly to the extended portion -- we'll simply drop those marks, rather than wrecking the restore
   *   process with an error.
   */

#if HAVE_SCHEME
  if (m->name)
    fprintf(sv->fd, "(add-mark! %" print_mus_long " sfile %d \"%s\" %s)\n", m->samp, cp->chan, m->name, mapped_sync);
  else fprintf(sv->fd, "(add-mark! %" print_mus_long " sfile %d #f %s)\n", m->samp, cp->chan, mapped_sync);
#endif

#if HAVE_RUBY
  if (m->name)
    fprintf(sv->fd, "add_mark!(%" print_mus_long ", sfile, %d, \"%s\", %s)\n", m->samp, cp->chan, m->name, mapped_sync);
  else fprintf(sv->fd, "add_mark!(%" print_mus_long ", sfile, %d, false, %s)\n", m->samp, cp->chan, mapped_sync);
#endif

#if HAVE_FORTH
  if (m->name)
    fprintf(sv->fd, "%" print_mus_long " sfile %d \"%s\" %s add-mark! drop\n", m->samp, cp->chan, m->name, mapped_sync);
  else fprintf(sv->fd, "%" print_mus_long " sfile %d #f %s add-mark! drop\n", m->samp, cp->chan, mapped_sync);
#endif

  free(mapped_sync);
  return(NULL); /* returning a mark here breaks out of the map mark loop */
}


void save_mark_list(FILE *fd, chan_info *cp, bool all_chans)
{
  /* used in save-marks (below) and the edit history stuff in snd-edits.c
   *   changed 23-Sep-06 -- restore-marks is now a no-op, and no attempt is made to save the entire mark history.
   */

  save_mark_info *sv;
  if ((!all_chans) && (!(find_any_marks(cp)))) return; /* in the sound (all_chans) case, this has been checked already */
  sv = (save_mark_info *)calloc(1, sizeof(save_mark_info));
  sv->fd = fd;
  sv->size = 0;
  sv->syncs = NULL;
  if (all_chans)
    {
      uint32_t i;
      snd_info *sp;
      sp = cp->sound;
      for (i = 0; i < sp->nchans; i++)
	map_over_marks(sp->chans[i], save_mark, (void *)sv, READ_FORWARD);
    }
  else map_over_marks(cp, save_mark, (void *)sv, READ_FORWARD);
  if (sv->size > 0)
    {
#if HAVE_SCHEME || HAVE_RUBY
      int i;
#endif
      fprintf(fd, "      ");

#if HAVE_SCHEME
      for (i = 0; i < sv->size; i++) fprintf(fd, ")");
#endif

#if HAVE_RUBY
      for (i = 0; i < sv->size; i++) fprintf(fd, "end\n");
#endif

      fprintf(fd, "\n");
    }
  if (sv->syncs) free(sv->syncs);
  free(sv);
}  


static Xen g_save_marks(Xen snd, Xen filename)
{
  #define H_save_marks "(" S_save_marks " :optional snd (filename \"<snd-file-name>.marks\")): save snd's marks in filename. \
The saved file is " Xen_language " code, so to restore the marks, load that file."

  snd_info *sp;
  Xen res = Xen_false;

  Snd_assert_sound(S_save_marks, snd, 1);
  Xen_check_type(Xen_is_string_or_unbound(filename), filename, 2, S_save_marks, "a string");  

  sp = get_sp(snd);
  if (!sp) 
    return(snd_no_such_sound_error(S_save_marks, snd));

  if (map_over_sound_chans(sp, find_any_marks)) /* are there any marks? */
    {
      char *newname = NULL;
      FILE *fd;
      if (Xen_is_string(filename))
	newname = mus_strdup(Xen_string_to_C_string(filename));
      else
	{
	  int i, len;
	  len = strlen(sp->filename);
	  newname = (char *)calloc(len + 7, sizeof(char));
	  strcopy(newname, sp->filename, len + 7);
	  for (i = len - 1; i > 0; i--) 
	    if (newname[i] == '.') 
	      break;
	  if (i > 0) len = i;
	  newname[len] = '\0';
	  strcat(newname, ".marks");
	}
      fd = FOPEN(newname, "w");
      if (!fd)
	{
	  Xen lname;
	  lname = C_string_to_Xen_string(newname);
	  free(newname);
	  Xen_error(Xen_make_error_type("cannot-save"),
		    Xen_list_3(C_string_to_Xen_string(S_save_marks ": can't save ~S, ~A"),
			       lname,
			       C_string_to_Xen_string(snd_open_strerror())));
	}
      else
	{
#if HAVE_FORTH
	  fprintf(fd, "#f value sfile\n");
#endif
	  open_save_sound_block(sp, fd, false);   /* find-sound or open it with enclosing let */
	  save_mark_list(fd, sp->chans[0], true); /* true -> save all chans, matching cross-channel syncs */
	  close_save_sound_block(fd, false);      /* close the let form */
	  snd_fclose(fd, newname);
	  res = C_string_to_Xen_string(newname);
	}
      free(newname);
    }
  return(res);
}


static Xen g_mark_properties(Xen n)
{
  mark *m;
  #define H_mark_properties "(" S_mark_properties " id):  A property list associated with the given mark."

  Xen_check_type(xen_is_mark(n), n, 1, S_mark_properties, "a mark");

  m = find_mark_from_id(Xen_mark_to_C_int(n), NULL, AT_CURRENT_EDIT_POSITION);
  if (!m)
    return(snd_no_such_mark_error(S_mark_properties, n));

  if (!(Xen_is_vector(m->properties)))
    {
      m->properties = Xen_make_vector(1, Xen_empty_list);
      m->properties_gc_loc = snd_protect(m->properties);
    }
  return(Xen_vector_ref(m->properties, 0));
}


static Xen g_set_mark_properties(Xen n, Xen val)
{
  mark *m;
  Xen_check_type(xen_is_mark(n), n, 1, S_mark_properties, "a mark");

  m = find_mark_from_id(Xen_mark_to_C_int(n), NULL, AT_CURRENT_EDIT_POSITION);
  if (!m)
    return(snd_no_such_mark_error(S_set S_mark_properties, n));

  if (!(Xen_is_vector(m->properties)))
    {
      m->properties = Xen_make_vector(1, Xen_empty_list);
      m->properties_gc_loc = snd_protect(m->properties);
    }

  Xen_vector_set(m->properties, 0, val);
  return(Xen_vector_ref(m->properties, 0));
}


static Xen g_mark_property(Xen key, Xen id) 
{
  #define H_mark_property "(" S_mark_property " key id) returns the value associated with 'key' in the given mark's property list, or " PROC_FALSE "."
  Xen_check_type(xen_is_mark(id), id, 2, S_mark_property, "a mark");
  return(Xen_assoc_ref(key, g_mark_properties(id)));
}

static Xen g_set_mark_property(Xen key, Xen id, Xen val) 
{
  Xen_check_type(xen_is_mark(id), id, 2, S_mark_property, "a mark");
  g_set_mark_properties(id, Xen_assoc_set(key, val, g_mark_properties(id)));
  return(val);
}



Xen_wrap_2_optional_args(g_mark_sample_w, g_mark_sample)
Xen_wrap_2_args(g_set_mark_sample_w, g_set_mark_sample)
Xen_wrap_1_arg(g_mark_sync_w, g_mark_sync)
Xen_wrap_2_args(g_set_mark_sync_w, g_set_mark_sync)
Xen_wrap_1_arg(g_mark_name_w, g_mark_name)
Xen_wrap_2_args(g_set_mark_name_w, g_set_mark_name)
Xen_wrap_no_args(g_mark_sync_max_w, g_mark_sync_max)
Xen_wrap_1_arg(g_mark_home_w, g_mark_home)
Xen_wrap_3_optional_args(g_marks_w, g_marks)
Xen_wrap_5_optional_args(g_add_mark_w, g_add_mark)
Xen_wrap_5_optional_args(g_add_mark_unchecked_w, g_add_mark_unchecked)
Xen_wrap_1_arg(g_delete_mark_w, g_delete_mark)
Xen_wrap_2_optional_args(g_delete_marks_w, g_delete_marks)
Xen_wrap_1_arg(g_syncd_marks_w, g_syncd_marks)
Xen_wrap_no_args(g_mark_tag_width_w, g_mark_tag_width)
Xen_wrap_1_arg(g_set_mark_tag_width_w, g_set_mark_tag_width)
Xen_wrap_no_args(g_mark_tag_height_w, g_mark_tag_height)
Xen_wrap_1_arg(g_set_mark_tag_height_w, g_set_mark_tag_height)
Xen_wrap_4_optional_args(g_find_mark_w, g_find_mark)
Xen_wrap_2_optional_args(g_save_marks_w, g_save_marks)
Xen_wrap_1_arg(g_is_mark_w, g_is_mark)
Xen_wrap_1_arg(g_integer_to_mark_w, g_integer_to_mark)
Xen_wrap_1_arg(g_mark_to_integer_w, g_mark_to_integer)
Xen_wrap_1_arg(g_mark_properties_w, g_mark_properties)
Xen_wrap_2_args(g_set_mark_properties_w, g_set_mark_properties)
Xen_wrap_2_args(g_mark_property_w, g_mark_property)
Xen_wrap_3_args(g_set_mark_property_w, g_set_mark_property)

#if HAVE_SCHEME
static s7_pointer acc_mark_tag_height(s7_scheme *sc, s7_pointer args) {return(g_set_mark_tag_height(s7_cadr(args)));}
static s7_pointer acc_mark_tag_width(s7_scheme *sc, s7_pointer args) {return(g_set_mark_tag_width(s7_cadr(args)));}

static s7_pointer s7_mark_to_let(s7_scheme *sc, s7_pointer args)
{
  /* this is called upon (object->let <mark>) */
  s7_pointer mark, env, val;
  mark = s7_car(args);
  env = s7_cadr(args);
  val = mark_get(mark, MARK_NAME, Xen_undefined, S_mark_name);
  if ((s7_is_string(val)) && 
      (s7_string_length(val) > 0))
    s7_varlet(sc, env, s7_make_symbol(sc, "name"), val);
  s7_varlet(sc, env, s7_make_symbol(sc, "sample"), mark_get(mark, MARK_SAMPLE, Xen_undefined, S_mark_sample));
  s7_varlet(sc, env, s7_make_symbol(sc, "sync"), mark_get(mark, MARK_SYNC, Xen_undefined, S_mark_sync));
  s7_varlet(sc, env, s7_make_symbol(sc, "home"), mark_get(mark, MARK_HOME, Xen_undefined, S_mark_home));
  return(env);
}
#endif

void g_init_marks(void)
{
  #define H_mark_drag_hook S_mark_drag_hook " (id): called when a mark is dragged"
  #define H_mark_hook S_mark_hook " (id snd chn reason): called when a mark added, deleted, or moved. \
'Reason' can be 0: add, 1: delete, 2: move, 3: delete all marks"

#if HAVE_SCHEME
  s7_pointer i, m, b, s, p, t, pl_im, pl_add, pl_i;
  i = s7_make_symbol(s7, "integer?");
  m = s7_make_symbol(s7, "mark?");
  b = s7_make_symbol(s7, "boolean?");
  s = s7_make_symbol(s7, "string?");
  p = s7_make_symbol(s7, "list?");
  t = s7_t(s7);
  pl_im = s7_make_signature(s7, 2, i, m);
  pl_add = s7_make_signature(s7, 6, s7_make_signature(s7, 2, m, b), i, t, t, s7_make_signature(s7, 2, s, b), i);
  pl_i = s7_make_circular_signature(s7, 0, 1, i);

  mark_to_let_func = s7_make_function(s7, "mark->let", s7_mark_to_let, 2, 0, false, "mark->let");
#endif

  init_xen_mark();

  mark_drag_hook = Xen_define_hook(S_mark_drag_hook, "(make-hook 'id)", 1, H_mark_drag_hook);
  mark_hook = Xen_define_hook(S_mark_hook, "(make-hook 'id 'snd 'chn 'reason)", 4, H_mark_hook); 

  Xen_define_typed_dilambda(S_mark_sample, g_mark_sample_w, H_mark_sample, S_set S_mark_sample, g_set_mark_sample_w, 1, 1, 2, 0, 
			    s7_make_signature(s7, 3, i, m, i), s7_make_signature(s7, 3, i, m, i)); 
  /* this is bad: the second arg to mark-sample is edit-position, but to set_mark_sample is the new sample! */
  Xen_define_typed_dilambda(S_mark_sync, g_mark_sync_w, H_mark_sync, S_set S_mark_sync, g_set_mark_sync_w, 1, 0, 2, 0, pl_im, 
			    s7_make_signature(s7, 3, i, m, s7_make_signature(s7, 2, i, b)));	    
  Xen_define_typed_dilambda(S_mark_name, g_mark_name_w, H_mark_name, S_set S_mark_name, g_set_mark_name_w, 1, 0, 2, 0, 
			    s7_make_signature(s7, 2, s, m), s7_make_signature(s7, 3, s, m, s));

  Xen_define_typed_procedure(S_mark_sync_max,   g_mark_sync_max_w,   0, 0, 0, H_mark_sync_max,    s7_make_signature(s7, 1, i));
  Xen_define_typed_procedure(S_mark_home,       g_mark_home_w,       1, 0, 0, H_mark_home,        s7_make_signature(s7, 2, p, m));
  Xen_define_typed_procedure(S_marks,           g_marks_w,           0, 3, 0, H_marks,            s7_make_circular_signature(s7, 1, 2, p, t));
  Xen_define_typed_procedure(S_add_mark,        g_add_mark_w,        0, 5, 0, H_add_mark,         pl_add);
  Xen_define_typed_procedure(S_add_mark "!",    g_add_mark_unchecked_w, 0, 5, 0, H_add_mark_unchecked, pl_add);
  Xen_define_typed_procedure(S_delete_mark,     g_delete_mark_w,     1, 0, 0, H_delete_mark,      s7_make_signature(s7, 2, t, m));
  Xen_define_typed_procedure(S_delete_marks,    g_delete_marks_w,    0, 2, 0, H_delete_marks,     s7_make_signature(s7, 3, b, t, t));
  Xen_define_typed_procedure(S_syncd_marks,     g_syncd_marks_w,     1, 0, 0, H_syncd_marks,      s7_make_signature(s7, 2, p, i));
  Xen_define_typed_procedure(S_find_mark,       g_find_mark_w,       1, 3, 0, H_find_mark,
			     s7_make_circular_signature(s7, 2, 3, s7_make_signature(s7, 2, m, b), s7_make_signature(s7, 2, i, s), t));
  Xen_define_typed_procedure(S_save_marks,      g_save_marks_w,      0, 2, 0, H_save_marks,       s7_make_signature(s7, 3, s, t, s));
  Xen_define_typed_procedure(S_is_mark,         g_is_mark_w,         1, 0, 0, H_is_mark,          s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_integer_to_mark, g_integer_to_mark_w, 1, 0, 0, H_integer_to_mark,  s7_make_signature(s7, 2, m, i));
  Xen_define_typed_procedure(S_mark_to_integer, g_mark_to_integer_w, 1, 0, 0, H_mark_to_integer,  s7_make_signature(s7, 2, i, m));

  Xen_define_typed_dilambda(S_mark_tag_width, g_mark_tag_width_w, H_mark_tag_width, S_set S_mark_tag_width, g_set_mark_tag_width_w, 0, 0, 1, 0, pl_i, pl_i);
  Xen_define_typed_dilambda(S_mark_tag_height, g_mark_tag_height_w, H_mark_tag_height, S_set S_mark_tag_height, g_set_mark_tag_height_w, 0, 0, 1, 0, pl_i, pl_i);
  Xen_define_typed_dilambda(S_mark_properties, g_mark_properties_w, H_mark_properties, S_set S_mark_properties, g_set_mark_properties_w, 1, 0, 2, 0,
			    s7_make_signature(s7, 2, p, m), s7_make_signature(s7, 3, p, m, p));
  Xen_define_typed_dilambda(S_mark_property, g_mark_property_w, H_mark_property, S_set S_mark_property, g_set_mark_property_w, 2, 0, 3, 0, 
			    s7_make_signature(s7, 3, t, t, m), s7_make_circular_signature(s7, 3, 4, t, t, m, t));

  #define H_draw_mark_hook S_draw_mark_hook " (id): called before a mark is drawn. \
If the hook returns " PROC_TRUE ", the mark is not drawn."

  draw_mark_hook = Xen_define_hook(S_draw_mark_hook, "(make-hook 'id)", 1, H_draw_mark_hook);

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->mark_tag_height_symbol, s7_make_function(s7, "[acc-" S_mark_tag_height "]", acc_mark_tag_height, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->mark_tag_width_symbol, s7_make_function(s7, "[acc-" S_mark_tag_width "]", acc_mark_tag_width, 2, 0, false, "accessor"));

  s7_symbol_set_documentation(s7, ss->mark_tag_height_symbol, "*mark-tag-height*: height (pixels) of mark tags (4)");
  s7_symbol_set_documentation(s7, ss->mark_tag_width_symbol, "*mark-tag-width*: width (pixels) of mark tags (10)");
#endif
}

