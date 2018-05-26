#include "snd.h"

static Xen save_hook;

static bool dont_save(snd_info *sp, const char *newname)
{
  Xen res = Xen_false;
  if (Xen_hook_has_list(save_hook))
    res = run_or_hook(save_hook,
		      Xen_list_2(C_int_to_Xen_sound(sp->index),
				 (newname) ? C_string_to_Xen_string(newname) : Xen_false),
		      S_save_hook);
  return(Xen_is_true(res));
}


void after_edit(chan_info *cp)
{
  reflect_edit_history_change(cp);
  reflect_enved_fft_change(cp);
  if ((cp->hookable == WITH_HOOK) &&
      (Xen_is_hook(cp->after_edit_hook)) && 
      (Xen_hook_has_list(cp->after_edit_hook)))
    run_hook(cp->after_edit_hook, Xen_empty_list, S_after_edit_hook);
}


void free_sound_list(chan_info *cp)
{
  if (cp)
    {
      if (cp->sounds)
	{
	  int i;
	  if ((cp->sound) && 
	      (cp->sound->playing)) 
	    stop_playing_sound_without_hook(cp->sound, PLAY_CLOSE);
	  for (i = 0; i < cp->sound_size; i++)
	    if (cp->sounds[i]) 
	      cp->sounds[i] = free_snd_data(cp->sounds[i]);
	  free(cp->sounds);
	  cp->sounds = NULL;
	}
      cp->sound_ctr = NOT_A_SOUND;
      cp->sound_size = 0;
    }
}


static void release_pending_sounds(chan_info *cp, int edit_ctr)
{
  /* look for buffers or temp files that are no longer reachable after pruning the edit tree */
  if ((cp) && (cp->sounds))
    {
      int i;
      if ((cp->sound) && 
	  (cp->sound->playing)) 
	stop_playing_sound_without_hook(cp->sound, PLAY_CLOSE);
      for (i = 0; i < cp->sound_size; i++)
	{
	  snd_data *sd;
	  sd = cp->sounds[i];
	  if (sd)
	    {
	      if (sd->edit_ctr >= edit_ctr)
		cp->sounds[i] = free_snd_data(sd);
	      else cp->sound_ctr = i;
	    }
	}
    }
}


#define EDIT_ALLOC_SIZE 32
/* EDIT_ALLOC_SIZE is the allocation amount (pointers) each time cp->sounds is (re)allocated */

void prepare_sound_list(chan_info *cp)
{
  cp->sound_ctr++;
  /* this is the only place the sound set is incremented */
  if (cp->sound_ctr >= cp->sound_size)
    {
      int i;
      cp->sound_size += EDIT_ALLOC_SIZE;
      cp->sounds = (snd_data **)realloc(cp->sounds, cp->sound_size * sizeof(snd_data *));
      for (i = cp->sound_ctr; i < cp->sound_size; i++) cp->sounds[i] = NULL;
    }
  if (cp->sounds[cp->sound_ctr]) 
    {
      if ((cp->sound) && (cp->sound->playing)) stop_playing_sound_without_hook(cp->sound, PLAY_CLOSE);
      cp->sounds[cp->sound_ctr] = free_snd_data(cp->sounds[cp->sound_ctr]);
    }
}


static int add_sound_file_to_edit_list(chan_info *cp, const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int chan)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_file(name, io, hdr, temp, cp->edit_ctr, chan);
  return(cp->sound_ctr);
}


static ed_list *free_ed_list(ed_list *ed, chan_info *cp);

static void prune_edits(chan_info *cp, int edpt)
{
  if (cp->edits[edpt]) 
    {
      int i;
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, edpt - 1);
      for (i = edpt; i < cp->edit_size; i++)
	cp->edits[i] = free_ed_list(cp->edits[i], cp);
      release_pending_sounds(cp, edpt);
    }
}


bool is_editable(chan_info *cp)
{
  if (!(cp->editable)) return(false);
  if ((Xen_is_hook(cp->edit_hook)) &&
      (Xen_hook_has_list(cp->edit_hook)))
    {
      Xen res;
      res = run_or_hook(cp->edit_hook, Xen_empty_list, S_edit_hook);
      if (Xen_is_true(res)) 
	return(false);
    }
  return(true);
}


static void increment_edit_ctr(chan_info *cp)
{
  cp->edit_ctr++;
  if (cp->edit_ctr >= cp->edit_size)
    {
      int i;
      cp->edit_size += EDIT_ALLOC_SIZE;
      if (!cp->edits) cp->edits = (ed_list **)calloc(cp->edit_size, sizeof(ed_list *));
      else cp->edits = (ed_list **)realloc(cp->edits, cp->edit_size * sizeof(ed_list *));
      for (i = cp->edit_ctr; i < cp->edit_size; i++) cp->edits[i] = NULL; 
    }
}


static bool prepare_edit_list(chan_info *cp, int pos, const char *caller)
{
  /* pos is the edit position the current edit is referring to --
   *   we normally can't set up an edit list entry that refers to a situation
   *   that will be clobbered by prune_edits below
   */
  snd_info *sp;

  if (!(is_editable(cp))) return(false); /* this may represent a second call on edit-hook for this edit */
  if (pos > cp->edit_ctr)
    {
      Xen_error(Xen_make_error_type("no-such-edit"),
		Xen_list_6(C_string_to_Xen_string("~A: edpos: ~A but ~A chan ~A has ~A edits"),
			   C_string_to_Xen_string(caller),
			   C_int_to_Xen_integer(pos),
			   C_string_to_Xen_string(cp->sound->short_filename),
			   C_int_to_Xen_integer(cp->chan),
			   C_int_to_Xen_integer(cp->edit_ctr)));
    }
  sp = cp->sound;
  stop_peak_env(cp);
  if ((sp) && (sp->playing)) stop_playing_sound_without_hook(sp, PLAY_EDIT);
  increment_edit_ctr(cp);
  prune_edits(cp, cp->edit_ctr);
  return(true);
}


static void reflect_sample_change_in_axis(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap)
    {
      mus_long_t samps;
      samps = current_samples(cp);
      ap->xmax = (double)samps / (double)snd_srate(cp->sound);
      ap->x_ambit = ap->xmax - ap->xmin;
      if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
      if ((samps == 0) || (ap->no_data))
	{
	  ap->no_data = (samps == 0);
	  if (ap->xlabel) free(ap->xlabel);
	  if (samps == 0) 
	    ap->xlabel = mus_strdup("(no data)"); 
	  else 
	    {
	      if (ap->default_xlabel)
		ap->xlabel = mus_strdup(ap->default_xlabel);
	      else ap->xlabel = mus_strdup("time");
	    }
	}
      set_x_bounds(ap);
    }
}


void reflect_file_change_in_label(chan_info *cp)
{
#if (!USE_NO_GUI)
  snd_info *sp;

  if (cp->edit_ctr == 0) return;
  sp = cp->sound;

  if (!(cp->squelch_update))
    {
      char *starred_name;
      int len;

      len = strlen(shortname(sp)) + 16;
      starred_name = (char *)calloc(len, sizeof(char));
      strcopy(starred_name, shortname_indexed(sp), len);

      if ((sp->user_read_only == FILE_READ_ONLY) || 
	  (sp->file_read_only == FILE_READ_ONLY))
	strcat(starred_name, "(*)");
      else strcat(starred_name, "*");
      set_sound_pane_file_label(sp, starred_name);
      free(starred_name);
    }
#endif
}


static void check_for_first_edit(chan_info *cp)
{
  if (cp->edit_ctr == 1) /* first edit on this file (?) */
    reflect_file_change_in_label(cp);
}


static Xen save_state_hook;

char *run_save_state_hook(const char *file)
{
  char *filename;
  filename = mus_strdup(file);
  if (Xen_hook_has_list(save_state_hook))
    {
      Xen result;
#if HAVE_SCHEME
      result = s7_call(s7, save_state_hook, s7_cons(s7, C_string_to_Xen_string(filename), s7_nil(s7)));
      if (Xen_is_string(result))
	{
	  free(filename);
	  filename = mus_strdup(Xen_string_to_C_string(result));
	}
#else
      Xen procs, fname;
      fname = C_string_to_Xen_string(filename);
      procs = Xen_hook_list(save_state_hook);
      while (!Xen_is_null(procs))
	{
	  result = Xen_call_with_1_arg(Xen_car(procs), fname, "save state hook");
	  if (Xen_is_string(result))
	    {
	      free(filename);
	      filename = mus_strdup(Xen_string_to_C_string(result));
	    }
	  procs = Xen_cdr(procs);
	}
#endif
    }
  return(filename);
}



/* -------- EDIT LISTS --------
 *
 * each channel has a list of lists containing the current edit history and the associated sound temp files or buffers
 * undo: back up current list position
 * redo: push position foward
 * No actual changes are flushed out to the file system until the file is saved.
 *
 * the editing possibilities are insert, change, delete, scale, zero, env, mix
 * All input goes through these lists (with minor exceptions -- see chn_sample below).
 *
 * The accessors are highly optimized (split into numerous choices) since everything else in Snd
 *   goes through the accessors to get at the data.  
 *
 * I tried a flattened version (using ed_fragment* rather than ed_fragment**) which involves fewer calls on malloc,
 *   but the edit list is a "real" list -- we do internal insertions and deletions and so on, so it's simpler
 *   to use in the current form.
 */


/* fragment ramp info */

typedef struct {
  double start, incr;
} ramp_state;

typedef struct {
  double start, incr, scl, off;
} xramp_state;

typedef struct {
  int ramps, xramps;
  ramp_state *ramp_list;
  xramp_state *xramp_list;
} ed_ramps;


/* fragment mix info */

typedef struct {
  int size;                              /* size of mix_list, but some entries can be empty */
  mix_state **mix_list;
} ed_mixes;


/* ed list fragment */

typedef struct ed_fragment {             /* this name is necessary even in straight C */
  int typ,                               /* code for accessor choice (ED_SIMPLE etc) */
      snd;                               /* either an index into the cp->sounds array (snd_data structs) or EDIT_LIST_END|ZERO_MARK */
  mus_long_t out,                               /* running segment location within current overall edited data */
        beg,                               /* index into the associated data => start point of data used in current segment */
        end;                               /* index into the associated data => end point of data used in current segment */
  mus_float_t scl;                               /* segment scaler */
  ed_ramps *ramps;
  ed_mixes *mixes; 
  struct ed_fragment *next;
} ed_fragment;



/* reader ramp info */

typedef struct {
  int ramps, xramps;
  double *incrs, *vals;
  double *xincrs, *xvals;
  mus_float_t (*rampf)(struct snd_fd *sf);
  mus_float_t (*rev_rampf)(struct snd_fd *sf);
} reader_ramps;


/* reader mix info */

typedef struct {
  short size;
  snd_fd **sfs;
} reader_mixes;



/* two ed_fragment->snd markers */

#define EDIT_LIST_END_MARK -2
#define EDIT_LIST_ZERO_MARK -1

#define FRAGMENTS(Ed)                         (ed_fragment **)((Ed)->fragments)
#define FRAGMENT(Ed, Pos)                    ((ed_fragment **)((Ed)->fragments))[Pos]

/* #define FRAGMENT_RAMPS(Ed, Pos)              ((ed_fragment **)((Ed)->fragments))[Pos]->ramps */
/* #define FRAGMENT_RAMP_LIST(Ed, Pos)          ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->ramp_list */
#define FRAGMENT_RAMP_LIST_SIZE(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->ramps
/* #define FRAGMENT_XRAMP_LIST(Ed, Pos)         ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->xramp_list */
#define FRAGMENT_XRAMP_LIST_SIZE(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->xramps

#define FRAGMENT_MIXES(Ed, Pos)              ((ed_fragment **)((Ed)->fragments))[Pos]->mixes
#define FRAGMENT_MIX_LIST_SIZE(Ed, Pos)      ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->size
#define FRAGMENT_MIX_LIST(Ed, Pos)           ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->mix_list

#define FRAGMENT_GLOBAL_POSITION(Ed, Pos)    ((ed_fragment **)((Ed)->fragments))[Pos]->out
#define FRAGMENT_LOCAL_POSITION(Ed, Pos)     ((ed_fragment **)((Ed)->fragments))[Pos]->beg
#define FRAGMENT_LOCAL_END(Ed, Pos)          ((ed_fragment **)((Ed)->fragments))[Pos]->end
#define FRAGMENT_SCALER(Ed, Pos)             ((ed_fragment **)((Ed)->fragments))[Pos]->scl
#define FRAGMENT_TYPE(Ed, Pos)               ((ed_fragment **)((Ed)->fragments))[Pos]->typ
#define FRAGMENT_SOUND(Ed, Pos)              ((ed_fragment **)((Ed)->fragments))[Pos]->snd
#define FRAGMENT_LENGTH(Ed, Pos)             (FRAGMENT_LOCAL_END(Ed, Pos) - FRAGMENT_LOCAL_POSITION(Ed, Pos) + 1)

#define FRAGMENT_RAMP_START(Ed, Pos, Rmp)    ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->ramp_list[Rmp].start
#define FRAGMENT_RAMP_INCR(Ed, Pos, Rmp)     ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->ramp_list[Rmp].incr
#define FRAGMENT_XRAMP_START(Ed, Pos, Rmp)   ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->xramp_list[Rmp].start
#define FRAGMENT_XRAMP_INCR(Ed, Pos, Rmp)    ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->xramp_list[Rmp].incr
#define FRAGMENT_XRAMP_SCALER(Ed, Pos, Rmp)  ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->xramp_list[Rmp].scl
#define FRAGMENT_XRAMP_OFFSET(Ed, Pos, Rmp)  ((ed_fragment **)((Ed)->fragments))[Pos]->ramps->xramp_list[Rmp].off

#define FRAGMENT_MIX_STATE(Ed, Pos, Mix)     ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->mix_list[Mix]
/* #define FRAGMENT_MIX_INDEX(Ed, Pos, Mix)     ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->mix_list[Mix]->index */
/* #define FRAGMENT_MIX_LENGTH(Ed, Pos, Mix)    ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->mix_list[Mix]->len */
/* #define FRAGMENT_MIX_SCALER(Ed, Pos, Mix)    ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->mix_list[Mix]->scaler */
/* #define FRAGMENT_MIX_BEG(Ed, Pos, Mix)       ((ed_fragment **)((Ed)->fragments))[Pos]->mixes->mix_list[Mix]->beg */


#define ED_GLOBAL_POSITION(Ed)               (Ed)->out
#define ED_LOCAL_POSITION(Ed)                (Ed)->beg
#define ED_LOCAL_END(Ed)                     (Ed)->end
#define ED_SCALER(Ed)                        (Ed)->scl
#define ED_TYPE(Ed)                          (Ed)->typ
#define ED_SOUND(Ed)                         (Ed)->snd

#define ED_RAMPS(Ed)                         (Ed)->ramps
#define ED_RAMP_LIST(Ed)                     (Ed)->ramps->ramp_list
#define ED_RAMP_LIST_SIZE(Ed)                (Ed)->ramps->ramps
#define ED_XRAMP_LIST(Ed)                    (Ed)->ramps->xramp_list
#define ED_XRAMP_LIST_SIZE(Ed)               (Ed)->ramps->xramps

#define ED_RAMP_START(Ed, Rmp)               (Ed)->ramps->ramp_list[Rmp].start
#define ED_RAMP_INCR(Ed, Rmp)                (Ed)->ramps->ramp_list[Rmp].incr
#define ED_XRAMP_START(Ed, Rmp)              (Ed)->ramps->xramp_list[Rmp].start
#define ED_XRAMP_INCR(Ed, Rmp)               (Ed)->ramps->xramp_list[Rmp].incr
#define ED_XRAMP_SCALER(Ed, Rmp)             (Ed)->ramps->xramp_list[Rmp].scl
#define ED_XRAMP_OFFSET(Ed, Rmp)             (Ed)->ramps->xramp_list[Rmp].off

#define ED_MIXES(Ed)                         (Ed)->mixes
#define ED_MIX_LIST(Ed)                      (Ed)->mixes->mix_list
/* #define ED_MIX_LIST_SIZE(Ed)                 (Ed)->mixes->size */
/* #define ED_MIX_STATE(Ed, Mix)                (Ed)->mixes->mix_list[Mix] */
/* #define ED_MIX_INDEX(Ed, Mix)                (Ed)->mixes->mix_list[Mix]->index */
/* #define ED_MIX_LENGTH(Ed, Mix)               (Ed)->mixes->mix_list[Mix]->len */
/* #define ED_MIX_SCALER(Ed, Mix)               (Ed)->mixes->mix_list[Mix]->scaler */
/* #define ED_MIX_BEG(Ed, Mix)                  (Ed)->mixes->mix_list[Mix]->beg */

#define MIX_LIST_SCALER(Ed, Mix)             (Ed)->mix_list[Mix]->scaler
#define MIX_LIST_STATE(Ed, Mix)              (Ed)->mix_list[Mix]
#define MIX_LIST_INDEX(Ed, Mix)              (Ed)->mix_list[Mix]->index
/* #define MIX_LIST_LENGTH(Ed, Mix)             (Ed)->mix_list[Mix]->len */
#define MIX_LIST_BEG(Ed, Mix)                (Ed)->mix_list[Mix]->beg


#define READER_GLOBAL_POSITION(Sf)           ((ed_fragment *)((Sf)->cb))->out
#define READER_LOCAL_POSITION(Sf)            ((ed_fragment *)((Sf)->cb))->beg
#define READER_LOCAL_END(Sf)                 ((ed_fragment *)((Sf)->cb))->end
#define READER_SCALER(Sf)                    ((ed_fragment *)((Sf)->cb))->scl
#define READER_TYPE(Sf)                      ((ed_fragment *)((Sf)->cb))->typ
#define READER_SOUND(Sf)                     ((ed_fragment *)((Sf)->cb))->snd

#define READER_RAMP_START(Sf, Pos)           ((ed_fragment *)((Sf)->cb))->ramps->ramp_list[Pos].start
#define READER_RAMP_INCR(Sf, Pos)            ((ed_fragment *)((Sf)->cb))->ramps->ramp_list[Pos].incr
#define READER_XRAMP_START(Sf, Pos)          ((ed_fragment *)((Sf)->cb))->ramps->xramp_list[Pos].start
#define READER_XRAMP_INCR(Sf, Pos)           ((ed_fragment *)((Sf)->cb))->ramps->xramp_list[Pos].incr
#define READER_XRAMP_SCALER(Sf, Pos)         ((ed_fragment *)((Sf)->cb))->ramps->xramp_list[Pos].scl
#define READER_XRAMP_OFFSET(Sf, Pos)         ((ed_fragment *)((Sf)->cb))->ramps->xramp_list[Pos].off

#define READER_RAMPS(Sf)                     ((reader_ramps *)((Sf)->ramps))->ramps
#define READER_XRAMPS(Sf)                    ((reader_ramps *)((Sf)->ramps))->xramps
#define READER_INCRS(Sf)                     ((reader_ramps *)((Sf)->ramps))->incrs
#define READER_VALS(Sf)                      ((reader_ramps *)((Sf)->ramps))->vals
#define READER_XINCRS(Sf)                    ((reader_ramps *)((Sf)->ramps))->xincrs
#define READER_XVALS(Sf)                     ((reader_ramps *)((Sf)->ramps))->xvals

#define READER_INCR(Sf, Pos)                 ((reader_ramps *)((Sf)->ramps))->incrs[Pos]
#define READER_VAL(Sf, Pos)                  ((reader_ramps *)((Sf)->ramps))->vals[Pos]
#define READER_XINCR(Sf, Pos)                ((reader_ramps *)((Sf)->ramps))->xincrs[Pos]
#define READER_XVAL(Sf, Pos)                 ((reader_ramps *)((Sf)->ramps))->xvals[Pos]
#define READER_RAMPF(Sf)                     ((reader_ramps *)((Sf)->ramps))->rampf
#define READER_REV_RAMPF(Sf)                 ((reader_ramps *)((Sf)->ramps))->rev_rampf

/* #define READER_MIXES(Sf)                     ((ed_fragment *)((Sf)->cb))->mixes */
#define READER_MIX_LIST_SIZE(Sf)             ((ed_fragment *)((Sf)->cb))->mixes->size
/* #define READER_MIX_LIST(Sf)                  ((ed_fragment *)((Sf)->cb))->mixes->mix_list */
#define READER_MIX_STATE(Sf, Mix)            ((ed_fragment *)((Sf)->cb))->mixes->mix_list[Mix]
#define READER_MIX_INDEX(Sf, Mix)            ((ed_fragment *)((Sf)->cb))->mixes->mix_list[Mix]->index
#define READER_MIX_LENGTH(Sf, Mix)           ((ed_fragment *)((Sf)->cb))->mixes->mix_list[Mix]->len
#define READER_MIX_SCALER(Sf, Mix)           ((ed_fragment *)((Sf)->cb))->mixes->mix_list[Mix]->scaler
#define READER_MIX_BEG(Sf, Mix)              ((ed_fragment *)((Sf)->cb))->mixes->mix_list[Mix]->beg



/* -------------------------------- fragment accessors -------------------------------- */

static mus_float_t next_ramp1_value(snd_fd *sf)
{
  mus_float_t val;
  val = READER_VAL(sf, 0);
  READER_VAL(sf, 0) += READER_INCR(sf, 0);
  return(val);
}

static mus_float_t previous_ramp1_value(snd_fd *sf)
{
  mus_float_t val;
  val = READER_VAL(sf, 0);
  READER_VAL(sf, 0) -= READER_INCR(sf, 0);
  return(val);
}


static mus_float_t next_ramp2_value(snd_fd *sf)
{
  mus_float_t val;
  val = READER_VAL(sf, 0) * READER_VAL(sf, 1);
  READER_VAL(sf, 0) += READER_INCR(sf, 0);
  READER_VAL(sf, 1) += READER_INCR(sf, 1);
  return(val);
}

static mus_float_t previous_ramp2_value(snd_fd *sf)
{
  mus_float_t val;
  val = READER_VAL(sf, 0) * READER_VAL(sf, 1);
  READER_VAL(sf, 0) -= READER_INCR(sf, 0);
  READER_VAL(sf, 1) -= READER_INCR(sf, 1);
  return(val);
}


static mus_float_t next_ramp_value(snd_fd *sf)
{
  mus_float_t val;
  int i;
  val = READER_VAL(sf, 0);
  READER_VAL(sf, 0) += READER_INCR(sf, 0);
  for (i = 1; i < READER_RAMPS(sf); i++)
    {
      val *= READER_VAL(sf, i);
      READER_VAL(sf, i) += READER_INCR(sf, i);
    }
  return(val);
}

static mus_float_t previous_ramp_value(snd_fd *sf)
{
  mus_float_t val;
  int i;
  val = READER_VAL(sf, 0);
  READER_VAL(sf, 0) -= READER_INCR(sf, 0);
  for (i = 1; i < READER_RAMPS(sf); i++)
    {
      val *= READER_VAL(sf, i);
      READER_VAL(sf, i) -= READER_INCR(sf, i);
    }
  return(val);
}


static mus_float_t next_xramp1_value(snd_fd *sf)
{
  mus_float_t val;
  val = (READER_XRAMP_OFFSET(sf, 0) + (READER_XRAMP_SCALER(sf, 0) * READER_XVAL(sf, 0)));
  READER_XVAL(sf, 0) *= READER_XINCR(sf, 0);
  return(val);
}

/* ideally we'd invert the increment if reading an xramp in reverse, making this a multiply, not a divide,
 *   but then we'd need a fixup when the reader changes direction -- probably not worth the complexity.
 */

static mus_float_t previous_xramp1_value(snd_fd *sf)
{
  mus_float_t val;
  val = (READER_XRAMP_OFFSET(sf, 0) + (READER_XRAMP_SCALER(sf, 0) * READER_XVAL(sf, 0)));
  READER_XVAL(sf, 0) /= READER_XINCR(sf, 0);
  return(val);
}


static mus_float_t next_xramp_value(snd_fd *sf)
{
  int i;
  mus_float_t val;
  val = (READER_XRAMP_OFFSET(sf, 0) + (READER_XRAMP_SCALER(sf, 0) * READER_XVAL(sf, 0)));
  READER_XVAL(sf, 0) *= READER_XINCR(sf, 0);
  for (i = 1; i < READER_XRAMPS(sf); i++)
    {
      val *= (READER_XRAMP_OFFSET(sf, i) + (READER_XRAMP_SCALER(sf, i) * READER_XVAL(sf, i)));
      READER_XVAL(sf, i) *= READER_XINCR(sf, i);
    }
  return(val);
}

static mus_float_t previous_xramp_value(snd_fd *sf)
{
  int i;
  mus_float_t val = 1.0;
  for (i = 0; i < READER_XRAMPS(sf); i++)
    {
      val *= (READER_XRAMP_OFFSET(sf, i) + (READER_XRAMP_SCALER(sf, i) * READER_XVAL(sf, i)));
      READER_XVAL(sf, i) /= READER_XINCR(sf, i);
    }
  return(val);
}


static mus_float_t next_xramp_ramp_value(snd_fd *sf)
{
  return(next_xramp_value(sf) * next_ramp_value(sf));
}

static mus_float_t previous_xramp_ramp_value(snd_fd *sf)
{
  return(previous_xramp_value(sf) * previous_ramp_value(sf));
}


static mus_float_t previous_sound(snd_fd *sf);
mus_float_t next_sound(snd_fd *sf);


static mus_float_t end_sample_value(snd_fd *ignore) {return(0.0);}


static mus_float_t next_sample_value(snd_fd *sf) 
{
  if (sf->loc > sf->last) 
    return(next_sound(sf)); 
  else return(sf->data[sf->loc++] * sf->fscaler);
}


static mus_float_t next_sample_value_unchecked(snd_fd *sf) 
{
  return(sf->data[sf->loc++] * sf->fscaler);
}

static mus_float_t previous_sample_value(snd_fd *sf) 
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf)); 
  else return(sf->data[sf->loc--] * sf->fscaler);
}

mus_float_t previous_sample_value_unchecked(snd_fd *sf) ;
mus_float_t previous_sample_value_unchecked(snd_fd *sf) 
{
  return(sf->data[sf->loc--] * sf->fscaler);
}


mus_float_t next_sample_value_unscaled(snd_fd *sf);
mus_float_t next_sample_value_unscaled(snd_fd *sf) 
{
  if (sf->loc > sf->last) 
    return(next_sound(sf)); 
  else return(sf->data[sf->loc++]);
}

mus_float_t next_sample_value_unscaled_and_unchecked(snd_fd *sf);
mus_float_t next_sample_value_unscaled_and_unchecked(snd_fd *sf) 
{
  return(sf->data[sf->loc++]);
}

mus_float_t previous_sample_value_unscaled(snd_fd *sf) ;
mus_float_t previous_sample_value_unscaled(snd_fd *sf) 
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf)); 
  else return(sf->data[sf->loc--]);
}


mus_float_t previous_sample_value_unscaled_and_unchecked(snd_fd *sf);
mus_float_t previous_sample_value_unscaled_and_unchecked(snd_fd *sf) 
{
  return(sf->data[sf->loc--]);
}


static mus_float_t next_zero(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf));
  sf->loc++;
  return(0.0);
}

static mus_float_t previous_zero(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf));
  sf->loc--;
  return(0.0);
}


static mus_float_t next_ramp1(snd_fd *sf)
{
  if (sf->loc > sf->last)
     return(next_sound(sf));
  else 
    {
      mus_float_t val;
      val = sf->data[sf->loc++] * READER_VAL(sf, 0);
      READER_VAL(sf, 0) += READER_INCR(sf, 0);
      return(val);
    }
}

static mus_float_t next_ramp1_unchecked(snd_fd *sf)
{
  mus_float_t val;
  val = sf->data[sf->loc++] * READER_VAL(sf, 0);
  READER_VAL(sf, 0) += READER_INCR(sf, 0);
  return(val);
}

static mus_float_t previous_ramp1(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  else
    {
      mus_float_t val;
      val = sf->data[sf->loc--] * READER_VAL(sf, 0);
      READER_VAL(sf, 0) -= READER_INCR(sf, 0);
      return(val);
    }
}


static mus_float_t next_ramp_f(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf)); 
  else return(sf->data[sf->loc++] * (*(READER_RAMPF(sf)))(sf));
}

static mus_float_t previous_ramp_f(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf)); 
  else return(sf->data[sf->loc--] * (*(READER_REV_RAMPF(sf)))(sf));
}


static mus_float_t next_ramp(snd_fd *sf) 
{
  if (sf->loc > sf->last)
    return(next_sound(sf)); 
  else return(sf->data[sf->loc++] * next_ramp_value(sf));
}

static mus_float_t previous_ramp(snd_fd *sf) 
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf)); 
  else return(sf->data[sf->loc--] * previous_ramp_value(sf));
}


static mus_float_t next_xramp1(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf)); 
  else return(sf->data[sf->loc++] * READER_SCALER(sf) * next_xramp1_value(sf));
}

static mus_float_t previous_xramp1(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf)); 
  else return(sf->data[sf->loc--] * READER_SCALER(sf) * previous_xramp1_value(sf));
}


static mus_float_t next_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf)); 
  else return(sf->data[sf->loc++] * READER_SCALER(sf) * next_xramp_value(sf));
}

static mus_float_t previous_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf)); 
  else return(sf->data[sf->loc--] * READER_SCALER(sf) * previous_xramp_value(sf));
}



/* mix readers */

static mus_float_t read_mix_list_samples(snd_fd *sf)
{
  reader_mixes *m;
  int i;
  mus_float_t sum;

  m = (reader_mixes *)(sf->mixes);
  if (m->size == 0) return(0.0); /* this is apparently possible?? */

  sum = read_sample(m->sfs[0]);
  for (i = 1; i < m->size; i++)
    sum += read_sample(m->sfs[i]);
  return(sum);
}


static mus_float_t next_mix(snd_fd *sf)
{
  /* read_sample here would call runf => next_mix => infinite recursion */
  if (sf->loc > sf->last) return(next_sound(sf)); /* next_sound here refers to whatever follows the mixed portion */
  return((sf->data[sf->loc++] * sf->fscaler) + read_mix_list_samples(sf));
}

/* if underlying data was scaled,and we added a mix, we couldn't share the existing scale,
 *   and mixes can be added after subsequent scaling, so we're constrained to use the mix-amps
 */

static mus_float_t previous_mix(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf));
  return((sf->data[sf->loc--] * sf->fscaler) + read_mix_list_samples(sf));
}


static mus_float_t next_mix_zero(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); /* next_sound here refers to whatever follows the mixed portion */
  sf->loc++;
  return(read_mix_list_samples(sf));
}

static mus_float_t previous_mix_zero(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf));
  sf->loc--;
  return(read_mix_list_samples(sf));
}


static mus_float_t next_one_mix(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); /* next_sound here refers to whatever follows the mixed portion */
  return((sf->data[sf->loc++] * sf->fscaler) + read_sample(((reader_mixes *)(sf->mixes))->sfs[0]));
}

static mus_float_t previous_one_mix(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf));
  return((sf->data[sf->loc--] * sf->fscaler) + read_sample(((reader_mixes *)(sf->mixes))->sfs[0]));
}


static mus_float_t next_one_mix_zero(snd_fd *sf)
{
  if (sf->loc > sf->last) return(next_sound(sf)); /* next_sound here refers to whatever follows the mixed portion */
  sf->loc++;
  return(read_sample(((reader_mixes *)(sf->mixes))->sfs[0]));
}

static mus_float_t previous_one_mix_zero(snd_fd *sf)
{
  if (sf->loc < sf->first) return(previous_sound(sf));
  sf->loc--;
  return(read_sample(((reader_mixes *)(sf->mixes))->sfs[0]));
}


static mus_float_t next_mix_ramp1(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf));
  return((sf->data[sf->loc++] * next_ramp1_value(sf)) + read_mix_list_samples(sf));
}

static mus_float_t previous_mix_ramp1(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  return((sf->data[sf->loc--] * previous_ramp1_value(sf)) + read_mix_list_samples(sf));
}


static mus_float_t next_mix_ramp2(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf));
  return((sf->data[sf->loc++] * next_ramp2_value(sf)) + read_mix_list_samples(sf));
}

static mus_float_t previous_mix_ramp2(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf));
  return((sf->data[sf->loc--] * previous_ramp2_value(sf)) + read_mix_list_samples(sf));
}


static mus_float_t next_mix_ramp(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf));
  return((sf->data[sf->loc++] * next_ramp_value(sf)) + read_mix_list_samples(sf));
}

static mus_float_t previous_mix_ramp(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf));
  return((sf->data[sf->loc--] * previous_ramp_value(sf)) + read_mix_list_samples(sf));
}


static mus_float_t next_mix_xramp1(snd_fd *sf)
{
  if (sf->loc > sf->last) 
    return(next_sound(sf));
  return((sf->data[sf->loc++] * READER_SCALER(sf) * next_xramp1_value(sf)) + read_mix_list_samples(sf));
}

static mus_float_t previous_mix_xramp1(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf));
  return((sf->data[sf->loc--] * READER_SCALER(sf) * previous_xramp1_value(sf)) + read_mix_list_samples(sf));
}


static mus_float_t next_mix_xramp(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  return((sf->data[sf->loc++] * READER_SCALER(sf) * next_xramp_value(sf)) + read_mix_list_samples(sf));
}

static mus_float_t previous_mix_xramp(snd_fd *sf)
{
  if (sf->loc < sf->first)
    return(previous_sound(sf));
  return((sf->data[sf->loc--] * READER_SCALER(sf) * previous_xramp_value(sf)) + read_mix_list_samples(sf));
}


static mus_float_t next_mix_xramp_f_ramp_f(snd_fd *sf)
{
  if (sf->loc > sf->last)
    return(next_sound(sf));
  return((sf->data[sf->loc++] * (*(READER_RAMPF(sf)))(sf)) + read_mix_list_samples(sf));
}

static mus_float_t previous_mix_xramp_f_ramp_f(snd_fd *sf)
{
  if (sf->loc < sf->first) 
    return(previous_sound(sf));
  return((sf->data[sf->loc--] * (*(READER_REV_RAMPF(sf)))(sf)) + read_mix_list_samples(sf));
}





/* this is a kind of state machine for the virtual editor: if, for example, the current
     op is ed_ramp and we want to add an xramp, we look at the add_xramp field of 
     ed_ramp's type_info struct, and if it's not -1, the add_xramp field is the
     new virtual editor op; otherwise the op is collapsed to a change edit.
     So, to add a new entry, make the accessor, and fill in the fields that
     can reach it.  Since, for example, ramp->xramp is flipped to xramp->ramp
     since a(b()) == b(a()) in this case, there can be several ways to reach
     a given op. Then fix the ramp segments in choose_accessor.  The first
     struct field is redundant, but makes editing this table much easier. 
*/

enum {ED_SIMPLE,                ED_MIX, 
      ED_ZERO,                  ED_MIX_ZERO,

      /* simple envelope special cases */
      ED_RAMP1,                 ED_MIX_RAMP1, 
      ED_RAMP2,                 ED_MIX_RAMP2,
      ED_XRAMP1,                ED_MIX_XRAMP1,

      /* envelopes */
      ED_RAMP,                  ED_MIX_RAMP,
      ED_XRAMP,                 ED_MIX_XRAMP,
      ED_XRAMP_RAMP,            ED_MIX_XRAMP_RAMP,

      NUM_OPS
};


typedef struct {
  int type, add_ramp, add_xramp, add_mix, subtract_mix;
  bool ramps, zero, mixes; /* zero = no underlying data (mix, etc), mixes = involves virtual mixes in some way */
  int scale_op;
  const char *name;
  mus_float_t (*next)(struct snd_fd *sf);  
  mus_float_t (*previous)(struct snd_fd *sf);  
  mus_float_t (*rampf)(struct snd_fd *sf);  
  mus_float_t (*rev_rampf)(struct snd_fd *sf);  
} fragment_type_info;


enum {NO_SCALE, SIMPLE_SCALE, SCALE_R, SCALE_X};

#define NO_RAMP false
#define NO_MIX false
#define ON_ZERO true
#define ON_DATA false
#define MIXED true
#define RAMPED true

static fragment_type_info type_info[NUM_OPS] = {

  {ED_SIMPLE, ED_RAMP1, ED_XRAMP1, ED_MIX, -1, 
   NO_RAMP, ON_DATA, NO_MIX, SIMPLE_SCALE,
   "ed_simple", next_sample_value, previous_sample_value, 
   NULL, NULL},

  {ED_MIX, -1, -1, ED_MIX, ED_SIMPLE, 
   NO_RAMP, ON_DATA, MIXED, NO_SCALE,
   "ed_mix_simple", next_mix, previous_mix, 
   NULL, NULL},

  {ED_ZERO, ED_ZERO, ED_ZERO, ED_MIX_ZERO, -1, 
   NO_RAMP, ON_ZERO, NO_MIX, NO_SCALE,
   "ed_zero", next_zero, previous_zero, 
   NULL, NULL},

  {ED_MIX_ZERO, -1, -1, ED_MIX_ZERO, ED_ZERO, 
   NO_RAMP, ON_ZERO, MIXED, NO_SCALE,
   "ed_mix_zero", next_mix_zero, previous_mix_zero, 
   NULL, NULL},

  {ED_RAMP1, ED_RAMP2, ED_XRAMP_RAMP, ED_MIX_RAMP1, -1, 
   RAMPED, ON_DATA, NO_MIX, SCALE_R,
   "ed_ramp1", next_ramp1, previous_ramp1, 
   NULL, NULL},

  {ED_MIX_RAMP1, -1, -1, ED_MIX_RAMP1, ED_RAMP1, 
   RAMPED, ON_DATA, MIXED, SCALE_R,
   "ed_mix_ramp1", next_mix_ramp1, previous_mix_ramp1, 
   NULL, NULL},

  {ED_RAMP2, ED_RAMP, ED_XRAMP_RAMP, ED_MIX_RAMP2, -1, 
   RAMPED, ON_DATA, NO_MIX, SCALE_R,
   "ed_ramp2", next_ramp_f, previous_ramp_f, 
   next_ramp2_value, previous_ramp2_value},

  {ED_MIX_RAMP2, -1, -1, ED_MIX_RAMP2, ED_RAMP2, 
   RAMPED, ON_DATA, MIXED, SCALE_R,
   "ed_mix_ramp2", next_mix_ramp2, previous_mix_ramp2, 
   NULL, NULL},

  {ED_XRAMP1, ED_XRAMP_RAMP, ED_XRAMP, ED_MIX_XRAMP1, -1, 
   RAMPED, ON_DATA, NO_MIX, NO_SCALE,
   "ed_xramp1", next_xramp1, previous_xramp1, 
   NULL, NULL},
  
  {ED_MIX_XRAMP1, -1, -1, ED_MIX_XRAMP1, ED_XRAMP1, 
   RAMPED, ON_DATA, MIXED, NO_SCALE,
   "ed_mix_xramp1", next_mix_xramp1, previous_mix_xramp1, 
   NULL, NULL},

  {ED_RAMP, ED_RAMP, ED_XRAMP_RAMP, ED_MIX_RAMP, -1, 
   RAMPED, ON_DATA, NO_MIX, SCALE_R,
   "ed_ramp", next_ramp, previous_ramp, 
   NULL, NULL},

  {ED_MIX_RAMP, -1, -1, ED_MIX_RAMP, ED_RAMP, 
   RAMPED, ON_DATA, MIXED, SCALE_R,
   "ed_mix_ramp", next_mix_ramp, previous_mix_ramp, 
   NULL, NULL},

  {ED_XRAMP, ED_XRAMP_RAMP, ED_XRAMP, ED_MIX_XRAMP, -1,
   RAMPED, ON_DATA, NO_MIX, NO_SCALE,
   "ed_xramp", next_xramp, previous_xramp, 
   NULL, NULL},

  {ED_MIX_XRAMP, -1, -1, ED_MIX_XRAMP, ED_XRAMP, 
   RAMPED, ON_DATA, MIXED, NO_SCALE,
   "ed_mix_xramp", next_mix_xramp, previous_mix_xramp, 
   NULL, NULL},

  {ED_XRAMP_RAMP, ED_XRAMP_RAMP, ED_XRAMP_RAMP, ED_MIX_XRAMP_RAMP, -1, 
   RAMPED, ON_DATA, NO_MIX, SCALE_R,
   "ed_xramp_ramp", next_ramp_f, previous_ramp_f, 
   next_xramp_ramp_value, previous_xramp_ramp_value},

  {ED_MIX_XRAMP_RAMP, -1, -1, ED_MIX_XRAMP_RAMP, ED_XRAMP_RAMP, 
   RAMPED, ON_DATA, MIXED, SCALE_R,
   "ed_mix_xramp_ramp", next_mix_xramp_f_ramp_f, previous_mix_xramp_f_ramp_f, 
   next_xramp_ramp_value, previous_xramp_ramp_value},
};


static bool zero_op(int type)
{
  return(type_info[type].zero);
}


static bool ramp_op(int type)
{
  return(type_info[type].ramps);
}


static bool is_mix_op(int type)
{
  return(type_info[type].mixes);
}

static bool is_unmixable_op(int type)
{
  return(type_info[type].subtract_mix != -1);
}

static bool is_mixable_op(int type)
{
  return(type_info[type].add_mix != -1);
}


#define DEBUG_EDIT_TABLES 0

#if DEBUG_EDIT_TABLES
static int hit_entry[NUM_OPS];

static void init_hit_entries(void)
{
  int i;
  for (i = 0; i < NUM_OPS; i++) hit_entry[i] = 0;
}


static void report_unhit_entries(void)
{
  int i;
  for (i = 0; i < NUM_OPS; i++)
    if (hit_entry[i] == 0)
      {
	if (type_info[i].next)
	  fprintf(stderr, "accessible ");
	fprintf(stderr, "%s unhit\n", type_info[i].name);
      }
}


static void check_type_info_entry(int op, int expected_ramps, int expected_xramps, bool is_zero)
{
  hit_entry[op]++;
  if (op != type_info[op].type) 
    fprintf(stderr, "%s type: %d %d\n", type_info[op].name, op, type_info[op].type);
  if (op != ED_SIMPLE)
    {
      if (!type_info[op].next) 
	fprintf(stderr, "%s no next\n", type_info[op].name);
      if (!type_info[op].previous) 
	fprintf(stderr, "%s no previous\n", type_info[op].name);
    }
  if (is_zero != type_info[op].zero) 
    fprintf(stderr, "%s zero: %d %d\n", type_info[op].name, is_zero, type_info[op].zero);  
  if ((type_info[op].add_ramp != -1) && (type_info[op].add_ramp != op))
    check_type_info_entry(type_info[op].add_ramp, expected_ramps + 1, expected_xramps, is_zero);
  if ((type_info[op].add_xramp != -1) && (type_info[op].add_xramp != op))
    check_type_info_entry(type_info[op].add_xramp, expected_ramps, expected_xramps + 1, is_zero);
  if ((type_info[op].add_mix != -1) &&
      (type_info[op].subtract_mix == -1)) /* not a loop! */
    check_type_info_entry(type_info[op].add_mix, expected_ramps, expected_xramps, is_zero);

  if ((!(type_info[op].mixes)) && (type_info[op].subtract_mix != -1))
    fprintf(stderr, "%s: mix confused (#f)?\n", type_info[op].name);

  if (type_info[op].subtract_mix != -1)
    {
      if ((type_info[op].add_mix != -1) && 
	  (type_info[op].add_mix != op))
	fprintf(stderr, "%s add_mix: %s\n", type_info[op].name, type_info[type_info[op].add_mix].name);

      if ((type_info[op].add_ramp != -1) &&
	  (op != ED_MIX_ZERO))
	fprintf(stderr, "%s add_ramp: %s\n", type_info[op].name, type_info[type_info[op].add_ramp].name);
      if (type_info[op].add_xramp != -1) fprintf(stderr, "%s add_xramp: %s\n", type_info[op].name, type_info[type_info[op].add_xramp].name);

      if (type_info[type_info[op].subtract_mix].add_mix != op)
	fprintf(stderr, "%s subtract: %s but its add: %s\n",
		type_info[op].name, type_info[type_info[op].subtract_mix].name, 
		(type_info[type_info[op].subtract_mix].add_mix != -1) ? type_info[type_info[type_info[op].subtract_mix].add_mix].name : "not an op");
    }
}
#endif


static void swap_readers(snd_fd *sf)
{
  mus_float_t (*rrunf)(struct snd_fd *sf);
  rrunf = sf->runf;
  sf->runf = sf->rev_runf;
  sf->rev_runf = rrunf;
}


void read_sample_change_direction(snd_fd *sf, read_direction_t dir1) /* can't use "dir" on Mac */
{
  /* direction reversal can happen in dac(speed arrow), src gen, or user can call next/previous independent of initial dir */
  swap_readers(sf);
  sf->direction = dir1;
  /* can't optimize anything here -- some accessors have state, but how to handle the loc=-1 case? */
  if ((dir1 == READ_FORWARD) && (sf->loc < 0)) 
    sf->loc = 0;
  else read_sample(sf);
}


static mus_float_t protected_next_sample(snd_fd *sf)
{
  if (sf->direction == READ_BACKWARD) 
    read_sample_change_direction(sf, READ_FORWARD);
  return(read_sample(sf));
}


static mus_float_t protected_previous_sample(snd_fd *sf)
{
  if (sf->direction == READ_FORWARD) 
    read_sample_change_direction(sf, READ_BACKWARD);
  return(read_sample(sf));
}


/* setting the at_end flag here means that a 0.0 is returned from read-sample and then
 *   the caller has to check at-end to see if that 0.0 is real.  Not sure how (or whether)
 *   this should be fixed.
 */
static void reader_out_of_data(snd_fd *sf)
{
  sf->at_eof = true;
  sf->runf = end_sample_value;
  sf->rev_runf = end_sample_value;
}


static snd_fd *cancel_reader(snd_fd *sf)
{
  sf->current_sound = NULL;
  sf->cbi = 0;
  reader_out_of_data(sf);
  return(sf);
}


static reader_mixes *free_reader_mixes(reader_mixes *md);

static void setup_mix(snd_fd *sf)
{
  reader_mixes *md;
  int i, list_size, active_mixes = 0;

  if (sf->mixes) 
    sf->mixes = (void *)free_reader_mixes((reader_mixes *)(sf->mixes));

  md = (reader_mixes *)calloc(1, sizeof(reader_mixes));
  sf->mixes = (void *)md;

  list_size = READER_MIX_LIST_SIZE(sf);
  for (i = 0; i < list_size; i++)
    if ((READER_MIX_STATE(sf, i)) &&
	(READER_MIX_SCALER(sf, i) != 0.0))
      active_mixes++;

  md->size = active_mixes;
  if (md->size > 0)
    {
      int j = 0;
      md->sfs = (snd_fd **)calloc(md->size, sizeof(snd_fd *));
      for (i = 0; i < list_size; i++)
	if ((READER_MIX_STATE(sf, i)) &&
	    (READER_MIX_SCALER(sf, i) != 0.0))
	  md->sfs[j++] = make_virtual_mix_reader(sf->cp, 
						 sf->frag_pos + READER_GLOBAL_POSITION(sf) - READER_MIX_BEG(sf, i), /* global position - mix position + fragment start loc */
						 READER_MIX_LENGTH(sf, i),
						 READER_MIX_INDEX(sf, i), 
						 READER_MIX_SCALER(sf, i), 
						 sf->direction);
      if (active_mixes == 1)
	{
	  if (READER_TYPE(sf) == ED_MIX)
	    {
	      sf->runf = next_one_mix;
	      sf->rev_runf = previous_one_mix;
  	    }
	  else
	    {
	      if (READER_TYPE(sf) == ED_MIX_ZERO)
		{
		  sf->runf = next_one_mix_zero;
		  sf->rev_runf = previous_one_mix_zero;
		}
	    }
	}
    }
}


static void setup_ramps(snd_fd *sf, int typ)
{
  int rmps, xrmps;
  ed_fragment *ed;
  ed = (ed_fragment *)(sf->cb);

  rmps = ED_RAMP_LIST_SIZE(ed);
  xrmps = ED_XRAMP_LIST_SIZE(ed);

  if (!sf->ramps)
    sf->ramps = (void *)calloc(1, sizeof(reader_ramps));
  
  /* make sure the ramp arrays are large enough (we'll free them at the end of the read) */
  
  if (rmps != READER_RAMPS(sf))
    {
      if (READER_RAMPS(sf) > 0)
	{
	  free(READER_INCRS(sf));
	  READER_INCRS(sf) = NULL;
	  free(READER_VALS(sf));
	  READER_VALS(sf) = NULL;
	}
      if (rmps > 0)
	{
	  READER_INCRS(sf) = (double *)calloc(rmps, sizeof(double));
	  READER_VALS(sf) = (double *)calloc(rmps, sizeof(double));
	}
      READER_RAMPS(sf) = rmps;                    /* this has to match the actual ramp number */
    }

  if (xrmps != READER_XRAMPS(sf))
    {
      if (READER_XRAMPS(sf) > 0)
	{
	  free(READER_XINCRS(sf));
	  READER_XINCRS(sf) = NULL;
	  free(READER_XVALS(sf));
	  READER_XVALS(sf) = NULL;
	}
      if (xrmps > 0)
	{
	  READER_XINCRS(sf) = (double *)calloc(xrmps, sizeof(double));
	  READER_XVALS(sf) = (double *)calloc(xrmps, sizeof(double));
	}
      READER_XRAMPS(sf) = xrmps;
    }
  
  if (rmps > 0)
    {
      int i;
      for (i = 0; i < rmps; i++)
	{
	  READER_INCR(sf, i) = READER_RAMP_INCR(sf, i);
	  READER_VAL(sf, i) = READER_RAMP_START(sf, i) + READER_INCR(sf, i) * sf->frag_pos;
	}
    }
    
  if (xrmps > 0)
    {
      int i;
      for (i = 0; i < xrmps; i++)
	{
	  READER_XINCR(sf, i) = READER_XRAMP_INCR(sf, i);
	  READER_XVAL(sf, i) = READER_XRAMP_START(sf, i) * exp(log(READER_XRAMP_INCR(sf, i)) * sf->frag_pos);
	}
    }

  READER_RAMPF(sf) = type_info[typ].rampf;
  READER_REV_RAMPF(sf) = type_info[typ].rev_rampf;
}


static void scale_ramp(snd_fd *sf, int rmp, mus_float_t scl)
{
  READER_INCR(sf, rmp) *= scl;
  READER_VAL(sf, rmp) *= scl;
}


static void scale_inputs(snd_fd *sf, int scl_type)
{
  switch (scl_type)
    {
    case NO_SCALE:
      break;

    case SIMPLE_SCALE:
      /* ED_SIMPLE special case */
      if ((READER_SCALER(sf) == 1.0) &&
	  (sf->fscaler == 1.0))
	{
	  sf->runf = next_sample_value_unscaled;
	  sf->rev_runf = previous_sample_value_unscaled;
	}
      break;

    case SCALE_R:
      scale_ramp(sf, 0, READER_SCALER(sf));
      break;
    }
}


static void choose_accessor(snd_fd *sf)
{
  int typ;
  /* fragment-specific reader choice */

  typ = READER_TYPE(sf);

  if (ramp_op(typ))
    setup_ramps(sf, typ);

  if (is_mix_op(typ))
    setup_mix(sf);

  sf->runf = type_info[typ].next;
  sf->rev_runf = type_info[typ].previous;

  scale_inputs(sf, type_info[typ].scale_op);

  if (sf->direction == READ_BACKWARD) swap_readers(sf);
}


static const char *edit_names[NUM_EDIT_TYPES] = {"insert", "delete", "set", "init", "scale", "zero", "env", "extend", "mix", "change mix"};


static void display_ed_list(chan_info *cp, FILE *outp, int i, ed_list *ed)
{
  int len, j;
  snd_data *sd;
  if (!ed)
    {
      fprintf(outp, "\n (NULL FRAGMENT at %d)", i);
      return;
    }
  if (i >= cp->edit_size)
    {
      fprintf(outp, "\n (BOGUS FRAGMENT at %d of %d)", i, cp->edit_size);
      return;
    }
  len = ed->size; /* number of fragments in this list */
  switch (ed->edit_type)
    {
    case INSERTION_EDIT:  fprintf(outp, "\n (insert %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                        break;
    case DELETION_EDIT:   fprintf(outp, "\n (delete %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                        break;
    case CHANGE_EDIT:     fprintf(outp, "\n (set %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                           break;
    case SCALED_EDIT:     fprintf(outp, "\n (scale %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                         break;
    case ZERO_EDIT:       fprintf(outp, "\n (silence %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                       break;
    case RAMP_EDIT:       fprintf(outp, "\n (ramp %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                          break;
    case EXTEND_EDIT:     fprintf(outp, "\n (extend edit list with no-op)");                                            break;
    case MIX_EDIT:        fprintf(outp, "\n (mix %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                           break; 
    case CHANGE_MIX_EDIT: fprintf(outp, "\n (change mix %" print_mus_long " %" print_mus_long ") ", ed->beg, ed->len);                    break; 
    case INITIALIZE_EDIT: fprintf(outp, "\n (begin) ");                                                                 break;
    default: break;
    }
  if (ed->origin) fprintf(outp, "; %s ", ed->origin);
  fprintf(outp, "[%d:%d]:", i, len);
  for (j = 0; j < len; j++)
    {
      int index;
      index = FRAGMENT_SOUND(ed, j);
      if (index == EDIT_LIST_END_MARK)
	fprintf(outp, "\n   (at %" print_mus_long ", end_mark)", FRAGMENT_GLOBAL_POSITION(ed, j));
      else
	{
	  int typ;
	  typ = FRAGMENT_TYPE(ed, j);
	  fprintf(outp, "\n   (at %" print_mus_long ", cp->sounds[%d][%" print_mus_long ":%" print_mus_long ", %.3f",
		  FRAGMENT_GLOBAL_POSITION(ed, j),
		  index,
		  FRAGMENT_LOCAL_POSITION(ed, j),
		  FRAGMENT_LOCAL_END(ed, j),
		  FRAGMENT_SCALER(ed, j));

	  if (ramp_op(typ))
	    {
	      int k;
	      for (k = 0; k < FRAGMENT_RAMP_LIST_SIZE(ed, j); k++)  /* step envs become successive scalings */
		fprintf(outp, ", [%d]%.3f -> %.3f", k + 1, 
			FRAGMENT_RAMP_START(ed, j, k), 
			FRAGMENT_RAMP_START(ed, j, k) + (FRAGMENT_LENGTH(ed, j) - 1) * FRAGMENT_RAMP_INCR(ed, j, k));

	      for (k = 0; k < FRAGMENT_XRAMP_LIST_SIZE(ed, j); k++)
		{
		  double y;
		  y = FRAGMENT_XRAMP_SCALER(ed, j, k) * FRAGMENT_XRAMP_START(ed, j, k);
		  fprintf(outp, ", [%d]%.3f -> %.3f, off: %.3f, scl: %.3f", 
			  k + 1 + FRAGMENT_RAMP_LIST_SIZE(ed, j),
			  FRAGMENT_XRAMP_OFFSET(ed, j, k) + y,
			  FRAGMENT_XRAMP_OFFSET(ed, j, k) + y * exp(log(FRAGMENT_XRAMP_INCR(ed, j, k)) * (FRAGMENT_LENGTH(ed, j) - 1)),
			  FRAGMENT_XRAMP_OFFSET(ed, j, k),
			  FRAGMENT_XRAMP_SCALER(ed, j, k));
		}
	    }

	  if (is_mix_op(typ))
	    {
	      ed_mixes *mxs;
	      mxs = FRAGMENT_MIXES(ed, j);
	      if (!mxs)
		fprintf(outp, ", %s but no mixes found?", type_info[typ].name);
	      else
		{
		  int i;
		  for (i = 0; i < mxs->size; i++)
		    {
		      if (MIX_LIST_STATE(mxs, i))
			fprintf(outp, ", ([%d]: %d %.3f %" print_mus_long ")",
				i,
				MIX_LIST_INDEX(mxs, i),
				MIX_LIST_SCALER(mxs, i),
				FRAGMENT_GLOBAL_POSITION(ed, j) - MIX_LIST_BEG(mxs, i));
		    }
		}
	    }
	  fprintf(outp, "])");
	  if (index != EDIT_LIST_ZERO_MARK)
	    {
	      sd = cp->sounds[index];
	      if (!sd) 
		fprintf(outp, " [nil!]");
	      else 
		if (sd->type == SND_DATA_FILE)
		  fprintf(outp, " [file: %s[%d]]", sd->filename, sd->chan);
		else 
		  if (sd->type == SND_DATA_BUFFER)
		    fprintf(outp, " [buf: %" print_mus_long "] ", sd->data_bytes / sizeof(mus_float_t));
		  else fprintf(outp, " [bogus!]");
	    }
	}
    }
  fprintf(outp, "\n");
}


mus_long_t edit_changes_begin_at(chan_info *cp, int edpos)
{
  return(cp->edits[edpos]->beg);
}


mus_long_t edit_changes_end_at(chan_info *cp, int edpos)
{
  /* the env code assumes a deletion passes in the number deleted so that the copy knows where to start in the old env */
  return(cp->edits[edpos]->beg + cp->edits[edpos]->len);
}


/* ---------------- edit list display, save, etc ---------------- */

char *edit_to_string(chan_info *cp, int edit)
{
  ed_list *ed;
  ed = cp->edits[edit];
  /* only for edit list in snd-g|xchn.c */

#if HAVE_FORTH
  return(mus_format("%s : %" print_mus_long " %" print_mus_long " %s", 
		    ed->origin, 
		    ed->beg, ed->len,
		    edit_names[(int)(ed->edit_type)]));
#endif
 
#if HAVE_RUBY
  return(mus_format("%s : %s(%" print_mus_long ", %" print_mus_long ")", 
		    ed->origin, 
		    edit_names[(int)(ed->edit_type)], 
		    ed->beg, ed->len));

#endif

#if HAVE_SCHEME
  return(mus_format("%s : (%s %" print_mus_long " %" print_mus_long ")", 
		    ed->origin, 
		    edit_names[(int)(ed->edit_type)], 
		    ed->beg, ed->len));
#endif

  return(NULL);
}


static void display_edits(chan_info *cp, FILE *outp)
{
  int i;
  fprintf(outp, "\nEDITS: %d\n", cp->edit_ctr);
  for (i = 0; i <= cp->edit_ctr; i++)
    display_ed_list(cp, outp, i, cp->edits[i]);
}


static io_error_t snd_make_file(const char *ofile, int chans, file_info *hdr, snd_fd **sfs, mus_long_t length, bool report_ok)
{
  /* create ofile, fill it by following sfs, use hdr for srate/type/format decisions */
  int ofd;
  int i, j, datumb;
  bool reporting = false;
  mus_long_t len = 0, total = 0, alloc_len;
  chan_info *cp = NULL;
  mus_float_t **obufs;
  io_error_t io_err = IO_NO_ERROR;
  int sl_err = MUS_NO_ERROR;
  bool need_clipping;

  ofd = open_temp_file(ofile, chans, hdr, &io_err);
  if (ofd == -1) 
    return(io_err);
  alloc_len = length;
  if (alloc_len > REPORTING_SIZE)
    alloc_len = REPORTING_SIZE;

  need_clipping = clipping(ss);
  if (need_clipping)
    {
      bool max_ok = false;
      mus_float_t mx = 0.0;
      for (i = 0; i < chans; i++)
	{
	  mus_float_t cur_mx;
	  chan_info *ncp;
	  int edpos;

	  ncp = sfs[i]->cp;
	  edpos = sfs[i]->edit_ctr;

	  if (peak_env_maxamp_ok(ncp, edpos))
	    cur_mx = peak_env_maxamp(ncp, edpos);
	  else
	    {
	      cur_mx = ed_maxamp(ncp, edpos);
	      if (cur_mx < 0.0) break;
	    }
	  
	  if (cur_mx > mx)
	    mx = cur_mx;
	  if (i == (chans - 1))
	    max_ok = true;
	}

      if ((max_ok) &&
	  (mx <= 1.0))
	need_clipping = false;
    }

  mus_file_set_clipping(ofd, need_clipping); /* clipping is very expensive, so try to avoid it */

  datumb = mus_bytes_per_sample(hdr->sample_type);
  ss->stopped_explicitly = false;

  obufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    obufs[i] = (mus_float_t *)calloc(alloc_len, sizeof(mus_float_t));
  j = 0;

  reporting = ((report_ok) && (length > alloc_len));
  if (reporting) 
    {
      cp = sfs[0]->cp;
      start_progress_report(cp);
    }

  if (chans == 1)
    {
      mus_float_t *buf;
      snd_fd *sf;
      buf = obufs[0];
      sf = sfs[0];
      if (length > alloc_len)
	{
	  sampler_set_safe(sf, length);
	  for (len = 0; len < length; )
	    {
	      int k, kdur;

	      kdur = length - len;
	      if (kdur > alloc_len) kdur = alloc_len;

	      for (k = 0; k < kdur; k++)
		buf[k] = read_sample(sf);

	      sl_err = mus_file_write(ofd, 0, kdur - 1, 1, obufs);
	      j = 0;
	      if (sl_err != MUS_NO_ERROR) break;
	      if (reporting)
		{
		  total += kdur;
		  progress_report(cp, (mus_float_t)((double)total / (double)length));
		}
	      len += kdur;
	    }
	}
      else
	{
	  samples_to_vct_with_reader(length, buf, sf);
	  len = length;
	  j = (int)length;
	}
    }
  else
    {
      if (length > alloc_len)
	{
	  mus_float_t *buf;
	  snd_fd *sf;

	  for (i = 0; i < chans; i++)
	    sampler_set_safe(sfs[i], length);

	  for (len = 0; len < length;)
	    {
	      int k, kdur;
	      kdur = length - len;
	      if (kdur > alloc_len) kdur = alloc_len;
	      
	      for (i = 0; i < chans; i++)
		{
		  buf = obufs[i];
		  sf = sfs[i];
		  for (k = 0; k < kdur; k++)
		    buf[k] = read_sample(sf);
		}

	      sl_err = mus_file_write(ofd, 0, kdur - 1, chans, obufs);
	      j = 0;
	      if (sl_err != MUS_NO_ERROR) break;
	      if (reporting)
		{
		  total += kdur;
		  progress_report(cp, (mus_float_t)((double)total / (double)length));
		}
	      len += kdur;
	    }
	}
      else
	{
	  for (i = 0; i < chans; i++)
	    samples_to_vct_with_reader(length, obufs[i], sfs[i]);
	  len = length;
	  j = (int)length;
	}
    }
  if ((sl_err == MUS_NO_ERROR) && 
      (j > 0))
    sl_err = mus_file_write(ofd, 0, j - 1, chans, obufs);
  if (sl_err == MUS_NO_ERROR)
    {
      io_err = close_temp_file(ofile, ofd, hdr->type, len * chans * datumb);
#if USE_MOTIF
      if (!(ss->file_monitor_ok))
	alert_new_file();
#endif
    }
  else 
    {
      mus_file_close(ofd);
      io_err = sndlib_error_to_snd(sl_err);
    }
  if (reporting) finish_progress_report(cp);
  for (i = 0; i < chans; i++) free(obufs[i]);
  free(obufs);
  return(io_err);
}


static io_error_t channel_to_file_with_bounds(chan_info *cp, const char *ofile, int edpos, mus_long_t beg, mus_long_t len, file_info *hdr, bool report_ok)
{
  snd_info *sp;
  snd_fd **sf;
  io_error_t err = IO_NO_ERROR;
  sp = cp->sound;
  sf = (snd_fd **)malloc(sizeof(snd_fd *));
  
  sf[0] = init_sample_read_any_with_bufsize(beg, cp, READ_FORWARD, edpos, len);
  if (!sf[0])
    {
      free(sf);
      snd_error("no such edit: %s[%d]: %d (this channel has %d edit%s",
		sp->short_filename,
		cp->chan,
		edpos,
		cp->edit_ctr,
		(cp->edit_ctr == 1) ? "" : "s");
    }
  else
    {
      err = snd_make_file(ofile, 1, hdr, sf, len, report_ok);
      free_snd_fd(sf[0]);
      free(sf);
      if ((err != IO_NO_ERROR) &&
	  (err != IO_INTERRUPTED))
	snd_error("can't save %s chan %d: %s %s", 
		  sp->short_filename,
		  cp->chan,
		  ofile,
		  snd_io_strerror());
    }
  return(err);
}


static io_error_t channel_to_file(chan_info *cp, const char *ofile, int edpos) /* preserves cp->sound's header settings */
{
  return(channel_to_file_with_bounds(cp, ofile, edpos, 0, cp->edits[edpos]->samples, cp->sound->hdr, true));
}


io_error_t channel_to_file_with_settings(chan_info *cp, const char *new_name, int srate, 
					 mus_sample_t samp_type, mus_header_t hd_type, const char *comment, int pos)
{ 
  file_info *hdr, *ohdr;
  snd_info *sp;
  io_error_t err = IO_NO_ERROR;
  sp = cp->sound;
  ohdr = sp->hdr;
  hdr = copy_header(new_name, ohdr);
  hdr->sample_type = samp_type;
  hdr->srate = srate;
  hdr->type = hd_type;
  if (comment) 
    hdr->comment = mus_strdup(comment); 
  else hdr->comment = NULL;
  hdr->data_location = 0; /* in case comment changes it */

  if (pos == AT_CURRENT_EDIT_POSITION)
    pos = cp->edit_ctr;

  if ((mus_strcmp(new_name, sp->filename)) &&      /* overwriting current file with one of its channels */
      ((sp->user_read_only == FILE_READ_ONLY) || 
       (sp->file_read_only == FILE_READ_ONLY)))
    {
      snd_error("can't save channel %d as %s (%s is write-protected)", cp->chan, new_name, sp->short_filename);
      return(IO_WRITE_PROTECTED);
    }

  err = channel_to_file_with_bounds(cp, new_name, pos, 0, cp->edits[pos]->samples, hdr, true);

  free_file_info(hdr);
  return(err);
}




/* these are used internally by the save-state process */
#define S_change_samples_with_origin    "change-samples-with-origin"
#define S_insert_samples_with_origin    "insert-samples-with-origin"
#define S_override_samples_with_origin  "override-samples-with-origin"

static void fprintf_with_possible_embedded_string(FILE *fd, const char *str)
{
  int i, len;
  len = mus_strlen(str);
  fputc('"', fd);
  for (i = 0; i < len; i++)
    {
      if (str[i] == '"')
	fputc('\\', fd);
      fputc(str[i], fd);
    }
  fputc('"', fd);
}


static char *edit_list_data_to_temp_file(chan_info *cp, ed_list *ed, file_delete_t delete_me, bool with_save_state_hook)
{
  snd_data *sd;
  char *ofile;
  if (with_save_state_hook)
    {
      char *nfile;
      nfile = shorter_tempnam(save_dir(ss), "snd_");
      ofile = run_save_state_hook(nfile);
      free(nfile);
    }
  else ofile = shorter_tempnam(save_dir(ss), "snd_");
  sd = cp->sounds[ed->sound_location];
  if (sd->type == SND_DATA_BUFFER)
    mus_array_to_file(ofile, sd->buffered_data, ed->len, DEFAULT_OUTPUT_SRATE, 1);
  else 
    {
      io_error_t io_err;
      io_err = copy_file(sd->filename, ofile);
      if (io_err != IO_NO_ERROR)
	{
	  if (io_err == IO_CANT_OPEN_FILE)
	    snd_warning("%s edit list original temp file %s: %s", io_error_name(io_err), sd->filename, snd_io_strerror());
	  else snd_warning("%s edit list saved temp file %s: %s", io_error_name(io_err), ofile, snd_io_strerror());
	}
    }
  if (delete_me == DELETE_ME) remember_temp(ofile, 1); /* deletion upon exit (forget_temps) if a temp (edit-list->function, but not save-state) */
  return(ofile);
}
  

#if HAVE_FORTH
/*
 * ret_name: last word in origin (function name)
 *     func: rest-origin (must be freed)
 */
static char *split_origin(char *origin, char **ret_name)
{
  if (origin && *origin)
    {
      char *func = (char *)calloc(strlen(origin), sizeof(char));
      if ((*ret_name = strrchr(origin, ' ')))
	{
	  (*ret_name)++;
	  memcpy(func, origin, strlen(origin) - strlen(*ret_name) - 1);
	}
      else *ret_name = origin;
      return(func);
    }
  return NULL;
}
#endif


void edit_history_to_file(FILE *fd, chan_info *cp, bool with_save_state_hook)
{
  /* write edit list as a scheme|ruby|forth program to fd (open for writing) for subsequent load */
  /*   the entire current list is written, then the edit_ctr is fixed up to reflect its current state */
  int i, edits;
#if HAVE_FORTH
  char *forth_func = NULL;
  bool mix_ed = false;
#endif
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  /* 0 case = open-sound */
  for (i = 1; i <= edits; i++)
    {
      ed_list *ed;
      ed = cp->edits[i];
      if (ed)
	{
          if (ed->backed_up && (ed->edit_type != MIX_EDIT))
	    {
	      /* as-one-edit (and internally backup_edit_list) remove edit history entries,
	       * making it impossible to reconstruct exactly the edit sequence in save/restore.
	       * The backed_up flag is set in the backed-up entry, and for save/restore, we
	       * override the entire current sound with a saved file.
	       */
	      char *nfile = NULL;
	      mus_long_t len;
	      io_error_t io_err;
	      if (with_save_state_hook)
		{
		  char *ofile;
		  ofile = shorter_tempnam(save_dir(ss), "snd_");
		  nfile = run_save_state_hook(ofile);
		  free(ofile);
		}
	      else nfile = shorter_tempnam(save_dir(ss), "snd_");
	      len = cp->edits[i]->samples;
	      io_err = channel_to_file(cp, nfile, i);

	      if (io_err != IO_NO_ERROR)
		{
		  /* error is trapped at lower level and pulled up via redirection */
		  free(nfile);
		  return;
		}
#if HAVE_RUBY
	      fprintf(fd, "      %s(\"%s\", %" print_mus_long ", sfile, %d, ", to_proc_name(S_override_samples_with_origin), nfile, len, cp->chan);
	      if (ed->origin) 
		fprintf_with_possible_embedded_string(fd, ed->origin);
	      else fprintf(fd, "\"\"");
 	      fprintf(fd, ", [%d, %" print_mus_long "])\n",
 		      (int)mus_sound_write_date(nfile),
 		      mus_sound_length(nfile));
#endif
#if HAVE_SCHEME
	      fprintf(fd, "      (%s \"%s\" %" print_mus_long " sfile %d ", S_override_samples_with_origin, nfile, len, cp->chan);
	      if (ed->origin) 
		fprintf_with_possible_embedded_string(fd, ed->origin);
	      else fprintf(fd, "\"\"");
	      fprintf(fd, " (list %d %" print_mus_long "))\n",
		      (int)mus_sound_write_date(nfile),
		      mus_sound_length(nfile));
#endif
#if HAVE_FORTH
	      fprintf(fd, "      \"%s\" %" print_mus_long " sfile %d ", nfile, len, cp->chan);
	      if (ed->origin) 
		fprintf_with_possible_embedded_string(fd, ed->origin);
	      else fprintf(fd, "\"\"");
 	      fprintf(fd, " '( %d %" print_mus_long " ) %s drop\n",
 		      (int)mus_sound_write_date(nfile),
 		      mus_sound_length(nfile),
		      S_override_samples_with_origin);
#endif
	      free(nfile);
	    }
	  else
	    {
	      char *nfile = NULL;
#if HAVE_RUBY || HAVE_FORTH
	      fprintf(fd, "      ");
#endif
#if HAVE_SCHEME
	      fprintf(fd, "      (");
#endif
#if HAVE_FORTH
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
		  /* samp data snd chn */
		  forth_func = S_insert_samples_with_origin;
		  fprintf(fd, "%" print_mus_long " %" print_mus_long " ",
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"%s\"", S_insert_samples);
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, " \"%s\" sfile %d", nfile, cp->chan);
		  break;

		case DELETION_EDIT:
		  /* samp samps snd chn */
		  forth_func = S_delete_samples;
		  fprintf(fd, "%" print_mus_long " %" print_mus_long " sfile %d",
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;

		case CHANGE_EDIT:
		  forth_func = S_change_samples_with_origin;
		  fprintf(fd, "%" print_mus_long " %" print_mus_long " ",
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"\"");
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, " \"%s\" sfile %d", nfile, cp->chan);
		  break;

		case EXTEND_EDIT:
		  /* not currently savable (this is a dummy edit fragment for zero-mix-drag position change) */
		  break;

		case ZERO_EDIT:
		  forth_func = S_pad_channel;
		  fprintf(fd, "%" print_mus_long " %" print_mus_long " sfile %d",
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;

		case SCALED_EDIT:
		case RAMP_EDIT:
		  {
		    char *func;
		    if ((func = split_origin(ed->origin, &forth_func)))
		      {
			fprintf(fd, "%s sfile %d", func, cp->chan);
			free(func);
		      }
		    else fprintf(fd, "sfile %d", cp->chan);
		  }
		  break;

		case MIX_EDIT:
		case CHANGE_MIX_EDIT:
		  mix_ed = true;
		  fprintf(fd, "sfile value snd\n");
		  fprintf(fd, "      %d     value chn\n", cp->chan);
		  fprintf(fd, "      ");
		  forth_func = ed->origin;
		  break;

		default:
		  snd_error("unknown edit branch: %s: %d %d",
			    ed->origin, 
			    ed->edit_type,
			    ed->sound_location);
		  break;
		}
#else
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
		  /* samp data snd chn */
		  fprintf(fd, "%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long PROC_SEP,
			  to_proc_name(S_insert_samples_with_origin),
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"%s\"", S_insert_samples);
		  fprintf(fd, PROC_SEP);
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, "\"%s\"" PROC_SEP "sfile" PROC_SEP "%d", nfile, cp->chan);
		  break;

		case DELETION_EDIT:
		  /* samp samps snd chn */
		  fprintf(fd, "%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long PROC_SEP "sfile" PROC_SEP "%d",
			  to_proc_name(S_delete_samples),
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;

		case CHANGE_EDIT:
		  fprintf(fd, "%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long PROC_SEP,
			  to_proc_name(S_change_samples_with_origin),
			  ed->beg,
			  ed->len);
		  if (ed->origin)
		    fprintf_with_possible_embedded_string(fd, ed->origin);
		  else fprintf(fd, "\"\"");
		  fprintf(fd, PROC_SEP);
		  nfile = edit_list_data_to_temp_file(cp, ed, DONT_DELETE_ME, with_save_state_hook);
		  fprintf(fd, "\"%s\"" PROC_SEP "sfile" PROC_SEP "%d", nfile, cp->chan);
		  break;

		case EXTEND_EDIT:
		  /* not currently savable (this is a dummy edit fragment for zero-mix-drag position change) */
		  break;

		case SCALED_EDIT: 
		  fprintf(fd, "%s" PROC_SEP "sfile" PROC_SEP "%d",
			  ed->origin, /* imports scaler */
			  cp->chan);
		  break;

		case ZERO_EDIT:
		  fprintf(fd, "%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long PROC_SEP "sfile" PROC_SEP "%d",
			  to_proc_name(S_pad_channel),
			  ed->beg,
			  ed->len,
			  cp->chan);
		  break;

		case RAMP_EDIT:
		  fprintf(fd, "%s" PROC_SEP "sfile" PROC_SEP "%d",
			  ed->origin,
			  cp->chan);
		  break;

		case MIX_EDIT:
#if HAVE_SCHEME
		  fprintf(fd, "(lambda (snd chn ignore) %s) sfile %d", ed->origin, cp->chan);
#else
		  fprintf(fd, "func = lambda do |snd, chn, ignore|\n        %s\n        end\n      func(sfile, %d", ed->origin, cp->chan);
#endif
		  break;

		case CHANGE_MIX_EDIT:
		  fprintf(fd, "begin %s", ed->origin);
		  break;

		default:
		  snd_error("unknown edit branch: %s: %d %d",
			    ed->origin, 
			    ed->edit_type,
			    ed->sound_location);
		  break;
		}
#endif
	      if ((ed->edpos != AT_CURRENT_EDIT_POSITION) &&
		  (ed->edpos != (i - 1)))
		fprintf(fd, PROC_SEP " %d", ed->edpos);
#if HAVE_RUBY
	      else fprintf(fd, ", false");
#endif
#if HAVE_SCHEME
	      else fprintf(fd, " #f");
#endif
#if HAVE_FORTH
	      else
		{
		  if (!mix_ed)
		    fprintf(fd, " #f");
		}
#endif
	      if (nfile) 
		{
#if HAVE_SCHEME
		  fprintf(fd, " (list %d %" print_mus_long ")",
			  (int)mus_sound_write_date(nfile),
			  mus_sound_length(nfile));
#endif
#if HAVE_RUBY
 		  fprintf(fd, ", [%d, %" print_mus_long "]",
  			  (int)mus_sound_write_date(nfile),
  			  mus_sound_length(nfile));
#endif
#if HAVE_FORTH
		  fprintf(fd, " '( %d %" print_mus_long " )",
			  (int)mus_sound_write_date(nfile),
			  mus_sound_length(nfile));
#endif
		  free(nfile);
		}
#if HAVE_FORTH
	      if (mix_ed)
		fprintf(fd, " %s\n", forth_func);
	      else fprintf(fd, " %s drop\n", forth_func);
#else
	      fprintf(fd, ")\n"); /* works for both Ruby and Scheme */
#endif
	    }
	}
    }
  if (cp->edit_ctr < edits) 
#if HAVE_RUBY
    fprintf(fd, "      undo(%d, sfile, %d);\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
#if HAVE_SCHEME
    fprintf(fd, "      (undo %d sfile %d)\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
#if HAVE_FORTH
    fprintf(fd, "      %d sfile %d undo drop\n",
	    edits - cp->edit_ctr,
	    cp->chan);
#endif
    save_mark_list(fd, cp, false); /* false -> save just the current channel's marks */
}


char *edit_list_to_function(chan_info *cp, int start_pos, int end_pos)
{
#if HAVE_SCHEME
  char *function = NULL, *old_function = NULL;
  bool close_mix_let = false;
  int i, edits;

  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;

  if ((end_pos > 0) &&    /* end_pos can be -1 = end of edits (?) */
      (end_pos < edits)) 
    edits = end_pos;

  if (start_pos > edits)
    return(mus_strdup("(lambda (snd chn) #f)"));
  if (start_pos == 0) start_pos = 1;

  if (channel_has_mixes(cp))
    {
      char *mix_list;
      mix_list = edit_list_mix_init(cp);
      if (mix_list)
	{
	  close_mix_let = true;
	  function = mus_format("(lambda (snd chn)\n  (let (%s)", mix_list);
	  free(mix_list);
	}
      else function = mus_strdup("(lambda (snd chn)");
    }
  else function = mus_strdup("(lambda (snd chn)");
  
  for (i = start_pos; i <= edits; i++)
    {
      ed_list *ed;
      ed = cp->edits[i];
      if (ed)
	{
	  old_function = function;
	  function = NULL;

	  /* most of these depend on the caller to supply a usable re-call string (origin). */
	  /*   In insert/change cases, there's basically no choice */
	  if (ed->backed_up)
	    {
	      if ((ed->origin) && 
		  (strncmp(ed->origin, "set!", 4) == 0))
		function = mus_format("%s\n  (%s)", old_function, ed->origin);
	      else function = mus_format("%s\n  (%s snd chn)", old_function, ed->origin);
	    }
	  else
	    {
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
		  /* this and change_edit are not bullet-proof -- there are many ways an incomplete
		   *   origin can get here, but we want to trap the mix setters.  In save-state above,
		   *   origin is just ignored, which is also less than ideal, but there are cases
		   *   (map-channel for example) where the lambda form can't be saved correctly,
		   *   so "the right thing" is not reachable.  Here, perhaps the strcmp should
		   *   check for "set! -mix" or "set! (". 
		   */
		  if ((!(ed->origin)) || 
		      (strcmp(ed->origin, S_insert_samples) == 0))
		    {
		      /* save data in temp file, use insert-samples with file name */
		      char *ofile;
		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
		      function = mus_format("%s\n  (%s %" print_mus_long " %" print_mus_long " \"%s\" snd chn)", old_function, S_insert_samples, ed->beg, ed->len, ofile);
		      free(ofile);
		    }
		  else function = mus_format("%s\n  (%s snd chn)", old_function, ed->origin);
		  break;

		case CHANGE_EDIT:
		  if ((!(ed->origin)) || 
		      (strcmp(ed->origin, "set-samples") == 0))
		    {
		      /* save data in temp file, use set-samples with file name */
		      char *ofile;
		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
		      function = mus_format("%s\n  (set-samples %" print_mus_long " %" print_mus_long " \"%s\" snd chn)", old_function, ed->beg, ed->len, ofile);
		      free(ofile);
		    }
		  else
		    {
		      if (strncmp(ed->origin, "set!", 4) == 0)
			function = mus_format("%s\n  (%s)", old_function, ed->origin);
		      else function = mus_format("%s\n  (%s snd chn)", old_function, ed->origin);
		    }
		  break;

		case DELETION_EDIT:
		  function = mus_format("%s\n  (%s %" print_mus_long " %" print_mus_long " snd chn)", old_function, S_delete_samples, ed->beg, ed->len);
		  break;

		case SCALED_EDIT: 
		  function = mus_format("%s\n  (%s snd chn)", old_function, ed->origin);
		  break;

		case EXTEND_EDIT:
		  /* mix drag case */
		  break;

		case RAMP_EDIT:
		  function = mus_format("%s\n  (%s snd chn)", old_function, ed->origin);
		  break;

		case ZERO_EDIT:
		  /* origin here is useless (see extend_with_zeros cases) */
		  function = mus_format("%s\n  (%s %" print_mus_long " %" print_mus_long " snd chn)", old_function, S_pad_channel, ed->beg, ed->len);
		  break;

		case MIX_EDIT:
		  function = mus_format("%s\n  %s", old_function, ed->origin);
		  break;

		case CHANGE_MIX_EDIT:
		  function = mus_format("%s\n  %s", old_function, ed->origin);
		  break;

		default: 
		  break;
		}
	    }
	  if (old_function) {free(old_function); old_function = NULL;}
	}
    }

  old_function = function;
  if (close_mix_let)
    function = mus_format("%s))", old_function);
  else function = mus_format("%s)", old_function);
  free(old_function);
  return(function);
#endif

#if HAVE_RUBY
  char *function = NULL, *old_function = NULL;
  bool close_mix_let = false, first = true;
  int i, edits;
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  if ((end_pos > 0) && (end_pos < edits)) edits = end_pos;
  if (start_pos > edits)
    return(mus_strdup("Proc.new {|snd, chn| false }"));
  if (channel_has_mixes(cp))
    {
      char *mix_list;
      mix_list = edit_list_mix_init(cp);
      if (mix_list)
	{
	  close_mix_let = true;
	  function = mus_format("Proc.new {|snd, chn| %s; ", mix_list);
	  free(mix_list);
	}
      else function = mus_strdup("Proc.new {|snd, chn| ");
    }
  else function = mus_strdup("Proc.new {|snd, chn| ");
  for (i = start_pos; i <= edits; i++)
    {
      ed_list *ed;
      ed = cp->edits[i];
      if (ed)
	{
	  old_function = function;
	  /* most of these depend on the caller to supply a usable re-call string (origin). */
	  /*   In insert/change/ cases, there's basically no choice */
	  if (ed->backed_up)
  	    {
 	      if ((ed->origin) &&
 		  (strncmp(ed->origin, "set_mix", 7) == 0))
  		function = mus_format("%s%s %s", function, (first) ? "" : ";", ed->origin);
 	      else function = mus_format("%s%s %s%ssnd, chn)",
 					 function,
 					 (first) ? "" : ";",
 					 ed->origin,
 					 (ed->origin[mus_strlen(ed->origin) - 1] == '(') ? "" : ", ");
  	    }
	  else
	    {
	      switch (ed->edit_type)
		{
		case INSERTION_EDIT: 
 		  if ((!(ed->origin)) || 
		      (strcmp(ed->origin, to_proc_name(S_insert_samples)) == 0))
 		    {
 		      /* from HAVE_SCHEME above */
 		      /* save data in temp file, use insert-samples with file name */
 		      char *ofile;
 		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
 		      function = mus_format("%s %s(%" print_mus_long ", %" print_mus_long ", \"%s\", snd, chn)", 
					    function, to_proc_name(S_insert_samples), ed->beg, ed->len, ofile);
 		      free(ofile);
 		    }
 		  else function = mus_format("%s%s %s, snd, chn)", function, (first) ? "" : ";", ed->origin);
  		  break;

  		case CHANGE_EDIT:
 		  if ((!(ed->origin)) || 
		      (strcmp(ed->origin, "set-samples") == 0))
 		    {
 		      /* from HAVE_SCHEME above */
 		      /* save data in temp file, use set-samples with file name */
 		      char *ofile;
 		      ofile = edit_list_data_to_temp_file(cp, ed, DELETE_ME, false);
 		      function = mus_format("%s set_samples(%" print_mus_long ", %" print_mus_long ", \"%s\", snd, chn)", 
					    function, ed->beg, ed->len, ofile);
 		      free(ofile);
 		    }
 		  else if ((ed->origin) &&
 			   (strncmp(ed->origin, "set_mix", 7) == 0))
 		    function = mus_format("%s%s %s", function, (first) ? "" : ";", ed->origin);
 		  else function = mus_format("%s%s %s%ssnd, chn)",
 					     function,
 					     (first) ? "" : ";",
 					     ed->origin,
 					     (ed->origin[mus_strlen(ed->origin) - 1] == '(') ? "" : ", ");
		  break;

		case DELETION_EDIT:
		  function = mus_format("%s%s %s(%" print_mus_long ", %" print_mus_long ", snd, chn)", 
					function, (first) ? "" : ";", to_proc_name(S_delete_samples), ed->beg, ed->len);
		  break;

		case SCALED_EDIT: 
		  function = mus_format("%s%s %s, snd, chn)", function, (first) ? "" : ";", ed->origin);
		  break;

		case EXTEND_EDIT:
		  /* mix drag case */
		  break;

		case RAMP_EDIT:
		  function = mus_format("%s%s %s, snd, chn)", function, (first) ? "" : ";", ed->origin);
		  break;

		case ZERO_EDIT:
		  /* origin here is useless (see extend_with_zeros cases) */
		  function = mus_format("%s%s %s(%" print_mus_long ", %" print_mus_long ", snd, chn)", 
					function, (first) ? "" : ";", to_proc_name(S_pad_channel), ed->beg, ed->len);
		  break;

		case MIX_EDIT:
		  function = mus_format("%s%s %s", function, (first) ? "" : ";", ed->origin);
		  break;

		case CHANGE_MIX_EDIT:
		  function = mus_format("%s%s %s", function, (first) ? "" : ";", ed->origin);
		  break;

		default: break;
		}
	    }
	  if (old_function) {free(old_function); old_function = NULL;}
	}
      first = false;
    }
  old_function = function;
  if (close_mix_let)
    function = mus_format("%s }", function);
  else function = mus_format("%s }", function);
  free(old_function);
  return(function);
#endif

#if HAVE_FORTH
  char *function = NULL, *old_function = NULL;
  int i, edits;
  edits = cp->edit_ctr;
  while ((edits < (cp->edit_size - 1)) && 
	 (cp->edits[edits + 1])) 
    edits++;
  if ((end_pos > 0) && (end_pos < edits)) edits = end_pos;
  if (start_pos > edits)
    return(mus_strdup("lambda: <{ snd chn -- val }> #f ;"));
  if (channel_has_mixes(cp))
    {
      char *mix_list;
      mix_list = edit_list_mix_init(cp);
      if (mix_list)
	{
	  function = mus_format("lambda: <{ snd chn -- val }> %s", mix_list);
	  free(mix_list);
	}
      else function = mus_strdup("lambda: <{ snd chn -- val }>");
    }
  else function = mus_strdup("lambda: <{ snd chn -- val }>");
  for (i = start_pos; i <= edits; i++)
    {
      ed_list *ed;
      ed = cp->edits[i];
      if (ed)
	{
	  old_function = function;
	  /* most of these depend on the caller to supply a usable re-call string (origin). */
	  /*   In insert/change cases, there's basically no choice */
	  if (ed->backed_up)
	    {
	      char *name, *func;
	      func = split_origin(ed->origin, &name);
	      if ((name) && (strncmp(name, "set-", 4) == 0))
		function = mus_format("%s %s drop", function, ed->origin);
	      else if ((ed->origin) && strstr(ed->origin, "mix-selection"))
		function = mus_format("%s %s", function, ed->origin);
	      else
		{
		  if (func)
		    function = mus_format("%s %s snd chn %s drop", function, func, name);
		  else function = mus_format("%s snd chn %s drop", function, name);
		}
	      if (func) free(func);
	    }
	  else
	    {
	      switch (ed->edit_type)
		{
		case CHANGE_EDIT:
		  {
		    char *name, *func;
		    func = split_origin(ed->origin, &name);
		    if ((name) && (strncmp(name, "set-", 4) == 0))
		      function = mus_format("%s %s drop", function, ed->origin);
		    else if ((ed->origin) && strstr(ed->origin, "mix-selection"))
		      function = mus_format("%s %s", function, ed->origin);
		    else
		      {
			if (func)
			  function = mus_format("%s %s snd chn %s drop", function, func, name);
			else function = mus_format("%s snd chn %s drop", function, name);
		      }
		    if (func) free(func);
		  }
		  break;
		case DELETION_EDIT:
		  function = mus_format("%s %" print_mus_long " %" print_mus_long " snd chn %s drop", 
					function, ed->beg, ed->len, S_delete_samples);
		  break;
		case INSERTION_EDIT: 
		case SCALED_EDIT: 
		case RAMP_EDIT:
		  {
		    char *name, *func;
		    if ((func = split_origin(ed->origin, &name)))
		      {
			function = mus_format("%s %s snd chn %s drop", function, func, name);
			free(func);
		      }
		    else function = mus_format("%s snd chn %s drop", function, name);
		  }
		  break;
		case EXTEND_EDIT:
		  /* mix drag case */
		  break;
		case ZERO_EDIT:
		  /* origin here is unpredictable -- most of these extensions should be backed-over and invisible */
		  /*   the one case that should survive (pad-channel) just passes its name as the origin */
		  function = mus_format("%s %" print_mus_long " %" print_mus_long " snd chn %s drop", 
					function, ed->beg, ed->len, S_pad_channel);
		  break;

		case MIX_EDIT:
		  function = mus_format("%s %s", function, ed->origin);
		  break;

		case CHANGE_MIX_EDIT:
		  function = mus_format("%s %s", function, ed->origin);
		  break;

		default: break;
		}
	    }
	  if (old_function) {free(old_function); old_function = NULL;}
	}
    }
  old_function = function;
  function = mus_format("%s ;", function);
  free(old_function);
  return(function);
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
  return(NULL);
#endif
}


static ed_fragment *fragment_free_list = NULL;

static ed_fragment *make_ed_fragment(void)
{
  if (fragment_free_list)
    {
      ed_fragment *e;
      e = fragment_free_list;
      fragment_free_list = (ed_fragment *)(fragment_free_list->next);
      memset((void *)e, 0, sizeof(ed_fragment));
      return(e);
    }
  return((ed_fragment *)calloc(1, sizeof(ed_fragment)));
}


static ed_mixes *copy_fragment_mixes(ed_mixes *old_mixes)
{
  ed_mixes *ed;
  ed = (ed_mixes *)calloc(1, sizeof(ed_mixes));
  ed->size = old_mixes->size;
  ed->mix_list = (mix_state **)malloc(ed->size * sizeof(mix_state *));
  /* pointer fixup is in ripple, but we need access to the old choice */
  memcpy((void *)(ed->mix_list), (void *)(old_mixes->mix_list), ed->size * sizeof(mix_state *));
  return(ed);
}


static ed_ramps *copy_fragment_ramps(ed_ramps *old_ramps)
{
  ed_ramps *new_ramps;
  new_ramps = (ed_ramps *)malloc(sizeof(ed_ramps));
  new_ramps->ramps = old_ramps->ramps;
  new_ramps->xramps = old_ramps->xramps;
  if (new_ramps->ramps > 0)
    {
      new_ramps->ramp_list = (ramp_state *)malloc(new_ramps->ramps * sizeof(ramp_state));
      memcpy((void *)(new_ramps->ramp_list), (void *)(old_ramps->ramp_list), new_ramps->ramps * sizeof(ramp_state));
    }
  else new_ramps->ramp_list = NULL;
  if (new_ramps->xramps > 0)
    {
      new_ramps->xramp_list = (xramp_state *)malloc(new_ramps->xramps * sizeof(xramp_state));
      memcpy((void *)(new_ramps->xramp_list), (void *)(old_ramps->xramp_list), new_ramps->xramps * sizeof(xramp_state));
    }
  else new_ramps->xramp_list = NULL;
  return(new_ramps);
}


static void copy_ed_fragment(ed_fragment *new_ed, ed_fragment *old_ed)
{
  new_ed->typ = old_ed->typ;
  new_ed->snd = old_ed->snd;
  new_ed->out = old_ed->out;
  new_ed->beg = old_ed->beg;
  new_ed->end = old_ed->end;
  new_ed->scl = old_ed->scl;

  if (ED_RAMPS(old_ed))
    ED_RAMPS(new_ed) = copy_fragment_ramps(ED_RAMPS(old_ed));

  if (ED_MIXES(old_ed))
    ED_MIXES(new_ed) = copy_fragment_mixes(ED_MIXES(old_ed));
}


static void copy_checked_ed_fragment(ed_fragment *new_ed, ed_fragment *old_ed)
{
  /* insert list special case */
  if ((ED_RAMPS(old_ed)) &&
      (ED_RAMPS(new_ed)))
    {
      if (ED_RAMP_LIST(new_ed))	free(ED_RAMP_LIST(new_ed));
      if (ED_XRAMP_LIST(new_ed)) free(ED_XRAMP_LIST(new_ed));
      free(ED_RAMPS(new_ed));
    }

  if ((ED_MIXES(old_ed)) &&
      (ED_MIXES(new_ed)))
    {
      if (ED_MIX_LIST(new_ed)) free(ED_MIX_LIST(new_ed));
      free(ED_MIXES(new_ed));
    }

  copy_ed_fragment(new_ed, old_ed);
}


static void clear_ed_fragment(ed_fragment *ed)
{
  /* used by free_ed_fragment and when zeroing a copied fragment */

  if (ED_RAMPS(ed))
    {
      if (ED_RAMP_LIST(ed)) free(ED_RAMP_LIST(ed));
      if (ED_XRAMP_LIST(ed)) free(ED_XRAMP_LIST(ed));
      free(ED_RAMPS(ed));
      ED_RAMPS(ed) = NULL;
    }

  if (ED_MIXES(ed)) 
    {
      if (ED_MIX_LIST(ed)) free(ED_MIX_LIST(ed));
      free(ED_MIXES(ed));
      ED_MIXES(ed) = NULL;
    }
}


static ed_fragment *free_ed_fragment(ed_fragment *ed)
{
  if (ed)
    {
      clear_ed_fragment(ed);
      ed->next = (struct ed_fragment *)fragment_free_list;
      fragment_free_list = ed;
    }
  return(NULL);
}


static ed_list *make_ed_list(int size)
{
  ed_list *ed;
  int i;
  ed = (ed_list *)calloc(1, sizeof(ed_list));

  ed->size = size;
  ed->allocated_size = size;
  ed->fragments = (ed_fragment **)malloc(size * sizeof(ed_fragment *)); /* can't use malloc/free -- compiler dislikes the assignment in free */
  for (i = 0; i < size; i++)
    FRAGMENT(ed, i) = make_ed_fragment();

  ed->origin = NULL;
  ed->maxamp = -1.0;
  ed->maxamp_position = -1;
  ed->selection_maxamp = -1.0;
  ed->selection_maxamp_position = -1;
  ed->properties_gc_loc = NOT_A_GC_LOC;
  ed->properties = Xen_false;
  return(ed);
}


void set_ed_maxamp(chan_info *cp, int edpos, mus_float_t val)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  ed->maxamp = val;
}


mus_float_t ed_maxamp(chan_info *cp, int edpos)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  return(ed->maxamp);
}


void set_ed_maxamp_position(chan_info *cp, int edpos, mus_long_t val)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  ed->maxamp_position = val;
}


mus_long_t ed_maxamp_position(chan_info *cp, int edpos)
{
  ed_list *ed;
  ed = cp->edits[edpos];
  return(ed->maxamp_position);
}


void set_ed_selection_maxamp(chan_info *cp, mus_float_t val)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  ed->selection_maxamp = val;
}


mus_float_t ed_selection_maxamp(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return(ed->selection_maxamp);
}


void set_ed_selection_maxamp_position(chan_info *cp, mus_long_t val)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  ed->selection_maxamp_position = val;
}


mus_long_t ed_selection_maxamp_position(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return(ed->selection_maxamp_position);
}


typedef struct {
  snd_fd **rds;
  int size;
} sf_info;

static ed_list *free_ed_list(ed_list *ed, chan_info *cp)
{
  if (ed)
    {
      if (FRAGMENTS(ed)) 
	{
	  int i;
	  for (i = 0; i < ed->allocated_size; i++)
	    free_ed_fragment(FRAGMENT(ed, i));
	  free(FRAGMENTS(ed));
	}
      if (ed->origin) 
	{
	  free(ed->origin);
	  ed->origin = NULL;
	}
      if (ed->marks)
	free_mark_list(ed);
      if (ed->peak_env)
	ed->peak_env = free_peak_env_info(ed->peak_env);
      if (ed->fft)
	ed->fft = free_enved_fft(ed->fft);
      if (ed->readers)
	{
	  int i;
	  sf_info *lst;
	  lst = (sf_info *)(ed->readers);
	  for (i = 0; i < lst->size; i++)
	    if (lst->rds[i])
	      {
		reader_out_of_data(lst->rds[i]);
		lst->rds[i]->current_state = NULL; /* this pointer is now being freed, so it can't be safe to leave it around */
		lst->rds[i]->cb = NULL;
		lst->rds[i] = NULL;
	      }
	  free(lst->rds);
	  free(lst);
	  ed->readers = NULL;
	}
      if (ed->properties_gc_loc != NOT_A_GC_LOC)
	{
	  snd_unprotect_at(ed->properties_gc_loc);
	  ed->properties_gc_loc = NOT_A_GC_LOC;
	  ed->properties = Xen_false;
	}
      if (ED_MIXES(ed))
	{
	  free_ed_mixes(ED_MIXES(ed));
	  ED_MIXES(ed) = NULL;
	}
      free(ed);
    }
  return(NULL);
}


static void backup_edit_list_1(chan_info *cp, bool freeing)
{
  int cur, i;
  ed_list *old_ed, *new_ed;
  mus_long_t old_end, new_end;
  ed_fragment *top = NULL;

  cur = cp->edit_ctr;
  if (cur <= 0) return;
  new_ed = cp->edits[cur];
  old_ed = cp->edits[cur - 1];
  new_ed->edpos = old_ed->edpos;
  new_ed->backed_up = true;
  
  old_end = old_ed->beg + old_ed->len;
  new_end = new_ed->beg + new_ed->len;
  if (old_end > new_end) new_end = old_end;
  if (old_ed->beg < new_ed->beg) new_ed->beg = old_ed->beg;
  new_ed->len = new_end - new_ed->beg + 1;

  if (freeing)
    top = fragment_free_list;
  free_ed_list(old_ed, cp);
  if (freeing)
    {
      while ((fragment_free_list) && (fragment_free_list != top))
	{
	  ed_fragment *e;
	  e = fragment_free_list;
	  fragment_free_list = e->next;
	  free(e);
	}
    }

  cp->edits[cur - 1] = new_ed;
  cp->edits[cur] = NULL;
  if (cp->sounds) /* protect from release_pending_sounds upon edit after undo after as-one-edit or whatever */
    for (i = 0; i < cp->sound_size; i++)
      {
	snd_data *sd;
	sd = cp->sounds[i];
	if ((sd) && (sd->edit_ctr == cur)) sd->edit_ctr--;
      }
  cp->edit_ctr--;
  reflect_edit_history_change(cp);
}


void backup_edit_list(chan_info *cp)
{
  backup_edit_list_1(cp, false);
}

void free_edit_list(chan_info *cp)
{
  if (cp)
    {
      if (cp->edits)
	{
	  int i;
	  for (i = 0; i < cp->edit_size; i++)
	    if (cp->edits[i]) 
	      cp->edits[i] = free_ed_list(cp->edits[i], cp);
	  free(cp->edits);
	  cp->edits = NULL;
	}
      cp->edit_ctr = -1;
      cp->edit_size = 0;
    }
}


ed_list *initial_ed_list(mus_long_t beg, mus_long_t end)
{
  ed_list *ed;
  ed = make_ed_list(2);
  ed->beg = beg;
  ed->len = end + 1;
  ed->selection_beg = NO_SELECTION;
  ed->selection_end = 0;
  ed->edit_type = INITIALIZE_EDIT;
  ed->sound_location = 0;
  ed->samples = end + 1;

  /* origin (channel %s %d) desc channel should be obvious from context */
  FRAGMENT_LOCAL_POSITION(ed, 0) = beg;
  FRAGMENT_LOCAL_END(ed, 0) = end;
  FRAGMENT_SCALER(ed, 0) = 1.0;
  FRAGMENT_TYPE(ed, 0) = ED_SIMPLE;
  if (ed->len > 0)
    {
      /* second block is our end-of-tree marker */
      FRAGMENT_SOUND(ed, 1) = EDIT_LIST_END_MARK;
      FRAGMENT_GLOBAL_POSITION(ed, 1) = end + 1;
    }
  else
    {
      FRAGMENT_SOUND(ed, 0) = EDIT_LIST_END_MARK;
      ed->size = 1;
    }
  return(ed);
}


snd_info *sound_is_silence(snd_info *sp)
{
  if (sp)
    {
      uint32_t i;
      for (i = 0; i < sp->nchans; i++)
	{
	  chan_info *cp;
	  ed_list *ed;
	  cp = sp->chans[i];
	  ed = cp->edits[0];
	  FRAGMENT_SCALER(ed, 0) = 0.0;
	  FRAGMENT_TYPE(ed, 0) = ED_ZERO;
	}
    }
  return(sp);
}


static void ensure_ed_ramps(ed_fragment *ed, int rmps, int xrmps)
{
  if (!(ED_RAMPS(ed)))
    {
      ED_RAMPS(ed) = (ed_ramps *)calloc(1, sizeof(ed_ramps));
      if (rmps > 0)
	{
	  ED_RAMP_LIST_SIZE(ed) = rmps;
	  ED_RAMP_LIST(ed) = (ramp_state *)calloc(rmps, sizeof(ramp_state));
	}
      if (xrmps > 0)
	{
	  ED_XRAMP_LIST_SIZE(ed) = xrmps;
	  ED_XRAMP_LIST(ed) = (xramp_state *)calloc(xrmps, sizeof(xramp_state));
	}
    }
  else
    {
      if (rmps > ED_RAMP_LIST_SIZE(ed))
	{
	  if (ED_RAMP_LIST_SIZE(ed) == 0)
	    ED_RAMP_LIST(ed) = (ramp_state *)calloc(rmps, sizeof(ramp_state));
	  else ED_RAMP_LIST(ed) = (ramp_state *)realloc(ED_RAMP_LIST(ed), rmps * sizeof(ramp_state));
	  ED_RAMP_LIST_SIZE(ed) = rmps;

	}
      if (xrmps > ED_XRAMP_LIST_SIZE(ed))
	{
	  if (ED_XRAMP_LIST_SIZE(ed) == 0)
	    ED_XRAMP_LIST(ed) = (xramp_state *)calloc(xrmps, sizeof(xramp_state));
	  else ED_XRAMP_LIST(ed) = (xramp_state *)realloc(ED_XRAMP_LIST(ed), xrmps * sizeof(xramp_state));
	  ED_XRAMP_LIST_SIZE(ed) = xrmps;
	}
    }
}


static void new_before_ramp(ed_fragment *new_before, ed_fragment *old_before)
{
  if (ramp_op(ED_TYPE(old_before)))
    {
      int i, rmps, xrmps;
      rmps = ED_RAMP_LIST_SIZE(old_before);
      xrmps = ED_XRAMP_LIST_SIZE(old_before);

      ensure_ed_ramps(new_before, rmps, xrmps);

      for (i = 0; i < rmps; i++)
	{
	  ED_RAMP_INCR(new_before, i) = ED_RAMP_INCR(old_before, i);
	  ED_RAMP_START(new_before, i) = ED_RAMP_START(old_before, i);
	}

      for (i = 0; i < xrmps; i++)
	{
	  ED_XRAMP_OFFSET(new_before, i) = ED_XRAMP_OFFSET(old_before, i);
	  ED_XRAMP_SCALER(new_before, i) = ED_XRAMP_SCALER(old_before, i);
	  ED_XRAMP_INCR(new_before, i) = ED_XRAMP_INCR(old_before, i);
	  ED_XRAMP_START(new_before, i) = ED_XRAMP_START(old_before, i);
	}
    }
}


static void new_after_ramp(ed_fragment *new_after, ed_fragment *old_after, mus_long_t samp)
{
  mus_long_t dur;
  double d_dur;

  dur = samp - ED_GLOBAL_POSITION(old_after);
  d_dur = (double)dur;

  if (ramp_op(ED_TYPE(old_after)))
    {
      int i, rmps, xrmps;
      rmps = ED_RAMP_LIST_SIZE(old_after);
      xrmps = ED_XRAMP_LIST_SIZE(old_after);

      ensure_ed_ramps(new_after, rmps, xrmps);

      for (i = 0; i < rmps; i++)
	{
	  ED_RAMP_INCR(new_after, i) = ED_RAMP_INCR(old_after, i);
	  ED_RAMP_START(new_after, i) = ED_RAMP_START(old_after, i) + ED_RAMP_INCR(old_after, i) * d_dur;
	}

      for (i = 0; i < xrmps; i++)
	{
	  ED_XRAMP_OFFSET(new_after, i) = ED_XRAMP_OFFSET(old_after, i);
	  ED_XRAMP_SCALER(new_after, i) = ED_XRAMP_SCALER(old_after, i);
	  ED_XRAMP_INCR(new_after, i) = ED_XRAMP_INCR(old_after, i);
	  ED_XRAMP_START(new_after, i) = ED_XRAMP_START(old_after, i) * exp(log(ED_XRAMP_INCR(old_after, i)) * d_dur);
	}
    }
}


static void ripple_mixes(chan_info *cp, mus_long_t beg, mus_long_t change);
static void ripple_mixes_with_scale(chan_info *cp, mus_long_t beg, mus_long_t len, mus_float_t scl);
static ed_list *change_samples_in_list(mus_long_t beg, mus_long_t num, int pos, chan_info *cp, ed_fragment **rtn, const char *origin);

static void ripple_all(chan_info *cp, mus_long_t beg, mus_long_t samps)
{
  ripple_marks(cp, beg, samps);
  ripple_mixes(cp, beg, samps);
  check_for_first_edit(cp);
}


static bool lock_affected_mixes(chan_info *cp, int edpos, mus_long_t beg, mus_long_t end)
{
  /* if a deletion, insertion, or change takes place on top of any part of
   *   a virtual mix, we have to write the mix+underlying stuff out as a
   *   change op.  This returns true if it changed the edit list.
   *
   * we assume this is called either just after prepare_edit_list so cp->edit_ctr
   *   points to the new (empty) edit list entry.
   */

  ed_list *ed;
  int i;
  bool changed = false;
  mus_long_t change_beg = -1, change_end = -1, possible_beg = -1, fragment_end;

  ed = cp->edits[edpos];

  /* first look for any directly affected mixes -- even if beg=0 and end=samples I think we want to 
   *    optimize the change as much as possible.
   */
  for (i = 0; i < ed->size; i++)
    {
      mus_long_t fragment_beg;
      fragment_beg = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (is_mix_op(FRAGMENT_TYPE(ed, i)))
	{
	  fragment_end = fragment_beg + FRAGMENT_LENGTH(ed, i);
	  if (possible_beg < 0)
	    possible_beg = fragment_beg;
	  
	  if ((fragment_beg <= end) &&
	      (fragment_end >= beg) && /* hit a mix in the changing section */
	      (change_beg < 0))
	    {
	      change_beg = possible_beg; /* this should track all the way back to where the current mixes started */
	      change_end = fragment_end;
	    }
	}
      else 
	{
	  possible_beg = -1; /* break current chain, if any */
	  if (change_beg > 0)
	    change_end = fragment_beg;
	  if (fragment_beg > end) break;
	}
    }

  if (change_beg >= 0)
    {
      /* now make the change edit, and make sure the affected mixes are removed from the mixes arrays */
      char *temp_file_name;
      io_error_t err;
      mus_long_t cur_len, cur_cursor;
      
      cur_len = ed->samples;
      cur_cursor = ed->cursor;
      temp_file_name = snd_tempnam();
      err = channel_to_file_with_bounds(cp, temp_file_name, edpos, change_beg, change_end - change_beg + 1, cp->sound->hdr, false);
      if (err == IO_NO_ERROR) /* else snd_error earlier? */
	{
	  file_info *hdr;
	  hdr = make_file_info(temp_file_name, FILE_READ_ONLY, FILE_NOT_SELECTED);
	  if (hdr) 
	    {
	      int fd;
	      ed_list *new_ed;
	      ed_fragment *cb = NULL;
	      bool full_file;

	      full_file = ((change_beg == 0) &&
			   (change_end >= ed->samples));

	      fd = snd_open_read(temp_file_name);
	      snd_file_open_descriptors(fd,
					temp_file_name,
					hdr->sample_type,
					hdr->data_location,
					hdr->chans,
					hdr->type);
	      if (full_file)
		{
		  new_ed = initial_ed_list(0, change_end);
		  new_ed->origin = mus_strdup("lock mixes");
		  new_ed->edpos = edpos;
		  cb = FRAGMENT(new_ed, 0);
		}
	      else new_ed = change_samples_in_list(change_beg, change_end - change_beg + 1, edpos, cp, &cb, NULL);
	      new_ed->edit_type = CHANGE_EDIT;
	      new_ed->samples = cur_len;
	      new_ed->cursor = cur_cursor;
	      cp->edits[cp->edit_ctr] = new_ed;
	      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, temp_file_name, 
							 make_file_state(fd, hdr, 0, 0, FILE_BUFFER_SIZE),
							 hdr, DELETE_ME, 0);
	      new_ed->sound_location = ED_SOUND(cb);

	      ripple_all(cp, 0, 0);
	      reflect_mix_change(ANY_MIX_ID);
	      changed = true;
	    }
	}
      if (temp_file_name) free(temp_file_name);
    }
  return(changed);
}



/* -------------------------------- insert samples -------------------------------- */

static ed_list *insert_section_into_list(mus_long_t samp, mus_long_t num, ed_list *current_state, ed_fragment **rtn, const char *origin, mus_float_t scaler)
{
  int cur_len, cur_i, new_i;
  ed_fragment *new_f, *inserted_f = NULL;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  cur_len = current_state->size;
  new_state = make_ed_list(cur_len + 3); /* leave room for possible split */
  for (cur_i = 0, new_i = 0; cur_i < cur_len; cur_i++, new_i++)
    {
      ed_fragment *cur_f;
      cur_f = FRAGMENT(current_state, cur_i);
      new_f = FRAGMENT(new_state, new_i);
      if (ED_GLOBAL_POSITION(cur_f) > samp)
	{
	  /* copy this fragment and ripple */
	  copy_ed_fragment(new_f, cur_f);
	  ED_GLOBAL_POSITION(new_f) += num;
	}
      else
	{
	  if (ED_GLOBAL_POSITION(cur_f) == samp)
	    {
	      /* insert new fragment, copy to end */
	      inserted_f = new_f;
	      
	      /* make newf and increment */
	      new_i++;
	      new_f = FRAGMENT(new_state, new_i);
	      copy_ed_fragment(new_f, cur_f);
	      ED_GLOBAL_POSITION(new_f) += num;
	    }
	  else
	    {
	      copy_ed_fragment(new_f, cur_f);
	      /* look for splits */
	      if (FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1)) > samp)
		{
		  ed_fragment *split_front_f, *split_back_f;
		  /* split current at samp */
		  split_front_f = new_f;
		  copy_checked_ed_fragment(split_front_f, cur_f);
		  ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + samp - ED_GLOBAL_POSITION(split_front_f) - 1;
		  /* samp - global position = where in current fragment, offset that by its local offset, turn into end sample */
		  
		  new_i++;
		  inserted_f = FRAGMENT(new_state, new_i);
		  /* deal with that later */
		  
		  new_i++;
		  split_back_f = FRAGMENT(new_state, new_i);
		  copy_ed_fragment(split_back_f, cur_f);
		  ED_LOCAL_POSITION(split_back_f) = ED_LOCAL_END(split_front_f) + 1;
		  ED_GLOBAL_POSITION(split_back_f) = samp + num; /* rippled */
		  
		  /* now fixup ramps affected by the split */
		  if (ramp_op(ED_TYPE(cur_f)))
		    {
		      new_before_ramp(split_front_f, cur_f);
		      new_after_ramp(split_back_f, cur_f, samp);
		    }
		}
	    }
	}
    }
  ED_GLOBAL_POSITION(inserted_f) = samp;
  ED_LOCAL_POSITION(inserted_f) = 0;
  ED_LOCAL_END(inserted_f) = num - 1;
  ED_TYPE(inserted_f) = ED_SIMPLE;
  ED_SCALER(inserted_f) = scaler;
  if (scaler == 0.0) ED_TYPE(inserted_f) = ED_ZERO;
  (*rtn) = inserted_f;
  new_state->size = new_i;
  new_state->beg = samp;
  new_state->len = num;
  if (origin) new_state->origin = mus_strdup(origin);
  return(new_state);
}


static ed_list *insert_samples_into_list(mus_long_t samp, mus_long_t num, int pos, chan_info *cp, ed_fragment **rtn, const char *origin, mus_float_t scaler)
{
  ed_list *new_state;
  new_state = insert_section_into_list(samp, num, cp->edits[pos], rtn, origin, scaler);
  new_state->edpos = pos;
  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      ed_list *old_state;
      old_state = cp->edits[cp->edit_ctr - 1];
      new_state->selection_beg = old_state->selection_beg;
      new_state->selection_end = old_state->selection_end;
      new_state->cursor = old_state->cursor;
    }
  if (new_state->cursor > samp) new_state->cursor += num;
  return(new_state);
}


static bool insert_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos)
{
  mus_long_t len, new_len;
  ed_fragment *cb;
  ed_list *ed, *old_ed;
  bool backup = false;

  old_ed = cp->edits[edpos];
  len = cp->edits[edpos]->samples;
  new_len = len + num;  /* we're inserting num zeros somewhere */

  if (lock_affected_mixes(cp, edpos, beg, beg))
    {
      edpos = cp->edit_ctr;
      old_ed = cp->edits[edpos];
      increment_edit_ctr(cp);
      backup = true;
    }

  ed = insert_samples_into_list(beg, num, edpos, cp, &cb, S_pad_channel, 0.0);

  ed->samples = new_len;
  ED_SOUND(cb) = EDIT_LIST_ZERO_MARK;
  ED_SCALER(cb) = 0.0;
  ED_TYPE(cb) = ED_ZERO;
  ed->edit_type = ZERO_EDIT;
  ed->sound_location = 0;
  ed->maxamp = old_ed->maxamp;
  ed->maxamp_position = old_ed->maxamp_position;
  if (ed->maxamp_position >= beg)
    ed->maxamp_position += num;
  cp->edits[cp->edit_ctr] = ed;
  peak_env_insert_zeros(cp, beg, num, edpos);

  ripple_all(cp, beg, num);
  ripple_selection(ed, beg, num);

  reflect_sample_change_in_axis(cp);
  reflect_mix_change(ANY_MIX_ID);
  after_edit(cp);

  if (backup)
    backup_edit_list(cp);

  return(true);
}


bool extend_with_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos, const char *origin)
{
  /* this can also be called when beg is within the current sound -> insert a block of zeros */

  int i;
  mus_long_t len, new_len;
  ed_fragment *cb;
  ed_list *new_ed, *old_ed;

  if (num <= 0) return(true); /* false if can't edit, but this is a no-op */

  if (!(prepare_edit_list(cp, edpos, origin)))
    return(false); 

  old_ed = cp->edits[edpos];
  len = cp->edits[edpos]->samples;

  /* check for insert zeros case */
  if (beg < len)
    return(insert_zeros(cp, beg, num, edpos));

  /* extend with zeros at end */
  new_len = beg + num; /* beg might even be > current end? */
  beg = len;
  num = new_len - beg;

  new_ed = make_ed_list(old_ed->size + 1);
  new_ed->beg = beg;
  new_ed->len = num;
  new_ed->samples = new_len;
  new_ed->cursor = old_ed->cursor;
  new_ed->origin = mus_strdup(origin);
  new_ed->edpos = edpos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->maxamp_position = old_ed->maxamp_position;

  for (i = 0; i < old_ed->size; i++) 
    copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));

  /* make the zero fragment, fixup the end fragment */
  if (FRAGMENT(new_ed, old_ed->size)) free_ed_fragment(FRAGMENT(new_ed, old_ed->size)); /* make room for extension */
  FRAGMENT(new_ed, old_ed->size) = FRAGMENT(new_ed, old_ed->size - 1);
  FRAGMENT_GLOBAL_POSITION(new_ed, old_ed->size) = new_len;
  
  cb = make_ed_fragment();
  
  FRAGMENT(new_ed, old_ed->size - 1) = cb;
  ED_SOUND(cb) = EDIT_LIST_ZERO_MARK;
  ED_SCALER(cb) = 0.0;
  ED_TYPE(cb) = ED_ZERO;
  ED_GLOBAL_POSITION(cb) = beg;
  ED_LOCAL_POSITION(cb) = 0;
  ED_LOCAL_END(cb) = num - 1;
  new_ed->edit_type = ZERO_EDIT;
  cp->edits[cp->edit_ctr] = new_ed;
  peak_env_insert_zeros(cp, beg, num, edpos);

  ripple_all(cp, 0, 0);
  reflect_sample_change_in_axis(cp);
  after_edit(cp);
  return(true);
}


bool file_insert_samples(mus_long_t beg, mus_long_t num, const char *inserted_file, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos)
{
  mus_long_t len;
  ed_fragment *cb;
  file_info *hdr;
  ed_list *ed, *old_ed;
  int backup = 0;

  old_ed = cp->edits[edpos];
  len = old_ed->samples;
  if (beg > len)
    {
      if (!(extend_with_zeros(cp, len, beg - len, edpos, origin)))
	return(false);
      edpos = cp->edit_ctr;
      len = current_samples(cp);
      backup++;
    }
  if (!(prepare_edit_list(cp, edpos, origin))) 
    return(false);
  
  if (lock_affected_mixes(cp, edpos, beg, beg))
    {
      edpos = cp->edit_ctr;
      increment_edit_ctr(cp);
      backup++;
    }

  ed = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 1.0);
  ed->samples = len + num;
  ed->edit_type = INSERTION_EDIT;
  cp->edits[cp->edit_ctr] = ed;

  if ((old_ed->maxamp_position != -1) &&
      (mus_sound_channel_maxamp_exists(inserted_file, chan)))
    {
      mus_float_t mx;
      mus_long_t pos;
      mx = mus_sound_channel_maxamp(inserted_file, chan, &pos);
      if (mx > old_ed->maxamp)
	{
	  ed->maxamp = mx;
	  ed->maxamp_position = beg + pos;
	}
      else
	{
	  ed->maxamp = old_ed->maxamp;
	  ed->maxamp_position = old_ed->maxamp_position;
	  if (ed->maxamp_position >= beg)
	    ed->maxamp_position += num;
	}
    }

  hdr = make_file_info(inserted_file, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (hdr)
    {
      int fd;
      fd = snd_open_read(inserted_file);
      snd_file_open_descriptors(fd,
				inserted_file,
				hdr->sample_type,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, inserted_file, SND_INSERT_FILE);
      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, inserted_file, 
						 make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
						 hdr, auto_delete, chan);
      ed->sound_location = ED_SOUND(cb);

      /* mixes are rippled first (ripple_all -> ripple_mixes)
       *   so the lock should affect those that were split by the insertion, which in current
       *   terms means we're interested only in 'beg'
       */
      ripple_all(cp, beg, num);
      ripple_selection(ed, beg, num);
      reflect_sample_change_in_axis(cp);
      reflect_mix_change(ANY_MIX_ID);
      after_edit(cp);

      if (backup > 0)
	{
	  backup_edit_list(cp);
	  if (backup > 1)
	    backup_edit_list(cp);
	}

      return(true);
    }
  return(false);
}


#define MAXAMP_CHECK_SIZE 10000
/* making this larger was slower, smaller no difference? */

bool insert_samples(mus_long_t beg, mus_long_t num, mus_float_t *vals, chan_info *cp, const char *origin, int edpos)
{
  mus_long_t len;
  ed_fragment *cb;
  ed_list *ed, *old_ed;
  int backup = 0;

  if (num <= 0) return(true);
  old_ed = cp->edits[edpos];
  len = old_ed->samples;
  if (beg > len)
    {
      if (!(extend_with_zeros(cp, len, beg - len, edpos, origin)))
	return(false);
      edpos = cp->edit_ctr;
      len = current_samples(cp);
      backup++;
    }

  if (!(prepare_edit_list(cp, edpos, origin))) 
    return(false);

  if (lock_affected_mixes(cp, edpos, beg, beg))
    {
      edpos = cp->edit_ctr;
      increment_edit_ctr(cp);
      backup++;
    }

  ed = insert_samples_into_list(beg, num, edpos, cp, &cb, origin, 1.0);
  ed->edit_type = INSERTION_EDIT;
  ed->samples = len + num;
  cp->edits[cp->edit_ctr] = ed;

  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(vals, (int)num, cp->edit_ctr);
  ED_SOUND(cb) = cp->sound_ctr;
  ed->sound_location = ED_SOUND(cb);

  if ((old_ed->maxamp_position != -1) &&
      (num < MAXAMP_CHECK_SIZE))
    {
      mus_float_t mx;
      int i, pos = 0;
      mx = fabs(vals[0]);
      for (i = 1; i < num; i++)
	{
	  mus_float_t temp;
	  temp = fabs(vals[i]);
	  if (temp > mx)
	    {
	      mx = temp;
	      pos = i;
	    }
	}
      if (mx > old_ed->maxamp)
	{
	  ed->maxamp = mx;
	  ed->maxamp_position = beg + pos;
	}
      else
	{
	  ed->maxamp = old_ed->maxamp;
	  ed->maxamp_position = old_ed->maxamp_position;
	  if (ed->maxamp_position >= beg)
	    ed->maxamp_position += num;
	}
    }

  ripple_all(cp, beg, num);
  ripple_selection(ed, beg, num);
  reflect_sample_change_in_axis(cp);
  reflect_mix_change(ANY_MIX_ID);
  after_edit(cp);

  if (backup > 0)
    {
      backup_edit_list(cp);
      if (backup > 1)
	backup_edit_list(cp);
    }

  return(true);
}


bool insert_complete_file(snd_info *sp, const char *str, mus_long_t chan_beg, file_delete_t auto_delete)
{
  int nc;
  bool ok = false;
  char *filename;
  filename = mus_expand_filename(str);
  nc = mus_sound_chans(filename);
  if (nc > 0)
    {
      mus_long_t len;
      len = mus_sound_framples(filename);
      if (len == 0)
	snd_warning("%s has no data", str);
      else
	{
	  int i, j, first_chan = 0;
	  chan_info *ncp;
	  if (sp->sync != 0)
	    ncp = sp->chans[0];
	  else ncp = any_selected_channel(sp);
	  first_chan = ncp->chan;
	  for (i = first_chan, j = 0; (j < nc) && (i < (int)sp->nchans); i++, j++)
	    {
	      char *origin;
	      ncp = sp->chans[i];
#if HAVE_FORTH
	      origin = mus_format("\"%s\" %" print_mus_long " %d %s drop", 
				  filename, chan_beg, j, S_insert_sound);
#else
	      origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP "%" print_mus_long PROC_SEP "%d", 
				  to_proc_name(S_insert_sound), filename, chan_beg, j);
#endif
	      ok = file_insert_samples(chan_beg, len, filename, ncp, j, auto_delete, origin, ncp->edit_ctr);
	      if (ok)
		update_graph(ncp);
	      free(origin);
	    }
	}
    }
  else snd_warning("can't read %s", str);
  free(filename);
  return(ok);
}


bool insert_complete_file_at_cursor(snd_info *sp, const char *filename)
{
  chan_info *ncp;
  ncp = any_selected_channel(sp);
  return(insert_complete_file(sp, filename, cursor_sample(ncp), DONT_DELETE_ME));
}



/* -------------------------------- delete samples -------------------------------- */

static ed_list *delete_section_from_list(mus_long_t beg, mus_long_t num, ed_list *current_state)
{
  int cur_len, cur_i, new_i;
  ed_fragment *new_f;
  mus_long_t end, next_pos;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  cur_len = current_state->size;
  end = beg + num;
  new_state = make_ed_list(cur_len + 3); /* leave room for possible splits */
  for (cur_i = 0, new_i = 0; cur_i < cur_len; cur_i++)
    {
      ed_fragment *cur_f;
      cur_f = FRAGMENT(current_state, cur_i);
      new_f = FRAGMENT(new_state, new_i);
      if (ED_GLOBAL_POSITION(cur_f) >= end)
	{
	  /* copy this fragment (we're past the deletion) */
	  copy_ed_fragment(new_f, cur_f);
	  ED_GLOBAL_POSITION(new_f) -= num;
	  new_i++;
	}
      else
	{
	  next_pos = FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1));
	  if (next_pos <= beg)
	    {
	      /* we're before deletion without any split, just copy */
	      copy_ed_fragment(new_f, cur_f);
	      new_i++;
	    }
	  else
	    {
	      /* split off begin (if any), delete until num used up, split off end (if any) */
	      /* if global_pos > beg and global_pos next <= end, just drop it, else split */
	      if (ED_GLOBAL_POSITION(cur_f) < beg)
		{
		  ed_fragment *split_front_f;
		  /* split front */
		  split_front_f = new_f;
		  copy_ed_fragment(split_front_f, cur_f);
		  new_i++;
		  ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + beg - ED_GLOBAL_POSITION(split_front_f) - 1;
		  /* samp - global position = where in current fragment, offset that by its local offset, turn into end sample */
		  if (ramp_op(ED_TYPE(cur_f)))
		    new_before_ramp(split_front_f, cur_f);
		}
	      next_pos = FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1));
	      if (next_pos > end)
		{
		  ed_fragment *split_back_f;
		  new_f = FRAGMENT(new_state, new_i);
		  split_back_f = new_f;
		  copy_ed_fragment(split_back_f, cur_f);
		  new_i++;
		  ED_GLOBAL_POSITION(split_back_f) = beg;
		  ED_LOCAL_POSITION(split_back_f) += end - ED_GLOBAL_POSITION(cur_f);
		  if (ramp_op(ED_TYPE(cur_f)))
		    new_after_ramp(split_back_f, cur_f, end);
		}
	    }
	}
    }
  new_state->size = new_i;
  new_state->beg = beg;
  new_state->len = num;
#if HAVE_FORTH
  new_state->origin = mus_format("%" print_mus_long " %" print_mus_long " %s drop", beg, num, S_delete_samples);
#else
#if HAVE_RUBY
  {
    char *temp;
    temp = to_proc_name(S_delete_samples);
    new_state->origin = mus_format("%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long, temp, beg, num);
    if (temp) free(temp);
  }
#else
  new_state->origin = mus_format("%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long, to_proc_name(S_delete_samples), beg, num);
#endif
#endif
  new_state->edit_type = DELETION_EDIT;
  new_state->sound_location = 0;
  return(new_state);
}


bool delete_samples(mus_long_t beg, mus_long_t num, chan_info *cp, int edpos)
{
  mus_long_t len;
  
  if (num <= 0) return(true);
  len = cp->edits[edpos]->samples;

  if ((beg < len) && (beg >= 0))
    {
      ed_list *ed;
      bool backup = false, read_max = false;

      if ((beg + num) > len) num = len - beg;
      if (!(prepare_edit_list(cp, edpos, S_delete_samples))) 
	return(false);

      if (lock_affected_mixes(cp, edpos, beg, beg + num))
	{
	  edpos = cp->edit_ctr;
	  increment_edit_ctr(cp);
	  backup = true;
	}

      ed = delete_section_from_list(beg, num, cp->edits[edpos]);
      if ((cp->edits) && (cp->edit_ctr > 0))
	{
	  ed_list *old_state;
	  old_state = cp->edits[cp->edit_ctr - 1];
	  ed->selection_beg = old_state->selection_beg;
	  ed->selection_end = old_state->selection_end;
	  ed->cursor = old_state->cursor;

	  if (((old_state->maxamp_position >= 0) && (old_state->maxamp_position < beg)) ||
	      (old_state->maxamp_position > (beg + num)))
	    {
	      ed->maxamp = old_state->maxamp;
	      ed->maxamp_position = old_state->maxamp_position;
	      if (old_state->maxamp_position > (beg + num))
		ed->maxamp_position -= num;
	    }
	  else
	    {
	      if ((beg == 0) && (num >= len))
		{
		  ed->maxamp = 0.0;
		  ed->maxamp_position = 0;
		}
	      else read_max = ((len - num) < MAXAMP_CHECK_SIZE);
	    }
	}
      ed->edpos = edpos;
      ed->samples = len - num;
      cp->edits[cp->edit_ctr] = ed;

      ripple_all(cp, beg, -num);
      ripple_selection(ed, beg, -num);
      if (ed->cursor > beg)
	{
	  /* this added 6-Dec-02 */
	  ed->cursor -= num;
	  if (ed->cursor < beg) ed->cursor = beg;
	}

      if (read_max)
	{
	  mus_long_t new_len;
	  mus_float_t mx;
	  int i, loc = 0;
	  snd_fd *sf;
	  new_len = ed->samples;
	  
	  sf = init_sample_read_any_with_bufsize(0, cp, READ_FORWARD, cp->edit_ctr, new_len + 1); /* read current samps */
	  sampler_set_safe(sf, new_len);
	  mx = fabs(read_sample(sf));
	  for (i = 1; i < new_len; i++)
	    {
	      mus_float_t temp;
	      temp = fabs(read_sample(sf));
	      if (temp > mx)
		{
		  mx = temp;
		  loc = i;
		}
	    }
	  free_snd_fd(sf);
	  ed->maxamp = mx;
	  ed->maxamp_position = loc;
	}
		
      reflect_sample_change_in_axis(cp);
      reflect_mix_change(ANY_MIX_ID);
      after_edit(cp);

      if (backup)
	backup_edit_list(cp);
      return(true);
    }
  return(false);
}



/* -------------------------------- change samples -------------------------------- */

static ed_list *change_samples_in_list(mus_long_t beg, mus_long_t num, int pos, chan_info *cp, ed_fragment **rtn, const char *origin)
{
  /* delete + insert -- already checked that beg < cur end */
  ed_list *new_state;
  mus_long_t del_num, cur_end;
  ed_fragment *changed_f;

  if (num <= 0) return(NULL);

  cur_end = cp->edits[pos]->samples;
  del_num = cur_end - beg;
  if (num < del_num) del_num = num;
  if (del_num > 0)
    { 
      ed_list *del_state;
      del_state = delete_section_from_list(beg, del_num, cp->edits[pos]);
      new_state = insert_section_into_list(beg, num, del_state, &changed_f, origin, 1.0);
      free_ed_list(del_state, cp);
    }
  else new_state = insert_section_into_list(beg, num, cp->edits[pos], &changed_f, origin, 1.0);

  (*rtn) = changed_f;

  if ((cp->edits) && (cp->edit_ctr > 0))
    {
      ed_list *old_state;
      old_state = cp->edits[cp->edit_ctr - 1];
      new_state->selection_beg = old_state->selection_beg;
      new_state->selection_end = old_state->selection_end;
    }
  new_state->edpos = pos;

  return(new_state);
}


bool file_change_samples(mus_long_t beg, mus_long_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos)
{
  file_info *hdr;
  hdr = make_file_info(tempfile, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (hdr)
    {
      ed_list *ed, *old_ed;
      mus_long_t prev_len, new_len;
      ed_fragment *cb = NULL;
      int fd;
      int backup = 0;

      old_ed = cp->edits[edpos];
      prev_len = old_ed->samples;
      if (beg > prev_len)
	{
	  if (!(extend_with_zeros(cp, prev_len, beg - prev_len, edpos, origin)))
	    {
	      free_file_info(hdr);
	      return(false);
	    }
	  backup++;
	  edpos = cp->edit_ctr;
	  prev_len = current_samples(cp);
	}
      new_len = beg + num;
      if (new_len < prev_len) new_len = prev_len;
      if (!(prepare_edit_list(cp, edpos, origin)))
	{
	  free_file_info(hdr);
	  return(false);
	}
      if (lock_affected_mixes(cp, edpos, beg, beg + num))
	{
	  edpos = cp->edit_ctr;
	  increment_edit_ctr(cp);
	  backup++;
	}

      ed = change_samples_in_list(beg, num, edpos, cp, &cb, origin);
      ed->edit_type = CHANGE_EDIT;
      ed->samples = new_len;
      if (cp->edit_ctr > 0) ed->cursor = cp->edits[cp->edit_ctr - 1]->cursor;
      cp->edits[cp->edit_ctr] = ed;
      
      if (((old_ed->maxamp_position >= 0) ||
	   ((beg == 0) && (num >= old_ed->samples))) &&
	  (mus_sound_channel_maxamp_exists(tempfile, chan)))
	{
	  mus_float_t mx;
	  mus_long_t pos;
	  mx = mus_sound_channel_maxamp(tempfile, chan, &pos);
	  if ((mx > old_ed->maxamp) ||
	      ((beg == 0) && (num >= old_ed->samples)))
	    {
	      ed->maxamp = mx;
	      ed->maxamp_position = beg + pos;
	    }
	  else
	    {
	      /* make sure old max info is still relevant */
	      if ((old_ed->maxamp_position < beg) ||
		  (old_ed->maxamp_position > (beg + num)))
		{
		  ed->maxamp = old_ed->maxamp;
		  ed->maxamp_position = old_ed->maxamp_position;
		}
	    }
	}

      fd = snd_open_read(tempfile);
      snd_file_open_descriptors(fd,
				tempfile,
				hdr->sample_type,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_CHANGE_FILE);
      ED_SOUND(cb) = add_sound_file_to_edit_list(cp, tempfile, 
						 make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
						 hdr, auto_delete, chan);
      ed->sound_location = ED_SOUND(cb);

      ripple_all(cp, 0, 0); /* copies marks/mixes */
      if (new_len > prev_len) reflect_sample_change_in_axis(cp);

      reflect_mix_change(ANY_MIX_ID);
      after_edit(cp);

      if (backup > 0)
	{
	  backup_edit_list(cp);
	  if (backup > 1)
	    backup_edit_list(cp);
	}
    }
  else
    {
      Xen_error(NO_SUCH_FILE,
		Xen_list_3(C_string_to_Xen_string("~A: ~A"),
			   C_string_to_Xen_string(origin),
			   C_string_to_Xen_string(snd_io_strerror())));
    }
  return(true);
}


bool file_override_samples(mus_long_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin)
{
  file_info *hdr;
  hdr = make_file_info(tempfile, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (hdr) 
    {
      int fd;
      ed_list *e;
      if (num == -1) num = (hdr->samples / hdr->chans);
      if (!(prepare_edit_list(cp, AT_CURRENT_EDIT_POSITION, origin)))
	{
	  free_file_info(hdr);
	  return(false);
	}
      /* don't need to lock mixes here -- we're simply ignoring all previous edit state */

      fd = snd_open_read(tempfile);
      snd_file_open_descriptors(fd,
				tempfile,
				hdr->sample_type,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, tempfile, SND_OVERRIDE_FILE);
      e = initial_ed_list(0, num - 1);
      if (origin) 
	e->origin = mus_strdup(origin);
      else e->origin = mus_strdup("file change samples");
      e->edit_type = CHANGE_EDIT;
      e->edpos = cp->edit_ctr - 1;
      e->samples = num;
      if (cp->edit_ctr > 0) e->cursor = cp->edits[cp->edit_ctr - 1]->cursor;
      cp->edits[cp->edit_ctr] = e;

      if (mus_sound_channel_maxamp_exists(tempfile, chan))
	{
	  mus_long_t pos;
	  e->maxamp = mus_sound_channel_maxamp(tempfile, chan, &pos);
	  e->maxamp_position = pos;
	}

      FRAGMENT_SOUND(e, 0) = add_sound_file_to_edit_list(cp, tempfile, 
							 make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
							 hdr, auto_delete, chan);
      e->sound_location = FRAGMENT_SOUND(e, 0);

      ripple_all(cp, 0, 0);
      reflect_sample_change_in_axis(cp);
      reflect_mix_change(ANY_MIX_ID);
      after_edit(cp);
    }
  else
    {
      Xen_error(NO_SUCH_FILE,
		Xen_list_3(C_string_to_Xen_string("~A: ~A"),
			   C_string_to_Xen_string(origin),
			   C_string_to_Xen_string(snd_io_strerror())));
    }
  return(true);
}


bool change_samples(mus_long_t beg, mus_long_t num, mus_float_t *vals, chan_info *cp, const char *origin, int edpos, mus_float_t mx)
{
  /* mx should be -1.0 except in the one case where vals is a block all of the same value */
  mus_long_t prev_len, new_len;
  ed_fragment *cb;
  ed_list *ed, *old_ed;
  int backup = 0;

  if (num <= 0) return(true);
  old_ed = cp->edits[edpos];
  prev_len = old_ed->samples;
  if (beg > prev_len)
    {
      if (!(extend_with_zeros(cp, prev_len, beg - prev_len, edpos, origin))) 
	return(false);
      edpos = cp->edit_ctr;
      prev_len = current_samples(cp);
      backup++;
    }
  new_len = beg + num;
  if (new_len < prev_len) new_len = prev_len;
  if (!(prepare_edit_list(cp, edpos, origin))) 
    return(false);
  if (lock_affected_mixes(cp, edpos, beg, beg + num))
    {
      edpos = cp->edit_ctr;
      increment_edit_ctr(cp);
      backup++;
    }

  ed = change_samples_in_list(beg, num, edpos, cp, &cb, origin);
  ed->edit_type = CHANGE_EDIT;
  ed->samples = new_len;
  cp->edits[cp->edit_ctr] = ed;
  if (cp->edit_ctr > 0) ed->cursor = cp->edits[cp->edit_ctr - 1]->cursor;
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(vals, (int)num, cp->edit_ctr);
  ED_SOUND(cb) = cp->sound_ctr;
  ed->sound_location = ED_SOUND(cb);

  if ((old_ed->maxamp_position >= 0) ||
      ((beg == 0) && (num >= old_ed->samples)))  /* perhaps old max is irrelevant */
    {
      int pos = 0;
      if ((mx < 0.0) &&
	  (num < MAXAMP_CHECK_SIZE))
	{
	  mus_float_t nmx;
	  int i;
	  nmx = fabs(vals[0]);
	  for (i = 1; i < num; i++)
	    {
	      mus_float_t temp;
	      temp = fabs(vals[i]);
	      if (temp > nmx)
		{
		  nmx = temp;
		  pos = i;
		}
	    }
	  mx = nmx;
	}
      if (mx >= 0.0)
	{
	  if ((mx > old_ed->maxamp) ||
	      ((beg == 0) && (num >= old_ed->samples)))
	    {
	      ed->maxamp = mx;
	      ed->maxamp_position = beg + pos;
	    }
	  else
	    {
	      if ((old_ed->maxamp_position < beg) ||
		  (old_ed->maxamp_position > (beg + num)))
		{
		  ed->maxamp = old_ed->maxamp;
		  ed->maxamp_position = old_ed->maxamp_position;
		}
	    }
	}
    }

  ripple_all(cp, 0, 0);
  if (new_len > prev_len) reflect_sample_change_in_axis(cp);
  reflect_mix_change(ANY_MIX_ID);
  after_edit(cp);

  if (backup > 0)
    {
      backup_edit_list(cp);
      if (backup > 1)
	backup_edit_list(cp);
    }

  return(true);
}


/* -------------------------------- ramp/scale -------------------------------- */

bool unrampable(chan_info *cp, mus_long_t beg, mus_long_t dur, int pos, bool is_xramp)
{
  /* from enveloper (snd-sig.c) */
  ed_list *ed;
  int i;
  mus_long_t end;
  ed = cp->edits[pos];
  end = beg + dur - 1;
  for (i = 0; i < ed->size - 1; i++) 
    {
      mus_long_t loc, next_loc;
      int typ;
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(false);
      typ = FRAGMENT_TYPE(ed, i);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);             /* i.e. next loc = current fragment end point */
      /* fragment starts at loc, ends just before next_loc, is of type typ */
      if ((next_loc > beg) &&
	  (((!is_xramp) && 
	    (type_info[typ].add_ramp == -1)) || 
	   ((is_xramp) && 
	    ((type_info[typ].add_xramp == -1)))))
	return(true);
    }
  return(false);
}


bool sound_fragments_in_use(chan_info *cp, int pos)
{
  /* (swap-channels): are there any non-simple/non-ramp edits? */
  int i;
  ed_list *ed;
  ed = cp->edits[pos];
  for (i = 0; i < ed->size - 1; i++) 
    {
      int index;
      index = FRAGMENT_SOUND(ed, i);
      if (index == EDIT_LIST_END_MARK) return(false);
      if ((index != 0) &&
	  (index != EDIT_LIST_ZERO_MARK))
	return(true);
    }
  return(false);
}


bool virtual_mix_ok(chan_info *cp, int edpos)
{
  /* since a mix can be dragged anywhere, and we want to continue treating it as a virtual mix anywhere,
   *   we have to make sure that all edits in the current edit list are mix-able.
   */
  ed_list *ed;
  int i;
  ed = cp->edits[edpos];
  for (i = 0; i < ed->size - 1; i++) 
    {
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(true);
      if (!(is_mixable_op(FRAGMENT_TYPE(ed, i)))) return(false);
    }
  return(true);
}


static bool found_virtual_mix(chan_info *cp, int edpos)
{
  ed_list *ed;
  int i;
  ed = cp->edits[edpos];
  for (i = 0; i < ed->size - 1; i++) 
    {
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      if (is_mix_op(FRAGMENT_TYPE(ed, i))) return(true);
    }
  return(false);
}


static bool found_unmixable_ramped_op(chan_info *cp, int edpos, mus_long_t beg, mus_long_t end, bool is_xramp)
{
  ed_list *ed;
  int i;
  ed = cp->edits[edpos];
  for (i = 0; i < ed->size - 1; i++) 
    {
      mus_long_t loc, next_loc;
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(false);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(false);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);         /* i.e. next loc = current fragment end point */
      /* this fragment (i) starts at loc, ends just before next_loc, is of type typ */
      if (next_loc > beg)
	{
	  int after_ramp_op;
	  if (is_xramp)
	    after_ramp_op = type_info[FRAGMENT_TYPE(ed, i)].add_xramp;
	  else after_ramp_op = type_info[FRAGMENT_TYPE(ed, i)].add_ramp;
	  if ((after_ramp_op == -1) ||        /* this should not happen since we check for it ahead of time */
	      (!(is_mixable_op(after_ramp_op))))
	    return(true);
	}
    }
  return(false);
}


static bool section_is_zero(chan_info *cp, mus_long_t beg, mus_long_t dur, int pos)
{
  ed_list *ed;
  int i;
  mus_long_t end;

  ed = cp->edits[pos];
  end = beg + dur - 1;
  for (i = 0; i < ed->size - 1; i++) 
    {
      mus_long_t loc, next_loc;
      int typ;
      if (FRAGMENT_SOUND(ed, i) == EDIT_LIST_END_MARK) return(true);
      loc = FRAGMENT_GLOBAL_POSITION(ed, i);
      if (loc > end) return(true);
      typ = FRAGMENT_TYPE(ed, i);
      next_loc = FRAGMENT_GLOBAL_POSITION(ed, i + 1);         /* i.e. next loc = current fragment end point */
      /* this fragment (i) starts at loc, ends just before next_loc, is of type typ */
      if ((next_loc > beg) &&
	  (typ != ED_ZERO))
	return(false);
    }
  return(true);
}


static ed_list *copy_and_split_list(mus_long_t beg, mus_long_t num, ed_list *current_state)
{
  mus_long_t end, next_pos;
  int cur_len, cur_i, new_i;
  ed_list *new_state;
  ed_fragment *new_f, *mid_f = NULL;
  if (num <= 0) return(NULL);
  cur_len = current_state->size;
  end = beg + num;
  new_state = make_ed_list(cur_len + 2); /* leave room for possible split */
  for (cur_i = 0, new_i = 0; cur_i < cur_len; cur_i++, new_i++)
    {
      ed_fragment *cur_f;
      cur_f = FRAGMENT(current_state, cur_i);
      new_f = FRAGMENT(new_state, new_i);
      if (ED_GLOBAL_POSITION(cur_f) >= end)
	{
	  /* after any split, copy this fragment */
	  copy_ed_fragment(new_f, cur_f);
	}
      else
	{
	  next_pos = FRAGMENT_GLOBAL_POSITION(current_state, (cur_i + 1));
	  if (next_pos <= beg)
	    {
	      /* we're before any split, just copy */
	      copy_ed_fragment(new_f, cur_f);
	    }
	  else
	    {
	      if ((ED_GLOBAL_POSITION(cur_f) >= beg) && (next_pos < end))
		{
		  /* entire segment is included */
		  copy_ed_fragment(new_f, cur_f);
		}
	      else
		{
		  /* check for front and back splits, copy cur */
		  copy_ed_fragment(new_f, cur_f);
		  if (ED_GLOBAL_POSITION(cur_f) < beg)
		    {
		      ed_fragment *split_front_f, *split_back_f;

		      /* split current at samp */
		      split_front_f = new_f;
		      new_i++;
		      split_back_f = FRAGMENT(new_state, new_i);
		      copy_ed_fragment(split_back_f, cur_f);
		      new_f = split_back_f;
		      ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + beg - ED_GLOBAL_POSITION(split_front_f) - 1;
		      /* beg - global position = where in current fragment, offset that by its local offset, turn into end sample */
		      ED_LOCAL_POSITION(split_back_f) = ED_LOCAL_END(split_front_f) + 1;
		      ED_GLOBAL_POSITION(split_back_f) = beg;

		      /* now fixup ramps affected by the split */
		      if (ramp_op(ED_TYPE(cur_f)))
			{
			  new_before_ramp(split_front_f, cur_f);
			  new_after_ramp(split_back_f, cur_f, beg);
			  mid_f = split_back_f;
			}
		    }
		  
		  if (next_pos > end)
		    {
		      ed_fragment *split_front_f, *split_back_f;

		      /* split current at samp */
		      split_front_f = new_f;
		      new_i++;
		      split_back_f = FRAGMENT(new_state, new_i);
		      copy_ed_fragment(split_back_f, cur_f);
		      ED_LOCAL_END(split_front_f) = ED_LOCAL_POSITION(split_front_f) + end - ED_GLOBAL_POSITION(split_front_f) - 1;
		      ED_LOCAL_POSITION(split_back_f) = ED_LOCAL_END(split_front_f) + 1;
		      ED_GLOBAL_POSITION(split_back_f) = end;

		      /* now fixup ramps affected by the split */
		      if (ramp_op(ED_TYPE(cur_f)))
			{
			  if (ED_RAMPS(split_front_f))
			    {
			      mus_float_t *ramp_begs = NULL, *xramp_begs = NULL;
			      int i, rmps, xrmps;
			      
			      rmps = ED_RAMP_LIST_SIZE(split_front_f);
			      xrmps = ED_XRAMP_LIST_SIZE(split_front_f);
			      if (rmps > 0) ramp_begs = (mus_float_t *)calloc(rmps, sizeof(mus_float_t));
			      if (xrmps > 0) xramp_begs = (mus_float_t *)calloc(xrmps, sizeof(mus_float_t));

			      for (i = 0; i < rmps; i++)
				ramp_begs[i] = ED_RAMP_START(split_front_f, i);
			      for (i = 0; i < xrmps; i++)
				xramp_begs[i] = ED_XRAMP_START(split_front_f, i);

			      new_before_ramp(split_front_f, cur_f);

			      if (mid_f == split_front_f)
				{
				  for (i = 0; i < rmps; i++)
				    ED_RAMP_START(split_front_f, i) = ramp_begs[i];
				  for (i = 0; i < xrmps; i++)
				    ED_XRAMP_START(split_front_f, i) = xramp_begs[i];
				}

			      if (ramp_begs) free(ramp_begs);
			      if (xramp_begs) free(xramp_begs);
			    }
			  new_after_ramp(split_back_f, cur_f, end);
			}
		    }
		}
	    }
	}
    }
  new_state->size = new_i;
  new_state->beg = beg;
  new_state->len = num;
  return(new_state);
}


bool scale_channel_with_origin(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, const char *origin)
{
  /* copy current ed-list and reset scalers */
  mus_long_t len = 0;
  int i, old_pos;
  ed_list *new_ed, *old_ed;
  bool backup = false;

  old_ed = cp->edits[pos];
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= old_ed->samples) ||
      ((scl == 1.0) && (pos == cp->edit_ctr)) ||
      (section_is_zero(cp, beg, num, pos)))
    return(false); 

  old_pos = pos;
  len = old_ed->samples;
  if (!(prepare_edit_list(cp, pos, S_scale_channel))) 
    return(false);
  
  if ((beg == 0) && 
      (num >= old_ed->samples))
    {
      if (scl == 0.0)
	{
	  new_ed = initial_ed_list(0, num - 1);
	  FRAGMENT_SCALER(new_ed, 0) = 0.0;
	  FRAGMENT_TYPE(new_ed, 0) = ED_ZERO;
	  new_ed->maxamp = 0.0;
	  new_ed->maxamp_position = 0;
	}
      else
	{
	  num = len;
	  new_ed = make_ed_list(old_ed->size);
	  new_ed->beg = beg;
	  new_ed->len = num;
	  for (i = 0; i < new_ed->size; i++) 
	    {
	      copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	      FRAGMENT_SCALER(new_ed, i) *= scl;
	    }
	  if (old_ed->maxamp_position != -1)
	    {
	      new_ed->maxamp = old_ed->maxamp * fabs(scl);
	      new_ed->maxamp_position = old_ed->maxamp_position;
	    }
	}
      new_ed->samples = len;
      cp->edits[cp->edit_ctr] = new_ed;
      peak_env_scale_by(cp, scl, pos); /* this seems wasteful if this is an intermediate (in_as_one_edit etc) */
    }
  else 
    {
      /* not sure how hard-nosed to be here -- we could probably get by if the scaled section completely encloses the mix(es) */
      if (lock_affected_mixes(cp, pos, beg, beg + num))
	{
	  pos = cp->edit_ctr;
	  old_ed = cp->edits[pos];	
	  increment_edit_ctr(cp);
	  backup = true;
	}

      if (beg + num > len) num = len - beg;
      new_ed = copy_and_split_list(beg, num, old_ed);
      new_ed->samples = len;
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg) 
	    {
	      FRAGMENT_SCALER(new_ed, i) *= scl;
	      if (scl == 0.0) 
		{
		  FRAGMENT_TYPE(new_ed, i) = ED_ZERO;
		  clear_ed_fragment(FRAGMENT(new_ed, i));
		}
	    }
	}
      peak_env_scale_selection_by(cp, scl, beg, num, pos);

      if (old_ed->maxamp_position >= 0)
	{
	  if ((old_ed->maxamp_position < beg) ||
	       (old_ed->maxamp_position > (beg + num)))
	    {
	      if (fabs(scl) <= 1.0)
		{
		  new_ed->maxamp = old_ed->maxamp;
		  new_ed->maxamp_position = old_ed->maxamp_position;
		}
	      else
		{
		  /* perhaps this costs more than it saves */
		  if (num < MAXAMP_CHECK_SIZE)
		    {
		      mus_float_t mx;
		      int i, loc = 0;
		      snd_fd *sf;
		      sf = init_sample_read_any_with_bufsize(beg, cp, READ_FORWARD, old_pos, num + 1);
		      sampler_set_safe(sf, num);
		      mx = fabs(read_sample(sf));
		      for (i = 1; i < num; i++)
			{
			  mus_float_t temp;
			  temp = fabs(read_sample(sf));
			  if (temp > mx)
			    {
			      mx = temp;
			      loc = i;
			    }
			}
		      free_snd_fd(sf);
		      mx *= fabs(scl);
		      if (mx > old_ed->maxamp)
			{
			  new_ed->maxamp = mx;
			  new_ed->maxamp_position = beg + loc;
			}
		      else
			{
			  new_ed->maxamp = old_ed->maxamp;
			  new_ed->maxamp_position = old_ed->maxamp_position;
			}
		    }
		}
	    }
	  else
	    {
	      if (fabs(scl) >= 1.0)
		{
		  new_ed->maxamp = old_ed->maxamp * fabs(scl);
		  new_ed->maxamp_position = old_ed->maxamp_position;
		}
	    }
	}
    }
  new_ed->cursor = old_ed->cursor;
  new_ed->edit_type = SCALED_EDIT;
  new_ed->sound_location = 0;
  if (origin)
    new_ed->origin = mus_strdup(origin);
  else
    {
      if (num == len)
#if HAVE_FORTH
	new_ed->origin = mus_format("%.3f %" print_mus_long PROC_SEP PROC_FALSE " %s", scl, beg, S_scale_channel);
#else
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%" print_mus_long PROC_SEP PROC_FALSE, to_proc_name(S_scale_channel), scl, beg);
#endif
      else
	{
#if HAVE_FORTH
	  new_ed->origin = mus_format("%.3f %" print_mus_long PROC_SEP "%" print_mus_long " %s", scl, beg, num, S_scale_channel);
#else
	  new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long, to_proc_name(S_scale_channel), scl, beg, num);
#endif
	}
    }
  new_ed->edpos = pos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;

  ripple_marks(cp, 0, 0);
  ripple_mixes_with_scale(cp, beg, num, scl);
  check_for_first_edit(cp);

  if (!in_as_one_edit) 
    {
      update_graph(cp);
      reflect_mix_change(ANY_MIX_ID);
      after_edit(cp);  /* "as_one_edit" here is an envelope, so it's not a "real" edit -- after_edit called explicitly in snd-sig.c in this case */
    }

  if (backup)
    backup_edit_list(cp);

  return(true);
}


bool scale_channel(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit)
{
  return(scale_channel_with_origin(cp, scl, beg, num, pos, in_as_one_edit, NULL));
}


static void add_ramp_to_fragment(ed_list *new_ed, int i, double start, double incr, mus_float_t scaler, mus_float_t offset, bool is_xramp)
{
  ed_fragment *ed;
  int rmps = 0, xrmps = 0, loc, typ;

  ed = FRAGMENT(new_ed, i);
  if (ED_TYPE(ed) == ED_ZERO) return;

  if ((!is_xramp) && (start == 0.0) && (incr == 0.0))
    {
      ED_TYPE(ed) = ED_ZERO;
      clear_ed_fragment(ed);
      return;
    }

  if (ED_RAMPS(ed))
    {
      rmps = ED_RAMP_LIST_SIZE(ed);
      xrmps = ED_XRAMP_LIST_SIZE(ed);
    }
  if (is_xramp)
    xrmps++;
  else rmps++;

  ensure_ed_ramps(ed, rmps, xrmps);
  typ = ED_TYPE(ed);

  if (is_xramp)
    {
      loc = xrmps - 1;
      ED_XRAMP_START(ed, loc) = start;
      ED_XRAMP_INCR(ed, loc) = incr;
      ED_XRAMP_SCALER(ed, loc) = scaler;
      ED_XRAMP_OFFSET(ed, loc) = offset;
      ED_TYPE(ed) = type_info[typ].add_xramp;
    }
  else
    {
      loc = rmps - 1;
      ED_RAMP_START(ed, loc) = start;
      ED_RAMP_INCR(ed, loc) = incr;
      ED_TYPE(ed) = type_info[typ].add_ramp;
    }
}


static bool all_ramp_channel(chan_info *cp, double start, double incr, double scaler, double offset, 
			     mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, const char *origin, 
			     bool is_xramp, mus_any *e, int xramp_seg_loc)
{
  mus_long_t len = 0;
  int i, old_pos;
  ed_list *new_ed, *old_ed;
  bool backup = false;
  double rstart;
  /*
  fprintf(stderr,"ramp: %f %f %f %f %" print_mus_long " %" print_mus_long "\n", start, incr, scaler, offset, beg, num);
  */

  old_ed = cp->edits[pos];
  if ((beg < 0) || 
      (num <= 0) ||
      (beg >= old_ed->samples) ||
      (section_is_zero(cp, beg, num, pos)))
    return(false);  /* was true, but this is a no-op */

  if ((!is_xramp) && ((incr == 0.0) || (num == 1)))                 /* in xramp case, we're ramping a power, not a scaler */
    return(scale_channel(cp, start, beg, num, pos, in_as_one_edit));

  len = old_ed->samples;
  old_pos = pos;

  if (!(prepare_edit_list(cp, pos, origin))) 
    return(false);

  if (found_virtual_mix(cp, pos))
    {
      mus_long_t lock_beg, lock_end;
      if (found_unmixable_ramped_op(cp, pos, beg, beg + num, is_xramp))
	{
	  lock_beg = 0;
	  lock_end = len - 1;
	}
      else
	{
	  lock_beg = beg;
	  lock_end = beg + num;
	}
      if (lock_affected_mixes(cp, pos, lock_beg, lock_end))
	{
	  pos = cp->edit_ctr;
	  old_ed = cp->edits[pos]; 
	  increment_edit_ctr(cp);
	  backup = true;
	}
    }

  rstart = start;
  if ((beg == 0) && 
      (num >= old_ed->samples))
    {
      /* one ramp over entire fragment list -- no splits will occur here */
      num = len;
      new_ed = make_ed_list(old_ed->size);
      new_ed->beg = beg;
      new_ed->len = num;
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size; i++)
	copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
      for (i = 0; i < new_ed->size - 1; i++) /* -1 here to leave end mark alone */
	{
	  add_ramp_to_fragment(new_ed, i, start, incr, scaler, offset, is_xramp);
	  if (!is_xramp)
	    start += (incr * FRAGMENT_LENGTH(new_ed, i));
	  else start *= exp(log(incr) * FRAGMENT_LENGTH(new_ed, i));
	}
    }
  else 
    {
      if (beg + num > len) num = len - beg;
      new_ed = copy_and_split_list(beg, num, old_ed);
      cp->edits[cp->edit_ctr] = new_ed;
      for (i = 0; i < new_ed->size - 1; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + num - 1)) break; /* not >= (1 sample selections) */
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg)
	    {
	      add_ramp_to_fragment(new_ed, i, start, incr, scaler, offset, is_xramp);
	      if (!is_xramp)
		start += (incr * FRAGMENT_LENGTH(new_ed, i));
	      else start *= exp(log(incr) * FRAGMENT_LENGTH(new_ed, i));
	    }
	}
      if ((old_ed->maxamp_position >= 0) ||
	  ((beg == 0) && (num >= old_ed->samples)))
	{
	  if ((old_ed->maxamp_position >= 0) &&
	      ((old_ed->maxamp_position < beg) ||
	       (old_ed->maxamp_position > (beg + num))) &&
	      (fabs(rstart) <= 1.0) &&
	      (((!is_xramp) &&
		(fabs(rstart + incr * num) <= 1.0)) ||
	       ((is_xramp) &&
		(fabs(rstart * exp(log(incr) * num)) <= 1.0))))
	    {
	      /* we have maxamp data for the previous edit and
	       *   the ramp does not hit the current maxamp and it stays within -1.0 to 1.0, 
	       *   so it can't affect the maxamp 
	       */
	      new_ed->maxamp = old_ed->maxamp;
	      new_ed->maxamp_position = old_ed->maxamp_position;
	    }
	  else
	    {
	      /* if ramped portion has a max > old max, we can use it in any case,
	       *   but we need to do the ramp by hand here, I think.
	       */
	      if ((num < MAXAMP_CHECK_SIZE) &&
		  (!is_xramp))
		{
		  mus_float_t mx, x;
		  int i, loc = 0;
		  snd_fd *sf;
		  x = rstart;
		  sf = init_sample_read_any_with_bufsize(beg, cp, READ_FORWARD, old_pos, num + 1);
		  sampler_set_safe(sf, num);
		  mx = fabs(x * read_sample(sf));
		  for (i = 1; i < num; i++)
		    {
		      mus_float_t temp;
		      x += incr;
		      temp = fabs(x * read_sample(sf));
		      if (temp > mx)
			{
			  mx = temp;
			  loc = i;
			}
		    }
		  free_snd_fd(sf);
		  if ((mx > old_ed->maxamp) ||
		      ((beg == 0) && (num >= old_ed->samples)))
		    {
		      new_ed->maxamp = mx;
		      new_ed->maxamp_position = beg + loc;
		    }
		  else
		    {
		      if ((old_ed->maxamp_position < beg) ||
			  (old_ed->maxamp_position > (beg + num)))
			{
			  new_ed->maxamp = old_ed->maxamp;
			  new_ed->maxamp_position = old_ed->maxamp_position;
			}
		    }
		}
	    }
	}
    }
  new_ed->samples = len;
  new_ed->cursor = old_ed->cursor;
  new_ed->edit_type = RAMP_EDIT;
  new_ed->sound_location = 0;
  if (!is_xramp)
    {
      mus_float_t rmp0, rmp1;
      rmp0 = rstart;
      rmp1 = rstart + incr * (num - 1); /* want end point */
#if HAVE_FORTH
      if (num == len)
	new_ed->origin = mus_format("%.3f %.3f %" print_mus_long PROC_SEP PROC_FALSE " %s", rmp0, rmp1, beg, origin);
      else
	new_ed->origin = mus_format("%.3f %.3f %" print_mus_long PROC_SEP "%" print_mus_long " %s", rmp0, rmp1, beg, num, origin);
#else
      if (num == len)
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP "%" print_mus_long PROC_SEP PROC_FALSE, to_proc_name(origin), rmp0, rmp1, beg);
      else
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long, to_proc_name(origin), rmp0, rmp1, beg, num);
#endif
    }
  else
    {
      mus_float_t *data;
      data = mus_data(e);
#if HAVE_FORTH
      if (num == len)
	new_ed->origin = mus_format("%.3f %.3f %.3f %" print_mus_long PROC_SEP PROC_FALSE " %s",
				    data[xramp_seg_loc * 2 + 1], data[xramp_seg_loc * 2 + 3], mus_increment(e), beg, origin);
      else
	new_ed->origin = mus_format("%.3f %.3f %.3f %" print_mus_long PROC_SEP "%" print_mus_long " %s",
				    data[xramp_seg_loc * 2 + 1], data[xramp_seg_loc * 2 + 3], mus_increment(e), beg, num, origin);
#else
      if (num == len)
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP "%.3f" PROC_SEP "%" print_mus_long PROC_SEP PROC_FALSE, 
				    to_proc_name(origin), data[xramp_seg_loc * 2 + 1], data[xramp_seg_loc * 2 + 3], mus_increment(e), beg);
      else
	new_ed->origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%.3f" PROC_SEP "%.3f" PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long, 
				    to_proc_name(origin), data[xramp_seg_loc * 2 + 1], data[xramp_seg_loc * 2 + 3], mus_increment(e), beg, num);
#endif
    }
  new_ed->edpos = pos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;

  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  reflect_mix_change(ANY_MIX_ID);
  after_edit(cp);

  if (backup)
    backup_edit_list(cp);

  return(true);
}


bool ramp_channel(chan_info *cp, double start, double incr, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit)
{
  return(all_ramp_channel(cp, start, incr, 0.0, 0.0, beg, num, pos, in_as_one_edit, S_ramp_channel, false, NULL, 0));
}


bool xramp_channel(chan_info *cp, double start, double incr, double scaler, double offset, 
		   mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, mus_any *e, int xramp_seg_loc)
{
  return(all_ramp_channel(cp, start, incr, scaler, offset, beg, num, pos, in_as_one_edit, S_xramp_channel, true, e, xramp_seg_loc));
}





/* -------------------------------- samplers -------------------------------- */

static reader_mixes *free_reader_mixes(reader_mixes *md)
{
  if (md)
    {
      if ((md->size > 0) && (md->sfs))
	{
	  int i;
	  for (i = 0; i < md->size; i++)
	    if (md->sfs[i])
	      {
		if (md->sfs[i]->current_state)
		  md->sfs[i]->current_state = free_ed_list(md->sfs[i]->current_state, md->sfs[i]->cp);  /* md->cp is ignored in this case -- MIX_EDIT */
		md->sfs[i] = free_snd_fd(md->sfs[i]);
	      }
	  free(md->sfs);
	}
      free(md);
    }
  return(NULL);
}


snd_fd *free_snd_fd_almost(snd_fd *sf)
{
  if ((sf) && (!(sf->freed)))
    {
      if (sf->ramps)
	{
	  if (READER_INCRS(sf)) free(READER_INCRS(sf));
	  if (READER_VALS(sf)) free(READER_VALS(sf));
	  if (READER_XINCRS(sf)) free(READER_XINCRS(sf));
	  if (READER_XVALS(sf)) free(READER_XVALS(sf));

	  free(sf->ramps);
	  sf->ramps = NULL;
	}

      if (sf->mixes)
	sf->mixes = (void *)free_reader_mixes((reader_mixes *)(sf->mixes));

      reader_out_of_data(sf);
      {
	snd_data *sd;
	sd = sf->current_sound;
	if ((sd) && 
	    ((sd->type == SND_DATA_BUFFER) || (sd->type == SND_DATA_FILE)))
	  {
	    sd->inuse = false;
	    if ((sd->copy) || (sd->free_me))
	      free_snd_data(sd); 
	  }
      }
      sf->current_sound = NULL;
      if (sf->current_state) 
	{
	  if (sf->type == MIX_READER)
	    sf->current_state = free_ed_list(sf->current_state, sf->cp);
	  sf->current_state = NULL;
	}
      sf->cp = NULL;
      sf->local_sp = NULL;
      sf->cb = NULL;
      sf->region = INVALID_REGION;
      sf->edit_ctr = -1;
      sf->freed = true;
    }
  return(NULL);
}


snd_fd *free_snd_fd(snd_fd *sf)
{
  if ((sf) && (!(sf->freed)))
    {
      free_snd_fd_almost(sf);
      free(sf);
    }
  return(NULL);
}


mus_long_t current_location(snd_fd *sf) 
{
  /* only used by moving cursor code in snd-dac.c [and sampler-position] */
  if (sf->current_sound)
    return(READER_GLOBAL_POSITION(sf) - READER_LOCAL_POSITION(sf) + io_beg(sf->current_sound->io) + sf->loc);
  return(READER_GLOBAL_POSITION(sf) - READER_LOCAL_POSITION(sf) + sf->loc);
}


void sampler_set_safe(snd_fd *sf, mus_long_t dur)
{
  /* tricky because we currently assume the reader protects against reading off the end by returning 0.0,
   *  perhaps pass in dur (= number of samples we will be reading), and if last-loc+1>=dur, we're safe?
   *  dur here has to match the number of samples we will read! 
   */
  
  /* fprintf(stderr, "%" print_mus_long " %" print_mus_long " %" print_mus_long ": %" print_mus_long "\n", sf->first, sf->loc, sf->last, dur); */

  if ((sf->last - sf->loc + 1) >= dur) /* two kinds of counter here: last is sample number, dur is how many samples */
    {
      if (sf->runf == next_sample_value_unscaled)
	sf->runf = next_sample_value_unscaled_and_unchecked;
      else
	{
	  if (sf->runf == next_sample_value)
	    sf->runf = next_sample_value_unchecked;
	  else
	    {
	      if (sf->runf == next_ramp1)
		sf->runf = next_ramp1_unchecked;
	    }
	}
    }
  else
    {
      if (sf->loc - sf->first + 1 >= dur)
	{
	  if (sf->runf == previous_sample_value_unscaled)
	    sf->runf = previous_sample_value_unscaled_and_unchecked;
	  else
	    {
	      if (sf->runf == previous_sample_value)
		sf->runf = previous_sample_value_unchecked;
	    }
	}
    }
}


snd_fd *init_sample_read_any_with_bufsize(mus_long_t samp, chan_info *cp, read_direction_t direction, int edit_position, int bufsize)
{
  snd_fd *sf;
  snd_info *sp;
  ed_list *ed;
  int len, i;
  mus_long_t curlen;
  snd_data *first_snd = NULL;
  /* bufsize might be ignored here except in the copied buffer case -- we use the pre-exisiting buffer? */

  if (cp->active < CHANNEL_HAS_EDIT_LIST) return(NULL);
  if ((edit_position < 0) || (edit_position >= cp->edit_size)) return(NULL); /* was ">" not ">=": 6-Jan-05 */

  ed = cp->edits[edit_position];
  if (!ed) return(NULL);

  sp = cp->sound;
  if (sp->inuse == SOUND_IDLE) return(NULL);

  if ((sp->need_update) &&
      (!(sp->writing)))
    {
      if (mus_file_probe(sp->filename) == 0)
	{
	  snd_warning("%s no longer exists!", sp->short_filename);
	  return(NULL);
	}
      else
	{
	  time_t write_date;
	  write_date = file_write_date(sp->filename);
	  if (sp->update_warning_write_date != write_date)
	    {
	      snd_warning("%s has changed since we last read it!", sp->short_filename);
	      sp->update_warning_write_date = write_date;
	      /* without this write-date check, there are cases where this can get into a loop sending warnings */
	    }
	}
    }

  curlen = cp->edits[edit_position]->samples;

  /* snd_fd allocated only here */
  sf = (snd_fd *)calloc(1, sizeof(snd_fd)); /* only creation point (... oops -- see below...)*/

  sf->freed = false;
  sf->region = INVALID_REGION;
  sf->type = SAMPLER;
  sf->initial_samp = samp;
  sf->cp = cp;
  sf->fscaler = 1.0;
  sf->direction = direction;
  sf->current_state = ed;
  sf->edit_ctr = edit_position;

  if ((curlen <= 0) ||    /* no samples, not ed->len (delete->len = #deleted samps) */
      (samp < 0) ||       /* this should never happen */
      ((samp >= curlen) && (direction == READ_FORWARD)))
    return(cancel_reader(sf));

  if (samp >= curlen) samp = curlen - 1;
  len = ed->size;

  for (i = 0; i < len; i++)
    {
      ed_fragment *cb;
      cb = FRAGMENT(ed, i);
      if ((ED_GLOBAL_POSITION(cb) > samp) || 
	  (ED_SOUND(cb) == EDIT_LIST_END_MARK))             /* i.e. we went one too far */
	{
	  mus_long_t ind0, ind1, indx;
	  sf->cb = FRAGMENT(ed, i - 1);                     /* so back up one */
	  sf->cbi = i - 1;
	  sf->frag_pos = samp - READER_GLOBAL_POSITION(sf);
	  ind0 = READER_LOCAL_POSITION(sf);                   /* cb->beg */
	  indx = ind0 + sf->frag_pos;
	  ind1 = READER_LOCAL_END(sf);                        /* cb->end */
	  sf->fscaler = READER_SCALER(sf);
	  if (zero_op(READER_TYPE(sf)))
	    {
	      sf->current_sound = NULL;
	      sf->loc = indx;
	      sf->first = ind0;
	      sf->last = ind1;
	      sf->data = NULL;
	      choose_accessor(sf);
	      return(sf);
	    }
	  first_snd = sf->cp->sounds[READER_SOUND(sf)];
	  if (!first_snd)
	    return(cancel_reader(sf));
	  if (first_snd->type == SND_DATA_FILE)
	    {
	      /* since arbitrarily many work procs can be running in parallel, reading the same 
	       * data (edit tree sound file entries), we can't share the clm-style IO buffers since these contain
	       * a local notion of current position which is not accessed on every sample by the
	       * samplers (they trust their snd_fd indices); we wouldn't want to be
	       * constantly jumping around and re-reading data buffers (in the worst case
	       * many times per sample) anyway, so we copy the IO buffer, allocate a relatively
	       * small(?? -- is this obsolete) data buffer, and then free all the copied snd_data stuff as soon as
	       * the current reader is done.
	       *
	       * but if all the data is in the current buffer, it won't be moving?
	       */
	      if ((first_snd->inuse) ||
		  (bufsize > FILE_BUFFER_SIZE))
		{
		  first_snd = copy_snd_data(first_snd, samp, bufsize);
		  if (!first_snd)
		    return(cancel_reader(sf));
		}
	      first_snd->inuse = true;
	      sf->current_sound = first_snd;
	      sf->data = first_snd->buffered_data;
	      if (direction == READ_FORWARD)
		file_buffers_forward(ind0, ind1, indx, sf, first_snd);
	      else file_buffers_back(ind0, ind1, indx, sf, first_snd);
	    }
	  else 
	    {
	      sf->current_sound = NULL;
	      sf->data = first_snd->buffered_data;
	      sf->first = ind0;
	      sf->last = ind1;
	      sf->loc = indx;
	    }
	  choose_accessor(sf);
	  return(sf);
	}
    }
  if (sf) free(sf);
  return(NULL);
}


snd_fd *init_sample_read_any(mus_long_t samp, chan_info *cp, read_direction_t direction, int edit_position)
{
  return(init_sample_read_any_with_bufsize(samp, cp, direction, edit_position, FILE_BUFFER_SIZE));
}


snd_fd *init_sample_read(mus_long_t samp, chan_info *cp, read_direction_t direction)
{
  return(init_sample_read_any_with_bufsize(samp, cp, direction, cp->edit_ctr, FILE_BUFFER_SIZE));
}


mus_float_t chn_sample(mus_long_t samp, chan_info *cp, int pos)
{ 
  snd_fd *sf;
  mus_float_t val = 0.0;

  /* pos is assumed to be right here, not AT_CURRENT_EDIT_POSITION for example */
  if ((cp->active < CHANNEL_HAS_EDIT_LIST) || 
      (samp < 0) || 
      (pos < 0) || 
      (pos >= cp->edit_size) || 
      (samp >= cp->edits[pos]->samples)) 
    return(0.0);

  /* try the quick case */
  if (pos == 0)
    {
      snd_data *sd;
      sd = cp->sounds[0];
      if ((sd) && (sd->io) && (io_beg(sd->io) <= samp) && (io_end(sd->io) >= samp))
	return(sd->buffered_data[samp - io_beg(sd->io)]);
    }

  /* do it the hard way */
  sf = init_sample_read_any_with_bufsize(samp, cp, READ_FORWARD, pos, 2);
  if (sf)
    {
      val = read_sample(sf);
      free_snd_fd(sf);
    }
  return(val);
}


static void previous_sound_1(snd_fd *sf) 
{
  mus_long_t ind0, ind1;
  bool at_start;
  if ((sf->cp) && 
      (sf->cp->active < CHANNEL_HAS_EDIT_LIST))
    {
      reader_out_of_data(sf);
      return;
    }
  at_start = ((!sf->cb) || 
	      (!sf->current_sound) || 
	      (READER_LOCAL_POSITION(sf) >= io_beg(sf->current_sound->io)));
  if (at_start)
    {
      snd_data *prev_snd;
      if (sf->current_sound) 
	{
	  prev_snd = sf->current_sound; 
	  prev_snd->inuse = false; 
	  sf->current_sound = NULL;
	  if (prev_snd->copy) free_snd_data(prev_snd);
	}
      if (sf->cbi == 0) 
	{
	  reader_out_of_data(sf);
	  return;
	}
      sf->cbi--;
      /* now start in the final portion of this block (if a file) */
      sf->cb = FRAGMENT((sf->current_state), sf->cbi);
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      sf->fscaler = READER_SCALER(sf);
      if (zero_op(READER_TYPE(sf)))
	{
	  sf->current_sound = NULL;
	  sf->loc = ind1;
	  sf->first = ind0;
	  sf->last = ind1;
	  sf->data = NULL;
	}
      else
	{
	  prev_snd = sf->cp->sounds[READER_SOUND(sf)];
	  if (prev_snd->type == SND_DATA_FILE)
	    {
	      if (prev_snd->inuse) 
		{
		  prev_snd = copy_snd_data(prev_snd, ind0, FILE_BUFFER_SIZE);
		  if (!prev_snd)
		    {
		      /* too many files open or something of that sort */
		      reader_out_of_data(sf);
		      return;
		    }
		}
	      sf->data = prev_snd->buffered_data;
	      prev_snd->inuse = true;
	      sf->current_sound = prev_snd;
	      file_buffers_back(ind0, ind1, ind1, sf, prev_snd);
	    }
	  else 
	    {
	      sf->data = prev_snd->buffered_data;
	      sf->loc = ind1;
	      sf->first = ind0;
	      sf->last = ind1;
	    }
	}
      sf->frag_pos = ind1 - ind0;
      choose_accessor(sf);
    }
  else
    {
      mus_long_t indx;
      /* back up in current file */
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      indx = io_beg(sf->current_sound->io) - 1;
      file_buffers_back(ind0, ind1, indx, sf, sf->current_sound);
    }
}


static mus_float_t previous_sound(snd_fd *sf)
{
  previous_sound_1(sf);
  return(read_sample(sf));
}


static void next_sound_1(snd_fd *sf)
{
  mus_long_t ind0, ind1;
  bool at_end = false;

  if ((sf->cp) && 
      (sf->cp->active < CHANNEL_HAS_EDIT_LIST))
    {
      reader_out_of_data(sf);
      return;
    }
  at_end = ((!sf->cb) || 
	    (!sf->current_sound) || 
	    (READER_LOCAL_END(sf) <= io_end(sf->current_sound->io)));

  if (at_end)
    {
      snd_data *nxt_snd;
      if (sf->current_sound) 
	{
	  nxt_snd = sf->current_sound; 
	  nxt_snd->inuse = false; 
	  sf->current_sound = NULL;
	  if (nxt_snd->copy) free_snd_data(nxt_snd);
	}
      sf->cbi++;
      if (sf->cbi >= (sf->current_state)->size) 
	{
	  reader_out_of_data(sf);
	  return;
	}
      sf->cb = FRAGMENT((sf->current_state), sf->cbi);
      if ((!(sf->cb)) || 
	  (READER_SOUND(sf) == EDIT_LIST_END_MARK))
	{
	  reader_out_of_data(sf);
	  return;
	}
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      sf->fscaler = READER_SCALER(sf);
      if (zero_op(READER_TYPE(sf)))
	{
	  sf->current_sound = NULL;
	  sf->loc = ind0;
	  sf->first = ind0;
	  sf->last = ind1;
	  sf->data = NULL;
	}
      else
	{
	  nxt_snd = sf->cp->sounds[READER_SOUND(sf)];
	  if (nxt_snd->type == SND_DATA_FILE)
	    {
	      if (nxt_snd->inuse)
		{
		  nxt_snd = copy_snd_data(nxt_snd, ind0, FILE_BUFFER_SIZE);
		  if (!nxt_snd)
		    {
		      reader_out_of_data(sf);
		      return;
		    }
		}
	      sf->data = nxt_snd->buffered_data;
	      nxt_snd->inuse = true;
	      sf->current_sound = nxt_snd;
	      file_buffers_forward(ind0, ind1, ind0, sf, nxt_snd);
	    }
	  else 
	    {
	      sf->data = nxt_snd->buffered_data;
	      sf->loc = ind0;
	      sf->first = ind0;
	      sf->last = ind1;
	    }
	}
      sf->frag_pos = 0;
      choose_accessor(sf);
    }
  else
    { 
      mus_long_t indx;
      ind0 = READER_LOCAL_POSITION(sf);
      ind1 = READER_LOCAL_END(sf);
      indx = io_end(sf->current_sound->io) + 1;
      file_buffers_forward(ind0, ind1, indx, sf, sf->current_sound);
    }
}


mus_float_t next_sound(snd_fd *sf)
{
  next_sound_1(sf);
  return(read_sample(sf));
}


void copy_then_swap_channels(chan_info *cp0, chan_info *cp1, int pos0, int pos1)
{
  int i, fd, new0, new1;
  char *name;
  ed_list *new_ed, *old_ed;
  file_info *hdr0, *hdr1;
  peak_env_info *e0 = NULL, *e1 = NULL;

  if ((!(prepare_edit_list(cp0, AT_CURRENT_EDIT_POSITION, S_swap_channels))) ||
      (!(prepare_edit_list(cp1, AT_CURRENT_EDIT_POSITION, S_swap_channels))))
    return;

  name = cp0->sound->filename;
  hdr0 = copy_header(name, cp0->sound->hdr);
  fd = snd_open_read(name);
  snd_file_open_descriptors(fd,
			    name,
			    hdr0->sample_type,
			    hdr0->data_location,
			    hdr0->chans,
			    hdr0->type);
  new0 = add_sound_file_to_edit_list(cp1, name,
				     make_file_state(fd, hdr0, cp0->chan, 0, FILE_BUFFER_SIZE),
				     hdr0, DONT_DELETE_ME, cp0->chan);
  name = cp1->sound->filename;
  hdr1 = copy_header(name, cp1->sound->hdr);
  fd = snd_open_read(name);
  snd_file_open_descriptors(fd,
			    name,
			    hdr1->sample_type,
			    hdr1->data_location,
			    hdr1->chans,
			    hdr1->type);
  new1 = add_sound_file_to_edit_list(cp0, name,
				     make_file_state(fd, hdr1, cp1->chan, 0, FILE_BUFFER_SIZE),
				     hdr1, DONT_DELETE_ME, cp1->chan);
  e0 = peak_env_copy(cp0, false, pos0);
  if (e0) e1 = peak_env_copy(cp1, false, pos1);
  old_ed = cp1->edits[pos1];
  new_ed = make_ed_list(old_ed->size);
  new_ed->edit_type = CHANGE_EDIT;
  new_ed->sound_location = new1;
  new_ed->edpos = pos1;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->maxamp_position = old_ed->maxamp_position;
  new_ed->samples = old_ed->samples;
  new_ed->cursor = old_ed->cursor;
  new_ed->beg = 0;
  new_ed->len = old_ed->len;
  new_ed->origin = mus_strdup(to_proc_name(S_swap_channels));
  cp0->edits[cp0->edit_ctr] = new_ed;
  if (new_ed->len > 0)
    for (i = 0; i < new_ed->size; i++) 
      {
	copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	if (FRAGMENT_SOUND(new_ed, i) == 0) FRAGMENT_SOUND(new_ed, i) = new1;
      }
  old_ed = cp0->edits[pos0];
  new_ed = make_ed_list(old_ed->size);
  new_ed->edit_type = CHANGE_EDIT;
  new_ed->sound_location = new0;
  new_ed->edpos = pos0;
  new_ed->maxamp = old_ed->maxamp;
  new_ed->maxamp_position = old_ed->maxamp_position;
  new_ed->beg = 0;
  new_ed->len = old_ed->len;
  new_ed->samples = old_ed->samples;
  new_ed->cursor = old_ed->cursor;
  new_ed->origin = mus_strdup(to_proc_name(S_swap_channels)); /* swap = stored change-edit at restore time, so no redundancy here */
  cp1->edits[cp1->edit_ctr] = new_ed;
  if (new_ed->len > 0)
    for (i = 0; i < new_ed->size; i++) 
      {
	copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	if (FRAGMENT_SOUND(new_ed, i) == 0) FRAGMENT_SOUND(new_ed, i) = new0;
      }
  if ((e0) && (e1))
    {
      cp0->edits[cp0->edit_ctr]->peak_env = e1;
      cp1->edits[cp1->edit_ctr]->peak_env = e0;
    }
  else
    {
      if (e0) free_peak_env_info(e0);
      if (e1) free_peak_env_info(e1);
    }

  ripple_all(cp0, 0, 0);
  ripple_all(cp1, 0, 0);
  swap_marks(cp0, cp1);
  if (cp0->edits[cp0->edit_ctr]->samples != cp0->edits[cp0->edit_ctr - 1]->samples)
    reflect_sample_change_in_axis(cp0);
  if (cp1->edits[cp1->edit_ctr]->samples != cp1->edits[cp1->edit_ctr - 1]->samples)
    reflect_sample_change_in_axis(cp1);
  after_edit(cp0);
  after_edit(cp1);
  update_graph(cp0);
  update_graph(cp1);
}


io_error_t save_edits_and_update_display(snd_info *sp)
{
  /* open temp, write current state, rename to old, reopen and clear all state */
  /* can't overwrite current because we may have cut/paste backpointers scattered around the current edit list */
  /* have to decide here what header/data type to write as well -- original? */
  /* if latter, must be able to write all headers! -- perhaps warn user and use snd/aiff/riff/ircam */

  /* read_only already checked */
  char *ofile = NULL;
  int i;
  mus_long_t samples = 0;
  mus_long_t *old_cursors = NULL;
  void *ms;
  axes_data *sa;
  file_info *sphdr = NULL;
  io_error_t io_err = IO_NO_ERROR;
  snd_fd **sf;
  mus_float_t *vals = NULL;
  mus_long_t *times = NULL;
  bool have_maxamps = true;

  if (dont_save(sp, NULL)) return(IO_SAVE_HOOK_CANCELLATION);
  ofile = snd_tempnam(); 
  /* this will use user's TMPDIR if temp_dir(ss) is not set, else stdio.h's P_tmpdir else /tmp */

  for (i = 0; i < (int)sp->nchans; i++)
    {
      chan_info *ncp;
      ncp = sp->chans[i];
      if ((ed_maxamp(ncp, ncp->edit_ctr) < 0.0) ||
	  (ed_maxamp_position(ncp, ncp->edit_ctr) < 0))
	{
	  have_maxamps = false;
	  break;
	}
    }
  if (have_maxamps)
    {
      vals = (mus_float_t *)calloc(sp->nchans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(sp->nchans, sizeof(mus_long_t));
      for (i = 0; i < (int)sp->nchans; i++)
	{
	  chan_info *ncp;
	  ncp = sp->chans[i];
	  vals[i] = ed_maxamp(ncp, ncp->edit_ctr);
	  times[i] = ed_maxamp_position(ncp, ncp->edit_ctr);
	}
    }
  
  sf = (snd_fd **)calloc(sp->nchans, sizeof(snd_fd *));
  for (i = 0; i < (int)sp->nchans; i++)
    {
      sf[i] = init_sample_read(0, sp->chans[i], READ_FORWARD);
      if (!sf[i])
	{
	  int j;
	  for (j = 0; j < i; j++) free_snd_fd(sf[j]);
	  free(sf);
	  if (vals) free(vals);
	  if (times) free(times);
	  return(IO_BAD_CHANNEL);
	}
      if (samples < current_samples(sp->chans[i]))
	samples = current_samples(sp->chans[i]);
    }
  
  /* write the new file */
  io_err = snd_make_file(ofile, sp->nchans, sp->hdr, sf, samples, true);
  for (i = 0; i < (int)sp->nchans; i++) free_snd_fd(sf[i]);
  free(sf);
  sf = NULL;
  if (io_err != IO_NO_ERROR)
    {
      if (ofile) free(ofile);
      if (vals)
	{
	  free(vals);
	  free(times);
	}
      return(io_err);
    }
  
  sa = make_axes_data(sp);
  sphdr = sp->hdr;
  sphdr->samples = samples * sp->nchans;
  ms = (void *)sound_store_marks(sp);
  old_cursors = (mus_long_t *)calloc(sp->nchans, sizeof(mus_long_t));
  for (i = 0; i < (int)sp->nchans; i++)
    {
      chan_info *cp;
      cp = sp->chans[i];
      old_cursors[i] = cursor_sample(cp);        /* depends on edit_ctr -- set to -1 by free_edit_list below */
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, -1);
      if (cp->edits) free_edit_list(cp); /* sets cp->edits to NULL */
      if (cp->sounds) free_sound_list(cp);
      cp->axis = free_axis_info(cp->axis);
    }

#ifndef _MSC_VER
  if (access(sp->filename, W_OK))
    {
      free_axes_data(sa);
      if (ofile) free(ofile);
      if (old_cursors) free(old_cursors);
      if (vals)
	{
	  free(vals);
	  free(times);
	}
      return(IO_WRITE_PROTECTED);
    }
#endif
  
  mus_sound_forget(sp->filename);
  sp->writing = true;
  io_err = move_file(ofile, sp->filename); /* should we cancel and restart a monitor? */
  sp->writing = false;
  if (io_err != IO_NO_ERROR)
    {
      if (ofile) free(ofile);
      if (old_cursors) free(old_cursors);
      if (vals)
	{
	  free(vals);
	  free(times);
	}
      return(io_err);
    }

  if (vals)
    {
      mus_sound_set_maxamps(sp->filename, sp->nchans, vals, times);
      free(vals);
      free(times);
    }
  sp->write_date = file_write_date(sp->filename);
  add_sound_data(sp->filename, sp, WITHOUT_INITIAL_GRAPH_HOOK);
  restore_axes_data(sp, sa, mus_sound_duration(sp->filename), true);
  sound_restore_marks(sp, ms);
  free_axes_data(sa);
  for (i = 0; i < (int)sp->nchans; i++)
    cursor_sample(sp->chans[i]) = old_cursors[i];
  free(old_cursors);
  reflect_file_revert_in_label(sp);
  if (ofile) 
    {
      free(ofile); 
      ofile = NULL;
    }
  if (!(ss->file_monitor_ok))
    if (auto_update(ss)) 
      for_each_sound(sound_not_current);
  return(IO_NO_ERROR);
}


io_error_t save_edits_without_display(snd_info *sp, const char *new_name, mus_header_t type, 
				      mus_sample_t sample_type, int srate, const char *comment, int pos)
{ 
  /* assume we've already checked for (over)write permissions, and header-type+sample-type writable,
   */
  file_info *hdr;
  snd_fd **sf;
  mus_long_t framples = 0;
  int i;
  file_info *ohdr;
  io_error_t err = IO_NO_ERROR;
  mus_float_t *vals = NULL;
  mus_long_t *times = NULL;
  bool have_maxamps = true;

  if (dont_save(sp, new_name)) 
    return(IO_SAVE_HOOK_CANCELLATION);

  ohdr = sp->hdr;
  hdr = copy_header(new_name, ohdr);
  hdr->sample_type = sample_type;
  hdr->srate = srate;
  hdr->type = type;
  if (comment) 
    hdr->comment = mus_strdup(comment); 
  else hdr->comment = NULL;
  hdr->data_location = 0; /* in case comment changes it */

  for (i = 0; i < (int)sp->nchans; i++)
    {
      chan_info *ncp;
      ncp = sp->chans[i];
      if ((ed_maxamp(ncp, ncp->edit_ctr) < 0.0) ||
	  (ed_maxamp_position(ncp, ncp->edit_ctr) < 0))
	{
	  have_maxamps = false;
	  break;
	}
    }
  if (have_maxamps)
    {
      vals = (mus_float_t *)calloc(sp->nchans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(sp->nchans, sizeof(mus_long_t));
      for (i = 0; i < (int)sp->nchans; i++)
	{
	  chan_info *ncp;
	  ncp = sp->chans[i];
	  vals[i] = ed_maxamp(ncp, ncp->edit_ctr);
	  times[i] = ed_maxamp_position(ncp, ncp->edit_ctr);
	}
    }

  sf = (snd_fd **)malloc(sp->nchans * sizeof(snd_fd *));
  for (i = 0; i < (int)sp->nchans; i++) 
    {
      chan_info *cp;
      int local_pos;
      cp = sp->chans[i];
      if (pos == AT_CURRENT_EDIT_POSITION) local_pos = cp->edit_ctr; else local_pos = pos;
      if (framples < cp->edits[local_pos]->samples) framples = cp->edits[local_pos]->samples;
      sf[i] = init_sample_read_any(0, cp, READ_FORWARD, local_pos); 
      if (!sf[i])
	{
	  uint32_t k;
	  /* this should not (cannot?) happen since we've supposedly checked before getting here... */
	  for (k = 0; k < sp->nchans; k++) 
	    sf[k] = free_snd_fd(sf[k]);
	  free(sf);
	  sf = NULL;
	  free_file_info(hdr);
	  if (vals)
	    {
	      free(vals);
	      free(times);
	    }
	  return(err);
	}
    }

  err = snd_make_file(new_name, sp->nchans, hdr, sf, framples, true);
  if (vals)
    {
      if (err == IO_NO_ERROR)
	mus_sound_set_maxamps(new_name, sp->nchans, vals, times);
      free(vals);
      free(times);
    }

  for (i = 0; i < (int)sp->nchans; i++) 
    free_snd_fd(sf[i]);
  free(sf);
  free_file_info(hdr);

  return(err);
}


io_error_t save_channel_edits(chan_info *cp, const char *ofile, int pos)
{
  /* channel extraction -- does not (normally) cause reversion of edits, or change of in-window file, etc */
  snd_info *sp;
  io_error_t err = IO_NO_ERROR;
  sp = cp->sound;
  if (pos == AT_CURRENT_EDIT_POSITION) 
    pos = cp->edit_ctr;
  if (mus_strcmp(ofile, sp->filename))        /* overwriting current file with one of its channels */
    {
      char *nfile = NULL;
      if ((sp->user_read_only == FILE_READ_ONLY) || 
	  (sp->file_read_only == FILE_READ_ONLY))
	{
	  snd_error("can't save channel as %s (%s is write-protected)", ofile, sp->short_filename);
	  return(IO_WRITE_PROTECTED);
	}
      nfile = snd_tempnam();
      err = channel_to_file(cp, nfile, pos);  /* snd_error unless MUS_INTERRUPTED (???) */
      if (err == IO_NO_ERROR)
	{
	  err = move_file(nfile, ofile);
	  if (err == IO_NO_ERROR)
	    snd_update(sp);
	  else
	    {
	      if (is_serious_io_error(err))
		{
		  free(nfile);
		  nfile = NULL;
		  snd_error("save channel %s -> %s: %s (%s)", 
			    nfile, ofile, 
			    io_error_name(err),
			    snd_io_strerror());
		}
	    }
	}
      if (nfile) free(nfile);
    }
  else err = channel_to_file(cp, ofile, pos); /* snd_error unless MUS_INTERRUPTED */
  return(err);
}


bool has_unsaved_edits(snd_info *sp)
{
  uint32_t i;
  for (i = 0; i < sp->nchans; i++)
    if (sp->chans[i]->edit_ctr > 0)
      return(true);
  return(false);
}


static io_error_t save_edits_1(snd_info *sp, bool ask)
{
  io_error_t err;
  time_t current_write_date;

  if (!sp)
    snd_error_without_format("save edits of null sound!");
  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    return(IO_WRITE_PROTECTED);

  if (!(has_unsaved_edits(sp))) return(IO_NO_CHANGES);

  /* check for change to file while we were editing it */
  current_write_date = file_write_date(sp->filename);
  /* returns -1 if file does not exist (stat -> -1) */

  if (current_write_date < 0)
    {
      snd_error("can't save edits; %s has disappeared!", sp->filename); 
      /* unless by chance it fits in one in-core buffer, there's nothing we can do now */
      return(IO_CANT_OPEN_FILE);
    }
  if ((ask) &&
      ((current_write_date - sp->write_date) > 1)) /* In Redhat 7.1 these can differ by 1?? Surely this is a bug! */
    return(IO_NEED_WRITE_CONFIRMATION);            /* see snd-kbd.c save_edits_from_kbd for the rest of this */

  /* this used to include ask_before_overwrite, but I don't think that is relevant.  If the file
   *    has changed from what we think it is, we have a problem -- the save is unlikely to do anything useful.
   */

  err = save_edits_and_update_display(sp);

  if (err == IO_NO_ERROR)
    {
      if (sp->edited_region) 
	save_region_backpointer(sp); /* region edit save is not reflected in other sounds because the region is a separate thing from the selection */
    }
  return(err);
}


io_error_t save_edits(snd_info *sp)
{
  return(save_edits_1(sp, true));
}


io_error_t save_edits_without_asking(snd_info *sp)
{
  return(save_edits_1(sp, false));
}


void revert_edits(chan_info *cp)
{
  if (cp->edit_ctr == 0) return;
  cp->edit_ctr = 0;
  clear_transform_edit_ctrs(cp);
  reflect_edit_counter_change(cp);
  reflect_sample_change_in_axis(cp);
  enved_reflect_selection(selection_is_active());
  reflect_selection_in_save_as_dialog(selection_is_active());

  if (Xen_hook_has_list(ss->effects_hook))
    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

  update_graph(cp);
  reflect_mix_change(ANY_MIX_ID);
  reflect_enved_fft_change(cp);
  if ((cp->hookable == WITH_HOOK) &&
      (Xen_is_hook(cp->undo_hook)) && 
      (Xen_hook_has_list(cp->undo_hook)))
    run_hook(cp->undo_hook, Xen_empty_list, S_undo_hook);
}


/* how to handle something like safe-map-channel that wants to disallow undo? */

bool undo_edit(chan_info *cp, int count)
{
  if ((cp) && 
      (cp->edit_ctr > 0) && 
      (count != 0))
    {
      snd_info *sp;
      sp = cp->sound;
      cp->edit_ctr -= count; 
      if (cp->edit_ctr < 0) cp->edit_ctr = 0;
      clear_transform_edit_ctrs(cp);
      reflect_edit_counter_change(cp);
      reflect_sample_change_in_axis(cp);
      enved_reflect_selection(selection_is_active());
      reflect_selection_in_save_as_dialog(selection_is_active());
      if (cp->edit_ctr == 0)
	{
	  reflect_file_revert_in_label(sp);
	}

      if (Xen_hook_has_list(ss->effects_hook))
	run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

      update_graph(cp);
      reflect_mix_change(ANY_MIX_ID);
      reflect_enved_fft_change(cp);
      if ((cp->hookable == WITH_HOOK) &&
	  (Xen_is_hook(cp->undo_hook)) && 
	  (Xen_hook_has_list(cp->undo_hook)))
	run_hook(cp->undo_hook, Xen_empty_list, S_undo_hook);
      return(true);
    }
  return(false);
}


bool undo_edit_with_sync(chan_info *cp, int count)
{
  /* there is a problem with syncd undo: if the syncd edit decided one portion
   *   was a no-op (scale by 1.0 etc), but not another, a subsequent undo with
   *   sync can end up in a state different from where it started.
   */
  if (count == 0) return(false);
  if (count < 0)
    return(redo_edit_with_sync(cp, -count));
  else
    {
      if (cp)
	{
	  snd_info *sp;
	  sync_info *si = NULL;
	  sp = cp->sound;
	  if (sp->sync != 0) si = snd_sync(sp->sync);
	  if (si)
	    {
	      bool something_changed = false;
	      int i;
	      for (i = 0; i < si->chans; i++) 
		if (undo_edit(si->cps[i], count))
		  something_changed = true;
	      free_sync_info(si);
	      return(something_changed);
	    }
	  else return(undo_edit(cp, count));
	}
    }
  return(false);
}


bool redo_edit(chan_info *cp, int count)
{
  /* returns true if an edit history change occurred */
  if ((cp) && (count != 0))
    {
      int old_edit_ctr;
      old_edit_ctr = cp->edit_ctr;
      cp->edit_ctr += count;
      if (cp->edit_ctr >= cp->edit_size) cp->edit_ctr = cp->edit_size - 1;
      while (!(cp->edits[cp->edit_ctr]))
	cp->edit_ctr--;
      if ((cp->edit_ctr != 0) &&          /* possibly a sync'd redo to chan that has no edits */
	  (cp->edit_ctr != old_edit_ctr)) /* or attempt to redo when nothing to redo */
	{
	  clear_transform_edit_ctrs(cp);
	  reflect_file_change_in_label(cp);
	  reflect_edit_counter_change(cp);
	  reflect_sample_change_in_axis(cp);
	  enved_reflect_selection(selection_is_active());
	  reflect_selection_in_save_as_dialog(selection_is_active());

	  if (Xen_hook_has_list(ss->effects_hook))
	    run_hook(ss->effects_hook, Xen_empty_list, S_effects_hook);

	  update_graph(cp);
	  reflect_mix_change(ANY_MIX_ID);
	  reflect_enved_fft_change(cp);
	  if ((cp->hookable == WITH_HOOK) &&
	      (Xen_is_hook(cp->undo_hook)) && 
	      (Xen_hook_has_list(cp->undo_hook)))
	    run_hook(cp->undo_hook, Xen_empty_list, S_undo_hook);
	  return(true);
	}
    }
  return(false);
}


bool redo_edit_with_sync(chan_info *cp, int count)
{
  if (count == 0) return(false);
  if (count < 0)
    return(undo_edit_with_sync(cp, -count));
  else
    {
      if (cp)
	{
	  snd_info *sp;
	  sync_info *si = NULL;
	  sp = cp->sound;
	  if (sp->sync != 0) si = snd_sync(sp->sync);
	  if (si)
	    {
	      bool something_changed = false;
	      int i;
	      for (i = 0; i < si->chans; i++) 
		if (redo_edit(si->cps[i], count))
		  something_changed = true;
	      free_sync_info(si);
	      return(something_changed);
	    }
	  else return(redo_edit(cp, count));
	}
    }
  return(false);
}



/* -------------------- virtual mixes -------------------- */

/* ramp on mix is technically possible, but ambiguous -- currently if we mix, then ramp elsewhere, then drag the mix
 *   to the ramped portion, the mix treats the ramp as prior data (adding);  if we had ramp_mix, it would want to
 *   treat that ramp as an envelope on the mix if the ramp happened after the mix was established but as data if before.
 *   Too tricky.  (We'd also need ED_RAMP_ZERO to make clean fixups upon drag and so on).
 *   Three times and out so far...
 *   ...
 *   but, if the envelope is global, it is not ambiguous, (except that we have scaled portions?) --
 *     still the ramp-but-no-mix sections need special handling (ramp_mix with 0 mixes?),
 *     as do the ramp-as-scale-but-no-mix portions (scale_mix? -- confusing name)
 *   ...
 *   this (ramp_mix as edit with locked mixes) turned out to be too tricky -- scaling especially is a mess.
 *   (scaling can't be globalized without saving the pre-mix scaler on every mix, for a start)
 */

static void make_mix_fragment(ed_list *new_ed, int i, mix_state *ms)
{
  ed_mixes *mxs;
  int mloc = -1;

  /* mix_loc = index into cp->sound arrays for reading the mixed-in data (buffer or file) */
  /* i = index into ed_fragment list for this edit */
  /* we're changing one fragment to add the mix */

  FRAGMENT_TYPE(new_ed, i) = type_info[FRAGMENT_TYPE(new_ed, i)].add_mix;
  if (!(FRAGMENT_MIXES(new_ed, i)))
    FRAGMENT_MIXES(new_ed, i) = (ed_mixes *)calloc(1, sizeof(ed_mixes));
  mxs = FRAGMENT_MIXES(new_ed, i);
  if (mxs->size == 0)
    {
      mxs->size = 1;
      mxs->mix_list = (mix_state **)malloc(sizeof(mix_state *));
      mloc = 0;
    }
  else
    {
      int j;
      for (j = 0; j < mxs->size; j++)
	if (!(MIX_LIST_STATE(mxs, j)))
	  {
	    mloc = j;
	    break;
	  }
      if (mloc == -1)
	{
	  mloc = mxs->size;
	  mxs->size++;
	  mxs->mix_list = (mix_state **)realloc(mxs->mix_list, mxs->size * sizeof(mix_state *));
	}
    }
  MIX_LIST_STATE(mxs, mloc) = ms;
}


static ed_list *make_mix_edit(ed_list *old_ed, mus_long_t beg, mus_long_t len, mix_state *ms, bool full_fragment)
{
  ed_list *new_ed;
  int i;
  if (full_fragment)
    {
      new_ed = make_ed_list(old_ed->size);
      new_ed->beg = 0;
      new_ed->len = len;
      for (i = 0; i < new_ed->size; i++) /* whole file changed in this case */
	{
	  copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(old_ed, i));
	  if (i < new_ed->size - 1)
	    make_mix_fragment(new_ed, i, ms);
	}
    }
  else 
    {
      new_ed = copy_and_split_list(beg, len, old_ed);
      for (i = 0; i < new_ed->size; i++) 
	{
	  if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (beg + len - 1)) 
	    break;                                                    /* not >= (1 sample selections) */
	  if ((FRAGMENT_GLOBAL_POSITION(new_ed, i) >= beg) &&
	      (i < new_ed->size - 1))
	    make_mix_fragment(new_ed, i, ms);
	}
    }

  new_ed->cursor = old_ed->cursor;
  new_ed->edit_type = MIX_EDIT;
  new_ed->sound_location = old_ed->sound_location;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  return(new_ed);
}


int mix_file_with_tag(chan_info *cp, const char *filename, int chan, mus_long_t beg, file_delete_t auto_delete, const char *origin)
{
  mus_long_t file_len, old_len, new_len;
  int edpos;
  int fd, mix_loc;
  ed_list *new_ed, *old_ed;
  file_info *hdr;
  mix_state *ms;
  bool backup = false;

  hdr = make_file_info(filename, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (chan >= hdr->chans)
    return(NO_MIX_TAG);

  edpos = cp->edit_ctr;
  file_len = mus_sound_framples(filename);
  old_ed = cp->edits[edpos];

  if ((beg < 0) || 
      (file_len <= 0))
    return(NO_MIX_TAG); 

  old_len = old_ed->samples;
  if (beg + file_len > old_len)
    {
      if (!(extend_with_zeros(cp, old_len, beg + file_len - old_len, edpos, origin))) 
	return(NO_MIX_TAG);
      edpos = cp->edit_ctr;
      new_len = beg + file_len;
      old_ed = cp->edits[edpos];
      backup = true;
    }
  else new_len = old_len; 

  if (!(prepare_edit_list(cp, edpos, origin)))
    return(NO_MIX_TAG);

  fd = snd_open_read(filename);
  snd_file_open_descriptors(fd,
			    filename,
			    hdr->sample_type,
			    hdr->data_location,
			    hdr->chans,
			    hdr->type);

  mix_loc = add_sound_file_to_edit_list(cp, filename, 
					make_file_state(fd, hdr, chan, 0, FILE_BUFFER_SIZE),
					hdr, auto_delete,
					chan);

  ms = prepare_mix_state_for_channel(cp, mix_loc, beg, file_len);
  new_ed = make_mix_edit(old_ed, beg, file_len, ms, ((beg == 0) && (file_len >= old_len)));
  new_ed->samples = new_len;
  new_ed->origin = mus_strdup(origin);
  new_ed->edpos = edpos;
  cp->edits[cp->edit_ctr] = new_ed;
  add_ed_mix(cp->edits[cp->edit_ctr], ms);

  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  reflect_mix_change(ANY_MIX_ID);
  after_edit(cp);

  if (backup)
    backup_edit_list(cp);

  return(ms->mix_id);
}


int mix_buffer_with_tag(chan_info *cp, mus_float_t *data, mus_long_t beg, mus_long_t buf_len, const char *origin)
{
  int edpos, mix_loc;
  mus_long_t old_len, new_len;
  ed_list *old_ed, *new_ed;
  bool backup = false;
  mix_state *ms;

  edpos = cp->edit_ctr;
  old_ed = cp->edits[edpos];

  if ((beg < 0) || 
      (buf_len == 0))
    return(NO_MIX_TAG);

  old_len = old_ed->samples;
  if (beg + buf_len > old_len)
    {
      if (!(extend_with_zeros(cp, old_len, beg + buf_len - old_len, edpos, origin))) 
	return(NO_MIX_TAG);
      edpos = cp->edit_ctr;
      new_len = beg + buf_len;
      old_ed = cp->edits[edpos];
      backup = true;
    }
  else new_len = old_len; 

  if (!(prepare_edit_list(cp, edpos, origin)))
    return(NO_MIX_TAG);

  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(data, (int)buf_len, cp->edit_ctr);
  mix_loc = cp->sound_ctr;
  
  ms = prepare_mix_state_for_channel(cp, mix_loc, beg, buf_len);
  new_ed = make_mix_edit(old_ed, beg, buf_len, ms, ((beg == 0) && (buf_len >= old_len)));
  new_ed->samples = new_len;
  new_ed->origin = mus_strdup(origin);
  new_ed->edpos = edpos;
  cp->edits[cp->edit_ctr] = new_ed;
  add_ed_mix(cp->edits[cp->edit_ctr], ms);

  ripple_all(cp, 0, 0); /* 0,0 -> copy marks */
  reflect_mix_change(ANY_MIX_ID);
  after_edit(cp);

  if (backup)
    backup_edit_list(cp);

  return(ms->mix_id);
}


void unmix(chan_info *cp, mix_state *ms)
{
  /* assume both mix_list (via ripple) and ed fragments list are ready for the unmix */
  /* used in set position and set speed (snd-mix) to remove ms prior to the edit/remix */
  int i;
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  for (i = 0; i < ed->size; i++) 
    {
      if ((FRAGMENT_GLOBAL_POSITION(ed, i) >= ms->beg) &&
	  (FRAGMENT_GLOBAL_POSITION(ed, i) < (ms->beg + ms->len)) &&
	  (is_unmixable_op(FRAGMENT_TYPE(ed, i))))
	{
	  /* look for ms in the current fragment's mix list */
	  int j, remaining_mixes = 0, mss_size;
	  ed_mixes *mxl;
	  mix_state **mss;
	  mxl = FRAGMENT_MIXES(ed, i);
	  mss = FRAGMENT_MIX_LIST(ed, i);
	  mss_size = FRAGMENT_MIX_LIST_SIZE(ed, i);
	  for (j = 0; j < mss_size; j++)
	    if (mss[j])
	      {
		if (mss[j]->index == ms->index)
		  mss[j] = NULL;
		else remaining_mixes++;
	      }
	  if (remaining_mixes == 0)
	    {
	      FRAGMENT_TYPE(ed, i) = type_info[FRAGMENT_TYPE(ed, i)].subtract_mix;
	      free(mss);
	      free(mxl);
	      FRAGMENT_MIXES(ed, i) = NULL;
	    }
	}
    }
}


void remix(chan_info *cp, mix_state *ms)
{
  int i;
  ed_list *new_ed;
  new_ed = cp->edits[cp->edit_ctr];
  for (i = 0; i < new_ed->size; i++) 
    {
      if (FRAGMENT_GLOBAL_POSITION(new_ed, i) > (ms->beg + ms->len - 1)) 
	break;                                                    /* not >= (1 sample selections) */
      if (FRAGMENT_GLOBAL_POSITION(new_ed, i) >= ms->beg)
	make_mix_fragment(new_ed, i, ms);
    }
}


static void ripple_mixes_1(chan_info *cp, mus_long_t beg, mus_long_t len, mus_long_t change, mus_float_t scl)
{
  /* this is where most of the time goes in mixing! */
  if ((cp) &&
      (cp->edit_ctr > 0))
    {
      ed_list *ed;
      int i, low_id = 0, high_id = 0, size = 0; /* low_id confuses the compiler, but it will always get set below (current_states starts NULL etc) */
      mix_state **current_states = NULL;

      ed = cp->edits[cp->edit_ctr];
      /* this may have mixes already (current op was mix op) or might be null */
      for (i = 0; i < ed->size; i++) 
	{
	  if ((FRAGMENT_MIXES(ed, i)) &&
	      (FRAGMENT_MIX_LIST(ed, i)) &&
	      (FRAGMENT_MIX_LIST_SIZE(ed, i) > 0))
	    {
	      int j;
	      if (!current_states)
		{
		  low_id = lowest_mix_id();
		  high_id = highest_mix_id();
		  size = high_id - low_id + 1;
		  current_states = (mix_state **)calloc(size, sizeof(mix_state *));
		  preload_mixes(current_states, low_id, ed);
		}
	      for (j = 0; j < FRAGMENT_MIX_LIST_SIZE(ed, i); j++)
		{
		  mix_state *old_ms;
		  old_ms = FRAGMENT_MIX_STATE(ed, i, j);            /* the old copy -- we (may) need to make a new one */
		  if (old_ms)
		    {
		      mix_state *new_ms;
		      new_ms = current_states[old_ms->mix_id - low_id];
		      if (!new_ms)
			{
			  new_ms = copy_mix_state(old_ms); /* cannot return null unless we're out of memory */
			  add_ed_mix(ed, new_ms);
			  if (new_ms->beg >= beg)
			    {
			      if ((len != 0) &&
				  (new_ms->beg < beg + len))
				new_ms->scaler *= scl;
			      if (change != 0)
				new_ms->beg += change;
			    }
			}
		      FRAGMENT_MIX_STATE(ed, i, j) = new_ms;
		      if (((new_ms->mix_id - low_id) < size) &&
			  ((new_ms->mix_id - low_id) >= 0))
			current_states[new_ms->mix_id - low_id] = new_ms;
		    }
		}
	    }
	}
      if (current_states) free(current_states);
    }
}


static void ripple_mixes(chan_info *cp, mus_long_t beg, mus_long_t change)
{
  ripple_mixes_1(cp, beg, 0, change, 1.0);
}


static void ripple_mixes_with_scale(chan_info *cp, mus_long_t beg, mus_long_t len, mus_float_t scl)
{
  ripple_mixes_1(cp, beg, len, 0, scl);
}


snd_fd *make_virtual_mix_reader(chan_info *cp, mus_long_t beg, mus_long_t len, int index, mus_float_t scl, read_direction_t direction)
{
  snd_fd *sf;
  snd_data *first_snd;
  mus_long_t ind0, ind1, indx;

  sf = (snd_fd *)calloc(1, sizeof(snd_fd));

  sf->freed = false;
  sf->region = INVALID_MIX_ID;
  sf->type = MIX_READER;
  sf->initial_samp = beg;
  sf->cp = cp;
  sf->direction = direction;
  sf->current_state = initial_ed_list(0, len - 1);  /* need size field here to signal eof -- GC'd in free_reader_mixes */
  sf->cb = FRAGMENT(sf->current_state, 0);
  sf->edit_ctr = 0;
  first_snd = cp->sounds[index];
  sf->frag_pos = 0;

  ind0 = 0;
  indx = beg;
  ind1 = len - 1; /* ind1 (LOCAL_END...) is a sample number, not a length */
  sf->fscaler = scl;

  if ((scl == 1.0) &&
      (sf->fscaler == 1.0))
    {
      sf->runf = next_sample_value_unscaled;
      sf->rev_runf = previous_sample_value_unscaled;
    }
  else
    {
      if (scl == 0.0)
	{
	  sf->runf = next_zero;
	  sf->rev_runf = previous_zero;
	}
      else
	{
	  sf->runf = next_sample_value;
	  sf->rev_runf = previous_sample_value;
	}
    }

  if (direction == READ_BACKWARD)
    swap_readers(sf);

  if (first_snd->type == SND_DATA_FILE)
    {

      if (first_snd->inuse)
	{
	  first_snd = copy_snd_data(first_snd, beg, FILE_BUFFER_SIZE);
	  if (!first_snd)
	    return(cancel_reader(sf));
	}

      first_snd->inuse = true;
      sf->current_sound = first_snd;
      sf->data = first_snd->buffered_data;
      if (direction == READ_FORWARD)
	file_buffers_forward(ind0, ind1, indx, sf, first_snd);
      else file_buffers_back(ind0, ind1, indx, sf, first_snd);
    }
  else 
    {
      sf->current_sound = NULL;
      sf->data = first_snd->buffered_data;
      sf->first = ind0;
      sf->last = ind1;
      sf->loc = indx;
    }
  return(sf);
}


bool begin_mix_op(chan_info *cp, mus_long_t old_beg, mus_long_t old_len, mus_long_t new_beg, mus_long_t new_len, int edpos, const char *caller)
{
  int i;
  ed_list *new_ed, *old_ed, *temp_ed;
  mus_long_t new_samples;

  old_ed = cp->edits[edpos];
  if (!(prepare_edit_list(cp, edpos, caller))) 
    return(false);

  new_samples = new_beg + new_len;

  if (new_samples > old_ed->samples)
    {
      ed_fragment *cb;
      mus_long_t old_pos;
      cb = make_ed_fragment();

      old_pos = FRAGMENT_GLOBAL_POSITION(old_ed, old_ed->size - 1);
      temp_ed = make_ed_list(old_ed->size + 1);
      for (i = 0; i < old_ed->size; i++) 
	copy_ed_fragment(FRAGMENT(temp_ed, i), FRAGMENT(old_ed, i));
      FRAGMENT_GLOBAL_POSITION(temp_ed, old_ed->size - 1) = new_samples + 1;

      if (FRAGMENT(temp_ed, old_ed->size)) free_ed_fragment(FRAGMENT(temp_ed, old_ed->size));
      FRAGMENT(temp_ed, old_ed->size) = FRAGMENT(temp_ed, old_ed->size - 1);
      FRAGMENT(temp_ed, old_ed->size - 1) = cb;
      ED_SOUND(cb) = EDIT_LIST_ZERO_MARK;
      ED_SCALER(cb) = 0.0;
      ED_TYPE(cb) = ED_ZERO;
      ED_GLOBAL_POSITION(cb) = old_pos;
      ED_LOCAL_POSITION(cb) = 0;
      ED_LOCAL_END(cb) = new_samples - old_ed->samples;
    }
  else temp_ed = old_ed;

  if ((new_beg == old_beg) &&
      (new_len == old_len))
    {
      if (temp_ed != old_ed)
	new_ed = temp_ed;
      else
	{
	  new_ed = make_ed_list(temp_ed->size);
	  for (i = 0; i < temp_ed->size; i++) 
	    copy_ed_fragment(FRAGMENT(new_ed, i), FRAGMENT(temp_ed, i));
	}
      new_ed->beg = new_beg;
      new_ed->len = new_len;
    }
  else 
    {
      mus_long_t old_end, new_end;
      new_ed = copy_and_split_list(new_beg, new_len, temp_ed);
      if (temp_ed != old_ed)
	{
	  for (i = 0; i < temp_ed->allocated_size; i++)
	    free_ed_fragment(FRAGMENT(temp_ed, i));
	  free(FRAGMENTS(temp_ed));
	  free(temp_ed);
	}
      old_end = old_beg + old_len;
      new_end = new_beg + new_len;
      if (old_beg < new_beg)
	new_ed->beg = old_beg;
      else new_ed->beg = new_beg;
      if (old_end > new_end)
	new_ed->len = old_end - new_ed->beg + 1;
      else new_ed->len = new_end - new_ed->beg + 1;
    }

  if (new_samples > old_ed->samples)
    new_ed->samples = new_samples;
  else new_ed->samples = old_ed->samples;

  new_ed->cursor = old_ed->cursor;
  new_ed->edit_type = CHANGE_MIX_EDIT;
  new_ed->sound_location = old_ed->sound_location;
  new_ed->origin = mus_strdup(caller);
  new_ed->edpos = edpos;
  new_ed->selection_beg = old_ed->selection_beg;
  new_ed->selection_end = old_ed->selection_end;
  cp->edits[cp->edit_ctr] = new_ed;
  ripple_all(cp, 0, 0); /* 0,0 -> copy current mix (and mark) lists */

  return(true);
}


static int check_splice_at(ed_list *new_ed, mus_long_t beg, int start)
{
  int i;  
  for (i = start; i < new_ed->size; i++)
    {
      if ((FRAGMENT_GLOBAL_POSITION(new_ed, i) == beg) &&
	  ((FRAGMENT_TYPE(new_ed, i) == ED_SIMPLE) || (FRAGMENT_TYPE(new_ed, i) == ED_ZERO)) &&
	  (FRAGMENT_TYPE(new_ed, i - 1) == FRAGMENT_TYPE(new_ed, i)) &&
	  (FRAGMENT_SOUND(new_ed, i) == FRAGMENT_SOUND(new_ed, i - 1)) &&
	  (FRAGMENT_SCALER(new_ed, i) == FRAGMENT_SCALER(new_ed, i -1)) &&
	  (FRAGMENT_LOCAL_END(new_ed, i - 1) == FRAGMENT_LOCAL_POSITION(new_ed, i) - 1))
	{
	  int k;
	  FRAGMENT_LOCAL_END(new_ed, i - 1) = FRAGMENT_LOCAL_END(new_ed, i);
	  free_ed_fragment(FRAGMENT(new_ed, i));
	  for (k = i + 1; k < new_ed->size; k++)
	    FRAGMENT(new_ed, k - 1) = FRAGMENT(new_ed, k);
	  FRAGMENT(new_ed, new_ed->size - 1) = NULL;
	  new_ed->size--;
	  return(i);
	}
    }
  return(0);
}


void end_mix_op(chan_info *cp, mus_long_t old_beg, mus_long_t old_len)
{
  /* if beg != 0, try to remove old splice points */
  if (old_beg > 0)
    {
      int start_loc;
      ed_list *new_ed;
      new_ed = cp->edits[cp->edit_ctr];
      start_loc = check_splice_at(new_ed, old_beg, 1);
      if (old_len > 0)
	check_splice_at(new_ed, old_beg + old_len, start_loc);
    }

  if (cp->edits[cp->edit_ctr - 1]->samples != cp->edits[cp->edit_ctr]->samples)
    reflect_sample_change_in_axis(cp);
  reflect_mix_change(ANY_MIX_ID);
}





/* ----------------------- Xen connection -------------------------------- */

static Xen g_display_edits(Xen snd, Xen chn, Xen edpos)
{
  #define H_display_edits "(" S_display_edits " :optional snd chn edpos): current edit tree"
  FILE *tmp = NULL;
  char *buf, *name;
  chan_info *cp;
  int fd, pos = AT_CURRENT_EDIT_POSITION;
  mus_long_t len;
  Xen res;
  ssize_t bytes;

  Snd_assert_channel(S_display_edits, snd, chn, 1);
  cp = get_cp(snd, chn, S_display_edits);
  if (!cp) return(Xen_false);

  if (Xen_is_integer(edpos)) 
    {
      pos = Xen_integer_to_C_int(edpos);
      if (pos == AT_CURRENT_EDIT_POSITION)
	pos = cp->edit_ctr;
      if ((pos < 0) || (pos >= cp->edit_size) || (!(cp->edits[pos])))
	Xen_error(Xen_make_error_type("no-such-edit"),
		  Xen_list_2(C_string_to_Xen_string(S_display_edits ": no such edit: ~A"),
			     edpos));
    }
  name = snd_tempnam();
  tmp = FOPEN(name, "w");
  if (tmp)
    {
      if (pos != AT_CURRENT_EDIT_POSITION)
	display_ed_list(cp, tmp, pos, cp->edits[pos]);
      else display_edits(cp, tmp);
      snd_fclose(tmp, name);
    }
  else Xen_error(Xen_make_error_type("cannot-save"),
		 Xen_list_3(C_string_to_Xen_string(S_display_edits ": can't save ~S, ~A"),
			    C_string_to_Xen_string(name),
			    C_string_to_Xen_string(snd_io_strerror())));
  fd = mus_file_open_read(name);
  len = lseek(fd, 0L, SEEK_END);
  buf = (char *)calloc(len + 1, sizeof(char));
  lseek(fd, 0L, SEEK_SET);
  bytes = read(fd, buf, len);
  snd_close(fd, name);
  snd_remove(name, IGNORE_CACHE);
  if (name) free(name);
  if (bytes != 0)
    res = C_string_to_Xen_string(buf);
  else res = C_string_to_Xen_symbol("read-error");
  free(buf);
  return(res);
}


static Xen g_edit_fragment(Xen uctr, Xen snd, Xen chn)
{
  #define H_edit_fragment "(" S_edit_fragment " :optional (ctr " S_current_edit_position ") snd chn): edit history entry at ctr \
associated with snd's channel chn; the returned value is a list (origin type start-sample samps)"

  chan_info *cp;
  int ctr;
  Snd_assert_channel(S_edit_fragment, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(uctr), uctr, 1, S_edit_fragment, "an integer");
  cp = get_cp(snd, chn, S_edit_fragment);
  if (!cp) return(Xen_false);

  ctr = (Xen_is_integer(uctr)) ? Xen_integer_to_C_int(uctr) : cp->edit_ctr;
  if ((ctr < cp->edit_size) && 
      (ctr >= 0))
    {
      ed_list *ed;
      ed = cp->edits[ctr];
      if (ed) 
	return(Xen_list_4(C_string_to_Xen_string(ed->origin),
			  C_string_to_Xen_string(edit_names[(int)(ed->edit_type)]),
			  C_llong_to_Xen_llong(ed->beg),
			  C_llong_to_Xen_llong(ed->len)));
    }
  Xen_error(Xen_make_error_type("no-such-edit"),
	    Xen_list_2(C_string_to_Xen_string(S_edit_fragment ": no such edit ~A"),
		       uctr));
  return(uctr);
}


static Xen g_edit_tree(Xen snd, Xen chn, Xen upos)
{
  #define H_edit_tree "(" S_edit_tree " :optional snd chn edpos): \
the edit lists '((global-pos data-num local-pos local-end scaler rmp0 rmp1 type-name)...)"
  /* internal debugging (auto-test) aid -- return complete ed list at pos */
  int i, len, pos;
  chan_info *cp;
  ed_list *eds;
  Xen res = Xen_empty_list;

  Snd_assert_channel(S_edit_tree, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_tree);
  if (!cp) return(Xen_false);
  pos = to_c_edit_position(cp, upos, S_edit_tree, 3);
  eds = cp->edits[pos];
  len = eds->size; /* fragments in this list */
  for (i = len - 1; i >= 0; i--)
    {
      ed_fragment *ed;
      mus_float_t rbeg, rend;
      ed = FRAGMENT(eds, i);
      if ((ED_RAMPS(ed)) && (ED_RAMP_LIST_SIZE(ed) > 0))
	{
	  /* this is how it used to work -- kinda dumb -- also we need the op name, not the number */
	  rbeg = ED_RAMP_START(ed, 0);
	  rend = ED_RAMP_INCR(ed, 0);
	}
      else
	{
	  rbeg = 0.0;
	  rend = 0.0;
	}
      res = Xen_cons(Xen_list_8(C_llong_to_Xen_llong(ED_GLOBAL_POSITION(ed)),
				C_int_to_Xen_integer(ED_SOUND(ed)),
				C_llong_to_Xen_llong(ED_LOCAL_POSITION(ed)),
				C_llong_to_Xen_llong(ED_LOCAL_END(ed)),
				C_double_to_Xen_real(ED_SCALER(ed)),
				C_double_to_Xen_real(rbeg),
				C_double_to_Xen_real(rend),
				C_int_to_Xen_integer(ED_TYPE(ed))),
		     res);
    }
  return(res);
}


#define S_edit_fragment_type_name "edit-fragment-type-name"
static Xen g_edit_fragment_type_name(Xen type)
{
  int typ;
  Xen_check_type(Xen_is_integer(type), type, 1, S_edit_fragment_type_name, "an int");
  typ = Xen_integer_to_C_int(type);
  if ((typ >= 0) && (typ < NUM_OPS))
    return(C_string_to_Xen_string(type_info[typ].name));
  return(Xen_false);
}



/* ---------------- samplers ---------------- */

static Xen_object_type_t sf_tag;

bool is_sampler(Xen obj) {return(Xen_c_object_is_type(obj, sf_tag));}

#define is_any_sampler(Obj) ((is_sampler(Obj)) || (is_mix_sampler(Obj)))


snd_fd *xen_to_sampler(Xen obj) {if (is_sampler(obj)) return((snd_fd *)Xen_object_ref(obj)); else return(NULL);}

#define Xen_to_C_sampler(obj) ((snd_fd *)Xen_object_ref(obj))


#if HAVE_SCHEME
static bool s7_equalp_sf(void *s1, void *s2)
{
  return(s1 == s2);
}

static s7_pointer length_sf(s7_scheme *sc, s7_pointer args)
{
  snd_fd *fd;
  fd = (snd_fd *)s7_c_object_value(s7_car(args));
  return(s7_make_integer(sc, current_samples(fd->cp)));
}
#endif


char *sampler_to_string(snd_fd *fd)
{
  char *desc;
  chan_info *cp;
#if HAVE_SCHEME
  desc = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
#else
  desc = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
#endif
  if (!fd)
    snprintf(desc, PRINT_BUFFER_SIZE, "#<sampler: null>");
  else
    {
      const char *name = NULL;
      cp = fd->cp;
      if ((fd->local_sp) && (fd->local_sp->hdr))
	name = (const char *)(((fd->local_sp)->hdr)->name);
      else
	{
	  if ((cp) && (cp->sound) && (cp->active >= CHANNEL_HAS_EDIT_LIST) && (!(fd->at_eof)))
	    {
	      if (fd->type == SAMPLER)
		{
		  name = cp->sound->short_filename;
		  if (!name)
		    switch (cp->sound->inuse)
		      {
		      case SOUND_IDLE:    name = "idle source";      break;
		      case SOUND_NORMAL:  name = "unknown source";   break;
		      case SOUND_WRAPPER: name = "wrapped source";   break;
		      case SOUND_REGION:  name = "region as source"; break;
		      case SOUND_READER:  name = "readable source";  break;
		      }
		}
	      else name = "region as source";
	    }
	}
      if (!name) name = "unknown source";
      if (fd->at_eof)
	snprintf(desc, PRINT_BUFFER_SIZE, "#<sampler: %s at eof or freed>",
		     name);
      else 
	{
	  if (cp)
	    snprintf(desc, PRINT_BUFFER_SIZE, "#<sampler: %s[%d: %d] from %" print_mus_long ", at %" print_mus_long ", %s>",
			 name, cp->chan, fd->edit_ctr, fd->initial_samp, current_location(fd), 
			 (fd->direction == READ_BACKWARD) ? "backward" : "forward");
	  else snprintf(desc, PRINT_BUFFER_SIZE, "#<sampler: %s from %" print_mus_long ", at %" print_mus_long ", %s>",
			    name, fd->initial_samp, current_location(fd),
			    (fd->direction == READ_BACKWARD) ? "backward" : "forward");
	}
    }
  return(desc);
}

#if HAVE_FORTH || HAVE_RUBY
Xen_wrap_print(snd_fd, print_sf, sampler_to_string)
#endif

#if HAVE_SCHEME
static s7_pointer g_sampler_to_string(s7_scheme *sc, s7_pointer args)
{
  char *str;
  s7_pointer result;
  str = sampler_to_string(Xen_to_C_sampler(s7_car(args)));
  result = s7_make_string(sc, str);
  if (str) free(str);
  return(result);
}
#endif

/* make-sampler can refer to any edit of any sound, user can subsequently
 *   either clobber that edit (undo, new edit), or close the sound, but forget
 *   that the reader is now invalid.  So, we keep a list of these and unconnect
 *   them by hand when an edit is pruned or a sound is closed.
 *
 * channel|sound-properties are ok in this regard because the variable stays in
 *   xen and is merely cleared, not freed at the C level.
 */

static void list_reader(snd_fd *fd)
{
  ed_list *ed;
  ed = fd->current_state;
  if (ed)
    {
      int loc = -1;
      sf_info *lst = NULL;
      if (!ed->readers)
	{
	  ed->readers = (void *)calloc(1, sizeof(sf_info));
	  lst = (sf_info *)(ed->readers);
	  lst->size = 2;
	  lst->rds = (snd_fd **)calloc(2, sizeof(snd_fd *));
	  loc = 0;
	}
      else
	{
	  int i;
	  lst = (sf_info *)(ed->readers);
	  for (i = 0; i < lst->size; i++)
	    if (!(lst->rds[i]))
	      {
		loc = i;
		break;
	      }
	  if (loc == -1)
	    {
	      loc = lst->size;
	      lst->size *= 2;
	      lst->rds = (snd_fd **)realloc(lst->rds, lst->size * sizeof(snd_fd *));
	      for (i = loc; i < lst->size; i++)
		lst->rds[i] = NULL;
	    }
	}
      lst->rds[loc] = fd;
    }
  else fprintf(stderr, "can't list reader?");
}


static void unlist_reader(snd_fd *fd)
{
  if ((fd) && 
      (!(fd->freed)) &&
#if 0
      (fd->cp) && 
      (fd->cp->active >= CHANNEL_HAS_EDIT_LIST) &&
#endif
      (fd->current_state) && 
      (fd->current_state->readers))
    {
      int i;
      ed_list *ed;
      sf_info *lst;
      ed = fd->current_state;
      lst = (sf_info *)(ed->readers);
      for (i = 0; i < lst->size; i++)
	if (fd == lst->rds[i])
	  lst->rds[i] = NULL;
    }
  /* unlist and read can be called on fully freed xen-allocated readers accessed through mixing,
   *   but this happens only if the underlying sound has been closed -- nutty cases at the end
   *   of snd-test.scm -- Snd goes on, but valgrind complains about it.  I don't think this is
   *   a bug, since we're deliberately breaking the rules, so to speak.
   */
}


static void sf_free(snd_fd *fd)
{
  if (fd) 
    {
      snd_info *sp;
      /* changed to reflect g_free_sampler 29-Oct-00 */
      unlist_reader(fd);
      sp = fd->local_sp; 
      fd->local_sp = NULL;
      free_snd_fd(fd);
      if (sp) completely_free_snd_info(sp);
    }
}


Xen_wrap_free(snd_fd, free_sf, sf_free)
/* sf_free is original, free_sf is wrapped form */


static Xen g_sampler_at_end(Xen obj) 
{
  #define H_sampler_at_end "(" S_is_sampler_at_end " obj): " PROC_TRUE " if sampler has reached the end of its data"
  Xen_check_type(is_any_sampler(obj), obj, 1, S_is_sampler_at_end, "a sampler (of any kind)");

  if (is_sampler(obj))
    {
      snd_fd *sf;
      sf = Xen_to_C_sampler(obj);
      return(C_bool_to_Xen_boolean(sf->at_eof));
    }

  if (is_mix_sampler(obj))
    return(g_mix_sampler_is_at_end(obj));

  return(Xen_false);
}


/* can sampler-position be settable? 
 *   this requires that we find the fragment that holds the new position (as at the start of init_sample_read_any_with_bufsize 6892)
 *   set the fragment bounds (ind0, ind1), call file_buffers_forward|backward
 *   also check for reader_out_of_data complications, etc
 *   so, it's simpler and just as fast to require that the user make a new reader or use random access (channel->vct)
 *   (the only thing we avoid is choose_accessor)
 */

static Xen g_sampler_position(Xen obj) 
{
  #define H_sampler_position "(" S_sampler_position " obj): current (sample-wise) location of sampler"
  Xen_check_type(is_any_sampler(obj), obj, 1, S_sampler_position, "a sampler (of any kind)");

  if (is_sampler(obj))
    {
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      if (fd->at_eof) return(Xen_integer_zero); /* -1? framples? */
      if ((fd->cp) && 
	  (fd->cp->active >= CHANNEL_HAS_EDIT_LIST) && 
	  (fd->cp->sound))
	{
	  if (fd->type == SAMPLER)
	    return(C_llong_to_Xen_llong(current_location(fd)));
	  return(C_llong_to_Xen_llong(region_current_location(fd)));
	}
    }
  if (is_mix_sampler(obj))
    return(g_mix_sampler_position(obj));

  return(Xen_integer_zero);
}


static Xen g_sampler_home(Xen obj)
{
  #define H_sampler_home "(" S_sampler_home " obj): (list sound-index chan-num) associated with a sound reader, or \
if 'obj' is a mix-sampler, the id of underlying mix"
  Xen_check_type(is_any_sampler(obj), obj, 1, S_sampler_home, "a sampler (of any kind)");

  if (is_sampler(obj))
    {
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      if ((fd->cp) && 
	  (fd->cp->active >= CHANNEL_HAS_EDIT_LIST) && 
	  (fd->cp->sound))
	{
	  if (fd->type == SAMPLER)
	    return(Xen_list_2(C_int_to_Xen_sound(fd->cp->sound->index),
			      C_int_to_Xen_integer(fd->cp->chan)));

	  return(Xen_list_2(C_int_to_Xen_region(fd->region),
			    C_int_to_Xen_integer(fd->cp->chan)));
	}
    }
  if (is_mix_sampler(obj))
    return(g_mix_sampler_home(obj));

  return(Xen_false);
}


Xen g_sampler_file_name(Xen obj)
{
  if (is_sampler(obj))
    {
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      if ((fd->cp) && 
	  (fd->cp->active >= CHANNEL_HAS_EDIT_LIST) && 
	  (fd->cp->sound))
	return(C_string_to_Xen_string(fd->cp->sound->filename));
      return(Xen_false);
    }

  return(C_string_to_Xen_string(mix_file_name(Xen_mix_to_C_int(obj))));
}


Xen g_c_make_sampler(snd_fd *fd)
{
  return(Xen_make_object(sf_tag, fd, 0, free_sf));
}


static Xen g_make_region_sampler(Xen reg, Xen samp_n, Xen chn, Xen dir1)
{
  #define H_make_region_sampler "(" S_make_region_sampler " reg :optional (start-samp 0) (chn 0) (dir 1)): \
return a reader ready to access region's channel chn data starting at start-samp going in direction dir"

  snd_fd *fd = NULL;
  int reg_n, chn_n = 0;
  mus_long_t beg;
  int direction = 1;

  Xen_check_type(xen_is_region(reg), reg, 1, S_make_region_sampler, "a region");
  Xen_check_type(Xen_is_integer_or_unbound(samp_n), samp_n, 2, S_make_region_sampler, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(chn), chn, 3, S_make_region_sampler, "an integer");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(dir1), dir1, 4, S_make_region_sampler, "an integer");
  reg_n = Xen_region_to_C_int(reg);

  if (!(region_ok(reg_n))) 
    Xen_error(Xen_make_error_type("no-such-region"),
	      Xen_list_2(C_string_to_Xen_string(S_make_region_sampler ": no such region: ~A"),
                         reg));

  if (Xen_is_integer(chn)) chn_n = Xen_integer_to_C_int(chn);
  if ((chn_n < 0) || (chn_n >= region_chans(reg_n)))
    return(snd_no_such_channel_error(S_make_region_sampler, Xen_list_1(reg), chn));

  beg = beg_to_sample(samp_n, S_make_region_sampler);
  if (Xen_is_integer(dir1)) direction = Xen_integer_to_C_int(dir1);

  if (direction == 1)
    fd = init_region_read(beg, reg_n, chn_n, READ_FORWARD);
  else
    {
      if (direction == -1)
	fd = init_region_read(beg, reg_n, chn_n, READ_BACKWARD);
      else Xen_error(Xen_make_error_type("no-such-direction"),
		     Xen_list_2(C_string_to_Xen_string(S_make_region_sampler ": bad direction: ~A"),
				dir1));
    }

  if (fd)
    {
      fd->edit_ctr = -2 - reg_n; /* can't use fd->cp because deferred case is pointer to original (not copied) data */
                                 /* has to be less than -1 because that is the "delete all readers" sign on chan close */
      fd->region = reg_n;
      fd->type = REGION_READER;
      list_reader(fd);
      return(Xen_make_object(sf_tag, fd, 0, free_sf));
    }

  return(Xen_false);
}


static Xen g_make_sampler(Xen samp_n, Xen snd, Xen chn, Xen dir1, Xen pos) /* "dir" confuses Mac OS-X Objective-C! */
{
  #define H_make_sampler "(" S_make_sampler " :optional (start-samp 0) snd chn (dir 1) edpos): \
return a reader ready to access snd's channel chn's data starting at start-samp, going in direction dir (1 = \
forward, -1 = backward), reading the version of the data indicated by edpos which defaults to the current version. \
snd can be a filename, a mix, a region, or a sound index number."

  snd_fd *fd = NULL;
  int edpos, direction = 1; /* in Scheme 1=forward, -1=backward */
  chan_info *cp;
  snd_info *loc_sp = NULL;
  mus_long_t beg;

  Xen_check_type(Xen_is_integer_or_unbound(samp_n), samp_n, 1, S_make_sampler, "an integer");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(dir1), dir1, 4, S_make_sampler, "an integer");

  if (xen_is_mix(snd))
    return(g_make_mix_sampler(snd, samp_n));

  if (xen_is_region(snd))
    return(g_make_region_sampler(snd, samp_n, chn, dir1));

  if (Xen_is_string(snd))
    {
      const char *filename;
      int chan = 0;
      Xen_check_type(Xen_is_integer_boolean_or_unbound(chn), chn, 3, S_make_sampler, "an integer or boolean");
      filename = Xen_string_to_C_string(snd);
      if (mus_file_probe(filename))
	loc_sp = make_sound_readable(filename, false);
      else return(snd_no_such_file_error(S_make_sampler, snd));
      if (Xen_is_integer(chn)) chan = Xen_integer_to_C_int(chn);
      if ((chan < 0) || 
	  (chan >= (int)loc_sp->nchans))
	{
	  completely_free_snd_info(loc_sp);
	  return(snd_no_such_channel_error(S_make_sampler, snd, chn));	
	}
      cp = loc_sp->chans[chan];
    }
  else 
    {
      Snd_assert_channel(S_make_sampler, snd, chn, 2);
      cp = get_cp(snd, chn, S_make_sampler);
      if (!cp) return(Xen_false);
    }

  edpos = to_c_edit_position(cp, pos, S_make_sampler, 5);
  if (Xen_is_integer(dir1)) direction = Xen_integer_to_C_int(dir1);
  beg = beg_to_sample(samp_n, S_make_sampler);

  if (direction == 1)
    fd = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  else
    {
      if (direction == -1)
	fd = init_sample_read_any(beg, cp, READ_BACKWARD, edpos);
      else Xen_error(Xen_make_error_type("no-such-direction"),
		     Xen_list_2(C_string_to_Xen_string(S_make_sampler ": bad direction: ~A"),
				dir1));
    }

  if (fd)
    {
      fd->local_sp = loc_sp;
      list_reader(fd);
      return(Xen_make_object(sf_tag, fd, 0, free_sf));
    }

  return(Xen_false);
}


static Xen g_is_sampler(Xen obj)
{
  #define H_is_sampler "(" S_is_sampler " obj): " PROC_TRUE " if obj is a sound sampler."

  if (is_sampler(obj))
    {
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      return(C_bool_to_Xen_boolean((fd->type == SAMPLER) || (fd->type == REGION_READER)));
    }
  if (is_mix_sampler(obj))
    return(C_string_to_Xen_symbol("mix"));

  return(Xen_false);
}


static Xen g_is_region_sampler(Xen obj)
{
  #define H_is_region_sampler "(" S_is_region_sampler " obj): " PROC_TRUE " if obj is a region sampler."
  if (is_sampler(obj))
    {
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      return(C_bool_to_Xen_boolean(fd->type == REGION_READER));
    }
  return(Xen_false);
}


static Xen g_copy_sampler(Xen obj)
{
  #define H_copy_sampler "(" S_copy_sampler " reader): return a copy of reader"
  Xen_check_type(is_any_sampler(obj), obj, 1, S_copy_sampler, "a sampler (of any kind)");

  if (is_sampler(obj))
    {
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      if ((fd->cp) && 
	  (fd->cp->active >= CHANNEL_HAS_EDIT_LIST) && 
	  (fd->cp->sound))
	{
	  if (fd->type == SAMPLER)
	    return(g_make_sampler(C_llong_to_Xen_llong(current_location(fd)),
				  C_int_to_Xen_sound(fd->cp->sound->index),
				  C_int_to_Xen_integer(fd->cp->chan),
				  C_int_to_Xen_integer((fd->direction == READ_FORWARD) ? 1 : -1), /* Scheme side is different from C side */
				  C_int_to_Xen_integer(fd->edit_ctr)));

	  return(g_make_region_sampler(C_int_to_Xen_region(fd->region),
				       C_llong_to_Xen_llong(region_current_location(fd)),
				       C_int_to_Xen_integer(fd->cp->chan),
				       C_int_to_Xen_integer((fd->direction == READ_FORWARD) ? 1 : -1)));
	}
      return(Xen_false);
    }
  if (is_mix_sampler(obj))
    return(g_copy_mix_sampler(obj));

  return(Xen_false);
}


static Xen g_next_sample(Xen obj)
{
  #define H_next_sample "(" S_next_sample " reader): next sample from reader"

  if (is_sampler(obj))
    return(C_double_to_Xen_real(protected_next_sample((snd_fd *)Xen_object_ref(obj))));
  if (is_mix_sampler(obj))
    return(C_double_to_Xen_real(protected_next_sample(xen_mix_to_snd_fd(obj))));

  Xen_check_type(false, obj, 1, S_next_sample, "a sampler");
  return(C_double_to_Xen_real(0.0));
}


mus_float_t read_sample_with_direction(void *p, int dir);
mus_float_t read_sample_with_direction(void *p, int dir)
{
  if (dir > 0)
    return(protected_next_sample((snd_fd *)p));
  return(protected_previous_sample((snd_fd *)p));
}


static Xen g_read_sample(Xen obj)
{
  #define H_read_sample "(" S_read_sample " reader): read sample from reader"
  if (is_sampler(obj))
    return(C_double_to_Xen_real(read_sample((snd_fd *)Xen_object_ref(obj))));
  if (is_mix_sampler(obj))
    return(C_double_to_Xen_real(read_sample(xen_mix_to_snd_fd(obj))));

  Xen_check_type(false, obj, 1, S_read_sample, "a sampler");
  return(C_double_to_Xen_real(0.0));
}

#define S_read_sample_with_direction "read-sample-with-direction"

static Xen g_read_sample_with_direction(Xen obj, Xen dir)
{
  #define H_read_sample_with_direction "(" S_read_sample_with_direction " reader dir): read sample from reader following dir (next/previous choice)"
  Xen_check_type(Xen_is_integer(dir), dir, 1, S_read_sample_with_direction, "an integer");
  if (is_sampler(obj))
    return(C_double_to_Xen_real(read_sample_with_direction((void *)Xen_object_ref(obj), Xen_integer_to_C_int(dir))));
  if (is_mix_sampler(obj))
    return(C_double_to_Xen_real(read_sample_with_direction((void *)xen_mix_to_snd_fd(obj), Xen_integer_to_C_int(dir))));

  Xen_check_type(false, obj, 1, S_read_sample_with_direction, "a sampler");
  return(C_double_to_Xen_real(0.0));
}


#if HAVE_SCHEME
static Xen s7_read_sample(s7_scheme *sc, Xen args)
{
  /* we can only get here if obj is already known to be a sampler */
  return(C_double_to_Xen_real(read_sample((snd_fd *)Xen_object_ref(s7_car(args)))));
}
#endif


static Xen g_previous_sample(Xen obj)
{
  #define H_previous_sample "(" S_previous_sample " reader): previous sample from reader"
  if (is_sampler(obj))
    return(C_double_to_Xen_real(protected_previous_sample((snd_fd *)Xen_object_ref(obj))));
  if (is_mix_sampler(obj))
    return(C_double_to_Xen_real(protected_previous_sample(xen_mix_to_snd_fd(obj))));

  Xen_check_type(false, obj, 1, S_previous_sample, "a sampler");
  return(C_double_to_Xen_real(0.0));
}


static Xen g_free_sampler(Xen obj)
{
  #define H_free_sampler "(" S_free_sampler " reader): free a sampler (of any kind)"
  Xen_check_type(is_any_sampler(obj), obj, 1, S_free_sampler, "a sampler");

  if (is_sampler(obj))
    {
      snd_info *sp;
      snd_fd *fd;
      fd = Xen_to_C_sampler(obj);
      unlist_reader(fd);
      sp = fd->local_sp; 
      fd->local_sp = NULL;
      free_snd_fd_almost(fd); /* this is different from sf_free! */
      if (sp) completely_free_snd_info(sp);
    }
  if (is_mix_sampler(obj))
    return(g_free_mix_sampler(obj));

  return(Xen_false);
}


static Xen g_save_edit_history(Xen filename, Xen snd, Xen chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename :optional snd chn): save snd channel's chn edit history in filename"
  FILE *fd;
  const char *name;
  char *mcf = NULL;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_save_edit_history, "a string");
  Snd_assert_channel(S_save_edit_history, snd, chn, 2);

  name = Xen_string_to_C_string(filename);
  mcf = mus_expand_filename(name);
  if (!mcf) return(Xen_false);

  fd = FOPEN(mcf, "w");
  free(mcf);

  if (fd)
    {
      if ((Xen_is_integer(chn)) && 
	  (Xen_is_integer(snd) || xen_is_sound(snd)))
	{
	  chan_info *cp;
	  cp = get_cp(snd, chn, S_save_edit_history);
	  if (!cp) return(Xen_false);
	  edit_history_to_file(fd, cp, false);
	}
      else
	{
	  int i;
	  snd_info *sp;
	  if (Xen_is_integer(snd) || xen_is_sound(snd))
	    {
	      sp = get_sp(snd);
	      if (sp)
		for (i = 0; i < (int)sp->nchans; i++)
		  edit_history_to_file(fd, sp->chans[i], false);
	    }
	  else
	    {
	      
	      for (i = 0; i < ss->max_sounds; i++)
		{
		  uint32_t j;
		  sp = ss->sounds[i];
		  if ((sp) && (sp->inuse == SOUND_NORMAL))
		    for (j = 0; j < sp->nchans; j++)
		      edit_history_to_file(fd, sp->chans[j], false);
		}
	    }
	}
      snd_fclose(fd, name);
    }
  else
    {
      Xen_error(Xen_make_error_type("cannot-save"),
		Xen_list_3(C_string_to_Xen_string(S_save_edit_history ": can't save ~S: ~A"),
			   filename,
			   C_string_to_Xen_string(snd_open_strerror())));
    }
  return(filename);
}


static Xen g_undo(Xen ed_n, Xen snd, Xen chn_n) /* opt ed_n */
{
  #define H_undo "(" S_undo " :optional (count 1) snd chn): undo 'count' edits in snd's channel chn"
  chan_info *cp;
  Xen_check_type(Xen_is_integer_or_unbound(ed_n), ed_n, 1, S_undo, "an integer");
  Snd_assert_channel(S_undo, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_undo);
  if (!cp) return(Xen_false);
  if (Xen_is_integer(ed_n))
    {
      int num;
      num = Xen_integer_to_C_int(ed_n);
      if ((num != 0) && (num < 1000000000) && (num > -1000000000))
	{
	  if (undo_edit_with_sync(cp, num))
	    return(C_int_to_Xen_integer(num));
	}
      return(Xen_integer_zero);
    }
  if (undo_edit_with_sync(cp, 1))
    return(C_int_to_Xen_integer(1));
  return(Xen_integer_zero);
}


static Xen g_redo(Xen ed_n, Xen snd, Xen chn_n) /* opt ed_n */
{
  #define H_redo "(" S_redo " :optional (count 1) snd chn): redo 'count' edits in snd's channel chn"
  chan_info *cp;
  Xen_check_type(Xen_is_integer_or_unbound(ed_n), ed_n, 1, S_redo, "an integer");
  Snd_assert_channel(S_redo, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_redo);
  if (!cp) return(Xen_false);
  if (Xen_is_integer(ed_n))
    {
      int num;
      num = Xen_integer_to_C_int(ed_n);
      if ((num != 0) && (num < 1000000000) && (num > -1000000000))
	{
	  if (redo_edit_with_sync(cp, num))
	    return(C_int_to_Xen_integer(num));
	}
      return(Xen_integer_zero);
    }
  if (redo_edit_with_sync(cp, 1))
    return(C_int_to_Xen_integer(1));
  return(Xen_integer_zero);
}


/* ---------------------------------------- AS-ONE-EDIT ---------------------------------------- */

#define INITIAL_AS_ONE_EDIT_POSITIONS_SIZE 2

void as_one_edit(chan_info *cp, int one_edit)
{
  /* it's not safe to back up during the as-one-edit function call because that function might refer back via the edpos args etc */

  bool need_backup;
  need_backup = (cp->edit_ctr > one_edit);      /* cp->edit_ctr will be changing, so save this */

  if (cp->edit_ctr >= one_edit)                 /* ">=" here because the origin needs to be set even if there were no extra edits */
    {
      if (ss->deferred_regions > 0)
	sequester_deferred_regions(cp, one_edit - 1);
      while (cp->edit_ctr > one_edit) 
	backup_edit_list_1(cp, true); /* i.e. free all the fragments */
      if (need_backup) prune_edits(cp, cp->edit_ctr + 1);
    }
}


static void init_as_one_edit(chan_info *cp) 
{
  if (cp->in_as_one_edit == 0)
    cp->previous_squelch_update = cp->squelch_update; /* preserve possible user setting across as-one-edit call */
  cp->squelch_update = true;
  if (cp->as_one_edit_positions_size == 0)
    {
      cp->as_one_edit_positions_size = INITIAL_AS_ONE_EDIT_POSITIONS_SIZE;
      cp->as_one_edit_positions = (int *)calloc(cp->as_one_edit_positions_size, sizeof(int));
    }
  else
    {
      if (cp->in_as_one_edit >= cp->as_one_edit_positions_size)
	{
	  cp->as_one_edit_positions_size += INITIAL_AS_ONE_EDIT_POSITIONS_SIZE;
	  cp->as_one_edit_positions = (int *)realloc(cp->as_one_edit_positions, cp->as_one_edit_positions_size * sizeof(int));
	}
    }
  cp->as_one_edit_positions[cp->in_as_one_edit] = cp->edit_ctr;
  cp->in_as_one_edit++;
}


static void as_one_edit_set_origin(chan_info *cp, void *origin)
{
  if (cp->as_one_edit_positions)
    {
      if ((cp->as_one_edit_positions[cp->in_as_one_edit] + 1) == cp->edit_ctr)
	{
	  ed_list *ed;
	  ed = cp->edits[cp->edit_ctr];
	  if (ed)
	    {
	      if (ed->origin) free(ed->origin);
	      ed->origin = mus_strdup((char *)origin);
	    }
	}
    }
}


static void finish_as_one_edit(chan_info *cp) 
{
  /* if a sound was opened within as-one-edit, it will have 0 here and no array */
  if ((cp->in_as_one_edit > 0) && 
      (cp->as_one_edit_positions))
    {
      cp->in_as_one_edit--;
      if (cp->in_as_one_edit < 0)
	cp->in_as_one_edit = 0;
      as_one_edit(cp, cp->as_one_edit_positions[cp->in_as_one_edit] + 1);
      if (cp->in_as_one_edit == 0)
	{
	  cp->squelch_update = cp->previous_squelch_update;
	  if (!(cp->squelch_update)) clear_status_area(cp->sound);
	  reflect_edit_history_change(cp);
	  update_graph(cp);
	}
    }
}

#if HAVE_SCHEME
static s7_pointer edit_finish;
static s7_pointer g_edit_finish(s7_scheme *sc, s7_pointer args)
{
  for_each_normal_chan(finish_as_one_edit);
  return(args);
}
#endif

static Xen g_as_one_edit(Xen proc, Xen origin)
{
  #define H_as_one_edit "(" S_as_one_edit " thunk :optional origin): evaluate thunk, collecting all edits into one from the edit history's point of view"
  Xen result = Xen_false;
  char *errmsg, *as_one_edit_origin = NULL;

  Xen_check_type((Xen_is_procedure(proc)), proc, 1, S_as_one_edit, "a procedure");
  Xen_check_type(Xen_is_string_or_unbound(origin), origin, 2, S_as_one_edit, "a string");
  errmsg = procedure_ok(proc, 0, S_as_one_edit, "edit", 1);
  if (errmsg)
    {
      Xen errstr;
      errstr = C_string_to_Xen_string(errmsg);
      free(errmsg);
      return(snd_bad_arity_error(S_as_one_edit, errstr, proc));
    }

  if (Xen_is_string(origin))
    as_one_edit_origin = mus_strdup(Xen_string_to_C_string(origin));
  else as_one_edit_origin = NULL;

  for_each_normal_chan(init_as_one_edit);

#if HAVE_SCHEME 
  result = s7_dynamic_wind(s7, xen_false, proc, edit_finish);
#else
  result = Xen_unprotected_call_with_no_args(proc);
  for_each_normal_chan(finish_as_one_edit);
#endif

  if (as_one_edit_origin)
    {
      for_each_normal_chan_with_void(as_one_edit_set_origin, (void *)as_one_edit_origin);
      free(as_one_edit_origin);
    }
  return(result);
}


static Xen g_scale_channel(Xen scl, Xen beg, Xen num, Xen snd, Xen chn, Xen edpos)
{
  #define H_scale_channel "(" S_scale_channel " scaler :optional (beg 0) (dur len) snd chn edpos): \
scale samples in the given sound/channel between beg and beg + num by scaler."

  mus_float_t scaler;
  chan_info *cp;
  mus_long_t samp;
  int pos;

  Xen_check_type(Xen_is_number(scl), scl, 1, S_scale_channel, "a number");
  Snd_assert_sample_type(S_scale_channel, beg, 2);
  Snd_assert_sample_type(S_scale_channel, num, 3);
  Snd_assert_sound(S_scale_channel, snd, 4);

  scaler = Xen_real_to_C_double(scl);
  samp = beg_to_sample(beg, S_scale_channel);
  cp = get_cp(snd, chn, S_scale_channel);
  if (!cp) return(Xen_false);
  pos = to_c_edit_position(cp, edpos, S_scale_channel, 6);

  scale_channel(cp, scaler, samp, dur_to_samples(num, samp, cp, pos, 3, S_scale_channel), pos, NOT_IN_AS_ONE_EDIT);

  return(scl);
}
			  

static Xen g_normalize_channel(Xen scl, Xen beg, Xen num, Xen snd, Xen chn, Xen edpos)
{
  #define H_normalize_channel "(" S_normalize_channel " norm :optional (beg 0) (dur len) snd chn edpos): \
scale samples in the given sound/channel between beg and beg + num to norm."

  mus_float_t norm, cur_max;
  chan_info *cp;
  mus_long_t samp, samps;
  int pos;
  char *origin = NULL;

  Xen_check_type(Xen_is_number(scl), scl, 1, S_normalize_channel, "a number");
  Snd_assert_sample_type(S_normalize_channel, beg, 2);
  Snd_assert_sample_type(S_normalize_channel, num, 3);
  Snd_assert_sound(S_normalize_channel, snd, 4);

  norm = Xen_real_to_C_double(scl);
  samp = beg_to_sample(beg, S_normalize_channel);
  cp = get_cp(snd, chn, S_normalize_channel);
  if (!cp) return(Xen_false);
  pos = to_c_edit_position(cp, edpos, S_normalize_channel, 6);
  samps = dur_to_samples(num, samp, cp, pos, 3, S_normalize_channel);

  /* in order to normalize the data, we need its current maxamp */
#if HAVE_FORTH
  if ((samp == 0) && (samps == cp->edits[pos]->samples))
    {
      cur_max = channel_maxamp(cp, pos);
      origin = mus_format("%.3f 0 " PROC_FALSE " %s", norm, S_normalize_channel);
    }
  else 
    {
      cur_max = channel_local_maxamp(cp, samp, samps, pos, NULL);
      origin = mus_format("%.3f %" print_mus_long PROC_SEP "%" print_mus_long " %s", norm, samp, samps, S_normalize_channel);
    }
#else
  if ((samp == 0) && (samps == cp->edits[pos]->samples))
    {
      cur_max = channel_maxamp(cp, pos);
      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "0" PROC_SEP PROC_FALSE, to_proc_name(S_normalize_channel), norm);
    }
  else 
    {
      cur_max = channel_local_maxamp(cp, samp, samps, pos, NULL);
      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "%" print_mus_long PROC_SEP "%" print_mus_long, to_proc_name(S_normalize_channel), norm, samp, samps);
    }
#endif

  if (cur_max != 0.0)
    scale_channel_with_origin(cp, norm / cur_max, samp, samps, pos, NOT_IN_AS_ONE_EDIT, origin);
  if (origin) free(origin);

  return(scl);
}			  


mus_float_t channel_maxamp_and_position(chan_info *cp, int edpos, mus_long_t *maxpos)
{
  /* maxamp position is not tracked in peak env because it gloms up the code, and cannot easily be saved/restored in the peak env files */
  mus_float_t val;
  int pos;
  mus_long_t locpos;

  if (edpos == AT_CURRENT_EDIT_POSITION) pos = cp->edit_ctr; else pos = edpos;

  if ((peak_env_maxamp_ok(cp, pos)) && (!maxpos))
    return(peak_env_maxamp(cp, pos));

  val = ed_maxamp(cp, pos);
  locpos = ed_maxamp_position(cp, pos);
  if (maxpos) (*maxpos) = locpos;
  if ((val >= 0.0) &&                          /* defaults to -1.0! */
      ((!maxpos) || (locpos >= 0)))
    return(val);

  val = channel_local_maxamp(cp, 0, cp->edits[pos]->samples, pos, &locpos);
  set_ed_maxamp(cp, pos, val);
  set_ed_maxamp_position(cp, pos, locpos);
  if (maxpos) (*maxpos) = locpos;

  return(val);
}


mus_float_t channel_maxamp(chan_info *cp, int edpos)
{
  return(channel_maxamp_and_position(cp, edpos, NULL));
}


mus_long_t channel_maxamp_position(chan_info *cp, int edpos)
{
  mus_long_t maxpos = 0;
  channel_maxamp_and_position(cp, edpos, &maxpos);
  return(maxpos);
}


mus_float_t channel_local_maxamp(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos, mus_long_t *maxpos)
{
  snd_fd *sf;
  mus_float_t ymax, x;
  mus_float_t *d;
  mus_long_t i, k, lim8, kend, mpos;

  sf = init_sample_read_any_with_bufsize(beg, cp, READ_FORWARD, edpos, (num > MAX_BUFFER_SIZE) ? MAX_BUFFER_SIZE : num);
  if (!sf) return(0.0);
  
  ymax = 0.0;
  mpos = -1;
  i = 0;

  while (true)
    {
      mus_long_t dur, left, offset;
      
      dur = sf->last - sf->loc + 1; /* current fragment, we're always reading forward here */
      left = num - i;
      if (dur > left) dur = left;
      offset = i - sf->loc;
      
      if (sf->runf == next_sample_value_unscaled)
	{
	  kend = sf->loc + dur;
	  lim8 = kend - 8;
	  k = sf->loc;
	  d = sf->data;

	  while (k <= lim8)
	    {
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	    }
	  while (k < kend)
	    {
	      x = fabs(d[k]); if (x > ymax) {ymax = x; mpos = k + offset;} k++;
	    }
	  i += dur;
	}
      else
	{
	  if (sf->runf == next_sample_value)
	    {
	      mus_float_t scl, lmax; /* provisional max -- we omit the scaling until the end */
	      mus_long_t lpos;       /* provisional max position */

	      scl = fabs(sf->fscaler);
	      lpos = -1;
	      lmax = 0.0;
	      kend = sf->loc + dur;
	      lim8 = kend - 8;
	      d = sf->data;

	      k = sf->loc;
	      while (k <= lim8)
		{
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		}
	      while (k < kend)
		{
		  x = fabs(d[k]); if (x > lmax) {lmax = x; lpos = k;} k++;
		}
	      if (lmax * scl > ymax)
		{
		  ymax = lmax * scl;
		  mpos = lpos + offset;
		}
	      i += dur;
	    }
	  else
	    {
	      if (sf->runf == next_ramp1)
		{
		  mus_float_t amp, incr;
		  mus_long_t j;
		  
		  amp = READER_VAL(sf, 0);
		  incr = READER_INCR(sf, 0);
		  kend = sf->loc + dur;
		  lim8 = kend - 4;
		  d = sf->data;

		  j = sf->loc;		  
		  while (j <= lim8)
		    {
		      x = fabs(amp * d[j]); if (x > ymax) {ymax = x; mpos = j + offset;} amp += incr; j++;
		      x = fabs(amp * d[j]); if (x > ymax) {ymax = x; mpos = j + offset;} amp += incr; j++;
		      x = fabs(amp * d[j]); if (x > ymax) {ymax = x; mpos = j + offset;} amp += incr; j++;
		      x = fabs(amp * d[j]); if (x > ymax) {ymax = x; mpos = j + offset;} amp += incr; j++;
		    }
		  while (j < kend) 
		    {
		      x = fabs(amp * d[j]); if (x > ymax) {ymax = x; mpos = j + offset;} amp += incr; j++;
		    }
		  READER_VAL(sf, 0) = amp;
		  i += dur;
		}
	      else
		{
		  if (sf->runf == end_sample_value)
		    break;
		  if (sf->runf == next_zero)
		    i += dur;
		  else
		    {
		      if (sf->runf == next_xramp1)
			{
			  mus_float_t off, scaler, xval, scl, incr;
			  mus_long_t j;

			  off = READER_XRAMP_OFFSET(sf, 0);
			  scl = READER_SCALER(sf);
			  scaler = READER_XRAMP_SCALER(sf, 0);
			  xval = READER_XVAL(sf, 0);
			  incr = READER_XINCR(sf, 0);
			  
			  d = sf->data;
			  kend = sf->loc + dur;
			  off *= scl;
			  scaler *= scl;

			  for (j = sf->loc; j < kend; j++) 
			    {
			      x = fabs(d[j] * (off + scaler * xval)); if (x > ymax) {ymax = x; mpos = j + offset;}
			      xval *= incr;
			    }
			  READER_XVAL(sf, 0) = xval;
			  i += dur;
			}
		      else
			{
			  left = i + dur;
			  for (; i < left; i++) 
			    {
			      x = fabs(read_sample(sf)); if (x > ymax) {ymax = x; mpos = i;}
			    }
			}
		    }
		}
	    }
	}
      if (i >= num) break;
      next_sound_1(sf);
    }
  
  /* fprintf(stderr, "use %f %" print_mus_long "\n", ymax, mpos); */
  if ((edpos == 0) &&
      (beg == 0) &&
      (num = cp->edits[0]->samples))
    mus_sound_channel_set_maxamp(cp->sound->filename, cp->chan, ymax, mpos);
  
  if (maxpos) (*maxpos) = mpos;
  free_snd_fd(sf);

  return(ymax);
}


static mus_float_t *g_floats_to_samples(Xen obj, int *size, const char *caller, int position)
{
  mus_float_t *vals = NULL;
  int i, num = 0;

  if (Xen_is_list(obj))
    {
      Xen lst;
      num = Xen_list_length(obj);
      if (num == 0) return(NULL);
      if (((*size) > 0) && (num > (*size))) 
	num = (*size);
      vals = (mus_float_t *)malloc(num * sizeof(mus_float_t));
      for (i = 0, lst = Xen_copy_arg(obj); i < num; i++, lst = Xen_cdr(lst)) 
	vals[i] = (Xen_real_to_C_double(Xen_car(lst)));
    }
  else
    {
      if (Xen_is_vector(obj))
	{
	  num = Xen_vector_length(obj); 
	  if (num == 0) return(NULL);
	  if (((*size) > 0) && (num > (*size)))
	    num = (*size);
	  vals = (mus_float_t *)malloc(num * sizeof(mus_float_t));
	  for (i = 0; i < num; i++) 
	    vals[i] = (Xen_real_to_C_double(Xen_vector_ref(obj, i)));
	}
      else
	{
	  /* this block probably can't happen anymore */
	  if (mus_is_vct(obj))
	    {
	      vct *v;
	      mus_float_t *vdata;
	      v = Xen_to_vct(obj);
	      vdata = mus_vct_data(v);
	      num = mus_vct_length(v); 
	      if (((*size) > 0) && (num > (*size)))
		num = (*size);
	      vals = (mus_float_t *)malloc(num * sizeof(mus_float_t));
	      for (i = 0; i < num; i++) 
		vals[i] = (vdata[i]);
	    }
	  else Xen_check_type(false, obj, position, caller, "a " S_vct ", vector, or list");
	}
    }
  (*size) = num;
  return(vals);
}


static Xen g_sample(Xen samp_n, Xen snd, Xen chn_n, Xen pos_n)
{
  #define H_sample "(" S_sample " samp :optional snd chn edpos): \
return sample samp in snd's channel chn (this is a slow access -- use samplers for speed)"
  mus_long_t beg;
  int pos;
  chan_info *cp;

  Xen_check_type(Xen_is_integer_or_unbound(samp_n), samp_n, 1, S_sample, "an integer");

  if (Xen_is_true(chn_n)) /* a convenience! */
    {
      Xen lst = Xen_empty_list;
      int i, loc;
      snd_info *sp;

      Snd_assert_sound(S_sample, snd, 1);

      sp = get_sp(snd);
      if (!sp) return(Xen_false);

      cp = any_selected_channel(sp);
      if (!cp) return(Xen_false);

      pos = to_c_edit_position(cp, pos_n, S_sample, 4);
      beg = beg_to_sample(samp_n, S_sample);
      loc = snd_protect(lst);
      
      for (i = 0; i < (int)sp->nchans; i++)
	{
	  if (pos > sp->chans[i]->edit_ctr)
	    lst = Xen_cons(C_double_to_Xen_real(chn_sample(beg, sp->chans[i], sp->chans[i]->edit_ctr)), lst);
	  else lst = Xen_cons(C_double_to_Xen_real(chn_sample(beg, sp->chans[i], pos)), lst);
	}

      snd_unprotect_at(loc);
      return(lst);
    }
  
  Snd_assert_channel(S_sample, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_sample);
  if (!cp) return(Xen_false);

  pos = to_c_edit_position(cp, pos_n, S_sample, 4);
  if (Xen_is_bound(samp_n))
    beg = beg_to_sample(samp_n, S_sample);
  else beg = cursor_sample(cp);

  return(C_double_to_Xen_real(chn_sample(beg, cp, pos)));
}


static Xen g_set_sample(Xen samp_n, Xen val, Xen snd, Xen chn_n, Xen edpos)
{
  /* each call consitutes a separate edit from the undo/redo point-of-view */
  chan_info *cp;
  int pos;
  char *origin;
  mus_long_t beg;
  mus_float_t fval;
  mus_float_t ival[1];

  Xen_check_type(Xen_is_integer_or_unbound(samp_n), samp_n, 1, S_set S_sample, "an integer");
  Xen_check_type(Xen_is_number(val), val, 2, S_set S_sample, "a number");
  Snd_assert_channel(S_set S_sample, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, S_set S_sample);
  if (!cp) return(Xen_false);
  pos = to_c_edit_position(cp, edpos, S_set S_sample, 5);
  if (pos > cp->edit_ctr)
    Xen_error(Xen_make_error_type("no-such-edit"),
	      Xen_list_2(C_string_to_Xen_string(S_set S_sample ": no such edit: ~A"),
			 edpos));
  if (Xen_is_bound(samp_n))
    beg = beg_to_sample(samp_n, S_set S_sample);
  else beg = cursor_sample(cp);

  fval = Xen_real_to_C_double(val);
  if ((fval == 1.0) && 
      (mus_bytes_per_sample(((cp->sound)->hdr)->sample_type) == 2))
    fval = 32767.0 / 32768.0;
  ival[0] = fval;

#if HAVE_FORTH
  origin = mus_format("%" print_mus_long " %.4f %s drop", beg, fval, "set-sample");
#else
  origin = mus_format("%s" PROC_OPEN "%" print_mus_long PROC_SEP "%.4f", to_proc_name("set-sample"), beg, fval);
#endif
  if (change_samples(beg, 1, ival, cp, origin, pos, fabs(fval)))
    update_graph(cp);
  free(origin);
  return(val);
}


#if HAVE_SCHEME
static Xen g_set_sample_reversed(s7_scheme *sc, s7_pointer args)
{
  int len;
  len = Xen_list_length(args);

  if (len == 1)
    return(g_set_sample(Xen_undefined, Xen_list_ref(args, 0), Xen_undefined, Xen_undefined, Xen_undefined));
  
  if (len == 2)
    return(g_set_sample(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_undefined, Xen_undefined, Xen_undefined));

  if (len == 3)
    return(g_set_sample(Xen_list_ref(args, 0), Xen_list_ref(args, 2), Xen_list_ref(args, 1), Xen_undefined, Xen_undefined)); 

  if (len == 4)
    return(g_set_sample(Xen_list_ref(args, 0), Xen_list_ref(args, 3), Xen_list_ref(args, 1), Xen_list_ref(args, 2), Xen_undefined)); 

  return(g_set_sample(Xen_list_ref(args, 0), Xen_list_ref(args, 4), Xen_list_ref(args, 1), Xen_list_ref(args, 2), Xen_list_ref(args, 3)));
}
#endif

file_delete_t xen_to_file_delete_t(Xen auto_delete, const char *caller)
{
  if (Xen_is_boolean(auto_delete))
    {
      if (Xen_boolean_to_C_bool(auto_delete))
	return(DELETE_ME);
      else return(DONT_DELETE_ME);
    }
  else 
    {
      if (Xen_is_integer(auto_delete))                            /* might be unbound */
	{
	  int val;
	  val = Xen_integer_to_C_int(auto_delete);
	  if ((val >= DONT_DELETE_ME) && (val <= MULTICHANNEL_DELETION_IF_FILE))
	    return((file_delete_t)val);
	  Xen_error(Xen_make_error_type("no-such-auto-delete-choice"),
		    Xen_list_3(C_string_to_Xen_string("~A: no such auto-delete option: ~A"), 
			       C_string_to_Xen_string(caller),
			       auto_delete));
	}
    }
  return(DONT_DELETE_ME);
}


static Xen g_set_samples_with_origin(Xen samp_0, Xen samps, Xen vect, Xen snd, Xen chn_n, Xen truncate, 
				     const char *edname, Xen infile_chan, Xen edpos, Xen auto_delete)
{
  #define H_set_samples "(set-" S_samples " start-samp samps data :optional snd chn truncate edname (infile-chan 0) edpos auto-delete): \
set snd's channel chn's samples starting at start-samp for samps from data (a " S_vct ", vector, or string (filename)); \
start-samp can be beyond current data end; if truncate is " PROC_TRUE " and start-samp is 0, the end of the file is set to match \
the new data's end."

  chan_info *cp;
  mus_long_t len = 0, beg;
  bool override = false;
  int pos;
  const char *caller;

  if (edname)
    caller = edname;
  else caller = "set-samples";

  Snd_assert_sample_type(caller, samp_0, 1);
  Snd_assert_sample_type(caller, samps, 2);
  Snd_assert_channel(caller, snd, chn_n, 4);
  Xen_check_type(Xen_is_boolean_or_unbound(truncate), truncate, 6, caller, "a boolean");

  cp = get_cp(snd, chn_n, caller);
  if (!cp) return(Xen_false);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(infile_chan), infile_chan, 8, caller, "an integer");

  pos = to_c_edit_position(cp, edpos, caller, 9);
  beg = beg_to_sample(samp_0, caller);
  len = dur_to_samples(samps, beg, cp, pos, 2, caller);
  if (len == 0) return(Xen_false);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(auto_delete), auto_delete, 10, caller, "a boolean or an integer");

  override = Xen_is_true(truncate);
  if (Xen_is_string(vect))
    {
      int inchan = 0;
      file_delete_t delete_file = DONT_DELETE_ME;
      mus_long_t curlen;
      const char *fname;

      curlen = cp->edits[pos]->samples;

      fname = Xen_string_to_C_string(vect);
      if (!mus_file_probe(fname))
	return(snd_no_such_file_error(caller, vect));

      if (Xen_is_integer(infile_chan)) inchan = Xen_integer_to_C_int(infile_chan);
      if ((inchan < 0) ||
	  (inchan >= mus_sound_chans(fname)))
	Xen_error(NO_SUCH_CHANNEL,
		  Xen_list_5(C_string_to_Xen_string("~A: no such channel: ~A (~S has ~A chans)"),
			     C_string_to_Xen_string(caller),
			     infile_chan,
			     vect,
			     C_int_to_Xen_integer(mus_sound_chans(fname))));

      delete_file = xen_to_file_delete_t(auto_delete, caller);
      if ((beg == 0) && 
	  ((len > curlen) || override))
	file_override_samples(len, fname, cp, inchan, delete_file, caller);
      else file_change_samples(beg, len, fname, cp, inchan, delete_file, caller, pos);
    }
  else
    {
      if (mus_is_vct(vect))
	{
	  vct *v;
	  v = Xen_to_vct(vect);
	  if (len > mus_vct_length(v)) len = mus_vct_length(v);
	  change_samples(beg, len, mus_vct_data(v), cp, caller, pos, -1.0);
	}
      else
	{
	  mus_float_t *ivals;
	  int ilen;
	  ilen = (int)len;
	  ivals = g_floats_to_samples(vect, &ilen, caller, 3);
	  if (ivals)
	    {
	      change_samples(beg, (mus_long_t)ilen, ivals, cp, caller, pos, -1.0);
	      free(ivals);
	    }
	}
    }
  update_graph(cp);
  return(vect);
}


static Xen g_set_samples(Xen samp_0, Xen samps, Xen vect, Xen snd, Xen chn_n, Xen truncate, Xen edname, Xen infile_chan, Xen edpos, Xen auto_delete)
{
  Xen_check_type(!Xen_is_bound(edname) || Xen_is_string(edname) || Xen_is_false(edname), edname, 7, "set-samples", "a string");
  return(g_set_samples_with_origin(samp_0, samps, vect, snd, chn_n, truncate,
				   (char *)((Xen_is_string(edname)) ? Xen_string_to_C_string(edname) : "set-samples"),
				   infile_chan, edpos, auto_delete));
}


static Xen g_set_samples_any(Xen args)
{
  Xen arg;
  Xen samp_0 = Xen_undefined, samps = Xen_undefined, vect = Xen_undefined;
  Xen snd = Xen_undefined, chn_n = Xen_undefined, truncate = Xen_undefined;
  Xen edname = Xen_undefined, infile_chan = Xen_undefined, edpos = Xen_undefined, auto_delete = Xen_undefined;
  arg = args;
  if (!Xen_is_null(arg))
    {
      samp_0 = Xen_car(arg); arg = Xen_cdr(arg);
      if (!Xen_is_null(arg))
	{
	  samps = Xen_car(arg); arg = Xen_cdr(arg);
	  if (!Xen_is_null(arg))
	    {
	      vect = Xen_car(arg); arg = Xen_cdr(arg);
	      if (!Xen_is_null(arg))
		{
		  snd = Xen_car(arg); arg = Xen_cdr(arg);
		  if (!Xen_is_null(arg))
		    {
		      chn_n = Xen_car(arg); arg = Xen_cdr(arg);
		      if (!Xen_is_null(arg))
			{
			  truncate = Xen_car(arg); arg = Xen_cdr(arg);
			  if (!Xen_is_null(arg))
			    {
			      edname = Xen_car(arg); arg = Xen_cdr(arg);
			      if (!Xen_is_null(arg))
				{
				  infile_chan = Xen_car(arg); arg = Xen_cdr(arg);
				  if (!Xen_is_null(arg))
				    {
				      edpos = Xen_car(arg); arg = Xen_cdr(arg);
				      if (!Xen_is_null(arg))
					auto_delete = Xen_car(arg);
				    }}}}}}}}}
  return(g_set_samples(samp_0, samps, vect, snd, chn_n, truncate, edname, infile_chan, edpos, auto_delete));
}


void check_saved_temp_file(const char *type, Xen filename, Xen date_and_length)
{
  const char *file;

  if (!Xen_is_list(date_and_length)) return; /* can this happen? */

  file = Xen_string_to_C_string(filename);
  if (mus_file_probe(file))
    {
      time_t old_time, new_time;
      mus_long_t old_bytes, new_bytes;
      old_time = (time_t)Xen_ulong_to_C_ulong(Xen_car(date_and_length));
      old_bytes = Xen_llong_to_C_llong(Xen_cadr(date_and_length));
      new_time = mus_sound_write_date(file);
      new_bytes = mus_sound_length(file);
      if ((new_time != old_time) || (new_bytes != old_bytes))
	{
	  char *buf = NULL;
	  if (old_time != new_time)
	    {
	      if (old_bytes != new_bytes)
		buf = mus_format("Saved %s temp file %s: original write date: %s, current: %s, original length: %" print_mus_long "bytes, current: %" print_mus_long,
				 type, file,
				 snd_strftime(STRFTIME_FORMAT, old_time),
				 snd_strftime(STRFTIME_FORMAT, new_time),
				 old_bytes, new_bytes);
	      else 
		buf = mus_format("Saved %s temp file %s: original write date: %s, current: %s",
				 type, file,
				 snd_strftime(STRFTIME_FORMAT, old_time),
				 snd_strftime(STRFTIME_FORMAT, new_time));
	    }
	  else buf = mus_format("Saved %s temp file %s: original length: %" print_mus_long "bytes, current: %" print_mus_long,
				 type, file,
				 old_bytes, new_bytes);
	  snd_warning_without_format(buf);
	  free(buf);
	}
    }
}


static Xen g_override_samples_with_origin(Xen filename, Xen samps, Xen snd, Xen chn_n, Xen origin, Xen date)
{
  check_saved_temp_file("sound", filename, date);
  return(g_set_samples(Xen_integer_zero, samps, filename, snd, chn_n, Xen_true, origin, Xen_integer_zero, Xen_false, Xen_false));
}


static Xen g_vct_to_channel(Xen v, Xen beg, Xen dur, Xen snd, Xen chn_n, Xen edpos, Xen origin)
{
  #define H_vct_to_channel "(" S_vct_to_channel " v :optional (beg 0) (dur len) snd chn edpos origin): \
set snd's channel chn's samples starting at beg for dur samps from " S_vct " v"
  const char *caller;
  Xen_check_type(mus_is_vct(v), v, 1, S_vct_to_channel, "a " S_vct);
  Xen_check_type(Xen_is_string_or_unbound(origin), origin, 7, S_vct_to_channel, "a string");
  if (!Xen_is_bound(beg)) beg = Xen_integer_zero;
  if (!Xen_is_bound(dur)) 
    {
      vct *v1;
      v1 = Xen_to_vct(v);
      dur = C_int_to_Xen_integer(mus_vct_length(v1));
    }
  if (!Xen_is_bound(origin))
    caller = S_vct_to_channel;
  else caller = (const char *)Xen_string_to_C_string(origin);
  return(g_set_samples_with_origin(beg, dur, v, snd, chn_n, Xen_false, caller, Xen_false, edpos, Xen_undefined));
}


vct *samples_to_vct(mus_long_t beg, mus_long_t len, chan_info *cp, int pos, mus_float_t *buf, snd_fd *reader)
{
  /* if reader, beg, cp, and pos are ignored */
  snd_fd *sf;
  vct *v = NULL;
  mus_float_t **d;
  mus_float_t *fvals;

  if (!buf)
    {
      v = mus_vct_make(len);
      fvals = mus_vct_data(v);
    }
  else
    {
      v = mus_vct_wrap(len, buf);
      fvals = buf;
    }

  if ((pos == 0) &&
      (beg + len <= cp->edits[0]->samples) &&
      (d = mus_sound_saved_data(cp->sound->filename)))
    {
      mus_float_t *dc;
      dc = d[cp->chan];
      memcpy((void *)fvals, (void *)(dc + beg), len * sizeof(mus_float_t));
      return(v);
    }

  if (!reader)
    sf = init_sample_read_any_with_bufsize(beg, cp, READ_FORWARD, pos, len);
  else sf = reader;

  if (sf)
    {
      mus_long_t i;
      i = 0;
      while (true)
	{
	  mus_long_t dur, left;
	  dur = sf->last - sf->loc + 1; /* current fragment, we're always reading forward here */
	  left = len - i;
	  if (dur > left) dur = left;

	  if (sf->runf == next_sample_value_unscaled)
	    {
	      mus_copy_floats(fvals + i, sf->data + sf->loc, dur);
	      i += dur;
	    }
	  else
	    {
	      if (sf->runf == next_sample_value)
		{
		  mus_float_t scl;
		  scl = sf->fscaler;
		  mus_copy_floats(fvals + i, sf->data + sf->loc, dur);
		  left = i + dur;
		  for (; i < left; i++) fvals[i] *= scl;
		}
	      else
		{
		  if (sf->runf == next_ramp1)
		    {
		      mus_float_t amp, incr;
		      mus_long_t j;
		      amp = READER_VAL(sf, 0);
		      incr = READER_INCR(sf, 0);
		      left = i + dur;
		      for (j = sf->loc; i < left; i++, j++) 
			{
			  fvals[i] = amp * sf->data[j];
			  amp += incr;
			}
		      READER_VAL(sf, 0) = amp;
		    }
		  else
		    {
		      if (sf->runf == end_sample_value)
			break;
		      if (sf->runf == next_zero)
			i += dur;
		      else
			{
			  if (sf->runf == next_xramp1)
			    {
			      mus_float_t offset, scaler, xval, scl, incr;
			      mus_long_t j;
			      offset = READER_XRAMP_OFFSET(sf, 0);
			      scl = READER_SCALER(sf);
			      scaler = READER_XRAMP_SCALER(sf, 0);
			      xval = READER_XVAL(sf, 0);
			      incr = READER_XINCR(sf, 0);

			      left = i + dur;
			      offset *= scl;
			      scaler *= scl;
			      for (j = sf->loc; i < left; i++, j++) 
				{
				  fvals[i] = sf->data[j] * (offset + scaler * xval);
				  xval *= incr;
				}
			      READER_XVAL(sf, 0) = xval;
			    }
			  else
			    {
			      left = i + dur;
			      for (; i < left; i++) fvals[i] = read_sample(sf);
			    }
			}
		    }
		}
	    }
	  if (i >= len) break;
	  next_sound_1(sf);
	}
      if (!reader)
	free_snd_fd(sf);
    }
  return(v);
}


vct *samples_to_vct_with_reader(mus_long_t len, mus_float_t *buf, snd_fd *reader)
{
  return(samples_to_vct(0, len, NULL, -1, buf, reader));
}


static Xen samples_to_vct_1(Xen samp_0, Xen samps, Xen snd, Xen chn_n, Xen edpos, const char *caller)
{
  chan_info *cp;
  mus_long_t len, beg;
  int pos;

  Xen_check_type(Xen_is_integer_or_unbound(samp_0) || Xen_is_false(samp_0), samp_0, 1, caller, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(samps) || Xen_is_false(samps), samps, 2, caller, "an integer");
  Snd_assert_channel(caller, snd, chn_n, 3);

  cp = get_cp(snd, chn_n, caller);
  if (!cp) return(Xen_false);

  pos = to_c_edit_position(cp, edpos, caller, 6);
  beg = beg_to_sample(samp_0, caller);
  len = dur_to_samples(samps, beg, cp, pos, 2, caller);
  if (len == 0) return(Xen_false); /* empty file (channel) possibility */

  return(vct_to_xen(samples_to_vct(beg, len, cp, pos, NULL, NULL)));
}


static Xen g_channel_to_vct(Xen samp_0, Xen samps, Xen snd, Xen chn_n, Xen edpos)
{
  #define H_channel_to_vct "(" S_channel_to_vct " :optional (beg 0) (dur len) snd chn edpos): \
return a " S_vct " containing snd channel chn's data starting at beg for dur samps"

  return(samples_to_vct_1(samp_0, samps, snd, chn_n, edpos, S_channel_to_vct));
}


static Xen g_samples(Xen samp_0, Xen samps, Xen snd, Xen chn_n, Xen edpos)
{
  #define H_samples "(" S_samples " :optional (start-samp 0) (samps len) snd chn edpos): \
return a " S_vct " containing snd channel chn's samples starting at start-samp for samps samples; edpos is the edit \
history position to read (defaults to current position). snd can be a filename, a mix, a region, or a sound index number."

  chan_info *cp;
  mus_long_t beg, len;
  int pos;

  Xen_check_type(Xen_is_integer_or_unbound(samp_0) || Xen_is_false(samp_0), samp_0, 1, S_samples, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(samps) || Xen_is_false(samps), samps, 2, S_samples, "an integer");
  beg = beg_to_sample(samp_0, S_samples);

  /* -------- a file -------- */
  if (Xen_is_string(snd))
    {
      snd_info *loc_sp = NULL;
      int chan = 0, chans;
      const char *filename;
      mus_float_t *fvals;
      vct *v;

      Xen_check_type(Xen_is_integer_boolean_or_unbound(chn_n), chn_n, 4, S_samples, "an integer");
      if (Xen_is_integer(chn_n)) chan = Xen_integer_to_C_int(chn_n);
      if (chan < 0) return(snd_no_such_channel_error(S_samples, snd, chn_n));	

      filename = Xen_string_to_C_string(snd);
      if (!mus_file_probe(filename))
	return(snd_no_such_file_error(S_make_sampler, snd));

      if (Xen_is_integer(samps))
	len = Xen_integer_to_C_int(samps);
      else len = mus_sound_framples(filename);
      if (len <= 0) return(Xen_false);

      chans = mus_sound_chans(filename);
      if (chan >= chans)
	return(snd_no_such_channel_error(S_samples, snd, chn_n));	

      loc_sp = make_sound_readable(filename, false);
      /* cp = loc_sp->chans[chan]; */
      v = mus_vct_make(len);
      fvals = mus_vct_data(v);
      mus_file_to_array(filename, chan, beg, len, fvals);

      completely_free_snd_info(loc_sp);
      return(vct_to_xen(v));
    }

  /* -------- a mix -------- */
  if (xen_is_mix(snd))
    return(g_mix_to_vct(snd, samp_0, samps));


  /* -------- a region -------- */
  if (xen_is_region(snd))
    return(g_region_to_vct(snd, samp_0, samps, chn_n, Xen_false));


  /* -------- a sound -------- */
  Snd_assert_channel(S_samples, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, S_samples);
  if (!cp) return(Xen_false);

  pos = to_c_edit_position(cp, edpos, S_samples, 5);
  len = dur_to_samples(samps, beg, cp, pos, 2, S_samples);
  if (len == 0) return(Xen_false);

  return(vct_to_xen(samples_to_vct(beg, len, cp, pos, NULL, NULL)));
}


#if HAVE_SCHEME
static Xen g_set_samples_reversed(s7_scheme *sc, s7_pointer args)
{
  int len;
  len = Xen_list_length(args);
  switch (len)
    {
    case 3:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 2), 
			   Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined, 
			   Xen_undefined, Xen_undefined));
    case 4:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 3), Xen_list_ref(args, 2), 
			   Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined));
    case 5:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 4), 
			   Xen_list_ref(args, 2), Xen_list_ref(args, 3), 
			   Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined));
    case 6:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 5), 
			   Xen_list_ref(args, 2), Xen_list_ref(args, 3), Xen_list_ref(args, 4), 
			   Xen_undefined, Xen_undefined, Xen_undefined, Xen_undefined));
    case 7:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 6), 
			   Xen_list_ref(args, 2), Xen_list_ref(args, 3), Xen_list_ref(args, 4), 
			   Xen_list_ref(args, 5), 
			   Xen_undefined, Xen_undefined, Xen_undefined));
    case 8:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 7),
			   Xen_list_ref(args, 2), Xen_list_ref(args, 3), Xen_list_ref(args, 4), 
			   Xen_list_ref(args, 5), Xen_list_ref(args, 6),
			   Xen_undefined, Xen_undefined));
    case 9:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 8),
			   Xen_list_ref(args, 2), Xen_list_ref(args, 3), Xen_list_ref(args, 4),
			   Xen_list_ref(args, 5), Xen_list_ref(args, 6), Xen_list_ref(args, 7), 
			   Xen_undefined));
    default:
      return(g_set_samples(Xen_list_ref(args, 0), Xen_list_ref(args, 1), Xen_list_ref(args, 9),
			   Xen_list_ref(args, 2), Xen_list_ref(args, 3), Xen_list_ref(args, 4),
			   Xen_list_ref(args, 5), Xen_list_ref(args, 6), Xen_list_ref(args, 7),
			   Xen_list_ref(args, 8)));
    }
}
#endif


static Xen g_change_samples_with_origin(Xen samp_0, Xen samps, Xen origin, Xen vect, Xen snd, Xen chn_n, Xen edpos, Xen date)
{
  chan_info *cp;
  int pos;
  mus_long_t beg, len = 0;

  Xen_check_type(Xen_is_llong(samp_0), samp_0, 1, S_change_samples_with_origin, "an integer");
  Xen_check_type(Xen_is_llong(samps), samps, 2, S_change_samples_with_origin, "an integer");
  Xen_check_type(Xen_is_string(origin), origin, 3, S_change_samples_with_origin, "a string");
  Xen_check_type(Xen_is_string(vect), vect, 4, S_change_samples_with_origin, "a filename");

  Snd_assert_channel(S_change_samples_with_origin, snd, chn_n, 5);
  cp = get_cp(snd, chn_n, S_change_samples_with_origin);
  if (!cp) return(Xen_false);

  beg = beg_to_sample(samp_0, S_change_samples_with_origin);
  if (Xen_is_llong(samps)) len = Xen_llong_to_C_llong(samps);
  if (len <= 0) return(Xen_false);

  pos = to_c_edit_position(cp, edpos, S_change_samples_with_origin, 7);
  check_saved_temp_file("sound", vect, date);

  file_change_samples(beg, len,
		      Xen_string_to_C_string(vect),
		      cp, 0, DONT_DELETE_ME,
		      Xen_string_to_C_string(origin), 
		      pos);
  update_graph(cp);
  return(vect);
}


static Xen g_insert_sound(Xen file, Xen ubeg, Xen file_chn, Xen snd, Xen chn_n, Xen edpos, Xen auto_delete)
{
  #if HAVE_SCHEME
    #define insert_sound_example "(" S_insert_sound " \"oboe.snd\" 1000)"
  #endif
  #if HAVE_RUBY
    #define insert_sound_example "insert_sound(\"oboe.snd\", 1000)"
  #endif
  #if HAVE_FORTH
    #define insert_sound_example "\"oboe.snd\" 1000 insert-sound"
  #endif

  #define H_insert_sound "(" S_insert_sound " file :optional (beg 0) (file-chan 0) snd chn edpos auto-delete): \
insert channel file-chan of file (or all chans if file-chan is not given) into snd's channel chn at beg or at the cursor \
position.\n  " insert_sound_example "\ninserts all of oboe.snd starting at sample 1000."

  chan_info *cp;
  static char *filename = NULL;
  int nc;
  char *origin;
  file_delete_t delete_file = DONT_DELETE_ME;
  mus_long_t beg = 0, len;

  Xen_check_type(Xen_is_string(file), file, 1, S_insert_sound, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(ubeg), ubeg, 2, S_insert_sound, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(file_chn), file_chn, 3, S_insert_sound, "an integer");

  Snd_assert_channel(S_insert_sound, snd, chn_n, 4);
  cp = get_cp(snd, chn_n, S_insert_sound);
  if (!cp) return(Xen_false);

  Xen_check_type(Xen_is_integer_boolean_or_unbound(auto_delete), auto_delete, 7, S_insert_sound, "a boolean or an integer");
  delete_file = xen_to_file_delete_t(auto_delete, S_insert_sound);

  if (filename) free(filename);
  filename = mus_expand_filename(Xen_string_to_C_string(file));
  if (!mus_file_probe(filename))
    return(snd_no_such_file_error(S_insert_sound, file));

  nc = mus_sound_chans(filename);
  if (nc <= 0)
    {
      Xen_error(BAD_HEADER,
		Xen_list_3(C_string_to_Xen_string(S_insert_sound ": chans <= 0? (~S has ~D chans)"),
			   file,
			   C_int_to_Xen_integer(nc)));
    }

  len = mus_sound_framples(filename);
  if (len <= 0) return(C_llong_to_Xen_llong(len));

  if (Xen_is_integer(ubeg))
    beg = beg_to_sample(ubeg, S_insert_sound);
  else beg = cursor_sample(cp);

  if (Xen_is_integer(file_chn))
    {
      int fchn;
      fchn = Xen_integer_to_C_int(file_chn);
      if (fchn < 0)
	Xen_error(NO_SUCH_CHANNEL, Xen_list_2(C_string_to_Xen_string(S_insert_sound ": file channel: ~D"), file_chn));

      if (fchn < nc)
	{
#if HAVE_FORTH
	  origin = mus_format("\"%s\" %" print_mus_long " %d %s drop", filename, beg, fchn, S_insert_sound);
#else
	  origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP "%" print_mus_long PROC_SEP "%d", to_proc_name(S_insert_sound), filename, beg, fchn);
#endif
	  if (file_insert_samples(beg, len, filename, cp, fchn, delete_file, origin,
				  to_c_edit_position(cp, edpos, S_insert_sound, 6)))
	    update_graph(cp);
	  free(origin);
	  return(C_llong_to_Xen_llong(len));
	}
      else return(snd_no_such_channel_error(S_insert_sound, file, file_chn));	
    }
  else
    {
      int i;
      snd_info *sp;
      sp = cp->sound;
      if ((int)sp->nchans < nc) nc = sp->nchans;
      for (i = 0; i < nc; i++)
	{
#if HAVE_FORTH
	  origin = mus_format("\"%s\" %" print_mus_long " %d %s drop", filename, beg, i, S_insert_sound);
#else
	  origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP "%" print_mus_long PROC_SEP "%d", to_proc_name(S_insert_sound), filename, beg, i);
#endif
	  if (file_insert_samples(beg, len, filename, sp->chans[i], i, delete_file, origin,
				  /* this edit_position cannot be optimized out -- each channel may have
				   *   a different edit history, but edpos might be -1 throughout etc.
				   */
				  to_c_edit_position(sp->chans[i], edpos, S_insert_sound, 6)))
	    update_graph(sp->chans[i]);
	  free(origin);
	}
      return(C_llong_to_Xen_llong(len));
    }
  return(Xen_false); /* not reached */
}


static Xen g_insert_sample(Xen samp_n, Xen val, Xen snd, Xen chn_n, Xen edpos)
{
  #define H_insert_sample "(" S_insert_sample " sample value :optional snd chn edpos): insert 'value' at 'sample' in snd's channel chn"
  chan_info *cp;
  char *origin;
  int pos;
  mus_long_t beg;
  mus_float_t fval;
  mus_float_t ival[1];

  Xen_check_type(Xen_is_integer(samp_n), samp_n, 1, S_insert_sample, "an integer");
  Xen_check_type(Xen_is_number(val), val, 2, S_insert_sample, "a number");
  Snd_assert_channel(S_insert_sample, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, S_insert_sample);
  if (!cp) return(Xen_false);
  beg = beg_to_sample(samp_n, S_insert_sample);
  pos = to_c_edit_position(cp, edpos, S_insert_sample, 5);
  fval = Xen_real_to_C_double(val);
  ival[0] = fval;
#if HAVE_FORTH
  origin = mus_format("%" print_mus_long " %.4f %s drop", beg, fval, S_insert_sample);
#else
  origin = mus_format("%s" PROC_OPEN "%" print_mus_long PROC_SEP "%.4f", to_proc_name(S_insert_sample), beg, fval);
#endif
  if (insert_samples(beg, 1, ival, cp, origin, pos))
    update_graph(cp); 
  free(origin);
  return(val);
}


static Xen g_insert_samples(Xen samp, Xen samps, Xen vect, Xen snd, Xen chn_n, Xen edpos, Xen auto_delete, Xen caller)
{
  #define H_insert_samples "(" S_insert_samples " start-samp samps data :optional snd chn edpos auto-delete origin): \
insert data (either a " S_vct ", a list of samples, or a filename) into snd's channel chn starting at 'start-samp' for 'samps' samples"

  chan_info *cp;
  int pos;
  char *origin = NULL;
  file_delete_t delete_file = DONT_DELETE_ME;
  mus_long_t beg, len = 0;

  Xen_check_type(Xen_is_integer(samp), samp, 1, S_insert_samples, "an integer");
  Xen_check_type(Xen_is_integer(samps), samps, 2, S_insert_samples, "an integer");
  Snd_assert_channel(S_insert_samples, snd, chn_n, 4);
  Xen_check_type(Xen_is_string_or_unbound(caller), caller, 8, S_insert_samples, "a string");
  cp = get_cp(snd, chn_n, S_insert_samples);
  if (!cp) return(Xen_false);

  beg = beg_to_sample(samp, S_insert_samples);
  len = Xen_llong_to_C_llong(samps);
  if (len <= 0) return(samps);
  pos = to_c_edit_position(cp, edpos, S_insert_samples, 6);

  Xen_check_type(Xen_is_integer_boolean_or_unbound(auto_delete), auto_delete, 7, S_insert_samples, "a boolean or an integer");
  delete_file = xen_to_file_delete_t(auto_delete, S_insert_samples);

  if (Xen_is_string(caller))
    origin = mus_strdup(Xen_string_to_C_string(caller));
  if (Xen_is_string(vect))
    {
      char *filename;
      filename = mus_expand_filename(Xen_string_to_C_string(vect));
      if (!mus_file_probe(filename))
	{
	  free(filename);
	  return(snd_no_such_file_error(S_insert_samples, vect));
	}
      if (mus_sound_framples(filename) <= 0) return(Xen_integer_zero);
#if HAVE_FORTH
      if (!origin) origin = mus_format("%" print_mus_long PROC_SEP "%" print_mus_long " \"%s\" %s drop", beg, len, filename, S_insert_samples);
#else
      if (!origin) origin = mus_format("%s" PROC_OPEN "%" print_mus_long PROC_SEP "%" print_mus_long PROC_SEP "\"%s\"", to_proc_name(S_insert_samples), beg, len, filename);
#endif
      file_insert_samples(beg, len, filename, cp, 0, delete_file, origin, pos);
      if (filename) free(filename);
    }
  else
    {
      if (mus_is_vct(vect))
	{
	  vct *v;
	  v = Xen_to_vct(vect);
	  if (len > mus_vct_length(v)) len = mus_vct_length(v);
	  if (!origin) origin = mus_strdup(to_proc_name(S_insert_samples));
	  insert_samples(beg, len, mus_vct_data(v), cp, origin, pos);
	}
      else
	{
	  int ilen;
	  mus_float_t *ivals;
	  ilen = (int)len;
	  ivals = g_floats_to_samples(vect, &ilen, S_insert_samples, 3);
	  if (ivals)
	    {
	      if (!origin) origin = mus_strdup(to_proc_name(S_insert_samples));
	      insert_samples(beg, (mus_long_t)ilen, ivals, cp, origin, pos);
	      free(ivals);
	    }
	}
    }
  if (origin) free(origin);
  update_graph(cp);
  return(C_llong_to_Xen_llong(len));
}


static Xen g_insert_samples_with_origin(Xen samp, Xen samps, Xen origin, Xen vect, Xen snd, Xen chn_n, Xen edpos, Xen date)
{
  chan_info *cp;
  int pos;
  mus_long_t beg, len;

  Xen_check_type(Xen_is_integer(samp), samp, 1, S_insert_samples_with_origin, "an integer");
  Xen_check_type(Xen_is_integer(samps), samps, 2, S_insert_samples_with_origin, "an integer");
  Xen_check_type(Xen_is_string(origin), origin, 3, S_insert_samples_with_origin, "a string");
  Xen_check_type(Xen_is_string(vect), vect, 4, S_insert_samples_with_origin, "a filename");

  Snd_assert_channel(S_insert_samples_with_origin, snd, chn_n, 5);
  cp = get_cp(snd, chn_n, S_insert_samples_with_origin);
  if (!cp) return(Xen_false);

  beg = beg_to_sample(samp, S_insert_samples_with_origin);
  len = Xen_llong_to_C_llong(samps);
  if (len <= 0) return(samps);

  pos = to_c_edit_position(cp, edpos, S_insert_samples_with_origin, 7);
  check_saved_temp_file("sound", vect, date);
  file_insert_samples(beg, len, Xen_string_to_C_string(vect), cp, 0, DONT_DELETE_ME, Xen_string_to_C_string(origin), pos);
  update_graph(cp);
  return(C_llong_to_Xen_llong(len));
}


static Xen g_delete_sample(Xen samp_n, Xen snd, Xen chn_n, Xen edpos)
{
  #define H_delete_sample "(" S_delete_sample " samp :optional snd chn edpos): delete sample 'samp' from snd's channel chn"
  chan_info *cp;
  mus_long_t samp;
  int pos;

  Xen_check_type(Xen_is_integer(samp_n), samp_n, 1, S_delete_sample, "an integer");
  Snd_assert_channel(S_delete_sample, snd, chn_n, 2);

  cp = get_cp(snd, chn_n, S_delete_sample);
  if (!cp) return(Xen_false);
  samp = beg_to_sample(samp_n, S_delete_sample);
  pos = to_c_edit_position(cp, edpos, S_delete_sample, 4);
  if ((samp < 0) || (samp > cp->edits[pos]->samples))
    Xen_error(Xen_make_error_type("no-such-sample"),
	      Xen_list_2(C_string_to_Xen_string(S_delete_sample ": no such sample: ~A"),
			 samp_n));

  if (delete_samples(samp, 1, cp, pos))
    update_graph(cp);

  return(samp_n);
}


static Xen g_delete_samples(Xen samp_n, Xen samps, Xen snd, Xen chn_n, Xen edpos)
{
  #define H_delete_samples "(" S_delete_samples " start-samp samps :optional snd chn edpos): \
delete 'samps' samples from snd's channel chn starting at 'start-samp'"

  chan_info *cp;
  int pos;
  mus_long_t samp, len;

  Xen_check_type(Xen_is_integer(samp_n), samp_n, 1, S_delete_samples, "an integer");
  Xen_check_type(Xen_is_integer(samps), samps, 2, S_delete_samples, "an integer");

  Snd_assert_channel(S_delete_samples, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, S_delete_samples);
  if (!cp) return(Xen_false);

  pos = to_c_edit_position(cp, edpos, S_delete_samples, 6);
  samp = beg_to_sample(samp_n, S_delete_samples);
  len = Xen_llong_to_C_llong(samps);
  if (len <= 0) return(Xen_false);

  if (delete_samples(samp, len, cp, pos))
    update_graph(cp);

  return(samp_n);
}



/* -------- re-direct CLM input (ina etc) to use samplers -------- */

#include "clm2xen.h"

static int snd_to_sample_tag = 0;

typedef struct {
  mus_any_class *core;
  snd_info *sp;
  snd_fd **sfs;
  int chans;
  mus_long_t *samps;
} snd_to_sample;

static bool is_snd_to_sample(mus_any *ptr) {return(mus_type(ptr) == snd_to_sample_tag);}

static bool snd_to_sample_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static int snd_to_sample_channels(mus_any *ptr) {return(((snd_to_sample *)ptr)->chans);}

static mus_long_t snd_to_sample_location(mus_any *ptr) {return(((snd_to_sample *)ptr)->samps[0]);}

static char *snd_to_sample_file_name(mus_any *ptr) {return(((snd_to_sample *)ptr)->sp->filename);}

static mus_long_t snd_to_sample_length(mus_any *ptr) {return(current_samples(((snd_to_sample *)ptr)->sp->chans[0]));}

static void snd_to_sample_free(mus_any *ptr)
{
  snd_to_sample *spl = (snd_to_sample *)ptr;
  if (spl->sfs)
    {
      int i;
      for (i = 0; i < spl->chans; i++)
	spl->sfs[i] = free_snd_fd(spl->sfs[i]);
      free(spl->sfs);
      spl->sfs = NULL;
    }
  if (spl->samps)
    {
      free(spl->samps);
      spl->samps = NULL;
    }
  free(spl);
}


static char *snd_to_sample_describe(mus_any *ptr)
{
  char *snd_to_sample_buf = NULL;
  int i, len = PRINT_BUFFER_SIZE;
  snd_to_sample *spl = (snd_to_sample *)ptr;
  char *temp;
  if (spl->sfs)
    {
      len += spl->chans * 8;
      for (i = 0; i < spl->chans; i++)
	if (spl->sfs[i])
	  {
	    temp = sampler_to_string(spl->sfs[i]);
	    if (temp)
	      {
		len += mus_strlen(temp);
		free(temp);
	      }
	  }
    }
  snd_to_sample_buf = (char *)calloc(len, sizeof(char));
  snprintf(snd_to_sample_buf, len, "%s reading %s (%d chan%s) at %" print_mus_long ":[", 
	       mus_name(ptr),
	       spl->sp->short_filename, 
	       spl->chans, 
	       (spl->chans > 1) ? "s" : "",
	       spl->samps[0]);
  if (spl->sfs)
    {
      for (i = 0; i < spl->chans; i++)
	if (spl->sfs[i])
	  {
	    temp = sampler_to_string(spl->sfs[i]);
	    if (temp)
	      {
		strcat(snd_to_sample_buf, temp);
		free(temp);
	      }
	    if (i < spl->chans - 1) 
	      strcat(snd_to_sample_buf, ", ");
	    else strcat(snd_to_sample_buf, "]");
	  }
	else strcat(snd_to_sample_buf, "nil, ");
    }
  else strcat(snd_to_sample_buf, "no readers]");
  return(snd_to_sample_buf);
}


static mus_float_t snd_to_sample_read(mus_any *ptr, mus_long_t frample, int chan) 
{
  snd_to_sample *spl = (snd_to_sample *)ptr;
  mus_long_t diff, i;
  if (!(spl->sfs)) 
    spl->sfs = (snd_fd **)calloc(spl->chans, sizeof(snd_fd *));
  if (!(spl->sfs[chan])) 
    {
      spl->sfs[chan] = init_sample_read(frample, spl->sp->chans[chan], READ_FORWARD);
      spl->samps[chan] = frample;
      return(next_sample_value(spl->sfs[chan]));
    }
  diff = frample - spl->samps[chan];
  if (diff == 1)
    {
      spl->samps[chan]++;
      return(next_sample_value(spl->sfs[chan]));
    }
  if (diff > 1)
    {
      for (i = 1; i < diff; i++) next_sample_value(spl->sfs[chan]); /* just push pointer forward */
      spl->samps[chan] = frample;
      return(next_sample_value(spl->sfs[chan]));
    }
  diff = -diff;
  for (i = 0; i <= diff; i++) previous_sample_value(spl->sfs[chan]); /* just push pointer backward (one too far) */
  spl->samps[chan] = frample;
  return(next_sample_value(spl->sfs[chan])); /* always end up going forward (for simpler code) */
}


static mus_any_class *snd_to_sample_class;

static mus_any *make_snd_to_sample(snd_info *sp)
{
  snd_to_sample *gen;
  gen = (snd_to_sample *)calloc(1, sizeof(snd_to_sample));
  gen->core = snd_to_sample_class;
  gen->chans = sp->nchans;
  gen->sp = sp;
  gen->samps = (mus_long_t *)calloc(sp->nchans, sizeof(mus_long_t));
  gen->sfs = NULL;           /* created as needed */
  return((mus_any *)gen);
}


static Xen g_is_snd_to_sample(Xen os) 
{
  #define H_is_snd_to_sample "(" S_is_snd_to_sample " gen): " PROC_TRUE " if gen is an " S_snd_to_sample " generator"
  return(C_bool_to_Xen_boolean((mus_is_xen(os)) && 
			       (is_snd_to_sample(Xen_to_mus_any(os)))));
}


static Xen g_snd_to_sample(Xen os, Xen frample, Xen chan)
{
  #define H_snd_to_sample "(" S_snd_to_sample " gen frample chan): input sample (via snd->sample gen) at frample in channel chan"
  Xen_check_type((mus_is_xen(os)) && (is_snd_to_sample(Xen_to_mus_any(os))), os, 1, S_snd_to_sample, "a " S_snd_to_sample " gen");
  Xen_check_type(Xen_is_llong(frample), frample, 2, S_snd_to_sample, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(chan), chan, 3, S_snd_to_sample, "an integer");
  return(C_double_to_Xen_real(snd_to_sample_read((mus_any *)Xen_to_mus_any(os), 
					    Xen_llong_to_C_llong(frample), 
					    (Xen_is_integer(chan)) ? Xen_integer_to_C_int(chan) : 0)));
}


static Xen g_make_snd_to_sample(Xen snd)
{
  #define H_make_snd_to_sample "(" S_make_snd_to_sample " snd): return a new " S_snd_to_sample " (Snd to CLM input) generator"
  mus_any *ge;
  snd_info *sp;

  Snd_assert_sound(S_make_snd_to_sample, snd, 1);

  sp = get_sp(snd);
  if (!sp)
    return(snd_no_such_sound_error(S_make_snd_to_sample, snd));

  ge = make_snd_to_sample(sp);
  if (ge)
    return(mus_xen_to_object(mus_any_to_mus_xen(ge)));
  return(Xen_false);
}


static Xen g_edit_list_to_function(Xen snd, Xen chn, Xen start, Xen end)
{
  #define H_edit_list_to_function "(" S_edit_list_to_function " :optional snd chn start end): function encapsulating edits"
  chan_info *cp;
  char *funcstr = NULL;
  Xen func;
  int start_pos = 1, end_pos = -1;

  Snd_assert_channel(S_edit_list_to_function, snd, chn, 1);
  cp = get_cp(snd, chn, S_edit_list_to_function);
  if (!cp) return(Xen_false);

  Xen_check_type(Xen_is_integer_or_unbound(start), start, 3, S_edit_list_to_function, "an integer");  
  Xen_check_type(Xen_is_integer_or_unbound(end), end, 4, S_edit_list_to_function, "an integer");  

  if (Xen_is_integer(start)) start_pos = Xen_integer_to_C_int(start);
  if (start_pos < 0) /* is 0 legal here? */
    Xen_out_of_range_error(S_edit_list_to_function, 3, start, "a non-negative integer");
  if (Xen_is_integer(end)) end_pos = Xen_integer_to_C_int(end);

  funcstr = edit_list_to_function(cp, start_pos, end_pos);
  func = Xen_eval_C_string(funcstr);

#if HAVE_RUBY
  rb_set_property(rb_obj_id(func), C_string_to_Xen_symbol("proc_source"), C_string_to_Xen_string(funcstr));
#endif

#if HAVE_FORTH
  fth_proc_source_set(func, C_string_to_Xen_string(funcstr));
#endif

  free(funcstr);
  return(func);
}

#if HAVE_SCHEME
static s7_double next_sample_dv(void *o)
{
  snd_fd *fd = (snd_fd *)o;
  return(protected_next_sample(fd));
}

static s7_double read_sample_dv(void *o)
{
  snd_fd *fd = (snd_fd *)o;
  return(read_sample(fd));
}

static s7_double next_mix_sample_dv(void *o)
{
  snd_fd *fd;
  fd = (snd_fd *)mf_to_snd_fd(o);
  return(protected_next_sample(fd));
}

static s7_double read_mix_sample_dv(void *o)
{
  snd_fd *fd;
  fd = (snd_fd *)mf_to_snd_fd(o);
  return(read_sample(fd));
}
#endif


Xen_wrap_5_optional_args(g_make_sampler_w, g_make_sampler)
Xen_wrap_4_optional_args(g_make_region_sampler_w, g_make_region_sampler)
Xen_wrap_1_arg(g_next_sample_w, g_next_sample)
Xen_wrap_1_arg(g_read_sample_w, g_read_sample)
Xen_wrap_2_args(g_read_sample_with_direction_w, g_read_sample_with_direction)
Xen_wrap_1_arg(g_previous_sample_w, g_previous_sample)
Xen_wrap_1_arg(g_free_sampler_w, g_free_sampler)
Xen_wrap_1_arg(g_sampler_home_w, g_sampler_home)
Xen_wrap_1_arg(g_sampler_position_w, g_sampler_position)
Xen_wrap_1_arg(g_is_sampler_w, g_is_sampler)
Xen_wrap_1_arg(g_is_region_sampler_w, g_is_region_sampler)
Xen_wrap_1_arg(g_sampler_at_end_w, g_sampler_at_end)
Xen_wrap_1_arg(g_copy_sampler_w, g_copy_sampler)
Xen_wrap_3_optional_args(g_save_edit_history_w, g_save_edit_history)
Xen_wrap_3_optional_args(g_edit_fragment_w, g_edit_fragment)
Xen_wrap_3_optional_args(g_undo_w, g_undo)
Xen_wrap_3_optional_args(g_redo_w, g_redo)
Xen_wrap_2_optional_args(g_as_one_edit_w, g_as_one_edit)
Xen_wrap_3_optional_args(g_display_edits_w, g_display_edits)
Xen_wrap_3_optional_args(g_edit_tree_w, g_edit_tree)
Xen_wrap_4_optional_args(g_delete_sample_w, g_delete_sample)
Xen_wrap_5_optional_args(g_delete_samples_w, g_delete_samples)
Xen_wrap_5_optional_args(g_insert_sample_w, g_insert_sample)
Xen_wrap_8_optional_args(g_insert_samples_w, g_insert_samples)
Xen_wrap_7_optional_args(g_vct_to_channel_w, g_vct_to_channel)
Xen_wrap_5_optional_args(g_channel_to_vct_w, g_channel_to_vct)
Xen_wrap_7_optional_args(g_insert_sound_w, g_insert_sound)
Xen_wrap_6_optional_args(g_scale_channel_w, g_scale_channel)
Xen_wrap_6_optional_args(g_normalize_channel_w, g_normalize_channel)
Xen_wrap_8_optional_args(g_change_samples_with_origin_w, g_change_samples_with_origin)
Xen_wrap_8_optional_args(g_insert_samples_with_origin_w, g_insert_samples_with_origin)
Xen_wrap_6_optional_args(g_override_samples_with_origin_w, g_override_samples_with_origin)
Xen_wrap_4_optional_args(g_sample_w, g_sample)
#if HAVE_SCHEME
#define g_set_sample_w g_set_sample_reversed
#define g_set_samples_w g_set_samples_reversed
Xen_wrap_5_optional_args(orig_g_set_sample_w, g_set_sample)
#else
Xen_wrap_5_optional_args(g_set_sample_w, g_set_sample)
Xen_wrap_any_args(g_set_samples_w, g_set_samples_any)
#endif
Xen_wrap_any_args(orig_g_set_samples_w, g_set_samples_any)
Xen_wrap_5_optional_args(g_samples_w, g_samples)
Xen_wrap_1_arg(g_is_snd_to_sample_w, g_is_snd_to_sample)
Xen_wrap_3_optional_args(g_snd_to_sample_w, g_snd_to_sample)
Xen_wrap_1_optional_arg(g_make_snd_to_sample_w, g_make_snd_to_sample)
Xen_wrap_4_optional_args(g_edit_list_to_function_w, g_edit_list_to_function)
Xen_wrap_1_arg(g_edit_fragment_type_name_w, g_edit_fragment_type_name)

void g_init_edits(void)
{
#if HAVE_SCHEME
  s7_pointer b, i, p, t, f, fnc, fv, r, s, smp, x, rg, pl_fx;
  b = s7_make_symbol(s7, "boolean?");
  i = s7_make_symbol(s7, "integer?");
  p = s7_make_symbol(s7, "pair?");
  f = s7_make_symbol(s7, "float?");
  fv = s7_make_symbol(s7, "float-vector?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  rg = s7_make_symbol(s7, "region?");
  fnc = s7_make_symbol(s7, "procedure?");
  smp = s7_make_symbol(s7, "sampler?");
  x = s7_make_signature(s7, 2, smp, s7_make_symbol(s7, "mix-sampler?"));
  t = s7_t(s7);
  /* pl_fx = s7_make_signature(s7, 2, f, x); */
  pl_fx = s7_make_signature(s7, 2, f, smp);

  sf_tag = s7_make_c_type(s7, "<sampler>");
  s7_c_type_set_free(s7, sf_tag, free_sf);
  s7_c_type_set_equal(s7, sf_tag, s7_equalp_sf);
  s7_c_type_set_ref(s7, sf_tag, s7_read_sample);
  s7_c_type_set_length(s7, sf_tag, length_sf);
  s7_c_type_set_to_string(s7, sf_tag, g_sampler_to_string);
#else
  sf_tag = Xen_make_object_type("Sampler", sizeof(snd_fd));
#endif

#if HAVE_RUBY
  rb_define_method(sf_tag, "to_s", Xen_procedure_cast print_sf, 0);
  rb_define_method(sf_tag, "call", Xen_procedure_cast g_read_sample, 0);
#endif

#if HAVE_FORTH
  fth_set_object_inspect(sf_tag, print_sf);
  fth_set_object_free(sf_tag, free_sf);
  fth_set_object_apply(sf_tag, Xen_procedure_cast g_read_sample, 0, 0, 0);
#endif

  Xen_define_constant(S_current_edit_position,   AT_CURRENT_EDIT_POSITION,  "represents the current edit history list position (-1)");

  Xen_define_typed_procedure(S_make_sampler,        g_make_sampler_w,        0, 5, 0, H_make_sampler,     s7_make_signature(s7, 6, smp, i, t, t, t, t));
  Xen_define_typed_procedure(S_make_region_sampler, g_make_region_sampler_w, 1, 3, 0, H_make_region_sampler, s7_make_signature(s7, 5, smp, rg, i, i, i));
  Xen_define_typed_procedure(S_read_sample,         g_read_sample_w,         1, 0, 0, H_read_sample,      pl_fx);
  Xen_define_typed_procedure(S_read_sample_with_direction, g_read_sample_with_direction_w, 2, 0, 0, H_read_sample_with_direction, s7_make_signature(s7, 3, f, x, i));
  Xen_define_typed_procedure(S_read_region_sample,  g_read_sample_w,         1, 0, 0, H_read_sample,      pl_fx);
  Xen_define_typed_procedure(S_next_sample,         g_next_sample_w,         1, 0, 0, H_next_sample,      pl_fx);
  Xen_define_typed_procedure(S_previous_sample,     g_previous_sample_w,     1, 0, 0, H_previous_sample,  pl_fx);
  Xen_define_typed_procedure(S_free_sampler,        g_free_sampler_w,        1, 0, 0, H_free_sampler,     s7_make_signature(s7, 2, b, x));
  Xen_define_typed_procedure(S_sampler_home,        g_sampler_home_w,        1, 0, 0, H_sampler_home,     s7_make_signature(s7, 2, t, x));
  Xen_define_typed_procedure(S_is_sampler,          g_is_sampler_w,          1, 0, 0, H_is_sampler,       s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_is_region_sampler,   g_is_region_sampler_w,   1, 0, 0, H_is_region_sampler, s7_make_signature(s7, 2, b, x));
  Xen_define_typed_procedure(S_is_sampler_at_end,   g_sampler_at_end_w,      1, 0, 0, H_sampler_at_end,   s7_make_signature(s7, 2, b, x));
  Xen_define_typed_procedure(S_sampler_position,    g_sampler_position_w,    1, 0, 0, H_sampler_position, s7_make_signature(s7, 2, i, x));
  Xen_define_typed_procedure(S_copy_sampler,        g_copy_sampler_w,        1, 0, 0, H_copy_sampler,     s7_make_signature(s7, 2, x, x));

  Xen_define_typed_procedure(S_save_edit_history,   g_save_edit_history_w,   1, 2, 0, H_save_edit_history, s7_make_signature(s7, 4, s, s, t, t));
  Xen_define_typed_procedure(S_edit_fragment,       g_edit_fragment_w,       0, 3, 0, H_edit_fragment,    s7_make_signature(s7, 4, p, i, t, t));
  Xen_define_typed_procedure(S_edit_fragment_type_name,g_edit_fragment_type_name_w, 1, 0, 0, "internal testing function", s7_make_signature(s7, 2, s, i));

  Xen_define_typed_procedure(S_undo,                g_undo_w,                0, 3, 0, H_undo,             s7_make_signature(s7, 4, i, i, t, t));
#if HAVE_RUBY
  Xen_define_procedure("undo_edit",                 g_undo_w,                0, 3, 0, H_undo);
#endif 
  Xen_define_typed_procedure(S_redo,                g_redo_w,                0, 3, 0, H_redo,             s7_make_signature(s7, 4, i, i, t, t));
  Xen_define_typed_procedure(S_as_one_edit,         g_as_one_edit_w,         1, 1, 0, H_as_one_edit,      s7_make_signature(s7, 3, t, fnc, s));
  Xen_define_typed_procedure(S_display_edits,       g_display_edits_w,       0, 3, 0, H_display_edits,    s7_make_circular_signature(s7, 1, 2, s, t));
  Xen_define_typed_procedure(S_edit_tree,           g_edit_tree_w,           0, 3, 0, H_edit_tree,        s7_make_circular_signature(s7, 1, 2, p, t));

  Xen_define_typed_procedure(S_delete_sample,       g_delete_sample_w,       1, 3, 0, H_delete_sample,    s7_make_circular_signature(s7, 2, 3, i, i, t));
  Xen_define_typed_procedure(S_delete_samples,      g_delete_samples_w,      2, 3, 0, H_delete_samples,   s7_make_circular_signature(s7, 3, 4, i, i, i, t));
  Xen_define_typed_procedure(S_insert_sample,       g_insert_sample_w,       2, 3, 0, H_insert_sample,    s7_make_circular_signature(s7, 2, 3, i, r, t));
  Xen_define_typed_procedure(S_insert_samples,      g_insert_samples_w,      3, 5, 0, H_insert_samples,   s7_make_signature(s7, 9, i, i, i, t, t, t, t, b, s));
  Xen_define_typed_procedure(S_vct_to_channel,      g_vct_to_channel_w,      1, 6, 0, H_vct_to_channel,   s7_make_circular_signature(s7, 0, 1, t));
  Xen_define_typed_procedure(S_channel_to_vct,      g_channel_to_vct_w,      0, 5, 0, H_channel_to_vct,   s7_make_circular_signature(s7, 3, 4, fv, i, t, t));
  Xen_define_typed_procedure(S_insert_sound,        g_insert_sound_w,        1, 6, 0, H_insert_sound,     s7_make_circular_signature(s7, 4, 5, i, s, i, i, t));
  Xen_define_typed_procedure(S_scale_channel,       g_scale_channel_w,       1, 5, 0, H_scale_channel,    s7_make_circular_signature(s7, 2, 3, r, r, t));
  Xen_define_typed_procedure(S_normalize_channel,   g_normalize_channel_w,   1, 5, 0, H_normalize_channel,s7_make_circular_signature(s7, 2, 3, r, r, t));

  Xen_define_typed_procedure(S_change_samples_with_origin,   g_change_samples_with_origin_w,   7, 1, 0, "internal function used in save-state",
			     s7_make_circular_signature(s7, 0, 1, t));
  Xen_define_typed_procedure(S_insert_samples_with_origin,   g_insert_samples_with_origin_w,   7, 1, 0, "internal function used in save-state",
			     s7_make_circular_signature(s7, 0, 1, t));
  Xen_define_typed_procedure(S_override_samples_with_origin, g_override_samples_with_origin_w, 5, 1, 0, "internal function used in save-state",
			     s7_make_circular_signature(s7, 0, 1, t));

  Xen_define_typed_dilambda(S_sample,  g_sample_w,  H_sample,  S_set S_sample,  g_set_sample_w,  0, 4, 1, 4,
			    s7_make_signature(s7, 5, r, i, t, t, t), s7_make_signature(s7, 6, r, i, t, t, t, r));
  Xen_define_typed_dilambda(S_samples, g_samples_w, H_samples, S_set S_samples, g_set_samples_w, 0, 5, 3, 7,
			    s7_make_circular_signature(s7, 2, 3, t, i, t), s7_make_circular_signature(s7, 0, 1, t));

#if HAVE_SCHEME
  Xen_define_typed_procedure("set-sample",          orig_g_set_sample_w,     2, 3, 0, H_sample,      s7_make_circular_signature(s7, 3, 4, r, i, r, t));
#endif
  Xen_define_typed_procedure("set-samples",         orig_g_set_samples_w,    0, 0, 1, H_set_samples, s7_make_circular_signature(s7, 3, 4, r, i, r, t));

  Xen_define_typed_procedure(S_is_snd_to_sample,    g_is_snd_to_sample_w,    1, 0, 0, H_is_snd_to_sample,   s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_make_snd_to_sample,  g_make_snd_to_sample_w,  0, 1, 0, H_make_snd_to_sample, s7_make_signature(s7, 2, t, t));
  Xen_define_typed_procedure(S_snd_to_sample,       g_snd_to_sample_w,       2, 1, 0, H_snd_to_sample,      s7_make_signature(s7, 4, f, t, i, i));
  Xen_define_unsafe_typed_procedure(S_edit_list_to_function, g_edit_list_to_function_w, 0, 4, 0, H_edit_list_to_function, s7_make_signature(s7, 5, t, t, t, i, i)); 
  /* not safe because it calls eval-string */

  #define H_save_hook S_save_hook " (snd name): called each time a file is about to be saved. \
If it returns " PROC_TRUE ", the file is not saved.  'name' is " PROC_FALSE " unless the file is being saved under a new name (as in sound-save-as)."

  save_hook = Xen_define_hook(S_save_hook, "(make-hook 'snd 'name)", 2, H_save_hook); 

  #define H_save_state_hook S_save_state_hook " (temp-filename): called each time the " S_save_state " \
mechanism is about to create a new temporary file to save some edit history sample values. \
temp-filename is the current file. \
If the hook returns a string, it is treated as the new temp filename.  This hook provides a way to \
keep track of which files are in a given saved state batch, and a way to rename or redirect those files."

  save_state_hook = Xen_define_hook(S_save_state_hook, "(make-hook 'name)", 1, H_save_state_hook); 


  snd_to_sample_tag = mus_make_generator_type();
  snd_to_sample_class = mus_make_generator(snd_to_sample_tag, (char *)S_snd_to_sample, snd_to_sample_free, snd_to_sample_describe, snd_to_sample_equalp);
  mus_generator_set_length(snd_to_sample_class, snd_to_sample_length);
  mus_generator_set_channels(snd_to_sample_class, snd_to_sample_channels);
  mus_generator_set_read_sample(snd_to_sample_class, snd_to_sample_read);
  mus_generator_set_file_name(snd_to_sample_class, snd_to_sample_file_name);
  mus_generator_set_location(snd_to_sample_class, snd_to_sample_location);
  mus_generator_set_extended_type(snd_to_sample_class, MUS_INPUT);

#if HAVE_SCHEME
  {
    s7_pointer f;
    edit_finish = s7_make_function(s7, "(finish-as-one-edit)", g_edit_finish, 0, 0, false, "");

    f = s7_name_to_value(s7, "next-sample");
    s7_set_d_v_function(f, next_sample_dv);

    f = s7_name_to_value(s7, "read-sample");
    s7_set_d_v_function(f, read_sample_dv);

    f = s7_name_to_value(s7, "next-mix-sample");
    s7_set_d_v_function(f, next_mix_sample_dv);

    f = s7_name_to_value(s7, "read-mix-sample");
    s7_set_d_v_function(f, read_mix_sample_dv);
  }
#endif

#if DEBUG_EDIT_TABLES
  /* consistency checks for the accessor state table */
  init_hit_entries();
  check_type_info_entry(ED_SIMPLE, 0, 0, 0, false);
  check_type_info_entry(ED_ZERO, 0, 0, 0, true);
  report_unhit_entries();
#endif
}

/* from Anders:

How to have the trees displayed in a meaningful manner,
especially related to common-music type work?  Maybe the edits
could be given optional names?  ie. "inversed", "prolongue-1",
"scale-1" etc. - and a more specialised graphing system put
together.

If all this is based on closures, doing "Select-All", should it
write out a temp-file of the state as now?  And how to import it
into some arbitrary place in the tree again?  Would pasting it in
be analogous to take a certain stage of the edit-history and
append the rest?
*/
