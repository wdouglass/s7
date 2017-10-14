#include "snd.h"

static void mix_set_file_name(int id, int chans, const char *name);


static bool mix_vct_untagged(vct *v, chan_info *cp, mus_long_t beg, const char *origin)
{
  mus_float_t *data, *vdata;
  int len;
  snd_fd *sf;
  bool result = false;

  len = mus_vct_length(v);
  vdata = mus_vct_data(v);
  data = (mus_float_t *)calloc(len, sizeof(mus_float_t)); /* don't add into v->data! */

  sf = init_sample_read(beg, cp, READ_FORWARD);
  samples_to_vct_with_reader(len, data, sf);
  mus_add_floats(data, vdata, len);
  free_snd_fd(sf);

  result = change_samples(beg, len, data, cp, origin, cp->edit_ctr, -1.0); /* cp->edit_ctr since mix-vct has no edpos arg, similarly mix */
  if (result) update_graph(cp);

  free(data);
  return(result);
}


mus_float_t next_sample_value_unscaled(snd_fd *sf);
mus_float_t next_sound(snd_fd *sf);

static bool mix_file_untagged(const char *filename, int in_chan, chan_info *cp, mus_long_t beg, mus_long_t num, file_delete_t auto_delete, const char *origin)
{
  file_info *ihdr, *ohdr;
  char *ofile;
  int ofd, ifd = -1;
  io_error_t io_err = IO_NO_ERROR;
  snd_fd *sf = NULL;
  mus_long_t i, j, size, in_chans;
  int err = 0;
  mus_float_t **data;
  mus_float_t *chandata;

  if ((num <= 0) || (!is_editable(cp)))
    return(false);

  ihdr = make_file_info(filename, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (!ihdr) return(false);

  if (in_chan >= ihdr->chans)
    {
      free_file_info(ihdr);
      return(false);
    }

  ofile = snd_tempnam();
  ohdr = make_temp_header(ofile, snd_srate(cp->sound), 1, 0, (char *)origin);
  ofd = open_temp_file(ofile, 1, ohdr, &io_err);
  if (ofd == -1) 
    {
      free_file_info(ihdr);
      free_file_info(ohdr);
      snd_error("%s mix temp file %s: %s", 
		(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		ofile, 
		snd_open_strerror()); 
      return(false);
    }

  if ((disk_has_space(num * mus_bytes_per_sample(ohdr->sample_type), ofile)) != DISK_SPACE_OK)
    return(false);

  sf = init_sample_read(beg, cp, READ_FORWARD);
  if (sf) ifd = snd_open_read(filename);
  if ((!sf) || (ifd < 0))
    {
      if (sf) free_snd_fd(sf);
      free_file_info(ihdr);
      free_file_info(ohdr);
      mus_file_close(ofd);
      snd_remove(ofile, REMOVE_FROM_CACHE);
      free(ofile);
      return(false);
    }

  if (beg < 0) beg = 0;
  in_chans = ihdr->chans;

  snd_file_open_descriptors(ifd, filename,
			    ihdr->sample_type,
			    ihdr->data_location,
			    ihdr->chans,
			    ihdr->type);
  during_open(ifd, filename, SND_MIX_FILE);
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;

  data = (mus_float_t **)calloc(in_chans, sizeof(mus_float_t *));
  data[in_chan] = (mus_float_t *)calloc(size, sizeof(mus_float_t));
  chandata = data[in_chan];
  
  sampler_set_safe(sf, num);
  lseek(ofd, ohdr->data_location, SEEK_SET);
  lseek(ifd, ihdr->data_location, SEEK_SET);
  mus_file_read_chans(ifd, 0, size, in_chans, data, data);

  for (i = 0; i < num; i += size)
    {
      mus_long_t kdur;
      kdur = num - i;
      if (kdur > size) kdur = size;
      if (sf->runf == next_sample_value_unscaled)
	{
#if (!WITH_VECTORIZE)
	  for (j = 0; j < kdur; j++)
	    chandata[j] += (sf->loc > sf->last) ? next_sound(sf) : sf->data[sf->loc++];
#else
	  for (j = 0; j < kdur; )
	    {
	      mus_long_t ksize;
	      ksize = sf->last - sf->loc + 1;
	      if (ksize == 1)
		chandata[j++] = next_sound(sf);
	      else 
		{
		  if (j + ksize > kdur) ksize = kdur - j;
		  mus_copy_floats((mus_float_t *)(chandata + j), (mus_float_t *)(sf->data + sf->loc), ksize);
		  j += ksize;
		  if (j < kdur)
		    chandata[j++] = next_sound(sf);
		}
	    }
#endif
	}
      else
	{
	  for (j = 0; j < kdur; j++)
	    chandata[j] += read_sample(sf);
	}
      err = mus_file_write(ofd, 0, kdur - 1, 1, &chandata);
      mus_file_read_chans(ifd, i, size, in_chans, data, data);
      if (err != MUS_NO_ERROR) break;
    }
  
  close_temp_file(ofile, ofd, ohdr->type, num * mus_bytes_per_sample(ohdr->sample_type));
  mus_file_close(ifd);
  free_snd_fd(sf);
  free(data[in_chan]);
  free(data);
  free_file_info(ihdr);
  free_file_info(ohdr);
  file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, origin, cp->edit_ctr);
  if (ofile) free(ofile);

  if (auto_delete == DELETE_ME)
    snd_remove(filename, REMOVE_FROM_CACHE);

  update_graph(cp);
  return(true);
}


int mix_complete_file_at_cursor(snd_info *sp, const char *filename)
{
  if ((sp) && (filename) && (*filename))
    {
      chan_info *cp;
      int err = 0;
      char *fullname;
      fullname = mus_expand_filename(filename);
      cp = any_selected_channel(sp);
      err = mix_complete_file(sp, cursor_sample(cp), fullname, with_mix_tags(ss), DONT_DELETE_ME, MIX_FOLLOWS_SYNC, NULL);
      if (err == MIX_FILE_NO_FILE) 
	snd_error("can't mix file: %s, %s", filename, snd_io_strerror());
      else
	{
	  if (err == MIX_FILE_NO_MIX) 
	    snd_error("no data to mix in %s", filename);
	}
      if (fullname) free(fullname);
      return(err);
    }
  return(MIX_FILE_NO_SP);
}


void drag_and_drop_mix_at_x_y(int data, const char *filename, int x, int y)
{
  int chn, snd;
  chn = unpack_channel(data);
  snd = unpack_sound(data);
  if ((snd >= 0) &&
      (snd < ss->max_sounds) && 
      (snd_ok(ss->sounds[snd])) &&
      (chn >= 0) &&
      (chn < (int)(ss->sounds[snd]->nchans)) &&
      (mus_file_probe(filename)))
    {
      snd_info *sp = NULL;
      chan_info *cp;
      mus_long_t sample;
      char *fullname = NULL;
      sp = ss->sounds[snd];
      cp = sp->chans[chn];
      if ((sp->nchans > 1) && 
	  (sp->channel_style == CHANNELS_COMBINED))
	{
	  cp = which_channel(sp, y);
	  chn = cp->chan;
	}
      select_channel(sp, chn);
      sample = snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)(snd_srate(sp)));
      if (sample < 0) sample = 0;
      fullname = mus_expand_filename(filename);
      mix_complete_file(sp, sample, fullname, with_mix_tags(ss), DONT_DELETE_ME, MIX_FOLLOWS_SYNC, NULL);
      if (fullname) free(fullname);
    }
}


static int mix_infos_ctr = 0;

int mix_complete_file(snd_info *sp, mus_long_t beg, const char *fullname, bool with_tag, file_delete_t auto_delete, mix_sync_t all_chans, int *out_chans)
{
  chan_info *cp;
  chan_info **cps = NULL;
  int chans, id = MIX_FILE_NO_MIX, old_sync;
  mus_long_t len;
  sync_info *si = NULL;

  len = mus_sound_framples(fullname);
  if (len < 0) return(MIX_FILE_NO_FILE);
  if (len == 0) return(MIX_FILE_NO_MIX);

  cp = any_selected_channel(sp);
  old_sync = sp->sync;
  if ((old_sync == 0) && 
      (all_chans == MIX_SETS_SYNC_LOCALLY))
    {
      sp->sync = ss->sound_sync_max + 1;
      ss->sound_sync_max++;
    }
  if (sp->sync != 0)
    {
      si = snd_sync(sp->sync); 
      cps = si->cps;
      chans = si->chans;
    }
  else
    {
      cps = (chan_info **)calloc(1, sizeof(chan_info *));
      cps[0] = cp;
      chans = 1;
    }

  id = mix_file(beg, len, chans, cps, fullname, auto_delete, NULL, with_tag, 0);
  if (si) 
    free_sync_info(si); 
  else 
    {
      if (cps) 
	free(cps);
    }
  sp->sync = old_sync;

  if (mix_exists(id)) /* bugfix thanks to Tito Latini, 18-Jan-17 */
    {
      if (chans > 1)
        {
          chans = mix_infos_ctr - id;
          if (chans > 1)
            {
              int i, sync = GET_NEW_SYNC;
              for (i = 0; i < chans; i++)
                sync = mix_set_sync_from_id(id + i, sync);
            }
        }
      if (out_chans) (*out_chans) = chans;
      mix_set_file_name(id, chans, fullname);
    }
  
  return(id);
}


static const char *b2s(bool val) 
{
  return((val) ? PROC_TRUE : PROC_FALSE);  /* cast needed by g++ > 3.4 */
} 


static char *tagged_mix_to_string(const char *mixinfile, mus_long_t beg, int file_channel, bool delete_file)
{
#if HAVE_FORTH
  return(mus_format("\"%s\" %" PRId64 " %d snd chn %s %s %s to -mix-%d", mixinfile, beg, file_channel, b2s(true), b2s(delete_file), S_mix, mix_infos_ctr));
#endif
#if HAVE_SCHEME
  return(mus_format("(varlet -env- '-mix-%d (%s \"%s\" %" PRId64 " %d snd chn %s %s))", mix_infos_ctr, S_mix, mixinfile, beg, file_channel, b2s(true), b2s(delete_file)));
#endif
#if HAVE_RUBY
  return(mus_format("_mix_%d = %s(\"%s\", %" PRId64 ", %d, snd, chn, %s, %s)", mix_infos_ctr, to_proc_name(S_mix), mixinfile, beg, file_channel, b2s(true), b2s(delete_file)));
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  return(NULL);
#endif
}


static char *untagged_mix_to_string(const char *mixinfile, mus_long_t beg, int file_channel, bool delete_file)
{
#if HAVE_FORTH
  return(mus_format("\"%s\" %" PRId64 " %d snd chn %s %s %s", mixinfile, beg, file_channel, b2s(false), b2s(delete_file), S_mix));
#endif
#if HAVE_SCHEME
  return(mus_format("(%s \"%s\" %" PRId64 " %d snd chn %s %s)", S_mix, mixinfile, beg, file_channel, b2s(false), b2s(delete_file)));
#endif
#if HAVE_RUBY
  return(mus_format("%s(\"%s\", %" PRId64 ", %d, snd, chn, %s, %s)", to_proc_name(S_mix), mixinfile, beg, file_channel, b2s(false), b2s(delete_file)));
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  return(NULL);
#endif
}


int mix_file(mus_long_t beg, mus_long_t num, int chans, chan_info **cps, const char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int start_chan)
{
  /* used in mix_selection and paste(mix)_region, and in mix_complete_file */

  int i, id = MIX_FILE_NO_MIX, in_chans;
#if HAVE_SCHEME
  int is_mix_selection = ((origin) && (strncmp(origin, "-mix-selection-", 15) == 0));
#endif
  char *new_origin = NULL;

  in_chans =  mus_sound_chans(mixinfile);
  if (chans > in_chans) chans = in_chans;

  if (temp == MULTICHANNEL_DELETION)
    remember_temp(mixinfile, in_chans);

  for (i = 0; i < chans; i++) 
    {
      chan_info *cp;
      cp = cps[i];

#if HAVE_SCHEME
      if (is_mix_selection)
        new_origin = mus_format("(varlet -env- '-mix-%d (%s %d %d))", mix_infos_ctr, origin,
                                start_chan + i, start_chan);
#endif
      if ((!with_tag) ||
	  (!virtual_mix_ok(cp, cp->edit_ctr)))
	{
	  /* not a virtual mix */
	  if (!origin)
	    new_origin = untagged_mix_to_string(mixinfile, beg, start_chan + i, temp != DONT_DELETE_ME);
	  else
	    {
	      if (!new_origin)
	        new_origin = mus_strdup(origin);
	    }
	  mix_file_untagged(mixinfile, i + start_chan, cp, beg, num, temp, new_origin);
	}
      else 
	{
	  /* virtual mix */
	  int cur_id;
	  if (!origin)
	    new_origin = tagged_mix_to_string(mixinfile, beg, start_chan + i, temp != DONT_DELETE_ME);
	  else
	    {
	      if (!new_origin)
	        new_origin = mus_strdup(origin);
	    }
	  cur_id = mix_file_with_tag(cp, mixinfile, i + start_chan, beg, temp, new_origin);
	  if (id == MIX_FILE_NO_MIX) id = cur_id;
	}

      if (new_origin) 
	{
	  free(new_origin); 
	  new_origin = NULL;
	}
    }

  for (i = 0; i < chans; i++) 
    update_graph(cps[i]);
  return(id);
}



static mix_state *free_mix_state(mix_state *ms)
{
  if (ms)
    {
      if (ms->amp_env) 
	ms->amp_env = free_env(ms->amp_env);
      free(ms);
    }
  return(NULL);
}


static mix_state *make_mix_state(int id, int index, mus_long_t beg, mus_long_t len)
{
  mix_state *ms;
  ms = (mix_state *)calloc(1, sizeof(mix_state));
  ms->mix_id = id;
  ms->scaler = 1.0;
  ms->speed = 1.0;
  ms->beg = beg;
  ms->len = len;
  ms->amp_env = NULL;
  ms->index = index;
  return(ms);
}


mix_state *copy_mix_state(mix_state *old_ms)
{
  mix_state *ms;
  ms = (mix_state *)calloc(1, sizeof(mix_state));
  ms->mix_id = old_ms->mix_id;
  ms->scaler = old_ms->scaler;
  ms->speed = old_ms->speed;
  ms->beg = old_ms->beg;
  ms->len = old_ms->len;
  ms->index = old_ms->index;
  ms->amp_env = copy_env(old_ms->amp_env); /* this is the amp env (not the peak env) */
  return(ms);
}


/* this is the edit list header for the list of mix states: ed_list->mixes (void* in snd-1.h) */
typedef struct {
  int size;
  mix_state **list;
} mix_list;

void free_ed_mixes(void *ptr)
{
  if (ptr)
    {
      int i;
      mix_list *mxl = (mix_list *)ptr;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  mxl->list[i] = free_mix_state(mxl->list[i]);
      free(mxl->list);
      free(mxl);
    }
}


void add_ed_mix(ed_list *ed, mix_state *ms)
{
  mix_list *mxl;
  int loc = -1;
  if (!(ed->mixes))
    {
      ed->mixes = (mix_list *)calloc(1, sizeof(mix_list));
      mxl = (mix_list *)(ed->mixes);
      mxl->size = 2;
      mxl->list = (mix_state **)calloc(mxl->size, sizeof(mix_state *));
      loc = 0;
    }
  else
    {
      int i;
      mxl = (mix_list *)(ed->mixes);
      for (i = 0; i < mxl->size; i++)
	if (!mxl->list[i])
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = mxl->size;
	  mxl->size *= 2;
	  mxl->list = (mix_state **)realloc(mxl->list, mxl->size * sizeof(mix_state *));
	  for (i = loc; i < mxl->size; i++) mxl->list[i] = NULL;
	}
    }
  mxl->list[loc] = ms;
}


void preload_mixes(mix_state **mixes, int low_id, ed_list *ed)
{
  mix_list *mxl;
  mxl = (mix_list *)(ed->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  mixes[mxl->list[i]->mix_id - low_id] = mxl->list[i];
    }
}


static mix_state *ed_mix_state(ed_list *ed, int mix_id)
{
  mix_list *mxl;
  mxl = (mix_list *)(ed->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if ((mxl->list[i]) &&
	    (mxl->list[i]->mix_id == mix_id))
	  return(mxl->list[i]);
    }
  return(NULL);
}


/* these are the nominally unchanging fields in a mix (they don't follow the edit lists) */

#define MIX_TAG_ERASED -1
#define ORIGINAL_SYNC_UNSET -1

typedef struct {
  int id;
  char *name;
  chan_info *cp;
  int original_index;          /* index into cp->sounds array for original mix data */
  char *in_filename;
  mus_long_t in_samps;
  int in_chan;
  int tag_x, tag_y;
  int sync, original_sync;
  file_delete_t temporary;     /* in-filename was written by us and needs to be deleted when mix state is deleted */
  peak_env_info *peak_env;
  Xen properties;
  int properties_gc_loc;
  color_t color, original_color;
  int x, y;  /* these are needed to know where to erase while dragging the tag */
} mix_info;


static mix_state *current_mix_state(mix_info *md)
{
  if (md)
    return(ed_mix_state(md->cp->edits[md->cp->edit_ctr], md->id));
  return(NULL);
}


#if 0
/* if edpos args to various mix field (getters anyway), mix? mixes make-mix-sampler... */

static mix_state *mix_state_at_edpos(mix_info *md, int edpos)
{
  if (md)
    {
      if (edpos == AT_CURRENT_EDIT_POSITION)
	return(current_mix_state(md));
      else
	{
	  chan_info *cp;
	  cp = md->cp;
	  if ((edpos >= 0) && 
	      (edpos < cp->edit_size) &&
	      (cp->edits[edpos]))
	    return(ed_mix_state(cp->edits[edpos], md->id));
	}
    }
  return(NULL);
}
#endif


/* for ease of access, all tagged mixes that are still accessible (perhaps via undo from fixed state, etc)
 *   are saved in an array indexed by the mix id.  A mix is "ok" if it's still in this array, and "active"
 *   if it's represented in the current edit's mix list.
 */

#define MIX_INFO_INCREMENT 16
static mix_info **mix_infos = NULL;
static int mix_infos_size = 0;

bool mix_exists(int n) 
{
  return((n >= 0) && 
	 (n < mix_infos_size) &&
	 (mix_infos[n]));
}


bool mix_is_active(int n)
{
  return((mix_exists(n)) &&
	 (current_mix_state(mix_infos[n])));
}


static mix_info *md_from_id(int n) 
{
  if (mix_exists(n))
    return(mix_infos[n]);
  return(NULL);
}


int any_mix_id(void)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_is_active(i)) 
      return(i);
  return(INVALID_MIX_ID);
}


int next_mix_id(int id)
{
  int i;
  for (i = id + 1; i < mix_infos_ctr; i++) 
    if (mix_is_active(i)) 
      return(i);
  return(INVALID_MIX_ID);
}


int previous_mix_id(int id)
{
  int i, top;
  top = id - 1;
  if (top >= mix_infos_ctr) top = mix_infos_ctr - 1;
  for (i = top; i >= 0; i--) 
    if (mix_is_active(i)) 
      return(i);
  return(INVALID_MIX_ID);
}


int lowest_mix_id(void)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_infos[i])
      return(i);
  return(INVALID_MIX_ID);
}


int highest_mix_id(void)
{
  int i;
  for (i = mix_infos_ctr - 1; i >= 0; i--)
    if (mix_infos[i])
      return(i);
  return(INVALID_MIX_ID);
}


static mix_info *free_mix_info(mix_info *md)
{
  if (md)
    {
      if (md->name) {free(md->name); md->name = NULL;}
      mix_infos[md->id] = NULL;
      if (md->temporary == DELETE_ME)
	{
	  if (mus_file_probe(md->in_filename))
	    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
	}
      if (md->in_filename) {free(md->in_filename); md->in_filename = NULL;}
      if (md->properties_gc_loc != NOT_A_GC_LOC)
	{
	  snd_unprotect_at(md->properties_gc_loc);
	  md->properties_gc_loc = NOT_A_GC_LOC;
	  md->properties = Xen_false;
	}
      if (md->peak_env)
	md->peak_env = free_peak_env_info(md->peak_env);
      free(md);
    }
  return(NULL);
}


void free_channel_mixes(chan_info *cp)
{
  /* called in snd-data.c during chan_info cleanup */
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	mix_infos[i] = free_mix_info(md);
    }
}


void reset_mix_ctr(void)
{
  mix_infos_ctr = 0;
}


const char *mix_name(int id)
{
  if (mix_exists(id))
    return(mix_infos[id]->name);
  return(NULL);
}


const char *mix_file_name(int id)
{
  if (mix_exists(id))
    return(mix_infos[id]->in_filename);
  return(NULL);
}


static void mix_set_file_name(int id, int chans, const char *name)
{
  int i;

  for (i = 0; i < chans; i++)
    {
      if (mix_exists(id + i))
        {
          mix_info *md;
          md = md_from_id(id + i);
          if (md->in_filename) free(md->in_filename);
          md->in_filename = mus_strdup(name);
          md->in_chan = i;
        }
    }
}


int mix_name_to_id(const char *name)
{
  int i, loc_so_far = -1;
  chan_info *selected_cp = NULL;
  selected_cp = selected_channel();
  for (i = 0; i < mix_infos_size; i++)
    if ((mix_infos[i]) &&
	(mus_strcmp(mix_infos[i]->name, name)))
      {
	if ((!selected_cp) ||
	    (mix_infos[i]->cp == selected_cp))  /* try to find mix in the currently selected channel (possible name collisions) */
	  return(i);
	if (loc_so_far == -1)
	  loc_so_far = i;
      }
  return(loc_so_far);
}


static mix_info *make_mix_info(chan_info *cp)
{
  mix_info *md;
  if (!mix_infos)
    {
      mix_infos_size = MIX_INFO_INCREMENT;
      mix_infos = (mix_info **)calloc(mix_infos_size, sizeof(mix_info *));
    }
  else
    {
      if (mix_infos_ctr >= mix_infos_size)
	{
	  int i;
	  mix_infos_size += MIX_INFO_INCREMENT;
	  mix_infos = (mix_info **)realloc(mix_infos, mix_infos_size * sizeof(mix_info *));
	  for (i = mix_infos_size - MIX_INFO_INCREMENT; i < mix_infos_size; i++) 
	    mix_infos[i] = NULL;
	}
    }
  md = (mix_info *)calloc(1, sizeof(mix_info));
  mix_infos[mix_infos_ctr] = md;
  md->id = mix_infos_ctr++;
  md->cp = cp;
  md->temporary = DONT_DELETE_ME;
  md->color = ss->mix_color;
  md->original_color = md->color;
  md->tag_y = 0;
  md->tag_x = 0;
  md->name = NULL;
  md->y = MIX_TAG_ERASED;
  md->peak_env = NULL;
  md->properties_gc_loc = NOT_A_GC_LOC;
  md->properties = Xen_false;
  md->sync = 0;
  md->original_sync = ORIGINAL_SYNC_UNSET;
  return(md);
}


mix_state *prepare_mix_state_for_channel(chan_info *cp, int mix_loc, mus_long_t beg, mus_long_t len)
{
  mix_info *md;
  md = make_mix_info(cp);  /* make the mix_info data for this virtual mix */
  md->original_index = mix_loc;
  md->in_samps = len;
  return(make_mix_state(md->id, mix_loc, beg, len)); 
}


static void for_each_channel_mix(chan_info *cp, void (*func)(mix_info *umx))
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	(*func)(md);
    }
}


static void for_each_syncd_mix(int current_mix_id, void (*func)(mix_info *md, void *data), void *udata)
{
  int sync;
  sync = mix_sync_from_id(current_mix_id);
  if (sync != 0)
    {
      int i;
      for (i = 0; i < mix_infos_ctr; i++)
	if ((i != current_mix_id) && 
	    (mix_is_active(i)))
	    {
	      mix_info *md;
	      md = mix_infos[i];
	      if ((md) && 
		  (md->sync == sync))
		(*func)(md, udata);
	    }
    }
}


bool channel_has_mixes(chan_info *cp)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && 
	  (md->cp == cp))
	return(true);
    }
  return(false);
}


bool channel_has_active_mixes(chan_info *cp)
{
  return((bool)(cp->edits[cp->edit_ctr]->mixes));
}


static void remove_temporary_mix_file(mix_info *md)
{
  if ((md->temporary == DELETE_ME) &&
      (mus_file_probe(md->in_filename)))
    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
}


void delete_any_remaining_mix_temp_files_at_exit(chan_info *cp)
{
  for_each_channel_mix(cp, remove_temporary_mix_file);
}


void mix_info_to_file(FILE *fd, chan_info *cp)
{
  int i, n;
  bool write_info = false;
  
  for (i = 0, n = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
        {
          if ((!write_info) &&
              ((md->sync > 0) ||
#if (!USE_NO_GUI)
               (md->tag_y > 0) ||
               (md->color != ss->mix_color) ||
#endif
               (md->name)))
            {
              write_info = true;
              fprintf(fd, "      (let ((m (list->vector (mixes sfile %d))))\n"
		      "        (when (> (length m) 0)",
                      cp->chan);
            }
          if (md->sync > 0)
            fprintf(fd, "\n          (set! (mix-sync (m %d)) %d)", n, md->sync);
          if (md->name)
            fprintf(fd, "\n          (set! (mix-name (m %d)) \"%s\")", n, md->name);
#if (!USE_NO_GUI)
          if (md->tag_y > 0)
            fprintf(fd, "\n          (set! (mix-tag-y (m %d)) %d)", n, md->tag_y);
          if (md->color != ss->mix_color)
            {
              double r, g, b;
#if USE_MOTIF
              XColor tmp_color;
              Display *dpy;
              dpy = XtDisplay(main_shell(ss));
              tmp_color.flags = DoRed | DoGreen | DoBlue;
              tmp_color.pixel = md->color;
              XQueryColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &tmp_color);
              r = rgb_to_float(tmp_color.red);
              g = rgb_to_float(tmp_color.green);
              b = rgb_to_float(tmp_color.blue);
#else
              color_t pix = md->color;
              r = rgb_to_float(pix->red);
              g = rgb_to_float(pix->green);
              b = rgb_to_float(pix->blue);
#endif
              fprintf(fd, "\n          (set! (mix-color (m %d)) (make-color %f %f %f))",
                      n, r, g, b);
            }
#endif
          n++;
        }
    }
  if (write_info) fprintf(fd, "))\n");
}


static int compare_mix_positions(const void *umx1, const void *umx2)
{
  mus_long_t mx1, mx2;
  mx1 = (*((mus_long_t *)umx1));
  mx2 = (*((mus_long_t *)umx2));
  if (mx1 > mx2) return(1);
  if (mx1 == mx2) return(0);
  return(-1);
}


void goto_mix(chan_info *cp, int count)
{
  /* C-x C-j */
  mix_list *mxl;
  if (!cp) return;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i, k = 0;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  k++;
      if (k > 0)
	{
	  int j = 0;
	  mus_long_t *begs;
	  begs = (mus_long_t *)calloc(k, sizeof(mus_long_t));
	  for (i = 0; i < mxl->size; i++)
	    if (mxl->list[i])
	      begs[j++] = mxl->list[i]->beg;
	  if (k == 1)
	    cursor_moveto(cp, begs[0]);
	  else
	    {
	      qsort((void *)begs, j, sizeof(mus_long_t), compare_mix_positions);
	      /* now find where we are via cursor_sample(cp) and go forward or back as per count */
	      if (count > 0)
		{
		  for (i = 0; i < j; i++)
		    if (begs[i] > cursor_sample(cp))
		      {
			count--;
			if (count == 0)
			  {
			    cursor_moveto(cp, begs[i]);
			    break;
			  }
		      }
		  if ((count > 0) && (cursor_sample(cp) < begs[j - 1]))
		    cursor_moveto(cp, begs[j - 1]);
		}
	      else
		{
		  for (i = j - 1; i >= 0; i--)
		    if (begs[i] < cursor_sample(cp))
		      {
			count++;
			if (count == 0)
			  {
			    cursor_moveto(cp, begs[i]);
			    break;
			  }
		      }
		  if ((count < 0) && (cursor_sample(cp) > begs[0]))
		    cursor_moveto(cp, begs[0]);
		}
	    }
	  free(begs);
	}
    }
}


mus_long_t zoom_focus_mix_in_channel_to_position(chan_info *cp)
{
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes);
  if (mxl)
    {
      mus_long_t lo, hi;
      int i;
      lo = cp->axis->losamp;
      hi = cp->axis->hisamp;
      for (i = 0; i < mxl->size; i++)
	{
	  mix_state *ms;
	  ms = mxl->list[i];
	  if ((ms) &&
	      (ms->beg >= lo) && 
	      (ms->beg <= hi))
	    return(ms->beg);
	}
    }
  return(-1);
}


/* follow edit list */
mus_long_t mix_position_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->beg);
  return(0);
}


mus_long_t mix_length_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->len);
  return(0);
}


mus_float_t mix_amp_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->scaler);
  return(0.0);
}


mus_float_t mix_speed_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->speed);
  return(0.0);
}


env *mix_amp_env_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->amp_env);
  return(NULL);
}


/* stable (not in edit list) */

int mix_sync_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->sync);
  return(0);
}


static int current_mix_sync_max = 0;

int mix_set_sync_from_id(int id, int new_sync)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    {
      if (new_sync == GET_NEW_SYNC) 
	{
	  new_sync = current_mix_sync_max + 1;
	  md->original_sync = new_sync;
	}
      else
	{
	  if (new_sync == GET_ORIGINAL_SYNC)
	    {
	      if (md->original_sync == ORIGINAL_SYNC_UNSET)
		new_sync = current_mix_sync_max + 1;
	      else new_sync = md->original_sync;
	    }
	  else
	    {
	      if (new_sync != 0)
		md->original_sync = new_sync;
	    }
	}

      md->sync = new_sync;
      if (new_sync > current_mix_sync_max)
	current_mix_sync_max = new_sync;
      return(md->sync);
    }
  return(0);
}


static const char *mix_name_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->name);
  return(NULL);
}


static const char *mix_set_name_from_id(int id, const char *new_name)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    {
      if (md->name) free(md->name);
      md->name = mus_strdup(new_name);
      return(new_name);
    }
  return(NULL);
}


chan_info *mix_chan_info_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->cp);
  return(NULL);
}


static int mix_tag_y_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->tag_y);
  return(0);
}


color_t mix_color_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(md->color);
  return(ss->mix_color);
}


color_t mix_set_color_from_id(int id, color_t new_color)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    md->color = new_color;
  return(new_color);
}


void mix_unset_color_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    md->color = md->original_color;
}


static void syncd_mix_unset_color_1(mix_info *md, void *ignore)
{
  md->color = md->original_color;
}


void syncd_mix_unset_color(int id)
{
  for_each_syncd_mix(id, syncd_mix_unset_color_1, NULL);
}


static void syncd_mix_set_color_1(mix_info *md, void *ignore)
{
  /* assume red (this is from the mix dialog) */
  md->color = ss->red;
}


void syncd_mix_set_color(int id, color_t col)
{
  for_each_syncd_mix(id, syncd_mix_set_color_1, NULL);
}


bool mix_set_amp_edit(int id, mus_float_t amp)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md); /* needed for edit bounds and existence check */
  if (old_ms)
    {
      if (old_ms->scaler != amp)
	{
	  char *origin = NULL;
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %.4f set-mix-amp", id, amp);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-amp (car -mix-%d)) %.4f)", id, amp);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_amp(_mix_%d, %.4f)", id, amp);
#endif
	  edited = begin_mix_op(md->cp, old_ms->beg, old_ms->len, old_ms->beg, old_ms->len, md->cp->edit_ctr, origin);
	  free(origin);
	  if (edited)
	    {
	      mix_state *ms;
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      ms->scaler = amp;
	      end_mix_op(md->cp, 0, 0);
	    }
	}
    }
  return(edited);
}


typedef struct {mus_float_t amp;} syncd_amp_info;

static void syncd_mix_set_amp_1(mix_info *md, void *amp)
{
  mus_long_t beg, len;
  syncd_amp_info *ai = (syncd_amp_info *)amp;
  mix_set_amp_edit(md->id, ai->amp);
  beg = mix_position_from_id(md->id);
  len = mix_length_from_id(md->id);
  mix_display_during_drag(md->id, beg, beg + len);
}


void syncd_mix_set_amp(int id, mus_float_t amp)
{
  syncd_amp_info *ai;
  ai = (syncd_amp_info *)malloc(sizeof(syncd_amp_info));
  ai->amp = amp;
  for_each_syncd_mix(id, syncd_mix_set_amp_1, (void *)ai);
  free(ai);
}


static mus_float_t src_input(void *arg, int direction)
{
  return(read_sample((snd_fd *)arg));
}


static int remake_mix_data(mix_state *ms, mix_info *md)
{
  chan_info *cp;
  mus_long_t len;
  snd_fd *mix_reader;
  mus_float_t old_amp;
  mus_any *egen = NULL, *src_gen = NULL;
  env *e;

  cp = md->cp;
  old_amp = ms->scaler;
  ms->scaler = 1.0;
  len = snd_round_mus_long_t((double)(md->in_samps) / (double)(ms->speed));
  e = ms->amp_env;

  mix_reader = make_virtual_mix_reader(cp, 0, md->in_samps, md->original_index, 1.0, READ_FORWARD);

  if (e)
    egen = mus_make_env(e->data, e->pts, 1.0, 0.0, 1.0, 0.0, len - 1, NULL);
  if (ms->speed != 1.0)
    src_gen = mus_make_src(&src_input, ms->speed, sinc_width(ss), (void *)mix_reader);

  prepare_sound_list(cp);
  if (cp->sounds[md->original_index]->type == SND_DATA_BUFFER)
    {
      int i;
      mus_float_t *new_buffer;
      new_buffer = (mus_float_t *)malloc(len * sizeof(mus_float_t));
      if (!src_gen)
	{
	  for (i = 0; i < len; i++)
	    new_buffer[i] = (mus_env(egen) * read_sample(mix_reader));
	}
      else
	{
	  if (!egen)
	    {
	      for (i = 0; i < len; i++)
		new_buffer[i] = (mus_src(src_gen, 0.0, &src_input));
	    }
	  else
	    {
 	      for (i = 0; i < len; i++)
		new_buffer[i] = (mus_src(src_gen, 0.0, &src_input) * mus_env(egen));
	    }
	}
      cp->sounds[cp->sound_ctr] = make_snd_data_buffer(new_buffer, (int)len, cp->edit_ctr);
      free(new_buffer);
    }
  else
    {
      mus_long_t i;
      file_info *hdr;
      int fd, err = 0;
      char *temp_file;
      io_error_t io_err = IO_NO_ERROR;
      mus_float_t **data;
      mus_float_t *new_buffer;
      int j = 0;
      
      temp_file = snd_tempnam();
      hdr = make_temp_header(temp_file, snd_srate(cp->sound), 1, len, S_set S_mix_amp_env);
      fd = open_temp_file(temp_file, 1, hdr, &io_err);
      data = (mus_float_t **)malloc(sizeof(mus_float_t *));
      new_buffer = (mus_float_t *)malloc(MAX_BUFFER_SIZE * sizeof(mus_float_t));
      data[0] = new_buffer;

      if (!src_gen)
	{
	  for (i = 0; i < len; i++)
	    {
	      new_buffer[j++] = (read_sample(mix_reader) * mus_env(egen));
	      if (j == MAX_BUFFER_SIZE)
		{
		  err = mus_file_write(fd, 0, j - 1, 1, data);
		  j = 0;
		  if (err != MUS_NO_ERROR) break;
		}
	    }
	}
      else
	{
	  if (!egen)
	    {
	      for (i = 0; i < len; i++)
		{
		  new_buffer[j++] = (mus_src(src_gen, 0.0, &src_input));
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(fd, 0, j - 1, 1, data);
		      j = 0;
		      if (err != MUS_NO_ERROR) break;
		    }
		}
	    }
	  else
	    {
	      for (i = 0; i < len; i++)
		{
		  new_buffer[j++] = (mus_src(src_gen, 0.0, &src_input) * mus_env(egen));
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(fd, 0, j - 1, 1, data);
		      j = 0;
		      if (err != MUS_NO_ERROR) break;
		    }
		}
	    }
	}
      if (j > 0) mus_file_write(fd, 0, j - 1, 1, data);
      close_temp_file(temp_file, fd, hdr->type, len * mus_bytes_per_sample(hdr->sample_type));
      free_file_info(hdr);
      
      hdr = make_file_info(temp_file, FILE_READ_ONLY, FILE_NOT_SELECTED);
      fd = snd_open_read(temp_file);
      snd_file_open_descriptors(fd,
				temp_file,
				hdr->sample_type,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      cp->sounds[cp->sound_ctr] = make_snd_data_file(temp_file, 
						     make_file_state(fd, hdr, 0, 0, FILE_BUFFER_SIZE),
						     hdr, DELETE_ME, cp->edit_ctr, 0);
      free(temp_file);
      free(new_buffer);
      free(data);
    }
  
  free_snd_fd(mix_reader);
  if (egen) mus_free(egen);
  if (src_gen) mus_free(src_gen);
  
  ms->scaler = old_amp;
  return(cp->sound_ctr);
}


bool mix_set_amp_env_edit(int id, env *e)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md); /* needed for edit bounds and existence check */
  if (old_ms)
    {
      if (!(envs_equal(old_ms->amp_env, e)))
	{
	  chan_info *cp;
	  char *origin = NULL, *envstr;
	  
	  envstr = env_to_string(e);
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %s set-mix-amp-env", id, envstr);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-amp-env (car -mix-%d)) %s)", id, envstr);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_amp_env(_mix_%d, %s)", id, envstr);
#endif
	  free(envstr);

	  cp = md->cp;
	  edited = begin_mix_op(cp, old_ms->beg, old_ms->len, old_ms->beg, old_ms->len, cp->edit_ctr, origin); /* this does not change beg or len */
	  free(origin);
	  if (edited)
	    {
	      mix_state *ms;
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      if (ms->amp_env) free_env(ms->amp_env);
	      ms->amp_env = copy_env(e);

	      /* can't use mus_env (as the reader op) here because we need to run backwards */
	      if ((e) || (ms->speed != 1.0))
		ms->index = remake_mix_data(ms, md);
	      else ms->index = md->original_index;
	  
	      end_mix_op(cp, 0, 0);
	    }
	}
    }
  return(edited);
}


static void syncd_mix_set_amp_env_1(mix_info *md, void *e)
{
  mus_long_t beg, len;
  env *amp_env = (env *)e;
  mix_set_amp_env_edit(md->id, amp_env);
  beg = mix_position_from_id(md->id);
  len = mix_length_from_id(md->id);
  mix_display_during_drag(md->id, beg, beg + len);
}


void syncd_mix_set_amp_env(int id, env *e)
{
  for_each_syncd_mix(id, syncd_mix_set_amp_env_1, (void *)e);
}



bool mix_set_position_edit(int id, mus_long_t pos)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  if (pos < 0) pos = 0;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md);
  if (old_ms)
    {
      if (old_ms->beg != pos)
	{
	  char *origin = NULL;
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %" PRId64 " set-mix-position", id, pos);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-position (car -mix-%d)) %" PRId64 ")", id, pos);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_position(_mix_%d, %" PRId64 ")", id, pos);
#endif
	  edited = begin_mix_op(md->cp, old_ms->beg, old_ms->len, pos, old_ms->len, md->cp->edit_ctr, origin); /* this does not change beg or len */

	  free(origin);
	  if (edited)
	    {
	      mix_state *ms;
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      unmix(md->cp, ms);
	      ms->beg = pos;
	      remix(md->cp, ms);
	      end_mix_op(md->cp, (old_ms->beg != pos) ? old_ms->beg : 0, old_ms->len);
	    }
	}
    }
  return(edited);
}


bool mix_set_speed_edit(int id, mus_float_t spd)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md); /* needed for edit bounds and existence check */
  if (old_ms)
    {
      if (old_ms->speed != spd)
	{
	  chan_info *cp;
	  mus_long_t len;
	  char *origin = NULL;
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %.4f set-mix-speed", id, spd);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-speed (car -mix-%d)) %.4f)", id, spd);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_speed(_mix_%d, %.4f)", id, spd);
#endif
	  cp = md->cp;
	  len = snd_round_mus_long_t((double)(md->in_samps) / (double)spd);
	  edited = begin_mix_op(cp, old_ms->beg, old_ms->len, old_ms->beg, len, cp->edit_ctr, origin);
	  
	  free(origin);
	  if (edited)
	    {
	      mix_state *ms;
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      unmix(cp, ms);                      /*   but unmix before changing mix length! */

	      ms->speed = spd;
	      ms->len = len;
	      if ((ms->speed != 1.0) || (ms->amp_env))
		ms->index = remake_mix_data(ms, md);
	      else ms->index = md->original_index;
	      
	      remix(cp, ms);
	      end_mix_op(cp, 0, 0); /* old_ms->beg, old_ms->len); */
	    }
	}
    }
  return(edited);
}


typedef struct {mus_float_t speed;} syncd_speed_info;

static void syncd_mix_set_speed_1(mix_info *md, void *speed)
{
  mus_long_t beg, len;
  syncd_speed_info *ai = (syncd_speed_info *)speed;
  mix_set_speed_edit(md->id, ai->speed);
  beg = mix_position_from_id(md->id);
  len = mix_length_from_id(md->id);
  mix_display_during_drag(md->id, beg, beg + len);
}


void syncd_mix_set_speed(int id, mus_float_t speed)
{
  syncd_speed_info *ai;
  ai = (syncd_speed_info *)malloc(sizeof(syncd_speed_info));
  ai->speed = speed;
  for_each_syncd_mix(id, syncd_mix_set_speed_1, (void *)ai);
  free(ai);
}





/* mix-samples/set-mix-samples? 
 *   combine mix_set_amp_env_edit with remake_mix_data accepting either vct or filename
 *   one problem is how to handle md->peak_env: currently I think it is based on the original
 *     and src stretches/mix-env uses env-on-env, so it's never remade.
 */


/* edit-list->function support for mixes:
 *      mix list search for current channel, make outer let holding all names as (-mix-### ###)
 *      origins for make-mix procs: (set! -mix-### (...))
 *      origins otherwise, reference to mix: -mix-###
 */

char *edit_list_mix_init(chan_info *cp)
{
  char *new_list = NULL;
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  {
	    char *old_list;
	    int id;
	    id = mxl->list[i]->mix_id;

	    old_list = new_list;
#if HAVE_SCHEME
	    new_list = mus_format("%s%s(-mix-%d #f)", 
				  (old_list) ? old_list : "", 
				  (old_list) ? " " : "",  /* strcat of previous + possible space */
				  id);                  
#endif
#if HAVE_RUBY
	    new_list = mus_format("%s%s_mix_%d = false", 
				  (old_list) ? old_list : "", 
				  (old_list) ? "; " : "",  /* strcat of previous + possible space */
				  id);                  
#endif
#if HAVE_FORTH
	    new_list = mus_format("%s%s#f { -mix-%d }", 
				  (old_list) ? old_list : "", 
				  (old_list) ? " " : "",   /* strcat of previous + possible space */
				  id);                  
#endif
	    if (old_list) free(old_list);
	  }
    }
  return(new_list);
}


#define MIX_TAG_Y_OFFSET mix_tag_height(ss)

int hit_mix(chan_info *cp, int x, int y) /* mix tag press in snd-chn.c */
{
  #define SLOPPY_MOUSE 3
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i, width, height;
      width = mix_tag_width(ss);
      height = mix_tag_height(ss);
      for (i = 0; i < mxl->size; i++)
	{
	  mix_state *ms;
	  ms = mxl->list[i];
	  if (ms)
	    {
	      int mx, my;
	      mx = mix_infos[ms->mix_id]->tag_x;
	      if (mx <= 0)
		mx = grf_x((double)(ms->beg) / (double)(snd_srate(cp->sound)), cp->axis);
	      my = mix_infos[ms->mix_id]->tag_y + MIX_TAG_Y_OFFSET + cp->axis->y_offset;
	      if ((x + SLOPPY_MOUSE >= (mx - width / 2)) && 
		  (x - SLOPPY_MOUSE <= (mx + width / 2)) &&
		  (y + SLOPPY_MOUSE >= (my - height)) && 
		  (y - SLOPPY_MOUSE <= (my + 0)))
		return(ms->mix_id);
	    }
	}
    }
  return(NO_MIX_TAG);
}


#if USE_MOTIF
  #define STRING_Y_OFFSET 3
  #define STRING_HEIGHT 12
#else
  #define STRING_Y_OFFSET -8
  #define STRING_HEIGHT 12
#endif

#define HIT_SLOP 4

int hit_mix_triangle(chan_info *cp, int x, int y)
{
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	{
	  mix_state *ms;
	  ms = mxl->list[i];
	  if (ms)
	    {
	      int mx, my;
	      mx = mix_infos[ms->mix_id]->tag_x;
	      if (mx <= 0)
		mx = grf_x((double)(ms->beg) / (double)(snd_srate(cp->sound)), cp->axis);
	      my = mix_infos[ms->mix_id]->tag_y + MIX_TAG_Y_OFFSET + STRING_HEIGHT + cp->axis->y_offset;
	      if ((mx < (x + HIT_SLOP)) &&
		  ((mx + play_arrow_size(ss) + HIT_SLOP) >= x) &&
		  ((y + HIT_SLOP) > my) &&
		  (y < (my + 2 * play_arrow_size(ss) + HIT_SLOP)))
		return(ms->mix_id);
	    }
	}
    }
  return(NO_MIX_TAG);
}


/* mix display */

void channel_set_mix_tags_erased(chan_info *cp)
{
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  {
	    mix_info *md;
	    md = mix_infos[mxl->list[i]->mix_id];
	    md->y = MIX_TAG_ERASED;
	  }
    }
}


static Xen draw_mix_hook;

static void draw_mix_tag(mix_info *md, int x, int y)
{
  chan_info *cp;
  int width, height;
  graphics_context *ax;
  char *lab = NULL;

  if (Xen_hook_has_list(draw_mix_hook))
    {
      Xen res;
      res = run_progn_hook(draw_mix_hook,
			   Xen_list_5(new_xen_mix(md->id), 
				      C_int_to_Xen_integer(md->x),
				      C_int_to_Xen_integer(md->y),
				      C_int_to_Xen_integer(x),
				      C_int_to_Xen_integer(y)),
			   S_draw_mix_hook);
      if (!(Xen_is_false(res)))
	{
	  md->x = x;
	  md->y = y;
	  if (Xen_is_list(res))
	    {
	      md->tag_x = Xen_integer_to_C_int(Xen_car(res));
	      md->tag_y = Xen_integer_to_C_int(Xen_cadr(res));
	    }
	  return;
	}
    }

  cp = md->cp;

  /* draw the mix tag */
  width = mix_tag_width(ss);
  height = mix_tag_height(ss);

  if (md->y != MIX_TAG_ERASED)
    {
      /* erase old tag and name */
      ax = erase_context(cp);
      fill_rectangle(ax, md->x - width / 2 - 1, md->y - height - 1, width + 2, height + STRING_HEIGHT);
      md->y = MIX_TAG_ERASED;
    }

  md->x = x;
  md->y = y;

  /* redraw the mix id */
  ax = copy_context(cp);
  set_tiny_numbers_font(cp, ax);
  if (cp->printing) ps_set_tiny_numbers_font();

  if (md->name)
    lab = mus_strdup(md->name);
  else
    {
      lab = (char *)calloc(16, sizeof(char));
      snprintf(lab, 16, "%d", md->id);
    }
  draw_string(ax, x - width / 2, y - height / 2 + STRING_Y_OFFSET, lab, strlen(lab));
  if (cp->printing) ps_draw_string(cp->axis, x - width / 2, y - height / 2 + STRING_Y_OFFSET, lab);

  if (lab) {free(lab); lab = NULL;}

  ax = mix_waveform_context(cp);
  set_foreground_color(ax, md->color); 
  fill_rectangle(ax, x - width / 2, y - height + STRING_HEIGHT, width, height);

  /* now draw the play triangle below the tag */
  y += (height - 4);
  fill_polygon(ax, 4,
	       x, y,
	       x + play_arrow_size(ss), y + play_arrow_size(ss),
	       x, y + 2 * play_arrow_size(ss),
	       x, y);
}


static int local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((int)(ap->x_base + val * ap->x_scale));
}


#define MIX_PEAK_ENV_CUTOFF 20000

static peak_env_info *make_mix_input_peak_env(mix_info *md)
{
  mix_state *ms;
  ms = current_mix_state(md);
  if (ms->len >= MIX_PEAK_ENV_CUTOFF)
    {
      peak_env_info *ep;
      snd_fd *sf;
      int val, sb = 0;
      mus_long_t n;

      ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      val = (int)(log((double)(ms->len)));
      if (val > 20) val = 20;
      ep->peak_env_size = snd_int_pow2(val);
      ep->samps_per_bin = (int)(ceil((double)(ms->len) / (double)(ep->peak_env_size)));
      ep->data_max = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
      ep->data_min = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
      ep->fmin = MIN_INIT;
      ep->fmax = MAX_INIT;

      sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, 1.0, READ_FORWARD);

      for (n = 0; n < ms->len; n += ep->samps_per_bin)
	{
	  mus_float_t ymin, ymax, val;
	  int i;
	  val = read_sample(sf);
	  ymin = val;
	  ymax = val;
	  for (i = 1; i < ep->samps_per_bin; i++)
	    {
	      val = read_sample(sf);
	      if (ymin > val) 
		ymin = val; 
	      else 
		if (ymax < val) 
		  ymax = val;
	    }
	  ep->data_max[sb] = ymax;
	  ep->data_min[sb++] = ymin;
	  if (ymin < ep->fmin) ep->fmin = ymin;
	  if (ymax > ep->fmax) ep->fmax = ymax;
	}

      ep->completed = true;
      free_snd_fd(sf);
      return(ep);
    }
  return(NULL);
}


static bool mix_input_peak_env_usable(mix_info *md, mus_float_t samples_per_pixel) 
{
  if (!(md->peak_env))
    md->peak_env = make_mix_input_peak_env(md);
  return((md->peak_env) && 
	 (samples_per_pixel >= (mus_float_t)(md->peak_env->samps_per_bin)));
}


static peak_env_info *env_on_env(env *e, peak_env_info *peaks)
{
  peak_env_info *ep;
  ep = copy_peak_env_info(peaks, false);
  if (ep)
    {
      int i;
      mus_any *me;
      me = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, ep->peak_env_size - 1, NULL);
      for (i = 0; i < ep->peak_env_size; i++) 
	{
	  mus_float_t val;
	  val = mus_env(me);
	  if (val >= 0.0)
	    {
	      ep->data_min[i] = ep->data_min[i] * val;
	      ep->data_max[i] = ep->data_max[i] * val;
	    }
	  else
	    {
	      ep->data_min[i] = ep->data_max[i] * val;
	      ep->data_max[i] = ep->data_min[i] * val;
	    }
	}
      mus_free(me);
    }
  return(ep);
}


static int prepare_mix_peak_env(mix_info *md, mus_float_t scl, int yoff, mus_long_t newbeg, mus_long_t newend, double srate, axis_info *ap)
{
  int i, j, mix_start;
  int lastx;
  double xend, xstart, xstep, mix_samps_per_bin;
  peak_env_info *ep;
  env *amp_env;
  mus_float_t ymin = 0.0, ymax = 0.0;

  amp_env = mix_amp_env_from_id(md->id);
  if (!amp_env)
    ep = md->peak_env;
  else ep = env_on_env(amp_env, md->peak_env);

  mix_samps_per_bin = (double)(ep->samps_per_bin) / mix_speed_from_id(md->id);

  /* mix starts at newbeg, current display starts at lo,
     mix goes to newend, current display goes to hi,
  */

  if (ap->losamp > newbeg)
    {
      mix_start = snd_round((double)(ap->losamp - newbeg) / mix_samps_per_bin);
      xstart = ap->x0;
    }
  else 
    {
      mix_start = 0;
      xstart = (double)(newbeg) / srate;
    }

  if (ap->hisamp < newend)
    xend = ap->x1;
  else xend = (double)(newend) / srate;

  xstep = mix_samps_per_bin / srate;
  lastx = local_grf_x(xstart, ap);

  for (i = mix_start, j = 0; (xstart < xend) && (i < ep->peak_env_size); xstart += xstep, i++)
    {
      mus_float_t low, high;
      int newx;
      low = ep->data_min[i];
      high = ep->data_max[i];
      newx = local_grf_x(xstart, ap);
      if (newx > lastx)                  /* set lastx's bin (j) from min/max for that output bin */
	{
	  set_grf_points(lastx, j++,
			 (int)(yoff - scl * ymin),
			 (int)(yoff - scl * ymax));
	  if (j >= POINT_BUFFER_SIZE) break;
	  lastx = newx;
	  ymin = low;
	  ymax = high;
	}
      else
	{
	  if (high > ymax) ymax = high;
	  if (low < ymin) ymin = low;
	}
    }

  if (amp_env)
    free_peak_env_info(ep);

  return(j);
}


static int prepare_mix_waveform(mix_info *md, mix_state *ms, axis_info *ap, mus_float_t scl, int yoff, double cur_srate, bool *two_sided)
{
  mus_long_t i, newbeg, newend;
  int pts = 0;
  mus_long_t samps;
  mus_float_t samples_per_pixel;
  double x;
  mus_long_t lo, hi;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time;

  newbeg = ms->beg;
  newend = newbeg + ms->len;
  lo = ap->losamp;
  hi = ap->hisamp;
  if ((newend <= lo) || (newbeg >= hi)) return(0);
  if ((ap->y_axis_y0 - ap->y_axis_y1) < scl) return(0);
  start_time = (double)(ap->losamp) / cur_srate;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  if (newend > hi) newend = hi;
  samps = ap->hisamp - ap->losamp + 1;
  samples_per_pixel = (mus_float_t)((double)(samps - 1) / (mus_float_t)(x_end - x_start));

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      int j;
      bool widely_spaced;
      double incr, initial_x;
      if (newbeg < lo) /* mix starts before current left x0 point */
	{
	  sf = make_virtual_mix_reader(md->cp, lo - newbeg, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);
	  newbeg = lo;
	}
      else sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);

      if (!sf) return(0);
      if (samples_per_pixel < 1.0)
	{
	  incr = 1.0 / samples_per_pixel;
	  initial_x = x_start;
	  widely_spaced = true;
	}
      else
	{
	  incr = (double)1.0 /cur_srate;
	  initial_x = start_time;
	  widely_spaced = false;
	}
      x = initial_x + (incr * (newbeg - lo));
      for (j = 0, i = newbeg; i <= newend; i++, j++, x += incr)
	{
	  int ina_i;
	  ina_i = (int)(yoff - read_sample(sf));
	  if (widely_spaced)
	    set_grf_point((int)x, j, ina_i);
	  else set_grf_point(local_grf_x(x, ap), j, ina_i);
	}
      free_snd_fd(sf);
      pts = j;
      (*two_sided) = false;
    }
  else
    {
      (*two_sided) = true;
      if (mix_input_peak_env_usable(md, samples_per_pixel))
        pts = prepare_mix_peak_env(md, ms->scaler * scl, yoff, newbeg, newend, (double)cur_srate, ap);
      else
	{
	  int xi, j;
	  mus_long_t endi;
	  mus_float_t ymin, ymax, xf;
	  if (newbeg < lo)
	    {
	      sf = make_virtual_mix_reader(md->cp, lo - newbeg, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);
	      newbeg = lo;
	    }
	  else sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);
	  if (!sf) return(0);

	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  if (newend < hi) endi = newend; else endi = hi;
	  for (i = lo; i < newbeg; i++)
	    {
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  for (i = newbeg; i <= endi; i++)
	    {
	      mus_float_t ina;
	      ina = read_sample(sf);
	      if (ina > ymax) ymax = ina;
	      if (ina < ymin) ymin = ina;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j,
				 (int)(yoff - ymin),
				 (int)(yoff - ymax));
		  j++;
		  ymin = 100.0;
		  ymax = -100.0;
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  pts = j;
	  free_snd_fd(sf);
	}
    }
  return(pts);
}


int prepare_mix_dialog_waveform(int mix_id, axis_info *ap, bool *two_sided)
{
  mix_info *md;
  mix_state *ms;
  mus_float_t scl, x0, x1, y0, y1;
  mus_long_t old_lo, old_hi;
  double cur_srate;
  int pts;

  md = md_from_id(mix_id);
  if (!md) return(0);

  scl = ap->y_axis_y0 - ap->y_axis_y1;
  old_lo = ap->losamp;
  old_hi = ap->hisamp;
  x0 = ap->x0;
  x1 = ap->x1;
  y0 = ap->y0;
  y1 = ap->y1;
  cur_srate = (double)snd_srate(md->cp->sound);
  ms = current_mix_state(md);
  ap->losamp = ms->beg;
  ap->hisamp = ms->beg + ms->len;
  ap->x0 = (double)(ap->losamp) / cur_srate;
  ap->x1 = (double)(ap->hisamp) / cur_srate;
  ap->y0 = -1.0;
  ap->y1 = 1.0;
  init_axis_scales(ap);

  pts = prepare_mix_waveform(md, ms, ap, scl * .5, (int)(scl * .5), cur_srate, two_sided);

  ap->x0 = x0;
  ap->x1 = x1;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->losamp = old_lo;
  ap->hisamp = old_hi;
  init_axis_scales(ap);

  return(pts);
}


static void erase_mix_tag_and_waveform(mix_state *ms, chan_info *cp, axis_info *ap, graphics_context *ax, int x, int y)
{
  int wave_width, wave_height, old_x;
  if (ap->hisamp > ap->losamp)
    wave_width = (int)(ms->len * ((double)(ap->x_axis_x1 - ap->x_axis_x0) / (double)(ap->hisamp - ap->losamp)));
  else wave_width = 0;
  wave_height = mix_waveform_height(ss);
  old_x = x + mix_tag_width(ss) - 2;
  if ((old_x + wave_width) > ap->x_axis_x1)
    wave_width = ap->x_axis_x1 - old_x;
  fill_rectangle(ax, old_x, y - wave_height + 6, wave_width + 2, wave_height + 12);
}


static void draw_mix_tag_and_waveform(mix_info *md, mix_state *ms, int x)
{
  bool show_wave;
  int y;
  chan_info *cp;
  axis_info *ap;

  cp = md->cp;
  ap = cp->axis;
  show_wave = ((show_mix_waveforms(ss)) && (cp->show_mix_waveforms));
  y = ap->y_offset + md->tag_y + MIX_TAG_Y_OFFSET;

  if (ms->beg >= ap->losamp)
    draw_mix_tag(md, x, y);

  if (show_wave)
    {
      bool two_sided = false;
      int pts;
      pts = prepare_mix_waveform(md, ms, ap, mix_waveform_height(ss), y + STRING_HEIGHT / 2, (double)snd_srate(cp->sound), &two_sided);
      if (pts > 0)
	{
	  graphics_context *ax;
	  ax = mix_waveform_context(cp);
	  if (two_sided)
	    draw_both_grf_points(cp->dot_size, ax, pts, cp->time_graph_style);
	  else draw_grf_points(cp->dot_size, ax, pts, ap, ungrf_y(ap, y), cp->time_graph_style);
	  copy_context(cp);
	}
    }
}


static void display_one_mix_with_bounds(mix_state *ms, chan_info *cp, axis_info *ap, mus_long_t beg, mus_long_t end)
{
  if ((ms) && 
      (ms->beg + ms->len > beg) && 
      (ms->beg < end))
    {
      mix_info *md;
      int x;
      md = mix_infos[ms->mix_id];
      x = grf_x((double)(ms->beg) / (double)snd_srate(cp->sound), ap);
      if ((x + mix_tag_width(ss)) <= ap->x_axis_x1)  /* not cut off on right */
	draw_mix_tag_and_waveform(md, ms, x);
    }
}


static void display_one_mix(mix_state *ms, chan_info *cp)
{
  display_one_mix_with_bounds(ms, cp, cp->axis, cp->axis->losamp, cp->axis->hisamp);
}


static void display_channel_mixes_with_bounds(chan_info *cp, mus_long_t beg, mus_long_t end)
{
  /* called in display_channel_data if cp has active mixes
   *   it used to draw the tag and waveform if show_mix_waveforms(ss), but I think the tag should be drawn in any case
   */
  mix_list *mxl;
  if ((cp->sound->channel_style == CHANNELS_SUPERIMPOSED) ||
      (cp->squelch_update))
    return;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	display_one_mix_with_bounds(mxl->list[i], cp, cp->axis, beg, end);
    }
}


void display_channel_mixes(chan_info *cp)
{
  display_channel_mixes_with_bounds(cp, cp->axis->losamp, cp->axis->hisamp);
}


#define MIX_WAIT_TIME    50
static timeout_result_t watch_mix_proc = 0;

static void stop_watch_mix_proc(void)
{
  if (watch_mix_proc != 0)
    {
      TIMEOUT_REMOVE(watch_mix_proc);
      watch_mix_proc = 0;
    }
}


static double watch_mix_x_incr = 1.0;

#if (!USE_NO_GUI)
static TIMEOUT_TYPE watch_mix(TIMEOUT_ARGS)
{
  mix_info *md = (mix_info *)context;
  if (watch_mix_proc != 0)
    {
      watch_mix_x_incr *= 1.1;
      move_mix_tag(md->id, (int)(md->x + watch_mix_x_incr), md->y);
      watch_mix_proc = CALL_TIMEOUT(watch_mix, MIX_WAIT_TIME, md);
    }
  TIMEOUT_RESULT
}
#endif

static int edpos_before_drag = 0;
static with_hook_t hookable_before_drag;
static bool mix_dragged = false;
static Xen mix_release_hook;
static Xen mix_drag_hook;
static mus_long_t orig_beg = 0;
/* also mix_click_hook in snd-chn.c */

static mus_long_t drag_beg = 0, drag_end = 0;


typedef struct {mus_long_t beg; bool axis_changed;} move_mix_data;

static mus_long_t syncd_mix_position(int id);

static void move_syncd_mix(mix_info *md, void *data)
{
  move_mix_data *mmd = (move_mix_data *)data;
  mix_set_position_edit(md->id, syncd_mix_position(md->id) + mmd->beg);
  if (!mmd->axis_changed)
    mix_display_during_drag(md->id, drag_beg, drag_end);
}


void move_mix_tag(int mix_id, int x, int y) 
{
  /* dragging mix, hit_mix returns id, called only from snd-chn.c and above (watch_mix) */
  mix_info *md;
  mix_state *ms;
  axis_info *ap;
  chan_info *cp;
  bool axis_changed = false;
  mus_long_t pos;

  md = md_from_id(mix_id);
  if (!md)
    {
      mix_dragged = false;
      if (watch_mix_proc != 0)
        {
          stop_watch_mix_proc();
          watch_mix_proc = 0;
        }
      return;
    }
  cp = md->cp;

  if (!mix_dragged) /* starting to drag -- unmix now while we know the original position */
    {
      edpos_before_drag = cp->edit_ctr;
      hookable_before_drag = cp->hookable;
      cp->hookable = WITHOUT_HOOK;
      drag_beg = mix_position_from_id(mix_id);
      orig_beg = drag_beg;
      drag_end = drag_beg + mix_length_from_id(mix_id);
      start_dragging_syncd_mixes(mix_id);
    }
  else 
    {
      cp->edit_ctr = edpos_before_drag;
      keep_dragging_syncd_mixes(mix_id);
    }
  
  pos = snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)(snd_srate(cp->sound)));
  mix_set_position_edit(mix_id, pos);

  mix_dragged = true;
  ap = cp->axis;
  ms = current_mix_state(md); 

  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      /* we're outside the graph */
      if (watch_mix_proc != 0)
	{
	  if ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) return;
	  if ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) return;
	}
      else
	{
	  if (mix_dragged)
	    {
	      if (x < ap->x_axis_x0)
		watch_mix_x_incr = -1.0;
	      else watch_mix_x_incr = 1.0;
	      watch_mix_proc = CALL_TIMEOUT(watch_mix, MIX_WAIT_TIME, md);
	    }
	}
      x = move_axis(cp, x); /* calls update_graph eventually (in snd-chn.c reset_x_display) */
      axis_changed = true;
    }
  else 
    {
      if (watch_mix_proc != 0) 
	{
	  stop_watch_mix_proc();
	  watch_mix_proc = 0;
	}
    }

  reflect_mix_change(mix_id);
  {
    move_mix_data *mmd;
    mmd = (move_mix_data *)malloc(sizeof(move_mix_data));
    mmd->beg = pos - orig_beg;
    mmd->axis_changed = axis_changed;
    for_each_syncd_mix(mix_id, move_syncd_mix, (void *)mmd);           /* syncd mixes drag together */
    free(mmd);
  }

  if ((axis_changed) ||
      (cp->sound->channel_style == CHANNELS_SUPERIMPOSED))
    display_channel_time_data(cp);
  else
    {
      mus_long_t cur_end;
      cur_end = ms->beg + ms->len;
      if (cur_end > drag_end)
	drag_end = cur_end;
      if (ms->beg < drag_beg)
	drag_beg = ms->beg;
#if USE_MOTIF
      make_partial_graph(cp, drag_beg, drag_end);
      display_channel_mixes_with_bounds(cp, drag_beg, drag_end);
#else
      display_channel_data(cp);
#endif
    }

  if (Xen_hook_has_list(mix_drag_hook))
    run_hook(mix_drag_hook,
	     Xen_list_3(new_xen_mix(mix_id),
			C_int_to_Xen_integer(x),
			C_int_to_Xen_integer(y)),
	     S_mix_drag_hook);
}


static void syncd_mix_set_position_1(mix_info *md, void *data)
{
  move_mix_data *mmd = (move_mix_data *)data;
  mix_set_position_edit(md->id, syncd_mix_position(md->id) + mmd->beg);
  after_edit(md->cp);
  update_graph(md->cp);
}


static void syncd_mix_set_position(int mix_id, mus_long_t pos)
{
  move_mix_data *pos_data;
  pos_data = (move_mix_data *)malloc(sizeof(move_mix_data));
  pos_data->beg = pos;
  for_each_syncd_mix(mix_id, syncd_mix_set_position_1, pos_data);
  free(pos_data);
}


void finish_moving_mix_tag(int mix_id, int x)
{
  /* from mouse release after tag drag in snd-chn.c only */
  mix_info *md;
  mus_long_t pos;
  Xen res = Xen_false;
  chan_info *cp;

  mix_dragged = false;
  if (watch_mix_proc != 0) 
    {
      stop_watch_mix_proc();
      watch_mix_proc = 0;
    }

  md = md_from_id(mix_id);
  if (!md) return;
  cp = md->cp;
  
  pos = snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)(snd_srate(cp->sound)));
  if (pos < 0) pos = 0;
  cp->hookable = hookable_before_drag;
  
  if (cp->edit_ctr > edpos_before_drag) /* possibly dragged it back to start point, so no edit took place */
    cp->edit_ctr--;
  keep_dragging_syncd_mixes(mix_id);    /* fixup edpos */

  if (Xen_hook_has_list(mix_release_hook))
    res = run_progn_hook(mix_release_hook,
			 Xen_list_2(new_xen_mix(mix_id),
				    C_llong_to_Xen_llong(pos - mix_position_from_id(mix_id))),
			 S_mix_release_hook);

  if (!(Xen_is_true(res)))
    {
      mus_long_t old_pos;
      old_pos = mix_position_from_id(mix_id);
      if (mix_set_position_edit(mix_id, pos))
	{
	  cursor_sample(cp) = pos;
	  after_edit(cp);
	  update_graph(cp); /* this causes flashing, but it's next to impossible to fix
			     *   display_channel_id assumes previous id was erased, as does any after_graph_hook function
			     *   and we have to run lisp/fft graphs in any case (and the hook),
			     *   but display_channel_data_1 erases the old graph, so it's hard to specialize for this case
			     */
	  syncd_mix_set_position(mix_id, pos - old_pos); /* assumes syncd_mixes list exists */
	}
    }
  stop_dragging_syncd_mixes(mix_id);
}


/* View:Mixes dialog display */

void mix_display_during_drag(int mix_id, mus_long_t drag_beg, mus_long_t drag_end)
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);

  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    display_channel_time_data(cp);
  else
    {
#if USE_MOTIF
      mus_long_t cur_end, ms_beg;
      ms_beg = mix_position_from_id(mix_id);
      cur_end = ms_beg + mix_length_from_id(mix_id);
      if (cur_end > drag_end)
	drag_end = cur_end;
      if (ms_beg < drag_beg)
	drag_beg = ms_beg;
      make_partial_graph(cp, drag_beg, drag_end);
      display_channel_mixes_with_bounds(cp, drag_beg, drag_end);
#else
      display_channel_data(cp);
#endif
    }
}



static Xen snd_no_such_mix_error(const char *caller, Xen n)
{
  Xen_error(Xen_make_error_type("no-such-mix"),
	    Xen_list_3(C_string_to_Xen_string("~A: no such mix, ~A"),
		       C_string_to_Xen_string(caller),
		       n));
  return(Xen_false);
}


void after_mix_edit(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if ((md) && (md->cp))
    {
      after_edit(md->cp);
      update_graph(md->cp);
    }
}




/* ---------------------------------------- syncd mixes ---------------------------------------- */

typedef struct {
  int mix_id, orig_edpos;
  mus_long_t orig_beg;
  chan_info *cp;
} syncd_mix_info;

static syncd_mix_info *syncd_mixes = NULL;
static int syncd_mixes_length = 0;


static mus_long_t syncd_mix_position(int id)
{
  if (syncd_mixes)
    {
      int i;
      for (i = 0; i < syncd_mixes_length; i++)
	if (id == syncd_mixes[i].mix_id)
	  return(syncd_mixes[i].orig_beg);
    }
  return(-1);
}


static void add_syncd_mix(mix_info *md, void *ignore)
{
  mus_long_t pos;
  int i, mix_id;

  mix_id = md->id;
  pos = mix_position_from_id(mix_id);

  i = syncd_mixes_length++;
  syncd_mixes[i].mix_id = mix_id;
  syncd_mixes[i].orig_beg = pos;
  syncd_mixes[i].orig_edpos = md->cp->edit_ctr;
  syncd_mixes[i].cp = md->cp;
}


static void count_syncd_mixes(mix_info *md, void *ignore)
{
  syncd_mixes_length++;
}


void start_dragging_syncd_mixes(int mix_id)
{
  syncd_mixes_length = 0;
  for_each_syncd_mix(mix_id, count_syncd_mixes, NULL);
  if (syncd_mixes_length > 0)
    {
      if (syncd_mixes) free(syncd_mixes);
      syncd_mixes = (syncd_mix_info *)calloc(syncd_mixes_length, sizeof(syncd_mix_info));
      syncd_mixes_length = 0;
      for_each_syncd_mix(mix_id, add_syncd_mix, NULL);
    }
}

void keep_dragging_syncd_mixes(int mix_id)
{
  int i;
  for (i = 0; i < syncd_mixes_length; i++)
    syncd_mixes[i].cp->edit_ctr = syncd_mixes[i].orig_edpos;
}

void stop_dragging_syncd_mixes(int mix_id)
{
  /* undo edit? */
  if (syncd_mixes)
    {
      free(syncd_mixes);
      syncd_mixes = NULL;
      syncd_mixes_length = 0;
    }
}
  

static void syncd_mix_change_position_1(mix_info *md, void *data)
{
  move_mix_data *mmd = (move_mix_data *)data;
  mix_set_position_edit(md->id, mix_position_from_id(md->id) + mmd->beg);
  if (!mmd->axis_changed)
    mix_display_during_drag(md->id, drag_beg, drag_end);
}


void syncd_mix_change_position(int mix_id, mus_long_t change)
{
  move_mix_data *pos_data;
  pos_data = (move_mix_data *)malloc(sizeof(move_mix_data));
  pos_data->beg = change;
  for_each_syncd_mix(mix_id, syncd_mix_change_position_1, pos_data);
  free(pos_data);
}


static int ms_chans = 0;
static chan_info **ms_cps = NULL;

static void update_syncd_chans(mix_info *md, void *ignore)
{
  int i;
  for (i = 0; i < ms_chans; i++)
    if (md->cp == ms_cps[i])
      return;
  ms_cps[ms_chans++] = md->cp;
  update_graph(md->cp);
}


void after_syncd_mix_edit(int id)
{
  ms_chans = active_channels(WITH_VIRTUAL_CHANNELS);
  if (ms_chans > 1)
    {
      ms_cps = (chan_info **)calloc(ms_chans, sizeof(chan_info *));
      ms_chans = 1;
      ms_cps[0] = mix_chan_info_from_id(id); /* base cp handled elsewhere */
      for_each_syncd_mix(id, update_syncd_chans, NULL);
      ms_chans = 0;
      free(ms_cps);
      ms_cps = NULL;
    }
}





/* ---------------------------------------- mix objects ---------------------------------------- */

typedef struct {
  int n;
} xen_mix;

/* md->cp->sounds[md->original_index] is the original mix data */


#define Xen_to_xen_mix(arg) ((xen_mix *)Xen_object_ref(arg))

int xen_mix_to_int(Xen n)
{
  xen_mix *mx;
  mx = Xen_to_xen_mix(n);
  return(mx->n);
}


static Xen_object_type_t xen_mix_tag;

bool xen_is_mix(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_mix_tag));
}


static void xen_mix_free(xen_mix *v) {if (v) free(v);}

Xen_wrap_free(xen_mix, free_xen_mix, xen_mix_free)


static char *xen_mix_to_string(xen_mix *v)
{
  #define xen_is_mixRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(xen_is_mixRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, xen_is_mixRINT_BUFFER_SIZE, "#<mix %d>", v->n);
  return(buf);
}

Xen_wrap_print(xen_mix, print_xen_mix, xen_mix_to_string)


#if HAVE_FORTH || HAVE_RUBY
static Xen g_xen_mix_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_mix_to_string "mix->string"

  Xen_check_type(xen_is_mix(obj), obj, 1, S_xen_mix_to_string, "a mix");

  vstr = xen_mix_to_string(Xen_to_xen_mix(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif

#if HAVE_SCHEME
static s7_pointer g_mix_methods = NULL;
static s7_pointer g_mix_sampler_methods = NULL;
static s7_pointer mix_to_let_func = NULL;
static s7_pointer mix_sampler_to_let_func = NULL;
#else

static bool xen_mix_equalp(xen_mix *v1, xen_mix *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static Xen equalp_xen_mix(Xen obj1, Xen obj2)
{
  if ((!(xen_is_mix(obj1))) || (!(xen_is_mix(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_mix_equalp(Xen_to_xen_mix(obj1), Xen_to_xen_mix(obj2))));
}
#endif


static xen_mix *xen_mix_make(int n)
{
  xen_mix *new_v;
  new_v = (xen_mix *)malloc(sizeof(xen_mix));
  new_v->n = n;
  return(new_v);
}


Xen new_xen_mix(int n)
{
  xen_mix *mx;
  if (n < 0)
    return(Xen_false);

  mx = xen_mix_make(n);
#if HAVE_SCHEME
  {
    s7_pointer m;
    m = Xen_make_object(xen_mix_tag, mx, 0, free_xen_mix);
    s7_c_object_set_let(m, g_mix_methods);
    return(m);
  }
#else
  return(Xen_make_object(xen_mix_tag, mx, 0, free_xen_mix));
#endif
}


#if HAVE_SCHEME
static Xen s7_xen_mix_length(s7_scheme *sc, s7_pointer obj)
{
  return(g_mix_length(obj));
}


static bool s7_xen_mix_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_mix *)obj1)->n == ((xen_mix *)obj2)->n));
}


static Xen s7_xen_mix_copy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj;
  obj = s7_car(args);
  return(new_xen_mix(copy_mix(Xen_mix_to_C_int(obj))));
}
#endif


static void init_xen_mix(void)
{
#if HAVE_SCHEME
  g_mix_methods = s7_openlet(s7, s7_inlet(s7, s7_list(s7, 2, s7_make_symbol(s7, "object->let"), mix_to_let_func)));
  s7_gc_protect(s7, g_mix_methods);
  xen_mix_tag = s7_make_c_type(s7, "<mix>");
  s7_c_type_set_print(s7, xen_mix_tag, print_xen_mix);
  s7_c_type_set_free(s7, xen_mix_tag, free_xen_mix);
  s7_c_type_set_equal(s7, xen_mix_tag, s7_xen_mix_equalp);
  s7_c_type_set_length(s7, xen_mix_tag, s7_xen_mix_length);
  s7_c_type_set_copy(s7, xen_mix_tag, s7_xen_mix_copy);
#else
#if HAVE_RUBY
  xen_mix_tag = Xen_make_object_type("XenMix", sizeof(xen_mix));
#else
  xen_mix_tag = Xen_make_object_type("Mix", sizeof(xen_mix));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_mix_tag,   print_xen_mix);
  fth_set_object_dump(xen_mix_tag,      g_xen_mix_to_string);
  fth_set_object_equal(xen_mix_tag,     equalp_xen_mix);
  fth_set_object_length(xen_mix_tag,    g_mix_length);
  fth_set_object_free(xen_mix_tag,      free_xen_mix);
#endif

#if HAVE_RUBY
  rb_define_method(xen_mix_tag, "to_s",     Xen_procedure_cast print_xen_mix, 0);
  rb_define_method(xen_mix_tag, "eql?",     Xen_procedure_cast equalp_xen_mix, 1);
  rb_define_method(xen_mix_tag, "==",       Xen_procedure_cast equalp_xen_mix, 1); 
  rb_define_method(xen_mix_tag, "length",   Xen_procedure_cast g_mix_length, 0);
  rb_define_method(xen_mix_tag, "to_str",   Xen_procedure_cast g_xen_mix_to_string, 0);
#endif
}
/* -------------------------------------------------------------------------------- */


static Xen g_integer_to_mix(Xen n)
{
  #define H_integer_to_mix "(" S_integer_to_mix " n) returns a mix object corresponding to the given integer"
  Xen_check_type(Xen_is_integer(n), n, 1, S_integer_to_mix, "an integer");
  if (mix_is_active(Xen_integer_to_C_int(n)))
    return(new_xen_mix(Xen_integer_to_C_int(n)));
  return(Xen_false);
}


static Xen g_mix_to_integer(Xen n)
{
  #define H_mix_to_integer "(" S_mix_to_integer " id) returns the integer corresponding to the given mix object"
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_to_integer, "a mix");
  return(C_int_to_Xen_integer(xen_mix_to_int(n)));
}


Xen g_mix_length(Xen n) 
{
  #define H_mix_length "(" S_mix_length " id): mix's length in samples"
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_length, "a mix");
  id = Xen_mix_to_C_int(n);
  if (mix_is_active(id))
    return(C_llong_to_Xen_llong(mix_length_from_id(id)));
  return(snd_no_such_mix_error(S_mix_length, n));
}


static Xen g_mix_position(Xen n) 
{
  int id;
  #define H_mix_position "(" S_mix_position " id): mix's begin time in the output in samples"
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_position, "a mix");
  id = Xen_mix_to_C_int(n);
  if (mix_is_active(id))
    return(C_llong_to_Xen_llong(mix_position_from_id(id)));
  return(snd_no_such_mix_error(S_mix_position, n));
}


static Xen g_set_mix_position(Xen n, Xen pos) 
{
  int id;
  mus_long_t beg;
  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_position, "a mix");
  Xen_check_type(Xen_is_llong(pos), pos, 2, S_set S_mix_position, "an integer");

  id = Xen_mix_to_C_int(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_set S_mix_position, n));

  beg = beg_to_sample(pos, S_set S_mix_position);
  if (mix_set_position_edit(id, beg))
    after_mix_edit(id);
  return(pos);
}


static Xen g_mix_amp(Xen n) 
{
  #define H_mix_amp "(" S_mix_amp " id): mix's scaler"
  int id;

  Xen_check_type(xen_is_mix(n), n, 1, S_mix_amp, "a mix");
  id = Xen_mix_to_C_int(n);

  if (mix_is_active(id))
    return(C_double_to_Xen_real(mix_amp_from_id(id)));
  return(snd_no_such_mix_error(S_mix_amp, n));
}


static Xen g_set_mix_amp(Xen n, Xen uval) 
{
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_amp, "a mix");
  Xen_check_type(Xen_is_number(uval), uval, 2, S_set S_mix_amp, "a number");

  id = Xen_mix_to_C_int(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_set S_mix_amp, n));

  if (mix_set_amp_edit(id, Xen_real_to_C_double(uval)))
    after_mix_edit(id);

  return(uval);
}


static Xen g_mix_amp_env(Xen n) 
{
  #define H_mix_amp_env "(" S_mix_amp_env " id): amplitude envelope applied to mix"
  int id;

  Xen_check_type(xen_is_mix(n), n, 1, S_mix_amp_env, "a mix");
  id = Xen_mix_to_C_int(n);

  if (mix_is_active(id))
    return(env_to_xen(mix_amp_env_from_id(id)));
  return(snd_no_such_mix_error(S_mix_amp_env, n));
}


static Xen g_set_mix_amp_env(Xen n, Xen val) 
{
  env *e = NULL;
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_amp_env, "a mix");
  Xen_check_type(Xen_is_list(val) || Xen_is_false(val), val, 2, S_set S_mix_amp_env, "a list or " PROC_FALSE);

  id = Xen_mix_to_C_int(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_set S_mix_amp_env, n));  

  if (Xen_is_list(val))
    e = get_env(val, S_set S_mix_amp_env);

  if (mix_set_amp_env_edit(id, e))
    after_mix_edit(id);

  /* e is copied by mix_set_amp_env_edit, and created by get_env (xen_to_env), so it should be freed here */
  if (e) free_env(e);
  return(val);
}


static Xen g_mix_speed(Xen n) 
{
  #define H_mix_speed "(" S_mix_speed " id): mix's resampling ratio"
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_speed, "a mix");
  id = Xen_mix_to_C_int(n);
  if (mix_is_active(id))
    return(C_double_to_Xen_real(mix_speed_from_id(id)));
  return(snd_no_such_mix_error(S_mix_speed, n));
}


static Xen g_set_mix_speed(Xen n, Xen uval) 
{
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_speed, "a mix");
  Xen_check_type(Xen_is_number(uval), uval, 2, S_set S_mix_speed, "a number");

  id = Xen_mix_to_C_int(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_set S_mix_speed, n));  

  if (mix_set_speed_edit(id, Xen_real_to_C_double(uval)))
    after_mix_edit(id);

  return(uval);
}


static Xen g_mix_name(Xen n) 
{
  #define H_mix_name "(" S_mix_name " id): name of mix"
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_name, "a mix");
  id = Xen_mix_to_C_int(n);
  if (mix_exists(id))
    return(C_string_to_Xen_string(mix_name_from_id(id)));
  return(snd_no_such_mix_error(S_mix_name, n));
}


static Xen g_set_mix_name(Xen n, Xen val) 
{
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_name, "a mix");
  Xen_check_type(Xen_is_string(val) || Xen_is_false(val), val, 2, S_set S_mix_name, "a string");
  id = Xen_mix_to_C_int(n);
  if (mix_exists(id))
    {
      mix_set_name_from_id(id, (Xen_is_string(val) ? Xen_string_to_C_string(val) : NULL));
      update_graph(mix_chan_info_from_id(id));
    }
  else return(snd_no_such_mix_error(S_set S_mix_name, n));
  return(val);
}


Xen g_mix_sync(Xen n) 
{
  #define H_mix_sync "(" S_mix_sync " id): mix sync field (an integer)"
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_sync, "a mix");
  id = Xen_mix_to_C_int(n);
  if (mix_exists(id))
    return(C_int_to_Xen_integer(mix_sync_from_id(id)));
  return(snd_no_such_mix_error(S_mix_sync, n));
}


Xen g_set_mix_sync(Xen n, Xen val) 
{
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_sync, "a mix");
  Xen_check_type(Xen_is_integer(val), val, 2, S_set S_mix_sync, "an integer");
  id = Xen_mix_to_C_int(n);
  if (mix_exists(id))
    mix_set_sync_from_id(id, Xen_integer_to_C_int(val));
  else return(snd_no_such_mix_error(S_set S_mix_sync, n));
  return(val);
}


static Xen g_mix_sync_max(void) 
{
  #define H_mix_sync_max "(" S_mix_sync_max "): max mix sync value seen so far"
  return(C_int_to_Xen_integer(current_mix_sync_max));
}


static Xen g_mix_tag_y(Xen n) 
{
  #define H_mix_tag_y "(" S_mix_tag_y " id): height of mix's tag"
  int id;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_tag_y, "a mix");
  id = Xen_mix_to_C_int(n);
  if (mix_exists(id))
    return(C_int_to_Xen_integer(mix_tag_y_from_id(id)));
  return(snd_no_such_mix_error(S_mix_tag_y, n));
}


static Xen g_set_mix_tag_y(Xen n, Xen val) 
{
  int id;

  Xen_check_type(xen_is_mix(n), n, 1, S_set S_mix_tag_y, "a mix");
  Xen_check_type(Xen_is_integer(val), val, 2, S_set S_mix_tag_y, "an integer");
  id = Xen_mix_to_C_int(n);

  if (mix_exists(id))
    {
      chan_info *cp;
      mix_state *ms;
      mix_info *md;

      md = md_from_id(id);
      ms = current_mix_state(md);
      cp = md->cp;
      if ((cp) &&
	  (!(cp->squelch_update)))
	{
	  graphics_context *ax;
	  ax = erase_context(cp);
#if USE_GTK
	  ss->cr = make_cairo(ax->wn);
#endif
	  erase_mix_tag_and_waveform(ms, cp, cp->axis, ax, md->x, cp->axis->y_offset + md->tag_y + MIX_TAG_Y_OFFSET);
	  md->tag_y = Xen_integer_to_C_int(val);
	  display_one_mix(ms, cp);
#if USE_GTK
	  free_cairo(ss->cr);
	  ss->cr = NULL;
	  copy_context(cp);
#endif
	}
      else md->tag_y = Xen_integer_to_C_int(val);
    }
  else return(snd_no_such_mix_error(S_set S_mix_tag_y, n));

  return(val);
}


static Xen g_mix_properties(Xen n)
{
  #define H_mix_properties "(" S_mix_properties " id):  A property list associated with the given mix."

  mix_info *md;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_properties, "a mix");

  md = md_from_id(Xen_mix_to_C_int(n));
  if (!md)
    return(snd_no_such_mix_error(S_mix_properties, n));

  if (!(Xen_is_vector(md->properties)))
    {
      md->properties = Xen_make_vector(1, Xen_empty_list);
      md->properties_gc_loc = snd_protect(md->properties);
    }
  return(Xen_vector_ref(md->properties, 0));
}


static Xen g_set_mix_properties(Xen n, Xen val)
{
  mix_info *md;
  Xen_check_type(xen_is_mix(n), n, 1, S_mix_properties, "a mix");

  md = md_from_id(Xen_mix_to_C_int(n));
  if (!md)
    return(snd_no_such_mix_error(S_set S_mix_properties, n));

  if (!(Xen_is_vector(md->properties)))
    {
      md->properties = Xen_make_vector(1, Xen_empty_list);
      md->properties_gc_loc = snd_protect(md->properties);
    }
  Xen_vector_set(md->properties, 0, val);
  return(Xen_vector_ref(md->properties, 0));
}


static Xen g_mix_property(Xen key, Xen id) 
{
  #define H_mix_property "(" S_mix_property " key id) returns the value associated with 'key' in the given mix's property list, or " PROC_FALSE "."
  Xen_check_type(xen_is_mix(id), id, 2, S_mix_property, "a mix");
  return(Xen_assoc_ref(key, g_mix_properties(id)));
}

static Xen g_set_mix_property(Xen key, Xen id, Xen val) 
{
  Xen_check_type(xen_is_mix(id), id, 2, S_mix_property, "a mix");
  g_set_mix_properties(id, Xen_assoc_set(key, val, g_mix_properties(id)));
  return(val);
}


static Xen g_mix_home(Xen n) 
{
  #define H_mix_home "(" S_mix_home " id): list of sound index and channel for the output of the mix, and the \
filename or " PROC_FALSE " and the input channel for its data."
  mix_info *md; 

  Xen_check_type(xen_is_mix(n), n, 1, S_mix_home, "a mix");
  md = md_from_id(Xen_mix_to_C_int(n));
  if (!md)
    return(snd_no_such_mix_error(S_mix_home, n));

  return(Xen_list_4(C_int_to_Xen_sound((md->cp->sound)->index),
		    C_int_to_Xen_integer((md->cp->chan)),
		    (md->in_filename) ? C_string_to_Xen_string(md->in_filename) : Xen_false,
		    C_int_to_Xen_integer(md->in_chan)));
}


void color_mixes(color_t color)
{
  int i;
  set_mix_color(color);
  for (i = 0; i < mix_infos_ctr; i++)
    if (mix_infos[i])
      {
	mix_infos[i]->color = color;
	mix_infos[i]->original_color = color;
      }
}


static double mix_maxamp(int mix_id)
{
  mix_info *md;
  mix_state *ms;
  snd_fd *sf;
  mus_long_t n;
  mus_float_t mx = 0.0;

  md = md_from_id(mix_id);
  if (!md)
    return(0.0);

  ms = current_mix_state(md);
  sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, 1.0, READ_FORWARD);
  for (n = 0; n < ms->len; n++)
    {
      mus_float_t val;
      val = fabs(read_sample(sf));
      if (val > mx) mx = val;
    }
  free_snd_fd(sf);
  return(mx);
}


Xen g_mix_maxamp(Xen mix_id)
{
  return(C_double_to_Xen_real(mix_maxamp(Xen_mix_to_C_int(mix_id))));
}


/* mix-related globals */

static void update_mix_waveforms(chan_info *cp)
{
  if ((cp) && (channel_has_active_mixes(cp)))
    update_graph(cp);
}


void set_mix_waveform_height(int new_val)
{
  in_set_mix_waveform_height(new_val);
  for_each_normal_chan(update_mix_waveforms);
}


static Xen g_mix_waveform_height(void) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height "): max height (pixels) of mix waveforms (20)"
  return(C_int_to_Xen_integer(mix_waveform_height(ss)));
}


static Xen g_set_mix_waveform_height(Xen val) 
{
  int new_val;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mix_waveform_height, "an integer"); 
  new_val = mus_iclamp(0, Xen_integer_to_C_int(val), LOTSA_PIXELS);
  set_mix_waveform_height(new_val);
  return(C_int_to_Xen_integer(mix_waveform_height(ss)));
}


static Xen g_with_mix_tags(void) 
{
  #define H_with_mix_tags "(" S_with_mix_tags "): " PROC_TRUE " if Snd should try to use virtual (tagged) mixing"
  return(C_bool_to_Xen_boolean(with_mix_tags(ss)));
}


static Xen g_set_with_mix_tags(Xen val) 
{
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_with_mix_tags, "a boolean");
  set_with_mix_tags(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(with_mix_tags(ss)));
}


static Xen g_mix_tag_width(void) 
{
  #define H_mix_tag_width "(" S_mix_tag_width "): width (pixels) of mix tags (6)"
  return(C_int_to_Xen_integer(mix_tag_width(ss)));
}


static Xen g_set_mix_tag_width(Xen val) 
{
  int width;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mix_tag_width, "an integer"); 
  width = mus_iclamp(0, Xen_integer_to_C_int(val), LOTSA_PIXELS);
  set_mix_tag_width(width);
  for_each_normal_chan(update_graph);
  return(C_int_to_Xen_integer(mix_tag_width(ss)));
}


static Xen g_mix_tag_height(void) 
{
  #define H_mix_tag_height "(" S_mix_tag_height "): height (pixels) of mix tags (14)"
  return(C_int_to_Xen_integer(mix_tag_height(ss)));
}


static Xen g_set_mix_tag_height(Xen val) 
{
  int height;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_mix_tag_height, "an integer"); 
  height = mus_iclamp(0, Xen_integer_to_C_int(val), LOTSA_PIXELS);
  set_mix_tag_height(height);
  for_each_normal_chan(update_graph);
  return(C_int_to_Xen_integer(mix_tag_height(ss)));
}


static Xen g_is_mix(Xen n) 
{
  #define H_is_mix "(" S_is_mix " id): returns " PROC_TRUE " if the 'n' is a mix that exists somewhere in the edit list."
  return(C_bool_to_Xen_boolean((xen_is_mix(n)) && (mix_exists(Xen_mix_to_C_int(n)))));
}


static Xen g_mixes(Xen snd, Xen chn)
{
  #define H_mixes "(" S_mixes " :optional snd chn): list of active mixes (ids) associated with snd and chn"
  snd_info *sp;
  Xen res1 = Xen_empty_list;
  
  Snd_assert_channel(S_mixes, snd, chn, 0);

  if (Xen_is_integer(snd) || xen_is_sound(snd))
    {
      int i;
      if (Xen_is_integer(chn))
	{
	  /* scan all mixes for any associated with this channel */
	  chan_info *cp;
	  cp = get_cp(snd, chn, S_mixes);
	  if (!cp) return(Xen_false);
	  for (i = mix_infos_ctr - 1; i >= 0; i--)
	    if ((mix_is_active(i)) &&
		(mix_infos[i]->cp == cp))
	      res1 = Xen_cons(new_xen_mix(i), res1);
	}
      else
	{
	  sp = get_sp(snd);
	  if (!sp) 
	    return(snd_no_such_sound_error(S_mixes, snd));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res1 = Xen_cons(g_mixes(snd, C_int_to_Xen_integer(i)), res1);
	}
    }
  else
    {
      int j;
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res1 = Xen_cons(g_mixes(C_int_to_Xen_integer(j), 
				    Xen_undefined), 
			    res1);
	}
    }
  return(res1);
}



static Xen g_mix_vct(Xen obj, Xen beg, Xen snd, Xen chn, Xen with_tag, Xen origin)
{
  #define H_mix_vct "(" S_mix_vct " data :optional (beg 0) snd chn (with-tag " S_with_mix_tags ") origin): \
mix data (a " S_vct ") into snd's channel chn starting at beg; return the new mix id, if any"

  vct *v;
  mus_long_t bg;
  chan_info *cp;
  const char *edname = NULL;
  mus_float_t *data;
  int len, mix_id = NO_MIX_TAG;
  bool with_mixer;

  Xen_check_type(mus_is_vct(obj), obj, 1, S_mix_vct, "a " S_vct);
  Snd_assert_channel(S_mix_vct, snd, chn, 3);
  Xen_check_type(Xen_is_integer_or_unbound(beg), beg, 2, S_mix_vct, "an integer");
  Xen_check_type(Xen_is_boolean_or_unbound(with_tag), with_tag, 5, S_mix_vct, "a boolean");
  Xen_check_type(Xen_is_string_or_unbound(origin), origin, 6, S_mix_vct, "a string");

  cp = get_cp(snd, chn, S_mix_vct);
  if (!cp) return(Xen_false);
  if (!(is_editable(cp))) return(Xen_false);

  if (Xen_is_string(origin))
    edname = Xen_string_to_C_string(origin);
  else edname = S_mix_vct;

  bg = beg_to_sample(beg, S_mix_vct);
  v = Xen_to_vct(obj);
  len = mus_vct_length(v);

  with_mixer = virtual_mix_ok(cp, cp->edit_ctr);
  if (with_mixer)
    {
      if (!Xen_is_bound(with_tag))
	with_mixer = with_mix_tags(ss);
      else with_mixer = Xen_boolean_to_C_bool(with_tag);
    }

  if (!with_mixer)
    return(C_bool_to_Xen_boolean(mix_vct_untagged(v, cp, bg, edname)));

  data = mus_vct_data(v);
  
  /* make_snd_data_buffer copies the data array, so we don't need GC protection */
  /*    we can't use v->data directly because the user might change it */
  {
    char *new_origin;
    /* (mix-vct (vct .1 .2 .3) 100 0 0 #t "mix-vct (vct .1 .2 .3)") */

#if HAVE_FORTH
    /* vct( 0.1 0.2 0.3 ) 100 snd chn #t "vct( 0.1 0.2, 0.3 )" mix-vct */ 
    { 
      if (edname && *edname) 
	{ 
	  char *name; 
	  if ((name = strrchr(edname, ' '))) 
	    name++; 
	  else 
	    name = S_mix_vct; 
	  new_origin = mus_format("%.*s %" PRId64 " snd chn %s to -mix-%d", 
				  (int)(strlen(edname) - strlen(name) - 1), edname, 
				  bg, name, mix_infos_ctr); 
	} 
      else new_origin = mus_format("vct( 0 ) %" PRId64 " snd chn %s to -mix-%d", bg, S_mix_vct, mix_infos_ctr); 
    } 
#endif
#if HAVE_SCHEME
    new_origin = mus_format("(varlet -env- '-mix-%d (%s %" PRId64 " snd chn))", mix_infos_ctr, edname, bg);
#endif
#if HAVE_RUBY
    /* mix_vct(vct(0.1, 0.2, 0.3), 100, snd, chn, true, "mix_vct(vct(0.1, 0.2, 0.3)") */ 
    new_origin = mus_format("_mix_%d = %s, %" PRId64 ", snd, chn)", mix_infos_ctr, edname, bg); 
#endif

    mix_id = mix_buffer_with_tag(cp, data, bg, len, new_origin); 
    free(new_origin);
  }

  update_graph(cp);

  return(new_xen_mix(mix_id));
}


static Xen g_mix(Xen file, Xen chn_samp_n, Xen file_chn, Xen snd_n, Xen chn_n, Xen tag, Xen auto_delete)
{
  #define H_mix "(" S_mix " file :optional (beg 0) (in-chan 0) snd chn (with-tag " S_with_mix_tags ") auto-delete): \
mix channel in-chan of file into snd's channel chn starting at beg (in the output), returning a list of the new mixes.  \
If in-chan is " PROC_TRUE ", all input channels are mixed into successive channels of snd, starting at chn. \
if with-tag is " PROC_FALSE ", no draggable tag is created.  If \
auto-delete is " PROC_TRUE ", the input file is deleted when it is no longer needed."

  chan_info *cp = NULL;
  static char *name = NULL;
  int chans, id = NO_MIX_TAG, file_channel = 0;
  bool with_mixer;
  mus_long_t beg = 0, len = 0;

  /* it might be nice to make mix generic: vct=mix-vct, region=mix-region, sound-data=mix-sound-data
   *        also mix-sound|mix-channel (sound object/int), but arg order is confusing (file-chn...)
   *        and mix-vct has "origin", also file_chn might be env: pan-mix-* [vector list?]
   *
   * mix-vct origin arg is not used (externally) except as a comment
   *
   * mix object :channel :out-channel :start (:end?) :with-tag :auto-delete (:edit-position?) (:channels?) :origin
   *   or
   * mix object :start :end :channel :edit-position :out-channel :with-tag :auto-delete :origin ?
   * mix in-object out-object :start :end :channel :edit-position :out-channel :with-tag :auto-delete :origin ?
   *   from
   * play object :start :end :channel :edit-position :out-channel :with-sync :wait :stop): 
   * save_sound_as :file :sound :srate :sample-type :header-type :channel :edit-position :comment): 
   */

  Xen_check_type(Xen_is_string(file), file, 1, S_mix, "a string");
  Xen_check_type(Xen_is_number_or_unbound(chn_samp_n), chn_samp_n, 2, S_mix, "an integer");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(file_chn), file_chn, 3, S_mix, "an integer or " PROC_TRUE);
  Snd_assert_channel(S_mix, snd_n, chn_n, 4);
  Xen_check_type(Xen_is_boolean_or_unbound(tag), tag, 6, S_mix, "a boolean");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(auto_delete), auto_delete, 7, S_mix, "a boolean or an integer");
  if (name) free(name);

  name = mus_expand_filename(Xen_string_to_C_string(file));
  if (!(mus_file_probe(name)))
    return(snd_no_such_file_error(S_mix, file));

  cp = get_cp(snd_n, chn_n, S_mix);
  if (!cp) return(Xen_false);

  if (Xen_is_llong(chn_samp_n))
    {
      beg = Xen_llong_to_C_llong(chn_samp_n);
      if ((beg < 0) ||
	  (beg > 4294967296LL))
	Xen_out_of_range_error(S_mix, 2, chn_samp_n, "begin time should be reasonable");
    }
  chans = mus_sound_chans(name);
  if (chans <= 0)
    {
      Xen_error(BAD_HEADER,
		Xen_list_3(C_string_to_Xen_string(S_mix ": ~S chans <= 0? (~A)"),
			   file,
			   C_int_to_Xen_integer(chans)));
    }
  if (Xen_is_integer(file_chn))
    {
      file_channel = Xen_integer_to_C_int(file_chn);
      if ((file_channel >= chans) ||
	  (file_channel < 0))
	{
	  Xen_error(NO_SUCH_CHANNEL,
		    Xen_list_4(C_string_to_Xen_string(S_mix ": chan: ~A, but ~S chans: ~A"),
			       file_chn,
			       file,
			       C_int_to_Xen_integer(chans)));
	}
    }
  else
    {
      if (Xen_is_true(file_chn)) /* this used to be the default as well -- !Xen_is_bound case */
	{
	  int i, out_chans = 1;
	  Xen result = Xen_empty_list;
	  file_delete_t delete_choice = DONT_DELETE_ME;

	  if (Xen_is_integer(auto_delete))
	    delete_choice = (file_delete_t)Xen_integer_to_C_int(auto_delete);
	  else
	    {
	      if (Xen_is_boolean(auto_delete))
		{
		  if (Xen_boolean_to_C_bool(auto_delete))
		    {
		      if ((chans > 1) &&
			  (cp->sound->nchans > 1)) /* mix_complete_file sets sync locally so all we care about here is nchans */
			{
			  remember_temp(name, chans);
			  delete_choice = MULTICHANNEL_DELETION;
			}
		      else delete_choice = DELETE_ME;
		    }
		  else delete_choice = DONT_DELETE_ME;
		}
	    }

	  id = mix_complete_file(cp->sound, beg, name, 
				 (!Xen_is_bound(tag)) ? with_mix_tags(ss) : Xen_boolean_to_C_bool(tag),
				 delete_choice, MIX_SETS_SYNC_LOCALLY,
				 &out_chans);
	  if (id == -1) return(Xen_false);

	  for (i = 0; i < out_chans; i++)
	    result = Xen_cons(new_xen_mix(id + i), result);
	  return(Xen_list_reverse(result));
	}
    }

  len = mus_sound_framples(name);
  if (len <= 0) return(Xen_false);

  with_mixer = virtual_mix_ok(cp, cp->edit_ctr);
  if (with_mixer)
    {
      if (!Xen_is_bound(tag))
	with_mixer = with_mix_tags(ss);
      else with_mixer = Xen_boolean_to_C_bool(tag);
    }

  {
    file_delete_t delete_file;

    delete_file = xen_to_file_delete_t(auto_delete, S_mix);
    if ((delete_file == MULTICHANNEL_DELETION) || (delete_file == MULTICHANNEL_DELETION_IF_FILE))
      remember_temp(name, chans);

    if (!with_mixer)
      {
	char *origin;
	origin = untagged_mix_to_string(name, beg, file_channel, delete_file == DELETE_ME);
	mix_file_untagged(name, file_channel, cp, beg, len, delete_file, origin);
	free(origin);
      }
    else 
      {
	char *origin;
	origin = tagged_mix_to_string(name, beg, file_channel, delete_file == DELETE_ME);

	if (len < FILE_BUFFER_SIZE)
	  {
	    mus_float_t *data;

	    data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
	    len = mus_file_to_array(name, file_channel, 0, len, data); 
	    if (len > 0)
	      id = mix_buffer_with_tag(cp, data, beg, len, origin);
	    free(data);

	    if (delete_file == DELETE_ME)
	      snd_remove(name, REMOVE_FROM_CACHE);
	    else
	      {
		if (delete_file == MULTICHANNEL_DELETION_IF_FILE)
		  forget_temp(name, file_channel);
	      }
	  }
	else 
	  {
	    id = mix_file_with_tag(cp, name, file_channel, beg, delete_file, origin);
	    if (mix_exists(id))   /* if edit is blocked, mix_file can return NO_MIX_TAG (-1) */
	      {
		mix_info *md;
		md = md_from_id(id);
		if (!md->in_filename)
		  {
		    md->in_filename = mus_strdup(name);
		    md->in_chan = file_channel;
		  }
	      }
	  }
	free(origin);
      }
  }

  update_graph(cp);

  if (id == NO_MIX_TAG) return(Xen_false);
  return(Xen_list_1(new_xen_mix(id)));
}



/* ---------------- mix samplers ---------------- */

typedef struct mix_fd {
  mix_info *md;
  snd_fd *sf;
} mix_fd;

static Xen_object_type_t mf_tag;

bool is_mix_sampler(Xen obj) 
{
  return(Xen_c_object_is_type(obj, mf_tag));
}

#define Xen_to_mix_sampler(obj) ((mix_fd *)Xen_object_ref(obj))


static Xen g_is_mix_sampler(Xen obj) 
{
  #define H_is_mix_sampler "(" S_is_mix_sampler " obj): " PROC_TRUE " if obj is a mix-sampler"
  return(C_bool_to_Xen_boolean(is_mix_sampler(obj)));
}


static char *mix_sampler_to_string(mix_fd *fd) 
{
  char *desc;
  desc = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  if ((!fd) || (!fd->sf))
    snprintf(desc, PRINT_BUFFER_SIZE, "#<mix-sampler: null>");
  else
    {
      if ((mix_is_active(fd->sf->region)) &&
	  (fd->md) &&
	  (fd->sf->region == (fd->md->id)))
	{
	  mix_info *md;
	  md = fd->md;
	  snprintf(desc, PRINT_BUFFER_SIZE, "#<mix-sampler mix %d, (from %" PRId64 ", at %" PRId64 "%s): %s>",
		       md->id,
		       fd->sf->initial_samp,
		       fd->sf->loc,
		       (fd->sf->at_eof) ? ", at eof" : "",
		       (md->in_filename) ? md->in_filename : S_vct);
	}
      else snprintf(desc, PRINT_BUFFER_SIZE, "#<mix-sampler: inactive>");
    }
  return(desc);
}

Xen_wrap_print(mix_fd, print_mf, mix_sampler_to_string)


static void mf_free(mix_fd *fd)
{
  if (fd) 
    {
      if (fd->sf)
	free_snd_fd(fd->sf);
      fd->sf = NULL;
      fd->md = NULL;
      free(fd);
    }
}


Xen_wrap_free(mix_fd, free_mf, mf_free)

#if HAVE_SCHEME
static bool s7_equalp_mf(void *m1, void *m2)
{
  return(m1 == m2);
}
#endif


Xen g_make_mix_sampler(Xen mix_id, Xen ubeg)
{
  #define H_make_mix_sampler "(" S_make_mix_sampler " id :optional (beg 0)): return a reader ready to access mix id"
  mix_info *md = NULL;
  mix_state *ms;
  mus_long_t beg;

  Xen_check_type(xen_is_mix(mix_id), mix_id, 1, S_make_mix_sampler, "a mix");
  Snd_assert_sample_type(S_make_mix_sampler, ubeg, 2);

  md = md_from_id(Xen_mix_to_C_int(mix_id));
  if (!md)
    return(snd_no_such_mix_error(S_make_mix_sampler, mix_id));
  beg = beg_to_sample(ubeg, S_make_mix_sampler);

  ms = current_mix_state(md);
  if (ms)
    {
      mix_fd *mf = NULL;
      mf = (mix_fd *)calloc(1, sizeof(mix_fd));
      mf->md = md;
      mf->sf = make_virtual_mix_reader(md->cp, beg, ms->len, ms->index, ms->scaler, READ_FORWARD);
      if (mf->sf)
	{
	  mf->sf->region = md->id;
#if HAVE_SCHEME
	  {
	    s7_pointer m;
	    m = Xen_make_object(mf_tag, mf, 0, free_mf);
	    s7_c_object_set_let(m, g_mix_sampler_methods);
	    return(m);
	  }
#else
	  return(Xen_make_object(mf_tag, mf, 0, free_mf));
#endif
	}
      free(mf);
    }
  return(Xen_false);
}


static Xen g_read_mix_sample(Xen obj)
{
  mix_fd *mf;
  #define H_read_mix_sample "(" S_read_mix_sample " reader): read sample from mix reader"
  Xen_check_type(is_mix_sampler(obj), obj, 1, S_read_mix_sample, "a mix-sampler");
  mf = Xen_to_mix_sampler(obj);
  return(C_double_to_Xen_real(read_sample(mf->sf)));
}


snd_fd *xen_mix_to_snd_fd(Xen obj)
{
  mix_fd *mf;
  mf = Xen_to_mix_sampler(obj);
  return(mf->sf);
}

snd_fd *mf_to_snd_fd(void *p) {return(((mix_fd *)p)->sf);}


#if HAVE_SCHEME
static Xen s7_read_mix_sample(s7_scheme *sc, Xen obj, Xen args)
{
  return(g_read_mix_sample(obj));
}
#endif


Xen g_copy_mix_sampler(Xen obj)
{
  mix_fd *mf;
  mf = Xen_to_mix_sampler(obj);
  return(g_make_mix_sampler(new_xen_mix(mf->md->id), C_llong_to_Xen_llong(current_location(mf->sf))));
}


Xen g_mix_sampler_home(Xen obj)
{
  mix_fd *mf;
  mf = Xen_to_mix_sampler(obj);
  return(new_xen_mix(mf->md->id));
}


static bool mix_sampler_is_at_end(void *ptr)
{
  mix_fd *mf = (mix_fd *)ptr;
  return(mf->sf->at_eof);
}


Xen g_mix_sampler_is_at_end(Xen obj)
{
  mix_fd *mf;
  mf = Xen_to_mix_sampler(obj);
  return(C_bool_to_Xen_boolean(mix_sampler_is_at_end(mf)));
}


Xen g_mix_sampler_position(Xen obj)
{
  mix_fd *mf;
  mf = Xen_to_mix_sampler(obj);
  if (mix_sampler_is_at_end(mf)) return(Xen_integer_zero);
  return(C_llong_to_Xen_llong(current_location(mf->sf)));
}


Xen g_free_mix_sampler(Xen obj)
{
  mix_fd *mf;
  mf = Xen_to_mix_sampler(obj);

  if (mf)
    {
      if (mf->sf)
	mf->sf = free_snd_fd(mf->sf);
      mf->md = NULL;
    }
  return(Xen_false);
}


static io_error_t save_mix(int id, const char *name, mus_header_t head_type, mus_sample_t samp_type)
{
  mix_info *md;
  chan_info *cp;
  snd_info *sp;
  mix_state *ms;
  io_error_t io_err;
  mus_long_t framples;

  md = md_from_id(id);
  cp = md->cp;
  sp = cp->sound;
  ms = current_mix_state(md);
  framples = ms->len;

  io_err = snd_write_header(name, head_type, snd_srate(sp), 1, framples, samp_type, NULL, NULL);

  if (io_err == IO_NO_ERROR)
    {
      mus_long_t oloc;
      int ofd;
      oloc = mus_header_data_location();
      ofd = snd_reopen_write(name);
      if (ofd != -1)
	{
	  mus_float_t **bufs;
	  mus_float_t *data;
	  mus_long_t i;
	  mix_fd *mf = NULL;

	  snd_file_open_descriptors(ofd, name, samp_type, oloc, 1, head_type);
	  mus_file_set_clipping(ofd, clipping(ss));
	  lseek(ofd, oloc, SEEK_SET);

	  mf = (mix_fd *)calloc(1, sizeof(mix_fd));
	  mf->md = md;
	  mf->sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, ms->scaler, READ_FORWARD);
	  mf->sf->region = md->id;

	  bufs = (mus_float_t **)calloc(1, sizeof(mus_float_t *));
	  bufs[0] = (mus_float_t *)calloc(FILE_BUFFER_SIZE, sizeof(mus_float_t));
	  data = bufs[0];

	  for (i = 0; i < framples; i += FILE_BUFFER_SIZE)
	    {
	      int err;
	      int cursamples, k;
	      if ((i + FILE_BUFFER_SIZE) < framples) 
		cursamples = FILE_BUFFER_SIZE; 
	      else cursamples = (framples - i);

	      for (k = 0; k < cursamples; k++)
		data[k] = read_sample(mf->sf);
	      err = mus_file_write(ofd, 0, cursamples - 1, 1, bufs);
	      if (err != MUS_NO_ERROR) 
		{
		  snd_warning("write error while saving mix");
		  break;
		}
	    }

	  free_snd_fd(mf->sf);
	  free(mf);
	  free(bufs[0]);
	  data = NULL;
	  free(bufs);

	  mus_file_close(ofd);
	}
      else snd_error("%s %d in %s: %s", S_save_mix, id, name, snd_io_strerror());
    }
  else snd_error("%s %d in %s: %s", S_save_mix, id, name, snd_io_strerror());
  return(io_err);
}

#define MIX_TAG_Y_SEPARATION 20

int copy_mix(int id)
{
  int new_id;
  mus_long_t pos;
  char *filename, *origin;
  mix_info *md, *new_md;

  md = md_from_id(id);
  if (!md) return(-1);

  filename = snd_tempnam();
  save_mix(id, filename, MUS_NEXT, MUS_OUT_SAMPLE_TYPE);

  pos = mix_position_from_id(id);
  origin = tagged_mix_to_string(filename, pos, 0, true); /* true = file should be auto-deleted, I think */

  new_id = mix_file_with_tag(md->cp, filename, 0, pos, DELETE_ME, origin);

  new_md = md_from_id(new_id);
  if (!new_md->in_filename)
    new_md->in_filename = mus_strdup(filename);

  new_md->tag_y = md->tag_y + MIX_TAG_Y_SEPARATION;

  free(origin);
  free(filename);
  return(new_id); 
}


static Xen g_save_mix(Xen m, Xen file)
{
  #define H_save_mix "(" S_save_mix " mix filename) saves mix's samples in the file 'filename'"
  int id;

  Xen_check_type(xen_is_mix(m), m, 1, S_save_mix, "a mix");
  id = Xen_mix_to_C_int(m);

  if (mix_is_active(id)) 
    {
      Xen_check_type(Xen_is_string(file), file, 2, S_save_mix, "a filename");
      save_mix(id, Xen_string_to_C_string(file), MUS_NEXT, MUS_OUT_SAMPLE_TYPE);
      return(m);
    }
  return(snd_no_such_mix_error(S_save_mix, m));
}


static Xen g_view_mixes_dialog(void)
{
  #define H_view_mixes_dialog "(" S_view_mixes_dialog "): start the Mix browser"
  return(Xen_wrap_widget(make_mix_dialog()));
}


static Xen g_mix_dialog_mix(void)
{
  #define H_mix_dialog_mix "(" S_mix_dialog_mix "): current mix id displayed in mix dialog."
  return(new_xen_mix(mix_dialog_mix()));
}


static Xen g_set_mix_dialog_mix(Xen val)
{
  Xen_check_type(xen_is_mix(val), val, 1, S_set S_mix_dialog_mix, "a mix");
  mix_dialog_set_mix(Xen_mix_to_C_int(val));
  return(val);
}


static bool play_mix(mix_info *md, mus_long_t beg, bool start_playing)
{
  mix_state *ms;
  ms = current_mix_state(md);
  if (!ms)
    {
      int i;
      for (i = md->cp->edit_ctr - 1; (!ms) && (i < 0); i--)
	ms = ed_mix_state(md->cp->edits[i], md->id);
    }
  if (ms)
    return(add_mix_to_play_list(ms, md->cp, beg, start_playing));
  return(false);
}


static void syncd_mix_play_1(mix_info *md, void *ignore)
{
  play_mix(md, 0, false);
}


void syncd_mix_play(int id)
{
  /* add any syncd mixes to the play list (started later) */
  for_each_syncd_mix(id, syncd_mix_play_1, NULL);
}


Xen g_play_mix(Xen num, mus_long_t samp)
{
  mix_info *md;

  md = md_from_id(Xen_mix_to_C_int(num));
  if (!md)
    return(snd_no_such_mix_error(S_play, num));
  play_mix(md, samp, true); 

  return(num);
}


bool play_mix_from_id(int mix_id) 
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(play_mix(md, 0, true));
  return(false);
}


#if HAVE_SCHEME
  #define S_mix_to_vct "mix->float-vector"
#else
  #define S_mix_to_vct "mix->vct"
#endif

Xen g_mix_to_vct(Xen mix_n, Xen beg_n, Xen num)
{
  mus_float_t *data;
  mix_info *md;
  mus_long_t beg, len;
  mus_long_t i;
  mix_fd *mf = NULL;
  mix_state *ms;

  Xen_check_type(xen_is_mix(mix_n), mix_n, 1, S_mix_to_vct, "a mix");
  Xen_check_type(Xen_is_integer_or_unbound(beg_n), beg_n, 2, S_mix_to_vct, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(num), num, 3, S_mix_to_vct, "an integer");

  md = md_from_id(Xen_mix_to_C_int(mix_n));
  if (!md)
    return(snd_no_such_mix_error(S_mix_to_vct, mix_n));
  ms = current_mix_state(md);

  if (Xen_is_integer(num)) 
    {
      len = Xen_llong_to_C_llong(num);
      if (len < 0)
	Xen_out_of_range_error(S_mix_to_vct, 2, num, "length < 0?");
      if (len > mix_length_from_id(md->id))
	len = mix_length_from_id(md->id);
    }
  else len = mix_length_from_id(md->id);

  beg = beg_to_sample(beg_n, S_mix_to_vct);
  if (beg >= len)
    return(Xen_false);

  mf = (mix_fd *)calloc(1, sizeof(mix_fd));
  mf->md = md;
  mf->sf = make_virtual_mix_reader(md->cp, beg, ms->len, ms->index, ms->scaler, READ_FORWARD);
  mf->sf->region = md->id;

  data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
  for (i = 0; i < len; i++)
    data[i] = read_sample(mf->sf);

  free_snd_fd(mf->sf);
  free(mf);

  return(xen_make_vct(len, data));
}


Xen_wrap_1_arg(g_mix_position_w, g_mix_position)
Xen_wrap_2_args(g_set_mix_position_w, g_set_mix_position)
Xen_wrap_1_arg(g_mix_speed_w, g_mix_speed)
Xen_wrap_2_args(g_set_mix_speed_w, g_set_mix_speed)
Xen_wrap_1_arg(g_mix_amp_w, g_mix_amp)
Xen_wrap_2_args(g_set_mix_amp_w, g_set_mix_amp)
Xen_wrap_1_arg(g_mix_amp_env_w, g_mix_amp_env)
Xen_wrap_2_args(g_set_mix_amp_env_w, g_set_mix_amp_env)

Xen_wrap_1_arg(g_mix_name_w, g_mix_name)
Xen_wrap_2_args(g_set_mix_name_w, g_set_mix_name)
Xen_wrap_1_arg(g_mix_tag_y_w, g_mix_tag_y)
Xen_wrap_2_args(g_set_mix_tag_y_w, g_set_mix_tag_y)
Xen_wrap_1_arg(g_mix_sync_w, g_mix_sync)
Xen_wrap_2_args(g_set_mix_sync_w, g_set_mix_sync)
Xen_wrap_no_args(g_mix_sync_max_w, g_mix_sync_max)
Xen_wrap_1_arg(g_mix_length_w, g_mix_length)
Xen_wrap_1_arg(g_integer_to_mix_w, g_integer_to_mix)
Xen_wrap_1_arg(g_mix_to_integer_w, g_mix_to_integer)
Xen_wrap_1_arg(g_mix_home_w, g_mix_home)
Xen_wrap_1_arg(g_mix_properties_w, g_mix_properties)
Xen_wrap_2_args(g_set_mix_properties_w, g_set_mix_properties)
Xen_wrap_2_args(g_mix_property_w, g_mix_property)
Xen_wrap_3_args(g_set_mix_property_w, g_set_mix_property)

Xen_wrap_no_args(g_mix_waveform_height_w, g_mix_waveform_height)
Xen_wrap_1_arg(g_set_mix_waveform_height_w, g_set_mix_waveform_height)
Xen_wrap_no_args(g_with_mix_tags_w, g_with_mix_tags)
Xen_wrap_1_arg(g_set_with_mix_tags_w, g_set_with_mix_tags)
Xen_wrap_no_args(g_mix_tag_width_w, g_mix_tag_width)
Xen_wrap_1_arg(g_set_mix_tag_width_w, g_set_mix_tag_width)
Xen_wrap_no_args(g_mix_tag_height_w, g_mix_tag_height)
Xen_wrap_1_arg(g_set_mix_tag_height_w, g_set_mix_tag_height)

Xen_wrap_1_arg(g_is_mix_w, g_is_mix)
Xen_wrap_2_optional_args(g_mixes_w, g_mixes)
Xen_wrap_7_optional_args(g_mix_w, g_mix)
Xen_wrap_6_optional_args(g_mix_vct_w, g_mix_vct)

Xen_wrap_2_optional_args(g_make_mix_sampler_w, g_make_mix_sampler)
Xen_wrap_1_arg(g_read_mix_sample_w, g_read_mix_sample)
Xen_wrap_1_arg(g_is_mix_sampler_w, g_is_mix_sampler)
Xen_wrap_2_args(g_save_mix_w, g_save_mix)

Xen_wrap_no_args(g_view_mixes_dialog_w, g_view_mixes_dialog)
Xen_wrap_no_args(g_mix_dialog_mix_w, g_mix_dialog_mix)
Xen_wrap_1_arg(g_set_mix_dialog_mix_w, g_set_mix_dialog_mix)

#if HAVE_SCHEME
static s7_pointer acc_mix_tag_height(s7_scheme *sc, s7_pointer args) {return(g_set_mix_tag_height(s7_cadr(args)));}
static s7_pointer acc_mix_tag_width(s7_scheme *sc, s7_pointer args) {return(g_set_mix_tag_width(s7_cadr(args)));}
static s7_pointer acc_mix_waveform_height(s7_scheme *sc, s7_pointer args) {return(g_set_mix_waveform_height(s7_cadr(args)));}
static s7_pointer acc_with_mix_tags(s7_scheme *sc, s7_pointer args) {return(g_set_with_mix_tags(s7_cadr(args)));}

static s7_pointer s7_mix_sampler_to_let(s7_scheme *sc, s7_pointer args)
{
  /* this is called upon (object->let <mix-sampler>) */
  s7_pointer m, env;
  mix_fd *fd;

  m = s7_car(args);
  env = s7_cadr(args);
  fd = Xen_to_mix_sampler(m);
  
  if ((fd) && (fd->sf) && 
      (mix_is_active(fd->sf->region)) &&
      (fd->md) &&
      (fd->sf->region == (fd->md->id)))
    {
      mix_info *md;
      md = fd->md;

      s7_varlet(sc, env, s7_make_symbol(sc, "mix"), (mix_is_active(md->id)) ? new_xen_mix(md->id) : Xen_false);
      s7_varlet(sc, env, s7_make_symbol(sc, "source"), (md->in_filename) ? s7_make_string(sc, md->in_filename) : Xen_false);
      s7_varlet(sc, env, s7_make_symbol(sc, "start"), s7_make_integer(sc, fd->sf->initial_samp));
      s7_varlet(sc, env, s7_make_symbol(sc, "position"), s7_make_integer(sc, fd->sf->loc));
      s7_varlet(sc, env, s7_make_symbol(sc, "eof"), s7_make_boolean(sc, fd->sf->at_eof));
    }
  return(env);
}

static s7_pointer s7_mix_to_let(s7_scheme *sc, s7_pointer args)
{
  /* this is called upon (object->let <mix>) */
  s7_pointer m, env;
  m = s7_car(args);
  env = s7_cadr(args);
  
  s7_varlet(sc, env, s7_make_symbol(sc, "position"), g_mix_position(m));
  return(env);
}
#endif

void g_init_mix(void)
{
#if HAVE_SCHEME
  s7_pointer i, m, b, s, p, t, f, ms, fv;
  i = s7_make_symbol(s7, "integer?");
  m = s7_make_symbol(s7, "mix?");
  ms = s7_make_symbol(s7, "mix-sampler?");
  b = s7_make_symbol(s7, "boolean?");
  s = s7_make_symbol(s7, "string?");
  p = s7_make_symbol(s7, "list?");
  f = s7_make_symbol(s7, "float?");
  fv = s7_make_symbol(s7, "float-vector?");
  t = s7_t(s7);

  mix_to_let_func = s7_make_function(s7, "mix->let", s7_mix_to_let, 2, 0, false, "mix->let");
  mix_sampler_to_let_func = s7_make_function(s7, "mix-sampler->let", s7_mix_sampler_to_let, 2, 0, false, "mix-sampler->let");
#endif

  init_xen_mix();

#if HAVE_SCHEME
  g_mix_sampler_methods = s7_openlet(s7, s7_inlet(s7, s7_list(s7, 2, s7_make_symbol(s7, "object->let"), mix_sampler_to_let_func)));
  s7_gc_protect(s7, g_mix_sampler_methods);
  mf_tag = s7_make_c_type(s7, "<mix-sampler>");
  s7_c_type_set_print(s7, mf_tag, print_mf);
  s7_c_type_set_free(s7, mf_tag, free_mf);
  s7_c_type_set_equal(s7, mf_tag, s7_equalp_mf);
  s7_c_type_set_apply(s7, mf_tag, s7_read_mix_sample);
#else
  mf_tag = Xen_make_object_type("MixSampler", sizeof(mix_fd));
#endif

#if HAVE_RUBY
  rb_define_method(mf_tag, "to_s", Xen_procedure_cast print_mf, 0);
  rb_define_method(mf_tag, "call", Xen_procedure_cast g_read_mix_sample, 0);
#endif

#if HAVE_FORTH
  fth_set_object_inspect(mf_tag, print_mf);
  fth_set_object_free(mf_tag, free_mf);
  fth_set_object_apply(mf_tag, Xen_procedure_cast g_read_mix_sample, 0, 0, 0);
#endif

  Xen_define_typed_procedure(S_make_mix_sampler,  g_make_mix_sampler_w,       1, 1, 0, H_make_mix_sampler, s7_make_signature(s7, 3, ms, m, i));
  Xen_define_typed_procedure(S_read_mix_sample,   g_read_mix_sample_w,        1, 0, 0, H_read_mix_sample,  s7_make_signature(s7, 2, f, ms));
  Xen_define_typed_procedure(S_is_mix_sampler,    g_is_mix_sampler_w,         1, 0, 0, H_is_mix_sampler,   s7_make_signature(s7, 2, b, t));

  Xen_define_typed_procedure(S_save_mix,          g_save_mix_w,               2, 0, 0, H_save_mix,         s7_make_signature(s7, 3, m, m, s));
  Xen_define_typed_procedure(S_mix,               g_mix_w,                    1, 6, 0, H_mix,              s7_make_signature(s7, 8, t, s, i, t, t, t, b, t));
  Xen_define_typed_procedure(S_mix_vct,           g_mix_vct_w,                1, 5, 0, H_mix_vct,          s7_make_signature(s7, 7, m, fv, i, t, t, b, s));

  Xen_define_typed_procedure(S_mixes,             g_mixes_w,                  0, 2, 0, H_mixes,            s7_make_signature(s7, 3, p, t, t));
  Xen_define_typed_procedure(S_mix_home,          g_mix_home_w,               1, 0, 0, H_mix_home,         s7_make_signature(s7, 2, p, m));
  Xen_define_typed_procedure(S_is_mix,            g_is_mix_w,                 1, 0, 0, H_is_mix,           s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_mix_length,        g_mix_length_w,             1, 0, 0, H_mix_length,       s7_make_signature(s7, 2, i, m));
  Xen_define_typed_procedure(S_integer_to_mix,    g_integer_to_mix_w,         1, 0, 0, H_integer_to_mix,   s7_make_signature(s7, 2, m, i));
  Xen_define_typed_procedure(S_mix_to_integer,    g_mix_to_integer_w,         1, 0, 0, H_mix_to_integer,   s7_make_signature(s7, 2, i, m));
  Xen_define_typed_procedure(S_view_mixes_dialog, g_view_mixes_dialog_w,      0, 0, 0, H_view_mixes_dialog, s7_make_signature(s7, 1, p));
  Xen_define_typed_procedure(S_mix_sync_max,      g_mix_sync_max_w,           0, 0, 0, H_mix_sync_max,     s7_make_signature(s7, 1, i));

  Xen_define_typed_dilambda(S_mix_position,   g_mix_position_w,   H_mix_position,   S_set S_mix_position,   g_set_mix_position_w,   1, 0, 2, 0,
			    s7_make_signature(s7, 2, i, m), s7_make_signature(s7, 3, i, m, i));
  Xen_define_typed_dilambda(S_mix_speed,      g_mix_speed_w,      H_mix_speed,      S_set S_mix_speed,      g_set_mix_speed_w,      1, 0, 2, 0,
			    s7_make_signature(s7, 2, f, m), s7_make_signature(s7, 3, f, m, f));
  Xen_define_typed_dilambda(S_mix_amp,        g_mix_amp_w,        H_mix_amp,        S_set S_mix_amp,        g_set_mix_amp_w,        1, 0, 2, 0,
			    s7_make_signature(s7, 2, f, m), s7_make_signature(s7, 3, f, m, f));
  Xen_define_typed_dilambda(S_mix_amp_env,    g_mix_amp_env_w,    H_mix_amp_env,    S_set S_mix_amp_env,    g_set_mix_amp_env_w,    1, 0, 2, 0,
			    s7_make_signature(s7, 2, p, m), s7_make_signature(s7, 3, p, m, s7_make_signature(s7, 2, p, b)));

  Xen_define_typed_dilambda(S_mix_name,       g_mix_name_w,       H_mix_name,       S_set S_mix_name,       g_set_mix_name_w,       1, 0, 2, 0,
			    s7_make_signature(s7, 2, s, m), s7_make_signature(s7, 3, s, m, s));
  Xen_define_typed_dilambda(S_mix_sync,       g_mix_sync_w,       H_mix_sync,       S_set S_mix_sync,       g_set_mix_sync_w,       1, 0, 2, 0,
			    s7_make_signature(s7, 2, i, m), s7_make_signature(s7, 3, i, m, s7_make_signature(s7, 2, i, b)));
  Xen_define_typed_dilambda(S_mix_properties, g_mix_properties_w, H_mix_properties, S_set S_mix_properties, g_set_mix_properties_w, 1, 0, 2, 0,
			    s7_make_signature(s7, 2, p, m), s7_make_signature(s7, 3, p, m, p));
  Xen_define_typed_dilambda(S_mix_property,   g_mix_property_w,   H_mix_property,   S_set S_mix_property,   g_set_mix_property_w,   2, 0, 3, 0,
			    s7_make_signature(s7, 3, t, t, m), s7_make_circular_signature(s7, 3, 4, t, t, m, t));
  Xen_define_typed_dilambda(S_mix_tag_y,      g_mix_tag_y_w,      H_mix_tag_y,      S_set S_mix_tag_y,      g_set_mix_tag_y_w,      1, 0, 2, 0,
			    s7_make_signature(s7, 2, i, m), s7_make_signature(s7, 3, i, m, i));

  Xen_define_typed_dilambda(S_mix_tag_width,  g_mix_tag_width_w,  H_mix_tag_width,  S_set S_mix_tag_width,  g_set_mix_tag_width_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));
  Xen_define_typed_dilambda(S_mix_tag_height, g_mix_tag_height_w, H_mix_tag_height, S_set S_mix_tag_height, g_set_mix_tag_height_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

  Xen_define_typed_dilambda(S_mix_waveform_height, g_mix_waveform_height_w, H_mix_waveform_height, S_set S_mix_waveform_height, g_set_mix_waveform_height_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));
  Xen_define_typed_dilambda(S_with_mix_tags, g_with_mix_tags_w, H_with_mix_tags, S_set S_with_mix_tags, g_set_with_mix_tags_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));
  Xen_define_typed_dilambda(S_mix_dialog_mix, g_mix_dialog_mix_w, H_mix_dialog_mix, S_set S_mix_dialog_mix, g_set_mix_dialog_mix_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, m), s7_make_signature(s7, 2, m, m));

  #define H_mix_release_hook S_mix_release_hook " (id samples): called after the mouse has dragged a mix to some new position. \
'samples' = samples moved in the course of the drag. If it returns " PROC_TRUE ", the actual remix is the hook's responsibility."

  mix_release_hook = Xen_define_hook(S_mix_release_hook, "(make-hook 'id 'samples)", 2, H_mix_release_hook);

  #define H_mix_drag_hook S_mix_drag_hook " (id x y): called when a mix is dragged"

  mix_drag_hook = Xen_define_hook(S_mix_drag_hook, "(make-hook 'id 'x 'y)", 3, H_mix_drag_hook); /* args = id, mouse x, mouse or tag y */

  /* the name draw-mix-hook is inconsistent with the other mix hooks (mix-draw-hook?), but is intended to parallel draw-mark-hook */
  #define H_draw_mix_hook S_draw_mix_hook " (id old-x old-y x y): called when a mix tag is about to be displayed"

  draw_mix_hook = Xen_define_hook(S_draw_mix_hook, "(make-hook 'id 'old-x 'old-y 'x 'y)", 5, H_draw_mix_hook);

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->mix_tag_height_symbol, s7_make_function(s7, "[acc-" S_mix_tag_height "]", acc_mix_tag_height, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->mix_tag_width_symbol, s7_make_function(s7, "[acc-" S_mix_tag_width "]", acc_mix_tag_width, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->mix_waveform_height_symbol, s7_make_function(s7, "[acc-" S_mix_waveform_height "]", acc_mix_waveform_height, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_mix_tags_symbol, s7_make_function(s7, "[acc-" S_with_mix_tags "]", acc_with_mix_tags, 2, 0, false, "accessor"));

  s7_symbol_set_documentation(s7, ss->mix_tag_height_symbol, "*mix-tag-height*: height (pixels) of mix tags (14)");
  s7_symbol_set_documentation(s7, ss->mix_tag_width_symbol, "*mix-tag-width*: width (pixels) of mix tags (6)");
  s7_symbol_set_documentation(s7, ss->mix_waveform_height_symbol, "*mix-waveform-height*: max height (pixels) of mix waveforms (20)");
  s7_symbol_set_documentation(s7, ss->with_mix_tags_symbol, "*with-mix-tags*: #t if Snd should try to use virtual (tagged) mixing");
#endif
}

