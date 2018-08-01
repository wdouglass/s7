#include "snd.h"
#include "sndlib-strings.h"
#include "clm2xen.h"


#define REGION_FILE 1
#define REGION_DEFERRED 0

/* region data can be stored either in a temp file that is deleted when the region is deleted (hence must be copied upon insert or mix)
 *    or as a descriptor of current chan/beg/num/edpos locs.  The descriptor form is used until some use is made of the data
 *    that requires a file anyway (e.g. mixing), or if the data the descriptor depends on is about to be flushed (e.g. the
 *    underlying edit list is about to be cleared, or the file is being closed, etc).
 */


#define CLEAR_REGION_DATA 0
#define COMPLETE_DELETION 1

static int region_id_ctr = 0;

typedef struct {
  int chans;
  mus_long_t len;
  chan_info **cps;
  int *edpos;
} deferred_region;


static deferred_region *free_deferred_region(deferred_region *dr)
{
  if (dr)
    {
      if (dr->cps) free(dr->cps);
      if (dr->edpos) free(dr->edpos);
      free(dr);
    }
  return(NULL);
}


typedef struct region {
  int chans;
  mus_long_t framples;
  int srate;                /* for file save (i.e. region->file) */
  int header_type;          /* for file save */
  snd_info *rsp;
  char *name, *start, *end; /* for region browser */
  char *filename;           /* if region data is stored in a temp file */
  int use_temp_file;        /* REGION_FILE = in temp file 'filename', REGION_DEFERRED = in 'dr' */
  mus_float_t maxamp;
  mus_long_t maxamp_position;
  snd_info *editor_copy;
  char *editor_name;
  int id;
  deferred_region *dr;      /* REGION_DEFERRED descriptor */
  peak_env_info **peak_envs;
  mus_long_t *begs, *ends;
} region;


static void deferred_region_to_temp_file(region *r);

static void free_region(region *r, int complete)
{
  /* if not complete, just clear out old data (edited region being saved) */
  if (r)
    {
      if (complete == COMPLETE_DELETION)
	{
	  if (r->editor_copy)
	    {
	      snd_info *sp;
	      sp = r->editor_copy; 
	      sp->edited_region = NULL;
	      r->editor_copy = NULL;
	    }
	  if (r->name) free(r->name);
	  if (r->start) free(r->start);
	  if (r->end) free(r->end);
	  if (r->begs) free(r->begs);
	  if (r->ends) free(r->ends);
	  if (r->peak_envs)
	    {
	      int i;
	      for (i = 0; i < r->chans; i++)
		if (r->peak_envs[i]) 
		  r->peak_envs[i] = free_peak_env_info(r->peak_envs[i]);
	      free(r->peak_envs);
	      r->peak_envs = NULL;
	    }
	}
      if (r->use_temp_file == REGION_FILE) /* we can delete this temp file because all references copy first */
	{
	  if (r->filename)
	    {
	      snd_remove(r->filename, REMOVE_FROM_CACHE);
	      free(r->filename);
	    }
	  r->filename = NULL;
	}
      if (r->use_temp_file == REGION_DEFERRED)
	r->dr = free_deferred_region(r->dr);
      if (r->rsp) 
	{
	  if ((r->rsp->chans) &&
	      (r->rsp->chans[0]->edits))
	    r->rsp->chans[0]->edits[0]->peak_env = NULL;
	  r->rsp = completely_free_snd_info(r->rsp);
	}
      if (complete == COMPLETE_DELETION) free(r);
    }
}


static region **regions = NULL;
static int regions_size = 0, regions_allocated_size = 0;

void allocate_regions(int numreg)
{
  int i;
  if (numreg > regions_allocated_size)
    {
      if (regions)
	{
	  regions = (region **)realloc(regions, numreg * sizeof(region *));
	  for (i = regions_allocated_size; i < numreg; i++) regions[i] = NULL;
	}
      else regions = (region **)calloc(numreg, sizeof(region *)); 
      regions_allocated_size = numreg;
    }
  if (regions_size > numreg)
    {
      for (i = numreg; i < regions_size; i++)
	if (regions[i])
	  {
	    free_region(regions[i], COMPLETE_DELETION);
	    regions[i] = NULL;
	  }
      if (region_browser_is_active()) update_region_browser(true);
    }
  regions_size = numreg;
}


static void set_max_regions(int n)
{
  if (n >= 0)
    {
      allocate_regions(n);
      allocate_region_rows(n);
      in_set_max_regions(n);
    }
}


int region_id_to_list_position(int id)
{
  int i;
  if ((id >= 0) && (id < region_id_ctr))
    for (i = 0; i < regions_size; i++)
      if ((regions[i]) && 
	  (regions[i]->id == id))
	return(i);
  return(INVALID_REGION);
}


int region_list_position_to_id(int n) 
{
  if ((n >= 0) && 
      (n < regions_size) &&
      (regions[n]))
    return(regions[n]->id);
  return(INVALID_REGION);
}


static region *id_to_region(int id)
{
  int i;
  if ((id >= 0) && (id < region_id_ctr))
    for (i = 0; i < regions_size; i++)
      if ((regions[i]) && 
	  (regions[i]->id == id))
	return(regions[i]);
  return(NULL);
}


bool region_ok(int id) 
{
  return((bool)id_to_region(id));
}


mus_long_t region_len(int n) 
{
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->framples); 
  return(0);
}


int region_chans(int n) 
{  
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->chans); 
  return(0);
}


int region_srate(int n) 
{  
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->srate); 
  return(0);
}


const char *region_file_name(int n) 
{  
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->name); 
  return(NULL);
}


static void get_region_maxamp(region *r)
{
  /* it exists as r->filename, so just use sndlib... */
  mus_float_t *vals;
  mus_long_t *times;
  int i;
  mus_long_t maxpos;
  mus_float_t maxsamp;
  vals = (mus_float_t *)calloc(r->chans, sizeof(mus_float_t));
  times = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));
  mus_sound_maxamps(r->filename, r->chans, vals, times);
  maxpos = times[0];
  maxsamp = vals[0];
  for (i = 1; i < r->chans; i++)
    if (vals[i] > maxsamp)
      {
	maxsamp = vals[i];
	maxpos = times[i];
      }
  free(vals);
  free(times);
  r->maxamp = maxsamp;
  r->maxamp_position = maxpos;
}


mus_float_t region_maxamp(int n) 
{
  region *r;
  r = id_to_region(n);
  if (r)
    {
      if (r->maxamp < 0.0)
	{
	  if (r->use_temp_file == REGION_DEFERRED)
	    deferred_region_to_temp_file(r);
	  get_region_maxamp(r);
	}
      return(r->maxamp); 
    }
  return(0.0);
}


static mus_long_t region_maxamp_position(int n) 
{
  region *r;
  r = id_to_region(n);
  if (r)
    {
      if (r->maxamp < 0.0)
	{
	  if (r->use_temp_file == REGION_DEFERRED)
	    deferred_region_to_temp_file(r);
	  get_region_maxamp(r);
	}
      return(r->maxamp_position);
    }
  return(-1);
}


static mus_float_t region_sample(int reg, int chn, mus_long_t samp)
{
  region *r;
  r = id_to_region(reg);
  if (r)
    {
      if ((samp < r->framples) && (chn < r->chans)) 
	{
	  snd_fd *sf;
	  mus_float_t val;
	  deferred_region *drp;
	  switch (r->use_temp_file)
	    {
	    case REGION_FILE:
	      sf = init_region_read(samp, reg, chn, READ_FORWARD);
	      val = read_sample(sf);
	      free_snd_fd(sf);
	      return(val);

	    case REGION_DEFERRED:
	      drp = r->dr;
	      return(chn_sample(samp + r->begs[chn], drp->cps[chn], drp->edpos[chn]));
	    }
	}
    }
  return(0.0);
}


mus_long_t region_current_location(snd_fd *fd)
{
  region *r;
  r = id_to_region(fd->region);
  switch (r->use_temp_file)
    {
    case REGION_FILE:
      return(current_location(fd));
    case REGION_DEFERRED:
      return(current_location(fd) - r->begs[0]);
    }
  return(-1);
}


static void region_samples(int reg, int chn, mus_long_t beg, mus_long_t num, mus_float_t *data)
{
  region *r;
  r = id_to_region(reg);
  if (r)
    {
      if ((beg < r->framples) && (chn < r->chans))
	{
	  snd_fd *sf;
	  deferred_region *drp;
	  switch (r->use_temp_file)
	    {
	    case REGION_FILE:
	      sf = init_region_read(beg, reg, chn, READ_FORWARD);
	      samples_to_vct_with_reader(num, data, sf);
	      free_snd_fd(sf);
	      break;

	    case REGION_DEFERRED:
	      drp = r->dr;
	      sf = init_sample_read_any_with_bufsize(beg + r->begs[chn], drp->cps[chn], READ_FORWARD, drp->edpos[chn], num);
	      samples_to_vct_with_reader(num, data, sf);
	      free_snd_fd(sf);
	      break;
	    }
	}
    }
}


static int first_region_active(void)
{
  int i;
  for (i = 0; i < regions_size; i++)
    if (regions[i]) 
      return(i);
  return(NO_REGIONS);
}


static int check_regions(void)
{
  int act;
  act = first_region_active();
  if (act == NO_REGIONS) 
    reflect_no_regions_in_region_browser();
  return(act);
}


static void make_region_readable(region *r)
{
  snd_info *regsp;
  file_info *hdr;
  int i;

  if (r->use_temp_file == REGION_DEFERRED) 
    deferred_region_to_temp_file(r);
  if (r->rsp) return;

  regsp = make_basic_snd_info(r->chans);
  regsp->nchans = r->chans;
  regsp->hdr = (file_info *)calloc(1, sizeof(file_info));
  regsp->inuse = SOUND_READER;

  hdr = regsp->hdr;
  hdr->samples = r->framples * r->chans;
  hdr->srate = r->srate;
  hdr->chans = r->chans;
  hdr->comment = NULL;

  for (i = 0; i < r->chans; i++)
    {
      hdr = make_file_info(r->filename, FILE_READ_ONLY, FILE_NOT_SELECTED);
      if (hdr)
	{
	  snd_io *io;
	  int fd;
	  chan_info *cp;

	  cp = make_chan_info(NULL, i, regsp);
	  cp->editable = false;
	  regsp->chans[i] = cp;
	  add_channel_data_1(cp, r->srate, r->framples, WITHOUT_GRAPH);
	  cp->hookable = WITHOUT_HOOK;

	  fd = snd_open_read(r->filename);
	  snd_file_open_descriptors(fd,
				    r->filename,
				    hdr->sample_type,
				    hdr->data_location,
				    hdr->chans,
				    hdr->type);
	  io = make_file_state(fd, hdr, i, 0, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(r->filename, io, hdr, DONT_DELETE_ME, cp->edit_ctr, i); /* don't auto-delete! */
	}
      else
	{
	  Xen_error(Xen_make_error_type("IO-error"),
		    Xen_list_3(C_string_to_Xen_string("can't read region file ~S, ~A"),
			       C_string_to_Xen_string(r->filename),
			       C_string_to_Xen_string(snd_open_strerror())));
	}
    }
  r->rsp = regsp;
}


file_info *fixup_region_data(chan_info *cp, int chan, int pos)
{
  /* for region browser labels */
  if ((pos >= 0) && 
      (pos < regions_size) &&
      (regions[pos]))
    {
      region *r;
      r = regions[pos];
      if (chan < r->chans)
	{
	  snd_info *nsp;
	  chan_info *ncp;

	  make_region_readable(r);
	  nsp = r->rsp;
	  ncp = nsp->chans[chan];

	  cp->sounds = ncp->sounds;
	  cp->sound_size = ncp->sound_size;
	  cp->edits = ncp->edits; /* ?? is this safe ?? */
	  cp->edit_size = ncp->edit_size;
	  cp->edit_ctr = ncp->edit_ctr;
	  cp->edits[0]->samples = ncp->edits[0]->samples;
	  cp->axis = ncp->axis;

	  /* cp here is actually region sound chan 0 */
	  if ((r->peak_envs) && (r->peak_envs[chan]))
	    cp->edits[0]->peak_env = r->peak_envs[chan];
	  else
	    {
	      if (cp->edits[0]->peak_env)
		cp->edits[0]->peak_env = NULL;
	    }

	  initialize_scrollbars(cp);
	  return(nsp->hdr);
	}
    }
  return(NULL);
}


void for_each_region_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value)
{
  /* used only in snd-io.c to remove dangling temp files (probably can't actually happen) */
  int i;
  for (i = 0; i < regions_size; i++)
    {
      int chn;
      region *r;
      r = regions[i];
      if ((r) && (r->rsp) && (r->use_temp_file == REGION_FILE))
	for (chn = 0; chn < r->chans; chn++)
	  {
	    chan_info *cp;
	    cp = r->rsp->chans[chn];
	    (*func)(cp, value);
	  }
    }
}


region_state *region_report(void)
{
  region_state *rs;
  int i, len, size;

  rs = (region_state *)calloc(1, sizeof(region_state));
  len = regions_size;
  for (i = 0; i < regions_size; i++) 
    if (!(regions[i])) 
      {
	len = i; 
	break;
      }
  rs->len = len;
  if (len == 0) return(rs);
  size = len * sizeof(char *);
  rs->name = (char **)calloc(size, 1);
  for (i = 0; i < len; i++)
    {
      region *r;
      char *reg_buf;
      r = regions[i];
      reg_buf = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
      snprintf(reg_buf, LABEL_BUFFER_SIZE, "%d: %s (%s:%s)", r->id, r->name, r->start, r->end);
      rs->name[i] = reg_buf;
    }
  return(rs);
}


char *region_description(int rg)
{
  region *r;
  r = id_to_region(rg);
  if (r)
    return(mus_format("region data from %s (%s : %s)", r->name, r->start, r->end));
  return(NULL);
}


void free_region_state(region_state *r)
{
  if (r)
    {
      int i;
      for (i = 0; i < r->len; i++)
	if (r->name[i]) 
	  free(r->name[i]);
      if (r->name) free(r->name);
      free(r);
    }
}


int remove_region_from_list(int pos) /* region browser */
{
  int i, id;
  id = region_list_position_to_id(pos);
  if (id == INVALID_REGION) return(INVALID_REGION);

  stop_playing_region(id, PLAY_CLOSE);

  free_region(id_to_region(id), COMPLETE_DELETION);
  for (i = pos; i < regions_size - 1; i++) 
    regions[i] = regions[i + 1]; 
  regions[regions_size - 1] = NULL;

  return(check_regions());
}


static void add_to_region_list(region *r) 
{
  int i, okr = -1;
  for (i = max_regions(ss) - 1; i >= 0; i--) 
    {
      if (!(regions[i]))
	{
	  okr = i; 
	  break;
	}
    }
  if (okr == -1)
    okr = max_regions(ss) - 1;
  if (regions[okr]) 
    {
      stop_playing_region(regions[okr]->id, PLAY_CLOSE);
      free_region(regions[okr], COMPLETE_DELETION);
    }
  for (i = okr; i > 0; i--) 
    regions[i] = regions[i - 1]; 
  regions[0] = r;
  if (!r) check_regions();
}


#define NOT_EDITABLE -2

static int paste_region_1(int n, chan_info *cp, bool add, mus_long_t beg, io_error_t *err, int start_chan, int *out_chans)
{
  region *r;
  char *origin = NULL;
  int id = -1;
  io_error_t io_err;
  sync_info *si = NULL;

  r = id_to_region(n);
  if ((!r) || 
      (r->framples == 0)) 
    return(INVALID_REGION);

  if (!(is_editable(cp))) 
    {
      (*err) = IO_EDIT_HOOK_CANCELLATION;
      return(NOT_EDITABLE);
    }

  if (r->use_temp_file == REGION_DEFERRED)
    deferred_region_to_temp_file(r);

  si = sync_to_chan(cp);
  (*out_chans) = si->chans;

  if (add)
    {
      /* unfortunately we need to copy here since the region may fall off the region stack while we're still using the mix */
      char *newname;
      newname = shorter_tempnam(temp_dir(ss), "snd_");
      io_err = copy_file(r->filename, newname);
      if (io_err != IO_NO_ERROR)
	{
	  (*err) = io_err;
	  return(INVALID_REGION);
	}
      else 
	{
#if HAVE_FORTH
	  origin = mus_format("%d %s %" print_mus_long " %s drop", n, S_integer_to_region, beg, S_mix_region);
#endif
#if HAVE_RUBY
	  origin = mus_format("%s(%s(%d), %" print_mus_long, to_proc_name(S_mix_region), to_proc_name(S_integer_to_region), n, beg);
#endif
#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
	  origin = mus_format("%s (%s %d) %" print_mus_long, S_mix_region, S_integer_to_region, n, beg);
#endif
	  if (si->chans > 1)
	    remember_temp(newname, si->chans);

	  id = mix_file(beg, r->framples, si->chans, si->cps, newname, 
			(si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
			origin, with_mix_tags(ss), start_chan);
	  free(origin);
	}
      if (newname) free(newname);
    }
  else
    {
      int i;
      char *tempfile = NULL;
      if (r->use_temp_file == REGION_FILE)
	{
	  tempfile = snd_tempnam();
	  io_err = copy_file(r->filename, tempfile);
	  if (io_err != IO_NO_ERROR)
	    {
	      if (si) free_sync_info(si);
	      (*err) = io_err;
	      return(INVALID_REGION);
	    }
	  else
	    if (r->chans > 1) 
	      remember_temp(tempfile, r->chans);
	}

#if HAVE_FORTH
	  origin = mus_format("%d %s %" print_mus_long " %s drop", n, S_integer_to_region, beg, S_insert_region);
#endif
#if HAVE_RUBY
	  origin = mus_format("%s(%s(%d), %" print_mus_long, to_proc_name(S_insert_region), to_proc_name(S_integer_to_region), n, beg);
#endif
#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
	  origin = mus_format("%s (%s %d) %" print_mus_long, S_insert_region, S_integer_to_region, n, beg);
#endif

      for (i = 0; ((i < r->chans) && (i < si->chans)); i++)
	{
	  chan_info *ncp;
	  ncp = si->cps[i];                       /* currently syncd chan that we might paste to */
	  if (file_insert_samples(beg, r->framples, tempfile, ncp, i,
				  (r->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				  origin, ncp->edit_ctr))
	    update_graph(si->cps[i]);
	}
      free(origin);
      if ((r->use_temp_file == REGION_FILE) && (tempfile)) free(tempfile);
    }
  if (si) free_sync_info(si);
  return(id);
}


static io_error_t paste_region_2(int n, chan_info *cp, bool add, mus_long_t beg)
{
  io_error_t err = IO_NO_ERROR;
  int chans = 0;
  paste_region_1(n, cp, add, beg, &err, 0, &chans);
  return(err);
}


io_error_t paste_region(int n, chan_info *cp) 
{
  return(paste_region_2(n, cp, false, cursor_sample(cp)));
}


io_error_t add_region(int n, chan_info *cp) 
{
  return(paste_region_2(n, cp, true, cursor_sample(cp)));
}


int define_region(sync_info *si, mus_long_t *ends)
{
  /* now look at all sync'd channels, collect them into the current region */
  /* we created the necessary pointers in create_selection above */
  int i;
  mus_long_t len;
  chan_info *cp0;
  snd_info *sp0;
  region *r;
  deferred_region *drp;

  len = 0;
  for (i = 0; i < si->chans; i++)
    if (len < (ends[i] - si->begs[i]))
      len = ends[i] - si->begs[i];
  len += 1;
  if (len <= 0) return(INVALID_REGION);

  cp0 = si->cps[0];
  sp0 = cp0->sound;

  r = (region *)calloc(1, sizeof(region));
  r->id = region_id_ctr++;

  if (regions[0]) 
    add_to_region_list(r); 
  else regions[0] = r;

  r->header_type = (sp0->hdr)->type;
  r->srate = snd_srate(sp0);
  r->maxamp = -1.0;
  r->maxamp_position = -1;
  r->editor_copy = NULL;
  r->name = mus_strdup(sp0->short_filename);
  r->chans = si->chans;
  r->framples = len;
  r->start = prettyf((double)(si->begs[0]) / (double)(r->srate), 2);
  r->end = prettyf((double)(ends[0]) / (double)(r->srate), 2);
  r->use_temp_file = REGION_DEFERRED;
  r->begs = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));
  r->ends = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));

  ss->deferred_regions++;
  r->dr = (deferred_region *)calloc(1, sizeof(deferred_region));
  drp = r->dr;
  drp->chans = si->chans;
  drp->cps = (chan_info **)calloc(drp->chans, sizeof(chan_info *));
  drp->edpos = (int *)calloc(drp->chans, sizeof(int));
  drp->len = len;

  for (i = 0; i < drp->chans; i++)
    {
      drp->cps[i] = si->cps[i];
      r->begs[i] = si->begs[i];
      r->ends[i] = ends[i] - si->begs[i];
      drp->edpos[i] = drp->cps[i]->edit_ctr;
      if (r->ends[i] > PEAK_ENV_CUTOFF)
	{
	  peak_env_info *ep;
	  ep = drp->cps[i]->edits[drp->edpos[i]]->peak_env;
	  if ((ep) && (ep->completed))
	    {
	      if (!r->peak_envs)
		r->peak_envs = (peak_env_info **)calloc(r->chans, sizeof(peak_env_info *));
	      r->peak_envs[i] = peak_env_section(drp->cps[i], r->begs[i], r->ends[i] + 1, drp->edpos[i]);
	    }
	}
    }

  reflect_regions_in_region_browser();
  if (region_browser_is_active()) update_region_browser(true);
  return(r->id);
}


static void deferred_region_to_temp_file(region *r)
{
  int i, datumb = 0;
  bool copy_ok;
  mus_long_t alloc_len, len = 0;
  snd_fd **sfs = NULL;
  snd_info *sp0;
  deferred_region *drp = NULL;
  mus_float_t **data = NULL;

  ss->deferred_regions--;
  drp = r->dr;
  len = drp->len;
  r->use_temp_file = REGION_FILE;
  r->filename = snd_tempnam();
  sp0 = drp->cps[0]->sound;

  copy_ok = ((mus_header_writable(MUS_NEXT, sp0->hdr->sample_type)) && 
	     (r->chans == (int)sp0->nchans) &&
	     (r->peak_envs) &&
	     ((drp->len - 1) == r->ends[0]));
  if (copy_ok)
    for (i = 0; i < r->chans; i++)
      if ((drp->edpos[i] != 0) || 
	  (drp->cps[i]->sound != sp0) ||
	  (r->begs[i] != r->begs[0]) ||
	  (r->ends[i] != (drp->len - 1)) ||
	  (!r->peak_envs[i]))
	{
	  copy_ok = false;
	  break;
	}

  if (copy_ok)
    {
      /* write next header with correct len
       * seek loc in sp0->filename (r->begs[0])
       * copy len*data-size bytes
       * get max from amp envs
       */
      mus_long_t err;

      datumb = mus_bytes_per_sample(sp0->hdr->sample_type);
      err = mus_write_header(r->filename, MUS_NEXT, r->srate, r->chans, drp->len * r->chans, sp0->hdr->sample_type, "region deferred temp");

      if (err != MUS_NO_ERROR)
	snd_error("can't write region temp file %s: %s", r->filename, snd_io_strerror());
      else
	{
	  int fdi, fdo;
	  mus_long_t oloc;
	  oloc = mus_header_data_location();
	  fdo = snd_reopen_write(r->filename);
	  lseek(fdo, oloc, SEEK_SET);
	  fdi = mus_file_open_read(sp0->filename);
	  if (fdi == -1)
	    snd_error("can't read region's original sound? %s: %s", sp0->filename, snd_io_strerror());
	  else
	    {
	      mus_long_t j, data_size;
	      char *buffer;

	      lseek(fdi, sp0->hdr->data_location + r->chans * datumb * r->begs[0], SEEK_SET);
	      data_size = drp->len * r->chans * datumb;
	      if (data_size > REPORTING_SIZE)
		alloc_len = REPORTING_SIZE;
	      else alloc_len = data_size;
	      buffer = (char *)malloc(alloc_len * sizeof(char));
	      for (j = 0; j < data_size; j += alloc_len)
		{
		  mus_long_t bytes;
		  ssize_t n;
		  bytes = data_size - j;
		  if (bytes > alloc_len)
		    bytes = alloc_len;

		  /* read and write return 0 to indicate end of file, apparently */
		  n = read(fdi, buffer, bytes);

		  if (n < 0)
		    fprintf(stderr, "IO error while reading region temp file: %d %s\n", (int)n, strerror(errno));
		  if (n > 0)
		    {
		      n = write(fdo, buffer, bytes);
		      if (n < 0)
			fprintf(stderr, "IO error while writing region temp file: %d %s\n", (int)n, strerror(errno));
		    }
		}
	      free(buffer);
	      snd_close(fdi, sp0->filename);
	    }
	  snd_close(fdo, r->filename);
	}
    }
  else
    {
      io_error_t io_err = IO_NO_ERROR;
      file_info *hdr = NULL;
      int ofd;

      hdr = make_temp_header(r->filename, r->srate, r->chans, 0, (char *)__func__);
      ofd = open_temp_file(r->filename, r->chans, hdr, &io_err);
      if (ofd == -1)
	snd_error("%s region temp file %s: %s", 
		  (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		  r->filename, 
		  snd_open_strerror());
      else
	{
	  sfs = (snd_fd **)calloc(r->chans, sizeof(snd_fd *));
	  data = (mus_float_t **)calloc(r->chans, sizeof(mus_float_t *));
	  datumb = mus_bytes_per_sample(hdr->sample_type);
	  /* here if peak_envs, maxamp exists */

	  alloc_len = len;
	  if (alloc_len > REPORTING_SIZE)
	    alloc_len = REPORTING_SIZE;

	  for (i = 0; i < r->chans; i++)
	    {
	      sfs[i] = init_sample_read_any(r->begs[i], drp->cps[i], READ_FORWARD, drp->edpos[i]);
	      data[i] = (mus_float_t *)calloc(alloc_len, sizeof(mus_float_t));
	    }

	  if ((r->chans == 1) &&
	      (r->ends[0] == (len - 1)))
	    {
	      snd_fd *sf;
	      mus_float_t *d;

	      sf = sfs[0];
	      d = data[0];
	      if (len <= REPORTING_SIZE)
		{
		  samples_to_vct_with_reader(len, d, sf);
		  mus_file_write(ofd, 0, len - 1, 1, data);
		}
	      else
		{
		  int k;
		  sampler_set_safe(sf, len);
		  for (k = 0; k < len; k += alloc_len) 
		    {
		      mus_long_t kdur, n;
		      int err;
		      kdur = len - k;
		      if (kdur > alloc_len) kdur = alloc_len;
		      for (n = 0; n < kdur; n++)
			d[n] = read_sample(sf);
		      err = mus_file_write(ofd, 0, kdur - 1, 1, data);
		      if (err != MUS_NO_ERROR) break;
		    }
		}
	    }
	  else
	    {
	      if (len <= REPORTING_SIZE)
		{
		  for (i = 0; i < r->chans; i++)
		    samples_to_vct_with_reader(len, data[i], sfs[i]);
		  mus_file_write(ofd, 0, len - 1, r->chans, data);
		}
	      else
		{
		  int k;
		  for (i = 0; i < r->chans; i++)
		    sampler_set_safe(sfs[i], len);
		  for (k = 0; k < len; k += alloc_len) 
		    {
		      int err;
		      mus_long_t kdur;
		      
		      kdur = len - k;
		      if (kdur > alloc_len) kdur = alloc_len;
		      for (i = 0; i < r->chans; i++)
			{
			  mus_long_t n;
			  snd_fd *p;
			  mus_float_t *buf;
			  buf = data[i];
			  p = sfs[i];
			  for (n = 0; n < kdur; n++)
			    buf[n] = read_sample(p);
			}
		      err = mus_file_write(ofd, 0, kdur - 1, r->chans, data);
		      if (err != MUS_NO_ERROR) break;
		    }
		}
	    }

	  close_temp_file(r->filename, ofd, hdr->type, len * r->chans * datumb);
	  for (i = 0; i < r->chans; i++) free(data[i]);
	  for (i = 0; i < r->chans; i++) free_snd_fd(sfs[i]);
	  free(sfs);
	  free(data);
	  data = NULL;
	}
      free_file_info(hdr);
    }
  r->dr = free_deferred_region(r->dr);
}


void sequester_deferred_regions(chan_info *cp, int edit_top)
{
  region *r;
  deferred_region *drp;
  int i;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if ((r) && (r->use_temp_file == REGION_DEFERRED))
	{
	  int j;
	  drp = r->dr;
	  for (j = 0; j < drp->chans; j++)
	    if ((drp->cps[j] == cp) &&
		(drp->edpos[j] > edit_top))
	      {
		if (r->ends[j] > 1000000)
		  status_report(cp->sound, "sequestering region %d...", r->id);
		deferred_region_to_temp_file(r);
		if (r->ends[j] > 1000000)
		  clear_status_area(cp->sound);
		break;
	      }
	}
    }
}


snd_fd *init_region_read(mus_long_t beg, int n, int chan, read_direction_t direction)
{
  /* conjure up a reasonable looking ed list and sound list */
  region *r;
  r = id_to_region(n);
  if ((r) && (chan < r->chans))
    {
      if ((beg == 0) && 
	  (direction == READ_BACKWARD)) 
	beg = r->framples - 1;
      if (r->use_temp_file == REGION_DEFERRED)
	{
	  deferred_region *drp;
	  drp = r->dr;
	  return(init_sample_read_any(r->begs[chan] + beg, drp->cps[chan], direction, drp->edpos[chan]));
	}
      else
	{
	  make_region_readable(r);
	  return(init_sample_read(beg, r->rsp->chans[chan], direction));
	}
    }
  return(NULL);
}


void cleanup_region_temp_files(void)
{ /* called upon exit to get rid of lingering region-related temp files */
  int i;
  for (i = 0; i < regions_size; i++)
    {
      region *r;
      r = regions[i];
      if ((r) && 
	  (r->use_temp_file == REGION_FILE) && 
	  (r->filename))
	{
	  snd_remove(r->filename, REMOVE_FROM_CACHE);
	  free(r->filename);
	  r->filename = NULL;
	}
    }
}


int snd_regions(void)
{
  int i, num;
  num = 0;
  for (i = 0; i < regions_size; i++) 
    if (regions[i]) 
      num++;
  return(num);
}


/* (restore-region n chans len srate maxamp name start end filename [date-and-length]) */

void save_regions(FILE *fd)
{
  int i;
  for (i = 0; i < regions_size; i++)
    {
      region *r;
      r = regions[i];
      if (r)
	{
	  io_error_t io_err;
	  char *newname;
	  char *ofile = NULL;

	  if (r->use_temp_file == REGION_DEFERRED) 
	    deferred_region_to_temp_file(r);
	  ofile = shorter_tempnam(save_dir(ss), "snd_save_");
	  newname = run_save_state_hook(ofile);
	  free(ofile);

	  io_err = copy_file(r->filename, newname);
	  if (io_err != IO_NO_ERROR)
	    {
	      snd_warning("trying to save region %d (%s) in %s: %s, %s", 
			  r->id, r->filename, newname, io_error_name(io_err),
			  strerror(ss->local_errno));
	    }
	  else
	    {
#if HAVE_RUBY
	      fprintf(fd, "%s(%d, %d, %" print_mus_long ", %d, %.4f, \"%s\", \"%s\", \"%s\", ",
		      "restore_region", i, r->chans, r->framples, r->srate, r->maxamp, r->name, r->start, r->end);
	      fprintf(fd, " \"%s\", [%d, %" print_mus_long "])\n",
		      newname,
		      (int)mus_sound_write_date(newname),
		      mus_sound_length(newname));
#endif
#if HAVE_SCHEME
	      fprintf(fd, "(%s %d %d %" print_mus_long " %d %.4f \"%s\" \"%s\" \"%s\"",
		      S_restore_region, i, r->chans, r->framples, r->srate, r->maxamp, r->name, r->start, r->end);
	      fprintf(fd, " \"%s\" (list %d %" print_mus_long "))\n",
		      newname,
		      (int)mus_sound_write_date(newname),
		      mus_sound_length(newname));
#endif
#if HAVE_FORTH
	  fprintf(fd, "%d %d %" print_mus_long " %d %.4f \"%s\" \"%s\" \"%s\"",
	          i, r->chans, r->framples, r->srate, r->maxamp, r->name, r->start, r->end);
 	  fprintf(fd, " \"%s\" '( %d %" print_mus_long " ) %s drop\n",
 		  newname,
 		  (int)mus_sound_write_date(newname),
 		  mus_sound_length(newname),
		  S_restore_region);
#endif
	    }
	  free(newname);
	}
    }
}


void region_edit(int pos)
{
  /* from region browser:
   *   load region into temp file, load that into snd editor,
   *   if 'save', save temp file and update region (browser also) (cancelling active display if any)
   *   while editing, if delete in browser, cut backpointer in editor and signal it
   */
  region *r = NULL;
  if ((pos >= 0) && 
      (pos < regions_size) &&
      (regions[pos]))
    r = regions[pos];
  if (r) 
    {
      if (r->editor_copy)
	snd_error("region %d already being edited", r->id);
      else
	{
	  io_error_t io_err;
	  char *temp_region_name;
	  if (r->use_temp_file == REGION_DEFERRED) 
	    deferred_region_to_temp_file(r);
	  temp_region_name = shorter_tempnam(temp_dir(ss), "region-");
	  io_err = copy_file(r->filename, temp_region_name);
	  if (io_err == IO_NO_ERROR)
	    {
	      snd_info *sp;
	      ss->open_requestor = FROM_REGION_EDIT;
	      sp = snd_open_file(temp_region_name, FILE_READ_WRITE);
	      if (sp)
		{
		  r->editor_copy = sp;
		  r->editor_name = mus_strdup(temp_region_name);
		  sp->edited_region = r;
		  /* save backpointer so subsequent save affects region if still legit */
		  /* also, since it's a temp file, if closed, delete temp */
		}
	      else snd_error("edit region: can't open region %d temp sound %s: %s!",
			     r->id, temp_region_name, snd_io_strerror());
	    }
	  else 
	    snd_error("edit region: can't save region %d in temp file (%s: %s)",
		      r->id, temp_region_name, snd_io_strerror());
	  free(temp_region_name);
	}
    }
  else snd_error("edit region: no region at position %d!", pos);
}


void clear_region_backpointer(snd_info *sp)
{
  if (sp->edited_region)
    {
      region *r;
      r = sp->edited_region;
      if (r)
	{
	  snd_remove(r->editor_name, REMOVE_FROM_CACHE);
	  free(r->editor_name);
	  r->editor_name = NULL;
	  r->editor_copy = NULL;
	}
      sp->edited_region = NULL;
    }
}


void save_region_backpointer(snd_info *sp)
{
  /* region being edited, user chose 'save' */
  region *r;
  int i;
  io_error_t io_err;

  r = sp->edited_region;
  /* update r's data in file, deleting old, redisplay if browser active etc */

  if (r == regions[0]) deactivate_selection();
  free_region(r, CLEAR_REGION_DATA);

  r->use_temp_file = REGION_FILE;
  r->maxamp = 0.0;
  r->maxamp_position = -1;
  r->framples = current_samples(sp->chans[0]);

  for (i = 0; i < (int)sp->nchans; i++)
    {
      mus_float_t val;
      val = channel_maxamp(sp->chans[i], AT_CURRENT_EDIT_POSITION);
      if (val > r->maxamp) r->maxamp = val;

      if (r->ends[i] > PEAK_ENV_CUTOFF)
        {
          chan_info *cp;
          cp = sp->chans[i];
          if (!r->peak_envs)
            r->peak_envs = (peak_env_info **)calloc(r->chans, sizeof(peak_env_info *));
          else 
	    {
	      if (r->peak_envs[i])
		free_peak_env_info(r->peak_envs[i]);
	    }
          /* if region file was edited, the peak_envs probably changed */
          r->peak_envs[i] = copy_peak_env_info(cp->edits[0]->peak_env, false);
        }
    }

  /* make new region temp file */
  r->filename = snd_tempnam();
  io_err = copy_file(r->editor_name, r->filename);
  if (io_err != IO_NO_ERROR)
    {
      if (io_err == IO_CANT_OPEN_FILE)
	snd_error("can't find edited region temp file (%s: %s)", r->editor_name, snd_io_strerror());
      else snd_error("can't make region temp file (%s: %s)", r->filename, snd_io_strerror());
    }
  else
    {
      make_region_readable(r);
      if (region_browser_is_active()) 
	update_region_browser(true);
    }
}


io_error_t save_region(int rg, const char *name, mus_sample_t samp_type, mus_header_t head_type, const char *comment)
{
  region *r;
  io_error_t io_err = IO_NO_ERROR;

  r = id_to_region(rg);
  if (r->use_temp_file == REGION_DEFERRED) 
    deferred_region_to_temp_file(r);

  io_err = snd_write_header(name, head_type, region_srate(rg), r->chans, r->chans * r->framples, samp_type, comment, NULL);
  if (io_err == IO_NO_ERROR)
    {
      mus_long_t oloc;
      int ofd;

      oloc = mus_header_data_location();
      ofd = snd_reopen_write(name);
      if (ofd != -1)
	{
	  int ifd;
	  snd_file_open_descriptors(ofd, name, samp_type, oloc, r->chans, head_type);
	  mus_file_set_clipping(ofd, clipping(ss));
	  lseek(ofd, oloc, SEEK_SET);
	  /* copy r->filename with possible header/sample type changes */

	  ifd = snd_open_read(r->filename);
	  if (ifd != -1)
	    {
	      mus_long_t iloc, framples, cursamples;
	      int chans, i, err = 0, ioff;
	      mus_float_t **bufs;

	      chans = mus_sound_chans(r->filename);
	      framples = mus_sound_samples(r->filename) / chans;
	      iloc = mus_sound_data_location(r->filename);

	      snd_file_open_descriptors(ifd,
					r->filename,
					mus_sound_sample_type(r->filename),
					iloc,
					chans,
					mus_sound_header_type(r->filename));
	      lseek(ifd, iloc, SEEK_SET);
	      bufs = (mus_float_t **)calloc(chans, sizeof(mus_float_t *));
	      for (i = 0; i < chans; i++) bufs[i] = (mus_float_t *)calloc(FILE_BUFFER_SIZE, sizeof(mus_float_t));

	      if (((framples * chans * mus_sound_datum_size(r->filename)) >> 10) > disk_kspace(name))
		snd_warning("not enough space to save region? -- need %" print_mus_long " bytes",
			    framples * chans * mus_sound_datum_size(r->filename));

	      for (ioff = 0; ioff < framples; ioff += FILE_BUFFER_SIZE)
		{
		  if ((ioff + FILE_BUFFER_SIZE) < framples) 
		    cursamples = FILE_BUFFER_SIZE; 
		  else cursamples = (framples - ioff);
		  mus_file_read(ifd, ioff, cursamples, chans, bufs);
		  err = mus_file_write(ofd, 0, cursamples - 1, chans, bufs);
		  if (err != MUS_NO_ERROR) 
		    {
		      snd_warning("write error during %s", S_save_region);
		      break;
		    }
		}
	      err = mus_file_close(ifd);
	      for (i = 0; i < chans; i++) free(bufs[i]);
	      free(bufs);

	      if (err != 0)
		snd_warning("can't close %s input!", S_save_region);
	      err = mus_file_close(ofd);

	      if (ss->file_monitor_ok)
		{
		  if (err != 0)
		    snd_error("%s %d: %s %s", S_save_region, rg, r->filename, snd_io_strerror());
		}
	      else
		{
#if USE_MOTIF
		  if (err == 0)
		    alert_new_file();
		  else
#else
		  if (err != 0)
#endif
		     snd_error("%s %d: %s %s", S_save_region, rg, r->filename, snd_io_strerror());
		}
	    }
	  else snd_error("%s %d: %s %s", S_save_region, rg, r->filename, snd_io_strerror());
	}
      else snd_error("%s %d: %s %s", S_save_region, rg, name, snd_io_strerror());
    }
  else snd_error("%s %d: %s %s", S_save_region, rg, name, snd_io_strerror());
  return(io_err);
}


/* ---------------------------------------- region objects ---------------------------------------- */

typedef struct {
  int n;
} xen_region;


#define Xen_to_xen_region(arg) ((xen_region *)Xen_object_ref(arg))

int xen_region_to_int(Xen n)
{
  xen_region *mx;
  mx = Xen_to_xen_region(n);
  return(mx->n);
}


static Xen_object_type_t xen_region_tag;

bool xen_is_region(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_region_tag));
}


static void xen_region_free(xen_region *v) {if (v) free(v);}

Xen_wrap_free(xen_region, free_xen_region, xen_region_free)


static char *xen_region_to_string(xen_region *v)
{
  #define REGION_PRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(REGION_PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, REGION_PRINT_BUFFER_SIZE, "#<region %d>", v->n);
  return(buf);
}


#if HAVE_FORTH || HAVE_RUBY
Xen_wrap_print(xen_region, print_xen_region, xen_region_to_string)

static Xen g_xen_region_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_region_to_string "region->string"
  Xen_check_type(xen_is_region(obj), obj, 1, S_xen_region_to_string, "a region");
  vstr = xen_region_to_string(Xen_to_xen_region(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#else
#if HAVE_SCHEME
static s7_pointer g_xen_region_to_string(s7_scheme *sc, s7_pointer args)
{
  char *vstr;
  Xen result;
  vstr = xen_region_to_string(Xen_to_xen_region(s7_car(args)));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif
#endif


#if (!HAVE_SCHEME)
static bool xen_region_equalp(xen_region *v1, xen_region *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static Xen equalp_xen_region(Xen obj1, Xen obj2)
{
  if ((!(xen_is_region(obj1))) || (!(xen_is_region(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_region_equalp(Xen_to_xen_region(obj1), Xen_to_xen_region(obj2))));
}
#endif


static xen_region *xen_region_make(int n)
{
  xen_region *new_v;
  new_v = (xen_region *)malloc(sizeof(xen_region));
  new_v->n = n;
  return(new_v);
}


Xen new_xen_region(int n)
{
  xen_region *mx;
  if (n < 0)
    return(Xen_false);

  mx = xen_region_make(n);
  return(Xen_make_object(xen_region_tag, mx, 0, free_xen_region));
}


#if HAVE_SCHEME
static bool s7_xen_region_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_region *)obj1)->n == ((xen_region *)obj2)->n));
}

static Xen s7_xen_region_length(s7_scheme *sc, Xen args)
{
  return(g_region_framples(s7_car(args), Xen_integer_zero));
}
#endif


static void init_xen_region(void)
{
#if HAVE_SCHEME
  xen_region_tag = s7_make_c_type(s7, "<region>");
  s7_c_type_set_free(s7, xen_region_tag, free_xen_region);
  s7_c_type_set_equal(s7, xen_region_tag, s7_xen_region_equalp);
  s7_c_type_set_length(s7, xen_region_tag, s7_xen_region_length);
  s7_c_type_set_to_string(s7, xen_region_tag, g_xen_region_to_string);
#else
#if HAVE_RUBY
  xen_region_tag = Xen_make_object_type("XenRegion", sizeof(xen_region));
#else
  xen_region_tag = Xen_make_object_type("Region", sizeof(xen_region));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_region_tag,   print_xen_region);
  fth_set_object_dump(xen_region_tag,      g_xen_region_to_string);
  fth_set_object_equal(xen_region_tag,     equalp_xen_region);
  fth_set_object_free(xen_region_tag,      free_xen_region);
#endif

#if HAVE_RUBY
  rb_define_method(xen_region_tag, "to_s",     Xen_procedure_cast print_xen_region, 0);
  rb_define_method(xen_region_tag, "eql?",     Xen_procedure_cast equalp_xen_region, 1);
  rb_define_method(xen_region_tag, "==",       Xen_procedure_cast equalp_xen_region, 1);
  rb_define_method(xen_region_tag, "to_str",   Xen_procedure_cast g_xen_region_to_string, 0);
#endif
}

/* -------------------------------------------------------------------------------- */


static Xen g_integer_to_region(Xen n)
{
  #define H_integer_to_region "(" S_integer_to_region " n) returns a region object corresponding to the given integer"
  region* r;
  Xen_check_type(Xen_is_integer(n), n, 1, S_integer_to_region, "an integer");
  r = id_to_region(Xen_integer_to_C_int(n));
  if (r)
    return(new_xen_region(r->id));
  return(Xen_false);
}


static Xen g_region_to_integer(Xen n)
{
  #define H_region_to_integer "(" S_region_to_integer " id) returns the integer corresponding to the given region"
  Xen_check_type(xen_is_region(n), n, 1, S_region_to_integer, "a region");
  return(C_int_to_Xen_integer(xen_region_to_int(n)));
}


static Xen snd_no_such_region_error(const char *caller, Xen n)
{
  Xen_error(Xen_make_error_type("no-such-region"),
	    Xen_list_3(C_string_to_Xen_string("~A: no such region, ~A"),
		       C_string_to_Xen_string(caller),
		       n));
  return(Xen_false);
}


/* Xen pos, Xen chans, Xen len, Xen srate, Xen maxamp, Xen name, Xen start, Xen end, Xen filename, Xen date */

static Xen g_restore_region(Xen args)
{
  /* internal function used by save-state mechanism -- not intended for external use */
  region *r;
  int i, regn;
  Xen arg, pos, chans, len, srate, maxamp, name, start, end, filename, date;
  
  Xen_check_type(Xen_list_length(args) == 10, args, 0, S_restore_region, "10 items");
  arg = args;
  pos = Xen_car(arg);
  Xen_check_type(Xen_is_integer(pos), pos, 1, S_restore_region, "a region id");

  arg = Xen_cdr(arg);
  chans = Xen_car(arg);
  Xen_check_type(Xen_is_integer(chans), chans, 2, S_restore_region, "an integer");

  arg = Xen_cdr(arg);
  len = Xen_car(arg);
  Xen_check_type(Xen_is_integer(len), len, 3, S_restore_region, "an integer");

  arg = Xen_cdr(arg);
  srate = Xen_car(arg);
  Xen_check_type(Xen_is_integer(srate), srate, 4, S_restore_region, "an integer");

  arg = Xen_cdr(arg);
  maxamp = Xen_car(arg);
  Xen_check_type(Xen_is_double(maxamp), maxamp, 5, S_restore_region, "a double");

  arg = Xen_cdr(arg);
  name = Xen_car(arg);
  Xen_check_type(Xen_is_string(name), name, 6, S_restore_region, "a string");

  arg = Xen_cdr(arg);
  start = Xen_car(arg);
  Xen_check_type(Xen_is_string(start), start, 7, S_restore_region, "a string");

  arg = Xen_cdr(arg);
  end = Xen_car(arg);
  Xen_check_type(Xen_is_string(end), end, 8, S_restore_region, "a string");

  arg = Xen_cdr(arg);
  filename = Xen_car(arg);
  Xen_check_type(Xen_is_string(filename), filename, 9, S_restore_region, "a string");

  arg = Xen_cdr(arg);
  date = Xen_car(arg);
  Xen_check_type(Xen_is_list(date) && (Xen_list_length(date) == 2), date, 10, S_restore_region, "a list: '(time bytes)");

  check_saved_temp_file("region", filename, date);

  r = (region *)calloc(1, sizeof(region));
  regn = Xen_integer_to_C_int(pos);
  if (regions[regn]) free_region(regions[regn], COMPLETE_DELETION);
  regions[regn] = r;
  r->id = region_id_ctr++;
  r->maxamp = Xen_real_to_C_double(maxamp);
  r->maxamp_position = -1; /* not saved/restored */
  r->chans = Xen_integer_to_C_int(chans);
  r->rsp = NULL;
  r->editor_copy = NULL;
  r->editor_name = NULL;
  r->framples = Xen_llong_to_C_llong(len);
  r->srate = Xen_integer_to_C_int(srate);
  r->name = mus_strdup(Xen_string_to_C_string(name));
  r->start = mus_strdup(Xen_string_to_C_string(start));
  r->end = mus_strdup(Xen_string_to_C_string(end));
  r->use_temp_file = REGION_FILE;
  r->filename = mus_strdup(Xen_string_to_C_string(filename));

  /* bugfix for saved regions -- thanks to Tito Latini */
  r->begs = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));
  r->ends = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));
  for (i = 0; i < r->chans; i++)
    {
      r->begs[i] = 0;
      r->ends[i] = r->framples - 1;
    }
 
  reflect_regions_in_region_browser();
  return(C_int_to_Xen_integer(r->id));
}


static Xen g_insert_region(Xen reg_n, Xen samp_n, Xen snd_n, Xen chn_n) /* opt reg_n */
{
  #define H_insert_region "("  S_insert_region " region :optional (start-samp 0) snd chn): \
insert region data into snd's channel chn starting at start-samp"

  chan_info *cp;
  int rg;
  mus_long_t samp;
  io_error_t err = IO_NO_ERROR;

  Xen_check_type(xen_is_region(reg_n), reg_n, 1, S_insert_region, "a region id");
  Xen_check_type(Xen_is_integer_or_unbound(samp_n), samp_n, 2, S_insert_region, "an integer");

  Snd_assert_channel(S_insert_region, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_region);
  if (!cp) return(Xen_false);

  rg = Xen_region_to_C_int(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_insert_region, reg_n));

  samp = beg_to_sample(samp_n, S_insert_region);
  err = paste_region_2(rg, cp, false, samp);

  if (is_serious_io_error(err))
    Xen_error(Xen_make_error_type("IO-error"),
	      Xen_list_3(C_string_to_Xen_string(S_insert_region ": can't edit ~A (region access problem?), ~A"),
			 C_string_to_Xen_string(cp->sound->filename),
			 C_string_to_Xen_string(io_error_name(err))));
  update_graph(cp);
  return(reg_n);
}


static Xen g_max_regions(void) 
{
  #define H_max_regions "(" S_max_regions "): max number of regions saved on the region list"
  return(C_int_to_Xen_integer(max_regions(ss)));
}


static Xen g_set_max_regions(Xen n) 
{
  int regs;
  Xen_check_type(Xen_is_integer(n), n, 1, S_set S_max_regions, "an integer"); 

  regs = Xen_integer_to_C_int(n);
  if (regs < 0)
    Xen_out_of_range_error(S_set S_max_regions, 1, n, S_max_regions "negative?");

  set_max_regions(regs);
  return(C_int_to_Xen_integer(max_regions(ss)));
}


static Xen g_is_region(Xen n)
{
  #define H_is_region "(" S_is_region " reg): " PROC_TRUE " if region is active"
  return(C_bool_to_Xen_boolean((xen_is_region(n)) && (region_ok(Xen_region_to_C_int(n)))));
}


Xen g_region_framples(Xen n, Xen chan) 
{
  region *r;
  int rg, chn;
  #define H_region_framples "(" S_region_framples " reg :optional (chan 0)): region length in framples"

  Xen_check_type(xen_is_region(n), n, 1, S_region_framples, "a region");
  Xen_check_type(Xen_is_integer_or_unbound(chan), chan, 2, S_region_framples, "an integer");

  rg = Xen_region_to_C_int(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_framples, n));

  if (!Xen_is_bound(chan))
    return(C_llong_to_Xen_llong(region_len(rg)));
  chn = Xen_integer_to_C_int(chan);
  if ((chn < 0) || (chn >= region_chans(rg)))
    return(snd_no_such_channel_error(S_region_framples, Xen_list_1(n), chan));

  r = id_to_region(rg);
  return(C_llong_to_Xen_llong(r->ends[chn] + 1));
}


static Xen g_region_position(Xen n, Xen chan) 
{
  region *r;
  int rg, chn = 0;
  #define H_region_position "(" S_region_position " reg :optional (chan 0)): region's position in the original sound"

  Xen_check_type(xen_is_region(n), n, 1, S_region_position, "a region");
  Xen_check_type(Xen_is_integer_or_unbound(chan), chan, 2, S_region_position, "an integer");

  rg = Xen_region_to_C_int(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_position, n));

  if (Xen_is_integer(chan)) chn = Xen_integer_to_C_int(chan);
  if ((chn < 0) || (chn >= region_chans(rg)))
    return(snd_no_such_channel_error(S_region_position, Xen_list_1(n), chan));

  r = id_to_region(rg);
  return(C_llong_to_Xen_llong(r->begs[chn]));
}


typedef enum {REGION_SRATE, REGION_CHANS, REGION_MAXAMP, REGION_FORGET, REGION_MAXAMP_POSITION, REGION_HOME} region_field_t;

static Xen region_get(region_field_t field, Xen n, const char *caller)
{
  int rg;
  rg = Xen_region_to_C_int(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(caller, n));

  switch (field)
    {
    case REGION_SRATE:  return(C_int_to_Xen_integer(region_srate(rg)));                              
    case REGION_CHANS:  return(C_int_to_Xen_integer(region_chans(rg)));                              
    case REGION_MAXAMP: return(C_double_to_Xen_real(region_maxamp(rg)));                             
    case REGION_MAXAMP_POSITION: return(C_llong_to_Xen_llong(region_maxamp_position(rg)));           
    case REGION_FORGET: delete_region_and_update_browser(region_id_to_list_position(rg)); return(n); 
    case REGION_HOME:
      {
	region *r;
	r = id_to_region(rg);
	if (r)
	  return(Xen_list_3(C_string_to_Xen_string(r->name), 
			    C_llong_to_Xen_llong(r->begs[0]), 
			    C_llong_to_Xen_llong(r->ends[0] + 1))); 
      }
      break;
    default: break;
    }
  return(Xen_false);
}


Xen g_region_srate(Xen n) 
{
  #define H_region_srate "(" S_region_srate " reg): region (nominal) srate"
  Xen_check_type(xen_is_region(n), n, 1, S_region_srate, "a region");
  return(region_get(REGION_SRATE, n, S_region_srate));
}


Xen g_region_chans(Xen n) 
{
  #define H_region_chans "(" S_region_chans " reg): region channels"
  Xen_check_type(xen_is_region(n), n, 1, S_region_chans, "a region");
  return(region_get(REGION_CHANS, n, S_region_chans));
}


static Xen g_region_home(Xen n) 
{
  #define H_region_home "(" S_region_home " reg): a list with the region source sound name and position info"
  Xen_check_type(xen_is_region(n), n, 1, S_region_home, "a region");
  return(region_get(REGION_HOME, n, S_region_home));
}


Xen g_region_maxamp(Xen n) 
{
  #define H_region_maxamp "(" S_region_maxamp " reg): region maxamp"
  Xen_check_type(xen_is_region(n), n, 1, S_region_maxamp, "a region");
  return(region_get(REGION_MAXAMP, n, S_region_maxamp));
}


static Xen g_region_maxamp_position(Xen n) 
{
  #define H_region_maxamp_position "(" S_region_maxamp_position " reg): first sample where region maxamp occurs"
  Xen_check_type(xen_is_region(n), n, 1, S_region_maxamp_position, "a region");
  return(region_get(REGION_MAXAMP_POSITION, n, S_region_maxamp_position));
}


static Xen g_forget_region(Xen n) 
{
  #define H_forget_region "(" S_forget_region " reg): remove region from the region list"
  Xen_check_type(xen_is_region(n), n, 1, S_forget_region, "a region");
  return(region_get(REGION_FORGET, n, S_forget_region));
}


Xen g_play_region(Xen n, play_process_t back, Xen stop_proc) 
{
  int rg;
  region *r;
  rg = Xen_region_to_C_int(n);
  r = id_to_region(rg);
  if (r)
    {
      make_region_readable(r);
      play_region_1(rg, back, stop_proc);
      return(n);
    }
  return(snd_no_such_region_error(S_play, n));
}


static Xen g_regions(void) 
{
  #define H_regions "(" S_regions "): current active regions (a list of region ids)"
  int i;
  Xen result;
  result = Xen_empty_list;
  for (i = (regions_size - 1); i >= 0; i--)
    if (regions[i])
      result = Xen_cons(C_int_to_Xen_region(regions[i]->id), result);
  return(result);
}


static Xen g_make_region(Xen beg, Xen end, Xen snd_n, Xen chn_n)
{
  #define H_make_region "(" S_make_region " :optional beg end snd chn): make a new region between beg and end in snd. \
If chn is " PROC_TRUE ", all chans are included, taking the snd sync field into account if it's not 0.  If no args are passed, the current \
selection is used."
  int id = INVALID_REGION;

  if (max_regions(ss) <= 0) return(Xen_false);
  if (!Xen_is_bound(beg))
    id = make_region_from_selection();
  else
    {
      sync_info *si = NULL;
      snd_info *sp;
      mus_long_t ibeg, iend;
      mus_long_t *ends = NULL;
      int i;

      Xen_check_type(Xen_is_integer(beg), beg, 1, S_make_region, "an integer");
      Xen_check_type(Xen_is_integer(end), end, 2, S_make_region, "an integer");

      ibeg = beg_to_sample(beg, S_make_region);
      iend = beg_to_sample(end, S_make_region);

      if (Xen_is_true(chn_n))
	{
	  /* all chans and all sync'd chans if sync not 0 */
	  sp = get_sp(snd_n);
	  if (sp)
	    {
	      int old_sync;
	      old_sync = sp->sync;
	      if (sp->sync == 0)
		{
		  /* set up temp sync for snd_sync */
		  sp->sync = ss->sound_sync_max + 1;
		  ss->sound_sync_max++;
		}
	      si = snd_sync(sp->sync);
	      sp->sync = old_sync;
	    }
	  else return(snd_no_such_sound_error(S_make_region, snd_n));
	}
      else
	{
	  chan_info *cp;
	  cp = get_cp(snd_n, chn_n, S_make_region);
	  si = make_simple_sync(cp, ibeg);
	}

      ends = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
      for (i = 0; i < si->chans; i++)
	{
	  if (current_samples(si->cps[i]) - 1 < iend)
	    ends[i] = current_samples(si->cps[i]) - 1;
	  else ends[i] = iend;
	  si->begs[i] = ibeg;
	  if (ends[i] < ibeg) 
	    {
	      free(ends);
	      ends = NULL;
	      si = free_sync_info(si);
	      Xen_out_of_range_error(S_make_region, 1, end, "end < beg?");
	    }
	}
      if (ends)
	{
	  id = define_region(si, ends);
	  if (selection_creates_region(ss))
	    reactivate_selection(si->cps[0], si->begs[0], ends[0]);
	  free_sync_info(si);
	  free(ends);
	}
    }
  return(C_int_to_Xen_region(id));
}


static void save_region_to_xen_error(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  Xen_error(Xen_make_error_type("cannot-save"),
	    Xen_list_2(C_string_to_Xen_string(S_save_region ": can't save ~S"),
		       C_string_to_Xen_string(msg)));
}

#define H_save_region "(" S_save_region " region file sample-type header-type comment): save region in file \
using sample-type (default depends on machine byte order), header-type (" S_mus_next "), and comment"

#if HAVE_SCHEME
static s7_pointer g_save_region(s7_scheme *sc, s7_pointer args)
{
  mus_header_t head_type;
  mus_sample_t samp_type;
  int rg;
  const char *com, *file;
  char *name;
  s7_pointer p, fp, res;

  fp = s7_car(args);
  if (!xen_is_region(fp))
    return(s7_wrong_type_arg_error(sc, S_save_region, 1, fp, "a region"));
  rg = Xen_region_to_C_int(fp);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_save_region, fp));

  fp = s7_cadr(args);
  if (fp == Xen_false)
    Xen_error(Xen_make_error_type("IO-error"),
	      Xen_list_1(C_string_to_Xen_string(S_save_region ": no output file?")));
  if (!s7_is_string(fp))
    return(s7_wrong_type_arg_error(sc, S_save_region, 2, fp, "a string"));
  file = s7_string(fp);
  name = mus_expand_filename(file);
  res = fp;

  p = s7_cddr(args);
  fp = s7_car(p);
  if (fp == Xen_false)
    samp_type = MUS_OUT_SAMPLE_TYPE;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 3, fp, "an integer"));
      samp_type = (mus_sample_t)s7_integer(fp);
    }

  fp = s7_cadr(p);
  if (fp == Xen_false)
    head_type = MUS_NEXT;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 4, fp, "an integer"));
      head_type = (mus_header_t)s7_integer(fp);
    }

  if (!(mus_header_writable(head_type, samp_type))) 
    {
      if (mus_header_writable(MUS_NEXT, samp_type))
	head_type = MUS_NEXT;
      else
	{
	  if (mus_header_writable(MUS_RIFF, samp_type))
	    head_type = MUS_RIFF;
	  else head_type = MUS_RAW;
	}
    }

  fp = s7_caddr(p);
  if (fp == Xen_false)
    com = NULL;
  else
    {
      if (!s7_is_string(fp))
	return(s7_wrong_type_arg_error(sc, S_save_selection, 5, fp, "a string"));
      com = s7_string(fp);
    }
  
  redirect_snd_error_to(save_region_to_xen_error, NULL); /* could perhaps pass name here for free in case of error */
  save_region(rg, name, samp_type, head_type, com);
  redirect_snd_error_to(NULL, NULL);

  if (name) free(name);
  return(res);
}
#else
static Xen kw_header_type, kw_comment, kw_file, kw_sample_type;

static void init_region_keywords(void)
{
  kw_header_type = Xen_make_keyword("header-type");
  kw_sample_type = Xen_make_keyword("sample-type");
  kw_comment = Xen_make_keyword("comment");
  kw_file = Xen_make_keyword("file");
}

static Xen g_save_region(Xen n, Xen arg1, Xen arg2, Xen arg3, Xen arg4, Xen arg5, Xen arg6, Xen arg7, Xen arg8)
{
  char *name = NULL;
  const char *file = NULL, *com = NULL;
  int rg, true_args;
  mus_sample_t sample_type = MUS_OUT_SAMPLE_TYPE;
  mus_header_t header_type = MUS_NEXT;
  Xen args[8]; 
  Xen keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;

  keys[0] = kw_file;
  keys[1] = kw_sample_type;
  keys[2] = kw_header_type;
  keys[3] = kw_comment;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 
  for (true_args = 0; true_args < 8; true_args++)
    if (!Xen_is_bound(args[true_args]))
      break;
  Xen_check_type(xen_is_region(n), n, 1, S_save_region, "a region id");

  rg = Xen_region_to_C_int(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_save_region, n));

  vals = mus_optkey_unscramble(S_save_region, true_args, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_save_region, orig_arg[0], NULL);
      sample_type = (mus_sample_t)mus_optkey_to_int(keys[1], S_save_region, orig_arg[1], (int)sample_type);
      header_type = (mus_header_t)mus_optkey_to_int(keys[2], S_save_region, orig_arg[2], (int)header_type);
      com = mus_optkey_to_string(keys[3], S_save_region, orig_arg[3], NULL);
    }

  if (!file) 
    Xen_error(Xen_make_error_type("IO-error"),
	      Xen_list_1(C_string_to_Xen_string(S_save_region ": no output file?")));

  name = mus_expand_filename(file);

  if (!(mus_header_writable(header_type, sample_type))) 
    {
      if (mus_header_writable(MUS_NEXT, sample_type))
	header_type = MUS_NEXT;
      else
	{
	  if (mus_header_writable(MUS_RIFF, sample_type))
	    header_type = MUS_RIFF;
	  else header_type = MUS_RAW;
	}
    }

  redirect_snd_error_to(save_region_to_xen_error, NULL); /* could perhaps pass name here for free in case of error */
  save_region(rg, name, sample_type, header_type, com);
  redirect_snd_error_to(NULL, NULL);

  if (name) free(name);
  return(args[orig_arg[0] - 1]); /* -> filename, parallel save-selection */
}
#endif

static Xen g_mix_region(Xen reg_n, Xen chn_samp_n, Xen snd_n, Xen chn_n, Xen reg_chn)
{
  #define H_mix_region "(" S_mix_region " region :optional (chn-samp 0) snd chn (region-chan " PROC_TRUE ")): \
mix region's channel region-chan (or all chans if region-chan is " PROC_TRUE ") into snd's channel chn starting at chn-samp; \
it returns a list of the new mixes"

  chan_info *cp;
  mus_long_t samp;
  io_error_t err = IO_NO_ERROR;
  int i, rg, id = -1, reg_chan = 0, reg_chans = 0;
  Xen result = Xen_empty_list;

  Xen_check_type(xen_is_region(reg_n), reg_n, 1, S_mix_region, "a region");
  Xen_check_type(Xen_is_integer_or_unbound(chn_samp_n), chn_samp_n, 2, S_mix_region, "an integer");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(reg_chn), reg_chn, 5, S_mix_region, "an integer or " PROC_TRUE);
  Snd_assert_channel(S_mix_region, snd_n, chn_n, 3);

  rg = Xen_region_to_C_int(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_mix_region, reg_n));

  cp = get_cp(snd_n, chn_n, S_mix_region);
  if (!cp) return(Xen_false);

  if (Xen_is_bound(chn_samp_n))
    samp = beg_to_sample(chn_samp_n, S_mix_region);
  else samp = cursor_sample(cp);

  if (Xen_is_integer(reg_chn))
    reg_chan = Xen_integer_to_C_int(reg_chn);

  id = paste_region_1(rg, cp, true, samp, &err, reg_chan, &reg_chans);

  /* id might legitmately be invalid mix id if with_mix_tags is #f or virtual mix not ok */
  if (is_serious_io_error(err))
    Xen_error(Xen_make_error_type("IO-error"),
	      Xen_list_2(C_string_to_Xen_string(S_mix_region ": can't edit, ~A"),
			 C_string_to_Xen_string(io_error_name(err))));
  
  if (id == -1) return(Xen_false);
  for (i = 0; i < reg_chans; i++)
    result = Xen_cons(new_xen_mix(id + i), result);

  return(Xen_list_reverse(result));
}


static Xen g_region_sample(Xen reg_n, Xen samp_n, Xen chn_n)
{
  #define H_region_sample "(" S_region_sample " region samp :optional (chan 0)): region's sample at samp in chan"

  int rg, chan = 0;
  mus_long_t samp;

  Xen_check_type(xen_is_region(reg_n), reg_n, 1, S_region_sample, "a region");
  Xen_check_type(Xen_is_integer(samp_n), samp_n, 2, S_region_sample, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(chn_n), chn_n, 3, S_region_sample, "an integer");

  if (Xen_is_integer(chn_n)) chan = Xen_integer_to_C_int(chn_n);
  rg = Xen_region_to_C_int(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_sample, reg_n));

  samp = beg_to_sample(samp_n, S_region_sample);
  if ((chan >= 0) && (chan < region_chans(rg)))
    return(C_double_to_Xen_real(region_sample(rg, chan, samp)));
  return(snd_no_such_channel_error(S_region_sample, Xen_list_1(reg_n), chn_n));
}


Xen g_region_to_vct(Xen reg_n, Xen beg_n, Xen num, Xen chn_n, Xen v)
{
  #define H_region_to_vct "(" S_region_to_vct " region :optional (beg 0) samps (chan 0) v): \
write region's samples starting at beg for samps in channel chan to " S_vct " v; return v (or create a new one)"

  int reg, chn = 0;
  mus_long_t len = 0;
  vct *v1 = xen_to_vct(v);

  Xen_check_type(xen_is_region(reg_n), reg_n, 1, S_region_to_vct, "a region");
  Xen_check_type(Xen_is_integer_or_unbound(beg_n), beg_n, 2, S_region_to_vct, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(num), num, 3, S_region_to_vct, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(chn_n), chn_n, 4, S_region_to_vct, "an integer");

  reg = Xen_region_to_C_int(reg_n);
  if (!(region_ok(reg)))
    return(snd_no_such_region_error(S_region_to_vct, reg_n));

  if (Xen_is_integer(chn_n)) 
    {
      chn = Xen_integer_to_C_int(chn_n);
      if ((chn < 0) || (chn >= region_chans(reg)))
	return(snd_no_such_channel_error(S_region_to_vct, Xen_list_1(reg_n), chn_n));
    }
  if (Xen_is_integer(num)) 
    {
      len = Xen_llong_to_C_llong(num);
      if (len < 0)
	Xen_out_of_range_error(S_region_to_vct, 2, num, "length < 0?");
    }
  if ((len == 0) || (len > region_len(reg)))
    len = region_len(reg);

  if (len > 0)
    {
      mus_float_t *data;
      mus_long_t beg;
      beg = beg_to_sample(beg_n, S_region_to_vct);
      if (beg >= region_len(reg)) 
	return(Xen_false);
      if (v1)
	{
	  data = mus_vct_data(v1);
	  if (len > mus_vct_length(v1)) len = mus_vct_length(v1);
	}
      else 
	{
	  if ((beg + len) > region_len(reg))
	    len = region_len(reg) - beg;
	  data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	}
      region_samples(reg, chn, beg, len, data);
      if (v1)
	return(v);
      else return(xen_make_vct(len, data));
    }
  return(Xen_false);
}


static Xen g_region_graph_style(void) {return(C_int_to_Xen_integer(region_graph_style(ss)));}

static Xen g_set_region_graph_style(Xen val) 
{
  int style;
  #define H_region_graph_style "(" S_region_graph_style "): graph style of the region dialog graph. \
The " S_region_graph_style " choices are " S_graph_lines ", " S_graph_dots ", " S_graph_filled ", " S_graph_lollipops ", \
and " S_graph_dots_and_lines "."

  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_region_graph_style, "an integer");

  style = Xen_integer_to_C_int(val);
  if (!is_graph_style(style))
    Xen_out_of_range_error(S_set S_region_graph_style, 1, val, "unknown " S_lisp_graph_style);

  set_region_graph_style((graph_style_t)style);
  reflect_region_graph_style();
  return(val);
}


Xen_wrap_any_args(g_restore_region_w, g_restore_region)
Xen_wrap_4_optional_args(g_insert_region_w, g_insert_region)
Xen_wrap_no_args(g_regions_w, g_regions)
Xen_wrap_2_optional_args(g_region_framples_w, g_region_framples)
Xen_wrap_2_optional_args(g_region_position_w, g_region_position)
Xen_wrap_1_arg(g_region_srate_w, g_region_srate)
Xen_wrap_1_arg(g_region_chans_w, g_region_chans)
Xen_wrap_1_arg(g_region_home_w, g_region_home)
Xen_wrap_1_arg(g_region_maxamp_w, g_region_maxamp)
Xen_wrap_1_arg(g_region_maxamp_position_w, g_region_maxamp_position)
Xen_wrap_1_arg(g_forget_region_w, g_forget_region)
Xen_wrap_4_optional_args(g_make_region_w, g_make_region)
Xen_wrap_5_optional_args(g_mix_region_w, g_mix_region)
Xen_wrap_3_optional_args(g_region_sample_w, g_region_sample)
Xen_wrap_5_optional_args(g_region_to_vct_w, g_region_to_vct)
Xen_wrap_1_arg(g_is_region_w, g_is_region)
Xen_wrap_no_args(g_max_regions_w, g_max_regions)
Xen_wrap_1_arg(g_set_max_regions_w, g_set_max_regions)
Xen_wrap_no_args(g_region_graph_style_w, g_region_graph_style)
Xen_wrap_1_arg(g_set_region_graph_style_w, g_set_region_graph_style)
Xen_wrap_1_arg(g_integer_to_region_w, g_integer_to_region)
Xen_wrap_1_arg(g_region_to_integer_w, g_region_to_integer)

#if HAVE_SCHEME
static s7_pointer acc_region_graph_style(s7_scheme *sc, s7_pointer args) {return(g_set_region_graph_style(s7_cadr(args)));}
static s7_pointer acc_max_regions(s7_scheme *sc, s7_pointer args) {return(g_set_max_regions(s7_cadr(args)));}
#else
Xen_wrap_9_optional_args(g_save_region_w, g_save_region)
#endif


void g_init_regions(void)
{
#if HAVE_SCHEME
  s7_pointer i, b, p, t, r, f, s, fv, rg;
  i = s7_make_symbol(s7, "integer?");
  rg = s7_make_symbol(s7, "region?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "list?");
  f = s7_make_symbol(s7, "float?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  fv = s7_make_symbol(s7, "float-vector?");
  t = s7_t(s7);
#endif

  init_xen_region();
#if (!HAVE_SCHEME)
  init_region_keywords();
#endif

  Xen_define_typed_procedure(S_restore_region,         g_restore_region_w,         0, 0, 1, "internal func used in save-state, restores a region",
			     s7_make_signature(s7, 11, i, i, i, i, i, r, s, s, s, s, p));
  Xen_define_typed_procedure(S_insert_region,          g_insert_region_w,          2, 2, 0, H_insert_region,   s7_make_signature(s7, 5, rg, rg, i, t, t));
  Xen_define_typed_procedure(S_forget_region,          g_forget_region_w,          1, 0, 0, H_forget_region,   s7_make_signature(s7, 2, rg, rg));
  Xen_define_typed_procedure(S_make_region,            g_make_region_w,            0, 4, 0, H_make_region,     s7_make_signature(s7, 5, rg, i, i, t, t));
  Xen_define_typed_procedure(S_mix_region,             g_mix_region_w,             1, 4, 0, H_mix_region,      s7_make_signature(s7, 6, t, rg, i, t, t, t));

  Xen_define_typed_procedure(S_regions,                g_regions_w,                0, 0, 0, H_regions,         s7_make_signature(s7, 1, p));
  Xen_define_typed_procedure(S_region_framples,        g_region_framples_w,        1, 1, 0, H_region_framples, s7_make_signature(s7, 3, i, rg, i));
  Xen_define_typed_procedure(S_region_position,        g_region_position_w,        1, 1, 0, H_region_position, s7_make_signature(s7, 3, i, rg, i));
  Xen_define_typed_procedure(S_region_srate,           g_region_srate_w,           1, 0, 0, H_region_srate,    s7_make_signature(s7, 2, i, rg));
  Xen_define_typed_procedure(S_region_chans,           g_region_chans_w,           1, 0, 0, H_region_chans,    s7_make_signature(s7, 2, i, rg));
  Xen_define_typed_procedure(S_region_home,            g_region_home_w,            1, 0, 0, H_region_home,     s7_make_signature(s7, 2, p, rg));
  Xen_define_typed_procedure(S_region_maxamp,          g_region_maxamp_w,          1, 0, 0, H_region_maxamp,   s7_make_signature(s7, 2, f, rg));
  Xen_define_typed_procedure(S_region_maxamp_position, g_region_maxamp_position_w, 1, 0, 0, H_region_maxamp_position, s7_make_signature(s7, 2, i, rg));
  Xen_define_typed_procedure(S_region_sample,          g_region_sample_w,          2, 1, 0, H_region_sample,   s7_make_signature(s7, 4, f, rg, i, i));
  Xen_define_typed_procedure(S_region_to_vct,          g_region_to_vct_w,          1, 4, 0, H_region_to_vct,   s7_make_signature(s7, 6, fv, rg, i, i, i, fv));
  Xen_define_typed_procedure(S_is_region,              g_is_region_w,              1, 0, 0, H_is_region,       s7_make_signature(s7, 2, b, t));

  Xen_define_typed_procedure(S_integer_to_region,      g_integer_to_region_w,      1, 0, 0, H_integer_to_region, s7_make_signature(s7, 2, rg, i));
  Xen_define_typed_procedure(S_region_to_integer,      g_region_to_integer_w,      1, 0, 0, H_region_to_integer, s7_make_signature(s7, 2, i, rg));

  Xen_define_typed_dilambda(S_max_regions, g_max_regions_w, H_max_regions, S_set S_max_regions, g_set_max_regions_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

  Xen_define_typed_dilambda(S_region_graph_style, g_region_graph_style_w, H_region_graph_style,
			    S_set S_region_graph_style, g_set_region_graph_style_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

#if HAVE_SCHEME
  s7_set_setter(s7, ss->max_regions_symbol, s7_make_function(s7, "[acc-" S_max_regions "]", acc_max_regions, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->region_graph_style_symbol, s7_make_function(s7, "[acc-" S_region_graph_style "]", acc_region_graph_style, 2, 0, false, "accessor"));

  s7_set_documentation(s7, ss->max_regions_symbol, "*max-regions*: max number of regions saved on the region list");
  s7_set_documentation(s7, ss->region_graph_style_symbol, "*region-graph-style*: graph style of the region dialog graph (graph-lines etc)");

  s7_define_safe_function_star(s7, S_save_region, g_save_region, "region file sample-type header-type comment", H_save_region);
#else
  Xen_define_typed_procedure(S_save_region,            g_save_region_w,            2, 7, 0, H_save_region,     s7_make_circular_signature(s7, 0, 1, t));
#endif
}

