#include "snd.h"

/* rather than try to track error indications through infinite levels of nested calls,
 *   I think I'll just issue a warning, and let the redirection mechanism deal with it.
 */

struct snd_io {
  int fd, chans, bufsize;
  mus_long_t framples, beg, end;
  mus_float_t **arrays;
};

mus_long_t io_beg(snd_io *io)
{
  return(io->beg);
}

mus_long_t io_end(snd_io *io)
{
  return(io->end);
}


void snd_remove(const char *name, cache_remove_t forget)
{
  int err;
  if (forget == REMOVE_FROM_CACHE) mus_sound_forget(name); /* no error here if not in sound tables */
  ss->local_errno = 0;
  err = remove(name);
  if (err != 0)
    snd_warning("remove %s: %s", name, snd_io_strerror());
}


void snd_close(int fd, const char *name)
{
  if (fd < 0)
    snd_warning("close %s, fd: %d", name, fd);
  else
    {
      int err;
      ss->local_errno = 0;
      err = close(fd);
      if (err != 0)
	snd_warning("close %s: %s", name, snd_io_strerror());
    }
}


void snd_fclose(FILE *fd, const char *name)
{
  if (!fd)
    snd_warning("fclose %s, fd null!", name);
  else
    {
      int err;
      ss->local_errno = 0;
      err = fclose(fd);
      if (err != 0)
	snd_warning("fclose %s: %s", name, snd_io_strerror());
    }
}


/* low level IO stuff to keep track of errno */

/* OPEN and FOPEN (_sndlib.h) also come here, so any sound file in Snd is opened/closed here */

FILE *snd_fopen(const char *filename, const char *modes)
{
  FILE *result = NULL;
  ss->local_errno = 0;
  ss->local_open_errno = 0;
  errno = 0;
  result = fopen(filename, modes);
  if (errno != 0) 
    {
      ss->local_errno = errno;
      ss->local_open_errno = errno;
    }
  return(result);
}


int snd_open(const char *filename, int flags, mode_t mode)
{
  int result = 0;
  ss->local_errno = 0;
  ss->local_open_errno = 0;
  errno = 0;
#if (defined(_MSC_VER) || __CYGWIN__)
  result = open(filename, flags);
#else
  result = open(filename, flags, mode);
#endif
  if (errno != 0) 
    {
      ss->local_errno = errno;
      ss->local_open_errno = errno;
    }
  return(result);
}


int snd_creat(const char *filename, mode_t mode)
{
  int result = 0;
  ss->local_errno = 0;
  ss->local_open_errno = 0;
  errno = 0;
  result = creat(filename, mode);
  if (errno != 0) 
    {
      ss->local_errno = errno;
      ss->local_open_errno = errno;
    }
  return(result);
}


io_error_t move_file(const char *oldfile, const char *newfile)
{
  io_error_t err = IO_NO_ERROR;
  int rename_err;
  rename_err = rename(oldfile, newfile);
  if (rename_err != 0)
    {
      if (errno == EXDEV)
	{
	  err = copy_file(oldfile, newfile);
	  if (err == IO_NO_ERROR)
	    snd_remove(oldfile, REMOVE_FROM_CACHE);
	}
    }
  return(err);
}


io_error_t copy_file(const char *oldname, const char *newname)
{
  /* make newname a copy of oldname */
  int ifd, ofd;
  mus_long_t bytes, wb;
  char *buf = NULL;
 
 ifd = OPEN(oldname, O_RDONLY, 0);
  if (ifd == -1) return(IO_CANT_OPEN_FILE);

  ofd = CREAT(newname, 0666);
  if (ofd == -1) 
    {
      snd_close(ifd, oldname);
      return(IO_CANT_CREATE_FILE);
    }

  buf = (char *)malloc(8192 * sizeof(char));
  while ((bytes = read(ifd, buf, 8192)))
    {
      wb = write(ofd, buf, bytes);
      if (wb != bytes) 
	{
	  snd_close(ofd, newname);
	  snd_close(ifd, oldname);
	  free(buf); 
	  return(IO_WRITE_ERROR);
	}
    }

  snd_close(ifd, oldname);
  wb = disk_kspace(newname);
  snd_close(ofd, newname);
  free(buf);
  if (wb < 0)
    return(IO_DISK_FULL);
  return(IO_NO_ERROR);
}



/* file buffers (i.e. a sliding window on a given file's data) */

static void c_io_bufclr(snd_io *io, int beg)
{
  int k;
  size_t bytes;

  bytes = (io->bufsize - beg) * sizeof(mus_float_t);
  for (k = 0; k < io->chans; k++)
    {
      mus_float_t *j;
      j = io->arrays[k];
      if (j)
	memset((void *)(j + beg), 0, bytes);
    }
}


static void reposition_file_buffers_1(mus_long_t loc, snd_io *io)
{
  /* called when loc is outside the current in-core frample for the file pointed to by io */
  mus_long_t framples;
  /* local framples is buffer-local, not a sample number. */

  framples = io->framples - loc;         /* io->framples is total samps in file */
  if (framples > io->bufsize) framples = io->bufsize;
  if (framples <= 0)                   /* tried to access beyond current end of file */
    {
      io->beg = loc; 
      c_io_bufclr(io, 0);
    }
  else
    {
      mus_file_seek_frample(io->fd, loc);
      io->beg = loc;
      mus_file_read_chans(io->fd,
			  loc, framples,
			  io->chans,
			  io->arrays,
			  io->arrays);
      if (framples < (io->bufsize - 1)) 
	c_io_bufclr(io, framples);
    }
  io->end = io->beg + io->bufsize - 1;
}


static void reposition_file_buffers(snd_data *sd, mus_long_t index)
{
  int fd = 0;
  bool reclose = false;
  if (index < 0) index = 0; /* if reading in reverse, don't fall off the start of the buffer */
  if (sd->open == FD_CLOSED)
    {
      file_info *hdr;
      /* try to open it with sndlib descriptors */
      fd = mus_file_open_read(sd->filename); 
      if (fd == -1) 
	{
	  /* our file has disappeared?!? */
	  snd_error("%s is unreadable: %s?", sd->filename, snd_io_strerror());
	  return;
	}
      hdr = sd->hdr;
      /* these need to flush active data before hidden close and fixup the io indices */
      snd_file_open_descriptors(fd,
				sd->filename,
				hdr->sample_type,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, sd->filename, SND_REOPEN_CLOSED_FILE);
      /* fix up io->fd and whatever else is clobbered by mus_file_close */
      sd->io->fd = fd;
      sd->open = FD_OPEN;
      reclose = true;
    }
  reposition_file_buffers_1(index, sd->io);
  if (reclose)
    {
      sd->open = FD_CLOSED; 
      sd->io->fd = -1;
      mus_file_close(fd); 
    }
}


snd_io *make_file_state(int fd, file_info *hdr, int chan, mus_long_t beg, int suggested_bufsize)
{
  snd_io *io;
  int bufsize;
  mus_long_t chansize;
  #define MAX_FILE_BUFFER_SIZE 65536

  bufsize = suggested_bufsize;

  chansize = (hdr->samples / hdr->chans); /* this can be bogus if the header is messed up */
  if ((chansize >= 0) && 
      ((bufsize > chansize) ||
       (MAX_FILE_BUFFER_SIZE > chansize)))
    bufsize = chansize + 1;

  io = (snd_io *)calloc(1, sizeof(snd_io)); /* only creation point */
  io->arrays = (mus_float_t **)calloc(hdr->chans, sizeof(mus_float_t *));
  io->fd = fd;
  io->chans = hdr->chans;
  io->framples = chansize;
  io->beg = 0;
  io->end = bufsize - 1;
  io->bufsize = bufsize;
  io->arrays[chan] = (mus_float_t *)malloc(bufsize * sizeof(mus_float_t));
  io->arrays[chan][bufsize - 1] = 0.0; /* there is buffer bounds confusion somewhere... */
  reposition_file_buffers_1(beg, io);  /* get ready to read -- we're assuming mus_file_read_chans here */
  return(io);
}


void file_buffers_forward(mus_long_t ind0, mus_long_t ind1, mus_long_t indx, snd_fd *sf, snd_data *cur_snd)
{
  /* need to track in-core buffer and file-relative index */
  if ((indx < cur_snd->io->beg) ||
      (indx > cur_snd->io->end)) 
    reposition_file_buffers(cur_snd, indx);
  sf->loc = indx - cur_snd->io->beg;
  if (ind0 >= cur_snd->io->beg)
    sf->first = ind0 - cur_snd->io->beg;
  else sf->first = 0;
  if (ind1 <= cur_snd->io->end) 
    sf->last = ind1 - cur_snd->io->beg;
  else sf->last = cur_snd->io->bufsize - 1;
}


void file_buffers_back(mus_long_t ind0, mus_long_t ind1, mus_long_t indx, snd_fd *sf, snd_data *cur_snd)
{
  if ((indx > cur_snd->io->end) || 
      (indx < cur_snd->io->beg)) 
    reposition_file_buffers(cur_snd, indx - cur_snd->io->bufsize + 1);
  sf->loc = indx - cur_snd->io->beg;
  if (ind1 <= cur_snd->io->end)
    sf->last = ind1 - cur_snd->io->beg;
  else sf->last = cur_snd->io->bufsize - 1;
  if (ind0 >= cur_snd->io->beg) 
    sf->first = ind0 - cur_snd->io->beg;
  else sf->first = 0;
}


/* wrappers for low level sndlib open/close/access functions -- we can't use
 * the sndlib versions directly because in some cases, Snd has more than FOPEN_MAX
 * files nominally open and accessible (mix temps in with-sound explode for example).
 * these wrappers provide checks for EMFILE as errno from open and try to close
 * temps to make room. 
 *
 * there is a hidden limit that might come into play if FOPEN_MAX > MUS_FILE_DESCRIPTORS (see io.c)
 * on the SGI, FOPEN_MAX is 100, but we can open many more files than that without hitting the EMFILE error.
 */

static void close_temp_files(chan_info *cp, int *closed)
{
  if ((cp) && (cp->sounds))
    {
      int i, rtn;
      rtn = (*closed);
      for (i = 0; i < cp->sound_size; i++)
	{
	  snd_data *sd;
	  sd = cp->sounds[i];
	  if ((sd) && 
	      (sd->type == SND_DATA_FILE) && 
	      (sd->io) && 
	      (sd->open == FD_OPEN))
	    {
	      int fd;
	      fd = sd->io->fd;
	      sd->open = FD_CLOSED;
	      sd->io->fd = -1;
	      mus_file_close(fd);
	      rtn++;
	    }
	}
      (*closed) = rtn;
    }
}


static int too_many_files_cleanup(void)
{
  int *closed;
  int rtn;
  closed = (int *)malloc(sizeof(int));
  (*closed) = 0;
  for_each_normal_chan_with_refint(close_temp_files, closed);
  if ((*closed) == 0) 
    for_each_region_chan_with_refint(close_temp_files, closed);
  if ((*closed) == 0)
    rtn = -1;
  else rtn = (*closed);
  free(closed);
  return(rtn);
}


int snd_open_read(const char *arg) 
{
  int fd;
  fd = OPEN(arg, O_RDONLY, 0);
  if ((fd == -1) && (errno == EMFILE)) /* there's also ENFILE = file table overflow (/usr/include/asm/errno.h) */
    {
      fd = too_many_files_cleanup();
      if (fd != -1) 
	fd = OPEN(arg, O_RDONLY, 0);
      if (fd == -1) 
	snd_error("%s: %s", arg, snd_io_strerror());
    }
  return(fd);
}

int snd_reopen_write(const char *arg)
{
  int fd;

  fd = OPEN(arg, O_RDWR, 0);
  if ((fd == -1) && 
      (errno == EMFILE))
    {
      fd = too_many_files_cleanup();
      if (fd != -1) 
	fd = OPEN(arg, O_RDWR, 0);
      if (fd == -1) 
	snd_error("%s: %s", arg, snd_io_strerror());
    }
  return(fd);
}


static int local_mus_error = MUS_NO_ERROR;
static mus_error_handler_t *old_error_handler;

static void local_mus_error_to_snd(int type, char *msg) 
{
  local_mus_error = type;
  if (ss->io_error_info) free(ss->io_error_info);
  ss->io_error_info = mus_strdup(msg);
}


io_error_t sndlib_error_to_snd(int sndlib_err)
{
  /* "mus_error" in sndlib is an int that includes all kinds of error conditions that
   *    aren't relevant to file IO in Snd; I need to translate to a more restrictive
   *    set to make it easier to generate informative error messages.
   */
  if (sndlib_err >= 0)
    switch (sndlib_err)
      {
      case MUS_NO_ERROR:                 return(IO_NO_ERROR);
      case MUS_MEMORY_ALLOCATION_FAILED: return(IO_NO_MEMORY);
      case MUS_CANT_OPEN_FILE:           return(IO_CANT_OPEN_FILE);
      case MUS_NO_SUCH_CHANNEL:          return(IO_BAD_CHANNEL);
      case MUS_NO_FILE_NAME_PROVIDED:    return(IO_NO_FILENAME);
      case MUS_UNSUPPORTED_SAMPLE_TYPE:  return(IO_BAD_SAMPLE_TYPE);
      case MUS_HEADER_READ_FAILED:       return(IO_BAD_HEADER);
      case MUS_UNSUPPORTED_HEADER_TYPE:  return(IO_BAD_HEADER_TYPE);
      case MUS_FILE_DESCRIPTORS_NOT_INITIALIZED: return(IO_SNDLIB_UNINITIALIZED);
      case MUS_NOT_A_SOUND_FILE:         return(IO_NOT_A_SOUND_FILE);
      case MUS_FILE_CLOSED:              return(IO_FILE_CLOSED);
      case MUS_WRITE_ERROR:              return(IO_WRITE_ERROR);
      case MUS_HEADER_WRITE_FAILED:      return(IO_CANT_REOPEN_FILE);
      case MUS_CANT_OPEN_TEMP_FILE:      return(IO_CANT_OPEN_FILE);
      case MUS_INTERRUPTED:              return(IO_INTERRUPTED);
      case MUS_CANT_CLOSE_FILE:          return(IO_CANT_CLOSE_FILE);
      }
  return(IO_UNKNOWN_SNDLIB_ERROR);
}


int snd_file_open_descriptors(int fd, const char *name, mus_sample_t samp_type, mus_long_t location, int chans, mus_header_t type)
{
  int sl_err;

  sl_err = mus_file_open_descriptors(fd, name, samp_type, mus_bytes_per_sample(samp_type), location, chans, type);
  if (sl_err != MUS_NO_ERROR)
    snd_warning("%s: open file descriptors: %s", name, mus_error_type_to_string(sl_err));

  if (mus_sound_saved_data(name))
    mus_file_save_data(fd, mus_sound_framples(name), mus_sound_saved_data(name));

  return(sl_err);
}


io_error_t snd_write_header(const char *name, mus_header_t head_type, int srate, int chans,
			    mus_long_t samples, mus_sample_t samp_type, const char *comment,
			    int *loops)
{
  int err; /* sndlib-style error */
  /* trap mus_error locally here so that callers of open_temp_file can cleanup samplers and whatnot */

  local_mus_error = MUS_NO_ERROR;
  old_error_handler = mus_error_set_handler(local_mus_error_to_snd);
  mus_sound_forget(name);
  mus_header_set_aiff_loop_info(loops);

  err = mus_write_header(name, head_type, srate, chans, samples, samp_type, comment);
  /* err here is a mus error */
  if (err != MUS_NO_ERROR)
    {
      if (errno == EMFILE) /* 0 => no error (err not actually returned unless it's -1) */
	{
	  err = too_many_files_cleanup();
	  if (err != -1) 
	    mus_write_header(name, head_type, srate, chans, samples, samp_type, comment);
	  else 
	    {
	      mus_error_set_handler(old_error_handler);
	      return(IO_TOO_MANY_OPEN_FILES);
	    }
	}
    }
  else mus_header_set_aiff_loop_info(NULL);
  mus_error_set_handler(old_error_handler);
  return(sndlib_error_to_snd(local_mus_error));
}


/* there are a few special-case multi-channel temp files that need a kind of reference count to handle deletion */
/* this machinery affects only these special cases, not temp files in general */

typedef struct {
  char *name;
  int chans;
  int *ticks;
} tempfile_ctr;

static tempfile_ctr **tempfiles = NULL;
static int tempfiles_size = 0;

void remember_temp(const char *filename, int chans)
{
  int i;
  tempfile_ctr *tmp = NULL;

  if (tempfiles_size == 0)
    {
      tempfiles_size = 8;
      tempfiles = (tempfile_ctr **)calloc(tempfiles_size, sizeof(tempfile_ctr *));
      i = 0;
    }
  else
    {
      for (i = 0; i < tempfiles_size; i++)
	if ((tempfiles[i]) &&
	    (mus_strcmp(filename, tempfiles[i]->name)))
	  return;

      for (i = 0; i < tempfiles_size; i++)
	if (!tempfiles[i])
	  break;

      if (i >= tempfiles_size)
	{
	  int old_size;
	  old_size = tempfiles_size;
	  tempfiles_size += 8;
	  tempfiles = (tempfile_ctr **)realloc(tempfiles, tempfiles_size * sizeof(tempfile_ctr *));
	  for (i = old_size; i < tempfiles_size; i++) tempfiles[i] = NULL;
	  i = old_size;
	}
    }

  tmp = (tempfile_ctr *)malloc(sizeof(tempfile_ctr));
  tempfiles[i] = tmp;
  tmp->name = mus_strdup(filename);
  tmp->chans = chans;
  tmp->ticks = (int *)calloc(chans, sizeof(int));
}


void forget_temp(const char *filename, int chan)
{
  int i, j;
  for (i = 0; i < tempfiles_size; i++)
    {
      tempfile_ctr *tmp;
      tmp = tempfiles[i];
      if ((tmp) && 
	  (mus_strcmp(filename, tmp->name)))
	{
	  tmp->ticks[chan]--;
	  for (j = 0; j < tmp->chans; j++)
	    if (tmp->ticks[j] > 0) 
	      return;
	  snd_remove(tmp->name, REMOVE_FROM_CACHE);
	  free(tmp->name);
	  free(tmp->ticks);
	  free(tmp);
	  tempfiles[i] = NULL;
	  return;
	}
    }
}


static void tick_temp(const char *filename, int chan)
{
  int i;
  for (i = 0; i < tempfiles_size; i++)
    {
      tempfile_ctr *tmp;
      tmp = tempfiles[i];
      if ((tmp) && 
	  (mus_strcmp(filename, tmp->name)))
	{
	  tmp->ticks[chan]++;
	  return;
	}
    }
}


void forget_temps(void)
{
  int i;
  for (i = 0; i < tempfiles_size; i++)
    if ((tempfiles[i]) && (mus_file_probe(tempfiles[i]->name)))
      snd_remove(tempfiles[i]->name, REMOVE_FROM_CACHE);
}


snd_data *make_snd_data_file(const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int ctr, int temp_chan)
{
  snd_data *sd;
  sd = (snd_data *)malloc(sizeof(snd_data));
  sd->type = SND_DATA_FILE;
  sd->buffered_data = io->arrays[temp_chan];
  sd->io = io;
  sd->filename = mus_strdup(name);
  sd->hdr = hdr;
  sd->temporary = temp;
  if ((temp == MULTICHANNEL_DELETION) || (temp == MULTICHANNEL_DELETION_IF_FILE)) tick_temp(name, temp_chan);
  sd->edit_ctr = ctr;
  sd->open = FD_OPEN;
  sd->inuse = false;
  sd->copy = false;
  sd->chan = temp_chan;
  sd->data_bytes = (hdr->samples) * (mus_bytes_per_sample(hdr->sample_type)) + hdr->data_location;
  sd->free_me = false;
  return(sd);
}


snd_data *copy_snd_data(snd_data *sd, mus_long_t beg, int bufsize)
{
  snd_data *sf;
  snd_io *io;
  int fd;
  file_info *hdr;
  hdr = sd->hdr;
  fd = snd_open_read(sd->filename);
  if (fd == -1) 
    return(NULL);
  snd_file_open_descriptors(fd,
			    sd->filename,
			    hdr->sample_type,
			    hdr->data_location,
			    hdr->chans,
			    hdr->type);
  during_open(fd, sd->filename, SND_COPY_READER);
  io = make_file_state(fd, hdr, sd->chan, beg, bufsize);
  sf = (snd_data *)malloc(sizeof(snd_data));
  sf->type = sd->type;
  sf->buffered_data = io->arrays[sd->chan];
  sf->io = io;
  sf->filename = mus_strdup(sd->filename);
  sf->hdr = hdr;
  sf->temporary = DONT_DELETE_ME;
  sf->edit_ctr = sd->edit_ctr;

  /* 17-Nov-14: this was sf->open = FD_OPEN; but I can't think of any reason to leave it open */
  sf->open = FD_CLOSED; 
  sf->io->fd = -1;
  mus_file_close(fd); 

  sf->inuse = false;
  sf->copy = true;
  sf->chan = 0;
  sf->data_bytes = 0;
  sf->free_me = false;
  return(sf);
}


snd_data *make_snd_data_buffer(mus_float_t *data, int len, int ctr)
{
  snd_data *sf;

  sf = (snd_data *)calloc(1, sizeof(snd_data));
  sf->type = SND_DATA_BUFFER;
  sf->buffered_data = (mus_float_t *)malloc((len + 1) * sizeof(mus_float_t));
  /* sigh... using len + 1 rather than len to protect against access to inserted buffer at end mixups (final fragment uses end + 1)
   *   the real problem here is that I never decided whether insert starts at the cursor or just past it
   *   when the cursor is on the final sample, this causes cross-fragment ambiguity as to the length of a trailing insertion
   *   C > (make-region 1000 2000) (insert-region (cursor)) C-v hits this empty slot and gets confused about the previously final sample value 
   */

  mus_copy_floats(sf->buffered_data, data, len);
  sf->buffered_data[len] = 0.0;
  sf->edit_ctr = ctr;
  sf->copy = false;
  sf->inuse = false;
  sf->data_bytes = len * sizeof(mus_float_t);
  return(sf);
}


snd_data *make_snd_data_buffer_for_simple_channel(int len)
{
  snd_data *sf;
  sf = (snd_data *)calloc(1, sizeof(snd_data));
  sf->type = SND_DATA_BUFFER;
  sf->buffered_data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
  sf->edit_ctr = 0;
  sf->copy = false;
  sf->inuse = false;
  sf->data_bytes = len * sizeof(mus_float_t);
  return(sf);
}


snd_data *free_snd_data(snd_data *sd)
{
  if (sd)
    {
      if (!(sd->inuse))
	{
	  /* assume the inuse cases will eventually be freed the GC.
	   *   this can happen if a sampler is created, and forgotten,
	   *   and the associated sound is closed.  The closing runs through
	   *   the snd_data (sounds) list freeing the descriptors, but the
	   *   forgotten sampler is still idle somewhere thinking it
	   *   might someday find a use for itself...
	   */
	  if (sd->temporary == ALREADY_DELETED)
	    return(NULL);
	  if (sd->temporary == MULTICHANNEL_DELETION)
	    forget_temp(sd->filename, sd->chan);
	  if ((sd->type == SND_DATA_BUFFER) && 
	      (sd->buffered_data)) 
	    free(sd->buffered_data);
	  sd->buffered_data = NULL;
	  if ((!(sd->copy)) && 
	      (sd->hdr)) 
	    free_file_info(sd->hdr);
	  sd->hdr = NULL;
	  if (sd->io)
	    {
	      int i, chans;
	      if (sd->open == FD_OPEN) mus_file_close(sd->io->fd);

	      /* free the IO buffers as well as the descriptor buffer */
	      chans = sd->io->chans;
	      for (i = 0; i < chans; i++)
		if (sd->io->arrays[i]) 
		  free(sd->io->arrays[i]);
	      free(sd->io->arrays);
	      free(sd->io);
	      sd->io = NULL;
	      if (sd->temporary == DELETE_ME)
		snd_remove(sd->filename, REMOVE_FROM_CACHE);
	    }
	  if (sd->filename) free(sd->filename);
	  sd->filename = NULL;
	  sd->temporary = ALREADY_DELETED;
	  sd->copy = false;
	  sd->type = SND_DATA_NO_DATA;
	  free(sd);
	}
      else 
	{
	  sd->free_me = true;
	}
    }
  return(NULL);
}


int open_temp_file(const char *ofile, int chans, file_info *hdr, io_error_t *err)
{
  /* returns io fd */
  int ofd, sl_err = MUS_NO_ERROR;
  if (!(mus_header_writable(hdr->type, hdr->sample_type)))
    {
      hdr->type = default_output_header_type(ss);
      if (mus_header_writable(hdr->type, default_output_sample_type(ss)))
	hdr->sample_type = default_output_sample_type(ss);
      else
	{
	  /* was default_output_* here, but that's for the user's output, not ours */
	  hdr->type = MUS_NEXT;
	  hdr->sample_type = MUS_OUT_SAMPLE_TYPE;
	}
    }
  (*err) = snd_write_header(ofile, hdr->type, hdr->srate, chans, 0, hdr->sample_type, hdr->comment, hdr->loops);
  if ((*err) != IO_NO_ERROR)
    {
      /* -1 as fd */
      return(-1);
    }
  ofd = snd_reopen_write(ofile);
  if (ofd == -1)
    {
      (*err) = IO_CANT_REOPEN_FILE;
      return(-1);
    }
  hdr->data_location = mus_header_data_location(); /* header might have changed size (aiff extras) */
  sl_err = snd_file_open_descriptors(ofd,
				     ofile,
				     hdr->sample_type,
				     hdr->data_location,
				     chans,
				     hdr->type);
  if (sl_err != MUS_NO_ERROR)
    (*err) = sndlib_error_to_snd(sl_err);
  lseek(ofd, hdr->data_location, SEEK_SET);
  return(ofd);
}


io_error_t close_temp_file(const char *filename, int ofd, mus_header_t type, mus_long_t bytes)
{
  int err;
  err = mus_file_close(ofd);
  if (err == MUS_NO_ERROR)
    {
      local_mus_error = MUS_NO_ERROR;
      old_error_handler = mus_error_set_handler(local_mus_error_to_snd);
      mus_header_change_data_size(filename, type, bytes);
      mus_sound_forget(filename);
      mus_error_set_handler(old_error_handler);
      return(sndlib_error_to_snd(local_mus_error));
    }
  return(sndlib_error_to_snd(err));
}



void set_up_snd_io(chan_info *cp, int i, int fd, const char *filename, file_info *hdr, bool post_close)
{
  snd_io *io;
  snd_file_open_descriptors(fd,
			    filename,
			    hdr->sample_type,
			    hdr->data_location,
			    hdr->chans,
			    hdr->type);
  io = make_file_state(fd, hdr, i, 0,
		       (post_close) ? MAX_BUFFER_SIZE : FILE_BUFFER_SIZE);
  cp->sounds[0] = make_snd_data_file(filename, io,
				     copy_header(hdr->name, hdr),
				     DONT_DELETE_ME, cp->edit_ctr, i);
  if (post_close) 
    {
      snd_data *sd;
      sd = cp->sounds[0]; 
      sd->open = FD_CLOSED; 
      io->fd = -1;
      if (mus_file_close(fd) != 0)
	snd_error("can't close file %s: %s", filename, snd_io_strerror());
    }
  /* this is not as crazy as it looks -- we've read in the first 64K (or whatever) samples,
   * and may need this file channel for other opens, so this file can be closed until reposition_file_state_buffers
   */
}
