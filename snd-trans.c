/* translate various special case sound files to something we can edit 
 *   (this code is going away...)
 *
 * I'm ignoring proprietary or licensed schemes even where the code is publicly available (Rockwell ADPCM, etc)
 *
 * currently supported:
 *   IEEE text
 *   Mus10 16-bit SAM (mode 4)
 *   HCOM (from Sox)
 *   shortpack NIST
 *   Dvi-Intel (IMA) ADPCM RIFF (comes in 3 and 4 bit flavors, but just 4-bit here) (MS and Apple are variations of this)
 *   MIDI sample dump
 *   Oki (Dialogic) ADPCM (RIFF)
 *   IBM ADPCM (as per Perry Cook)
 *   Yamaha TX-16 12-bit
 *
 *   libavcodec has an amazing number of decoders -- are these useful in this context?
 *   also libmpcodecs (available with MPlayer)
 */

#include "snd.h"

#define TRANS_BUF_SIZE 8192

static char write_error_buffer[PRINT_BUFFER_SIZE];

static int64_t snd_checked_write(int fd, uint8_t *buf, int64_t bytes, const char *filename)
{
  /* io.c checked_write assumes its file descriptors are around */
  /* can't call mus_error here because we need to clean up first in case of error */
  int64_t bytes_written;
  mus_long_t kfree;
  kfree = (int64_t)disk_kspace(filename);
  if (kfree < 0) 
    {
      snprintf(write_error_buffer, PRINT_BUFFER_SIZE,
		   "no space left on device: %s",
		   snd_io_strerror()); 
      return(MUS_ERROR);
    }
  if (kfree < (bytes >> 10))
    { 
      snprintf(write_error_buffer, PRINT_BUFFER_SIZE,
		   "only %" print_mus_long " bytes left on device (we need %" print_mus_long " bytes)",
		   kfree << 10, bytes);
      return(MUS_ERROR);
    }
  bytes_written = write(fd, buf, bytes);
  if (bytes_written != bytes)
    {
      snprintf(write_error_buffer, PRINT_BUFFER_SIZE,
		   "write error (wrote %" print_mus_long " of requested %" print_mus_long " bytes): %s",
		   bytes_written, bytes, snd_io_strerror());
      return(MUS_ERROR);
    }
  return(bytes_written);
}


static int be_snd_checked_write(int fd, uint8_t *buf, int bytes, const char *filename)
{
  /* handle little-endian swap if necessary */
#if MUS_LITTLE_ENDIAN
  int i;
  for (i = 0; i < bytes; i += 2)
    {
      uint8_t tmp;
      tmp = buf[i];
      buf[i] = buf[i + 1];
      buf[i + 1] = tmp;
    }
#endif
  return(snd_checked_write(fd, buf, bytes, filename));
}


#define return_mus_io_error(IO_Func, IO_Name) return(mus_error(MUS_CANT_OPEN_FILE, "translator: %s(%s) %s", IO_Func, IO_Name, snd_io_strerror()))

#define return_mus_write_error(OldName, NewName) \
  do { \
      mus_error(MUS_WRITE_ERROR, "can't translate %s to %s:\n  %s", OldName, NewName, write_error_buffer); \
      write_error_buffer[0] = '\0'; \
      return(MUS_ERROR); \
    } \
  while (false)

#define return_mus_alloc_error(OldName, Bytes, VariableName) \
  return(mus_error(MUS_MEMORY_ALLOCATION_FAILED, "translate %s: can't allocate %d bytes for %s", OldName, Bytes, VariableName))


/* I'm using the same variable names in most cases below, so these two macros save lots of repetition */

#define CLEANUP(OldName, NewName)		\
  do { \
    if (fs != -1) snd_close(fs, NewName); \
    if (fd != -1) snd_close(fd, OldName); \
      if (buf) free(buf); \
     } \
  while (false)


#define STARTUP(OldName, NewName, BufSize, BufType) \
  do { \
    fs = CREAT(NewName, 0666); \
    if (fs == -1) return_mus_io_error("create", NewName); \
    fd = OPEN(OldName, O_RDONLY, 0); \
    if (fd == -1) \
      { \
        CLEANUP(OldName, NewName); \
        return_mus_io_error("open", OldName); \
      } \
    buf = (BufType *)calloc(BufSize, sizeof(BufType)); \
    if (!buf) \
      { \
        CLEANUP(OldName, NewName); \
        return_mus_alloc_error(OldName, BufSize, "buf"); \
      } \
    } \
  while (false)




/* -------------------------------- MIDI sample dump -------------------------------- */

/* F0 7E <ID> 01 ss ss ee ff ff ff gg gg gg hh hh hh ii ii ii jj f7
 * ss: sample# (LSB MSB), ee: #bits, ff: 1/srate in nsec, gg: samples, hh: loop ii: loop jj: loop 
 * 0000000       f07e 0001 0000 1048 5007 7479 0000 0000
 * 0000020       0000 007f f7f0 7e00 0200 4000 003f 7140
 */

static int read_midi_sample_dump(const char *oldname, const char *newname, char *hdr)
{
  int fs = -1, fd = -1, err = MUS_NO_ERROR, chans, srate, inp, outp;
  ssize_t totalin;
  bool happy;
  int val = 0, bits, block_count, header_count, state, samples, shift1, shift2, offset;
  int osp;
  uint8_t *buf = NULL;
  chans = 1;
  STARTUP(oldname, newname, TRANS_BUF_SIZE, uint8_t);
  totalin = read(fd, buf, TRANS_BUF_SIZE);
  bits = buf[6];
  srate = (int)(1.0e9 / (double)((buf[7] + (buf[8] << 7) + (buf[9] << 14))));
  samples = (buf[10] + (buf[11] << 7) + (buf[12] << 14));
  mus_bint_to_char((uint8_t *)(hdr + 16), srate);
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  if (bits == 16) 
    mus_bint_to_char((uint8_t *)(hdr + 8), samples * 2); 
  else mus_bint_to_char((uint8_t *)(hdr + 8), samples);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  happy = true;
  inp = 21;
  block_count = 120;
  state = 2;
  header_count = 5;
  outp = 0;
  osp = 0;
  /* we could be really wacked out and implement any sample width here */
  if (bits == 16) 
    {
      shift1 = 9; 
      shift2 = 5;
      offset = 32768;
    }
  else 
    {
      shift1 = 1;
      shift2 = 6;
      offset = 128;
    }
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = false;
	  else 
	    {
	      totalin = read(fd, buf, TRANS_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs, (uint8_t *)hdr, TRANS_BUF_SIZE, newname) == MUS_ERROR) 
	    {
	      CLEANUP(oldname, newname);
	      return_mus_write_error(oldname, newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (state != 2) 
	    {
	      block_count--; 
	      if (block_count == 0) 
		{
		  state = 2; 
		  header_count = 7;
		}
	    }
	  switch (state)
	    {
	    case 0: 
	      /* val = buf[inp];  */
	      /* hmmm...  I wonder about this -- the MIDI spec says LSB first,
	       *   but the Goldwave midi sample dump output sends MSB first.
	       * I bet this is a bug 
	       */
	      val = buf[inp] << shift1;
	      state = ((bits == 16) ? 1 : 3); 
	      break;
	    case 1: 
	      /* val |= (buf[inp] << 7);  */
	      val |= (buf[inp] << 2);
	      state = 3; 
	      break;
	    case 2: 
	      header_count--; 
	      if (header_count == 0) 
		{
		  state = 0; 
		  block_count = 121;
		} 
	      break;
	    case 3: 
	      /* val |= (buf[inp] << shift1);  */
	      val |= (buf[inp] >> shift2);
	      state = 0; 
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)(val - offset));
	      osp += 2;
	      outp += 2; 
	      break;
	    }
	  inp++;
	}
    }
  if (outp > 0) err = snd_checked_write(fs, (uint8_t *)hdr, outp, newname);
  CLEANUP(oldname, newname);
  if (err == MUS_ERROR) return_mus_write_error(oldname, newname);
  return(MUS_NO_ERROR);
}



/* -------------------------------- IEEE TEXT -------------------------------- */

static int read_ieee_text(const char *oldname, const char *newname, char *hdr)
{
  /* from untext.c */
  /* look for "%sampling rate: nn.nn KHz\n", also get end of to comment (i.e. data location) */
  char str[32];
  char *buf = NULL;
  int fd = -1, fs = -1;
  ssize_t totalin;
  int inp, outp, op, i, j, s0, srate, err = MUS_NO_ERROR;
  bool commenting, happy;
  float fsrate;
  int osp;
  STARTUP(oldname, newname, TRANS_BUF_SIZE, char);
  totalin = read(fd, buf, TRANS_BUF_SIZE);      
  commenting = true;
  inp = 0;
  outp = 24;
  srate = 0;
  op = 0;
  while ((commenting) && (inp < totalin))
    {
      if (buf[inp] == '%') {op = inp; inp++;}
      else
	{
	  if (buf[inp] == '\n')
	    {
	      if (srate == 0)
		{
		  for (i = op + 1, j = 0; (i < inp) && (j < 13); i++, j++) str[j] = buf[i];
		  str[13] = '\0';
		  if (strcmp(str, "sampling rate") == 0) 
		    {
		      for (i = op + 15, j = 0; j < 6; i++, j++) str[j] = buf[i];
		      str[6] = '\0';
		      sscanf(str, "%6f", &fsrate);
		      srate = (int)(fsrate * 1000);
		    }
		  else
		    {
		      if (strcmp(str, "Sampling Rate") == 0)
			{
			  for (i = op + 15, j = 0; j < 6; i++, j++) str[j] = buf[i];
			  str[6] = '\0';
			  sscanf(str, "%6d", &srate);
			}
		    }
		}
	      inp++;
	      if (buf[inp] != '%') commenting = false;
	      else
		{
		  hdr[outp] = '\n';
		  outp++;
		}
	    }
	  else
	    {
	      hdr[outp] = buf[inp];
	      outp++;
	      inp++;
	    }
	}
    }
  if (srate == 0) 
    {
      CLEANUP(oldname, newname);
      return(MUS_ERROR);
    }
  i = (outp % 4);
  outp += i;
  mus_bint_to_char((uint8_t *)(hdr + 4), outp);
  mus_bint_to_char((uint8_t *)(hdr + 16), srate);
  if (snd_checked_write(fs, (uint8_t *)hdr, outp, newname) == MUS_ERROR) 
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  happy = true;
  s0 = 0;
  outp = 0;
  osp = 0;
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = false;
	  else 
	    {
	      totalin = read(fd, buf, TRANS_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs, (uint8_t *)hdr, TRANS_BUF_SIZE, newname) == MUS_ERROR) 
	    {
	      CLEANUP(oldname, newname);
	      return_mus_write_error(oldname, newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (buf[inp] == '\n')
	    {
	      str[s0] = '\0';
	      sscanf(str, "%12d", &j);
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)j);
	      osp += 2;
	      outp += 2;
	      inp++;
	      s0 = 0;
	    }
	  else
	    {
	      str[s0] = buf[inp];
	      s0++;
	      inp++;
	    }
	}
    }
  err = snd_checked_write(fs, (uint8_t *)hdr, outp, newname);
  /* update size field? */
  CLEANUP(oldname, newname);
  if (err == MUS_ERROR) return_mus_write_error(oldname, newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- Mus10 -------------------------------- */

#define PDP_BUF_SIZE (9*1024)

static int read_mus10(const char *oldname, const char *newname, char *hdr)
{
  /* from trans.lisp */
  /* nostalgic code -- 36 bit words, two 16-bit samples, right justified */
  /* or (even more archaeological) 12 bits packed 3 to a 36-bit word */
  uint8_t *buf = NULL;
  int fd = -1, fs = -1, inp, outp, val, err = MUS_NO_ERROR;
  ssize_t totalin;
  bool happy;
  int osp;
  double fsrate, fraction;
  int srateH, srateL, sign, exponent, chans, mode;
  STARTUP(oldname, newname, PDP_BUF_SIZE, uint8_t);
  totalin = read(fd, buf, PDP_BUF_SIZE);      
  /* read the PDP-10 float srate, nchans, mode, etc */
  /* old header started with 36 bits of 0xaaaaaaaaa */
  srateH = (((buf[4] & 0xF) << 14) | (buf[5] << 6) | (buf[6] >> 2));
  srateL = (((buf[6] & 0x3) << 16) | (buf[7] << 8) | (buf[8]));
  /* PDP-10 floating point format was sign in bit 0 , excess 128 exponent in 1-8, fraction in 9-35 */
  if (srateH & 0400000) sign = -1; else sign = 1;
  exponent = ((srateH & 0377000) >> 9) - 128;
  fraction = (float)(((srateH & 0777) << 18) | srateL) / snd_int_pow2(27);
  fsrate = sign * snd_int_pow2(exponent) * fraction;
  if (fsrate > 6400.0) 
    mus_bint_to_char((uint8_t *)(hdr + 16), (int)fsrate);  
  else
    {
      /* perhaps old style header? */
      if (srateH != 0) mus_bint_to_char((uint8_t *)(hdr + 16), srateH);
    }
  mode = ((buf[11] & 0x3F) << 12) | (buf[12] << 4) | (buf[13] >> 4);
  chans = ((buf[15] & 0x3) << 12) | (buf[16] << 8) | buf[17];
  if (chans == 0) chans = 1;
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  if ((mode != 4) && (mode != 0)) 
    {
      CLEANUP(oldname, newname);
      return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE,
		       "read_mus10: can't translate Mus10 file %s:\n  mode = %d\n",
		       oldname, mode));
    }
  /* 4 = SAM 16-bit packing mode, 0 = 12 bit 3 to a word */
  /* now jump to data start */
  inp = 576;
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR) 
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  happy = true;
  outp = 0;
  osp = 0;
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < PDP_BUF_SIZE) 
	    happy = false;
	  else 
	    {
	      totalin = read(fd, buf, PDP_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs, (uint8_t *)hdr, TRANS_BUF_SIZE, newname) == MUS_ERROR) 
	    {
	      CLEANUP(oldname, newname);
	      return_mus_write_error(oldname, newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (mode == 4)
	    {
	      /* packed 4 bits junk | 16 bit | 16 bit per each 36 */
	      /* so we grab four at a time here to keep the pointers aligned */
	      /* we've chosen an input buffer size that is a multiple of 9 so that this code need not constantly check bounds */
	      val = ((buf[inp] & 0xF) << 12) | (buf[inp + 1] << 4) | (buf[inp + 2] >> 4);
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)val); osp += 2;
	      val = ((buf[inp + 2] & 0xF) << 12) | (buf[inp + 3] << 4) | (buf[inp + 4] >> 4);
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)val); osp += 2;
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)((buf[inp + 5] << 8) | buf[inp + 6])); osp += 2;
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)((buf[inp + 7] << 8) | buf[inp + 8])); osp += 2;
	      outp += 8;
	      inp += 9;
	    }
	  else
	    {
	      val = (buf[inp] << 8) | (buf[inp + 1] & 0xF0);
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)val); osp += 2;
	      val = ((buf[inp + 1] & 0xF) << 12) | (buf[inp + 2] << 4);
	      mus_bshort_to_char((uint8_t *)(hdr + osp), (short)val); osp += 2;
	      outp += 4;
	      inp += 3;
	    }
	}
    }
  err = snd_checked_write(fs, (uint8_t *)hdr, outp, newname);
  CLEANUP(oldname, newname);
  if (err == MUS_ERROR) return_mus_write_error(oldname, newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- HCOM (from Sox) -------------------------------- */

static int read_hcom(const char *oldname, const char *newname, char *hdr)
{
  short **d = NULL;
  int osp, isp;
  int dc = 0, di, bits, outp;
  ssize_t totalin;
  bool happy;
  uint32_t curval = 0;
  int i, sample, size = 0, datum, count = 0, err = MUS_NO_ERROR;
  uint8_t *buf = NULL;
  int fd = -1, fs = -1;
  size_t bytes;
  STARTUP(oldname, newname, TRANS_BUF_SIZE, uint8_t);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  lseek(fd, 132, SEEK_SET);
  bytes = read(fd, buf, 18);  /* count sum type div size */
  if (bytes != 0)
    {
      count = mus_char_to_bint((uint8_t *)buf) - 1;
      dc = mus_char_to_bint((uint8_t *)(buf + 8));
      size = mus_char_to_bshort((uint8_t *)(buf + 16));
      d = (short **)calloc(size, sizeof(short *));
      bytes = read(fd, buf, size * 4 + 2); /* 2 for pad byte + first sample */
    }
  if (bytes == 0)
    {
      if (d) free(d);
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname); /* read actually, but it's a generic message */
    }
  osp = 0;
  for (i = 0; i < size; i++) 
    {
      d[i] = (short *)calloc(2, sizeof(short));
      d[i][0] = mus_char_to_bshort((uint8_t *)(buf + osp)); osp += 2;
      d[i][1] = mus_char_to_bshort((uint8_t *)(buf + osp)); osp += 2;
    }
  sample = mus_char_to_bshort((uint8_t *)(buf + osp)) & 0xff;
  di = 0;
  totalin = read(fd, buf, TRANS_BUF_SIZE);
  osp = 0;
  isp = 0;
  happy = true;
  outp = 2;
  mus_bshort_to_char((uint8_t *)(hdr + osp), (short)((sample - 128) * 0x100)); osp += 2;
  bits = 0;
  while ((happy) && (count > 0))
    {
      if (isp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = false;
	  else 
	    {
	      totalin = read(fd, buf, TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs, (uint8_t *)hdr, TRANS_BUF_SIZE, newname) == MUS_ERROR) 
	    {
	      for (i = 0; i < size; i++) free(d[i]);
	      free(d);
	      CLEANUP(oldname, newname);
	      return_mus_write_error(oldname, newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (bits == 0) 
	    {
	      curval = mus_char_to_bint((uint8_t *)(buf + isp)); 
	      isp += 4; 
	      bits = 32;
	    }
	  if (curval & 0x80000000) di = d[di][1]; else di = d[di][0];
	  curval = curval << 1;
	  bits--;
	  if (d[di][0] < 0) 
	    {
	      datum = d[di][1];
	      if (!dc) sample = 0;
	      sample = (sample + datum) & 0xff;
	      count--;
	      if (sample == 0) mus_bshort_to_char((uint8_t *)(hdr + osp), (short)(-127 * 0x100));
	      else mus_bshort_to_char((uint8_t *)(hdr + osp), (short)((sample - 128) * 0x100));
	      osp += 2;
	      outp += 2;
	      di = 0;
	    }
	}
    }
  err = snd_checked_write(fs, (uint8_t *)hdr, outp, newname);
  for (i = 0; i < size; i++) free(d[i]);
  free(d);
  CLEANUP(oldname, newname);
  if (err == MUS_ERROR) return_mus_write_error(oldname, newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- NIST shortpack -------------------------------- */

static unsigned short log2s[] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768};

static int read_nist_shortpack(const char *oldname, const char *newname, char *hdr)
{
  int fs = -1, fd = -1, err = MUS_NO_ERROR, chans, srate, outp, i = 0, k, num, bits = 0, out, els = 0;
  ssize_t totalin;
  bool happy;
  int isp, osp;
  unsigned short *ptr = NULL, *stop, *start, *kptr;
  short temp = 0;
  bool negative;
  uint8_t *buf = NULL;
  chans = mus_sound_chans(oldname);
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((uint8_t *)(hdr + 16), srate);
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  STARTUP(oldname, newname, TRANS_BUF_SIZE, uint8_t);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  lseek(fd, 1024, SEEK_SET);                       /* NIST header always 1024 bytes */
  totalin = read(fd, buf, TRANS_BUF_SIZE);
  happy = true;
  outp = 0;
  num = 0;
  osp = 0; /* hdr */
  isp = 0; /* buf */
  start = &(log2s[15]);
  stop = log2s;
  while (happy)
    {
      /* now the shortpack algorithm, taken from wavio's shortpack_io.c */
      if (num == 0)
	{
	  num = (int)buf[isp]; 
	  bits = (int)buf[isp + 1];
	  isp += 2; 
	  if (isp >= totalin) 
	    {
	      totalin = read(fd, buf, TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	  temp = mus_char_to_bshort((uint8_t *)(buf + isp)); 
	  isp += 2;
	  if (isp >= totalin) 
	    {
	      totalin = read(fd, buf, TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	  ptr = start;
	  i = 0;
	  els = (int)((num * (bits + 1)) / 16.0);
	  if ((num * (bits + 1)) % 16 != 0) els++;
	  els--;
	}
      else
	{
	  /* get next sample */
	  out = 0;
	  negative = ((temp & *(ptr--)) != 0);
	  if (ptr < stop)
	    {
	      ptr = start;
	      if (els > 0)
		{
		  temp = mus_char_to_bshort((uint8_t *)(buf + isp)); 
		  isp += 2;
		  if (isp >= totalin) 
		    {
		      totalin = read(fd, buf, TRANS_BUF_SIZE); 
		      isp = 0;
		    }
		  els--;
		}
	    }
	  kptr = &(log2s[bits - 1]);
	  for (k = bits + 1; (--k) > 0; )
	    {
	      if ((temp & *(ptr--)) != 0) out |= *kptr;
	      kptr--;
	      if (ptr < stop)
		{
		  ptr = start;
		  if (els > 0)
		    {
		      temp = mus_char_to_bshort((uint8_t *)(buf + isp)); 
		      isp += 2;
		      if (isp >= totalin) 
			{
			  totalin = read(fd, buf, TRANS_BUF_SIZE); 
			  isp = 0;
			}
		      els--;
		    }
		}
	    }
	  if (negative)
	    {
	      if (out != 0) 
		mus_bshort_to_char((uint8_t *)(hdr + osp), (short)(-out));
	      else mus_bshort_to_char((uint8_t *)(hdr + osp), (short)32767);
	    }
	  else mus_bshort_to_char((uint8_t *)(hdr + osp), (short)out);
	  osp += 2; 
	  outp += 2;
	  i++;
	  if (i == num) num = 0;
	}
      if (isp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = false;
	  else 
	    {
	      totalin = read(fd, buf, TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs, (uint8_t *)hdr, TRANS_BUF_SIZE, newname) == MUS_ERROR)
	    {
	      CLEANUP(oldname, newname);
	      return_mus_write_error(oldname, newname);
	    }
	  osp = 0;
	  outp = 0;
	}
    }
  err = snd_checked_write(fs, (uint8_t *)hdr, outp, newname);
  CLEANUP(oldname, newname);
  if (err == MUS_ERROR) return_mus_write_error(oldname, newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- IBM ADPCM --------------------------------
 *
 * taken from Perry Cook's adpcmdec.c
 */

static int read_ibm_adpcm(const char *oldname, const char *newname, char *hdr)
{
  short MAX_STEP = 2048, MIN_STEP = 16;
  int i, j, k, fs = -1, fd = -1;
  uint8_t *buf = NULL;
  short *buf1;
  mus_long_t loc;
  short XHAT1 = 0, delndec, del1dec = 8, ln1dec, temp;
  float M[16] = {0.909, 0.909, 0.909, 0.909, 1.21, 1.4641, 1.771561, 2.143589, 0.909, 0.909, 0.909, 0.909, 1.21, 1.4641, 1.771561, 2.143589};
  float del_table[16] = {0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 0.0, -0.25, -0.5, -0.75, -1.0, -1.25, -1.5, -1.75};
  loc = mus_sound_data_location(oldname);
  mus_bint_to_char((uint8_t *)(hdr + 16), mus_sound_srate(oldname));
  mus_bint_to_char((uint8_t *)(hdr + 20), 1);
  STARTUP(oldname, newname, TRANS_BUF_SIZE, uint8_t);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  lseek(fd, loc, SEEK_SET);
  buf1 = (short *)calloc(TRANS_BUF_SIZE, sizeof(short));
  while (true)
    {
      ssize_t totalin;
      totalin = read(fd, buf, TRANS_BUF_SIZE / 4);
      if (totalin <= 0) break;
      for (i = 0, j = 0; i < totalin; i += 2, j += 4)
	{
	  unsigned short data_in;
	  data_in = (*((short *)(buf + i)));
	  for (k = 0; k < 4; k++)
	    {
	      ln1dec = data_in >> 12;
	      data_in = data_in << 4;
	      delndec = (short)(del1dec * M[ln1dec]);
	      temp = delndec - MIN_STEP;
	      if (temp < 0) delndec = MIN_STEP; 
	      temp = MAX_STEP - delndec;
	      if (temp < 0) delndec = MAX_STEP;
	      del1dec = delndec;
	      XHAT1 += (short)(delndec * del_table[ln1dec]);
	      if (XHAT1 > 32000 || XHAT1 < -32000) XHAT1 = (short)(XHAT1 * 0.95);
	      buf1[j + k] = XHAT1;
	    }
	}
      if (be_snd_checked_write(fs, (uint8_t *)buf1, j * 2, newname) == MUS_ERROR) 
	{
	  free(buf1);
	  CLEANUP(oldname, newname);
	  return_mus_write_error(oldname, newname);
	}
    }
  free(buf1);
  CLEANUP(oldname, newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- Intel ADPCM --------------------------------
 *
 * described in detail in the Microsoft RIFF docs.  This code assumes bits = 4.
 * in 'wave' file, these are stored as block_align sized blocks, each with a
 * header storing the current state.  These can be multi-channel, but we're handling
 * only mono until someone complains.  See also Apple Tech note 1081 by Mark Cookson.
 */

static int indexTable[16] = {-1, -1, -1, -1, 2, 4, 6, 8, -1, -1, -1, -1, 2, 4, 6, 8};

static int stepsizeTable[89] = {7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
				50, 55, 60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173, 190, 209, 230, 253, 279, 307,
				337, 371, 408, 449, 494, 544, 598, 658, 724, 796, 876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066,
				2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358,
				5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
				15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767};

static int adpcm_decoder(uint8_t *indata, short *outdata, int totalbytes, int type)
{
  uint32_t delta, inputbuffer = 0;
  int valpred, index, i, j;
  bool happy;
  bool bufferstep = false;
  happy = true;
  if (type == 0)
    {
      j = 4;
      valpred = mus_char_to_lshort(indata);
      index = indata[2];
    }
  else
    {
      j = 2;
      index = indata[1] & 0x7f;
      valpred = (indata[0] * 0x100) + (indata[1] & 0xff80);
    }
  i = 1;
  outdata[0] = valpred;
  while (happy)
    {
      int step, vpdiff;
      if (bufferstep) 
	{
	  delta = inputbuffer & 0xf;
	  if (j == totalbytes) happy = false;
	} 
      else 
	{
	  inputbuffer = indata[j++];
	  delta = (inputbuffer >> 4) & 0xf;
	}
      bufferstep = !bufferstep;
      step = stepsizeTable[index];
      vpdiff = (step >> 3);
      if (delta & 1) vpdiff += (step >> 2);
      if (delta & 2) vpdiff += (step >> 1);
      if (delta & 4) vpdiff += step;
      if (delta & 8) valpred -= vpdiff; else valpred += vpdiff;
      if (valpred > 32767)  valpred = 32767; else if (valpred < -32768)  valpred = -32768;
      outdata[i++] = valpred;
      index += indexTable[delta];
      if (index < 0) index = 0; else if (index > 88) index = 88;
    }
  return(i);
}


static int read_dvi_adpcm(const char *oldname, const char *newname, char *hdr, int type)
{
  int fs = -1, fd = -1, chans, srate, blksiz, samps;
  uint8_t *buf = NULL;
  mus_long_t loc;
  loc = mus_sound_data_location(oldname);
  chans = mus_sound_chans(oldname);
  blksiz = mus_sound_block_align(oldname);
  samps = mus_sound_fact_samples(oldname);
  if ((chans != 1) || (mus_sound_bits_per_sample(oldname) != 4))
    return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE,
		     "read_dvi_adpcm: can't translate DVI ADPCM file %s: chans: %d and bits: %d\n",
		     oldname, chans, mus_sound_bits_per_sample(oldname)));
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((uint8_t *)(hdr + 16), srate);
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  STARTUP(oldname, newname, blksiz, uint8_t);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  lseek(fd, loc, SEEK_SET);
  while (samps > 0)
    {
      int samps_read;
      ssize_t totalin;
      totalin = read(fd, buf, blksiz);
      if (totalin < blksiz) break;
      samps_read = adpcm_decoder(buf, (short *)hdr, totalin, type);
      if (be_snd_checked_write(fs, (uint8_t *)hdr, samps_read * 2, newname) == MUS_ERROR) 
	{
	  CLEANUP(oldname, newname);
	  return_mus_write_error(oldname, newname);
	}
      samps -= samps_read;
    }
  CLEANUP(oldname, newname);
  return(MUS_NO_ERROR);
}



/* --------------------------------Oki (Dialogic) ADPCM --------------------------------
 *
 * from vox.tar.gz:
 *   "PC Telephony - The complete guide to designing, building and programming systems
 *    using Dialogic and Related Hardware" by Bob Edgar. pg 272-276.
 */

struct oki_adpcm_status {short last; short step_index;};

static short oki_step_size[49] = { 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41,
     45, 50, 55, 60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173,
     190, 209, 230, 253, 279, 307, 337, 371, 408, 449, 494, 544, 598, 658,
     724, 796, 876, 963, 1060, 1166, 1282, 1408, 1552 };

static short oki_adjust[8] = {-1, -1, -1, -1, 2, 4, 6, 8};


static short oki_adpcm_decode(char code, struct oki_adpcm_status *stat) 
{
  short diff, E8, SS8, samp; /* SS apparently a predefined macro in Solaris 10.4/opteron */
  SS8 = oki_step_size[stat->step_index];
  E8 = SS8/8;
  if (code & 0x01) E8 += SS8 / 4;
  if (code & 0x02) E8 += SS8 / 2;
  if (code & 0x04) E8 += SS8;
  diff = (code & 0x08) ? -E8 : E8;
  samp = stat->last + diff;
  if (samp > 2048) samp = 2048;
  if (samp < -2048) samp = -2048;
  stat->last = samp;
  stat->step_index += oki_adjust[code & 0x07];
  if (stat->step_index < 0) stat->step_index = 0;
  if (stat->step_index > 48) stat->step_index = 48;
  if (samp == 2048)
    return(32767);
  return(samp << 4);
}


static int read_oki_adpcm(const char *oldname, const char *newname, char *hdr)
{
  int fs = -1, fd = -1, i, j, chans, srate, blksiz, samps;
  uint8_t *buf = NULL;
  mus_long_t loc;
  short *buf1;
  struct oki_adpcm_status stat;
  chans = mus_sound_chans(oldname);
  if (chans != 1)
    return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE,
		     "read_oki_adpcm: can't translate Oki ADPCM file %s: chans: %d\n",
		     oldname, chans));
  loc = mus_sound_data_location(oldname);
  blksiz = mus_sound_block_align(oldname);
  if (blksiz == 0) blksiz = 256;
  STARTUP(oldname, newname, blksiz, uint8_t);
  buf1 = (short *)calloc(blksiz * 2, sizeof(short));
  samps = mus_sound_fact_samples(oldname);
  if (samps == 0) samps = (int)mus_sound_framples(oldname);
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((uint8_t *)(hdr + 16), srate);
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      free(buf1);
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  lseek(fd, loc, SEEK_SET);
  stat.last = 0;
  stat.step_index = 0;
  while (samps > 0)
    {
      int samps_read;
      ssize_t totalin;
      totalin = read(fd, buf, blksiz);
      if (totalin <= 0) break;
      for (i = 0, j = 0; i < totalin; i++)
	{
	  /* samps_read will be twice totalin because these are 4-bit quantities */
	  buf1[j++] = oki_adpcm_decode((char)((buf[i] >> 4) & 0x0f), &stat);
	  buf1[j++] = oki_adpcm_decode((char)(buf[i] & 0x0f), &stat);
	}
      samps_read = totalin * 2;
      if (be_snd_checked_write(fs, (uint8_t *)buf1, samps_read * 2, newname) == MUS_ERROR) 
	{
	  free(buf1);
	  CLEANUP(oldname, newname);
	  return_mus_write_error(oldname, newname);
	}
      samps -= samps_read;
    }
  free(buf1);
  CLEANUP(oldname, newname);
  return(MUS_NO_ERROR);
}



/* -------------------------------- 12 bit cases --------------------------------
 */

static int read_12bit(const char *oldname, const char *newname, char *hdr)
{
  int chans, i, j, fs = -1, fd = -1;
  uint8_t *buf = NULL;
  short *buf1;
  mus_long_t loc, samps;
  loc = mus_sound_data_location(oldname);
  chans = mus_sound_chans(oldname);
  samps = mus_sound_samples(oldname);
  mus_bint_to_char((uint8_t *)(hdr + 16), mus_sound_srate(oldname));
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  STARTUP(oldname, newname, ((int)(TRANS_BUF_SIZE * 1.5)), uint8_t);
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR)
    {
      CLEANUP(oldname, newname);
      return_mus_write_error(oldname, newname);
    }
  lseek(fd, loc, SEEK_SET);
  buf1 = (short *)calloc(TRANS_BUF_SIZE, sizeof(short));
  while (samps > 0)
    {
      ssize_t totalin;
      totalin = read(fd, buf, (int)(TRANS_BUF_SIZE * 1.5));
      if (totalin <= 0) break;
      for (i = 0, j = 0; i < totalin; i += 3, j += 2)
	{
	  buf1[j] = (signed short)((buf[i] << 8) + (buf[i + 1] & 0xf0));
	  buf1[j + 1] = (signed short)((buf[i + 2] << 8) + ((buf[i + 1] & 0xf) << 4));
	}
      if (be_snd_checked_write(fs, (uint8_t *)buf1, j * 2, newname) == MUS_ERROR) 
	{
	  free(buf1);
	  CLEANUP(oldname, newname);
	  return_mus_write_error(oldname, newname);
	}
      samps -= j;
    }
  free(buf1);
  CLEANUP(oldname, newname);
  return(MUS_NO_ERROR);
}


#if G7XX
/*  -------------------------------- G721 and G723 from Sun --------------------------------
 * code boiled down considerably here since I have no love of compression schemes.
 */

struct g72x_state {long yl; short yu; short dms; short dml; short ap; short a[2]; short b[6]; short pk[2]; short dq[6]; short sr[2]; char td;};

static int quan(short val)
{
#if 0
  if (val >= 16384) return(15);
  if (val >= 8192) return(14);
  if (val >= 4096) return(13);
  if (val >= 2048) return(12);
  if (val >= 1024) return(11);
  if (val >= 512) return(10);
  if (val >= 256) return(9);
  if (val >= 128) return(8);
  if (val >= 64) return(7);
  if (val >= 32) return(6);
  if (val >= 16) return(5);
  if (val >= 8) return(4);
  if (val >= 4) return(3);
  if (val >= 2) return(2);
  if (val >= 1) return(1);
  return(0);
#else
  if (val >= 256) 
    {
      if (val >= 1024) 
	{
	  if (val >= 4096) 
	    {
	      if (val >= 16384) return(15);
	      if (val >= 8192) return(14);
	      return(13);
	    }
	  if (val >= 2048) return(12);
	  return(11);
	}
      if (val >= 512) return(10);
      return(9);
    }
  if (val >= 8) 
    {
      if (val >= 64) 
	{
	  if (val >= 128) return(8);
	  return(7);
	}
      if (val >= 32) return(6);
      if (val >= 16) return(5);
      return(4);
    }
  if (val >= 4) return(3);
  if (val >= 2) return(2);
  if (val >= 1) return(1);
  return(0);
#endif
}


static int fmult(int an, int srn)
{
  short anmag, anexp, anmant;
  short wanexp, wanmant;
  short retval;
  anmag = (an > 0) ? an : ((-an) & 0x1FFF);
  anexp = quan(anmag) - 6;
  anmant = (anmag == 0) ? 32 : (anexp >= 0) ? anmag >> anexp : anmag << -anexp;
  wanexp = anexp + ((srn >> 6) & 0xF) - 13;
  wanmant = (anmant * (srn & 077) + 0x30) >> 4;
  retval = (wanexp >= 0) ? ((wanmant << wanexp) & 0x7FFF) : (wanmant >> -wanexp);
  return(((an ^ srn) < 0) ? -retval : retval);
}


static void g72x_init_state(struct g72x_state *state_ptr)
{
  int cnta;
  state_ptr->yl = 34816;
  state_ptr->yu = 544;
  state_ptr->dms = 0;
  state_ptr->dml = 0;
  state_ptr->ap = 0;
  for (cnta = 0; cnta < 2; cnta++)
    {
      state_ptr->a[cnta] = 0;
      state_ptr->pk[cnta] = 0;
      state_ptr->sr[cnta] = 32;
    }
  for (cnta = 0; cnta < 6; cnta++) 
    {
      state_ptr->b[cnta] = 0;
      state_ptr->dq[cnta] = 32;
    }
  state_ptr->td = 0;
}


static int predictor_zero(struct g72x_state *state_ptr)
{
  int i, sezi;
  sezi = fmult(state_ptr->b[0] >> 2, state_ptr->dq[0]);
  for (i = 1; i < 6; i++) sezi += fmult(state_ptr->b[i] >> 2, state_ptr->dq[i]);
  return(sezi);
}


static int predictor_pole(struct g72x_state *state_ptr)
{
  return(fmult(state_ptr->a[1] >> 2, state_ptr->sr[1]) + fmult(state_ptr->a[0] >> 2, state_ptr->sr[0]));
}


static int step_size(struct g72x_state *state_ptr)
{
  int y, dif, al;
  if (state_ptr->ap >= 256)  return(state_ptr->yu);
  else 
    {
      y = state_ptr->yl >> 6;
      dif = state_ptr->yu - y;
      al = state_ptr->ap >> 2;
      if (dif > 0) y += (dif * al) >> 6;
      else if (dif < 0) y += (dif * al + 0x3F) >> 6;
      return(y);
    }
}


static int reconstruct(int sign, int dqln, int y)
{
  short	dql, dex, dqt, dq;
  dql = dqln + (y >> 2);
  if (dql < 0) {return((sign) ? -0x8000 : 0);} 
  else {
    dex = (dql >> 7) & 15;
    dqt = 128 + (dql & 127);
    dq = (dqt << 7) >> (14 - dex);
    return((sign) ? (dq - 0x8000) : dq);
  }
}


static void update(int code_size, int y, int wi, int fi, int dq, int sr, int dqsez, struct g72x_state *state_ptr)
{
  int cnt;
  short	mag, exp, a2p = 0, a1ul, pks1, fa1, ylint, thr2, dqthr, ylfrac, thr1, pk0;
  char tr;
  pk0 = (dqsez < 0) ? 1 : 0;
  mag = dq & 0x7FFF;
  ylint = state_ptr->yl >> 15;
  ylfrac = (state_ptr->yl >> 10) & 0x1F;
  thr1 = (32 + ylfrac) << ylint;
  thr2 = (ylint > 9) ? 31 << 10 : thr1;
  dqthr = (thr2 + (thr2 >> 1)) >> 1; 
  if (state_ptr->td == 0) tr = 0; else if (mag <= dqthr) tr = 0; else tr = 1;
  state_ptr->yu = y + ((wi - y) >> 5);
  if (state_ptr->yu < 544) state_ptr->yu = 544; else if (state_ptr->yu > 5120) state_ptr->yu = 5120;
  state_ptr->yl += state_ptr->yu + ((-state_ptr->yl) >> 6);
  if (tr == 1) {
    int i;
    state_ptr->a[0] = 0;
    state_ptr->a[1] = 0;
    for (i = 0; i < 6; i++) state_ptr->b[i] = 0;
  } else { 
    pks1 = pk0 ^ state_ptr->pk[0];		/* UPA2 */
    a2p = state_ptr->a[1] - (state_ptr->a[1] >> 7);
    if (dqsez != 0) {
      fa1 = (pks1) ? state_ptr->a[0] : -state_ptr->a[0];
      if (fa1 < -8191) a2p -= 0x100; else if (fa1 > 8191) a2p += 0xFF; else a2p += fa1 >> 5;
      if (pk0 ^ state_ptr->pk[1])
	{if (a2p <= -12160) a2p = -12288; else {if (a2p >= 12416) a2p = 12288; else a2p -= 0x80;}}
      else {if (a2p <= -12416) a2p = -12288; else {if (a2p >= 12160) a2p = 12288; else a2p += 0x80;}}
    }
    state_ptr->a[1] = a2p;
    state_ptr->a[0] -= state_ptr->a[0] >> 8;
    if (dqsez != 0) {if (pks1 == 0) state_ptr->a[0] += 192; else state_ptr->a[0] -= 192;}
    a1ul = 15360 - a2p;
    if (state_ptr->a[0] < -a1ul) state_ptr->a[0] = -a1ul; else if (state_ptr->a[0] > a1ul) state_ptr->a[0] = a1ul;
    for (cnt = 0; cnt < 6; cnt++) 
      {
	if (code_size == 5) state_ptr->b[cnt] -= state_ptr->b[cnt] >> 9;
	else state_ptr->b[cnt] -= state_ptr->b[cnt] >> 8;
	if (dq & 0x7FFF) 
	  {
	    if ((dq ^ state_ptr->dq[cnt]) >= 0) state_ptr->b[cnt] += 128;
	    else state_ptr->b[cnt] -= 128;
	  }
      }
  }
  for (cnt = 5; cnt > 0; cnt--) state_ptr->dq[cnt] = state_ptr->dq[cnt-1];
  if (mag == 0) 
    {
      state_ptr->dq[0] = (dq >= 0) ? 0x20 : 0xFC20;
    } 
  else 
    {
      exp = quan(mag);
      state_ptr->dq[0] = (dq >= 0) ?
	(exp << 6) + ((mag << 6) >> exp) :
	(exp << 6) + ((mag << 6) >> exp) - 0x400;
    }
  state_ptr->sr[1] = state_ptr->sr[0];
  if (sr == 0) 
    {
      state_ptr->sr[0] = 0x20;
    } 
  else 
    if (sr > 0) 
      {
	exp = quan(sr);
	state_ptr->sr[0] = (exp << 6) + ((sr << 6) >> exp);
      } 
    else 
      if (sr > -32768) 
	{
	  mag = -sr;
	  exp = quan(mag);
	  state_ptr->sr[0] =  (exp << 6) + ((mag << 6) >> exp) - 0x400;
	} 
      else
	state_ptr->sr[0] = 0xFC20; /* it's declared short?? */
  state_ptr->pk[1] = state_ptr->pk[0];
  state_ptr->pk[0] = pk0;
  if (tr == 1) state_ptr->td = 0; else if (a2p < -11776) state_ptr->td = 1; else state_ptr->td = 0;
  state_ptr->dms += (fi - state_ptr->dms) >> 5;
  state_ptr->dml += (((fi << 2) - state_ptr->dml) >> 7);
  if (tr == 1)
    state_ptr->ap = 256;
  else if (y < 1536)
    state_ptr->ap += (0x200 - state_ptr->ap) >> 4;
  else if (state_ptr->td == 1)
    state_ptr->ap += (0x200 - state_ptr->ap) >> 4;
  else if (abs((state_ptr->dms << 2) - state_ptr->dml) >= (state_ptr->dml >> 3))
    state_ptr->ap += (0x200 - state_ptr->ap) >> 4;
  else
    state_ptr->ap += (-state_ptr->ap) >> 4;
}


static int g721_decoder(int i, struct g72x_state *state_ptr)
{
  static short dqlntab[16] = {-2048, 4, 135, 213, 273, 323, 373, 425, 425, 373, 323, 273, 213, 135, 4, -2048};
  static short witab[16] = {-12, 18, 41, 64, 112, 198, 355, 1122, 1122, 355, 198, 112, 64, 41, 18, -12};
  static short fitab[16] = {0, 0, 0, 0x200, 0x200, 0x200, 0x600, 0xE00, 0xE00, 0x600, 0x200, 0x200, 0x200, 0, 0, 0};
  short	sezi, sei, sez, se, y, sr, dq, dqsez;
  i &= 0x0f;
  sezi = predictor_zero(state_ptr);
  sez = sezi >> 1;
  sei = sezi + predictor_pole(state_ptr);
  se = sei >> 1;
  y = step_size(state_ptr);
  dq = reconstruct(i & 0x08, dqlntab[i], y);
  sr = (dq < 0) ? (se - (dq & 0x3FFF)) : se + dq;
  dqsez = sr - se + sez;
  update(4, y, witab[i] << 5, fitab[i], dq, sr, dqsez, state_ptr);
  return(sr << 2);
}


static int g723_24_decoder(int	i, struct g72x_state *state_ptr)
{
  static short dqlntab[8] = {-2048, 135, 273, 373, 373, 273, 135, -2048};
  static short witab[8] = {-128, 960, 4384, 18624, 18624, 4384, 960, -128};
  static short fitab[8] = {0, 0x200, 0x400, 0xE00, 0xE00, 0x400, 0x200, 0};
  short	sezi, sei, sez, se, y, sr, dq, dqsez;
  i &= 0x07;
  sezi = predictor_zero(state_ptr);
  sez = sezi >> 1;
  sei = sezi + predictor_pole(state_ptr);
  se = sei >> 1;
  y = step_size(state_ptr);
  dq = reconstruct(i & 0x04, dqlntab[i], y); 
  sr = (dq < 0) ? (se - (dq & 0x3FFF)) : (se + dq);
  dqsez = sr - se + sez;
  update(3, y, witab[i], fitab[i], dq, sr, dqsez, state_ptr);
  return(sr << 2);
}


static int g723_40_decoder(int i, struct g72x_state *state_ptr)
{
  static short dqlntab[32] = {-2048, -66, 28, 104, 169, 224, 274, 318, 358, 395, 429, 459, 488, 514, 539, 566,
				566, 539, 514, 488, 459, 429, 395, 358, 318, 274, 224, 169, 104, 28, -66, -2048};
  static short witab[32] = {448, 448, 768, 1248, 1280, 1312, 1856, 3200, 4512, 5728, 7008, 8960, 11456, 14080, 16928, 22272,
			      22272, 16928, 14080, 11456, 8960, 7008, 5728, 4512, 3200, 1856, 1312, 1280, 1248, 768, 448, 448};
  static short fitab[32] = {0, 0, 0, 0, 0, 0x200, 0x200, 0x200, 0x200, 0x200, 0x400, 0x600, 0x800, 0xA00, 0xC00, 0xC00,
			      0xC00, 0xC00, 0xA00, 0x800, 0x600, 0x400, 0x200, 0x200, 0x200, 0x200, 0x200, 0, 0, 0, 0, 0};
  short	sezi, sei, se, sez, y, sr, dq, dqsez;
  i &= 0x1f;
  sezi = predictor_zero(state_ptr);
  sez = sezi >> 1;
  sei = sezi + predictor_pole(state_ptr);
  se = sei >> 1;
  y = step_size(state_ptr);
  dq = reconstruct(i & 0x10, dqlntab[i], y);
  sr = (dq < 0) ? (se - (dq & 0x7FFF)) : (se + dq);
  dqsez = sr - se + sez;
  update(5, y, witab[i], fitab[i], dq, sr, dqsez, state_ptr);
  return(sr << 2);
}


static bool unpack_input(FILE *fin, uint8_t *code, int bits)
{
  static uint32_t in_buffer = 0;
  static int in_bits = 0;
  uint8_t	in_byte;
  if (in_bits < bits) 
    {
      if (fread(&in_byte, sizeof(char), 1, fin) != 1) 
	{
	  *code = 0;
	  return(false);
	}
      in_buffer |= (in_byte << in_bits);
      in_bits += 8;
    }
  *code = in_buffer & ((1 << bits) - 1);
  in_buffer >>= bits;
  in_bits -= bits;
  return(true);
}


static int read_g72x_adpcm(const char *oldname, const char *newname, char *hdr, int which_g)
{
  int fs = -1, j, chans, srate, dec_bits = 0, err = MUS_NO_ERROR;
  FILE *fd;
  mus_long_t loc;
  uint8_t code;
  short *buf = NULL;
  size_t bytes;
  struct g72x_state state;
  g72x_init_state(&state);
  chans = mus_sound_chans(oldname);
  if (chans != 1)
    return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE,
		     "read_g72x_adpcm: can't translate G72x file %s: chans: %d\n",
		     oldname, chans));
  fs = CREAT(newname, 0666);
  if (fs == -1) return_mus_io_error("create", newname);
  loc = mus_sound_data_location(oldname);
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((uint8_t *)(hdr + 16), srate);
  mus_bint_to_char((uint8_t *)(hdr + 20), chans);
  fd = FOPEN(oldname, "rb");
  if (!fd) 
    {
      snd_close(fs, newname); 
      return_mus_io_error("fopen", oldname);
    }
  if (snd_checked_write(fs, (uint8_t *)hdr, 28, newname) == MUS_ERROR) 
    {
      snd_close(fs, newname); 
      snd_fclose(fd, oldname);
      return_mus_write_error(oldname, newname);
    }
  buf = (short *)calloc(TRANS_BUF_SIZE, sizeof(short));
  if (!buf) 
    {
      snd_close(fs, newname); 
      snd_fclose(fd, oldname); 
      return_mus_alloc_error(oldname, TRANS_BUF_SIZE, "buf");
    }
  bytes = fread(buf, 1, loc, fd);
  if (bytes == 0)
    {
      snd_close(fs, newname); 
      snd_fclose(fd, oldname); 
      free(buf); 
      return_mus_write_error(oldname, newname);
    }
  switch (which_g)
    {
    case 0: /* G721 */ dec_bits = 4; break;
    case 1: /* G723_24 */ dec_bits = 3; break;
    case 2: /* G723_40 */ dec_bits = 5; break;
    }
  j = 0;
  while (unpack_input(fd, &code, dec_bits))
    {
      switch (which_g)
	{
	case 0: buf[j++] = g721_decoder(code, &state); break;
	case 1: buf[j++] = g723_24_decoder(code, &state); break;
	case 2: buf[j++] = g723_40_decoder(code, &state); break;
	}
      if (j >= TRANS_BUF_SIZE)
	{
	  if (be_snd_checked_write(fs, (uint8_t *)buf, j * 2, newname) == MUS_ERROR) 
	    {
	      snd_close(fs, newname); 
	      snd_fclose(fd, oldname); 
	      free(buf); 
	      return_mus_write_error(oldname, newname);
	    }
	  j = 0;
	}
    }
  if (j > 0) err = be_snd_checked_write(fs, (uint8_t *)buf, j * 2, newname);
  snd_close(fs, newname);
  snd_fclose(fd, oldname);
  free(buf);
  if (err == MUS_ERROR) return_mus_write_error(oldname, newname);
  return(MUS_NO_ERROR);
}
#endif



/* -------------------------------- TRANSLATE -------------------------------- */

#define RIFF_Intel_ADPCM 0x11
#define RIFF_Oki_ADPCM 0x10
#define RIFF_MS_ADPCM 2
#define RIFF_IBM_ADPCM 0x103

#if G7XX
#define RIFF_G723 0x14
#define RIFF_MS_G723 0x42
#define RIFF_Lucent_G723 0x59
#define RIFF_Vivo_G723 0x111
#define RIFF_G721 0x40

#define NeXT_G721 23
#define NeXT_G723 25
#define NeXT_G723_5 26
#endif

static const char *any_samp_type_name(const char *name)
{
  mus_sample_t samp_type;
  samp_type = mus_sound_sample_type(name);
  if (samp_type != MUS_UNKNOWN_SAMPLE)
    return(mus_sample_type_name(samp_type));
  else return(mus_header_original_sample_type_name(mus_sound_original_sample_type(name), mus_sound_header_type(name)));
}


int snd_translate(const char *oldname, const char *newname, mus_header_t type)
{
  /* read oldname, translate to newname as 16-bit linear NeXT file */
  /* called from snd-file.c */
  int err;
  char *hdr;

  err = MUS_CANT_TRANSLATE;
  hdr = (char *)calloc(TRANS_BUF_SIZE, sizeof(char));
  /* set up default output header */
  mus_bint_to_char((uint8_t *)hdr, 0x2e736e64);   /* .snd */
  mus_bint_to_char((uint8_t *)(hdr + 4), 28);     /* data location */
  mus_bint_to_char((uint8_t *)(hdr + 8), 0);      /* bytes in data portion */
  mus_bint_to_char((uint8_t *)(hdr + 12), 3);     /* 16-bit linear */
  mus_bint_to_char((uint8_t *)(hdr + 16), 22050);
  mus_bint_to_char((uint8_t *)(hdr + 20), 1);     /* chans */

  switch (type)
    {
    case MUS_MIDI_SAMPLE_DUMP: 
      err = read_midi_sample_dump(oldname, newname, hdr); 
      break;

    case MUS_IEEE: 
      err = read_ieee_text(oldname, newname, hdr); 
      break;

    case MUS_MUS10: 
      err = read_mus10(oldname, newname, hdr); 
      break;

    case MUS_HCOM: 
      err = read_hcom(oldname, newname, hdr); 
      break;

    case MUS_YAMAHA_TX16W: 
      err = read_12bit(oldname, newname, hdr); 
      break;

#if G7XX
    case MUS_NVF: 
      err = read_g72x_adpcm(oldname, newname, hdr, 0); 
      break;
#endif

    case MUS_RF64:
    case MUS_RIFF:
      switch (mus_sound_original_sample_type(oldname))
	{
	case RIFF_MS_ADPCM: case RIFF_Intel_ADPCM: 
	  err = read_dvi_adpcm(oldname, newname, hdr, 0); 
	  break;

	case RIFF_IBM_ADPCM: 
	  err = read_ibm_adpcm(oldname, newname, hdr); 
	  break;

	case RIFF_Oki_ADPCM: 
	  err = read_oki_adpcm(oldname, newname, hdr); 
	  break;

#if G7XX
	case RIFF_G721: 
	  err = read_g72x_adpcm(oldname, newname, hdr, 0); 
	  break; /* untested */

	case RIFF_G723: case RIFF_MS_G723: case RIFF_Lucent_G723: case RIFF_Vivo_G723: /* untested */
	  if (mus_sound_bits_per_sample(oldname) == 3)
	    err = read_g72x_adpcm(oldname, newname, hdr, 1);
	  else
	    if (mus_sound_bits_per_sample(oldname) == 5)
	      err = read_g72x_adpcm(oldname, newname, hdr, 2);
	  break;
#endif
	}
      break;

    case MUS_NIST:
      if (mus_sound_original_sample_type(oldname) == MUS_NIST_SHORTPACK) 
	err = read_nist_shortpack(oldname, newname, hdr); 
      break;

#if G7XX
    case MUS_NEXT:
      switch (mus_sound_original_sample_type(oldname))
	{
	case NeXT_G721:   err = read_g72x_adpcm(oldname, newname, hdr, 0); break;
	case NeXT_G723:   err = read_g72x_adpcm(oldname, newname, hdr, 1); break;
	case NeXT_G723_5: err = read_g72x_adpcm(oldname, newname, hdr, 2); break;
	}
      break;
#endif

    case MUS_AIFC:
      if (mus_sound_original_sample_type(oldname) == MUS_AIFF_IMA_ADPCM) 
	err = read_dvi_adpcm(oldname, newname, hdr, 1); 
      break;
 
    case MUS_OGG: case MUS_SPEEX: case MUS_FLAC: case MUS_MIDI: case MUS_MPEG:
    case MUS_TTA: case MUS_WAVPACK:
    default:
      if (snd_decode(type, oldname, newname) == 0)
	err = MUS_NO_ERROR;
      break;
    }

  free(hdr);
  if (err == MUS_CANT_TRANSLATE)
    return(mus_error(MUS_CANT_TRANSLATE,
		     "can't translate %s\n  (%s header: %s (0x%x) sample type)\n",
		     oldname,
		     mus_header_type_name(type),
		     any_samp_type_name(oldname),
		     mus_sound_original_sample_type(oldname)));
  return(err);
}
