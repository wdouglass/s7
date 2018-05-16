/* readers/writers for various sound file headers
 *
 * Currently supported read/write (in standard sample types):                         
 *      NeXT/Sun/DEC/AFsp
 *      AIFF/AIFC
 *      RIFF (microsoft wave)
 *      RF64 (EBU)
 *      IRCAM (old style)
 *      NIST-sphere
 *      CAFF
 *      no header
 *
 * Currently supported read-only (in selected sample types):
 *      8SVX (IFF), EBICSF, INRS, ESPS, SPPACK, ADC (OGI), AVR, VOC, CSL, snack "SMP", PVF,
 *      Sound Tools, Turtle Beach SMP, SoundFont 2.0, Sound Designer I, PSION alaw, MAUD, 
 *      Gravis Ultrasound, Comdisco SPW, Goldwave sample, OMF, quicktime, sox,
 *      Sonic Foundry (w64), SBStudio II, Delusion digital, Digiplayer ST3, Farandole Composer WaveSample,
 *      Ultratracker WaveSample, Sample Dump exchange, Yamaha SY85 and SY99 (buggy), Yamaha TX16W, 
 *      Covox v8, AVI, Kurzweil 2000, Paris Ensoniq, Impulse tracker, Korg, Akai type 4, Maui,
 *
 * for a few of these I'm still trying to get documentation -- best sources of info are:
 *     ftp.cwi.nl:pub/audio (info files) 
 *     the AFsp sources http://www.TSP.ECE.McGill.CA/MMSP/Documents/AudioFormats/index.html
 *     the SOX sources
 *     svr-ftp.eng.cam.ac.uk:/comp.speech/tools
 *     http://www.wotsit.org
 *     CAFF: http://developer.apple.com/documentation/MusicAudio/Reference/CAFSpec/
 *           and afconvert can be found in /Developer/Examples/CoreAudio/Services/AudioFileTools/
 *     RIFF: Microsoft Multimedia Programmer's Reference Manual at ftp.microsoft.com:/SoftLib/MSLFILES/MDRK.EXE
 *     AVI: http://www.rahul.net/jfm/avi.html
 *     EBU RF64: http://www.ebu.ch/CMSimages/en/tec_doc_t3306_tcm6-42570.pdf
 *     Sound Designer: "Developer Documentation" from Digidesign
 *
 * test cases (sample files): ccrma-ftp.stanford.edu:/pub/Lisp/sf.tar.gz
 */

#include "mus-config.h"

#if USE_SND
  #include "snd.h"
#endif

#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#ifndef _MSC_VER
  #include <unistd.h>
#else
  #include <io.h>
  #pragma warning(disable: 4244)
#endif
#include <stdint.h>
#include <inttypes.h>

#include "_sndlib.h"
#include "sndlib-strings.h"


/* can't use LONG_MAX here because we want a 4-byte max even on 64-bit machines */
#define BIGGEST_4_BYTE_SIGNED_INT  2147483647L
#define BIGGEST_4_BYTE_UNSIGNED_INT 4294967295UL


static const uint8_t I_DSND[4] = {'.','s','n','d'};  /* NeXT/Sun/Dec/SGI/AFsp first word */
static const uint8_t I_FORM[4] = {'F','O','R','M'};  /* AIFF first word */
static const uint8_t I_AIFF[4] = {'A','I','F','F'};  /* AIFF second word */
static const uint8_t I_AIFC[4] = {'A','I','F','C'};  /* ditto but might be compressed data */
static const uint8_t I_COMM[4] = {'C','O','M','M'};
static const uint8_t I_COMT[4] = {'C','O','M','T'};
static const uint8_t I_INFO[4] = {'I','N','F','O'};
static const uint8_t I_INST[4] = {'I','N','S','T'};
static const uint8_t I_MARK[4] = {'M','A','R','K'};
static const uint8_t I_SSND[4] = {'S','S','N','D'};
static const uint8_t I_NONE[4] = {'N','O','N','E'};
static const uint8_t I_ULAW[4] = {'U','L','A','W'};  /* AIFC compression types that we can handle */
static const uint8_t I_ulaw[4] = {'u','l','a','w'};  /* or maybe it's lowercase (Apple) ... */
static const uint8_t I_raw_[4] = {'r','a','w',' '};  /* AIFC offset binary OS 8.5 (others are 'MAC3' 'MAC6' 'cdx4' 'cdx2' 'str4') */
static const uint8_t I_sowt[4] = {'s','o','w','t'};  /* AIFC 16-bit little endian -- used by Mac when extracting CD tracks */
static const uint8_t I_fl32[4] = {'f','l','3','2'};  /* AIFC 32-bit float */
static const uint8_t I_fl64[4] = {'f','l','6','4'};  /* AIFC 64-bit float */
static const uint8_t I_alaw[4] = {'a','l','a','w'};  /* apple */
static const uint8_t I_APPL[4] = {'A','P','P','L'};
static const uint8_t I_MUS_[4] = {'C','L','M',' '};  /* I hereby claim this AIFF chunk name */
static const uint8_t I_RIFF[4] = {'R','I','F','F'};  /* RIFF first word */
static const uint8_t I_RIFX[4] = {'R','I','F','X'};  /* RIFX first word (big-endian RIFF file) */
static const uint8_t I_WAVE[4] = {'W','A','V','E'};
static const uint8_t I_fmt_[4] = {'f','m','t',' '};
static const uint8_t I_data[4] = {'d','a','t','a'};
static const uint8_t I_fact[4] = {'f','a','c','t'};  /* used by compressed RIFF files */
static const uint8_t I_clm_[4] = {'c','l','m',' '};
static const uint8_t I_NIST[4] = {'N','I','S','T'};  /* first word of NIST SPHERE files */
static const uint8_t I_VOC0[4] = {'C','r','e','a'};  /* Actual text is "Creative Voice File" */
static const uint8_t I_SOUN[4] = {'S','O','U','N'};  /* Sound Tools first word="SOUND" -- not unique as SMP files start with "SOUND SAMPLE" */
static const uint8_t I_ANNO[4] = {'A','N','N','O'};
static const uint8_t I_NAME[4] = {'N','A','M','E'};
static const uint8_t I_AVR_[4] = {'2','B','I','T'};  /* first word of AVR files */
static const uint8_t I_SPIB[4] = {'%','/','/','\n'}; /* first word of IEEE spib text sound files */
static const uint8_t I_S___[4] = {'%','-','-','-'};  /* first word of other IEEE spib text sound files */
static const uint8_t I_ALaw[4] = {'A','L','a','w'};  /* first word of PSION alaw files */
static const uint8_t I_MThd[4] = {'M','T','h','d'};  /* sigh -- the M word */
static const uint8_t I_DECN[4] = {'.','s','d','\0'}; /* first word of DEC files (?) */
static const uint8_t I_LIST[4] = {'L','I','S','T'};
static const uint8_t I_GF1P[4] = {'G','F','1','P'};  /* first word of Gravis Ultrsound patch files */
static const uint8_t I_DSIG[4] = {'$','S','I','G'};  /* first word of Comdisco SPW file */
static const uint8_t I_GOLD[4] = {'G','O','L','D'};  /* first word Goldwave(?) sample file */
static const uint8_t I_SRFS[4] = {'S','R','F','S'};  /* first word Sonic Resource Foundry file(?) */
static const uint8_t I_Diam[4] = {'D','i','a','m'};  /* first word DiamondWare file */
static const uint8_t I_CSRE[4] = {'C','S','R','E'};  /* adf first word -- second starts with "40" */
static const uint8_t I_SND_[4] = {'S','N','D',' '};  /* SBStudio II */
static const uint8_t I_DDSF[4] = {'D','D','S','F'};  /* Delusion Digital Sound File */
static const uint8_t I_FSMt[4] = {'F','S','M',(uint8_t)'\376'};  /* Farandole Composer WaveSample */
static const uint8_t I_UWFD[4] = {'U','W','F','D'};  /* Ultratracker Wavesample */
static const uint8_t I_LM89[4] = {'L','M','8','9'};  /* Yamaha TX-16 */
static const uint8_t I_SY80[4] = {'S','Y','8','0'};  /* Yamaha SY-99 */
static const uint8_t I_SY85[4] = {'S','Y','8','5'};  /* Yamaha SY-85 */
static const uint8_t I_SCRS[4] = {'S','C','R','S'};  /* Digiplayer ST3 */
static const uint8_t I_covox[4] = {(uint8_t)'\377','\125',(uint8_t)'\377',(uint8_t)'\252'};
static const uint8_t I_PRAM[4] = {'P','R','A','M'};  /* Kurzweil 2000 */
static const uint8_t I__PAF[4] = {' ','p','a','f'};  /* Paris Ensoniq */
static const uint8_t I_FAP_[4] = {'f','a','p',' '};  /* Paris Ensoniq */
static const uint8_t I_file[4] = {'f','i','l','e'};  /* snack "SMP" */
static const uint8_t I_PVF1[4] = {'P','V','F','1'};  /* portable voice format (mgetty) */
static const uint8_t I_PVF2[4] = {'P','V','F','2'};
static const uint8_t I_riff[4] = {'r','i','f','f'};  /* SoundForge */
static const uint8_t I_TWIN[4] = {'T','W','I','N'};  /* TwinVQ */
static const uint8_t I_IMPS[4] = {'I','M','P','S'};  /* Impulse Tracker */
static const uint8_t I_SMP1[4] = {'S','M','P','1'};  /* Korg */
static const uint8_t I_Maui[4] = {'M','a','u','i'};  /* Turtle Beach */
static const uint8_t I_SDIF[4] = {'S','D','I','F'};  /* IRCAM sdif */
#if G7XX
static const uint8_t I_NVF_[4] = {'N','V','F',' '};  /* Nomad II Creative NVF */
#endif
static const uint8_t I_ajkg[4] = {'a','j','k','g'};  /* shorten */
static const uint8_t I_RF64[4] = {'R','F','6','4'};  /* EBU RF64 */
static const uint8_t I_ds64[4] = {'d','s','6','4'};  /* EBU RF64 */
static const uint8_t I_caff[4] = {'c','a','f','f'};  /* Apple CAFF */
static const uint8_t I_desc[4] = {'d','e','s','c'};  /* Apple CAFF */
static const uint8_t I_lpcm[4] = {'l','p','c','m'};  /* Apple CAFF */
static const uint8_t I_dSoX[4] = {'.','S','o','X'};  /* Sox intermediate (little-endian?) */
static const uint8_t I_XoSd[4] = {'X','o','S','.'};  /* Sox intermediate */

#define HDRBUFSIZ 256
static uint8_t *hdrbuf;
#define INITIAL_READ_SIZE 256

/* AIFF files can have any number of ANNO chunks, so we'll grab at least 4 of them */
#define AUX_COMMENTS 4
static mus_long_t *aux_comment_start = NULL, *aux_comment_end = NULL;

#define LOOPS 2
static int *loop_modes = NULL, *loop_starts = NULL, *loop_ends = NULL;
static int markers = 0;
static int *marker_ids = NULL, *marker_positions = NULL;

static bool hdrbuf_is_inited = false;


/* for CLM */
void mus_reset_headers_c(void) 
{
  hdrbuf_is_inited = false; 
  markers = 0;
}


int mus_header_initialize(void)
{
  if (!hdrbuf_is_inited)
    {
      hdrbuf_is_inited = true;
      hdrbuf = (uint8_t *)calloc(HDRBUFSIZ, sizeof(uint8_t));
      aux_comment_start = (mus_long_t *)calloc(AUX_COMMENTS, sizeof(mus_long_t));
      aux_comment_end = (mus_long_t *)calloc(AUX_COMMENTS, sizeof(mus_long_t));
      loop_modes = (int *)calloc(LOOPS, sizeof(int));
      loop_starts = (int *)calloc(LOOPS, sizeof(int));
      loop_ends = (int *)calloc(LOOPS, sizeof(int));
      if ((!hdrbuf) || (!aux_comment_start) || (!aux_comment_end) ||
	  (!loop_modes) || (!loop_starts) || (!loop_ends))
	return(mus_error(MUS_MEMORY_ALLOCATION_FAILED, "mus_header_initialize: buffer allocation failed"));
    }
  return(MUS_NO_ERROR);
}


#define I_IRCAM_VAX  0x0001a364
#define I_IRCAM_SUN  0x0002a364
#define I_IRCAM_MIPS 0x0003a364
#define I_IRCAM_NEXT  0x0004a364

static mus_long_t data_location = 0;
static int srate = 0, chans = 0, original_sample_type = 0;
static mus_sample_t sample_type = MUS_UNKNOWN_SAMPLE;
static mus_header_t header_type = MUS_UNKNOWN_HEADER;
static int type_specifier = 0, bits_per_sample = 0, block_align = 0, fact_samples = 0;
static mus_long_t comment_start = 0, comment_end = 0;
static mus_long_t true_file_length = 0, data_size = 0;
static int base_detune = 0, base_note = 0;
static bool little_endian = false;

mus_long_t mus_header_samples(void)            {return(data_size);}
mus_long_t mus_header_data_location(void)      {return(data_location);}
int mus_header_chans(void)                     {return(chans);}
int mus_header_srate(void)                     {return(srate);}
mus_header_t mus_header_type(void)             {return(header_type);}
mus_sample_t mus_header_sample_type(void)      {return(sample_type);}
mus_long_t mus_header_comment_start(void)      {return(comment_start);}
mus_long_t mus_header_comment_end(void)        {return(comment_end);}
mus_long_t mus_header_aux_comment_start(int n) {if (aux_comment_start) return(aux_comment_start[n]); else return(-1);}
mus_long_t mus_header_aux_comment_end(int n)   {if (aux_comment_end) return(aux_comment_end[n]); else return(-1);}
int mus_header_type_specifier(void)            {return(type_specifier);}
int mus_header_bits_per_sample(void)           {return(bits_per_sample);}
int mus_header_fact_samples(void)              {return(fact_samples);}
int mus_header_block_align(void)               {return(block_align);}
mus_long_t mus_header_true_length(void)        {return(true_file_length);}
int mus_header_original_sample_type(void)      {return(original_sample_type);}
int mus_header_loop_mode(int which)            {if (loop_modes) return(loop_modes[which]); else return(-1);}
int mus_header_loop_start(int which)           {if (loop_starts) return(loop_starts[which]); else return(-1);}
int mus_header_loop_end(int which)             {if (loop_ends) return(loop_ends[which]); else return(-1);}
int mus_header_mark_position(int id)           {int i; for (i = 0; i < markers; i++) {if (marker_ids[i] == id) return(marker_positions[i]);} return(-1);}
int mus_header_base_detune(void)               {return(base_detune);}
int mus_header_base_note(void)                 {return(base_note);}

int mus_header_mark_info(int **m_ids, int **m_positions)
{
  (*m_ids) = marker_ids;
  (*m_positions) = marker_positions;
  return(markers);
}


int mus_bytes_per_sample(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_BYTE:             return(1);
    case MUS_BSHORT:           return(2);
    case MUS_UBYTE:            return(1);
    case MUS_MULAW:            return(1);
    case MUS_ALAW:             return(1);
    case MUS_BINT:             return(4);
    case MUS_BFLOAT:           return(4);
    case MUS_BFLOAT_UNSCALED:  return(4);
    case MUS_B24INT:           return(3);
    case MUS_BDOUBLE:          return(8);
    case MUS_BDOUBLE_UNSCALED: return(8);
    case MUS_LSHORT:           return(2);
    case MUS_LINT:             return(4);
    case MUS_LFLOAT:           return(4);
    case MUS_LDOUBLE:          return(8);
    case MUS_LFLOAT_UNSCALED:  return(4);
    case MUS_LDOUBLE_UNSCALED: return(8);
    case MUS_L24INT:           return(3);
    case MUS_UBSHORT:          return(2);
    case MUS_ULSHORT:          return(2);
    case MUS_BINTN:            return(4);
    case MUS_LINTN:            return(4);
    default:                   return(1); /* we divide by this number, so 0 is not safe */
    }
}


mus_long_t mus_samples_to_bytes (mus_sample_t samp_type, mus_long_t size) 
{
  return(size * (mus_bytes_per_sample(samp_type)));
}


mus_long_t mus_bytes_to_samples (mus_sample_t samp_type, mus_long_t size) 
{
  return((mus_long_t)(size / (mus_bytes_per_sample(samp_type))));
}


static bool equal_big_or_little_endian(const uint8_t *n1, const uint32_t n2)
{
  return((mus_char_to_ubint(n1) == n2) || (mus_char_to_ulint(n1) == n2));
}


static short big_or_little_endian_short(const uint8_t *n, bool little)
{
  if (little) return(mus_char_to_lshort(n));
  return(mus_char_to_bshort(n));
}


static int big_or_little_endian_int(const uint8_t *n, bool little)
{
  if (little) return(mus_char_to_lint(n));
  return(mus_char_to_bint(n));
}


static uint32_t big_or_little_endian_uint(const uint8_t *n, bool little)
{
  if (little) return(mus_char_to_ulint(n));
  return(mus_char_to_ubint(n));
}


static float big_or_little_endian_float(const uint8_t *n, bool little)
{
  if (little) return(mus_char_to_lfloat(n));
  return(mus_char_to_bfloat(n));
}


static bool match_four_chars(const uint8_t *head, const uint8_t *match)
{ 
  return((head[0] == match[0]) &&
	 (head[1] == match[1]) &&
	 (head[2] == match[2]) &&
	 (head[3] == match[3]));
}

  
static void write_four_chars(uint8_t *head, const uint8_t *match)
{
  head[0] = match[0];
  head[1] = match[1];
  head[2] = match[2];
  head[3] = match[3];
}


const char *mus_header_type_name(mus_header_t type)
{
  switch (type)
    {
    case MUS_NEXT:             return("Sun/Next");                
    case MUS_AIFC:             return("AIFC");                    
    case MUS_RIFF:             return("RIFF");                    
    case MUS_BICSF:            return("BICSF");                   
    case MUS_NIST:             return("NIST");                    
    case MUS_INRS:             return("INRS");                    
    case MUS_ESPS:             return("ESPS");                    
    case MUS_SVX:              return("SVX8");                    
    case MUS_VOC:              return("VOC");                     
    case MUS_SNDT:             return("SNDT");                    
    case MUS_SOX:              return("Sox");                     
    case MUS_RAW:              return("raw (no header)");         
    case MUS_SMP:              return("SMP");                     
    case MUS_AVR:              return("AVR");                     
    case MUS_IRCAM:            return("IRCAM");                   
    case MUS_SD1:              return("Sound Designer 1");        
    case MUS_SPPACK:           return("SPPACK");                  
    case MUS_MUS10:            return("Mus10");                   
    case MUS_HCOM:             return("HCOM");                    
    case MUS_PSION:            return("PSION");                   
    case MUS_MAUD:             return("MAUD");                    
    case MUS_IEEE:             return("IEEE text");               
    case MUS_MATLAB:           return("Matlab");                  
    case MUS_ADC:              return("ADC/OGI");                 
    case MUS_MIDI:             return("MIDI");                    
    case MUS_SOUNDFONT:        return("SoundFont");               
    case MUS_GRAVIS:           return("Gravis Ultrasound patch"); 
    case MUS_COMDISCO:         return("Comdisco SPW signal");     
    case MUS_GOLDWAVE:         return("Goldwave sample");         
    case MUS_SRFS:             return("SRFS");                    
    case MUS_MIDI_SAMPLE_DUMP: return("MIDI sample dump");        
    case MUS_DIAMONDWARE:      return("DiamondWare");             
    case MUS_ADF:              return("CSRE adf");                
    case MUS_SBSTUDIOII:       return("SBStudioII");              
    case MUS_DELUSION:         return("Delusion");                
    case MUS_FARANDOLE:        return("Farandole");               
    case MUS_SAMPLE_DUMP:      return("Sample dump");             
    case MUS_ULTRATRACKER:     return("Ultratracker");            
    case MUS_YAMAHA_TX16W:     return("TX-16W");                  
    case MUS_YAMAHA_SY85:      return("Sy-85");                   
    case MUS_YAMAHA_SY99:      return("Sy-99");                   
    case MUS_KURZWEIL_2000:    return("Kurzweil 2000");           
    case MUS_KORG:             return("Korg");                    
    case MUS_MAUI:             return("Turtle Beach");            
    case MUS_IMPULSETRACKER:   return("Impulse Tracker");         
    case MUS_AKAI4:            return("AKAI 4");                  
    case MUS_DIGIPLAYER:       return("Digiplayer ST3");          
    case MUS_COVOX:            return("Covox V8");                
    case MUS_AVI:              return("AVI");                     
    case MUS_OMF:              return("OMF");                     
    case MUS_QUICKTIME:        return("Quicktime");               
    case MUS_ASF:              return("asf");                     
    case MUS_AIFF:             return("AIFF");                    
    case MUS_PAF:              return("Ensoniq Paris");           
    case MUS_CSL:              return("CSL");                     
    case MUS_FILE_SAMP:        return("snack SMP");               
    case MUS_PVF:              return("Portable Voice Format");   
    case MUS_SOUNDFORGE:       return("SoundForge");              
    case MUS_TWINVQ:           return("TwinVQ");                  
    case MUS_SDIF:             return("IRCAM sdif");              
#if G7XX
    case MUS_NVF:              return("Creative NVF");            
#endif
    case MUS_OGG:              return("Ogg Vorbis");              
    case MUS_FLAC:             return("Flac");                    
    case MUS_SPEEX:            return("Speex");                   
    case MUS_MPEG:             return("mpeg");                    
    case MUS_SHORTEN:          return("shorten");                 
    case MUS_TTA:              return("tta");                     
    case MUS_WAVPACK:          return("wavpack");                 
    case MUS_RF64:             return("rf64");                    
    case MUS_CAFF:             return("caff");                    
    default:                   return("unknown");                 
    }
}


const char *mus_sample_type_name(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_BSHORT:           return("big endian short (16 bits)");               
    case MUS_MULAW:            return("mulaw (8 bits)");                           
    case MUS_BYTE:             return("signed byte (8 bits)");                     
    case MUS_BFLOAT:           return("big endian float (32 bits)");               
    case MUS_BFLOAT_UNSCALED:  return("big endian float (32 bits, unscaled)");     
    case MUS_BINT:             return("big endian int (32 bits)");                 
    case MUS_ALAW:             return("alaw (8 bits)");                            
    case MUS_UBYTE:            return("unsigned byte (8 bits)");                   
    case MUS_B24INT:           return("big endian int (24 bits)");                 
    case MUS_BDOUBLE:          return("big endian double (64 bits)");              
    case MUS_BDOUBLE_UNSCALED: return("big endian double (64 bits, unscaled)");    
    case MUS_LSHORT:           return("little endian short (16 bits)");            
    case MUS_LINT:             return("little endian int (32 bits)");              
    case MUS_LFLOAT:           return("little endian float (32 bits)");            
    case MUS_LDOUBLE:          return("little endian double (64 bits)");           
    case MUS_LFLOAT_UNSCALED:  return("little endian float (32 bits, unscaled)");  
    case MUS_LDOUBLE_UNSCALED: return("little endian double (64 bits, unscaled)"); 
    case MUS_UBSHORT:          return("unsigned big endian short (16 bits)");      
    case MUS_ULSHORT:          return("unsigned little endian short (16 bits)");   
    case MUS_L24INT:           return("little endian int (24 bits)");              
    case MUS_BINTN:            return("normalized big endian int (32 bits)");      
    case MUS_LINTN:            return("normalized little endian int (32 bits)");   
    default:                   return("unknown");                                  
    }
}


const char *mus_sample_type_short_name(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_BSHORT:           return("short int");       
    case MUS_MULAW:            return("mulaw");           
    case MUS_BYTE:             return("signed byte");     
    case MUS_BFLOAT:           return("float");           
    case MUS_BFLOAT_UNSCALED:  return("float unscaled)"); 
    case MUS_BINT:             return("int");             
    case MUS_ALAW:             return("alaw");            
    case MUS_UBYTE:            return("unsigned byte");   
    case MUS_B24INT:           return("24-bit int");      
    case MUS_BDOUBLE:          return("double");          
    case MUS_BDOUBLE_UNSCALED: return("double unscaled"); 
    case MUS_LSHORT:           return("short int");       
    case MUS_LINT:             return("int");             
    case MUS_LFLOAT:           return("float");           
    case MUS_LDOUBLE:          return("double");          
    case MUS_LFLOAT_UNSCALED:  return("float unscaled");  
    case MUS_LDOUBLE_UNSCALED: return("double unscaled"); 
    case MUS_UBSHORT:          return("unsigned short");  
    case MUS_ULSHORT:          return("unsigned short");  
    case MUS_L24INT:           return("24-bit int");      
    case MUS_BINTN:            return("normalized int");  
    case MUS_LINTN:            return("normalized int");  
    default:                   return("unknown");         
    }
}


#if (HAVE_EXTENSION_LANGUAGE) && (HAVE_RUBY) /* HAVE_EXTENSION_LANGUAGE is not redundant -- make sndinfo for example */
  #define TO_LANG(Str) (const char *)xen_scheme_constant_to_ruby(Str)
#else
  #define TO_LANG(Str) Str
#endif


const char *mus_header_type_to_string(mus_header_t type)
{
  switch (type)
    {
    case MUS_NEXT:      return(TO_LANG(S_mus_next));
    case MUS_AIFF:      return(TO_LANG(S_mus_aiff));
    case MUS_AIFC:      return(TO_LANG(S_mus_aifc));
    case MUS_RIFF:      return(TO_LANG(S_mus_riff));
    case MUS_NIST:      return(TO_LANG(S_mus_nist));
    case MUS_IRCAM:     return(TO_LANG(S_mus_ircam));
    case MUS_RAW:       return(TO_LANG(S_mus_raw));
    case MUS_BICSF:     return(TO_LANG(S_mus_bicsf));
    case MUS_VOC:       return(TO_LANG(S_mus_voc));
    case MUS_SVX:       return(TO_LANG(S_mus_svx));
    case MUS_SOUNDFONT: return(TO_LANG(S_mus_soundfont));
    case MUS_RF64:      return(TO_LANG(S_mus_rf64));
    case MUS_CAFF:      return(TO_LANG(S_mus_caff));
    default: break;
    }
  return(NULL);
}


const char *mus_sample_type_to_string(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_BSHORT:           return(TO_LANG(S_mus_bshort));
    case MUS_LSHORT:           return(TO_LANG(S_mus_lshort));
    case MUS_MULAW:            return(TO_LANG(S_mus_mulaw));
    case MUS_ALAW:             return(TO_LANG(S_mus_alaw));
    case MUS_BYTE:             return(TO_LANG(S_mus_byte));
    case MUS_UBYTE:            return(TO_LANG(S_mus_ubyte));
    case MUS_BFLOAT:           return(TO_LANG(S_mus_bfloat));
    case MUS_LFLOAT:           return(TO_LANG(S_mus_lfloat));
    case MUS_BINT:             return(TO_LANG(S_mus_bint));
    case MUS_LINT:             return(TO_LANG(S_mus_lint));
    case MUS_BINTN:            return(TO_LANG(S_mus_bintn));
    case MUS_LINTN:            return(TO_LANG(S_mus_lintn));
    case MUS_B24INT:           return(TO_LANG(S_mus_b24int));
    case MUS_L24INT:           return(TO_LANG(S_mus_l24int));
    case MUS_BDOUBLE:          return(TO_LANG(S_mus_bdouble));
    case MUS_LDOUBLE:          return(TO_LANG(S_mus_ldouble));
    case MUS_UBSHORT:          return(TO_LANG(S_mus_ubshort));
    case MUS_ULSHORT:          return(TO_LANG(S_mus_ulshort));
    case MUS_BDOUBLE_UNSCALED: return(TO_LANG(S_mus_bdouble_unscaled));
    case MUS_LDOUBLE_UNSCALED: return(TO_LANG(S_mus_ldouble_unscaled));
    case MUS_BFLOAT_UNSCALED:  return(TO_LANG(S_mus_bfloat_unscaled));
    case MUS_LFLOAT_UNSCALED:  return(TO_LANG(S_mus_lfloat_unscaled));
    default: break;
    }
  return(NULL);
}


static const char *any_sample_type_name(mus_sample_t sndlib_samp_type)
{
  if (mus_is_sample_type(sndlib_samp_type))
    return(mus_sample_type_name(sndlib_samp_type));
  else return(mus_header_original_sample_type_name(mus_header_original_sample_type(), mus_header_type()));
}


#define SEEK_FILE_LENGTH(File) lseek(File, 0L, SEEK_END)
static int read_bicsf_header(const char *filename, int fd);



/* ------------------------------------ NeXT (or Sun) -------------------------------- 
 * 
 *   0:  ".snd"
 *   4:  data_location (bytes) (not necessarily word aligned on Sun)
 *   8:  data_size (bytes) -- sometimes incorrect ("advisory")
 *   12: sample type indicator -- see below
 *   16: srate (int)
 *   20: chans
 *   24: comment start
 *   
 * in an AFsp file, the first 4 bytes of the comment are "AFsp",
 * for bicsf, the integer at 28 is 107364 or 107415
 *
 * on NeXTStep, always big-endian.  ".snd"==0x2e736e64 on big-endian machines.
 *
 * formats are: 
 * 0 unspecified, 1 mulaw_8, 2 linear_8, 3 linear_16, 4 linear_24, 5 linear_32, 6 float,
 * 7 double, 8 indirect, 9 nested, 10 dsp_core, 11 dsp_data_8, 12 dsp_data_16, 13 dsp_data_24,
 * 14 dsp_data_32, 16 display, 17 mulaw_squelch, 18 emphasized, 19 compressed, 20 compressed_emphasized
 * 21 dsp_commands, 22 dsp_commands_samples, 23 adpcm_g721, 24 adpcm_g722, 25 adpcm_g723,
 * 26 adpcm_g723_5, 27 alaw_8, 28 aes, 29 delat_mulaw_8 
 *   internal Snd(lib)-only formats: 
 *     30: mus_lint, 31: mus_lfloat, 
 *     32: mus_bintn, 33: mus_lintn,
 *     34: mus_ldouble and others... (added by me for Snd internal use)
 */

/* according to the file /usr/share/magic, the DECN versions were little endian */


static int read_next_header(const char *filename, int fd)
{
  int maybe_bicsf, err = MUS_NO_ERROR, i;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
  data_location = mus_char_to_ubint((uint8_t *)(hdrbuf + 4));
  if (data_location < 24) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data location: %" print_mus_long "?", filename, data_location));

  data_size = mus_char_to_ubint((uint8_t *)(hdrbuf + 8)); /* changed to unsigned 11-Nov-06 */
  /* can be bogus -- fixup if possible */
  true_file_length = SEEK_FILE_LENGTH(fd);
  if ((data_size <= 24) || (data_size > true_file_length))
    data_size = (true_file_length - data_location);
  else
    {
      if (true_file_length > (mus_long_t)0x80000000) /* (1 << 31)) */
	data_size = true_file_length - data_location; /* assume size field overflowed 32 bits */
    }

  original_sample_type = mus_char_to_bint((uint8_t *)(hdrbuf + 12));
  switch (original_sample_type) 
    {
    case 1:  sample_type = MUS_MULAW;            break;
    case 2:  sample_type = MUS_BYTE;             break; /* some sound files assume MUS_UBYTE here! (NAS from 1994 X11R6 contrib) */
    case 3:  sample_type = MUS_BSHORT;           break;
    case 4:  sample_type = MUS_B24INT;           break;
    case 5:  sample_type = MUS_BINT;             break;
    case 6:  sample_type = MUS_BFLOAT;           break;
    case 7:  sample_type = MUS_BDOUBLE;          break;
    case 18: sample_type = MUS_BSHORT;           break; /* "emphasized": Xavier Serra's de-emphasis filter: y(n) = x(n) + .9 y(n-1) */
    case 27: sample_type = MUS_ALAW;             break;
    case 30: sample_type = MUS_LINT;             break; /* from here on, for Snd's internal benefit -- these are probably not used elsewhere */
    case 31: sample_type = MUS_LFLOAT;           break; 
    case 32: sample_type = MUS_BINTN;            break; 
    case 33: sample_type = MUS_LINTN;            break; 
    case 34: sample_type = MUS_LDOUBLE;          break; 
    case 35: sample_type = MUS_ULSHORT;          break; 
    case 36: sample_type = MUS_UBSHORT;          break; 
    case 37: sample_type = MUS_LFLOAT_UNSCALED;  break;
    case 38: sample_type = MUS_BFLOAT_UNSCALED;  break;
    case 39: sample_type = MUS_LDOUBLE_UNSCALED; break;
    case 40: sample_type = MUS_BDOUBLE_UNSCALED; break;
    case 41: sample_type = MUS_LSHORT;           break; 
    case 42: sample_type = MUS_L24INT;           break; 
    case 43: sample_type = MUS_UBYTE;            break; 
    default: sample_type = MUS_UNKNOWN_SAMPLE;   break;
    }

  srate = mus_char_to_bint((uint8_t *)(hdrbuf + 16));
  chans = mus_char_to_bint((uint8_t *)(hdrbuf + 20));

  comment_start = 0;
  comment_end = 0;
  for (i = 24; i < data_location - 1; i++)
    if (hdrbuf[i] == '\0') 
      break;
    else
      {
	if (hdrbuf[i] != ' ')
	  {
	    comment_start = i;
	    comment_end = data_location - 1;
	    break;
	  }
      }
  if (comment_end < comment_start) comment_end = comment_start;

  maybe_bicsf = mus_char_to_bint((uint8_t *)(hdrbuf + 28));
  if (maybe_bicsf == 107364) err = read_bicsf_header(filename, fd);

  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(err);
}


static int sndlib_format_to_next(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_MULAW:            return(1);  
    case MUS_BYTE:             return(2);  
    case MUS_BSHORT:           return(3);  
    case MUS_B24INT:           return(4);  
    case MUS_BINT:             return(5);  
    case MUS_BFLOAT:           return(6);  
    case MUS_BDOUBLE:          return(7);  
    case MUS_ALAW:             return(27); 
    case MUS_LINT:             return(30); 
    case MUS_LFLOAT:           return(31); 
    case MUS_BINTN:            return(32); 
    case MUS_LINTN:            return(33); 
    case MUS_LDOUBLE:          return(34); 
    case MUS_ULSHORT:          return(35); 
    case MUS_UBSHORT:          return(36); 
    case MUS_LFLOAT_UNSCALED:  return(37); 
    case MUS_BFLOAT_UNSCALED:  return(38); 
    case MUS_LDOUBLE_UNSCALED: return(39); 
    case MUS_BDOUBLE_UNSCALED: return(40); 
    case MUS_LSHORT:           return(41); 
    case MUS_L24INT:           return(42); 
    case MUS_UBYTE:            return(43); 
    default: 
      return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE, "Next header: can't write sample type: %d (%s)",
		       samp_type,
		       any_sample_type_name(samp_type)));
    }
}


static int header_write(int fd, uint8_t *buf, int chars)
{
  if (chars > 0)
    {
      int64_t bytes;
      bytes = (int64_t)write(fd, buf, chars);
      if (bytes != chars)
	{
	  char *errstr;
	  errstr = STRERROR(errno);
	  return(mus_error(MUS_WRITE_ERROR, "header_write: wrote %" print_mus_long " of %d bytes, %s", 
			   bytes, chars, (errstr) ? errstr : "unknown error?"));
	}
    }
  return(MUS_NO_ERROR);
}


static int header_read(int fd, uint8_t *buf, int chars)
{
  if (chars > 0)
    {
      int64_t bytes;
      bytes = (int64_t)read(fd, buf, chars);
      if (bytes != chars) 
	{
	  char *errstr;
	  errstr = STRERROR(errno);
	  return(mus_error(MUS_READ_ERROR, "header_read: read %" print_mus_long " of %d bytes, %s", 
			   bytes, chars, (errstr) ? errstr : "unknown error?"));
	}
    }
  return(MUS_NO_ERROR);
}


static void write_next_comment(int fd, const char *comment, int len, int loc)
{
  if (len > 0)
    header_write(fd, (uint8_t *)comment, len);
  len = loc - (len + 24);
  if (len > 0)
    {
      uint8_t *combuf;
      combuf = (uint8_t *)calloc(len, sizeof(uint8_t));
      header_write(fd, combuf, len);
      free(combuf);
    }
}

static int mus_header_write_next_header(int fd, int wsrate, int wchans, int loc, int siz, mus_sample_t samp_type, const char *comment, int len)
{
  int i, j;
  write_four_chars((uint8_t *)hdrbuf, I_DSND); /* ".snd" */
  i = len / 4;
  j = 24 + (4 * (i + 1));
  if (loc < j) loc = j;
  mus_bint_to_char((uint8_t *)(hdrbuf + 4), loc);
  mus_bint_to_char((uint8_t *)(hdrbuf + 8), siz);
  mus_bint_to_char((uint8_t *)(hdrbuf + 12), sndlib_format_to_next(samp_type));
  mus_bint_to_char((uint8_t *)(hdrbuf + 16), wsrate);
  mus_bint_to_char((uint8_t *)(hdrbuf + 20), wchans);
  header_write(fd, hdrbuf, 24);
  write_next_comment(fd, comment, len, loc);
  data_location = loc;
  return(MUS_NO_ERROR);
}



/* ------------------------------------ AIFF ------------------------------------ 
 *
 *  0: "FORM"
 *  4: size (bytes)
 *  8: "AIFF" or "AIFC" -- the latter includes compressed formats (list extended for 8.5 Sound.h)
 *
 *  Thereafter the file is organized into "chunks", each chunk being 
 *  a 4-byte identifer followed by an int (4-bytes) giving the chunk size
 *  not including the 8-byte header.  AIFF data is signed.  If the chunk
 *  size is odd, an extra (unaccounted-for) null byte is added at the end.
 *
 *  The chunks we want are "COMM", "SSND", and "APPL".
 *
 * COMM: 0: chans
 *       2: framples
 *       6: bits per sample
 *       8: srate as 80-bit IEEE float
 *  then if AIFC (not AIFF), 4 bytes giving compression id ("NONE"=not compressed)
 *    followed by Pascal string giving long name of compression type
 *
 * SSND: 0: data location (offset within SSND chunk)
 *
 * Other chunks include:  ANNO: a comment, INST: loop control, MARK: marker, MIDI: midi,
 *                        COMT: comment (max 65536 chars), NAME: sound name, AUTH: author's name
 *                        (c), AESD: recording data, APPL: application specific stuff
 *    "MARK" size short-#marks {marks} -- latter are short-ID long-position pstring-name.
 *    "INST" size chars[baseNote detune lowNote highNote lowVelocity HighVelocity] short-gain loops[sustain release]
 *      loop: short-playMode marker-begin marker-end (signed?) shorts)
 *         playMode: 0 no loop, 1 forward loop, 2 forward/backward loop
 *      chars are MIDI data (detune is in cents)
 *    "MIDI" size MIDI-data...
 *    "AESD" size AES Channel Status Data (24 bytes as specified by AES)
 *      see "AES: Guidelines for the use of the AES3 interface"
 *      byte 0: bit 0: 0 = consumer, 1 = pro
 *              bit 1: 0 = audio, 1 = non-audio
 *              bits 2:4: emphasis: 0:none, 4:none, 6:CD, 7:CCITT J17
 *              bits 6:7: srate: 00 = 48KHz, 01 = 48, 10 = 44.1, 11 = 32
 *      byte 1: bits 0:3: chans: 2:mono, else stereo
 *      byte 2 for word size stuff (always ends up 16-bit): bits 3-5 = sample length where 4 = 16-bit
 *      byte 3: multi-channels modes, 4: AES sync ref, 5:unused, 6-9:ASCII source ID, 10-13:ASCII destination ID
 *      byte 14-17:local sample addr, 18-21:time of day addr, then CRC checks
 *    "APPL" size signature data
 *    "COMT" size short-#comments {comments} -- the latter are long-time marker short-text-length char-text
 *       time is in seconds since 1-Jan-1904
 *    "NAME"/"AUTH"/"(c) "/"ANNO" size char-name
 *    "FVER" size(4) AIFC-format-version -- currently always 0xA2805140
 *    "SAXL" -- a desperate kludge to get around Apple's own compression schemes!
 *
 * always big-endian
 * There was also (briefly) an AIFS file, now deprecated.
 */


/* ieee-80 conversions -- design by committee! */
/* this code taken from CSound sources -- apparently originally written by Malcolm Slaney at Apple */

#define ULPOW2TO31	((uint32_t)0x80000000)
#define DPOW2TO31	((double)2147483648.0)	/* 2^31 */

static double myUlongToDouble(uint32_t ul)
{
  double val;
  if (ul & ULPOW2TO31) val = DPOW2TO31 + (ul & (~ULPOW2TO31));
  else val = ul;
  return val;
}


static uint32_t myDoubleToUlong(double val)
{
  uint32_t ul;
  if (val < DPOW2TO31) ul = (uint32_t)val;
  else ul = ULPOW2TO31 | (uint32_t)(val-DPOW2TO31);
  return ul;
}


static double ieee_80_to_double(uint8_t *p)
{
  uint8_t sign;
  short lexp = 0;
  uint32_t mant1 = 0;
  uint32_t mant0 = 0;
  lexp = *p++;  lexp <<= 8;  lexp |= *p++;  sign = (lexp & 0x8000) ? 1 : 0;  lexp &= 0x7FFF;
  mant1 = *p++;  mant1 <<= 8;  mant1 |= *p++;  mant1 <<= 8;  mant1 |= *p++;  mant1 <<= 8;  mant1 |= *p++;
  mant0 = *p++;  mant0 <<= 8;  mant0 |= *p++;  mant0 <<= 8;  mant0 |= *p++;  mant0 <<= 8;  mant0 |= *p++;
  if (mant1 == 0 && mant0 == 0 && lexp == 0 && sign == 0)
    return 0.0;
  else
    {
      double val;
      val = myUlongToDouble(mant0) * pow(2.0, -63.0);
      val += myUlongToDouble(mant1) * pow(2.0, -31.0);
      val *= pow(2.0, ((double) lexp) - 16383.0);
      return sign ? -val : val;
    }
}


static void double_to_ieee_80(double val, uint8_t *p)
{
  short lexp = 0;
  uint8_t sign = 0;
  uint32_t mant1 = 0;
  uint32_t mant0 = 0;
  if (val < 0.0)
    {  
      sign = 1;  
      val = -val; 
    }
  if (val != 0.0)	/* val identically zero -> all elements zero */
    {
      lexp = (short)(log(val) / log(2.0) + 16383.0);
      val *= pow(2.0, 31.0 + 16383.0 - (double)lexp);
      mant1 = myDoubleToUlong(val);
      val -= myUlongToDouble(mant1);
      val *= pow(2.0, 32.0);
      mant0 = myDoubleToUlong(val);
    }
  *p++ = ((sign << 7) | (lexp >> 8));  *p++ = 0xFF & lexp;  
  *p++ = 0xFF & (mant1 >> 24);  *p++ = 0xFF & (mant1 >> 16);  *p++ = 0xFF & (mant1 >> 8);  *p++ = 0xFF & (mant1);
  *p++ = 0xFF & (mant0 >> 24);  *p++ = 0xFF & (mant0 >> 16);  *p++ = 0xFF & (mant0 >> 8);  *p++ = 0xFF & (mant0);
}


static mus_long_t update_form_size, update_framples_location, update_ssnd_location, update_rf64_location;

static int64_t seek_and_read(int fd, uint8_t *buf, mus_long_t offset, int nbytes)
{
  if (offset < 0) return(-1);
  lseek(fd, offset, SEEK_SET);
  return(read(fd, buf, nbytes));
}


static int read_aiff_marker(int m, uint8_t *buf)
{
  int psize;
  marker_ids[m] = mus_char_to_bshort((uint8_t *)buf);
  marker_positions[m] = mus_char_to_bint((uint8_t *)(buf + 2));
  psize = (int)buf[6] + 1; 
  if (psize & 1) psize++; 
  return(psize+6);
}


static void read_aif_mark_chunk(int fd, uint8_t *buf, mus_long_t offset)
{
  int num_marks, m, moff, msize;
  /* unsigned short #marks, each mark: id pos name (pstring damn it) */
  num_marks = mus_char_to_ubshort((uint8_t *)(buf + 8));
  if (num_marks > markers)
    {
      if (markers > 0)
	{
	  if (marker_ids) free(marker_ids); 
	  if (marker_positions) free(marker_positions);
	}
      markers = num_marks;
      marker_ids = (int *)calloc(markers, sizeof(int));
      marker_positions = (int *)calloc(markers, sizeof(int));
    }
  moff = 10;
  for (m = 0; m < num_marks; m++)
    {
      if (seek_and_read(fd, (uint8_t *)buf, offset + moff, 8) > 0)
	{
	  msize = read_aiff_marker(m, (uint8_t *)buf);
	  moff += msize;
	}
    }
}


static void read_aif_inst_chunk(uint8_t *buf)
{
  base_note = buf[8];
  base_detune = buf[9];
  loop_modes[0] = mus_char_to_bshort((uint8_t *)(buf + 16));
  loop_starts[0] = mus_char_to_bshort((uint8_t *)(buf + 18));
  loop_ends[0] = mus_char_to_bshort((uint8_t *)(buf + 20));
  loop_modes[1] = mus_char_to_bshort((uint8_t *)(buf + 22));
  loop_starts[1] = mus_char_to_bshort((uint8_t *)(buf + 24));
  loop_ends[1] = mus_char_to_bshort((uint8_t *)(buf + 26));
  /* these are mark numbers */
}


static void read_aif_aux_comment(uint8_t *buf, mus_long_t offset, int chunksize)
{
  int i, j = 0;
  for (i = 0; i < AUX_COMMENTS; i++) 
    if (aux_comment_start[i] == 0) 
      {
	j = i; 
	break;
      }
  if (j >= AUX_COMMENTS) 
    {
      mus_print("read_aiff_header: ran out of auxiliary comment space");
      j = 0;
    }
  aux_comment_start[j] = offset + 8;
  if (match_four_chars((uint8_t *)buf, I_COMT)) 
    aux_comment_start[j] += 8; /* skip time stamp and markerId (not ID, I assume!) */
  aux_comment_end[j] = offset + 7 + chunksize;
}


static void read_aif_appl_chunk(uint8_t *buf, mus_long_t offset, int chunksize)
{
  static const uint8_t I_SU7M[4] = {'S','U','7','M'};  
  static const uint8_t I_SU7R[4] = {'S','U','7','R'};  

  if (match_four_chars((uint8_t *)(buf + 8), I_MUS_))
    {
      /* my chunk has an arbitrary length comment (a lisp program evaluated in the CLM package) 
       *   to handle mix et al.  Can't use the built-in chunk for this because the comment length might
       *   be greater than 65536 chars.  Need to remember to pad to even length here. 
       */
      comment_start = offset + 12;
      comment_end = comment_start + chunksize - 5;
    }
  else 
    {
      if ((match_four_chars((uint8_t *)(buf + 8), I_SU7M)) ||
	  (match_four_chars((uint8_t *)(buf + 8), I_SU7R)))
	{
	  mus_print("this is an SU700 ssp file?");
	  data_location = 512;
	  chans = 1;
	  /* actually SU7M and SU7R point to 2 chan data as separate chunks */
	}
    }
}


static void mus_ubint_to_char(uint8_t *j, uint32_t x)
{
  uint8_t *ox = (uint8_t *)&x;
#if MUS_LITTLE_ENDIAN
  j[0] = ox[3]; j[1] = ox[2]; j[2] = ox[1]; j[3] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 4);
#endif
}


static int read_aiff_header(const char *filename, int fd, int overall_offset)
{
  /* we know we have checked for FORM xxxx AIFF|AIFC when we arrive here */
  /* as far as I can tell, the COMM block has the header data we seek, and the SSND block has the sound data */
  uint32_t chunkloc, ssnd_bytes = 0;
  int i;
  bool happy = true, got_comm = false;
  mus_long_t offset = 0;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 8 + overall_offset));
  update_ssnd_location = 0;
  chunkloc = 12 + overall_offset;
  for (i = 0; i < AUX_COMMENTS; i++) aux_comment_start[i] = 0;
  sample_type = MUS_BSHORT;
  srate = 0;
  chans = 0;
  markers = 0;
  if (marker_ids) free(marker_ids); 
  if (marker_positions) free(marker_positions);
  marker_ids = NULL;
  marker_positions = NULL;

  true_file_length = SEEK_FILE_LENGTH(fd);
  update_form_size = mus_char_to_ubint((uint8_t *)(hdrbuf + 4 + overall_offset)); /* should be file-size - 8 unless there are multiple forms */

  while (happy)
    {
      uint32_t chunksize;
      offset += chunkloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	{
	  if ((got_comm) && (data_location > 0))
	    {
	      mus_print("%s, aiff header: chunks confused at %" print_mus_long "; will try to continue", filename, offset);
	      break;
	    }
	  return(mus_error(MUS_HEADER_READ_FAILED, "%s, aiff header: chunks confused at %" print_mus_long , filename, offset));
	}

      chunksize = mus_char_to_ubint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) && /* can be empty data chunk */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;

      /* fprintf(stderr, "chunk: %c%c%c%c for %d\n", hdrbuf[0], hdrbuf[1], hdrbuf[2], hdrbuf[3], chunksize); */

      if (match_four_chars((uint8_t *)hdrbuf, I_COMM))
	{
	  uint32_t framples;
	  got_comm = true;

	  chans = mus_char_to_bshort((uint8_t *)(hdrbuf + 8));
	  framples = mus_char_to_ubint((uint8_t *)(hdrbuf + 10)); /* was bint 27-Jul-01 */
	  update_framples_location = 10 + offset;

	  original_sample_type = mus_char_to_bshort((uint8_t *)(hdrbuf + 14));
	  if ((original_sample_type % 8) != 0) 
	    {
	      /* weird sizes are legal --
	       * these samples are left-justified (and zero padded on the right), so
	       * we can handle any bit size by rounding up to the nearest byte.
	       */
	      original_sample_type = 8 * (1 + (original_sample_type >> 3));
	    }
	  if (original_sample_type == 8) sample_type = MUS_BYTE;
	  else if (original_sample_type == 16) sample_type = MUS_BSHORT;
	  else if (original_sample_type == 24) sample_type = MUS_B24INT;
	  else if (original_sample_type == 32) sample_type = MUS_BINT;
	  else if (original_sample_type == 64) sample_type = MUS_BDOUBLE;
	  else return(mus_error(MUS_HEADER_READ_FAILED, "%s: bits per sample: %d?", filename, mus_char_to_bshort((uint8_t *)(hdrbuf + 14))));

	  srate = (int)ieee_80_to_double((uint8_t *)(hdrbuf + 16));

	  /* if AIFC, compression type over-rides (possibly bogus) original_sample_type */
	  if (type_specifier == mus_char_to_uninterpreted_int((unsigned const char *)I_AIFC))
	    {
	      static const uint8_t I_twos[4] = {'t','w','o','s'};  /* AIFC big endian? */
	      /* some aifc files assume the compression field is a new and very weird chunk!! -- surely a bug? */
	      /* AIFF spec says COMM size is always 18, but this is amended in the newer AIFC spec */
	      if (chunksize == 18) chunksize += (5 + ((int)hdrbuf[30]));             /* 5 = chunk header length in this case */
	      if ((!(match_four_chars((uint8_t *)(hdrbuf + 26), I_NONE))) &&
		  (!(match_four_chars((uint8_t *)(hdrbuf + 26), I_twos))))
		{
		  static const uint8_t I_ALAW[4] = {'A','L','A','W'};
		  original_sample_type = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 26));
		  if ((match_four_chars((uint8_t *)(hdrbuf + 26), I_ALAW)) || 
		      (match_four_chars((uint8_t *)(hdrbuf + 26), I_alaw)))
		    sample_type = MUS_ALAW;
		  else 
		    {
		      if ((match_four_chars((uint8_t *)(hdrbuf + 26), I_ULAW)) ||
			  (match_four_chars((uint8_t *)(hdrbuf + 26), I_ulaw)))
			sample_type = MUS_MULAW;
		      else 
			{
			  static const uint8_t I_ni23[4] = {'n','i','2','3'};
			  if ((match_four_chars((uint8_t *)(hdrbuf + 26), I_sowt)) ||
			      (match_four_chars((uint8_t *)(hdrbuf + 26), I_ni23)))
			    {
			      /* Sound.h sez sowt is just 16-bit format */
			      if (sample_type == MUS_BSHORT) sample_type = MUS_LSHORT;
			      else if (sample_type == MUS_B24INT) sample_type = MUS_L24INT;
			      else if (sample_type == MUS_BINT) sample_type = MUS_LINT;
			    }
			  else
			    {
			      if (match_four_chars((uint8_t *)(hdrbuf + 26), I_raw_))
				{
				  if (sample_type == MUS_BYTE) sample_type = MUS_UBYTE;
				  else if (sample_type == MUS_BSHORT) sample_type = MUS_UBSHORT;
				}
			      else
				{
				  uint8_t I_FL32[4] = {'F','L','3','2'};  /* 32-bit float (apparently used by CSound and SoundHack) */
				  if ((match_four_chars((uint8_t *)(hdrbuf + 26), I_fl32)) ||
				      (match_four_chars((uint8_t *)(hdrbuf + 26), I_FL32)))
				    sample_type = MUS_BFLOAT;
				  else
				    {
				      if (match_four_chars((uint8_t *)(hdrbuf + 26), I_fl64))
					sample_type = MUS_BDOUBLE;
				      else
					{
					  static const uint8_t I_ima4[4] = {'i','m','a','4'};  /* AIFC IMA adpcm apparently */
					  if (match_four_chars((uint8_t *)(hdrbuf + 26), I_ima4))
					    {
					      block_align = 34;
					      original_sample_type = MUS_AIFF_IMA_ADPCM;
					    }
					  else
					    {
					      static const uint8_t I_in32[4] = {'i','n','3','2'};
					      if (match_four_chars((uint8_t *)(hdrbuf + 26), I_in32))
						sample_type = MUS_BINT;
					      else
						{
						  static const uint8_t I_in24[4] = {'i','n','2','4'};
						  if (match_four_chars((uint8_t *)(hdrbuf + 26), I_in24))
						    sample_type = MUS_B24INT;
						  else
						    {
						      /* others from Sound.h:
							 0x6D730002, -- Microsoft ADPCM - ACM code 2
							 0x6D730011, -- DVI/Intel IMA ADPCM - ACM code 17
							 'MAC3' -- MACE 3:1
							 'MAC6' -- MACE 6:1
							 'cdx4' -- CD/XA 4:1
							 'cdx2' -- CD/XA 2:1
							 'dvca' -- DV Audio
							 'QDMC' -- QDesign music
							 'QDM2' -- QDesign2 music
							 'Qclp' -- QUALCOMM PureVoice
							 0x6D730055 -- MPEG Layer 3, CBR only (pre QT4.1)
							 '.mp3' -- MPEG Layer 3, CBR & VBR (QT4.1 and later)

							 openquicktime and ffmpeg have decoders for some of these; all too complex for my taste
							 on a Mac, we could apparently pick up decoders from coreaudio
						      */
						      sample_type = MUS_UNKNOWN_SAMPLE;
						    }
						}
					    }
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	  data_size = (framples * mus_bytes_per_sample(sample_type) * chans);
	}
      else
	{
	  if (match_four_chars((uint8_t *)hdrbuf, I_SSND))
	    {
	      if (data_location != 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: two SSND chunks found", filename));
	      update_ssnd_location = offset + 4;
	      data_location = mus_char_to_bint((uint8_t *)(hdrbuf + 8)) + offset + 16; /* Baroque! */

	      /* offset is where the hdrbuf is positioned in the file, the sound data offset itself is at loc+8 and the */
	      /* 0-based location of the sound data is at the end of the chunk = 16 (8 = header+4 = offset+4 = blocksize) */
	      /* the next int can be the block size if the data is block-aligned */
	      /* only one SSND per AIFF is allowed */

	      if (chunksize == 0) break; /* this may happen while pre-reading an in-progress output file for updating */
	      ssnd_bytes = offset + chunksize - data_location + 8;
	    }
	  else
	    {
	      static const uint8_t I_AUTH[4] = {'A','U','T','H'};
	      if ((match_four_chars((uint8_t *)hdrbuf, I_ANNO)) || 
		  (match_four_chars((uint8_t *)hdrbuf, I_COMT)) ||
		  (match_four_chars((uint8_t *)hdrbuf, I_NAME)) ||
		  (match_four_chars((uint8_t *)hdrbuf, I_AUTH)))
		read_aif_aux_comment(hdrbuf, offset, chunksize);
	      else
		{
		  if (match_four_chars((uint8_t *)hdrbuf, I_APPL))
		    read_aif_appl_chunk(hdrbuf, offset, chunksize);
		  else
		    {
		      if (match_four_chars((uint8_t *)hdrbuf, I_INST))
			read_aif_inst_chunk(hdrbuf);
		      else
			{
			  if (match_four_chars((uint8_t *)hdrbuf, I_MARK))
			    read_aif_mark_chunk(fd, hdrbuf, offset);
			}
		    }
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
      if ((offset + chunkloc) >= update_form_size) happy = false;
    }

  if (!got_comm)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no COMM chunk", filename));
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no SSND (data) chunk", filename));

  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }

  if ((data_size > ssnd_bytes) && (sample_type != MUS_UNKNOWN_SAMPLE))
    data_size = ssnd_bytes;
  data_size = mus_bytes_to_samples(sample_type, data_size);

  return(MUS_NO_ERROR);
}


static int sndlib_format_to_aiff_bits(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_BSHORT: case MUS_LSHORT: case MUS_UBSHORT: case MUS_ULSHORT:  return(16); 
    case MUS_B24INT: case MUS_L24INT:                                      return(24); 
    case MUS_BINT: case MUS_LINT: case MUS_BFLOAT: case MUS_LFLOAT:        return(32); 
    case MUS_BDOUBLE: case MUS_LDOUBLE:                                    return(64); 
    case MUS_BYTE: case MUS_UBYTE: case MUS_MULAW: case MUS_ALAW:          return(8);  
    default: 
      return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE, "aiff header: can't write sample type: %d (%s)",
		       samp_type,
		       any_sample_type_name(samp_type)));
      break;
    }
}


static const char *sndlib_format_to_aifc_name(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_BSHORT: case MUS_B24INT: case MUS_BINT: case MUS_BYTE: return((const char *)I_NONE); /* use in24 and in32? */
    case MUS_LSHORT: case MUS_L24INT: case MUS_LINT:                return((const char *)I_sowt); /* should this use ni23? */
    case MUS_BFLOAT:                                                return((const char *)I_fl32); 
    case MUS_BDOUBLE:                                               return((const char *)I_fl64); 
    case MUS_UBYTE: case MUS_UBSHORT:                               return((const char *)I_raw_); 
    case MUS_MULAW:                                                 return((const char *)I_ulaw); 
    case MUS_ALAW:                                                  return((const char *)I_alaw); 
    default:                                                        return((const char *)I_NONE); 
    }
}


static int write_aif_header(int fd, int wsrate, int wchans, mus_long_t siz, mus_sample_t samp_type, const char *comment, int len, bool aifc_header)
{
  /* we write the simplest possible AIFC header: AIFC | COMM | APPL-MUS_ if needed | SSND eof. */
  /* the assumption being that we're going to be appending sound data once the header is out   */
  /* INST and MARK chunks added Jul-95 for various programs that expect them (MixView).        */
  /* set aifc_header to false to get old-style AIFF header */
  int i, j, lenhdr = 0, lenloop, curend = 0, extra = 0;         

  lenloop = 38;
  if ((loop_modes[0] != 0) || (loop_modes[1] != 0)) lenloop = 42 + 28;
  if (len != 0) 
    {
      lenhdr = 12;
      if ((len % 4) != 0)
	extra = (4 - (len % 4));
    }

  write_four_chars((uint8_t *)hdrbuf, I_FORM);
  if (aifc_header) 
    mus_ubint_to_char((uint8_t *)(hdrbuf + 4), len + 30 + 16 + lenloop + siz + lenhdr + extra + 12 + 10);
  else mus_ubint_to_char((uint8_t *)(hdrbuf + 4), len + 30 + 16 + lenloop + siz + lenhdr + extra);

  /* 
   * comment length + 4 for AIFF 18+8 for I_COMM info + 16 for I_SSND info + 38 for INST and MARK +
   * siz for data + 12 for comment header if any + padding == total size - 8 (i.e. FORM header).   
   * INST+MARK (38) added 3-Jul-95 for Notam software compatibility 
   */
  if (aifc_header) 
    {
      static const uint8_t I_FVER[4] = {'F','V','E','R'};
      write_four_chars((uint8_t *)(hdrbuf + 8), I_AIFC); 
      header_write(fd, hdrbuf, 12);
      curend = 12;
      write_four_chars((uint8_t *)hdrbuf, I_FVER);
      mus_bint_to_char((uint8_t *)(hdrbuf + 4), 4);
      mus_bint_to_char((uint8_t *)(hdrbuf + 8), 0xA2805140);
    }
  else write_four_chars((uint8_t *)(hdrbuf + 8), I_AIFF);

  write_four_chars((uint8_t *)(hdrbuf + 12), I_COMM);
  if (aifc_header) 
    mus_bint_to_char((uint8_t *)(hdrbuf + 16), 18 + 10); 
  else mus_bint_to_char((uint8_t *)(hdrbuf + 16), 18);

  mus_bshort_to_char((uint8_t *)(hdrbuf + 20), (short)wchans);
  if (wchans > 0)
    mus_ubint_to_char((uint8_t *)(hdrbuf + 22), siz / (wchans * mus_bytes_per_sample(samp_type)));

  mus_bshort_to_char((uint8_t *)(hdrbuf + 26), sndlib_format_to_aiff_bits(samp_type));
  double_to_ieee_80((double)wsrate, (uint8_t *)(hdrbuf + 28));

  if (aifc_header)
    {
      char *str;
      str = (char *)sndlib_format_to_aifc_name(samp_type);
      write_four_chars((uint8_t *)(hdrbuf + 38), (const uint8_t *)str);
      (*(uint8_t *)(hdrbuf + 42)) = 4; /* final pad null not accounted-for */
      write_four_chars((uint8_t *)(hdrbuf + 43), (const uint8_t *)str);
      (*(uint8_t *)(hdrbuf + 47)) = 0;
      i = 48;
    }
  else i = 38;

  if (len != 0)
    {
      if (aifc_header)
	{
	  write_four_chars((uint8_t *)(hdrbuf + 48), I_APPL);
	  mus_bint_to_char((uint8_t *)(hdrbuf + 52), len + 4 + extra);
	  write_four_chars((uint8_t *)(hdrbuf + 56), I_MUS_);
	  i = 60;
	}
      else
	{
	  write_four_chars((uint8_t *)(hdrbuf + 38), I_APPL);
	  mus_bint_to_char((uint8_t *)(hdrbuf + 42), len + 4 + extra);
	  write_four_chars((uint8_t *)(hdrbuf + 46), I_MUS_);
	  i = 50;
	}
      for (j = 0; j < len; j++)
	{
	  if (i == HDRBUFSIZ)
	    {
	      curend += HDRBUFSIZ;
	      header_write(fd, hdrbuf, HDRBUFSIZ);
	      i = 0;
	    }
	  hdrbuf[i] = comment[j];
	  i++;
	}
      if (extra != 0)
	{
	  if ((i + extra) > HDRBUFSIZ)
	    {
	      curend += i;
	      header_write(fd, hdrbuf, i);
	      i = 0;
	    }
	  for (j = 0; j < extra; j++)
	    {
	      hdrbuf[i] = 0;
	      i++;
	    }
	}
    }
  curend += i;
  header_write(fd, hdrbuf, i);

  if ((loop_modes[0] == 0) && (loop_modes[1] == 0))
    {
      write_four_chars((uint8_t *)hdrbuf, I_MARK);   /* SoundHack includes a blank MARK chunk for some reason */
      mus_bint_to_char((uint8_t *)(hdrbuf + 4), 2);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 8), 0);
      write_four_chars((uint8_t *)(hdrbuf + 10), I_INST);
      mus_bint_to_char((uint8_t *)(hdrbuf + 14), 20);
      mus_bint_to_char((uint8_t *)(hdrbuf + 18), 0x3c00007f); /* base-note = middle C, detune = 0, lownote = 0, highnote = 0x7f */
      mus_bint_to_char((uint8_t *)(hdrbuf + 22), 0x017f0000); /* lowvelocity = 1, highvelocity = 0x7f, gain = 0 */
      mus_bint_to_char((uint8_t *)(hdrbuf + 26), 0);          /* no loops */
      mus_bint_to_char((uint8_t *)(hdrbuf + 30), 0); 
      mus_bint_to_char((uint8_t *)(hdrbuf + 34), 0);
      header_write(fd, hdrbuf, 38);
      curend += 38;
    }
  else
    {
      write_four_chars((uint8_t *)hdrbuf, I_MARK); 
      mus_bint_to_char((uint8_t *)(hdrbuf + 4), 8 * 4 + 2); /* 2 for mark#, then 2:id + 4:pos + 2:pstr */
      /* loop_info: 0..3 are markers positions (ids 1..4) */
      mus_bshort_to_char((uint8_t *)(hdrbuf + 8), 4);
      for (j = 0; j < 4; j++)
	{
	  mus_bshort_to_char((uint8_t *)(hdrbuf + 10 + 8 * j), j + 1);
	  switch (j)
	    {
	    case 0: mus_bint_to_char((uint8_t *)(hdrbuf + 10 + 8 * j + 2), loop_starts[0]); break;
	    case 1: mus_bint_to_char((uint8_t *)(hdrbuf + 10 + 8 * j + 2), loop_ends[0]);   break;
	    case 2: mus_bint_to_char((uint8_t *)(hdrbuf + 10 + 8 * j + 2), loop_starts[1]); break;
	    case 3: mus_bint_to_char((uint8_t *)(hdrbuf + 10 + 8 * j + 2), loop_ends[1]);   break;
	    }
	  mus_bshort_to_char((uint8_t *)(hdrbuf + 10 + 8 * j + 6), 0);
	}
      header_write(fd, hdrbuf, 42);
      curend += 42;
      write_four_chars((uint8_t *)hdrbuf, I_INST); 
      mus_bint_to_char((uint8_t *)(hdrbuf + 4), 20);
      mus_bint_to_char((uint8_t *)(hdrbuf + 8), 0x3c00007f);
      mus_bint_to_char((uint8_t *)(hdrbuf + 12), 0x017f0000);
      hdrbuf[8] = (uint8_t)(base_note);
      hdrbuf[9] = (uint8_t)(base_detune);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 16), loop_modes[0]);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 18), 1);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 20), 2);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 22), loop_modes[1]);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 24), 3);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 26), 4);
      header_write(fd, hdrbuf, 28);
      curend += 28;
    }
  write_four_chars((uint8_t *)(hdrbuf), I_SSND);
  mus_bint_to_char((uint8_t *)(hdrbuf + 4), siz + 8);
  mus_bint_to_char((uint8_t *)(hdrbuf + 8), 0);                        /* "offset" */
  mus_bint_to_char((uint8_t *)(hdrbuf + 12), 0);                       /* "blocksize " */
  header_write(fd, hdrbuf, 16);
  data_location = 16 + curend;
  return(MUS_NO_ERROR);
}


char *mus_header_aiff_aux_comment(const char *name, mus_long_t *starts, mus_long_t *ends)
{
  /* AIFC: look for aux comments (ANNO chunks) */
  char *sc = NULL;
  if ((starts) && (starts[0] != 0))
    {
      mus_long_t full_len;
      int fd, i;
      fd = mus_file_open_read(name);
      if (fd == -1) return(NULL);
      full_len = 0;
      for (i = 0; i < AUX_COMMENTS; i++) 
	if ((starts[i] > 0) && 
	    (starts[i] < ends[i]))
	  full_len += (ends[i] - starts[i] + 3);
      if (full_len > 0)
	{
	  mus_long_t sc_len;
	  sc = (char *)calloc(full_len, sizeof(char));
	  sc_len = 0;
	  for (i = 0; i < AUX_COMMENTS; i++) 
	    {
	      mus_long_t start, end;
	      start = starts[i];
	      end = ends[i];
	      if ((start > 0) && (start < end))
		{
		  int j;
		  mus_long_t len;
		  len = end - start + 1;
		  lseek(fd, start, SEEK_SET);
		  header_read(fd, (uint8_t *)(sc + sc_len), len);
		  for (j = 0; j < len; j++) 
		    if (sc[j + sc_len] == 0) 
		      sc[j + sc_len] = ' ';
		  sc_len += len;
		  sc[sc_len++] = '\n';
		}
	    }
	}
      CLOSE(fd, name);
    }
  return(sc);
}



/* ------------------------------------ CAFF ------------------------------------
 *
 * this is a new format from Apple ("Core Audio File Format") described at
 *    http://developer.apple.com/documentation/MusicAudio/Reference/CAFSpec
 *
 * all chunks as in AIFC but size is signed 64-bit int
 *
 * 0: 'caff'
 * 4: 1 ("version")
 * 6: 0 ("flags")
 * 8: 'desc' (required to be in this position)
 * 12: sizeof apple's CAFAudioFormat struct -- (32)
 *   20: srate (float64)
 *   28: format (int32)
 *   32: format flags
 *   36: bytes per "packet"
 *   40: framples per packet
 *   44: channels per frample
 *   48: bits per channel
 * audio data is in 'data' chunk
 */


static int read_caff_header(int fd)
{
  mus_long_t chunksize = 8, offset = 0;
  bool happy = false;
  int err = MUS_NO_ERROR;

  #define data_is_float (1L << 0)
  #define data_is_big_endian (1L << 1)
  /* misleading name -- flag is 1 if data is little endian */

  sample_type = MUS_UNKNOWN_SAMPLE;
  srate = 0;
  chans = 0;
  data_size = 0;
  true_file_length = SEEK_FILE_LENGTH(fd);

  while (!happy)
    {
      offset += chunksize;
      if (offset >= true_file_length) break;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 64) <= 0) break;
      chunksize = mus_char_to_blong((uint8_t *)(hdrbuf + 4));
      if (chunksize < 0)
	break;

      /* 'desc' is always the first chunk, but easier to handle in the loop */
      if (match_four_chars((uint8_t *)hdrbuf, I_desc))
	{
	  int format_flags, channels_per_frample, bits_per_channel;
	  srate = (int)mus_char_to_bdouble((uint8_t *)(hdrbuf + 12));
	  format_flags = mus_char_to_ubint((uint8_t *)(hdrbuf + 24));
	  /* bytes_per_packet = mus_char_to_ubint((uint8_t *)(hdrbuf + 28)); */
	  /* framples_per_packet = mus_char_to_ubint((uint8_t *)(hdrbuf + 32)); */
	  channels_per_frample = mus_char_to_ubint((uint8_t *)(hdrbuf + 36));
	  bits_per_channel = mus_char_to_ubint((uint8_t *)(hdrbuf + 40));
	  chans = channels_per_frample;

	  /* format id can be 'lpcm' 'alaw' 'ulaw' and a bunch of others we ignore */
	  original_sample_type = mus_char_to_bint((uint8_t *)(hdrbuf + 20));	  
	  if (match_four_chars((uint8_t *)(hdrbuf + 20), I_lpcm))
	    {
	      if (format_flags & data_is_float)
		{
		  if (!(format_flags & data_is_big_endian))
		    {
		      if (bits_per_channel == 32)
			sample_type = MUS_BFLOAT;
		      else
			{
			  if (bits_per_channel == 64)
			    sample_type = MUS_BDOUBLE;
			  else err = MUS_UNSUPPORTED_SAMPLE_TYPE;
			}
		    }
		  else
		    {
		      if (bits_per_channel == 32)
			sample_type = MUS_LFLOAT;
		      else
			{
			  if (bits_per_channel == 64)
			    sample_type = MUS_LDOUBLE;
			  else err = MUS_UNSUPPORTED_SAMPLE_TYPE;
			}
		    }
		}
	      else
		{
		  if (!(format_flags & data_is_big_endian))
		    {
		      if (bits_per_channel == 32)
			sample_type = MUS_BINTN;
		      else
			{
			  if (bits_per_channel == 24)
			    sample_type = MUS_B24INT;
			  else
			    {
			      if (bits_per_channel == 16)
				sample_type = MUS_BSHORT;
			      else
				{
				  if (bits_per_channel == 8)
				    sample_type = MUS_BYTE;
				  else err = MUS_UNSUPPORTED_SAMPLE_TYPE;
				}
			    }
			}
		    }
		  else
		    {
		      if (bits_per_channel == 32)
			sample_type = MUS_LINTN;
		      else
			{
			  if (bits_per_channel == 24)
			    sample_type = MUS_L24INT;
			  else
			    {
			      if (bits_per_channel == 16)
				sample_type = MUS_LSHORT;
			      else
				{
				  if (bits_per_channel == 8)
				    sample_type = MUS_BYTE;
				  else err = MUS_UNSUPPORTED_SAMPLE_TYPE;
				}
			    }
			}
		    }
		}
	    }
	  else
	    {
	      if (match_four_chars((uint8_t *)(hdrbuf + 20), I_alaw))
		{
		  sample_type = MUS_ALAW;
		}
	      else
		{
		  if (match_four_chars((uint8_t *)(hdrbuf + 20), I_ulaw))
		    {
		      sample_type = MUS_MULAW;
		    }
		  else err = MUS_UNSUPPORTED_SAMPLE_TYPE;
		}
	    }
	}
      else
	{
	  if (match_four_chars((uint8_t *)hdrbuf, I_data))
	    {
	      happy = true;
	      data_location = offset + 16; /* skip the 4 bytes for "edit count" */
	      update_framples_location = offset + 4;

	      /* here chunksize can be -1! */
	      if (chunksize > 0)
		data_size = chunksize;
	      else data_size = true_file_length - data_location;
	    }
	  /* else edct or something for comment? */
	      
	}
      chunksize += 12;
    }

  if (err == MUS_NO_ERROR)
    data_size = mus_bytes_to_samples(sample_type, data_size);

  return(err);
}


static int write_caff_header(int fd, int wsrate, int wchans, mus_long_t wsize, mus_sample_t samp_type)
{
  int format_flags = 0, bytes_per_packet = 0, framples_per_packet = 1, bits_per_channel = 0;

  switch (samp_type)
    {
    case MUS_ALAW:
      bytes_per_packet = 1;
      bits_per_channel = 8;
      break;

    case MUS_MULAW:
      bytes_per_packet = 1;
      bits_per_channel = 8;
      break;

    default:
      if ((samp_type == MUS_LFLOAT) || (samp_type == MUS_LDOUBLE) || (samp_type == MUS_BFLOAT) || (samp_type == MUS_BDOUBLE))
	format_flags = 1;
      if ((samp_type == MUS_LFLOAT) || (samp_type == MUS_LDOUBLE) || (samp_type == MUS_LINTN) || (samp_type == MUS_L24INT) || (samp_type == MUS_LSHORT))
	format_flags |= 2;
      switch (samp_type)
	{
	case MUS_BYTE:
	  bytes_per_packet = 1;
	  bits_per_channel = 8;
	  break;

	case MUS_LSHORT: case MUS_BSHORT:
	  bytes_per_packet = 2;
	  bits_per_channel = 16;
	  break;

	case MUS_L24INT: case MUS_B24INT:
	  bytes_per_packet = 3;
	  bits_per_channel = 24;
	  break;

	case MUS_LFLOAT: case MUS_BFLOAT: case MUS_BINTN: case MUS_LINTN:
	  bytes_per_packet = 4;
	  bits_per_channel = 32;
	  break;

	case MUS_LDOUBLE: case MUS_BDOUBLE:
	  bytes_per_packet = 8;
	  bits_per_channel = 64;
	  break;

	default: break;
	}
      break;
    }
  bytes_per_packet *= wchans;

  write_four_chars((uint8_t *)hdrbuf, I_caff);
  mus_bshort_to_char((uint8_t *)(hdrbuf + 4), 1);
  mus_bshort_to_char((uint8_t *)(hdrbuf + 6), 0);
  write_four_chars((uint8_t *)(hdrbuf + 8), I_desc);
  mus_blong_to_char((uint8_t *)(hdrbuf + 12), 32);
  mus_bdouble_to_char((uint8_t *)(hdrbuf + 20), (double)wsrate);
  switch (samp_type)
    {
    case MUS_ALAW:
      write_four_chars((uint8_t *)(hdrbuf + 28), I_alaw);
      break;

    case MUS_MULAW:
      write_four_chars((uint8_t *)(hdrbuf + 28), I_ulaw);
      break;

    default:
      write_four_chars((uint8_t *)(hdrbuf + 28), I_lpcm);
      break;
    }
  mus_bint_to_char((uint8_t *)(hdrbuf + 32), format_flags);
  mus_bint_to_char((uint8_t *)(hdrbuf + 36), bytes_per_packet);
  mus_bint_to_char((uint8_t *)(hdrbuf + 40), framples_per_packet);
  mus_bint_to_char((uint8_t *)(hdrbuf + 44), wchans);
  mus_bint_to_char((uint8_t *)(hdrbuf + 48), bits_per_channel);
  write_four_chars((uint8_t *)(hdrbuf + 52), I_data);
  mus_blong_to_char((uint8_t *)(hdrbuf + 56), wsize);
  update_framples_location = 56;
  mus_bint_to_char((uint8_t *)(hdrbuf + 64), 0);
  data_location = 68;
  header_write(fd, hdrbuf, 68);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ RIFF (wave) ------------------------------------
 *
 * see ftp.microsoft.com:/SoftLib/MSLFILES/MDRK.EXE (also MMSYSTEM.H and MMREG.H)
 *     ftp://ftp.isi.edu/in-notes/rfc2361.txt
 *
 *   0: "RIFF" (little-endian) or "RIFX" (big-endian)
 *   4: size
 *   8: "WAVE"  ("RMID" = midi data, others are AVI, CPPO, ACON, DLS? etc)
 *       AVI chunk can include audio data
 *  
 *   rest very similar to AIFF (odd-sized chunks are padded)
 *
 * fmt  0: format code (see below)
 *      2: chans
 *      4: srate (long)
 *      8: average rate "for buffer estimation"
 *     12: alignment "block size"
 *     14: data size (bits per sample) (PCM only)
 *     16: count (bytes) of extra info in the header (i.e. trailing info added to this basic header)
 *     20: samples per block (short) in dvi_adpcm
 *  
 * formats are: 0: unknown, 1: PCM, 2: ADPCM, 3: IEEE float, 4: VSELP, 5: IBM_CVSD, 6: alaw, 7: mulaw
 *              0x10: OKI_ADPCM, 0x11: DVI_ADPCM, 0x12: MediaSpace_ADPCM,
 *              0x13: Sierra_ADPCM, 0x14: G723_ADPCM, 0x15: DIGISTD, 0x16: DIGIFIX, 0x17: Dialogic ADPCM,
 *              0x18: Mediavision ADPCM, 0x19: HP cu codec, 
 *              0x20: Yamaha_ADPCM, 0x21: SONARC, 0x22: DSPGroup_TrueSpeech
 *              0x23: EchoSC1, 0x24: AudioFile_AF36, 0x25: APTX, 0x26: AudioFile_AF10
 *              0x27: prosody 1612, 0x28: lrc,
 *              0x30: Dolby_Ac2, 0x31: GSM610, 0x32: MSN audio codec, 0x33: Antext_ADPCM, 0x34: Control_res_vqlpc,
 *              0x35: DIGIREAL, 0x36: DIGIADPCM, 0x37: Control_res_cr10, 0x38: NMS_VBXADPCM, 0x39:Roland rdac,
 *              0x3a: echo sc3, 0x3b: Rockwell adpcm, 0x3c: Rockwell digitalk codec, 0x3d: Xebec,
 *              0x40: G721_ADPCM, 0x41: G728 CELP, 0x42: MS G723, 0x50: MPEG, 
 *              0x52: RT24, 0x53: PAC, 0x55: Mpeg layer 3, 0x59: Lucent G723, 0x60: Cirrus,
 *              0x61: ESS Tech pcm, 0x62: voxware (obsolete), 0x63: canopus atrac,
 *              0x64: G726, 0x65: G722, 0x66: DSAT, 0x67: DSAT display,
 *              0x69: voxware (obsolete), 0x70: voxware ac8 (obsolete), 0x71: voxware ac10 (obsolete), 
 *              0x72: voxware ac16 (obsolete), 0x73: voxware ac20 (obsolete), 0x74: voxware rt24, 
 *              0x75: voxware rt29, 0x76: voxware rt29hw (obsolete), 0x77: voxware vr12 (obsolete),
 *              0x78: voxware vr18 (obsolete), 0x79: voxware tq40 (obsolete), 
 *              0x80: softsound, 0x81: voxware tq60 (obsolete), 0x82: MS RT24, 0x83: G729A,
 *              0x84: MVI_MVI2, 0x85: DF G726, 0x86: DF GSM610, 0x88: isaudio, 0x89: onlive,
 *              0x91: sbc24, 0x92: dolby ac3 spdif, 0x97: zyxel adpcm, 0x98: philips lpcbb,
 *              0x99: packed, 0x100: rhetorex adpcm, 
 *              0x101: Irat, 0x102: IBM_alaw?, 0x103: IBM_ADPCM?, 
 *              0x111: vivo G723, 0x112: vivo siren, 0x123: digital g273
 *              0x200: Creative_ADPCM, 0x202: Creative fastspeech 8, 0x203: Creative fastspeech 10, 
 *              0x220: quarterdeck, 0x300: FM_TOWNS_SND, 0x400: BTV digital, 0x680: VME vmpcm,
 *              0x1000: OLIGSM, 0x1001: OLIADPCM, 0x1002: OLICELP, 0x1003: OLISBC, 0x1004: OLIOPR
 *              0x1100: LH codec, 0x1400: Norris, 0x1401: isaudio, 0x1500: Soundspace musicompression, 0x2000: DVM
 * (see http://www.microsoft.com/asf/resources/draft-ietf-fleischman-codec-subtree-00.txt)
 *   and new:   0xFFFE: wave_format_extensible: bits/sample, mapping, 16 byte guid, 1st 2 bytes are code as above
 *
 * RIFF and LIST chunks have nested chunks.  Registered chunk names include:
 *   LIST with subchunks, one of which can be:
 *     INFO itself containing:
 *       IARL: archival location, IART: artist, ICMS: commissioned, ICMT: comment, ICOP: copyright, ICRD: creation date,
 *       ICRP: uh...cropped, IDIM: dimensions, IDPI: dpi, IENG: engineer, IGNR: genre, IKEY: keywords, ILGT: lightness,
 *       IMED: medium, INAM: name, IPLT: palette, IPRD: product, ISBJ: subject, ISFT: software, ISHP: sharpness,
 *       ISRC: source, ISRF: source form, ITCH: technician, ISMP: SMPTE time code, IDIT: digitization time
 *
 * data chunk has the samples
 * other (currently ignored) chunks are wavl = waveform data, fact, cues of some sort, slnt = silence,
 *     plst = playlist, adtl = associated data list, labl = cue label, note = cue comments,
 *     ltxt = text associated with data segment (cue), file, DISP = displayable object,
 *     JUNK = EBU placeholder -- see below, PAD = padding, etc
 * fact chunk generally has number of samples (used in compressed files)
 * bext chunk has comments -- perhaps add these to the info/list list?  I need an example! -- is the chunk id "bext"?
 */

static mus_sample_t wave_to_sndlib_format(int osf, int bps, bool little)
{
  switch (osf)
    {
    case 1:
      switch (bps)
	{
	case 8: return(MUS_UBYTE); break;
	case 16: if (little) return(MUS_LSHORT); else return(MUS_BSHORT); break;
	case 32: if (little) return(MUS_LINT); else return(MUS_BINT); break;
	case 24: if (little) return(MUS_L24INT); else return(MUS_B24INT); break;
	default: return(MUS_UBYTE); break;
	}
      break;
    case 3: 
      if (little) 
	{
	  if (bps == 64)
	    return(MUS_LDOUBLE);
	  else return(MUS_LFLOAT); 
	  }
      else 
	{
	  if (bps == 64)
	    return(MUS_BDOUBLE);
	  else return(MUS_BFLOAT); 
	  }
      break;
    case 6: if (bps == 8) return(MUS_ALAW); break;
    case 7: if (bps == 8) return(MUS_MULAW); break;
      /* IBM mulaw follows G711 specs like other versions (this info direct from IBM) */
    case 0x101: return(MUS_MULAW); break;
    case 0x102: return(MUS_ALAW); break;
    }
  return(MUS_UNKNOWN_SAMPLE);
}


static void read_riff_fmt_chunk(uint8_t *hbuf, bool little)
{
  /* fmt chunk (also used in RF64 below)
   *
   * 8:  short format code        --1 = PCM for example
   * 10: short chans              --1
   * 12: long rate                --48000 (0xbb80)
   * 16: long ave rate            --65655 (0x10077)
   * 20: short align              --2
   * 22: short data size (bits)   --16
   * 24: bytes of extra
   * ... some extra data dependent on format
   *
   *  R I  F F  # #  # #  W A  V E  f m  t sp
   *  5249 4646 f851 0500 5741 5645 666d 7420
   *  e40f 0000 0100 0100 80bb 0000 0077 0100
   *  0200 1000 0000 0000 0000 0000 0000 0000
   *  
   *  #x000551f8 = 348664 = size in bytes - 8
   *  #x00000fe4 = 4068 [fmt_ chunk size?]
   */
  original_sample_type = big_or_little_endian_short((uint8_t *)(hbuf + 8), little);
  chans = big_or_little_endian_short((uint8_t *)(hbuf + 10), little);
  srate = big_or_little_endian_int((uint8_t *)(hbuf + 12), little);
  block_align = big_or_little_endian_short((uint8_t *)(hbuf + 20), little);
  bits_per_sample = big_or_little_endian_short((uint8_t *)(hbuf + 22), little);
  if (original_sample_type == -2)        /* 0xFFFE = "extensible" : short size=22, short bits, long chanmap, short format */
    original_sample_type = big_or_little_endian_short((uint8_t *)(hbuf + 24 + 8), little);
  sample_type = wave_to_sndlib_format(original_sample_type, bits_per_sample, little);
}


static int write_riff_fmt_chunk(int fd, uint8_t *hbuf, mus_sample_t samp_type, int wsrate, int wchans)
{
  int err = MUS_NO_ERROR;
  write_four_chars((uint8_t *)hbuf, I_fmt_);
  mus_lint_to_char((uint8_t *)(hbuf + 4), 16);
  switch (samp_type)
    {
    case MUS_MULAW: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 7); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 8); 
      break;

    case MUS_ALAW: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 6); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 8); 
      break;

    case MUS_UBYTE: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 1); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 8); 
      break;

    case MUS_LSHORT: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 1); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 16); 
      break;

    case MUS_L24INT: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 1); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 24); 
      break;

    case MUS_LINT: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 1); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 32); 
      break;

    case MUS_LFLOAT: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 3); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 32); 
      break;

    case MUS_LDOUBLE: 
      mus_lshort_to_char((uint8_t *)(hbuf + 8), 3); 
      mus_lshort_to_char((uint8_t *)(hbuf + 22), 64); 
      break;

    default: 
      /* don't call mus_error directly -- we need to close the file first in mus_header_write */
      err = MUS_UNSUPPORTED_SAMPLE_TYPE;
      break;
    }
  mus_lshort_to_char((uint8_t *)(hbuf + 10), (short)wchans);
  mus_lint_to_char((uint8_t *)(hbuf + 12), wsrate);
  mus_lint_to_char((uint8_t *)(hbuf + 16), wsrate * wchans * mus_bytes_per_sample(samp_type)); /* added chans 10-Mar-99 */
  mus_lshort_to_char((uint8_t *)(hbuf + 20), (short)(wchans * mus_bytes_per_sample(samp_type)));
  /* 22 short written above = bits/sample */
  header_write(fd, hbuf, 24);
  return(err);
}


static const uint8_t I_JUNK[4] = {'J','U','N','K'};

static int read_riff_header(const char *filename, int fd)
{
  /* we know we have checked for RIFF xxxx WAVE when we arrive here */
  int chunkloc = 12, i;
  bool little = true, got_fmt = false;
  mus_long_t offset = 0;

  if (match_four_chars((uint8_t *)hdrbuf, I_RIFX)) little = false; /* big-endian data in this case, but I've never seen one */
  little_endian = little;
  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 8));
  sample_type = MUS_UNKNOWN_SAMPLE;
  srate = 0;
  chans = 0;
  fact_samples = 0;
  bits_per_sample = 0;
  for (i = 0; i < AUX_COMMENTS; i++) aux_comment_start[i] = 0;
  true_file_length = SEEK_FILE_LENGTH(fd);
  update_form_size = big_or_little_endian_int((uint8_t *)(hdrbuf + 4), little);

  while (true)
    {
      uint32_t chunksize;
      offset += chunkloc;
      if (offset >= true_file_length) break;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 64) <= 0) break;
      chunksize = big_or_little_endian_uint((uint8_t *)(hdrbuf + 4), little);
      if ((chunksize == 0) && /* can be empty data chunk */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (match_four_chars((uint8_t *)hdrbuf, I_fmt_))
	{
	  got_fmt = true;
	  update_framples_location = 12 + offset;
	  read_riff_fmt_chunk(hdrbuf, little);
	}
      else
	{
	  if ((match_four_chars((uint8_t *)hdrbuf, I_data)) && (data_location == 0))
	    {
	      update_ssnd_location = offset + 4;
	      data_location = offset + 8;
	      data_size = big_or_little_endian_uint((uint8_t *)(hdrbuf + 4), little); /* was int 27-Jul-01 */
	      if (chunksize == 0) break; /* see aiff comment */
	    }
	  else
	    {
	      if (match_four_chars((uint8_t *)hdrbuf, I_fact))
		{
		  fact_samples = big_or_little_endian_int((uint8_t *)(hdrbuf + 8), little);
		}
	      else
		{
		  static const uint8_t I_inst[4] = {'i','n','s','t'};  /* RIFF wants lower case, just to be different */
		  if (match_four_chars((uint8_t *)hdrbuf, I_inst))
		    {
		      base_note = hdrbuf[8];
		      base_detune = hdrbuf[9];
		      /* rest is gain low-note high-note low-velocity high-velocity */
		    }
		  else
		    {
		      if (match_four_chars((uint8_t *)hdrbuf, I_clm_))
			{
			  comment_start = offset + 8;
			  comment_end = comment_start + chunksize - 1; /* end of comment not start of next chunk */
			}
		      else
			{
			  if ((match_four_chars((uint8_t *)hdrbuf, I_LIST)) &&
			      (match_four_chars((uint8_t *)(hdrbuf + 8), I_INFO)))
			    {
			      aux_comment_start[0] = offset + 8;
			      aux_comment_end[0] = offset + 8 + chunksize - 1;
			    }
			  else
			    {
			      if (match_four_chars((uint8_t *)hdrbuf, I_JUNK))
				update_rf64_location = offset;
			    }
			}
		    }
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
    }
  if (!got_fmt)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no fmt chunk?", filename));
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no data chunk?", filename));
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}


static void write_riff_clm_comment(int fd, const char *comment, int len, int extra)
{
  int i = 0, j;
  write_four_chars((uint8_t *)hdrbuf, I_clm_);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), len + extra);
  i = 8;
  for (j = 0; j < len; j++)
    {
      if (i == HDRBUFSIZ)
	{
	  header_write(fd, hdrbuf, HDRBUFSIZ);
	  i = 0;
	}
      hdrbuf[i++] = comment[j];
    }
  if (extra != 0)
    {
      if ((i + extra) > HDRBUFSIZ)
	{
	  header_write(fd, hdrbuf, i);
	  i = 0;
	}
      for (j = 0; j < extra; j++)
	hdrbuf[i++] = 0;
    }
  if (i > 0)
    {
      header_write(fd, hdrbuf, i);
    }
}


static void mus_ulint_to_char(uint8_t *j, uint32_t x)
{
  uint8_t *ox = (uint8_t *)&x;
#if (!MUS_LITTLE_ENDIAN)
  j[0] = ox[3]; j[1] = ox[2]; j[2] = ox[1]; j[3] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 4);
#endif
}


static int write_riff_header(int fd, int wsrate, int wchans, mus_long_t siz, mus_sample_t samp_type, const char *comment, int len)
{
  int j, extra = 0, err = MUS_NO_ERROR;

  data_location = 36 + 36 + 8;
  if (len != 0) 
    {
      if ((len % 4) != 0)
	extra = (4 - (len % 4));
      data_location += (8 + len + extra); 
    }
  /* 36 = "RIFF" + size(4) + "WAVE" + "fmt " + size(4) + 16 for data */
  /*   second 36 is for "JUNK" chunk, 8 is data chunk header */

  write_four_chars((uint8_t *)hdrbuf, I_RIFF);
  mus_ulint_to_char((uint8_t *)(hdrbuf + 4), (uint32_t)(data_location + siz - 8)); /* added -8 25-June-07 */
  write_four_chars((uint8_t *)(hdrbuf + 8), I_WAVE);
  header_write(fd, hdrbuf, 12);

  /* add JUNK chunk for possible change to RF64 during write */
  write_four_chars((uint8_t *)hdrbuf, I_JUNK);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), 28);
  for (j = 8; j < 36; j++) hdrbuf[j] = 0;
  header_write(fd, hdrbuf, 36);

  /* fmt chunk */
  err = write_riff_fmt_chunk(fd, hdrbuf, samp_type, wsrate, wchans);

  /* include possible clm (comment) chunk */
  if (len > 0)
    write_riff_clm_comment(fd, comment, len, extra);

  /* start the data chunk */
  write_four_chars((uint8_t *)hdrbuf, I_data);
  mus_ulint_to_char((uint8_t *)(hdrbuf + 4), (uint32_t)siz);
  header_write(fd, hdrbuf, 8);
  return(err);
}


char *mus_header_riff_aux_comment(const char *name, mus_long_t *starts, mus_long_t *ends)
{
  char *sc = NULL, *auxcom;
  if ((starts) && (starts[0] != 0))
    {
      int j, fd, k, m;
      mus_long_t i, end;
      /* found a LIST+INFO chunk (and no other comment) */
      fd = mus_file_open_read(name);
      if (fd == -1) return(NULL);
      i = starts[0];
      end = ends[0];
      sc = (char *)calloc(end - i + 2, sizeof(char));
      j = 0;
      k = 4;
      lseek(fd, i, SEEK_SET);
      auxcom = (char *)calloc(end - i + 2, sizeof(char));
      header_read(fd, (uint8_t *)auxcom, end - i + 1);
      CLOSE(fd, name);
      i += 4;
      while (i < end)
	{
	  int len;
	  for (m = 0; m < 4; m++) sc[j++] = auxcom[k++];
	  len = mus_char_to_lint((uint8_t *)(auxcom + k));
	  if ((len <= 0) || (len > end)) break;
	  sc[j++] = ':';
	  sc[j++] = ' ';
	  k += 4;
	  for (m = 0; m < len; m++)
	    if (auxcom[k] != 0) 
	      sc[j++] = auxcom[k++]; 
	    else k++;
	  sc[j++] ='\n';
	  if (len & 1) 
	    {
	      len++; 
	      k++;
	    }
	  i += (len + 8);
	}
      free(auxcom);
    }
  return(sc);
}



/* ------------------------------------ W64 ------------------------------------
 *
 * soundforge -- just a quick hack until I get better documentation 
 */

static int read_soundforge_header(const char *filename, int fd)
{
  /* like RIFF but lowercase and 64-bit vals */
  int i, off;
  mus_long_t offset, chunkloc;
  chunkloc = 12 * 2 + 16;
  offset = 0;
  sample_type = MUS_UNKNOWN_SAMPLE;
  srate = 0;
  chans = 0;
  fact_samples = 0;
  bits_per_sample = 0;
  for (i = 0; i < AUX_COMMENTS; i++) aux_comment_start[i] = 0;
  true_file_length = SEEK_FILE_LENGTH(fd);
  update_form_size = mus_char_to_llong((uint8_t *)(hdrbuf + 4 * 2));
  while (true)
    {
      int chunksize;
      offset += chunkloc;
      if (offset >= true_file_length) break;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 64) <= 0) break;
      chunksize = mus_char_to_llong((uint8_t *)(hdrbuf + 16));
      if ((chunksize == 0) && /* can be empty data chunk? */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < 0)
	break;

      if (match_four_chars((uint8_t *)hdrbuf, I_fmt_))
	{
	  off = 16;
	  original_sample_type = mus_char_to_lshort((uint8_t *)(hdrbuf + 8 + off));
	  chans = mus_char_to_lshort((uint8_t *)(hdrbuf + 10 + off));
	  srate = mus_char_to_lint((uint8_t *)(hdrbuf + 12 + off));
	  block_align = mus_char_to_lshort((uint8_t *)(hdrbuf + 20 + off));
	  bits_per_sample = mus_char_to_lshort((uint8_t *)(hdrbuf + 22 + off));
	  sample_type = wave_to_sndlib_format(original_sample_type, bits_per_sample, true);
	}
      else
	{
	  if ((match_four_chars((uint8_t *)hdrbuf, I_data)) && (data_location == 0))
	    {
	      data_location = offset + 16 + 8;
	      data_size = mus_char_to_llong((uint8_t *)(hdrbuf + 16));
	      if (chunksize == 0) break; /* see aiff comment */
	    }
	  else
	    {
	      if (match_four_chars((uint8_t *)hdrbuf, I_fact))
		{
		  fact_samples = mus_char_to_llong((uint8_t *)(hdrbuf + 8));
		}
	    }
	}
      chunkloc = chunksize % 8;
      if (chunkloc == 0)
	chunkloc = chunksize;
      else chunkloc = chunksize + 8 - chunkloc;
      /* I think they are rounding up to a multiple of 8 here (sigh) */
    }

  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no data chunk?", filename));

  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);

  return(MUS_NO_ERROR);
}



/* ------------------------------------ RF64 ------------------------------------
 *
 * see http://www.ebu.ch/CMSimages/en/tec_doc_t3306_tcm6-42570.pdf.
 *
 * RF64 0xFFFFFFFF WAVE
 *   ds64 size(32) RIFF-size(64) data-size(64) fact-samples(64) table-len table
 *   fmt_ size format-data
 *   data 0xFFFFFFFF data...
 *
 * if RIFF WAVE is being written with possibility of overflow size, use
 * RIFF size WAVE JUNK then the ds64 chunk as above
 *   if size overflows, reset RIFF->RF64, JUNK->ds64, size->-1 twice etc
 *
 * JUNK size = 28
 * all ints are little endian
 * 
 */

static int read_rf64_header(const char *filename, int fd)
{
  /* we've checked RF64 xxxx WAVE before getting here */
  /* this is very similar (identical) to RIFF for the most part, but I decided it was cleanest to copy the code */

  mus_long_t chunkloc;
  bool got_fmt = false, got_ds64 = false;
  mus_long_t offset;
  little_endian = true;
  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 8));
  chunkloc = 12;
  offset = 0;
  sample_type = MUS_UNKNOWN_SAMPLE;
  srate = 0;
  chans = 0;
  fact_samples = 0;
  bits_per_sample = 0;
  {
    int i;
    for (i = 0; i < AUX_COMMENTS; i++) aux_comment_start[i] = 0;
  }
  true_file_length = SEEK_FILE_LENGTH(fd);
  update_form_size = -1;                                                          /* at hdrbuf+4 -> 0xFFFFFFFF */

  while (true)
    {
      mus_long_t chunksize;
      offset += chunkloc;
      if (offset >= true_file_length) break;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 64) <= 0) break;
      chunksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) &&                                                      /* can be empty data chunk */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < -1)
	break;

      if (match_four_chars((uint8_t *)hdrbuf, I_ds64))
	{
	  /* RIFF form size | data size | fact samples */
	  update_form_size = mus_char_to_llong((uint8_t *)(hdrbuf + 8));
	  data_size = mus_char_to_llong((uint8_t *)(hdrbuf + 16));
	  fact_samples = (int)mus_char_to_llong((uint8_t *)(hdrbuf + 24));
	  update_rf64_location = offset + 8;
	  got_ds64 = true;
	  /* ignore "table" for now */
	}
      else
	{
	  if (match_four_chars((uint8_t *)hdrbuf, I_fmt_))
	    {
	      got_fmt = true;
	      update_framples_location = 12 + offset;
	      read_riff_fmt_chunk(hdrbuf, true);
	    }
	  else
	    {
	      if ((match_four_chars((uint8_t *)hdrbuf, I_data)) && (data_location == 0))
		{
		  update_ssnd_location = offset + 4;
		  data_location = offset + 8;
		  /* chunksize (at hdrbuf + 4) here is -1 */
		  chunksize = data_size;
		  if (chunksize <= 0) break; /* argh -- we need the ds64 chunk before this chunk */
		}
	      else
		{
		  /* ignore possible fact, inst chunks */
		  if (match_four_chars((uint8_t *)hdrbuf, I_clm_))
		    {
		      comment_start = offset + 8;
		      comment_end = comment_start + chunksize - 1; /* end of comment not start of next chunk */
		    }
		  else
		    {
		      if ((match_four_chars((uint8_t *)hdrbuf, I_LIST)) &&
			  (match_four_chars((uint8_t *)(hdrbuf + 8), I_INFO)))
			{
			  aux_comment_start[0] = offset + 8;
			  aux_comment_end[0] = offset + 8 + chunksize - 1;
			}
		    }
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
    }

  if (!got_fmt)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no fmt chunk?", filename));
  if (!got_ds64)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no ds64 chunk?", filename));

  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no data chunk?", filename));

  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);

  return(MUS_NO_ERROR);
}


static int write_rf64_header(int fd, int wsrate, int wchans, mus_long_t size, mus_sample_t samp_type, const char *comment, int len)
{
  int extra = 0, err = MUS_NO_ERROR;
  data_location = 36 + 36 + 8;
  if (len != 0) 
    {
      if ((len % 4) != 0)
	extra = (4 - (len % 4));
      data_location += (8 + len + extra); 
    }

  write_four_chars((uint8_t *)hdrbuf, I_RF64);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), -1);
  write_four_chars((uint8_t *)(hdrbuf + 8), I_WAVE);
  header_write(fd, hdrbuf, 12);

  /* ds64 chunk */
  write_four_chars((uint8_t *)hdrbuf, I_ds64);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), 28);
  mus_llong_to_char((uint8_t *)(hdrbuf + 8), data_location + size - 8); /* -8 added 25-June-07 */
  mus_llong_to_char((uint8_t *)(hdrbuf + 16), size);
  mus_llong_to_char((uint8_t *)(hdrbuf + 24), size);
  mus_lint_to_char((uint8_t *)(hdrbuf + 32), 0); /* "table size" */
  header_write(fd, hdrbuf, 36);

  err = write_riff_fmt_chunk(fd, hdrbuf, samp_type, wsrate, wchans);

  if (len > 0)
    write_riff_clm_comment(fd, comment, len, extra);

  write_four_chars((uint8_t *)hdrbuf, I_data);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), -1);
  header_write(fd, hdrbuf, 8);
  return(err);
}


static int mus_header_convert_riff_to_rf64(const char *filename, mus_long_t size)
{
  int err, fd;

  update_rf64_location = -1;
  update_ssnd_location = 0;
  /* mus_header_change_type copies the entire file, which is probably a bad idea in this case */

  err = mus_header_read(filename);
  if (err != MUS_NO_ERROR) return(err);

  if ((update_ssnd_location == 0) ||
      (update_rf64_location <= 0))  /* "JUNK" chunk has to be there already for this to work */
    return(MUS_CANT_CONVERT);

  fd = mus_file_reopen_write(filename);
  if (fd == -1) return(false);

  /* change overall type to rf64 and set size to RIFF -1 */
  write_four_chars((uint8_t *)hdrbuf, I_RF64);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), -1);
  header_write(fd, hdrbuf, 8);

  /* set data chunk's size to -1 */
  lseek(fd, update_ssnd_location, SEEK_SET);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), -1);
  header_write(fd, hdrbuf, 4);

  /* convert the "JUNK" chunk to be a "ds64" chunk */
  lseek(fd, update_rf64_location, SEEK_SET);
  write_four_chars((uint8_t *)hdrbuf, I_ds64);
  mus_lint_to_char((uint8_t *)(hdrbuf + 4), 28);
  mus_llong_to_char((uint8_t *)(hdrbuf + 8), data_location + size - 8);
  mus_llong_to_char((uint8_t *)(hdrbuf + 16), size);
  mus_llong_to_char((uint8_t *)(hdrbuf + 24), size);
  header_write(fd, hdrbuf, 36);

  CLOSE(fd, filename);
  return(true);
}


/* ------------------------------------ AVI ------------------------------------
 * actually a video format, but it sometimes contains embedded 'wave' data
 *
 * RIFF xxx AVI 
 *   <various LISTs>
 *   LIST xxxx hdr1 LIST strl(?) strh | strf | strn etc
 *     strf is the WAVE header starting with the sound format
 *   LIST xxxx movi ##db|##wb -- wb subblocks have the audio data (these need to be collected as a single stream)
 * there are many complications that we make no effort to handle here
 *
 * described in http://www.rahul.net/jfm/avi.html
 */

static int read_avi_header(const char *filename, int fd)
{
  static const uint8_t I_strf[4] = {'s','t','r','f'};  
  static const uint8_t I_movi[4] = {'m','o','v','i'};  

  /* we know we have checked for RIFF xxxx AVI  when we arrive here */
  int chunkloc, cksize, bits;
  bool happy = true;
  mus_long_t ckoff, cktotal, offset;
  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 8));
  chunkloc = 12;
  offset = 0;
  sample_type = MUS_UNKNOWN_SAMPLE;
  srate = 0;
  chans = 1;
  true_file_length = SEEK_FILE_LENGTH(fd);
  while (happy)
    {
      int chunksize;
      offset += chunkloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s avi header: chunks confused at %" print_mus_long, filename, offset));
      chunksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) && /* can be empty data chunk? */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < 0)
	break;
      if (match_four_chars((uint8_t *)hdrbuf, I_LIST))
	{
	  ckoff = offset + 12;
	  cktotal = 12;
	  if (match_four_chars((uint8_t *)(hdrbuf + 8), I_movi))
	    {
	      while (cktotal < chunksize)
		{
		  lseek(fd, ckoff, SEEK_SET);
		  header_read(fd, hdrbuf, 8);
		  cksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
		  if ((hdrbuf[2] == 'w') && (hdrbuf[3] == 'b'))
		    {
		      data_location = ckoff;
		      if (srate != 0) happy = false;
		      break;
		    }
		  ckoff += (8 + cksize);
		  cktotal += (8 + cksize);
		}
	    }
	  else
	    {
	      while (cktotal < chunksize)
		{
		  lseek(fd, ckoff, SEEK_SET);
		  header_read(fd, hdrbuf, 8);
		  cksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
		  ckoff += (8 + cksize);
		  cktotal += (8 + cksize);
		  if (match_four_chars((uint8_t *)hdrbuf, I_LIST))
		    {
		      mus_long_t ckoffr, cktotalr, rdsize;
		      ckoffr = ckoff + 12;
		      cktotalr = 12;
		      while (cktotalr < cksize)
			{
			  mus_long_t cksizer;
			  lseek(fd, ckoffr, SEEK_SET);
			  header_read(fd, hdrbuf, 8);
			  cksizer = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
			  ckoffr += (8 + cksizer);
			  cktotalr += (8 + cksizer);
			  if (match_four_chars((uint8_t *)hdrbuf, I_strf))
			    {
			      if (cksizer < HDRBUFSIZ) 
				rdsize = cksizer; 
			      else rdsize = HDRBUFSIZ;
			      header_read(fd, hdrbuf, rdsize);
			      original_sample_type = mus_char_to_lshort((uint8_t *)hdrbuf);
			      chans = mus_char_to_lshort((uint8_t *)(hdrbuf + 2));
			      srate = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
			      /* block_align = mus_char_to_lshort((uint8_t *)(hdrbuf + 12)); */
			      bits = mus_char_to_lshort((uint8_t *)(hdrbuf + 14));
			      /* only 16 bit linear little endian for now */
			      if ((bits == 16) && (original_sample_type == 1))
				sample_type = MUS_LSHORT;
			      if (data_location != 0) happy = false;
			      break;
			    }
			}
		    }
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
    }
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no movi chunk?", filename));
  if (data_location > true_file_length) 
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ SoundFont 2.0 ------------------------------------
 *
 * Emu's SoundFont(tm) format uses RIFF -- at ftp.creaf.com:/pub/emu/sf2_00a.ps)
 *
 * RIFF xxxx sfbk followed by
 *   LIST xxxx INFO chunk (nothing of interest -- icmt subchunk might have comments)
 *   LIST xxxx sdta chunk = data
 *     smpl chunk (16 bit linear little-endian)
 *   LIST xxxx pdta list chunk 
 *     shdr subchunk has srate at 40 (int), samples at 28
 *
 * http://smurf.sourceforge.net/sfont_intro.php
 * http://www.hammersound.net/
 */

static int soundfont_entries = 0;
static int *soundfont_starts = NULL, *soundfont_ends = NULL, *soundfont_loop_starts = NULL, *soundfont_loop_ends = NULL;
static int soundfont_size = 0;
static char **soundfont_names = NULL;

static void soundfont_entry(const char *name, int start, int end, int loop_start, int loop_end)
{
  if (soundfont_entries == soundfont_size)
    {
      int i;
      if (soundfont_size == 0)
	{
	  soundfont_size = 8;
	  soundfont_starts = (int *)calloc(soundfont_size, sizeof(int));
	  soundfont_ends = (int *)calloc(soundfont_size, sizeof(int));
	  soundfont_loop_starts = (int *)calloc(soundfont_size, sizeof(int));
	  soundfont_loop_ends = (int *)calloc(soundfont_size, sizeof(int));
	  soundfont_names = (char **)calloc(soundfont_size, sizeof(char *));
	}
      else
	{
	  if (soundfont_size < 123123123)
	    soundfont_size += 8;
	  /* believe it or not, without the 123123123 shuffle, gcc complains at mus_header_read_1 [line 5519!]
	   * that we are making a naughty assumption about overflows.
	   */
	  soundfont_starts = (int *)realloc(soundfont_starts, soundfont_size * sizeof(int));
	  soundfont_ends = (int *)realloc(soundfont_ends, soundfont_size * sizeof(int));
	  soundfont_loop_starts = (int *)realloc(soundfont_loop_starts, soundfont_size * sizeof(int));
	  soundfont_loop_ends = (int *)realloc(soundfont_loop_ends, soundfont_size * sizeof(int));
	  soundfont_names = (char **)realloc(soundfont_names, soundfont_size * sizeof(char *));
	}
      for (i = soundfont_entries; i < soundfont_size; i++) soundfont_names[i] = NULL;
    }
  if (!soundfont_names[soundfont_entries]) soundfont_names[soundfont_entries] = (char *)calloc(20, sizeof(char));
  strcpy(soundfont_names[soundfont_entries], name);
  soundfont_starts[soundfont_entries] = start;
  soundfont_ends[soundfont_entries] = end;
  soundfont_loop_starts[soundfont_entries] = loop_start;
  soundfont_loop_ends[soundfont_entries] = loop_end;
  soundfont_entries++;
}


int mus_header_sf2_entries(void) {return(soundfont_entries);}
char *mus_header_sf2_name(int n) {return(soundfont_names[n]);}
int mus_header_sf2_start(int n) {return(soundfont_starts[n]);}
int mus_header_sf2_end(int n) {return(soundfont_ends[n]);}
int mus_header_sf2_loop_start(int n) {return(soundfont_loop_starts[n]);}
int mus_header_sf2_loop_end(int n) {return(soundfont_loop_ends[n]);}


static int read_soundfont_header(const char *filename, int fd)
{
  static const uint8_t I_sdta[4] = {'s','d','t','a'};
  static const uint8_t I_shdr[4] = {'s','h','d','r'};
  static const uint8_t I_pdta[4] = {'p','d','t','a'};

  /* we know we have checked for RIFF xxxx sfbk when we arrive here */
  int chunkloc, type, cksize, i, this_end, last_end;
  mus_long_t ckoff, offset;
  bool happy = true;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 8));
  chunkloc = 12;
  offset = 0;
  soundfont_entries = 0;
  sample_type = MUS_LSHORT;
  srate = 0;
  chans = 1; 
  last_end = 0;
  true_file_length = SEEK_FILE_LENGTH(fd);

  while (happy)
    {
      int chunksize;
      offset += chunkloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s soundfont header: chunks confused at %" print_mus_long, filename, offset));
      chunksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) && /* can be empty data chunk? */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < 0)
	break;
      if (match_four_chars((uint8_t *)hdrbuf, I_LIST))
	{
	  /* everything is squirreled away in LIST chunks in this format */
	  if (match_four_chars((uint8_t *)(hdrbuf + 8), I_pdta))
	    {
	      /* go searching for I_shdr -- headers this complicated should be illegal. */
	      ckoff = offset + 12;
	      lseek(fd, ckoff, SEEK_SET);
	      while (srate == 0)
		{
		  int64_t bytes;
		  bytes = (int64_t)read(fd, hdrbuf, 8);
		  if (bytes == 0)
		    {
		      happy = false;
		      break;
		    }
		  i = 0;
		  cksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
		  ckoff += (8 + cksize);
		  /* here we need to jump over subchunks! -- 4-Aug-97 */
		  if (match_four_chars((uint8_t *)hdrbuf, I_shdr))
		    {
		      /* each sound: 
		       *  0: name
		       * 20: start (in samples from start of bank)
		       * 24: end
		       * 28: loop start (also relative to start of bank)
		       * 32: loop end
		       * 36: sample rate
		       * 40: pitch (60 = middle C)
		       * 41: detune (cents)
		       * 42: link (to other channel if any?)
		       * 44: type (1 = mono, 2 = mono right, 4 = mono left, others (0x8000) apparently for ROM presets?)
		       */
		      while (i < cksize)
			{
			  header_read(fd, hdrbuf, 46);
			  i += 46;
			  type = mus_char_to_lshort((uint8_t *)(hdrbuf + 44));
			  if ((type == 1) &&
			      (mus_char_to_lint((uint8_t *)(hdrbuf + 24)) > 0)) 
			    {
			      if (srate == 0) 
				srate = mus_char_to_lint((uint8_t *)(hdrbuf + 36));
			      soundfont_entry((char *)(hdrbuf),
					      mus_char_to_lint((uint8_t *)(hdrbuf + 20)),
					      this_end = mus_char_to_lint((uint8_t *)(hdrbuf + 24)),
					      mus_char_to_lint((uint8_t *)(hdrbuf + 28)),
					      mus_char_to_lint((uint8_t *)(hdrbuf + 32)));
			      if (this_end > last_end) last_end = this_end;
			    }
			}
		      happy = (data_location == 0);
		    }
		  else 
		    {
		      if (ckoff >= offset + 8 + chunksize) 
			break;
		      lseek(fd, ckoff, SEEK_SET);
		    }
		}
	    }
	  else
	    {
	      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_sdta))
		{
		  /* assume smpl follows + subchunk size */
		  /* Convert 1.4 appears to create a separate smpl chunk */
		  data_location = offset + 20; /* LIST xxxx sdta smpl xxxx ... */
		  happy = (srate == 0);
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
    }
  if (srate == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: srate == 0", filename));
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no sdta chunk?", filename));
  if (last_end > 0)
    data_size = last_end; /* samples already */
  else data_size = (true_file_length - data_location) / 2;
  return(MUS_NO_ERROR);
}




/* ------------------------------------ NIST ------------------------------------ 
 *
 * code available in ogitools-v1.0.tar.gz at svr-ftp.eng.cam.ac.uk:comp.speech/sources
 * 
 *   0: "NIST_1A"
 *   8: data_location as ASCII representation of integer (apparently always "   1024")
 *  16: start of complicated header -- see below for details
 *
 *  The most recent version of the SPHERE package is available
 *  via anonymous ftp from jaguar.ncsl.nist.gov [129.6.48.157] in the pub directory
 *  in compressed tar form as "sphere-v.tar.Z" (where "v" is the version
 *  code 2.6a last I looked).  shortpack is also at this site.
 *
 *  here's an example:
 *
 *  NIST_1A
 *     1024
 *  database_id -s5 TIMIT
 *  database_version -s3 1.0
 *  utterance_id -s8 aks0_sa1
 *  channel_count -i 1
 *  sample_count -i 63488
 *  sample_rate -i 16000
 *  sample_min -i -6967
 *  sample_max -i 7710
 *  sample_n_bytes -i 2
 *  sample_byte_format -s2 01
 *  sample_sig_bits -i 16
 *  end_head
 *
 * the sample_byte_format can be "10"=big-endian or "01"=little-endian, or "shortpack-v0"=compressed via shortpack
 * other formats are wavpack and shorten.
 *
 * another field is 'sample_coding' which can be pcm (i.e. linear), 'pcm, embedded-shorten-v1.09', mu-law, alaw, ulaw, pculaw etc --
 *   so unpredictable as to be totally useless. This means we sometimes try to decode shorten-encoded files because
 *   we ignore this field.  And worse, there's a 'channels_interleaved' field that (apparently) can be false.  Tough.
 */

#define MAX_FIELD_LENGTH 80

static int decode_nist_value(char *str, int base, int end)
{
  /* can be -i -r or -snnn where nnn = ascii rep of integer = len of string (!) */
  /* we'll deal only with integer fields (and well-behaved string fields) */
  int i, j;
  char value[MAX_FIELD_LENGTH];
  memset((void *)value, 0, MAX_FIELD_LENGTH);
  i = base;
  while ((i < end) && (i < MAX_FIELD_LENGTH) && (str[i] != '-')) i++; /* look for -i or whatever */
  while ((i < end) && (i < MAX_FIELD_LENGTH) && (str[i] != ' ')) i++; /* look for space after it */
  i++;
  if (i >= MAX_FIELD_LENGTH) return(0);
  for (j = 0; i < end; j++, i++)
    value[j] = str[i];
  value[j] = 0;
  if (value[0] =='s') return(MUS_NIST_SHORTPACK);
  sscanf(value, "%12d", &i);
  /* what is the correct way to use mus_long_ts here for the sample count? */
  return(i);
}


static int read_nist_header(const char *filename, int fd)
{
  char str[MAX_FIELD_LENGTH], name[MAX_FIELD_LENGTH];
  bool happy = true;
  mus_long_t curbase;
  int k, hend, j, n, nm, samples, bytes, byte_format, idata_location = 0;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf); /* the actual id is "NIST_1A" */
  memset((void *)str, 0, MAX_FIELD_LENGTH);
  memset((void *)name, 0, MAX_FIELD_LENGTH);

  for (k = 8; k < 16; k++) 
    str[k - 8] = hdrbuf[k];

  sscanf(str, "%12d", &idata_location);       /* always "1024" */
  if (idata_location != 1024)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s NIST data location: %d?", filename, idata_location));

  data_location = 1024;
  n = 16;
  hend = INITIAL_READ_SIZE;
  k = 0;
  curbase = 0;
  samples = 0;
  bytes = 0;
  srate = 0;
  chans = 0;
  comment_start = 16;
  comment_end = 16;
  byte_format = 10;
  for (j = 0; j < MAX_FIELD_LENGTH; j++) 
    str[j] =' ';  

  while (happy) 
    {
      /* much as in xIFF files, march through the file looking for the data we're after */
      /* in this case we munch a character at a time... */
      str[k] = hdrbuf[n];
      if ((((str[k] == '\0') || (str[k] == '\n')) || ((curbase + n + 1) >= data_location)) || (k == 79))
	{
	  /* got a complete record (assuming no embedded newlines, of course) */
	  /* now look for a record we care about and decode it */
	  nm = 0;
	  while ((nm < MAX_FIELD_LENGTH) && (str[nm] != ' ') && (str[nm] != '\0') && (str[nm] != '\n'))
	    {
	      name[nm] = str[nm];
	      nm++;
	    }
	  if (nm >= MAX_FIELD_LENGTH) 
	    {
	      header_type = MUS_RAW; 
	      sample_type = MUS_UNKNOWN_SAMPLE; 
	      return(mus_error(MUS_UNSUPPORTED_HEADER_TYPE, "%s nist header: unreadable field (length = %d)?", filename, nm));
	    }
	  name[nm] = 0;
	  if (strcmp(name, "sample_rate") == 0) srate = decode_nist_value(str, nm, k); else
	    if (strcmp(name, "channel_count") == 0) chans = decode_nist_value(str, nm, k); else
	      if (strcmp(name, "end_head") == 0) {happy = false; comment_end = curbase + n - 9;} else
		if (strcmp(name, "sample_count") == 0) samples = decode_nist_value(str, nm, k); else
		  if ((bytes == 0) && (strcmp(name, "sample_n_bytes") == 0)) bytes = decode_nist_value(str, nm, k); else
		    if ((bytes == 0) && (strcmp(name, "sample_sig_bits") == 0)) {bytes = decode_nist_value(str, nm, k); bytes = (bytes >> 3);} else
		      if (strcmp(name, "sample_byte_format") == 0) byte_format = decode_nist_value(str, nm, k);
	  for (j = 0; j <= k; j++) str[j] =' ';
	  k = 0;
	  if ((curbase + n + 1) > 1024) happy = false;
	}
      else
	k++;
      n++;
      if (n >= hend)
	{
	  int64_t read_bytes;
	  curbase += hend;
	  n = 0;
	  read_bytes = (int64_t)read(fd, hdrbuf, HDRBUFSIZ);
	  if (read_bytes < HDRBUFSIZ)
	    return(mus_error(MUS_HEADER_READ_FAILED, "%s NIST header truncated?", filename));
	  hend = HDRBUFSIZ;
	}
    }

  data_size = samples * bytes;
  if (byte_format == MUS_NIST_SHORTPACK)
    {
      sample_type = MUS_UNKNOWN_SAMPLE;
      original_sample_type = MUS_NIST_SHORTPACK;
    }
  else
    {
      switch (bytes)
	{
	case 1: 
	  sample_type = MUS_MULAW; 
	  break;

	case 2:
	  if (byte_format == 10) 
	    sample_type = MUS_BSHORT;
	  else sample_type = MUS_LSHORT;
	  break;

	case 3:
	  if (byte_format == 10) 
	    sample_type = MUS_B24INT;
	  else sample_type = MUS_L24INT;
	  break;

	case 4:
	  if (byte_format == 10) 
	    sample_type = MUS_BINT;
	  else sample_type = MUS_LINT;
	  break;

	default: 
	  sample_type = MUS_BYTE; 
	  break;
	}
    }

  true_file_length = SEEK_FILE_LENGTH(fd);
  if ((data_size > true_file_length) && (original_sample_type != MUS_NIST_SHORTPACK))
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);

  return(MUS_NO_ERROR);
}


static int write_nist_header(int fd, int wsrate, int wchans, mus_long_t size, mus_sample_t samp_type)
{
  char *header;
  int datum;
  datum = mus_bytes_per_sample(samp_type);
  header = (char *)calloc(1024, sizeof(char));
  snprintf(header, 1024, "NIST_1A\n   1024\nchannel_count -i %d\nsample_rate -i %d\nsample_n_bytes -i %d\nsample_byte_format -s2 %s\nsample_sig_bits -i %d\nsample_count -i %" print_mus_long "\nend_head\n",
	  wchans, wsrate, datum,
	  ((samp_type == MUS_BSHORT) || (samp_type == MUS_B24INT) || (samp_type == MUS_BINT)) ? "10" : "01",
	  datum * 8, 
	  size / datum);
  header_write(fd, (uint8_t *)header, 1024);
  data_location = 1024;
  free(header);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ BICSF ------------------------------------ 
 * (actually, this is EBICSF and the old BICSF is called IRCAM below)
 *
 * 0-28: NeXT-compatible header, read by read_next_header above.
 *   28: bicsf magic number (107364 or trouble)
 *   32: srate as a 32-bit float
 *   36: chans
 *   40: sample type indicator (2 = 16-bit linear, 4 = 32-bit float)
 *   44: begin chunks, if any
 *
 * followed by AIFF-style chunked header info with chunks like:
 *
 *   COMM size comment
 *   MAXA size {max amps (up to 4)} (frample offsets) time-tag unix msec counter
 *   CUE, PRNT, ENV etc 
 *
 * except in Paul Lansky's "hybrid" headers, according to MixViews.
 */

static int read_bicsf_header(const char *filename, int fd)
{
  int chunksize, chunkname, offset, chunkloc;
  bool happy = true;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 28));
  header_type = MUS_BICSF;
  data_location = 1024;
  if (data_size == 0) data_size = (true_file_length - data_location);

  lseek(fd, 40, SEEK_SET);
  header_read(fd, hdrbuf, HDRBUFSIZ);
  original_sample_type = mus_char_to_bint((uint8_t *)hdrbuf);
  switch (original_sample_type) 
    {
    case 2: sample_type = MUS_BSHORT; break;
    case 4: sample_type = MUS_BFLOAT; break;
    case 8: sample_type = MUS_BDOUBLE; break;
    default: break;
    }

  /* now check for a COMM chunk, setting the comment pointers */
  chunkloc = 4; /* next header + magic number, srate, chans, packing, then chunks, I think */
  offset = 40;

  while (happy)
    {
      if (((offset + chunkloc) >= data_location) ||
	  ((offset + chunkloc) < 40))
	happy = false;
      else
	{
	  offset += chunkloc;
	  if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	    return(mus_error(MUS_HEADER_READ_FAILED, "%s bicsf header: chunks confused at %d", filename, offset));

	  chunkname = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
	  chunksize = mus_char_to_bint((uint8_t *)(hdrbuf + 4));

	  if (chunksize < 0)
	    break;
	  if (match_four_chars((uint8_t *)hdrbuf, I_COMM))
	    {
	      comment_start = 8 + offset;
	      comment_end = comment_start + chunksize -1;
	      happy = false;
	    }
	  else
	    {
	      if ((chunkname == 0) || (chunksize <= 0)) 
		happy = false;
	    }
	  chunkloc = (8 + chunksize);
	}
    }

  return(MUS_NO_ERROR);
  /* from here we fall back into read_next_header */
}



/* ------------------------------------ IRCAM ------------------------------------ 
 * old-style BICSF -- added write option for Sun port 12-Dec-94
 *
 *    0: 0x1a364 or variations thereof -- byte order gives big/little_endian decision,
 *         ^ digit gives machine info, according to AFsp sources -- see IRCAM ints above
 *    4: srate as a 32-bit float
 *    8: chans
 *   12: sample type indicator (2 = 16-bit linear, 4 = 32-bit float)
 *       according to new Sox (version 11), these packing modes are now bytes/sample in low short, code in high
 *       so 1 = char, 0x10001 = alaw, 0x20001 = mulaw, 2 = short, 3 = 24bit?, 0x40004 = long, 4 = float (AFsp sez 4 can also be double)
 *   16: comment start -- how to tell if it's a real comment?
 *       apparently these are separated as short code, short blocksize, then data
 *       codes: 0 = end, 1 = maxamp, 2 = comment, 3 = pvdata, 4 = audioencode and codemax??
 * 1024: data start
 * 
 * apparently the byte order depends on the machine.
 * and yet... convert 1.4 makes a .sf file with little endian header, the VAX id, and big endian data?
 *            Csound also uses the VAX magic number with little-endian unscaled floats!  Argh. 
 *            even worse, Paul Lansky plops some version of this at the end of a NeXT header!  Complete chaos...
 */

static int read_ircam_header(const char *filename, int fd)
{
  short bloc;
  int offset;
  bool little, happy = true;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
  if ((mus_char_to_lint((uint8_t *)hdrbuf) == I_IRCAM_VAX) || 
      (mus_char_to_lint((uint8_t *)hdrbuf) == I_IRCAM_MIPS))
    little = true;
  else little = false;
  little_endian = little;
  data_location = 1024;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - 1024);
  original_sample_type = big_or_little_endian_int((uint8_t *)(hdrbuf + 12), little);
  sample_type = MUS_UNKNOWN_SAMPLE;

  if (original_sample_type == 2) 
    {
      if (little) 
	sample_type = MUS_LSHORT; 
      else sample_type = MUS_BSHORT;
    }
  else if (original_sample_type == 4) 
    {
      if (little) 
	{
	  if (mus_char_to_lint((uint8_t *)hdrbuf) == I_IRCAM_VAX)
	    sample_type = MUS_LFLOAT_UNSCALED; /* Csound and MixViews */
	  else sample_type = MUS_LFLOAT;
	}
      else sample_type = MUS_BFLOAT;
    }
  else if (original_sample_type == 0x40004) 
    {
      if (little) sample_type = MUS_LINT;
      else sample_type = MUS_BINT;
    }
  else if (original_sample_type == 0x10001) sample_type = MUS_ALAW;
  else if (original_sample_type == 0x20001) sample_type = MUS_MULAW;
  else if (original_sample_type == 1) sample_type = MUS_BYTE;
  else if (original_sample_type == 3)
    {
      if (little) sample_type = MUS_L24INT;
      else sample_type = MUS_B24INT;
    }
  else if (original_sample_type == 8)
    {
      if (little) sample_type = MUS_LDOUBLE;
      else sample_type = MUS_BDOUBLE;
    }

  srate = (int)big_or_little_endian_float((uint8_t *)(hdrbuf + 4), little);
  chans = big_or_little_endian_int((uint8_t *)(hdrbuf + 8), little);
  bloc = 16;
  offset = 0;

  while (happy)
    {
      short bcode, bsize;
      offset += bloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s ircam header: chunks confused at %d", filename, offset));

      bcode = big_or_little_endian_short((uint8_t *)hdrbuf, little);
      bsize = big_or_little_endian_short((uint8_t *)(hdrbuf + 2), little);

      if (bcode == 2)
	{
	  happy = false;
	  comment_start = 4 + offset;
	  comment_end = comment_start + bsize - 1; /* was -5? */
	}
      bloc = bsize;
      if ((bsize <= 0) || (bcode <= 0) || ((offset + bloc) > 1023)) happy = false;
    }

  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}


static int sndlib_format_to_ircam(mus_sample_t samp_type)
{
  switch (samp_type)
    {
    case MUS_MULAW:  return(0x20001); break;
    case MUS_ALAW:   return(0x10001); break;
    case MUS_BSHORT: return(2);       break;
    case MUS_BINT:   return(0x40004); break;
    case MUS_BFLOAT: return(4);       break;
    default: 
      return(mus_error(MUS_UNSUPPORTED_SAMPLE_TYPE, "IRCAM header unsupported sample type: %d (%s)", samp_type, any_sample_type_name(samp_type)));
      break;
    }
}


static void write_ircam_comment(int fd, const char *comment, int len)
{
  if (len > 0)
    {
      mus_bshort_to_char((uint8_t *)hdrbuf, 2);
      mus_bshort_to_char((uint8_t *)(hdrbuf + 2), (short)len);
      header_write(fd, hdrbuf, 4);
      header_write(fd, (uint8_t *)comment, len);
    }
  else
    {
      mus_bint_to_char((uint8_t *)hdrbuf, 0);
      header_write(fd, hdrbuf, 4);
    }
  len = 1024 - (len + 20);
  if (len > 0)
    {
      uint8_t *combuf;
      combuf = (uint8_t *)calloc(len, sizeof(uint8_t));
      header_write(fd, combuf, len);
      free(combuf);
    }
}


static int write_ircam_header(int fd, int wsrate, int wchans, mus_sample_t samp_type, const char *comment, int len)
{
  mus_bint_to_char((uint8_t *)hdrbuf, 0x2a364); /* SUN id */
  mus_bfloat_to_char((uint8_t *)(hdrbuf + 4), (float)wsrate);
  mus_bint_to_char((uint8_t *)(hdrbuf + 8), wchans);
  mus_bint_to_char((uint8_t *)(hdrbuf + 12), sndlib_format_to_ircam(samp_type));
  header_write(fd, hdrbuf, 16);
  data_location = 1024;
  write_ircam_comment(fd, comment, len);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ 8SVX ------------------------------------- 
 * (also known as IFF)
 *
 * very similar to AIFF:
 *  "BODY" => [4] samples [n] data
 *  "VHDR" => srate (short)
 *  "CHAN" => chans
 *  "ANNO" and "NAME"
 *
 * big_endian throughout
 */

static int read_8svx_header(const char *filename, int fd, bool bytewise)
{
  static const uint8_t I_BODY[4] = {'B','O','D','Y'};
  static const uint8_t I_CHAN[4] = {'C','H','A','N'};
  static const uint8_t I_VHDR[4] = {'V','H','D','R'};
  int offset, chunkloc;
  bool happy = true;

  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
  chunkloc = 12;
  offset = 0;
  if (bytewise) sample_type = MUS_BYTE; else sample_type = MUS_BSHORT;
  srate = 0;
  chans = 1;
  true_file_length = SEEK_FILE_LENGTH(fd);
  update_form_size = mus_char_to_bint((uint8_t *)(hdrbuf + 4));

  while (happy)
    {
      int chunksize;
      offset += chunkloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s 8svx header: chunks confused at %d", filename, offset));

      chunksize = mus_char_to_bint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) && /* can be empty data chunk? */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < 0)
	break;

      if (match_four_chars((uint8_t *)hdrbuf, I_CHAN))
	{
	  chans = mus_char_to_bint((uint8_t *)(hdrbuf + 8));
	  chans = (chans & 0x01) + 
	          ((chans & 0x02) >> 1) + 
	          ((chans & 0x04) >> 2) + 
                  ((chans & 0x08) >> 3);
	  /* what in heaven's name is this?  Each bit corresponds to a channel? */
	}
      else
	{
	  if (match_four_chars((uint8_t *)hdrbuf, I_VHDR))
	    {
	      /* num_samples (int) at hdrbuf + 8 */
	      srate = mus_char_to_ubshort((uint8_t *)(hdrbuf + 20));
	      original_sample_type = hdrbuf[23];
	      if (original_sample_type != 0) 
		sample_type = MUS_UNKNOWN_SAMPLE;
	    }
	  else
	    {
	      if ((match_four_chars((uint8_t *)hdrbuf, I_ANNO)) || 
		  (match_four_chars((uint8_t *)hdrbuf, I_NAME)))
		{
		  comment_start = offset + 8;
		  comment_end = comment_start + chunksize - 1;
		}
	      else
		{
		  if (match_four_chars((uint8_t *)hdrbuf, I_BODY))
		    {
		      data_size = chunksize;
		      data_location = offset + 12;
		      happy = false;
		    }
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
    }

  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no BODY chunk?", filename));

  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);

  return(MUS_NO_ERROR);
}


/* ------------------------------------ VOC -------------------------------------- 
 *
 *   0: "Creative Voice File" followed by a couple ctrl-Z ('32) (swapped data)
 *  20: header end (short) {8svx, 26 = data_offset, 0x10a = version, ((~version + 0x1234) & 0xffff) = 0x1129}
 * [20]: first block:
 *     block code, 1 = data, 0 = end, 9 = data_16 (2 = continue, 3 = silence, 4 = marker, 5 = text, 6 = loop, 7 = loop-end, 8 = extended)
 *     block len as 24 bit int(?)
 *     if data, then rate code (byte), then data (assuming 8-bit unsigned, mono)
 *     if data_16, long srate, byte: data size (8 or 16), byte chans
 *     if text, ascii text (a comment)
 *     if extended (8) precedes 1 (data): 8 4 then time constant (short), byte: packing code (0), byte chans (0 = mono)
 *
 * apparently always little_endian
 * updated extensively 29-Aug-95 from sox10 voc.c
 */

static int read_voc_header(const char *filename, int fd)
{
  mus_long_t curbase;
  int voc_extended, bits, code;
  bool happy = true;

  sample_type = MUS_UBYTE;
  chans = 1;
  voc_extended = 0;
  true_file_length = SEEK_FILE_LENGTH(fd);
  curbase = mus_char_to_lshort((uint8_t *)(hdrbuf + 20));
  if (true_file_length < curbase)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: block location %" print_mus_long " > file length: %" print_mus_long, filename, curbase, true_file_length));

  lseek(fd, curbase, SEEK_SET);
  header_read(fd, hdrbuf, HDRBUFSIZ);

  while (happy)
    {
      int type, len;
      type = (int)(hdrbuf[0]);
      len = (((int)hdrbuf[3]) << 16) + (((int)hdrbuf[2]) << 8) + (((int)hdrbuf[1]));
      if (type == 1) /* voc_data */
	{
	  data_size = len - 1; /* was -3 */
	  data_location = curbase + 6;
	  if (voc_extended == 0) 
	    {
	      srate = (int)(1000000.0 / (256 - ((int)(hdrbuf[4] & 0xff))));
	      original_sample_type = hdrbuf[5];
	      if (hdrbuf[5] == 0) 
		sample_type = MUS_UBYTE; 
	      else sample_type = MUS_UNKNOWN_SAMPLE;
	    }
	  happy = false;
	}
      else
	{
	  if (type == 9) /* voc_data_16 */
	    {
	      data_size = len - 1; /* was -3 */
	      data_location = curbase + 6;
	      srate = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
	      bits = ((int)hdrbuf[8]);
	      if (bits == 8)
		{
		  code = mus_char_to_lshort((uint8_t *)(hdrbuf + 10));
		  if (code == 6) 
		    sample_type = MUS_ALAW;
		  else
		    if (code == 7)
		      sample_type = MUS_MULAW;
		    else sample_type = MUS_UBYTE; 
		}
	      else 
		if (bits == 16) 
		  sample_type = MUS_LSHORT;
		else sample_type = MUS_UNKNOWN_SAMPLE;
	      chans = (int)hdrbuf[9];
	      if (chans == 0) chans = 1;
	      happy = false;
	    }
	  else
	    {
	      if (((len + curbase) < true_file_length) && (type != 0))
		{
		  if (type == 5) /* voc_text */
		    {
		      comment_start = curbase + 4;
		      comment_end = comment_start + len - 1;
		    }
		  else
		    {
		      if (type == 8) /* voc_extended */
			{
			  /* should voc_extended be set to 1 here? */
			  srate = (256000000 / (65536 - mus_char_to_lshort((uint8_t *)(hdrbuf + 4))));
			  if ((int)(hdrbuf[7]) == 0) chans = 1; else chans = 2;
			  if ((int)(hdrbuf[6]) != 0) sample_type = MUS_UNKNOWN_SAMPLE;
			}
		      /* I'd add loop support here if I had any example sound files to test with */
		    }
		  if (seek_and_read(fd, (uint8_t *)hdrbuf, curbase + len + 4, HDRBUFSIZ) <= 0)
		    return(mus_error(MUS_HEADER_READ_FAILED, "%s voc header: ran off end of file", filename));
		  curbase += len;
		}
	      else happy = false;
	    }
	}
    }

  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no data(type 1 or 9) chunk?", filename));

  if ((data_size > true_file_length) || (data_size < (mus_long_t)(true_file_length / 10))) /* some VOC files seem to have completely bogus lengths */
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);

  return(MUS_NO_ERROR);
}



/* ------------------------------------ TwinVQ ------------------------------------ 
 *
 * from Audio Tools Library (atl.zip) at http://jfaul.de/atl.
 * a chunked header for proprietary (and apparently obsolete?) compressed data
 *
 * 0: "TWIN"
 * 4: version id (string)
 * 12: header size ["cardinal" -> bint]
 * common chunk header (4 of ID, bint size)
 * 24: channels (bint: 0=mono 1=stereo)
 * bitrate (bint)
 * 32: srate (bint khz 11, 22, 44 else *1000)
 * security (bint 0)
 * filesize (bint bytes)
 * possible chunks: NAME COMT AUTH (c) FILE ALBM DATA
 */ 

/* Monkey files start with "MAC ", but this is yet another compression-oriented format, I think (APE?) */

static int read_twinvq_header(const char *filename, int fd)
{
  sample_type = MUS_UNKNOWN_SAMPLE;
  data_location = mus_char_to_bint((uint8_t *)(hdrbuf + 12)) + 16 + 8;
  chans = 1 + mus_char_to_bint((uint8_t *)(hdrbuf + 24));
  srate = mus_char_to_bint((uint8_t *)(hdrbuf + 32));
  if (srate == 11) srate = 11025; else
    if (srate == 22) srate = 22050; else
      if (srate == 44) srate = 44100; else
	srate = 48000;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  return(MUS_NO_ERROR);
}


/* ------------------------------------ SDIF ------------------------------------ 
 * 
 * not usable in this context -- even the apparently non-existent 1TDS (sampled data)
 *   format consists of a sequence of chunks, each of any size.
 *   Why invent an uncompressed format that makes random access impossible?
 */

static int read_sdif_header(const char *filename, int fd)
{
  static const uint8_t I_1FQ0[4] = {'1','F','Q','0'}; 
  static const uint8_t I_1STF[4] = {'1','S','T','F'}; 
  static const uint8_t I_1PIC[4] = {'1','P','I','C'}; 
  static const uint8_t I_1TRC[4] = {'1','T','R','C'}; 
  static const uint8_t I_1HRM[4] = {'1','H','R','M'}; 
  static const uint8_t I_1RES[4] = {'1','R','E','S'}; 
  static const uint8_t I_1TDS[4] = {'1','T','D','S'};  /* samples -- all others are useless */
  static const char *sdif_names[7] = {"fundamental frequency", "FFT", "spectral peak", "sinusoidal track", "harmonic track", "resonance", "unknown"};

  int offset;
  offset = 16;
  while (true)
    {
      int size;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s, sdif header: chunks confused at %d", filename, offset));
      size = mus_char_to_bint((uint8_t *)(hdrbuf + 4)) + 8; 

      if (match_four_chars((uint8_t *)hdrbuf, I_1TDS))
	break;
      else
	{
	  int type = 0;
	  if (match_four_chars((uint8_t *)hdrbuf, I_1FQ0))
	    type = 0;
	  else if (match_four_chars((uint8_t *)hdrbuf, I_1STF))
	    type = 1;
	  else if (match_four_chars((uint8_t *)hdrbuf, I_1PIC))
	    type = 2;
	  else if (match_four_chars((uint8_t *)hdrbuf, I_1TRC))
	    type = 3;
	  else if (match_four_chars((uint8_t *)hdrbuf, I_1HRM))
	    type = 4;
	  else if (match_four_chars((uint8_t *)hdrbuf, I_1RES))
	    type = 5;
	  else type = 6;
	  return(mus_error(MUS_HEADER_READ_FAILED, "this SDIF file contains %s data, not sampled sound", sdif_names[type]));
	}
      /* unreachable */
      offset += size;
    }
  return(MUS_HEADER_READ_FAILED);
}


#if G7XX
/* ------------------------------------ NVF ------------------------------------ 
 */

static int read_nvf_header(const char *filename, int fd)
{
  static const uint8_t I_VFMT[4] = {'V','F','M','T'};  /* Nomad II Creative NVF */

  /* info from nvftools by Tom Mander: */
  /*
    Numbers stored little-endian.

    bytes 0-3:   "NVF "                 magic number
    bytes 4-7:    0x00000001            NVF version number?
    bytes 8-11:   0x00000020            size of rest of header
    bytes 12-15:  "VFMT"                VFMT chunk h
    bytes 16-19:  0x00000001            VFMT version number?
    bytes 20-23:  0x00000014            size of reset of VFMT header
    bytes 24-27:  0x00007D00            32000 bit/s bitrate
    bytes 28-29:  0x0001                channels
    bytes 30-31:  0x0000                unknown
    bytes 32-35:  0x00001F40            8000kHz sample rate
    bytes 36-39:  0x00003E80            16000
    bytes 40-41:  0x0002                width in bytes of uncompressed data?
    bytes 42-43:  0x0010                width in bits of compressed data?

    The rest of the data is G.721 data nibble packing big-endian, 4bits per
    sample (nibble) single channel at 32kbit. When the Nomad records an NVF
    file it does it in 92 sample (46 byte) framples or 0.0115sec.
  */
  if (mus_char_to_lint((uint8_t *)(hdrbuf + 4)) != 1) return(mus_error(MUS_HEADER_READ_FAILED, "%s: NVF[4] != 1", filename));
  if (!(match_four_chars((uint8_t *)(hdrbuf + 12), I_VFMT))) return(mus_error(MUS_HEADER_READ_FAILED, "%s: no VFMT chunk", filename));
  sample_type = MUS_UNKNOWN_SAMPLE; /* g721 --translate elsewhere */
  chans = 1;
  srate = 8000;
  data_location = 44;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location) * 2; /* 4 bit samps? */
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  return(MUS_NO_ERROR);
}
#endif



/* ------------------------------------ ADC ------------------------------------ 
 * also known as OGI format
 * TIMIT format is identical except it omits the sample type field (header size claims to be bytes)
 *
 * from ad.h and other files, ogitools-v1.0.tar.gz
 * we'll look for the big/little endian sequence (short) 8 1 1-or-2 given big/little decision
 *
 * 0: header size in shorts (8 = 16 bytes) (OGI says this is in bytes)
 * 2: version (1)
 * 4: chans
 * 6: rate (srate = 4000000/rate)
 * 8: samples (int) -- seems to be off by 2 -- are they counting ints here?
 * 12: sample type (0 = big-endian)
 * 16: data start
*/ 

static int read_adc_header(const char *filename, int fd)
{
  bool little;
  little = (mus_char_to_uninterpreted_int((uint8_t *)(hdrbuf + 12)) != 0); /* 0 = big endian */
  data_location = 16;
  if (little) sample_type = MUS_LSHORT; else sample_type = MUS_BSHORT;
  chans = big_or_little_endian_short((uint8_t *)(hdrbuf + 4), little);
  srate = 4000000 / big_or_little_endian_short((uint8_t *)(hdrbuf + 6), little);
  data_size = 2 * big_or_little_endian_int((uint8_t *)(hdrbuf + 8), little);
  comment_start = 0;
  comment_end = 0;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ AVR -------------------------------------- 
 *
 *   0: "2BIT"
 *   4: sample name (null padded ASCII)
 *  12: chans (short) (0 = mono, -1 = stereo)
 *  14: sample size (8 or 16 bit) (short) (value is 8, 12, or 16)
 *  16: sample type (signed or unsigned) (short) (0 = unsigned, -1 = signed)
 *  18: loop (on/off), 20: midi (-1 = no MIDI)
 *  22: srate 
 *      avr.txt has:
 *      22: Replay speed 0 = 5.485 Khz, 1 = 8.084 Khz, 2 = 10.971 Khz, 3 = 16.168 Khz, 4 = 21.942 Khz, 5 = 32.336 Khz, 6 = 43.885 Khz, 7 = 47.261 Khz
 *      23: sample rate	in Hertz (as a 3 byte quantity??)
 *  26: length in samples
 *  30: loop beg, 34: loop end, 38: midi (keyboard split), 40: compression, 42: nada ("reserved"), 44: name
 *  64: comment (limited to 64 bytes)
 * 128: data start
 *
 * the Atari .avr files appear to be 8000 Hz, mono, 8-bit linear unsigned data with an unknown header of 128 words
 * apparently there was a change in format sometime in the 90's.
 * 
 * The actual avr files I've found on the net are either garbled, or
 * something is wrong with this definition (taken from CMJ and www.wotsit.org's avr.txt). 
 * SGI dmconvert assumes big-endian here -- this is an Atari format, so it's probably safe to assume big-endian.
 */

static int read_avr_header(const char *filename, int fd)
{
  int dsize, dsigned, i;
  chans = mus_char_to_bshort((uint8_t *)(hdrbuf + 12));
  if (chans == 0) chans = 1; else if (chans == -1) chans = 2; else return(mus_error(MUS_HEADER_READ_FAILED, "%s chans: %d", filename, chans));
  data_location = 128;
  data_size = mus_char_to_bint((uint8_t *)(hdrbuf + 26));
  srate = mus_char_to_ubshort((uint8_t *)(hdrbuf + 24));
  dsize = mus_char_to_bshort((uint8_t *)(hdrbuf + 14));
  dsigned = mus_char_to_bshort((uint8_t *)(hdrbuf + 16));
  if (dsize == 16) 
    {
      if (dsigned == 0)
	sample_type = MUS_UBSHORT;
      else sample_type = MUS_BSHORT;
    }
  else 
    {
      if (dsize == 8)
	{
	  if (dsigned == 0) 
	    sample_type = MUS_UBYTE;
	  else sample_type = MUS_BYTE;
	}
      else return(mus_error(MUS_HEADER_READ_FAILED, "%s: unknown sample type", filename));
    }
  if (seek_and_read(fd, (uint8_t *)hdrbuf, 64, 64) <= 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s avr header: ran off end of file", filename));
  comment_start = 64;
  i = 0;
  while ((i < 64) && (hdrbuf[i] != 0)) i++;
  comment_end = 64 + (i - 1);
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ SNDT -------------------------------------
 *
 * this taken from sndrtool.c (sox-10): (modified 6-Feb-98)
 *   0: "SOUND" (or off by two throughout if not "0x1a"?)
 *   5: 0x1a
 *   6-7: 0
 *   8-11: nsamps (at 12)
 *  12-15: 0
 *  16-19: nsamps
 *  20-21: srate (little endian short) (at 22)
 *  22-23: 0 
 *  24-25: 10
 *  26-27: 4
 *  28-> : <filename> "- File created by Sound Exchange"
 *  .->95: 0 ?
 */

/* similar is Sounder format: 
 * 0: 0
 * 2: short srate (little endian)
 * 4: 10
 * 6: 4
 * then data
 * but this format can't be distinguished from a raw sound file
 */

static int read_sndt_header(const char *filename, int fd)
{
  if (hdrbuf[4] != 'D') return(mus_error(MUS_HEADER_READ_FAILED, "%s: SNDT[4] != 'D'", filename));
  sample_type = MUS_UBYTE;
  chans = 1;
  srate = mus_char_to_ulshort((uint8_t *)(hdrbuf + 20));
  data_location = 126;
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 8));
  if (data_size < 0) data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 10));
  if (srate <= 1) srate = mus_char_to_ulshort((uint8_t *)(hdrbuf + 22));
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Covox v8 ------------------------------------- 
 *
 *  0: 377 125 377 252 377 125 377 252 x x 0's to 16
 * then 8-bit unsigned data
 */

static int read_covox_header(const char *filename, int fd)
{
  sample_type = MUS_UBYTE;
  chans = 1;
  data_location = 16;
  srate = 8000;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = true_file_length - data_location;
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  return(MUS_NO_ERROR);
}



/* ------------------------------------ SMP ------------------------------------- 
 *
 *  0: "SOUND SAMPLE DATA "
 * 18: "2.1 "
 * 22-81: comment
 * 82-111: sample name
 * header 112 bytes
 * long samples (bytes = samples*2)
 * then data start
 * data
 * always little endian
 */

static int read_smp_header(const char *filename, int fd)
{
  sample_type = MUS_LSHORT;
  chans = 1;
  comment_start = 22;
  comment_end = 81;
  data_location = 116;
  lseek(fd, 112, SEEK_SET);
  if (read(fd, hdrbuf, 4) != 4) return(mus_error(MUS_HEADER_READ_FAILED, "%s: SMP header truncated?", filename));
  data_size = (mus_char_to_lint((uint8_t *)hdrbuf));
  sample_type = MUS_LSHORT; /* just a guess */
  srate = 8000; /* docs mention an srate floating around at the end of the file, but I can't find it in any example */
  true_file_length = SEEK_FILE_LENGTH(fd);
  if ((data_size * 2) > true_file_length)
    {
      data_size = (true_file_length - data_location) / 2;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  return(MUS_NO_ERROR);
}



/* ------------------------------------ SPPACK ------------------------------------- 
 * 
 * from AF docs:
 *         Bytes   Type    Contents
 *     0   160    char   Text strings (2 * 80)
 *   160    80    char   Command line
 *   240     2    int    Domain (1-time, 2-freq, 3-qfreq)
 *   242     2    int    Frample size
 *   244     4    float  Sampling frequency
 *   252     2    int    File identifier (i.e. #o100 #o303)
 *   254     2    int    Data type (0xfc0e = sampled data file)
 *   256     2    int    Resolution (in bits 8, 16)
 *   258     2    int    Companding flag
 *   272   240    char   Text strings (3 * 80)
 *   512   ...    --     Audio data
 *
 * at least one program is writing these headers using little endian data...
 */

static int read_sppack_header(const char *filename, int fd)
{
  int typ;
  data_location = 512;
  chans = 1;
  lseek(fd, 240, SEEK_SET);
  if (read(fd, hdrbuf, 22) != 22) return(mus_error(MUS_HEADER_READ_FAILED, "%s SPPACK header truncated?", filename));
  typ = mus_char_to_bshort((uint8_t *)hdrbuf);
  sample_type = MUS_UNKNOWN_SAMPLE;
  if (typ == 1) 
    {
      if (((hdrbuf[254]) == 252) && ((hdrbuf[255]) == 14)) /* #xfc and #x0e */
	{
	  int bits;
	  float sr;
	  typ = mus_char_to_bshort((uint8_t *)(hdrbuf + 18));
	  bits = mus_char_to_bshort((uint8_t *)(hdrbuf + 16));
	  sr = mus_char_to_bfloat((uint8_t *)(hdrbuf + 4));
	  srate = (int)sr;
	  switch (typ)
	    {
	    case 1: if (bits == 16) sample_type = MUS_BSHORT; else sample_type = MUS_BYTE; break;
	    case 2: sample_type = MUS_ALAW; break;
	    case 3: sample_type = MUS_MULAW; break;
	    default: sample_type = MUS_UNKNOWN_SAMPLE; break;
	    }
	  data_size = SEEK_FILE_LENGTH(fd);
	  data_size = mus_bytes_to_samples(sample_type, data_size - 512);
	  comment_start = 0;
	  comment_end = 0;
	}
    }
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location) 
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  if (data_size > mus_bytes_to_samples(sample_type, true_file_length))
    data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ ESPS (Entropic Signal Processing System) ------------------------------------- 
 *
 * specs at ftp.entropic.com (also known as "SD" format)
 * from AFgetInfoES.c:
 * 
 *       Bytes     Type    Contents
 *      8 -> 11    --     Header size (bytes)
 *     12 -> 15    int    Sampled data record size
 *     16 -> 19    int    File identifier: 0x00006a1a or 0x1a6a0000
 *     40 -> 65    char   File creation date
 *    124 -> 127   int    Number of samples
 *    132 -> 135   int    Number of doubles in a data record
 *    136 -> 139   int    Number of floats in a data record
 *    140 -> 143   int    Number of longs in a data record
 *    144 -> 147   int    Number of shorts in a data record
 *    148 -> 151   int    Number of chars in a data record
 *    160 -> 167   char   User name 
 *    333 -> H-1   --     "Generic" header items, including "record_freq" {followed by a "double8"=64-bit ?}
 *      H -> ...   --     Audio data
 */

static int read_esps_header(const char *filename, int fd)
{
  char str[80];
  bool happy = true;
  mus_long_t curbase, hend;
  int k, j, n, chars, floats, shorts, doubles;
  int64_t bytes;
  bool little;
  little = (hdrbuf[18] == 0);
  if (little)
    data_location = mus_char_to_lint((uint8_t *)(hdrbuf + 8));
  else data_location = mus_char_to_bint((uint8_t *)(hdrbuf + 8));
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = true_file_length - data_location;
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 8000;
  chans = 1;
  lseek(fd, 132, SEEK_SET);
  header_read(fd, hdrbuf, HDRBUFSIZ);
  if (little)
    {
      doubles = mus_char_to_lint((uint8_t *)hdrbuf);
      floats = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
      shorts = mus_char_to_lint((uint8_t *)(hdrbuf + 12));
      chars = mus_char_to_lint((uint8_t *)(hdrbuf + 16));
    }
  else
    {
      doubles = mus_char_to_bint((uint8_t *)hdrbuf);
      floats = mus_char_to_bint((uint8_t *)(hdrbuf + 4));
      shorts = mus_char_to_bint((uint8_t *)(hdrbuf + 12));
      chars = mus_char_to_bint((uint8_t *)(hdrbuf + 16));
    }
  if (shorts != 0)
    {
      sample_type = ((little) ? MUS_LSHORT : MUS_BSHORT); 
      chans = shorts;
    }
  else
    {
      if (doubles != 0)
	{
	  sample_type = ((little) ? MUS_LDOUBLE_UNSCALED : MUS_BDOUBLE_UNSCALED);
	  chans = doubles;
	}
      else
	{
	  if (floats != 0)
	    {
	      sample_type = ((little) ? MUS_LFLOAT_UNSCALED : MUS_BFLOAT_UNSCALED);
	      chans = floats;
	    }
	  else
	    {
	      if (chars != 0)
		{
		  sample_type = MUS_BYTE; /* ?? */
		  chans = chars;
		}
	    }
	}
    }
  /* search for "record_freq" to get srate */
  lseek(fd, 333, SEEK_SET);
  header_read(fd, hdrbuf, HDRBUFSIZ);
  curbase = 333;
  hend = curbase + HDRBUFSIZ;
  k = 0;
  n = 0;
  for (j = 0; j < 80; j++) str[j] =' ';  
  while (happy) 
    {
      str[k] = hdrbuf[n];
      if ((str[k] == 'q') || (str[k] == 3) || ((curbase + n + 1) >= data_location) || (k == 78))
	{ /* 3 = C-C marks end of record (?) */
	  str[k + 1] = 0;
	  if (strcmp(str, "record_freq") == 0) 
	    {
	      if (seek_and_read(fd, (uint8_t *)hdrbuf, curbase + n, 32) <= 0)
		return(mus_error(MUS_HEADER_READ_FAILED, "%s esps header: ran off end of file", filename));
	      n = 0;
	      if (little)
		srate = (int)mus_char_to_ldouble((uint8_t *)(hdrbuf + 8));
	      else srate = (int)mus_char_to_bdouble((uint8_t *)(hdrbuf + 8));
	      happy = false;
	    }
	  if ((curbase + n + 1) >= data_location) happy = false;
	  k = 0;
	}
      else
	k++;
      n++;
      if (n >= hend)
	{
	  curbase += hend;
	  n = 0;
	  bytes = read(fd, hdrbuf, HDRBUFSIZ);
	  if (bytes != HDRBUFSIZ) break;
	  hend = HDRBUFSIZ;
	}
    }
  if (srate == 0) srate = 8000;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}

 
 
/* ------------------------------------ INRS ------------------------------------- 
 * 
 *   from AFgetInfoIN.c:
 * 
 *    INRS-Telecommunications audio file:
 *       Bytes     Type    Contents
 *      0 ->  3    float  Sampling Frequency (VAX float format)
 *      6 -> 25    char   Creation time (e.g. Jun 12 16:52:50 1990)
 *     26 -> 29    int    Number of speech samples in the file (? -- old INRS files omit this)
 *   The data in an INRS-Telecommunications audio file is in 16-bit integer (little-endian)
 *   format. Header is always 512 bytes.  Always mono.
 * 
 */

static int read_inrs_header(const char *filename, int fd, int loc)
{
  true_file_length = SEEK_FILE_LENGTH(fd);
  comment_start = 6;
  comment_end = 25;
  sample_type = MUS_LSHORT;
  srate = loc;
  chans = 1;
  data_location = 512;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location) 
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ MAUD ------------------------------------- 
 *
 * very similar to AIFF:
 *  "MHDR" => 4: chunksize (32)
 *            8: samples 
 *           12: bits 
 *           14: ditto
 *           16: clock freq
 *           20: clock div (srate = freq/div)
 *           22: chan info (0 = mono, 1 = stereo)
 *           24: ditto(?!)
 *           26: format (0 = unsigned 8 or signed 16 (see bits), 2 = alaw, 3 = mulaw)
 *           28-40: unused
 *  "MDAT" => data
 *  "ANNO" => comment
 *
 * according to /usr/share/magic, this stands for "MacroSystem Audio"
 */

static int read_maud_header(const char *filename, int fd)
{
  static const uint8_t I_MHDR[4] = {'M','H','D','R'};
  static const uint8_t I_MDAT[4] = {'M','D','A','T'};

  int offset, chunkloc;
  bool happy = true;
  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
  chunkloc = 12;
  offset = 0;
  sample_type = MUS_BYTE;
  srate = 0;
  chans = 1;
  update_form_size = mus_char_to_bint((uint8_t *)(hdrbuf + 4));
  while (happy)
    {
      int chunksize;
      offset += chunkloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 32) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s maud header: chunks confused at %d", filename, offset));
      chunksize = mus_char_to_bint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) && /* can be empty data chunk? */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < 0)
	break;
      if (match_four_chars((uint8_t *)hdrbuf, I_MHDR))
	{
	  int num, den;
	  data_size = mus_char_to_bint((uint8_t *)(hdrbuf + 8));
	  num = mus_char_to_bint((uint8_t *)(hdrbuf + 16));
	  den = mus_char_to_bshort((uint8_t *)(hdrbuf + 20));
	  if (den == 0) den = 1;
	  srate = (int)(num / den);
	  num = mus_char_to_bshort((uint8_t *)(hdrbuf + 12));
	  den = mus_char_to_bshort((uint8_t *)(hdrbuf + 26));
	  if (num == 8)
	    {
	      switch (den)
		{
		case 0: sample_type = MUS_UBYTE; break;
		case 2: sample_type = MUS_ALAW; break;
		case 3: sample_type = MUS_MULAW; break;
		default: sample_type = MUS_UNKNOWN_SAMPLE; break;
		}
	    }
	  else sample_type = MUS_BSHORT;
	  num = mus_char_to_bshort((uint8_t *)(hdrbuf + 22));
	  if (num == 0) chans = 1; else chans = 2;
	}
      else
	{
	  if (match_four_chars((uint8_t *)hdrbuf, I_ANNO))
	    {
	      comment_start = offset + 8;
	      comment_end = comment_start + chunksize - 1;
	    }
	  else
	    {
	      if (match_four_chars((uint8_t *)hdrbuf, I_MDAT))
		{
		  data_location = offset + 12;
		  happy = false;
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++; /* extra null appended to odd-length chunks */
    }
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no MDAT chunk?", filename));
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ CSL ------------------------------------- 
 *
 * "Computerized Speech Labs -- this info taken from wavesurfer/snack
 *
 * very similar to AIFF:
 * 0: FORM
 * 4: DS16 (kinda weird)
 * 8: size (int le)
 * 12: chunks
 *     HEDR or HDR8
 *     4: size (int)
 *     samp: short, chans: 1 at 36 if not (int) -1, chans: 2?
 *     srate at 28 (le int?)
 * other chunks: SD_B, SDA_ SDAB with data bytes as data followed by data
 */

static int read_csl_header(const char *filename, int fd)
{
  static const uint8_t I_HEDR[4] = {'H','E','D','R'};  
  static const uint8_t I_HDR8[4] = {'H','D','R','8'};  
  static const uint8_t I_SDA_[4] = {'S','D','A','_'};  
  static const uint8_t I_SDAB[4] = {'S','D','A','B'};  
  static const uint8_t I_SD_B[4] = {'S','D','_','B'};  
  static const uint8_t I_NOTE[4] = {'N','O','T','E'};  

  int offset, chunkloc;
  bool happy = true;
  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
  chunkloc = 12;
  offset = 0;
  sample_type = MUS_LSHORT;
  srate = 0;
  chans = 1;
  update_form_size = mus_char_to_lint((uint8_t *)(hdrbuf + 8));
  while (happy)
    {
      int chunksize;
      offset += chunkloc;
      if (seek_and_read(fd, (uint8_t *)hdrbuf, offset, 64) <= 0)
	return(mus_error(MUS_HEADER_READ_FAILED, "%s csl header: chunks confused at %d", filename, offset));
      chunksize = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
      if ((chunksize == 0) && /* can be empty data chunk? */
	  (hdrbuf[0] == 0) && (hdrbuf[1] == 0) && (hdrbuf[2] == 0) && (hdrbuf[3] == 0))
	break;
      if (chunksize < 0)
	break;
      if ((match_four_chars((uint8_t *)hdrbuf, I_HEDR)) || 
	  (match_four_chars((uint8_t *)hdrbuf, I_HDR8)))
	{
	  /* 8..20: date as ascii */
	  /* 32: data length (int) in bytes */
	  if ((mus_char_to_lshort((uint8_t *)(hdrbuf + 36)) != -1) && /* these are maxamps, -1=none */
	      (mus_char_to_lshort((uint8_t *)(hdrbuf + 38)) != -1))
	    chans = 2;
	  srate = mus_char_to_lint((uint8_t *)(hdrbuf + 28));
	}
      else
	{
	  if (match_four_chars((uint8_t *)hdrbuf, I_NOTE))
	    {
	      comment_start = offset + 8;
	      comment_end = comment_start + chunksize - 1;
	    }
	  else
	    {
	      if ((match_four_chars((uint8_t *)hdrbuf, I_SDA_)) ||
		  (match_four_chars((uint8_t *)hdrbuf, I_SDAB)) ||
		  (match_four_chars((uint8_t *)hdrbuf, I_SD_B)))
		{
		  data_location = offset + 8;
		  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 4));
		  happy = false;
		}
	    }
	}
      chunkloc = (8 + chunksize);
      if (chunksize & 1) chunkloc++;
    }
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no SDxx chunk?", filename));
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ snack SMP ------------------------------------- 
 *
 * there's apparently yet another "smp" format (from nist??)
 * file = samp
 * sftot = 22050
 * msb = last
 * nchans = 1
 * preemph = none
 * born = snack
 * msb = last here -> little endian?
 * data at 1024
 */

static int read_file_samp_header(const char *filename, int fd)
{
  int i = 0;
  uint8_t *locbuf;
  data_location = 1024;
  chans = 1;
  srate = 8000;
  sample_type = MUS_LSHORT;
  lseek(fd, 10, SEEK_SET);
  locbuf = (uint8_t *)calloc(1024, sizeof(uint8_t));
  header_read(fd, locbuf, 1024);
  while (i < 1024)
    {
      if (strncmp((char *)(locbuf + i), "sftot", 5) == 0)
	sscanf((const char *)(&locbuf[i + 6]), "%12d", &srate);
      if (strncmp((char *)(locbuf + i), "nchans", 6) == 0)
	sscanf((const char *)(&locbuf[i + 7]), "%12d", &chans);
      if (strncmp((char *)(locbuf + i), "msb", 3) == 0)
	if (strncmp((char *)(locbuf + i + 4), "first", 5) == 0)
	  sample_type = MUS_BSHORT;
      while ((i < 1024) && (locbuf[i] != 10) && (locbuf[i] != 0)) i++;
      i++;
    }
  free(locbuf);
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Sound Designer I -------------------------------------
 *
 * complicated and defined in terms of Pascal records, so the following is a stab in the dark:
 *
 * 0:    1336 (i.e. header size)
 * 764:  comment (str255)
 * 1020: sample rate (long)
 * 1028: data size (short)
 * 1030: a code string describing the data type (i.e. "linear") (str32)
 * 1064: user comment (str255)
 *
 * file type: 'SFIL'
 *
 * always big_endian
 */

static int read_sd1_header(const char *filename, int fd)
{
  int n;
  chans = 1;
  data_location = 1336;
  lseek(fd, 1020, SEEK_SET);
  if (read(fd, hdrbuf, 64) != 64) return(mus_error(MUS_HEADER_READ_FAILED, "%s Sound Designer I header truncated?", filename));
  srate = mus_char_to_bint((uint8_t *)hdrbuf);
  n = mus_char_to_bshort((uint8_t *)(hdrbuf + 8));
  if (n == 16)
    sample_type = MUS_BSHORT;
  else sample_type = MUS_BYTE;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  n = ((uint8_t)hdrbuf[44]);
  if (n != 0) 
    {
      comment_start = 1064;
      comment_end = comment_start + n - 1;
    }
  return(MUS_NO_ERROR);
}



/* ------------------------------------ PSION alaw -------------------------------------
 *
 * 0: "ALawSoundFile**"
 * 16: version 
 * 18: length (bytes)
 * 22: padding
 * 24: repeats
 * 26-32: nada
 * 32: data
 *
 * always mono 8-bit alaw 8000 Hz. All the examples on the psion net site appear to be little endian.
 */

static int read_psion_header(const char *filename, int fd)
{
  if ((hdrbuf[13] != '*') || (hdrbuf[14] != '*')) return(mus_error(MUS_HEADER_READ_FAILED, "%s: PSION[13, 14] != '*'", filename));
  chans = 1;
  data_location = 32;
  srate = 8000;
  sample_type = MUS_ALAW;
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 18)); /* always little-endian? */
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Gravis Ultrasound Patch -------------------------------------
 *
 * http://www.gravis.com/Public/sdk/PATCHKIT.ZIP
 *
 * header [128], instruments [62], layers [49], waveheaders (nested)
 * always little endian, actual files don't match exactly with any documentation
 *
 * Header block:
 *   0:  "GF1PATCH100" or "GF1PATCH110"
 *   12: "ID#000002"
 *   22: comment (copyright notice) (60 bytes ASCIZ)
 *   82: number of instruments
 *   83: number of voices
 *   84: wave channels
 *   85: number of waves
 *   87: vol
 *   89: size? 
 *   93: reserved (36? bytes)
 *
 * Instrument block:
 *   0: id
 *   2: name (16 bytes)
 *   18: size
 *   22: number of layers
 *   23: reserved (40? bytes)
 *
 * Layer block:
 *   0: "previous"
 *   1: id
 *   2: size
 *   6: number of wave samples
 *  10: reserved (40? bytes)
 *
 * Wave block:
 *   0: name (7 bytes ASCIZ)
 *   7: "fractions"
 *   8: data size of wave sample
 *  12: loop start
 *  16: loop end
 *  20: sample rate
 *  22: low freq
 *  26: high freq
 *  30: root freq
 *  34: tune
 *  36: balance
 *  37: envelope data (6+6 bytes I think)
 *  49: tremolo and vibrato data (6 bytes)
 *  55: mode bit 0: 8/16, 1: signed/unsigned
 *  56: scale freq
 *  58: scale factor
 *  60: reserved (36 bytes)
 *  followed by data presumably
 */

static int read_gravis_header(const char *filename, int fd)
{
  int mode;
  chans = hdrbuf[84];
  if (chans == 0) chans = 1;
  comment_start = 22;
  comment_end = 81;
  lseek(fd, 239, SEEK_SET); /* try to jump to wave sample block (128+62+49) */
  header_read(fd, hdrbuf, 128);
  srate = mus_char_to_ulshort((uint8_t *)(hdrbuf + 20));
  data_size = mus_char_to_ulshort((uint8_t *)(hdrbuf + 8));
  mode = hdrbuf[55];
  if (mode & 1)
    {
      if (mode & 2)
	sample_type = MUS_ULSHORT;
      else sample_type = MUS_LSHORT;
    }
  else
    {
      if (mode & 2)
	sample_type = MUS_UBYTE;
      else sample_type = MUS_BYTE;
    }
  data_location = 337;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Goldwave -------------------------------------
 *
 * http://web.cs.mun.ca/~chris3/goldwave/goldwave.html
 */

static int read_goldwave_header(const char *filename, int fd)
{
  chans = 1;
  data_location = 28;
  sample_type = MUS_LSHORT;
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 22));
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  if ((data_size <= 24) || (data_size > true_file_length))
    data_size = (true_file_length - data_location) / 2;
  else data_size /= 2;
  srate = mus_char_to_lint((uint8_t *)(hdrbuf + 18));
  return(MUS_NO_ERROR);
}


/* ------------------------------------ Sonic Resource Foundry -------------------------------------
 *
 * more reverse engineering...
 * http://www.sfoundry.com/
 */

static int read_srfs_header(const char *filename, int fd)
{
  chans = 1; /* might be short at header[4] */
  data_location = 32;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = (true_file_length - data_location) / 2;
  srate = mus_char_to_lint((uint8_t *)(hdrbuf + 6));
  sample_type = MUS_LSHORT;
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Quicktime -------------------------------------
 *
 * infinitely complicated -- see Quicktime File Format doc from Apple.
 * there's no relation between this document and actual files -- a bizarre joke?
 */

static int read_qt_header(const char *filename, int fd)
{
  chans = 1;
  data_location = 12;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 11025; /* ?? */
  sample_type = MUS_UBYTE;
  return(MUS_NO_ERROR);
}



/* ------------------------------------ SBStudioII -------------------------------------
 *
 * from a file created by Convert 1.4
 * 0: SND <space>
 * 8: file size - 8
 * SNNA SNIN SNDT blocks:
 *
 * built in blocks, other names are SNIN, SNDT
 * need to scan for SNDT, block length, data
 * SNNA len name 
 * supposedly ends with END (but my examples don't)
 * SNIN: 
 *   num (2), reserved (2), tuning (1), vol (2), type (2) bit 0: 1 = PCM, bit 1: 1 = 16, 0 = 8 (then loop data)
 * info from Pac.txt (pac.zip) at http://www.wotsit.org/music.htm 
 */

static int read_sbstudio_header(const char *filename, int fd)
{
  static const uint8_t I_SNIN[4] = {'S','N','I','N'};
  static const uint8_t I_SNNA[4] = {'S','N','N','A'};
  static const uint8_t I_SNDT[4] = {'S','N','D','T'};

  int i, tmp;
  bool happy = true;
  uint8_t *bp;
  chans = 1; 
  srate = 8000; /* no sampling rate field in this header */
  sample_type = MUS_UNKNOWN_SAMPLE;
  true_file_length = SEEK_FILE_LENGTH(fd);
  i = 8;
  bp = (uint8_t *)(hdrbuf + 8);
  while (happy)
    {
      if (match_four_chars(bp, I_SNDT))
	{
	  data_size = mus_char_to_lint((uint8_t *)(bp + 4));
	  data_location = i + 8;
	  happy = false;
	}
      else
	{
	  if (match_four_chars(bp, I_SNIN))
	    {
	      tmp = mus_char_to_lshort((uint8_t *)(bp + 15));
	      if ((tmp & 1) == 0) 
		sample_type = MUS_UNKNOWN_SAMPLE;
	      else
		{
		  if ((tmp & 2) == 0) 
		    sample_type = MUS_BYTE;
		  else sample_type = MUS_LSHORT;
		}
	      i += 26;
	      bp += 26;
	    }
	  else
	    {
	      if (match_four_chars(bp, I_SNNA))
		{
		  tmp = mus_char_to_lint((uint8_t *)(bp + 4));
		  i += tmp;
		  bp += tmp;
		}
	      else
		{
		  i++;
		  bp++;
		}
	    }
	}
      if (i >= HDRBUFSIZ)
	{
	  sample_type = MUS_UNKNOWN_SAMPLE;
	  happy = false;
	}
    }
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: no SNDT chunk?", filename));
  if ((data_size == 0) || (sample_type == MUS_UNKNOWN_SAMPLE)) 
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data size or sample type bogus", filename));
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Delusion Sound -------------------------------------
 *
 * more reverse engineering...
 * from a file created by Convert 1.4
 * 0: DDSF
 * 5: name (text)
 * 55: data
 * probaby similar to DMF format described in Dmf-form.txt but I don't see any other block names in the data
 */

static int read_delusion_header(const char *filename, int fd)
{
  if ((hdrbuf[4] != 1) || (hdrbuf[5] > 128) || (hdrbuf[6] > 128) || (hdrbuf[7] > 128)) 
    return(mus_error(MUS_HEADER_READ_FAILED, "%s DDSF name bogus", filename));
  chans = 1; 
  data_location = 55;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 8000;
  sample_type = MUS_LSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Farandole Composer WaveSample -------------------------------------
 *
 * 0: FSM 254
 *    libmodplug load_far.cpp uses: #define FARFILEMAGIC	0xFE524146 ("FAR="?)
 * 4: name (text) (32 bytes)
 * 36: 10, 13, 26 or something like that
 * 39: len?
 * 40: volume
 * 41: looping data
 * 49: type (0 = 8-bit, else 16)
 * 50: loop mode
 * 51: data
 * described in Fsm.txt and Far-form.txt http://www.wotsit.org/music.htm 
 */

static int read_farandole_header(const char *filename, int fd)
{
  chans = 1; 
  data_location = 51;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 8000;
  if (hdrbuf[49] == 0)
    sample_type = MUS_BYTE;
  else sample_type = MUS_LSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Yamaha TX-16W -------------------------------------
 *
 * ftp://ftp.t0.or.at/pub/sound/tx16w/samples.yamaha
 * ftp://ftp.t0.or.at/pub/sound/tx16w/faq/tx16w.tec
 * http://www.t0.or.at/~mpakesch/tx16w/
 *
 * from tx16w.c sox 12.15: (7-Oct-98) (Mark Lakata and Leigh Smith)
 *  char filetype[6] "LM8953"
 *  nulls[10],
 *  dummy_aeg[6]
 *  format 0x49 = looped, 0xC9 = non-looped
 *  sample_rate 1 = 33 kHz, 2 = 50 kHz, 3 = 16 kHz 
 *  atc_length[3] if sample rate 0, [2]&0xfe = 6: 33kHz, 0x10:50, 0xf6: 16, depending on [5] but to heck with it
 *  rpt_length[3] (these are for looped samples, attack and loop lengths)
 *  unused[2]
 */

static int read_tx16w_header(const char *filename, int fd)
{
  if ((hdrbuf[4] != '5') || (hdrbuf[5] != '3')) return(mus_error(MUS_HEADER_READ_FAILED, "%s TX16 magic number bogus", filename));
  chans = 1; 
  data_location = 32;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 16000;
  if (hdrbuf[23] == 1) srate = 33000;
  else if (hdrbuf[23] == 2) srate = 50000;
  else if (hdrbuf[23] == 3) srate = 16000;
  else if (hdrbuf[23] == 0)
    {
      if ((hdrbuf[26] & 0xFE) == 6) srate = 33000;
      else if ((hdrbuf[26] & 0xFE) == 0x10) srate = 50000;
      else if ((hdrbuf[26] & 0xFE) == 0xf6) srate = 16000;
    }
  original_sample_type = MUS_UNKNOWN_SAMPLE;
  sample_type = MUS_UNKNOWN_SAMPLE;
  data_size = (mus_long_t)((double)data_size / 1.5);
  if (hdrbuf[22] == 0x49)
    {
      loop_modes[0] = 1;
      loop_starts[0] = ((hdrbuf[26] & 1) << 16) + (hdrbuf[25] << 8) + hdrbuf[24];
      loop_ends[0] = loop_starts[0] + ((hdrbuf[29] & 1) << 16) + (hdrbuf[28] << 8) + hdrbuf[27];
    }
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Yamaha SY-85 and SY-99 -------------------------------------
 *
 * more reverse engineering...
 * 0: SY85 (SY80 is SY-99) SY85ALL SY80 SYALL
 * 5: name ("WAVE1")
 * (26 int len)
 * (33: comment or prompt?)
 * data in 16-bit little endian (?)
 */

static int read_sy85_header(const char *filename, int fd)
{
  if ((hdrbuf[4] != ' ') && (hdrbuf[4] != 'A')) return(mus_error(MUS_HEADER_READ_FAILED, "%s: unknown magic number", filename));
  chans = 1; 
  data_location = 1024;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 8000; /* unknown */
  sample_type = MUS_BSHORT; /* not right */
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Kurzweil 2000 -------------------------------------
 * 
 * "PRAM" then header len as big endian int??
 * from krz2tx.c (Mark Lakata)
 */
static int read_kurzweil_2000_header(const char *filename, int fd)
{
  chans = 1; 
  data_location = mus_char_to_bint((uint8_t *)(hdrbuf + 4));
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 44100; /* unknown */
  sample_type = MUS_BSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Korg -------------------------------------
 * 
 * "SMP1" -- guessing on the rest
 */
static int read_korg_header(const char *filename, int fd)
{
  chans = 1; 
  data_location = 70;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = mus_char_to_bint((uint8_t *)(hdrbuf + 48));
  sample_type = MUS_BSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Maui -------------------------------------
 * 
 * "Maui" -- guessing on the rest
 */
static int read_maui_header(const char *filename, int fd)
{
  lseek(fd, 420, SEEK_SET);
  if (read(fd, hdrbuf, 64) != 64) return(mus_error(MUS_HEADER_READ_FAILED, "%s truncated maui header?", filename));
  chans = 1; 
  data_location = 776;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 8));
  if ((data_size * 2) > true_file_length)
    data_size = (true_file_length - data_location) / 2;
  srate = mus_char_to_lint((uint8_t *)(hdrbuf));
  sample_type = MUS_LSHORT;
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Impulse Tracker -------------------------------------
 * 
 * data from its2raw.c by Ben Collver
 * 0:  IMPS
 * 4:  filename (12 bytes)
 * 17: global vol
 * 18: flags (1: 16-bit or 8(0), 2: stereo or mono(0)
 * 19: default vol
 * 20: sample name (26 bytes)
 * 46: convert
 * 47: default pan
 * 48: length (samps)
 * 52: loop start
 * 56: loop end
 * 60: srate
 * 64: sustain loop start
 * 68: sustain loop end
 * 72: data location
 * 76: vib speed
 * 77: vib depth
 * 78: vib wave
 * 79: vib rate
 */
static int read_impulsetracker_header(const char *filename, int fd)
{
  if (hdrbuf[18] & 4) chans = 2; else chans = 1;
  if (hdrbuf[18] & 2) sample_type = MUS_LSHORT; else sample_type = MUS_BYTE;
  data_location = mus_char_to_lint((uint8_t *)(hdrbuf + 72));
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = (true_file_length - data_location);
  srate = mus_char_to_lint((uint8_t *)(hdrbuf + 60));
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}

#if 0
/* ------------------------------------ AKAI 3? -------------------------------------
 */
static int read_akai3_header(const char *filename, int fd)
{
  chans = 1;
  data_location = 192;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  if (hdrbuf[1] == 0) srate = 22050; else srate = 44100;
  sample_type = MUS_LSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}
#endif



/* ------------------------------------ AKAI 4 -------------------------------------
 * 
 * 1, 4, info from Paul Kellet -- lost the url ("MPC-2000")
 */
static int read_akai4_header(const char *filename, int fd)
{
  chans = hdrbuf[21] + 1;
  data_location = 42;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = mus_char_to_ulshort((uint8_t *)(hdrbuf + 40));
  sample_type = MUS_LSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ PVF (portable voice format) -------------------------------------
 *
 * info from mgetty-voice-1.1.22/voice/libpvf/lib.c
 * this is a modem-related interchange format
 * 
 * PVF1\n
 * 1 11025 32\n
 * then data
 * PVF1 = binary data, PVF2 = ascii
 * chans | srate | sample size
 */

static int read_pvf_header(const char *filename, int fd)
{
  char *buf;
  int bits, i;
  if (hdrbuf[4] != '\n') return(mus_error(MUS_HEADER_READ_FAILED, "PVF header messed up"));
  type_specifier = mus_char_to_uninterpreted_int((uint8_t *)hdrbuf);
  buf = (char *)(hdrbuf + 5);
  sscanf(buf, "%12d %12d %12d", &chans, &srate, &bits);
  if (chans < 1) chans = 1;
  if (srate < 0) srate = 8000;
  if (bits < 8) bits = 8;
  for (i = 6; i < INITIAL_READ_SIZE; i++)
    if (hdrbuf[i] == '\n')
      {
	data_location = i + 1;
	break;
      }
  if (data_location == 0)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s PVF header bad data location", filename));
  if (match_four_chars((uint8_t *)hdrbuf, I_PVF2))
    {
      sample_type = MUS_UNKNOWN_SAMPLE; /* ascii text */
      return(mus_error(MUS_HEADER_READ_FAILED, "%s PVF header unknown sample type", filename));
    }
  /* big endian data -- they're using htonl etc */
  if (bits == 8)
    sample_type = MUS_BYTE;
  else
    if (bits == 16)
      sample_type = MUS_BSHORT;
    else sample_type = MUS_BINT;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Ultratracker WaveSample -------------------------------------
 *
 * 0..31: name (32 = ctrl-Z?) 
 * 33: PMUWFD (but docs say this is "dos name" -- perhaps we can't recognize this header type reliably)
 * 44: 4 ints giving loop and size data
 * 60: vol
 * 61: "bidi" 0|8|24->8 bit else 16 -- but actual example has 0 with 16-bit
 * 62: finetune
 * 64: data (or 68?)
 * described in Ult-form.txt http://www.wotsit.org/music.htm 
 */

static int read_ultratracker_header(const char *filename, int fd)
{
  chans = 1; 
  data_location = 64;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  data_size = (true_file_length - data_location);
  srate = 8000;
  sample_type = MUS_LSHORT;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Sample dump exchange -------------------------------------
 *
 * 0: SDX:
 * sdx2tx.c (Mark Lakata) reads from 4 for 26 (^z), then
 * version (1)
 * comment as pascal-style string (byte len, bytes chars)
 * then 23 bytes:
 *  0: packing (0 = pcm)
 *  1: midi channel
 *  2 + 256*[3]: sample number
 *  4: sample type (15: 16 bit unsigned(?), 8: 8bit unsigned(?)
 *  5: sample rate (big int?)
 *  9: sample length
 * 13: loop start
 * 17: loop end
 * 21: loop type 
 * 22: reserved
 */

static int read_sample_dump_header(const char *filename, int fd)
{
  int i, len;
  for (i = 4; i < HDRBUFSIZ; i++) if (hdrbuf[i] == 26) break;
  len = hdrbuf[i + 2];
  if (len > 0)
    {
      comment_start = i + 3;
      comment_end = i + 3 + len;
      }
  seek_and_read(fd, (uint8_t *)hdrbuf, i + 3 + len, HDRBUFSIZ);
  srate = mus_char_to_lint((uint8_t *)(hdrbuf + 5));
  loop_modes[0] = 0;
  if (hdrbuf[21] == 0)
    {
      loop_modes[0] = 1;
      loop_starts[0] = mus_char_to_lint((uint8_t *)(hdrbuf + 13));
      loop_ends[0] = mus_char_to_lint((uint8_t *)(hdrbuf + 17));
    }
  /* data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 9)); */
  if ((srate < 100) || (srate > 100000)) srate = 8000;
  chans = 1; 
  data_location = i + 3 + len + 23;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  if (hdrbuf[0] == 0)
    sample_type = MUS_ULSHORT;
  else sample_type = MUS_UNKNOWN_SAMPLE;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Digiplayer ST3 -------------------------------------
 *
 * 0: 1 (use 'SCRS' at 76)
 * 1: name
 * 13: nada
 * 14: "paragraph" offset of sample data
 * 16: length in bytes (looks like #samples in the actual files...)
 * 20: loop start
 * 24: loop end
 * 28: vol
 * 29: ?
 * 30: 0 = unpacked, 1 = DP30ADPCM
 * 31: bits: 0 = loop, 1 = stereo (chans not interleaved!), 2 = 16-bit samples (little endian)
 * 32: freq
 * 36: nada
 * 40: nada
 * 42: 512
 * 44: date?
 * 48: sample name (28 char ASCIZ)
 * 76: 'SCRS'
 * 80: data starts
 *
 * info from http://www.wotsit.org/ S3m-form.txt
 */

static int read_digiplayer_header(const char *filename, int fd)
{
  chans = 1; 
  data_location = 80;
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  srate = 8000;
  sample_type = MUS_ULSHORT;
  if (hdrbuf[30] & 2) chans = 2;
  if (hdrbuf[30] & 1) 
    sample_type = MUS_UNKNOWN_SAMPLE;
  else
    {
      if (hdrbuf[30] & 4) sample_type = MUS_UBYTE; /* may be backwards -- using Convert 1.4 output here */
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ CSRE adf -------------------------------------
 *
 * Info from Stuart Rosen
 *
 * 0-7: CSRE40
 * 8:   samples in file (long)
 * 12:  center line(?) (long)
 * 16:  start channel(?) (unsigned)
 * 18:  bits -- 12 or 16 (unsigned) -- is 12 bit sample file packed?
 * 20:  number system (0 = signed, 1 = unsigned)
 * 22:  srate in kHz (float)
 * 26:  peak sample in file (long) (can be 0)
 * 30-511: comment possibly
 *
 * probably always little-endian (S.R. reads each sample using sizeof(int) -> 16 bits I think)
 * if 12-bit unsigned we need to handle the offset somewhere
 */

static int read_adf_header(const char *filename, int fd)
{
  int bits, numsys;
  lseek(fd, 0, SEEK_SET);
  if ((hdrbuf[4] != '4') || (hdrbuf[5] != '0')) return(mus_error(MUS_HEADER_READ_FAILED, "%s csre header bad magic number", filename));
  if (read(fd, hdrbuf, 30) != 30) return(mus_error(MUS_HEADER_READ_FAILED, "%s csre header truncated?", filename));
  chans = 1;
  numsys = mus_char_to_ulshort((uint8_t *)(hdrbuf + 20));
  bits = mus_char_to_ulshort((uint8_t *)(hdrbuf + 18));
  if ((bits == 16) || (bits == 12))
    {
      if (numsys == 0)
	sample_type = MUS_LSHORT;
      else sample_type = MUS_ULSHORT;
    }
  else sample_type = MUS_UNKNOWN_SAMPLE;
  srate = (int)(1000 * mus_char_to_lfloat((uint8_t *)(hdrbuf + 22)));
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 8));
  data_location = 512;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  if (data_size > mus_bytes_to_samples(sample_type, true_file_length - data_location))
    data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Diamondware -------------------------------------
 * 
 * info from Keith Weiner at DiamondWare (www.dw.com):
 *
 * 0-22:   DWD Header Byte "DiamondWare Digitized\n\0" 
 * 23:     1A (EOF to abort printing of file) 
 * 24:     Major version number 
 * 25:     Minor version number 
 * 26-29:  Unique sound ID (checksum XOR timestamp) 
 * 30:     Reserved 
 * 31:     Compression type (0 = none) 
 * 32-33:  Sampling rate (in Hz) 
 * 34:     Number of channels (1 = mono, 2 = stereo) (interleaved)
 * 35:     Number of bits per sample (8, 16) (all data signed)
 * 36-37:  Absolute value of largest sample in file 
 * 38-41:  length of data section (in bytes) 
 * 42-45:  # samples (16-bit stereo is 4 bytes/sample) 
 * 46-49:  Offset of data from start of file (in bytes) 
 * 50-53:  Reserved for future expansion (markers) 
 * 54-55:  Padding 
 * 56:offset -- additional text: field = value
 *  suggested fields: TITLE, ORGARTIST, GENRE, KEYWORDS, ORGMEDIUM, EDITOR, DIGITIZER, COMMENT, SUBJECT, COPYRIGHT, SOFTWARE, CREATEDATE
 *
 * since this is all Windows/DOS oriented, I'll assume little-endian byte order.
 */

static int read_diamondware_header(const char *filename, int fd)
{
  chans = hdrbuf[34];
  if (hdrbuf[31] == 0)
    {
      if (hdrbuf[35] == 8) sample_type = MUS_BYTE;
      else sample_type = MUS_LSHORT;
    }
  else 
    {
      sample_type = MUS_UNKNOWN_SAMPLE;
      return(mus_error(MUS_HEADER_READ_FAILED, "%s unknown sample type", filename));
    }
  srate = mus_char_to_ulshort((uint8_t *)(hdrbuf + 32));
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + 38));
  data_location = mus_char_to_lint((uint8_t *)(hdrbuf + 46));
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  if (data_size > true_file_length - data_location)
    data_size = true_file_length - data_location;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Ensoniq Paris -------------------------------------
 * _paf -> Ensoniq Paris?  (this info from libaudiofile)
 *  0   paf (or fap)
 *  4  version (0)
 *  8  endianess (0 = big)
 * 12  rate (uint32_t)
 * 16  format (0: 16-bit linear, 24-bit linear)
 * 20  channels
 * 24  source (an encoded comment)
 * 2048 data (24 bit files are compressed)
 */

static int read_paf_header(const char *filename, int fd)
{
  int form;
  bool little = false;
  sample_type = MUS_UNKNOWN_SAMPLE;
  if (mus_char_to_bint((uint8_t *)(hdrbuf + 8))) little = true;
  if (little)
    {
      srate = mus_char_to_ulint((uint8_t *)(hdrbuf + 12));
      form = mus_char_to_ulint((uint8_t *)(hdrbuf + 16));
      if (form == 0) sample_type = MUS_LSHORT;
      if (form == 2) sample_type = MUS_BYTE;
      chans = mus_char_to_ulint((uint8_t *)(hdrbuf + 20));
    }
  else
    {
      srate = mus_char_to_ubint((uint8_t *)(hdrbuf + 12));
      form = mus_char_to_ubint((uint8_t *)(hdrbuf + 16));
      if (form == 0) sample_type = MUS_BSHORT;
      if (form == 2) sample_type = MUS_BYTE;
      chans = mus_char_to_ubint((uint8_t *)(hdrbuf + 20));
    }
  data_location = 2048;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (true_file_length < data_location)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_location %" print_mus_long " > file length: %" print_mus_long, filename, data_location, true_file_length));
  if (sample_type != MUS_UNKNOWN_SAMPLE) 
    data_size = mus_bytes_to_samples(sample_type, true_file_length - 2048);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Comdisco SPW -------------------------------------
 * info from AFsp libtsp/AF/nucleus/AFgetSWpar.c
 *
 * header is text as in NIST:
 *
 *   $SIGNAL_FILE 9\n (12 chars)
 *   $USER_COMMENT
 *   <comment line(s)>
 *   $COMMON_INFO
 *   SPW Version        = 3.10
 *   System Type        = <machine> (e.g. "sun4", "hp700")
 *   Sampling Frequency = <Sfreq>   (e.g. "8000")
 *   Starting Time      = 0
 *   $DATA_INFO
 *   Number of points   = <Nsamp>   (e.g. "2000")
 *   Signal Type        = <type>    ("Double", "Float", "Fixed-point", "Integer", "Logical")
 *   Fixed Point Format = <16, 0, t> <16, 16, t> <8, 8, t> <8, 0, t> (optional)
 *   Complex Format     = Real_Imag (optional)
 *   $DATA <data_type>              ("ASCII", "BINARY")
 *
 * the fixed point <n, m, b> is decoded as n = number of bits total per sample, m = integer bits, b = t: signed, u: unsigned
 * if $DATA ASCII, data is ascii text as in IEEE text files.
 * There are other complications as well.  We'll just hack up a stop-gap until someone complains.
 */

static int read_comdisco_header(const char *filename, int fd)
{
  /* need to grab a line at a time, call strcmp over and over.  This is very tedious. */
  char *line;
  int i, j, k, m, n, curend, offset, len, type, d_size = 0;
  bool happy = true, little, commenting;

  k = 15;
  line = (char *)calloc(256, sizeof(char));
  little = false;
  offset = 0;
  type = 0;
  srate = 0;
  curend = INITIAL_READ_SIZE;
  commenting = false;
  while (happy)
    {
      for (i = 0; i < 256; i++)
	{
	  if (k == curend)
	    {
	      offset += curend;
	      if (read(fd, hdrbuf, HDRBUFSIZ) != HDRBUFSIZ) 
		{
		  free(line);
		  return(mus_error(MUS_HEADER_READ_FAILED, "%s comdisco header truncated?", filename));
		}
	      k = 0;
	      curend = HDRBUFSIZ;
	    }
	  if (hdrbuf[k] == '\n') 
	    {
	      k++; 
	      break;
	    }
	  line[i] = hdrbuf[k++];
	}
      line[i] = '\0';
      if ((strcmp(line, "$DATA BINARY") == 0) || 
	  (strcmp(line, "$DATA ASCII") == 0)) 
	{
	  happy = false; 
	  data_location = offset + k;
	}
      if (strcmp(line, "$USER_COMMENT") == 0)
	{
	  comment_start = offset + k;
	  commenting = true;
	}
      else
	{
	  if (commenting)
	    {
	      if (line[0] == '$')
		{
		  comment_end = offset + k - 2 - strlen(line);
		  commenting = false;
		}
	    }
	}
      if (line[0] != '$')
	{
	  char portion[32];
	  char value[32];
	  len = strlen(line);
	  for (j = 0; j < 8; j++) 
	    portion[j] = line[j];
	  portion[8] ='\0';
	  for (j = 8; j < len; j++) 
	    if (line[j] == '=') 
	      break;
	  for (n = 0, m = j + 2; m < len; m++, n++) 
	    value[n] = line[m];
	  value[n] ='\0';
	  if (strcmp(portion, "Sampling") == 0) sscanf(value, "%12d", &srate); else
	  if (strcmp(portion, "Number o") == 0) sscanf(value, "%12d", &d_size); else
	  if (strcmp(portion, "Signal T") == 0) {if (value[1] == 'o') type = 2; else if (value[1] == 'l') type = 1;} else
	  if (strcmp(portion, "Fixed Po") == 0) {if (value[1] == '8') type = 3;}
	}
    }

  /* now clean up this mess */
  if (data_location == 0) {free(line); return(mus_error(MUS_HEADER_READ_FAILED, "%s: no $DATA BINARY field?", filename));}
  if (srate == 0) {free(line); return(mus_error(MUS_HEADER_READ_FAILED, "%s: srate == 0", filename));}
  chans = 1;
  if (d_size != 0) data_size = (mus_long_t)d_size;
  switch (type)
    {
    case 0: if (little) sample_type = MUS_LSHORT; else sample_type = MUS_BSHORT; break;
    case 1: if (little) sample_type = MUS_LFLOAT; else sample_type = MUS_BFLOAT; break;
    case 2: if (little) sample_type = MUS_LDOUBLE; else sample_type = MUS_BDOUBLE; break;
    case 3: sample_type = MUS_BYTE; break;
    }
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > mus_bytes_to_samples(sample_type, true_file_length - data_location))
    data_size = mus_bytes_to_samples(sample_type, true_file_length - data_location);
  free(line);
  return(MUS_NO_ERROR);
}



/* ------------------------------------ MS ASF -------------------------------------
 *
 * asf format is described at http://www.microsoft.com/asf/specs.htm
 * http://www.microsoft.com/asf/spec3/ASF0198ps.exe
 *
 * this header is completely insane
 */

static int read_asf_header(const char *filename, int fd)
{
  /* a chunked sample type, so not really acceptable here or elsewhere -- needs to be unchunked */
  int len, ilen = 0, i, j, bits = 0;
  bool asf_huge = false, present;
  /* apparently "huge" has some meaning in Windoze C */

  len = mus_char_to_lint((uint8_t *)(hdrbuf + 16)); /* actually 64 bits */
  i = (128+64) / 8;
  srate = 0;
  chans = 0;

  while (i < len)
    {
      seek_and_read(fd, (uint8_t *)hdrbuf, i, HDRBUFSIZ);
      if ((uint32_t)(hdrbuf[1]) == 0x29) 
	switch (hdrbuf[0])
	  {
	  case 0xd0: 
	    asf_huge = (hdrbuf[((128+64+128+64+64+64+64+32)/8)] & 2);
	    break;

	  case 0xd4: 
	    present = ((hdrbuf[16+8+16+8+8+ 4+4+4+4+ 4+4] >> 3) & 0x3);
	    if (present)
	      j = 16+8+16+8+8+ 4+4+4+4+ 4+4+ 4+ (4+4+4) + 2;
	    else j = 16+8+16+8+8+ 4+4+4+4+ 4+4+ 4+ 2;
	    srate = mus_char_to_lint((uint8_t *)(hdrbuf + j+11+36));
	    bits = mus_char_to_lint((uint8_t *)(hdrbuf + j+11+32));
	    chans = mus_char_to_ulshort((uint8_t *)(hdrbuf + j+65));
	    original_sample_type = mus_char_to_lint((uint8_t *)(hdrbuf + j+11));
	    break;

	  default: 
	    break;
	  }
      ilen = mus_char_to_lint((uint8_t *)(hdrbuf + 16));
      if (ilen <= 0) break;
      if ((chans > 0) && (srate > 0)) break;
      i += ilen;
    }
  i = len;
  seek_and_read(fd, (uint8_t *)hdrbuf, i, HDRBUFSIZ);
  sample_type = MUS_UNKNOWN_SAMPLE;
  if (((uint32_t)(hdrbuf[1]) == 0x29) && ((uint32_t)(hdrbuf[0]) == 0xd2))
    {
      int a_huge = 2;
      ilen = mus_char_to_lint((uint8_t *)(hdrbuf + 16));
      if (asf_huge) a_huge = 4;
      data_location = i + 20 + a_huge + 2+4+3+1;
      if (bits == 0) bits = 8; 
      sample_type = wave_to_sndlib_format(original_sample_type, bits, true);
    }
  else return(mus_error(MUS_HEADER_READ_FAILED, "%s: unknown sample type", filename));
  data_size = ilen - data_location;
  true_file_length = SEEK_FILE_LENGTH(fd);
  if (data_size > true_file_length)
    {
      data_size = true_file_length - data_location;
      if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
    }
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}


/* ------------------------------------ Sox -------------------------------------
 *  sox now has its own format (intended as an intermediate file)
 *    0: .SoX or .XoS
 *    4: header_bytes (int)
 *    8: num_samples (int64_t)
 *    16: srate (double)
 *    24: chans (int)
 *    28: comment bytes (int)
 *    32: comment... [if any]
 *    header_bytes: data (always 32-bit int in native format)
 */

static int read_sox_header(const char *filename, int fd)
{
  int comment_len;
  mus_long_t samps;

  if (match_four_chars((uint8_t *)(hdrbuf + 0), I_dSoX))
    {
      sample_type = MUS_LINTN;
      samps = mus_char_to_llong((uint8_t *)(hdrbuf + 8));
      srate = (int)mus_char_to_ldouble((uint8_t *)(hdrbuf + 16));
      little_endian = true;
    }
  else 
    { /* untested */
      sample_type = MUS_BINTN;
      samps = mus_char_to_blong((uint8_t *)(hdrbuf + 8));
      srate = (int)mus_char_to_bdouble((uint8_t *)(hdrbuf + 16));
      little_endian = false;
    }

  data_location = 4 + big_or_little_endian_int((uint8_t *)(hdrbuf + 4), little_endian);
  chans = big_or_little_endian_int((uint8_t *)(hdrbuf + 24), little_endian);
  comment_len = big_or_little_endian_int((uint8_t *)(hdrbuf + 28), little_endian);
  if (comment_len > 0)
    {
      comment_start = 32;
      comment_end = comment_start + comment_len;
    }
  true_file_length = SEEK_FILE_LENGTH(fd);
  data_size = (true_file_length - data_location);
  if (data_size < 0) return(mus_error(MUS_HEADER_READ_FAILED, "%s: data_size = %" print_mus_long "?", filename, data_size));
  data_size = mus_bytes_to_samples(sample_type, data_size);
  if (samps < data_size) data_size = samps;
  return(MUS_NO_ERROR);
}



/* ------------------------------------ Matlab 5 ------------------------------------- */
/* info from the matlab site, Gary Scavone's site, and wotsit.org "Mat-File Format" in matlab.zip
 *
 * 128 byte header starting with "MATLAB 5.0 MAT-file"
 * 124: version (normally 0x100)
 * 126: endianess (the chars 'M' and 'I')
 * data chunked much like aiff -- 8 bytes: type | number-of-bytes, followed by data
 *   4 bytes type: 1: s8 2: u8 3: s16 4: u16 5: s32 6: u32 
 *                 7: IEEE 754 single 9: IEEE 754 double 12: s64 13: u64 
 *                 14: array (15 compressed?)
 *   4 bytes size: (not including the tag)
 *   array types: 6: double 7:float 
 *
 * I'll assume a sound file is stored as a matlab array
 * and each separate array is a channel (non-interleaved, so if chans>1 we have to pre-translate it).
 *
 */

static int read_matlab_5_header(const char *filename, int fd)
{
  int i, type, loc, size;
  /* bool swapped = false; */
  comment_start = 0;
  comment_end = 124;

  for (i = 1; i < 124; i++)
    if (hdrbuf[i] == 0)
      {
	comment_end = i;
	break;
      }
  
  /* if (mus_char_to_lshort((uint8_t *)(hdrbuf + 126)) == 0x494d) swapped = true; */
  /* byte swapping not handled yet */

  type = mus_char_to_lint((uint8_t *)(hdrbuf + 128));
  if (type != 14)
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: unknown Matlab sample type (%d)", filename, type));

  /* chunksize = mus_char_to_lint((uint8_t *)(hdrbuf + 132)); */

  /* now grovel through the array header */
  /*   assume the "flags" are not interesting */
  /*   assume "dimensions" = number of samples, but we'll pick that up from the data size */
  /*   do we care about the original variable name? */

  /* should we try to handle multiple signals in a file?  as separate channels? */

  loc = 136 + 32 + 8;
  size = mus_char_to_lint((uint8_t *)(hdrbuf + 172)); /* var name len -- need to round up to multiple of 8 */
  i = size % 8;
  if (i == 0)
    loc += size;
  else loc += size + 8 - i;
  
  type = mus_char_to_lint((uint8_t *)(hdrbuf + loc));
  if (type == 9)
    sample_type = MUS_LDOUBLE;
  else sample_type = MUS_LFLOAT;
  data_size = mus_char_to_lint((uint8_t *)(hdrbuf + loc + 4));
  
  chans = 1;
  srate = 44100; /* a plausible guess */

  true_file_length = SEEK_FILE_LENGTH(fd);
  return(MUS_NO_ERROR);
}




/* ------------------------------------ no header ------------------------------------- */

static int raw_header_srate = 44100;
static int raw_header_chans = 2;
static mus_sample_t raw_header_sample_type = MUS_BSHORT;

static int read_no_header(int fd)
{
  srate = raw_header_srate;
  chans = raw_header_chans;
  sample_type = raw_header_sample_type;
  data_location = 0;
  data_size = SEEK_FILE_LENGTH(fd);
  true_file_length = data_size;
  data_size = mus_bytes_to_samples(sample_type, data_size);
  return(MUS_NO_ERROR);
}


void mus_header_set_raw_defaults(int sr, int chn, mus_sample_t samp_type)
{
  if (sr > 0) raw_header_srate = sr;
  if (chn > 0) raw_header_chans = chn;
  if (mus_is_sample_type(samp_type)) raw_header_sample_type = samp_type;
}


void mus_header_raw_defaults(int *sr, int *chn, mus_sample_t *samp_type)
{
  (*sr) = raw_header_srate;
  (*chn) = raw_header_chans;
  (*samp_type) = raw_header_sample_type;
}



/* ------------------------------------ all together now ------------------------------------ */

static int mus_header_read_1(const char *filename, int fd)
{
  static const uint8_t I_HCOM[4] = {'H','C','O','M'};
  static const uint8_t I_FSSD[4] = {'F','S','S','D'};
  static const uint8_t I_8SVX[4] = {'8','S','V','X'};
  static const uint8_t I_16SV[4] = {'1','6','S','V'};
  static const uint8_t I_VOC1[4] = {'t','i','v','e'};
  static const uint8_t I_Soun[4] = {'S','o','u','n'}; 
  static const uint8_t I_MAUD[4] = {'M','A','U','D'}; 
  static const uint8_t I_mdat[4] = {'m','d','a','t'};  /* quicktime */
  static const uint8_t I_sfbk[4] = {'s','f','b','k'};  /* SoundFont 2.0 */
  static const uint8_t I_ATCH[4] = {'A','T','C','H'}; 
  static const uint8_t I_NAL_[4] = {'N','A','L','_'};
  static const uint8_t I__WAV[4] = {' ','S','A','M'};
  static const uint8_t I_ondW[4] = {'o','n','d','W'};
  static const uint8_t I_SDXc[4] = {'S','D','X',':'};  /* Sample dump exchange format */
  static const uint8_t I_AVI_[4] = {'A','V','I',' '};  /* RIFF AVI */
  static const uint8_t I_ones[4] = {(uint8_t)'\377',(uint8_t)'\377',(uint8_t)'\377',(uint8_t)'\377'};
  static const uint8_t I_zeros[4] = {'\0','\0','\0','\0'};
  static const uint8_t I_asf0[4] = {(uint8_t)'\321','\051',(uint8_t)'\342',(uint8_t)'\326'};
  static const uint8_t I_asf1[4] = {(uint8_t)'\332','\065',(uint8_t)'\321','\021'};
  static const uint8_t I_asf2[4] = {(uint8_t)'\220','\064','\000',(uint8_t)'\240'};
  static const uint8_t I_asf3[4] = {(uint8_t)'\311','\003','\111',(uint8_t)'\276'};
  static const uint8_t I_DS16[4] = {'D','S','1','6'};  /* CSL */
  static const uint8_t I__sam[4] = {'=','s','a','m'};  
  static const uint8_t I_OggS[4] = {'O','g','g','S'};  /* Ogg-related files, apparently -- ogg123 has "vorbis" instead of "Speex" */
  static const uint8_t I_fLaC[4] = {'f','L','a','C'};  /* FLAC */
  static const uint8_t I_TTA1[4] = {'T','T','A','1'};  /* ttaenc */
  static const uint8_t I_wvpk[4] = {'w','v','p','k'};  /* wavpack */
  static const uint8_t I_MATL[4] = {'M','A','T','L'};  /* matlab 5.0 */
  static const uint8_t I_AB_5[4] = {'A','B',' ','5'};  

  #define NINRS	7
  static const uint32_t I_INRS[NINRS] = {0xcb460020, 0xd0465555, 0xfa460000, 0x1c470040, 0x3b470080, 0x7a470000, 0x9c470040};
  static int inrs_srates[NINRS] = {6500, 6667, 8000, 10000, 12000, 16000, 20000};

  /* returns 0 on success (at least to the extent that we can report the header type), -1 for error */
  int i, loc = 0;
  int64_t bytes;

  header_type = MUS_UNKNOWN_HEADER;
  sample_type = MUS_UNKNOWN_SAMPLE;
  comment_start = 0;
  comment_end = 0;
  data_size = 0;
  data_location = 0;

  if (loop_modes)
    {
      loop_modes[0] = 0;
      loop_modes[1] = 0;
    }

  bytes = (int64_t)read(fd, hdrbuf, INITIAL_READ_SIZE);
  /* if it's a 0 length file we need to get out */
  if (bytes < 0) 
    return(mus_error(MUS_HEADER_READ_FAILED, "%s: %s", filename, (errno) ? STRERROR(errno) : "bytes read < 0?"));

  if (bytes == 0) 
    {
      header_type = MUS_RAW;
      srate = raw_header_srate;
      chans = raw_header_chans;
      sample_type = raw_header_sample_type;
      data_location = 0;
      true_file_length = 0;
      return(MUS_NO_ERROR);
    }

  if (bytes < 4) 
    {
      header_type = MUS_RAW;
      return(read_no_header(fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_DSND)) || 
      (match_four_chars((uint8_t *)hdrbuf, I_DECN)))
    {
      if (bytes < 24) 
	return(mus_error(MUS_HEADER_READ_FAILED, "%s NeXT header truncated? found only %" print_mus_long " bytes", filename, bytes));
      header_type = MUS_NEXT;
      return(read_next_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_FORM))
    {
      /* next 4 bytes are apparently the file size or something equally useless */
      if (bytes < 12) 
	return(mus_error(MUS_HEADER_READ_FAILED, "%s AIFF header truncated? found only %" print_mus_long " bytes", filename, bytes));

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_AIFF))
	{ 
	  header_type = MUS_AIFF;
	  return(read_aiff_header(filename, fd, 0));
	}

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_AIFC))
	{ 
	  header_type = MUS_AIFC;
	  return(read_aiff_header(filename, fd, 0));
	}

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_8SVX))
	{
	  header_type = MUS_SVX;
	  return(read_8svx_header(filename, fd, true));
	}

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_16SV))
	{
	  header_type = MUS_SVX;
	  return(read_8svx_header(filename, fd, false));
	}

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_MAUD))
	{
	  header_type = MUS_MAUD;
	  return(read_maud_header(filename, fd));
	}

      /* apparently SAMP here -> sampled audio data (?) */

      if (match_four_chars((uint8_t *)(hdrbuf + 4), I_DS16))
	{
	  header_type = MUS_CSL;
	  return(read_csl_header(filename, fd));
	}
      return(mus_error(MUS_HEADER_READ_FAILED,	"%s: unrecognized \"FORM\" (i.e. AIFF) header",	filename));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_RIFF)) ||
      (match_four_chars((uint8_t *)hdrbuf, I_RIFX)))
    {
      if (bytes < 12) 
	return(mus_error(MUS_HEADER_READ_FAILED, "%s RIFF header truncated? found only %" print_mus_long " bytes", filename, bytes));

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_WAVE))
	{
	  header_type = MUS_RIFF;
	  return(read_riff_header(filename, fd));
	}

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_sfbk))
	{
	  header_type = MUS_SOUNDFONT;
	  return(read_soundfont_header(filename, fd));
	}

      if (match_four_chars((uint8_t *)(hdrbuf + 8), I_AVI_))
	{
	  header_type = MUS_AVI;
	  return(read_avi_header(filename, fd));
	}

      return(mus_error(MUS_HEADER_READ_FAILED, "%s: unrecognized \"RIFF\" (i.e. 'wave') header", filename));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_RF64))
    { 
      header_type = MUS_RF64;
      if (bytes < 28) 
	return(mus_error(MUS_HEADER_READ_FAILED, "%s RF64 header truncated? found only %" print_mus_long " bytes", filename, bytes));
      if ((mus_char_to_lint((uint8_t *)(hdrbuf + 4)) != -1) ||
	  (!(match_four_chars((uint8_t *)(hdrbuf + 8), I_WAVE))))
	return(mus_error(MUS_HEADER_READ_FAILED, "%s: messed up RF64 header", filename));
      return(read_rf64_header(filename, fd));
    }

  if ((equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_VAX)) || 
      (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_SUN)) ||
      (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_MIPS)) || 
      (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_NEXT)))
    {
      if (bytes < 24) 
	return(mus_error(MUS_HEADER_READ_FAILED, "%s IRCAM header truncated? found only %" print_mus_long " bytes", filename, bytes));
      header_type = MUS_IRCAM;
      return(read_ircam_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_NIST))
    {
      header_type = MUS_NIST;
      return(read_nist_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_caff))
    {
      if (bytes < 32) /* INITIAL_READ_SIZE */
	return(mus_error(MUS_HEADER_READ_FAILED, "%s CAFF header truncated? found only %" print_mus_long " bytes", filename, bytes));
      header_type = MUS_CAFF;
      return(read_caff_header(fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_MATL)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_AB_5)))
    {
      if (bytes < 128) /* INITIAL_READ_SIZE=256 */
	return(mus_error(MUS_HEADER_READ_FAILED, "%s Matlab header truncated? found only %" print_mus_long " bytes", filename, bytes));
      header_type = MUS_MATLAB;
      return(read_matlab_5_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SOUN))
    {
      static const uint8_t I_D_SA[4] = {'D',' ','S','A'};
      static const uint8_t I_MPLE[4] = {'M','P','L','E'};
      if ((match_four_chars((uint8_t *)(hdrbuf + 4), I_D_SA)) && 
	  (match_four_chars((uint8_t *)(hdrbuf + 8), I_MPLE)))
	{
	  header_type = MUS_SMP;
	  return(read_smp_header(filename, fd));
	}
      else
	{
	  header_type = MUS_SNDT;
	  return(read_sndt_header(filename, fd));
	}
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_dSoX)) ||
      (match_four_chars((uint8_t *)hdrbuf, I_XoSd)))
    {
      header_type = MUS_SOX;
      return(read_sox_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_VOC0)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_VOC1)))
    {
      if (bytes < 24) 
	return(mus_error(MUS_HEADER_READ_FAILED, "%s VOC header truncated? found only %" print_mus_long " bytes", filename, bytes));
      header_type = MUS_VOC;
      return(read_voc_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_AVR_))
    {
      header_type = MUS_AVR;
      return(read_avr_header(filename, fd));
    }

  if (mus_char_to_bshort((uint8_t *)hdrbuf) == 1336)
    {
      header_type = MUS_SD1;
      return(read_sd1_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_ALaw)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_Soun)))
    {
      header_type = MUS_PSION;
      return(read_psion_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_GF1P)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_ATCH)))
    {
      header_type = MUS_GRAVIS;
      return(read_gravis_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_DSIG)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_NAL_)))
    {
      header_type = MUS_COMDISCO;
      return(read_comdisco_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_GOLD)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I__WAV)))
    {
      header_type = MUS_GOLDWAVE;
      return(read_goldwave_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_Diam)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_ondW)))
    {
      header_type = MUS_DIAMONDWARE;
      return(read_diamondware_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SRFS))
    {
      header_type = MUS_SRFS;
      return(read_srfs_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_CSRE))
    {
      header_type = MUS_ADF;
      return(read_adf_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_fLaC))
    {
      header_type = MUS_FLAC;
      return(MUS_NO_ERROR);
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_ajkg))
    {
      header_type = MUS_SHORTEN;
      return(MUS_NO_ERROR);
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_TTA1))
    {
      header_type = MUS_TTA;
      return(MUS_NO_ERROR);
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_wvpk))
    {
      header_type = MUS_WAVPACK;
      return(MUS_NO_ERROR);
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_OggS))
    {
      if ((hdrbuf[29] == 'v') && (hdrbuf[30] == 'o') && (hdrbuf[31] == 'r'))
	header_type = MUS_OGG;
      else
	{
	  if ((hdrbuf[28] == 'S') && (hdrbuf[29] == 'p') && (hdrbuf[30] == 'e'))
	    header_type = MUS_SPEEX;
	}
      return(MUS_NO_ERROR);
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_file)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 4), I__sam)))
    {
      header_type = MUS_FILE_SAMP;
      return(read_file_samp_header(filename, fd));
    }

  if ((hdrbuf[0] == 0xf0) && (hdrbuf[1] == 0x7e) && (hdrbuf[3] == 0x01))
    {
      header_type = MUS_MIDI_SAMPLE_DUMP;
      chans = 1;
      srate = (int)(1.0e9 / (double)((hdrbuf[7] + (hdrbuf[8] << 7) + (hdrbuf[9] << 14))));
      data_size = (hdrbuf[10] + (hdrbuf[11] << 7) + (hdrbuf[12] << 14));
      /* since this file type has embedded blocks, we have to translate it elsewhere */
      return(MUS_NO_ERROR);
    }

  if (mus_char_to_ubint((uint8_t *)hdrbuf) == 0xAAAAAAAA)
    {
      header_type = MUS_MUS10;
      return(MUS_NO_ERROR);
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_SPIB)) || 
      (match_four_chars((uint8_t *)hdrbuf, I_S___)))
    {
      header_type = MUS_IEEE;
      return(MUS_NO_ERROR);
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_riff))
    {
      header_type = MUS_SOUNDFORGE;
      return(read_soundforge_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_PVF1)) || 
      (match_four_chars((uint8_t *)hdrbuf, I_PVF2)))
    {
      header_type = MUS_PVF;
      return(read_pvf_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_MThd))
    {
      header_type = MUS_MIDI;
      return(MUS_ERROR);
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SND_))
    {
      header_type = MUS_SBSTUDIOII;
      return(read_sbstudio_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_FSMt))
    {
      header_type = MUS_FARANDOLE;
      return(read_farandole_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SDXc))
    {
      header_type = MUS_SAMPLE_DUMP;
      return(read_sample_dump_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_DDSF))
    {
      header_type = MUS_DELUSION;
      return(read_delusion_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_LM89))
    {
      header_type = MUS_YAMAHA_TX16W;
      return(read_tx16w_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SY85))
    {
      header_type = MUS_YAMAHA_SY85;
      return(read_sy85_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SY80))
    {
      header_type = MUS_YAMAHA_SY99;
      return(read_sy85_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_PRAM))
    {
      header_type = MUS_KURZWEIL_2000;
      return(read_kurzweil_2000_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SMP1))
    {
      header_type = MUS_KORG;
      return(read_korg_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_Maui))
    {
      header_type = MUS_MAUI;
      return(read_maui_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_IMPS))
    {
      header_type = MUS_IMPULSETRACKER;
      return(read_impulsetracker_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)(hdrbuf + 35), I_UWFD))
    {
      header_type = MUS_ULTRATRACKER;
      return(read_ultratracker_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)(hdrbuf + 76), I_SCRS))
    {
      header_type = MUS_DIGIPLAYER;
      return(read_digiplayer_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_covox))
    {
      header_type = MUS_COVOX;
      return(read_covox_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I__PAF)) ||
      (match_four_chars((uint8_t *)hdrbuf, I_FAP_)))
    {
      header_type = MUS_PAF;
      return(read_paf_header(filename, fd));
    }

  if (match_four_chars((uint8_t *)hdrbuf, I_SDIF))
    {
      header_type = MUS_SDIF;
      return(read_sdif_header(filename, fd));
    }

#if G7XX
  if (match_four_chars((uint8_t *)hdrbuf, I_NVF_))
    {
      header_type = MUS_NVF;
      return(read_nvf_header(filename, fd));
    }
#endif

  if (match_four_chars((uint8_t *)hdrbuf, I_TWIN))
    {
      header_type = MUS_TWINVQ;
      return(read_twinvq_header(filename, fd));
    }

  /* ESPS is either 0x00006a1a or 0x1a6a0000 at byte 16 */
  if (equal_big_or_little_endian((uint8_t *)(hdrbuf + 16), 0x00006a1a))
    {
      header_type = MUS_ESPS;
      return(read_esps_header(filename, fd));
    }

  if ((hdrbuf[252] == 64) && (hdrbuf[253] == 195)) /* #o100 and #o303 */
    {
      header_type = MUS_SPPACK;
      return(read_sppack_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)(hdrbuf + 65), I_FSSD)) && 
      (match_four_chars((uint8_t *)(hdrbuf + 128), I_HCOM)))
    {
      header_type = MUS_HCOM;
      return(MUS_NO_ERROR);
    }

#if MUS_LITTLE_ENDIAN
  if (mus_char_to_uninterpreted_int((uint8_t *)hdrbuf) == 0x01000800)
#else
  if (mus_char_to_uninterpreted_int((uint8_t *)hdrbuf) == 0x00080001)
#endif
    {
      header_type = MUS_ADC;
      return(read_adc_header(filename, fd));
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_ones)) &&
      (match_four_chars((uint8_t *)(hdrbuf + 12), I_FORM)))
    {
      /* possibly an OMF file with an embedded AIFF data file -- this is just a guess... */
      header_type = MUS_OMF;
      return(read_aiff_header(filename, fd, 12));
      /* another (apparently) along these lines is TOC */
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_zeros)) &&
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_mdat)))
    {
      /* possibly quicktime?? */
      header_type = MUS_QUICKTIME;
      return(read_qt_header(filename, fd));
    }

  if ((hdrbuf[0] == 1) && (hdrbuf[1] == 4)) /* name follows --check? */
    {
      header_type = MUS_AKAI4;
      return(read_akai4_header(filename, fd));
    }

#if 0
  if ((hdrbuf[0] == 3) && (hdrbuf[16] == 128))
    {
      header_type = MUS_AKAI4;
      return(read_akai3_header(filename, fd));
    }
#endif

  for (i = 0; i < NINRS; i++) 
    {
      if (equal_big_or_little_endian((uint8_t *)hdrbuf, I_INRS[i]))
	{
	  loc = inrs_srates[i];
	  header_type = MUS_INRS;
	  return(read_inrs_header(filename, fd, loc));
	}
    }

  if ((match_four_chars((uint8_t *)hdrbuf, I_asf0)) &&
      (match_four_chars((uint8_t *)(hdrbuf + 4), I_asf1)) &&
      (match_four_chars((uint8_t *)(hdrbuf + 8), I_asf2)) &&
      (match_four_chars((uint8_t *)(hdrbuf + 12), I_asf3)))
    {
      header_type = MUS_ASF;
      return(read_asf_header(filename, fd));
    }

  /* SMS files start with 767 (4-byte int, apparently in native order) */
  
  /* try to catch mpeg... */
  {
    int len;
    len = strlen(filename);
    if (len > 4)
      {
	if (strcmp((const char *)(filename + len - 4), ".mp3") == 0)
	  {
	    header_type = MUS_MPEG;
	    return(MUS_NO_ERROR);
	  }
      }
  }

  header_type = MUS_RAW;
  return(read_no_header(fd));
}


static int local_error_type = MUS_NO_ERROR;
static char *local_error_msg = NULL;
static mus_error_handler_t *old_error_handler;

static void local_mus_error(int type, char *msg)
{
  local_error_type = type;
  if (local_error_msg) 
    free(local_error_msg);
  if (msg)
    local_error_msg = mus_strdup(msg);
  else local_error_msg = NULL;
}


int mus_header_read(const char *name)
{
  int fd, err = 0;
  fd = mus_file_open_read(name);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "can't open %s: %s", name, STRERROR(errno)));
  old_error_handler = mus_error_set_handler(local_mus_error);
  err = mus_header_read_1(name, fd);
  CLOSE(fd, name);
  mus_error_set_handler(old_error_handler);
  if (err != MUS_NO_ERROR)
    return(mus_error(local_error_type, "%s", local_error_msg)); /* pass error info on up the chain now that we've cleaned up the open file descriptor */
  return(err);
}


static mus_header_write_hook_t *mus_header_write_hook = NULL;

mus_header_write_hook_t *mus_header_write_set_hook(mus_header_write_hook_t *new_hook) 
{
  mus_header_write_hook_t *old_hook;
  old_hook = mus_header_write_hook;
  mus_header_write_hook = new_hook;
  return(old_hook);
}


int mus_header_write(const char *name, mus_header_t type, int in_srate, int in_chans, mus_long_t loc, 
		     mus_long_t size_in_samples, mus_sample_t samp_type, const char *comment, int len)
{
  /* the "loc" arg is a mistake -- just always set it to 0 */

  int fd, err = MUS_NO_ERROR;
  mus_long_t siz;

  fd = mus_file_create(name);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "can't write %s: %s", name, STRERROR(errno)));

  if (mus_header_write_hook)
    (*mus_header_write_hook)(name);

  siz = mus_samples_to_bytes(samp_type, size_in_samples);
  switch (type)
    {
    case MUS_RAW: case MUS_IRCAM: case MUS_NEXT: case MUS_RIFF: case MUS_RF64: case MUS_CAFF:
      break;

    default:
      if (siz > BIGGEST_4_BYTE_SIGNED_INT)
	{
	  err = MUS_BAD_SIZE;
	  siz = BIGGEST_4_BYTE_SIGNED_INT;
	}
      break;
    }

  switch (type)
    {
    case MUS_NEXT:  err = mus_header_write_next_header(fd, in_srate, in_chans, loc, siz, samp_type, comment, len);  break;
    case MUS_AIFC:  err = write_aif_header(fd, in_srate, in_chans, siz, samp_type, comment, len, true);             break;
    case MUS_AIFF:  err = write_aif_header(fd, in_srate, in_chans, siz, samp_type, comment, len, false);            break;
    case MUS_RF64:  err = write_rf64_header(fd, in_srate, in_chans, siz, samp_type, comment, len);                  break;
    case MUS_CAFF:  err = write_caff_header(fd, in_srate, in_chans, siz, samp_type);                                break;
    case MUS_IRCAM: err = write_ircam_header(fd, in_srate, in_chans, samp_type, comment, len);                      break;
    case MUS_NIST:  err = write_nist_header(fd, in_srate, in_chans, siz, samp_type);                                break;

    case MUS_RIFF:  
      err = write_riff_header(fd, in_srate, in_chans, siz, samp_type, comment, len); 
      if (err != MUS_NO_ERROR)
	{
	  CLOSE(fd, name);
	  return(mus_error(err,  "can't write %s header for %s (fd: %d, sample type: %s, srate: %d, chans: %d)",
			   mus_header_type_name(type), 
			   name, fd,
			   mus_sample_type_short_name(samp_type),
			   in_srate, in_chans));
	}
      break;

    case MUS_RAW: 
      data_location = 0; 
      data_size = mus_bytes_to_samples(samp_type, siz);
      srate = in_srate; 
      chans = in_chans; 
      header_type = MUS_RAW;
      sample_type = samp_type;
      break;

    default:
      CLOSE(fd, name);
      return(mus_error(MUS_UNSUPPORTED_HEADER_TYPE,  "can't write %s header for %s", mus_header_type_name(type), name));
      break;
    }

  CLOSE(fd, name);
  return(err);
}


int mus_write_header(const char *name, mus_header_t type, int in_srate, int in_chans, mus_long_t size_in_samples, mus_sample_t samp_type, const char *comment)
{
  int len = 0;
  if (comment)
    len = strlen(comment);
  return(mus_header_write(name, type, in_srate, in_chans, 0, size_in_samples, samp_type, comment, len));
}


int mus_header_change_data_size(const char *filename, mus_header_t type, mus_long_t size) /* in bytes */
{
  /* the read header at sample update (sound-close) time could be avoided if the
   *   ssnd_location (etc) were saved and passed in -- perhaps an added optimized
   *   header change data size?  Means saving the relevant data, and exporting it
   *  from headers.c. Can we guarantee consistency here?
   */

  int fd, err = MUS_NO_ERROR;
  switch (type)
    {
    case MUS_AIFF:
    case MUS_AIFC:
    case MUS_NIST:
    case MUS_RIFF:
    case MUS_RF64:
      err = mus_header_read(filename);
      break;
    default:
      break;
    }
  if (err == MUS_ERROR) return(err);

  fd = mus_file_reopen_write(filename);
  if (fd == -1) return(mus_error(MUS_HEADER_WRITE_FAILED, "%s: %s", filename, STRERROR(errno)));

  if (size < 0) 
    {
      CLOSE(fd, filename);
      return(mus_error(MUS_BAD_SIZE, "%s: change size to %" print_mus_long "?", filename, size));
    }

  switch (type)
    {
    case MUS_NEXT: 
      if (size > (mus_long_t)(BIGGEST_4_BYTE_UNSIGNED_INT))
	{
	  err = MUS_BAD_SIZE;
	  size = BIGGEST_4_BYTE_UNSIGNED_INT;
	}
      lseek(fd, 8L, SEEK_SET);
      mus_bint_to_char((uint8_t *)(hdrbuf + 0), (uint32_t)size);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_AIFC: 
    case MUS_AIFF: 
      /* we apparently have to make sure the form size and the data size are correct 
       * assumed here that we'll only be updating our own AIFF files 
       * There are 3 such locations -- the second word of the file which is the overall form size, 
       * the framples variable in the COMM chunk, and the chunk-size variable in the SSND chunk 
       * an unexpected hassle for CLM is that we can open/close the output file many times if running mix,
       * so we have to update the various size fields taking into account the old size 
       */
      /* read sets current update_form_size, data_size, sample_type, update_framples_location, update_ssnd_location */
      if (size > BIGGEST_4_BYTE_SIGNED_INT)
	{
	  err = MUS_BAD_SIZE;
	  mus_print("%s size: %" print_mus_long " is too large for %s headers", filename, size, mus_header_type_name(type));
	  size = BIGGEST_4_BYTE_SIGNED_INT;
	}
      lseek(fd, 4L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, (int)size + update_form_size - mus_samples_to_bytes(sample_type, data_size));
      /* cancel old data_size from previous possible write */
      header_write(fd, hdrbuf, 4);
      lseek(fd, update_framples_location, SEEK_SET);
      mus_ubint_to_char((uint8_t *)hdrbuf, (uint32_t)size / (chans * mus_bytes_per_sample(sample_type)));
      header_write(fd, hdrbuf, 4);
      lseek(fd, update_ssnd_location, SEEK_SET);
      mus_ubint_to_char((uint8_t *)hdrbuf, (uint32_t)size + 8);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_RIFF: 
      /* read sets current update_form_size, sample_type, data_size, update_ssnd_location */
      if (size > BIGGEST_4_BYTE_SIGNED_INT)
	{
	  CLOSE(fd, filename);
	  return(mus_header_convert_riff_to_rf64(filename, size));
	}
      lseek(fd, 4L, SEEK_SET);
      mus_lint_to_char((uint8_t *)hdrbuf, (uint32_t)size + update_form_size - mus_samples_to_bytes(sample_type, data_size)); 
      header_write(fd, hdrbuf, 4);
      lseek(fd, update_ssnd_location, SEEK_SET);
      mus_ulint_to_char((uint8_t *)hdrbuf, (uint32_t)size);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_RF64:
      /* read sets current update_form_size, sample_type, data_size, update_ssnd_location, update_rf64_location */
      /* assume here that this really is an rf64 file (i.e. -1 for form size and data chunk size */
      lseek(fd, update_rf64_location, SEEK_SET);
      mus_llong_to_char((uint8_t *)hdrbuf, data_location + size - 8); /* 25-June-07 */
      mus_llong_to_char((uint8_t *)(hdrbuf + 8), size);
      mus_llong_to_char((uint8_t *)(hdrbuf + 16), size);
      header_write(fd, hdrbuf, 24);
      break;

    case MUS_IRCAM: 
    case MUS_RAW:
      /* size is implicit in file size */
      break;

    case MUS_NIST: 
      /* read sets current srate, chans, sample_type */
      if (size > BIGGEST_4_BYTE_SIGNED_INT)
	{
	  err = MUS_BAD_SIZE;
	  mus_print("%s size: %" print_mus_long " is too large for %s headers", filename, size, mus_header_type_name(type));
	  size = BIGGEST_4_BYTE_SIGNED_INT;
	}
      lseek(fd, 0L, SEEK_SET);
      write_nist_header(fd, mus_header_srate(), mus_header_chans(), size, mus_header_sample_type());
      break;

    case MUS_CAFF:
      if (update_framples_location < 56) update_framples_location = 56;
      lseek(fd, update_framples_location, SEEK_SET);
      mus_blong_to_char((uint8_t *)(hdrbuf + 0), size);
      header_write(fd, hdrbuf, 8);
      break;

    default:
      CLOSE(fd, filename);
      return(mus_error(MUS_UNSUPPORTED_HEADER_TYPE, "mus_header_change_data_size: can't update %s headers", mus_header_type_name(type)));
      break;
    }

  CLOSE(fd, filename);
  return(err);
}


int mus_header_change_chans(const char *filename, mus_header_t type, int new_chans)
{
  int err = MUS_NO_ERROR, fd;
  mus_long_t new_framples;
  switch (type)
    {
    case MUS_AIFF:
    case MUS_AIFC:
    case MUS_NIST:
    case MUS_RIFF:
    case MUS_RF64:
      err = mus_header_read(filename);
      break;
    default:
      break;
    }
  if (err == MUS_ERROR) return(err);
  fd = mus_file_reopen_write(filename);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "mus_header_change_chans for %s failed: %s", filename, STRERROR(errno)));
  switch (type)
    {
    case MUS_NEXT:
      lseek(fd, 20L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, new_chans);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_IRCAM:
      lseek(fd, 8L, SEEK_SET);
      if (little_endian) 
	mus_lint_to_char((uint8_t *)hdrbuf, new_chans);
      else mus_bint_to_char((uint8_t *)hdrbuf, new_chans);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_NIST:
      lseek(fd, 0L, SEEK_SET);
      write_nist_header(fd, srate, new_chans, mus_bytes_per_sample(sample_type) * data_size, sample_type);
      /* header size is always 1024, so this is safe */
      break;

    case MUS_AIFF: case MUS_AIFC:
      lseek(fd, update_framples_location - 2, SEEK_SET);
      new_framples = data_size / new_chans;
      mus_bshort_to_char((uint8_t *)hdrbuf, new_chans);
      mus_bint_to_char((uint8_t *)(hdrbuf + 2), new_framples);
      header_write(fd, hdrbuf, 6);
      break;

    case MUS_RIFF:
    case MUS_RF64:
      lseek(fd, update_framples_location - 2, SEEK_SET);
      if (little_endian)
	mus_lshort_to_char((uint8_t *)hdrbuf, new_chans);
      else mus_bshort_to_char((uint8_t *)hdrbuf, new_chans);
      header_write(fd, hdrbuf, 2);
      break;

    case MUS_CAFF:
      lseek(fd, 44L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, new_chans);
      header_write(fd, hdrbuf, 4);
      /* we should probably also change bytes_per_packet at 36, but... */
      break;

    default: break;
    }
  CLOSE(fd, filename);
  return(err);
}


int mus_header_change_srate(const char *filename, mus_header_t type, int new_srate)
{
  int err = MUS_NO_ERROR, fd;
  switch (type)
    {
    case MUS_AIFF:
    case MUS_AIFC:
    case MUS_NIST:
    case MUS_RIFF:
    case MUS_RF64:
      err = mus_header_read(filename);
      break;
    default:
      break;
    }
  if (err == MUS_ERROR) return(err);
  fd = mus_file_reopen_write(filename);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "mus_header_change_srate for %s failed: %s", filename, STRERROR(errno)));
  switch (type)
    {
    case MUS_NEXT:
      lseek(fd, 16L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, new_srate);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_IRCAM:
      lseek(fd, 4L, SEEK_SET);
      if (little_endian) 
	mus_lfloat_to_char((uint8_t *)hdrbuf, (float)new_srate);
      else mus_bfloat_to_char((uint8_t *)hdrbuf, (float)new_srate);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_NIST:
      lseek(fd, 0L, SEEK_SET);
      write_nist_header(fd, new_srate, chans, mus_bytes_per_sample(sample_type) * data_size, sample_type);
      break;

    case MUS_AIFF: 
    case MUS_AIFC:
      lseek(fd, update_framples_location + 6, SEEK_SET);
      double_to_ieee_80((double)new_srate, (uint8_t *)hdrbuf);
      header_write(fd, hdrbuf, 10);
      break;

    case MUS_RIFF:
    case MUS_RF64:
      lseek(fd, update_framples_location, SEEK_SET);
      if (little_endian)
	mus_lint_to_char((uint8_t *)hdrbuf, new_srate);
      else mus_bint_to_char((uint8_t *)hdrbuf, new_srate);
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_CAFF:
      lseek(fd, 20, SEEK_SET);
      mus_bdouble_to_char((uint8_t *)hdrbuf, (double)new_srate);      
      header_write(fd, hdrbuf, 8);
      break;

    default: break;
    }
  CLOSE(fd, filename);
  return(err);
}


int mus_header_change_type(const char *filename, mus_header_t new_type, mus_sample_t new_format)
{
  int err;
  /* open temp, write header, copy data, replace original with temp */
  err = mus_header_read(filename);
  if (err == MUS_NO_ERROR)
    {
      if (header_type != new_type)
	{
	  int ofd, ifd, len;
	  int64_t nbytes;
	  mus_long_t loc;
	  uint8_t *buf = NULL;
	  char *new_file, *comment = NULL;

	  if ((header_type == MUS_RIFF) && 
	      (new_type == MUS_RF64))
	    return(mus_header_convert_riff_to_rf64(filename, data_size));

	  len = strlen(filename) + 5;
	  new_file = (char *)malloc(len * sizeof(char));
	  snprintf(new_file, len, "%s.tmp", filename);
	  loc = mus_header_data_location();
	  if (new_type != MUS_RAW)
	    {
	      if (comment_end > comment_start)
		{
		  mus_long_t clen;
		  clen = comment_end - comment_start + 1;
		  comment = (char *)calloc(clen + 1, sizeof(char));
		  ifd = mus_file_open_read(filename);
		  lseek(ifd, comment_start, SEEK_SET);
		  header_read(ifd, (uint8_t *)comment, clen);
		  CLOSE(ifd, filename);
		}
	      data_size = data_size * mus_bytes_per_sample(sample_type) / mus_bytes_per_sample(new_format);
	      mus_write_header(new_file, new_type, srate, chans, data_size, new_format, comment);
	    }
	  else mus_file_create(new_file);
	  ifd = mus_file_open_read(filename);
	  lseek(ifd, loc, SEEK_SET);
	  ofd = mus_file_reopen_write(new_file);
	  lseek(ofd, 0L, SEEK_END);
	  buf = (uint8_t *)calloc(8192, sizeof(uint8_t));
	  while ((nbytes = read(ifd, buf, 8192))) header_write(ofd, buf, (int)nbytes);
	  CLOSE(ifd, filename);
	  CLOSE(ofd, new_file);
	  free(buf);
	  if (comment) free(comment);
	  rename(new_file, filename);
	  free(new_file);
	}
    }
  return(err);
}


int mus_header_change_sample_type(const char *filename, mus_header_t type, mus_sample_t new_format)
{
  int err = MUS_NO_ERROR, fd, fr;
  mus_long_t old_bytes;
  switch (type)
    {
    case MUS_AIFF:
    case MUS_AIFC:
    case MUS_NIST:
    case MUS_RIFF:
    case MUS_RF64:
      err = mus_header_read(filename);
      break;
    default:
      break;
    }

  if (err == MUS_ERROR) return(err);
  fd = mus_file_reopen_write(filename);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "mus_header_change_sample_type for %s failed: %s", filename, STRERROR(errno)));

  switch (type)
    {
    case MUS_NEXT:
      lseek(fd, 12L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, sndlib_format_to_next(new_format));
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_IRCAM:
      lseek(fd, 12L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, sndlib_format_to_ircam(new_format));
      header_write(fd, hdrbuf, 4);
      break;

    case MUS_NIST:
      lseek(fd, 0L, SEEK_SET);
      write_nist_header(fd, srate, chans, mus_bytes_per_sample(sample_type) * data_size, new_format);
      break;

    case MUS_AIFF: 
    case MUS_AIFC:
      old_bytes = data_size * mus_bytes_per_sample(sample_type);
      lseek(fd, update_framples_location, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, old_bytes / (chans * mus_bytes_per_sample(new_format)));
      mus_bshort_to_char((uint8_t *)(hdrbuf + 4), sndlib_format_to_aiff_bits(new_format));
      header_write(fd, hdrbuf, 6);
      if (header_type == MUS_AIFC)
	{
	  const char *str;
	  str = sndlib_format_to_aifc_name(new_format);
	  lseek(fd, update_framples_location + 16, SEEK_SET);
	  write_four_chars((uint8_t *)(hdrbuf + 0), (const uint8_t *)str);
	  (*(uint8_t *)(hdrbuf + 4)) = 4; /* final pad null not accounted-for */
	  write_four_chars((uint8_t *)(hdrbuf + 5), (const uint8_t *)str);
	  (*(uint8_t *)(hdrbuf + 9)) = 0;
	  header_write(fd, hdrbuf, 10);
	}
      break;

    case MUS_RIFF:
    case MUS_RF64:
      lseek(fd, update_framples_location + 24, SEEK_SET);
      if (little_endian)
	mus_lshort_to_char((uint8_t *)hdrbuf, sndlib_format_to_aiff_bits(new_format));
      else mus_bshort_to_char((uint8_t *)hdrbuf, sndlib_format_to_aiff_bits(new_format));
      header_write(fd, hdrbuf, 2);
      lseek(fd, update_framples_location + 10, SEEK_SET);
      switch (new_format)
	{
	case MUS_MULAW: 
	  fr = 7; 
	  break;

	case MUS_ALAW:  
	  fr = 6; 
	  break;

	case MUS_UBYTE: 
	case MUS_LSHORT: case MUS_L24INT: case MUS_LINT: 
	case MUS_BSHORT: case MUS_B24INT: case MUS_BINT: 
	  fr = 1;
	  break;

	case MUS_LFLOAT: case MUS_LDOUBLE: 
	case MUS_BFLOAT: case MUS_BDOUBLE:
	  fr = 3;
	  break;

	default: fr = 0; break;
	}
      if (little_endian)
	mus_lshort_to_char((uint8_t *)hdrbuf, fr);
      else mus_bshort_to_char((uint8_t *)hdrbuf, fr);
      header_write(fd, hdrbuf, 2);
      break;
      
    default: break;
    }
  CLOSE(fd, filename);
  return(err);
}


int mus_header_change_location(const char *filename, mus_header_t type, mus_long_t new_location)
{
  /* only Next/Sun changeable in this regard */
  int err = MUS_NO_ERROR, fd;
  fd = mus_file_reopen_write(filename);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "mus_header_change_location for %s failed: %s", filename, STRERROR(errno)));
  if (type == MUS_NEXT)
    {
      lseek(fd, 4L, SEEK_SET);
      mus_bint_to_char((uint8_t *)hdrbuf, new_location);
      header_write(fd, hdrbuf, 4);
    }
  CLOSE(fd, filename);
  return(err);
}


int mus_header_change_comment(const char *filename, mus_header_t type, const char *new_comment)
{
  int err;
  err = mus_header_read(filename);
  if (err == MUS_NO_ERROR)
    {
      int fd;
      bool need_ripple = false;
      switch (type)	  
	{
	case MUS_IRCAM:
	  {
	    int len = 0;
	    fd = mus_file_reopen_write(filename);
	    lseek(fd, 16L, SEEK_SET);
	    if (new_comment) len = strlen(new_comment);
	    write_ircam_comment(fd, new_comment, len);
	    CLOSE(fd, filename);
	  }
	  break;

	case MUS_NEXT:
	  fd = mus_file_reopen_write(filename);
	  lseek(fd, 24L, SEEK_SET);
	  if (!new_comment)
	    write_next_comment(fd, new_comment, 0, data_location); /* erase old possibly */
	  else
	    {
	      if ((comment_start != comment_end) && 
		  ((int)(data_location - 24) >= (int)strlen(new_comment)))
		write_next_comment(fd, new_comment, strlen(new_comment), data_location); /* there's room to overwrite old comment */
	      else need_ripple = true;
	    }
	  CLOSE(fd, filename);
	  break;

	default:
	  need_ripple = true;
	  break;
	}
      if (need_ripple)
	{
	  /* open temp, write header, copy data, replace original with temp */
	  char *new_file;
	  int ofd, ifd, len;
	  mus_long_t loc;
	  int64_t nbytes;
	  uint8_t *buf;
	  len = strlen(filename) + 5;
	  new_file = (char *)malloc(len * sizeof(char));
	  snprintf(new_file, len, "%s.tmp", filename);
	  loc = mus_header_data_location();
	  mus_write_header(new_file, header_type, srate, chans, data_size, sample_type, new_comment);
	  ifd = mus_file_open_read(filename);
	  lseek(ifd, loc, SEEK_SET);
	  ofd = mus_file_reopen_write(new_file);
	  lseek(ofd, 0L, SEEK_END);
	  buf = (uint8_t *)calloc(8192, sizeof(uint8_t));
	  while ((nbytes = read(ifd, buf, 8192))) header_write(ofd, buf, (int)nbytes);
	  CLOSE(ifd, filename);
	  CLOSE(ofd, new_file);
	  free(buf);
	  rename(new_file, filename);
	  free(new_file);
	}
    }
  return(err);
}


bool mus_header_writable(mus_header_t type, mus_sample_t samp_type) /* MUS_IGNORE_SAMPLE to ignore sample type for this call */
{
  switch (type)
    {
    case MUS_NEXT:
      if (samp_type == MUS_UNKNOWN_SAMPLE) return(false);
      return(true);
      break;

    case MUS_NIST:
      if (samp_type == MUS_IGNORE_SAMPLE) return(true);
      switch (samp_type)
	{
	case MUS_BYTE: case MUS_BSHORT: case MUS_B24INT: case MUS_BINT: 
	case MUS_LSHORT: case MUS_L24INT: case MUS_LINT: 
	  return(true); break;
	default: 
	  return(false); break;
	}
      break;

    case MUS_AIFC: 
      if (samp_type == MUS_IGNORE_SAMPLE) return(true);
      switch (samp_type)
	{
	case MUS_BSHORT: case MUS_B24INT: case MUS_BINT:
	case MUS_BYTE: case MUS_MULAW: case MUS_ALAW:
	case MUS_BFLOAT: case MUS_BDOUBLE: case MUS_UBYTE: case MUS_UBSHORT:
	case MUS_LSHORT: case MUS_L24INT: case MUS_LINT:
	  return(true);
	  break;
	default:
	  return(false);
	  break;
	}
      break;

    case MUS_AIFF:
      if (samp_type == MUS_IGNORE_SAMPLE) return(true);
      switch (samp_type)
	{
	case MUS_BSHORT: case MUS_B24INT: case MUS_BINT: case MUS_BYTE: 
	  return(true);
	  break;
	default: 
	  return(false);
	  break;
	}
      break;

    case MUS_RIFF:
    case MUS_RF64:
      if (samp_type == MUS_IGNORE_SAMPLE) return(true);
      switch (samp_type)
	{
	case MUS_MULAW: case MUS_ALAW: case MUS_UBYTE: case MUS_LFLOAT:
	case MUS_LSHORT: case MUS_L24INT: case MUS_LINT: case MUS_LDOUBLE:
	  return(true);
	  break;
	default: 
	  return(false);
	  break;
	}
      break;

    case MUS_IRCAM:
      if (samp_type == MUS_IGNORE_SAMPLE) return(true);
      switch (samp_type)
	{
	case MUS_MULAW: case MUS_ALAW: case MUS_BSHORT: case MUS_BINT: case MUS_BFLOAT:
	  return(true);
	  break;
	default:
	  return(false);
	  break;
	}
      break;

    case MUS_CAFF:
      if (samp_type == MUS_IGNORE_SAMPLE) return(true);
      switch (samp_type)
	{
	case MUS_MULAW: case MUS_ALAW: case MUS_BYTE:
	case MUS_LFLOAT: case MUS_LSHORT: case MUS_L24INT: case MUS_LINTN: case MUS_LDOUBLE:
	case MUS_BFLOAT: case MUS_BSHORT: case MUS_B24INT: case MUS_BINTN: case MUS_BDOUBLE:
	  return(true);
	  break;
	default:
	  return(false);
	  break;
	}
      break;

    case MUS_RAW:
      return(true);
      break;

    default: 
      return(false); 
      break;
    }
  return(false);
}


static char aifc_format[5];

/* try to give some info on sample types that aren't supported by sndlib */
const char *mus_header_original_sample_type_name(int samp_type, mus_header_t type)
{
  switch (type)
    {
    case MUS_NEXT:
      switch (samp_type)
	{
	case 0: return("unspecified");
	case 8: return("indirect");
	case 9: return("nested");
 	case 10: return("dsp_core");
	case 11: return("dsp_data_8");
	case 12: return("dsp_data_16");
 	case 13: return("dsp_data_24");
	case 14: return("dsp_data_32");
	case 16: return("display"); 
	case 17: return("mulaw_squelch"); 
	case 18: return("emphasized"); 
	case 19: return("compressed"); 	
	case 20: return("compressed_emphasized"); 
	case 21: return("dsp_commands"); 
	case 22: return("dsp_commands_samples");
 	case 23: return("adpcm_g721"); 
	case 24: return("adpcm_g722"); 
	case 25: return("adpcm_g723"); 	
	case 26: return("adpcm_g723_5"); 
	case 28: return("aes"); 
	case 29: return("delat_mulaw_8");
 	}
      break;

    case MUS_AIFC: 
    case MUS_CAFF:
      aifc_format[4] = 0;
#if MUS_LITTLE_ENDIAN
      snprintf(aifc_format, 5, "%c%c%c%c", samp_type & 0xff, (samp_type >> 8) & 0xff, (samp_type >> 16) & 0xff, (samp_type >> 24) & 0xff);
#else
      snprintf(aifc_format, 5, "%c%c%c%c", (samp_type >> 24) & 0xff, (samp_type >> 16) & 0xff, (samp_type >> 8) & 0xff, samp_type & 0xff);
#endif	
      return(aifc_format);
      break;

    case MUS_PVF:
      if (type_specifier == mus_char_to_uninterpreted_int((unsigned const char *)I_PVF2))
	return("ascii text");
      break;

    case MUS_RIFF:
    case MUS_RF64:
      switch (samp_type)
	{
	case 2: return("ADPCM"); 
	case 4: return("VSELP"); 
	case 5: return("IBM_CVSD"); 	
	case 0x10: return("OKI_ADPCM"); 
	case 0x11: return("DVI_ADPCM"); 
	case 0x12: return("MediaSpace_ADPCM"); 	
	case 0x13: return("Sierra_ADPCM"); 
	case 0x14: return("G723_ADPCM"); 
	case 0x15: return("DIGISTD"); 	
	case 0x16: return("DIGIFIX"); 
	case 0x17: return("Dialogic ADPCM"); 
	case 0x18: return("Mediavision ADPCM"); 	
	case 0x19: return("HP cu codec"); 
	case 0x20: return("Yamaha_ADPCM"); 
	case 0x21: return("SONARC"); 	
	case 0x22: return("DSPGroup_TrueSpeech"); 
	case 0x23: return("EchoSC1"); 
	case 0x24: return("AudioFile_AF36"); 	
	case 0x25: return("APTX"); 
	case 0x26: return("AudioFile_AF10"); 
	case 0x27: return("prosody 1612"); 	
	case 0x28: return("lrc"); 
	case 0x30: return("Dolby_Ac2"); 
	case 0x31: return("GSM610"); 	
	case 0x32: return("MSN audio codec"); 
	case 0x33: return("Antext_ADPCM"); 
	case 0x34: return("Control_res_vqlpc"); 	
	case 0x35: return("DIGIREAL"); 
	case 0x36: return("DIGIADPCM"); 
	case 0x37: return("Control_res_cr10"); 	
	case 0x38: return("NMS_VBXADPCM"); 
	case 0x39: return("oland rdac"); 
	case 0x3a: return("echo sc3"); 	
	case 0x3b: return("Rockwell adpcm"); 
	case 0x3c: return("Rockwell digitalk codec"); 
	case 0x3d: return("Xebec"); 	
	case 0x40: return("G721_ADPCM"); 
	case 0x41: return("G728 CELP"); 
	case 0x42: return("MS G723"); 	
	case 0x50: return("MPEG"); 
	case 0x52: return("RT24"); 
	case 0x53: return("PAC"); 	
	case 0x55: return("Mpeg layer 3"); 
	case 0x59: return("Lucent G723"); 
	case 0x60: return("Cirrus"); 	
	case 0x61: return("ESS Tech pcm"); 
	case 0x62: return("voxware "); 
	case 0x63: return("canopus atrac"); 	
	case 0x64: return("G726"); 
	case 0x65: return("G722"); 
	case 0x66: return("DSAT"); 	
	case 0x67: return("DSAT display"); 
	case 0x69: return("voxware "); 
	case 0x70: return("voxware ac8 "); 	
	case 0x71: return("voxware ac10 "); 
	case 0x72: return("voxware ac16"); 
	case 0x73: return("voxware ac20"); 	
	case 0x74: return("voxware rt24"); 
	case 0x75: return("voxware rt29"); 
	case 0x76: return("voxware rt29hw"); 	
	case 0x77: return("voxware vr12 "); 
	case 0x78: return("voxware vr18"); 
	case 0x79: return("voxware tq40"); 	
	case 0x80: return("softsound"); 
	case 0x81: return("voxware tq60 "); 
	case 0x82: return("MS RT24"); 	
	case 0x83: return("G729A"); 
	case 0x84: return("MVI_MVI2"); 
	case 0x85: return("DF G726"); 	
	case 0x86: return("DF GSM610"); 
	case 0x88: return("isaudio"); 
	case 0x89: return("onlive"); 	
	case 0x91: return("sbc24"); 
	case 0x92: return("dolby ac3 spdif"); 
	case 0x97: return("zyxel adpcm"); 	
	case 0x98: return("philips lpcbb"); 
	case 0x99: return("packed"); 
	case 0x100: return("rhetorex adpcm"); 	
	case 0x101: return("Irat"); 
	case 0x102: return("IBM_alaw?"); 
	case 0x103: return("IBM_ADPCM?"); 	
	case 0x111: return("vivo G723"); 
	case 0x112: return("vivo siren"); 
	case 0x123: return("digital g273"); 	
	case 0x200: return("Creative_ADPCM"); 
	case 0x202: return("Creative fastspeech 8"); 
	case 0x203: return("Creative fastspeech 10"); 	
	case 0x220: return("quarterdeck"); 
	case 0x300: return("FM_TOWNS_SND"); 
	case 0x400: return("BTV digital"); 	
	case 0x680: return("VME vmpcm"); 
	case 0x1000: return("OLIGSM"); 
	case 0x1001: return("OLIADPCM"); 	
	case 0x1002: return("OLICELP"); 
	case 0x1003: return("OLISBC"); 
	case 0x1004: return("OLIOPR"); 	
	case 0x1100: return("LH codec"); 
	case 0x1400: return("Norris"); 
	case 0x1401: return("isaudio"); 	
	case 0x1500: return("Soundspace musicompression"); 
	case 0x2000: return("DVM"); 
	}
      break;

    default: break;
    }
  return("unknown"); /* NULL here isn't safe -- Sun segfaults */
}


bool mus_header_no_header(const char *filename)
{
  int fd;
  int64_t bytes;
  bool ok = false;
  fd = mus_file_open_read(filename);
  if (fd == -1) 
    return(mus_error(MUS_CANT_OPEN_FILE, "mus_header: can't open %s: %s", filename, STRERROR(errno)));
  bytes = (int64_t)read(fd, hdrbuf, INITIAL_READ_SIZE);
  CLOSE(fd, filename);
  if (bytes > 4) 
    ok = ((match_four_chars((uint8_t *)hdrbuf, I_DSND)) || 
	  (match_four_chars((uint8_t *)hdrbuf, I_DECN)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_FORM)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_RIFF)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_RIFX)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_RF64)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_caff)) ||
	  (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_VAX)) || 
	  (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_SUN)) ||
	  (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_MIPS)) || 
	  (equal_big_or_little_endian((uint8_t *)hdrbuf, I_IRCAM_NEXT)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_NIST)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SOUN)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_VOC0)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_AVR_)) ||
	  (mus_char_to_bshort((uint8_t *)hdrbuf) == 1336) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_ALaw)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_GF1P)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_DSIG)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_GOLD)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_Diam)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SRFS)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_CSRE)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_file)) ||
	  ((hdrbuf[0] == 0xf0) && (hdrbuf[1] == 0x7e) && (hdrbuf[3] == 0x01)) ||
	  (equal_big_or_little_endian((uint8_t *)(hdrbuf + 16), 0x00006a1a)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SPIB)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_S___)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_riff)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_PVF1)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_PVF2)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_MThd)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SND_)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_FSMt)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_DDSF)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_LM89)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SY85)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SY80)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_PRAM)) ||
	  (match_four_chars((uint8_t *)(hdrbuf + 35), I_UWFD)) ||
	  (match_four_chars((uint8_t *)(hdrbuf + 76), I_SCRS)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_covox)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I__PAF)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_FAP_)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_TWIN)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_IMPS)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SMP1)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_Maui)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_SDIF)) ||
	  (match_four_chars((uint8_t *)hdrbuf, I_ajkg)) 
#if G7XX
	  || (match_four_chars((uint8_t *)hdrbuf, I_NVF_))
#endif
	  );
  return(!ok);
}


void mus_header_set_aiff_loop_info(int *data)
{
  /* include modes */
  if (data)
    {
      loop_starts[0] = data[0];
      loop_ends[0] = data[1];
      loop_starts[1] = data[2];
      loop_ends[1] = data[3];
      base_note = data[4];
      base_detune = data[5];
      loop_modes[0] = data[6];
      loop_modes[1] = data[7];
    }
  else
    {
      loop_modes[0] = 0;
      loop_modes[1] = 0;
    }
}


bool mus_is_header_type(int n)
{
  switch (n)
    {
    case MUS_NEXT: case MUS_AIFC: case MUS_RIFF: case MUS_RF64: case MUS_BICSF: case MUS_NIST: 
    case MUS_INRS: case MUS_ESPS: case MUS_SVX: case MUS_VOC: case MUS_SNDT: case MUS_RAW: case MUS_SOX:
    case MUS_SMP: case MUS_AVR: case MUS_IRCAM: case MUS_SD1: case MUS_SPPACK: case MUS_MUS10: 
    case MUS_HCOM: case MUS_PSION: case MUS_MAUD: case MUS_IEEE: case MUS_MATLAB: case MUS_ADC:
    case MUS_MIDI: case MUS_SOUNDFONT: case MUS_GRAVIS: case MUS_COMDISCO: case MUS_GOLDWAVE: 
    case MUS_SRFS: case MUS_MIDI_SAMPLE_DUMP: case MUS_DIAMONDWARE: case MUS_ADF: case MUS_SBSTUDIOII:
    case MUS_DELUSION: case MUS_FARANDOLE: case MUS_SAMPLE_DUMP: case MUS_ULTRATRACKER: 
    case MUS_YAMAHA_SY85: case MUS_YAMAHA_TX16W: case MUS_DIGIPLAYER: case MUS_COVOX: case MUS_AVI:
    case MUS_OMF: case MUS_QUICKTIME: case MUS_ASF: case MUS_YAMAHA_SY99: case MUS_KURZWEIL_2000: 
    case MUS_AIFF: case MUS_PAF: case MUS_CSL: case MUS_FILE_SAMP: case MUS_PVF: case MUS_SOUNDFORGE: 
    case MUS_TWINVQ: case MUS_AKAI4: case MUS_IMPULSETRACKER: case MUS_KORG: case MUS_CAFF: 
    case MUS_MAUI: case MUS_SDIF: case MUS_OGG: case MUS_FLAC: case MUS_SPEEX: case MUS_MPEG: 
    case MUS_SHORTEN: case MUS_TTA: case MUS_WAVPACK:  
#if G7XX
    case MUS_NVF: 
#endif
      return(true);
      break;
    }
  return(false);
}


bool mus_is_sample_type(int n)
{
  switch (n)
    {
    case MUS_BSHORT: case MUS_MULAW: case MUS_BYTE: case MUS_BFLOAT: case MUS_BINT: case MUS_ALAW: 
    case MUS_UBYTE: case MUS_B24INT: case MUS_BDOUBLE: case MUS_LSHORT: case MUS_LINT: case MUS_LFLOAT: 
    case MUS_LDOUBLE: case MUS_UBSHORT: case MUS_ULSHORT: case MUS_L24INT: case MUS_BINTN: case MUS_LINTN: 
    case MUS_BFLOAT_UNSCALED: case MUS_LFLOAT_UNSCALED: case MUS_BDOUBLE_UNSCALED: case MUS_LDOUBLE_UNSCALED: 
      return(true);
      break;
    }
  return(false);
}


