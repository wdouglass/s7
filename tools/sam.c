/* a samson box emulator */

/* I assume what people really want is a good rendition from their ancient SAM files,
 *   not an exact replica of the Samson box output.  The latter used 12, 14, 20, 24, 28, and 30-bit
 *   fractional and integer fields, which are a pain to deal with when we would rather use doubles.
 *
 *        gcc sam.c -o sam -lm -O2
 *        sam TEST.SAM
 *        -> TEST.wav ("wav" or "riff" header, quad, little-endian float data at box srate)
 *
 * to include a read-data file, convert the old SAD file to a raw file of little-endian floats,
 *   then sam TEST.SAM test.snd
 *
 * here's the Snd code I use to turn quad into stereo and scale the result to .9:
 *

(define* (quad->stereo (snd 0))
  "turn a quad sound into a (new) stereo sound by mixing 4->1 and 3->2"
  (let ((r0 (make-sampler 0 snd 0))
	(r1 (make-sampler 0 snd 1))
	(r2 (make-sampler 0 snd 2))
	(r3 (make-sampler 0 snd 3)))
    (let ((new-snd (new-sound :channels 2 
			      :srate (srate snd) 
			      :size (frames snd) 
			      :header-type (header-type snd) 
			      :sample-type (sample-type snd))))
      (map-channel (lambda (y) 
                     (+ (next-sample r0) (next-sample r3))) 
                   0 (frames snd) new-snd 0)
      (map-channel (lambda (y) 
                     (+ (next-sample r1) (next-sample r2))) 
                   0 (frames snd) new-snd 1)
      (let* ((mx (apply max (maxamp new-snd #t)))
	     (scl (/ 0.9 mx)))
	(map-channel (lambda (y) (* y scl)) 0 (frames snd) new-snd 0)
	(map-channel (lambda (y) (* y scl)) 0 (frames snd) new-snd 1)))))
 *
 *
 * Thanks to Michael McNabb for bug fixes and enhancements!
 * And thanks to Peter Samson for going back to the schematics to answer our questions!
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>


#define TOTAL_SAMPLES -1
/* set TOTAL_SAMPLES to the number of samples you want computed, or -1 to compute all of them */

#define DEFAULT_DESCRIBE_COMMANDS false
#define REPORT_BAD_COMMANDS true
#define FLUSH_BAD_COMMANDS false
#define FLUSH_TRAILING_LINGERS false

static bool describe_commands = DEFAULT_DESCRIBE_COMMANDS;
static int start_describing = -1, stop_describing = -1;
static int dump_patch_at = -1;


#define LDB(Cmd, Size, Position) ((Cmd >> Position) & ((1 << Size) - 1))
#define BIT(Cmd, Position) ((Cmd >> Position) & 1)

#define TWOS_12(N) ((N < (1 << 11)) ? N : ((N & 0x7ff) - (1 << 11)))
#define TWOS_20(N) ((N < (1 << 19)) ? N : ((N & 0x7ffff) - (1 << 19)))
#define TWOS_24(N) ((N < (1 << 23)) ? N : ((N & 0x7fffff) - (1 << 23)))
#define TWOS_28(N) ((N < (1 << 27)) ? N : ((N & 0x7ffffff) - (1 << 27)))
#define TWOS_30(N) ((N < (1 << 29)) ? N : ((N & 0x1fffffff) - (1 << 29)))

#define TWOS_12_TO_DOUBLE(N) ((double)TWOS_12(N) / (double)(1 << 11))
#define TWOS_20_TO_DOUBLE(N) ((double)TWOS_20(N) / (double)(1 << 19))

#define DOUBLE_12(N) ((double)N / (double)(1 << 11))
#define DOUBLE_20(N) ((double)N / (double)(1 << 19))
#define DOUBLE_24(N) ((double)N / (double)(1 << 23))
#define DOUBLE_28(N) ((double)N / (double)(1 << 27))
#define DOUBLE_30(N) ((double)N / (double)(1 << 29))

/* mmm -- slightly more accurate to use 1<<12-1, I think */
#define UNSIGNED_12_TO_DOUBLE(N) ((double)N / (double)((1 << 12) - 1))
#define DOUBLE_TO_TWOS_20(X) ((X >= 0.0) ? (int)(X * (1 << 19)) : (int)((X + 1.0) * (1 << 19)))


#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif


typedef struct {
  int GO, GJ, GK, GN, GM, GP, GQ, GL, GSUM, GFM, GS, GMODE;
  double f_GO, f_GJ, f_GK, f_GM, f_GP, f_GQ, f_GL;
} generator;

typedef struct {
  int M0, M1, L0, L1, MIN, MRM, MSUM, MMODE, MMMMM, T, mult_scl_1, mult_scl_0, o_M0, o_M1; 
  double f_M0, f_M1, f_L0, f_L1, o_f_M0, o_f_M1;
  /* by "2nd multiplication" I think Pete means M0 since it follows M1 so AA -> M0 and BB -> M1 */
} modifier;

typedef struct {
  int P, Z, Y, X, I; /* "I" = table lookup index received from modifier */
  double xd1, xd2; /* mmm - accounts for "extra" hidden delay */
} delay;

#define SUM_MEMORY_SIZE 64
static double gen_outs[SUM_MEMORY_SIZE], gen_ins[SUM_MEMORY_SIZE], mod_outs[SUM_MEMORY_SIZE], mod_ins[SUM_MEMORY_SIZE]; /* "sum memory" */
static double prev_gen_ins[SUM_MEMORY_SIZE], prev_mod_ins[SUM_MEMORY_SIZE]; /* some debugging info */
static double peak_gen_ins[SUM_MEMORY_SIZE], peak_mod_ins[SUM_MEMORY_SIZE];

#define GENERATORS 256
#define MODIFIERS 128
#define DELAYS 32

static generator *gens[GENERATORS];
static modifier *mods[MODIFIERS];
static delay *dlys[DELAYS];

#define DELAY_MEMORY_SIZE 65536
static double delay_memory[DELAY_MEMORY_SIZE];
static float dac_out[4], dac_out_peak[4];

static int tick, pass, DX, processing_ticks, highest_tick_per_pass, samples = 0, srate = 1, total_commands = 0, current_command = 0;

FILE *snd_file = NULL;                /* for now just riff/wave quad, but srate depends on tick setting */
FILE *read_data_file = NULL;
static char *filename = NULL;         /* mmm - Keep SAM filename around */
static char *output_filename = NULL;  /* mmm - And generate matching output file name == <sam name>.wav */

static void start_clean(void)
{
  int i;
  for (i = 0; i < SUM_MEMORY_SIZE; i++)
    {
      gen_outs[i] = 0.0; /* "outs" are this pass */
      gen_ins[i] = 0.0;  /* "ins" are last pass */
      mod_outs[i] = 0.0;
      mod_ins[i] = 0.0;

      prev_mod_ins[i] = 0.0;
      prev_gen_ins[i] = 0.0;
      peak_mod_ins[i] = 0.0;
      peak_gen_ins[i] = 0.0;
    }

  for (i = 0; i < GENERATORS; i++)
    gens[i] = (generator *)calloc(1, sizeof(generator));

  for (i = 0; i < MODIFIERS; i++)
    {
      mods[i] = (modifier *)calloc(1, sizeof(modifier));
      mods[i]->mult_scl_1 = 1;
      mods[i]->mult_scl_0 = 1;
    }

  for (i = 0; i < DELAYS; i++)
    dlys[i] = (delay *)calloc(1, sizeof(delay));

  for (i = 0; i < DELAY_MEMORY_SIZE; i++)
    delay_memory[i] = 0.0;

  for (i = 0; i < 4; i++)
    {
      dac_out[i] = 0.0;
      dac_out_peak[i] = 0.0;
    }

  tick = 0;
  pass = 0;
}


static void all_done(void)
{
  if (snd_file)
    {
      int header_info[1];
      fclose(snd_file);
      snd_file = fopen(output_filename, "r+"); /* mmm */
      fseek(snd_file, 4L, SEEK_SET);
      header_info[0] = 88 + samples * 4 * 4;  /* total data bytes  4 chans, 4 bytes/float */
      fwrite((void *)header_info, 4, 1, snd_file);
      fseek(snd_file, 76L, SEEK_SET);
      header_info[0] = samples * 4 * 4;
      fwrite((void *)header_info, 4, 1, snd_file);
      fclose(snd_file);

      fprintf(stderr, "%s: %dHz, %d samples, %.4f secs", output_filename, srate, samples, (double)samples / (double)srate); /* mmm */
      fprintf(stderr, ", maxamps: %.3f %.3f %.3f %.3f\n", dac_out_peak[0], dac_out_peak[1], dac_out_peak[2], dac_out_peak[3]);
    }
  exit(0);
}


static void dac_write(double data, int chan)
{
  /* during a given pass we accumulate output to the dac */
  dac_out[chan] += (float)(data / 2.0); 
  /* mmm - /2 seems best now that other scalings have been adjusted */
}


/* ---------------------------------------- generator processing ---------------------------------------- */

/*
 * DAJ - Here is JOS's translation into english of the generator processing.
 *
 * Associated with each generator are the following quantities:
 * FrqSwp20  (20 bits) alpha -- oscillator frequency sweep rate
 * OscFrq28  (28 bits) omega -- oscillator frequency
 * OscAng20  (20 bits) theta -- oscillator angle
 * NumCos11  (11 bits) number of cosines to be summed
 * CosScl4   (4 bits) binary scale of cosine or sum of cosines
 * AmpSwp20  (20 bits) delta -- decay rate
 * CurAmp24  (24 bits) phi -- decay exponent
 * AmpOff12  (12 bits) asymptote
 * OutSum6   (6 bits) sum memory address into which output is added
 * FmSum7    (7 bits) sum memory address from which frequency modulation data is taken
 *	    FmSum7 = QAAAAAA
 *	    Q: 0  generator-last-pass quadrant
 *	       1  modifier-last-pass quadrant
 *	     AAAAAA:  sum address within quadrant
 * Gmode10   (10 bits) generator mode
 *	     Gmode10 = RRRREESSSS
 *
 * Processing
 * ----------
 *
 *	Calculations performed for a generator, governed by its
 * mode, proceed as detailed below.
 *
 * 1)  The word in sum memory addressed by FmSum7 is read (20 bits);
 * 	the sum is formed of it and the high-order 20 bits of
 * 	OscFrq28 (call the result FmPhase20).
 * 
 * 2)  If the oscillator side is running, FrqSwp20, right-adjusted with
 * 	sign extended, is added into OscFrq28.
 * 
 * 3)  If the oscillator mode is SIN(J+Fm), FmPhase20 is taken; otherwise OscAng20.
 * 	Call the 20-bit result Phase20, and its high-order 13 bits
 * 	Phase13.
 * 
 * 4)  If the oscillator side is running, FmPhase20 is added into OscAng20.
 * 
 * 5)  If the run mode is WRITEDATA, the word in sum memory addressed by FmSum7
 * 	is sent to the CPU as the next write-data item; if the run
 * 	mode is DACOUT it is sent to the DAC addressed by the low-order
 * 	4 bits of FrqSwp20.
 * 
 * 6)  In oscillator modes other than SIN(K) and SIN(J+Fm), Phase13 is multiplied
 * 	by NumCos11.  Call the low-order 12 bits of the product, with two bits
 * 	equal to 01 appended to the right, the 14-bit result SinAdr.
 * 	In oscillator modes SIN(K) and SIN(J+Fm), SinAdr is the high-order 13
 * 	bits of Phase20, with a bit equal to 1 appended to the right.
 * 
 * 7)  If the oscillator mode is SIN(K) or SIN(J+Fm), pi/2 is taken (the binary
 * 	number 010...0); otherwise Phase13.  Call the result CscAdr.
 * 
 * 8)  In floating point, the product csc (CscAdr) * sin (SinAdr) is
 * 	formed; then converted to fixed point with a scale factor
 * 	of 2**(-CosScl4).  Call the result (13 bits) TblOut13.
 * 
 * 
 * 9)  The result of the oscillator side (13 bits, call it OscOut13) is
 * 	then determined according to the oscillator mode.
 * 	SSSS: SUMCOS 	TblOut13
 * 	      SAWTOOTH 	Phase13 (but 0 when Phase13 is 1000000000000)
 * 	      SQUARE 	-1/2 (on a scale from -1 to +1) if Phase13 is negative,
 * 		   	  else +1/2
 * 	      PULSE 	+1/2 if overflow occured in step 1) or 4) above;
 * 		     	  else 0.
 * 	      SIN(K) 	TblOut13
 * 	      SIN(J+Fm) TblOut13
 * 
 * 10)  The high-order 12 bits of CurAmp24 are taken (call the result CurAmp12).
 * 
 * 11)  If the envelope side is running, AmpSwp20 right-adjusted, sign
 * 	extended, is added into CurAmp24 (overflow dealt with according
 * 	to the run mode).  (The overflow condition is CurAmp24 changing
 * 	sign such that the high-order bit of the resultant CurAmp24 equals
 * 	the sign bit of AmpSwp20.)
 * 
 * 12)  If the envelope mode is 10 or 11, 2**(-CurAmp12) is looked up;
 * 	otherwise CurAmp12 is taken.  Call the resulting 12 bits NewAmp12.
 * 	Scaling is such that if CurAmp12 is 0 then 2**(-CurAmp12) is
 * 	111 111 111 101 binary; if CurAmp12 is 000 100 000 000 binary,
 * 	then 2**(-CurAmp12) is 011 111 111 110.
 * 
 * 13)  If the envelope mode is 01 or 11, NewAmp12 is added to AmpOff12; else
 * 	it is subtracted from AmpOff12.  This creates Env12, the result
 * 	of the envelope side.
 * 
 * 14)  OscOut13 is multiplied by Env12.  If the run mode specifies adding
 * 	into sum memory, the high-order 19 bits of the rounded product,
 * 	right-adjusted with sign extended, are added into the sum
 * 	memory location designated by OutSum6; except that in run mode
 * 	READDATA, the product is added to the next read-data item from the
 * 	CPU and the sum replaces the contents of the sum memory
 * 	location addressed.
 */

#define osc_mode(gmode) (gmode & 0xf)

/*
SSSS: 0100  sum of cosines
      0001  sawtooth
      0010  square
      0011  pulse train
      0000  sin (K)
      1000  sin (J + fm)
*/

#define SUMCOS 4
#define SAWTOOTH 1
#define SQUARE 2
#define PULSE 3
#define SIN_K 0
#define SIN_FM 8


#define osc_env(gmode) ((gmode >> 4) & 0x3)

/*
EE: 00  L - Q
    01  L + Q
    10  L - 2**(-Q)
    11  L + 2**(-Q)
*/

#define L_PLUS_Q 1
#define L_MINUS_Q 0
#define L_MINUS_2_TO_MINUS_Q 2
#define L_PLUS_2_TO_MINUS_Q 3


#define osc_run(gmode) ((gmode >> 6) & 0xf)

static void set_osc_run(int gen, int RRRR)
{
  generator *g;
  if (gen >= GENERATORS) {fprintf(stderr, "gen mode set overflow\n"); gen = 0;}
  g = gens[gen];
  /* RRRREESSSS */
  g->GMODE = (g->GMODE & 0x3f) | (RRRR << 6);

  if (g->GMODE == 3) g->GMODE = 2; /* if write data, send it to the DAC outputs instead */
}

/*				 osc. run?  env. run?  add to sum?
  RRRR:0000 inactive		    no	       no	   no
     0001 pause			    no	       no	   no
     1111 running A		    yes	    yes, sticky	   yes
     1110 running B		    yes	    yes, free;	   yes
					  triggers subseq.
					    on overflow
     1001 wait			    yes	       no	   no
     1101 running C		    yes	    yes, free;	   yes
					    stops and
					  triggers subseq.
					    on overflow
     0111 read data from computer   no	       yes	   yes
     0011 write data to computer    no	       no	   no
     0010 write data to DAC	    no	       no	   no
	   (address in GO)
*/


static bool osc_is_running(int mode)
{
  int RRRR;
  RRRR = osc_run(mode);
  return((RRRR == 15) || (RRRR == 14) || (RRRR == 9) || (RRRR == 13));
}


static bool env_is_running(int mode)
{
  int RRRR;
  RRRR = osc_run(mode);
  return((RRRR == 15) || (RRRR == 14) || (RRRR == 7) || (RRRR == 13));
}


static bool adding_to_sum(int mode)
{
  int RRRR;
  RRRR = osc_run(mode);
  return((RRRR == 15) || (RRRR == 14) || (RRRR == 7) || (RRRR == 13));
}


static bool gen_is_active(generator *g)
{
  return((osc_is_running(g->GMODE)) && (g->GQ != 0) && (g->GJ != 0));
}


static double gen_amp(generator *g)
{
  int emode;
  double Q;

  if (osc_run(g->GMODE) == 0) return(0.0);

  emode = osc_env(g->GMODE);
  if ((emode == L_PLUS_2_TO_MINUS_Q) || 
      (emode == L_MINUS_2_TO_MINUS_Q))
    Q = pow(2.0, -16.0 * g->f_GQ);
  else Q = g->f_GQ;

  if ((emode == L_PLUS_Q) || 
      (emode == L_PLUS_2_TO_MINUS_Q))  
    return(g->f_GL + Q);
  return(g->f_GL - Q);
}


static bool read_data_warned = false;

static void process_gen(int gen)
{
  #define FmSum7    g->GFM
  #define OutSum6   g->GSUM
  #define FrqSwp20  g->f_GO
  #define OscFreq28 g->f_GJ
  #define OscAng20  g->f_GK
  #define NumCos11  g->GN
  #define AmpSwp20  g->f_GP
  #define AmpOff12  g->f_GL
  #define Gmode10   g->GMODE
  #define CurAmp24  g->f_GQ
  #define CosScl4   g->GM
  #define ShiftOut  g->GS

  generator *g;
  double fm, FmPhase20, Phase20, SinAdr, CscAdr, TblOut13, OscOut13, CurAmp12, NewAmp12, Env12, temp;

  g = gens[gen];
  if (osc_run(g->GMODE) == 0) /* inactive */
    return;

  if (osc_run(Gmode10) == 3)
    {
      /* mmm - just ignore write-data generators since everything is being written out anyway */
      return;
    }

  if ((FmSum7 >> 6) == 0)
    fm = gen_ins[FmSum7 & 0x3f];
  else fm = mod_ins[FmSum7 & 0x3f];
  /* fm *= 0.5; */

  FmPhase20 = fm + OscFreq28; 
  
  if (osc_is_running(Gmode10))
    OscFreq28 += (FrqSwp20 / 256.0);     /* right adjusted 20 bit */
  
  if (osc_mode(Gmode10) == SIN_FM) /* sin(J+fm) */
    Phase20 = FmPhase20;
  else Phase20 = OscAng20;

  if (osc_is_running(Gmode10))
    OscAng20 += FmPhase20;

  /* mmm - dac write goes here and does not stop the processing (probably makes no diff) */
  if (osc_run(Gmode10) == 2)
    {
      dac_write(fm, g->GO & 0xf); /* in this case, we need the integer value of GO */
      return;
    }

  /* probably should be osc_mode(Gmode10) == SUMCOS */
  if ((osc_mode(Gmode10) != SIN_K) &&
      (osc_mode(Gmode10) != SIN_FM))
    {
      SinAdr = (Phase20 * NumCos11);           /* was & 0xfff) << 2) + 1 */
      CscAdr = Phase20;
      if (fmod(CscAdr, 1.0) != 0.0)
	temp = sin(M_PI * SinAdr) / sin(M_PI * CscAdr);       /* was (1 << 13)) */
      else temp = (double)NumCos11;
    }
  else 
    {
      SinAdr = Phase20;                        /* was >> 6) | 1 */
      temp = sin(M_PI * SinAdr);
    }

  TblOut13 = temp / (double)(1 << CosScl4);

  switch (osc_mode(Gmode10))
    {
    case SUMCOS: case SIN_K: case SIN_FM:
      OscOut13 = TblOut13;
      break;
      
    case SAWTOOTH:
      OscOut13 = fmod(Phase20, 2.0) - 1.0;
      break;

    case SQUARE:
      if (fmod(Phase20, 2.0) < 1.0) 
	OscOut13 = -0.5;
      else OscOut13 = 0.5;
      break;

    case PULSE:
      /* pulse mode was primarily used for triggered noise */
      if ((OscAng20 >= 2.0) || (OscAng20 < -2.0))
	{
	  OscAng20 = fmod(OscAng20, 2.0);
	  OscOut13 = 0.5;
	}
      else OscOut13 = 0.0;
      break;
    }

  CurAmp12 = CurAmp24;
  
  if (env_is_running(Gmode10))
    {
      double old_amp;
      old_amp = CurAmp24;
      CurAmp24 += (AmpSwp20 / 32.0); /* was 16.0 */  /* mmm - don't know why 32 but it seems to be more accurate than 16 */
      /*
	The envelope side of the generator can be sticky, which means
	that rather than overflow it will stay at the last value it attained
	before it would have overflowed; or it can be free, in which case it
	wraps around.

	Transitions between run modes can be accomplished in various ways.
	1)  A command can output a new GMODE.
	2)  A MISC command can specify "clear all pause bits", which
		will cause any generator in run mode 0001 to change to
		mode 1111.
	3)  A MISC command can specify "clear all wait bits", which
		will cause any generator in run mode 1001 to change to
		mode 1111.
	4)  If the envelope side of a generator in run mode 1101
		overflows, that generator goes to run mode 1001.
	5)  A generator in run mode 1001 will go to run mode 1101 if
		on the same pass the preceding generator (the one
		whose generator number is one less) caused a
		trigger (was in run mode 1110 or 1101 and envelope
		overflowed).
      */
      if ((CurAmp24 > 1.0) || (CurAmp24 < 0.0))  /*  if ((BIT(CurAmp24, 23) != BIT(old_amp, 23)) && (BIT(CurAmp24, 22) == BIT(AmpSwp20, 19))) */
	{
	  /* overflow */
	  if (osc_run(Gmode10) == 15)              /* "running A" */
	    CurAmp24 = old_amp;
	  else
	    {
	      if (osc_run(Gmode10) == 13)          /* "running C" */
		{
		  set_osc_run(gen, 9);
		  if (osc_run(gens[gen + 1]->GMODE) == 9)
		    set_osc_run(gen + 1, 13);
		}
	      else
		{
		  if ((osc_run(Gmode10) == 14) &&  /* "running B" */
		      (osc_run(gens[gen + 1]->GMODE) == 9))
		    set_osc_run(gen + 1, 13);		      
		}
	    }
	}
    }

  if ((osc_env(Gmode10) == L_PLUS_2_TO_MINUS_Q) || 
      (osc_env(Gmode10) == L_MINUS_2_TO_MINUS_Q))
    NewAmp12 = pow(2.0, -16.0 * CurAmp12);
  else NewAmp12 = CurAmp12; /* was / 4 */  /* mmm - no scaling called for here */

  /* I think this matches the spec:
   *    if temp6 is 0, then 2^(-temp6) is 1, the specs say #b111111111101, 
   *       which assuming 12 bit unsigned fractions is 4093/4096,
   *   if temp6 is #b000100000000 (256), 2^(-temp6) is #b011111111110,
   *       which is .5 (fractional) so we really want 2^(-16*temp6) = 2^-1
   */

  /* in the notes: "The scaling involved is a left shift of temp6 by 4 bits".
   *    This scaling matters in FM since it is a multiplier on the index, and in pluck.
   */

  if ((osc_env(Gmode10) == L_PLUS_Q) || 
      (osc_env(Gmode10) == L_PLUS_2_TO_MINUS_Q))  
    Env12 = AmpOff12 + NewAmp12;
  else Env12 = AmpOff12 - NewAmp12;

  OscOut13 *= Env12;
  if (adding_to_sum(Gmode10))
    {
      if (osc_run(Gmode10) != 7)
	{
	  /* "If GS is 0, the high-order 19 bits
	     of the rounded product are taken, right-adjusted with sign
	     extended; if GS is 1, the high-order 20 bits of the rounded
	     product are taken."
	  */
	  if (g->GS == 0)
	    gen_outs[OutSum6] += OscOut13 / 2.0;   /* mmm - right-shifted high order 19 bits so divide by 2 */
	  else gen_outs[OutSum6] += OscOut13;      /* mmm - no shift, so leave value alone */
	}
      else 
	{
	  /* read-data: assume we're reading floats from a raw file */
	  if (read_data_file)
	    {
	      float read_data_value;
	      fread((void *)(&read_data_value), 4, 1, read_data_file);
	      gen_outs[OutSum6] = OscOut13 + read_data_value;  /* was * 2 */
	      /* 
		 "If the run mode 
		 specifies adding into sum memory, Temp9 is added into the sum
		 memory location designated by GSUM; except that in run mode
		 0111, the product is added to the next read-data item from the
		 CPU and the sum replaces the contents of the sum memory
		 location addressed."
	      */
	    }
	  else
	    {
	      if (!read_data_warned)
		{
		  fprintf(stderr, "read data?!?\n");
		  read_data_warned = true;
		}
	    }
	}
    }
}


/* ---------------------------------------- modifier processing ---------------------------------------- */

/* 
 * 	Each modifier has the following numeric parameters.
 * M0  (30 bits) coefficient
 * M1  (30 bits) other coefficient
 * L0  (20 bits) running term
 * L1  (20 bits) other running term
 * MIN  (8 bits) address in sum memory where modifier reads "A" data
 * MRM  (8 bits) address in sum memory where modifier reads "B" data
 * 	MIN, MRM = QQAAAAAA
 *     QQ:	
 *      00  generator-last-pass quadrant
 * 	01  modifier-last-pass quadrant
 * 	10  modifier-this-pass quadrant
 * 	11  (reserved)
 *     AAAAAA: sum address within quadrant
 * MSUM  (7 bits) result address in sum memory
 * 	MSUM = RAAAAAA
 *     R: 0  add to sum
 *        1  replace sum
 *     AAAAAA: sum address in modifier-this-pass quadrant
 */

static void print_mod_read_name(int m)
{
  char *mem_names[4] = {"gen-ins", "mod-ins", "mod-outs", "oops"};
  fprintf(stderr, "%s[", mem_names[(m >> 6) & 0x3]);
  if (((m & 0x3f) == 0) && (((m >> 6) & 0x3) != 0)) 
    fprintf(stderr, "zero");
  else fprintf(stderr, "%d", m & 0x3f);
  fprintf(stderr, "]");
}


static double mod_read(int addr)
{
  int QQ, A;
  A = addr & 0x3f;
  QQ = LDB(addr, 2, 6);
  switch (QQ)
    {
    case 0: return(gen_ins[A]);
    case 1: return(mod_ins[A]);
    case 2: return(mod_outs[A]);

    case 3: 
      /* "reserved", but it seems to happen in MARS.SAM, and Pete says:
       *
       * "Thanks to Al Kossow of the Computer History Museum for putting scans   
       *  of the (preliminary) synthesizer schematics and the theory-of- 
       *  operation manual up on bitsavers.org. 
       *
       *  It appears that QQ=3 will work the same as QQ=1, i.e. modifier last   
       *  pass quadrant."
       */
      return(mod_ins[A]);
    }

  return(0);
}


static void mod_write(int addr, double val)
{
  int R, AAAAAA;

  if (isnan(val))
    {
      fprintf(stderr, "write %d %d NaN!\n", addr >> 6, addr & 0x3f);
    }
  AAAAAA = addr & 0x3f;
  R = BIT(addr, 6);
  if (R == 0)
    mod_outs[AAAAAA] += val;
  else mod_outs[AAAAAA] = val;
}

/*
 * MMODE  (9 bits) modifier mode
 * 	MMODE = MMMMMAABB
 * AA:  scale of second multiplication
 * BB:  scale of first multiplication
 * For fraction multiplications:
 *   00:  x 1
 *   01:  x 2
 *   10:  x 4
 *   11:  x 8
 * For integer multiplications:
 *   00:  x 1/4
 *   01:  x 1/2
 *   10:  x 1
 *   11:  x 2
 *  A multiplication involving parameter M1 will be the first
 *   multiplication; one involving M0 will be the second.
 * 
 * MMMMM: function
 *   00000:  inactive
 *   00010:  uniform noise
 *   00011:  triggered uniform noise
 *   00100:  latch
 *   00110:  threshold
 *   00111:  invoke delay unit
 * 
 *   01000:  two poles
 *   01001:  two poles, M0 variable
 *   01011:  two poles, M1 variable
 *   01100:  two zeros
 *   01101:  two zeros, M0 variable
 *   01111:  two zeros, M1 variable
 * 
 *   10000:  integer mixing
 *   10001:  one pole
 *   10100:  mixing
 *   10110:  one zero
 * 
 *   11000:  four-quadrant multiplication
 *   11001:  amplitude modulation
 *   11010:  maximum
 *   11011:  minimum
 *   11100:  signum
 *   11101:  zero-crossing pulser
 * 
 *   others:  (reserved)
 */

#define mod_mode(M) ((M >> 4) & 0x1f)
#define M_INACTIVE 0
#define M_NOISE 2
#define M_TRIGGERED_NOISE 3
#define M_LATCH 4
#define M_THRESHOLD 6
#define M_DELAY 7
#define M_TWO_POLE 8
#define M_TWO_POLE_M0 9
#define M_TWO_POLE_M1 11
#define M_TWO_ZERO 12
#define M_TWO_ZERO_M0 13
#define M_TWO_ZERO_M1 15
#define M_INTEGER_MIXING 16
#define M_ONE_POLE 17
#define M_MIXING 20
#define M_ONE_ZERO 22
#define M_MULTIPLY 24
#define M_AMP_MOD 25
#define M_MAX 26
#define M_MIN 27
#define M_SIGNUM 28
#define M_ZERO_CROSS 29

static double delay_read(int dly);
static void delay_write(int dly, double val);

static void process_mod(int mod)
{
  modifier *m;
  int mode, IS;
  double S, A, B, tmp0, tmp1;

  m = mods[mod];
  mode = mod_mode(m->MMODE);
  if (mode == M_INACTIVE)
    {
      /* technically, mod_write(m->MSUM, 0.0) which might be in "replace" mode if BIT(m->MSUM, 6) is not 0 */
      return;
    }

  A = mod_read(m->MIN);
  B = mod_read(m->MRM);
  
  switch (mode)
    {
    case M_INACTIVE:
      /* 00000:	inactive.  S := 0 
       */
      break;

    case M_NOISE:
      /* 00010:	uniform noise.  S := L0 + L1*M0 (integer multiply, low-order
       *                        20 bits of product used; overflow ignored); L1 := S
       *
       * see below -- I don't think this is correct.
       */
      /* IS = (m->L0 + (m->L1 * m->M0)) & 0xfffff; */

      IS = (m->L0 + ((m->L1 * m->M0) >> 10)) & 0xfffff;
      mod_write(m->MSUM, TWOS_20_TO_DOUBLE(IS));
      m->L1 = IS;
      break;

    case M_TRIGGERED_NOISE:
      /* 00011:	triggered uniform noise.  S := L0 + L1*M0 (integer multiply,
       *          low-order 20 bits of product used; overflow ignored);
       *          if B*M1 (integer multiply, low-order 20 bits of product
       *          used; overflow ignored) is not 0, L1 := S
       */

      /* IS = (m->L0 + (m->L1 * m->M0)) & 0xfffff; */
      /* I'm getting an immediate fixed-point from the SAM files that used triggered noise! */
      /* they used the M0 seed of 359035904, (L1: 204282), which immediately cycles. */
      /* perhaps the spec is wrong... -- I'll try taking the middle bits */

      IS = (m->L0 + ((m->L1 * m->M0) >> 10)) & 0xfffff;
      mod_write(m->MSUM, TWOS_20_TO_DOUBLE(IS));
      if ((B != 0.0) && 
	  (m->M1 != 0))
	m->L1 = IS;
      break;

    case M_LATCH:
      /* 00100:	latch (sample and hold).  S := L1;  If B*M1 is not 0, L1 := A 
       *   but in the errata:
       *   "BIL has discovered empirically that the modifier latch mode operation should actually read
       * 00100:	latch (sample and hold).  S := L1;  If B*M1 is not 0, L1 := A*M0"
       */
      mod_write(m->MSUM, m->f_L1);
      if ((B * m->f_M1) != 0.0) m->f_L1 = A * m->f_M0;
      break;

    case M_THRESHOLD:
      /* 00110:	threshold.  If A*M0 + L0 is less than 0, then S := 0;
       *                    if A*M0 + L0 is equal to or greater than 0, then S := B*M1
       */
      tmp0 = A * m->f_M0 + m->f_L0;
      if (tmp0 < 0.0)
	mod_write(m->MSUM, 0.0);
      else mod_write(m->MSUM, B * m->f_M1);
      break;

    case M_DELAY:
      /* 00111:	invoke delay unit.
       *     Unit # := MRM (low-order 5 bits);
       *     S := L0 + L1*M0;  L0 := DM;  Temp0 := A + DM*M1;
       *     L1 := Temp0;  DM := Temp0
       */
      /* to handle table lookups, we need the integer side here */
      /* fprintf(stderr, "d%d, m%d: %.4f = %.4f + %.4f * %.4f\n", m->MRM & 0x1f, mod, m->f_L0 + m->f_L1 * m->f_M0, m->f_L0, m->f_L1, m->f_M0); */
      
      mod_write(m->MSUM, m->f_L0 + m->f_L1 * m->f_M0);
      m->f_L0 = delay_read(m->MRM & 0x1f);
      m->f_L1 = A + m->f_L0 * m->f_M1;
      delay_write(m->MRM & 0x1f, m->f_L1);
      break;
      
    case M_TWO_POLE:
    case M_TWO_POLE_M0:
    case M_TWO_POLE_M1:
      /* 01000:	two poles.               S := L1*M1 + L0*M0 + A; L0 := L1; L1 := S
       *
       * 01001:	two poles, M0 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := S; M0 := M0 + B
       *
       * 01011:	two poles, M1 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := S; M1 := M1 + B
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = m->f_L0 * m->f_M0;
      S = tmp0 + tmp1 + A;  /* divide A by 1024.0 here probably */
      mod_write(m->MSUM, S);
      m->f_L0 = m->f_L1;
      m->f_L1 = S;
      if (mode == M_TWO_POLE_M0)
	m->f_M0 += (B / 1024.0); 
      /* "when a quantity is added to M0 or M1 it is added right-justified, with sign extended"
       *    does that include "A" above? I think it does... (see one and two_zero below).
       */
      if (mode == M_TWO_POLE_M1)
	m->f_M1 += (B / 1024.0);
      break;

    case M_TWO_ZERO:
    case M_TWO_ZERO_M0:
    case M_TWO_ZERO_M1:
      /* 01100:	two zeros.               S := L1*M1 + L0*M0 + A; L0 := L1; L1 := A
       *
       * 01101: two zeros, M0 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := A; M0 := M0 + B
       *
       * 01101:	two zeros, M0 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := A; M1 := M1 + B
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = m->f_L0 * m->f_M0;
      mod_write(m->MSUM, tmp0 + tmp1 + A); /* divide A by 1024.0 here probably */
      m->f_L0 = m->f_L1;
      m->f_L1 = A / 1024.0;
      if (mode == M_TWO_ZERO_M0)
	m->f_M0 += (B / 1024.0);
      if (mode == M_TWO_ZERO_M1)
	m->f_M1 += (B / 1024.0);
      break;

    case M_INTEGER_MIXING:
      /* 10000:	integer mixing.  S := A*M0 + B*M1 (integer multiply, low-order
       * 20 bits of product used; overflow ignored)
       */
      /* I don't remember how we used this -- I'll assume the M's are the ints */
      mod_write(m->MSUM, A * m->M0 + B * m->M1);
      break;

    case M_MIXING:
      /* 10100:	mixing.  S := A*M0 + B*M1 
       */
      mod_write(m->MSUM, A * m->f_M0 + B * m->f_M1);
      break;

    case M_ONE_POLE:
      /* 10001:	one pole.  S := L1*M1 + B*M0; L1 := S
       *
       *    but in the errata:
       *    "DAJ - It seems that the modifier mode one pole is really
       * 10001:	one pole.  S := L1*M1 + B*L0; L1 := S"
       *
       * but I think that is incorrect; old reverbs are definitely using the spec form of the 1-pole filter
       */
      tmp0 = m->f_L1 * m->f_M1;
      /* tmp1 = B * m->f_L0; */
      tmp1 = B * m->f_M0;

      m->f_L1 = tmp0 + tmp1;
      mod_write(m->MSUM, m->f_L1);
      break;
      
    case M_ONE_ZERO:
      /* 10110:	one zero.  S := L1*M1 + L0*M0; L0 := L1; L1 := A
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = m->f_L0 * m->f_M0;
      m->f_L0 = m->f_L1;
      m->f_L1 = A / 1024.0;
      mod_write(m->MSUM, tmp0 + tmp1);
      break;

    case M_MULTIPLY:
      /* 11000:	four-quadrant multiplication.  S := L1*M1; L1 := A*B
       */
      mod_write(m->MSUM, m->f_L1 * m->f_M1);
      m->f_L1 = A * B;
      break;

    case M_AMP_MOD:
      /* 11001:	amplitude modulation.  S := L1*M1;  L1 := A * ((B+1)/2)
       *        (The term ((B+1)/2) interprets B as a signed two's-complement
       *        fraction ranging in value from -1 to +1-epsilon.)
       */
      mod_write(m->MSUM, m->f_L1 * m->f_M1);
      m->f_L1 = A * (B + 1.0) * 0.5;
      break;

    case M_MAX:
      /* 11010:	maximum.  S := max (A*M0, B*M1) 
       */
      tmp0 = A * m->f_M0;
      tmp1 = B * m->f_M1;
      mod_write(m->MSUM, (tmp0 > tmp1) ? tmp0 : tmp1);
      break;

    case M_MIN:
      /* 11011:	minimum.  S := min (A*M0, B*M1) 
       */
      tmp0 = A * m->f_M0;
      tmp1 = B * m->f_M1;
      mod_write(m->MSUM, (tmp0 < tmp1) ? tmp0 : tmp1);
      break;

    case M_SIGNUM:
      /* 11100:	signum.  If A*M0 is less than B*M1, then S := -1 (integer)
       *                 if A*M0 equals B*M1, then S := 0;
       *                 if A*M0 is greater than B*M1, the S := 1 (integer)
       */
      tmp0 = A * m->f_M0;
      tmp1 = B * m->f_M1;
      if (tmp0 < tmp1) mod_write(m->MSUM, TWOS_20_TO_DOUBLE(-1));
      else if (tmp0 == tmp1) mod_write(m->MSUM, 0.0);
      else mod_write(m->MSUM, TWOS_20_TO_DOUBLE(1));
      break;
      
    case M_ZERO_CROSS:
      /* 11101:	zero-crossing pulser.  Temp0 := B*M0; Temp1 := L1*M1;
       *        if Temp1 is not 0 and either Temp0 is 0 or Temp0*Temp1 is
       *        negative then S := -epsilon, else S := 0; L1 := Temp0
       *        (The term -epsilon is a binary number with all bits set.)
       */
      tmp0 = B * m->f_M0;
      tmp1 = m->f_L1 * m->f_M0;
      if ((tmp1 != 0) &&
	  ((tmp0 == 0) || (tmp0 * tmp1 < 0)))
	mod_write(m->MSUM, TWOS_20_TO_DOUBLE(-1));
      m->f_L1 = tmp0;
      break;

    default:
      fprintf(stderr, "reserved modifier mode?\n");
      break;
    }
}



/* ---------------------------------------- delay processing ---------------------------------------- */

/*	Each delay unit has the following numeric parameters.
 * 
 * P  mode (4 bits).  The mode is interpreted as follows:
 * 		mode: 0000  inactive
 * 		      1000  delay line
 * 		      1010  table look-up
 * 		      1011  table look-up, argument rounded
 * 		      1100  delay tap
 * 		      others: (reserved)
 */

#define D_INACTIVE 0
#define D_LINE 8
#define D_TABLE_LOOKUP 10
#define D_TABLE_LOOKUP_ROUNDED 11
#define D_TAP 12

/*
 * Z  unit length (16 bits) or binary scale factor (4 bits).
 * 	In delay line and delay tap modes, Z gives 1 less than the 
 * 	total number of locations in delay memory used by the delay 
 * 	unit, i.e. the index of the last delay memory address for 
 * 	this unit.  In table look-up modes, the low-order four bits 
 * 	of Z specify the number of binary places that the argument 
 * 	is shifted to the right before it is used to address the 
 * 	memory; if rounding is specified, the address after shifting
 * 	is incremented by 1 if the most-significant bit shifted out
 * 	was a 1.
 * 
 * Y  index (16 bits).  In delay line and delay tap modes, this is the 
 * 	running index on the memory area for the unit.
 * 
 * X  base address (16 bits).  The base address is the lowest-numbered
 * 	delay memory location used by this unit.
 *
 * 	In inactive mode, delay memory is not modified and the unit
 * returns indeterminate results.  Delay units not accommodated due
 * to the number of ticks in a pass act as if in the inactive mode.
 * If the number of processing ticks is 4*n + m where m is 1, 2, or 3,
 * delay unit number n should be put in the inactive mode.
 * 
 * 	In delay line mode, a 20-bit data word is received from
 * the modifier that calls for the delay unit, and another 20-bit
 * word is sent to it.  The word received is put into the next slot
 * in the delay line.  It will be retrieved and sent back to the
 * modifier Z+3 passes later.  In delay tap mode, a word is sent to
 * the modifier but delay memory is not written into.
 *
 * 	In table look-up mode, the 20-bit data word received
 * from the modifier is shifted to the right Z bits, bringing in zeros,
 * and the right 16 bits of the result are used to address the memory
 * area assigned to the unit.  The 20-bit word in the addressed memory
 * location is returned to the modifier three passes later.
 */

static bool table_read_warned = false, table_write_warned = false;

static double delay_read(int dly)
{
  delay *d;
  d = dlys[dly];
  switch (d->P)
    {
    case D_INACTIVE:
      return(0.0);
      
    case D_LINE:
    case D_TAP:
      /* return the value with a hidden 2 sample delay (Z+3 == total delay length + 2) */
#if 0
      return(delay_memory[d->X + d->Y]);
#else
      {
	/* I originally thought this was making a raspy or crackling sound in the reverbs, but now I don't hear it (bil) */
	double val;
	val = d->xd2;
	d->xd2 = d->xd1;
	d->xd1 = delay_memory[d->X + d->Y];
	return val;
      }
#endif

    case D_TABLE_LOOKUP:
    case D_TABLE_LOOKUP_ROUNDED:
      {
	int Z_shift, dY;
	if (!table_read_warned)
	  {
	    fprintf(stderr, "table lookup read is unlikely to work.\n");
	    table_read_warned = true;
	  }
	Z_shift = d->Z & 0xf;
	dY = (d->I >> Z_shift) & 0xffff;
	return(delay_memory[d->X + dY]); 
      }
    }
  return(0);
}


static void delay_write(int dly, double val)
{
  delay *d;
  d = dlys[dly];
  switch (d->P)
    {
    case D_INACTIVE:
    case D_TAP:
      break;
      
    case D_LINE:
      delay_memory[d->X + d->Y] = val;
      break;
      
    case D_TABLE_LOOKUP:
    case D_TABLE_LOOKUP_ROUNDED:
	if (!table_write_warned)
	  {
	    fprintf(stderr, "table lookup write is unlikely to work.\n");
	    table_write_warned = true;
	  }
      d->I = DOUBLE_TO_TWOS_20(val); /* can this work? */
      break;
    }
}


static void process_dly(int dly)
{
  delay *d;
  d = dlys[dly];
  d->Y += 1;
  if (d->Y > d->Z) /* unit size - 1 so not >= ? */
    d->Y = 0;
}


/* ---------------------------------------- run! ---------------------------------------- */

static void dump_patch(void);

static void linger(int time)
{
  /* process each sample ("pass") until pass == time */
  /*   but linger was a 20-bit number, so it wrapped around I believe, so pass should be mod 2^20? */
  
  if (!snd_file)
    {
      fprintf(stderr, "no ticks setting found!\n");
      exit(0);
    }

  if (time < pass)
    pass = pass - (1 << 20);

  /* old SAM files had endless strings of lingers at the end generating enormous empty sound files, but
   *
   * mmm - this was causing the long trailing reverb of some of my files to be cut off.
   */

  if ((FLUSH_TRAILING_LINGERS) &&
      ((total_commands - current_command) < 100) && 
      (total_commands > 1000) &&
      ((time - pass) > (6 * srate)))
    {
      fprintf(stderr, "ignore trailing %d sample (%.3f second) linger (%d)\n", 
	      time - pass, (double)(time - pass) / (double)srate, total_commands - current_command);
      pass = time;
      return;
    }

  while (pass < time)
    {
      /* run through all available ticks, processing gen+mod+dly, 
       *   then write accumulated dac_outs, clear, update memories (this-pass -> last-pass),
       *   and increment pass 
       */
      int i, tick, gen = 0, mod = 0, dly = 0;

      for (tick = 0; tick < processing_ticks; tick++)
	{
	  /* given the timing info I'll simplify a bit and run 1 gen per tick, 1 mod every 2 ticks, and 1 delay every 4 ticks */
	  if (gen < GENERATORS)
	    process_gen(gen++);

	  /* I'm guessing... */
	  if (((tick & 1) == 0) &&
	      (mod < MODIFIERS))
	    process_mod(mod++);

	  if (((tick & 3) == 0) &&
	      (dly < DELAYS))
	    process_dly(dly++);
	}

      if (dump_patch_at == samples)
	dump_patch();

      for (i = 0; i < SUM_MEMORY_SIZE; i++)
	{
	  if (fabs(gen_ins[i]) > peak_gen_ins[i])
	    peak_gen_ins[i] = fabs(gen_ins[i]);
	  prev_gen_ins[i] = gen_ins[i];
	  gen_ins[i] = gen_outs[i];
	  gen_outs[i] = 0.0;

	  if (fabs(mod_ins[i]) > peak_mod_ins[i])
	    peak_mod_ins[i] = fabs(mod_ins[i]);
	  prev_mod_ins[i] = mod_ins[i];
	  mod_ins[i] = mod_outs[i];
	  mod_outs[i] = 0.0;
	}

      fwrite(dac_out, 4, 4, snd_file);
      samples++;
      for (i = 0; i < 4; i++) 
	{
	  if (fabs(dac_out[i]) > dac_out_peak[i])
	    dac_out_peak[i] = fabs(dac_out[i]);
	  dac_out[i] = 0.0;
	}
      pass++;

      if (samples == TOTAL_SAMPLES) 
	all_done();
    }
}


/* ---------------------------------------- commands ---------------------------------------- */

/* 
 *    -----------------------------------------------------------------
 *    :	      (20) data		: 0  0  0  0  0:  RR : x  x: W: P: S:
 *    -----------------------------------------------------------------
 * MISC
 *      RR: 00  no effect
 *          01  load DX from data
 *	    10  load TTL buffer A from left 16 bits of data
 *	    11  load TTL buffer B from left 16 bits of data
 *		  set analog output filters from right 4 bits of data:
 *		    01xx  Mode 0
 *		    00nn  Mode 1, frequency f0, f1, f2, or f3 according
 *			to nn
 *	W:  if 1, clear all wait bits
 *	P:  if 1, clear all pause bits
 *	S:  if 1, stop clock
 */

static void misc_command(int cmd)
{
  int data, RR, W, P, S;
  char *RR_name[4] = {"noop", "load DX", "TTL-A", "TTL-B"};

  data = LDB(cmd, 20, 12);
  RR = LDB(cmd, 2, 5);
  W = BIT(cmd, 2);
  P = BIT(cmd, 1);
  S = BIT(cmd, 0);

  if (describe_commands)
    fprintf(stderr, "sam: %d, %s%s%s%s\n", 
	    data, 
	    RR_name[RR], 
	    (W == 1) ? "" : ", clear waits",
	    (P == 1) ? "" : ", clear pauses",
	    (S == 1) ? "" : ", stop clock");

  if (RR == 1) DX = data;

  if (W == 1) 
    {
      /* cause any generator in run mode 1001 to change to mode 1111 */
      int i;
      for (i = 0; i < GENERATORS; i++)
	if ((gens[i]) && (osc_run(gens[i]->GMODE) == 9))
	  set_osc_run(i, 15);
    }

  if (P == 1) 
    {
      /* cause any generator in run mode 0001 to change to mode 1111 */
      int i;
      for (i = 0; i < GENERATORS; i++)
	if ((gens[i]) && (osc_run(gens[i]->GMODE) == 1))
	  set_osc_run(i, 15);
    }

  if (REPORT_BAD_COMMANDS)
    {
      if ((S == 1) && 
	  ((total_commands - current_command) > 1000))
	fprintf(stderr, "sam: %x: stop clock?\n", cmd);
    }
}


/*
 *    -----------------------------------------------------------------
 *    :	  (16) data	:(4)data: 0  0  0  0  1: U  U:  (5) unit #  :
 *    -----------------------------------------------------------------
 * DLY X, Y, Z
 *	UU:  00  X    16 bits base address; clear Y
 *	     01  Y    16 bits one's complement of index
 *	     10  Z,P  16 bits delay unit size minus 1, or scale (low
 *			  4 bits of 16); 4 bits mode
 *	     11  (unused)
 */

static const char *P_name(int P)
{
  switch (P)
    {
    case D_INACTIVE:             return("inactive");
    case D_LINE:                 return("line");
    case D_TAP:                  return("tap");
    case D_TABLE_LOOKUP:         return("table");
    case D_TABLE_LOOKUP_ROUNDED: return("rtable");
    default:                     return("unknown");
    }
}


static void dly_command(int cmd)
{
  int unit, UU, data_4, data_16;
  delay *d;
  char *UU_name[4] = {"set base, clear index", "set index", "set size", "un-used!"};

  unit = (cmd & 0x1f);
  UU = LDB(cmd, 2, 5);

  if (UU == 3)
    {
      fprintf(stderr, "unknown delay command!\n");
      return;
    }

  data_4 = LDB(cmd, 4, 12);
  data_16 = LDB(cmd, 16, 16);
  
  d = dlys[unit];
  switch (UU)
    {
    case 0: 
      d->X = data_16;
      d->Y = 0;
      break;

    case 1:
      d->Y = data_16;
      break;

    case 2:
      d->Z = data_16;
      d->P = data_4;
      break;
    }

  if (describe_commands)
    {
      fprintf(stderr, "d%d %s", unit, UU_name[UU]);
      if (UU == 0)
	fprintf(stderr, ": X: %d", d->X);
      else
	{
	  if (UU == 1)
	    fprintf(stderr, ": Y: %d", d->Y);
	  else fprintf(stderr, ": Z: %d, P: %s", d->Z, P_name(d->P));
	}
      fprintf(stderr, "\n");
    }
}


/*
 *    -----------------------------------------------------------------
 *    :	      (20) data		: 0  0  0  1  0: x  x: T  T: x  x  x:
 *    -----------------------------------------------------------------
 * TIMER
 *	TT: 00  no effect
 *	    10  Linger: process no further commands until pass counter
 *		    equals data
 *	    11  clear pass counter, then Linger as for 10
 *	    01  set pass counter from data
 */

static void timer_command(int cmd)
{
  int data, TT;
  char *TT_name[4] = {"noop", "set pass", "linger", "clear pass and linger"};

  TT = LDB(cmd, 2, 3);
  data = LDB(cmd, 20, 12);

  if (describe_commands)
    fprintf(stderr, "sam %s: %d at sample %d %.4f\n", TT_name[TT], data, samples, (double)samples / (double)srate);

  switch (TT)
    {
    case 0: 
      break;
    case 1: 
      pass = data; 
      break;
    case 2: 
      linger(data);
      break;
    case 3: 
      pass = 0; 
      linger(data);
      break;
    }
}


/*
 *   -----------------------------------------------------------------
 *   : xxx xxx xxx x : (10) data  : 0  0  0  1  1: x  x: 0: Q: x  x  x:
 *   -----------------------------------------------------------------
 * TICKS
 *	Q: 0  designate highest-numbered processing tick per pass
 *		    (should not exceed 255 [See appendix - DAJ])
 *	   1  designate next-to-highest-numbered tick (processing
 *	      plus overhead plus update) per pass
 */
static bool bit_31_warned = false;

static void ticks_command(int cmd)
{
  int Q, data, bit_31;
  char *Q_name[2] = {"set highest processing tick", "set highest tick"};

  bit_31 = BIT(cmd, 4);
  Q = BIT(cmd, 3);
  data = LDB(cmd, 10, 12);

  if (REPORT_BAD_COMMANDS)
    {
      if (bit_31 != 0) 
	{
	  if (!bit_31_warned)
	    {
	      fprintf(stderr, "ticks bit 31 is on?\n");
	      bit_31_warned = true;
	    }
	  return; /* what is going on here? */
	}
    }

  if (data != 0) /* used at end of some box sequences, but that confuses srate */
    {
      if (Q == 0)
	processing_ticks = data + 1; /* mmm - data is highest numbered processing tick per pass, so processing_ticks is 1 greater. */
      else 
	{
	  if (srate <= 1)
	    {
	      /* mmm - srate can now be set from the command line in certain cases. I had some weird tick settings for some reason.
	       * mmm - highest_tick_per_pass is actually being set here to the max *number* of ticks per pass, including overhead
	       */
	      highest_tick_per_pass = data + 2; /* why isn't this 9? */

	      /* "It's not clear from the documentation, so to clarify:  On the # TICKS
	       *  command, the number to be supplied for Q=1 is the total number of ticks
	       * per pass minus 2. (TVR - 7 August 1984)"
	       */

	      /* it's a 10 bit field, and higher bits are ignored, so the slowest we
	       *   can run is 5010Hz or thereabouts
	       */

	      if (highest_tick_per_pass > GENERATORS)
		highest_tick_per_pass = GENERATORS;  /* mmm - could it not be higher in some cases? */

	      srate = (int)(1000000000.0 / (double)(highest_tick_per_pass * 195));
	    }
	  else
	    {
	      highest_tick_per_pass = (1000000000.0 / (double)srate / 195.0);
	    }
	}
    }

  if (describe_commands)
    {
      fprintf(stderr, "sam %s: %d", Q_name[Q], data);
      if (Q == 1)
	fprintf(stderr, " (%d Hz)", srate);
      fprintf(stderr, "\n");
    }

  if ((data != 0) && (srate != 0))
    {
      if ((snd_file) && (samples == 0) && (Q == 1)) /* 2 tick commands at the start? */
	{
	  fclose(snd_file);                         /* start over... */
	  snd_file = NULL;
	}

      if (snd_file == NULL)
	{
	  /* now that we know the sampling rate, open the output file */
	  int header_info[24] = {1179011410, 88, 1163280727, 1263424842,
				 28, 0, 0, 0,
				 0, 0, 0, 0,
				 544501094, 16, 262147, 44100,
				 705600, 2097168, 1635017060, 16,
				 0, 0, 0, 0};
	  header_info[15] = srate;

	  /* mmm - generate output filename based on input filename */
	  {					
	    char *dot;
	    int i, len;
	    len = strlen(filename);
	    output_filename = (char *)malloc(len + 1);
	    strcpy(output_filename, filename);
	    /* dot = strchr(output_filename, '.');
	     *     can be confused by ../test/TEST.SAM
	     */
	    for (i = len - 1; i > 0; i--)
	      if (filename[i] == '.')
		{
		  dot = (char *)(output_filename + i);
		  break;
		}
	    strcpy(dot + 1, "wav");
	    snd_file = fopen(output_filename, "w");
	  }

	  if (!snd_file)
	    {
	      fprintf(stderr, "can't open test.snd!\n");
	      exit(0);
	    }
	  fwrite((void *)header_info, 4, 24, snd_file);
	}
    }
}


static int last_GMODE_command = 0;

/* GQ  (24 bits) phi -- decay exponent
 *    -----------------------------------------------------------------
 * GQ  :      (20) data         : 0  0  1: E:      (8)   gen #      :
 *    -----------------------------------------------------------------
 *
 *	E: 0  Q right-adjusted, sign extended
 *	   1  Q left-adjusted, low bits from left of DX; clear DX
 */

static void gq_command(int cmd)
{
  /* GQ is 24 bits */
  int data, E, gen, old_DX = 0, old_GQ;
  double old_f_GQ;
  generator *g;
  char *E_name[2] = {"right adjusted", "left adjusted + DX"};

  gen = LDB(cmd, 8, 0);
  E = BIT(cmd, 8);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  old_GQ = g->GQ;
  old_f_GQ = g->f_GQ;

  /* spec says "sign extended" which makes me think this number is signed, but I think it is unsigned in exp modes */
  /* mmm - I also believe it is unsigned. */
  /* pete: 
   *       Hmm, it looks like it makes more sense to call it unsigned. Certainly   
   *       the multiplication of envelope times waveform treats the envelope as   
   *       unsigned (i.e. non-negative). 
   */

  if (E == 0)
    g->GQ = data; /* mmm */ 
  else 
    {
      g->GQ = (data << 4) | ((DX >> 16) & 0xf); /* mmm */
      old_DX = DX;
      DX = 0;
    }

  g->f_GQ = (double)(g->GQ) / (double)(1 << 24); /* mmm - proper scaling of unsigned value */

  if (describe_commands)
    {
      if (E == 0)
	fprintf(stderr, "g%d amp: %s, %d %.4f\n", gen, E_name[E], g->GQ, g->f_GQ);
      else fprintf(stderr, "g%d amp: %s, %d = %d %.4f (DX: %d)\n", gen, E_name[E], data, g->GQ, g->f_GQ, old_DX);
    }

#if 0
  if ((gen_is_active(g)) && 
      (samples > last_GMODE_command))
    {
      if (REPORT_BAD_COMMANDS)
	fprintf(stderr, "sample %d (%.3f), command %d, stray amp: g%d %.4f from %.4f (last mode sample: %d)\n", 
		samples, (double)samples / (double)srate, current_command, 
		gen, g->f_GQ, old_f_GQ,
		last_GMODE_command);
      if (FLUSH_BAD_COMMANDS)
	{
	  g->GQ = old_GQ;
	  g->f_GQ = old_f_GQ;
	}
    }
#endif
}


/* GJ  (28 bits) omega -- oscillator frequency
 *    -----------------------------------------------------------------
 * GJ  :      (20) data         : 0  1  0: E:      (8)   gen #      :
 *    -----------------------------------------------------------------
 *
 *	E: 0  J right-adjusted, sign extended
 *	   1  J left-adjusted, low bits from left of DX; clear DX
 */

static void gj_command(int cmd)
{
  /* GJ is 28 bits */
  int data, E, gen, old_DX = 0, old_GJ;
  double old_f_GJ;
  generator *g;
  char *E_name[2] = {"right adjusted", "left adjusted + DX"};

  gen = LDB(cmd, 8, 0);
  E = BIT(cmd, 8);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  old_GJ = g->GJ;
  old_f_GJ = g->GJ;

  if (E == 0)
    g->GJ = TWOS_20(data);
  else 
    {
      g->GJ = TWOS_28(((data << 8) + (DX >> 12))); /* need 28 - 20 = 8 bits? */
      old_DX = DX;
      DX = 0;
    }

  g->f_GJ = DOUBLE_28(g->GJ);

  if (describe_commands)
    {
      if (E == 0)
	fprintf(stderr, "g%d freq: %s, %d %.4f (%.4f Hz)\n", gen, E_name[E], g->GJ, g->f_GJ, g->f_GJ * 0.5 * srate);
      else fprintf(stderr, "g%d freq: %s (DX: %d), %d = %d %.4f (%.4f Hz)\n", gen, E_name[E], old_DX, data, g->GJ, g->f_GJ, g->f_GJ * 0.5 * srate);
    }

  if ((gen_is_active(g)) && 
      (g->GJ != old_GJ) && 
      (samples > last_GMODE_command))
    {
      if (REPORT_BAD_COMMANDS)
	fprintf(stderr, "sample %d (%.3f), command %d, stray freq: g%d %.4f from %.4f (last mode sample: %d), data: %d\n", 
		samples, (double)samples / (double)srate, current_command, 
		gen, g->f_GJ * 0.5 * srate, DOUBLE_28(old_GJ) * 0.5 *srate,
		last_GMODE_command, data);

      if (FLUSH_BAD_COMMANDS)
	{
	  g->GJ = old_GJ;
	  g->f_GJ = old_f_GJ;
	}
    }
}


/* GP  (20 bits) delta -- decay rate
 *    -----------------------------------------------------------------
 * GP  :      (20) data         : 0  1  1  0:      (8)   gen #      :
 *    -----------------------------------------------------------------
 */

static void gp_command(int cmd)
{
  /* GP is 20 bits */
  int data, gen;
  generator *g;

  gen = LDB(cmd, 8, 0);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  g->GP = TWOS_20(data);
  g->f_GP = DOUBLE_20(g->GP);

  if (describe_commands)
    fprintf(stderr, "g%d amp change: %d (%.4f/sec), amp: %.4f\n", gen, g->GP, g->f_GP * srate, g->f_GQ);
}


/* GN  (11 bits) number of cosines to be summed
 * GM  (4 bits) binary scale of cosine or sum of cosines
 * GS  (1 bit) whether to shift output when adding to sum memory
 * GN, -----------------------------------------------------------------
 * GM, :N:M:S S:x: (11) GN :(4) GM : 0  1  1  1:      (8)   gen #      :
 * GS  -----------------------------------------------------------------
 *
 *	N:  if 1, disable loading GN
 *	M:  if 1, disable loading GM
 *	SS: 00  clear GS to 0
 *	    01  set GS to 1
 *	    10  no effect
 *	    11  (reserved)
 */

static void gn_command(int cmd)
{
  int N, M, SS, GN, GM, gen;
  generator *g;
  char *SS_name[4] = {", clear GS", ", set GS to 1", "", ", GS reserved?"};

  gen = LDB(cmd, 8, 0);
  GM = LDB(cmd, 4, 12);
  GN = LDB(cmd, 11, 16);
  SS = LDB(cmd, 2, 28);
  M = BIT(cmd, 30);
  N = BIT(cmd, 31);

  if (describe_commands)
    {
      if (N == 1)
	{
	  if (M == 1)
	    fprintf(stderr, "g%d sum-memory shift:%s\n", gen, SS_name[SS]);
	  else fprintf(stderr, "g%d ncos scale: %d%s\n", gen, GM, SS_name[SS]);
	}
      else
	{
	  if (M == 1)
	    fprintf(stderr, "g%d ncos: %d%s\n", gen, GN, SS_name[SS]);
	  else fprintf(stderr, "g%d ncos: %d%s, scale: %d\n", gen, GN, SS_name[SS], GM);
	}
    }

  g = gens[gen];
  if (N == 0)
    g->GN = GN;
  if (M == 0)
    g->GM = GM;

  switch(SS)
    {
    case 0:
      g->GS = 0;
      break;

    case 1:
      g->GS = 1;
      break;
    }
}


/* GL  (12 bits) asymptote
 * GSUM  (6 bits) sum memory address into which output is added
 *      -----------------------------------------------------------------
 * GL,  :L:S:  (12) GL   : (6) GSUM : 1  0  0  0:      (8)   gen #      :
 * GSUM -----------------------------------------------------------------
 *
 *	L:  if 1, disable loading GL
 *	S:  if 1, disable loading GSUM
 */

static void gl_command(int cmd)
{
  int GL, GSUM, L, S, gen, old_GSUM;
  generator *g;

  gen = LDB(cmd, 8, 0);
  GSUM = LDB(cmd, 6, 12);
  GL = LDB(cmd, 12, 18);
  L = BIT(cmd, 31);
  S = BIT(cmd, 30);

  g = gens[gen];
  old_GSUM = g->GSUM;

  if (L == 0)
    {
      /* is this signed? -- posies treats it as unsigned, I believe */
#if 1
      g->GL = GL;
      g->f_GL = UNSIGNED_12_TO_DOUBLE(GL);
#else
      g->GL = TWOS_12(GL);
      g->f_GL = DOUBLE_12(g->GL);
#endif
    }

  if (S == 0)
    g->GSUM = GSUM;

  if (describe_commands)
    {
      if (L == 1)
	{
	  if (S == 1)
	    fprintf(stderr, "g%d: noop\n", gen);
	  else fprintf(stderr, "g%d outloc: gen-outs[%d]\n", gen, g->GSUM);
	}
      else
	{
	  if (S == 0)
	    fprintf(stderr, "g%d amp offset: %d = %.4f\n", gen, g->GL, g->f_GL);
	  else fprintf(stderr, "g%d outloc: gen-outs[%d] + amp offset: %d = %.4f\n", gen, g->GSUM, g->GL, g->f_GL);
	}
    }

  if (REPORT_BAD_COMMANDS)
    {
      if ((GL == 1) && (L == 1) && (S == 0))
	fprintf(stderr, "sample %d (%.3f), command %d, possible gen output loc overflow: g%d %d\n",
		samples, (double)samples / (double)srate, current_command, 
		gen, GSUM);

      if ((gen_is_active(g)) && 
	  (g->GSUM != old_GSUM) && 
	  (samples > last_GMODE_command) &&
	  (S == 0))
	fprintf(stderr, "sample %d (%.3f), command %d, stray output loc: g%d %d from %d (last mode sample: %d)\n", 
		samples, (double)samples / (double)srate, current_command, 
		gen, g->GSUM, old_GSUM,
		last_GMODE_command);
    }
}


/* (20 bits) theta -- oscillator angle
 *    -----------------------------------------------------------------
 * GK  :      (20) data         : 1  0  0  1:      (8)   gen #      :
 *    -----------------------------------------------------------------
*/

static void gk_command(int cmd)
{
  /* GK is 20 bits */
  int data, gen, old_GK;
  double old_f_GK;
  generator *g;

  gen = LDB(cmd, 8, 0);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  old_GK = g->GK;
  old_f_GK = g->f_GK;

  g->GK = TWOS_20(data);
  g->f_GK = DOUBLE_20(g->GK);

  if (describe_commands)
    fprintf(stderr, "g%d phase: %d %.4f\n", gen, g->GK, g->f_GK);

  if ((gen_is_active(g)) && 
      (samples > last_GMODE_command))
    {
      if (REPORT_BAD_COMMANDS)
	fprintf(stderr, "sample %d (%.3f), command %d, stray phase: g%d %.4f (last mode sample: %d)\n", 
		samples, (double)samples / (double)srate, current_command, 
		gen, g->f_GK,
		last_GMODE_command);
      if (FLUSH_BAD_COMMANDS)
	{
	  g->GK = old_GK;
	  g->f_GK = old_f_GK;
	}
    }
}


/* GFM  (7 bits) sum memory address from which frequency modulation
 * GMODE  (10 bits) generator mode
 *    -----------------------------------------------------------------
 *    :M:F:C:  (10) GMODE :(7) GFM: 1  0  1  0:      (8)   gen #      :
 *    -----------------------------------------------------------------
 * GMODE,
 * GFM	M:  if 1, disable loading GMODE
 *	F:  if 1, disable loading GFM
 *	C:  if 1, clear GK
 */

static bool bad_mode(int mode)
{
  int R, E, S;
  R = osc_run(mode);
  E = osc_env(mode);
  S = osc_mode(mode);

  if ((R != 2) && (R != 7) && (R != 3) && (R != 0))
    switch (S)
      {
      case SUMCOS: case SAWTOOTH: case SQUARE: case PULSE: case SIN_K: case SIN_FM: 
	break;
      default: 
	return(true);
      }

  switch (R)
    {
    case 0: case 1: case 15: case 14: case 9: case 13: case 7: case 3: case 2: 
      break;
    default: 
      return(true);
    }

  return(false);
}


static void print_gmode_name(int mode)
{
  /* RRRREESSSS */
  int R, E, S;
  char *E_name[4] = {"L-Q", "L+Q", "L-2^Q", "L+2^Q"};

  R = osc_run(mode);
  E = osc_env(mode);
  S = osc_mode(mode);

  if (R == 0)
    {
      fprintf(stderr, "inactive");
      return;
    }

  if ((R != 2) && (R != 7) && (R != 3))
    {
      switch (S)
	{
	case SUMCOS:   fprintf(stderr, "ncos");    break;
	case SAWTOOTH: fprintf(stderr, "saw");     break;
	case SQUARE:   fprintf(stderr, "square");  break;
	case PULSE:    fprintf(stderr, "pulse");   break;
	case SIN_K:    fprintf(stderr, "sin");     break;
	case SIN_FM:   fprintf(stderr, "sin+fm");  break;
	default:       fprintf(stderr, "unknown"); break;
	}
      
      fprintf(stderr, "-%s-", E_name[E]);
    }

  switch (R)
    {
    case 1:  fprintf(stderr, "pause");  break;
    case 15: fprintf(stderr, "A");      break;
    case 14: fprintf(stderr, "B");      break;
    case 9:  fprintf(stderr, "wait");   break;
    case 13: fprintf(stderr, "C");      break;
    case 7:  fprintf(stderr, "rd");     break;
    case 3:  fprintf(stderr, "wrt");    break;
    case 2:  fprintf(stderr, "DAC");    break;
    default: fprintf(stderr, "unknown"); break;
    }
}


static void gmode_command(int cmd)
{
  int gen, M, F, C, GMODE, GFM, old_GMODE, old_GFM;
  bool gen_was_active;
  generator *g;

  last_GMODE_command = samples;

  gen = LDB(cmd, 8, 0);
  GFM = LDB(cmd, 7, 12);
  GMODE = LDB(cmd, 10, 19);
  M = BIT(cmd, 31);
  F = BIT(cmd, 30);
  C = BIT(cmd, 29);

  g = gens[gen];
  old_GFM = g->GFM;
  old_GMODE = g->GMODE;
  gen_was_active = gen_is_active(g);

  if (M == 0)
    g->GMODE = GMODE;
  if (F == 0)
    g->GFM = GFM;
  if (C == 1)
    g->GK = 0;

  /*
  if (osc_env(GMODE) > 1) fprintf(stderr, "expt %d ", samples);
  */

  if (describe_commands)
    {
      fprintf(stderr, "g%d ", gen);
      if (M == 0)
	{
	  fprintf(stderr, "mode: ");
	  print_gmode_name(g->GMODE);
	}
      if (F == 0)
	{
	  if (M == 0) fprintf(stderr, ", ");
	  fprintf(stderr, "inloc: %s[%d]", ((g->GFM >> 6) == 0) ? "gen-ins" : "mod-ins", g->GFM & 0x3f);
	}
      if (C == 1)
	{
	  if ((M == 0) || (F == 0))
	    fprintf(stderr, ", ");
	  fprintf(stderr, "clear phase");
	}
      fprintf(stderr, "\n");
    }

  if (REPORT_BAD_COMMANDS)
    {
      if (bad_mode(GMODE))
	fprintf(stderr, "sample %d (%.3f), command %d, bad mode: g%d %x\n", 
		samples, (double)samples / (double)srate, current_command, 
		gen, GMODE);

      if ((gen_is_active(g)) &&
	  (gen >= processing_ticks))
	fprintf(stderr, "sample %d (%.3f), command %d, g%d cannot actually run (procticks: %d)\n", 
		samples, (double)samples / (double)srate, current_command, 
		gen, processing_ticks);
    
#if 0
      if ((gen_was_active) &&
	  (!gen_is_active(g)) &&
	  (g->f_GQ != 0.0))
	fprintf(stderr, "sample %d (%.3f), command %d, g%d turned off with amp %.4f\n", 
		samples, (double)samples / (double)srate, current_command,
		gen, g->f_GQ);
#endif
	
      if ((gen_was_active) && 
	  ((g->GFM != old_GFM) || (g->GMODE != old_GMODE)) &&
	  (samples > last_GMODE_command))
	{
	  if (g->GFM != old_GFM)
	    fprintf(stderr, "sample %d (%.3f), command %d, stray input loc: g%d %d from %d (last mode sample: %d)\n", 
		    samples, (double)samples / (double)srate, current_command, 
		    gen, g->GFM, old_GFM,
		    last_GMODE_command);
	  else
	    {
	      fprintf(stderr, "sample %d (%.3f), command %d, stray mode: g%d ",
		      samples, (double)samples / (double)srate, current_command, gen);
	      print_gmode_name(g->GMODE);
	      fprintf(stderr, " from ");
	      print_gmode_name(old_GMODE);
	      fprintf(stderr, " (last mode sample: %d)\n", last_GMODE_command);
	    }
	}
    }
}


/* GO  (20 bits) alpha -- oscillator frequency sweep rate
 *     -----------------------------------------------------------------
 * GO  :      (20) data         : 1  0  1  1:      (8)   gen #      :
 *     -----------------------------------------------------------------
*/

static void go_command(int cmd)
{
  /* GO is 20  bits */
  int data, gen;
  generator *g;

  gen = LDB(cmd, 8, 0);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  g->GO = TWOS_20(data);
  g->f_GO = DOUBLE_20(g->GO);

  if (describe_commands)
    {
      if (osc_run(g->GMODE) == 2)
	fprintf(stderr, "g%d DAC out: %d\n", gen, data);
      else fprintf(stderr, "g%d freq change: %d %.4f (%.4f Hz/sec), freq: %.4f\n", 
		   gen, g->GO, g->f_GO, g->f_GO * 0.5 * srate * srate / 256.0, g->f_GJ * srate * 0.5);
    }
}


/* M0  (30 bits) coefficient
 * M1  (30 bits) other coefficient
 *    -----------------------------------------------------------------
 * MM  :      (20) data         : 1  1  0: V  V:     (7)   mod #    :
 *    -----------------------------------------------------------------
 *
 *	VV: 00  M0 right-adjusted, sign extended
 *	    01  M1 right-adjusted, sign extended
 *	    10  M0 left-adjusted, low bits from left of DX; clear DX
 *	    11  M1 left-adjusted, low bits from left of DX; clear DX
 */

/* To avoid endless repetition in the modifier processing, I'll incorporate the scalers
 *   into M0 and M1 when they are set, or when the scalers are changed, but this means
 *   (for simplicity) keeping track of the original M0 and M1 values: ("o_M0" and friends)
 */

static void mm_command(int cmd)
{
  /* M0 and M1 are 30 bits */
  int mod, VV, data, old_DX = 0;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  VV = LDB(cmd, 2, 7);
  data = LDB(cmd, 20, 12);

  m = mods[mod];

  switch (VV)
    {
    case 0:
      m->M0 = TWOS_20(data);
      m->f_M0 = DOUBLE_30(m->M0);
      m->o_M0 = m->M0;
      m->o_f_M0 = m->f_M0;
      m->M0 = m->M0 * m->mult_scl_0 / 4;
      m->f_M0 *= m->mult_scl_0;
      break;

    case 1:
      m->M1 = TWOS_20(data);
      m->f_M1 = DOUBLE_30(m->M1);
      m->o_M1 = m->M1;
      m->o_f_M1 = m->f_M1;
      m->M1 = m->M1 * m->mult_scl_1 / 4;
      m->f_M1 *= m->mult_scl_1;
      break;

    case 2:
      m->M0 = TWOS_30(((data << 10) + ((DX >> 10) & 0x3ff)));
      m->f_M0 = DOUBLE_30(m->M0);
      m->o_M0 = m->M0;
      m->o_f_M0 = m->f_M0;
      old_DX = DX;
      DX = 0;
      m->M0 = (m->M0 / 4) * m->mult_scl_0; /* try not to set the sign bit */
      m->f_M0 *= m->mult_scl_0;
      break;

    case 3:
      m->M1 = TWOS_30(((data << 10) + ((DX >> 10) & 0x3ff)));
      m->f_M1 = DOUBLE_30(m->M1);
      m->o_M1 = m->M1;
      m->o_f_M1 = m->f_M1;
      old_DX = DX;
      DX = 0;
      m->M1 = (m->M1 / 4) * m->mult_scl_1;
      m->f_M1 *= m->mult_scl_1;
      break;
    }

  if (describe_commands)
    {
      switch (VV)
	{
	case 0: 
	  fprintf(stderr, "m%d M0: %d: %d %.6f\n", mod, data, m->M0, m->f_M0); 
	  break;
	case 1: 
	  fprintf(stderr, "m%d M1: %d: %d %.6f\n", mod, data, m->M1, m->f_M1); 
	  break;
	case 2: 
	  fprintf(stderr, "m%d M0+DX: data: %d + DX: %d (scl: %d), %d -> %d, %.6f -> %.6f\n", 
		  mod, data, old_DX, m->mult_scl_0, m->o_M0, m->M0, m->o_f_M0, m->f_M0); 
	  break;
	case 3: 
	  fprintf(stderr, "m%d M1+DX: data: %d + DX: %d (scl: %d), %d -> %d, %.6f -> %.6f\n", 
		  mod, data, old_DX, m->mult_scl_1, m->o_M1, m->M1, m->o_f_M1, m->f_M1); 
	  break;
	}
    }
}


/* L0  (20 bits) running term
 * L1  (20 bits) other running term
 *    -----------------------------------------------------------------
 * ML :      (20) data         : 1  1  1  0: N:     (7)   mod #    :
 *    -----------------------------------------------------------------
 *
 *	N: 0  L0
 *	   1  L1
 */

static void ml_command(int cmd)
{
  int mod, N, data;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  data = LDB(cmd, 20, 12);
  N = BIT(cmd, 7);

  m = mods[mod];
  if (N == 0)
    {
      m->L0 = TWOS_20(data);
      m->f_L0 = DOUBLE_20(m->L0);
    }
  else 
    {
      m->L1 = TWOS_20(data);
      m->f_L1 = DOUBLE_20(m->L1);
    }

  if (describe_commands)
    {
      if (N == 0)
	fprintf(stderr, "m%d L0: %d: %d %.6f\n", mod, data, m->L0, m->f_L0);
      else fprintf(stderr, "m%d L1: %d: %d %.6f\n", mod, data, m->L1, m->f_L1);
    }
}


/* MSUM  (7 bits) result address in sum memory
 * MMODE  (9 bits) modifier mode
 *    -----------------------------------------------------------------
 *    :M:S:C:H: (9) MMODE :(7)MSUM: 1  1  1  1  0:      (7)  mod #    :
 *    -----------------------------------------------------------------
 *
 * MMODE,
 * MSUM	M:  if 1, disable loading MMMMM bits of MMODE
 *	S:  if 1, disable loading MSUM
 *	C:  if 1, clear L0
 *	H:  if 1, disable loading AABB bits of MMODE
 */

static const char *mode_name(int m)
{
  switch (m)
    {
    case M_INACTIVE:        return("inactive");
    case M_NOISE:           return("noise");
    case M_TRIGGERED_NOISE: return("triggered-noise");
    case M_LATCH:           return("latch");
    case M_THRESHOLD:       return("thresh");
    case M_DELAY:           return("delay");
    case M_TWO_POLE:        return("2pole");
    case M_TWO_POLE_M0:     return("2pole-M0");
    case M_TWO_POLE_M1:     return("2pole-M1");
    case M_TWO_ZERO:        return("2zero");
    case M_TWO_ZERO_M0:     return("2zero-M0");
    case M_TWO_ZERO_M1:     return("2zero-M1");
    case M_INTEGER_MIXING:  return("int-mix");
    case M_ONE_POLE:        return("1pole");
    case M_MIXING:          return("mix");
    case M_ONE_ZERO:        return("1zero");
    case M_MULTIPLY:        return("multiply");
    case M_AMP_MOD:         return("am");
    case M_MAX:             return("max");
    case M_MIN:             return("min");
    case M_SIGNUM:          return("signum");
    case M_ZERO_CROSS:      return("0cross");
    }
  return("unknown");
}


static void mmode_command(int cmd)
{
  int mod, MSUM, MMODE, M, S, C, H;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  MSUM = LDB(cmd, 7, 12);
  MMODE = LDB(cmd, 9, 19);
  M = BIT(cmd, 31);
  S = BIT(cmd, 30);
  C = BIT(cmd, 29);
  H = BIT(cmd, 28);

  m = mods[mod];
  if (S == 0)
    m->MSUM = MSUM;
  if (C == 1)
    {
      m->L0 = 0;
      m->f_L0 = 0.0;
    }

  /* MMODE is MMMMMAABB */
  if (H == 0)
    {
      /* set up the scale factors now, so we don't have to futz around later */
      /* BB = first (!) */
      m->mult_scl_1 = (1 << (MMODE & 0x3));
      m->mult_scl_0 = (1 << ((MMODE >> 2) & 0x3));
      /* whenever M0/M1 are set, we will include these factors */

      m->M0 = (m->o_M0 / 4) * m->mult_scl_0; /* order matters -- don't want to set sign bit by accident */
      m->M1 = (m->o_M1 / 4) * m->mult_scl_1;
      m->f_M0 = m->o_f_M0 * m->mult_scl_0;
      m->f_M1 = m->o_f_M1 * m->mult_scl_1;

      if (M == 0)
	m->MMODE = MMODE;                                  /* set both */
      else m->MMODE = (MMODE & 0xf) + (m->MMODE & 0x1f0); /* H is 0, so set AABB */
    }
  else
    {
      if (M == 0)
	m->MMODE = (MMODE & 0x1f0) + (m->MMODE & 0xf);    /* M is 0, so set MMMMM */
    }

  if (describe_commands)
    {
      fprintf(stderr, "m%d ", mod);
      if (M == 0)
	fprintf(stderr, "mode: %s", mode_name(MMODE >> 4));
      if (H == 0)
	{
	  if (M == 0)
	    fprintf(stderr, ", ");
	  fprintf(stderr, "AA: %d, BB: %d (M0: %d, %.3f, M1: %d, %.3f)", (MMODE >> 2) & 0x3, MMODE & 0x3, m->M0, m->f_M0, m->M1, m->f_M1);
	}
      if (S == 0)
	{
	  if ((H == 0) || (M == 0))
	    fprintf(stderr, ", ");
	  fprintf(stderr, "outloc(%s): mod-outs[%d]", ((MSUM >> 6) == 0) ? "+" : "=", MSUM & 0x3f);
	}
      if (C == 1)
	{
	  if ((S == 0) || (H == 0) || (M == 0))
	    fprintf(stderr, ", ");
	  fprintf(stderr, "L0=0");
	}
      fprintf(stderr, "\n");
    }

  if (REPORT_BAD_COMMANDS)
    {
      if (((MMODE >> 4) != M_INACTIVE) &&
	  ((mod * 2) >= processing_ticks))
	fprintf(stderr, "sample %d (%.3f), command %d, m%d cannot actually run (procticks: %d)\n", 
		samples, (double)samples / (double)srate, current_command, 
		mod, processing_ticks);
    }
}


/* MIN  (8 bits) address in sum memory where modifier reads "A" data
 * MRM  (8 bits) address in sum memory where modifier reads "B" data
 *    -----------------------------------------------------------------
 *    :R:I:C C: (8) MRM : (8) MIN : 1  1  1  1  1:      (7)  mod #    :
 *    -----------------------------------------------------------------
 *
 * MRM,
 * MIN,	R:  if 1, disable loading MRM
 * MT	I:  if 1, disable loading MIN
 *	CC: 00  turn off truncation
 *	    01  turn on truncation
 *	    10  clear L1
 *	    11  no effect
 */

static void mrm_command(int cmd)
{
  int mod, MRM, MIN, R, I, CC;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  MIN = LDB(cmd, 8, 12);
  MRM = LDB(cmd, 8, 20);
  R = BIT(cmd, 31);
  I = BIT(cmd, 30);
  CC = LDB(cmd, 2, 28);

  m = mods[mod];
  if (R == 0)
    m->MRM = MRM;
  if (I == 0)
    m->MIN = MIN;

  switch (CC)
    {
    case 0: 
      m->T = 0;
      break;

    case 1:
      m->T = 1;
      break;

    case 2:
      m->L1 = 0;
      m->f_L1 = 0.0;
      break;
    }

  if (describe_commands)
    {
      fprintf(stderr, "m%d inlocs:", mod);
      if (R == 0)
	{
	  if (mod_mode(m->MMODE) == M_DELAY)
	    fprintf(stderr, ", delay: %d", MRM & 0x1f);
	  else 
	    {
	      fprintf(stderr, ", MRM: ");
	      print_mod_read_name(MRM);
	    }
	}
      if (I == 0)
	{
	  fprintf(stderr, ", MIN: ");
	  print_mod_read_name(MIN);
	}
      if (CC == 0) fprintf(stderr, ", trunc off");
      if (CC == 1) fprintf(stderr, ", trunc on");
      if (CC == 2) fprintf(stderr, ", L1=0");
      fprintf(stderr, "\n");
    }
}


static void handle_command(int cmd)
{
  /* actually we should take highest_tick - processing_ticks - 8 commands at a time, then run a sample */

  int op;
  if ((start_describing <= samples) &&
      (stop_describing >= samples))
    describe_commands = true;
  else describe_commands = DEFAULT_DESCRIBE_COMMANDS;

  op = LDB(cmd, 4, 8);
  
  switch (op)
    {
    case 0: 
      if (BIT(cmd, 7) == 1)
	dly_command(cmd); 
      else misc_command(cmd);
      break;

    case 1:
      if (BIT(cmd, 7) == 1)
	ticks_command(cmd);
      else timer_command(cmd);
      break;

    case 2: case 3: 
      gq_command(cmd); 
      break;

    case 4: case 5: 
      gj_command(cmd); 
      break;

    case 6: 
      gp_command(cmd); 
      break;

    case 7: 
      gn_command(cmd); 
      break;

    case 8:
      gl_command(cmd);
      break;

    case 9: 
      gk_command(cmd); 
      break;

    case 10: 
      gmode_command(cmd);
      break;

    case 11: 
      go_command(cmd);
      break;

    case 12: case 13: 
      mm_command(cmd);
      break;

    case 14: 
      ml_command(cmd);
      break;

    case 15: 
      if (BIT(cmd, 7) == 0)
	mmode_command(cmd);
      else mrm_command(cmd);
      break;

    default: 
      fprintf(stderr, "impossible command\n"); 
      break;
    }

  current_command++;
}


/* ---------------------------------------- debugging ---------------------------------------- */

#if 0
static void dump_gens(void)
{
  int i;
  for (i = 0; i < GENERATORS; i++)
    if (gens[i]->GMODE != 0)
      fprintf(stderr, "g%d GMODE: %d, %d [%.3f] -> %d [%.3f], GQ: %.3f, GP: %.3f, GL: %.3f, GJ: %.3f, GO: %.3f, GN: %d, GS: %d\n",
	      i, 
	      gens[i]->GMODE, 
	      gens[i]->GFM, ((gens[i]->GFM >> 6) == 0) ? gen_ins[gens[i]->GFM & 0x3f] : mod_ins[gens[i]->GFM & 0x3f],
	      gens[i]->GSUM, gen_outs[gens[i]->GSUM],
	      gens[i]->f_GQ, gens[i]->f_GP, gens[i]->f_GL, gens[i]->f_GJ, gens[i]->f_GO, 
	      gens[i]->GN, gens[i]->GS);
}


static void dump_mods(void)
{
  int i;
  for (i = 0; i < MODIFIERS; i++)
    if (mods[i]->MMODE != 0)
      fprintf(stderr, "m%d MMODE: %d, (%d [%.3f] %d [%.3f]) -> %d [%.3f], M0: %.3f, M1: %.3f, L0: %.3f, L1: %.3f\n",
	      i,
	      mods[i]->MMODE,
	      mods[i]->MIN, mod_read(mods[i]->MIN),
	      mods[i]->MRM, mod_read(mods[i]->MRM),
	      mods[i]->MSUM, mod_outs[mods[i]->MSUM],
	      mods[i]->f_M0, mods[i]->f_M1, mods[i]->f_L0, mods[i]->f_L1);
}
#endif

static void dump_gen_sum(int addr)
{
  int i;
  /* show prev-ins : ins : out, g%d for all writers */
  fprintf(stderr, "g-sum%d: %.3f %.3f %.3f [max: %.3f]", addr, prev_gen_ins[addr], gen_ins[addr], gen_outs[addr], peak_gen_ins[addr]);

  for (i = 0; i < GENERATORS; i++)
    if ((gens[i]->GMODE != 0) && 
	(gens[i]->GSUM == addr))
      fprintf(stderr, " g%d", i);
}


static void dump_mod_sum(int addr)
{
  int i;
  /* show prev-ins : ins : out, m%d for all writers */
  fprintf(stderr, "m-sum%d: %.3f %.3f %.3f [max: %.3f]", addr, prev_mod_ins[addr], mod_ins[addr], mod_outs[addr], peak_mod_ins[addr]);

  for (i = 0; i < MODIFIERS; i++)
    if ((mod_mode(mods[i]->MMODE) != M_INACTIVE) && 
	((mods[i]->MSUM &0x3f) == addr))
      fprintf(stderr, " m%d", i);
}


static void print_mod_sum(int addr)
{
  int loc;
  loc = addr & 0x3f;
  switch ((addr >> 6) & 0x3)
    {
    case 0: 
      fprintf(stderr, "[");
      dump_gen_sum(loc);
      break;
    case 1:
      fprintf(stderr, "[");
      if (loc == 0)
	{
	  if ((prev_mod_ins[0] != 0.0) || (mod_ins[0] != 0.0) || (mod_outs[0] != 0.0) || (peak_mod_ins[0] != 0))
	    dump_mod_sum(0);
	  else fprintf(stderr, "zero");
	}
      else dump_mod_sum(loc);
      break;
    case 2:
      fprintf(stderr, "-out[");
      if (loc == 0)
	{
	  if ((prev_mod_ins[0] != 0.0) || (mod_ins[0] != 0.0) || (mod_outs[0] != 0.0) || (peak_mod_ins[0] != 0))
	    dump_mod_sum(0);
	  else fprintf(stderr, "zero");
	}
      else dump_mod_sum(loc);
      break;
    case 3:
      fprintf(stderr, "[illegal: %d", addr);
      break;
    }
}


static int gen_mem_readers(int addr)
{
  int i, rds = 0;
  for (i = 0; i < GENERATORS; i++)
    if ((gens[i]->GMODE != 0) &&
	(gens[i]->GFM == addr)) /* Q bit 0 = gen */
      rds++;
  for (i = 0; i < MODIFIERS; i++)
    if (mod_mode(mods[i]->MMODE) != M_INACTIVE)
    {
      if (mods[i]->MIN == addr) /* QQ bits = 0 = gen */
	rds++;
      if ((mod_mode(mods[i]->MMODE) != M_DELAY) &&
	  (mods[i]->MRM == addr))
	rds++;
    }
  return(rds);
}


static int mod_mem_readers(int addr)
{
  int i, rds = 0;
  for (i = 0; i < GENERATORS; i++)
    if ((gens[i]->GMODE != 0) &&
	(gens[i]->GFM == 64 + addr)) /* Q bit 1 = mod */
      rds++;
  for (i = 0; i < MODIFIERS; i++)
    if (mod_mode(mods[i]->MMODE) != M_INACTIVE)
    {
      if ((mods[i]->MIN == 64 + addr) ||
	  (mods[i]->MIN == 128 + addr))
	rds++;
      if ((mod_mode(mods[i]->MMODE) != M_DELAY) &&
	  ((mods[i]->MRM == 64 + addr) ||
	   (mods[i]->MRM == 128 + addr)))
	rds++;
    }
  return(rds);
}


static void dump_patch(void)
{
  /* try to show all currently active elements and memory with some history */
  int i, p;

  fprintf(stderr, "sample: %d, command: %d, ", samples, current_command);

  for (i = 0, p = 0; i < GENERATORS; i++)
    if (gens[i]->GMODE != 0)
      p++;
  fprintf(stderr, "active gens: %d, ", p);

  for (i = 0, p = 0; i < MODIFIERS; i++)
    if (mod_mode(mods[i]->MMODE) != M_INACTIVE)
      p++;
  fprintf(stderr, "active mods: %d, ", p);
  
  for (i = 0, p = 0; i < DELAYS; i++)
    if (dlys[i]->P != 0)
      p++;
  fprintf(stderr, "active delays: %d\n\n", p);

  for (i = 0; i < GENERATORS; i++)
    if (gens[i]->GMODE != 0)
      {
	generator *g;
	g = gens[i];
	fprintf(stderr, "g%d ", i);
	print_gmode_name(g->GMODE);

	fprintf(stderr, " [");
	if ((g->GFM >> 6) == 0)
	  dump_gen_sum(g->GFM & 0x3f);
	else print_mod_sum(g->GFM);

	fprintf(stderr, "]->[");
	if (osc_run(g->GMODE) == 2)
	  fprintf(stderr, "OUT%d", g->GO & 0xf);
	else dump_gen_sum(g->GSUM);

	fprintf(stderr, " (%d)], (amp: %.3f, freq: %.3f", 
		gen_mem_readers(g->GSUM),
		gen_amp(g),
		g->f_GJ * 0.5 * srate);
	if (g->f_GJ == 0.0)
	  fprintf(stderr, ", phase: %.3f", g->f_GK);

	fprintf(stderr, ")\n");
      }
  fprintf(stderr, "\n");

  for (i = 0; i < MODIFIERS; i++)
    if (mod_mode(mods[i]->MMODE) != M_INACTIVE)
      {
	modifier *m;
	m = mods[i];
	fprintf(stderr, "m%d %s ", i, mode_name(mod_mode(m->MMODE)));

	if (mod_mode(m->MMODE) == M_MIXING)
	  fprintf(stderr, "%.4f * ", m->f_M0);
	fprintf(stderr, "A");
	print_mod_sum(m->MIN);
	fprintf(stderr, "], ");

	if (mod_mode(m->MMODE) == M_MIXING)
	  fprintf(stderr, "%.4f * ", m->f_M1);
	fprintf(stderr, "B");
	if (mod_mode(m->MMODE) == M_DELAY)
	  {
	    delay *d;
	    d = dlys[m->MRM & 0x1f];
	    fprintf(stderr, "[delay: %d (%.4f)", m->MRM & 0x1f, delay_memory[d->X + d->Y]);
	    fprintf(stderr, ", M0: %.4f, M1: %.4f, L0: %.4f, L1: %.4f", m->f_M0, m->f_M1, m->f_L0, m->f_L1);
	  }
	else print_mod_sum(m->MRM);

	fprintf(stderr, "]->[");
	if ((m->MSUM >> 6) != 0)
	  fprintf(stderr, "-replace");
	dump_mod_sum(m->MSUM & 0x3f);

	fprintf(stderr, " (%d)]\n", mod_mem_readers(m->MSUM));
      }
  fprintf(stderr, "\n");

  for (i = 0; i < DELAYS; i++)
    if (dlys[i]->P != D_INACTIVE)
      {
	delay *d;
	d = dlys[i];
	fprintf(stderr, "d%d %s %.3f (%d + %d of %d)\n",
		i, P_name(d->P), 
		delay_memory[d->X + d->Y],
		d->X, d->Y, d->Z);
      }
  {
    double dmax;
    dmax = fabs(delay_memory[0]);
    for (i = 1; i < DELAY_MEMORY_SIZE; i++)
      if (fabs(delay_memory[i]) > dmax)
	dmax = fabs(delay_memory[i]);
    fprintf(stderr, "delay memory peak: %.4f\n\n", dmax);
  }
}



/* ---------------------------------------- main program ---------------------------------------- */

int main(int argc, char **argv)
{
  if (argc < 2)
    fprintf(stderr, "sam filename [read_data file] [srate]\n"); /* mmm */
  else
    {
      FILE *sam_file;
      filename = argv[1];

      sam_file = fopen(filename, "r");
      if (!sam_file)
	fprintf(stderr, "can't find %s\n", filename);
      else
	{
	  long size;
	  fseek(sam_file, 0, SEEK_END);
	  size = ftell(sam_file);
	  rewind(sam_file);

	  if (size <= 0)
	    {
	      fprintf(stderr, "%s is empty\n", filename);
	      fclose(sam_file);
	    }
	  else
	    {
	      size_t bytes;
	      unsigned char *command;
	      int i;

	      if (argc > 2)
		{
		  read_data_file = fopen(argv[2], "r");
		  if (argc > 3) 
		    {
		      /* mmm - set srate explicitly.  I had an inexplicably high max tick setting in one sam file with read data input. */
		      sscanf(argv[3], "%d", &srate);
		    }
		}

	      start_clean();

	      command = (unsigned char *)calloc(size + 1, sizeof(unsigned char));
	      bytes = fread(command, sizeof(unsigned char), size, sam_file);
	      fclose(sam_file);

	      /* these were stored in at least 2 different formats
	       *
	       * FASTF.SAM:  "Type: 32BITR BADSAM ;Looks like a SAM command file but has questionable data"
	       * MACDON.SAM: "Type: SAM SIMPLE    ;Simple SAM command file (corresponding sound file possible)"
	       *
	       * FASTF was written as 32 bits (using the 1st case below), and MACDON as 36 (using the 2nd case).
	       * it looks like someone got a flag backwards, and wrote the known-good 32-bit files as 36,
	       *    and the possibly not-32 bit files as 32. I can't find the corresponding code in the writers
	       *    that Nando found on the exabyte tapes.  
	       *
	       * The *.SAM.snd files are raw big-endian 24-bit int data (stereo?) 
	       *    with many (6?) renditions?
	       */

#if 1
	      if ((command[0] != 0) || /* just a first guess */
		  (command[1] != 0))
		{
		  fprintf(stderr, "32\n");
		  total_commands = bytes / 4;
		  current_command = 0;
		  for (i = 0; i < bytes; i += 4)
		    {
		      int cmd;
		      int b1, b2, b3, b4;
		      b1 = command[i + 0];
		      b2 = command[i + 1];
		      b3 = command[i + 2];
		      b4 = command[i + 3];
		      cmd = b4 + (b3 << 8) + (b2 << 16) + (b1 << 24);
		      handle_command(cmd);
		    }
		}
	      else
		{
		  fprintf(stderr, "36\n");
		  total_commands = bytes / 5;
		  current_command = 0;
		  for (i = 0; i < bytes; i += 5)
		    {
		      int cmd;
		      int b1, b2, b3, b4, b5;
		      b1 = command[i + 0];
		      b2 = command[i + 1];
		      b3 = command[i + 2];
		      b4 = command[i + 3];
		      b5 = command[i + 4];
		      cmd = ((b5 >> 4) & 0xff) + (b4 << 4) + (b3 << 12) + (b2 << 20) + ((b1 & 0xff) << 28);
		      handle_command(cmd);
		    }
		}
#else
	      /* another format that Mike used:
	       *   cmd = (b1 << 28) | (b2 << 24) | (b3 << 16) | (b4 << 8) | b5; 
	       */
	      total_commands = bytes / 5;
	      current_command = 0;
	      for (i = 0; i < bytes; i += 5)
		{
		  int cmd;
		  int b1, b2, b3, b4, b5;
		  b1 = command[i + 0];
		  b2 = command[i + 1];
		  b3 = command[i + 2];
		  b4 = command[i + 3];
		  b5 = command[i + 4];
		  cmd = (b1 << 28) | (b2 << 24) | (b3 << 16) | (b4 << 8) | b5; 
		  handle_command(cmd);
		}
#endif
	    }
	  
	  all_done();
	}
    }
  return(0);
}

/* on the cover of an old copy of the specs:

   NOT TO LEAVE THE MUSIC ROOM                 [red ink and underlined]

   Would be an awful fate,                     [pencilled in below]
   Said Cleopatra to her groom,
   and struck him on the pate!
*/
