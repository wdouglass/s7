#include "snd.h"
#include "clm2xen.h"

/*
 * each channel currently being played has an associated dac_info struct
 *   all active dac_info structs are held in a play_list
 *   channels can come and go as a play is in progress
 */

/* -------------------------------- per-channel control-panel state -------------------------------- */

typedef struct {
  mus_any *gen;
  struct dac_info *dp;
  bool speeding;
  mus_float_t sr;
} spd_info;

typedef enum {DAC_NOTHING, DAC_CHANNEL, DAC_REGION, DAC_MIX, DAC_XEN} dac_source_t;

typedef struct dac_info {
  dac_source_t type;
  mus_float_t cur_index;
  mus_float_t cur_amp;
  mus_float_t cur_srate;
  mus_float_t cur_exp;
  mus_float_t cur_rev;       /* rev scaler -- len is set at initialization */
  mus_float_t contrast_amp;
  bool expanding, reverbing, filtering; /* these need lots of preparation, so they're noticed only at the start */
  int audio_chan;      /* where channel's output is going (wrap-around if not enough audio output channels) */
  int slot;
  mus_float_t *a;      /* filter coeffs */
  int a_size;          /* user can change filter order while playing (sigh...) */
  snd_fd *chn_fd;      /* sampler, null if DAC_Xen */
  spd_info *spd;
  mus_any *flt;
  int region, mix_id;  /* to reset region-browser play button upon completion */
  bool selection;
  mus_any *src;
  snd_info *sp;        /* needed to see button callback changes etc */
  chan_info *cp;
  bool never_sped;
  int expand_ring_framples;
  mus_long_t end;
  Xen stop_procedure;
  int stop_procedure_gc_loc;
  Xen func;
  int func_gc_loc, direction;
  mus_float_t (*dac_sample)(struct dac_info *dp);
} dac_info;

#define AMP_CONTROL(sp, dp) ((dp->cp->amp_control) ? (dp->cp->amp_control[0]) : sp->amp_control)
/* an experiment */


static mus_float_t dac_read_sample(struct dac_info *dp)
{
  return(read_sample(dp->chn_fd));
}


static mus_float_t dac_no_sample(struct dac_info *dp)
{
  return(0.0);
}


static mus_float_t dac_xen_sample(struct dac_info *dp)
{
  Xen result;
  result = Xen_unprotected_call_with_no_args(dp->func);
  if (Xen_is_false(result))
    {
      dp->end = 0;
      return(0.0);
    }
  return(Xen_real_to_C_double(result));
}


/* -------- filter -------- */

static mus_any *make_flt(dac_info *dp, int order, mus_float_t *env)
{
  if (order <= 0) return(NULL);
  dp->a_size = order;
  dp->a = (mus_float_t *)calloc(order, sizeof(mus_float_t));
  if (env) mus_make_fir_coeffs(order, env, dp->a);
  return(mus_make_fir_filter(order, dp->a, NULL));
}


/* -------- sample-rate conversion -------- */

static mus_float_t dac_src_input_as_needed(void *arg, int direction) 
{
  dac_info *dp = (dac_info *)arg;
  if ((direction != dp->direction) && 
      (dp->chn_fd))
    {
      read_sample_change_direction(dp->chn_fd, (direction == 1) ? READ_FORWARD : READ_BACKWARD);
      dp->direction = direction;
    }
  return((*(dp->dac_sample))(dp));
}


static mus_float_t speed(dac_info *dp, mus_float_t sr)
{
  if (dp->never_sped)
    return((*(dp->dac_sample))(dp));
  if (!dp->src)
    dp->src = mus_make_src(&dac_src_input_as_needed, sr, sinc_width(ss), (void *)dp);
  mus_set_increment(dp->src, sr);
  return(mus_src(dp->src, 0.0, &dac_src_input_as_needed));
}


/* -------- granular synthesis -------- */

static mus_float_t expand_input_as_needed(void *arg, int dir) 
{
  spd_info *spd = (spd_info *)arg;
  dac_info *dp;
  dp = spd->dp;
  if (spd->speeding)
    return(speed(dp, spd->sr));
  else return((*(dp->dac_sample))(dp));
}


static int max_expand_control_len(snd_info *sp)
{
  if (sp->expand_control_length > .5)
    return(0);
  return((int)(snd_srate(sp) * .5));
}


static void *make_expand(snd_info *sp, mus_float_t initial_ex, dac_info *dp)
{
  spd_info *spd;
  spd = (spd_info *)calloc(1, sizeof(spd_info));
  spd->gen = mus_make_granulate(&expand_input_as_needed,
				initial_ex, sp->expand_control_length,
				.6, /* expand scaler, not currently settable -- dac_set_expand_scaler below */
				sp->expand_control_hop, sp->expand_control_ramp, 
				sp->expand_control_jitter,
				max_expand_control_len(sp), NULL, (void *)spd);
  spd->dp = dp;
  spd->speeding = false;
  spd->sr = 0.0;
  return(spd);
}


static void free_expand(void *ur_spd)
{
  spd_info *spd = (spd_info *)ur_spd;
  if (ur_spd)
    {
      mus_free(spd->gen);
      free(spd);
    }
}


static mus_float_t expand(dac_info *dp, mus_float_t sr, mus_float_t ex)
{
  /* from mixer.sai, used in "Leviathan", 1986 */
  bool speeding;
  snd_info *sp;
  spd_info *spd;
  sp = dp->sp;
  speeding = ((sp->speed_control_direction != 1) || (!(snd_feq(sp->speed_control, 1.0))) || (!(snd_feq(dp->cur_srate, 1.0))));
  spd = dp->spd;
  spd->speeding = speeding;
  spd->sr = sr;
  return(mus_granulate(spd->gen, &expand_input_as_needed));
}


/* -------- reverb -------- */

/* to implement run-time reverb-length, we would need to save the individual delay lens, both nominal
   and actual (given srate); set up the combs/allpasses to make room for 5.0 as reverb_control_length,
   then at each call, check current reverb len (how?), and if not original, recalculate how many
   samples each delay needs to be expanded by and so on -- more complexity than it's worth!
*/

static int prime(int num)
{
  if (num == 2) return(1);
  if ((num % 2) == 1)
    {
      int lim, i;
      lim = (int)(sqrt(num));
      for (i = 3; i < lim; i += 2)
	if ((num % i) == 0) 
	  return(0);
      return(1);
    }
  return(0);
}


static int get_prime(int num)
{
  int i;
  if ((num % 2) == 1)
    i = num;
  else i = num + 1;
  while (!(prime(i))) i += 2;
  return(i);
}


typedef struct {
  int num_combs;
  mus_any **combs;
  int num_allpasses;
  mus_any **allpasses;
  mus_any *onep;
} rev_info;

static rev_info *global_rev = NULL;

static void nrev(rev_info *r, mus_float_t *rins, mus_float_t *routs, int chans)
{
  mus_float_t rout, rin = 0.0;
  int i;
  for (i = 0; i < chans; i++) rin += rins[i];
  rout = mus_all_pass_unmodulated_noz(r->allpasses[3],
	   mus_one_pole(r->onep,
	     mus_all_pass_unmodulated_noz(r->allpasses[2],
	       mus_all_pass_unmodulated_noz(r->allpasses[1],
		 mus_all_pass_unmodulated_noz(r->allpasses[0],
	           mus_comb_unmodulated_noz(r->combs[0], rin) + 
	           mus_comb_unmodulated_noz(r->combs[1], rin) + 
	           mus_comb_unmodulated_noz(r->combs[2], rin) + 
	           mus_comb_unmodulated_noz(r->combs[3], rin) + 
	           mus_comb_unmodulated_noz(r->combs[4], rin) + 
	           mus_comb_unmodulated_noz(r->combs[5], rin))))));
  for (i = 0; i < chans; i++)
    routs[i] = mus_all_pass_unmodulated_noz(r->allpasses[i + 4], rout);
}


static mus_float_t *r_ins, *r_outs;
static int reverb_chans = 0;

static void reverb(rev_info *r, mus_float_t **rins, mus_float_t **outs, int ind)
{
  int i, chans;
  chans = reverb_chans;
  for (i = 0; i < chans; i++)
    {
      r_ins[i] = rins[i][ind];
      r_outs[i] = 0.0;
    }
  nrev(r, r_ins, r_outs, chans); 
  for (i = 0; i < chans; i++)
    outs[i][ind] += (r_outs[i]);
}


static void free_reverb(void) 
{
  rev_info *r;
  r = global_rev;
  if (r)
    {
      int i;
      if (r->combs)
	{
	  for (i = 0; i < r->num_combs; i++) 
	    if (r->combs[i]) 
	      mus_free(r->combs[i]);
	  free(r->combs);
	}
      if (r->onep) mus_free(r->onep);
      if (r->allpasses)
	{
	  for (i = 0; i < r->num_allpasses; i++) 
	    if (r->allpasses[i]) 
	      mus_free(r->allpasses[i]);
	  free(r->allpasses);
	}
      free(r);
    }
  global_rev = NULL;
}


static mus_float_t *comb_factors = NULL;

static rev_info *make_nrev(snd_info *sp, int chans) 
{
  /* Mike McNabb's nrev from Mus10 days (ca. 1978) */
  #define BASE_DLY_LEN 14
  static int base_dly_len[BASE_DLY_LEN] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 43, 37, 29, 19};
  static int dly_len[BASE_DLY_LEN];
  #define NREV_COMBS 6
  static mus_float_t nrev_comb_factors[NREV_COMBS] = {0.822, 0.802, 0.773, 0.753, 0.753, 0.733};
  mus_float_t srscale;
  int i, j, len;
  rev_info *r;

  if (!sp) return(NULL);

  srscale = sp->reverb_control_length * snd_srate(sp) / 25641.0;
  for (i = 0; i < BASE_DLY_LEN; i++) 
    dly_len[i] = get_prime((int)(srscale * base_dly_len[i]));
  r = (rev_info *)calloc(1, sizeof(rev_info));
  r->num_combs = NREV_COMBS;
  r->combs = (mus_any **)calloc(r->num_combs, sizeof(mus_any *));
  r->num_allpasses = 4 + chans;
  r->allpasses = (mus_any **)calloc(r->num_allpasses, sizeof(mus_any *));

  if (comb_factors) free(comb_factors);
  comb_factors = (mus_float_t *)calloc(r->num_combs, sizeof(mus_float_t));
  for (i = 0; i < r->num_combs; i++) 
    {
      comb_factors[i] = nrev_comb_factors[i];
      r->combs[i] = mus_make_comb(comb_factors[i] * sp->reverb_control_feedback, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    }

  r->onep = mus_make_one_pole(sp->reverb_control_lowpass, sp->reverb_control_lowpass - 1.0);

  for (i = 0, j = r->num_combs; i < 4; i++, j++) 
    r->allpasses[i] = mus_make_all_pass(-0.700, 0.700, dly_len[j], NULL, dly_len[j], MUS_INTERP_LINEAR);

  for (i = 0, j = 10; i < chans; i++)
    {
      if (j < BASE_DLY_LEN) 
	len = dly_len[j]; 
      else len = get_prime((int)(40 + mus_random(20.0)));
      r->allpasses[i + 4] = mus_make_all_pass(-0.700, 0.700, len, NULL, len, MUS_INTERP_LINEAR);
    }

  return(r);
}


static void make_reverb(snd_info *sp, int chans)
{ 
  reverb_chans = chans;
  global_rev = make_nrev(sp, chans);
}


static void set_reverb_filter_coeff(mus_float_t newval)
{
  if (global_rev)
    {
      mus_set_xcoeff(global_rev->onep, 0, newval);
      mus_set_ycoeff(global_rev->onep, 1, 1.0 - newval);
    }
}


static void set_reverb_comb_factors(mus_float_t val)
{
  int j;
  if (global_rev)
    for (j = 0; j < global_rev->num_combs; j++)
      mus_set_scaler(global_rev->combs[j], comb_factors[j] * val);
}


/* -------- contrast-enhancement -------- */

static mus_float_t contrast (dac_info *dp, mus_float_t amp, mus_float_t index, mus_float_t inval)
{
  return(amp * mus_contrast_enhancement(dp->contrast_amp * inval, index));
}



static mus_error_handler_t *old_error_handler;
static bool got_local_error = false;

static void local_mus_error(int type, char *msg)
{
  got_local_error = true;
  snd_error_without_format(msg);
}


mus_float_t *sample_linear_env(env *e, int order)
{
  /* used only for filter coeffs (env here is the frequency response curve) */
  mus_any *ge;
  int ordp;
  mus_float_t *data = NULL;
  mus_float_t last_x, step;

  last_x = e->data[(e->pts - 1) * 2];
  step = 2 * last_x / ((mus_float_t)order - 1);
  ordp = e->pts; 
  if (ordp < order) ordp = order;

  got_local_error = false;
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, 1000 * ordp, NULL);
  mus_error_set_handler(old_error_handler);

  if (!got_local_error)
    {
      int i, j;
      mus_float_t x;
      data = (mus_float_t *)malloc(order * sizeof(mus_float_t));
      for (i = 0, x = 0.0; i < order / 2; i++, x += step) 
	data[i] = mus_env_interp(x, ge);
      for (j = order / 2 - 1, i = order / 2; (i < order) && (j >= 0); i++, j--) 
	data[i] = data[j];
      mus_free(ge);
    }

  return(data);
}


static void free_dac_info(dac_info *dp, play_stop_t reason)
{
  bool looping;
  looping = (dp->stop_procedure == Xen_true);
  if (dp->stop_procedure_gc_loc != NOT_A_GC_LOC)
    {
      Xen_call_with_1_arg(dp->stop_procedure, 
			  C_int_to_Xen_integer((int)reason),
			  "play stop procedure");
      snd_unprotect_at(dp->stop_procedure_gc_loc);
      dp->stop_procedure = Xen_false;
      dp->stop_procedure_gc_loc = NOT_A_GC_LOC;
    }
  if (dp->func_gc_loc != NOT_A_GC_LOC)
    {
      snd_unprotect_at(dp->func_gc_loc);
      dp->func = Xen_false;
      dp->func_gc_loc = NOT_A_GC_LOC;
    }
  if (dp->a) {free(dp->a); dp->a = NULL; dp->a_size = 0;}
  dp->chn_fd = free_snd_fd(dp->chn_fd);
  if (dp->spd) free_expand(dp->spd);
  if (dp->src) mus_free(dp->src);
  if (dp->flt) mus_free(dp->flt);
  dp->sp = NULL;
  dp->cp = NULL;
  dp->type = DAC_NOTHING;
  free(dp);
  if ((looping) &&
      (reason == PLAY_COMPLETE))
    loop_play_selection();
}


static int dac_max_sounds = 0;
static dac_info **play_list = NULL;
#define INITIAL_MAX_SOUNDS 16
static int play_list_members = 0;
static int max_active_slot = -1;


/* -------------------------------- special control panel variables -------------------------------- */

typedef enum {DAC_EXPAND, DAC_EXPAND_RAMP, DAC_EXPAND_LENGTH, DAC_EXPAND_HOP, DAC_EXPAND_SCALER, 
	      DAC_CONTRAST_AMP, DAC_REVERB_FEEDBACK, DAC_REVERB_LOWPASS} dac_field_t;

static void dac_set_field(snd_info *sp, mus_float_t newval, dac_field_t field)
{
  /* if sp == NULL, sets globally */
  if (play_list)
    {
      if (field == DAC_REVERB_LOWPASS)
	{
	  if (global_rev)
	    set_reverb_filter_coeff(newval);
	}
      else
	{
	  if (field == DAC_REVERB_FEEDBACK)
	    {
	      if (global_rev)
		set_reverb_comb_factors(newval);
	    }
	  else
	    {
	      int i;
	      for (i = 0; i <= max_active_slot; i++)
		{
		  dac_info *dp;
		  dp = play_list[i];
		  if ((dp) && ((!sp) || (sp == dp->sp)))
		    {
		      int val;
		      switch (field)
			{
			case DAC_EXPAND: 
			  if (dp->spd)
			    mus_set_increment(dp->spd->gen, newval); 
			  break;

			case DAC_EXPAND_LENGTH: /* segment length */
			  if (dp->spd)
			    {
			      val = (int)(snd_srate(sp) * newval);
			      mus_set_length(dp->spd->gen, val);
			      mus_set_ramp(dp->spd->gen, (int)(val * sp->expand_control_ramp));
			    }
			  break;

			case DAC_EXPAND_RAMP: 
			  if (dp->spd)
			    {
			      val = (int)(newval * sp->expand_control_length * snd_srate(sp));
			      mus_set_ramp(dp->spd->gen, val); 
			    }
			  break;

			case DAC_EXPAND_HOP: /* output hop */
			  if (dp->spd)
			    {
			      val = (int)(snd_srate(sp) * newval);
			      mus_set_hop(dp->spd->gen, val); 
			      mus_set_increment(dp->spd->gen, sp->expand_control);
			    }
			  break;

			case DAC_EXPAND_SCALER:
			  if (dp->spd)
			    mus_set_scaler(dp->spd->gen, newval); 
			  break;

			case DAC_CONTRAST_AMP:
			  dp->contrast_amp = newval;
			  break;

			default:
			  break;
			}
		    }
		}
	    }
	}
    }
}


void dac_set_expand(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_EXPAND);}
void dac_set_expand_length(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_EXPAND_LENGTH);}
void dac_set_expand_ramp(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_EXPAND_RAMP);}
void dac_set_expand_hop(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_EXPAND_HOP);}
/* static void dac_set_expand_scaler(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_EXPAND_SCALER);} */ /* not currently accessible */
void dac_set_contrast_amp(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_CONTRAST_AMP);}
void dac_set_reverb_feedback(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_REVERB_FEEDBACK);}
void dac_set_reverb_lowpass(snd_info *sp, mus_float_t newval) {dac_set_field(sp, newval, DAC_REVERB_LOWPASS);}



/* -------------------------------- stop playing (remove from play-list) -------------------------------- */

typedef struct {
  int srate;                   /* output srate */
  int channels;                /* total output channels currently active */
  int framples;                /* samples per channel per output block */
  int devices;                 /* output devices active */
  int *chans_per_device;       /* channels sent to each active device */
  mus_sample_t out_samp_type;  /* output sample type */
  int slice;                   /* background process state (i.e. starting, running, quitting) */
  mus_long_t reverb_ring_framples; /* how long the reverb rings after the end (if reverb, of course) */

} dac_state;

static dac_state *snd_dacp = NULL;

static void free_dac_state(void)
{
  if (snd_dacp)
    {
      if (snd_dacp->chans_per_device) free(snd_dacp->chans_per_device);
      free(snd_dacp);
      snd_dacp = NULL;
    }
}

static bool dac_running = false;
static Xen play_hook;
static Xen start_playing_hook;
static Xen stop_playing_hook;
static Xen stop_playing_selection_hook;
static Xen start_playing_selection_hook;


#define MAX_DEVICES 8
static int dev_fd[MAX_DEVICES] = {-1, -1, -1, -1, -1, -1, -1, -1};

void cleanup_dac(void)
{
  /* called only from snd_exit_cleanly in snd-main.c */
  int i;
  if (dac_running) 
    for (i = 0; i < MAX_DEVICES; i++) 
      if (dev_fd[i] != -1) 
	{
	  mus_audio_close(dev_fd[i]);
	  dev_fd[i] = -1;
	}
  for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
  dac_running = false;
}


static bool something_is_playing(void)
{
  int i;
  if (play_list)
    for (i = 0; i < dac_max_sounds; i++)
      if (play_list[i]) return(true);
  return(false);
}


static void reflect_play_stop(snd_info *sp) 
{
#if (!USE_NO_GUI)
  set_control_panel_play_button(sp);
#endif
  set_open_file_play_button(false);
#if (!USE_NO_GUI)
  view_files_unplay();
#endif
  if (!something_is_playing())
    ss->tracking = false;
}


static void free_player_sound(snd_info *sp);

typedef enum {WITHOUT_TOGGLE, WITH_TOGGLE} dac_toggle_t;

static void stop_playing_with_toggle(dac_info *dp, dac_toggle_t toggle, with_hook_t with_hook, play_stop_t reason)
{
  snd_info *sp = NULL;
  bool sp_stopping = false;

  if ((!dp) || (!play_list)) return;
  sp = dp->sp;
  if ((sp) && (sp->inuse != SOUND_IDLE))
    {
      if (sp->playing > 0) sp->playing--;
      if (sp->playing == 0) sp_stopping = true;

      if ((sp_stopping) &&
	  (ss->tracking) &&
	  (sp->inuse == SOUND_NORMAL) && 
	  (sp->index >= 0))
	{
	  unsigned int i;
	  for (i = 0; i < sp->nchans; i++)
	    {
	      chan_info *cp;
	      cp = sp->chans[i];
	      if ((cp == dp->cp) ||
		  (sp->sync != 0))        /* don't move the cursor in a channel that isn't playing */
		{
		  if (with_tracking_cursor(ss) == TRACK_AND_STAY)
		    {
		      if (dp->cur_srate > 0.0)
			{
			  mus_long_t samp;
			  samp = current_location(dp->chn_fd) + ((snd_dacp) ? snd_dacp->framples : 0);
			  if (dp->selection)
			    {
			      if (samp > selection_end(cp))
				samp = selection_end(cp);
			    }
			  else
			    {
			      if (samp > current_samples(cp))
				samp = current_samples(cp);
			    }
			  cursor_sample(cp) = samp - 1;
			}
		      cp->original_left_sample = cursor_sample(cp) - (cp->original_window_size / 2);
		      if (cp->original_left_sample < 0)
			cp->original_left_sample = 0;
		    }
		  else cursor_sample(cp) = cp->original_cursor;
		  cursor_moveto_with_window(cp, cursor_sample(cp), cp->original_left_sample, cp->original_window_size);
		  update_graph(cp); 
		}
	    }
	}
      /* if ctrl-click play, c-t, c-q -> this flag is still set from aborted previous play, so clear at c-t (or c-g) */
    }
  else sp = NULL; /* don't free it as a player below */
  play_list[dp->slot] = NULL;
  play_list_members--;

  if (toggle == WITH_TOGGLE) 
    {
      switch (dp->type)
	{
	case DAC_CHANNEL:
	  if ((sp) && (sp->playing <= 0)) 
	    reflect_play_stop(sp);
	  break;
	case DAC_REGION:
	  if (dp->region != INVALID_REGION)
	    reflect_play_region_stop(dp->region);
	  break;
	case DAC_MIX:
	  if (dp->mix_id != INVALID_MIX_ID)
	    reflect_mix_play_stop();
	case DAC_XEN:
	  break;
	case DAC_NOTHING:
	  break;
	}
    }

  if (dp->slot == max_active_slot) max_active_slot--;
  if (with_hook == WITH_HOOK)
    {
      if (dp->type == DAC_CHANNEL)
	{
	  if (dp->selection)
	    {
	      reflect_play_selection_stop();
	      if (Xen_hook_has_list(stop_playing_selection_hook))
		run_hook(stop_playing_selection_hook, 
			 Xen_empty_list, 
			 S_stop_playing_selection_hook);
	    }
	  else
	    {
	      if ((sp_stopping) && (Xen_hook_has_list(stop_playing_hook)))
		run_hook(stop_playing_hook,
			 Xen_list_1(C_int_to_Xen_sound(sp->index)),
			 S_stop_playing_hook);
	    }
	}
    }

  if ((sp) && (is_player_sound(sp))) 
    {
      int k;
      bool free_ok = true;
      dp->sp = NULL; /* free_player_sound frees it */
      for (k = dp->slot + 1; k < dac_max_sounds; k++)
	{
	  dac_info *ndp;
	  ndp = play_list[k];
	  if ((ndp) &&
	      (ndp->sp) &&
	      (ndp->sp == sp))
	    {
	      free_ok = false;
	      break;
	    }
	}
      if (free_ok) 
	free_player_sound(sp); 
      sp = NULL;
    }
  free_dac_info(dp, reason); /* this will call the stop-function, if any */

  if ((sp) && (sp_stopping) && (sp->delete_me)) 
    {
#if USE_MOTIF || USE_GTK
      if (sp->delete_me != (void *)1) 
	clear_deleted_snd_info(sp->delete_me); /* for various file dialog play buttons */
#endif
      completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-xen.c */
    }
}


static void stop_playing(dac_info *dp, with_hook_t with_hook, play_stop_t reason) 
{
  stop_playing_with_toggle(dp, WITH_TOGGLE, with_hook, reason);
}


static idle_func_t dac_in_background(any_pointer_t ptr);
static idle_func_t dac_work_proc = 0; /* needed in gtk to force process to stop */


static void stop_playing_sound_with_toggle(snd_info *sp, dac_toggle_t toggle, with_hook_t with_hook, play_stop_t reason)
{
  /* this needs to scan all current play_list members and remove any that are referring
   * to sp, even indirectly (as through the current selection)
   */

  /* if reason = PLAY_STOP_CALLED, PLAY_C_T, or PLAY_C_G, we need to slam the output volumes to 0.0 (so the
   *    draining output is not played), then once we're sure output has stopped, put them back where they were.
   *    But, this action is sound-driver (OS) specific -- perhaps just hit MUS_AUDIO_AMP and hope?
   *    How to tell that on-card buffers are finally empty?  Wait 1 or 2 seconds?
   *
   * an added annoyance -- to get current amps, we have to make separate read call for each chan!
   */

  if ((sp) && (play_list))
    {
      int i;
      dac_info *dp;
      for (i = 0; i < dac_max_sounds; i++)
	if ((play_list[i]) && 
	    (sp == (play_list[i]->sp)))
	  {
	    dp = play_list[i];
	    play_list[i] = NULL;	  
	    stop_playing_with_toggle(dp, toggle, with_hook, reason);
	  }
      if ((play_list_members > 0) && (with_hook == WITH_HOOK))
	{
	  /* see comment below */
	  for (i = 0; i < dac_max_sounds; i++)
	    if ((play_list[i]) && 
		(sp == (play_list[i]->sp)))
	      {
		dp = play_list[i];
		play_list[i] = NULL;	  
		stop_playing_with_toggle(dp, toggle, WITHOUT_HOOK, reason); /* do it again but with hook disabled */
	      }
	}
    }
}


void stop_playing_sound(snd_info *sp, play_stop_t reason) 
{
  stop_playing_sound_with_toggle(sp, WITH_TOGGLE, WITH_HOOK, reason);
}


void stop_playing_sound_without_hook(snd_info *sp, play_stop_t reason) 
{
  stop_playing_sound_with_toggle(sp, WITH_TOGGLE, WITHOUT_HOOK, reason);
}


void stop_playing_sound_no_toggle(snd_info *sp, play_stop_t reason) 
{
  stop_playing_sound_with_toggle(sp, WITHOUT_TOGGLE, WITH_HOOK, reason);
}


static void stop_playing_all_sounds_1(with_hook_t with_hook, play_stop_t reason)
{
  if (play_list)
    {
      int i;
      dac_info *dp;
      for (i = 0; i < dac_max_sounds; i++)
	{
	  dp = play_list[i];
	  play_list[i] = NULL;	  
	  stop_playing(dp, with_hook, reason);
	  /* this can call stop_playing_hook which can make a new play_list member where the old one used to be! */
	}
      if ((play_list_members > 0) && (with_hook == WITH_HOOK))
	{
	  /* stop-playing-hook must have started a new player, but in this case,
	   *   the rest of Snd assumes the dac has stopped no matter what.  So,
	   *   we make a second pass with the hooks disabled.
	   */
	  for (i = 0; i < dac_max_sounds; i++)
	    {
	      dp = play_list[i];
	      play_list[i] = NULL;
	      stop_playing(dp, WITHOUT_HOOK, reason);
	    }
	}
    }
  if (snd_dacp)
    {
      snd_dacp->slice = 2;
      dac_in_background(NULL);   /* try to force completion (work proc might not be called above) */
    }
}


void stop_playing_all_sounds(play_stop_t reason) 
{
  stop_playing_all_sounds_1(WITH_HOOK, reason);
}


static void stop_playing_all_sounds_without_hook(play_stop_t reason) 
{
  stop_playing_all_sounds_1(WITHOUT_HOOK, reason);
}


void stop_playing_region(int n, play_stop_t reason)
{
  if (play_list)
    {
      int i;
      dac_info *dp;
      for (i = 0; i < dac_max_sounds; i++)
	if ((play_list[i]) &&
	    (play_list[i]->type == DAC_REGION) &&
	    (play_list[i]->region == n))
	  {
	    dp = play_list[i];
	    play_list[i] = NULL;	  
	    stop_playing(dp, WITHOUT_HOOK, reason);
	  }
    }
}


/* -------------------------------- play (add to play-list) -------------------------------- */

static int find_slot_to_play(void)
{
  int i, old_size;

  if (!play_list)
    {
      dac_max_sounds = INITIAL_MAX_SOUNDS;
      play_list = (dac_info **)calloc(dac_max_sounds, sizeof(dac_info *));
    }

  for (i = 0; i < dac_max_sounds; i++) 
    if (!play_list[i]) 
      return(i);

  old_size = dac_max_sounds;
  dac_max_sounds += INITIAL_MAX_SOUNDS;
  play_list = (dac_info **)realloc(play_list, dac_max_sounds * sizeof(dac_info *));
  for (i = old_size; i < dac_max_sounds; i++) play_list[i] = NULL;

  return(old_size);
}


static dac_info *make_dac_info(int slot, chan_info *cp, snd_info *sp, snd_fd *fd, mus_long_t end, int out_chan, dac_source_t type, Xen func)
{
  dac_info *dp;

  dp = (dac_info *)calloc(1, sizeof(dac_info)); /* only place dac_info is created */
  dp->stop_procedure = Xen_false;
  dp->stop_procedure_gc_loc = NOT_A_GC_LOC;
  dp->region = INVALID_REGION;
  dp->mix_id = INVALID_MIX_ID;
  dp->direction = 1;
  dp->type = type; /* dac needs to know how to get input before calling mus_make_src below */
  if (type == DAC_NOTHING)
    dp->dac_sample = dac_no_sample;
  else
    {
      if (type == DAC_XEN)
	{
	  dp->dac_sample = dac_xen_sample;
	  dp->func = func;
	  dp->func_gc_loc = snd_protect(func);
	}
      else 
	{
	  dp->dac_sample = dac_read_sample;
	  dp->func = Xen_false;
	  dp->func_gc_loc = NOT_A_GC_LOC;
	}
    }
  dp->a = NULL;
  dp->a_size = 0;
  dp->audio_chan = out_chan;
  dp->never_sped = true;
  dp->chn_fd = fd;
  dp->sp = sp;
  dp->cp = cp;

  /* next block may get input */
  if (sp)
    {
      dp->expanding = sp->expand_control_on;
      dp->filtering = ((sp->filter_control_on) && (sp->filter_control_order > 0));
      dp->reverbing = sp->reverb_control_on;
      dp->contrast_amp = sp->contrast_control_amp;

      if ((!(snd_feq(sp->speed_control, 1.0))) ||
	  (sp->speed_control_direction == -1))
	{
	  dp->src = mus_make_src(&dac_src_input_as_needed, sp->speed_control, sinc_width(ss), (void *)dp);
	  dp->direction = sp->speed_control_direction;
	}

      if (dp->expanding) 
	{
	  dp->spd = (spd_info *)make_expand(sp, sp->expand_control, dp);
	  dp->expand_ring_framples = (int)(snd_srate(sp) * sp->expand_control * sp->expand_control_length * 2);
	}

      if (dp->filtering)
	{
	  sp->filter_control_changed = false;
	  if (!(sp->filter_control_envelope)) 
	    dp->filtering = false;
	  else
	    {
	      mus_float_t *data = NULL;
	      data = sample_linear_env(sp->filter_control_envelope, sp->filter_control_order);
	      if (data)
		{
		  dp->flt = make_flt(dp, sp->filter_control_order, data);
		  free(data);
		}
	      else dp->filtering = false;
	    }
	}
    }

  play_list_members++;
  dp->end = end;
  play_list[slot] = dp;
  dp->slot = slot;
  if (max_active_slot < slot) max_active_slot = slot;
  if (sp)
    {
      dp->cur_srate = sp->speed_control * sp->speed_control_direction;
      if (!(snd_feq(dp->cur_srate, 1.0))) dp->never_sped = false;
      dp->cur_amp = AMP_CONTROL(sp, dp);
      dp->cur_index = sp->contrast_control;
      dp->cur_exp = sp->expand_control;
      dp->cur_rev = sp->reverb_control_scale;
    }

  return(dp);
}


static void start_dac(int srate, int channels, play_process_t background, mus_float_t decay)
{
  int i;
  /* look for channel folding cases etc */
  /* channels = how many output audio chans we have; dac_combines_channels sets whether to wrap or muffle chans outside this limit */

  if (with_tracking_cursor(ss) != DONT_TRACK)
    ss->tracking = true;

  for (i = 0; i <= max_active_slot; i++)
    {
      dac_info *dp;
      dp = play_list[i];
      if ((dp) && (dac_running))                          /* dac_running also if apply */
	{
	  /* in the "normal" (non-apply) case the reverb allocation is deferred until we're sure about the number of output channels */
	  if ((dp->reverbing) && 
	      (dp->sp) && 
	      (!global_rev))
	    make_reverb(dp->sp, channels);
	  if (snd_dacp)
	    {
	      if (dp->audio_chan >= snd_dacp->channels)      /* if dac_running, the number of channels has already been set and won't change */
		{
		  if (dac_combines_channels(ss))
		    dp->audio_chan %= snd_dacp->channels;
		  else stop_playing(dp, WITHOUT_HOOK, PLAY_NO_CHANNEL);
		}
	    }
	}
    }
  /* any number of sounds can be piling into the dac buffers with new ones added at any time
   *   (this is the play_list -- an array of dac_info pointers, each one a separate channel rendition)
   */
  if (!dac_running)
    {
      if (snd_dacp) free_dac_state();
      snd_dacp = (dac_state *)calloc(1, sizeof(dac_state));
      snd_dacp->slice = 0;
      snd_dacp->srate = srate;
      snd_dacp->out_samp_type = MUS_AUDIO_COMPATIBLE_SAMPLE_TYPE;
      if (snd_dacp->srate <= 0) snd_dacp->srate = 44100;
      snd_dacp->channels = channels;
      if (dac_size(ss) > 0)
	snd_dacp->framples = dac_size(ss);
      else snd_dacp->framples = 256;
      snd_dacp->devices = 1;  /* just a first guess */
      snd_dacp->reverb_ring_framples = (mus_long_t)(srate * decay);
      if (background == IN_BACKGROUND) 
	dac_work_proc = BACKGROUND_ADD(dac_in_background, NULL);
      else
	{
	  /* here we want to play as an atomic (not background) action */
	  while (dac_in_background(NULL) == BACKGROUND_CONTINUE)
	    check_for_event(); /* need to be able to C-g out of this */
	}
    }
}


/* pos = to_c_edit_position(cp, edpos, caller, arg_pos); */

static dac_info *add_channel_to_play_list(chan_info *cp, snd_info *sp, mus_long_t start, mus_long_t end, int pos, int out_chan)
{
  /* if not sp, control panel is ignored */
  int slot;
  read_direction_t direction = READ_FORWARD;
  mus_long_t beg = 0;
  snd_fd *sf;

  if (pos == AT_CURRENT_EDIT_POSITION) pos = cp->edit_ctr;
  if (sp)
    {
      if (sp->speed_control_direction == 1) 
	{
	  if (start >= cp->edits[pos]->samples) return(NULL);
	  direction = READ_FORWARD; 
	  beg = start;
	}
      else 
	{
	  direction = READ_BACKWARD;
	  if (start < end)
	    {
	      mus_long_t tmp;
	      tmp = start;
	      start = end;
	      end = tmp;
	    }
	  if ((start == 0) || (start >= cp->edits[pos]->samples)) /* don't be pedantic about the start point */
	    beg = cp->edits[pos]->samples - 1;
	  else beg = start;
	  if ((end == 0) || (end >= (cp->edits[pos]->samples)))
	    end = NO_END_SPECIFIED;
	}
    }

  slot = find_slot_to_play();
  if (slot == -1) return(NULL);

  sf = init_sample_read_any(beg, cp, direction, pos);
  if (sf)
    {
      if (sp) 
	{
	  sp->playing++;
	  if (((with_tracking_cursor(ss) != DONT_TRACK) || (ss->tracking)) &&
              (!(is_player_sound(sp))) &&
	      (sp->inuse == SOUND_NORMAL))
	    {
	      cp->original_cursor = cursor_sample(cp);
	      if (cp->axis)
		{
		  cp->original_left_sample = cp->axis->losamp;
		  cp->original_window_size = cp->axis->hisamp - cp->axis->losamp;
		}
	      cp->cursor_on = true;
	      cursor_moveto_without_verbosity(cp, start);
#if USE_MOTIF
	      if (cp->chan_widgets)
		update_graph(cp);
#endif
	    }
	}
      return(make_dac_info(slot, cp, sp, sf, end, out_chan, DAC_CHANNEL, Xen_false));
    }
  return(NULL);
}


static dac_info *add_region_channel_to_play_list(int region, int chan, mus_long_t beg, mus_long_t end, int out_chan)
{
  int slot;
  snd_fd *fd;

  slot = find_slot_to_play();
  if (slot == -1) return(NULL);

  fd = init_region_read(beg, region, chan, READ_FORWARD);
  if (fd)
    {
      dac_info *dp;
      dp = make_dac_info(slot, fd->cp, NULL, fd, end, out_chan, DAC_REGION, Xen_false);
      if (dp) dp->region = region;
      return(dp);
    }
  return(NULL);
}


void play_region_1(int region, play_process_t background, Xen stop_proc)
{
  /* just plays region (not current selection) -- no control panel etc */
  int chans, i;
  dac_info *dp = NULL, *rtn_dp = NULL;

  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (!(region_ok(region))) return;
  chans = region_chans(region);
  if (chans == 0) return;

  for (i = 0; i < chans; i++) 
    {
      dp = add_region_channel_to_play_list(region, i, 0, NO_END_SPECIFIED, i); /* i = out chan */
      if (dp) 
	{
	  if (!rtn_dp) rtn_dp = dp;
	}
    }
  if (rtn_dp)
    {
      rtn_dp->stop_procedure = stop_proc;
      if (Xen_is_procedure(stop_proc))
	rtn_dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      start_dac(region_srate(region), chans, background, DEFAULT_REVERB_CONTROL_DECAY);
    }
}


void play_region(int region, play_process_t background)
{
  play_region_1(region, background, Xen_false);
}


bool add_mix_to_play_list(mix_state *ms, chan_info *cp, mus_long_t beg_within_mix, bool start_playing)
{
  int slot;
  slot = find_slot_to_play();
  if (slot != -1)
    {
      snd_fd *fd;
      fd = make_virtual_mix_reader(cp, beg_within_mix, ms->len - beg_within_mix, ms->index, 1.0, READ_FORWARD);
      if (fd)
	{
	  dac_info *dp;
	  dp = make_dac_info(slot, cp, NULL, fd, NO_END_SPECIFIED, 0, DAC_MIX, Xen_false);
	  if (dp)
	    {
	      dp->mix_id = ms->index; /* any valid mix id will do */
	      if (start_playing)
		start_dac(snd_srate(cp->sound), 1, NOT_IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY);
	      return(true);
	    }
	}
      else free_snd_fd(fd);
    }
  return(false);
}


/* (play (let ((osc (make-oscil 440.0)) 
               (samp 0)) 
           (lambda () 
             (if (> samp 20000)
		 #f
		 (begin
             (set! samp (+ samp 1))
             (* .5 (oscil osc))))))) ; you can use explicit control panel accessors
 */

static bool add_zeros_to_play_list(int srate, int chans)
{
  int slot;
  slot = find_slot_to_play();
  if (slot != -1)
    {
      dac_info *dp;
      dp = make_dac_info(slot, NULL, NULL, NULL, NO_END_SPECIFIED, 0, DAC_NOTHING, Xen_false);
      if (dp)
	{
	  start_dac(srate, chans, IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY);
	  return(true);
	}
    }
  return(false);
}


static bool add_xen_to_play_list(Xen func)
{
  int slot;
  slot = find_slot_to_play();
  if (slot != -1)
    {
      dac_info *dp;
      dp = make_dac_info(slot, NULL, NULL, NULL, NO_END_SPECIFIED, 0, DAC_XEN, func);
      if (dp)
	{
#if USE_NO_GUI
	  start_dac((int)mus_srate(), 1, NOT_IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY);
#else
	  start_dac((int)mus_srate(), 1, IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY);
#endif
	  /*                          ^ output channels */
	  return(true);
	}
    }
  return(false);
}


static bool call_start_playing_hook(snd_info *sp)
{
  if ((Xen_hook_has_list(start_playing_hook)) &&
      (Xen_is_true(run_or_hook(start_playing_hook,
			      Xen_list_1(C_int_to_Xen_sound(sp->index)), /* this can be 123456 (TEMP_SOUND_INDEX in snd-file.c) -- View:Files dialog play button */
			      S_start_playing_hook))))
    {
      reflect_play_stop(sp);           /* turns off buttons */
      if (sp->delete_me) 
	completely_free_snd_info(sp);  /* dummy snd_info struct for (play "filename") in snd-xen.c */
      return(true);
    }
  return(false);
}


static bool call_start_playing_selection_hook(void)
{
  return((Xen_hook_has_list(start_playing_selection_hook)) &&
	 (Xen_is_true(run_or_hook(start_playing_selection_hook,
				 Xen_empty_list,
				 S_start_playing_selection_hook))));
}


static dac_info *play_channel_1(chan_info *cp, mus_long_t start, mus_long_t end, play_process_t background, 
				int pos, Xen stop_proc, int out_chan)
{
  /* just plays one channel (ignores possible sync), includes start-hook */
  snd_info *sp = NULL;
  dac_info *dp = NULL;

  sp = cp->sound;
  if (sp->inuse == SOUND_IDLE) return(NULL);
  if (call_start_playing_hook(sp)) return(NULL);

  dp = add_channel_to_play_list(cp, sp, start, end, pos, out_chan);
  if (dp) 
    {
      dp->stop_procedure = stop_proc;
      if (Xen_is_procedure(stop_proc))
	dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      set_play_button(sp, true);
      start_dac(snd_srate(sp), 1, background, sp->reverb_control_decay);
    }
  return(dp);
}


void play_channel(chan_info *cp, mus_long_t start, mus_long_t end)
{
  play_channel_1(cp, start, end, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION, Xen_false, cp->chan);
}


static dac_info *play_sound_1(snd_info *sp, mus_long_t start, mus_long_t end, play_process_t background, 
			      Xen edpos, Xen stop_proc, const char *caller, int arg_pos)
{
  /* just plays one sound (ignores possible sync) */
  unsigned int i;
  dac_info *dp = NULL, *rtn_dp = NULL;

  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return(NULL);
  if (sp->inuse == SOUND_IDLE) return(NULL);
  if (call_start_playing_hook(sp)) return(NULL);
  for (i = 0; i < sp->nchans; i++)
    {
      int pos;
      pos  = to_c_edit_position(sp->chans[i], edpos, caller, arg_pos);
      dp = add_channel_to_play_list(sp->chans[i], sp, start, end, pos, i); /* i = out chan */
      if ((dp) && (!rtn_dp)) rtn_dp = dp;
    }
  if (rtn_dp)
    {
      rtn_dp->stop_procedure = stop_proc;
      if (Xen_is_procedure(stop_proc))
	rtn_dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      set_play_button(sp, true);
      start_dac(snd_srate(sp), sp->nchans, background, sp->reverb_control_decay);
    }
  return(rtn_dp);
}


void play_sound(snd_info *sp, mus_long_t start, mus_long_t end)
{
  play_sound_1(sp, start, end, IN_BACKGROUND, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), Xen_false, NULL, 0);
}


static dac_info *play_channels_1(chan_info **cps, int chans, mus_long_t *starts, mus_long_t *ur_ends, play_process_t background, 
				 Xen edpos, bool selection, Xen stop_proc, const char *caller, int arg_pos)
{
  /* ends can be NULL */
  int i;
  snd_info *sp = NULL;
  dac_info *dp = NULL, *rtn_dp = NULL;
  mus_long_t *ends;

  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return(NULL);
  if (chans <= 0) return(NULL);

  if (!selection)
    {
      if (call_start_playing_hook(cps[0]->sound)) return(NULL);
    }
  else 
    {
      if (call_start_playing_selection_hook()) return(NULL);
    }

  if (ur_ends)
    ends = ur_ends;
  else
    {
      ends = (mus_long_t *)calloc(chans, sizeof(mus_long_t));
      for (i = 0; i < chans; i++) 
	ends[i] = NO_END_SPECIFIED;
    }

  sp = cps[0]->sound;
  for (i = 0; i < chans; i++) 
    {
      int pos;
      pos  = to_c_edit_position(cps[i], edpos, caller, arg_pos);
      dp = add_channel_to_play_list(cps[i], sp, starts[i], ends[i], pos, i);
      if (dp) 
	{
	  if (!rtn_dp) rtn_dp = dp;
	  if (selection) dp->selection = true;
	}
    }
  if (!ur_ends) free(ends);
  if ((sp) && (rtn_dp)) 
    {
      rtn_dp->stop_procedure = stop_proc;
      if (Xen_is_procedure(stop_proc))
	rtn_dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      set_play_button(sp, true);
      start_dac(snd_srate(sp), chans, background, sp->reverb_control_decay);
    }
  return(rtn_dp);
}


void play_channels(chan_info **cps, int chans, mus_long_t *starts, mus_long_t *ur_ends, 
		   play_process_t background, Xen edpos, bool selection, const char *caller, int arg_pos)
{
  play_channels_1(cps, chans, starts, ur_ends, background, edpos, selection, Xen_false, caller, arg_pos);
}


void play_channel_with_sync(chan_info *cp, mus_long_t start, mus_long_t end)
{
  snd_info *sp;
  sync_info *si;
  mus_long_t *ends = NULL;
  int i;

  sp = cp->sound;
  if ((sp->sync == 0) ||
      (is_player_sound(sp)))
    {
      play_channel(cp, start, end);
      return;
    }

  si = snd_sync(sp->sync);
  if (end != NO_END_SPECIFIED)
    {
      ends = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
      for (i = 0; i < si->chans; i++) ends[i] = end;
    }
  for (i = 0; i < si->chans; i++) si->begs[i] = start;

  play_channels_1(si->cps, si->chans, si->begs, ends, IN_BACKGROUND, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), false, Xen_false, NULL, 0);

  free_sync_info(si);
  if (ends) free(ends);
}


static dac_info *play_selection_1(play_process_t background, Xen stop_proc)
{
  /* just plays the current selection */
  dac_info *dp = NULL;

  if (selection_is_active())
    {
      sync_info *si = NULL;
      si = selection_sync();
      if (si)
	{
	  int i;
	  mus_long_t *ends;

	  ends = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
	  for (i = 0; i < si->chans; i++) 
	    ends[i] = si->begs[i] + selection_len();

	  dp = play_channels_1(si->cps, si->chans, si->begs, ends, background, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), true, stop_proc, NULL, 0);
	  /* dp->dac_sample is the reader */
	  free_sync_info(si); /* does not free samplers */
	  free(ends);
	}
    }
  return(dp);
}


void play_selection(play_process_t background)
{
  play_selection_1(background, Xen_false);
}


void loop_play_selection(void)
{
  play_selection_1(IN_BACKGROUND, Xen_true);
}


/* -------------------------------- process samples and write to DAC -------------------------------- */

#define NO_CHANGE 0
#define JUST_AMP 1
#define JUST_SPEED 2
#define ALL_CHANGES 3

static int choose_dac_op(dac_info *dp, snd_info *sp)
{
  if (!sp) return(NO_CHANGE);
  if ((dp->expanding) || (dp->filtering) || (dp->reverbing) || (sp->contrast_control_on)) 
    return(ALL_CHANGES);
  else
    {
      if ((sp->speed_control_direction != 1) || (!(snd_feq(sp->speed_control, 1.0))) || (!(snd_feq(dp->cur_srate, 1.0))))
	return(JUST_SPEED);
      else
	{
	  if ((AMP_CONTROL(sp, dp) == dp->cur_amp) && (snd_feq(AMP_CONTROL(sp, dp), 1.0)))
	    return(NO_CHANGE);
	  else return(JUST_AMP);
	}
    }
}

static int cursor_time;
/* can't move cursor on each dac buffer -- causes clicks */

static bool dac_pausing = false;

void toggle_dac_pausing(void) {dac_pausing = (!dac_pausing); play_button_pause(dac_pausing);} /* only below and snd-kbd.c */
bool play_in_progress(void) {return(play_list_members > 0);}


static unsigned char **audio_bytes = NULL;
static int audio_bytes_size = 0;
static int audio_bytes_devices = 0;

static mus_float_t **dac_buffers = NULL;
static int dac_buffer_size = 0;
static int dac_buffer_chans = 0; /* chans allocated */
static mus_float_t **rev_ins;

#define WRITE_TO_DAC 1
#define WRITE_TO_FILE 0

static int fill_dac_buffers(int write_ok)
{
  /* return value used only by Apply */
  int i;
  bool cursor_change = false;
  int framples;
  snd_info *sp;

  framples = snd_dacp->framples;
  /* clear buffers */
  for (i = 0; i < snd_dacp->channels; i++) 
    mus_clear_floats(dac_buffers[i], framples);
  if (global_rev)
    for (i = 0; i < snd_dacp->channels; i++) 
      mus_clear_floats(rev_ins[i], framples);

  if (dac_pausing) 
    cursor_change = false;
  else
    {
      dac_info *dp;
      mus_float_t *buf;
      mus_float_t *revin;

      if (Xen_hook_has_list(play_hook))
	run_hook(play_hook, 
		 Xen_list_1(C_int_to_Xen_integer(framples)),
		 S_play_hook);
      cursor_time += framples;
      cursor_change = (cursor_time >= (int)(snd_dacp->srate * cursor_update_interval(ss)));
      /* the check for hooks and so on has to be separate from the fill loop to keep everything synchronized across stop-function replay requests etc */

      /* first check for disasters (user closed sound while playing it or running Apply!) */
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      sp = dp->sp; /* can be nil if region playing */
	      if ((sp) && ((sp->inuse == SOUND_IDLE) || (sp->playing == 0)))
		stop_playing(dp, WITHOUT_HOOK, PLAY_CLOSE); 
	    }
	}
      /* now read currently active chans and update locations -- don't call any hooks or stop-functions! */
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      int j;
	      mus_float_t amp, incr, sr, sincr, ind, indincr, rev, revincr, fval;
	      if (dp->audio_chan >= snd_dacp->channels) /* this can happen if we're playing 1 chan, try to add 2 chans */
		{
		  if (dac_combines_channels(ss))
		    dp->audio_chan %= snd_dacp->channels;
		  else continue;
		}

	      /* check for moving cursor */
	      sp = dp->sp;                             /* can be nil if region playing */
	      if ((sp) && 
		  (ss->tracking) &&
		  (cursor_change) && 
		  (dp->chn_fd) &&
		  (!(dp->chn_fd->at_eof)) &&
#if (!USE_NO_GUI)
		  (dp->cp->chan_widgets) &&             /* nil if play_file */
#endif
		  (dp->chn_fd->cb))
		{
		  mus_long_t loc;
		  bool old_just_zero;
		  old_just_zero = dp->cp->just_zero;
		  dp->cp->just_zero = true;
		  loc = current_location(dp->chn_fd);
		  loc -= (int)(cursor_location_offset(ss) * dp->cur_srate); /* should work in either direction */
		  cursor_moveto_without_verbosity(dp->cp, loc);
		  dp->cp->just_zero = old_just_zero;
		}

	      /* add a buffer's worth from the current source into dp->audio_chan */
	      buf = dac_buffers[dp->audio_chan];
	      if (!buf) continue;
	      revin = rev_ins[dp->audio_chan];
	      switch (choose_dac_op(dp, sp))
		{
		case NO_CHANGE:
		  /* simplest case -- no changes at all */
		  for (j = 0; j < framples; j++)
		    buf[j] += (mus_float_t)((*(dp->dac_sample))(dp));
		  break;

		case JUST_AMP:
		  /* AMP_CONTROL(sp, dp) is current UI value, dp->cur_amp is current local value */
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (mus_float_t)(framples);
		  for (j = 0; j < framples; j++, amp += incr) 
		    buf[j] += (mus_float_t)(amp * (*(dp->dac_sample))(dp));
		  dp->cur_amp = amp;
		  break;

		case JUST_SPEED:
		  /* includes amp changes */
		  /* sp->speed_control is current UI value, dp->cur_srate is current local value */
		  dp->never_sped = false;
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (mus_float_t)(framples);
		  sr = dp->cur_srate;
		  sincr = (sp->speed_control * sp->speed_control_direction - sr) / (mus_float_t)(framples);
		  if ((sr != 0.0) || (sincr != 0.0))
		    {
		      if (dp->src)
			{
			  if ((amp == 1.0) && (incr == 0.0) && (sincr == 0.0))
			    {
			      mus_set_increment(dp->src, sr);
			      mus_src_to_buffer(dp->src, &dac_src_input_as_needed, buf, framples);
			    }
			  else
			    {
			      for (j = 0; j < framples; j++, amp += incr, sr += sincr) 
				{
				  mus_set_increment(dp->src, sr);
				  buf[j] += (amp * mus_src(dp->src, 0.0, &dac_src_input_as_needed));
				}
			    }
			}
		      else
			{
			  for (j = 0; j < framples; j++, amp += incr, sr += sincr) 
			    buf[j] += (amp * speed(dp, sr));
			}
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  break;

		case ALL_CHANGES:
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (mus_float_t)(framples);
		  sr = dp->cur_srate;
		  sincr = (sp->speed_control * sp->speed_control_direction - sr) / (mus_float_t)(framples);
		  if ((sincr != 0.0) || (!(snd_feq(sr, 1.0)))) dp->never_sped = false;
		  ind = dp->cur_index;
		  indincr = (sp->contrast_control - ind) / (mus_float_t)(framples);
		  rev = dp->cur_rev;
		  revincr = (sp->reverb_control_scale - rev) / (mus_float_t)(framples);
		  if ((dp->filtering) && (sp->filter_control_changed))
		    {
		      mus_float_t *data = NULL;
		      data = sample_linear_env(sp->filter_control_envelope, sp->filter_control_order);
		      if (data)
			{
			  /* check all chans in sp before clearing filter_order_changed flag */
			  int j;
			  for (j = i; j <= max_active_slot; j++)
			    {
			      dac_info *ndp;
			      ndp = play_list[j];
			      if ((ndp) && 
				  (ndp->sp == sp) && 
				  (ndp->filtering))
				{
				  if (sp->filter_control_order > ndp->a_size) /* need more room in dp->a == flt->xcoeffs and flt->state */
				    {
				      free(ndp->a);
				      ndp->a_size = sp->filter_control_order;
				      ndp->a = (mus_float_t *)calloc(ndp->a_size, sizeof(mus_float_t));
				    }
				  mus_make_fir_coeffs(sp->filter_control_order, data, ndp->a);      /* fill dp->a with new coeffs */
				  mus_filter_set_xcoeffs(ndp->flt, ndp->a);                         /* tell gen about them */
				  if (mus_filter_set_order(ndp->flt, sp->filter_control_order) < 0) /* fixup gen's order (and internal state array) */
				    snd_warning("trouble in filter (order not changed?)");
				}
			    }
			  free(data);
			}
		      sp->filter_control_changed = false;
		    }
		  if (dp->expanding)
		    {
		      mus_float_t ex, exincr;
		      ex = dp->cur_exp;
		      exincr = (sp->expand_control - ex) / (mus_float_t)(framples);
		      for (j = 0; j < framples; j++, amp += incr, sr += sincr, ind += indincr, ex += exincr, rev += revincr) 
			{
			  fval = expand(dp, sr, ex);
			  if (sp->contrast_control_on) fval = contrast(dp, amp, ind, fval); else fval *= amp;
			  if (dp->filtering) fval = mus_fir_filter(dp->flt, fval);
			  if (dp->reverbing) revin[j] += fval * rev;
			  buf[j] += fval;
			}
		      dp->cur_exp = ex;
		    }
		  else
		    {
		      if (dp->filtering)
			{
			  for (j = 0; j < framples; j++, amp += incr, sr += sincr, ind += indincr, rev += revincr) 
			    {
			      fval = speed(dp, sr);
			      if (sp->contrast_control_on) fval = contrast(dp, amp, ind, fval); else fval *= amp;
			      fval = mus_fir_filter(dp->flt, fval);
			      if (dp->reverbing) revin[j] += fval * rev;
			      buf[j] += fval;
			    }
			}
		      else
			{
			  if (sp->contrast_control_on)
			    {
			      for (j = 0; j < framples; j++, amp += incr, sr += sincr, ind += indincr, rev += revincr) 
				{
				  fval = contrast(dp, amp, ind, speed(dp, sr));
				  if (dp->reverbing) revin[j] += fval * rev;
				  buf[j] += fval;
				}
			    }
			  else
			    {
			      if (dp->never_sped)
				{
				  for (j = 0; j < framples; j++, amp += incr, rev += revincr) 
				    {
				      fval = amp * (*(dp->dac_sample))(dp);
				      revin[j] += fval * rev;
				      buf[j] += fval;
				    }
				}
			      else
				{
				  for (j = 0; j < framples; j++, amp += incr, sr += sincr, rev += revincr) 
				    {
				      fval = amp * speed(dp, sr);
				      revin[j] += fval * rev;
				      buf[j] += fval;
				    }
				}
			    }
			}
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  dp->cur_rev = rev;
		  dp->cur_index = ind;
		  break;
		}
	      if ((dp->end != NO_END_SPECIFIED) && (dp->end != 0))
		{
		  if ((!dp->chn_fd->cb) ||
		      ((dp->sp->speed_control_direction > 0) && 
		       ((dp->chn_fd->at_eof) || 
			(dp->end <= current_location(dp->chn_fd)))) ||
		      ((dp->sp->speed_control_direction < 0) && 
			(dp->end > current_location(dp->chn_fd))))
		    dp->end = 0;
		}
	    }
	} /* fill-case loop through max_active_slot */

      if (global_rev) 
	for (i = 0; i < framples; i++)
	  reverb(global_rev, rev_ins, dac_buffers, i);

      /* now hooks, stop-function etc */
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      /* check for EOF or specified end point */
	      if (write_ok == WRITE_TO_DAC) /* not Apply */
		{
		  if (dp->end == 0)
		    stop_playing(dp, WITH_HOOK, PLAY_COMPLETE);
		  else
		    {
		      if ((dp->chn_fd) &&
			  (dp->chn_fd->at_eof))
			{
			  if (!(dp->expanding))
			    stop_playing(dp, WITH_HOOK, PLAY_COMPLETE);
			  else
			    {
			      dp->expand_ring_framples -= framples;
			      if (dp->expand_ring_framples <= 0)
				stop_playing(dp, WITH_HOOK, PLAY_COMPLETE);
			    }
			}
		    }
		}
	      else /* Apply, always sets the end point explicitly */
		{
		  if (dp->end == 0)
		    {
		      stop_playing_all_sounds_without_hook(PLAY_COMPLETE);
		      play_list_members = 0; 
		      max_active_slot = -1;
		    }
		}
	    }
	} /* stop-case loop through max_active_slot */

      if ((global_rev) &&
	  (snd_dacp) &&
	  (play_list_members == 0))
	{
	  snd_dacp->reverb_ring_framples -= framples;
	  if (snd_dacp->reverb_ring_framples <= 0) 
	    free_reverb();
	}
    }
  /* now parcel these buffers out to the available devices */

  if (snd_dacp)
    {
#if (HAVE_OSS || HAVE_ALSA)
      if (write_ok == WRITE_TO_DAC) 
	{
	  mus_float_t **dev_bufs;
	  dev_bufs = dac_buffers;
	  for (i = 0; i < snd_dacp->devices; i++)
	    if (dev_fd[i] != -1)
	      {
		mus_file_write_buffer(snd_dacp->out_samp_type,
				      0, framples - 1,
				      snd_dacp->chans_per_device[i],
				      dev_bufs,
				      (char *)(audio_bytes[i]),
				      clipping(ss));
		dev_bufs += snd_dacp->chans_per_device[i];
	      }
	  for (i = 0; i < snd_dacp->devices; i++)
	    if (dev_fd[i] != -1)
	      {
		int bytes;
		bytes = snd_dacp->chans_per_device[i] * framples * mus_bytes_per_sample(snd_dacp->out_samp_type);
		mus_audio_write(dev_fd[i], (char *)(audio_bytes[i]), bytes);
	      }
	}
#else
      if (write_ok == WRITE_TO_DAC) 
	{
	  int bytes;
	  mus_file_write_buffer(snd_dacp->out_samp_type, 0, framples - 1, snd_dacp->channels, dac_buffers, (char *)(audio_bytes[0]), clipping(ss));
	  bytes = snd_dacp->channels * framples * mus_bytes_per_sample(snd_dacp->out_samp_type);
	  mus_audio_write(dev_fd[0], (char *)(audio_bytes[0]), bytes);
	}
#endif
    }
  if (cursor_change) cursor_time = 0;
  return(framples);
}


/* -------------------------------- specialize mus_print -------------------------------- */

static mus_print_handler_t *old_dac_printer = NULL;
static char *last_print = NULL;

static void dac_mus_print(char *msg)
{
  if (last_print) free(last_print);
  last_print = mus_strdup(msg);
  (*old_dac_printer)(msg);
}


static void set_dac_print(void)
{
  if (last_print) free(last_print);
  last_print = NULL;
  if (old_dac_printer != dac_mus_print)
    old_dac_printer = mus_print_set_handler(dac_mus_print);
}


static void unset_dac_print(void)
{
  mus_print_set_handler(old_dac_printer);
}


#if WITH_AUDIO
static const char *describe_dac(void)
{
  /* this is only called in dac_error and only then upon a failed mus_audio_open_output */
  int players = 0, i;
  dac_info *ptr = NULL;
  for (i = 0; i < dac_max_sounds; i++) 
    if (play_list[i]) 
      {
	ptr = play_list[i]; 
	players++;
      }
  if ((players == 1) && 
      (ptr->sp))
    return((const char *)(ptr->sp->short_filename));
  return("");
}
#endif


static void dac_error(void)
{
  stop_playing_all_sounds_without_hook(PLAY_ERROR);
#if WITH_AUDIO
  snd_error("can't play %s: %s",
	    describe_dac(),
	    (last_print) ? last_print : "reason not known");
#endif
}


/* -------------------------------- initialize DAC -------------------------------- */

static void make_dac_buffers(void)
{
  /* make the per-channel buffers and audio output buffers */
  int bytes, i;

  if ((!dac_buffers) || 
      (dac_buffer_chans < snd_dacp->channels) || 
      (dac_buffer_size < snd_dacp->framples))
    {
      if (dac_buffers)
	{
	  for (i = 0; i < dac_buffer_chans; i++) free(dac_buffers[i]);
	  free(dac_buffers);
	}
      if (rev_ins)
	{
	  for (i = 0; i < dac_buffer_chans; i++) free(rev_ins[i]);
	  free(rev_ins);
	}
      dac_buffers = (mus_float_t **)calloc(snd_dacp->channels, sizeof(mus_float_t *));
      rev_ins = (mus_float_t **)calloc(snd_dacp->channels, sizeof(mus_float_t *));
      for (i = 0; i < snd_dacp->channels; i++) 
	{
	  dac_buffers[i] = (mus_float_t *)calloc(snd_dacp->framples, sizeof(mus_float_t));
	  rev_ins[i] = (mus_float_t *)calloc(snd_dacp->framples, sizeof(mus_float_t));
	}
      dac_buffer_chans = snd_dacp->channels;
      dac_buffer_size = snd_dacp->framples;
      if (r_outs) free(r_outs);
      if (r_ins) free(r_ins);
      r_outs = (mus_float_t *)calloc(snd_dacp->channels, sizeof(mus_float_t));
      r_ins = (mus_float_t *)calloc(snd_dacp->channels, sizeof(mus_float_t));
    }

  bytes = snd_dacp->channels * dac_buffer_size * mus_bytes_per_sample(snd_dacp->out_samp_type);
  if ((audio_bytes_size < bytes) || 
      (audio_bytes_devices < snd_dacp->devices))
    {
      if (audio_bytes)
	{
	  for (i = 0; i < audio_bytes_devices; i++) free(audio_bytes[i]);
	  free(audio_bytes);
	}
      audio_bytes = (unsigned char **)calloc(snd_dacp->devices, sizeof(unsigned char *));
      for (i = 0; i < snd_dacp->devices; i++) 
	audio_bytes[i] = (unsigned char *)calloc(bytes, sizeof(unsigned char));
      audio_bytes_size = bytes;
      audio_bytes_devices = snd_dacp->devices;
    }
}


static void stop_audio_output(void);

#if (HAVE_ALSA || HAVE_OSS)


/* Controls behavior of device selection logic below. No amount of logic
 * can make everybody happy all the time. The [i]logic below cannot always
 * produce the desired result but deleting it altogether will break the
 * systems that currently rely on it. Not wise without an external api
 * in place designed to select whatever the user _really_ wants. Till 
 * then set this to "1" to always send to the first device. */

static int feed_first_device = 0;

#define ALSA_MAX_DEVICES 64
static int alsa_devices[ALSA_MAX_DEVICES];
static int alsa_available_chans[ALSA_MAX_DEVICES];
static int alsa_max_chans[ALSA_MAX_DEVICES];
static int alsa_min_chans[ALSA_MAX_DEVICES];

static int alsa_max_chans_value = 0;
static int alsa_max_chans_dev = 0;
static bool audio_devices_scanned = false;
static int alsa_devices_available = 0;

void mus_audio_alsa_channel_info(int dev, int *info);
void mus_audio_alsa_device_list(int sys, int size, int *val);
int mus_audio_alsa_device_direction(int dev);

static void scan_audio_devices(void)
{
  int cards, card, devs, dev, d;
  int index = 0;
  int direction;
  int val[ALSA_MAX_DEVICES];
  if (!audio_devices_scanned)
    {
      audio_devices_scanned = true;
       /* At this time
       * we always select the widest device if the requested channels fit into it. 
       * Otherwise we try to combine devices, if all fails we modify snd settings
       * so that channel folding takes place. This is inefficient but works for now. 
       */
      cards = 1;
      index = 0;
      /* scan all cards and build a list of available output devices */
      for (card = 0; card < cards; card++) 
	{
	  mus_audio_alsa_device_list(MUS_AUDIO_PACK_SYSTEM(card), ALSA_MAX_DEVICES, val);
	  devs = val[0];
	  /* scan all devices in the card */
	  for (d = 0; d < devs; d++) 
	    {
	      dev = val[d + 1];
	      direction = mus_audio_alsa_device_direction(MUS_AUDIO_PACK_SYSTEM(card) | dev);
	      if (direction == 0) 
		{
		  /* remember output device */
		  alsa_devices[index++] = MUS_AUDIO_PACK_SYSTEM(card) | dev;
		  if (index >= ALSA_MAX_DEVICES) goto NO_MORE_DEVICES;
		}
	    }
	}
    NO_MORE_DEVICES:
      /* get channel availability for all devices */
      for (d = 0; d < index; d++) 
	{
	  int chan_info[4];
	  alsa_available_chans[d] = 0;
	  alsa_min_chans[d] = 0;
	  alsa_max_chans[d] = 0;
	  mus_audio_alsa_channel_info(alsa_devices[d], chan_info);
	  alsa_available_chans[d] = chan_info[0];
	  alsa_min_chans[d] = chan_info[1];
	  alsa_max_chans[d] = chan_info[2];
	  if (alsa_max_chans[d] > alsa_max_chans_value) 
	    {
	      /* remember widest device */
	      alsa_max_chans_value = alsa_max_chans[d];
	      alsa_max_chans_dev = d;
	    }
	}
    }
  alsa_devices_available = index;
}

int mus_audio_alsa_samples_per_channel(int dev);

static bool start_audio_output_1(void)
{
  int i;

  /* -------------------- ALSA not OSS -------------------- */
  if (mus_audio_api() == MUS_ALSA_API) 
    {
      static int out_dev[ALSA_MAX_DEVICES];
      int d, alloc_chans, alloc_devs;
      scan_audio_devices();
      /* allocate devices for playback */
      alloc_chans = 0;
      alloc_devs = 0;
      for (d = 0; d < ALSA_MAX_DEVICES; d++) out_dev[d] = -1; 
      for (d = 0; d < MAX_DEVICES; d++) dev_fd[d] = -1;
      if (feed_first_device == 0) 
	{
	  /* see if widest device can accommodate all channels */
	  if (alsa_max_chans_value >= snd_dacp->channels) 
	    {
	      out_dev[alloc_devs++] = alsa_max_chans_dev;
	      alloc_chans += alsa_max_chans_value;
	    }
	  if (alloc_devs == 0) 
	    {
	      /* try to use several devices */
	      int prev_samp_type = -1;
	      for (d = 0; d < alsa_devices_available; d++) 
		{
		  int this_samp_type;
		  this_samp_type = mus_audio_compatible_sample_type(alsa_devices[d]);
		  if (prev_samp_type == -1) 
		    {
		      prev_samp_type = this_samp_type;
		    }
		  /* samp_type for all selected devices should match */
		  if (this_samp_type == prev_samp_type) 
		    {
		      out_dev[alloc_devs++] = d;
		      alloc_chans += alsa_available_chans[d];
		      if (alloc_devs >= ALSA_MAX_DEVICES)
			break;
		    }
		}
	      if ((alloc_devs != 0) && (alloc_chans < snd_dacp->channels))
		{
		  /* not enough available channels, give up */
		  for (d = 0; d < ALSA_MAX_DEVICES; d++) out_dev[d] = -1;
		  alloc_devs = 0;
		  alloc_chans = 0;
		}
	      if (alloc_devs == 0) 
		{
		  /* fold all channels into the first device */
		  out_dev[alloc_devs++] = 0;
		  alloc_chans += alsa_available_chans[0];
		}
	    }
	}
      else 
	{
	  /* first device on first card is the one */
	  out_dev[alloc_devs++] = 0;
	  alloc_chans += alsa_available_chans[0];
	}
      snd_dacp->out_samp_type = mus_audio_compatible_sample_type(alsa_devices[out_dev[0]]);
      if (alloc_devs < 2) 
	{
	  /* see if we have a minimum sized frample to fill 
	   * FIXME: could this happen in more than one device? */
	  int c;
	  c = alsa_min_chans[out_dev[0]];
	  if (c > snd_dacp->channels) 
	    {
	      snd_dacp->channels = c;
	    }
	}
      /* see if we have to fold channels */
      if (alloc_chans < snd_dacp->channels) 
	{
	  if (dac_combines_channels(ss)) 
	    snd_warning("folding %d chans into %d ", 
			snd_dacp->channels, alloc_chans);
	  snd_dacp->channels = alloc_chans;
	}

      {
	int samples_per_channel;
        /* read the number of samples per channel the device wants buffered */
	samples_per_channel = mus_audio_alsa_samples_per_channel(alsa_devices[out_dev[0]]);
	snd_dacp->framples = samples_per_channel;
	/* set_dac_size(snd_dacp->framples * mus_bytes_per_sample(snd_dacp->out_samp_type)); */
	set_dac_size(samples_per_channel); /* bil 24-Mar-13 */
      }
      /* open all allocated devices */
      for (d = 0; d < alloc_devs; d++) 
	{
	  int channels;
	  channels = alsa_available_chans[out_dev[d]];
	  if (alloc_chans <= alsa_available_chans[out_dev[d]]) 
	    {
	      if (snd_dacp->channels < alsa_min_chans[out_dev[d]]) 
		{
		  channels = alsa_min_chans[out_dev[d]];
		} 
	      else 
		{
		  channels = snd_dacp->channels;
		}
	    }
	  /* FIXME: assumes devices are same size... */
	  set_dac_print();
	  dev_fd[d] = mus_audio_open_output(alsa_devices[out_dev[d]], 
					    snd_dacp->srate,
					    channels, 
					    snd_dacp->out_samp_type, 
					    snd_dacp->framples * channels * mus_bytes_per_sample(snd_dacp->out_samp_type));
	  unset_dac_print();
      
	  if (dev_fd[d] == -1) 
	    {
	      /* could not open a device, close all others and quit playback */
	      int i;
	      for (i = 0; i < d; i++) 
		{
		  mus_audio_close(alsa_devices[out_dev[i]]);
		}
	      dac_error();
	      dac_running = false;
	      if (global_rev) free_reverb();
	      max_active_slot = -1;
	      return(false);
	    }
	}
      snd_dacp->devices = alloc_devs;
      /* for now assume all are same number of chans */
      snd_dacp->chans_per_device = (int *)calloc(snd_dacp->devices, sizeof(int));
      for (i = 0; i < snd_dacp->devices; i++) 
	snd_dacp->chans_per_device[i] = snd_dacp->channels / snd_dacp->devices;
      make_dac_buffers();
    } 
  else 
    {

      /* -------------------- OSS not ALSA -------------------- */
      /* api == MUS_OSS_API -- MUS_JACK_API should not intrude here because we're in HAVE_ALSA || HAVE_OSS */
      int oss_available_chans = 2;

      if (snd_dacp->channels > 2)
	oss_available_chans = mus_audio_device_channels(MUS_AUDIO_DEFAULT);
      for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
      /* see if we can play 16 bit output */
      snd_dacp->out_samp_type = mus_audio_compatible_sample_type(MUS_AUDIO_DEFAULT);

      /* removed PPC stuff here 30-Apr-12 */

      if ((oss_available_chans < snd_dacp->channels) &&
          (oss_available_chans > 0))
 	{
	  if (dac_combines_channels(ss)) 
	    snd_warning("folding %d chans into %d ", 
			snd_dacp->channels, oss_available_chans);
	  snd_dacp->channels = oss_available_chans;
	}
      set_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	dev_fd[0] = mus_audio_open_output(MUS_AUDIO_DEFAULT,
					  snd_dacp->srate, snd_dacp->channels, 
					  snd_dacp->out_samp_type, 
					  dac_size(ss));
      unset_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	{
	  dac_error();
	  stop_audio_output();
	  return(false);
	}
      snd_dacp->devices = (dev_fd[1] != -1) ? 2 : 1;
      snd_dacp->chans_per_device = (int *)calloc(snd_dacp->devices, sizeof(int));
      for (i = 0; i < snd_dacp->devices; i++) 
	snd_dacp->chans_per_device[i] = snd_dacp->channels / snd_dacp->devices;
      make_dac_buffers();
    }
  return(true);
}
#else /* not ALSA or OSS */

static bool start_audio_output_1(void)
{
  int i;
  int available_chans = 2;

  if (snd_dacp->channels > 2)
    available_chans = mus_audio_device_channels(MUS_AUDIO_DEFAULT);

  if (available_chans <= 0)
    {
      snd_warning("no available channels??");
      return(false);
    }
  if (available_chans < snd_dacp->channels) 
    {
      if (dac_combines_channels(ss)) 
	snd_warning("folding %d chans into %d ", 
		    snd_dacp->channels, 
		    available_chans);
      snd_dacp->channels = available_chans;
    }

  for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
  snd_dacp->out_samp_type = mus_audio_device_sample_type(MUS_AUDIO_DEFAULT);

  set_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    dev_fd[0] = mus_audio_open_output(MUS_AUDIO_DEFAULT,
				      snd_dacp->srate, 
				      snd_dacp->channels, 
				      snd_dacp->out_samp_type, 
				      dac_size(ss));
  unset_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    {
      dac_error();
      stop_audio_output();
      return(false);
    }
  snd_dacp->devices = 1;
  snd_dacp->chans_per_device = (int *)calloc(snd_dacp->devices, sizeof(int));
  for (i = 0; i < snd_dacp->devices; i++) 
    snd_dacp->chans_per_device[i] = available_chans / snd_dacp->devices;
  make_dac_buffers();
  return(true);
}
#endif


static bool start_audio_output(void)
{
  /* at this point the desired output srate and chans are set in dacp (via start_dac) */
  cursor_time = 0;
  if (start_audio_output_1()) /* number of channels may be less than requested initially */
    {
      int i;
      for (i = 0; i <= max_active_slot; i++)
	{
	  dac_info *dp;
	  dp = play_list[i];
	  if (dp)
	    {
	      /* deferred reverb allocation since start_audio_output_1 may force more chans open */
	      if ((dp->reverbing) && 
		  (dp->sp) && 
		  (!global_rev))
		make_reverb(dp->sp, snd_dacp->channels);

	      if (dp->audio_chan >= snd_dacp->channels)
		{
		  if (dac_combines_channels(ss))
		    dp->audio_chan %= snd_dacp->channels;
		  else stop_playing(dp, WITHOUT_HOOK, PLAY_NO_CHANNEL);
		}
	    }
	}
      dac_running = true;
      fill_dac_buffers(WRITE_TO_DAC);
      if (!snd_dacp) return(false);
      return(true);
    }
  return(false);
}
 

static void stop_audio_output(void)
{
   int i;
   for (i = 0; i < MAX_DEVICES; i++)
     if (dev_fd[i] != -1) 
       {
	 mus_audio_close(dev_fd[i]);
	 dev_fd[i] = -1;
       }
   dac_running = false;
   dac_pausing = false;
   if (global_rev) free_reverb();
   max_active_slot = -1;
}


static idle_func_t dac_in_background(any_pointer_t ptr)
{
  /* slice 0: try to open audio output devices and get ready to send samples
   *       1: loop sending data until the play_list is empty or some error (C-g) happens
   *       2: try to close all active outputs and remove background procedure
   */
  if (!snd_dacp) return(BACKGROUND_QUIT);
  switch (snd_dacp->slice)
    {
    case 0:
      if (start_audio_output())
	{
	  if (!snd_dacp) return(BACKGROUND_QUIT);
	  snd_dacp->slice = 1;
	  return(BACKGROUND_CONTINUE);
	}
      else 
	{
	  free_dac_state();
	  return(BACKGROUND_QUIT);
	}
      break;

    case 1:
      fill_dac_buffers(WRITE_TO_DAC);
      if (!snd_dacp) return(BACKGROUND_QUIT); /* dac-hook called stop-playing or something equally perverse */
      if ((!global_rev) && (play_list_members == 0)) snd_dacp->slice = 2;
      return(BACKGROUND_CONTINUE);
      break;

     case 2:
       stop_audio_output();
       free_dac_state();
#if USE_GTK
       /* in gtk:
	*   (play)
	*   (apply-controls) ; while playing
	*   -> segfault and many other weird problems
	* This bug appears to be in gtk -- if a background function returns false, it is not supposed to be called 
	* again.  But for some reason gtk continues to call dac_in_background after I return false.  I haven't 
	* tracked this down in the glib sources (glib/gmain.c) -- I suspect that they're "destroying" the "source"
	* (in their jargon) before checking whether to remove the function from the poll list.  Or possibly,
	* I'm destroying it somewhere -- in any case, I'll stop the thing by hand.
	*/
       if (dac_work_proc)
 	 {
 	   BACKGROUND_REMOVE(dac_work_proc);
 	   dac_work_proc = 0;
 	 }
#endif
       return(BACKGROUND_QUIT);
       break;
     }
  return(BACKGROUND_QUIT);
}
 


/* ---------------- support for Apply button (snd-apply.c) ---------------- */

void initialize_apply(snd_info *sp, int chans, mus_long_t beg, mus_long_t dur)
{
  int curchan = 0;

  stop_playing_all_sounds_without_hook(PLAY_APPLY);
  /* see note above about gtk and work procs */

  if (snd_dacp)                             /* may not have been able to complete despite slice=2 in stop all */
    {
      snd_dacp->slice = 2;
      dac_in_background(NULL);              /* so try to force completion */
    }

  if (chans <= 0) return;
  max_active_slot = -1;
  play_list_members = 0;
  dac_running = true; /* this keeps start_dac from actually starting the dac */
  if (snd_dacp) free_dac_state();

  snd_dacp = (dac_state *)calloc(1, sizeof(dac_state));
  snd_dacp->slice = 0;
  snd_dacp->srate = snd_srate(sp);
  snd_dacp->out_samp_type = MUS_AUDIO_COMPATIBLE_SAMPLE_TYPE;
  if (snd_dacp->srate <= 0) snd_dacp->srate = 44100;
  snd_dacp->channels = chans;
  snd_dacp->framples = 8192;
  snd_dacp->devices = 1;
  snd_dacp->chans_per_device = (int *)calloc(1, sizeof(int));
  snd_dacp->chans_per_device[0] = chans;
  snd_dacp->reverb_ring_framples = (mus_long_t)(snd_dacp->srate * sp->reverb_control_decay);
  make_dac_buffers();

  switch (ss->apply_choice)
    {
    case APPLY_TO_SOUND: 
      play_sound(sp, beg, beg + dur); 
      break;

    case APPLY_TO_SELECTION: 
      play_selection(IN_BACKGROUND); 
      break;

    case APPLY_TO_CHANNEL: 
      if (sp->selected_channel != NO_SELECTION)
	curchan = sp->selected_channel;
      play_channel(sp->chans[curchan], beg, beg + dur); 
      break;
    }
}


void finalize_apply(snd_info *sp)
{
  /* if no reverb, these need to be cleaned up */
  stop_playing_all_sounds_without_hook(PLAY_APPLY);
  max_active_slot = -1;
  play_list_members = 0;
  sp->playing = 0;
  dac_running = false;
  if (snd_dacp) free_dac_state();
  if (global_rev) free_reverb();
}


int run_apply(int ofd)
{
  int len, chans;
  if (!(snd_dacp)) return(-1);
  chans = snd_dacp->channels; /* snd_dacp might be freed by fill_dac_buffers */
  len = fill_dac_buffers(WRITE_TO_FILE);
  mus_file_write(ofd, 0, len - 1, chans, dac_buffers);
  return(len);
}



/* -------- players -------- */

static snd_info **players = NULL;
static int *player_chans = NULL;
static int players_size = 0;

static int new_player_index(void)
{
  int i, old_size;

  if (players_size == 0)
    {
      players_size = 8;
      players = (snd_info **)calloc(players_size, sizeof(snd_info *));
      player_chans = (int *)calloc(players_size, sizeof(int));
      return(-1);
    }

  for (i = 1; i < players_size; i++)
    if (!players[i])
      return(-i);

  old_size = players_size;
  players_size += 8;
  players = (snd_info **)realloc(players, players_size * sizeof(snd_info *));
  player_chans = (int *)realloc(player_chans, players_size * sizeof(int));
  for (i = old_size; i < players_size; i++)
    {
      players[i] = NULL;
      player_chans[i] = 0;
    }
  return(-old_size);
}

#define PLAYER(Sp) -(Sp->index)

static int make_player(snd_info *sp, chan_info *cp)
{
  /* store sp so we can access it via find_sound (get_sp) later */
  players[PLAYER(sp)] = sp;
  player_chans[PLAYER(sp)] = cp->chan;
  return(-sp->index);
}

static void free_player_sound(snd_info *sp)
{
  if (players)
    {
      players[PLAYER(sp)] = NULL;
      player_chans[PLAYER(sp)] = 0;
    }
  free(sp->filename);
  sp->filename = NULL;
  free(sp->chans);
  sp->chans = NULL;
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  sp->inuse = SOUND_IDLE;
  free(sp);
}


void clear_players(void)
{
  /* called only in free_snd_info, snd-data.c -- make sure currently closing sound is not playing (via a user-created player) */
  int i;
  for (i = 0; i < players_size; i++)
    {
      unsigned int j;
      snd_info *sp;
      sp = players[i];
      if (sp)
	for (j = 0; j < sp->nchans; j++)
	  if ((!sp->chans[j]) ||
	      (sp->chans[j]->active < CHANNEL_HAS_EDIT_LIST) ||
	      (!sp->chans[j]->sound))
	    {
	      int k;
	      for (k = 0; k <= max_active_slot; k++)
		{
		  dac_info *dp;
		  dp = play_list[k];
		  if ((dp) && (sp == dp->sp))
		    {
		      play_list[k] = NULL;
		      play_list_members--;
		      free_dac_info(dp, PLAY_CLOSE); /* calls stop-function, if any. used in snd-data.c in free_snd_info */
		    }
		}
	      if (is_player_sound(sp)) free_player_sound(sp);
	      break;
	    }
    }
}


/* ---------------------------------------- player objects ---------------------------------------- */

typedef struct {
  int n;
} xen_player;


#define Xen_to_xen_player(arg) ((xen_player *)Xen_object_ref(arg))

static int xen_player_to_int(Xen n)
{
  xen_player *mx;
  mx = Xen_to_xen_player(n);
  return(mx->n);
}

#define Xen_player_to_C_int(Player) xen_player_to_int(Player)


snd_info *get_player_sound(Xen obj)
{
  return(players[xen_player_to_int(obj)]);
}


static Xen_object_type_t xen_player_tag;

bool xen_is_player(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_player_tag));
}


static void xen_player_free(xen_player *v) {if (v) free(v);}

Xen_wrap_free(xen_player, free_xen_player, xen_player_free)


static char *xen_player_to_string(xen_player *v)
{
  #define PLAYER_PRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(PLAYER_PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, PLAYER_PRINT_BUFFER_SIZE, "#<player %d>", v->n);
  return(buf);
}

Xen_wrap_print(xen_player, print_xen_player, xen_player_to_string)


#if HAVE_FORTH || HAVE_RUBY
static Xen g_xen_player_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_player_to_string "player->string"

  Xen_check_type(xen_is_player(obj), obj, 1, S_xen_player_to_string, "a player");

  vstr = xen_player_to_string(Xen_to_xen_player(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static bool xen_player_equalp(xen_player *v1, xen_player *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static Xen equalp_xen_player(Xen obj1, Xen obj2)
{
  if ((!(xen_is_player(obj1))) || (!(xen_is_player(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_player_equalp(Xen_to_xen_player(obj1), Xen_to_xen_player(obj2))));
}
#endif


static xen_player *xen_player_make(int n)
{
  xen_player *new_v;
  new_v = (xen_player *)malloc(sizeof(xen_player));
  new_v->n = n;
  return(new_v);
}


static Xen new_xen_player(int n)
{
  xen_player *mx;
  if (n < 0)
    return(Xen_false);

  mx = xen_player_make(n);
  return(Xen_make_object(xen_player_tag, mx, 0, free_xen_player));
}

#define C_int_to_Xen_player(val) new_xen_player(val)


#if HAVE_SCHEME
static bool s7_xen_player_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_player *)obj1)->n == ((xen_player *)obj2)->n));
}

static Xen s7_xen_player_length(s7_scheme *sc, Xen player)
{
  chan_info *cp;
  int index;
  index = Xen_player_to_C_int(player);
  cp = players[index]->chans[player_chans[index]];
  return(C_llong_to_Xen_llong(current_samples(cp)));
}
#endif


static void init_xen_player(void)
{
#if HAVE_SCHEME
  xen_player_tag = s7_make_c_type(s7, "<player>");
  s7_c_type_set_print(s7, xen_player_tag, print_xen_player);
  s7_c_type_set_free(s7, xen_player_tag, free_xen_player);
  s7_c_type_set_equal(s7, xen_player_tag, s7_xen_player_equalp);
  s7_c_type_set_length(s7, xen_player_tag, s7_xen_player_length);
#else
#if HAVE_RUBY
  xen_player_tag = Xen_make_object_type("XenPlayer", sizeof(xen_player));
#else
  xen_player_tag = Xen_make_object_type("Player", sizeof(xen_player));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_player_tag,   print_xen_player);
  fth_set_object_dump(xen_player_tag,      g_xen_player_to_string);
  fth_set_object_equal(xen_player_tag,     equalp_xen_player);
  fth_set_object_free(xen_player_tag,      free_xen_player);
#endif

#if HAVE_RUBY
  rb_define_method(xen_player_tag, "to_s",     Xen_procedure_cast print_xen_player, 0);
  rb_define_method(xen_player_tag, "eql?",     Xen_procedure_cast equalp_xen_player, 1);
  rb_define_method(xen_player_tag, "==",       Xen_procedure_cast equalp_xen_player, 1);
  rb_define_method(xen_player_tag, "to_str",   Xen_procedure_cast g_xen_player_to_string, 0);
#endif
}

/* -------------------------------------------------------------------------------- */

static Xen play_file(const char *play_name, mus_long_t start, mus_long_t end, int in_channel, int out_channel, play_process_t background, Xen stop_func)
{
  snd_info *sp;

  if (!(mus_file_probe(play_name)))
    return(snd_no_such_file_error(S_play, C_string_to_Xen_string(play_name)));

  if (!(mus_is_header_type(mus_sound_header_type(play_name))))
    Xen_error(BAD_HEADER,
	      Xen_list_3(C_string_to_Xen_string(S_play ": ~S has unknown header: ~A"),
			 C_string_to_Xen_string(play_name),
			 C_string_to_Xen_string(mus_header_type_name(mus_header_type()))));

  if (!(mus_is_sample_type(mus_sound_sample_type(play_name))))
    Xen_error(Xen_make_error_type("bad-sample-type"),
	      Xen_list_3(C_string_to_Xen_string(S_play ": ~S has unknown sample type: ~A"),
			 C_string_to_Xen_string(play_name),
			 C_string_to_Xen_string(mus_header_original_sample_type_name(mus_sound_original_sample_type(play_name), mus_sound_header_type(play_name)))));
  sp = make_sound_readable(play_name, false);
  sp->short_filename = filename_without_directory(play_name);
  sp->filename = NULL;
  sp->delete_me = (void *)1;
  if (in_channel != -1)
    play_channel_1(sp->chans[in_channel], start, end, background, 0, stop_func, (out_channel < 0) ? 0 : out_channel);
  else play_sound_1(sp, start, end, background, Xen_integer_zero, stop_func, S_play, -1);
  
  return(Xen_false);
}

#if (!HAVE_SCHEME)
static Xen kw_start, kw_end, kw_channel, kw_wait, kw_edit_position, kw_stop, kw_out_channel, kw_with_sync, kw_srate, kw_channels;

static void init_play_keywords(void)
{
  kw_start = Xen_make_keyword("start");
  kw_end = Xen_make_keyword("end");
  kw_wait = Xen_make_keyword("wait");
  kw_channel = Xen_make_keyword("channel");
  kw_out_channel = Xen_make_keyword("out-channel");
  kw_edit_position = Xen_make_keyword("edit-position");
  kw_stop = Xen_make_keyword("stop");
  kw_with_sync = Xen_make_keyword("with-sync");
  kw_srate = Xen_make_keyword("srate");
  kw_channels = Xen_make_keyword("channels");
}
#endif

  #if HAVE_SCHEME
    #define play_example "(play \"oboe.snd\")"
  #endif
  #if HAVE_RUBY
    #define play_example "play(\"oboe.snd\")"
  #endif
  #if HAVE_FORTH
    #define play_example "\"oboe.snd\" play"
  #endif

#define H_play "(" S_play " object start end channel edit-position out-channel with-sync wait stop srate channels): \
play the object from start to end.  If channel is not given, play all channels.  If with-sync, play all objects sync'd \
to the current object.  If wait, wait for the play process to finish before going on.  If out-channel, send the samples \
to that DAC channel.  If edit-position, play that member of the edit list, otherwise play the current state of the object. \
If stop, call that function when the play process finishes.  \
If object is a string, it is assumed to be a file name: \n    " play_example "\n."

#if HAVE_SCHEME
static s7_pointer g_play(s7_scheme *sc, s7_pointer args)
{
  s7_pointer object, p, fp;
  mus_long_t start, end; 
  int channel, out_channel, srate, channels, edpos_argpos = 4, channel_argpos = 3;
  bool with_sync;
#if (!USE_NO_GUI)
  bool wait;
#endif
  s7_pointer stop_func, edit_position, channel_arg;
  play_process_t background;
  snd_info *sp;

  object = s7_car(args);
  args = s7_cdr(args);

  fp = s7_car(args);
  if (fp == Xen_false)
    start = 0;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_play, 1, fp, "an integer (start)"));
      start = s7_integer(fp);
      if (start < 0) 
	Xen_out_of_range_error(S_play, 1, fp, "start is negative?");
    }
  
  fp = s7_cadr(args);
  if (fp == Xen_false)
    end = NO_END_SPECIFIED;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_play, 2, fp, "an integer (end)"));
      end = s7_integer(fp);
      if (end < -1)
	Xen_out_of_range_error(S_play, 2, fp, "end is negative?");
    }

  p = s7_cddr(args);
  fp = s7_car(p);
  channel_arg = fp;
  if (fp == Xen_false)
    channel = -1;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_play, 3, fp, "an integer (channel)"));
      channel = s7_integer(fp);
    }

  edit_position = s7_cadr(p);

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp == Xen_false)
    out_channel = -1;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_play, 5, fp, "an integer (out channel)"));
      out_channel = s7_integer(fp);
    }

  fp = s7_cadr(p);
  with_sync = (fp != Xen_false);

  p = s7_cddr(p);
#if (!USE_NO_GUI)
  fp = s7_car(p);
  wait = (fp != Xen_false);
#endif

  stop_func = s7_cadr(p);
  if ((stop_func != Xen_false) && 
      (!s7_is_procedure(stop_func)))
    return(s7_wrong_type_arg_error(sc, S_play, 8, stop_func, "a procedure (stop)"));
  if ((s7_is_procedure(stop_func)) && 
      (!s7_is_aritable(sc, stop_func, 1)))
    Xen_bad_arity_error(S_play, 8, fp, "stop function should take 1 argument");

  p = s7_cddr(p);
  fp = s7_car(p);
  if (fp == Xen_false)
    srate = 44100;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_play, 9, fp, "an integer (srate)"));
      srate = s7_integer(fp);
      if (srate <= 0) 
	Xen_out_of_range_error(S_play, 9, fp, "srate <= 0?");
    }

  fp = s7_cadr(p);
  if (fp == Xen_false)
    channels = 2;
  else
    {
      if (!s7_is_integer(fp))
	return(s7_wrong_type_arg_error(sc, S_play, 10, fp, "an integer (channels)"));
      channels = s7_integer(fp);
      if (channels <= 0)
	Xen_out_of_range_error(S_play, 10, fp, "channels <= 0?");
    }

#else
static Xen g_play(Xen arglist)
{
  Xen object = Xen_undefined;
  mus_long_t start = 0, end = NO_END_SPECIFIED;
  int channel = -1, out_channel = -1, srate = 44100, channels = 2, edpos_argpos = 0, channel_argpos = 0;
  bool with_sync = false, wait = false;
  Xen stop_func = Xen_false, edit_position = Xen_false, channel_arg = Xen_false;
  play_process_t background;
  snd_info *sp;

  if (!Xen_is_null(arglist))
    {
      #define NARGS 10
      Xen args[NARGS * 2]; 
      Xen keys[NARGS];
      int orig_arg[NARGS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      int vals, i, arglist_len;

      if (!Xen_is_keyword(Xen_car(arglist)))
	{
	  object = Xen_car(arglist);
	  arglist = Xen_cdr(arglist);
	}

      keys[0] = kw_start;
      keys[1] = kw_end;
      keys[2] = kw_channel;
      keys[3] = kw_edit_position;
      keys[4] = kw_out_channel;
      keys[5] = kw_with_sync;
      keys[6] = kw_wait;
      keys[7] = kw_stop;
      keys[8] = kw_srate;
      keys[9] = kw_channels;

      for (i = 0; i < NARGS * 2; i++) args[i] = Xen_undefined;
      arglist_len = Xen_list_length(arglist);
      if (arglist_len > NARGS)
	Xen_out_of_range_error(S_play, 0, arglist, "too many arguments");

      for (i = 0; i < arglist_len; i++) args[i] = Xen_list_ref(arglist, i);
      vals = mus_optkey_unscramble(S_play, arglist_len, NARGS, keys, args, orig_arg);

      if (vals > 0)
	{
	  start = mus_optkey_to_mus_long_t(keys[0], S_play, orig_arg[0], start);
	  if (start < 0) 
	    Xen_out_of_range_error(S_play, orig_arg[0], keys[0], "start is negative?");

	  end = mus_optkey_to_mus_long_t(keys[1], S_play, orig_arg[1], end);
	  if (end < -1)
	    Xen_out_of_range_error(S_play, orig_arg[1], keys[1], "end is negative?");

	  channel = mus_optkey_to_int(keys[2], S_play, orig_arg[2], channel);
	  channel_argpos = orig_arg[2];
	  channel_arg = keys[2];
	  
	  if (!(Xen_is_keyword(keys[3]))) 
	    {
	      edit_position = keys[3];
	      edpos_argpos = orig_arg[3];
	    }
	  out_channel = mus_optkey_to_int(keys[4], S_play, orig_arg[4], out_channel);

	  with_sync = mus_optkey_to_bool(keys[5], S_play, orig_arg[5], with_sync);
	  wait = mus_optkey_to_bool(keys[6], S_play, orig_arg[6], wait);
	  stop_func = mus_optkey_to_procedure(keys[7], S_play, orig_arg[7], stop_func, 1, "play stop function takes 1 argument");

	  srate = mus_optkey_to_int(keys[8], S_play, orig_arg[8], srate);
	  if (srate <= 0) 
	    Xen_out_of_range_error(S_play, orig_arg[8], keys[8], "srate <= 0?");

	  channels = mus_optkey_to_int(keys[9], S_play, orig_arg[9], channels);
	  if (channels <= 0)
	    Xen_out_of_range_error(S_play, orig_arg[9], keys[9], "channels <= 0?");
	}
    }
#endif

#if USE_NO_GUI
  background = NOT_IN_BACKGROUND;
#else
  if (wait) background = NOT_IN_BACKGROUND; else background = IN_BACKGROUND;
  /* confusing! wait=#f means don't wait for the play to complete => IN_BACKGROUND */
#endif
  /* unspecified object means the current sound, all chans, all samps, with sync, without wait, current edpos */
  if (!Xen_is_bound(object))
    {
      sp = any_selected_sound();
      if (sp) 
	play_sound_1(sp, start, end, background, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), Xen_false, NULL, 0);
      return(Xen_false);
    }

  /* #f object means start sending out zeros */
  if (Xen_is_false(object))
    return(C_bool_to_Xen_boolean(add_zeros_to_play_list(srate, channels)));

  /* procedure object means add that function to the play list */
  if (Xen_is_procedure(object))
    return(C_bool_to_Xen_boolean(add_xen_to_play_list(object)));

  /* mix object */
  if (xen_is_mix(object))
    return(g_play_mix(object, start));

  /* selection object */
  if (xen_is_selection(object))
    {
      if (selection_is_active())
	play_selection_1(background, stop_func);
      return(object);
    }

  /* region object */
  if (xen_is_region(object))
    return(g_play_region(object, background, stop_func));

  /* string object = filename */
  if (Xen_is_string(object))
    {
      char *name;
      name = mus_expand_filename(Xen_string_to_C_string(object));
      play_file((const char *)name, start, end, channel, out_channel, background, stop_func);
      free(name);
      return(object);
    }

  /* otherwise object is either a player or a sound */
  if (xen_is_player(object))
    sp = get_player_sound(object);
  else sp = get_sp(object);

  if (!sp) 
    return(snd_no_such_sound_error(S_play, object));

  if ((with_sync) && 
      (sp->sync != 0) && 
      (!(is_player_sound(sp))))
    {
      sync_info *si;
      mus_long_t *ends = NULL;
      int i;

      si = snd_sync(sp->sync);
      if (end != NO_END_SPECIFIED)
	{
	  ends = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
	  for (i = 0; i < si->chans; i++) ends[i] = end;
	}
      for (i = 0; i < si->chans; i++) si->begs[i] = start;
      play_channels_1(si->cps, si->chans, si->begs, ends, background, edit_position, false, stop_func, S_play, edpos_argpos);

      free_sync_info(si);
      if (ends) free(ends);
      return(Xen_false);
    }

  if (channel == -1)
    play_sound_1(sp, start, end, background, edit_position, stop_func, S_play, edpos_argpos);
  else 
    {
      if ((channel <(int)(sp->nchans)) &&
	  (channel >= 0))
	{
	  int pos;
	  chan_info *cp;
	  cp = sp->chans[channel];
	  if (out_channel < 0) out_channel = channel;
	  pos = to_c_edit_position(cp, edit_position, S_play, edpos_argpos);
	  play_channel_1(cp, start, end, background, pos, stop_func, out_channel);
	}
      else Xen_out_of_range_error(S_play, channel_argpos, channel_arg, "channel does not exist?");
    }

  return(object);
}


Xen no_such_player_error(const char *caller, Xen player)
{
  Xen_error(Xen_make_error_type("no-such-player"),
	    Xen_list_3(C_string_to_Xen_string("~A: no such player, ~A"),
		       C_string_to_Xen_string(caller),
		       player));
  return(Xen_false);
}


static Xen g_stop_playing(Xen snd)
{
  #define H_stop_playing "(" S_stop_playing " :optional snd): stop play (DAC output) in progress"
  snd_info *sp = NULL;

  Xen_check_type(Xen_is_integer(snd) || xen_is_sound(snd) || !Xen_is_bound(snd) || xen_is_player(snd), snd, 1, S_stop_playing, "a sound or player");

  if (Xen_is_integer(snd) || xen_is_sound(snd))
    {
      sp = get_sp(snd);
      if (!sp)
	return(snd_no_such_sound_error(S_stop_playing, snd));
    }
  else
    {
      if (xen_is_player(snd))
	{
	  sp = get_player_sound(snd);
	  if (!sp)
	    return(no_such_player_error(S_stop_playing, snd));
	}
    }

  if (sp) 
    stop_playing_sound(sp, PLAY_STOP_CALLED); 
  else stop_playing_all_sounds(PLAY_STOP_CALLED);

  return(Xen_false);
}




/* add-player make-player stop-player start-playing */

static Xen g_make_player(Xen snd, Xen chn)
{
  #define H_make_player "(" S_make_player " :optional snd chn): \
make a new player associated with snd's channel chn. \
A player is a sort of wrapper for a channel of a sound; it supports \
all the control panel functions.  Once created, you can set these \
fields, then call " S_add_player " to add this channel to the list of \
channels either being played (if a play is in progress) or about \
to be played (via " S_start_playing ")."

  snd_info *true_sp, *new_sp;
  chan_info *cp;

  Snd_assert_channel(S_make_player, snd, chn, 1);
  true_sp = get_sp(snd);
  if (!true_sp) 
    return(snd_no_such_sound_error(S_make_player, snd));

  cp = get_cp(snd, chn, S_make_player);
  new_sp = make_snd_info(NULL, "make_player:wrapper", true_sp->hdr, new_player_index(), FILE_READ_ONLY);
  new_sp->chans[cp->chan] = cp;

  return(C_int_to_Xen_player(make_player(new_sp, cp)));
}


static Xen g_player_home(Xen player)
{
  #define H_player_home "(" S_player_home " player): a list of the sound index and channel number associated with player"
  int index;

  Xen_check_type(xen_is_player(player), player, 1, S_player_home, "a player");
  index = Xen_player_to_C_int(player);

  if ((index > 0) && 
      (index < players_size) && 
      (players[index]) &&
      (players[index]->chans) &&
      (player_chans[index] < (int)players[index]->nchans))
    {
      chan_info *cp;
      cp = players[index]->chans[player_chans[index]]; /* trying to get back to the original sound index (not the player index) */

      if ((cp->sound) && 
	  (cp->sound->active))
	return(Xen_list_2(C_int_to_Xen_sound(cp->sound->index),
			  C_int_to_Xen_integer(cp->chan)));
      return(Xen_false); /* can this happen? */
    }

  return(no_such_player_error(S_player_home, player));
}


static Xen g_add_player(Xen player, Xen start, Xen end, Xen edpos, Xen stop_proc, Xen out_chan)
{
  #define H_add_player "(" S_add_player " player :optional (start 0) (end len) (pos -1) stop-proc (out-chan player-chan)): \
adds a player to the play list; the play begins when " S_start_playing " is called. \
The start, end, and edit-position of the portion played can be specified.  'out-chan' is \
the audio hardware output channel to use to play this channel.  It defaults to the \
channel number in the sound that contains the channel being played."

  snd_info *sp = NULL;
  int index, pos;
  chan_info *cp;
  dac_info *dp = NULL;
  int i, ochan = -1;

  Xen_check_type(xen_is_player(player), player, 1, S_add_player, "a player");
  Xen_check_type(Xen_is_integer_or_unbound(start), start, 2, S_add_player, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(end), end, 3, S_add_player, "an integer");
  Xen_check_type(((Xen_is_procedure(stop_proc)) && (procedure_arity_ok(stop_proc, 1))) ||
		  (!Xen_is_bound(stop_proc)) || 
		  (Xen_is_false(stop_proc)), 
		  stop_proc, 5, S_add_player, "a procedure of 1 arg");
  Xen_check_type(Xen_is_integer_or_unbound(out_chan), out_chan, 6, S_add_player, "an integer");

  index = Xen_player_to_C_int(player);
  if ((index > 0) && (index < players_size)) 
    sp = players[index];

  if (!sp) 
    return(no_such_player_error(S_add_player, player));

  if (play_list)
    for (i = 0; i < dac_max_sounds; i++)
      if ((play_list[i]) && 
	  (sp == (play_list[i]->sp)))
	Xen_error(Xen_make_error_type("arg-error"),
		  Xen_list_2(C_string_to_Xen_string(S_add_player ": player ~A is already in the play list"),
			     player));

  cp = sp->chans[player_chans[index]];
  pos = to_c_edit_position(cp, edpos, S_add_player, 4);
  if (Xen_is_integer(out_chan)) ochan = Xen_integer_to_C_int(out_chan);
  if (ochan < 0) ochan = cp->chan;

  dp = add_channel_to_play_list(cp,
				sp, /* this is not cp->sound! */
				beg_to_sample(start, S_add_player),
				(Xen_is_llong(end)) ? Xen_llong_to_C_llong(end) : NO_END_SPECIFIED,
				pos,
				ochan);
  if (!dp) return(Xen_false);

  dp->stop_procedure = stop_proc;
  if (Xen_is_procedure(stop_proc))
    dp->stop_procedure_gc_loc = snd_protect(stop_proc);

  return(player);
}


static Xen g_start_playing(Xen Chans, Xen Srate, Xen In_Background)
{
  #define H_start_playing "(" S_start_playing " :optional (chans 1) (srate 44100) (in-background " PROC_TRUE ")): \
If a play-list is waiting, start it."

  int chans = 1, srate = 44100;
  bool back;

  Xen_check_type(Xen_is_integer_or_unbound(Chans), Chans, 1, S_start_playing, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(Srate), Srate, 2, S_start_playing, "an integer");
  Xen_check_type(Xen_is_boolean_or_unbound(In_Background), In_Background, 3, S_start_playing, "a boolean");

  if (Xen_is_integer(Chans)) chans = Xen_integer_to_C_int(Chans);
  if ((chans <= 0) || (chans > MUS_MAX_CHANS))
    Xen_out_of_range_error(S_start_playing, 1, Chans, "chans <= 0 or > 256?");

  if (Xen_is_integer(Srate)) srate = Xen_integer_to_C_int(Srate);
  if (srate <= 0)
    Xen_out_of_range_error(S_start_playing, 2, Srate, "srate <= 0?");

  back = Xen_boolean_to_C_bool(In_Background);

  start_dac(srate, chans, (back) ? IN_BACKGROUND : NOT_IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY);
  return(Xen_false);
}


static Xen g_stop_player(Xen player)
{
  #define H_stop_player "(" S_stop_player " player): stop player and remove its associated sound from the current DAC playlist"
  snd_info *sp = NULL;

  Xen_check_type(xen_is_player(player), player, 1, S_stop_player, "a player");

  sp = get_player_sound(player);
  if (sp) 
    stop_playing_sound(sp, PLAY_STOP_CALLED);
  return(player);
}


static Xen g_free_player(Xen player)
{
  #define H_free_player "(" S_free_player " player): free all resources associated with 'player' and remove it from the current DAC playlist"
  snd_info *sp = NULL;

  Xen_check_type(xen_is_player(player), player, 1, S_free_player, "a player");

  sp = get_player_sound(player);
  if (sp) 
    free_player_sound(sp);
  return(Xen_false);
}


/* also the dac filler needs to run on empty buffers in this case? */

static Xen g_is_player(Xen obj)
{
  #define H_is_player "(" S_is_player " obj): is 'obj' an active player"
  if (xen_is_player(obj))
    {
      int index;
      index = Xen_player_to_C_int(obj);
      return(C_bool_to_Xen_boolean((index > 0) && 
			      (index < players_size) && 
			      (players[index])));
    }
  return(Xen_false);
}


/* player-position? -- need quick way from index to dp to its sampler, then C_llong_to_Xen_llong(current_location(fd)) */

static Xen g_players(void)
{
  #define H_players "(" S_players ") -> list of currently active players."
  Xen lst = Xen_empty_list;
  int i;
  for (i = 0; i < players_size; i++)
    if (players[i])
      lst = Xen_cons(C_int_to_Xen_player(i), lst);
  return(lst);
}


static Xen g_dac_size(void) {return(C_int_to_Xen_integer(dac_size(ss)));}

static Xen g_set_dac_size(Xen val) 
{
  #define H_dac_size "(" S_dac_size "): the current DAC buffer size in framples (256)"
  int len;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_dac_size, "an integer");
  len = Xen_integer_to_C_int(val);
  if (len > 0)
    set_dac_size(len); /* macro in snd-0.h */
  return(C_int_to_Xen_integer(dac_size(ss)));
}


static Xen g_dac_combines_channels(void) {return(C_bool_to_Xen_boolean(dac_combines_channels(ss)));}

static Xen g_set_dac_combines_channels(Xen val) 
{
  #define H_dac_combines_channels "(" S_dac_combines_channels "): " PROC_TRUE " if extra channels are to be mixed into available ones during playing. \
That is, if the sound to be played has 4 channels, but the DAC can only handle 2, if this \
variable is " PROC_TRUE ", the extra channels are mixed into the available ones; otherwise they are ignored."

  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_dac_combines_channels, "a boolean");
  set_dac_combines_channels(Xen_boolean_to_C_bool(val)); 
  return(C_bool_to_Xen_boolean(dac_combines_channels(ss)));
}


static Xen g_playing(void) 
{
  #define H_playing "(" S_playing "): " PROC_TRUE " if sound output is in progress.  If players are waiting to start, \
setting this to " PROC_TRUE " starts them; setting it to " PROC_FALSE " stops output."
  return(C_bool_to_Xen_boolean(something_is_playing()));
}


static Xen g_set_playing(Xen on)
{
  bool starting;
  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_playing, "a boolean");
  starting = Xen_boolean_to_C_bool(on);
  if (starting)
    start_dac((int)mus_srate(), 1, IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY); /* how to get plausible srate chans here? */
  else stop_playing_all_sounds(PLAY_STOP_CALLED);
  return(on);
}


static Xen g_pausing(void)
{
  #define H_pausing "(" S_pausing "): " PROC_TRUE " if sound output is currently pausing (this can be set)."
  return(C_bool_to_Xen_boolean(dac_pausing));
}


static Xen g_set_pausing(Xen pause)
{
  Xen_check_type(Xen_is_boolean(pause), pause, 1, S_set S_pausing, "a boolean");
  dac_pausing = Xen_boolean_to_C_bool(pause);
  play_button_pause(dac_pausing);
  return(pause);
}


static Xen g_cursor_update_interval(void) {return(C_double_to_Xen_real(cursor_update_interval(ss)));}

static Xen g_set_cursor_update_interval(Xen val) 
{
  mus_float_t ctime;
  #define H_cursor_update_interval "(" S_cursor_update_interval "): time (seconds) between cursor updates if " S_with_tracking_cursor "."

  Xen_check_type(Xen_is_number(val), val, 1, S_set S_cursor_update_interval, "a number"); 

  ctime = Xen_real_to_C_double(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    Xen_out_of_range_error(S_set S_cursor_update_interval, 1, val, "invalid time");
  set_cursor_update_interval(ctime);

  return(C_double_to_Xen_real(cursor_update_interval(ss)));
}


static Xen g_cursor_location_offset(void) {return(C_int_to_Xen_integer(cursor_location_offset(ss)));}

static Xen g_set_cursor_location_offset(Xen val) 
{
  int ctime;
  #define H_cursor_location_offset "(" S_cursor_location_offset "): samples added to cursor location if cursor displayed during play."

  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_cursor_location_offset, "an integer"); 

  ctime = Xen_integer_to_C_int(val);
  set_cursor_location_offset(ctime);

  return(C_int_to_Xen_integer(cursor_location_offset(ss)));
}


static Xen g_with_tracking_cursor(void) 
{
  #define H_with_tracking_cursor "("  S_with_tracking_cursor "): " PROC_TRUE " if cursor always moves along in waveform display as sound is played"
  if (with_tracking_cursor(ss) == DONT_TRACK)
    return(Xen_false);
  if (with_tracking_cursor(ss) == TRACK_AND_RETURN)
    return(Xen_true);
  return(Xen_make_keyword("track-and-stay"));
}

static Xen g_set_with_tracking_cursor(Xen on) 
{
  if (Xen_is_boolean(on)) /* for backwards compatibility */
    {
      set_with_tracking_cursor(ss, (Xen_boolean_to_C_bool(on)) ? TRACK_AND_RETURN : DONT_TRACK);
      return(on);
    }

  if (Xen_is_integer(on))
    {
      int val;
      val = Xen_integer_to_C_int(on);
      if (val == 2) 
	set_with_tracking_cursor(ss, TRACK_AND_STAY);
      else set_with_tracking_cursor(ss, ((val == 1) ? TRACK_AND_RETURN : DONT_TRACK));
      return(C_int_to_Xen_integer((int)with_tracking_cursor(ss)));
    }

  if (Xen_is_keyword(on))
    {
      if (Xen_keyword_is_eq(on, Xen_make_keyword("track-and-return")))
	{
	  set_with_tracking_cursor(ss, TRACK_AND_RETURN);
	  return(on);
	}
      if (Xen_keyword_is_eq(on, Xen_make_keyword("track-and-stay")))
	{
	  set_with_tracking_cursor(ss, TRACK_AND_STAY);
	  return(on);
	}
      if (Xen_keyword_is_eq(on, Xen_make_keyword("dont-track")))
	{
	  set_with_tracking_cursor(ss, DONT_TRACK);
	  return(on);
	}
    }

  Xen_check_type(false, on, 1, S_set S_with_tracking_cursor, ":dont-track, :track-and-return, or :track-and-stay");
  return(on);
}





Xen_wrap_no_args(g_with_tracking_cursor_w, g_with_tracking_cursor)
Xen_wrap_1_arg(g_set_with_tracking_cursor_w, g_set_with_tracking_cursor)
#if (!HAVE_SCHEME)
Xen_wrap_any_args(g_play_w, g_play)
#endif
Xen_wrap_1_optional_arg(g_stop_playing_w, g_stop_playing)
Xen_wrap_2_optional_args(g_make_player_w, g_make_player)
Xen_wrap_6_optional_args(g_add_player_w, g_add_player)
Xen_wrap_1_arg(g_player_home_w, g_player_home)
Xen_wrap_3_optional_args(g_start_playing_w, g_start_playing)
Xen_wrap_1_arg(g_stop_player_w, g_stop_player)
Xen_wrap_1_arg(g_free_player_w, g_free_player)
Xen_wrap_no_args(g_players_w, g_players)
Xen_wrap_1_arg(g_is_player_w, g_is_player)
Xen_wrap_no_args(g_dac_size_w, g_dac_size)
Xen_wrap_1_arg(g_set_dac_size_w, g_set_dac_size)
Xen_wrap_no_args(g_dac_combines_channels_w, g_dac_combines_channels)
Xen_wrap_1_arg(g_set_dac_combines_channels_w, g_set_dac_combines_channels)
Xen_wrap_no_args(g_playing_w, g_playing)
Xen_wrap_1_arg(g_set_playing_w, g_set_playing)
Xen_wrap_no_args(g_pausing_w, g_pausing)
Xen_wrap_1_arg(g_set_pausing_w, g_set_pausing)
Xen_wrap_no_args(g_cursor_update_interval_w, g_cursor_update_interval)
Xen_wrap_1_arg(g_set_cursor_update_interval_w, g_set_cursor_update_interval)
Xen_wrap_no_args(g_cursor_location_offset_w, g_cursor_location_offset)
Xen_wrap_1_arg(g_set_cursor_location_offset_w, g_set_cursor_location_offset)

#if HAVE_SCHEME
static s7_pointer acc_cursor_location_offset(s7_scheme *sc, s7_pointer args) {return(g_set_cursor_location_offset(s7_cadr(args)));}
static s7_pointer acc_cursor_update_interval(s7_scheme *sc, s7_pointer args) {return(g_set_cursor_update_interval(s7_cadr(args)));}
static s7_pointer acc_dac_combines_channels(s7_scheme *sc, s7_pointer args) {return(g_set_dac_combines_channels(s7_cadr(args)));}
static s7_pointer acc_dac_size(s7_scheme *sc, s7_pointer args) {return(g_set_dac_size(s7_cadr(args)));}
static s7_pointer acc_with_tracking_cursor(s7_scheme *sc, s7_pointer args) {return(g_set_with_tracking_cursor(s7_cadr(args)));}
#endif

void g_init_dac(void)
{
#if HAVE_SCHEME
  s7_pointer i, b, p, t, r, pl, l, pcl_t;
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "pair?");
  l = s7_make_symbol(s7, "list?");
  r = s7_make_symbol(s7, "real?");
  pl = s7_make_symbol(s7, "player?");
  t = s7_t(s7);
  pcl_t = s7_make_circular_signature(s7, 0, 1, t);
#endif

  init_xen_player();
#if (!HAVE_SCHEME)
  init_play_keywords();
  Xen_define_typed_procedure(S_play,           g_play_w,           0, 0, 1, H_play,          pcl_t);
#else
  s7_define_function_star(s7, S_play, g_play, "(object #<undefined>) start end channel edit-position out-channel with-sync wait stop srate channels", H_play);
#endif

  Xen_define_typed_procedure(S_stop_playing,   g_stop_playing_w,   0, 1, 0, H_stop_playing,  s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_make_player,    g_make_player_w,    0, 2, 0, H_make_player,   s7_make_signature(s7, 3, pl, t, t));
  Xen_define_typed_procedure(S_add_player,     g_add_player_w,     1, 5, 0, H_add_player,    s7_make_signature(s7, 7, pl, pl, i, i, t, t, i));
  Xen_define_typed_procedure(S_player_home,    g_player_home_w,    1, 0, 0, H_player_home,   s7_make_signature(s7, 2, p, pl));
  Xen_define_typed_procedure(S_start_playing,  g_start_playing_w,  0, 3, 0, H_start_playing, s7_make_signature(s7, 4, b, i, i, b));
  Xen_define_typed_procedure(S_stop_player,    g_stop_player_w,    1, 0, 0, H_stop_player,   s7_make_signature(s7, 2, pl, pl));
  Xen_define_typed_procedure(S_free_player,    g_free_player_w,    1, 0, 0, H_free_player,   s7_make_signature(s7, 2, b, pl));
  Xen_define_typed_procedure(S_players,        g_players_w,        0, 0, 0, H_players,       s7_make_signature(s7, 1, l));
  Xen_define_typed_procedure(S_is_player,      g_is_player_w,      1, 0, 0, H_is_player,     s7_make_signature(s7, 2, b, t));

  Xen_define_typed_dilambda(S_pausing, g_pausing_w, H_pausing,
			    S_set S_pausing, g_set_pausing_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));

  Xen_define_typed_dilambda(S_playing, g_playing_w, H_playing, 
			    S_set S_playing, g_set_playing_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));

  Xen_define_typed_dilambda(S_with_tracking_cursor, g_with_tracking_cursor_w, H_with_tracking_cursor,
			    S_set S_with_tracking_cursor, g_set_with_tracking_cursor_w, 0, 0, 1, 0, 
			    pcl_t, pcl_t);

  Xen_define_typed_dilambda(S_dac_size, g_dac_size_w, H_dac_size,
			    S_set S_dac_size, g_set_dac_size_w,  0, 0, 1, 0, 
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

  Xen_define_typed_dilambda(S_dac_combines_channels, g_dac_combines_channels_w, H_dac_combines_channels,
			    S_set S_dac_combines_channels, g_set_dac_combines_channels_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));

  Xen_define_typed_dilambda(S_cursor_update_interval, g_cursor_update_interval_w, H_cursor_update_interval,
			    S_set S_cursor_update_interval, g_set_cursor_update_interval_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, r), s7_make_signature(s7, 2, r, r));

  Xen_define_typed_dilambda(S_cursor_location_offset, g_cursor_location_offset_w, H_cursor_location_offset,
			    S_set S_cursor_location_offset, g_set_cursor_location_offset_w,  0, 0, 1, 0,
			    s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));


  #define H_stop_playing_hook S_stop_playing_hook " (snd): called when a sound finishes playing."
  #define H_play_hook S_play_hook " (size): called each time a buffer is sent to the DAC."
  #define H_start_playing_hook S_start_playing_hook " (snd): called when a play request is triggered. \
If it returns " PROC_TRUE ", the sound is not played."

  #define H_stop_playing_selection_hook S_stop_playing_selection_hook " (): called when the selection stops playing"
  #define H_start_playing_selection_hook S_start_playing_selection_hook " (): called when the selection starts playing"

  stop_playing_hook =            Xen_define_hook(S_stop_playing_hook,            "(make-hook 'snd)",  1, H_stop_playing_hook);
  start_playing_hook =           Xen_define_hook(S_start_playing_hook,           "(make-hook 'snd)",  1, H_start_playing_hook);
  play_hook =                    Xen_define_hook(S_play_hook,                    "(make-hook 'size)", 1, H_play_hook); 
  stop_playing_selection_hook =  Xen_define_hook(S_stop_playing_selection_hook,  "(make-hook)",       0, H_stop_playing_selection_hook);
  start_playing_selection_hook = Xen_define_hook(S_start_playing_selection_hook, "(make-hook)",       0, H_start_playing_selection_hook);

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->cursor_location_offset_symbol, s7_make_function(s7, "[acc-" S_cursor_location_offset "]", acc_cursor_location_offset, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->cursor_update_interval_symbol, s7_make_function(s7, "[acc-" S_cursor_update_interval "]", acc_cursor_update_interval, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->dac_combines_channels_symbol, s7_make_function(s7, "[acc-" S_dac_combines_channels "]", acc_dac_combines_channels, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->dac_size_symbol, s7_make_function(s7, "[acc-" S_dac_size "]", acc_dac_size, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->with_tracking_cursor_symbol, s7_make_function(s7, "[acc-" S_with_tracking_cursor "]", acc_with_tracking_cursor, 2, 0, false, "accessor"));

  s7_symbol_set_documentation(s7, ss->cursor_location_offset_symbol, "*cursor-location-offset*: samples added to cursor location if cursor displayed during play.");
  s7_symbol_set_documentation(s7, ss->cursor_update_interval_symbol, "*cursor-update-interval*: time (seconds) between cursor updates if with-tracking-cursor.");
  s7_symbol_set_documentation(s7, ss->dac_combines_channels_symbol, "*dac-combines-channels*: #t if extra channels are to be mixed into available ones during playing.");
  s7_symbol_set_documentation(s7, ss->dac_size_symbol, "*dac-size*: the current DAC buffer size in framples (256)");
  s7_symbol_set_documentation(s7, ss->with_tracking_cursor_symbol, "*with-tracking-cursor*: #t if cursor always moves along in waveform display as sound is played");
#endif
}

/* dac loop [need start/end of loop in dac_info, reader goes to start when end reached (requires rebuffering)
 *   looper does not stop/restart -- just keep going]
 *   play_selection_1 could put ends somewhere, set ends to NO_END_SPECIFIED, dac_loop_sample can
 *   use begs/other-ends to get loop points, so free_dac_info does not need to restart the loop(?)
 *   If start/end selection changed while playing, are these loop points updated?
 */
