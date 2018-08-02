#include "snd.h"


/* -------- Keyboard Macros -------- */

#define MIN_KEY_CODE 0
#define MAX_KEY_CODE 65535
#define MIN_KEY_STATE 0
#define MAX_KEY_STATE 15

static bool defining_macro = false;
static int macro_cmd_size = 0;
static int macro_size = 0;
typedef struct {int keysym; int state;} macro_cmd;
static macro_cmd **macro_cmds = NULL;


static void allocate_macro_cmds(void)
{
  int old_size;
  old_size = macro_cmd_size;
  macro_cmd_size += 16;
  if (!macro_cmds)
    macro_cmds = (macro_cmd **)calloc(macro_cmd_size, sizeof(macro_cmd *));
  else 
    {
      int i;
      macro_cmds = (macro_cmd **)realloc(macro_cmds, macro_cmd_size * sizeof(macro_cmd *));
      for (i = old_size; i < macro_cmd_size; i++) macro_cmds[i] = NULL;
    }
}


static void start_defining_macro(void)
{
  macro_size = 0;
  defining_macro = true;
  if ((!macro_cmds) || (macro_size == macro_cmd_size)) allocate_macro_cmds();
}


static void stop_defining_macro(void)
{
  /* the last C-x ) went into the macro before we noticed it should not have */
  macro_size -= 2;
  defining_macro = false;
}


static void execute_last_macro(chan_info *cp, int count)
{
  int i, j;
  if ((macro_cmds) && (macro_size > 0))
    for (j = 0; j < count; j++)
      for (i = 0; i < macro_size; i++) 
	keyboard_command(cp, 
			 macro_cmds[i]->keysym, 
			 macro_cmds[i]->state);
}


static void continue_macro(int keysym, int state)
{
  if (!(macro_cmds[macro_size])) macro_cmds[macro_size] = (macro_cmd *)calloc(1, sizeof(macro_cmd));
  macro_cmds[macro_size]->keysym = keysym;
  macro_cmds[macro_size]->state = state;
  macro_size++;
  if (macro_size == macro_cmd_size) 
    allocate_macro_cmds();
}




/* ---------------- keys ---------------- */

typedef struct {
  int key; 
  int state; 
  int args; 
  Xen func; 
  bool cx_extended; /* Sun/Forte C defines "extended" somewhere */
  const char *origin, *prefs_info;
  int gc_loc;
} key_entry; 

static key_entry *keymap = NULL;
static int keymap_size = 0;
static int keymap_top = 0;


int in_keymap(int key, int state, bool cx_extended)
{
  int i;
  if (keymap_top == 0) return(-1);
  for (i = 0; i < keymap_top; i++)
    if ((keymap[i].key == key) && 
	(keymap[i].state == state) &&
	(keymap[i].cx_extended == cx_extended) && 
	(Xen_is_bound(keymap[i].func)))
      return(i);
  return(-1);
}


#if HAVE_SCHEME || HAVE_FORTH
  #define kbd_false 0
#else
  #define kbd_false Xen_false
#endif

#define NUM_BUILT_IN_KEYS 81

static key_entry built_in_keys[NUM_BUILT_IN_KEYS] = {
  {snd_K_Down,       0, 0, kbd_false, false, "zoom out",                                                 NULL, -1},
  {snd_K_Up,         0, 0, kbd_false, false, "zoom in",                                                  NULL, -1},
  {snd_K_Left,       0, 0, kbd_false, false, "move window left",                                         NULL, -1},
  {snd_K_Right,      0, 0, kbd_false, false, "move window right",                                        NULL, -1},
  {snd_keypad_Down,  0, 0, kbd_false, false, "zoom fft out",                                             NULL, -1},
  {snd_keypad_Up,    0, 0, kbd_false, false, "zoom fft in",                                              NULL, -1},
  {snd_keypad_Left,  0, 0, kbd_false, false, "move fft left",                                            NULL, -1},
  {snd_keypad_Right, 0, 0, kbd_false, false, "move fft right",                                           NULL, -1},
  {snd_K_less,       0, 0, kbd_false, false, "move cursor to sample 0",                                  NULL, -1},
  {snd_K_greater,    0, 0, kbd_false, false, "move cursor to last sample",                               NULL, -1},
  {snd_K_space,      0, 0, kbd_false, false, "play from cursor or stop playing",                         NULL, -1},

  {snd_K_less,       snd_ControlMask, 0, kbd_false, false, "move cursor to sample 0",                    NULL, -1},
  {snd_K_greater,    snd_ControlMask, 0, kbd_false, false, "move cursor to last sample",                 NULL, -1},
  {snd_K_a,          snd_ControlMask, 0, kbd_false, false, "move cursor to window start",                NULL, -1},
  {snd_K_b,          snd_ControlMask, 0, kbd_false, false, "move cursor back one pixel",                 NULL, -1},
  {snd_K_d,          snd_ControlMask, 0, kbd_false, false, "delete sample at cursor",                    NULL, -1},
  {snd_K_e,          snd_ControlMask, 0, kbd_false, false, "move cursor to window end",                  NULL, -1},
  {snd_K_f,          snd_ControlMask, 0, kbd_false, false, "move cursor ahead one pixel",                NULL, -1},
  {snd_K_g,          snd_ControlMask, 0, kbd_false, false, "abort current command",                      NULL, -1},
  {snd_K_h,          snd_ControlMask, 0, kbd_false, false, "delete previous sample",                     NULL, -1},
  {snd_K_i,          snd_ControlMask, 0, kbd_false, false, "display cursor info",                        NULL, -1},
  {snd_K_j,          snd_ControlMask, 0, kbd_false, false, "goto mark",                                  NULL, -1},
  {snd_K_k,          snd_ControlMask, 0, kbd_false, false, "delete one line's worth of samples",         NULL, -1},
  {snd_K_l,          snd_ControlMask, 0, kbd_false, false, "position window so cursor is in the middle", NULL, -1},
  {snd_K_m,          snd_ControlMask, 0, kbd_false, false, "place (or remove) mark at cursor location",  NULL, -1},
  {snd_K_n,          snd_ControlMask, 0, kbd_false, false, "move cursor ahead one 'line'",               NULL, -1},
  {snd_K_o,          snd_ControlMask, 0, kbd_false, false, "insert one zero sample at cursor",           NULL, -1},
  {snd_K_p,          snd_ControlMask, 0, kbd_false, false, "move cursor back one 'line'",                NULL, -1},
  {snd_K_q,          snd_ControlMask, 0, kbd_false, false, "play current channel starting at cursor",    "play-channel-from-cursor", -1},
  {snd_K_s,          snd_ControlMask, 0, kbd_false, false, "search",                                     NULL, -1},
  {snd_K_t,          snd_ControlMask, 0, kbd_false, false, "stop playing",                               NULL, -1},
  {snd_K_u,          snd_ControlMask, 0, kbd_false, false, "start arg count definition.",                NULL, -1},
  {snd_K_v,          snd_ControlMask, 0, kbd_false, false, "move cursor to mid-window",                  NULL, -1},
  {snd_K_w,          snd_ControlMask, 0, kbd_false, false, "delete selection",                           "delete-selection", -1},
  {snd_K_y,          snd_ControlMask, 0, kbd_false, false, "insert selection.",                          "insert-selection", -1},
  {snd_K_z,          snd_ControlMask, 0, kbd_false, false, "set sample at cursor to 0.0",                NULL, -1},
  {snd_K_underscore, snd_ControlMask, 0, kbd_false, false, "undo",                                       NULL, -1},
  {snd_K_space,      snd_ControlMask, 0, kbd_false, false, "start selection definition",                 NULL, -1},
  {snd_K_g,          snd_ControlMask | snd_MetaMask, 0, kbd_false, false, "clear listener",              NULL, -1},
  {snd_K_less,       snd_MetaMask,    0, kbd_false, false, "move cursor to sample 0",                    NULL, -1},
  {snd_K_greater,    snd_MetaMask,    0, kbd_false, false, "move cursor to last sample",                 NULL, -1},

  {snd_K_b,          0, 0, kbd_false, true, "position window so cursor is on left margin",               NULL, -1},
  {snd_K_c,          0, 0, kbd_false, true, "define selection from cursor to nth mark",                  NULL, -1},
  {snd_K_e,          0, 0, kbd_false, true, "execute keyboard macro",                                    NULL, -1},
  {snd_K_f,          0, 0, kbd_false, true, "position window so cursor is on right margin",              NULL, -1},
  {snd_K_k,          0, 0, kbd_false, true, "close file",                                                NULL, -1},
  {snd_K_l,          0, 0, kbd_false, true, "position selection in mid-view",                            NULL, -1},
  {snd_K_o,          0, 0, kbd_false, true, "move to next or previous graph",                            NULL, -1},
  {snd_K_p,          0, 0, kbd_false, true, "play selection or region n",                                "play-selection", -1},
  {snd_K_q,          0, 0, kbd_false, true, "mix in selection",                                          "mix-selection", -1},
  {snd_K_r,          0, 0, kbd_false, true, "redo",                                                      NULL, -1},
  {snd_K_u,          0, 0, kbd_false, true, "undo",                                                      NULL, -1},
  {snd_K_v,          0, 0, kbd_false, true, "position window over selection",                            NULL, -1},
  {snd_K_z,          0, 0, kbd_false, true, "smooth selection",                                          NULL, -1},
  {snd_K_openparen,  0, 0, kbd_false, true, "begin keyboard macro definition",                           NULL, -1},
  {snd_K_closeparen, 0, 0, kbd_false, true, "end keyboard macro definition",                             NULL, -1},

  {snd_K_b, snd_ControlMask, 0, kbd_false, true, "set x window bounds (preceded by 1 arg)",              NULL, -1},
  {snd_K_c, snd_ControlMask, 0, kbd_false, true, "hide control panel",                                   NULL, -1},
  {snd_K_f, snd_ControlMask, 0, kbd_false, true, "open file",                                            NULL, -1},
  {snd_K_g, snd_ControlMask, 0, kbd_false, true, "abort command",                                        NULL, -1},
  {snd_K_o, snd_ControlMask, 0, kbd_false, true, "show control panel",                                   NULL, -1},
  {snd_K_p, snd_ControlMask, 0, kbd_false, true, "set window size (preceded by 1 arg)",                  NULL, -1},
  {snd_K_q, snd_ControlMask, 0, kbd_false, true, "mix in file",                                          NULL, -1},
  {snd_K_r, snd_ControlMask, 0, kbd_false, true, "redo",                                                 "save-sound", -1},
  {snd_K_s, snd_ControlMask, 0, kbd_false, true, "save file",                                            NULL, -1},
  {snd_K_u, snd_ControlMask, 0, kbd_false, true, "undo",                                                 NULL, -1},
  {snd_K_v, snd_ControlMask, 0, kbd_false, true, "set window size as percentage of total",               NULL, -1},
  {snd_K_w, snd_ControlMask, 0, kbd_false, true, "save current channel in file",                         NULL, -1},
  {snd_K_z, snd_ControlMask, 0, kbd_false, true, "smooth using cosine",                                  NULL, -1},
};


void map_over_keys(bool (*func)(int key, int state, bool cx, Xen xf))
{
  int i;
  for (i = 0; i < keymap_top; i++)
    if ((Xen_is_bound(keymap[i].func)) &&
	((*func)(keymap[i].key, 
		 keymap[i].state, 
		 keymap[i].cx_extended, 
		 keymap[i].func)))
      return;
}


static key_info *make_key_info(key_entry k)
{
  key_info *ki;
  ki = (key_info *)calloc(1, sizeof(key_info));
#if USE_MOTIF
  ki->key = XKeysymToString(k.key); /* no free! */
#else
  #if USE_GTK
  ki->key = gdk_keyval_name(k.key);
  #endif
#endif
  ki->c = k.state & snd_ControlMask;
  ki->m = k.state & snd_MetaMask;
  ki->x = k.cx_extended;
  return(ki);
}


key_info *find_prefs_key(const char *prefs_name)
{
  int i;
  key_info *ki;
  for (i = 0; i < keymap_top; i++)
    if ((Xen_is_bound(keymap[i].func)) &&
	(mus_strcmp(keymap[i].prefs_info, prefs_name)))
      return(make_key_info(keymap[i]));

  for (i = 0; i < NUM_BUILT_IN_KEYS; i++)
    if (mus_strcmp(built_in_keys[i].prefs_info, prefs_name))
      return(make_key_info(built_in_keys[i]));

  ki = (key_info *)calloc(1, sizeof(key_info));
  ki->key = NULL;
  ki->c = false;
  ki->m = false;
  ki->x = false;
  return(ki);
}


char *key_description(int key, int state, bool cx_extended)
{
  int pos;
  if ((key < MIN_KEY_CODE) || (key > MAX_KEY_CODE) ||
      (state < MIN_KEY_STATE) || (state > MAX_KEY_STATE))
    return(NULL);
  pos = in_keymap(key, state, cx_extended);
  if (pos < 0) pos = in_keymap(key, state, cx_extended);
  if (pos >= 0)
    {
      if (keymap[pos].origin)
	return(mus_strdup(keymap[pos].origin));
      return(mus_strdup("something indescribable")); /* NULL would mean "no binding" */
    }
  for (pos = 0; pos < NUM_BUILT_IN_KEYS; pos++)
    if ((built_in_keys[pos].key == key) && 
	(built_in_keys[pos].state == state) && 
	(built_in_keys[pos].cx_extended == cx_extended))
      return(mus_strdup(built_in_keys[pos].origin));
  return(NULL);
}


void set_keymap_entry(int key, int state, int args, Xen func, bool cx_extended, const char *origin, const char *prefs_info)
{
  int i;
  i = in_keymap(key, state, cx_extended);
  if (i == -1)
    {
      if (keymap_size == keymap_top)
	{
	  keymap_size += 16;
	  if (keymap_top == 0)
	    {
	      keymap = (key_entry *)calloc(keymap_size, sizeof(key_entry));
	      for (i = 0; i < keymap_size; i++) 
		keymap[i].func = Xen_undefined;
	    }
	  else 
	    {
	      keymap = (key_entry *)realloc(keymap, keymap_size * sizeof(key_entry));
	      for (i = keymap_top; i < keymap_size; i++) 
		{
		  keymap[i].key = 0; 
		  keymap[i].state = 0; 
		  keymap[i].func = Xen_undefined;
		  keymap[i].cx_extended = false;
		  keymap[i].origin = NULL;
		  keymap[i].prefs_info = NULL;
		  keymap[i].gc_loc = NOT_A_GC_LOC;
		}
	    }
	}
      keymap[keymap_top].key = key;
      keymap[keymap_top].state = state;
      keymap[keymap_top].cx_extended = cx_extended;
      check_menu_labels(key, state, cx_extended);
      i = keymap_top;
      keymap_top++;
    }
  else
    {
      if ((Xen_is_procedure(keymap[i].func)) &&
	  (keymap[i].gc_loc != NOT_A_GC_LOC))
	{
	  snd_unprotect_at(keymap[i].gc_loc);
	  keymap[i].gc_loc = NOT_A_GC_LOC;
	}
      if (keymap[i].origin)
	{
	  /* this is silly... */
	  char *tmp;
	  tmp = (char *)keymap[i].origin;
	  keymap[i].origin = NULL;
	  free(tmp);
	}
      if (keymap[i].prefs_info)
	{
	  char *tmp;
	  tmp = (char *)keymap[i].prefs_info;
	  keymap[i].prefs_info = NULL;
	  free(tmp);
	}
    }
  keymap[i].origin = mus_strdup(origin);
  keymap[i].prefs_info = mus_strdup(prefs_info);
  keymap[i].args = args;
  keymap[i].func = func;
  if (Xen_is_procedure(func)) 
    keymap[i].gc_loc = snd_protect(func);
}


static void call_keymap(int hashedsym, int count)
{
  kbd_cursor_t res = KEYBOARD_NO_ACTION;

  if (Xen_is_bound(keymap[hashedsym].func))
    {
      /* not _NO_CATCH here because the code is not protected at any higher level
       * if key-bind function returns some impossible cursor-action, return keyboard-no-action
       */
      Xen result;
      if (keymap[hashedsym].args == 0)
	result = Xen_call_with_no_args(keymap[hashedsym].func, keymap[hashedsym].origin);
      else result = Xen_call_with_1_arg(keymap[hashedsym].func, C_int_to_Xen_integer(count), keymap[hashedsym].origin);
      if (Xen_is_integer(result))
	{
	  int r;
	  r = (int)Xen_integer_to_C_int(result);
	  if ((r <= KEYBOARD_NO_ACTION) &&
	      (r >= CURSOR_IN_VIEW))
	    res = (kbd_cursor_t)r;
	}
    }
  handle_cursor(selected_channel(), res);
}




/* ---------------- other kbd built-in commands ---------------- */

static void cursor_moveto_end(chan_info *cp)
{
  cursor_moveto(cp, current_samples(cp) - 1);
}


static void set_window_bounds(chan_info *cp, int count) 
{
  /* count = sample number to start at */
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit != 0.0)
    {
      double sx;
      sx = (((double)count / (double)snd_srate(cp->sound)) - ap->xmin) / ap->x_ambit;
      reset_x_display(cp, sx, ap->zx);
    }
}


static void set_window_size(chan_info *cp, int count) 
{
  /* set samples within window */
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit != 0.0)
    {
      double zx;
      zx = ((double)count / (((double)snd_srate(cp->sound)) * ap->x_ambit));
      reset_x_display(cp, ap->sx, zx);
    }
}


static void set_window_percentage(chan_info *cp, int count) 
{
  /* set percentage of file within window */
  axis_info *ap;
  double zx;
  ap = cp->axis;
  zx = (double)count / (double)snd_srate(cp->sound);
  reset_x_display(cp, ap->sx, zx);
}


static void window_framples_selection(chan_info *cp)
{
  double x0, x1;
  int i;
  x0 = (((double)(selection_beg(cp))) / ((double)snd_srate(cp->sound)));
  x1 = x0 + ((double)(selection_len())) / ((double)(snd_srate(cp->sound)));
  set_x_axis_x0x1(cp, x0, x1);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL) && 
	  (cp->sound != sp) && 
	  (selection_is_active_in_channel(sp->chans[0])) && 
	  (sp->sync != (cp->sound->sync)))
	set_x_axis_x0x1(sp->chans[0], x0, x1);
    }
}


static chan_info *goto_next_graph(chan_info *cp, int count);

static chan_info *goto_previous_graph(chan_info *cp, int count)
{
  snd_info *sp;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return(cp);
  sp = cp->sound;
  if (sp->inuse != SOUND_NORMAL) return(cp);
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) 
    k = -count; 
  else return(goto_next_graph(cp, count)); 
  if (chan > 0)
    {
      /* goto previous channel in current sound */
      k -= chan;
      if (k <= 0)
	ncp = sp->chans[chan + count];
    }
  while (k > 0)
    {
      /* look for previous sound, (wrap around) */
      /* goto channel n */
      for (i = (sp->index - 1); i >= 0; i--)
	if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
	  {
	    sp = (snd_info *)(ss->sounds[i]);
	    j = k;
	    k -= sp->nchans;
	    if (k <= 0)
	      ncp = sp->chans[sp->nchans - j];
	    break;
	  }
      if (k > 0)
	for (i = ss->max_sounds - 1; i >= sp->index; i--)
	  if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[sp->nchans - j];
	      break;
	    }
    }
  if (ncp == vcp) return(ncp);
  if (!ncp) snd_error_without_format("goto previous graph failed!");
  select_channel(ncp->sound, ncp->chan);
  return(ncp);
}


static chan_info *goto_next_graph(chan_info *cp, int count)
{
  snd_info *sp;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return(cp);
  sp = cp->sound;
  if (sp->inuse != SOUND_NORMAL) return(cp);
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) 
    return(goto_previous_graph(cp, count)); 
  k = count;
  if (chan < ((int)(sp->nchans) - 1))
    {
      /* goto next channel in current sound */
      k -= (sp->nchans-chan - 1);
      if (k <= 0)
	ncp = sp->chans[chan + count];
    }
  while (k > 0)
    {
      /* look for next sound, (wrap around) */
      /* goto channel 0 */
      for (i = (sp->index + 1); i < ss->max_sounds; i++)
	if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
	  {
	    sp = (snd_info *)(ss->sounds[i]);
	    j = k;
	    k -= sp->nchans;
	    if (k <= 0)
	      ncp = sp->chans[j - 1];
	    break;
	  }
      if (k > 0)
	for (i = 0; i <= sp->index; i++)
	  if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[j - 1];
	      break;
	    }
    }
  if (ncp == vcp) return(ncp);
  if (!ncp) snd_error_without_format("goto next graph failed!");
  select_channel(ncp->sound, ncp->chan);
  return(ncp);
}


void save_edits_from_kbd(snd_info *sp)
{
  /* this used to prompt for confirmation, but we now use a dialog
   */
  redirect_everything_to(printout_to_status_area, (void *)sp);
#if (!USE_NO_GUI)
  {
    io_error_t err;
    err = save_edits(sp);
    if (err == IO_NEED_WRITE_CONFIRMATION)
      changed_file_dialog(sp);
  }
#else
  save_edits_without_asking(sp);
#endif

  redirect_everything_to(NULL, NULL);
}


/* ---------------- key response ---------------- */

static mus_long_t get_count_1(char *number_buffer, int number_ctr, bool dot_seen, chan_info *cp)
{
  /* allow floats here = secs */
  int i;
  if (number_ctr == 0) return(1); /* c-u followed by nothing = 1 */
  number_buffer[number_ctr] = '\0';
  if (number_ctr == 1)
    { /* handle special cases of just - or + */
      if (number_buffer[0] == '-') return(-1);
      if (number_buffer[0] == '+') return(1);
      if (number_buffer[0] == '.') return(0);  /* -. and +. -> 0 from sscanf */
    }
  if (dot_seen)
    {
      float f;
      if (!(sscanf(number_buffer, "%12f", &f))) /* 12 is number_buffer size */
	{
	  /* this doesn't happen for most bogus cases -- the C spec says "it is not possible to determine
	   *    directly whether matches of literal character in the control string succeed or fail."
	   *    but really bad stuff like C-u ... C-f does get flagged.
	   */
	  snd_error("invalid number: %s", number_buffer);
	  return(0);
	}
      return((mus_long_t)(f * snd_srate(cp->sound)));
    }
  if (!(sscanf(number_buffer, "%12d", &i)))
    {
      snd_error("invalid number: %s", number_buffer);
      return(0);
    }
  return(i);
}


static mus_long_t get_count(char *number_buffer, int number_ctr, bool dot_seen, chan_info *cp, bool mark_wise)
{
  mus_long_t val, old_cursor;
  val = get_count_1(number_buffer, number_ctr, dot_seen, cp);
  if (!mark_wise) return(val);
  old_cursor = cursor_sample(cp);
  if (!(goto_mark(cp, val)))
    set_status(cp->sound, "no such mark", false);
  val = cursor_sample(cp) - old_cursor; /* will be 0 if no relevant marks */
  cursor_sample(cp) = old_cursor;
  return(val);
}


#define NUMBER_BUFFER_SIZE 12

static mus_float_t state_amount(int state)
{
  mus_float_t amount;
  amount = 1.0;
  if (state & snd_ControlMask) amount *= 0.5;
  if (state & snd_MetaMask) amount *= 0.5;
  if (state & snd_ShiftMask) amount *= 0.5;
  return(amount);
}


static void zoom_fft(mus_float_t amount)
{
  mus_float_t zx, mx;
  zx = spectrum_end(ss) - spectrum_start(ss);
  mx = (spectrum_end(ss) + spectrum_start(ss)) * 0.5;
  zx *= amount;
  mx -= (zx * 0.5);
  if (mx < 0.0) mx = 0.0;
  set_spectrum_start(mx);
  if ((mx + zx) <= 1.0)
    set_spectrum_end(mx + zx);
  else set_spectrum_end(1.0);
}


static void move_fft(mus_float_t amount)
{
  mus_float_t mx, zx;
  zx = spectrum_end(ss) - spectrum_start(ss);
  mx = spectrum_start(ss) + zx * amount;
  if (mx < 0.0) mx = 0.0;
  if (mx >= 1.0) mx = 1.0 - zx;
  set_spectrum_start(mx);
  if ((mx + zx) <= 1.0)
    set_spectrum_end(mx + zx);
  else set_spectrum_end(1.0);
}


static bool stop_selecting(int keysym, int state)
{
  return(((state & snd_ControlMask) == 0) ||
	 (keysym == snd_K_D) || (keysym == snd_K_d) ||
	 (keysym == snd_K_H) || (keysym == snd_K_h) ||
	 (keysym == snd_K_Y) || (keysym == snd_K_y));
}


static const char *key_to_name(int keysym) 
{
  if (keysym) 
    {
      const char *str;
      str = KEY_TO_NAME(keysym);
      if (str) return(str);
    }
  return("NUL");
}


static int number_ctr = 0;
static bool dot_seen = false;
static bool counting = false;
static bool extended_mode = false;

void control_g(snd_info *sp)
{
  ss->C_g_typed = true;
  number_ctr = 0; 
  counting = false; 
  dot_seen = false; 
  extended_mode = false;
  /* if (selection_is_active()) deactivate_selection(); */
  defining_macro = false;
  clear_stdin();
  redirect_everything_to(NULL, NULL);
  if (sp) clear_status_area(sp); /* do this before stop_playing! */

  if ((ss->checking_explicitly) || 
      (play_in_progress())) 
    ss->stopped_explicitly = true; 
  /* this tries to break out of long filter/src computations (and perhaps others) */
  /*   but, as in other such cases, it leaves this flag set so all subsequent uses of it need to clear it first */

  stop_playing_all_sounds(PLAY_C_G); /* several scm files assume hooks called upon C-g -- could be region play, etc */
  if (sp)
    {
      if (sp->applying) stop_applying(sp);
      for_each_sound_chan(sp, stop_fft_in_progress);
    }

  if (selection_creation_in_progress())
    deactivate_selection();
}

#ifndef SND_KEYMASK
  #define SND_KEYMASK (snd_ControlMask | snd_MetaMask)
#endif

#if HAVE_EXTENSION_LANGUAGE && (!USE_NO_GUI)
  static chan_info *last_searcher = NULL;
#endif


void keyboard_command(chan_info *cp, int keysym, int unmasked_state)
{
  /* we can't use the meta bit in some cases because this is trapped at a higher level for the Menu mnemonics */
  /* state here is the kbd bucky-bit state -- it might have bogus junk like NumLock */
  /* keysym has Shift taken into account already (see snd-xchn.c XKeycodeToKeysym, and same snd-xsnd.c) */

  static bool u_count = false;
  static char number_buffer[NUMBER_BUFFER_SIZE];
  static mus_long_t count = 1;
  static bool got_count = false;
  static bool m = false;
  int shift = 0;
  int hashloc, i, state;
  static mus_long_t ext_count = 1;
  static bool got_ext_count = false;
  snd_info *sp;
  axis_info *ap;

  /* fprintf(stderr, "%d (%s %d%s) ", keysym, KEY_TO_NAME(keysym), unmasked_state, (extended_mode) ? " (c-x)" : ""); */

  if ((!cp) || (!(cp->sound)) || (cp->active < CHANNEL_HAS_EDIT_LIST)) return;
  sp = cp->sound;
  if ((!sp) || (sp->inuse != SOUND_NORMAL)) return;

  ap = cp->axis;

  if (keysym >= snd_K_Shift_L) return;
  /* this happens when the user presses Control or Shift etc prior to hitting the actual (modified) key */
  shift = unmasked_state & snd_ShiftMask;
  state = unmasked_state & SND_KEYMASK; /* mask off stuff we don't care about */

  if (defining_macro) continue_macro(keysym, state);
  if (!m) count = 1; else m = false;
  
  if ((selection_creation_in_progress()) && (!counting) &&
      ((extended_mode) || (stop_selecting(keysym, state))))
    finish_selection_creation();

  if ((counting) && (((keysym < snd_K_0) || (keysym > snd_K_9)) && 
		     ((keysym < snd_keypad_0) || (keysym > snd_keypad_9)) && /* these are in order in both Motif (X11) and gdk */
		     (keysym != snd_K_minus) && 
		     (keysym != snd_K_period) && 
		     (keysym != snd_keypad_Decimal) &&
		     (keysym != snd_K_plus)))
    {
      m = ((u_count) && 
	   ((keysym == snd_K_M) || (keysym == snd_K_m)));
      count = get_count(number_buffer, number_ctr, dot_seen, cp, m);
      got_count = true;
      number_ctr = 0;
      counting = false;
      dot_seen = false;
      if (m) return;
    }

  u_count = false;

  if ((keysym != snd_K_X) && (keysym != snd_K_x) &&
      (keysym != snd_K_B) && (keysym != snd_K_b) &&
      (keysym != snd_K_F) && (keysym != snd_K_f))
    {
      got_count = false;
      if (count == 0) return;
    }

#if HAVE_EXTENSION_LANGUAGE
  hashloc = in_keymap(keysym, state, extended_mode);
  if (hashloc != -1)                       /* found user-defined key */
    {
      extended_mode = false;
      call_keymap(hashloc, count);
      return;
    }
#endif

  if (state & snd_ControlMask)
    {
      if (!extended_mode)
	{
	  mus_long_t loc;
	  /* -------------------------------- C-key -------------------------------- */
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      cp->cursor_on = true; 
	      loc = (mus_long_t)(ap->x0 * snd_srate(sp)); 
	      if ((loc + 1) == ap->losamp) loc = ap->losamp; /* handle dumb rounding problem */
	      cursor_moveto(cp, loc); 
	      break;

	    case snd_K_B: case snd_K_b: 
	      /* this was by samples which is nuts if we're looking at a large window -- should be by pixel, I think */
	      {
		int samples_per_pixel = 1;
		if ((!got_count) &&                              /* C-u 2.1 C-b moves back 2.1 seconds */
		    (cp->axis->x_axis_x1 > cp->axis->x_axis_x0)) /* avoid divide by zero if window tiny */
		  {
		    samples_per_pixel = (cp->axis->hisamp - cp->axis->losamp) / (cp->axis->x_axis_x1 - cp->axis->x_axis_x0);
		    if (samples_per_pixel < 1) samples_per_pixel = 1;
		  }
		cp->cursor_on = true; 
		cursor_move(cp, -count * samples_per_pixel);
		got_count = false;
	      }
	      break;

	    case snd_K_D: case snd_K_d: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, count); 
	      break;

	    case snd_K_E: case snd_K_e:
	      cp->cursor_on = true; 
	      loc = (mus_long_t)(ap->x1 * (double)snd_srate(sp));
	      if ((loc + 1) == ap->hisamp) loc = ap->hisamp;
	      cursor_moveto(cp, loc); 
	      break;

	    case snd_K_F: case snd_K_f:
	      {
		int samples_per_pixel = 1;
		if ((!got_count) &&
		    (cp->axis->x_axis_x1 > cp->axis->x_axis_x0))
		  {
		    samples_per_pixel = (cp->axis->hisamp - cp->axis->losamp) / (cp->axis->x_axis_x1 - cp->axis->x_axis_x0);
		    if (samples_per_pixel < 1) samples_per_pixel = 1;
		  }
		cp->cursor_on = true; 
		cursor_move(cp, count * samples_per_pixel); 
		got_count = false;
	      }
	      break;

	    case snd_K_G: case snd_K_g: 
	      if (state & snd_MetaMask)
		clear_listener();
	      else control_g(sp);
	      break;

	    case snd_K_H: case snd_K_h: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, -count); 
	      break; 

	    case snd_K_I: case snd_K_i: 
	      show_cursor_info(cp); 
	      break;

	    case snd_K_J: case snd_K_j: 
	      cp->cursor_on = true; 
	      if (!(goto_mark(cp, count)))
		set_status(cp->sound, "no such mark", false);
	      break;

	    case snd_K_K: case snd_K_k: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, count * 128);
	      break;

	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = true; 
	      handle_cursor_with_sync(cp, CURSOR_IN_MIDDLE);
	      break;

	    case snd_K_M: case snd_K_m:
	      {
		mark *mk = NULL;
		if (count > 0) 
		  {
		    cp->cursor_on = true;
		    set_show_marks(true);
		    mk = add_mark(cursor_sample(cp), NULL, cp);
		    display_channel_marks(cp);
		  }
		else 
		  {
		    if (!(delete_mark_samp(cursor_sample(cp), cp)))
		      status_report(cp->sound, "no mark at sample %" print_mus_long, cursor_sample(cp));
		  }
		if ((keysym == snd_K_M) && 
		    (cp->sound->sync != 0))
		  {
		    sync_info *si;
		    int sync_num;
		    sync_num = mark_sync_max() + 1; 
		    if (mk) set_mark_sync(mk, sync_num);
		    si = snd_sync(cp->sound->sync);
		    for (i = 0; i < si->chans; i++) 
		      if (cp != si->cps[i])
			{
			  if (count > 0)
			    {
			      mk = add_mark(cursor_sample(cp), NULL, si->cps[i]);
			      if (mk)
				{
				  set_mark_sync(mk, sync_num);
				  display_channel_marks(si->cps[i]);
				}
			    }
			  else 
			    {
			      if (!(delete_mark_samp(cursor_sample(cp), si->cps[i])))
				status_report(cp->sound, "no mark at sample %" print_mus_long, cursor_sample(cp));
			    }
			}
		    si = free_sync_info(si);
		  }
	      }
	      break;

	    case snd_K_N: case snd_K_n: 
	      cp->cursor_on = true; 
	      cursor_move(cp, count * 128); 
	      break;

	    case snd_K_O: case snd_K_o: 
	      cp->cursor_on = true; 
	      cursor_insert(cp, cursor_sample(cp), count); 
	      break;

	    case snd_K_P: case snd_K_p: 
	      cp->cursor_on = true; 
	      cursor_move(cp, -count * 128); 
	      break;

	    case snd_K_Q: case snd_K_q: 
	      play_channel(cp, cursor_sample(cp), NO_END_SPECIFIED);
	      break;

#if HAVE_EXTENSION_LANGUAGE && (!USE_NO_GUI)
	    case snd_K_S: case snd_K_s: 
	      if ((cp == last_searcher) &&
		  (find_dialog_is_active()))
		find_dialog_find(NULL, READ_FORWARD, cp);
	      last_searcher = cp;
	      find_dialog(cp);
	      break;

	    case snd_K_R: case snd_K_r: 
	      if ((cp == last_searcher) &&
		  (find_dialog_is_active()))
		find_dialog_find(NULL, READ_BACKWARD, cp);
	      last_searcher = cp;
	      find_dialog(cp);
	      break;
#endif
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp, PLAY_C_T); 
	      set_play_button(sp, false);
	      break;

	    case snd_K_U: case snd_K_u: 
	      counting = true; 
	      u_count = true;
	      number_ctr = 0; 
	      dot_seen = false; 
	      break;

	    case snd_K_V: case snd_K_v:
	      cp->cursor_on = true;
	      /* in emacs this is move ahead one window, but for some reason in Snd it's center cursor?? */
	      cursor_moveto(cp, (mus_long_t)((ap->losamp + ap->hisamp) / 2));
	      break;

	    case snd_K_W: case snd_K_w: 
	      delete_selection(UPDATE_DISPLAY); 
	      break;

	    case snd_K_X: case snd_K_x: 
	      extended_mode = true; 
	      if (got_count) 
		{
		  ext_count = count; 
		  got_ext_count = got_count;
		  got_count = false;
		}
	      break;

	    case snd_K_Y: case snd_K_y: 
	      paste_region(region_list_position_to_id(0), cp);
	      break;

	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = true; 
	      cursor_zeros(cp, count, OVER_SOUND); 
	      break;

	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state | shift));
	      break;

	    case snd_K_Left: 
	      sx_incremented(cp, -state_amount(state | shift)); 
	      break;

	    case snd_K_Up: 
	      zx_incremented(cp, 1.0 + state_amount(state | shift)); 
	      break;

	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;

	    case snd_keypad_0: case snd_keypad_1: case snd_keypad_2: case snd_keypad_3: case snd_keypad_4:
	    case snd_keypad_5: case snd_keypad_6: case snd_keypad_7: case snd_keypad_8: case snd_keypad_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_keypad_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;

	    case snd_K_space: 
	      if (count > 0)
		{
		  start_selection_creation(cp, cursor_sample(cp));
		  status_report(sp, "selection starts at %" print_mus_long, cursor_sample(cp));
		}
	      break;

	    case snd_K_period:
	    case snd_keypad_Decimal:
	      counting = true; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = true; 
	      break;

	    case snd_K_greater: 
	      cp->cursor_on = true; 
	      cursor_moveto_end(cp); 
	      break;

	    case snd_K_less: 
	      cp->cursor_on = true; 
	      cursor_moveto(cp ,0); 
	      break;

	    case snd_K_minus: 
	      counting = true; 
	      number_ctr = 1; 
	      number_buffer[0] = '-'; 
	      break;

	    case snd_K_underscore: 
	      undo_edit_with_sync(cp, count); 
	      break;

	    default:
	      status_report(sp, "C-%s undefined", key_to_name(keysym));
	      break;
	    }
	}
      else /* extended mode with ctrl down */
	{
	  /* -------------------------------- C-x C-key -------------------------------- */
	  if (!got_ext_count) ext_count = 1;
	  extended_mode = false;
	  switch (keysym)
	    {
	    case snd_K_B: case snd_K_b: 
	      set_window_bounds(cp, ext_count); 
	      break;

	    case snd_K_C: case snd_K_c: 
	      hide_controls(sp); 
	      break;

	    case snd_K_F: case snd_K_f: 
	      make_open_file_dialog(FILE_READ_WRITE, true);
	      break;

	    case snd_K_G: case snd_K_g: 
	      control_g(sp);
	      break;

	    case snd_K_J: case snd_K_j:
	      cp->cursor_on = true; 
	      goto_mix(cp, ext_count); 
	      break;

	    case snd_K_O: case snd_K_o: 
	      /* this doesn't change the View:Controls menu label because (sigh...) it's specific to the currently selected sound */
	      show_controls(sp); 
	      break;

	    case snd_K_P: case snd_K_p: 
	      set_window_size(cp, ext_count); 
	      break;

	    case snd_K_Q: case snd_K_q: 
	      make_mix_file_dialog(true);
	      break;

	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, ext_count); 
	      break;

	    case snd_K_S: case snd_K_s: 
	      save_edits_from_kbd(sp);
	      break;

	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp, PLAY_C_T); 
	      break;

	    case snd_K_U: case snd_K_u: 
	      undo_edit_with_sync(cp, ext_count); 
	      break;

	    case snd_K_V: case snd_K_v:
	      set_window_percentage(cp, ext_count);
	      break;

#if (!USE_NO_GUI)
	    case snd_K_W: case snd_K_w: 
	      {
		chan_info *cp;
		cp = any_selected_channel(any_selected_sound());
		make_channel_extract_dialog(cp->chan);
	      }
	      break;
#endif

	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = true; 
	      cos_smooth(cp, cursor_sample(cp), ext_count, OVER_SOUND); 
	      break;

	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state | shift));
	      break;

	    case snd_K_Left:  
	      sx_incremented(cp, -state_amount(state | shift)); 
	      break;

	    case snd_K_Up: 
	      zx_incremented(cp, 1.0 + state_amount(state | shift)); 
	      break;

	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    default:
	      status_report(sp, "C-x C-%s undefined", key_to_name(keysym));
	      break;
	    }
	}
    }
  else
    {
      if (!extended_mode)
	/* -------------------------------- key (or M-key) -------------------------------- */
	{ /* no control (but possibly meta), not extended mode -- bare alpha chars sent to listener if possible */
	  /*   (we already checked for possible user key bindings above) */
	  switch (keysym)
	    {
	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym-snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      break;

	    case snd_keypad_0: case snd_keypad_1: case snd_keypad_2: case snd_keypad_3: case snd_keypad_4:
	    case snd_keypad_5: case snd_keypad_6: case snd_keypad_7: case snd_keypad_8: case snd_keypad_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_keypad_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      break;

	    case snd_K_period: 
	    case snd_keypad_Decimal:
	      counting = true; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = true; 
	      break;

	    case snd_K_greater: 
	      cp->cursor_on = true; 
	      cursor_moveto_end(cp); 
	      break;

	    case snd_K_less: 
	      cp->cursor_on = true; 
	      cursor_moveto(cp, 0); 
	      break;

	    case snd_K_minus: 
	      counting = true; 
	      number_buffer[0] = '-'; 
	      number_ctr = 1; 
	      break;

	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state | shift)); 
	      break;

	    case snd_K_Left:  
	      sx_incremented(cp, -state_amount(state | shift)); 
	      break;

	    case snd_K_Up:    
	      zx_incremented(cp, 1.0 + state_amount(state | shift)); 
	      break;

	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    case snd_K_Home: 
	      redirect_everything_to(printout_to_status_area, (void *)sp);
	      sp = snd_update(sp); 
	      redirect_everything_to(NULL, NULL);
	      break;

	    case snd_K_space: 
	      if (play_in_progress())
		toggle_dac_pausing(); 
	      else play_sound(sp, cursor_sample(cp), NO_END_SPECIFIED); /* was deactivate_selection */
	      break;

	    case snd_keypad_Add:
	      if (time_graph_type(ss) == GRAPH_AS_WAVOGRAM) 
		set_wavo_trace(wavo_trace(ss) + 1); 
	      else set_spectro_hop(spectro_hop(ss) + 1);
	      reflect_spectro(); 
	      break;

	    case snd_keypad_Subtract: 
	      if (time_graph_type(ss) == GRAPH_AS_WAVOGRAM) 
		{
		  if (wavo_trace(ss) > 1) 
		    set_wavo_trace(wavo_trace(ss) - 1);
		} 
	      else 
		{
		  if (spectro_hop(ss) > 1) 
		    set_spectro_hop(spectro_hop(ss) - 1);
		}
	      reflect_spectro(); 
	      break;

	    case snd_keypad_Multiply: 
	      set_transform_size(transform_size(ss) * 2); 
	      break;

	    case snd_keypad_Divide: 
	      if (transform_size(ss) > 4) 
		set_transform_size(transform_size(ss) / 2); 
	      break;

	    case snd_keypad_Delete:
	      set_dot_size(dot_size(ss) + 1); 
	      break;

	    case snd_keypad_Insert:
	      if (dot_size(ss) > 1) 
		set_dot_size(dot_size(ss) - 1); 
	      break;

	    case snd_keypad_Enter: 
	      reset_spectro(); 
	      reflect_spectro(); 
	      break;

	    case snd_keypad_Up:
	      zoom_fft(1.0 + state_amount(state | shift));
	      break;

	    case snd_keypad_Down:
	      zoom_fft(1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    case snd_keypad_Left:
	      move_fft(-state_amount(state | shift)); 
	      break;

	    case snd_keypad_Right:
	      move_fft(state_amount(state | shift)); 
	      break;

	    default:
	      /* try to send unbuckified random keyboard input to the lisp listener */

#if USE_MOTIF
	      /* if meta key, assume for now that it's intended as a menu accelerator (don't send it to the listener) */
	      /*    in gtk (and apparently some motif's?) we don't even see the accelerator, but in Linux we do */
	      if ((state & snd_MetaMask) &&
		  ((keysym == snd_K_f) || (keysym == snd_K_e) || (keysym == snd_K_v) || (keysym == snd_K_o) || (keysym == snd_K_h)))
		/* here the accelerators are f e v o h (not upper case for some reason -- it's given as 'F' in snd-xmenu.c) */
		return;
	      /* it might be better to remove the menu accelerators -- they are a dubious feature to begin with */
#endif
#if HAVE_EXTENSION_LANGUAGE
	      {
		char buf[2];
		buf[0] = keysym; 
		buf[1] = 0;
		if (listener_is_visible())
		  {
		    goto_listener();
		    listener_append(buf);
		  }
	      }
#else
	      status_report(sp, "key %s%s undefined", (state & snd_MetaMask) ? "M-" : "", key_to_name(keysym));
#endif
	      break;
	    }
	}
      else 
	/* extended mode with ctrl up -- where not an emacs analogy, related to the current selection */
	/*   the active selection now follows the edit list, so there may be no relation between
	 *   region 0 and the active selection -- in ambiguous cases, we need to look explicitly
	 *   for the selection first.
	 */
	{
	  /* -------------------------------- C-x key -------------------------------- */
	  extended_mode = false;
	  if (!(state & snd_MetaMask))
	    {
	      switch (keysym)
		{
		case snd_K_B: case snd_K_b: 
		  cp->cursor_on = true; 
		  handle_cursor(cp, CURSOR_ON_LEFT);
		  break;

		case snd_K_C: case snd_K_c: 
		  if (!(mark_define_region(cp, (!got_ext_count) ? 1 : ext_count)))
		    set_status(cp->sound, "no such mark", false);
		  break;

		case snd_K_E: case snd_K_e: 
		  if (defining_macro) 
		    {
		      set_status(sp, "can't call macro while it's being defined", false);
		      defining_macro = false;
		      macro_size = 0; /* so subsequent M-x e doesn't get something silly */
		    }
		  else
		    {
		      execute_last_macro(cp, (!got_ext_count) ? 1 : ext_count);
		      if ((cp) && (cp->sound) && (cp->active >= CHANNEL_HAS_EDIT_LIST)) handle_cursor(cp, cursor_decision(cp)); else return;
		    }
		  break;

		case snd_K_F: case snd_K_f: 
		  cp->cursor_on = true; 
		  handle_cursor(cp, CURSOR_ON_RIGHT); 
		  break;

		case snd_K_K: case snd_K_k: 
		  snd_close_file(sp); 
		  break;

		case snd_K_L: case snd_K_l: 
		  cp->cursor_on = true;
		  if (selection_is_active_in_channel(cp))
		    cursor_moveto(cp, (mus_long_t)(selection_beg(cp) + 0.5 * selection_len()));
		  else set_status(sp, "no active selection", false);
		  handle_cursor_with_sync(cp, CURSOR_IN_MIDDLE);
		  break;

		case snd_K_O: case snd_K_o: 
		  if (ext_count > 0) 
		    goto_next_graph(cp, ext_count); 
		  else goto_previous_graph(cp, ext_count); 
		  break;

		case snd_K_P: case snd_K_p: 
		  if (!got_ext_count)
		    play_selection(IN_BACKGROUND);
		  else play_region(ext_count, IN_BACKGROUND);
		  break;

		case snd_K_Q: case snd_K_q: 
		  add_selection_or_region((!got_ext_count) ? 0 : ext_count, cp); 
		  break;

		case snd_K_R: case snd_K_r: 
		  redo_edit_with_sync(cp, (!got_ext_count) ? 1 : ext_count); 
		  break;

		case snd_K_U: case snd_K_u: 
		  undo_edit_with_sync(cp, (!got_ext_count) ? 1 : ext_count); 
		  break;

		case snd_K_V: case snd_K_v: 
		  if (selection_is_active_in_channel(cp))
		    window_framples_selection(cp); 
		  else 
		    {
		      bool complain = true;
		      if (cp->sound->channel_style != CHANNELS_SEPARATE)
			{
			  int i;
			  for (i = 0; i < (int)sp->nchans; i++)
			    if ((i != cp->chan) &&
				(selection_is_active_in_channel(sp->chans[i])))
			      {
				window_framples_selection(sp->chans[i]);
				complain = false;
				break;
			      }
			}
		      if (complain)
			set_status(sp, "no active selection", false);
		    }
		  break;

		case snd_K_Z: case snd_K_z: 
		  if (selection_is_active_in_channel(cp))
		    cos_smooth(cp, cursor_sample(cp), (!got_ext_count) ? 1 : ext_count, OVER_SELECTION); 
		  else set_status(sp, "no active selection", false);
		  break;

		case snd_K_Right:   
		  sx_incremented(cp, state_amount(state | shift));
		  break;

		case snd_K_Left:
		  sx_incremented(cp, -state_amount(state | shift));
		  break;

		case snd_K_Up:
		  zx_incremented(cp, 1.0 + state_amount(state | shift));
		  break;

		case snd_K_Down:
		  zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift)));
		  break;

		case snd_K_less:
		  cp->cursor_on = true; 
		  cursor_moveto(cp, 0); 
		  break;

		case snd_K_greater: 
		  cp->cursor_on = true; 
		  cursor_moveto_end(cp);
		  break;

		case snd_K_openparen:
		  if (defining_macro) 
		    set_status(sp, "macro definition already in progress", false);
		  else
		    {
		      start_defining_macro(); 
		      set_status(sp, "defining macro...", false); 
		    }
		  break;

		case snd_K_closeparen: 
		  if (defining_macro)
		    {
		      stop_defining_macro(); 
		      clear_status_area(sp); 
		    }
		  break;

		default:
		  status_report(sp, "C-x %s undefined", key_to_name(keysym));
		  break;
		}
	    }
	  else
	    {
	      status_report(sp, "C-x M-%s undefined", key_to_name(keysym));
	    }
	}
    }
  if (!extended_mode) {got_ext_count = false; ext_count = 1;}
}



char *make_key_name(char *buf, int buf_size, int key, int state, bool extended)
{
  snprintf(buf, buf_size, "%s%s%s",
	       (extended) ? "C-x " : "",
	       (state & snd_ControlMask) ? ((state & snd_MetaMask) ? "CM-" : "C-") : ((state & snd_MetaMask) ? "M-" : ""),
	       (key == snd_K_less) ? "<" : 
	       ((key == snd_K_greater) ? ">" : 
		((key == snd_K_openparen) ? "(" :
		 ((key == snd_K_closeparen) ? ")" :
		  ((key == snd_K_slash) ? "/" :
		   KEY_TO_NAME(key))))));
  return(buf);
}


static int key_name_to_key(Xen key, const char *caller)
{
  /* Ruby thinks chars are strings */
  if (Xen_is_integer(key))
    return(Xen_integer_to_C_int(key)); /* includes 0xffc0 style keys, and in Ruby things like ?a */

#if (!HAVE_RUBY)
  if (Xen_is_char(key))
    return((int)(Xen_char_to_C_char(key)));
#endif

#if USE_MOTIF
  return((int)XStringToKeysym(Xen_string_to_C_string(key)));  /* these are the X names: not "+" but "plus" etc */
#endif
#if USE_GTK
  return((int)gdk_keyval_from_name(Xen_string_to_C_string(key)));
#endif
  return(0);
}


static Xen check_for_key_error(int k, int s, const char *caller)
{
  if ((k < MIN_KEY_CODE) || (k > MAX_KEY_CODE) ||
      (s < MIN_KEY_STATE) || (s > MAX_KEY_STATE))
    Xen_error(Xen_make_error_type("no-such-key"),
	      Xen_list_4(C_string_to_Xen_string("~A: no such key: ~A, state: ~A"),
			 C_string_to_Xen_string(caller),
			 C_int_to_Xen_integer(k),
			 C_int_to_Xen_integer(s)));
  return(Xen_false);
}


static Xen g_key_binding(Xen key, Xen state, Xen cx_extended)
{
  #define H_key_binding "(" S_key_binding " key :optional (state 0) extended): function bound to this key and associated \
modifiers.  As in " S_bind_key ", state is the logical 'or' of ctrl=4, meta=8, and 'extended' is " PROC_TRUE " if the key is \
prefixed with C-x. 'key' can be a character, a key name such as 'Home', or an integer."
  int i, k, s;

  Xen_check_type(Xen_is_integer(key) || Xen_is_char(key) || Xen_is_string(key), key, 1, S_key_binding, "an integer, character, or string");
  Xen_check_type(Xen_is_integer_or_unbound(state), state, 2, S_key_binding, "an integer");
  Xen_check_type(Xen_is_boolean_or_unbound(cx_extended), cx_extended, 3, S_key_binding, "a boolean");

  k = key_name_to_key(key, S_key_binding);
  s = ((Xen_is_integer(state)) ? Xen_integer_to_C_int(state) : 0) & 0xfffe; /* no shift bit */
  check_for_key_error(k, s, S_key_binding);
  i = in_keymap(k, s, Xen_is_true(cx_extended));
  if (i >= 0) 
    return(keymap[i].func);

  return(Xen_undefined);
}


static Xen g_bind_key_1(Xen key, Xen state, Xen code, Xen cx_extended, Xen origin, Xen prefs_info, const char *caller)
{
  int k, s;
  bool e;

  Xen_check_type(Xen_is_integer(key) || Xen_is_string(key) || Xen_is_char(key), key, 1, caller, "an integer, char, or string");
  Xen_check_type(Xen_is_integer(state), state, 2, caller, "an integer");
  Xen_check_type((Xen_is_false(code) || Xen_is_procedure(code)), code, 3, caller, PROC_FALSE " or a procedure");
  Xen_check_type(Xen_is_boolean_or_unbound(cx_extended), cx_extended, 4, caller, "a boolean");
  Xen_check_type(Xen_is_string_or_unbound(origin), origin, 5, caller, "a string");
  Xen_check_type(Xen_is_string_or_unbound(prefs_info), prefs_info, 6, caller, "a string");

  k = key_name_to_key(key, caller);
  s = Xen_integer_to_C_int(state) & 0xfffe; /* get rid of shift bit */
  check_for_key_error(k, s, caller);
  e = (Xen_is_true(cx_extended));

  if (Xen_is_false(code))
    set_keymap_entry(k, s, 0, Xen_undefined, e, NULL, NULL);
  else 
    {
      int args;
      char buf[256];
      const char *comment = NULL, *prefs = NULL;
      args = Xen_required_args(code);
      if (args > 1)
	{
	  Xen errmsg;
	  char *errstr;
	  errstr = mus_format(S_bind_key " function arg should take either zero or one args, not %d", args);
	  errmsg = C_string_to_Xen_string(errstr);

	  free(errstr);
	  return(snd_bad_arity_error(caller, errmsg, code));
	}
      if (Xen_is_string(origin))
	comment = Xen_string_to_C_string(origin); 
      else comment = make_key_name(buf, 256, k, s, e);
      if (Xen_is_string(prefs_info)) prefs = Xen_string_to_C_string(prefs_info);
      set_keymap_entry(k, s, args, code, e, comment, prefs);
    }
  return(code);
}


static Xen g_bind_key(Xen key, Xen state, Xen code, Xen cx_extended, Xen origin, Xen prefs_info)
{
  #define H_bind_key "(" S_bind_key " key modifiers func :optional extended origin prefs-info): \
causes 'key' (an integer, character, or string) \
when typed with 'modifiers' (0:none, 4:control, 8:meta) (and C-x if extended) to invoke 'func', a function of \
zero or one arguments. If the function takes one argument, it is passed the preceding C-u number, if any. \
The function should return one of the cursor choices (e.g. " S_keyboard_no_action ").  'origin' is \
the name reported if an error occurs. The 'key' argument can be the X/Gtk name of the key (e.g. \"plus\" for \"+\" or \"Home\"), \
the character on the key (#\\a), or the integer corresponding to that character: (\"(char->integer #\\a)\" in Scheme, \
\"?a\" in Ruby, \
or \"<char> a\" in Forth)."
  
  return(g_bind_key_1(key, state, code, cx_extended, origin, prefs_info, S_bind_key));
}


static Xen g_unbind_key(Xen key, Xen state, Xen cx_extended)
{
  #define H_unbind_key "(" S_unbind_key " key state :optional extended): undo the effect of a prior " S_bind_key " call."
  return(g_bind_key_1(key, state, Xen_false, cx_extended, Xen_undefined, Xen_undefined, S_unbind_key));
}


static Xen g_key(Xen kbd, Xen buckybits, Xen snd, Xen chn)
{
  #define H_key "(" S_key " key modifiers :optional snd chn): simulate typing 'key' with 'modifiers' in snd's channel chn"
  chan_info *cp;
  int k, s;

  Xen_check_type(Xen_is_integer(kbd) || Xen_is_char(kbd) || Xen_is_string(kbd), kbd, 1, S_key, "an integer, character, or string");
  Xen_check_type(Xen_is_integer(buckybits), buckybits, 2, S_key, "an integer");
  Snd_assert_channel(S_key, snd, chn, 3);

  cp = get_cp(snd, chn, S_key);
  if (!cp) return(Xen_false);

  k = key_name_to_key(kbd, S_key);
  s = Xen_integer_to_C_int(buckybits);
  check_for_key_error(k, s, S_key);
  keyboard_command(cp, k, s);

  return(kbd);
}


Xen_wrap_3_optional_args(g_key_binding_w, g_key_binding)
Xen_wrap_6_optional_args(g_bind_key_w, g_bind_key)
Xen_wrap_3_optional_args(g_unbind_key_w, g_unbind_key)
Xen_wrap_4_optional_args(g_key_w, g_key)

void g_init_kbd(void)
{
  int i;
#if HAVE_SCHEME
  s7_pointer n, t, b, s;
  t = s7_t(s7);
  n = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  s = s7_make_symbol(s7, "string?");
#endif

  #define H_cursor_in_view     "The value for a " S_bind_key " function that moves the window so that the cursor is in the view"
  #define H_cursor_on_left     "The value for a " S_bind_key " function that moves the window so that the cursor is at the left edge"
  #define H_cursor_on_right    "The value for a " S_bind_key " function that moves the window so that the cursor is at the right edge"
  #define H_cursor_in_middle   "The value for a " S_bind_key " function that moves the window so that the cursor is in the middle"
  #define H_keyboard_no_action "The value for a " S_bind_key " function that does nothing upon return"

  Xen_define_constant(S_cursor_in_view,     CURSOR_IN_VIEW,     H_cursor_in_view);
  Xen_define_constant(S_cursor_on_left,     CURSOR_ON_LEFT,     H_cursor_on_left);
  Xen_define_constant(S_cursor_on_right,    CURSOR_ON_RIGHT,    H_cursor_on_right);
  Xen_define_constant(S_cursor_in_middle,   CURSOR_IN_MIDDLE,   H_cursor_in_middle);
  Xen_define_constant(S_keyboard_no_action, KEYBOARD_NO_ACTION, H_keyboard_no_action);

  Xen_define_typed_procedure(S_key_binding, g_key_binding_w, 1, 2, 0, H_key_binding, s7_make_signature(s7, 4, t, t, n, b));
  Xen_define_typed_procedure(S_bind_key,    g_bind_key_w,    3, 3, 0, H_bind_key,    s7_make_signature(s7, 7, t, t, n, t, b, s, s)); 
  Xen_define_typed_procedure(S_unbind_key,  g_unbind_key_w,  2, 1, 0, H_unbind_key,  s7_make_signature(s7, 4, t, t, n, b));
  Xen_define_unsafe_typed_procedure(S_key,  g_key_w,         2, 2, 0, H_key,         s7_make_signature(s7, 5, t, n, n, t, t));

  for (i = 0; i < NUM_BUILT_IN_KEYS; i++)
    built_in_keys[i].func = Xen_false;
}
