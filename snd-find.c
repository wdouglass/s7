#include "snd.h"

static void clear_search_state(void)
{
  if (Xen_is_procedure(ss->search_proc)) 
    {
      snd_unprotect_at(ss->search_proc_loc);
      ss->search_proc_loc = NOT_A_GC_LOC;
    }
  ss->search_proc = Xen_undefined;

  if (ss->search_expr) free(ss->search_expr);
  ss->search_expr = NULL;
}


#if (!USE_NO_GUI)

static void find_report(chan_info *cp, const char *msg)
{
  /* msg can be NULL */
  if (cp)
    status_report(cp->sound, "%s", msg);
  find_dialog_set_label(msg);
}


static bool search_in_progress = false;
static chan_info *previous_channel = NULL;
#define MANY_PASSES 100000

static mus_long_t channel_find_forward(chan_info *cp)
{
  mus_long_t i, end, start;

  end = current_samples(cp);
  start = cursor_sample(cp) + 1;
  if (start >= end)
    start = 0;
  i = scan_channel(cp, start, end, ss->search_proc);
  if (i < end)
    return(i);
  return(-1);
}


static mus_long_t channel_find_backward(chan_info *cp)
{
  bool reported = false;
  mus_long_t i, start, passes;
  snd_fd *sf = NULL;
  Xen res;

  start = cursor_sample(cp) - 1;
  if (start < 0)
    start = current_samples(cp) - 1;

  sf = init_sample_read(start, cp, READ_BACKWARD);
  if (!sf)
    return(-1);

  ss->stopped_explicitly = false;
  for (i = start, passes = 0; i >= 0; i--, passes++)
    {
      res = Xen_call_with_1_arg(ss->search_proc, 
				C_double_to_Xen_real((double)(read_sample(sf))), 
				"search function");
      if (!Xen_is_false(res)) 
	break;
      if (passes >= MANY_PASSES)
	{
	  passes = 0;
	  check_for_event();
	  if (!(ss->stopped_explicitly))
	    {
	      char *msg;
	      msg = mus_format("search at minute %d", (int)floor(i / (snd_srate(cp->sound) * 60)));
	      find_report(cp, msg);
	      free(msg);
	      reported = true;
	    }
	  /* if user types C-s during an active search, we risk stomping on our current pointers */
	  if (!(cp->sound->active)) break;
	}
      if (ss->stopped_explicitly) break;
    }

  ss->stopped_explicitly = false;
  if (reported) find_report(cp, NULL);
  free_snd_fd(sf);

  if (i >= 0)
    return(i);
  return(-1);
}


static char *channel_search(chan_info *cp, read_direction_t direction)
{
  mus_long_t samp;
  char *s1, *s2, *msg;

  if (direction == READ_FORWARD)
    samp = channel_find_forward(cp);
  else samp = channel_find_backward(cp);
  
  previous_channel = cp;

  if (samp == -1)
    return(NULL);

  s1 = prettyf(chn_sample(samp, cp, cp->edit_ctr), 2);
  s2 = x_axis_location_to_string(cp, (double)samp / (double)snd_srate(cp->sound));
  msg = mus_format("%s at %s (%" print_mus_long ")", s1, s2, samp);
  cursor_moveto_without_verbosity(cp, samp);
  free(s1);
  free(s2);

  return(msg);
}


static char *global_search(read_direction_t direction, bool repeating)
{
  int i;

  if ((repeating) &&
      ((!previous_channel) ||
       (!(previous_channel->sound)) ||
       (!(previous_channel->sound->active))))
    repeating = false;
       
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      uint32_t j;
      sp = ss->sounds[i];
      if ((sp) &&
	  (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  {
	    chan_info *cp;
	    cp = (chan_info *)(sp->chans[j]);
	    if ((!repeating) ||
		(cp == previous_channel))
	      {
		char *msg;
		repeating = false; /* after we find the channel, look at everything */
		msg = channel_search(cp, direction);
		if (msg)
		  return(msg);
	      }
	  }
    }
  return(NULL);
}


#if HAVE_EXTENSION_LANGUAGE
void find_dialog_find(char *str, read_direction_t direction, chan_info *cp)
{
  Xen proc;
  bool repeating_search = false;

  if (search_in_progress) 
    {
      find_report(cp, "search already in progress");
      return;
    }

  proc = Xen_false;

  /* str can be null, or equal to the previous call's str -- in this case use
   *   the current search procedure if possible, else complain.
   * if str not null, make a new (local?) search-procedure
   */

  if ((!str) || 
      (!(*str)) ||
      (mus_strcmp(str, ss->search_expr)))
    {
      proc = ss->search_proc;
      if (!(Xen_is_procedure(proc)))
	return;
      repeating_search = true;
    }
  else
    {
      char *buf = NULL;

      redirect_errors_to(errors_to_find_text, NULL);
      proc = snd_catch_any(eval_str_wrapper, str, str);
      redirect_errors_to(NULL, NULL);

      if ((!(Xen_is_procedure(proc))) ||
	  (!(procedure_arity_ok(proc, 1))))
	return;

      clear_search_state(); /* free previous, if any */
      repeating_search = false;

      ss->search_proc = proc;
      ss->search_expr = mus_strdup(str);
      ss->search_proc_loc = snd_protect(proc);

      buf = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      snprintf(buf, PRINT_BUFFER_SIZE, "%s %s", I_find, str);
      find_dialog_set_label(buf);
      free(buf);
    }

  /* now we have a search procedure, possibly optimized */

  search_in_progress = true;
  find_dialog_stop_label(true);
  redirect_xen_error_to(stop_search_if_error, NULL);
  if (cp)
    str = channel_search(cp, direction);
  else str = global_search(direction, repeating_search);
  redirect_xen_error_to(NULL, NULL);
  find_dialog_stop_label(false);
  search_in_progress = false;

  if ((str) && (*str)) 
    {
      find_report(cp, str);
      free(str);
    }
  else find_report(cp, "not found");
}
#endif
#endif
/* end no gui */



/* -------------------------------------------------------------------------------- */

static Xen g_search_procedure(void)
{
  #define H_search_procedure "(" S_search_procedure "): the function used by the find dialog or C-s if none is otherwise specified."
  return(ss->search_proc);
}


static Xen g_set_search_procedure(Xen proc)
{
  char *error = NULL;
  /* (set! (search-procedure) (lambda (y) #t)) -> #<procedure #f ((n) #t)> */
  
  Xen_check_type(Xen_is_procedure(proc) || Xen_is_false(proc), proc, 1, S_set S_search_procedure, "a procedure or " PROC_FALSE);

  error = procedure_ok(proc, 1, S_search_procedure, "search", 1);
  if (!error)
    {
      clear_search_state();
      if (Xen_is_procedure(proc))
	{
	  ss->search_proc = proc;
	  ss->search_proc_loc = snd_protect(proc);
	}
    }
  else 
    {
      Xen errstr;
      errstr = C_string_to_Xen_string(error);
      free(error);
      return(snd_bad_arity_error(S_set S_search_procedure, errstr, proc));
    }
  return(proc);
}


Xen_wrap_no_args(g_search_procedure_w, g_search_procedure)
Xen_wrap_1_arg(g_set_search_procedure_w, g_set_search_procedure)

void g_init_find(void)
{
  Xen_define_typed_dilambda(S_search_procedure, g_search_procedure_w, H_search_procedure,
			    S_set S_search_procedure, g_set_search_procedure_w,  0, 0, 1, 0,
			    s7_make_circular_signature(s7, 0, 1, s7_t(s7)), s7_make_circular_signature(s7, 0, 1, s7_t(s7)));
}
