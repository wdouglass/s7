/* this file included as text in snd-g|xprefs.c */

static void int_to_textfield(widget_t w, int val)
{
  char *str;
  str = (char *)calloc(16, sizeof(char));
  snprintf(str, 16, "%d", val);
  SET_TEXT(w, str);
  free(str);
}


static void mus_long_t_to_textfield(widget_t w, mus_long_t val)
{
  char *str;
  str = (char *)calloc(32, sizeof(char));
  snprintf(str, 32, "%" PRId64, val);
  SET_TEXT(w, str);
  free(str);
}


static void float_to_textfield(widget_t w, mus_float_t val)
{
  char *str;
  str = (char *)calloc(12, sizeof(char));
  snprintf(str, 12, "%.3f", val);
  SET_TEXT(w, str);
  free(str);
}


static void float_1_to_textfield(widget_t w, mus_float_t val)
{
  char *str;
  str = (char *)calloc(12, sizeof(char));
  snprintf(str, 12, "%.1f", val);
  SET_TEXT(w, str);
  free(str);
}


static TIMEOUT_TYPE unpost_any_error(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  prf->got_error = false;
  black_text(prf);
  set_label(prf->label, prf->saved_label);
  if (prf->reflect_func) (*(prf->reflect_func))(prf);
  TIMEOUT_RESULT
}


static void any_error_to_text(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  red_text(prf);
  set_label(prf->label, msg);
  TIMEOUT(unpost_any_error);
}


static void redirect_post_prefs_error(const char *msg, void *data)
{
  post_prefs_error(msg, (prefs_info *)data);
}


static int prefs_size = 0, prefs_top = 0;
static prefs_info **prefs = NULL;

static void remember_pref(prefs_info *prf, 
			  void (*reflect_func)(struct prefs_info *prf),
			  void (*save_func)(struct prefs_info *prf, FILE *fd),
			  const char *(*help_func)(struct prefs_info *prf),
			  void (*clear_func)(struct prefs_info *prf),
			  void (*revert_func)(struct prefs_info *prf))
{
  if (prefs_size == 0)
    {
      prefs_size = 100;
      prefs = (prefs_info **)calloc(prefs_size, sizeof(prefs_info *));
    }
  else
    {
      if (prefs_top >= prefs_size)
	{
	  int i;
	  prefs_size += 100;
	  prefs = (prefs_info **)realloc(prefs, prefs_size * sizeof(prefs_info *));
	  for (i = prefs_top; i < prefs_size; i++) prefs[i] = NULL;
	}
    }

  prf->reflect_func = reflect_func;
  prf->save_func = save_func;
  prf->help_func = help_func;
  prf->clear_func = clear_func;
  prf->revert_func = revert_func;
  prefs[prefs_top++] = prf;

  if ((help_func) &&
      (prf->label))
    {
      if (prf->var_name)
	{
	  char *str;
	  str = mus_format("%s\n  See %s for more info.", (*help_func)(prf), prf->var_name);
	  add_tooltip(prf->label, str); /* don't free it... */
	}
      else add_tooltip(prf->label, (*help_func)(prf));
    }
}


static void reflect_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->reflect_func))
	(*(prf->reflect_func))(prf);
    }
}


static void revert_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->revert_func))
	(*(prf->revert_func))(prf);
    }
}


static void clear_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->clear_func))
	(*(prf->clear_func))(prf);
    }
}


static void preferences_revert_or_clear(bool revert)
{
  clear_prefs_dialog_error();
  if (revert)
    revert_prefs();
  else
    {
      snd_set_global_defaults(true);
      clear_prefs();
    }

  reflect_prefs();
  prefs_unsaved = false;

  if (prefs_saved_filename) 
    {
      char *fullname;
      fullname = mus_expand_filename(prefs_saved_filename);
      if (mus_file_probe(fullname))
	snd_remove(fullname, IGNORE_CACHE);
      free(prefs_saved_filename);
      free(fullname);
      prefs_saved_filename = NULL;
    }
  prefs_set_dialog_title(NULL);
}


static bool local_access(char *dir)
{
  int err;
  char *temp;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err != -1)
    {
      snd_close(err, temp);
      snd_remove(temp, IGNORE_CACHE);
    }
  free(temp);
  return(err != -1);
}


static bool is_string_member(const char *val, char **lst, int len)
{
  int i;
  if ((len == 0) || (!lst) || (!val)) return(false);
  for (i = 0; i < len; i++)
    if (mus_strcmp(val, lst[i]))
      return(true);
  return(false);
}


static char **load_path_to_string_array(int *len)
{
  char **cdirs = NULL;
  int dir_len = 0, j = 0;
  Xen dirs;

  dirs = Xen_load_path;
  dir_len = Xen_list_length(dirs);

  if (dir_len > 0)
    {
      int i;
      cdirs = (char **)calloc(dir_len, sizeof(char *));
      for (i = 0; i < dir_len; i++)
	{
	  const char *path;
	  path = Xen_string_to_C_string(Xen_list_ref(dirs, i));
	  if ((path) && (!(is_string_member(path, cdirs, j))))   /* try to remove duplicates */
	    cdirs[j++] = mus_strdup(path);
	}
    }

  (*len) = j;
  return(cdirs);
}


static void add_local_load_path(FILE *fd, char *path)
{
#if HAVE_RUBY
  fprintf(fd, "if (not $LOAD_PATH.include?(\"%s\")) then $LOAD_PATH.push(\"%s\") end\n", path, path);
#endif

#if HAVE_FORTH
  /* this already checks */
  fprintf(fd, "\"%s\" add-load-path\n", path); /* no drop here */
#endif

#if HAVE_SCHEME
  fprintf(fd, "(if (not (member \"%s\" *load-path*)) (set! *load-path* (cons \"%s\" *load-path*)))\n", path, path);
#endif
}


#if HAVE_EXTENSION_LANGUAGE
static void save_prefs(void)
{
  char *fullname;
  const char *filename;
  FILE *fd;

  /* save_options_in_prefs passes us the filename after calling save_options which handles all
   *   the simple cases.  
   */
  
  filename = save_options_in_prefs();
  if (!filename) return;

  fullname = mus_expand_filename(filename);
  fd = FOPEN(fullname, "a");

  if (fd)
    {
      char **current_dirs;
      int i, current_dirs_len = 0;

      fprintf(fd, "\n");

      /* LOAD_PATH has the current load-path list,
       *   GET_TEXT(load_path_text_widget) has the current text (independent of activation)
       *   include_load_path has whatever the last <cr> set it to.
       *
       * load_path_to_string_array can turn the LOAD_PATH into a char** array.
       *
       * load-path needs to be set even if a later init file adds to it; we need a true
       *   load-path, but this can be called repeatedly, and across executions, so we 
       *   don't want to fill up the list with repetitions
       */

      current_dirs = load_path_to_string_array(&current_dirs_len);
      if (current_dirs)
	for (i = current_dirs_len - 1; i >= 0; i--) /* consing on front, so keep original order of paths */
	  add_local_load_path(fd, current_dirs[i]); /* don't try to be smart about startup paths -- just include everybody */

      if ((include_load_path) &&
	  (!(is_string_member(include_load_path, current_dirs, current_dirs_len))))
	add_local_load_path(fd, include_load_path);

      if (load_path_text_widget)
	{
	  char *unchecked_load_path;
	  unchecked_load_path = GET_TEXT(load_path_text_widget);
	  if ((unchecked_load_path) &&                                                          /* text widget has an entry */
	      (local_access(unchecked_load_path)) &&                                            /* it's a legit path */
	      (!(is_string_member(unchecked_load_path, current_dirs, current_dirs_len))) &&      /* it's not in LOAD_PATH */
	      (!(mus_strcmp(unchecked_load_path, include_load_path))))                          /* it's not already included above */
	    add_local_load_path(fd, unchecked_load_path);
	  if (unchecked_load_path) {free_TEXT(unchecked_load_path);} /* a no-op in gtk */
	}

      if (current_dirs)
	{
	  for (i = 0; i < current_dirs_len; i++)
	    if (current_dirs[i]) free(current_dirs[i]);
	  free(current_dirs);
	  current_dirs = NULL;
	}

      /* now finally the load path is set up, so we can call load if we need to */
      for (i = 0; i < prefs_top; i++)
	{
	  prefs_info *prf;
	  prf = prefs[i];
	  if ((prf) &&
	      (prf->save_func))
	    (*(prf->save_func))(prf, fd);
	}
      snd_fclose(fd, filename);
    }
  else snd_error("can't save preferences: %s %s", filename, snd_io_strerror());
  free(fullname);
  prefs_unsaved = false;
  prefs_set_dialog_title(filename);
}
#endif


static char *trim_string(const char *str)
{
  int i = 0, len, j = 0, k, m;
  char *trimmed_str;
  len = strlen(str);
  trimmed_str = (char *)calloc(len + 1, sizeof(char));
  while ((i < len) && (isspace((int)str[i]))) i++;
  k = len - 1;
  while ((k > i) && (isspace((int)str[k]))) k--;
  for (m = i; m <= k; m++)
    trimmed_str[j++] = str[m];
  return(trimmed_str);
}


static char key_buf[16];

static char *possibly_quote(char *key)
{
  int i, j, len;
  len = mus_strlen(key);
  if (len > 12) len = 12;
  for (i = 0, j = 0; i < len; i++)
    if (!(isspace((int)key[i])))
      {
	if ((j == 0) && (isalpha((int)key[i])))
	  key_buf[j++] = '\"';
	key_buf[j++] = key[i];
      }
  if ((key_buf[0] == '\"') && (key_buf[j - 1] != '\"'))
    key_buf[j++] = '\"';
  key_buf[j++] = '\0';
#if 0
  fprintf(stderr,"return key %s\n", key_buf);
#endif
  return(key_buf);
}


/* ---------------- auto-resize ---------------- */

static bool rts_auto_resize = DEFAULT_AUTO_RESIZE;
static void reflect_auto_resize(prefs_info *prf) {SET_TOGGLE(prf->toggle, auto_resize(ss));}
static void resize_toggle(prefs_info *prf) {set_auto_resize(GET_TOGGLE(prf->toggle));}
static void revert_auto_resize(prefs_info *prf) {set_auto_resize(rts_auto_resize);}
static void save_auto_resize(prefs_info *prf, FILE *ignore) {rts_auto_resize = auto_resize(ss);}

static const char *help_auto_resize(prefs_info *prf)
{
  return("\
  If this option is set, Snd's main window   \n\
  changes size as sounds come and go.       ");
}



/* ---------------- ask-before-overwrite ---------------- */

static bool rts_ask_before_overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
static void reflect_ask_before_overwrite(prefs_info *prf) {SET_TOGGLE(prf->toggle, ask_before_overwrite(ss));}
static void overwrite_toggle(prefs_info *prf) {set_ask_before_overwrite(GET_TOGGLE(prf->toggle));}
static void revert_ask_before_overwrite(prefs_info *prf) {set_ask_before_overwrite(rts_ask_before_overwrite);}
static void save_ask_before_overwrite(prefs_info *prf, FILE *ignore) {rts_ask_before_overwrite = ask_before_overwrite(ss);}

static const char *help_ask_before_overwrite(prefs_info *prf)
{
  return("\
  If this option is set, Snd will ask you before   \n\
  it overwrites an existing sound.  ");
}



/* ---------------- show-controls ---------------- */

static bool rts_show_controls = DEFAULT_SHOW_CONTROLS;
static void reflect_show_controls(prefs_info *prf) {SET_TOGGLE(prf->toggle, in_show_controls(ss));}
static void controls_toggle(prefs_info *prf) {set_show_controls(GET_TOGGLE(prf->toggle));}
static void revert_show_controls(prefs_info *prf) {set_show_controls(rts_show_controls);}
static void save_show_controls(prefs_info *prf, FILE *ignore) {rts_show_controls = in_show_controls(ss);}


/* ---------------- just-sounds ---------------- */

static bool rts_just_sounds = DEFAULT_JUST_SOUNDS;
static void prefs_reflect_just_sounds(prefs_info *prf) {SET_TOGGLE(prf->toggle, just_sounds(ss));}
static void just_sounds_toggle(prefs_info *prf) {set_just_sounds(GET_TOGGLE(prf->toggle));}
static void revert_just_sounds(prefs_info *prf) {set_just_sounds(rts_just_sounds);}
static void save_just_sounds(prefs_info *prf, FILE *ignore) {rts_just_sounds = just_sounds(ss);}


/* ---------------- verbose-cursor ---------------- */

static bool rts_with_verbose_cursor = DEFAULT_WITH_VERBOSE_CURSOR;
static void reflect_with_verbose_cursor(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_verbose_cursor(ss));}
static void with_verbose_cursor_toggle(prefs_info *prf) {set_with_verbose_cursor(GET_TOGGLE(prf->toggle));}
static void revert_with_verbose_cursor(prefs_info *prf) {set_with_verbose_cursor(rts_with_verbose_cursor);}
static void save_with_verbose_cursor(prefs_info *prf, FILE *ignore) {rts_with_verbose_cursor = with_verbose_cursor(ss);}


/* ---------------- graphs-horizontal ---------------- */

static bool rts_graphs_horizontal = DEFAULT_GRAPHS_HORIZONTAL;
static void reflect_graphs_horizontal(prefs_info *prf) {SET_TOGGLE(prf->toggle, graphs_horizontal(ss));}
static void graphs_horizontal_toggle(prefs_info *prf) {in_set_graphs_horizontal(GET_TOGGLE(prf->toggle));}
static void revert_graphs_horizontal(prefs_info *prf) {in_set_graphs_horizontal(rts_graphs_horizontal);}
static void save_graphs_horizontal(prefs_info *prf, FILE *ignore) {rts_graphs_horizontal = graphs_horizontal(ss);}


/* ---------------- show-y-zero ---------------- */

static bool rts_show_y_zero = DEFAULT_SHOW_Y_ZERO;
static void reflect_show_y_zero(prefs_info *prf) {SET_TOGGLE(prf->toggle, show_y_zero(ss));}
static void y_zero_toggle(prefs_info *prf) {set_show_y_zero(GET_TOGGLE(prf->toggle));}
static void revert_show_y_zero(prefs_info *prf) {set_show_y_zero(rts_show_y_zero);}
static void save_show_y_zero(prefs_info *prf, FILE *ignore) {rts_show_y_zero = show_y_zero(ss);}


/* ---------------- show-grid ---------------- */

static with_grid_t rts_show_grid = DEFAULT_SHOW_GRID;
static void reflect_show_grid(prefs_info *prf) {SET_TOGGLE(prf->toggle, show_grid(ss) == WITH_GRID);}
static void grid_toggle(prefs_info *prf) {set_show_grid(((GET_TOGGLE(prf->toggle)) ? WITH_GRID : NO_GRID));}
static void revert_show_grid(prefs_info *prf) {set_show_grid(rts_show_grid);}
static void save_show_grid(prefs_info *prf, FILE *ignore) {rts_show_grid = show_grid(ss);}


/* ---------------- fft-log-magnitude ---------------- */

static bool rts_fft_log_magnitude = DEFAULT_FFT_LOG_MAGNITUDE;
static void reflect_fft_log_magnitude(prefs_info *prf) {SET_TOGGLE(prf->toggle, fft_log_magnitude(ss));}
static void log_magnitude_toggle(prefs_info *prf) {set_fft_log_magnitude(GET_TOGGLE(prf->toggle));}
static void revert_fft_log_magnitude(prefs_info *prf) {set_fft_log_magnitude(rts_fft_log_magnitude);}
static void save_fft_log_magnitude(prefs_info *prf, FILE *ignore) {rts_fft_log_magnitude = fft_log_magnitude(ss);}


/* ---------------- fft-log-frequency ---------------- */

static bool rts_fft_log_frequency = DEFAULT_FFT_LOG_FREQUENCY;
static void reflect_fft_log_frequency(prefs_info *prf) {SET_TOGGLE(prf->toggle, fft_log_frequency(ss));}
static void log_frequency_toggle(prefs_info *prf) {set_fft_log_frequency(GET_TOGGLE(prf->toggle));}
static void revert_fft_log_frequency(prefs_info *prf) {set_fft_log_frequency(rts_fft_log_frequency);}
static void save_fft_log_frequency(prefs_info *prf, FILE *ignore) {rts_fft_log_frequency = fft_log_frequency(ss);}


/* ---------------- dac-combines-channels ---------------- */

static bool rts_dac_combines_channels = DEFAULT_DAC_COMBINES_CHANNELS;
static void reflect_dac_combines_channels(prefs_info *prf) {SET_TOGGLE(prf->toggle, dac_combines_channels(ss));}
static void dac_combines_channels_toggle(prefs_info *prf) {set_dac_combines_channels(GET_TOGGLE(prf->toggle));}
static void revert_dac_combines_channels(prefs_info *prf) {set_dac_combines_channels(rts_dac_combines_channels);}
static void save_dac_combines_channels(prefs_info *prf, FILE *ignore) {rts_dac_combines_channels = dac_combines_channels(ss);}


/* ---------------- show-listener ---------------- */

static bool rts_show_listener = false, prefs_show_listener = false;

static void reflect_show_listener(prefs_info *prf) 
{
  prefs_show_listener = listener_is_visible();
  SET_TOGGLE(prf->toggle, prefs_show_listener);
}


static void show_listener_toggle(prefs_info *prf)
{
  prefs_show_listener = GET_TOGGLE(prf->toggle);
  handle_listener(prefs_show_listener);
}


static void save_show_listener(prefs_info *prf, FILE *fd)
{
  rts_show_listener = prefs_show_listener;
}


static void revert_show_listener(prefs_info *prf)
{
  prefs_show_listener = rts_show_listener;
  handle_listener(rts_show_listener);
}


static void clear_show_listener(prefs_info *prf)
{
  prefs_show_listener = false;
  handle_listener(false);
}



/* ---------------- basic-color ---------------- */

/* we need the original color (Clear), the last saved color (Revert)
 *   the colors are updated continuously, so the current color variable (and the reflection func) is irrelevant
 *   so: 
 *       set original in snd-gxmain or somewhere
 *       set rts in prefs dialog startup
 *       reflect_color: a no-op
 *       save_color: save current color_t value in rts value (actual fd output dealt with elsewhere)
 *       clear_color: set to original (leave rts value alone)
 *       revert_color: set to rts (leave original alone)
 *       help_color: built-in via color variable name
 *       color_func: set color based on rgb values
 */

static color_t saved_basic_color;
static void save_basic_color(prefs_info *prf, FILE *ignore) {saved_basic_color = ss->basic_color;}
static void basic_color_func(prefs_info *prf, double r, double g, double b) {set_basic_color(rgb_to_color(r, g, b));}


static void revert_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_basic_color); 
  set_basic_color(saved_basic_color);
}


static void clear_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->basic_color);
  set_basic_color(ss->orig_basic_color);
}



/* ---------------- highlight-color ---------------- */

static color_t saved_highlight_color;
static void save_highlight_color(prefs_info *prf, FILE *ignore) {saved_highlight_color = ss->highlight_color;}
static void highlight_color_func(prefs_info *prf, double r, double g, double b) {set_highlight_color(rgb_to_color(r, g, b));}


static void revert_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_highlight_color); 
  set_highlight_color(saved_highlight_color);
}


static void clear_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_highlight_color); 
  set_highlight_color(ss->orig_highlight_color);
}



/* ---------------- position-color ---------------- */

static color_t saved_position_color;
static void save_position_color(prefs_info *prf, FILE *ignore) {saved_position_color = ss->position_color;}
static void position_color_func(prefs_info *prf, double r, double g, double b) {set_position_color(rgb_to_color(r, g, b));}


static void revert_position_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_position_color); 
  set_position_color(saved_position_color);
}


static void clear_position_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_position_color); 
  set_position_color(ss->orig_position_color);
}



/* ---------------- zoom-color ---------------- */

static color_t saved_zoom_color;
static void save_zoom_color(prefs_info *prf, FILE *ignore) {saved_zoom_color = ss->zoom_color;}
static void zoom_color_func(prefs_info *prf, double r, double g, double b) {set_zoom_color(rgb_to_color(r, g, b));}


static void revert_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_zoom_color); 
  set_zoom_color(saved_zoom_color);
}


static void clear_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_zoom_color); 
  set_zoom_color(ss->orig_zoom_color);
}



/* ---------------- cursor-color ---------------- */

static color_t saved_cursor_color;
static void save_cursor_color(prefs_info *prf, FILE *ignore) {saved_cursor_color = ss->cursor_color;}

static void cursor_color_func(prefs_info *prf, double r, double g, double b)
{
  color_cursor(rgb_to_color(r, g, b));
  for_each_chan(update_graph);
}


static void revert_cursor_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_cursor_color); 
  color_cursor(saved_cursor_color);
}


static void clear_cursor_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_cursor_color); 
  color_cursor(ss->orig_cursor_color);
}



/* ---------------- data-color ---------------- */

static color_t saved_data_color;
static void save_data_color(prefs_info *prf, FILE *ignore) {saved_data_color = ss->data_color;}
static void data_color_func(prefs_info *prf, double r, double g, double b) {set_data_color(rgb_to_color(r, g, b));}


static void revert_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_data_color); 
  set_data_color(saved_data_color);
}


static void clear_data_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_data_color); 
  set_data_color(ss->orig_data_color);
}



/* ---------------- graph-color ---------------- */

static color_t saved_graph_color;
static void save_graph_color(prefs_info *prf, FILE *ignore) {saved_graph_color = ss->graph_color;}
static void graph_color_func(prefs_info *prf, double r, double g, double b) {set_graph_color(rgb_to_color(r, g, b));}


static void revert_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_graph_color); 
  set_graph_color(saved_graph_color);
}


static void clear_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_graph_color); 
  set_graph_color(ss->orig_graph_color);
}



/* ---------------- selected-data-color ---------------- */

static color_t saved_selected_data_color;
static void save_selected_data_color(prefs_info *prf, FILE *ignore) {saved_selected_data_color = ss->selected_data_color;}
static void selected_data_color_func(prefs_info *prf, double r, double g, double b) {set_selected_data_color(rgb_to_color(r, g, b));}


static void revert_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_data_color); 
  set_selected_data_color(saved_selected_data_color);
}


static void clear_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_selected_data_color); 
  set_selected_data_color(ss->orig_selected_data_color);
}



/* ---------------- selected-graph-color ---------------- */

static color_t saved_selected_graph_color;
static void save_selected_graph_color(prefs_info *prf, FILE *ignore) {saved_selected_graph_color = ss->selected_graph_color;}
static void selected_graph_color_func(prefs_info *prf, double r, double g, double b) {set_selected_graph_color(rgb_to_color(r, g, b));}


static void revert_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_graph_color); 
  set_selected_graph_color(saved_selected_graph_color);
}


static void clear_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_selected_graph_color); 
  set_selected_graph_color(ss->orig_selected_graph_color);
}



/* ---------------- selection-color ---------------- */

static void set_selection_color(color_t color)
{
  color_selection(color);
  for_each_chan(update_graph);
}


static color_t saved_selection_color;
static void save_selection_color(prefs_info *prf, FILE *ignore) {saved_selection_color = ss->selection_color;}
static void selection_color_func(prefs_info *prf, double r, double g, double b) {set_selection_color(rgb_to_color(r, g, b));}


static void revert_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selection_color); 
  set_selection_color(saved_selection_color);
}


static void clear_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_selection_color); 
  set_selection_color(ss->orig_selection_color);
}



/* ---------------- mark-color ---------------- */

static color_t saved_mark_color;
static void save_mark_color(prefs_info *prf, FILE *ignore) {saved_mark_color = ss->mark_color;}
static void mark_color_func(prefs_info *prf, double r, double g, double b) {color_marks(rgb_to_color(r, g, b));}


static void revert_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mark_color);
  color_marks(saved_mark_color);
}


static void clear_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_mark_color);
  color_marks(ss->orig_mark_color);
}



/* ---------------- mix-color (waveform) ---------------- */

static color_t saved_mix_color;
static void save_mix_color(prefs_info *prf, FILE *ignore) {saved_mix_color = ss->mix_color;}
static void mix_color_func(prefs_info *prf, double r, double g, double b) {color_mixes(rgb_to_color(r, g, b));}


static void revert_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mix_color);
  color_mixes(saved_mix_color);
}


static void clear_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_mix_color);
  color_mixes(ss->orig_mix_color);
}



/* ---------------- listener-color ---------------- */

static color_t saved_listener_color;
static void save_listener_color(prefs_info *prf, FILE *ignore) {saved_listener_color = ss->listener_color;}
static void listener_color_func(prefs_info *prf, double r, double g, double b) {color_listener(rgb_to_color(r, g, b));}


static void revert_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_color);
  color_listener(saved_listener_color);
}


static void clear_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_listener_color);
  color_listener(ss->orig_listener_color);
}



/* ---------------- listener-text-color ---------------- */

static color_t saved_listener_text_color;
static void save_listener_text_color(prefs_info *prf, FILE *ignore) {saved_listener_text_color = ss->listener_text_color;}
static void listener_text_color_func(prefs_info *prf, double r, double g, double b) {color_listener_text(rgb_to_color(r, g, b));}


static void revert_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_text_color);
  color_listener_text(saved_listener_text_color);
}


static void clear_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->orig_listener_text_color);
  color_listener_text(ss->orig_listener_text_color);
}



/* ---------------- axis-label-font ---------------- */

static char *rts_axis_label_font = NULL;

static TIMEOUT_TYPE axis_label_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, axis_label_font(ss));
  TIMEOUT_RESULT
}


static void save_axis_label_font(prefs_info *prf, FILE *ignore)
{
  if (rts_axis_label_font) free(rts_axis_label_font);
  rts_axis_label_font = mus_strdup(axis_label_font(ss));
}


static void reflect_axis_label_font(prefs_info *prf) {SET_TEXT(prf->text, axis_label_font(ss));}
static void revert_axis_label_font(prefs_info *prf) {set_axis_label_font(rts_axis_label_font);}
static void clear_axis_label_font(prefs_info *prf) {set_axis_label_font(ss->orig_axis_label_font);}


static void axis_label_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, axis_label_font(ss));
      return;
    }
  if (!(set_axis_label_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(axis_label_font_error_erase_func);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- axis-numbers-font ---------------- */

static char *rts_axis_numbers_font = NULL;

static TIMEOUT_TYPE axis_numbers_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, axis_numbers_font(ss));
  TIMEOUT_RESULT
}


static void save_axis_numbers_font(prefs_info *prf, FILE *ignore)
{
  if (rts_axis_numbers_font) free(rts_axis_numbers_font);
  rts_axis_numbers_font = mus_strdup(axis_numbers_font(ss));
}


static void reflect_axis_numbers_font(prefs_info *prf) {SET_TEXT(prf->text, axis_numbers_font(ss));}
static void revert_axis_numbers_font(prefs_info *prf) {set_axis_numbers_font(rts_axis_numbers_font);}
static void clear_axis_numbers_font(prefs_info *prf) {set_axis_numbers_font(ss->orig_axis_numbers_font);}


static void axis_numbers_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, axis_numbers_font(ss));
      return;
    }
  if (!(set_axis_numbers_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(axis_numbers_font_error_erase_func);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- peaks-font ---------------- */

static char *rts_peaks_font = NULL;

static TIMEOUT_TYPE peaks_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, peaks_font(ss));
  TIMEOUT_RESULT
}


static void save_peaks_font(prefs_info *prf, FILE *ignore)
{
  if (rts_peaks_font) free(rts_peaks_font);
  rts_peaks_font = mus_strdup(peaks_font(ss));
}


static void reflect_peaks_font(prefs_info *prf) {SET_TEXT(prf->text, peaks_font(ss));}
static void revert_peaks_font(prefs_info *prf) {set_peaks_font(rts_peaks_font);}
static void clear_peaks_font(prefs_info *prf) {set_peaks_font(ss->orig_peaks_font);}


static void peaks_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, peaks_font(ss));
      return;
    }
  if (!(set_peaks_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(peaks_font_error_erase_func);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- bold-peaks-font ---------------- */

static char *rts_bold_peaks_font = NULL;

static TIMEOUT_TYPE bold_peaks_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, bold_peaks_font(ss));
  TIMEOUT_RESULT
}


static void save_bold_peaks_font(prefs_info *prf, FILE *ignore)
{
  if (rts_bold_peaks_font) free(rts_bold_peaks_font);
  rts_bold_peaks_font = mus_strdup(bold_peaks_font(ss));
}


static void reflect_bold_peaks_font(prefs_info *prf) {SET_TEXT(prf->text, bold_peaks_font(ss));}
static void revert_bold_peaks_font(prefs_info *prf) {set_bold_peaks_font(rts_bold_peaks_font);}
static void clear_bold_peaks_font(prefs_info *prf) {set_bold_peaks_font(ss->orig_bold_peaks_font);}


static void bold_peaks_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, bold_peaks_font(ss));
      return;
    }
  if (!(set_bold_peaks_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(bold_peaks_font_error_erase_func);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- tiny-font ---------------- */

static char *rts_tiny_font = NULL;

static TIMEOUT_TYPE tiny_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, tiny_font(ss));
  TIMEOUT_RESULT
}


static void save_tiny_font(prefs_info *prf, FILE *ignore)
{
  if (rts_tiny_font) free(rts_tiny_font);
  rts_tiny_font = mus_strdup(tiny_font(ss));
}


static void reflect_tiny_font(prefs_info *prf) {SET_TEXT(prf->text, tiny_font(ss));}
static void revert_tiny_font(prefs_info *prf) {set_tiny_font(rts_tiny_font);}
static void clear_tiny_font(prefs_info *prf) {set_tiny_font(ss->orig_tiny_font);}


static void tiny_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, tiny_font(ss));
      return;
    }
  if (!(set_tiny_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(tiny_font_error_erase_func);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- listener-font ---------------- */

static char *rts_listener_font = NULL;

static TIMEOUT_TYPE listener_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, listener_font(ss));
  TIMEOUT_RESULT
}


static void save_listener_font(prefs_info *prf, FILE *ignore)
{
  if (rts_listener_font) free(rts_listener_font);
  rts_listener_font = mus_strdup(listener_font(ss));
}


static void reflect_listener_font(prefs_info *prf) {SET_TEXT(prf->text, listener_font(ss));}
static void revert_listener_font(prefs_info *prf) {set_listener_font(rts_listener_font);}
static void clear_listener_font(prefs_info *prf) {set_listener_font(ss->orig_listener_font);}


static void listener_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, listener_font(ss));
      return;
    }
  if (!(set_listener_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(listener_font_error_erase_func);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- save-state-file ---------------- */

static char *rts_save_state_file = NULL;

static void reflect_save_state_file(prefs_info *prf) 
{
  SET_TEXT(prf->text, save_state_file(ss));
}


static void revert_save_state_file(prefs_info *prf) 
{
  if (save_state_file(ss)) free(save_state_file(ss));
  in_set_save_state_file(mus_strdup(rts_save_state_file));
}


static void save_save_state_file(prefs_info *prf, FILE *ignore) 
{
  if (rts_save_state_file) free(rts_save_state_file);
  rts_save_state_file = mus_strdup(save_state_file(ss));
}


static void save_state_file_text(prefs_info *prf)
{
  char *str, *file = NULL;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str))) 
    file = mus_strdup(DEFAULT_SAVE_STATE_FILE);
  else file = mus_strdup(str);
  if (save_state_file(ss)) free(save_state_file(ss));
  in_set_save_state_file(file);
  if (str) {free_TEXT(str);}
}



/* ---------------- temp-dir ---------------- */

static char *rts_temp_dir = NULL;

static void reflect_temp_dir(prefs_info *prf) 
{
  SET_TEXT(prf->text, temp_dir(ss));
}


static void revert_temp_dir(prefs_info *prf) 
{
  if (temp_dir(ss)) free(temp_dir(ss));
  set_temp_dir(mus_strdup(rts_temp_dir));
}


static void save_temp_dir(prefs_info *prf, FILE *ignore) 
{
  if (rts_temp_dir) free(rts_temp_dir);
  rts_temp_dir = mus_strdup(temp_dir(ss));
}


static TIMEOUT_TYPE temp_dir_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, rts_temp_dir);
  TIMEOUT_RESULT
}


static void temp_dir_text(prefs_info *prf)
{
  char *str, *dir;

  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str))) 
    dir = MUS_DEFAULT_TEMP_DIR;
  else dir = str;

  if (local_access(dir))
    {
      if (temp_dir(ss)) free(temp_dir(ss));
      set_temp_dir(mus_strdup(dir));
    }
  else
    {
      SET_TEXT(prf->text, "can't access that directory");
      TIMEOUT(temp_dir_error_erase_func);
    }
#if USE_MOTIF
  if (str) XtFree(str);
#endif
}



/* ---------------- save-dir ---------------- */

static char *rts_save_dir = NULL;

static void reflect_save_dir(prefs_info *prf) 
{
  SET_TEXT(prf->text, save_dir(ss));
}


static void revert_save_dir(prefs_info *prf) 
{
  if (save_dir(ss)) free(save_dir(ss));
  set_save_dir(mus_strdup(rts_save_dir));
}


static void save_save_dir(prefs_info *prf, FILE *ignore) 
{
  if (rts_save_dir) free(rts_save_dir);
  rts_save_dir = mus_strdup(save_dir(ss));
}


static TIMEOUT_TYPE save_dir_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, rts_save_dir);
  TIMEOUT_RESULT
}


static void save_dir_text(prefs_info *prf)
{
  char *str, *dir;

  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str))) 
    dir = MUS_DEFAULT_SAVE_DIR;
  else dir = str;

  if (local_access(dir))
    {
      if (save_dir(ss)) free(save_dir(ss));
      set_save_dir(mus_strdup(dir));
    }
  else
    {
      SET_TEXT(prf->text, "can't access that directory");
      TIMEOUT(save_dir_error_erase_func);
    }
#if USE_MOTIF
  if (str) XtFree(str);
#endif
}



#if HAVE_LADSPA
/* ---------------- ladspa-dir ---------------- */

static char *rts_ladspa_dir = NULL;

static void reflect_ladspa_dir(prefs_info *prf)
{
  SET_TEXT(prf->text, ladspa_dir(ss));
}


static void revert_ladspa_dir(prefs_info *prf)
{
  if (ladspa_dir(ss)) free(ladspa_dir(ss));
  set_ladspa_dir(mus_strdup(rts_ladspa_dir));
}


static void save_ladspa_dir(prefs_info *prf, FILE *ignore)
{
  if (rts_ladspa_dir) free(rts_ladspa_dir);
  rts_ladspa_dir = mus_strdup(ladspa_dir(ss));
}


static void ladspa_dir_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (ladspa_dir(ss)) free(ladspa_dir(ss));
  if (str)
    {
      set_ladspa_dir(mus_strdup(str));
      free_TEXT(str);
    }
  else set_ladspa_dir(mus_strdup(DEFAULT_LADSPA_DIR));
}

static const char *help_ladspa_dir(prefs_info *prf)
{
  return("This sets the location of the ladspa libraries");
}
#endif


#if USE_MOTIF
/* ---------------- view-files directory ---------------- */

static char *rts_vf_directory = NULL;

static void reflect_view_files_directory(prefs_info *prf)
{
  SET_TEXT(prf->text, view_files_find_any_directory());
}


static void revert_view_files_directory(prefs_info *prf)
{
  if (rts_vf_directory)
    view_files_add_directory(NULL_WIDGET, (const char *)rts_vf_directory);
}


static void save_view_files_directory(prefs_info *prf, FILE *fd)
{
  if (rts_vf_directory) free(rts_vf_directory);
  rts_vf_directory = mus_strdup(view_files_find_any_directory());
  if (rts_vf_directory) 
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\")\n", S_add_directory_to_view_files_list, rts_vf_directory);
#endif

#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\")\n", to_proc_name(S_add_directory_to_view_files_list), rts_vf_directory);
#endif

#if HAVE_FORTH
      fprintf(fd, "\"%s\" %s drop\n", rts_vf_directory, S_add_directory_to_view_files_list);
#endif
    }
}


static void view_files_directory_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      view_files_add_directory(NULL_WIDGET, (const char *)str);
      free_TEXT(str);
    }
}
#endif



/* ---------------- html-program ---------------- */

static char *rts_html_program = NULL;

static void reflect_html_program(prefs_info *prf)
{
  SET_TEXT(prf->text, html_program(ss));
}


static void revert_html_program(prefs_info *prf)
{
  if (html_program(ss)) free(html_program(ss));
  set_html_program(mus_strdup(rts_html_program));
}


static void save_html_program(prefs_info *prf, FILE *ignore)
{
  if (rts_html_program) free(rts_html_program);
  rts_html_program = mus_strdup(html_program(ss));
}


static void html_program_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (html_program(ss)) free(html_program(ss));
  if (str)
    {
      set_html_program(mus_strdup(str));
      free_TEXT(str);
    }
  else set_html_program(mus_strdup(DEFAULT_HTML_PROGRAM));
}


/* ---------------- listener-prompt ---------------- */

static char *rts_listener_prompt = NULL;

static void reflect_listener_prompt(prefs_info *prf)
{
  SET_TEXT(prf->text, listener_prompt(ss));
}


static void revert_listener_prompt(prefs_info *prf)
{
  if (rts_listener_prompt)
    {
      if (listener_prompt(ss)) free(listener_prompt(ss));
      set_listener_prompt(mus_strdup(rts_listener_prompt));
    }
}


static void save_listener_prompt(prefs_info *prf, FILE *ignore)
{
  if (rts_listener_prompt) free(rts_listener_prompt);
  rts_listener_prompt = mus_strdup(listener_prompt(ss));
}


static void listener_prompt_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      if (listener_prompt(ss)) free(listener_prompt(ss));
      set_listener_prompt(mus_strdup(str));
      free_TEXT(str);
    }
}



/* ---------------- show-transform-peaks ---------------- */

static bool rts_show_transform_peaks = DEFAULT_SHOW_TRANSFORM_PEAKS;
static int rts_max_transform_peaks = DEFAULT_MAX_TRANSFORM_PEAKS;

static void reflect_transform_peaks(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, show_transform_peaks(ss));
  int_to_textfield(prf->text, max_transform_peaks(ss));
}


static void revert_transform_peaks(prefs_info *prf) 
{
  set_show_transform_peaks(rts_show_transform_peaks);
  set_max_transform_peaks(rts_max_transform_peaks);
}


static void save_transform_peaks(prefs_info *prf, FILE *ignore)
{
  rts_show_transform_peaks = show_transform_peaks(ss);
  rts_max_transform_peaks = max_transform_peaks(ss);
}


static void transform_peaks_toggle(prefs_info *prf)
{
  set_show_transform_peaks(GET_TOGGLE(prf->toggle));
}


static void max_peaks_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "max peaks");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	set_max_transform_peaks(value);
      free_TEXT(str);
    }
}



/* ---------------- show-mix-waveforms ---------------- */

static bool rts_show_mix_waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
static int rts_mix_waveform_height = DEFAULT_MIX_WAVEFORM_HEIGHT;

static void reflect_mix_waveforms(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, show_mix_waveforms(ss));
  int_to_textfield(prf->text, mix_waveform_height(ss));
}


static void revert_mix_waveforms(prefs_info *prf) 
{
  set_show_mix_waveforms(rts_show_mix_waveforms);
  set_mix_waveform_height(rts_mix_waveform_height);
}


static void save_mix_waveforms(prefs_info *prf, FILE *ignore)
{
  rts_show_mix_waveforms = show_mix_waveforms(ss);
  rts_mix_waveform_height = mix_waveform_height(ss);
}


static void show_mix_waveforms_toggle(prefs_info *prf)
{
  set_show_mix_waveforms(GET_TOGGLE(prf->toggle));
}


static void mix_waveform_height_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "mix waveform height");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	set_mix_waveform_height(value);
      free_TEXT(str);
    }
}



/* ---------------- selection-creates-region, max-regions ---------------- */

static bool rts_selection_creates_region = DEFAULT_SELECTION_CREATES_REGION;
static int rts_max_regions = DEFAULT_MAX_REGIONS;


static void reflect_selection_creates_region(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, selection_creates_region(ss));
  int_to_textfield(prf->text, max_regions(ss));
}


static void revert_selection_creates_region(prefs_info *prf) 
{
  set_selection_creates_region(rts_selection_creates_region);
  in_set_max_regions(rts_max_regions);
}


static void save_selection_creates_region(prefs_info *prf, FILE *ignore)
{
  rts_selection_creates_region = selection_creates_region(ss);
  rts_max_regions = max_regions(ss);
}


static void selection_creates_region_toggle(prefs_info *prf)
{
  set_selection_creates_region(GET_TOGGLE(prf->toggle));
}


static void max_regions_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "max regions");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	in_set_max_regions(value);
      free_TEXT(str);
    }
}



/* ---------------- sinc width ---------------- */

static int rts_sinc_width = DEFAULT_SINC_WIDTH;

static void reflect_sinc_width(prefs_info *prf) {int_to_textfield(prf->text, sinc_width(ss));}
static void revert_sinc_width(prefs_info *prf) {set_sinc_width(rts_sinc_width);}
static void save_sinc_width(prefs_info *prf, FILE *ignore) {rts_sinc_width = sinc_width(ss);}

static void sinc_width_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "sinc width");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	set_sinc_width(value);
      free_TEXT(str);
    }
}



/* ---------------- print-length ---------------- */

static int rts_print_length = DEFAULT_PRINT_LENGTH;

static void reflect_print_length(prefs_info *prf) {int_to_textfield(prf->text, print_length(ss));}
static void revert_print_length(prefs_info *prf) {set_print_length(rts_print_length); mus_vct_set_print_length(rts_print_length);}
static void save_print_length(prefs_info *prf, FILE *ignore) {rts_print_length = print_length(ss);}


static void print_length_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "print length");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	{
	  set_print_length(value);
	  mus_vct_set_print_length(value);
	  /* the clm array print length variable will be taken care of when ww.scm is loaded in the new context */
	}
      free_TEXT(str);
    }
}



/* ---------------- dac-size ---------------- */

static int rts_dac_size = DEFAULT_DAC_SIZE;

static void reflect_dac_size(prefs_info *prf) {int_to_textfield(prf->text, dac_size(ss));}
static void revert_dac_size(prefs_info *prf) {set_dac_size(rts_dac_size);}
static void save_dac_size(prefs_info *prf, FILE *ignore) {rts_dac_size = dac_size(ss);}


static void dac_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "dac size");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	set_dac_size(value);
      free_TEXT(str);
    }
}



/* ---------------- min-dB ---------------- */

static mus_float_t rts_min_dB = DEFAULT_MIN_DB;

static void reflect_min_dB(prefs_info *prf) {float_1_to_textfield(prf->text, min_dB(ss));}
static void revert_min_dB(prefs_info *prf) {set_min_dB(rts_min_dB);}
static void save_min_dB(prefs_info *prf, FILE *ignore) {rts_min_dB = min_dB(ss);}


static void min_dB_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      double value;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = (double)string_to_mus_float_t(str, -100000.0, "min dB");
      redirect_errors_to(NULL, NULL);

      if ((!(prf->got_error)) && (value < 0.0))
	set_min_db(value); /* snd-chn.c -- redisplays */
      free_TEXT(str);
    }
}



/* ---------------- fft-window-beta ---------------- */

static mus_float_t rts_fft_window_beta = DEFAULT_FFT_WINDOW_BETA;

static void reflect_fft_window_beta(prefs_info *prf)
{
  SET_SCALE(fft_window_beta(ss) / prf->scale_max);
  float_to_textfield(prf->text, fft_window_beta(ss));
}


static void revert_fft_window_beta(prefs_info *prf) {set_fft_window_beta(rts_fft_window_beta);}
static void save_fft_window_beta(prefs_info *prf, FILE *ignore) {rts_fft_window_beta = fft_window_beta(ss);}
static void fft_window_beta_scale_callback(prefs_info *prf) {set_fft_window_beta(GET_SCALE() * prf->scale_max);}


static void fft_window_beta_text_callback(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      double value;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = (double)string_to_mus_float_t(str, 0.0, "fft beta");
      redirect_errors_to(NULL, NULL);

      if ((!(prf->got_error)) && (value <= prf->scale_max))
	{
	  set_fft_window_beta(value);
	  SET_SCALE(value / prf->scale_max);
	}
      free_TEXT(str);
    }
}


/* ---------------- grid-density ---------------- */

static mus_float_t rts_grid_density = DEFAULT_GRID_DENSITY;

static void reflect_grid_density(prefs_info *prf)
{
  SET_SCALE(grid_density(ss) / prf->scale_max);
  float_to_textfield(prf->text, grid_density(ss));
}


static void revert_grid_density(prefs_info *prf) {set_grid_density(rts_grid_density);}
static void save_grid_density(prefs_info *prf, FILE *ignore) {rts_grid_density = grid_density(ss);}
static void grid_density_scale_callback(prefs_info *prf) {set_grid_density(GET_SCALE() * prf->scale_max);}


static void grid_density_text_callback(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      double value;

      redirect_errors_to(any_error_to_text, (void *)prf);
      value = (double)string_to_mus_float_t(str, 0.0, "grid density");
      redirect_errors_to(NULL, NULL);

      if ((!(prf->got_error)) && (value <= prf->scale_max))
	{
	  set_grid_density(value);
	  SET_SCALE(value / prf->scale_max);
	}
      free_TEXT(str);
    }
}



/* ---------------- sync style ---------------- */

static sync_style_t rts_sync_style = DEFAULT_SYNC_STYLE;
static void revert_sync_style(prefs_info *prf) {set_sync_style(rts_sync_style);}
static void clear_sync_style(prefs_info *prf) {set_sync_style(DEFAULT_SYNC_STYLE);}
static void save_sync_style(prefs_info *prf, FILE *ignore) {rts_sync_style = sync_style(ss);}

static const char *help_sync_style(prefs_info *prf)
{
  return("\
  Many operations can operate either on all channels at once,\n\
  or only on the currently selected channel.  If 'within each sound'\n\
  is set, the channels within a sound are tied together, but not\n\
  across sounds.");
}


static void reflect_sync_style(prefs_info *prf)
{
  rts_sync_style = sync_style(ss);
  SET_TOGGLE(prf->toggle, rts_sync_style == SYNC_BY_SOUND);
  SET_TOGGLE(prf->toggle2, rts_sync_style == SYNC_ALL);
}


static void sync1_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle))
    rts_sync_style = SYNC_BY_SOUND;
  else rts_sync_style = SYNC_NONE;
  SET_TOGGLE(prf->toggle2, false);
  set_sync_style(rts_sync_style); 
}


static void sync2_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle2))
    rts_sync_style = SYNC_ALL;
  else rts_sync_style = SYNC_NONE;
  SET_TOGGLE(prf->toggle, false);
  set_sync_style(rts_sync_style);
}



/* ---------------- mark-tag size ---------------- */

static int rts_mark_tag_width = DEFAULT_MARK_TAG_WIDTH, rts_mark_tag_height = DEFAULT_MARK_TAG_HEIGHT;

static void reflect_mark_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mark_tag_width(ss));
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
}


static void revert_mark_tag_size(prefs_info *prf)
{
  set_mark_tag_width(rts_mark_tag_width);
  set_mark_tag_height(rts_mark_tag_height);
}


static void save_mark_tag_size(prefs_info *prf, FILE *ignore)
{
  rts_mark_tag_width = mark_tag_width(ss);
  rts_mark_tag_height = mark_tag_height(ss);
}


static TIMEOUT_TYPE mark_tag_width_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mark_tag_width(ss));
  TIMEOUT_RESULT
}


static TIMEOUT_TYPE mark_tag_height_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
  TIMEOUT_RESULT
}


static void mark_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->text, "must be > 0");
  TIMEOUT(mark_tag_width_erase_func);
}


static void mark_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->rtxt, "must be > 0");
  TIMEOUT(mark_tag_height_erase_func);
}


static void mark_tag_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int width = 0;

      redirect_errors_to(mark_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mark tag width");
      redirect_errors_to(NULL, NULL);

      if (width > 0) set_mark_tag_width(width);
      free_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;

	  redirect_errors_to(mark_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mark tag height");
	  redirect_errors_to(NULL, NULL);

	  if (height > 0) set_mark_tag_height(height);
	  free_TEXT(str);
	}
    }
}


/* ---------------- mix-tag size ---------------- */

static int rts_mix_tag_width = DEFAULT_MIX_TAG_WIDTH, rts_mix_tag_height = DEFAULT_MIX_TAG_HEIGHT;

static void reflect_mix_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mix_tag_width(ss));
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
}


static void revert_mix_tag_size(prefs_info *prf)
{
  set_mix_tag_width(rts_mix_tag_width);
  set_mix_tag_height(rts_mix_tag_height);
}


static void save_mix_tag_size(prefs_info *prf, FILE *ignore)
{
  rts_mix_tag_width = mix_tag_width(ss);
  rts_mix_tag_height = mix_tag_height(ss);
}


static TIMEOUT_TYPE mix_tag_width_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mix_tag_width(ss));
  TIMEOUT_RESULT
}


static TIMEOUT_TYPE mix_tag_height_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
  TIMEOUT_RESULT
}


static void mix_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->text, "must be > 0");
  TIMEOUT(mix_tag_width_erase_func);
}


static void mix_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->rtxt, "must be > 0");
  TIMEOUT(mix_tag_height_erase_func);
}


static void mix_tag_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int width = 0;

      redirect_errors_to(mix_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mix tag width");
      redirect_errors_to(NULL, NULL);

      if (width > 0) set_mix_tag_width(width);
      free_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;

	  redirect_errors_to(mix_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mix tag height");
	  redirect_errors_to(NULL, NULL);

	  if (height > 0) set_mix_tag_height(height);
	  free_TEXT(str);
	}
    }
}


/* ---------------- start up size ---------------- */

static int rts_init_window_width = DEFAULT_INIT_WINDOW_WIDTH, rts_init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;

static void revert_init_window_size(prefs_info *prf)
{
  ss->init_window_width = rts_init_window_width;
  ss->init_window_height = rts_init_window_height;
}


static void reflect_init_window_size(prefs_info *prf)
{
  char *str;

  str = mus_format("%d", ss->init_window_width);
  SET_TEXT(prf->text, str);
  free(str);

  str = mus_format("%d", ss->init_window_height);
  SET_TEXT(prf->rtxt, str);
  free(str);
}


static void clear_init_window_size(prefs_info *prf)
{
  ss->init_window_width = DEFAULT_INIT_WINDOW_WIDTH;
  ss->init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;
}


static void save_init_window_size(prefs_info *prf, FILE *ignore)
{
  rts_init_window_width = ss->init_window_width;
  rts_init_window_height = ss->init_window_height;
}


static TIMEOUT_TYPE startup_width_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, ss->init_window_width);
  TIMEOUT_RESULT
}


static TIMEOUT_TYPE startup_height_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, ss->init_window_height);
  TIMEOUT_RESULT
}


static void startup_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->text, "must be > 0");
  TIMEOUT(startup_width_erase_func);
}


static void startup_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->rtxt, "must be > 0");
  TIMEOUT(startup_height_erase_func);
}


static void startup_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int width = 0;

      redirect_errors_to(startup_width_error, (void *)prf);
      width = string_to_int(str, 1, "startup width");
      redirect_errors_to(NULL, NULL);

      if (width > 0) ss->init_window_width = width;
      free_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;

	  redirect_errors_to(startup_height_error, (void *)prf);
	  height = string_to_int(str, 1, "startup height");
	  redirect_errors_to(NULL, NULL);

	  if (height > 0) ss->init_window_height = height;
	  free_TEXT(str);
	}
    }
}


/* ---------------- ask-about-unsaved-edits ---------------- */

static bool rts_unsaved_edits = DEFAULT_ASK_ABOUT_UNSAVED_EDITS;
static void revert_unsaved_edits(prefs_info *prf) {set_ask_about_unsaved_edits(rts_unsaved_edits);}
static void clear_unsaved_edits(prefs_info *prf) {set_ask_about_unsaved_edits(DEFAULT_ASK_ABOUT_UNSAVED_EDITS);}
static void save_unsaved_edits(prefs_info *prf, FILE *fd) {rts_unsaved_edits = ask_about_unsaved_edits(ss);}
static void reflect_unsaved_edits(prefs_info *prf) {SET_TOGGLE(prf->toggle, ask_about_unsaved_edits(ss));}
static void unsaved_edits_toggle(prefs_info *prf) {set_ask_about_unsaved_edits(GET_TOGGLE(prf->toggle));}

static const char *help_unsaved_edits(prefs_info *prf)
{
  return("\
  This option looks for unsaved edits when you \n\
  close a file, or exit Snd.  If it finds any, it \n\
  asks you whether you want to save them.");
}



/* ---------------- with-inset-graph ---------------- */

static bool rts_with_inset_graph = DEFAULT_WITH_INSET_GRAPH;
static void revert_with_inset_graph(prefs_info *prf) {set_with_inset_graph(rts_with_inset_graph);}
static void clear_with_inset_graph(prefs_info *prf) {set_with_inset_graph(DEFAULT_WITH_INSET_GRAPH);}
static void reflect_with_inset_graph(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_inset_graph(ss));}

static void with_inset_graph_toggle(prefs_info *prf) 
{
  set_with_inset_graph(GET_TOGGLE(prf->toggle));
  for_each_chan(update_graph);
}

static void save_with_inset_graph(prefs_info *prf, FILE *fd)
{
  rts_with_inset_graph = GET_TOGGLE(prf->toggle);
  set_with_inset_graph(rts_with_inset_graph);
}

static const char *help_inset_graph(prefs_info *prf)
{
  return("\
  This option displays a small graph of the entire sound \n\
  in the upper right corner of the screen with an indication \n\
  of where the current window is. If you click somewhere in the \n\
little graph, the cursor and main window are moved to that spot.");
}



/* ---------------- with-smpte-label ---------------- */

static bool rts_with_smpte_label = DEFAULT_WITH_SMPTE_LABEL;
static void revert_smpte(prefs_info *prf) {set_with_smpte_label(rts_with_smpte_label);}
static void clear_smpte(prefs_info *prf) {set_with_smpte_label(DEFAULT_WITH_SMPTE_LABEL);}
static void reflect_smpte(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_smpte_label(ss));}

static void smpte_toggle(prefs_info *prf) 
{
  set_with_smpte_label(GET_TOGGLE(prf->toggle));
  for_each_chan(update_graph);
}

static void save_smpte(prefs_info *prf, FILE *fd)
{
  rts_with_smpte_label = GET_TOGGLE(prf->toggle);
  set_with_smpte_label(rts_with_smpte_label);
}

static const char *help_smpte(prefs_info *prf)
{
  return("  This option displays the SMPTE data in the time domain graph.  ");
}



/* ---------------- with-pointer-focus ---------------- */

static bool rts_with_pointer_focus = DEFAULT_WITH_POINTER_FOCUS;

static void revert_with_pointer_focus(prefs_info *prf) {set_with_pointer_focus(rts_with_pointer_focus);}
static void clear_with_pointer_focus(prefs_info *prf) {set_with_pointer_focus(DEFAULT_WITH_POINTER_FOCUS);}
static void with_pointer_focus_toggle(prefs_info *prf) {set_with_pointer_focus(GET_TOGGLE(prf->toggle));}
static void reflect_with_pointer_focus(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_pointer_focus(ss));}

static void save_with_pointer_focus(prefs_info *prf, FILE *fd)
{
  rts_with_pointer_focus = GET_TOGGLE(prf->toggle);
  set_with_pointer_focus(rts_with_pointer_focus);
}


static const char *help_pointer_focus(prefs_info *prf)
{
  return("\
  If this option is set, when the mouse moves over a \n\
  text or graph widget, the widget is activated.  ");
}




/* ---------------- cursor-size ---------------- */

static int rts_cursor_size = DEFAULT_CURSOR_SIZE;

#define MIN_CURSOR_SIZE 1
#define MAX_CURSOR_SIZE 500

static void revert_cursor_size(prefs_info *prf) {set_cursor_size(rts_cursor_size);}
static void save_cursor_size(prefs_info *prf, FILE *ignore) {rts_cursor_size = cursor_size(ss);}


static void reflect_cursor_size(prefs_info *prf)
{
  int_to_textfield(prf->text, cursor_size(ss));
  SET_SENSITIVE(prf->arrow_up, cursor_size(ss) < MAX_CURSOR_SIZE);
  SET_SENSITIVE(prf->arrow_down, cursor_size(ss) > MIN_CURSOR_SIZE);
}


static void cursor_size_up(prefs_info *prf)
{
  int size;
  size = cursor_size(ss) + 1;
  if (size >= MAX_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_up, false);
  if (size > MIN_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_down, true);
  set_cursor_size(size);
  int_to_textfield(prf->text, cursor_size(ss));
}


static void cursor_size_down(prefs_info *prf)
{
  int size;
  size = cursor_size(ss) - 1;
  if (size <= MIN_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_down, false);
  if (size < MAX_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_up, true);
  set_cursor_size(size);
  int_to_textfield(prf->text, cursor_size(ss));
}


static void cursor_size_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int size;
      prf->got_error = false;

      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "cursor size"); 
      redirect_errors_to(NULL, NULL);

      free_TEXT(str);
      if (!(prf->got_error))
	{
	  if (size >= MIN_CURSOR_SIZE)
	    {
	      if (size <= MAX_CURSOR_SIZE)
		set_cursor_size(size);
	      else va_post_prefs_error("%s > %d?", prf, str, MAX_CURSOR_SIZE);
	    }
	  else va_post_prefs_error("%s < %d?", prf, str, MIN_CURSOR_SIZE);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", prf);
}



/* ---------------- dot-size ---------------- */

static int rts_dot_size = DEFAULT_DOT_SIZE;

#define MIN_DOT_SIZE 0
#define MAX_DOT_SIZE 100

static void revert_dot_size(prefs_info *prf) {set_dot_size(rts_dot_size);}
static void save_dot_size(prefs_info *prf, FILE *ignore) {rts_dot_size = dot_size(ss);}


static void reflect_dot_size(prefs_info *prf)
{
  int_to_textfield(prf->text, dot_size(ss));
  SET_SENSITIVE(prf->arrow_up, dot_size(ss) < MAX_DOT_SIZE);
  SET_SENSITIVE(prf->arrow_down, dot_size(ss) > MIN_DOT_SIZE);
}


static void dot_size_up(prefs_info *prf)
{
  int size;
  size = dot_size(ss) + 1;
  if (size >= MAX_DOT_SIZE) SET_SENSITIVE(prf->arrow_up, false);
  if (size > MIN_DOT_SIZE) SET_SENSITIVE(prf->arrow_down, true);
  set_dot_size(size);
  int_to_textfield(prf->text, dot_size(ss));
}


static void dot_size_down(prefs_info *prf)
{
  int size;
  size = dot_size(ss) - 1;
  if (size <= MIN_DOT_SIZE) SET_SENSITIVE(prf->arrow_down, false);
  if (size < MAX_DOT_SIZE) SET_SENSITIVE(prf->arrow_up, true);
  set_dot_size(size);
  int_to_textfield(prf->text, dot_size(ss));
}


static void dot_size_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int size;
      prf->got_error = false;

      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "dot size"); 
      redirect_errors_to(NULL, NULL);

      free_TEXT(str);
      if (!(prf->got_error))
	{
	  if (size >= MIN_DOT_SIZE)
	    {
	      if (size <= MAX_DOT_SIZE)
		set_dot_size(size);
	      else va_post_prefs_error("%s > %d?", prf, str, MAX_DOT_SIZE);
	    }
	  else va_post_prefs_error("%s < %d?", prf, str, MIN_DOT_SIZE);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", prf);
}


/* ---------------- fft-size ---------------- */

static mus_long_t rts_fft_size = DEFAULT_TRANSFORM_SIZE;

#define MAX_TRANSFORM_SIZE 1073741824
#define MIN_TRANSFORM_SIZE 2

static void revert_fft_size(prefs_info *prf) {set_transform_size(rts_fft_size);}
static void save_fft_size(prefs_info *prf, FILE *ignore) {rts_fft_size = transform_size(ss);}


static void reflect_fft_size(prefs_info *prf)
{
  mus_long_t_to_textfield(prf->text, transform_size(ss));
  SET_SENSITIVE(prf->arrow_up, transform_size(ss) < MAX_TRANSFORM_SIZE);
  SET_SENSITIVE(prf->arrow_down, transform_size(ss) > MIN_TRANSFORM_SIZE);
}


static void fft_size_up(prefs_info *prf)
{
  mus_long_t size;
  size = transform_size(ss) * 2;
  if (size >= MAX_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_up, false);
  if (size > MIN_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_down, true);
  set_transform_size(size);
  mus_long_t_to_textfield(prf->text, transform_size(ss));
}


static void fft_size_down(prefs_info *prf)
{
  mus_long_t size;
  size = transform_size(ss) / 2;
  if (size <= MIN_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_down, false);
  if (size < MAX_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_up, true);
  set_transform_size(size);
  mus_long_t_to_textfield(prf->text, transform_size(ss));
}


static void fft_size_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      mus_long_t size;
      prf->got_error = false;

      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      size = string_to_mus_long_t(str, MIN_TRANSFORM_SIZE, "size"); 
      redirect_errors_to(NULL, NULL);

      free_TEXT(str);
      if (!(prf->got_error))
	{
	  if (is_power_of_2(size))
	    {
	      if (size <= MAX_TRANSFORM_SIZE)
		set_transform_size(size);
	      else va_post_prefs_error("%s > %d?", prf, str, MAX_TRANSFORM_SIZE);
	    }
	  else post_prefs_error("size must be a power of 2", prf);
	}
      else prf->got_error = false;
    }
}


/* ---------------- with-tracking-cursor ---------------- */

static tracking_cursor_t rts_with_tracking_cursor = DEFAULT_WITH_TRACKING_CURSOR;
static mus_float_t rts_cursor_update_interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
static int rts_cursor_location_offset = DEFAULT_CURSOR_LOCATION_OFFSET;


static void revert_with_tracking_cursor(prefs_info *prf)
{
  set_with_tracking_cursor(ss, rts_with_tracking_cursor);
  set_cursor_update_interval(rts_cursor_update_interval);
  set_cursor_location_offset(rts_cursor_location_offset);
}


static void save_with_tracking_cursor(prefs_info *prf, FILE *ignore)
{
  rts_with_tracking_cursor = with_tracking_cursor(ss);
  rts_cursor_update_interval = cursor_update_interval(ss);
  rts_cursor_location_offset = cursor_location_offset(ss);
}


static void reflect_with_tracking_cursor(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, (with_tracking_cursor(ss) != DONT_TRACK));
  int_to_textfield(prf->rtxt, cursor_location_offset(ss));
  float_to_textfield(prf->text, cursor_update_interval(ss));
}


static void with_tracking_cursor_toggle(prefs_info *prf)
{
  set_with_tracking_cursor(ss, (GET_TOGGLE(prf->toggle)) ? TRACK_AND_RETURN : DONT_TRACK);
}


static void cursor_location_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      double interval;

      redirect_errors_to(any_error_to_text, (void *)prf);
      interval = (double)string_to_mus_float_t(str, 0.0, "cursor offset");
      redirect_errors_to(NULL, NULL);

      if (!(prf->got_error))
	set_cursor_update_interval(interval);
      free_TEXT(str);

      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int loc;

	  redirect_errors_to(any_error_to_text, (void *)prf);
	  loc = string_to_int(str, 0, "cursor offset");
	  redirect_errors_to(NULL, NULL);

	  if (!(prf->got_error))
	    set_cursor_location_offset(loc);
	  free_TEXT(str);
	}
    }
}


/* ---------------- channel-style ---------------- */

static channel_style_t rts_channel_style = DEFAULT_CHANNEL_STYLE;

static const char *channel_styles[NUM_CHANNEL_STYLES] = {"separate ", "combined ", "superimposed"};


static void reflect_channel_style(prefs_info *prf) {set_radio_button(prf, (int)channel_style(ss));}
static void revert_channel_style(prefs_info *prf) {set_channel_style(rts_channel_style);}
static void save_channel_style(prefs_info *prf, FILE *ignore) {rts_channel_style = channel_style(ss);}


static void channel_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_channel_style((channel_style_t)which_radio_button(prf));
}


/* ---------------- cursor-style ---------------- */

static cursor_style_t rts_cursor_style = DEFAULT_CURSOR_STYLE;

#define NUM_CURSOR_STYLES 2
static const char *cursor_styles[NUM_CURSOR_STYLES] = {"cross ", "line"};


static void reflect_cursor_style(prefs_info *prf) {set_radio_button(prf, (int)cursor_style(ss));}
static void revert_cursor_style(prefs_info *prf) {set_cursor_style(rts_cursor_style);}
static void save_cursor_style(prefs_info *prf, FILE *ignore) {rts_cursor_style = cursor_style(ss);}


static void cursor_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_cursor_style((cursor_style_t)which_radio_button(prf));
}


/* ---------------- tracking-cursor-style ---------------- */

static cursor_style_t rts_tracking_cursor_style = DEFAULT_CURSOR_STYLE;


static void reflect_tracking_cursor_style(prefs_info *prf) {set_radio_button(prf, (int)tracking_cursor_style(ss));}
static void revert_tracking_cursor_style(prefs_info *prf) {in_set_tracking_cursor_style(rts_tracking_cursor_style);}
static void save_tracking_cursor_style(prefs_info *prf, FILE *ignore) {rts_tracking_cursor_style = tracking_cursor_style(ss);}


static void tracking_cursor_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_tracking_cursor_style((cursor_style_t)which_radio_button(prf));
}


/* ---------------- transform-graph-type ---------------- */

static graph_type_t rts_transform_graph_type = DEFAULT_TRANSFORM_GRAPH_TYPE;

#define NUM_TRANSFORM_GRAPH_TYPES 3
static const char *transform_graph_types[NUM_TRANSFORM_GRAPH_TYPES] = {"normal ", "sonogram ", "spectrogram"};


static void reflect_transform_graph_type(prefs_info *prf) {set_radio_button(prf, (int)transform_graph_type(ss));}
static void revert_transform_graph_type(prefs_info *prf) {set_transform_graph_type(rts_transform_graph_type);}
static void save_transform_graph_type(prefs_info *prf, FILE *ignore) {rts_transform_graph_type = transform_graph_type(ss);}


static void transform_graph_type_choice(prefs_info *prf) 
{
  if (GET_TOGGLE(prf->radio_button))
    set_transform_graph_type((graph_type_t)which_radio_button(prf));
}


/* ---------------- transform-normalization ---------------- */

static fft_normalize_t rts_transform_normalization = DEFAULT_TRANSFORM_NORMALIZATION;

static const char *transform_normalizations[NUM_TRANSFORM_NORMALIZATIONS] = {"none ", "by channel ", "by sound ", "global"};


static void reflect_transform_normalization(prefs_info *prf) {set_radio_button(prf, (int)transform_normalization(ss));}
static void revert_transform_normalization(prefs_info *prf) {set_transform_normalization(rts_transform_normalization);}
static void save_transform_normalization(prefs_info *prf, FILE *ignore) {rts_transform_normalization = transform_normalization(ss);}


static void transform_normalization_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_transform_normalization((fft_normalize_t)which_radio_button(prf));
}


/* ---------------- graph-style ---------------- */

static graph_style_t rts_graph_style = DEFAULT_GRAPH_STYLE;

static const char *graph_styles[NUM_GRAPH_STYLES] = {"line ", "dot ", "filled ", "dot+line ", "lollipop"};


static void reflect_graph_style(prefs_info *prf) {set_radio_button(prf, (int)graph_style(ss));}
static void revert_graph_style(prefs_info *prf) {set_graph_style(rts_graph_style);}
static void save_graph_style(prefs_info *prf, FILE *ignore) {rts_graph_style = graph_style(ss);}


static void graph_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_graph_style((graph_style_t)which_radio_button(prf));
}


/* ---------------- speed control ---------------- */

static speed_style_t rts_speed_control_style = DEFAULT_SPEED_CONTROL_STYLE;
static int rts_speed_control_tones = DEFAULT_SPEED_CONTROL_TONES;

#define MIN_SPEED_CONTROL_SEMITONES 1
static const char *speed_control_styles[NUM_SPEED_CONTROL_STYLES] = {"double ", "ratio ", "semitones:"};

static void show_speed_control_semitones(prefs_info *prf)
{
  int_to_textfield(prf->text, speed_control_tones(ss));
  SET_SENSITIVE(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
}


static void speed_control_up(prefs_info *prf)
{
  in_set_speed_control_tones(ss, speed_control_tones(ss) + 1);
  show_speed_control_semitones(prf);
}


static void speed_control_down(prefs_info *prf)
{
  in_set_speed_control_tones(ss, speed_control_tones(ss) - 1);
  show_speed_control_semitones(prf);
}


static void speed_control_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int tones;
      prf->got_error = false;
      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      tones = string_to_int(str, MIN_SPEED_CONTROL_SEMITONES, "semitones");
      redirect_errors_to(NULL, NULL);
      free_TEXT(str);
      if (!(prf->got_error))
	{
	  in_set_speed_control_tones(ss, tones);
	  SET_SENSITIVE(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
	}
      else prf->got_error = false;
    }
}


static void reflect_speed_control(prefs_info *prf)
{
  set_radio_button(prf, (int)speed_control_style(ss));
  show_speed_control_semitones(prf);
}


static void speed_control_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_speed_control_style(ss, (speed_style_t)which_radio_button(prf));
}


static void revert_speed_control(prefs_info *prf) 
{
  in_set_speed_control_style(ss, rts_speed_control_style);
  in_set_speed_control_tones(ss, rts_speed_control_tones);
}


static void save_speed_control(prefs_info *prf, FILE *ignore) 
{
  rts_speed_control_style = speed_control_style(ss);
  rts_speed_control_tones = speed_control_tones(ss);
}


/* ---------------- default-output-chans etc ---------------- */

static int rts_default_output_chans = DEFAULT_OUTPUT_CHANS;
static int rts_default_output_srate = DEFAULT_OUTPUT_SRATE;
static mus_sample_t rts_default_output_sample_type = DEFAULT_OUTPUT_SAMPLE_TYPE;
static mus_header_t rts_default_output_header_type = DEFAULT_OUTPUT_HEADER_TYPE;

static prefs_info *output_sample_type_prf = NULL, *output_header_type_prf = NULL;

#define NUM_OUTPUT_CHAN_CHOICES 4
static const char *output_chan_choices[NUM_OUTPUT_CHAN_CHOICES] = {"1 ", "2 ", "4 ", "8"};
static int output_chans[NUM_OUTPUT_CHAN_CHOICES] = {1, 2, 4, 8};

#define NUM_OUTPUT_SRATE_CHOICES 4
static const char *output_srate_choices[NUM_OUTPUT_SRATE_CHOICES] = {"8000 ", "22050 ", "44100 ", "48000"};
static int output_srates[NUM_OUTPUT_SRATE_CHOICES] = {8000, 22050, 44100, 48000};

#define NUM_OUTPUT_HEADER_TYPE_CHOICES 7
static const char *output_header_type_choices[NUM_OUTPUT_HEADER_TYPE_CHOICES] = {"aifc ", "wave ", "au ", "rf64 ", "nist ", "aiff ", "caff"};
static mus_header_t output_header_types[NUM_OUTPUT_HEADER_TYPE_CHOICES] = {MUS_AIFC, MUS_RIFF, MUS_NEXT, MUS_RF64, MUS_NIST, MUS_AIFF, MUS_CAFF};

#define NUM_OUTPUT_SAMPLE_TYPE_CHOICES 4
static const char *output_sample_type_choices[NUM_OUTPUT_SAMPLE_TYPE_CHOICES] = {"short ", "int ", "double ", "double"};
static mus_sample_t output_sample_types[NUM_OUTPUT_SAMPLE_TYPE_CHOICES] = {MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE};


static mus_sample_t header_to_sample_type(mus_header_t ht, mus_sample_t samp_type)
{
  /* nist -> short or int (lb)
     aiff -> short or int (b)
     aifc -> any (b)
     next -> any (b)
     wave -> any (l)
     caff -> any
  */
  switch (ht)
    {
    case MUS_NEXT: case MUS_AIFC:
      switch (samp_type)
	{
	case MUS_LSHORT:  return(MUS_BSHORT); break;
	case MUS_LINT:    return(MUS_BINT); break;
	case MUS_LFLOAT:  return(MUS_BFLOAT); break;
	case MUS_LDOUBLE: return(MUS_BDOUBLE); break;
	default: break;
	}
      break;

    case MUS_AIFF:
      switch (samp_type)
	{
	case MUS_LSHORT: return(MUS_BSHORT); break;
	case MUS_LINT:   return(MUS_BINT); break;
	case MUS_LFLOAT: case MUS_LDOUBLE: case MUS_BFLOAT: case MUS_BDOUBLE: return(MUS_BINT); break;
	default: break;
	}
      break;

    case MUS_NIST:
      switch (samp_type)
	{
	case MUS_LFLOAT: case MUS_LDOUBLE: return(MUS_LINT); break;
	case MUS_BFLOAT: case MUS_BDOUBLE: return(MUS_BINT); break;
	default: break;
	}
      break;

    case MUS_RF64:
    case MUS_RIFF:
      switch (samp_type)
	{
	case MUS_BSHORT:  return(MUS_LSHORT); break;
	case MUS_BINT:    return(MUS_LINT); break;
	case MUS_BFLOAT:  return(MUS_LFLOAT); break;
	case MUS_BDOUBLE: return(MUS_LDOUBLE); break;
	default: break;
	}
      break;

    case MUS_CAFF:
      if (samp_type == MUS_LINT)
	return(MUS_LINTN);
      if (samp_type == MUS_BINT)
	return(MUS_BINTN);
      break;

    default: break;
    }
  return(samp_type);
}


static int chans_to_button(int chans)
{
  int i;
  for (i = 0; i < NUM_OUTPUT_CHAN_CHOICES; i++)
    if (chans == output_chans[i])
      return(i);
  return(0);
}


static void reflect_default_output_chans(prefs_info *prf) {set_radio_button(prf, chans_to_button(default_output_chans(ss)));}
static void revert_default_output_chans(prefs_info *prf) {set_default_output_chans(rts_default_output_chans);}
static void save_default_output_chans(prefs_info *prf, FILE *ignore) {rts_default_output_chans = default_output_chans(ss);}


static void default_output_chans_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_default_output_chans(output_chans[which_radio_button(prf)]);
}


static int srate_to_button(int srate)
{
  int i;
  for (i = 0; i < NUM_OUTPUT_SRATE_CHOICES; i++)
    if (output_srates[i] == srate)
      return(i);
  return(0);
}


static void reflect_default_output_srate(prefs_info *prf) {set_radio_button(prf, srate_to_button(default_output_srate(ss)));}
static void revert_default_output_srate(prefs_info *prf) {set_default_output_srate(rts_default_output_srate);}
static void save_default_output_srate(prefs_info *prf, FILE *ignore) {rts_default_output_srate = default_output_srate(ss);}


static void default_output_srate_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_default_output_srate(output_srates[which_radio_button(prf)]);
}


static void reflect_default_output_header_type(prefs_info *prf)
{
  int which = -1;
  switch (default_output_header_type(ss))
    {
    case MUS_AIFC: which = 0; break;
    case MUS_AIFF: which = 5; break;
    case MUS_RIFF: which = 1; break;
    case MUS_RF64: which = 3; break;
    case MUS_NEXT: which = 2; break;
    case MUS_NIST: which = 4; break;
    case MUS_CAFF: which = 6; break;
    default: break;
    }
  set_radio_button(prf, which);
}


static void revert_default_output_header_type(prefs_info *prf) 
{
  set_default_output_header_type(rts_default_output_header_type);
}


static void save_default_output_header_type(prefs_info *prf, FILE *ignore) 
{
  rts_default_output_header_type = default_output_header_type(ss);
}


static void reflect_default_output_sample_type(prefs_info *prf)
{
  int which = -1;
  switch (default_output_sample_type(ss))
    {
    case MUS_LINT: case MUS_BINT:       which = 1; break;
    case MUS_LSHORT: case MUS_BSHORT:   which = 0; break;
    case MUS_LFLOAT: case MUS_BFLOAT:   which = 2; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: which = 3; break;
    default: break;
    }
  set_radio_button(prf, which);
}


static void default_output_header_type_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    {
      set_default_output_header_type(output_header_types[which_radio_button(prf)]);
      set_default_output_sample_type(header_to_sample_type(default_output_header_type(ss), default_output_sample_type(ss)));
      reflect_default_output_sample_type(output_sample_type_prf);
    }
}

static void revert_default_output_sample_type(prefs_info *prf) 
{
  set_default_output_sample_type(rts_default_output_sample_type);
}


static void save_default_output_sample_type(prefs_info *prf, FILE *ignore) 
{
  rts_default_output_sample_type = default_output_sample_type(ss);
}


static void default_output_sample_type_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    {
      int which;
      which = which_radio_button(prf);
      set_default_output_sample_type(output_sample_types[which]);

      switch (default_output_sample_type(ss))
	{
	case MUS_LSHORT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      set_default_output_sample_type(MUS_BSHORT); 
	      break;
	    default: break;
	    }
	  break;
	  
	case MUS_LINT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      set_default_output_sample_type(MUS_BINT); 
	      break;
	    default: break;
	    }
	  break;

	case MUS_LFLOAT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      set_default_output_sample_type(MUS_BFLOAT); 
	      break;
	    case MUS_AIFF:
	      set_default_output_header_type(MUS_AIFC);
	      set_default_output_sample_type(MUS_BFLOAT); 
	      break;
	    case MUS_NIST: 
	      set_default_output_header_type(MUS_RIFF); 
	      break;
	    default: break;
	    }
	  break;

	case MUS_LDOUBLE:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      set_default_output_sample_type(MUS_BDOUBLE); 
	      break;
	    case MUS_AIFF:
	      set_default_output_header_type(MUS_AIFC);
	      set_default_output_sample_type(MUS_BDOUBLE); 
	      break;
	    case MUS_NIST: 
	      set_default_output_header_type(MUS_RIFF); 
	      break;
	    default: break;
	    }
	  break;

	default: break;
	}
      reflect_default_output_header_type(output_header_type_prf);
    }
}


/* ---------------- raw sound defaults ---------------- */

static int rts_raw_chans = DEFAULT_OUTPUT_CHANS;
static int rts_raw_srate = DEFAULT_OUTPUT_SRATE;
static mus_sample_t rts_raw_sample_type = DEFAULT_OUTPUT_SAMPLE_TYPE;

static void revert_raw_chans(prefs_info *prf)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  chans = rts_raw_chans;
  mus_header_set_raw_defaults(srate, chans, samp_type);
}


static void save_raw_chans(prefs_info *prf, FILE *ignore)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  rts_raw_chans = chans;
}


static void reflect_raw_chans(prefs_info *prf)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  int_to_textfield(prf->text, chans);
}


static void raw_chans_choice(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      int srate = 0, chans = 0;
      mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
      mus_header_raw_defaults(&srate, &chans, &samp_type);
      redirect_errors_to(any_error_to_text, (void *)prf);
      chans = string_to_int(str, 1, "raw chans");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	mus_header_set_raw_defaults(srate, chans, samp_type);
      free_TEXT(str);
    }
}


static void revert_raw_srate(prefs_info *prf)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  srate = rts_raw_srate;
  mus_header_set_raw_defaults(srate, chans, samp_type);
}


static void save_raw_srate(prefs_info *prf, FILE *ignore)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  rts_raw_srate = srate;
}


static void reflect_raw_srate(prefs_info *prf)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  int_to_textfield(prf->text, srate);
}


static void raw_srate_choice(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      int srate = 0, chans = 0;
      mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
      mus_header_raw_defaults(&srate, &chans, &samp_type);
      redirect_errors_to(any_error_to_text, (void *)prf);
      srate = string_to_int(str, 1, "raw srate");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	mus_header_set_raw_defaults(srate, chans, samp_type);
      free_TEXT(str);
    }
}


static char *raw_sample_type_to_string(mus_sample_t samp_type)
{
  /* the "mus-" prefix carries no information in this context, so strip it off */
  const char *name;
  name = mus_sample_type_to_string(samp_type);
  if (name)
    {
      char *rtn;
      int i, j, len;
      len = strlen(name);
      rtn = (char *)calloc(len, sizeof(char));
      for (i = 0, j = 4; j < len; i++, j++)
	{
	  if (name[j] == '-')
	    {
	      rtn[i] = 'u';
	      return(rtn);
	    }
	  else rtn[i] = name[j];
	}
      return(rtn);
    }
  return(mus_strdup("unknown"));
}


static void revert_raw_sample_type(prefs_info *prf)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  samp_type = rts_raw_sample_type;
  mus_header_set_raw_defaults(srate, chans, samp_type);
}


static void save_raw_sample_type(prefs_info *prf, FILE *ignore)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  rts_raw_sample_type = samp_type;
}


static void reflect_raw_sample_type(prefs_info *prf)
{
  int srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  char *str;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  str = raw_sample_type_to_string(samp_type);
  SET_TEXT(prf->text, str);
  free(str);
}


static char **raw_sample_type_choices = NULL;

static void raw_sample_type_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      int i, srate = 0, chans = 0;
      mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
      mus_header_raw_defaults(&srate, &chans, &samp_type);
      for (i = 0; i < MUS_NUM_SAMPLES - 1; i++)
	if (STRCMP(raw_sample_type_choices[i], str) == 0)
	  {
	    mus_header_set_raw_defaults(srate, chans, (mus_sample_t)(i + 1)); /* skipping MUS_UNKNOWN_SAMPLE = 0 */
	    reflect_raw_sample_type(prf);
	    free_TEXT(str);
	    return;
	  }
    }
}


#if USE_MOTIF
static void raw_sample_type_from_menu(prefs_info *prf, char *value)
{
  int i, srate = 0, chans = 0;
  mus_sample_t samp_type = MUS_UNKNOWN_SAMPLE;
  mus_header_raw_defaults(&srate, &chans, &samp_type);
  for (i = 0; i < MUS_NUM_SAMPLES - 1; i++)
    if (STRCMP(raw_sample_type_choices[i], value) == 0)
      {
	mus_header_set_raw_defaults(srate, chans, (mus_sample_t)(i + 1));
	SET_TEXT(prf->text, raw_sample_type_choices[i]);
	return;
      }
}
#endif


/* ---------------- with-toolbar ---------------- */

static bool rts_with_toolbar = DEFAULT_WITH_TOOLBAR;

static const char *help_with_toolbar(prefs_info *prf)
{
  return("  If this is set, a toolbar is displayed.  ");
}

static void revert_with_toolbar(prefs_info *prf) {set_with_toolbar_and_display(rts_with_toolbar);}
static void clear_with_toolbar(prefs_info *prf) {set_with_toolbar_and_display(DEFAULT_WITH_TOOLBAR);}
static void reflect_with_toolbar(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_toolbar(ss));}
static void save_with_toolbar(prefs_info *prf, FILE *fd) {rts_with_toolbar = with_toolbar(ss);}
static void toggle_with_toolbar(prefs_info *prf) {set_with_toolbar_and_display(GET_TOGGLE(prf->toggle));}



/* ---------------- with-tooltips ---------------- */

static bool rts_with_tooltips = DEFAULT_WITH_TOOLTIPS;

static const char *help_with_tooltips(prefs_info *prf)
{
  return("  If this is set, tooltips may be displayed.  ");
}

static void revert_with_tooltips(prefs_info *prf) {set_with_tooltips(rts_with_tooltips);}
static void clear_with_tooltips(prefs_info *prf) {set_with_tooltips(DEFAULT_WITH_TOOLTIPS);}
static void reflect_with_tooltips(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_tooltips(ss));}
static void save_with_tooltips(prefs_info *prf, FILE *fd) {rts_with_tooltips = with_tooltips(ss);}
static void toggle_with_tooltips(prefs_info *prf) {set_with_tooltips(GET_TOGGLE(prf->toggle));}



#if USE_GTK
/* ---------------- with-menu-icons ---------------- */

static bool rts_with_menu_icons = DEFAULT_WITH_MENU_ICONS;

static const char *help_with_menu_icons(prefs_info *prf)
{
  return("  If this is set, some menus include icons.  ");
}

static void revert_with_menu_icons(prefs_info *prf) {set_with_menu_icons(rts_with_menu_icons);}
static void clear_with_menu_icons(prefs_info *prf) {set_with_menu_icons(DEFAULT_WITH_MENU_ICONS);}
static void reflect_with_menu_icons(prefs_info *prf) {SET_TOGGLE(prf->toggle, with_menu_icons(ss));}
static void save_with_menu_icons(prefs_info *prf, FILE *fd) {rts_with_menu_icons = with_menu_icons(ss);}
static void toggle_with_menu_icons(prefs_info *prf) {set_with_menu_icons(GET_TOGGLE(prf->toggle));}
#endif



/* ---------------- remember-sound-state ---------------- */

static bool rts_remember_sound_state = DEFAULT_REMEMBER_SOUND_STATE;

static const char *help_remember_sound_state(prefs_info *prf)
{
  return("\
  This option causes Snd to save most of a sound's display \n\
  state when it is closed, and if that same sound is later re-opened, \n\
  Snd restores the previous state. This only takes effect upon restarting Snd.");
}

static void revert_remember_sound_state(prefs_info *prf) {set_remember_sound_state(rts_remember_sound_state);}
static void clear_remember_sound_state(prefs_info *prf) {set_remember_sound_state(DEFAULT_REMEMBER_SOUND_STATE);}
static void reflect_remember_sound_state(prefs_info *prf) {SET_TOGGLE(prf->toggle, remember_sound_state(ss));}
static void save_remember_sound_state(prefs_info *prf, FILE *fd) {rts_remember_sound_state = remember_sound_state(ss);}
static void toggle_remember_sound_state(prefs_info *prf) {set_remember_sound_state(GET_TOGGLE(prf->toggle));}



/* ---------------- show-axes ---------------- */

static show_axes_t rts_show_axes = DEFAULT_SHOW_AXES;

static const char *show_axes_choices[NUM_SHOW_AXES] = {"none", "X and Y", "just X", "X and Y unlabelled", "just X unlabelled", "bare X"};

static void reflect_show_axes(prefs_info *prf) {SET_TEXT(prf->text, (char *)show_axes_choices[(int)show_axes(ss)]);}
static void revert_show_axes(prefs_info *prf) {set_show_axes(rts_show_axes);}
static void clear_show_axes(prefs_info *prf) {set_show_axes(DEFAULT_SHOW_AXES);}
static void save_show_axes(prefs_info *prf, FILE *ignore) {rts_show_axes = show_axes(ss);}


#if USE_MOTIF
static void show_axes_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_SHOW_AXES; i++)
    if (mus_strcmp(value, show_axes_choices[i]))
      {
	set_show_axes((show_axes_t)i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif


static void show_axes_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      free_TEXT(str);
      if (mus_strlen(trimmed_str) > 0)
	{
	  int i, curpos = -1;
	  for (i = 0; i < NUM_SHOW_AXES; i++)
	    if (STRCMP(trimmed_str, show_axes_choices[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    set_show_axes((show_axes_t)curpos);
	  else post_prefs_error("unknown axis choice", prf);
	}
      else post_prefs_error("need an axis choice", prf);
      free(trimmed_str);
    }
  else post_prefs_error("need an axis choice", prf);
}



/* ---------------- x-axis-style ---------------- */

static x_axis_style_t rts_x_axis_style = DEFAULT_X_AXIS_STYLE;

static const char *x_axis_styles[NUM_X_AXIS_STYLES] = {"seconds", "samples", "% of total", "beats", "measures", "clock"};

static void reflect_x_axis_style(prefs_info *prf) {SET_TEXT(prf->text, (char *)x_axis_styles[(int)x_axis_style(ss)]);}
static void revert_x_axis_style(prefs_info *prf) {set_x_axis_style(rts_x_axis_style);}
static void clear_x_axis_style(prefs_info *prf) {set_x_axis_style(DEFAULT_X_AXIS_STYLE);}
static void save_x_axis_style(prefs_info *prf, FILE *ignore) {rts_x_axis_style = x_axis_style(ss);}


#if USE_MOTIF
static void x_axis_style_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_X_AXIS_STYLES; i++)
    if (mus_strcmp(value, x_axis_styles[i]))
      {
	set_x_axis_style((x_axis_style_t)i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif


static void x_axis_style_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      free_TEXT(str);
      if (mus_strlen(trimmed_str) > 0)
	{
	  int i, curpos = -1;
	  for (i = 0; i < NUM_X_AXIS_STYLES; i++)
	    if (STRCMP(trimmed_str, x_axis_styles[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    set_x_axis_style((x_axis_style_t)curpos);
	  else post_prefs_error("unknown axis style", prf);
	}
      else post_prefs_error("need an axis style", prf);
      free(trimmed_str);
    }
  else post_prefs_error("need an axis style", prf);
}



/* ---------------- transform-type ---------------- */

static int rts_transform_type = DEFAULT_TRANSFORM_TYPE;

static const char *transform_types[NUM_BUILTIN_TRANSFORM_TYPES] = {"Fourier", "Wavelet", "Walsh", "Autocorrelate", "Cepstrum", "Haar"};

static list_completer_info *transform_type_completer_info = NULL;


static void reflect_transform_type(prefs_info *prf)
{
  SET_TEXT(prf->text, (char *)transform_types[mus_iclamp(0, transform_type(ss), NUM_BUILTIN_TRANSFORM_TYPES - 1)]); 
}


static void revert_transform_type(prefs_info *prf) {set_transform_type(rts_transform_type);}
static void clear_transform_type(prefs_info *prf) {set_transform_type(DEFAULT_TRANSFORM_TYPE);}
static void save_transform_type(prefs_info *prf, FILE *ignore) {rts_transform_type = transform_type(ss);}


static char *transform_type_completer(widget_t w, const char *text, void *data)
{
  if (!transform_type_completer_info)
    {
      transform_type_completer_info = (list_completer_info *)calloc(1, sizeof(list_completer_info));
      transform_type_completer_info->exact_match = false;
      transform_type_completer_info->values = (char **)transform_types;
      transform_type_completer_info->num_values = NUM_BUILTIN_TRANSFORM_TYPES;
      transform_type_completer_info->values_size = NUM_BUILTIN_TRANSFORM_TYPES;
    }
  return(list_completer(w, text, (void *)transform_type_completer_info));
}


#if USE_MOTIF
static void transform_type_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_BUILTIN_TRANSFORM_TYPES; i++)
    if (mus_strcmp(value, transform_types[i]))
      {
	set_transform_type(i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif


static void transform_type_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      free_TEXT(str);
      if (mus_strlen(trimmed_str) > 0)
	{
	  int i, curpos = -1;
	  for (i = 0; i < NUM_BUILTIN_TRANSFORM_TYPES; i++)
	    if (STRCMP(trimmed_str, transform_types[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    set_transform_type(curpos);
	  else post_prefs_error("unknown tranform", prf);
	}
      else post_prefs_error("no transform?", prf);
      free(trimmed_str);
    }
  else post_prefs_error("no transform?", prf);
}



/* -------- fft-window -------- */

static mus_fft_window_t rts_fft_window = DEFAULT_FFT_WINDOW;

static void reflect_fft_window(prefs_info *prf) {SET_TEXT(prf->text, (char *)mus_fft_window_name(fft_window(ss)));}
static void revert_fft_window(prefs_info *prf) {set_fft_window(rts_fft_window);}
static void clear_fft_window(prefs_info *prf) {set_fft_window(DEFAULT_FFT_WINDOW);}
static void save_fft_window(prefs_info *prf, FILE *ignore) {rts_fft_window = fft_window(ss);}

static list_completer_info *fft_window_completer_info = NULL;


static char *fft_window_completer(widget_t w, const char *text, void *data)
{
  if (!fft_window_completer_info)
    {
      fft_window_completer_info = (list_completer_info *)calloc(1, sizeof(list_completer_info));
      fft_window_completer_info->exact_match = false;
      fft_window_completer_info->values = (char **)mus_fft_window_names();
      fft_window_completer_info->num_values = MUS_NUM_FFT_WINDOWS;
      fft_window_completer_info->values_size = MUS_NUM_FFT_WINDOWS;
    }
  return(list_completer(w, text, (void *)fft_window_completer_info));
}


#if USE_MOTIF
static void fft_window_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < MUS_NUM_FFT_WINDOWS; i++)
    if (mus_strcmp(value, mus_fft_window_name((mus_fft_window_t)i)))
      {
	set_fft_window((mus_fft_window_t)i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif


static void fft_window_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      free_TEXT(str);
      if (mus_strlen(trimmed_str) > 0)
	{
	  int i, curpos = -1;
	  for (i = 0; i < MUS_NUM_FFT_WINDOWS; i++)
	    if (STRCMP(trimmed_str, mus_fft_window_name((mus_fft_window_t)i)) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    set_fft_window((mus_fft_window_t)curpos);
	  else post_prefs_error("unknown window", prf);
	}
      else post_prefs_error("no window?", prf);
      free(trimmed_str);
    }
  else post_prefs_error("no window?", prf);
}



/* ---------------- colormap ---------------- */

static int rts_colormap = DEFAULT_COLOR_MAP;

static char *colormap_completer(widget_t w, const char *text, void *data)
{
  list_completer_info *compinfo;
  char **cmaps;
  int i, len;
  char *result;
  len = num_colormaps();
  cmaps = (char **)calloc(len, sizeof(char *));
  for (i = 0; i < len; i++)
    cmaps[i] = colormap_name(i);
  compinfo = (list_completer_info *)calloc(1, sizeof(list_completer_info));
  compinfo->exact_match = false;
  compinfo->values = (char **)mus_fft_window_names();
  compinfo->num_values = len;
  compinfo->values_size = len;
  result = list_completer(w, text, (void *)compinfo);
  free(cmaps);
  return(result);
}


static void reflect_colormap(prefs_info *prf) {SET_TEXT(prf->text, colormap_name(color_map(ss)));}
static void clear_colormap(prefs_info *prf) {set_color_map(DEFAULT_COLOR_MAP);}
static void save_colormap(prefs_info *prf, FILE *ignore) {rts_colormap = color_map(ss);}


static void revert_colormap(prefs_info *prf) 
{
  if (!(is_colormap(rts_colormap))) rts_colormap = DEFAULT_COLOR_MAP;
  set_color_map(rts_colormap);
}


static void colormap_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      free_TEXT(str);
      if (mus_strlen(trimmed_str) > 0)
	{
	  int i, len, curpos = -1;
	  len = num_colormaps();
	  for (i = 0; i < len; i++)
	    if ((colormap_name(i)) &&
		(STRCMP(trimmed_str, colormap_name(i)) == 0))
	      {
		curpos = i;
		break;
	      }
	  if (is_colormap(curpos))
	    set_color_map(curpos);
	  else post_prefs_error("unknown colormap", prf);
	}
      else post_prefs_error("no colormap?", prf);
      free(trimmed_str);
    }
  else post_prefs_error("no colormap?", prf);
}


#if USE_MOTIF
static void colormap_from_menu(prefs_info *prf, char *value)
{
  int i, len;
  len = num_colormaps();
  for (i = 0; i < len; i++)
    if (mus_strcmp(value, colormap_name(i)))
      {
	set_color_map(i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif



/* ---------------- peak-envs ---------------- */

static bool include_peak_envs = false, rts_peak_envs = false;
static char *include_peak_env_directory = NULL, *rts_peak_env_directory = NULL;

static const char *help_peak_envs(prefs_info *prf)
{
  return("\
  When a very large file is first opened, Snd scans \n\
  all the data to build up an overall representation of \n\
  the sound.  If you like to view the entire sound upon \n\
  opening it, you can speed up the process a lot by saving \n\
  this initial representation.  The data is called a 'peak-env' file \n\
  and it resides in the 'peak-env-dir'.");
}


static bool find_peak_envs(void) 
{
  return(peak_env_dir(ss));
}


static void clear_peak_envs(prefs_info *prf)
{
  if (include_peak_env_directory) free(include_peak_env_directory); 
  include_peak_env_directory = NULL;
  include_peak_envs = false;
}


static void revert_peak_envs(prefs_info *prf)
{
  if (include_peak_env_directory) free(include_peak_env_directory); 
  include_peak_env_directory = mus_strdup(rts_peak_env_directory);
  include_peak_envs = rts_peak_envs;
}


static void save_peak_envs(prefs_info *prf, FILE *fd)
{
  rts_peak_envs = GET_TOGGLE(prf->toggle);
  if (rts_peak_env_directory) free(rts_peak_env_directory);
  rts_peak_env_directory = mus_strdup(include_peak_env_directory);
}


static void reflect_peak_envs(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, include_peak_envs);
  SET_TEXT(prf->text, include_peak_env_directory);
}


static void peak_envs_toggle(prefs_info *prf)
{
  include_peak_envs = GET_TOGGLE(prf->toggle);
}


static void peak_envs_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      if (include_peak_env_directory) {free(include_peak_env_directory); include_peak_env_directory = NULL;}
      include_peak_env_directory = mus_strdup(str);
      free_TEXT(str);
    }
}



/* ---------------- load path ---------------- */

static char *rts_load_path = NULL;

static const char *help_load_path(prefs_info *prf)
{
  static char *hlp = NULL;
  char *temp = NULL;
  if (hlp) free(hlp);

  hlp = mus_format("Much of Snd's functionality is loaded as needed \n\
from the Scheme, Ruby, or Forth files found in the Snd tarball.  \n\
You can run Snd without these files, but there's no reason to!  \n\
Just add the directory containing them to the load path \n\
variable %s.  " Xen_language " searches these directories \n\
for any *." Xen_file_extension " files that it can't find elsewhere.  \n\
The current load path list is: \n\n%s\n",
#if HAVE_RUBY
		   ", $LOAD_PATH",
#else
#if HAVE_FORTH || HAVE_SCHEME
		   ", *load-path*",
#else
		   "",
#endif
#endif
		   temp = (char *)Xen_object_to_C_string(Xen_load_path));
#if HAVE_SCHEME
  if (temp) free(temp);
#endif
  return(hlp);
}


static char *find_sources(void) /* returns directory name where it finds extensions.* */
{
  char *file = NULL;
  #define BASE_FILE "extensions." Xen_file_extension

#if HAVE_SCHEME
  /* mimic Forth code below -- get *load-path* value and run through it */
  {
      int i, len, base_len;
      Xen load_path;
      load_path = Xen_load_path;
      len = Xen_list_length(load_path);
      base_len = strlen(BASE_FILE);
      for (i = 0; i < len; i++)
	{
	  char *fname;
	  const char *path;
	  int flen;
	  path = Xen_string_to_C_string(Xen_list_ref(load_path, i));
	  flen = base_len + 32 + strlen(path);
	  fname = (char *)calloc(flen, sizeof(char));
	  snprintf(fname, flen, "%s/%s", path, BASE_FILE);
	  if (mus_file_probe(fname)) 
	    {
	      file = fname;
	      break;
	    }
	}
    }
#endif

#if HAVE_RUBY
  {
    Xen xfile;
    xfile = rb_find_file(C_string_to_Xen_string(BASE_FILE));
    if (Xen_is_string(xfile))
      file = mus_expand_filename(Xen_string_to_C_string(xfile));
  }
#endif

#if HAVE_FORTH
    {
      /* taken from fth/src/misc.c -- fth_find_file looks for an already-loaded file */
      int i, len, base_len;
      Xen load_path;
      load_path = Xen_load_path;
      len = fth_array_length(load_path);
      base_len = strlen(BASE_FILE);
      for (i = 0; i < len; i++)
	{
	  char *fname, *path;
	  int flen;
	  path = fth_string_ref(fth_array_ref(load_path, i));
	  flen = base_len + 32 + strlen(path);
	  fname = (char *)calloc(flen, sizeof(char));
	  snprintf(fname, flen, "%s/%s", path, BASE_FILE);
	  if (mus_file_probe(fname)) 
	    {
	      file = fname;
	      break;
	    }
	}
    }
#endif

  if (file)
    {
      int len, exts_len;
      len = mus_strlen(file);
      exts_len = strlen(BASE_FILE);
      if (len > exts_len)
	file[len - exts_len - 1] = '\0';
      return(file);
    }
  return(NULL);
}


static void clear_load_path(prefs_info *prf)
{
  char *str;
  str = find_sources();
  SET_TEXT(prf->text, str);
  if (str) 
    {
      black_text(prf);
      free(str);
    }
  else 
    {
      red_text(prf);
    }
}


static void revert_load_path(prefs_info *prf)
{
  SET_TEXT(prf->text, rts_load_path);
  if (rts_load_path) 
    black_text(prf);
  else 
    {
      red_text(prf);
    }
}


static void reflect_load_path(prefs_info *prf) {}

static void load_path_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    return;
  if (local_access(str))
    {
      black_text(prf);
      if (include_load_path) free(include_load_path);
      include_load_path = mus_strdup(str);
      Xen_add_to_load_path(include_load_path);
    }
  if (str) {free_TEXT(str);}
}



/* ---------------- initial bounds ---------------- */

static mus_float_t rts_initial_beg = DEFAULT_INITIAL_BEG, rts_initial_dur = DEFAULT_INITIAL_DUR;
static bool rts_full_duration = DEFAULT_SHOW_FULL_DURATION;

static const char *help_initial_bounds(prefs_info *prf)
{
  return("\
  Normally Snd displays just the first 0.1 seconds of a \n\
  sound in its initial graph. This option sets either new \n\
  bounds for that display, or directs Snd to display the entire sound.");
}


static char *initial_bounds_to_string(void)
{
  return(mus_format("%.2f : %.2f", initial_beg(ss), initial_dur(ss)));
}


static void save_initial_bounds(prefs_info *prf, FILE *fd)
{
  rts_full_duration = GET_TOGGLE(prf->toggle);
}


static void reflect_initial_bounds(prefs_info *prf)
{
  /* text has beg : dur, toggle true if full dur */
  char *str;
  str = initial_bounds_to_string();
  SET_TEXT(prf->text, str);
  free(str);
  SET_TOGGLE(prf->toggle, show_full_duration(ss));
}


static void revert_initial_bounds(prefs_info *prf)
{
  set_initial_beg(rts_initial_beg);
  set_initial_dur(rts_initial_dur);
  set_show_full_duration(rts_full_duration);
}


static void clear_initial_bounds(prefs_info *prf)
{
  set_initial_beg(DEFAULT_INITIAL_BEG);
  set_initial_dur(DEFAULT_INITIAL_DUR);
  set_show_full_duration(DEFAULT_SHOW_FULL_DURATION);
}


static void initial_bounds_toggle(prefs_info *prf)
{
  set_show_full_duration(GET_TOGGLE(prf->toggle));
}


static void initial_bounds_text(prefs_info *prf)
{
  double beg = 0.0, dur = 0.1;
  char *str;

  str = GET_TEXT(prf->text);
  sscanf(str, "%lf : %lf", &beg, &dur);
  set_initial_beg(beg);
  set_initial_dur(dur);
  free_TEXT(str);
}



/* ---------------- keys ---------------- */

static void reflect_key(prefs_info *prf, const char *key_name)
{
  key_info *ki;
  ki = find_prefs_key(key_name);
  SET_TOGGLE(prf->toggle, ki->c);
  SET_TOGGLE(prf->toggle2, ki->m);
  SET_TOGGLE(prf->toggle3, ki->x);
  SET_TEXT(prf->text, ki->key);
  free(ki);
}


static void save_key(prefs_info *prf, FILE *fd, char *(*binder)(char *key, bool c, bool m, bool x))
{
  char *key;
  key = GET_TEXT(prf->text);
  if ((key) && (*key))
    {
      char *expr;
      expr = (*binder)(key, 
		       GET_TOGGLE(prf->toggle),
		       GET_TOGGLE(prf->toggle2),
		       GET_TOGGLE(prf->toggle3));
      fprintf(fd, "%s", expr);
      free(expr);
      free_TEXT(key);
    }
}


static void key_bind(prefs_info *prf, char *(*binder)(char *key, bool c, bool m, bool x))
{
  char *key;
  bool ctrl, meta, cx;
  key = GET_TEXT(prf->text);
  ctrl = GET_TOGGLE(prf->toggle);
  meta = GET_TOGGLE(prf->toggle2);
  cx = GET_TOGGLE(prf->toggle3);
  if ((key) && (*key))
    {
      char *expr;
      expr = (*binder)(key, ctrl, meta, cx);
      free_TEXT(key);
      Xen_eval_C_string(expr);
      free(expr);
    }
}


static void clear_key(prefs_info *prf, const char *name)
{
  key_info *ki;
  ki = find_prefs_key(name);
  if (ki)
    {
      if (ki->key)
	{
	  int state = 0;
	  if (ki->c) state |= 4;
	  if (ki->m) state |= 8;
#if USE_MOTIF
	  set_keymap_entry((int)XStringToKeysym(ki->key), state, 0, Xen_undefined, ki->x, name, name);
#else
	  set_keymap_entry((int)gdk_keyval_from_name(ki->key), state, 0, Xen_undefined, ki->x, name, name);
#endif
	}
      free(ki);
    }
}



/* -------- key: play all chans from cursor -------- */

static const char *help_play_from_cursor(prefs_info *prf)
{
  return("  This option binds a key to play the entire sound. ");
}


static char *make_pfc(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () (set! (pausing) #f) (play (cursor))) %s \"play sound from cursor\" \"play-from-cursor\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  set_pausing(false)\n  play(cursor())\n  end, %s, \"play sound from cursor\", \"play-from-cursor\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f set-pausing drop  #f #f #f cursor play drop ; %s \"play sound from cursor\" \"play-from-cursor\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_play_from_cursor(prefs_info *prf)
{
  reflect_key(prf, "play-from-cursor");
}


static void save_pfc(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_pfc);
}


static void bind_play_from_cursor(prefs_info *prf)
{
  key_bind(prf, make_pfc);
}


static void clear_play_from_cursor(prefs_info *prf) 
{
  clear_key(prf, "play-from-cursor");
}



/* -------- key: show all of sound -------- */

static const char *help_show_all(prefs_info *prf)
{
  return("\
  This option binds a key to show all of the current sound \n\
  in the current time domain window, equivalent to moving the \n\
  'zoom' slider all the way to the right.");
}


static char *make_show_all(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () \
                                       (let ((old-sync (sync))) \
                                         (set! (sync) (+ (sync-max) 1)) \
                                         (set! (x-bounds) (list 0.0 (/ (framples) (srate)))) \
                                         (set! (sync) old-sync))) %s \"show entire sound\" \"show-all\")\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n\
                                        old_sync = sync()\n\
                                        sync(1 + sync_max())\n\
                                        set_x_bounds([0.0, framples() / srate()])\n\
                                        set_sync(old_sync)\n\
                                        end, %s, \"show entire sound\", \"show-all\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f sync sync-max 1+ #f set-sync drop '( 0.0 #f #f #f framples #f srate f/ ) #f #f set-x-bounds drop #f set-sync ; %s \"show entire sound\" \"show-all\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_show_all(prefs_info *prf)
{
  reflect_key(prf, "show-all");
}


static void save_show_all(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_show_all);
}


static void bind_show_all(prefs_info *prf)
{
  key_bind(prf, make_show_all);
}


static void clear_show_all(prefs_info *prf) 
{
  clear_key(prf, "show-all");
}



/* -------- key: select all of sound -------- */

static const char *help_select_all(prefs_info *prf)
{
  return("\
  This option binds a key to select all of the current sound.  \n\
  The 'Select all' Edit menu item follows the 'sync' buttons when \n\
  deciding which channels to select.");
}


static char *make_select_all(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () \
                                       (let ((old-sync (sync))) \
                                         (set! (sync) (+ (sync-max) 1)) \
                                         (select-all) \
                                         (set! (sync) old-sync))) %s \"select entire sound\" \"select-all\")\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n\
                                        old_sync = sync()\n\
                                        sync(1 + sync_max())\n\
                                        select_all()\n\
                                        set_sync(old_sync)\n\
                                        end, %s, \"select entire sound\", \"select-all\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f sync sync-max 1+ #f set-sync drop #f #f select-all drop #f set-sync ; %s \"select entire sound\" \"select-all\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_select_all(prefs_info *prf)
{
  reflect_key(prf, "select-all");
}


static void save_select_all(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_select_all);
}


static void bind_select_all(prefs_info *prf)
{
  key_bind(prf, make_select_all);
}


static void clear_select_all(prefs_info *prf) 
{
  clear_key(prf, "select-all");
}



/* -------- key: undo all edits -------- */

static const char *help_revert(prefs_info *prf)
{
  return("\
  This option binds a key to undo any edits in the current sound, \n\
  equivalent to the File:Revert menu item.");
}


static char *make_revert(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d revert-sound %s \"undo all edits\" \"revert-sound\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  revert_sound()\n  end, %s, \"undo all edits\", \"revert-sound\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f revert-sound ; %s \"undo all edits\" \"revert-sound\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_revert(prefs_info *prf)
{
  reflect_key(prf, "revert-sound");
}


static void save_revert(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_revert);
}


static void bind_revert(prefs_info *prf)
{
  key_bind(prf, make_revert);
}


static void clear_revert_sound(prefs_info *prf) 
{
  clear_key(prf, "revert-sound");
}



/* -------- key: exit -------- */

static const char *help_exit(prefs_info *prf)
{
  return("\
  This option binds a key to exit from Snd, \n\
  equivalent to the File:Exit menu item.");
}


static char *make_exit(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d exit %s \"exit\" \"exit\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  exit()\n  end, %s, \"exit\", \"exit\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> 0 snd-exit ; %s \"exit\" \"exit\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_exit(prefs_info *prf)
{
  reflect_key(prf, "exit");
}


static void save_exit(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_exit);
}


static void bind_exit(prefs_info *prf)
{
  key_bind(prf, make_exit);
}


static void clear_exit(prefs_info *prf) 
{
  clear_key(prf, "exit");
}



/* -------- key: goto maxamp -------- */

static const char *help_goto_maxamp(prefs_info *prf)
{
  return("\
  This option binds a key to move the view (and cursor) to the \n\
  position of the current channel's maximum sample.");
}


static char *make_goto_maxamp(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () (set! (cursor) (maxamp-position))) %s \"goto maxamp\" \"goto-maxamp\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  set_cursor(maxamp_position())\n  end, %s, \"goto maxamp\", \"goto-maxamp\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f #f #f maxamp-position #f #f #f set-cursor ; %s \"goto maxamp\" \"goto-maxamp\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_goto_maxamp(prefs_info *prf)
{
  reflect_key(prf, "goto-maxamp");
}


static void save_goto_maxamp(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_goto_maxamp);
}


static void bind_goto_maxamp(prefs_info *prf)
{
  key_bind(prf, make_goto_maxamp);
}


static void clear_goto_maxamp(prefs_info *prf) 
{
  clear_key(prf, "goto-maxamp");
}



/* -------- key: show selection -------- */

static const char *help_show_selection(prefs_info *prf)
{
  return("\
  This option binds a key to cause the current selection to \n\
  fill the time domain graph.");
}


static char *make_show_selection(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d show-selection %s \"show selection\" \"show-selection\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif

#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  show_selection()\n  end, %s, \"show selection\", \"show-selection\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif

#if HAVE_FORTH
  return(mus_format("%s %d ' show-selection %s \"show selection\" \"show-selection\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}


static void reflect_show_selection(prefs_info *prf)
{
  reflect_key(prf, "show-selection");
}


static void save_show_selection(prefs_info *prf, FILE *fd)
{
  save_key(prf, fd, make_show_selection);
}


static void bind_show_selection(prefs_info *prf)
{
  key_bind(prf, make_show_selection);
}


static void clear_show_selection(prefs_info *prf) 
{
  clear_key(prf, "show-selection");
}

static const char *help_show_controls(prefs_info *prf) 
{
  return("\
  The control panel is the set of playback controls  \n\
  under the sound graphs.  It is normally hidden.  ");
}

static const char *help_selection_creates_region(prefs_info *prf) 
{
  return("\
  This determines whether a region is created whenever  \n\
  you make a selection.  ");
}

static const char *help_just_sounds(prefs_info *prf) 
{
  return("\
  The various file dialogs can restrict the files  \n\
  displayed to just sound files.  This sets the default\n\
  value of the 'just sounds' button in those dialogs. ");
}

static const char *help_temp_dir(prefs_info *prf) 
{
  return("\
  Snd sometimes needs a place to write a temporary  \n\
  file.  This is the directory to use.   ");
}

static const char *help_save_dir(prefs_info *prf) 
{
  return("\
  When you choose the Options: Save state item,  \n\
  the saved settings are placed in a file in this directory.   ");
}

static const char *help_save_state_file(prefs_info *prf) 
{
  return("\
  When you choose Options: Save state, the state  \n\
  is saved in this file, in the save directory.  ");
}

static const char *help_html_program(prefs_info *prf) 
{
  return("\
  The help dialog has links to the Snd documentation,  \n\
  and this option gives the name of the HTML viewer to use.  ");
}

static const char *help_default_output_chans(prefs_info *prf) 
{
  return("\
  This sets the default number of output channels  \n\
  when a new sound is started.   ");
}

static const char *help_default_output_srate(prefs_info *prf) 
{
  return("\
  This sets the default sampling rate when a new  \n\
  sound is opened.  ");
}

static const char *help_default_output_header_type(prefs_info *prf) 
{
  return("\
  This sets the default output header type (AIFC, etc)  \n\
  when a new sound is opened.  ");
}

static const char *help_default_output_sample_type(prefs_info *prf) 
{
  return("\
  This sets the default sample type (big-endian float, etc)  \n\
  when a new sound is opened.  ");
}

static const char *help_with_verbose_cursor(prefs_info *prf) 
{
  return("\
  If this is set, the cursor's position and the underlying  \n\
  sample's value are displayed in the status area as the cursor moves.  ");
}

static const char *help_with_tracking_cursor(prefs_info *prf) 
{
  return("\
  If this is set, the cursor always tries to show where the playback \n\
  is in the sound as it is played.  It is only an approximation.  ");
}

static const char *help_cursor_size(prefs_info *prf) 
{
  return("  This sets the cursor size in pixels.  ");
}

static const char *help_cursor_style(prefs_info *prf) 
{
  return("  This sets the cursor shape.  ");
}

static const char *help_tracking_cursor_style(prefs_info *prf) 
{
  return("  This sets the tracking cursor shape (the 'tracking cursor'\n\
  tries to show where you are in the sound while it is begin played).  ");
}

static const char *help_cursor_color(prefs_info *prf) 
{
  return("  This is the cursor color.  ");
}

static const char *help_basic_color(prefs_info *prf) 
{
  return("  This is the main background color used throughout Snd.  ");
}

static const char *help_highlight_color(prefs_info *prf) 
{
  return("  This color is used for highlighted items.  ");
}

static const char *help_position_color(prefs_info *prf) 
{
  return("  This is the color of the position-related sliders.  ");
}

static const char *help_zoom_color(prefs_info *prf) 
{
  return("  This is the color of the zoom-related sliders.  ");
}

static const char *help_graph_style(prefs_info *prf) 
{
  return("\
  This sets how sound data is displayed.  Normally samples  \n\
  are joined by lines, but you can also display them as isolated \n\
  dots, or as filled polygons, or as dots-on-stilts, as in the DSP  \n\
  textbooks.   ");
}

static const char *help_dot_size(prefs_info *prf) 
{
  return("\
  If the graph style uses dots, this sets the dot size.  ");
}

static const char *help_channel_style(prefs_info *prf) 
{
  return("\
  When a sound has more than one channel, this chooses how \n\
  to lay them out: in separate windows, combined in one window  \n\
  or superimposed on each other in a single graph.  ");
}

static const char *help_graphs_horizontal(prefs_info *prf) 
{
  return("\
  This chooses whether sounds are layed out horizontally or vertically.  ");
}

static const char *help_show_y_zero(prefs_info *prf) 
{
  return("\
  If this is set, each channel graph includes a line at y=0.  ");
}

static const char *help_show_grid(prefs_info *prf) 
{
  return("\
  If this option is set, each channel graph has a grid, sort \n\
  of like engineering graph paper.  ");
}

static const char *help_grid_density(prefs_info *prf) 
{
  return("\
  If grids are in use, this sets how close the lines are.  ");
}

static const char *help_show_axes(prefs_info *prf) 
{
  return("  This chooses which axes to display.  ");
}

static const char *help_x_axis_style(prefs_info *prf) 
{
  return("  This sets the x axis labelling.  ");
}

static const char *help_data_color(prefs_info *prf) 
{
  return("  This is the waveform color in unselected graphs.  ");
}

static const char *help_graph_color(prefs_info *prf) 
{
  return("  This is the background color in unselected graphs.  ");
}

static const char *help_selected_data_color(prefs_info *prf) 
{
  return("  This is the waveform color in a selected graph.  ");
}

static const char *help_selected_graph_color(prefs_info *prf) 
{
  return("  This is the background color in a selected graph.  ");
}

static const char *help_selection_color(prefs_info *prf) 
{
  return("  This is the color used to show the current selection.  ");
}

static const char *help_axis_label_font(prefs_info *prf) 
{
  return("  This is a font used to label axes.  ");
}

static const char *help_axis_numbers_font(prefs_info *prf) 
{
  return("  This is the font used for axis numbers.  ");
}

static const char *help_peaks_font(prefs_info *prf) 
{
  return("  This is the font used in the FFT peaks listing.  ");
}

static const char *help_bold_peaks_font(prefs_info *prf) 
{
  return("\
  This is the font used in the FFT peaks list\n\
  to show a major peak.  ");
}

static const char *help_transform_graph_type(prefs_info *prf) 
{
  return("\
  FFT results can be displayed as a waveform, a sonogram, \n\
  or a spectrogram.  ");
}

static const char *help_transform_type(prefs_info *prf) 
{
  return("\
  This chooses the kind of transform displayed \n\
  in the fft graph.  ");
}

static const char *help_fft_window(prefs_info *prf) 
{
  return("  This sets the fft data window.  ");
}

static const char *help_fft_window_beta(prefs_info *prf) 
{
  return("  If the FFT window has an associated parameter, this sets it.  ");
}

static const char *help_colormap(prefs_info *prf) 
{
  return("  If the FFT is being displayed as a sonogram or spectrogram,\n\
  this sets the colormap to use.  ");
}

static const char *help_fft_log_magnitude(prefs_info *prf) 
{
  return("  If this option is set, the FFTs show the magnitude axis using a log scale. ");
}

static const char *help_min_dB(prefs_info *prf) 
{
  return("\
  If the FFT graphs are using a dB scale, this sets the\n\
  minimum dB value displayed.  ");
}

static const char *help_fft_log_frequency(prefs_info *prf) 
{
  return("  If this is set, FFTs show the frequency axis using a log scale. ");
}

static const char *help_transform_normalization(prefs_info *prf) 
{
  return("  This chooses whether FFT data is normalized before display.  ");
}

static const char *help_mark_color(prefs_info *prf) 
{
  return("  This is the mark color.  ");
}

static const char *help_mix_color(prefs_info *prf) 
{
  return("  This is the color of the mix handle.  ");
}

static const char *help_sinc_width(prefs_info *prf) 
{
  return("\
  If sampling rate conversion is needed, this sets \n\
  the width of the sinc used for low-pass filtering.  ");
}

static const char *help_show_listener(prefs_info *prf) 
{
  return("  This option chooses whether to open the listener window. ");
}

static const char *help_listener_prompt(prefs_info *prf) 
{
  return("  This is the prompt displayed in the listener window.  ");
}

static const char *help_print_length(prefs_info *prf) 
{
  return("\
  When a vector is printed in the listener, this sets\n\
  the maximum number of values displayed.  ");
}

static const char *help_listener_font(prefs_info *prf) 
{
  return("  This is the font used in the listener.  ");
}

static const char *help_listener_color(prefs_info *prf) 
{
  return("  This is the background color of the listener.  ");
}

static const char *help_dac_size(prefs_info *prf) 
{
  return("  This is the DAC buffer size.  ");
}

static const char *help_dac_combines_channels(prefs_info *prf) 
{
  return("\
  If the DAC has fewer output channels than the sound you\n\
  want to play, and this option is set, then the extra \n\
  channels are mixed into the existing ones.  ");
}

#if USE_MOTIF
static const char *help_view_files_directory(prefs_info *prf) 
{
  return("  This directory is added to the View:files dialog's list.  ");
}
#endif

static const char *help_raw_chans(prefs_info *prf) 
{
  return("\
  When a raw (no header) sound is opened, this sets the \n\
  default number of channels.  ");
}

static const char *help_raw_srate(prefs_info *prf) 
{
  return("\
  When a raw (no header) sound is opened, this sets the \n\
  default sampling rate.  ");
}

static const char *help_raw_sample_type(prefs_info *prf) 
{
  return("\
  When a raw (no header) sound is opened, this sets the \n\
  default sample type.  ");
}

static const char *help_tiny_font(prefs_info *prf) 
{
  return("  When graph space is tight, Snd uses this font.  ");
}

static const char *help_fft_size(prefs_info *prf) 
{
  return("  This is the default FFT size.  ");
}

static const char *help_transform_peaks(prefs_info *prf) 
{
  return("  If this is set, FFTs include peak listings.  ");
}

static const char *help_mark_tag_size(prefs_info *prf) 
{
  return("  This is the size of the upper tag on a mark.  ");
}

static const char *help_mix_tag_size(prefs_info *prf) 
{
  return("  This sets the mix tag size.  ");
}

static const char *help_mix_waveforms(prefs_info *prf) 
{
  return("  If this is set, mixes display their waveforms.  ");
}

static const char *help_speed_control(prefs_info *prf) 
{
  return("\
  This chooses how the speed control in the control panel\n\
  divides up the speeds; as a continuous scale, by simple ratios, etc.  ");
}

static const char *help_listener_text_color(prefs_info *prf) 
{
  return("  This is the listener text color.  ");
}

static const char *help_init_window_size(prefs_info *prf) 
{
  return("  This sets Snd's size when it first comes up.  ");
}

