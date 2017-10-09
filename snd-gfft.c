/* transform settings dialog */

#include "snd.h"

/* when options window expanded vertically, graph gets less than half (empty space in button box) 
 *   this is a table grid, so I don't think much is possible
 */


static slist *transform_list = NULL, *size_list = NULL, *window_list = NULL, *wavelet_list = NULL;
static GtkWidget *transform_dialog = NULL; /* main dialog shell */
static GtkWidget *outer_table, *db_button, *peaks_button, *logfreq_button, *sono_button, *spectro_button, *normal_fft_button, *phases_button;
static GtkWidget *normalize_button, *selection_button;
static GtkWidget *alpha_label = NULL, *beta_label = NULL, *graph_drawer = NULL, *fft_window_label = NULL;
static GtkAdjustment *beta_adj, *alpha_adj, *spectrum_start_adj, *spectrum_end_adj;
static GtkWidget *spectrum_start_scale, *spectrum_end_scale;
static GtkWidget *db_txt, *peaks_txt, *lf_txt;
static gc_t *fft_gc = NULL, *fgc = NULL;
static bool ignore_callbacks;

#define NUM_TRANSFORM_SIZES 15
static const char *transform_size_names[NUM_TRANSFORM_SIZES] = 
  {"32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "65536", "262144", "1048576", "4194304    ", "16777216"};

static mus_long_t transform_sizes[NUM_TRANSFORM_SIZES] = 
  {32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576, 4194304, 16777216};


/* ---------------- window graphs ---------------- */

#define GRAPH_SIZE 128
static mus_float_t graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static mus_float_t graph_fftr[GRAPH_SIZE * 2];
static mus_float_t graph_ffti[GRAPH_SIZE * 2];

static mus_float_t fp_dB(mus_float_t py)
{
  return((py <= ss->lin_dB) ? 0.0 : (1.0 - (20.0 * log10(py) / min_dB(ss))));
}


static axis_info *axis_ap = NULL;

static void graph_redisplay(void)
{
  Drawable *wn;
  int ix0, iy0, ix1, iy1, i;
  mus_float_t xincr, x;
  graphics_context *ax;

  if (!(transform_dialog_is_active())) return;
  if (!graph_drawer) return;

  wn = WIDGET_TO_WINDOW(graph_drawer);
  if (!wn) return;

  if (!axis_ap)
    {
      axis_ap = (axis_info *)calloc(1, sizeof(axis_info));
      ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      axis_ap->ax = ax;
    }
  else ax = axis_ap->ax;
  ax->wn = WIDGET_TO_WINDOW(graph_drawer);
  ax->w = graph_drawer;
  ax->gc = fft_gc;
  ax->current_font = TINY_FONT(ss);  /* we're right on the edge; this changes if user makes dialog bigger */
  axis_ap->xmin = 0.0;
  axis_ap->xmax = 1.0;
  axis_ap->x_ambit = 1.0;
  axis_ap->x0 = 0.0;
  axis_ap->x1 = 1.0;

  ss->cr = make_cairo(ax->wn);
  cairo_push_group(ss->cr);

  /* erase previous */
  cairo_set_source_rgba(ss->cr, ax->gc->bg_color->red, ax->gc->bg_color->green, ax->gc->bg_color->blue, ax->gc->bg_color->alpha);
  cairo_rectangle(ss->cr, 0, 0, widget_width(graph_drawer), widget_height(graph_drawer));
  cairo_fill(ss->cr);

  if (axis_ap->xlabel) free(axis_ap->xlabel);
  if (fft_beta_max(fft_window(ss)) != 1.0)
    axis_ap->xlabel = mus_format("(%d, beta: %.2f)", GRAPH_SIZE, fft_beta_max(fft_window(ss)) * fft_window_beta(ss));
  else axis_ap->xlabel = mus_format("(%d)", GRAPH_SIZE);

  if (fft_window(ss) == MUS_FLAT_TOP_WINDOW)
    {
      axis_ap->ymin = -0.1;
      axis_ap->ymax = 1.0;
      axis_ap->y_ambit = 1.1;
      axis_ap->y0 = -0.1;
      axis_ap->y1 = 1.0;
    }
  else 
    {
      axis_ap->ymin = 0.0;
      axis_ap->ymax = 1.0;
      axis_ap->y_ambit = 1.0;
      axis_ap->y0 = 0.0;
      axis_ap->y1 = 1.0;
    }
  axis_ap->width = widget_width(graph_drawer);
  axis_ap->window_width = axis_ap->width;
  axis_ap->y_offset = 0;
  axis_ap->height = widget_height(graph_drawer);
  axis_ap->graph_x0 = 0;

  make_axes_1(axis_ap, X_AXIS_IN_SECONDS, 1 /* "srate" */, SHOW_ALL_AXES, NOT_PRINTING, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));

  ax->gc = fft_gc;
  ix1 = grf_x(0.0, axis_ap);
  iy1 = grf_y(graph_data[0], axis_ap);
  xincr = 1.0 / (mus_float_t)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x, axis_ap);
      iy1 = grf_y(graph_data[i], axis_ap);
      draw_line(ax, ix0, iy0, ix1, iy1);
    }
  ax->gc = fgc;
  ix1 = grf_x(0.0, axis_ap);
  iy1 = grf_y(graph_fftr[0], axis_ap);
  xincr = 1.0 / (mus_float_t)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x, axis_ap);
      if (fft_log_magnitude(ss))
	iy1 = grf_y(fp_dB(graph_fftr[i]), axis_ap);
      else iy1 = grf_y(graph_fftr[i], axis_ap);
      draw_line(ax, ix0, iy0, ix1, iy1);
    }

  cairo_pop_group_to_source(ss->cr);
  cairo_paint(ss->cr);
  free_cairo(ss->cr);
  ss->cr = NULL;
}


static void get_fft_window_data(void)
{
  int i;
  mus_make_fft_window_with_window(fft_window(ss), GRAPH_SIZE, 
				  fft_window_beta(ss) * fft_beta_max(fft_window(ss)), 
				  fft_window_alpha(ss), graph_data);
  mus_clear_floats(graph_fftr, GRAPH_SIZE * 2);
  mus_clear_floats(graph_ffti, GRAPH_SIZE * 2);
  mus_copy_floats(graph_fftr, graph_data, GRAPH_SIZE);
  mus_spectrum(graph_fftr, graph_ffti, NULL, GRAPH_SIZE * 2, MUS_SPECTRUM_IN_DB);
  for (i = 0; i < GRAPH_SIZE; i++)
    graph_fftr[i] = (graph_fftr[i] + 80.0) / 80.0; /* min dB -80.0 */
}


/* ---------------- transform size ---------------- */

static void chans_transform_size(chan_info *cp, mus_long_t size)
{
  cp->transform_size = size;
  if (cp->fft) cp->fft->size = size;
}


static void size_browse_callback(const char *name, int row, void *data)
{
  for_each_chan(force_fft_clear);
  in_set_transform_size(transform_sizes[row]);
  for_each_chan_with_mus_long_t(chans_transform_size, transform_size(ss));
  for_each_chan(calculate_fft);
  if (fft_window_label) 
    gtk_label_set_text(GTK_LABEL(fft_window_label), mus_fft_window_name(fft_window(ss)));
}


void set_transform_size(mus_long_t val)
{
  for_each_chan(force_fft_clear);
  in_set_transform_size(val);
  for_each_chan_with_mus_long_t(chans_transform_size, val);
  if (transform_dialog)
    {
      int i;
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == val)
	  {
	    slist_select(size_list, i);
	    slist_moveto(size_list, i);
	    break;
	  }
    }
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
}




/* ---------------- wavelet choice ---------------- */

static void chans_wavelet_type(chan_info *cp, int value)
{
  cp->wavelet_type = value;
}


static void wavelet_browse_callback(const char *name, int row, void *data)
{
  in_set_wavelet_type(row);
  for_each_chan_with_int(chans_wavelet_type, row);
  if (transform_type(ss) == WAVELET)
    for_each_chan(calculate_fft);
}


void set_wavelet_type(int val)
{
  if (transform_dialog) 
    {
      slist_select(wavelet_list, val);
      slist_moveto(wavelet_list, val);
    }
  in_set_wavelet_type(val);
  for_each_chan_with_int(chans_wavelet_type, val);
  if ((transform_type(ss) == WAVELET) && 
      (!(ss->graph_hook_active))) 
    for_each_chan(calculate_fft);
}




/* ---------------- window choice ---------------- */

#if GTK_CHECK_VERSION(3, 0, 0)
/* this won't work in gtk2 because it requires the alpha field (GdkRGBA not defined) */
static color_t not_so_black;

static void alpha_beta_alpha(mus_fft_window_t val)
{
#if (!GTK_CHECK_VERSION(3, 16, 0))
  if (fft_window_beta_in_use(val))
    gtk_widget_override_color(beta_label, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->black));
  else gtk_widget_override_color(beta_label, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)not_so_black);
 
  if (fft_window_alpha_in_use(val))
    gtk_widget_override_color(alpha_label, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->black));
  else gtk_widget_override_color(alpha_label, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)not_so_black);
#endif
}
#endif


static void window_browse_callback(const char *name, int row, void *data)
{
  in_set_fft_window((mus_fft_window_t)row);
  for_each_chan(calculate_fft);
  if (fft_window_label) 
    gtk_label_set_text(GTK_LABEL(fft_window_label), mus_fft_window_name(fft_window(ss)));
  get_fft_window_data();
  graph_redisplay();
#if GTK_CHECK_VERSION(3, 0, 0)
  alpha_beta_alpha(fft_window(ss));
#endif
}


void set_fft_window(mus_fft_window_t val)
{
  in_set_fft_window(val);
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
  if ((transform_dialog) && (graph_drawer))
    {
      slist_select(window_list, (int)val);
      slist_moveto(window_list, (int)val);
      if (fft_window_label) 
	gtk_label_set_text(GTK_LABEL(fft_window_label), mus_fft_window_name(val));
      get_fft_window_data();
      graph_redisplay();
#if GTK_CHECK_VERSION(3, 0, 0)
      alpha_beta_alpha(val);
#endif
    }
}
  




/* ---------------- transform type ---------------- */

static void chans_transform_type(chan_info *cp, int value)
{
  cp->transform_type = value;
}


static void transform_browse_callback(const char *name, int row, void *data)
{
  for_each_chan(force_fft_clear);
  in_set_transform_type(row);
  for_each_chan_with_int(chans_transform_type, row);
  for_each_chan(calculate_fft);
}


void make_transform_type_list(void)
{
  if (transform_dialog)
    {
      int i, j, num;
      const char **transform_names;
      num = max_transform_type();
      transform_names = (const char **)calloc(num, sizeof(char *));
      for (i = 0, j = 0; i < num; i++) 
	if (is_transform(i))
	  {
	    set_transform_position(i, j);
	    transform_names[j++] = transform_name(i);
	  }
      if (!transform_list)
	{
	  transform_list = slist_new_with_title_and_table_data("type", outer_table, transform_names, j, TABLE_ATTACH, 0, 3, 0, 4);
	  transform_list->select_callback = transform_browse_callback;
	}
      else
	{
	  slist_clear(transform_list);
	  for (i = 0; i < j; i++)
	    slist_append(transform_list, transform_names[i]);
	  slist_select(transform_list, transform_type_to_position(transform_type(ss)));
	}
      free(transform_names);
    }
}


void set_transform_type(int val)
{
  if (is_transform(val))
    {
      if (!(ss->graph_hook_active)) for_each_chan(force_fft_clear);
      in_set_transform_type(val);
      for_each_chan_with_int(chans_transform_type, val);
      if (!(ss->graph_hook_active)) 
	for_each_chan(calculate_fft);
      if (transform_dialog) 
	slist_select(transform_list, transform_type_to_position(val));
    }
}



/* ---------------- graph type (sonogram etc) ---------------- */

static void normal_fft_callback(GtkWidget *w, gpointer context)
{
  if (ignore_callbacks) return;
  if (TOGGLE_BUTTON_ACTIVE(w))
    in_set_transform_graph_type(GRAPH_ONCE);
  else in_set_transform_graph_type(GRAPH_AS_SONOGRAM);
  for_each_chan(calculate_fft);
}


static void sonogram_callback(GtkWidget *w, gpointer context)
{
  if (ignore_callbacks) return;
  if (TOGGLE_BUTTON_ACTIVE(w))
    in_set_transform_graph_type(GRAPH_AS_SONOGRAM);
  else in_set_transform_graph_type(GRAPH_ONCE);
  for_each_chan(calculate_fft);
}


static void spectrogram_callback(GtkWidget *w, gpointer context)
{
  if (ignore_callbacks) return;
  if (TOGGLE_BUTTON_ACTIVE(w))
    in_set_transform_graph_type(GRAPH_AS_SPECTROGRAM);
  else in_set_transform_graph_type(GRAPH_ONCE);
  for_each_chan(calculate_fft);
}


void set_transform_graph_type(graph_type_t val)
{
  in_set_transform_graph_type(val);
  if (transform_dialog) 
    switch (val)
      {
      case GRAPH_ONCE: 
	set_toggle_button(normal_fft_button, true, false, NULL); 
	break;
      case GRAPH_AS_SONOGRAM:   
	set_toggle_button(sono_button, true, false, NULL); 
	break;
      case GRAPH_AS_SPECTROGRAM:
	set_toggle_button(spectro_button, true, false, NULL);
	break;
      case GRAPH_AS_WAVOGRAM:
	break;
      }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}




/* ---------------- show peaks ---------------- */

static void map_show_transform_peaks(chan_info *cp, bool value) 
{
  cp->show_transform_peaks = value;
}


static void peaks_callback(GtkWidget *w, gpointer context)
{
  bool val;
  val = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  in_set_show_transform_peaks(val);
  for_each_chan_with_bool(map_show_transform_peaks, val);
  for_each_chan(calculate_fft);
}


static void max_peaks_callback(GtkWidget *w, gpointer data)
{
  int new_peaks;
  new_peaks = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(data));
  set_max_transform_peaks(new_peaks);
  for_each_chan(calculate_fft);
}


void reflect_peaks_in_transform_dialog(void) 
{
  if (transform_dialog)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(peaks_txt), max_transform_peaks(ss));
}


void set_show_transform_peaks(bool val)
{
  in_set_show_transform_peaks(val);
  for_each_chan_with_bool(map_show_transform_peaks, val);
  if (transform_dialog) 
    set_toggle_button(peaks_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}





/* ---------------- log magnitude ---------------- */

static void chans_fft_log_magnitude(chan_info *cp, bool value)
{
  cp->fft_log_magnitude = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


void set_fft_log_magnitude(bool val)
{
  in_set_fft_log_magnitude(val);
  for_each_chan_with_bool(chans_fft_log_magnitude, val);
  if (transform_dialog) 
    set_toggle_button(db_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}




/* ---------------- dB ---------------- */

static void db_callback(GtkWidget *w, gpointer context)
{
  bool val;
  val = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  in_set_fft_log_magnitude(val);
  graph_redisplay();
  for_each_chan_with_bool(chans_fft_log_magnitude, val);
  for_each_chan(calculate_fft);
}


static void min_db_callback(GtkWidget *w, gpointer data)
{
  mus_float_t new_db;
  new_db = gtk_spin_button_get_value(GTK_SPIN_BUTTON(data));
  set_min_db(-new_db);
}


void reflect_min_db_in_transform_dialog(void) 
{
  if (transform_dialog)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(db_txt), (gfloat)(-(min_dB(ss))));
}




/* ---------------- log frequency ---------------- */

static void chans_fft_log_frequency(chan_info *cp, bool value)
{
  cp->fft_log_frequency = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


static void logfreq_callback(GtkWidget *w, gpointer context)
{
  bool val;
  val = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  in_set_fft_log_frequency(val);
  for_each_chan_with_bool(chans_fft_log_frequency, val);
  for_each_chan(calculate_fft);
}


static void log_freq_callback(GtkWidget *w, gpointer data)
{
  mus_float_t new_lfb;
  new_lfb = gtk_spin_button_get_value(GTK_SPIN_BUTTON(data));
  set_log_freq_start(new_lfb);
}


void reflect_log_freq_start_in_transform_dialog(void) 
{
  if (transform_dialog)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(lf_txt), log_freq_start(ss));
}


void set_fft_log_frequency(bool val)
{
  in_set_fft_log_frequency(val);
  for_each_chan_with_bool(chans_fft_log_frequency, val);
  if (transform_dialog)
    set_toggle_button(logfreq_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}





/* ---------------- normalization ---------------- */

static void chans_transform_normalization(chan_info *cp, int value)
{
  cp->transform_normalization = (fft_normalize_t)value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


static void normalize_callback(GtkWidget *w, gpointer context)
{
  fft_normalize_t choice;
  if (TOGGLE_BUTTON_ACTIVE(w)) 
    choice = NORMALIZE_BY_CHANNEL; 
  else choice = DONT_NORMALIZE;
  in_set_transform_normalization(choice);
  for_each_chan_with_int(chans_transform_normalization, (int)choice);
  for_each_chan(calculate_fft);
}


void set_transform_normalization(fft_normalize_t val)
{
  in_set_transform_normalization(val);
  for_each_chan_with_int(chans_transform_normalization, (int)val);
  if (transform_dialog) 
    set_toggle_button(normalize_button, (val != DONT_NORMALIZE), false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}




/* ---------------- show selection ---------------- */

static void selection_callback(GtkWidget *w, gpointer context)
{
  in_set_show_selection_transform(TOGGLE_BUTTON_ACTIVE(w));
  for_each_chan(calculate_fft);
}


void set_show_selection_transform(bool show)
{
  in_set_show_selection_transform(show);
  if (transform_dialog)
    set_toggle_button(selection_button, show, false, NULL); 
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}




/* ---------------- phases ---------------- */

static void chans_fft_with_phases(chan_info *cp, bool value)
{
  cp->fft_with_phases = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


static void phases_callback(GtkWidget *w, gpointer context)
{
  bool val;
  val = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  in_set_fft_with_phases(val);
  graph_redisplay();
  for_each_chan_with_bool(chans_fft_with_phases, val);
  for_each_chan(calculate_fft);
}


void set_fft_with_phases(bool val)
{
  in_set_fft_with_phases(val);
  for_each_chan_with_bool(chans_fft_with_phases, val);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}




/* ---------------- beta ---------------- */

static void beta_callback(GtkAdjustment *adj, gpointer context)
{
  in_set_fft_window_beta((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_BETA, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
} 


void set_fft_window_beta(mus_float_t val)
{
  in_set_fft_window_beta(val);
  chans_field(FCP_BETA, val);
  if (transform_dialog) 
    {
      ADJUSTMENT_SET_VALUE(beta_adj, val);
      get_fft_window_data();
      graph_redisplay();
    }
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
}




/* ---------------- alpha ---------------- */

static void alpha_callback(GtkAdjustment *adj, gpointer context)
{
  in_set_fft_window_alpha((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_ALPHA, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  if (fft_window_alpha_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
} 


void set_fft_window_alpha(mus_float_t val)
{
  in_set_fft_window_alpha(val);
  chans_field(FCP_ALPHA, val);
  if (transform_dialog) 
    {
      ADJUSTMENT_SET_VALUE(alpha_adj, val);
      get_fft_window_data();
      graph_redisplay();
    }
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
}



/* ---------------- spectrum start/end ---------------- */

static void chans_spectrum_changed(chan_info *cp) 
{
  cp->fft_changed = FFT_CHANGE_LOCKED;
  update_graph(cp);
}


static void set_spectrum_start_scale(mus_float_t val)
{
  ADJUSTMENT_SET_VALUE(spectrum_start_adj, val);
}


static void set_spectrum_end_scale(mus_float_t val)
{
  ADJUSTMENT_SET_VALUE(spectrum_end_adj, val);
}


static void check_spectrum_start(mus_float_t end)
{
  /* don't display chans, but do reset if necessary */
  if (spectrum_start(ss) > end)
    {
      in_set_spectrum_start(end);
      if (transform_dialog)
	set_spectrum_start_scale(end);
      chans_field(FCP_SPECTRUM_START, end);
    }
}


static void check_spectrum_end(mus_float_t start)
{
  /* don't display chans, but do reset if necessary */
  if (spectrum_end(ss) < start)
    {
      in_set_spectrum_end(start);
      if (transform_dialog)
	set_spectrum_end_scale(start);
      chans_field(FCP_SPECTRUM_END, start);
    }
}


void set_spectrum_start(mus_float_t val) 
{
  if (transform_dialog)
    set_spectrum_start_scale(val);
  in_set_spectrum_start(val);
  check_spectrum_end(val);
  chans_field(FCP_SPECTRUM_START, val);
  for_each_chan(chans_spectrum_changed);
}


void set_spectrum_end(mus_float_t val)
{
  if (transform_dialog)
    set_spectrum_end_scale(val);
  in_set_spectrum_end(val);
  check_spectrum_start(val);
  chans_field(FCP_SPECTRUM_END, val);
  for_each_chan(chans_spectrum_changed);
}


static void spectrum_start_callback(GtkAdjustment *adj, gpointer context)
{
  mus_float_t start;
  start = ADJUSTMENT_VALUE(adj);
  in_set_spectrum_start(start);
  check_spectrum_end(start);
  chans_field(FCP_SPECTRUM_START, start);
  for_each_chan(chans_spectrum_changed);

}


static void spectrum_end_callback(GtkAdjustment *adj, gpointer context)
{
  mus_float_t end;
  end = ADJUSTMENT_VALUE(adj);
  in_set_spectrum_end(end);
  check_spectrum_start(end);
  chans_field(FCP_SPECTRUM_END, end);
  for_each_chan(chans_spectrum_changed);
}




/* ---------------- dialog buttons ---------------- */

static gboolean graph_configure_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  graph_redisplay();
  return(false);
}


static gboolean graph_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  graph_redisplay();
  return(false);
}


static void dismiss_transform_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(transform_dialog);
}


static void color_orientation_transform_callback(GtkWidget *w, gpointer context)
{
  make_color_orientation_dialog(true);
}


static gint delete_transform_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(transform_dialog);
  return(true);
}


static void help_transform_callback(GtkWidget *w, gpointer context)
{
  transform_dialog_help();
}


gboolean spin_button_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  widget_modify_base(w, GTK_STATE_NORMAL, ss->white);
  return(false);
}


gboolean spin_button_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  widget_modify_base(w, GTK_STATE_NORMAL, ss->basic_color);
  return(false);
}


#define RIGHT_SEP_SIZE 10


GtkWidget *make_transform_dialog(bool managed)
{
  bool need_callback = false, need_moveto = true;
  if (!transform_dialog)
    {
      GtkWidget *buttons;
      GtkWidget *display_frame, *help_button, *dismiss_button;
      GtkWidget *color_button;
      GtkWidget *se_box, *se_frame, *se_label;
      GtkWidget *end_box, *end_label, *start_box, *start_label;

#if GTK_CHECK_VERSION(3, 0, 0)
      not_so_black = rgb_to_color(0.0, 0.0, 0.0);
      not_so_black->alpha = 0.5;
#endif

      transform_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(transform_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      gtk_widget_set_name(transform_dialog, "fft_dialog");
      SG_SIGNAL_CONNECT(transform_dialog, "delete_event", delete_transform_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(transform_dialog), "Transform Options");
      sg_make_resizable(transform_dialog);
      gtk_container_set_border_width(GTK_CONTAINER(transform_dialog), 6);
      gtk_widget_realize(transform_dialog);
      /* gtk_window_resize(GTK_WINDOW(transform_dialog), 400, 500); */

      help_button = gtk_dialog_add_button(GTK_DIALOG(transform_dialog), "Help", GTK_RESPONSE_NONE);
      dismiss_button = gtk_dialog_add_button(GTK_DIALOG(transform_dialog), "Go away", GTK_RESPONSE_NONE);
      color_button = gtk_dialog_add_button(GTK_DIALOG(transform_dialog), "Color/Orientation", GTK_RESPONSE_NONE);

      gtk_widget_set_name(help_button, "dialog_button");
      gtk_widget_set_name(dismiss_button, "dialog_button");
      gtk_widget_set_name(color_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(dismiss_button);
      add_highlight_button_style(color_button);
      add_highlight_button_style(help_button);
#endif

      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_transform_callback, NULL);
      SG_SIGNAL_CONNECT(color_button, "clicked", color_orientation_transform_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", help_transform_callback, NULL);

      gtk_widget_show(dismiss_button);
      gtk_widget_show(color_button);
      gtk_widget_show(help_button);

      outer_table = gtk_table_new(16, 11, false); /* rows cols */
      gtk_container_add(GTK_CONTAINER(DIALOG_CONTENT_AREA(transform_dialog)), outer_table);
      gtk_table_set_row_spacings(GTK_TABLE(outer_table), 16);
      gtk_table_set_col_spacings(GTK_TABLE(outer_table), 16);
      gtk_table_set_homogeneous(GTK_TABLE(outer_table), true);

      /* now 8 boxes within the main box:
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list)      |  graph (fft?) of current window
         alpha/beta ==========================
	 spectrum start/end ==================
      */

      /* TYPE */
      make_transform_type_list();

      /* SIZE */
      size_list = slist_new_with_title_and_table_data("size", outer_table, transform_size_names, NUM_TRANSFORM_SIZES, TABLE_ATTACH, 3, 6, 0, 6);
      size_list->select_callback = size_browse_callback;

      /* DISPLAY */
      display_frame = gtk_frame_new(NULL);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), display_frame, 6, 11, 0, 7);
      gtk_frame_set_shadow_type(GTK_FRAME(display_frame), GTK_SHADOW_IN);

      {
	GtkWidget *label, *vbb, *hb;
	GtkWidget *pk_lab, *db_lab, *lf_lab;
	GtkAdjustment *pk_vals, *db_vals, *lf_vals;
	GtkWidget *vb, *sep1, *sep2;

	#define DISPLAY_MARGIN 2
	#define LEFT_MARGIN 8

	vbb = gtk_vbox_new(false, 6);
	gtk_container_add(GTK_CONTAINER(display_frame), vbb);
	gtk_widget_show(vbb);

	label = snd_gtk_highlight_label_new("display");
	gtk_box_pack_start(GTK_BOX(vbb), label, false, false, 0);
	gtk_widget_show(label);

	hb = gtk_hbox_new(false, 15);
	gtk_box_pack_start(GTK_BOX(vbb), hb, false, false, 0);
	gtk_widget_show(hb);

	buttons = gtk_vbox_new(false, 0);
	gtk_container_add(GTK_CONTAINER(hb), buttons);
	
	normal_fft_button = gtk_radio_button_new_with_label(NULL, "single transform");
	widget_set_margin_left(normal_fft_button, LEFT_MARGIN);
	add_check_button_style(normal_fft_button);
	gtk_box_pack_start(GTK_BOX(buttons), normal_fft_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(normal_fft_button);
	SG_SIGNAL_CONNECT(normal_fft_button, "clicked", normal_fft_callback, NULL);
	
	sono_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(normal_fft_button)), "sonogram");
	widget_set_margin_left(sono_button, LEFT_MARGIN);
	add_check_button_style(sono_button);
	gtk_box_pack_start(GTK_BOX(buttons), sono_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(sono_button);
	SG_SIGNAL_CONNECT(sono_button, "clicked", sonogram_callback, NULL);
	
	spectro_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(normal_fft_button)), "spectrogram");
	widget_set_margin_left(spectro_button, LEFT_MARGIN);
	add_check_button_style(spectro_button);
	gtk_box_pack_start(GTK_BOX(buttons), spectro_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(spectro_button);
	SG_SIGNAL_CONNECT(spectro_button, "clicked", spectrogram_callback, NULL);
	
	peaks_button = gtk_check_button_new_with_label("peaks");
	widget_set_margin_left(peaks_button, LEFT_MARGIN);
	add_check_button_style(peaks_button);
	gtk_box_pack_start(GTK_BOX(buttons), peaks_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(peaks_button);
	SG_SIGNAL_CONNECT(peaks_button, "toggled", peaks_callback, NULL);
	
	db_button = gtk_check_button_new_with_label("dB");
	widget_set_margin_left(db_button, LEFT_MARGIN);
	add_check_button_style(db_button);
	gtk_box_pack_start(GTK_BOX(buttons), db_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(db_button);
	SG_SIGNAL_CONNECT(db_button, "toggled", db_callback, NULL);
	
	logfreq_button = gtk_check_button_new_with_label("log freq");
	widget_set_margin_left(logfreq_button, LEFT_MARGIN);
	add_check_button_style(logfreq_button);
	gtk_box_pack_start(GTK_BOX(buttons), logfreq_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(logfreq_button);
	SG_SIGNAL_CONNECT(logfreq_button, "toggled", logfreq_callback, NULL);
	
	normalize_button = gtk_check_button_new_with_label("normalize");
	widget_set_margin_left(normalize_button, LEFT_MARGIN);
	add_check_button_style(normalize_button);
	gtk_box_pack_start(GTK_BOX(buttons), normalize_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(normalize_button);
	SG_SIGNAL_CONNECT(normalize_button, "toggled", normalize_callback, NULL);
	
	selection_button = gtk_check_button_new_with_label("selection");
	widget_set_margin_left(selection_button, LEFT_MARGIN);
	add_check_button_style(selection_button);
	gtk_box_pack_start(GTK_BOX(buttons), selection_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(selection_button);
	SG_SIGNAL_CONNECT(selection_button, "toggled", selection_callback, NULL);

	phases_button = gtk_check_button_new_with_label("with phases");
	widget_set_margin_left(phases_button, LEFT_MARGIN);
	add_check_button_style(phases_button);
	gtk_box_pack_end(GTK_BOX(buttons), phases_button, false, false, DISPLAY_MARGIN);
	gtk_widget_show(phases_button);
	SG_SIGNAL_CONNECT(phases_button, "toggled", phases_callback, NULL);
	
	
	vb = gtk_vbox_new(false, 0);
	gtk_container_add(GTK_CONTAINER(hb), vb);
	
	pk_lab = snd_gtk_highlight_label_new("max peaks");
	gtk_box_pack_start(GTK_BOX(vb), pk_lab, false, false, 0);
	gtk_widget_show(pk_lab);
	
	pk_vals = (GtkAdjustment *)gtk_adjustment_new(max_transform_peaks(ss), 2, 1000, 2, 10, 0);
	peaks_txt = gtk_spin_button_new(GTK_ADJUSTMENT(pk_vals), 0.0, 0);
	gtk_box_pack_start(GTK_BOX(vb), peaks_txt, false, false, 0);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(peaks_txt), true);
	SG_SIGNAL_CONNECT(pk_vals, "value_changed", max_peaks_callback, (gpointer)peaks_txt);
	SG_SIGNAL_CONNECT(peaks_txt, "enter_notify_event", spin_button_focus_callback, NULL);
	SG_SIGNAL_CONNECT(peaks_txt, "leave_notify_event", spin_button_unfocus_callback, NULL);
	gtk_widget_show(peaks_txt);

	sep1 = gtk_vseparator_new();
	gtk_box_pack_start(GTK_BOX(vb), sep1, false, false, RIGHT_SEP_SIZE);
	gtk_widget_show(sep1);

	db_lab = snd_gtk_highlight_label_new("min dB");
	gtk_box_pack_start(GTK_BOX(vb), db_lab, false, false, 0);
	gtk_widget_show(db_lab);
      
	db_vals = (GtkAdjustment *)gtk_adjustment_new((int)(-(min_dB(ss))), 2, 1000, 2, 10, 0); /* can't be negative!! */
	db_txt = gtk_spin_button_new(GTK_ADJUSTMENT(db_vals), 0.0, 0);
	gtk_box_pack_start(GTK_BOX(vb), db_txt, false, false, 0);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(db_txt), true);
	SG_SIGNAL_CONNECT(db_vals, "value_changed", min_db_callback, (gpointer)db_txt);
	SG_SIGNAL_CONNECT(db_txt, "enter_notify_event", spin_button_focus_callback, NULL);
	SG_SIGNAL_CONNECT(db_txt, "leave_notify_event", spin_button_unfocus_callback, NULL);
	gtk_widget_show(db_txt);

	sep2 = gtk_vseparator_new();
	gtk_box_pack_start(GTK_BOX(vb), sep2, false, false, RIGHT_SEP_SIZE);
	gtk_widget_show(sep2);

	lf_lab = snd_gtk_highlight_label_new("log freq start");
	gtk_box_pack_start(GTK_BOX(vb), lf_lab, false, false, 0);
	gtk_widget_show(lf_lab);
      
	lf_vals = (GtkAdjustment *)gtk_adjustment_new((int)(log_freq_start(ss)), 1, 1000, 1, 10, 0);
	lf_txt = gtk_spin_button_new(GTK_ADJUSTMENT(lf_vals), 0.0, 0);
	gtk_box_pack_start(GTK_BOX(vb), lf_txt, false, false, 0);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(lf_txt), true);
	SG_SIGNAL_CONNECT(lf_vals, "value_changed", log_freq_callback, (gpointer)lf_txt);
	SG_SIGNAL_CONNECT(lf_txt, "enter_notify_event", spin_button_focus_callback, NULL);
	SG_SIGNAL_CONNECT(lf_txt, "leave_notify_event", spin_button_unfocus_callback, NULL);

	gtk_widget_show(lf_txt);
	gtk_widget_show(vb);
      }

      gtk_widget_show(buttons);
      gtk_widget_show(display_frame);


      /* WINDOWS */
      window_list = slist_new_with_title_and_table_data("window", outer_table, (const char **)mus_fft_window_names(), MUS_NUM_FFT_WINDOWS, TABLE_ATTACH, 0, 3, 6, 12);
      window_list->select_callback = window_browse_callback;

      
      /* WAVELETS */
      wavelet_list = slist_new_with_title_and_table_data("wavelet", outer_table, (const char **)wavelet_names(), NUM_WAVELETS, TABLE_ATTACH, 3, 6, 6, 12);
      wavelet_list->select_callback = wavelet_browse_callback;

      
      /* ALPHA/BETA */

      {
	GtkWidget *alpha_box, *beta_box;
	GtkWidget *alpha_scale, *beta_scale, *ab_box, *ab_frame, *ab_label;
	ab_frame = gtk_frame_new(NULL);
	gtk_table_attach_defaults(GTK_TABLE(outer_table), ab_frame, 0, 6, 12, 14);
	gtk_frame_set_shadow_type(GTK_FRAME(ab_frame), GTK_SHADOW_IN);
	
	ab_box = gtk_vbox_new(false, 2);
	gtk_container_add(GTK_CONTAINER(ab_frame), ab_box);	
	
	ab_label = snd_gtk_highlight_label_new("window parameter");
	gtk_box_pack_start(GTK_BOX(ab_box), ab_label, false, false, 1);
	gtk_widget_show(ab_label);
	
	
	alpha_box = gtk_hbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(ab_box), alpha_box, false, false, 1);      
	gtk_widget_show(alpha_box);
	
	alpha_label = gtk_label_new("alpha: ");
	gtk_box_pack_start(GTK_BOX(alpha_box), alpha_label, false, false, 8);
	gtk_widget_show(alpha_label);      
	
	alpha_adj = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.01, 0.001, 0.01, .01);
	alpha_scale = gtk_hscale_new(GTK_ADJUSTMENT(alpha_adj));
	UNSET_CAN_FOCUS(alpha_scale);
	gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(alpha_scale)), GTK_UPDATE_CONTINUOUS);
	gtk_scale_set_digits(GTK_SCALE(alpha_scale), 2);
	gtk_scale_set_value_pos(GTK_SCALE(alpha_scale), GTK_POS_LEFT);
	gtk_scale_set_draw_value(GTK_SCALE(alpha_scale), true);
	SG_SIGNAL_CONNECT(alpha_adj, "value_changed", alpha_callback, NULL);
	SG_SIGNAL_CONNECT(alpha_scale, "format-value", scale_double_format_callback, NULL);
	
	gtk_box_pack_start(GTK_BOX(alpha_box), alpha_scale, true, true, 1);      
	
	
	beta_box = gtk_hbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(ab_box), beta_box, false, false, 1);      
	gtk_widget_show(beta_box);
	

	beta_label = gtk_label_new("beta:  ");
	gtk_box_pack_start(GTK_BOX(beta_box), beta_label, false, false, 8);
	gtk_widget_show(beta_label);      
	
	beta_adj = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.01, 0.001, 0.01, .01);
	beta_scale = gtk_hscale_new(GTK_ADJUSTMENT(beta_adj));
	UNSET_CAN_FOCUS(beta_scale);
	gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(beta_scale)), GTK_UPDATE_CONTINUOUS);
	gtk_scale_set_digits(GTK_SCALE(beta_scale), 2);
	gtk_scale_set_value_pos(GTK_SCALE(beta_scale), GTK_POS_LEFT);
	gtk_scale_set_draw_value(GTK_SCALE(beta_scale), true);
	SG_SIGNAL_CONNECT(beta_adj, "value_changed", beta_callback, NULL);
	SG_SIGNAL_CONNECT(beta_scale, "format-value", scale_double_format_callback, NULL);
	
	gtk_box_pack_start(GTK_BOX(beta_box), beta_scale, true, true, 1);      
	
	gtk_widget_show(alpha_scale);
	gtk_widget_show(beta_scale);
	gtk_widget_show(ab_box);
	gtk_widget_show(ab_frame);
      }


      /* SPECTRUM_START/END */

      se_frame = gtk_frame_new(NULL);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), se_frame, 0, 6, 14, 16);
      gtk_frame_set_shadow_type(GTK_FRAME(se_frame), GTK_SHADOW_IN);

      se_box = gtk_vbox_new(false, 2);
      gtk_container_add(GTK_CONTAINER(se_frame), se_box);	

      se_label = snd_gtk_highlight_label_new("spectrum start/end");
      gtk_box_pack_start(GTK_BOX(se_box), se_label, false, false, 1);
      gtk_widget_show(se_label);

      start_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(se_box), start_box, false, false, 1);      
      gtk_widget_show(start_box);

      start_label = gtk_label_new("start: ");
      gtk_box_pack_start(GTK_BOX(start_box), start_label, false, false, 8);
      gtk_widget_show(start_label);      

      spectrum_start_adj = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.01, 0.001, 0.01, .01);
      spectrum_start_scale = gtk_hscale_new(GTK_ADJUSTMENT(spectrum_start_adj));
      UNSET_CAN_FOCUS(spectrum_start_scale);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(spectrum_start_scale)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(spectrum_start_scale), 2);
      gtk_scale_set_value_pos(GTK_SCALE(spectrum_start_scale), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(spectrum_start_scale), true);
      SG_SIGNAL_CONNECT(spectrum_start_adj, "value_changed", spectrum_start_callback, NULL);
      SG_SIGNAL_CONNECT(spectrum_start_scale, "format-value", scale_double_format_callback, NULL);

      gtk_box_pack_start(GTK_BOX(start_box), spectrum_start_scale, true, true, 1);      


      end_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(se_box), end_box, false, false, 1);      
      gtk_widget_show(end_box);

      end_label = gtk_label_new("end:  ");
      gtk_box_pack_start(GTK_BOX(end_box), end_label, false, false, 8);
      gtk_widget_show(end_label);      

      spectrum_end_adj = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.01, 0.001, 0.01, .01);
      spectrum_end_scale = gtk_hscale_new(GTK_ADJUSTMENT(spectrum_end_adj));
      UNSET_CAN_FOCUS(spectrum_end_scale);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(spectrum_end_scale)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(spectrum_end_scale), 2);
      gtk_scale_set_value_pos(GTK_SCALE(spectrum_end_scale), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(spectrum_end_scale), true);
      SG_SIGNAL_CONNECT(spectrum_end_adj, "value_changed", spectrum_end_callback, NULL);
      SG_SIGNAL_CONNECT(spectrum_end_scale, "format-value", scale_double_format_callback, NULL);

      gtk_box_pack_start(GTK_BOX(end_box), spectrum_end_scale, true, true, 1);      

      gtk_widget_show(spectrum_start_scale);
      gtk_widget_show(spectrum_end_scale);
      gtk_widget_show(se_box);
      gtk_widget_show(se_frame);



      /* GRAPH */
      {
	GtkWidget *graph_frame, *g_vbox;

	graph_frame = gtk_frame_new(NULL);
	gtk_table_attach_defaults(GTK_TABLE(outer_table), graph_frame, 6, 11, 7, 16);
	gtk_frame_set_label_align(GTK_FRAME(graph_frame), 0.5, 0.0);
	gtk_frame_set_shadow_type(GTK_FRAME(graph_frame), GTK_SHADOW_IN);

	g_vbox = gtk_vbox_new(false, 2);
	gtk_container_add(GTK_CONTAINER(graph_frame), g_vbox);	

	fft_window_label = snd_gtk_highlight_label_new(mus_fft_window_name(fft_window(ss)));
	gtk_box_pack_start(GTK_BOX(g_vbox), fft_window_label, false, false, 1);
	gtk_widget_show(fft_window_label);

	graph_drawer = gtk_drawing_area_new();
	gtk_box_pack_end(GTK_BOX(g_vbox), graph_drawer, true, true, 0);
	widget_modify_bg(graph_drawer, GTK_STATE_NORMAL, ss->white);
	gtk_widget_show(g_vbox);

	fgc = gc_new();
	gc_set_background(fgc, ss->white);
	gc_set_foreground(fgc, ss->enved_waveform_color);

	fft_gc = gc_new();
	gc_set_background(fft_gc, ss->white);
	gc_set_foreground(fft_gc, ss->black);

	gtk_widget_show(graph_drawer);
	gtk_widget_show(graph_frame);
      }

      ignore_callbacks = true;
      if (transform_graph_type(ss) == GRAPH_ONCE) set_toggle_button(normal_fft_button, true, false, NULL);
      if (transform_graph_type(ss) == GRAPH_AS_SONOGRAM) set_toggle_button(sono_button, true, false, NULL);
      if (transform_graph_type(ss) == GRAPH_AS_SPECTROGRAM) set_toggle_button(spectro_button, true, false, NULL);
      ignore_callbacks = false;

      set_toggle_button(peaks_button, show_transform_peaks(ss), false, NULL);
      set_toggle_button(db_button, fft_log_magnitude(ss), false, NULL);
      set_toggle_button(logfreq_button, fft_log_frequency(ss), false, NULL);
      set_toggle_button(normalize_button, (transform_normalization(ss) != DONT_NORMALIZE), false, NULL);
      set_toggle_button(selection_button, show_selection_transform(ss), false, NULL);
      set_toggle_button(phases_button, fft_with_phases(ss), false, NULL);

      if (spectrum_start(ss) != 0.0) set_spectrum_start_scale(spectrum_start(ss));
      if (spectrum_end(ss) != 0.0) set_spectrum_end_scale(spectrum_end(ss));
      if (fft_window_alpha(ss) != 0.0) ADJUSTMENT_SET_VALUE(alpha_adj, fft_window_alpha(ss));
      if (fft_window_beta(ss) != 0.0) ADJUSTMENT_SET_VALUE(beta_adj, fft_window_beta(ss));

#if GTK_CHECK_VERSION(3, 0, 0)
      alpha_beta_alpha(fft_window(ss));
#endif
      need_callback = true;
      gtk_widget_show(outer_table);
      set_dialog_widget(TRANSFORM_DIALOG, transform_dialog);
    }
  else 
    {
      raise_dialog(transform_dialog);
      need_moveto = false;
    }

  if (managed) 
    {
      gtk_widget_show(transform_dialog);
      if (need_moveto)
	{
	  int i;
	  slist_select(window_list, (int)fft_window(ss));
	  slist_moveto(window_list, (int)fft_window(ss));

	  slist_select(wavelet_list, wavelet_type(ss));
	  slist_moveto(wavelet_list, wavelet_type(ss));

	  slist_select(transform_list, transform_type_to_position(transform_type(ss)));
	  for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	    if (transform_sizes[i] == transform_size(ss))
	      {
		slist_select(size_list, i);
		slist_moveto(size_list, i);
		break;
	      }
	}
    }
  if (need_callback)
    {
      get_fft_window_data();
      SG_SIGNAL_CONNECT(graph_drawer, DRAW_SIGNAL, graph_expose_callback, NULL);
      SG_SIGNAL_CONNECT(graph_drawer, "configure_event", graph_configure_callback, NULL);
    }

  return(transform_dialog);
}


bool transform_dialog_is_active(void)
{
  return((transform_dialog) && 
	 (widget_is_active(transform_dialog)));
}
