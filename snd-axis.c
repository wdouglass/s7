#include "snd.h"

bool is_x_axis_style(int n)
{
  switch (n)
    {
    case X_AXIS_IN_SECONDS: case X_AXIS_IN_SAMPLES: case X_AXIS_AS_PERCENTAGE: 
    case X_AXIS_IN_BEATS: case X_AXIS_IN_MEASURES: case X_AXIS_AS_CLOCK: 
      return(true);
      break;
    }
  return(false);
}


bool shows_axes(int n)
{
  switch (n)
    {
    case SHOW_NO_AXES: case SHOW_ALL_AXES: case SHOW_X_AXIS: 
    case SHOW_ALL_AXES_UNLABELLED: case SHOW_X_AXIS_UNLABELLED: 
    case SHOW_BARE_X_AXIS: 
      return(true);
      break;
    }
  return(false);
}


typedef struct tick_descriptor {
  double hi, lo; 
  int max_ticks;
  double flo, fhi, mlo, mhi, step, tenstep;
  int tens;
  int maj_tick_len, min_tick_len, min_label_width, max_label_width;
  char *min_label, *max_label;
  mus_float_t grid_scale;
} tick_descriptor;


static tick_descriptor *free_tick_descriptor(tick_descriptor *td)
{
  if (td)
    {
      if (td->min_label) {free(td->min_label); td->min_label = NULL;}
      if (td->max_label) {free(td->max_label); td->max_label = NULL;}
      free(td);
    }
  return(NULL);
}


static tick_descriptor *describe_ticks(tick_descriptor *gd_td, double lo, double hi, int max_ticks, mus_float_t grid_scale)
{
  /* given absolute (unchangeable) axis bounds lo and hi, and maximum number of ticks to use, find a "pretty" tick placement */
  /* much of the work here involves floating point rounding problems.  We assume the tick labeller will round as well */

  tick_descriptor *td;
  int ten, hib, lob, offset = 0;
  double flog10, plog10;
  double frac, ften, hilo_diff, eten, flt_ten;
  double inside, mfdiv, mten, mften;
  int mticks, mdiv;

  if (!gd_td)
    td = (tick_descriptor *)calloc(1, sizeof(tick_descriptor));
  else 
    {
      td = gd_td;
      if ((td->hi == hi) && 
	  (td->lo == lo) && 
	  (td->max_ticks == max_ticks) &&
	  ((fabs(td->grid_scale - grid_scale)) < .01))
	return(td);
    }
  td->hi = hi;
  td->lo = lo;
  td->grid_scale = grid_scale;
  hilo_diff = hi - lo;
  if (hilo_diff < .001) 
    {
      offset = (int)hi;
      hi -= offset;
      lo -= offset;
    }
  td->max_ticks = max_ticks;
  flt_ten = log10(hilo_diff);
  ten = (int)floor(flt_ten);
  frac = flt_ten - ten;
  if (frac > .9999) ten++;
  eten = pow(10, ten);
  hib = (int)floor(hi / eten);
  lob = (int)ceil(lo / eten);
  /* it's possible to wrap-around here and get negative numbers (so we keep the offset separate above) */
  if (lob != hib) 
    {
      td->mlo = (double)(lob * eten);
      td->mhi = (double)(hib * eten);
    }
  else
    {
      double flt_ften;
      /* try next lower power of ten */
      ften = eten * .1;
      flt_ften = (hi / ften);
      hib = (int)floor(flt_ften);
      frac = flt_ften - hib;
      if (frac > .9999) hib++;
      lob = (int)ceil(lo / ften);
      td->mlo = (double)(lob * ften);
      td->mhi = (double)(hib * ften);
    }
  inside = (td->mhi - td->mlo) / hilo_diff;
  mticks = (int)floor(inside * max_ticks);

  if (mticks <= 1) mdiv = 1;
  else if (mticks < 3) mdiv = mticks;
  else if (mticks == 3) mdiv = 2;
  else if (mticks < 6) mdiv = mticks;
  else if (mticks < 10) mdiv = 5;
  else mdiv = (int)(10 * floor(mticks / 10));

  mfdiv = (td->mhi - td->mlo) / mdiv;
  flog10 = floor(log10(mfdiv));
  plog10 = pow(10, flog10);
  td->tens = (int)fabs(flog10);

  mten = grid_scale * (double)(floor(4.0 * (.00001 + (mfdiv / plog10)))) / 4.0;
  if (mten < 1.0) mten = 1.0;
  if ((mten == 1.0) || (mten == 2.0) || (mten == 2.5) || (mten == 5.0)) ften = mten;
  else if (mten < 2.0) ften = 2.0;
  else if (mten < 2.5) ften = 2.5;
  else if (mten < 5.0) ften = 5.0;
  else ften = 10.0;

  td->tenstep = ften;
  mften = ften * plog10;
  td->step = mften;
  flt_ten = lo / mften;
  lob = (int)ceil(flt_ten);
  frac = lob - flt_ten;
  if (frac > .9999) lob--;
  td->flo = lob * mften;
  flt_ten = (hi / mften);
  hib = (int)floor(flt_ten);
  frac = flt_ten - hib;
  if (frac > .9999) hib++;
  td->fhi = hib * mften;
  if (hilo_diff < .001) 
    {
      td->mlo += offset;
      td->mhi += offset;
      td->flo += offset;
      td->fhi += offset;
    }
  return(td);
}


static bool first_beat(chan_info *cp, double val)
{
  int measure, beat;
  double beat_frac;
  beat = (int)val;
  beat_frac = val - beat;
  measure = (int)(beat / cp->beats_per_measure);
  beat = beat - measure * cp->beats_per_measure;
  return((beat == 0) &&
	 (beat_frac < .001));
}


static char *measure_number(int bpm, double val)
{
  /* split out the measure number, change to beat count (1-based), add fraction, if any */
  /* "val" is in terms of beats */
  char *buf;
  int measure, beat;
  double beat_frac;
  beat = (int)val;
  beat_frac = val - beat;
  measure = (int)(beat / bpm);
  beat = beat - measure * bpm;
  buf = (char *)calloc(64, sizeof(char));
  if (beat_frac > .001)
    {
      char *frac_buf, *tmp; /* according to the C spec, there's no way to get %f to omit the leading "0" */
      frac_buf = (char *)calloc(32, sizeof(char));
      snprintf(frac_buf, 32, "%.3f", beat_frac);
      if (frac_buf[0] == '0')
	tmp = (frac_buf + 1);  /* omit the leading "0" */
      else tmp = frac_buf;
      snprintf(buf, 64, "%d(%d)%s", 1 + measure, 1 + beat, tmp);
      free(frac_buf);
    }
  else snprintf(buf, 64, "%d(%d)", 1 + measure, 1 + beat);
  return(buf);
}


static char *clock_number(double loc, int tens)
{
  /* DD:HH:MM:SS.ddd */
  #define CLOCK_BUFFER_SIZE 64

  int day, hour, minute, second;
  double frac_second;
  char *buf;

  second = (int)loc;
  frac_second = loc - second;
  minute = (int)floor(second / 60);
  hour = (int)floor(minute / 60);
  day = (int)floor(hour / 24);
  second %= 60;
  minute %= 60;
  hour %= 24;

  buf = (char *)calloc(CLOCK_BUFFER_SIZE, sizeof(char));

  if (day > 0)
    snprintf(buf, CLOCK_BUFFER_SIZE, "%02d:%02d:%02d:%02d.%0*d", day, hour, minute, second, tens, (int)(frac_second * pow(10.0, tens)));
  else
    {
      if (hour > 0)
	snprintf(buf, CLOCK_BUFFER_SIZE, "%02d:%02d:%02d.%0*d", hour, minute, second, tens, (int)(frac_second * pow(10.0, tens)));
      else
	{
	  if (minute > 0)
	    snprintf(buf, CLOCK_BUFFER_SIZE, "%02d:%02d.%0*d", minute, second, tens, (int)(frac_second * pow(10.0, tens)));
	  else
	    {
	      if (second > 0)
		snprintf(buf, CLOCK_BUFFER_SIZE, "%d.%0*d", second, tens, (int)(frac_second * pow(10.0, tens)));
	      else snprintf(buf, CLOCK_BUFFER_SIZE, "0.%0*d", tens, (int)(frac_second * pow(10.0, tens)));
	    }
	}
    }

  return(buf);
}


static char *location_to_string(double loc, int style, int bpm, int tens)
{
  if (style == X_AXIS_IN_SAMPLES)
    tens = 0;
  else
    {
      if (tens == 0)
	tens = 1; /* in x axis we usually want the ".0" */
    }

  switch (style)
    {
    case X_AXIS_AS_CLOCK:
      return(clock_number(loc, tens));
      break;
    case X_AXIS_IN_MEASURES:
      return(measure_number(bpm, loc));
      break;
    default:
      return(prettyf(loc, tens));
      break;
    }
  return(prettyf(loc, tens));
}


char *x_axis_location_to_string(chan_info *cp, double loc)
{
  if (cp)
    {
      axis_info *ap;
      ap = cp->axis; /* time graph */
      if (ap)
	{
	  tick_descriptor *tdx;
	  tdx = ap->x_ticks;
	  if (tdx)
	    return(location_to_string(loc, cp->x_axis_style, cp->beats_per_measure, tdx->tens));
	}
    }
  return(prettyf(loc, 2));
}


axis_info *free_axis_info(axis_info *ap)
{
  if (!ap) return(NULL);
  if (ap->x_ticks) ap->x_ticks = free_tick_descriptor(ap->x_ticks);
  if (ap->y_ticks) ap->y_ticks = free_tick_descriptor(ap->y_ticks);
  /* leave ap->ax alone -- it actually belongs to cp */
  if (ap->xlabel) 
    {
      free(ap->xlabel); 
      ap->xlabel = NULL;
    }
  if (ap->default_xlabel) 
    {
      free(ap->default_xlabel); 
      ap->default_xlabel = NULL;
    }
  if (ap->ylabel) 
    {
      free(ap->ylabel); 
      ap->ylabel = NULL;
    }
  free(ap);
  return(NULL);
}


int grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((int)(ap->x_base + val * ap->x_scale));
}


int grf_y(mus_float_t val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((int)(ap->y_base + val * ap->y_scale));
}


void init_axis_scales(axis_info *ap)
{
  if ((ap->x_axis_x0 == ap->x_axis_x1) || (ap->x0 == ap->x1))
    ap->x_scale = 0.0;
  else ap->x_scale = ((double)(ap->x_axis_x1 - ap->x_axis_x0)) / ((double)(ap->x1 - ap->x0));
  ap->x_base = (double)(ap->x_axis_x0 - ap->x0 * ap->x_scale);
  if ((ap->y_axis_y0 == ap->y_axis_y1) || (ap->y0 == ap->y1))
    ap->y_scale = 0.0;
  else ap->y_scale = (mus_float_t)(ap->y_axis_y1 - ap->y_axis_y0) / (ap->y1 - ap->y0);
  ap->y_base = (mus_float_t)(ap->y_axis_y0 - ap->y0 * ap->y_scale);
}


static int tick_grf_x(double val, axis_info *ap, x_axis_style_t style, int srate)
{
  int res = 0;
  switch (style)
    {
    case X_AXIS_AS_CLOCK:
    case X_AXIS_IN_SECONDS: 
    default:
      res = (int)(ap->x_base + val * ap->x_scale); 
      break;

    case X_AXIS_IN_BEATS: 
    case X_AXIS_IN_MEASURES:
      if (ap->cp)
	res = (int)(ap->x_base + val * ap->x_scale * 60.0 / ap->cp->beats_per_minute);
      else res = (int)(ap->x_base + val * ap->x_scale); 
      break;

    case X_AXIS_IN_SAMPLES: 
      res = (int)(ap->x_axis_x0 + (val - ap->x0 * srate) * ap->x_scale / srate); 
      break;

    case X_AXIS_AS_PERCENTAGE: 
      res = (int)(ap->x_axis_x0 + (val - ap->x0 / ap->xmax) * ap->x_scale * ap->xmax); 
      break;
    }
  if (res >= -32768) 
    {
      if (res < 32768) return(res);
      return(32767);
    }
  return(-32768);
}


#if HAVE_GL
  #if WITH_GL2PS
    void gl2ps_text(const char *msg);
  #endif

static bool gl_fonts_activated = false;
static int label_base, number_base;

static void activate_gl_fonts(void)
{
#if USE_MOTIF
  if (!gl_fonts_activated)
    {
      XFontStruct *label, *number;
      label = (XFontStruct *)(AXIS_LABEL_FONT(ss));
      number = (XFontStruct *)(AXIS_NUMBERS_FONT(ss));
      label_base = glGenLists(128);
      number_base = glGenLists(128);
      glXUseXFont(label->fid, 32, 96, label_base + 32);
      glXUseXFont(number->fid, 32, 96, number_base + 32);
      gl_fonts_activated = true;
    }
#else
  if (!gl_fonts_activated)
    {
      label_base = glGenLists(128);
      number_base = glGenLists(128);
      gl_fonts_activated = true;
    }
#endif
}


void reload_label_font(void)
{
#if USE_MOTIF
  if (gl_fonts_activated)
    {
      XFontStruct *label;
      glDeleteLists(label_base, 128);
      label_base = glGenLists(128);
      label = (XFontStruct *)(AXIS_LABEL_FONT(ss));
      glXUseXFont(label->fid, 32, 96, label_base + 32);
    }
#else
  if (gl_fonts_activated)
    {
      glDeleteLists(label_base, 128);
      label_base = glGenLists(128);
      /* gdk_gl_font_use_pango_font(AXIS_LABEL_FONT(ss), 32, 96, label_base + 32); */
    }
#endif
}


void reload_number_font(void)
{
#if USE_MOTIF
  if (gl_fonts_activated)
    {
      XFontStruct *number;
      glDeleteLists(number_base, 128);
      number_base = glGenLists(128);
      number = (XFontStruct *)(AXIS_NUMBERS_FONT(ss));
      glXUseXFont(number->fid, 32, 96, number_base + 32);
    }
#else
  if (gl_fonts_activated)
    {
      glDeleteLists(number_base, 128);
      number_base = glGenLists(128);
      /* gdk_gl_font_use_pango_font(AXIS_NUMBERS_FONT(ss), 32, 96, number_base + 32); */
    }
#endif
}
#endif


static void draw_horizontal_grid_line(int y, axis_info *ap, graphics_context *ax)
{
  color_t old_color;
  old_color = get_foreground_color(ax);
  if (ap->cp->selected)
    set_foreground_color(ax, ss->selected_grid_color);
  else set_foreground_color(ax, ss->grid_color);
  draw_line(ax, ap->y_axis_x0, y, ap->x_axis_x1, y);
  set_foreground_color(ax, old_color);
}


static void draw_vertical_grid_line(int x, axis_info *ap, graphics_context *ax)
{
  color_t old_color;
  old_color = get_foreground_color(ax);
  if (ap->cp->selected)
    set_foreground_color(ax, ss->selected_grid_color);
  else set_foreground_color(ax, ss->grid_color);
  draw_line(ax, x, ap->x_axis_y0, x, ap->y_axis_y1);
  set_foreground_color(ax, old_color);
}


static void draw_x_number(const char *label, int x, int y, int hgt, axis_info *ap, graphics_context *ax, printing_t printing)
{
  /* from motif point of view, gtk is down by font height (ascent) in pixels */

  if (x < 0) x = 0; /* if no y axis and no labels, this sometimes is pushed left too far */

#if USE_MOTIF
  y = y + hgt + 1;
#else
  y = y + 4;
#endif
  draw_string(ax, x, y, label, mus_strlen(label));
  if (printing) 
    ps_draw_string(ap, x, y, label);
}


static void draw_y_number(const char *label, int x, int y, int hgt, axis_info *ap, graphics_context *ax, printing_t printing)
{
  /* from motif point of view, gtk is down by font height (ascent) in pixels */
#if USE_MOTIF
  y = y + (int)(hgt / 2);
#else
  y = y - (int)(hgt / 2);
#endif
  draw_string(ax, x, y, label, mus_strlen(label));
  if (printing) 
    ps_draw_string(ap, x, y, label);
}


static void draw_label(const char *label, int x, int y, int yoff, axis_info *ap, graphics_context *ax, printing_t printing)
{
  /* from motif point of view, gtk is down by font height (ascent) in pixels */
#if USE_GTK
  y -= yoff;
#endif
  draw_string(ax, x - 10, y, label, mus_strlen(label));
  if (printing) 
    ps_draw_string(ap, x - 10, y, label);
}


static void draw_vertical_tick(int x, int y0, int y1, axis_info *ap, graphics_context *ax, printing_t printing, bool include_grid)
{
  draw_line(ax, x, y1, x, y0);
  if (printing) ps_draw_line(ap, x, y1, x, y0);
  if (include_grid) draw_vertical_grid_line(x, ap, ax);
}


static void draw_horizontal_tick(int x0, int x1, int y, axis_info *ap, graphics_context *ax, printing_t printing, bool include_grid)
{
  draw_line(ax, x0, y, x1, y);
  if (printing) ps_draw_line(ap, x0, y, x1, y);
  if (include_grid) draw_horizontal_grid_line(y, ap, ax);
}


static void draw_log_tick_label(const char *label, int logx, int y, int hgt, int x_label_width, int right_border_width, 
				axis_info *ap, graphics_context *ax, printing_t printing, bool use_tiny_font)
{
  /* is there room for a label? */
  /* the main label is at ap->x_label_x to that plus x_label_width */
  int lx0, lx1, tx0, tx1, label_width;
  lx0 = ap->x_label_x;
  lx1 = lx0 + x_label_width;
  label_width = number_width(label, use_tiny_font);
  tx0 = (int)(logx - .45 * label_width);
  if ((tx0 + label_width) > ap->x_axis_x1)
    tx0 = (int)(logx - label_width + .75 * right_border_width);
  tx1 = tx0 + label_width;
  if ((lx0 > tx1) || (lx1 < tx0))
    draw_x_number(label, tx0, y, hgt, ap, ax, printing);
}


static void use_tiny(graphics_context *ax, printing_t printing)
{
#if USE_MOTIF
  ax->current_font = ((XFontStruct *)(TINY_FONT(ss)))->fid;
  XSetFont(ax->dp, ax->gc, ((XFontStruct *)(TINY_FONT(ss)))->fid);
#else
#if USE_GTK
  ax->current_font = TINY_FONT(ss);
#endif
#endif
  if (printing) ps_set_tiny_numbers_font();
}


void set_numbers_font(graphics_context *ax, printing_t printing, bool use_tiny_font)
{
  if (use_tiny_font)
    use_tiny(ax, printing);
  else
    {
#if USE_MOTIF
      ax->current_font = ((XFontStruct *)(AXIS_NUMBERS_FONT(ss)))->fid;
      XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_NUMBERS_FONT(ss)))->fid);
#else
#if USE_GTK
      ax->current_font = AXIS_NUMBERS_FONT(ss);
#endif
#endif
      if (printing) ps_set_number_font();
    }
}


static void set_labels_font(graphics_context *ax, printing_t printing, bool use_tiny_font)
{
  if (use_tiny_font)
    use_tiny(ax, printing);
  else
    {
#if USE_MOTIF
      ax->current_font = ((XFontStruct *)(AXIS_LABEL_FONT(ss)))->fid;
      XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_LABEL_FONT(ss)))->fid);
#else
  #if USE_GTK
      ax->current_font = AXIS_LABEL_FONT(ss);
  #endif
#endif
      if (printing) ps_set_label_font();
    }
}


void make_axes_1(axis_info *ap, x_axis_style_t x_style, int srate, show_axes_t axes, printing_t printing, 
		 with_x_axis_t show_x_axis, with_grid_t with_grid, log_axis_t log_axes, mus_float_t grid_scale)
{
  int width, height;
  int axis_thickness, left_border_width, bottom_border_width, top_border_width, right_border_width, inner_border_width;
  int major_tick_length, minor_tick_length, x_tick_spacing, y_tick_spacing;
  bool include_x_label, include_x_ticks, include_x_tick_labels, include_y_ticks, include_y_tick_labels, include_grid;
  bool y_axis_linear = true, x_axis_linear = true, use_tiny_font = false;
  int x_label_width, x_label_height, x_number_height;
  int num_ticks;
  tick_descriptor *tdx = NULL, *tdy = NULL;
  int curx, cury;
  graphics_context *ax;
#if HAVE_GL
  mus_float_t xthick, ythick, xmajorlen, xminorlen, ymajorlen, yminorlen;
#endif

  ax = ap->ax;
  width = ap->width;
  height = ap->height;
  ap->graph_active = ((width > 4) || (height > 10));
  include_grid = ((ap->cp) && (with_grid));

  if ((axes == SHOW_NO_AXES) || (width < 40) || (height < 40) || (ap->xmax == 0.0))
    {
      /* leave it set up for bare graph */
      if (height > 100)
	{
	  ap->y_axis_y0 = ap->y_offset + height - 20;
	  ap->y_axis_y1 = ap->y_offset + 10;
	}
      else
	{
	  ap->y_axis_y0 = ap->y_offset + (int)(0.8 * height);
	  ap->y_axis_y1 = ap->y_offset + (int)(0.1 * height);
	}
      if (width > 100)
	{
	  ap->x_axis_x1 = ap->graph_x0 + width - 10;
	  ap->x_axis_x0 = ap->graph_x0 + 20;
	}
      else
	{
	  ap->x_axis_x1 = ap->graph_x0 + (int)(0.9 * width);
	  ap->x_axis_x0 = ap->graph_x0 + (int)(0.2 * width);
	}
      init_axis_scales(ap);
      return;
    }

  left_border_width = 10;
#if USE_MOTIF
  bottom_border_width = 10;
#else
  bottom_border_width = 8;
#endif
  top_border_width = 10;
  right_border_width = 14;
  inner_border_width = 5;
  x_tick_spacing = 20; 
  if (width < 250) x_tick_spacing = 10 + (width / 25);
  y_tick_spacing = 20; 
  if (height < 250) y_tick_spacing = 10 + (height / 25);
  if ((height < 100) || (width < 100))
    {
      major_tick_length = 4;
      minor_tick_length = 2;
    }
  else
    {
      major_tick_length = 9;
      minor_tick_length = 5;
    }
  axis_thickness = 2;
#if HAVE_GL
  xthick = (mus_float_t)(2 * axis_thickness) / (mus_float_t)height;
  ythick = (mus_float_t)(2 * axis_thickness) / (mus_float_t)width;
  xmajorlen = (mus_float_t)(2 * major_tick_length) / (mus_float_t)height;
  xminorlen = (mus_float_t)(2 * minor_tick_length) / (mus_float_t)height;
  ymajorlen = (mus_float_t)(2 * major_tick_length) / (mus_float_t)width;
  yminorlen = (mus_float_t)(2 * minor_tick_length) / (mus_float_t)width;
#endif
  
  if (show_x_axis)
    {
      if (axes == SHOW_BARE_X_AXIS)
	{
	  include_x_label = false;
	  include_x_tick_labels = false;
	  include_x_ticks = false;
	}
      else
	{
	  if ((axes == SHOW_X_AXIS_UNLABELLED) || (axes == SHOW_ALL_AXES_UNLABELLED))
	    include_x_label = false;
	  else include_x_label = ((ap->xlabel) && ((height > 100) && (width > 100)));
	  include_x_tick_labels = ((height > 60) && (width > 100));
	  include_x_ticks = ((height > 40) && (width > 40));
	}
    }
  else
    {
      include_x_label = false;
      include_x_tick_labels = false;
      include_x_ticks = false;
    }
  if (log_axes == WITH_LOG_X_AXIS) x_axis_linear = false;
  
  if ((axes != SHOW_X_AXIS) && (axes != SHOW_X_AXIS_UNLABELLED))
    {
      include_y_tick_labels = ((width > 100) && (height > 60));
      include_y_ticks = ((width > 100) && (height > 40));
    }
  else
    {
      include_y_tick_labels = false;
      include_y_ticks = false;
    }
  if (log_axes == WITH_LOG_Y_AXIS) y_axis_linear = false;
  
  curx = left_border_width;
  cury = height - bottom_border_width;
  use_tiny_font = ((width < 250) || (height < 140));
  
  x_number_height = number_height((use_tiny_font) ? TINY_FONT(ss) : AXIS_NUMBERS_FONT(ss));
  x_label_height = 0;
  x_label_width = 0;
  
  if (include_x_label)
    {
      x_label_width = label_width(ap->xlabel, use_tiny_font);
      if ((x_label_width + curx + right_border_width) > width)
	{
	  include_x_label = false;
	  x_label_width = 0;
	  x_label_height = 0;
	}
      else x_label_height = label_height(use_tiny_font);
    }
  else
    {
      if (include_x_ticks) 
	x_label_height = label_height(use_tiny_font);
      /* bare x axis case handled later */
    }
  
  if (y_axis_linear)
    {
      if (include_y_ticks)
	{
	  /* figure out how long the longest tick label will be and make room for it */
	  /* values go from ap->y0 to ap->y1 */
	  /* basic tick spacing is tick_spacing pixels */

	  num_ticks = cury / y_tick_spacing;
	  /* ticks start and stop at 10-based locations (i.e. sloppy axis bounds) */
	  /* so, given the current min..max, find the pretty min..max for ticks */

	  if (ap->y1 <= ap->y0)
	    {
	      if (ap->y0 != 0.0)
		ap->y1 = ap->y0 * 1.25;
	      else ap->y1 = 1.0;
	    }

	  tdy = describe_ticks(ap->y_ticks, ap->y0, ap->y1, num_ticks, grid_scale);
	  ap->y_ticks = tdy;
	  if (include_y_tick_labels)
	    {
	      int tick_label_width;
	      if (tdy->min_label) 
		{
		  free(tdy->min_label); 
		  tdy->min_label = NULL;
		}
	      tdy->min_label = prettyf(tdy->mlo, tdy->tens);
	      tdy->min_label_width = number_width(tdy->min_label, use_tiny_font);
	      
	      if (tdy->max_label) 
		{
		  free(tdy->max_label); 
		  tdy->max_label = NULL;
		}
	      tdy->max_label = prettyf(tdy->mhi, tdy->tens);
	      tdy->max_label_width = number_width(tdy->max_label, use_tiny_font);
	      tick_label_width = tdy->min_label_width;
	      if (tick_label_width < tdy->max_label_width) 
		tick_label_width = tdy->max_label_width;
	      if (((curx + tick_label_width) > (int)(.61 * width)) || 
		  ((4 * x_number_height) > height))
		include_y_tick_labels = false;
	      else curx += tick_label_width;
	    }
	  
	  curx += major_tick_length;
	  tdy->maj_tick_len = major_tick_length;
	  tdy->min_tick_len = minor_tick_length;
	  ap->y_axis_y1 = top_border_width;
	}
      else ap->y_axis_y1 = 0;
    }
  else
    {
      /* log case */
      if (include_y_tick_labels)
	curx += number_width("10000", use_tiny_font);
      curx += major_tick_length;
      ap->y_axis_y1 = include_y_ticks ? top_border_width : 0;
    }
  
  ap->x_axis_x1 = width - right_border_width;
  ap->x_axis_x0 = curx;

  if (ap->x1 <= ap->x0)
    {
      if (ap->x0 != 0.0)
	ap->x1 = ap->x0 * 1.25;
      else ap->x1 = .1;
    }

  if ((x_axis_linear) && (include_x_ticks))
    {
      num_ticks = (ap->x_axis_x1 - curx) / x_tick_spacing;
      switch (x_style)
	{
	case X_AXIS_AS_CLOCK:
	case X_AXIS_IN_SECONDS: 
	default:
	  tdx = describe_ticks(ap->x_ticks, ap->x0, ap->x1, num_ticks, grid_scale); 
	  break;

	case X_AXIS_IN_SAMPLES: 
	  tdx = describe_ticks(ap->x_ticks, ap->x0 * srate, ap->x1 * srate, num_ticks, grid_scale); 
	  break;

	case X_AXIS_IN_BEATS: 
	case X_AXIS_IN_MEASURES:
	  if (ap->cp) /* cp==null probably can't happen -- ap->cp is null (only?) if we're called from the envelope editor */
	    {
	      mus_float_t beats_per_second;
	      beats_per_second = ap->cp->beats_per_minute / 60.0;
	      tdx = describe_ticks(ap->x_ticks, 
				   ap->x0 * beats_per_second,
				   ap->x1 * beats_per_second,
				   num_ticks, 
				   grid_scale); 
	      /* here we are getting the intra-beat settings, if x-axis-in-measures */
	    }
	  else tdx = describe_ticks(ap->x_ticks, ap->x0, ap->x1, num_ticks, grid_scale); 
	  break;
	  
	  /* measure-positions or some such list could use marks */
	  
	case X_AXIS_AS_PERCENTAGE: 
	  tdx = describe_ticks(ap->x_ticks, ap->x0 / ap->xmax, ap->x1 / ap->xmax, num_ticks, grid_scale); 
	  break;
	}
      ap->x_ticks = tdx;
      if (include_x_tick_labels)
	{
	  int tick_label_width;
	  if (tdx->min_label) 
	    {
	      free(tdx->min_label); 
	      tdx->min_label = NULL;
	    }
	  tdx->min_label = location_to_string(tdx->mlo, x_style, (ap->cp) ? (ap->cp->beats_per_measure) : 1, tdx->tens);
	  tdx->min_label_width = number_width(tdx->min_label, use_tiny_font);
	  if (tdx->max_label) 
	    {
	      free(tdx->max_label); 
	      tdx->max_label = NULL;
	    }
	  tdx->max_label = location_to_string(tdx->mhi, x_style, (ap->cp) ? (ap->cp->beats_per_measure) : 1, tdx->tens);
	  tdx->max_label_width = number_width(tdx->max_label, use_tiny_font);
	  tick_label_width = tdx->min_label_width;
	  if (tick_label_width < tdx->max_label_width) 
	    tick_label_width = tdx->max_label_width;
	  if ((curx + 2 * tick_label_width) > (int)(.61 * width)) 
	    include_x_tick_labels = false;
	}
      tdx->maj_tick_len = major_tick_length;
      tdx->min_tick_len = minor_tick_length;
    }
  
  if ((include_x_label) || (include_x_tick_labels))
    {
      ap->x_label_y = cury;
      ap->x_label_x = (curx + width - x_label_width) / 2;
      cury -= (x_label_height + inner_border_width);
    }
  else
    {
      if (axes == SHOW_BARE_X_AXIS)
	cury -= (label_height(false) + inner_border_width);
    }

  ap->y_axis_y0 = cury;
  ap->y_axis_x0 = curx;
  ap->y_axis_x0 += ap->graph_x0;
  ap->x_axis_x0 += ap->graph_x0;
  ap->x_axis_x1 += ap->graph_x0;
  ap->x_label_x += ap->graph_x0;
  ap->x_axis_y0 = cury;
  /* now if y_offset is in use, apply global shift in y direction */
  ap->x_axis_y0 += ap->y_offset;
  ap->y_axis_y0 += ap->y_offset;
  ap->y_axis_y1 += ap->y_offset;
  ap->x_label_y += ap->y_offset;

  init_axis_scales(ap);

  if ((printing) && (ap->cp) &&
      ((ap->cp->chan == 0) || (ap->cp->sound->channel_style != CHANNELS_SUPERIMPOSED)))
    ps_bg(ap, ax);
#if HAVE_GL
  if (ap->use_gl) activate_gl_fonts();
  ap->used_gl = ap->use_gl;
#endif
  
  /* x axis label */
  if (include_x_label)
    {
      set_labels_font(ax, printing, use_tiny_font);
#if HAVE_GL
      if (ap->use_gl)
	{
	  mus_float_t yl;
	  yl = -0.5 - xthick - ((mus_float_t)(4 * major_tick_length + x_label_height) / (mus_float_t)height);
	  glColor3f(0.0, 0.0, 0.0);
	  glRasterPos3f(-0.1, 0.0, yl);
	  glListBase(label_base);
  #if WITH_GL2PS
	  if (ss->gl_printing) gl2ps_text(ap->xlabel);
  #endif
	  glCallLists(mus_strlen(ap->xlabel), GL_UNSIGNED_BYTE, (GLubyte *)(ap->xlabel));
	}
      else
#endif
	{
	  draw_label(ap->xlabel, ap->x_label_x, ap->x_label_y + inner_border_width, x_label_height, ap, ax, printing);
	}
    }
  
  /* x axis */
  if (show_x_axis)
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  glBegin(GL_POLYGON);
	  glColor3f(0.0, 0.0, 0.0);
	  glVertex3f(-0.501 - ythick, 0.0, -0.501 - xthick);
	  glVertex3f(0.50, 0.0, -0.501 - xthick);
	  glVertex3f(0.50, 0.0, -0.501);
	  glVertex3f(-0.501 - ythick, 0.0, -0.501);
	  glEnd();
	}
      else 
#endif
	{
	  fill_rectangle(ax, ap->x_axis_x0, ap->x_axis_y0, (uint32_t)(ap->x_axis_x1 - ap->x_axis_x0), axis_thickness);
	}
    }
  
  /* y axis */
  if ((axes != SHOW_X_AXIS) && (axes != SHOW_X_AXIS_UNLABELLED))
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  glBegin(GL_POLYGON);
	  glColor3f(0.0, 0.0, 0.0);
	  glVertex3f(-0.501 - ythick, 0.0, -0.501 - xthick);
	  glVertex3f(-0.501, 0.0, -0.501 - xthick);
	  glVertex3f(-0.501, 0.0, 0.50);
	  glVertex3f(-0.501 - ythick, 0.0, 0.50);
	  glEnd();

	  /* draw rotated text here doesn't look very good -- the code in Mesa/progs/xdemos/xrotfontdemo.c uses
	   *   Mesa/progs/xdemos/xuserotfont.c which first sets up a bitmap very much like rotate_text in snd-xutils.c
	   *   then code much like the x axis label drawer above.
	   */
	}
      else
#endif
	{
#if (!USE_NO_GUI)
	  if ((ap->cp) && (ap->ylabel) && (include_y_tick_labels))
	    {
	      int y_label_width = 0;
	      y_label_width = label_width(ap->ylabel, use_tiny_font);
	      if ((ap->y_axis_y0 - ap->y_axis_y1) > (y_label_width + 20))
		draw_rotated_axis_label(ap->cp,	ax, ap->ylabel, 
					(tdy) ? 
#if USE_GTK
					    (ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width - 10) :
#else
					    (ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width) :
#endif
					    (ap->y_axis_x0 - inner_border_width - 30),
					/* tdy might be null if not y_axis_linear (sonogram + log-freq + y axis label) */
					(int)((ap->y_axis_y0 + ap->y_axis_y1 - y_label_width) * 0.5) - 8);
	    }
#endif
	  fill_rectangle(ax, ap->y_axis_x0, ap->y_axis_y1, axis_thickness, (uint32_t)(ap->y_axis_y0 - ap->y_axis_y1));
	}
    }
  
  if (printing) 
    {
      if (show_x_axis)
	ps_fill_rectangle(ap, ap->x_axis_x0, ap->x_axis_y0, ap->x_axis_x1 - ap->x_axis_x0, axis_thickness);
      if ((axes != SHOW_X_AXIS) && (axes != SHOW_X_AXIS_UNLABELLED))
	ps_fill_rectangle(ap, ap->y_axis_x0, ap->y_axis_y1, axis_thickness, ap->y_axis_y0 - ap->y_axis_y1);
    }
  
  /* linear axis ticks/labels */
  if ((include_y_tick_labels) || 
      (include_x_tick_labels))
    set_numbers_font(ax, printing, use_tiny_font);
  
  if ((y_axis_linear) && (include_y_tick_labels))
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  mus_float_t xl;
	  xl = -0.5 - ythick - ((mus_float_t)(3 * tdy->maj_tick_len + tdy->min_label_width + inner_border_width) / (mus_float_t)width);
	  glRasterPos3f(xl, 0.0, (tdy->mlo - ap->y0) / (ap->y1 - ap->y0) - 0.51);
	  glListBase(number_base);
  #if WITH_GL2PS
	  if (ss->gl_printing) gl2ps_text(tdy->min_label);
  #endif
	  glCallLists(mus_strlen(tdy->min_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdy->min_label));
	  
	  xl = -0.5 - ythick - ((mus_float_t)(3 * tdy->maj_tick_len + tdy->max_label_width + inner_border_width) / (mus_float_t)width);
	  glRasterPos3f(xl, 0.0, (tdy->mhi - ap->y0) / (ap->y1 - ap->y0) - 0.51);
	  glListBase(number_base);
  #if WITH_GL2PS
	  if (ss->gl_printing) gl2ps_text(tdy->max_label);
  #endif
	  glCallLists(mus_strlen(tdy->max_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdy->max_label));
	}
      else
#endif
	{
	  /* y axis numbers */
	  
	  draw_y_number(tdy->min_label,
			ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width,
			grf_y(tdy->mlo, ap), x_number_height,
			ap, ax, printing);
	  draw_y_number(tdy->max_label,
			ap->y_axis_x0 - tdy->maj_tick_len - tdy->max_label_width - inner_border_width,
			grf_y(tdy->mhi, ap), x_number_height,
			ap, ax, printing);
	}
    }

  if ((x_axis_linear) && (include_x_tick_labels))
    {
      int lx0, lx1, tx0, tx1;
      /* the label is at ap->x_label_x to that plus x_label_width */
      /* the number label widths are tdx->max|min_label_width */
      lx0 = ap->x_label_x;
      lx1 = lx0 + x_label_width;
      tx0 = (int)(tick_grf_x(tdx->mlo, ap, x_style, srate) - .45 * tdx->min_label_width);
      tx1 = tx0 + tdx->min_label_width;
      if ((lx0 > tx1) || (lx1 < tx0))
	{
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      mus_float_t yl;
	      yl = -0.5 - xthick - ((mus_float_t)(3 * major_tick_length + x_number_height + inner_border_width) / (mus_float_t)height);
	      glRasterPos3f((tdx->mlo - ap->x0) / (ap->x1 - ap->x0) - 0.53, 0.0, yl);
	      glListBase(number_base);
  #if WITH_GL2PS
	      if (ss->gl_printing) gl2ps_text(tdx->min_label);
  #endif
	      glCallLists(mus_strlen(tdx->min_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdx->min_label));
	    }
	  else
#endif
	    {
	      /* x axis min label */
	      draw_x_number(tdx->min_label, tx0, ap->x_axis_y0 + major_tick_length, x_number_height, ap, ax, printing);
	    }
	}
      tx0 = (int)(tick_grf_x(tdx->mhi, ap, x_style, srate) - (.45 * tdx->max_label_width)); /* try centered label first */
      if ((int)(tx0 + tdx->max_label_width) > ap->x_axis_x1)
	tx0 = (int)(tick_grf_x(tdx->mhi, ap, x_style, srate) - tdx->max_label_width + .75 * right_border_width);
      tx1 = tx0 + tdx->max_label_width;
      if ((lx0 > tx1) || (lx1 < tx0))
	{
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      mus_float_t yl;
	      yl = -0.5 - xthick - ((mus_float_t)(3 * major_tick_length + x_number_height + inner_border_width) / (mus_float_t)height);
	      glRasterPos3f((tdx->mhi - ap->x0) / (ap->x1 - ap->x0) - 0.53, 0.0, yl);
	      glListBase(number_base);
  #if WITH_GL2PS
	      if (ss->gl_printing) gl2ps_text(tdx->max_label);
  #endif
	      glCallLists(mus_strlen(tdx->max_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdx->max_label));
	    }
	  else
#endif
	    {
	      /* x axis max label */
	      draw_x_number(tdx->max_label, tx0, ap->x_axis_y0 + major_tick_length, x_number_height, ap, ax, printing);
	    }
	}
    }

  if ((y_axis_linear) && (include_y_ticks))
    {
      double fy, tens;
      int ty, x0, majx, minx, x;
      /* start ticks at flo, go to fhi by step, major ticks at mlo mhi and intervals of tenstep surrounding */
      x0 = ap->y_axis_x0;
      majx = x0 - tdy->maj_tick_len;
      minx = x0 - tdy->min_tick_len;
      fy = tdy->mlo;
      ty = grf_y(fy, ap);
#if HAVE_GL
      if (ap->use_gl)
	{
	  mus_float_t ypos;
	  ypos = (fy - ap->y0) / (ap->y1 - ap->y0) - 0.5;
	  glBegin(GL_LINES);
	  glVertex3f(-0.50 - ymajorlen, 0.0, ypos);
	  glVertex3f(-0.501, 0.0, ypos);
	  glEnd();
	}
      else
#endif
	{
	  draw_horizontal_tick(majx, x0, ty, ap, ax, printing, include_grid);
	}
      
      tens = 0.0;
      fy -= tdy->step;
      while (fy >= tdy->flo)
	{
	  tens += tdy->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      x = majx;
	    }
	  else x = minx;
	  ty = grf_y(fy, ap);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      mus_float_t ypos;
	      ypos = (fy - ap->y0) / (ap->y1 - ap->y0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(-0.50 - ((x == majx) ? ymajorlen : yminorlen), 0.0, ypos);
	      glVertex3f(-0.501, 0.0, ypos);
	      glEnd();
	    }
	  else
#endif
	    {
	      draw_horizontal_tick(x, x0, ty, ap, ax, printing, include_grid);
	    }
	  fy -= tdy->step;
	}
      tens = 0.0;
      fy = tdy->mlo;
      fy += tdy->step;
      while (fy <= tdy->fhi)
	{
	  tens += tdy->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      x = majx;
	    }
	  else x = minx;
	  ty = grf_y(fy, ap);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      mus_float_t ypos;
	      ypos = (fy - ap->y0) / (ap->y1 - ap->y0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(-0.50 - ((x == majx) ? ymajorlen : yminorlen), 0.0, ypos);
	      glVertex3f(-0.501, 0.0, ypos);
	      glEnd();
	    }
	  else
#endif
	    {
	      draw_horizontal_tick(x, x0, ty, ap, ax, printing, include_grid);
	    }
	  fy += tdy->step;
	}
    }
  if ((x_axis_linear) && (include_x_ticks))
    {
      bool major_tick_is_less_than_measure = false;
      double fx, tens;
      int tx, y0, majy, miny, y;
      y0 = ap->x_axis_y0;
      majy = y0 + tdx->maj_tick_len;
      miny = y0 + tdx->min_tick_len;
      /* start at leftmost major tick and work toward y axis */
      fx = tdx->mlo;
      tx = tick_grf_x(fx, ap, x_style, srate);
      
      if ((ap->cp) && 
	  (x_style == X_AXIS_IN_MEASURES) &&
	  ((tdx->tenstep * tdx->step) <= ((60.0 * ap->cp->beats_per_measure) / (mus_float_t)(ap->cp->beats_per_minute))))
	major_tick_is_less_than_measure = true;
      
#if HAVE_GL
      if (ap->use_gl)
	{
	  mus_float_t xpos;
	  xpos = (fx - ap->x0) / (ap->x1 - ap->x0) - 0.5;
	  glBegin(GL_LINES);
	  glVertex3f(xpos, 0.0, -0.50 - xmajorlen);
	  glVertex3f(xpos, 0.0, -0.501);
	  glEnd();
	}
      else
#endif
	{
	  if ((major_tick_is_less_than_measure) &&
	      (first_beat(ap->cp, fx)))
	    draw_vertical_tick(tx, y0 - major_tick_length, majy + minor_tick_length, ap, ax, printing, include_grid);
	  else draw_vertical_tick(tx, y0, majy, ap, ax, printing, include_grid);
	}
      tens = 0.0;
      fx -= tdx->step;
      while (fx >= tdx->flo)
	{
	  tens += tdx->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      y = majy;
	    }
	  else y = miny;
	  tx = tick_grf_x(fx, ap, x_style, srate);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      mus_float_t xpos;
	      xpos = (fx - ap->x0) / (ap->x1 - ap->x0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(xpos, 0.0, -0.50 - ((y == majy) ? xmajorlen : xminorlen));
	      glVertex3f(xpos, 0.0, -0.501);
	      glEnd();
	    }
	  else
#endif
	    {
	      if ((major_tick_is_less_than_measure) &&
		  (first_beat(ap->cp, fx)))
		draw_vertical_tick(tx, y0 - major_tick_length, y + minor_tick_length, ap, ax, printing, include_grid);
	      else draw_vertical_tick(tx, y0, y, ap, ax, printing, include_grid);
	    }
	  fx -= tdx->step;
	}
      tens = 0.0;
      fx = tdx->mlo;
      /* now start at leftmost major tick and work towards right end */
      fx += tdx->step;
      while (fx <= tdx->fhi)
	{
	  tens += tdx->tenstep;
	  if (tens == 10.0)
	    {
 	      tens = 0.0;
	      y = majy;
	    }
	  else y = miny;
	  tx = tick_grf_x(fx, ap, x_style, srate);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      mus_float_t xpos;
	      xpos = (fx - ap->x0) / (ap->x1 - ap->x0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(xpos, 0.0, -0.50 - ((y == majy) ? xmajorlen : xminorlen));
	      glVertex3f(xpos, 0.0, -0.501);
	      glEnd();
	    }
	  else
#endif
	    {
	      if ((major_tick_is_less_than_measure) &&
		  (first_beat(ap->cp, fx)))
		draw_vertical_tick(tx, y0 - major_tick_length, y + minor_tick_length, ap, ax, printing, include_grid);
	      else draw_vertical_tick(tx, y0, y, ap, ax, printing, include_grid);
	    }
	  fx += tdx->step;
	}
    }
  
  /* All linear axis stuff has been taken care of. Check for log axes (assume fft context, not spectrogram so no GL) */
  /* x axis overall label has already gone out */
  
  if ((!x_axis_linear) && 
      (show_x_axis) && 
      (include_x_ticks))
    {
      double min_freq, max_freq;
      mus_float_t minlx = 0.0, maxlx, fap_range, log_range, lscale = 1.0, curlx;
      int logx;
      int y0, majy, miny, i;
      const char *label = NULL;
      mus_float_t freq = 0.0, freq10 = 0.0;
      /* get min (log-freq or spectrum-start), max, add major ticks and brief labels, then if room add log-style minor ticks (100's, 1000's) */
      y0 = ap->x_axis_y0;
      majy = y0 + major_tick_length;
      miny = y0 + minor_tick_length;
      min_freq = ap->x0;
      max_freq = ap->x1;
      if (include_x_tick_labels)
	set_numbers_font(ax, printing, use_tiny_font);
      fap_range = max_freq - min_freq;
      if (min_freq > 1.0) minlx = log(min_freq); else minlx = 0.0;
      maxlx = log(max_freq);
      log_range = (maxlx - minlx);
      lscale = fap_range / log_range;
      for (i = 0; i < 3; i++)
	{
	  switch (i)
	    {
	    case 0:
	      label = "100";
	      freq = 100.0;
	      break;

	    case 1:
	      label = "1000";
	      freq = 1000.0;
	      break;

	    case 2:
	      label = "10000";
	      freq = 10000.0;
	      break;
	    }
	  if ((min_freq <= freq) && (max_freq >= freq))
	    {
	      /* draw major tick at freq */
	      freq10 = freq / 10.0;
	      logx = grf_x(min_freq + lscale * (log(freq) - minlx), ap);
	      draw_vertical_tick(logx, y0, majy, ap, ax, printing, include_grid);	      
	      draw_log_tick_label(label, logx, ap->x_axis_y0 + major_tick_length, x_number_height, x_label_width, right_border_width, 
				  ap, ax, printing, use_tiny_font);
	      if (width > 200)
		{
		  curlx = snd_round(min_freq / freq10) * freq10;
		  for (; curlx < freq; curlx += freq10)
		    {
		      logx = grf_x(min_freq + lscale * (log(curlx) - minlx), ap);
		      draw_vertical_tick(logx, y0, miny, ap, ax, printing, include_grid);	      
		    }
		}
	    }
	}
    }
  
  if ((!y_axis_linear) && 
      (axes != SHOW_X_AXIS) && 
      (axes != SHOW_X_AXIS_UNLABELLED) &&
      (include_y_ticks))
    {
      double min_freq, max_freq;
      mus_float_t minlx = 0.0, maxlx, fap_range, log_range, lscale = 1.0, curly;
      int logy;
      int x0, majx, minx, i;
      const char *label = NULL;
      mus_float_t freq = 0.0, freq10 = 0.0;
      /* get min (log-freq or spectrum-start), max, add major ticks and brief labels, then if room add log-style minor ticks (100's, 1000's) */
      x0 = ap->y_axis_x0;
      majx = x0 - major_tick_length;
      minx = x0 - minor_tick_length;
      min_freq = ap->y0;
      max_freq = ap->y1;
      if (include_y_tick_labels)
	set_numbers_font(ax, printing, use_tiny_font);
      fap_range = max_freq - min_freq;
      if (min_freq > 1.0) minlx = log(min_freq); else minlx = 0.0;
      maxlx = log(max_freq);
      log_range = (maxlx - minlx);
      lscale = fap_range / log_range;
      for (i = 0; i < 3; i++)
	{
	  switch (i)
	    {
	    case 0:
	      label = "100";
	      freq = 100.0;
	      break;

	    case 1:
	      label = "1000";
	      freq = 1000.0;
	      break;

	    case 2:
	      label = "10000";
	      freq = 10000.0;
	      break;
	    }
	  if ((min_freq <= freq) && (max_freq >= freq))
	    {
	      /* draw major tick at freq */
	      freq10 = freq / 10.0;
	      logy = grf_y(min_freq + lscale * (log(freq) - minlx), ap);
	      draw_horizontal_tick(majx, x0, logy, ap, ax, printing, include_grid);	      
	      if (include_y_tick_labels)
		{
		  int label_width;
		  label_width = number_width(label, use_tiny_font);
		  /* y axis number */
		  draw_y_number(label,
				ap->y_axis_x0 - major_tick_length - label_width - inner_border_width,
				logy, x_number_height,
				ap, ax, printing);
		}
	      if (height > 200)
		{
		  curly = snd_round(min_freq / freq10) * freq10;
		  for (; curly < freq; curly += freq10)
		    {
		      logy = grf_y(min_freq + lscale * (log(curly) - minlx), ap);
		      draw_horizontal_tick(minx, x0, logy, ap, ax, printing, include_grid);	      
		    }
		}
	    }
	}
    }
}


axis_info *make_axis_info (chan_info *cp, double xmin, double xmax, mus_float_t ymin, mus_float_t ymax, 
			   const char *xlabel, double x0, double x1, mus_float_t y0, mus_float_t y1, axis_info *old_ap)
{
  axis_info *ap;
  if (old_ap) 
    ap = old_ap;
  else
    {
      ap = (axis_info *)calloc(1, sizeof(axis_info));
      ap->cp = cp;
    }
  ap->xmin = xmin;
  ap->xmax = xmax;
  if (ap->xmin == ap->xmax) ap->xmax += .001;
  ap->ymin = ymin;
  ap->ymax = ymax;
  if ((xlabel) && 
      (!(mus_strcmp(xlabel, ap->xlabel))))
    {
      /* this apparently should leave the default_xlabel and ylabels alone */
      if (ap->xlabel) free(ap->xlabel);
      ap->xlabel = mus_strdup(xlabel);
    }
  ap->x0 = x0;
  ap->x1 = x1;
  if (ap->x0 == ap->x1) ap->x1 += .001;
  if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->y_offset = 0;
  ap->y_ambit = ap->ymax - ap->ymin;
  ap->x_ambit = ap->xmax - ap->xmin;
  return(ap);
}


#if (!USE_NO_GUI)

static axis_info *get_ap(chan_info *cp, axis_info_t ap_id, const char *caller)
{
  #define AXIS_INFO_ID_OK(Id)    (Id <= (int)LISP_AXIS_INFO)

  if (AXIS_INFO_ID_OK(ap_id))
    switch (ap_id)
      {
      case TIME_AXIS_INFO:      return(cp->axis);                              break;
      case TRANSFORM_AXIS_INFO: if (cp->fft) return(cp->fft->axis);            break;
      case LISP_AXIS_INFO:      if (cp->lisp_info) return(lisp_info_axis(cp)); break;
      }

  Xen_error(Xen_make_error_type("no-such-axis"),
	    Xen_list_6(((!(cp->squelch_update)) || (!(AXIS_INFO_ID_OK(ap_id)))) ?
		         C_string_to_Xen_string("~A: no such axis: ~A of sound ~A (~A), chan: ~A (axis should be " S_time_graph ", " S_lisp_graph ", or " S_transform_graph ")") :
		         C_string_to_Xen_string("~A: no such axis: ~A of sound ~A (~A), chan: ~A does not exist, probably because output is squelched"),
		       C_string_to_Xen_string(caller),
		       C_int_to_Xen_integer((int)(ap_id)),
		       C_int_to_Xen_sound(cp->sound->index),
		       C_string_to_Xen_string(cp->sound->short_filename),
		       C_int_to_Xen_integer(cp->chan)));
  return(NULL);
}

#define TO_C_AXIS_INFO(Snd, Chn, Ap, Caller) get_ap(get_cp(Snd, Chn, Caller), (Xen_is_integer(Ap)) ? (axis_info_t)Xen_integer_to_C_int(Ap) : TIME_AXIS_INFO, Caller)


static Xen g_x_to_position(Xen val, Xen snd, Xen chn, Xen ap)
{
  #define H_x_to_position "(" S_x_to_position " val :optional snd chn (ax " S_time_graph ")): x pixel loc of val"
  Xen_check_type(Xen_is_number(val), val, 1, S_x_to_position, "a number");
  Snd_assert_channel(S_x_to_position, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(ap), ap, 4, S_x_to_position, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_int_to_Xen_integer(grf_x(Xen_real_to_C_double(val),
				    TO_C_AXIS_INFO(snd, chn, ap, S_x_to_position))));
}


static Xen g_y_to_position(Xen val, Xen snd, Xen chn, Xen ap)
{
  #define H_y_to_position "(" S_y_to_position " val :optional snd chn (ax " S_time_graph ")): y pixel loc of val"
  Xen_check_type(Xen_is_number(val), val, 1, S_y_to_position, "a number");
  Snd_assert_channel(S_y_to_position, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(ap), ap, 4, S_y_to_position, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_int_to_Xen_integer(grf_y(Xen_real_to_C_double(val),
				    TO_C_AXIS_INFO(snd, chn, ap, S_y_to_position))));
}


static Xen g_position_to_x(Xen val, Xen snd, Xen chn, Xen ap)
{
  #define H_position_to_x "(" S_position_to_x " val :optional snd chn (ax " S_time_graph ")): x axis value corresponding to pixel val"
  Xen_check_type(Xen_is_integer(val), val, 1, S_position_to_x, "an integer");
  Snd_assert_channel(S_position_to_x, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(ap), ap, 4, S_position_to_x, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_double_to_Xen_real(ungrf_x(TO_C_AXIS_INFO(snd, chn, ap, S_position_to_x),
				      Xen_integer_to_C_int(val))));
}


static Xen g_position_to_y(Xen val, Xen snd, Xen chn, Xen ap)
{
  #define H_position_to_y "(" S_position_to_y " val :optional snd chn (ax " S_time_graph ")): y axis value corresponding to pixel val"
  Xen_check_type(Xen_is_integer(val), val, 1, S_position_to_y, "an integer");
  Snd_assert_channel(S_position_to_y, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(ap), ap, 4, S_position_to_y, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_double_to_Xen_real(ungrf_y(TO_C_AXIS_INFO(snd, chn, ap, S_position_to_y),
				      Xen_integer_to_C_int(val))));
}


static Xen g_axis_info(Xen snd, Xen chn, Xen ap_id)
{
  #define H_axis_info "(" S_axis_info " :optional snd chn (ax " S_time_graph ")): info about axis: (list losamp hisamp \
x0 y0 x1 y1 xmin ymin xmax ymax pix_x0 pix_y0 pix_x1 pix_y1 y_offset xscale yscale xlabel ylabel new-peaks)"
  axis_info *ap;
  Snd_assert_channel(S_axis_info, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ap_id), ap_id, 3, S_axis_info, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ap_id, S_axis_info);
  if (!ap) return(Xen_empty_list);
  return(Xen_cons(C_llong_to_Xen_llong(ap->losamp),
	  Xen_cons(C_llong_to_Xen_llong(ap->hisamp),
	   Xen_cons(C_double_to_Xen_real(ap->x0),
	    Xen_cons(C_double_to_Xen_real(ap->y0),
	     Xen_cons(C_double_to_Xen_real(ap->x1),
	      Xen_cons(C_double_to_Xen_real(ap->y1),
	       Xen_cons(C_double_to_Xen_real(ap->xmin),
		Xen_cons(C_double_to_Xen_real(ap->ymin),
		 Xen_cons(C_double_to_Xen_real(ap->xmax),
		  Xen_cons(C_double_to_Xen_real(ap->ymax),
		   Xen_cons(C_int_to_Xen_integer(ap->x_axis_x0),
		    Xen_cons(C_int_to_Xen_integer(ap->y_axis_y0),
		     Xen_cons(C_int_to_Xen_integer(ap->x_axis_x1),
		      Xen_cons(C_int_to_Xen_integer(ap->y_axis_y1),
		       Xen_cons(C_int_to_Xen_integer(ap->y_offset),
			Xen_cons(C_double_to_Xen_real(ap->x_scale),
			 Xen_cons(C_double_to_Xen_real(ap->y_scale),
			  Xen_cons((ap->xlabel) ? C_string_to_Xen_string(ap->xlabel) : Xen_false,
			   Xen_cons((ap->ylabel) ? C_string_to_Xen_string(ap->ylabel) : Xen_false,
			    Xen_cons((ap->cp) ? C_bool_to_Xen_boolean(ap->cp->new_peaks) : Xen_false,
			     Xen_empty_list)))))))))))))))))))));
}


#if USE_MOTIF
  #define Xen_unwrap_snd_gc(Value) Xen_unwrap_C_pointer(Xen_cadr(Value))
  #define Xen_is_GC(Value) (Xen_is_list(Value) && (Xen_list_length(Value) >= 2) && \
                          (Xen_is_symbol(Xen_car(Value))) && \
			  (strcmp("GC", Xen_symbol_to_C_string(Xen_car(Value))) == 0))
#else
  #if USE_GTK
      #define Xen_unwrap_snd_gc(Value) (gc_t *)(Xen_unwrap_C_pointer(Xen_cadr(Value)))
      #define Xen_is_GC(Value) (Xen_is_list(Value) && (Xen_list_length(Value) >= 2) && \
                              (Xen_is_symbol(Xen_car(Value))) && \
			      (strcmp("gc_t_", Xen_symbol_to_C_string(Xen_car(Value))) == 0))
  #else
    #define Xen_unwrap_snd_gc(Value) 0
    #define Xen_is_GC(Value) 0
  #endif
#endif


static Xen g_draw_axes(Xen args)
{
  #define H_draw_axes "(" S_draw_axes " wid gc label (x0 0.0) (x1 1.0) (y0 -1.0) (y1 1.0) (style " S_x_axis_in_seconds ") (axes " S_show_all_axes ")): \
draws axes in the widget 'wid', using the graphics context 'gc', with the x-axis label 'label' \
going from x0 to x1 (floats) along the x axis, y0 to y1 along the y axis, with " S_x_axis_style " \
'style' (" S_x_axis_in_seconds " etc); the axes are actually displayed if 'axes' is " S_show_all_axes ". \
Returns actual (pixel) axis bounds -- a list (x0 y0 x1 y1)."

#if USE_MOTIF
  Widget w; 
  GC gc; 
#endif
#if USE_GTK
  GtkWidget *w; 
  gc_t *gc;
#endif

  Xen val, xwid, xgc, label_ref;
  double x0 = 0.0, x1 = 1.0; 
  mus_float_t y0 = -1.0, y1 = 1.0; 
  x_axis_style_t x_style = X_AXIS_IN_SECONDS;
  show_axes_t axes = SHOW_ALL_AXES;
  graphics_context *ax;
  axis_info *ap;
  int len;

  len = Xen_list_length(args);
#if (!USE_GTK)
  Xen_check_type((len >= 3) && (len < 10), args, 1, S_draw_axes, "3 required and 6 optional args");
#else
  Xen_check_type((len >= 3) && (len < 11), args, 1, S_draw_axes, "3 required and 7 optional args");
#endif
  
  xwid = Xen_list_ref(args, 0);
  Xen_check_type(Xen_is_widget(xwid), xwid, 1, S_draw_axes, "widget");
  xgc = Xen_list_ref(args, 1);
  Xen_check_type(Xen_is_GC(xgc), xgc, 2, S_draw_axes, "snd-gc");

#if USE_MOTIF
  w = (Widget)(Xen_unwrap_widget(xwid));
  gc = (GC)(Xen_unwrap_snd_gc(xgc));
#endif
#if USE_GTK
  w = (GtkWidget *)(Xen_unwrap_widget(xwid));
  gc = (gc_t *)(Xen_unwrap_snd_gc(xgc));
#endif

  label_ref = Xen_list_ref(args, 2);
  Xen_check_type(Xen_is_string(label_ref) || Xen_is_false(label_ref), label_ref, 3, S_draw_axes, "a string");
  if (len > 3) 
    {
      Xen xx0;
      xx0 = Xen_list_ref(args, 3);
      Xen_check_type(Xen_is_number(xx0), xx0, 4, S_draw_axes, "a number");
      x0 = Xen_real_to_C_double(xx0);
      if (len > 4) 
	{
	  Xen xx1;
	  xx1 = Xen_list_ref(args, 4);
	  Xen_check_type(Xen_is_number(xx1), xx1, 5, S_draw_axes, "a number");
	  x1 = Xen_real_to_C_double(xx1);
	  if (len > 5) 
	    {
	      Xen xy0;
	      xy0 = Xen_list_ref(args, 5);
	      Xen_check_type(Xen_is_number(xy0), xy0, 6, S_draw_axes, "a number");
	      y0 = Xen_real_to_C_double(xy0);
	      if (len > 6) 
		{
		  Xen xy1;
		  xy1 = Xen_list_ref(args, 6);
		  Xen_check_type(Xen_is_number(xy1), xy1, 7, S_draw_axes, "a number");
		  y1 = Xen_real_to_C_double(xy1);
		  if (len > 7) 
		    {
		      Xen xstyle;
		      int tmp;
		      xstyle = Xen_list_ref(args, 7);
		      Xen_check_type(Xen_is_integer(xstyle), xstyle, 8, S_draw_axes, "axis style");
		      tmp = Xen_integer_to_C_int(xstyle);
		      if (!(is_x_axis_style(tmp)))
			Xen_out_of_range_error(S_draw_axes, 7, xstyle, "axis style");
		      x_style = (x_axis_style_t)tmp;
		      if (len > 8) 
			{
			  Xen xaxes;
			  xaxes = Xen_list_ref(args, 8);
			  Xen_check_type(Xen_is_integer(xaxes), xaxes, 9, S_draw_axes, S_show_axes " choice");
			  tmp = Xen_integer_to_C_int(xaxes);
			  if (!(shows_axes(tmp)))
			    Xen_out_of_range_error(S_draw_axes, 8, xaxes, S_show_axes " choice");
			  axes = (show_axes_t)Xen_integer_to_C_int(xaxes);
#if USE_GTK
			  if (len > 9)
			    ss->cr = (cairo_t *)Xen_unwrap_C_pointer(Xen_cadr(Xen_list_ref(args, 9)));
#endif
			}}}}}}

  ap = (axis_info *)calloc(1, sizeof(axis_info));
  ax = (graphics_context *)calloc(1, sizeof(graphics_context));
  ap->ax = ax;

#if USE_MOTIF
  ax->dp = XtDisplay(w);
  ax->wn = XtWindow(w);
#endif

#if USE_GTK
  ax->wn = WIDGET_TO_WINDOW(w);
  ax->w = w;
  if (!ss->cr)
    Xen_error(Xen_make_error_type("no-cairo-t"),
	      Xen_list_1(C_string_to_Xen_string(S_draw_axes ": in Gtk, the trailing cairo_t argument is not optional")));
#endif

  ap->xmin = x0;
  ap->xmax = x1;
  ap->ymin = y0;
  ap->ymax = y1;
  ap->y_ambit = y1 - y0;
  ap->x_ambit = x1 - x0;
  if (Xen_is_string(label_ref))
    ap->xlabel = mus_strdup(Xen_string_to_C_string(label_ref));
  ap->x0 = x0;
  ap->x1 = x1;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->width = widget_width(w);
  ap->window_width = ap->width;
  ap->y_offset = 0;
  ap->height = widget_height(w);
  ap->graph_x0 = 0;
  clear_window(ax);
  ax->gc = gc;

  make_axes_1(ap, x_style, 1, axes, NOT_PRINTING, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));

  val = Xen_cons(C_int_to_Xen_integer(ap->x_axis_x0),
	 Xen_cons(C_int_to_Xen_integer(ap->y_axis_y0),
	  Xen_cons(C_int_to_Xen_integer(ap->x_axis_x1),
	   Xen_cons(C_int_to_Xen_integer(ap->y_axis_y1),
	    Xen_empty_list))));

  free_axis_info(ap);
  return(val);
}


static Xen g_x_axis_label(Xen snd, Xen chn, Xen ax)
{
  #define H_x_axis_label "(" S_x_axis_label " :optional snd chn (ax " S_time_graph ")): current x axis label"
  axis_info *ap;

  Snd_assert_channel(S_x_axis_label, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 3, S_x_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_x_axis_label);

  return(C_string_to_Xen_string(ap->xlabel));
}


static Xen g_set_x_axis_label(Xen label, Xen snd, Xen chn, Xen ax)
{
  axis_info *ap;

  Snd_assert_channel(S_set S_x_axis_label, snd, chn, 2);
  Xen_check_type(Xen_is_string(label) || Xen_is_false(label), label, 1, S_set S_x_axis_label, "a string");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_set S_x_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);

  ap = TO_C_AXIS_INFO(snd, chn, ax, S_x_axis_label);
  if (ap->xlabel) free(ap->xlabel);
  if (ap->default_xlabel) free(ap->default_xlabel);

  if (Xen_is_false(label))
    {
      ap->xlabel = NULL;
      ap->default_xlabel = NULL;
    }
  else
    {
      ap->xlabel = mus_strdup(Xen_string_to_C_string(label));
      if ((Xen_is_integer(ax)) && (Xen_integer_to_C_int(ax) == (int)TRANSFORM_AXIS_INFO))
	set_fft_info_xlabel(ap->cp, ap->xlabel);
      ap->default_xlabel = mus_strdup(ap->xlabel);
    }

  update_graph(ap->cp);
  return(label);
}

with_four_setter_args(g_set_x_axis_label_reversed, g_set_x_axis_label)
  

static Xen g_y_axis_label(Xen snd, Xen chn, Xen ax)
{
  #define H_y_axis_label "(" S_y_axis_label " :optional snd chn (ax " S_time_graph ")): current y axis label"
  axis_info *ap;

  Snd_assert_channel(S_y_axis_label, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 3, S_y_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_y_axis_label);

  return(C_string_to_Xen_string(ap->ylabel));
}

static Xen g_set_y_axis_label(Xen label, Xen snd, Xen chn, Xen ax)
{
  axis_info *ap;

  Snd_assert_channel(S_set S_y_axis_label, snd, chn, 2);
  Xen_check_type(Xen_is_string(label) || Xen_is_false(label), label, 1, S_set S_y_axis_label, "a string");

  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_set S_y_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_y_axis_label);
  if (ap->ylabel) free(ap->ylabel);

  if (Xen_is_false(label))
    ap->ylabel = NULL;
  else ap->ylabel = mus_strdup(Xen_string_to_C_string(label));
  update_graph(ap->cp);

  return(label);
}

with_four_setter_args(g_set_y_axis_label_reversed, g_set_y_axis_label)



static Xen g_x_bounds(Xen snd, Xen chn, Xen ax)
{
  #define H_x_bounds "(" S_x_bounds " :optional snd chn axis): a list (x0 x1) giving the current x axis bounds of snd channel chn"
  axis_info *ap;

  Snd_assert_channel(S_x_bounds, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_x_bounds, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_x_bounds);

  return(Xen_list_2(C_double_to_Xen_real(ap->x0),
		    C_double_to_Xen_real(ap->x1)));
  /* wavogram settings depend on context -- no easy way to map back to user's notion of bounds */
}


static Xen g_set_x_bounds(Xen bounds, Xen snd, Xen chn, Xen ax)
{
  chan_info *cp;
  axis_info *ap;
  mus_float_t x0 = 0.0, x1 = 0.0;

  Snd_assert_channel(S_set S_x_bounds, snd, chn, 2);
  Xen_check_type(Xen_is_number(bounds) || (Xen_is_list(bounds) && (Xen_list_length(bounds) == 2)), bounds, 1, S_set S_x_bounds, "a list: (x0 x1) or a number");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_set S_x_bounds, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_set S_x_bounds);

  cp = get_cp(snd, chn, S_set S_x_bounds);
  if (!cp) return(Xen_false);

  if (Xen_is_number(bounds))
    {
      x0 = 0.0;
      x1 = Xen_real_to_C_double(bounds);
    }
  else
    {
      x0 = Xen_real_to_C_double(Xen_car(bounds));
      x1 = Xen_real_to_C_double(Xen_cadr(bounds));
      if (x1 < x0)
	Xen_out_of_range_error(S_set S_x_bounds, 1, bounds, "x1 < x0?");
    }

  if (ap == cp->axis)
    {
      if (cp->time_graph_type == GRAPH_ONCE) 
	{
	  snd_info *sp;
	  set_x_axis_x0x1(cp, x0, x1);
	  sp = cp->sound;
	  if (sp->nchans > 1)
	    {
	      if ((!Xen_is_bound(chn)) && (cp->sound->channel_style == CHANNELS_COMBINED))
		{
		  uint32_t i;
		  for (i = 0; i < sp->nchans; i++)
		    if ((int)i != cp->chan)
		      set_x_axis_x0x1(sp->chans[i], x0, x1);
		  /* y-bounds are already tied together in the channels-combined case */
		}
	    }
	}
    }
  else
    {
      ap->x0 = x0;
      ap->x1 = x1;
    }
  return(bounds);
}

with_four_setter_args(g_set_x_bounds_reversed, g_set_x_bounds)



static Xen g_y_bounds(Xen snd, Xen chn, Xen ax)
{
  #define H_y_bounds "(" S_y_bounds " :optional snd chn axis): a list (y0 y1) giving the current y axis bounds of snd channel chn"
  axis_info *ap;

  Snd_assert_channel(S_y_bounds, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_y_bounds, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_y_bounds);

  return(Xen_list_2(C_double_to_Xen_real(ap->y0),
		    C_double_to_Xen_real(ap->y1)));
}


static Xen g_set_y_bounds(Xen bounds, Xen snd, Xen chn, Xen ax)
{
  chan_info *cp;
  axis_info *ap;
  mus_float_t low = 0.0, hi = 0.0;

  Snd_assert_channel(S_set S_y_bounds, snd, chn, 2);
  Xen_check_type((Xen_is_number(bounds)) || (Xen_is_list(bounds)), bounds, 1, S_set S_y_bounds, "a list or a number");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_set S_y_bounds, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_set S_y_bounds);

  cp = get_cp(snd, chn, S_set S_y_bounds);
  if (!cp) return(Xen_false);

  if (Xen_is_number(bounds))
    {
      hi = Xen_real_to_C_double(bounds);
      low = -hi;
    }
  else
    {
      int len;
      Xen y0 = Xen_undefined, y1 = Xen_undefined;
      len = Xen_list_length(bounds);
      if (len > 0)
	{
	  y0 = Xen_car(bounds);
	  if (len > 1)
	    y1 = Xen_cadr(bounds);
	}
      if (Xen_is_number(y0))
	{
	  low = Xen_real_to_C_double(y0);
	  if (Xen_is_number(y1))
	    hi = Xen_real_to_C_double(y1);
	  else
	    {
	      if (low < 0.0)
		hi = -low;
	      else
		{
		  hi = low;
		  low = -low;
		}
	    }
	}
      else
	{
	  if (ap == cp->axis)
	    {
	      /* if no bounds given, use maxamp */
	      hi = channel_maxamp(cp, AT_CURRENT_EDIT_POSITION);
	      if (hi < 0.0) hi = -hi;
	      if (hi == 0.0) hi = .001;
	      low = -hi;
	    }
	}
    }

  if (hi > low)
    {
      ap->ymin = low;
      ap->ymax = hi;
      ap->y_ambit = (ap->ymax - ap->ymin);
      ap->y0 = low;
      ap->y1 = hi;
      ap->zy = 1.0;
      ap->sy = 0.0;
      if (ap == cp->axis)
	{
	  resize_sy_and_zy(cp);
	  apply_y_axis_change(cp);
	}
    }

  return(bounds);
}

with_four_setter_args(g_set_y_bounds_reversed, g_set_y_bounds)
  

Xen_wrap_4_optional_args(g_x_to_position_w, g_x_to_position)
Xen_wrap_4_optional_args(g_y_to_position_w, g_y_to_position)
Xen_wrap_4_optional_args(g_position_to_x_w, g_position_to_x)
Xen_wrap_4_optional_args(g_position_to_y_w, g_position_to_y)
Xen_wrap_3_optional_args(g_axis_info_w, g_axis_info)
#if (!USE_NO_GUI)
Xen_wrap_any_args(g_draw_axes_w, g_draw_axes)
#endif
Xen_wrap_3_optional_args(g_x_axis_label_w, g_x_axis_label)
Xen_wrap_3_optional_args(g_y_axis_label_w, g_y_axis_label)
Xen_wrap_3_optional_args(g_x_bounds_w, g_x_bounds)
Xen_wrap_3_optional_args(g_y_bounds_w, g_y_bounds)
#if HAVE_SCHEME
#define g_set_x_axis_label_w g_set_x_axis_label_reversed
#define g_set_y_axis_label_w g_set_y_axis_label_reversed
#define g_set_x_bounds_w g_set_x_bounds_reversed
#define g_set_y_bounds_w g_set_y_bounds_reversed
#else
Xen_wrap_4_optional_args(g_set_x_axis_label_w, g_set_x_axis_label)
Xen_wrap_4_optional_args(g_set_y_axis_label_w, g_set_y_axis_label)
Xen_wrap_4_optional_args(g_set_x_bounds_w, g_set_x_bounds)
Xen_wrap_4_optional_args(g_set_y_bounds_w, g_set_y_bounds)
#endif

  
void g_init_axis(void)
{
#if HAVE_SCHEME
  s7_pointer i, p, t, f, r, s;
  i = s7_make_symbol(s7, "integer?");
  p = s7_make_symbol(s7, "pair?");
  f = s7_make_symbol(s7, "float?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  t = s7_t(s7);
#endif

  Xen_define_typed_procedure(S_x_to_position, g_x_to_position_w,   1, 3, 0, H_x_to_position, s7_make_signature(s7, 5, i, r, t, t, i));
  Xen_define_typed_procedure(S_y_to_position, g_y_to_position_w,   1, 3, 0, H_y_to_position, s7_make_signature(s7, 5, i, r, t, t, i));
  Xen_define_typed_procedure(S_position_to_x, g_position_to_x_w,   1, 3, 0, H_position_to_x, s7_make_signature(s7, 5, f, i, t, t, i));
  Xen_define_typed_procedure(S_position_to_y, g_position_to_y_w,   1, 3, 0, H_position_to_y, s7_make_signature(s7, 5, f, i, t, t, i));
  Xen_define_typed_procedure(S_axis_info,     g_axis_info_w,       0, 3, 0, H_axis_info,     s7_make_signature(s7, 4, p, t, t, i));
  Xen_define_typed_procedure(S_draw_axes,     g_draw_axes_w,       0, 0, 1, H_draw_axes,     s7_make_signature(s7, 11, p, t, t, t, r, r, r, r, i, i, p));
  
  Xen_define_typed_dilambda(S_x_axis_label, g_x_axis_label_w, H_x_axis_label, S_set S_x_axis_label, g_set_x_axis_label_w, 0, 3, 1, 3,
			    s7_make_signature(s7, 4, s, t, t, i), s7_make_signature(s7, 5, s, t, t, i, s));
  Xen_define_typed_dilambda(S_y_axis_label, g_y_axis_label_w, H_y_axis_label, S_set S_y_axis_label, g_set_y_axis_label_w, 0, 3, 1, 3,
			    s7_make_signature(s7, 4, s, t, t, i), s7_make_signature(s7, 5, s, t, t, i, s));
  Xen_define_typed_dilambda(S_x_bounds, g_x_bounds_w, H_x_bounds, S_set S_x_bounds, g_set_x_bounds_w, 0, 3, 1, 3,
			    s7_make_signature(s7, 4, p, t, t, t), s7_make_signature(s7, 5, p, t, t, t, t));
  Xen_define_typed_dilambda(S_y_bounds, g_y_bounds_w, H_y_bounds, S_set S_y_bounds, g_set_y_bounds_w, 0, 3, 1, 3,
			    s7_make_signature(s7, 4, p, t, t, t), s7_make_signature(s7, 5, p, t, t, t, t));

  Xen_define_constant(S_time_graph,      TIME_AXIS_INFO,      "time domain graph axis info");
  Xen_define_constant(S_transform_graph, TRANSFORM_AXIS_INFO, "frequency domain graph axis info");
  Xen_define_constant(S_lisp_graph,      LISP_AXIS_INFO,      "lisp graph axis info");
}

#endif
/* end no gui (covers entire xen section) */
/*   (if something is moved here, remember to add stubs to snd-nogui.c) */
