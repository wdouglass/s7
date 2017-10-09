#include "snd.h"

/* create Postscript version of graph */


static char *pbuf = NULL;
static int bbx = 0, bby = 0, bx0 = 0, by0 = 0;
static int ps_fd;

static char *nbuf = NULL;
static int nbuf_ctr = 0;
#define NBUF_SIZE 8192


static void ps_flush(int fd)
{
  if (nbuf_ctr > 0)
    {
      ssize_t bytes;
      bytes = write(fd, nbuf, nbuf_ctr);
      if (bytes == 0) fprintf(stderr, "ps_flush write error");
      nbuf_ctr = 0;
      memset((void *)nbuf, 0, NBUF_SIZE);
    }
}


static void ps_write(const char *buf)
{
  /* sending tiny buffers over the net is a total loss -- grab a bunch at a time */
  int i, len;
  if (!nbuf)
    {
      nbuf = (char *)calloc(NBUF_SIZE, sizeof(char));
      nbuf_ctr = 0;
    }
  len = mus_strlen(buf);
  for (i = 0; i < len; i++)
    {
      nbuf[nbuf_ctr++] = buf[i];
      if (nbuf_ctr == NBUF_SIZE) ps_flush(ps_fd);
    }
  memset((void *)buf, 0, PRINT_BUFFER_SIZE);
}


static int start_ps_graph(const char *output, const char *title) 
{ 
  ps_fd = CREAT(output, 0666);
  if (ps_fd == -1) return(-1);
  if (!pbuf) pbuf = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  bbx = 0;
  bby = 0;

  snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "%%!PS-Adobe-2.0 EPSF-2.0\n%%%%Title: %s\n%%%%Creator: Snd: %s\n%%%%CreationDate: %s", 
	       title, 
	       SND_DATE,
	       snd_local_time());
  ps_write(pbuf);
  snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "\n%%%%BoundingBox:(atend)\n%%%%EndComments\n%%%%EndProlog\n%%%%Page: 1 1\n");
  ps_write(pbuf);
  snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "/LT {lineto} bind def\n/RF {rectfill} bind def\n/RG {setrgbcolor} bind def\n/NAF {newpath arc fill} bind def\n\n");
  ps_write(pbuf);

  snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "gsave [%.3f 0.0 0.0 %.3f %.3f %.3f] concat\n\n",
	       eps_size(ss), eps_size(ss), eps_left_margin(ss), eps_bottom_margin(ss));
  ps_write(pbuf);
  return(0);
}


static void ps_graph(chan_info *cp, int x0, int y0)
{
  cp->printing = PRINTING;
  bx0 = x0;
  by0 = y0;
  display_channel_data(cp);
  cp->printing = NOT_PRINTING;
}


static void end_ps_graph(void)
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "%s\nshowpage\n%%%%Trailer\n%%%%BoundingBox: %d %d %d %d\n",
	       ((eps_left_margin(ss) != 0) || (eps_bottom_margin(ss) != 0)) ? "\ngrestore" : "",
	       0, 0,
	       (int)(bbx + 10 + eps_left_margin(ss)),
	       (int)(bby + 10 + eps_bottom_margin(ss)));
  ps_write(pbuf);
  ps_flush(ps_fd);
  snd_close(ps_fd, "eps file");
  if (nbuf)
    {
      free(nbuf);
      nbuf = NULL;
      nbuf_ctr = 0;
    }
}


/* the x and y values in the "points" are relative to grf_x/y:
 *
 *  x: ap->x_axis_x0 + (val - ap->x0) * ap->x_scale
 *  y: ap->y_axis_y0 + (val * MUS_FIX_TO_FLOAT - ap->y0) * ap->y_scale
 *
 * kept here in full precision since normally printers have much higher resolution than screens 
 */

static int reflect_y(axis_info *ap, int y)
{
  return(ap->height - y);
}


static mus_float_t *xpts = NULL;
static mus_float_t *ypts = NULL;
static mus_float_t *ypts1 = NULL;

void ps_allocate_grf_points(void)
{
  if (!xpts) xpts = (mus_float_t *)calloc(POINT_BUFFER_SIZE, sizeof(mus_float_t));
  if (!ypts) ypts = (mus_float_t *)calloc(POINT_BUFFER_SIZE, sizeof(mus_float_t));
  if (!ypts1) ypts1 = (mus_float_t *)calloc(POINT_BUFFER_SIZE, sizeof(mus_float_t));
}


void ps_set_grf_points(double x, int j, mus_float_t ymin, mus_float_t ymax) 
{
  xpts[j] = x;
  ypts[j] = ymin;
  ypts1[j] = ymax;
}


void ps_set_grf_point(double x, int j, mus_float_t y) 
{
  xpts[j] = x;
  ypts[j] = y;
}


static mus_float_t ps_grf_x(axis_info *ap, mus_float_t val)
{
  return(ap->x_axis_x0 + bx0 + (val - ap->x0) * ap->x_scale);
}


static mus_float_t ps_grf_y(axis_info *ap, mus_float_t val)
{
  return(by0 + ap->height - (ap->y_axis_y0 + (val - ap->y0) * ap->y_scale));
}


static void ps_draw_lines(axis_info *ap, int j, mus_float_t *xpts, mus_float_t *ypts)
{
  int i;
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[0]), ps_grf_y(ap, ypts[0]));
  ps_write(pbuf);
  for (i = 1; i < j; i++)
    {
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
      ps_write(pbuf);
    }
  snprintf(pbuf, PRINT_BUFFER_SIZE, " stroke\n");
  ps_write(pbuf);
}


static void ps_draw_dots(axis_info *ap, int j, mus_float_t *xpts, mus_float_t *ypts, int dot_size)
{
  int i;
  mus_float_t arc_size;
  arc_size = .5 * dot_size; /* radius here, diameter in X */
  for (i = 0; i < j; i++)
    {
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f 0 360 NAF\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]), arc_size);
      ps_write(pbuf);
    }
}


static void ps_fill_polygons(axis_info *ap, int j, mus_float_t *xpts, mus_float_t *ypts, mus_float_t y0)
{
  int i;
  for (i = 1; i < j; i++)
    {
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts[i - 1]));
      ps_write(pbuf);
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
      ps_write(pbuf);
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, y0));
      ps_write(pbuf);
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, y0));
      ps_write(pbuf);
      snprintf(pbuf, PRINT_BUFFER_SIZE, " closepath fill\n");
      ps_write(pbuf);
    }
}


void ps_draw_grf_points(axis_info *ap, int j, mus_float_t y0, graph_style_t graph_style, int dot_size) 
{
  switch (graph_style)
    {
    case GRAPH_LINES:
    default:
      ps_draw_lines(ap, j, xpts, ypts);
      break;
    case GRAPH_DOTS:
      ps_draw_dots(ap, j, xpts, ypts, dot_size);
      break;
    case GRAPH_FILLED:
      ps_fill_polygons(ap, j, xpts, ypts, y0);
      break;
    case GRAPH_DOTS_AND_LINES:
      ps_draw_lines(ap, j, xpts, ypts);
      if (dot_size > 1) ps_draw_dots(ap, j, xpts, ypts, dot_size);
      break;
    case GRAPH_LOLLIPOPS:
      {
	int i, gy0, size8, size4;
	if (dot_size > 1) ps_draw_dots(ap, j, xpts, ypts, dot_size);
	gy0 = (int)ps_grf_y(ap, y0);
	size8 = dot_size / 8;
	size4 = dot_size / 4;
	if (size4 < 1) size4 = 1;
	for (i = 0; i < j; i++)
	  {
	    snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f %.2f RF\n",
			 ps_grf_x(ap, xpts[i]) - size8,
			 (double)gy0,
			 (double)size4,
			 ps_grf_y(ap, ypts[i]) - gy0);
	    ps_write(pbuf);
	  }
      }
      break;
    }
}


void ps_draw_both_grf_points(axis_info *ap, int j, graph_style_t graph_style, int dot_size) 
{
  int i, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES:
    default:
      ps_draw_lines(ap, j, xpts, ypts);
      ps_draw_lines(ap, j, xpts, ypts1);
      break;
    case GRAPH_DOTS:
      ps_draw_dots(ap, j, xpts, ypts, dot_size);
      ps_draw_dots(ap, j, xpts, ypts1, dot_size);
      break;
    case GRAPH_FILLED:
      for (i = 1; i < j; i++)
	{
	  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts[i - 1]));
	  ps_write(pbuf);
	  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
	  ps_write(pbuf);
	  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts1[i]));
	  ps_write(pbuf);
	  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts1[i - 1]));
	  ps_write(pbuf);
	  snprintf(pbuf, PRINT_BUFFER_SIZE, " closepath fill\n");
	  ps_write(pbuf);
	}
      break;
    case GRAPH_DOTS_AND_LINES:
      if (dot_size > 1)
	{
	  ps_draw_dots(ap, j, xpts, ypts, dot_size);
	  ps_draw_dots(ap, j, xpts, ypts1, dot_size);
	}
      ps_draw_lines(ap, j, xpts, ypts);
      ps_draw_lines(ap, j, xpts, ypts1);
      break;
    case GRAPH_LOLLIPOPS:
      if (dot_size > 1)
	{
	  ps_draw_dots(ap, j, xpts, ypts, dot_size);
	  ps_draw_dots(ap, j, xpts, ypts1, dot_size);
	}
      size8 = dot_size / 8;
      size4 = dot_size / 4;
      if (size4 < 1) size4 = 1;
      for (i = 0; i < j; i++)
	{
	  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f %.2f RF\n",
		       ps_grf_x(ap, xpts[i]) - size8,
		       ps_grf_y(ap, ypts[i]),
		       (double)size4,
		       ps_grf_y(ap, ypts1[i]) - ps_grf_y(ap, ypts[i]));
	  ps_write(pbuf);
	}

      break;
    }
}


static int last_color = -1;

void ps_draw_sono_rectangle(axis_info *ap, int color, mus_float_t x, mus_float_t y, mus_float_t width, mus_float_t height)
{
  rgb_t r, g, b;
  if (last_color != color)
    {
      get_current_color(color_map(ss), color, &r, &g, &b);
      last_color = color;
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", rgb_to_float(r), rgb_to_float(g), rgb_to_float(b));
      ps_write(pbuf);
    }
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.1f %.1f %.2f %.2f RF\n", ps_grf_x(ap, x) + 2, ps_grf_y(ap, y), width, height);
  ps_write(pbuf);
}


void ps_reset_color(void)
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, " 0 setgray\n");
  ps_write(pbuf);
  last_color = -1;
}


#if USE_MOTIF || USE_GTK
static void ps_set_color(color_t color)
{
#if USE_MOTIF
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  dpy = XtDisplay(main_shell(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = color;
  XQueryColor(dpy, cmap, &tmp_color);
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n",
	       rgb_to_float(tmp_color.red),
	       rgb_to_float(tmp_color.green),
	       rgb_to_float(tmp_color.blue));
  ps_write(pbuf);
#else
  #if USE_GTK
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", 
	       rgb_to_float(color->red), 
	       rgb_to_float(color->green), 
	       rgb_to_float(color->blue));
  ps_write(pbuf);
  #endif
#endif
  last_color = -1;
}
#endif


void ps_bg(axis_info *ap, graphics_context *ax)
{
  /* get background color, fill graph, then set foreground for axis */
  chan_info *cp;
  cp = ap->cp;
#if USE_MOTIF
  {
    XGCValues gv;
    XGetGCValues(main_display(ss), ax->gc, GCBackground, &gv);
    ps_set_color(gv.background);
  }
#else
#if USE_GTK
  {
    if (cp->selected) 
      ps_set_color(ss->selected_graph_color);
    else ps_set_color(ss->graph_color);
  }
#endif
#endif
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d %d %d RF\n",
	       ap->graph_x0 + bx0, ap->y_offset + by0, ap->width, ap->height);
  ps_write(pbuf);
  ps_fg(cp, ax);
}


void ps_fg(chan_info *cp, graphics_context *ax)
{
  /* set foreground color for subsequent line drawing */
#if USE_MOTIF
  ps_set_color(get_foreground_color(ax));
#else
#if USE_GTK
  {
    if (cp->selected) 
      ps_set_color(ss->selected_data_color);
    else ps_set_color(ss->data_color);
  }
#endif
#endif
}


/* the rest are in real coordinates except upsidedown from PS point of view */

void ps_draw_line(axis_info *ap, int x0, int y0, int x1, int y1) 
{
  int py0, py1, px0, px1;
  px0 = x0 + bx0;
  px1 = x1 + bx0;
  py0 = reflect_y(ap, y0) + by0;
  py1 = reflect_y(ap, y1) + by0;
  if (px0 > bbx) bbx = px0;
  if (px1 > bbx) bbx = px1;
  if (py0 > bby) bby = py0;
  if (py1 > bby) bby = py1;
  snprintf(pbuf, PRINT_BUFFER_SIZE, " 0 setlinewidth %d %d moveto %d %d lineto stroke\n", px0, py0, px1, py1);
  ps_write(pbuf);
}


void ps_draw_spectro_line(axis_info *ap, int color, mus_float_t x0, mus_float_t y0, mus_float_t x1, mus_float_t y1)
{
  /* these are in local coords */
  rgb_t r, g, b;
  if (last_color != color)
    {
      get_current_color(color_map(ss), color, &r, &g, &b);
      last_color = color;
      snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", rgb_to_float(r), rgb_to_float(g), rgb_to_float(b));
      ps_write(pbuf);
    }
  ps_draw_line(ap, (int)x0, (int)y0, (int)x1, (int)y1);
}


void ps_fill_rectangle(axis_info *ap, int x0, int y0, int width, int height) 
{
  int py0, py1, px0, px1;
  px0 = x0 + bx0;
  px1 = x0 + bx0 + width;
  py0 = reflect_y(ap, y0) + by0;
  py1 = reflect_y(ap, y0 + height) + by0;
  if (px0 > bbx) bbx = px0;
  if (px1 > bbx) bbx = px1;
  if (py0 > bby) bby = py0;
  if (py1 > bby) bby = py1;
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d %d %d RF\n", px0, py0, width, -height);
  ps_write(pbuf);
}


void ps_draw_string(axis_info *ap, int x0, int y0, const char *str) 
{
  int px0, py0;
  px0 = x0 + bx0;
  py0 = reflect_y(ap, y0) + by0;
#if USE_GTK
  py0 -= 12;
#endif
  if (px0 > bbx) bbx = px0;
  if (py0 > bby) bby = py0;
  snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d moveto (%s) show\n", px0, py0, str);
  ps_write(pbuf);
}


void ps_set_number_font(void) 
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, " /Courier findfont 15 scalefont setfont\n");
  ps_write(pbuf);
}


void ps_set_label_font(void) 
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 20 scalefont setfont\n");
  ps_write(pbuf);
}


void ps_set_bold_peak_numbers_font(void) 
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Bold findfont 14 scalefont setfont\n");
  ps_write(pbuf);
}


void ps_set_peak_numbers_font(void) 
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 14 scalefont setfont\n");
  ps_write(pbuf);
}


void ps_set_tiny_numbers_font(void) 
{
  snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 12 scalefont setfont\n");
  ps_write(pbuf);
}


#define PRINTED_VERTICAL_SPACING 25 

static char *snd_print_or_error(const char *output)
{
  if ((output) && (*output))
    {
      int j, i, err;
      int *offsets = NULL;
      sync_info *si;
      chan_info *ccp;
      char *errstr = NULL;
      ccp = current_channel();
      if (!ccp) 
	return(mus_strdup("nothing to print?"));
      si = sync_to_chan(ccp);
      offsets = (int *)calloc(si->chans, sizeof(int));
      for (j = 0, i = (si->chans - 1); i >= 0; i--)
	{
	  offsets[i] = j;
	  j += ((((axis_info *)((si->cps[i])->axis))->height) + PRINTED_VERTICAL_SPACING);
	}
      if (si->chans > 1)
	for (i = 0; i < si->chans; )
	  {
	    snd_info *sp;
	    sp = (si->cps[i])->sound;
	    if (!sp) break;
	    if (sp->channel_style == CHANNELS_COMBINED)
	      for (j = i + 1; (j < i + (int)sp->nchans) && (j < si->chans); j++) 
		offsets[j] = offsets[i];
	    else
	      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
		for (j = i; (j < i + (int)sp->nchans - 1) && (j < si->chans); j++) 
		  offsets[j] = offsets[i + sp->nchans - 1];
	    i += sp->nchans;
	  }
      err = start_ps_graph(output, ((si->cps[0])->sound)->filename);
      if (err == 0)
	{
	  for (i = 0; i < si->chans; i++)
	    ps_graph(si->cps[i], 0, offsets[i]);
	  end_ps_graph();
	}
      else errstr = mus_format("print %s failed: %s", output, snd_io_strerror());
      free_sync_info(si);
      if (offsets) free(offsets);
      return(errstr);
    }
  else return(mus_strdup("print sound: eps file name needed"));
}


bool snd_print(const char *output)
{
  char *error;
  error = snd_print_or_error(output);
  if (error)
    {
      snd_error_without_format(error);
      free(error);
      return(false);
    }
  return(true);
}


void print_enved(const char *output, int y0)
{
  if ((output) && (*output))
    {
      int err;
      err = start_ps_graph(output, "Envelope Editor");
      if (err == 0)
	{
	  bx0 = 0;
	  by0 = y0;
	  env_redisplay_with_print();
	  end_ps_graph();
	}
      else snd_error("print env %s failed: %s", output, snd_io_strerror());
    }
  else snd_error_without_format("print envelope: eps file name needed");
}


static Xen g_graph_to_ps(Xen filename)
{
  #define H_graph_to_ps "(" S_graph_to_ps " :optional (filename eps-file)): write the current Snd displays to an EPS file"

  char *error;
  const char *file;

  Xen_check_type(Xen_is_string_or_unbound(filename), filename, 1, S_graph_to_ps, "a string (filename)");

  if (Xen_is_string(filename))
    file = Xen_string_to_C_string(filename);
  else file = eps_file(ss);

  error = snd_print_or_error(file);
  if (error)
    {
      Xen result;
      result = C_string_to_Xen_string(error);
      free(error);
      Xen_error(Xen_make_error_type("cannot-print"),
		Xen_list_3(C_string_to_Xen_string(S_graph_to_ps ": can't print ~S (~A)"),
			   C_string_to_Xen_string(file),
			   result));
    }
  return(C_string_to_Xen_string(file));
}


/* ---------------- gl -> ps ---------------- */

#if HAVE_GL && WITH_GL2PS

#include "gl2ps.h"

#define NUM_GL2PS_TYPES 6
static int gl2ps_types[NUM_GL2PS_TYPES] = {GL2PS_EPS, GL2PS_PS, GL2PS_PDF, GL2PS_TEX, GL2PS_SVG, GL2PS_PGF};


static Xen g_gl_graph_to_ps(Xen filename, Xen output_type, Xen snd, Xen chn)
{
  #define H_gl_graph_to_ps "(" S_gl_graph_to_ps " :optional filename (type 0) snd chn) produces a postscript output file from \
OpenGL graphics. type can be 0: eps, 1: ps, 2: pdf, 3: tex, 4: svg, 5: pgf."

  const char *file;
  FILE *fp;
  chan_info *cp;
  int state = GL2PS_OVERFLOW, buffsize = 1024 * 1024, type = 0;

  Xen_check_type(Xen_is_string_or_unbound(filename), filename, 1, S_gl_graph_to_ps, "a string (filename)");
  Xen_check_type(Xen_is_integer_or_unbound(output_type), output_type, 2, S_gl_graph_to_ps, "an integer, 0=eps");

  Snd_assert_channel(S_gl_graph_to_ps, snd, chn, 3);
  cp = get_cp(snd, chn, S_gl_graph_to_ps);
  if (!cp) return(Xen_false);

  if (Xen_is_string(filename))
    file = Xen_string_to_C_string(filename);
  else file = eps_file(ss);

  if (Xen_is_integer(output_type))
    type = Xen_integer_to_C_int(output_type);
  if ((type < 0) || (type >= NUM_GL2PS_TYPES))
    Xen_out_of_range_error(S_gl_graph_to_ps, 2, output_type, "must be between 0 and 5");

  fp = fopen(file, "wb");

  while (state == GL2PS_OVERFLOW)
    {
      GLint err;
      buffsize += 1024 * 1024;
      err = gl2psBeginPage(cp->sound->short_filename, "Snd", NULL, 
			   gl2ps_types[type],
			   GL2PS_BSP_SORT,
			   GL2PS_DRAW_BACKGROUND | GL2PS_USE_CURRENT_VIEWPORT | GL2PS_OCCLUSION_CULL, /* perhaps also GL2PS_NO_BLENDING */
			   GL_RGBA, 0, NULL, 0, 0, 0, buffsize, fp, file);
      if (err != GL2PS_SUCCESS) 
	{
	  /* if an error occurs, gl2ps itself insists on printing something -- this needs to be fixed! */
	  state = (int)err;
	}
      else
	{
	  ss->gl_printing = true;
	  display_channel_fft_data(cp);
	  ss->gl_printing = false;

	  state = gl2psEndPage();
	}
    }
  fclose(fp);
  
  return(C_string_to_Xen_string(file));
}
  

char *gl2ps_version(void);
char *gl2ps_version(void)
{
  char *buf;
  buf = (char *)calloc(128, sizeof(char));
  snprintf(buf, 128, "gl2ps %d.%d.%d", GL2PS_MAJOR_VERSION, GL2PS_MINOR_VERSION, GL2PS_PATCH_VERSION);
  return(buf);
}


void gl2ps_text(const char *msg);
void gl2ps_text(const char *msg)
{
  gl2psText(msg, "Times-Roman", 20);
}

#else

static Xen g_gl_graph_to_ps(Xen filename, Xen output_type, Xen snd_ignore, Xen chn_ignore)
{
  #define H_gl_graph_to_ps "gl-graph->ps is a no-op in this version of Snd"
  Xen_check_type(Xen_is_string_or_unbound(filename), filename, 1, S_gl_graph_to_ps, "a string (filename)");
  Xen_check_type(Xen_is_integer_or_unbound(output_type), output_type, 2, S_gl_graph_to_ps, "an integer, 0=eps");
  return(Xen_false);
}
#endif

/* -------------------------------- */


static Xen g_eps_file(void) {return(C_string_to_Xen_string(eps_file(ss)));}

static Xen g_set_eps_file(Xen val) 
{
  #define H_eps_file "(" S_eps_file "): File:Print and " S_graph_to_ps " file name (snd.eps)"
  Xen_check_type(Xen_is_string(val), val, 1, S_set S_eps_file, "a string"); 
  if (eps_file(ss)) free(eps_file(ss));
  set_eps_file(mus_strdup(Xen_string_to_C_string(val))); 
  return(C_string_to_Xen_string(eps_file(ss)));
}


#define MAX_EPS_MARGIN 1000.0

static Xen g_eps_left_margin(void) {return(C_double_to_Xen_real(eps_left_margin(ss)));}

static Xen g_set_eps_left_margin(Xen val) 
{
  #define H_eps_left_margin "(" S_eps_left_margin "): File:Print and " S_graph_to_ps " left margin"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_eps_left_margin, "a number"); 
  set_eps_left_margin(mus_fclamp(0.0, Xen_real_to_C_double(val), MAX_EPS_MARGIN));
  return(C_double_to_Xen_real(eps_left_margin(ss)));
}


static Xen g_eps_bottom_margin(void) {return(C_double_to_Xen_real(eps_bottom_margin(ss)));}

static Xen g_set_eps_bottom_margin(Xen val) 
{
  #define H_eps_bottom_margin "(" S_eps_bottom_margin "): File:Print and " S_graph_to_ps " bottom margin"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_eps_bottom_margin, "a number"); 
  set_eps_bottom_margin(mus_fclamp(0.0, Xen_real_to_C_double(val), MAX_EPS_MARGIN));
  return(C_double_to_Xen_real(eps_bottom_margin(ss)));
}


static Xen g_eps_size(void) {return(C_double_to_Xen_real(eps_size(ss)));}

static Xen g_set_eps_size(Xen val) 
{
  #define MAX_EPS_SIZE 1000.0
  #define H_eps_size "(" S_eps_size "): File:Print and " S_graph_to_ps " output size scaler (1.0)"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_eps_size, "a number"); 
  set_eps_size(mus_fclamp(0.0, Xen_real_to_C_double(val), MAX_EPS_SIZE));
  return(C_double_to_Xen_real(eps_size(ss)));
}


Xen_wrap_1_optional_arg(g_graph_to_ps_w, g_graph_to_ps)
Xen_wrap_4_optional_args(g_gl_graph_to_ps_w, g_gl_graph_to_ps)
Xen_wrap_no_args(g_eps_file_w, g_eps_file)
Xen_wrap_1_arg(g_set_eps_file_w, g_set_eps_file)
Xen_wrap_no_args(g_eps_left_margin_w, g_eps_left_margin)
Xen_wrap_1_arg(g_set_eps_left_margin_w, g_set_eps_left_margin)
Xen_wrap_no_args(g_eps_size_w, g_eps_size)
Xen_wrap_1_arg(g_set_eps_size_w, g_set_eps_size)
Xen_wrap_no_args(g_eps_bottom_margin_w, g_eps_bottom_margin)
Xen_wrap_1_arg(g_set_eps_bottom_margin_w, g_set_eps_bottom_margin)

#if HAVE_SCHEME
static s7_pointer acc_eps_bottom_margin(s7_scheme *sc, s7_pointer args) {return(g_set_eps_bottom_margin(s7_cadr(args)));}
static s7_pointer acc_eps_file(s7_scheme *sc, s7_pointer args) {return(g_set_eps_file(s7_cadr(args)));}
static s7_pointer acc_eps_left_margin(s7_scheme *sc, s7_pointer args) {return(g_set_eps_left_margin(s7_cadr(args)));}
static s7_pointer acc_eps_size(s7_scheme *sc, s7_pointer args) {return(g_set_eps_size(s7_cadr(args)));}
#endif

void g_init_print(void)
{
#if HAVE_SCHEME
  s7_pointer i, t, r, s, pcl_s, pcl_r;
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  i = s7_make_symbol(s7, "integer?");
  t = s7_t(s7);
  pcl_s = s7_make_circular_signature(s7, 0, 1, s);
  pcl_r = s7_make_circular_signature(s7, 0, 1, r);
#endif

  Xen_define_typed_procedure(S_graph_to_ps, g_graph_to_ps_w, 0, 1, 0, H_graph_to_ps, pcl_s);
  Xen_define_typed_procedure(S_gl_graph_to_ps, g_gl_graph_to_ps_w, 0, 4, 0, H_gl_graph_to_ps, s7_make_signature(s7, 5, s, s, i, t, t));

  Xen_define_typed_dilambda(S_eps_file, g_eps_file_w, H_eps_file, 
			    S_set S_eps_file, g_set_eps_file_w,  0, 0, 1, 0, pcl_s, pcl_s);

  Xen_define_typed_dilambda(S_eps_left_margin, g_eps_left_margin_w, H_eps_left_margin,
			    S_set S_eps_left_margin, g_set_eps_left_margin_w,  0, 0, 1, 0, pcl_r, pcl_r);
  
  Xen_define_typed_dilambda(S_eps_bottom_margin, g_eps_bottom_margin_w, H_eps_bottom_margin,
			    S_set S_eps_bottom_margin, g_set_eps_bottom_margin_w,  0, 0, 1, 0, pcl_r, pcl_r);

  Xen_define_typed_dilambda(S_eps_size, g_eps_size_w, H_eps_size,
			    S_set S_eps_size, g_set_eps_size_w,  0, 0, 1, 0, pcl_r, pcl_r);

#if HAVE_GL && WITH_GL2PS
  Xen_provide_feature("gl2ps");
#endif

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->eps_bottom_margin_symbol, s7_make_function(s7, "[acc-" S_eps_bottom_margin "]", acc_eps_bottom_margin, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->eps_file_symbol, s7_make_function(s7, "[acc-" S_eps_file "]", acc_eps_file, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->eps_left_margin_symbol, s7_make_function(s7, "[acc-" S_eps_left_margin "]", acc_eps_left_margin, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->eps_size_symbol, s7_make_function(s7, "[acc-" S_eps_size "]", acc_eps_size, 2, 0, false, "accessor"));

  s7_symbol_set_documentation(s7, ss->eps_bottom_margin_symbol, "*eps-bottom-margin*: File:Print and graph->ps bottom margin");
  s7_symbol_set_documentation(s7, ss->eps_file_symbol, "*eps-file*: File:Print and graph->ps file name (snd.eps)");
  s7_symbol_set_documentation(s7, ss->eps_left_margin_symbol, "*eps-left-margin*: File:Print and graph->ps left margin");
  s7_symbol_set_documentation(s7, ss->eps_size_symbol, "*eps-size*: File:Print and graph->ps output size scaler (1.0)");
#endif
}
