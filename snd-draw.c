#include "snd.h"


#if (!USE_NO_GUI)

/* our "current path" */
static point_t points[POINT_BUFFER_SIZE];
static point_t points1[POINT_BUFFER_SIZE];

void set_grf_points(int xi, int j, int ymin, int ymax)
{
  points[j].x = xi;
  points1[j].x = xi;
  points[j].y = ymax;
  points1[j].y = ymin;
}


void set_grf_point(int xi, int j, int yi)
{
  points[j].x = xi;
  points[j].y = yi;
}


point_t *get_grf_points(void) {return(points);} 

point_t *get_grf_points1(void) {return(points1);}


void draw_both_grf_points(int dot_size, graphics_context *ax, int j, graph_style_t graph_style)
{
  int i;
  switch (graph_style)
    {
    case GRAPH_LINES:
    default:
      draw_lines(ax, points, j);
      draw_lines(ax, points1, j);
      break;

    case GRAPH_DOTS:
      draw_points(ax, points, j, dot_size);
      draw_points(ax, points1, j, dot_size);
      break;

    case GRAPH_FILLED:
      fill_two_sided_polygons(ax, points, points1, j);
      break;

    case GRAPH_DOTS_AND_LINES:
      if (dot_size > 1)
	{
	  draw_points(ax, points, j, dot_size);
	  draw_points(ax, points1, j, dot_size);
	}
      draw_lines(ax, points, j);
      draw_lines(ax, points1, j);
      break;

    case GRAPH_LOLLIPOPS:
      if (dot_size == 1)
	{
	  for (i = 0; i < j; i++)
	    draw_line(ax, points[i].x, points[i].y, points1[i].x, points1[i].y);
	}
      else
	{
	  int size8, size4;
	  size8 = dot_size / 8;
	  size4 = dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, dot_size);
	  draw_points(ax, points1, j, dot_size);
	  for (i = 0; i < j; i++)
	    fill_rectangle(ax, points[i].x - size8, points[i].y, size4, points1[i].y - points[i].y);
	}
      break;
    }
}


void draw_grf_points(int dot_size, graphics_context *ax, int j, axis_info *ap, mus_float_t y0, graph_style_t graph_style)
{
  int i, gy0;
  switch (graph_style)
    {
    case GRAPH_LINES:
    default:
      draw_lines(ax, points, j); 
      break;

    case GRAPH_DOTS: 
      draw_points(ax, points, j, dot_size); 
      break;

    case GRAPH_FILLED: 
      fill_polygons(ax, points, j, grf_y(y0, ap)); 
      break;

    case GRAPH_DOTS_AND_LINES: 
      if (dot_size > 1) 
	draw_points(ax, points, j, dot_size); 
      draw_lines(ax, points, j); 
      break;

    case GRAPH_LOLLIPOPS:
      gy0 = grf_y(y0, ap);
      if (dot_size == 1)
	for (i = 0; i < j; i++)
	  draw_line(ax, points[i].x, points[i].y, points[i].x, gy0);
      else
	{
	  int size8, size4;
	  size8 = dot_size / 8;
	  size4 = dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, dot_size);
	  for (i = 0; i < j; i++)
	    if (points[i].y > gy0)
	      fill_rectangle(ax, points[i].x - size8, gy0, size4, points[i].y - gy0);
	    else fill_rectangle(ax, points[i].x - size8, points[i].y, size4, gy0 - points[i].y);
	}
      break;
    }
}


void draw_cursor(chan_info *cp)
{
  cursor_style_t cur;
#if USE_GTK
  color_t old_color;
#endif
  axis_info *ap;
  graphics_context *ax;

  if (!(cp->graph_time_on)) return;
  ap = cp->axis;

#if USE_GTK
  ax = ap->ax;
  if (!ax)
    {
      fprintf(stderr,"axis->ax is null...");
      ap->ax = cp->ax;
      ax = ap->ax;
    }
  old_color = get_foreground_color(ax);
  set_foreground_color(ax, ss->cursor_color);

  /* if (cp->cx > cp->cursor_size) cx0 = cp->cx - cp->cursor_size; */
  /* if (cp->cy > cp->cursor_size) cy0 = cp->cy - cp->cursor_size; */
  /* csize = 2 * cp->cursor_size + 1; */
  /* what were those lines for? */
#else
  ax = cursor_context(cp);
#endif

  if ((ss->tracking) && (cp->sound->playing))
    cur = cp->tracking_cursor_style;
  else cur = cp->cursor_style;

  switch (cur)
    {
    case CURSOR_CROSS:
      draw_line(ax, cp->cx, cp->cy - cp->cursor_size, cp->cx, cp->cy + cp->cursor_size);
      draw_line(ax, cp->cx - cp->cursor_size, cp->cy, cp->cx + cp->cursor_size, cp->cy);
      break;

    case CURSOR_LINE:
      if ((with_inset_graph(ss)) &&
	  (cp->inset_graph))
	draw_inset_line_cursor(cp, ax);
      else draw_line(ax, cp->cx, ap->y_axis_y0 - 1, cp->cx, ap->y_axis_y1);
      break;

    case CURSOR_PROC:
#if USE_GTK
      free_cairo(ss->cr);
      ss->cr = NULL;
#endif
      Xen_call_with_3_args((Xen_is_procedure(cp->cursor_proc)) ? (cp->cursor_proc) : (ss->cursor_proc),
		 C_int_to_Xen_sound(cp->sound->index),
		 C_int_to_Xen_integer(cp->chan),
		 /* this was time-graph, which was useless. It's now #t if we're in tracking-cursor mode */
		 C_bool_to_Xen_boolean(ss->tracking),
		 S_cursor_style " procedure");
#if USE_GTK
      ss->cr = make_cairo(ap->ax->wn);
      copy_context(cp);
#endif
      break;
    }

  /* now draw the play triangle below the x axis */
  fill_polygon(ax, 4,
	       cp->cx, ap->y_axis_y0,
	       cp->cx + play_arrow_size(ss), ap->y_axis_y0 + play_arrow_size(ss),
	       cp->cx, ap->y_axis_y0 + 2 * play_arrow_size(ss),
	       cp->cx, ap->y_axis_y0);

#if USE_GTK
  set_foreground_color(ax, old_color);
#endif
}



/* -------------------------------------------------------------------------------- */

#define AXIS_CONTEXT_ID_OK(Id) ((Id >= CHAN_GC) && (Id <= CHAN_TMPGC))
#define NO_SUCH_WIDGET Xen_make_error_type("no-such-widget")

#if USE_MOTIF
static graphics_context *get_ax(chan_info *cp, int ax_id, const char *caller, Xen ignored)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    return(set_context(cp, (chan_gc_t)ax_id));

  Xen_error(Xen_make_error_type("no-such-graphics-context"),
	    Xen_list_6(C_string_to_Xen_string("~A: no such graphics context: ~A, sound index: ~A (~A), chan: ~A"),
		       C_string_to_Xen_string(caller),
		       C_int_to_Xen_integer(ax_id),
		       C_int_to_Xen_sound(cp->sound->index),
		       C_string_to_Xen_string(cp->sound->short_filename),
		       C_int_to_Xen_integer(cp->chan)));
  return(NULL);
}


static graphics_context *get_ax_no_cr(chan_info *cp, int ax_id, const char *caller)
{
  return(get_ax(cp,ax_id, caller, Xen_false));
}
#endif

#if USE_GTK
static graphics_context *get_ax(chan_info *cp, int ax_id, const char *caller, Xen xcr)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    {
      graphics_context *ax;
      ax = set_context(cp, (chan_gc_t)ax_id);
      /* (gdk_cairo_create (GDK_DRAWABLE (gtk_widget_get_window (car (channel-widgets 0 0)))))) -> '(cairo_t_ #<c_pointer 0x12bbca0>)
       *    (eq? (car hi) 'cairo_t_) -> #t
       */
      if ((Xen_is_list(xcr)) &&
	  (Xen_list_length(xcr) == 2) &&
	  (Xen_is_symbol(Xen_car(xcr))) &&
	  (strcmp("cairo_t_", Xen_symbol_to_C_string(Xen_car(xcr))) == 0))
	ss->cr = (cairo_t *)Xen_unwrap_C_pointer(Xen_cadr(xcr));
      else 
	Xen_error(Xen_make_error_type("not-a-graphics-context"),
		  Xen_list_2(C_string_to_Xen_string("~A: cairo_t argument is not a cairo_t pointer"),
			     C_string_to_Xen_string(caller)));
      return(ax);
    }
  Xen_error(Xen_make_error_type("no-such-graphics-context"),
	    Xen_list_6(C_string_to_Xen_string("~A: no such graphics context: ~A, sound index: ~A (~A), chan: ~A"),
		       C_string_to_Xen_string(caller),
		       C_int_to_Xen_integer(ax_id),
		       C_int_to_Xen_sound(cp->sound->index),
		       C_string_to_Xen_string(cp->sound->short_filename),
		       C_int_to_Xen_integer(cp->chan)));
  return(NULL);
}

static graphics_context *get_ax_no_cr(chan_info *cp, int ax_id, const char *caller)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    {
      graphics_context *ax;
      ax = set_context(cp, (chan_gc_t)ax_id);
      return(ax);
    }
  Xen_error(Xen_make_error_type("no-such-graphics-context"),
	    Xen_list_6(C_string_to_Xen_string("~A: no such graphics context: ~A, sound index: ~A (~A), chan: ~A"),
		       C_string_to_Xen_string(caller),
		       C_int_to_Xen_integer(ax_id),
		       C_int_to_Xen_sound(cp->sound->index),
		       C_string_to_Xen_string(cp->sound->short_filename),
		       C_int_to_Xen_integer(cp->chan)));
  return(NULL);
}
#endif


#define TO_C_AXIS_CONTEXT(Snd, Chn, Ax, Caller, Cr) get_ax(get_cp(Snd, Chn, Caller), (Xen_is_integer(Ax)) ? Xen_integer_to_C_int(Ax) : (int)CHAN_GC, Caller, Cr)
#define TO_C_AXIS_CONTEXT_NO_CR(Snd, Chn, Ax, Caller) get_ax_no_cr(get_cp(Snd, Chn, Caller), (Xen_is_integer(Ax)) ? Xen_integer_to_C_int(Ax) : (int)CHAN_GC, Caller)


static Xen g_draw_line(Xen x0, Xen y0, Xen x1, Xen y1, Xen snd, Xen chn, Xen ax, Xen xcr)
{
  #define H_draw_line "(" S_draw_line " x0 y0 x1 y1 :optional snd chn (ax " S_time_graph ") cr): draw a line"

  Snd_assert_channel(S_draw_line, snd, chn, 5);
  Xen_check_type(Xen_is_integer(x0), x0, 1, S_draw_line, "an integer");
  Xen_check_type(Xen_is_integer(y0), y0, 2, S_draw_line, "an integer");
  Xen_check_type(Xen_is_integer(x1), x1, 3, S_draw_line, "an integer");
  Xen_check_type(Xen_is_integer(y1), y1, 4, S_draw_line, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 7, S_draw_line, "an integer such as " S_time_graph);

  draw_line(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_line, xcr),
	    Xen_integer_to_C_int(x0),
	    Xen_integer_to_C_int(y0),
	    Xen_integer_to_C_int(x1),
	    Xen_integer_to_C_int(y1));
  return(Xen_false);
}


static Xen g_draw_dot(Xen x0, Xen y0, Xen size, Xen snd, Xen chn, Xen ax, Xen xcr)
{
  #define H_draw_dot "(" S_draw_dot " x0 y0 size :optional snd chn (ax " S_time_graph ") cr): draw a dot"
 
  Snd_assert_channel(S_draw_dot, snd, chn, 4);
  Xen_check_type(Xen_is_integer(x0), x0, 1, S_draw_dot, "an integer");
  Xen_check_type(Xen_is_integer(y0), y0, 2, S_draw_dot, "an integer");
  Xen_check_type(Xen_is_integer(size), size, 3, S_draw_dot, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 6, S_draw_dot, "an integer such as " S_time_graph);

  draw_dot(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dot, xcr),
	   Xen_integer_to_C_int(x0),
	   Xen_integer_to_C_int(y0),
	   Xen_integer_to_C_int(size));
  return(Xen_false);
}


static Xen g_fill_rectangle(Xen x0, Xen y0, Xen width, Xen height, Xen snd, Xen chn, Xen ax, Xen erase, Xen xcr)
{
  #define H_fill_rectangle "(" S_fill_rectangle " x0 y0 width height :optional snd chn (ax " S_time_graph ") erase cr): draw a filled rectangle"

  Snd_assert_channel(S_fill_rectangle, snd, chn, 5);
  Xen_check_type(Xen_is_integer(x0), x0, 1, S_fill_rectangle, "an integer");
  Xen_check_type(Xen_is_integer(y0), y0, 2, S_fill_rectangle, "an integer");
  Xen_check_type(Xen_is_integer(width), width, 3, S_fill_rectangle, "an integer");
  Xen_check_type(Xen_is_integer(height), height, 4, S_fill_rectangle, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 7, S_fill_rectangle, "an integer such as " S_time_graph);
  Xen_check_type(Xen_is_boolean_or_unbound(erase), erase, 8, S_fill_rectangle, "a boolean");

  if ((Xen_is_boolean(erase)) &&
      (Xen_is_true(erase)))
    erase_rectangle(get_cp(snd, chn, S_fill_rectangle),
		    TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle, xcr),
		    Xen_integer_to_C_int(x0),
		    Xen_integer_to_C_int(y0),
		    Xen_integer_to_C_int(width),
		    Xen_integer_to_C_int(height));
  else fill_rectangle(TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle, xcr),
		      Xen_integer_to_C_int(x0),
		      Xen_integer_to_C_int(y0),
		      Xen_integer_to_C_int(width),
		      Xen_integer_to_C_int(height));
  return(Xen_false);
}


static Xen g_draw_string(Xen text, Xen x0, Xen y0, Xen snd, Xen chn, Xen ax, Xen xcr)
{
  #define H_draw_string "(" S_draw_string " text x0 y0 :optional snd chn (ax " S_time_graph ") cr): draw a string"
  
  const char *tmp = NULL;
  Snd_assert_channel(S_draw_string, snd, chn, 4);
  Xen_check_type(Xen_is_string(text), text, 1, S_draw_string, "a string");
  Xen_check_type(Xen_is_integer(x0), x0, 2, S_draw_string, "an integer");
  Xen_check_type(Xen_is_integer(y0), y0, 3, S_draw_string, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 6, S_draw_string, "an integer such as " S_time_graph);

  tmp = Xen_string_to_C_string(text);
#if USE_MOTIF
  /* snd-xdraw to make motif draw-string act in the same way (coordinate-wise) as gtk */
  /*   despite the name, this is not a gtk function */
  gtk_style_draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string, xcr),
#else
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string, xcr),
#endif
	      Xen_integer_to_C_int(x0),
	      Xen_integer_to_C_int(y0),
	      tmp,
	      mus_strlen(tmp));
  return(text);
}


static point_t *vector_to_points(Xen pts, const char *caller, int *vector_len)
{
  int i, j, vlen = 0, len = 0;
  point_t *pack_pts;

  vlen = Xen_vector_length(pts);
  if (vlen & 1)
    Xen_error(Xen_make_error_type("bad-length"),
	      Xen_list_3(C_string_to_Xen_string("~A: length of vector of points (~A) must be even"),
			 C_string_to_Xen_string(caller),
			 C_int_to_Xen_integer(vlen)));
  if (vlen <= 0) 
    Xen_error(NO_DATA,
	      Xen_list_3(C_string_to_Xen_string("~A: empty points vector? ~A"), 
			 C_string_to_Xen_string(caller), 
			 pts));
  len = vlen / 2;
  (*vector_len) = len;
  pack_pts = (point_t *)calloc(len, sizeof(point_t));

  for (i = 0, j = 0; i < len; i++, j += 2)
    {
      Xen p;
      p = Xen_vector_ref(pts, j);
      Xen_check_type(Xen_is_integer(p), p, j, caller, "an integer");
      pack_pts[i].x = Xen_integer_to_C_int(p);
      p = Xen_vector_ref(pts, j + 1);
      Xen_check_type(Xen_is_integer(p), p, j + 1, caller, "an integer");
      pack_pts[i].y = Xen_integer_to_C_int(p);
    }
  return(pack_pts);
}


  static Xen g_draw_lines(Xen pts, Xen snd, Xen chn, Xen ax, Xen xcr)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_lines "(" S_draw_lines " lines :optional snd chn (ax " S_time_graph ") cr): draw a vector of lines"

  point_t *pack_pts;
  graphics_context *ax1;
  int vlen = 0;

  Snd_assert_channel(S_draw_lines, snd, chn, 2);
  Xen_check_type(Xen_is_vector(pts), pts, 1, S_draw_lines, "a vector");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_draw_lines, "an integer such as " S_time_graph);

  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_lines, xcr);

  pack_pts = vector_to_points(pts, S_draw_lines, &vlen);
  draw_lines(ax1, pack_pts, vlen);

  free(pack_pts);
  return(pts);
}


  static Xen g_draw_dots(Xen pts, Xen size, Xen snd, Xen chn, Xen ax, Xen xcr)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_dots "(" S_draw_dots " positions :optional dot-size snd chn (ax " S_time_graph ") cr): draw a vector of dots"
 
  point_t *pack_pts;
  graphics_context *ax1;
  int vlen = 0;

  Snd_assert_channel(S_draw_dots, snd, chn, 3);
  Xen_check_type(Xen_is_vector(pts), pts, 1, S_draw_dots, "a vector");
  Xen_check_type(Xen_is_integer_or_unbound(size), size, 2, S_draw_dots, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 5, S_draw_dots, "an integer such as " S_time_graph);

  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dots, xcr);

  pack_pts = vector_to_points(pts, S_draw_dots, &vlen);
  draw_points(ax1,
	      pack_pts, 
	      vlen,
	      (Xen_is_integer(size)) ? Xen_integer_to_C_int(size) : 1);

  free(pack_pts);
  return(pts);
}


  static Xen g_fill_polygon(Xen pts, Xen snd, Xen chn, Xen ax_id, Xen xcr)
{ 
  #define H_fill_polygon "(" S_fill_polygon " points :optional snd chn (ax " S_time_graph ") cr): draw a filled polygon"

  point_t *pack_pts;
  graphics_context *ax;
  int vlen = 0;

  Snd_assert_channel(S_fill_polygon, snd, chn, 2);
  Xen_check_type(Xen_is_vector(pts), pts, 1, S_fill_polygon, "a vector");
  Xen_check_type(Xen_is_integer_or_unbound(ax_id), ax_id, 4, S_fill_polygon, "an integer such as " S_time_graph);

  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_fill_polygon, xcr);

  pack_pts = vector_to_points(pts, S_fill_polygon, &vlen);
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, vlen, Complex, CoordModeOrigin);
#else
  fill_polygon_from_array(ax, pack_pts, vlen);
#endif

  free(pack_pts);
  return(pts);
}


static Xen g_make_bezier(Xen args1)
{
  /* used in musglyphs.rb -- not currently used anywhere else */

  #define S_make_bezier "make-bezier"
  #define H_make_bezier "(" S_make_bezier " x0 y0 x1 y1 x2 y2 x3 y3 n): return a vector of points corresponding to the bezier curve \
defined by the 4 controlling points x0..y3; 'n' is how many points to return"
  /* XDrawBezier from cmn's glfed.c (where it was assuming PostScript coordinates) */

  int ax, ay, bx, by, cx, cy, i;
  float incr, val;
  int x[4];
  int y[4];
  int n = 50;
  Xen pts, args;

  args = Xen_copy_arg(args1);
  for (i = 0; i < 4; i++)
    {
      x[i] = Xen_integer_to_C_int(Xen_car(args));
      y[i] = Xen_integer_to_C_int(Xen_cadr(args));
      args = Xen_cddr(args);
    }
  if (!Xen_is_null(args)) 
    n = Xen_integer_to_C_int(Xen_car(args));

  cx = 3 * (x[1] - x[0]);
  cy = 3 * (y[1] - y[0]);
  bx = 3 * (x[2] - x[1]) - cx;
  by = 3 * (y[2] - y[1]) - cy;
  ax = x[3] - (x[0] + cx + bx);
  ay = y[3] - (y[0] + cy + by);
  incr = 1.0 / (double)n;
  pts = Xen_make_vector(2 * (n + 1), Xen_integer_zero);

  Xen_vector_set(pts, 0, C_int_to_Xen_integer(x[0]));
  Xen_vector_set(pts, 1, C_int_to_Xen_integer(y[0]));

  for (i = 1, val = incr; i <= n; i++, val += incr)
    {
      Xen_vector_set(pts, i * 2, C_int_to_Xen_integer((int)(x[0] + val * (cx + (val * (bx + (val * ax)))))));
      Xen_vector_set(pts, i * 2 + 1, C_int_to_Xen_integer((int)(y[0] + val * (cy + (val * (by + (val * ay)))))));
    }
  return(pts);
}


 static Xen g_foreground_color(Xen snd, Xen chn, Xen xax)
{
  #define H_foreground_color "(" S_foreground_color " :optional snd chn (ax " S_time_graph ")): current drawing color"
  chan_info *cp;
  graphics_context *ax;

  Snd_assert_channel(S_foreground_color, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(xax), xax, 3, S_foreground_color, "an integer");

  cp = get_cp(snd, chn, S_foreground_color);
  if (!cp) return(Xen_false);

  ax = get_ax_no_cr(cp, (Xen_is_integer(xax)) ? Xen_integer_to_C_int(xax) : (int)CHAN_GC, S_foreground_color);
  return(Xen_wrap_pixel(get_foreground_color(ax)));
}


 static Xen g_set_foreground_color(Xen color, Xen snd, Xen chn, Xen ax)
{
  chan_info *cp;

  Snd_assert_channel(S_set S_foreground_color, snd, chn, 2);
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_foreground_color, "a color");
  Xen_check_type(Xen_is_integer_or_unbound(ax), ax, 4, S_set S_foreground_color, "an integer");

  cp = get_cp(snd, chn, S_set S_foreground_color);
  if (!cp) return(Xen_false);

  set_foreground_color(get_ax_no_cr(cp, (Xen_is_integer(ax)) ? Xen_integer_to_C_int(ax) : (int)CHAN_GC, S_set S_foreground_color),
		       Xen_unwrap_pixel(color));
  return(color);
}

with_four_setter_args(g_set_foreground_color_reversed, g_set_foreground_color)


#if USE_MOTIF

static Xen g_set_current_font(Xen id, Xen snd, Xen chn, Xen ax_id)
{
  graphics_context *ax;

  Snd_assert_channel(S_set S_current_font, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(ax_id), ax_id, 4, S_set S_current_font, "an integer such as time-graph");
  Xen_check_type((Xen_is_list(id)) &&
		  (Xen_list_length(id) >= 2) &&
		  (Xen_is_symbol(Xen_car(id))) &&
		  (strcmp("Font", Xen_symbol_to_C_string(Xen_car(id))) == 0), id, 1, S_set S_current_font, "a Font");

  ax = TO_C_AXIS_CONTEXT_NO_CR(snd, chn, ax_id, S_current_font);
  ax->current_font = (Font)Xen_ulong_to_C_ulong(Xen_cadr(id));
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}


static Xen g_current_font(Xen snd, Xen chn, Xen ax_id)
{
  #define H_current_font "(" S_current_font " :optional snd chn (ax " S_time_graph ")): current font id"
  graphics_context *ax;
  chan_info *cp;

  Snd_assert_channel(S_current_font, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ax_id), ax_id, 3, S_current_font, "an integer such as time-graph");
  cp = get_cp(snd, chn, S_current_font);
  if (!cp) return(Xen_false);

  ax = get_ax_no_cr(cp, (Xen_is_integer(ax_id)) ? Xen_integer_to_C_int(ax_id) : (int)CHAN_GC, S_current_font);
  if (ax->current_font == 0)
    {
      if ((cp->axis) && (cp->axis->ax))
	return(Xen_list_2(C_string_to_Xen_symbol("Font"),
			  C_ulong_to_Xen_ulong(cp->axis->ax->current_font)));
      else return(Xen_false);
    }
  return(Xen_list_2(C_string_to_Xen_symbol("Font"),
		    C_ulong_to_Xen_ulong(ax->current_font)));
}


#else

static Xen g_set_current_font(Xen id, Xen snd, Xen chn, Xen ax_id)
{
  graphics_context *ax;

  Snd_assert_channel(S_set S_current_font, snd, chn, 2);
  Xen_check_type(Xen_is_integer_or_unbound(ax_id), ax_id, 4, S_set S_current_font, "an integer such as time-graph");

  ax = TO_C_AXIS_CONTEXT_NO_CR(snd, chn, ax_id, S_set S_current_font);
  Xen_check_type((Xen_is_wrapped_c_pointer(id)) ||
		  (Xen_is_list(id) && 
		   (Xen_list_length(id) >= 2) &&
		   (Xen_is_symbol(Xen_car(id)))),
		  id, 1, S_set S_current_font, "a wrapped object or a raw pointer");

  if (Xen_is_wrapped_c_pointer(id))
    ax->current_font = (PangoFontDescription *)Xen_unwrap_C_pointer(id); 
  else ax->current_font = (PangoFontDescription *)Xen_unwrap_C_pointer(Xen_cadr(id));
  return(id);
}


static Xen g_current_font(Xen snd, Xen chn, Xen ax_id)
{
  #define H_current_font "(" S_current_font " :optional snd chn (ax " S_time_graph ")): current font id"
  graphics_context *ax;
  Snd_assert_channel(S_current_font, snd, chn, 1);
  Xen_check_type(Xen_is_integer_or_unbound(ax_id), ax_id, 3, S_current_font, "an integer such as time-graph");
  ax = TO_C_AXIS_CONTEXT_NO_CR(snd, chn, ax_id, S_current_font);
  return(Xen_wrap_C_pointer(ax->current_font));
}

#endif

with_four_setter_args(g_set_current_font_reversed, g_set_current_font)

static Xen g_make_graph_data(Xen snd, Xen chn, Xen edpos, Xen lo, Xen hi)
{
  #define H_make_graph_data "(" S_make_graph_data " :optional snd chn edpos low high): \
return either a " S_vct " (if the graph has one trace), or a list of two " S_vct "s (the two sides of the envelope graph). \
'edpos' defaults to the " S_current_edit_position ", 'low' defaults to the current window left sample, and \
'high' defaults to the current rightmost sample. (" S_graph_data " (" S_make_graph_data ")) reimplements the time domain graph."

  chan_info *cp;

  Snd_assert_channel(S_make_graph_data, snd, chn, 1);
  cp = get_cp(snd, chn, S_make_graph_data);
  if (!cp) return(Xen_false);

  Xen_check_type(Xen_is_integer_or_unbound(lo), lo, 4, S_make_graph_data, "an integer");
  Xen_check_type(Xen_is_integer_or_unbound(hi), hi, 5, S_make_graph_data, "an integer");

  return(make_graph_data(cp,
			 to_c_edit_position(cp, edpos, S_make_graph_data, 3),
			 (Xen_is_llong(lo)) ? Xen_llong_to_C_llong(lo) : -1,
			 (Xen_is_llong(hi)) ? Xen_llong_to_C_llong(hi) : -1));
}


 static Xen g_graph_data(Xen data, Xen snd, Xen chn, Xen ax, Xen lo, Xen hi, Xen style, Xen xcr)
{
  #define H_graph_data "(" S_graph_data " data :optional snd chn (context " S_copy_context ") low high graph-style cr): \
display 'data' in the time domain graph of snd's channel chn using the graphics context context (normally " S_copy_context "), placing the \
data in the recipient's graph between points low and high in the drawing mode graphic-style."

  chan_info *cp;
  vct *v0, *v1 = NULL;

  Snd_assert_channel(S_graph_data, snd, chn, 2);

  cp = get_cp(snd, chn, S_graph_data);
  if (!cp) return(Xen_false);

  if (Xen_is_false(data)) return(Xen_false);
  Xen_check_type((Xen_is_list(data) && 
		   (Xen_list_length(data) == 2) &&
		   (mus_is_vct(Xen_car(data))) &&
		   (mus_is_vct(Xen_cadr(data)))) || 
		  (mus_is_vct(data)), 
		  data, 1, S_graph_data, "a list of 2 " S_vct "s or a " S_vct);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(ax), ax, 4, S_graph_data, "an integer");
  Xen_check_type(Xen_is_llong(lo) || Xen_is_false(lo) || !Xen_is_bound(lo), lo, 5, S_graph_data, "a sample number");
  Xen_check_type(Xen_is_llong(hi) || Xen_is_false(hi) || !Xen_is_bound(hi), hi, 6, S_graph_data, "a sample number");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(style), style, 7, S_graph_data, "an integer");

  if (Xen_is_list(data))
    {
      v0 = xen_to_vct(Xen_car(data));
      v1 = xen_to_vct(Xen_cadr(data));
    }
  else v0 = xen_to_vct(data);

  draw_graph_data(cp, 
		  (Xen_is_llong(lo)) ? Xen_llong_to_C_llong(lo) : -1,
		  (Xen_is_llong(hi)) ? Xen_llong_to_C_llong(hi) : -1,
		  mus_vct_length(v0),
		  mus_vct_data(v0),
		  (v1) ? (mus_vct_data(v1)) : NULL,
		  get_ax(cp, (Xen_is_integer(ax)) ? Xen_integer_to_C_int(ax) : (int)CHAN_GC, S_graph_data, xcr),
		  (Xen_is_integer(style)) ? (graph_style_t)Xen_integer_to_C_int(style) : cp->time_graph_style);
  return(data);
}


static Xen g_main_widgets(void)
{
  #define H_main_widgets "(" S_main_widgets "): top level \
widgets (list (0)main-app (1)main-shell (2)main-pane (3)sound-pane (4)listener-pane (5)notebook-outer-pane)"

  Xen bad_temp, res; /* needed by old gcc -- gets confused by straight arg list */
  int loc;
#if USE_MOTIF
  bad_temp = Xen_list_2(C_string_to_Xen_symbol("XtAppContext"), 
			C_ulong_to_Xen_ulong((unsigned long)main_app(ss)));
#else
  bad_temp = Xen_wrap_window(main_window(ss));
#endif
  loc = snd_protect(bad_temp);
  res = Xen_cons(bad_temp,
	   Xen_cons(Xen_wrap_widget(main_shell(ss)),
             Xen_cons(Xen_wrap_widget(main_pane(ss)),
               Xen_cons(Xen_wrap_widget(sound_pane(ss)),
		 Xen_cons(Xen_wrap_widget(ss->listener_pane),
		   Xen_cons(Xen_wrap_widget(sound_pane_box(ss)),
		     Xen_empty_list))))));
  snd_unprotect_at(loc);
  return(res);
}


static Xen dialog_widgets;
static Xen new_widget_hook;
/* ideally this would be an "after method" on XtCreateWidget or gtk_new_* */

void run_new_widget_hook(widget_t w)
{
  if (Xen_hook_has_list(new_widget_hook))
    run_hook(new_widget_hook, Xen_list_1(Xen_wrap_widget(w)), S_new_widget_hook);
}


static void check_dialog_widget_table(void)
{
  if (!(Xen_is_vector(dialog_widgets)))
    {
      dialog_widgets = Xen_make_vector(NUM_DIALOGS, Xen_false);
      Xen_GC_protect(dialog_widgets);
    }
}


static Xen g_dialog_widgets(void)
{
  #define H_dialog_widgets "(" S_dialog_widgets "): dialog widgets (each " PROC_FALSE " if not yet created): (list \
 (0 " S_color_orientation_dialog ") (1 " S_enved_dialog ") (2 " S_transform_dialog ") \
 (3 " S_open_file_dialog ") (4 " S_save_sound_dialog ") (5 " S_view_files_dialog ") (6 raw data dialog) (7 new file dialog) \
 (8 " S_mix_file_dialog ") (9 " S_edit_header_dialog ") (10 " S_find_dialog ") (11 " S_help_dialog ") \
 (12 " S_view_mixes_dialog ") (13 " S_print_dialog ") (14 " S_view_regions_dialog ") \
 (15 " S_info_dialog ") (16 extra controls dialog) (17 " S_save_selection_dialog ") (18 " S_insert_file_dialog ") \
 (19 " S_save_region_dialog ") (20 " S_preferences_dialog "))"

  check_dialog_widget_table();
  return(Xen_vector_to_Xen_list(dialog_widgets));
}


void set_dialog_widget(snd_dialog_t which, widget_t wid)
{
  if (!ss->dialogs)
    {
      ss->dialogs_size = 8;
      ss->num_dialogs = 0;
      ss->dialogs = (widget_t *)calloc(ss->dialogs_size, sizeof(widget_t));
    }
  else
    {
      if (ss->num_dialogs == ss->dialogs_size)
	{
	  int i;
	  ss->dialogs_size += 8;
	  ss->dialogs = (widget_t *)realloc(ss->dialogs, ss->dialogs_size * sizeof(widget_t));
	  for (i = ss->num_dialogs; i < ss->dialogs_size; i++) ss->dialogs[i] = NULL;
	}
    }
  ss->dialogs[ss->num_dialogs++] = wid;
  check_dialog_widget_table();

  if (Xen_is_false(Xen_vector_ref(dialog_widgets, (int)which)))
    Xen_vector_set(dialog_widgets, (int)which, 
		   Xen_wrap_widget(wid));
  else 
    {
      if (Xen_is_widget(Xen_vector_ref(dialog_widgets, (int)which)))
	Xen_vector_set(dialog_widgets, (int)which, 
		       Xen_list_2(Xen_wrap_widget(wid), 
				  Xen_vector_ref(dialog_widgets, (int)which)));
      else Xen_vector_set(dialog_widgets, (int)which, 
			  Xen_cons(Xen_wrap_widget(wid), 
				   Xen_vector_ref(dialog_widgets, (int)which)));
    }
  run_new_widget_hook(wid);
}


static Xen g_widget_position(Xen wid)
{
  #define H_widget_position "(" S_widget_position " wid): widget's position, (list x y), in pixels"
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_widget_position, "a Widget");  
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (!w)
    Xen_error(NO_SUCH_WIDGET,
	      Xen_list_2(C_string_to_Xen_string(S_widget_position ": no such widget: ~A"),
			 wid));
  return(Xen_list_2(C_int_to_Xen_integer(widget_x(w)),
		    C_int_to_Xen_integer(widget_y(w))));
}


static Xen g_set_widget_position(Xen wid, Xen xy)
{
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_set S_widget_position, "a Widget");  
  Xen_check_type(Xen_is_list(xy) && (Xen_list_length(xy) == 2), xy, 2, S_set S_widget_position, "a list: (x y)");  
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    {
      Xen_check_type(Xen_is_integer(Xen_car(xy)), Xen_car(xy), 0, S_set S_widget_position, "an integer");
      Xen_check_type(Xen_is_integer(Xen_cadr(xy)), Xen_cadr(xy), 0, S_set S_widget_position, "an integer");
      set_widget_position(w,
			  mus_iclamp(0, Xen_integer_to_C_int(Xen_car(xy)), LOTSA_PIXELS),
			  mus_iclamp(0, Xen_integer_to_C_int(Xen_cadr(xy)), LOTSA_PIXELS));
    }
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_set S_widget_position ": no such widget: ~A"),
			    wid));
  return(wid);
}


static Xen g_widget_size(Xen wid)
{
  #define H_widget_size "(" S_widget_size " wid): widget's size, (list width height), in pixels"
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_widget_size, "a Widget"); 
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (!w)
    Xen_error(NO_SUCH_WIDGET,
	      Xen_list_2(C_string_to_Xen_string(S_widget_size ": no such widget: ~A"),
			 wid));
  return(Xen_list_2(C_int_to_Xen_integer(widget_width(w)),
		    C_int_to_Xen_integer(widget_height(w))));
}


static Xen g_set_widget_size(Xen wid, Xen wh)
{
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_set S_widget_size, "a Widget");  
  Xen_check_type(Xen_is_list(wh) && (Xen_list_length(wh) == 2), wh, 2, S_set S_widget_size, "a list: (width height)");  
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    {
      Xen_check_type(Xen_is_integer(Xen_car(wh)), Xen_car(wh), 0, S_set S_widget_size, "an integer");
      Xen_check_type(Xen_is_integer(Xen_cadr(wh)), Xen_cadr(wh), 0, S_set S_widget_size, "an integer");
      set_widget_size(w,
		      mus_iclamp(1, Xen_integer_to_C_int(Xen_car(wh)), LOTSA_PIXELS),
		      mus_iclamp(1, Xen_integer_to_C_int(Xen_cadr(wh)), LOTSA_PIXELS));
    }
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_set S_widget_size ": no such widget: ~A"),
			    wid));
  return(wid);
}


static Xen g_widget_text(Xen wid)
{
  #define H_widget_text "(" S_widget_text " wid): widget's text or label"
  widget_t w;

  Xen res = Xen_false;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_widget_text, "a Widget");

  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    {
#if USE_MOTIF
      char *text = NULL;
      if ((XmIsText(w)) || (XmIsTextField(w)))
	{
	  text = XmTextGetString(w);
	  res = C_string_to_Xen_string(text);
	}
      else
	{
	  XmString s1 = NULL;
	  XtVaGetValues(w, XmNlabelString, &s1, NULL);
	  if (XmStringEmpty(s1)) return(Xen_false);
	  text = (char *)XmStringUnparse(s1, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  XmStringFree(s1);
	  res = C_string_to_Xen_string(text);
	}
      if (text) XtFree(text);
      return(res);
#else
      if (GTK_IS_ENTRY(w))
	return(C_string_to_Xen_string((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      else
	{
	  if ((GTK_IS_BIN(w)) && (GTK_IS_LABEL(BIN_CHILD(w))))
	    return(C_string_to_Xen_string((char *)gtk_label_get_text(GTK_LABEL(BIN_CHILD(w)))));
	  else
	    {
	      if (GTK_IS_LABEL(w))
		return(C_string_to_Xen_string((char *)gtk_label_get_text(GTK_LABEL(w))));
	      else
		{
		  if (GTK_IS_TEXT_VIEW(w))
		    {
		      Xen val = Xen_false;
		      char *text;
		      text = sg_get_text(w, 0, -1);
		      if (text)
			{
			  val = C_string_to_Xen_string(text); /* this copies, so it should be safe to free the original */
			  g_free(text);
			}
		      return(val);
		    }
		}
	    }
	}
#endif
    }
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_widget_text ": no such widget: ~A"),
			    wid));
  return(res);
}


static Xen g_set_widget_text(Xen wid, Xen text)
{
  widget_t w;

  Xen_check_type(Xen_is_widget(wid), wid, 1, S_set S_widget_text, "a Widget");
  Xen_check_type(Xen_is_string(text) || Xen_is_false(text), text, 2, S_set S_widget_text, "a string");

  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    {
      const char *str = NULL;
      if (Xen_is_string(text)) str = Xen_string_to_C_string(text);
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	XmTextSetString(w, (char *)str);
      else set_button_label(w, str);
#else
      if (GTK_IS_ENTRY(w))
	gtk_entry_set_text(GTK_ENTRY(w), str);
      else set_button_label(w, str);
#endif
    }
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_set S_widget_text ": no such widget: ~A"),
			    wid));
  return(text);
}


static Xen g_hide_widget(Xen wid)
{
  #define H_hide_widget "(" S_hide_widget " widget): hide or undisplay widget"
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_hide_widget, "a Widget");  
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    {
#if USE_MOTIF
      XtUnmanageChild(w);
#else
      gtk_widget_hide(w);
#endif
    }
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_hide_widget ": no such widget: ~A"),
			    wid));
  return(wid);
}


static Xen g_show_widget(Xen wid)
{
  #define H_show_widget "(" S_show_widget " widget): show or display widget"
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_show_widget, "a Widget");  
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    {
#if USE_MOTIF
      XtManageChild(w);
#else
      gtk_widget_show(w);
#endif
    }
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_show_widget ": no such widget: ~A"),
			    wid));
  return(wid);
}


static Xen g_focus_widget(Xen wid)
{
  #define H_focus_widget "(" S_focus_widget " widget): cause widget to receive input focus"
  widget_t w;
  Xen_check_type(Xen_is_widget(wid), wid, 1, S_focus_widget, "a Widget");
  w = (widget_t)(Xen_unwrap_widget(wid));
  if (w)
    goto_window(w);
  else Xen_error(NO_SUCH_WIDGET,
		 Xen_list_2(C_string_to_Xen_string(S_focus_widget ": no such widget: ~A"),
			    wid));
  return(wid);
}


static Xen g_snd_gcs(void)
{
  #define H_snd_gcs "(" S_snd_gcs "): a list of Snd graphics contexts (list (0 basic) (1 selected_basic) (2 combined) (3 \
cursor) (4 selected_cursor) (5 selection) (6 selected_selection) (7 erase) (8 selected_erase) (9 mark) (10 selected_mark) (11 mix) (12 \
fltenv_basic) (13 fltenv_data))."

#if USE_MOTIF
      #define Xen_wrap_snd_gc(Value) Xen_list_2(C_string_to_Xen_symbol("GC"), Xen_wrap_C_pointer(Value))
#else
  #if USE_GTK
      #define Xen_wrap_snd_gc(Value) Xen_list_2(C_string_to_Xen_symbol("gc_t_"), Xen_wrap_C_pointer(Value))
  #else
      #define Xen_wrap_snd_gc(Value) Xen_false
  #endif
#endif

#if (!USE_NO_GUI)
    return(Xen_cons(Xen_wrap_snd_gc(ss->basic_gc),
	    Xen_cons(Xen_wrap_snd_gc(ss->selected_basic_gc), 
	     Xen_cons(Xen_wrap_snd_gc(ss->combined_basic_gc), 
	      Xen_cons(Xen_wrap_snd_gc(ss->cursor_gc), 
               Xen_cons(Xen_wrap_snd_gc(ss->selected_cursor_gc), 
                Xen_cons(Xen_wrap_snd_gc(ss->selection_gc), 
                 Xen_cons(Xen_wrap_snd_gc(ss->selected_selection_gc), 
                  Xen_cons(Xen_wrap_snd_gc(ss->erase_gc), 
                   Xen_cons(Xen_wrap_snd_gc(ss->selected_erase_gc), 
                    Xen_cons(Xen_wrap_snd_gc(ss->mark_gc), 
                     Xen_cons(Xen_wrap_snd_gc(ss->selected_mark_gc), 
                      Xen_cons(Xen_wrap_snd_gc(ss->mix_gc), 
                       Xen_cons(Xen_wrap_snd_gc(ss->fltenv_basic_gc), 
                        Xen_cons(Xen_wrap_snd_gc(ss->fltenv_data_gc), 
			 Xen_empty_list)))))))))))))));
#else
  return(Xen_empty_list);
#endif
}


static Xen g_snd_color(Xen choice)
{
  #define H_snd_color "(" S_snd_color " num): color associated with 'num' -- see table of colors in snd-draw.c"
  color_t col;
  Xen_check_type(Xen_is_integer(choice), choice, 1, S_snd_color, "an integer");

  switch (Xen_integer_to_C_int(choice))
    {
    case 0: col = ss->white;                          break;
    case 1: col = ss->black;                          break;
    case 2: col = ss->red;                            break;
    case 3: col = ss->yellow;                         break;
    case 4: col = ss->green;                          break;
    case 5: col = ss->light_blue;                     break;
    case 6: col = ss->lighter_blue;                   break;

    case 7: col = ss->data_color;                     break;
    case 8: col = ss->selected_data_color;            break;
    case 9: col = ss->mark_color;                     break;
    case 10: col = ss->graph_color;                   break;
    case 11: col = ss->selected_graph_color;          break;
    case 12: col = ss->listener_color;                break;
    case 13: col = ss->listener_text_color;           break;

    case 14: col = ss->basic_color;                   break;
    case 15: col = ss->selection_color;               break;
    case 16: col = ss->zoom_color;                    break;
    case 17: col = ss->position_color;                break;
    case 18: col = ss->highlight_color;               break;
    case 19: col = ss->enved_waveform_color;          break;
    case 20: col = ss->cursor_color;                  break;

    case 21: col = ss->text_focus_color;              break;
    case 22: col = ss->filter_control_waveform_color; break;
    case 23: col = ss->mix_color;                     break;
    case 25: col = ss->sash_color;                    break;

    case 31: col = ss->grid_color;                    break;
    case 32: col = ss->selected_grid_color;           break;
    case 33: 
      if (ss->axis_color_set)
	col = ss->axis_color;
      else col = ss->black;
      break;
    default: col = ss->black;                         break;
    }
  return(Xen_wrap_pixel(col));
}


static Xen g_snd_font(Xen choice)
{
#if USE_MOTIF
  #define WRAP_FONT(Value) Xen_list_2(C_string_to_Xen_symbol("Font"), C_ulong_to_Xen_ulong((unsigned long)Value))
#else
  #define WRAP_FONT(Value) Xen_list_2(C_string_to_Xen_symbol("PangoFontDescription_"), Xen_wrap_C_pointer(Value))
#endif

  #define H_snd_font "(" S_snd_font " num): font associated with 'num' -- see table of fonts in snd-draw.c"
  Xen_check_type(Xen_is_integer(choice), choice, 1, S_snd_font, "an integer");

  switch (Xen_integer_to_C_int(choice))
    {
#if USE_MOTIF
    case 0: return(WRAP_FONT(ss->peaks_fontstruct->fid));        
    case 1: return(WRAP_FONT(ss->bold_peaks_fontstruct->fid));   
    case 2: return(WRAP_FONT(ss->tiny_fontstruct->fid));         
    case 3: return(WRAP_FONT(ss->axis_label_fontstruct->fid));   
    case 4: return(WRAP_FONT(ss->axis_numbers_fontstruct->fid)); 
    case 5: return(WRAP_FONT(ss->listener_fontstruct->fid));     
#endif
#if USE_GTK
    case 0: return(WRAP_FONT(ss->peaks_fnt));                    
    case 1: return(WRAP_FONT(ss->bold_peaks_fnt));               
    case 2: return(WRAP_FONT(ss->tiny_fnt));                     
    case 3: return(WRAP_FONT(ss->axis_label_fnt));               
    case 4: return(WRAP_FONT(ss->axis_numbers_fnt));             
    case 5: return(WRAP_FONT(ss->listener_fnt));                 
#endif
    default: return(Xen_false);                                  
    }
  return(Xen_false);
}

#if 1
static Xen g_make_cairo(Xen drawer)
{
  #define H_make_cairo "(" S_make_cairo " widget) in gtk, this returns a new cairo_t to draw on the widget."

#if USE_GTK
  #define C_to_Xen_cairo_t(Value) Xen_list_2(C_string_to_Xen_symbol("cairo_t_"), Xen_wrap_C_pointer(Value))
  cairo_t *cr;

  Xen_check_type(Xen_is_widget(drawer), drawer, 1, S_make_cairo, "a widget");

#if (!GTK_CHECK_VERSION(3, 0, 0))
  cr = make_cairo(GDK_DRAWABLE(gtk_widget_get_window(Xen_unwrap_widget(drawer))));
#else
  cr = make_cairo(GDK_WINDOW(gtk_widget_get_window(Xen_unwrap_widget(drawer))));
#endif

  return(C_to_Xen_cairo_t(cr));
#endif

  return(Xen_false);
}


static Xen g_free_cairo(Xen xcr)
{
  #define H_free_cairo "(" S_free_cairo " cr) in gtk, this frees (destroys) the cairo_t 'cr'."

#if USE_GTK
  if ((Xen_is_list(xcr)) &&
      (Xen_list_length(xcr) == 2) &&
      (Xen_is_symbol(Xen_car(xcr))) &&
      (strcmp("cairo_t_", Xen_symbol_to_C_string(Xen_car(xcr))) == 0))
    free_cairo((cairo_t *)Xen_unwrap_C_pointer(Xen_cadr(xcr)));
  else 
    Xen_error(Xen_make_error_type("not-a-graphics-context"),
	      Xen_list_2(C_string_to_Xen_string(S_free_cairo ": cairo_t argument is not a cairo_t pointer: ~A"), xcr));
#endif
  return(Xen_false);
}
#endif


#if HAVE_GL
static mus_float_t gl_currents[6] = {DEFAULT_SPECTRO_X_ANGLE, DEFAULT_SPECTRO_Y_ANGLE, DEFAULT_SPECTRO_Z_ANGLE, 
			       DEFAULT_SPECTRO_X_SCALE, DEFAULT_SPECTRO_Y_SCALE, DEFAULT_SPECTRO_Z_SCALE};
static mus_float_t x_currents[6] = {90.0, 0.0, 358.0, 1.0, 1.0, 0.1};

void sgl_save_currents(void)
{
  mus_float_t *vals;
  if (with_gl(ss)) vals = gl_currents; else vals = x_currents;
  vals[0] = spectro_x_angle(ss);
  vals[1] = spectro_y_angle(ss);
  vals[2] = spectro_z_angle(ss);
  vals[3] = spectro_x_scale(ss);
  vals[4] = spectro_y_scale(ss);
  vals[5] = spectro_z_scale(ss);
}


void sgl_set_currents(bool with_dialogs)
{
  mus_float_t *vals;
  if (with_gl(ss)) vals = gl_currents; else vals = x_currents;
  in_set_spectro_x_angle(vals[0]);
  in_set_spectro_y_angle(vals[1]);
  in_set_spectro_z_angle(vals[2]);
  in_set_spectro_x_scale(vals[3]);
  in_set_spectro_y_scale(vals[4]);
  in_set_spectro_z_scale(vals[5]);
  if (with_dialogs) reflect_spectro();
  chans_field(FCP_X_ANGLE, vals[0]);
  chans_field(FCP_Y_ANGLE, vals[1]);
  chans_field(FCP_Z_ANGLE, vals[2]);
  chans_field(FCP_X_SCALE, vals[3]);
  chans_field(FCP_Y_SCALE, vals[4]);
  chans_field(FCP_Z_SCALE, vals[5]);
}
#endif



/* -------- shared color funcs -------- */

static Xen g_is_color(Xen obj) 
{
  #define H_is_color "(" S_is_color " obj): " PROC_TRUE " if obj is a color"
  return(C_bool_to_Xen_boolean(Xen_is_pixel(obj)));
}


mus_float_t check_color_range(const char *caller, Xen val)
{
  mus_float_t rf;
  rf = Xen_real_to_C_double(val);
  if ((rf > 1.0) || (rf < 0.0))
    Xen_out_of_range_error(caller, 1, val, "value must be between 0.0 and 1.0");
  return(rf);
}


static Xen g_set_cursor_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_cursor_color, "a color"); 
  color_cursor(Xen_unwrap_pixel(color));
  for_each_chan(update_graph);
  return(color);
}


static Xen g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color "): cursor color"
  return(Xen_wrap_pixel(ss->cursor_color));
}


#if USE_MOTIF
static void highlight_recolor_everything(widget_t w, color_t color)
{
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == color)
	XmChangeColor(w, ss->highlight_color);
    }
  /* to handle the gtk side correctly here, we'd need a list of widgets to modify --
   *    currently basic-color hits every background, so the whole thing is messed up.
   */
}
#endif


void set_highlight_color(color_t color)
{
#if USE_MOTIF
  color_t old_color;
  old_color = ss->highlight_color;
#endif
  ss->highlight_color = color; 
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->highlight_color_symbol, Xen_wrap_pixel(color));
#endif
#if USE_MOTIF
  map_over_children_with_color(main_shell(ss), highlight_recolor_everything, old_color);
#endif
}


static Xen g_set_highlight_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_highlight_color, "a color"); 
  set_highlight_color(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color "): color of highlighted text or buttons"
  return(Xen_wrap_pixel(ss->highlight_color));
}


static Xen g_set_mark_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_mark_color, "a color"); 
  color_marks(Xen_unwrap_pixel(color));
  for_each_chan(update_graph);
  return(color);
}


static Xen g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color "): mark color"
  return(Xen_wrap_pixel(ss->mark_color));
}


void set_zoom_color(color_t color)
{
  ss->zoom_color = color; 
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->zoom_color_symbol, Xen_wrap_pixel(color));
#endif
  color_chan_components(ss->zoom_color, COLOR_ZOOM);
}


static Xen g_set_zoom_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_zoom_color, "a color"); 
  set_zoom_color(Xen_unwrap_pixel(color)); 
  return(color);
}


static Xen g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color "): color of zoom sliders"
  return(Xen_wrap_pixel(ss->zoom_color));
}


void set_position_color(color_t color)
{
  ss->position_color = color; 
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->position_color_symbol, Xen_wrap_pixel(color));
#endif
  color_chan_components(ss->position_color, COLOR_POSITION);
}


static Xen g_set_position_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_position_color, "a color"); 
  set_position_color(Xen_unwrap_pixel(color)); 
  return(color);
}


static Xen g_position_color(void) 
{
  #define H_position_color "(" S_position_color "): color of position sliders"
  return(Xen_wrap_pixel(ss->position_color));
}


static Xen g_set_listener_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_listener_color, "a color"); 
  color_listener(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color "): background color of the lisp listener"
  return(Xen_wrap_pixel(ss->listener_color));
}


static Xen g_set_listener_text_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_listener_text_color, "a color"); 
  color_listener_text(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color "): text color in the lisp listener"
  return(Xen_wrap_pixel(ss->listener_text_color));
}


static Xen g_set_enved_waveform_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_enved_waveform_color, "a color"); 
  ss->enved_waveform_color = Xen_unwrap_pixel(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->enved_waveform_color_symbol, color);
#endif
  color_enved_waveform(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color "): color of the envelope editor wave display"
  return(Xen_wrap_pixel(ss->enved_waveform_color));
}


static Xen g_set_filter_control_waveform_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_filter_control_waveform_color, "a color");
  ss->filter_control_waveform_color = Xen_unwrap_pixel(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->filter_control_waveform_color_symbol, color);
#endif
  color_filter_waveform(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_filter_control_waveform_color(void) 
{
  #define H_filter_control_waveform_color "(" S_filter_control_waveform_color "): color of the filter waveform"
  return(Xen_wrap_pixel(ss->filter_control_waveform_color));
}


static Xen g_set_selection_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_selection_color, "a color"); 
  color_selection(Xen_unwrap_pixel(color));
  for_each_chan(update_graph);
  return(color);
}


static Xen g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color "): selection color"
  return(Xen_wrap_pixel(ss->selection_color));
}


static Xen g_set_text_focus_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_text_focus_color, "a color"); 
  ss->text_focus_color = Xen_unwrap_pixel(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->text_focus_color_symbol, color);
#endif
  return(color);
}


static Xen g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color "): color used to show a text field has focus"
  return(Xen_wrap_pixel(ss->text_focus_color));
}


static Xen g_set_sash_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_sash_color, "a color"); 
  ss->sash_color = Xen_unwrap_pixel(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->sash_color_symbol, color);
#endif
  return(color);
}


static Xen g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color "): color used to draw paned window sashes"
  return(Xen_wrap_pixel(ss->sash_color));
}


static Xen g_data_color(void) 
{
  #define H_data_color "(" S_data_color "): color used to draw unselected data"
  return(Xen_wrap_pixel(ss->data_color));
}


void set_data_color(color_t color)
{
  ss->data_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->data_color_symbol, Xen_wrap_pixel(color));
#endif
  color_data(color);
  ss->grid_color = get_in_between_color(ss->data_color, ss->graph_color);
  for_each_chan(update_graph);
}


static Xen g_set_data_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_data_color, "a color"); 
  set_data_color(Xen_unwrap_pixel(color));
  return(color);
}


void set_selected_data_color(color_t color)
{
  chan_info *cp;
  ss->selected_data_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->selected_data_color_symbol, Xen_wrap_pixel(color));
#endif
  color_selected_data(color);
  ss->selected_grid_color = get_in_between_color(ss->selected_data_color, ss->selected_graph_color);
  cp = selected_channel();
  if (cp) update_graph(cp);
}


static Xen g_set_selected_data_color(Xen color)
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_selected_data_color, "a color"); 
  set_selected_data_color(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color "): color used for selected data"
  return(Xen_wrap_pixel(ss->selected_data_color));
}


void set_graph_color(color_t color)
{
  color_graph(color);
  ss->graph_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->graph_color_symbol, Xen_wrap_pixel(color));
#endif
  color_unselected_graphs(color);
  ss->grid_color = get_in_between_color(ss->data_color, ss->graph_color);
}


static Xen g_set_graph_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_graph_color, "a color");
  set_graph_color(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color "): background color used for unselected data"
  return(Xen_wrap_pixel(ss->graph_color));
}


void set_selected_graph_color(color_t color)
{
  chan_info *cp;
  ss->selected_graph_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->selected_graph_color_symbol, Xen_wrap_pixel(color));
#endif
  color_selected_graph(color);
  ss->selected_grid_color = get_in_between_color(ss->selected_data_color, ss->selected_graph_color);
  cp = selected_channel();
  if (cp) 
    {
#if USE_MOTIF
      XtVaSetValues(channel_graph(cp), XmNbackground, ss->selected_graph_color, NULL);
#else
#if (!GTK_CHECK_VERSION(3, 0, 0))
      widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, ss->selected_graph_color);
#endif
#endif
    }
}


static Xen g_set_selected_graph_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_selected_graph_color, "a color");
  set_selected_graph_color(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color "): background color of selected data"
  return(Xen_wrap_pixel(ss->selected_graph_color));
}


static Xen g_set_axis_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_axis_color, "a color");
  ss->axis_color = Xen_unwrap_pixel(color);
  ss->axis_color_set = true;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->axis_color_symbol, color);
#endif
  for_each_chan(update_graph);
  return(color);
}


static Xen g_axis_color(void) 
{
  #define H_axis_color "(" S_axis_color "): color of axis (defaults to current data color)"
  return(Xen_wrap_pixel(ss->axis_color));
}


static Xen g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color "): Snd's basic color"
  return(Xen_wrap_pixel(ss->basic_color));
}


#if USE_GTK

#if GTK_CHECK_VERSION(3, 0, 0)
#if (!GTK_CHECK_VERSION(3, 16, 0))
static bool is_dark(color_info *color)
{
  return(color->red + color->green + color->blue < 0.75);
}
#endif
#endif

static void recolor_everything_1(widget_t w, gpointer color)
{
#if (!GTK_CHECK_VERSION(3, 16, 0))
  if ((GTK_IS_WIDGET(w)) &&
      (w != ss->listener_pane))
    {
#if (!GTK_CHECK_VERSION(3, 0, 0))
      /* this is a gigantic memory leak */
#if 0
      gtk_widget_modify_bg(w, GTK_STATE_NORMAL, (GdkColor *)color);
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), recolor_everything_1, color);
#endif

#else
      gtk_widget_override_background_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)color);
      if (GTK_IS_LABEL(w))
	{
	  if (is_dark((color_info *)color))
	    gtk_widget_override_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->white));
	  else gtk_widget_override_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->black));
	}
	
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), recolor_everything_1, color);
#endif
    }
#endif
}


void recolor_everything(widget_t w, gpointer color)
{
#if (!GTK_CHECK_VERSION(3, 0, 0))
  GdkColor *nc;
  nc = rgb_to_gdk_color((color_t)color);
#else
  GdkRGBA *nc;
  nc = (GdkRGBA *)color;
#endif
  recolor_everything_1(w, (gpointer)nc);
}


static Xen g_color_to_list(Xen obj)
{
  #define H_color_to_list "(" S_color_to_list " obj): 'obj' rgb values as a list of floats"
  color_info *v;
  Xen_check_type(Xen_is_pixel(obj), obj, 1, S_color_to_list, "a color"); 
  v = Xen_unwrap_pixel(obj);
  if (v)
    return(Xen_list_4(C_double_to_Xen_real(rgb_to_float(v->red)),
		      C_double_to_Xen_real(rgb_to_float(v->green)),
		      C_double_to_Xen_real(rgb_to_float(v->blue)),
		      C_double_to_Xen_real(v->alpha)));
  return(Xen_empty_list);
}


static Xen g_make_color(Xen r, Xen g, Xen b, Xen alpha)
{
  #define H_make_color "(" S_make_color " r g b alpha): return a color object with the indicated rgb values"
  color_info *ccolor;
  mus_float_t rf, gf, bf;

  Xen_check_type(Xen_is_number(r), r, 1, S_make_color, "a number");
  /* someday accept a list as r */
  Xen_check_type(Xen_is_number(g), g, 2, S_make_color, "a number");
  Xen_check_type(Xen_is_number(b), b, 3, S_make_color, "a number");

  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  ccolor = (color_info *)calloc(1, sizeof(color_info)); /* memleak here */
  ccolor->red = rf;
  ccolor->green = gf;
  ccolor->blue = bf;

  if (Xen_is_number(alpha))
    ccolor->alpha = check_color_range(S_make_color, alpha);
  else ccolor->alpha = 1.0;

  return(Xen_wrap_pixel(ccolor));
}
#endif



#if USE_MOTIF
static void recolor_everything(widget_t w, color_t color)
{
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == color)
	XmChangeColor(w, ss->basic_color);
    }
}

static Xen g_color_to_list(Xen obj)
{
  #define H_color_to_list "(" S_color_to_list " obj): 'obj' rgb values as a list of floats"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;

  Xen_check_type(Xen_is_pixel(obj), obj, 1, S_color_to_list, "a color"); 

  dpy = XtDisplay(main_shell(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = Xen_unwrap_pixel(obj);
  XQueryColor(dpy, cmap, &tmp_color);
  return(Xen_list_3(C_double_to_Xen_real(rgb_to_float(tmp_color.red)),
		    C_double_to_Xen_real(rgb_to_float(tmp_color.green)),
		    C_double_to_Xen_real(rgb_to_float(tmp_color.blue))));
}


static Xen g_make_color(Xen r, Xen g, Xen b, Xen alpha)
{
  /* alpha is ignored in Motif */
  #define H_make_color "(" S_make_color " r g b alpha): return a color object with the indicated rgb values"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  mus_float_t rf, gf, bf;

  Xen_check_type(Xen_is_number(r), r, 1, S_make_color, "a number");
  /* someday accept a list as r */
  Xen_check_type(Xen_is_number(g), g, 2, S_make_color, "a number");
  Xen_check_type(Xen_is_number(b), b, 3, S_make_color, "a number");

  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  dpy = XtDisplay(main_shell(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.red = float_to_rgb(rf);
  tmp_color.green = float_to_rgb(gf);
  tmp_color.blue = float_to_rgb(bf);

  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
    Xen_error(Xen_make_error_type("no-such-color"),
	      Xen_list_4(C_string_to_Xen_string(S_make_color ": can't allocate this color! (~A ~A ~A)"),
			 r, g, b));

  return(Xen_wrap_pixel(tmp_color.pixel));
}
#endif


void set_basic_color(color_t color)
{
#if USE_MOTIF
  color_t old_color;
  old_color = ss->basic_color;
#endif
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->basic_color_symbol, Xen_wrap_pixel(color));
#endif
  ss->basic_color = color; 
#if USE_MOTIF
  map_over_children_with_color(main_shell(ss), recolor_everything, old_color);
#endif
#if USE_GTK
  recolor_everything(main_shell(ss), color);
#endif

#if USE_MOTIF
  make_sound_icons_transparent_again(old_color, ss->basic_color);
  make_mixer_icons_transparent_again(old_color, ss->basic_color);
#endif
}


static Xen g_set_basic_color(Xen color) 
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_basic_color, "a color"); 
  set_basic_color(Xen_unwrap_pixel(color));
  return(color);
}


static Xen g_mix_color(Xen mix_id) 
{
  #define H_mix_color "(" S_mix_color " :optional mix-id): color of all mix tags (if mix-id is omitted), or of mix-id's tag"
  if (xen_is_mix(mix_id))
    return(Xen_wrap_pixel(mix_color_from_id(Xen_mix_to_C_int(mix_id))));
  return(Xen_wrap_pixel(ss->mix_color));
}


static Xen g_set_mix_color(Xen color, Xen mix_id)
{
  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_mix_color, "a color"); 
  Xen_check_type(xen_is_mix(mix_id) || !Xen_is_bound(mix_id), mix_id, 2, S_set S_mix_color, "a mix");
  if (xen_is_mix(mix_id))
    mix_set_color_from_id(Xen_mix_to_C_int(mix_id), Xen_unwrap_pixel(color));
  else color_mixes(Xen_unwrap_pixel(color));
  return(color);
}

with_two_setter_args(g_set_mix_color_reversed, g_set_mix_color)


bool foreground_color_ok(Xen color, graphics_context *ax)
{
  if (Xen_is_pixel(color))
    {
      set_foreground_color(ax, (color_t)Xen_unwrap_pixel(color));
      return(true);
    }
  return(false);
}



static Xen g_combined_data_color(Xen snd, Xen chn)
{
  #define H_combined_data_color "(" S_combined_data_color " snd chn): color of this channel's data if graphed with channels-combined"
  chan_info *cp;

  Snd_assert_channel(S_combined_data_color, snd, chn, 1);
  cp = get_cp(snd, chn, S_combined_data_color);
  if (!cp) return(Xen_false);

  return(Xen_wrap_pixel(cp->combined_data_color));
}


static Xen g_set_combined_data_color(Xen color, Xen snd, Xen chn)
{
  chan_info *cp;

  Xen_check_type(Xen_is_pixel(color), color, 1, S_set S_combined_data_color, "a color"); 
  Snd_assert_channel(S_combined_data_color, snd, chn, 1);
  cp = get_cp(snd, chn, S_combined_data_color);
  if (!cp) return(Xen_false);

  cp->combined_data_color = Xen_unwrap_pixel(color);
  update_graph(cp);
  return(color);
}

with_three_setter_args(g_set_combined_data_color_reversed, g_set_combined_data_color)



Xen_wrap_8_optional_args(g_draw_line_w, g_draw_line)
Xen_wrap_7_optional_args(g_draw_dot_w, g_draw_dot)
Xen_wrap_5_optional_args(g_draw_lines_w, g_draw_lines)
Xen_wrap_6_optional_args(g_draw_dots_w, g_draw_dots)
Xen_wrap_7_optional_args(g_draw_string_w, g_draw_string)
Xen_wrap_9_optional_args(g_fill_rectangle_w, g_fill_rectangle)
Xen_wrap_5_optional_args(g_fill_polygon_w, g_fill_polygon)
Xen_wrap_3_optional_args(g_foreground_color_w, g_foreground_color)
Xen_wrap_3_optional_args(g_current_font_w, g_current_font)
Xen_wrap_no_args(g_main_widgets_w, g_main_widgets)
Xen_wrap_no_args(g_dialog_widgets_w, g_dialog_widgets)
Xen_wrap_1_arg(g_widget_size_w, g_widget_size)
Xen_wrap_2_args(g_set_widget_size_w, g_set_widget_size)
Xen_wrap_1_arg(g_widget_position_w, g_widget_position)
Xen_wrap_2_args(g_set_widget_position_w, g_set_widget_position)
Xen_wrap_1_arg(g_widget_text_w, g_widget_text)
Xen_wrap_2_args(g_set_widget_text_w, g_set_widget_text)
Xen_wrap_1_arg(g_hide_widget_w, g_hide_widget)
Xen_wrap_1_arg(g_show_widget_w, g_show_widget)
Xen_wrap_1_arg(g_focus_widget_w, g_focus_widget)
Xen_wrap_5_optional_args(g_make_graph_data_w, g_make_graph_data)
Xen_wrap_8_optional_args(g_graph_data_w, g_graph_data)
Xen_wrap_any_args(g_make_bezier_w, g_make_bezier)
Xen_wrap_no_args(g_snd_gcs_w, g_snd_gcs)
Xen_wrap_1_arg(g_snd_color_w, g_snd_color)
Xen_wrap_1_arg(g_snd_font_w, g_snd_font)
#if 1
Xen_wrap_1_arg(g_make_cairo_w, g_make_cairo)
Xen_wrap_1_arg(g_free_cairo_w, g_free_cairo)
#endif

Xen_wrap_no_args(g_selection_color_w, g_selection_color)
Xen_wrap_1_arg(g_set_selection_color_w, g_set_selection_color)
Xen_wrap_no_args(g_zoom_color_w, g_zoom_color)
Xen_wrap_1_arg(g_set_zoom_color_w, g_set_zoom_color)
Xen_wrap_no_args(g_position_color_w, g_position_color)
Xen_wrap_1_arg(g_set_position_color_w, g_set_position_color)
Xen_wrap_no_args(g_mark_color_w, g_mark_color)
Xen_wrap_1_arg(g_set_mark_color_w, g_set_mark_color)
Xen_wrap_no_args(g_listener_color_w, g_listener_color)
Xen_wrap_1_arg(g_set_listener_color_w, g_set_listener_color)
Xen_wrap_no_args(g_listener_text_color_w, g_listener_text_color)
Xen_wrap_1_arg(g_set_listener_text_color_w, g_set_listener_text_color)
Xen_wrap_no_args(g_enved_waveform_color_w, g_enved_waveform_color)
Xen_wrap_1_arg(g_set_enved_waveform_color_w, g_set_enved_waveform_color)
Xen_wrap_no_args(g_filter_control_waveform_color_w, g_filter_control_waveform_color)
Xen_wrap_1_arg(g_set_filter_control_waveform_color_w, g_set_filter_control_waveform_color)
Xen_wrap_no_args(g_highlight_color_w, g_highlight_color)
Xen_wrap_1_arg(g_set_highlight_color_w, g_set_highlight_color)
Xen_wrap_no_args(g_cursor_color_w, g_cursor_color)
Xen_wrap_1_arg(g_set_cursor_color_w, g_set_cursor_color)
Xen_wrap_no_args(g_text_focus_color_w, g_text_focus_color)
Xen_wrap_1_arg(g_set_text_focus_color_w, g_set_text_focus_color)
Xen_wrap_no_args(g_sash_color_w, g_sash_color)
Xen_wrap_1_arg(g_set_sash_color_w, g_set_sash_color)
Xen_wrap_no_args(g_data_color_w, g_data_color)
Xen_wrap_1_arg(g_set_data_color_w, g_set_data_color)
Xen_wrap_no_args(g_graph_color_w, g_graph_color)
Xen_wrap_1_arg(g_set_graph_color_w, g_set_graph_color)
Xen_wrap_no_args(g_selected_graph_color_w, g_selected_graph_color)
Xen_wrap_1_arg(g_set_selected_graph_color_w, g_set_selected_graph_color)
Xen_wrap_no_args(g_selected_data_color_w, g_selected_data_color)
Xen_wrap_1_arg(g_set_selected_data_color_w, g_set_selected_data_color)
Xen_wrap_no_args(g_axis_color_w, g_axis_color)
Xen_wrap_1_arg(g_set_axis_color_w, g_set_axis_color)
Xen_wrap_no_args(g_basic_color_w, g_basic_color)
Xen_wrap_1_arg(g_set_basic_color_w, g_set_basic_color)
Xen_wrap_1_arg(g_is_color_w, g_is_color)
Xen_wrap_4_optional_args(g_make_color_w, g_make_color)
Xen_wrap_1_arg(g_color_to_list_w, g_color_to_list)
Xen_wrap_1_optional_arg(g_mix_color_w, g_mix_color)
Xen_wrap_2_args(g_combined_data_color_w, g_combined_data_color)

#if HAVE_SCHEME
#define g_set_current_font_w g_set_current_font_reversed
#define g_set_foreground_color_w g_set_foreground_color_reversed
#define g_set_mix_color_w g_set_mix_color_reversed
#define g_set_combined_data_color_w g_set_combined_data_color_reversed

static s7_pointer acc_data_color(s7_scheme *sc, s7_pointer args) {return(g_set_data_color(s7_cadr(args)));}
static s7_pointer acc_selected_data_color(s7_scheme *sc, s7_pointer args) {return(g_set_selected_data_color(s7_cadr(args)));}
static s7_pointer acc_mark_color(s7_scheme *sc, s7_pointer args) {return(g_set_mark_color(s7_cadr(args)));}
static s7_pointer acc_graph_color(s7_scheme *sc, s7_pointer args) {return(g_set_graph_color(s7_cadr(args)));}
static s7_pointer acc_selected_graph_color(s7_scheme *sc, s7_pointer args) {return(g_set_selected_graph_color(s7_cadr(args)));}
static s7_pointer acc_listener_color(s7_scheme *sc, s7_pointer args) {return(g_set_listener_color(s7_cadr(args)));}
static s7_pointer acc_listener_text_color(s7_scheme *sc, s7_pointer args) {return(g_set_listener_text_color(s7_cadr(args)));}
static s7_pointer acc_basic_color(s7_scheme *sc, s7_pointer args) {return(g_set_basic_color(s7_cadr(args)));}
static s7_pointer acc_zoom_color(s7_scheme *sc, s7_pointer args) {return(g_set_zoom_color(s7_cadr(args)));}
static s7_pointer acc_selection_color(s7_scheme *sc, s7_pointer args) {return(g_set_selection_color(s7_cadr(args)));}
static s7_pointer acc_position_color(s7_scheme *sc, s7_pointer args) {return(g_set_position_color(s7_cadr(args)));}
static s7_pointer acc_highlight_color(s7_scheme *sc, s7_pointer args) {return(g_set_highlight_color(s7_cadr(args)));}
static s7_pointer acc_enved_waveform_color(s7_scheme *sc, s7_pointer args) {return(g_set_enved_waveform_color(s7_cadr(args)));}
static s7_pointer acc_cursor_color(s7_scheme *sc, s7_pointer args) {return(g_set_cursor_color(s7_cadr(args)));}
static s7_pointer acc_filter_control_waveform_color(s7_scheme *sc, s7_pointer args) {return(g_set_filter_control_waveform_color(s7_cadr(args)));}
static s7_pointer acc_sash_color(s7_scheme *sc, s7_pointer args) {return(g_set_sash_color(s7_cadr(args)));}
static s7_pointer acc_axis_color(s7_scheme *sc, s7_pointer args) {return(g_set_axis_color(s7_cadr(args)));}
static s7_pointer acc_mix_color(s7_scheme *sc, s7_pointer args) {return(g_set_mix_color(s7_cadr(args), s7_undefined(s7)));}
static s7_pointer acc_text_focus_color(s7_scheme *sc, s7_pointer args) {return(g_set_text_focus_color(s7_cadr(args)));}

#else
Xen_wrap_4_optional_args(g_set_current_font_w, g_set_current_font)
Xen_wrap_4_optional_args(g_set_foreground_color_w, g_set_foreground_color)
Xen_wrap_2_optional_args(g_set_mix_color_w, g_set_mix_color)
Xen_wrap_3_args(g_set_combined_data_color_w, g_set_combined_data_color)
#endif

void g_init_draw(void)
{
#if HAVE_SCHEME
  s7_pointer i, b, p, t, r, mx, s, v, pcl_p, pcl_t, bi;
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "pair?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  v = s7_make_symbol(s7, "vector?");
  mx = s7_make_symbol(s7, "mix?");
  t = s7_t(s7);
  pcl_p = s7_make_circular_signature(s7, 0, 1, p);
  pcl_t = s7_make_circular_signature(s7, 0, 1, t);
  bi = s7_make_signature(s7, 2, i, b);
#endif

  dialog_widgets = Xen_undefined;

  Xen_define_constant(S_copy_context,      CHAN_GC,    "graphics context to draw a line");
  Xen_define_constant(S_cursor_context,    CHAN_CGC,   "graphics context for the cursor");
  Xen_define_constant(S_selection_context, CHAN_SELGC, "graphics context to draw in the selection color");
  Xen_define_constant(S_mark_context,      CHAN_MGC,   "graphics context for a mark");

  Xen_define_typed_procedure(S_draw_line,        g_draw_line_w,       4, 4, 0, H_draw_line,       s7_make_signature(s7, 9, b, i, i, i, i, t, t, i, p));
  Xen_define_typed_procedure(S_draw_dot,         g_draw_dot_w,        2, 5, 0, H_draw_dot,        s7_make_signature(s7, 8, b, i, i, i, t, t, i, p));
  Xen_define_typed_procedure(S_draw_lines,       g_draw_lines_w,      1, 4, 0, H_draw_lines,      s7_make_signature(s7, 6, v, v, t, t, i, p));
  Xen_define_typed_procedure(S_draw_dots,        g_draw_dots_w,       1, 5, 0, H_draw_dots,       s7_make_signature(s7, 7, v, v, i, t, t, i, p));
  Xen_define_typed_procedure(S_draw_string,      g_draw_string_w,     3, 4, 0, H_draw_string,     s7_make_signature(s7, 8, s, s, i, i, t, t, i, p));
  Xen_define_typed_procedure(S_fill_rectangle,   g_fill_rectangle_w,  4, 5, 0, H_fill_rectangle,  s7_make_signature(s7, 10, b, i, i, i, i, t, t, i, b, p));
  Xen_define_typed_procedure(S_fill_polygon,     g_fill_polygon_w,    1, 4, 0, H_fill_polygon,    s7_make_signature(s7, 6, v, v, t, t, i, p));
  Xen_define_typed_procedure(S_make_graph_data,  g_make_graph_data_w, 0, 5, 0, H_make_graph_data, s7_make_signature(s7, 6, t, t, t, t, i, i));
  Xen_define_typed_procedure(S_graph_data,       g_graph_data_w,      1, 7, 0, H_graph_data,      s7_make_signature(s7, 9, t, t, t, t, t, bi, bi, bi, p));

  Xen_define_typed_procedure(S_main_widgets,     g_main_widgets_w,    0, 0, 0, H_main_widgets,   pcl_p);
  Xen_define_typed_procedure(S_dialog_widgets,   g_dialog_widgets_w,  0, 0, 0, H_dialog_widgets, pcl_p);
  Xen_define_typed_procedure(S_hide_widget,      g_hide_widget_w,     1, 0, 0, H_hide_widget,    pcl_t);
  Xen_define_typed_procedure(S_show_widget,      g_show_widget_w,     1, 0, 0, H_show_widget,    pcl_t);
  Xen_define_typed_procedure(S_focus_widget,     g_focus_widget_w,    1, 0, 0, H_focus_widget,   pcl_t);

  Xen_define_typed_dilambda(S_foreground_color, g_foreground_color_w, H_foreground_color, 
			    S_set S_foreground_color, g_set_foreground_color_w, 0, 3, 1, 3,
			    s7_make_signature(s7, 4, p, t, t, i), s7_make_signature(s7, 5, p, t, t, i, p));
  Xen_define_typed_dilambda(S_current_font, g_current_font_w, H_current_font, 
			    S_set S_current_font, g_set_current_font_w, 0, 3, 1, 3,
			    s7_make_signature(s7, 4, p, t, t, i), s7_make_signature(s7, 5, p, t, t, i, p));
  Xen_define_typed_dilambda(S_widget_size, g_widget_size_w, H_widget_size, 
			    S_set S_widget_size, g_set_widget_size_w,  1, 0, 2, 0,
			    s7_make_signature(s7, 2, p, t), s7_make_signature(s7, 3, p, t, p));
  Xen_define_typed_dilambda(S_widget_position, g_widget_position_w, H_widget_position, 
			    S_set S_widget_position, g_set_widget_position_w,  1, 0, 2, 0,
			    s7_make_signature(s7, 2, p, t), s7_make_signature(s7, 3, p, t, p));
  Xen_define_typed_dilambda(S_widget_text, g_widget_text_w, H_widget_text, 
			    S_set S_widget_text, g_set_widget_text_w,  1, 0, 2, 0,
			    s7_make_signature(s7, 2, s, t), s7_make_signature(s7, 3, s, t, s));
  Xen_define_typed_dilambda(S_mix_color, g_mix_color_w, H_mix_color, 
			    S_set S_mix_color, g_set_mix_color_w, 0, 1, 1, 1,
			    s7_make_signature(s7, 2, p, mx), s7_make_signature(s7, 3, p, mx, p));
  Xen_define_typed_dilambda(S_combined_data_color, g_combined_data_color_w, H_combined_data_color, 
			    S_set S_combined_data_color, g_set_combined_data_color_w, 2, 0, 3, 0,
			    s7_make_signature(s7, 3, p, t, t), s7_make_signature(s7, 4, p, t, t, p));

  Xen_define_typed_dilambda(S_selection_color, g_selection_color_w, H_selection_color, 
			    S_set S_selection_color, g_set_selection_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_zoom_color, g_zoom_color_w, H_zoom_color, 
			    S_set S_zoom_color, g_set_zoom_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_position_color, g_position_color_w, H_position_color, 
			    S_set S_position_color, g_set_position_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_mark_color, g_mark_color_w, H_mark_color, 
			    S_set S_mark_color, g_set_mark_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_listener_color, g_listener_color_w, H_listener_color, 
			    S_set S_listener_color, g_set_listener_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_listener_text_color, g_listener_text_color_w, H_listener_text_color, 
			    S_set S_listener_text_color, g_set_listener_text_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_enved_waveform_color, g_enved_waveform_color_w, H_enved_waveform_color, 
			    S_set S_enved_waveform_color, g_set_enved_waveform_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_filter_control_waveform_color, g_filter_control_waveform_color_w, H_filter_control_waveform_color, 
			    S_set S_filter_control_waveform_color, g_set_filter_control_waveform_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_highlight_color, g_highlight_color_w, H_highlight_color, 
			    S_set S_highlight_color, g_set_highlight_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_cursor_color, g_cursor_color_w, H_cursor_color, 
			    S_set S_cursor_color, g_set_cursor_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_text_focus_color, g_text_focus_color_w, H_text_focus_color, 
			    S_set S_text_focus_color, g_set_text_focus_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_sash_color, g_sash_color_w, H_sash_color, 
			    S_set S_sash_color, g_set_sash_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_data_color, g_data_color_w, H_data_color, 
			    S_set S_data_color, g_set_data_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_graph_color, g_graph_color_w, H_graph_color, 
			    S_set S_graph_color, g_set_graph_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_selected_graph_color, g_selected_graph_color_w, H_selected_graph_color, 
			    S_set S_selected_graph_color, g_set_selected_graph_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_selected_data_color, g_selected_data_color_w, H_selected_data_color, 
			    S_set S_selected_data_color, g_set_selected_data_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_axis_color, g_axis_color_w, H_axis_color, 
			    S_set S_axis_color, g_set_axis_color_w,  0, 0, 1, 0, pcl_p, pcl_p);
  Xen_define_typed_dilambda(S_basic_color, g_basic_color_w, H_basic_color, 
			    S_set S_basic_color, g_set_basic_color_w,  0, 0, 1, 0, pcl_p, pcl_p);

  Xen_define_typed_procedure(S_is_color,      g_is_color_w,       1, 0, 0, H_is_color,      s7_make_signature(s7, 2, b, p));
  Xen_define_typed_procedure(S_make_color,    g_make_color_w,     3, 1, 0, H_make_color,    s7_make_circular_signature(s7, 1, 2, p, r));
  Xen_define_typed_procedure(S_color_to_list, g_color_to_list_w,  1, 0, 0, H_color_to_list, s7_make_signature(s7, 2, p, p));

  Xen_define_typed_procedure(S_make_bezier,   g_make_bezier_w,    0, 0, 1,  H_make_bezier,  s7_make_circular_signature(s7, 1, 2, v, r));
  Xen_define_typed_procedure(S_snd_gcs,       g_snd_gcs_w,        0, 0, 0,  H_snd_gcs,      s7_make_signature(s7, 1, p));
  Xen_define_typed_procedure(S_snd_color,     g_snd_color_w,      1, 0, 0,  H_snd_color,    s7_make_signature(s7, 2, p, i));
  Xen_define_typed_procedure(S_snd_font,      g_snd_font_w,       1, 0, 0,  H_snd_font,     s7_make_signature(s7, 2, p, i));

#if 1
  Xen_define_typed_procedure(S_make_cairo,    g_make_cairo_w,     1, 0, 0,  H_make_cairo,   s7_make_signature(s7, 2, p, t));
  Xen_define_typed_procedure(S_free_cairo,    g_free_cairo_w,     1, 0, 0,  H_free_cairo,   s7_make_signature(s7, 2, b, p));
#endif

  #define H_new_widget_hook S_new_widget_hook " (widget): called each time a dialog or \
a new set of channel or sound widgets is created."

  new_widget_hook = Xen_define_hook(S_new_widget_hook, "(make-hook 'widget)", 1, H_new_widget_hook);

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->data_color_symbol, s7_make_function(s7, "[acc-" S_data_color "]", acc_data_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->highlight_color_symbol, s7_make_function(s7, "[acc-" S_highlight_color "]", acc_highlight_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->axis_color_symbol, s7_make_function(s7, "[acc-" S_axis_color "]", acc_axis_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->sash_color_symbol, s7_make_function(s7, "[acc-" S_sash_color "]", acc_sash_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->filter_control_waveform_color_symbol, s7_make_function(s7, "[acc-" S_filter_control_waveform_color "]", acc_filter_control_waveform_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->mix_color_symbol, s7_make_function(s7, "[acc-" S_mix_color "]", acc_mix_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->selected_data_color_symbol, s7_make_function(s7, "[acc-" S_selected_data_color "]", acc_selected_data_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->mark_color_symbol, s7_make_function(s7, "[acc-" S_mark_color "]", acc_mark_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->graph_color_symbol, s7_make_function(s7, "[acc-" S_graph_color "]", acc_graph_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->selected_graph_color_symbol, s7_make_function(s7, "[acc-" S_selected_graph_color "]", acc_selected_graph_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->listener_color_symbol, s7_make_function(s7, "[acc-" S_listener_color "]", acc_listener_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->listener_text_color_symbol, s7_make_function(s7, "[acc-" S_listener_text_color "]", acc_listener_text_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->basic_color_symbol, s7_make_function(s7, "[acc-" S_basic_color "]", acc_basic_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->selection_color_symbol, s7_make_function(s7, "[acc-" S_selection_color "]", acc_selection_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->zoom_color_symbol, s7_make_function(s7, "[acc-" S_zoom_color "]", acc_zoom_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->position_color_symbol, s7_make_function(s7, "[acc-" S_position_color "]", acc_position_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->enved_waveform_color_symbol, s7_make_function(s7, "[acc-" S_enved_waveform_color "]", acc_enved_waveform_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->cursor_color_symbol, s7_make_function(s7, "[acc-" S_cursor_color "]", acc_cursor_color, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->text_focus_color_symbol, s7_make_function(s7, "[acc-" S_text_focus_color "]", acc_text_focus_color, 2, 0, false, "accessor"));

  s7_symbol_set_documentation(s7, ss->axis_color_symbol, "*axis-color*: color of axis (defaults to current data color)");
  s7_symbol_set_documentation(s7, ss->basic_color_symbol, "*basic-color*: Snd's basic color");
  s7_symbol_set_documentation(s7, ss->cursor_color_symbol, "*cursor-color*: cursor color");
  s7_symbol_set_documentation(s7, ss->data_color_symbol, "*data-color*: color used to draw unselected data");
  s7_symbol_set_documentation(s7, ss->enved_waveform_color_symbol, "*enved-waveform-color*: color of the envelope editor wave display");
  s7_symbol_set_documentation(s7, ss->filter_control_waveform_color_symbol, "*filter-control-waveform-color*: color of the filter waveform");
  s7_symbol_set_documentation(s7, ss->graph_color_symbol, "*graph-color*: background color used for unselected data");
  s7_symbol_set_documentation(s7, ss->highlight_color_symbol, "*highlight-color*: color of highlighted text or buttons");
  s7_symbol_set_documentation(s7, ss->listener_color_symbol, "*listener-color*: background color of the lisp listener");
  s7_symbol_set_documentation(s7, ss->listener_text_color_symbol, "*listener-text-color*: text color in the lisp listener");
  s7_symbol_set_documentation(s7, ss->mark_color_symbol, "*mark-color*: mark color");
  s7_symbol_set_documentation(s7, ss->mix_color_symbol, "*mix-color*: color of mix tags");
  s7_symbol_set_documentation(s7, ss->position_color_symbol, "*position-color*: color of position sliders");
  s7_symbol_set_documentation(s7, ss->sash_color_symbol, "*sash-color*: color used to draw paned window sashes");
  s7_symbol_set_documentation(s7, ss->selected_data_color_symbol, "*selected-data-color*: color used for selected data");
  s7_symbol_set_documentation(s7, ss->selected_graph_color_symbol, "*selected-graph-color*: background color of selected data");
  s7_symbol_set_documentation(s7, ss->selection_color_symbol, "*selection-color*: selection color");
  s7_symbol_set_documentation(s7, ss->text_focus_color_symbol, "*text-focus-color*: color used to show a text field has focus");
  s7_symbol_set_documentation(s7, ss->zoom_color_symbol, "*zoom-color*: color of zoom sliders");
#endif
}

#else
/* no gui -- extension lang tie-ins are in snd-nogui.c */
void set_grf_points(int xi, int j, int ymin, int ymax) {}
void set_grf_point(int xi, int j, int yi) {}
void draw_grf_points(int dot_size, graphics_context *ax, int j, axis_info *ap, mus_float_t y0, graph_style_t graph_style) {}
void draw_both_grf_points(int dot_size, graphics_context *ax, int j, graph_style_t graph_style) {}
void draw_cursor(chan_info *cp) {}
point_t *get_grf_points(void) {return(NULL);} 
point_t *get_grf_points1(void) {return(NULL);}
bool foreground_color_ok(Xen color, graphics_context *ax) {return(true);}
#endif
