#include "snd.h"

#if USE_MOTIF
  #define BIG_DOT_SIZE 10
  #define MEDIUM_DOT_SIZE 7
  #define LITTLE_DOT_SIZE 4
#else
  #define BIG_DOT_SIZE 5
  #define MEDIUM_DOT_SIZE 3
  #define LITTLE_DOT_SIZE 2
#endif

static int current_dot_size = BIG_DOT_SIZE;

#define MAX_PIXEL 10000


env *free_env(env *e)
{
  if (e)
    {
      if (e->data) {free(e->data); e->data = NULL;}
      free(e);
    }
  return(NULL);
}


env *copy_env(env *e)
{
  if (e)
    {
      env *ne;
      ne = (env *)calloc(1, sizeof(env));
      ne->pts = e->pts;
      ne->data_size = e->pts * 2;
      ne->data = (mus_float_t *)malloc(ne->data_size * sizeof(mus_float_t));
      mus_copy_floats(ne->data, e->data, ne->data_size);
      ne->base = e->base;
      return(ne);
    }
  return(NULL);
}


bool envs_equal(env *e1, env *e2)
{
  /* snd-mix.c check for set mix amp env no-op */
  int i;
  if (!e1) return(!e2);
  if (!e2) return(false);
  if (e1->pts != e2->pts) return(false);
  for (i = 0; i < e1->pts * 2; i++)
    if (e1->data[i] != e2->data[i])
      return(false);
  if (e1->base != e2->base) return(false); /* 1 and 0 are possibilities here */
  return(true);
}


char *env_to_string(env *e)
{
  char *news = NULL;
  if (e)
    {
      int i, j, len;
      bool first = true;
      char *expr_buf;
      len = 4 + (e->pts * 2 * 16);
      news = (char *)calloc(len, sizeof(char));
#if HAVE_RUBY
      news[0] = '[';
#endif
#if HAVE_SCHEME
      news[0] = '\'';
      news[1] = '(';
#endif
#if HAVE_FORTH
      news[0] = '\'';
      news[1] = '(';
      news[2] = ' ';
#endif
      expr_buf = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      for (i = 0, j = 0; i < e->pts; i++, j += 2)
	{
	  if (fabs(e->data[j + 1]) < .0000001) e->data[j + 1] = 0.0; /* try to get rid of -0.000 */
#if HAVE_RUBY
	  snprintf(expr_buf, PRINT_BUFFER_SIZE, "%s%.3f, %.3f", (first) ? "" : ", ", e->data[j], e->data[j + 1]);
#endif
#if HAVE_SCHEME || HAVE_FORTH
	  snprintf(expr_buf, PRINT_BUFFER_SIZE, "%s%.3f %.3f", (first) ? "" : " ", e->data[j], e->data[j + 1]);
#endif
	  news = mus_strcat(news, expr_buf, &len);
	  first = false;
	}
      free(expr_buf);
#if HAVE_RUBY
      news = mus_strcat(news, "]", &len);
#endif
#if HAVE_SCHEME
      news = mus_strcat(news, ")", &len);
#endif
#if HAVE_FORTH
      news = mus_strcat(news, " )", &len);
#endif
    }
  else news = mus_strdup(PROC_FALSE);
  return(news);
}


env *make_envelope_with_offset_and_scaler(mus_float_t *env_buffer, int len, mus_float_t offset, mus_float_t scaler)
{
  env *e;
  int i, flen;
  if (len < 4) flen = 4; else flen = len;
  if (flen & 1) flen++;
  e = (env *)calloc(1, sizeof(env));
  e->data = (mus_float_t *)calloc(flen, sizeof(mus_float_t));
  e->data_size = flen;
  e->pts = flen / 2;
  for (i = 0; i < len; i += 2) 
    {
      e->data[i] = env_buffer[i];
      e->data[i + 1] = offset + scaler * env_buffer[i + 1];
    }
  if ((flen == 4) && (len == 2))  /* fixup degenerate envelope */
    {
      e->data[2] = e->data[0] + 1.0; 
      e->data[3] = e->data[1];
    }
  e->base = 1.0;
  return(e);
}


static env *make_envelope(mus_float_t *env_buffer, int len)
{
  return(make_envelope_with_offset_and_scaler(env_buffer, len, 0.0, 1.0));
}


static void add_point(env *e, int pos, mus_float_t x, mus_float_t y)
{
  int i, j;
  if (e->pts * 2 == e->data_size)
    {
      e->data_size += 16;
      e->data = (mus_float_t *)realloc(e->data, (e->data_size) * sizeof(mus_float_t));
    }
  for (i = e->pts - 1, j = (e->pts - 1) * 2; i >= pos; i--, j -= 2)
    {
      e->data[j + 2] = e->data[j];
      e->data[j + 3] = e->data[j + 1];
    }
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
  e->pts++;
}


static void move_point(env *e, int pos, mus_float_t x, mus_float_t y)
{
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
}


static void delete_point(env *e, int pos)
{
  int i, j;
  for (i = pos, j = pos * 2; i < e->pts - 1; i++, j += 2)
    {
      e->data[j] = e->data[j + 2];
      e->data[j + 1] = e->data[j + 3];
    }
  e->pts--;
}


static int place_point(int *cxs, int points, int x, env *e, mus_float_t bx)
{
  int i;
  for (i = 0; i < points; i++)
    {
      if (x == cxs[i])
	{
	  /* use true values to disambiguate */
	  if (e->data[i * 2] > bx)
	    return(i - 1);
	  else return(i);
	}
      else
	{
	  if (x < cxs[i]) 
	    return(i - 1);
	}
    }
  return(points);
}


static int hit_point(int *cxs, int *cys, int points, int x, int y)
{
  /* enved dot size (10) is big enough that we need to search for the closest dot
   *   but I think we can assume that the x's are in order
   */
  int i, cur_i = -1, cur_min_x = MAX_PIXEL, cur_min_y = MAX_PIXEL, lim_x;
  lim_x = x + current_dot_size;
  for (i = 0; (i < points) && (cxs[i] <= lim_x); i++)
    if (((x > (cxs[i] - current_dot_size)) && 
	 (x < (cxs[i] + current_dot_size))) &&
	((y > (cys[i] - current_dot_size)) && 
	 (y < (cys[i] + current_dot_size))))
      {
	if (abs(x - cxs[i]) <= cur_min_x)
	  {
	    if (abs(y - cys[i]) < cur_min_y)
	      {
		cur_i = i;
		cur_min_x = abs(x - cxs[i]);
		cur_min_y = abs(y - cys[i]);
	      }
	  }
      }
  return(cur_i);
}


env *default_env(mus_float_t x1, mus_float_t y)
{
  env *e;
  e = (env *)calloc(1, sizeof(env));
  e->data = (mus_float_t *)calloc(4, sizeof(mus_float_t));
  e->data_size = 4;
  e->pts = 2;
  e->data[0] = 0.0; 
  e->data[1] = y; 
  e->data[2] = x1;
  e->data[3] = y;
  e->base = 1.0;
  return(e);
}


bool is_default_env(env *e)
{
  if (!e) return(true);
  if (e->pts != 2) return(false);
  return((snd_feq(e->data[0], 0.0)) &&
	 (snd_feq(e->data[1], 1.0)) &&
	 (snd_feq(e->data[2], 1.0)) &&
	 (snd_feq(e->data[3], 1.0)));
}


env_editor *new_env_editor(void)
{
  env_editor *edp;
  edp = (env_editor *)calloc(1, sizeof(env_editor));
  edp->current_xs = (int *)calloc(8, sizeof(int));
  edp->current_ys = (int *)calloc(8, sizeof(int));
  edp->axis = (axis_info *)calloc(1, sizeof(axis_info));
  edp->current_size = 8;
  edp->env_dragged = false;
  edp->env_pos = 0;
  edp->click_to_delete = false;
  edp->edited = false;
  edp->clipping = true;
  edp->in_dB = false;
  return(edp);
}


static void env_editor_set_current_point(env_editor *edp, int pos, int x, int y)
{
  if (pos == edp->current_size)
    {
      edp->current_size += 8;
      edp->current_xs = (int *)realloc(edp->current_xs, edp->current_size * sizeof(int));
      edp->current_ys = (int *)realloc(edp->current_ys, edp->current_size * sizeof(int));
    }
  edp->current_xs[pos] = x;
  edp->current_ys[pos] = y;
}


static short env_editor_grf_y_dB(env_editor *edp, mus_float_t val)
{
  if (edp->in_dB)
    return(grf_y(in_dB(min_dB(ss), ss->lin_dB, val), edp->axis));
  else return(grf_y(val, edp->axis));
}


static mus_float_t un_dB(mus_float_t py)
{
  return((py <= min_dB(ss)) ? 0.0 : pow(10.0, py * .05));
}


double env_editor_ungrf_y_dB(env_editor *edp, int y)
{
  if (edp->in_dB)
    return(un_dB(ungrf_y(edp->axis, y)));
  else return(ungrf_y(edp->axis, y));
}


#define EXP_SEGLEN 4
typedef enum {ENVED_ADD_POINT, ENVED_DELETE_POINT, ENVED_MOVE_POINT} enved_point_t;

static bool check_enved_hook(env *e, int pos, mus_float_t x, mus_float_t y, enved_point_t reason);

/* enved display can call mus_make_env which can throw 'mus-error, so we need local protection */
static mus_error_handler_t *old_error_handler;

static void local_mus_error(int type, char *msg)
{
  snd_error_without_format(msg);
}


void env_editor_display_env(env_editor *edp, env *e, graphics_context *ax, const char *name, 
			    int x0, int y0, int width, int height, printing_t printing)
{
  int i;
  mus_float_t ex0, ey0, ex1, ey1;
  axis_info *ap;
  if (e)
    {
      ex0 = e->data[0];
      ey0 = e->data[1];
      ex1 = e->data[(e->pts * 2) - 2];
      ey1 = ey0;
      for (i = 3; i < e->pts * 2; i += 2)
	{
	  mus_float_t val;
	  val = e->data[i];
	  if (ey0 > val) ey0 = val;
	  if (ey1 < val) ey1 = val;
	}
      if (ey0 > 0.0) ey0 = 0.0;
      if ((ey0 == ey1) && (ey1 == 0.0)) ey1 = 1.0; /* fixup degenerate case */
      if ((edp->with_dots) && (ey1 < 1.0)) ey1 = 1.0;
    }
  else
    {
      if (edp != ss->enved) return;
      ex0 = 0.0;
      ex1 = 1.0;
      ey0 = 0.0;
      ey1 = 1.0;
    }
  if (edp->in_dB)
    {
      ey0 = min_dB(ss); 
      ey1 = 0.0;
    }
  if (edp == ss->enved)
    {
      /* don't free edp->axis! */
      ap = enved_make_axis(name, ax, x0, y0, width, height, ex0, ex1, ey0, ey1, printing); /* ax used only for GC here */
      edp->axis = ap;
    }
  else 
    {
      ap = edp->axis;
      ap->ax = ax;
      if (edp->with_dots)
	init_env_axes(ap, name, x0, y0, width, height, ex0, ex1, ey0, ey1, NOT_PRINTING);
    }
  if (!(ax->wn)) return;

  if (e)
    {
      int j, dur;
      mus_float_t curx, xincr;
      int ix0, ix1, iy0, iy1;
      ix1 = grf_x(e->data[0], ap);
      iy1 = env_editor_grf_y_dB(edp, e->data[1]);
      if (edp->with_dots)
	{
	  if (e->pts < 50)
	    current_dot_size = BIG_DOT_SIZE;
	  else
	    {
	      if (e->pts < 100)
		current_dot_size = MEDIUM_DOT_SIZE;
	      else current_dot_size = LITTLE_DOT_SIZE;
	    }
	  env_editor_set_current_point(edp, 0, ix1, iy1);
	  draw_dot(ax, ix1, iy1, current_dot_size);
	}
      if (e->base == 1.0)
	{
	  if (edp->in_dB)
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = env_editor_grf_y_dB(edp, e->data[i + 1]);
		  if (edp->with_dots)
		    {
		      env_editor_set_current_point(edp, j, ix1, iy1);
		      draw_dot(ax, ix1, iy1, current_dot_size);
		    }
		  /* now try to fill in from the last point to this one */
		  if ((ix1 - ix0) < (2 * EXP_SEGLEN))
		    {
		      /* points are too close to be worth interpolating */
		      draw_line(ax, ix0, iy0, ix1, iy1);
		    }
		  else
		    {
		      /* interpolate so the display looks closer to dB */
		      mus_float_t yval, yincr;
		      int lx1, ly1, k;
		      dur = (ix1 - ix0) / EXP_SEGLEN;
		      xincr = (e->data[i] - e->data[i - 2]) / (mus_float_t)dur;
		      curx = e->data[i - 2] + xincr;
		      lx1 = ix0;
		      ly1 = iy0;
		      yval = e->data[i - 1];
		      yincr = (e->data[i + 1] - yval) / (mus_float_t)dur;
		      yval += yincr;
		      for (k = 1; k < dur; k++, curx += xincr, yval += yincr)
			{
			  int lx0, ly0;
			  lx0 = lx1;
			  ly0 = ly1;
			  lx1 = grf_x(curx, ap);
			  ly1 = grf_y(in_dB(min_dB(ss), ss->lin_dB, yval), ap);
			  draw_line(ax, lx0, ly0, lx1, ly1);
			}
		      draw_line(ax, lx1, ly1, ix1, iy1);
		    }
		}
	    }
	  else
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = grf_y(e->data[i + 1], ap);
		  if (edp->with_dots)
		    {
		      env_editor_set_current_point(edp, j, ix1, iy1);
		      draw_dot(ax, ix1, iy1, current_dot_size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		}
	    }
	}
      else
	{
	  if (e->base <= 0.0)
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = env_editor_grf_y_dB(edp, e->data[i + 1]);
		  if (edp->with_dots)
		    {
		      env_editor_set_current_point(edp, j, ix1, iy1);
		      draw_dot(ax, ix1, iy1, current_dot_size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy0);
		  draw_line(ax, ix1, iy0, ix1, iy1);
		  if (printing) 
		    {
		      ps_draw_line(ap, ix0, iy0, ix1, iy0);
		      ps_draw_line(ap, ix1, iy0, ix1, iy1);
		    }
		}
	    }
	  else
	    {
	      int index = 0;
	      mus_float_t env_val;
	      mus_any *ce;
	      if (edp->with_dots)
		for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		  env_editor_set_current_point(edp, j, grf_x(e->data[i], ap), grf_y(e->data[i + 1], ap));

	      /* exponential case */
	      dur = width / EXP_SEGLEN;
	      old_error_handler = mus_error_set_handler(local_mus_error);
	      ce = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, dur - 1, NULL);
	      mus_error_set_handler(old_error_handler);
	      if (!ce) return;
	      if (dur < e->pts) dur = e->pts;
	      env_val = mus_env(ce);
	      ix1 = grf_x(0.0, ap);
	      iy1 = env_editor_grf_y_dB(edp, env_val);
	      xincr = (ex1 - ex0) / (mus_float_t)dur;
	      for (i = 1, curx = ex0 + xincr; i < dur; i++, curx += xincr)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  env_val = mus_env(ce);
		  ix1 = grf_x(curx, ap);
		  iy1 = env_editor_grf_y_dB(edp, env_val);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		  if ((edp->with_dots) && (index != mus_position(ce)))
		    {
		      index = mus_position(ce);
		      if (index < (e->pts - 1))
			draw_dot(ax, ix1, iy1, current_dot_size);
		    }
		}
	      if (curx < ex1)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  ix1 = grf_x(ex1, ap);
		  iy1 = env_editor_grf_y_dB(edp, e->data[e->pts * 2 - 1]);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		}
	      if (edp->with_dots)
		draw_dot(ax, ix1, iy1, current_dot_size);
	      mus_free(ce);
	    }
	}
    }
}


void env_editor_button_motion_with_xy(env_editor *edp, int evx, int evy, oclock_t motion_time, env *e, mus_float_t *new_x, mus_float_t *new_y)
{
  axis_info *ap;
  mus_float_t x0, x1, x, y;
  if ((!e) || (!edp)) return;
  if ((motion_time - edp->down_time) < ss->click_time) return;
  edp->env_dragged = true;
  edp->click_to_delete = false;
  ap = edp->axis;
  x = ungrf_x(ap, evx);
  if (edp->env_pos > 0) 
    x0 = e->data[edp->env_pos * 2 - 2]; 
  else x0 = e->data[0];
  if (edp->env_pos < (e->pts - 1))
    x1 = e->data[edp->env_pos * 2 + 2]; /* looking for next point on right to avoid crossing it */
  else x1 = e->data[e->pts * 2 - 2];
  {
    mus_float_t dist = 0.0001;
    if ((x1 - x0) <= dist)
      dist = (x1 - x0) / 2.0;
    if (x <= x0) x = x0 + dist;
    if (x >= x1) x = x1 - dist;
  }
  if (edp->env_pos == 0) x = e->data[0];
  if (edp->env_pos == (e->pts - 1)) x = e->data[(e->pts - 1) * 2];
  y = ungrf_y(ap, evy);
  if ((edp->clipping) || (edp->in_dB))
    {
      if (y < ap->y0) y = ap->y0;
      if (y > ap->y1) y = ap->y1;
    }
  if (edp->in_dB) y = un_dB(y);
  if ((edp != ss->enved) || 
      (check_enved_hook(e, edp->env_pos, x, y, ENVED_MOVE_POINT) == 0))
    move_point(e, edp->env_pos, x, y);
  edp->edited = true;
  if (new_x) (*new_x) = x;
  if (new_y) (*new_y) = y;
}


void env_editor_button_motion(env_editor *edp, int evx, int evy, oclock_t motion_time, env *e)
{
  env_editor_button_motion_with_xy(edp, evx, evy, motion_time, e, NULL, NULL);
}


bool env_editor_button_press(env_editor *edp, int evx, int evy, oclock_t time, env *e)
{
  int pos;
  mus_float_t x, y;
  axis_info *ap;
  ap = edp->axis;
  edp->down_time = time;
  edp->env_dragged = false;
  pos = hit_point(edp->current_xs, edp->current_ys, e->pts, evx, evy);
  x = ungrf_x(ap, evx);
  y = env_editor_ungrf_y_dB(edp, evy);
  if (edp->clipping)
    {
      if (y < ap->y0) y = ap->y0;
      if (y > ap->y1) y = ap->y1;
    }
  if (pos == -1)
    {
      if (x <= ap->x0)
	{
	  pos = 0;
	  x = ap->x0;
	}
      else 
	{
	if (x >= ap->x1) 
	  {
	    pos = e->pts - 1;
	    x = ap->x1;
	  }
	}
    }
  edp->env_pos = pos;
  /* if not -1, then user clicked existing point -- wait for drag/release to decide what to do */
  if (pos == -1) 
    {
      pos = place_point(edp->current_xs, e->pts, evx, e, x);
      /* place returns left point index of current segment or pts if off left end */
      /* in this case, user clicked in middle of segment, so add point there */
      if ((edp != ss->enved) || (check_enved_hook(e, pos, x, y, ENVED_ADD_POINT) == 0))
	add_point(e, pos + 1, x, y);
      edp->env_pos = pos + 1;
      edp->click_to_delete = false;
    }
  else edp->click_to_delete = true;
  edp->edited = true;
  return(pos == -1);
}


void env_editor_button_release(env_editor *edp, env *e)
{
  if ((edp->click_to_delete) && 
      (!(edp->env_dragged)) && 
      (edp->env_pos > 0) && 
      (edp->env_pos < (e->pts - 1)) &&
      ((edp != ss->enved) || (check_enved_hook(e, edp->env_pos, 0, 0, ENVED_DELETE_POINT) == 0)))
    delete_point(e, edp->env_pos);
  prepare_enved_edit(e);
  edp->env_pos = 0;
  edp->env_dragged = false;
  edp->click_to_delete = false;
}



/* -------- (main) ENVELOPE EDITOR FUNCTIONS -------- */

static int env_list_size = 0;    /* current size of env edits list */
static int env_list_top = 0;     /* one past current active position in list */
static env **env_list = NULL;    /* env edits list (for local undo/redo/revert) */

static env **all_envs = NULL;    /* all envs, either loaded or created in editor */
static char **all_names = NULL;  /* parallel names */
static int all_envs_size = 0;    /* size of this array */
static int all_envs_top = 0;     /* one past pointer to last entry in this array */


void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax, printing_t printing)
{
  if (ap->xlabel) free(ap->xlabel);
  ap->xmin = xmin;
  ap->xmax = xmax;
  ap->ymin = ymin;
  ap->ymax = ymax;
  ap->y_ambit = ap->ymax - ap->ymin;
  ap->x_ambit = ap->xmax - ap->xmin;
  ap->xlabel = mus_strdup(name);
  ap->x0 = xmin;
  ap->x1 = xmax;
  ap->y0 = ymin;
  ap->y1 = ymax;
  ap->width = width;
  ap->window_width = width;
  ap->y_offset = ey0;
  ap->height = height;
  ap->graph_x0 = x_offset;
  make_axes_1(ap, X_AXIS_IN_SECONDS, 1, SHOW_ALL_AXES, printing, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));
  /* if this is too small for an axis, it still sets up the fields needed for grf_x|y, so tiny envelope graphs will work */
}


void view_envs(int env_window_width, int env_window_height, printing_t printing)
{
  /* divide space available into a grid (if needed) that shows all currently defined envelopes */
  /* I suppose if there were several hundred envelopes, we'd need a scrollable viewer... */
  int cols, rows, i, j, width, height, x, y, k;
  if (all_envs_top > 1)
    {
      cols = snd_round(sqrt((mus_float_t)(all_envs_top * env_window_width) / (mus_float_t)env_window_height));
      rows = snd_round((mus_float_t)all_envs_top / (mus_float_t)cols);
      if ((rows * cols) < all_envs_top) rows++;
    }
  else
    {
      cols = 1;
      rows = 1;
    }
  width = (int)((mus_float_t)env_window_width / (mus_float_t)cols);
  height = (int)((mus_float_t)env_window_height / (mus_float_t)rows);
  k = 0;
  for (i = 0, x = 0; i < cols; i++, x += width)
    for (j = 0, y = 0; j < rows; j++, y += height)
      {
	display_enved_env_with_selection(all_envs[k], all_names[k], x, y, width, height, 0, printing);
	k++;
	if (k == all_envs_top) return;
      }
}


int hit_env(int xe, int ye, int env_window_width, int env_window_height)
{
  if (all_envs_top == 0)
    return(-1);
  else
    {
      if (all_envs_top == 1)
	return(0);
      else
	{
	  int cols, rows, i, j, width, height, x, y, k;
	  cols = snd_round(sqrt((mus_float_t)(all_envs_top * env_window_width) / (mus_float_t)env_window_height));
	  rows = snd_round((mus_float_t)all_envs_top / (mus_float_t)cols);
	  if ((rows * cols) < all_envs_top) rows++;
	  width = (int)((mus_float_t)env_window_width / (mus_float_t)cols);
	  height = (int)((mus_float_t)env_window_height / (mus_float_t)rows);
	  k = 0;
	  for (i = 0, x = width; i < cols; i++, x += width)
	    if (x > xe)
	      for (j = 0, y = height; j < rows; j++, y += height)
		{
		  if (y > ye) return(k);
		  k++;
		}
	    else k += rows;
	}
    }
  return(0);
}


void prepare_enved_edit(env *new_env)
{
  int i;
  if (env_list_top == env_list_size)
    {
      env_list_size += 16;
      if (env_list)
	{
	  env_list = (env **)realloc(env_list, env_list_size * sizeof(env *));
	  for (i = env_list_top; i < env_list_size; i++) env_list[i] = NULL;
	}
      else env_list = (env **)calloc(env_list_size, sizeof(env *));
    }
  /* clear out current edit list above this edit */
  for (i = env_list_top; i < env_list_size; i++)
    env_list[i] = free_env(env_list[i]);
  env_list[env_list_top] = copy_env(new_env);
  env_list_top++;
}


void redo_env_edit(void)
{
  if (env_list)
    {
      if ((env_list_top < env_list_size) && 
	  (env_list[env_list_top])) 
	{
	  env_list_top++;
	  set_enved_undo_sensitive(true);
	  set_enved_revert_sensitive(true);
	}
      if ((env_list_top == env_list_size) || 
	  (!env_list[env_list_top])) 
	set_enved_redo_sensitive(false);
      set_enved_save_sensitive(true);
    }
}


void undo_env_edit(void)
{
  if (env_list)
    {
      if (env_list_top > 0)
	{
	  env_list_top--;
	  set_enved_redo_sensitive(true);
	}
      if (env_list_top == 0)
	{
	  set_enved_undo_sensitive(false);
	  /* set_enved_revert_sensitive(false); */
	}
      set_enved_save_sensitive(true);
    }
}


void revert_env_edit(void)
{
  if (env_list)
    {
      if (env_list_top > 0)
	set_enved_redo_sensitive(true);
      if (env_list_top > 1) 
	env_list_top = 1; 
      else 
	{
	  env_list_top = 0;
	  set_enved_undo_sensitive(false);
	  set_enved_revert_sensitive(false);
	  set_enved_save_sensitive(false);
	}
    }
}


static int find_env(const char *name)
{ /* -1 upon failure */
  int i;
  if ((all_envs) && (name))
    for (i = 0; i < all_envs_top; i++)
      if (mus_strcmp(name, all_names[i]))
	return(i);
  return(-1);
}


int enved_all_envs_top(void) {return(all_envs_top);}
char *enved_all_names(int n) {return(all_names[n]);}
void set_enved_env_list_top(int n) {env_list_top = n;}
/* env *enved_all_envs(int pos) {return(all_envs[pos]);} */


static void add_envelope(const char *name, env *val)
{
  if (all_envs_top == all_envs_size)
    {
      all_envs_size += 16;
      if (all_envs)
	{
	  int i;
	  all_envs = (env **)realloc(all_envs, all_envs_size * sizeof(env *));
	  all_names = (char **)realloc(all_names, all_envs_size * sizeof(char *));
	  for (i = all_envs_size - 16; i < all_envs_size; i++) {all_names[i] = NULL; all_envs[i] = NULL;}
	}
      else
	{
	  all_envs = (env **)calloc(all_envs_size, sizeof(env *));
	  all_names = (char **)calloc(all_envs_size, sizeof(char *));
	}
    }
  all_envs[all_envs_top] = val;
  if (all_names[all_envs_top]) free(all_names[all_envs_top]);
  all_names[all_envs_top] = mus_strdup(name);
  all_envs_top++;
  if (enved_dialog_is_active())
    {
      set_enved_show_sensitive(true);
      make_scrolled_env_list();
    }
}


#if 0
void delete_envelope(const char *name)
{
  int pos;
  pos = find_env(name);
  if (pos != -1)
    {
      int i;
      if (all_names[pos]) free(all_names[pos]);
      for (i = pos; i < all_envs_size - 1; i++)
	{
	  all_envs[i] = all_envs[i + 1]; 
	  all_envs[i + 1] = NULL;
	  all_names[i] = all_names[i + 1]; 
	  all_names[i + 1] = NULL;
	}
      all_envs_top--;
      if (enved_dialog_is_active())
	{
	  if (all_envs_top > 0)
	    set_enved_show_sensitive(true);
	  make_scrolled_env_list();
	}
    }
}
#endif


void alert_envelope_editor(const char *name, env *val)
{
  /* whenever an envelope is defined, we get notification through this function */
  int i;
  if (!val) return;
  i = find_env(name);
  if (i != -1)
    {
      free_env(all_envs[i]);
      all_envs[i] = val;
    }
  else add_envelope(name, val);
}



struct enved_fft {
  mus_long_t size;
  mus_float_t *data;
  mus_float_t scale;
};


enved_fft *free_enved_fft(enved_fft *ef)
{
  if (ef)
    {
      if (ef->data) free(ef->data);
      ef->data = NULL;
      free(ef);
    }
  return(NULL);
}


void reflect_enved_fft_change(chan_info *cp)
{
  if ((enved_dialog_is_active()) &&
      (enved_target(ss) == ENVED_SPECTRUM) &&
      (cp == current_channel()))
    env_redisplay();
}



#define DEFAULT_ENVED_MAX_FFT_SIZE 1048576
static mus_long_t enved_max_fft_size = DEFAULT_ENVED_MAX_FFT_SIZE;


static enved_fft *make_enved_spectrum(chan_info *cp)
{
  enved_fft *ef;

  if (!cp->edits[cp->edit_ctr]->fft)
    cp->edits[cp->edit_ctr]->fft = (enved_fft *)calloc(1, sizeof(enved_fft));
  ef = cp->edits[cp->edit_ctr]->fft;

  if ((ef) && 
      (ef->size == 0)) /* otherwise it is presumably already available */
    {
      mus_long_t i, data_len;
      mus_float_t data_max = 0.0;
      snd_fd *sf;

      data_len = cp->axis->hisamp - cp->axis->losamp;
      if (data_len > enved_max_fft_size)
	data_len = enved_max_fft_size;
      if (data_len == 0) return(NULL);

      sf = init_sample_read(cp->axis->losamp, cp, READ_FORWARD);
      if (!sf) return(NULL);

      ef->size = snd_to_int_pow2(data_len);
      ef->data = (mus_float_t *)malloc(ef->size * sizeof(mus_float_t));
      if (!ef->data) return(NULL);

      fourier_spectrum(sf, ef->data, ef->size, data_len, NULL, NULL);
      free_snd_fd(sf);
      for (i = 0; i < ef->size; i++) 
	if (ef->data[i] > data_max) 
	  data_max = ef->data[i];
      if (data_max > 0.0) ef->scale = data_max;
    }
  return(ef);
}


static void display_enved_spectrum(chan_info *cp, enved_fft *ef, axis_info *ap)
{
  if (ef)
    {
      mus_float_t incr, x = 0.0;
      int i = 0;
      mus_long_t hisamp;
      mus_float_t samples_per_pixel;
      ap->losamp = 0;
      ap->hisamp = ef->size - 1;
      ap->y0 = 0.0;
      ap->y1 = ef->scale;
      ap->x0 = 0.0;
      ap->x1 = snd_srate(cp->sound) / 2;
      init_axis_scales(ap);
      hisamp = ef->size / 2;
      incr = (mus_float_t)snd_srate(cp->sound) / (mus_float_t)(ef->size);
      samples_per_pixel = (mus_float_t)((double)hisamp / (mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0));
      if ((samples_per_pixel < 4.0) &&
	  (hisamp < POINT_BUFFER_SIZE))
	{
	  for (i = 0, x = 0.0; i < hisamp; i++, x += incr)
	    set_grf_point(grf_x(x, ap), i, grf_y(ef->data[i], ap));
	  draw_grf_points(1, ap->ax, i, ap, 0.0, GRAPH_LINES);
	}
      else
	{
	  int j = 0;
	  mus_float_t xf = 0.0, ymax;
	  ymax = -1.0;
	  while (i < hisamp)
	    {
	      mus_float_t ina;
	      ina = ef->data[i++];
	      if (ina > ymax) ymax = ina;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_point(grf_x(x, ap), j++, grf_y(ymax, ap));
		  x += (incr * samples_per_pixel); 
		  xf -= samples_per_pixel;
		  ymax = -1.0;
		}
	    }
	  draw_grf_points(1, ap->ax, j, ap, 0.0, GRAPH_LINES);
	}
    }
}


void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, printing_t printing)
{
  printing_t old_printing;
  bool two_sided = false;
  axis_info *active_ap = NULL;
  chan_info *active_channel = NULL;

  if (!(any_selected_sound())) return;

  if ((!gray_ap) || (!ap)) return;
  gray_ap->x_axis_x0 = ap->x_axis_x0;
  gray_ap->x_axis_x1 = ap->x_axis_x1;
  gray_ap->y_axis_y0 = ap->y_axis_y0;
  gray_ap->y_axis_y1 = ap->y_axis_y1;

  active_channel = current_channel();
  if ((!active_channel) || 
      (active_channel->active < CHANNEL_HAS_AXES) ||
      (!active_channel->edits) ||
      ((show_fft) && (!active_channel->fft))) /* added show_fft 27-Dec-18? */
    return;

  old_printing = active_channel->printing;
  active_channel->printing = printing;

  if (show_fft)
    {
      if (enved_max_fft_size < transform_size(ss)) enved_max_fft_size = transform_size(ss);
      if (enved_max_fft_size < active_channel->transform_size) enved_max_fft_size = active_channel->transform_size;
      
#if USE_MOTIF 
      /* uses fft_pix etc in chan_info */
      if ((active_channel->transform_graph_type == GRAPH_AS_SONOGRAM) &&
	  (active_channel->graph_transform_on))
	{
	  /* if the sonogram isn't ready, try to get it 
	   *   this is for frequency envelopes as in animals.scm
	   */
	  if ((!(active_channel->fft_pix)) ||
	      (!(active_channel->fft_pix_ready)))
	    display_channel_fft_data(active_channel);
	}

      if ((active_channel->fft_pix) &&
	  (active_channel->fft_pix_ready) &&
	  (active_channel->transform_graph_type == GRAPH_AS_SONOGRAM) &&
	  (active_channel->graph_transform_on))
	{
	  int old_x0, old_y0;

	  old_x0 = active_channel->fft_pix_x0;
	  old_y0 = active_channel->fft_pix_y0; /* this actually aligns with the top of the enved window */
	  active_channel->fft_pix_x0 = ap->x_axis_x0;
	  active_channel->fft_pix_y0 = ap->y_axis_y1;

	  restore_fft_pix(active_channel, gray_ap->ax);

	  active_channel->fft_pix_x0 = old_x0;
	  active_channel->fft_pix_y0 = old_y0;
	}
      else display_enved_spectrum(active_channel, make_enved_spectrum(active_channel), gray_ap);
#else
      /* for gtk, we need to redisplay the sonogram? 
       */
	/* this is not so slow as to be an annoyance, so maybe it's ok */
      if ((active_channel->transform_graph_type == GRAPH_AS_SONOGRAM) &&
	  (active_channel->graph_transform_on))
	{
	  axis_info *old_ap;
	  int x0, x1, y0, y1;
	  old_ap = active_channel->axis;
	  x0 = old_ap->x_axis_x0;
	  x1 = old_ap->x_axis_x1;
	  y0 = old_ap->y_axis_y0;
	  y1 = old_ap->y_axis_y1;
	  active_channel->axis = gray_ap;
	  active_channel->fft->axis->x_axis_x0 = gray_ap->x_axis_x0;
	  active_channel->fft->axis->x_axis_x1 = gray_ap->x_axis_x1;
	  active_channel->fft->axis->y_axis_y0 = gray_ap->y_axis_y0;
	  active_channel->fft->axis->y_axis_y1 = gray_ap->y_axis_y1;
	  make_sonogram(active_channel);
	  active_channel->axis = old_ap;
	  old_ap->x_axis_x0 = x0;
	  old_ap->x_axis_x1 = x1;
	  old_ap->y_axis_y0 = y0;
	  old_ap->y_axis_y1 = y1;
	}
      else
	display_enved_spectrum(active_channel, make_enved_spectrum(active_channel), gray_ap);
#endif
    }
  else
    {
      mus_long_t samps;
      graph_type_t old_time_graph_type = GRAPH_ONCE;
      int srate, pts = 0;
      active_ap = active_channel->axis;
      if (apply_to_selection)
	{
	  if (!(selection_is_active())) return;
	  samps = selection_len();
	  srate = selection_srate();
	  gray_ap->losamp = selection_beg(NULL);
	  gray_ap->hisamp = gray_ap->losamp + samps - 1;
	  gray_ap->x0 = (double)(gray_ap->losamp) / (double)srate;
	  gray_ap->x1 = (double)(gray_ap->hisamp) / (double)srate;
	  gray_ap->y0 = active_ap->y0;
	  gray_ap->y1 = active_ap->y1;
	}
      else
	{
	  /* show current channel overall view in gray scale */
	  samps = current_samples(active_channel);
	  srate = snd_srate(active_channel->sound);
	  gray_ap->losamp = 0;
	  gray_ap->hisamp = samps - 1;
	  if (active_channel->time_graph_type == GRAPH_AS_WAVOGRAM)
	    {
	      gray_ap->y0 = -1.0;
	      gray_ap->y1 = 1.0;
	    }
	  else
	    {
	      gray_ap->y0 = active_ap->y0;
	      gray_ap->y1 = active_ap->y1;
	    }
	  gray_ap->x0 = 0.0;
	  gray_ap->x1 = (double)samps / (double)srate;
	}
      init_axis_scales(gray_ap);
      active_channel->axis = gray_ap;
      old_time_graph_type = active_channel->time_graph_type;
      active_channel->time_graph_type = GRAPH_ONCE;
      pts = make_background_graph(active_channel, srate, &two_sided);
      active_channel->time_graph_type = old_time_graph_type;
      active_channel->axis = active_ap;
      if (pts > 0) 
	{
	  if (two_sided)
	    draw_both_grf_points(1, gray_ap->ax, pts, GRAPH_LINES);
	  else draw_grf_points(1, gray_ap->ax, pts, gray_ap, 0.0, GRAPH_LINES);
	}
    }
  active_channel->printing = old_printing;
}


env *enved_next_env(void)
{
  if (env_list_top > 0) 
    return(copy_env(env_list[env_list_top - 1])); 
  else return(NULL);
}


char *env_name_completer(widget_t w, const char *text, void *data)
{
  int matches = 0;
  char *current_match = NULL;
  if ((all_envs) && (text) && (*text))
    {
      int i, j, len, curlen;
      len = strlen(text);
      for (i = 0; i < all_envs_top; i++)
	if (strncmp(text, all_names[i], len) == 0)
	  {
	    matches++;
	    add_possible_completion(all_names[i]);
	    if (!current_match)
	      current_match = mus_strdup(all_names[i]);
	    else 
	      {
		curlen = strlen(current_match);
		for (j = 0; j < curlen; j++)
		  if (current_match[j] != all_names[i][j])
		    {
		      current_match[j] = '\0';
		      break;
		    }
	      }
	  }
    }
  set_completion_matches(matches);
  if ((current_match) && (*current_match))
    return(current_match);
  return(mus_strdup(text));
}


void save_envelope_editor_state(FILE *fd)
{
  int i;
  for (i = 0; i < all_envs_top; i++)
    {
      char *estr;
      estr = env_to_string(all_envs[i]);
      if (estr)
	{
#if HAVE_SCHEME
	  fprintf(fd, "(%s %s %s %.4f)\n", S_define_envelope, all_names[i], estr, all_envs[i]->base);
#endif
#if HAVE_RUBY
	  {
	    char *name;
	    name = xen_scheme_procedure_to_ruby(S_define_envelope);
	    fprintf(fd, "%s(\"%s\", %s, %.4f)\n", name, all_names[i], estr, all_envs[i]->base);
	    free(name);
	  }
#endif
#if HAVE_FORTH
	  fprintf(fd, "\"%s\" %s %.4f %s drop\n", all_names[i], estr, all_envs[i]->base, S_define_envelope);
#endif
	  free(estr);
	}
    }
}


env *xen_to_env(Xen res)
{
  env *rtn = NULL;
  if (Xen_is_list(res))
    {
      int len = 0;
      len = Xen_list_length(res);
      if (len > 0)
	{
	  int i;
	  mus_float_t *data = NULL;
	  Xen lst;
	  
	  if (Xen_is_number(Xen_car(res)))
	    {
	      data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	      for (i = 0, lst = Xen_copy_arg(res); i < len; i++, lst = Xen_cdr(lst))
		{
		  Xen el;
		  el = Xen_car(lst);
		  data[i] = Xen_real_to_C_double(el);
		}
	    }
	  else
	    {
	      /* embedded lists '((0 0) (100 1)) */
	      if (Xen_is_list(Xen_car(res)))
		{
		  len *= 2;
		  data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
		  for (i = 0, lst = Xen_copy_arg(res); i < len; i += 2, lst = Xen_cdr(lst))
		    {
		      Xen el;
		      el = Xen_car(lst);
		      if ((!Xen_is_pair(el)) ||
			  (!(Xen_is_number(Xen_car(el)))) ||
			  (!(Xen_is_pair(Xen_cdr(el)))) ||
			  (!(Xen_is_number(Xen_cadr(el)))))
			{
			  free(data);
			  return(NULL);
			}
		      data[i] = Xen_real_to_C_double(Xen_car(el));
		      data[i + 1] = Xen_real_to_C_double(Xen_cadr(el));
		    }
		}
	      else
		{
		  /* something is screwed up */
		  return(NULL);
		}
	    }
	  if (data)
	    {
	      if (len > 1)
		rtn = make_envelope(data, len);
	      free(data);
	    }
	}
    }
  return(rtn);
}


static bool x_increases(Xen res)
{
  int i, len;
  Xen lst;
  mus_float_t x;
  len = Xen_list_length(res);
  x = Xen_real_to_C_double(Xen_car(res));
  for (i = 2, lst = Xen_cddr(Xen_copy_arg(res)); i < len; i += 2, lst = Xen_cddr(lst))
    {
      mus_float_t nx;
      nx = Xen_real_to_C_double(Xen_car(lst));
      if (x >= nx) return(false);
      x = nx;
    }
  return(true);
}


#if (!HAVE_EXTENSION_LANGUAGE)
  #define ENV_BUFFER_SIZE 128
  static int env_buffer_size = 0;
  static mus_float_t *env_buffer = NULL;
  static char env_white_space[5] = {' ', '(', ')', '\t', '\''};
#endif


env *string_to_env(const char *str) 
{
#if HAVE_EXTENSION_LANGUAGE
  Xen res;
  res = snd_catch_any(eval_str_wrapper, (void *)str, "string->env");
  if (Xen_is_list(res))
    {
      int len;
      len = Xen_list_length(res);
      if ((len % 2) == 0)
	{
	  if (x_increases(res))
	    return(xen_to_env(res));
	  else snd_error("x axis points not increasing: %s", str);
	}
      else snd_error("odd length envelope? %s", str);
    }
  else snd_error("%s is not a list", str);
  return(NULL);
#else
  char *tok, *tmp;
  int i;
  float f;
  if ((str) && (*str))
    {
      char *old_tmp;
      tmp = mus_strdup(str);
      old_tmp = tmp;
      i = 0;
      if (env_buffer_size == 0)
	{
	  env_buffer_size = ENV_BUFFER_SIZE;
	  env_buffer = (mus_float_t *)calloc(ENV_BUFFER_SIZE, sizeof(mus_float_t));
	}
      if ((*tmp) == '\'') tmp++;
      if ((*tmp) == '(') tmp++;
      tok = strtok(tmp, env_white_space);
      while (tok)
	{
	  if (!(sscanf(tok, "%f", &f)))
	    {
	      snd_error("%s in env list is not a number", tok);
	      return(NULL);
	    }
	  env_buffer[i] = (mus_float_t)f;
	  i++;
	  if (i == env_buffer_size)
	    {
	      env_buffer_size *= 2;
	      env_buffer = (mus_float_t *)realloc(env_buffer, env_buffer_size * sizeof(mus_float_t));
	    }
	  tok = strtok(NULL, env_white_space);
	}
      if ((i == 0) || (i & 1)) 
	snd_error("odd length envelope? %s", str);
      free(old_tmp);
      return(make_envelope(env_buffer, i));
    }
  return(NULL);
#endif
}


env *position_to_env(int pos)
{
  if (pos < 0) return(NULL);
  return(copy_env(all_envs[pos]));
}


env *name_to_env(const char *str)
{
  env *e;
  int pos;
  if ((!str) || (!(*str))) return(NULL);
  pos = find_env(str);
  if (pos >= 0) return(copy_env(all_envs[pos]));
#if HAVE_SCHEME || HAVE_FORTH
  e = xen_to_env(C_string_to_Xen_value(str));
#else
  e = xen_to_env(Xen_eval_C_string((char *)str));
#endif
  return(e);
}


#if HAVE_RUBY
#define SND_ENV_MAX_VARS 100
static Xen snd_env_array[SND_ENV_MAX_VARS];
static int env_index = -1;
#endif


static Xen g_define_envelope(Xen name, Xen data, Xen base)
{
  env *e;
  const char *ename;

  #define H_define_envelope "(" S_define_envelope " name data :optional base): load 'name' with associated 'data', a list of breakpoints \
into the envelope editor."

  Xen_check_type(Xen_is_string(name) || Xen_is_symbol(name), name, 1, S_define_envelope, "a string or symbol");
  Xen_check_type(Xen_is_list(data), data, 2, S_define_envelope, "a list of breakpoints");
  Xen_check_type(Xen_is_number_or_unbound(base) || Xen_is_false(base), base, 3, S_define_envelope, "a float or " PROC_FALSE);

  if (Xen_is_string(name))
    ename = Xen_string_to_C_string(name);
  else ename = Xen_symbol_to_C_string(name);

  e = xen_to_env(data);
  if (!e) return(Xen_false);

  if (Xen_is_number(base))
    e->base = Xen_real_to_C_double(base);

#if HAVE_RUBY
  {
    char *name;
    alert_envelope_editor(name = xen_scheme_global_variable_to_ruby(ename), e);
    if (env_index >= SND_ENV_MAX_VARS)
      env_index = 0;
    else
      env_index++;
    Xen_define_variable(ename, snd_env_array[env_index], data); /* need global C variable */
    free(name);
    return(snd_env_array[env_index]);
  }
#endif

#if HAVE_SCHEME || HAVE_FORTH
  {
    Xen temp;
    alert_envelope_editor(ename, e);
    Xen_define_variable(ename, temp, data); /* already gc protected */
    return(temp);
  }
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
  return(0);
#endif
}


Xen env_to_xen(env *e)
{
  if (e) 
    return(mus_array_to_list(e->data, 0, e->pts * 2));
  return(Xen_empty_list);
}


void add_or_edit_symbol(const char *name, env *val)
{
  /* called from envelope editor -- pass new definition into scheme */
#if HAVE_RUBY
  char *buf, *tmpstr = NULL;
  int len;

  if (!val) return;
  tmpstr = env_to_string(val);
  len = mus_strlen(tmpstr) + mus_strlen(name) + 32;
  buf = (char *)calloc(len, sizeof(char));
  snprintf(buf, len, "%s = %s", name, tmpstr);
  if (tmpstr) free(tmpstr);

  snd_catch_any(eval_str_wrapper, buf, buf);

  free(buf);
#endif

#if HAVE_SCHEME
  Xen e;
  if (!val) return;
  if (Xen_is_defined(name))
    {
      e = s7_make_symbol(s7, name);
      Xen_variable_set(e, env_to_xen(val));
    }
  else Xen_define_variable(name, e, env_to_xen(val));
#endif

#if HAVE_FORTH
  if (!val) return;
  if (Xen_is_defined(name))
    Xen_variable_set(name, env_to_xen(val));
  else fth_define_variable(name, env_to_xen(val), NULL);
#endif
}


env *get_env(Xen e, const char *origin) /* list in e */
{
  int i, len = 0;
  env *new_env;

  Xen_check_type(Xen_is_list(e), e, 1, origin, "a list");
  len = Xen_list_length(e);
  if (len == 0)
    Xen_error(NO_DATA,
	      Xen_list_3(C_string_to_Xen_string("~A: null env, ~A"), 
			 C_string_to_Xen_string(origin), 
			 e));
  new_env = xen_to_env(e);
  if (!new_env)
    Xen_error(Xen_make_error_type("env-error"),
	      Xen_list_2(C_string_to_Xen_string("envelope break point list is screwed up: ~A"),
			 e));

  for (i = 2; i < new_env->pts * 2; i += 2)
    if (new_env->data[i - 2] > new_env->data[i])
      {
	Xen msg;
	char *buf;
	buf = (char *)calloc(1024, sizeof(char));
	snprintf(buf, 1024, "%s: env at breakpoint %d: x axis value %f > %f", origin, i / 2, new_env->data[i - 2], new_env->data[i]);
	msg = C_string_to_Xen_string(buf);
	free(buf);
	free_env(new_env);
	Xen_error(Xen_make_error_type("env-error"),
		  Xen_list_3(C_string_to_Xen_string("~A, ~A"),
			     msg,
			     e));
      }
  return(new_env);
}


static Xen g_save_envelopes(Xen filename)
{
  #define H_save_envelopes "(" S_save_envelopes " :optional filename): save the envelopes known to the envelope editor in filename"
  char *name = NULL;

  Xen_check_type((Xen_is_string(filename) || (Xen_is_false(filename)) || (!Xen_is_bound(filename))), 
		  filename, 1, S_save_envelopes, "a string or " PROC_FALSE);
  if (Xen_is_string(filename)) 
    name = mus_expand_filename(Xen_string_to_C_string(filename));
  else name = mus_strdup("envs.save");
  
  if (name)
    {
      FILE *fd;
      fd = FOPEN(name, "w");
      if (fd) 
	{
	  save_envelope_editor_state(fd);
	  snd_fclose(fd, name);
	}
      free(name);

      if (!fd)
	{
	  Xen_error(Xen_make_error_type("cannot-save"),
		    Xen_list_3(C_string_to_Xen_string(S_save_envelopes ": can't save ~S, ~A"),
			       filename,
			       C_string_to_Xen_string(snd_open_strerror())));
	}
    }
  return(filename);
}


static Xen enved_hook;

static bool check_enved_hook(env *e, int pos, mus_float_t x, mus_float_t y, enved_point_t reason)
{
  if (Xen_hook_has_list(enved_hook))
    {
      Xen result = Xen_false;
      /* if hook procedure returns a list, that is the new contents of the
       * envelope -- if its length doesn't match current, we need to remake
       * current. Otherwise return 0, and assume the caller will handle default
       */

#if HAVE_SCHEME
      result = s7_call(s7, enved_hook, 
		       Xen_list_5(env_to_xen(e),
				  C_int_to_Xen_integer(pos),
				  C_double_to_Xen_real(x),
				  C_double_to_Xen_real(y),
				  C_int_to_Xen_integer((int)reason)));
#else
      {
	Xen procs, env_list;
	env_list = env_to_xen(e);
	procs = Xen_hook_list(enved_hook);
	while (!Xen_is_null(procs))
	  {
	    Xen temp;
	    temp = Xen_apply(Xen_car(procs), 
			       Xen_list_5(env_list,
					  C_int_to_Xen_integer(pos),
					  C_double_to_Xen_real(x),
					  C_double_to_Xen_real(y),
					  C_int_to_Xen_integer((int)reason)),
			       S_enved_hook);
	    if (!Xen_is_false(temp))
	      {
		result = temp;
		env_list = temp;
	      }
	    procs = Xen_cdr(procs);
	  }
      }
#endif
      if (Xen_is_list(result))
	{
	  int i, len;
	  Xen lst;
	  len = Xen_list_length(result);
	  if (len > e->data_size)
	    {
	      free(e->data);
	      e->data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	      e->data_size = len;
	    }
	  e->pts = len / 2;
	  for (i = 0, lst = Xen_copy_arg(result); i < len; i++, lst = Xen_cdr(lst))
	    e->data[i] = Xen_real_to_C_double(Xen_car(lst));
	  return(true);
	}
    }
  return(false);
}


static Xen g_enved_base(void) {return(C_double_to_Xen_real(enved_base(ss)));}

static Xen g_set_enved_base(Xen val) 
{
  #define H_enved_base "(" S_enved_base "): envelope editor exponential base value (1.0)"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_enved_base, "a number"); 
  set_enved_base(mus_fclamp(0.0, Xen_real_to_C_double(val), 300000.0));
  return(C_double_to_Xen_real(enved_base(ss)));
}


static Xen g_enved_power(void) {return(C_double_to_Xen_real(enved_power(ss)));}

static Xen g_set_enved_power(Xen val) 
{
  #define H_enved_power "(" S_enved_power "): envelope editor base scale range (9.0^power)"
  Xen_check_type(Xen_is_number(val), val, 1, S_set S_enved_power, "a number"); 
  set_enved_power(mus_fclamp(0.0, Xen_real_to_C_double(val), 10.0));
  return(C_double_to_Xen_real(enved_power(ss)));
}


static Xen g_enved_clipping(void) {return(C_bool_to_Xen_boolean(enved_clipping(ss)));}

static Xen g_set_enved_clipping(Xen on)
{
  #define H_enved_clipping "(" S_enved_clipping "): envelope editor clip button setting; \
if clipping, the motion of the mouse is restricted to the current graph bounds."

  Xen_check_type(Xen_is_boolean(on), on, 1, S_set S_enved_clipping, "a boolean");
  set_enved_clipping(Xen_boolean_to_C_bool(on)); 
  return(C_bool_to_Xen_boolean(enved_clipping(ss)));
}


static Xen g_enved_style(void) {return(C_int_to_Xen_integer(enved_style(ss)));}

static Xen g_set_enved_style(Xen val) 
{
  #define H_enved_style "(" S_enved_style "): envelope editor breakpoint connection choice: can \
be " S_envelope_linear ", or " S_envelope_exponential

  int choice;
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_enved_style, S_envelope_linear ", or " S_envelope_exponential);
  choice = Xen_integer_to_C_int(val);
  if ((choice == ENVELOPE_LINEAR) || (choice == ENVELOPE_EXPONENTIAL))
    {
      set_enved_style((env_type_t)choice);
      reflect_enved_style();
    }
  else Xen_out_of_range_error(S_enved_style, 1, val, S_enved_style " should be " S_envelope_linear ", or " S_envelope_exponential);
  return(val);
}


static Xen g_enved_target(void) {return(C_int_to_Xen_integer((int)enved_target(ss)));}

static Xen g_set_enved_target(Xen val) 
{
  int in_n;

  #define H_enved_target "(" S_enved_target "): determines how the envelope edit envelope is applied; \
choices are " S_enved_amplitude ", " S_enved_srate "(apply to speed), and " S_enved_spectrum "(apply as a filter)."

  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_enved_target, "an integer"); 
  in_n = Xen_integer_to_C_int(val);
  if ((in_n < 0) ||
      (in_n > (int)ENVED_SRATE))
    Xen_out_of_range_error(S_set S_enved_target, 1, val, S_enved_target " should be " S_enved_amplitude ", " S_enved_srate ", or " S_enved_spectrum);
  else
    {
      enved_target_t n;
      n = (enved_target_t)in_n;
      /* there is a huge bug in some versions of g++ that make it necessary to: */
      if (in_n < 0) n = ENVED_AMPLITUDE;
      if (in_n > 2) n = ENVED_SRATE;
      set_enved_target(n); 
    }
  return(C_int_to_Xen_integer((int)enved_target(ss)));
}


static Xen g_enved_with_wave(void) {return(C_bool_to_Xen_boolean(enved_with_wave(ss)));}

static Xen g_set_enved_with_wave(Xen val) 
{
  #define H_enved_with_wave "(" S_enved_with_wave "): " PROC_TRUE " if the envelope editor is displaying the waveform to be edited"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_enved_with_wave, "a boolean");
  set_enved_with_wave(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(enved_with_wave(ss)));
}


static Xen g_enved_in_dB(void) {return(C_bool_to_Xen_boolean(enved_in_dB(ss)));}

static Xen g_set_enved_in_dB(Xen val) 
{
  #define H_enved_in_dB "(" S_enved_in_dB "): " PROC_TRUE " if the envelope editor is using dB"
  Xen_check_type(Xen_is_boolean(val), val, 1, S_set S_enved_in_dB, "a boolean");
  set_enved_in_dB(Xen_boolean_to_C_bool(val)); 
  return(C_bool_to_Xen_boolean(enved_in_dB(ss)));
}


static Xen g_enved_filter_order(void) {return(C_int_to_Xen_integer(enved_filter_order(ss)));}

static Xen g_set_enved_filter_order(Xen val) 
{
  #define H_enved_filter_order "(" S_enved_filter_order "): envelope editor's FIR filter order (40)"
  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_enved_filter_order, "an integer"); 
  set_enved_filter_order(Xen_integer_to_C_int(val));
  return(C_int_to_Xen_integer(enved_filter_order(ss)));
}


static Xen g_enved_dialog(void) 
{
  #define H_enved_dialog "(" S_enved_dialog "): start the Envelope Editor"
  return(Xen_wrap_widget(create_envelope_editor()));
}


Xen_wrap_no_args(g_enved_base_w, g_enved_base)
Xen_wrap_1_arg(g_set_enved_base_w, g_set_enved_base)
Xen_wrap_no_args(g_enved_power_w, g_enved_power)
Xen_wrap_1_arg(g_set_enved_power_w, g_set_enved_power)
Xen_wrap_no_args(g_enved_clipping_w, g_enved_clipping)
Xen_wrap_1_arg(g_set_enved_clipping_w, g_set_enved_clipping)
Xen_wrap_no_args(g_enved_style_w, g_enved_style)
Xen_wrap_1_arg(g_set_enved_style_w, g_set_enved_style)
Xen_wrap_no_args(g_enved_target_w, g_enved_target)
Xen_wrap_1_arg(g_set_enved_target_w, g_set_enved_target)
Xen_wrap_no_args(g_enved_with_wave_w, g_enved_with_wave)
Xen_wrap_1_arg(g_set_enved_with_wave_w, g_set_enved_with_wave)
Xen_wrap_no_args(g_enved_in_dB_w, g_enved_in_dB)
Xen_wrap_1_arg(g_set_enved_in_dB_w, g_set_enved_in_dB)
Xen_wrap_no_args(g_enved_filter_order_w, g_enved_filter_order)
Xen_wrap_1_arg(g_set_enved_filter_order_w, g_set_enved_filter_order)
Xen_wrap_no_args(g_enved_dialog_w, g_enved_dialog)
Xen_wrap_1_optional_arg(g_save_envelopes_w, g_save_envelopes)
Xen_wrap_3_optional_args(g_define_envelope_w, g_define_envelope)

#if HAVE_SCHEME
static s7_pointer acc_enved_base(s7_scheme *sc, s7_pointer args) {return(g_set_enved_base(s7_cadr(args)));}
static s7_pointer acc_enved_filter_order(s7_scheme *sc, s7_pointer args) {return(g_set_enved_filter_order(s7_cadr(args)));}
static s7_pointer acc_enved_power(s7_scheme *sc, s7_pointer args) {return(g_set_enved_power(s7_cadr(args)));}
static s7_pointer acc_enved_style(s7_scheme *sc, s7_pointer args) {return(g_set_enved_style(s7_cadr(args)));}
static s7_pointer acc_enved_target(s7_scheme *sc, s7_pointer args) {return(g_set_enved_target(s7_cadr(args)));}
static s7_pointer acc_enved_with_wave(s7_scheme *sc, s7_pointer args) {return(g_set_enved_with_wave(s7_cadr(args)));}
#endif

void g_init_env(void)
{
  #define H_enved_amplitude "The value for " S_enved_target " that sets the envelope editor 'amp' button."
  #define H_enved_spectrum "The value for " S_enved_target " that sets the envelope editor 'flt' button."
  #define H_enved_srate "The value for " S_enved_target " that sets the envelope editor 'src' button."

#if HAVE_SCHEME
  s7_pointer i, b, l, t, r, s;
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  l = s7_make_symbol(s7, "list?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  t = s7_t(s7);
#endif

  Xen_define_constant(S_enved_amplitude, ENVED_AMPLITUDE, H_enved_amplitude);
  Xen_define_constant(S_enved_spectrum,  ENVED_SPECTRUM,  H_enved_spectrum);
  Xen_define_constant(S_enved_srate,     ENVED_SRATE,     H_enved_srate);

  Xen_define_constant(S_envelope_linear,      ENVELOPE_LINEAR,      S_enved_style " choice: linear connections between breakpoints");
  Xen_define_constant(S_envelope_exponential, ENVELOPE_EXPONENTIAL, S_enved_style " choice: exponential connections between breakpoints");

  Xen_define_typed_dilambda(S_enved_base,   g_enved_base_w,   H_enved_base,   
			    S_set S_enved_base, g_set_enved_base_w, 0, 0, 1, 0, s7_make_signature(s7, 1, r), s7_make_signature(s7, 2, r, r));
  Xen_define_typed_dilambda(S_enved_power,  g_enved_power_w,  H_enved_power,  
			    S_set S_enved_power,  g_set_enved_power_w,   0, 0, 1, 0, s7_make_signature(s7, 1, r), s7_make_signature(s7, 2, r, r));
  Xen_define_typed_dilambda(S_enved_clipping, g_enved_clipping_w, H_enved_clipping, 
			    S_set S_enved_clipping, g_set_enved_clipping_w, 0, 0, 1, 0, s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));
  Xen_define_typed_dilambda(S_enved_style,  g_enved_style_w,  H_enved_style, 
			    S_set S_enved_style, g_set_enved_style_w, 0, 0, 1, 0, s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));
  Xen_define_typed_dilambda(S_enved_target, g_enved_target_w, H_enved_target, 
			    S_set S_enved_target, g_set_enved_target_w,  0, 0, 1, 0, s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));
  Xen_define_typed_dilambda(S_enved_with_wave, g_enved_with_wave_w, H_enved_with_wave, 
			    S_set S_enved_with_wave, g_set_enved_with_wave_w,  0, 0, 1, 0, s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));
  Xen_define_typed_dilambda(S_enved_in_dB,  g_enved_in_dB_w,  H_enved_in_dB,  
			    S_set S_enved_in_dB, g_set_enved_in_dB_w, 0, 0, 1, 0, s7_make_signature(s7, 1, b), s7_make_signature(s7, 2, b, b));
  Xen_define_typed_dilambda(S_enved_filter_order, g_enved_filter_order_w, H_enved_filter_order,
			    S_set S_enved_filter_order, g_set_enved_filter_order_w,  0, 0, 1, 0, s7_make_signature(s7, 1, i), s7_make_signature(s7, 2, i, i));

  Xen_define_typed_procedure(S_enved_dialog,    g_enved_dialog_w,    0, 0, 0, H_enved_dialog,   s7_make_signature(s7, 1, t));
  Xen_define_typed_procedure(S_save_envelopes,  g_save_envelopes_w,  0, 1, 0, H_save_envelopes, s7_make_signature(s7, 2, s, s));

#if HAVE_SCHEME
  Xen_define_typed_procedure(S_define_envelope "-1", g_define_envelope_w, 2, 1, 0, H_define_envelope, s7_make_signature(s7, 4, t, s, l, r));
  Xen_eval_C_string("(define-macro (define-envelope a . b) `(define-envelope-1 ',a ,@b))");
#else
  Xen_define_procedure(S_define_envelope, g_define_envelope_w, 2, 1, 0, H_define_envelope);
#endif

  Xen_define_constant(S_enved_add_point,      ENVED_ADD_POINT,      S_enved_hook " 'reason' arg when point is added");
  Xen_define_constant(S_enved_delete_point,   ENVED_DELETE_POINT,   S_enved_hook " 'reason' arg when point is deleted");
  Xen_define_constant(S_enved_move_point,     ENVED_MOVE_POINT,     S_enved_hook " 'reason' arg when point is moved");

#if HAVE_SCHEME
  #define H_enved_hook S_enved_hook " (envelope point x y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
and decay portions in the envelope editor, or use functions such as \
stretch-envelope from env.scm: \n\
 (hook-push " S_enved_hook "\n\
   (lambda (hook) \n\
     ((lambda (env pt x y reason)\n\
        (if (= reason " S_enved_move_point ")\n\
            (let* ((old-x (list-ref env (* pt 2)))\n\
                   (new-env (stretch-envelope env old-x x)))\n\
              (list-set! new-env (+ (* pt 2) 1) y)\n\
              (set! (hook 'result) new-env))))) \n\
      (hook 'envelope) (hook 'point) (hook 'x) (hook 'y) (hook 'reason))))"
#endif
#if HAVE_RUBY
  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
and decay portions in the envelope editor."
#endif
#if HAVE_FORTH
  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
and decay portions in the envelope editor, or use functions such as \
stretch-envelope from env.fth: \n\
" S_enved_hook " lambda: <{ en pt x y reason }>\n\
  reason " S_enved_move_point " = if\n\
    en old-x  en pt 2* list@  x stretch-envelope  pt 2* 1+ y list!\n\
  else\n\
    #f\n\
  then\n\
; add-hook!"
#endif

  enved_hook = Xen_define_hook(S_enved_hook, "(make-hook 'envelope 'point 'x 'y 'reason)", 5, H_enved_hook);
  /* not 'env as hook arg name! that can confuse clm2xen's optimizer */

  ss->enved = new_env_editor();
  free(ss->enved->axis);
  ss->enved->axis = NULL;
  ss->enved->in_dB = DEFAULT_ENVED_IN_DB;
  ss->enved->clipping = DEFAULT_ENVED_CLIPPING;

#if HAVE_SCHEME
  s7_set_setter(s7, ss->enved_base_symbol, s7_make_function(s7, "[acc-" S_enved_base "]", acc_enved_base, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->enved_filter_order_symbol, s7_make_function(s7, "[acc-" S_enved_filter_order "]", acc_enved_filter_order, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->enved_power_symbol, s7_make_function(s7, "[acc-" S_enved_power "]", acc_enved_power, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->enved_style_symbol, s7_make_function(s7, "[acc-" S_enved_style "]", acc_enved_style, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->enved_target_symbol, s7_make_function(s7, "[acc-" S_enved_target "]", acc_enved_target, 2, 0, false, "accessor"));
  s7_set_setter(s7, ss->enved_with_wave_symbol, s7_make_function(s7, "[acc-" S_enved_with_wave "]", acc_enved_with_wave, 2, 0, false, "accessor"));

  s7_set_documentation(s7, ss->enved_base_symbol, "*enved-base*: envelope editor exponential base value (1.0)");
  s7_set_documentation(s7, ss->enved_filter_order_symbol, "*enved-filter-order*: envelope editor's FIR filter order (40)");
  s7_set_documentation(s7, ss->enved_power_symbol, "*enved-power*: envelope editor base scale range (9.0^power)");
  s7_set_documentation(s7, ss->enved_style_symbol, "*enved-style*: envelope editor breakpoint connection choice: envelope-linear or envelope-exponential");
  s7_set_documentation(s7, ss->enved_target_symbol, "*enved-target*: determines how the envelope edit envelope is applied; enved-amplitude etc");
  s7_set_documentation(s7, ss->enved_with_wave_symbol, "*enved-wave?*: #t if the envelope editor is displaying the waveform to be edited");
#endif
}
