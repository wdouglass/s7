#include "snd.h"

typedef struct {
  int size;
  char *name;
  rgb_t *r, *g, *b;
  Xen lambda;
  int gc_loc;
  mus_float_t **(*make_rgb)(int size, Xen func);
  void (*get_rgb)(double x, rgb_t *r, rgb_t *g, rgb_t *b);
} cmap;

static cmap **cmaps = NULL;
static int cmaps_size = 0;

#define no_such_colormap Xen_make_error_type("no-such-colormap")


bool is_colormap(int n)
{
  return((n >= 0) &&
	 (n < cmaps_size) && 
	 (cmaps[n]));
}


char *colormap_name(int n) 
{
  if (is_colormap(n))
    return(cmaps[n]->name); 
  return(NULL);
}


int num_colormaps(void)
{
  int i;
  for (i = cmaps_size - 1; i >= 0; i--)
    if (cmaps[i])
      return(i + 1);
  return(0);
}


static cmap *delete_cmap(int index)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (c->r) free(c->r);
      if (c->g) free(c->g);
      if (c->b) free(c->b);
      if (c->name) free(c->name);
      if (Xen_is_procedure(c->lambda))
	snd_unprotect_at(c->gc_loc);
      free(c);
      cmaps[index] = NULL;
    }
  return(NULL);
}


static rgb_t *mus_float_ts_to_rgb_t(int size, mus_float_t *data)
{
  int i;
  rgb_t *new_data;
  new_data = (rgb_t *)calloc(size, sizeof(rgb_t));
  for (i = 0; i < size; i++)
    new_data[i] = float_to_rgb(data[i]);
  return(new_data);
}


static void rebuild_colormap(cmap *c)
{
  mus_float_t **rgb;
  int i;
  /* release old colormap data */
  if (c->r) free(c->r);
  if (c->g) free(c->g);
  if (c->b) free(c->b);
  c->size = color_map_size(ss);
  /* make new data */
  rgb = (*(c->make_rgb))(c->size, c->lambda);
  c->r = mus_float_ts_to_rgb_t(c->size, rgb[0]);
  c->g = mus_float_ts_to_rgb_t(c->size, rgb[1]);
  c->b = mus_float_ts_to_rgb_t(c->size, rgb[2]);
  for (i = 0; i < 3; i++) free(rgb[i]);
  free(rgb);
}


void get_current_color(int index, int n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (c->get_rgb)
	(c->get_rgb)((double)n / (double)color_map_size(ss), r, g, b);
      else
	{
	  if (color_map_size(ss) != c->size)
	    rebuild_colormap(c);
	  if ((n >= 0) && (n < c->size))
	    {
	      (*r) = c->r[n];
	      (*g) = c->g[n];
	      (*b) = c->b[n];
	    }
	}
    }
}

#if HAVE_GL
rgb_t *color_map_reds(int index)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (c->get_rgb) return(NULL);
      if (color_map_size(ss) != c->size)
	rebuild_colormap(c);
      return(c->r);
    }
  return(NULL);
}


rgb_t *color_map_greens(int index)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (c->get_rgb) return(NULL);
      if (color_map_size(ss) != c->size)
	rebuild_colormap(c);
      return(c->g);
    }
  return(NULL);
}


rgb_t *color_map_blues(int index)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (c->get_rgb) return(NULL);
      if (color_map_size(ss) != c->size)
	rebuild_colormap(c);
      return(c->b);
    }
  return(NULL);
}
#endif


static cmap *new_cmap(const char *name, int size, mus_float_t **rgb)
{
  cmap *c;
  c = (cmap *)calloc(1, sizeof(cmap));
  c->name = mus_strdup(name);
  c->size = size;
  if (rgb)
    {
      c->r = mus_float_ts_to_rgb_t(size, rgb[0]);
      c->g = mus_float_ts_to_rgb_t(size, rgb[1]);
      c->b = mus_float_ts_to_rgb_t(size, rgb[2]);
    }
  c->lambda = Xen_false;
  c->gc_loc = NOT_A_GC_LOC;
  return(c);
}


static cmap *make_builtin_cmap(int size, const char *name, 
			       mus_float_t **(*make_rgb)(int size, Xen ignored),
			       void (*get_rgb)(double x, rgb_t *r, rgb_t *g, rgb_t *b))
{
  mus_float_t **rgb = NULL;
  cmap *c = NULL;
  if ((make_rgb) && (!get_rgb))
    rgb = make_rgb(size, Xen_false);
  c = new_cmap(name, size, rgb);
  c->get_rgb = get_rgb;
  c->make_rgb = make_rgb;
  if (rgb)
    {
      int i;
      for (i = 0; i < 3; i++) free(rgb[i]);
      free(rgb);
    }
  return(c);
}


static mus_float_t **make_base_rgb(int size)
{
  mus_float_t **rgb;
  int i;
  rgb = (mus_float_t **)calloc(3, sizeof(mus_float_t *));
  for (i = 0; i < 3; i++) rgb[i] = (mus_float_t *)calloc(size, sizeof(mus_float_t));
  return(rgb);
}


static char *add_colormap_func_error_msg = NULL;
static bool add_colormap_func_hit_error = false;

static void add_colormap_func_error(const char *msg, void *data)
{
  add_colormap_func_hit_error = true;
  add_colormap_func_error_msg = mus_strdup(msg); /* msg itself is freed by the error handler in snd-xen.c */
}


static mus_float_t **make_xen_colormap(int size, Xen lambda)
{
  Xen xrgb;
  mus_float_t **rgb = NULL;
  add_colormap_func_hit_error = false;
  redirect_xen_error_to(add_colormap_func_error, NULL);
  xrgb = Xen_call_with_1_arg(lambda,
		    C_int_to_Xen_integer(size),
		    S_add_colormap);
  redirect_xen_error_to(NULL, NULL);
  if (add_colormap_func_hit_error)
    {
      Xen str;
      if (add_colormap_func_error_msg)
	{
	  str = C_string_to_Xen_string(add_colormap_func_error_msg);
	  free(add_colormap_func_error_msg);
	  add_colormap_func_error_msg = NULL;
	}
      else str = Xen_false;

      Xen_error(Xen_make_error_type("colormap-error"),
		Xen_list_2(C_string_to_Xen_string(S_add_colormap ": function error ~A"),
			   str));
    }

  if (!(Xen_is_list(xrgb)))
    Xen_error(Xen_make_error_type("colormap-error"),
	      Xen_list_3(C_string_to_Xen_string(S_add_colormap ": colormap function, ~A, returned ~A, but should return a list of 3 " S_vct "s"),
			 lambda,
			 xrgb));
  else
    {
      vct *xr, *xg, *xb;
      mus_float_t *xrdata, *xgdata, *xbdata;
      int i, gc_loc;

      /* user-defined colormap func returns a list of 3 vcts (r g b) */
      gc_loc = snd_protect(xrgb);

      if (!(mus_is_vct(Xen_list_ref(xrgb, 0)))) 
	Xen_error(Xen_make_error_type("colormap-error"),
		  Xen_list_2(C_string_to_Xen_string(S_add_colormap ": function did not return a list of " S_vct "s! ~A"),
			     xrgb));

      xr = Xen_to_vct(Xen_list_ref(xrgb, 0));
      xrdata = mus_vct_data(xr);
      if (mus_vct_length(xr) < size)
	Xen_error(Xen_make_error_type("colormap-error"),
		  Xen_list_2(C_string_to_Xen_string(S_add_colormap ": function did not return a list of " S_vct "s of the correct size: ~A"),
			     xrgb));

      xg = Xen_to_vct(Xen_list_ref(xrgb, 1));
      xgdata = mus_vct_data(xg);
      xb = Xen_to_vct(Xen_list_ref(xrgb, 2));
      xbdata = mus_vct_data(xb);
      rgb = make_base_rgb(size);
      for (i = 0; i < size; i++)
	{
	  rgb[0][i] = xrdata[i];
	  rgb[1][i] = xgdata[i];
	  rgb[2][i] = xbdata[i];
	}

      snd_unprotect_at(gc_loc);
    }
  return(rgb);
}


static int add_colormap(const char *name, Xen func)
{
  cmap *c;
  mus_float_t **rgb;
  int index = -1, i, loc;
  for (i = 0; i < cmaps_size; i++)
    if (!(cmaps[i]))
      {
	index = i;
	break;
      }
  if (index == -1) /* no free slot */
    {
      index = cmaps_size;
      cmaps_size += 8;
      cmaps = (cmap **)realloc(cmaps, cmaps_size * sizeof(cmap *));
      for (i = index; i < cmaps_size; i++) cmaps[i] = NULL;
    }
  loc = snd_protect(func);
  rgb = make_xen_colormap(color_map_size(ss), func);
  c = new_cmap(name, color_map_size(ss), rgb);
  c->make_rgb = make_xen_colormap;
  c->lambda = func;
  c->gc_loc = loc;
  for (i = 0; i < 3; i++) free(rgb[i]);
  free(rgb);
  cmaps[index] = c;
  return(index);
}


static mus_float_t **make_black_and_white_colormap(int size, Xen ignored)
{
  /* (r 0) (g 0) (b 0) */
  return(make_base_rgb(size));
}


#if USE_GTK
static void black_and_white_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = 0.0;
  (*g) = 0.0;
  (*b) = 0.0;
}
#else
  #define black_and_white_rgb NULL
#endif


/* colormap functions taken mostly from (GPL) octave-forge code written by Kai Habel <kai.habel@gmx.de> */

static mus_float_t **make_gray_colormap(int size, Xen ignored)
{
  /* (r x) (g x) (b x) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = x;
      rgb[1][i] = x;
      rgb[2][i] = x;
    }
  return(rgb);
}


#if USE_GTK
static void gray_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = n;
  (*g) = n;
  (*b) = n;
}
#else
  #define gray_rgb NULL
#endif


static mus_float_t **make_autumn_colormap(int size, Xen ignored)
{
  /* (r 1.0) (g x) (b 0.0) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = 1.0;
      rgb[1][i] = x;
      rgb[2][i] = 0.0;
    }
  return(rgb);
}


#if USE_GTK
static void autumn_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = 1.0;
  (*g) = n;
  (*b) = 0.0;
}
#else
  #define autumn_rgb NULL
#endif


static mus_float_t **make_spring_colormap(int size, Xen ignored)
{
  /* (r 1.0) (g x) (b (- 1.0 x)) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = 1.0;
      rgb[1][i] = x;
      rgb[2][i] = 1.0 - x;
    }
  return(rgb);
}


#if USE_GTK
static void spring_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = 1.0;
  (*g) = n;
  (*b) = 1.0 - n;
}
#else
  #define spring_rgb NULL
#endif


static mus_float_t **make_winter_colormap(int size, Xen ignored)
{
  /* (r 0.0) (g x) (b (- 1.0 (/ x 2))) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = 0.0;
      rgb[1][i] = x;
      rgb[2][i] = 1.0 - (x * 0.5);
    }
  return(rgb);
}


#if USE_GTK
static void winter_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = 0.0;
  (*g) = n;
  (*b) = 1.0 - (n * 0.5);
}
#else
  #define winter_rgb NULL
#endif


static mus_float_t **make_summer_colormap(int size, Xen ignored)
{
  /* (r x) (g (+ 0.5 (/ x 2))) (b 0.4) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = x;
      rgb[1][i] = 0.5 + (0.5 * x);
      rgb[2][i] = 0.4;
    }
  return(rgb);
}


#if USE_GTK
static void summer_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = n;
  (*g) = 0.5 + (0.5 * n);
  (*b) = 0.4;
}
#else
  #define summer_rgb NULL
#endif


static mus_float_t **make_cool_colormap(int size, Xen ignored)
{
  /* (r x) (g (- 1.0 x)) (b 1.0) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = x;
      rgb[1][i] = 1.0 - x;
      rgb[2][i] = 1.0;
    }
  return(rgb);
}


#if USE_GTK
static void cool_rgb(double n, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = n;
  (*g) = 1.0 - n;
  (*b) = 1.0;
}
#else
  #define cool_rgb NULL
#endif


static mus_float_t **make_copper_colormap(int size, Xen ignored)
{
  /* (r (if (< x 4/5) (* 5/4 x) 1.0)) (g (* 4/5 x)) (b (* 1/2 x)) */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < 0.8) ? (1.25 * x) : 1.0;
      rgb[1][i] = 0.8 * x;
      rgb[2][i] = 0.5 * x;
    }
  return(rgb);
}


#if USE_GTK
static void copper_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = (x < 0.8) ? (1.25 * x) : 1.0;
  (*g) = 0.8 * x;
  (*b) = 0.5 * x;
}
#else
  #define copper_rgb NULL
#endif


static mus_float_t **make_flag_colormap(int size, Xen ignored)
{
  /* (r (if (or (= k 0) (= k 1)) 1.0 0.0)) (g (if (= k 1) 1.0 0.0)) (b (if (or (= k 1) (= k 2)) 1.0 0.0)) */
  mus_float_t **rgb;
  int i, k = 0;
  rgb = make_base_rgb(size);
  for (i = 0; i < size; i++)
    {
      rgb[0][i] = (k < 2) ? 1.0 : 0.0;
      rgb[1][i] = (k == 1) ? 1.0 : 0.0;
      rgb[2][i] = ((k == 1) || (k == 2)) ? 1.0 : 0.0;
      k++;
      if (k == 4) k = 0;
    }
  return(rgb);
}


#if USE_GTK
static void flag_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  int k;
  k = ((int)(x * color_map_size(ss))) % 4;
  (*r) = (k < 2) ? 1.0 : 0.0;
  (*g) = (k == 1) ? 1.0 : 0.0;
  (*b) = ((k == 1) || (k == 2)) ? 1.0 : 0.0;
}
#else
  #define flag_rgb NULL
#endif


static mus_float_t **make_prism_colormap(int size, Xen ignored)
{
  /* (r (list-ref (list 1 1 1 0 0 2/3) k)) (g (list-ref (list 0 1/2 1 1 0 0) k)) (b (list-ref (list 0 0 0 0 1 1) k)) */
  mus_float_t **rgb;
  int i, k = 0;
  mus_float_t rs[6] = {1.0, 1.0, 1.0, 0.0, 0.0, 0.6667};
  mus_float_t gs[6] = {0.0, 0.5, 1.0, 1.0, 0.0, 0.0};
  mus_float_t bs[6] = {0.0, 0.0, 0.0, 0.0, 1.0, 1.0};
  rgb = make_base_rgb(size);
  for (i = 0; i < size; i++)
    {
      rgb[0][i] = rs[k];
      rgb[1][i] = gs[k];
      rgb[2][i] = bs[k];
      k++;
      if (k == 6) k = 0;
    }
  return(rgb);
}


#if USE_GTK
static void prism_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  int k;
  mus_float_t rs[6] = {1.0, 1.0, 1.0, 0.0, 0.0, 0.6667};
  mus_float_t gs[6] = {0.0, 0.5, 1.0, 1.0, 0.0, 0.0};
  mus_float_t bs[6] = {0.0, 0.0, 0.0, 0.0, 1.0, 1.0};

  k = ((int)(x * color_map_size(ss))) % 6;
  (*r) = rs[k];
  (*g) = gs[k];
  (*b) = bs[k];
}
#else
  #define prism_rgb NULL
#endif


static mus_float_t **make_bone_colormap(int size, Xen ignored)
{
  /* (r (if (< x 3/4) (* 7/8 x) (- (* 11/8 x) 3/8)))
     (g (if (< x 3/8) (* 7/8 x) (if (< x 3/4) (- (* 29/24 x) 1/8) (+ (* 7/8 x) 1/8))))
     (b (if (< x 3/8) (* 29/24 x) (+ (* 7/8 x) 1/8)))
  */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .75) ? (x * .875) : ((x * 11.0 / 8.0) - .375);
      rgb[1][i] = (x < .375) ? (x * .875) : ((x < .75) ? ((x * 29.0 / 24.0) - .125) : ((x * .875) + .125));
      rgb[2][i] = (x < .375) ? (x * 29.0 / 24.0) : ((x * .875) + .125);
    }
  return(rgb);
}


#if USE_GTK
static void bone_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = (x < .75) ? (x * .875) : ((x * 11.0 / 8.0) - .375);
  (*g) = (x < .375) ? (x * .875) : ((x < .75) ? ((x * 29.0 / 24.0) - .125) : ((x * .875) + .125));
  (*b) = (x < .375) ? (x * 29.0 / 24.0) : ((x * .875) + .125);
}
#else
  #define bone_rgb NULL
#endif


static mus_float_t **make_hot_colormap(int size, Xen ignored)
{
  /* (mimicking matlab here, not octave)
     (r (if (< x 3/8) (* 8/3 x) 1.0))
     (g (if (< x 3/8) 0.0 (if (< x 3/4) (- (* 8/3 x) 1.0) 1.0)))
     (b (if (< x 3/4) 0.0 (- (* 4 x) 3)))
  */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .375) ? (x * 8.0 / 3.0) : 1.0;
      rgb[1][i] = (x < .375) ? 0.0 : ((x < .75) ? ((x * 8.0 / 3.0) - 1.0) : 1.0);
      rgb[2][i] = (x < .75) ? 0.0 : ((x * 4.0) - 3.0);
    }
  return(rgb);
}


#if USE_GTK
static void hot_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = (x < .375) ? (x * 8.0 / 3.0) : 1.0;
  (*g) = (x < .375) ? 0.0 : ((x < .75) ? ((x * 8.0 / 3.0) - 1.0) : 1.0);
  (*b) = (x < .75) ? 0.0 : ((x * 4.0) - 3.0);
}
#else
  #define hot_rgb NULL
#endif


static mus_float_t **make_jet_colormap(int size, Xen ignored)
{
  /* 
     (r (if (< x 3/8) 0.0 (if (< x 5/8) (- (* 4 x) 3/2) (if (< x 7/8) 1.0 (+ (* -4 x) 9/2)))))
     (g (if (< x 1/8) 0.0 (if (< x 3/8)	(- (* 4 x) 1/2)	(if (< x 5/8) 1.0 (if (< x 7/8)	(+ (* -4 x) 7/2) 0.0)))))
     (b (if (< x 1/8) (+ (* 4 x) 1/2) (if (< x 3/8) 1.0	(if (< x 5/8) (+ (* -4 x) 5/2) 0.0))))
  */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .375) ? 0.0 : ((x < .625) ? ((x * 4.0) - 1.5) : ((x < .875) ? 1.0 : ((x * -4.0) + 4.5)));
      rgb[1][i] = (x < .125) ? 0.0 : ((x < .375) ? ((x * 4.0) - 0.5) : ((x < .625) ? 1.0 : ((x < .875) ? ((x * -4.0) + 3.5) : 0.0)));
      rgb[2][i] = (x < .125) ? ((x * 4.0) + 0.5) : ((x < .375) ? 1.0 : ((x < .625) ? ((x * -4.0) + 2.5) : 0.0));
    }
  return(rgb);
}


#if USE_GTK
static void jet_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = (x < .375) ? 0.0 : ((x < .625) ? ((x * 4.0) - 1.5) : ((x < .875) ? 1.0 : ((x * -4.0) + 4.5)));
  (*g) = (x < .125) ? 0.0 : ((x < .375) ? ((x * 4.0) - 0.5) : ((x < .625) ? 1.0 : ((x < .875) ? ((x * -4.0) + 3.5) : 0.0)));
  (*b) = (x < .125) ? ((x * 4.0) + 0.5) : ((x < .375) ? 1.0 : ((x < .625) ? ((x * -4.0) + 2.5) : 0.0));
}
#else
  #define jet_rgb NULL
#endif


static mus_float_t **make_pink_colormap(int size, Xen ignored)
{
  /* matlab uses log here, or something like that -- this version is quite different
     (r (if (< x 3/8) (* 14/9 x) (+ (* 2/3 x) 1/3)))
     (g (if (< x 3/8) (* 2/3 x) (if (< x 3/4) (- (* 14/9 x) 1/3) (+ (* 2/3 x) 1/3))))
     (b (if (< x 3/4) (* 2/3 x) (- (* 2 x) 1)))			
  */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .375) ? (x * 14.0 / 9.0) : ((x * 2.0 / 3.0) + 1.0 / 3.0);
      rgb[1][i] = (x < .375) ? (x * 2.0 / 3.0) : ((x < .75) ? ((x * 14.0 / 9.0) - 1.0 / 3.0) : ((x * 2.0 / 3.0) + 1.0 / 3.0));
      rgb[2][i] = (x < .75) ? (x * 2.0 / 3.0) : ((x * 2.0) - 1.0);
    }
  return(rgb);
}


#if USE_GTK
static void pink_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = (x < .375) ? (x * 14.0 / 9.0) : ((x * 2.0 / 3.0) + 1.0 / 3.0);
  (*g) = (x < .375) ? (x * 2.0 / 3.0) : ((x < .75) ? ((x * 14.0 / 9.0) - 1.0 / 3.0) : ((x * 2.0 / 3.0) + 1.0 / 3.0));
  (*b) = (x < .75) ? (x * 2.0 / 3.0) : ((x * 2.0) - 1.0);
}
#else
  #define pink_rgb NULL
#endif


static mus_float_t **make_rainbow_colormap(int size, Xen ignored)
{
  /* 
     (r (if (< x 2/5) 1.0 (if (< x 3/5) (+ (* -5 x) 3) (if (< x 4/5) 0.0 (- (* 10/3 x) 8/3)))))
     (g (if (< x 2/5) (* 5/2 x) (if (< x 3/5) 1.0 (if (< x 4/5) (+ (* -5 x) 4) 0.0))))
     (b (if (< x 3/5) 0.0 (if (< x 4/5)	(- (* 5 x) 3) 1.0)))
  */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .4) ? 1.0 : ((x < .6) ? ((x * -5.0) + 3.0) : ((x < .8) ? 0.0 : ((x * 10.0 / 3.0) - 8.0 / 3.0)));
      rgb[1][i] = (x < .4) ? (x * 2.5) : ((x < .6) ? 1.0 : ((x < .8) ? ((x * -5.0) + 4.0) : 0.0));
      rgb[2][i] = (x < .6) ? 0.0 : ((x < .8) ? ((x * 5.0) - 3.0) : 1.0);
    }
  return(rgb);
}


#if USE_GTK
static void rainbow_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  (*r) = (x < .4) ? 1.0 : ((x < .6) ? ((x * -5.0) + 3.0) : ((x < .8) ? 0.0 : ((x * 10.0 / 3.0) - 8.0 / 3.0)));
  (*g) = (x < .4) ? (x * 2.5) : ((x < .6) ? 1.0 : ((x < .8) ? ((x * -5.0) + 4.0) : 0.0));
  (*b) = (x < .6) ? 0.0 : ((x < .8) ? ((x * 5.0) - 3.0) : 1.0);
}
#else
  #define rainbow_rgb NULL
#endif


static mus_float_t **make_phases_colormap(int size, Xen ignored)
{
  /* 0 and pi: blue->green, pi/2 and 3pi/2: red->black */
  mus_float_t **rgb;
  int i;
  mus_float_t x, incr;
  rgb = make_base_rgb(size);
  incr = (2.0 * M_PI) / (mus_float_t)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      if (x <= 0.5 * M_PI)
	{
	  rgb[0][i] = x / (0.5 * M_PI);
	  rgb[1][i] = 0.0;
	  rgb[2][i] = 1.0 - rgb[0][i];
	}
      else
	{
	  if (x <= M_PI)
	    {
	      rgb[0][i] = 1.0 - ((x - 0.5 * M_PI) / (0.5 * M_PI));
	      rgb[1][i] = 1.0 - rgb[0][i];
	      rgb[2][i] = 0.0;
	    }
	  else
	    {
	      if (x <= 1.5 * M_PI)
		{
		  rgb[0][i] = 0.0;
		  rgb[1][i] = 1.0 - ((x - M_PI) / (0.5 * M_PI));
		  rgb[2][i] = 0.0;
		}
	      else 
		{
		  rgb[0][i] = 0.0;
		  rgb[1][i] = 1.0 - ((x - (1.5 * M_PI)) / (0.5 * M_PI));
		  rgb[2][i] = 0.0;
		}
	    }
	}
    }
  return(rgb);
}


#if USE_GTK
void phases_rgb(double x, rgb_t *r, rgb_t *g, rgb_t *b)
{
  x *= (2.0 * M_PI); /* match code above */

  if (x <= 0.5 * M_PI)
    {
      (*r) = x / (0.5 * M_PI);
      (*g) = 0.0;
      (*b) = 1.0 - (*r);
    }
  else
    {
      if (x <= M_PI)
	{
	  (*r) = 1.0 - ((x - 0.5 * M_PI) / (0.5 * M_PI));
	  (*g) = 1.0 - (*r);
	  (*b) = 0.0;
	}
      else
	{
	  if (x <= 1.5 * M_PI)
	    {
	      (*r) = 0.0;
	      (*g) = 1.0 - ((x - M_PI) / (0.5 * M_PI));
	      (*b) = 0.0;
	    }
	  else 
	    {
	      (*r) = 0.0;
	      (*g) = 1.0 - ((x - (1.5 * M_PI)) / (0.5 * M_PI));
	      (*b) = 0.0;
	    }
	}
    }
}
#else
  #define phases_rgb NULL
#endif


/* ---------------------------------------- colormap objects ---------------------------------------- */

typedef struct {
  int n;
} xen_colormap;


#define Xen_to_xen_colormap(arg) ((xen_colormap *)Xen_object_ref(arg))

static int xen_colormap_to_int(Xen n)
{
  xen_colormap *col;
  col = Xen_to_xen_colormap(n);
  return(col->n);
}

#define Xen_colormap_to_C_int(n) xen_colormap_to_int(n)


static Xen_object_type_t xen_colormap_tag;

static bool xen_is_colormap(Xen obj) 
{
  return(Xen_c_object_is_type(obj, xen_colormap_tag));
}


static void xen_colormap_free(xen_colormap *v) {if (v) free(v);}

Xen_wrap_free(xen_colormap, free_xen_colormap, xen_colormap_free)


static char *xen_colormap_to_string(xen_colormap *v)
{
  #define COLORMAP_PRINT_BUFFER_SIZE 64
  char *buf;
  if (!v) return(NULL);
  buf = (char *)calloc(COLORMAP_PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, COLORMAP_PRINT_BUFFER_SIZE, "#<colormap %s>", colormap_name(v->n));
  return(buf);
}

Xen_wrap_print(xen_colormap, print_xen_colormap, xen_colormap_to_string)


#if HAVE_FORTH || HAVE_RUBY
static Xen g_xen_colormap_to_string(Xen obj)
{
  char *vstr;
  Xen result;
  #define S_xen_colormap_to_string "colormap->string"

  Xen_check_type(xen_is_colormap(obj), obj, 1, S_xen_colormap_to_string, "a colormap");

  vstr = xen_colormap_to_string(Xen_to_xen_colormap(obj));
  result = C_string_to_Xen_string(vstr);
  free(vstr);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static bool xen_colormap_equalp(xen_colormap *v1, xen_colormap *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static Xen equalp_xen_colormap(Xen obj1, Xen obj2)
{
  if ((!(xen_is_colormap(obj1))) || (!(xen_is_colormap(obj2)))) return(Xen_false);
  return(C_bool_to_Xen_boolean(xen_colormap_equalp(Xen_to_xen_colormap(obj1), Xen_to_xen_colormap(obj2))));
}
#endif


static xen_colormap *xen_colormap_make(int n)
{
  xen_colormap *new_v;
  new_v = (xen_colormap *)malloc(sizeof(xen_colormap));
  new_v->n = n;
  return(new_v);
}


static Xen new_xen_colormap(int n)
{
  xen_colormap *mx;
  if (n < 0)
    return(Xen_false);

  mx = xen_colormap_make(n);
  return(Xen_make_object(xen_colormap_tag, mx, 0, free_xen_colormap));
}

#define C_int_to_Xen_colormap(Val) new_xen_colormap(Val)


#if HAVE_SCHEME
static bool s7_xen_colormap_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_colormap *)obj1)->n == ((xen_colormap *)obj2)->n));
}


static Xen s7_xen_colormap_length(s7_scheme *sc, Xen args)
{
  return(C_int_to_Xen_integer(color_map_size(ss)));
}


static Xen g_colormap_ref(Xen map, Xen pos);
static Xen s7_colormap_apply(s7_scheme *sc, Xen args1)
{
  s7_pointer obj, args;
  obj = s7_car(args1);
  args = s7_cdr(args1);
  if (!s7_is_pair(args))
    s7_wrong_number_of_args_error(sc, "colormap ref", args);
  return(g_colormap_ref(obj, Xen_car(args)));
}
#endif


static void init_xen_colormap(void)
{
#if HAVE_SCHEME
  xen_colormap_tag = s7_make_c_type(s7, "<colormap>");
  s7_c_type_set_print(s7, xen_colormap_tag, print_xen_colormap);
  s7_c_type_set_free(s7, xen_colormap_tag, free_xen_colormap);
  s7_c_type_set_equal(s7, xen_colormap_tag, s7_xen_colormap_equalp);
  s7_c_type_set_length(s7, xen_colormap_tag, s7_xen_colormap_length);
  s7_c_type_set_ref(s7, xen_colormap_tag, s7_colormap_apply);
#else
#if HAVE_RUBY
  xen_colormap_tag = Xen_make_object_type("XenColormap", sizeof(xen_colormap));
#else
  xen_colormap_tag = Xen_make_object_type("Colormap", sizeof(xen_colormap));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_colormap_tag,   print_xen_colormap);
  fth_set_object_dump(xen_colormap_tag,      g_xen_colormap_to_string);
  fth_set_object_equal(xen_colormap_tag,     equalp_xen_colormap);
  fth_set_object_free(xen_colormap_tag,      free_xen_colormap);
#endif

#if HAVE_RUBY
  rb_define_method(xen_colormap_tag, "to_s",     Xen_procedure_cast print_xen_colormap, 0);
  rb_define_method(xen_colormap_tag, "eql?",     Xen_procedure_cast equalp_xen_colormap, 1);
  rb_define_method(xen_colormap_tag, "==",       Xen_procedure_cast equalp_xen_colormap, 1);
  rb_define_method(xen_colormap_tag, "to_str",   Xen_procedure_cast g_xen_colormap_to_string, 0);
#endif
}


/* -------------------------------------------------------------------------------- */

static Xen g_integer_to_colormap(Xen n)
{
  #define H_integer_to_colormap "(" S_integer_to_colormap " n) returns a colormap object corresponding to the given integer"
  int id;
  Xen_check_type(Xen_is_integer(n), n, 1, S_integer_to_colormap, "an integer");
  id = Xen_integer_to_C_int(n);
  if (is_colormap(id))
    return(new_xen_colormap(id));
  return(Xen_false);
}


static Xen g_colormap_to_integer(Xen n)
{
  #define H_colormap_to_integer "(" S_colormap_to_integer " id) returns the integer corresponding to the given colormap"
  Xen_check_type(xen_is_colormap(n), n, 1, S_colormap_to_integer, "a colormap");
  return(C_int_to_Xen_integer(xen_colormap_to_int(n)));
}



static Xen g_colormap_ref(Xen map, Xen pos)
{
  int index;
  mus_float_t x;
  rgb_t r = 0, g = 0, b = 0;

  #define H_colormap_ref "(" S_colormap_ref " colormap pos): (list r g b). Pos is between 0.0 and 1.0."

  Xen_check_type(xen_is_colormap(map), map, 1, S_colormap_ref, "a colormap object");
  Xen_check_type(Xen_is_number(pos), pos, 2, S_colormap_ref, "a number between 0.0 and 1.0");

  index = Xen_colormap_to_C_int(map);
  if (!(is_colormap(index)))
    Xen_error(no_such_colormap,
	      Xen_list_2(C_string_to_Xen_string(S_colormap_ref ": no such colormap: ~A"),
			 map));

  x = Xen_real_to_C_double(pos);
  if ((isnan(x)) || (x < 0.0) || (x > 1.0))
    Xen_out_of_range_error(S_colormap_ref, 2, pos, "x must be between 0.0 and 1.0");

  get_current_color(index, (int)(color_map_size(ss) * x + 0.5), &r, &g, &b);
  return(Xen_list_3(C_double_to_Xen_real(rgb_to_float(r)),
		    C_double_to_Xen_real(rgb_to_float(g)),
		    C_double_to_Xen_real(rgb_to_float(b))));
}

/* can't use Colormap -- it's the X type name */


static Xen g_colormap(void) 
{
  #define H_colormap "(" S_colormap "): current colormap choice."
  return(C_int_to_Xen_colormap(color_map(ss)));
}

static Xen g_set_colormap(Xen val) 
{
  int index;

  Xen_check_type(xen_is_colormap(val), val, 1, S_set S_colormap, "a colormap"); 

  index = Xen_colormap_to_C_int(val);
  if (!(is_colormap(index)))
    Xen_error(no_such_colormap,
	      Xen_list_2(C_string_to_Xen_string(S_colormap ": no such colormap: ~A"),
			 val));

  set_color_map(index); /* this normally redisplays */
  return(val);
}


static Xen g_colormap_size(void) {return(C_int_to_Xen_integer(color_map_size(ss)));}

static Xen g_set_colormap_size(Xen val) 
{
  int size;
  #define H_colormap_size "(" S_colormap_size "): current colormap size; default is 512."

  Xen_check_type(Xen_is_integer(val), val, 1, S_set S_colormap_size, "an integer"); 

  size = Xen_integer_to_C_int(val);
  if (size < 0)
    Xen_out_of_range_error(S_set S_colormap_size, 1, val, "size < 0?");
  if (size > (1 << 26))
    Xen_out_of_range_error(S_set S_colormap_size, 1, val, "size too large");

  set_color_map_size(size);
  check_colormap_sizes(color_map_size(ss));

  return(C_int_to_Xen_integer(color_map_size(ss)));
}


static Xen g_colormap_name(Xen col)
{
  int map;
  #define H_colormap_name "(" S_colormap_name " colormap) returns the colormap's name (used in the Color/Orientation dialog)."

  Xen_check_type(xen_is_colormap(col), col, 1, S_colormap_name, "a colormap"); 

  map = Xen_colormap_to_C_int(col);
  if (!(is_colormap(map)))
    Xen_error(no_such_colormap,
	      Xen_list_2(C_string_to_Xen_string(S_colormap_name ": no such colormap: ~A"),
			 col));

  return(C_string_to_Xen_string(cmaps[map]->name));
}


static Xen g_is_colormap(Xen obj)
{
  #define H_is_colormap "(" S_is_colormap " obj) -> " PROC_TRUE " if 'obj' is a colormap."
  return(C_bool_to_Xen_boolean(xen_is_colormap(obj) && 
			       is_colormap(Xen_colormap_to_C_int(obj))));
}


static Xen g_delete_colormap(Xen col)
{
  int map;
  #define H_delete_colormap "(" S_delete_colormap " colormap) frees the specified colormap."

  Xen_check_type(xen_is_colormap(col), col, 1, S_delete_colormap, "a colormap"); 

  map = Xen_colormap_to_C_int(col);
  if (!(is_colormap(map)))
    Xen_error(no_such_colormap,
	      Xen_list_2(C_string_to_Xen_string(S_delete_colormap ": no such colormap: ~A"),
			 col));

  delete_cmap(map);
  reflect_color_list(false);
  if (color_map(ss) == map) set_color_map(DEFAULT_COLOR_MAP);

  return(col);
}


#include "clm2xen.h"

static Xen g_add_colormap(Xen name, Xen func)
{
  int index;
  #define H_add_colormap "(" S_add_colormap " name func) adds the colormap created by func to the colormap table, \
returning the new colormap. 'name' is the colormap's name in the View:Color/Orientation dialog."

  Xen_check_type(Xen_is_string(name), name, 1, S_add_colormap, "a string"); 
  Xen_check_type(Xen_is_procedure(func) && (!mus_is_xen(func)), func, 2, S_add_colormap, "a function of 2 args");

  if (!(procedure_arity_ok(func, 1)))
    return(snd_bad_arity_error(S_add_colormap, 
			       C_string_to_Xen_string("func should take 1 arg"), 
			       func));

  index = add_colormap(Xen_string_to_C_string(name), func);
  reflect_color_list(false);

  return(C_int_to_Xen_colormap(index));
}


Xen_wrap_2_args(g_colormap_ref_w, g_colormap_ref)
Xen_wrap_no_args(g_colormap_w, g_colormap)
Xen_wrap_1_arg(g_is_colormap_w, g_is_colormap)
Xen_wrap_1_arg(g_set_colormap_w, g_set_colormap)
Xen_wrap_no_args(g_colormap_size_w, g_colormap_size)
Xen_wrap_1_arg(g_set_colormap_size_w, g_set_colormap_size)
Xen_wrap_1_arg(g_colormap_name_w, g_colormap_name)
Xen_wrap_1_arg(g_delete_colormap_w, g_delete_colormap)
Xen_wrap_2_args(g_add_colormap_w, g_add_colormap)
Xen_wrap_1_arg(g_integer_to_colormap_w, g_integer_to_colormap)
Xen_wrap_1_arg(g_colormap_to_integer_w, g_colormap_to_integer)

#if HAVE_SCHEME
static s7_pointer acc_colormap(s7_scheme *sc, s7_pointer args) {return(g_set_colormap(s7_cadr(args)));}
static s7_pointer acc_colormap_size(s7_scheme *sc, s7_pointer args) {return(g_set_colormap_size(s7_cadr(args)));}
#endif

#if (!HAVE_SCHEME)
static Xen colormap_temp[16]; /* static for Ruby's sake */
#endif

void g_init_gxcolormaps(void)
{
#if HAVE_SCHEME
  s7_pointer i, p, t, b, r, s, col, fnc;
  b = s7_make_symbol(s7, "boolean?");
  i = s7_make_symbol(s7, "integer?");
  p = s7_make_symbol(s7, "pair?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  col = s7_make_symbol(s7, "colormap?");
  fnc = s7_make_symbol(s7, "procedure?");
  t = s7_t(s7);
#endif

  cmaps_size = NUM_BUILTIN_COLORMAPS;
  cmaps = (cmap **)calloc(cmaps_size, sizeof(cmap *));
  
  init_xen_colormap();

  cmaps[BLACK_AND_WHITE_COLORMAP] = make_builtin_cmap(1, "black-and-white", make_black_and_white_colormap, black_and_white_rgb); 
  cmaps[GRAY_COLORMAP] =    make_builtin_cmap(1, "gray",    make_gray_colormap,    gray_rgb); 
  cmaps[AUTUMN_COLORMAP] =  make_builtin_cmap(1, "autumn",  make_autumn_colormap,  autumn_rgb); 
  cmaps[SPRING_COLORMAP] =  make_builtin_cmap(1, "spring",  make_spring_colormap,  spring_rgb); 
  cmaps[WINTER_COLORMAP] =  make_builtin_cmap(1, "winter",  make_winter_colormap,  winter_rgb); 
  cmaps[SUMMER_COLORMAP] =  make_builtin_cmap(1, "summer",  make_summer_colormap,  summer_rgb); 
  cmaps[COOL_COLORMAP] =    make_builtin_cmap(1, "cool",    make_cool_colormap,    cool_rgb); 
  cmaps[COPPER_COLORMAP] =  make_builtin_cmap(1, "copper",  make_copper_colormap,  copper_rgb); 
  cmaps[FLAG_COLORMAP] =    make_builtin_cmap(1, "flag",    make_flag_colormap,    flag_rgb); 
  cmaps[PRISM_COLORMAP] =   make_builtin_cmap(1, "prism",   make_prism_colormap,   prism_rgb); 
  cmaps[BONE_COLORMAP] =    make_builtin_cmap(1, "bone",    make_bone_colormap,    bone_rgb); 
  cmaps[HOT_COLORMAP] =     make_builtin_cmap(1, "hot",     make_hot_colormap,     hot_rgb); 
  cmaps[JET_COLORMAP] =     make_builtin_cmap(1, "jet",     make_jet_colormap,     jet_rgb); 
  cmaps[PINK_COLORMAP] =    make_builtin_cmap(1, "pink",    make_pink_colormap,    pink_rgb); 
  cmaps[RAINBOW_COLORMAP] = make_builtin_cmap(1, "rainbow", make_rainbow_colormap, rainbow_rgb); 
  cmaps[PHASES_COLORMAP] =  make_builtin_cmap(1, "phases",  make_phases_colormap,  phases_rgb); 

#if HAVE_SCHEME
  s7_define_constant(s7, "black-and-white-colormap", C_int_to_Xen_colormap(0));
  s7_define_constant(s7, "gray-colormap",            C_int_to_Xen_colormap(1));
  s7_define_constant(s7, "hot-colormap",             C_int_to_Xen_colormap(2));
  s7_define_constant(s7, "cool-colormap",            C_int_to_Xen_colormap(3));
  s7_define_constant(s7, "bone-colormap",            C_int_to_Xen_colormap(4));
  s7_define_constant(s7, "copper-colormap",          C_int_to_Xen_colormap(5));
  s7_define_constant(s7, "pink-colormap",            C_int_to_Xen_colormap(6));
  s7_define_constant(s7, "jet-colormap",             C_int_to_Xen_colormap(7));
  s7_define_constant(s7, "prism-colormap",           C_int_to_Xen_colormap(8));
  s7_define_constant(s7, "autumn-colormap",          C_int_to_Xen_colormap(9));
  s7_define_constant(s7, "winter-colormap",          C_int_to_Xen_colormap(10));
  s7_define_constant(s7, "spring-colormap",          C_int_to_Xen_colormap(11));
  s7_define_constant(s7, "summer-colormap",          C_int_to_Xen_colormap(12));
  s7_define_constant(s7, "rainbow-colormap",         C_int_to_Xen_colormap(13));
  s7_define_constant(s7, "flag-colormap",            C_int_to_Xen_colormap(14));
  s7_define_constant(s7, "phases-colormap",          C_int_to_Xen_colormap(15));
#else
  Xen_define_variable("black-and-white-colormap", colormap_temp[0], C_int_to_Xen_colormap(0));
  Xen_define_variable("gray-colormap",            colormap_temp[1], C_int_to_Xen_colormap(1));
  Xen_define_variable("hot-colormap",             colormap_temp[2], C_int_to_Xen_colormap(2));
  Xen_define_variable("cool-colormap",            colormap_temp[3], C_int_to_Xen_colormap(3));
  Xen_define_variable("bone-colormap",            colormap_temp[4], C_int_to_Xen_colormap(4));
  Xen_define_variable("copper-colormap",          colormap_temp[5], C_int_to_Xen_colormap(5));
  Xen_define_variable("pink-colormap",            colormap_temp[6], C_int_to_Xen_colormap(6));
  Xen_define_variable("jet-colormap",             colormap_temp[7], C_int_to_Xen_colormap(7));
  Xen_define_variable("prism-colormap",           colormap_temp[8], C_int_to_Xen_colormap(8));
  Xen_define_variable("autumn-colormap",          colormap_temp[9], C_int_to_Xen_colormap(9));
  Xen_define_variable("winter-colormap",          colormap_temp[10], C_int_to_Xen_colormap(10));
  Xen_define_variable("spring-colormap",          colormap_temp[11], C_int_to_Xen_colormap(11));
  Xen_define_variable("summer-colormap",          colormap_temp[12], C_int_to_Xen_colormap(12));
  Xen_define_variable("rainbow-colormap",         colormap_temp[13], C_int_to_Xen_colormap(13));
  Xen_define_variable("flag-colormap",            colormap_temp[14], C_int_to_Xen_colormap(14));
  Xen_define_variable("phases-colormap",          colormap_temp[15], C_int_to_Xen_colormap(15));
#endif

  Xen_define_typed_procedure(S_is_colormap,         g_is_colormap_w,         1, 0, 0, H_is_colormap,         s7_make_signature(s7, 2, b, t));
  Xen_define_typed_procedure(S_colormap_ref,        g_colormap_ref_w,        2, 0, 0, H_colormap_ref,        s7_make_signature(s7, 3, p, col, r));
  Xen_define_typed_procedure(S_add_colormap,        g_add_colormap_w,        2, 0, 0, H_add_colormap,        s7_make_signature(s7, 3, col, s, fnc));
  Xen_define_typed_procedure(S_colormap_name,       g_colormap_name_w,       1, 0, 0, H_colormap_name,       s7_make_signature(s7, 2, s, col));
  Xen_define_typed_procedure(S_delete_colormap,     g_delete_colormap_w,     1, 0, 0, H_delete_colormap,     s7_make_signature(s7, 2, col, col));
  Xen_define_typed_procedure(S_integer_to_colormap, g_integer_to_colormap_w, 1, 0, 0, H_integer_to_colormap, 
			     s7_make_signature(s7, 2, s7_make_signature(s7, 2, col, b), i));
  Xen_define_typed_procedure(S_colormap_to_integer, g_colormap_to_integer_w, 1, 0, 0, H_colormap_to_integer, s7_make_signature(s7, 2, i, col));

  Xen_define_typed_dilambda(S_colormap, g_colormap_w, H_colormap,
			    S_set S_colormap, g_set_colormap_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, col),  s7_make_signature(s7, 2, col, col));

  Xen_define_typed_dilambda(S_colormap_size, g_colormap_size_w, H_colormap_size, 
			    S_set S_colormap_size, g_set_colormap_size_w, 0, 0, 1, 0,
			    s7_make_signature(s7, 1, i),  s7_make_signature(s7, 2, i, i));

#if HAVE_SCHEME
  s7_symbol_set_setter(s7, ss->color_map_size_symbol, s7_make_function(s7, "[acc-" S_colormap_size "]", acc_colormap_size, 2, 0, false, "accessor"));
  s7_symbol_set_setter(s7, ss->color_map_symbol, s7_make_function(s7, "[acc-" S_colormap "]", acc_colormap, 2, 0, false, "accessor"));

  s7_symbol_set_documentation(s7, ss->color_map_size_symbol, "*colormap-size*: current colormap size; default is 512.");
  s7_symbol_set_documentation(s7, ss->color_map_symbol, "*colormap*: current colormap choice.");
#endif
}
