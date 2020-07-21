#include <locale.h>
#include <notcurses/notcurses.h>
#include "s7.h"

static s7_pointer g_notcurses_version(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, notcurses_version()));
}

/* -------- ncdirect -------- */
static s7_pointer g_ncdirect_init(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncdirect_init((const char *)s7_string(s7_car(args)), (FILE *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_fg_default(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_fg_default((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_bg_default(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_bg_default((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_dim_x(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_dim_x((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_dim_y(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_dim_y((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_cursor_enable(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_enable((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_cursor_disable(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_disable((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_cursor_push(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_push((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_cursor_pop(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_pop((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_clear(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_clear((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_stop(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_stop((struct ncdirect *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdirect_fg(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_fg((struct ncdirect *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_bg(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_bg((struct ncdirect *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_styles_set(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_styles_set((struct ncdirect *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_styles_on(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_styles_on((struct ncdirect *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_styles_off(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_styles_off((struct ncdirect *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_cursor_up(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_up((struct ncdirect *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_cursor_left(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_left((struct ncdirect *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_cursor_right(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_right((struct ncdirect *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_cursor_down(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_down((struct ncdirect *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncdirect_cursor_move_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdirect_cursor_move_yx((struct ncdirect *)s7_c_pointer(s7_car(args)), 
						     (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncdirect_cursor_yx(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0, z;
  z = ncdirect_cursor_yx((struct ncdirect *)s7_c_pointer(s7_car(args)), &y, &x);
  return(s7_list(sc, 3, s7_make_integer(sc, z), s7_make_integer(sc, y), s7_make_integer(sc, x)));
}


#if 0
/* TODO: accessors for structs */
typedef struct notcurses_options {
  const char* termtype;
  FILE* renderfp;
  ncloglevel_e loglevel;
  int margin_t, margin_r, margin_b, margin_l;
  uint64_t flags;
} notcurses_options;
#endif


/* -------- notcurses_options* -------- */
static s7_pointer g_notcurses_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(notcurses_options)), s7_make_symbol(sc, "notcurses_options*"), s7_f(sc)));
}

static s7_pointer g_notcurses_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "notcurses_options*"), __func__, 0));
}


static s7_pointer g_notcurses_options_set_flags(s7_scheme *sc, s7_pointer args)
{
  notcurses_options *no;
  no = (notcurses_options *)s7_c_pointer(s7_car(args));
  no->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_car(args));
}

/* -------- notcurses* -------- */
static s7_pointer g_notcurses_init(s7_scheme *sc, s7_pointer args)
{
  s7_pointer noptions, fp;
  notcurses_options *no;
  FILE *f;
  setlocale(LC_ALL, "");
  noptions = s7_car(args);
  if (noptions == s7_f(sc))
    no = NULL;
  else no = (notcurses_options *)s7_c_pointer(noptions);
  fp = s7_cadr(args);
  if (fp == s7_f(sc))
    f = NULL;
  else f = (FILE *)s7_c_pointer(fp);
  return(s7_make_c_pointer_with_type(sc, notcurses_init(no, f), s7_make_symbol(sc, "notcurses*"), s7_f(sc)));
}

static s7_pointer g_notcurses_stop(s7_scheme *sc, s7_pointer args)
{
  notcurses_stop((struct notcurses *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_notcurses_render(s7_scheme *sc, s7_pointer args)
{
  notcurses_render((struct notcurses *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_notcurses_input_ready_fd(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, notcurses_inputready_fd((struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_mouse_enable(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, notcurses_mouse_enable((struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_mouse_disable(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, notcurses_mouse_disable((struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_cursor_enable(s7_scheme *sc, s7_pointer args)
{
  notcurses_cursor_enable((struct notcurses *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_notcurses_cursor_disable(s7_scheme *sc, s7_pointer args)
{
  notcurses_cursor_disable((struct notcurses *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_notcurses_supported_styles(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)notcurses_supported_styles((struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_top(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, notcurses_top((struct notcurses *)s7_c_pointer(s7_car(args))), s7_make_symbol(sc, "ncplane*"), s7_f(sc)));
}

static s7_pointer g_notcurses_stdplane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, notcurses_stdplane((struct notcurses *)s7_c_pointer(s7_car(args))), s7_make_symbol(sc, "ncplane*"), s7_f(sc)));
}

#if 0
static s7_pointer g_notcurses_stdplane_const(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, notcurses_stdplane_const((const struct notcurses *)s7_c_pointer(s7_car(args))), s7_make_symbol(sc, "ncplane*"), s7_f(sc)));
}
#endif

static s7_pointer g_notcurses_drop_planes(s7_scheme *sc, s7_pointer args)
{
  notcurses_drop_planes((struct notcurses *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_palette256_new(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, palette256_new((struct notcurses *)s7_c_pointer(s7_car(args))), s7_make_symbol(sc, "palette256*"), s7_f(sc)));
}

static s7_pointer g_notcurses_palette_size(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, notcurses_palette_size((struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_canfade(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_canfade((const struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_canchangecolor(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_canchangecolor((const struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_canopen_images(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_canopen_images((const struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_canopen_videos(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_canopen_videos((const struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_canutf8(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_canutf8((const struct notcurses *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_notcurses_cansixel(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_cansixel((const struct notcurses *)s7_c_pointer(s7_car(args)))));
}
 
#if 0
                  static s7_pointer g_timespec_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct timespec)), s7_make_symbol(sc, "timespec*"), s7_f(sc)));}
                  static s7_pointer g_sigset_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(sigset_t)), s7_make_symbol(sc, "sigset_t*"), s7_f(sc)));}

typedef struct ncinput {
  char32_t id;
  int y;
  int x;
  bool alt;
  bool shift;
  bool ctrl;
  uint64_t seqnum;
} ncinput;
                  static s7_pointer g_ncinput_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncinput)), s7_make_symbol(sc, "ncinput*"), s7_f(sc)));}
#endif

static s7_pointer g_notcurses_getc(s7_scheme *sc, s7_pointer args) /* TODO: need local timespec sigset_t ncinput make */
{
  return(s7_make_character(sc, notcurses_getc((struct notcurses *)s7_c_pointer(s7_car(args)),
					      (const struct timespec *)s7_c_pointer(s7_cadr(args)), 
					      (sigset_t *)s7_c_pointer(s7_caddr(args)),
					      (ncinput *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer g_notcurses_refresh(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0, z;
  z = notcurses_refresh((struct notcurses *)s7_c_pointer(s7_car(args)), &x, &y); /* I assume these are by reference */
  return(s7_list(sc, 3, s7_make_integer(sc, z), s7_make_integer(sc, x), s7_make_integer(sc, y)));
}

static s7_pointer g_notcurses_at_yx(s7_scheme *sc, s7_pointer args)
{
  uint32_t attrword = 0;
  uint64_t channels = 0;
  char *c;
  c = notcurses_at_yx((struct notcurses *)s7_c_pointer(s7_car(args)),
		      (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)),
		      &attrword, &channels);
  return(s7_list(sc, 3, s7_make_string(sc, c), s7_make_integer(sc, attrword), s7_make_integer(sc, channels)));
}

static s7_pointer g_notcurses_lex_margins(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, notcurses_lex_margins((const char *)s7_string(s7_car(args)), (notcurses_options *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_notcurses_lex_scalemode(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, notcurses_lex_scalemode((const char *)s7_string(s7_car(args)), (ncscale_e *)s7_c_pointer(s7_cadr(args)))));
}


/* -------- ncstats* -------- */

static s7_pointer g_ncstats_renders(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->renders));}
static s7_pointer g_ncstats_failed_renders(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->failed_renders));}
static s7_pointer g_ncstats_render_bytes(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_bytes));}
static s7_pointer g_ncstats_render_max_bytes(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_max_bytes));}
static s7_pointer g_ncstats_render_min_bytes(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_min_bytes));}
static s7_pointer g_ncstats_render_ns(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_ns));}
static s7_pointer g_ncstats_render_max_ns(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_max_ns));}
static s7_pointer g_ncstats_render_min_ns(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_min_ns));}
static s7_pointer g_ncstats_cellelisions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->cellelisions));}
static s7_pointer g_ncstats_cellemissions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->cellemissions));}
static s7_pointer g_ncstats_fgelisions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->fgelisions));}
static s7_pointer g_ncstats_fgemissions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->fgemissions));}
static s7_pointer g_ncstats_bgelisions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->bgelisions));}
static s7_pointer g_ncstats_bgemissions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->bgemissions));}
static s7_pointer g_ncstats_defaultelisions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->defaultelisions));}
static s7_pointer g_ncstats_defaultemissions(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->defaultemissions));}
static s7_pointer g_ncstats_fbbytes(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->fbbytes));}
static s7_pointer g_ncstats_planes(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->planes));}

static s7_pointer g_ncstats_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (ncstats *)calloc(1, sizeof(ncstats)), s7_make_symbol(sc, "ncstats*"), s7_f(sc)));
}

static s7_pointer g_ncstats_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncstats*"), __func__, 0));
}

static s7_pointer g_notcurses_stats(s7_scheme *sc, s7_pointer args)
{
  notcurses_stats((const struct notcurses *)s7_c_pointer(s7_car(args)), (ncstats *)s7_c_pointer(s7_cadr(args)));
  return(s7_cadr(args));
}

static s7_pointer g_notcurses_reset_stats(s7_scheme *sc, s7_pointer args)
{
  notcurses_reset_stats((struct notcurses *)s7_c_pointer(s7_car(args)), (ncstats *)s7_c_pointer(s7_cadr(args)));
  return(s7_cadr(args));
}



/* -------- ncplane* -------- */

static s7_pointer g_ncplane_notcurses(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncplane_notcurses((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

/* also const struct notcurses* ncplane_notcurses_const(const struct ncplane* n) */

static s7_pointer g_ncplane_destroy(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_destroy((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_move_top(s7_scheme *sc, s7_pointer args)
{
  ncplane_move_top((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_move_bottom(s7_scheme *sc, s7_pointer args)
{
  ncplane_move_bottom((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_rotate_cw(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_rotate_cw((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_rotate_ccw(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_rotate_ccw((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_userptr(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncplane_userptr((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_home(s7_scheme *sc, s7_pointer args)
{
  ncplane_home((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_below(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncplane_below((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_channels(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_channels((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_attr(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_attr((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_erase(s7_scheme *sc, s7_pointer args)
{
  ncplane_erase((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_set_fg_default(s7_scheme *sc, s7_pointer args)
{
  ncplane_set_fg_default((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_set_bg_default(s7_scheme *sc, s7_pointer args)
{
  ncplane_set_bg_default((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_styles(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_styles((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncfadectx_setup(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncfadectx_setup((struct ncplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncfadectx_free(s7_scheme *sc, s7_pointer args)
{
  ncfadectx_free((struct ncfadectx *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncfadectx_iterations(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncfadectx_iterations((const struct ncfadectx *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncplane_set_fg(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_fg((struct ncplane *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_bg(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_bg((struct ncplane *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_styles_set(s7_scheme *sc, s7_pointer args)
{
  ncplane_styles_set((struct ncplane *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_styles_on(s7_scheme *sc, s7_pointer args)
{
  ncplane_styles_on((struct ncplane *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_styles_off(s7_scheme *sc, s7_pointer args)
{
  ncplane_styles_off((struct ncplane *)s7_c_pointer(s7_car(args)), (unsigned)s7_integer(s7_cadr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_set_fg_palindex(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_fg_palindex((struct ncplane *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_bg_palindex(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_bg_palindex((struct ncplane *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_fg_alpha(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_fg_alpha((struct ncplane *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_bg_alpha(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_bg_alpha((struct ncplane *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_channels(s7_scheme *sc, s7_pointer args)
{
  ncplane_set_channels((struct ncplane *)s7_c_pointer(s7_car(args)), (uint64_t)s7_integer(s7_cadr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_set_attr(s7_scheme *sc, s7_pointer args)
{
  ncplane_set_attr((struct ncplane *)s7_c_pointer(s7_car(args)), (uint32_t)s7_integer(s7_cadr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_dim_yx(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0;
  ncplane_dim_yx((const struct ncplane *)s7_c_pointer(s7_car(args)), &y, &x);
  return(s7_list(sc, 2, s7_make_integer(sc, y), s7_make_integer(sc, x)));
}

static s7_pointer g_cell_load(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, cell_load((struct ncplane *)s7_c_pointer(s7_car(args)), 
				       (cell *)s7_c_pointer(s7_cadr(args)),
				       (const char *)s7_string(s7_caddr(args)))));
}

static s7_pointer g_cell_duplicate(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, cell_load((struct ncplane *)s7_c_pointer(s7_car(args)), 
				       (cell *)s7_c_pointer(s7_cadr(args)),
				       (const char *)s7_string(s7_caddr(args)))));
}

static s7_pointer g_cell_release(s7_scheme *sc, s7_pointer args)
{
  cell_release((struct ncplane *)s7_c_pointer(s7_car(args)), (cell *)s7_c_pointer(s7_cadr(args)));
  return(s7_f(sc));
}

static s7_pointer g_cell_extended_gcluster(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, cell_extended_gcluster((struct ncplane *)s7_c_pointer(s7_car(args)), (const cell *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_base_cell(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_base_cell((struct ncplane *)s7_c_pointer(s7_car(args)), (const cell *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_base(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_base((struct ncplane *)s7_c_pointer(s7_car(args)), (cell *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_polyfill_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_polyfill_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
						 (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), 
						 (const cell *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer g_ncplane_putc_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putc_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
					     (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), 
					     (const cell *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer g_ncplane_hline_interp(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_hline_interp((struct ncplane *)s7_c_pointer(s7_car(args)), 
						  (const cell *)s7_c_pointer(s7_cadr(args)),
						  (int)s7_integer(s7_caddr(args)), 
						  (uint64_t)s7_integer(s7_cadddr(args)), 
						  (uint64_t)s7_integer(s7_car(s7_cdddr(args))))));
}

static s7_pointer g_ncplane_vline_interp(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_vline_interp((struct ncplane *)s7_c_pointer(s7_car(args)), 
						  (const cell *)s7_c_pointer(s7_cadr(args)),
						  (int)s7_integer(s7_caddr(args)), 
						  (uint64_t)s7_integer(s7_cadddr(args)), 
						  (uint64_t)s7_integer(s7_car(s7_cdddr(args))))));
}

static s7_pointer g_ncplane_box(s7_scheme *sc, s7_pointer args)
{
  const cell *ul, *ur, *ll, *lr, *hline, *vline;
  int ystop, xstop;
  unsigned int ctlword;
  s7_pointer arg;
  arg = s7_cdr(args);
  ul = (const cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ur = (const cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ll = (const cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  lr = (const cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  hline = (const cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  vline = (const cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ystop = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xstop = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ctlword = (unsigned int)s7_integer(s7_car(arg));
  return(s7_make_integer(sc, ncplane_box((struct ncplane *)s7_c_pointer(s7_car(args)), ul, ur, ll, lr, hline, vline, ystop, xstop, ctlword)));
}

static s7_pointer g_ncplane_move_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_move_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
					     (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)))));

}

static s7_pointer g_ncplane_cursor_move_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_cursor_move_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
						    (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)))));

}

static s7_pointer g_ncplane_set_fg_rgb(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_fg_rgb((struct ncplane *)s7_c_pointer(s7_car(args)), 
						(int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), (int)s7_integer(s7_cadddr(args)))));

}

static s7_pointer g_ncplane_set_bg_rgb(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_bg_rgb((struct ncplane *)s7_c_pointer(s7_car(args)), 
						(int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), (int)s7_integer(s7_cadddr(args)))));

}

static s7_pointer g_ncplane_set_fg_rgb_clipped(s7_scheme *sc, s7_pointer args)
{
  ncplane_set_fg_rgb_clipped((struct ncplane *)s7_c_pointer(s7_car(args)), 
			     (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), (int)s7_integer(s7_cadddr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_set_bg_rgb_clipped(s7_scheme *sc, s7_pointer args)
{
  ncplane_set_bg_rgb_clipped((struct ncplane *)s7_c_pointer(s7_car(args)), 
			     (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), (int)s7_integer(s7_cadddr(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncplane_format(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_format((struct ncplane *)s7_c_pointer(s7_car(args)), 
					    (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)), (uint32_t)s7_integer(s7_cadddr(args)))));
}

static s7_pointer g_ncplane_dup(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncplane_dup((const struct ncplane *)s7_c_pointer(s7_car(args)), (void *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_userptr(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncplane_set_userptr((struct ncplane *)s7_c_pointer(s7_car(args)), (void *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_set_scrolling(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, ncplane_set_scrolling((struct ncplane *)s7_c_pointer(s7_car(args)), s7_boolean(sc, s7_cadr(args)))));
}

static s7_pointer g_ncplane_move_above(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_move_above((struct ncplane *)s7_c_pointer(s7_car(args)), (struct ncplane *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_move_below(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_move_below((struct ncplane *)s7_c_pointer(s7_car(args)), (struct ncplane *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_reparent(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncplane_reparent((struct ncplane *)s7_c_pointer(s7_car(args)), (struct ncplane *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_mergedown(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_mergedown((struct ncplane *)s7_c_pointer(s7_car(args)), (struct ncplane *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncplane_putsimple_stainable(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putsimple_stainable((struct ncplane *)s7_c_pointer(s7_car(args)), s7_character(s7_cadr(args)))));
}

static s7_pointer g_ncplane_translate_abs(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0;
  bool res;
  res = ncplane_translate_abs((const struct ncplane *)s7_c_pointer(s7_car(args)), &y, &x);
  return(s7_list(sc, 3, s7_make_boolean(sc, res), s7_make_integer(sc, y), s7_make_integer(sc, x)));
}

static s7_pointer g_ncplane_yx(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0;
  ncplane_yx((const struct ncplane *)s7_c_pointer(s7_car(args)), &y, &x);
  return(s7_list(sc, 2, s7_make_integer(sc, y), s7_make_integer(sc, x)));
}

static s7_pointer g_ncplane_cursor_yx(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0;
  ncplane_cursor_yx((const struct ncplane *)s7_c_pointer(s7_car(args)), &y, &x);
  return(s7_list(sc, 2, s7_make_integer(sc, y), s7_make_integer(sc, x)));
}

static s7_pointer g_ncplane_center_abs(s7_scheme *sc, s7_pointer args)
{
  int x = 0, y = 0;
  ncplane_center_abs((const struct ncplane *)s7_c_pointer(s7_car(args)), &y, &x);
  return(s7_list(sc, 2, s7_make_integer(sc, y), s7_make_integer(sc, x)));
}

static s7_pointer g_ncplane_at_cursor(s7_scheme *sc, s7_pointer args)
{
  uint32_t attrword = 0;
  uint64_t channels = 0;
  ncplane_at_cursor((struct ncplane *)s7_c_pointer(s7_car(args)), &attrword, &channels);
  return(s7_list(sc, 2, s7_make_integer(sc, attrword), s7_make_integer(sc, channels)));
}

static s7_pointer g_ncplane_putegc_stainable(s7_scheme *sc, s7_pointer args)
{
  int res, sbytes = 0;
  res = ncplane_putegc_stainable((struct ncplane *)s7_c_pointer(s7_car(args)), (const char *)s7_string(s7_cadr(args)), &sbytes);
  return(s7_list(sc, 2, s7_make_integer(sc, res), s7_make_integer(sc, sbytes)));
}

static s7_pointer g_ncplane_putwegc_stainable(s7_scheme *sc, s7_pointer args)
{
  int res, sbytes = 0;
  res = ncplane_putwegc_stainable((struct ncplane *)s7_c_pointer(s7_car(args)), (const wchar_t *)s7_string(s7_cadr(args)), &sbytes);
  return(s7_list(sc, 2, s7_make_integer(sc, res), s7_make_integer(sc, sbytes)));
}

/* fadecb is a function:
 * typedef int (*fadecb)(struct notcurses* nc, struct ncplane* ncp, const struct timespec*, void* curry);
 * int ncplane_fadeout(struct ncplane* n, const struct timespec* ts, fadecb fader, void* curry);
 * int ncplane_fadein(struct ncplane* n, const struct timespec* ts, fadecb fader, void* curry);
 * int ncplane_fadeout_iteration(struct ncplane* n, struct ncfadectx* nctx, int iter, fadecb fader, void* curry);
 * int ncplane_fadein_iteration(struct ncplane* n, struct ncfadectx* nctx, int iter, fadecb fader, void* curry);
 * int ncplane_pulse(struct ncplane* n, const struct timespec* ts, fadecb fader, void* curry);
 */

static s7_pointer g_ncplane_putegc_yx(s7_scheme *sc, s7_pointer args)
{
  int res, sbytes = 0;
  res = ncplane_putegc_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
			  (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)),
			  (const char *)s7_string(s7_cadddr(args)), &sbytes);
  return(s7_list(sc, 2, s7_make_integer(sc, res), s7_make_integer(sc, sbytes)));
}

static s7_pointer g_ncplane_putstr_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putstr_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
					       (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)),
					       (const char *)s7_string(s7_cadddr(args)))));
}

static s7_pointer g_ncplane_putnstr_aligned(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putnstr_aligned((struct ncplane *)s7_c_pointer(s7_car(args)), 
						     (int)s7_integer(s7_cadr(args)), (ncalign_e)s7_integer(s7_caddr(args)),
						     (size_t)s7_integer(s7_cadddr(args)), (const char *)s7_string(s7_car(s7_cdddr(args))))));
}

static s7_pointer g_ncplane_putnstr_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putnstr_yx((struct ncplane *)s7_c_pointer(s7_car(args)), 
						(int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)),
						(size_t)s7_integer(s7_cadddr(args)), (const char *)s7_string(s7_car(s7_cdddr(args))))));
}


/* int ncplane_vprintf_aligned(struct ncplane* n, int y, ncalign_e align, const char* format, va_list ap); */

static s7_pointer g_ncplane_new(s7_scheme *sc, s7_pointer args)
{
  int rows, cols, xoff, yoff;
  void *opaque;
  s7_pointer arg;
  arg = s7_cdr(args);
  rows = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  cols = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  yoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  opaque = (void *)s7_c_pointer(s7_car(arg));
  return(s7_make_c_pointer(sc, (void *)ncplane_new((struct notcurses *)s7_c_pointer(s7_car(args)), rows, cols, yoff, xoff, opaque)));
}

static s7_pointer g_ncplane_aligned(s7_scheme *sc, s7_pointer args)
{
  int rows, cols, yoff;
  ncalign_e align;
  void *opaque;
  s7_pointer arg;
  arg = s7_cdr(args);
  rows = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  cols = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  yoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  align = (ncalign_e)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  opaque = (void *)s7_c_pointer(s7_car(arg));
  return(s7_make_c_pointer(sc, (void *)ncplane_aligned((struct ncplane *)s7_c_pointer(s7_car(args)), rows, cols, yoff, align, opaque)));
}

static s7_pointer g_ncplane_bound(s7_scheme *sc, s7_pointer args)
{
  int rows, cols, yoff, xoff;
  void *opaque;
  s7_pointer arg;
  arg = s7_cdr(args);
  rows = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  cols = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  yoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  opaque = (void *)s7_c_pointer(s7_car(arg));
  return(s7_make_c_pointer(sc, (void *)ncplane_bound((struct ncplane *)s7_c_pointer(s7_car(args)), rows, cols, yoff, xoff, opaque)));
}

static s7_pointer g_ncplane_translate(s7_scheme *sc, s7_pointer args)
{
  int y = 0, x = 0;
  ncplane_translate((const struct ncplane *)s7_c_pointer(s7_car(args)), 
		    (const struct ncplane *)s7_c_pointer(s7_cadr(args)), &y, &x);
  return(s7_list(sc, 2, s7_make_integer(sc, y), s7_make_integer(sc, x)));
}

static s7_pointer g_ncplane_resize(s7_scheme *sc, s7_pointer args)
{
  int keepy, keepx, keepleny, keeplenx, yoff, xoff, ylen, xlen;
  s7_pointer arg;
  arg = s7_cdr(args);
  keepy = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  keepx = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  keepleny = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  keeplenx = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  yoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xoff = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ylen = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xlen = (int)s7_integer(s7_car(arg));
  return(s7_make_integer(sc, ncplane_resize((struct ncplane *)s7_c_pointer(s7_car(args)),
					    keepy, keepx, keepleny, keeplenx,
					    yoff, xoff, ylen, xlen)));
}

static s7_pointer g_ncplane_set_base(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_set_base((struct ncplane *)s7_car(args),
					      (const char *)s7_string(s7_cadr(args)),
					      (uint32_t)s7_integer(s7_caddr(args)), (uint64_t)s7_integer(s7_cadddr(args)))));
}

static s7_pointer g_ncplane_at_yx(s7_scheme *sc, s7_pointer args)
{
  char *res;
  uint32_t attrword = 0;
  uint64_t channels = 0;
  res = ncplane_at_yx((const struct ncplane *)s7_car(args),
		      (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)),
		      &attrword, &channels);
  return(s7_list(sc, 3, s7_make_string(sc, res), s7_make_integer(sc, attrword), s7_make_integer(sc, channels)));
}

static s7_pointer g_ncplane_contents(s7_scheme *sc, s7_pointer args)
{
  int begy, begx, leny, lenx;
  s7_pointer arg;
  arg = s7_cdr(args);
  begy = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  begx = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  leny = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  lenx = s7_integer(s7_car(arg)); 
  return(s7_make_string(sc, ncplane_contents((const struct ncplane *)s7_c_pointer(s7_car(args)), begy, begx, leny, lenx)));
}

/* int ncplane_vprintf_yx(struct ncplane* n, int y, int x, const char* format, va_list ap); */

static s7_pointer g_ncplane_putstr_aligned(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putstr_aligned((struct ncplane *)s7_c_pointer(s7_car(args)),
						    (int)s7_integer(s7_cadr(args)), (ncalign_e)s7_integer(s7_caddr(args)),
						    (const char *)s7_string(s7_cadddr(args)))));
}

static s7_pointer g_ncplane_puttext(s7_scheme *sc, s7_pointer args)
{
  size_t bytes = 0;
  int res;
  res = ncplane_puttext((struct ncplane *)s7_c_pointer(s7_car(args)), 
			(int)s7_integer(s7_cadr(args)), (ncalign_e)s7_integer(s7_caddr(args)),
			(const char *)s7_string(s7_cadddr(args)), &bytes);
  return(s7_list(sc, 2, s7_make_integer(sc, res), s7_make_integer(sc, bytes)));
}

static s7_pointer g_ncplane_stain(s7_scheme *sc, s7_pointer args)
{
  int ystop, xstop, ul, ur, ll, lr;
  s7_pointer arg;
  arg = s7_cdr(args);
  ystop = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xstop = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ul = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ur = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ll = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  lr = s7_integer(s7_car(arg));
  return(s7_make_integer(sc, ncplane_stain((struct ncplane *)s7_c_pointer(s7_car(args)), ystop, xstop, ul, ur, ll, lr)));
}

static s7_pointer g_ncplane_highgradient(s7_scheme *sc, s7_pointer args)
{
  int ystop, xstop, ul, ur, ll, lr;
  s7_pointer arg;
  arg = s7_cdr(args);
  ul = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ur = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ll = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  lr = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ystop = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xstop = s7_integer(s7_car(arg));
  return(s7_make_integer(sc, ncplane_highgradient((struct ncplane *)s7_c_pointer(s7_car(args)), ul, ur, ll, lr, ystop, xstop)));
}

static s7_pointer g_ncplane_gradient(s7_scheme *sc, s7_pointer args)
{
  int ystop, xstop, ul, ur, ll, lr;
  uint32_t attrword;
  const char *egc;
  s7_pointer arg;
  arg = s7_cdr(args);
  egc = (const char *)s7_string(s7_car(arg)); arg = s7_cdr(arg);
  attrword = (uint32_t)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ul = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ur = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ll = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  lr = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ystop = s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  xstop = s7_integer(s7_car(arg));
  return(s7_make_integer(sc, ncplane_gradient((struct ncplane *)s7_c_pointer(s7_car(args)), egc, attrword, ul, ur, ll, lr, ystop, xstop)));
}


/* ---------------- initialization ---------------- */

void notcurses_s7_init(s7_scheme *sc);
void notcurses_s7_init(s7_scheme *sc)
{
  s7_pointer old_curlet, old_shadow, notcurses_let;

  s7_define_constant(sc, "*notcurses*", notcurses_let = s7_inlet(sc, s7_nil(sc)));
  old_curlet = s7_set_curlet(sc, notcurses_let);
  old_shadow = s7_set_shadow_rootlet(sc, notcurses_let);

  #define nc_int(Name) s7_define(sc, notcurses_let, s7_make_symbol(sc, #Name), s7_make_integer(sc, (s7_int)Name))

  nc_int(NCOPTION_INHIBIT_SETLOCALE);
  nc_int(NCOPTION_VERIFY_SIXEL);
  nc_int(NCOPTION_NO_WINCH_SIGHANDLER);
  nc_int(NCOPTION_NO_QUIT_SIGHANDLERS);
  nc_int(NCOPTION_RETAIN_CURSOR);
  nc_int(NCOPTION_SUPPRESS_BANNERS);
  nc_int(NCOPTION_NO_ALTERNATE_SCREEN);

  nc_int(CELL_WIDEASIAN_MASK);
  nc_int(CELL_BGDEFAULT_MASK);
  nc_int(CELL_FGDEFAULT_MASK);
  nc_int(CELL_BG_RGB_MASK);
  nc_int(CELL_FG_RGB_MASK);
  nc_int(CELL_BG_PALETTE);
  nc_int(CELL_FG_PALETTE);
  nc_int(CELL_BG_ALPHA_MASK);
  nc_int(CELL_FG_ALPHA_MASK);

  nc_int(CELL_ALPHA_HIGHCONTRAST);
  nc_int(CELL_ALPHA_TRANSPARENT);
  nc_int(CELL_ALPHA_BLEND);
  nc_int(CELL_ALPHA_OPAQUE);

  nc_int(NCSTYLE_MASK);
  nc_int(NCSTYLE_STANDOUT);
  nc_int(NCSTYLE_UNDERLINE);
  nc_int(NCSTYLE_REVERSE);
  nc_int(NCSTYLE_BLINK);
  nc_int(NCSTYLE_DIM);
  nc_int(NCSTYLE_BOLD);
  nc_int(NCSTYLE_INVIS);
  nc_int(NCSTYLE_PROTECT);
  nc_int(NCSTYLE_ITALIC);

  nc_int(WCHAR_MAX_UTF8BYTES);

  nc_int(NCBOXMASK_TOP);
  nc_int(NCBOXMASK_RIGHT);
  nc_int(NCBOXMASK_BOTTOM);
  nc_int(NCBOXMASK_LEFT);
  nc_int(NCBOXGRAD_TOP);
  nc_int(NCBOXGRAD_RIGHT);
  nc_int(NCBOXGRAD_BOTTOM);
  nc_int(NCBOXGRAD_LEFT);
  nc_int(NCBOXCORNER_MASK);
  nc_int(NCBOXCORNER_SHIFT);

  nc_int(NCPALETTESIZE);

  nc_int(NCVISUAL_OPTION_NODEGRADE);
  nc_int(NCVISUAL_OPTION_BLEND);

  nc_int(NCREEL_OPTION_INFINITESCROLL);
  nc_int(NCREEL_OPTION_CIRCULAR);

  nc_int(PREFIXCOLUMNS);
  nc_int(IPREFIXCOLUMNS);
  nc_int(BPREFIXCOLUMNS);
  nc_int(PREFIXSTRLEN);
  nc_int(IPREFIXSTRLEN);
  nc_int(BPREFIXSTRLEN);

  nc_int(NCREADER_OPTION_HORSCROLL);
  nc_int(NCREADER_OPTION_VERSCROLL);

  nc_int(NCPLOT_OPTION_LABELTICKSD);
  nc_int(NCPLOT_OPTION_EXPONENTIALD);
  nc_int(NCPLOT_OPTION_VERTICALI);
  nc_int(NCPLOT_OPTION_NODEGRADE);
  nc_int(NCPLOT_OPTION_DETECTMAXONLY);

  nc_int(NCMENU_OPTION_BOTTOM);
  nc_int(NCMENU_OPTION_HIDING);

  nc_int(NCLOGLEVEL_SILENT);
  nc_int(NCLOGLEVEL_PANIC);
  nc_int(NCLOGLEVEL_FATAL);
  nc_int(NCLOGLEVEL_ERROR);
  nc_int(NCLOGLEVEL_WARNING);
  nc_int(NCLOGLEVEL_INFO);
  nc_int(NCLOGLEVEL_VERBOSE);
  nc_int(NCLOGLEVEL_DEBUG);
  nc_int(NCLOGLEVEL_TRACE);

  nc_int(NCSCALE_NONE);
  nc_int(NCSCALE_SCALE);
  nc_int(NCSCALE_STRETCH);

  nc_int(NCALIGN_LEFT);
  nc_int(NCALIGN_CENTER);
  nc_int(NCALIGN_RIGHT);

  nc_int(NCBLIT_DEFAULT);
  nc_int(NCBLIT_1x1);
  nc_int(NCBLIT_2x1);
  nc_int(NCBLIT_1x1x4);
  nc_int(NCBLIT_2x2);
  nc_int(NCBLIT_4x1);
  nc_int(NCBLIT_BRAILLE);
  nc_int(NCBLIT_8x1);
  nc_int(NCBLIT_SIXEL);

  #define nc_func(Name, Req, Opt, Rst) s7_define_function(sc, #Name, g_ ## Name, Req, Opt, Rst, NULL)
    
  nc_func(ncdirect_init, 2, 0, false);
  nc_func(ncdirect_fg_default, 1, 0, false);
  nc_func(ncdirect_bg_default, 1, 0, false);
  nc_func(ncdirect_dim_x, 1, 0, false);
  nc_func(ncdirect_dim_y, 1, 0, false);
  nc_func(ncdirect_cursor_enable, 1, 0, false);
  nc_func(ncdirect_cursor_disable, 1, 0, false);
  nc_func(ncdirect_cursor_push, 1, 0, false);
  nc_func(ncdirect_cursor_pop, 1, 0, false);
  nc_func(ncdirect_clear, 1, 0, false);
  nc_func(ncdirect_stop, 1, 0, false);
  nc_func(ncdirect_fg, 2, 0, false);
  nc_func(ncdirect_bg, 2, 0, false);
  nc_func(ncdirect_styles_set, 2, 0, false);
  nc_func(ncdirect_styles_on, 2, 0, false);
  nc_func(ncdirect_styles_off, 2, 0, false);
  nc_func(ncdirect_cursor_up, 2, 0, false);
  nc_func(ncdirect_cursor_left, 2, 0, false);
  nc_func(ncdirect_cursor_right, 2, 0, false);
  nc_func(ncdirect_cursor_down, 2, 0, false);
  nc_func(ncdirect_cursor_move_yx, 3, 0, false);
  nc_func(ncdirect_cursor_yx, 1, 0, false);

  nc_func(notcurses_version, 0, 0, false);
  nc_func(notcurses_options_make, 0, 0, false);
  nc_func(notcurses_options_free, 1, 0, false);
  nc_func(notcurses_options_set_flags, 2, 0, false);

  nc_func(notcurses_init, 0, 2, false);
  nc_func(notcurses_stop, 1, 0, false);
  nc_func(notcurses_render, 1, 0, false);
  nc_func(notcurses_input_ready_fd, 1, 0, false);
  nc_func(notcurses_mouse_enable, 1, 0, false);
  nc_func(notcurses_mouse_disable, 1, 0, false);
  nc_func(notcurses_supported_styles, 1, 0, false);
  nc_func(notcurses_palette_size, 1, 0, false);
  nc_func(notcurses_canfade, 1, 0, false);
  nc_func(notcurses_canchangecolor, 1, 0, false);
  nc_func(notcurses_canopen_images, 1, 0, false);
  nc_func(notcurses_canopen_videos, 1, 0, false);
  nc_func(notcurses_canutf8, 1, 0, false);
  nc_func(notcurses_cansixel, 1, 0, false);
  nc_func(notcurses_top, 1, 0, false);
  nc_func(notcurses_drop_planes, 1, 0, false);
  nc_func(notcurses_stdplane, 1, 0, false);
  /* nc_func(notcurses_stdplane_const, 1, 0, false); */
  nc_func(notcurses_cursor_enable, 1, 0, false);
  nc_func(notcurses_cursor_disable, 1, 0, false);
  nc_func(notcurses_getc, 4, 0, false);
  nc_func(notcurses_refresh, 1, 0, false);
  nc_func(notcurses_at_yx, 5, 0, false);
  nc_func(notcurses_lex_margins, 2, 0, false);
  nc_func(notcurses_lex_scalemode, 2, 0, false);
  nc_func(palette256_new, 1, 0, false);

  nc_func(ncstats_make, 0, 0, false);
  nc_func(ncstats_free, 1, 0, false);
  nc_func(notcurses_stats, 2, 0, false);
  nc_func(notcurses_reset_stats, 2, 0, false);
  nc_func(ncstats_renders, 1, 0, false);
  nc_func(ncstats_failed_renders, 1, 0, false);
  nc_func(ncstats_render_bytes, 1, 0, false);
  nc_func(ncstats_render_max_bytes, 1, 0, false);
  nc_func(ncstats_render_min_bytes, 1, 0, false);
  nc_func(ncstats_render_ns, 1, 0, false);
  nc_func(ncstats_render_max_ns, 1, 0, false);
  nc_func(ncstats_render_min_ns, 1, 0, false);
  nc_func(ncstats_cellelisions, 1, 0, false);
  nc_func(ncstats_cellemissions, 1, 0, false);
  nc_func(ncstats_fgelisions, 1, 0, false);
  nc_func(ncstats_fgemissions, 1, 0, false);
  nc_func(ncstats_bgelisions, 1, 0, false);
  nc_func(ncstats_bgemissions, 1, 0, false);
  nc_func(ncstats_defaultelisions, 1, 0, false);
  nc_func(ncstats_defaultemissions, 1, 0, false);
  nc_func(ncstats_fbbytes, 1, 0, false);
  nc_func(ncstats_planes, 1, 0, false);

  nc_func(ncplane_notcurses, 1, 0, false);
  nc_func(ncplane_destroy, 1, 0, false);
  nc_func(ncplane_move_top, 1, 0, false);
  nc_func(ncplane_move_bottom, 1, 0, false);
  nc_func(ncplane_rotate_cw, 1, 0, false);
  nc_func(ncplane_rotate_ccw, 1, 0, false);
  nc_func(ncplane_userptr, 1, 0, false);
  nc_func(ncplane_home, 1, 0, false);
  nc_func(ncplane_below, 1, 0, false);
  nc_func(ncplane_channels, 1, 0, false);
  nc_func(ncplane_attr, 1, 0, false);
  nc_func(ncplane_erase, 1, 0, false);
  nc_func(ncplane_set_fg_default, 1, 0, false);
  nc_func(ncplane_set_bg_default, 1, 0, false);
  nc_func(ncplane_styles, 1, 0, false);

  nc_func(ncplane_set_fg, 2, 0, false);
  nc_func(ncplane_set_bg, 2, 0, false);
  nc_func(ncplane_styles_set, 2, 0, false);
  nc_func(ncplane_styles_on, 2, 0, false);
  nc_func(ncplane_styles_off, 2, 0, false);
  nc_func(ncplane_set_fg_palindex, 2, 0, false);
  nc_func(ncplane_set_bg_palindex, 2, 0, false);
  nc_func(ncplane_set_fg_alpha, 2, 0, false);
  nc_func(ncplane_set_bg_alpha, 2, 0, false);
  nc_func(ncplane_set_channels, 2, 0, false);
  nc_func(ncplane_set_attr, 2, 0, false);
  nc_func(ncplane_dim_yx, 1, 0, false);

  nc_func(ncplane_set_base_cell, 2, 0, false);
  nc_func(ncplane_base, 2, 0, false);
  nc_func(ncplane_polyfill_yx, 4, 0, false);
  nc_func(ncplane_putc_yx, 4, 0, false);
  nc_func(ncplane_hline_interp, 5, 0, false);
  nc_func(ncplane_vline_interp, 5, 0, false);
  nc_func(ncplane_box, 10, 0, false);

  nc_func(ncplane_move_yx, 3, 0, false);
  nc_func(ncplane_cursor_move_yx, 3, 0, false);
  nc_func(ncplane_set_fg_rgb, 4, 0, false);
  nc_func(ncplane_set_bg_rgb, 4, 0, false);
  nc_func(ncplane_set_bg_rgb_clipped, 4, 0, false);
  nc_func(ncplane_set_fg_rgb_clipped, 4, 0, false);
  nc_func(ncplane_format, 4, 0, false);

  nc_func(ncplane_dup, 2, 0, false);
  nc_func(ncplane_set_scrolling, 2, 0, false);
  nc_func(ncplane_move_above, 2, 0, false);
  nc_func(ncplane_move_below, 2, 0, false);
  nc_func(ncplane_set_userptr, 2, 0, false);
  nc_func(ncplane_putsimple_stainable, 2, 0, false);
  nc_func(ncplane_reparent, 2, 0, false);
  nc_func(ncplane_mergedown, 2, 0, false);

  nc_func(ncplane_translate_abs, 1, 0, false);
  nc_func(ncplane_yx, 1, 0, false);
  nc_func(ncplane_center_abs, 1, 0, false);
  nc_func(ncplane_cursor_yx, 1, 0, false);
  nc_func(ncplane_at_cursor, 1, 0, false);

  nc_func(ncplane_putegc_stainable, 2, 0, false);
  nc_func(ncplane_putwegc_stainable, 2, 0, false);
  nc_func(ncplane_putegc_yx, 4, 0, false);
  nc_func(ncplane_putstr_yx, 4, 0, false);
  nc_func(ncplane_putnstr_aligned, 5, 0, false);
  nc_func(ncplane_putnstr_yx, 5, 0, false);
  nc_func(ncplane_new, 6, 0, false);
  nc_func(ncplane_aligned, 6, 0, false);
  nc_func(ncplane_bound, 6, 0, false);
  nc_func(ncplane_translate, 2, 0, false);
  nc_func(ncplane_resize, 9, 0, false);
  nc_func(ncplane_set_base, 4, 0, false);
  nc_func(ncplane_at_yx, 3, 0, false);
  nc_func(ncplane_contents, 5, 0, false);
  nc_func(ncplane_putstr_aligned, 4, 0, false);
  nc_func(ncplane_puttext, 4, 0, false);
  nc_func(ncplane_stain, 7, 0, false);
  nc_func(ncplane_highgradient, 7, 0, false);
  nc_func(ncplane_gradient, 9, 0, false);

  nc_func(cell_load, 3, 0, false);
  nc_func(cell_duplicate, 3, 0, false);
  nc_func(cell_release, 2, 0, false);
  nc_func(cell_extended_gcluster, 2, 0, false);

  nc_func(ncfadectx_setup, 1, 0, false);
  nc_func(ncfadectx_free, 1, 0, false);
  nc_func(ncfadectx_iterations, 1, 0, false);

  s7_set_curlet(sc, old_curlet);
  s7_set_shadow_rootlet(sc, old_shadow);
}


/* gcc -fPIC -c notcurses_s7.c
 * gcc notcurses_s7.o -shared -o notcurses_s7.so -lnotcurses
 * repl
 *   > (load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))
 *   > (notcurses_version)
 */


#if 0
typedef struct cell {
  uint32_t gcluster;
  uint32_t attrword;
  uint64_t channels;
} cell;

#define CELL_TRIVIAL_INITIALIZER { .gcluster = '\0', .attrword = 0, .channels = 0, }
#define CELL_SIMPLE_INITIALIZER(c) { .gcluster = (c), .attrword = 0, .channels = 0, }
#define CELL_INITIALIZER(c, a, chan) { .gcluster = (c), .attrword = (a), .channels = (chan), }

API int cells_rounded_box(struct ncplane* n, uint32_t attr, uint64_t channels, cell* ul, cell* ur, cell* ll, cell* lr, cell* hl, cell* vl);
API int cells_double_box(struct ncplane* n, uint32_t attr, uint64_t channels,  cell* ul, cell* ur, cell* ll, cell* lr, cell* hl, cell* vl);

API struct ncvisual* ncvisual_from_file(const char* file, nc_err_e* ncerr);
API struct ncvisual* ncvisual_from_rgba(const void* rgba, int rows, int rowstride, int cols);
API struct ncvisual* ncvisual_from_bgra(const void* rgba, int rows, int rowstride, int cols);

API struct ncvisual* ncvisual_from_plane(const struct ncplane* n, ncblitter_e blit, int begy, int begx, int leny, int lenx);

struct ncvisual_options {
  struct ncplane* n;
  ncscale_e scaling;
  int y, x;
  int begy, begx;
  int leny, lenx;
  ncblitter_e blitter;
  uint64_t flags;
};

API uint32_t* ncplane_rgba(const struct ncplane* nc, ncblitter_e blit, int begy, int begx, int leny, int lenx);
API int ncvisual_geom(const struct notcurses* nc, const struct ncvisual* n, const struct ncvisual_options* vopts, int* y, int* x, int* toy, int* tox);
API void ncvisual_destroy(struct ncvisual* ncv);
API nc_err_e ncvisual_decode(struct ncvisual* nc);
API nc_err_e ncvisual_rotate(struct ncvisual* n, double rads);
API nc_err_e ncvisual_resize(struct ncvisual* n, int rows, int cols);
API int ncvisual_polyfill_yx(struct ncvisual* n, int y, int x, uint32_t rgba);
API int ncvisual_at_yx(const struct ncvisual* n, int y, int x, uint32_t* pixel);
API int ncvisual_set_yx(const struct ncvisual* n, int y, int x, uint32_t pixel);
API struct ncplane* ncvisual_render(struct notcurses* nc, struct ncvisual* ncv, const struct ncvisual_options* vopts);
API char* ncvisual_subtitle(const struct ncvisual* ncv);

typedef int (*streamcb)(struct ncvisual*, struct ncvisual_options*, const struct timespec*, void*);

API int ncvisual_simple_streamer(struct ncvisual* ncv, struct ncvisual_options* vopts, const struct timespec* tspec, void* curry);
API int ncvisual_stream(struct notcurses* nc, struct ncvisual* ncv, nc_err_e* ncerr, float timescale, streamcb streamer, const struct ncvisual_options* vopts, void* curry);
API int ncblit_rgba(const void* data, int linesize, const struct ncvisual_options* vopts);
API int ncblit_bgrx(const void* data, int linesize, const struct ncvisual_options* vopts);

typedef struct ncreel_options {
  int min_supported_cols;
  int min_supported_rows;
  int max_supported_cols;
  int max_supported_rows;
  int toff, roff, boff, loff;
  unsigned bordermask;
  uint64_t borderchan;
  unsigned tabletmask;
  uint64_t tabletchan;
  uint64_t focusedchan;
  uint64_t bgchannel;
  uint64_t flags;
} ncreel_options;

struct nctablet;
struct ncreel;

API struct ncreel* ncreel_create(struct ncplane* nc, const ncreel_options* popts, int efd);
API struct ncplane* ncreel_plane(struct ncreel* pr);

typedef int (*tabletcb)(struct nctablet* t, int begx, int begy, int maxx, int maxy, bool cliptop);

API struct nctablet* ncreel_add(struct ncreel* pr, struct nctablet* after, struct nctablet* before, tabletcb cb, void* opaque);
API int ncreel_tabletcount(const struct ncreel* pr);
API int ncreel_touch(struct ncreel* pr, struct nctablet* t);
API int ncreel_del(struct ncreel* pr, struct nctablet* t);
API int ncreel_del_focused(struct ncreel* pr);
API int ncreel_move(struct ncreel* pr, int y, int x);
API int ncreel_redraw(struct ncreel* pr);
API struct nctablet* ncreel_focused(struct ncreel* pr);
API struct nctablet* ncreel_next(struct ncreel* pr);
API struct nctablet* ncreel_prev(struct ncreel* pr);
API int ncreel_destroy(struct ncreel* pr);
API void* nctablet_userptr(struct nctablet* t);
API struct ncplane* nctablet_ncplane(struct nctablet* t);

#define NCMETRICFWIDTH(x, cols) ((int)(strlen(x) - mbswidth(x) + (cols)))
#define PREFIXFMT(x) NCMETRICFWIDTH((x), PREFIXCOLUMNS), (x)
#define IPREFIXFMT(x) NCMETRIXFWIDTH((x), IPREFIXCOLUMNS), (x)
#define BPREFIXFMT(x) NCMETRICFWIDTH((x), BPREFIXCOLUMNS), (x)

API const char* ncmetric(uintmax_t val, uintmax_t decimal, char* buf, int omitdec, uintmax_t mult, int uprefix);

typedef struct palette256 {uint32_t chans[NCPALETTESIZE];} palette256;

API int palette256_use(struct notcurses* nc, const palette256* p);
API void palette256_free(palette256* p);
API void ncplane_greyscale(struct ncplane* n);

struct ncselector_item {
  char* option;
  char* desc;
  size_t opcolumns;
  size_t desccolumns;
};

typedef struct ncselector_options {
  char* title;
  char* secondary;
  char* footer;
  struct ncselector_item* items;
  unsigned itemcount;
  unsigned defidx;
  unsigned maxdisplay;
  uint64_t opchannels;
  uint64_t descchannels;
  uint64_t titlechannels;
  uint64_t footchannels;
  uint64_t boxchannels;
  uint64_t bgchannels;
  uint64_t flags;
} ncselector_options;

API struct ncselector* ncselector_create(struct ncplane* n, int y, int x, const ncselector_options* opts);
API int ncselector_additem(struct ncselector* n, const struct ncselector_item* item);
API int ncselector_delitem(struct ncselector* n, const char* item);
API const char* ncselector_selected(const struct ncselector* n);
API struct ncplane* ncselector_plane(struct ncselector* n);
API const char* ncselector_previtem(struct ncselector* n);
API const char* ncselector_nextitem(struct ncselector* n);
API bool ncselector_offer_input(struct ncselector* n, const struct ncinput* nc);
API void ncselector_destroy(struct ncselector* n, char** item);

struct ncmselector_item {
  char* option;
  char* desc;
  bool selected;
};

typedef struct ncmultiselector_options {
  char* title;
  char* secondary;
  char* footer;
  struct ncmselector_item* items;
  unsigned itemcount;
  unsigned maxdisplay;
  uint64_t opchannels;
  uint64_t descchannels;
  uint64_t titlechannels;
  uint64_t footchannels;
  uint64_t boxchannels;
  uint64_t bgchannels;
  uint64_t flags;
} ncmultiselector_options;

API struct ncmultiselector* ncmultiselector_create(struct ncplane* n, int y, int x, const ncmultiselector_options* opts);
API int ncmultiselector_selected(struct ncmultiselector* n, bool* selected, unsigned count);
API struct ncplane* ncmultiselector_plane(struct ncmultiselector* n);
API bool ncmultiselector_offer_input(struct ncmultiselector* n, const struct ncinput* nc);
API void ncmultiselector_destroy(struct ncmultiselector* n, char** item);

struct ncmenu_item {
  char* desc;
  ncinput shortcut;
};

struct ncmenu_section {
  char* name;
  int itemcount;
  struct ncmenu_item* items;
  ncinput shortcut;
};

typedef struct ncmenu_options {
  struct ncmenu_section* sections;
  int sectioncount;
  uint64_t headerchannels;
  uint64_t sectionchannels;
  uint64_t flags;
} ncmenu_options;

API struct ncmenu* ncmenu_create(struct ncplane* nc, const ncmenu_options* opts);
API int ncmenu_unroll(struct ncmenu* n, int sectionidx);
API int ncmenu_rollup(struct ncmenu* n);
API int ncmenu_nextsection(struct ncmenu* n);
API int ncmenu_prevsection(struct ncmenu* n);
API int ncmenu_nextitem(struct ncmenu* n);
API int ncmenu_previtem(struct ncmenu* n);
API const char* ncmenu_selected(const struct ncmenu* n, struct ncinput* ni);
API struct ncplane* ncmenu_plane(struct ncmenu* n);
API bool ncmenu_offer_input(struct ncmenu* n, const struct ncinput* nc);
API int ncmenu_destroy(struct ncmenu* n);

typedef struct ncplot_options {
  uint64_t maxchannel;
  uint64_t minchannel;
  ncblitter_e gridtype;
  int rangex;
  uint64_t flags;
} ncplot_options;

API struct ncuplot* ncuplot_create(struct ncplane* n, const ncplot_options* opts, uint64_t miny, uint64_t maxy);
API struct ncdplot* ncdplot_create(struct ncplane* n, const ncplot_options* opts, double miny, double maxy);
API struct ncplane* ncuplot_plane(struct ncuplot* n);
API struct ncplane* ncdplot_plane(struct ncdplot* n);
API int ncuplot_add_sample(struct ncuplot* n, uint64_t x, uint64_t y);
API int ncdplot_add_sample(struct ncdplot* n, uint64_t x, double y);
API int ncuplot_set_sample(struct ncuplot* n, uint64_t x, uint64_t y);
API int ncdplot_set_sample(struct ncdplot* n, uint64_t x, double y);
API int ncuplot_sample(const struct ncuplot* n, uint64_t x, uint64_t* y);
API int ncdplot_sample(const struct ncdplot* n, uint64_t x, double* y);
API void ncuplot_destroy(struct ncuplot* n);
API void ncdplot_destroy(struct ncdplot* n);

typedef int(*ncfdplane_callback)(struct ncfdplane* n, const void* buf, size_t s, void* curry);
typedef int(*ncfdplane_done_cb)(struct ncfdplane* n, int fderrno, void* curry);

typedef struct ncfdplane_options {
  void* curry;
  bool follow;
  uint64_t flags;
} ncfdplane_options;

API struct ncfdplane* ncfdplane_create(struct ncplane* n, const ncfdplane_options* opts, int fd, ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
API struct ncplane* ncfdplane_plane(struct ncfdplane* n);
API int ncfdplane_destroy(struct ncfdplane* n);

typedef struct ncsubproc_options {
  void* curry;
  uint64_t restart_period;
  uint64_t flags;
} ncsubproc_options;

API struct ncsubproc* ncsubproc_createv(struct ncplane* n, const ncsubproc_options* opts, const char* bin,  char* const arg[], ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
API struct ncsubproc* ncsubproc_createvp(struct ncplane* n, const ncsubproc_options* opts, const char* bin,  char* const arg[], ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
API struct ncsubproc* ncsubproc_createvpe(struct ncplane* n, const ncsubproc_options* opts, const char* bin,  char* const arg[], char* const env[], ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
API struct ncplane* ncsubproc_plane(struct ncsubproc* n);
API int ncsubproc_destroy(struct ncsubproc* n);
API int ncplane_qrcode(struct ncplane* n, ncblitter_e blitter, int* ymax, int* xmax, const void* data, size_t len);


typedef struct ncreader_options {
  uint64_t tchannels;
  uint64_t echannels;
  uint32_t tattrword;
  uint32_t eattrword;
  char* egc;
  int physrows;
  int physcols;
  uint64_t flags;
} ncreader_options;

API struct ncreader* ncreader_create(struct ncplane* nc, int y, int x, const ncreader_options* opts);
API int ncreader_clear(struct ncreader* n);
API struct ncplane* ncreader_plane(struct ncreader* n);
API bool ncreader_offer_input(struct ncreader* n, const struct ncinput* ni);
API char* ncreader_contents(const struct ncreader* n);
API void ncreader_destroy(struct ncreader* n, char** contents);
API void notcurses_debug(struct notcurses* nc, FILE* debugfp);

struct blitset {
  ncblitter_e geom;
  int width;
  int height;
  const wchar_t* egcs;
  int (*blit)(struct ncplane* nc, int placey, int placex, int linesize,
              const void* data, int begy, int begx, int leny, int lenx,
              bool bgr, bool blendcolors);
  bool fill;
};

API extern const struct blitset notcurses_blitters[];
#endif

