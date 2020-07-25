/* s7 FFI for the notcurses library
 *
 *   Fedora: notcurses notcurses-devel notcurses-utils
 */

#include <locale.h>
#include <notcurses/notcurses.h>
#include "s7.h"

static s7_pointer g_notcurses_version(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, notcurses_version()));
}


/* -------- ncdirect -------- */
#if 0
/*
  gcc -fPIC -c notcurses_s7.c
  gcc notcurses_s7.o -shared -o notcurses_s7.so -lnotcurses

  (load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))
  (define ncd (ncdirect_init (c-pointer 0)))
  (when (not (equal? ncd (c-pointer 0)))
    (ncdirect_styles_on ncd NCSTYLE_STANDOUT)
    (ncdirect_fg ncd #x0339dc)
    (format *stdout* "a test\n") ; printed in blue
    (ncdirect_fg_default ncd)
    (ncdirect_styles_off ncd NCSTYLE_STANDOUT)
    (ncdirect_stop ncd))
  (exit)
*/
#endif

static s7_pointer g_ncdirect_init(s7_scheme *sc, s7_pointer args)
{
  s7_pointer termtype;
  FILE *fp;
  termtype = s7_car(args);
  if (s7_is_pair(s7_cdr(args)))
    fp = (FILE *)s7_c_pointer(s7_cadr(args));
  else fp = stdout;
  if ((s7_is_c_pointer(termtype)) &&
      (s7_c_pointer(termtype) == NULL))
    return(s7_make_c_pointer(sc, ncdirect_init(NULL, fp)));
  return(s7_make_c_pointer(sc, ncdirect_init((const char *)s7_string(termtype), fp)));
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


/* -------- notcurses_options* -------- */
#if 0
/*
  (load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))
  (define noptions (notcurses_options_make))
  (set! (notcurses_options_flags noptions) NCOPTION_SUPPRESS_BANNERS)
  (define nc (notcurses_init noptions))
  (notcurses_render nc)
  (define stats (ncstats_make))
  (notcurses_stats nc stats)
  (define bytes (ncstats_render_bytes stats))
  (notcurses_stop nc)
  (format *stderr* "bytes: ~S~%" bytes)
  (exit)
*/
#endif

static s7_pointer g_notcurses_options_termtype(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->termtype));
}

static s7_pointer g_notcurses_options_renderfp(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->renderfp));
}

static s7_pointer g_notcurses_options_loglevel(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->loglevel));
}

static s7_pointer g_notcurses_options_margin_t(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->margin_t));
}

static s7_pointer g_notcurses_options_margin_r(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->margin_r));
}

static s7_pointer g_notcurses_options_margin_b(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->margin_b));
}

static s7_pointer g_notcurses_options_margin_l(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->margin_l));
}

static s7_pointer g_notcurses_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((notcurses_options *)s7_c_pointer(s7_car(args)))->flags));
}


static s7_pointer g_set_notcurses_options_margin_t(s7_scheme *sc, s7_pointer args)
{
  notcurses_options *no;
  no = (notcurses_options *)s7_c_pointer(s7_car(args));
  no->margin_t = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_notcurses_options_margin_r(s7_scheme *sc, s7_pointer args)
{
  notcurses_options *no;
  no = (notcurses_options *)s7_c_pointer(s7_car(args));
  no->margin_r = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_notcurses_options_margin_b(s7_scheme *sc, s7_pointer args)
{
  notcurses_options *no;
  no = (notcurses_options *)s7_c_pointer(s7_car(args));
  no->margin_b = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_notcurses_options_margin_l(s7_scheme *sc, s7_pointer args)
{
  notcurses_options *no;
  no = (notcurses_options *)s7_c_pointer(s7_car(args));
  no->margin_l = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_notcurses_options_flags(s7_scheme *sc, s7_pointer args)
{
  notcurses_options *no;
  no = (notcurses_options *)s7_c_pointer(s7_car(args));
  no->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_notcurses_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(notcurses_options)), s7_make_symbol(sc, "notcurses_options*"), s7_f(sc)));
}

static s7_pointer g_notcurses_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "notcurses_options*"), __func__, 0));
  return(s7_f(sc));
}


/* -------- ncinput* -------- */
#if 0
/*
  (load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))
  (define noptions (notcurses_options_make))
  (set! (notcurses_options_flags noptions) NCOPTION_SUPPRESS_BANNERS)
  (define nc (notcurses_init noptions))
  (notcurses_cursor_enable nc)
  (define ncp (ncplane_new nc 20 20 0 0 (c-pointer 0)))
  (ncplane_putstr_yx ncp 0 0 "> ")
  (notcurses_render nc)
  (let ((ni (ncinput_make)))
    (do ((c (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni)
  	    (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni))
         (i 2 (+ i 1)))
        ((and (= c (integer->char #\Q)) ; C-Q to exit
  	      (ncinput_ctrl ni)))
      (if (< c 256) (ncplane_putstr_yx ncp 0 i (string c)))
      (notcurses_render nc)))
  (notcurses_stop nc)
  (exit)
*/
#endif

#if 0
typedef struct ncinput {
  char32_t id; 
  int y;       
  int x;       
  bool alt;    
  bool shift;  
  bool ctrl;   
  uint64_t seqnum; 
} ncinput;
#endif

static s7_pointer g_ncinput_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncinput)), s7_make_symbol(sc, "ncinput*"), s7_f(sc)));
}

static s7_pointer g_ncinput_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncinput_id(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->id));
}

static s7_pointer g_ncinput_y(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->y));
}

static s7_pointer g_ncinput_x(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->x));
}

static s7_pointer g_ncinput_alt(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_boolean(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->alt));
}

static s7_pointer g_ncinput_shift(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_boolean(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->shift));
}

static s7_pointer g_ncinput_ctrl(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_boolean(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->ctrl));
}

static s7_pointer g_ncinput_seqnum(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncinput *)s7_c_pointer(s7_car(args)))->seqnum));
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

static s7_pointer g_notcurses_stdplane_const(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)notcurses_stdplane_const((const struct notcurses *)s7_c_pointer(s7_car(args))), s7_make_symbol(sc, "ncplane*"), s7_f(sc)));
}

static s7_pointer g_notcurses_drop_planes(s7_scheme *sc, s7_pointer args)
{
  notcurses_drop_planes((struct notcurses *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

#if 0
typedef struct palette256 {uint32_t chans[NCPALETTESIZE];} palette256;
#endif

static s7_pointer g_palette256_new(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, palette256_new((struct notcurses *)s7_c_pointer(s7_car(args))), s7_make_symbol(sc, "palette256*"), s7_f(sc)));
}

static s7_pointer g_palette256_free(s7_scheme *sc, s7_pointer args)
{
  palette256_free((palette256 *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_palette256_use(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, palette256_use((struct notcurses *)s7_c_pointer(s7_car(args)), (const palette256 *)s7_c_pointer(s7_cadr(args)))));
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
 
static s7_pointer g_notcurses_getc(s7_scheme *sc, s7_pointer args) /* TODO: need local timespec sigset_t ncinput make */
{
  return(s7_make_integer(sc, notcurses_getc((struct notcurses *)s7_c_pointer(s7_car(args)),
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

static s7_pointer g_ncstats_renders(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->renders));
}

static s7_pointer g_ncstats_failed_renders(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->failed_renders));
}

static s7_pointer g_ncstats_render_bytes(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_bytes));
}

static s7_pointer g_ncstats_render_max_bytes(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_max_bytes));
}

static s7_pointer g_ncstats_render_min_bytes(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_min_bytes));
}

static s7_pointer g_ncstats_render_ns(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_ns));
}

static s7_pointer g_ncstats_render_max_ns(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_max_ns));
}

static s7_pointer g_ncstats_render_min_ns(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->render_min_ns));
}

static s7_pointer g_ncstats_cellelisions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->cellelisions));
}

static s7_pointer g_ncstats_cellemissions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->cellemissions));
}

static s7_pointer g_ncstats_fgelisions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->fgelisions));
}

static s7_pointer g_ncstats_fgemissions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->fgemissions));
}

static s7_pointer g_ncstats_bgelisions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->bgelisions));
}

static s7_pointer g_ncstats_bgemissions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->bgemissions));
}

static s7_pointer g_ncstats_defaultelisions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->defaultelisions));
}

static s7_pointer g_ncstats_defaultemissions(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->defaultemissions));
}

static s7_pointer g_ncstats_fbbytes(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->fbbytes));
}

static s7_pointer g_ncstats_planes(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncstats *)s7_c_pointer(s7_car(args)))->planes));
}


static s7_pointer g_ncstats_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (ncstats *)calloc(1, sizeof(ncstats)), s7_make_symbol(sc, "ncstats*"), s7_f(sc)));
}

static s7_pointer g_ncstats_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncstats*"), __func__, 0));
  return(s7_f(sc));
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

static s7_pointer g_cell_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, (void *)calloc(1, sizeof(cell))));
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

#if 0
/*
(let ((br 0)
      (c1 (cell_make))
      (c2 (cell_make))
      (c3 (cell_make))
      (c4 (cell_make))
      (c5 (cell_make))
      (c6 (cell_make)))
  (cells_rounded_box ncp br 0 c1 c2 c3 c4 c5 c6)
  (ncplane_cursor_move_yx ncp 0 0)
  (ncplane_box ncp c1 c2 c3 c4 c5 c6 20 20 0))
*/
#endif

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
#if 0
static s7_pointer g_ncplane_putnstr_aligned(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncplane_putnstr_aligned((struct ncplane *)s7_c_pointer(s7_car(args)), 
						     (int)s7_integer(s7_cadr(args)), (ncalign_e)s7_integer(s7_caddr(args)),
						     (size_t)s7_integer(s7_cadddr(args)), (const char *)s7_string(s7_car(s7_cdddr(args))))));
}
#endif
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

static s7_pointer g_ncplane_greyscale(s7_scheme *sc, s7_pointer args)
{
  ncplane_greyscale((struct ncplane *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}



/* -------- cell -------- */
#if 0
typedef struct cell {
  uint32_t gcluster;
  uint32_t attrword;
  uint64_t channels;
} cell;
#endif 

static s7_pointer g_cell_gcluster(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((cell *)s7_c_pointer(s7_car(args)))->gcluster));
}

static s7_pointer g_cell_attrword(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((cell *)s7_c_pointer(s7_car(args)))->attrword));
}

static s7_pointer g_cell_channels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((cell *)s7_c_pointer(s7_car(args)))->channels));
}


static s7_pointer g_set_cell_gcluster(s7_scheme *sc, s7_pointer args) 
{
  ((cell *)s7_c_pointer(s7_car(args)))->gcluster = (uint32_t)s7_integer(s7_cadr(args)); return(s7_cadr(args));
}

static s7_pointer g_set_cell_attrword(s7_scheme *sc, s7_pointer args) 
{
  ((cell *)s7_c_pointer(s7_car(args)))->attrword = (uint32_t)s7_integer(s7_cadr(args)); return(s7_cadr(args));
}

static s7_pointer g_set_cell_channels(s7_scheme *sc, s7_pointer args) 
{
  ((cell *)s7_c_pointer(s7_car(args)))->channels = (uint64_t)s7_integer(s7_cadr(args)); return(s7_cadr(args));
}


static s7_pointer g_cells_double_box(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg;
  cell *ul, *ur, *ll, *lr, *hl, *vl;
  uint32_t attr;
  uint64_t channels;

  arg = s7_cdr(args);
  attr = (uint32_t)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  channels = (uint64_t)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ul = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ur = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ll = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  lr = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  hl = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  vl = (cell *)s7_c_pointer(s7_car(arg)); 
  return(s7_make_integer(sc, cells_double_box((struct ncplane *)s7_c_pointer(s7_car(args)), attr, channels, ul, ur, ll, lr, hl, vl)));
}

static s7_pointer g_cells_rounded_box(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg;
  cell *ul, *ur, *ll, *lr, *hl, *vl;
  uint32_t attr;
  uint64_t channels;

  arg = s7_cdr(args);
  attr = (uint32_t)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  channels = (uint64_t)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  ul = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ur = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  ll = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  lr = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  hl = (cell *)s7_c_pointer(s7_car(arg)); arg = s7_cdr(arg);
  vl = (cell *)s7_c_pointer(s7_car(arg)); 
  return(s7_make_integer(sc, cells_rounded_box((struct ncplane *)s7_c_pointer(s7_car(args)), attr, channels, ul, ur, ll, lr, hl, vl)));
}


/* -------- ncselector_options -------- */
#if 0
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
#endif

static s7_pointer g_ncselector_item_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct ncselector_item)), s7_make_symbol(sc, "ncselector_item*"), s7_f(sc)));
}

static s7_pointer g_ncselector_item_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncselector_item*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncselector_item_option(s7_scheme *sc, s7_pointer args) {return(s7_make_string(sc, ((struct ncselector_item *)s7_c_pointer(s7_car(args)))->option));}
static s7_pointer g_ncselector_item_desc(s7_scheme *sc, s7_pointer args) {return(s7_make_string(sc, ((struct ncselector_item *)s7_c_pointer(s7_car(args)))->desc));}

static s7_pointer g_set_ncselector_item_option(s7_scheme *sc, s7_pointer args)
{
  struct ncselector_item *no;
  s7_pointer str;
  no = (struct ncselector_item *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->option = NULL;
  else
    {
      no->option = (char *)malloc(s7_string_length(str));
      strcpy(no->option, s7_string(str));
    }
  return(str);
}

static s7_pointer g_set_ncselector_item_desc(s7_scheme *sc, s7_pointer args)
{
  struct ncselector_item *no;
  s7_pointer str;
  no = (struct ncselector_item *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->desc = NULL;
  else
    {
      no->desc = (char *)malloc(s7_string_length(str));
      strcpy(no->desc, s7_string(str));
    }
  return(str);
}


static s7_pointer g_ncselector_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncselector_options)), s7_make_symbol(sc, "ncselector_options*"), s7_f(sc)));
}

static s7_pointer g_ncselector_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncselector_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncselector_options_title(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->title));
}

static s7_pointer g_ncselector_options_secondary(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->secondary));
}

static s7_pointer g_ncselector_options_footer(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->footer));
}

static s7_pointer g_ncselector_options_items(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->items));
}

static s7_pointer g_ncselector_options_itemcount(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->itemcount));
}

static s7_pointer g_ncselector_options_defidx(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->defidx));
}

static s7_pointer g_ncselector_options_maxdisplay(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->maxdisplay));
}

static s7_pointer g_ncselector_options_opchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->opchannels));
}

static s7_pointer g_ncselector_options_descchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->descchannels));
}

static s7_pointer g_ncselector_options_titlechannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->titlechannels));
}

static s7_pointer g_ncselector_options_footchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->footchannels));
}

static s7_pointer g_ncselector_options_boxchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->boxchannels));
}

static s7_pointer g_ncselector_options_bgchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->bgchannels));
}

static s7_pointer g_ncselector_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncselector_options *)s7_c_pointer(s7_car(args)))->flags));
}


static s7_pointer g_set_ncselector_options_title(s7_scheme *sc, s7_pointer args)
{
  ncselector_options *no;
  s7_pointer str;
  no = (ncselector_options *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->title = NULL;
  else
    {
      no->title = (char *)calloc(1, s7_string_length(str) + 1);
      strcpy(no->title, s7_string(str));
    }
  return(str);
}

static s7_pointer g_set_ncselector_options_secondary(s7_scheme *sc, s7_pointer args)
{
  ncselector_options *no;
  s7_pointer str;
  no = (ncselector_options *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->secondary = NULL;
  else
    {
      no->secondary = (char *)calloc(1, s7_string_length(str) + 1);
      strcpy(no->secondary, s7_string(str));
    }
  return(s7_cadr(args));
}

static s7_pointer g_set_ncselector_options_footer(s7_scheme *sc, s7_pointer args)
{
  ncselector_options *no;
  s7_pointer str;
  no = (ncselector_options *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->footer = NULL;
  else
    {
      no->footer = (char *)calloc(1, s7_string_length(str) + 1);
      strcpy(no->footer, s7_string(str));
    }
  return(s7_cadr(args));
}

static s7_pointer g_set_ncselector_options_items(s7_scheme *sc, s7_pointer args)
{
  ncselector_options *no;
  no = (ncselector_options *)s7_c_pointer(s7_car(args));
  no->items = (struct ncselector_item *)s7_c_pointer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncselector_options_itemcount(s7_scheme *sc, s7_pointer args)
{
  ncselector_options *no;
  no = (ncselector_options *)s7_c_pointer(s7_car(args));
  no->itemcount = (unsigned int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncselector_options_maxdisplay(s7_scheme *sc, s7_pointer args)
{
  ncselector_options *no;
  no = (ncselector_options *)s7_c_pointer(s7_car(args));
  no->maxdisplay = (unsigned int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}


/* -------- ncselector -------- */

static s7_pointer g_ncselector_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, (void *)ncselector_create((struct ncplane *)s7_c_pointer(s7_car(args)),
							 s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), 
							 (const ncselector_options *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer g_ncselector_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, (void *)ncselector_plane((struct ncselector *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncselector_previtem(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, ncselector_previtem((struct ncselector *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncselector_nextitem(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, ncselector_nextitem((struct ncselector *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncselector_selected(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, ncselector_selected((const struct ncselector *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncselector_additem(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncselector_additem((struct ncselector *)s7_c_pointer(s7_car(args)),
						(const struct ncselector_item *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncselector_delitem(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncselector_delitem((struct ncselector *)s7_c_pointer(s7_car(args)), (const char *)s7_string(s7_cadr(args)))));
}

static s7_pointer g_ncselector_offer_input(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, ncselector_offer_input((struct ncselector *)s7_c_pointer(s7_car(args)),
						    (const struct ncinput *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncselector_destroy(s7_scheme *sc, s7_pointer args)
{
  ncselector_destroy((struct ncselector *)s7_c_pointer(s7_car(args)), (char **)s7_c_pointer(s7_cadr(args)));
  return(s7_f(sc));
}


/* -------- ncmultiselector_options -------- */
#if 0
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

struct ncmselector_item {
  char* option;
  char* desc;
  bool selected;
};
#endif

static s7_pointer g_ncmselector_item_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct ncmselector_item)), s7_make_symbol(sc, "ncmselector_item*"), s7_f(sc)));
}

static s7_pointer g_ncmselector_item_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncmselector_item*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncmselector_item_option(s7_scheme *sc, s7_pointer args) {return(s7_make_string(sc, ((struct ncmselector_item *)s7_c_pointer(s7_car(args)))->option));}
static s7_pointer g_ncmselector_item_desc(s7_scheme *sc, s7_pointer args) {return(s7_make_string(sc, ((struct ncmselector_item *)s7_c_pointer(s7_car(args)))->desc));}

static s7_pointer g_set_ncmselector_item_option(s7_scheme *sc, s7_pointer args)
{
  struct ncmselector_item *no;
  s7_pointer str;
  no = (struct ncmselector_item *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->option = NULL;
  else
    {
      no->option = (char *)malloc(s7_string_length(str));
      strcpy(no->option, s7_string(str));
    }
  return(str);
}

static s7_pointer g_set_ncmselector_item_desc(s7_scheme *sc, s7_pointer args)
{
  struct ncmselector_item *no;
  s7_pointer str;
  no = (struct ncmselector_item *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->desc = NULL;
  else
    {
      no->desc = (char *)malloc(s7_string_length(str));
      strcpy(no->desc, s7_string(str));
    }
  return(str);
}


static s7_pointer g_ncmultiselector_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncmultiselector_options)), s7_make_symbol(sc, "ncmultiselector_options*"), s7_f(sc)));
}

static s7_pointer g_ncmultiselector_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncmultiselector_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncmultiselector_options_title(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->title));
}

static s7_pointer g_ncmultiselector_options_secondary(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->secondary));
}

static s7_pointer g_ncmultiselector_options_footer(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->footer));
}

static s7_pointer g_ncmultiselector_options_items(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->items));
}

static s7_pointer g_ncmultiselector_options_itemcount(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->itemcount));
}

static s7_pointer g_ncmultiselector_options_maxdisplay(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->maxdisplay));
}

static s7_pointer g_ncmultiselector_options_opchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->opchannels));
}

static s7_pointer g_ncmultiselector_options_descchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->descchannels));
}

static s7_pointer g_ncmultiselector_options_titlechannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->titlechannels));
}

static s7_pointer g_ncmultiselector_options_footchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->footchannels));
}

static s7_pointer g_ncmultiselector_options_boxchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->boxchannels));
}

static s7_pointer g_ncmultiselector_options_bgchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->bgchannels));
}

static s7_pointer g_ncmultiselector_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmultiselector_options *)s7_c_pointer(s7_car(args)))->flags));
}


static s7_pointer g_set_ncmultiselector_options_title(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_options *no;
  s7_pointer str;
  no = (ncmultiselector_options *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->title = NULL;
  else
    {
      no->title = (char *)calloc(1, s7_string_length(str) + 1);
      strcpy(no->title, s7_string(str));
    }
  return(str);
}

static s7_pointer g_set_ncmultiselector_options_secondary(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_options *no;
  s7_pointer str;
  no = (ncmultiselector_options *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->secondary = NULL;
  else
    {
      no->secondary = (char *)calloc(1, s7_string_length(str) + 1);
      strcpy(no->secondary, s7_string(str));
    }
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmultiselector_options_footer(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_options *no;
  s7_pointer str;
  no = (ncmultiselector_options *)s7_c_pointer(s7_car(args));
  str = s7_cadr(args);
  if (s7_string_length(str) == 0)
    no->footer = NULL;
  else
    {
      no->footer = (char *)calloc(1, s7_string_length(str) + 1);
      strcpy(no->footer, s7_string(str));
    }
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmultiselector_options_items(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_options *no;
  no = (ncmultiselector_options *)s7_c_pointer(s7_car(args));
  no->items = (struct ncmselector_item *)s7_c_pointer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmultiselector_options_itemcount(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_options *no;
  no = (ncmultiselector_options *)s7_c_pointer(s7_car(args));
  no->itemcount = (unsigned int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmultiselector_options_maxdisplay(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_options *no;
  no = (ncmultiselector_options *)s7_c_pointer(s7_car(args));
  no->maxdisplay = (unsigned int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}


/* -------- ncmultiselector -------- */

static s7_pointer g_ncmultiselector_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, (void *)ncmultiselector_create((struct ncplane *)s7_c_pointer(s7_car(args)),
							      s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), 
							      (const ncmultiselector_options *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer g_ncmultiselector_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, (void *)ncmultiselector_plane((struct ncmultiselector *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmultiselector_selected(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmultiselector_selected((struct ncmultiselector *)s7_c_pointer(s7_car(args)),
						      (bool *)s7_c_pointer(s7_cadr(args)),
						      (unsigned)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncmultiselector_offer_input(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, ncmultiselector_offer_input((struct ncmultiselector *)s7_c_pointer(s7_car(args)),
							 (const struct ncinput *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncmultiselector_destroy(s7_scheme *sc, s7_pointer args)
{
  ncmultiselector_destroy((struct ncmultiselector *)s7_c_pointer(s7_car(args)), (char **)s7_c_pointer(s7_cadr(args)));
  return(s7_f(sc));
}



/* -------- ncmenu_item -------- */
#if 0
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
#endif

/* -------- ncmenu_options -------- */
#if 0
typedef struct ncmenu_options {
  struct ncmenu_section* sections;
  int sectioncount;
  uint64_t headerchannels;
  uint64_t sectionchannels;
  uint64_t flags;
} ncmenu_options;
#endif

static s7_pointer g_ncmenu_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncmenu_options)), s7_make_symbol(sc, "ncmenu_options*"), s7_f(sc)));
}

static s7_pointer g_ncmenu_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncmenu_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncmenu_options_sections(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer(sc, ((ncmenu_options *)s7_c_pointer(s7_car(args)))->sections));
}

static s7_pointer g_ncmenu_options_sectioncount(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmenu_options *)s7_c_pointer(s7_car(args)))->sectioncount));
}

static s7_pointer g_ncmenu_options_headerchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmenu_options *)s7_c_pointer(s7_car(args)))->headerchannels));
}

static s7_pointer g_ncmenu_options_sectionchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmenu_options *)s7_c_pointer(s7_car(args)))->sectionchannels));
}

static s7_pointer g_ncmenu_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncmenu_options *)s7_c_pointer(s7_car(args)))->flags));
}

static s7_pointer g_set_ncmenu_options_sections(s7_scheme *sc, s7_pointer args) 
{
  ((ncmenu_options *)s7_c_pointer(s7_car(args)))->sections = (struct ncmenu_section *)s7_c_pointer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmenu_options_sectioncount(s7_scheme *sc, s7_pointer args) 
{
  ((ncmenu_options *)s7_c_pointer(s7_car(args)))->sectioncount = (int)s7_integer(s7_cadr(args)); 
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmenu_options_headerchannels(s7_scheme *sc, s7_pointer args) 
{
  ((ncmenu_options *)s7_c_pointer(s7_car(args)))->headerchannels = (uint64_t)s7_integer(s7_cadr(args)); 
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmenu_options_sectionchannels(s7_scheme *sc, s7_pointer args) 
{
  ((ncmenu_options *)s7_c_pointer(s7_car(args)))->sectionchannels = (uint64_t)s7_integer(s7_cadr(args)); 
  return(s7_cadr(args));
}

static s7_pointer g_set_ncmenu_options_flags(s7_scheme *sc, s7_pointer args) 
{
  ((ncmenu_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args)); 
  return(s7_cadr(args));
}


/* -------- ncmenu -------- */

static s7_pointer g_ncmenu_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncmenu_create((struct ncplane *)s7_c_pointer(s7_car(args)), (const ncmenu_options *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncmenu_unroll(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_unroll((struct ncmenu *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)))));
}

static s7_pointer g_ncmenu_rollup(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_rollup((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_nextsection(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_nextsection((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_prevsection(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_prevsection((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_nextitem(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_nextitem((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_previtem(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_previtem((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncmenu_plane((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_destroy(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncmenu_destroy((struct ncmenu *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncmenu_selected(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, ncmenu_selected((const struct ncmenu *)s7_c_pointer(s7_car(args)), (struct ncinput *)s7_cadr(args))));
}
 
static s7_pointer g_ncmenu_offer_input(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, ncmenu_offer_input((struct ncmenu *)s7_c_pointer(s7_car(args)), (const struct ncinput *)s7_cadr(args))));
}
 

/* -------- ncplot_options -------- */
#if 0
typedef struct ncplot_options {
  uint64_t maxchannel;
  uint64_t minchannel;
  ncblitter_e gridtype;
  int rangex;
  uint64_t flags;
} ncplot_options;
#endif

static s7_pointer g_ncplot_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncplot_options)), s7_make_symbol(sc, "ncplot_options*"), s7_f(sc)));
}

static s7_pointer g_ncplot_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncplot_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncplot_options_maxchannel(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncplot_options *)s7_c_pointer(s7_car(args)))->maxchannel));
}

static s7_pointer g_ncplot_options_minchannel(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncplot_options *)s7_c_pointer(s7_car(args)))->minchannel));
}

static s7_pointer g_ncplot_options_gridtype(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncplot_options *)s7_c_pointer(s7_car(args)))->gridtype));
}

static s7_pointer g_ncplot_options_rangex(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncplot_options *)s7_c_pointer(s7_car(args)))->rangex));
}

static s7_pointer g_ncplot_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((ncplot_options *)s7_c_pointer(s7_car(args)))->flags));
}

static s7_pointer g_set_ncplot_options_maxchannel(s7_scheme *sc, s7_pointer args) 
{
  ((ncplot_options *)s7_c_pointer(s7_car(args)))->maxchannel = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncplot_options_minchannel(s7_scheme *sc, s7_pointer args) 
{
  ((ncplot_options *)s7_c_pointer(s7_car(args)))->minchannel = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncplot_options_gridtype(s7_scheme *sc, s7_pointer args) 
{
  ((ncplot_options *)s7_c_pointer(s7_car(args)))->gridtype = (ncblitter_e)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncplot_options_rangex(s7_scheme *sc, s7_pointer args) 
{
  ((ncplot_options *)s7_c_pointer(s7_car(args)))->rangex = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncplot_options_flags(s7_scheme *sc, s7_pointer args) 
{
  ((ncplot_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}


/* -------- ncplot -------- */

static s7_pointer g_ncuplot_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncuplot_create((struct ncplane *)s7_c_pointer(s7_car(args)),
					      (const ncplot_options *)s7_c_pointer(s7_cadr(args)),
					      (uint64_t)s7_integer(s7_caddr(args)),
					      (uint64_t)s7_integer(s7_cadddr(args)))));
}

static s7_pointer g_ncdplot_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncdplot_create((struct ncplane *)s7_c_pointer(s7_car(args)),
					      (const ncplot_options *)s7_c_pointer(s7_cadr(args)),
					      (double)s7_real(s7_caddr(args)),
					      (double)s7_real(s7_cadddr(args)))));
}

static s7_pointer g_ncuplot_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncuplot_plane((struct ncuplot *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncdplot_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncdplot_plane((struct ncdplot *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncuplot_destroy(s7_scheme *sc, s7_pointer args)
{
  ncuplot_destroy((struct ncuplot *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncdplot_destroy(s7_scheme *sc, s7_pointer args)
{
  ncdplot_destroy((struct ncdplot *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncuplot_add_sample(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncuplot_add_sample((struct ncuplot *)s7_c_pointer(s7_car(args)), 
						(uint64_t)s7_integer(s7_cadr(args)), 
						(uint64_t)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncdplot_add_sample(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdplot_add_sample((struct ncdplot *)s7_c_pointer(s7_car(args)), 
						(uint64_t)s7_integer(s7_cadr(args)), 
						(double)s7_real(s7_caddr(args)))));
}

static s7_pointer g_ncuplot_set_sample(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncuplot_set_sample((struct ncuplot *)s7_c_pointer(s7_car(args)), 
						(uint64_t)s7_integer(s7_cadr(args)), 
						(uint64_t)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncdplot_set_sample(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncdplot_set_sample((struct ncdplot *)s7_c_pointer(s7_car(args)), 
						(uint64_t)s7_integer(s7_cadr(args)), 
						(double)s7_real(s7_caddr(args)))));
}

static s7_pointer g_ncuplot_sample(s7_scheme *sc, s7_pointer args)
{
  int res;
  uint64_t y;
  res = ncuplot_sample((struct ncuplot *)s7_c_pointer(s7_car(args)), (uint64_t)s7_integer(s7_cadr(args)), &y);
  return(s7_list(sc, 2, s7_make_integer(sc, res), s7_make_integer(sc, y)));
}

static s7_pointer g_ncdplot_sample(s7_scheme *sc, s7_pointer args)
{
  int res;
  double y;
  res = ncdplot_sample((struct ncdplot *)s7_c_pointer(s7_car(args)), (uint64_t)s7_integer(s7_cadr(args)), &y);
  return(s7_list(sc, 2, s7_make_integer(sc, res), s7_make_real(sc, y)));
}


/* -------- ncreel_options -------- */
#if 0
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
#endif

static s7_pointer g_ncreel_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncreel_options)), s7_make_symbol(sc, "ncreel_options*"), s7_f(sc)));
}

static s7_pointer g_ncreel_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncreel_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncreel_options_min_supported_cols(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->min_supported_cols));
}

static s7_pointer g_ncreel_options_min_supported_rows(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->min_supported_rows));
}

static s7_pointer g_ncreel_options_max_supported_cols(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->max_supported_cols));
}

static s7_pointer g_ncreel_options_max_supported_rows(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->max_supported_rows));
}

static s7_pointer g_ncreel_options_toff(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->toff));
}

static s7_pointer g_ncreel_options_roff(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->roff));
}

static s7_pointer g_ncreel_options_boff(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->boff));
}

static s7_pointer g_ncreel_options_loff(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->loff));
}

static s7_pointer g_ncreel_options_bordermask(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->bordermask));
}

static s7_pointer g_ncreel_options_borderchan(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->borderchan));
}

static s7_pointer g_ncreel_options_tabletmask(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->tabletmask));
}

static s7_pointer g_ncreel_options_tabletchan(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->tabletchan));
}

static s7_pointer g_ncreel_options_focusedchan(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->focusedchan));
}

static s7_pointer g_ncreel_options_bgchannel(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->bgchannel));
}

static s7_pointer g_ncreel_options_flags(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->flags));
}


static s7_pointer g_set_ncreel_options_min_supported_cols(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->min_supported_cols = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_min_supported_rows(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->min_supported_rows = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_max_supported_cols(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->max_supported_cols = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_max_supported_rows(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->max_supported_rows = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_toff(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->toff = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_roff(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->roff = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_boff(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->boff = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_loff(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->loff = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_bordermask(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->bordermask = (unsigned)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_borderchan(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->borderchan = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_tabletmask(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->tabletmask = (unsigned)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_tabletchan(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->tabletchan = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_focusedchan(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->focusedchan = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_bgchannel(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->bgchannel = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreel_options_flags(s7_scheme *sc, s7_pointer args)
{
  ((struct ncreel_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}



/* -------- ncreel -------- */

static s7_pointer g_ncreel_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreel_create((struct ncplane *)s7_c_pointer(s7_car(args)), 
					     (const ncreel_options *)s7_c_pointer(s7_cadr(args)),
					     (int)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncreel_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreel_plane((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_tabletcount(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_tabletcount((const struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_del_focused(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_del_focused((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_redraw(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_redraw((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_focused(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreel_focused((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_next(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreel_next((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_prev(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreel_prev((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_destroy(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_destroy((struct ncreel *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreel_move(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_move((struct ncreel *)s7_c_pointer(s7_car(args)), (int)s7_integer(s7_cadr(args)), (int)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncreel_touch(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_touch((struct ncreel *)s7_c_pointer(s7_car(args)), (struct nctablet *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncreel_del(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreel_del((struct ncreel *)s7_c_pointer(s7_car(args)), (struct nctablet *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_nctablet_userptr(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, nctablet_userptr((struct nctablet *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_nctablet_ncplane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, nctablet_ncplane((struct nctablet *)s7_c_pointer(s7_car(args)))));
}

#if 0
/* typedef int (*tabletcb)(struct nctablet* t, int begx, int begy, int maxx, int maxy, bool cliptop);
 * struct nctablet* ncreel_add(struct ncreel* pr, struct nctablet* after, struct nctablet* before, tabletcb cb, void* opaque);
 */
#endif


/* -------- ncreader_options -------- */
#if 0
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
#endif

static s7_pointer g_ncreader_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(ncreader_options)), s7_make_symbol(sc, "ncreader_options*"), s7_f(sc)));
}

static s7_pointer g_ncreader_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncreader_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncreader_options_tchannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->tchannels));
}

static s7_pointer g_ncreader_options_echannels(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->echannels));
}

static s7_pointer g_ncreader_options_tattrword(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->tattrword));
}

static s7_pointer g_ncreader_options_eattrword(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->eattrword));
}

static s7_pointer g_ncreader_options_physrows(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->physrows));
}

static s7_pointer g_ncreader_options_physcols(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->physcols));
}

static s7_pointer g_ncreader_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->flags));
}

static s7_pointer g_ncreader_options_egc(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_string(sc, ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->egc));
}

static s7_pointer g_set_ncreader_options_tchannels(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->tchannels = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_echannels(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->echannels = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_tattrword(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->tattrword = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_eattrword(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->eattrword = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_physrows(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->physrows = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_physcols(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->physcols = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_flags(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncreader_options_egc(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncreader_options *)s7_c_pointer(s7_car(args)))->egc = (char *)s7_string(s7_cadr(args)); /* TODO: copy */
  return(s7_cadr(args));
}


/* -------- ncreader -------- */

static s7_pointer g_ncreader_create(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreader_create((struct ncplane *)s7_c_pointer(s7_car(args)),
					       (int)s7_integer(s7_cadr(args)),
					       (int)s7_integer(s7_caddr(args)),
					       (const ncreader_options *)s7_c_pointer(s7_cadddr(args)))));

}

static s7_pointer g_ncreader_clear(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncreader_clear((struct ncreader *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreader_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncreader_plane((struct ncreader *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreader_contents(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, ncreader_contents((const struct ncreader *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncreader_offer_input(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, ncreader_offer_input((struct ncreader *)s7_c_pointer(s7_car(args)),
						  (const struct ncinput *)s7_c_pointer(s7_cadr(args)))));
}

static s7_pointer g_ncreader_destroy(s7_scheme *sc, s7_pointer args)
{
  ncreader_destroy((struct ncreader *)s7_c_pointer(s7_car(args)), (char **)s7_c_pointer(s7_cadr(args)));
  return(s7_f(sc));
}


/* -------- ncvisual_options -------- */
#if 0
struct ncvisual_options {
  struct ncplane* n;
  ncscale_e scaling;
  int y, x;
  int begy, begx;
  int leny, lenx;
  ncblitter_e blitter;
  uint64_t flags;
};
#endif

static s7_pointer g_ncvisual_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct ncvisual_options)), s7_make_symbol(sc, "ncvisual_options*"), s7_f(sc)));
}

static s7_pointer g_ncvisual_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncvisual_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncvisual_options_n(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_c_pointer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->n));
}

static s7_pointer g_ncvisual_options_scaling(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->scaling));
}

static s7_pointer g_ncvisual_options_y(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->y));
}

static s7_pointer g_ncvisual_options_x(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->x));
}

static s7_pointer g_ncvisual_options_begy(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->begy));
}

static s7_pointer g_ncvisual_options_begx(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->begx));
}

static s7_pointer g_ncvisual_options_leny(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->leny));
}

static s7_pointer g_ncvisual_options_lenx(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->lenx));
}

static s7_pointer g_ncvisual_options_blitter(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->blitter));
}

static s7_pointer g_ncvisual_options_flags(s7_scheme *sc, s7_pointer args) 
{
  return(s7_make_integer(sc, ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->flags));
}


static s7_pointer g_set_ncvisual_options_n(s7_scheme *sc, s7_pointer args)  /* ?? */
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->n = (struct ncplane *)s7_c_pointer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_scaling(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->scaling = (ncscale_e)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_y(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->y = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_x(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->x = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_begy(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->begy = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_begx(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->begx = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_leny(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->leny = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_lenx(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->lenx = (int)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_blitter(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->blitter = (ncblitter_e)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncvisual_options_flags(s7_scheme *sc, s7_pointer args) 
{
  ((struct ncvisual_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}


/* -------- ncvisual -------- */

static s7_pointer g_ncvisual_from_file(s7_scheme *sc, s7_pointer args)
{
  nc_err_e err;
  s7_pointer res;
  res = s7_make_c_pointer_with_type(sc, ncvisual_from_file((const char *)s7_string(s7_car(args)), &err), s7_make_symbol(sc, "ncvisual*"), s7_f(sc));
  return(s7_list(sc, 2, s7_make_integer(sc, err), res));
}

static s7_pointer g_ncvisual_from_rgba(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, ncvisual_from_rgba((const void *)s7_c_pointer(s7_car(args)),  /* TODO: rgba* */
							    (int)s7_integer(s7_cadr(args)),
							    (int)s7_integer(s7_caddr(args)),
							    (int)s7_integer(s7_cadddr(args))),
				     s7_make_symbol(sc, "ncvisual*"), s7_f(sc)));
}

static s7_pointer g_ncvisual_from_bgra(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, ncvisual_from_bgra((const void *)s7_c_pointer(s7_car(args)),  /* TODO: bgra* */
							    (int)s7_integer(s7_cadr(args)),
							    (int)s7_integer(s7_caddr(args)),
							    (int)s7_integer(s7_cadddr(args))),
				     s7_make_symbol(sc, "ncvisual*"), s7_f(sc)));
}

static s7_pointer g_ncvisual_from_plane(s7_scheme *sc, s7_pointer args)
{
  int begy, begx, leny, lenx;
  s7_pointer arg;
  arg = s7_cddr(args);
  begy = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  begx = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  leny = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  lenx = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  return(s7_make_c_pointer_with_type(sc, ncvisual_from_plane((const struct ncplane *)s7_c_pointer(s7_car(args)),
							     (ncblitter_e)s7_integer(s7_cadr(args)),
							     begy, begx, leny, lenx),
				     s7_make_symbol(sc, "ncvisual*"), s7_f(sc)));
}

static s7_pointer g_ncvisual_destroy(s7_scheme *sc, s7_pointer args)
{
  ncvisual_destroy((struct ncvisual *)s7_c_pointer(s7_car(args)));
  return(s7_f(sc));
}

static s7_pointer g_ncvisual_decode(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncvisual_decode((struct ncvisual *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncvisual_subtitle(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, ncvisual_subtitle((const struct ncvisual *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncvisual_rotate(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncvisual_rotate((struct ncvisual *)s7_c_pointer(s7_car(args)), (double)s7_real(s7_cadr(args)))));
}

static s7_pointer g_ncvisual_resize(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncvisual_resize((struct ncvisual *)s7_c_pointer(s7_car(args)), 
					     (int)s7_integer(s7_cadr(args)),
					     (int)s7_integer(s7_caddr(args)))));
}

static s7_pointer g_ncvisual_polyfill_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncvisual_polyfill_yx((struct ncvisual *)s7_c_pointer(s7_car(args)), 
						  (int)s7_integer(s7_cadr(args)),
						  (int)s7_integer(s7_caddr(args)),
						  (uint32_t)s7_integer(s7_cadddr(args)))));
}

static s7_pointer g_ncvisual_set_yx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncvisual_set_yx((const struct ncvisual *)s7_c_pointer(s7_car(args)), 
					     (int)s7_integer(s7_cadr(args)),
					     (int)s7_integer(s7_caddr(args)),
					     (uint32_t)s7_integer(s7_cadddr(args)))));
}

static s7_pointer g_ncvisual_at_yx(s7_scheme *sc, s7_pointer args)
{
  uint32_t pix;
  s7_pointer res;
  res = s7_make_integer(sc, ncvisual_at_yx((const struct ncvisual *)s7_c_pointer(s7_car(args)), 
					   (int)s7_integer(s7_cadr(args)),
					   (int)s7_integer(s7_caddr(args)),
					   &pix));
  return(s7_list(sc, 2, res, s7_make_integer(sc, pix)));
}

static s7_pointer g_ncvisual_render(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncvisual_render((struct notcurses *)s7_c_pointer(s7_car(args)),
					       (struct ncvisual *)s7_c_pointer(s7_cadr(args)),
					       (const struct ncvisual_options *)s7_c_pointer(s7_caddr(args)))));
}

static s7_pointer g_ncvisual_simple_streamer(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncvisual_simple_streamer((struct ncvisual *)s7_c_pointer(s7_car(args)),
						      (struct ncvisual_options *)s7_c_pointer(s7_cadr(args)),
						      (const struct timespec *)s7_c_pointer(s7_caddr(args)),
						      (void *)s7_c_pointer(s7_cadddr(args)))));
}

static s7_pointer g_ncblit_rgba(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncblit_rgba((const void *)s7_c_pointer(s7_car(args)),
					 (int)s7_integer(s7_cadr(args)),
					 (const struct ncvisual_options *)s7_c_pointer(s7_caddr(args)))));
}

static s7_pointer g_ncblit_bgrx(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncblit_bgrx((const void *)s7_c_pointer(s7_car(args)),
					 (int)s7_integer(s7_cadr(args)),
					 (const struct ncvisual_options *)s7_c_pointer(s7_caddr(args)))));
}

static s7_pointer g_ncplane_rgba(s7_scheme *sc, s7_pointer args)
{
  int begy, begx, leny, lenx;
  s7_pointer arg;
  arg = s7_cddr(args);
  begy = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  begx = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  leny = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  lenx = (int)s7_integer(s7_car(arg)); arg = s7_cdr(arg);
  return(s7_make_c_pointer(sc, ncplane_rgba((const struct ncplane *)s7_c_pointer(s7_car(args)), /* TODO: returns uint32_t* */
					    (ncblitter_e)s7_integer(s7_cadr(args)),
					    begy, begx, leny, lenx)));
}

static s7_pointer g_ncvisual_geom(s7_scheme *sc, s7_pointer args)
{
  int y, x, toy, tox;
  s7_pointer res;
  res = s7_make_integer(sc, ncvisual_geom((const struct notcurses *)s7_c_pointer(s7_car(args)),
					  (const struct ncvisual *)s7_c_pointer(s7_cadr(args)),
					  (const struct ncvisual_options *)s7_c_pointer(s7_caddr(args)),
					  &y, &x, &toy, &tox));
  return(s7_list(sc, 5, res, s7_make_integer(sc, y), s7_make_integer(sc, x), s7_make_integer(sc, toy), s7_make_integer(sc, tox)));
}

/* typedef int (*streamcb)(struct ncvisual*, struct ncvisual_options*, const struct timespec*, void*);
 * int ncvisual_stream(struct notcurses* nc, struct ncvisual* ncv, nc_err_e* ncerr, float timescale, streamcb streamer, const struct ncvisual_options* vopts, void* curry);
 */


/* -------- ncfdplane/ncsubproc -------- */
#if 0
typedef struct ncfdplane_options {
  void* curry;
  bool follow;
  uint64_t flags;
} ncfdplane_options;

typedef struct ncsubproc_options {
  void* curry;
  uint64_t restart_period;
  uint64_t flags;
} ncsubproc_options;
#endif

static s7_pointer g_ncfdplane_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct ncfdplane_options)), s7_make_symbol(sc, "ncfdplane_options*"), s7_f(sc)));
}

static s7_pointer g_ncfdplane_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncfdplane_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncfdplane_options_curry(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ((struct ncfdplane_options *)s7_c_pointer(s7_car(args)))->curry));
}

static s7_pointer g_ncfdplane_options_follow(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, ((struct ncfdplane_options *)s7_c_pointer(s7_car(args)))->follow));
}

static s7_pointer g_ncfdplane_options_flags(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncfdplane_options *)s7_c_pointer(s7_car(args)))->flags));
}

static s7_pointer g_set_ncfdplane_options_curry(s7_scheme *sc, s7_pointer args)
{
  ((struct ncfdplane_options *)s7_c_pointer(s7_car(args)))->curry = (void *)s7_c_pointer(s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncfdplane_options_follow(s7_scheme *sc, s7_pointer args)
{
  ((struct ncfdplane_options *)s7_c_pointer(s7_car(args)))->follow = (bool)s7_boolean(sc, s7_cadr(args));
  return(s7_cadr(args));
}

static s7_pointer g_set_ncfdplane_options_flags(s7_scheme *sc, s7_pointer args)
{
  ((struct ncfdplane_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}


static s7_pointer g_ncsubproc_options_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct ncsubproc_options)), s7_make_symbol(sc, "ncsubproc_options*"), s7_f(sc)));
}

static s7_pointer g_ncsubproc_options_free(s7_scheme *sc, s7_pointer args)
{
  free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "ncsubproc_options*"), __func__, 0));
  return(s7_f(sc));
}

static s7_pointer g_ncsubproc_options_curry(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ((struct ncsubproc_options *)s7_c_pointer(s7_car(args)))->curry));
}
 
static s7_pointer g_ncsubproc_options_restart_period(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncsubproc_options *)s7_c_pointer(s7_car(args)))->restart_period));
}
 
static s7_pointer g_ncsubproc_options_flags(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ((struct ncsubproc_options *)s7_c_pointer(s7_car(args)))->flags));
}

static s7_pointer g_set_ncsubproc_options_curry(s7_scheme *sc, s7_pointer args)
{
  ((struct ncsubproc_options *)s7_c_pointer(s7_car(args)))->curry = (void *)s7_c_pointer(s7_cadr(args));
  return(s7_cadr(args));
}
 
static s7_pointer g_set_ncsubproc_options_restart_period(s7_scheme *sc, s7_pointer args)
{
  ((struct ncsubproc_options *)s7_c_pointer(s7_car(args)))->restart_period = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}
 
static s7_pointer g_set_ncsubproc_options_flags(s7_scheme *sc, s7_pointer args)
{
  ((struct ncsubproc_options *)s7_c_pointer(s7_car(args)))->flags = (uint64_t)s7_integer(s7_cadr(args));
  return(s7_cadr(args));
}


static s7_pointer g_ncfdplane_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncfdplane_plane((struct ncfdplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncfdplane_destroy(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncfdplane_destroy((struct ncfdplane *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncsubproc_plane(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer(sc, ncsubproc_plane((struct ncsubproc *)s7_c_pointer(s7_car(args)))));
}

static s7_pointer g_ncsubproc_destroy(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, ncsubproc_destroy((struct ncsubproc *)s7_c_pointer(s7_car(args)))));
}


#if 0
int ncplane_qrcode(struct ncplane* n, ncblitter_e blitter, int* ymax, int* xmax, const void* data, size_t len);

#define NCMETRICFWIDTH(x, cols) ((int)(strlen(x) - mbswidth(x) + (cols)))
#define PREFIXFMT(x) NCMETRICFWIDTH((x), PREFIXCOLUMNS), (x)
#define IPREFIXFMT(x) NCMETRIXFWIDTH((x), IPREFIXCOLUMNS), (x)
#define BPREFIXFMT(x) NCMETRICFWIDTH((x), BPREFIXCOLUMNS), (x)

const char* ncmetric(uintmax_t val, uintmax_t decimal, char* buf, int omitdec, uintmax_t mult, int uprefix);
#endif

/* typedef int(*ncfdplane_callback)(struct ncfdplane* n, const void* buf, size_t s, void* curry);
 * typedef int(*ncfdplane_done_cb)(struct ncfdplane* n, int fderrno, void* curry);
 * struct ncfdplane* ncfdplane_create(struct ncplane* n, const ncfdplane_options* opts, int fd, ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
 * struct ncsubproc* ncsubproc_createv(struct ncplane* n, const ncsubproc_options* opts, const char* bin,  char* const arg[], ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
 * struct ncsubproc* ncsubproc_createvp(struct ncplane* n, const ncsubproc_options* opts, const char* bin,  char* const arg[], ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
 * struct ncsubproc* ncsubproc_createvpe(struct ncplane* n, const ncsubproc_options* opts, const char* bin,  char* const arg[], char* const env[], ncfdplane_callback cbfxn, ncfdplane_done_cb donecbfxn);
 */




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

  nc_int(NCERR_SUCCESS);
  nc_int(NCERR_NOMEM);
  nc_int(NCERR_EOF);
  nc_int(NCERR_DECODE);
  nc_int(NCERR_SYSTEM);
  nc_int(NCERR_INVALID_ARG);
  nc_int(NCERR_UNIMPLEMENTED);

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

  /* notcurses/nckeys.h */
  nc_int(NCKEY_INVALID);
  nc_int(NCKEY_RESIZE);
  nc_int(NCKEY_UP);
  nc_int(NCKEY_RIGHT);
  nc_int(NCKEY_DOWN);
  nc_int(NCKEY_LEFT);
  nc_int(NCKEY_INS);
  nc_int(NCKEY_DEL);
  nc_int(NCKEY_BACKSPACE);
  nc_int(NCKEY_PGDOWN);
  nc_int(NCKEY_PGUP);
  nc_int(NCKEY_HOME);
  nc_int(NCKEY_END);
  nc_int(NCKEY_F00);
  nc_int(NCKEY_F01);
  nc_int(NCKEY_F02);
  nc_int(NCKEY_F03);
  nc_int(NCKEY_F04);
  nc_int(NCKEY_F05);
  nc_int(NCKEY_F06);
  nc_int(NCKEY_F07);
  nc_int(NCKEY_F08);
  nc_int(NCKEY_F09);
  nc_int(NCKEY_F10);
  nc_int(NCKEY_ENTER);
  nc_int(NCKEY_CLS);
  nc_int(NCKEY_DLEFT);
  nc_int(NCKEY_DRIGHT);
  nc_int(NCKEY_ULEFT);
  nc_int(NCKEY_URIGHT);
  nc_int(NCKEY_CENTER);
  nc_int(NCKEY_BEGIN);
  nc_int(NCKEY_CANCEL);
  nc_int(NCKEY_CLOSE);
  nc_int(NCKEY_COMMAND);
  nc_int(NCKEY_COPY);
  nc_int(NCKEY_EXIT);
  nc_int(NCKEY_PRINT);
  nc_int(NCKEY_REFRESH);
  nc_int(NCKEY_BUTTON1);
  nc_int(NCKEY_BUTTON2);
  nc_int(NCKEY_BUTTON3);
  nc_int(NCKEY_BUTTON4);
  nc_int(NCKEY_BUTTON5);
  nc_int(NCKEY_BUTTON6);
  nc_int(NCKEY_BUTTON7);
  nc_int(NCKEY_BUTTON8);
  nc_int(NCKEY_BUTTON9);
  nc_int(NCKEY_BUTTON10);
  nc_int(NCKEY_BUTTON11);
  nc_int(NCKEY_RELEASE);
  nc_int(NCKEY_SCROLL_UP);
  nc_int(NCKEY_SCROLL_DOWN);
  nc_int(NCKEY_RETURN);

  #define nc_func(Name, Req, Opt, Rst) s7_define_function(sc, #Name, g_ ## Name, Req, Opt, Rst, NULL)
    
  nc_func(ncdirect_init, 1, 1, false);
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

  nc_func(notcurses_options_termtype, 1, 0, false);
  nc_func(notcurses_options_renderfp, 1, 0, false);
  nc_func(notcurses_options_loglevel, 1, 0, false);

  #define nc_func2(Name) s7_dilambda(sc, #Name, g_ ## Name, 1, 0, g_set_ ## Name, 2, 0, NULL)
  
  nc_func2(notcurses_options_margin_t);
  nc_func2(notcurses_options_margin_r);
  nc_func2(notcurses_options_margin_b);
  nc_func2(notcurses_options_margin_l);
  nc_func2(notcurses_options_flags);

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
  nc_func(notcurses_stdplane_const, 1, 0, false);
  nc_func(notcurses_cursor_enable, 1, 0, false);
  nc_func(notcurses_cursor_disable, 1, 0, false);
  nc_func(notcurses_getc, 4, 0, false);
  nc_func(notcurses_refresh, 1, 0, false);
  nc_func(notcurses_at_yx, 5, 0, false);
  nc_func(notcurses_lex_margins, 2, 0, false);
  nc_func(notcurses_lex_scalemode, 2, 0, false);

  nc_func(palette256_new, 1, 0, false);
  nc_func(palette256_free, 1, 0, false);
  nc_func(palette256_use, 2, 0, false);

  nc_func(ncinput_id, 1, 0, false);
  nc_func(ncinput_y, 1, 0, false);
  nc_func(ncinput_x, 1, 0, false);
  nc_func(ncinput_alt, 1, 0, false);
  nc_func(ncinput_shift, 1, 0, false);
  nc_func(ncinput_ctrl, 1, 0, false);
  nc_func(ncinput_seqnum, 1, 0, false);
  nc_func(ncinput_make, 0, 0, false);
  nc_func(ncinput_free, 1, 0, false);

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
  /* nc_func(ncplane_putnstr_aligned, 5, 0, false); */
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
  nc_func(ncplane_greyscale, 1, 0, false);

  nc_func(cell_make, 0, 0, false);
  nc_func(cell_load, 3, 0, false);
  nc_func(cell_duplicate, 3, 0, false);
  nc_func(cell_release, 2, 0, false);
  nc_func(cell_extended_gcluster, 2, 0, false);
  nc_func(cells_double_box, 9, 0, false);
  nc_func(cells_rounded_box, 9, 0, false);
  nc_func2(cell_gcluster);
  nc_func2(cell_attrword);
  nc_func2(cell_channels);

  nc_func(ncfadectx_setup, 1, 0, false);
  nc_func(ncfadectx_free, 1, 0, false);
  nc_func(ncfadectx_iterations, 1, 0, false);

  nc_func(ncselector_item_make, 0, 0, false);
  nc_func(ncselector_item_free, 1, 0, false);
  nc_func2(ncselector_item_option);
  nc_func2(ncselector_item_desc);

  nc_func(ncselector_options_make, 0, 0, false);
  nc_func(ncselector_options_free, 1, 0, false);
  nc_func2(ncselector_options_title);
  nc_func2(ncselector_options_secondary);
  nc_func2(ncselector_options_footer);
  nc_func2(ncselector_options_items);
  nc_func2(ncselector_options_itemcount);
  nc_func(ncselector_options_defidx, 1, 0, false);
  nc_func2(ncselector_options_maxdisplay);
  nc_func(ncselector_options_opchannels, 1, 0, false);
  nc_func(ncselector_options_descchannels, 1, 0, false);
  nc_func(ncselector_options_titlechannels, 1, 0, false);
  nc_func(ncselector_options_footchannels, 1, 0, false);
  nc_func(ncselector_options_boxchannels, 1, 0, false);
  nc_func(ncselector_options_bgchannels, 1, 0, false);
  nc_func(ncselector_options_flags, 1, 0, false);

  nc_func(ncselector_create, 4, 0, false);
  nc_func(ncselector_offer_input, 2, 0, false);
  nc_func(ncselector_destroy, 2, 0, false);
  nc_func(ncselector_additem, 2, 0, false);
  nc_func(ncselector_delitem, 2, 0, false);
  nc_func(ncselector_plane, 1, 0, false);
  nc_func(ncselector_previtem, 1, 0, false);
  nc_func(ncselector_nextitem, 1, 0, false);
  nc_func(ncselector_selected, 1, 0, false);

  nc_func(ncmselector_item_make, 0, 0, false);
  nc_func(ncmselector_item_free, 1, 0, false);
  nc_func2(ncmselector_item_option);
  nc_func2(ncmselector_item_desc);

  nc_func(ncmultiselector_options_make, 0, 0, false);
  nc_func(ncmultiselector_options_free, 1, 0, false);
  nc_func2(ncmultiselector_options_title);
  nc_func2(ncmultiselector_options_secondary);
  nc_func2(ncmultiselector_options_footer);
  nc_func2(ncmultiselector_options_items);
  nc_func2(ncmultiselector_options_itemcount);
  nc_func2(ncmultiselector_options_maxdisplay);
  nc_func(ncmultiselector_options_opchannels, 1, 0, false);
  nc_func(ncmultiselector_options_descchannels, 1, 0, false);
  nc_func(ncmultiselector_options_titlechannels, 1, 0, false);
  nc_func(ncmultiselector_options_footchannels, 1, 0, false);
  nc_func(ncmultiselector_options_boxchannels, 1, 0, false);
  nc_func(ncmultiselector_options_bgchannels, 1, 0, false);
  nc_func(ncmultiselector_options_flags, 1, 0, false);

  nc_func(ncmultiselector_create, 4, 0, false);
  nc_func(ncmultiselector_offer_input, 2, 0, false);
  nc_func(ncmultiselector_destroy, 2, 0, false);
  nc_func(ncmultiselector_plane, 1, 0, false);
  nc_func(ncmultiselector_selected, 1, 0, false);

  nc_func(ncmenu_options_make, 1, 0, false);
  nc_func(ncmenu_options_free, 1, 0, false);
  nc_func2(ncmenu_options_sections);
  nc_func2(ncmenu_options_sectioncount);
  nc_func2(ncmenu_options_headerchannels);
  nc_func2(ncmenu_options_sectionchannels);
  nc_func2(ncmenu_options_flags);

  nc_func(ncmenu_create, 2, 0, false);
  nc_func(ncmenu_unroll, 2, 0, false);
  nc_func(ncmenu_rollup, 1, 0, false);
  nc_func(ncmenu_nextsection, 1, 0, false);
  nc_func(ncmenu_prevsection, 1, 0, false);
  nc_func(ncmenu_nextitem, 1, 0, false);
  nc_func(ncmenu_previtem, 1, 0, false);
  nc_func(ncmenu_plane, 1, 0, false);
  nc_func(ncmenu_destroy, 1, 0, false);
  nc_func(ncmenu_selected, 2, 0, false);
  nc_func(ncmenu_offer_input, 2, 0, false);

  nc_func(ncplot_options_make, 1, 0, false);
  nc_func(ncplot_options_free, 1, 0, false);
  nc_func2(ncplot_options_maxchannel);
  nc_func2(ncplot_options_minchannel);
  nc_func2(ncplot_options_gridtype);
  nc_func2(ncplot_options_rangex);
  nc_func2(ncplot_options_flags);

  nc_func(ncuplot_create, 4, 0, false);
  nc_func(ncdplot_create, 4, 0, false);
  nc_func(ncuplot_plane, 1, 0, false);
  nc_func(ncdplot_plane, 1, 0, false);
  nc_func(ncuplot_destroy, 1, 0, false);
  nc_func(ncdplot_destroy, 1, 0, false);
  nc_func(ncuplot_add_sample, 3, 0, false);
  nc_func(ncdplot_add_sample, 3, 0, false);
  nc_func(ncuplot_set_sample, 3, 0, false);
  nc_func(ncdplot_set_sample, 3, 0, false);
  nc_func(ncuplot_sample, 2, 0, false);
  nc_func(ncdplot_sample, 2, 0, false);

  nc_func(ncreader_options_make, 1, 0, false);
  nc_func(ncreader_options_free, 1, 0, false);
  nc_func2(ncreader_options_tchannels);
  nc_func2(ncreader_options_echannels);
  nc_func2(ncreader_options_tattrword);
  nc_func2(ncreader_options_eattrword);
  nc_func2(ncreader_options_egc);
  nc_func2(ncreader_options_physrows);
  nc_func2(ncreader_options_physcols);
  nc_func2(ncreader_options_flags);

  nc_func(ncreader_create, 4, 0, false);
  nc_func(ncreader_clear, 1, 0, false);
  nc_func(ncreader_plane, 1, 0, false);
  nc_func(ncreader_contents, 1, 0, false);
  nc_func(ncreader_offer_input, 2, 0, false);
  nc_func(ncreader_destroy, 2, 0, false);

  nc_func(ncreel_options_make, 1, 0, false);
  nc_func(ncreel_options_free, 1, 0, false);
  nc_func2(ncreel_options_min_supported_cols);
  nc_func2(ncreel_options_min_supported_rows);
  nc_func2(ncreel_options_max_supported_cols);
  nc_func2(ncreel_options_max_supported_rows);
  nc_func2(ncreel_options_toff);
  nc_func2(ncreel_options_roff);
  nc_func2(ncreel_options_boff);
  nc_func2(ncreel_options_loff);
  nc_func2(ncreel_options_bordermask);
  nc_func2(ncreel_options_borderchan);
  nc_func2(ncreel_options_tabletmask);
  nc_func2(ncreel_options_tabletchan);
  nc_func2(ncreel_options_focusedchan);
  nc_func2(ncreel_options_bgchannel);
  nc_func2(ncreel_options_flags);

  nc_func(ncreel_create, 3, 0, false);
  nc_func(ncreel_plane, 1, 0, false);
  nc_func(ncreel_tabletcount, 1, 0, false);
  nc_func(ncreel_del_focused, 1, 0, false);
  nc_func(ncreel_focused, 1, 0, false);
  nc_func(ncreel_redraw, 1, 0, false);
  nc_func(ncreel_next, 1, 0, false);
  nc_func(ncreel_prev, 1, 0, false);
  nc_func(ncreel_destroy, 1, 0, false);
  nc_func(ncreel_move, 3, 0, false);
  nc_func(ncreel_touch, 2, 0, false);
  nc_func(ncreel_del, 2, 0, false);
  nc_func(nctablet_userptr, 1, 0, false);
  nc_func(nctablet_ncplane, 1, 0, false);

  nc_func(ncvisual_options_make, 1, 0, false);
  nc_func(ncvisual_options_free, 1, 0, false);
  nc_func2(ncvisual_options_n);
  nc_func2(ncvisual_options_scaling);
  nc_func2(ncvisual_options_y);
  nc_func2(ncvisual_options_x);
  nc_func2(ncvisual_options_begy);
  nc_func2(ncvisual_options_begx);
  nc_func2(ncvisual_options_leny);
  nc_func2(ncvisual_options_lenx);
  nc_func2(ncvisual_options_blitter);
  nc_func2(ncvisual_options_flags);

  nc_func(ncvisual_from_file, 1, 0, false);
  nc_func(ncvisual_from_rgba, 4, 0, false);
  nc_func(ncvisual_from_bgra, 4, 0, false);
  nc_func(ncvisual_from_plane, 6, 0, false);
  nc_func(ncvisual_destroy, 1, 0, false);
  nc_func(ncvisual_decode, 1, 0, false);
  nc_func(ncvisual_subtitle, 1, 0, false);
  nc_func(ncvisual_rotate, 2, 0, false);
  nc_func(ncvisual_resize, 3, 0, false);
  nc_func(ncvisual_polyfill_yx, 4, 0, false);
  nc_func(ncvisual_set_yx, 4, 0, false);
  nc_func(ncvisual_at_yx, 3, 0, false);
  nc_func(ncvisual_render, 3, 0, false);
  nc_func(ncvisual_simple_streamer, 4, 0, false);
  nc_func(ncvisual_geom, 7, 0, false);

  nc_func(ncplane_rgba, 6, 0, false);
  nc_func(ncblit_rgba, 3, 0, false);
  nc_func(ncblit_bgrx, 3, 0, false);

  nc_func2(ncfdplane_options_curry);
  nc_func2(ncfdplane_options_follow);
  nc_func2(ncfdplane_options_flags);
  nc_func2(ncsubproc_options_curry);
  nc_func2(ncsubproc_options_restart_period);
  nc_func2(ncsubproc_options_flags);

  nc_func(ncfdplane_options_make, 1, 0, false);
  nc_func(ncfdplane_options_free, 1, 0, false);
  nc_func(ncsubproc_options_make, 1, 0, false);
  nc_func(ncsubproc_options_free, 1, 0, false);
  nc_func(ncfdplane_plane, 1, 0, false);
  nc_func(ncfdplane_destroy, 1, 0, false);
  nc_func(ncsubproc_plane, 1, 0, false);
  nc_func(ncsubproc_destroy, 1, 0, false);


  s7_set_curlet(sc, old_curlet);
  s7_set_shadow_rootlet(sc, old_shadow);
}

/* gcc -fPIC -c notcurses_s7.c
 * gcc notcurses_s7.o -shared -o notcurses_s7.so -lnotcurses
 * repl
 *   > (load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))
 *   > (notcurses_version)
 */

/* TODO: ncmenu_item(s) various callbacks palette256-chans? */
