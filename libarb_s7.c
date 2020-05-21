#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>

#include <arb.h>
#include <acb.h>
#include <acb_hypgeom.h>

#define WITH_GMP 1
#include "s7.h"

static void s7_number_to_acb(s7_scheme *sc, acb_t z, s7_pointer x, slong prec)
{
  if (s7_is_big_real(x))
    {
      arf_set_mpfr((arf_struct *)acb_realref(z), *s7_big_real(x));
      arb_zero(acb_imagref(z));
    }
  else
    {
      if (s7_is_big_integer(x))
	{
	  arf_set_mpz((arf_struct *)acb_realref(z), *s7_big_integer(x));
	  arb_zero(acb_imagref(z));
	}
      else
	{
	  if (s7_is_integer(x))
	    acb_set_si(z, s7_integer(x));
	  else
	    {
	      if (s7_is_big_ratio(x))
		{
		  mpfr_t mq;
		  mpfr_init2(mq, prec);
		  mpfr_set_q(mq, *s7_big_ratio(x), MPFR_RNDN);
		  arf_set_mpfr((arf_struct *)acb_realref(z), mq);
		  arb_zero(acb_imagref(z));
		  mpfr_clear(mq);
		}
	      else
		{
		  if (s7_is_real(x))
		    acb_set_d(z, s7_real(x));
		  else 
		    {
		      if (s7_is_big_complex(x))
			{
			  arf_set_mpfr((arf_struct *)acb_realref(z), mpc_realref(*s7_big_complex(x)));
			  arf_set_mpfr((arf_struct *)acb_imagref(z), mpc_imagref(*s7_big_complex(x)));
			}
		      else
			{
			  if (s7_is_complex(x))
			    acb_set_d_d(z, s7_real_part(x), s7_imag_part(x));
			}}}}}}
}

static s7_pointer acb_to_s7(s7_scheme *sc, acb_t w, slong prec)
{
  s7_pointer result;
  if (arb_is_zero(acb_imagref(w)))
    {
      mpfr_t mp;
      mpfr_init2(mp, prec);
      arf_get_mpfr(mp, (const arf_struct *)acb_realref(w), MPFR_RNDN);
      result = s7_make_big_real(sc, &mp);
      mpfr_clear(mp);
    }
  else
    {
      mpc_t mp;
      mpc_init2(mp, prec);
      arf_get_mpfr(mpc_realref(mp), (const arf_struct *)acb_realref(w), MPFR_RNDN);
      arf_get_mpfr(mpc_imagref(mp), (const arf_struct *)acb_imagref(w), MPFR_RNDN);
      result = s7_make_big_complex(sc, &mp);
      mpc_clear(mp);
    }
  return(result);
}

static acb_t nu, w, z;
/* these should be local to s7_scheme someday via a void* list or something */


/* -------------------------------- arb_bessel_* -------------------------------- */
static s7_pointer arb_b(s7_scheme *sc, s7_pointer args, const char *b_name, 
			void acb_func(acb_t res, const acb_t nu, const acb_t z, slong prec))
{
  s7_pointer n, x;
  slong prec;

  prec = s7_integer(s7_let_field_ref(sc, s7_make_symbol(sc, "bignum-precision")));

  n = s7_car(args);
  if (!s7_is_number(n))
    return(s7_wrong_type_arg_error(sc, b_name, 1, n, "number"));
  s7_number_to_acb(sc, nu, n, prec);

  x = s7_cadr(args);
  if (!s7_is_number(x))
    return(s7_wrong_type_arg_error(sc, b_name, 2, x, "number"));
  s7_number_to_acb(sc, z, x, prec);

  acb_func(w, nu, z, prec);
  return(acb_to_s7(sc, w, prec));
}
			
static s7_pointer arb_bessel_j(s7_scheme *sc, s7_pointer args) {return(arb_b(sc, args, "arb_bessel_j", acb_hypgeom_bessel_j));}
static s7_pointer arb_bessel_y(s7_scheme *sc, s7_pointer args) {return(arb_b(sc, args, "arb_bessel_y", acb_hypgeom_bessel_y));}
static s7_pointer arb_bessel_i(s7_scheme *sc, s7_pointer args) {return(arb_b(sc, args, "arb_bessel_i", acb_hypgeom_bessel_i));}
static s7_pointer arb_bessel_k(s7_scheme *sc, s7_pointer args) {return(arb_b(sc, args, "arb_bessel_k", acb_hypgeom_bessel_k));}


/* -------------------------------- libarb_s7_init -------------------------------- */
void libarb_s7_init(s7_scheme *sc);
void libarb_s7_init(s7_scheme *sc)
{
  s7_pointer old_shadow, arb, old_curlet;

  acb_init(nu);
  acb_init(w);
  acb_init(z);

  s7_define_constant(sc, "*arb*", arb = s7_inlet(sc, s7_nil(sc)));
  old_curlet = s7_set_curlet(sc, arb);
  old_shadow = s7_set_shadow_rootlet(sc, arb);

  s7_define_function(sc, "arb_bessel_j", arb_bessel_j, 2, 0, false, "(arb_bessel_j n x) returns Jn(x)");
  s7_define_function(sc, "arb_bessel_y", arb_bessel_y, 2, 0, false, "(arb_bessel_y n x) returns Yn(x)");
  s7_define_function(sc, "arb_bessel_i", arb_bessel_i, 2, 0, false, "(arb_bessel_i n x) returns In(x)");
  s7_define_function(sc, "arb_bessel_k", arb_bessel_k, 2, 0, false, "(arb_bessel_k n x) returns Kn(x)");

  s7_set_curlet(sc, old_curlet);
  s7_set_shadow_rootlet(sc, old_shadow);
}

/* gcc -fPIC -c libarb_s7.c
 *   gcc libarb_s7.o -shared -o libarb_s7.so -lflint -larb
 *   repl
 *   > (load "libarb_s7.so" (inlet 'init_func 'libarb_s7_init))
 *   > (arb_bessel_j 0 1.0)
 *   7.651976865579665514497175261026632209096E-1
 */


#if 0
/* arb_hypgeom.h */
void arb_hypgeom_pfq(arb_t res, arb_srcptr a, slong p, arb_srcptr b, slong q, const arb_t z, int regularized, slong prec);

void arb_hypgeom_0f1(arb_t res, const arb_t a, const arb_t z, int regularized, slong prec);
void arb_hypgeom_m(arb_t res, const arb_t a, const arb_t b, const arb_t z, int regularized, slong prec);
void arb_hypgeom_1f1(arb_t res, const arb_t a, const arb_t b, const arb_t z, int regularized, slong prec);
void arb_hypgeom_u(arb_t res, const arb_t a, const arb_t b, const arb_t z, slong prec);
void arb_hypgeom_2f1(arb_t res, const arb_t a, const arb_t b, const arb_t c, const arb_t z, int regularized, slong prec);

void arb_hypgeom_erf(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_erf_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_erfc(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_erfc_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_erfi(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_erfi_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_fresnel(arb_t res1, arb_t res2, const arb_t z, int normalized, slong prec);
void arb_hypgeom_fresnel_series(arb_poly_t s, arb_poly_t c, const arb_poly_t h, int normalized, slong len, slong prec);

void arb_hypgeom_ei(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_ei_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_si(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_si_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_ci(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_ci_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_shi(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_shi_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_chi(arb_t res, const arb_t z, slong prec);
void arb_hypgeom_chi_series(arb_poly_t g, const arb_poly_t h, slong len, slong prec);

void arb_hypgeom_li(arb_t res, const arb_t z, int offset, slong prec);
void arb_hypgeom_li_series(arb_poly_t g, const arb_poly_t h, int offset, slong len, slong prec);

void arb_hypgeom_bessel_jy(arb_t res1, arb_t res2, const arb_t nu, const arb_t z, slong prec);
void arb_hypgeom_bessel_i_scaled(arb_t res, const arb_t nu, const arb_t z, slong prec);
void arb_hypgeom_bessel_k_scaled(arb_t res, const arb_t nu, const arb_t z, slong prec);

void arb_hypgeom_airy(arb_t ai, arb_t aip, arb_t bi, arb_t bip, const arb_t z, slong prec);
void arb_hypgeom_airy_jet(arb_ptr ai, arb_ptr bi, const arb_t z, slong len, slong prec);
void arb_hypgeom_airy_series(arb_poly_t ai, arb_poly_t ai_prime, arb_poly_t bi, arb_poly_t bi_prime, const arb_poly_t z, slong len, slong prec);

void arb_hypgeom_airy_zero(arb_t ai, arb_t aip, arb_t bi, arb_t bip, const fmpz_t n, slong prec);

void arb_hypgeom_coulomb(arb_t F, arb_t G, const arb_t l, const arb_t eta, const arb_t z, slong prec);
void arb_hypgeom_coulomb_jet(arb_ptr F, arb_ptr G, const arb_t l, const arb_t eta, const arb_t z, slong len, slong prec);
void arb_hypgeom_coulomb_series(arb_poly_t F, arb_poly_t G, const arb_t l, const arb_t eta, const arb_poly_t z, slong len, slong prec);

void arb_hypgeom_expint(arb_t res, const arb_t s, const arb_t z, slong prec);

void arb_hypgeom_gamma_lower(arb_t res, const arb_t s, const arb_t z, int regularized, slong prec);
void arb_hypgeom_gamma_lower_series(arb_poly_t g, const arb_t s, const arb_poly_t h, int regularized, slong n, slong prec);

void arb_hypgeom_gamma_upper(arb_t res, const arb_t s, const arb_t z, int regularized, slong prec);
void arb_hypgeom_gamma_upper_series(arb_poly_t g, const arb_t s, const arb_poly_t h, int regularized, slong n, slong prec);

void arb_hypgeom_beta_lower(arb_t res, const arb_t a, const arb_t c, const arb_t z, int regularized, slong prec);
void arb_hypgeom_beta_lower_series(arb_poly_t res, const arb_t a, const arb_t b, const arb_poly_t z, int regularized, slong len, slong prec);

void arb_hypgeom_chebyshev_t(arb_t res, const arb_t nu, const arb_t z, slong prec);
void arb_hypgeom_chebyshev_u(arb_t res, const arb_t nu, const arb_t z, slong prec);
void arb_hypgeom_jacobi_p(arb_t res, const arb_t n, const arb_t a, const arb_t b, const arb_t z, slong prec);
void arb_hypgeom_gegenbauer_c(arb_t res, const arb_t n, const arb_t m, const arb_t z, slong prec);
void arb_hypgeom_laguerre_l(arb_t res, const arb_t n, const arb_t m, const arb_t z, slong prec);
void arb_hypgeom_hermite_h(arb_t res, const arb_t nu, const arb_t z, slong prec);
void arb_hypgeom_legendre_p(arb_t res, const arb_t n, const arb_t m, const arb_t z, int type, slong prec);
void arb_hypgeom_legendre_q(arb_t res, const arb_t n, const arb_t m, const arb_t z, int type, slong prec);

void arb_hypgeom_legendre_p_ui_deriv_bound(mag_t dp, mag_t dp2, ulong n, const arb_t x, const arb_t x2sub1);
void arb_hypgeom_legendre_p_ui_rec(arb_t res, arb_t res_prime, ulong n, const arb_t x, slong prec);
void arb_hypgeom_legendre_p_ui_asymp(arb_t res, arb_t res2, ulong n, const arb_t x, slong K, slong prec);
void arb_hypgeom_legendre_p_ui_one(arb_t res, arb_t res2, ulong n, const arb_t x, slong K, slong prec);
void arb_hypgeom_legendre_p_ui_zero(arb_t res, arb_t res2, ulong n, const arb_t x, slong K, slong prec);
void arb_hypgeom_legendre_p_ui(arb_t res, arb_t res_prime, ulong n, const arb_t x, slong prec);

void arb_hypgeom_legendre_p_ui_root(arb_t res, arb_t weight, ulong n, ulong k, slong prec);
void arb_hypgeom_central_bin_ui(arb_t res, ulong n, slong prec);
void arb_hypgeom_dilog(arb_t res, const arb_t z, slong prec);

/* 
acbca_lc.h	 arb_fmpz_poly.h 	      
acb_dft.h	 acb_mat.h	arb.h	    mag.h
acb_dirichlet.h  acb_modular.h	bool_mat.h  partitions.h
acb_elliptic.h	 acb_poly.h	arb_mat.h	 
acb.h		 arb_poly.h	       
*/

#endif

