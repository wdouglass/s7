#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>

#include <arb.h>
#include <acb.h>
#include <acb_hypgeom.h>
#include <acb_elliptic.h>

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


static acb_t nu, w, z, b;
/* these should be local to s7_scheme someday via a void* list or something */


/* -------------------------------- aci -------------------------------- */
static s7_pointer aci(s7_scheme *sc, s7_pointer args, const char *b_name, void acb_func(acb_t res, const acb_t z, slong prec))
{
  s7_pointer n;
  slong prec;

  prec = s7_integer(s7_let_field_ref(sc, s7_make_symbol(sc, "bignum-precision")));

  n = s7_car(args);
  if (!s7_is_number(n))
    return(s7_wrong_type_arg_error(sc, b_name, 1, n, "number"));
  s7_number_to_acb(sc, z, n, prec);

  acb_func(w, z, prec);
  return(acb_to_s7(sc, w, prec));
}

static s7_pointer a_erf(s7_scheme *sc, s7_pointer args)          {return(aci(sc, args, "acb_erf",          acb_hypgeom_erf));}
static s7_pointer a_erfc(s7_scheme *sc, s7_pointer args)         {return(aci(sc, args, "acb_erfc",         acb_hypgeom_erfc));}
static s7_pointer a_erfi(s7_scheme *sc, s7_pointer args)         {return(aci(sc, args, "acb_erfi",         acb_hypgeom_erfi));}
static s7_pointer a_ei(s7_scheme *sc, s7_pointer args)           {return(aci(sc, args, "acb_ei",           acb_hypgeom_ei));}
static s7_pointer a_si(s7_scheme *sc, s7_pointer args)           {return(aci(sc, args, "acb_si",           acb_hypgeom_si));}
static s7_pointer a_ci(s7_scheme *sc, s7_pointer args)           {return(aci(sc, args, "acb_ci",           acb_hypgeom_ci));}
static s7_pointer a_shi(s7_scheme *sc, s7_pointer args)          {return(aci(sc, args, "acb_shi",          acb_hypgeom_shi));}
static s7_pointer a_chi(s7_scheme *sc, s7_pointer args)          {return(aci(sc, args, "acb_chi",          acb_hypgeom_chi));}
static s7_pointer a_erf_1f1a(s7_scheme *sc, s7_pointer args)     {return(aci(sc, args, "acb_erf_1f1a",     acb_hypgeom_erf_1f1a));}
static s7_pointer a_erf_1f1b(s7_scheme *sc, s7_pointer args)     {return(aci(sc, args, "acb_erf_1f1b",     acb_hypgeom_erf_1f1b));}
static s7_pointer a_ei_asymp(s7_scheme *sc, s7_pointer args)     {return(aci(sc, args, "acb_ei_asymp",     acb_hypgeom_ei_asymp));}
static s7_pointer a_ei_2f2(s7_scheme *sc, s7_pointer args)       {return(aci(sc, args, "acb_ei_2f2",       acb_hypgeom_ei_2f2));}
static s7_pointer a_si_asymp(s7_scheme *sc, s7_pointer args)     {return(aci(sc, args, "acb_si_asymp",     acb_hypgeom_si_asymp));}
static s7_pointer a_si_1f2(s7_scheme *sc, s7_pointer args)       {return(aci(sc, args, "acb_si_1f2",       acb_hypgeom_si_1f2));}
static s7_pointer a_ci_asymp(s7_scheme *sc, s7_pointer args)     {return(aci(sc, args, "acb_ci_asymp",     acb_hypgeom_ci_asymp));}
static s7_pointer a_ci_2f3(s7_scheme *sc, s7_pointer args)       {return(aci(sc, args, "acb_ci_2f3",       acb_hypgeom_ci_2f3));}
static s7_pointer a_chi_asymp(s7_scheme *sc, s7_pointer args)    {return(aci(sc, args, "acb_chi_asymp",    acb_hypgeom_chi_asymp));}
static s7_pointer a_chi_2f3(s7_scheme *sc, s7_pointer args)      {return(aci(sc, args, "acb_chi_2f3",      acb_hypgeom_chi_2f3));}
static s7_pointer a_dilog_bernoulli(s7_scheme *sc, s7_pointer args) {return(aci(sc, args, "acb_dilog_bernoulli", acb_hypgeom_dilog_bernoulli));}
static s7_pointer a_dilog_zero_taylor(s7_scheme *sc, s7_pointer args) {return(aci(sc, args, "acb_dilog_zero_taylor", acb_hypgeom_dilog_zero_taylor));}
static s7_pointer a_dilog_zero(s7_scheme *sc, s7_pointer args)   {return(aci(sc, args, "acb_dilog_zero",   acb_hypgeom_dilog_zero));}
static s7_pointer a_dilog(s7_scheme *sc, s7_pointer args)        {return(aci(sc, args, "acb_dilog",        acb_hypgeom_dilog));}
static s7_pointer a_elliptic_k(s7_scheme *sc, s7_pointer args)   {return(aci(sc, args, "acb_elliptic_k",   acb_elliptic_k));}
static s7_pointer a_elliptic_e(s7_scheme *sc, s7_pointer args)   {return(aci(sc, args, "acb_elliptic_e",   acb_elliptic_e));}
static s7_pointer a_elliptic_rc1(s7_scheme *sc, s7_pointer args) {return(aci(sc, args, "acb_elliptic_rc1", acb_elliptic_rc1));}
static s7_pointer a_gamma(s7_scheme *sc, s7_pointer args)        {return(aci(sc, args, "acb_gamma",        acb_gamma));}
static s7_pointer a_rgamma(s7_scheme *sc, s7_pointer args)       {return(aci(sc, args, "acb_rgamma",       acb_rgamma));}
static s7_pointer a_lgamma(s7_scheme *sc, s7_pointer args)       {return(aci(sc, args, "acb_lgamma",       acb_lgamma));}
static s7_pointer a_log_sin_pi(s7_scheme *sc, s7_pointer args)   {return(aci(sc, args, "acb_log_sin_pi",   acb_log_sin_pi));}
static s7_pointer a_digamma(s7_scheme *sc, s7_pointer args)      {return(aci(sc, args, "acb_digamma",      acb_digamma));}
static s7_pointer a_zeta(s7_scheme *sc, s7_pointer args)         {return(aci(sc, args, "acb_zeta",         acb_zeta));}
static s7_pointer a_log_barnes_g(s7_scheme *sc, s7_pointer args) {return(aci(sc, args, "acb_log_barnes_g", acb_log_barnes_g));}
static s7_pointer a_barnes_g(s7_scheme *sc, s7_pointer args)     {return(aci(sc, args, "acb_barnes_g",     acb_barnes_g));}
static s7_pointer a_sinc(s7_scheme *sc, s7_pointer args)         {return(aci(sc, args, "acb_sinc",         acb_sinc));}
static s7_pointer a_sinc_pi(s7_scheme *sc, s7_pointer args)      {return(aci(sc, args, "acb_sinc_pi",      acb_sinc_pi));}
static s7_pointer a_agm1(s7_scheme *sc, s7_pointer args)         {return(aci(sc, args, "acb_agm1",         acb_agm1));}


/* -------------------------------- acci -------------------------------- */
static s7_pointer acci(s7_scheme *sc, s7_pointer args, const char *b_name, void acb_func(acb_t res, const acb_t nu, const acb_t z, slong prec))
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
			
static s7_pointer a_bessel_j(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_bessel_j", acb_hypgeom_bessel_j));}
static s7_pointer a_bessel_y(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_bessel_y", acb_hypgeom_bessel_y));}
static s7_pointer a_bessel_i(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_bessel_i", acb_hypgeom_bessel_i));}
static s7_pointer a_bessel_k(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_bessel_k", acb_hypgeom_bessel_k));}

static s7_pointer a_hermite_h(s7_scheme *sc, s7_pointer args)       {return(acci(sc, args, "acb_hermite_h",       acb_hypgeom_hermite_h));}
static s7_pointer a_chebyshev_t(s7_scheme *sc, s7_pointer args)     {return(acci(sc, args, "acb_chebyshev_t",     acb_hypgeom_chebyshev_t));}
static s7_pointer a_chebyshev_u(s7_scheme *sc, s7_pointer args)     {return(acci(sc, args, "acb_chebyshev_u",     acb_hypgeom_chebyshev_u));}
static s7_pointer a_bessel_j_0f1(s7_scheme *sc, s7_pointer args)    {return(acci(sc, args, "acb_bessel_j_0f1",    acb_hypgeom_bessel_j_0f1));}
static s7_pointer a_bessel_j_asymp(s7_scheme *sc, s7_pointer args)  {return(acci(sc, args, "acb_bessel_j_asymp",  acb_hypgeom_bessel_j_asymp));}
static s7_pointer a_bessel_i_scaled(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_bessel_i_scaled", acb_hypgeom_bessel_i_scaled));}
static s7_pointer a_bessel_k_scaled(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_bessel_k_scaled", acb_hypgeom_bessel_k_scaled));}
static s7_pointer a_expint(s7_scheme *sc, s7_pointer args)          {return(acci(sc, args, "acb_expint",          acb_hypgeom_expint));}
static s7_pointer a_elliptic_pi(s7_scheme *sc, s7_pointer args)     {return(acci(sc, args, "acb_elliptic_pi",     acb_elliptic_pi));}
static s7_pointer a_elliptic_p(s7_scheme *sc, s7_pointer args)      {return(acci(sc, args, "acb_elliptic_p",      acb_elliptic_p));}
static s7_pointer a_elliptic_zeta(s7_scheme *sc, s7_pointer args)   {return(acci(sc, args, "acb_elliptic_zeta",   acb_elliptic_zeta));}
static s7_pointer a_elliptic_sigma(s7_scheme *sc, s7_pointer args)  {return(acci(sc, args, "acb_elliptic_sigma",  acb_elliptic_sigma));}
static s7_pointer a_elliptic_inv_p(s7_scheme *sc, s7_pointer args)  {return(acci(sc, args, "acb_elliptic_inv_p",  acb_elliptic_inv_p));}
static s7_pointer a_hurwitz_zeta(s7_scheme *sc, s7_pointer args)    {return(acci(sc, args, "acb_hurwitz_zeta",    acb_hurwitz_zeta));}
static s7_pointer a_polygamma(s7_scheme *sc, s7_pointer args)       {return(acci(sc, args, "acb_polygamma",       acb_polygamma));}
static s7_pointer a_polylog(s7_scheme *sc, s7_pointer args)         {return(acci(sc, args, "acb_polylog",         acb_polylog));}
static s7_pointer a_agm(s7_scheme *sc, s7_pointer args)             {return(acci(sc, args, "acb_agm",             acb_agm));}
static s7_pointer a_dilog_continuation(s7_scheme *sc, s7_pointer args) {return(acci(sc, args, "acb_dilog_continuation", acb_hypgeom_dilog_continuation));}


/* -------------------------------- accii -------------------------------- */
static s7_pointer accii(s7_scheme *sc, s7_pointer args, const char *b_name, void acb_func(acb_t res, const acb_t nu, const acb_t z, int scaled, slong prec))
{
  s7_pointer n, x, scl;
  slong prec;
  s7_int scaled;

  prec = s7_integer(s7_let_field_ref(sc, s7_make_symbol(sc, "bignum-precision")));

  n = s7_car(args);
  if (!s7_is_number(n))
    return(s7_wrong_type_arg_error(sc, b_name, 1, n, "number"));
  s7_number_to_acb(sc, nu, n, prec);

  x = s7_cadr(args);
  if (!s7_is_number(x))
    return(s7_wrong_type_arg_error(sc, b_name, 2, x, "number"));
  s7_number_to_acb(sc, z, x, prec);

  scl = s7_caddr(args);
  if (!s7_is_integer(scl))
    return(s7_wrong_type_arg_error(sc, b_name, 3, scl, "integer"));
  scaled = s7_integer(scl);

  acb_func(w, nu, z, scaled, prec);
  return(acb_to_s7(sc, w, prec));
}

static s7_pointer a_bessel_i_0f1(s7_scheme *sc, s7_pointer args)       {return(accii(sc, args, "acb_bessel_i_0f1",       acb_hypgeom_bessel_i_0f1));}
static s7_pointer a_bessel_i_asymp(s7_scheme *sc, s7_pointer args)     {return(accii(sc, args, "acb_bessel_i_asymp",     acb_hypgeom_bessel_i_asymp));}
static s7_pointer a_bessel_k_0f1(s7_scheme *sc, s7_pointer args)       {return(accii(sc, args, "acb_bessel_k_0f1",       acb_hypgeom_bessel_k_0f1));}
static s7_pointer a_bessel_k_asymp(s7_scheme *sc, s7_pointer args)     {return(accii(sc, args, "acb_bessel_k_asymp",     acb_hypgeom_bessel_k_asymp));}
static s7_pointer a_hypgeom_0f1_asymp(s7_scheme *sc, s7_pointer args)  {return(accii(sc, args, "acb_hypgeom_0f1_asymp",  acb_hypgeom_0f1_asymp));}
static s7_pointer a_hypgeom_0f1_direct(s7_scheme *sc, s7_pointer args) {return(accii(sc, args, "acb_hypgeom_0f1_direct", acb_hypgeom_0f1_direct));}
static s7_pointer a_hypgeom_0f1(s7_scheme *sc, s7_pointer args)        {return(accii(sc, args, "acb_hypgeom_0f1",        acb_hypgeom_0f1));}
static s7_pointer a_gamma_lower(s7_scheme *sc, s7_pointer args)        {return(accii(sc, args, "acb_gamma_lower",        acb_hypgeom_gamma_lower));}
static s7_pointer a_gamma_upper_asymp(s7_scheme *sc, s7_pointer args)  {return(accii(sc, args, "acb_gamma_upper_asymp",  acb_hypgeom_gamma_upper_asymp));}
static s7_pointer a_gamma_upper_1f1a(s7_scheme *sc, s7_pointer args)   {return(accii(sc, args, "acb_gamma_upper_1f1a",   acb_hypgeom_gamma_upper_1f1a));}
static s7_pointer a_gamma_upper_1f1b(s7_scheme *sc, s7_pointer args)   {return(accii(sc, args, "acb_gamma_upper_1f1b",   acb_hypgeom_gamma_upper_1f1b));}
static s7_pointer a_gamma_upper(s7_scheme *sc, s7_pointer args)        {return(accii(sc, args, "acb_gamma_upper",        acb_hypgeom_gamma_upper));}
static s7_pointer a_elliptic_e_inc(s7_scheme *sc, s7_pointer args)     {return(accii(sc, args, "acb_elliptic_e_inc",     acb_elliptic_e_inc));}
static s7_pointer a_elliptic_f(s7_scheme *sc, s7_pointer args)         {return(accii(sc, args, "acb_elliptic_f",         acb_elliptic_f));}


/* -------------------------------- accci -------------------------------- */
static s7_pointer accci(s7_scheme *sc, s7_pointer args, const char *b_name, void acb_func(acb_t res, const acb_t nu, const acb_t z, const acb_t b, slong prec))
{
  s7_pointer n, x, b1;
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

  b1 = s7_caddr(args);
  if (!s7_is_number(b1))
    return(s7_wrong_type_arg_error(sc, b_name, 3, b1, "number"));
  s7_number_to_acb(sc, b, b1, prec);

  acb_func(w, nu, z, b, prec);
  return(acb_to_s7(sc, w, prec));
}

static s7_pointer a_hypgeom_u_1f1(s7_scheme *sc, s7_pointer args) {return(accci(sc, args, "acb_hypgeom_u_1f1", acb_hypgeom_u_1f1));}
static s7_pointer a_hypgeom_u(s7_scheme *sc, s7_pointer args)     {return(accci(sc, args, "acb_hypgeom_u",     acb_hypgeom_u));}
static s7_pointer a_gegenbauer_c(s7_scheme *sc, s7_pointer args)  {return(accci(sc, args, "acb_gegenbauer_c",  acb_hypgeom_gegenbauer_c));}
static s7_pointer a_laguerre_l(s7_scheme *sc, s7_pointer args)    {return(accci(sc, args, "acb_laguerre_l",    acb_hypgeom_laguerre_l));}


/* -------------------------------- acccii -------------------------------- */
static s7_pointer acccii(s7_scheme *sc, s7_pointer args, const char *b_name, void acb_func(acb_t res, const acb_t nu, const acb_t z, const acb_t b, int scaled, slong prec))
{
  s7_pointer n, x, scl, b1;
  slong prec;
  s7_int scaled;

  prec = s7_integer(s7_let_field_ref(sc, s7_make_symbol(sc, "bignum-precision")));

  n = s7_car(args);
  if (!s7_is_number(n))
    return(s7_wrong_type_arg_error(sc, b_name, 1, n, "number"));
  s7_number_to_acb(sc, nu, n, prec);

  x = s7_cadr(args);
  if (!s7_is_number(x))
    return(s7_wrong_type_arg_error(sc, b_name, 2, x, "number"));
  s7_number_to_acb(sc, z, x, prec);

  b1 = s7_caddr(args);
  if (!s7_is_number(b1))
    return(s7_wrong_type_arg_error(sc, b_name, 3, b1, "number"));
  s7_number_to_acb(sc, b, b1, prec);

  scl = s7_cadddr(args);
  if (!s7_is_integer(scl))
    return(s7_wrong_type_arg_error(sc, b_name, 4, scl, "integer"));
  scaled = s7_integer(scl);

  acb_func(w, nu, z, b, scaled, prec);
  return(acb_to_s7(sc, w, prec));
}

static s7_pointer a_hypgeom_m_asymp(s7_scheme *sc, s7_pointer args)    {return(acccii(sc, args, "acb_hypgeom_m_asymp",    acb_hypgeom_m_asymp));}
static s7_pointer a_hypgeom_m_1f1(s7_scheme *sc, s7_pointer args)      {return(acccii(sc, args, "acb_hypgeom_m_1f1",      acb_hypgeom_m_1f1));}
static s7_pointer a_hypgeom_m(s7_scheme *sc, s7_pointer args)          {return(acccii(sc, args, "acb_hypgeom_m",          acb_hypgeom_m));}
static s7_pointer a_hypgeom_1f1(s7_scheme *sc, s7_pointer args)        {return(acccii(sc, args, "acb_hypgeom_1f1",        acb_hypgeom_1f1));}
static s7_pointer a_hypgeom_beta_lower(s7_scheme *sc, s7_pointer args) {return(acccii(sc, args, "acb_beta_lower",         acb_hypgeom_beta_lower));}
static s7_pointer a_hypgeom_legendre_p(s7_scheme *sc, s7_pointer args) {return(acccii(sc, args, "acb_legendre_p",         acb_hypgeom_legendre_p));}
static s7_pointer a_hypgeom_legendre_q(s7_scheme *sc, s7_pointer args) {return(acccii(sc, args, "acb_legendre_q",         acb_hypgeom_legendre_q));}
static s7_pointer a_elliptic_rf(s7_scheme *sc, s7_pointer args)        {return(acccii(sc, args, "acb_elliptic_rf",        acb_elliptic_rf));}
static s7_pointer a_elliptic_rg(s7_scheme *sc, s7_pointer args)        {return(acccii(sc, args, "acb_elliptic_rg",        acb_elliptic_rg));}
static s7_pointer a_elliptic_pi_inc(s7_scheme *sc, s7_pointer args)    {return(acccii(sc, args, "acb_elliptic_pi_inc",    acb_elliptic_pi_inc));}


/* -------------------------------- libarb_s7_init -------------------------------- */

void libarb_s7_init(s7_scheme *sc);
void libarb_s7_init(s7_scheme *sc)
{
  s7_pointer old_shadow, arb, old_curlet, aci_sig, acci_sig, accii_sig, accci_sig, acccii_sig;

  acb_init(b);
  acb_init(nu);
  acb_init(w);
  acb_init(z);

  s7_define_constant(sc, "*arb*", arb = s7_inlet(sc, s7_nil(sc)));
  old_curlet = s7_set_curlet(sc, arb);
  old_shadow = s7_set_shadow_rootlet(sc, arb);

  aci_sig = s7_make_signature(sc, 2, s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?")); /* "i" in aci=precision */
  acci_sig = s7_make_signature(sc, 3, s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"));
  accii_sig = s7_make_signature(sc, 4, s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "integer?"));
  accci_sig = s7_make_signature(sc, 4, s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"));
  acccii_sig = s7_make_signature(sc, 5, s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), 
				 s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "number?"), s7_make_symbol(sc, "integer?"));

  s7_define_typed_function(sc, "acb_erf",             a_erf,             1, 0, false, "(acb_erf x)",             aci_sig);
  s7_define_typed_function(sc, "acb_erfc",            a_erfc,            1, 0, false, "(acb_erfc x)",            aci_sig);
  s7_define_typed_function(sc, "acb_erfi",            a_erfi,            1, 0, false, "(acb_erfi x)",            aci_sig);
  s7_define_typed_function(sc, "acb_ei",              a_ei,              1, 0, false, "(acb_ei x)",              aci_sig);
  s7_define_typed_function(sc, "acb_si",              a_si,              1, 0, false, "(acb_si x)",              aci_sig);
  s7_define_typed_function(sc, "acb_ci",              a_ci,              1, 0, false, "(acb_ci x)",              aci_sig);
  s7_define_typed_function(sc, "acb_shi",             a_shi,             1, 0, false, "(acb_shi x)",             aci_sig);
  s7_define_typed_function(sc, "acb_chi",             a_chi,             1, 0, false, "(acb_chi x)",             aci_sig);
  s7_define_typed_function(sc, "acb_erf_1f1a",        a_erf_1f1a,        1, 0, false, "(acb_erf_1f1a x)",        aci_sig);
  s7_define_typed_function(sc, "acb_erf_1f1b",        a_erf_1f1b,        1, 0, false, "(acb_erf_1f1b x)",        aci_sig);
  s7_define_typed_function(sc, "acb_ei_asymp",        a_ei_asymp,        1, 0, false, "(acb_ei_asymp x)",        aci_sig);
  s7_define_typed_function(sc, "acb_ei_2f2",          a_ei_2f2,          1, 0, false, "(acb_ei_2f2 x)",          aci_sig);
  s7_define_typed_function(sc, "acb_si_asymp",        a_si_asymp,        1, 0, false, "(acb_si_asymp x)",        aci_sig);
  s7_define_typed_function(sc, "acb_si_1f2",          a_si_1f2,          1, 0, false, "(acb_si_1f2 x)",          aci_sig);
  s7_define_typed_function(sc, "acb_ci_asymp",        a_ci_asymp,        1, 0, false, "(acb_ci_asymp x)",        aci_sig);
  s7_define_typed_function(sc, "acb_ci_2f3",          a_ci_2f3,          1, 0, false, "(acb_ci_2f3 x)",          aci_sig);
  s7_define_typed_function(sc, "acb_chi_asymp",       a_chi_asymp,       1, 0, false, "(acb_chi_asymp x)",       aci_sig);
  s7_define_typed_function(sc, "acb_chi_2f3",         a_chi_2f3,         1, 0, false, "(acb_chi_2f3 x)",         aci_sig);
  s7_define_typed_function(sc, "acb_dilog_bernoulli", a_dilog_bernoulli, 1, 0, false, "(acb_dilog_bernoulli x)", aci_sig);
  s7_define_typed_function(sc, "acb_dilog_zero_taylor", a_dilog_zero_taylor, 1, 0, false, "(acb_dilog_zero_taylor x)", aci_sig);
  s7_define_typed_function(sc, "acb_dilog_zero",      a_dilog_zero,      1, 0, false, "(acb_dilog_zero x)",      aci_sig);
  s7_define_typed_function(sc, "acb_dilog",           a_dilog,           1, 0, false, "(acb_dilog x)",           aci_sig);
  s7_define_typed_function(sc, "acb_elliptic_k",      a_elliptic_k,      1, 0, false, "(acb_elliptic_k x)",      aci_sig);
  s7_define_typed_function(sc, "acb_elliptic_e",      a_elliptic_e,      1, 0, false, "(acb_elliptic_e x)",      aci_sig);
  s7_define_typed_function(sc, "acb_elliptic_rc1",    a_elliptic_rc1,    1, 0, false, "(acb_elliptic_rc1 x)",    aci_sig);
  s7_define_typed_function(sc, "acb_gamma",           a_gamma,           1, 0, false, "(acb_gamma x)",           aci_sig);
  s7_define_typed_function(sc, "acb_rgamma",          a_rgamma,          1, 0, false, "(acb_rgamma x)",          aci_sig);
  s7_define_typed_function(sc, "acb_lgamma",          a_lgamma,          1, 0, false, "(acb_lgamma x)",          aci_sig);
  s7_define_typed_function(sc, "acb_log_sin_pi",      a_log_sin_pi,      1, 0, false, "(acb_log_sin_pi x)",      aci_sig);
  s7_define_typed_function(sc, "acb_digamma",         a_digamma,         1, 0, false, "(acb_digamma x)",         aci_sig);
  s7_define_typed_function(sc, "acb_zeta",            a_zeta,            1, 0, false, "(acb_zeta x)",            aci_sig);
  s7_define_typed_function(sc, "acb_log_barnes_g",    a_log_barnes_g,    1, 0, false, "(acb_log_barnes_g x)",    aci_sig);
  s7_define_typed_function(sc, "acb_barnes_g",        a_barnes_g,        1, 0, false, "(acb_barnes_g x)",        aci_sig);
  s7_define_typed_function(sc, "acb_sinc",            a_sinc,            1, 0, false, "(acb_sinc x)",            aci_sig);
  s7_define_typed_function(sc, "acb_sinc_pi",         a_sinc_pi,         1, 0, false, "(acb_sinc_pi x)",         aci_sig);
  s7_define_typed_function(sc, "acb_agm1",            a_agm1,            1, 0, false, "(acb_agm1 x)",            aci_sig);

  s7_define_typed_function(sc, "acb_bessel_j",        a_bessel_j,        2, 0, false, "(acb_bessel_j n x)",      acci_sig);
  s7_define_typed_function(sc, "acb_bessel_y",        a_bessel_y,        2, 0, false, "(acb_bessel_y n x)",      acci_sig);
  s7_define_typed_function(sc, "acb_bessel_i",        a_bessel_i,        2, 0, false, "(acb_bessel_i n x)",      acci_sig);
  s7_define_typed_function(sc, "acb_bessel_k",        a_bessel_k,        2, 0, false, "(acb_bessel_k n x)",      acci_sig);
  s7_define_typed_function(sc, "acb_hermite_h",       a_hermite_h,       2, 0, false, "(acb_hermite_h n x)",     acci_sig);
  s7_define_typed_function(sc, "acb_chebyshev_t",     a_chebyshev_t,     2, 0, false, "(acb_chebyshev_t n x)",   acci_sig);
  s7_define_typed_function(sc, "acb_chebyshev_u",     a_chebyshev_u,     2, 0, false, "(acb_chebyshev_u n x)",   acci_sig);
  s7_define_typed_function(sc, "acb_bessel_j_0f1",    a_bessel_j_0f1,    2, 0, false, "(acb_bessel_j_0f1 n x)",  acci_sig);
  s7_define_typed_function(sc, "acb_bessel_j_asymp",  a_bessel_j_asymp,  2, 0, false, "(acb_bessel_j_asymp n x)", acci_sig);
  s7_define_typed_function(sc, "acb_bessel_i_scaled", a_bessel_i_scaled, 2, 0, false, "(acb_bessel_i_scaled n x)", acci_sig);
  s7_define_typed_function(sc, "acb_bessel_k_scaled", a_bessel_k_scaled, 2, 0, false, "(acb_bessel_k_scaled n x)", acci_sig);
  s7_define_typed_function(sc, "acb_expint",          a_expint,          2, 0, false, "(acb_expint n x)",        acci_sig);
  s7_define_typed_function(sc, "acb_elliptic_pi",     a_elliptic_pi,     2, 0, false, "(acb_elliptic_pi)",       acci_sig);
  s7_define_typed_function(sc, "acb_elliptic_p",      a_elliptic_p,      2, 0, false, "(acb_elliptic_p)",        acci_sig);
  s7_define_typed_function(sc, "acb_elliptic_zeta",   a_elliptic_zeta,   2, 0, false, "(acb_elliptic_zeta)",     acci_sig);
  s7_define_typed_function(sc, "acb_elliptic_sigma",  a_elliptic_sigma,  2, 0, false, "(acb_elliptic_sigma)",    acci_sig);
  s7_define_typed_function(sc, "acb_elliptic_inv_p",  a_elliptic_inv_p,  2, 0, false, "(acb_elliptic_inv_p)",    acci_sig);
  s7_define_typed_function(sc, "acb_hurwitz_zeta",    a_hurwitz_zeta,    2, 0, false, "(acb_hurwitz_zeta)",      acci_sig);
  s7_define_typed_function(sc, "acb_polygamma",       a_polygamma,       2, 0, false, "(acb_polygamma)",         acci_sig);
  s7_define_typed_function(sc, "acb_polylog",         a_polylog,         2, 0, false, "(acb_polylog)",           acci_sig);
  s7_define_typed_function(sc, "acb_agm",             a_agm,             2, 0, false, "(acb_agm)",               acci_sig);
  s7_define_typed_function(sc, "acb_dilog_continuation", a_dilog_continuation, 2, 0, false, "(acb_dilog_continuation)", acci_sig);

  s7_define_typed_function(sc, "acb_bessel_i_0f1",       a_bessel_i_0f1,       3, 0, false, "(acb_bessel_i_0f1)",       accii_sig);
  s7_define_typed_function(sc, "acb_bessel_i_asymp",     a_bessel_i_asymp,     3, 0, false, "(acb_bessel_i_asymp)",     accii_sig);
  s7_define_typed_function(sc, "acb_bessel_k_0f1",       a_bessel_k_0f1,       3, 0, false, "(acb_bessel_k_0f1)",       accii_sig);
  s7_define_typed_function(sc, "acb_bessel_k_asymp",     a_bessel_k_asymp,     3, 0, false, "(acb_bessel_k_asymp)",     accii_sig);
  s7_define_typed_function(sc, "acb_hypgeom_0f1_asymp",  a_hypgeom_0f1_asymp,  3, 0, false, "(acb_hypgeom_0f1_asymp)",  accii_sig);
  s7_define_typed_function(sc, "acb_hypgeom_0f1_direct", a_hypgeom_0f1_direct, 3, 0, false, "(acb_hypgeom_0f1_direct)", accii_sig);
  s7_define_typed_function(sc, "acb_hypgeom_0f1",        a_hypgeom_0f1,        3, 0, false, "(acb_hypgeom_0f1)",        accii_sig);
  s7_define_typed_function(sc, "acb_gamma_lower",        a_gamma_lower,        3, 0, false, "(acb_gamma_lower)",        accii_sig);
  s7_define_typed_function(sc, "acb_gamma_upper_asymp",  a_gamma_upper_asymp,  3, 0, false, "(acb_gamma_upper_asymp)",  accii_sig);
  s7_define_typed_function(sc, "acb_gamma_upper_1f1a",   a_gamma_upper_1f1a,   3, 0, false, "(acb_gamma_upper_1f1a)",   accii_sig);
  s7_define_typed_function(sc, "acb_gamma_upper_1f1b",   a_gamma_upper_1f1b,   3, 0, false, "(acb_gamma_upper_1f1b)",   accii_sig);
  s7_define_typed_function(sc, "acb_gamma_upper",        a_gamma_upper,        3, 0, false, "(acb_gamma_upper)",        accii_sig);
  s7_define_typed_function(sc, "acb_elliptic_e_inc",     a_elliptic_e_inc,     3, 0, false, "(acb_elliptic_e_inc)",     accii_sig);
  s7_define_typed_function(sc, "acb_elliptic_f",         a_elliptic_f,         3, 0, false, "(acb_elliptic_f)",         accii_sig);

  s7_define_typed_function(sc, "acb_hypgeom_u_1f1",      a_hypgeom_u_1f1,      3, 0, false, "(acb_hypgeom_u_1f1)", accci_sig);
  s7_define_typed_function(sc, "acb_hypgeom_u",          a_hypgeom_u,          3, 0, false, "(acb_hypgeom_u)",     accci_sig);
  s7_define_typed_function(sc, "acb_gegenbauer_c",       a_gegenbauer_c,       3, 0, false, "(acb_gegenbauer_c)",  accci_sig);
  s7_define_typed_function(sc, "acb_laguerre_l",         a_laguerre_l,         3, 0, false, "(acb_laguerre_l)",    accci_sig);

  s7_define_typed_function(sc, "acb_hypgeom_m_asymp",    a_hypgeom_m_asymp,    4, 0, false, "(acb_hypgeom_m_asymp)", acccii_sig);
  s7_define_typed_function(sc, "acb_hypgeom_m_1f1",      a_hypgeom_m_1f1,      4, 0, false, "(acb_hypgeom_m_1f1)",   acccii_sig);
  s7_define_typed_function(sc, "acb_hypgeom_m",          a_hypgeom_m,          4, 0, false, "(acb_hypgeom_m)",       acccii_sig);
  s7_define_typed_function(sc, "acb_hypgeom_1f1",        a_hypgeom_1f1,        4, 0, false, "(acb_hypgeom_1f1)",     acccii_sig);
  s7_define_typed_function(sc, "acb_beta_lower",         a_hypgeom_beta_lower, 4, 0, false, "(acb_beta_lower)",      acccii_sig);
  s7_define_typed_function(sc, "acb_legendre_p",         a_hypgeom_legendre_p, 4, 0, false, "(acb_legendre_p)",      acccii_sig);
  s7_define_typed_function(sc, "acb_legendre_q",         a_hypgeom_legendre_q, 4, 0, false, "(acb_legendre_q)",      acccii_sig);
  s7_define_typed_function(sc, "acb_elliptic_rf",        a_elliptic_rf,        4, 0, false, "(acb_elliptic_rf)",     acccii_sig);
  s7_define_typed_function(sc, "acb_elliptic_rg",        a_elliptic_rg,        4, 0, false, "(acb_elliptic_rg)",     acccii_sig);
  s7_define_typed_function(sc, "acb_elliptic_pi_inc",    a_elliptic_pi_inc,    4, 0, false, "(acb_elliptic_pi_inc)", acccii_sig);


  s7_set_curlet(sc, old_curlet);
  s7_set_shadow_rootlet(sc, old_shadow);
}

/* gcc -fPIC -c libarb_s7.c
 * gcc libarb_s7.o -shared -o libarb_s7.so -lflint -larb
 * repl
 *   > (load "libarb_s7.so" (inlet 'init_func 'libarb_s7_init))
 *   > (acb_bessel_j 0 1.0)
 *   7.651976865579665514497175261026632209096E-1
 */


#if 0
;; src_ptr?
void acb_hypgeom_pfq(acb_t res, acb_srcptr a, slong p, acb_srcptr b, slong q, const acb_t z, int regularized, slong prec);

void acb_hypgeom_airy(acb_t ai, acb_t aip, acb_t bi, acb_t bip, const acb_t z, slong prec);
void acb_hypgeom_coulomb(acb_t F, acb_t G, acb_t Hpos, acb_t Hneg, const acb_t l, const acb_t eta, const acb_t z, slong prec);
void acb_hypgeom_fresnel(acb_t res1, acb_t res2, const acb_t z, int normalized, slong prec);
void acb_hypgeom_bessel_jy(acb_t res1, acb_t res2, const acb_t nu, const acb_t z, slong prec);

void acb_hypgeom_2f1(acb_t res, const acb_t a, const acb_t b, const acb_t c, const acb_t z, int regularized, slong prec);
void acb_elliptic_rj(acb_t res, const acb_t x, const acb_t y, const acb_t z, const acb_t p, int flags, slong prec);

;; fmpz?
void acb_lambertw(acb_t res, const acb_t z, const fmpz_t k, int flags, slong prec);
void acb_quadratic_roots_fmpz(acb_t r1, acb_t r2, const fmpz_t a, const fmpz_t b, const fmpz_t c, slong prec);

void acb_hypgeom_li(acb_t res, const acb_t z, int offset, slong prec);

void acb_chebyshev_t_ui(acb_t a, ulong n, const acb_t x, slong prec);
void acb_chebyshev_u_ui(acb_t a, ulong n, const acb_t x, slong prec);
void acb_chebyshev_t2_ui(acb_t a, acb_t b, ulong n, const acb_t x, slong prec);
void acb_chebyshev_u2_ui(acb_t a, acb_t b, ulong n, const acb_t x, slong prec);
#endif
