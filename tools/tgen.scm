(if (not (or (provided? 'snd)
	     (provided? 'sndlib)))
    (begin
      (format *stderr* "tgen depends on sndlib...~%")
      (system "./snd -noinit tgen.scm")
      (exit)))

(if (provided? 'snd)
    (begin
      (load "ws.scm")
      (load "hooks.scm")
      (reset-all-hooks)
      (set! (auto-update) #f)
      (set! (auto-update-interval) 0.0)
      (set! *to-snd* #f))
    (load "sndlib-ws.scm"))

(load "generators.scm")

(set! *clm-file-buffer-size* 16)
(set! *clm-table-size* 16)
(set! *clm-clipped* #f)
;(set! (*s7* 'gc-stats) #t)

(define start-run (get-internal-real-time))
(define M (float-vector 0 0 1 10))
(define P (float-vector 0 0 1 1))

(set! (*s7* 'initial-string-port-length) 8192)

(mus-sound-preload "1a.snd")

(define (env-1 g) (env g))
(define (make-env-1 size) (make-env M :length 10))

(define pulsed-env-1 pulsed-env)
(define (make-pulsed-env-1 size) (make-pulsed-env P .01 1000.0))

(define delay-1 delay)
(define (make-delay-1 size) (make-delay 4))

(define comb-1 comb)
(define (make-comb-1 size) (make-comb .9 4))

(define filtered-comb-1 filtered-comb)
(define (make-filtered-comb-1 size) (make-filtered-comb .9 4))

(define notch-1 notch)
(define (make-notch-1 size) (make-notch .9 4))

(define all-pass-1 all-pass)
(define (make-all-pass-1 size) (make-all-pass .9 .4 4))

(define one-pole-all-pass-1 one-pole-all-pass)
(define (make-one-pole-all-pass-1 size) (make-one-pole-all-pass 4 .5))

(define moving-average-1 moving-average)
(define (make-moving-average-1 size) (make-moving-average 4))

(define moving-max-1 moving-max)
(define (make-moving-max-1 size) (make-moving-max 3))

(define moving-norm-1 moving-norm)
(define (make-moving-norm-1 size) (make-moving-norm 3))

(define one-pole-1 one-pole)
(define (make-one-pole-1 size) (make-one-pole .9 .4))

(define two-pole-1 two-pole)
(define (make-two-pole-1 size) (make-two-pole .9 .4 .1))

(define one-zero-1 one-zero)
(define (make-one-zero-1 size) (make-one-zero .9 .4))

(define two-zero-1 two-zero)
(define (make-two-zero-1 size) (make-two-zero .9 .4 .1))

(define table-lookup-1 table-lookup)
(define table-lookup-table (partials->wave '(1 1.0)))
(define (make-table-lookup-1 size) (make-table-lookup 16 :wave table-lookup-table))

(define formant-1 formant)
(define (make-formant-1 size) (make-formant size .1))

(define firmant-1 firmant)
(define (make-firmant-1 size) (make-firmant size .1))

(define fx (float-vector .1 -.2 .3))
(define fy (float-vector -.1 .02 -.3))

(define filter-1 filter)
(define (make-filter-1 size) (make-filter 3 fx fy))

(define fir-filter-1 fir-filter)
(define (make-fir-filter-1 size) (make-fir-filter 3 fx))

(define iir-filter-1 iir-filter)
(define (make-iir-filter-1 size) (make-iir-filter 3 fx))

(define readin-1 readin)
(define (make-readin-1 size) (make-readin "1a.snd"))

(define (io dir) .1)
(define src-1 src)
(define (make-src-1 size) (make-src io 2.0))

(define granulate-1 granulate)
(define (make-granulate-1 size) (make-granulate io 2.0 0.001 0.6 0.001 .4 0.0))

(define phase-vocoder-1 phase-vocoder)
(define (make-phase-vocoder-1 size) (make-phase-vocoder io 16))

(define ssb-am-1 ssb-am)
(define (make-ssb-am-1 size) (make-ssb-am 100.0 20))

(define wave-train-1 wave-train)
(define wt-train (make-float-vector 20 0.1))
(define (make-wave-train-1 size) (make-wave-train 1000.0 0.0 wt-train))

(define convolve-1 convolve)
(define (make-convolve-1 size) (make-convolve io (make-float-vector 16 .2) 16))

(define oscil-bank-1 oscil-bank)
(define ob-freqs (float-vector 100.0 200.0))
(define ob-amps (float-vector 0.5 0.5))
(define (make-oscil-bank-1 size) (make-oscil-bank ob-freqs (float-vector 0.0 0.0) ob-amps))

(define formant-bank-1 formant-bank)
(define (make-formant-bank-1 size) (make-formant-bank (vector (make-formant 440.0 .95))))

(define comb-bank-1 comb-bank)
(define (make-comb-bank-1 size) (make-comb-bank (vector (make-comb .5 6))))

(define filtered-comb-bank-1 filtered-comb-bank)
(define (make-filtered-comb-bank-1 size) (make-filtered-comb-bank (vector (make-filtered-comb .5 6))))

(define all-pass-bank-1 all-pass-bank)
(define (make-all-pass-bank-1 size) (make-all-pass-bank (vector (make-all-pass .5 .4 6))))

(define rand-1 rand)
(define (make-rand-1 size) (set! (mus-rand-seed) 12345) (make-rand 5.0 0.1))

(define rand-interp-1 rand-interp)
(define (make-rand-interp-1 size) (set! (mus-rand-seed) 12345) (make-rand-interp 5.0 0.1))


;;; we are creating millions of functions here, so we need to keep them from
;;;   being removed from the heap, and make sure they're GC'd -- *safety*=1
;;;   keeps them in the heap, but s7 continues to allocate space for each redefinition,
;;;   hence the extra let below.


(define-constant (vequal v1 v2)
  (or (morally-equal? v1 v2)
      (float-vector-equal? v1 v2 1e-5))) ; "relative" equality: diff/mx

;(set! (*s7* 'morally-equal-float-epsilon) 1e-5)
;(define vequal morally-equal?)

(define-constant (checkout str V v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
  (if (not (and (vequal V v1)
		(vequal v1 v2)
		(vequal v1 v3)
		(vequal v1 v4)))
      (format *stderr* "~S:~%    no do:  ~A~%    fv-set: ~A~%    outa->v:~A~%    outa:   ~A~%    list:   ~A~%" str V v1 v2 v3 v4))
  (if (not (vequal v5 v6)) (format *stderr* "dox ~S:~%   fv-set: ~A~%    outa->v:~A~%" str v5 v6))
  (if (not (vequal v7 v8)) (format *stderr* "let ~S:~%    ~A~%    ~A~%" str v7 v8))
  (if (not (vequal v9 v10)) (format *stderr* "env let ~S:~%    ~A~%    ~A~%" str v9 v10))
  (if (not (vequal v11 v12)) (format *stderr* "letx ~S:~%    ~A~%    ~A~%" str v11 v12)))

(define-constant (checkout-1 str V v1 v2 v3 v4 v5 v6 v11 v12)
  (if (not (and (vequal V v1)
		(vequal v1 v2)
		(vequal v1 v3)
		(vequal v1 v4)))
      (format *stderr* "~S:~%    no do:  ~A~%    fv-set: ~A~%    outa->v:~A~%    outa:   ~A~%    list:   ~A~%" str V v1 v2 v3 v4))
  (if (not (vequal v5 v6)) (format *stderr* "dox ~S:~%   fv-set: ~A~%    outa->v:~A~%" str v5 v6))
  (if (not (vequal v11 v12)) (format *stderr* "letx ~S:~%    ~A~%    ~A~%" str v11 v12)))

(define-constant F (make-env (float-vector 0.0 .1 1.0 1.0) :length 100))
(define-constant K (float-vector 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
(define-constant V (make-float-vector 10))
(define-constant VV1 (make-float-vector 10))
(define-constant VV4 (make-float-vector 10))
(define-constant VV5 (make-float-vector 10))
(define-constant VV6 (make-float-vector 10))
(define-constant VV8 (make-float-vector 10))
(define-constant VV10 (make-float-vector 10))
(define (Z) (mus-copy F))

(define G #f)
(define I #f)
(define (O) (vector #f (mus-copy I) #f))
(define (Q) (mus-copy G))

(define (try1 form gen)
  (let ((make-gen (string->symbol (string-append "make-" (symbol->string gen)))))
    (let ((body
     `(let ()
	(define (tester-1)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1) (v VV1))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (float-vector-set! v i ,form))))
	
	(define (tester-2)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (outa i ,form))))
	
	(define (tester-3)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (out-any i ,form 0))))
	
	(define (tester-4)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (do ((i 0 (+ i 1)) (lst ())) ((= i 10) (apply float-vector (reverse! lst)))
	      (set! lst (cons ,form lst)))))
	
	(define (tester-5)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (y -0.5) (k 1) (v VV4))
	    (set! *output* (make-sample->file "test.snd" 1 mus-ldouble mus-next "t816"))
	    (do ((i 0 (+ i 1)) (x 0.0 (+ x 0.1))) ((= i 10))
	      (outa i ,form))
	    (mus-close *output*)
	    (file->array "test.snd" 0 0 10 v)))
	
	(define (tester-6)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (k 1) (v VV5))
	    (do ((i 0 (+ i 1)) (y -0.5) (x 0.0 (+ x 0.1))) ((= i 10) v)
	      (float-vector-set! v i ,form))))
	
	(define (tester-11)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (y -0.5) (k 1) (v VV6))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((x (,gen o)))
		(set! (v i) ,form)))))
	
	(define (tester-12)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((x (,gen o)))
		(outa i ,form)))))

	(set! G (,make-gen 1000)) 
	(set! I (,make-gen 500)) 

	(let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	  (do ((i 0 (+ i 1))) ((= i 10))
	    (set! (V i) ,form)))
	
	(checkout-1 ',form V (tester-1) (tester-2) (tester-3) (tester-4) (tester-5) (tester-6) (tester-11) (tester-12))
	)))
      (define the-body (apply lambda () (list (copy body :readable))))
      (the-body))))

(define (try2 form gen)
  (let ((make-gen (string->symbol (string-append "make-" (symbol->string gen)))))
    (let ((body
     `(let ()
	(define (tester-1)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1) (v VV1))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (float-vector-set! v i ,form))))
	
	(define (tester-2)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (outa i ,form))))
	
	(define (tester-3)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (out-any i ,form 0))))
	
	(define (tester-4)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (do ((i 0 (+ i 1)) (lst ())) ((= i 10) (apply float-vector (reverse! lst)))
	      (set! lst (cons ,form lst)))))
	
	(define (tester-5)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (y -0.5) (k 1) (v VV4))
	    (set! *output* (make-sample->file "test.snd" 1 mus-ldouble mus-next "t816"))
	    (do ((i 0 (+ i 1)) (x 0.0 (+ x 0.1))) ((= i 10))
	      (outa i ,form))
	    (mus-close *output*)
	    (file->array "test.snd" 0 0 10 v)))
	
	(define (tester-6)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (k 1) (v VV5))
	    (do ((i 0 (+ i 1)) (y -0.5) (x 0.0 (+ x 0.1))) ((= i 10) v)
	      (float-vector-set! v i ,form))))
	
	(define (tester-7)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1) (v VV6))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((z ,form))
		(float-vector-set! v i (,gen o z))))))
	
	(define (tester-8)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((z ,form))
		(outa i (,gen o z))))))
	
	(define (tester-9)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1) (v VV8))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((z ,form))
		(float-vector-set! v i (* (env a) (,gen o z)))))))
	
	(define (tester-10)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((z ,form))
		(outa i (* (env a) (,gen o z)))))))
	
	(define (tester-11)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (y -0.5) (k 1) (v VV10))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((x (,gen o)))
		(set! (v i) ,form)))))
	
	(define (tester-12)
	  (let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (y -0.5) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((x (,gen o)))
		(outa i ,form)))))

	(set! G (,make-gen 1000)) 
	(set! I (,make-gen 500)) 

	(let ((o (Q)) (p (Q)) (q (Q)) (oscs (O)) (a (Z)) (b (Z)) (x 3.14) (y -0.5) (k 1))
	  (do ((i 0 (+ i 1))) ((= i 10))
	    (set! (V i) ,form)))
	
	(checkout ',form V
		  (tester-1) (tester-2) (tester-3) (tester-4) (tester-5) (tester-6) 
		  (tester-7) (tester-8) (tester-9) (tester-10) (tester-11) (tester-12))
	)))
      (define the-body (apply lambda () (list (copy body :readable))))
      (the-body))))

(define (try34 form gen args)
  (let ((make-gen (string->symbol (string-append "make-" (symbol->string gen)))))
    (let ((body
     `(let ()
	(define (tester-1)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1) (v VV1))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (float-vector-set! v i ,form))))
	
	(define (tester-2)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (outa i ,form))))
	
	(define (tester-3)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (out-any i ,form 0))))
	
	(define (tester-4)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (k 1) (z 0.1))
	    (do ((i 0 (+ i 1)) (lst ())) ((= i 10) (apply float-vector (reverse! lst)))
	      (set! lst (cons ,form lst)))))
	
	(define (tester-5)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (y -0.5) (k 1) (z 0.1) (v VV4))
	    (set! *output* (make-sample->file "test.snd" 1 mus-ldouble mus-next "t816"))
	    (do ((i 0 (+ i 1)) (x 0.0 (+ x 0.1))) ((= i 10))
	      (outa i ,form))
	    (mus-close *output*)
	    (file->array "test.snd" 0 0 10 v)))
	
	(define (tester-6)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (k 1) (v VV5))
	    (do ((i 0 (+ i 1)) (y -0.5) (z 0.1) (x 0.0 (+ x 0.1))) ((= i 10) v)
	      (set! (v i) ,form))))
	
	(define (tester-7)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (k 1) (z 0.1) (v VV6))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((zz ,form))
		(float-vector-set! v i (,gen o zz))))))
	
	(define (tester-8)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((zz ,form))
		(outa i (,gen o zz))))))
	
	(define (tester-9)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1) (v VV8))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((zz ,form))
		(float-vector-set! v i (* (env a) (,gen o zz)))))))
	
	(define (tester-10)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((zz ,form))
		(outa i (* (env a) (,gen o zz)))))))
	
	(define (tester-11)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (y -0.5) (z 0.1) (k 1) (v VV10))
	    (do ((i 0 (+ i 1))) ((= i 10) v)
	      (let ((x (,gen o)))
		(float-vector-set! v i ,form)))))
	
	(define (tester-12)
	  (let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (y -0.5) (z 0.1) (k 1))
	    (set! *output* (make-float-vector 10))
	    (do ((i 0 (+ i 1))) ((= i 10) *output*)
	      (let ((x (,gen o)))
		(outa i ,form)))))

	(set! G (,make-gen 1000)) 
	(set! I (,make-gen 500)) 

	(let ((o (Q)) (p (Q)) (q (Q)) (s (Q)) (t (Q)) (oscs (O)) (a (Z)) (b (Z)) (c (Z)) (d (Z)) (x 3.14) (y -0.5) (z 0.1) (k 1))
	  (do ((i 0 (+ i 1))) ((= i 10))
	    (float-vector-set! V i ,form)))
	
	(if (= args 1)
	    (checkout-1 ',form V (tester-1) (tester-2) (tester-3) (tester-4) (tester-5) (tester-6) (tester-11) (tester-12))
	    (checkout ',form V
		      (tester-1) (tester-2) (tester-3) (tester-4) (tester-5) (tester-6) 
		      (tester-7) (tester-8) (tester-9) (tester-10) (tester-11) (tester-12))
	    ))))
      (define the-body (apply lambda () (list (copy body :readable))))
      (the-body))))

(define (test-gen gen nargs)
  (define args1 (list 1.5 (list gen 'p) '(env a) 'x 'i (list gen 'o) '(- 1.0 x) (list gen '(vector-ref oscs k))))
  (define args2 (list 1.5 (list gen 'q) '(env b) 'y 'i '(ina i K)))
  (define args3 (list 1.5 (list gen 's) '(env c) 'z 'i '(cos x)))
  (define args4 (list 1.5 (list gen 't) '(env d) 'x 'i))

  (if (= nargs 1)
      (begin
	(for-each 
	 (lambda (a)
	   (try1 a gen)
	   (when (> nargs 1)
	     (try1 `(,gen o ,a) gen)
	     (try1 `(abs (,gen o ,a)) gen)))
	 args1)

	(for-each 
	 (lambda (a) 
	   (for-each 
	    (lambda (b) 
	      (try1 `(+ ,a ,b) gen)
	      (try1 `(- ,a ,b) gen)
	      (try1 `(* ,a ,b) gen)
	      (try1 `(cos (+ ,a ,b)) gen)
	      (try1 `(sin (* ,a ,b)) gen)
	      (try1 `(abs (* ,a ,b)) gen)
	      (try1 `(* ,a (abs ,b)) gen)
	      (when (> nargs 1)
		(try1 `(,gen o (+ ,a ,b)) gen)
		(try1 `(,gen o (* ,a ,b)) gen)
		(try1 `(+ ,a (,gen o ,b)) gen)
		(try1 `(* ,a (,gen o ,b)) gen)
		(try1 `(+ (,gen o ,a) ,b) gen)
		(try1 `(* (,gen o ,a) ,b) gen)
		(try1 `(* (abs (,gen o ,a)) ,b) gen)))
	    args2))
	 args1))

      (begin
       (for-each 
	(lambda (a)
	  (try2 a gen)
	  (when (> nargs 1)
	    (try2 `(,gen o ,a) gen)
	    (try2 `(abs (,gen o ,a)) gen)))
	args1)
       
       (for-each 
	(lambda (a) 
	  (for-each 
	   (lambda (b) 
	     (try2 `(+ ,a ,b) gen)
	     (try2 `(- ,a ,b) gen)
	     (try2 `(* ,a ,b) gen)
	     (try2 `(cos (+ ,a ,b)) gen)
	     (try2 `(sin (* ,a ,b)) gen)
	     (try2 `(abs (* ,a ,b)) gen)
	     (try2 `(* ,a (abs ,b)) gen)
	     (when (> nargs 1)
	       (try2 `(,gen o (+ ,a ,b)) gen)
	       (try2 `(,gen o (* ,a ,b)) gen)
	       (try2 `(+ ,a (,gen o ,b)) gen)
	       (try2 `(* ,a (,gen o ,b)) gen)
	       (try2 `(+ (,gen o ,a) ,b) gen)
	       (try2 `(* (,gen o ,a) ,b) gen)
	       (try2 `(* (abs (,gen o ,a)) ,b) gen)))
	   args2))
	args1)))
#|
  (for-each 
   (lambda (c)
     (for-each
      (lambda (b)
	(for-each
	 (lambda (a)
	   (try34 `(+ ,a ,b ,c) gen nargs)
	   (try34 `(+ (* ,a ,b) ,c) gen nargs)
	   (try34 `(+ ,a (* ,b ,c)) gen nargs)
	   (try34 `(* ,a ,b ,c) gen nargs)
	   (try34 `(* ,a (+ ,b ,c)) gen nargs)
	   (try34 `(* (+ ,a ,b) ,c) gen nargs)
	   (when (> nargs 1)
	     (try34 `(,gen o (+ ,a ,b ,c)) gen nargs)
	     (try34 `(,gen o (* ,a ,b ,c)) gen nargs)
	     (try34 `(,gen o (* ,a (+ ,b ,c))) gen nargs)
	     (try34 `(,gen o (+ ,a (* ,b ,c))) gen nargs)
	     (try34 `(,gen o (* (+ ,a ,b) ,c)) gen nargs)
	     (try34 `(,gen o (+ (* ,a ,b) ,c)) gen nargs)
	     (try34 `(+ ,a (,gen o (+ ,b ,c))) gen nargs)
	     (try34 `(+ ,a (,gen o (* ,b ,c))) gen nargs)
	     (try34 `(* ,a (,gen o (+ ,b ,c))) gen nargs)
	     (try34 `(* ,a (,gen o (* ,b ,c))) gen nargs)
	     
	     (try34 `(+ ,a ,b (,gen o ,c)) gen nargs)
	     (try34 `(* ,a ,b (,gen o ,c)) gen nargs)
	     (try34 `(+ (* ,a ,b) (,gen o ,c)) gen nargs)
	     (try34 `(* (+ ,a ,b) (,gen o ,c)) gen nargs)
	     (try34 `(+ ,a (* ,b (,gen o ,c))) gen nargs)
	     (try34 `(* ,a (+ ,b (,gen o ,c))) gen nargs)
	     
	     (try34 `(+ ,a (,gen o ,b) ,c) gen nargs)
	     (try34 `(* ,a (,gen o ,b) ,c) gen nargs)
	     (try34 `(+ (* ,a (,gen o ,b)) ,c) gen nargs)
	     (try34 `(* (+ ,a (,gen o ,b)) ,c) gen nargs)
	     (try34 `(+ ,a (* (,gen o ,b) ,c)) gen nargs)
	     (try34 `(* ,a (+ (,gen o ,b) ,c)) gen nargs)
	     
	     (try34 `(+ (,gen o ,a) ,b ,c) gen nargs)
	     (try34 `(+ (,gen o ,a) (* ,b ,c)) gen nargs)
	     (try34 `(* (,gen o ,a) (+ ,b ,c)) gen nargs)
	     (try34 `(* (,gen o ,a) ,b ,c) gen nargs))
	   
	   (try34 `(+ ,a (abs ,b) ,c) gen nargs)
	   (try34 `(+ ,a (sin ,b) ,c) gen nargs)
	   (try34 `(+ ,a (cos ,b) ,c) gen nargs))
	 args3))
      args2))
   args1)

  (for-each
   (lambda (d)
     (for-each 
      (lambda (c)
	(for-each
	 (lambda (b)
	   (for-each
	    (lambda (a)
	      (try34 `(+ ,a ,b ,c ,d) gen nargs)
	      (try34 `(* ,a ,b ,c ,d) gen nargs)
	      (try34 `(+ (* ,a ,b) (* ,c ,d)) gen nargs)
	      (try34 `(* (+ ,a ,b) (+ ,c ,d)) gen nargs)
	      (try34 `(+ ,a (* ,b ,c ,d)) gen nargs)
	      (try34 `(* ,a (+ ,b ,c ,d)) gen nargs)
	      (try34 `(+ ,a (* ,b (+ ,c ,d))) gen nargs)
	      (try34 `(* ,a (+ ,b (* ,c ,d))) gen nargs)
	      (try34 `(+ (* ,a ,b ,c) ,d) gen nargs)
	      (try34 `(* (+ ,a ,b ,c) ,d) gen nargs)
	      (try34 `(+ (* ,a (+ ,b ,c)) ,d) gen nargs)
	      (try34 `(* (+ ,a (* ,b ,c)) ,d) gen nargs)
	      
	      (when (> nargs 1)
		(try34 `(+ (,gen o ,a) ,b ,c ,d) gen nargs)
		(try34 `(* (,gen o ,a) ,b ,c ,d) gen nargs)
		(try34 `(+ (* (,gen o ,a) ,b) (* ,c ,d)) gen nargs)
		(try34 `(* (+ (,gen o ,a) ,b) (+ ,c ,d)) gen nargs)
		(try34 `(+ (,gen o ,a) (* ,b ,c ,d)) gen nargs)
		(try34 `(* (,gen o ,a) (+ ,b ,c ,d)) gen nargs)
		(try34 `(+ (,gen o ,a) (* ,b (+ ,c ,d))) gen nargs)
		(try34 `(* (,gen o ,a) (+ ,b (* ,c ,d))) gen nargs)
		(try34 `(+ (* (,gen o ,a) ,b ,c) ,d) gen nargs)
		(try34 `(* (+ (,gen o ,a) ,b ,c) ,d) gen nargs)
		(try34 `(+ (* (,gen o ,a) (+ ,b ,c)) ,d) gen nargs)
		(try34 `(* (+ (,gen o ,a) (* ,b ,c)) ,d) gen nargs)
		
		(try34 `(+ ,a (,gen o ,b) ,c ,d) gen nargs)
		(try34 `(* ,a (,gen o ,b) ,c ,d) gen nargs)
		(try34 `(+ (* ,a (,gen o ,b)) (* ,c ,d)) gen nargs)
		(try34 `(* (+ ,a (,gen o ,b)) (+ ,c ,d)) gen nargs)
		(try34 `(+ ,a (* (,gen o ,b) ,c ,d)) gen nargs)
		(try34 `(* ,a (+ (,gen o ,b) ,c ,d)) gen nargs)
		(try34 `(+ ,a (* (,gen o ,b) (+ ,c ,d))) gen nargs)
		(try34 `(* ,a (+ (,gen o ,b) (* ,c ,d))) gen nargs)
		(try34 `(+ (* ,a (,gen o ,b) ,c) ,d) gen nargs)
		(try34 `(* (+ ,a (,gen o ,b) ,c) ,d) gen nargs)
		(try34 `(+ (* ,a (+ (,gen o ,b) ,c)) ,d) gen nargs)
		(try34 `(* (+ ,a (* (,gen o ,b) ,c)) ,d) gen nargs)
		
		(try34 `(+ ,a ,b (,gen o ,c) ,d) gen nargs)
		(try34 `(* ,a ,b (,gen o ,c) ,d) gen nargs)
		(try34 `(+ (* ,a ,b) (* (,gen o ,c) ,d)) gen nargs)
		(try34 `(* (+ ,a ,b) (+ (,gen o ,c) ,d)) gen nargs)
		(try34 `(+ ,a (* ,b (,gen o ,c) ,d)) gen nargs)
		(try34 `(* ,a (+ ,b (,gen o ,c) ,d)) gen nargs)
		(try34 `(+ ,a (* ,b (+ (,gen o ,c) ,d))) gen nargs)
		(try34 `(* ,a (+ ,b (* (,gen o ,c) ,d))) gen nargs)
		(try34 `(+ (* ,a ,b (,gen o ,c)) ,d) gen nargs)
		(try34 `(* (+ ,a ,b (,gen o ,c)) ,d) gen nargs)
		(try34 `(+ (* ,a (+ ,b (,gen o ,c))) ,d) gen nargs)
		(try34 `(* (+ ,a (* ,b (,gen o ,c))) ,d) gen nargs)
		
		(try34 `(+ ,a ,b ,c (,gen o ,d)) gen nargs)
		(try34 `(* ,a ,b ,c (,gen o ,d)) gen nargs)
		(try34 `(+ (* ,a ,b) (* ,c (,gen o ,d))) gen nargs)
		(try34 `(* (+ ,a ,b) (+ ,c (,gen o ,d))) gen nargs)
		(try34 `(+ ,a (* ,b ,c (,gen o ,d))) gen nargs)
		(try34 `(* ,a (+ ,b ,c (,gen o ,d))) gen nargs)
		(try34 `(+ ,a (* ,b (+ ,c (,gen o ,d)))) gen nargs)
		(try34 `(* ,a (+ ,b (* ,c (,gen o ,d)))) gen nargs)
		(try34 `(+ (* ,a ,b ,c) (,gen o ,d)) gen nargs)
		(try34 `(* (+ ,a ,b ,c) (,gen o ,d)) gen nargs)
		(try34 `(+ (* ,a (+ ,b ,c)) (,gen o ,d)) gen nargs)
		(try34 `(* (+ ,a (* ,b ,c)) (,gen o ,d)) gen nargs)
	      
		(try34 `(,gen o (+ ,a ,b ,c ,d)) gen nargs)
		(try34 `(,gen o (* ,a ,b ,c ,d)) gen nargs)
		(try34 `(,gen o (+ (* ,a ,b) (* ,c ,d))) gen nargs)
		(try34 `(,gen o (* (+ ,a ,b) (+ ,c ,d))) gen nargs)
		(try34 `(,gen o (+ ,a (* ,b ,c ,d))) gen nargs)
		(try34 `(,gen o (* ,a (+ ,b ,c ,d))) gen nargs)
		(try34 `(,gen o (+ ,a (* ,b (+ ,c ,d)))) gen nargs)
		(try34 `(,gen o (* ,a (+ ,b (* ,c ,d)))) gen nargs)
		(try34 `(,gen o (+ (* ,a ,b ,c) ,d)) gen nargs)
		(try34 `(,gen o (* (+ ,a ,b ,c) ,d)) gen nargs)
		(try34 `(,gen o (+ (* ,a (+ ,b ,c)) ,d)) gen nargs)
		(try34 `(,gen o (* (+ ,a (* ,b ,c)) ,d)) gen nargs)
		
		(try34 `(+ (,gen o (* ,a ,b)) (* ,c ,d)) gen nargs)
		(try34 `(* (,gen o (+ ,a ,b)) (+ ,c ,d)) gen nargs)
		(try34 `(,gen o (+ (* ,a ,b) (* ,c ,d))) gen nargs)
		(try34 `(+ ,a (* ,b (,gen o (+ ,c ,d)))) gen nargs)
		(try34 `(* ,a (+ ,b (,gen o (* ,c ,d)))) gen nargs)
		(try34 `(+ (* ,a (,gen o (+ ,b ,c))) ,d) gen nargs)
		(try34 `(* (+ ,a (,gen o (* ,b ,c))) ,d) gen nargs))
	      
	      (try34 `(+ ,a ,b ,c ,d (,gen o)) gen nargs)
	      (try34 `(* ,a ,b ,c ,d (,gen o)) gen nargs))
	    args4))
	 args3))
      args2))
   args1)
|#
  )

(for-each 
 (lambda (gen nargs)
   ;(gc)
   (set! start-run (get-internal-real-time))
   (test-gen gen nargs)
   (format *stderr* "~A: ~20T~,3F~%" gen (* 1.0 (/ (- (get-internal-real-time) start-run) internal-time-units-per-second))))
; '(adjustable-oscil)
; '(2)

 '(;rand-1 rand-interp-1 ; the y-as-fm case will be different (ignore printout)
   r2k!cos filter-1 fir-filter-1 iir-filter-1 oscil
   one-pole-all-pass-1 env-1 pulsed-env-1 
   formant-1 firmant-1 
   polywave polyshape ncos nsin nrxycos nrxysin rxyk!sin rxyk!cos asymmetric-fm square-wave triangle-wave pulse-train sawtooth-wave
   one-pole-1 one-zero-1 two-pole-1 two-zero-1
   oscil-bank-1 
   delay-1 comb-1 notch-1 all-pass-1 filtered-comb-1
   moving-max-1 moving-norm-1 moving-average-1 
   table-lookup-1 wave-train-1
   formant-bank-1 comb-bank-1 filtered-comb-bank-1 all-pass-bank-1
   adjustable-oscil
   readin-1 convolve-1 src-1 granulate-1 ssb-am-1 phase-vocoder-1
   )

 '(;2 2 
   2 2 2 2 2 
   2 1 1 
   2 2 
   2 2 2 2 2 2 2 2 2 2 2 2 2 
   2 2 2 2 
   1 
   2 2 2 2 2 
   2 2 2 
   2 2 
   2 2 2 2 
   2 
   1 1 2 1 2 1
   )

 )

;(gc)

(s7-version)
(exit)

