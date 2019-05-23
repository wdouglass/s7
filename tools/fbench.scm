;;;  John Walker's Floating Point Benchmark, derived from...
;;;
;;;  Marinchip Interactive Lens Design System
;;;
;;;  By John Walker
;;;     http://www.fourmilab.ch/
;;;
;;;  This program may be used, distributed, and modified freely as
;;;  long as the origin information is preserved.
;;;
;;;  This is a complete optical design raytracing algorithm,
;;;  stripped of its user interface and recast into Common Lisp.
;;;  It not only determines execution speed on an extremely
;;;  floating point (including trig function) intensive
;;;  real-world application, it checks accuracy on an algorithm
;;;  that is exquisitely sensitive to errors.  The performance of
;;;  this program is typically far more sensitive to changes in
;;;  the efficiency of the trigonometric library routines than the
;;;  average floating point program.
;;;
;;;  Ported from the C language implementation in September 2005
;;;  by John Walker.
;;;
;;;  Ported to s7 (and Guile) 20-May-2019

(cond-expand ((not s7) (use-modules (ice-9 format)) (define s7-version exit))) ; for Guile

;;      Wavelengths of standard spectral lines in Angstroms
(define spectral-line
  '(( A-line . 7621.0 )	        ; A
    ( B-line . 6869.955 )       ; B
    ( C-line . 6562.816 )       ; C
    ( D-line . 5895.944 )       ; D
    ( E-line . 5269.557 )       ; E
    ( F-line . 4861.344 )       ; F
    ( Gprime-line . 4340.477 )  ; G'
    ( H-line . 3968.494 )))     ; H

(define D-light (cdr (assq 'D-line spectral-line)))
(define C-light (cdr (assq 'C-line spectral-line)))
(define F-light (cdr (assq 'F-line spectral-line)))

;;      The test case used in this program is the design for a 4 inch
;;      f/12 achromatic telescope objective used as the example in Wyld's
;;      classic work on ray tracing by hand, given in Amateur Telescope
;;      Making, Volume 3 (Volume 2 in the 1996 reprint edition).
(define testcase
  '(( 27.05 1.5137 63.6 0.52 )
    ( -16.68 1 0 0.138 )
    ( -16.68 1.6164 36.7 0.38 )
    ( -78.1 1 0 0 )))

(define default-iteration-count 100000)

;;      Reference results.  These happen to be derived from a
;;      run on Microsoft Quick BASIC on the IBM PC/AT.
(define reference-results
  '("   Marginal ray          47.09479120920   0.04178472683"
    "   Paraxial ray          47.08372160249   0.04177864821"
    "Longitudinal spherical aberration:        -0.01106960671"
    "    (Maximum permissible):                 0.05306749907"
    "Offense against sine condition (coma):     0.00008954761"
    "    (Maximum permissible):                 0.00250000000"
    "Axial chromatic aberration:                0.00448229032"
    "    (Maximum permissible):                 0.05306749907"))

(define current-surfaces 0)
(define paraxial 0)
(define clear-aperture 0)
(define aberr-lspher 0)
(define aberr-osc 0)
(define aberr-lchrom 0)
(define max-lspher 0)
(define max-osc 0)
(define max-lchrom 0)
(define radius-of-curvature 0)
(define object-distance 0)
(define ray-height 0)
(define axis-slope-angle 0)
(define from-index 0)
(define to-index 0)

;;      Calculate passage through surface
;;
;;      If the variable paraxial is 1, the trace through the
;;      surface will be done using the paraxial approximations.
;;      Otherwise, the normal trigonometric trace will be done.
;;
;;      This subroutine takes the following global inputs:
;;
;;      radius-of-curvature     Radius of curvature of surface
;;                              being crossed.  If 0, surface is
;;                              plane.
;;
;;      object-distance         Distance of object focus from
;;                              lens vertex.  If 0, incoming
;;                              rays are parallel and
;;                              the following must be specified:
;;
;;      ray-height              Height of ray from axis.  Only
;;                              relevant if $object-distance == 0
;;
;;      axis-slope-angle        Angle incoming ray makes with axis
;;                              at intercept
;;
;;      from-index              Refractive index of medium being left
;;
;;      to-index                Refractive index of medium being
;;                              entered.
;;
;;      The outputs are the following global variables:
;;
;;      object-distance         Distance from vertex to object focus
;;                              after refraction.
;;
;;      axis-slope-angle        Angle incoming ray makes with axis
;;                              at intercept after refraction.

(define (transit-surface)
  (let ((iang-sin 0))
    (if (= paraxial 1)
	(if (= radius-of-curvature 0)
	    (begin
	      (set! object-distance (* object-distance (/ to-index from-index)))
	      (set! axis-slope-angle (* axis-slope-angle (/ from-index to-index))))
	    (begin
	      (if (= object-distance 0)
		  (begin
		    (set! axis-slope-angle 0)
		    (set! iang-sin (/ ray-height radius-of-curvature)))
		  (set! iang-sin (* (/ (- object-distance radius-of-curvature) radius-of-curvature) axis-slope-angle)))
	      (let ((rang-sin (* (/ from-index to-index) iang-sin))
		    (old-axis-slope-angle axis-slope-angle))
		(set! axis-slope-angle (- (+ axis-slope-angle iang-sin) rang-sin))
		(if (not (= object-distance 0))
		    (set! ray-height (* object-distance old-axis-slope-angle)))
		(set! object-distance (/ ray-height axis-slope-angle)))))
	(if (= radius-of-curvature 0)
	    (let ((rang (- (asin (* (/ from-index to-index) (sin axis-slope-angle))))))
	      (set! object-distance (* object-distance (/ (* to-index (cos rang)) (* from-index (cos axis-slope-angle)))))
	      (set! axis-slope-angle (- rang)))
	    (begin
	      (if (= object-distance 0)
		  (begin
		    (set! axis-slope-angle 0)
		    (set! iang-sin (/ ray-height radius-of-curvature)))
		  (set! iang-sin (* (/ (- object-distance radius-of-curvature) radius-of-curvature) (sin axis-slope-angle))))
	      (let ((iang (asin iang-sin))
		    (rang-sin (* (/ from-index to-index) iang-sin))
		    (old-axis-slope-angle axis-slope-angle))
		(set! axis-slope-angle (+ axis-slope-angle (- iang (asin rang-sin))))
		(let ((sagitta (sin (/ (+ old-axis-slope-angle iang) 2.0))))
		  (set! sagitta (* 2 radius-of-curvature sagitta sagitta))
		  (set! object-distance (+ (/ (* radius-of-curvature (sin (+ old-axis-slope-angle iang))) (tan axis-slope-angle)) sagitta)))))))))

;;	Perform ray trace in specific spectral line
(define (trace-line line ray-h)
  (set! object-distance 0)
  (set! ray-height ray-h)
  (set! from-index 1)
  (for-each (lambda (surface)
	      (set! radius-of-curvature (car surface))
	      (set! to-index (cadr surface))
	      (if (> to-index 1)
		  (set! to-index (+ to-index (/ (* (- D-light line) (- (cadr surface) 1)) 
						(* (- C-light F-light) (caddr surface))))))
	      (transit-surface)
	      (set! from-index to-index)
	      (if (cdr surface)
		  (set! object-distance (- object-distance (cadddr surface)))))
	    testcase))

;;	Set parameters for the design to be traced
(set! clear-aperture 4)
(define clear-aperture/2 (/ clear-aperture 2))
(set! current-surfaces 4)

(define (fbench iteration-count)
  (let ((od-sa '()))
    (do ((iteration 0 (+ iteration 1)))
	((> iteration iteration-count))
      (set! od-sa '())
      (do ((jp 0 (+ jp 1)))
	  ((> jp 1))
	;;  Do main trace in D light
	(set! paraxial jp)
	(trace-line D-light clear-aperture/2)
	;; (set! od-sa (append od-sa (list (list object-distance axis-slope-angle)))) ; yikes!
	(set! od-sa (cons (list object-distance axis-slope-angle) od-sa)))
      (set! od-sa (reverse! od-sa))
      
      (set! paraxial 0)
      
      ;;	Trace marginal ray in C
      (trace-line C-light clear-aperture/2)
      (let ((od-cline object-distance))
	
	;;	Trace marginal ray in F
	(trace-line F-light clear-aperture/2)
	(set! aberr-lspher (- (caadr od-sa) (caar od-sa)))
	(set! aberr-osc (- 1 (/ (* (caadr od-sa) (cadadr od-sa)) (* (sin (cadar od-sa)) (caar od-sa)))))
	(set! aberr-lchrom (- object-distance od-cline))
	(set! max-lspher (sin (cadar od-sa)))
	  
	;;  D light
	(set! max-lspher (/ 0.0000926 (* max-lspher max-lspher)))
	(set! max-osc 0.0025)
	(set! max-lchrom max-lspher)))
    
    (let ((results
	   (list
	    (format #f "   Marginal ray          ~14,11F  ~14,11F" (caar od-sa) (cadar od-sa))
	    (format #f "   Paraxial ray          ~14,11F  ~14,11F" (caadr od-sa) (cadadr od-sa))
	    (format #f "Longitudinal spherical aberration:      ~16,11F" aberr-lspher)
	    (format #f "    (Maximum permissible):              ~16,11F" max-lspher)
	    (format #f "Offense against sine condition (coma):  ~16,11F" aberr-osc)
	    (format #f "    (Maximum permissible):              ~16,11F" max-osc)
	    (format #f "Axial chromatic aberration:             ~16,11F" aberr-lchrom)
	    (format #f "    (Maximum permissible):              ~16,11F" max-lchrom))))
      
      (if (equal? results reference-results)
	  (format #t "No errors in results.~%")
	  (do ((errors 0)
	       (received results (cdr received))
	       (expected reference-results (cdr expected))
	       (line 1 (+ line 1)))
	      ((null? received)
	       (format #t "~D error~A in results.~%" errors (if (> errors 1) "s" ""))) 
	    (if (not (equal? (car expected) (car received)))
		(begin
		  (set! errors (+ errors 1))
		  (format #t "Error in results in line ~D...~%" line)
		  (format #t "Expected: ~A~%" (car expected))
		  (format #t "Received: ~A~%" (car received)))))))))


(fbench 50000)
;(fbench 1)

(s7-version)
(exit)
