;(set! (*s7* 'gc-stats) #t)

(define (safe-divide a b)
  "divide but check for 0 denom"
  (if (zero? b)
      a
      (/ a b)))

(define (real-time) 
  (* 1.0 (/ (get-internal-real-time) internal-time-units-per-second)))

(define (hundred n) 
  (round (* 1000 n)))

(define-macro (time-it a) 
  `(begin (gc) 
     (let ((start (real-time))) 
       ,a 
       (- (real-time) start))))

(define size 1000000)

(define (test-do)
  (do ((j 0 (+ j 1))) ((= j 1))
    (do ((i 0 (+ i 1)))
	((= i size))
      i)))

(let ((do1 (time-it (test-do))))
  (format *stderr* "~15T~A" (hundred do1)))

(define (test-*1)
  (let ((x 1.0))
    (do ((j 0 (+ j 1))) ((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! x (+ x (* i 2.0)))))))

(let ((do1 (time-it (test-*1))))
  (format *stderr* "~5T~A" (hundred do1)))


(define (test-osc)
  (let ((vals (make-float-vector size))
	(osc (make-oscil :frequency 440))
	(e1 (make-env '(0 0 1 1 2 0) :length size)))
    (set! *output* vals)
    (do ((i 0 (+ i 1)))
	((= i size))
      (outa i (* (env e1) (oscil osc))))
    (set! *output* #f)))

(let ((do1 (time-it (test-osc))))
  (format *stderr* "~5T~A" (hundred do1)))


(load "ws.scm")
;;; from chk.scm
;(set! *clm-file-buffer-size* (* 8 1024 1024))
(set! *to-snd* #f)

;(set! *clm-file-name* "/dev/null")

(define calls ())
(define last-caller #f)
;(set! *clm-notehook* (lambda args (set! calls (cons (list (report_env_1) last-caller) calls)) (set! last-caller (car args))))

(load "v.scm")

(define (test-fmv)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble)
    (fm-violin 0 20 440 .1)))

(let ((do1 (time-it (test-fmv))))
  (format *stderr* "~4T~A" (hundred do1)))

(define (test-fm2)
  (with-sound (:channels 2 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (fm-violin 0 20 440 .1 :degree 45)))

(let ((do1 (time-it (test-fm2))))
  (format *stderr* "~4T~A" (hundred do1)))


(define (test-fm1000)
  (with-sound (:statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (do ((i 0 (+ i 1))) ((= i 10000)) (fm-violin (* i .001) .01 440 .001))))

(let ((do1 (time-it (test-fm1000))))
  (format *stderr* "~4T~A" (hundred do1)))


(load "jcrev.scm")

(define (test-fmj)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble :reverb jc-reverb) 
    (fm-violin 0 20 440 .1)))

(let ((do1 (time-it (test-fmj))))
  (format *stderr* "~4T~A" (hundred do1)))


(load "nrev.scm")

(define (test-fmn)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble :reverb nrev) 
    (fm-violin 0 20 440 .1)))

(let ((do1 (time-it (test-fmn))))
  (format *stderr* "~4T~A" (hundred do1)))


(load "expandn.scm")
(if (not (file-exists? "oboe.snd"))
    (system "cp ~/cl/oboe.snd ."))

(define (test-exp)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (expandn 0 10 "oboe.snd" 1 :expand 4)))

(let ((do1 (time-it (test-exp))))
  (format *stderr* "~5T~A" (hundred do1)))


(load "singer.scm")

(define (test-sing)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (singer 0 .1 
	    (list (list .4 ehh.shp test.glt 523.0 .8 0.0 .01) 
		  (list .6 oo.shp test.glt 523.0 .7 .1 .01)))))

(let ((do1 (time-it (test-sing))))
  (format *stderr* "~4T ~A" (hundred do1)))


(load "grani.scm")

(define (test-gran)
  (with-sound (:channels 2 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (grani 0 1 .5 "oboe.snd" 
	   :grain-envelope '(0 0 0.2 0.2 0.5 1 0.8 0.2 1 0))))

(let ((do1 (time-it (test-gran))))
  (format *stderr* "~5T~A" (hundred do1)))


(load "fullmix.scm")
(if (not (file-exists? "pistol.snd"))
    (system "cp ~/cl/pistol.snd ."))
(if (not (file-exists? "2.snd"))
    (system "cp ~/cl/2.snd ."))

(define (test-full)
  (with-sound (:channels 2 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (fullmix "pistol.snd" 0 2 0 #f .5)  
    (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))))))

(let ((do1 (time-it (test-full))))
  (format *stderr* "~4T~A" (hundred do1)))


(define (test-fuller)
  (with-sound (:channels 2 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (fullmix "pistol.snd")
    (fullmix "2.snd" .5 1)
    (fullmix "2.snd" 1.5 1 0 #f 2.0)
    (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))))
    (fullmix "pistol.snd" 2 1 0 #f .5)
    (fullmix "2.snd" 0 2 0 (list (list .1 .2) (list .3 .4)) 2.0)
    (fullmix "oboe.snd" 3 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))) .25)
    (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	  (e0->1 (make-env '(0 1 1 0) :duration 2))
	  (e1->0 (make-env '(0 1 1 0) :duration 2))
	  (e1->1 (make-env '(0 0 1 1) :duration 2)))
      (fullmix "2.snd" 4 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1))))
    (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	  (e0->1 (make-env '(0 1 1 0) :duration 2))
	  (e1->0 (make-env '(0 1 1 0) :duration 2))
	  (e1->1 (make-env '(0 0 1 1) :duration 2)))
      (fullmix "2.snd" 6 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)) 2.0))))

(let ((do1 (time-it (test-fuller))))
  (format *stderr* "~4T ~A" (hundred do1)))



(load "clm-ins.scm")

(define (test-pins)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (pins 0 3 "oboe.snd" 1.0 :max-peaks 8)))

(let ((do1 (time-it (test-pins))))
  (format *stderr* "~5T ~A" (hundred do1)))



(if (not (file-exists? "popi.scm"))
    (system "cp ~/cl/popi.scm ."))

(define (test-popi)
  (load "popi.scm"))

(let ((do1 (time-it (test-popi))))
  (format *stderr* "~5T~A" (hundred do1)))



;(set! *clm-file-buffer-size* (* 64 1024 1024))

(load "piano.scm")

(define (test-piano)
  (with-sound (:channels 1 :statistics #f :to-snd #f :play #f :sample-type mus-ldouble) 
    (p 0 3)))

(let ((do1 (time-it (test-piano))))
  (format *stderr* "~5T~A" (hundred do1)))


(set! *clm-sample-type* mus-ldouble)
(load "animals.scm") 

(define test-bird calling-all-animals)

(let ((do1 (time-it (test-bird))))
  (format *stderr* "~4T~A~%" (hundred do1)))


;(for-each
; (lambda (n)
;   (format #t "~A~%" n))
; (sort! calls (lambda (a b) (< (car a) (car b)))))

(when (> (*s7* 'profile) 0)
  (show-profile 200))
(#_exit)


#|
/home/bil/snd-12/ ./snd --version
This is Snd version 12.13 of 6-July-12:

/home/bil/snd-12/ ./snd t502.scm
times are in milliseconds
do:        15
float-vector-set:   38
float-vector ind:   59
vset:      41
*1:        52
osc:       116
fmv:       145, C: 107
fm2:       159
fm1000:    834
fmj:       388
fmn:       398
expand:    588
sing:      1853
gran:      57
full:      53
fuller:    477
pins:      1279
popi:      1727
piano:     10315
bird:      8928
|#
