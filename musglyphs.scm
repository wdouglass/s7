;; this file loads the music symbol glyphs in cmn-glyphs.lisp (from the cmn package),
;;; each function in that package becomes a Scheme function of the form:
;;;   name x y size style snd chn ax
;;;   style: #t for lines, #f (default) for filled polygon
;;;
;;; although Snd based here, all this file needs externally are draw-lines, draw-dot, and fill-polygon

;;; currently this works only in Motif

(provide 'snd-musglyphs.scm)

(define (make-polygon args)
  (let set-vals ((vects args)
		 (start 0)
		 (vals (make-vector (let total-length ((vects args) (len 0))
				      (cond ((null? vects)         len)
					    ((vector? (car vects)) (total-length (cdr vects) (+ len (length (car vects)))))
					    ((car vects)           (total-length (cdr vects) (+ len 1)))
					    (else                  (total-length (cdr vects) len)))))))
    (cond ((null? vects) vals)
	  ((vector? (car vects))
	   (let ((vect (car vects)))
	     (do ((len (length vect))
		  (i 0 (+ 1 i)))
		 ((= i len) (set-vals (cdr vects) (+ start len) vals))
	       (set! (vals (+ start i)) (vect i)))))
	  ((car vects)
	   (set! (vals start) (car vects))
	   (set-vals (cdr vects) (+ start 1) vals))
	  (else (set-vals (cdr vects) start vals)))))

(define (make-bezier-1 x0 y0 x1 y1 x2 y2 x3 y3 n)
  ;; creates a line-segment approximation of a bezier curve: n = number of segments
  ;; this is built into Snd as make-bezier, but I wanted an all Scheme version
  (let ((cx (* 3 (- x1 x0)))
	(cy (* 3 (- y1 y0))))
    (let ((bx (- (* 3 (- x2 x1)) cx))
	  (by (- (* 3 (- y2 y1)) cy)))
      (let ((ax (- x3 x0 cx bx))
	    (ay (- y3 y0 cy by))
	    (incr (/ 1.0 n))
	    (pts (make-vector (* 2 (+ n 1)))))
	(set! (pts 0) x0)
	(set! (pts 1) y0)
	(do ((i 0 (+ 1 i))
	     (val incr (+ val incr)))
	    ((> i n) pts)
	  (set! (pts (* i 2)) (floor (+ x0 (* val (+ cx (* val (+ bx (* val ax))))))))
	  (set! (pts (+ (* i 2) 1)) (floor (+ y0 (* val (+ cy (* val (+ by (* val ay)))))))))))))

;; pass our Snd context into the graphics procedures (there's probably a cleaner way)
(define ps-snd 0)
(define ps-chn 0)
(define ps-ax 0)
(define yoff 0)
(define xoff 0)

;; now make a little Postscript interpreter...
(define curx 0)  ; the Postscript "path" handlers
(define cury 0)
(define pathlist ())
(define ps-size 50)
(define bezier-segments 50)
(define (->x x) 
  (floor (+ xoff (* ps-size x))))
(define (->y y) 
  ;; Postscript is right side up, X is upside-down
  (floor (- yoff (* ps-size y))))

;; now functions and macros to make it possible to load the original Common Lisp glyphs directly

(define (moveto score x y)
  (set! curx x)
  (set! cury y)
  #f)

(define (rmoveto score x y)
  (set! curx (+ curx x))
  (set! cury (+ cury y))
  #f)

(define (curveto score x0 y0 x1 y1 x2 y2)
  (set! pathlist (cons (make-bezier-1 (->x curx) (->y cury) (->x x0) (->y y0) 
				      (->x x1) (->y y1) (->x x2) (->y y2) 
				      bezier-segments)
			 pathlist))
  (set! curx x2)
  (set! cury y2)
  #f)
  
(define (lineto score x y)
  (let ((v (make-vector 2)))
    (set! (v 0) (->x x))
    (set! (v 1) (->y y))
    (set! curx x)
    (set! cury y)
    (set! pathlist (cons v pathlist))
    #f))

(define (rlineto score x y)
  (let ((v (make-vector 2)))
    (set! curx (+ curx x))
    (set! cury (+ cury y))
    (set! (v 0) (->x curx))
    (set! (v 1) (->y cury))
    (set! pathlist (cons v pathlist))
    #f))

(if (provided? 'snd-gtk)
    (begin
      (define* (fill-in score :rest args)
	(if (pair? pathlist)
	    (let ((cr (make-cairo (car (channel-widgets ps-snd ps-chn)))))
	      (fill-polygon
	       (make-polygon
		(reverse pathlist))
	       ps-snd ps-chn ps-ax cr)
	      (free-cairo cr)))
	(set! pathlist ())
	#f)
      
      (define (draw score arg)
	(if (pair? pathlist)
	    (let ((cr (make-cairo (car (channel-widgets ps-snd ps-chn)))))
	      (draw-lines
	       (make-polygon
		(reverse pathlist))
	       ps-snd ps-chn ps-ax cr)
	      (free-cairo cr)))
	(set! pathlist ())
	#f)
      
      (define (circle score x0 y0 rad . rest)
	(let ((cr (make-cairo (car (channel-widgets ps-snd ps-chn)))))
	  (draw-dot (->x x0) (->y y0) 
		    (floor (* ps-size rad 2))
		    ps-snd ps-chn ps-ax cr)
	  (free-cairo cr)))

      (define (fill-rectangle-1 . args)
	(let ((cr (make-cairo (car (channel-widgets ps-snd ps-chn))))
	      (new-args (copy args (make-list 9 #f))))
	  (if (not (new-args 6))
	      (set! (new-args 6) time-graph))
	  (set! (new-args 8) cr)
	  (apply fill-rectangle new-args)
	  (free-cairo cr))))
    (begin
      (define* (fill-in score :rest args)
	(if (not (null? pathlist))
	    (fill-polygon
	     (make-polygon
	      (reverse pathlist))
	     ps-snd ps-chn ps-ax))
	(set! pathlist ())
	#f)
      
      (define (draw score arg)
	(if (not (null? pathlist))
	    (draw-lines
	     (make-polygon
	      (reverse pathlist))
	     ps-snd ps-chn ps-ax))
	(set! pathlist ())
	#f)
      
      (define (circle score x0 y0 rad . rest)
	(draw-dot (->x x0) (->y y0) 
		  (floor (* ps-size rad 2))
		  ps-snd ps-chn ps-ax))

      (define fill-rectangle-1 fill-rectangle)
      ))


;(define-macro (defvar name value) `(define ,name ,value))
(define defvar define)

;(define-macro (setf a b) `(set! ,a ,b))
;(define-macro setf set!)
(define setf set!)

(define-macro (defun name ignored-args . body)
  ;; in cmn-glyphs every defun has two args, the "score" and an optional "style"
  `(define ,name (lambda args 
		   ((lambda (score style) ; needed by the procedure body
		      (let ((arglen (length args))
			    (style #f))
			(if (> arglen 0) (set! xoff (list-ref args 0)))
			(if (> arglen 1) (set! yoff (list-ref args 1)))
			(if (> arglen 2) (set! ps-size (list-ref args 2)))
			(if (> arglen 3) (set! style (list-ref args 3)))
			(if (> arglen 4) (set! ps-snd (list-ref args 4)))
			(if (> arglen 5) (set! ps-chn (list-ref args 5)))
			(if (> arglen 6) (set! ps-ax (list-ref args 6)))
			,@body))
		    #f #f))))

(define t #t)
(define-macro (in-package name) #f)
(define (music-font score) #f)
(define g-mustext (lambda args #f))
(define (output-type score) #f)
;(if (defined? 'declare) (define old-declare declare) (define old-declare #f))
;(define (declare args) #f)
(define sound-comment comment)

;(define comment
;  (dilambda
;   (lambda* ((scr #f) (msg 0))
;     "(comment (scr #f) (msg 0)) tries to make musglyph.scm safe for comments"
;     (if (or (number? msg)
;	     (not scr))
;	 (sound-comment scr)))
;   (lambda* (snd (val #f))
;     (if (not val)
;	 (apply (setter sound-comment) (list #f snd))
;	 (apply (setter sound-comment) (list snd val))))))

(define progn begin)

(load "cmn-glyphs.lisp")

;;; a portion of CMN here to make it easier to display notes

(define frequency->note-octave-and-accidental
  (let ((frequency->pitch 
	 (lambda (freq)
	   (floor (* 12 (+ (log (/ freq 16.351) 2) 
			   (/ 1.0 24)))))))
    (lambda (freq)
      (let* ((pitch (frequency->pitch freq))
	     (pclass (modulo pitch 12)))
	(let ((octave (floor (/ pitch 12)))
	      (cclass (case pclass
			((0 1) 0)   ; c-sharp
			((2) 1) 
			((3 4) 2)   ; e-flat
			((5 6) 3)   ; f-sharp
			((7) 4) 
			((8 9) 5)   ; a-flat
			(else 6)))) ; b-flat
	  (list pclass octave 
		(if (memv pclass '(1 6))
		    :sharp
		    (and (memv pclass '(3 8 10)) 
			 :flat))
		cclass
		pitch))))))

(define note-data->pclass car)
(define note-data->octave cadr)
(define note-data->accidental caddr)
(define note-data->cclass cadddr)
(define (note-data->pitch val) (list-ref val 4))

(if (provided? 'snd-gtk)
    (define (draw-staff x0 y0 width line-sep)
      (do ((cr (make-cairo (car (channel-widgets ps-snd ps-chn))))
	   (line 0 (+ 1 line))
	   (x x0) 
	   (y y0 (+ y (floor line-sep))))
	  ((= line 5)
	   (free-cairo cr))
	(draw-line x y (floor (+ x width)) y ps-snd ps-chn time-graph cr)))
    (define (draw-staff x0 y0 width line-sep)
      (do ((line 0 (+ 1 line))
	   (x x0) 
	   (y y0 (+ y (floor line-sep))))
	  ((= line 5))
	(draw-line x y (floor (+ x width)) y))))


(define treble-tag-y 30)
(define bass-tag-y (+ treble-tag-y (* 4 *mix-waveform-height*)))


(define (draw-a-note freq dur x0 ty0 size with-clef treble)
  (let* ((note-data (frequency->note-octave-and-accidental freq))
	 (accidental (note-data->accidental note-data))
	 (cclass (note-data->cclass note-data))
	 (octave (note-data->octave note-data))
	 (y0 (if treble treble-tag-y bass-tag-y)))
    (let ((line-sep (* size .250))
	  (line  (- (if treble 
			(+ (* (- 5 octave) 7) 3)
			(+ (* (- 3 octave) 7) 5))
		    cclass))
	  (notehead-x x0)
	  (notehead-y y0))
      
      (let ((width (* (+ (if with-clef (if treble 0.9 1.2) 0.75)
			 (if accidental 0.1 0.0) 
			 (if (< dur .8) 0.5 0.0)) 
		      size)))
	(draw-staff x0 y0 width line-sep))
      
      (if with-clef
	  (begin
	    (if treble 
		(draw-treble-clef x0 (+ y0 (* size .76)) size)
		(draw-bass-clef (+ x0 (* size .075)) (+ y0 (* size .26)) size))
	    (set! x0 (+ x0 (* size .8))))
	  (set! x0 (+ x0 (* size (if accidental .1 .25)))))
      
      ;; accidental
      (if accidental
	  (begin
	    ((if (eq? accidental :sharp) draw-sharp draw-flat) x0 (+ y0 (* .02 size) (* line-sep 0.5 line)) size)
	    (set! x0 (+ x0 line-sep))))
      
      ;; notehead
      (set! notehead-y (+ y0 (* .02 size) (* line-sep 0.5 line)))
      (set! notehead-x x0)
      ((if (< dur 1.5)
	   draw-quarter-note
	   (if (< dur 3.5)
	       draw-half-note
	       draw-whole-note))
       notehead-x notehead-y size)
      
      ;; leger line(s)
      (if (> line 9)
	  (do ((i 10 (+ i 2)))
	      ((>= i line))
	    (fill-rectangle-1 (floor (- x0 (* .1 size)))
			      (floor (+ y0 (* -.02 size) (* line-sep 0.5 i)))
			      (floor (* .5 size))
			      (floor (* .05 size)))))
      (if (< line 0)
	  (do ((i -2 (- i 2)))
	      ((< i line))
	    (fill-rectangle-1 (floor (- x0 (* .1 size)))
			      (floor (+ y0 (* -.02 size) (* line-sep 0.5 i)))
			      (floor (* .5 size))
			      (floor (* .05 size)))))
      
      ;; stem
      (if (< dur 3)
	  (fill-rectangle-1
	   (if (> line 3) ; stem up
	       (values (floor (+ x0 line-sep))
		       (floor (+ y0 (* 0.02 size) (* size -0.8) (* line-sep 0.5 line))))
	       (values (floor (- x0 (* size 0.02)))
		       (floor (+ y0 (* line-sep line 0.5)))))
	   (floor (* size 0.05))
	   (floor (* size 0.8))))
      
      ;; flags
      (if (< dur .6)
	  (let ((base (+ y0 (* line-sep 0.5 line))))
	    (if (> line 2)
		(draw-8th-flag-up (+ x0 line-sep) (+ base (* size -0.6)) size)
		(draw-8th-flag-down x0 (+ base (* .7 size)) size))
	    (if (< dur .3)
		(begin
		  (if (> line 2)
		      (draw-extend-flag-up (+ x0 line-sep) (+ base (* size -0.8)) size)
		      (draw-extend-flag-down x0 (+ base (* .9 size)) size))
		  (if (< dur .15)
		      (if (> line 2)
			  (draw-extend-flag-up (+ x0 line-sep) (+ base (* size -1.0)) size)
			  (draw-extend-flag-down x0 (+ base (* 1.1 size)) size)))))))
      (list notehead-x notehead-y))))
  

#|
;; this is the example in the documentation

(define (draw-mix-tag id ox oy x y)
  (let ((width *mix-tag-width*)
	(height *mix-tag-height*)
	(home (mix-home id)))
    (if (not (= oy -1)) ; already erased?
	(fill-rectangle-1 (- ox 1 (/ width 2)) (- oy 1 height) (+ width 2) (+ height 2) (car home) (cadr home) time-graph #t))
    (fill-rectangle-1 (- x (/ width 2)) (- y height) width height (car home) (cadr home))))

(hook-push draw-mix-hook
	   (lambda (hook)
	     (let ((id (hook 'id))
		   (ox (hook 'old-x))
		   (oy (hook 'old-y))
		   (x (hook 'x))
		   (y (hook 'y)))
	       (draw-mix-tag id ox oy x y)
	       (draw-a-note (or (mix-property 'frequency id) 440.0)
			    (/ (length id) (srate))
			    x y
			    (* 2 *mix-waveform-height*)
			    #f
			    (eq? (mix-property 'instrument id) 'violin)))
	     (set! (hook 'result) #t)))

(hook-push after-graph-hook
	   (lambda (hook)
	     (let ((size (* 2 *mix-waveform-height*)))
	       (draw-staff 0 treble-tag-y (* 1.0 size) (* .25 size))
	       (draw-staff 0 bass-tag-y (* 1.0 size) (* .25 size))
	       (draw-treble-clef 0 (+ treble-tag-y (* size .76)) size)
	       (draw-bass-clef (* size .075) (+ bass-tag-y (* size .26)) size))))

(set! *mix-waveform-height* 20)

(let ((oldie (find-sound "test.snd")))
  (if (sound? oldie)
      (close-sound oldie)))

(let ((index (new-sound "test.snd" :channels 1)))
  
  (define (violin beg dur freq amp)
    (let* ((frq-hz (* 2 (->frequency freq #t)))
	   (id (car (mix (with-temp-sound () (fm-violin 0 dur frq-hz amp))
			 (->sample beg) 0 index 0  ; start, file in-chan, sound, channel
			 #t #t))))                  ; with tag and auto-delete
      (set! (mix-property 'frequency id) frq-hz)
      (set! (mix-property 'instrument id) 'violin)))
  
  (define (cello beg dur freq amp)
    (let* ((frq-hz (* 2 (->frequency freq #t)))
	   (id (car (mix (with-temp-sound () (fm-violin 0 dur frq-hz amp :fm-index 1.5))
			 (->sample beg) 0 index 0
			 #t #t))))
      (set! (mix-property 'frequency id) frq-hz)
      (set! (mix-property 'instrument id) 'cello)))
  
  (as-one-edit
   (lambda ()
     (violin 0 1 'e4 .2)  (violin 1 1.5 'g4 .2)  (violin 2.5 .5 'g3 .2)
     (cello  0 1 'c3 .2)  (cello  1 1.5 'e3 .2)  (cello  2.5 .5 'g2 .2)
     
     (violin 3 3 'f4 .2)
     (cello  3 3 'd3 .2)
     
     (violin 6 1 'e4 .2)   (violin 7 1 'g3 .2)   (violin 8 1 'e4 .2)
     (cello  6 1 'c3 .2)   (cello  7 1 'g2 .2)   (cello  8 1 'c3 .2)
     
     (violin 9 3 'd4 .2)
     (cello  9 3 'b2 .2)
     
     (violin 12 1 'f4 .2)  (violin 13 1.5 'a4 .2)  (violin 14.5 .5 'g3 .2)
     (cello  12 1 'd3 .2)  (cello  13 1.5 'f3 .2)  (cello  14.5 .5 'g2 .2)
     
     (violin 15 3 'g4 .2)
     (cello  15 3 'e3 .2)
     
     (violin 18 1 'f4 .2)  (violin 19 1 'g3 .2)  (violin 20 1 'f4 .2)
     (cello  18 1 'd3 .2)  (cello  19 1 'g2 .2)  (cello  20 1 'd3 .2)
     
     (violin 21 3 'e4 .2)
     (cello  21 3 'c3 .2))))
|#

#|
;;; here is the same example, but with vertical drag interpreted as pitch change, and the
;;;   note head itself is the mix tag:

(set! *show-mix-waveforms* #f)

(hook-push draw-mix-hook
	   (lambda (hook)
	     (let* ((id (hook 'id))
		    (x (hook 'x))
		    (y (hook 'y))
		    (xy (draw-a-note (or (mix-property 'frequency id) 440.0)
				     (/ (length id) (srate))
				     x y
				     (* 2 *mix-waveform-height*)
				     #f
				     (eq? (mix-property 'instrument id) 'violin)))
		    (note-x (round (car xy)))
		    (note-y (round (- (cadr xy) *mix-tag-height*))))
	       (if (not (mix-property 'original-tag-y id))
		   (begin
		     (set! (mix-property 'original-frequency id) (mix-property 'frequency id))
		     (set! (mix-property 'original-tag-y id) note-y)
		     (set! (mix-property 'interval id) 0)))
	       (set! (hook 'result) (list note-x note-y)))))

(hook-push after-graph-hook
	   (lambda (hook)
	     (let ((size (* 2 *mix-waveform-height*)))
	       (draw-staff 0 treble-tag-y (* 1.0 size) (* .25 size))
	       (draw-staff 0 bass-tag-y (* 1.0 size) (* .25 size))
	       (draw-treble-clef 0 (+ treble-tag-y (* size .76)) size)
	       (draw-bass-clef (* size .075) (+ bass-tag-y (* size .26)) size))))

(hook-push mix-drag-hook
	   (lambda (hook)
	     (let* ((n (hook 'id))
		    (x (hook 'x))
		    (y (hook 'y))
		    (orig-y (mix-property 'original-tag-y n)))
	       (if orig-y
		   (let ((interval (round (/ (* 12 (- (+ *mix-tag-height* orig-y) y))
					     (* 2 *mix-waveform-height*))))
			 (current-interval (mix-property 'interval n)))
		     ;; this gives the number of semitones we have drifted
		     (if (not (= current-interval interval))
			 (begin
			   (set! (mix-property 'interval n) interval)
			   (set! (mix-property 'frequency n) (* (mix-property 'original-frequency n)
								(expt 2.0 (/ interval 12.0)))))))))))
	       
(hook-push mix-release-hook
	   (lambda (hook)
	     (let ((n (hook 'id))
		   (samps (hook 'samples)))
	       (as-one-edit
		(lambda ()
		  (set! (mix-position id) (+ samps (mix-position id)))
		  (let ((interval (mix-property 'interval id)))
		    (if (not (= interval 0))
			(let ((last-interval (mix-property 'last-interval id)))
			  (if (or (not last-interval)
				  (not (= last-interval interval)))
			      (set! (mix-speed id) (expt 2.0 (/ interval 12.0))))))
		    (set! (mix-property 'last-interval id) interval))))
	       (set! (hook 'result) #t))))

(set! *mix-waveform-height* 20)

(let ((oldie (find-sound "test.snd")))
  (if (sound? oldie)
      (close-sound oldie)))

(let ((violin-sync 1)
      (violin-color (make-color 0 0 0.85)) ; blueish
      (cello-sync 2)
      (cello-color (make-color 1 .7 0)) ; orangeish
      (index (new-sound "test.snd" :channels 1))) ; :size (* 22050 22))))
  
  (define (violin beg dur freq amp)
    (let* ((frq-hz (* 2 (->frequency freq #t)))
	   (id (car (mix (with-temp-sound () (fm-violin 0 dur frq-hz amp))
			 (->sample beg) 0 index 0  ; start, file in-chan, sound, channel
			 #t #t))))                  ; with tag and auto-delete
      (set! (mix-property 'frequency id) frq-hz)
      (set! (mix-property 'instrument id) 'violin)
      (set! (mix-sync id) violin-sync)
      (set! (mix-color id) violin-color)))
  
  (define (cello beg dur freq amp)
    (let* ((frq-hz (* 2 (->frequency freq #t)))
	   (id (car (mix (with-temp-sound () (fm-violin 0 dur frq-hz amp :fm-index 1.5))
			 (->sample beg) 0 index 0
			 #t #t))))
      (set! (mix-property 'frequency id) frq-hz)
      (set! (mix-property 'instrument id) 'cello)
      (set! (mix-sync id) cello-sync)
      (set! (mix-color id) cello-color)))
  
  (as-one-edit
   (lambda ()
     (violin 0 1 'e4 .2)  (violin 1 1.5 'g4 .2)  (violin 2.5 .5 'g3 .2)
     (cello  0 1 'c3 .2)  (cello  1 1.5 'e3 .2)  (cello  2.5 .5 'g2 .2)
     
     (violin 3 3 'f4 .2)
     (cello  3 3 'd3 .2)
     
     (violin 6 1 'e4 .2)   (violin 7 1 'g3 .2)   (violin 8 1 'e4 .2)
     (cello  6 1 'c3 .2)   (cello  7 1 'g2 .2)   (cello  8 1 'c3 .2)
     
     (violin 9 3 'd4 .2)
     (cello  9 3 'b2 .2)
     
     (violin 12 1 'f4 .2)  (violin 13 1.5 'a4 .2)  (violin 14.5 .5 'g3 .2)
     (cello  12 1 'd3 .2)  (cello  13 1.5 'f3 .2)  (cello  14.5 .5 'g2 .2)
     
     (violin 15 3 'g4 .2)
     (cello  15 3 'e3 .2)
     
     (violin 18 1 'f4 .2)  (violin 19 1 'g3 .2)  (violin 20 1 'f4 .2)
     (cello  18 1 'd3 .2)  (cello  19 1 'g2 .2)  (cello  20 1 'd3 .2)
     
     (violin 21 3 'e4 .2)
     (cello  21 3 'c3 .2)
     
     index)))
|#

