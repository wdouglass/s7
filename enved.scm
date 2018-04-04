;;; use the lisp graph section of the display as an envelope editor
;;;
;;; (start-enveloping) sets this in progress (for subsequently opened sounds)
;;; (stop-enveloping) turns it off
;;; (channel-envelope snd chn) returns the current envelope associated with snd's channel chn
;;; (set! (channel-envelope snd chn) env) sets it to a new list of breakpoints
;;;
;;; (play-with-envs snd) sets channel amps during playback from the associated enved envelopes
;;; (play-panned snd) pans a mono sound following its enved envelope into a stereo sound

(provide 'snd-enved.scm)

(define channel-envelope
  (dilambda
   (let ((+documentation+ "(channel-envelope snd chn) returns the current enved envelope associated with snd's channel chn"))
     (lambda (snd chn)
       (or (channel-property 'enved-envelope snd chn)
	   ())))
   (lambda (snd chn new-env)
     (set! (channel-property 'enved-envelope snd chn) new-env)
     (graph new-env "" 0.0 1.0 0.0 1.0 snd chn))))


(define (create-initial-envelopes hook)
  (let ((snd (hook 'snd)))
    (do ((i 0 (+ i 1)))
	((= i (channels snd)))
      (set! (dot-size snd i) 8)
      (set! (channel-envelope snd i) (list 0.0 1.0 1.0 1.0)))))


;;; if click on point, delete,
;;; if click/drag on point, move it until release
;;; if click off point, add (and subsequent drag)

(define mouse-down 0)
(define mouse-up 0)
(define click-time (/ internal-time-units-per-second 10.0)) ; sets whether a mouse motion is a click or a drag
(define mouse-pos 0)
(define mouse-new #f)

(define mouse-press-envelope 
  (let ((add-envelope-point 
	 (lambda (x y cur-env)
	   (let ((new-env ()))
	     (let search-point ((e cur-env))
	       (cond ((null? e) (append new-env (list x y)))
		     ((= (car e) x) (append new-env (list x y) (cddr e)))
		     ((> (car e) x) (append new-env (list x y) e))
		     (else
		      (set! new-env (append new-env (list (car e) (cadr e))))
		      (search-point (cddr e))))))))
	
	(envelope-position 
	 (lambda (x cur-env)
	   (do ((e cur-env (cddr e))
		(pos 0 (+ pos 2)))
	       ((= (car e) x) 
		pos)))))
    
    (letrec ((on-dot? 
	      (let ((mouse-radius .03))
		(lambda (x y cur-env pos)
		  (and (pair? cur-env)
		       (pair? (cdr cur-env))
		       (or (and (< (abs (- (car cur-env) x)) mouse-radius)
				(< (abs (- (cadr cur-env) y)) mouse-radius)
				pos)
			   (on-dot? x y (cddr cur-env) (+ pos 2))))))))

      (lambda (hook)
	(let ((snd (hook 'snd))
	      (chn (hook 'chn))
	      (ux (hook 'x))
	      (uy (hook 'y)))
	  
	  (let* ((x (max 0.0 (min ux 1.0)))
		 (y (max 0.0 (min uy 1.0)))
		 (cur-env (channel-envelope snd chn))
		 (pos (on-dot? x y cur-env 0)))
	    (set! mouse-new (not pos))
	    (set! mouse-down (get-internal-real-time))
	    (if pos
		(set! mouse-pos pos)
		(let ((new-x (max 0.001 (min x .999))))
		  (set! (channel-envelope snd chn) 
			(add-envelope-point new-x y cur-env))
		  (set! mouse-pos (envelope-position new-x (channel-envelope snd chn)))))))))))


(define (mouse-drag-envelope hook)
  ;; point exists, needs to be edited with check for various bounds
  (let ((snd (hook 'snd))
	(chn (hook 'chn))
	(x (hook 'x))
	(y (hook 'y)))
    (let ((cur-env (channel-envelope snd chn)))
      (let ((lx (if (= mouse-pos 0)
		    0.0
		    (if (>= mouse-pos (- (length cur-env) 2))
			1.0
			(max (+ (list-ref cur-env (- mouse-pos 2)) .001)
			     (min x
				  (- (list-ref cur-env (+ mouse-pos 2)) .001))))))
	    (ly (max 0.0 (min y 1.0))))
	(set! (channel-envelope snd chn) 
	      (do ((new-env ())
		   (e cur-env (cddr e))
		   (npos 0 (+ npos 2)))
		  ((= npos mouse-pos) 
		   (append new-env (list lx ly) (cddr e)))
		(set! new-env (append new-env (list (car e) (cadr e))))))
	(update-lisp-graph snd chn)))))


(define (mouse-release-envelope hook)
      (let ((snd (hook 'snd))
	    (chn (hook 'chn))
	    (axis (hook 'axis)))
	(when (= axis lisp-graph)
	  (let ((cur-env (channel-envelope snd chn)))
	    (set! mouse-up (get-internal-real-time))
	    (if (not (or mouse-new
			 (> (- mouse-up mouse-down) click-time)
			 (= mouse-pos 0)
			 (>= mouse-pos (- (length cur-env) 2))))
		(set! (channel-envelope snd chn)
		      (let ((new-env ()))
			(let search-point ((e cur-env)
					   (npos 0))
			  (if (null? e)
			      new-env
			      (if (= mouse-pos npos)
				  (append new-env (cddr e))
				  (begin
				    (set! new-env (append new-env (list (car e) (cadr e))))
				    (search-point (cddr e) (+ npos 2))))))))))
	  (update-lisp-graph snd chn)
	  (set! mouse-new #f)
	  (set! (hook 'result) #t))))


(define (enveloping-key-press hook)
  (let ((snd (hook 'snd))
	(chn (hook 'chn))
	(key (hook 'key))
	(state4 (= (hook 'state) 4)))
    
    ;; C-g returns to original env
    ;; C-. applies current env to amplitude
    (if (and (= key (char->integer #\.))
	     state4)
	(begin
	  (env-channel (channel-envelope snd chn) 0 (framples snd chn) snd chn)
	  (set! (hook 'result) #t))
	(if (and (= key (char->integer #\g))
		 state4)
	    (begin
	      (set! (channel-envelope snd chn) '(0.0 1.0 1.0 1.0))
	      (set! (hook 'result) #t))))))

(define start-enveloping
  (let ((+documentation+ "(start-enveloping) starts the enved processes, displaying an envelope editor in each channel"))
    (lambda ()
      (hook-push after-open-hook create-initial-envelopes)
      (hook-push mouse-press-hook mouse-press-envelope)
      (hook-push mouse-drag-hook mouse-drag-envelope)
      (hook-push mouse-click-hook mouse-release-envelope)
      (hook-push key-press-hook enveloping-key-press))))

(define stop-enveloping
  (let ((+documentation+ "(stop-enveloping) turns off the enved channel-specific envelope editors"))
    (lambda ()
      (hook-remove after-open-hook create-initial-envelopes)
      (hook-remove mouse-press-hook mouse-press-envelope)
      (hook-remove mouse-drag-hook mouse-drag-envelope)
      (hook-remove mouse-click-hook mouse-release-envelope)
      (hook-remove key-press-hook enveloping-key-press))))


;;; --------------------------------------------------------------------------------
;;;
;;; some examples of using this envelope editor

(define play-with-envs
  (let ((+documentation+ "(play-with-envs snd) sets channel amps during playback from the associated enved envelopes"))
    (lambda* (sound)
      (do ((chans (channels sound))
	   (chan 0 (+ 1 chan)))
	  ((= chan chans)
	   (start-playing chans (srate sound)))   
	(let ((player (make-player sound chan))
	      (e (make-env (channel-envelope sound chan) 
			   :length (floor (/ (framples sound chan) *dac-size*)))))
	  (add-player player 0 -1 -1 (lambda (reason) (set! (hook-functions play-hook) ())))
	  (hook-push play-hook (lambda (hook)
				 ;; if dac buffer size in framples is not dac-size, we should do something debonair
				 (set! (amp-control player) (env e)))))))))

