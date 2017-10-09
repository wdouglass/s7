;;; most of this file is obsolete.
;;;
;;; playing-related examples previously scattered around at random
;;;
;;; play-sound func -- play current sound, calling (func data) on each buffer if func is passed
;;; play-often, play-until-c-g -- play sound n times or until C-g is typed
;;; play-region-forever -- play region over and over until C-g typed
;;; loop-between-marks -- play while looping continuously between two movable marks
;;; start-dac -- hold DAC open and play sounds via keyboard
;;; "vector synthesis"
;;; play-with-amps -- play channels with individually settable amps
;;; play-sine and play-sines -- produce tones direct to DAC

;;; see also play-syncd-marks and play-between-marks in marks.scm

(provide 'snd-play.scm)

#|
(define* (open-play-output out-chans out-srate out-format out-bufsize)
  ;; returns (list audio-fd chans frames)
  (let* ((outchans (or out-chans 1))
	 (cur-srate (or out-srate (and (pair? (sounds)) (srate)) 22050))
	 (pframes (or out-bufsize 256))
	 (frm (or out-format (if (little-endian?) mus-lshort mus-bshort)))
	 (outbytes (* pframes 2))     ; 2 here since we'll first try to send short (16-bit) data to the DAC
	 (audio-fd 
	  ;; ALSA throws an error where the rest of the audio cases simply report failure
	  ;;   so we turn off the "error" printout, catch the error itself, and toss it
	  (let ((no-error (null? (hook-functions mus-error-hook))))
	    (if no-error
		(hook-push mus-error-hook (lambda (hook) (set! (hook 'result) #t))))
	    (let ((val (catch #t
			 (lambda ()
			   (mus-audio-open-output 0 cur-srate outchans frm outbytes))
			 (lambda args -1)))) ; -1 returned in case of error
	      (if no-error 
		  (set! (hook-functions mus-error-hook) ()))
	      val))))
    (if (not (= audio-fd -1))
	(set! (dac-size) outbytes))
    (list audio-fd outchans pframes)))


(define play-sound 
  (let ((+documentation+ "(play-sound func) plays the currently selected sound, calling func on each data buffer, if func exists (mono only)"))
    (lambda* (func)
      (if (pair? (sounds))
	  (let* ((filechans (chans))
		 (audio-info (open-play-output filechans (srate) #f 256))
		 (audio-fd (car audio-info))
		 ;; (outchans (cadr audio-info))
		 (pframes (caddr audio-info)))
	    (if (not (= audio-fd -1))
		(let ((len (framples)))
		  (do ((beg 0 (+ beg pframes)))
		      ((> beg len))
		    (mus-audio-write audio-fd (make-shared-vector (func (channel->float-vector beg pframes)) (list 1 pframes)) pframes))
		  (mus-audio-close audio-fd))
		(snd-print ";could not open dac")))
	  (snd-print ";no sounds open")))))

;; (play-sound (lambda (data) (float-vector-scale! data 2.0) data))

;; this could also be done with a function argument to the play function -- get a
;;   sampler for the sound, call it on each invocation of the argument function etc
|#


;;; -------- play sound n times -- (play-often 3) for example.

(define play-often 
  (let ((+documentation+ "(play-often n) plays the selected sound 'n' times (interruptible via C-g)"))
    (lambda (n) 
      (letrec ((play-once (let ((plays (- n 1)))
			    (lambda (reason)
			      (if (and (> plays 0)
				       (= reason 0))
				  (begin
				    (set! plays (- plays 1))
				    (play (selected-sound) :start 0 :stop play-once)))))))
	(play (selected-sound) :start 0 :stop play-once)))))

;;(bind-key #\p 0 (lambda (n) "play often" (play-often (max 1 n))))


;;; -------- play sound until c-g

(define play-until-c-g
  (let ((+documentation+ "(play-until-c-g) plays the selected sound until you interrupt it via C-g")
	(play-once (lambda (reason)
		     (if (= reason 0)
			 (play (selected-sound) :start 0 :stop play-once)))))
    (lambda ()
      (play (selected-sound) :start 0 :stop play-once))))


;;; -------- play region over and over until C-g typed

(define play-region-forever 
  (let ((+documentation+ "(play-region-forever reg) plays region 'reg' until you interrupt it via C-g"))
    (lambda (reg1)
      (let ((reg (if (integer? reg1) (integer->region reg1) reg1)))
	(define (play-region-again reason)
	  (if (= reason 0) ; 0=play completed normally
	      (play reg :wait #f :stop play-region-again)))
	(play reg :wait #f :stop play-region-again)))))

					;(bind-key #\p 0 (lambda (n) "play region forever" (play-region-forever ((regions) (max 0 n)))))


#|
;;; -------- play while looping continuously between two movable marks

(define loop-between-marks 
  (let ((+documentation+ "(loop-between-marks mark1 mark2 buffersize) plays while looping between two marks.  \
x typed in the graph, or C-g in the listener exits the loop."))
    (lambda (m1 m2 bufsize)
      (let* ((pos1 (mark-sample m1))
	     (pos2 (mark-sample m2))
	     (beg (min pos1 pos2))
	     (end (max pos1 pos2))
	     (bytes (* bufsize 2)) ; mus-audio-write handles the translation to short (and takes frames, not bytes as 3rd arg)
	     (audio-fd (mus-audio-open-output 0 (srate) 1 mus-lshort bytes))
	     (stop-looping #f))
	(if (not (= audio-fd -1))
	    (begin
	      (bind-key #\space 0 (lambda () (set! stop-looping #t))) ; type space in the graph to stop this loop
	      (do ()
		  (stop-looping
		   (mus-audio-close audio-fd)
		   (unbind-key #\space 0))
		(mus-audio-write audio-fd (make-shared-vector (channel->float-vector beg bufsize) (list 1 bufsize)) bufsize)
		(set! beg (+ beg bufsize))
		(if (>= beg end)
		    (begin
		      (set! pos1 (mark-sample m1)) ; get current mark positions (can change while looping)
		      (set! pos2 (mark-sample m2))
		      (set! beg (min pos1 pos2))
		      (set! end (max pos1 pos2)))))))))))

;; m1 and m2 are marks
;; (loop-between-marks (caaar (marks)) (cadaar (marks)) 512)
|#


;;; -------- hold DAC open and play sounds via keyboard

(define start-dac 
  (let ((+documentation+ "(start-dac (srate 44100) (chans 1)) starts the DAC running continuously in the background"))
    (lambda* ((sr 44100) (chans 1))
      (play #f :srate sr :channels chans))))

(define stop-dac stop-playing)



;; play-with-amps -- play channels with individually settable amps

(define play-with-amps
  (let ((+documentation+ "(play-with-amps snd :rest amps) plays snd with each channel scaled by the corresponding 
amp: (play-with-amps 0 1.0 0.5) plays channel 2 of stereo sound at half amplitude"))
    (lambda (sound . amps)
      (do ((chans (channels sound))
	   (chan 0 (+ 1 chan)))
	  ((= chan chans)
	   (start-playing chans (srate sound)))
	(let ((player (make-player sound chan)))
	  (set! (amp-control player) (amps chan))
	  (add-player player))))))
      

;;; play-sine and play-sines

(define play-sine 
  (let ((+documentation+ "(play-sine freq amp) plays a 1 second sinewave at freq and amp"))
    (lambda (freq amp)
      (let ((len 44100)
	    (osc (make-oscil freq)))
	(play (lambda ()
		(and (positive? (set! len (- len 1)))
		     (* amp (oscil osc)))))))))


(define play-sines 
  (let ((+documentation+ "(play-sines freqs-and-amps) produces a tone given its spectrum: (play-sines '((440 .4) (660 .3)))"))
    (lambda (freqs-and-amps)
      (let ((num-oscs (length freqs-and-amps)))
	(let ((len 44100)
	      (frqs (make-float-vector num-oscs))
	      (amps (make-float-vector num-oscs)))
	  (do ((i 0 (+ i 1)))
	      ((= i num-oscs))
	    (set! (frqs i) (hz->radians (car (freqs-and-amps i))))
	    (set! (amps i) (cadr (freqs-and-amps i))))
	  (let ((ob (make-oscil-bank frqs (make-float-vector num-oscs) amps #t)))
	    (play (lambda ()
		    (and (positive? (set! len (- len 1)))
			 (oscil-bank ob))))))))))

;; (play-sines '((425 .05) (450 .01) (470 .01) (546 .02) (667 .01) (789 .034) (910 .032)))
