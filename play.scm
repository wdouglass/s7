;;; most of this file is obsolete.
;;;
;;; playing-related examples previously scattered around at random
;;;
;;; play-often, play-until-c-g -- play sound n times or until C-g is typed
;;; play-region-forever -- play region over and over until C-g typed
;;; start-dac -- hold DAC open and play sounds via keyboard
;;; play-with-amps -- play channels with individually settable amps
;;; play-sine and play-sines -- produce tones direct to DAC

;;; see also play-syncd-marks and play-between-marks in marks.scm

(provide 'snd-play.scm)


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
