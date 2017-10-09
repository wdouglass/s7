;;; 
;;; tankrev.scm - 'plate reverb', Anders Vinjar 2016
;;;
;;;
;;; Dattorro style fig-8 tank-reverb, as outlined in paper:
;;;
;;;  Jon Dattorro: Effect Design Part 1: Reverberator and Other
;;;  Filters (1997)
;;;
;;; Paper available online:
;;; https://ccrma.stanford.edu/~dattorro/EffectDesignPart1.pdf
;;; 
;;; PARAMETERS (+ default values presented in paper):
;;; 
;;; FS=29761 Hz			; (AV: in JDs paper, see smpls->samples below)
;;; EXCURSION = 16		; maximum peak sample excursion of delay modulation
;;; decay = 0.5			; rate of decay
;;; decay diffusion 1 = 0.70	; Controls density of tail
;;; decay diffusion 2 = 0.50	; Decorrelates tank signals; decay diffusion 2 = decay + 0.15, floor = 0.25, ceiling = 0.5
;;; input diffusion 1 = 0.750	; decorrelates incoming signal
;;; input diffusion 2 = 0.6250
;;; bandwidth = 0.9995		; High-frequncey attenuation on input; full bandwidth = 0.99999999
;;; damping = 0.0005		; High-frequncey damping; no damping = 0.0

(provide 'snd-tankrev.scm)

(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))

(define (make-diffuser siz scl)
  (make-all-pass (- scl) scl siz))

(define (make-mod-all-pass siz diffusion)
  (make-all-pass diffusion (- diffusion) siz :max-size (* 2 siz)))

(define (smpls->samples smpl)
  (let ((FS 29761))			;orig. srate
    (round (* (/ *clm-srate* FS) smpl))))

;; To enter taps and sizes as prescribed in Dattorros article directly:

(define* (tank-reverb (predelay 0.0) (decay 0.5) (bandwidth 0.9995) (damping 0.005) (reverb-decay-time 1.0))
  "(tank-reverb (predelay 0.0) (decay 0.5) (bandwidth 0.9995) (damping 0.005) (reverb-decay-time 1.0))"

  ;; try setting 'decay = 1.0 for a nice 'freeze' effect
  
  (let ((decay-diffusion-1 0.70)
	(decay-diffusion-2 0.50)
	(input-diffusion-1 0.750)
	(input-diffusion-2 0.625))
    
    (let ((len (+ (framples *reverb*) (seconds->samples reverb-decay-time)))
	  (predly (make-delay (seconds->samples predelay)))
	  (lp1 (make-one-pole bandwidth (- bandwidth 1)))
	  
	  ;; input diffusers = series of 4 all-pass-filters (mono input):
	  (input-diffusers (make-all-pass-bank
			    (vector (make-diffuser (smpls->samples 142) input-diffusion-1)
				    (make-diffuser (smpls->samples 107) input-diffusion-1)
				    (make-diffuser (smpls->samples 379) input-diffusion-2)
				    (make-diffuser (smpls->samples 277) input-diffusion-2))))
	  
	  ;; tank, fig-of-eight in Dattorro's figure p. 662:
	  (excursion_24 (make-oscil 1.0)) ;max 'Excursion' = 16 samples (FS=29761)
	  (excursion_48 (make-oscil 0.707))
	  (modallpass_23 (make-mod-all-pass (smpls->samples 672) decay-diffusion-1))
	  (modallpass_46 (make-mod-all-pass (smpls->samples 908) decay-diffusion-1))
	  (delay_24_30 (make-delay (smpls->samples 4453)))
	  (delay_48_54 (make-delay (smpls->samples 4217)))
	  (damper_30 (make-one-pole (- 1 damping) (- damping)))
	  (damper_54 (make-one-pole (- 1 damping) (- damping)))
	  (diffuser_31_33 (make-diffuser (smpls->samples 1800) decay-diffusion-2))
	  (diffuser_55_59 (make-diffuser (smpls->samples 2656) decay-diffusion-2))
	  (delay_33_39 (make-delay (smpls->samples 3720)))
	  (delay_59_63 (make-delay (smpls->samples 3163)))
	  (dc-block-1 (make-filter 2 (float-vector 1 -1) (float-vector 0 -0.99)))
	  (dc-block-2 (make-filter 2 (float-vector 1 -1) (float-vector 0 -0.99)))
	  (tank_1 0.0) (tank_2 0.0)
	  
	  (smpl-266 (* 1.0 (smpls->samples 266)))
	  (smpl-2974 (* 1.0 (smpls->samples 2974)))
	  (smpl-1913 (* 1.0 (smpls->samples 1913)))
	  (smpl-1996 (* 1.0 (smpls->samples 1996)))
	  (smpl-1990 (* 1.0 (smpls->samples 1990)))
	  (smpl-187 (* 1.0 (smpls->samples 187)))
	  (smpl-1066 (* 1.0 (smpls->samples 1066)))
	  
	  (smpl-353 (* 1.0 (smpls->samples 353)))
	  (smpl-3627 (* 1.0 (smpls->samples 3627)))
	  (smpl-1228 (* 1.0 (smpls->samples 1228)))
	  (smpl-2673 (* 1.0 (smpls->samples 2673)))
	  (smpl-2111 (* 1.0 (smpls->samples 2111)))
	  (smpl-335 (* 1.0 (smpls->samples 335)))
	  (smpl-121 (* 1.0 (smpls->samples 121)))
	  
	  (smpl-8 (* 1.0 (smpls->samples 8))))
      
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((sig (all-pass-bank input-diffusers
				  (one-pole lp1
					    (delay predly
						   (ina i *reverb*))))))
	  
	  ;; add incoming signal to rotated tank:
	  
	  (set! tank_1 (delay delay_33_39
			      (all-pass diffuser_31_33
					(one-pole damper_30
						  (delay delay_24_30
							 (all-pass modallpass_23
								   (filter dc-block-1 (+ sig (* decay tank_2)))
								   (+ smpl-8
								      (* smpl-8 (oscil excursion_24)))))))))
	  (set! tank_2 (delay delay_59_63
			      (all-pass diffuser_55_59
					(one-pole damper_54
						  (delay delay_48_54
							 (all-pass modallpass_46
								   (filter dc-block-2 (+ sig (* decay tank_1)))
								   (+ smpl-8
								      (* smpl-8 (oscil excursion_48)))))))))
	  ;; tap reflections and output:

	  (outa i 
		(+ (* +0.6 (tap delay_48_54 smpl-266))
		   (* +0.6 (tap delay_48_54 smpl-2974))
		   (* -0.6 (tap diffuser_55_59 smpl-1913))
		   (* +0.6 (tap delay_59_63 smpl-1996))
		   (* -0.6 (tap delay_24_30 smpl-1990))
		   (* -0.6 (tap diffuser_31_33 smpl-187))
		   (* -0.6 (tap delay_33_39 smpl-1066))))
	  (outb i 
		(+ (* +0.6 (tap delay_24_30 smpl-353))
		   (* +0.6 (tap delay_24_30 smpl-3627))
		   (* -0.6 (tap diffuser_31_33 smpl-1228))
		   (* +0.6 (tap delay_33_39 smpl-2673))
		   (* -0.6 (tap delay_48_54 smpl-2111))
		   (* -0.6 (tap diffuser_55_59 smpl-335))
		   (* -0.6 (tap delay_59_63 smpl-121))))
	  )))))



#|
(with-sound (:channels 2 :reverb tank-reverb :reverb-data '(:damping 0.04
								     :predelay 0.03
								     :decay 0.99
								     ;; :decay 1.0
								     :reverb-decay-time 4.0
								     :bandwidth 0.99999))
  (let* ((input (make-readin "piano.wav"))
	 (len (framples input))
	 ;;(len (* 3 44100))
	 (aenv (make-env '(0 1 0.9 1 1 0) :length len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (let* ((a (env aenv))
	     (rd (* a 0.5 (readin input))))
	(outa i rd)
	(outb i rd)
	(outa i (* 0.2 rd) *reverb*)
))))

(with-sound (:reverb tank-reverb :reverb-data '(:reverb-decay-time 3.0))
	    (outa 0 .1)
	    (outa 0 .5 *reverb*)
	    (outa 1230 -0.3 *reverb*))

(let* ((dur 3000)
       (s (make-oscil 10))
       (e (make-env `(0 ,pi 1 0) :length dur)))
  (with-sound (:reverb tank-reverb :reverb-data '(:damping 0.0 :reverb-decay-time 3.0))
	      (do ((i 0 (+ 1 i)))
		  ((= i dur))
		(let ((sig (* 0.8 (oscil s (env e)))))
		  (outa i sig) (outb i sig)
		  (outa i (* 0.3 sig) *reverb*)))))

|#

;(set! smpls->samples #f)
