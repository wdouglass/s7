(if (and (provided? 'snd-gtk)
	 (not (provided? 'gtk4)))
    (error 'gtk-error "fft-menu.scm only works in gtk4"))

(when (and (provided? 'xm)
	   (not (provided? 'snd-effects-utils.scm)))
  (load "effects-utils.scm"))

(if (and (provided? 'gtk4)
	 (not (provided? 'snd-gtk-effects-utils.scm)))
    (load "gtk-effects-utils.scm"))

(define *e* (if (provided? 'snd-motif) *motif* *gtk*))
(define update-label (*e* 'update-label))
(define change-label (*e* 'change-label))
(define make-effect-dialog (*e* 'make-effect-dialog))
(define add-sliders (*e* 'add-sliders))
(define activate-dialog (*e* 'activate-dialog))
(define select-file (*e* 'select-file))

(require snd-examp.scm)

(provide 'snd-fft-menu.scm)


(define fft-list ()) ; menu labels are updated to show current default settings

(define fft-menu (add-to-main-menu "FFT Edits" 
				   (lambda ()
				     (for-each (lambda (fft) (fft)) fft-list))))

;;; ------ FFT edit
;;;

(define fft-edit-low-frequency 100)
(define fft-edit-high-frequency 1000)
(define fft-edit-label "FFT notch filter")
(define fft-edit-dialog #f)
(define fft-edit-menu-label #f)

(define (cp-fft-edit)
  (fft-edit fft-edit-low-frequency fft-edit-high-frequency))

(if (not (or (provided? 'xg) 
	     (provided? 'xm)))
    (set! fft-edit-menu-label (add-to-menu fft-menu fft-edit-label cp-fft-edit))
    (begin
      (define (post-fft-edit-dialog)
        (unless fft-edit-dialog
	  ;; if fft-edit-dialog doesn't exist, create it
	  (let ((initial-fft-edit-low-frequency 100)
		(initial-fft-edit-high-frequency 1000)
		(sliders ()))
	    
	    (set! fft-edit-dialog
		  (make-effect-dialog fft-edit-label
				      (if (provided? 'snd-gtk)
					  (values (lambda (w context) 
						    (cp-fft-edit))
						  (lambda (w context)
						    (help-dialog "FFT notch filter"
								 "A simple example of FFT-based editing. It takes an FFT of the entire sound, \
removes all energy below the low frequency and above the high frequency, then computes the inverse FFT."))
						  (lambda (w data)
						    (set! fft-edit-low-frequency initial-fft-edit-low-frequency)
						    (set! fft-edit-high-frequency
							  initial-fft-edit-high-frequency)
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (car sliders))
						     (floor fft-edit-low-frequency))
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (cadr sliders))
						     (floor fft-edit-high-frequency))))
					  (values (lambda (w context info) 
						    (cp-fft-edit))
						  (lambda (w context info)
						    (help-dialog "FFT notch filter"
								 "A simple example of FFT-based editing. It takes an FFT of the entire sound,\
 removes all energy below the low frequency and above the high frequency, then computes the inverse FFT."))
						  (lambda (w c i)
						    (set! fft-edit-low-frequency initial-fft-edit-low-frequency)
						    (set! fft-edit-high-frequency
							  initial-fft-edit-high-frequency)
						    ((*motif* 'XtSetValues) (car sliders) (list (*motif* 'XmNvalue) (floor fft-edit-low-frequency)))
						    ((*motif* 'XtSetValues) (cadr sliders) (list (*motif* 'XmNvalue) (floor fft-edit-high-frequency))))))))
	    (set! sliders
		  (add-sliders 
		   fft-edit-dialog
		   
		   (list (let ((low-func (if (provided? 'snd-gtk)
					     (lambda (w data) (set! fft-edit-low-frequency ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w))))
					     (lambda (w context info) (set! fft-edit-low-frequency ((*motif* '.value) info))))))
			   (list "low frequency" 20 initial-fft-edit-low-frequency 22050 low-func 1))
			 (let ((high-func (if (provided? 'snd-gtk) 
					      (lambda (w data) (set! fft-edit-high-frequency ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w))))
					      (lambda (w context info) (set! fft-edit-high-frequency ((*motif* '.value) info))))))
			   (list "high frequency" 20 initial-fft-edit-high-frequency 22050 high-func 1)))))))
        (activate-dialog fft-edit-dialog))
      (set! fft-edit-menu-label (add-to-menu fft-menu "FFT notch filter" post-fft-edit-dialog))))

(set! fft-list (cons (lambda ()
		       (let ((new-label (format #f "FFT notch filter (~D ~D)" fft-edit-low-frequency fft-edit-high-frequency)))
			 (if fft-edit-menu-label (change-label fft-edit-menu-label new-label))
			 (set! fft-edit-label new-label)))
		     fft-list))



;;; ------ FFT squelch
;;;

(define fft-squelch-amount 0.0)
(define fft-squelch-label "FFT squelch")
(define fft-squelch-dialog #f)
(define fft-squelch-menu-label #f)

(define (cp-fft-squelch)
  (fft-squelch fft-squelch-amount))

(if (not (or (provided? 'xg) 
	     (provided? 'xm)))
    (set! fft-squelch-menu-label (add-to-menu fft-menu fft-squelch-label cp-fft-squelch))
    (begin
      
      (define (post-fft-squelch-dialog)
        (unless fft-squelch-dialog
	  ;; if fft-squelch-dialog doesn't exist, create it
	  (let ((initial-fft-squelch-amount 0.0)
		(sliders ()))
	    
	    (set! fft-squelch-dialog
		  (make-effect-dialog fft-squelch-label
				      (if (provided? 'snd-gtk)
					  (values (lambda (w data) 
						    (cp-fft-squelch))
						  (lambda (w data)
						    (help-dialog "FFT squelch"
								 "Removes all energy below the squelch amount. This is sometimes useful for noise-reduction."))
						  (lambda (w data)
						    (set! fft-squelch-amount initial-fft-squelch-amount)
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (car sliders))
						     (round (* fft-squelch-amount 100)))))
					  (values (lambda (w context info)
						    (cp-fft-squelch))
						  (lambda (w context info)
						    (help-dialog "FFT squelch"
								 "Removes all energy below the squelch amount. This is sometimes useful for noise-reduction."))
						  (lambda (w c i)
						    (set! fft-squelch-amount initial-fft-squelch-amount)
						    ((*motif* 'XtSetValues) (list-ref sliders 0)
						     (list (*motif* 'XmNvalue) (round (* fft-squelch-amount 100)))))))))
	    (set! sliders
		  (add-sliders 
		   fft-squelch-dialog
		   (let ((squelch-func (if (provided? 'snd-gtk)
					   (lambda (w data)
					     (set! fft-squelch-amount (/ ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w)) 100)))
					   (lambda (w context info)
					     (set! fft-squelch-amount (/ ((*motif* '.value) info) 100))))))
		   (list (list "squelch amount" 0.0 initial-fft-squelch-amount 1.0 squelch-func 100)))))))
	
        (activate-dialog fft-squelch-dialog))
      (set! fft-squelch-menu-label (add-to-menu fft-menu "FFT squelch" post-fft-squelch-dialog))))


(set! fft-list (cons (lambda ()
		       (let ((new-label (format #f "FFT squelch (~1,2F)" fft-squelch-amount)))
			 (if fft-squelch-menu-label (change-label fft-squelch-menu-label new-label))
			 (set! fft-squelch-label new-label)))
		     fft-list))

(add-to-menu fft-menu #f #f)

(add-to-menu fft-menu "Squelch vowels" squelch-vowels)
