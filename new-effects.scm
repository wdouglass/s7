(if (not (provided? 'snd-motif)) 
    (snd-error "new-effects.scm is Motif-specific"))

(provide 'snd-new-effects.scm)
(require snd-effects-utils.scm snd-xm-enved.scm snd-moog.scm snd-rubber.scm snd-dsp.scm)

(with-let (sublet *motif*)
  
  (define effects-list ()) ; menu labels are updated to show current settings
  
  (define effects-menu (add-to-main-menu "Effects" (lambda () (update-label effects-list))))
  
  (define plausible-mark-samples
    (let ((+documentation+ "(plausible-mark-samples) finds two marks in the current channel in or nearest to current window"))
      (lambda ()
	(let* ((snd (selected-sound))
	       (chn (selected-channel))
	       (ms (sort! (map mark-sample (marks snd chn)) <)))
	  (if (< (length ms) 2)
	      (error 'no-such-mark (list "mark-related action requires two marks"))
	      (if (= (length ms) 2)
		  ms
		  (let ((favor (let ((lw (left-sample snd chn))
				     (rw (right-sample snd chn))
				     (cw (cursor snd chn)))
				 (if (>= rw cw lw)
				     cw
				     (* .5 (+ lw rw))))))
		    ;; favor is the point we center the search on
		    (let centered-points ((points ms))
		      (if (= (length points) 2)
			  points
			  (let ((p1 (car points))
				(p2 (cadr points))
				(p3 (caddr points)))
			    (if (< (abs (- p1 favor)) (abs (- p3 favor)))
				(list p1 p2)
				(centered-points (cdr points)))))))))))))
  
  (define map-chan-over-target-with-sync
    ;; target: 'marks -> beg=closest marked sample, dur=samples to next mark
    ;;         'sound -> beg=0, dur=all samples in sound
    ;;         'selection -> beg=selection-position, dur=selection-framples
    ;;         'cursor -> beg=cursor, dur=samples to end of sound
    ;; decay is how long to run the effect past the end of the sound
    (lambda (func target origin decay)
      (cond ((and (eq? target 'selection)
		  (not (selection?)))
	     (snd-print ";no selection"))
	    ((and (eq? target 'sound)
		  (null? (sounds)))
	     (snd-print ";no sound"))
	    ((and (eq? target 'marks)
		  (or (null? (sounds))
		      (< (length (marks (selected-sound) (selected-channel))) 2)))
	     (snd-print ";no marks"))
	    (else
	     (let ((ms (and (eq? target 'marks)
			    (plausible-mark-samples))))
	       (let ((snc (sync))
		     (beg (case target
			    ((sound) 0)
			    ((selection) (selection-position))
			    ((cursor) (cursor (selected-sound) (selected-channel)))
			    (else (car ms))))
		     (overlap (if decay (floor (* (srate) decay)) 0)))
		 (apply for-each
			(lambda (snd chn)
			  (if (= (sync snd) snc)
			      (let ((end (case target 
					   ((sound cursor) (- (framples snd chn) 1))
					   ((selection)    (+ (selection-position) (selection-framples)))
					   (else (cadr ms)))))
				(map-channel (func (- end beg)) beg (+ end overlap 1) snd chn
					     #f
					     (format #f "~A ~A ~A" (origin target (- end beg)) (if (eq? target 'sound) 0 beg)
						     (and (not (eq? target 'sound)) (- (+ end 1) beg)))))))
			(if (> snc 0)
			    (all-chans)
			    (list (list (selected-sound)) (list (selected-channel)))))))))))
  
  (define (add-target mainform target-callback truncate-callback)
    ;; add a set of 3 radio buttons at the bottom of the main section for choice between sound, selection, between-marks
    ;;   target-callback should take one arg, a symbol: 'sound, 'selection, 'marks, and apply the effect accordingly (upon "DoIt")
    ;;   truncate-callback (if any) takes one arg: boolean representing toggle state (#t = on)
    
    (define yellow-pixel
      (let ((pix #f))
	(lambda ()
	  (if (not pix)
	      (let* ((dpy (XtDisplay (cadr (main-widgets))))
		     (cmap (DefaultColormap dpy (DefaultScreen dpy)))
		     (col (XColor)))
		(if (= (XAllocNamedColor dpy cmap "yellow" col col) 0)
		    (snd-error "can't allocate yellow!")
		    (set! pix (.pixel col)))))
	  pix)))
  
    (XtCreateManagedWidget "sep" xmSeparatorWidgetClass mainform
			   (list XmNorientation      XmHORIZONTAL
				 XmNseparatorType    XmSHADOW_ETCHED_OUT
				 XmNbackground       *basic-color*))
    (let ((rc (XtCreateManagedWidget "rc"  xmRowColumnWidgetClass mainform
				     (list XmNorientation      XmHORIZONTAL
					   XmNbackground       *basic-color*
					   XmNradioBehavior    #t
					   XmNradioAlwaysOne   #t
					   XmNbottomAttachment XmATTACH_FORM
					   XmNleftAttachment   XmATTACH_FORM
					   XmNrightAttachment  XmATTACH_FORM
					   XmNentryClass       xmToggleButtonWidgetClass
					   XmNisHomogeneous    #t))))
      (for-each
       (lambda (name type on)
	 (XtCreateManagedWidget name xmToggleButtonWidgetClass rc
				(list XmNbackground       *basic-color*
				      XmNset              on
				      XmNselectColor      (yellow-pixel)
				      XmNindicatorType    XmONE_OF_MANY_ROUND
				      XmNarmCallback      (list (lambda (w c i) 
								  (target-callback type)) 
								#f))))
       '("entire sound" "selection" "between marks")
       '(sound selection marks)
       '(#t #f #f))
      (if truncate-callback
	  (XtCreateManagedWidget "trsep" xmSeparatorWidgetClass mainform
				 (list XmNorientation      XmHORIZONTAL))
	  (let ((trbutton (XtCreateManagedWidget "truncate at end" xmToggleButtonWidgetClass mainform
						 (list XmNbackground       *basic-color*
						       XmNset              #t
						       XmNselectColor      (yellow-pixel)))))
	    (XtAddCallback trbutton XmNvalueChangedCallback (lambda (w c i) (truncate-callback (.set i))))))
      rc))
  
  (define (effect-framples target)
    (case target 
      ((sound)	    (- (framples) 1))
      ((selection) (selection-framples))
      (else (+ 1 (abs (apply - (plausible-mark-samples)))))))
  
  
;;; changed 12-Oct-04 to make detached edit lists work (need globally accessible effect functions, etc)
;;; changed 12-Oct-06 for new watcher mechanism, rather than the selection-button list, selection-changed-hook etc
  
  
;;; *******************************
;;;                              **
;;; BEGIN PARAMETRIZED EFFECTS   **
;;;                              **
;;; *******************************
  
  
;;; AMPLITUDE EFFECTS
  
  (define* (effects-squelch-channel amp gate-size snd chn no-silence)
    (let ((avgf (let ((f0 (make-moving-average gate-size))
		      (f1 (make-moving-average gate-size :initial-element 1.0)))
		  (if no-silence
		      (lambda (y)
			(let ((val (* y (moving-average f1 (ceiling (- (moving-average f0 (* y y)) amp))))))
			  (and (not (zero? val)) val)))
		      (lambda (y)
			(* y (moving-average f1 (ceiling (- (moving-average f0 (* y y)) amp)))))))))
      (map-channel avgf 0 #f snd chn #f (format #f "effects-squelch-channel ~A ~A" amp gate-size))))
  
  (let ((amp-menu-list ())
	(amp-menu (XmCreatePulldownMenu (main-menu effects-menu) "Amplitude Effects"
					(list XmNbackground *basic-color*))))
    (XtAddCallback (XtCreateManagedWidget "Amplitude Effects" xmCascadeButtonWidgetClass (main-menu effects-menu)
					  (list XmNsubMenuId amp-menu
						XmNbackground *basic-color*))
		   XmNcascadingCallback 
		   (lambda (w c i)
		     (update-label amp-menu-list)))
    
    
;;; -------- Gain (gain set by gain-amount)
    
    (let* ((gain-amount 1.0)
	   (post-gain-dialog
	    (let ((gain-label "Gain")
		  (gain-dialog #f)
		  (gain-target 'sound)
		  (gain-envelope #f))
	      (lambda ()
		(if (Widget? gain-dialog)
		    (activate-dialog gain-dialog)
		    ;; if gain-dialog doesn't exist, create it
		    (let ((sliders ()))
		      (let ((initial-gain-amount 1.0))
			(set! gain-dialog
			      (make-effect-dialog 
			       gain-label
			       
			       (lambda (w context info)
				 (let ((with-env (and (not (equal? (xe-envelope gain-envelope) '(0.0 1.0 1.0 1.0)))
						      (let scale-envelope ((e (xe-envelope gain-envelope))
									   (scl gain-amount))
							(if (null? e)
							    ()
							    (cons (car e) 
								  (cons (* scl (cadr e)) 
									(scale-envelope (cddr e) scl))))))))
				   (case gain-target 
				     ((sound)
				      (if with-env
					  (env-sound with-env)
					  (scale-by gain-amount)))
				     ((selection)
				      (if (selection?)
					  (if with-env
					      (env-selection with-env)
					      (scale-selection-by gain-amount))
					  (snd-print ";no selection")))
				     (else 
				      (let ((pts (catch 'no-such-mark 
						   plausible-mark-samples
						   (lambda args #f))))
					(if pts
					    (if with-env
						(env-sound with-env (car pts) (- (cadr pts) (car pts)))
						(scale-by gain-amount (car pts) (- (cadr pts) (car pts))))
					    (snd-print ";no marks")))))))
			       
			       (lambda (w context info)
				 (help-dialog "Gain"
					      "Move the slider to change the gain scaling amount."))
			       
			       (lambda (w c i)
				 (set! gain-amount initial-gain-amount)
				 (set! (xe-envelope gain-envelope) (list 0.0 1.0 1.0 1.0))
				 (XtSetValues (car sliders) (list XmNvalue (floor (* gain-amount 100)))))
			       
			       (lambda () 
				 (effect-target-ok gain-target))))

			(set! sliders
			      (add-sliders gain-dialog
					   (list (list "gain" 0.0 initial-gain-amount 5.0
						       (lambda (w context info)
							 (set! gain-amount (/ (.value info) 100.0)))
						       100)))))

		      (let* ((fr (XtCreateManagedWidget "fr" xmFrameWidgetClass (XtParent (XtParent (car sliders)))
						      (list XmNheight              200
							    XmNleftAttachment      XmATTACH_FORM
							    XmNrightAttachment     XmATTACH_FORM
							    XmNtopAttachment       XmATTACH_WIDGET
							    XmNtopWidget           (sliders (- (length sliders) 1))
							    XmNshadowThickness     4
							    XmNshadowType          XmSHADOW_ETCHED_OUT)))
			     (target-row (add-target (XtParent (XtParent (car sliders)))
						      (lambda (target) 
							(set! gain-target target)
							(XtSetSensitive (XmMessageBoxGetChild gain-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
						      #f)))
			(activate-dialog gain-dialog)
			  
			(set! gain-envelope (xe-create-enved "gain"  fr
							     (list XmNheight 200)
							     '(0.0 1.0 0.0 1.0)))
			(set! (xe-envelope gain-envelope) (list 0.0 1.0 1.0 1.0))
			(XtVaSetValues fr (list XmNbottomAttachment XmATTACH_WIDGET
						XmNbottomWidget     target-row))))))))
      
	   (child (XtCreateManagedWidget "Gain" xmPushButtonWidgetClass amp-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-gain-dialog)))
      (set! amp-menu-list (cons (lambda ()
				  (change-label child (format #f "Gain (~1,2F)"  gain-amount)))
				amp-menu-list)))
    
    
;;; -------- Normalize
;;;
    
    (let* ((normalize-amount 1.0)
	   (post-normalize-dialog
	    (let ((normalize-label "Normalize")
		  (normalize-dialog #f)
		  (normalize-target 'sound))
	      (lambda ()
		(unless (Widget? normalize-dialog)
		  ;; if normalize-dialog doesn't exist, create it
		  (let ((initial-normalize-amount 1.0)
			(sliders ()))
		    (set! normalize-dialog 
			  (make-effect-dialog 
			   normalize-label
			   
			   (lambda (w context info)
			     (case normalize-target 
			       ((sound) 
				(scale-to normalize-amount))
				((selection)
				 (if (selection?)
				     (scale-selection-to normalize-amount)
				     (snd-print ";no selection")))
				(else
				 (let ((pts (plausible-mark-samples)))
				   (if pts
				       (scale-to normalize-amount (car pts) (- (cadr pts) (car pts))))))))
			   
			   (lambda (w context info)
			     (help-dialog "Normalize"
					  "Normalize scales amplitude to the normalize amount. Move the slider to change the scaling amount."))
			   
			   (lambda (w c i)
			     (set! normalize-amount initial-normalize-amount)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* normalize-amount 100)))))
			   
			   (lambda () 
			     (effect-target-ok normalize-target))))
		    
		    (set! sliders
			  (add-sliders normalize-dialog
				       (list (list "normalize" 0.0 initial-normalize-amount 1.0 
						   (lambda (w context info)
						     (set! normalize-amount (/ (.value info) 100.0)))
						   100))))
		    (add-target (XtParent (car sliders)) 
				(lambda (target) 
				  (set! normalize-target target)
				  (XtSetSensitive (XmMessageBoxGetChild normalize-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				#f)))
		
		(activate-dialog normalize-dialog))))
	   
	   (child (XtCreateManagedWidget "Normalize" xmPushButtonWidgetClass amp-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-normalize-dialog)))
	
      (set! amp-menu-list (cons (lambda ()
				  (change-label child (format #f "Normalize (~1,2F)"  normalize-amount)))
				amp-menu-list)))
    
    
;;; -------- Gate (gate set by gate-amount)
;;;
    
    (let* ((gate-amount 0.01)
	   (post-gate-dialog
	    (let ((gate-label "Gate")
		  (gate-dialog #f)
		  (gate-size 128)
		  (omit-silence #f))
	      (lambda ()
		(unless (Widget? gate-dialog)
		  ;; if gate-dialog doesn't exist, create it
		  (let ((initial-gate-amount 0.01)
			(sliders ()))
		    (set! gate-dialog
			  (make-effect-dialog 
			   gate-label
			   
			   (lambda (w context info)
			     (let ((snc (sync)))
			       (if (> snc 0)
				   (apply map
					  (lambda (snd chn)
					    (if (= (sync snd) snc)
						(effects-squelch-channel (* gate-amount gate-amount) gate-size snd chn omit-silence)))
					  (all-chans))
				   (effects-squelch-channel (* gate-amount gate-amount) gate-size (selected-sound) (selected-channel) omit-silence))))
			   
			   (lambda (w context info)
			     (help-dialog "Gate"
					  "Move the slider to change the gate intensity. Higher values gate more of the sound."))
			   
			   (lambda (w c i)
			     (set! gate-amount initial-gate-amount)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* gate-amount 1000)))))
			   
			   (lambda () 
			     (pair? (sounds)))))
		    
		    (set! sliders
			  (add-sliders gate-dialog
				       (list (list "gate" 0.0 initial-gate-amount 0.1
						   (lambda (w context info)
						     (set! gate-amount (/ (.value info) 1000.0)))
						   1000))))
		    ;; now add a toggle button setting omit-silence 
		    ;;  (need to use XtParent here because the containing RowColumn widget is
		    ;;  hidden in add-sliders -- perhaps it should be returned in the slider list)
		    
		    (let* ((s1 (XmStringCreateLocalized "Omit silence"))
			   (toggle
			    (XtCreateManagedWidget "Omit silence" xmToggleButtonWidgetClass (XtParent (car sliders))
						   (list XmNselectColor  *selection-color*
							 XmNbackground   *basic-color*
							 XmNvalue        (if omit-silence 1 0)
							 XmNlabelString  s1))))
		      (XmStringFree s1)
		      (XtAddCallback toggle XmNvalueChangedCallback (lambda (w c i)
								      (set! omit-silence (.set i)))))))
		(activate-dialog gate-dialog))))
      
	   (child (XtCreateManagedWidget "Gate" xmPushButtonWidgetClass amp-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-gate-dialog)))
      
      (set! amp-menu-list (cons (lambda ()
				  (change-label child (format #f "Gate (~1,4F)"  gate-amount)))
				amp-menu-list))))

  
;;; DELAY EFFECTS
;;;
  
  (let ((delay-menu-list ())
	(delay-menu (XmCreatePulldownMenu (main-menu effects-menu) "Delay Effects"
					  (list XmNbackground *basic-color*))))
    (let ((delay-cascade (XtCreateManagedWidget "Delay Effects" xmCascadeButtonWidgetClass (main-menu effects-menu)
						(list XmNsubMenuId delay-menu
						      XmNbackground *basic-color*))))
      (XtAddCallback delay-cascade XmNcascadingCallback (lambda (w c i) (update-label delay-menu-list))))
    
;;; -------- Echo (controlled by delay-time and echo-amount)
    
    (let ((delay-time .5) ; i.e. delay between echoes
	  (echo-amount .2))
      (let* ((post-echo-dialog
	      (let ((echo-label "Echo")
		    (echo-dialog #f)
		    (echo-target 'sound)
		    (echo-truncate #t))
		(lambda ()
		  (unless (Widget? echo-dialog)
		    ;; if echo-dialog doesn't exist, create it
		    (let ((initial-delay-time 0.5)
			  (initial-echo-amount 0.2)
			  (sliders ()))
		      (set! echo-dialog 
			    (make-effect-dialog 
			     echo-label
			     
			     (lambda (w context info)
			       (map-chan-over-target-with-sync
				(lambda (cutoff) 
				  (let ((del (make-delay (round (* delay-time (srate)))))
					(genv (make-env (list 0.0 1.0 cutoff 1.0 (+ cutoff 1) 0.0 (+ cutoff 100) 0.0) :length (+ cutoff 100))))
				    (lambda (inval)
				      (+ inval
					 (delay del
						(* echo-amount (+ (tap del) (* (env genv) inval))))))))
				echo-target
				(lambda (target input-samps) 
				  (format #f "effects-echo ~A ~A ~A" 
					  (and (not (eq? target 'sound)) input-samps)
					  delay-time echo-amount))
				(and (not echo-truncate) 
				     (* 4 delay-time))))
			     
			     (lambda (w context info)
			       (help-dialog "Echo"
					    "The sliders change the delay time and echo amount."))
			     
			     (lambda (w c i)   
			       (set! delay-time initial-delay-time)
			       (XtSetValues (car sliders) (list XmNvalue (floor (* delay-time 100))))
			       (set! echo-amount initial-echo-amount)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor (* echo-amount 100)))))
			     
			     (lambda ()
			       (effect-target-ok echo-target))))
		      
		      (set! sliders
			    (add-sliders echo-dialog
					 (list (list "delay time" 0.0 initial-delay-time 2.0
						     (lambda (w context info)
						       (set! delay-time (/ (.value info) 100.0)))
						     100)
					       (list "echo amount" 0.0 initial-echo-amount 1.0
						     (lambda (w context info)
						       (set! echo-amount (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! echo-target target)
				    (XtSetSensitive (XmMessageBoxGetChild echo-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  (lambda (truncate) 
				    (set! echo-truncate truncate)))))
		  
		  (activate-dialog echo-dialog))))
	     
	     (child (XtCreateManagedWidget "Echo" xmPushButtonWidgetClass delay-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-echo-dialog)))
	
	(set! delay-menu-list (cons (lambda ()
				      (change-label child (format #f "Echo (~1,2F ~1,2F)" delay-time echo-amount)))
				    delay-menu-list))))
    
    
;;; -------- Filtered echo
    
    (let ((flecho-scaler 0.5)
	  (flecho-delay 0.9))
      (let* ((post-flecho-dialog
	      (let ((flecho-label "Filtered echo")
		    (flecho-dialog #f)
		    (flecho-target 'sound)
		    (flecho-truncate #t))
		(lambda ()
		  (unless (Widget? flecho-dialog)
		    ;; if flecho-dialog doesn't exist, create it
		    (let ((initial-flecho-scaler 0.5)
			  (initial-flecho-delay 0.9)
			  (sliders ()))
		      (set! flecho-dialog 
			    (make-effect-dialog 
			     flecho-label
			     
			     (lambda (w context info)
			       (map-chan-over-target-with-sync
				(lambda (input-samps) 
				  (let ((flt (make-fir-filter :order 4 
							      :xcoeffs #r(.125 .25 .25 .125)))
					(del (make-delay (round (* flecho-delay (srate)))))
					(genv (make-env (list 0.0 1.0 input-samps 1.0 (+ input-samps 1) 0.0 (+ input-samps 100) 0.0) 
							:length (+ input-samps 100))))
				    (lambda (inval)
				      (+ inval 
					 (delay del 
						(fir-filter flt (* flecho-scaler 
								   (+ (tap del) 
								      (* (env genv) inval)))))))))
				flecho-target 
				(lambda (target input-samps) 
				  (format #f "effects-flecho ~A ~A ~A"
					  flecho-scaler flecho-delay
					  (and (not (eq? target 'sound)) input-samps)))
				(and (not flecho-truncate) 
				     (* 4 flecho-delay))))
			     
			     (lambda (w context info)
			       (help-dialog "Filtered echo"
					    "Move the sliders to set the filter scaler and the delay time in seconds."))
			     
			     (lambda (w c i)
			       (set! flecho-scaler initial-flecho-scaler)
			       (XtSetValues (sliders 0) (list XmNvalue (floor (* flecho-scaler 100))))
			       (set! flecho-delay initial-flecho-delay)
			       (XtSetValues (sliders 1) (list XmNvalue (floor (* flecho-delay 100)))))
			     
			     (lambda () 
			       (effect-target-ok flecho-target))))
		      
		      (set! sliders
			    (add-sliders flecho-dialog
					 (list (list "filter scaler" 0.0 initial-flecho-scaler 1.0
						     (lambda (w context info)
						       (set! flecho-scaler (/ (.value info) 100.0)))
						     100)
					       (list "delay time (secs)" 0.0 initial-flecho-delay 3.0
						     (lambda (w context info)
						       (set! flecho-delay (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! flecho-target target)
				    (XtSetSensitive (XmMessageBoxGetChild flecho-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  (lambda (truncate) 
				    (set! flecho-truncate truncate)))))
		  
		  (activate-dialog flecho-dialog))))
	     
	     (child (XtCreateManagedWidget "Filtered echo" xmPushButtonWidgetClass delay-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-flecho-dialog)))
	
	(set! delay-menu-list (cons (lambda ()
				      (change-label child (format #f "Filtered echo (~1,2F ~1,2F)" flecho-scaler flecho-delay)))
				    delay-menu-list))))
    
    
;;; -------- Modulated echo
;;; -------- very slick
    
    (let ((zecho-scaler 0.5)
	  (zecho-delay 0.75)
	  (zecho-freq 6)
	  (zecho-amp 10.0))
      (let* ((post-zecho-dialog
	      (let ((zecho-label "Modulated echo")
		    (zecho-dialog #f)
		    (zecho-target 'sound)
		    (zecho-truncate #t))
		(lambda ()
		  (unless (Widget? zecho-dialog)
		    ;; if zecho-dialog doesn't exist, create it
		    (let ((initial-zecho-scaler 0.5)
			  (initial-zecho-delay 0.75)
			  (initial-zecho-freq 6)
			  (initial-zecho-amp 10.0)
			  (sliders ())
			  (zecho-1 (lambda (scaler secs frq amp cutoff)
				     (let ((os (make-oscil frq))
					   (del (let ((len (round (* secs (srate)))))
						  (make-delay len :max-size (round (+ len amp 1)))))
					   (genv (make-env (list 0.0 1.0 cutoff 1.0 (+ cutoff 1) 0.0 (+ cutoff 100) 0.0) :length (+ cutoff 100))))
				       (lambda (inval)
					 (+ inval 
					    (delay del 
						   (* scaler (+ (tap del) (* (env genv) inval)))
						   (* amp (oscil os)))))))))
		      (set! zecho-dialog 
			    (make-effect-dialog 
			     zecho-label
			     
			     (lambda (w context info)
			       (map-chan-over-target-with-sync
				(lambda (input-samps)
				  (zecho-1 zecho-scaler zecho-delay zecho-freq zecho-amp input-samps)) 
				zecho-target
				(lambda (target input-samps) 
				  (format #f "effects-zecho ~A ~A ~A ~A ~A"
					  zecho-scaler zecho-delay zecho-freq zecho-amp
					  (and (not (eq? target 'sound)) input-samps)))
				(and (not zecho-truncate)
				     (* 4 zecho-delay))))
			     
			     (lambda (w context info)
			       (help-dialog "Modulated echo"
					    "Move the sliders to set the echo scaler, 
the delay time in seconds, the modulation frequency, and the echo amplitude."))
			     
			     (lambda (w c i)
			       (set! zecho-scaler initial-zecho-scaler)
			       (XtSetValues (sliders 0) (list XmNvalue (floor (* zecho-scaler 100))))
			       (set! zecho-delay initial-zecho-delay)
			       (XtSetValues (sliders 1) (list XmNvalue (floor (* zecho-delay 100))))
			       (set! zecho-freq initial-zecho-freq)
			       (XtSetValues (sliders 2) (list XmNvalue (floor (* zecho-freq 100))))
			       (set! zecho-amp initial-zecho-amp)
			       (XtSetValues (sliders 3) (list XmNvalue (floor (* zecho-amp 100)))))
			     
			     (lambda () 
			       (effect-target-ok zecho-target))))
		      
		      (set! sliders
			    (add-sliders zecho-dialog
					 (list (list "echo scaler" 0.0 initial-zecho-scaler 1.0
						     (lambda (w context info)
						       (set! zecho-scaler (/ (.value info) 100.0)))
						     100)
					       (list "delay time (secs)" 0.0 initial-zecho-delay 3.0
						     (lambda (w context info)
						       (set! zecho-delay (/ (.value info) 100.0)))
						     100)
					       (list "modulation frequency" 0.0 initial-zecho-freq 100.0
						     (lambda (w context info)
						       (set! zecho-freq (/ (.value info) 100.0)))
						     100)
					       (list "modulation amplitude" 0.0 initial-zecho-amp 100.0
						     (lambda (w context info)
						       (set! zecho-amp (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! zecho-target target)
				    (XtSetSensitive (XmMessageBoxGetChild zecho-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  (lambda (truncate) 
				    (set! zecho-truncate truncate)))))
		  (activate-dialog zecho-dialog))))
	     
	     (child (XtCreateManagedWidget "Modulated echo" xmPushButtonWidgetClass delay-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-zecho-dialog)))
	
	(set! delay-menu-list (cons (lambda ()
				      (change-label child (format #f "Modulated echo (~1,2F ~1,2F ~1,2F ~1,2F)" 
								  zecho-scaler zecho-delay zecho-freq zecho-amp)))
				    delay-menu-list)))))
  
;;; FILTERS
;;;
  
  (let ((filter-menu-list ())
	(filter-menu (XmCreatePulldownMenu (main-menu effects-menu) "Filter Effects"
					   (list XmNbackground *basic-color*))))

    (let ((filter-cascade (XtCreateManagedWidget "Filter Effects" xmCascadeButtonWidgetClass (main-menu effects-menu)
						 (list XmNsubMenuId filter-menu
						       XmNbackground *basic-color*))))
      (XtAddCallback filter-cascade XmNcascadingCallback (lambda (w c i) (update-label filter-menu-list))))
    
;;; -------- Butterworth band-pass filter
    
    (let ((band-pass-freq 1000)
	  (band-pass-bw 100))
      (let* ((post-band-pass-dialog
	      (let ((band-pass-label "Band-pass filter")
		    (band-pass-dialog #f)
		    (band-pass-target 'sound))
		(lambda ()
		  (unless (Widget? band-pass-dialog)
		    ;; if band-pass-dialog doesn't exist, create it
		    (let ((initial-band-pass-freq 1000)
			  (initial-band-pass-bw 100)
			  (sliders ()))
		      (set! band-pass-dialog 
			    (make-effect-dialog 
			     band-pass-label
			     
			     (lambda (w context info)
			       (let ((flt (make-butter-band-pass band-pass-freq band-pass-bw)))
				 (case band-pass-target 
				   ((sound)
				    (filter-sound flt #f #f #f #f (format #f "effects-bbp ~A ~A 0 #f" band-pass-freq band-pass-bw)))
				   ((selection)
				    (filter-selection flt))
				   (else
				    (let ((ms (plausible-mark-samples)))
				      (let ((bg (car ms))
					    (nd (- (+ (cadr ms) 1) (car ms))))
					(clm-channel flt bg nd #f #f #f #f 
						     (format #f "effects-bbp ~A ~A ~A ~A" band-pass-freq band-pass-bw bg nd))))))))
			     (lambda (w context info)
			       (help-dialog "Band-pass filter"
					    "Butterworth band-pass filter. Move the sliders to change the center frequency and bandwidth."))
			     
			     (lambda (w c i)
			       (set! band-pass-freq initial-band-pass-freq)
			       (XtSetValues (car sliders) (list XmNvalue (scale-log->linear 20 band-pass-freq 22050)))
			       (set! band-pass-bw initial-band-pass-bw)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor band-pass-bw))))
			     
			     (lambda () 
			       (effect-target-ok band-pass-target))))
		      
		      (set! sliders
			    (add-sliders band-pass-dialog
					 (list (list "center frequency" 20 initial-band-pass-freq 22050
						     (lambda (w context info)
						       (set! band-pass-freq (scale-linear->log 20 (.value info) 22050)))
						     1 'log)
					       (list "bandwidth" 0 initial-band-pass-bw 1000
						     (lambda (w context info)
						       (set! band-pass-bw (.value info)))
						     1))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! band-pass-target target)
				    (XtSetSensitive (XmMessageBoxGetChild band-pass-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog band-pass-dialog))))
	     
	     (child (XtCreateManagedWidget "Band-pass filter" xmPushButtonWidgetClass filter-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-band-pass-dialog)))
	
	(set! filter-menu-list (cons (lambda ()
				       (change-label child (format #f "Band-pass filter (~,2F ~D" band-pass-freq band-pass-bw)))
				     filter-menu-list))))
    
;;; -------- Butterworth band-reject (notch) filter
    
    (let ((notch-freq 100)
	  (notch-bw 100))
      (let* ((post-notch-dialog
	      (let ((notch-label "Band-reject filter")
		    (notch-dialog #f)
		    (notch-target 'sound))
		(lambda ()
		  (unless (Widget? notch-dialog)
		    ;; if notch-dialog doesn't exist, create it
		    (let ((initial-notch-freq 100)
			  (initial-notch-bw 100)
			  (sliders ()))
		      (set! notch-dialog 
			    (make-effect-dialog 
			     notch-label
			     
			     (lambda (w context info) 
			       (let ((flt (make-butter-band-reject notch-freq notch-bw)))
				 (case notch-target 
				   ((sound)
				    (filter-sound flt #f #f #f #f (format #f "effects-bbr ~A ~A 0 #f" notch-freq notch-bw)))
				   ((selection)
				    (filter-selection flt))
				   (else
				    (let ((ms (plausible-mark-samples)))
				      (let ((bg (car ms))
					    (nd (- (+ (cadr ms) 1) (car ms))))
					(clm-channel flt bg nd #f #f #f #f 
						     (format #f "effects-bbr ~A ~A ~A ~A" notch-freq notch-bw bg nd))))))))
			     (lambda (w context info)
			       (help-dialog "Band-reject filter"
					    "Butterworth band-reject filter. Move the sliders to change the center frequency and bandwidth."))
			     
			     (lambda (w c i)
			       (set! notch-freq initial-notch-freq)
			       (XtSetValues (car sliders) (list XmNvalue (scale-log->linear 20 notch-freq 22050)))
			       (set! notch-bw initial-notch-bw)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor notch-bw))))
			     
			     (lambda () 
			       (effect-target-ok notch-target))))
		      
		      (set! sliders
			    (add-sliders notch-dialog
					 (list (list "center frequency" 20 initial-notch-freq 22050
						     (lambda (w context info)
						       (set! notch-freq (scale-linear->log 20 (.value info) 22050)))
						     1 'log)
					       (list "bandwidth" 0 initial-notch-bw 1000
						     (lambda (w context info)
						       (set! notch-bw (.value info)))
						     1))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! notch-target target)
				    (XtSetSensitive (XmMessageBoxGetChild notch-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog notch-dialog))))
	     
	     (child (XtCreateManagedWidget "Band-reject filter" xmPushButtonWidgetClass filter-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-notch-dialog)))
	
	(set! filter-menu-list (cons (lambda ()
				       (change-label child (format #f "Band-reject filter (~,2F ~D)" notch-freq notch-bw)))
				     filter-menu-list))))
    
;;; -------- Butterworth high-pass filter
    
    (let* ((high-pass-freq 100)
	   (post-high-pass-dialog
	    (let ((high-pass-label "High-pass filter")
		  (high-pass-dialog #f)
		  (high-pass-target 'sound))
	      (lambda ()
		(unless (Widget? high-pass-dialog)
		  ;; if high-pass-dialog doesn't exist, create it
		  (let ((initial-high-pass-freq 100)
			(sliders ()))
		    (set! high-pass-dialog 
			  (make-effect-dialog 
			   high-pass-label
			   
			   (lambda (w context info)
			     (let ((flt (make-butter-high-pass high-pass-freq)))
			       (case high-pass-target 
				 ((sound)
				  (filter-sound flt #f #f #f #f (format #f "effects-bhp ~A 0 #f" high-pass-freq)))
				 ((selection)
				  (filter-selection flt))
				 (else
				  (let ((ms (plausible-mark-samples)))
				    (let ((bg (car ms))
					  (nd (- (+ (cadr ms) 1) (car ms))))
				      (clm-channel flt bg nd #f #f #f #f 
						   (format #f "effects-bhp ~A ~A ~A" high-pass-freq bg nd))))))))
			   
			   (lambda (w context info)
			     (help-dialog "High-pass filter"
					  "Butterworth high-pass filter. Move the slider to change the high-pass cutoff frequency."))
			   
			   (lambda (w c i)
			     (set! high-pass-freq initial-high-pass-freq)
			     (XtSetValues (car sliders) (list XmNvalue (scale-log->linear 20 high-pass-freq 22050))))
			   
			   (lambda () 
			     (effect-target-ok high-pass-target))))
		    
		    (set! sliders
			  (add-sliders high-pass-dialog
				       (list (list "high-pass cutoff frequency" 20 initial-high-pass-freq 22050
						   (lambda (w context info)
						     (set! high-pass-freq (scale-linear->log 20 (.value info) 22050)))
						   1 'log))))
		    (add-target (XtParent (car sliders)) 
				(lambda (target) 
				  (set! high-pass-target target)
				  (XtSetSensitive (XmMessageBoxGetChild high-pass-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				#f)))
		
		(activate-dialog high-pass-dialog))))
      
	   (child (XtCreateManagedWidget "High-pass filter" xmPushButtonWidgetClass filter-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-high-pass-dialog)))
      
      (set! filter-menu-list (cons (lambda ()
				     (change-label child (format #f "High-pass filter (~,2F)" high-pass-freq)))
				   filter-menu-list)))
    
    
;;; -------- Butterworth low-pass filter
    
    (let* ((low-pass-freq 1000)
	   (post-low-pass-dialog
	    (let ((low-pass-label "Low-pass filter")
		  (low-pass-dialog #f)
		  (low-pass-target 'sound))
	      (lambda ()
		(unless (Widget? low-pass-dialog)
		  ;; if low-pass-dialog doesn't exist, create it
		  (let ((initial-low-pass-freq 1000)
			(sliders ()))
		    (set! low-pass-dialog 
			  (make-effect-dialog 
			   low-pass-label
			   
			   (lambda (w context info)
			     (let ((flt (make-butter-low-pass low-pass-freq)))
			       (case low-pass-target 
				 ((sound)
				  (filter-sound flt #f #f #f #f (format #f "effects-blp ~A 0 #f" low-pass-freq)))
				 ((selection)
				  (filter-selection flt))
				 (else
				  (let ((ms (plausible-mark-samples)))
				    (let ((bg (car ms))
					  (nd (- (+ (cadr ms) 1) (car ms))))
				      (clm-channel flt bg nd #f #f #f #f 
						   (format #f "effects-blp ~A ~A ~A" low-pass-freq bg nd))))))))
			   
			   (lambda (w context info)
			     (help-dialog "Low-pass filter"
					  "Butterworth low-pass filter. Move the slider to change the low-pass cutoff frequency."))
			   
			   (lambda (w c i)
			     (set! low-pass-freq initial-low-pass-freq)
			     (XtSetValues (car sliders) (list XmNvalue (scale-log->linear 20 low-pass-freq 22050))))
			   
			   (lambda () 
			     (effect-target-ok low-pass-target))))
		    
		    (set! sliders
			  (add-sliders low-pass-dialog
				       (list (list "low-pass cutoff frequency" 20 initial-low-pass-freq 22050
						   (lambda (w context info)
						     (set! low-pass-freq (scale-linear->log 20 (.value info) 22050)))
						   1 'log))))
		    (add-target (XtParent (car sliders)) 
				(lambda (target) 
				  (set! low-pass-target target)
				  (XtSetSensitive (XmMessageBoxGetChild low-pass-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				#f)))
		
		(activate-dialog low-pass-dialog))))
      
	   (child (XtCreateManagedWidget "Low-pass filter" xmPushButtonWidgetClass filter-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-low-pass-dialog)))
      
      (set! filter-menu-list (cons (lambda ()
				     (change-label child (format #f "Low-pass filter (~,2F)" low-pass-freq)))
				   filter-menu-list)))
    
;;; more filters
    
;;; -------- Comb filter
;;;
;;; (truncate)
    
    (let ((comb-scaler 0.1)
	  (comb-size 50))
      (let* ((post-comb-dialog
	      (let ((comb-label "Comb filter")
		    (comb-dialog #f)
		    (comb-target 'sound))
		(lambda ()
		  (unless (Widget? comb-dialog)
		    ;; if comb-dialog doesn't exist, create it
		    (let ((initial-comb-scaler 0.1)
			  (initial-comb-size 50)
			  (sliders ()))
		      (set! comb-dialog 
			    (make-effect-dialog 
			     comb-label
			     
			     (lambda (w context info) 
			       (map-chan-over-target-with-sync
				(lambda (ignored) 
				  (let ((delay-line (make-float-vector comb-size))
					(delay-loc 0))
				    (lambda (x)
				      (let ((result (delay-line delay-loc)))
					(set! (delay-line delay-loc) (+ x (* comb-scaler result)))
					(set! delay-loc (+ 1 delay-loc))
					(if (= delay-loc comb-size) (set! delay-loc 0))
					result))))
				comb-target 
				(lambda (target samps)
				  (format #f "effects-comb-filter ~A ~A" comb-scaler comb-size))
				#f))
			     
			     (lambda (w context info)
			       (help-dialog "Comb filter"
					    "Move the sliders to change the comb scaler and size."))
			     
			     (lambda (w c i)
			       (set! comb-scaler initial-comb-scaler)
			       (XtSetValues (car sliders) (list XmNvalue (floor (* comb-scaler 100))))
			       (set! comb-size initial-comb-size)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor comb-size))))
			     
			     (lambda () 
			       (effect-target-ok comb-target))))
		      
		      (set! sliders
			    (add-sliders comb-dialog
					 (list (list "scaler" 0.0 initial-comb-scaler 1.0
						     (lambda (w context info)
						       (set! comb-scaler (/ (.value info) 100.0)))
						     100)
					       (list "size" 0 initial-comb-size 100
						     (lambda (w context info)
						       (set! comb-size (.value info)))
						     1))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! comb-target target)
				    (XtSetSensitive (XmMessageBoxGetChild comb-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog comb-dialog))))
	     
	     (child (XtCreateManagedWidget "Comb filter" xmPushButtonWidgetClass filter-menu
					   (list XmNbackground *basic-color*))))
	
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-comb-dialog)))
	
	(set! filter-menu-list (cons (lambda ()
				       (change-label child (format #f "Comb filter (~1,2F ~D)" comb-scaler comb-size)))
				     filter-menu-list))))
    
;;; -------- Comb-chord filter
;;;
;;; (truncate)
    
    (let ((new-comb-chord-scaler 0.95)
	  (new-comb-chord-size 60)
	  (new-comb-chord-amp 0.3)
	  (new-comb-chord-interval-one 0.75)
	  (new-comb-chord-interval-two 1.20))
      (let* ((post-new-comb-chord-dialog
	      (let ((new-comb-chord-label "Comb chord filter")
		    (new-comb-chord-dialog #f)
		    (new-comb-chord-target 'sound))
		(lambda ()
		  (unless (Widget? new-comb-chord-dialog)
		    ;; if new-comb-chord-dialog doesn't exist, create it
		    (let ((initial-new-comb-chord-scaler 0.95)
			  (initial-new-comb-chord-size 60)
			  (initial-new-comb-chord-amp 0.3)
			  (initial-new-comb-chord-interval-one 0.75)
			  (initial-new-comb-chord-interval-two 1.20)
			  (sliders ())
			  (new-comb-chord (lambda (scaler size amp interval-one interval-two)
					    ;; Comb chord filter: create chords by using filters at harmonically related sizes.
					    (let ((cs (make-comb-bank (vector (make-comb scaler size)
									      (make-comb scaler (floor (* size interval-one)))
									      (make-comb scaler (floor (* size interval-two)))))))
					      (lambda (x)
						(* amp (comb-bank cs x)))))))
		      (set! new-comb-chord-dialog
			    (make-effect-dialog 
			     new-comb-chord-label
			     
			     (lambda (w context info)
			       (map-chan-over-target-with-sync
				(lambda (ignored)
				  (new-comb-chord new-comb-chord-scaler new-comb-chord-size new-comb-chord-amp
						  new-comb-chord-interval-one new-comb-chord-interval-two))
				new-comb-chord-target
				(lambda (target samps)
				  (format #f "effects-comb-chord ~A ~A ~A ~A ~A" 
					  new-comb-chord-scaler new-comb-chord-size new-comb-chord-amp
					  new-comb-chord-interval-one new-comb-chord-interval-two))
				#f))
			     
			     (lambda (w context info)
			       (help-dialog "Comb chord filter"
					    "Creates chords by using filters at harmonically related sizes. Move the sliders to set the comb chord parameters."))
			     
			     (lambda (w c i)
			       (set! new-comb-chord-scaler initial-new-comb-chord-scaler)
			       (XtSetValues (sliders 0) (list XmNvalue (floor (* new-comb-chord-scaler 100))))
			       (set! new-comb-chord-size initial-new-comb-chord-size)
			       (XtSetValues (sliders 1) (list XmNvalue new-comb-chord-size))
			       (set! new-comb-chord-amp initial-new-comb-chord-amp)
			       (XtSetValues (sliders 2) (list XmNvalue (floor (* new-comb-chord-amp 100))))
			       (set! new-comb-chord-interval-one initial-new-comb-chord-interval-one)
			       (XtSetValues (sliders 3) (list XmNvalue (floor (* new-comb-chord-interval-one 100))))
			       (set! new-comb-chord-interval-two initial-new-comb-chord-interval-two)
			       (XtSetValues (sliders 4) (list XmNvalue (floor (* new-comb-chord-interval-two 100)))))
			     
			     (lambda () 
			       (effect-target-ok new-comb-chord-target))))
		      
		      (set! sliders
			    (add-sliders new-comb-chord-dialog
					 (list (list "chord scaler" 0.0 initial-new-comb-chord-scaler 1.0
						     (lambda (w context info)
						       (set! new-comb-chord-scaler (/ (.value info) 100.0)))
						     100)
					       (list "chord size" 0 initial-new-comb-chord-size 100
						     (lambda (w context info)
						       (set! new-comb-chord-size (.value info)))
						     1)
					       (list "amplitude" 0.0 initial-new-comb-chord-amp 1.0
						     (lambda (w context info)
						       (set! new-comb-chord-amp (/ (.value info) 100.0)))
						     100)
					       (list "interval one" 0.0 initial-new-comb-chord-interval-one 2.0
						     (lambda (w context info)
						       (set! new-comb-chord-interval-one (/ (.value info) 100.0)))
						     100)
					       (list "interval two" 0.0 initial-new-comb-chord-interval-two 2.0
						     (lambda (w context info)
						       (set! new-comb-chord-interval-two (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! new-comb-chord-target target)
				    (XtSetSensitive (XmMessageBoxGetChild new-comb-chord-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog new-comb-chord-dialog))))
	     
	     (child (XtCreateManagedWidget "Comb chord filter" xmPushButtonWidgetClass filter-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-new-comb-chord-dialog)))
	
	(set! filter-menu-list (cons (lambda ()
				       (change-label child 
						     (format #f "Comb chord filter (~1,2F ~D ~1,2F ~1,2F ~1,2F)"  
							     new-comb-chord-scaler new-comb-chord-size new-comb-chord-amp 
							     new-comb-chord-interval-one new-comb-chord-interval-two)))
				     filter-menu-list))))
      
;;; -------- Moog filter
;;;
    (let ((moog-cutoff-frequency 10000)
	  (moog-resonance 0.5))
      (let* ((post-moog-dialog
	      (let ((moog-dialog #f)
		    (moog-target 'sound)
		    (moog (lambda (freq Q)
			    (let ((gen (make-moog-filter freq Q)))
			      (lambda (inval)
				(moog-filter gen inval))))))
		(lambda ()
		  (unless (Widget? moog-dialog)
		    ;; if moog-dialog doesn't exist, create it
		    (let ((initial-moog-cutoff-frequency 10000)
			  (initial-moog-resonance 0.5)
			  (sliders ())
			  (moog-label "Moog filter"))
		      (set! moog-dialog 
			    (make-effect-dialog 
			     moog-label
			     
			     (lambda (w context info)
			       (map-chan-over-target-with-sync
				(lambda (ignored) (moog moog-cutoff-frequency moog-resonance)) 
				moog-target 
				(lambda (target samps)
				  (format #f "effects-moog-filter ~A ~A" moog-cutoff-frequency moog-resonance))
				#f))
			     
			     (lambda (w context info)
			       (help-dialog "Moog filter"
					    "Moog-style 4-pole lowpass filter with 24db/oct rolloff and variable resonance.
Move the sliders to set the filter cutoff frequency and resonance."))
			     
			     (lambda (w c i)
			       (set! moog-cutoff-frequency initial-moog-cutoff-frequency)
			       (XtSetValues (car sliders) (list XmNvalue (scale-log->linear 20 moog-cutoff-frequency 22050)))
			       (set! moog-resonance initial-moog-resonance)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor (* moog-resonance 100)))))
			     
			     (lambda () 
			       (effect-target-ok moog-target))))
		      
		      (set! sliders
			    (add-sliders moog-dialog
					 (list (list "cutoff frequency" 20 initial-moog-cutoff-frequency 22050
						     (lambda (w context info)
						       (set! moog-cutoff-frequency (scale-linear->log 20 (.value info) 22050)))
						     1 'log)
					       (list "resonance" 0.0 initial-moog-resonance 1.0
						     (lambda (w context info)
						       (set! moog-resonance (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! moog-target target)
				    (XtSetSensitive (XmMessageBoxGetChild moog-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog moog-dialog))))
	     
	     (child (XtCreateManagedWidget "Moog filter" xmPushButtonWidgetClass filter-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-moog-dialog)))
	
	(set! filter-menu-list (cons (lambda ()
				       (change-label child (format #f "Moog filter (~,2F ~1,2F)" moog-cutoff-frequency moog-resonance)))
				     filter-menu-list)))))
  

;;; FREQUENCY EFFECTS
;;;
  
  
  (let* ((freq-menu-list ())
	 (freq-menu (XmCreatePulldownMenu (main-menu effects-menu) "Frequency Effects"
					  (list XmNbackground *basic-color*)))
	 (freq-cascade (XtCreateManagedWidget "Frequency Effects" xmCascadeButtonWidgetClass (main-menu effects-menu)
					      (list XmNsubMenuId freq-menu
						    XmNbackground *basic-color*))))
    
    (XtAddCallback freq-cascade XmNcascadingCallback (lambda (w c i) (update-label freq-menu-list)))
    
;;; -------- Sample rate conversion (resample)
;;;
    
    (let* ((src-amount 0.0)
	   (post-src-dialog
	    (let ((src-label "Sample rate conversion")
		  (src-dialog #f)
		  (src-target 'sound))
	      (lambda ()
		(unless (Widget? src-dialog)
		  ;; if src-dialog doesn't exist, create it
		  (let ((initial-src-amount 0.0)
			(sliders ()))
		    (set! src-dialog
			  (make-effect-dialog 
			   src-label
			   
			   (lambda (w context info)		     
			     (case src-target 
			       ((sound)
				(src-sound src-amount))
			       ((selection)
				(if (selection?)
				    (src-selection src-amount)
				    (snd-print ";no selection")))
			       (else (snd-print "can't apply src between marks yet"))))
			   
			   (lambda (w context info)
			     (help-dialog "Sample rate conversion"
					  "Move the slider to change the sample rate. Values greater than 1.0 speed up file play, negative values reverse it."))
			   
			   (lambda (w c i)
			     (set! src-amount initial-src-amount)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* src-amount 100)))))
			   
			   (lambda () 
			     (effect-target-ok src-target))))
		    
		    (set! sliders
			  (add-sliders src-dialog
				       (list (list "sample rate" -2.0 initial-src-amount 2.0
						   (lambda (w context info)
						     (set! src-amount (/ (.value info) 100.0)))
						   100))))
		    (add-target (XtParent (car sliders)) 
				(lambda (target) 
				  (set! src-target target)
				  (XtSetSensitive (XmMessageBoxGetChild src-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				#f)))
		(activate-dialog src-dialog))))
	  
	   (child (XtCreateManagedWidget "Sample rate scaling" xmPushButtonWidgetClass freq-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-src-dialog)))
      
      (set! freq-menu-list (cons (lambda ()
				   (change-label child (format #f "Sample rate scaling (~1,2F)" src-amount)))
				 freq-menu-list)))
    
    
;;; -------- Time and pitch scaling by granular synthesis and sampling rate conversion
;;;
    
    (let ((time-scale 1.0)
	  (pitch-scale 1.0))
      (let *((post-expsrc-dialog
	      (let ((expsrc-label "Time/pitch scaling")
		    (expsrc-dialog #f)
		    (expsrc-target 'sound)
		    (hop-size 0.05)
		    (segment-length 0.15)
		    (ramp-scale 0.5))
		(lambda ()
		  (unless (Widget? expsrc-dialog)
		    (let ((initial-time-scale 1.0)
			  (initial-hop-size 0.05)
			  (initial-segment-length 0.15)
			  (initial-ramp-scale 0.5)
			  (initial-pitch-scale 1.0)
			  (sliders ()))
		      (set! expsrc-dialog 
			    (make-effect-dialog 
			     expsrc-label
			     
			     (lambda (w context info)
			       (let ((snd (selected-sound)))
				 (save-controls snd)
				 (reset-controls snd)
				 (set! (speed-control snd) pitch-scale)
				 (let ((new-time (* pitch-scale time-scale)))
				   (if (not (= new-time 1.0))
				       (begin
					 (set! (expand-control? snd) #t)
					 (set! (expand-control snd) new-time)
					 (set! (expand-control-hop snd) hop-size)
					 (set! (expand-control-length snd) segment-length)
					 (set! (expand-control-ramp snd) ramp-scale))))
				 (if (eq? expsrc-target 'marks)
				     (let ((ms (plausible-mark-samples)))
				       (apply-controls snd 0 (car ms) (- (+ (cadr ms) 1) (car ms))))
				     (apply-controls snd (if (eq? expsrc-target 'sound) 0 2)))
				 (restore-controls snd)))
			     
			     (lambda (w context info)
			       (help-dialog "Time/pitch scaling"
					    "Move the sliders to change the time/pitch scaling parameters."))
			     
			     (lambda (w c i)
			       (set! time-scale initial-time-scale)
			       (XtSetValues (sliders 0) (list XmNvalue (floor (* time-scale 100))))
			       (set! hop-size initial-hop-size)
			       (XtSetValues (sliders 1) (list XmNvalue (floor (* hop-size 100))))
			       (set! segment-length initial-segment-length)
			       (XtSetValues (sliders 2) (list XmNvalue (floor (* segment-length 100))))
			       (set! ramp-scale initial-ramp-scale)
			       (XtSetValues (sliders 3) (list XmNvalue (floor (* ramp-scale 100))))
			       (set! pitch-scale initial-pitch-scale)
			       (XtSetValues (sliders 4) (list XmNvalue (floor (* pitch-scale 100)))))
			     
			     (lambda () 
			       (effect-target-ok expsrc-target))))
		      
		      (set! sliders
			    (add-sliders expsrc-dialog
					 (list (list "time scale" 0.0 initial-time-scale 5.0
						     (lambda (w context info)
						       (set! time-scale (/ (.value info) 100.0)))
						     100)
					       (list "hop size" 0.0 initial-hop-size 1.0
						     (lambda (w context info)
						       (set! hop-size (/ (.value info) 100.0)))
						     100)
					       (list "segment length" 0.0 initial-segment-length 0.5
						     (lambda (w context info)
						       (set! segment-length (/ (.value info) 100.0)))
						     100)
					       (list "ramp scale" 0.0 initial-ramp-scale 0.5
						     (lambda (w context info)
						       (set! ramp-scale (/ (.value info) 100.0)))
						     1000)
					       (list "pitch scale" 0.0 initial-pitch-scale 5.0
						     (lambda (w context info)
						       (set! pitch-scale (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! expsrc-target target)
				    (XtSetSensitive (XmMessageBoxGetChild expsrc-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog expsrc-dialog))))
	     
	     (child (XtCreateManagedWidget "Time/pitch scaling" xmPushButtonWidgetClass freq-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-expsrc-dialog)))
	
	(set! freq-menu-list (cons (lambda ()
				     (change-label child (format #f "Time/pitch scaling (~1,2F ~1,2F)" time-scale pitch-scale)))
				   freq-menu-list))))
    
    
;;; -------- Time-varying sample rate conversion (resample)
;;; (KSM)
    
    (let* ((post-src-timevar-dialog
	    (let ((src-timevar-scale 1.0)
		  (src-timevar-label "Src-Timevar")
		  (src-timevar-dialog #f)
		  (src-timevar-target 'sound)
		  (src-timevar-envelope #f))
	      (define (scale-envelope e scl)
		(if (null? e)
		    ()
		    (cons (car e) (cons (* scl (cadr e)) (scale-envelope (cddr e) scl)))))
	      (lambda ()
		
		(if (Widget? src-timevar-dialog)
		    (activate-dialog src-timevar-dialog)
		    ;; if src-timevar-dialog doesn't exist, create it
		    (let ((sliders ()))
		      (let ((initial-src-timevar-scale 1.0))
			(set! src-timevar-dialog
			      (make-effect-dialog 
			       src-timevar-label
			       
			       (lambda (w context info)
				 (let ((env (scale-envelope (xe-envelope src-timevar-envelope)
							    src-timevar-scale)))
				   (case src-timevar-target
				     ((sound)
				      (src-sound env))
				     ((selection)
				      (if (selection-member? (selected-sound))
					  (src-selection env)
					  (display ";no selection")))
				     (else 
				      (let ((pts (plausible-mark-samples)))
					(if pts
					    (let* ((beg (car pts))
						   (len (- (cadr pts) beg)))
					      (src-channel (make-env env :length len) beg len (selected-sound)))))))))
			       
			       (lambda (w context info)
				 (help-dialog "Src-Timevar"
					      "Move the slider to change the src-timevar scaling amount."))
			       
			       (lambda (w c i)
				 (set! src-timevar-scale initial-src-timevar-scale)
				 (set! (xe-envelope src-timevar-envelope) (list 0.0 1.0 1.0 1.0))
				 (XtSetValues (car sliders) (list XmNvalue (* src-timevar-scale 100))))
			       
			       (lambda () 
				 (effect-target-ok src-timevar-target))))
			
			(set! sliders
			      (add-sliders src-timevar-dialog
					   (list (list "Resample factor" 0.0 initial-src-timevar-scale 10.0
						       (lambda (w context info)
							 (set! src-timevar-scale (/ (.value info) 100.0)))
						       100)))))
		      (let* ((fr (XtCreateManagedWidget "fr" xmFrameWidgetClass (XtParent (XtParent (car sliders)))
						      (list XmNheight              200
							    XmNleftAttachment      XmATTACH_FORM
							    XmNrightAttachment     XmATTACH_FORM
							    XmNtopAttachment       XmATTACH_WIDGET
							    XmNtopWidget           (sliders (- (length sliders) 1))
							    XmNshadowThickness     4
							    XmNshadowType          XmSHADOW_ETCHED_OUT)))
			     (target-row (add-target (XtParent (XtParent (car sliders))) 
						     (lambda (target) 
						       (set! src-timevar-target target)
						       (XtSetSensitive (XmMessageBoxGetChild src-timevar-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
						     #f)))
			(activate-dialog src-timevar-dialog)
			
			(set! src-timevar-envelope (xe-create-enved "src-timevar"  fr
								    (list XmNheight 200)
								    '(0.0 1.0 0.0 1.0)))
			(set! (xe-envelope src-timevar-envelope) (list 0.0 1.0 1.0 1.0))
			(XtVaSetValues fr (list XmNbottomAttachment XmATTACH_WIDGET
						XmNbottomWidget     target-row))))))))
      
	   (child (XtCreateManagedWidget "Time-varying sample rate scaling" xmPushButtonWidgetClass freq-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-src-timevar-dialog)))
      
      (set! freq-menu-list (cons (lambda ()
				   (change-label child "Time-varying sample rate scaling"))
				 freq-menu-list))))
  

;;; MODULATION EFFECTS
;;;
  
  (let* ((mod-menu-list ())
	 (mod-menu (XmCreatePulldownMenu (main-menu effects-menu) "Modulation Effects"
					 (list XmNbackground *basic-color*)))
	 (mod-cascade (XtCreateManagedWidget "Modulation Effects" xmCascadeButtonWidgetClass (main-menu effects-menu)
					     (list XmNsubMenuId mod-menu
						   XmNbackground *basic-color*))))
    
    (XtAddCallback mod-cascade XmNcascadingCallback (lambda (w c i) (update-label mod-menu-list)))
    
;;; -------- Amplitude modulation
;;;
    
    (let* ((am-effect-amount 100.0)
	   (post-am-effect-dialog
	    (let ((am-effect-label "Amplitude modulation")
		  (am-effect-envelope #f)
		  (am-effect-target 'sound)
		  (am-effect-dialog #f))
	      (define am-effect
		(lambda (freq)
		  (let ((os (make-oscil freq))
			(e (and (not (equal? (xe-envelope am-effect-envelope) '(0.0 1.0 1.0 1.0)))
				(make-env (xe-envelope am-effect-envelope) :length (effect-framples am-effect-target)))))
		    (if (env? e)
			(lambda (inval)
			  (amplitude-modulate 1.0 inval (* (env e) (oscil os))))
			(lambda (inval)
			  (amplitude-modulate 1.0 inval (oscil os)))))))
	      (lambda ()		
		(if (Widget? am-effect-dialog)
		    (activate-dialog am-effect-dialog)
		    ;; if am-effect-dialog doesn't exist, create it
		    (let ((sliders ()))
		      (let ((initial-am-effect-amount 100.0))
			(set! am-effect-dialog
			      (make-effect-dialog 
			       am-effect-label
			       
			       (lambda (w context info)
				 (map-chan-over-target-with-sync
				  (lambda (ignored) 
				    (am-effect am-effect-amount)) 
				  am-effect-target 
				  (lambda (target samps)
				    (format #f "effects-am ~A ~A" am-effect-amount
					    (let ((e (and (not (equal? (xe-envelope am-effect-envelope) '(0.0 1.0 1.0 1.0)))
							  (xe-envelope am-effect-envelope))))
					      (and e (format #f "'~A" e)))))
				  #f))
			       
			       (lambda (w context info)
				 (help-dialog "Amplitude modulation"
					      "Move the slider to change the modulation amount."))
			       
			       (lambda (w c i)
				 (set! am-effect-amount initial-am-effect-amount)
				 (set! (xe-envelope am-effect-envelope) (list 0.0 1.0 1.0 1.0))
				 (XtSetValues (car sliders) (list XmNvalue (floor am-effect-amount))))
			       
			       (lambda () 
				 (effect-target-ok am-effect-target))))
			
			(set! sliders
			      (add-sliders am-effect-dialog
					   (list (list "amplitude modulation" 0.0 initial-am-effect-amount 1000.0
						       (lambda (w context info)
							 (set! am-effect-amount (.value info)))
						       1)))))
		      (let* ((fr (XtCreateManagedWidget "fr" xmFrameWidgetClass (XtParent (XtParent (car sliders)))
						      (list XmNheight              200
							    XmNleftAttachment      XmATTACH_FORM
							    XmNrightAttachment     XmATTACH_FORM
							    XmNtopAttachment       XmATTACH_WIDGET
							    XmNtopWidget           (sliders (- (length sliders) 1))
							    XmNshadowThickness     4
							    XmNshadowType          XmSHADOW_ETCHED_OUT)))
			     (target-row (add-target (XtParent (XtParent (car sliders))) 
						     (lambda (target) 
						       (set! am-effect-target target)
						       (XtSetSensitive (XmMessageBoxGetChild am-effect-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
						     #f)))
			
			(activate-dialog am-effect-dialog)
			(set! am-effect-envelope (xe-create-enved "am"  fr
								  (list XmNheight 200)
								  '(0.0 1.0 0.0 1.0)))
			(set! (xe-envelope am-effect-envelope) (list 0.0 1.0 1.0 1.0))
			(XtVaSetValues fr (list XmNbottomAttachment XmATTACH_WIDGET
						XmNbottomWidget     target-row))))))))
      
	   (child (XtCreateManagedWidget "Amplitude modulation" xmPushButtonWidgetClass mod-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-am-effect-dialog)))
      
      (set! mod-menu-list (cons (lambda ()
				  (change-label child (format #f "Amplitude modulation (~1,2F)"  am-effect-amount)))
				mod-menu-list)))
    
;;; -------- Ring modulation
;;;
    
    (let ((rm-frequency 100)
	  (rm-radians 100))
      (let* ((post-rm-dialog
	      (let ((rm-label "Ring modulation")
		    (rm-target 'sound)
		    (rm-envelope #f)
		    (rm-dialog #f))
		(define rm-effect ; avoid collision with examp.scm
		  (lambda (freq gliss-env)
		    (let ((os (make-oscil freq))
			  (e (and (not (equal? (xe-envelope rm-envelope) '(0.0 1.0 1.0 1.0)))
				  (make-env (xe-envelope rm-envelope) :length (effect-framples rm-target)))))
		      (if (env? e)
			  (lambda (inval)
			    (* inval (env e) (oscil os)))
			  (lambda (inval)
			    (* inval (oscil os)))))))
		(lambda ()		  
		  (if (Widget? rm-dialog)
		      (activate-dialog rm-dialog)
		      ;; if rm-dialog doesn't exist, create it
		      (let ((initial-rm-frequency 100)
			    (sliders ()))
			(let ((initial-rm-radians 100))
			  (set! rm-dialog
				(make-effect-dialog 
				 rm-label
				 
				 (lambda (w context info)
				   (map-chan-over-target-with-sync
				    (lambda (ignored) 
				      (rm-effect rm-frequency #f)) ;(list 0 0 1 (hz->radians rm-radians)) -- gliss-env is not implemented above
				    rm-target 
				    (lambda (target samps)
				      (format #f "effects-rm ~A ~A" rm-frequency
					      (let ((e (and (not (equal? (xe-envelope rm-envelope) '(0.0 1.0 1.0 1.0)))
							    (xe-envelope rm-envelope))))
						(and e (format #f "'~A" e)))))
				    #f))
				 
				 (lambda (w context info)
				   (help-dialog "Ring modulation"
						"Move the slider to change the ring modulation parameters."))
				 
				 (lambda (w c i)
				   (set! rm-frequency initial-rm-frequency)
				   (set! (xe-envelope rm-envelope) (list 0.0 1.0 1.0 1.0))
				   (XtSetValues (car sliders) (list XmNvalue rm-frequency))
				   (set! rm-radians initial-rm-radians)
				   (XtSetValues (cadr sliders) (list XmNvalue rm-radians)))
				 
				 (lambda () 
				   (effect-target-ok rm-target))))
			  
			  (set! sliders
				(add-sliders rm-dialog
					     (list 
					      (list "modulation frequency" 0 initial-rm-frequency 1000
						    (lambda (w context info)
						      (set! rm-frequency (.value info)))
						    1)
					      (list "modulation radians" 0 initial-rm-radians 360
						    (lambda (w context info)
						      (set! rm-radians (.value info)))
						    1)))))
			(let* ((fr (XtCreateManagedWidget "fr" xmFrameWidgetClass (XtParent (XtParent (car sliders)))
							  (list XmNheight              200
								XmNleftAttachment      XmATTACH_FORM
								XmNrightAttachment     XmATTACH_FORM
								XmNtopAttachment       XmATTACH_WIDGET
								XmNtopWidget           (sliders (- (length sliders) 1))
								XmNshadowThickness     4
								XmNshadowType          XmSHADOW_ETCHED_OUT)))
			       (target-row (add-target (XtParent (XtParent (car sliders))) 
						       (lambda (target) 
							 (set! rm-target target)
							 (XtSetSensitive (XmMessageBoxGetChild rm-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
						       #f)))
			  
			  (activate-dialog rm-dialog)
			  (set! rm-envelope (xe-create-enved "rm frequency"  fr
							     (list XmNheight 200)
							     '(0.0 1.0 0.0 1.0)))
			  (set! (xe-envelope rm-envelope) (list 0.0 1.0 1.0 1.0))
			  (XtVaSetValues fr (list XmNbottomAttachment XmATTACH_WIDGET
						  XmNbottomWidget     target-row))))))))
	     
	     (child (XtCreateManagedWidget "Ring modulation" xmPushButtonWidgetClass mod-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-rm-dialog)))
	
	(set! mod-menu-list (cons (lambda ()
				    (change-label child (format #f "Ring modulation (~D ~D)" rm-frequency rm-radians)))
				  mod-menu-list)))))
    
    
;;; REVERBS
;;;
  
  (define effects-cnv 
    (let ((+documentation+ "(effects-cnv snd0-1 amp snd chn) is used by the effects dialog to tie into edit-list->function"))
      (lambda* (snd0-1 amp snd chn)
	(let* ((snd0 (if (sound? snd0-1) snd0-1 (car (sounds))))
	       (flt-len (framples snd0))
	       (total-len (+ flt-len (framples snd chn)))
	       (cnv (make-convolve :filter (channel->float-vector 0 flt-len snd0)
				   :input (make-sampler 0 snd chn)))
	       (out-data (make-float-vector total-len)))
	  (do ((i 0 (+ i 1)))
	      ((= i total-len))
	    (set! (out-data i) (convolve cnv)))
	  (float-vector-scale! out-data amp)
	  (let ((max-samp (float-vector-peak out-data)))
	    (float-vector->channel out-data 0 total-len snd chn #f (format #f "effects-cnv ~A ~A" snd0 amp))
	    (if (> max-samp 1.0) (set! (y-bounds snd chn) (list (- max-samp) max-samp)))
	    max-samp)))))
  
  
  (define effects-jc-reverb 
    (let ((+documentation+ "(effects-jc-reverb volume beg dur snd chn) is used by the effects dialog to tie into edit-list->function"))
      (lambda* (volume beg dur snd chn)
	(let ((allpass1 (make-all-pass -0.700 0.700 1051))
	      (allpass2 (make-all-pass -0.700 0.700  337))
	      (allpass3 (make-all-pass -0.700 0.700  113))
	      (comb1 (make-comb 0.742 4799))
	      (comb2 (make-comb 0.733 4999))
	      (comb3 (make-comb 0.715 5399))
	      (comb4 (make-comb 0.697 5801))
	      (outdel1 (make-delay (round (* .013 (srate)))))
	      (e (make-env '(0 1 1 0) :scaler volume :base 0.0 :length dur)))
	  (let ((combs (make-comb-bank (vector comb1 comb2 comb3 comb4)))
		(allpasses (make-all-pass-bank (vector allpass1 allpass2 allpass3))))
	    (lambda (inval)
	      (+ inval
		 (delay outdel1 (comb-bank combs (all-pass-bank allpasses (* inval (env e))))))))))))
  
  (let ((reverb-menu-list ())
	(reverb-menu (XmCreatePulldownMenu (main-menu effects-menu) "Reverbs"
					   (list XmNbackground *basic-color*))))
    (let ((reverb-cascade (XtCreateManagedWidget "Reverbs" xmCascadeButtonWidgetClass (main-menu effects-menu)
						 (list XmNsubMenuId reverb-menu
						       XmNbackground *basic-color*))))
      (XtAddCallback reverb-cascade XmNcascadingCallback (lambda (w c i) (update-label reverb-menu-list))))
    
    
;;; -------- Reverb from Michael McNabb's Nrev 
;;; -------- very nice reverb actually
;;;
;;; (truncate)
    
    (let ((reverb-amount 0.1)
	  (reverb-filter 0.5)
	  (reverb-feedback 1.09))    ;; add reverb-control-decay (with ramp?) and reverb-truncate
      (let* ((post-reverb-dialog
	      (let ((reverb-label "McNabb reverb")
		    (reverb-dialog #f)
		    (reverb-target 'sound))
		(lambda ()
		  (unless (Widget? reverb-dialog)
		    ;; if reverb-dialog doesn't exist, create it
		    (let ((initial-reverb-amount 0.1)
			  (initial-reverb-filter 0.5)
			  (initial-reverb-feedback 1.09)
			  (sliders ()))
		      (set! reverb-dialog 
			    (make-effect-dialog 
			     reverb-label
			     
			     (lambda (w context info)
			       (let ((snd (selected-sound)))
				 (save-controls snd)
				 (reset-controls snd)
				 (set! (reverb-control? snd) #t)
				 (set! (reverb-control-scale snd) reverb-amount)
				 (set! (reverb-control-lowpass snd) reverb-filter)
				 (set! (reverb-control-feedback snd) reverb-feedback)
				 (if (eq? reverb-target 'marks)
				     (let ((ms (plausible-mark-samples)))
				       (apply-controls snd 0 (car ms) (- (+ (cadr ms) 1) (car ms))))
				     (apply-controls snd (if (eq? reverb-target 'sound) 0 2)))
				 (restore-controls snd)))
			     
			     (lambda (w context info)
			       (help-dialog "McNabb reverb"
					    "Reverberator from Michael McNabb. 
Adds reverberation scaled by reverb amount, lowpass filtering, and feedback. Move the sliders to change the reverb parameters."))
			     
			     (lambda (w c i)
			       (set! reverb-amount initial-reverb-amount)
			       (XtSetValues (car sliders) (list XmNvalue (floor (* reverb-amount 100))))
			       (set! reverb-filter initial-reverb-filter)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor (* reverb-filter 100))))
			       (set! reverb-feedback initial-reverb-feedback)
			       (XtSetValues (caddr sliders) (list XmNvalue (floor (* reverb-feedback 100)))))
			     
			     (lambda () 
			       (effect-target-ok reverb-target))))
		      
		      (set! sliders
			    (add-sliders reverb-dialog
					 (list (list "reverb amount" 0.0 initial-reverb-amount 1.0
						     (lambda (w context info)
						       (set! reverb-amount (/ (.value info) 100.0)))
						     100)
					       (list "reverb filter" 0.0 initial-reverb-filter 1.0
						     (lambda (w context info)
						       (set! reverb-filter (/ (.value info) 100.0)))
						     100)
					       (list "reverb feedback" 0.0 initial-reverb-feedback 1.25
						     (lambda (w context info)
						       (set! reverb-feedback (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! reverb-target target)
				    (XtSetSensitive (XmMessageBoxGetChild reverb-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog reverb-dialog))))
	     
	     (child (XtCreateManagedWidget "McNabb reverb" xmPushButtonWidgetClass reverb-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-reverb-dialog)))
	
	(set! reverb-menu-list (cons (lambda ()
				       (change-label child (format #f "McNabb reverb (~1,2F ~1,2F ~1,2F)" 
								   reverb-amount reverb-filter reverb-feedback)))
				     reverb-menu-list))))
    
    
;;; -------- Chowning reverb
;;;
    
    (let ((jc-reverb-decay 2.0)
	  (jc-reverb-volume 0.1))
      (let* ((post-jc-reverb-dialog
	      (let ((jc-reverb-label "Chowning reverb")
		    (jc-reverb-dialog #f)
		    (jc-reverb-target 'sound)
		    (jc-reverb-truncate #f))
		(lambda ()
		  (unless (Widget? jc-reverb-dialog)
		    ;; if jc-reverb-dialog doesn't exist, create it
		    (let ((initial-jc-reverb-decay 2.0)
			  (initial-jc-reverb-volume 0.1)
			  (sliders ()))
		      (set! jc-reverb-dialog
			    (make-effect-dialog 
			     jc-reverb-label
			     
			     (lambda (w context info) 
					;(pad-channel (- (framples) 1) (srate))
			       (map-chan-over-target-with-sync
				(lambda (samps) 
				  (effects-jc-reverb jc-reverb-volume samps (framples)))
				jc-reverb-target 
				(lambda (target samps) 
				  (format #f "effects-jc-reverb ~A" jc-reverb-volume))
				(and (not jc-reverb-truncate) jc-reverb-decay)))
			     
			     (lambda (w context info)
			       (help-dialog "Chowning reverb"
					    "Nice reverb from John Chowning. Move the sliders to set the reverb parameters."))
			     
			     (lambda (w c i)
			       (set! jc-reverb-decay initial-jc-reverb-decay)
			       (XtSetValues (sliders 0) (list XmNvalue (floor (* jc-reverb-decay 100))))
			       (set! jc-reverb-volume initial-jc-reverb-volume)
			       (XtSetValues (sliders 1) (list XmNvalue (floor (* jc-reverb-volume 100)))))
			     
			     (lambda () 
			       (effect-target-ok jc-reverb-target))))
		      
		      (set! sliders
			    (add-sliders jc-reverb-dialog
					 (list (list "decay duration" 0.0 initial-jc-reverb-decay 10.0
						     (lambda (w context info)
						       (set! jc-reverb-decay (/ (.value info) 100)))
						     100)
					       (list "reverb volume" 0.0 initial-jc-reverb-volume 1.0
						     (lambda (w context info)
						       (set! jc-reverb-volume (/ (.value info) 100)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! jc-reverb-target target)
				    (XtSetSensitive (XmMessageBoxGetChild jc-reverb-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  (lambda (truncate) 
				    (set! jc-reverb-truncate truncate)))))
		  (activate-dialog jc-reverb-dialog))))
	     
	     (child (XtCreateManagedWidget "Chowning reverb" xmPushButtonWidgetClass reverb-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-jc-reverb-dialog)))
	
	(set! reverb-menu-list (cons (lambda ()
				       (change-label child (format #f "Chowning reverb (~1,2F ~1,2F)" jc-reverb-decay jc-reverb-volume)))
				     reverb-menu-list))))
    
;;; -------- Convolution
;;;
;;; (progress report? truncate?)
    
    (let ((convolve-sound-one 0)
	  (convolve-sound-two 1)
	  (convolve-amp 0.01))
      (let* ((post-convolve-dialog
	      (let ((convolve-label "Convolution")
		    (convolve-dialog #f))
		(lambda ()
		  (unless (Widget? convolve-dialog)
		    ;; if convolve-dialog doesn't exist, create it
		    (let ((initial-convolve-sound-one 0)
			  (initial-convolve-sound-two 1)
			  (initial-convolve-amp 0.01)
			  (sliders ()))
		      (set! convolve-dialog
			    (make-effect-dialog 
			     convolve-label
			     
			     (lambda (w context info) 
			       (effects-cnv convolve-sound-one convolve-amp convolve-sound-two))
			     
			     (lambda (w context info)
			       (help-dialog "Convolution"
					    "Very simple convolution. Move the sliders to set the numbers of the soundfiles to be convolved and the amount for the amplitude scaler. Output will be scaled to floating-point values, resulting in very large (but not clipped) amplitudes. Use the Normalize amplitude effect to rescale the output. The convolution data file typically defines a natural reverberation source, and the output from this effect can provide very striking reverb effects. You can find convolution data files on sites listed at http://www.bright.net/~dlphilp/linux_csound.html under Impulse Response Data."))
			     
			     (lambda (w c i)
			       (set! convolve-sound-one initial-convolve-sound-one)
			       (XtSetValues (sliders 0) (list XmNvalue convolve-sound-one))
			       (set! convolve-sound-two initial-convolve-sound-two)
			       (XtSetValues (sliders 1) (list XmNvalue convolve-sound-two))
			       (set! convolve-amp initial-convolve-amp)
			       (XtSetValues (sliders 2) (list XmNvalue (floor (* convolve-amp 100)))))
			     
			     (lambda () 
			       (pair? (sounds)))))
		      
		      (set! sliders
			    (add-sliders convolve-dialog
					 (list (list "impulse response file" 0 initial-convolve-sound-one 24
						     (lambda (w context info)
						       (set! convolve-sound-one (.value info)))
						     1)
					       (list "sound file" 0 initial-convolve-sound-two 24
						     (lambda (w context info)
						       (set! convolve-sound-two (.value info)))
						     1)
					       (list "amplitude" 0.0 initial-convolve-amp 0.10
						     (lambda (w context info)
						       (set! convolve-amp (/ (.value info) 100.0)))
						     1000))))))
		  (activate-dialog convolve-dialog))))
	     
	     (child (XtCreateManagedWidget "Convolution" xmPushButtonWidgetClass reverb-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-convolve-dialog)))
	
	(set! reverb-menu-list (cons (lambda ()
				       (change-label child (format #f "Convolution (~D ~D ~1,2F)" 
								   convolve-sound-one convolve-sound-two convolve-amp)))
				     reverb-menu-list)))))
    
;;; VARIOUS AND MISCELLANEOUS
;;;
  
  (define effects-hello-dentist 
    (let ((+documentation+ "(hello-dentist frq amp snd chn) varies the sampling rate randomly, making a voice sound quavery: (hello-dentist 40.0 .1)"))
      (lambda* (frq amp beg dur snd chn)
	(let ((rn (make-rand-interp :frequency frq :amplitude amp))
	      (len (or dur (- (framples snd chn) beg)))
	      (rd (make-src :srate 1.0 
			    :input (let ((sf (make-sampler beg snd chn)))
				     (lambda (dir) 
				       (read-sample-with-direction sf dir))))))
	  (map-channel
	   (lambda (y)
	     (src rd (rand-interp rn)))
	   beg len snd chn #f (format #f "effects-hello-dentist ~A ~A ~A ~A" frq amp beg (and (not (= len (framples snd chn))) len)))))))
  
  
  (define* (effects-fp sr osamp osfrq beg dur snd chn)
    (let ((os (make-oscil osfrq))
	  (len (framples snd chn))
	  (s (make-src :srate sr
		       :input (let ((sf (make-sampler beg snd chn)))
				(lambda (dir) 
				  (read-sample-with-direction sf dir))))))
      (map-channel
       (lambda (y)
	 (src s (* osamp (oscil os))))
       beg len snd chn #f (format #f "effects-fp ~A ~A ~A ~A ~A" sr osamp osfrq beg (and (not (= len (framples snd chn))) len)))))
  
  
  (define effects-position-sound 
    (let ((+documentation+ "(effects-position-sound mono-snd pos-1 snd chn) is used by the effects dialog to tie into edit-list->function"))
      (lambda* (mono-snd pos snd chn)
	(let ((len (framples mono-snd))
	      (reader1 (make-sampler 0 mono-snd)))
	  (if (number? pos)
	      (map-channel (lambda (y)
			     (+ y (* pos (read-sample reader1))))
			   0 len snd chn #f
			   (format #f "effects-position-sound ~A ~A" mono-snd pos))
	      (let ((ef (let ((e1 (make-env pos :length len)))
			  (if (eqv? chn 1)
			      (lambda (y)
				(+ y (* (env e1) (read-sample reader1))))
			      (lambda (y)
				(+ y (* (- 1.0 (env e1)) (read-sample reader1))))))))
		(map-channel ef 0 len snd chn #f (format #f "effects-position-sound ~A '~A" mono-snd pos))))))))

  
  (define effects-cross-synthesis 
    (let ((+documentation+ "(effects-cross-synthesis cross-snd amp fftsize r) is used by the effects dialog to tie into edit-list->function"))
      (lambda (cross-snd amp fftsize r)
	;; cross-snd is the index of the other sound (as opposed to the map-channel sound)
	(let ((freq-inc (/ fftsize 2)))
	  (let ((fdr (make-float-vector fftsize))
		(fdi (make-float-vector fftsize))
		(spectr (make-float-vector freq-inc))
		(inctr 0)
		(ctr freq-inc)
		(formants (make-vector freq-inc)))
	    (do ((radius (- 1.0 (/ r fftsize)))
		 (bin (/ (srate) fftsize))
		 (i 0 (+ 1 i)))
		((= i freq-inc))
	      (set! (formants i) (make-formant (* i bin) radius)))
	    (set! formants (make-formant-bank formants spectr))
	    (lambda (inval)
	      (if (= ctr freq-inc)
		  (begin
		    (set! fdr (channel->float-vector inctr fftsize cross-snd 0))
		    (set! inctr (+ inctr freq-inc))
		    (spectrum fdr fdi #f 2)
		    (float-vector-subtract! fdr spectr)
		    (float-vector-scale! fdr (/ 1.0 freq-inc))
		    (set! ctr 0)))
	      (set! ctr (+ ctr 1))
	      (float-vector-add! spectr fdr)
	      (* amp (formant-bank formants inval))))))))

  (let ((misc-menu-list ())
	(misc-menu (XmCreatePulldownMenu (main-menu effects-menu) "Various"
					 (list XmNbackground *basic-color*))))
    (let ((misc-cascade (XtCreateManagedWidget "Various" xmCascadeButtonWidgetClass (main-menu effects-menu)
					       (list XmNsubMenuId misc-menu
						     XmNbackground *basic-color*))))
    
      (XtAddCallback misc-cascade XmNcascadingCallback (lambda (w c i) (update-label misc-menu-list))))
    
    
;;; -------- Place sound
;;;
    
    (let ((mono-snd 0)
	  (stereo-snd 1)
	  (pan-pos 45))
      (let* ((post-place-sound-dialog
	      (let ((place-sound-label "Place sound")
		    (place-sound-dialog #f)
		    (place-sound-target 'sound)
		    (place-sound-envelope #f)
		    
		    (place-sound 
		     (lambda (mono-snd stereo-snd pan-env)
		       ;; (place-sound mono-snd stereo-snd pan-env) mixes a mono sound into a stereo sound, splitting 
		       ;; it into two copies whose amplitudes depend on the envelope 'pan-env'.  If 'pan-env' is 
		       ;; a number, the sound is split such that 0 is all in channel 0 and 90 is all in channel 1.
		       (if (number? pan-env)
			   (let ((pos (/ pan-env 90.0)))
			     (effects-position-sound mono-snd pos stereo-snd 1)
			     (effects-position-sound mono-snd (- 1.0 pos) stereo-snd 0))
			   (begin
			     (effects-position-sound mono-snd pan-env stereo-snd 1)
			     (effects-position-sound mono-snd pan-env stereo-snd 0))))))
		
		(lambda ()
		  (unless (Widget? place-sound-dialog)
		    (let ((fr #f))
		      (let ((sliders ())
			    (initial-mono-snd 0)
			    (initial-stereo-snd 1)
			    (initial-pan-pos 45))
			(set! place-sound-dialog 
			      (make-effect-dialog 
			       place-sound-label
			       
			       (lambda (w context info) 
				 (let ((e (xe-envelope place-sound-envelope)))
				   (place-sound mono-snd stereo-snd (if (not (equal? e '(0.0 1.0 1.0 1.0))) e pan-pos))))
			       
			       (lambda (w context info)
				 (help-dialog "Place sound"
					      "Mixes mono sound into stereo sound field."))
			       
			       (lambda (w c i)
				 (set! mono-snd initial-mono-snd)
				 (XtSetValues (sliders 0) (list XmNvalue mono-snd))
				 (set! stereo-snd initial-stereo-snd)
				 (XtSetValues (sliders 1) (list XmNvalue stereo-snd))
				 (set! pan-pos initial-pan-pos)
				 (XtSetValues (sliders 2) (list XmNvalue pan-pos)))
			       
			       (lambda () 
				 (effect-target-ok place-sound-target))))
			
			(set! sliders
			      (add-sliders place-sound-dialog
					   (list (list "mono sound" 0 initial-mono-snd 50
						       (lambda (w context info)
							 (set! mono-snd (.value info)))
						       1)
						 (list "stereo sound" 0 initial-stereo-snd 50
						       (lambda (w context info)
							 (set! stereo-snd (.value info)))
						       1)
						 (list "pan position" 0 initial-pan-pos 90
						       (lambda (w context info)
							 (set! pan-pos (.value info)))
						       1))))
			(set! fr (XtCreateManagedWidget "fr" xmFrameWidgetClass (XtParent (XtParent (car sliders)))
							(list XmNheight              200
							      XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_WIDGET
							      XmNbottomAttachment    XmATTACH_FORM
							      XmNtopWidget           (sliders (- (length sliders) 1))
							      XmNshadowThickness     4
							      XmNshadowType          XmSHADOW_ETCHED_OUT))))
		      (activate-dialog place-sound-dialog)
		      (set! place-sound-envelope (xe-create-enved "panning"  fr
								  (list XmNheight 200
									XmNbottomAttachment XmATTACH_FORM)
								  '(0.0 1.0 0.0 1.0)))
		      (set! (xe-envelope place-sound-envelope) (list 0.0 1.0 1.0 1.0))))
		  
		  (activate-dialog place-sound-dialog))))
	     
	     (child (XtCreateManagedWidget "Place sound" xmPushButtonWidgetClass misc-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-place-sound-dialog)))
	
	(set! misc-menu-list (cons (lambda ()
				     (change-label child (format #f "Place sound (~D ~D ~D)" mono-snd stereo-snd pan-pos)))
				   misc-menu-list))))
    
    
;;; -------- Insert silence (at cursor, silence-amount in secs)
;;;
    
    (let* ((silence-amount 1.0)
	   (post-silence-dialog
	    (let ((silence-label "Add silence")
		  (silence-dialog #f))
	      (lambda ()
		(unless (Widget? silence-dialog)
		  ;; if silence-dialog doesn't exist, create it
		  (let ((initial-silence-amount 1.0)
			(sliders ()))
		    (set! silence-dialog
			  (make-effect-dialog 
			   silence-label
			   
			   (lambda (w context info)
			     (insert-silence (cursor)
					     (floor (* (srate) silence-amount))))
			   
			   (lambda (w context info)
			     (help-dialog "Add silence"
					  "Move the slider to change the number of seconds of silence added at the cursor position."))
			   
			   (lambda (w c i)
			     (set! silence-amount initial-silence-amount)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* silence-amount 100)))))
			   
			   (lambda ()
			     (pair? (sounds)))))
		    
		    (set! sliders
			  (add-sliders silence-dialog
				       (list (list "silence" 0.0 initial-silence-amount 5.0
						   (lambda (w context info)
						     (set! silence-amount (/ (.value info) 100.0)))
						   100))))))
		(activate-dialog silence-dialog))))
      
	   (child (XtCreateManagedWidget "Add silence" xmPushButtonWidgetClass misc-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-silence-dialog)))
      
      (set! misc-menu-list (cons (lambda ()
				   (change-label child (format #f "Add silence (~1,2F)" silence-amount)))
				 misc-menu-list)))
    
    
;;; -------- Contrast (brightness control)
;;;
    
    (let* ((contrast-amount 1.0)
	   (post-contrast-dialog
	    (let ((contrast-label "Contrast enhancement")
		  (contrast-dialog #f)
		  (contrast-target 'sound))
	      (lambda ()
		(unless (Widget? contrast-dialog)
		  ;; if contrast-dialog doesn't exist, create it
		  (let ((initial-contrast-amount 1.0)
			(sliders ()))
		    (set! contrast-dialog
			  (make-effect-dialog 
			   contrast-label
			   
			   (lambda (w context info)
			     (let ((peak (maxamp))
				   (snd (selected-sound)))
			       (save-controls snd)
			       (reset-controls snd)
			       (set! (contrast-control? snd) #t)
			       (set! (contrast-control snd) contrast-amount)
			       (set! (contrast-control-amp snd) (/ 1.0 peak))
			       (set! (amp-control snd) peak)
			       (if (eq? contrast-target 'marks)
				   (let ((ms (plausible-mark-samples)))
				     (apply-controls snd 0 (car ms) (- (+ (cadr ms) 1) (car ms))))
				   (apply-controls snd (if (eq? contrast-target 'sound) 0 2)))
			       (restore-controls snd)))
			   
			   (lambda (w context info)
			     (help-dialog "Contrast enhancement"
					  "Move the slider to change the contrast intensity."))
			   
			   (lambda (w c i)
			     (set! contrast-amount initial-contrast-amount)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* contrast-amount 100)))))
			   
			   (lambda () 
			     (effect-target-ok contrast-target))))
		    
		    (set! sliders
			  (add-sliders contrast-dialog
				       (list (list "contrast enhancement" 0.0 initial-contrast-amount 10.0
						   (lambda (w context info)
						     (set! contrast-amount (/ (.value info) 100.0)))
						   100))))
		    (add-target (XtParent (car sliders)) 
				(lambda (target) 
				  (set! contrast-target target)
				  (XtSetSensitive (XmMessageBoxGetChild contrast-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				#f)))
		
		(activate-dialog contrast-dialog))))
      
	   (child (XtCreateManagedWidget "Contrast enhancement" xmPushButtonWidgetClass misc-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-contrast-dialog)))
      
      (set! misc-menu-list (cons (lambda ()
				   (change-label child (format #f "Contrast enhancement (~1,2F)" contrast-amount)))
				 misc-menu-list)))
    
;;; -------- Cross synthesis
;;;
    
    (let ((cross-synth-sound 1)
	  (cross-synth-amp .5)
	  (cross-synth-fft-size 128)
	  (cross-synth-radius 6.0))
      (let* ((post-cross-synth-dialog
	      (let ((cross-synth-label "Cross synthesis")
		    (cross-synth-dialog #f)
		    (cross-synth-default-fft-widget #f)
		    (cross-synth-target 'sound))
		(lambda ()
		  (unless (Widget? cross-synth-dialog)
		    ;; if cross-synth-dialog doesn't exist, create it
		    (let ((sliders ()))
		      (let ((initial-cross-synth-sound 1)
			    (initial-cross-synth-amp .5)
			    (initial-cross-synth-radius 6.0))
			(let ((initial-cross-synth-fft-size 128))
			  (set! cross-synth-dialog
				(make-effect-dialog 
				 cross-synth-label
				 
				 (lambda (w context info)
				   (map-chan-over-target-with-sync
				    (lambda (ignored) 
				      (effects-cross-synthesis cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius))
				    cross-synth-target 
				    (lambda (target samps)
				      (format #f "effects-cross-synthesis ~A ~A ~A ~A"
					      cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius))
				    #f))
				 
				 (lambda (w context info)
				   (help-dialog "Cross synthesis"
						"The sliders set the number of the soundfile to be cross-synthesized, 
the synthesis amplitude, the FFT size, and the radius value."))
				 
				 (lambda (w c i)
				   (set! cross-synth-sound initial-cross-synth-sound)
				   (XtSetValues (sliders 0) (list XmNvalue cross-synth-sound))
				   (set! cross-synth-amp initial-cross-synth-amp)
				   (XtSetValues (sliders 1) (list XmNvalue (floor (* cross-synth-amp 100))))
				   (set! cross-synth-fft-size initial-cross-synth-fft-size)
				   (if use-combo-box-for-fft-size ; defined in effects-utils.scm
				       (XtSetValues cross-synth-default-fft-widget (list XmNselectedPosition 1))
				       (XmToggleButtonSetState cross-synth-default-fft-widget #t #t))
				   (set! cross-synth-radius initial-cross-synth-radius)
				   (XtSetValues (sliders 2) (list XmNvalue (floor (* cross-synth-radius 100)))))
				 
				 (lambda () 
				   (effect-target-ok cross-synth-target)))))
			
			(set! sliders
			      (add-sliders cross-synth-dialog
					   (list (list "input sound" 0 initial-cross-synth-sound 20
						       (lambda (w context info)
							 (set! cross-synth-sound (.value info)))
						       1)
						 (list "amplitude" 0.0 initial-cross-synth-amp 1.0
						       (lambda (w context info)
							 (set! cross-synth-amp (/ (.value info) 100)))
						       100)
						 (list "radius" 0.0 initial-cross-synth-radius 360.0
						       (lambda (w context info)
							 (set! cross-synth-radius (/ (.value info) 100)))
						       100)))))
		      
		      ;; now add either a radio-button box or a combo-box for the fft size
		      ;;   need to use XtParent here since "mainform" isn't returned by add-sliders
		      
		      (if use-combo-box-for-fft-size
			  ;; this block creates a "combo box" to handle the fft size
			  (let* ((s1 (XmStringCreateLocalized "FFT size"))
				 (frm (let ((frame (XtCreateManagedWidget "frame" xmFrameWidgetClass (XtParent (car sliders))
									  (list XmNborderWidth 1
										XmNshadowType XmSHADOW_ETCHED_IN
										XmNpositionIndex 2))))
					(XtCreateManagedWidget "frm" xmFormWidgetClass frame
							       (list XmNleftAttachment      XmATTACH_FORM
								     XmNrightAttachment     XmATTACH_FORM
								     XmNtopAttachment       XmATTACH_FORM
								     XmNbottomAttachment    XmATTACH_FORM
								     XmNbackground          *basic-color*))))
				 (lab (XtCreateManagedWidget "FFT size" xmLabelWidgetClass frm
							     (list XmNleftAttachment      XmATTACH_FORM
								   XmNrightAttachment     XmATTACH_NONE
								   XmNtopAttachment       XmATTACH_FORM
								   XmNbottomAttachment    XmATTACH_FORM
								   XmNlabelString         s1
								   XmNbackground          *basic-color*)))
				 (fft-labels (map XmStringCreateLocalized '("64" "128" "256" "512" "1024" "4096")))
				 (combo (XtCreateManagedWidget "fftsize" xmComboBoxWidgetClass frm
							       (list XmNleftAttachment      XmATTACH_WIDGET
								     XmNleftWidget          lab
								     XmNrightAttachment     XmATTACH_FORM
								     XmNtopAttachment       XmATTACH_FORM
								     XmNbottomAttachment    XmATTACH_FORM
								     XmNitems               fft-labels
								     XmNitemCount           (length fft-labels)
								     XmNcomboBoxType        XmDROP_DOWN_COMBO_BOX
								     XmNbackground          *basic-color*))))
			    (set! cross-synth-default-fft-widget combo)
			    (for-each XmStringFree fft-labels)
			    (XmStringFree s1)
			    (XtSetValues combo (list XmNselectedPosition 1))
			    (XtAddCallback combo XmNselectionCallback
					   (lambda (w c i)
					     (set! cross-synth-fft-size 
						   (string->number (XmStringUnparse (.item_or_text i) #f XmCHARSET_TEXT XmCHARSET_TEXT #f 0 XmOUTPUT_ALL))))))
			  ;; this block creates a "radio button box"
			  (let* ((s1 (XmStringCreateLocalized "FFT size"))
				 (frm (let ((frame (XtCreateManagedWidget "frame" xmFrameWidgetClass (XtParent (car sliders))
									  (list XmNborderWidth 1
										XmNshadowType XmSHADOW_ETCHED_IN
										XmNpositionIndex 2))))
					(XtCreateManagedWidget "frm" xmFormWidgetClass frame
							       (list XmNleftAttachment      XmATTACH_FORM
								     XmNrightAttachment     XmATTACH_FORM
								     XmNtopAttachment       XmATTACH_FORM
								     XmNbottomAttachment    XmATTACH_FORM
								     XmNbackground          *basic-color*))))
				 (rc (XtCreateManagedWidget "rc" xmRowColumnWidgetClass frm
							    (list XmNorientation XmHORIZONTAL
								  XmNradioBehavior #t
								  XmNradioAlwaysOne #t
								  XmNentryClass xmToggleButtonWidgetClass
								  XmNisHomogeneous #t
								  XmNleftAttachment      XmATTACH_FORM
								  XmNrightAttachment     XmATTACH_FORM
								  XmNtopAttachment       XmATTACH_FORM
								  XmNbottomAttachment    XmATTACH_NONE
								  XmNbackground          *basic-color*))))
			    (XtCreateManagedWidget "FFT size" xmLabelWidgetClass frm
						   (list XmNleftAttachment      XmATTACH_FORM
							 XmNrightAttachment     XmATTACH_FORM
							 XmNtopAttachment       XmATTACH_WIDGET
							 XmNtopWidget           rc
							 XmNbottomAttachment    XmATTACH_FORM
							 XmNlabelString         s1
							 XmNalignment           XmALIGNMENT_BEGINNING
							 XmNbackground          *basic-color*))
			    (for-each 
			     (lambda (size)
			       (let ((button (XtCreateManagedWidget (format #f "~D" size) xmToggleButtonWidgetClass rc
								    (list XmNbackground           *basic-color*
									  XmNvalueChangedCallback (list (lambda (w c i) 
													  (if (.set i) 
													      (set! cross-synth-fft-size c))) 
													size)
									  XmNset                  (= size cross-synth-fft-size)))))
				 (if (= size cross-synth-fft-size)
				     (set! cross-synth-default-fft-widget button))))
			     '(64 128 256 512 1024 4096))
			    (XmStringFree s1)))
		      
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! cross-synth-target target)
				    (XtSetSensitive (XmMessageBoxGetChild cross-synth-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog cross-synth-dialog))))
	     
	     (child (XtCreateManagedWidget "Cross synthesis" xmPushButtonWidgetClass misc-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-cross-synth-dialog)))
	
	(set! misc-menu-list (cons (lambda ()
				     (change-label child (format #f "Cross synthesis (~D ~1,2F ~D ~1,2F)" 
								 cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius)))
				   misc-menu-list))))
    
;;; -------- Flange and phasing
;;;
    
    (let ((flange-speed 2.0)
	  (flange-amount 5.0)
	  (flange-time 0.001))
      (let* ((post-flange-dialog
	      (let ((flange-label "Flange")
		    (flange-dialog #f)
		    (flange-target 'sound))
		(lambda ()
		  (unless (Widget? flange-dialog)
		    ;; if flange-dialog doesn't exist, create it
		    (let ((initial-flange-speed 2.0)
			  (initial-flange-amount 5.0)
			  (initial-flange-time 0.001)
			  (sliders ()))
		      (set! flange-dialog
			    (make-effect-dialog 
			     flange-label
			     
			     (lambda (w context info)
			       (map-chan-over-target-with-sync 
				(lambda (ignored)
				  (let ((ri (make-rand-interp :frequency flange-speed :amplitude flange-amount))
					(del (let ((len (round (* flange-time (srate)))))
					       (make-delay len :max-size (round (+ len flange-amount 1))))))
				    (lambda (inval)
				      (* .75 (+ inval
						(delay del
						       inval
						       (rand-interp ri)))))))
				flange-target 
				(lambda (target samps) 
				  (format #f "effects-flange ~A ~A ~A" flange-amount flange-speed flange-time))
				#f))
			     
			     (lambda (w context info)
			       (help-dialog "Flange"
					    "Move the sliders to change the flange speed, amount, and time"))
			     
			     (lambda (w c i)
			       (set! flange-speed initial-flange-speed)
			       (XtSetValues (car sliders) (list XmNvalue (floor (* flange-speed 10))))
			       (set! flange-amount initial-flange-amount)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor (* flange-amount 10))))
			       (set! flange-time initial-flange-time)
			       (XtSetValues (caddr sliders) (list XmNvalue (floor (* flange-time 100)))))
			     
			     (lambda () 
			       (effect-target-ok flange-target))))
		      
		      (set! sliders
			    (add-sliders flange-dialog
					 (list (list "flange speed" 0.0 initial-flange-speed 100.0
						     (lambda (w context info)
						       (set! flange-speed (/ (.value info) 10.0)))
						     10)
					       (list "flange amount" 0.0 initial-flange-amount 100.0
						     (lambda (w context info)
						       (set! flange-amount (/ (.value info) 10.0)))
						     10)
					       ;; flange time ought to use a non-linear scale (similar to amp in control panel)
					       (list "flange time" 0.0 initial-flange-time 1.0
						     (lambda (w context info)
						       (set! flange-time (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! flange-target target)
				    (XtSetSensitive (XmMessageBoxGetChild flange-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog flange-dialog))))
	     
	     (child (XtCreateManagedWidget "Flange" xmPushButtonWidgetClass misc-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-flange-dialog)))
	
	(set! misc-menu-list (cons (lambda ()
				     (change-label child (format #f "Flange (~1,2F ~1,2F ~1,3F)" flange-speed flange-amount flange-time)))
				   misc-menu-list))))
    
    
;;; -------- Randomize phase
;;;
;;; (source, progress, target)
    
    (let* ((random-phase-amp-scaler 3.14)
	   (post-random-phase-dialog
	    (let ((random-phase-label "Randomize phase")
		  (random-phase-dialog #f))
	      (lambda ()
		(unless (Widget? random-phase-dialog)
		  ;; if random-phase-dialog doesn't exist, create it
		  (let ((initial-random-phase-amp-scaler 3.14)
			(sliders ()))
		    (set! random-phase-dialog
			  (make-effect-dialog 
			   random-phase-label
			   
			   (lambda (w context info)
			     (rotate-phase (lambda (x) (random random-phase-amp-scaler))))
			   
			   (lambda (w context info)
			     (help-dialog "Randomize phase"
					  "Move the slider to change the randomization amplitude scaler."))
			   
			   (lambda (w c i)
			     (set! random-phase-amp-scaler initial-random-phase-amp-scaler)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* random-phase-amp-scaler 100)))))
			   
			   (lambda ()
			     (pair? (sounds)))))
		    
		    (set! sliders
			  (add-sliders random-phase-dialog
				       (list (list "amplitude scaler" 0.0 initial-random-phase-amp-scaler 100.0
						   (lambda (w context info)
						     (set! random-phase-amp-scaler (/ (.value info) 100.0)))
						   100))))))
		(activate-dialog random-phase-dialog))))
      
	   (child (XtCreateManagedWidget "Randomize phase" xmPushButtonWidgetClass misc-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-random-phase-dialog)))
      
      (set! misc-menu-list (cons (lambda ()
				   (change-label child (format #f "Randomize phase (~1,2F)"  random-phase-amp-scaler)))
				 misc-menu-list)))
    
;;; -------- Robotize
;;;
;;; (progress report?)
    
    (let ((samp-rate 1.0)
	  (osc-amp 0.3)
	  (osc-freq 20))
      (let* ((post-robotize-dialog
	      (let ((robotize-label "Robotize")
		    (robotize-dialog #f)
		    (robotize-target 'sound))
		(lambda ()
		  (unless (Widget? robotize-dialog)
		    ;; if robotize-dialog doesn't exist, create it
		    (let ((initial-samp-rate 1.0)
			  (initial-osc-amp 0.3)
			  (initial-osc-freq 20)
			  (sliders ()))
		      (set! robotize-dialog
			    (make-effect-dialog 
			     robotize-label
			     
			     (lambda (w context info)
			       (let ((ms (and (eq? robotize-target 'marks)
					      (plausible-mark-samples))))
				 (effects-fp samp-rate osc-amp osc-freq
					     (case robotize-target 
					       ((sound)     (values 0 (framples)))
					       ((selection) (values (selection-position) (selection-framples)))
					       (else        (values (car ms) (- (cadr ms) (car ms))))))))
			     (lambda (w context info)
			       (help-dialog "Robotize"
					    "Move the sliders to set the sample rate, oscillator amplitude, and oscillator frequency."))
			     
			     (lambda (w c i)
			       (set! samp-rate initial-samp-rate)
			       (XtSetValues (car sliders) (list XmNvalue (floor (* samp-rate 100))))
			       (set! osc-amp initial-osc-amp)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor (* osc-amp 100))))
			       (set! osc-freq initial-osc-freq)
			       (XtSetValues (caddr sliders) (list XmNvalue (floor (* osc-freq 100)))))
			     
			     (lambda () 
			       (effect-target-ok robotize-target))))
		      
		      (set! sliders
			    (add-sliders robotize-dialog
					 (list (list "sample rate" 0.0 initial-samp-rate 2.0
						     (lambda (w context info)
						       (set! samp-rate (/ (.value info) 100.0)))
						     100)
					       (list "oscillator amplitude" 0.0 initial-osc-amp 1.0
						     (lambda (w context info)
						       (set! osc-amp (/ (.value info) 100.0)))
						     100)
					       (list "oscillator frequency" 0.0 initial-osc-freq 60
						     (lambda (w context info)
						       (set! osc-freq (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! robotize-target target)
				    (XtSetSensitive (XmMessageBoxGetChild robotize-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog robotize-dialog))))
	     
	     (child (XtCreateManagedWidget "Robotize" xmPushButtonWidgetClass misc-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-robotize-dialog)))
	
	(set! misc-menu-list (cons (lambda ()
				     (change-label child (format #f "Robotize (~1,2F ~1,2F ~1,2F)" samp-rate osc-amp osc-freq)))
				   misc-menu-list))))
    
;;; -------- Rubber sound
;;;
    
    (let* ((rubber-factor 1.0)
	   (post-rubber-dialog
	    (let ((rubber-label "Rubber sound")
		  (rubber-dialog #f)
		  (rubber-target 'sound))
	      (lambda ()
		(unless (Widget? rubber-dialog)
		  ;; if rubber-dialog doesn't exist, create it
		  (let ((initial-rubber-factor 1.0)
			(sliders ()))
		    (set! rubber-dialog
			  (make-effect-dialog 
			   rubber-label
			   
			   (lambda (w context info)
			     (rubber-sound rubber-factor))
			   
			   (lambda (w context info)
			     (help-dialog "Rubber sound"
					  "Stretches or contracts the time of a sound. Move the slider to change the stretch factor."))
			   
			   (lambda (w c i)
			     (set! rubber-factor initial-rubber-factor)
			     (XtSetValues (car sliders) (list XmNvalue (floor (* rubber-factor 100)))))
			   
			   (lambda () 
			     (effect-target-ok rubber-target))))
		    
		    (set! sliders
			  (add-sliders rubber-dialog
				       (list (list "stretch factor" 0.0 initial-rubber-factor 5.0
						   (lambda (w context info)
						     (set! rubber-factor (/ (.value info) 100.0)))
						   100))))
		    (add-target (XtParent (car sliders)) 
				(lambda (target) 
				  (set! rubber-target target)
				  (XtSetSensitive (XmMessageBoxGetChild rubber-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))			
				#f)))
		(activate-dialog rubber-dialog))))
      
	   (child (XtCreateManagedWidget "Rubber sound" xmPushButtonWidgetClass misc-menu
					 (list XmNbackground *basic-color*))))
      (XtAddCallback child XmNactivateCallback
		     (lambda (w c i)
		       (post-rubber-dialog)))
      
      (set! misc-menu-list (cons (lambda ()
				   (change-label child (format #f "Rubber sound (~1,2F)"  rubber-factor)))
				 misc-menu-list)))
    
    
;;; -------- Wobble
;;;
;;; (progress report)
    
    (let ((wobble-frequency 50)
	  (wobble-amplitude 0.5))
      (let* ((post-wobble-dialog
	      (let ((wobble-label "Wobble")
		    (wobble-dialog #f)
		    (wobble-target 'sound))
		(lambda ()
		  (unless (Widget? wobble-dialog)
		    ;; if wobble-dialog doesn't exist, create it
		    (let ((initial-wobble-frequency 50)
			  (initial-wobble-amplitude 0.5)
			  (sliders ()))
		      (set! wobble-dialog
			    (make-effect-dialog 
			     wobble-label
			     
			     (lambda (w context info)
			       (let ((ms (and (eq? wobble-target 'marks)
					      (plausible-mark-samples))))
				 (effects-hello-dentist
				  wobble-frequency wobble-amplitude
				  (case wobble-target 
				    ((sound)     (values 0 (framples)))
				    ((selection) (values (selection-position) (selection-framples)))
				    (else        (values (car ms)	(- (cadr ms) (car ms))))))))
			     
			     (lambda (w context info)
			       (help-dialog "Wobble"
					    "Move the sliders to set the wobble frequency and amplitude."))
			     
			     (lambda (w c i)
			       (set! wobble-frequency initial-wobble-frequency)
			       (XtSetValues (car sliders) (list XmNvalue (floor (* wobble-frequency 100))))
			       (set! wobble-amplitude initial-wobble-amplitude)
			       (XtSetValues (cadr sliders) (list XmNvalue (floor (* wobble-amplitude 100)))))
			     
			     (lambda () 
			       (effect-target-ok wobble-target))))
		      
		      (set! sliders
			    (add-sliders wobble-dialog
					 (list (list "wobble frequency" 0 initial-wobble-frequency 100
						     (lambda (w context info)
						       (set! wobble-frequency (/ (.value info) 100.0)))
						     100)
					       (list "wobble amplitude" 0.0 initial-wobble-amplitude 1.0
						     (lambda (w context info)
						       (set! wobble-amplitude (/ (.value info) 100.0)))
						     100))))
		      (add-target (XtParent (car sliders)) 
				  (lambda (target) 
				    (set! wobble-target target)
				    (XtSetSensitive (XmMessageBoxGetChild wobble-dialog XmDIALOG_OK_BUTTON) (effect-target-ok target)))
				  #f)))
		  
		  (activate-dialog wobble-dialog))))
	     
	     (child (XtCreateManagedWidget "Wobble" xmPushButtonWidgetClass misc-menu
					   (list XmNbackground *basic-color*))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (post-wobble-dialog)))
	
	(set! misc-menu-list (cons (lambda ()
				     (change-label child (format #f "Wobble (~1,2F ~1,2F)" wobble-frequency wobble-amplitude)))
				   misc-menu-list)))))
  
;;;
;;; END PARAMETRIZED EFFECTS
;;;
  
  (add-to-menu effects-menu #f #f)
  (add-to-menu effects-menu "Octave-down" (lambda () (down-oct 2)))
  (add-to-menu effects-menu "Remove clicks"
	       (lambda ()
		 (let remove-click ((loc 0))
		   (let ((click (let ((reader (make-sampler loc))
				      (mmax (make-moving-max 10))
				      (samp0 0.0)
				      (samp1 0.0)
				      (samp2 0.0)
				      (len (framples))
				      (local-max 0.0))
				  (call-with-exit
				   (lambda (return)
				     (do ((ctr loc (+ ctr 1)))
					 ((= ctr len) #f)
				       (set! samp0 samp1)
				       (set! samp1 samp2)
				       (set! samp2 (next-sample reader))
				       (set! local-max (max .1 (moving-max mmax samp0)))
				       (if (and (> (abs (- samp0 samp1)) local-max)
						(> (abs (- samp1 samp2)) local-max)
						(< (abs (- samp0 samp2)) (/ local-max 2)))
					   (return (- ctr 1)))))))))
		     (when (integer? click)
		       (smooth-sound (- click 2) 4)
		       (remove-click (+ click 2)))))))
  
  (add-to-menu effects-menu "Remove DC" 
	       (lambda* (snd chn)
		 (let* ((len (framples snd chn))
			(data (make-float-vector len))
			(reader (make-sampler 0 snd chn)))
		   (do ((lastx 0.0)
			(lasty 0.0)
			(i 0 (+ i 1)))
		       ((= i len))
		     (let ((inval (next-sample reader)))
		       (set! lasty (- (+ inval (* 0.999 lasty)) lastx))
		       (set! lastx inval)
		       (float-vector-set! data i lasty)))
		   (float-vector->channel data 0 len snd chn current-edit-position "effects-remove-dc"))))
  
  (add-to-menu effects-menu "Spiker" spike)
  
  (add-to-menu effects-menu "Compand" 
	       (lambda* (snd chn)
		 (let ((tbl (float-vector -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250
					  0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
		   (let ((len (framples snd chn)))
		     (let ((reader (make-sampler 0 snd chn))
			   (data (make-float-vector len)))
		       (do ((i 0 (+ i 1)))
			   ((= i len))
			 (float-vector-set! data i (array-interp tbl (+ 8.0 (* 8.0 (next-sample reader))) 17)))
		       (float-vector->channel data 0 len snd chn current-edit-position "effects-compand"))))))
  
  (add-to-menu effects-menu "Invert" (lambda () (scale-by -1)))
  (add-to-menu effects-menu "Reverse" reverse-sound)
  (add-to-menu effects-menu "Null phase" zero-phase)
  
  
  )
