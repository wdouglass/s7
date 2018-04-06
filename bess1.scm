;;; bess1.scm -- some examples from clm-2/rt.lisp and clm-2/bess5.cl

;; Author: Michael Scholz <scholz-micha@gmx.de>
;; Created: Thu May 29 04:14:35 CEST 2003
;; Last: Sun Jun 15 03:50:21 CEST 2003
;; changed slightly 14-Jun-06 Bill to match bess.scm, fix pitch problem in make-oscil.
;;   then again 18-Dec-09 to use s7 rather than Guile
;; changed float-vector-map! to use a loop instead (Bill 4-July-12)

(if (not (provided? 'snd-motif)) (error 'gui-error "bess1.scm needs motif"))

;;; Commentary:

;; This file provides simple mono real time output to DAC.  Tempo,
;; frequency, amplitude, and FM index can be controlled via sliders.
;; The music algorithms are taken from clm-2/rt.lisp and
;; clm-2/bess5.cl.

;; (main) calls (rt-motif) which starts a Motif widget with two DAC
;; tests.
;;
;; (rt-motif :srate       *clm-srate*        ;; 22050
;;           :sample-type *clm-sample-type*) ;; mus-lshort

;;; Code:

;(set! *clm-srate* 22050)

(define *clm-sample-type* mus-lfloat)

(define ctempo 1.0)
(define camp 1.0)
(define cfreq 1.0)
(define cindex 1.0)
(define cplay #t)
(define silence (lambda () 0.0))
  
(define sliderback "lightsteelblue")
(define background "lightsteelblue1")

(define make-rt-violin 
  (let ((+documentation+ "(make-rt-violin dur freq amp (fm-index 1.0) (amp-env '(0 0 25 1 75 1 100 0))) real time simple violin (see fm.html)"))
    (lambda* (dur freq amp (fm-index 1.0) (amp-env '(0 0 25 1 75 1 100 0)))
      (let* ((frq-scl (hz->radians freq))
	     (maxdev (* frq-scl fm-index)))
	(let ((carrier (make-oscil :frequency freq))
	      (fmosc1 (make-oscil :frequency freq))
	      (fmosc2 (make-oscil :frequency (* 3 freq)))
	      (fmosc3 (make-oscil :frequency (* 4 freq)))
	      (ampf (make-env :envelope amp-env :scaler amp :duration dur))
	      (indf1 (make-env :envelope '(0 1 25 0.4 75 0.6000000000000001 100 0) 
			       :scaler (* maxdev (/ 5.0 (log freq)))
			       :duration dur))
	      (indf2 (make-env :envelope '(0 1 25 0.4 75 0.6000000000000001 100 0) 
			       :scaler (/ (* maxdev 3.0 (- 8.5 (log freq))) (+ 3.0 (/ freq 1000)))
			       :duration dur))
	      (indf3 (make-env :envelope '(0 1 25 0.4 75 0.6000000000000001 100 0) 
			       :scaler (* maxdev (/ 4.0 (sqrt freq)))
			       :duration dur))
	      (pervib (make-triangle-wave :frequency 5 :amplitude (* 0.0025 frq-scl)))
	      (ranvib (make-rand-interp :frequency 16 :amplitude (* 0.005 frq-scl))))
	  (lambda ()
	    (let ((vib (+ (triangle-wave pervib) (rand-interp ranvib))))
	      (* (env ampf)
		 (oscil carrier
			(+ vib 
			   (* (env indf1) (oscil fmosc1 vib))
			   (* (env indf2) (oscil fmosc2 (* 3.0 vib)))
			   (* (env indf3) (oscil fmosc3 (* 4.0 vib)))))))))))))

(define lim 256)

;; from clm-2/rt.lisp
(define* (make-test (srate *clm-srate*))
  (let ((vpits (make-vector (+ 1 lim) 0))
	(vbegs (make-vector (+ 1 lim) 0)))
    (do ((i 0 (+ 1 i)))
	((= i lim))
      (set! (vpits i) (random 12))
      (set! (vbegs i) (+ 1 (random 3))))
    (set! *clm-srate* srate)

    (let ((cellbeg 0)
	  (cellsiz 6)
	  (cellctr 0)
	  (func #f)
	  (len 0)
	  (dur 0.0)
	  (vmode (vector 0 12 2 4 14 4 5 5 0 7 7 11 11)))
      (lambda ()
	(if (> len 0)
	    (set! len (- len 1))
	    (begin
	      (set! dur (/ (vbegs (+ cellctr 1)) ctempo))
	      (set! cellctr (+ cellctr 1))
	      (if (> cellctr (+ cellsiz cellbeg))
		  (begin
		    (if (> (random 1.0) 0.5) (set! cellbeg (+ 1 cellbeg)))
		    (if (> (random 1.0) 0.5) (set! cellsiz (+ 1 cellsiz)))
		    (set! cellctr cellbeg)))
	      
	      (if cplay
		  (let ((freq (* cfreq 16.351 16
				 (expt 2 (/ (vmode (vpits cellctr)) 12.0)))))
		    ;(format () "dur: ~A, freq: ~,3F, amp: ~A, index: ~A~%" dur freq camp cindex)
		    (set! func (make-rt-violin dur freq camp :fm-index cindex)))
		  (set! func silence))
	      (set! len (seconds->samples dur))))
	(func)))))


;; from clm-2/bess5.cl and clm-2/clm-example.lisp
(define time 60)
(define mode (vector 0 0 2 4 11 11 5 6 7 9 2 0 0))
(define rats (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0))

(define bell '(0 0 10 0.25 90 1.0 100 1.0))

(define pits (make-vector (+ 1 lim) 0))
(define octs (make-vector (+ 1 lim) 0))
(define rhys (make-vector (+ 1 lim) 0))
(define begs (make-vector (+ 1 lim) 0))
(define amps (make-vector (+ 1 lim) 0))

(define (tune x)
  (* (rats (modulo x 12))
     (expt 2 (floor (/ x 12)))))

(define (rbell x)
  (envelope-interp (* x 100) bell))

(define* (make-agn (srate *clm-srate*))
  (do ((i 0 (+ i 1)))
      ((= i lim))
    (set! (octs i) (floor (+ 4 (* 2 (rbell (random 1.0))))))
    (set! (pits i) (mode (random 12)))
    (set! (rhys i) (+ 4 (random 6)))
    (set! (begs i) (if (< (random 1.0) 0.9) 
		       (+ 4 (random 2))
		       (random 24)))
    (set! (amps i) (floor (+ 1 (* 8 (rbell (random 1.0)))))))
  (set! *clm-srate* srate)

  (let ((wins (vector '(0 0 40 0.1 60 0.2 75 0.4 82 1 100 0)
		      '(0 0 60 0.1 80 0.2 90 0.4 95 1 100 0)
		      '(0 0 10 1 16 0 32 0.1 50 1 56 0 60 0 80 0.3 100 0)
		      '(0 0 30 1 56 0 60 0 80 0.3 100 0)
		      '(0 0 50 1 80 0.3 100 0)
		      '(0 0 40 0.1 60 0.2 75 0.4 82 1 100 0)
		      '(0 0 40 0.1 60 0.2 75 0.4 82 1 100 0)
		      '(0 0 10 1 32 0.1 50 1 80 0.3 100 0)
		      '(0 0 60 0.1 80 0.3 100 0)
		      '(0 0 80 0.1 100 0)))
	(nextbeg 0.0)
	(beg 0.0)
	(dur 0.0)
	(freq 0.0)
	(ampl 0.0)
	(ind 0.0)
	(cellctr 0)
	(cellsiz 4)
	(cellbeg 0)
	(whichway 1)
	(func #f)
	(len 0))
    (lambda ()
      (if (> len 0)
	  (set! len (- len 1))
	  (begin
	    (set! beg (+ beg nextbeg))
	    (set! nextbeg (+ nextbeg (max 0.025 (/ (* (+ 0.95 (random 0.1)) (begs cellctr)) ctempo))))
	    (set! dur (max 0.025 (/ (* (+ 0.85 (random 0.1)) (rhys cellctr)) ctempo)))
	    (set! freq (* cfreq 16.351 (tune (pits cellctr)) (expt 2 (octs cellctr))))
	    (set! ampl (* camp 10 (max 0.003 (* (amps cellctr) 0.01))))
	    (set! ind (* cindex (random 3.0)))
	    (set! cellctr (+ cellctr 1))
	    (if (> cellctr (+ cellsiz cellbeg))
		(begin
		  (set! cellbeg (+ 1 cellbeg))
		  (if (> (random 1.0) 0.5) (set! cellsiz (+ cellsiz whichway)))
		  (cond ((and (> cellsiz 10)
			      (> (random 1.0) 0.99))
			 (set! whichway -2))
			((and (> cellsiz 6)
			      (> (random 1.0) 0.999))
			 (set! whichway -1))
			((< cellsiz 4) 
			 (set! whichway 1)))
		  (set! nextbeg (+ nextbeg (random 1.0)))
		  (set! cellctr cellbeg)))
	    (set! func (if cplay
			   (make-rt-violin dur freq ampl
					   :fm-index ind
					   :amp-env (wins (random 10)))
			   silence))
	    (set! len (seconds->samples dur))))
      (func))))

(when (provided? 'snd-motif)
  (with-let (sublet *motif*)
    
    ;; set up our user-interface
    (let* ((shell (let* ((xdismiss (XmStringCreate "Go away" XmFONTLIST_DEFAULT_TAG))
			 (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
			 (titlestr (XmStringCreate "FM Forever!" XmFONTLIST_DEFAULT_TAG))
			 (dialog (XmCreateTemplateDialog (cadr (main-widgets)) "FM Forever!"
							 (list XmNcancelLabelString   xdismiss
							       XmNhelpLabelString     xhelp
							       XmNautoUnmanage        #f
							       XmNdialogTitle         titlestr
							       XmNwidth               600
							       XmNresizePolicy        XmRESIZE_GROW
							       XmNnoResize            #f
							       XmNtransient           #f))))
		    (XtAddCallback dialog 
				   XmNhelpCallback (lambda (w context info)
						     (snd-print "This dialog lets you experiment with simple FM")))
		    (XmStringFree xhelp)
		    (XmStringFree xdismiss)
		    (XmStringFree titlestr)
		    dialog))
	   
	   (screen (DefaultScreenOfDisplay (XtDisplay shell)))
	   (black (BlackPixelOfScreen screen))
	   (white (WhitePixelOfScreen screen))
	   
	   (form (XtCreateManagedWidget "form" xmFormWidgetClass shell 
					(list XmNbackground white
					      XmNforeground black
					      XmNresizePolicy XmRESIZE_GROW)))
	   ;; toggle named "play"
	   (play-button (XtCreateManagedWidget "play" xmToggleButtonWidgetClass form
					       (list XmNleftAttachment   XmATTACH_FORM
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_FORM
						     XmNrightAttachment  XmATTACH_NONE
                                                     XmNset              #t
						     XmNbackground       white)))
	   ;; carrier freq
	   (carrier (XtCreateManagedWidget "carrier freq:" xmLabelWidgetClass form
					   (list XmNleftAttachment   XmATTACH_FORM
						 XmNbottomAttachment XmATTACH_NONE
						 XmNtopAttachment    XmATTACH_WIDGET
						 XmNtopWidget        play-button
						 XmNrightAttachment  XmATTACH_NONE
						 XmNrecomputeSize    #f
						 XmNbackground       white)))
	   (freq-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					      (list XmNleftAttachment   XmATTACH_WIDGET
						    XmNleftWidget       carrier
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						    XmNtopWidget        carrier
						    XmNrightAttachment  XmATTACH_NONE
						    XmNbackground       white)))
	   (freq-scale (XtCreateManagedWidget "carrier freq" xmScaleWidgetClass form
					      (list XmNleftAttachment   XmATTACH_WIDGET
						    XmNleftWidget       freq-label
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						    XmNtopWidget        freq-label
						    XmNrightAttachment  XmATTACH_FORM
						    XmNshowValue        #f
						    XmNorientation      XmHORIZONTAL
						    XmNbackground       *position-color*)))
	   ;; amp
	   (amp-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					     (let ((amp (XtCreateManagedWidget "amp:" xmLabelWidgetClass form
									       (list XmNleftAttachment   XmATTACH_FORM
										     XmNbottomAttachment XmATTACH_NONE
										     XmNtopAttachment    XmATTACH_WIDGET
										     XmNtopWidget        carrier
										     XmNrightAttachment  XmATTACH_NONE
										     XmNrecomputeSize    #f
										     XmNbackground       white))))
					       (list XmNleftAttachment   XmATTACH_WIDGET
						     XmNleftWidget       amp
						     XmNbottomAttachment XmATTACH_NONE
						     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						     XmNtopWidget        amp
						     XmNrightAttachment  XmATTACH_NONE
						     XmNbackground       white))))
	   (amp-scale (XtCreateManagedWidget "amp" xmScaleWidgetClass form
					     (list XmNleftAttachment   XmATTACH_WIDGET
						   XmNleftWidget       amp-label
						   XmNbottomAttachment XmATTACH_NONE
						   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						   XmNtopWidget        amp-label
						   XmNrightAttachment  XmATTACH_FORM
						   XmNshowValue        #f
						   XmNorientation      XmHORIZONTAL
						   XmNbackground       *position-color*)))
	   ;; fm index
	   (fm-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					    (let ((fm-index (XtCreateManagedWidget "fm index:" xmLabelWidgetClass form
										   (list XmNleftAttachment   XmATTACH_FORM
											 XmNbottomAttachment XmATTACH_NONE
											 XmNtopAttachment    XmATTACH_WIDGET
											 XmNtopWidget        amp-scale
											 XmNrightAttachment  XmATTACH_NONE
											 XmNrecomputeSize    #f
											 XmNbackground       white))))
					      (list XmNleftAttachment   XmATTACH_WIDGET
						    XmNleftWidget       fm-index
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						    XmNtopWidget        fm-index
						    XmNrightAttachment  XmATTACH_NONE
						    XmNbackground       white))))
	   (fm-scale (XtCreateManagedWidget "fm index" xmScaleWidgetClass form
					    (list XmNleftAttachment   XmATTACH_WIDGET
						  XmNleftWidget       fm-label
						  XmNbottomAttachment XmATTACH_NONE
						  XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						  XmNtopWidget        fm-label
						  XmNrightAttachment  XmATTACH_FORM
						  XmNshowValue        #f
						  XmNorientation      XmHORIZONTAL
						  XmNbackground       *position-color*)))
	   ;; tempo
	   (tempo-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					    (let ((cm-ratio (XtCreateManagedWidget "tempo:" xmLabelWidgetClass form
										   (list XmNleftAttachment   XmATTACH_FORM
											 XmNbottomAttachment XmATTACH_NONE
											 XmNtopAttachment    XmATTACH_WIDGET
											 XmNtopWidget        fm-scale
											 XmNrightAttachment  XmATTACH_NONE
											 XmNrecomputeSize    #f
											 XmNbackground       white))))
					      (list XmNleftAttachment   XmATTACH_WIDGET
						    XmNleftWidget       cm-ratio
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						    XmNtopWidget        cm-ratio
						    XmNrightAttachment  XmATTACH_NONE
						    XmNbackground       white))))
	   (tempo-scale (XtCreateManagedWidget "tempo" xmScaleWidgetClass form
					    (list XmNleftAttachment   XmATTACH_WIDGET
						  XmNleftWidget       tempo-label
						  XmNbottomAttachment XmATTACH_NONE
						  XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						  XmNtopWidget        tempo-label
						  XmNrightAttachment  XmATTACH_FORM
						  XmNshowValue        #f
						  XmNorientation      XmHORIZONTAL
						  XmNbackground       *position-color*)))
	   (set-flabel 
	    (lambda (label value)
	      (let ((s1 (XmStringCreate (format #f "~,3F" value) XmFONTLIST_DEFAULT_TAG)))
		(XtVaSetValues label (list XmNlabelString s1))
		(XmStringFree s1)))))
	   
      (define (freq-callback w c i)
	(set! cfreq (* .05 (.value i)))
	(set-flabel freq-label cfreq))
      
      (define (amp-callback w c i)
	(set! camp (* 0.01 (.value i)))
	(set-flabel amp-label camp))
      
      (define (fm-callback w c i)
	(set! cindex (* .05 (.value i)))
	(set-flabel fm-label cindex))
      
      (define (tempo-callback w c i)
	(set! ctempo (* .1 (.value i)))
	(set-flabel tempo-label ctempo))
      
      
      ;; --------------------------------
      ;; go-away button
      (XtAddCallback shell XmNcancelCallback (lambda (w c i) (stop-playing) (XtUnmanageChild w)))
      
      ;; add scale-change (drag and value-changed) callbacks
      (XtAddCallback freq-scale XmNdragCallback freq-callback)
      (XtAddCallback freq-scale XmNvalueChangedCallback freq-callback)
      
      (XtAddCallback amp-scale XmNdragCallback amp-callback)
      (XtAddCallback amp-scale XmNvalueChangedCallback amp-callback)
      
      (XtAddCallback fm-scale XmNdragCallback fm-callback)
      (XtAddCallback fm-scale XmNvalueChangedCallback fm-callback)
      
      (XtAddCallback tempo-scale XmNdragCallback tempo-callback)
      (XtAddCallback tempo-scale XmNvalueChangedCallback tempo-callback)
      
      (XtAddCallback play-button XmNvalueChangedCallback (lambda (w c i) (set! cplay (.set i))))
      (XmAddWMProtocolCallback (XtParent shell) (XmInternAtom (XtDisplay (cadr (main-widgets))) "WM_DELETE_WINDOW" #f) (lambda (w c i) (stop-playing)) #f)
      
      ;; set initial values
      (set-flabel freq-label 1.0)
      (set-flabel amp-label 0.1)
      (set-flabel fm-label 1.0)
      (set-flabel tempo-label 1.0)
      
      (XmScaleSetValue freq-scale 20)
      (set! cfreq 1.0)
      (XmScaleSetValue amp-scale 10)
      (set! camp 0.1)
      (XmScaleSetValue fm-scale 20)
      (set! cindex 1.0)
      (XmScaleSetValue tempo-scale 10)
      (set! ctempo 1.0)
      
      (XtManageChild shell)
      (XtRealizeWidget shell)
      
      (play (make-test)))))

