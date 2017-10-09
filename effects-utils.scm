(provide 'snd-effects-utils.scm)
(require snd-motif)

(with-let *motif*
  
  (define raise-dialog 
    (let ((+documentation+ "(raise-dialog w) tries to put 'w' on top of any widgets that are obscuring it"))
      (lambda (w)
	(if (and (Widget? w) 
		 (XtIsManaged w))
	    (let ((parent (XtParent w)))
	      (if (and (Widget? parent)
		       (XtIsSubclass parent xmDialogShellWidgetClass))
		  (XtPopup parent XtGrabNone)))))))
  
  (define activate-dialog 
    (let ((+documentation+ "(activate-dialog dialog) makes 'dialog' active and brings it to the top of the currently displayed widgets"))
      (lambda (dialog)
	((if (not (XtIsManaged dialog)) XtManageChild raise-dialog) dialog))))
  
  (define for-each-child 
    (let ((+documentation+ "(for-each-child w func) applies 'func' to 'w' and to its descendents"))
      (lambda (w func)
	(func w)
	(if (XtIsComposite w)
	    (for-each 
	     (lambda (n)
	       (for-each-child n func))
	     (cadr (XtGetValues w (list XmNchildren 0) 1)))))))
  
  (define use-combo-box-for-fft-size #f) ; cross-synthesis fft size: radio-buttons or combo-box choice
  
  (define current-screen
    (let ((+documentation+ "(current-screen) returns the current X screen number of the current display"))
      (lambda ()
	(DefaultScreenOfDisplay 
	  (XtDisplay (cadr (main-widgets)))))))
  
  (define all-chans ; for later use in new-effects.scm?
    (let ((+documentation+ "(all-chans) returns a list of all current sound objects and channel numbers"))
      (lambda ()
	(let ((sndlist ())
	      (chnlist ()))
	  (for-each (lambda (snd)
		      (do ((i (- (channels snd) 1) (- i 1)))
			  ((< i 0))
			(set! sndlist (cons snd sndlist))
			(set! chnlist (cons i chnlist))))
		    (sounds))
	  (list sndlist chnlist)))))
  
  (define update-label 
    (let ((+documentation+ "(update-label effects) evaluates the elements of the list 'effects'"))
      (lambda (effects)
	(for-each (lambda (effect) (effect)) effects))))
  
  (define effect-target-ok 
    (let ((+documentation+ "(effect-target-ok target) returns #t if the current effect's chosen target is ready"))
      (lambda (target)
	(case target 
	  ((sound) (pair? (sounds)))
	  ((selection)    (selection?))
	  (else           (and (selected-sound)
			       (>= (length (marks (selected-sound) (selected-channel))) 2)))))))
  
  (define make-effect-dialog 
    (let ((+documentation+ "(make-effect-dialog label ok-callback help-callback reset-callback target-ok-callback) makes a standard effects dialog"))
      (lambda* (label ok-callback help-callback reset-callback target-ok-callback)
	;; make a standard dialog
	(let* ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
	       (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	       (xok (XmStringCreate "DoIt" XmFONTLIST_DEFAULT_TAG))
	       (titlestr (XmStringCreate label XmFONTLIST_DEFAULT_TAG))
	       (new-dialog (XmCreateTemplateDialog
			    (cadr (main-widgets)) label
			    (list XmNcancelLabelString   xdismiss
				  XmNhelpLabelString     xhelp
				  XmNokLabelString       xok
				  XmNautoUnmanage        #f
				  XmNdialogTitle         titlestr
				  XmNresizePolicy        XmRESIZE_GROW
				  XmNnoResize            #f
				  XmNbackground          *basic-color*
				  XmNtransient           #f))))
	  (for-each
	   (lambda (button color)
	     (XtVaSetValues
	      (XmMessageBoxGetChild new-dialog button)
	      (list XmNarmColor   *selection-color*
		    XmNbackground color)))
	   (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
	   (list *highlight-color* *highlight-color* *highlight-color*))
	  
	  (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i) (XtUnmanageChild new-dialog)))
	  (XtAddCallback new-dialog XmNhelpCallback help-callback)  ; "Help"
	  (XtAddCallback new-dialog XmNokCallback ok-callback)      ; "DoIt"
	  
	  (if reset-callback
	      ;; add a Reset button
	      (let ((reset-button (XtCreateManagedWidget "Reset" xmPushButtonWidgetClass new-dialog
							 (list XmNbackground *highlight-color*
							       XmNforeground (BlackPixelOfScreen (current-screen))
							       XmNarmColor   *selection-color*))))
		(XtAddCallback reset-button XmNactivateCallback reset-callback)))
	  (for-each XmStringFree (vector xhelp xok xdismiss titlestr))
	  
	  (if target-ok-callback
	      (begin
		(XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (target-ok-callback))
		(hook-push effects-hook
			   (lambda (hook) 
			     (XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (target-ok-callback)))))
	      (begin
		(XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (pair? (sounds)))
		(hook-push effects-hook
			   (lambda (hook) 
			     (XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (pair? (sounds)))))))
	  
	  new-dialog))))
  
  
;;; replacement for change-menu-label
  (define change-label 
    (let ((+documentation+ "(change-label widget new-label) changes the label of 'widget' to be 'new-label'"))
      (lambda (widget new-label)
	(let ((str (XmStringCreateLocalized new-label)))
	  (XtSetValues widget (list XmNlabelString str))
	  (XmStringFree str)))))
  
  
;;; -------- log scaler widget
  
  (define log-scale-ticks 500) ; sets precision (to some extent) of slider 
  
  (define scale-log->linear 
    (let ((+documentation+ "(scale-log->linear lo val hi) given user-relative low..val..hi returns val as scale-relative (0..log-scale-ticks)"))
      (lambda (lo val hi)
	(let ((log-lo (log (max lo 1.0) 2))
	      (log-hi (log hi 2))
	      (log-val (log val 2)))
	  (floor (* log-scale-ticks (/ (- log-val log-lo) (- log-hi log-lo))))))))
  
  (define scale-linear->log 
    (let ((+documentation+ "(scale-linear->log lo val hi) given user-relative lo..hi and scale-relative val, returns the user-relative val"))
      (lambda (lo val hi)
	;; since log-scale widget assumes 0..log-scale-ticks, val can be used as ratio (log-wise) between lo and hi
	(let ((log-lo (log (max lo 1.0) 2))
	      (log-hi (log hi 2)))
	  (expt 2.0 (+ log-lo (* (/ val log-scale-ticks) (- log-hi log-lo))))))))
  
  (define scale-log-label 
    (let ((+documentation+ "(scale-log-label lo val hi) makes a log scale label"))
      (lambda (lo val hi)
	(format #f "~,2F" (scale-linear->log lo val hi)))))
  
  (define create-log-scale-widget 
    (let ((+documentation+ "(create-log-scale-widget parent title low initial high) returns a log scale widget"))
      (lambda (parent title low initial high)
	(let ((label (XtCreateManagedWidget (format #f "~,2F" initial) xmLabelWidgetClass parent
					    (list XmNbackground          *basic-color*)))
	      (scale (XtCreateManagedWidget "scale" xmScaleWidgetClass parent
					    (list XmNorientation   XmHORIZONTAL
						  XmNshowValue     #f
						  XmNminimum       0
						  XmNmaximum       log-scale-ticks
						  XmNvalue         (floor (scale-log->linear low initial high))
						  XmNdecimalPoints 0
						  XmNtitleString   title
						  XmNbackground    *basic-color*))))
	  (XtAddCallback scale XmNvalueChangedCallback
			 (lambda (widget context info)
			   (change-label label (scale-log-label low (.value info) high))))
	  (XtAddCallback scale XmNdragCallback
			 (lambda (widget context info)
			   (change-label label (scale-log-label low (.value info) high))))
	  scale))))
  
  
;;; -------- semitone scaler widget
;;; 
;;; set up like log scale (use 'semi in place of 'log),
;;;   to get the ratio from the semitones, use (expt 2.0 (/ value 12.0)) -- semitones->ratio below					 
  
  (define semi-range 24) ; 2 octaves either way
  
  (define semi-scale-label 
    (let ((+documentation+ "(semi-scale-label val) makes a semitone label"))
      (lambda (val)
	(format #f "semitones: ~D" (- val semi-range)))))
  
  (define semitones->ratio 
    (let ((+documentation+ "(semitones->ratio val) takes a semitone number 'val' and returns the corresponding float ratio"))
      (lambda (val)
	(expt 2.0 (/ val 12.0)))))
  
  (define ratio->semitones 
    (let ((+documentation+ "(ratio->semitones ratio) takes a float ratio and returns the corresponding number of semitones"))
      (lambda (ratio)
	(round (* 12 (log ratio 2))))))
  
  (define create-semi-scale-widget 
    (let ((+documentation+ "(create-semi-scale-widget parent title initial) returns a semitone scale widget"))
      (lambda (parent title initial)
	(let ((label (XtCreateManagedWidget (format #f "semitones: ~D" (ratio->semitones initial)) xmLabelWidgetClass parent
					    (list XmNbackground          *basic-color*)))
	      (scale (XtCreateManagedWidget "scale" xmScaleWidgetClass parent
					    (list XmNorientation   XmHORIZONTAL
						  XmNshowValue     #f
						  XmNminimum       0
						  XmNmaximum       (* 2 semi-range)
						  XmNvalue         (+ semi-range (ratio->semitones initial))
						  XmNdecimalPoints 0
						  XmNtitleString   title
						  XmNbackground    *basic-color*))))
	  (XtAddCallback scale XmNvalueChangedCallback
			 (lambda (widget context info)
			   (change-label label (semi-scale-label (.value info)))))
	  (XtAddCallback scale XmNdragCallback
			 (lambda (widget context info)
			   (change-label label (semi-scale-label (.value info)))))
	  scale))))
  
  (define add-sliders 
    (let ((+documentation+ "(add-sliders dialog sliders) takes 'sliders', a list of lists, each inner list being (title low initial high callback scale ['log]) \
and returns a list of widgets (for reset callbacks)"))
      (lambda* (dialog sliders)
	(let ((mainform (let ((mainfrm (XtCreateManagedWidget "formd" xmFormWidgetClass dialog
							      (list XmNleftAttachment      XmATTACH_FORM
								    XmNrightAttachment     XmATTACH_FORM
								    XmNtopAttachment       XmATTACH_FORM
								    XmNbottomAttachment    XmATTACH_WIDGET
								    XmNbottomWidget        (XmMessageBoxGetChild dialog XmDIALOG_SEPARATOR)
								    XmNbackground          *highlight-color*))))
			  (XtCreateManagedWidget "formd" xmRowColumnWidgetClass mainfrm
						 (list XmNleftAttachment      XmATTACH_FORM
						       XmNrightAttachment     XmATTACH_FORM
						       XmNbackground          *highlight-color*
						       XmNorientation         XmVERTICAL)))))
	  (map
	   (lambda (slider-data)
	     (let* ((title (XmStringCreate (slider-data 0) XmFONTLIST_DEFAULT_TAG))
		    (low (slider-data 1))
		    (initial (slider-data 2))
		    (high (slider-data 3))
		    (func (slider-data 4))
		    (new-slider (if (= (length slider-data) 7)
				    (if (eq? (slider-data 6) 'log)
					(create-log-scale-widget mainform title low initial high)
					(create-semi-scale-widget mainform title initial))
				    (let ((scale (slider-data 5)))
				      (XtCreateManagedWidget (car slider-data) xmScaleWidgetClass mainform
							     (list XmNorientation   XmHORIZONTAL
								   XmNshowValue     #t
								   XmNminimum       (floor (* low scale))
								   XmNmaximum       (floor (* high scale))
								   XmNvalue         (floor (* initial scale))
								   XmNdecimalPoints (case scale ((10000) 4) ((1000) 3) ((100) 2) ((10) 1) (else 0))
								   XmNtitleString   title
								   XmNleftAttachment XmATTACH_FORM
								   XmNrightAttachment XmATTACH_FORM
								   XmNbackground    *basic-color*))))))
	       (XmStringFree title)
	       (XtAddCallback new-slider XmNvalueChangedCallback func)
	       new-slider))
	   sliders)))))
  
  )
