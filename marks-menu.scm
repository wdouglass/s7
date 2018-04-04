(if (and (provided? 'snd-gtk)
	 (not (provided? 'gtk4)))
    (error 'gtk-error "marks-menu.scm only works in gtk4"))

(provide 'snd-marks-menu.scm)

(if (provided? 'xm)
    (begin
      (require snd-effects-utils.scm)
      (if (not (defined? 'mark-sync-color)) 
	  (load "snd-motif.scm"))))

(if (provided? 'gtk4)
    (begin
      (require snd-gtk-effects-utils.scm)
      (if (not (defined? 'mark-sync-color)) 
	  (define (mark-sync-color x) x))))

(when (provided? 'snd-motif)
  (define mark-sync-color (*motif* 'mark-sync-color)))

(if (not (defined? 'mark-loops)) (load "examp.scm"))
(if (not (defined? 'play-between-marks)) (load "marks.scm"))
(if (not (defined? 'loop-between-marks)) (load "play.scm"))

(define *e* (if (provided? 'snd-motif) *motif* *gtk*))
(define update-label (*e* 'update-label))
(define change-label (*e* 'change-label))
(define make-effect-dialog (*e* 'make-effect-dialog))
(define add-sliders (*e* 'add-sliders))
(define activate-dialog (*e* 'activate-dialog))
(define select-file (*e* 'select-file))

(define marks-list ()) ; menu labels are updated to show current default settings

(define marks-menu (add-to-main-menu "Marks" (lambda ()
					       (update-label marks-list))))
(define find-two-marks
  (let ((+documentation+ "(find-two-marks) looks for the marks for the marks-menu functions to use"))
    (lambda ()
      (let ((ms (marks (selected-sound) (selected-channel))))
	(if (> (length ms) 1)
	    (map mark->integer (list (car ms) (cadr ms)))
	    ())))))


;;; -------- Play between by marks

(define play-between-marks-m1 0)
(define play-between-marks-m2 1)
(define play-between-marks-label "Play between marks")
(define play-between-marks-dialog #f)
(define play-between-marks-menu-label #f)

(define cp-play-between-marks
  (let ((+documentation+ "(cp-play-between-marks) plays between 2 marks (marks-menu)"))
    (lambda ()
      (play-between-marks (integer->mark play-between-marks-m1) (integer->mark play-between-marks-m2)))))

(if (not (or (provided? 'xm) 
	     (provided? 'xg)))
    (set! play-between-marks-menu-label (add-to-menu marks-menu play-between-marks-label cp-play-between-marks))
    (begin
      
      (define (set-syncs)
	(for-each 
	 (lambda (snd-marks)
	   (for-each 
	    (lambda (chan-marks)
	      (for-each 
	       (lambda (m)
		 (set! (sync m) (if (or (= (mark->integer m) play-between-marks-m1)
					(= (mark->integer m) play-between-marks-m2))
				    1 0)))
	       chan-marks))
	    snd-marks))
	 (marks))
	(update-time-graph))
      
      (define (max-mark) ; "id" here
	(apply max (map mark->integer (marks (selected-sound) (selected-channel)))))
      
      (define (min-mark)
	(apply min (map mark->integer (marks (selected-sound) (selected-channel)))))
      
      (define (post-play-between-marks-dialog)
        (unless play-between-marks-dialog
	  (let ((inits (find-two-marks))
		(max-mark-id (max-mark))
		(sliders ()))
	    
	    (if (null? inits)
		(snd-display ";no marks")
		
		(begin
		  (set! play-between-marks-m1 (car inits))
		  (set! play-between-marks-m2 (cadr inits))
		  (set-syncs)
		  (mark-sync-color "yellow")
		  
		  (set! play-between-marks-dialog 
			(make-effect-dialog play-between-marks-label
					    (if (provided? 'snd-gtk)
						(values (lambda (w context)
							  (cp-play-between-marks))
							(lambda (w context)
							  (help-dialog "Define selection by marks Help"
								       "Plays area between specified marks. Use the sliders to select the boundary marks."))
							(lambda (w data)
							  ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (car sliders)) play-between-marks-m1)
							  ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (cadr sliders)) play-between-marks-m2)))
						(values (lambda (w context info)
							  (cp-play-between-marks))
							(lambda (w context info)
							  (help-dialog "Define selection by marks Help"
								       "Plays area between specified marks. Use the sliders to select the boundary marks."))
							(lambda (w c i)
							  ((*motif* 'XtSetValues) (sliders 0) (list (*motif* 'XmNvalue) play-between-marks-m1))
							  ((*motif* 'XtSetValues) (sliders 1) (list (*motif* 'XmNvalue) play-between-marks-m2)))))))
		  (set! sliders
			(add-sliders 
			 play-between-marks-dialog
			 (list (let ((plyf1 (if (provided? 'snd-gtk)
						(lambda (w context)
						  (set! play-between-marks-m1 ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w)))
						  (set-syncs))
						(lambda (w context info)
						  (set! play-between-marks-m1 ((*motif* '.value) info))
						  (set-syncs)))))
				 (list "mark one" 0 play-between-marks-m1 max-mark-id plyf1 1))
			       (let ((plyf2 (if (provided? 'snd-gtk)
						(lambda (w context)
						  (set! play-between-marks-m2 ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w)))
						  (set-syncs))
						(lambda (w context info)
						  (set! play-between-marks-m2 ((*motif* '.value) info))
						  (set-syncs)))))
				 (list "mark two" 0 play-between-marks-m2 max-mark-id plyf2 1)))))
		  
		  (if (provided? 'snd-motif)
		      (with-let (sublet *motif*)
			(hook-push select-channel-hook (lambda (hook)
							 (let ((max-ms (max-mark))
							       (min-ms (min-mark))
							       (current-ms (find-two-marks)))
							   (if (null? current-ms)
							       (set! current-ms (list min-ms max-ms)))
							   (if max-ms
							       (for-each
								(lambda (slider)
								  (XtVaSetValues slider 
										 (list XmNmaximum max-ms
										       XmNminimum min-ms
										       XmNvalue (car current-ms)))
								  (set! current-ms (cdr current-ms)))
								sliders)))))
			(hook-push mark-hook (lambda (hook)
					       (if (and (= (hook 'snd) (selected-sound))
							(= (hook 'chn) (selected-channel))
							(= (hook 'reason) 0)) ; add-mark
						   (for-each
						    (lambda (slider)
						      (XtVaSetValues slider (list XmNmaximum (max-mark))))
						    sliders))))))))
	    (if play-between-marks-dialog
		(activate-dialog play-between-marks-dialog)))))
      
      (set! play-between-marks-menu-label (add-to-menu marks-menu "Play between marks" post-play-between-marks-dialog))))



(set! marks-list (cons (lambda ()
			 (let ((new-label (format #f "Play between marks (~D ~D)" play-between-marks-m1 play-between-marks-m2)))
			   (if play-between-marks-menu-label (change-label play-between-marks-menu-label new-label))
			   (set! play-between-marks-label new-label)))
		       marks-list))


;;; -------- Loop play between marks

(when (provided? 'xm)
  (with-let (sublet *motif*)
    
    (define loop-between-marks-m1 0)
    (define loop-between-marks-m2 1)
    (define loop-between-marks-buffer-size 512)
    (define loop-between-marks-label "Loop play between marks")
    (define loop-between-marks-dialog #f)
    (define loop-between-marks-default-buffer-widget #f)
    (define loop-between-marks-menu-label #f)
    
    (define use-combo-box-for-buffer-size #f) ; radio-buttons or combo-box choice
    
    (define (cp-loop-between-marks)
      ;; cp-loop-between-marks) loops between two marks, playing (marks-menu)
      (loop-between-marks (integer->mark loop-between-marks-m1) (integer->mark loop-between-marks-m2) loop-between-marks-buffer-size))
    
    (define (overall-max-mark-id default-max)
      (let ((maxid default-max))
	(for-each 
	 (lambda (snd-marks)
	   (for-each 
	    (lambda (chan-marks)
	      (for-each 
	       (lambda (m)
		 (set! maxid (max maxid (mark->integer m))))
	       chan-marks))
	    snd-marks))
	 (marks))
	maxid))
    
    (define (post-loop-between-marks-dialog)
      (unless loop-between-marks-dialog
	;; if loop-between-marks-dialog doesn't exist, create it
	(let ((initial-loop-between-marks-m1 0)
	      (initial-loop-between-marks-m2 1)
	      (sliders ())
	      (max-mark-id (overall-max-mark-id 25)))
	  (set! loop-between-marks-dialog
		(make-effect-dialog 
		 loop-between-marks-label
		 (lambda (w context info) 
		   (cp-loop-between-marks))
		 (lambda (w context info)
		   (help-dialog "Loop play between marks"
				"Move the sliders to set the mark numbers. Check a radio button to set the buffer size."))
		 (lambda (w c i)
		   (stop-playing))))
	  (set! sliders
		(add-sliders 
		 loop-between-marks-dialog
		 (list (list "mark one" 0 initial-loop-between-marks-m1 max-mark-id
			     (lambda (w context info)
			       (set! loop-between-marks-m1 (.value info)))
			     1)
		       (list "mark two" 0 initial-loop-between-marks-m2 max-mark-id
			     (lambda (w context info)
			       (set! loop-between-marks-m2 (.value info)))
			     1))))
	  
	  ;; now add either a radio-button box or a combo-box for the buffer size
	  ;;   need to use XtParent here since "mainform" isn't returned by add-sliders
	  
	  (if use-combo-box-for-buffer-size
	      ;; this block creates a "combo box" to handle the buffer size
	      (let* ((s1 (XmStringCreateLocalized "Buffer size"))
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
		     (lab (XtCreateManagedWidget "Buffer size" xmLabelWidgetClass frm
						 (list XmNleftAttachment      XmATTACH_FORM
						       XmNrightAttachment     XmATTACH_NONE
						       XmNtopAttachment       XmATTACH_FORM
						       XmNbottomAttachment    XmATTACH_FORM
						       XmNlabelString         s1
						       XmNbackground          *basic-color*)))
		     (buffer-labels (map XmStringCreateLocalized '("64" "128" "256" "512" "1024" "2048" "4096")))
		     (combo (XtCreateManagedWidget "buffersize" xmComboBoxWidgetClass frm
						   (list XmNleftAttachment      XmATTACH_WIDGET
							 XmNleftWidget          lab
							 XmNrightAttachment     XmATTACH_FORM
							 XmNtopAttachment       XmATTACH_FORM
							 XmNbottomAttachment    XmATTACH_FORM
							 XmNitems               buffer-labels
							 XmNitemCount           (length buffer-labels)
							 XmNcomboBoxType        XmDROP_DOWN_COMBO_BOX
							 XmNbackground          *basic-color*))))
		(set! loop-between-marks-default-buffer-widget combo)
		(for-each XmStringFree buffer-labels)
		(XmStringFree s1)
		(XtSetValues combo (list XmNselectedPosition 1))
		(XtAddCallback combo XmNselectionCallback
			       (lambda (w c i)
				 (set! loop-between-marks-buffer-size 
				       (string->number (XmStringUnparse (.item_or_text i) #f XmCHARSET_TEXT XmCHARSET_TEXT #f 0 XmOUTPUT_ALL))))))

	      ;; this block creates a "radio button box"
	      (let* ((s1 (XmStringCreateLocalized "Buffer size"))
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
		(XtCreateManagedWidget "Buffer size" xmLabelWidgetClass frm
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
							      XmNvalueChangedCallback (list (lambda (w c i) (if (.set i) (set! loop-between-marks-buffer-size c))) size)
							      XmNset                  (= size loop-between-marks-buffer-size)))))
		     (if (= size loop-between-marks-buffer-size)
			 (set! loop-between-marks-default-buffer-widget button))))
		 '(64 128 256 512 1024 2048 4096))
		(XmStringFree s1)))))
      (activate-dialog loop-between-marks-dialog))
    
    (set! loop-between-marks-menu-label (add-to-menu marks-menu "Loop play between marks" post-loop-between-marks-dialog))
    
    (set! marks-list (cons (lambda ()
			     (let ((new-label (format #f "Loop play between marks (~D ~D ~D)"
						      loop-between-marks-m1 loop-between-marks-m2 loop-between-marks-buffer-size)))
			       (if loop-between-marks-menu-label (change-label loop-between-marks-menu-label new-label))
			       (set! loop-between-marks-label new-label)))
			   marks-list))))

(add-to-menu marks-menu #f #f)


;;; -------- trim from and back (goes by first or last mark)

(define trim-front
  (let ((+documentation+ "trim-front finds the first mark in each of the syncd channels and removes all samples before it")
	(trim-front-one-channel 
	 (lambda (snd chn)
	   (if (null? (marks snd chn))
	       (status-report "trim-front needs a mark" snd)
	       (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)))))
    (lambda ()
      (let ((snc (sync)))
	(if (> snc 0)
	    (apply map
		   (lambda (snd chn)
		     (if (= (sync snd) snc)
			 (trim-front-one-channel snd chn)))
		   (all-chans))
	    (trim-front-one-channel (selected-sound) (selected-channel)))))))

(add-to-menu marks-menu "Trim before mark" trim-front)

(define trim-back
  (let ((+documentation+ "trim-back finds the last mark in each of the syncd channels and removes all samples after it")
	(trim-back-one-channel 
	 (lambda (snd chn)
	   (if (null? (marks snd chn))
	       (status-report "trim-back needs a mark" snd)
	       (let ((endpt (let ((ms (marks snd chn)))
			      (mark-sample (list-ref ms (- (length ms) 1))))))
		 (delete-samples (+ endpt 1) (- (framples snd chn) endpt)))))))
    (lambda ()
      (let ((snc (sync)))
	(if (> snc 0)
	    (apply map
		   (lambda (snd chn)
		     (if (= (sync snd) snc)
			 (trim-back-one-channel snd chn)))
		   (all-chans))
	    (trim-back-one-channel (selected-sound) (selected-channel)))))))

(add-to-menu marks-menu "Trim behind mark" trim-back)


;;; -------- crop (trims front and back)

(define crop
  (let ((+documentation+ "crop finds the first and last marks in each of the syncd channels and removes all samples outside them")
	(crop-one-channel 
	 (lambda (snd chn)
	   (if (< (length (marks snd chn)) 2)
	       (status-report "crop needs start and end marks" snd)
	       (as-one-edit
		(lambda ()
		  (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
		  (let ((endpt (let ((ms (marks snd chn)))
				 (mark-sample (list-ref ms (- (length ms) 1))))))
		    (delete-samples (+ endpt 1) (- (framples snd chn) endpt))))
		"crop")))))
    (lambda ()
      (let ((snc (sync)))
	(if (> snc 0)
	    (apply map
		   (lambda (snd chn)
		     (if (= (sync snd) snc)
			 (crop-one-channel snd chn)))
		   (all-chans))
	    (crop-one-channel (selected-sound) (selected-channel)))))))

(add-to-menu marks-menu "Crop around marks" crop)

(add-to-menu marks-menu #f #f)


;;; -------- Fit selection to marks

(define fit-to-mark-one 0)
(define fit-to-mark-two 1)
(define fit-to-mark-label "Fit selection to marks")
(define fit-to-mark-dialog #f)
(define fit-to-mark-menu-label #f)

(define cp-fit-to-marks
  (let ((+documentation+ "(cp-fit-to-marks) fits the selection between two marks (marks-menu)"))
    (lambda ()
      ((if (selection?) fit-selection-between-marks define-selection-via-marks)
       (integer->mark fit-to-mark-one) 
       (integer->mark fit-to-mark-two)))))

(if (not (or (provided? 'xm) 
	     (provided? 'xg)))
    (set! fit-to-mark-menu-label (add-to-menu marks-menu fit-to-mark-label cp-fit-to-marks))
    (begin
      
      (define (post-fit-to-mark-dialog)
        (unless fit-to-mark-dialog
	  (let ((initial-fit-to-mark-one 0)
		(initial-fit-to-mark-two 1)
		(sliders ()))
	    
	    (set! fit-to-mark-dialog 
		  (make-effect-dialog fit-to-mark-label
				      (if (provided? 'snd-gtk)
					  (values (lambda (w context) 
						    (cp-fit-to-marks))
						  (lambda (w context)
						    (help-dialog "Fit selection to marks Help"
								 "Fit-selection-between-marks tries to squeeze the current selection \
between two marks,using the granulate generator to fix up the selection duration (this still is not perfect). Move the sliders to set the mark numbers."))
						  (lambda (w data)
						    (set! fit-to-mark-one initial-fit-to-mark-one)
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (car sliders)) fit-to-mark-one)
						    (set! fit-to-mark-two initial-fit-to-mark-two)
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (cadr sliders)) fit-to-mark-two)))
					  (values (lambda (w context info)
						    (cp-fit-to-marks))
						  (lambda (w context info)
						    (help-dialog "Fit selection to marks Help"
								 "Fit-selection-between-marks tries to squeeze the current selection \
between two marks,using the granulate generator to fix up the selection duration (this still is not perfect). Move the sliders to set the mark numbers."))
						  (lambda (w c i)
						    (set! fit-to-mark-one initial-fit-to-mark-one)
						    ((*motif* 'XtSetValues) (sliders 0) (list (*motif* 'XmNvalue) fit-to-mark-one))
						    (set! fit-to-mark-two initial-fit-to-mark-two)
						    ((*motif* 'XtSetValues) (sliders 1) (list (*motif* 'XmNvalue) fit-to-mark-two)))))))
	    (set! sliders
		  (add-sliders 
		   fit-to-mark-dialog
		   (list (let ((fitf1 (if (provided? 'snd-gtk)
					 (lambda (w context) (set! fit-to-mark-one ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w))))
					 (lambda (w context info) (set! fit-to-mark-one ((*motif* '.value) info))))))
			   (list "mark one" 0 initial-fit-to-mark-one 20 fitf1 1))
			 (let ((fitf2 (if (provided? 'snd-gtk)
					  (lambda (w context) (set! fit-to-mark-two ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w))))
					  (lambda (w context info) (set! fit-to-mark-two (.value info))))))
			   (list "mark two" 0 initial-fit-to-mark-two 20 fitf2 1)))))))

	(activate-dialog fit-to-mark-dialog))
      
      (set! fit-to-mark-menu-label (add-to-menu marks-menu "Fit selection to marks" post-fit-to-mark-dialog))))


(set! marks-list (cons (lambda ()
			 (let ((new-label (format #f "Fit selection to marks (~D ~D)" fit-to-mark-one fit-to-mark-two)))
			   (if fit-to-mark-menu-label (change-label fit-to-mark-menu-label new-label))
			   (set! fit-to-mark-label new-label)))
		       marks-list))


;;; -------- Define selection by marks

(define define-by-mark-one 0)
(define define-by-mark-two 1)
(define define-by-mark-label "Define selection by marks")
(define define-by-mark-dialog #f)
(define define-by-mark-menu-label #f)

(define define-selection-via-marks 
  (let ((+documentation+ "(define-selection-via-marks m1 m2) defines the selection via marks (marks-menu)"))
    (lambda (m1 m2)
      (let ((m1sc (mark-home m1))
	    (m2sc (mark-home m2)))
	(if (not (equal? m1sc m2sc))
	    (snd-error "define-selection-via-marks assumes the marks are in the same channel")
	    (let ((beg (min (mark-sample m1) (mark-sample m2)))
		  (end (max (mark-sample m1) (mark-sample m2)))
		  (snd (car m1sc))
		  (chn (cadr m1sc)))
	      (set! (selection-member? snd chn) #t)
	      (set! (selection-position snd chn) beg)
	      (set! (selection-framples snd chn) (- (+ end 1) beg))))))))

(define (cp-define-by-marks)
  (define-selection-via-marks (integer->mark define-by-mark-one) (integer->mark define-by-mark-two)))

(if (not (or (provided? 'xm) 
	     (provided? 'xg)))
    (set! define-by-mark-menu-label (add-to-menu marks-menu define-by-mark-label cp-define-by-marks))
    (begin
      
      (define (post-define-by-mark-dialog)
        (unless define-by-mark-dialog
	  (let ((initial-define-by-mark-one 0)
		(initial-define-by-mark-two 1)
		(sliders ()))
	    
	    (set! define-by-mark-dialog 
		  (make-effect-dialog define-by-mark-label
				      (if (provided? 'snd-gtk)
					  (values (lambda (w context)
						    (cp-define-by-marks))
						  (lambda (w context)
						    (help-dialog "Define selection by marks Help"
								 "Selects and highlights area between marks. Use the sliders to choose the boundary marks."))
						  (lambda (w data)
						    (set! define-by-mark-one initial-define-by-mark-one)
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (car sliders)) define-by-mark-one)
						    (set! define-by-mark-two initial-define-by-mark-two)
						    ((*gtk* 'gtk_adjustment_set_value) ((*gtk* 'GTK_ADJUSTMENT) (cadr sliders)) define-by-mark-two)))
					  (values (lambda (w context info)
						    (cp-define-by-marks))
						  (lambda (w context info)
						    (help-dialog "Define selection by marks Help"
								 "Selects and highlights area between marks. Use the sliders to choose the boundary marks."))
						  (lambda (w c i)
						    (set! define-by-mark-one initial-define-by-mark-one)
						    ((*motif* 'XtSetValues) (sliders 0) (list (*motif* 'XmNvalue) define-by-mark-one))
						    (set! define-by-mark-two initial-define-by-mark-two)
						    ((*motif* 'XtSetValues) (sliders 1) (list (*motif* 'XmNvalue) define-by-mark-two)))))))
	    (set! sliders
		  (add-sliders 
		   define-by-mark-dialog
		   (list (let ((def1 (if (provided? 'snd-gtk)
					 (lambda (w context) (set! define-by-mark-one ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w))))
					 (lambda (w context info) (set! define-by-mark-one ((*motif* '.value) info))))))
			   (list "mark one" 0 initial-define-by-mark-one 25 def1 1))
			 (let ((def2 (if (provided? 'snd-gtk)
					 (lambda (w context) (set! define-by-mark-two ((*gtk* 'gtk_adjustment_get_value) ((*gtk* 'GTK_ADJUSTMENT) w))))
					 (lambda (w context info) (set! define-by-mark-two ((*motif* '.value) info))))))
			   (list "mark two" 0 initial-define-by-mark-two 25 def2 1)))))))

	(activate-dialog define-by-mark-dialog))
      
      (set! define-by-mark-menu-label (add-to-menu marks-menu "Define selection by marks" post-define-by-mark-dialog))))


(set! marks-list (cons (lambda ()
			 (let ((new-label (format #f "Define selection by marks (~D ~D)" define-by-mark-one define-by-mark-two)))
			   (if define-by-mark-menu-label (change-label define-by-mark-menu-label new-label))
			   (set! define-by-mark-label new-label)))
		       marks-list))

(add-to-menu marks-menu #f #f)


;;; ------- Start/stop mark sync

(define mark-sync-menu-label #f)

(define mark-sync-number 0)

(define start-sync 
  (let ((+documentation+ "(start-sync) starts mark syncing (marks-menu)"))
    (lambda ()
      (set! mark-sync-number (+ (mark-sync-max) 1)))))

(define stop-sync 
  (let ((+documentation+ "(stop-sync) stops mark-syncing (marks-menu)"))
    (lambda ()
      (set! mark-sync-number 0))))

(define click-to-sync 
  (let ((+documentation+ "(click-to-sync id) sets a mark's sync field when it is clicked (marks-menu)"))
    (lambda (id)
      (set! (sync id) mark-sync-number)
      #f)))

(hook-push mark-click-hook (lambda (hook) (click-to-sync (hook 'id))))


(define m-sync #f)
(define m-sync-label "Mark sync (On)")
(define no-m-sync-label "Mark sync (Off)")

(define msync!
  (let ((+documentation+ "(msync!) starts mark syncing (marks-menu)"))
    (lambda ()
      (set! m-sync #t)
      (if mark-sync-menu-label (change-label mark-sync-menu-label m-sync-label))
      (start-sync)
      (mark-sync-color "yellow"))))

(define unmsync!
  (let ((+documentation+ "(unmsync!) stops mark syncing (marks-menu)"))
    (lambda ()
      (set! m-sync #f)
      (if mark-sync-menu-label (change-label mark-sync-menu-label no-m-sync-label))
      (stop-sync))))

(set! mark-sync-menu-label 
      (add-to-menu marks-menu no-m-sync-label
		   (lambda ()
		     (if m-sync
			 (unmsync!)
			 (msync!)))))

(add-to-menu marks-menu #f #f)


;;; -------- Places marks at loop points specified in the file header

(add-to-menu marks-menu "Mark sample loop points" mark-loops)



;;; -------- mark loop dialog (this refers to sound header mark points, not Snd mark objects!)

(when (provided? 'xm) 
  (with-let (sublet *motif*)
    
    ;; Here is a first stab at the loop dialog (I guessed a lot as to what these buttons
    ;; are supposed to do -- have never used these loop points).
    
    (define loop-dialog #f)
    (define loop-data '(0 0 0 0 0 0 1 1))
    
    (define (update-labels start range end sus-rel range-in-secs)
      (let ((sr2 (* sus-rel 2)))
	(if range-in-secs
	    (begin
	      (change-label start (format #f "~,3F" (/ (loop-data sr2) (srate))))
	      (change-label range (format #f "~,3F" (/ (- (loop-data (+ 1 sr2)) (loop-data sr2)) (srate))))
	      (change-label end (format #f "~,3F" (/ (loop-data (+ 1 sr2)) (srate)))))
	    (begin
	      (change-label start (format #f "~D" (loop-data sr2)))
	      (change-label range (format #f "~D" (- (loop-data (+ 1 sr2)) (loop-data sr2))))
	      (change-label end (format #f "~D" (loop-data (+ 1 sr2))))))))
    
    (define (create-loop-dialog)
      (unless (Widget? loop-dialog)
	(let ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
	      (xsave (XmStringCreate "Save" XmFONTLIST_DEFAULT_TAG))
	      (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	      (titlestr (XmStringCreate "Loop Points" XmFONTLIST_DEFAULT_TAG)))
	  (set! loop-dialog
		(XmCreateTemplateDialog (cadr (main-widgets)) "loop-points"
					(list XmNcancelLabelString   xdismiss
					      XmNhelpLabelString     xhelp
					      XmNokLabelString       xsave
					      XmNautoUnmanage        #f
					      XmNdialogTitle         titlestr
					      XmNresizePolicy        XmRESIZE_GROW
					      XmNnoResize            #f
					      XmNbackground          *basic-color*
					      XmNtransient           #f)))
	  (XtAddCallback loop-dialog
			 XmNcancelCallback (lambda (w context info)
					     (XtUnmanageChild loop-dialog)))
	  (XtAddCallback loop-dialog
			 XmNhelpCallback (lambda (w context info)
					   (snd-print "set loop points")))
	  (XtAddCallback loop-dialog
			 XmNokCallback (lambda (w context info)
					 (set! (sound-loop-info) loop-data)))
	  (for-each XmStringFree (vector xhelp xdismiss titlestr xsave))
	  (let* ((mainform
		  (XtCreateManagedWidget "form" xmFormWidgetClass loop-dialog
					 (list XmNleftAttachment      XmATTACH_FORM
					       XmNrightAttachment     XmATTACH_FORM
					       XmNtopAttachment       XmATTACH_FORM
					       XmNbottomAttachment    XmATTACH_WIDGET
					       XmNbottomWidget        (XmMessageBoxGetChild loop-dialog XmDIALOG_SEPARATOR)
					       XmNbackground          *basic-color*)))
		 (leftform
		  (XtCreateManagedWidget "lform" xmFormWidgetClass mainform
					 (list XmNleftAttachment      XmATTACH_FORM
					       XmNrightAttachment     XmATTACH_POSITION
					       XmNrightPosition       50
					       XmNtopAttachment       XmATTACH_FORM
					       XmNbottomAttachment    XmATTACH_FORM
					       XmNbackground          *basic-color*)))
		 (rightform
		  (XtCreateManagedWidget "rform" xmFormWidgetClass mainform
					 (list XmNleftAttachment      XmATTACH_WIDGET
					       XmNleftWidget          leftform
					       XmNrightAttachment     XmATTACH_FORM
					       XmNtopAttachment       XmATTACH_FORM
					       XmNbottomAttachment    XmATTACH_FORM
					       XmNbackground          *basic-color*))))
	    (for-each
	     (lambda (parent top-label offset)
	       (let* ((frame-form 
		       (let ((main-frame 
			      (let ((main-label (XtCreateManagedWidget top-label xmLabelWidgetClass parent
								       (list XmNleftAttachment      XmATTACH_FORM
									     XmNrightAttachment     XmATTACH_FORM
									     XmNtopAttachment       XmATTACH_FORM
									     XmNbottomAttachment    XmATTACH_NONE))))
				(XtCreateManagedWidget "fr"  xmFrameWidgetClass parent
						       (list XmNleftAttachment      XmATTACH_FORM
							     XmNrightAttachment     XmATTACH_FORM
							     XmNtopAttachment       XmATTACH_WIDGET
							     XmNtopWidget           main-label
							     XmNbottomAttachment    XmATTACH_FORM
							     XmNshadowThickness     6
							     XmNshadowType          XmSHADOW_ETCHED_OUT)))))
			 (XtCreateManagedWidget "fform" xmFormWidgetClass main-frame ())))
		      (top-frame (XtCreateManagedWidget "topf" xmFrameWidgetClass frame-form
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_NONE)))
		      (top-form (XtCreateManagedWidget "tform" xmFormWidgetClass top-frame ()))
		      (left-column (XtCreateManagedWidget "lcol" xmRowColumnWidgetClass top-form
							  (list XmNorientation         XmVERTICAL
								XmNbackground          *position-color*
								XmNleftAttachment      XmATTACH_FORM
								XmNrightAttachment     XmATTACH_POSITION
								XmNrightPosition       40
								XmNtopAttachment       XmATTACH_FORM
								XmNbottomAttachment    XmATTACH_FORM)))
		      (mid-column (XtCreateManagedWidget "lcol" xmFormWidgetClass top-form
							 (list XmNleftAttachment      XmATTACH_WIDGET
							       XmNleftWidget          left-column
							       XmNrightAttachment     XmATTACH_POSITION
							       XmNrightPosition       60
							       XmNtopAttachment       XmATTACH_FORM
							       XmNbottomAttachment    XmATTACH_FORM)))
		      (right-column (XtCreateManagedWidget "lcol" xmRowColumnWidgetClass top-form
							   (list XmNorientation         XmVERTICAL
								 XmNbackground          *position-color*
								 XmNleftAttachment      XmATTACH_WIDGET
								 XmNleftWidget          mid-column
								 XmNrightAttachment     XmATTACH_FORM
								 XmNtopAttachment       XmATTACH_FORM
								 XmNbottomAttachment    XmATTACH_FORM)))
		      (rowlefttop (XtCreateManagedWidget "r1"  xmRowColumnWidgetClass left-column
							 (list XmNorientation         XmHORIZONTAL
							       XmNbackground          *position-color*
							       XmNspacing             0)))
		      (leftrange (XtCreateManagedWidget "range" xmPushButtonWidgetClass left-column ()))
		      (rowleftbottom (XtCreateManagedWidget "r1" xmRowColumnWidgetClass left-column
							    (list XmNorientation         XmHORIZONTAL
								  XmNbackground          *position-color*
								  XmNspacing             0)))
		      (rowrighttop (XtCreateManagedWidget "r1" xmRowColumnWidgetClass right-column
							  (list XmNorientation         XmHORIZONTAL
								XmNbackground          *position-color*
								XmNspacing             0)))
		      (rowrightbottom (XtCreateManagedWidget "r1" xmRowColumnWidgetClass right-column
							     (list XmNorientation         XmHORIZONTAL
								   XmNbackground          *position-color*
								   XmNspacing             0)))
		      (midlab1 (XtCreateManagedWidget "0.000" xmLabelWidgetClass mid-column
						      (list    XmNleftAttachment      XmATTACH_FORM
							       XmNrightAttachment     XmATTACH_FORM
							       XmNtopAttachment       XmATTACH_POSITION
							       XmNtopPosition         10
							       XmNbottomAttachment    XmATTACH_NONE)))
		      (midlab2 (XtCreateManagedWidget "0.000" xmLabelWidgetClass mid-column
						      (list    XmNleftAttachment      XmATTACH_FORM
							       XmNrightAttachment     XmATTACH_FORM
							       XmNtopAttachment       XmATTACH_POSITION
							       XmNtopPosition         40
							       XmNbottomAttachment    XmATTACH_NONE)))
		      (midlab3 (XtCreateManagedWidget "0.000" xmLabelWidgetClass mid-column
						      (list    XmNleftAttachment      XmATTACH_FORM
							       XmNrightAttachment     XmATTACH_FORM
							       XmNtopAttachment       XmATTACH_NONE
							       XmNbottomAttachment    XmATTACH_POSITION
							       XmNbottomPosition      90)))
		      (bottom-left-button 
		       (let ((bottom-left-label 
			      (let ((bottom-left 
				     (let ((bottom-form (XtCreateManagedWidget "bform" xmFormWidgetClass frame-form
									       (list XmNleftAttachment      XmATTACH_FORM
										     XmNrightAttachment     XmATTACH_FORM
										     XmNtopAttachment       XmATTACH_WIDGET
										     XmNtopWidget           top-frame
										     XmNbottomAttachment    XmATTACH_FORM))))
				       (XtCreateManagedWidget "bleft" xmFormWidgetClass bottom-form
							      (list XmNleftAttachment      XmATTACH_FORM
								    XmNrightAttachment     XmATTACH_NONE
								    XmNtopAttachment       XmATTACH_FORM
								    XmNbottomAttachment    XmATTACH_FORM)))))
				(XtCreateManagedWidget "Loop Mode" xmLabelWidgetClass bottom-left
						       (list XmNleftAttachment      XmATTACH_FORM
							     XmNrightAttachment     XmATTACH_FORM
							     XmNtopAttachment       XmATTACH_FORM
							     XmNbottomAttachment    XmATTACH_NONE)))))
			 (XtCreateManagedWidget "forwards" xmPushButtonWidgetClass bottom-left
						(list XmNleftAttachment      XmATTACH_FORM
						      XmNrightAttachment     XmATTACH_FORM
						      XmNtopAttachment       XmATTACH_WIDGET
						      XmNtopWidget           bottom-left-label
						      XmNbottomAttachment    XmATTACH_FORM))))
		      (range-in-secs #t))
		 (let ((mode 1))
		   (XtAddCallback bottom-left-button
				  XmNactivateCallback
				  (lambda (w context info)
				    (set! mode (if (= mode 1) 2 1))
				    (set! (loop-data (+ offset 6)) mode)
				    (change-label w (if (= mode 1) "forward" "forw/back")))))
		 (XtAddCallback leftrange XmNactivateCallback
				(lambda (w c i)
				  (set! range-in-secs (not range-in-secs))
				  (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))
		 (for-each
		  (lambda (rparent loc)
		    (let ((farleft (XtCreateManagedWidget "<<" xmPushButtonWidgetClass rparent ())))
		      (XtAddCallback farleft XmNactivateCallback
				     (lambda (w c i)
				       (let ((ml (if (= loc 0) 0 (loop-data sus-rel-start))))
					 (set! (loop-data (+ loc (* offset 2))) ml)
					 (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))
		    (let ((stopleft (XtCreateManagedWidget " O " xmPushButtonWidgetClass rparent ())))
		      (XtAddCallback stopleft XmNactivateCallback
				     (lambda (w c i)
				       (let ((ml (if (= loc 0) 0 (loop-data sus-rel-start))))
					 (set! (loop-data (+ loc (* offset 2))) ml)
					 (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))
		    (let ((lotsleft (XtCreateManagedWidget "<< " xmPushButtonWidgetClass rparent ())))
		      (XtAddCallback lotsleft XmNactivateCallback
				     (lambda (w c i)
				       (let ((ml (if (= loc 0) 0 (loop-data sus-rel-start))))
					 (set! (loop-data (+ loc (* offset 2))) (max ml (- (loop-data (+ loc (* offset 2))) 10)))
					 (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))
		    (let ((someleft (XtCreateManagedWidget " < " xmPushButtonWidgetClass rparent ()))
			  (sus-rel-start (* offset 2)))
		      (XtAddCallback someleft XmNactivateCallback
				     (lambda (w c i)
				       (let ((ml (if (= loc 0) 0 (loop-data sus-rel-start))))
					 (set! (loop-data (+ loc (* offset 2))) (max ml (- (loop-data (+ loc (* offset 2))) 1)))
					 (update-labels midlab1 midlab2 midlab3 offset range-in-secs))))))
		  (list rowlefttop rowleftbottom)
		  '(0 1))
		 
		 (for-each
		  (lambda (rparent loc)
		    (let ((sus-rel-start (+ (* offset 2) 1)))
		      (let ((someright (XtCreateManagedWidget " > " xmPushButtonWidgetClass rparent ())))
			(XtAddCallback someright XmNactivateCallback
				       (lambda (w c i)
					 (let ((ml (if (= loc 0) (loop-data sus-rel-start) (framples))))
					   (set! (loop-data (+ loc (* offset 2))) (min ml (+ (loop-data (+ loc (* offset 2))) 1)))
					   (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))
		      (let ((lotsright (XtCreateManagedWidget " >>" xmPushButtonWidgetClass rparent ())))
			(XtAddCallback lotsright XmNactivateCallback
				       (lambda (w c i)
					 (let ((ml (if (= loc 0) (loop-data sus-rel-start) (framples))))
					   (set! (loop-data (+ loc (* offset 2))) (min ml (+ (loop-data (+ loc (* offset 2))) 10)))
					   (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))
		      (let ((stopright (XtCreateManagedWidget " O " xmPushButtonWidgetClass rparent ())))
			(XtAddCallback stopright XmNactivateCallback
				       (lambda (w c i)
					 (let ((ml (if (= loc 0) (loop-data sus-rel-start) (framples))))
					   (set! (loop-data (+ loc (* offset 2))) ml)
					   (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))
		      (let ((farright (XtCreateManagedWidget ">>" xmPushButtonWidgetClass rparent ())))
			(XtAddCallback farright XmNactivateCallback
				       (lambda (w c i)
					 (let ((ml (if (= loc 0) (loop-data sus-rel-start) (framples))))
					   (set! (loop-data (+ loc (* offset 2))) ml)
					   (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))))))
		  (list rowrighttop rowrightbottom)
		  '(0 1))))
	     
	     (list leftform rightform)
	     '("Sustain" "Release")
	     '(0 1)))
	  (for-each-child
	   loop-dialog
	   (lambda (n)
	     (if (and (XtIsWidget n)
		      (not (XmIsRowColumn n))
		      (not (XmIsSeparator n)))
		 (begin
		   (XmChangeColor n *basic-color*)
		   (if (XmIsToggleButton n)
		       (XtVaSetValues n (list XmNselectColor
					      (let* ((col (XColor))
						     (dpy (XtDisplay (cadr (main-widgets))))
						     (cmap (DefaultColormap dpy (DefaultScreen dpy))))
						(XAllocNamedColor dpy cmap "yellow" col col)
						(.pixel col)))))))))
	  ))
      (XtManageChild loop-dialog))
    
    (add-to-menu marks-menu "Show loop editor" create-loop-dialog)
    ))


(add-to-menu marks-menu #f #f)


;;; -------- Delete all marks 

(add-to-menu marks-menu "Delete all marks" delete-marks)

(add-to-menu marks-menu #f #f)


;;; -------- Explode all marks to separate files

(define mark-explode
  (let ((+documentation+ "(mark-explode) produces separate files as delineated by successive marks (marks-menu)"))
    (lambda ()
      (let ((start 0))
	(for-each
	 (lambda (mark)
	   (let ((len (- (mark-sample mark) start))
		 (filename (snd-tempnam)))
	     (array->file filename
			  (channel->float-vector start len)
			  len (srate) 1)
	     (set! start (mark-sample mark))))
	 (caar (marks)))))))

(add-to-menu marks-menu "Explode marks to files" mark-explode)
