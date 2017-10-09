(provide 'snd-misc.scm)

(if (not (provided? 'snd-motif)) (snd-error "misc.scm only works in the Motif version of Snd."))

(require snd-snd-motif.scm snd-examp.scm snd-extensions.scm snd-dsp.scm snd-draw.scm snd-env.scm snd-enved.scm)
(require snd-hooks.scm snd-marks.scm snd-mix.scm snd-moog.scm snd-play.scm snd-rubber.scm snd-zip.scm snd-edit123.scm)
(require snd-new-effects.scm snd-special-menu.scm snd-new-backgrounds.scm snd-marks-menu.scm snd-fft-menu.scm snd-effects-utils.scm)

(with-let *motif*

(keep-file-dialog-open-upon-ok)
(set! *ask-about-unsaved-edits* #t)
(if (not (hook-member show-disk-space after-open-hook))
    (hook-push after-open-hook show-disk-space))

;(define wd (make-pixmap (cadr (main-widgets)) rough)) ; this comes from new-backgrounds.scm
;(for-each-child (cadr (main-widgets)) (lambda (w) (XtSetValues w (list XmNbackgroundPixmap wd))))


(let ((paint-all 
       (let ((wd (make-pixmap (cadr (main-widgets)) rough)))
	 (lambda (widget)
	   (for-each-child
	    widget
	    (lambda (w)
	      (if (and (Widget? w)
		       (or (not (XmIsPushButton w))
			   (member (XtName w) '("revscl-label" "contrast-label" "expand-label" "srate-label" "amp-label") string=?)))
		  (XtSetValues w (list XmNbackgroundPixmap wd)))))))))

  (define (hook-paint-all hook) 
    (paint-all (hook 'widget)))
  
  (paint-all (cadr (main-widgets)))
  (for-each
   (lambda (w)
     (if (and w
	      (Widget? w))
	 (paint-all w)))
   (dialog-widgets))
  
  (if (not (hook-member hook-paint-all new-widget-hook))
      (hook-push new-widget-hook hook-paint-all)))

(set! *mix-waveform-height* 32)

;;; (with-level-meters 2)

(add-mark-pane)

(for-each add-sound-file-extension '("ogg" "OGG" "sf" "SF2" "mp3" "MP3" "W01" "W02" "W03" "W04" "W05" "W06" "W07" 
				     "W08" "W09" "W10" "w01" "w02" "w03" "w04" "w05" "w06" "w07" "w08" "w09" "w10"))

;;;
;;; disable original Play radio button
;;;

;(hook-push after-open-hook
;           (lambda (hook)
;             (XtUnmanageChild (find-child (list-ref (sound-widgets (hook 'snd)) 2) "play"))))


;;;
;;; main menu additions
;;;

;;; -------- add delete and rename options to the file menu

(define (add-delete-option)
  (add-to-menu 0 "Delete" ; add Delete option to File menu
	       (lambda ()
		 ;; close current sound and delete it
		 (if (>= (selected-sound) 0)
		     (let ((filename (file-name)))
		       (close-sound)
		       (delete-file filename))))
	       8)) ; place after File:New

(define (add-rename-option)
  (let ((rename-dialog #f)
	(rename-text #f))
    (add-to-menu 0 "Rename" 
      (lambda ()
	;; open dialog to get new name, save-as that name, open
	(unless rename-dialog
	  ;; make a standard dialog
	  (let* ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
		 (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
		 (xok (XmStringCreate "DoIt" XmFONTLIST_DEFAULT_TAG))
		 (titlestr (XmStringCreate "Rename" XmFONTLIST_DEFAULT_TAG))
		 (new-dialog (XmCreateTemplateDialog
			      (cadr (main-widgets)) "Rename"
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
	    
	    (XtAddCallback new-dialog XmNcancelCallback 
			   (lambda (w c i) (XtUnmanageChild w)))
	    
	    (XtAddCallback new-dialog XmNhelpCallback 
			   (lambda (w c i)
			     (help-dialog "Rename" "Give a new file name to rename the currently selected sound.")))
	    
	    (XtAddCallback new-dialog XmNokCallback 
			   (lambda (w c i)
			     (let ((new-name (XmTextFieldGetString rename-text)))
			       (when (and (string? new-name)
					  (> (length new-name) 0)
					  (>= (selected-sound) 0))
				 (save-sound-as new-name)
				 (close-sound)
				 (open-sound new-name)
				 (XtUnmanageChild w)))))
	    (for-each XmStringFree (vector xhelp xok xdismiss titlestr))
	    (set! rename-dialog new-dialog)
	    (let* ((mainform (XtCreateManagedWidget "formd" xmRowColumnWidgetClass rename-dialog
						    (list XmNleftAttachment      XmATTACH_FORM
							  XmNrightAttachment     XmATTACH_FORM
							  XmNtopAttachment       XmATTACH_FORM
							  XmNbottomAttachment    XmATTACH_WIDGET
							  XmNbottomWidget        (XmMessageBoxGetChild rename-dialog XmDIALOG_SEPARATOR)
							  XmNorientation         XmVERTICAL
							  XmNbackground          *basic-color*)))
		   (label (XtCreateManagedWidget "new name:" xmLabelWidgetClass mainform
						 (list XmNleftAttachment      XmATTACH_FORM
						       XmNrightAttachment     XmATTACH_NONE
						       XmNtopAttachment       XmATTACH_FORM
						       XmNbottomAttachment    XmATTACH_FORM
						       XmNbackground          *basic-color*))))
	      (set! rename-text 
		    (XtCreateManagedWidget "newname" xmTextFieldWidgetClass mainform
					   (list XmNleftAttachment      XmATTACH_WIDGET
						 XmNleftWidget          label
						 XmNrightAttachment     XmATTACH_FORM
						 XmNtopAttachment       XmATTACH_FORM
						 XmNbottomAttachment    XmATTACH_FORM
						 XmNbackground          *basic-color*)))
	      (XtAddEventHandler rename-text EnterWindowMask #f
				 (lambda (w context ev flag)
				   (XmProcessTraversal w XmTRAVERSE_CURRENT)
				   (XtSetValues w (list XmNbackground (white-pixel)))))
	      (XtAddEventHandler rename-text LeaveWindowMask #f
				 (lambda (w context ev flag)
				   (XtSetValues w (list XmNbackground *basic-color*)))))))
	((if (not (XtIsManaged rename-dialog)) XtManageChild raise-dialog) rename-dialog))
      8)))

(install-searcher-with-colors (lambda (file) #t))
(add-delete-option)
(add-rename-option)



(add-to-menu 1 #f #f) ; separator

;;;
;;; additions to Edit menu
;;;

;;; -------- cut selection -> new file

(define cut-selection->new
  (let ((selctr 0))
    (lambda ()
      (if (selection?)
	  (let ((new-file-name (format #f "sel-~D.snd" selctr)))
	    (set! selctr (+ selctr 1))
	    (save-selection new-file-name)
	    (delete-selection)
	    (open-sound new-file-name))))))

;;; (add-to-menu 1 "Cut Selection -> New" cut-selection->new)

;;; -------- append selection

(define (append-selection)
  (if (selection?)
      (insert-selection (framples))))

(add-to-menu 1 "Append Selection" append-selection)

;;; Replace with selection
;;;

(define (replace-with-selection)
  (let ((beg (cursor))
        (len (selection-framples)))
    (delete-samples beg len)
    (insert-selection beg)))

(add-to-menu 1 "Replace with Selection" replace-with-selection)

;;; (add-to-menu 1 #f #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; open and convert stereo MP3 files automatically
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hook-push open-raw-sound-hook
           (lambda (hook)
             (set! (hook 'result) (list 2 44100 (if (little-endian?) mus-lshort mus-bshort)))))

(hook-push open-hook
           (lambda (hook)
	     (let ((filename (hook 'name)))
	       (if (= (mus-sound-header-type filename) mus-raw)
		   (let ((rawfile (string-append filename ".raw")))
		     (system (format #f "mpg123 -s ~A > ~A" filename rawfile))
		     (set! (hook 'result) rawfile))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; open and convert stereo OGG files automatically
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hook-push open-raw-sound-hook
           (lambda (hook)
             (set! (hook 'result) (list 2 44100 (if (little-endian?) mus-lshort mus-bshort)))))

(hook-push open-hook
           (lambda (hook)
	     (let ((filename (hook 'name)))
	       (if (= (mus-sound-header-type filename) mus-raw)
		   (let ((rawfile (string-append filename ".raw")))
		     (system (format #f "ogg123 -d raw -f ~A ~A" rawfile filename))
		     (set! (hook 'result) rawfile))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; set up a region play list
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (region-play-list data)
  ;; data is list of lists (list (list time region)...), time in secs
  (for-each
   (lambda (tone)
     (let ((time (* 1000 (car tone)))
           (region (cadr tone)))
       (if (region? region)
           (in time (lambda () (play region))))))
   data))

;;; (region-play-list (list (list 0.0 0) (list 0.5 1) (list 1.0 2) (list 1.0 0)))

;;; Deselect function
;;;

(define deselect-all unselect-all)

)
