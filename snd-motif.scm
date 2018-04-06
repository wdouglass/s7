;;; snd-motif.scm -- Motif-related procedures (all use xm.so, most assume Motif 2)
;;;
;;; (install-searcher proc) -- use proc as File Selection Box filter, also install-searcher-with-colors
;;; (zync) and (unzync) -- cause y-zoom sliders to move together
;;; (for-each-child w func) -- apply func to w and all its children, similarly find-child and display-widget-tree
;;; (make-pixmap strs) turns xpm-style description into pixmap
;;; (disable-control-panel) does away with the control panel
;;; (add-mark-pane) adds a pane of mark locations to each channel that has marks
;;; (snd-clock-icon snd hour) show an animated clock icon
;;; (make-sound-box name parent select-func peak-func sounds args) makes a box of sound icons
;;; (make-channel-drop-site snd chn) -- add a drop site
;;; (set-channel-drop drop snd chn) -- change given graph drop callback to drop
;;; (select-file func title dir filter help) starts a Snd-like File Selection Dialog running func if a file is selected
;;; (show-disk-space snd) adds a label to the status-area area showing the current free space 
;;; (keep-file-dialog-open-upon-ok) changes File:Open so that clicking "ok" does not "unmanage" the dialog
;;; (add-amp-controls) adds amp sliders to the control panel for multi-channel sounds
;;; (remove-main-menu menu) removes a top-level menu
;;; add delete and rename options to the file menu (add-delete-option) (add-rename-option)
;;; (mark-sync-color new-color) sets the color of syncd marks
;;; (add-tooltip widget tip) adds tooltip tip to widget
;;; (menu-option name) to access menu items
;;; (show-all-atoms) shows all X atoms
;;; show-widget-font shows what fonts are associated with a widget
;;; add-find-to-listener enables C-s and C-r in the listener
;;; add a function to be called when the window manager sends us a "save yourself" message
;;; add-text-to-status-area puts a text widget in the notebook status area
;;; make-variable-display displays an arbitrary set of expressions/variables in a notebook widget
;;; with-minmax-button adds an open/close button to each sound pane
;;; notebook-with-top-tabs (for Xemacs-like list of open files across the top of the window)
;;; create-audit-dialog
;;; equalize-panes

(provide 'snd-snd-motif.scm)
(require snd-motif snd-extensions.scm snd-play.scm snd-dsp.scm)

(with-let *motif*
  
  (define (find-if pred lst)
    (cond ((null? lst) #f)
	  ((pred (car lst)) (car lst))
	  (else (find-if pred (cdr lst)))))
  
  (define load-font 
    (let ((+documentation+ "(load-font name) loads the font 'name', returning the font id"))
      (lambda (name)
	(let ((fs (XLoadQueryFont (XtDisplay (cadr (main-widgets))) name)))
	  (and fs (XFontStruct? fs) (.fid fs))))))
  
  (define current-screen
    (let ((+documentation+ "(current-screen) returns the current X screen number of the current display"))
      (lambda ()
	(DefaultScreenOfDisplay 
	  (XtDisplay (cadr (main-widgets)))))))
  
  (define white-pixel
    (let ((+documentation+ "(white-pixel) returns a white pixel"))
      (lambda ()
	(WhitePixelOfScreen (current-screen)))))
  
  (define black-pixel
    (let ((+documentation+ "(black-pixel) returns a black pixel"))
      (lambda ()
	(BlackPixelOfScreen (current-screen)))))
  
  (define screen-depth
    (let ((+documentation+ "(screen-depth) returns the current screen depth"))
      (lambda ()
	(DefaultDepthOfScreen (current-screen)))))
  
  (define xm-clean-string 
    (let ((+documentation+ "(xm-clean-string str) changes slash to underbar in the filename 'str' (for the peak env file)"))
      (lambda (str)
	;; full file name should be unique, so I think we need only fix it up to look like a flat name
	(let ((len (length str)))
	  (do ((new-str (make-string len #\.))
	       (i 0 (+ i 1)))
	      ((= i len) new-str)
	    (let ((c (str i)))
	      (set! (new-str i) (if (memv c '(#\\ #\/)) #\_ c))))))))
  
  
;;; -------- apply func to every widget belonging to w (and w) --------
  
  (define for-each-child 
    (let ((+documentation+ "(for-each-child w func) applies func to w and its descendents"))
      (lambda (w func)
	(func w)
	(if (XtIsComposite w)
	    (for-each 
	     (lambda (n)
	       (for-each-child n func))
	     (cadr (XtGetValues w (list XmNchildren 0) 1)))))))
  
  (define find-child 
    (let ((+documentation+ "(find-child widget name) returns a widget named 'name', if one can be found in the widget hierarchy beneath 'widget'"))
      (lambda (widget name)
	;; unfortunately, if the widget's name has been set for some non-English locale, this
	;;   won't work -- we need to add gettext support (see snd.c for an example)
	(call-with-exit
	 (lambda (return)
	   (for-each-child
	    widget
	    (lambda (child)
	      (if (string=? (XtName child) name)
		  (return child))))
	   (throw 'no-such-widget (list "find-child" name)))))))
  
  (define display-widget-tree 
    (let ((+documentation+ "(display-widget-tree widget) displays the hierarchy of widgets beneath 'widget'"))
      (lambda (widget)
	(let display-widget ((w widget)
			     (spaces ""))
	  (let ((name (XtName w)))
	    (if (or (not (string? name))
		    (string=? name ""))
		(set! name "<unnamed>"))
	    (format () "~A~A~%" spaces name)
	    (if (XtIsComposite w)
		(for-each (lambda (n)
			    (display-widget n (string-append spaces "  ")))
			  (cadr (XtGetValues w (list XmNchildren 0) 1)))))))))

  (define set-main-color-of-widget 
    (let ((+documentation+ "(set-main-color-of-widget w) sets the background color of widget 'w'"))
      (lambda (w)
	(for-each-child 
	 w
	 (lambda (n)
	   (if (XtIsWidget n)
	       (XmChangeColor n (if (XmIsScrollBar n) *position-color* *basic-color*))))))))
  
  (define host-name
    (let ((+documentation+ "(host-name) -> name of current machine"))
      (lambda ()
	(let ((host (let ((dpy (XtDisplay (cadr (main-widgets))))
			  (win (XtWindow (cadr (main-widgets)))))
		      (XGetWindowProperty dpy win (XInternAtom (XtDisplay (cadr (main-widgets))) "WM_CLIENT_MACHINE" #f) 0 32 #f XA_STRING))))
	  (and (pair? host) (host 5))))))
  
  
;;; -------- install-searcher --------
;;;
;;; replaces the current file search procedure in the File Selection Box
  
  (define install-searcher-with-colors 
    (let ((match-sound-files 
	   (lambda args
	     (let ((func (car args))
		   (matches ()))
	       (for-each
		(lambda (file)
		  (if (func file)
		      (set! matches (cons file matches))))
		(sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
	       matches)))
	  (XmString->string 
	   (lambda (str)
	     (XmStringUnparse str #f XmCHARSET_TEXT XmCHARSET_TEXT #f 0 XmOUTPUT_ALL))))
      (lambda (proc)
	(let ((dialog (open-file-dialog #f))
	      ;; (XtGetValues dialog (XmNfileSearchProc 0)) to get the default
	      (rendertable (let* ((tags (vector "one" "two" "three" "four"))
				  (pixels (let* ((dpy (XtDisplay (cadr (main-widgets))))
						 (cmap (DefaultColormap dpy (DefaultScreen dpy))))
					    (map
					     (lambda (color)
					       (let ((col (XColor)))
						 (if (= (XAllocNamedColor dpy cmap color col col) 0)
						     (snd-error (format #f "can't allocate ~A" color))
						     (.pixel col))))
					     '("black" "red" "blue" "orange")))))
			     (XmRenderTableAddRenditions 
			      #f 
			      (map (lambda (tag pix)
				     (XmRenditionCreate 
				      (cadr (main-widgets))
				      tag
				      (list XmNrenditionForeground pix
					    XmNfontName "9x15"
					    XmNfontType XmFONT_IS_FONT)))
				   tags pixels)
			      (length tags)
			      XmMERGE_NEW))))
	  (XtSetValues dialog
		       (list XmNfileSearchProc
			     (lambda (widget info)
			       (let* ((files (let ((dir (XmString->string (.dir info))))  ; may need filter text here?
					       (sort! (map 
						       (lambda (n) 
							 (string-append dir n)) 
						       (match-sound-files proc dir))
						      string<?)))               ; alphabetical order
				      (fileTable (map
						  (lambda (n)
						    (XmStringGenerate 
						     n #f XmCHARSET_TEXT 
						     (case (channels n)
						       ((1) "one")
						       ((2) "two")
						       ((3) "three")
						       (else "four"))))
						  files)))
				 (XtSetValues widget
					      (list XmNfileListItems fileTable
						    XmNfileListItemCount (length files)
						    XmNlistUpdated #t))
				 (for-each XmStringFree fileTable)))))
	  (XtUnmanageChild (XmFileSelectionBoxGetChild dialog XmDIALOG_DIR_LIST))
	  (XtUnmanageChild (XmFileSelectionBoxGetChild dialog XmDIALOG_DIR_LIST_LABEL))
	  (XtSetValues (XmFileSelectionBoxGetChild dialog XmDIALOG_LIST)
		       (list XmNrenderTable rendertable))
	  (XmFileSelectionDoSearch dialog #f)))))
      
  ;; (install-searcher-with-colors (lambda (file) #t))
  
  
;;; -------- keep-file-dialog-open-upon-ok
;;;
;;; change File:Open (or File:Mix) so that clicking "ok" does not "unmanage" the dialog
  
  (define keep-file-dialog-open-upon-ok
    (let ((+documentation+ "(keep-file-dialog-open-upon-ok) changes the File:Open menu so that clicking 'ok' does not close the dialog"))
      (lambda ()
	(let ((dialog (open-file-dialog #f)))
	  (XtRemoveAllCallbacks dialog XmNokCallback) ; remove built-in version
	  (XtAddCallback dialog XmNokCallback
			 (lambda (widget context info)
			   ;; same as built-in "ok" callback, but does not "unmanage" the dialog
			   (let ((filename (XmStringUnparse (.value info) #f XmCHARSET_TEXT XmCHARSET_TEXT #f 0 XmOUTPUT_ALL)))
			     (format () "filename: ~A~%" filename)
			     
			     (if (file-exists? filename)
				 (if (not (directory? filename))
				     (begin
				       (open-sound filename)
				       (select-channel 0))
				     (snd-error (format #f "~S is a directory" filename)))
				 (snd-error (format #f "no such file: ~A" filename))))))
	  'ok)))) ; prettier in listener than printing out a callback procedure
  
  
  
;;; -------- zync and unzync: start or stop y-zoom slider sync --------
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop
  
  (define remove-dragger 
    (let ((+documentation+ "(remove-dragger snd) undoes an earlier add-dragger which syncs together all the y-zoom sliders"))
      (lambda (snd)
	(let ((calls (sound-property 'dragger snd)))
	  (if calls
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn (channels snd)))
		(let ((zy ((channel-widgets snd chn) 6)))
		  (XtRemoveCallback zy XmNdragCallback (car calls))
		  (set! calls (cdr calls))))))
	(set! (sound-property 'dragger snd) #f))))
  
  (define add-dragger 
    (let ((+documentation+ "(add-dragger snd) syncs together y-zoom sliders"))
      (lambda (hook)
	(let ((snd (hook 'snd)))
	  (set! (sound-property 'save-state-ignore snd)
		(cons 'dragger
		      (or (sound-property 'save-state-ignore snd)
			  (list 'save-state-ignore))))
	  (set! (sound-property 'dragger snd)
		(do ((calls ())
		     (chn 0 (+ 1 chn)))
		    ((= chn (channels snd))
		     (set! (hook 'result) (reverse calls)))     
		  (let ((new-callback 
			 (let* ((zy ((channel-widgets snd chn) 6))
				(zy-div (max 10 (- (cadr (XtGetValues zy (list XmNmaximum 0))) 
						   (cadr (XtGetValues zy (list XmNsliderSize 0))))))) ; this is relative to max size
			   (XtAddCallback zy
					  XmNdragCallback 
					  (lambda (w data info)
					    (let ((v (/ (.value info) zy-div)))
					      (do ((i 0 (+ i 1)))
						  ((= i (channels snd)))
						(if (not (= i chn))
						    (begin
						      (set! (y-zoom-slider snd i) (* v v))
						      (set! (y-position-slider snd i) (y-position-slider snd chn)))))))))))
		    (set! calls (cons new-callback calls)))))))))
  
  (define zync
    (let ((+documentation+ "(zync) ties each sound's y-zoom sliders together so that all change in parallel if one changes"))
      (lambda ()
	(hook-push after-open-hook add-dragger)
	(for-each
	 (lambda (n)
	   (if (not (sound-property 'dragger n))
	       (add-dragger n)))
	 (sounds)))))
  
  (define unzync
    (let ((+documentation+ "(unzync) undoes a previous (zync) -- subsequently each sound's y-zoom sliders are independent"))
      (lambda ()
	(hook-remove after-open-hook add-dragger)
	(for-each
	 (lambda (n)
	   (if (sound-property 'dragger n)
	       (remove-dragger n)))
	 (sounds)))))
  
  
  
;;; -------- add our own pane to the channel section --------
  
  (define add-channel-pane 
    (let ((+documentation+ "(add-channel-pane snd chn name type (args ())) adds a pane to the channel section"))
      (lambda* (snd chn name type (args ()))
	(XtCreateManagedWidget name type (XtParent (XtParent ((channel-widgets snd chn) 7))) args))))
  
  
;;; -------- add our own pane to the sound section (underneath the controls in this case) --------
  
  (define add-sound-pane 
    (let ((+documentation+ "(add-sound-pane snd name type (args ())) adds a pane to the sound section (underneath the control panel)"))
      (lambda* (snd name type (args ()))
	(XtCreateManagedWidget name type (car (sound-widgets snd)) args))))
  
  
;;; -------- add our own pane to the overall Snd window (underneath the listener in this case) --------
  
  (define add-main-pane 
    (let ((+documentation+ "(add-main-pane name type (args ())) adds a pane to Snd (underneath the listener)"))
      (lambda* (name type (args ()))
	(XtCreateManagedWidget name type (or ((main-widgets) 5) ((main-widgets) 3)) args))))
  
  
;;; -------- add a widget at the top of the listener
  
  (define add-listener-pane 
    (let ((+documentation+ "(add-listener-pane name type args) adds a widget at the top of the listener"))
      (lambda (name type args)
	(let* ((listener-scroll (XtParent (find-child (cadr (main-widgets)) "lisp-listener")))
	       (listener-form (XtParent listener-scroll)))
	  ;; to insert the new widget at the top of the listener pane we need to detach the
	  ;;   listener scrolled window etc -- assume here that the "args" list does not
	  ;;   include any ATTACH_* arguments
	  (XtUnmanageChild listener-scroll)
	  (let ((top-widget (XtCreateManagedWidget name type listener-form
						   (append 
						    (list XmNleftAttachment   XmATTACH_FORM
							  XmNrightAttachment  XmATTACH_FORM
							  XmNtopAttachment    XmATTACH_FORM)
						    args))))
	    (XtVaSetValues listener-scroll (list XmNtopAttachment XmATTACH_WIDGET
						 XmNtopWidget     top-widget))
	    (XtManageChild listener-scroll)
	    top-widget)))))
  
					;(add-channel-pane "new-pane" 
					;		  xmDrawingAreaWidgetClass 
					;		  (list XmNbackground *graph-color*
					;			XmNforeground *data-color*))
  
  (define remove-menu-bar-menu 
    (let ((+documentation+ "(remove-menu-bar-menu which) removes a top-level menu; 'which' can be 0: top-level-menu-bar, 1: file-menu, \
2: edit-menu, 3: view-menu, 4: options-menu, 5: help-menu, 6: default popup menu"))
      (lambda (which)
	(XtUnmanageChild ((menu-widgets) which)))))
  
  #|
  (define (add-second-row)
  ;; adds a row-column widget just under the menu bar
  (let ((menu-bar (car (menu-widgets)))
  (main-pane (caddr (main-widgets)))
  (sound-pane (cadddr (main-widgets))))
  (XtUnmanageChild sound-pane)
  (let* ((second-row (XtCreateManagedWidget "second-row" xmRowColumnWidgetClass main-pane
  (list XmNleftAttachment    XmATTACH_FORM
  XmNrightAttachment  XmATTACH_FORM
  XmNbottomAttachment XmATTACH_NONE
  XmNtopAttachment    XmATTACH_WIDGET
  XmNtopWidget        menu-bar
  XmNbackground       *highlight-color*)))
  (sep (XtCreateManagedWidget "sep" xmSeparatorWidgetClass main-pane
  (list XmNleftAttachment    XmATTACH_FORM
  XmNrightAttachment  XmATTACH_FORM
  XmNbottomAttachment XmATTACH_NONE
  XmNtopAttachment    XmATTACH_WIDGET
  XmNtopWidget        second-row
  XmNbackground       *highlight-color*
  XmNorientation      XmHORIZONTAL
  XmNseparatorType    XmSHADOW_ETCHED_OUT))))
  (XtVaSetValues sound-pane (list XmNtopAttachment XmATTACH_WIDGET
  XmNtopWidget sep
  XmNbackground       *highlight-color*))
  (XtManageChild sound-pane)
  (XtCreateManagedWidget "a name" xmPushButtonWidgetClass second-row
  (list XmNbackground       *highlight-color*)))))
  |#
  
;;; -------- disable control panel --------
  
  (define disable-control-panel 
    (let ((+documentation+ "(disable-control-panel snd) disables the control panel for the sound 'snd'"))
      (lambda (snd)
	(let ((swc (caddr (sound-widgets snd))))
	  (for-each-child swc 
			  (lambda (n) 
			    (if (and n 
				     (not (string=? (XtName n) "snd-name-form")) 
				     (not (string=? (XtName (XtParent n)) "snd-name-form")))
				(XtUnmanageChild n))))
	  (XtSetValues swc (list XmNpaneMaximum 1 
				 XmNpaneMinimum 1))
	  (remove-from-menu 2 "Show controls")
	  (XtManageChild swc)))))
  
  
  
;;; -------- bring possibly-obscured dialog to top
  
  (define raise-dialog 
    (let ((+documentation+ "(raise-dialog w) tries to bring 'w' to the top of the widget heirarchy"))
      (lambda (w)
	(if (and (Widget? w) 
		 (XtIsManaged w))
	    (let ((parent (XtParent w)))
	      (if (and (Widget? parent)
		       (XtIsSubclass parent xmDialogShellWidgetClass))
		  (XtPopup parent XtGrabNone)))))))
  
  
  
;;; -------- make-pixmap --------
  
  (define arrow-strs (list
		      "16 12 6 1"
		      " 	c None s None"
		      ".	c gray50"
		      "X	c black"
		      "o	c white"
		      "O	c yellow"
		      "-      c ivory2 s basiccolor"
		      "--------X---------"
		      "---------X--------"
		      "----------X-------"
		      "-----------X------"
		      "------------X-----"
		      "XXXXXXXXXXXXXX----"
		      "------------X-----"
		      "-----------X------"
		      "----------X-------"
		      "---------X--------"
		      "--------X---------"
		      "-------X----------"))
  
  (define make-pixmap 
    (let ((+documentation+ "(make-pixmap w strs) creates a pixmap using the X/Xpm string-based pixmap description"))
      (lambda (widget strs) ; strs is list of strings as in arrow-strs above
	(and (defined? 'XpmAttributes)
	     (let ((attr (XpmAttributes))
		   (dpy (XtDisplay widget)))
	       (set! (.depth attr) (cadr (XtGetValues widget (list XmNdepth 0))))
	       (set! (.colormap attr) (cadr (XtGetValues widget (list XmNcolormap 0))))
	       (set! (.visual attr) (DefaultVisual dpy (DefaultScreen dpy)))
	       (set! (.colorsymbols attr) (list (XpmColorSymbol "basiccolor" #f *basic-color*)))
	       (set! (.numsymbols attr) 1)
	       (set! (.valuemask attr) (logior XpmColorSymbols XpmDepth XpmColormap XpmVisual))
	       (cadr (XpmCreatePixmapFromData dpy (XtWindow widget) strs attr)))))))
  
  (define right-arrow (list
		       #x00 #x04 #x10 #x08 #x00 #x10 #x04 #x20 #x00 #x40 #xa5 #xbf
		       #x00 #x40 #x04 #x20 #x00 #x10 #x10 #x08 #x00 #x04 #x00 #x00))
  
  (define bitmap->pixmap 
    (let ((+documentation+ "(bitmap->pixmap widget bits width height) takes an X-style bitmap and turns it into a pixmap"))
      (lambda (widget bits width height)
	(XCreateBitmapFromData (XtDisplay widget) (XtWindow widget) bits width height))))
  
					; (XtSetValues ((sound-widgets) 8) (list XmNlabelPixmap (bitmap->pixmap ((sound-widgets) 8) iconw right-arrow 16 12)))
  
  
;;; -------- add-mark-pane --------
;;;
;;; adds a pane to each channel giving the current mark locations (sample values)
;;;   these can be edited to move the mark, or deleted to delete the mark
;;;   can't use channel-property here because the widget lists are permanent (just unmanaged)
  
  (define including-mark-pane #f) ; for prefs
  
  (define add-mark-pane
    (letrec ((find-mark-list 
	      (lambda (snd chn dats)
		(and (pair? dats)
		     (let ((cur (car dats)))
		       (if (and (equal? (car cur) snd)
				(= (cadr cur) chn))
			   (caddr cur)
			   (find-mark-list snd chn (cdr dats)))))))
    
	     (mark-list-length
	      (let* ((mark-list-lengths ())
		     (remove-mark-list 
		      (lambda (snd chn)
			(set! mark-list-lengths (remove-if 
						 (lambda (n) 
						   (and (equal? (car n) snd) 
							(= (cadr n) chn))) 
						 mark-list-lengths)))))
		(dilambda
		 (lambda (snd chn)
		   (or (find-mark-list snd chn mark-list-lengths)
		       0))
		 (lambda (snd chn len)
		   (remove-mark-list snd chn)
		   (set! mark-list-lengths (cons (list snd chn len) mark-list-lengths))))))
    
	     (mark-list
	      (let ((mark-lists ()))
		(dilambda
		 (lambda (snd chn)
		   (cond ((find-mark-list snd chn mark-lists) => caddr) (else #f)))
		 (lambda (snd chn wid)
		   (set! mark-lists (cons (list snd chn wid) mark-lists))))))
    
	     (deactivate-mark-list 
	      (lambda (snd chn)
		(let ((current-mark-list-length (mark-list-length snd chn)))
		  (if (and (> current-mark-list-length 0)
			   (Widget? (mark-list snd chn)))
		      (for-each XtUnmanageChild (cadr (XtGetValues (mark-list snd chn) (list XmNchildren 0) 1)))))))
    
	     (make-mark-list 
	      (lambda (snd chn)
		(let ((current-mark-list-length (mark-list-length snd chn)))
		  (deactivate-mark-list snd chn)
		  (if (not (Widget? (mark-list snd chn)))
		      (let* ((mark-box (add-channel-pane snd chn "mark-box" xmFormWidgetClass
							 (list XmNbackground       *basic-color*
							       XmNorientation      XmVERTICAL
							       XmNpaneMinimum      100
							       XmNbottomAttachment XmATTACH_FORM)))
			     (mark-scroller (let ((mark-label (XtCreateManagedWidget "Marks" xmLabelWidgetClass mark-box
										     (list XmNbackground       *highlight-color*
											   XmNleftAttachment   XmATTACH_FORM
											   XmNrightAttachment  XmATTACH_FORM
											   XmNalignment        XmALIGNMENT_CENTER
											   XmNtopAttachment    XmATTACH_FORM))))
					      (XtCreateManagedWidget "mark-scroller" xmScrolledWindowWidgetClass mark-box
								     (list XmNbackground       *basic-color*
									   XmNscrollingPolicy  XmAUTOMATIC
									   XmNscrollBarDisplayPolicy XmSTATIC
									   XmNleftAttachment   XmATTACH_FORM
									   XmNrightAttachment  XmATTACH_FORM
									   XmNtopAttachment    XmATTACH_WIDGET
									   XmNtopWidget        mark-label
									   XmNbottomAttachment XmATTACH_FORM))))
			     (mlist (XtCreateManagedWidget "mark-list"  xmRowColumnWidgetClass mark-scroller
							   (list XmNorientation      XmVERTICAL
								 XmNtopAttachment    XmATTACH_FORM
								 XmNbottomAttachment XmATTACH_FORM
								 XmNspacing          0))))
			(set-main-color-of-widget mark-scroller)
			(XtSetValues mark-box (list XmNpaneMinimum 1))
			(set! (mark-list snd chn) (list snd chn mlist))))
		  
		  (let ((new-marks (marks snd chn)))
		    (when (> (length new-marks) current-mark-list-length)
		      (let ((lst (mark-list snd chn)))
			(do ((i current-mark-list-length (+ i 1)))
			    ((= i (length new-marks)))
			  (let ((tf (XtCreateWidget "field" xmTextFieldWidgetClass lst
						    (list XmNbackground *basic-color*))))
			    (XtAddCallback tf XmNfocusCallback
					   (lambda (w c i)
					     (XtSetValues w (list XmNbackground (white-pixel)))))
			    (XtAddCallback tf XmNlosingFocusCallback
					   (lambda (w c i)
					     (XtSetValues w (list XmNbackground *basic-color*))))
			    (XtAddCallback tf XmNactivateCallback
					   (lambda (w c i)
					     (let ((id (integer->mark (cadr (XtGetValues w (list XmNuserData 0)))))
						   (samp (let ((txt (cadr (XtGetValues w (list XmNvalue 0)))))
							   (and (string? txt) 
								(> (length txt) 0)
								(string->number txt)))))
					       (if samp
						   (if (mark? id)
						       (set! (mark-sample id) samp))
						   (delete-mark id))
					       (XtSetValues w (list XmNbackground *basic-color*)))))))))
		    
		    (set! (mark-list-length snd chn) (length new-marks))
		    (let ((lst (mark-list snd chn)))
		      (call-with-exit
		       (lambda (quit)
			 (for-each
			  (lambda (n)
			    (if (null? new-marks) (quit #f))
			    (if (XmIsTextField n)
				(begin
				  (XtSetValues n (list XmNvalue (number->string (mark-sample (car new-marks)))
						       XmNuserData (mark->integer (car new-marks))))
				  (XtManageChild n)
				  (set! new-marks (cdr new-marks)))))
			  (cadr (XtGetValues lst (list XmNchildren 0) 1)))))))
		  #f)))
    
	     (remark (lambda (hook)
		       (make-mark-list (hook 'snd) (hook 'chn))))
    
	     (unremark (lambda (hook)
			 (do ((i 0 (+ i 1)))
			     ((= i (channels (hook 'snd))))
			   (deactivate-mark-list (hook 'snd) i))))
    
	     (open-remarks (lambda (hook)
			     (let ((snd (hook 'snd)))
			       (do ((i 0 (+ i 1)))
				   ((= i (channels snd)))
				 (hook-push (after-edit-hook snd i) 
					    (lambda (hook)
					      (if (Widget? (mark-list snd i)) 
						  (make-mark-list snd i))))
				 (hook-push (undo-hook snd i) 
					    (lambda (hook) 
					      (if (Widget? (mark-list snd i)) 
						  (make-mark-list snd i)))))))))
      (lambda ()
	(set! including-mark-pane #t)
	(hook-push mark-hook remark)
	(hook-push close-hook unremark)
	(hook-push after-open-hook open-remarks)
	(hook-push update-hook (lambda (hook) 
				 ;; update-sound (called if header is changed, for example), calls open-sound
				 ;;   which restores our channel-local mark-pane hooks, but doesn't re-activate
				 ;;   the mark pane itself. So, we return a procedure from the update-hook
				 ;;   evaluation that will recreate our pane immediately upon update completion.
				 (set! (hook 'result)
				       (lambda (updated-snd) 
					 ;; this is the procedure to be called when the update is done
					 (do ((i 0 (+ i 1)))
					     ((= i (channels updated-snd)))
					   (make-mark-list updated-snd i)))))))))
  
  
;;; -------- select-file --------
;;;
;;; (select-file func title dir filter help)
;;;   starts a Snd-like File Selection Dialog, runs func if a file is selected
;;;
;;; (add-to-menu 0 "Insert File" 
;;;   (lambda () 
;;;     (select-file 
;;;       (lambda (filename)
;;;         (insert-sound filename))
;;;       "Insert File" "." "*" "file will be inserted at cursor")))
  
  (define select-file
    
    (letrec ((file-selector-dialogs ())    ; (list (list widget inuse func title help) ...)
	     (find-free-dialog (lambda (ds)
				 (and (pair? ds)
				      (pair? (car ds))
				      (pair? (cdar ds))
				      (if (cadar ds)
					  (find-free-dialog (cdr ds))
					  (begin
					    (set! ((car ds) 1) #t)
					    (caar ds))))))
	     (find-dialog-widget (lambda (wid ds)
				   (and (pair? ds)
					(pair? (car ds))
					(if (equal? wid (caar ds))
					    (car ds)
					    (find-dialog-widget wid (cdr ds)))))))
      (lambda args
	;; (file-select func title dir filter help)
	(let* ((func (and (pair? args) (args 0)))
	       (title (if (> (length args) 1) (args 1) "select file"))
	       (dir (if (> (length args) 2) (args 2) "."))
	       (filter (if (> (length args) 3) (args 3) "*"))
	       (help (and (> (length args) 4) (args 4)))
	       (dialog (or (find-free-dialog file-selector-dialogs)
			   (let ((new-dialog (XmCreateFileSelectionDialog 
					      (cadr (main-widgets)) 
					      title
					      (list XmNbackground *basic-color*))))
			     (XtAddCallback new-dialog XmNhelpCallback
					    (lambda (w c i)
					      (let ((lst (find-dialog-widget w file-selector-dialogs)))
						(if (lst 4)
						    (help-dialog (lst 3) (lst 4))))))
			     (XtAddCallback new-dialog XmNokCallback 
					    (lambda (w c i)
					      (let ((lst (find-dialog-widget w file-selector-dialogs)))
						((lst 2) (XmStringUnparse (.value i) #f XmCHARSET_TEXT XmCHARSET_TEXT #f 0 XmOUTPUT_ALL))
						(set! (lst 1) #f)
						(XtUnmanageChild w))))
			     (XtAddCallback new-dialog XmNcancelCallback
					    (lambda (w c i)
					      (let ((lst (find-dialog-widget w file-selector-dialogs)))
						(set! (lst 1) #f)
						(XtUnmanageChild w))))
			     (set! file-selector-dialogs (cons (list new-dialog #t func title help) file-selector-dialogs))
			     (set-main-color-of-widget new-dialog)
			     (XtSetValues (XmFileSelectionBoxGetChild new-dialog XmDIALOG_DIR_LIST) 
					  (list XmNbackground (white-pixel)))
			     (XtSetValues (XmFileSelectionBoxGetChild new-dialog XmDIALOG_LIST) 
					  (list XmNbackground (white-pixel)))
			     (XtSetValues (XtNameToWidget new-dialog "Cancel")
					  (list XmNarmColor *selection-color*))
			     (XtSetValues (XtNameToWidget new-dialog "Help")
					  (list XmNarmColor *selection-color*))
			     (XtSetValues (XtNameToWidget new-dialog "OK")
					  (list XmNarmColor *selection-color*))
			     new-dialog))))
	  ((if (not help) XtUnmanageChild XtManageChild) (XmFileSelectionBoxGetChild dialog XmDIALOG_HELP_BUTTON))
	  (let ((patstr (XmStringCreateLocalized filter))
		(titlestr (XmStringCreateLocalized title)))
	    (let ((dirstr (XmStringCreateLocalized dir)))
	      (XtSetValues dialog
			   (list XmNdirectory dirstr
				 XmNpattern patstr
				 XmNdialogTitle titlestr))
	      (XmStringFree dirstr))
	    (XmStringFree patstr)
	    (XmStringFree titlestr)
	    (XtManageChild dialog))))))
  
					; (select-file (lambda (n) (snd-print n)))
  
  
  
;;; -------- snd-clock-icon --------
;;;
;;; a clock icon to replace Snd's hourglass
;;;   call from a work proc or whatever with hour going from 0 to 12 then #f
  
  (define snd-clock-icon
    (let ((shell ((main-widgets) 1)))
      (let ((dpy (XtDisplay shell))
	    (clock-pixmaps (make-vector 12))
	    (dgc (car (snd-gcs))))
	(do ((i 0 (+ i 1)))
	    ((= i 12))
	  ;; it's actually possible to simply redraw on one pixmap, but updates are unpredictable
	  (let* ((pix (XCreatePixmap dpy (XtWindow shell) 16 16 (screen-depth)))
		 (pixwin (list 'Window (cadr pix)))) ; C-style cast to Window for X graphics procedures
	    (set! (clock-pixmaps i) pix)
	    (XSetForeground dpy dgc *basic-color*)
	    (XFillRectangle dpy pixwin dgc 0 0 16 16)
	    (XSetForeground dpy dgc (white-pixel))
	    (XFillArc dpy pixwin dgc 1 1 14 14 0 23040) ; (* 64 360))
	    (XSetForeground dpy dgc (black-pixel))
	    (XDrawArc dpy pixwin dgc 1 1 14 14 0 23040) ; (* 64 360))
	    (XDrawLine dpy pixwin dgc 8 8
		       (+ 8 (round (* 7 (sin (* i (/ 3.1416 6.0))))))
		       (- 8 (round (* 7 (cos (* i (/ 3.1416 6.0)))))))))
	(XSetBackground dpy dgc *graph-color*)
	(XSetForeground dpy dgc *data-color*)
	(lambda (snd hour)
	  (if hour
	      (XtSetValues ((sound-widgets snd) 8)
			   (list XmNlabelPixmap (clock-pixmaps hour)))
	      (bomb snd #f)))))) ; using bomb to clear the icon
  
  
  
;;; -------- make-sound-box --------
;;;
;;; make-sound-box makes a container of sound file icons, each icon
;;;   containing a little sketch of the waveform, the length of the
;;;   file, and the filename.  What happens when an icon is selected
;;;   is up to caller-supplied procedure.  However, if you drag (via 
;;;   button 2) the icon to the menubar, that sound will be opened,
;;;   and if you drag it to a channel graph, it will be mixed at the
;;;   drag point in that channel.
  
  
  (define thumbnail-graph 
    (let ((+documentation+ "(thumbnail-graph dpy wn gc pts width height) makes a little graph of the data"))
      (lambda (dpy wn gc pts width height)
	(let ((top-margin 2)
	      (bottom-margin 6))
	  (let ((y->grfy (lambda (y height)
			   (min (- height bottom-margin)
				(max top-margin
				     (round (+ top-margin
					       (* height (- 1.0 y))))))))
		(range (/ (- height top-margin bottom-margin) 2))
		(left-margin 2)
		(len (length pts)))
	    (let ((ly (y->grfy (pts 0) range))
		  (lx left-margin)
		  (xinc (/ (- width left-margin 2) len)) ; 2=right-margin
		  (y 0))
	      (do ((i 1 (+ i 1))
		   (x lx (+ x xinc)))
		  ((= i len))
		(set! y (y->grfy (pts i) range))
		(XDrawLine dpy wn gc lx ly (round x) y)
		(set! lx (round x))
		(set! ly y))))))))
  
  (define make-sound-box 
    ;; graphics stuff (fonts etc)
    (let*  ((gv (XGCValues))
	    (shell ((main-widgets) 1))
	    (button-fontstruct (XLoadQueryFont (XtDisplay shell) 
					       (if (> (length *listener-font*) 0) 
						   *listener-font* 
						   "9x15"))))
      (set! (.foreground gv) *data-color*)
      (set! (.background gv) *basic-color*)
      (if (and button-fontstruct (.fid button-fontstruct))
	  (set! (.font gv) (.fid button-fontstruct)))
      (let ((gc (XCreateGC (XtDisplay shell) 
			   (XtWindow shell) 
			   (logior GCForeground GCBackground GCFont) gv))
	    (sound-buttons ())
	    ;; button data list handlers
	    (sound-button-gc (dilambda
			      (lambda (data) (data 0))
			      (lambda (data val) (set! (data 0) val))))
	    (sound-button-filename (dilambda
				    (lambda (data) (data 1))
				    (lambda (data val) (set! (data 1) val))))
	    (sound-button (dilambda
			   (lambda (data) (data 2))
			   (lambda (data val) (set! (data 2) val))))
	    (sound-button-peaks (dilambda
				 (lambda (data) (data 3))
				 (lambda (data val) (set! (data 3) val)))))

	(define (make-sound-button-pixmap dpy wn data width height)
	  (if (pair? (sound-button-peaks data))
	      (let ((mins (car (sound-button-peaks data)))
		    (maxes (cadr (sound-button-peaks data)))
		    (gc (sound-button-gc data))
		    ;;(name (sound-button-filename data))
		    )	     
		(let* ((secs (format #f "~,1F" (mus-sound-duration (sound-button-filename data))))
		       (size (XTextWidth button-fontstruct secs (length secs))))
		  (if (<= size width) 
		      (XDrawString dpy wn gc (- width size) height secs (length secs))))
		(thumbnail-graph dpy wn gc mins width height)
		(thumbnail-graph dpy wn gc maxes width height))))
	
	(define make-sound-icon
	  (let ((cast-to-window (lambda (n) (list 'Window (cadr n)))))
	    (lambda (filename parent peak-func gc width height args)
	      (let* ((dpy (XtDisplay parent))
		     (pix (XCreatePixmap dpy (XtWindow parent) width height (screen-depth)))
		     (str (XmStringCreateLocalized filename))
		     (data (list gc filename #f (channel-amp-envs filename 0 width peak-func))))
		(XSetForeground dpy gc *basic-color*)
		(XFillRectangle dpy (cast-to-window pix) gc 0 0 width height)
		(XSetForeground dpy gc *data-color*)
		(make-sound-button-pixmap dpy (cast-to-window pix) data width height)
		(let ((icon (XtCreateManagedWidget filename xmIconGadgetClass parent
						   (append (list XmNbackground      *basic-color*
								 XmNforeground      *data-color*
								 XmNlabelString     str
								 XmNlargeIconPixmap pix
								 XmNsmallIconPixmap pix)
							   args))))
		  (set! (sound-button data) icon)
		  (set! sound-buttons (cons data sound-buttons))
		  icon)))))
	
	;; now the actual sound-box maker
	(lambda (name parent select-func peak-func snds args)
	  ;; select-func called when sound selected and passed sound file name
	  ;; peak-func (if any) tells icon where to find peak-env-info-file (if any)
	  ;; snds is list of sound file names
	  ;; args is list of resource settings for each icon
	  ;; (make-sound-box name parent select-func peak-func sounds args) makes a box of sound icons
	  (let ((container (XtCreateManagedWidget name xmContainerWidgetClass parent
						  (list XmNlayoutType         XmSPATIAL
							XmNspatialResizeModel XmGROW_BALANCED
							XmNbackground         (white-pixel)
							XmNentryViewType      XmANY_ICON
							XmNlargeCellWidth     120))))
	    (XtVaSetValues parent (list XmNworkWindow container))
	    (XtAddCallback container XmNselectionCallback 
			   (lambda (w c i)
			     (if (and (= (.auto_selection_type i) XmAUTO_BEGIN) ; just click to select for now
				      (pair? (.selected_items i)))
				 (select-func (XtName (car (.selected_items i)))))))
	    (for-each
	     (lambda (file)
	       (make-sound-icon file
				container
				peak-func
				gc
				96 64
				args))
	     snds)
	    container)))))
  
  (define show-sounds-in-directory 
    (let ((+documentation+ "(show-sounds-in-directory (dir \".\")) calls make-sound-box with the given directory"))
      (lambda* ((dir "."))
	(make-sound-box
	 "sounds"
	 (XtCreateManagedWidget "scrolled-window" xmScrolledWindowWidgetClass ((main-widgets) 3)
				(list XmNscrollBarDisplayPolicy XmAS_NEEDED
				      XmNbackground             *basic-color*
				      XmNvisualPolicy           XmVARIABLE
				      XmNscrollingPolicy        XmAUTOMATIC))
	 open-sound
	 (lambda (file chn)
	   (format #f "~~/peaks/~A-peaks-~D"                              
		   (xm-clean-string (file-name file))
		   chn))
	 (sound-files-in-directory dir)
	 ()))))
  
  
  
#|
;;; -------- show-smpte-label
;;;
;;; (show-smpte-label on-or-off)
;;;   turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample
;;; this is now built-in under with-smpte-label
  
  (define smpte-frames-per-second 24.0)
  
  (define draw-smpte-label
    (let* ((dpy (XtDisplay (cadr (main-widgets))))
	   (fs (XLoadQueryFont dpy *axis-numbers-font*))
	   (width (+ 8 (XTextWidth fs "00:00:00:00" 11)))
	   (height (+ 8 (caddr (XTextExtents fs "0" 1)))))
      
      (define (smpte-label samp sr)
	(define (round-down val) (truncate val))
	(let* ((seconds (/ samp sr))
	       (len (* seconds smpte-frames-per-second))
	       (minutes (round-down (/ seconds 60)))
	       (hours (round-down (/ minutes 60))))
	  (format #f "~2,'0D:~2,'0D:~2,'0D:~2,'0D"
		  hours
		  (- minutes (* hours 60))
		  (round-down (- seconds (* minutes 60)))
		  (round-down (- len (* (round-down seconds) smpte-frames-per-second))))))
      
      (let ((+documentation+ "(draw-smpte-label snd chn) draws a SMPTE time stamp in a box on a graph"))    
	(lambda (hook)
	  (let ((snd (hook 'snd))
		(chn (hook 'chn)))
	    (let* ((axinf (axis-info snd chn))
		   (x (axinf 10))
		   (y (axinf 13))
		   (grf-width (- (axinf 12) x))
		   (grf-height (- (axinf 11) y)))
	      (if (and (> grf-height (* 2 height))
		       (> grf-width (* 1.5 width))
		       (time-graph? snd chn))
		  (let* ((smpte (smpte-label (car axinf) (srate snd)))
			 (samp (car axinf)))
		    (fill-rectangle x y width 2 snd chn)
		    (fill-rectangle x (+ y height) width 2 snd chn)
		    (fill-rectangle x y 2 height snd chn)
		    (fill-rectangle (+ x width -2) y 2 height snd chn)
		    (if (and fs (.fid fs))
			(XSetFont dpy
				  (if (= chn (selected-channel snd))
				      (cadr (snd-gcs))
				      (car (snd-gcs)))
				  (.fid fs)))
		    (draw-string smpte (+ x 4) (+ y 4) snd chn)))))))))
  
  (define show-smpte-label
    (let ((+documentation+ "(show-smpte-label on-or-off) turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample"))
      (lambda arg
	(if (or (null? arg)
		(car arg))
	    (if (not (member draw-smpte-label (hook-functions after-graph-hook)))
		(begin
		  (hook-push after-graph-hook draw-smpte-label)
		  (update-time-graph #t #t)))
	    (begin
	      (hook-remove after-graph-hook draw-smpte-label)
	      (update-time-graph #t #t))))))
  
  (define smpte-is-on ; for prefs dialog
    (let ((+documentation+ "(smpte-is-on) is #t if we are drawing SMPTE time stamps"))
      (lambda ()
	(member draw-smpte-label (hook-functions after-graph-hook)))))
|#
  
  
  (define red-pixel
    (let ((pix #f)
	  (+documentation+ "(red-pixel) returns a red pixel"))
      (lambda ()
	(if (not pix)
	    (let* ((dpy (XtDisplay (cadr (main-widgets))))
		   (cmap (DefaultColormap dpy (DefaultScreen dpy)))
		   (col (XColor)))
	      (if (= (XAllocNamedColor dpy cmap "red" col col) 0)
		  (snd-error "can't allocate red!")
		  (set! pix (.pixel col)))))
	pix)))
  
#|
;;; -------- with-level-meters, make-level-meter, display-level
  
  (define make-level-meter 
    (let ((+documentation+ "(make-level-meter parent width height args (resizable #t)) makes a VU level meter"))
      (lambda* (parent width height args (resizable #t))
	(let* ((frame (XtCreateManagedWidget "meter-frame" xmFrameWidgetClass parent
					     (append (list XmNshadowType       XmSHADOW_ETCHED_IN
							   XmNwidth            width
							   XmNheight           height
							   XmNshadowThickness  (if (> width 500) 6 3))
						     args)))
	       (meter (XtCreateManagedWidget "meter" xmDrawingAreaWidgetClass frame
					     (if resizable				       
						 (list XmNbackground       (white-pixel)
						       XmNforeground       (black-pixel)
						       XmNtopAttachment    XmATTACH_FORM
						       XmNbottomAttachment XmATTACH_FORM
						       XmNleftAttachment   XmATTACH_FORM
						       XmNrightAttachment  XmATTACH_FORM)
						 (list XmNbackground       (white-pixel)
						       XmNforeground       (black-pixel)
						       XmNwidth            width
						       XmNheight           height
						       XmNresizePolicy     XmRESIZE_NONE
						       XmNtopAttachment    XmATTACH_FORM
						       XmNbottomAttachment XmATTACH_FORM
						       XmNleftAttachment   XmATTACH_FORM
						       XmNrightAttachment  XmATTACH_FORM))))
	       (context (list meter 0.0 1.0 0.0 0.0 width height)))
	  (XtAddCallback meter XmNexposeCallback 
			 (lambda (w c i) 
			   (display-level c)) 
			 context)
	  (if resizable
	      (XtAddCallback meter XmNresizeCallback 
			     (lambda (w c i) 
			       (set! (c 5) (cadr (XtGetValues w (list XmNwidth 0))))
			       (set! (c 6) (cadr (XtGetValues w (list XmNheight 0))))
			       (display-level c))
			     context))
	  context))))
  
  (define display-level 
    (let ((+documentation+ "(display-level meter-data) displays a VU level meter"))
      (lambda (meter-data)
	(let* ((meter (car meter-data))
	       (level (meter-data 1))
	       (last-level (meter-data 3))
	       (red-deg (meter-data 4))
	       (width (meter-data 5))
	       (height (meter-data 6))
	       ;; (size (meter-data 2))
	       (dpy (XtDisplay meter))
	       (win (XtWindow meter))
	       (major-tick (round (/ width 24)))
	       (minor-tick (round (* major-tick .6)))
	       (ang0 (* 45 64))
	       (ang1 (* 90 64))
	       (wid2 (floor (/ width 2)))
	       (gc (car (snd-gcs)))
	       (top (round (/ height 3.2)))) ; distance of label from top of meter
	  (if (and (> top 10)
		   (> width 10)
		   (> height 10))
	      (begin
		(XSetForeground dpy gc (white-pixel))
		(XFillRectangle dpy win gc 0 0 width height)
		(XSetForeground dpy gc (black-pixel))
		(XDrawArc dpy win gc 0 top width width ang0 ang1)
		(XDrawArc dpy win gc 0 (- top 1) width width ang0 ang1)
		(if (> width 100)
		    (XDrawArc dpy win gc 0 (- top 2) width width ang0 ang1))
		(XDrawArc dpy win gc 4 (+ top 4) (- width 8) (- width 8) ang0 ang1)
		(do ((i 0 (+ i 1)))
		    ((= i 5))
		  (let* ((rdeg (degrees->radians (- 45 (* i 22.5))))
			 (sinr (sin rdeg))
			 (cosr (cos rdeg))
			 (x0 (round (+ wid2 (* wid2 sinr))))
			 (y0 (round (- (+ wid2 top) (* wid2 cosr))))
			 (x1 (round (+ wid2 (* (+ wid2 major-tick) sinr))))
			 (y1 (round (- (+ wid2 top) (* (+ wid2 major-tick) cosr)))))
		    (XDrawLine dpy win gc x0 y0 x1 y1)
		    (XDrawLine dpy win gc (+ x0 1) y0 (+ x1 1) y1)
		    (if (< i 4)
			(do ((j 1 (+ 1 j)))
			    ((= j 6))
			  (let* ((rdeg (degrees->radians (- 45 (* i 22.5) (* j (/ 90.0 20.0)))))
				 (sinr (sin rdeg))
				 (cosr (cos rdeg))
				 (x0 (round (* wid2 (+ 1.0 sinr))))
				 (y0 (round (- (+ wid2 top) (* wid2 cosr))))
				 (x1 (round (+ wid2 (* (+ wid2 minor-tick) sinr))))
				 (y1 (round (- (+ wid2 top) (* (+ wid2 minor-tick) cosr)))))
			    (XDrawLine dpy win gc x0 y0 x1 y1))))))
		(let* ((needle-speed 0.25)
		       (bubble-speed 0.025)
		       (bubble-size (* 15 64))
		       (val (+ (* level needle-speed) (* last-level (- 1.0 needle-speed))))
		       (deg (- (* val 90.0) 45.0))
		       (rdeg (degrees->radians deg))
		       (nx1 (round (+ wid2 (* (+ wid2 major-tick) (sin rdeg)))))
		       (ny1 (round (- (+ wid2 top) (* (+ wid2 major-tick) (cos rdeg))))))
		  (XDrawLine dpy win gc wid2 (+ top wid2) nx1 ny1)
		  (set! (meter-data 3) val)
		  (if (> val red-deg)
		      (set! (meter-data 4) val)
		      (set! (meter-data 4) (+ (* val bubble-speed) (* red-deg (- 1.0 bubble-speed)))))
		  (if (> (meter-data 4) .01)
		      (begin
			(XSetForeground dpy gc (red-pixel))
			(let* ((redx (floor (* (meter-data 4) 90 64)))
			       (redy (min redx bubble-size)))
			  (do ((i 0 (+ i 1)))
			      ((= i 4))
			    (XDrawArc dpy win gc i (+ top i) (- width (* i 2)) (- width (* i 2)) (- (* 135 64) redx) redy))
			  (XSetForeground dpy gc (black-pixel))))))))))))
  
  (define with-level-meters 
    (let ((+documentation+ "(with-level-meters n) adds 'n' level meters to a pane at the top of the Snd window"))
      (lambda (n)
	(let* ((parent ((main-widgets) 3))
	       (height 70)
	       (width (floor (/ (cadr (XtGetValues parent (list XmNwidth 0))) n)))
	       (meters (XtCreateManagedWidget "meters" xmFormWidgetClass parent
					      (list XmNpositionIndex 0  ; top pane
						    XmNbackground    *basic-color*
						    XmNfractionBase  (* n 10)
						    XmNpaneMinimum   height)))
	       (meter-list ()))
	  (do ((i 0 (+ i 1)))
	      ((= i n))
	    (set! meter-list 
		  (cons (make-level-meter meters width height
					  (list XmNtopAttachment    XmATTACH_FORM
						XmNbottomAttachment XmATTACH_FORM
						XmNleftAttachment   XmATTACH_POSITION
						XmNleftPosition     (* i 10)
						XmNrightAttachment  XmATTACH_POSITION
						XmNrightPosition    (* (+ 1 i) 10))) 
			meter-list)))
	  (hook-push dac-hook 
		     (lambda (hook)
		       (let ((maxes (map float-vector-peak (hook 'data))))
			 (for-each
			  (lambda (meter)
			    (if (null? maxes)
				(set! (meter 1) 0.0)
				(begin
				  (set! (meter 1) (car maxes))
				  (display-level meter)
				  (set! maxes (cdr maxes)))))
			  (reverse meter-list)))))
	  (hook-push stop-dac-hook
		     (lambda (hook) ; drain away the bubble
		       (XtAppAddWorkProc (car (main-widgets))
					 (let ((ctr 0))
					   (lambda (ignored)
					     (for-each 
					      (lambda (meter)
						(set! (meter 1) 0.0)
						(display-level meter))
					      meter-list)
					     (set! ctr (+ ctr 1))
					     (> ctr 200))))))
	  (XtSetValues meters (list XmNpaneMinimum 1))
	  meter-list))))
|#
  
  
;;; -------- add a drop site
;;;
;;; this adds a pane to the current channel which can respond to drag-and-drop operations
;;;   (this is a Motif 1.2 style drop -- I've had trouble getting the new style to work at all)
  
  (define make-channel-drop-site
    (let ((+documentation+ "(make-channel-drop-site snd) adds a drop site pane to the current channel"))
      (lambda args
	(let ((widget (let ((snd (if (pair? args) (car args) (selected-sound))))
			(add-channel-pane snd (selected-channel snd)
					  "drop here" xmDrawingAreaWidgetClass
					  (list XmNbackground (white-pixel)
						XmNleftAttachment      XmATTACH_FORM
						XmNrightAttachment     XmATTACH_FORM
						XmNtopAttachment       XmATTACH_FORM
						XmNbottomAttachment    XmATTACH_FORM)))))
	  (XmDropSiteRegister
	   widget 
	   (list XmNdropSiteOperations XmDROP_COPY
		 XmNimportTargets      (list XA_STRING) ; list of Atoms we can deal with -- in this case, just strings
		 XmNnumImportTargets   1
		 XmNdropProc 
		 (lambda (w c i)
		   ;; i is the callback data (XmDropProcCallbackStruct), c is always #f
		   (if (not (and (= (.dropAction i) XmDROP)
				 (= (.operation i) XmDROP_COPY)))
		       (set! (.dropSiteStatus i) XmINVALID_DROP_SITE)
		       (begin
			 (set! (.operation i) XmDROP_COPY) ; tell system drop has succeeded
			 (XmDropTransferStart 
			  (.dragContext i)
			  (list XmNdropTransfers (list XA_STRING)
				XmNnumDropTransfers 1
				XmNtransferProc 
				(lambda (w context selection type val len fmt)
				  ;; the actual in-coming string (properly terminated in xm.c) is 'value'
				  (snd-print (format #f "got: ~A ~A ~A ~A ~A ~A ~A~%"
						     w context selection type val len fmt))))))))))))))
  
  
;;; -------- change a drop callback
;;;
;;; drop arg is 3-arg func: filename snd chn
  
  (define set-channel-drop 
    (let ((+documentation+ "(set-channel-drop drop snd chn) changes a drop callback function; 'drop' is function of 3 args (filename snd chn)"))
      (lambda (drop snd chn)
	(XmDropSiteUpdate
	 (car (channel-widgets snd chn))
	 (list XmNdropProc
	       (lambda (w c i)
		 (if (not (and (= (.dropAction i) XmDROP)
			       (= (.operation i) XmDROP_COPY)))
		     (set! (.dropSiteStatus i) XmINVALID_DROP_SITE)
		     (begin
		       (set! (.operation i) XmDROP_COPY)
		       (XmDropTransferStart 
			(.dragContext i)
			(list XmNdropTransfers (list (XInternAtom (XtDisplay (cadr (main-widgets))) "FILE_NAME" #f))
			      
			      ;; this is saying that the in-coming drag-and-drop is expected to pass us a FILE_NAME atom
			      ;; to find out what Atoms the selection translator can handle, use
			      ;;   (XtVaGetValues (.dragContext i) (list XmNexportTargets 0))
			      ;; which will return a list of acceptable Atoms
			      
			      XmNnumDropTransfers 1
			      XmNtransferProc 
			      (lambda (w context selection type val len fmt)
				(drop val snd chn))))))))))))
  
  
;;; -------- show-disk-space
;;;
;;; adds a label to the status-area area showing the current free space 
  
  (define showing-disk-space #f) ; for prefs dialog
  
  (define show-disk-space
    (let ((+documentation+ "(show-disk-space snd) adds a label to snd's status-area area showing the current free space (for use with after-open-hook: (set! (hook-functions after-open-hook) (list (*motif* 'show-disk-space)))")
	  (labelled-snds ())

	  (kmg (lambda (num)
		 (cond ((<= num 0)      (copy "disk full!"))
		       ((<= num 1024)   (format #f "space: ~10DK" num))
		       ((> num 1048576) (format #f "space: ~6,3FG" (/ num (* 1024.0 1024.0))))
		       (else            (format #f "space: ~6,3FM" (/ num 1024.0))))))
	  
	  (show-label (lambda (data id)
			(if (sound? (car data))
			    (let ((str (XmStringCreateLocalized (kmg (disk-kspace (file-name (car data)))))))
			      (XtSetValues (cadr data) (list XmNlabelString str))
			      (XmStringFree str)
			      (XtAppAddTimeOut (caddr data) 10000 show-label data))))))
      (lambda (hook)
	(let* ((snd (hook 'snd))
	       (previous-label (find-if (lambda (n) (equal? (car n) snd)) labelled-snds)))
	  (unless previous-label
	    (if (not snd)
		(snd-error "no sound found for disk space label")
		(let* ((app (car (main-widgets)))
		       (widgets (sound-widgets snd))
		       (status-area (widgets 3))
		       (unite-button (widgets 6))
		       (sync-button (widgets 9))
		       (name-form (XtParent status-area)) ; "snd-name-form"
		       (str (XmStringCreateLocalized (kmg (disk-kspace (file-name snd))))))
		  (set! showing-disk-space #t)
		  (XtUnmanageChild status-area)
		  (XtVaSetValues status-area (list XmNrightAttachment XmATTACH_NONE))
		  (let ((new-label (XtCreateManagedWidget "space:" xmLabelWidgetClass name-form 
							  (list XmNbackground      *basic-color*
								XmNleftAttachment  XmATTACH_NONE
								XmNlabelString     str
								XmNrightAttachment XmATTACH_WIDGET
								XmNrightWidget     (if (XtIsManaged unite-button)
										       unite-button
										       sync-button)
								XmNtopAttachment   XmATTACH_FORM))))
		    (XtVaSetValues status-area (list XmNrightWidget new-label XmNrightAttachment XmATTACH_WIDGET))
		    (XtManageChild status-area)
		    (XmStringFree str)
		    (set! previous-label (list snd new-label app))
		    (set! labelled-snds (cons previous-label labelled-snds))
		    (XtAppAddTimeOut (caddr previous-label) 10000 show-label previous-label)))))))))
  
  
  
;;; -------- add amp sliders in control panel for multi-channel sounds
;;;
;;; use control-button to move all at once
;;;
;;; the max scrollbar value can change (it's now 10000), so ideally this code should notice it
  
  (define add-amp-controls
    (let ((+documentation+ "(add-amp-controls) adds amplitude sliders to the control panel for each channel in multi-channel sounds")
	  (label-name (lambda (chan) (if (= chan 0) (copy "amp-label") (format #f "amp-label-~D" chan))))
	  (number-name (lambda (chan) (if (= chan 0) (copy "amp-number") (format #f "amp-number-~D" chan))))
	  (scroller-name (lambda (chan) (if (= chan 0) (copy "amp") (format #f "amp-~D" chan)))))

      (lambda ()
	
	(define amp-callback 
	  ;; c is (list number-widget snd chan)
	  (let ((scroll->amp 
		 (lambda (snd val)
		   (cond ((<= val 0)    (car (amp-control-bounds snd)))
			 ((>= val 9000) (cadr (amp-control-bounds snd)))
			 ((> val 4500)  (+ (* (- (/ val 4500.0) 1.0) (- (cadr (amp-control-bounds snd)) 1.0)) 1.0))
			 (else          (+ (* val (/ (- 1.0 (car (amp-control-bounds snd))) 4500.0)) (car (amp-control-bounds snd))))))))
	    (lambda (w c info)
	      (let* ((snd (cadr c))
		     (amp (scroll->amp snd (.value info)))
		     (ampstr (XmStringCreateLocalized (format #f "~,3F " amp)))
		     (chn (- (channels snd) 1 (caddr c)))
		     (ctrl (and (.event info) (not (= (logand (.state (.event info)) ControlMask) 0)))))
		(XtSetValues (car c) (list XmNlabelString ampstr))
		(XmStringFree ampstr)
		(if ctrl
		    (let ((snd-amp (find-child ((sound-widgets snd) 2) "snd-amp"))
			  (chns (channels snd)))
		      (do ((i 0 (+ i 1)))
			  ((= i chns))
			(let* ((ampscr (find-child snd-amp (scroller-name i)))
			       (ampvals (cdr (XmScrollBarGetValues ampscr))))
			  (XmScrollBarSetValues ampscr (.value info) (car ampvals) (cadr ampvals) (caddr ampvals) #t)
			  (set! (amp-control snd i) amp))))
		    (set! (amp-control snd chn) amp))))))
	    
	(define (reset-to-one scroller number)
	  (XtSetValues scroller (list XmNvalue 4500))
	  (let ((ampstr (XmStringCreateLocalized "1.000 ")))
	    (XtSetValues number (list XmNlabelString ampstr))
	    (XmStringFree ampstr)))
	
	(define amp-controls-reflect-chans 
	  
	  (let ((make-amp-control 
		 (lambda (snd chan parent)
		   (let* ((s1 (XmStringCreateLocalized "amp:"))
			  (label (XtCreateManagedWidget (label-name chan) xmPushButtonWidgetClass parent
							(list XmNbackground       *basic-color*
							      XmNalignment        XmALIGNMENT_BEGINNING
							      XmNtopAttachment    XmATTACH_FORM
							      XmNbottomAttachment XmATTACH_NONE
							      XmNleftAttachment   XmATTACH_FORM
							      XmNrightAttachment  XmATTACH_NONE
							      XmNlabelString      s1
							      XmNmarginHeight     1
							      XmNrecomputeSize    #f
							      XmNshadowThickness  0
							      XmNhighlightThickness 0
							      XmNfillOnArm        #f)))
			  (s2 (XmStringCreateLocalized "1.000 ")))
		     (let* ((number (XtCreateManagedWidget (number-name chan) xmLabelWidgetClass parent
							   (list XmNbackground       *basic-color*
								 XmNalignment        XmALIGNMENT_BEGINNING
								 XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
								 XmNtopWidget        label
								 XmNbottomAttachment XmATTACH_NONE
								 XmNleftAttachment   XmATTACH_WIDGET
								 XmNleftWidget       label
								 XmNrightAttachment  XmATTACH_NONE
								 XmNlabelString      s2
								 XmNmarginHeight     1
								 XmNmarginRight      3
								 XmNrecomputeSize    #f)))
			    (scroll (XtCreateManagedWidget (scroller-name chan) xmScrollBarWidgetClass parent
							   (list XmNbackground       *position-color*
								 XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
								 XmNtopWidget        label
								 XmNbottomAttachment XmATTACH_NONE
								 XmNheight           16
								 XmNleftAttachment   XmATTACH_WIDGET
								 XmNleftWidget       number
								 XmNrightAttachment  XmATTACH_FORM
								 XmNorientation      XmHORIZONTAL
								 XmNmaximum          10000
								 XmNvalue            4500
								 XmNdragCallback     (list amp-callback (list number snd chan))
								 XmNvalueChangedCallback (list amp-callback (list number snd chan))))))
		       (XtOverrideTranslations scroll
					       (XtParseTranslationTable "c<Btn1Down>: Select()
                                                        c<Btn1Motion>: Moved()
						        c<Btn1Up>:   Release()"))
		       
		       (XtAddCallback label XmNactivateCallback (lambda (w c i)
								  (reset-to-one scroll number))))
		     (XmStringFree s1)
		     (XmStringFree s2)
		     label))))
	    
	    (lambda (snd)	      
	      (let* ((ctrls ((sound-widgets snd) 2))
		     (snd-amp (find-child ctrls "snd-amp"))
		     (chns (channels snd)))
		
		(when (Widget? snd-amp)
		  (let ((height (cadr (XtGetValues ctrls (list XmNheight 0))))
			(panemin (cadr (XtGetValues ctrls (list XmNpaneMinimum 0))))
			(panemax (cadr (XtGetValues ctrls (list XmNpaneMaximum 0)))))
		    (XtUnmanageChild ctrls)
		    
		    (if (not (sound-property 'amp-controls snd))
			(let ((orig-amp (find-child snd-amp "amp")))
			  (XtOverrideTranslations orig-amp
						  (XtParseTranslationTable "c<Btn1Down>: Select()
                                                                    c<Btn1Motion>: Moved()
						                    c<Btn1Up>:   Release()"))
			  (XtAddCallback orig-amp XmNdragCallback
					 (lambda (w c info)
					   (if (and (.event info) (not (= (logand (.state (.event info)) ControlMask) 0)))
					       (do ((i 1 (+ i 1)))
						   ((= i chns))
						 (let* ((ampscr (find-child snd-amp (scroller-name i)))
							(ampvals (cdr (XmScrollBarGetValues ampscr))))
						   (XmScrollBarSetValues ampscr (.value info) (car ampvals) (cadr ampvals) (caddr ampvals) #t))))))))
		    (let ((existing-controls (or (sound-property 'amp-controls snd) 1)))
		      (if (< existing-controls chns)
			  (begin
			    (if (> height 20)
				(set! height (+ height (* 18 (- chns existing-controls)))))
			    (do ((i existing-controls (+ i 1)))
				((= i chns))
			      (make-amp-control snd i snd-amp))
			    (set! (sound-property 'amp-controls snd) chns)
			    (set! existing-controls chns)))
		      (do ((i 0 (+ i 1)))
			  ((= i existing-controls))
			(let ((ampn (find-child snd-amp (number-name i)))
			      (amp (find-child snd-amp (scroller-name i))))
			  (XtUnmanageChild (find-child snd-amp (label-name i)))
			  (XtUnmanageChild ampn)
			  (XtUnmanageChild amp)))
		      (do ((i 0 (+ i 1)))
			  ((= i chns))
			(let ((ampc (find-child snd-amp (label-name i)))
			      (ampn (find-child snd-amp (number-name i)))
			      (amp (find-child snd-amp (scroller-name i))))
			  (let ((next-amp (and (< i (- chns 1))
					       (find-child snd-amp (label-name (+ i 1))))))
			    (reset-to-one amp ampn)
			    (XtSetValues ampc (list XmNtopAttachment 
						    (if next-amp (values XmATTACH_WIDGET XmNtopWidget next-amp) XmATTACH_FORM))))
			  (XtManageChild ampc)
			  (XtManageChild ampn)
			  (XtManageChild amp))))
		    
		    (XtSetValues ctrls (list XmNpaneMinimum height XmNpaneMaximum height))
		    (XtManageChild ctrls)
		    (XtSetValues ctrls (list XmNpaneMinimum panemin XmNpaneMaximum panemax))))))))
	    
	(define (amp-controls-clear snd)
	  (if (> (channels snd) 1)
	      (let ((snd-amp (find-child ((sound-widgets snd) 2) "snd-amp"))
		    (top (- (channels snd) 1)))
		(do ((i 1 (+ i 1)))
		    ((= i (channels snd)))
		  (let ((ampn (find-child snd-amp (number-name i)))
			(amp (find-child snd-amp (scroller-name i))))
		    (reset-to-one amp ampn)
		    (set! (amp-control snd (- top i)) 1.0))))))
	
	(hook-push after-open-hook (lambda (hook) (amp-controls-reflect-chans (hook 'snd))))
	(hook-push after-apply-controls-hook (lambda (hook) (amp-controls-clear (hook 'snd)))))))
  
  
  
;;; -------- remove top level menu
;;;
;;; (remove-main-menu 5) removes the Help menu
  
  (define remove-main-menu 
    (let ((+documentation+ "(remove-main-menu menu) removes the specified top-level menu: (remove-main-menu 5) removes the Help menu"))
      (lambda (menu)
	(let* ((cascade ((menu-widgets) menu))
	       (top (cadr (XtGetValues cascade (list XmNsubMenuId 0)))))
	  (XtUnmanageChild cascade)
	  (XtUnmanageChild top)))))
  
  
;;; -------- add delete and rename options to the file menu
  
  (define add-delete-option
    (let ((+documentation+ "(add-delete-option) adds a delete (file) option to the File menu"))
      (lambda ()
	(add-to-menu 0 "Delete" ; add Delete option to File menu
		     (lambda ()
		       ;; close current sound and delete it
		       (if (selected-sound)
			   (let ((filename (file-name)))
			     (close-sound)
			     (delete-file filename))))
		     8)))) ; place after File:New
  
  (define add-rename-option
    (let ((+documentation+ "(add-rename-option) adds a rename (file) option to the File menu"))
      (lambda ()
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
					      (help-dialog "Rename" "give a new file name to rename the currently selected sound")))
			     
			     (XtAddCallback new-dialog XmNokCallback 
					    (lambda (w c i)
					      (let ((new-name (XmTextFieldGetString rename-text)))
						(when (and (string? new-name)
							   (> (length new-name) 0)
							   (selected-sound))
						  (save-sound-as new-name)
						  (close-sound)
						  (open-sound new-name)
						  (XtUnmanageChild w)))))
			     (for-each XmStringFree (vector xhelp xok xdismiss titlestr))
			     (set! rename-dialog new-dialog)
			     
			     (let* ((mainform (XtCreateManagedWidget "formd" xmRowColumnWidgetClass rename-dialog
								     (list XmNleftAttachment   XmATTACH_FORM
									   XmNrightAttachment  XmATTACH_FORM
									   XmNtopAttachment    XmATTACH_FORM
									   XmNbottomAttachment XmATTACH_WIDGET
									   XmNbottomWidget     (XmMessageBoxGetChild rename-dialog XmDIALOG_SEPARATOR)
									   XmNorientation      XmVERTICAL
									   XmNbackground       *basic-color*)))
				    (label (XtCreateManagedWidget "new name:" xmLabelWidgetClass mainform
								  (list XmNleftAttachment   XmATTACH_FORM
									XmNrightAttachment  XmATTACH_NONE
									XmNtopAttachment    XmATTACH_FORM
									XmNbottomAttachment XmATTACH_FORM
									XmNbackground       *basic-color*))))
			       (set! rename-text 
				     (XtCreateManagedWidget "newname" xmTextFieldWidgetClass mainform
							    (list XmNleftAttachment   XmATTACH_WIDGET
								  XmNleftWidget       label
								  XmNrightAttachment  XmATTACH_FORM
								  XmNtopAttachment    XmATTACH_FORM
								  XmNbottomAttachment XmATTACH_FORM
								  XmNbackground       *basic-color*)))
			       (XtAddEventHandler rename-text EnterWindowMask #f
						  (lambda (w context ev flag)
						    (XmProcessTraversal w XmTRAVERSE_CURRENT)
						    (XtSetValues w (list XmNbackground (white-pixel)))))
			       (XtAddEventHandler rename-text LeaveWindowMask #f
						  (lambda (w context ev flag)
						    (XtSetValues w (list XmNbackground *basic-color*)))))))
			 ((if (not (XtIsManaged rename-dialog)) XtManageChild raise-dialog) rename-dialog))
		       8)))))
  
  
  (define change-label 
    (let ((+documentation+ "(change-label widget new-label) changes widget's label to new-label"))
      (lambda (widget new-label)
	(let ((str (XmStringCreateLocalized new-label)))
	  (XtSetValues widget (list XmNlabelString str))
	  (XmStringFree str)))))
  
;;; this deletes the play button
					;(hook-push after-open-hook
					;	   (lambda (hook)
					;	     (let* ((ctrls ((sound-widgets (hook 'snd)) 2))
					;		    (play-button (find-child ctrls "play"))
					;		    (sync-button (find-child ctrls "sync")))
					;	     (XtUnmanageChild play-button)
					;	     (XtVaSetValues sync-button (list XmNrightAttachment XmATTACH_FORM)))))
  
  
;;; -------- mark-sync-color
;;;
;;; (mark-sync-color "blue")
  
  (define mark-sync-color 
    (let ((+documentation+ "(mark-sync-color new-color) sets the color for sync'd marks")
	  (get-color (lambda (color-name)
		       (let* ((col (XColor))
			      (dpy (XtDisplay (cadr (main-widgets))))
			      (cmap (DefaultColormap dpy (DefaultScreen dpy))))
			 (if (= (XAllocNamedColor dpy cmap color-name col col) 0)
			     (snd-error (format #f "can't allocate ~A" color-name))
			     (.pixel col))))))
      (lambda (new-color)
	(let ((mark-gc ((snd-gcs) 9))
	      (selected-mark-gc ((snd-gcs) 10))
	      (dpy (XtDisplay (cadr (main-widgets))))
	      (original-mark-color (list 'Pixel (logxor (cadr *mark-color*) 
							(cadr *graph-color*))))
	      (original-selected-mark-color (list 'Pixel (logxor (cadr *mark-color*) 
								 (cadr *selected-graph-color*))))
	      (new-mark-color (list 'Pixel (logxor (cadr *graph-color*) 
						   (cadr (get-color new-color)))))
	      (new-selected-mark-color (list 'Pixel (logxor (cadr *selected-graph-color*)
							    (cadr (get-color new-color))))))
	  (if (pair? (hook-functions draw-mark-hook))
	      (set! (hook-functions draw-mark-hook) ()))
	  (hook-push draw-mark-hook
		     (lambda (hook)
		       (if (> (sync (hook 'id)) 0)
			   (begin
			     (XSetForeground dpy mark-gc new-mark-color)
			     (XSetForeground dpy selected-mark-gc new-selected-mark-color))
			   (begin
			     (XSetForeground dpy mark-gc original-mark-color)
			     (XSetForeground dpy selected-mark-gc original-selected-mark-color)))
		       #f))))))
  
  
  
;;; -------- add "tooltip" to a widget
;;;
;;; (add-tooltip (cadr (channel-widgets)) "the w button")
  
  (define tooltip-shell #f)
  (define tooltip-label #f)
  
  (define add-tooltip 
    (let ((+documentation+ "(add-tooltip widget tip) adds the tooltip 'tip' to the widget"))
      (lambda (widget tip)
	(let ((tool-proc #f)
	      (quit-proc #f)
	      
	      (last-time 0))  ; try to squelch "fluttering"
	  
	  (define (stop-tooltip)
	    (if tool-proc
		(begin
		  (XtRemoveTimeOut tool-proc)
		  (set! tool-proc #f)))
	    (if quit-proc
		(begin
		  (XtRemoveTimeOut quit-proc)
		  (set! quit-proc #f)))
	    (if (and tooltip-shell (XtIsManaged tooltip-shell))
		(XtUnmanageChild tooltip-shell)))
	  
	  (let ((start-tooltip 
		 (let ((quittime 3000) ; millisecs to show tip (if pointer not already moved out of widget)
		       (timeout 500))   ; millisecs after mouse enters widget to tip display 
		   (lambda (ev)
		     (if (and *with-tooltips*
			      (not tool-proc))
			 (set! tool-proc (XtAppAddTimeOut 
					  (car (main-widgets))
					  timeout 
					  (lambda (data id)
					    (if tooltip-shell
						(change-label tooltip-label tip)
						(begin
						  (set! tooltip-shell (XtCreatePopupShell 
								       tip 
								       overrideShellWidgetClass 
								       (cadr (main-widgets)) 
								       (list XmNallowShellResize #t)))
						  (set! tooltip-label
							(XtCreateManagedWidget 
							 tip
							 xmLabelWidgetClass 
							 tooltip-shell
							 (list XmNrecomputeSize #t
							       XmNbackground *highlight-color*)))))
					    (let ((loc (XtTranslateCoords widget (.x ev) (.y ev))))
					      (XtVaSetValues tooltip-shell (list XmNx (car loc) XmNy (cadr loc))))
					    (XtManageChild tooltip-shell)
					    (set! quit-proc (XtAppAddTimeOut
							     (car (main-widgets))
							     quittime
							     (lambda (data id)
							       (XtUnmanageChild tooltip-shell)
							       (set! quit-proc #f))))))))))))
	    
	    (XtAddEventHandler widget EnterWindowMask #f 
			       (lambda (w c ev flag)
				 (if (> (- (cadr (.time ev)) last-time) 50)
				     (start-tooltip ev))
				 (set! last-time (cadr (.time ev))))))
	  (XtAddEventHandler widget LeaveWindowMask #f 
			     (lambda (w c ev flag) 
			       (set! last-time (cadr (.time ev)))
			       (stop-tooltip)))))))
  
  (define menu-option 
    (let ((+documentation+ "(menu-option name) finds the widget associated with a given menu item name"))
      (lambda (name)
	(call-with-exit
	 (lambda (return)
	   (for-each
	    (lambda (top-menu)
	      (for-each-child
	       top-menu
	       (lambda (w)
		 (let ((option-holder (cadr (XtGetValues w (list XmNsubMenuId 0)))))
		   (for-each-child
		    option-holder
		    (lambda (menu)
		      (if (string=? name (XtName menu))
			  (return menu)
			  (if (XmIsCascadeButton menu)
			      (let ((options (cadr (XtGetValues menu (list XmNsubMenuId 0)))))
				(for-each-child
				 options
				 (lambda (inner-menu)
				   (if (string=? name (XtName inner-menu))
				       (return inner-menu)))))))))))))
	    (cdr (menu-widgets)))
	   (throw 'no-such-menu (list "menu-option" name)))))))
  
  (define show-all-atoms
    (let ((+documentation+ "(show-all-atoms) displays all current X atom names"))
      (lambda ()
	(let ((i 1)
	      (dpy (XtDisplay (cadr (main-widgets))))
	      (happy #t))
	  (XSetErrorHandler (lambda (dpy err)
			      (set! happy #f)))
	  (while happy
		 (let ((name (XGetAtomName dpy (list 'Atom i))))
		   (if (string? name)
		       (format () "~D: ~A~%" i name)
		       (set! happy #f)))
		 (set! i (+ i 1)))
	  (XSetErrorHandler #f)))))
  
  
;;; -------- enable C-s and C-r in listener
  
  (define add-find-to-listener
    (let ((find-forward #t)
	  (listener-text ((main-widgets) 4))
	  (snd-app (car (main-widgets))))
      (lambda ()
	
	(let ((start-dialog
	       (let ((shell (cadr (main-widgets)))
		     (dialog #f)	
		     (find-new #t)
		     (find-text #f))
		 (lambda ()
		   (unless dialog
		     (let ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
			   (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
			   (xfind (XmStringCreate "Find" XmFONTLIST_DEFAULT_TAG)))
		       (set! dialog (XmCreateMessageDialog shell
							   "Find"
							   (list XmNcancelLabelString   xdismiss
								 XmNokLabelString       xfind
								 XmNhelpLabelString     xhelp
								 XmNautoUnmanage        #f
								 XmNresizePolicy        XmRESIZE_GROW
								 XmNnoResize            #f
								 XmNtransient           #f
								 XmNbackground          *basic-color*)))
		       (for-each
			(lambda (button color)
			  (XtVaSetValues (XmMessageBoxGetChild dialog button)
					 (list XmNarmColor   *selection-color*
					       XmNbackground color)))
			(list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
			(list *highlight-color* *highlight-color* *highlight-color*))
		       (XtAddCallback dialog XmNcancelCallback (lambda (w context info) (XtUnmanageChild dialog)))
		       (XtAddCallback dialog XmNhelpCallback (lambda (w context info) (help-dialog "Find" "no help yet")))
		       (XtAddCallback dialog XmNokCallback (lambda (w context info)
							     (let* ((search-str (XmTextFieldGetString find-text))
								    (len (length search-str))
								    (pos (XmTextFindString listener-text
											   (if find-new
											       (XmTextGetCursorPosition listener-text)
											       (+ (XmTextGetCursorPosition listener-text)
												  (if find-forward 1 -1)))
											   search-str
											   (if find-forward XmTEXT_FORWARD XmTEXT_BACKWARD))))
							       (if (not pos)
								   (set! pos (XmTextFindString listener-text
											       (if find-forward 0 (XmTextGetLastPosition listener-text))
											       search-str
											       (if find-forward XmTEXT_FORWARD XmTEXT_BACKWARD))))
							       (if (number? pos)
								   (begin
								     (XmTextSetInsertionPosition listener-text pos)
								     (XmTextSetHighlight listener-text pos (+ pos len) XmHIGHLIGHT_SELECTED) ; flash the string briefly
								     (XtAppAddTimeOut snd-app 200 
										      (lambda (context id) 
											(XmTextSetHighlight listener-text pos (+ pos len) XmHIGHLIGHT_NORMAL)))))
							       (set! find-new #f))))
		       (XmStringFree xhelp)
		       (XmStringFree xdismiss)
		       (XmStringFree xfind)
		       (set! find-text (XtCreateManagedWidget "text" xmTextFieldWidgetClass dialog
							      (list XmNleftAttachment      XmATTACH_FORM
								    XmNrightAttachment     XmATTACH_FORM
								    XmNtopAttachment       XmATTACH_FORM
								    XmNbottomAttachment    XmATTACH_WIDGET
								    XmNbottomWidget        (XmMessageBoxGetChild dialog XmDIALOG_SEPARATOR)
								    XmNbackground          *basic-color*)))
		       (XtAddCallback find-text XmNfocusCallback 
				      (lambda (w c i)
					(XtVaSetValues w (list XmNbackground (WhitePixelOfScreen (DefaultScreenOfDisplay (XtDisplay shell)))))))
		       (XtAddCallback find-text XmNlosingFocusCallback (lambda (w c i) (XtSetValues w (list XmNbackground *basic-color*))))
		       (XtAddCallback find-text XmNvalueChangedCallback (lambda (w c i) (set! find-new #t)))))
		   (XtManageChild dialog)))))
	  
	  (XtAppAddActions snd-app
			   (list (list "search-forward" 
				       (lambda args 
					 (set! find-forward #t)
					 (start-dialog)))
				 (list "search-backward"
				       (lambda args
					 (set! find-forward #f)
					 (start-dialog))))))
	(XtOverrideTranslations listener-text
				(XtParseTranslationTable "Ctrl <Key>s: search-forward()
						        Ctrl <Key>r: search-backward()")))))
  
  
;;; -------- add a function to be called when the window manager sends us a "save yourself" or "take focus" message
  
  (define upon-save-yourself 
    (let ((+documentation+ "(upon-save-yourself thunk) causes 'thunk' to be called if a 'save yourself' message is received"))
      (lambda (thunk)
	(XmAddWMProtocolCallback 
	 (cadr (main-widgets))
	 (XmInternAtom (XtDisplay (cadr (main-widgets))) "WM_SAVE_YOURSELF" #f)
	 (lambda (w c i)
	   (thunk))
	 #f))))
  
;;; similarly for "take focus"
  
  (define upon-take-focus 
    (let ((+documentation+ "(upon-take-focus thunk) causes 'thunk' to be called if a 'take focus' message is received"))
      (lambda (thunk)
	(XmAddWMProtocolCallback 
	 (cadr (main-widgets))
	 (XmInternAtom (XtDisplay (cadr (main-widgets))) "WM_TAKE_FOCUS" #f)
	 (lambda (w c i)
	   (thunk))
	 #f))))
  
  
;;; -------- add text widget to notebook "status" area --------
  
  (define add-text-to-status-area
    (let ((+documentation+ "(add-text-to-status-area) adds a text widget to the notebook status area"))
      (lambda ()
	;; it might be a better use of this space to put dlp's icon row in it
	(let ((notebook ((main-widgets) 3)))
	  (and (XmIsNotebook notebook)
	       (let ((text (XtCreateManagedWidget "notebook-text" xmTextFieldWidgetClass notebook
						  (list XmNbackground *basic-color*))))
		 (XtAddCallback text XmNfocusCallback
				(lambda (w c i)
				  (XtSetValues w (list XmNbackground (white-pixel)))))
		 (XtAddCallback text XmNlosingFocusCallback
				(lambda (w c i)
				  (XtSetValues w (list XmNbackground *basic-color*))))
		 text))))))
  
  
;;; -------- variable display panel --------
  
  (define variables-dialog #f)
  (define variables-notebook #f)
  (define variables-pages ())
  
  (define make-variables-dialog
    (let ((+documentation+ "(make-variables-dialog) makes a variable-display dialog"))
      (lambda ()
	(let ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
	      (titlestr (XmStringCreate "Variables" XmFONTLIST_DEFAULT_TAG)))
	  (set! variables-dialog 
		(XmCreateTemplateDialog (cadr (main-widgets)) "variables-dialog"
					(list XmNokLabelString       xdismiss
					      XmNautoUnmanage        #f
					      XmNdialogTitle         titlestr
					      XmNresizePolicy        XmRESIZE_GROW
					      XmNnoResize            #f
					      XmNtransient           #f
					      XmNheight              400
					      XmNwidth               400
					      XmNbackground          *basic-color*)))
	  
	  (XtVaSetValues (XmMessageBoxGetChild variables-dialog XmDIALOG_OK_BUTTON)
			 (list XmNarmColor   *selection-color*
			       XmNbackground *highlight-color*))
	  (XtAddCallback variables-dialog 
			 XmNokCallback (lambda (w context info)
					 (XtUnmanageChild variables-dialog)))
	  (XmStringFree xdismiss)
	  (XmStringFree titlestr)
	  
	  (set! variables-notebook
		(XtCreateManagedWidget "variables-notebook" xmNotebookWidgetClass variables-dialog
				       (list XmNleftAttachment      XmATTACH_FORM
					     XmNrightAttachment     XmATTACH_FORM
					     XmNtopAttachment       XmATTACH_FORM
					     XmNbottomAttachment    XmATTACH_WIDGET
					     XmNbottomWidget        (XmMessageBoxGetChild variables-dialog XmDIALOG_SEPARATOR)
					     XmNbackground          *basic-color*
					     XmNframeBackground     *zoom-color*
					     XmNbindingWidth        14)))
	  (XtManageChild variables-dialog)
	  variables-dialog))))
  
  (define make-variable-display 
    (let ((+documentation+ "(make-variable-display page-name variable-name (type 'text) (range (list 0.0 1.0))) makes a variable \
display widget; type = 'text, 'meter, 'graph, 'spectrum, 'scale"))
      (lambda* (page-name variable-name (type 'text) (range (list 0.0 1.0)))
	(if (not (Widget? variables-dialog)) (make-variables-dialog))
	;; rowColumn widget gets confused by drawingareas, so I'll split them out into separate panes
	(let ((page-info (assoc page-name variables-pages)))
	  (if (not page-info)
	      (let* ((panes (XtCreateManagedWidget page-name xmPanedWindowWidgetClass variables-notebook ()))
		     (simple-cases (XtCreateManagedWidget page-name xmRowColumnWidgetClass panes
							  (list XmNorientation XmVERTICAL
								XmNpaneMinimum 30
								XmNbackground  *basic-color*))))
		(set! page-info (list page-name panes simple-cases))
		(XtCreateManagedWidget page-name xmPushButtonWidgetClass variables-notebook
				       (list XmNnotebookChildType XmMAJOR_TAB
					     XmNbackground        *basic-color*))
		(set! variables-pages (cons page-info variables-pages))))
	  (let ((row-pane (caddr page-info))
		(pane (cadr page-info))
		(var-label (string-append variable-name ":")))
	    (case type
	      ((text)
	       ;; add a horizontal pair: label text
	       (let ((row (XtCreateManagedWidget (string-append variable-name "-row") xmRowColumnWidgetClass row-pane
						 (list XmNorientation XmHORIZONTAL
						       XmNbackground  *basic-color*))))
		 (XtCreateManagedWidget var-label xmLabelWidgetClass row
					(list XmNbackground  *basic-color*))
		 (XtCreateManagedWidget (string-append variable-name "-value") xmTextFieldWidgetClass row
					(list XmNeditable #f
					      XmNresizeWidth #t
					      XmNbackground (WhitePixelOfScreen (DefaultScreenOfDisplay (XtDisplay variables-dialog)))))))
	      ((scale)
	       ;; scale bar with red "thermometer"
	       (let* ((title (XmStringCreate var-label XmFONTLIST_DEFAULT_TAG))
		      (scl (XtCreateManagedWidget variable-name xmScaleWidgetClass row-pane
						  (list XmNbackground  *basic-color*
							XmNslidingMode XmTHERMOMETER
							XmNminimum (floor (* 100 (car range)))
							XmNmaximum (floor (* 100 (cadr range)))
							XmNdecimalPoints 2
							XmNtitleString title
							XmNorientation XmHORIZONTAL
							XmNshowValue XmNEAR_BORDER))))
		 (XtVaSetValues (find-child scl "Scrollbar") (list XmNtroughColor (red-pixel)))
		 (XmStringFree title)
		 scl))

	      ((graph)
	       (let ((snd (make-variable-graph  
			   (XtCreateManagedWidget var-label xmFormWidgetClass pane (list XmNpaneMinimum 100))
			   (string-append variable-name ": time") 2048 (floor *clm-srate*))))
		 (list (sound->integer snd) (channel-data snd 0))))

	      ((spectrum)
	       (let ((snd (make-variable-graph 
			   (XtCreateManagedWidget var-label xmFormWidgetClass pane (list XmNpaneMinimum 100))
			   variable-name 2048 (floor *clm-srate*))))
		 (set! (time-graph? snd 0) #f)
		 (set! (transform-graph? snd 0) #t)
		 (set! (x-axis-label snd 0 transform-graph) (string-append variable-name ": frequency"))
		 (list (sound->integer snd) (channel-data snd 0))))

	      (else #f)))))))
  
  (define variable-display 
    (let ((+documentation+ "(variable-display var widget) displays the value of 'var' in 'widget'"))
      (lambda (var widget)
	(if (Widget? widget)
	    (if (XmIsTextField widget)
		;; text representation
		(let ((old-str (XmTextFieldGetString widget))
		      (new-str (object->string var)))
		  (if (not (string=? old-str new-str))
		      (begin
			(XmTextFieldSetString widget new-str)
			(if (XtIsManaged widget)
			    (XmUpdateDisplay widget)))))
		(if (XmIsScale widget)
		    ;; thermometer
		    (XmScaleSetValue widget (floor (* 100 var)))))
	    (if (and (pair? widget)
		     (or (number? (car widget))
			 (sound? (car widget))))
		;; graph/spectrum -- does this need an explicit update?
		(let ((snd (car widget))
		      (data (cadr widget)))
		  (let ((len (length data))
			(loc (cursor snd 0)))
		    (set! (data loc) var)
		    (if (time-graph? snd) (update-time-graph snd))
		    (if (transform-graph? snd) (update-transform-graph snd))
		    (set! cursor (if (= (+ loc 1) len) 0 (+ loc 1)))))))
	var)))
  
  (define variable-display-reset 
    (let ((+documentation+ "(variable-display-reset widget) restarts the variable graphs -- this is intended for the start (or perhaps end) of a note"))
      (lambda (widget)
	(if (and (pair? widget)
		 (number? (car widget)))
	    ;; graph/spectrum
	    (let ((data (cadr widget)))
	      (let ((snd (car widget))
		    (len (length data)))
		(set! (cursor snd 0) 0)
		(do ((i 0 (+ i 1)))
		    ((= i len))
		  (vector-set! data 0 i 0.0))))))))
  
  
#|
  (define wid (make-variable-display "do-loop" "i*2" 'text))
  (define wid1 (make-variable-display "do-loop" "i" 'text))
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (variable-display (* (variable-display i wid1) 2) wid))
  
  (define wid2 (make-variable-display "a-loop" "k*2" 'meter))
					;(define wid3 (make-variable-display "a-loop" "k" 'scale '(0 40)))
  (do ((k 0 (+ k 1)))
      ((= k 11))
    (variable-display (* k .02) wid2))
|#
  
  (define with-minmax-button
    (let ((maxed-snds ()))
      (lambda (snd)
	(let ((previous-minmax (find-if (lambda (n) (equal? (car n) snd)) maxed-snds)))
	  (unless previous-minmax
	    (let* ((widgets (sound-widgets snd))
		   (status-area (widgets 3))
		   (play-button (widgets 4))
		   (cur-size (cadr (XtVaGetValues (car widgets) (list XmNheight 0)))))
	      (XtUnmanageChild play-button)
	      (let ((new-minmax (XtCreateManagedWidget "." xmPushButtonWidgetClass 
						       (XtParent status-area)
						       (list XmNbackground      *basic-color*
							     XmNrightAttachment XmATTACH_FORM
							     XmNtopAttachment   XmATTACH_FORM
							     XmNmarginWidth 2
							     XmNmarginHeight 0
							     XmNshadowThickness 0))))
		(XtVaSetValues play-button (list XmNrightAttachment XmATTACH_WIDGET
						 XmNrightWidget new-minmax))
		(XtManageChild play-button)
		(XtAddCallback 
		 new-minmax XmNactivateCallback 
		 (lambda (w c i)
		   (let ((mv (find-if (lambda (n) (equal? (car n) c)) maxed-snds)))
		     (when mv
		       (let ((maxed (caddr mv)))
			 (if maxed
			     (begin
			       (set! (mv 3) (cadr (XtVaGetValues (car (sound-widgets c)) (list XmNheight 0))))
			       (set! (mv 4) (show-controls c))
			       (do ((i 0 (+ i 1)))
				   ((= i (channels c)))
				 (XtUnmanageChild ((channel-widgets c i) 10)))
			       (set! (show-controls c) #f)
			       (XmChangeColor new-minmax (make-color 1 1 0))
			       (XtVaSetValues (car (sound-widgets c)) (list XmNpaneMaximum 25)))
			     (let ((prev-size (mv 3)))
			       (do ((i 0 (+ i 1)))
				   ((= i (channels c)))
				 (XtManageChild ((channel-widgets c i) 10)))
			       (if (mv 4) (set! (show-controls c) #t))
			       (XmChangeColor new-minmax *basic-color*)
			       (XtVaSetValues (car (sound-widgets c)) (list XmNpaneMaximum prev-size XmNpaneMinimum (- prev-size 1)))
			       (XtVaSetValues (car (sound-widgets c)) (list XmNpaneMaximum 1000 XmNpaneMinimum 1))))
			 (set! (mv 2) (not maxed))))))
		 snd)
		
		(set! previous-minmax (list snd new-minmax #t cur-size (show-controls snd)))
		(set! maxed-snds (cons previous-minmax maxed-snds)))))
	  #f))))
  
  
					;(hook-push after-open-hook (lambda (hook) (with-minmax-button (hook 'snd))))
  
#|
  (define set-root-window-color 
    (let ((+documentation+ "(set-root-window-color color) sets the color of the overall X background"))
      (lambda (color)
	(let* ((dpy (XtDisplay (cadr (main-widgets))))
	       (root-window (DefaultRootWindow dpy)))
	  (XSetWindowBackground dpy root-window color)
	  (XClearWindow dpy root-window)))))
|#
  
  
;;; you can get a different scrollbar style with:
;;; (XtVaSetValues (XmGetXmDisplay (XtDisplay (cadr (main-widgets)))) (list XmNenableThinThickness #t))
  
  
;;; get open file list across top of window (like Xemacs): use -notebook, then:
;;; this is now the default
  (define notebook-with-top-tabs
    (let ((+documentation+ "(notebook-with-top-tabs) posts the list of open sounds across the top of the Snd window (like the Emacs buffer list)"))
      (lambda ()
	(XtVaSetValues ((main-widgets) 3)
		       (list XmNorientation XmVERTICAL
			     XmNbindingType XmNONE
			     XmNbackPagePlacement XmTOP_RIGHT)))))
  
  
#|
;;; -------- create-ssb-dialog --------
;;;
;;; this needs auto-pitch detection
  
  (define ssb-dialog #f)
  
  (define create-ssb-dialog
    (let ((+documentation+ "(create-ssb-dialog) creates a dialog for testing the ssb-am stuff"))
      (lambda ()
	(if (not (Widget? ssb-dialog))
	    (let ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
		  (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
		  (titlestr (XmStringCreate "SSB-Expand" XmFONTLIST_DEFAULT_TAG))
		  (running #f))
	      (set! ssb-dialog 
		    (XmCreateTemplateDialog (cadr (main-widgets)) "ssb-expand"
					    (list XmNcancelLabelString   xdismiss
						  XmNhelpLabelString     xhelp
						  XmNautoUnmanage        #f
						  XmNdialogTitle         titlestr
						  XmNresizePolicy        XmRESIZE_GROW
						  XmNnoResize            #f
						  XmNbackground          *basic-color*
						  XmNwidth               400
						  XmNtransient           #f) ))
	      (XtAddCallback ssb-dialog 
			     XmNcancelCallback (lambda (w context info)
						 (if running (set! running #f))
						 (XtUnmanageChild ssb-dialog)))
	      (XtAddCallback ssb-dialog XmNhelpCallback (lambda (w context info) (snd-print "set 'play' and move the sliders!")))
	      (XmStringFree xhelp)
	      (XmStringFree xdismiss)
	      (XmStringFree titlestr)
	      
	      (let ((ratio 1.0)
		    (old-freq 550.0)
		    (new-freq 550.0)
		    (hilbert-order 40)
		    (ssb-pairs 10)
		    (bw 50.0)
		    (ssbs (make-vector 512))
		    (bands (make-vector 512))
		    (reader #f))
		
		(letrec ((ssb-expand 
			  (lambda ()
			    (let ((sum 0.0)
				  (sig (reader)))
			      (do ((i 0 (+ i 1)))
				  ((= i ssb-pairs) sum)
				(set! sum (+ sum (ssb-am (ssbs i) (fir-filter (bands i) sig))))))))
			 (set-freq 
			  (lambda (nfreq)
			    (set! old-freq nfreq)
			    (set! ratio (/ (- new-freq old-freq) old-freq))
			    (if running
				(do ((i 0 (+ i 1)))
				    ((= i ssb-pairs))
				  (set! (mus-frequency (ssbs i)) (* (+ i 1) ratio old-freq))))))
			 (set-ratio
			  (lambda (nfreq)
			    (set! new-freq nfreq)
			    (set! ratio (/ (- new-freq old-freq) old-freq))
			    (if running
				(do ((i 0 (+ i 1)))
				    ((= i ssb-pairs))
				  (set! (mus-frequency (ssbs i)) (* (+ i 1) ratio old-freq))))))
			 (set-pairs 
			  (lambda (pairs)
			    (set! ssb-pairs pairs)))
			 (set-order 
			  (lambda (order)
			    (set! hilbert-order order))))
		  (let* ((mainform 
			  (XtCreateManagedWidget "formd" xmRowColumnWidgetClass ssb-dialog
						 (list XmNleftAttachment      XmATTACH_FORM
						       XmNrightAttachment     XmATTACH_FORM
						       XmNtopAttachment       XmATTACH_FORM
						       XmNbottomAttachment    XmATTACH_WIDGET
						       XmNbottomWidget        (XmMessageBoxGetChild ssb-dialog XmDIALOG_SEPARATOR)
						       XmNbackground          *basic-color*
						       XmNorientation         XmVERTICAL)))
			 (button 
			  (XtCreateManagedWidget "play" xmToggleButtonWidgetClass mainform
						 (list XmNbackground  *basic-color*)))
			 (freqstr (XmStringCreate "original freq" XmFONTLIST_DEFAULT_TAG))
			 (freq-scale
			  (XtCreateManagedWidget "frq" xmScaleWidgetClass mainform
						 (list XmNorientation XmHORIZONTAL
						       XmNshowValue   #t
						       XmNbackground  *basic-color*
						       XmNvalue       (floor old-freq)
						       XmNmaximum     1000
						       XmNtitleString freqstr
						       XmNdecimalPoints 0)))
			 (ratiostr (XmStringCreate "new freq" XmFONTLIST_DEFAULT_TAG))
			 (ratio-scale
			  (XtCreateManagedWidget "nfrq" xmScaleWidgetClass mainform
						 (list XmNorientation XmHORIZONTAL
						       XmNshowValue   #t
						       XmNbackground  *basic-color*
						       XmNvalue       (floor new-freq)
						       XmNmaximum     1000
						       XmNtitleString ratiostr
						       XmNdecimalPoints 0)))
			 (orderstr (XmStringCreate "order" XmFONTLIST_DEFAULT_TAG))
			 (order-scale
			  (XtCreateManagedWidget "order" xmScaleWidgetClass mainform
						 (list XmNorientation XmHORIZONTAL
						       XmNshowValue   #t
						       XmNbackground  *basic-color*
						       XmNvalue       hilbert-order
						       XmNmaximum     100
						       XmNtitleString orderstr
						       XmNdecimalPoints 0)))
			 (pairsstr (XmStringCreate "ssbs" XmFONTLIST_DEFAULT_TAG))
			 (pairs-scale
			  (XtCreateManagedWidget "pairs" xmScaleWidgetClass mainform
						 (list XmNorientation XmHORIZONTAL
						       XmNshowValue   #t
						       XmNbackground  *basic-color*
						       XmNvalue       ssb-pairs
						       XmNmaximum     100
						       XmNtitleString pairsstr
						       XmNdecimalPoints 0))))
		    (XmStringFree freqstr)
		    (XmStringFree ratiostr)
		    (XmStringFree orderstr)
		    (XmStringFree pairsstr)
		    (XtAddCallback freq-scale XmNvalueChangedCallback (lambda (w context info) (set-freq (.value info))))
		    (XtAddCallback freq-scale XmNdragCallback (lambda (w context info) (set-freq (.value info))))
		    (XtAddCallback ratio-scale XmNvalueChangedCallback (lambda (w context info) (set-ratio (.value info))))
		    (XtAddCallback ratio-scale XmNdragCallback (lambda (w context info) (set-ratio (.value info))))
		    (XtAddCallback order-scale XmNvalueChangedCallback (lambda (w context info) (set-order (.value info))))
		    (XtAddCallback order-scale XmNdragCallback (lambda (w context info) (set-order (.value info))))
		    (XtAddCallback pairs-scale XmNvalueChangedCallback (lambda (w context info) (set-pairs (.value info))))
		    (XtAddCallback pairs-scale XmNdragCallback (lambda (w context info) (set-pairs (.value info))))
		    (XtAddCallback button XmNvalueChangedCallback 
				   (lambda (w context info)
				     (if running
					 (set! running #f)
					 (let* ((audio-info (open-play-output 1 44100 #f 128))
						(audio-fd (car audio-info))
						(outchans (cadr audio-info))
						(len (caddr audio-info))
						(data (make-float-vector (list outchans len) 0.0)))
					   (if (not (= audio-fd -1))
					       (begin
						 (do ((i 1 (+ i 1)))
						     ((> i ssb-pairs))
						   (let ((aff (* i old-freq))
							 (bwf (* bw (+ 1.0 (/ i (* 2 ssb-pairs))))))
						     (set! (ssbs (- i 1)) (make-ssb-am (* i ratio old-freq)))
						     (set! (bands (- i 1)) (make-bandpass (hz->radians (- aff bwf)) 
											  (hz->radians (+ aff bwf)) 
											  hilbert-order))))
						 (set! reader (make-sampler 0))
						 (set! running #t)
						 (do ()
						     ((or (not running)
							  (sampler-at-end? reader))
						      (begin
							(XmToggleButtonSetValue button 0 #f)
							(set! running #f)
							(free-sampler reader)
							(mus-audio-close audio-fd)))
						   (do ((k 0 (+ k 1)))
						       ((= k len))
						     (vector-set! data 0 k (ssb-expand)))
						   (mus-audio-write audio-fd data len)))))))))))))
	(XtManageChild ssb-dialog))))
  
  
;;; -------- create-audit-dialog --------
;;;
;;; amp + freq sliders up to 20KHz
  
  (define audit-dialog #f)
  
  (define create-audit-dialog
    (let ((+documentation+ "(create-audit-dialog) creates a slightly dangerous hearing test dialog (don't push the amps way up if you can't hear anything)"))
      (lambda ()
	(if (not (Widget? audit-dialog))
	    (let ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
		  (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
		  (titlestr (XmStringCreate "Hear Anything Yet?" XmFONTLIST_DEFAULT_TAG))
		  (running #f))
	      (set! audit-dialog 
		    (XmCreateTemplateDialog (cadr (main-widgets)) "audit"
					    (list XmNcancelLabelString   xdismiss
						  XmNhelpLabelString     xhelp
						  XmNautoUnmanage        #f
						  XmNdialogTitle         titlestr
						  XmNresizePolicy        XmRESIZE_GROW
						  XmNnoResize            #f
						  XmNbackground          *basic-color*
						  XmNwidth               400
						  XmNtransient           #f) ))
	      (XtAddCallback audit-dialog 
			     XmNcancelCallback (lambda (w context info)
						 (if running (set! running #f))
						 (XtUnmanageChild audit-dialog)))
	      (XtAddCallback audit-dialog XmNhelpCallback (lambda (w context info) (snd-print "set 'play' and move the sliders!")))
	      (XmStringFree xhelp)
	      (XmStringFree xdismiss)
	      (XmStringFree titlestr)
	      (set! *clm-srate* 44100)
	      
	      (let* ((frequency 440.0)
		     (amplitude 0.1)
		     (carrier (make-oscil frequency)))
		(letrec ((v (lambda ()
			      (* amplitude
				 (oscil carrier)))))
		  (let* ((mainform 
			  (XtCreateManagedWidget "formd" xmRowColumnWidgetClass audit-dialog
						 (list XmNleftAttachment      XmATTACH_FORM
						       XmNrightAttachment     XmATTACH_FORM
						       XmNtopAttachment       XmATTACH_FORM
						       XmNbottomAttachment    XmATTACH_WIDGET
						       XmNbottomWidget        (XmMessageBoxGetChild audit-dialog XmDIALOG_SEPARATOR)
						       XmNbackground          *basic-color*
						       XmNorientation         XmVERTICAL)))
			 (button 
			  (XtCreateManagedWidget "play" xmToggleButtonWidgetClass mainform
						 (list XmNbackground  *basic-color*)))
			 (ampstr (XmStringCreate "amp" XmFONTLIST_DEFAULT_TAG))
			 (amp-scale
			  (XtCreateManagedWidget "amp" xmScaleWidgetClass mainform
						 (list XmNorientation XmHORIZONTAL
						       XmNshowValue   #t
						       XmNbackground  *basic-color*
						       XmNvalue       (floor (* amplitude 100))
						       XmNmaximum     100
						       XmNtitleString ampstr
						       XmNdecimalPoints 2)))
			 (freqstr (XmStringCreate "freq" XmFONTLIST_DEFAULT_TAG))
			 (freq-scale
			  (XtCreateManagedWidget "freq" xmScaleWidgetClass mainform
						 (list XmNorientation XmHORIZONTAL
						       XmNshowValue   #t
						       XmNbackground  *basic-color*
						       XmNvalue       (floor frequency)
						       XmNmaximum     20000
						       XmNtitleString freqstr
						       XmNdecimalPoints 0))))
		    (XmStringFree ampstr)
		    (XmStringFree freqstr)
		    (XtAddCallback amp-scale XmNvalueChangedCallback (lambda (w context info) (set! amplitude (* .01 (.value info)))))
		    (XtAddCallback amp-scale XmNdragCallback (lambda (w context info) (set! amplitude (* .01 (.value info)))))
		    (XtAddCallback freq-scale XmNvalueChangedCallback (lambda (w context info) (set! (mus-frequency carrier) (.value info))))
		    (XtAddCallback freq-scale XmNdragCallback (lambda (w context info) (set! (mus-frequency carrier) (.value info))))
		    (XtAddCallback button XmNvalueChangedCallback 
				   (lambda (w context info)
				     (if running
					 (set! running #f)
					 (let* ((audio-info (open-play-output 1 44100 #f 256))
						(audio-fd (car audio-info))
						(outchans (cadr audio-info))
						(len (caddr audio-info))
						(data (make-float-vector (list outchans len) 0.0)))
					   (if (not (= audio-fd -1))
					       (begin
						 (set! running #t)
						 (do ()
						     ((not running)
						      (begin
							(set! running #f)
							(mus-audio-close audio-fd)))
						   (do ((k 0 (+ k 1)))
						       ((= k len))
						     (vector-set! data 0 k (v)))
						   (mus-audio-write audio-fd data len)))))))))))))
	(XtManageChild audit-dialog))))
|#

  
;;; -------- equalize-panes
;;;
;;; this used to be built-in, but never really worked right
  
  (define equalize-panes
    (let ((equalize-sound 
	   (lambda (ind)
	     (let-temporarily (((channel-style ind) channels-combined))))))
      (lambda* (snd)
	(if snd
	    (equalize-sound snd)
	    (for-each equalize-sound (sounds))))))
  
  ) ; end with-let *motif*

