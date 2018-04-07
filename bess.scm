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
	   ;; c/m ratio
	   (cm-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
					    (let ((cm-ratio (XtCreateManagedWidget "c/m ratio:" xmLabelWidgetClass form
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
	   (cm-scale (XtCreateManagedWidget "cm ratio" xmScaleWidgetClass form
					    (list XmNleftAttachment   XmATTACH_WIDGET
						  XmNleftWidget       cm-label
						  XmNbottomAttachment XmATTACH_NONE
						  XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						  XmNtopWidget        cm-label
						  XmNrightAttachment  XmATTACH_FORM
						  XmNshowValue        #f
						  XmNorientation      XmHORIZONTAL
						  XmNbackground       *position-color*))))
      (let ((frequency 220.0)
	    (low-frequency 40.0)
	    (high-frequency 2000.0)
	    (amplitude 0.25)
	    (index 1.0)
	    (high-index 3.0)
	    (ratio 1)
	    (high-ratio 10)
	    (playing 0.0)
	    
	    (set-flabel 
	     (lambda (label value)
	       (let ((s1 (XmStringCreate (format #f "~,3F" value) XmFONTLIST_DEFAULT_TAG)))
		 (XtVaSetValues label (list XmNlabelString s1))
		 (XmStringFree s1))))
	    
	    (set-ilabel 
	     (lambda (label value)
	       (let ((s1 (XmStringCreate (format #f "~D" value) XmFONTLIST_DEFAULT_TAG)))
		 (XtVaSetValues label (list XmNlabelString s1))
		 (XmStringFree s1)))))
	
	(define (freq-callback w c i)
	  (set! frequency (+ low-frequency (* (.value i) (/ (- high-frequency low-frequency) 100.0))))
	  (set-flabel freq-label frequency))
	
	(define (amp-callback w c i)
	  (set! amplitude (/ (.value i) 100.0))
	  (set-flabel amp-label amplitude))
	
	(define (fm-callback w c i)
	  (set! index (* (.value i) (/ high-index 100.0)))
	  (set-flabel fm-label index))
	
	(define (ratio-callback w c i)
	  (set! ratio (floor (* (.value i) (/ high-ratio 100.0))))
	  (set-ilabel cm-label ratio))

	(define fm
	  (let ((carosc (make-oscil 0.0))
		(modosc (make-oscil 0.0)))
	    (lambda ()
	      (* amplitude playing
		 (oscil carosc 
			(+ (hz->radians frequency)
			   (* index 
			      (oscil modosc 
				     (hz->radians (* ratio frequency))))))))))

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
	
	(XtAddCallback cm-scale XmNdragCallback ratio-callback)
	(XtAddCallback cm-scale XmNvalueChangedCallback ratio-callback)
	
	(XtAddCallback play-button XmNvalueChangedCallback (lambda (w c i) (set! playing (if (.set i) 1.0 0.0))))
	(XmAddWMProtocolCallback (XtParent shell) (XmInternAtom (XtDisplay (cadr (main-widgets))) "WM_DELETE_WINDOW" #f) (lambda (w c i) (stop-playing)) #f)
	
	;; set initial values
	(set-flabel freq-label frequency)
	(set-flabel amp-label amplitude)
	(set-flabel fm-label index)
	(set-ilabel cm-label ratio)
	
	(XmScaleSetValue freq-scale (floor (* 100 (/ (- frequency low-frequency) (- high-frequency low-frequency)))))
	(XmScaleSetValue amp-scale (floor (* 100 amplitude)))
	(XmScaleSetValue fm-scale (floor (* 100 (/ index high-index))))
	(XmScaleSetValue cm-scale (floor (* ratio (/ 100 high-ratio))))
	
	(XtManageChild shell)
	(XtRealizeWidget shell)
	
	(play fm)))))


(when (provided? 'gtk4)

  ;; if (play fm), the callbacks aren't called? 

  ;; this should be a standard function somewhere
  (when (or (not (defined? '*gtk*))
	    (eq? #<undefined> (*gtk* 'gtk_window_set_title)))
    (format *stderr* "looking for libgtk_s7~%")
    (if (file-exists? "libgtk_s7.so")
	(load "libgtk_s7.so" (define *gtk* (inlet 'init_func 'libgtk_s7_init)))
	(if (file-exists? "libgtk_s7.c")
	    (begin
	      (format *stderr* "building libgtk_s7~%")
	      (system "gcc -c libgtk_s7.c -o libgtk_s7.o -I. -fPIC `pkg-config --libs gtk+-4.0 --cflags` -lm -ldl")
	      (system "gcc libgtk_s7.o -shared -o libgtk_s7.so")
	      (load "libgtk_s7.so" (define *gtk* (inlet 'init_func 'libgtk_s7_init))))
	    (error 'no-such-file "can't find libgtk_s7.c"))))

  (with-let (sublet *gtk*)
    (let ((fm-dialog (gtk_dialog_new)))
      (gtk_window_set_transient_for (GTK_WINDOW fm-dialog) (GTK_WINDOW ((main-widgets) 1)))
      (gtk_window_set_title (GTK_WINDOW fm-dialog) "FM!")
      (gtk_window_set_default_size (GTK_WINDOW fm-dialog) 500 200)
      (gtk_window_set_resizable (GTK_WINDOW fm-dialog) #t)
      (gtk_widget_realize fm-dialog)
      (g_signal_connect fm-dialog "delete_event" 
			(lambda (w ev data)
			  (stop-playing)
			  (gtk_widget_hide fm-dialog) ; or destroy??
			  #t)
			#f)
      (let ((dismiss-button (gtk_dialog_add_button (GTK_DIALOG fm-dialog) "Go Away" GTK_RESPONSE_NONE)))
	(g_signal_connect dismiss-button "clicked" 
			  (lambda (w data) 
			    (stop-playing)
			    (gtk_widget_hide fm-dialog))
			  #f)
	(gtk_widget_show dismiss-button)
	(gtk_widget_set_name dismiss-button "quit_button"))
      
      (let* ((mainform (gtk_box_new GTK_ORIENTATION_VERTICAL 2))
	     (table (gtk_grid_new)))
	(gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG fm-dialog))) mainform)
	(gtk_widget_set_hexpand mainform #t)
	(gtk_widget_set_vexpand mainform #t)
	(gtk_widget_show mainform)
	
	(gtk_box_pack_start (GTK_BOX mainform) table)
	(gtk_grid_set_row_spacing (GTK_GRID table) 4)
	(gtk_grid_set_column_spacing (GTK_GRID table) 4)
	(gtk_widget_show table)
	
	(let ((frequency 220.0)
	      (low-frequency 40.0)
	      (high-frequency 2000.0)
	      (amplitude 0.25)
	      (index 1.0)
	      (high-index 10.0)
	      (ratio 1)
	      (high-ratio 10)
	      (playing 0.0))
	  
	  (define fm
	    (let ((carosc (make-oscil 0.0))
		  (modosc (make-oscil 0.0)))
	      (lambda ()
		(* amplitude playing
		   (oscil carosc 
			  (+ (hz->radians frequency)
			     (* index 
				(oscil modosc 
				       (hz->radians (* ratio frequency))))))))))
	  
	  ;; play
	  (let ((button (gtk_check_button_new_with_label "play")))
	    (gtk_grid_attach (GTK_GRID table) button 0 0 1 1)        ; left top width height
	    (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) #f)
	    (gtk_widget_show button)
	    (g_signal_connect button "toggled" 
			      (lambda (w d)
				(set! playing (if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w)) 1.0 0.0)))
			      #f))
	  ;; frequency
	  (let ((freq-label (gtk_label_new "freq"))
		(freq-adj (gtk_adjustment_new frequency low-frequency high-frequency 0.0 0.0 0.0)))
	    (let ((freq-scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT freq-adj))))
	      (gtk_grid_attach (GTK_GRID table) freq-label 0 1 1 1)        ; left top width height
	      (gtk_widget_show freq-label)
	      (gtk_scale_set_digits (GTK_SCALE freq-scale) 2)
	      (gtk_scale_set_draw_value (GTK_SCALE freq-scale) #t)
	      (gtk_scale_set_value_pos (GTK_SCALE freq-scale) GTK_POS_LEFT)
	      (gtk_grid_attach (GTK_GRID table) freq-scale 1 1 1 1)
	      (gtk_widget_set_hexpand (GTK_WIDGET freq-scale) #t)
	      (gtk_widget_show freq-scale)
	      (g_signal_connect freq-adj "value_changed" 
				(lambda (w data)
				  (set! frequency (gtk_adjustment_get_value (GTK_ADJUSTMENT freq-adj))))
				#f)))
	  ;; amplitude
	  (let ((amp-label (gtk_label_new "amp"))
		(amp-adj (gtk_adjustment_new amplitude 0.0 1.0 0.0 0.0 0.0)))
	    (let ((amp-scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT amp-adj))))
	      (gtk_grid_attach (GTK_GRID table) amp-label 0 2 1 1)        ; left top width height
	      (gtk_widget_show amp-label)
	      (gtk_scale_set_digits (GTK_SCALE amp-scale) 2)
	      (gtk_scale_set_draw_value (GTK_SCALE amp-scale) #t)
	      (gtk_scale_set_value_pos (GTK_SCALE amp-scale) GTK_POS_LEFT)
	      (gtk_grid_attach (GTK_GRID table) amp-scale 1 2 1 1)
	      (gtk_widget_set_hexpand (GTK_WIDGET amp-scale) #t)
	      (gtk_widget_show amp-scale)
	      (g_signal_connect amp-adj "value_changed" 
				(lambda (w data)
				  (set! amplitude (gtk_adjustment_get_value (GTK_ADJUSTMENT amp-adj))))
				#f)))
	  ;; fm-index
	  (let ((index-label (gtk_label_new "index"))
		(index-adj (gtk_adjustment_new index 0.0 high-index 0.0 0.0 0.0)))
	    (let ((index-scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT index-adj))))
	      (gtk_grid_attach (GTK_GRID table) index-label 0 3 1 1)        ; left top width height
	      (gtk_widget_show index-label)
	      (gtk_scale_set_digits (GTK_SCALE index-scale) 2)
	      (gtk_scale_set_draw_value (GTK_SCALE index-scale) #t)
	      (gtk_scale_set_value_pos (GTK_SCALE index-scale) GTK_POS_LEFT)
	      (gtk_grid_attach (GTK_GRID table) index-scale 1 3 1 1)
	      (gtk_widget_set_hexpand (GTK_WIDGET index-scale) #t)
	      (gtk_widget_show index-scale)
	      (g_signal_connect index-adj "value_changed" 
				(lambda (w data)
				  (set! index (gtk_adjustment_get_value (GTK_ADJUSTMENT index-adj))))
				#f)))
	  ;; c/m ratio
	  (let ((ratio-label (gtk_label_new "ratio"))
		(ratio-adj (gtk_adjustment_new ratio 0 high-ratio 0 0 1)))
	    (let ((ratio-scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT ratio-adj))))
	      (gtk_grid_attach (GTK_GRID table) ratio-label 0 4 1 1)        ; left top width height
	      (gtk_widget_show ratio-label)
	      (gtk_scale_set_digits (GTK_SCALE ratio-scale) 0)
	      (gtk_scale_set_draw_value (GTK_SCALE ratio-scale) #t)
	      (gtk_scale_set_value_pos (GTK_SCALE ratio-scale) GTK_POS_LEFT)
	      (gtk_grid_attach (GTK_GRID table) ratio-scale 1 4 1 1)
	      (gtk_widget_set_hexpand (GTK_WIDGET ratio-scale) #t)
	      (gtk_widget_show ratio-scale)
	      (g_signal_connect ratio-adj "value_changed" 
				(lambda (w data)
				  (set! ratio (gtk_adjustment_get_value (GTK_ADJUSTMENT ratio-adj))))
				#f)))
	  
	  (gtk_widget_show fm-dialog)
	  (gtk_window_present (GTK_WINDOW fm-dialog))
	  (play fm))))))
