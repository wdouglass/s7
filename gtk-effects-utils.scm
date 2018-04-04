(when (not (provided? 'gtk4))
  (error 'gtk-error "gtk-effects-utils.scm only works in gtk4"))

(require snd-gtk)
(provide 'snd-gtk-effects-utils.scm)

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

(with-let *gtk* 

  (define (all-chans)
    (let ((sndlist ())
	  (chnlist ()))
      (for-each (lambda (snd)
		  (do ((i (- (channels snd) 1) (- i 1)))
		      ((< i 0))
		    (set! sndlist (cons snd sndlist))
		    (set! chnlist (cons i chnlist))))
		(sounds))
      (list sndlist chnlist)))
  
  (define (update-label effects)
    (for-each (lambda (effect) (effect)) effects))
  
  (define (effect-target-ok target)
    (case target 
      ((sound) (pair? (sounds)))
      ((selection) (selection?))
      (else (and (selected-sound)
		 (>= (length (marks (selected-sound) (selected-channel))) 2)))))
  
  (define* (make-effect-dialog label ok-callback help-callback reset-callback target-ok-callback)
    ;; make a standard dialog
    ;; callbacks take 2 args: widget data
    (let ((new-dialog (gtk_dialog_new)))
      
      (if (defined? 'gtk_widget_set_clip) ; gtk 3.14.0
	  (gtk_window_set_transient_for (GTK_WINDOW new-dialog) (GTK_WINDOW ((main-widgets) 1))))
      (gtk_window_set_title (GTK_WINDOW new-dialog) label)
      (gtk_window_set_default_size (GTK_WINDOW new-dialog) -1 -1)
      (gtk_window_set_resizable (GTK_WINDOW new-dialog) #t)
      (gtk_widget_realize new-dialog)
      (g_signal_connect new-dialog "delete_event" 
			(lambda (w ev data) 
			  (gtk_widget_hide new-dialog)
			  #t) ; this is crucial -- thanks to Kjetil for catching it!
			#f)
      
      (let ((dismiss-button (gtk_dialog_add_button (GTK_DIALOG new-dialog) "Go Away" GTK_RESPONSE_NONE)))
	(g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide new-dialog)) #f)
	(gtk_widget_show dismiss-button)
	(gtk_widget_set_name dismiss-button "quit_button"))
      
      (let ((ok-button (gtk_dialog_add_button (GTK_DIALOG new-dialog) "DoIt" GTK_RESPONSE_NONE)))
	(g_signal_connect ok-button "clicked" ok-callback #f)
	(gtk_widget_show ok-button)
	(gtk_widget_set_name ok-button "doit_button")
	
	(if reset-callback
	    (let ((reset-button (gtk_dialog_add_button (GTK_DIALOG new-dialog) "Reset" GTK_RESPONSE_NONE)))
	      (g_signal_connect reset-button "clicked" reset-callback #f)
	      (gtk_widget_set_name reset-button "reset_button")
	      (gtk_widget_show reset-button)))
	
	(let ((help-button (gtk_dialog_add_button (GTK_DIALOG new-dialog) "Help" GTK_RESPONSE_NONE)))
	  (g_signal_connect help-button "clicked" help-callback #f)
	  (gtk_widget_show help-button)
	  (gtk_widget_set_name help-button "help_button"))
	
	(if target-ok-callback
	    (begin
	      (gtk_widget_set_sensitive ok-button (target-ok-callback))
	      (hook-push effects-hook
			 (lambda (hook) (gtk_widget_set_sensitive ok-button (target-ok-callback)))))
	    (begin
	      (gtk_widget_set_sensitive ok-button (pair? (sounds)))
	      (hook-push effects-hook
			 (lambda (hook) (gtk_widget_set_sensitive ok-button (pair? (sounds)))))))
	
	(g_object_set_data (G_OBJECT new-dialog) "ok-button" (GPOINTER ok-button))
	new-dialog)))
  
  
  ;; -------- log scaler widget
  
  (define log-scale-ticks 500) ; sets precision (to some extent) of slider 
  
  (define (scale-log->linear lo val hi)
    ;; given user-relative low..val..hi return val as scale-relative (0..log-scale-ticks)
    (let (;; using log 2 here to get equally spaced octaves
	  (log-lo (log (max lo 1.0) 2))
	  (log-hi (log hi 2))
	  (log-val (log val 2)))
      (floor (* log-scale-ticks (/ (- log-val log-lo) (- log-hi log-lo))))))
  
  (define (scale-linear->log lo val hi)
    ;; given user-relative lo..hi and scale-relative val, return user-relative val
    ;; since log-scale widget assumes 0..log-scale-ticks, val can be used as ratio (log-wise) between lo and hi
    (let ((log-lo (log (max lo 1.0) 2))
	  (log-hi (log hi 2)))
      (expt 2.0 (+ log-lo (* (/ val log-scale-ticks) (- log-hi log-lo))))))
  
  (define (scale-log-label lo val hi)
    (format #f "~,2F" (scale-linear->log lo val hi)))
  
  (define (add-sliders dialog sliders)
    ;; sliders is a list of lists, each inner list being (title low initial high callback scale ['log])
    ;; returns list of widgets (for reset callbacks)
    (let* ((mainform (gtk_box_new GTK_ORIENTATION_VERTICAL 2))
	   (use-hbox (and (pair? sliders) (null? (cdr sliders))))
	   (table (if (not use-hbox) (gtk_grid_new))))
      (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG dialog))) mainform)
      (gtk_widget_show mainform)
      (if (not use-hbox)
	  (begin
	    (gtk_box_pack_start (GTK_BOX mainform) table)
	    (gtk_grid_set_row_spacing (GTK_GRID table) 4)
	    (gtk_grid_set_column_spacing (GTK_GRID table) 4)
	    (gtk_widget_show table)))
      (map 
       (let ((slider 0))
	 (lambda (slider-data)
	   (let* ((title (slider-data 0))
		  (low (slider-data 1))
		  (initial (slider-data 2))
		  (high (slider-data 3))
		  (func (slider-data 4))
		  (scaler (slider-data 5))
		  (use-log (and (= (length slider-data) 7)
				(eq? (slider-data 6) 'log)))
		  (hbox (and use-hbox (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0)))
		  (label (gtk_label_new 
			  (format #f (if use-hbox
					 (if use-log 
					     (values "~A: ~,2F" title initial)
					     (values "~A:" title))
					 (if use-log
					     (values "~A (~,2F)" title initial)
					     (values "~A" title))))))
		  (adj (if use-log 
			   (gtk_adjustment_new (scale-log->linear low initial high) 0 log-scale-ticks 1 10 1)
			   (gtk_adjustment_new initial low high 0.0 0.0 0.0)))
		  (scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT adj))))
	     (if use-hbox
		 (begin
		   (gtk_box_pack_start (GTK_BOX mainform) hbox)
		   (gtk_widget_show hbox)
		   (gtk_box_pack_start (GTK_BOX hbox) label))
		 (gtk_grid_attach (GTK_GRID table) label 0 slider 1 1))
	     (gtk_widget_show label)
	     (gtk_scale_set_digits (GTK_SCALE scale)
				   (cond (use-log 0)
					 ((assoc scaler '((1000 . 3) (100 . 2) (10 . 1)) =) => cdr)
					 (else 0)))
	     (gtk_scale_set_draw_value (GTK_SCALE scale) (not use-log))
	     (if use-hbox
		 (gtk_box_pack_start (GTK_BOX hbox) scale)
		 (begin
		   (gtk_widget_set_hexpand (GTK_WIDGET scale) #t)
		   (gtk_grid_attach (GTK_GRID table) scale 1 slider 1 1)
		   (set! slider (+ 1 slider))))
	     (gtk_widget_show scale)
	     (let ((label-func (if (not use-log)
				   func
				   (lambda (w d) 
				     (func w d)
				     (gtk_label_set_text (GTL_LABEL label)
							 (format #f "~A: ~,2F" 
								 title 
								 (scale-linear->log low (gtk_adjustment_get_value (GTK_ADJUSTMENT adj)) high)))))))
	       (g_signal_connect adj "value_changed" label-func #f))
	     adj)))
       sliders)))
  
  (define (activate-dialog w)
    (gtk_widget_show w)
    (gtk_window_present (GTK_WINDOW w)))
  
  )
