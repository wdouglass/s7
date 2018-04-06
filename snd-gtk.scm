;;; translations from snd-motif.scm
;;;
;;; zync and unzync
;;; disable control panel
;;; show-disk-space
;;; remove top level menu
;;; keep-file-dialog-open-upon-ok
;;; snd-clock-icon
;;; bring possibly-obscured dialog to top
;;; select-file
;;; add delete and rename options to the file menu
;;; notebook-with-top-tabs
;;; make-font-selector-dialog
;;; add-main-menu-mnemonics

(when (not (provided? 'gtk4))
  (error 'gtk-error "gtk-effects-utils.scm only works in gtk4"))

(provide 'snd-snd-gtk.scm)
(require snd-gtk snd-extensions.scm snd-play.scm)

(with-let *gtk*
  
  (define load-font pango_font_description_from_string)
  
  (define (g-list-foreach glist func)
    (let ((len (g_list_length glist)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(func (g_list_nth_data glist i)))))
  
  (define for-each-child 
    (let ((+documentation+ "(for-each-child w func) applies func to w and each of its children"))
      (lambda (w func)
	(func w)
	(g-list-foreach (gtk_container_get_children (GTK_CONTAINER w))
			(lambda (w)
			  (func (GTK_WIDGET w)))))))
  
  
  (define host-name ; this is the same as (define (machine-name) (caddr ((*libc* 'uname))))
    (let ((+documentation+ "(host-name) -> name of current machine"))
      (lambda ()
	(let ((val (gdk_property_get (car (main-widgets))
				     (gdk_atom_intern "WM_CLIENT_MACHINE" #f)
				     GDK_TARGET_STRING 0 1024 0)))
	  ;; val is list: (success atom element-size length unterminated-string)
	  (and (car val)
	       (substring (val 4) 0 (val 3)))))))
  
  
;;; -------- zync and unzync: start or stop y-zoom slider sync --------
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop
  
  
  (define (add-dragger hook)
    (let ((snd (hook 'snd)))
      
      (define (dragger-callback adj context)
	(let ((val (- 1.0 (gtk_adjustment_get_value (GTK_ADJUSTMENT adj))))
	      (snd (car context))
	      (chn (cadr context)))
	  (if (sound-property 'dragger snd)
	      (do ((i 0 (+ i 1)))
		  ((= i (channels snd))
		   (g_signal_stop_emission (GPOINTER adj)
					   (g_signal_lookup "value_changed" (G_OBJECT_TYPE (G_OBJECT adj)))
					   0))
		(unless (= i chn)
		  (set! (y-zoom-slider snd i) (* val val))
		  (set! (y-position-slider snd i) (y-position-slider snd chn)))))))
      
      (set! (sound-property 'dragger snd) #t)
      (set! (sound-property 'save-state-ignore snd)
	    (cons 'dragger
		  (or (sound-property 'save-state-ignore snd)
		      (list 'save-state-ignore))))
      (do ((chn 0 (+ 1 chn)))
	  ((= chn (channels snd)))
	(let ((zy ((channel-widgets snd chn) 14)))
	  (g_signal_connect_closure_by_id (GPOINTER zy)
					  (g_signal_lookup "value_changed" (G_OBJECT_TYPE (G_OBJECT zy)))
					  0
					  (g_cclosure_new dragger-callback (list snd chn) (list 'GClosureNotify 0))
					  #f)))))
  
  (define zync
    (let ((+documentation+ "(zync) ties each sound's y-zoom sliders together so that all change in paralle if one changes"))
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
	       (set! (sound-property 'dragger n) #f)))
	 (sounds)))))
  
  
  
;;; -------- disable control panel --------
  
  (define (disable-control-panel snd)
    (gtk_widget_hide (caddr (sound-widgets snd)))
    (remove-from-menu 2 "Show controls"))
  
  
  
;;; -------- show-disk-space
;;;
;;; adds a label to the status-area area showing the current free space 
  
  (define show-disk-space
    (let ((labelled-snds ())
	  
	  (kmg (lambda (num)
		 (cond ((<= num 0)      (copy "disk full!"))
		       ((<= num 1024)   (format #f "space: ~10DK" num))
		       ((> num 1048576) (format #f "space: ~6,3FG" (/ num (* 1024.0 1024.0))))
		       (else            (format #f "space: ~6,3FM" (/ num 1024.0))))))
	  
	  (show-label (lambda (data)
			(if (sound? (car data))
			    (let ((space (kmg (disk-kspace (file-name (car data))))))
			      (gtk_label_set_text (GTK_LABEL (cadr data)) space)
			      (g_timeout_add 10000 show-label data) ; every 10 seconds recheck space
			      0)))))
      (lambda (hook)
	;; (show-disk-space snd) adds a label to snd's status-area area showing the current free space (for use with after-open-hook)
	;; (set! (hook-functions after-open-hook) (list (*motif* 'show-disk-space)))
	
	(let* ((snd (hook 'snd))
	       (previous-label (let find-if ((pred (lambda (n)
						     (equal? (car n) snd)))
					     (lst labelled-snds))
				 (cond ((null? lst) #f)
				       ((pred (car lst)) (car lst))
				       (else (find-if pred (cdr lst)))))))
	  (if (not previous-label)
	      (if (not snd)
		  (snd-error "no sound found for disk space label")
		  (let* ((name-form ((sound-widgets) 10))
			 (new-label (gtk_label_new (kmg (disk-kspace (file-name snd))))))
		    (gtk_box_pack_start (GTK_BOX name-form) new-label)
		    (gtk_widget_show new-label)
		    (set! previous-label (list snd new-label))
		    (set! labelled-snds (cons previous-label labelled-snds))
		    (g_timeout_add 10000 show-label previous-label))))))))
  
  
  
;;; -------- remove top level menu
;;;
;;; (remove-main-menu 5) removes the Help menu
  
  (define remove-main-menu 
    (let ((+documentation+ "(remove-main-menu menu) removes the specified top-level menu: ((*gtk* 'remove-main-menu) 5) removes the Help menu"))
      (lambda (menu)
	(gtk_widget_hide ((menu-widgets) menu)))))
  
  
;;; -------- keep-file-dialog-open-upon-ok
;;;
;;; this seems to work, but it's a kludge
  
  (define (keep-file-dialog-open-upon-ok)
    (let ((dialog (open-file-dialog #f)))
      (g_object_set_data (G_OBJECT dialog) "hide-me" (GPOINTER 1)))) ; anything not 0 means don't hide (this is a stupid kludge forced on me by goddamn gtk)
  
  
  
;;; -------- snd-clock-icon --------
;;;
;;; a clock icon to replace Snd's hourglass
;;;   call from a work proc or whatever with hour going from 0 to 12 then #f
  
  (define snd-clock-icon
    (lambda (snd hour)
      (let ((cr (make_cairo (GDK_WINDOW (gtk_widget_get_window ((sound-widgets snd) 8))))))
	(let ((bg (color->list *basic-color*)))
	  (cairo_set_source_rgb cr (car bg) (cadr bg) (caddr bg)))
	(cairo_rectangle cr 0 0 16 16) ; icon bg
	(cairo_fill cr)
	(cairo_set_source_rgb cr 1.0 1.0 1.0)
	(cairo_arc cr 8 8 7 0 (* 2 pi))  ; clock face
	(cairo_fill cr)
	(cairo_set_line_width cr 2.0)
	(cairo_set_source_rgb cr 0.0 0.0 0.0)
	(cairo_move_to cr 8 8)         ; clock hour hand
	(cairo_line_to cr (+ 8 (* 7 (sin (* hour (/ 3.1416 6.0)))))
		       (- 8 (* 7 (cos (* hour (/ 3.1416 6.0))))))
	(cairo_stroke cr)
	(free_cairo cr))))
  
  
#|  
;;; this is the happy face progress bar
  
  (define (snd-happy-face snd progress)
    (let* ((window (GDK_WINDOW (gtk_widget_get_window ((sound-widgets snd) 8))))
	   (cr (make_cairo window))
	   (fc (list 1.0 progress 0.0)))
      (let ((bg (color->list *basic-color*)))
        ;; overall background
        (cairo_set_source_rgb cr (car bg) (cadr bg) (caddr bg)))
      (cairo_rectangle cr 0 0 16 16)
      (cairo_fill cr)
      
      ;; round face
      (cairo_set_source_rgb cr (car fc) (cadr fc) (caddr fc))
      (cairo_arc cr 8 8 8 0.0 (* 2 pi))
      (cairo_fill cr)
      
      ;; eyes
      (cairo_set_source_rgb cr 0.0 0.0 0.0)
      (cairo_arc cr 5 6 1.5 0 (* 2 pi))
      (cairo_fill cr)
      
      (cairo_arc cr 11 6 1.5 0 (* 2 pi))
      (cairo_fill cr)
      
      ;; mouth
      (cairo_set_line_width cr 1.0)
      (if (< progress 0.4)
	  (cairo_arc cr 8 14 4 (* 17/16 pi) (* -1/16 pi))
	  (if (< progress 0.7)
	      (begin
		(cairo_move_to cr 4 12)
		(cairo_rel_line_to cr 8 0))
	      (cairo_arc cr 8 8 5 (* 1/16 pi) (* 15/16 pi))))
      (cairo_stroke cr)
      
      (free_cairo cr)))
|#
  
  
;;; -------- bring possibly-obscured dialog to top
  
  (define (raise-dialog w)
    (gtk_widget_show w)
    (gtk_window_present (GTK_WINDOW w)))
  
  
;;; -------- select-file --------
;;;
;;; (select-file func title dir filter help)
;;;   starts a File Chooser Dialog, runs func if a file is selected
;;;
;;; (add-to-menu 0 "Insert File" 
;;;   (lambda () 
;;;     (select-file 
;;;       (lambda (filename)
;;;         (insert-sound filename))
;;;       "Insert File" "." "*" "file will be inserted at cursor")))
  
  (define select-file
    
    (letrec ((file-selector-dialogs ())   ; (list (list widget inuse func title help) ...)
	     (find-free-dialog 
	      (lambda (ds)
		(and (pair? ds)
		     (pair? (car ds))
		     (pair? (cdar ds))
		     (if (cadar ds)
			 (find-free-dialog (cdr ds))
			 (begin
			   (set! ((car ds) 1) #t)
			   (caar ds)))))))
      (lambda args
	;; (file-select func title dir filter help)
	(let* ((func (and (pair? args) (args 0)))
	       (title (if (and (pair? args) (pair? (cdr args))) (args 1) "select file"))
	       (dir (if (> (length args) 2) (args 2) "."))
	       (dialog (or (find-free-dialog file-selector-dialogs)
			   (GTK_FILE_CHOOSER_DIALOG (gtk_file_chooser_dialog_new
						     title
						     #f
						     GTK_FILE_CHOOSER_ACTION_OPEN
						     (list "process-stop" GTK_RESPONSE_REJECT
							   "Ok" GTK_RESPONSE_ACCEPT))))))	
	  (gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER dialog) dir)
	  (if (and (= GTK_RESPONSE_ACCEPT (gtk_dialog_run (GTK_DIALOG dialog)))
		   func)
	      (func (gtk_file_chooser_get_filename (GTK_FILE_CHOOSER dialog))))
	  (gtk_widget_hide (GTK_WIDGET dialog))))))
  
  ;; ((*gtk* 'select-file) (lambda (n) (snd-print n)))
  
  
  
#|
;;; -------- with-level-meters, make-level-meter, display-level
  
  (define (make-level-meter parent width height)
    (let ((frame (gtk_frame_new #f)))
      (gtk_widget_set_size_request frame width height)
      (gtk_box_pack_start (GTK_BOX parent) frame)
      (gtk_widget_show frame)
      (let ((meter (gtk_drawing_area_new)))
        ;(gtk_widget_set_events meter (logior GDK_EXPOSURE_MASK GDK_STRUCTURE_MASK))
	(gtk_container_add (GTK_CONTAINER frame) meter)
	(gtk_widget_show meter)
	(let ((context (list meter 0.0 1.0 0.0 0.0 width height)))
	  (g_signal_connect meter "draw" (lambda (w e d) (display-level d)) context)
	  (g_signal_connect meter "configure_event" 
			    (lambda (w e d)
			      (let ((xy (list (gtk_widget_get_allocated_width w)
					      (gtk_widget_get_allocated_height w))))
				(set! (d 5) (car xy))
				(set! (d 6) (cadr xy))
				(display-level d)))
			    context)
	  context))))
  
  (define (display-level meter-data)
    (let* ((meter (car meter-data))
	   (level (meter-data 1))
	   (last-level (meter-data 3))
	   (red-deg (meter-data 4))
	   (width (meter-data 5))
	   (height (meter-data 6))
	   ;; (size (meter-data 2))
	   (win (GDK_WINDOW (gtk_widget_get_window meter))))
      
      ;; this is too slow -- can we save the plate? (also if just 1 meter, put pivot higher?)
      (let ((cr (make_cairo win)))
	
	;; put our origin at the meter pivot point scaled (as a square so the dial remains circular) to 0..1
	(cairo_translate cr (* 0.5 width) (+ (* 0.5 width) (* 0.2 height)))
	(cairo_scale cr width width)
	
	;; background
	(let ((pat (cairo_pattern_create_radial 0 0 .1 0 0 0.75)))
	  (cairo_pattern_add_color_stop_rgb pat 0.0 1.0 0.9 0.0) 
	  (cairo_pattern_add_color_stop_rgb pat 1.0 1.0 1.0 1.0)
	  (cairo_rectangle cr -1 -1 2 2)
	  (cairo_set_source cr pat)
	  (cairo_fill cr)
	  (cairo_pattern_destroy pat))
	
	;; dial markings
	(cairo_set_source_rgb cr 0.0 0.0 0.0)
	
	;; outer arc
	(cairo_set_line_width cr (/ 2.0 width))
	(cairo_arc cr 0 0 0.5 (* -0.75 pi) (* -0.25 pi))
	(cairo_stroke cr)
	
	;; inner arc
	(cairo_set_line_width cr (/ 0.5 width))
	(cairo_arc cr 0 0 (- 0.5 (/ 6.0 width)) (* -0.75 pi) (* -0.25 pi))
	(cairo_stroke cr)
	
	;; save unrotated coords
	(cairo_save cr)
	
	;; ticks
	(cairo_rotate cr (* 5 (/ pi 4)))
	(do ((i 0 (+ i 1)))
	    ((= i 5))
	  (cairo_set_line_width cr (/ 1.5 width))
	  (if (or (= i 0) (= i 4))
	      (begin
		(cairo_move_to cr (- 0.5 (/ 6.0 width)) 0.0)
		(cairo_rel_line_to cr (/ 15.0 width) 0))
	      (begin
		(cairo_move_to cr 0.5 0.0)
		(cairo_rel_line_to cr (/ 9.0 width) 0)))
	  (cairo_stroke cr)
	  (if (< i 4)
	      (begin
		(cairo_set_line_width cr (/ 0.5 width))
		(do ((j 0 (+ 1 j)))
		    ((= j 5))
		  (cairo_move_to cr 0.5 0.0)
		  (cairo_rel_line_to cr (/ 6.0 width) 0)
		  (cairo_rotate cr (/ pi (* 8 5)))
		  (cairo_stroke cr)))))
	(cairo_restore cr)
	
	;; needle and bubble
	(let* ((needle-speed 0.25)
	       (bubble-speed 0.025)
	       (bubble-size (/ pi 12))
	       (val (+ (* level needle-speed) (* last-level (- 1.0 needle-speed)))))
	  (cairo_save cr)
	  (cairo_set_line_width cr (/ 2.0 width))
	  (cairo_rotate cr (+ (* 5 (/ pi 4)) (* val pi 0.5)))
	  (cairo_move_to cr 0 0)
	  (cairo_rel_line_to cr 0.55 0.0)
	  (cairo_stroke cr)
	  (cairo_restore cr)
	  
	  (set! (meter-data 3) val)
	  (if (<= val red-deg)
	      (set! val (+ (* val bubble-speed) (* red-deg (- 1.0 bubble-speed)))))
	  (set! (meter-data 4) val)
	  
	  ;; now the red bubble...
	  (if (> val .01)
	      (begin
		(cairo_set_source_rgb cr 1.0 0.0 0.0)
		(cairo_set_line_width cr (/ 5.0 width))
		(let ((redx (* val 0.5 pi)))
		  (cairo_arc cr 0 0 (- 0.5 (/ 3.0 width))  (+ (* 5 (/ pi 4)) (max 0.0 (- redx bubble-size))) (+ (* 5 (/ pi 4)) redx))
		  (cairo_stroke cr)))))
	
	(free_cairo cr))))
  
  
  (define (with-level-meters n)
    ;; add n level meters to a pane at the top of the Snd window
    (let* ((parent ((main-widgets) 5))
	   (height (if (> n 2) 70 85))
	   (pw (gtk_widget_get_window parent))
	   (parent-width (gtk_widget_get_allocated_height pw))
	   (width (floor (/ parent-width n)))
	   (meters (gtk_box_new GTK_ORIENTATION_HORIZONTAL 4))
	   (meter-list ()))
      (gtk_box_pack_start (GTK_BOX parent) meters)
      (gtk_widget_set_size_request meters width height)
      (gtk_widget_show meters)
      (do ((i 0 (+ i 1)))
	  ((= i n))
	(set! meter-list (cons (make-level-meter meters width height) meter-list)))
      (hook-push dac-hook 
		 (lambda (hook)
		   (let* ((sdobj (hook 'data))
			  (maxes (map float-vector-peak sdobj)))
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
		   (g_idle_add 
		    (let ((ctr 0))
		      (lambda (ignored)
			(for-each 
			 (lambda (meter)
			   (set! (meter 1) 0.0)
			   (display-level meter))
			 meter-list)
			(set! ctr (+ ctr 1))
			(< ctr 200)))
		    #f)))
      meter-list))
|#
  
  
  
;;; -------- state display panel --------
  
  (define variables-dialog #f)
  (define variables-notebook #f)
  (define variables-pages ())
  
  (define (make-variables-dialog)
    (set! variables-dialog (gtk_dialog_new))
    (gtk_window_set_title (GTK_WINDOW variables-dialog) "Variables")
    (gtk_window_set_default_size (GTK_WINDOW variables-dialog) -1 -1)
    (gtk_window_set_resizable (GTK_WINDOW variables-dialog) #t)
    (gtk_widget_realize variables-dialog)
    (g_signal_connect variables-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide variables-dialog) #t) #f)
      
    (let ((dismiss-button (gtk_dialog_add_button (GTK_DIALOG variables-dialog) "Go Away" GTK_RESPONSE_NONE)))
      (g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide variables-dialog)) #f)
      (gtk_widget_show dismiss-button)
      (gtk_widget_set_name dismiss-button "quit_button"))
      
    (set! variables-notebook (gtk_notebook_new))
    (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG variables-dialog))) variables-notebook)
    (gtk_notebook_set_tab_pos (GTK_NOTEBOOK variables-notebook) GTK_POS_RIGHT)
    (gtk_widget_show variables-notebook)
    (gtk_widget_show variables-dialog)
    variables-dialog)
  
  (define* (make-variable-display page-name variable-name (type 'text) (range (list 0.0 1.0)))
    ;; type = 'text, 'meter, 'graph, 'spectrum, 'scale
    (if (not variables-dialog) (make-variables-dialog))
    (let ((page-info (assoc page-name variables-pages)))
      (if (not page-info)
	  (let ((vbox (gtk_box_new GTK_ORIENTATION_VERTICAL 0))
		(tab (gtk_label_new page-name)))
	    (gtk_widget_show tab)
	    (gtk_widget_show vbox)
	    (gtk_notebook_append_page (GTK_NOTEBOOK variables-notebook) vbox tab)
	    (set! page-info (cons page-name vbox))
	    (set! variables-pages (cons page-info variables-pages))))
      (let ((pane (cdr page-info))
	    (var-label (string-append variable-name ":")))
	(case type
	  ((text)
	   ;; add a horizontal pair: label text
	   (let ((text (gtk_label_new "")))
	     (let ((hbox (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0)))
	       (let ((label (gtk_label_new var-label)))
		 (gtk_box_pack_start (GTK_BOX pane) hbox)
		 (gtk_widget_show hbox)
		 (gtk_box_pack_start (GTK_BOX hbox) label)
		 (gtk_widget_set_halign (GTK_WIDGET label) GTK_ALIGN_START)
		 (gtk_widget_show label))
	       (gtk_box_pack_start (GTK_BOX hbox) text))
	     (gtk_widget_set_halign (GTK_WIDGET text) GTK_ALIGN_START)
	     (gtk_widget_show text)
	     text))
	  ((scale)
	   (let ((label (gtk_label_new var-label))
		 (hbox (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0))
		 (scale (gtk_progress_bar_new)))
	     (gtk_box_pack_start (GTK_BOX pane) hbox)
	     (gtk_widget_show hbox)
	     (gtk_box_pack_start (GTK_BOX hbox) label)
	     (gtk_widget_set_halign (GTK_WIDGET label) GTK_ALIGN_START)
	     (gtk_widget_show label)
	     (gtk_box_pack_start (GTK_BOX hbox) scale)
	     (gtk_widget_show scale)
	     (list scale (car range) (cadr range))))
	  ((graph)
	   (let ((snd (make-variable-graph pane (string-append variable-name ": time") 2048 *clm-srate*)))
	     (list (sound->integer snd) (channel-data snd 0))))
	  ((spectrum)
	   (let ((snd (make-variable-graph pane variable-name 2048 *clm-srate*)))
	     (set! (time-graph? snd 0) #f)
	     (set! (transform-graph? snd 0) #t)
	     (set! (x-axis-label snd 0 transform-graph) (string-append variable-name ": frequency"))
	     (list (sound->integer snd) (channel-data snd 0))))
	  (else #f)))))
  
  (define variable-display 
    (let ((force-update (lambda (wid)
			  (gdk_window_invalidate_rect (GDK_WINDOW (gtk_widget_get_window (GTK_WIDGET wid))) (list 'GdkRectangle_ 0) #t)
			  ))
	  (widget? (lambda (w) 
		     (and (pair? w) 
			  (= (length w) 2)
			  (eq? (car w) 'GtkWidget_)))))
      ;; (let ((wid1 (make-variable-display "do-loop" "i*1" 'spectrum))) (variable-display 0.1 wid1))
      (lambda (var widget)
	(if (widget? widget)
	    (if (GTK_IS_LABEL widget)
		(begin
		  (gtk_label_set_text (GTK_LABEL widget) (object->string var))
		  (force-update widget)))
	    
	    (when (and (pair? widget)
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
		  (set! (cursor snd 0) (if (= (+ loc 1) len) 0 (+ loc 1))))
		(if (GTK_IS_PROGRESS_BAR (car widget))
		    ;; "thermometer"
		    (let ((y0 (cadr widget))
			  (y1 (caddr widget)))
		      ;; (define wid (make-variable-display "do-loop" "i*2" 'scale))
		      (gtk_progress_bar_set_fraction 
		       (GTK_PROGRESS_BAR (car widget))
		       (max 0.0 (min 1.0 (/ (- var y0) (- y1 y0))))))))))
	var)))

  (define (notebook-with-top-tabs)
    (gtk_notebook_set_tab_pos (GTK_NOTEBOOK ((main-widgets) 5)) GTK_POS_TOP))

  ) ; *gtk*
