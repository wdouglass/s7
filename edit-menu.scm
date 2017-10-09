;;; add some useful options to the Edit menu
;;;
;;; these used to be in the effects menu

(provide 'snd-edit-menu.scm)

(define edit-menu 1)


;;; -------- selection -> new file

(define selection->new
  (let ((+documentation+ "(selection-<new) saves the selection in a new file, then opens that file"))
    (lambda ()
      (and (selection?)
	   (let ((new-file-name (snd-tempnam)))
	     (save-selection new-file-name)
	     (open-sound new-file-name))))))

(add-to-menu edit-menu "Selection->new" selection->new 8) ;pos=8 puts this in the selection section in the Edit menu


;;; -------- cut selection -> new file

(define cut-selection->new
  (let ((+documentation+ "(cut-selection->new) saves the selection, deletes it, then opens the saved file"))
    (lambda ()
      (and (selection?)
	   (let ((new-file-name (snd-tempnam)))
	     (save-selection new-file-name)
	     (delete-selection)
	     (open-sound new-file-name))))))

(add-to-menu edit-menu "Cut selection->new" cut-selection->new 9)


;;; -------- append selection

(define append-selection
  (let ((+documentation+ "(append-selection) appends the current selection"))
    (lambda ()
      (if (selection?)
	  (insert-selection (framples))))))

(add-to-menu edit-menu "Append selection" append-selection 10)


;;; -------- make-stereofile
(define (make-stereofile)
  (let* ((ofile-name (file-name))
	 (old-sound (selected-sound))
	 (nsnd (new-sound (string-append ofile-name ".stereo") 2 (srate) (sample-type) (header-type))))
    (if (not nsnd)
	(begin
	  (display "Could not make new sound.")(newline))
	(begin
	  (insert-sound ofile-name 0 0 nsnd 0)
	  (insert-sound ofile-name 0 (if (> (channels old-sound) 1) 1 0) nsnd 1)))))

(add-to-menu edit-menu "Make Stereofile" make-stereofile)

;;; --------


(add-to-menu edit-menu #f #f)

;;; -------- trim front and back (goes by first or last mark)

(define trim-front
  (let ((+documentation+ "(trim-front) finds the first mark in each of the syncd channels and removes all samples before it")
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
	    (trim-front-one-channel 
	     (or (selected-sound) (car (sounds))) 
	     (or (selected-channel) 0)))))))

(add-to-menu edit-menu "Trim front" trim-front)

(define trim-back
  (let ((+documentation+ "(trim-back) finds the last mark in each of the syncd channels and removes all samples after it")
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
	    (trim-back-one-channel 
	     (or (selected-sound) (car (sounds))) 
	     (or (selected-channel) 0)))))))

(add-to-menu edit-menu "Trim back" trim-back)


;;; -------- crop (trims front and back)

(define* (crop-one-channel snd chn)
  (if (< (length (marks snd chn)) 2)
      (status-report "crop needs start and end marks" snd)
      (as-one-edit
       (lambda ()
	 (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
	      (let ((endpt (let ((ms (marks snd chn)))
			     (mark-sample (list-ref ms (- (length ms) 1))))))
		(delete-samples (+ endpt 1) (- (framples snd chn) endpt))))
       "crop-one-channel")))

(define crop
  (let ((+documentation+ "(crop) finds the first and last marks in each of the syncd channels and removes all samples outside them"))
    (lambda ()
      (let ((snc (sync)))
	(if (> snc 0)
	    (apply map
		   (lambda (snd chn)
		     (if (= (sync snd) snc)
			 (crop-one-channel snd chn)))
		   (all-chans))
	    (crop-one-channel 
	     (or (selected-sound) (car (sounds)))
	     (or (selected-channel) 0)))))))

(add-to-menu edit-menu "Crop" crop)


;;; -------- add these to the Edit menu, if possible

(when (and (not (provided? 'snd-gtk))
	   (provided? 'xm))
  (with-let (sublet *motif*)
    (define (for-each-child w func)
      ;; (for-each-child w func) applies func to w and its descendents
      (func w)
      (if (XtIsComposite w)
	  (for-each 
	   (lambda (n)
	     (for-each-child n func))
	   (cadr (XtGetValues w (list XmNchildren 0) 1)))))
      
    (let* ((edit-cascade (list-ref (menu-widgets) 2))
	   (edit-menu (cadr (XtGetValues edit-cascade (list XmNsubMenuId 0)))))
      (XtAddCallback edit-cascade XmNcascadingCallback 
		     (lambda (w c i)
		       (for-each-child 
			edit-menu
			(lambda (child)
			  (cond ((member (XtName child) '("Selection->new" "Cut selection->new" "Append selection") string=?)
				 (XtSetSensitive child (selection?)))
				((string=? (XtName child) "Crop")
				 (XtSetSensitive child (and (selected-sound)
							    (> (length (marks (selected-sound) (selected-channel))) 1))))
				((member (XtName child) '("Trim front" "Trim back") string=?)
				 (XtSetSensitive child (and (selected-sound)
							    (>= (length (marks (selected-sound) (selected-channel))) 1))))))))))))
