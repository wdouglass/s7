;;; provide pop-up help in the Files viewer
;;;   if use-gdbm is #t, any data associated with the file in the gdbm database will also be posted
;;;   the database name is defined by nb-database
;;;   the function (nb file note) adds note to the info currently associated with file
;;;   to remove a file's info, (unb file)
;;;   to clean non-existent file references out of the database, (prune-db)

(provide 'snd-nb.scm)

(define use-gdbm #f)
(define nb-database "nb.db")

(define nb 
  (let ((+documentation+ "(nb file note) adds 'note' to the info asociated with 'file'"))
    (lambda (file note)
      (let ((ptr (gdbm-open nb-database 'create)))
	(if (gdbm? ptr)
	    (let ((current-note (and (gdbm-exists? ptr file)
				     (gdbm-fetch ptr file))))
	      (gdbm-store! ptr 
			   file 
			   (if (string? current-note)
			       (string-append note (string #\newline) current-note)
			       note)
			   'replace)
	      (gdbm-close! ptr)))))))

(define unb 
  (let ((+documentation+ "(unb file) removes file's info from the nb (gdbm) data base"))
    (lambda (file)
      (let ((ptr (gdbm-open nb-database 'write)))
	(if (gdbm? ptr)
	    (begin
	      (gdbm-delete! ptr file)
	      (gdbm-close! ptr)))))))

(define prune-db
  (letrec ((+documentation+ "(prune-db) cleans up the nb (gdbm) data base by removing references to non-existent files")
	   (collect-files (lambda (ptr key files)
			    (if key
				(collect-files ptr (gdbm-next-key ptr key) (cons key files))
				files)))
	   (prune-file (lambda (ptr files)
			 (do ((ptr ptr)
			      (files files (cdr files)))
			     ((not (pair? files)))
			   (unless (file-exists? (car files))
			     (snd-print (format #f "pruning ~A" (car files)))
			     (gdbm-delete! ptr (car files)))))))
    (lambda ()
      (let ((ptr (gdbm-open nb-database 'read)))
	(if (gdbm? ptr)
	    (let ((files (collect-files ptr (gdbm-first-key ptr) ())))
	      (gdbm-close! ptr)
	      (if (pair? files)
		  (let ((ptr (gdbm-open nb-database 'write)))
		    (prune-file ptr files)
		    (gdbm-close! ptr)))))))))

(define nb-mouse-response-time 0)

(define files-popup-info
  (let ((+documentation+ "(files-popup-info type position name) is intended as a mouse-enter-label hook function. 
It causes a description of the file to popup when the mouse crosses the filename")

	(file-info (lambda (file)
		     ;; (file-info file) -> description (as a string) of file
		     (format #f "~A:  ~%  chans: ~D, srate: ~D, len: ~A~%  ~A ~A~A~%  written: ~A~A~A"
			     file
			     (channels file)
			     (srate file)
			     (let ((den (* (channels file) (srate file))))
			       (if (> den 0)
				   (format #f "~1,3F" (* 1.0 (/ (mus-sound-samples file) den)))
				   "unknown"))
			     (mus-header-type-name (mus-sound-header-type file))
			     (mus-sample-type-name (mus-sound-sample-type file))
			     (if (mus-sound-maxamp-exists? file)
				 (format #f "~%  maxamp: ~A" (mus-sound-maxamp file))
				 "")
			     (strftime "%d-%b %H:%M %Z" (localtime (mus-sound-write-date file)))
			     (let ((comment (mus-sound-comment file)))
			       (if (and (string? comment)
					(> (length comment) 0))
				   (format #f "~%  comment: ~A" comment)
				   ""))
			     (if (not (and use-gdbm
					   (file-exists? nb-database)))
				 ""
				 (let* ((ptr (gdbm-open nb-database 'read))
					(note (gdbm-fetch ptr file)))
				   (gdbm-close! ptr)
				   (if (string? note)
				       (format #f "~%~A" note)
				       ""))))))
	(region-viewer 2))
    (lambda (type position name)
      (set! nb-mouse-response-time (get-internal-real-time))
      (if (not (= type region-viewer))
	  (let ((info-exists (list-ref (dialog-widgets) 15)))
	    (info-dialog name (file-info name))
	    (let ((info-widget (list-ref (dialog-widgets) 15)))
	      (if (and info-widget
		       (not info-exists)) ; keep the help dialog from overlapping the files dialog
		  (let ((files-dialog (list-ref (dialog-widgets) 5)))
		    (let ((files-position (widget-position files-dialog))
			  (files-size (widget-size files-dialog)))
		      (set! (widget-position info-widget) (list (+ (car files-position) (car files-size) 10)
								(+ (cadr files-position) 10))))))))))))


(define (files-popdown-info)
  (let ((cur-time (get-internal-real-time)))
    (in 1000 (lambda ()
	       (if (> cur-time nb-mouse-response-time)
		   (hide-widget (list-ref (dialog-widgets) 15)))))))

(hook-push mouse-enter-label-hook (lambda (hook) (files-popup-info (hook 'type) #f (hook 'label))))
(hook-push mouse-leave-label-hook (lambda (hook) (files-popdown-info)))


