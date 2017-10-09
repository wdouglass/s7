;;; -------- auto-save 

(provide 'snd-autosave.scm)

(define auto-save-interval 60.0) ;seconds between auto-save checks
(define auto-saving #f)

(define cancel-auto-save
  (let ((+documentation+ "(cancel-auto-save) turns off the auto-save mechanism"))
    (lambda ()
      (set! auto-saving #f))))

(define auto-save
  (let ((+documentation+ "(auto-save) starts watching files, automatically saving backup copies as edits accumulate")

	(auto-save-temp-name 
	 (lambda (snd)
	   (string-append (if (and (string? *temp-dir*)
				   (> (length *temp-dir*) 0))
			      (string-append *temp-dir* "/")
			      "")
			  "#" (short-file-name snd) "#")))
      
	(clear-unsaved-edits 
	 (lambda (snd)
	   (set! (sound-property 'auto-save snd) 0))))
      
    (let ((auto-save-open-func 
	   (lambda (snd)
	     (let ((temp-file (auto-save-temp-name snd)))
	       (if (and (file-exists? temp-file)
			(< (file-write-date (file-name snd)) (file-write-date temp-file)))
		   (snd-warning (format #f "auto-saved version of ~S (~S) is newer"
					(short-file-name snd)
					temp-file)))
	       (do ((i 0 (+ 1 i)))
		   ((= i (channels snd)))
		 (if (null? (hook-functions (edit-hook snd i)))
		     (hook-push (edit-hook snd i) (lambda (hook) 
						    (let ((snd (hook 'snd)))
						      (set! (sound-property 'auto-save snd) (+ 1 (sound-property 'auto-save snd))))))))		       
	       (clear-unsaved-edits snd))))
      
	  (auto-save-done 
	   (lambda (snd)
	     (let ((temp-file (auto-save-temp-name snd)))
	       (if (file-exists? temp-file)
		   (delete-file temp-file))
	       (clear-unsaved-edits snd)))))
      
      (letrec ((auto-save-func
		(lambda ()
		  (if auto-saving
		      (begin
			(for-each (lambda (snd)
				    (if (cond ((sound-property 'auto-save snd) => positive?) (else #f))
					(let ((save-name (auto-save-temp-name snd)))
					  (status-report (string-append "auto-saving as " save-name "...") snd)
					  (in 3000 (lambda () (status-report "" snd)))
					  (save-sound-as save-name snd)
					  (clear-unsaved-edits snd))))
				  (sounds))
			(in (floor (* 1000 auto-save-interval)) auto-save-func))))))
	
	(lambda ()
	  (if (not (member auto-save-done (hook-functions close-hook)))
	      (begin
		(for-each auto-save-open-func (sounds))
		(hook-push after-open-hook (lambda (hook) (auto-save-open-func (hook 'snd))))
		(hook-push close-hook (lambda (hook) (auto-save-done (hook 'snd))))
		(hook-push save-hook (lambda (hook) (auto-save-done (hook 'snd))))
		(hook-push exit-hook (lambda (hook) (for-each auto-save-done (sounds))))))
	  (set! auto-saving #t)
	  (in (floor (* 1000 auto-save-interval)) auto-save-func))))))

(auto-save)
