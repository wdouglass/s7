;;; easy edit
;;;
;;;version 0.8
;;;keybindings and functions to make sound editing as easy and convenient
;;;as pressing 1 2 3
;;;1 set mark
;;;2 move mark backward
;;;3 move mark backward by a fraction
;;;C-2 move mark forward
;;;C-3 move mark forward by a fraction
;;;p set mark fixed and play (important: you have to use this key to finalize the setting of the  start mark )
;;;f fade
;;;c crop delete the part before and after the marks
;;;C-1 toggles start and end status 
;;; keys have multiple actions assigned depending on the status of the editing process
;;; p is a customized playfunction
;;; P (thats Shift P) plays the end of the track and sets the system for setting an end mark
;;; after having set the start mark pressing P (Shift-P) and then M-2 and M-3 is a good approach to quickly get to the desired area.
;;; there are some more keybindings, that i commented out.
;;; you can suit them to your taste.
;;; for a more in depth description have a look at 
;;; http://www.users.startplus.de/rawdlite/sound/HOWTO_record_from_radio_to_mp3.html
;;; enjoy
;;; tom@tomroth.de

(provide 'snd-edit123.scm)
(require snd-selection.scm)

(define status 0)
(define curpos 0)
(define playing? 0)

(define open-next-file-in-directory
  (let ((last-file-opened #f)
	(current-directory #f)
	(current-sorted-files #f)
	(directory-from-path (lambda (curfile)
			       (do ((last-slash 0)
				    (i 0 (+ 1 i)))
				   ((= i (length curfile))
				    (substring curfile 0 last-slash))   
				 (if (char=? (curfile i) #\/)
				     (set! last-slash i))))))

    (define find-next-file
      (let ((file-from-path (lambda (curfile)
			      (do ((last-slash 0)
				   (i 0 (+ 1 i)))
				  ((= i (length curfile))
				   (substring curfile (+ 1 last-slash)))   
				(if (char=? (curfile i) #\/)
				    (set! last-slash i))))))
	(lambda ()
	  ;; find the next file in the sorted list, with wrap-around
	  (let ((choose-next (not (string? last-file-opened)))
		(just-filename (file-from-path last-file-opened)))
	    (call-with-exit
	     (lambda (return)
	       (for-each
		(lambda (file)
		  (if choose-next
		      (return file)
		      (if (string=? file just-filename)
			  (set! choose-next #t))))
		current-sorted-files)
	       ;; if we get here we wrapped around
	       (car current-sorted-files)))))))
    
    (define (get-current-files dir)
      (set! current-directory dir)
      (set! current-sorted-files (sort! (sound-files-in-directory dir) string<?)))
    
    (define (get-current-directory hook)
      (let ((filename (hook 'name)))
	(set! last-file-opened filename)
	(display last-file-opened)
	(let ((new-path (directory-from-path (mus-expand-filename filename))))
	  (if (not (equal? current-directory new-path))
	      (get-current-files new-path)))))
    
    (lambda ()
      (if (not (member get-current-files (hook-functions open-hook)))
	  (hook-push open-hook get-current-directory))
      (if (and (not (string? last-file-opened))
	       (pair? (sounds)))
	  (set! last-file-opened (file-name (or (selected-sound)
						(car (sounds))))))
      (if (not current-directory)
	  (get-current-files
	   (if (null? (sounds))
	       (getcwd)
	       (directory-from-path last-file-opened))))
      (if (null? current-sorted-files)
	  (error 'no-such-file (list "open-next-file-in-directory" current-directory))
	  (let ((next-file (find-next-file)))
	    (if (find-sound next-file)
		(error 'file-already-open (list "open-next-file-in-directory" next-file))
		(begin
		  (if (pair? (sounds))
		      (close-sound (or (selected-sound)  ; not sure this is what you want -- closes current file
				       (car (sounds)))))
		  (open-sound next-file)))))
      #t)))

(define (click-middle-button-to-open-next-file-in-directory)
  (hook-push mouse-click-hook
	     (lambda (hook)
	       (if (= (hook 'button) 2)
		   (set! (hook 'result) (open-next-file-in-directory))))))

;; or you could bind this (i.e. "(open-next-file-in-directory)") to some key.



(hook-push after-open-hook
	   (lambda (hook)
	     (set! status 0)
	     ))

(hook-push start-playing-hook 
	   (lambda (hook)
	     (set! playing? 1)))


(hook-push stop-playing-hook 
	   (lambda (hook)
	     (set! playing? 0)))

(define (eos )
  (set! (cursor) (+ (selection-position) (selection-framples))))

(define (mark-named name)
  (select-channel 0)
  (set! (mark-name (add-mark (cursor) )) name)
  )

(define (goto-named-mark name)
  (select-channel 0)
  (set! (cursor) (cond ((find-mark name) => mark-sample)
		       (else (error 'no-such-mark "can't find mark named ~S" name))))
  )

(define (delete-named-mark name)
  (select-channel 0)
  (cond ((find-mark name) => delete-mark)))

(define (my-play-selection pos1 pos2)
  (stop-playing)
  (make-selection pos1 pos2)
					;(set! (x-bounds) 
					;    (list (/ (selection-position) (srate))
					;          (/ (+ (selection-position) (selection-framples)) (srate))))
  (play (selection)))

(define (test-mark-forw name  length)
  (stop-playing)
  (select-channel 0)
  (goto-named-mark name)
  (make-selection (cursor)  (+ (cursor) length))
  (play (selection)))

(define (test-mark-backw name  length)
  (stop-playing)
  (select-channel 0)
  (goto-named-mark name)
  (make-selection (- (cursor) length) (cursor)  )
  (play (selection)))



(define (move-start dif length)
  (stop-playing)
  (select-channel 0)
  (goto-named-mark "start")
  (delete-named-mark "start")
  (set! (cursor) (+ (cursor) dif))
  (mark-named "start")
  (make-selection (cursor)  (+ (cursor) length))
  (play (selection)))

(define (move-end dif length)
  (stop-playing) 
  (select-channel 0)
  (goto-named-mark "end")
  (delete-named-mark "end")
  (set! (cursor) (+ (cursor) dif))
  (mark-named "end")
  (make-selection (- (cursor) length) (cursor))
  (play (selection)))



(define (my-play-selection-forw dif length)
  (stop-playing)
  (select-channel 0)
  (set! (cursor) (+ (cursor) dif))
  (make-selection (cursor)  (+ (cursor) length))
					;(set! (x-bounds) 
					;    (list (/ (selection-position) (srate))
					;          (/ (+ (selection-position) (selection-framples)) (srate))))
  (play (selection)))



(define (my-play-selection-backw dif length)
  (stop-playing)
  (select-channel 0)
  (set! (cursor) (- (cursor) dif))
  (make-selection (cursor)  (+ (cursor) length))
  (play (selection)))



(define (forward-selection)
  (if (not (selection?)) (make-selection (cursor)  (+ (cursor) 20000)))
  (if (< (selection-framples) 2000) (make-selection  (- (cursor) 2000) (cursor)))
					; is cursor inside of selection ?
  (if (not (<= (selection-position) (cursor) (+ (selection-position) (selection-framples))))
      (begin
	(make-selection (cursor)  (+ (cursor) (selection-framples)))
	(stop-playing)
	(eos)))
  (if (>= (+ (selection-position) (selection-framples)) (cursor) (selection-position))
      (begin  
	(stop-playing)
	(eos)
	(make-selection (cursor)  (+ (cursor) (selection-framples)))))
  (play (selection)))


(define (backward-selection)
  (if (not (selection?)) (make-selection  (- (cursor) 20000) (cursor)))
  (if (< (selection-framples) 2000) (make-selection  (- (cursor) 2000) (cursor)))
  (if (not (<= (selection-position) (cursor) (+ (selection-position) (selection-framples))))
      (begin
	(make-selection (cursor)  (- (cursor) (selection-framples)))
	(stop-playing)
	(set! (cursor) (selection-position))))
  
  (if (>= (+ (selection-position) (selection-framples)) (cursor) (selection-position))
      (begin  
	(stop-playing)
	(set! (cursor) (selection-position))))
  (make-selection  (- (cursor) (selection-framples)) (cursor) )
  (play (selection)))


(define (mark-start  length)
  (select-channel 0)
  (if (find-mark "start")
      (delete-named-mark "start"))
  (mark-named "start")
  (stop-playing)
  (goto-named-mark "start")
  (set! status 1)
  (make-selection (cursor)  (+ (cursor) length))
  (stop-playing)
  (key (char->integer #\t) 4)
  (play (selection)))

(define (mark-end  length)
  (select-channel 0)
  (if (find-mark "end")
      (delete-named-mark "end")
      )
  (mark-named "end")
  (stop-playing)
  (goto-named-mark "end")
  (set! status 3)
  (make-selection  (- (cursor) length) (cursor) )
  (key (char->integer #\t) 4)
  (play (selection)))

(define (stop-song)
  (set! curpos (cursor))
  (stop-playing)
  (set! (cursor) curpos))

(define (play-song)
  (stop-playing)
  (select-channel 0)
					;(if (eq? status 1)(set! status 2))
					;(if (eq? status 3)(set! status 0))
  (let ((old-tracking (with-tracking-cursor)))
    (set! (with-tracking-cursor) #t)
    (hook-push stop-playing-hook 
               (lambda (hook)
                 (set! (with-tracking-cursor) old-tracking)
		 (set! (channel-style (hook 'snd)) channels-superimposed))))
  (play (selected-sound) :start (cursor)))

(define (play-end)
  (key (char->integer #\>) 1)
  (stop-playing)
  (key (char->integer #\t) 4)
  (set! status 2)
  (set! (cursor) (- (cursor) 100000))
  (play (selected-sound) :start (cursor))
  )

(define (toggle-play)
  (lambda () (case playing?
	       ((1) (stop-playing))
	       ((0) (play))
	       )))

					;double-selection
(define (double-selection)
  (set! (cursor) (selection-position))
  (make-selection (selection-position) (+ (selection-position)(*  (selection-framples) 2)))
  )


					;half-selection
(define (half-selection)
  (set! (cursor) (selection-position))
  (make-selection (selection-position) (+ (selection-position)(/  (selection-framples) 2)))
  )

					;play-selection



					; to lazy to code ;-)

(define (my_crop)
  (select-channel 0)
  (if (find-mark "start")
      (begin
	(key (char->integer #\<) 4)
	(key (char->integer #\ ) 4)
	(key (char->integer #\j) 4)
	(key (char->integer #\d) 0)
	))
  (if (find-mark "end")
      (begin
	(key (char->integer #\j) 4)
	(key (char->integer #\ ) 4)
	(key (char->integer #\>) 5)
	(key (char->integer #\d) 0)
	))
  (key (char->integer #\<) 0)
  )

(define (my_save)
  (save-sound )
  (open-next-file-in-directory)
  )

(bind-key (char->integer #\d) 0  delete-selection)
(bind-key (char->integer #\s) 0  my_save)
(bind-key (char->integer #\c) 0  my_crop)
(bind-key (char->integer #\n) 0  open-next-file-in-directory)
(bind-key (char->integer #\ ) 8  stop-song)
(bind-key (char->integer #\c) 0  my_crop)
(bind-key (char->integer #\x) 0  half-selection)
(bind-key (char->integer #\X) 1  double-selection)
(bind-key (char->integer #\y) 0  double-selection);german version
(bind-key (char->integer #\z) 0  double-selection);english version
(bind-key (char->integer #\^) 0 (lambda () (my-play-selection-forw 50000 50000)))
(bind-key (char->integer #\^) 4 (lambda () (my-play-selection-backw 50000 50000)))
(bind-key (char->integer #\t) 8 (lambda () (play (selection))))
(bind-key (char->integer #\p) 0 play-song)
(bind-key (char->integer #\P) 1 play-end)
(bind-key (char->integer #\p) 8 toggle-play)
(bind-key (char->integer #\3) 8 forward-selection)
(bind-key (char->integer #\2) 8 backward-selection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; status 
;;;;             0 = new
;;;              1 = start is set
;;;              2 = playing
;;;              3 = end is set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key #xFFBE 0 (lambda () (case status
				((0 1) (mark-start 50000))
				((2) (mark-end     50000))
				((3) (mark-end     40000))
				(else (set! status 0))
				)))


(bind-key (char->integer #\1) 0 (lambda () (case status
					     ((0 1) (mark-start 50000))
					     ((2)   (mark-end   50000))
					     ((3)   (mark-end   40000))
					     (else  (set! status 0))
					     )))

(bind-key (char->integer #\1) 4 (lambda () (case status
					     ((0) (mark-end 50000))
					     ((1) (set! status 3))
					     ((2) (mark-start   50000))
					     ((3) (set! status 1))
					     (else (set! status 0))
					     )))

(bind-key (char->integer #\1) 8 (lambda () (set! status (case status ((0) 2) ((1) 3) ((3) 1) (else 0)))))

(bind-key (char->integer #\2) 4 (lambda () (case status
					     ((0) (mark-start 30000))
					     ((1) (move-start 2000 50000))
					     ((2) (mark-end   50000))
					     ((3) (move-end 2000 50000))
					     (else (set! status 0))
					     )))

(bind-key (char->integer #\2) 0 (lambda ()(case status
					    ((0) (mark-start 30000))
					    ((1) (move-start -2000 50000))
					    ((2) (mark-end   50000))
					    ((3) (move-end -2000 50000))
					    (else (set! status 0))
					    )))

(bind-key (char->integer #\3) 4 (lambda () (case status
					     ((0) (mark-start 20000))
					     ((1) (move-start 300 20000))
					     ((2) (mark-end   20000))
					     ((3) (move-end 300 20000))
					     (else (set! status 0))
					     )))

(bind-key (char->integer #\3) 0 (lambda () (case status
					     ((0) (mark-start 20000))
					     ((1) (move-start -300 20000))
					     ((2) (mark-end   20000))
					     ((3) (move-end -300 20000))
					     (else (set! status 0))
					     )))

(bind-key (char->integer #\p) 4 toggle-play)
(bind-key (char->integer #\p) 0 play-song)
(bind-key (char->integer #\P) 1 play-end)

					; f fade in or out  selection
(bind-key (char->integer #\f) 0  (lambda()  (case status
					      ((0) (mark-start 50000))
					      ((1) (env-selection '(0 0  2 1)))
					      ((2) (mark-end   50000))
					      ((3) (env-selection '(0 1  2 0)))
					      (else (set! status 0))
					      ))) 

(bind-key (char->integer #\t) 0 (lambda () (case status
					     
					     ((1) (goto-named-mark "start") (my-play-selection (cursor) (+ (cursor) 100000)))
					     
					     
					     ((3)  (goto-named-mark "end") (my-play-selection (- (cursor) 100000) (cursor)))
					;				(else (status-report "status:" (status) ) )
					     )
					)
	  )
					;  change status



					; + zoom in 
(bind-key (char->integer #\+) 0 (lambda () (set! (x-bounds) 
						 (list (/ (selection-position) (srate))
						       (/ (+ (selection-position) (selection-framples)) (srate)))) 
					(cursor-on-left)
					)
	  )

					; - zoom out
(bind-key (char->integer #\-) 0 (lambda () (let ((curs (cursor)))
					     (set! (x-bounds) (list 0 5))
					     (set! (cursor) curs))
					)
	  )

