;;; Examples of mark-related functions

(provide 'snd-marks.scm)
(require snd-selection.scm snd-hooks.scm)

;;; Contents:
;;;     mark-name->id is a global version of find-mark
;;;     move-syncd-marks moves all syncd marks together
;;;     describe-mark shows mark history
;;;     click marks between start-sync and stop-sync to sync them together
;;;     synchronize sounds at a given mark
;;;     fit selection between marks, expanding via granulate (this needs some tweaking...)
;;;     pad-marks inserts silence before each in a list of marks
;;;     play-syncd-marks and play-between-marks
;;;     report-mark-names causes mark names to be posted in the minibuffer as a sound is played
;;;     snap-marks places marks at current selection boundaries
;;;     define-selection-via-marks selects the portion between two marks
;;;     snap-mark-to-beat forces dragged mark to end up on a beat
;;;     mark-explode splits a sound into a bunch of sounds based on mark placements
;;;     save-mark-properties sets up an after-save-state-hook function to save any mark-properties
;;;     mark-click-info is a mark-click-hook function that describes a mark and its properties


;;; -------- mark-name->id is a global version of find-mark

(define mark-name->id 
  (let ((+documentation+ "(mark-name->id name) is like find-mark but searches all currently accessible channels"))
    (lambda (name)
      (call-with-exit
       (lambda (return)
	 (for-each
	  (lambda (snd)
	    (do ((chn 0 (+ 1 chn)))
		((= chn (channels snd)))
	      (let ((mark (find-mark name snd chn)))
		(if (mark? mark) 
		    (return mark)))))
	  (sounds))
	 'no-such-mark)))))


;;; -------- move-syncd-marks moves all syncd marks together

(define move-syncd-marks 
  (let ((+documentation+ "(move-syncd-marks sync diff) moves all marks sharing sync by diff samples"))
    (lambda (synch diff)
      (for-each (lambda (m)
		  (set! (mark-sample m) (+ (mark-sample m) diff)))
		(syncd-marks synch)))))


;;; -------- describe-mark shows mark history

(define describe-mark 
  (let ((+documentation+ "(describe-mark mark) returns a description of the movements of mark over the channel's edit history"))
    (lambda (id)
      (let ((mark-setting (catch 'no-such-mark (lambda () (mark-home id)) (lambda args #f))))
	(if (not mark-setting)
	    ;; not an active mark, so go scrounging for it
	    ;;   we're looking at every edit of every channel
	    (set! mark-setting (call-with-exit
				(lambda (return)
				  (for-each
				   (lambda (snd)
				     (do ((chn 0 (+ 1 chn)))
					 ((= chn (channels snd)))
				       (let ((max-edits (apply + (edits snd chn))))
					 (do ((ed 0 (+ 1 ed)))
					     ((> ed max-edits))
					   (if (member id (marks snd chn ed))
					       (return (list snd chn)))))))
				   (sounds))
				  #f))))
	(if (pair? mark-setting)
	    (let ((snd (car mark-setting))
		  (chn (cadr mark-setting)))
	      (let ((max-edits (apply + (edits snd chn)))
		    (descr ())
		    (header (list id sound: snd (short-file-name snd) channel: chn)))
		(do ((i max-edits (- i 1)))
		    ((< i 0))
		  (set! descr (cons (and (member id (marks snd chn i)) (mark-sample id i)) descr)))
		(cons header descr)))
	    (error 'no-such-mark (list "describe-mark" id)))))))


;;; -------- click marks between start-sync and stop-sync to sync them together
;;;  (easier than calling mark-sync over and over by hand)

(define mark-sync-number 0)
(define (start-sync) (set! mark-sync-number (+ (mark-sync-max) 1)))
(define (stop-sync) (set! mark-sync-number 0))
(define (click-to-sync id) (set! (sync id) mark-sync-number) #f)
					;(hook-push mark-click-hook (lambda (hook) (click-to-sync (hook 'id))))


;;; -------- syncronize sounds at a given mark

(define syncup
  (let ((+documentation+ "(syncup marks) pads the channels with zeros so that all the given marks occur at the same time"))
    (lambda ids
      (let* ((samps (map mark-sample ids))
	     (max-samp (apply max samps)))
	(do ((lst-ids ids (cdr lst-ids))
	     (lst-samps samps (cdr lst-samps)))
	    ((not (pair? lst-ids)))
	  (if (< (car lst-samps) max-samp)
	      (let ((nsamps (- max-samp (car lst-samps)))
		    (snd-chn (mark-home (car lst-ids))))
		(insert-samples 0 nsamps (make-float-vector nsamps) (car snd-chn) (cadr snd-chn)))))))))


;;; -------- fit selection between marks, expanding via granulate (this needs some tweaking...)

(define fit-selection-between-marks 
  (let ((+documentation+ "(fit-selection-between-marks m1 m2) fits (and mixes) the current selection (via granulate) between the given marks"))
    (lambda (m1 m2)
      (let ((m1-samp (mark-sample m1))
	    (m2-samp (mark-sample m2))
	    (m1-home (mark-home m1))
	    (m2-home (mark-home m2)))
	(if (not (selection?))
	    (error 'no-active-selection))
	(if (not (equal? m1-home m2-home))
	    (snd-print (format #f "~A is in ~A[~A] but ~A is in ~A[~A]?" 
			       m1 (car m1-home) (cadr m1-home)
			       m2 (car m2-home) (cadr m2-home)))
	    (let ((selection-samps (selection-framples)))
	      (let ((mark-samps (- m2-samp m1-samp))
		    (reg-data (let ((data (make-float-vector selection-samps))
				    (rd (make-sampler (selection-position))))
				(do ((i 0 (+ 1 i)))
				    ((= i selection-samps))
				  (set! (data i) (rd)))
				data))
		    (inctr 0))
		(if (= mark-samps selection-samps)
		    (map-channel (lambda (y) 
				   (let ((val (+ y (reg-data inctr))))
				     (set! inctr (+ 1 inctr))
				     val))
				 m1-samp mark-samps (car m1-home) (cadr m1-home))
		    (let ((gr (make-granulate :expansion (/ mark-samps selection-samps)
					      :input (lambda (dir)
						       (if (or (>= inctr selection-samps)
							       (< inctr 0))
							   0.0
							   (let ((val (reg-data inctr)))
							     (set! inctr (+ inctr dir))
							     val))))))
		      (map-channel (lambda (y)
				     (+ y (granulate gr)))
				   m1-samp mark-samps (car m1-home) (cadr m1-home)))))))))))


;;; -------- pad-marks inserts silence before each in a list of marks

(define pad-marks 
  (let ((+documentation+ "(pad-marks marks secs) inserts secs seconds of silence before each mark"))
    (lambda (ids secs)
      (let* ((silence-length (floor (* secs (srate))))
	     (silence-samps (make-float-vector silence-length)))
	(as-one-edit
	 (lambda ()
	   (for-each 
	    (lambda (n)
	      (let ((samp (max 0 (- (mark-sample n) 1)))
		    (home (mark-home n)))
		(insert-samples samp silence-length silence-samps (car home) (cadr home))))
	    ids)))))))


;;; -------- play-syncd-marks

(define play-syncd-marks 
  (let ((+documentation+ "(play-syncd-marks sync) starts playing from all marks sharing sync"))
    (lambda (synch)
      (let ((chans 1)
	    (rate 22050))
	(for-each
	 (lambda (m)
	   (let* ((sound (car (mark-home m)))
		  (channel (cadr (mark-home m)))
		  (new-player (make-player sound channel)))
	     (add-player new-player (mark-sample m))
	     (set! chans (max chans (+ 1 channel)))
	     (set! rate (max rate (srate sound)))))
	 (syncd-marks synch))
	(start-playing chans rate)))))

(define play-between-marks
  (let ((+documentation+ "(play-between-marks ...) plays the portion between the marks (searching for plausible default marks)"))
    (lambda args
      (let* ((snd (or (selected-sound) (car (sounds))))
	     (chn (or (selected-channel) 0))
	     (m1 (if (pair? args)
		     (car args)
		     (let find-mark ((ms (marks snd chn)))
		       (if (null? ms)
			   (begin
			     (snd-print ";no marks in current window?")
			     #f)
			   (if (>= (mark-sample (car ms)) (left-sample snd chn))
			       (car ms)
			       (find-mark (cdr ms)))))))
	     (m2 (and (mark? m1)
		      (if (and (pair? args)
			       (pair? (cdr args)))
			  (cadr args)
			  (let find-another-mark ((ms (marks snd chn)))
			    (if (null? ms)
				(begin
				  (snd-print ";no second mark?")
				  #f)
				(if (> (mark-sample (car ms)) (mark-sample m1))
				    (car ms)
				    (find-another-mark (cdr ms)))))))))
	(if (and (mark? m1)
		 (mark? m2))
	    (let ((pos1 (mark-sample m1))
		  (pos2 (mark-sample m2)))
	      (let ((beg (min pos1 pos2))
		    (end (max pos1 pos2)))
		(play (car (mark-home m1)) 
		      :channel (cadr (mark-home m1))
		      :start beg 
		      :end end))))))))


;;; -------- report-mark-names causes mark names to be posted in the minibuffer as a sound is played

(define report-mark-names
  (let ((+documentation+ "(report-mark-names) causes mark names to be printed as they are passed while playing"))
    (lambda ()
      (hook-push start-playing-hook 
		 (lambda (snd)
		   (let ((report-mark-names-play-hook
			   (let ((marklist (marks snd 0)))
			     (let ((samplist (map mark-sample marklist))
				   (samp 0))
			       (lambda (hook)
				 (set! samp (+ samp (hook 'size)))
				 (if (and (pair? samplist)
					  (>= samp (car samplist)))
				     (begin
				       (status-report (mark-name (car marklist)) snd)
				       (set! marklist (cdr marklist))
				       (set! samplist (cdr samplist)))))))))
		     (letrec ((report-mark-names-stop-playing-hook 
			       (lambda (hook)
				 (status-report "" (hook 'snd))
				 (hook-remove play-hook report-mark-names-play-hook)
				 (hook-remove stop-playing-hook report-mark-names-stop-playing-hook))))
		       (hook-push stop-playing-hook report-mark-names-stop-playing-hook)
		       (hook-push play-hook report-mark-names-play-hook)
		       #f)))))))


;;; -------- snap-marks

(define snap-marks
  (let ((+documentation+ "snap-marks places marks at current selection boundaries"))
    (lambda ()
      (let ((ms ()))
	(if (selection?)
	    (for-each 
	     (lambda (select)
	       (let ((pos (apply selection-position select))
		     (len (apply selection-framples select)))
		 (set! ms (cons (apply add-mark (+ pos len) select) 
				(cons (apply add-mark pos select) ms)))))
	     (selection-members)))
	(reverse ms)))))


;;; -------- define-selection-via-marks

(define define-selection-via-marks 
  (let ((+documentation+ "(define-selection-via-marks m1 m2) defines the current selection to lie between the marks given"))
    (lambda (m1 m2)
      (let ((m1sc (mark-home m1))
	    (m2sc (mark-home m2)))
	(if (not (equal? m1sc m2sc))
	    (snd-error "define-selection-via-marks assumes the marks are in the same channel")
	    (let ((beg (min (mark-sample m1) (mark-sample m2)))
		  (end (max (mark-sample m1) (mark-sample m2)))
		  (snd (car m1sc))
		  (chn (cadr m1sc)))
	      (if (selection?)
		  (set! (selection-member? #t) #f)) ; clear entire current selection, if any
	      (set! (selection-member? snd chn) #t)
	      (set! (selection-position snd chn) beg)
	      (set! (selection-framples snd chn) (- (+ end 1) beg))))))))


;;; -------- snap-mark-to-beat

(define snap-mark-to-beat
  (let ((+documentation+ "(snap-mark-to-beat) ensures that when a mark is dragged, its released position is always on a beat"))
    (lambda ()
      (hook-push mark-hook 
		 (lambda (hook)
		   (let ((mrk (hook 'id))
			 (snd (hook 'snd))
			 (chn (hook 'chn))
			 (reason (hook 'reason))
			 (mark-release 4))			   
		     (if (= reason mark-release)
			 (let* ((samp (mark-sample mrk))
				(bps (/ (beats-per-minute snd chn) 60.0))
				(sr (srate snd))
				(beat (floor (/ (* samp bps) sr))))
			   (let ((lower (floor (/ (* beat sr) bps)))
				 (higher (floor (/ (* (+ 1 beat) sr) bps))))
			     (set! (mark-sample mrk)
				   (if (< (- samp lower) (- higher samp))
				       lower
				       higher)))))))))))

;;; -------- mark-explode
;;;
;;; write out each section of a file between marks as a separate file

(define mark-explode 
  (let ((+documentation+ "(mark-explode header-type sample-type) splits a sound into a bunch of sounds based on mark placements"))
    (lambda* ((htype mus-next) (dformat mus-bfloat))
      (let ((start 0)
	    (file-ctr 0)
	    (snd (or (selected-sound) (car (sounds)))))
	(for-each
	 (lambda (mark)
	   (let ((end (mark-sample mark)))
	     (if (> end start)
		 (let ((filename (format #f "mark-~D.snd" file-ctr))
		       (dur (- end start)))
		   (set! file-ctr (+ 1 file-ctr))
		   (do ((i 0 (+ 1 i)))
		       ((= i (channels snd)))
		     (set! (selection-member? snd i) #t)
		     (set! (selection-position snd i) start)
		     (set! (selection-framples snd i) dur))
		   (save-selection filename :header-type htype :sample-type dformat :srate (srate snd))
		   (do ((i 0 (+ 1 i)))
		       ((= i (channels snd)))
		     (set! (selection-member? snd i) #f))))
	     (set! start end)))
	 (car (marks snd)))
	(update-time-graph snd)))))


;;; -------- save mark property lists

(define save-mark-properties
  (let ((+documentation+ "(save-mark-properties) sets up an after-save-state-hook function to save any mark-properties"))
    (lambda ()
      (hook-push after-save-state-hook 
		 (lambda (hook)
		   (let ((fd (open-output-file (hook 'name) "a")))
		     (format fd "~%~%;;; from remember-mark-properties in marks.scm~%")
		     (format fd "(if (not (defined? 'mark-property)) (load \"marks.scm\"))~%")
		     (for-each 
		      (lambda (snd-m)
			(for-each 
			 (lambda (chn-m)
			   (for-each
			    (lambda (m)
			      (let ((mp (mark-properties m)))
				(if (pair? mp)
				    (let ((mhome (mark-home m))
					  (msamp (mark-sample m)))
				      (format fd "(let ((s (find-sound ~S)))~%" (file-name (car mhome)))
				      (format fd "  (if (sound? s)~%")
				      (format fd "      (let ((m (find-mark ~A s ~A)))~%" msamp (cadr mhome))
				      (format fd "        (if (mark? m)~%")
				      (format fd "            (set! (mark-properties m) '~A)))))~%" mp)))))
			    chn-m))
			 snd-m))
		      (marks))
		     (close-output-port fd)))))))


(define mark-click-info 
  (let ((+documentation+ "(mark-click-info n) is a mark-click-hook function that describes a mark and its properties"))
    (lambda (hook)
      (let ((n (hook 'id)))
	(help-dialog "Mark Help"
		     (format #f "Mark ~A~A:~%  sample: ~D = ~,3F secs~A~A"
			     n 
			     (let ((name (mark-name n)))
			       (if (> (length name) 0)
				   (format #f " (~S)" name)
				   ""))
			     (mark-sample n)
			     (* 1.0 (/ (mark-sample n) (srate (car (mark-home n)))))
			     (if (= (sync n) 0)
				 ""
				 (format #f "~%  sync: ~A" (sync n)))
			     (let ((props (mark-properties n)))
			       (if (pair? props)
				   (format #f "~%  properties: '~A" props)
				   ""))))
	(set! (hook 'result) #t)))))


#|
;;; this code saves mark info in the sound file header, and reads it back in when the sound is later reopened

(define (eval-header sndf)
  (and (string? (comment sndf))
       (catch #t
	 (lambda ()
	   (eval-string (comment sndf)))
	 (lambda args #f))))

(define (marks->string sndf)
  (let ((str (format #f "(require snd-marks.scm)~%(let ((m #f))~%"))
	(chan 0))
    (for-each
     (lambda (chan-marks)
       (for-each 
	(lambda (m)
	  (set! str 
		(string-append str 
			       (format #f
				       "  (set! m (add-mark ~A #f ~D ~A ~D))~%" 
				       (mark-sample m)
				       chan
				       (if (and (string? (mark-name m))
						(> (length (mark-name m)) 0))
					   (format #f "~S" (mark-name m))
					   #f)
				       (sync m))))
	  (if (pair? (mark-properties m))
	      (set! str
		    (string-append str 
				   (format #f
					   "  (set! (mark-properties m) '~A)~%"
					   (mark-properties m))))))
	chan-marks)
       (set! chan (+ 1 chan)))
     (marks sndf))
    (string-append str (format #f "  m)~%"))))

(hook-push output-comment-hook (lambda (hook) (set! (hook 'result) (marks->string (selected-sound)))))
(hook-push after-open-hook (lambda (hook) (set! (hook 'result) (eval-header (hook 'snd)))))
|#

#|
;; from snd11?
(define (delete-between-marks)
  (let ((mx (marks 0 0)))
    (if (and (pair? mx)
	     (pair? (cdr mx))) ; there are at least 2 marks
	(let ((pos (cursor 0 0))
	      (deb #f)
	      (fin #f))
	  (for-each
	   (lambda (m)
	     (if (<= (mark-sample m) pos)
		 (set! deb (mark-sample m)))
	     (if (and (not fin)
		      (> (mark-sample m) pos))
		 (set! fin (mark-sample m))))
	   (sort! mx (lambda (a b) 
		       (< (mark-sample a) (mark-sample b)))))
	  (when (and deb fin)
	    (let ((dur (- fin deb)))
	      (apply for-each
		     (lambda (s c)
		       (delete-samples deb dur s c))
		     (all-chans))))))))
|#
