;;; selection.scm -- selection-related functions
;;;
;;;   swap-selection-channels
;;;   replace-with-selection
;;;   selection-members
;;;   make-selection
;;;   filter-selection-and-smooth
;;;   with-temporary-selection

(provide 'snd-selection.scm)

(if (not (defined? 'all-chans))
    (define (all-chans)
      (let ((sndlist ())
	    (chnlist ()))
	(for-each (lambda (snd)
		    (do ((i (- (channels snd) 1) (- i 1)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))



;;; -------- swap selection chans

(define swap-selection-channels
  (let ((+documentation+ "(swap-selection-channels) swaps the currently selected data's channels")
	(find-selection-sound 
	 (lambda (not-this)
	   (let ((scs (all-chans)))
	     (call-with-exit
	      (lambda (return)
		(map 
		 (lambda (snd chn)
		   (if (and (selection-member? snd chn)
			    (or (null? not-this)
				(not (equal? snd (car not-this)))
				(not (= chn (cadr not-this)))))
		       (return (list snd chn))))
		 (car scs)
		 (cadr scs))))))))
    (lambda ()
      (if (not (selection?))
	  (error 'no-active-selection "swap-selection-channels needs a selection")
	  (if (not (= (selection-chans) 2))
	      (error 'wrong-number-of-channels "swap-selection-channels needs a stereo selection")
	      (let* ((snd-chn0 (find-selection-sound ()))
		     (snd-chn1 (find-selection-sound snd-chn0)))
		(if snd-chn1
		    (swap-channels (car snd-chn0) (cadr snd-chn0) 
				   (car snd-chn1) (cadr snd-chn1)
				   (selection-position)
				   (selection-framples))
		    (error 'wrong-number-of-channels "swap-selection-channels needs two channels to swap"))))))))


;;; -------- replace-with-selection

(define replace-with-selection
  (let ((+documentation+ "(replace-with-selection) replaces the samples from the cursor with the current selection"))
    (lambda ()
      (let ((beg (cursor))
	    (len (selection-framples)))
	(insert-selection beg) ; put in the selection before deletion, since delete-samples can deactivate the selection
	(delete-samples (+ beg len) len)))))



;;; -------- selection-members
;;;
;;; returns a list of lists of (snd chn): channels in current selection

(define selection-members
  (let ((+documentation+ "(selection-members) -> list of lists of (snd chn) indicating the channels participating in the current selection."))
    (lambda ()
      (let ((sndlist ()))
	(if (selection?)
	    (for-each (lambda (snd)
			(do ((i (- (channels snd) 1) (- i 1)))
			    ((< i 0))
			  (if (selection-member? snd i)
			      (set! sndlist (cons (list snd i) sndlist)))))
		      (sounds)))
	sndlist))))


;;; -------- make-selection

;;; the regularized form of this would use dur not end

(define make-selection 
  (let ((+documentation+ "(make-selection beg end snd chn) makes a selection like make-region but without creating a region.
make-selection follows snd's sync field, and applies to all snd's channels if chn is not specified. end defaults
to end of channel, beg defaults to 0, snd defaults to the currently selected sound.")

	(add-chan-to-selection 
	 (lambda (s0 s1 s c)
	   (set! (selection-member? s c) #t)
	   (set! (selection-position s c) (or s0 0))
	   (set! (selection-framples s c) (- (or (and (number? s1) (+ 1 s1)) (framples s c)) (or s0 0))))))
	
    (lambda* (beg end snd chn)
      (let ((current-sound (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? current-sound))
	    (error 'no-such-sound "make-selection can't find sound"))
	
	(let ((current-sync (sync current-sound)))
	  (unselect-all)
	  (if (number? chn)
	      (add-chan-to-selection beg end snd chn)
	      (for-each
	       (lambda (s)
		 (if (or (eq? snd #t)
			 (equal? s current-sound)
			 (and (not (= current-sync 0))
			      (= current-sync (sync s))))
		     (do ((i 0 (+ 1 i)))
			 ((= i (channels s)))
		       (add-chan-to-selection beg end s i))))
	       (sounds))))))))



;;; -------- with-temporary-selection

(define with-temporary-selection 
  
  (let ((+documentation+ "(with-temporary-selection thunk beg dur snd chn) saves the current selection placement, makes a new selection \
of the data from sample beg to beg + dur in the given channel.  It then calls thunk, and
restores the previous selection (if any).  It returns whatever 'thunk' returned."))
    (lambda (thunk beg dur snd chn)
      
      (let ((seldata (and (selection?) 
			  (car (selection-members)))))
	(if (selection?)
	    (set! seldata (append seldata (list (selection-position) (selection-framples)))))
	(make-selection beg (- (+ beg dur) 1) snd chn)
	(let ((result (thunk)))
	  (if (not seldata)
	      (unselect-all)
	      (make-selection (caddr seldata) 
			      (- (+ (caddr seldata) (cadddr seldata)) 1)
			      (car seldata)
			      (cadr seldata)))
	  result)))))



;;; -------- filter-selection-and-smooth

(define filter-selection-and-smooth 
  (let ((+documentation+ "(filter-selection-and-smooth ramp-dur flt order) applies 'flt' (via filter-sound) to \
the selection, the smooths the edges with a ramp whose duration is 'ramp-dur' (in seconds)"))
    (lambda* (ramp-dur flt order)
      (let ((temp-file (snd-tempnam)))
	(save-selection temp-file)
	(let ((selsnd (open-sound temp-file)))
	  (filter-sound flt (or order (length flt)) selsnd)
	  (let ((tmp-dur (samples->seconds (framples selsnd))))
	    (set! (sync selsnd) (+ 1 (sync-max))) ; make sure env-sound hits all chans
	    (env-sound (list 0 0  ramp-dur 1  (- tmp-dur ramp-dur) 1  tmp-dur 0) 0 #f 1.0 selsnd)
	    (save-sound selsnd)
	    (close-sound selsnd)
	    (env-selection (list 0 1  ramp-dur 0  (- tmp-dur ramp-dur) 0  tmp-dur 1))))
	(mix temp-file (selection-position) #t #f #f #f #f)))))

;;; (filter-selection-and-smooth .01 (float-vector .25 .5 .5 .5 .25))
