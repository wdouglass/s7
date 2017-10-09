;;; various mix related functions
;;;
;;; (mix->float-vector mix) return mix data in float-vector
;;; (snap-mix-to-beat) forces dragged mix to end up on a beat
;;; (silence-all-mixes) sets all mix amps to 0.0
;;; (find-mix sample snd chn) returns the mix at the given sample, or #f
;;; (mix-maxamp mix) maxamp of mix
;;;
;;; mix-click-sets-amp sets up hook functions so that mix click zeros amps, then subsequent click resets to the before-zero value
;;; check-mix-tags tries to move mix tags around to avoid collisions


(require snd-env.scm)
(provide 'snd-mix.scm)

(define tree-for-each 
  (let ((+documentation+ "(tree-for-each func tree) applies func to every leaf of 'tree'"))
    (lambda (func tree)
      (cond ((null? tree) ())
	    ((not (pair? tree)) (func tree))
	    (else (tree-for-each func (car tree))
		  (tree-for-each func (cdr tree)))))))

(define tree-for-each-reversed 
  (let ((+documentation+ "(tree-for-each-reversed func tree) applies func to every leaf of 'tree' moving in reverse through all the lists"))
    (lambda (func tree)
      (for-each func
		(reverse
		 (let flatten ((lst tree))
		   (cond ((null? lst) ())
			 ((not (pair? lst)) lst)
			 ((pair? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
			 (else (cons (car lst) (flatten (cdr lst)))))))))))

(define mix-sound 
  (let ((+documentation+ "(mix-sound file start) mixes file (all chans) at start in the currently selected sound."))
    (lambda (file start)
      (mix file start #t))))


(define silence-all-mixes
  (let ((+documentation+ "(silence-all-mixes) sets all mix amps to 0"))
    (lambda ()
      (as-one-edit
       (lambda ()
	 (tree-for-each
	  (lambda (id)
	    (set! (mix-amp id) 0.0))
	  (mixes)))))))


(define find-mix 
  (let ((+documentation+ "(find-mix sample snd chn) returns the mix at the given sample, or #f"))
    (lambda* (samp snd chn)
      (let ((mix-list (mixes (or snd (selected-sound) (car (sounds))) (or chn (selected-channel snd) 0))))
	(call-with-exit
	 (lambda (found-it)
	   (for-each
	    (lambda (n)
	      (if (= (mix-position n) samp)
		  (found-it n)))
	    mix-list)
	   #f))))))


(define (mix->float-vector id) (samples 0 (mix-length id) id))

;;; 12-Nov-09: moved save-mix to C (snd-mix.c)

(define (mix-maxamp id) (float-vector-peak (mix->float-vector id)))


;;; -------- snap dragged mix(es) to the nearest beat

(define (snap-mix-1 id samps-moved)
  (let* ((samp (+ samps-moved (mix-position id)))
	 (snd (car (mix-home id)))
	 (bps (/ (beats-per-minute snd (cadr (mix-home id))) 60.0))
	 (sr (srate snd))
	 (beat (floor (/ (* samp bps) sr))))
    (let ((lower (floor (/ (* beat sr) bps)))
	  (higher (floor (/ (* (+ 1 beat) sr) bps))))
      (set! (mix-position id)
	    (if (< (- samp lower) (- higher samp))
		(max 0 lower)
		higher))
      #t)))

(define snap-mix-to-beat
  (let ((+documentation+ "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  (hook-remove mix-release-hook snap-mix-1) to cancel."))
    (lambda ()
      (hook-push mix-release-hook (lambda (hook) (set! (hook 'result) (snap-mix-1 (hook 'id) (hook 'samples))))))))


(define (snap-syncd-mixes-1 id samps-moved)
  (let* ((new-position (let* ((samp (+ samps-moved (mix-position id)))
			      (snd (car (mix-home id)))
			      (bps (/ (beats-per-minute snd (cadr (mix-home id))) 60.0))
			      (sr (srate snd))
			      (beat (floor (/ (* samp bps) sr))))
			 (let ((lower (floor (/ (* beat sr) bps)))
			       (higher (floor (/ (* (+ 1 beat) sr) bps))))
			   (if (< (- samp lower) (- higher samp))
			       (max 0 lower)
			       higher))))
	 (true-samps-moved (- new-position (mix-position id))))
    (if (= (sync id) 0)
	(set! (mix-position id) new-position)
	(move-mixes (syncd-mixes (sync id)) true-samps-moved))
    #t))

(define snap-syncd-mixes-to-beat
  (let ((+documentation+ "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute). \
All mixes sync'd to it are also moved the same number of samples. (hook-remove mix-release-hook snap-syncd-mixes-1) to cancel."))
    (lambda ()
      (hook-push mix-release-hook (lambda (hook) (set! (hook 'result) (snap-syncd-mixes-1 (hook 'id) (hook 'samples))))))))




;;; -------- mix-click-sets-amp

(define (mix-click-sets-amp)
  (hook-push mix-click-hook 
	     (lambda (hook)
	       (let* ((n (hook 'id))
		      (zeroed (mix-property :zero n)))
		 (if zeroed
		     (begin
		       (set! (mix-amp n) (mix-property :amp n))
		       (set! (mix-property :zero n) #f))
		     (begin
		       (set! (mix-property :amp n) (mix-amp n))
		       (set! (mix-amp n) 0.0)
		       (set! (mix-property :zero n) #t)))
		 (set! (hook 'result) #t)))))

					;(mix-click-sets-amp)


;;; ---------- mix-click-info

(define mix-click-info 
  (let ((+documentation+ "(mix-click-info n) is a mix-click-hook function that describes a mix and its properties"))
    (lambda (n)
      (help-dialog "Mix Help"
		   (format #f "Mix ~A (sync: ~A):~%  position: ~D = ~,3F secs~%  length: ~D (~,3F secs)~%  in: ~A[~D]~%  scaler: ~A~%  speed: ~A~%  env: ~A~A"
			   (format #f (if (string=? (mix-name n) "")
					  (values "~A" n)
					  (values "~S (~A)" (mix-name n) n)))
			   (mix-sync n)
			   (mix-position n)
			   (* 1.0 (/ (mix-position n) (srate (car (mix-home n)))))
			   (framples n)
			   (* 1.0 (/ (framples n) (srate (car (mix-home n)))))
			   (short-file-name (car (mix-home n))) 
			   (cadr (mix-home n))
			   (mix-amp n)
			   (mix-speed n)
			   (mix-amp-env n)
			   (let ((props (mix-properties n)))
			     (if (pair? props)
				 (format #f "~%  properties: '~A" props)
				 ""))))
      #t)))

					;(hook-push mix-click-hook (lambda (hook) (set! (hook 'result) (mix-click-info (hook 'id)))))


;;; -------- mix-name->id

(define mix-name->id 
  (let ((+documentation+ "(mix-name->id name) returns the mix associated with 'name'"))
    (lambda (name)
      (call-with-exit
       (lambda (return)
	 (for-each
	  (lambda (snd)
	    (do ((chn 0 (+ 1 chn)))
		((= chn (channels snd)))
	      (for-each
	       (lambda (m)
		 (if (equal? (mix-name m) name)
		     (return m)))
	       (mixes snd chn))))
	  (sounds))
	 'no-such-mix)))))


;;; ---------------- backwards compatibilty

(define delete-mix 
  (let ((+documentation+ "(delete-mix mix) sets the mix's amp to 0.0"))
    (lambda (id) 
      (set! (mix-amp id) 0.0))))



;;; -------- mix lists (used to be called "tracks")
;;;
;;; to use these based on a mix-sync setting, use syncd-mixes below:
;;;   (scale-mixes (syncd-mixes 2) 2.0) scales all mixes whose mix-sync field is 2 by 2.0.

(define scale-mixes 
  (let ((+documentation+ "(scale-mixes mix-list scl) scales the amplitude of each mix in 'mix-list' by 'scl'"))
    (lambda (mix-list scl)
      (as-one-edit
       (lambda ()
	 (for-each
	  (lambda (m)
	    (set! (mix-amp m) (* scl (mix-amp m))))
	  mix-list))))))


(define silence-mixes 
  (let ((+documentation+ "(silence-mixes mix-list) sets the amplitude of each mix in 'mix-list' to 0.0"))
    (lambda (mix-list)
      (scale-mixes mix-list 0.0))))


(define move-mixes 
  (let ((+documentation+ "(move-mixes mix-list samps) moves each mix in 'mix-list' by 'samps' samples"))
    (lambda (mix-list samps)
      (as-one-edit
       (lambda ()
	 (for-each
	  (lambda (m)
	    (set! (mix-position m) (max 0 (+ (mix-position m) samps))))
	  mix-list))))))


(define src-mixes 
  (let ((+documentation+ "(src-mixes mix-list sr) multiplies the speed (resampling ratio) of each mix in 'mix-list' by 'sr'"))
    (lambda (mix-list sr)
      (if (not (= sr 0.0))
	  (as-one-edit
	   (lambda ()
	     (for-each
	      (lambda (m)
		(set! (mix-speed m) (* (mix-speed m) sr)))
	      mix-list)))))))


(define transpose-mixes 
  (let ((+documentation+ "(transpose-mixes mix-list semitones) transposes each mix in mix-list by semitones"))
    (lambda (mix-list semitones)
      (if (not (= semitones 0))
	  (src-mixes mix-list (expt 2.0 (/ semitones 12.0)))))))


(define color-mixes 
  (let ((+documentation+ "(color-mixes mix-list color) sets the tag and waveform color of each mix in 'mix-list' to 'color'"))
    (lambda (mix-list col)
      (for-each
       (lambda (m)
	 (set! (mix-color m) col))
       mix-list))))


(define set-mixes-tag-y 
  (let ((+documentation+ "(set-mixes-tag-y mix-list new-y) sets the mix tag vertical position of each mix in 'mix-list' to 'new-y'.  The \
position is measured from the top of the graph, so higher tag-y values position the tag lower in the graph. For \
example, if you know the frequency of the mix sound, you can reflect that in the tag height with: \n\n\
\n\
   (set! (mix-tag-y mix-id) (round (* 100 (- 1.0 (/ (log (/ freq 40.0)) (* (log 2.0) 7))))))\n"))
    (lambda (mix-list new-y)
      
      (for-each
       (lambda (m)
	 (set! (mix-tag-y m) new-y))
       mix-list))))


(define mixes-maxamp 
  (let ((+documentation+ "(mixes-maxamp mix-list) returns the maximum amplitude of the data in the mixes in 'mix-list'"))
    (lambda (mix-list)
      (let ((mx 0.0))
	(for-each
	 (lambda (m)
	   (set! mx (max mx (mix-maxamp m))))
	 mix-list)
	mx))))


(define scale-tempo 
  (let ((+documentation+ "(scale-tempo mix-list scl) changes the rate at which the mixes in 'mix-list' occur to reflect \
the tempo scaler 'scl'.  If 'scl' is 2.0, for example, the mixes are re-positioned so that they \
happen twice as slowly (the data is not resampled -- each mix is untouched except that its begin time \
may change)"))
    (lambda (mix-list tempo-scl)
      
      (let* ((first-beg (mix-position (car mix-list)))
	     (last-beg first-beg))
	(for-each
	 (lambda (m)
	   (let ((pos (mix-position m)))
	     (set! first-beg (min first-beg pos))
	     (set! last-beg (max last-beg pos))))
	 (cdr mix-list))
	(as-one-edit
	 (lambda ()
	   (for-each
	    (lambda (m)
	      (let ((diff (round (* tempo-scl (- (mix-position m) first-beg)))))
		(if (not (= diff 0))
		    (set! (mix-position m) (+ first-beg diff)))))
	    mix-list)))))))

;;; reverse-mix-list is (scale-tempo mix-list -1.0)


(define mixes-length 
  (let ((+documentation+ "(mixes-length mix-list) returns the number of samples between the start of the earliest mix and the \
last end of the mixes in 'mix-list'"))
    (lambda (mix-list)
      (- (+ (apply max (map (lambda (m) 
			      (+ (mix-position m) (framples m)))
			    mix-list))
	    1)
	 (apply min (map mix-position mix-list))))))


(define env-mixes 
  (let ((+documentation+ "(env-mixes mix-list amp-env) applies 'amp-env' as a global amplitude envelope to the mixes in 'mix-list'"))
    (lambda (mix-list overall-amp-env)
      (let* ((mix-begs (map mix-position mix-list))
	     (mix-ends (map (lambda (m) (+ (mix-position m) (framples m))) mix-list))
	     (beg (apply min mix-begs))
	     (end (apply max mix-ends))
	     (first-x (car overall-amp-env))
	     (x-scale (let ((last-x (envelope-last-x overall-amp-env)))
			(/ (- last-x first-x) (- (+ end 1) beg)))))
	(as-one-edit
	 (lambda ()
	   (for-each 
	    (lambda (m)
	      (let ((wenv (let ((beg-x (+ first-x (* x-scale (- (mix-position m) beg))))
				(end-x (+ first-x (* x-scale (- (+ (mix-position m) (framples m)) beg)))))
			    (window-envelope beg-x end-x overall-amp-env))))
		(set! (mix-amp-env m) (if (null? (mix-amp-env m)) 
					  wenv
					  (multiply-envelopes (mix-amp-env m) wenv)))))
	    mix-list)))))))


(define sync-all-mixes 
  ;; a replacement for set-all-tracks in snd-8
  (let ((+documentation+ "(sync-all-mixes (new-sync 1)) sets the mix-sync field of every active mix to new-sync"))
    (lambda* ((new-sync 1))
      (for-each
       (lambda (snd-m)
	 (for-each
	  (lambda (chn-m)
	    (for-each
	     (lambda (m)
	       (set! (sync m) new-sync))
	     chn-m))
	  snd-m))
       (mixes)))))


(define syncd-mixes 
  (let ((+documentation+ "(syncd-mixes val) returns a list (possibly null) of all mixes whose mix-sync field is set to 'val'"))
    (lambda (val)
      (if (<= val 0)
	  ()
	  (let ((mix-list ()))
	    (for-each
	     (lambda (snd-m)
	       (for-each
		(lambda (chn-m)
		  (for-each
		   (lambda (m)
		     (if (= (sync m) val)
			 (set! mix-list (cons m mix-list))))
		   chn-m))
		snd-m))
	     (mixes))
	    mix-list)))))


(define play-mixes 
  (let ((+documentation+ "(play-mixes mix-list) plays the mixes in 'mix-list'"))
    (lambda (mix-list)
      (let* ((sorted-mixes (sort! (copy mix-list) (lambda (a b) (< (mix-position a) (mix-position b)))))
	     (now (mix-position (car sorted-mixes))))
	(play (lambda ()
		(while (and (pair? sorted-mixes)
			    (= now (mix-position (car sorted-mixes))))
		       (play (let ((mx (car sorted-mixes)))
			       (if (integer? mx)
				   (integer->mix mx)
				   mx)))
		       (set! sorted-mixes (cdr sorted-mixes)))
		(set! now (+ 1 now))
		(and (pair? sorted-mixes)
		     0.0)))))))


;;; -------- pan-mix --------

(define pan-mix 
  
  (letrec ((+documentation+ "(pan-mix file start pan-env snd (auto-delete #f)) mixes 'file' into the sound 'snd'
starting at 'start' (in samples) using 'pan-env' to decide how to split the sound between the output channels (0: all chan 0, 1: all chan 1).
So, (pan-mix \"oboe.snd\" 0 '(0 0 1 1)) goes from all chan 0 to all chan 1.
'auto-delete' determines whether the in-coming file should be treated as a temporary file and deleted when the mix
is no longer accessible.  pan-mix returns a list of the mixes performing the
panning operation.")

	   (invert-envelope 
	    (lambda (e)
	      (if (null? e)
		  ()
		  (cons (car e)
			(cons (- 1.0 (cadr e))
			      (invert-envelope (cddr e))))))))
    
    (lambda* (name beg pan snd auto-delete)
      (let ((deletion-choice (if auto-delete 3 0))) ; multichannel deletion case
	(let ((index (or snd (selected-sound) (and (sounds) (car (sounds)))))
	      (end-deletion-choice (if (= deletion-choice 3) 4 0)))
	  (if (not (sound? index))
	      (error 'no-such-sound (list "pan-mix" snd)))
	  (if (not (file-exists? name))
	      (error 'no-such-file (list "pan-mix" name)))
	  
	  (as-one-edit
	   (lambda ()
	     (let ((incoming-chans (channels name))
		   (receiving-mono (= (channels index) 1)))
	       
	       (if (= incoming-chans 1)
		   ;; mono input
		   (if receiving-mono
		       
		       ;; mono to mono = just scale or envelope
		       (let ((idx (mix name beg 0 index 0 *with-mix-tags* auto-delete))) ; file start in-chan snd chn ...
			 (and (pair? idx)
			      (mix? (car idx))
			      (let ((id (car idx)))
				(set! (mix-amp-env id) (invert-envelope pan))
				idx)))
		       
		       ;; mono to stereo
		       (let ((idx0 (mix name beg 0 index 0 *with-mix-tags* deletion-choice))
			     (idx1 (mix name beg 0 index 1 *with-mix-tags* end-deletion-choice)))
			 (and (pair? idx0)
			      (mix? (car idx0))
			      (pair? idx1)
			      (mix? (car idx1))
			      (let ((id0 (car idx0))
				    (id1 (car idx1)))
				(set! (mix-amp-env id0) (invert-envelope pan))
				(set! (mix-amp-env id1) pan)
				(list id0 id1)))))
		   
		   ;; stero input
		   (if receiving-mono
		       
		       ;; stereo -> mono => scale or envelope both input chans into the output
		       (let ((idx0 (mix name beg 0 index 0 *with-mix-tags* deletion-choice))
			     (idx1 (mix name beg 1 index 0 *with-mix-tags* end-deletion-choice)))
			 (and (pair? idx0)
			      (mix? (car idx0))
			      (pair? idx1)
			      (mix? (car idx1))
			      (let ((id0 (car idx0))
				    (id1 (car idx1)))
				(set! (mix-amp-env id0) (invert-envelope pan))
				(set! (mix-amp-env id1) (invert-envelope pan))
				(list id0 id1))))
		       
		       ;; stereo -> stereo => incoming chans are treated equally, each panned into outputs
		       (let ((idx00 (mix name beg 0 index 0 *with-mix-tags* deletion-choice))
			     (idx01 (mix name beg 0 index 1 *with-mix-tags* deletion-choice))
			     (idx10 (mix name beg 1 index 0 *with-mix-tags* deletion-choice))
			     (idx11 (mix name beg 1 index 1 *with-mix-tags* end-deletion-choice)))
			 
			 (and (pair? idx00)
			      (mix? (car idx00))
			      (pair? idx01)
			      (mix? (car idx01))
			      (pair? idx10)
			      (mix? (car idx10))
			      (pair? idx11)
			      (mix? (car idx11))
			      (let ((id00 (car idx00))
				    (id01 (car idx01))
				    (id10 (car idx10))
				    (id11 (car idx11)))
				(set! (mix-amp-env id00) (invert-envelope pan))
				(set! (mix-amp-env id10) (invert-envelope pan))
				(set! (mix-amp-env id01) pan)
				(set! (mix-amp-env id11) pan)
				(list id00 id01 id10 id11))))))))))))))
  
  
(define pan-mix-selection 
  (let ((+documentation+ "(pan-mix-selection start pan-env snd) mixes the current selection  into the sound 'snd'
starting at 'start' (in samples) using 'pan-env' to pan (0: all chan 0, 1: all chan 1)."))
    (lambda* (beg pan snd)
      (if (not (selection?))
	  (error 'no-active-selection (list "pan-mix-selection"))
	  (pan-mix (save-selection (snd-tempnam)) beg pan snd #t)))))


(define pan-mix-region 
  (let ((+documentation+ "(pan-mix-region reg start pan-env snd) mixes the given region into the sound 'snd' 
starting at 'start' (in samples) using 'pan-env' to pan (0: all chan 0, 1: all chan 1)."))
    (lambda* (reg beg pan snd)
      (if (not (region? reg))
	  (error 'no-such-region (list "pan-mix-region" reg))
	  (pan-mix (save-region reg (snd-tempnam)) beg pan snd #t)))))


(define pan-mix-float-vector 
  (let ((+documentation+ "(pan-mix-float-vector v start pan-env snd) mixes the float-vector data into the sound 'snd' 
starting at 'start' (in samples) using 'pan-env' to pan (0: all chan 0, 1: all chan 1)."))
    (lambda* (v beg pan snd)
      (let ((temp-file (snd-tempnam)))
	(array->file temp-file v (length v) (srate snd) 1)
	(pan-mix temp-file beg pan snd #t)))))



;;; -------- delay-channel-mixes

(define delay-channel-mixes 
  (let ((+documentation+ "(delay-channel-mixes beg dur snd chn) adds dur (which can be negative) to the \
begin time of each mix that starts after beg in the given channel"))
    (lambda* (beg dur snd chn)
      (for-each
       (lambda (m)
	 (if (>= (mix-position m) beg)
	     (set! (mix-position m) (max 0 (+ (mix-position m) dur)))))
       (mixes (or snd 
		  (selected-sound) 
		  (car (sounds))) 
	      (or chn 0))))))


;;; -------- check-mix-tags

(define check-mix-tags 
  (let ((+documentation+ "(check-mix-tags snd chn) tries to move mix tags around to avoid collisions"))
    (lambda* (snd chn)
      (if (not snd)
	  (for-each check-mix-tags (sounds))
	  (if (not chn)
	      (do ((chns (channels snd))
		   (i 0 (+ i 1)))
		  ((= i chns))
		(check-mix-tags snd i))
	      (let ((mxs (mixes snd chn))
		    (changed #f))
		(let check-mix ((mx (car mxs))
				(trailing-mixes (cdr mxs)))
		  (when (pair? trailing-mixes)
		    (let ((pos (mix-position mx))
			  (ls (left-sample snd chn))
			  (rs (right-sample snd chn)))
		      (when (<= ls pos rs)
			(let ((x (x->position (/ pos (srate snd))))
			      (y (mix-tag-y mx)))
			  (for-each (lambda (other-mix)
				      (let ((other-pos (mix-position other-mix)))
					(if (<= ls other-pos rs)
					    (let ((other-x (x->position (/ other-pos (srate snd))))
						  (other-y (mix-tag-y other-mix)))
					      (when (and (< (abs (- x other-x)) 6)
							 (< (abs (- y other-y)) 10))
						(set! (mix-tag-y other-mix) (+ (mix-tag-y other-mix) 20))
						(set! changed #t))))))
				    trailing-mixes)))
		      (check-mix (car trailing-mixes) (cdr trailing-mixes)))))
		(if changed
		    (update-time-graph snd chn))))))))
	      
