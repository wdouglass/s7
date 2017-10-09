;   spokenword.scm
;
;   April 17th 2007
;
;   Ville Koskinen
;   j.v.o.koskinen@gmail.com
;
;   Scripts to make editing spoken word files faster and easier. See "Variables" below for fine tuning.
;
;   New keybindings are as follows:
;   - left/right:       Move 0.05 sec to left or right
;   - C-left/right:     Move to the previous or next "phrase"
;   - i:                Insert an "In" mark
;   - o:                Insert an "Out" mark
;   - l:                Listen to 3 secs from cursor, or if "In" and "Out" marks have been defined, 3 secs
;                       prior to the out-mark and 3 secs after the in-mark. In effect preview delete before
;                       commiting it. If already playing, stop. Cursor does not follow.
;   - C-backspace:      Delete audio between "Out" and "In" and smooth the splice
;   - C-c:              Clear all marks
;   - p:                Play from cursor with tracking cursor. If already playing, stop.

; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define secs->samples
  (lambda (time)
    (round (* 44100 time))))

(define size                (secs->samples 0.10)) ;length of the window for rms and peak calculations
(define some-size           (secs->samples 0.05)) ;length of the cursor step for right/left movement
(define peak-threshold       0.15)                ;threshold for non-silence peak detection
(define rms-threshold        0.01)                ;threshold for rms in silence detection
(define phrase-look-offset  (secs->samples 0.10)) ;offset for detection of a start of a phrase
(define jump-length         (secs->samples 0.10)) ;step size for phrase lookup
(define preview-length      (secs->samples 3))    ;length for listening a preview of delete or from cursor

; Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define local-data
  (lambda (position)
    (channel->float-vector (max 0 (- position (* size .5))) size)))

(define local-rms
  (lambda  (position)
    (let ((data (local-data position)))
      (sqrt (/ (dot-product data data) size)))))

(define local-peak
  (lambda (position)
    (float-vector-peak (local-data position))))

(define local-smooth
  (lambda (position)
    (smooth-channel (- position 16) 32)))

(define silence?
  (lambda (position)
    (< (local-rms position) rms-threshold)))

(define phrase? 
  (lambda (position)
    (and (> (local-peak position) peak-threshold)
	 (> (local-rms position) rms-threshold))))

(define phrase-start?
  (lambda (position)
    (and (silence? position) 
	 (phrase? (+ position phrase-look-offset)))))

(define next-phrase
  (lambda (position)
    (do ((i 0 (+ i 1)) 
	 (found #f (phrase-start? position)))
        ((or (= i 100) found (= position (framples))) position)
          (set! position (min (framples) (+ position jump-length))))))

(define previous-phrase
  (lambda (position)
    (do ((i 0 (+ i 1)) 
	 (found #f (phrase-start? position)))
        ((or (= i 100) found (= position 0)) position)
          (set! position (max 0 (- position jump-length))))))

(define mark-out
  (lambda (position)
    (let ((in-mark (find-mark "In"))
	  (out-mark (find-mark "Out")))
    (if (and (mark? in-mark)
	     (<= (mark-sample in-mark) position))
	(delete-mark in-mark))
    (if (mark? out-mark)
        (set! (mark-sample out-mark) position)
        (add-mark position 0 0 "Out")))))

(define mark-in
  (lambda (position)
    (let ((in-mark (find-mark "In"))
	  (out-mark (find-mark "Out")))
    (if (and (mark? out-mark)
	     (>= (mark-sample out-mark) position))
	(delete-mark out-mark))
    (if (mark? in-mark)
        (set! (mark-sample in-mark) position)
        (add-mark position 0 0 "In")))))

(define delete-from-out-to-in
  (lambda ()
    (let ((in-mark (find-mark "In"))
	  (out-mark (find-mark "Out")))
    (if (and (mark? in-mark)
	     (mark? out-mark)
	     (< (mark-sample out-mark) (mark-sample in-mark)))
	(begin
	  (set! (cursor) (mark-sample out-mark))
	  (as-one-edit
	   (lambda()
	     (delete-samples (mark-sample out-mark) (- (+ (mark-sample in-mark) 1) (mark-sample out-mark)))
	     (local-smooth (cursor)))))))))

(define (play-preview)
  (let ((in-mark (find-mark "In"))
	(out-mark (find-mark "Out")))
    (let ((in-position (if (mark? in-mark) (mark-sample in-mark) 0))
	  (out-position (if (mark? out-mark) (mark-sample out-mark) 0))
	  (play-next (lambda (reason)
		       (if (= reason 0)
			   (play (selected-sound) in-position (+ in-position preview-length))))))
      (if (and
	   (mark? in-mark)
	   (mark? out-mark))
	  (if (< out-position in-position)
	      (play (max 0 (- out-position preview-length)) #f #f #f out-position #f play-next))
	  (play (selected-sound) (cursor) (+ (cursor) preview-length))))))


(set! *with-tracking-cursor* :track-and-stay)


; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key #\i 0
  (lambda () ; Mark an in-position
    (mark-in (cursor))
    cursor-in-view))

(bind-key #\o 0
  (lambda () ; Mark an out-position
    (mark-out (cursor))
    cursor-in-view))

(bind-key #\c 4
  (lambda () ; Clear all marks
    (delete-marks)
    cursor-in-view))

(bind-key "BackSpace" 4
  (lambda () ; Commit delete from mark Out to mark In
    (delete-from-out-to-in)
    cursor-in-view))

(bind-key #\p 0
  (lambda () ; Play from cursor
    (set! *with-tracking-cursor* #t)
    (if (playing)
        (stop-playing)
        (play (selected-sound) (cursor)))
    cursor-in-view))

(bind-key #\l 0
  (lambda () ; Listen to a preview before commiting delete or listen from cursor
    (set! *with-tracking-cursor* #f)
    (if (playing)
        (stop-playing)
        (play-preview))
    cursor-in-view))

(bind-key "Right" 0
  (lambda () ; Move cursor to the right
    (set! (cursor) (+ (cursor) some-size))
    cursor-in-view))

(bind-key "Right" 4
  (lambda () ; Move cursor to next interesting position
    (set! (cursor) (next-phrase (cursor)))
    cursor-in-view))

(bind-key "Left" 0
  (lambda () ; Move cursor to the left
    (set! (cursor) (- (cursor) some-size))
    cursor-in-view))

(bind-key "Left" 4
  (lambda () ; Move cursor to previous interesting position
    (set! (cursor) (previous-phrase (cursor)))
    cursor-in-view))
