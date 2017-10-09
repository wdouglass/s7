;;; hook-related functions

(provide 'snd-hooks.scm)

;;; -------- snd-hooks

(define snd-hooks
  (let ((+documentation+ "(snd-hooks) -> list of all global (not channel-specific) hooks"))
    (lambda ()
      (list after-graph-hook after-lisp-graph-hook lisp-graph-hook before-transform-hook mix-release-hook save-hook mus-error-hook
	    mouse-enter-graph-hook mouse-leave-graph-hook open-raw-sound-hook select-channel-hook after-open-hook close-hook drop-hook update-hook
	    mark-click-hook mark-drag-hook name-click-hook open-hook help-hook before-save-state-hook
	    output-comment-hook play-hook snd-error-hook snd-warning-hook start-playing-hook stop-playing-hook
	    mouse-enter-listener-hook mouse-leave-listener-hook select-sound-hook
	    exit-hook during-open-hook after-transform-hook mouse-enter-label-hook mouse-leave-label-hook initial-graph-hook
	    graph-hook key-press-hook mouse-drag-hook mouse-press-hook enved-hook mouse-click-hook new-widget-hook
	    mark-hook stop-playing-selection-hook after-apply-controls-hook draw-mark-hook
	    bad-header-hook save-state-hook new-sound-hook color-hook orientation-hook listener-click-hook mix-click-hook after-save-state-hook
	    mouse-enter-text-hook mouse-leave-text-hook mix-drag-hook 
	    start-playing-selection-hook after-save-as-hook before-save-as-hook draw-mix-hook
	    before-exit-hook before-close-hook clip-hook))))

(define reset-all-hooks
  (let ((+documentation+ "(reset-all-hooks) removes all Snd hook functions"))
    (lambda ()
      (for-each 
       (lambda (n)
	 (set! (hook-functions n) ()))
       (snd-hooks))
      (for-each 
       (lambda (snd)
	 (do ((chn 0 (+ chn 1)))
	     ((= chn (channels snd)))
	   (set! (hook-functions (edit-hook snd chn)) ())
	   (set! (hook-functions (after-edit-hook snd chn)) ())
	   (set! (hook-functions (undo-hook snd chn)) ())))
       (sounds)))))



;;; -------- describe-hook

(define describe-hook 
  (let ((+documentation+ "(describe-hook hook) -> description of functions on 'hook'"))
    (lambda (hook)
      (for-each 
       (lambda (n) 
	 (snd-print (format #f "~%~A" n)))
       (reverse (hook-functions hook))))))


;;; -------- local hook

(define with-local-hook 
  (let ((+documentation+ "(with-local-hook hook local-hook-procs thunk) evaluates thunk with hook set to local-hook-procs (a list), then restores hook to its previous state"))
    (lambda (hook local-hook-procs thunk)
      (let ((old-hook-procs (hook-functions hook)))
	(set! (hook-functions hook) local-hook-procs)
	(let ((result (thunk)))
	  (set! (hook-functions hook) old-hook-procs)
	  result)))))


;;; -------- hook-member --------

(define hook-member 
  (let ((+documentation+ "(hook-member value hook) returns non-#f if 'value' is a member of the hook's function list"))
    (lambda (value hook) 
      (member value (hook-functions hook)))))
