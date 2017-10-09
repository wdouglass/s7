(defun dired-open-snd-file ()
  "Open the file where point is or the marked files in Dired with inf-snd program."
  (interactive)
  (let* ((file-list
          (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (snd-scheme-open-file file-path))
     file-list)))

(define-key dired-mode-map (kbd "C-<return>") 'dired-open-snd-file)

;;; USAGE: Open a directory in emacs' dired-mode. position the cursor on a
;;; soundfile or mark some soundfiles and press C-<return> to open them.
