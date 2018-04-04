;;; various lint-like checks

(define (find-if pred lst)
  (cond ((null? lst) #f)
	((pred (car lst)) (car lst))
	(else (find-if pred (cdr lst)))))
  
(define (va)
  (for-each
   (lambda (func)
     (system (format #f "fgrep ~A *.c > vahi" func))
     (call-with-input-file "vahi"
       (lambda (file)
	 (let loop ((line (read-line file #t)))
	   (or (eof-object? line)
	       (let ((len (length line))
		     (precount 0)
		     (ok #f)
		     (count 0)
		     (flen (length func)))
		 ;; look for * miscounts
		 (call-with-exit
		  (lambda (return)
		    (do ((i 0 (+ i 1)))
			((= i len))
		      (case (string-ref line i)
			((#\*)
			 (set! count (+ 1 count)))
			((#\=)
			 (set! count 0))
			(else
			 (cond ((and (< i (- len 2))
				     (string=? (substring line i (+ i 2)) "/*"))
				(return #f))
			       ((and (< i (- len flen))
				     (string=? (substring line i (+ i flen)) func))
				(set! precount count)
				(set! count 0))
			       ((and (< i (- len 6))
				     (string=? (substring line i (+ i 6)) "sizeof"))
				(set! ok #t)
				(set! count 0))))))))
		 (if (and ok
			  (not (= precount count 0))
			  (not (= count (- precount 1))))
		     (format () "calloc ~D->~D: ~A~%" precount count line))
		 (loop (read-line file #t))))))))
   '("calloc" "malloc" "realloc"))
  
  
  (load "lint.scm")
  
  (for-each
   (lambda (filename)
     (when (and (provided? 'gtk3)
		(provided? 'xg))
       (call-with-input-file filename
	 (lambda (file)
	   (let ((line-number 0))
	     (let loop ((line (read-line file #t)))
	       (or (eof-object? line)
		   (let ((len (length line)))
		     (set! line-number (+ line-number 1))
		     (if (> len 8)
			 (do ((start 0)
			      (i 1 (+ i 1)))
			     ((>= i len))
			   (let ((chr (line i)))
			     (if (or (char-whitespace? chr)
				     (char=? chr #\)))
				 (let* ((name (substring line (+ 1 start) i))
					(name-len (length name)))
				   (if (and (or (and (> name-len 4)
						     (member (substring name 0 4) '("gtk_" "gdk_") string-ci=?))
						(and (> name-len 6)
						     (member (substring name 0 6) '("pango_" "cairo_") string-ci=?)))
					    (not (defined? (string->symbol name) *gtk*)))
				       (format () "~A (~A[~D]) is not defined~%" name filename line-number))))
			     (if (not (or (char=? chr #\_)
					  (char-alphabetic? chr)
					  (char-numeric? chr)))
				 (set! start i)))))
		     (loop (read-line file #t)))))))))
     
					;   (if (string=? (substring filename (- (length filename) 3)) "scm")
					;       (lint filename))
     )
   '("analog-filter.scm"
     "animals.scm"
     "autosave.scm"
     "bess.scm"
     "bess1.scm"
     "big-gens.scm"
     "binary-io.scm"
     "bird.scm"
     "clean.scm"
     "clm-ins.scm"
     "clm23.scm"
     "dlocsig.scm"
     "draw.scm"
     "dsp.scm"
     "edit-menu.scm"
     "edit123.scm"
     "effects-utils.scm"
     "env.scm"
     "enved.scm"
     "examp.scm"
     "expandn.scm"
     "extensions.scm"
     "fade.scm"
     "fft-menu.scm"
     "fmv.scm"
					;  "frame.scm"
     "freeverb.scm"
     "fullmix.scm"
     "generators.scm"
     "grani.scm"
     "gtk-effects-utils.scm"
     "gtk-effects.scm"
     "hooks.scm"
     "index.scm"
     "jcrev.scm"
     "jcvoi.scm"
     "lint.scm"
     "maraca.scm"
     "marks-menu.scm"
     "marks.scm"
     "maxf.scm"
     "misc.scm"
     "mix.scm"
					;  "mixer.scm"
     "moog.scm"
     "musglyphs.scm"
     "nb.scm"
     "new-effects.scm"
     "noise.scm"
     "nrev.scm"
     "numerics.scm"
     "peak-phases.scm"
     "piano.scm"
     "play.scm"
     "poly.scm"
     "prc95.scm"
     "primes.scm"
     "pvoc.scm"
     "rgb.scm"
					;  "rtio.scm"
     "rubber.scm"
     "s7-slib-init.scm"
     "s7test.scm"
     "selection.scm"
     "singer.scm"
     "snd-gl.scm"
     "snd-gtk.scm"
     "snd-motif.scm"
     "snd-test.scm"
					;    "snd11.scm"
					;    "snd12.scm"
     "snddiff.scm"
     "sndlib-ws.scm"
     "sndwarp.scm"
     "special-menu.scm"
     "spectr.scm"
     "spokenword.scm"
     "stochastic.scm"
     "strad.scm"
     "v.scm"
     "write.scm"
     "ws.scm"
     "xm-enved.scm"
     "zip.scm"
     
     "snd.html"
     "sndscm.html"
     "grfsnd.html"
     "extsnd.html"
     "sndclm.html"
     "fm.html"
     "s7.html"
     "sndlib.html")))

(va)


#|
(for-each
 (lambda (filename)
   (call-with-input-file filename
     (lambda (file)
       (let ((line-number 0)
	     (last-name ""))
	 (let loop ((line (read-line file #t)))
	   (or (eof-object? line)
	       (let ((len (length line)))
		 (set! line-number (+ line-number 1))
		 (if (> len 0)
		     (let ((start #f))
		       (do ((i 0 (+ i 1)))
			   ((>= i len))
			 (let ((chr (line i)))
			   (if (not (char-whitespace? chr))
			       (if (not start)
				   (set! start i))
			       (if start
				   (let* ((name (substring line start i))
					  (name-len (length name)))
				     (if (and (> name-len 0)
					      (char-alphabetic? (name 0))
					      (string=? name last-name))
					 (format () ";~A[~D]: ~A repeats in ~A~%" filename line-number name line))
				     (set! last-name name)
				     (set! start #f))))))))
		 (loop (read-line file #t)))))))))
 (list
  "snd.html"
  "sndscm.html"
  "grfsnd.html"
  "extsnd.html"
  "sndclm.html"
  "fm.html"
  "s7.html"
  "sndlib.html"
  ))
|#


#|
(format () "--------------------------------------------------------------------------------~%")
(let ((png-files (directory->list "/home/bil/cl/pix"))
      (baddies ()))
  (for-each
   (lambda (file)
     (if (and (not (directory? file))
	      (not (zero? (system (format #f "fgrep ~A *.html" file)))))
	 (set! baddies (cons file baddies))))
   png-files)
  (if (not (null? baddies))
      (begin
	(format () "--------------------------------------------------------------------------------~%")
	(format () ";unused pix/png: ~{~A ~}~%" baddies)
	(format () "--------------------------------------------------------------------------------~%"))))
|#

(exit)


