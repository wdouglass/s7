;;; examples of Scheme extensions to Snd
;;;
;;; documentation examples made harder to break
;;; 'info' from extsnd.html using format
;;; correlation
;;; XEmacs-like "Buffers" menu
;;; Reopen menu
;;; set transform-size based on current time domain window size
;;; superimpose spectra of sycn'd sounds
;;; translate mpeg input to 16-bit linear and read into Snd
;;; read and write OGG files
;;; make dot size dependent on number of samples being displayed
;;; move window left edge to mark upon 'm' key
;;; flash selected data red and green
;;; use loop info (if any) to set marks at loop points
;;; mapping extensions (map arbitrary single-channel function over various channel collections)
;;;     do-chans, do-all-chans, do-sound-chans
;;;     every-sample?
;;;     sort-samples
;;; mix mono sound into stereo sound panning according to env, also simple sound placement
;;; fft-edit, fft-squelch, squelch-vowels, fft-env-interp, fft-smoother -- FFT based editing, fft-smoothing
;;; comb-filter, notch-filter, formant-filter
;;; echo (delays)
;;; ring-modulation, am, vibro
;;; src-related sound effects (src, rand-interp, etc)
;;; compand (array-interp)
;;; shift pitch keeping duration constant (src+granulate)
;;; tempo change via envelope (granulate)
;;; cross-synthesis (using a formant bank)
;;; voiced->unvoiced (formants)
;;; convolution (convolve)
;;; time varying FIR filter, notch filter
;;; sound-interp, env-sound-interp
;;; filtered-env (low-pass and amplitude follow envelope)
;;; multi-colored rxvt printout
;;; lisp graph with draggable x axis
;;; pointer focus within Snd
;;; View: Files dialog chooses which sound is displayed
;;; remove-clicks
;;; searching examples (zero+, next-peak, find-pitch)
;;; file->floats and a sort of cue-list, I think, and region-play-list, region-play-sequence
;;; explode-sf2 -- turn soundfont file into a bunch of files of the form sample-name.aif
;;; open-next-file-in-directory -- middle button click closes current file and opens next
;;; chain-dsps
;;; scramble-channels -- reorder chans
;;; scramble-channel -- randomly reorder segments within a sound
;;; reverse-by-blocks and reverse-within-blocks -- reorder or reverse blocks within a channel
;;; sound segmentation
;;; sync-everything

(provide 'snd-examp.scm)
(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))
(require snd-env.scm)


;;; -------- (ext)snd.html examples made harder to break --------
;;;
;;; this mainly involves keeping track of the current sound/channel

(define selection-rms
  (let ((+documentation+ "(selection-rms) -> rms of selection data using samplers"))
    (lambda ()
      (if (selection?)
	  (let ((data (samples (selection-position) (selection-framples))))
	    (sqrt (/ (dot-product data data) (selection-framples))))
	  (error 'no-active-selection (list "selection-rms-1"))))))


(define region-rms
  (let ((+documentation+ "(region-rms n) -> rms of region n's data (chan 0)"))
    (lambda (reg)
      (if (region? reg)
	  (let ((data (region->float-vector reg 0 0)))
	    (sqrt (/ (dot-product data data) (length data))))
	  (error 'no-such-region (list "region-rms" reg))))))


(define window-samples
  (let ((+documentation+ "(window-samples snd chn) -> samples in snd channel chn in current graph window"))
    (lambda* (snd chn)
      (let ((wl (left-sample snd chn))
	    (wr (right-sample snd chn)))
	(channel->float-vector wl (- (+ wr 1) wl) snd chn)))))


(define display-energy 
  ;; in this version, the y-zoom-slider controls the graph amp
  (let ((+documentation+ "(display-energy hook) is a lisp-graph-hook function to display the time domain data as energy (squared)"))
    (lambda (hook)
      (let ((snd (hook 'snd))
	    (chn (hook 'chn)))
	(let ((ls (left-sample snd chn))
	      (rs (right-sample snd chn))
	      (data (let ((datal (make-graph-data snd chn)))
		      (if (float-vector? datal) datal (cadr datal))))
	      (sr (srate snd))
	      (y-max (y-zoom-slider snd chn)))
	  (if (and data ls rs)
	      (begin
		(float-vector-multiply! data data)
		(graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* y-max y-max) snd chn #f))))))))
  
;; (hook-push lisp-graph-hook display-energy)


(define display-db 
  (let ((+documentation+ "(display-db hook) is a lisp-graph-hook function to display the time domain data in dB")
	(dB (lambda (val)
	      (if (< val .001)
		  -60.0
		  (* 20.0 (log val 10))))))
    (lambda (hook)
      (let ((snd (hook 'snd))
	    (chn (hook 'chn)))
	(let ((datal (make-graph-data snd chn)))
	
	  (if datal
	      (let ((data (if (float-vector? datal) datal (cadr datal))))
		(let ((len (length data))
		      (sr (srate snd)))
		  (do ((i 0 (+ i 1)))
		      ((= i len))
		    (set! (data i) (+ 60.0 (dB (abs (data i))))))
		  (graph data "dB" 
			 (/ (left-sample snd chn) sr) (/ (right-sample snd chn) sr)  
			 0.0 60.0
			 snd chn)))))))))

;; (hook-push lisp-graph-hook display-db)


(define window-rms
  (let ((+documentation+ "(window-rms) -> rms of data in currently selected graph window"))
    (lambda ()
      (let* ((data (channel->float-vector (left-sample) (- (+ (right-sample) 1) (left-sample))))
	     (len (length data)))
	(sqrt (/ (dot-product data data) len))))))


(define fft-peak 
  (let ((+documentation+ "(fft-peak hook) returns the peak spectral magnitude.  It is intended for use with after-transform-hook."))
    (lambda (hook)
      (let ((snd (hook 'snd))
	    (chn (hook 'chn)))
	(if (and (transform-graph?) 
		 (= *transform-graph-type* graph-once))
	    (status-report 
	     (number->string (/ (* 2.0 (float-vector-peak (transform->float-vector snd chn))) 
				*transform-size*))
	     snd))))))

					;(hook-push after-transform-hook fft-peak)


;;; -------- 'info' from extsnd.html using format --------

(define finfo 
  (let ((+documentation+ "(finfo file) -> description (as a string) of file"))
    (lambda (file)
      (format #f "~A: chans: ~D, srate: ~D, ~A, ~A, len: ~1,3F"
	      file
	      (channels file)
	      (srate file)
	      (mus-header-type-name (mus-sound-header-type file))
	      (mus-sample-type-name (mus-sound-sample-type file))
	      (* 1.0 (/ (mus-sound-samples file) (channels file) (srate file)))))))


;;; -------- Correlation --------
;;;
;;; correlation of channels in a stereo sound

(define display-correlation 
  (let ((+documentation+ "(display-correlation hook) returns the correlation of snd's 2 channels (intended for use with graph-hook).  y0 and y1 are ignored."))
    (lambda (hook)
      (let ((snd (hook 'snd)))
	(if (not (and (= (channels snd) 2)
		      (> (framples snd 0) 1)
		      (> (framples snd 1) 1)))
	    (status-report "display-correlation wants stereo input")
	    (let* ((ls (left-sample snd 0))
		   (fftlen (floor (expt 2 (ceiling (log (- (+ (right-sample snd 0) 1) ls) 2))))))
	      (let ((fftscale (/ 1.0 fftlen))
		    (rl1 (channel->float-vector ls fftlen snd 0))
		    (rl2 (channel->float-vector ls fftlen snd 1))
		    (im1 (make-float-vector fftlen))
		    (im2 (make-float-vector fftlen)))
		(fft rl1 im1 1)
		(fft rl2 im2 1)
		(let ((tmprl (copy rl1))
		      (tmpim (copy im1)))
		  (float-vector-multiply! tmprl rl2)     ; (* tempr1 tempr2)
		  (float-vector-multiply! tmpim im2)     ; (* tempi1 tempi2)
		  (float-vector-multiply! im2 rl1)       ; (* tempr1 tempi2)
		  (float-vector-multiply! rl2 im1)       ; (* tempr2 tempi1)
		  (float-vector-add! tmprl tmpim)        ; add the first two
		  (float-vector-subtract! im2 rl2)       ; subtract the 4th from the 3rd
		  (fft tmprl im2 -1)
		  (float-vector-scale! tmprl fftscale)   ; scale by fftscale
		  (graph tmprl "lag time" 0 fftlen)))))))))

;; (hook-push graph-hook display-correlation)


;;; -------- set transform-size based on current time domain window size
;;;
;;; also zoom spectrum based on y-axis zoom slider

(define zoom-spectrum 
  (let ((+documentation+ "(zoom-spectrum hook) sets the transform size to correspond to the time-domain window size (use with graph-hook)"))
    (lambda (hook)
      (let ((snd (hook 'snd))
	    (chn (hook 'chn)))
	(if (and (transform-graph? snd chn) 
		 (= (transform-graph-type snd chn) graph-once))
	    (begin
	      (set! (transform-size snd chn)
		    (expt 2 (ceiling (log (- (right-sample snd chn) (left-sample snd chn)) 2.0))))
	      (set! (spectrum-end snd chn) (y-zoom-slider snd chn))))))))


					;(hook-push graph-hook zoom-spectrum)



;;; -------- superimpose spectra of sycn'd sounds

(define superimpose-ffts 
  (let ((+documentation+ "(superimpose-ffts hook) superimposes ffts of multiple (syncd) sounds (use with graph-hook)"))
    (lambda (hook)
      (let ((maxsync (apply max (map sync (sounds))))
	    (snd (hook 'snd))
	    (chn (hook 'chn))
	    (y0 (hook 'y0))
	    (y1 (hook 'y1)))
	(if (and (> (sync snd) 0)
		 (> (right-sample snd chn) (left-sample snd chn))
		 (equal? snd (integer->sound (apply min (map (lambda (n) 
							       (if (= (sync snd) (sync n))
								   (sound->integer n)
								   (+ 1 maxsync)))
							     (sounds))))))
	    (let* ((ls (left-sample snd chn))
		   (pow2 (ceiling (log (max 1 (- (right-sample snd chn) ls)) 2)))
		   (fftlen (floor (expt 2 pow2))))
	      (if (> pow2 2)
		  (let ((ffts ()))
		    (for-each
		     (lambda (n)
		       (if (and (= (sync n) (sync snd))
				(> (channels n) chn))
			   (set! ffts (append ffts (let ((fdr (channel->float-vector ls fftlen n chn))
							 (fdi (make-float-vector fftlen))
							 (spectr (make-float-vector (/ fftlen 2))))
						     (list (float-vector-add! spectr (spectrum fdr fdi #f 2))))))))
		     (sounds))
		    (graph ffts "spectra" 0.0 0.5 y0 y1 snd chn)))))))))

;;(hook-push graph-hook superimpose-ffts)


;;; -------- translate mpeg input to 16-bit linear and read into Snd
;;;
;;; mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
;;;   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

(define mpg 
  (let ((+documentation+ "(mpg file tmpname) converts file from MPEG to raw 16-bit samples using mpg123"))
    (lambda (mpgfile rawfile)
      (let* ((fd (open-input-file mpgfile "r"))
	     (b0 (read-byte fd))
	     (b1 (read-byte fd))
	     (b2 (read-byte fd))
	     (b3 (read-byte fd)))
	(close-input-port fd)
	(if (not (and (= b0 255)
		      (= (logand b1 #b11100000) #b11100000)))
	    (snd-print (format #f "~S is not an MPEG file (first 11 bytes: #b~B #b~B)" mpgfile b0 (logand b1 #b11100000)))
	    (let ((id (ash (logand b1 #b11000) -3))
		  (layer (ash (logand b1 #b110) -1))
		  ;; (protection (logand b1 1))
		  ;; (bitrate-index (ash (logand b2 #b11110000) -4))
		  (srate-index (ash (logand b2 #b1100) -2))
		  ;; (padding (ash (logand b2 #b10) -1))
		  (channel-mode (ash (logand b3 #b11000000) -6))
		  ;; (mode-extension (ash (logand b3 #b110000) -4))
		  ;; (copyright (ash (logand b3 #b1000) -3))
		  ;; (original (ash (logand b3 #b100) -2))
		  ;; (emphasis (logand b3 #b11))
		  )
	      (if (= id 1)
		  (snd-print (format #f "odd: ~S is using a reserved Version ID" mpgfile)))
	      (if (= layer 0)
		  (snd-print (format #f "odd: ~S is using a reserved layer description" mpgfile)))
	      (let ((chans (if (= channel-mode 3) 1 2))
		    (mpeg-layer (case layer ((3) 1) ((2)) (else 3)))
		    (srate (/ (#i(44100 48000 32000 0) srate-index)
			      (case id ((0) 4) ((2)) (else 1)))))
		(snd-print (format #f "~S: ~A Hz, ~A, MPEG-~A~%" 
				   mpgfile srate (if (= chans 1) "mono" "stereo") mpeg-layer))
		(system (format #f "mpg123 -s ~A > ~A" mpgfile rawfile))
		(open-raw-sound rawfile chans srate (if (little-endian?) mus-lshort mus-bshort)))))))))

;;; (mpg "mpeg.mpg" "mpeg.raw")


;;; -------- read and write OGG files

(define read-ogg 
  (let ((+documentation+ "(read-ogg filename) tries to open an OGG file"))
    (lambda (filename)
      ;; check for "OggS" first word, if found, translate to something Snd can read
      ;; (open-sound (read-ogg "/home/bil/sf1/oboe.ogg"))
      (and (call-with-input-file filename 
	     (lambda (fd)
	       (string=? (read-string 4 fd) "OggS")))
	   (let ((aufile (string-append filename ".au")))
	     (if (file-exists? aufile) (delete-file aufile))
	     (system (format #f "ogg123 -d au -f ~A ~A" aufile filename))
	     aufile)))))

#|  
(hook-push open-hook
           (lambda (hook)
	     (let ((filename (hook 'name)))
	       (if (= (mus-sound-header-type filename) mus-raw)
		   (read-ogg filename)))))
;; was returning #f?
|#

(define write-ogg 
  (let ((+documentation+ "(write-ogg snd) writes 'snd' in OGG format"))
    (lambda (snd)
      (if (or (> (car (edits snd)) 0)
	      (not (= (header-type snd) mus-riff)))
	  (let ((file (string-append (file-name snd) ".tmp")))
	    (save-sound-as file snd :header-type mus-riff)
	    (system (format #f "oggenc ~A" file))
	    (delete-file file))
	  (system (format #f "oggenc ~A" (file-name snd)))))))


;;; -------- read and write Speex files

(define read-speex 
  (let ((+documentation+ "(read-speex filename) tries to open a SPEEX file"))
    (lambda (filename)
      (let ((wavfile (string-append filename ".wav")))
	(if (file-exists? wavfile) (delete-file wavfile))
	(system (format #f "speexdec ~A ~A" filename wavfile))
	wavfile))))

(define write-speex 
  (let ((+documentation+ "(write-speex snd) writes 'snd' in Speex format"))
    (lambda (snd)
      ;; write snd data in Speex format
      (if (or (> (car (edits snd)) 0)
	      (not (= (header-type snd) mus-riff)))
	  (let ((file (string-append (file-name snd) ".wav"))
		(spxfile (string-append (file-name snd) ".spx")))
	    (save-sound-as file snd :header-type mus-riff)
	    (system (format #f "speexenc ~A ~A" file spxfile))
	    (delete-file file))
	  (system (format #f "speexenc ~A ~A.spx" (file-name snd) (file-name snd)))))))


;;; -------- read and write FLAC files

(define read-flac 
  (let ((+documentation+ "(read-flac filename) tries to read a FLAC file"))
    (lambda (filename)
      (system (format #f "flac -d ~A" filename)))))

(define write-flac 
  (let ((+documentation+ "(write-flac snd) writes 'snd' in a FLAC file"))
    (lambda (snd)
      ;; write snd data in FLAC format
      (if (or (> (car (edits snd)) 0)
	      (not (= (header-type snd) mus-riff)))
	  (let ((file (string-append (file-name snd) ".wav")))
	    (save-sound-as file snd :header-type mus-riff)
	    (system (format #f "flac ~A" file))
	    (delete-file file))
	  (system (format #f "flac ~A" (file-name snd)))))))


;;; -------- play AC3 via a52dec

(define play-ac3 
  (let ((+documentation+ "(play-ac3 name) uses a52dec to play an AC3 sound"))
    (lambda (name)
      ;;   to turn an AC3 file into something Snd can edit, /usr/local/bin/a52dec test.ac3 -o wav > test.wav
      (system (format #f "a52dec ~A" name)))))


;;; -------- read ASCII files
;;;
;;; these are used by Octave (WaveLab) -- each line has one integer, apparently a signed short.

(define read-ascii 
  (let ((+documentation+ "(read-ascii in-filename (out-filename \"test.snd\") (out-type mus-next) (out-format mus-bshort) (out-srate 44100)) tries to \
read an ASCII sound file"))
    (lambda* (in-filename (out-filename "test.snd") (out-type mus-next) (out-format mus-bshort) (out-srate 44100))
      (let ((in-fd (open-input-file in-filename))
	    (out-fd (new-sound out-filename 1 out-srate out-format out-type (format #f "created by read-ascii: ~A" in-filename)))
	    (bufsize 8192)
	    (bufsize1 8191))
	(as-one-edit
	 (lambda ()
	   (do ((data (make-float-vector bufsize))
		(short->float (/ 1.0 32768.0))
		(fr 0 (+ fr bufsize)))
	       ((eof-object? (peek-char in-fd)))
	     (do ((loc 0 (+ loc 1))
		  (val (read-line in-fd) (read-line in-fd)))
		 ((or (eof-object? val)
		      (= loc bufsize1)) ; bufsize-1 so that we don't throw away a sample at the buffer end
		  (if (number? val)
		      (begin
			(float-vector-set! data loc (* (string->number val) short->float))
			(float-vector->channel data fr (+ loc 1) out-fd 0))
		      (float-vector->channel data fr loc out-fd 0)))
	       (float-vector-set! data loc (* (string->number val) short->float))))))
	(close-input-port in-fd)
	out-fd))))



;;; -------- make dot size dependent on number of samples being displayed
;;; 
;;; this could be extended to set time-graph-style to graph-lines if many samples are displayed, etc

(define auto-dot 
  (let ((+documentation+ "(auto-dot hook) sets the dot size depending on the number of samples being displayed (use with graph-hook)"))
    (lambda (hook)
      (let* ((snd (hook 'snd))
	     (chn (hook 'chn))
	     (dots (- (right-sample snd chn)
		      (left-sample snd chn))))
	(set! (dot-size snd chn)
	      (cond ((assoc dots '((100 . 1) (50 . 2) (25 . 3)) >) => cdr) 
		    (else 5)))))))

;;; (hook-push graph-hook auto-dot)



;;; -------- move window left edge to mark upon 'm'
;;;
;;; in large sounds, it can be pain to get the left edge of the window
;;; aligned with a specific spot in the sound.  In this code, we assume
;;; the desired left edge has a mark, and the 'm' key (without control)
;;; will move the window left edge to that mark.

(define first-mark-in-window-at-left
  (let ((+documentation+ "(first-mark-in-window-at-left) moves the graph so that the leftmost visible mark is at the left edge"))
    (lambda ()
      (let* ((keysnd (or (selected-sound) (car (sounds))))
	     (keychn (or (selected-channel keysnd) 0))
	     (chan-marks (marks keysnd keychn)))
	(letrec ((find-leftmost-mark (let ((current-left-sample (left-sample keysnd keychn)))
				       (lambda (samps)
					 (and (pair? samps)
					      (if (> (car samps) current-left-sample)
						  (car samps)
						  (find-leftmost-mark (cdr samps))))))))
	  (if (null? chan-marks)
	      (status-report "no marks!")
	      (let ((leftmost (find-leftmost-mark (map mark-sample chan-marks))))
		(if (number? leftmost)
		    (begin
		      (set! (left-sample keysnd keychn) leftmost)
		      keyboard-no-action)
		    (status-report "no mark in window")))))))))
    
;; (bind-key #\m 0 (lambda () "align window left edge with mark" (first-mark-in-window-at-left)))


;;; -------- flash selected data red and green

(define flash-selected-data
  (let ((data-red? #t)
	(red (make-color 1 0 0))
	(green (make-color 0 1 0))
	(+documentation+ "(flash-selected-data millisecs) causes the selected data to flash red and green"))
    (lambda (interval)
      (if (selected-sound)
	  (begin
	    (set! *selected-data-color* (if data-red? green red))
	    (set! data-red? (not data-red?))
	    (in interval (lambda () (flash-selected-data interval))))))))


;;; --------  use loop info (if any) to set marks at loop points

(define mark-loops
  (let ((+documentation+ "(mark-loops) places marks at loop points found in the selected sound's header"))
    (lambda ()
      (let ((loops (or (sound-loop-info)
		       (mus-sound-loop-info (file-name)))))
	(if (pair? loops)
	    (if (not (= (car loops) 0 (cadr loops)))
		(begin
		  (add-mark (car loops))
		  (add-mark (cadr loops))
		  (if (not (= (caddr loops) 0 (cadddr loops)))
		      (begin
			(add-mark (caddr loops))
			(add-mark (cadddr loops))))))
	    (snd-print (format #f "~S has no loop info" (short-file-name))))))))



;;; -------- mapping extensions (map arbitrary single-channel function over various channel collections)
;;;

(define all-chans
  (let ((+documentation+ "(all-chans) -> two parallel lists, the first sound objects, the second channel numbers.  If we have 
two sounds open (indices 0 and 1 for example), and the second has two channels, (all-chans) returns '((#<sound 0> #<sound 1> #<sound 1>) (0 0 1))"))
    (lambda ()
      (let ((sndlist ())
	    (chnlist ()))
	(for-each (lambda (snd)
		    (do ((i (- (channels snd) 1) (- i 1)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist)))))

(define do-all-chans 
  (let ((+documentation+ "(do-all-chans func edhist) applies func to all active channels, using edhist as the edit history 
indication: (do-all-chans (lambda (val) (* 2.0 val)) \"double all samples\")"))
    (lambda* (func origin)
      (apply for-each (lambda (snd chn)
			(map-channel func 0 #f snd chn #f origin))
	     (all-chans)))))

(define update-graphs
  (let ((+documentation+ "(update-graphs) updates (redraws) all graphs"))
    (lambda ()
      (apply for-each update-time-graph (all-chans)))))

(define do-chans 
  (let ((+documentation+ "(do-chans func edhist) applies func to all sync'd channels using edhist as the edit history indication"))
    (lambda* (func origin)
      (let ((snc (sync)))
	(if (> snc 0)
	    (apply for-each
		   (lambda (snd chn)
		     (if (= (sync snd) snc)
			 (map-channel func 0 #f snd chn #f origin)))
		   (all-chans))
	    (snd-warning "sync not set"))))))

(define do-sound-chans 
  (let ((+documentation+ "(do-sound-chans func edhist) applies func to all selected channels using edhist as the edit history indication"))
    (lambda* (proc origin)
      (let ((snd (selected-sound)))
	(if snd
	    (do ((chn 0 (+ 1 chn)))
		((= chn (channels snd)) #f)
	      (map-channel proc 0 #f snd chn #f origin))
	    (snd-warning "no selected sound"))))))

(define every-sample? 
  (let ((+documentation+ "(every-sample func) -> #t if func is not #f for all samples in the current channel, 
otherwise it moves the cursor to the first offending sample"))
    (lambda (proc)
      (let ((reader (make-sampler))
	    (len (framples)))
	(call-with-exit
	 (lambda (quit)
	   (do ((i 0 (+ i 1)))
	       ((= i len)) ; returns #t
	     (if (not (proc (next-sample reader)))
		 (begin
		   (set! (cursor) i)
		   (quit #f))))))))))

(define sort-samples 
  (let ((+documentation+ "(sort-samples bins) provides a histogram in 'bins' bins"))
    (lambda (nbins)
      (let ((bins (make-vector nbins 0))
	    (reader (make-sampler))
	    (len (framples))
	    (ops (make-vector nbins)))
	(do ((i 0 (+ i 1)))
	    ((= i nbins))
	  (set! (ops i) (make-one-pole 1.0 -1.0)))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (one-pole (vector-ref ops (floor (* nbins (abs (next-sample reader))))) 1.0))
	(do ((i 0 (+ i 1)))
	    ((= i nbins) bins)
	  (set! (bins i) (floor (one-pole (ops i) 0.0))))))))


;;; -------- mix mono sound into stereo sound panning according to env

(define place-sound 
  (let ((+documentation+ "(place-sound mono-snd stereo-snd pan-env) mixes a mono sound into a stereo sound, splitting 
it into two copies whose amplitudes depend on the envelope 'pan-env'.  If 'pan-env' is 
a number, the sound is split such that 0 is all in channel 0 and 90 is all in channel 1."))
    (lambda (mono-snd stereo-snd pan-env)
      (let ((len (framples mono-snd))
	    (reader0 (make-sampler 0 mono-snd))
	    (reader1 (make-sampler 0 mono-snd)))
	(if (number? pan-env)
	    (let ((pos (/ pan-env 90.0)))
	      (map-channel (lambda (y)
			     (+ y (* pos (read-sample reader1))))
			   0 len stereo-snd 1)
	      (map-channel (lambda (y)
			     (+ y (* (- 1.0 pos) (read-sample reader0))))
			   0 len stereo-snd 0))
	    (let ((e0 (make-env pan-env :length len))
		  (e1 (make-env pan-env :length len)))
	      (map-channel (lambda (y)
			     (+ y (* (env e1) (read-sample reader1))))
			   0 len stereo-snd 1)
	      (map-channel (lambda (y)
			     (+ y (* (- 1.0 (env e0)) (read-sample reader0))))
			   0 len stereo-snd 0)))))))



;;; -------- FFT-based editing
;;;

(define fft-edit 
  (let ((+documentation+ "(fft-edit low-Hz high-Hz snd chn) ffts an entire sound, removes all energy below low-Hz and all above high-Hz, 
then inverse ffts."))
    (lambda* (bottom top snd chn)
      (let ((sr (srate snd))
	    (len (framples snd chn)))
	(let ((fsize (expt 2 (ceiling (log len 2)))))
	  (let ((fsize2 (/ fsize 2))
		(rdata (channel->float-vector 0 fsize snd chn))
		(idata (make-float-vector fsize)))
	    (fft rdata idata 1)
	    (let ((lo (round (/ (* bottom fsize) sr)))) 
	      (if (> lo 0)
		  (begin
		    (fill! rdata 0.0 0 lo)
		    (fill! idata 0.0 0 lo)
		    (fill! rdata (- fsize lo) fsize)
		    (fill! idata (- fsize lo) fsize))))
	    (let ((hi (round (/ (* top fsize) sr))))
	      (if (< hi fsize2)
		  (begin 
		    (fill! rdata 0.0 hi (- fsize hi))
		    (fill! idata 0.0 hi (- fsize hi)))))
	    (fft rdata idata -1)
	    (float-vector-scale! rdata (/ 1.0 fsize))
	    (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "fft-edit ~A ~A" bottom top))))))))
  
  
(define fft-squelch 
  (let ((+documentation+ "(fft-squelch squelch snd chn) ffts an entire sound, sets all bins to 0.0 whose energy is below squelch, then inverse ffts"))
    (lambda* (squelch snd chn)
      (let* ((len (framples snd chn))
	     (fsize (expt 2 (ceiling (log len 2)))))
	(let ((rdata (channel->float-vector 0 fsize snd chn))
	      (idata (make-float-vector fsize))
	      (scaler 1.0))
	  (fft rdata idata 1)
	  (let ((vr (copy rdata))
		(vi (copy idata)))
	    (rectangular->polar vr vi)
	    (set! scaler (float-vector-peak vr)))
	  (let ((scl-squelch (* squelch scaler))
		(rd (copy rdata))
		(id (copy idata)))
	    (float-vector-multiply! rd rd)
	    (float-vector-multiply! id id)
	    (float-vector-add! rd id)
	    (do ((i 0 (+ i 1)))
		((= i fsize))
	      (if (< (sqrt (float-vector-ref rd i)) scl-squelch)
		  (begin
		    (set! (rdata i) 0.0)
		    (set! (idata i) 0.0))))
	    (fft rdata idata -1)
	    (float-vector-scale! rdata (/ 1.0 fsize)))
	  (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "fft-squelch ~A" squelch))
	  scaler)))))


(define fft-cancel 
  (let ((+documentation+ "(fft-cancel lo-freq hi-freq snd chn) ffts an entire sound, sets the bin(s) representing lo-freq to hi-freq to 0.0, then inverse ffts"))
    (lambda* (lo-freq hi-freq snd chn)
      (let* ((len (framples snd chn))
	     (fsize (expt 2 (ceiling (log len 2)))))
	(let ((rdata (channel->float-vector 0 fsize snd chn))
	      (idata (make-float-vector fsize)))
	  (fft rdata idata 1)
	  (let ((hz-bin (/ (srate snd) fsize)))
	    (let ((lo-bin (round (/ lo-freq hz-bin)))
		  (hi-bin (round (/ hi-freq hz-bin))))
	      (fill! rdata 0.0 lo-bin hi-bin)
	      (fill! idata 0.0 lo-bin hi-bin)
	      (fill! rdata 0.0 (- fsize hi-bin) (- fsize lo-bin))))
	  (fft rdata idata -1)
	  (float-vector-scale! rdata (/ 1.0 fsize))
	  (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "fft-cancel ~A ~A" lo-freq hi-freq)))))))


;;; same idea but used to distinguish vowels (steady-state) from consonants

(define ramp 
  (let ((+documentation+ "(ramp gen up) is a kind of CLM generator that produces a ramp of a given length, then sticks at 0.0 or 1.0 until the 'up' argument changes"))
    (lambda (gen up)
      ;; gen is list: ctr size
      ;;  the idea here is that we want to ramp in or out a portion of a sound based on some
      ;;  factor of the sound data -- the ramp gen produces a ramp up when 'up' is #t, sticking
      ;;  at 1.0, and a ramp down when 'up' is #f, sticking at 0.0
      ;;
      ;; this could use the moving-average generator (or one-pole?)
      
      (let-set! gen 'up up)
      (with-let gen
	(set! val (min 1.0 (max 0.0 (+ val (if up incr (- incr))))))))))

(define* (make-ramp (size 128))
  (inlet 'val 0.0 'incr (/ 1.0 size) 'up 1))

;;; (let ((r (make-ramp))) (map-channel (lambda (y) (* y (ramp r (> (random 1.0) 0.5))))))

(define squelch-vowels 
  (let ((+documentation+ "(squelch-vowels snd chn) suppresses portions of a sound that look like steady-state"))
    (lambda* (snd chn)
      (let ((fft-size 32))
	(let ((rl (make-float-vector fft-size))
	      (im (make-float-vector fft-size))
	      (ramper (make-ramp 256)) ; 512 ok too
	      (peak (/ (* 2 (maxamp)) fft-size))
	      (read-ahead (make-sampler 0 snd chn))
	      (ctr 0)
	      (in-vowel #f))
	  (do ((i 0 (+ i 1)))
	      ((= i fft-size))
	    (float-vector-set! rl i (read-sample read-ahead)))
	  (set! ctr (- fft-size 1))
	  (map-channel (lambda (y)
			 (set! ctr (+ ctr 1))
			 (if (= ctr fft-size)
			     (begin
			       (fft rl im 1)
			       (float-vector-multiply! rl rl)
			       (float-vector-multiply! im im)
			       (float-vector-add! rl im)
			       (set! in-vowel (> (+ (rl 0) (rl 1) (rl 2) (rl 3)) peak))
			       ;; fancier version checked here ratio of this sum and
			       ;;   sum of all rl vals, returned vowel if > 0.5
			       (set! ctr 0)
			       (do ((i 0 (+ i 1)))
				   ((= i fft-size))
				 (float-vector-set! rl i (read-sample read-ahead)))
			       (fill! im 0.0)))
			 (* y (- 1.0 (ramp ramper in-vowel))))
					; squelch consonants if just ramp value (not 1.0-val)
					;(and (> rval 0.0) ; if this is included, the vowel-portions are omitted
					; squelch vowels 
					;(* y (+ (* 2 rval) .1)) ;accentuate consonants
		       0 #f snd chn #f "squelch-vowels"))))))


(define fft-env-data 
  (let ((+documentation+ "(fft-env-data fft-env snd chn) applies fft-env as spectral env to current sound, returning float-vector of new data"))
    (lambda* (fft-env snd chn)
      (let ((fsize (expt 2 (ceiling (log (framples snd chn) 2)))))
	(let ((rdata (channel->float-vector 0 fsize snd chn))
	      (idata (make-float-vector fsize))
	      (ve (make-float-vector fsize)))
	  (fft rdata idata 1)
	  (do ((e (make-env (concatenate-envelopes fft-env (reverse-envelope fft-env)) :length fsize))
	       (i 0 (+ i 1)))
	      ((= i fsize))
	    (float-vector-set! ve i (env e)))
	  (float-vector-multiply! rdata ve)
	  (float-vector-multiply! idata ve)
	  (fft rdata idata -1)
	  (float-vector-scale! rdata (/ 1.0 fsize)))))))


(define fft-env-edit 
  (let ((+documentation+ "(fft-env-edit fft-env snd chn) edits (filters) current chan using fft-env"))
    (lambda* (fft-env snd chn)
      (float-vector->channel (fft-env-data fft-env snd chn) 0 (- (framples) 1) snd chn #f (format #f "fft-env-edit '~A" fft-env)))))


(define fft-env-interp 
  (let ((+documentation+ "(fft-env-interp env1 env2 interp snd chn) interpolates between two fft-filtered versions (env1 and env2 are the 
spectral envelopes) following interp (an env between 0 and 1)"))
    (lambda* (env1 env2 interp snd chn)
      (let ((data1 (fft-env-data env1 snd chn))
	     (data2 (fft-env-data env2 snd chn))
	     (len (framples snd chn)))
	(let ((new-data (make-float-vector len))
	      (e (make-env interp :length len))
	      (erev (make-env (scale-envelope interp -1.0 1.0) :length len))) ; 1.0 - e
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (float-vector-set! new-data i
			       (+ (* (env erev) (float-vector-ref data1 i))
				  (* (env e) (float-vector-ref data2 i)))))
	  (float-vector->channel new-data 0 (- len 1) snd chn #f (format #f "fft-env-interp '~A '~A '~A" env1 env2 interp)))))))


(define filter-fft 
  (let ((+documentation+ "(filter-fft flt normalize snd chn) gets the spectrum of all the data in the given channel, \
applies the function 'flt' to it, then inverse ffts.  'flt' should take one argument, the \
current spectrum value.  (filter-fft (lambda (y) (if (< y .01) 0.0 y))) is like fft-squelch."))
    (lambda* (flt (normalize #t) snd chn)
      (let* ((len (framples snd chn))
	     (fsize (expt 2 (ceiling (log len 2))))
	     (fsize2 (/ fsize 2))
	     (rdata (channel->float-vector 0 fsize snd chn)))
	(let ((mx (maxamp snd chn))
	      (idata (make-float-vector fsize))
	      (vf (make-float-vector fsize)))
	  (let ((spect (snd-spectrum rdata rectangular-window fsize #t 1.0 #f normalize))) ; not in-place!
	    (fft rdata idata 1)
	    (flt (spect 0))
	    (do ((i 1 (+ i 1))
		 (j (- fsize 1) (- j 1)))
		((= i fsize2))
	      (float-vector-set! vf j (float-vector-set! vf i (/ (flt (spect i)) (max (spect i) 1e-5))))))
	  (float-vector-multiply! rdata vf)
	  (float-vector-multiply! idata vf)
	  (fft rdata idata -1)
	  (if (= mx 0.0)
	      (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "filter-fft ~A" flt))
	      (let ((pk (float-vector-peak rdata)))
		(float-vector->channel (float-vector-scale! rdata (/ mx pk)) 0 (- len 1) snd chn #f (format #f "filter-fft ~A" flt)))))))))
  

;; (let ((op (make-one-zero .5 .5))) (filter-fft op))
;; (let ((op (make-one-pole .05 .95))) (filter-fft op))
;; (filter-fft (lambda (y) (if (< y .1) 0.0 y)))
;; (let ((rd (make-sampler 0 0 0 1 0))) (scale-by 0) (filter-fft (lambda (y) (rd)))) ; treat original sound as spectrum
;; (filter-fft contrast-enhancement)
;; (filter-fft (lambda (y) (* y y y))) ; extreme low pass

#|
(let* ((ind (or (find-sound "now.snd")
		(open-sound "now.snd")))
       (mx (maxamp ind 0)))
  (do ((i 1 (+ i 1))
       (lo 0.0 (+ lo .1)))
      ((= i 8))
    (filter-fft (lambda (y) (contrast-enhancement y (+ 1.0 (* lo 30.0)))) #t ind 0))
  (let ((mixers (make-vector 8)))
    (do ((i 0 (+ i 1))
	 (lo 0.001 (+ lo .12)))
	((= i 8))
      (env-sound (list 0 0 lo 1 1 0) 0 #f 32.0 ind 0 (+ i 1))
      (set! (mixers i) (make-sampler 0 ind 0 1 (edit-position ind 0))))
    (scale-by 0.0)
    (map-channel
     (lambda (y)
       (let ((sum 0.0))
	 (do ((i 0 (+ i 1)))
	     ((= i 8) sum)
	   (set! sum (+ sum (read-sample (mixers i)))))))))
  (scale-to mx))
|#



(define fft-smoother 
  (let ((+documentation+ "(fft-smoother cutoff start samps snd chn) uses fft-filtering to smooth a 
section: (float-vector->channel (fft-smoother .1 (cursor) 400) (cursor) 400)"))
    (lambda* (cutoff start samps snd chn)
      (let ((fftpts (floor (expt 2 (ceiling (log (+ 1 samps) 2))))))
	(let ((rl (channel->float-vector start fftpts snd chn))
	      (im (make-float-vector fftpts))
	      (top (floor (* fftpts cutoff))))
	  (let ((old0 (rl 0))
		(old1 (rl (- samps 1)))
		(oldmax (float-vector-peak rl)))
	    (fft rl im 1)
	    (do ((i top (+ i 1)))
		((= i fftpts))
	      (set! (rl i) 0.0)
	      (set! (im i) 0.0))
	    (fft rl im -1)
	    (float-vector-scale! rl (/ 1.0 fftpts))
	    (let ((newmax (float-vector-peak rl)))
	      (if (= newmax 0.0)
		  rl
		  (begin
		    (if (> (/ oldmax newmax) 1.5)
			(float-vector-scale! rl (/ oldmax newmax)))
		    (let* ((new0 (rl 0))
			   (new1 (rl (- samps 1)))
			   (offset0 (- old0 new0)))
		      (do ((incr (let ((offset1 (- old1 new1)))
				   (if (= offset1 offset0) 0.0 (/ (- offset1 offset0) samps))))
			   (i 0 (+ i 1))
			   (trend offset0))
			  ((= i samps))
			(set! (rl i) (+ (rl i) trend))
			(set! trend (+ trend incr)))
		      rl))))))))))



;;; -------- comb-filter

(define comb-filter 
  (let ((+documentation+ "(comb-filter scaler size) returns a comb-filter ready for map-channel etc: (map-channel (comb-filter .8 32)).  If you're 
in a hurry use: (clm-channel (make-comb .8 32)) instead"))
    (lambda (scaler size)
      (let ((cmb (make-comb scaler size)))
	(lambda (x) 
	  (comb cmb x))))))


;;; by using filters at harmonically related sizes, we can get chords:

(define comb-chord 
  (let ((+documentation+ "(comb-chord scaler size amp) returns a set of harmonically-related comb filters: (map-channel (comb-chord .95 100 .3))"))
    (lambda (scaler size amp)
      (let ((cs (make-comb-bank (vector (make-comb scaler (floor size))
					(make-comb scaler (floor (* size .75)))
					(make-comb scaler (floor (* size 1.2)))))))
	(lambda (x) 
	  (* amp (comb-bank cs x)))))))


;;; or change the comb length via an envelope:

(define zcomb 
  (letrec ((+documentation+ "(zcomb scaler size pm) returns a comb filter whose length varies according to an 
envelope: (map-channel (zcomb .8 32 '(0 0 1 10)))")
	   (max-envelope-1 
	    (lambda (e mx)
	      (if (null? e)
		  mx
		  (max-envelope-1 (cddr e) (max mx (abs (cadr e))))))))
    (lambda (scaler size pm)
      (let ((cmb (make-comb scaler size :max-size (floor (+ size 1 (max-envelope-1 pm 0.0)))))
	    (penv (make-env pm :length (framples))))
	(lambda (x)
	  (comb cmb x (env penv)))))))


(define notch-filter 
  (let ((+documentation+ "(notch-filter scaler size) returns a notch-filter: (map-channel (notch-filter .8 32))"))
    (lambda (scaler size)
      (let ((cmb (make-notch scaler size)))
	(lambda (x) 
	  (notch cmb x))))))


(define formant-filter 
  (let ((+documentation+ "(formant-filter radius frequency) returns a formant generator: (map-channel (formant-filter .99 2400)). Faster 
is: (filter-sound (make-formant 2400 .99))"))
    (lambda (radius frequency)
      (let ((frm (make-formant frequency radius)))
	(lambda (x) 
	  (formant frm x))))))


;;; to impose several formants, just add them in parallel:

(define formants 
  (let ((+documentation+ "(formants r1 f1 r2 f2 r3 f3) returns 3 formant filters in parallel: (map-channel (formants .99 900 .98 1800 .99 2700))"))
    (lambda (r1 f1 r2 f2 r3 f3)
      (let ((fr1 (make-formant f1 r1))
	    (fr2 (make-formant f2 r2))
	    (fr3 (make-formant f3 r3)))
	(lambda (x)
	  (+ (formant fr1 x)
	     (formant fr2 x)
	     (formant fr3 x)))))))


(define moving-formant 
  (let ((+documentation+ "(moving-formant radius move) returns a time-varying (in frequency) formant filter: (map-channel (moving-formant .99 '(0 1200 1 2400)))"))
    (lambda (radius move)
      (let ((frm (make-formant (cadr move) radius))
	    (menv (make-env move :length (framples))))
	(lambda (x)
	  (let ((val (formant frm x)))
	    (mus-set-formant-frequency frm (env menv))
	    val))))))


(define osc-formants 
  (let ((+documentation+ "(osc-formants radius bases amounts freqs) set up any number of independently oscillating 
formants, then calls map-channel: (osc-formants .99 (float-vector 400.0 800.0 1200.0) (float-vector 400.0 800.0 1200.0) (float-vector 4.0 2.0 3.0))"))
    (lambda (radius bases amounts freqs) ; changed to call map-channel itself, 21-Apr-05
      (let ((len (length bases)))
	(if (= len 3)
	    ;; this way is faster but verbose
	    (let ((fa1 (amounts 0))
		  (fa2 (amounts 1))
		  (fa3 (amounts 2))
		  (frq1 (bases 0))
		  (frq2 (bases 1))
		  (frq3 (bases 2))
		  (fr1 (make-formant (bases 0) radius))
		  (fr2 (make-formant (bases 1) radius))
		  (fr3 (make-formant (bases 2) radius))
		  (o1 (make-oscil (freqs 0)))
		  (o2 (make-oscil (freqs 1)))
		  (o3 (make-oscil (freqs 2))))
	      (map-channel
	       (lambda (y)
		 (+ (formant fr1 y (hz->radians (+ frq1 (* fa1 (oscil o1)))))
		    (formant fr2 y (hz->radians (+ frq2 (* fa2 (oscil o2)))))
		    (formant fr3 y (hz->radians (+ frq3 (* fa3 (oscil o3)))))))))
	    
	    (let ((frms (make-vector len))
		  (oscs (make-vector len))
		  (amps (make-float-vector len 1.0)))
	      (do ((i 0 (+ i 1)))
		  ((= i len))
		(set! (frms i) (make-formant (bases i) radius))
		(set! (oscs i) (make-oscil (freqs i))))
	      (let ((frms1 (make-formant-bank frms amps)))
		(map-channel
		 (lambda (x)
		   (let ((val (formant-bank frms1 x)))
		     (do ((i 0 (+ i 1)))
			 ((= i len))
		       (mus-set-formant-frequency (vector-ref frms i)
						  (+ (bases i)
						     (* (amounts i) 
							(oscil (oscs i))))))
		     val))))))))))



;;; -------- echo

(define echo 
  (let ((+documentation+ "(echo scaler secs) returns an echo maker: (map-channel (echo .5 .5) 0 44100)"))
    (lambda (scaler secs)
      (let ((del (make-delay (round (* secs (srate))))))
	(lambda (inval)
	  (+ inval (delay del (* scaler (+ (tap del) inval)))))))))


(define zecho 
  (let ((+documentation+ "(zecho scaler secs freq amp) returns a modulated echo maker: (map-channel (zecho .5 .75 6 10.0) 0 65000)"))
    (lambda (scaler secs frq amp)
      (let ((os (make-oscil frq))
	    (del (let ((len (round (* secs (srate)))))
		   (make-delay len :max-size (floor (+ len amp 1))))))
	(lambda (inval)
	  (+ inval 
	     (delay del 
		    (* scaler (+ (tap del) inval))
		    (* amp (oscil os)))))))))


(define flecho 
  (let ((+documentation+ "(flecho scaler secs) returns a low-pass filtered echo maker: (map-channel (flecho .5 .9) 0 75000)"))
    (lambda (scaler secs)
      (let ((flt (make-fir-filter :order 4 :xcoeffs #r(.125 .25 .25 .125)))
	    (del (make-delay  (round (* secs (srate))))))
	(lambda (inval)
	  (+ inval 
	     (delay del 
		    (fir-filter flt (* scaler (+ (tap del) inval))))))))))


;;; -------- ring-mod and am
;;;
;;; CLM instrument is ring-modulate.ins

(define ring-mod 
  (let ((+documentation+ "(ring-mod freq gliss-env) returns a time-varying ring-modulation filter: (map-channel (ring-mod 10 (list 0 0 1 (hz->radians 100))))"))
    (lambda (freq gliss-env)
      (let ((os (make-oscil :frequency freq))
	    (genv (make-env gliss-env :length (framples))))
	(lambda (inval)
	  (* (oscil os (env genv)) inval))))))


(define am 
  (let ((+documentation+ "(am freq)returns an amplitude-modulator: (map-channel (am 440))"))
    (lambda (freq)
      (let ((os (make-oscil freq))) 
	(lambda (inval) 
	  (amplitude-modulate 1.0 inval (oscil os)))))))


;;; this taken from sox (vibro.c)

(define vibro 
  (let ((+documentation+ "(vibro speed depth) adds vibrato or tremolo"))
    (lambda (speed depth)
      (let ((sine (make-oscil speed))
	    (scl (* 0.5 depth)))
	(let ((offset (- 1.0 scl)))
	  (lambda (y)
	    (* y (+ offset (* scl (oscil sine))))))))))


;;; -------- hello-dentist
;;;
;;; CLM instrument version is in clm.html

(define hello-dentist 
  (let ((+documentation+ "(hello-dentist frq amp snd chn) varies the sampling rate randomly, making a voice sound quavery: (hello-dentist 40.0 .1)"))
    (lambda* (frq amp snd chn)
      (let ((rn (make-rand-interp :frequency frq :amplitude amp))
	    (len (framples))
	    (sr (make-src :srate 1.0 
			  :input (let ((rd (make-sampler 0 snd chn)))
				   (lambda (dir) 
				     (read-sample-with-direction rd dir))))))
	(map-channel
	 (lambda (y)
	   (src sr (rand-interp rn)))
	 0 len snd chn #f (format #f "hello-dentist ~A ~A" frq amp))))))


;;; a very similar function uses oscil instead of rand-interp, giving
;;; various "Forbidden Planet" sound effects:

(define fp 
  (let ((+documentation+ "(fp sr osamp osfrq snd chn) varies the sampling rate via an oscil: (fp 1.0 .3 20)"))
    (lambda* (sr osamp osfrq snd chn)
      (let ((os (make-oscil osfrq))
	    (s (make-src :srate sr :input (let ((sf (make-sampler 0 snd chn)))
					    (lambda (dir) 
					      (read-sample-with-direction sf dir))))))
	(map-channel
	 (lambda (y)
	   (src s (* osamp (oscil os))))
	 0 #f snd chn #f (format #f "fp ~A ~A ~A" sr osamp osfrq))))))



;;; -------- compand, compand-channel

(define compand-table (float-vector -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
				    0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000))
;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")

(define compand
  (let ((+documentation+ "(compand) returns a compander: (map-channel (compand))"))
    (lambda ()
      (lambda (inval)
	(array-interp compand-table (+ 8.0 (* 8.0 inval)) 17)))))


;;; -------- shift pitch keeping duration constant
;;;
;;; both src and granulate take a function argument to get input whenever it is needed.
;;; in this case, src calls granulate which reads the currently selected file.
;;; CLM version is in expsrc.ins

(define expsrc 
  (let ((+documentation+ "(expsrc rate snd chn) uses sampling-rate conversion and granular synthesis 
to produce a sound at a new pitch but at the original tempo.  It returns a function for map-channel."))
    (lambda* (rate snd chn)
      (let ((sr (make-src :srate rate
			  :input (let ((gr (make-granulate :expansion rate
							   :input (make-sampler 0 snd chn))))
				   (lambda (dir) 
				     (granulate gr))))))
	(lambda (inval)
	  (src sr 0.0))))))


;;; the next (expsnd) changes the tempo according to an envelope; the new duration
;;; will depend on the expansion envelope -- we integrate it to get
;;; the overall expansion, then use that to decide the new length.

(define expsnd 
  (let ((+documentation+ "(expsnd gr-env snd chn) uses the granulate generator to change tempo according to an envelope: (expsnd '(0 .5 2 2.0))"))
    (lambda* (gr-env snd chn)
      (let* ((dur (/ (* (/ (framples snd chn) (srate snd)) 
			(integrate-envelope gr-env)) ; in env.scm
		     (envelope-last-x gr-env)))
	     (len (max (round (* (srate snd) dur)) (framples snd chn))))
	(do ((out-data (make-float-vector len))
	     (gr (make-granulate :expansion (cadr gr-env) 
				 :jitter 0
				 :input (make-sampler 0 snd chn)))
	     (ge (make-env gr-env :duration dur))
	     (i 0 (+ i 1)))
	    ((= i len)
	     (float-vector->channel out-data 0 len snd chn #f (format #f "expsnd '~A" gr-env)))
	  (float-vector-set! out-data i (granulate gr))
	  (set! (mus-increment gr) (env ge)))))))


;;; -------- cross-synthesis
;;;
;;; CLM version is in clm.html

(define cross-synthesis 
  (let ((+documentation+ "(cross-synthesis cross-snd amp fftsize r) does cross-synthesis between 'cross-snd' (a sound object) and the currently 
selected sound: (map-channel (cross-synthesis (integer->sound 0) .5 128 6.0))"))
    (lambda (cross-snd amp fftsize r)
      (let ((freq-inc (/ fftsize 2)))
	(let ((spectr (make-float-vector freq-inc))
	      (formants (make-vector freq-inc)))
	  (let-temporarily ((*clm-srate* (srate)))
	    ;; if mus-srate is 44.1k and srate is 48k, make-formant thinks we're trying to go past srate/2
	    ;;    and in any case it's setting its formants incorrectly for the actual output srate
	    (do ((radius (- 1.0 (/ r fftsize)))
		 (bin (/ (srate) fftsize))
		 (i 0 (+ i 1)))
		((= i freq-inc))
	      (set! (formants i) (make-formant (* i bin) radius)))
	    (set! formants (make-formant-bank formants spectr)))
	  (let ((fdr #f)
		(ctr freq-inc)
		(inctr 0))
	    (lambda (inval)
	      (if (= ctr freq-inc)
		  (let ((fdi (make-float-vector fftsize)))
		    (set! fdr (channel->float-vector inctr fftsize cross-snd 0))
		    (set! inctr (+ inctr freq-inc))
		    (spectrum fdr fdi #f 2)
		    (float-vector-subtract! fdr spectr)
		    (float-vector-scale! fdr (/ 1.0 freq-inc))
		    (set! ctr 0)))
	      (set! ctr (+ ctr 1))
	      (float-vector-add! spectr fdr)
	      (* amp (formant-bank formants inval)))))))))
  
  
;;; similar ideas can be used for spectral cross-fades, etc -- for example:

(define voiced->unvoiced 
  (let ((+documentation+ "(voiced->unvoiced amp fftsize r tempo snd chn) turns a vocal sound into whispering: (voiced->unvoiced 1.0 256 2.0 2.0)"))
    (lambda* (amp fftsize r tempo snd chn)
      (let ((freq-inc (/ fftsize 2))
	    (len (framples snd chn)))
	(let ((outlen (floor (/ len tempo)))
	      (fdr #f)
	      (fdi (make-float-vector fftsize))
	      (spectr (make-float-vector freq-inc))
	      (noi (make-rand (/ (srate snd) 3)))
	      (ctr 0)
	      (hop (floor (* freq-inc tempo)))
	      (formants (make-vector freq-inc))
	      (old-peak-amp 0.0))
	  (let ((out-data (make-float-vector (max len outlen))))
	    
	    (do ((bin (/ (srate snd) fftsize))
		 (radius (- 1.0 (/ r fftsize)))
		 
		 (i 0 (+ i 1)))
		((= i freq-inc))
	      (set! (formants i) (make-formant (* i bin) radius)))
	    (set! formants (make-formant-bank formants spectr))
	    
	    (do ((inctr 0)
		 (i 0 (+ i freq-inc)))
		((>= i outlen))
	      (set! ctr (min (- outlen i) freq-inc))
	      (if (odd? ctr) (set! ctr (- ctr 1)))
	      
	      (set! fdr (channel->float-vector inctr fftsize snd chn))
	      (set! old-peak-amp (max (float-vector-peak fdr) old-peak-amp))
	      (spectrum fdr fdi #f 2)
	      (float-vector-subtract! fdr spectr)
	      (float-vector-scale! fdr (/ 2.0 freq-inc))
	      (set! inctr (+ inctr hop))
	      
	      (do ((k 0 (+ k 2))
		   (j i (+ j 2)))
		  ((= k ctr))
		(float-vector-add! spectr fdr)
		(float-vector-set! out-data j (formant-bank formants (rand noi)))
		(float-vector-set! out-data (+ j 1) (formant-bank formants (rand noi)))))
	  
	  (float-vector-scale! out-data (* amp (/ old-peak-amp (float-vector-peak out-data))))
	  (float-vector->channel out-data 0 (max len outlen) snd chn)))))))


;;; very similar but use ncos (glottal pulse train?) instead of white noise

(define pulse-voice 
  (let ((+documentation+ "(pulse-voice cosines (freq 440) (amp 1.0) (fftsize 256) (r 2.0) snd chn) uses ncos to manipulate speech sounds"))
    (lambda* (cosines (freq 440.0) (amp 1.0) (fftsize 256) (r 2.0) snd chn)
      (let ((freq-inc (/ fftsize 2)))
	(let ((spectr (make-float-vector freq-inc))
	      (formants (make-vector freq-inc))
	      (len (framples snd chn))) 

	  (do ((radius (- 1.0 (/ r fftsize)))
	       (bin (/ (srate snd) fftsize))
	       (i 0 (+ i 1)))
	      ((= i freq-inc))
	    (set! (formants i) (make-formant (* i bin) radius)))
	  (set! formants (make-formant-bank formants spectr))
	  
	  (do ((old-peak-amp 0.0)
	       (pulse (make-ncos freq cosines)) 
	       (out-data (make-float-vector len)) 
	       (fdr #f)
	       (inctr 0)
	       (fdi (make-float-vector fftsize))
	       (ctr 0)
	       (i 0 (+ i freq-inc)))
	      ((>= i len)
	       (float-vector-scale! out-data (* amp (/ old-peak-amp (float-vector-peak out-data))))
	       (float-vector->channel out-data 0 len snd chn))
	    (set! ctr (min (- len i) freq-inc))
	    
	    (set! fdr (channel->float-vector inctr fftsize snd chn))
	    (set! old-peak-amp (max (float-vector-peak fdr) old-peak-amp))
	    (spectrum fdr fdi #f 2)
	    (float-vector-subtract! fdr spectr)
	    (float-vector-scale! fdr (/ 1.0 freq-inc))
	    (set! inctr (+ inctr freq-inc))
	    
	    (do ((k 0 (+ k 1))
		 (j i (+ j 1)))
		((= k ctr))
	      (float-vector-add! spectr fdr)
	      (float-vector-set! out-data j (formant-bank formants (ncos pulse))))))))))

;;; (pulse-voice 80 20.0 1.0 1024 0.01)
;;; (pulse-voice 80 120.0 1.0 1024 0.2)
;;; (pulse-voice 30 240.0 1.0 1024 0.1)
;;; (pulse-voice 30 240.0 1.0 2048)
;;; (pulse-voice 6 1000.0 1.0 512)


;;; -------- convolution example

(define cnvtest 
  (let ((+documentation+ "(cnvtest snd0 snd1 amp) convolves snd0 and snd1, scaling by amp, returns new max amp: (cnvtest 0 1 .1)"))
    (lambda (snd0 snd1 amp)
      (let* ((flt-len (framples snd0))
	     (total-len (+ flt-len (framples snd1)))
	     (cnv (make-convolve :filter (channel->float-vector 0 flt-len snd0)
				 :input (make-sampler 0 snd1)))
	     (out-data (make-float-vector total-len)))
	(do ((i 0 (+ i 1)))
	    ((= i total-len))
	  (float-vector-set! out-data i (convolve cnv)))
	(float-vector-scale! out-data amp)
	(let ((max-samp (float-vector-peak out-data)))
	  (float-vector->channel out-data 0 total-len snd1)
	  (if (> max-samp 1.0) (set! (y-bounds snd1) (list (- max-samp) max-samp)))
	  max-samp)))))



;;; -------- locate-zero (Anders Vinjar)

(define locate-zero 
  (let ((+documentation+ "(locate-zero limit) looks for successive samples that sum to less than 'limit', moving the cursor if successful"))
    (lambda (limit)
      (let* ((start (cursor))
	     (sf (make-sampler start)))
	(do ((n start (+ 1 n))
	     (val0 (abs (next-sample sf)))
	     (val1 (abs (next-sample sf)) (abs (next-sample sf))))
	    ((or (sampler-at-end? sf)
		 (< (+ val0 val1) limit))
	     (set! (cursor) n))
	  (set! val0 val1))))))


;;; -------- sound interp
;;;
;;; make-sound-interp sets up a sound reader that reads a channel at an arbitary location,
;;;   interpolating between samples if necessary, the corresponding "generator" is sound-interp

(define make-sound-interp 
  (let ((+documentation+ "(make-sound-interp start snd chn) -> an interpolating reader for snd's channel chn"))
    (lambda* (start snd chn)
      (let* ((data (channel->float-vector start #f snd chn))
	     (size (length data)))
	(lambda (loc)
	  (array-interp data loc size))))))

(define sound-interp 
  (let ((+documentation+ "(sound-interp func loc) -> sample at loc (interpolated if necessary) from func created by make-sound-interp"))
    (lambda (func loc) ;make it look like a clm generator
      (func loc))))

#|
(define test-interp
  (lambda (freq)
    ;; use a sine wave to lookup the current sound
    (let ((osc (make-oscil :frequency freq :initial-phase (+ pi (/ pi 2))))
	  (reader (make-sound-interp 0 0 0)) 
	  (len (- (framples 0 0) 1)))
      (map-channel (lambda (val) 
		     (sound-interp reader (* len (+ 0.5 (* 0.5 (oscil osc))))))))))

;;; (test-interp 0.5)

;;; our FM index is len * 0.5 * (hz->radians freq)

(define (sound-via-sound snd1 snd2) ; "sound composition"??
  (let* ((intrp (make-sound-interp 0 snd1 0))
	 (len (- (framples snd1 0) 1))
	 (rd (make-sampler 0 snd2 0))
	 (mx (maxamp snd2 0)))
    (map-channel (lambda (val) 
		   (sound-interp intrp (floor (* len (* 0.5 (+ 1.0 (/ (read-sample rd) mx))))))))))
|#


;; env-sound-interp takes an envelope that goes between 0 and 1 (y-axis), and a time-scaler
;;   (1.0 = original length) and returns a new version of the data in the specified channel
;;   that follows that envelope (that is, when the envelope is 0 we get sample 0, when the
;;   envelope is 1 we get the last sample, envelope = .5 we get the middle sample of the 
;;   sound and so on. (env-sound-interp '(0 0 1 1)) will return a copy of the
;;   current sound; (env-sound-interp '(0 0 1 1 2 0) 2.0) will return a new sound 
;;   with the sound copied first in normal order, then reversed.  src-sound with an
;;   envelope could be used for this effect, but it is much more direct to apply the
;;   envelope to sound sample positions.

(define env-sound-interp 
  (let ((+documentation+ "(env-sound-interp env (time-scale 1.0) snd chn) reads snd's channel chn according to env and time-scale"))
    (lambda* (envelope (time-scale 1.0) snd chn)
      ;; since the old/new sounds can be any length, we'll write a temp file rather than trying to use map-channel
      
      (let* ((len (framples snd chn))
	     (newlen (floor (* time-scale len)))
	     (new-snd (with-sound ((snd-tempnam) :to-snd #f :srate (srate snd))
			 (let ((data (channel->float-vector 0 #f snd chn))
			       (read-env (make-env envelope :length (+ 1 newlen) :scaler len)))
			   (do ((i 0 (+ i 1)))
			       ((= i newlen))
			     (outa i (array-interp data (env read-env) len)))))))
	(set-samples 0 newlen new-snd snd chn #t
		     (format #f "env-sound-interp '~A ~A" envelope time-scale)
		     0 current-edit-position #t)))))


;;; (env-sound-interp '(0 0 1 1 2 0) 2.0)



;;; here's a very similar function that uses granular synthesis to move at a varying tempo through a sound

(define granulated-sound-interp 
  
  (let ((+documentation+ "(granulated-sound-interp envelope (time-scale 1.0) (grain-length 0.10) (grain-envelope '(0 0 1 1 2 1 3 0)) (output-hop 0.05) snd chn) reads \
the given channel following 'envelope' (as in env-sound-interp), using grains to create the re-tempo'd read"))
    (lambda* (envelope (time-scale 1.0) (grain-length 0.10) (grain-envelope '(0 0 1 1 2 1 3 0)) (output-hop 0.05) snd chn)
      
      (let* ((len (framples snd chn))
	     (newlen (floor (* time-scale len))))
	(let ((read-env (make-env envelope :length newlen :scaler len))
	      (grain-frames (round (* grain-length (srate snd))))
	      (hop-frames (round (* output-hop (srate snd))))
	      (num-readers (ceiling (/ grain-length output-hop)))
	      (cur-readers 0)
	      (next-reader 0)
	      (jitter (* (srate snd) .005)))
	  
	  (let ((readers (make-vector num-readers #f))
		(grain-envs (make-vector num-readers #f)))
	    (do ((i 0 (+ i 1)))
		((= i num-readers))
	      (set! (grain-envs i) (make-env grain-envelope :length grain-frames)))
	    
	    (let ((new-snd (with-sound ((snd-tempnam) :to-snd #f :srate (srate snd))
			     
			     (do ((i 0 (+ i hop-frames)))
				 ((>= i newlen))
			       (let ((start i)
				     (stop (min newlen (+ i hop-frames))))
				 (set! (mus-location read-env) i)
				 (let ((position-in-original (env read-env)))
				   (set! (readers next-reader)
					 (make-sampler (max 0 (round (+ position-in-original (mus-random jitter)))) snd chn)))
				 (mus-reset (grain-envs next-reader)) ; restart grain env
				 (set! next-reader (modulo (+ next-reader 1) num-readers))
				 (set! cur-readers (max cur-readers next-reader))
				 
				 (do ((e #f)
				      (r #t)
				      (k 0 (+ k 1)))
				     ((= k cur-readers))
				   (set! e (grain-envs k))
				   (set! r (readers k))
				   (do ((j start (+ j 1)))
				       ((= j stop))
				     (outa j (* (env e) (next-sample r))))))))))
	      
	      (set-samples 0 newlen new-snd snd chn #t
			   (format #f "granulated-sound-interp '~A ~A ~A ~A ~A" envelope time-scale grain-length grain-envelope output-hop)
			   0 current-edit-position #t))))))))

;;; (granulated-sound-interp '(0 0 1 .1 2 1) 1.0 0.2 '(0 0 1 1 2 0))
;;; (granulated-sound-interp '(0 0 1 1) 2.0)
;;; (granulated-sound-interp '(0 0 1 .1 2 1) 1.0 0.2 '(0 0 1 1 2 0) 0.02)



;;; -------- filtered-env 

(define filtered-env 
  (let ((+documentation+ "(filtered-env env snd chn) is a time-varying one-pole filter: when env is at 1.0, no filtering, 
as env moves to 0.0, low-pass gets more intense; amplitude and low-pass amount move together"))
    (lambda* (e snd chn)
      (let ((flt (make-one-pole 1.0 0.0)))
	(let ((xc (mus-xcoeffs flt))
	      (yc (mus-ycoeffs flt))
	      (amp-env (make-env e :length (framples))))
	  (map-channel
	   (lambda (val)
	     (let ((env-val (env amp-env)))
	       (float-vector-set! xc 0 env-val)
	       (float-vector-set! yc 1 (- env-val 1.0))
	       (one-pole flt (* env-val val))))
	   0 #f snd chn #f (format #f "filtered-env '~A" e)))))))



;;; -------- multi-colored rxvt printout
;;;
;;; if you're using display to write to rxvt, you can use the latter's escape sequences
;;;   for things like multi-colored text:

#|
(define red-text (format #f "~C[31m" #\escape))
(define normal-text (format #f "~C[0m" #\escape))

;;; there are a bunch of these:

(define black-on-red-text (format #f "~C[30m~C[41m" #\escape #\escape))

;;; or perhaps more convenient:

(define black-fg (format #f "~C[30m" #\escape))  (define black-bg (format #f "~C[40m" #\escape))
(define red-fg (format #f "~C[31m" #\escape))    (define red-bg (format #f "~C[41m" #\escape))
(define green-fg (format #f "~C[32m" #\escape))  (define green-bg (format #f "~C[42m" #\escape))
(define yellow-fg (format #f "~C[33m" #\escape)) (define yellow-bg (format #f "~C[43m" #\escape))
(define blue-fg (format #f "~C[34m" #\escape))   (define blue-bg (format #f "~C[44m" #\escape))
;;; etc (magenta: 35 cyan: 36 white: 37 default: 39)

(define bold-text (format #f "~C[1m" #\escape))       (define unbold-text (format #f "~C[22m" #\escape))  
(define underline-text (format #f "~C[4m" #\escape))  (define ununderline-text (format #f "~C[24m" #\escape))  
(define blink-text (format #f "~C[5m" #\escape))      (define unblink-text (format #f "~C[25m" #\escape))  
|#


;;; -------- remove-clicks 

(define find-click 
  (let ((+documentation+ "(find-click loc) finds the next click starting at 'loc'"))
    (lambda (loc)
      (let ((reader (make-sampler loc))
	    (mmax (make-moving-max 10))
	    (samp0 0.0)
	    (samp1 0.0)
	    (samp2 0.0)
	    (len (framples))
	    (local-max 0.0))
	(call-with-exit
	 (lambda (return)
	   (do ((ctr loc (+ ctr 1)))
	       ((= ctr len) #f)
	     (set! samp0 samp1)
	     (set! samp1 samp2)
	     (set! samp2 (next-sample reader))
	     (set! local-max (max .1 (moving-max mmax samp0)))
	     (if (and (> (abs (- samp0 samp1)) local-max)
		      (> (abs (- samp1 samp2)) local-max)
		      (< (abs (- samp0 samp2)) (/ local-max 2)))
		 (return (- ctr 1))))))))))

(define remove-clicks
  (let ((+documentation+ "(remove-clicks) tries to find and smooth-over clicks"))
    (lambda ()
      ;; this is very conservative -- the click detection limits above could be set much tighter in many cases
      (let remove-click ((loc 0))
        (let ((click (find-click loc)))
          (if click
              (begin
                (smooth-sound (- click 2) 4)
                (remove-click (+ click 2)))))))))


;;; -------- searching examples (zero+, next-peak)

(define search-for-click
  (let ((+documentation+ "(search-for-click) looks for the next click (use with C-s)"))
    (lambda ()
      (let ((samp0 0.0)
	    (samp1 0.0)
	    (samp2 0.0)
	    (mmax (make-moving-max 10))
	    (local-max 0.0))
	(lambda (val)
	  (set! samp0 samp1)
	  (set! samp1 samp2)
	  (set! samp2 val)
	  (set! local-max (max .1 (moving-max mmax samp0)))
	  (and (>= (abs (- samp0 samp1)) local-max)
	       (>= (abs (- samp1 samp2)) local-max)
	       (<= (abs (- samp0 samp2)) (/ local-max 2))))))))


(define zero+
  (let ((+documentation+ "(zero+) finds the next positive-going zero crossing (if searching forward) (for use with C-s)"))
    (lambda ()
      (let ((lastn 0.0))
	(lambda (n)
	  (let ((rtn (and (< lastn 0.0)
			  (>= n 0.0))))
	    (set! lastn n)
	    rtn))))))


(define next-peak
  (let ((+documentation+ "(next-peak) finds the next max or min point in the time-domain waveform (for use with C-s)"))
    (lambda ()
      (let ((last0 #f)
	    (last1 #f))
	(lambda (n)
	  (let ((rtn (and (number? last0)
			  (or (and (< last0 last1) (> last1 n))
			      (and (> last0 last1) (< last1 n))))))
	    (set! last0 last1)
	    (set! last1 n)
	    rtn))))))


(define find-pitch 
  (let ((+documentation+ "(find-pitch pitch) finds the point in the current sound where 'pitch' (in Hz) predominates -- C-s (find-pitch 300) 
In most cases, this will be slightly offset from the true beginning of the note")

	(interpolated-peak-offset 
	 (lambda (la pk ra)
	   (let ((logla (log (/ (max la .0000001) pk) 10))
		 (logra (log (/ (max ra .0000001) pk) 10)))
	     (/ (* 0.5 (- logla logra))
		(+ logla logra))))))
      
    (lambda (pitch)
      (let ((data (make-float-vector *transform-size*))
	    (data-loc 0))
	(lambda (n)
	  (set! (data data-loc) n)
	  (set! data-loc (+ data-loc 1))
	  (let ((rtn #f))
	    (if (= data-loc *transform-size*)
		(begin
		  (set! data-loc 0)
		  (if (> (float-vector-peak data) .001) ;ignore noise sections??
		      (let ((spectr (snd-spectrum data rectangular-window *transform-size*))
			    (pk 0.0)
			    (pkloc 0))
			(let ((pit 
			       (do ((i 0 (+ i 1)))
				   ((= i (/ *transform-size* 2)) 
				    (if (or (= pk 0.0)
					    (= pkloc 0))
					0.0
					(/ (* (+ pkloc
						 (interpolated-peak-offset (spectr (- pkloc 1))
									   pk
									   (spectr (+ 1 pkloc))))
					      (srate))
					   *transform-size*)))
				 (if (> (spectr i) pk)
				     (begin
				       (set! pk (spectr i))
				       (set! pkloc i))))))
			  (if (< (abs (- pitch pit)) (/ (srate) 2 *transform-size*)) ; uh... why not do it direct?
			      (set! rtn #t)))))
		  (fill! data 0.0)))
	    rtn))))))


;;; -------- file->floats and a sort of cue-list, I think

(define (file->floats file) (samples 0 (framples file) file))


(define add-notes 
  (let ((+documentation+ "(add-notes notes snd chn) adds (mixes) 'notes' which is a list of lists of the form: file (offset 0.0) (amp 1.0) 
starting at the cursor in the currently selected channel: (add-notes '((\"oboe.snd\") (\"pistol.snd\" 1.0 2.0)))"))
    (lambda* (notes snd chn)
      (let ((start (cursor snd chn)))
	(as-one-edit
	 (lambda ()
	   (for-each 
	    (lambda (note)
	      (let ((file (car note))
		    (amp (and (> (length note) 2) (caddr note)))
		    (beg (+ start (floor (* (srate snd) 
					     (if (> (length note) 1) 
						 (cadr note)
						 0.0))))))    
		(if (and (number? amp)
			 (not (= amp 1.0)))
		    (mix-float-vector (float-vector-scale! (file->floats file) amp) beg snd chn #f "add-notes")
		    (mix file beg 0 snd chn #f))))
	    notes))
	 (format #f "add-notes '~S" notes))))))


(define region-play-list 
  (let ((+documentation+ "(region-play-list data): 'data' is list of lists (list (list reg time)...), time in secs, setting up 
a sort of play list: (region-play-list (list (list reg0 0.0) (list reg1 0.5) (list reg2 1.0) (list reg0 1.0)))"))
    (lambda (data)
      (for-each
       (lambda (tone)
	 (let ((time (floor (* 1000 (cadr tone))))
	       (region (car tone)))
	   (if (region? region)
	       (in time (lambda () (play region))))))
       data))))


(define region-play-sequence 
  (let ((+documentation+ "(region-play-sequence data): 'data' is list of regions which will be played one after the other: (region-play-sequence (list reg0 reg2 reg1))"))
    (lambda (data)
      (let ((time 0.0))
	(region-play-list
	 (map 
	  (lambda (id)
	    (let ((cur time))
	      (set! time (+ time (/ (framples id) (srate id))))
	      (list cur id)))
	  data))))))


;;; -------- explode-sf2

(define explode-sf2
  (let ((+documentation+ "(explode-sf2) turns the currently selected soundfont file into a bunch of files of the form sample-name.aif"))
    (lambda ()
      (let sf2it ((lst (soundfont-info)))
	(if (pair? lst)
	    (let* ((vals (car lst))
		   (start (cadr vals)))
	      (let ((end (if (null? (cdr lst))
			     (framples)
			     (cadadr lst)))
		    (loop-start (- (caddr vals) start))
		    (loop-end (- (cadddr vals) start))
		    (filename (string-append (car vals) ".aif")))
		(if (selection?)
		    (set! (selection-member? #t) #f))
		(set! (selection-member?) #t)
		(set! (selection-position) start)
		(set! (selection-framples) (- end start))
		(save-selection filename (selection-srate) mus-bshort mus-aifc)
		(let ((temp (open-sound filename)))
		  (set! (sound-loop-info temp) (list loop-start loop-end))
		  (close-sound temp))
		(sf2it (cdr lst)))))))))


;;; -------- open-next-file-in-directory

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
      (set! current-sorted-files (sort! (copy (sound-files-in-directory dir)) string<?)))
    
    (define (get-current-directory filename)
      (set! last-file-opened filename)
      (display last-file-opened)
      (let ((new-path (directory-from-path (file-name filename))))
	(if (not (equal? current-directory new-path))
	    (get-current-files new-path)))
      #f)
    
    (lambda ()
      (if (not (member get-current-files (hook-functions open-hook)))
	  (hook-push open-hook (lambda (hook) (get-current-directory (hook 'name)))))
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
		      (close-sound (or (selected-sound)
				       (car (sounds)))))
		  (open-sound next-file)))))
      #t)))


(define click-middle-button-to-open-next-file-in-directory
  (let ((+documentation+ "(click-middle-button-to-open-next-file-in-directory) adds open-next-file-in-directory to the mouse-click-hook"))
    (lambda ()
      (hook-push mouse-click-hook
		 (lambda (hook)
		   (if (= (hook 'button) 2)
		       (set! (hook 'result) (open-next-file-in-directory))))))))


;;; -------- chain-dsps

(define chain-dsps 
  (let ((+documentation+ "(chain-dsps beg dur :rest dsps) sets up a generator patch from its arguments"))
    (lambda* (beg dur :rest dsps)
      
      ;; assume the dsps are already made, 
      ;;        the envs are present as break-point lists
      ;;        the calls are ordered out->in (or last first)
      ;; we take this list and create and evaluate a new function
      
      (let ((dsp-chain (reverse (apply vector (map (lambda (gen)
						     (if (pair? gen)
							 (make-env gen :duration dur)
							 gen))
						   dsps))))
	    (start (seconds->samples beg))
	    (samps (seconds->samples dur))
	    (body 0.0)
	    (closure ()))
	(let ((end (+ start samps))
	      (len (length dsp-chain)))
	  
	  ;; create the let variable list and lambda body of our new function
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (let ((g (dsp-chain i))
		  (gname (string->symbol (format #f "g~D" i))))
	      (set! closure (cons (list gname (list 'dsp-chain i)) closure))
	      (set! body (cond ((env? g)
				(if (eqv? body 0.0)
				    (list 'env gname)
				    (list '* (list 'env gname) body)))
			       ((readin? g)
				(if (eqv? body 0.0)
				    (list 'readin gname)
				    (list '+ body (list 'readin gname))))
			       ((not (mus-generator? g))
				(list gname body))
			       ((eqv? body 0.0)
				(list (string->symbol (mus-name g)) gname))
			       (else
				(list (string->symbol (mus-name g)) gname body))))))
	  
	  ;; now patch the two together (the apply let below) and evaluate the resultant thunk
	  (apply define (list 'inner)
		 `((let ,closure
		     (do ((k ,start (+ k 1)))
			 ((= k ,end))
		       (outa k ,body)))))
	  (inner))))))
#|
(with-sound ()
  (chain-dsps 0 1.0 '(0 0 1 .5 2 0) (make-oscil 440))
  (chain-dsps 1 1.0 '(0 0 1 4 2 0) (make-one-zero .5) (make-readin "oboe.snd"))
  (chain-dsps 2 1.0 '(0 0 1 .5 2 0) (let ((osc1 (make-oscil 220)) 
					  (osc2 (make-oscil 440))) 
				      (lambda (val) (+ (osc1 val) 
						       (osc2 (* 2 val)))))))
|#



;;; amplitude-modulate-channel could be (lambda (y data forward) (* y 0.5 (+ 1.0 (sin angle))) etc ...)


;;; -------- re-order channels 

(define scramble-channels
  
  (letrec ((+documentation+ "scramble-channels can arbitrarily re-order a sound's channels. The new channel order is \
passed as the arguments so to end with channel 3 in channel 0, 2 in 1, 0 in 2, and 1 in 3, (scramble-channels 3 2 0 1)")
      
	   (scramble-channels-1
	    (let ((find-chan (lambda (chans chan len)
			       (do ((pos #f)
				    (i 0 (+ i 1)))
				   ((or pos (= i len)) pos)
				 (if (= (chans i) chan)
				     (set! pos i))))))
	      (lambda (cur-chans end-chans chans loc)
		(if (> chans loc)
		    (let* ((end-chan (end-chans loc)) ; we want this channel at loc
			   (cur-chan (cur-chans loc)) ; this (original) channel is currently at loc
			   (end-loc (find-chan cur-chans end-chan chans))) ; where is end-chan currently?
		      ;; end-chan goes in cur-chan's slot
		      (if (not (= cur-chan end-chan))
			  (begin
			    (swap-channels #f end-loc #f loc)
			    (set! (cur-chans end-loc) cur-chan)
			    (set! (cur-chans loc) end-chan)))
		      (scramble-channels-1 cur-chans end-chans chans (+ 1 loc))))))))
      
    (lambda new-order
      (let ((len (length new-order)))
	(if (> len 1)
	    (let ((end-chans (apply vector new-order))
		  (cur-chans (make-vector len)))
	      (do ((i 0 (+ i 1)))
		  ((= i len))
		(set! (cur-chans i) i))
	      (scramble-channels-1 cur-chans end-chans len 0)))))))


(define (scramble-channel silence-1)
  ;; (scramble-channel .01)
  (let ((buffer (make-moving-average 128))
	(silence (/ silence-1 128))
	(edges ())
	(in-silence #t)
	(old-max *max-regions*)
	(old-tags *with-mix-tags*))
    (dynamic-wind
	(lambda ()
	  (set! *max-regions* 1024)
	  (set! *with-mix-tags* #f))
	(lambda ()
	  (let ((len (framples))
		(reader (make-sampler)))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (let ((now-silent (let ((sum-of-squares (let ((y (next-sample reader)))
							(moving-average buffer (* y y)))))
				  (< sum-of-squares silence))))
		(if (not (eq? in-silence now-silent))
		    (set! edges (cons i edges)))
		(set! in-silence now-silent))))
	  (set! edges (append (reverse edges) (list (framples))))
	  (let ((len (length edges)))
	    (let ((pieces (make-vector len #f))
		  (start 0)
		  (ctr 0))
	      (for-each
	       (lambda (end)
		 (set! (pieces ctr) (make-region start end))
		 (set! ctr (+ ctr 1))
		 (set! start end))
	       edges)
	      (set! start 0)
	      (as-one-edit
	       (lambda()
		 (scale-by 0.0)
		 (do ((i 0 (+ i 1)))
		     ((= i len))
		   (let* ((this (random len))
			  (reg (pieces this)))
		     (set! (pieces this) #f)
		     (if (not reg)
			 (begin
			   (do ((j (+ 1 this) (+ j 1)))
			       ((or (= j len)
				    reg))
			     (set! reg (pieces j))
			     (if reg (set! (pieces j) #f)))
			   (if (not reg)
			       (do ((j (- this 1) (- j 1)))
				   ((or (< j 0)
					reg))
				 (set! reg (pieces j))
				 (if reg (set! (pieces j) #f))))))
		     (mix-region reg start)
		     (set! start (+ start (framples reg)))
		     (forget-region reg))))))))
	(lambda ()
	  (set! *with-mix-tags* old-tags)
	  (set! *max-regions* old-max)))))


;; -------- reorder blocks within channel

(define reverse-by-blocks 
  (let ((+documentation+ "(reverse-by-blocks block-len snd chn): divide sound into block-len blocks, recombine blocks in reverse order"))
    (lambda* (block-len snd chn)
      (let* ((len (framples snd chn))
	     (num-blocks (floor (/ len (srate snd) block-len))))
	(if (> num-blocks 1)
	    (let ((actual-block-len (ceiling (/ len num-blocks))))
	      (let ((rd (make-sampler (- len actual-block-len) snd chn))
		    (beg 0)
		    (ctr 1))
		(map-channel
		 (lambda (y)
		   (let ((val (read-sample rd)))
		     (if (< beg 10) ; ramp start and end to avoid clicks (might want to mix with next section)
			 (set! val (* val beg .1))
			 (if (> beg (- actual-block-len 10))
			     (set! val (* val (- actual-block-len beg) .1))))
		     (set! beg (+ beg 1))
		     (if (= beg actual-block-len)
			 (begin
			   (set! ctr (+ ctr 1))
			   (set! beg 0)
			   (set! rd (make-sampler (max 0 (- len (* ctr actual-block-len))) snd chn))))
		     val))
		 0 #f snd chn #f (format #f "reverse-by-blocks ~A" block-len)))))))))


(define reverse-within-blocks 
  (let ((+documentation+ "(reverse-within-blocks block-len snd chn): divide sound into blocks, recombine in order, but each block internally reversed"))
    (lambda* (block-len snd chn)
      (let* ((len (framples snd chn))
	     (num-blocks (floor (/ len (srate snd) block-len))))
	(if (> num-blocks 1)
	    (let ((actual-block-len (ceiling (/ len num-blocks)))
		  (no-clicks-env (list 0.0 0.0  .01 1.0  .99 1.0  1.0 0.0)))
	      (as-one-edit
	       (lambda ()
		 (do ((beg 0 (+ beg actual-block-len)))
		     ((>= beg len))
		   (reverse-channel beg actual-block-len snd chn)
		   (env-channel no-clicks-env beg actual-block-len snd chn)))
	       (format #f "reverse-within-blocks ~A" block-len)))
	    (reverse-channel 0 #f snd chn))))))


;;; -------- channel-clipped?

#|
(define channel-clipped? 
  (let ((+documentation+ "(channel-clipped? snd chn) returns the sample number if it finds clipping"))
    (lambda* (snd chn)
      (let ((last-y 0.0)
	    (len (framples snd chn))
	    (reader (make-sampler 0 snd chn)))
	(call-with-exit
	 (lambda (quit)
	   (do ((i 0 (+ i 1)))
	       ((= i len) #f)
	     (let ((y (next-sample reader)))
	       (if (and (>= (abs y) 0.9999)
			(>= (abs last-y) 0.9999))
		   (quit i)
		   (set! last-y y))))))))))
|#
;;; not pretty but faster:

(define channel-clipped? 
  (let ((+documentation+ "(channel-clipped? snd chn) returns the sample number if it finds clipping"))
    (lambda* (snd chn)
      (do ((pos (scan-channel (lambda (y) (>= (abs y) 0.9999)) 0 #f snd chn) 
		(scan-channel (lambda (y) (>= (abs y) 0.9999)) (+ pos 1) #f snd chn)))
	  ((or (not pos)
	       (>= (abs (sample (+ pos 1) snd chn)) 0.9999))
	   pos))))) ; or (and pos (+ pos 1)) to mimic the old version


;;; -------- sync-everything

(define sync-everything
  (let ((+documentation+ "(sync-everything) sets the sync fields of all currently open sounds to the same, unique value"))
    (lambda ()
      (let ((new-sync (+ 1 (sync-max))))
	(for-each
	 (lambda (snd)
	   (set! (sync snd) new-sync))
	 (sounds))))))
