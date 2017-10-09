\ examp.fs -- examples from examp.scm|rb

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/07/05 13:09:37
\ Changed: 15/01/29 23:39:32
\
\ @(#)examp.fs	1.68 1/29/15

\ With original comments and doc strings from examp.scm.
\
\ all-chans		( -- array-of-lists )
\ close-sound-extend	( snd -- )
\ snd-snd		( :optional snd -- snd )
\ snd-chn		( :optional chn -- chn )
\ make-read-eval-loop	( -- )
\ remove-read-eval-loop	( -- )
\ toggle-read-eval-loop	( -- )
\
\ from frame.scm
\ insert-vct		( v :optional beg dur snd chn edpos -- samps )
\
\ examp.(scm|rb)
\ selection-rms		( -- val )
\ region-rms		( :optional n -- val )
\ window-samples	( :optional snd chn -- vct )
\ display-energy	( snd chn -- val )
\ display-db		( snd chn -- val )
\ window-rms		( -- val )
\ fft-peak		( snd chn scaler -- pk )
\ finfo			( file -- str )
\ display-correlate	( snd chn y0 y1 -- val )
\ zoom-spectrum		( snd chn y0 y1 -- val )
\ superimpose-ffts	( snd chn y0 y1 -- val )
\ locate-zero		( limit -- samp )
\ mpg			( mpgfile rawfile -- )
\ auto-dot		( snd chn y0 y1 -- val )
\ first-mark-in-window-at-left ( -- )
\ flash-selected-data	( millisecs -- )
\ mark-loops		( -- )
\
\ do-all-chans		( func :optional origin -- )
\ update-graphs		( -- )
\ do-chans		( func :optional origin -- )
\ do-sound-chans	( func :optional origin -- )
\ every-sample?		( func -- f )
\ sort-samples		( nbins -- ary )
\ place-sound		( mono stereo pan -- res )
\ fft-edit		( bottom top :optional snd chn -- vct )
\ fft-squelch		( squelch :optional snd chn -- scl )
\ fft-cancel		( lo-freq hi-freq :optional snd chn -- vct )
\ make-ramp		( :optional size -- gen )
\ ramp			( gen up -- val )
\ squelch-vowels	( :optional snd chn -- val )
\ fft-env-data		( fft-env :optional snd chn -- vct )
\ fft-env-edit		( fft-env :optional snd chn -- vct )
\ fft-env-interp	( env1 env2 interp :optional snd chn -- vct )
\ filter-fft		( flt :optional normalize snd chn -- val )
\ fft-smoother		( cutoff start samps :optional snd chn -- val )
\ comb-filter		( scaler size -- prc; x self -- res )
\ comb-chord		( scaler size amp -- prc; x self -- res )
\ zcomb			( scaler size pm -- prc; x self -- val )
\ notch-filter		( scaler size -- prc; x self -- val )
\ formant-filter	( radius frequency -- prc; x self -- val )
\ formants		( r1 f1 r2 f2 r3 f3 -- prc; x self -- val )
\ moving-formant	( radius move -- prc; x self -- val )
\ osc-formants		( radius bases amounts freqs -- prc; x self -- val )
\ 
\ echo			( scaler secs -- prc; y self -- val )
\ zecho			( scaler secs freq amp -- prc; y self -- val )
\ flecho		( scaler secs -- prc; y self -- val )
\ ring-mod		( freq gliss-env -- prc; y self -- val )
\ am			( freq -- prc; y self -- val )
\ vibro			( speed depth -- prc; y self -- val )
\ 
\ hello-dentist		( frq amp :optional snd chn -- vct )
\ fp			( sr osamp osfrq :optional snd chn -- vct )
\ compand		( -- prc; y self -- val )
\ expsrc		( rate :optional snd chn -- val )
\ expsnd		( gr-env :optional snd chn -- vct )
\ cross-synthesis	( cross-snd amp fftsize r -- prc; y self -- val )
\ voiced->unvoiced	( amp fftsize r tempo :optional snd chn -- vct )
\ pulse-voice		( cos :optional freq amp fftsize r snd chn -- vct )
\ cnvtest		( snd0 snd1 amp -- mx )
\ swap-selection-channels ( -- )
\ make-sound-interp	( start :optional snd chn -- prc; loc self -- val )
\ sound-interp		( func loc -- val )
\ env-sound-interp	( env :optional time-scale snd chn -- vct )
\ granulated-sound-interp ( e :optional tscl glen genv ohop snd chn -- vct )
\ filtered-env		( e :optional snd chn -- val )
\  
\ find-click		( loc -- pos )
\ remove-clicks		( -- )
\ search-for-click	( -- pos )
\ zero+			( -- prc; n self -- val )
\ next-peak		( -- prc; n self -- val )
\ find-pitch		( pitch -- prc; y self -- val )
\ file->vct		( file -- vct )
\ add-notes		( notes :optional snd chn -- #f )
\ region-play-list	( data -- )
\ region-play-sequence	( data -- )
\ replace-with-selection ( -- )
\ explode-sf2		( -- )
\ open-next-file-in-directory ( -- f )
\ click-middle-button-to-open-next-file-in-directory ( -- )
\ chain-dsps		( start dur :optional dsps -- )
\ 
\ scramble-channels	( new-order -- )
\ scramble-channel	( silence -- )
\ reverse-by-blocks	( block-len :optional snd chn -- val )
\ reverse-within-blocks	( block-len :optional snd chn -- val )
\ channel-clipped?	( :optional snd chn -- val )
\ sync-everything	( -- )
\ 
\ make-moog-filter	( freq Q -- gen )
\ moog-frequecy@	( gen -- frq )
\ moog-frequecy!	( frq gen -- )
\ moog-filter		( gen sig -- A )

'snd-nogui provided? [if]
	<'> noop alias show-widget ( a -- a )
	<'> noop alias hide-widget ( a -- a )
	<'> noop alias widget-size ( a -- a )
	: set-widget-size     ( a b -- a )    drop ;
	: window-property     ( a b -- a )    drop ;
	: set-window-property ( a b c -- a ) 2drop ;
[then]

\ #( #( snd0 chn0 ) #( snd0 chn1 ) ... )
: all-chans ( -- array-of-lists )
	nil { snd }
	#() ( ary )
	sounds each to snd
		snd channels 0 ?do
			( ary ) #( snd i ) array-push
		loop
	end-each
	( ary )
;

\ 5 == notebook widget
: close-sound-extend <{ snd -- }>
	main-widgets 5 object-ref if
		sounds snd object-index 0 max { idx }
		snd close-sound drop
		sounds empty? unless
			sounds
			    idx  sounds length < if
				    idx
			    else
				    -1
			    then object-ref set-selected-sound drop
		then
	else
		snd close-sound drop
	then
;

: snd-snd <{ :optional snd #f -- snd }>
	snd sound? if
		snd
	else
		selected-sound to snd
		snd sound? if
			snd
		else
			sounds car
		then
	then
;

: snd-chn <{ :optional chn #f -- chn }>
	chn integer? if
		chn
	else
		#f selected-channel to chn
		chn integer? if
			chn
		else
			0
		then
	then
;

\ === Traditional Forth Read-Eval-Loop for Snd's listener. ===

"ok " constant read-eval-loop-prompt

hide
#f value rel-ficl-stack
#f value rel-old-listener-prompt
#f value rel-old-hooks

: rel-cb <{ text -- flag }>
	rel-ficl-stack restore-stack
	$space snd-print drop
	text string-eval save-stack to rel-ficl-stack
	$cr snd-print drop
	read-eval-loop-prompt snd-print drop
	\ reset-listener-cursor returns #f
	reset-listener-cursor not
;

: rel-add ( -- )
	listener-prompt to rel-old-listener-prompt
	read-eval-loop-prompt set-listener-prompt drop
	read-hook hook->array to rel-old-hooks
	read-hook reset-hook!
	read-hook <'> rel-cb add-hook!
;

: rel-remove ( -- )
	read-hook <'> rel-cb remove-hook!
	nil { prc }
	rel-old-hooks empty? unless
		rel-old-hooks each to prc
			read-hook prc add-hook!
		end-each
	then
	rel-old-listener-prompt set-listener-prompt drop
;

: rel-toggle ( key -- )
	( key ) case
		'on of
			read-hook <'> rel-cb hook-member? unless
				rel-add
			then
		endof
		'off of
			rel-remove
		endof
		'toggle of
			read-hook <'> rel-cb hook-member? if
				rel-remove
			else
				rel-add
			then
		endof
	endcase
	#f to rel-ficl-stack
	stack-reset
	reset-listener-cursor drop
;
set-current

<'> noop alias ok

: make-read-eval-loop   ( -- )   'on rel-toggle ;
: remove-read-eval-loop ( -- )   'off rel-toggle ;
: toggle-read-eval-loop ( -- )   'toggle rel-toggle ;
previous

require clm
require env
require rgb

\ === from frame.scm
\
: insert-vct <{ v :optional beg 0 dur #f snd #f chn #f edpos #f -- samps }>
	doc" Insert vct V's data into sound SND at BEG."
	v vct? v 1 "a vct" assert-type
	dur v vct-length || { len }
	beg len v snd chn edpos #f "%S %s %s %s"
	    #( v beg dur get-func-name ) string-format insert-samples
;

\ === examp.scm|rb
\ 
\ this mainly involves keeping track of the current sound/channel
: selection-rms ( -- val )
	doc" Return rms of selection data using sample readers."
	undef selection? unless
		'no-active-selection #( get-func-name ) fth-throw
	then
	selection-position #f #f 1 #f make-sampler { rd }
	selection-framples { len }
	0.0 ( sum ) len 0 ?do
		rd next-sample dup f* f+ ( sum += ... )
	loop
	len f/ fsqrt
;

: region-rms <{ :optional reg 0 -- val }>
	doc" Return rms of region N's data (chan 0)."
	reg region? unless
		'no-such-region #( "%s: %s" get-func-name reg ) fth-throw
	then
	reg 0 0 region->vct { data }
	data dup dot-product data length f/ fsqrt
;

: window-samples <{ :optional snd #f chn #f -- vct }>
	doc" Sample in SND channel CHN in current graph window."
	snd chn left-sample { wl }
	snd chn right-sample { wr }
	wl  wr wl - 1+  snd chn #f channel->vct
;

: display-energy <{ snd chn -- v }>
	doc" A lisp-graph-hook function to display the time \
domain data as energy (squared).\n\
list-graph-hook <'> display-energy add-hook!."
	snd chn undef undef undef make-graph-data dup array? if
		1 array-ref
	then { data }
	data if
		snd chn left-sample { ls }
		snd chn right-sample { rs }
		snd srate { sr }
		snd chn y-zoom-slider { y-max }
		data data vct-multiply! "energy" ls sr f/ rs sr f/
		    0.0 y-max dup f*  snd chn #t undef graph
	else
		#f
	then
;
\ lisp-graph-hook <'> display-energy add-hook!

hide
: db-calc { val -- r }
	val 0.001 f< if
		-60.0
	else
		20.0 val flog10 f*
	then
;
set-current

: display-db <{ snd chn -- v }>
	doc" A lisp-graph-hook function to display \
the time domain data in dB.\n\
list-graph-hook <'> display-db add-hook!."
	snd chn undef undef undef make-graph-data dup array? if
		1 array-ref
	then { data }
	data if
		snd chn left-sample { ls }
		snd chn right-sample { rs }
		snd srate { sr }
		data map
			*key* fabs db-calc 60.0 f+
		end-map "dB" ls sr f/ rs sr f/ 0.0 60.0 snd chn #t undef graph
	else
		#f
	then
;
previous
\ lisp-graph-hook <'> display-db add-hook!

: window-rms ( -- val )
	doc" Return rms of data in currently selected graph window."
	#f #f left-sample  { ls }
	#f #f right-sample { rs }
	ls rs ls - 1+ #f #f #f channel->vct { data }
	data vct-length { len }
	data dup len dot-product len f/ fsqrt
;

: fft-peak <{ snd chn scaler -- pk }>
	doc" print peak spectral magnitude\n\
Returns the peak spectral magnitude.  \
It is intended for use with after-transform-hook."
	snd chn transform-graph?
	snd chn transform-graph-type graph-once = && if
		snd chn #f transform->vct vct-peak f2*
		    snd chn transform-size f/ { pk }
		pk number->string snd status-report drop
		pk
	else
		#f
	then
;
\ after-transform-hook <'> fft-peak add-hook!

\ ;;; -------- 'info' from extsnd.html using format --------

: finfo ( file -- str )
	doc" Return description (as a string) of file."
	find-file { file }
	file unless 
		'no-such-file #( "%s: %S" get-func-name file ) fth-throw
	then
	"%s: chans: %d, srate: %d, %s, %s, len: %1.3f"
	    #( file
	       file mus-sound-chans
	       file mus-sound-srate
	       file mus-sound-header-type mus-header-type-name
	       file mus-sound-sample-type mus-sample-type-name
	       file mus-sound-samples
	       file mus-sound-chans
	       file mus-sound-srate f* f/ ) string-format
;

\ ;;; -------- Correlation --------
\ ;;;
\ ;;; correlation of channels in a stereo sound

: display-correlate <{ snd chn y0 y1 -- val }>
	doc" Return the correlation of SND's 2 channels (intended \
for use with graph-hook).  \
y0 and y1 are ignored."
	snd channels 2 = if
		snd 0 #f framples 1 >
		snd 1 #f framples 1 > && if
			snd chn left-sample  { ls }
			snd chn right-sample { rs }
			rs ls - 1+ { ilen }
			ilen flog 2.0 flog f/ fround->s { pow2 }
			2.0 pow2 f** fround->s { fftlen }
			fftlen 2/ { fftlen2 }
			fftlen 1/f { fftscale }
			ls fftlen snd 0 #f channel->vct { rl1 }
			ls fftlen snd 1 #f channel->vct { rl2 }
			fftlen 0.0 make-vct { im1 }
			fftlen 0.0 make-vct { im2 }
			rl1 im1 1 fft drop
			rl2 im2 1 fft drop
			rl1 vct-copy { tmprl }
			im1 vct-copy { tmpim }
			fftlen2 0.0 make-vct { data3 }
			tmprl rl2 vct-multiply! drop
			tmpim im2 vct-multiply! drop
			im2   rl1 vct-multiply! drop
			rl2   im1 vct-multiply! drop
			tmprl tmpim 0 vct-add! drop
			im2 rl2 vct-subtract! drop
			tmprl im2 -1 fft drop
			data3 tmprl 0 vct-add! drop
			data3 fftscale vct-scale! drop
			data3 "lag time" 0 fftlen2
			    undef undef snd chn #t undef graph
		then
	else
		get-func-name " wants stereo input" $+ snd status-report
	then
;
\ graph-hook <'> display-correlate add-hook!

\ ;;; -------- set transform-size based on current time domain window size
\ ;;;
\ ;;; also zoom spectrum based on y-axis zoom slider

: zoom-spectrum <{ snd chn y0 y1 -- val }>
	doc" Set the transform size to correspond to the \
time-domain window size (use with graph-hook)."
	snd chn transform-graph?
	snd chn transform-graph-type graph-once = && if
		2.0 snd chn right-sample snd chn left-sample f-
		    flog 2.0 flog f/ fceil f** fround->s { val }
		val 0> if
			val snd chn set-transform-size drop
		then
		snd chn y-zoom-slider snd chn set-spectrum-end drop
	then
	#f
;
\ graph-hook <'> zoom-spectrum add-hook!

\ ;;; -------- superimpose spectra of sycn'd sounds

: superimpose-ffts <{ snd chn y0 y1 -- val }>
	doc" Superimpose ffts of multiple (syncd) sounds (use with graph-hook)."
	0 ( maxsync )
	sounds each ( snd )
		sync max
	end-each { maxsync }
	0 sounds each { n }
		n sync snd sync = if
			n
		else
			maxsync 1+
		then min
	end-each { sndsync }
	snd chn left-sample  { ls }
	snd chn right-sample { rs }
	snd sync 0>
	rs ls > &&
	snd sndsync = && if
		rs ls f- 1.0 fmax 2.0 flog fceil->s { pow2 }
		2.0 pow2 f** floor->s { fftlen }
		pow2 2 > if
			#() { ffts }
			sounds each { n }
				n sync snd sync =
				n channels chn > && if
					ls fftlen n chn #f channel->vct { fdr }
					fftlen 0.0 make-vct { fdi }
					fftlen 2/ 0.0 make-vct ( spectr )
					    fdr fdi #f 2 spectrum
					    0 vct-add! { val }
					ffts #( val ) array-push drop
				then
			end-each
			ffts "spectra" 0.0 0.5 y0 y1
			    snd chn #t undef graph drop
		then
	then
	#f
;
\ graph-hook <'> superimpose-ffts add-hook!

\ ;;; -------- c-g? example (Anders Vinjar)

: locate-zero ( limit -- samp )
	doc" Look for successive samples that sum to less than LIMIT, \
moving the cursor if successful."
	{ limit }
	#f #f #f cursor { start }
	start #f #f 1 #f make-sampler { sf }
	sf next-sample fabs { val0 }
	sf next-sample fabs { val1 }
	begin
		sf sampler-at-end?
		val0 val1 f+ limit f< || not
	while
		start 1+ to start
		val1 to val0
		sf next-sample fabs to val1
	repeat
	sf free-sampler drop
	start #f #f #f set-cursor
;

\ ;;; -------- translate mpeg input to 16-bit linear and read into Snd
\ ;;;
\ ;;; mpg123 with the -s switch sends the 16-bit (mono or stereo)
\ ;;;   representation of an mpeg file to stdout.  There's also
\ ;;;   apparently a switch to write 'wave' output.

: mpg ( mpgfile rawfile -- )
	doc" Convert file from MPEG to raw 16-bit samples using mpg123."
	{ mpgfile rawfile }
	mpgfile io-open-read { io }
	io io-getc { b0 }
	io io-getc { b1 }
	io io-getc { b2 }
	io io-getc { b3 }
	io io-close
	b0 255 <>
	b1 0b11100000 and 0b11100000 <> || if
		"%s is not an MPEG file (first 11 bytes: %b %b)"
		    #( mpgfile b0 b1 0b11100000 and ) clm-print
		exit
	then
	b1 0b11000    and 3 rshift { id }
	b1 0b110      and 1 rshift { layer }
	b2 0b1100     and 2 rshift { srate-index }
	b3 0b11000000 and 6 rshift { channel-mode }
	id 1 = if
		"odd: %s is using a reserved Version ID"
		    #( mpgfile ) clm-print
	then
	layer 0= if
		"odd: %s is using a reserved layer description"
		    #( mpgfile ) clm-print
	then
	channel-mode 3 = if
		1
	else
		2
	then { chans }
	id 0= if
		4
	else
		id 2 = if
			2
		else
			1
		then
	then { mpegnum }
	layer 3 = if
		1
	else
		layer 2 = if
			2
		else
			3
		then
	then { mpeg-layer }
	#( 44100 48000 32000 0 ) srate-index array-ref mpegnum / { srate }
	"%s: %s Hz, %s, MPEG-%s"
	    #( mpgfile
	       srate
	       chans 1 = if
		       "mono"
	       else
		       "stereo"
	       then mpeg-layer ) clm-print
	"mpg123 -s %s > %s" #( mpgfile rawfile ) string-format file-system if
		rawfile chans srate little-endian? if
			mus-lshort
		else
			mus-bshort
		then open-raw-sound drop
	else
		"system in %s: %s" #( get-func-name exit-status ) fth-throw
	then
;
\ "mpeg.mpg" "mpeg.raw" mpg

\ ;;; -------- read ASCII files
\ ;;;
\ ;;; these are used by Octave (WaveLab) -- each line has one integer,
\ ;;;   apparently a signed short.

hide
: read-ascii-cb { fname snd -- prc; self -- }
	0 proc-create ( prc )
	fname , snd ,
  does> { self -- }
	self @ ( fname ) readlines { in-buffer }
	self cell+ @ { snd }
	512 { bufsize }
	bufsize 0.0 make-vct { data }
	0 { loc }
	0 { frame }
	32768.0 1/f { short->float }
	nil { val }
	in-buffer each ( line )
		nil string-split each ( str-val )
			string->number ( val ) short->float f*
			    data loc rot vct-set! drop
			loc 1+ to loc
			loc bufsize = if
				data frame bufsize snd 0 vct->channel drop
				frame bufsize d+ to frame
				0 to loc
			then
		end-each
	end-each
	loc d0> if
		data frame loc snd 0 vct->channel drop
	then
;
set-current

: read-ascii
    <{ fl :optional sf "test.snd" st mus-next ht mus-bshort sr 44100 -- snd }>
	doc" Try to read an ASCII sound file."
	"created by %s: %s" #( get-func-name fl ) string-format { com }
	sf 1 sr ht st com new-sound { snd }
	fl snd read-ascii-cb as-one-edit drop
	snd
;
previous

\ ;;; -------- make dot size dependent on number of samples being displayed
\ ;;; 
\ ;;; this could be extended to set time-graph-style to graph-lines
\ ;;; if many samples are displayed, etc

: auto-dot <{ snd chn y0 y1 -- val }>
	doc" Set the dot size depending on the number of \
samples being displayed (use with graph-hook)."
	snd chn right-sample snd chn left-sample - { dots }
	dots 100 > if
		1
	else
		dots 50 > if
			2
		else
			dots 25 > if
				3
			else
				5
			then
		then
	then snd chn set-dot-size
;
\ graph-hook <'> auto-dot add-hook!

\ ;;; -------- move window left edge to mark upon 'm'
\ ;;;
\ ;;; in large sounds, it can be pain to get the left edge of the window
\ ;;; aligned with a specific spot in the sound.  In this code, we assume
\ ;;; the desired left edge has a mark, and the 'm' key (without control)
\ ;;; will move the window left edge to that mark.

: first-mark-in-window-at-left <{ -- val }>
	doc" Move the graph so that the leftmost \
visible mark is at the left edge."
	#f snd-snd { keysnd }
	#f snd-chn { keychn }
	keysnd keychn left-sample { current-left-sample }
	keysnd keychn #f marks { chan-marks }
	chan-marks length 0= if
		"no marks" status-report
	else
		#f ( flag )
		chan-marks map
			*key* undef mark-sample
		end-map each { samp }
			samp current-left-sample > if
				drop 	\ drop #f
				samp
				leave
			then
		end-each { leftmost }
		leftmost if
			leftmost keysnd keychn set-left-sample
		else
			"no mark in window" status-report
		then
	then drop
	keyboard-no-action
;
\ "m" 0 <'> first-mark-in-window-at-left
\     #f "align window left edge with mark" dup bind-key drop

\ ;;; -------- flash selected data red and green

hide
: fsd-cb { ms xt -- prc; self -- val }
	0 proc-create ( prc )
	ms , xt ,
  does> { self -- val }
	self @ ( ms ) self cell+ @ ( xt ) execute
	#f
;
user data-red?
#t data-red? !
set-current

: flash-selected-data ( millisecs -- )
	doc" Cause the selected data to flash red and green."
	{ ms }
	selected-sound sound? if
		data-red? @ if
			green
		else
			red
		then set-selected-data-color drop
		data-red? @ not data-red? !
		ms ms running-word fsd-cb in drop
	then
;
previous

\ ;;; --------  use loop info (if any) to set marks at loop points

: mark-loops ( -- )
	doc" Place marks at loop points found in the selected sound's header."
	#f snd-snd { snd }
	#f snd-chn { chn }
	snd sound-loop-info
	snd file-name mus-sound-loop-info || { loops }
	loops empty? if
		"%s has no loop info"
		    #( snd short-file-name ) string-format
		    snd status-report drop
		exit
	then
	loops 0 array-ref 0<>
	loops 1 array-ref 0<> || if
		loops 0 array-ref  snd chn undef undef add-mark drop
		loops 1 array-ref snd chn undef undef add-mark drop
		loops 2 array-ref 0<>
		loops 3 array-ref 0<> || if
			loops 2 array-ref  snd chn undef undef add-mark drop
			loops 3 array-ref snd chn undef undef add-mark drop
		then
	then
;

\ ;;; -------- mapping extensions
\ ;;; (map arbitrary single-channel function over various channel collections)

: do-all-chans <{ func :optional origin #f -- }>
	doc" Apply FUNC to all active channels, \
using ORIGIN as the edit history indication:\n\
lambda: <{ val -- val*2 }> val f2* ; \"double all samples\" do-all-chans."
	all-chans each { lst }
		lst 0 array-ref { snd }
		lst 1 array-ref { chn }
		func 0 #f snd chn #f origin map-channel drop
	end-each
;

: update-graphs ( -- )
	doc" Update (redraw) all graphs."
	all-chans each { lst }
		lst 0 array-ref  { snd }
		lst 1 array-ref { chn }
		snd chn update-time-graph drop
	end-each
;

: do-chans <{ func :optional origin #f -- }>
	doc" Apply FUNC to all sync'd channels \
using ORIGIN as the edit history indication."
	#f snd-snd sync { snc }
	snc 0> if
		all-chans each { lst }
			lst 0 array-ref  { snd }
			lst 1 array-ref { chn }
			snd sync snc = if
				func 0 #f snd chn #f origin map-channel drop
			then
		end-each
	else
		"sync not set" snd status-report drop
	then
;

: do-sound-chans <{ func :optional origin #f -- }>
	doc" Apply FUNC to all selected channels \
using ORIGIN as the edit history indication."
	selected-sound { snd }
	snd sound? if
		snd channels 0 ?do
			func 0 #f snd i ( chn ) #f origin map-channel drop
		loop
	else
		"no selected sound" snd status-report drop
	then
;

hide
: everys-cb { func -- prc; y self -- f }
	1 proc-create ( prc )
	func ,
  does> { y self -- f }
	self @ ( func ) #( y ) run-proc not
;
set-current

: every-sample? ( func -- f )
	doc" Return #t if FUNC is not #f for all samples in the \
current channel, otherwise it moves the cursor to the first offending sample."
	{ func }
	func everys-cb 0 #f #f #f #f scan-channel { baddy }
	baddy if
		baddy 1 array-ref #f #f #f set-cursor drop
	then
	baddy not
;
previous

hide
: sorts-cb { bins -- prc; y self -- f }
	1 proc-create ( prc )
	bins ,
  does> { y self -- f }
	self @ { bins }
	y fabs  bins length f* fround->s { bin }
	bins bin 1 object-set+!
	#f
;
set-current

: sort-samples ( nbins -- ary )
	doc" Provide histogram in BINS bins."
	{ nbins }
	nbins :initial-element 0 make-array { bins }
	bins sorts-cb 0 #f #f #f #f scan-channel drop
	bins
;
previous

\ ;;; -------- mix mono sound into stereo sound panning according to env

hide
: places1-cb { rd pos -- prc; y self -- val }
	1 proc-create ( prc )
	rd , pos ,
  does> { y self -- val }
	self @ { rd }
	self cell+ @ { pos }
	rd read-sample pos f* y f+
;

: places0-cb { rd pos -- prc; y self -- val }
	1 proc-create ( prc )
	rd , pos ,
  does> { y self -- val }
	self @ { rd }
	self cell+ @ { pos }
	rd read-sample 1.0 pos f- f* y f+
;

: places3-cb { rd en -- prc; y self -- val }
	1 proc-create ( prc )
	rd , en ,
  does> { y self -- val }
	self @ { rd }
	self cell+ @ { en }
	rd read-sample en env f* y f+
;

: places2-cb { rd en -- prc; y self -- val }
	1 proc-create ( prc )
	rd , en ,
  does> { y self -- val }
	self @ { rd }
	self cell+ @ { en }
	rd read-sample 1.0 en env f- f* y f+
;
set-current

: place-sound ( mono stereo pan -- res )
	doc" Mix a mono sound into a stereo sound, \
splitting it into two copies whose amplitudes depend on the envelope PAN-ENV.  \
If PAN-ENV is a number, the sound is split such that 0 is all in channel 0 \
and 90 is all in channel 1."
	{ mono stereo pan }
	mono #f #f #f framples { len }
	pan number? if
		pan 90.0 f/ { pos }
		0 mono #f 1 #f make-sampler { reader0 }
		0 mono #f 1 #f make-sampler { reader1 }
		reader1 pos places1-cb 0 len stereo 1 #f #f map-channel drop
		reader0 pos places0-cb 0 len stereo 0 #f #f map-channel drop
	else
		:envelope pan :length len make-env { e0 }
		:envelope pan :length len make-env { e1 }
		0 mono #f 1 #f make-sampler { reader0 }
		0 mono #f 1 #f make-sampler { reader1 }
		reader1 e1 places3-cb 0 len stereo 1 #f #f map-channel drop
		reader0 e0 places2-cb 0 len stereo 0 #f #f map-channel drop
	then
;
previous

\ ;;; -------- FFT-based editing

: fft-edit <{ bottom top :optional snd #f chn #f -- vct }>
	doc" Fft an entire sound, remove all energy \
below BOTTOM and all above TOP, then inverse fft."
	snd srate { sr }
	snd chn #f framples { len }
	2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
	0 fsize snd chn #f channel->vct { rdata }
	fsize 0.0 make-vct { idata }
	bottom sr fsize f/ f/ fround->s { lo }
	top    sr fsize f/ f/ fround->s { hi }
	rdata idata 1 fft drop
	lo 0> if
		rdata 0 0.0 vct-set! drop
		idata 0 0.0 vct-set! drop
		fsize 1- { jj }
		lo 1 ?do
			rdata i  0.0 vct-set! drop
			rdata jj 0.0 vct-set! drop
			idata i  0.0 vct-set! drop
			idata jj 0.0 vct-set! drop
			jj 1- to jj
		loop
	then
	hi fsize 2/ < if
		fsize hi - { jj }
		fsize 2/ hi ?do
			rdata i  0.0 vct-set! drop
			rdata jj 0.0 vct-set! drop
			idata i  0.0 vct-set! drop
			idata jj 0.0 vct-set! drop
			jj 1- to jj
		loop
	then
	rdata idata -1 fft drop
	"%s %s %s" #( bottom top get-func-name ) string-format { origin }
	rdata fsize 1/f vct-scale! 0 len 1- snd chn #f origin vct->channel
;

: fft-squelch <{ squelch :optional snd #f chn #f -- scl }>
	doc" Fft an entire sound, set all bins \
to 0.0 whose energy is below squelch, then inverse fft."
	snd chn #f framples { len }
	2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
	0 fsize snd chn #f channel->vct { rdata }
	fsize 0.0 make-vct { idata }
	fsize 2/ { fsize2 }
	rdata idata 1 fft drop
	rdata vct-copy { vr }
	idata vct-copy { vi }
	vr vi rectangular->polar drop
	vr vct-peak { scaler }
	squelch scaler f* { scl-squelch }
	rdata 0 vct-ref dup f*
	    idata 0 vct-ref dup f* f+ fsqrt scl-squelch f< if
		rdata 0 0.0 vct-set! drop
		idata 0 0.0 vct-set! drop
	then
	fsize 1- { jj }
	fsize 1 ?do
		rdata i vct-ref dup f*
		    idata i vct-ref dup f* f+ fsqrt scl-squelch f< if
			rdata i  0.0 vct-set! drop
			rdata jj 0.0 vct-set! drop
			idata i  0.0 vct-set! drop
			idata jj 0.0 vct-set! drop
		then
		jj 1- to jj
	loop
	rdata idata -1 fft drop
	"%s %s" #( squelch get-func-name ) string-format { origin }
	rdata fsize 1/f vct-scale! 0 len 1- snd chn #f origin vct->channel drop
	scaler
;

: fft-cancel <{ lo-freq hi-freq :optional snd #f chn #f -- vct }>
	doc" Fft an entire sound, set the bin(s) \
representing LO-FREQ to HI-FREQ to 0, then inverse fft."
	snd srate { sr }
	snd chn #f framples { len }
	2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
	0 fsize snd chn #f channel->vct { rdata }
	fsize 0.0 make-vct { idata }
	rdata idata 1 fft drop
	sr fsize f/ { hz-bin }
	lo-freq hz-bin f/ fround->s { lo-bin }
	hi-freq hz-bin f/ fround->s { hi-bin }
	fsize lo-bin - { jj }
	hi-bin 1+ lo-bin ?do
		rdata i  0.0 vct-set! drop
		rdata jj 0.0 vct-set! drop
		idata i  0.0 vct-set! drop
		idata jj 0.0 vct-set! drop
		jj 1- to jj
	loop
	rdata idata -1 fft drop
	"%s %s %s" #( lo-freq hi-freq get-func-name ) string-format { origin }
	rdata fsize 1/f vct-scale! 0 len 1- snd chn #f origin vct->channel
;

: make-ramp <{ :optional size 128 -- gen }>
	doc" Return ramp generator."
	vct( 0.0 size )
;

: ramp <{ gen up -- val }>
	doc" Is a kind of CLM generator that produces a ramp of a \
given length, then sticks at 0.0 or 1.0 until the UP argument changes."
	gen 0 vct-ref { ctr }
	gen 1 vct-ref { size }
	ctr size f/ { val }
	gen 0  ctr up if
		1
	else
		-1
	then f+ 0 fmax size fmin  vct-set! drop
	val
;

hide
: squelch-vowels-cb { snd chn -- prc; y self -- val }
	32 { fft-size }
	0 snd chn 1 #f make-sampler { read-ahead }
	1 proc-create { prc }
	fft-size 0.0 make-vct map!
		read-ahead #() apply
	end-map ( rl ) ,
	fft-size 0.0 make-vct ( im ) ,
	256 make-ramp ( ramper ) ,
	snd chn #f maxamp fft-size f2/ f/ ( peak ) ,
	read-ahead ,
	#f ( in-vowel ) ,
	prc
  does> { y self -- val }
	self           @ { rl }
	self   cell+   @ { im }
	self 2 cells + @ { ramper }
	self 3 cells + @ { peak }
	self 4 cells + @ { read-ahead }
	self 5 cells + @ { in-vowel }
	rl read-ahead #() apply cycle-set!
	rl cycle-start@ 0= if
		rl im 1 fft ( rl ) dup vct-multiply! drop
		im im vct-multiply! drop
		rl im 0 vct-add! drop
		rl 0 vct-ref
		    rl 1 vct-ref f+
		    rl 2 vct-ref f+
		    rl 3 vct-ref f+ peak f> to in-vowel
		in-vowel self 5 cells + ! ( in-vowel )
		im 0.0 vct-fill! drop
	then
	1.0  ramper in-vowel ramp  f- ( rval ) y f*
;
set-current

: squelch-vowels <{ :optional snd #f chn #f -- val }>
	doc" Suppress portions of a sound that look like steady-state."
	snd chn squelch-vowels-cb 0 #f snd chn #f get-func-name map-channel
;
previous

: fft-env-data <{ fft-env :optional snd #f chn #f -- vct }>
	doc" Apply FFT-ENV as spectral env to current sound, \
returning vct of new data."
	snd chn #f framples { len }
	2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
	0 fsize snd chn #f channel->vct { rdata }
	fsize 0.0 make-vct { idata }
	fsize 2/ { fsize2 }
	:envelope fft-env :length fsize2 make-env { e }
	rdata idata 1 fft drop
	e env { val }
	rdata 0 val object-set*!
	idata 0 val object-set*!
	fsize 1- { jj }
	fsize2 1 ?do
		e env to val
		rdata i  val object-set*!
		rdata jj val object-set*!
		idata i  val object-set*!
		idata jj val object-set*!
		jj 1- to jj
	loop
	rdata idata -1 fft ( rdata ) fsize 1/f vct-scale!
;

: fft-env-edit <{ fft-env :optional snd #f chn #f -- vct }>
	doc" Edit (filter) current chan using FFT-ENV."
	"%s %s" #( fft-env get-func-name ) string-format { origin }
	fft-env snd chn fft-env-data 0 snd chn #f framples 1-
	    snd chn #f origin vct->channel
;

: fft-env-interp <{ env1 env2 interp :optional snd #f chn #f -- vct }>
	doc" Interpolate between two fft-filtered \
versions (ENV1 and ENV2 are the spectral envelopes) \
following interp (an env between 0 and 1)."
	env1 snd chn fft-env-data { data1 }
	env2 snd chn fft-env-data { data2 }
	snd chn #f framples { len }
	:envelope interp :length len make-env { e }
	"%s %s %s %s" #( env1 env2 interp get-func-name )
	    string-format { origin }
	len 0.0 make-vct map!
		e env { pan }
		1.0 pan f- data1 i vct-ref f* data2 i vct-ref pan f* f+
	end-map ( new-data ) 0 len 1- snd chn #f origin vct->channel
;

: filter-fft <{ flt :optional normalize #t snd #f chn #f -- val }>
	doc" Get the spectrum of all the data in the given channel, \
apply the function FLT to it, then inverse fft.  \
FLT should take one argument, the current spectrum value.\n\
lambda: <{ y -- val }> y 0.01 f< if 0.0 else y then ; filter-fft\n\
is like fft-squelch."
	snd chn #f framples { len }
	snd chn #f maxamp { mx }
	2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
	fsize 2/ { fsize2 }
	0 fsize snd chn #f channel->vct { rdata }
	fsize 0.0 make-vct { idata }
	rdata rectangular-window fsize #t 1.0
	    #f ( in-place == #f ) normalize snd-spectrum { spect }
	rdata idata 1 fft drop
	flt #( spect 0 vct-ref ) run-proc drop
	fsize 1- { jj }
	fsize2 1 ?do
		spect i vct-ref { orig }
		flt #( orig ) run-proc { cur }
		orig fabs 0.000001 f> if
			cur orig f/ { scl }
			rdata i  scl object-set*!
			idata i  scl object-set*!
			rdata jj scl object-set*!
			idata jj scl object-set*!
		else
			cur fabs 0.000001 f> if
				cur 2.0 fsqrt f/ { scl }
				rdata i  scl vct-set! drop
				idata i  scl vct-set! drop
				rdata jj scl vct-set! drop
				idata jj scl fnegate vct-set! drop
			then
		then
		jj 1- to jj
	loop
	rdata idata -1 fft drop
	"<'> %s %s" #( flt get-func-name ) string-format { origin }
	mx f0<> if
		rdata vct-peak { pk }
		rdata mx pk f/ vct-scale!
	else
		rdata
	then 0 len 1- snd chn #f origin vct->channel
;
\ 0.5  0.5  make-one-zero filter-fft
\ 0.05 0.05 make-one-pole filter-fft
\ lambda: <{ y -- val }> y 0.1 f< if 0.0 else y then ; filter-fft
\ 0 0 0 1 0 make-sampler filter-fft
\ <'> contrast-enhancement filter-fft
\ lambda: <{ y -- val }> y y f* y f* ; filter-fft

: fft-smoother <{ cutoff start samps :optional snd #f chn #f -- val }>
	doc" Use fft-filtering to smooth a section:\n\
#f #f #f cursor value curs
0.1 curs 400 #f #f fft-smoother  curs 400 #f #f #f undef vct->channel."
	2.0  samps 1+ flog 2.0 flog f/ fceil  f** fround->s { fftpts }
	start fftpts snd chn #f channel->vct { rl }
	fftpts 0.0 make-vct { im }
	fftpts cutoff f* fround->s { top }
	rl 0 vct-ref { old0 }
	rl samps 1- vct-ref { old1 }
	rl vct-peak { oldmax }
	rl im 1 fft drop
	fftpts top ?do
		rl i 0.0 vct-set! drop
		im i 0.0 vct-set! drop
	loop
	rl im -1 fft ( rl ) fftpts 1/f vct-scale! drop
	rl vct-peak { newmax }
	newmax f0<> if
		oldmax newmax f/ 1.5 f> if
			rl oldmax newmax f/ vct-scale! drop
		then
		rl 0 vct-ref { new0 }
		rl samps 1- vct-ref { new1 }
		old0 new0 f- { offset0 }
		old1 new1 f- { offset1 }
		offset1 offset0 f= if
			0.0
		else
			offset1 offset0 f- samps f/
		then { incr }
		offset0 { trend }
		samps 0 ?do
			rl i trend object-set+!
			trend incr f+ to trend
		loop
	then
	rl
;

\ ;;; -------- comb-filter

: comb-filter ( scaler size -- prc; x self -- val )
	doc" Return comb-filter ready for map-channel etc: \
0.8 32 comb-filter map-channel.  \
If you're in a hurry use: 0.8 32 make-comb clm-channel instead."
	{ scaler size }
	scaler size make-comb { gen }
	1 proc-create ( prc )
	gen ,
  does> { x self -- val }
	self @ ( cmb ) x 0.0 comb
;

\ ;;; by using filters at harmonically related sizes, we can get chords:

: comb-chord ( scaler size amp -- prc; x self -- val )
	doc" Return set of harmonically-related comb \
filters: 0.95 100 0.3 comb-chord map-channel."
	{ scaler size amp }
	scaler size make-comb { c1 }
	scaler size 0.75 f* f>s make-comb { c2 }
	scaler size 1.2  f* f>s make-comb { c3 }
	1 proc-create ( prc )
	amp , c1 , c2 , c3 ,
  does> ( x self -- val )
	{ x self }
	self @ { amp }
	self 1 cells + @ { c1 }
	self 2 cells + @ { c2 }
	self 3 cells + @ { c3 }
	c1 x 0.0 comb c2 x 0.0 comb c3 x 0.0 comb f+ f+ amp f*
;

\ ;;; or change the comb length via an envelope:

: zcomb ( scaler size pm -- prc; x self -- val )
	doc" Return comb filter whose length varies according to an envelope:\n\
0.8 32 #( 0 0 1 10 ) zcomb map-channel."
	{ scaler size pm }
	:size size
	    :max-size pm 0.0 max-envelope 1.0 f+ size f+ fround->s
	    make-comb { gen }
	:envelope pm :length #f #f #f framples make-env { penv }
	1 proc-create ( prc )
	gen , penv ,
  does> { x self -- val }
	self @ ( gen ) x self cell+ @ ( penv ) env comb
;

: notch-filter ( scaler size -- prc; x self -- val )
	doc" Return notch-filter: 0.8 32 notch-filter map-channel."
	make-notch { gen }
	1 proc-create ( prc )
	gen ,
  does> { x self -- val }
	self @ ( gen ) x 0.0 notch
;

: formant-filter ( radius frequency -- prc; x self -- val )
	doc" Return formant generator: 2400 0.99 formant-filter map-channel.  \
Faster is: 2400 0.99 make-formant filter-sound."
	make-formant { gen }
	1 proc-create ( prc )
	gen ,
  does> { x self -- val }
	self @ ( gen ) x formant
;

: formants ( r1 f1 r2 f2 r3 f3 -- prc; x self -- val )
	doc" Return 3 formant filters in \
parallel: 0.99 900 0.98 1800 0.99 2700 formants map-channel."
	{ r1 f1 r2 f2 r3 f3 }
	f1 r1 make-formant { fr1 }
	f2 r2 make-formant { fr2 }
	f3 r3 make-formant { fr3 }
	1 proc-create ( prc )
	fr1 , fr2 , fr3 ,
  does> { x self -- val }
	self           @ ( fr1 ) x formant
	self 1 cells + @ ( fr2 ) x formant f+
	self 2 cells + @ ( fr3 ) x formant f+
;

: moving-formant ( radius move -- prc; x self -- val )
	doc" Return time-varying (in frequency) formant filter:\n\
0.99 #( 0 1200 1 2400 ) moving-formant map-channel."
	{ radius move }
	move 1 array-ref radius make-formant { frm }
	:envelope move :length #f #f #f framples make-env { menv }
	1 proc-create ( prc )
	frm , menv ,
  does> { x self -- val }
	self @ ( frm ) x formant ( val )
	self @ ( frm ) self cell+ @ ( menv ) env set-mus-frequency drop
	( val )
;

: osc-formants ( radius bases amounts freqs -- prc; x self -- val )
	doc" Return time-varying (in frequency) formant filter:\n\
0.99 #( 0 1200 1 2400 ) moving-formant map-channel."
	{ radius bases amounts freqs }
	bases vct-length { len }
	len make-array map!
		bases i vct-ref radius make-formant
	end-map { frms }
	len make-array map!
		freqs i vct-ref make-oscil
	end-map { oscs }
	1 proc-create ( prc )
	frms , amounts , oscs , bases ,
  does> { x self -- val }
	self           @ { frms }
	self 1 cells + @ { amounts }
	self 2 cells + @ { oscs }
	self 3 cells + @ { bases }
	0.0 ( val )
	frms each { frm }
		frm x formant f+ ( val += ... )
		frm bases i vct-ref
		    amounts i vct-ref
		    oscs i array-ref 0.0 0.0 oscil f* f+ set-mus-frequency drop
	end-each
	( val )
;

\ ;;; -------- echo

: echo ( scaler secs -- prc; y self -- val )
	doc" Return echo maker: 0.5 0.5 echo 0 44100 map-channel."
	{ scaler secs }
	secs #f srate f* fround->s make-delay { del }
	1 proc-create ( prc )
	del , scaler ,
  does> { y self -- val }
	self @ { del }
	self cell+ @ { scaler }
	del  del 0.0 tap y f+ scaler f*  0.0  delay y f+
;

: zecho ( scaler secs freq amp -- prc; y self -- val )
	doc" Return modulated echo maker: \
0.5 0.75 6 10.0 zecho 0 65000 map-channel."
	{ scaler secs freq amp }
	freq make-oscil { os }
	secs #f srate f* fround->s { len }
	:size len :max-size len amp f+ fround->s 1+ make-delay { del }
	1 proc-create ( prc )
	del , scaler , os , amp ,
  does> { y self -- val }
	self @ { del }
	self cell+ @ { scaler }
	self 2 cells + @ { os }
	self 3 cells + @ { amp }
	del  del 0.0 tap y f+ scaler f*  os 0.0 0.0 oscil amp f*  delay y f+
;

: flecho ( scaler secs -- prc; y self -- val )
	doc" Return low-pass filtered echo maker: \
0.5 0.9 flecho 0 75000 map-channel."
	{ scaler secs }
	:order 4 :xcoeffs vct( 0.125 0.25 0.25 0.125 ) make-fir-filter { flt }
	secs #f srate f* fround->s make-delay { del }
	1 proc-create ( prc )
	del , scaler , flt ,
  does> { y self -- val }
	self @ { del }
	self cell+ @ { scaler }
	self 2 cells + @ { flt }
	del  flt  del 0.0 tap y f+ scaler f*  fir-filter  0.0  delay y f+
;

\ ;;; -------- ring-mod and am
\ ;;;
\ ;;; CLM instrument is ring-modulate.ins

: ring-mod ( freq gliss-env -- prc; y self -- val )
	doc" Return time-varying ring-modulation filter:\n\
10 #( 0 0 1 100 hz->radians ) ring-mod map-channel."
	{ freq gliss-env }
	:frequency freq make-oscil { os }
	:envelope gliss-env :length #f #f #f framples make-env { genv }
	1 proc-create ( prc )
	os , genv ,
  does> { y self -- val }
	self @ { os }
	self cell+ @ { genv }
	os  genv env 0.0  oscil y f*
;

: am ( freq -- prc; y self -- val )
	doc" Return amplitude-modulator: 440 am map-channel."
	make-oscil { os }
	1 proc-create ( prc )
	os ,
  does> { y self -- val }
	1.0  y  self @ ( os ) 0.0 0.0 oscil  amplitude-modulate
;

\ ;;; this taken from sox (vibro.c)
: vibro ( speed depth -- prc; y self -- val )
	{ speed depth }
	speed make-oscil { sine }
	depth f2/ { scl }
	1.0 scl f- { offset }
	1 proc-create ( prc )
	sine , scl , offset ,
  does> { y self -- val }
	self @ { sine }
	self cell+ @ { scl }
	self 2 cells + @ { offset }
	sine 0.0 0.0 oscil scl f* offset f+ y f*
;

\ ;;; -------- hello-dentist
\ ;;;
\ ;;; CLM instrument version is in sndclm.html

hide
: hd-input-cb { in-data -- prc; dir self -- val }
	1 proc-create ( prc )
	in-data  , 0 ( idx ) ,
  does> { dir self -- val }
	self       @ { in-data }
	self cell+ @ { idx }
	in-data idx object-range? if
		in-data idx vct-ref
	else
		0.0
	then ( val )
	idx dir + self cell+ ! ( idx )
	( val )
;
set-current

: hello-dentist <{ frq amp :optional snd #f chn #f -- vct }>
	doc" Vary the sampling rate randomly, making a voice sound quavery:\n\
40.0 0.1 #f #f hello-dentist drop."
	:frequency frq :amplitude amp make-rand-interp { rn }
	snd chn #f framples { len }
	0 len snd chn #f channel->vct { in-data }
	:srate 1.0 :input in-data hd-input-cb make-src { rd }
	"%s %s %s" #( frq amp get-func-name ) string-format { origin }
	amp f2* 1.0 f+ len f* fround->s 0.0 make-vct map!
		rd  rn 0.0 rand-interp  #f src
	end-map ( out-data ) 0 len snd chn #f origin vct->channel
;
previous

\ ;;; a very similar function uses oscil instead of rand-interp, giving
\ ;;; various "Forbidden Planet" sound effects:

hide
: fp-input-cb { sf -- prc; dir self -- val }
	1 proc-create ( prc )
	sf ,
  does> { dir self -- val }
	self @ ( sf ) dir 0> if
		next-sample
	else
		previous-sample
	then
;
set-current

: fp <{ sr osamp osfrq :optional snd #f chn #f -- vct }>
	doc" Vary the sampling rate via an oscil: 1.0 0.3 20 #f #f fp."
	osfrq make-oscil { os }
	0 snd chn 1 #f make-sampler { sf }
	:srate sr :input sf fp-input-cb make-src { s }
	snd chn #f framples { len }
	"%s %s %s %s" #( sr osamp osfrq get-func-name ) string-format { origin }
	len 0.0 make-vct map!
		s os 0.0 0.0 oscil osamp f* #f src
	end-map ( out-data ) 0 len snd chn #f origin vct->channel ( vct )
	sf free-sampler drop
;
previous

vct( -1.000 -0.960 -0.900 -0.820
     -0.720 -0.600 -0.450 -0.250 
      0.000  0.250  0.450  0.600
      0.720  0.820  0.900  0.960 1.000 ) constant compand-table

: compand ( -- prc; y self -- val )
	doc" Return compander: compand map-channel."
	1 proc-create ( prc )
  does> { y self -- val }
	compand-table  y 8.0 f* 8.0 f+ compand-table vct-length  array-interp
;

\ ;;; -------- shift pitch keeping duration constant
\ ;;;
\ ;;; both src and granulate take a function argument to get input
\ ;;;   whenever it is needed.
\ ;;;   In this case, src calls granulate which reads the currently
\ ;;;   selected file.  CLM version is in expsrc.ins

hide
: expgr-cb { v snd chn -- prc; dir self -- val }
	1 proc-create ( prc )
	v , snd , chn , 0 ,
  does> { dir self -- val }
	self @ { v }
	v cycle-ref ( val )
	v cycle-start@ 0= if
		self cell+ @ { snd }
		self 2 cells + @ { chn }
		self 3 cells + @ { vbeg }
		vbeg v vct-length + dup self 3 cells + ! ( vbeg += v-len )
		vbeg v vct-length snd chn #f channel->vct drop
	then
	( val )
;

: expsr-cb { gr -- prc; dir self -- val }
	1 proc-create ( prc )
	gr ,
  does> { dir self -- val }
	self @ ( gr ) #f #f granulate
;
set-current

: expsrc <{ rate :optional snd #f chn #f -- val }>
	doc" Use sampling-rate conversion and granular synthesis to \
produce a sound at a new pitch but at the original tempo.  \
It returns a function for map-channel."
	0 1024 snd chn #f channel->vct { v }
	:input v snd chn expgr-cb :expansion rate make-granulate { gr }
	:input gr expsr-cb :srate rate make-src { sr }
	1 proc-create ( prc )
	sr ,
  does> { y self -- val }
	self @ ( sr ) 0.0 #f src
;
previous

\ ;;; the next (expsnd) changes the tempo according to an envelope; the new
\ ;;; duration will depend on the expansion envelope -- we integrate it to get
\ ;;; the overall expansion, then use that to decide the new length.

hide
: es-input-cb { sf -- prc; dir self -- val }
	1 proc-create ( prc )
	sf ,
  does> { dir self -- val }
	self @ ( sf ) next-sample
;
set-current

: expsnd <{ gr-env :optional snd #f chn #f -- vct }>
	doc" Use the granulate generator to change tempo \
according to an envelope:\n\
#( 0 0.5 2 2.0 ) #f #f expsnd."
	snd chn #f framples { len }
	len snd srate f/ gr-env integrate-envelope f*
	    gr-env envelope-last-x f/ { dur }
	0 snd chn 1 #f make-sampler { sf }
	:expansion gr-env 1 array-ref
	    :jitter 0
	    :input sf es-input-cb make-granulate { gr }
	:envelope gr-env :duration dur make-env { ge }
	snd srate dur f* fround->s { sound-len }
	sound-len len max to len
	"%s %s" #( gr-env get-func-name ) string-format { origin }
	len 0.0 make-vct map!
		gr #f granulate ( val )
		gr ge env set-mus-increment drop
	end-map ( out-data ) 0 len snd chn #f origin vct->channel ( vct )
	sf free-sampler drop
;
previous

\ ;;; -------- cross-synthesis
\ ;;;
\ ;;; CLM version is in sndclm.html

: cross-synthesis ( cross-snd amp fftsize r -- prc; y self -- val )
	doc" Do cross-synthesis between CROSS-SND (a sound index) \
and the currently selected sound:\n\
1 0.5 128 6.0 cross-synthesis map-channel."
	{ cross-snd amp fftsize r }
	fftsize 2/ { freq-inc }
	fftsize 0.0 make-vct { fdr }
	fftsize 0.0 make-vct { fdi }
	freq-inc 0.0 make-vct { spectr }
	1.0 r fftsize f/ f- { radius }
	#f srate fftsize / { bin }
	freq-inc make-array map!
	  i bin * radius make-formant
	end-map spectr make-formant-bank { formants }
	1 proc-create ( prc )
	fdr , fdi , spectr , formants ,
	amp , freq-inc , cross-snd , fftsize , 0 ,
  does> { y self -- val }
	self @ { fdr }
	self cell+ @ { fdi }
	self 2 cells + @ { spectr }
	self 3 cells + @ { formants }
	self 4 cells + @ { amp }
	self 5 cells + @ { ctr }
	ctr formants length = if
		self 6 cells + @ { cross-snd }
		self 7 cells + @ { fftsize }
		self 8 cells + @ { inctr }
		inctr fftsize cross-snd 0 #f channel->vct dup self ! to fdr
		inctr fftsize 2/ + self 8 cells + ! ( inctr += freq-inc )
		fdr fdi #f 2 spectrum ( fdr )
		    spectr vct-subtract! ( fdr ) fftsize 2/ 1/f
		vct-scale! drop
		0 self 5 cells + ! ( ctr = 0 )
	then
	1 self 5 cells + +! ( ctr++ )
	spectr fdr 0 vct-add! drop
	formants y formant-bank amp f*
;

: voiced->unvoiced <{ amp fftsize r tempo :optional snd #f chn #f -- vct }>
	doc" Turn vocal sound into \
whispering: 1.0 256 2.0 2.0 #f #f voiced->unvoiced."
	fftsize 2/ { freq-inc }
	nil { fdr }
	fftsize 0.0 make-vct { fdi }
	freq-inc 0.0 make-vct { spectr }
	snd srate 3.0 f/ make-rand { noi }
	0 { inctr }
	1.0 r fftsize f/ f- { radius }
	snd srate fftsize / { bin }
	snd chn #f framples { len }
	len tempo f/ fround->s len max { out-len }
	freq-inc tempo f* fround->s { hop }
	0.0 0.0 { old-peak-amp new-peak-amp }
	"%s %s %s %s %s"
	    #( amp fftsize r tempo get-func-name ) string-format { origin }
	freq-inc make-array map!
		i bin * radius make-formant
	end-map ( formants ) spectr make-formant-bank { formants }
	freq-inc 1/f { 1/freq-inc }

 	out-len 0.0 make-vct map!
 		i freq-inc mod 0= if
 			inctr fftsize snd chn #f channel->vct to fdr
 			fdr vct-peak old-peak-amp fmax to old-peak-amp
 			fdr fdi #f 2 spectrum ( fdr )
 			    spectr vct-subtract! ( fdr )
 			    1/freq-inc vct-scale! drop
 			hop inctr + to inctr
 		then
 		spectr fdr 0 vct-add! drop
		formants noi 0.0 rand formant-bank ( outval )
 	end-map { out-data }
	out-data old-peak-amp out-data vct-peak f/ amp f* vct-scale! ( odata )
 	    0 out-len snd chn #f origin vct->channel ( odata )
;

: pulse-voice
    <{ co :optional freq 440.0 amp 1.0 fftsize 256 r 2.0 snd #f chn #f -- vct }>
	doc" Use sum-of-cosines to manipulate speech sounds."
	fftsize 2/ { freq-inc }
	fftsize 0.0 make-vct { fdr }
	fftsize 0.0 make-vct { fdi }
	freq-inc 0.0 make-vct { spectr }
	:cosines co :frequency freq make-sum-of-cosines { pulse }
	0 { inctr }
	1.0 r fftsize f/ f- { radius }
	snd srate fftsize / { bin }
	snd chn #f framples { len }
	0.0 0.0 { old-peak-amp new-peak-amp }
	"%s %s %s %s %s %s"
	    #( co freq amp fftsize r get-func-name ) string-format { origin }
	freq-inc make-array map!
		i bin * radius make-formant
	end-map spectr make-formant-bank { formants }
	len 0.0 make-vct map!
		i freq-inc mod 0= if
			inctr fftsize snd chn #f channel->vct to fdr
			fdr vct-peak old-peak-amp fmax to old-peak-amp
			fdr fdi #f 2 spectrum ( fdr )
			    spectr vct-subtract! ( fdr )
			    freq-inc 1/f vct-scale! drop
			freq-inc inctr + to inctr
		then
		spectr fdr 0 vct-add! drop
		formants pulse 0.0 sum-of-cosines formant-bank ( outval )
		    dup fabs new-peak-amp fmax to new-peak-amp
		( outval )
	end-map
	old-peak-amp new-peak-amp f/ amp f* vct-scale!
	    0 len snd chn #f origin vct->channel ( vct )
;
\   20.0 1.0 1024 0.01 pulse-voice
\  120.0 1.0 1024 0.2  pulse-voice
\  240.0 1.0 1024 0.1  pulse-voice
\  240.0 1.0 2048      pulse-voice
\ 1000.0 1.0  512      pulse-voice

'snd-nogui provided? [unless]
	\ ;;; -------- convolution example

	hide
	: cnv-cb { sf -- prc; dir self -- val }
		1 proc-create ( prc )
		sf ,
	  does> { dir self -- val }
		self @ ( sf ) next-sample
	;
	set-current

	: cnvtest ( snd0 snd1 amp -- mx )
		doc" Convolve SND0 and SND1, scaling by AMP, \
returns new max amp: 0 1 0.1 cnvtest."
		{ snd0 snd1 amp }
		snd0 #f #f framples { flt-len }
		snd1 #f #f framples flt-len + { total-len }
		0 snd1 0 1 #f make-sampler { sf }
		:input sf cnv-cb :filter 0 flt-len
		    snd0 #f #f channel->vct make-convolve { cnv }
		total-len 0.0 make-vct map!
			cnv #f convolve
		end-map amp vct-scale! ( out-data )
		0 total-len snd1 #f #f get-func-name
		    vct->channel vct-peak { max-samp }
		sf free-sampler drop
		max-samp 1.0 f> if
			#( max-samp fnegate max-samp ) snd1 #f set-y-bounds drop
		then
		max-samp
	;
	previous
[then]

\ ;;; -------- swap selection chans

: swap-selection-channels ( -- )
	doc" Swap the currently selected data's channels."
	undef selection? unless
		'no-active-selection #( get-func-name ) fth-throw
	then
	selection-chans 2 = if
		selection-position { beg }
		selection-framples { len }
		#f { snd-chn0 }
		#f { snd-chn1 }
		all-chans each { lst }
			lst car lst cadr selection-member? if
				snd-chn0 false? if
					lst to snd-chn0
				else
					snd-chn1 false? if
						lst to snd-chn1
						leave
					then
				then
			then
		end-each
		snd-chn1 if
			snd-chn0 car snd-chn0 cadr snd-chn1 car snd-chn1 cadr
			    beg len #f #f swap-channels drop
		else
			'wrong-number-of-channels
			    #( "%s: needs two channels to swap"
			       get-func-name ) fth-throw
		then
	else
		'wrong-number-of-channels
		    #( "%s: needs a stereo selection (not %s chans)"
		       get-func-name
		       selection-chans ) fth-throw
	then
;

\ ;;; -------- sound interp
\ ;;;
\ ;;; make-sound-interp sets up a sound reader that reads a channel at an
\ ;;;    arbitary location, interpolating between samples if necessary,
\ ;;;    the corresponding "generator" is sound-interp

: make-sound-interp <{ start :optional snd #f chn #f -- prc; loc self -- val }>
	doc" Return an interpolating reader for SND's channel CHN."
	0 #f snd chn #f channel->vct { data }
	1 proc-create ( prc )
	data , data vct-length ,
  does> { loc self -- val }
	self @ ( data ) loc self cell+ @ ( size ) array-interp
;

: sound-interp { func loc -- val }
	doc" Return sample at LOC (interpolated if necessary) \
from FUNC created by make-sound-interp."
	loc func execute
;

\ ;; env-sound-interp takes an envelope that goes between 0 and 1
\ ;;   (y-axis), and a time-scaler (1.0 = original length) and returns a
\ ;;   new version of the data in the specified channel that follows that
\ ;;   envelope (that is, when the envelope is 0 we get sample 0, when the
\ ;;   envelope is 1 we get the last sample, envelope = .5 we get the middle
\ ;;   sample of the sound and so on. (env-sound-interp #(0 0 1 1)) will
\ ;;   return a copy of the current sound; (env-sound-interp #(0 0 1 1 2 0)
\ ;;   2.0) will return a new sound with the sound copied first in normal
\ ;;   order, then reversed.  src-sound with an envelope could be used
\ ;;   for this effect, but it is much more direct to apply the envelope
\ ;;   to sound sample positions.

: env-sound-interp <{ en :optional time-scale 1.0 snd #f chn #f -- vct }>
	doc" Read SND's channel CHN according to EN and TIME-SCALE."
	snd chn #f framples { len }
	time-scale len f* fround->s { newlen }
	:envelope en :length newlen 1+ :scaler len make-env { read-env }
	0 #f snd chn #f channel->vct { data }
	newlen 0.0 make-vct map!
		data read-env env len array-interp
	end-map { new-snd }
	"%s %s %s" #( en time-scale get-func-name ) string-format { origin }
	0 newlen new-snd snd chn #t origin
	    0 current-edit-position #t set-samples ( vct )
;
\ #( 0 0 1 1 2 0 ) 2.0 env-sound-interp

: granulated-sound-interp
  <{ en :optional tscl 1.0 grlen 0.1 gren #f ohop 0.05 snd #f chn #f -- vct }>
	doc" Read the given channel following EN (as env-sound-interp), \
using grains to create the re-tempo'd read."
	gren empty? if
		#( 0 0 1 1 2 1 3 0 ) to gren
	then
	snd chn #f framples { len }
	tscl len f* fround->s { newlen }
	:envelope en :length newlen :scaler len make-env { read-env }
	snd srate { sr }
	grlen sr f* fround->s { grain-framples }
	ohop sr f* fround->s { hop-framples }
	grlen ohop f/ fceil->s { num-readers }
	0 0 { cur-readers next-reader }
	sr 0.005 f* { jitter }
	num-readers make-array { readers }
	num-readers make-array map!
		:envelope gren :length grain-framples make-env
	end-map { grain-envs }
	newlen 0.0 make-vct { new-snd }
	newlen 0 ?do
		read-env i set-mus-location drop
		read-env env { position-in-original }
		position-in-original jitter mus-random f+ fround->s 0 max { mx }
		mx snd chn 1 #f make-sampler { srd }
		readers srd cycle-set!
		grain-envs next-reader array-ref mus-reset drop
		readers cycle-start@ to next-reader
		cur-readers next-reader < if
			next-reader to cur-readers
		then
		cur-readers 0 ?do
			grain-envs i array-ref { e }
			readers i array-ref { rd }
			\ j is index from outer loop
			newlen hop-framples j + min j ?do
				new-snd i e env rd next-sample f* vct-set! drop
			loop
		loop
	hop-framples +loop
	"%s %s %s %s %s %s"
	    #( en tscl grlen gren ohop get-func-name ) string-format { origin }
	0 newlen new-snd snd chn #t origin
	    0 current-edit-position #t set-samples ( vct )
;

\ #( 0 0 1 .1 2 1 ) 1.0 0.2 #( 0 0 1 1 2 0 )      granulated-sound-interp
\ #( 0 0 1 1 ) 2.0                                granulated-sound-interp
\ #( 0 0 1 .1 2 1 ) 1.0 0.2 #( 0 0 1 1 2 0 ) 0.02 granulated-sound-interp

\ ;;; -------- filtered-env 

hide
: fe-cb { flt amp-env -- prc; y self -- val }
	1 proc-create ( prc )
	flt , amp-env ,
  does> { y self -- val }
	self @ { flt }
	self cell+ @ ( amp-env ) env { env-val }
	flt 0 env-val        set-mus-xcoeff drop
	flt 1 env-val 1.0 f- set-mus-xcoeff drop
	flt env-val y f* one-pole
;
set-current

: filtered-env <{ e :optional snd #f chn #f -- val }>
	doc" It's a time-varying one-pole filter: \
when env is at 1.0, no filtering, as env moves to 0.0, \
low-pass gets more intense; \
amplitude and low-pass amount move together."
	1.0 0.0 make-one-pole { flt }
	:envelope e :length snd chn #f framples make-env { amp-env }
	flt amp-env fe-cb  0 #f snd chn #f
	    "%s %s" #( e get-func-name ) string-format map-channel
;
previous

\ ;;; -------- C-x b support: hide all but one of the current sounds
\ ;;;  (more like Emacs)

hide
#f value xb-last-buffer
#f value xb-current-buffer
0  value xb-last-width
0  value xb-last-height
set-current

: open-current-buffer { width heigth -- }
	width  to xb-last-width
	heigth to xb-last-height
	xb-current-buffer 0 array-ref sound-widgets 0 array-ref { sound-pane }
	sound-pane if
		sound-pane show-widget drop
		sound-pane #( width heigth ) set-widget-size drop
		xb-current-buffer 0 array-ref select-sound drop
		xb-current-buffer 1 array-ref select-channel drop
	then
;

: close-all-buffers ( -- )
	sounds each ( s )
		sound-widgets 0 array-ref hide-widget drop
	end-each
;
previous

\ ;;; -------- remove-clicks 

: find-click ( loc -- pos )
	doc" Find the next click starting at LOC."
	{ loc }
	loc #f #f 1 #f make-sampler { rd }
	0.0 0.0 0.0 { samp0 samp1 samp2 }
	10 0.0 make-vct { samps }
	#f 					\ flag
	#f #f #f framples loc ?do
		samp1 to samp0
		samp2 to samp1
		rd next-sample to samp2
		samps samp0 cycle-set!
		samps vct-peak 0.1 fmax { local-max }
		samp0 samp1 f- fabs local-max f>
		samp1 samp2 f- fabs local-max f>     &&
		samp0 samp2 f- fabs local-max f2/ f< && if
			drop ( flag )
			i
			leave
		then
	loop
;

: remove-clicks ( -- )
	doc" Try to find and smooth-over clicks."
	-2 { click }
	begin
		click 2+ find-click to click
		click
	while
		click 2- 4 #f #f smooth-sound drop
	repeat
;

: search-for-click ( -- prc; val self -- pos )
	doc" Look for the next click (for use with C-s)."
	1 proc-create ( prc )
	10 0.0 make-vct , 0.0 , 0.0 , 0.0 ,
  does> { val self -- pos }
	self @ { samps }
	self cell+ @ { samp0 }
	self 2 cells + @ { samp1 }
	self 3 cells + @ { samp2 }
	samp1 to samp0
	samp2 to samp1
	val   to samp2
	samp0 self cell+ !
	samp1 self 2 cells + !
	samp2 self 3 cells + !
	samps samp0 cycle-set!
	samps vct-peak 0.1 fmax { local-max }
	samp0 samp1 f- fabs local-max f>=
	samp1 samp2 f- fabs local-max f>=     &&
	samp0 samp2 f- fabs local-max f2/ f<= && if
		-1
	else
		#f
	then
;

: zero+ ( -- prc; n self -- val )
	doc" Find the next positive-going zero crossing (if \
searching forward) (for use with C-s)."
	1 proc-create ( prc )
	0.0 ( lastn ) ,
  does> { n self -- val }
	self @ ( lastn ) f0< n f0>= && -1 && ( val )
	n self ! ( lastn = n )
	( val )
;

: next-peak ( -- prc; n self -- val )
	doc" Find the next max or min point in the \
time-domain waveform (for use with C-s)."
	1 proc-create ( prc )
	( last0 ) #f , ( last1 ) #f ,
  does> { n self -- val }
	self       @ { last0 }
	self cell+ @ { last1 }
	last0 number?
	last0 last1 f< last1 n f> &&
	last0 last1 f> last1 n f< && || &&
	-1 && ( val )
	last1 self !   ( last0 = last1 )
	n self cell+ ! ( last1 = n )
	( val )
;

: find-pitch ( pitch -- prc; y self -- val )
	doc" Find the point in the current sound \
where PITCH (in Hz) predominates:\n\
C-s 300 find-pitch\n\
In most cases, this will be slightly offset from the true \
beginning of the note."
	{ pitch }
	1 proc-create ( prc )
	#f #f transform-size 0.0 make-vct , pitch ,
  does> { y self -- val }
	self @ { data }
	self cell+ @ { pitch }
	data y cycle-set!
	data cycle-start@ 0= if
		data vct-peak 0.001 f> if
			data rectangular-window data length
			    #t 0.0 undef #t snd-spectrum { spectr }
			10.0 flog { log10 }
			0.0 0 { pk pkloc }
			data length 2/ 0 ?do
				spectr i vct-ref dup pk f> if
					( val ) to pk
					i to pkloc
				else
					( val ) drop
				then
			loop
			pkloc 0> if
				spectr pkloc 1- vct-ref { la }
				spectr pkloc    vct-ref { ca }
				spectr pkloc 1+ vct-ref { ra }
				la ca fmax ra fmax 0.001 f* { pk1 }
				la 0.0000001 fmax pk1 f/ flog log10 f/ { logla }
				ca 0.0000001 fmax pk1 f/ flog log10 f/ { logca }
				ra 0.0000001 fmax pk1 f/ flog log10 f/ { logra }
				logla logra f- f2/
				    logla logra f+  logca f2*  f- f/
			else
				0.0
			then pkloc f+ #f srate f* data length f/ { pit }
			pitch pit f- fabs #f srate data length f2* f/ f< if
				data length 2/ negate
			else
				#f
			then ( val )
		then
		data 0.0 vct-fill! drop
	else
		#f
	then
;

\ ;;; -------- file->vct and a sort of cue-list, I think

[ifundef] file->vct
	: file->vct ( file -- vct )
		doc" Return a vct with FILE's data."
		{ file }
		file find-file to file
		file unless
			'no-such-file #( "%s: %S" get-func-name file ) fth-throw
		then
		0 file undef 1 #f make-sampler { reader }
		file mus-sound-framples 0.0 make-vct map!
			reader next-sample
		end-map ( data )
		reader free-sampler drop
	;
[then]

hide
: an-cb ( notes snd chn -- prc; self -- #f )
	{ notes snd chn }
	0 proc-create ( prc )
	notes , snd , chn ,
  does> { self -- #f }
	self           @ { notes }
	self   cell+   @ { snd }
	self 2 cells + @ { chn }
	snd chn #f cursor { start }
	notes each { note }
		note 0 array-ref find-file { file }
		file unless
			'no-such-file #( "add-notes: %S" file ) fth-throw
		then
		note length 1 > if
			note 1 array-ref
		else
			0.0
		then { offset }
		note length 2 > if
			note 2 array-ref
		else
			1.0
		then { amp }
		snd srate offset f* fround->s start + { beg }
		amp 1.0 f<> if
			file file->vct amp vct-scale!
			    beg snd chn #f "add-notes" mix-vct
		else
			file beg 0 snd chn #f undef mix
		then drop
	end-each
	#f
;
set-current

: add-notes <{ notes :optional snd #f chn #f -- #f }>
	doc" Add (mix) NOTES which is a list of lists of the form:\n\
file :optional offset 0.0 amp 1.0\n\
starting at the cursor in the currently selected channel:\n\
#( #( \"oboe.snd\" ) #( \"pistol.snd\" 1.0 2.0 ) ) add-notes."
	notes snd chn an-cb
	    "%s %s" #( notes get-func-name ) string-format as-one-edit
;
previous

hide
: rpl-cb { reg -- prc; self -- val }
	0 proc-create ( prc )
	reg ,
  does> { self -- val }
	self @ ( reg ) play
;
set-current

: region-play-list ( data -- )
	doc" DATA is list of lists #( #( time reg ) ... ), TIME in secs, \
setting up a sort of play list:\n\
#( #( 0.0 0 ) #( 0.5 1 ) #( 1.0 2 ) #( 1.0 0 ) ) region-play-list."
	( data ) each { tone }
		tone 0 array-ref 1000.0 f* fround->s { time }
		tone 1 array-ref { region }
		region region? if
			time region rpl-cb in drop
		then
	end-each
;
previous

: region-play-sequence ( data -- )
	doc" DATA is list of region ids which will be played \
one after the other:\n\
#( 0 2 1 ) region-play-sequence."
	0.0 { time }
	( data ) map
		*key* { id }
		time { cur }
		id 0 region-framples id region-srate f/ time f+ to time
		#( cur id )
	end-map region-play-list
;

\ ;;; -------- replace-with-selection

: replace-with-selection ( -- )
	doc" Replace the samples from the cursor with the current selection."
	#f #f #f cursor { beg }
	#f #f selection-framples { len }
	beg #f #f insert-selection drop
	beg len + len #f #f #f delete-samples drop
;

\ ;;; -------- explode-sf2

: explode-sf2 ( -- )
	doc" Turn the currently selected soundfont file into \
a bunch of files of the form sample-name.aif."
	#f soundfont-info { lst }
	lst length 1- { last }
	lst each { vals }
		\ #( name start loop-start loop-end )
		vals 0 array-ref { name }
		vals 1 array-ref { start }
		i last < if
			lst i 1+ array-ref 1 array-ref
		else
			#f #f #f framples
		then { end }
		vals 2 array-ref start d- { loop-start }
		vals 3 array-ref start d- { loop-end }
		name ".aif" $+ { filename }
		undef selection? if
			#f #t #f set-selection-member? drop
		then
		#t #f #f set-selection-member? drop
		start #f #f set-selection-position drop
		end start d- #f #f set-selection-framples drop
		:file filename :header-type mus-aifc save-selection drop
		filename open-sound { temp }
		temp #( loop-start loop-end ) set-sound-loop-info drop
		temp close-sound drop
	end-each
;

\ ;;; -------- open-next-file-in-directory

hide
#f value nd-last-file-opened		\ string
#f value nd-current-directory		\ string
#f value nd-current-sorted-files	\ array

: gcf-sort-cb <{ a b -- n }>
	a b string< if
		-1
	else
		a b string> if
			1
		else
			0
		then
	then
;

: get-current-files { dir -- }
	dir to nd-current-directory
	dir sound-files-in-directory
	    <'> gcf-sort-cb sort to nd-current-sorted-files
;

: get-current-directory <{ filename -- filename }>
	filename to nd-last-file-opened
	filename mus-expand-filename file-dirname { new-path }
	nd-current-directory new-path string<> if
		new-path get-current-files
	then
	filename
;
set-current

: open-next-file-in-directory ( -- f )
	\ open-hook <'> get-current-directory hook-member? unless
	\     open-hook <'> get-current-directory add-hook!
	\ then
	nd-last-file-opened string? not
	sounds nil? not && if
		#f snd-snd file-name to nd-last-file-opened
	then
	nd-current-directory string? unless
		sounds nil? if
			file-pwd
		else
			nd-last-file-opened file-dirname
		then get-current-files
	then
	nd-current-sorted-files empty? if
		'no-such-file
		    #( "%s: %s" get-func-name nd-current-directory ) fth-throw
	else
		nd-current-sorted-files cycle-ref { next-file }
		next-file 0 find-sound if
			'file-already-open
			    #( "%s: %s" get-func-name next-file ) fth-throw
		else
			sounds nil? unless
				#f snd-snd close-sound drop
			then
			next-file find-file dup if
				open-sound
			then drop
		then
	then
	#t
;
previous

hide
: mouse-click-to-open-cb <{ snd chn button state x y axis -- f }>
	button 2 = if
		open-next-file-in-directory
	else
		#f
	then
;
set-current

: click-middle-button-to-open-next-file-in-directory ( -- )
	mouse-click-hook <'> mouse-click-to-open-cb add-hook!
	open-hook <'> get-current-directory add-hook!
;
previous

\ ;;; -------- chain-dsps

instrument: chain-dsps <{ start dur :optional dsps #() -- }>
	dsps map
		*key* array? if
			:envelope *key* :duration dur make-env
		else
			*key*
		then
	end-map { dsp-chain }
	start dur nil run-instrument
		0.0 { val }
		dsp-chain each { gen }
			gen env? if
				gen env val f*
			else
				gen readin? if
					gen readin val f+
				else
					gen mus-generator? if
						gen val 0.0 mus-apply
					else
						gen #( val ) run-proc
					then
				then
			then to val
		end-each
		val
	end-run
;instrument

hide
: cdsps-cb { os1 os2 -- prc; val self -- r }
	1 proc-create ( prc )
	os1 , os2 ,
  does> { val self -- r }
	self       @ ( osc1 ) val     0.0 oscil
	self cell+ @ ( osc2 ) val f2* 0.0 oscil f+
;
set-current

0 [if]
	lambda: ( -- )
		440.0 make-oscil { os1 }
		0 1.0  #( #( 0 0 1 1 2 0 ) os1 ) chain-dsps
		0.5 make-one-zero { oz }
		"oboe.snd" find-file make-readin { rd }
		0 1.0   #( #( 0 0 1 1 2 0 ) oz rd  ) chain-dsps
		220 make-oscil { osc1 }
		440 make-oscil { osc2 }
		osc1 osc2 cdsps-cb { cb }
		0 1.0 #( #( 0 0 1 1 2 0 ) cb ) chain-dsps
	; with-sound
[then]
previous

\ ;;; -------- re-order channels 

: scramble-channels ( new-order -- )
	\ ;; (scramble-channels 3 2 0 1) means chan 3 goes to 0, etc
	{ end-chans }
	end-chans length { len }
	len 1 > if
		end-chans map
			i
		end-map { cur-chans }
		end-chans each { end-chan }
			cur-chans i array-ref { cur-chan }
			end-chan cur-chan <> if
				#f cur-chans each { chn }
					chn end-chan = if
						drop ( #f )
						i
						leave
					then
				end-each { end-loc }
				#f end-loc #f i 0 len #f swap-channels drop
				cur-chans end-loc cur-chan array-set!
				cur-chans i end-chan array-set!
			then
		end-each
	then
;

hide
: sc-scan-cb { buffer silence in-silence edges samp -- prc; y self -- #f }
	1 proc-create ( prc )
	buffer , silence , in-silence , edges , samp ,
  does> { y self -- #f }
	self @ ( buffer ) y y f* moving-average { sum-of-squares }
	sum-of-squares self cell+ @ ( silence ) f< { now-silent }
	self 2 cells + @ ( in-silence ) now-silent equal? unless
		self 3 cells + @ ( edges )
		    self 4 cells + @ ( samp ) array-push drop
	then
	now-silent self 2 cells + ! ( in-silence = now-silent )
	1 self 4 cells + +! ( samp++ )
	#f
;

: sc-edit-cb { pieces -- prc; self -- val }
	0 proc-create ( prc )
	pieces , 0 ( start ) ,
  does> { self -- val }
	self @ { pieces }
	self cell+ @ { start }
	0.0 { scale-by }
	pieces length { len }
	len 0 ?do
		len random fround->s { this }
		pieces this array-ref { reg }
		pieces this #f array-set!
		reg unless
			len this 1+ ?do
				pieces i array-ref dup if
					to reg
					pieces i #f array-set!
					leave
				then
			loop
			reg unless
				0 this 1- ?do
					pieces i array-ref dup if
						to reg
						pieces i #f array-set!
						leave
					then
				-1 +loop
			then
		then
		reg start #f #f 0 mix-region drop
		reg 0 region-framples start + to start
		reg forget-region drop
	loop
	pieces
;
set-current

: scramble-channel ( silence -- )
	\ ;; (scramble-channel .01)
	{ silence }
	128 make-moving-average { buffer }
	silence 128 f/ to silence
	#() { edges }
	0 { samp }
	#t { in-silence }
	max-regions { old-max }
	with-mix-tags { old-tags }
	1024 set-max-regions drop
	#f set-with-mix-tags drop
	buffer silence in-silence edges samp sc-scan-cb
	    0 #f #f #f #f scan-channel drop
	edges #f #f #f framples array-push drop
	0 0 { start end }
	edges map
		start *key* #f #f make-region
		*key* to start
	end-map ( pieces ) sc-edit-cb get-func-name as-one-edit drop
	old-max set-max-regions drop
	old-tags set-with-mix-tags drop
;
previous

\ ;; -------- reorder blocks within channel

hide
: rbb-cb { rd beg ctr actual-block-len len snd chn -- prc; y self -- val }
	1 proc-create ( prc )
	rd , beg , ctr , actual-block-len , len , snd , chn ,
  does> { y self -- val }
	self @ { rd }
	self cell+ @ { beg }
	self 2 cells + @ { ctr }
	self 3 cells + @ { actual-block-len }
	self 4 cells + @ { len }
	self 5 cells + @ { snd }
	self 6 cells + @ { chn }
	rd read-sample { val }
	beg 10 < if
		val beg f* 0.1 f* to val
	else
		beg actual-block-len 10 - > if
			val actual-block-len beg - f* 0.1 f* to val
		then
	then
	beg 1+ to beg
	beg actual-block-len = if
		1 self 2 cells + +! ( ctr++ )
		0 self 1 cells + !  ( beg = 0 )
		len self 2 cells + @ ( ctr ) actual-block-len * - 0 max
		    snd chn 1 #f make-sampler self !
	then
	val
;
set-current

: reverse-by-blocks <{ block-len :optional snd #f chn #f -- val }>
	doc" Divide sound into block-len blocks, \
recombine blocks in reverse order."
	snd chn #f framples { len }
	len snd srate block-len f* f/ fround->s { num-blocks }
	num-blocks 1 > if
		len num-blocks f/ fceil f>s { actual-block-len }
		len actual-block-len - snd chn 1 #f make-sampler { rd }
		0 { beg }
		1 { ctr }
		"%s %s" #( block-len get-func-name ) string-format { origin }
		rd beg ctr actual-block-len len snd chn rbb-cb
		    0 #f snd chn #f origin map-channel
	else
		#f
	then
;
previous

hide
: rwb-cb { len actual-block-len no-clicks-env snd chn -- prc; self -- val }
	0 proc-create ( prc )
	len , actual-block-len , no-clicks-env , snd , chn ,
  does> { self -- val }
	self           @ { len }
	self   cell+   @ { actual-block-len }
	self 2 cells + @ { no-clicks-env }
	self 3 cells + @ { snd }
	self 4 cells + @ { chn }
	len 0 ?do
		i actual-block-len snd chn #f reverse-channel drop
		no-clicks-env i actual-block-len snd chn #f env-channel drop
	actual-block-len +loop
	#t
;
set-current

: reverse-within-blocks <{ block-len :optional snd #f chn #f -- val }>
	doc" Divide sound into blocks, recombine in order, \
but each block internally reversed."
	snd chn #f framples { len }
	len snd srate block-len f* f/ fround->s { num-blocks }
	num-blocks 1 > if
		len num-blocks f/ fceil f>s { actual-block-len }
		#( 0.0 0.0  0.01 1.0  0.99 1.0  1.0 0.0 ) { no-clicks-env }
		"%s %s" #( block-len get-func-name ) string-format { origin }
		len actual-block-len no-clicks-env snd chn rwb-cb
		    origin as-one-edit
	else
		0 #f snd chn #f reverse-channel
	then
;
previous

\ ;;; -------- channel-clipped?

hide
: cc-cb ( -- prc; y self -- f )
	1 proc-create ( prc )
	0.0 ( last-y ) ,
  does> { y self -- f }
	self @ { last-y }
	y      fabs 0.9999 f>=
	last-y fabs 0.9999 f>= && ( flag )
	y self ! ( last-y = y )
	( flag )
;
set-current

: channel-clipped? <{ :optional snd #f chn #f -- val }>
	doc" Return #t and a sample number if it finds clipping."
	cc-cb 0 #f snd chn #f scan-channel
;
previous

\ ;;; -------- sync-everything

: sync-everything ( -- )
	doc" Set the sync fields of all currently open sounds \
to the same, unique value."
	sync-max 1+ { new-sync }
	sounds each ( snd )
		new-sync swap set-sync drop
	end-each
;

\ === Moog Filter ===

hide
vct( 0.999969 0.990082 0.980347 0.970764 0.961304 0.951996 0.94281 0.933777
     0.924866 0.916077 0.90741 0.898865 0.890442 0.882141 0.873962 0.865906
     0.857941 0.850067 0.842346 0.834686 0.827148 0.819733 0.812378 0.805145
     0.798004 0.790955 0.783997 0.77713 0.770355 0.763672 0.75708 0.75058
     0.744141 0.737793 0.731537 0.725342 0.719238 0.713196 0.707245 0.701355
     0.695557 0.689819 0.684174 0.678558 0.673035 0.667572 0.66217 0.65686
     0.651581 0.646393 0.641235 0.636169 0.631134 0.62619 0.621277 0.616425
     0.611633 0.606903 0.602234 0.597626 0.593048 0.588531 0.584045 0.579651
     0.575287 0.570953 0.566681 0.562469 0.558289 0.554169 0.550079 0.546051
     0.542053 0.538116 0.53421 0.530334 0.52652 0.522736 0.518982 0.515289
     0.511627 0.507996 0.504425 0.500885 0.497375 0.493896 0.490448 0.487061
     0.483704 0.480377 0.477081 0.473816 0.470581 0.467377 0.464203 0.46109
     0.457977 0.454926 0.451874 0.448883 0.445892 0.442932 0.440033 0.437134
     0.434265 0.431427 0.428619 0.425842 0.423096 0.42038 0.417664 0.415009
     0.412354 0.409729 0.407135 0.404572 0.402008 0.399506 0.397003 0.394501
     0.392059 0.389618 0.387207 0.384827 0.382477 0.380127 0.377808 0.375488
     0.37323 0.370972 0.368713 0.366516 0.364319 0.362122 0.359985 0.357849
     0.355713 0.353607 0.351532 0.349457 0.347412 0.345398 0.343384 0.34137
     0.339417 0.337463 0.33551 0.333588 0.331665 0.329773 0.327911 0.32605
     0.324188 0.322357 0.320557 0.318756 0.316986 0.315216 0.313446 0.311707
     0.309998 0.308289 0.30658 0.304901 0.303223 0.301575 0.299927 0.298309
     0.296692 0.295074 0.293488 0.291931 0.290375 0.288818 0.287262 0.285736
     0.284241 0.282715 0.28125 0.279755 0.27829 0.276825 0.275391 0.273956
     0.272552 0.271118 0.269745 0.268341 0.266968 0.265594 0.264252 0.262909
     0.261566 0.260223 0.258911 0.257599 0.256317 0.255035
     0.25375 ) constant moog-gaintable

#( 0.0        -1.0
   0.03311111 -0.9
   0.06457143 -0.8
   0.0960272  -0.7
   0.127483   -0.6
   0.1605941  -0.5
   0.1920544  -0.4
   0.22682086 -0.3
   0.2615873  -0.2
   0.29801363 -0.1
   0.33278003 -0.0
   0.37086168  0.1
   0.40893877  0.2
   0.4536417   0.3
   0.5         0.4
   0.5463583   0.5
   0.5943719   0.6
   0.6556281   0.7
   0.72185487  0.8
   0.8096009   0.9
   0.87913835  0.95
   0.9933787   1.0
   1.0         1.0 ) constant moog-freqtable

#( "moog-freq"
   "moog-Q"
   "moog-s"
   "moog-y"
   "moog-fc" ) create-struct make-moog-filter-struct
set-current

<'> moog-freq@ alias moog-frequecy@ ( gen -- frq )
: moog-frequency! { gen frq -- }
	gen frq moog-freq!
	gen frq mus-srate f2/ f/ moog-freqtable 1.0 envelope-interp moog-fc!
;

: make-moog-filter ( freq Q -- gen )
	doc" Make a new moog-filter generator.  \
FREQ is the cutoff in Hz, Q sets the resonance: \
0 = no resonance, 1: oscillates at FREQUENCY."
	{ freq Q }
	make-moog-filter-struct { gen }
	gen freq moog-frequency!
	gen Q moog-Q!
	gen vct( 0.0 0.0 0.0 0.0 ) moog-s!
	gen 0.0 moog-y!
	gen
;

: moog-filter ( gen sig -- A )
	{ gen sig }
	0.25 sig gen moog-y@ f- f* { A }
	gen moog-s@ each { st }
		gen moog-fc@ A st f- f* A f+ -0.95 fmax 0.95 fmin to A
		gen moog-s@ i A vct-set! drop
		A st f+ -0.95 fmax 0.95 fmin to A
	end-each
	gen moog-fc@ 99.0 f* { ix }
	ix fround->s { ixint }
	ix ixint f- { ixfrac }
	A gen moog-Q@ f*
	    1.0 ixfrac f- moog-gaintable ixint 99 + vct-ref
	    f* ixfrac moog-gaintable ixint 100 + vct-ref f* f+ f*
	    gen swap moog-y!
	A
;
previous

\ 500.0 0.1 make-moog-filter value gen
\ lambda: <{ y -- val }> gen y moog-filter ; map-channel
\ gen 1.0 moog-filter

\ examp.fs ends here
