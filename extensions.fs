\ extensions.fs -- extensions.scm -> extensions.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/12/18 19:21:00
\ Changed: 14/12/03 17:15:55
\
\ @(#)extensions.fs	1.50 12/3/14

\ With comments and doc strings from extensions.scm.
\
\ remove-sound-property		( key :optional snd -- props )
\ set-sound-property-save-state-ignore ( key snd -- val )
\ remove-channel-property	( key :optional snd chn -- props )
\ set-channel-property-save-state-ignore ( key snd chn -- val )
\ channel-sync			( snd chn -- val )
\ set-channel-sync		( snd chn val -- )
\
\ normalize-mix			( filename beg in-chn snd chn -- scl )
\ enveloped-mix			( filename beg env -- )
\ map-sound-files		( func :optional dir -- lst )
\ for-each-sound-file		( func :optional dir -- )
\ match-sound-files		( func :optional dir -- ary )
\ selection-members		( -- array-of-arrays )
\ make-selection		( :optional beg end snd chn -- )
\
\ mix-channel			( fdata :optional beg dur s c edp -- val )
\ insert-channel		( fdata :optional beg 0 dur s c edp -- val )
\ redo-channel			( :optional edits snd chn -- )
\ undo-channel			( :optional edits snd chn -- )
\ any-env-channel		( en fc :optional beg dur s c edp or -- val )
\ sine-ramp			( r0 r1 :optional beg dur s c edpos -- val )
\ sine-env-channel		( en :optional beg dur s c edpos -- val )
\ blackman4-ramp		( r0 r1 :optional beg dur s c ep -- val )
\ blackman4-env-channel		( en :optional beg dur s c ep -- val )
\ ramp-squared			( r0 r1 :optional sym beg dur s c ep -- val )
\ env-squared-channel		( en :optional sym beg dur s c ep -- val )
\ ramp-expt			( rmp0 rmp1 exponent ... )
\ env-expt-channel		( en exponent ... )
\ offset-channel		( amount :optional beg dur s c ep -- val )
\ offset-sound			( offset :optional beg dur snd -- )
\ pad-sound			( beg dur :optional snd -- )
\ dither-channel		( :optional amount beg dur s c ep -- val )
\ dither-sound			( :optional amount beg dur s -- )
\ contrast-channel		( index :optional beg dur s c ep -- val )
\ contrast-sound		( index :optional beg dur snd -- )
\ scale-sound			( scl :optional beg dur snd -- )
\ normalize-sound		( amp :optional beg dur snd -- )
\ 
\ channels=			( s1 c1 s2 c2 :optional allowable-diff -- f )
\ channels-equal?		( s1 c1 s2 c2 :optional allowable-diff -- f )
\ mono->stereo			( new-name snd1 chn1 snd2 chn2 -- snd )
\ mono-files->stereo		( new-name chan1-name chan2-name -- snd )
\ stereo->mono			( orig-s c1-name c2-name -- snd0 snd1 )
\
\ with-reopen-menu		( -- )
\ with-buffers-menu		( -- )
\ 
\ if-cursor-follows-play-it-stays-where-play-stopped ( :optional enable -- )

require clm
require examp

\ ;;; -------- sound-property

: remove-sound-property <{ key :optional snd #f -- props }>
	doc" Remove key-value pair in the given sound's \
property list and return altered list."
	snd sound-properties key array-assoc-remove!
;

\ 'save-state-ignore key snd set-sound-property
: set-sound-property-save-state-ignore <{ key :optional snd #f -- val }>
	'save-state-ignore snd sound-property { ign }
	ign array? if
		ign key array-push
	else
		#( 'save-state-ignore key )
	then to ign
	'save-state-ignore ign snd set-sound-property
;

\ ;;; -------- channel-property

: remove-channel-property <{ key :optional snd #f chn #f -- props }>
	doc" Remove key-value pair in the given channel's \
property list and return altered list."
	snd chn channel-properties key array-assoc-remove!
;

: set-channel-property-save-state-ignore
    <{ key :optional snd #f chn #f -- val }>
	'save-state-ignore snd chn channel-property { ign }
	ign array? if
		ign key array-push
	else
		#( 'save-state-ignore key )
	then to ign
	'save-state-ignore ign snd chn set-channel-property
;

: channel-sync { snd chn -- val }
	'sync snd chn channel-property
;

: set-channel-sync { snd chn val -- }
	'sync val snd chn set-channel-property drop
;

\ ;;; -------- mix with result at original peak amp

: normalize-mix ( filename beg in-chn snd chn -- scl )
	doc" It is like mix but the mix result has same peak \
amp as unmixed SND/CHN (returns scaler)."
	{ filename beg in-chan snd chn }
	snd chn #f maxamp { original-maxamp }
	filename beg in-chan snd chn undef undef undef mix drop
	snd chn #f maxamp { new-maxamp }
	original-maxamp new-maxamp f<> if
		original-maxamp new-maxamp f/ { scaler }
		snd sync { old-sync }
		0 snd set-sync drop
		scaler snd chn scale-by drop
		old-sync snd set-sync drop
		scaler
	else
		1.0
	then
;

\ ;;;-------- mix with envelope on mixed-in file
\ ;;;
\ ;;; there are lots of ways to do this; this version uses functions
\ ;;;   from Snd, CLM, and Sndlib.

hide
: em-cb { amp-env rd -- prc; y self -- val }
	1 proc-create ( prc ) amp-env , rd ,
  does> { y self -- val }
	self @ { amp-env }
	self cell+ @ { rd }
	amp-env env rd readin f* y f+
;
set-current

: enveloped-mix ( filename beg en -- )
	doc" Mix FILENAME starting at BEG with amplitude envelope EN.\n\
\"pistol.snd\" 0 #( 0 0 1 1 2 0 ) enveloped-mix."
	{ filename beg en }
	filename framples { len }
	:envelope en :length len make-env { amp-env }
	filename make-readin { rd }
	amp-env rd em-cb beg len map-channel drop
;
previous

\ ;;; -------- map-sound-files, match-sound-files
\ ;;;
\ ;;; apply a function to each sound in dir
\ ;;;
\ ;;;   (map-sound-files (lambda (n) (if (> (mus-sound-duration n) 10.0)
\ ;;;     (snd-print n))))

: map-sound-files <{ func :optional dir "." -- lst }>
	doc" Apply FUNC to each sound file in DIR."
	dir sound-files-in-directory map
		func #( *key* ) run-proc
	end-map
;
\ lambda: <{ n -- str|#f }>
\ 	n mus-sound-duration 10.0 f> if
\		n snd-print ( n remains on stack )
\		cr
\	else
\		#f
\	then
\ ; map-sound-files

: for-each-sound-file <{ func :optional dir "." -- }>
	doc" Apply FUNC to each sound file in DIR."
	dir sound-files-in-directory each { f }
		func #( f ) run-proc drop
	end-each
;
0 [if]
	"/home/bil/sf" value loop-path
	lambda: <{ n -- }>
		loop-path "/" $+ n $+
		    <'> mus-sound-loop-info #t nil fth-catch if
			stack-reset
		else
			( loop-list ) empty? unless
				n snd-print drop
				cr
			then
		then
	; loop-path for-each-sound-file
[then]

: match-sound-files <{ func :optional dir "." -- ary }>
	doc" Apply FUNC to each sound file in DIR and \
returns an array of files for which FUNC does not return #f."
	#() { matches }
	dir sound-files-in-directory each { f }
		func #( f ) run-proc if
			matches f array-push drop
		then
	end-each
	matches
;
\ lambda: <{ n -- f }> /\.(wav?|snd)$/ n regexp-match ; "." match-sound-files

\ ;;; -------- selection-members
\ ;;;
\ ;;; returns a list of lists of (snd chn): channels in current selection

: selection-members ( -- array-of-lists )
	doc" Return an array of lists of #( snd chn ) indicating \
the channels participating in the current selection."
	#() { sndlist }
	undef selection? if
		sounds each { snd }
			snd channels 0 ?do
				snd i selection-member? if
					sndlist #( snd i ) array-push drop
				then
			loop
		end-each
	then
	sndlist
;

\ ;;; -------- make-selection
\ ;;;
\ ;;; the regularized form of this would use dur not end

hide
: add-chan-to-selection { beg end snd chn -- }
	beg integer? unless
		0 to beg
	then
	end integer? if
		end 1+
	else
		snd chn #f framples
	then beg - to end
	#t snd chn set-selection-member? drop
	beg snd chn set-selection-position drop
	end snd chn set-selection-framples drop
;
set-current

: make-selection <{ :optional beg 0 end #f snd #f chn #f -- }>
	doc" Make a selection like make-region but without creating a region.  \
It follows SND's sync field, and applies to all SND's \
channels if CHN is not specified.  \
END defaults to end of channel, BEG defaults to 0, \
SND defaults to the currently selected sound."
	snd snd-snd { current-sound }
	current-sound sound? unless
		'no-such-sound
		    #( "%s: can't find sound %s" get-func-name snd ) fth-throw
	then
	current-sound sync { current-sync }
	unselect-all drop
	chn integer? if
		beg end snd chn add-chan-to-selection
	else
		sounds each { s }
			snd #t =
			s current-sound = ||
			current-sync 0<> 
			current-sync s sync = && || if
				s channels 0 ?do
					beg end s i add-chan-to-selection
				loop
			then
		end-each
	then
;
previous

\ ;;; -------- mix-channel, insert-channel, c-channel

hide
: mc-cb { rd -- prc; y self -- val }
	1 proc-create ( prc )
	rd ,
  does> { y self -- val }
	self @ ( rd ) next-sample y f+
;
set-current

: mix-channel <{ file :optional beg 0 dur #f snd #f chn #f edpos #f -- r }>
	doc" Mix in FILE.  \
FILE can be the file name or a list #( file-name [beg [channel]] )."
	file string? if
		file
	else
		file 0 array-ref
	then { file-name }
	file-name find-file to file-name
	file-name unless
		'no-such-file #( "%s: %S" get-func-name file-name ) fth-throw
	then
	file string?
	file length 2 < || if
		0
	else
		file 1 array-ref
	then { file-beg }
	file string?
	file length 3 < || if
		0
	else
		file 2 array-ref
	then { file-channel }
	dur file-name mus-sound-framples file-beg - || { len }
	beg 0< if
		'no-such-sample #( "%s: %s" get-func-name beg ) fth-throw
	then
	len 0> if
		file-beg file-name file-channel 1 #f make-sampler { reader }
		"%S %s %s %s" #( file beg dur get-func-name )
		    string-format { origin }
		reader mc-cb beg len snd chn edpos origin map-channel
	else
		#f
	then
;
previous

: insert-channel <{ file :optional beg 0 dur #f snd #f chn #f edpos #f -- r }>
	doc" Insert the FILE.  \
FILE can be the file name or a list #( file-name [beg [channel]] )."
	file string? if
		file
	else
		file 0 array-ref
	then { file-name }
	file-name find-file to file-name
	file-name unless
		'no-such-file #( "%s: %S" get-func-name file-name ) fth-throw
	then
	file string?
	file length 2 < || if
		0
	else
		file 1 array-ref
	then { file-beg }
	file string?
	file length 3 < || if
		0
	else
		file 2 array-ref
	then { file-channel }
	dur file-name mus-sound-framples file-beg - || { len }
	beg 0< if
		'no-such-sample #( "%s: %s" get-func-name beg ) fth-throw
	then
	len 0> if
		file-beg file-name file-channel 1 #f make-sampler { reader }
		len 0.0 make-vct map!
			reader next-sample
		end-map { data }
		reader free-sampler drop
		"%S %s %s %s" #( file beg dur get-func-name )
		    string-format { origin }
		beg len data snd chn edpos #f origin insert-samples
	else
		#f
	then
;

\ ;;; -------- redo-channel, undo-channel

: redo-channel <{ :optional edits 1 snd #f chn #f -- }>
	doc" It's the regularized version of redo."
	snd fixnum?
	snd sync 0<> &&
	chn fixnum?  && if
		snd chn edit-position edits + snd chn set-edit-position drop
	else
		edits snd chn redo drop
	then
;

: undo-channel <{ :optional edits 1 snd #f chn #f -- }>
	doc" It's the regularized version of undo."
	snd fixnum?
	snd sync 0<> &&
	chn fixnum?  && if
		snd chn edit-position edits - 0 max
		    snd chn set-edit-position drop
	else
		edits snd chn undo drop
	then
;

\ ;;; -------- any-env-channel

hide
: aec-cb { en func beg dur snd chn edpos -- prc; self -- val }
	0 proc-create ( prc )
	en , func , beg , dur , snd , chn , edpos ,
  does> { self -- val }
	self           @ { en }
	self   cell+   @ { func }
	self 2 cells + @ { beg }
	self 3 cells + @ { dur }
	self 4 cells + @ { snd }
	self 5 cells + @ { chn }
	self 6 cells + @ { edpos }
	0.0 0.0 { x0 y0 }
	en 0 array-ref { x1 }
	en 1 array-ref { y1 }
	en envelope-last-x en 0 array-ref f- { xrange }
	beg { ramp-beg }
	0 { ramp-dur }
	en length 1- 2 ?do
		x1 to x0
		y1 to y0
		en i    array-ref to x1
		en i 1+ array-ref to y1
		x1 x0 f- xrange f/ dur f* fround->s to ramp-dur
		y0 y1 f= if
			y0 ramp-beg ramp-dur snd chn edpos scale-channel
		else
			func #( y0 y1 ramp-beg ramp-dur snd chn edpos ) run-proc
		then
		ramp-dur +to ramp-beg
	2 +loop
;
set-current

: any-env-channel
    <{ en func :optional beg 0 dur #f snd #f chn #f edpos #f origin #f -- val }>
	en nil? if
		#f
		exit
	then
	en envelope-length ( pts ) 1 = if
		en 0 array-ref beg dur snd chn edpos scale-channel
	else
		dur integer? unless
			snd chn #f framples to dur
		then
		en func beg dur snd chn edpos aec-cb origin as-one-edit
	then
;
previous

\ ;;; -------- sine-ramp sine-env-channel 

hide
: sine-ramp-cb { rmp0 rmpd incr -- prc; y self -- val }
	1 proc-create ( prc )
	rmp0 , rmpd , pi fnegate ( angle ) , incr ,
  does> { y self -- val }
	self           @ { rmp0 }
	self 1 cells + @ { rmpd }
	self 2 cells + @ { angle }
	self 3 cells + @ { incr }
	angle fcos 0.5 f* 0.5 f+ rmpd f* rmp0 f+ y f* ( val )
	angle incr f+ self 2 cells + ! ( angle += incr )
	( val )
;
set-current

: sine-ramp <{ rmp0 rmp1 :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
	doc" Produce a sinsusoidal connection from RMP0 to RMP1."
	"%s %s %s %s %s" #( rmp0 rmp1 beg dur get-func-name )
	    string-format { origin }
	dur number? if
		dur
	else
		snd chn edpos framples  beg  f-
	then pi swap f/ { incr }
	rmp0 rmp1 rmp0 f- incr sine-ramp-cb
	    beg dur snd chn edpos origin map-channel
;
previous

: sine-env-channel <{ en :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
	doc" Connect ENV's dots with sinusoids."
	"%s %s %s %s" #( en beg dur get-func-name ) string-format { origin }
	en <'> sine-ramp beg dur snd chn edpos origin any-env-channel
;
\ #( 0 0 1 1 2 -0.5 3 1 ) sine-env-channel

\ ;;; an obvious extension of this idea is to use the blackman fft
\ ;;;   window formulas to get sharper sinusoids (i.e. use the sum of n
\ ;;;   cosines, rather than just 1)
\ 
\ ;;; -------- blackman4-ramp, blackman4-env-channel

hide
: b4r-cb { rmp0 rmpd incr -- prc; y self -- val }
	1 proc-create ( prc )
	rmp0 , rmpd , 0.0 ( angle ) , incr ,
  does> { y self -- val }
	self           @ { rmp0 }
	self 1 cells + @ { rmpd }
	self 2 cells + @ { angle }
	self 3 cells + @ { incr }
	angle fcos { cx }
	cx 0.041194 f* -0.20762 f+
	cx f* 0.375696 f+ 
	cx f* -0.29145 f+ 
	cx f* 0.084037 f+
	rmpd f* rmp0 f+ 
	y f* ( val )
	angle incr f+ self 2 cells + ! ( angle += incr )
	( val )
;
set-current

: blackman4-ramp <{ rm0 rm1 :optional beg 0 dur #f snd #f chn #f ep #f -- val }>
	"%s %s %s %s %s" #( rm0 rm1 beg dur get-func-name )
	    string-format { origin }
	dur number? if
		dur
	else
		snd chn ep framples beg f-
	then pi swap f/ { incr }
	rm0 rm1 rm0 f- incr b4r-cb beg dur snd chn ep origin map-channel
;
previous

: blackman4-env-channel <{ en :optional beg 0 dur #f snd #f chn #f ep #f -- r }>
	"%s %s %s %s" #( en beg dur get-func-name ) string-format { origin }
	en <'> blackman4-ramp beg dur snd chn ep origin any-env-channel
;

\ ;;; -------- ramp-squared, env-squared-channel

hide
: rsq-cb { rmp0 rmpd incr -- prc; y self -- val }
	1 proc-create ( prc )
	rmp0 , rmpd , 0.0 ( angle ) , incr ,
  does> { y self -- val }
	self           @ { rmp0 }
	self 1 cells + @ { rmpd }
	self 2 cells + @ { angle }
	self 3 cells + @ { incr }
	angle dup f* rmpd f* rmp0 f+ y f* ( val )
	angle incr f+ self 2 cells + ! ( angle += incr )
	( val )
;
set-current

: ramp-squared <{ r0 r1 :optional sy #t beg 0 dur #f snd #f chn #f ep #f -- r }>
	doc" Connect R0 and R1 with an x^2 curve."
	"%s %s %s %s %s %s" #( r0 r1 sy beg dur get-func-name )
	    string-format { origin }
	dur number? if
		dur
	else
		snd chn ep framples beg f-
	then 1/f { incr }
	r0 r1 r0 f- incr rsq-cb beg dur snd chn ep origin map-channel
;
previous

hide
: esqc-cb { symmetric -- prc; r0 r1 b d s c e self -- val }
	7 proc-create ( prc )
	symmetric ,
  does> { r0 r1 b d s c e self -- val }
	r0 r1 self @ ( symmetric ) b d s c e ramp-squared
;
set-current

: env-squared-channel <{ en :optional sy #t beg 0 dur #f s #f c #f ep #f -- r }>
	doc" Connect ENV's dots with x^2 curves."
	"%s %s %s %s %s" #( en sy beg dur get-func-name )
	    string-format { origin }
	en sy esqc-cb beg dur s c ep origin any-env-channel
;
previous
\ #( 0 0 1 1 2 -0.5 3 1 ) env-squared-channel

\ ;;; -------- ramp-expt, env-expt-channel

: ramp-expt
    <{ rmp0 rmp1 expt :optional sym #t beg 0 dur #f snd #f chn #f ep #f -- r }>
	doc" Connect RMP0 and RMP1 with an x^EXPT curve."
	\ ;; a^x = exp(x * log(a))
	"%s %s %s %s %s %s %s"
	    #( rmp0 rmp1 expt sym beg dur get-func-name )
	    string-format { origin }
	dur number? if
		dur
	else
		snd chn ep framples beg d-
	then { len }
	len 1/f { incr }
	beg len snd chn ep samples { data }
	rmp1 rmp0 f- { scl }
	0.0 { angle }
	sym
	rmp1 rmp0 f< && if
		scl fnegate to scl
		1.0 to angle
		len 0 ?do
			data i
			    data i vct-ref
			    rmp1 scl angle expt f** f* f+
			    f*
			    vct-set! drop
			angle incr f- to angle
		loop
	else
		0.0 to angle
		len 0 ?do
			data i
			    data i vct-ref
			    rmp0 scl angle expt f** f* f+
			    f*
			    vct-set! drop
			angle incr f+ to angle
		loop
	then
	data beg len snd chn current-edit-position origin vct->channel
;

hide
: expc-cb { sym expt -- prc; r0 r1 b d s c e self -- val }
	7 proc-create ( prc ) expt , sym ,
  does> { r0 r1 b d s c e self -- val }
	r0 r1 self @ ( expt ) self cell+ @ ( sym ) b d s c e ramp-expt
;
set-current

: env-expt-channel
    <{ en ex :optional sym #t beg 0 dur #f snd #f chn #f ep #f -- r }>
	doc" Connect ENV's dots with x^exponent curves."
	ex 1.0 f= if
		en beg dur snd chn ep env-channel
	else
		"%s %s %s %s %s %s" #( en ex sym beg dur get-func-name )
		    string-format { origin }
		sym ex expc-cb { prc }
		en prc beg dur snd chn ep origin any-env-channel
	then
;
previous

\ ;;; -------- offset-channel

hide
: offc-cb { dc -- prc; y self -- val }
	1 proc-create ( prc )
	dc ,
  does> { y self -- val }
	self @ ( dc ) y f+
;
set-current

: offset-channel <{ amount :optional beg 0 dur #f snd #f chn #f edpos #f -- r }>
	doc" Add AMOUNT to each sample."
	"%s %s %s %s" #( amount beg dur get-func-name ) string-format { origin }
	amount offc-cb beg dur snd chn edpos origin map-channel
;
previous

: offset-sound <{ offset :optional beg 0 dur #f snd #f -- }>
	doc" Add OFFSET to every sample in SND."
	snd snd-snd to snd
	snd sound? if
		snd channels 0 ?do
			offset beg dur snd i ( chn ) #f offset-channel drop
		loop
	else
		'no-such-sound #( "%s: %s" get-func-name snd ) fth-throw
	then
;

\ ;;; -------- pad-sound

: pad-sound <{ beg dur :optional snd #f -- }>
	doc" Place a block of DUR zeros in every \
channel of SND starting at BEG."
	snd snd-snd to snd
	snd sound? if
		snd channels 0 ?do
			beg dur snd i ( chn ) #f pad-channel drop
		loop
	else
		'no-such-sound #( "%s: %s" get-func-name snd ) fth-throw
	then
;

\ ;;; -------- dither-channel

hide
: dith-cb { dither -- prc; y self -- val }
	1 proc-create ( prc )
	dither ,
  does> { y self -- val }
	self @ ( dither ) dup mus-random swap mus-random f+ y f+
;
set-current

: dither-channel
    <{ :optional amount 0.00006 beg 0 dur #f snd #f chn #f ep #f -- r }>
	doc" Add AMOUNT dither to each sample."
	"%s %s %s %s" #( amount beg dur get-func-name ) string-format { origin }
	amount f2/ dith-cb beg dur snd chn ep origin map-channel
;
previous

: dither-sound <{ :optional amount 0.00006 beg 0 dur #f snd #f -- }>
	doc" Add dithering to every sample of SND."
	snd snd-snd to snd
	snd sound? if
		snd channels 0 ?do
			amount beg dur snd i ( chn ) #f dither-channel drop
		loop
	else
		'no-such-sound #( "%s: %s" get-func-name snd ) fth-throw
	then
;

\ ;;; -------- contrast-channel

hide
: cntr-cb { index -- prc; y self -- val }
	1 proc-create ( prc )
	index ,
  does> { y self -- val }
	y two-pi f* fsin self @ ( index ) f*  y half-pi f*  f+ fsin
;
set-current

: contrast-channel <{ idx :optional beg 0 dur #f snd #f chn #f edpos #f -- r }>
	doc" Apply contrast-enhancement to the sound."
	"%s %s %s %s" #( idx beg dur get-func-name ) string-format { origin }
	idx cntr-cb beg dur snd chn edpos origin map-channel
;
previous

: contrast-sound <{ index :optional beg 0 dur #f snd #f -- }>
	doc" Apply contrast-enhancement to every channel of SND."
	snd snd-snd to snd
	snd sound? if
		snd channels 0 ?do
			index beg dur snd i ( chn ) #f contrast-channel drop
		loop
	else
		'no-such-sound #( "%s: %s" get-func-name snd ) fth-throw
	then
;

\ ;;; -------- scale-sound

: scale-sound <{ scl :optional beg 0 dur #f snd #f -- }>
	doc" Multiply every sample in SND by SCL."
	snd snd-snd to snd
	snd sound? if
		snd channels 0 ?do
			scl beg dur snd i ( chn ) #f scale-channel drop
		loop
	else
		'no-such-sound #( "%s: %s" get-func-name snd ) fth-throw
	then
;

\ ;;; -------- normalize-sound

: normalize-sound <{ amp :optional beg 0 dur #f snd #f -- }>
	doc" Scale SND to peak amplitude AMP."
	snd snd-snd to snd
	snd sound? if
		amp 0.0 snd #t #f maxamp each ( mx )
			fabs fmax
		end-each f/ { scl }
		snd channels 0 ?do
			scl beg dur snd i ( chn ) #f scale-channel drop
		loop
	else
		'no-such-sound #( "%s: %s" get-func-name snd ) fth-throw
	then
;

\ ;;; -------- channels-equal

hide
: c-equal-cb { rd diff -- prc; y self -- f }
	1 proc-create ( prc )
	rd , diff ,
  does> { y self -- f }
	self @ ( rd ) read-sample y f- fabs self cell+ @ ( diff ) f>
;
set-current

: channels= <{ snd1 chn1 snd2 chn2 :optional allowable-difference 0.0 -- f }>
	doc" Return #t if the two channels are the \
same (within diff) modulo trailing 0's."
	snd1 snd2 =
	chn1 chn2 = && if
		#t
	else
		snd1 chn1 #f maxamp { mx1 }
		snd2 chn2 #f maxamp { mx2 }
		mx1 mx2 f- fabs allowable-difference f> if
			#f
		else
			snd1 chn1 #f framples { len1 }
			snd2 chn2 #f framples { len2 }
			len1 len2 >= if
				len1 snd1 snd2 chn1 chn2
			else
				len2 snd2 snd1 chn2 chn1
			then { len s1 s2 c1 c2 }
			0 s2 c2 1 #f make-sampler { read2 }
			read2 allowable-difference c-equal-cb
			    0 len s1 c1 #f #f scan-channel not
		then
	then
;
previous

: channels-equal? <{ s1 c1 s2 c2 :optional allowable-difference 0.0 -- f }>
	doc" Return #t if the two channels are the same (within diff)."
	s1 c1 #f framples s2 c2 #f framples <> if
		#f
	else
		s1 c1 s2 c2 allowable-difference channels=
	then
;

\ ;;; -------- mono->stereo, mono-files->stereo

: mono->stereo ( new-name snd1 chn1 snd2 chn2 -- snd )
	doc" Take the two channels and combine \
them into a stereo sound NEW-NAME."
	{ new-name snd1 chn1 snd2 chn2 }
	snd1 chn1 edit-position { old-ed1 }
	snd2 chn2 edit-position { old-ed2 }
	:file new-name :channels 2 :srate snd1 srate new-sound { ind }
	ind 0 snd1 chn1 0 #f #f swap-channels drop
	ind 1 snd2 chn2 0 #f #f swap-channels drop
	old-ed1 snd1 chn1 set-edit-position drop
	old-ed2 snd2 chn2 set-edit-position drop
	ind
;
\ "test.snd" 0 0 1 0 mono->stereo

: mono-files->stereo ( new-name chan1-name chan2-name -- snd )
	doc" Combine two mono files into the stereo file NEW-NAME."
	{  new-name chan1-name chan2-name }
	chan1-name find-file to chan1-name
	chan1-name unless
		'no-such-file #( "%s: %S" get-func-name chan1-name ) fth-throw
	then
	chan1-name open-sound { ind1 }
	chan2-name find-file to chan2-name
	chan2-name unless
		'no-such-file #( "%s: %S" get-func-name chan2-name ) fth-throw
	then
	chan2-name open-sound { ind2 }
	new-name ind1 0 ind2 0 mono->stereo { ind3 }
	ind1 close-sound drop
	ind2 close-sound drop
	ind3
;
\ "test.snd" "oboe.snd" "pistol.snd" mono-files->stereo

: stereo->mono ( orig-snd chan1-name chan2-name -- snd0 snd1 )
	doc" Split a stereo sound into two mono sounds \
named CHAN1-NAME and CHAN2-NAME."
	{ orig-snd chan1-name chan2-name }
	orig-snd 0 edit-position { old-ed0 }
	orig-snd 1 edit-position { old-ed1 }
	:file chan1-name :srate orig-snd srate new-sound { chan1 }
	:file chan2-name :srate orig-snd srate new-sound { chan2 }
	orig-snd 0 chan1 0 0 #f #f swap-channels drop
	orig-snd 1 chan2 0 0 #f #f swap-channels drop
	old-ed0 orig-snd 0 set-edit-position drop
	old-ed1 orig-snd 1 set-edit-position drop
	chan1 chan2
;
\ 0 "hi1.snd" "hi2.snd" stereo->mono

\ === PREFERENCES DIALOG ===

\ --- reopen menu ---

hide
"empty" value reopen-empty
#() value reopen-names
#f value reopen-menu
16 value reopen-max-length

<'> noop 0 make-proc constant extensions-noop

: reopen-select-cb { brief-name long-name -- prc; self -- }
	0 proc-create ( prc )
	long-name , brief-name ,
  does> ( self -- )
	{ self }
	self @ { long-name }
	self cell+ @ { brief-name }
	reopen-menu brief-name remove-from-menu drop
	long-name file-exists? if
		long-name open-sound drop
	then
;

: add-to-reopen-menu <{ snd -- #f }>
	snd short-file-name { brief-name }
	snd file-name { long-name }
	reopen-names brief-name array-member? if
		#f
		exit
	then
	reopen-names reopen-empty array-member? if
		reopen-menu reopen-empty remove-from-menu drop
		#() to reopen-names
	then
	reopen-menu brief-name brief-name long-name reopen-select-cb
	    0 add-to-menu drop
	reopen-names brief-name array-push to reopen-names
	reopen-names length reopen-max-length > if
		reopen-menu reopen-names array-shift remove-from-menu drop
	then
	#f
;

: check-reopen-menu <{ file -- }>
	file #f file-basename { brief-name }
	reopen-names brief-name array-member? if
		reopen-menu brief-name remove-from-menu drop
		reopen-names brief-name array-index { idx }
		idx 0>= if
			reopen-names idx array-delete! drop
		then
	then
	reopen-names empty? if
		reopen-menu reopen-empty extensions-noop undef add-to-menu drop
		reopen-names reopen-empty array-push to reopen-names
	then
;
set-current

#f value including-reopen-menu	\ for prefs

: with-reopen-menu ( -- )
	including-reopen-menu unless
		#() to reopen-names
		reopen-menu false? if
			"Reopen" extensions-noop add-to-main-menu to reopen-menu
		then
		reopen-menu reopen-empty extensions-noop 0 add-to-menu drop
		reopen-names reopen-empty array-push to reopen-names
		#t to including-reopen-menu
		close-hook <'> add-to-reopen-menu add-hook!
		open-hook  <'> check-reopen-menu  add-hook!
	then
;
previous

\ --- buffers menu ---

hide
"empty" value buffer-empty
#() value buffer-names
#f value buffer-menu

: buffer-select-cb { file -- prc ; self -- }
	0 proc-create ( prc )
	file ,
  does> { self -- }
	self @ ( file ) 0 find-sound dup sound? if
		select-sound
	then drop
;

: open-buffer <{ file -- }>
	buffer-names buffer-empty array-member? if
		buffer-menu buffer-empty remove-from-menu drop
		#() to buffer-names
	then
	buffer-menu file file buffer-select-cb -1 add-to-menu drop
	buffer-names file array-push to buffer-names
;

: close-buffer <{ snd -- #f }>
	buffer-menu snd file-name remove-from-menu drop
	buffer-names snd file-name array-index { idx }
	idx 0>= if
		buffer-names idx array-delete! drop
	then
	buffer-names empty? if
		buffer-menu buffer-empty extensions-noop 0 add-to-menu drop
		buffer-names buffer-empty array-push to buffer-names
	then
	#f
;
set-current

#f value including-buffers-menu	\ for prefs

: with-buffers-menu ( -- )
	including-buffers-menu unless
		#() to buffer-names
		buffer-menu unless
			"Buffers" extensions-noop
			    add-to-main-menu to buffer-menu
		then
		buffer-menu buffer-empty extensions-noop 0 add-to-menu drop
		buffer-names buffer-empty array-push to buffer-names
		#t to including-buffers-menu
		open-hook  <'> open-buffer  add-hook!
		close-hook <'> close-buffer add-hook!
	then
;
previous

\ ;;; -------- cursor-follows-play and stays where it was when the play ended
\ moved from examp.fs to extensions.fs [ms]

hide
: current-cursor { snd chn -- cur }
	'cursor snd chn channel-property
;

: set-current-cursor { snd chn val -- }
	'cursor val snd chn set-channel-property
;

: original-cursor { snd chn -- cur }
	'original-cursor snd chn channel-property
;

: set-original-cursor { snd chn val -- }
	'original-cursor val snd chn set-channel-property
;

: local-start-playing-func <{ snd -- val }>
	snd channels 0 ?do
		snd i #f cursor { cur }
		snd i cur set-original-cursor
		snd i cur set-current-cursor
	loop
	#f
;

: local-stop-playing-func <{ snd -- val }>
	snd 0 current-cursor snd #t #f set-cursor
;
set-current

: if-cursor-follows-play-it-stays-where-play-stopped <{ :optional enable #t }>
	enable if
		start-playing-hook <'> local-start-playing-func add-hook!
		stop-playing-hook <'> local-stop-playing-func add-hook!
	else
		start-playing-hook <'> local-start-playing-func
		    remove-hook! drop
		stop-playing-hook <'> local-stop-playing-func remove-hook! drop
	then
;
previous

\ extensions.fs ends here
