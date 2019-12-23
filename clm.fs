\ clm.fs -- clm related base words, with-sound and friends

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 04/03/15 19:25:58
\ Changed: 19/12/23 15:34:09
\
\ @(#)clm.fs	2.5 12/23/19

\ clm-print		( fmt :optional args -- )
\ clm-message		( fmt :optional args -- )
\ 
\ now@			( -- secs )
\ now!			( secs -- )
\ step			( secs -- )
\ tempo@		( -- secs )
\ tempo!		( secs -- )
\ interval->hertz	( n -- r )
\ keynum->hertz		( n -- r )
\ hertz->keynum		( r -- n )
\ bpm->seconds		( bpm -- secs )
\ rhythm->seconds	( rhy -- secs )
\ 
\ fth-tempnam		( -- name )
\ make-default-comment	( -- str )
\ times->samples	( start dur -- len beg )
\ 
\ instrument:		( "name" -- )
\ ;instrument		( -- )
\ event:		( "name" -- )
\ ;event		( -- )
\
\ find-file		( file -- fname|#f )
\ snd-info		( obj -- )
\ play-sound		( :optional input verbose player -- )
\ clm-mix		( infile keyword-args -- )
\ 
\ ws-local-variables	( -- )
\ ws-info		( start dur vars -- start dur )
\ run			( start dur -- )
\ run-instrument	( start dur locsig-args -- )
\ end-run		( sample -- )
\ ws-out[a-c]		( idx val gen -- )
\ ws-out-any		( idx val chn gen -- )
\ reverb-info		( caller in-chans out-chans -- )
\ run-reverb		( dur -- in-val )
\ end-run-reverb	( -- )
\ end-run-reverb-out-1	( samp -- )
\ end-run-reverb-out-2	( samp1 samp2 -- )
\ end-run-reverb-out-4	( samp1 samp2 samp3 samp4 -- )
\ set-to-snd		( f -- )
\ run-gen-instrument	( start dur dummy --; samp args -- val )
\ end-run-gen		( -- )
\ run-gen-body		( samp y -- y' )
\ run-gen		( -- prc; y self -- y' )
\
\ ws-play		( ws -- )
\ ws-output		( ws -- fname )
\ ws-framples		( gen -- len )
\ ws-close-snd		( fname -- )
\ ws-is-output?		( gen -- f )
\ with-sound-main	( body-xt ws -- ws )
\ with-sound		( body-xt keyword-args -- ws )
\ clm-load		( fname keyword-args -- ws )
\ with-current-sound	( body-xt keyword-args -- )
\ scaled-to		( body-xt scl -- )
\ scaled-by		( body-xt scl -- )
\ with-offset		( body-xt secs -- )
\ with-mix		( body-str args fname start -- )
\ sound-let		( ws-xt-lst body-xt -- )
\
\ example instruments:
\ simp			( star dur freq amp -- )
\ src-simp		( start dur amp sr sr-env fname -- )
\ conv-simp		( start dur filt fname amp -- )
\ arpeggio		( start dur freq amp keyword-args -- )
\ simp-gen		( start dur freq amp --; samp args -- val )
\ violin-gen		( start dur freq amp keyword-args --; samp args -- val )
\
\ from generators.scm:
\ make-waveshape	( :optional freq parts wave size -- )
\ waveshape		( gen :optional index fm -- val )
\ waveshape?		( obj -- f )
\ partials->waveshape	( part :optional size -- wave )
\ make-sum-of-sines	( keyword-args -- gen )
\ sum-of-sines		( gen :optional fm -- val )
\ sum-of-sines?		( obj -- f )
\ make-sum-of-cosines	( keyword-args -- gen )
\ sum-of-cosines	( gen :optional fm -- val )
\ sum-of-cosines?	( obj -- f )
\ make-sine-summation	( keyword-args -- gen )
\ sine-summation	( gen :optional fm -- val )
\ sine-summation?	( obj -- f )

[ifundef] flog10
	<'> flog  alias flog10
	<'> fln   alias flog
	<'> flnp1 alias flogp1
[then]

\ if configured --with-shared-sndlib
'sndlib provided? [unless] dl-load sndlib Init_sndlib [then]

'snd provided? [unless]
	<'> noop alias channel->vct
	<'> noop alias close-sound
	<'> noop alias find-sound
	<'> noop alias framples
	<'> noop alias mix-vct
	<'> noop alias new-sound
	<'> noop alias open-sound
	<'> noop alias save-sound
	<'> noop alias scale-channel
	<'> noop alias sound?
	<'> noop alias update-sound
	<'> noop alias vct->channel

	\ Some generic words for non-Snd use.
	: channels <{ :optional obj #f -- n }>
		obj string? if
			obj mus-sound-chans
		else
			obj mus-generator? if
				obj mus-channels
			else
				obj object-length
			then
		then
	;

	: srate <{ :optional obj #f -- n }>
		obj string? if
			obj mus-sound-srate
		else
			mus-srate f>s
		then
	;

	: maxamp <{ :optional obj #f chn #f edpos #f -- r }>
		obj string? if
			obj mus-sound-maxamp
		else
			obj vct? if
				obj vct-peak
			else
				obj mus-generator? if
					obj mus-data vct-peak
				else
					0.0
				then
			then
		then
	;

	\ set to *clm-file-name* below
	defer *clm-fname*

	: file-name <{ :optional obj #f -- name }>
		obj string? if
			obj			
		else
			obj mus-generator? if
				obj mus-file-name
			else
				*clm-fname*
			then
		then
	;
[then]

[ifundef] clm-print
	\ "hello" clm-print
	\ "file %s, line %d\n" '( "clm.fs" 135 ) clm-print
	[ifdef] snd-print
		: clm-print ( fmt :optional args -- )
			string-format snd-print drop
		;
	[else]
		<'> fth-print alias clm-print ( fmt :optional args -- )
	[then]
[then]

[ifundef] stack-check
	\ added to examples/fth-lib/fth.fs on 2017/12/16
	hide
	: (stack-check) ( req xt -- )
		{ req xt }
		depth req < if
			depth { d }
			'wrong-number-of-args
			    #( "%s: not enough arguments, %s instead of %s"
			       xt xt->name
			       d
			       req ) fth-throw
		then
	;
	set-current

	: stack-check ( req -- )
		postpone running-word postpone (stack-check)
	; immediate compile-only
	previous
[then]

\ Put comment sign before output string and finish with carriage return.
: clm-message ( fmt :optional args -- )
	string-format { msg }
	"\\ %s\n" '( msg ) clm-print
;

\ === Notelist ===
hide
0.00 value *clm-current-time*
60.0 value *clm-tempo*
0.25 value *clm-beat*
set-current

: now@ ( -- secs )	*clm-current-time* ;
: now! ( secs -- )	to *clm-current-time* ;
: step ( secs -- )	now@ f+ now! ;
: tempo@ ( -- secs )	*clm-tempo* ;
: tempo! ( secs -- )	to *clm-tempo* ;
previous

\ --- Pitches ---
6.875 constant lowest-freq

: interval->hertz { n -- 5 }
	2.0 12.0 n 3.0 f+ f+ 12.0 f/ f** lowest-freq f*
;

: keynum->hertz { n -- r }
	2.0 n 3.0 f+ 12.0 f/ f** lowest-freq f*
;

: hertz->keynum ( r -- n )
	lowest-freq f/ 2.0 flogn 12.0 f* 3.0 f- f>s
;

hide
: pitch ( interval octave "name" --; self -- freq )
	{ interval octave }
	2.0 octave 1.0 f+ 12.0 f* interval 3.0 f+ f+ 12.0 f/ f** lowest-freq f*
	create ,
  does> ( self -- freq )
	@
;
set-current

 0 0 pitch |C0    1 0 pitch |Cs0    1 0 pitch |Df0
 2 0 pitch |D0    3 0 pitch |Ds0    3 0 pitch |Ef0
 4 0 pitch |E0    4 0 pitch |Ff0    5 0 pitch |Es0
 5 0 pitch |F0    6 0 pitch |Fs0    6 0 pitch |Gf0
 7 0 pitch |G0    8 0 pitch |Gs0    8 0 pitch |Af0
 9 0 pitch |A0   10 0 pitch |As0   10 0 pitch |Bf0
11 0 pitch |B0   11 0 pitch |Cf0   12 0 pitch |Bs0

 0 1 pitch |C1    1 1 pitch |Cs1    1 1 pitch |Df1
 2 1 pitch |D1    3 1 pitch |Ds1    3 1 pitch |Ef1
 4 1 pitch |E1    4 1 pitch |Ff1    5 1 pitch |Es1
 5 1 pitch |F1    6 1 pitch |Fs1    6 1 pitch |Gf1
 7 1 pitch |G1    8 1 pitch |Gs1    8 1 pitch |Af1
 9 1 pitch |A1   10 1 pitch |As1   10 1 pitch |Bf1
11 1 pitch |B1   11 1 pitch |Cf1   12 1 pitch |Bs1

 0 2 pitch |C2    1 2 pitch |Cs2    1 2 pitch |Df2
 2 2 pitch |D2    3 2 pitch |Ds2    3 2 pitch |Ef2
 4 2 pitch |E2    4 2 pitch |Ff2    5 2 pitch |Es2
 5 2 pitch |F2    6 2 pitch |Fs2    6 2 pitch |Gf2
 7 2 pitch |G2    8 2 pitch |Gs2    8 2 pitch |Af2
 9 2 pitch |A2   10 2 pitch |As2   10 2 pitch |Bf2
11 2 pitch |B2   11 2 pitch |Cf2   12 2 pitch |Bs2

 0 3 pitch |C3    1 3 pitch |Cs3    1 3 pitch |Df3
 2 3 pitch |D3    3 3 pitch |Ds3    3 3 pitch |Ef3
 4 3 pitch |E3    4 3 pitch |Ff3    5 3 pitch |Es3
 5 3 pitch |F3    6 3 pitch |Fs3    6 3 pitch |Gf3
 7 3 pitch |G3    8 3 pitch |Gs3    8 3 pitch |Af3
 9 3 pitch |A3   10 3 pitch |As3   10 3 pitch |Bf3
11 3 pitch |B3   11 3 pitch |Cf3   12 3 pitch |Bs3

 0 4 pitch |C4    1 4 pitch |Cs4    1 4 pitch |Df4
 2 4 pitch |D4    3 4 pitch |Ds4    3 4 pitch |Ef4
 4 4 pitch |E4    4 4 pitch |Ff4    5 4 pitch |Es4
 5 4 pitch |F4    6 4 pitch |Fs4    6 4 pitch |Gf4
 7 4 pitch |G4    8 4 pitch |Gs4    8 4 pitch |Af4
 9 4 pitch |A4   10 4 pitch |As4   10 4 pitch |Bf4
11 4 pitch |B4   11 4 pitch |Cf4   12 4 pitch |Bs4

 0 5 pitch |C5    1 5 pitch |Cs5    1 5 pitch |Df5
 2 5 pitch |D5    3 5 pitch |Ds5    3 5 pitch |Ef5
 4 5 pitch |E5    4 5 pitch |Ff5    5 5 pitch |Es5
 5 5 pitch |F5    6 5 pitch |Fs5    6 5 pitch |Gf5
 7 5 pitch |G5    8 5 pitch |Gs5    8 5 pitch |Af5
 9 5 pitch |A5   10 5 pitch |As5   10 5 pitch |Bf5
11 5 pitch |B5   11 5 pitch |Cf5   12 5 pitch |Bs5

 0 6 pitch |C6    1 6 pitch |Cs6    1 6 pitch |Df6
 2 6 pitch |D6    3 6 pitch |Ds6    3 6 pitch |Ef6
 4 6 pitch |E6    4 6 pitch |Ff6    5 6 pitch |Es6
 5 6 pitch |F6    6 6 pitch |Fs6    6 6 pitch |Gf6
 7 6 pitch |G6    8 6 pitch |Gs6    8 6 pitch |Af6
 9 6 pitch |A6   10 6 pitch |As6   10 6 pitch |Bf6
11 6 pitch |B6   11 6 pitch |Cf6   12 6 pitch |Bs6

 0 7 pitch |C7    1 7 pitch |Cs7    1 7 pitch |Df7
 2 7 pitch |D7    3 7 pitch |Ds7    3 7 pitch |Ef7
 4 7 pitch |E7    4 7 pitch |Ff7    5 7 pitch |Es7
 5 7 pitch |F7    6 7 pitch |Fs7    6 7 pitch |Gf7
 7 7 pitch |G7    8 7 pitch |Gs7    8 7 pitch |Af7
 9 7 pitch |A7   10 7 pitch |As7   10 7 pitch |Bf7
11 7 pitch |B7   11 7 pitch |Cf7   12 7 pitch |Bs7

 0 8 pitch |C8    1 8 pitch |Cs8    1 8 pitch |Df8
 2 8 pitch |D8    3 8 pitch |Ds8    3 8 pitch |Ef8
 4 8 pitch |E8    4 8 pitch |Ff8    5 8 pitch |Es8
 5 8 pitch |F8    6 8 pitch |Fs8    6 8 pitch |Gf8
 7 8 pitch |G8    8 8 pitch |Gs8    8 8 pitch |Af8
 9 8 pitch |A8   10 8 pitch |As8   10 8 pitch |Bf8
11 8 pitch |B8   11 8 pitch |Cf8   12 8 pitch |Bs8
previous

\ --- Note length ---
: bpm->seconds ( bpm -- secs )		60.0 swap f/ ;
: rhythm->seconds ( rhy -- secs )	4.0 tempo@ bpm->seconds f* f* ;

hide
: notelength ( scale "name" --; self -- r )
	create ,
  does> ( self -- r )
	@ ( scale ) rhythm->seconds ( secs )
;
set-current

 1.0     notelength |W			\ whole
 2.0 1/f notelength |H			\ half
 4.0 1/f notelength |Q			\ quarter
 8.0 1/f notelength |A			\ eighth
16.0 1/f notelength |S			\ sixteenth
32.0 1/f notelength |T			\ thirty-second
 1.0      2.0 1/f f+ notelength |W.
 2.0 1/f  4.0 1/f f+ notelength |H.
 4.0 1/f  8.0 1/f f+ notelength |Q.
 8.0 1/f 16.0 1/f f+ notelength |A.
16.0 1/f 32.0 1/f f+ notelength |S.
previous

\ === Global User Variables (settable in ~/.snd_forth or ~/.fthrc) ===
"fth 2019/12/23"	value *clm-version*
mus-lshort	value *clm-audio-format*
#f		value *clm-comment*
1.0		value *clm-decay-time*
#f		value *clm-delete-reverb*
"test.snd"	value *clm-file-name*
#f		value *clm-notehook*
#f		value *clm-play*
#f		value *clm-player*           
#f		value *clm-reverb*
1		value *clm-reverb-channels*
#()		value *clm-reverb-data*
"test.reverb"	value *clm-reverb-file-name*
#f		value *clm-statistics*
#f		value *clm-to-dac*
#f		value *clm-to-snd*
#f		value *clm-verbose*
#f		value *clm-debug*
"CLM_SEARCH_PATH" getenv "." || ":" string-split value *clm-search-list*

<'> *clm-search-list*
"List of directories with sound files." help-set!

#() value *clm-instruments*
<'> *clm-instruments*
"List of #( ins-name start dur local-vars ) elements.  \
Instruments using RUN or RUN-INSTRUMENT add entries to the list." help-set!

#() value *dac-instruments*
<'> *dac-instruments*
"List of collected dac instruments of #( ins-xt beg end ) elements.  \
Used with :to-dac #t." help-set!

#f value *clm-current-instrument*
<'> *clm-current-instrument*
"Current instrument set in INSTRUMENT:." help-set!

'snd provided? [unless]
	<'> *clm-file-name* is *clm-fname*
	1          constant default-output-chans
	44100      constant default-output-srate
	mus-next   constant default-output-header-type
	mus-lfloat constant default-output-sample-type
	1024       constant dac-size
[then]

default-output-chans		value *clm-channels*
default-output-srate		value *clm-srate*
locsig-type			value *clm-locsig-type*
default-output-header-type	value *clm-header-type*
default-output-sample-type	value *clm-sample-type*
dac-size			value *clm-rt-bufsize*
mus-file-buffer-size		value *clm-file-buffer-size*
mus-clipping			value *clm-clipped*
mus-array-print-length		value *clm-array-print-length*
clm-table-size			value *clm-table-size*
440.0				value *clm-default-frequency*
 
<'> *clm-table-size* lambda: <{ val -- res }>
	val set-clm-table-size
; trace-var

<'> *clm-srate* lambda: <{ val -- res }>
	val set-mus-srate f>s
; trace-var

\ internal global variables
*clm-channels*		value *channels*
*clm-verbose*		value *verbose*
*clm-notehook*		value *notehook*
*clm-decay-time*	value *decay-time*
0.0			value *degree*
1.0			value *distance*
0.05			value *reverbamount*
0			value *start*
#f			value *outgen*

'snd provided? [if]
	<'> snd-tempnam alias fth-tempnam
[else]
	hide
	user *fth-file-number*
	set-current

	: fth-tempnam ( -- name )
		doc" Look for environment variables TMP, TEMP, and TMPDIR.  \
If none of them is set, use /tmp as temporary path.  \
Produces something like:\n\
/tmp/fth-12345-1.snd\n\
/tmp/fth-12345-2.snd\n\
/tmp/fth-12345-3.snd\n\
..."
		1 *fth-file-number* +!
		environ { env }
		"%s/fth-%d-%d.snd"
		env "TMP" array-assoc-ref ?dup-if
			( tmp )
		else
			env "TEMP" array-assoc-ref ?dup-if
				( temp )
			else
				env "TMPDIR" array-assoc-ref ?dup-if
					( tmpdir )
				else
					"/tmp"
				then
			then
		then ( tmp ) getpid *fth-file-number* @  3 >array string-format
	;
	previous
[then]

: make-default-comment ( -- str )
	"\\ Written %s by %s at %s using clm (%s)"
	    #( "%a %d-%b-%y %H:%M %Z" current-time strftime
	       getlogin
	       gethostname
	       *clm-version* ) string-format
;

: times->samples { start dur -- len beg }
	start s>f seconds->samples { beg }
	dur   s>f seconds->samples { len }
	beg len d+ beg
;

\ === Helper functions for instruments ===
hide
: ins-info ( ins-name -- )   to *clm-current-instrument* ;
: event-info { ename -- }
	*clm-verbose* if
		ename #() clm-message
	then
;
set-current

: instrument: ( "name" -- )
	>in @ parse-word $>string { ins-name } >in !
	:
	ins-name postpone literal <'> ins-info compile,
;

: event: ( "name" -- )
	>in @ parse-word $>string { ev-name }  >in !
	:
	ev-name  postpone literal <'> event-info compile,
;

: ;instrument ( -- ) postpone ; ; immediate
<'> ;instrument alias ;event immediate
previous

<'> #{}      alias #w{}    ( -- ws )
<'> hash?    alias ws?     ( obj -- f )
<'> hash-ref alias ws-ref  ( ws key     -- val )
: ws-set! ( ws key val -- 'ws ) 3 pick >r hash-set! r> ;

\ === Playing Sound Files ===
: find-file ( file -- fname|#f )
	doc" Return the possible full path name of FILE if FILE exists or \
if FILE was found in *CLM-SEARCH-LIST*, otherwise return #f."
	{ file }
	file file-exists? if
		file
	else
		#f { fname }
		file string?
		*clm-search-list* array? && if
			*clm-search-list* each ( dir )
				"/" $+ file $+ dup file-exists? if
					to fname leave
				else
					drop
				then
			end-each
		then
		fname
	then
;

hide
: .maxamps { fname name sr scl? -- }
	fname file-exists? if
		fname mus-sound-maxamp { vals }
		scl? if
			" (before scaling)"
		else
			""
		then { scaled }
		vals length 0 ?do
			"%6s %c: %.3f (near %.3f secs)%s"
			    #( name
			       [char] A i 2/ +
			       vals i 1+ array-ref
			       vals i    array-ref sr f/
			       scaled ) clm-message
		2 +loop
	then
;

: .timer { obj -- }
	"    real: %.3f  (utime %.3f, stime %.3f)"
	    #( obj real-time@
	       obj user-time@
	       obj system-time@ ) clm-message
;

: .timer-ratio { sr frms obj -- }
	frms 0> if
		sr frms f/ { m }
		"   ratio: %.2f  (uratio %.2f)"
		    #( obj real-time@ m f*
		       obj user-time@ m f* )
	else
		"   ratio: no ratio" #()
	then clm-message
;

: .file { output chans srate -- }
	"filename: %s" #( output ) clm-message
	"   chans: %d, srate: %d" #( chans srate ) clm-message
;

: .file-info { output reverb-file-name scaled? timer -- }
	output mus-sound-duration { dur }
	output mus-sound-framples { frms }
	output mus-sound-chans { chans }
	output mus-sound-srate { srate }
	output mus-sound-sample-type mus-sample-type-name { st }
	output mus-sound-header-type mus-header-type-name { ht }
	output mus-sound-write-date { dt }
	"%a %b %d %H:%M:%S %Z %Y" dt strftime { tm }
	output mus-sound-comment { meta }
	output chans srate .file
	"  format: %s [%s]"		#( st ht ) clm-message
	"  length: %.3f  (%d framples)"	#( dur frms ) clm-message
	timer timer? if
		timer .timer
		srate frms timer .timer-ratio
	then
	output "maxamp" srate scaled? .maxamps
	reverb-file-name ?dup-if
		"revamp" srate #f .maxamps
	then
	" written: %s"			#( tm ) clm-message
	meta empty? unless
		" comment: %s"		#( meta ) clm-message
	then
;

: .dac-info { ws -- }
	ws :timer ws-ref { timer }
	timer if
		timer .timer
		ws :srate ws-ref ws :framples ws-ref timer .timer-ratio
	then
;
set-current

\ obj:	a string or ws object
\	string: an existing file name (for play-sound)
\	    ws: *clm-to-dac* is #t and keyargs are not used
: snd-info { obj -- }
	obj string? if
		obj #f #f #f .file-info
	else
		*clm-to-dac* if
			obj .dac-info
		else
			obj :output ws-ref		{ output }
			obj :reverb-file-name ws-ref	{ reverb-file }
			obj :scaled-to ws-ref
			obj :scaled-by ws-ref ||	{ scaled }
			obj :timer ws-ref		{ tm }
			output reverb-file scaled tm .file-info
		then
	then
;

: dac-info { ws -- }
	ws :output ws-ref ws :channels ws-ref ws :srate ws-ref .file
;
previous

\ === Playing Sounds ===

defer ws-play

: play-sound <{ :optional
    input   *clm-file-name*
    verbose *clm-verbose*
    player  *clm-player* -- }>
	doc" Play sound file INPUT.\n\
\"bell.snd\" #t play-sound\n\
\"bell.snd\" #f \"sndplay\" play-sound"
	input string? if
		input find-file dup unless
			drop
			'no-such-file
			    #( "%s: %s" get-func-name input ) fth-throw
		else
			to input
		then
	else
		input mus-output? if
			input mus-file-name
		else
			*output* mus-output? if
				*output* mus-file-name
			else
				#f
			then
		then to input
	then
	input if
		verbose if
			input snd-info
		then
		#w{} :output input ws-set! :player player ws-set! ws-play
	then
;

'snd provided? [unless]
	: play ( keyword-args :optional obj -- f )
		:start		#f get-optkey drop
		:end		#f get-optkey drop
		:channel	#f get-optkey drop
		:edit-position	#f get-optkey drop
		:out-channel	#f get-optkey drop
		:with-sync	#f get-optkey drop
		:wait		#f get-optkey drop
		:stop		#f get-optkey drop
		:srate		#f get-optkey drop
		:channels	#f get-optkey drop
		0 *clm-file-name* get-optarg #f #f play-sound
		#f
	;
[then]

: clm-mix <{ infile :key
    output #f
    output-frame 0
    framples #f
    input-frame 0
    scaler #f -- }>
	doc" Mix files in with-sound's *output* generator.\n\
\"oboe.snd\" clm-mix\n\
Mixes oboe.snd in *output* at *output*'s \
location 0 from oboe.snd's location 0 on.  \
The whole oboe.snd file will be mixed in because :framples is not specified."
	0 { chans }
	*output* mus-output? { outgen }
	*output* sound? { outsnd }
	output unless
		outgen if
			*output* mus-channels to chans
			*output* mus-file-name to output
		else
			outsnd if
				*output* channels to chans
				*output* file-name to output
			else
				'with-sound-error
				    #( "%s: *output* gen or :output required"
				       get-func-name ) fth-throw
			then
		then
	then
	infile find-file to infile
	infile unless
		'file-not-found
		    #( "%s: %S not found" get-func-name infile ) fth-throw
	then
	framples
	infile mus-sound-framples || dup unless
		drop undef
	then to framples
	outgen if
		*output* mus-close drop
	else
		outsnd if
			*output* save-sound drop
			*output* close-sound drop
		then
	then
	scaler number? if
		scaler f0<> scaler 1.0 f<> && if
			chans chans * scaler make-vct
		else
			#f
		then
	else
		#f
	then { mx }
	output       ( outfile )
	infile       ( infile )
	output-frame ( outloc )
	framples     ( framples )
	input-frame  ( inloc )
	mx           ( matrix )
	#f           ( envs ) mus-file-mix drop
	outgen if
		output continue-sample->file to *output*
	else
		outsnd if
			output open-sound to *output*
		then
	then
;

\ === With-Sound Run-Instrument ===
"with-sound error"     create-exception with-sound-error
"with-sound interrupt" create-exception with-sound-interrupt
#() value *ws-args*			\ array for recursive with-sound calls 

: ws-local-variables ( -- )
	nil { vals }
	*clm-instruments* empty? if
		"*clm-instruments* is empty" #() clm-message
	else
		"" #() clm-message
		*clm-instruments* each to vals
			"=== %s [%.3f-%.3f] ==="
			#( vals 0 array-ref
			   vals 1 array-ref
			   vals 2 array-ref ) clm-message
			vals 3 array-ref each { var }
				\ var: '( name value ) )
				"%16s = %s" var clm-message
			end-each
			"" #() clm-message
		end-each
	then
;

: ws-info { start dur vars -- start dur }
	start s>f to start
	dur   s>f to dur
	#( *clm-current-instrument* start dur vars ) { args }
	*clm-instruments* args array-push drop
	args array-pop drop
	*notehook* word? if
		*notehook* args run-proc drop
	then
	start dur
;

hide
defer (run) ( start dur vars -- end beg )

: (run-snd) ( start dur vars -- end beg )
	ws-info ( start dur ) nip seconds->samples 0
;

: (run-clm) ( start dur vars -- end beg )
	ws-info ( start dur ) times->samples ( end beg )
;

defer (run-instrument) ( start dur args vars -- end beg )

: f0<>|| { res def -- val }
	res number? if
		res f0<> if
			res
		else
			def
		then
	else
		def
	then
;

: (run-snd-instrument) { start dur args vars -- end beg }
	args hash? unless
		#{} to args
	then
	args :degree	hash-ref 45.0	f0<>|| to *degree*
	args :distance	hash-ref 1.0	f0<>|| to *distance*
	args :reverb	hash-ref 0.05	f0<>|| to *reverbamount*
	start s>f seconds->samples to *start*
	dur   s>f seconds->samples 0.0 make-vct to *outgen*
	start dur vars (run-snd) ( end beg )
;

: (run-clm-instrument) { start dur args vars -- end beg }
	args hash? unless
		#{} to args
	then
	:degree		args :degree	hash-ref 45.0		f0<>||
	:distance	args :distance	hash-ref *distance*	f0<>||
	:reverb		args :reverb	hash-ref *reverbamount*	f0<>||
	:channels	args :channels	hash-ref *channels*	||
	:output		args :output	hash-ref *output*	||
	:revout		args :revout	hash-ref *reverb*	||
	:type		args :type	hash-ref locsig-type	||
	make-locsig to *outgen*
	start dur vars (run-clm) ( end beg )
;

defer (end-run) ( val idx -- )

: (end-snd-run) ( val idx -- )
	\ gen idx val gen-set!
	\ *outgen* => vct
	*outgen* swap rot vct-set! drop
;

: (end-clm-run) ( val idx -- )
	\ gen idx val gen-set!
	\ *outgen* => locsig gen
	*outgen* swap rot locsig drop
;

defer (end-run-finish) ( -- )

: (end-snd-run-finish) ( -- )
	*start*         { beg }
	*output*        { snd }
	0               { chn }
	snd channels    { chans }
	*outgen*        { v }
	*degree*        { frac }
	*distance*      { scl }
	scl             { s }
	chans 1 = if
		v scl vct-scale! beg snd chn #f undef mix-vct drop
	else
		*degree* 90.0 f/ to frac
		scl 1.0 frac f- f*	{ left }
		scl frac f*		{ right }
		chans 2 = if
			0 to chn
			v vct-copy left vct-scale!
			beg snd chn #f undef mix-vct drop
			1 to chn
			v right vct-scale!
			beg snd chn #f undef mix-vct drop
		else
			chans 1 do i to chn
				i 2 mod if
					right
				else
					left
				then to s
				v vct-copy s vct-scale!
				beg snd chn #f undef mix-vct drop
			loop
			0 to chn
			right to s
			v s vct-scale!
			beg snd chn #f undef mix-vct drop
		then
	then
	*reverb* sound? if
		v vct-length { len }
		v *reverbamount* vct-scale!
		beg len *reverb* 0 #f undef vct->channel drop
	then
;
set-current

\ RUN/LOOP is only a simple replacement of
\ start dur TIMES->SAMPLES ?DO ... LOOP
\
\ RUN-INSTRUMENT/END-RUN for use with with-sound instruments.
\ Requires at least an opened *output* (file->sample or sound),
\ optional an opened *reverb* generator or sound.  At the end of
\ the loop a sample value must remain on stack!
\
\ instrument: foo
\   0 0.1 nil run-instrument 0.2 end-run
\ ;instrument
\ <'> foo :srate 22050 with-sound
\
\ fills a sound file of length 0.1 seconds with 2205 samples (srate
\ 22050) with 0.2.
\ 
\ 0.0 1.0                   RUN            ... LOOP
\ 0.0 1.0 #{ :degree 45.0 } RUN-INSTRUMENT ... END-RUN
: run ( start dur -- )
	postpone local-variables
	postpone (run)
	postpone ?do
; immediate compile-only

: run-instrument ( start dur locsig-args -- )
	postpone local-variables
	postpone (run-instrument)
	postpone ?do
; immediate compile-only

: end-run ( sample -- )
	postpone r@
	postpone (end-run)
	postpone loop
	postpone (end-run-finish)
; immediate compile-only
previous

defer ws-outa		( idx val gen -- )
defer ws-outb		( idx val gen -- )
defer ws-outc		( idx val gen -- )
defer ws-outd		( idx val gen -- )
defer ws-out-any	( idx val chn gen -- )

\ XXX: ws-snd-outX
\ We vct-set sample in a fresh vct, later vct-mix'ed in the sound.
\ No need for vct-ref val f+ vct-set here.

: ws-snd-out-any { idx val chn gen -- }
	swap array-ref rot rot vct-set! drop
;

: ws-snd-outa ( idx val gen -- )
	0 array-ref rot rot vct-set! drop
;

: ws-snd-outb ( idx val gen -- )
	1 array-ref rot rot vct-set! drop
;

: ws-snd-outc ( idx val gen -- )
	2 array-ref rot rot vct-set! drop
;

: ws-snd-outd ( idx val gen -- )
	3 array-ref rot rot vct-set! drop
;

: ws-clm-out-any ( idx val chn gen -- )	out-any drop ;
: ws-clm-outa	( idx val gen -- )	outa drop ;
: ws-clm-outb	( idx val gen -- )	outb drop ;
: ws-clm-outc	( idx val gen -- )	outc drop ;
: ws-clm-outd	( idx val gen -- )	outd drop ;

\ XXX: *clm-to-snd* == #t   idx gen ina etc.
\ If gen is a vct, ina, inb, and in-any all return the same value.
\ If gen is an array, it should be an array of numbers not of vcts.
\
\ #( vct( 0 1 2 ) vct( 2 1 0 ) ) value gen
\ builds not the expected stereo gen
\ 0 gen inb => garbage ( first vct taken as double )

<'> ina		alias ws-ina
<'> inb		alias ws-inb
<'> in-any	alias ws-in-any

: reverb-info { caller in-chans out-chans -- }
	"%s on %d in and %d out channels"
	    #( caller in-chans out-chans ) clm-message
;

hide
defer (run-reverb) ( dur vars -- end beg )

: rr-before-reverb { dur vars -- end beg }
	0.0 dur vars ws-info ( start dur ) times->samples { end beg }
	*verbose* if
		*clm-current-instrument* *reverb* channels *output* channels
		    reverb-info
	then
	end beg
;

#f value old-*output*
#f value old-*reverb*

: (run-snd-reverb) { dur vars -- end beg }
	dur vars rr-before-reverb { end beg }
	*output* save-sound drop
	*output* to old-*output*
	*output* channels make-array map!
		beg end *output* i #f channel->vct
	end-map to *output*
	*reverb* save-sound drop
	*reverb* to old-*reverb*
	beg end *reverb* 0 #f channel->vct to *reverb*
	\ *output* is Array of Vcts
	\ *reverb* is Vct
	beg to *start*
	end 0
;

: (run-clm-reverb) { dur vars -- end beg }
	dur vars rr-before-reverb { end beg }
	*reverb* to old-*reverb*
	*reverb* mus-file-name { revfile }
	revfile undef make-file->sample to *reverb*
	*reverb* file->sample? unless
		'with-sound-error
		    #( "%s: can't open %s" get-func-name revfile ) fth-throw
	then
	\ *output* is sample->file (mus-output)
	\ *reverb* is file->sample (mus-input)
	beg to *start*
	end 0
;

: (run-reverb-inval-1) ( idx -- in-val )
	*reverb* ina
;

: (end-run-reverb-1) ( samp idx -- )
	swap *output* ws-outa
;

: (end-run-reverb-2) { samp1 samp2 idx -- }
	idx samp1 *output* ws-outa
	idx samp2 *output* ws-outb
;

: (end-run-reverb-4) { samp1 samp2 samp3 samp4 idx -- }
	idx samp1 *output* ws-outa
	idx samp2 *output* ws-outb
	idx samp3 *output* ws-outc
	idx samp4 *output* ws-outd
;

defer (end-run-reverb-finish) ( -- )

: (end-snd-run-reverb-finish) ( -- )
	*output* each ( v )
		*start* old-*output* i #f undef mix-vct drop
	end-each
	old-*output* to *output*
	old-*reverb* to *reverb*
;

: (end-clm-run-reverb-finish) ( -- )
	*output* mus-close drop
	*reverb* mus-close drop
	old-*reverb* to *reverb*
;
set-current

\ RUN-REVERB/END-RUN-REVERB-OUT-1|2|4 for use with with-sound reverb
\ instruments.  Requires an opened *output* (file->sample or sound)
\ and an opened *reverb* generator or sound.  The inval is the sample
\ from the reverb file, the out samples are written to the output
\ file.
\
\ run-reverb ( dur -- inval )
\ end-run-reverb-out-1 ( samp -- )
\ end-run-reverb-out-2 ( samp1 samp2 -- )
\
\ reverb for mono output file:
\ 10.0 run-reverb { inval }
\	inval 2.0 f*
\ end-run-reverb-out-1
\
\ reverb for stereo output file:
\ 10.0 run-reverb { inval }
\	inval 2.0 f* ( samp1 )
\	inval 4.0 f* ( samp1 samp2 )
\ end-run-reverb-out-2
\
\ reverb for quad output file:
\ 10.0 run-reverb { inval }
\	inval 2.0 f* ( samp1 )
\	inval 4.0 f* ( samp1 samp2 )
\	inval 2.0 f* ( samp1 samp2 samp3 )
\	inval 4.0 f* ( samp1 samp2 samp3 samp4 )
\ end-run-reverb-out-4

: run-reverb ( dur -- in-val )
	postpone local-variables
	postpone (run-reverb)
	postpone ?do
	postpone r@
	postpone (run-reverb-inval-1)
; immediate compile-only

: end-run-reverb ( -- )
	postpone loop
	postpone (end-run-reverb-finish)
; immediate compile-only

: end-run-reverb-out-1 ( samp -- )
	postpone r@
	postpone (end-run-reverb-1)
	postpone loop
	postpone (end-run-reverb-finish)
; immediate compile-only

: end-run-reverb-out-2 ( samp1 samp2 - )
	postpone r@
	postpone (end-run-reverb-2)
	postpone loop
	postpone (end-run-reverb-finish)
; immediate compile-only

: end-run-reverb-out-4 ( samp1 samp2 samp3 samp4 - )
	postpone r@
	postpone (end-run-reverb-4)
	postpone loop
	postpone (end-run-reverb-finish)
; immediate compile-only

: set-to-snd ( f -- )
	( f ) 'snd provided? && if
		#t to *clm-to-snd*
		\ RUN-INSTRUMENT
		<'> (run-snd)			[is] (run)
		<'> (run-snd-instrument)	[is] (run-instrument)
		<'> (end-snd-run)		[is] (end-run)
		<'> (end-snd-run-finish)	[is] (end-run-finish)
		\ RUN-REVERB
		<'> (run-snd-reverb)		[is] (run-reverb)
		<'> (end-snd-run-reverb-finish)	[is] (end-run-reverb-finish)
		\ OUT-ANY
		<'> ws-snd-outa			[is] ws-outa
		<'> ws-snd-outb			[is] ws-outb
		<'> ws-snd-outc			[is] ws-outc
		<'> ws-snd-outd			[is] ws-outd
		<'> ws-snd-out-any		[is] ws-out-any
	else
		#f to *clm-to-snd*
		\ RUN-INSTRUMENT
		<'> (run-clm)			[is] (run)
		<'> (run-clm-instrument)	[is] (run-instrument)
		<'> (end-clm-run)		[is] (end-run)
		<'> noop			[is] (end-run-finish)
		\ RUN-REVERB
		<'> (run-clm-reverb)		[is] (run-reverb)
		<'> (end-clm-run-reverb-finish)	[is] (end-run-reverb-finish)
		\ OUT-ANY
		<'> ws-clm-outa			[is] ws-outa
		<'> ws-clm-outb			[is] ws-outb
		<'> ws-clm-outc			[is] ws-outc
		<'> ws-clm-outd			[is] ws-outd
		<'> ws-clm-out-any		[is] ws-out-any
	then
;
previous

\ Instruments prepared with run-gen-instrument ... end-run-gen can
\ be used for map-channel or ":to-dac #t with-sound".  Example
\ instruments and gen tests can be found at the end of this file.
\
\ <'> test-gen :channels 1 :srate 22050 :to-dac #t with-sound drop
\ <'> violin-gen-test :channels 1 :srate 11025 :to-dac #t with-sound drop
\ or
\ test-gen run-gen map-channel
\ violin-gen-test run-gen map-channel
hide
lambda: <{ a b -- f }>
	a 1 array-ref { ba }
	b 1 array-ref { bb }
	ba bb < if
		-1
	else
		ba bb > if
			1
		else
			0
		then
	then
; value dac-sort

lambda: <{ a b -- f }>
	a 1 array-ref { ba }
	b 1 array-ref { bb }
	ba bb f< if
		-1
	else
		ba bb f> if
			1
		else
			0
		then
	then
; value clm-sort

: (run-gen-instrument) { start dur dummy vars -- vars }
	start s>f to start
	dur   s>f to dur
	#( *clm-current-instrument* start dur vars ) { args }
	*clm-instruments* args array-push clm-sort array-sort! drop
	start dur times->samples { end beg }
	1 proc-create { prc }
	*dac-instruments* #( prc beg end ) array-push dac-sort array-sort! drop
	vars
;
set-current

: run-gen-instrument ( start dur dummy --; samp args -- val )
	\ This replaces the following:
	\ 	start dur dummy local-variables (instrument-does) ,
	\   does> ( samp self -- val )
	\ 	@	this replaces self's address with its contents,
	\ 		a hash with local variables
	\ the stack is now: ( samp args )
	postpone local-variables
	postpone (run-gen-instrument) ( vars ) postpone compile,
	postpone does> ( samp self -- val )
	postpone @ ( samp args )
; immediate compile-only

<'> noop	alias end-run-gen
<'> hash-ref	alias args@

: run-gen-body { samp -- res }
	nil nil 0 0 { args prc beg end }
	0.0 { sum }
	*dac-instruments* each to args
		args 0 array-ref to prc
		args 1 array-ref to beg
		args 2 array-ref to end
		samp beg end within if
			samp prc execute sum f+ to sum
		then
	end-each
	sum
;

\ Returns a proc ( y -- res ) for use with map-channel.
\ Requires a filled *dac-instruments* variable, usually done with
\ run-gen-instrument ... end-run-gen prepared functions, see simp-gen
\ and violin-gen at the end of this file.
: run-gen ( -- prc; y self -- res )
	*dac-instruments* empty? if
		'with-sound-error
		    #( "%s: filled *dac-instruments* required"
		       get-func-name ) fth-throw
	then
	nil nil 0 0 0 { args prc beg end len }
	*dac-instruments* each to args
		args 2 array-ref len max to len
	end-each
	len 0.0 make-vct { v }
	*dac-instruments* each to args
		args 0 array-ref to prc
		args 1 array-ref to beg
		args 2 array-ref to end
		end beg ?do
			i prc execute v i rot object-set+!
		loop
	end-each
	1 proc-create ( prc )
	v ,
  does> { y self -- res }
	self @ ( v ) cycle-ref y f+
;
previous

hide
: ws-get-snd ( ws -- snd )
	( ws ) :output ws-ref find-file { fname }
	fname 0 find-sound { snd }
	snd sound? if
		snd save-sound drop
		snd close-sound drop
	then
	fname open-sound
;

: ws-scaled-to { ws -- }
	ws :scaled-to ws-ref { scale }
	'snd provided? if
		ws ws-get-snd { snd }
		0.0 snd #t #f maxamp each
			fmax
		end-each { mx }
		mx f0<> if
			scale mx f/ to scale
			snd #f #f framples { len }
			ws :channels ws-ref 0 ?do
				scale 0 len snd i ( chn ) #f scale-channel drop
			loop
		then
		snd save-sound drop
	else
		ws :output ws-ref mus-sound-maxamp { smax }
		0.0 smax length 1 ?do
			smax i array-ref fabs fmax
		2 +loop { mx }
		mx f0<> if
			ws :output ws-ref :scaler scale mx f/ clm-mix
		then
	then
;

: ws-scaled-by { ws -- }
	ws :scaled-by ws-ref { scale }
	'snd provided? if
		ws ws-get-snd { snd }
		snd #f #f framples { len }
		ws :channels ws-ref 0 ?do
			scale 0 len snd i ( chn ) #f scale-channel drop
		loop
		snd save-sound drop
	else
		ws :output ws-ref :scaler scale clm-mix
	then
;

: ws-before-output { ws -- }
	ws     :old-table-size		clm-table-size		ws-set!
	( ws ) :old-file-buffer-size	mus-file-buffer-size	ws-set!
	( ws ) :old-array-print-length	mus-array-print-length	ws-set!
	( ws ) :old-clipped		mus-clipping		ws-set!
	( ws ) :old-srate		mus-srate f>s		ws-set!
	( ws ) :old-locsig-type		locsig-type		ws-set!
	( ws ) :old-*output*		*output*		ws-set!
	( ws ) :old-*reverb*		*reverb*		ws-set!
	( ws ) :old-verbose		*verbose*		ws-set! 
	( ws ) :old-debug		*clm-debug*		ws-set!
	( ws ) :old-channels		*channels*		ws-set!
	( ws ) :old-notehook		*notehook*		ws-set!
	( ws ) :old-decay-time		*clm-decay-time*	ws-set! to ws
	ws :verbose	ws-ref to *verbose*
	ws :debug	ws-ref to *clm-debug*
	ws :channels	ws-ref to *channels*
	ws :notehook	ws-ref to *notehook*
	ws :decay-time	ws-ref to *clm-decay-time*
	*clm-file-buffer-size*		set-mus-file-buffer-size drop
	*clm-array-print-length*	set-mus-array-print-length drop
	ws :scaled-to ws-ref
	ws :scaled-by ws-ref || if
		#( mus-bfloat
		   mus-lfloat
		   mus-bdouble
		   mus-ldouble ) ws :sample-type ws-ref array-member? if
			#f
		else
			*clm-clipped*
		then
	else
		*clm-clipped*
	then set-mus-clipping drop
	ws :to-dac ws-ref if
		#t set-mus-clipping drop
	then
	ws :srate	ws-ref set-mus-srate drop
	ws :locsig-type	ws-ref set-locsig-type drop
;

: ws-after-output { ws -- ws }
	ws :old-table-size		ws-ref set-clm-table-size drop
	ws :old-file-buffer-size	ws-ref set-mus-file-buffer-size drop
	ws :old-array-print-length	ws-ref set-mus-array-print-length drop
	ws :old-clipped			ws-ref set-mus-clipping drop
	ws :old-srate			ws-ref set-mus-srate drop
	ws :old-locsig-type		ws-ref set-locsig-type drop
	ws :old-*output*		ws-ref to *output*
	ws :old-*reverb*		ws-ref to *reverb*
	ws :old-verbose			ws-ref to *verbose*
	ws :old-debug			ws-ref to *clm-debug*
	ws :old-channels		ws-ref to *channels*
	ws :old-notehook		ws-ref to *notehook*
	ws :old-decay-time		ws-ref to *clm-decay-time*
	*ws-args* array-pop
;

: set-args { key def ws -- }
	key def get-optkey ws key rot ws-set! to ws
;
set-current

\ player: xt, proc, string, or #f.
\
\  xt/proc: player #( output ) run-proc
\   string: "player output" system
\ else snd: output :wait #t play
\   or clm: output play-sound
\ 
\ A player may look like this:
\
\ : play-3-times { output -- }
\ 	3 0 do
\		output :wait #t play drop
\	loop
\ ;
\ <'> play-3-times to *clm-player*

: (ws-play) { ws -- }
	ws :output ws-ref { output }
	ws :player ws-ref { player }
	player word? if
		player #( output ) run-proc drop
	else
		player unless
			"sndplay" to player
		then
		player string? if
			"%s %s" '( player output ) string-format { cmd }
			cmd file-system unless
				    "%s: can't execute %S (exit %d)"
					'( get-func-name cmd
					   exit-status ) fth-warning
			then
		else
			'snd provided? if
				output find-file :wait #t play drop
			else
				"%s: no player found for %s"
				    '( get-func-name output ) fth-warning
			then
		then
	then
;
<'> (ws-play) is ws-play

: ws-output ( ws -- fname )
	:output ws-ref
;

: ws-framples { gen -- len }
	0 { len }
	gen sound? if
		gen #f #f framples to len
	else
		#f { cont }
		gen mus-output? if
			#t to cont
			gen mus-close drop
		then
		gen file-name mus-sound-framples to len
		cont if
			gen file-name continue-sample->file to gen
		then
	then
	len
;

'snd provided? [if]
	: ws-close-snd { fname -- }
		fname 0 find-sound { snd }
		snd sound? if
			snd close-sound drop
		then
	;
[else]
	: ws-close-snd ( fname -- ) drop ;
[then]

: ws-is-output? ( gen -- f )
	*clm-to-snd* if
		sound?
	else
		mus-output?
	then
;
previous

hide
: with-sound-default-args ( keyword-args -- ws )
	#() to *clm-instruments*
	#() to *dac-instruments*
	#w{} { ws }
	*ws-args* ws array-push to *ws-args*
	:channels		*clm-channels*		ws set-args
	:clipped		*clm-clipped*		ws set-args
	:comment		*clm-comment*		ws set-args
	:continue-old-file	#f			ws set-args
	:debug			*clm-debug*		ws set-args
	:decay-time		*clm-decay-time*	ws set-args
	:delete-reverb		*clm-delete-reverb*	ws set-args
	:header-type		*clm-header-type*	ws set-args
	:locsig-type		*clm-locsig-type*	ws set-args
	:notehook		*clm-notehook*		ws set-args
	:output			*clm-file-name*		ws set-args
	:play			*clm-play*		ws set-args
	:player			*clm-player*		ws set-args
	:reverb			*clm-reverb*		ws set-args
	:reverb-channels	*clm-reverb-channels*	ws set-args
	:reverb-data		*clm-reverb-data*	ws set-args
	:reverb-file-name	*clm-reverb-file-name*	ws set-args
	:sample-type		*clm-sample-type*	ws set-args
	:scaled-by		#f			ws set-args
	:scaled-to		#f			ws set-args
	:srate			*clm-srate*		ws set-args
	:statistics		*clm-statistics*	ws set-args
	:to-snd			*clm-to-snd*		ws set-args	
	:to-dac			*clm-to-dac*		ws set-args	
	:verbose		*clm-verbose*		ws set-args
	ws :to-dac ws-ref if
		:output "dac" ws set-args
	else
		ws :output ws-ref "dac" string= if
			:to-dac #t ws set-args
		then
	then
	ws :to-dac ws-ref to *clm-to-dac*
	ws :to-snd ws-ref set-to-snd
	ws
;  

: with-sound-args ( keyword-args -- ws )
	#w{} { ws }
	*ws-args* -1 array-ref { ws1 }
	*ws-args* ws array-push to *ws-args*
	:continue-old-file	#f ws set-args
	:play			#f ws set-args
	:player			#f ws set-args
	:statistics		#f ws set-args
	:channels		ws1 :channels		ws-ref ws set-args
	:comment "with-sound level %d"
	    #( *ws-args* length ) string-format ws set-args
	:debug			ws1 :debug		ws-ref ws set-args
	:decay-time		ws1 :decay-time		ws-ref ws set-args
	:delete-reverb		ws1 :delete-reverb	ws-ref ws set-args
	:header-type		ws1 :header-type	ws-ref ws set-args
	:locsig-type		ws1 :locsig-type	ws-ref ws set-args
	:notehook		ws1 :notehook		ws-ref ws set-args
	:output			ws1 :output		ws-ref ws set-args
	:reverb			ws1 :reverb		ws-ref ws set-args
	:reverb-channels	ws1 :reverb-channels	ws-ref ws set-args
	:reverb-data		ws1 :reverb-data	ws-ref ws set-args
	:reverb-file-name	ws1 :reverb-file-name	ws-ref ws set-args
	:sample-type		ws1 :sample-type	ws-ref ws set-args
	:scaled-by		ws1 :scaled-by		ws-ref ws set-args
	:scaled-to		ws1 :scaled-to		ws-ref ws set-args
	:srate			ws1 :srate		ws-ref ws set-args
	:verbose		ws1 :verbose		ws-ref ws set-args
	ws
;

: ws-is-sound? ( gen -- f )
	*clm-to-snd* if
		sound?
	else
		sample->file?
	then
;

: ws-create-sound { fname chans sr st ht com -- gen }
	*clm-to-snd* if
		save-stack { rest }
		fname chans sr st ht com 1 new-sound { gen }
		rest restore-stack gen
	else
		fname chans st ht com make-sample->file
	then
;

: ws-continue-sound { fname -- gen }
	*clm-to-snd* if
		fname 0 find-sound
	else
		fname continue-sample->file
	then
;

: ws-close-sound { gen -- }
	*clm-to-snd* if
		gen sound? if
			gen save-sound drop
			gen close-sound drop
		then
	else
		gen file-name ws-close-snd
		gen mus-close drop
	then
;

: ws-reset-handler <{ retval -- }>
	stack-reset
	*output* if
		*output* ws-close-sound
	then
	*reverb* if
		*reverb* ws-close-sound
	then
	*ws-args* array-pop drop
	"#<=== WS-ERROR: %s ===>\n" '( retval car exception-name ) clm-print
	*clm-debug* if
		"#<DEBUG: %s>\n" '( retval ) clm-print
	then
	#f #f #f fth-raise
;

: play-cb { len -- prc; self -- val }
	0 proc-create ( prc )
	0 , len ,
  does> { self -- val }
	self @ { samp }
	self cell+ @ { len }
	samp len <= if
		samp run-gen-body ( sum )
		samp 1+ self !
	else
		#f
	then
;

: (with-sound-file-main) ( body-xt ws -- ws )
	2 stack-check
	{ body-xt ws }
	body-xt word? body-xt 1 "a proc or xt" assert-type
	ws      ws?   ws      2 "a ws object"  assert-type
	ws ws-before-output
	ws :reverb ws-ref { reverb-xt }
	reverb-xt if
		reverb-xt word? reverb-xt 3 "a proc or xt" assert-type
		#t
	else
		#f
	then { rev? }
	ws :output		ws-ref { output }
	ws :reverb-file-name	ws-ref { revout }
	ws :continue-old-file	ws-ref { cont? }
	ws :channels		ws-ref { chans }
	ws :reverb-channels	ws-ref { rchans }
	ws :srate		ws-ref { sr }
	ws :sample-type		ws-ref { st }
	ws :header-type		ws-ref { ht }
	ws :comment		ws-ref { com }
	com empty? if
		make-default-comment to com
	then
	cont? if
		output ws-continue-sound
	else
		output file-delete
		output chans sr st ht com ws-create-sound
	then to *output*
	*output* ws-is-sound? unless
		'with-sound-error
		    #( "%s: can't open %s" get-func-name output ) fth-throw
	then
	cont? if
		*clm-to-snd* if
			*output* close-sound drop
		else
			output mus-sound-srate set-mus-srate drop
			output ws-close-snd
		then
	then
	rev? if
		cont? if
			revout ws-continue-sound
		else
			"with-sound temporary reverb file" to com
			revout file-delete
			revout rchans sr st ht com ws-create-sound
		then to *reverb*
		*reverb* ws-is-sound? unless
			'with-sound-error
			    #( "%s: can't open reverb %s"
			       get-func-name revout ) fth-throw
		then
	then
	ws :timer make-timer ws-set! to ws
	\ compute ws body
	body-xt execute
	reverb-xt if
		ws :decay-time ws-ref to *decay-time*
		*reverb* mus-output? if
			*reverb* mus-close drop
		then
		\ compute ws reverb
		\ push reverb arguments on stack
		ws :reverb-data ws-ref each end-each reverb-xt execute
		*reverb* ws-close-sound
	then
	*output* ws-close-sound
	ws :timer ws-ref stop-timer
	'snd provided? if
		ws ws-get-snd drop
	then
	ws :statistics ws-ref if
		ws snd-info
	then
	reverb-xt if
		ws :delete-reverb ws-ref if
			revout file-delete
		then
	then
	ws :scaled-to ws-ref if
		ws ws-scaled-to
	then
	ws :scaled-by ws-ref if
		ws ws-scaled-by
	then
	ws :play ws-ref if
		ws ws-play
	then
	ws ws-after-output ( ws )
;

: (with-sound-dac-main) ( body-xt ws -- ws )
	1 stack-check
	{ ws }
	ws ws? ws 1 "a ws object" assert-type
	0 #f get-optarg { body-xt }
	ws ws-before-output
	ws :timer make-timer ws-set! drop
	body-xt if
		body-xt execute
	then
	*notehook* word? if
		*dac-instruments* each { args }
			*notehook* args run-proc drop
		end-each
	then
	0 { len }
	*dac-instruments* each ( args )
		2 array-ref len max to len
	end-each
	ws :framples len ws-set! drop
	ws :statistics ws-ref if
		ws dac-info
	then
	len play-cb :wait #t play drop
	ws :timer ws-ref stop-timer
	ws :statistics ws-ref if
		ws snd-info
	then
	ws ws-after-output ( ws )
;
set-current

: with-sound-main ( body-xt ws -- ws )
	*clm-to-dac* if
		<'> (with-sound-dac-main)
	else
		<'> (with-sound-file-main)
	then #t <'> ws-reset-handler fth-catch drop ( ws )
;

\ Usage: <'> resflt-test with-sound drop
\        <'> resflt-test :play #f :channels 2 with-sound . cr
\        lambda: resflt-test ; :output "resflt.snd" with-sound drop
: with-sound ( body-xt keyword-args -- ws )
	doc" \\ keywords and default values:\n\
:channels          *clm-channels*         (1)\n\
:clipped           *clm-clipped*          (#f)\n\
:comment           *clm-comment*          (#f)\n\
:continue-old-file                        (#f)\n\
:debug             *clm-debug*            (#f)\n\
:decay-time        *clm-decay-time*       (1.0)\n\
:delete-reverb     *clm-delete-reverb*    (#f)\n\
:header-type       *clm-header-type*      (mus-next)\n\
:locsig-type       *clm-locsig-type*      (mus-interp-linear)\n\
:notehook          *clm-notehook*         (#f)\n\
:output            *clm-file-name*        (\"test.snd\")\n\
:play              *clm-play*             (#f)\n\
:player            *clm-player*           (#f)\n\
:reverb            *clm-reverb*           (#f)\n\
:reverb-channels   *clm-reverb-channels*  (1)\n\
:reverb-data       *clm-reverb-data*      (#())\n\
:reverb-file-name  *clm-reverb-file-name* (\"test.reverb\")\n\
:sample-type       *clm-sample-type*      (mus-lfloat)\n\
:scaled-by                                (#f)\n\
:scaled-to                                (#f)\n\
:srate             *clm-srate*            (44100)\n\
:statistics        *clm-statistics*       (#f)\n\
:to-snd            *clm-to-snd*           (#f)\n\
:to-dac            *clm-to-dac*           (#f)\n\
:verbose           *clm-verbose*          (#f)\n\
Execute BODY-XT, a proc object or an xt, \
and returns a ws-args object with with-sound arguments.\n\
<'> resflt-test with-sound .$ cr\n\
<'> resflt-test :play #t :channels 2 :srate 48000 with-sound drop"
	*ws-args* empty? if
		with-sound-default-args
	else
		with-sound-args
	then ( ws ) with-sound-main ( ws )
;

: clm-load ( fname keyword-args -- ws )
	doc" Load and eval the CLM instrument file FNAME.  \
See with-sound for a full keyword list.\n\
\"test.fsm\" :play #t :player \"sndplay\" clm-load drop"
	*ws-args* empty? if
		with-sound-default-args
	else
		with-sound-args
	then { ws }
	{ fname }
	fname file-exists? if
		ws :verbose ws-ref if
			"loading %s" #( fname ) clm-message
		then
		fname <'> file-eval ws with-sound-main to ws
	else
		'no-such-file
		    #( "%s: %S not found" get-func-name fname ) fth-throw
	then
	ws ws-output ws-close-snd
	ws
;

: with-current-sound <{ body-xt :key offset 0.0 scaled-to #f scaled-by #f -- }>
	doc" Must be called within with-sound body.  \
Takes all arguments from current with-sound except \
:output, :scaled-to, :scaled-by, and :comment."
	*output* mus-output? false? if
		'with-sound-error
		    #( "%s: can only be called within with-sound"
		       get-func-name ) fth-throw
	then
	with-sound-args { ws }
	fth-tempnam { output }
	ws     :output    output    ws-set!
	( ws ) :scaled-to scaled-to ws-set!
	( ws ) :scaled-by scaled-by ws-set! to ws
	body-xt ws with-sound-main drop
	output :output-frame offset seconds->samples clm-mix
	output file-delete
;
previous

: scaled-to <{ body-xt scl -- }>
	doc" Must be called within with-sound body.  \
Scales BODY-XT's resulting sound file to SCL.\n\
lambda: ( -- )\n\
  0.0 0.1 660.0 0.5 fm-violin\n\
  0.5 0.1 550.0 0.1 <'> fm-violin 0.8 scaled-to ( scaled to 0.8 )\n\
; with-sound"
	body-xt :scaled-to scl with-current-sound
;

: scaled-by <{ body-xt scl -- }>
	doc" Must be called within with-sound body.  \
Scales BODY-XT's resulting sound file by SCL.\n\
lambda: ( -- )\n\
  0.0 0.1 660.0 0.5 fm-violin\n\
  0.5 0.1 550.0 0.1 <'> fm-violin 2.0 scaled-by ( scaled to 0.2 )\n\
; with-sound"
	body-xt :scaled-by scl with-current-sound
;

: with-offset <{ body-xt sec -- }>
	doc" Must be called within with-sound body.  \
Mixes BODY-XT's resulting sound file into main sound file at SEC seconds.\n\
lambda: ( -- )\n\
  0.0 0.1 660.0 0.5 fm-violin\n\
  0.5 0.1 550.0 0.1 <'> fm-violin 1.0 with-offset\n\
  ( its actual begin time is 1.5 )\n\
; with-sound"
	body-xt :offset sec with-current-sound
;

: with-mix <{ body-str args fname start -- }>
	doc" BODY-STR is a string with with-sound commands or NIL, \
ARGS is an array of with-sound arguments, \
FNAME is the temporary mix file name without extension, \
and START is the begin time for mix in.  \
If BODY-STR is NIL, a notelist file FNAME.fsm must exist.\n\
lambda: ( -- )\n\
  0.0 0.1 440 0.1 fm-violin\n\
  \"\n\
  0.0 0.1 550 0.1 fm-violin\n\
  0.1 0.1 660 0.1 fm-violin\n\
  \" #() \"sec1\" 0.5 with-mix\n\
  \"\n\
  0.0 0.1  880 0.1 :reverb-amount 0.2 fm-violin\n\
  0.1 0.1 1320 0.1 :reverb-amount 0.2 fm-violin\n\
  \" #( :reverb <'> jc-reverb ) \"sec2\" 1.0 with-mix\n\
  2.0 0.1 220 0.1 fm-violin\n\
; with-sound drop"
	body-str string? body-str nil? || body-str
	    1 "a string or nil" assert-type
	args array? args list? || args
	    2 "an array or a list" assert-type
	fname string? fname 3 "a string" assert-type
	start number? start 4 "a number" assert-type
	*output* ws-is-output? unless
		'with-sound-error
		    #( "%s: can only be called within with-sound"
			get-func-name ) fth-throw
	then
	fname ".snd" $+ { snd-file }
	fname ".fsm" $+ { mix-file }
	fname ".reverb" $+ { rev-file }
	snd-file file-exists? if
		snd-file file-mtime
	else
		0 s>d
	then { snd-time }
	body-str string? if
		mix-file file-exists? if
			mix-file readlines "" array-join
		else
			""
		then ( old-body ) body-str string<> if
			mix-file #( body-str ) writelines
		then
		mix-file file-mtime
	else			\ body-str is nil
		mix-file file-exists? if
			mix-file file-mtime
		else
			'no-such-file
			    #( "%s: %S not found" get-func-name mix-file )
			    fth-throw
		then
	then { mix-time }
	snd-time mix-time d< if
		mix-file args each
			( put all args on stack )
		end-each :output snd-file :reverb-file-name rev-file
		    clm-load drop
	then
	snd-file :output-frame start s>f seconds->samples clm-mix
;

: sound-let ( ws-xt-lst body-xt -- )
	doc" Requires an array of arrays WS-XT-LST with with-sound args \
and xts, and a BODY-XT.  \
The BODY-XT must take WS-XT-LST length arguments which are tempfile names.  \
with-sound gets ws-args und ws-xts from WS-XT-LST.  \
These temporary files will be deleted after execution of BODY-XT.\n\
\\ The WS-XT-LST:\n\
'( '( '( :reverb <'> jc-reverb ) 0.0 1 220 0.2 <'> fm-violin )\n\
   '( '()                        0.5 1 440 0.3 <'> fm-violin )\n\
   '( '()                        '( 10 'a-symbol ) ) )\n\
\\ The BODY-XT:\n\
lambda: <{ tmp1 tmp2 tmp3 -- }>\n\
  tmp1 . cr\n\
  tmp2 . cr\n\
  tmp3 . cr\n\
  tmp1 clm-mix\n\
  tmp2 clm-mix\n\
; <'> sound-let with-sound drop"
	2 stack-check
	{ ws-xt-lst body-xt }
	ws-xt-lst array? ws-xt-lst 1 "an array"     assert-type
	body-xt word?    body-xt   2 "a proc or xt" assert-type
	nil nil { args rest }
	ws-xt-lst map
		*key* car to args
		*key* cdr to rest
		rest -1 list-ref word? if
			\ '( 0.0 1 220 0.2 <'> fm-violin )
			rest each
				( put all args and xt on stack )
			end-each
			\ '( :reverb <'> jc-reverb )
			args ( with-sound args ) each
				( put all ws-args on stack )
			end-each
			:output fth-tempnam with-sound ws-output ( outfile )
		else
			\ a single value; from example above: '( 10 'a-symbol )
			rest car ( val )
		then
	end-map { outfiles }
	body-xt word? if
		body-xt outfiles run-proc drop
	then
	outfiles each { file }
		file file-exists? if
			file ws-close-snd
			file file-delete
		then
	end-each
;

0 [if]
\ CLM examples (see clm.html) and their Snd/Forth counterparts:

\ (with-sound () 
\   (mix (with-sound (:output "hiho.snd") 
\          (fm-violin 0 1 440 .1))
\        :amplitude .5))

lambda: ( -- )
	0.0 1.0 440 0.1 <'> fm-violin
	:output "hiho.snd" with-sound ws-output :scaler 2.0 clm-mix
; with-sound drop 

\ (with-sound ()
\   (with-mix () "s1" 0
\     (sound-let ((tmp () (fm-violin 0 1 440 .1)))
\       (mix tmp))))

lambda: ( -- )
	"
	'( '( '() 0.0 1.0 440 0.1 <'> fm-violin ) )
	lambda: <{ tmp -- }>
		tmp clm-mix
	; sound-let
	" '() "s1" 0 with-mix
; with-sound drop 

\ (with-sound (:verbose t)
\   (with-mix () "s6" 0
\     (sound-let ((tmp () (fm-violin 0 1 440 .1))
\                 (tmp1 (:reverb nrev) (mix "oboe.snd")))
\       (mix tmp1)
\       (mix tmp :amplitude .2 :output-frame *srate*))
\     (fm-violin .5 .1 330 .1)))

lambda: ( -- )
	"
	'( '( '() 0.0 1.0 440 0.1 <'> fm-violin )
	   '( '( :reverb <'> nrev ) \"oboe.snd\" <'> clm-mix ) )
	lambda: <{ tmp tmp1 -- }>
		tmp1 clm-mix
		tmp :scaler 5.0 :output-frame 1.0 seconds->samples clm-mix
	; sound-let
	0.5 0.1 330 0.1 fm-violin
	" '() "s6" 0 with-mix
; :verbose #t with-sound drop 

\ (with-sound (:verbose t)
\   (sound-let ((tmp () (with-mix () "s7" 0
\                    (sound-let ((tmp () (fm-violin 0 1 440 .1))
\                                (tmp1 () (mix "oboe.snd")))
\                      (mix tmp1)
\                      (mix tmp :output-frame *srate*))
\                    (fm-violin .5 .1 330 .1))))
\      (mix tmp :amplitude .5)))

'( '( '() "
	'( '( '() 0.0 1.0 440 0.1 <'> fm-violin )
	   '( '()      \"oboe.snd\" <'> clm-mix ) )
	lambda: <{ tmp tmp1 -- }>
		tmp1 clm-mix
		tmp :output-frame 1.0 seconds->samples clm-mix
	; sound-let
	0.5 0.1 330 0.1 fm-violin
	" '() "s7" 0 <'> with-mix ) )
lambda: <{ tmp -- }>
	tmp :scaler 2.0 clm-mix
; <'> sound-let :verbose #t with-sound drop 
[then]

\ === Example instruments, more in clm-ins.fs ===
instrument: simp { start dur freq amp -- }
	:frequency freq make-oscil { os }
	:envelope #( 0 0 25 1 75 1 100 0 )
	    :duration dur :scaler amp make-env { en }
	\ start dur run
	\	i  os 0.0 0.0 oscil en env f*  *output* outa drop
	\ loop	
	start dur nil run-instrument
		os 0.0 0.0 oscil  en env  f*
	end-run
;instrument

: run-test ( -- )	0.0 1.0 330.0 0.5 simp ;

: input-fn { gen -- prc; dir self -- r }
	1 proc-create ( prc )
	gen ,
  does> { dir self -- r }
	self @ ( gen ) readin
;

instrument: src-simp { start dur amp sr sr-env fname -- }
	:file fname find-file make-readin { f }
	:input f input-fn :srate sr make-src { sc }
	:envelope sr-env :duration dur make-env { en }
	\ start dur run
	\	i  sc en env #f src amp f*  *output* outa drop
	\ loop
	start dur nil run-instrument
		sc  en env  #f src  amp f*
	end-run
	f mus-close drop
;instrument

instrument: conv-simp { start dur filt fname amp -- }
	:file fname find-file make-readin { f }
	filt string? if
		8192 0.0 make-vct { v }
		filt find-file 0 0 v length v file->array
	else
		filt
	then { data }
	:input f input-fn :filter data make-convolve { cv }
	\ start dur run
	\	i cv #f convolve  amp f*  *output* outa drop
	\ loop
	start dur nil run-instrument
		cv #f convolve  amp f*
	end-run
	f mus-close drop
;instrument

\ <'> src-test with-sound drop
event: src-test ( -- )
	0.0 1.0 1.0 0.2 #( 0 0 50 1 100 0 ) "oboe.snd" src-simp
;event

\ <'> conv1-test with-sound drop
event: conv1-test ( -- )
	0.0 1.0 vct( 0.5 0.2 0.1 0.05 0 0 0 0 ) "fyow.snd" 1.0 conv-simp
;event

\ <'> conv2-test with-sound drop
event: conv2-test ( -- )
	0.0 1.0 "pistol.snd" "fyow.snd" 0.2 conv-simp
;event

\ <'> inst-test with-sound drop
event: inst-test ( -- )
	0.0 1.0 1.0 0.2 #( 0 0 50 1 100 0 ) "oboe.snd" src-simp
	1.2 1.0 vct( 0.5 0.2 0.1 0.05 0 0 0 0 ) "fyow.snd" 1.0 conv-simp
	2.4 1.0 "pistol.snd" "fyow.snd" 0.2 conv-simp
;event

'snd provided? [if]
	instrument: snd-arpeggio
	  <{ start dur freq amp :key ampenv #( 0 0 0.5 1 1 0 ) offset 1.0 -- }>
		start dur times->samples { end beg }
		12 make-array map!
			:frequency i 6 - 0.03 f* offset f* freq f+
			    :partials #( 1 1.0
					 5 0.7
					 6 0.7
					 7 0.7
					 8 0.7
					 9 0.7
					10 0.7 ) make-polyshape
		end-map { waveshbank }
		:envelope ampenv
		    :scaler amp 0.1 f*
		    :length end make-env { amp-env }
		end 0.0 make-vct map!
			0.0 ( sum )
			waveshbank each ( wv )
				1.0 0.0 polyshape f+ ( sum += ... )
			end-each ( sum ) amp-env env f*
		end-map ( vct-output )
		#f channels 0 ?do
			( vct-output ) beg end #f i #f undef vct->channel
		loop ( vct-output ) drop
	;instrument

	event: snd-arpeggio-test ( -- snd )
		mus-srate { old-sr }
		48000 set-mus-srate drop
		:file "arpeggio.snd"
		    :header-type mus-next
		    :sample-type mus-bdouble
		    :channels 2
		    :srate mus-srate f>s
		    :comment make-default-comment new-sound { snd }
		0 10 65 0.5 snd-arpeggio
		snd save-sound drop
		old-sr set-mus-srate drop
		snd
	;event
[then]

instrument: arpeggio <{ start dur freq amp :key
    ampenv #( 0 0 0.5 1 1 0 )
    offset 1.0 -- }>
	start dur times->samples { end beg }
	12 make-array map!
		:frequency i 6 - 0.03 f* offset f* freq f+
		    :partials #( 1 1.0
				 5 0.7
				 6 0.7
				 7 0.7
				 8 0.7
				 9 0.7
				10 0.7 ) make-polyshape
	end-map { waveshbank }
	:envelope ampenv
	    :scaler amp 0.1 f*
	    :length end make-env { amp-env }
	start dur #{ :degree 90.0 random } run-instrument
		0.0 ( sum )
		waveshbank each ( wv )
			1.0 0.0 polyshape f+ ( sum += ... )
		end-each ( sum ) amp-env env f*
	end-run
;instrument

\ <'> arpeggio-test :output "arpeggio.snd" with-sound
event: arpeggio-test ( -- )
	0 10 65 0.5 arpeggio
;event

instrument: simp-gen { start dur freq amp -- ; samp args -- val }
	doc" simple example for an instrument generator:\n\
<'> test-gen :channels 1 :srate 22050 :to-dac #t with-sound drop\n\
or\n\
test-gen run-gen map-channel drop"
	:frequency freq make-oscil { os }
	:envelope #( 0 0 25 1 75 1 100 0 )
	:duration dur :scaler amp make-env { en }
	start dur nil run-gen-instrument { samp args -- val }
		args "os" args@ 0.0 0.0 oscil  args "en" args@ env f*
	end-run-gen
;instrument

\ <'> test-gen :channels 1 :srate 22050 :to-dac #t with-sound drop
\ or
\ test-gen run-gen map-channel drop
: test-gen ( -- )
	0.0 0.1  440 0.2 simp-gen
	0.5 0.2  550 0.2 simp-gen
	0.6 0.1  660 0.2 simp-gen
	1.0 0.1  880 0.2 simp-gen
	1.1 0.1 1320 0.2 simp-gen
	2.0 0.1  220 0.2 simp-gen
;

\ snd/fm.html
\ see clm-ins.fs for file version
instrument: violin-gen <{ start dur freq amp :key
    fm-index 1.0
    amp-env #( 0 0 25 1 75 1 100 0 )
    index-env #( 0 1 25 0.4 75 0.6 100 0 )
    degree #f
    distance #f
    reverb-amount #f -- }>
	doc" Violin example from snd/fm.html as generator:\n\
<'> violin-gen-test :channels 1 :srate 11025 :to-dac #t with-sound drop\n\
or\n\
violin-gen-test run-gen map-channel drop"
	freq hz->radians { frq-scl }
	frq-scl fm-index f* { maxdev }
	5.0 freq flog f/ maxdev f* { index1 }
	8.5 freq flog f- 3.0 freq 1000.0 f/ f+ f/ maxdev 3.0 f* f* { index2 }
	4.0 freq fsqrt f/ maxdev f* { index3 }
	:frequency freq make-oscil { carrier }
	:frequency freq make-oscil { fmosc1 }
	:frequency freq 3.0 f* make-oscil { fmosc2 }
	:frequency freq 4.0 f* make-oscil { fmosc3 }
	:envelope amp-env :scaler amp :duration dur make-env { ampf }
	:envelope index-env :scaler index1 :duration dur make-env { indf1 }
	:envelope index-env :scaler index2 :duration dur make-env { indf2 }
	:envelope index-env :scaler index3 :duration dur make-env { indf3 }
	:frequency 5.0
	    :amplitude 0.0025 frq-scl f* make-triangle-wave { pervib }
	:frequency 16.0
	    :amplitude 0.005 frq-scl f* make-rand-interp   { ranvib }
	start dur nil run-gen-instrument { samp args -- val }
		args "pervib" args@ 0.0 triangle-wave
		args "ranvib" args@ 0.0 rand-interp f+ { vib }
		args "carrier" args@
		    vib
		    args "fmosc1" args@ vib 0.0 oscil
		    args "indf1" args@ env f* f+
		    args "fmosc2" args@ 3.0 vib f* 0.0 oscil
		    args "indf2" args@ env f* f+
		    args "fmosc3" args@ 4.0 vib f* 0.0 oscil
		    args "indf3" args@ env f* f+
		    0.0 oscil
		args "ampf" args@ env f*
	end-run-gen
;instrument

\ <'> violin-gen-test :channels 1 :srate 11025 :to-dac #t with-sound drop
\ or
\ violin-gen-test run-gen map-channel drop
: violin-gen-test <{ :optional start 0.0 dur 1.0 -- }>
	start now!
	now@ dur |Bf4 0.5 violin-gen dur f2/ step
	now@ dur |A4  0.5 violin-gen dur f2/ step
	now@ dur |C5  0.5 violin-gen dur f2/ step
	now@ dur |B4  0.5 violin-gen dur f2/ step
	0.2 step
;

: violin-dac-test ( -- )
	<'> violin-gen-test :channels 1 :srate 11025 :to-dac #t with-sound drop
;

'snd provided? [if]
	: violin-map-test ( -- )
		\ fill *dac-instruments* with #( prc beg end ) elements
		violin-gen-test
		0 { size }
		*dac-instruments* each { el }
			el 2 array-ref size max to size
		end-each
		get-func-name ".snd" $+ :channels 1 :size size new-sound { snd }
		run-gen map-channel drop
		snd play drop
	;
[then]

\ generators.scm
: make-waveshape <{ :optional
    freq *clm-default-frequency*
    parts #( 1 1 )
    wave #f
    size *clm-table-size* -- gen }>
	doc" See make-polyshape."
	:frequency freq
	    wave if
		    :coeffs wave
	    else
		    :partials parts
	    then make-polyshape
;

<'> polyshape  alias waveshape  ( gen :optional index 1.0 fm 0.0 -- val )
<'> polyshape? alias waveshape? ( obj -- f )
<'> waveshape  <'> polyshape  help-ref  help-set!
<'> waveshape? <'> polyshape? help-ref  help-set!

: partials->waveshape <{ partials :optional size *clm-table-size* -- wave }>
	doc" See partials->polynomial."
	partials partials->polynomial ( wave )
;

\ snd10.scm
: make-sum-of-sines <{ :key
    sines 1
    frequency 0.0
    initial-phase 0.0 -- gen }>
	doc" See make-nsin."
	:frequency frequency :n sines make-nsin { gen }
	gen initial-phase set-mus-phase drop
	gen
;

<'> nsin  alias sum-of-sines  ( gen :optional fm 0.0 -- val )
<'> nsin? alias sum-of-sines? ( obj -- f )
<'> sum-of-sines  <'> nsin  help-ref  help-set!
<'> sum-of-sines? <'> nsin? help-ref  help-set!

: make-sum-of-cosines <{ :key
    cosines 1
    frequency 0.0
    initial-phase 0.0 -- gn }>
	doc" See make-ncos."
	:frequency frequency :n cosines make-ncos { gen }
	gen initial-phase set-mus-phase drop
	gen
;

<'> ncos  alias sum-of-cosines  ( gen :optional fm 0.0 -- val )
<'> ncos? alias sum-of-cosines? ( obj -- f )
<'> sum-of-cosines  <'> ncos  help-ref  help-set!
<'> sum-of-cosines? <'> ncos? help-ref  help-set!

: make-sine-summation <{ :key
    frequency 0.0
    initial-phase 0.0
    n 1
    a 0.5
    ratio 1.0 -- gen }>
	doc" See make-nrxysin."
	:frequency frequency :ratio ratio :n n :r a make-nrxysin { gen }
	gen initial-phase set-mus-phase drop
	gen
;

<'> nrxysin  alias sine-summation  ( gen :optional fm 0.0 -- val )
<'> nrxysin? alias sine-summation? ( obj -- f )
<'> sine-summation  <'> nrxysin  help-ref  help-set!
<'> sine-summation? <'> nrxysin? help-ref  help-set!

\ clm.fs ends here
