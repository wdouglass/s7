\ marks.fs -- marks.scm|rb -> marks.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/12/27 19:22:06
\ Changed: 17/12/02 07:56:15
\
\ @(#)marks.fs	1.28 12/2/17

\ Commentary:
\
\ add-named-mark	( samp name snd chn -- mark )
\ mark-name->id		( name -- m )
\ move-syncd-marks	( sync diff -- )
\ describe-mark		( id -- ary )
\
\ save-mark-properties	( -- )
\ mark-click-info	( id -- #t )
\ marks->string		( snd -- str )

\ Code:

require clm
require examp

<'> integer? alias channel?

\ snd #f: all sounds
\ chn #f: all channels
: marks-length <{ :optional snd #f chn #f edpos #f -- len }>
	snd sound? if
		chn channel? if
			snd chn edpos marks
		else
			snd chn edpos marks car
		then
	else
		snd chn edpos marks car car
	then length
;

: marks? ( :optional snd chn edpos -- f )
	marks-length 0>
;

\ from rubber.fs
: add-named-mark { samp name snd chn -- mark }
	samp snd chn add-mark { m }
	m name set-mark-name drop
	m
;

\ mark-name->id is a global version of find-mark
: mark-name->id ( name -- m )
	doc" Like find-mark but searche all currently accessible channels."
	{ name }
	#f			\ flag
	sounds each { snd }
		snd channels 0 ?do
			name snd i undef find-mark { m }
			m mark? if
				drop	\ replace #f with mark
				m
				exit
			then
		loop
	end-each
;

: move-syncd-marks ( sync diff -- )
	doc" Move all marks sharing SYNC by DIFF samples."
	{ syn diff }
	syn syncd-marks each { m }
		m undef mark-sample diff + { val }
		m val set-mark-sample drop
	end-each
;

: describe-mark ( id -- ary )
	doc" Return a description of the movements of mark ID over \
the channel's edit history."
	{ id }
	id <'> mark-home 'no-such-mark nil fth-catch if
		sounds each { snd }
			snd channels 0 ?do
				0 snd i ( chn ) edits each
					+
				end-each 1+ ( max-edits ) 0 ?do
					snd j ( chn ) #f marks { m }
					m
					id m array-member? && if
						#( snd j ( chn ) )
						leave
					then
				loop
			loop
		end-each
	then { mark-setting }
	mark-setting array? if
		mark-setting 0 array-ref { snd }
		mark-setting 1 array-ref { chn }
		#( #( 'mark id
		      'sound snd snd short-file-name
		      'channel chn ) ) { descr }
		0 snd chn edits each
			+
		end-each 1+ 1 ?do
			descr
			snd chn i marks id array-member? if
				id i mark-sample
			else
				#f
			then array-push drop
		loop
		descr
	else
		'no-such-mark #( "%s: %s" get-func-name id ) fth-throw
	then
;

\ --- Mark Properties ---
hide
: mark-writeit { mp mhome msamp io -- }
	io "\t%S 0 find-sound to snd\n" #( mhome car file-name ) io-write-format
	io "\tsnd sound? if\n" io-write
	io "\t\t%d snd %d find-mark to mk\n"
	    #( msamp mhome cadr ) io-write-format
	io "\t\tmk mark? if\n" io-write
	io "\t\t\tmk %S set-mark-properties drop\n" #( mp ) io-write-format
	io "\t\tthen\n" io-write
	io "\tthen\n" io-write
;

: save-mark-properties-cb <{ filename -- }>
	undef undef undef marks car car cons? if
		filename :fam a/o io-open { io }
		io "\n\\ from save-mark-properties in %s\n"
		    #( *filename* ) io-write-format
		io "require marks\n\n" io-write
		io "let: ( -- )\n" io-write
		io "\tnil nil { snd mk }\n" io-write
		undef undef undef marks each ( snd-m )
			each ( chn-m )
				each { m }
					m mark-properties { mp }
					mp if
						m mark-home { mhome }
						m undef mark-sample { msamp }
						mp mhome msamp io mark-writeit
					then
				end-each
			end-each
		end-each
		io ";let\n" io-write
		io io-close
	then
;
set-current

: save-mark-properties ( -- )
	doc" Set up an after-save-state-hook \
function to save any mark-properties."
	after-save-state-hook <'> save-mark-properties-cb add-hook!
;
previous

: mark-click-info <{ id -- #t }>
	doc" A mark-click-hook function that describes a \
mark and its properties.\n\
mark-click-hook <'> mark-click-info add-hook!."
	id mark-name { name }
	id undef mark-sample { samp }
	id mark-home car { snd }
	id mark-sync { syn }
	id mark-properties { props }
	"\n   mark id: %s\n\n" #( id ) string-format
	    make-string-output-port { prt }
	name empty? unless
		prt "      name: %s\n" #( name ) port-puts-format
	then
	prt "    sample: %s (%.3f secs)\n"
	    #( samp samp snd srate f/ ) port-puts-format
	syn if
		prt "      sync: %s\n" #( syn ) port-puts-format
	then
	props empty? unless
		prt "properties: %s" #( props ) port-puts-format
	then
	"Mark Info" prt port->string info-dialog drop
	#t
;
\ mark-click-hook <'> mark-click-info add-hook!

\ This code saves mark info in the sound file header, and reads it
\ back in when the sound is later reopened.
: marks->string { snd -- str }
	"\nrequire marks\n" make-string-output-port { prt }
	prt "let: ( -- )\n" port-puts
	prt "\t#f { mk }\n" port-puts
	snd marks each { chan-marks }
		prt "\n\t\\ channel %d\n" #( i ) port-puts-format
		chan-marks each { m }
			m nil? ?leave
			prt "\t%s #f %d %S %d add-mark to mk\n"
			    #( m undef mark-sample
			       j ( chn )
			       m mark-name length 0= if
				       #f
			       else
				       m mark-name
			       then
			       m mark-sync ) port-puts-format
			m mark-properties { props }
			props if
				prt "\tmk %S set-mark-properties drop\n"
				    #( props ) port-puts-format
			then
		end-each
	end-each
	prt ";let\n" port-puts
	prt port->string
;

0 [if]
	output-comment-hook lambda: <{ str -- str' }>
		selected-sound marks->string
	; add-hook!

	after-open-hook lambda: <{ snd -- }>
		snd comment ( str ) <'> string-eval #t nil fth-catch if
			( str ) drop
		then
	; add-hook!
[then]

\ marks.fs ends here
