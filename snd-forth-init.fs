\ .snd_forth -- start up file for Snd/Forth
\
\ @(#)snd-forth-init.fs	1.44 9/25/17
\

\ You can install the *.fs scripts with:
\ 
\   cd ${top_srcdir}/examples/site-lib
\   ./install.fth
\ 
\ or even better
\
\   cd ${top_builddir}
\   make install
\
\ If you have installed *.fs scripts with one of the above mentioned
\ commands, you don't need to add a path to *load-path*.
\ ${prefix}/share/fth/site-fth is already included.  Otherwise you can
\ add a path with e.g.:
\ 
\   "/home/mike/snd" add-load-path

\ A special *SND-HOME* path points to ~/.snd.d:
\ 
\ ~/.snd.d/sound      directory for *clm-file-name*
\ ~/.snd.d/zap        directory for set-temp-dir
\                                   set-save-dir
\ ~/.snd.d/peaks      directory for set-peak-env-dir
\
\ "HOME" getenv       constant *home*
\ *home* "/.snd.d" $+ constant *snd-home*
\
\ Change these paths!
\

#t to *fth-verbose*
#f to *fth-debug*

\ Redirect Fth output (stdout) to the Snd listener (with snd-print).
:port-name "sndout" :write-line <'> snd-print make-soft-port set-*stdout* drop
\ Now force output to listener:
#t set-show-listener drop

before-load-hook lambda: <{ fname -- f }>
	*fth-verbose* if
		"\\ loading %s\n" #( fname ) fth-print
	then
	#t
; add-hook!

before-load-hook '( *filename* ) run-hook drop

"HOME" getenv		constant *home*
*home* "/.snd.d" $+	constant *snd-home*
hostname		constant *hostname*
*hostname* "." string-split car constant *short-hostname*
*argv* length 0> [if]
	*argv* car undef file-basename
[else]
	"snd"
[then] constant *program-name*

\ if configured --with-shared-sndlib
'sndlib provided? [unless] dl-load sndlib Init_sndlib [then]

\ Set them before loading clm.fs.
2		set-default-output-chans drop
48000		set-default-output-srate drop
mus-bdouble	set-default-output-sample-type drop
512		set-dac-size drop
mus-clipping	set-clipping drop
1024 1024 *	set-mus-file-buffer-size drop
48		set-mus-array-print-length drop
mus-array-print-length set-print-length drop
128		set-object-print-length

require clm
require clm-ins

\ Environment variable CLM_SEARCH_PATH
\ Path variable where sound files reside.
\ csh: setenv CLM_SEARCH_PATH /usr/gnu/sound/SFiles:${HOME}/.snd.d/sound
\  sh: export CLM_SEARCH_PATH=/usr/gnu/sound/SFiles:${HOME}/.snd.d/sound

"CLM_SEARCH_PATH" getenv dup [if]
	":" string-split [each]
		*clm-search-list* swap array-push to *clm-search-list*
	[end-each]
[else]
	drop
	*clm-search-list* *snd-home* "/sound" $+ array-push to *clm-search-list*
[then]

: clm-print-instrument <{ ins beg dur -- }>
	"%14s: %5.2f %5.2f" '( ins beg dur ) clm-message
;

: clm-sox-player <{ output -- }>
	"sox -qV1 %s -d" #( output ) string-format file-system unless
		"exit %d\n" #( exit-status ) fth-print
	then
;

#t to *clm-play*
#t to *clm-statistics*
#t to *clm-verbose*
#f to *clm-debug*
*snd-home* "/sound/fth-test.snd" $+ to *clm-file-name*
*snd-home* "/sound/fth-test.reverb" $+ to *clm-reverb-file-name*
#t to *clm-delete-reverb*
<'> clm-print-instrument to *clm-notehook*
<'> clm-sox-player to *clm-player*

*snd-home* add-load-path
"BROWSER" getenv "firefox" || set-html-program drop
*snd-home* "/sound" $+	set-open-file-dialog-directory ( dir )
"/saved-snd.fs" $+	set-save-state-file drop
*snd-home* "/zap" $+	set-save-dir ( dir )
			set-temp-dir drop
0.0			set-auto-update-interval drop
#t			set-trap-segfault drop

#( "rev" "reverb" "wave" ) [each] ( ext )
	add-sound-file-extension drop
[end-each]

\ make-default-comment from clm.fs
output-comment-hook lambda: <{ str -- s }>
	str empty? if
		make-default-comment
	else
		str
	then
; add-hook!

require examp
[ifundef] read-eval-loop-prompt 
	"ok " value read-eval-loop-prompt 
[then]

'snd-nogui provided? [if]
	\ snd-nogui repl and prompt hooks
	before-repl-hook reset-hook!	\ remove default hook
	before-repl-hook lambda: <{ -- }>
		"" #f clm-message
		"Starting session on %s."
		    #( "%a %b %d %r %Z %Y" current-time strftime ) clm-message
		"" #f clm-message
	; add-hook!

	\
	\ Remove duplicates from history file.
	\
	after-repl-hook lambda: <{ history -- }>
		history readlines array-reverse! { hary }
		#() "" "" { nhary hline tline }
		hary array-length 0 ?do
			hary i    array-ref to hline
			hary i 1+ array-ref to tline
			nhary hline array-member? unless
				nhary hline array-unshift
				    ( nhary ) tline array-unshift drop
			then
		2 +loop
		history nhary writelines
		\ Be polite.
		"" #f clm-message
		"Thank you for using %s!"
		    #( *program-name* string-upcase ) clm-message
		"" #f clm-message
		1 sleep
	; add-hook!

	\
	\ A more elaborated prompt for fth and snd-forth-nogui.
	\
	before-prompt-hook lambda: <{ prompt pos -- new-prompt }>
		"%I:%M%p" current-time strftime string-downcase! { tm }
		"%%S[%s %s] (%d)%%s %%Bok%%b "
		    #( *short-hostname* tm pos ) string-format
	; add-hook!
[else]				\ snd-motif|gtk
	read-hook lambda: <{ text -- flag }>
		\ Prints "\n" to put output at next line.
		\ This separates better input from output.
		cr
		#f
	; add-hook!

	require snd-xm
	after-open-hook <'> show-disk-space add-hook!

	require effects
	#f to use-combo-box-for-fft-size	\ boolean (default #f)

	'snd-motif provided? [if]
		*clm-search-list* [each] ( dir )
			undef add-directory-to-view-files-list drop
		[end-each]
		\ snd-xm.fs
		add-mark-pane
		require popup
		edhist-save-hook lambda: <{ prc -- }>
			"%S" #( prc ) clm-message
		; add-hook!
	[then]

	require extensions
	with-reopen-menu
	with-buffers-menu

	\ examp.fs
	graph-hook <'> auto-dot add-hook!
	graph-hook <'> zoom-spectrum add-hook!
	lisp-graph-hook <'> display-energy add-hook!
	after-transform-hook <'> fft-peak add-hook!
	\ graph-hook <'> display-correlate add-hook!
	\ graph-hook <'> superimpose-ffts add-hook!
	\ lisp-graph-hook <'> display-db add-hook!

	require mix
	mix-click-hook <'> mix-click-sets-amp add-hook!
	mix-click-hook <'> mix-click-info add-hook!

	require marks
	save-mark-properties
	mark-click-hook <'> mark-click-info add-hook!

	require dsp
	require env
	enved-hook lambda: <{ en pt x y reason -- en'|#f }>
		reason enved-move-point = if
			x en 0 array-ref f>
			x en -2 array-ref f< && if
				en en pt 2* array-ref
				    x #f #f stretch-envelope ( new-en )
				    dup pt 2* 1+ y array-set!
				    ( new-en )
			else
				#f
			then
		else
			#f
		then
	; add-hook!

	\ xm-enved.fs (already loaded by effects.fs)
	before-enved-hook lambda: <{ gen pos x y reason -- f }>
 		enved-hook hook-empty? if
 			#f
 		else
			gen xenved-envelope@ { res }
			enved-hook each { prc }
				prc #( res pos x y reason ) run-proc to res
				res false? ?leave
			end-each
			res array? if
				gen res xenved-envelope!
			then
			res
 		then
	; add-hook!

	after-open-hook lambda: <{ snd -- }>
		snd channels 0 ?do
			snd short-file-name
			    snd i ( chn ) time-graph set-x-axis-label drop
			\ to force a verbose cursor
			0 snd i ( chn ) #f set-cursor drop 
		loop
		cursor-line snd #t set-cursor-style drop
		channels-combined snd set-channel-style
	; add-hook!

	require rgb
	blue			set-selected-data-color drop
	beige			set-selected-graph-color drop

	rainbow-colormap	set-colormap drop
	#t			set-enved-wave? drop
	#t			set-just-sounds drop
	\ defined in examp.fs
	read-eval-loop-prompt	set-listener-prompt drop
	#t			set-show-full-duration drop
	#t			set-show-indices drop
	#t			set-show-transform-peaks drop
	#t			set-show-y-zero drop
	speed-control-as-ratio	set-speed-control-style drop
	\ graph-once 
	\ graph-as-sonogram 
	\ graph-as-spectrogram
	graph-once		set-transform-graph-type drop
	#t			set-with-inset-graph drop
	#t			set-with-pointer-focus drop
	#t			set-with-smpte-label drop
	#t			set-with-toolbar drop
	#t			set-with-tracking-cursor drop
	#t			set-with-verbose-cursor drop
	1200			set-window-width drop
	150			set-window-x drop
	0			set-window-y drop
	\ The listener appears in a more convenient size with this trick:
	800			set-window-height drop
	1000			set-window-height drop

	\ bind-key ( key modifiers func
	\            :optional extended=#f origin="" prefs-info="" -- val )
	\ 
	\ modifiers:
	\   0 normal
	\   1 shift
	\   4 control
	\   8 meta
	\ 
	\ extended (prefix key):
	\   #t  C-x
	\   #f  none
	\
	\ func ( -- val )
	\
	\ val should be:
	\   cursor-in-view
	\   cursor-on-left
	\   cursor-on-right
	\   cursor-in-middle
	\   keyboard-no-action
	\ 
	\ C-x C-c terminate Snd
	<char> c 4 lambda: <{ -- val }>
		0 snd-exit drop
		cursor-in-view
	; #t "terminate Snd" "terminate-snd" bind-key drop

	\ C-x k close selected sound
	<char> k 0 lambda: <{ -- val }>
		selected-sound close-sound-extend
		cursor-in-view
	; #t "close sound and jump to next open"
	    "close-current-sound" bind-key drop

	\ C-x C-k toggle listener
	<char> k 4 lambda: <{ -- val }>
		show-listener not set-show-listener drop
		cursor-in-view
	; #t "show listener" "show-listener" bind-key drop
	    
	\ C-x C-x play
	<char> x 4 lambda: <{ -- val }>
		#t play drop
		cursor-in-view
	; #t "play current sound" "play-current-sound" bind-key drop
	    
	\ C-x C-t play from cursor
	<char> t 4 lambda: <{ -- val }>
		selected-sound :start undef undef undef cursor play drop
		cursor-in-view
	; #t "play from cursor" "play-from-cursor" bind-key drop
	
	"End" 0 lambda: <{ -- val }>
		selected-sound { snd }
		snd #f #f framples { frms }
		snd srate { sr }
		'( 0.0 frms sr f/ ) snd #f undef set-x-bounds ( val )
	; #f "view full sound" undef bind-key drop
	
	<char> m 0 <'> first-mark-in-window-at-left #f
	    "align window left edge with mark"
	    "first-mark-in-window-at-left" bind-key drop
[then]				\ snd-nogui

\ find-file searchs in *clm-search-list*
let:
	sounds empty? if
		*clm-file-name* find-file { fname }
		fname if
			fname open-sound drop
		then
		cr
	then
;let

"%s (Fth %s)" #( snd-version fth-version ) clm-message

\ Finally, after loading files with possible error messages, redirect
\ Fth error output (stderr) to the Snd listener too.
*stdout* set-*stderr* drop

\ .snd_forth ends here
