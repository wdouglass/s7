\ popup.fs -- popup.scm|rb --> popup.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/12/23 00:28:28
\ Changed: 17/12/16 05:22:17
\
\ @(#)popup.fs	1.50 12/16/17

\ selection-popup-menu
\ graph-popup-menu
\ fft-popup-menu
\ edit-history-menu
\ listener-popup-menu
\
\ add-popups			( -- )
\
\ edhist-help-edits		( w c i -- )
\ change-menu-color		( menu new-color -- )
\ change-selection-popup-color	( new-color -- )
\ change-graph-popup-color	( new-color -- )
\ change-fft-popup-color	( new-color -- )
\ change-edhist-popup-color	( new-color -- )
\ change-listener-popup-color	( new-color -- )

'snd-motif provided? [unless] skip-file [then]

require snd-xm
require extensions

\ for prefs
: edhist-help-edits <{ w c info  -- }>
	"Edit History Functions"
	    "This popup menu gives access to the edit-list \
function handlers in Snd.  \
At any time you can backup in the edit list, \
'save' the current trailing edits, make some \
new set of edits, then 'reapply' the saved edits.  \
The 'apply' choice gives access to all \
currently saved edit lists -- any such list can be applied to any channel.  \
'Clear' deletes all saved edit lists."
	    #( "{edit lists}" "{edit-list->function}" )
	    #( "extsnd.html#editlists" "extsnd.html#editlist_to_function" )
	    help-dialog drop
;

1 "PROC is the only argument." create-hook edhist-save-hook

hide
#() value cascade-popup-cb-list

: widget->name ( w -- s )
	FXtName
;

: popup-post-it   { menu info -- }
	info menu Fset_menuToPost drop
;

\ --- make-simple-popdown-menu for fft-popup-menu ---
: popup-cascade-cb { children cb -- prc; w c i self -- }
	3 proc-create ( prc )
	cb , children ,
  does> { w c info self -- }
	self       @ { cb }
	self cell+ @ { children }
	cb #( children ) run-proc drop
;

: make-simple-popdown-menu { label popdown-labels parent cascade-cb -- }
	parent label #( FXmNbackground highlight-color )
	    undef FXmCreatePulldownMenu { top }
	label FxmCascadeButtonWidgetClass parent
	    #( FXmNbackground highlight-color FXmNsubMenuId top )
	    undef FXtCreateManagedWidget { menu }
	#() { children }
	nil { c }
	popdown-labels proc? if
		\ edhist sends a proc to set TOP to edhist-widgets
		popdown-labels #( top ) run-proc drop
	else
		\ else arrays of #( name proc ) lists
		popdown-labels each { poplab }
			poplab 0 array-ref FxmPushButtonWidgetClass top
			    #( FXmNbackground highlight-color )
			    undef FXtCreateManagedWidget to c
			c FXmNactivateCallback poplab 1 array-ref
			    undef FXtAddCallback drop
			children c array-push drop
		end-each
	then
	cascade-cb if
		menu FXmNcascadingCallback
		    children cascade-cb popup-cascade-cb
		    undef FXtAddCallback drop
	then
;

\ --- make-popdown-entry for listener-popup-menu ---
#() value listener-values

: collector-cb { func collector -- prc; w c i self -- val }
	3 proc-create ( prc )
	func , collector ,
  does> { w c info self -- val }
	self       @ { func }
	self cell+ @ { collector }
	collector #( sounds ) run-proc ( lst )
	    0 array-ref 1 >array func swap run-proc
;

: cas-cb ( func -- prc; w c i self -- val )
	{ func }
	3 proc-create ( prc )
	func ,
  does> { w c info self -- val }
	self @ ( func ) #( w current-label 0 find-sound ) run-proc
;

: popdown-cascade-cb { func coll menu children -- prc; w c i self -- }
	3 proc-create ( prc )
	func , coll , menu , children ,
  does> { w c info self -- }
	self           @ { func }
	self   cell+   @ { collector }
	self 2 cells + @ { menu }
	self 3 cells + @ { children }
	children each ( child )
		FXtUnmanageChild drop
	end-each
	collector #( sounds ) run-proc { snds }
	children length { clen }
	snds length { slen }
	clen slen < if
		slen clen ?do
			"" FxmPushButtonWidgetClass menu
			    #( FXmNbackground highlight-color )
			    undef FXtCreateManagedWidget { c }
			c FXmNactivateCallback
			    func cas-cb
			    undef FXtAddCallback drop
			children c array-push drop
		loop
	then
	slen if
		children each { child }
			snds i array-ref { snd }
			child snd short-file-name change-label
			child FXtManageChild drop
		end-each
	then
;

: make-popdown-entry { label parent func collector with-one -- }
	#f { widget }
	#() { children }
	with-one if
		label FxmPushButtonWidgetClass parent
		    #( FXmNbackground highlight-color )
		    undef FXtCreateManagedWidget to widget
		widget FXmNactivateCallback
		    func collector collector-cb
		    undef FXtAddCallback drop
	then
	parent label #( FXmNbackground highlight-color )
	    undef FXmCreatePulldownMenu { menu }
	label FxmCascadeButtonWidgetClass parent
	    #( FXmNbackground highlight-color
	       FXmNsubMenuId menu )
	    undef FXtCreateManagedWidget { cas-wid }
	cas-wid FXmNcascadingCallback
	    func collector menu children popdown-cascade-cb
	    undef FXtAddCallback drop
	listener-values #( widget menu cas-wid collector )
	    array-push drop
;

\ --- make-popup-menu for graph-, fft-, and listener-menu ---
\
\ general entries:
\ entries: #( #( name type cb func ) ... )
\
\ simple popdown (fft-menu)
\ entries: #( #( name type labels-array cb ) ... )
\
\ special popdown (listener)
\ entries: #( #( name type func collector with-one ) ... )
: make-popup-menu ( name parent entries -- menu )
	{ name parent entries }
	parent name
	    #( FXmNpopupEnabled FXmPOPUP_AUTOMATIC
	       FXmNbackground   highlight-color )
	    undef FXmCreatePopupMenu { menu }
	entries each { entry }
		#f { casc }
		\ string
		entry 0 array-ref { label }
		\ 'label|'separator|'cascade|#f
		entry 1 array-ref { typ }
		typ 'label = if
			FxmLabelWidgetClass
		else
			typ 'separator = if
				FxmSeparatorWidgetClass
			else
				FxmPushButtonWidgetClass
			then
		then { class }
		typ 'cascade = if
			entry length 4 = if	\ fft/edhist menu
				label
				    entry 2 array-ref ( labels )
				    menu
				    entry 3 array-ref ( prc )
				    make-simple-popdown-menu
			else			\ listener menu
				label
				    menu
				    entry 2 array-ref ( func )
				    entry 3 array-ref ( collector )
				    entry 4 array-ref ( with-one )
				    make-popdown-entry
			then
		else
			label class menu
			    #( FXmNbackground highlight-color )
			    undef FXtCreateManagedWidget { wid }
			entry 2 array-ref { cb }	\ cb: 3 args or #f
			cb if
				wid FXmNactivateCallback cb
				    undef FXtAddCallback drop
			then
			entry 3 array-ref { prc }	\ func: 1 arg or #f
			prc if
				prc #( wid ) run-proc drop
			then
		then
	end-each
	menu
;

\ --- selection popup ---
: sel-stop-play-cb { vars -- prc; self -- val }
	0 proc-create ( prc )
	vars ,
  does> { self -- val }
	self @ { vars }
	vars :stopping array-assoc-ref if
		vars :stopping #f array-assoc-set!
		    ( vars ) :stop-widget array-assoc-ref { w }
		w widget? if
			w "Play" change-label
		then
	then
	#f
;

: sel-play-again-cb ( -- val )
	selection play
;

: sel-play-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ { vars }
	vars :stopping array-assoc-ref if
		w "Play" change-label
		vars :stopping #f array-assoc-set!
		    ( vars ) :stopping1 array-assoc-ref if
			vars :stopping1 #f array-assoc-set!
			    ( vars ) :stop-widget1 array-assoc-ref
			    "Loop play" change-label
			stop-playing-selection-hook
			    <'> sel-play-again-cb remove-hook! drop
		then
		undef stop-playing
	else
		w "Stop" change-label
		vars :stop-widget w array-assoc-set!
		    ( vars ) :stopping #t array-assoc-set! drop
		selection play
	then
;

: sel-loop-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ { vars }
	vars :stopping1 array-assoc-ref if
		w "Loop play" change-label
		vars :stopping1 #f array-assoc-set!
		    ( vars ) :stopping array-assoc-ref if
		vars :stopping #f array-assoc-set!
		    ( vars ) :stop-widget array-assoc-ref "Play" change-label
		then
		stop-playing-selection-hook
		    <'> sel-play-again-cb remove-hook! drop
		undef stop-playing
	else
		w "Stop!" change-label
		vars :stop-widget1 w  array-assoc-set!
		    ( vars ) :stopping1 #t array-assoc-set! drop
		stop-playing-selection-hook <'> sel-play-again-cb add-hook!
		selection play
	then
;

: as-one-edit-thunk { sel -- prc; self -- val }
	0 proc-create ( prc )
	sel ,
  does> { self -- val }
	self @ { sel }
	sel 0 array-ref { snd }
	sel 1 array-ref { chn }
	snd chn selection-position { beg }
	snd chn selection-framples { len }
	beg 0> if
		0 beg snd chn delete-samples drop
	then
	snd chn #f framples { frms }
	len frms < if
		len 1+  frms len -  snd chn delete-samples drop
	then
	#f
;

: sel-del <{ w c info -- val }>
	delete-selection
;

: sel-zero <{ w c info -- val }>
	0.0 scale-selection-by
;

: sel-norm <{ w c info -- val }>
	1.0 scale-selection-to
;

: sel-crop <{ w c info -- val }>
	selection-members each ( sel )
		as-one-edit-thunk "" as-one-edit drop
	end-each
	#f
;

: sel-save-as <{ w c info -- val }>
	#t save-selection-dialog
;

: sel-copy <{ w c info -- val }>
	snd-tempnam { new-file-name }
	new-file-name save-selection drop
	new-file-name open-sound
;

: sel-cut <{ w c info -- val }>
	snd-tempnam { new-file-name }
	new-file-name save-selection drop
	delete-selection drop
	new-file-name open-sound
;

: sel-marks <{ w c info -- val }>
	selection-members each { select }
		select 0 array-ref { snd }
		select 1 array-ref { chn }
		snd chn selection-position { pos }
		snd chn selection-framples 1- { len }
		pos snd chn #f 0 add-mark drop
		pos len d+ snd chn #f 0 add-mark drop
	end-each
	#f
;

\ choice 2 == selection
: sel-appcnt <{ w c info -- val }>
	\ (apply-controls :optional snd (choice 0) (beg 0) (dur len))
	\ choice: 0 snd
	\         1 chn
	\         2 sel
	selected-sound 2 undef undef apply-controls
;

: sel-rescnt <{ w c info -- val }>
	selected-sound reset-controls
;

: sel-unsel <{ w c info -- val }>
	#f #t #f set-selection-member?
;

: sel-rev <{ w c info -- val }>
	reverse-selection
;

: sel-mix <{ w c info -- val }>
	#f #f #f cursor mix-selection
;

: sel-invert <{ w c info -- val }>
	-1.0 scale-selection-by
;

: sel-info <{ w c info -- val }>
	selected-sound { snd }
	snd selected-channel { chn }
	snd chn selection-position { beg }
	snd chn selection-framples { len }
	"    start: %d, %.3f\n" #( beg beg #f srate f/ ) string-format ( str )
	"      end: %d, %.3f\n" #( beg len + dup #f srate f/ ) string-format $+
	" duration: %d, %.3f\n" #( len len #f srate f/ ) string-format $+
	"    chans: %d\n" #( selection-chans ) string-format $+
	"   maxamp: %.3f\n" #( #f #f selection-maxamp ) string-format $+ { str }
	"Selection info" str info-dialog
;

let: ( -- menu )
	#a( :stopping #f
	    :stopping1 #f
	    :stop-widget #f
	    :stop-widget1 #f ) { vars }
	stop-playing-selection-hook vars sel-stop-play-cb add-hook!
	"selection-popup" main-widgets 2 array-ref
	#( #( "Selection"        'label     #f               #f )
	   #( "sep"              'separator #f               #f )
	   #( "Play"             #f         vars sel-play-cb #f )
	   #( "Loop play"        #f         vars sel-loop-cb #f )
	   #( "Delete"           #f         <'> sel-del      #f )
	   #( "-> 0.0"           #f         <'> sel-zero     #f )
	   #( "-> 1.0"           #f         <'> sel-norm     #f )
	   #( "Crop"             #f         <'> sel-crop     #f )
	   #( "Save as"          #f         <'> sel-save-as  #f )
	   #( "Copy -> new"      #f         <'> sel-copy     #f )
	   #( "Cut -> new"       #f         <'> sel-cut      #f )
	   #( "Snap marks"       #f         <'> sel-marks    #f )
	   #( "Apply controls"   #f         <'> sel-appcnt   #f )
	   #( "Reset controls"   #f         <'> sel-rescnt   #f )
	   #( "Unselect"         #f         <'> sel-unsel    #f )
	   #( "Reverse"          #f         <'> sel-rev      #f )
	   #( "Mix"              #f         <'> sel-mix      #f )
	   #( "Invert"           #f         <'> sel-invert   #f )
	   #( "Info"             #f         <'> sel-info     #f )
	) make-popup-menu
;let constant selection-popup-menu

\ --- time domain popup ---
: graph-popup-snd ( -- snd ) #f snd-snd ;
: graph-popup-chn ( -- chn ) #f snd-chn ;

: stop-playing-cb { vars -- prc; snd self -- val }
	1 proc-create ( prc )
	vars ,
  does> { snd self -- val }
	self @ { vars }
	vars :stopping array-assoc-ref if
		vars :stopping #f array-assoc-set!
		    ( vars ) :stop-widget array-assoc-ref ?dup-if
			"Play" change-label
		then
	then
	#f
;

: play-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ { vars }
	vars :stopping array-assoc-ref if
		vars :stopping #f array-assoc-set! drop
		w "Play" change-label
		undef stop-playing
	else
		w "Stop" change-label
		vars :stopping #t array-assoc-set! drop
		graph-popup-snd play
	then
;

: stop-cb { vars -- prc; widget self -- val }
	1 proc-create ( prc )
	vars ,
  does> { w self -- val }
	self @ ( vars ) :stop-widget w array-assoc-set!
;

: pchan-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ ( vars ) :stopping #t array-assoc-set!
	    ( vars ) :stop-widget array-assoc-ref "Stop" change-label
	graph-popup-snd :channel graph-popup-chn play
;

: pcur-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ ( vars ) :stopping #t array-assoc-set!
	    ( vars ) :stop-widget array-assoc-ref "Stop" change-label
	graph-popup-snd
	    :start graph-popup-snd graph-popup-chn #f cursor
	    play
;

: pprev-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ ( vars ) :stopping #t array-assoc-set!
	    ( vars ) :stop-widget array-assoc-ref "Stop" change-label
	graph-popup-snd
	    :channel graph-popup-chn
	    :edit-position graph-popup-snd graph-popup-chn edit-position 1-
	    play
;

: porig-cb { vars -- prc; w c i self -- val }
	3 proc-create ( prc )
	vars ,
  does> { w c info self -- val }
	self @ ( vars ) :stopping #t array-assoc-set!
	    ( vars ) :stop-widget array-assoc-ref "Stop" change-label
	graph-popup-snd :channel graph-popup-chn :edit-position 0 play
;

: pundo-cb <{ w c info -- val }>
	1 graph-popup-snd graph-popup-chn undo
;

: predo-cb <{ w c info -- val }>
	1 graph-popup-snd graph-popup-chn redo
;

: prev-cb <{ w c info -- val }>
	graph-popup-snd revert-sound
;

: popen-cb <{ w c info -- val }>
	#t open-file-dialog
;

: psave-cb <{ w c info -- val }>
	graph-popup-snd save-sound
;

: psaveas-cb <{ w c info -- val }>
	graph-popup-snd select-sound drop #t save-sound-dialog
;

: pupdate-cb <{ w c info -- val }>
	graph-popup-snd update-sound
;

: pclose-cb <{ w c info -- val }>
	graph-popup-snd close-sound-extend
	#f
;

: pmixsel-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn #f cursor
	    graph-popup-snd graph-popup-chn mix-selection
;

: pinssel-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn #f cursor
	    graph-popup-snd graph-popup-chn insert-selection
;

: prepsel-cb <{ w c info -- val }>
	graph-popup-snd { snd }
	graph-popup-chn { chn }
	snd chn #f cursor { beg }
	snd chn selection-framples { len }
	snd chn selection-position { sbeg }
	snd chn selection-member? not
	beg len + sbeg < ||
	beg sbeg len + > || if
		beg len snd chn #f delete-samples drop
		beg snd chn insert-selection
	else
		beg sbeg < if
			beg sbeg beg - snd chn #f delete-samples
		else
			#f
		then
	then
;

: pselall-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn select-all
;

: punsel-cb <{ w c info -- val }>
	#f #t #f set-selection-member?
;

: papcnt-cb <{ w c info -- val }>
	\ (apply-controls :optional snd (choice 0) (beg 0) (dur len))
	\ choice: 0 snd
	\         1 chn
	\         2 sel
	graph-popup-snd 0 undef undef apply-controls
;

: precnt-cb <{ w c info -- val }>
	graph-popup-snd reset-controls
;

: print-props { props -- str }
	object-print-length { old-len }
	print-length        { old-vct-len }
	3 set-object-print-length
	3 set-print-length drop
	"" ( str )
	props each { prop }	\ ( key . val )
		( str )
		"  %s:  %s\n"
		    #( prop 0 array-ref prop 1 array-ref ) string-format $+
	end-each
	( str )
	old-len set-object-print-length
	old-vct-len set-print-length drop
	( str )
;

: paddmrk-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn #f cursor ( samp )
	    graph-popup-snd graph-popup-chn #f ( name ) 0 ( sync ) add-mark
;

: pdelmrk-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn #f marks { ms }
	ms nil? unless
		ms length 1 = if
			ms 0 array-ref delete-mark drop
		else
			graph-popup-snd graph-popup-chn #f cursor { loc }
			ms 0 array-ref { id }
			loc id undef mark-sample - abs { cur-min }
			ms each { m }
				loc m undef mark-sample - abs { this-min }
				this-min cur-min < if
					this-min to cur-min
					m to id
				then
			end-each
			id delete-mark
		then
	then
	#f
;

: pdelamrk-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn delete-marks
;

: pnextmrk-cb <{ w c info -- val }>
	[char] j 4 graph-popup-snd graph-popup-chn key \ C-j
;

: plastmrk-cb <{ w c info -- val }>
	[char] - 4 graph-popup-snd graph-popup-chn key drop \ C--
	[char] j 4 graph-popup-snd graph-popup-chn key \ C-j
;

: pnorm-cb <{ w c info -- val }>
	1.0 graph-popup-snd graph-popup-chn scale-to
;

: pinfo-cb <{ w c info -- val }>
	graph-popup-snd { snd }
	graph-popup-chn { chn }
	snd chn #f framples { frms }
	snd srate { sr }
	"   chans: %d, srate: %d\n" #( snd channels sr ) string-format ( str )
	"  format: %s [%s]\n"
	    #( snd sample-type mus-sample-type-name
	       snd header-type mus-header-type-name ) string-format $+
	"  length: %.3f  (%d framples)\n" #( frms sr f/ frms ) string-format $+
	snd #t #f maxamp each { mx }
		"%6s %c: %.3f\n" #( "maxamp" [char] A i + mx ) string-format $+
	end-each
	snd comment empty? unless
		" comment: %s\n" #( snd comment ) string-format $+
	then
	snd file-name mus-sound-loop-info { loops }
	loops nil? unless
		"    loop: %s\n" #( loops ) string-format $+
	then
	snd header-type mus-soundfont = if
		"  sounds: %s\n" #( snd soundfont-info ) string-format $+
	then
	snd sound-properties { props }
	props nil? unless
		"properties:\n" $+
		props print-props $+
	then
	snd channels 0 ?do
		snd i channel-properties to props
		props nil? unless
			"chan %d properties:\n" #( i ) string-format $+
			props print-props $+
		then
	loop { str }
	snd file-name " info" $+ str info-dialog
;

: exit-cb <{ w c info -- val }>
	0 snd-exit
;

let: ( -- menu )
	#a( :stopping #f :stop-widget #f ) { vars }
	stop-playing-hook vars stop-playing-cb add-hook!
	"graph-popup" main-widgets 2 array-ref
	#( #( "Snd"              'label       #f              #f )
	   #( "sep"              'separator   #f              #f )
	   #( "Play"             #f         vars play-cb    vars stop-cb )
	   #( "Play channel"     #f         vars pchan-cb   #f )
	   #( "Play from cursor" #f         vars pcur-cb    #f )
	   #( "Play previous"    #f         vars pprev-cb   #f )
	   #( "Play original"    #f         vars porig-cb   #f )
	   #( "Undo"             #f         <'> pundo-cb    #f )
	   #( "Redo"             #f         <'> predo-cb    #f )
	   #( "Revert"           #f         <'> prev-cb     #f )
	   #( "Open"             #f         <'> popen-cb    #f )
	   #( "Save"             #f         <'> psave-cb    #f )
	   #( "Save as"          #f         <'> psaveas-cb  #f )
	   #( "Update"           #f         <'> pupdate-cb  #f )
	   #( "Close"            #f         <'> pclose-cb   #f )
	   #( "Mix selection"    #f         <'> pmixsel-cb  #f )
	   #( "Insert selection" #f         <'> pinssel-cb  #f )
	   #( "Replace with selection" #f   <'> prepsel-cb  #f )
	   #( "Select all"       #f         <'> pselall-cb  #f )
	   #( "Unselect"         #f         <'> punsel-cb   #f )
	   #( "Apply controls"   #f         <'> papcnt-cb   #f )
	   #( "Reset controls"   #f         <'> precnt-cb   #f )
	   #( "Add mark"         #f         <'> paddmrk-cb  #f )
	   #( "Delete mark"      #f         <'> pdelmrk-cb  #f )
	   #( "Delete all marks" #f         <'> pdelamrk-cb #f )
	   #( "To next mark"     #f         <'> pnextmrk-cb #f )
	   #( "To last mark"     #f         <'> plastmrk-cb #f )
	   #( "-> 1.0"           #f         <'> pnorm-cb    #f )
	   #( "Info"             #f         <'> pinfo-cb    #f )
	   #( "sep"              'separator #f              #f )
	   #( "Exit"             #f         <'> exit-cb     #f )
	) make-popup-menu
;let constant graph-popup-menu

: graph-popup-cb { snd chn -- prc; w self -- }
	1 proc-create ( prc )
	chn , snd ,
  does> { w self -- }
	self       @   { chn }
	self cell+ @   { snd }
	snd chn edits  { eds }
	w widget->name { name }
	name "Snd" string= if
		snd channels 1 > if
			"%s[%d]"
			    #( snd short-file-name chn )
			    string-format w swap change-label
		else
			w snd short-file-name change-label
		then
		#f
		exit
	then
	name "Save"          string=
	name "Undo"          string= ||
	name "Revert"        string= ||
	name "Play previous" string= || if
		w eds 0 array-ref 0> if
			show-widget
		else
			hide-widget
		then
		exit
	then
	name "Play channel" string= if
		w snd channels 1 > if
			show-widget
		else
			hide-widget
		then
		exit
	then
	name "Redo" string= if
		w eds 1 array-ref 0> if
			show-widget
		else
			hide-widget
		then
		exit
	then
	name "Mix selection"          string=
	name "Insert selection"       string= ||
	name "Unselect"               string= ||
	name "Replace with selection" string= || if
		w undef selection? if
			show-widget
		else
			hide-widget
		then
		exit
	then
	name "Play from cursor" string= if
		w snd chn #f cursor 0> if
			show-widget
		else
			hide-widget
		then
		exit
	then
	name "Play original" string= if
		w eds 0 array-ref 1 > if
			show-widget
		else
			hide-widget
		then
		exit
	then
	name "Delete mark"       string=
	name "Delete all marks"  string= ||
	name "To next mark"      string= ||
	name "To last mark"      string= || if
		w snd chn #f marks nil? unless
			show-widget
		else
			hide-widget
		then
	else
		#f
	then
;

\ --- fft popup ---
: choose-chan ( -- chn )
	graph-popup-snd channel-style channels-separate = if
		graph-popup-chn
	else
		#t
	then
;

: fft-peaks-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn show-transform-peaks not
	    graph-popup-snd choose-chan set-show-transform-peaks
;

: fft-db-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn fft-log-magnitude not
	    graph-popup-snd choose-chan set-fft-log-magnitude
;

: fft-frq-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn fft-log-frequency not
	    graph-popup-snd choose-chan set-fft-log-frequency
;

: fft-norm-cb <{ w c info -- val }>
	graph-popup-snd graph-popup-chn
	    transform-normalization dont-normalize = if
		normalize-by-channel graph-popup-snd choose-chan
	else
		dont-normalize       graph-popup-snd choose-chan
	then set-transform-normalization
;

: grp-lst-cb { val -- prc; w c i self -- val }
	3 proc-create ( prc )
	val ,
  does> { w c info self -- val }
	self @ ( val ) graph-popup-snd choose-chan set-transform-graph-type
;

: grp-labs ( -- ary )
	#( #( "once"        graph-once           grp-lst-cb )
	   #( "sonogram"    graph-as-sonogram    grp-lst-cb )
	   #( "spectrogram" graph-as-spectrogram grp-lst-cb ) )
;

: grp-set <{ lst -- }>
	graph-popup-snd graph-popup-chn transform-graph-type { tp }
	lst each ( child )
		i tp <> set-sensitive
	end-each
;

#( 32 64 128 256 512 1024 2048 4096 8192 16384 65536 262144 1048576 )
    constant fft-siz-sizes

: siz-lst-cb { val -- prc; w c i self -- val }
	3 proc-create ( prc )
	val ,
  does> { w c info self -- val }
	self @ ( val ) graph-popup-snd choose-chan set-transform-size
;

: siz-labs ( -- ary )
	fft-siz-sizes map
		#( *key* object->string *key* siz-lst-cb )
	end-map ( ary )
;

: siz-set <{ lst -- }>
	graph-popup-snd graph-popup-chn transform-size { siz }
	lst each ( child )
		fft-siz-sizes i array-ref siz <> set-sensitive
	end-each
;

#( rectangular-window
   hann-window
   welch-window
   parzen-window
   bartlett-window
   hamming-window
   blackman2-window 
   blackman3-window
   blackman4-window
   exponential-window
   riemann-window
   kaiser-window
   cauchy-window
   poisson-window
   gaussian-window
   tukey-window
   dolph-chebyshev-window
   hann-poisson-window
   connes-window
   samaraki-window
   ultraspherical-window
   bartlett-hann-window
   bohman-window
   flat-top-window
   blackman5-window
   blackman6-window
   blackman7-window
   blackman8-window
   blackman9-window
   blackman10-window
   rv2-window
   rv3-window
   rv4-window
   mlt-sine-window
   papoulis-window
   sinc-window ) constant fft-win-windows

: win-lst-cb { val -- prc; w c i self -- val }
	3 proc-create ( prc )
	val ,
  does> { w c info self -- val }
	self @ ( val ) graph-popup-snd choose-chan set-fft-window
;

: win-labs ( -- ary )
	#( "Rectangular"
	   "Hann"
	   "Welch"
	   "Parzen"
	   "Bartlett"
	   "Hamming"
	   "Blackman2"
	   "Blackman3"
	   "Blackman4"
	   "Exponential"
	   "Riemann"
	   "Kaiser"
	   "Cauchy"
	   "Poisson"
	   "Gaussian"
	   "Tukey"
	   "Dolph-Chebyshev"
	   "Hann-Poisson"
	   "Connes"
	   "Samaraki"
	   "Ultraspherical"
	   "Bartlett-Hann"
	   "Bohman"
	   "Flat-top"
	   "Blackman5"
	   "Blackman6"
	   "Blackman7"
	   "Blackman8"
	   "Blackman9"
	   "Blackman10"
	   "Rife-Vincent2"
	   "Rife-Vincent3"
	   "Rife-Vincent4"
	   "MLT Sine"
	   "Papoulis"
	   "Sinc" ) map
		#( *key* fft-win-windows i array-ref win-lst-cb )
	end-map
;

: win-set <{ lst -- }>
	graph-popup-snd graph-popup-chn fft-window { win }
	lst each ( child )
		fft-win-windows i array-ref win <> set-sensitive
	end-each
;

#( fourier-transform
   wavelet-transform
   walsh-transform
   autocorrelation
   cepstrum
   haar-transform ) value fft-trn-transform

: trn-lst-cb { val -- prc; w c i self -- val }
	3 proc-create ( prc )
	val ,
  does> { w c info self -- }
	self @ ( val ) graph-popup-snd choose-chan set-transform-type
;

: trn-labs ( -- ary )
	#( "Fourier" "Wavelet" "Walsh" "Autocorrelate" "Cepstrum" "Haar" ) map
		#( *key* fft-trn-transform i array-ref trn-lst-cb )
	end-map ( ary )
;

: trn-set <{ lst -- }>
	graph-popup-snd graph-popup-chn transform-type { trn }
	lst each ( child )
		fft-trn-transform i array-ref trn equal? not set-sensitive
	end-each
;

: typ-lst-cb { val -- prc; w c i self -- val }
	3 proc-create ( prc )
	val ,
  does> { w c info self -- val }
	self @ ( val ) graph-popup-snd choose-chan set-wavelet-type
;

: typ-labs ( -- ary )
	#( "doub4"
	   "doub6"
	   "doub8"
	   "doub10"
	   "doub12"
	   "doub14"
	   "doub16"
	   "doub18"
	   "doub20"
	   "battle-lemarie"
	   "burt-adelson"
	   "beylkin"
	   "coif2"
	   "coif4"
	   "coif6"
	   "sym2"
	   "sym3"
	   "sym4"
	   "sym5"
	   "sym6"
	   "doub22"
	   "doub24"
	   "doub26"
	   "doub28"
	   "doub30"
	   "doub32"
	   "doub34"
	   "doub36"
	   "doub38"
	   "doub40"
	   "doub42"
	   "doub44"
	   "doub46"
	   "doub48"
	   "doub50"
	   "doub52"
	   "doub54"
	   "doub56"
	   "doub58"
	   "doub60"
	   "doub62"
	   "doub64"
	   "doub66"
	   "doub68"
	   "doub70"
	   "doub72"
	   "doub74"
	   "doub76" ) map
		#( *key* i typ-lst-cb )
	end-map
;

: typ-set <{ lst -- }>
	graph-popup-snd graph-popup-chn wavelet-type { tp }
	lst each ( child )
		i tp <> set-sensitive
	end-each
;

: fft-color <{ w c info -- val }>
	#t color-orientation-dialog
;

let: ( -- menu )
	"fft-popup" main-widgets 2 array-ref
	#( #( "Transform"        'label     #f               #f )
	   #( "sep"              'separator #f               #f )
	   #( "Peaks"            #f         <'> fft-peaks-cb #f )
	   #( "dB"               #f         <'> fft-db-cb    #f )
	   #( "Log freq"         #f         <'> fft-frq-cb   #f )
	   #( "Normalize"        #f         <'> fft-norm-cb  #f )
	   #( "Graph type"       'cascade   grp-labs         <'> grp-set )
	   #( "Size"             'cascade   siz-labs         <'> siz-set )
	   #( "Window"           'cascade   win-labs         <'> win-set )
	   #( "Transform type"   'cascade   trn-labs         <'> trn-set )
	   #( "Wavelet type"     'cascade   typ-labs         <'> typ-set )
	   #( "Color/Orientation" #f        <'> fft-color    #f )
	) make-popup-menu
;let constant fft-popup-menu

: fft-popup-cb { snd chn -- prc; w self -- val }
	1 proc-create ( prc )
	chn , snd ,
  does> { w self -- val }
	self       @   { chn }
	self cell+ @   { snd }
	w widget->name { name }
	name "Peaks" string= if
		w snd chn show-transform-peaks if
			"No peaks"
		else
			"Peaks"
		then change-label
	else
		name "dB" string= if
			w snd chn fft-log-magnitude if
				"Linear"
			else
				"dB"
			then change-label
		else
			name "Log freq" string= if
				w snd chn fft-log-frequency if
					"Linear freq"
				else
					"Log freq"
				then change-label
			then
		then
	then
	cascade-popup-cb-list each { entry }
		entry 0 array-ref #( entry 1 array-ref ) run-proc drop
	end-each
	#f
;

: popup-install { snd chn xe ye info -- }
	snd chn transform-graph? if
		snd chn transform-graph axis-info
	else
		#f
	then { fax }
	snd chn lisp-graph? if
		snd chn lisp-graph axis-info
	else
		#f
	then { lax }
	fax if
		xe fax 10 array-ref >=
		xe fax 12 array-ref <= && if
			\ in fft
			fft-popup-menu snd chn fft-popup-cb for-each-child
			fft-popup-menu info popup-post-it
			#f
		else
			#t
		then
	else
		#t
	then if
		lax if
			xe lax 10 array-ref >=
			xe lax 12 array-ref <= && if
				\ in lisp
				#f
			else
				#t
			then
		else
			#t
		then if
			undef selection? if
				snd graph-popup-chn selection-position { pos }
				snd srate { sr }
				\ BEG and END should be floats
				pos sr f/ { beg }
				pos snd graph-popup-chn selection-framples f+
				    sr f/ { end }
				xe beg snd chn undef x->position >=
				xe end snd chn undef x->position <= && if
					selection-popup-menu info popup-post-it
					#f
				else
					#t
				then
			else
				#t
			then if
				graph-popup-menu
				    graph-popup-snd graph-popup-chn
				    graph-popup-cb for-each-child
				graph-popup-menu info popup-post-it
			then
		then
	then
;

\ --- edit history popup ---
#() value edhist-funcs
#() value edhist-widgets
: edhist-snd ( -- snd ) #f snd-snd ;
: edhist-chn ( -- chn ) #f snd-chn ;

: edhist-funcs-index { key -- idx|-1 }
	-1 ( idx )
	edhist-funcs each
		( val ) car key equal? if
			drop			\ drop -1
			i			\ set to current index
			leave
		then
	end-each ( idx )
;

: edhist-funcs-ref { key -- val|#f }
	key edhist-funcs-index { idx }
	idx 0>= if
		edhist-funcs idx array-ref cadr	\ => proc
	else
		#f
	then
;

: edhist-funcs-set! { key val -- }
	key edhist-funcs-index { idx }
	idx 0>= if
		edhist-funcs idx #( key val ) array-set!
	else
		edhist-funcs #( key val ) array-push to edhist-funcs
	then
;

: edhist-clear-edits <{ w c info -- #f }>
	#() to edhist-funcs
	#f
;

: edhist-save-edits <{ w c info -- val }>
	edhist-snd { snd }
	edhist-chn { chn }
	#( snd chn ) edhist-funcs-ref { old-prc }
	snd chn edits { cur-edits }
	0 ( sum )
	cur-edits each ( val )
		+ ( sum += val )
	end-each { sum }
	snd chn cur-edits car 1+ sum edit-list->function { prc }
	edhist-save-hook #( prc ) run-hook drop
	#( snd chn ) prc edhist-funcs-set!
	#f
;

: edhist-reapply-edits <{ w c info -- val }>
	#( edhist-snd edhist-chn ) edhist-funcs-ref { prc }
	prc proc? if
		prc #( edhist-snd edhist-chn ) run-proc
	else
		#f
	then
;

: edhist-set-wid <{ w -- }>
	edhist-widgets w array-push to edhist-widgets
;

: edhist-apply <{ w c info -- val }>
	\ context (c) is index (i) from edhist-apply-edits, see below
	edhist-funcs c range? if
		edhist-funcs c array-ref cadr ( prc )
		#( edhist-snd edhist-chn ) run-proc
	else
		#f
	then
;

: edhist-apply-edits <{ dummy -- val }>
	edhist-widgets car { parent }
	edhist-widgets cdr { wids }
	edhist-funcs each car { label }
		nil { button }
		wids nil? if
			"wid" FxmPushButtonWidgetClass parent
			    #( FXmNbackground highlight-color )
			    undef FXtCreateManagedWidget to button
			edhist-widgets button array-push to edhist-widgets
			\ index (i) is context (c) in edhist-apply, see above
			button FXmNactivateCallback
			    <'> edhist-apply i FXtAddCallback drop
		else
			wids car to button
			wids cdr to wids
			button FXtManageChild drop
		then
		label array? if		\ label: #( snd chn )
			button  "%s[%s]"
			    #( label car short-file-name
			       label cadr ) string-format
		else			\ label: "file-name[chn]"
			button label
		then change-label
	end-each
	wids each ( w )
		FXtUnmanageChild drop
	end-each
	#f
;

: edhist-close-hook-cb <{ snd -- }>
	snd short-file-name { name }
	snd channels 0 ?do
		#( snd i ) edhist-funcs-ref { old-val }
		old-val array? if
			old-val 0 "%s[%d]" #( name i ) string-format array-set!
		then
	loop
;

let: ( -- menu )
	close-hook <'> edhist-close-hook-cb add-hook!
	"edhist-popup" main-widgets 2 array-ref
	#( #( "Edits"   'label     #f                       #f )
	   #( "sep"     'separator #f                       #f )
	   #( "Save"    #f         <'> edhist-save-edits    #f )
	   #( "Reapply" #f         <'> edhist-reapply-edits #f )
	   #( "Apply"   'cascade   <'> edhist-set-wid <'> edhist-apply-edits )
	   #( "Clear"   #f         <'> edhist-clear-edits   #f )
	   #( "sep"     'separator #f                       #f )
	   #( "Help"    #f         <'> edhist-help-edits    #f )
	) make-popup-menu
;let constant edit-history-menu

: edhist-popup-cb <{ w -- val }>
	edhist-snd { snd }
	edhist-chn { chn }
	w FXtName { name }
	name "Clear" string=
	name "Apply" string= || if
		w edhist-funcs empty? not set-sensitive
	else
		name "Save" string= if
			snd chn edits { eds }
			0 ( sum )
			eds each ( val )
				+ ( sum += val )
			end-each { sum }
			w sum 0> set-sensitive
		else
			name "Reapply" string= if
				#( snd chn ) edhist-funcs-ref { prc }
				w prc word? set-sensitive
			then
		then
	then
	#f
;

: edhist-popup-handler-cb <{ w c info -- val }>
	info Fevent { ev }
	FButtonPress ev Ftype = if
		edit-history-menu <'> edhist-popup-cb for-each-child
		edit-history-menu info popup-post-it
	then
	#f
;  

: popup-handler-cb <{ w c info -- val }>
	info Fevent { ev }
	ev Fx_root w 0 0 FXtTranslateCoords 0 array-ref - { xe }
	ev Fy { ye }
	FButtonPress ev Ftype = if
		edhist-snd edhist-chn xe ye info popup-install
	then
	#f
;

\ --- listener popup ---
: identity-cb <{ snds -- lst }>
	snds
;

: edited-cb <{ snds -- lst }>
	snds each { snd }
		snd channels 0 ?do
			snd i edits 0 array-ref 0= if
				snds snd array-delete-key drop
			then
		loop
	end-each
	snds
;

: focused-cb <{ snds -- lst }>
	snds length 1 > if
		snds
	else
		#()
	then
;

: list-play-cb <{ snd -- val }>
	snd play
;

: list-focus-cb <{ us -- val }>
	\ 5 == notebook-outer-pane
	main-widgets 5 array-ref FWidget? if
		us set-selected-sound
	else
		us sound-widgets 0 array-ref { pane }
		main-widgets 1 array-ref #( FXmNallowShellResize #f )
		    FXtVaSetValues drop
		sounds each ( them )
			sound-widgets 0 array-ref FXtUnmanageChild drop
		end-each
		pane FXtManageChild drop
		main-widgets 1 array-ref
		    #( FXmNallowShellResize auto-resize )
		    FXtVaSetValues
	then
;

: list-help-cb <{ w c info -- val }>
	listener-selection { selected }
	selected if
		selected undef snd-help { help }
		help if
			selected help undef undef help-dialog
		then
	then
;

: list-clear-cb <{ w c info -- val }>
	clear-listener
;

: listener-edit <{ w -- }>
	w FXtName "Help" string= if
		listener-selection ?dup-if
			1 >list w "Help on %S" rot
			    string-format change-label
			w FXtManageChild
		else
			w FXtUnmanageChild
		then drop
	then
;

: listener-popup-cb <{ w c info -- }>
	c { menu }
	FButtonPress info Fevent Ftype = if
		listener-values each { vals }
			vals array? if
				vals 0 array-ref { top-one }
				vals 1 array-ref { top-two }
				vals 2 array-ref { top-two-cascade }
				vals 3 array-ref #( sounds )
				    run-proc length { len }
				top-two FXtUnmanageChild drop
				top-two-cascade FXtUnmanageChild drop
				top-one if
					top-one FXtUnmanageChild drop
				then
				len 1 > if
					top-two-cascade FXtManageChild drop
					top-two FXtManageChild drop
				then
				top-one FWidget?
				len 1 = && if
					top-one FXtManageChild drop
				then
			then
		end-each
		menu <'> listener-edit for-each-child
		info menu Fset_menuToPost drop
	then
;

let: ( -- menu )
	main-widgets 4 array-ref ?dup-if
		( parent )
	else
		#t set-show-listener drop
		#f set-show-listener drop
		main-widgets 4 array-ref
	then { parent }
	"listener-popup" parent
	#( #( "Listener" 'label   #f                     #f )
	   #( "sep"    'separator #f                     #f )
	   #( "Play"   'cascade   <'> list-play-cb       <'> identity-cb #t )
	   #( "Help"   #f         <'> list-help-cb       #f )
	   #( "Open"   #f         <'> popen-cb           #f )
	   #( "Clear listener" #f <'> list-clear-cb      #f )
	   #( "Close"  'cascade   <'> close-sound-extend <'> identity-cb #t )
	   #( "Save"   'cascade   <'> save-sound         <'> edited-cb #t )
	   #( "Revert" 'cascade   <'> revert-sound       <'> edited-cb #t )
	   #( "Focus"  'cascade   <'> list-focus-cb      <'> focused-cb  #f )
	   #( "sep"    'separator #f                     #f )
	   #( "Exit"   #f         <'> exit-cb            #f )
	) make-popup-menu { menu }
	parent FXmNpopupHandlerCallback <'> listener-popup-cb menu
	    FXtAddCallback drop
	menu
;let constant listener-popup-menu

#() constant popups
: add-popup <{ snd -- }>
	snd channels 0 ?do
		popups #( snd i ) array-member? unless
			popups #( snd i ) array-push to popups
			snd i channel-widgets 7 array-ref ( chn-edhist )
			    FXmNpopupHandlerCallback
			    <'> edhist-popup-handler-cb undef
			    FXtAddCallback drop
			snd i channel-widgets 0 array-ref ( chn-grf )
			FXmNpopupHandlerCallback <'> popup-handler-cb
			    undef FXtAddCallback drop
		then
	loop
;

: change-color-col-cb { col -- prc; w self -- val }
	1 proc-create ( prc )
	col , 
  does> { w self -- val }
	w self @ ( col ) FXmChangeColor
;
set-current

: change-menu-color ( menu new-color -- )
	doc" Change the color of MENU to NEW-COLOR.  \
NEW-COLOR can be the color name, an xm Pixel, a snd color, \
or a list of rgb values (as in Snd's make-color)."
	{ menu new-color }
	new-color string? if	\ assuming X11 color names here
		main-widgets 1 array-ref { shell }
		shell FXtDisplay { dpy }
		dpy FDefaultScreen { scr }
		dpy scr FDefaultColormap { cmap }
		FXColor { col }
		dpy cmap new-color col col FXAllocNamedColor 0= if
			"can't allocate %S"
			    #( new-color ) string-format snd-error
		else
			col Fpixel
		then
	else
		new-color color? if
			new-color
		else
			new-color each
				( vals-to-stack )
			end-each make-color
		then
	then ( color-pixel ) menu swap
	    change-color-col-cb for-each-child
;

: change-selection-popup-color ( new-color -- )
	doc" Change the selection popup menu's color: \
\"red\" change-selection-popup-color."
	selection-popup-menu swap change-menu-color
;

: change-graph-popup-color ( new-color -- )
	doc" Change the time-domain popup menu's color: \
basic-color change-graph-popup-color."
	selection-popup-menu swap change-menu-color
;

: change-fft-popup-color ( new-color -- )
	doc" Change the fft popup menu's color: \
#(0.5 0.5 0.5) change-fft-popup-color."
	fft-popup-menu swap change-menu-color
;

: change-edhist-popup-color ( new-color -- )
	doc" Change the time-domain popup menu's color: \
basic-color change-graph-popup-color."
	edit-history-menu swap change-menu-color
;

: change-listener-popup-color ( new-color -- )
	doc" Change the listener popup menu's color."
	listener-popup-menu swap change-menu-color
;

: add-popups ( -- )
	after-open-hook <'> add-popup add-hook!
	sounds each ( snd )
		add-popup
	end-each
;
previous

\ install all popups
add-popups

\ popup.fs ends here
