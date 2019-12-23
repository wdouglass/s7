\ snd-xm.fs -- snd-motif|gtk.scm|snd-xm.rb --> snd-xm.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/12/26 22:36:46
\ Changed: 19/12/23 15:07:35
\
\ @(#)snd-xm.fs	1.42 12/23/19

\ Commentary:
\
\ Requires --with-motif|gtk
\
\ Tested with Snd 20.x
\             Fth 1.4.x
\             Motif 2.3.3 X11R6
\             Gtk+ 3.0.12, Glib 2.28.8, Pango 1.28.4, Cairo 1.10.2
\ 
\ Motif and Gtk:
\
\ widget?                  ( w -- f )
\ set-sensitive            ( w f -- )
\ is-managed?              ( w -- f )
\ change-label     	   ( widget new-label -- )
\ for-each-child   	   ( widget prc -- )
\ load-font                ( name -- fid|#f )
\ host-name                ( -- host )
\ add-main-pane            ( name class args -- wid )
\ raise-dialog             ( dialog -- )
\ activate-dialog          ( dialog -- )
\ show-disk-space          ( snd -- )
\
\ Motif only:
\ 
\ main-dpy                 ( -- dpy )
\ current-screen   	   ( -- scr )
\ white-pixel      	   ( -- pix )
\ black-pixel      	   ( -- pix )
\ screen-depth     	   ( -- n )
\ 
\ children->array          ( widget -- array )
\ find-child               ( widget name -- wid )
\ widget-exists?           ( widget name -- f )
\ main-widget-exists?      ( name -- f )
\ display-widget-tree      ( widget -- )
\ set-main-color-of-widget ( w -- )
\ 
\ add-channel-pane         ( snd chn name type args -- wid )
\ add-sound-pane           ( snd name type args -- wid )
\ add-listener-pane        ( name type args -- wid )
\ 
\ add-mark-pane            ( -- )
\ 
\ current-label    	   ( widget -- label )

\ Code:

'snd-nogui provided? [if] skip-file [then]

'snd-gtk provided? [if]
	'gtk3 provided? not [if]
		\ We use is-managed? in snd-test.fs
		[defined] Fgtk_widget_get_realized [if]
			<'> Fgtk_widget_get_realized
		[else]
			<'> noop
		[then] alias is-managed? ( wid -- f )
		.( snd-gtk: gtk3 required -- skipping snd-xm.fs ) cr
		skip-file
	[then]
[then]

"X error" create-exception x-error

require extensions

\
\ main-widgets
\
0 constant top-level-application
1 constant top-level-shell
2 constant main-pane-shell
3 constant main-sound-pane
4 constant listener-pane
5 constant notebook-outer-pane

\
\ usage: top-level-shell main-widgets-ref { wid }
\
: main-widgets-ref { idx -- w }
	main-widgets idx array-ref
;

\
\ menu-widgets
\
0 constant top-menu-bar
1 constant file-menu
2 constant edit-menu
3 constant view-menu
4 constant options-menu
5 constant help-menu

: menu-widgets-ref { idx -- w }
	menu-widgets idx array-ref
;

\
\ sound-widgets
\
0 constant sw-main-pane
1 constant sw-name-label
2 constant sw-control-panel
3 constant sw-minibuffer
4 constant sw-play
5 constant sw-filter-graph
6 constant sw-unite
7 constant sw-minibuffer-label
8 constant sw-name-icon
9 constant sw-sync

: sound-widgets-ref { snd idx -- w }
	snd sound-widgets idx array-ref
;

\
\ channel-widgets
\
 0 constant cw-graph
 1 constant cw-w
 2 constant cw-f
 3 constant cw-sx
 4 constant cw-sy
 5 constant cw-zx
 6 constant cw-zy
 7 constant cw-edhist
 8 constant cw-gsy
 9 constant cw-gzy
10 constant cw-channel-main-pane

: channel-widgets-ref { snd chn idx -- w }
	snd chn channel-widgets idx array-ref
;

\
\ dialog-widgets
\
 0 constant dw-orientation-dialog
 1 constant dw-enved-dialog
 2 constant dw-transform-dialog
 3 constant dw-file-open-dialog
 4 constant dw-file-save-as-dialog
 5 constant dw-view-files-dialog
 6 constant dw-raw-data-dialog
 7 constant dw-new-file-dialog
 8 constant dw-file-mix-dialog
 9 constant dw-edit-header-dialog
10 constant dw-find-dialog
11 constant dw-help-dialog
12 constant dw-mix-panel-dialog
13 constant dw-print-dialog
14 constant dw-region-dialog
15 constant dw-info-dialog
16 constant dw-extra-controls-dialog
17 constant dw-save-selection-dialog
18 constant dw-insert-file-dialog
19 constant dw-save-region-dialog
20 constant dw-preferences-dialog

: dialog-widgets-ref { idx -- w }
	dialog-widgets idx array-ref
;

'snd-motif provided? [if]	\ HAVE_MOTIF
	\
	\ dialog-ok-widget	( dialog -- child )
	\ dialog-cancel-widget	( dialog -- child )
	\ dialog-help-widget	( dialog -- child )
	\
	\ These words take a dialog widget and return the corresponding button
	\ widgets.
	\
	'( "ok" "cancel" "help" ) let: ( button -- )
		each { name }
			"\
	: dialog-%s-widget ( dialog -- child )
		FXmDIALOG_%s_BUTTON FXmMessageBoxGetChild
	;" '( name dup string-upcase ) string-format string-eval
		end-each
	;let

	: xm-length ( lst -- lst len/2 )
		dup length 2/
	;

	\
	\ Some Motif convenience functions not found in snd/xm.c but in
	\ include/Xm/*.h
	\
	\ It creates for example:
	\
	\ '( "Label" ) let: ... ;let ==>
	\	FXmVaCreateManagedLabel ( parent name args -- w )
	\
	'( "ArrowButton"
	   "ArrowButtonGadget"
	   "BulletinBoard"
	   "ButtonBox"
	   "CascadeButton"
	   "CascadeButtonGadget"
	   "ColorSelector"
	   "Column"
	   "ComboBox"
	   "Command"
	   "Container"
	   "DataField"
	   "DrawingArea"
	   "DrawnButton"
	   "DropDown"
	   "FileSelectionBox"
	   "Form"
	   "Frame"
	   "Label"
	   "LabelGadget"
	   "List"
	   "MainWindow"
	   "MenuBar"
	   "MessageBox"
	   "Notebook"
	   "PanedWindow"
	   "PushButton"
	   "PushButtonGadget"
	   "RowColumn"
	   "Scale"
	   "ScrollBar"
	   "ScrolledWindow"
	   "ScrolledText"
	   "SelectionBox"
	   "Separator"
	   "SeparatorGadget"
	   "SimpleSpinBox"
	   "SpinBox"
	   "TabStack"
	   "Text"
	   "TextField"
	   "ToggleButton"
	   "ToggleButtonGadget" ) let: ( names -- )
		each { name }
			"\
	: FXmVaCreateManaged%s ( parent name args -- w )
		xm-length FXmCreate%s dup FXtManageChild drop
	;" '( name dup ) string-format string-eval
		end-each
	;let
[then]				\ HAVE_MOTIF

\ ;;; -------- show-disk-space
\ ;;;
\ ;;; adds a label to the minibuffer area showing the current free space 

hide
#() value labelled-snds

: kmg { num -- str }
	num 0<= if
		"disk full!"
	else
		"" { str }
		num 1024 d> if
			num 1024 1024 * d> if
				"space: %6.3fG" #( num 1024.0 1024.0 f* f/ )
			else
				"space: %6.3fM" #( num 1024.0 f/ )
			then
		else
			"space: %10dK" #( num )
		then string-format
	then
;
set-current

#f value showing-disk-space	\ for prefs
\
\ after-open-hook <'> show-disk-space add-hook!
\
previous

\ ===== Gtk and Motif =====

'snd-gtk provided? [if]		\ !HAVE_MOTIF
	: widget? ( w -- f )
		FGTK_IS_WIDGET
	;

	: set-sensitive ( w f -- )
		Fgtk_widget_set_sensitive drop
	;

	[defined] Fgtk_widget_get_realized [if]
		<'> Fgtk_widget_get_realized
	[else]
		<'> noop
	[then] alias is-managed? ( wid -- f )

	: change-label ( wid new-label -- )
		doc" Change WIDGET's label to be NEW-LABEL."
		{ wid new-label }
		wid if
			wid FGTK_IS_LABEL if
				wid
			else
				wid FGTK_BIN Fgtk_bin_get_child
			then FGTK_LABEL new-label Fgtk_label_set_text drop
		then
	;

	hide
	: for-each-cb ( cb -- prc; w d self -- val )
		{ cb }
		2 proc-create ( prc )
		cb ,
	  does> { w d self -- val }
		self @ ( cb ) #( w ) run-proc
	;
	set-current

	: for-each-child { wid prc -- }
		doc" Apply PRC ( w -- val ) to WIDGET and each of its children."
		wid widget? if
			prc #( wid ) run-proc drop
			wid FGTK_CONTAINER prc for-each-cb
			    Fgtk_container_foreach drop
		then
	;
	previous

	: load-font ( name -- fid|#f )
		Fpango_font_description_from_string
	;

	: host-name ( -- host )
		doc" Return name of current machine."
		top-level-application main-widgets-ref
		"WM_CLIENT_MACHINE" #f Fgdk_atom_intern
		FGDK_TARGET_STRING 0 1024 0 Fgdk_property_get { val }
		\ we get '( #t #( GdkAtom 0x3f ) 8 21 "pumpkin.fth-devel.net" )
		val car if
			val 4 list-ref 0 val 3 list-ref string-substring
		else
			#f
		then
	;

	\ --- add our own pane to the overall Snd window (underneath the
	\     listener in this case) ---

	\ notebook-outer-pane main-widgets-ref (without -notebook) and
	\ main-pane-shell main-widgets-ref
	\ seem to be of type GTK_BOX [ms]

	: add-main-pane <{ name
	    :optional class-not-needed #f args-not-needed #f -- wid }>
		FGTK_ORIENTATION_HORIZONTAL 0 Fgtk_box_new { pane }
		notebook-outer-pane main-widgets-ref { parent }
		parent FGTK_IS_BOX unless
			main-pane-shell main-widgets-ref to parent
		then
		parent FGTK_IS_BOX unless
			'x-error
			    #( "%s: no GTK_BOX widget found"
			       get-func-name ) fth-throw
		then
		parent FGTK_BOX pane #f #f 4 Fgtk_box_pack_start drop
		pane Fgtk_widget_show drop
		pane name Fgtk_widget_set_name drop
		pane
	;

	\ --- bring possibly-obscured dialog to top ---

	: raise-dialog ( dialog -- )
		{ w }
		w Fgtk_widget_show drop
		w FGTK_WINDOW Fgtk_window_present drop
	;

	<'> raise-dialog alias activate-dialog ( dialog -- )

	\ --- show-disk-space, Gtk specific ---

	hide
	: show-label <{ data -- n }>
		data 0 array-ref sound? if
			data 0 array-ref { snd }
			data 1 array-ref { wid }
			snd file-name disk-kspace kmg { space }
			wid FGTK_LABEL space Fgtk_label_set_text drop
			10000 running-word data Fg_timeout_add drop
			0
		else
			nil
		then
	;
	set-current

	: show-disk-space <{ snd -- }>
		doc" Add a label to the minibuffer area showing the current \
free space (for use with after-open-hook)."
		#f ( flag )
		labelled-snds each { n }
			n 0 array-ref snd equal? if
				( flag ) drop
				n
				leave
			then
		end-each { previous-label }
		previous-label unless
			snd sound? if
				#t to showing-disk-space
				snd 10 sound-widgets-ref { name-form }
				snd file-name disk-kspace kmg { space }
				space Fgtk_label_new { new-label }
				name-form FGTK_BOX new-label #f #f 6
				    Fgtk_box_pack_start drop
				new-label Fgtk_widget_show drop
				#( snd new-label ) to previous-label
				labelled-snds previous-label array-push drop
				10000 <'> show-label previous-label
				    Fg_timeout_add drop
			else
				"no sound found for disk space label"
				    snd-error drop
			then
		then
	;
	previous
[else]				\ HAVE_MOTIF
	: widget? ( w -- f)
		FWidget?
	;

	: set-sensitive ( w f -- )
		FXtSetSensitive drop
	;

	<'> FXtIsManaged alias is-managed? ( wid -- f )

	: change-label ( wid new-label -- )
		doc" Change WIDGET's label to be NEW-LABEL."
		{ wid new-label }
		new-label FXmStringCreateLocalized { str }
		wid #( FXmNlabelString str ) FXtVaSetValues drop
		str FXmStringFree drop
	;

	: for-each-child { wid prc -- }
		doc" Apply PRC to WIDGET and each of its children."
		prc #( wid ) run-proc drop
		wid FXtIsComposite if
			wid #( FXmNchildren 0 )
			    FXtVaGetValues 1 array-ref each ( w )
				prc recurse
			end-each
		then
	;

	: main-dpy ( -- dpy )
		top-level-shell main-widgets-ref FXtDisplay
	;

	: load-font ( name -- fid|#f )
		{ name }
		main-dpy name FXLoadQueryFont { fs }
		fs FXFontStruct? if
			fs Ffid
		else
			#f
		then 
	;

	: current-screen ( -- scr )
		doc" Return the current X screen number of the current display."
		main-dpy FDefaultScreenOfDisplay
	;

	: white-pixel ( -- pix )   current-screen FWhitePixelOfScreen ;
	: black-pixel ( -- pix )   current-screen FBlackPixelOfScreen ;
	: screen-depth ( -- n )   current-screen FDefaultDepthOfScreen ;

	\ --- apply func to every widget belonging to w ---

	hide
	: children->array-cb ( ary -- prc; child self -- )
		{ ary }
		1 proc-create ( prc )
		ary ,
  	  does> { child self -- }
		self @ ( ary ) child array-push drop
	;
	set-current

	: children->array ( widget -- array )
		#() { ary }
		( widget ) ary children->array-cb for-each-child
		ary
	;
	previous

	: find-child ( widget name -- wid )
		doc" Return a widget named NAME, if one can be found in the \
		widget hierarchy beneath WIDGET."
		{ widget name }
		#f
		widget children->array each { w }
			w FXtName name string= if
				not
				w swap
				leave
			then
		end-each unless
			'no-such-widget
			    #( "%s: %S" get-func-name name ) fth-throw
		then
	;

	: widget-exists? { widget name -- f }
		#f ( flag ) widget children->array each ( w )
			FXtName name string= if
				( flag ) not leave
			then
		end-each ( flag )
	;

	: main-widget-exists? { name -- f }
		top-level-shell main-widgets-ref name widget-exists?
	;

	hide
	: display-widget <{ widget n -- }>
		widget FXtName empty? if
			"<unnamed>"
		else
			widget FXtName
		then
		n spaces .string cr
		widget FXtIsComposite if
			widget #( FXmNchildren 0 )
			    FXtVaGetValues 1 array-ref each ( w )
				n 2 + recurse
			end-each
		then
	;
	set-current

	: display-widget-tree { widget -- }
		doc" Display the hierarchy of widgets beneath WIDGET."
		<'> display-widget #( widget 0 ) run-proc drop
	;
	previous

	hide
	: change-color-cb <{ w -- }>
		w FXtIsWidget if
			w FXmIsScrollBar if
				w position-color FXmChangeColor drop
			else
				w basic-color FXmChangeColor drop
			then
		then
	;
	set-current

	: set-main-color-of-widget ( widget -- )
		doc" Set the background color of WIDGET."
		<'> change-color-cb for-each-child
	;
	previous

	: host-name ( -- host )
		doc" Return name of current machine."
		top-level-shell main-widgets-ref { wid }
		wid FXtWindow { win }
		main-dpy win main-dpy "WM_CLIENT_MACHINE" #f
		    FXInternAtom 0 32 #f FXA_STRING FXGetWindowProperty { host }
		host if
			host 5 array-ref
		else
			host
		then
	;

	\ --- add our own pane to the channel section ---
	\ 
	\ 0 0 "new-pane" FxmDrawingAreaWidgetClass
	\ #( FXmNbackground graph-color FXmNforeground data-color )
	\   add-channel-pane value draw-widget

	: add-channel-pane { snd chn name typ args -- wid }
		name typ snd chn cw-edhist channel-widgets-ref
		    FXtParent FXtParent args FXtCreateManagedWidget
	;

	\ --- add our own pane to the sound section (underneath the controls
	\     in this case) ---

	: add-sound-pane { snd name typ args -- wid }
		name typ snd sw-main-pane sound-widgets-ref args
		    undef FXtCreateManagedWidget
	;

	\ --- add our own pane to the overall Snd window (underneath the
	\     listener in this case) ---

	: add-main-pane { name class args -- wid }
		notebook-outer-pane main-widgets-ref dup unless
			drop main-sound-pane main-widgets-ref
		then { parent }
		name class parent args undef FXtCreateManagedWidget
	;

	\ --- add a widget at the top of the listener ---

	: add-listener-pane { name typ args -- wid }
		top-level-shell main-widgets-ref
		    "lisp-listener" find-child { listener }
		listener FXtParent { listener-scroll }
		listener-scroll FXtParent { listener-form }
		listener-scroll FXtUnmanageChild drop
		args
		    #( FXmNleftAttachment  FXmATTACH_FORM
		       FXmNrightAttachment FXmATTACH_FORM
		       FXmNtopAttachment   FXmATTACH_FORM ) array-append to args
		name typ listener-form args
		    undef FXtCreateManagedWidget { top-widget }
		listener-scroll
		    #( FXmNtopAttachment FXmATTACH_WIDGET
		       FXmNtopWidget top-widget ) FXtVaSetValues drop
		listener-scroll FXtManageChild drop
		top-widget
	;

	\ --- bring possibly-obscured dialog to top ---

	: raise-dialog ( dialog -- )
		{ w }
		w FWidget? if
			w FXtIsManaged if
				w FXtParent { parent }
				parent FWidget? if
					parent FxmDialogShellWidgetClass
					    FXtIsSubclass if
						parent FXtGrabNone
						    FXtPopup drop
					then
				then
			then
		then
	;

	: activate-dialog ( dialog -- )
		dup FXtIsManaged if
			raise-dialog
		else
			FXtManageChild drop
		then
	;

	\ --- add-mark-pane ---

	#f value including-mark-pane

	hide
	#() value mark-list-lengths
	#() value mark-lists

	: find-mark-list { snd chn dats -- lst }
		#f		\ flag
		dats each { dat }
			snd dat 0 array-ref =
			chn dat 1 array-ref = && if
				drop	\ drop flag
				dat 2 array-ref
				leave
			then
		end-each
	;

	: mark-list-length ( snd chn -- len )
		mark-list-lengths find-mark-list dup unless
			drop 0
		then
	;

	: set-mark-list-length { snd chn len -- }
		mark-list-lengths each { dat }
			snd dat 0 array-ref =
			chn dat 1 array-ref = && if
				mark-list-lengths i array-delete! drop
				leave
			then
		end-each
		mark-list-lengths #( snd chn len ) array-push drop
	;

	: mark-list ( snd chn -- lst )
		mark-lists find-mark-list dup if
			2 array-ref
		else
			drop #()
		then
	;

	: set-mark-list { snd chn lst -- }
		mark-lists #( snd chn lst ) array-push drop
	;

	: deactivate-channel { snd chn -- }
		snd chn mark-list-length 0>
		snd chn mark-list FWidget? && if
			snd chn mark-list #( FXmNchildren 0 )
			    FXtVaGetValues 1 array-ref each ( w )
				FXtUnmanageChild drop
			end-each
		then
	;

	: marks-focus-cb <{ w c i -- f }>
		w #( FXmNbackground white-pixel ) FXtVaSetValues
	;

	: marks-losing-focus-cb <{ w c i -- f }>
		w #( FXmNbackground basic-color ) FXtVaSetValues
	;

	: marks-activate-cb <{ w c info -- }>
		w #( FXmNuserData 0 ) FXtVaGetValues 1 array-ref { id }
		w #( FXmNvalue 0 )    FXtVaGetValues 1 array-ref { txt }
		txt string?
		txt length 0> && if
			txt string->number
		else
			#f
		then
		{ samp }
		samp if
			id samp set-mark-sample
		else
			id delete-mark
		then drop
		w #( FXmNbackground basic-color ) FXtVaSetValues drop
	;

	: marks-enter-cb <{ w c i f -- f }>
		mouse-enter-text-hook #( w ) run-hook
	;

	: marks-leave-cb <{ w c i f -- f }>
		mouse-leave-text-hook #( w ) run-hook
	;

	: make-mark-list { snd chn -- }
		snd chn mark-list-length { cur-len }
		snd chn deactivate-channel
		snd chn mark-list FWidget? unless
			snd chn "mark-box" FxmFormWidgetClass
			    #( FXmNbackground       basic-color
			       FXmNorientation      FXmVERTICAL
			       FXmNpaneMinimum      100
			       FXmNbottomAttachment FXmATTACH_FORM )
			    add-channel-pane { mark-box }
			mark-box "Marks"
			    #( FXmNbackground       highlight-color
			       FXmNleftAttachment   FXmATTACH_FORM
			       FXmNrightAttachment  FXmATTACH_FORM
			       FXmNalignment        FXmALIGNMENT_CENTER
			       FXmNtopAttachment    FXmATTACH_FORM )
			    FXmVaCreateManagedLabel { mark-label }
			mark-box "mark-scroller"
			    #( FXmNbackground       basic-color
			       FXmNscrollingPolicy  FXmAUTOMATIC
			       FXmNscrollBarDisplayPolicy FXmSTATIC
			       FXmNleftAttachment   FXmATTACH_FORM
			       FXmNrightAttachment  FXmATTACH_FORM
			       FXmNtopAttachment    FXmATTACH_WIDGET
			       FXmNtopWidget        mark-label
			       FXmNbottomAttachment FXmATTACH_FORM )
			    FXmVaCreateManagedScrolledWindow { mark-scroller }
			mark-scroller "mark-list"
			    #( FXmNorientation      FXmVERTICAL
			       FXmNtopAttachment    FXmATTACH_FORM
			       FXmNbottomAttachment FXmATTACH_FORM
			       FXmNspacing          0 )
			    FXmVaCreateManagedRowColumn { mlist }
			mark-scroller set-main-color-of-widget
			mark-box #( FXmNpaneMinimum 1 ) FXtVaSetValues drop
			snd chn #( snd chn mlist ) set-mark-list
		then
		snd chn #f marks { new-marks }
		new-marks length cur-len > if
			snd chn mark-list { lst }
			new-marks length cur-len ?do
				"field" FxmTextFieldWidgetClass lst
				    #( FXmNbackground basic-color )
				    undef FXtCreateWidget { tf }
				tf FXmNfocusCallback
				    <'> marks-focus-cb
				    undef FXtAddCallback drop
				tf FXmNlosingFocusCallback
				    <'> marks-losing-focus-cb
				    undef FXtAddCallback drop
				tf FXmNactivateCallback
				    <'> marks-activate-cb
				    undef FXtAddCallback drop
				tf FEnterWindowMask #f
				    <'> marks-enter-cb
				    undef FXtAddEventHandler drop
				tf FLeaveWindowMask #f
				    <'> marks-leave-cb
				    undef FXtAddEventHandler drop
			loop
		then
		snd chn new-marks length set-mark-list-length
		snd chn mark-list #( FXmNchildren 0 )
		FXtVaGetValues 1 array-ref each { wid }
			new-marks empty? ?leave
			wid FXmIsTextField if
				wid #( FXmNvalue
				       new-marks car undef mark-sample
				       number->string
				       FXmNuserData
				       new-marks car ) FXtVaSetValues drop
				wid FXtManageChild drop
				new-marks array-shift to new-marks
			then
		end-each
		#f
	;

	: remark <{ id snd chn reason -- }>
		snd chn make-mark-list
	;

	: unremark <{ snd -- }>
		snd channels 0 ?do
			snd i deactivate-channel
		loop
	;

	: marks-edit-cb { snd chn -- prc; self -- }
		0 proc-create ( prc )
		chn , snd ,
	  does> { self -- }
		self @ { chn }
		self cell+ @ { snd }
		snd chn mark-list FWidget? if
			snd chn make-mark-list
		then
	;

	: open-remarks <{ snd -- }>
		snd channels 0 ?do
			snd i after-edit-hook snd i marks-edit-cb add-hook!
			snd i undo-hook       snd i marks-edit-cb add-hook!
		loop
	;

	: marks-update-proc <{ snd -- }>
		snd channels 0 ?do
			snd i make-mark-list
		loop
	;

	: marks-update-cb <{ snd -- proc }>
		snd <'> marks-update-proc
	;
	set-current

	: add-mark-pane ( -- )
		#t to including-mark-pane
		mark-hook       <'> remark          add-hook!
		close-hook      <'> unremark        add-hook!
		after-open-hook <'> open-remarks    add-hook!
		update-hook     <'> marks-update-cb add-hook!
	;
	previous

	\ --- show-disk-space, Motif specific ---

	hide
	: show-label <{ data id -- }>
		data 0 array-ref empty? unless
			data 0 array-ref { snd }
			data 1 array-ref { wid }
			data 2 array-ref { app }
			snd sound? if
				wid snd file-name disk-kspace kmg change-label
			then
			app 10000 running-word data FXtAppAddTimeOut drop
		then
	;
	set-current

	: show-disk-space <{ snd -- }>
		doc" Add a label to the minibuffer area showing the current \
free space (for use with after-open-hook)."
		#f ( flag )
		labelled-snds each { n }
			n 0 array-ref snd equal? if
				( flag ) drop
				n
				leave
			then
		end-each { previous-label }
		previous-label if
			exit
		then
		snd sound? unless
			"no sound found for disk space label" snd-error drop
			exit
		then
		#t to showing-disk-space
		top-level-application main-widgets-ref { app }
		snd sw-minibuffer sound-widgets-ref { minibuffer }
		snd sw-unite      sound-widgets-ref { unite-button }
		snd sw-sync       sound-widgets-ref { sync-button }
		minibuffer FXtParent { name-form }
		snd file-name disk-kspace kmg FXmStringCreateLocalized { str }
		minibuffer FXtUnmanageChild drop
		minibuffer #( FXmNrightAttachment FXmATTACH_NONE )
		    FXtVaSetValues drop
		name-form "space"
		    #( FXmNbackground      basic-color
		       FXmNleftAttachment  FXmATTACH_NONE
		       FXmNlabelString     str
		       FXmNrightAttachment FXmATTACH_WIDGET
		       FXmNrightWidget
		       unite-button FXtIsManaged if
			       unite-button
		       else
			       sync-button
		       then
		       FXmNtopAttachment   FXmATTACH_FORM )
		    FXmVaCreateManagedLabel { new-label }
		minibuffer
		    #( FXmNrightWidget new-label
		       FXmNrightAttachment FXmATTACH_WIDGET )
		    FXtVaSetValues drop
		minibuffer FXtManageChild drop
		str FXmStringFree drop
		#( snd new-label app ) to previous-label
		labelled-snds previous-label array-push drop
		app 10000 <'> show-label previous-label FXtAppAddTimeOut drop
	;
	previous

	: current-label ( widget -- label )
		doc" Return WIDGET's label."
		( wid ) #( FXmNlabelString 0 ) FXtVaGetValues
		    1 array-ref ( xmstr ) #f FXmCHARSET_TEXT
		    FXmCHARSET_TEXT #f 0 FXmOUTPUT_ALL FXmStringUnparse
	;
[then]

\ snd-xm.fs ends here
