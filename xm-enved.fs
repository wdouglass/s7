\ xm-enved.fs -- xm-enved.scm -> xm-enved.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/10/21 18:22:57
\ Changed: 20/09/13 13:40:44
\
\ @(#)xm-enved.fs	1.38 9/13/20

\ Commentary:
\
\ Requires --with-motif
\
\ Tested with Snd 20.x
\             Fth 1.4.x
\             Motif 2.3.3 X11R6
\
\ This is an example of an object type written in Forth.
\
\ XENVED
\
\ before-enved-hook	( xe pos x y reason -- f )
\ xenved?     	  	( obj -- f )
\ make-xenved 	  	( name parent :key envelope axis-bounds args -- xe )
\
\ xenved-length		( xe -- len|-1 )
\ xenved-ref		( xe index -- point )
\ xenved-set!		( xe index point -- )
\ xenved-index		( xe x -- index|-1 )
\ xenved-insert!	( xe index point -- )
\ xenved-delete!	( xe index -- )
\
\ xenved-envelope@	( xe -- ary )
\ xenved-envelope!	( xe ary -- )
\ xenved-open		( xe -- )
\ xenved-close		( xe -- )
\
\ xe-envelope		( xe -- ary )
\ set-xe-envelope	( xe ary -- )
\
\ xenved-test           ( name -- xe )

'snd-nogui provided? [if] skip-file [then]

require enved
require snd-xm

\ === XENVED OBJECT TYPE ===

5 "( xe pos x y reason -- f )  \
Will be called before changing a breakpoint in XE's envelope.  \
This hook runs the global ENVED-HOOK at first, \
subsequent procedures can directly manipulate XE's envelope \
or the returned array of the preceding hook procedure.\n\
This instance hook is like the global ENVED-HOOK; \
POS is ENVELOPE's x-position, X and Y are the new points, \
and REASON is one of the Snd constants ENVED-ADD-POINT, \
ENVED-DELETE-POINT, ENVED-MOVE-POINT.  \
If one of the hook procedures in the hook array returns #f, \
xenved changes the breakpoint, \
otherwise the last hook procedure is responsible for manipulating \
XE's envelope itself." create-hook before-enved-hook

\
\ Example hook function:
\ Applies Snd's enved-hook functions to xenved.
\
\ before-enved-hook lambda: <{ xe pos x y reason -- f }>
\ 	enved-hook hook-empty? if
\ 		#f
\ 	else
\ 		xe xenved-envelope@ { res }
\ 		enved-hook each { prc }
\ 			prc #( res pos x y reason ) run-proc to res
\ 			res false? ?leave
\ 		end-each
\		res array? if
\ 			xe res xenved-envelope!
\		then
\ 		res
\ 	then
\ ; add-hook!

"xenved" make-object-type constant fth-xenved
fth-xenved make-?obj xenved?

hide
#( "xe-enved"
   "xe-name"
   "xe-parent"
   "xe-args"
   "xe-drawer"
   "xe-gcs"
   "xe-bx0"
   "xe-bx1"
   "xe-by0"
   "xe-by1"
   "xe-px0"
   "xe-px1"
   "xe-py0"
   "xe-py1"
   "xe-mouse-up"
   "xe-mouse-down"
   "xe-mouse-pos"
   "xe-mouse-new"
   "xe-click-time"
   "xe-dragging" ) create-instance-struct make-xenved-instance

: axis-bounds? ( obj -- f )
  array-length 4 =
;

: xe-length ( self -- len )   xe-enved@ enved-length ;

: xe-inspect { self -- str }
	"#<%s[%d]: axis-bounds: #( %s %s %s %s ), envelope: %s>"
	    #( self object-name
	       self xe-length
	       self xe-bx0@
	       self xe-bx1@
	       self xe-by0@
	       self xe-by1@
	       self xe-enved@ ) string-format
;

: xe->string ( self -- str )   xe-enved@ object->string ;  

: xe-dump { self -- str }
	"%S %S :envelope %s :axis-bounds #( %s %s %s %s ) :args %S make-xenved"
	    #( self xe-name@
	       self xe-parent@
	       self xe-enved@
	       self xe-bx0@
	       self xe-bx1@
	       self xe-by0@
	       self xe-by1@
	       self xe-args@ ) string-format
;

: xe->array ( self -- ary )   xe-enved@ object->array ;
: xe-ref ( self index -- point )   swap xe-enved@ swap enved-ref ;
: xe-set! ( self index point -- )   rot xe-enved@ -rot enved-set! ;

: xe-equal? { self obj -- f }
	self obj = if
		#t
	else
		self xe-enved@ obj xe-enved@ object-equal?
	then
;

\ Init object type Xenved.
<'> xe-inspect fth-xenved set-object-inspect	\ xe .inspect
<'> xe->string fth-xenved set-object->string	\ xe object->string
<'> xe-dump    fth-xenved set-object-dump	\ xe object-dump
<'> xe->array  fth-xenved set-object->array	\ xe object->array
<'> xe-ref     fth-xenved set-object-value-ref	\ xe idx object-ref => #( x y )
<'> xe-set!    fth-xenved set-object-value-set	\ xe idx #( x y ) object-set!
<'> xe-equal?  fth-xenved set-object-equal-p	\ obj1 obj2 object-equal?
<'> xe-length  fth-xenved set-object-length	\ xe object-length => (lstlen/2)
<'> xe-ref     fth-xenved 1 set-object-apply	\ xe idx apply => #( x y )

: run-before-enved-hook { xe point reason -- f }
	xe xenved? xe 1 "an xenved" assert-type
	point array-length 2 = point 1 "an array #( x y )" assert-type
	before-enved-hook hook-empty? if
		#t
	else
		#f ( flag )
		before-enved-hook each { prc }
			prc #( xe
			       xe xe-mouse-pos@
			       point 0 array-ref
			       point 1 array-ref
			       reason ) run-proc false? if
				not ( toggle flag )
				leave
			then
		end-each
	then
;
set-current

: xenved-length { xe -- len|-1 }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-length
;

: xenved-ref { xe index -- point }
	xe xenved? xe 1 "an xenved" assert-type
	xe index xe-ref
;

: xenved-set! { xe index point -- }
	xe xenved? xe 1 "an xenved" assert-type
	xe index point xe-set!
;

: xenved-index { xe x -- index|-1 }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-enved@ x enved-index
;

: xenved-insert! { xe index point -- }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-enved@ index point enved-insert!
;

: xenved-delete! { xe index -- }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-enved@ index enved-delete!
;
previous

hide
0.03 constant mouse-radius

: grfx { xe x -- n }
	xe xe-px0@ xe xe-px1@ = if
		xe xe-px0@
	else
		xe xe-px0@ { px0 }
		xe xe-px1@ { px1 }
		xe xe-bx0@ { bx0 }
		xe xe-bx1@ { bx1 }
		x bx0 f-
		    bx1 bx0 f- f/
		    px1 px0 f- f*
		    px0 f+ floor f>s
		    px0 max
		    px1 min
	then
;

: grfy { xe y -- n }
	xe xe-py0@ xe xe-py1@ = if
		xe xe-py0@
	else
		xe xe-py0@ { py0 }
		xe xe-py1@ { py1 }
		xe xe-by0@ { by0 }
		xe xe-by1@ { by1 }
		y by1 f-
		    by0 by1 f- f/
		    py0 py1 f- f*
		    py1 f+ floor f>s
		    py1 max
		    py0 min
	then
;

: ungrfx { xe x -- r }
	xe xe-px0@ xe xe-px1@ = if
		xe xe-bx0@ s>f
	else
		xe xe-px0@ { px0 }
		xe xe-px1@ { px1 }
		xe xe-bx0@ { bx0 }
		xe xe-bx1@ { bx1 }
		x px0 f-
		    px1 px0 f- f/
		    bx1 bx0 f- f*
		    bx0 f+
		    bx0 fmax
		    bx1 fmin
	then
;

: ungrfy { xe y -- r }
	xe xe-py0@ xe xe-py1@ = if
		xe xe-by1@ s>f
	else
		xe xe-py0@ { py0 }
		xe xe-py1@ { py1 }
		xe xe-by0@ { by0 }
		xe xe-by1@ { by1 }
		py0 y f-
		    py0 py1 f- f/
		    by1 by0 f- f*
		    by0 f+
		    by0 fmax
		    by1 fmin
	then
;

360 64 * constant 360*64

: xe-redraw { xe -- }
	xe xe-drawer@ { drawer }
	drawer is-managed?
	xe xe-py0@ xe xe-py1@ > && if
		xe xe-gcs@ { gc }
		drawer FXtDisplay { dpy }
		drawer FXtWindow { win }
		dpy win FXClearWindow drop
		\ Motif's DRAW-AXES takes 6 optional arguments.
		\ '( x0 y0 x1 y1 ) = draw-axes(wid gc label
		\                        x0=0.0 x1=1.0 y0=-1.0 y1=1.0
		\                        style=x-axis-in-seconds
		\                        axes=show-all-axes)
		\ arity #( 3 6 #f )
		drawer gc xe xe-name@
		    xe xe-bx0@ xe xe-bx1@ xe xe-by0@ xe xe-by1@
		    x-axis-in-seconds show-all-axes draw-axes drop
		#f #f { lx ly }
		10 { mouse-d }
		5  { mouse-r }
		xe each { point }
			xe point 0 array-ref grfx { cx }
			xe point 1 array-ref grfy { cy }
			dpy win gc cx mouse-r - cy mouse-r -
			    mouse-d mouse-d 0 360*64 FXFillArc drop
			lx if
				dpy win gc lx ly cx cy FXDrawLine drop
			then
			cx to lx
			cy to ly
		end-each
	then
;

: add-envelope-point { xe x y -- }
	xe xe-mouse-pos@ { mpos }
	xe x xenved-index dup 0>= if
		to mpos
	else
		drop
		xe each
			0 array-ref x f> if
				i to mpos
				leave
			then
		end-each
	then
	xe mpos #( x y ) xenved-insert!
	xe mpos xe-mouse-pos!
;

: draw-axes-set-points { lst xe -- }
	xe lst 0 array-ref xe-px0!
	xe lst 1 array-ref xe-py0!
	xe lst 2 array-ref xe-px1!
	xe lst 3 array-ref xe-py1!
	xe xe-redraw
;

: mouse-press { xe xx yy -- }
	xe xx ungrfx { x }
	xe yy ungrfy { y }
	#f ( flag )
	xe each { point }
		point 0 array-ref x f- fabs mouse-radius f<
		point 1 array-ref y f- fabs mouse-radius f< && if
			drop ( flag )
			i ( flag is now pos )
			leave
		then
	end-each { pos }
	xe pos not xe-mouse-new!
	xe time xe-mouse-down!
	pos number? if
		xe pos xe-mouse-pos!
	else
		xe #( x y ) enved-add-point run-before-enved-hook if
			xe x y add-envelope-point
		then
		xe xe-redraw
	then
;

: mouse-release { xe -- }
	xe xe-mouse-pos@ { mpos }
	xe time xe-mouse-up!
	xe xe-mouse-new@ unless
		xe xe-mouse-up@ xe xe-mouse-down@ f-
		xe xe-click-time@ f<= if
			mpos 0<> if
				mpos xe xenved-length 1- < if
					xe mpos xenved-ref { point }
					xe point enved-delete-point
					    run-before-enved-hook if
						xe mpos xenved-delete!
					then
					xe xe-redraw
				then
			then
		then
	then
	xe #f xe-mouse-new!
;

: mouse-drag { xe xx yy -- }
	xe xx ungrfx { x }
	xe yy ungrfy { y }
	xe xe-mouse-pos@ { mpos }
	mpos 0= if
		xe 0 xenved-ref 0 array-ref
	else
		mpos xe xenved-length 1- >= if
			xe -1 xenved-ref 0 array-ref
		else
			xe mpos 1- xenved-ref 0 array-ref
			xe mpos 1+ xenved-ref 0 array-ref  x fmin fmax
		then
	then to x
	xe #( x y ) enved-move-point run-before-enved-hook if
		xe mpos #( x y ) xenved-set!
	then
	xe xe-redraw
;

: draw-axes-cb <{ w xe info -- }>
	xe xe-drawer@ xe xe-gcs@ xe xe-name@
	    xe xe-bx0@ xe xe-bx1@ xe xe-by0@
	    x-axis-in-seconds show-all-axes draw-axes
	    xe draw-axes-set-points
;

: mouse-press-cb <{ w xe ev f -- }>
	xe ev Fx ev Fy mouse-press
;

: mouse-release-cb <{ w xe ev f -- }>
	xe mouse-release
;

: mouse-drag-cb <{ w xe ev f -- }>
	xe ev Fx ev Fy mouse-drag
;

: define-cursor-cb <{ w xe ev f -- x }>
	w FXtDisplay w FXtWindow xe FXDefineCursor
;

: undefine-cursor-cb <{ w xe ev f -- x }>
	w FXtDisplay w FXtWindow FXUndefineCursor
;

: make-drawer { name parent args -- drawer }
	args FXmNbackground array-member? unless
		args FXmNbackground array-push graph-color
		    array-push to args
	then
	args FXmNforeground array-member? unless
		args FXmNforeground array-push data-color
		    array-push to args
	then
	parent name args FXmVaCreateManagedDrawingArea ( drawer )
;

: init-xenved-cbs { xe -- }
	xe xe-drawer@ { drawer }
	drawer FXtDisplay FXC_crosshair
	    FXCreateFontCursor { arrow-cursor }
	drawer FXmNresizeCallback <'> draw-axes-cb xe
	    FXtAddCallback drop
	drawer FXmNexposeCallback <'> draw-axes-cb xe
	    FXtAddCallback drop
	drawer FButtonPressMask #f <'> mouse-press-cb xe
	    FXtAddEventHandler drop
	drawer FButtonReleaseMask #f <'> mouse-release-cb xe
	    FXtAddEventHandler drop
	drawer FButtonMotionMask #f <'> mouse-drag-cb xe
	    FXtAddEventHandler drop
	drawer FEnterWindowMask #f <'> define-cursor-cb arrow-cursor
	    FXtAddEventHandler drop
	drawer FLeaveWindowMask #f <'> undefine-cursor-cb #f
	    FXtAddEventHandler drop
;
set-current

: make-xenved <{ name parent
    :key
    envelope    #( 0.0 0.0 1.0 1.0 )
    axis-bounds #( 0.0 1.0 0.0 1.0 )
    args        #() -- xe }>
	parent widget? parent 2 "a widget" assert-type
	axis-bounds axis-bounds? axis-bounds 4
	    "an array of axis bounds" assert-type
	fth-xenved make-xenved-instance { xe }
	xe envelope make-enved xe-enved!
	name string? unless
		"xe-test" to name
	then
	xe name parent args make-drawer xe-drawer!
	xe name   xe-name!
	xe parent xe-parent!
	xe args   xe-args!
	xe snd-gcs 0 array-ref xe-gcs!
	xe axis-bounds 0 array-ref xe-bx0!
	xe axis-bounds 2 array-ref xe-by0!
	xe axis-bounds 1 array-ref xe-bx1!
	xe axis-bounds 3 array-ref xe-by1!
	xe 0 xe-px0!		\ points == ints
	xe 0 xe-py0!
	xe 0 xe-px1!
	xe 0 xe-py1!
	xe 0.0 xe-mouse-up!
	xe 0.0 xe-mouse-down!
	xe 0.5 xe-click-time!
	xe 0   xe-mouse-pos!
	xe #f  xe-mouse-new!
	xe #f  xe-dragging!
	xe init-xenved-cbs
	xe
;

: xenved-envelope@ { xe -- ary }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-enved@ enved-envelope@
;


: xenved-envelope! { xe ary -- }
	xe xenved? xe 1 "an xenved" assert-type
	ary array?  ary 2 "an array"  assert-type
	xe xe-enved@ ary enved-envelope!
;

<'> xenved-envelope@ alias xe-envelope

: set-xe-envelope { xe ary -- }
	xe ary xenved-envelope!
	xe xe-redraw
;

\ XXX
\ For backwards compatibility.
<'> xenved-envelope@ alias xe-envelope@

\ XXX
\ For backwards compatibility with arguments swapped.
: xe-envelope! { ary xe -- }
	xe ary xenved-envelope!
;

: xenved-open { xe -- }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-drawer@ widget? if
		xe xe-drawer@ show-widget drop
	then
	xe xe-redraw
;

: xenved-close { xe -- }
	xe xenved? xe 1 "an xenved" assert-type
	xe xe-drawer@ widget? if
		xe xe-drawer@ hide-widget drop
	then
;
previous

#f value test-widget-type
#f value test-widget-args
#f value test-xenved-args

FxmFormWidgetClass  to test-widget-type
#( FXmNheight 200 ) to test-widget-args
#( FXmNleftAttachment   FXmATTACH_WIDGET
   FXmNtopAttachment    FXmATTACH_WIDGET
   FXmNbottomAttachment FXmATTACH_WIDGET
   FXmNrightAttachment  FXmATTACH_WIDGET ) to test-xenved-args

: xenved-test <{ :optional name "xenved" -- xe }>
	doc" create a drawing test widget\n\
xenved-test value xe\n\
xe             => #( 0.0 0.0 1.0 1.0 )\n\
xe xe-envelope => #( 0.0 0.0 1.0 1.0 )\n\
\\ some clicks later\n\
xe xe-envelope => #( 0.0 0.0\n\
                     0.190736 0.562264\n\
                     0.632152 0.932075\n\
                     0.848774 0.316981\n\
                     1.0 1.0 )\n\
xe #( 0 1 1 1 ) set-xe-envelope\n\
xe xe-envelope => #( 0 1 1 1 )\n\
xe xe-close."
	name
	    name test-widget-type test-widget-args add-main-pane
	    :envelope    #( 0.0 0.0 1.0 1.0 )
	    :axis-bounds #( 0.0 1.0 0.0 1.0 )
	    :args test-xenved-args make-xenved ( xe )
;

\ xm-enved.fs ends here
