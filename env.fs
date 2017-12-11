\ env.fs -- env.scm -> env.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/10/27 04:51:42
\ Changed: 17/12/02 03:19:40
\
\ @(#)env.fs	1.26 12/2/17

\ From env.scm|rb with original comments and doc strings from env.scm.
\ 
\ envelope?		( obj -- f )
\ envelope-copy		( env1 -- env2 )
\ 
\ envelope-interp	( x env :optional base -- r )
\ interp		( x env -- r )
\ window-envelope	( beg end en1 -- en2 )
\ map-envelopes		( env1 env2 xt -- env3 )
\ add-envelopes		( env1 env2 -- env3 )      alias envelopes+
\ multiply-envelopes	( env1 env2 -- env3 )      alias envelopes*
\ max-envelope		( env -- r )
\ min-envelope		( env -- r )
\ integrate-envelope	( en -- r )
\ envelope-length	( env -- n )
\ envelope-last-x	( env -- r )
\ stretch-envelope	( env oa na :optional old-decay new-decay -- new-env )
\ scale-envelope	( env1 scl :offset offset -- env2 )
\ reverse-envelope	( env1 -- env2 )
\ envelope-concatenate	( en1 ... enn n -- ne )
\ concatenate-envelopes	( en1 ... enn n -- ne )
\ repeat-envelope	( ur-env reps :optional reflected normalized -- en )
\
\ make-power-env	( envelope :key scaler offset duration -- pe )
\ power-env		( pe -- val )
\ power-env-channel	( pe :optional beg dur snd chn edpos edname -- curb )
\ powenv-channel	( envelope :optional beg dur snd chn edpos -- val )
\ envelope-exp		( en1 :optional power xgrid -- en2 )
\ rms-envelope		( file :key beg dur rfreq db -- en )
\
\ normalize-envelope	( env1 -- env2 )

require clm

: envelope? ( obj -- f )
	doc" Return #t if OBJ is a vct or an array \
with even length and length >= 2."
	( obj ) length dup 2 mod 0= swap 2 >= &&
;

<'> object-copy alias envelope-copy
<'> envelope-copy
"( en1 -- en2 )  Copy EN1, which may be a vct or an array." help-set!

\ ;;; -------- envelope-interp

: envelope-interp <{ x en :optional base 1.0 -- r }>
	doc" Return value of ENV at X; \
BASE controls connecting segment type; \
ENV may be a vct, or an array:\n\
0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) 1.0 envelope-interp => 0.6."
	en empty? if
		0.0
	else
		en length { size }
		x en 0 object-ref f<=
		size 2 <= || if
			en 1 object-ref
		else
			en 2 object-ref x f> if
				en 1 object-ref en 3 object-ref f=
				base f0= || if
					en 1 object-ref
				else
					base 1.0 f= if
						en 1 object-ref
						    x en 0 object-ref f-
						    en 3 object-ref
						    en 1 object-ref f-
						    en 2 object-ref
						    en 0 object-ref f- f/
						    f* f+
					else
						en 1 object-ref
						    en 3 object-ref
						    en 1 object-ref f-
						    base 1.0 f- f/ base x
						    en 0 object-ref f-
						    en 2 object-ref
						    en 0 object-ref f- f/
						    f** 1.0 f- f* f+
					then
				then
			else
				x size 2 ?do
					en i object-ref
				loop
				size 2 - >array base recurse ( envelope-interp )
			then
		then
	then
;

: interp ( x env -- r )
	doc" 0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) interp => 0.6."
	1.0 envelope-interp
;

\ ;;; -------- window-envelope (a kinda brute-force translation from the CL version in env.lisp)

: window-envelope ( beg end en1 -- en2 )
	doc" Return portion of EN1 lying between x axis values BEG and END:\n\
1.0 3.0 #( 0.0 0.0 5.0 1.0 ) window-envelope => #( 1.0 0.2 3.0 0.6 )."
	{ beg end en1 }
	#() { en2 }
	en1 length 1 > if
		en1 1 array-ref
	else
		0.0
	then { lasty }
	#f { return-early? }
	en1 length 0 ?do
		en1 i    array-ref { x }
		en1 i 1+ array-ref { y }
		y to lasty
		en2 empty? if
			x beg f>= if
				en2 beg array-push
				    beg en1 1.0 envelope-interp
				    array-push to en2
				x beg f<> if
					x end f>= if
						en2 end array-push
						    end en1 1.0
						    envelope-interp
						    array-push to en2
						#t to return-early?
						leave
					else
						en2 #( x y ) array-push to en2
					then
				then
			then
		else
			x end f<= if
				en2 x array-push y array-push to en2
				x end f= if
					#t to return-early?
					leave
				then
			else
				x end f> if
					en2 end array-push
					    end en1 1.0 envelope-interp
					    array-push to en2
					#t to return-early?
					leave
				then
			then
		then
	2 +loop
	return-early? unless
		en2 end array-push lasty array-push to en2
	then
	en2
;

\ ;;; -------- map-envelopes like map-across-envelopes in env.lisp

hide
: fnumb-cmp ( a b -- -1|0|1 )
	{ a b }
	a b f< if
		-1
	else
		a b f= if
			0
		else
			1
		then
	then
;

: (at0) ( en xs-array -- en' )
	{ en xs }
	en 0 object-ref { diff }
	en -2 object-ref { lastx }
	en length 0 ?do
		en i object-ref diff f-  lastx f/  ( x )
		    xs over array-push drop  en i rot object-set!
	2 +loop
	en
;
set-current

: map-envelopes ( en1 en2 xt -- en3 )
	doc" Map XT over the breakpoints in EN1 and EN2 \
returning a new envelope."
	{ en1 en2 xt }
	#() { xs }
	#() { en3 }
	en1 empty? if
		en2 xs (at0) to en3
	else
		en2 empty? if
			en1 xs (at0) to en3
		else
			en1 xs (at0) { ee1 }
			en2 xs (at0) { ee2 }
			xs array-uniq! <'> fnumb-cmp array-sort! drop
			xs length 2* :initial-element 0.0 make-array to en3
			xs each { x }
				en3 i 2* x array-set!
				x ee1 1.0 envelope-interp
				    x ee2 1.0 envelope-interp
				    xt execute en3 i 2* 1+ rot array-set!
			end-each
		then
	then
	en1 array? if
		en3
	else
		en1 vct? if
			en3 vector->vct
		then
	then
;
previous

\ ;;; -------- multiply-envelopes, add-envelopes

: add-envelopes ( env1 env2 -- env3 )
	doc" Add break-points of ENV1 and ENV2 returning a new envelope."
	<'> f+ map-envelopes
;

: multiply-envelopes ( env1 env2 -- env3 )
	doc" Multiply break-points of ENV1 and ENV2 \
returning a new envelope:\n\
#( 0 0 2 0.5 ) #( 0 0 1 2 2 1 ) multiply-envelopes\n\
  => #( 0.0 0.0 0.5 0.5 1.0 0.5 )."
	<'> f* map-envelopes
;
<'> add-envelopes      alias envelopes+
<'> multiply-envelopes alias envelopes*

\ ;;; -------- max-envelope

: max-envelope ( en -- r )
	doc" Return max y value in EN."
	{ en }
	en 1 object-ref en length 1 ?do
		en i object-ref fmax
	2 +loop
;

\ ;;; -------- min-envelope

: min-envelope ( en -- r )
	doc" Return min y value in EN."
	{ en }
	en 1 object-ref en length 1 ?do
		en i object-ref fmin
	2 +loop
;

\ ;;; -------- integrate-envelope

: integrate-envelope ( en -- r )
	doc" Area under env."
	{ en }
	0.0 ( sum ) en length 3 - 0 ?do
		en i 1+ object-ref en i 3 + object-ref f+ 0.5 f*
		en i 2+ object-ref en i     object-ref f- f*  f+ ( sum+=... )
	2 +loop
	( sum )
;

: envelope-length ( en -- n )
	doc" Return number of points in EN."
	length 2/
;

\ ;;; -------- envelope-last-x

: envelope-last-x ( en -- r )
	doc" Return max x axis break point position."
	-2 object-ref
;

\ ;;; -------- stretch-envelope

: stretch-envelope <{ fn old-attack new-attack
    :optional old-decay #f new-decay #f -- new-env }>
	doc" Take ENV and returns a new envelope based on it \
but with the attack and optionally decay portions stretched or squeezed; \
OLD-ATTACK is the original x axis attack end point, \
NEW-ATTACK is where that section should end in the new envelope.  \
Similarly for OLD-DECAY and NEW-DECAY.  \
This mimics divseg in early versions of CLM \
and its antecedents in Sambox and Mus10 (linen).  \
ENV may be a vct, or an array.\n\
#( 0 0 1 1 )     0.1 0.2         stretch-envelope => #( 0 0 0.2 0.1 1 1 )\n\
#( 0 0 1 1 2 0 ) 0.1 0.2 1.5 1.6 stretch-envelope => #( 0 0 0.2 0.1 1.1 1 1.6 0.5 2 0 )."
	old-decay
	new-decay not && if
		'argument-error
		    #( "%s: old-decay, %s, but no new-decay, %s?"
		       get-func-name
		       old-decay
		       new-decay ) fth-throw
	then
	fn length { len }
	fn 0 object-ref dup { x0 new-x }
	fn 1 object-ref { y0 }
	fn -2 object-ref { last-x }
	#( x0 y0 ) { new-fn }
	new-attack x0 f- 0.0001 old-attack x0 f- fmax f/ { scl }
	old-decay if
		old-decay old-attack f= if
			old-decay 0.000001 last-x f* f+ to old-decay
		then
	then
	len 1- 2 ?do
		fn i object-ref { x1 }
		fn i 1+ object-ref { y1 }
		x0 old-attack f<
		x1 old-attack f>= && if
			x1 old-attack f= if
				y1
			else
				y0 y1 y0 f- old-attack
				    x0 f- x1 x0 f- f/ f* f+
			then to y0
			old-attack to x0
			new-attack to new-x
			new-fn new-x array-push y0 array-push drop
			old-decay if
				new-decay new-attack f-
				    old-decay old-attack f- f/
			else
				last-x new-attack f-
				    last-x old-attack f- f/
			then to scl
		then
		old-decay if
			x0 old-decay f<
			x1 old-decay f>= && if
				x1 old-decay f= if
					y1
				else
					y0 y1 y0 f- old-decay
					    x0 f- x1 x0 f- f/ f* f+
				then to y0
				old-decay to x0
				new-decay to new-x
				new-fn new-x array-push y0 array-push drop
				last-x new-decay f-
				    last-x old-decay f- f/ to scl
			then
		then
		x0 x1 f<> if
			new-x scl x1 x0 f- f* f+ to new-x
			new-fn new-x array-push y1 array-push drop
			x1 to x0
			y1 to y0
		then
	2 +loop
	fn array? if
		new-fn
	else
		fn vct? if
			new-fn vector->vct
		then
	then
;

\ ;;; -------- scale-envelope

: scale-envelope <{ en scl :optional offset 0 -- new-en }>
	doc" Scale y axis values by SCL and optionally adds OFFSET.  \
EN may be a list, a vct, or an array."
	en map
		*key* i 2 mod if
			scl f* offset f+
		then
	end-map ( new-en )
;

\ ;;; -------- reverse-envelope

: reverse-envelope ( en1 -- en2 )
	doc" Reverse the breakpoints in EN1."
	{ en1 }
	en1 length { size }
	en1 envelope-copy { en2 }
	size 2 - { idx }
	en1 -2 object-ref { xmax }
	en1 length 1- 0 ?do
		en2 idx     xmax en1 i    object-ref f-  object-set!
		en2 idx 1+       en1 i 1+ object-ref     object-set!
		idx 2 - to idx
	2 +loop
	en2
;

\ ;;; -------- envelope-concatenate from clm/env.lisp

: envelope-concatenate ( en1 ... enn n -- ne )
	doc" Concatenate N envelopes into a new envelope (from clm/env.lisp)."
	>array { envs }
	envs object-length 1 = if
		envs 0 array-ref
	else
		0.0 { xoff }
		#() { rne }
		envs each { en }
			en first-ref { firstx }
			en object-length 0 ?do
				en i    object-ref { x }
				en i 1+ object-ref { y }
				rne x firstx f- xoff f+ array-push
				    y array-push to rne
			2 +loop
			rne -2 array-ref 0.01 f+ to xoff
		end-each
		rne
	then
;

\ ;;; -------- concatenate-envelopes from snd/env.scm

: concatenate-envelopes ( en1 ... enn n -- ne )
	doc" Concatenate N envelopes into a new envelope (from snd/env.scm)."
	>array { envs }
	envs object-length 1 = if
		envs 0 array-ref
	else
		0.0 { xoff }
		#() { rne }
		envs each { en }
			en first-ref { firstx }
			rne object-length 0> if
				rne last-ref en second-ref f= if
					xoff 0.01 f- to xoff
					2
				else
					0
				then
			else
				0
			then { beg }
			en object-length beg ?do
				en i    object-ref { x }
				en i 1+ object-ref { y }
				rne x firstx f- xoff f+ array-push
				    y array-push to rne
			2 +loop
			rne -2 object-ref 0.01 f+ to xoff
		end-each
		rne
	then
;

\ ;;; -------- repeat-envelope

: repeat-envelope <{ ur-env repeats
    :optional reflected #f normalized #f -- en }>
	doc" Repeat UR-ENV REPEATS times.\n\
#( 0 0 100 1 ) 2 repeat-envelope => #( 0 0 100 1 101 0 201 1 )\n\
If the final y value is different from the first y value, \
a quick ramp is inserted between repeats.  \
NORMALIZED causes the new envelope's x axis to have the same extent as the original's.  \
REFLECTED causes every other repetition to be in reverse."
	repeats reflected if
		f2/ fround->s
	then { times }
	reflected if
		ur-env envelope-last-x     { lastx }
		ur-env array-copy          { n-env }
		ur-env 0 -3 array-subarray array-reverse { r-env }
		r-env object-length 1- 0 ?do
			r-env i    object-ref { xx }
			r-env i 1+ object-ref { yy }
			n-env #( lastx yy f- lastx f+ xx ) array-append to n-env
		2 +loop
		n-env
	else
		ur-env
	then { e }
	e second-ref { first-y }
	e envelope-last-x { x-max }
	e first-ref { x }
	first-y e last-ref f= { first-y-is-last-y }
	#( x first-y ) { new-env }
	times 0 ?do
		e object-length 1- 2 ?do
			e i object-ref e i 2- object-ref f- x f+ to x
			new-env #( x e i 1+ object-ref ) array-append to new-env
		2 +loop
		i times 1- < first-y-is-last-y not && if
			x-max 100.0 f/ x f+ to x
			new-env #( x first-y ) array-append to new-env
		then
	loop
	normalized if
		x-max x f/ { scl }
		new-env map!
			*key* i 2 mod 0= if
				scl f*
			then
		end-map ( new-env' )
	else
		new-env
	then
;

\ ;;; -------- power-env 

: make-power-env <{ envelope :key scaler 1.0 offset 0.0 duration 1.0 -- pe }>
	envelope -3 object-ref envelope 0 object-ref f- { xext }
	0 { jj }
	envelope length 3.0 f/ fround->s 1- ( len ) make-array map!
		envelope jj     object-ref { x0 }
		envelope jj 3 + object-ref { x1 }
		envelope jj 1+  object-ref { y0 }
		envelope jj 4 + object-ref { y1 }
		envelope jj 2+  object-ref { base }
		3 +to jj
		:envelope #( 0.0 y0 1.0 y1 )
		    :base base
		    :scaler scaler
		    :offset offset
		    :duration x1 x0 f- xext f/ duration f* make-env
	end-map { envs }
	#{ :envs envs
	   :current-env 0
	   :current-pass envs 0 array-ref mus-length }
;

: power-env ( pe -- val )
	{ pe }
	pe :envs hash-ref pe :current-env hash-ref array-ref env ( val )
	pe :current-pass hash-ref 1- { pass }
	pass 0= if
		pe :current-env  pe :current-env hash-ref 1+  hash-set!
		( pe ) :envs hash-ref pe :current-env hash-ref
		    array-ref mus-length to pass
	then
	pe :current-pass pass hash-set! drop
	( val )
;

[defined] env-channel [if]
	hide
	: pec-cb { pe beg snd chn edpos -- prc; self -- curbeg }
		0 proc-create ( prc )
		pe , beg , snd , chn , edpos ,
	  does> { self -- curbeg }
		self           @ { pe }
		self   cell+   @ { beg }
		self 2 cells + @ { snd }
		self 3 cells + @ { chn }
		self 4 cells + @ { edpos }
		pe :envs hash-ref each { e }
			e mus-length 1+ { len }
			e beg len snd chn edpos env-channel drop
			len +to beg
		end-each
		beg
	;
	set-current

	: power-env-channel <{ pe :optional beg 0 dur #f snd #f chn #f edpos #f edname "power-env-channel" -- curbeg }>
		pe beg snd chn edpos pec-cb edname as-one-edit
	;
	previous
[then]

\ ;;; here's a simpler version that takes the breakpoint list, rather than the power-env structure:

[defined] xramp-channel [if]
	hide
	: powc-cb { en beg dur snd chn edpos -- prc; self -- base }
		dur snd chn #f framples || to dur
		0 proc-create ( prc )
		en , beg , dur , snd , chn , edpos ,
	  does> { self -- base }
		self           @ { en }
		self   cell+   @ { curbeg }
		self 2 cells + @ { dur }
		self 3 cells + @ { snd }
		self 4 cells + @ { chn }
		self 5 cells + @ { edpos }
		en 0 array-ref { x1 }
		en -3 object-ref x1 f- { xrange }
		en 1 array-ref { y1 }
		en 2 array-ref { base }
		0.0 0.0 { x0 y0 }
		en length 3 ?do
			x1 to x0
			y1 to y0
			en i    object-ref to x1
			en i 1+ object-ref to y1
			x1 x0 f- xrange f/ dur f* fround->s { curdur }
			y0 y1 base curbeg curdur snd chn edpos
			    xramp-channel drop
			curdur +to curbeg
			en i 2+ object-ref to base
		3 +loop
		base
	;
	set-current

	\ ;; envelope with a separate base for each segment:
	\ #( 0 0 0.325  1 1 32.0 2 0 32.0 ) powenv-channel
	: powenv-channel <{ envelope :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
		envelope length 3 = if
			envelope 1 array-ref beg dur snd chn edpos
			    scale-channel
		else
			"%s %s %s %s"
			    #( envelope beg dur get-func-name )
			    string-format { origin }
			envelope beg dur snd chn edpos powc-cb
			    origin as-one-edit
		then
	;
	previous
[then]

\ ;;; by Anders Vinjar:
\ ;;;
\ ;;; envelope-exp can be used to create exponential segments to include in
\ ;;; envelopes.  Given 2 or more breakpoints, it approximates the
\ ;;; curve between them using 'xgrid linesegments and 'power as the
\ ;;; exponent. 
\ ;;; 
\ ;;; env is a list of x-y-breakpoint-pairs,
\ ;;; power applies to whole envelope,
\ ;;; xgrid is how fine a solution to sample our new envelope with.

: envelope-exp <{ en1 :optional power 1.0 xgrid 100 -- en2 }>
	en1 min-envelope { mn }
	en1 max-envelope mn f- { largest-diff }
	en1 first-ref { x-min }
	en1 envelope-last-x { x-max }
	x-max x-min f- xgrid f/ { x-incr }
	#() { en2 }
	x-min { x }
	0.0 { y }
	largest-diff f0= if
		begin
			x en1 1.0 envelope-interp to y
			en2 #( x  y ) array-append to en2
			x-incr x f+ to x
			x x-max f>=
		until
	else
		begin
			x en1 1.0 envelope-interp to y
			en2 #( x
			       y mn f- largest-diff f/ power f**
			           largest-diff f* mn f+ ) array-append to en2

			x-incr x f+ to x
			x x-max f>=
		until
	then
	en2
;

\ ;;; rms-envelope

[defined] make-sampler [if]
	: rms-envelope <{ file :key beg 0.0 dur #f rfreq 30.0 db #f -- en }>
		file find-file to file
		file unless
			'no-such-file #( "%s: %S" get-func-name file ) fth-throw
		then
		#() { en }
		rfreq 1/f { incr }
		file mus-sound-srate { fsr }
		incr fsr f* fround->s { incrsamps }
		beg fsr f* fround->s { start }
		start file 0 1 #f make-sampler { reader }
		dur if
			fsr dur f* start f+ fround->s
			  file mus-sound-framples min
		else
			file mus-sound-framples
		then { end }
		:size incrsamps make-moving-average { rms }
		end 0 ?do
			0.0 { rms-val }
			incrsamps 0 ?do
				reader #() apply { val }
				rms val dup f* moving-average to rms-val
			loop
			en i fsr f/ array-push to en
			rms-val fsqrt to rms-val
			db if
				rms-val 0.00001 f< if
					-100.0
				else
					rms-val flog 10.0 flog f/ 20.0 f*
				then
			else
				rms-val
			then en swap array-push to en
		incrsamps +loop
		en array-reverse
	;
[then]

\ ;;; -------- normalize-envelope

: normalize-envelope <{ en1 :optional new-max 1.0 -- en2 }>
	doc" Scale envelope by NEW-MAX / max-envelope(EN1)."
	en1 second-ref en1 length 1 ?do
		en1 i object-ref fabs fmax
	2 +loop { peak }
	en1 new-max peak f/ 0.0 scale-envelope
;

\ env.fs ends here
