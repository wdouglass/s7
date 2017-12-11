\ enved.fs -- enved object type

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/11/13 13:59:42
\ Changed: 17/12/02 02:57:20
\
\ @(#)enved.fs	1.19 12/2/17

\ Commentary:

\ This is an example of an Object type written in Forth.
\
\ ENVED
\
\ enved?		( obj -- f )
\ make-enved		( ary -- en )
\
\ enved-length		( en -- len|-1 )
\ enved-ref		( en index -- point )
\ enved-set!		( en index point -- )
\ enved-index           ( en x -- index|-1 )
\ enved-insert!         ( en index point -- )
\ enved-delete!         ( en index -- )

\ === ENVED OBJECT TYPE ===

"enved" make-object-type constant fth-enved

\
\ ENVED?
\
fth-enved make-?obj enved?
<'> enved? "( obj -- f )  test if OBJ is an enved\n\
#( 0 1 1 0 ) make-enved enved? => #t\n\
#( 0 1 1 0 ) enved? => #f\n\
Return #t if OBJ is an enved object, otherwise #f." help-set!

hide
\ Creates setter enved-envelope! and getter enved-envelope@ and
\ make-enved-instance for use in make-en.
#( "enved-envelope" ) create-instance-struct make-enved-instance

: (enved-out-of-bounds) { pos arg name -- ex args }
	'out-of-range #( "%s arg %d: %d is out of bounds" name pos arg )
;

: enved-out-of-bounds ( pos arg -- )
	postpone get-func-name
	postpone (enved-out-of-bounds)
	postpone fth-throw
; immediate

: make-en { ary -- en }
	fth-enved make-enved-instance { en }
	en ary enved-envelope!
	en
;

: en-inspect { self -- str }
	self enved-envelope@ { ary }
	"#<%s[%d]: %s>"
		#( self object-name
		   ary array-length 2/
		   ary ) string-format
;

: en->string ( self -- str )   enved-envelope@ object->string ;
: en-dump ( self -- str )   en->string " make-enved" $+ ;
: en->array ( self -- ary )   enved-envelope@ array->array ;
: en-copy ( self -- en ) enved-envelope@ array-copy make-en ;
: en-length ( self -- len )   enved-envelope@ array-length 2/ ;

: en-ref { self index -- point }
	self index object-range? if
		index 2* to index
		#( self enved-envelope@ index    array-ref
		   self enved-envelope@ index 1+ array-ref )
	else
		2 index enved-out-of-bounds
	then
;

: en-set! { self index point -- }
	self index object-range? if
		index 2* to index
		self enved-envelope@ index    point 0 array-ref array-set!
		self enved-envelope@ index 1+ point 1 array-ref array-set!
	else
		2 index enved-out-of-bounds
	then
;

: en-equal? { self obj -- f }
	self obj = if
		#t
	else
		self enved-envelope@ obj enved-envelope@ object-equal?
	then
;

\ Init object type Enved.
<'> en-inspect fth-enved set-object-inspect	\ en .inspect
<'> en->string fth-enved set-object->string	\ en object->string
<'> en-dump    fth-enved set-object-dump	\ en object-dump
<'> en->array  fth-enved set-object->array	\ en object->array
<'> en-copy    fth-enved set-object-copy	\ en object-copy
<'> en-ref     fth-enved set-object-value-ref	\ en idx object-ref => #( x y )
<'> en-set!    fth-enved set-object-value-set	\ en idx #( x y ) object-set!
<'> en-equal?  fth-enved set-object-equal-p	\ obj1 obj2 object-equal?
<'> en-length  fth-enved set-object-length	\ en object-length => (len/2)
<'> en-ref     fth-enved 1 set-object-apply	\ en idx apply => #( x y )
set-current

\
\ MAKE-ENVED
\
: make-enved ( ary -- en )
	doc" create enved\n\
#( 0 1 1 0 ) make-enved value en\n\
Return new enved object from ARY \
which must be an array of even length."
	{ ary }
	ary array-length 2 mod 0= ary 1 "an array of even length" assert-type
	ary make-en
;

\
\ ENVED-LENGTH
\
: enved-length ( obj -- len|-1 )
	doc" return enved length\n\
#( 0 1 1 0 ) make-enved enved-length => 2\n\
#( 0 1 1 0 ) enved-length => -1\n\
If OBJ is an enved object, return its length, otherwise -1."
	{ obj }
	obj enved? if
		obj en-length
	else
		-1
	then
;

\
\ ENVED-REF
\
: enved-ref ( en index -- point )
	doc" return point at INDEX\n\
#( 0 1 1 0 ) make-enved value en\n\
en 0 enved-ref => #( 0 1 )\n\
en 1 enved-ref => #( 1 0 )\n\
Return point at position INDEX.  \
Negative index counts from backward.  \
Raise OUT-OF-RANGE exception if INDEX is not in EN's range."
	{ en index }
	en enved? en 1 "an enved" assert-type
	index 0< if
		en en-length index + to index
	then
	en index en-ref
;

\
\ ENVED-SET!
\
: enved-set! ( en index point -- )
	doc" set point at INDEX\n\
#( 0 1 1 0 ) make-enved value en\n\
en 0 #( 0 0.5 ) enved-set!\n\
en .$ => #( 0 0.5 1 0 )\n\
en 1 #( 1 0.5 ) enved-set!\n\
en .$ => #( 0 0.5 1 0.5 )\n\
Store POINT at position INDEX.  \
Negative index counts from backward.  \
Raise OUT-OF-RANGE exception if INDEX is not in EN's range."
	{ en index point }
	en enved? en 1 "an enved" assert-type
	index 0< if
		en en-length index + to index
	then
	en index point en-set!
;

\
\ ENVED-INDEX
\
: enved-index ( en x -- index|-1 )
	doc" return index of X in EN\n\
#( 1 3  2 3 ) make-enved value en\n\
en 1 enved-index => 0\n\
en 2 enved-index => 1\n\
en 3 enved-index => -1\n\
Return point-index where X is point's x value, or -1 if not found."
	{ en x }
	en enved? en 1 "an enved" assert-type
	-1 { idx }
	en enved-envelope@ { ary }
	ary array-length 0 ?do
		ary i array-ref x f= if
			i 2/ to idx leave      
		then
	2 +loop
	idx
;

\
\ ENVED-INSERT!
\
: enved-insert! ( en index point -- )
	doc" insert POINT at INDEX\n\
#( 1 1  2 2 ) make-enved value en\n\
en 0 #( 0 0 ) enved-insert!\n\
en .$ => #( 0 0 1 1 2 2 )\n\
en 3 #( 3 3 ) enved-insert!\n\
en .$ => #( 0 0 1 1 2 2 3 3 )\n\
Insert POINT at INDEX into EN.  \
Negative index counts from backward.  \
Raise OUT-OF-RANGE exception if INDEX is not in EN's range."
	{ en index point }
	en enved? en 1 "an enved" assert-type
	point array-length 2 = point 3 "a point #( x y )" assert-type
	index 0< if
		en en-length index + to index
	then
	en index object-range? if
		index 2* to index
		en enved-envelope@ index point array-insert! drop
	else
		index en en-length = if
			en enved-envelope@ ( ary )
			    point 0 array-ref array-push ( ary' )
			    point 1 array-ref array-push drop
		else
			2 index enved-out-of-bounds
		then
	then
;

\
\ ENVED-DELETE!
\
: enved-delete! ( en index -- )
	doc" delete point at INDEX\n\
#( 1 1  2 2 ) make-enved value en\n\
en 1 enved-delete!\n\
en .$ => #( 1 1 )\n\
Delete point at INDEX from EN.  \
Negative index counts from backward.  \
Raise OUT-OF-RANGE exception if INDEX is not in EN's range."
	{ en index }
	en enved? en 1 "an enved" assert-type
	index 0< if
		en en-length index + to index
	then
	en index object-range? if
		index 2* to index
		en enved-envelope@ index array-delete! drop
		en enved-envelope@ index array-delete! drop
	else
		2 index enved-out-of-bounds
	then
;

\ XXX
\ For backwards compatibility.
: envelope@ { en -- ary }
	en enved? en 1 "an enved" assert-type
	en enved-envelope@
;

\ XXX
\ For backwards compatibility with arguments swapped.
: envelope! { ary en -- }
	ary array? ary 1 "an array" assert-type
	en enved? en 2 "an enved" assert-type
	en ary enved-envelope!
;
previous

hide
: test-display { res req str -- }
	res req object-equal? unless
		"\\ %s: res %s, req %s\n" #( str res req ) fth-print
	then
;

: test-catch { xt ex -- tag }
	xt ex nil fth-catch { tag }
	stack-reset
	tag
;
set-current

: enved-test ( -- )
	nil nil nil { en1 en2 tag }
	\
	\ make-enved
	\
	#( 0 1 1 0 ) make-enved to en1
	#( 0 1 1 0 ) make-enved to en2
	#( 0 1 2 ) <'> make-enved 'wrong-type-arg test-catch to tag
	tag car 'wrong-type-arg "#( 0 1 2 ) make-enved" test-display
	\
	\ enved?
	\
	en1 enved? #t "en1 enved?" test-display
	en2 enved? #t "en2 enved?" test-display
	#( 0 1 1 0 ) enved? #f "#( 0 1 1 0 ) enved?" test-display
	\
	\ enved-length
	\
	en1 enved-length 2 "en1 enved-length" test-display
	en2 enved-length 2 "en2 enved-length" test-display
	#( 0 1 1 0 ) enved-length -1 "#( 0 1 1 0 ) enved-length" test-display
	\
	\ enved-ref
	\
	en1 0 enved-ref #( 0 1 ) "en1 0 enved-ref" test-display 
	en1 1 enved-ref #( 1 0 ) "en1 1 enved-ref" test-display
	en1 2 <'> enved-ref 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 2 enved-ref" test-display
	\
	\ enved-set!
	\
	#( 0 1 1 0 ) make-enved to en1
	en1 0 #( 0 0 ) enved-set!
	#( 0 0 1 0 ) make-enved to en2
	en1 en2 "en1 0 #( 0 0 ) enved-set!" test-display
	#( 0 1 1 0 ) make-enved to en1
	en1 1 #( 1 1 ) enved-set!
	#( 0 1 1 1 ) make-enved to en2
	en1 en2 "en1 1 #( 1 1 ) enved-set!" test-display
	en1 2 #( 2 0 ) <'> enved-set! 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 2 enved-set!" test-display
	\
	\ enved-index
	\
	#( 0 1 2 0 ) make-enved to en1
	en1 0 enved-index 0 "en1 0 enved-index" test-display
	en1 2 enved-index 1 "en1 2 enved-index" test-display
	en1 1 enved-index -1 "en1 1 enved-index" test-display
	\
	\ enved-insert!
	\
	#( 1 1 2 2 ) make-enved to en1
	en1 0 #( 0 0 ) enved-insert!
	#( 0 0 1 1 2 2 ) make-enved to en2
	en1 en2 "en1 0 #( 0 0 ) enved-insert!" test-display
	en1 3 #( 3 3 ) enved-insert!
	#( 0 0 1 1 2 2 3 3 ) make-enved to en2
	en1 en2 "en1 3 #( 3 3 ) enved-insert!" test-display
	en1 5 #( 5 5 ) <'> enved-insert! 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 5 #( 5 5 ) enved-insert!" test-display
	\
	\ enved-delete!
	\
	#( 0 0 1 1 2 2 3 3 ) make-enved to en1
	en1 0 enved-delete!
	#( 1 1 2 2 3 3 ) make-enved to en2
	en1 en2 "en1 0 enved-delete!" test-display
	en1 2 enved-delete!
	#( 1 1 2 2 ) make-enved to en2
	en1 en2 "en1 2 enved-delete!" test-display
	en1 3 <'> enved-delete! 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 3 enved-delete!" test-display
	\
	\ object-inspect
	\
	#( 0 1 1 0 ) make-enved to en1
	en1 object-inspect "#<enved[2]: #( 0 1 1 0 )>"
	    "object-inspect" test-display
	\
	\ object->string
	\
	en1 object->string "#( 0 1 1 0 )" "object->string" test-display
	\
	\ object-dump
	\
	en1 object-dump "#( 0 1 1 0 ) make-enved" "object-dump" test-display
	\
	\ object->array
	\
	en1 object->array #( 0 1 1 0 ) "object->array" test-display
	\
	\ object-copy
	\
	en1 object-copy en1 "object-copy" test-display
	\
	\ object-ref
	\
	en1 0 object-ref #( 0 1 ) "en1 0 object-ref" test-display 
	en1 1 object-ref #( 1 0 ) "en1 1 object-ref" test-display
	en1 2 <'> object-ref 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 2 object-ref" test-display
	\
	\ object-set!
	\
	#( 0 1 1 0 ) make-enved to en1
	en1 0 #( 0 0 ) object-set!
	#( 0 0 1 0 ) make-enved to en2
	en1 en2 "en1 0 #( 0 0 ) object-set!" test-display
	#( 0 1 1 0 ) make-enved to en1
	en1 1 #( 1 1 ) object-set!
	#( 0 1 1 1 ) make-enved to en2
	en1 en2 "en1 1 #( 1 1 ) object-set!" test-display
	en1 2 #( 2 0 ) <'> object-set! 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 2 object-set!" test-display
	\
	\ object-length
	\
	#( 0 1 1 0 ) make-enved to en1
	#( 0 1 1 0 ) make-enved to en2
	en1 object-length 2 "en1 object-length" test-display
	en2 object-length 2 "en2 object-length" test-display
	\
	\ object-equal?
	\
	en1 en2 "object-equal?" test-display
	\
	\ object-apply
	\
	en1 0 object-apply #( 0 1 ) "en1 0 object-apply" test-display 
	en1 1 object-apply #( 1 0 ) "en1 1 object-apply" test-display
	en1 2 <'> object-apply 'out-of-range test-catch to tag
	tag car 'out-of-range "en1 2 object-apply" test-display
;
previous

\ enved.fs ends here
