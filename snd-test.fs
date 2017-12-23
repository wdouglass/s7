\ snd-test.fs -- Snd Forth code and tests

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 2006/08/05 00:09:28
\ Changed: 2017/12/23 06:12:07

\ Tags:  FIXME - something is wrong
\        XXX   - info marker
\
\ Tested with:
\   Snd 18.x
\   Fth 1.3.x
\
\ The most Gtk tests will be skipped if not gtk3.
\
\ Reads init file ./.sndtest.fs or ~/.sndtest.fs for global variables,
\ hooks, etc.
\
\ Example:
\
\ cat ./.sndtest.fs
\ "/tmp" set-save-dir to original-save-dir
\ save-dir set-temp-dir to original-temp-dir
\
\ #t to with-big-file
\ "/usr/opt/sound/SFiles/bigger.snd" to bigger-snd
\ "/usr/opt/sound/sf1/" to sf-dir
\ #t to all-args
\ 2 to *tests*
\ #t to *snd-test-verbose*
\
\ lambda: <{ output -- }>
\   "sox -qV1 %s -d" #( output ) string-format file-system unless
\     "exit %d\n" #( exit-status ) fth-print
\   then
\ ; to *snd-test-ws-player*
\
\ #t to *snd-test-ws-play*
\ #t to *snd-test-ws-statistics*
\ #t to *snd-test-ws-verbose*

\
\ Start tests:
\
\ snd -noinit -load snd-test.fs          \ all tests
\ snd -noinit -load snd-test.fs 10 15 19 \ test 10 15 19
\ snd -noinit -load snd-test.fs -23      \ all tests except 23
\
\ test 00: constants
\ test 01: defaults
\ test 02: headers
\ test 03: variables
\ test 04: sndlib
\ test 05: simple overall checks
\ test 08: clm
\ test 10: marks
\ test 15: chan-local vars
\ test 19: save and restore
\ test 23: with-sound
\ test 27: general ( will be replaced in the future )
\ test 28: errors

'alsa       provided? constant *with-test-alsa*
'complex    provided? constant *with-test-complex*
'gl         provided? constant *with-test-gl*
'gl2ps      provided? constant *with-test-gl2ps*
'gsl        provided? constant *with-test-gsl*
'gtk3       provided? constant *with-test-gtk3*
'snd-gtk    provided? constant *with-test-gtk*
'snd-ladspa provided? constant *with-test-ladspa*
'snd-motif  provided? constant *with-test-motif*
'snd-nogui  provided? constant *with-test-nogui*
*with-test-nogui* not constant *with-test-gui*

24 set-object-print-length

*with-test-complex* [unless]
  -1 constant 0+i
[then]

*with-test-nogui* [if]
  #f value stdout-io
  #f value stderr-io
[else]
  \ Prints to Snd's listener and stdout/stderr.

  \ The original CLM-PRINT uses only SND-PRINT but we want output
  \ to stdout/stderr too.
  : clm-print ( fmt :optional args -- )
    fth-format ( str ) snd-print ( str ) .stdout
  ;

  :port-name "sndout"
  :write-line lambda: <{ line -- }> line snd-print ( line ) .stdout ;
  make-soft-port set-*stdout* value stdout-io

  :port-name "snderr"
  :write-line lambda: <{ line -- }> line snd-print ( line ) .stderr ;
  make-soft-port set-*stderr* value stderr-io
[then]

\ Output words: We can't use clm-print here if we want xterm output
\ (clm-message uses clm-print).

\ SND-TEST-MESSAGE: Puts a comment sign before output
\                   and terminates with a carriage return.
: snd-test-message ( fmt args -- ) ." \ " fth-print cr ;

\ SND-DISPLAY: Wraps text like snd-test-message and prepends text with
\ current line number ("\ [102] text\n").
: (snd-display) { fmt args lno -- }
  fmt args string-format { str }
  "\\ [%d] %s\n" #( lno str ) fth-print
;

: snd-display ( fmt args -- )
  postpone *lineno*
  postpone (snd-display)
; immediate

\ *SND-TEST-VERBOSE*: progress information in long tests
\
\ test 19-save/restore: function names
\ test       28-errors: prints proc-array length and progress information
#f value *snd-test-verbose*

\ WITH-SOUND control
\
\ options for test 23-with-sound:
\ :play
\ :player
\ :statistics
\ :verbose (event (bird) and instrument names)
#f value *snd-test-ws-play*
lambda: <{ output -- }>
  \ Snd's play doesn't complain about more than 2 chans, see
  \ test23-balance
  save-stack { s }
  output play drop
  s restore-stack
; value *snd-test-ws-player*
#f value *snd-test-ws-statistics*
#f value *snd-test-ws-verbose*

\ run snd-test.fs *tests* times
1 value *tests*

\ You may set them in .sndtest.fs.
#f value my-snd-error-hook
#f value my-mus-error-hook

"HOME" getenv value *home*
save-dir *home* "/zap/snd" $+ || value original-save-dir
temp-dir *home* "/zap/tmp" $+ || value original-temp-dir
listener-prompt value original-prompt
mus-file-buffer-size value default-file-buffer-size
8 value *info-array-print-length*

"/home/bil/sf1/" value sf-dir
"/home/bil/zap/sounds/bigger.snd" value bigger-snd
#f value with-big-file
#f value all-args
1000 value base-length

\ Global variables may be overridden in `pwd`/.sndtest.fs or ~/.sndtest.fs.
".sndtest.fs" load-init-file

\ default 1, can be reset in .sndtest.fs
*tests* integer?  [if]
  *tests* 0> [if]
    *tests*
  [else]
    1
  [then]
[else]
  1
[then] to *tests*
0 value *clmtest*

*with-test-nogui* [if]
  '( 0.0 0.1 ) value x-bounds-value
  : x-bounds <{ :optional snd 0 chn 0 axis 0 -- res }>
    x-bounds-value
  ;

  : set-x-bounds <{ bounds :optional snd 0 chn 0 axis 0 -- res }>
    bounds dup to x-bounds-value
  ;

  '( -1.0 1.0 ) value y-bounds-value
  : y-bounds <{ :optional snd 0 chn 0 axis 0 -- res }>
    y-bounds-value
  ;

  : set-y-bounds <{ bounds :optional snd 0 chn 0 axis 0 -- res }>
    bounds dup to y-bounds-value
  ;

  : position->x <{ val :optional snd 0 chn 0 ax time-graph -- res }>
    #f
  ;
  <'> position->x alias position->y

  : x->position <{ val :optional snd 0 chn 0 ax time-graph -- res }>
    #f
  ;
  <'> x->position alias y->position

  : channel-widgets <{ :optional snd 0 chn 0 -- res }>
    #f
  ;

  #t value enved-filter-value
  : enved-filter <{ -- res }>
    enved-filter-value
  ;

  : set-enved-filter <{ val -- res }>
    val dup to enved-filter-value
  ;

  34 value graph-cursor-value
  : graph-cursor <{ -- res }>
    graph-cursor-value
  ;

  : set-graph-cursor <{ val -- res }>
    val dup to graph-cursor-value
  ;

  nil value enved-envelope-value
  : enved-envelope <{ -- res }>
    enved-envelope-value
  ;

  : set-enved-envelope <{ val -- res }>
    val dup to enved-envelope-value
  ;

  hot-colormap value colormap-value
  : colormap <{ -- res }>
    colormap-value
  ;

  : set-colormap <{ val -- res }>
    val positive?
    val 20 <= && if
      val to colormap-value
    then
    val
  ;

  : add-colormap <{ name func -- res }>
    #f
  ;

  <'> noop alias integer->colormap ( n -- cm )
  <'> noop alias colormap->integer ( cm -- n )
  
  : make-graph-data <{ :optional snd 0 chn 0 edpos 0 low -1 high -1 -- res }>
    #f
  ;

  : graph-data <{ data :optional snd 0 chn 0 cx 0 low -1 high -1 gs 0 cr 0 -- }>
    #f
  ;
  
  : draw-line <{ x0 y0 x1 y1 :optional snd 0 chn 0 ax time-graph cr 0 -- res }>
    #f
  ;

  : draw-axes <{ wid gc lab :optional x0 0 x1 0 y0 0 y1 0 st 0 axes 0 -- res }>
    #f
  ;
  <'> noop alias axis-color
  <'> noop alias foreground-color
  <'> noop alias highlight-color
  <'> noop alias snd-gcs

  \ These are already created in snd-nogui.c
  \
  \ 2 #f create-hook mouse-enter-graph-hook
  \ 3 #f create-hook mouse-enter-label-hook
  \ 1 #f create-hook mouse-enter-listener-hook
  \ 1 #f create-hook mouse-enter-text-hook
  \ 2 #f create-hook mouse-leave-graph-hook
  \ 3 #f create-hook mouse-leave-label-hook
  \ 1 #f create-hook mouse-leave-listener-hook
  \ 1 #f create-hook mouse-leave-text-hook
[then]

require clm
require clm-ins
require examp
require hooks
require marks
require extensions
require env
require mix
require dsp
require snd-xm
require effects
require bird.fsm

*clm-search-list* file-pwd array-push to *clm-search-list*
reset-all-hooks

*with-test-motif* [if]
  lambda: <{ dpy e -- }>
    \ XGetErrorText's return code is '( id string )
    dpy e Ferror_code   nil 1024 FXGetErrorText { res }
    "Xlib error_code[%s]: %s"   res fth-warning
    dpy e Frequest_code nil 1024 FXGetErrorText to res
    "Xlib request_code[%s]: %s" res fth-warning
    dpy e Fminor_code   nil 1024 FXGetErrorText to res
    "Xlib minor_code[%s]: %s" res fth-warning
  ; FXSetErrorHandler drop

  lambda: <{ dpy -- }>
    "Xlib IO Error dpy: %S" #( dpy ) fth-error
  ; FXSetIOErrorHandler drop
[then]

: fneq-err ( r1 r2 err -- f ) -rot ( r1 r2 ) f- fabs f<= ;

: fneq   ( a b -- f ) 0.001 fneq-err ;
: ffneq  ( a b -- f ) 0.010 fneq-err ;
: fffneq ( a b -- f ) 0.100 fneq-err ;

: cneq-err ( c1 c2 err -- f )
  { c1 c2 err }
  c1 real-ref  c2 real-ref  err fneq-err
  c1 image-ref c2 image-ref err fneq-err ||
;

: cneq   ( a b -- f ) 0.001 cneq-err ;

: fequal-err ( r1 r2 err -- f ) -rot ( r1 r2 ) f- fabs f> ;

: fequal? ( a b -- f ) 0.001 fequal-err ;

: any->vct ( obj )
  { obj }
  obj vct? if
    obj
  else
    obj array? if
      obj vector->vct
    else
      #f
    then
  then
;

: vequal-err ( v0 v1 err -- f )
  { val0 val1 err }
  val0 any->vct { v0 }
  val1 any->vct { v1 }
  v0
  v1 &&
  v0 length v1 length = && if
    v0 vct-copy v1 vct-subtract! vct-peak err f<=
  else
    #f
  then
;

: vequal?   ( v0 v1 -- f ) 0.001   vequal-err ;

: vvequal   ( v0 v1 -- f ) 0.00002 vequal-err ;
: vfequal   ( v0 v1 -- f ) 0.01    vequal-err ;
: vffequal  ( v0 v1 -- f ) 0.1     vequal-err ;
: vfffequal ( v0 v1 -- f ) 0.5     vequal-err ;

<'> vequal? alias vequal

: feql-err ( obj0 obj1 err -- f )
  { obj0 obj1 err }
  obj0 object-length obj1 object-length = if
    #t ( flag )
    obj0 each ( r0 )
      obj1 i object-ref ( r1 ) err fneq-err if
        not ( toggle flag )
        leave
      then
    end-each ( flag )
  else
    #f
  then
;

: feql   ( obj0 obj1 -- f ) 0.001 feql-err ;
: ffeql  ( obj0 obj1 -- f ) 0.01  feql-err ;
: fffeql ( obj0 obj1 -- f ) 0.1   feql-err ;

: fveql  ( v1 v2 idx -- f)
  { v1 v2 idx }
  #t ( flag )
  v1 length v2 length min idx ?do
    v1 i object-ref v2 i object-ref fneq if
      not ( toggle flag )
      leave
    then
  loop
;

: list-equal? { obj1 obj2 -- f }
  obj1 list?
  obj2 list? && if
    #t ( flag )
    obj1 each ( entry )
      obj2 i list-ref object-equal? unless
        not ( toggle flag )
        leave
      then
    end-each
  else
    #f
  then
;

\ arity: #( req opt rest )
: arity-ok <{ proc args -- f }>
  proc proc? if
    proc proc-arity { args-lst }
    args-lst 0 array-ref { req }  \ int
    args-lst 1 array-ref { opt }  \ int
    args-lst 2 array-ref { rest } \ bool
    opt 0> if
      args req >=
      args req opt + <= &&
    else
      args req =
      rest ||
    then
  else
    #f
  then
;

: set-arity-ok <{ proc args -- f }>
  proc set-xt args arity-ok
; 

: symbol-defined? ( sym -- f )
  "defined? " swap symbol-name $+ string-eval
;

: vector? { obj -- f }
  obj array? if
    \ car doesn't raise an error if length == 0
    obj car number?
  else
    #f
  then
;

: snd-test-vector? { obj -- f }
  obj vct?
  obj vector? ||
;

: snd-test-catch ( xt -- tag )
  #t nil fth-catch { tag }
  stack-reset
  tag
;

: snd-test-format { sndfmt res req fmt args -- str }
  sndfmt #( res req ) string-format { str }
  fmt empty? if
    str
  else
    fmt args string-format ": " $+ str $+
  then
;

: snd-format { res req op fmt args -- str }
  req snd-test-vector? if
    mus-array-print-length { old-alen }
    print-length { old-vlen }
    *info-array-print-length* set-mus-array-print-length drop
    *info-array-print-length* set-print-length drop
    "res " op $+ " req?\n\\ => res %S\n\\ => req %S" $+ res req fmt args
      snd-test-format ( str )
    old-alen set-mus-array-print-length drop
    old-vlen set-print-length drop
    ( str )
  else
    "res %S " op $+ " req %S?" $+ res req fmt args snd-test-format ( str )
  then
;

: snd-test-equal? { res req -- f }
  res req  req float? if
    fequal?
  else
    req snd-test-vector? if
      vequal?
    else
      equal?
    then
  then
;

: (snd-test-neq) { res req fmt args lno -- }
  res req snd-test-equal? unless
    res req "!=" fmt args snd-format { str }
    "\\ [%d] %s\n" #( lno str ) fth-print
  then
;

: (snd-test-eq) { res req fmt args lno -- }
  res req snd-test-equal? if
    res req "==" fmt args snd-format { str }
    "\\ [%d] %s\n" #( lno str ) fth-print
  then
;

: (snd-test-any-neq) { res req func fmt args lno -- }
  res req func execute unless
    res req func xt->name fmt args snd-format { str }
    "\\ [%d] %s\n" #( lno str ) fth-print
  then
;

: (snd-test-any-eq) { res req func fmt args lno -- }
  res req func execute if
    res req func xt->name fmt args snd-format { str }
    "\\ [%d] %s\n" #( lno str ) fth-print
  then
;

: snd-test-neq ( res req fmt args -- )
  postpone *lineno*
  postpone (snd-test-neq)
; immediate

: snd-test-eq  ( res req fmt args -- )
  postpone *lineno*
  postpone (snd-test-eq)
; immediate

\ res req <'> ffeql "more info" #() snd-test-any-neq
: snd-test-any-neq ( res req func fmt args -- )
  postpone *lineno*
  postpone (snd-test-any-neq)
; immediate

\ res req <'> ffeql "more info" #() snd-test-any-eq
: snd-test-any-eq  ( res req func fmt args -- )
  postpone *lineno*
  postpone (snd-test-any-eq)
; immediate

: check-file-name { name -- fsnd }
  name file-exists? if
    name
  else
    sf-dir name $+
  then
;

1 0 0 make-color constant safe-color

: make-color-with-catch { c1 c2 c3 -- color }
  c1 c2 c3 <'> make-color 'no-such-color #t fth-catch if
    stack-reset
    safe-color
  then
;

: reset-almost-all-hooks ( -- )
  reset-all-hooks
  my-snd-error-hook proc? if
    snd-error-hook my-snd-error-hook add-hook!
  then
  my-mus-error-hook proc? if
    mus-error-hook my-mus-error-hook add-hook!
  then
;

*with-test-nogui* [if]
  <'> noop alias dismiss-all-dialogs
[else]
  : dismiss-all-dialogs ( -- )
    nil nil { dialog d }
    dialog-widgets each to dialog
      dialog if
        dialog 0 array-ref symbol? if
          dialog is-managed? if
            dialog hide-widget drop
          then
        else                    \ not widget?
          dialog each to d
            d 0 array-ref symbol? if
              d is-managed? if
                d hide-widget drop
              then
            then
          end-each
        then                    \ widget? 
      then                      \ dialog
    end-each
  ;
[then]

#f  value overall-start-time
#() value test-numbers

: run-fth-test ( xt -- )
  { xt }
  xt xt->name { name }
  name 0 2 string-substring { num }
  num string->number to num
  test-numbers num array-member? if
    name #f snd-test-message
    stack-reset
    gc-run
    make-timer { tm }
    xt execute
    tm stop-timer
    stack-reset
    sounds if
      "open sounds: %s" #( #t short-file-name ) snd-test-message
      sounds each ( snd )
        close-sound drop
      end-each
    then
    #f set-ask-about-unsaved-edits drop
    #f set-remember-sound-state drop
    "%s: %s\n\\ " #( name tm ) snd-test-message
  then
;

: start-snd-test ( -- )
  *with-test-motif* if
    "motif"
  else
    *with-test-gtk* if
      "gtk"
    else
      *with-test-nogui* if
        #t set-with-mix-tags drop
        "nogui"
      else
        "unknown"
      then
    then
  then { kind }
  stack-reset
  "test.snd" file-exists? if
    "test.snd" 0o644 file-chmod
  then
  "=== Snd version: %s (snd-%s)" #( snd-version kind ) snd-test-message
  "=== Fth version: %s"          #( fth-version )      snd-test-message
  ""   #f snd-test-message
  date #f snd-test-message
  ""   #f snd-test-message
  default-file-buffer-size set-mus-file-buffer-size to *clm-file-buffer-size*
  #f  set-with-background-processes drop
  600 set-window-x drop
  10  set-window-y drop
  #t  set-show-listener drop
  reset-almost-all-hooks
  44100 to *clm-srate*
  stack-reset
  make-timer to overall-start-time
;

: finish-snd-test ( -- )
  overall-start-time stop-timer
  stack-reset
  regions each ( r )
    forget-region drop
  end-each
  sounds if
    stop-playing drop
  then
  reset-almost-all-hooks
  #f set-ask-about-unsaved-edits drop
  #f set-remember-sound-state drop
  "all done!" #f snd-test-message
  "" #f snd-test-message
  "summary: %s" #( overall-start-time ) snd-test-message
  0 nil nil { file-count path file }
  #( original-save-dir original-temp-dir "/tmp" ) each to path
    path file-directory? if
      path file-dir each to file
        /snd_/ file regexp-match if
          file file-delete
          1 +to file-count
        then
      end-each
    then
  end-each
  "" #f snd-test-message
  "%d files deleted" #( file-count ) snd-test-message
  "" #f snd-test-message
  "test.snd" file-exists? if
    "test.snd" 0o644 file-chmod
  then
  #( "1"
     "aaa.eps"
     "accelmap"
     "envs.save"
     "fmv.snd"
     "fmv.wav"
     "fmv0.snd"
     "fmv1.snd"
     "fmv2.snd"
     "fmv3.snd"
     "fmv4.reverb"
     "fmv4.snd"
     "gtk-errors"
     "hiho.marks"
     "hiho.snd"
     "hiho.tmp"
     "hiho.wave"
     "ho"
     "new.snd"
     "oboe.marks"
     "obtest.snd.stereo"
     "remembered-oboe.snd.fs"
     "s1.fsm"
     "s1.snd"
     "s6.fsm"
     "s6.reverb"
     "s6.snd"
     "s7.fsm"
     "s7.snd"
     "saved-snd.fs"
     "sec1.fsm"
     "sec1.snd"
     "sec2.fsm"
     "sec2.reverb"
     "sec2.snd"
     "snd.eps"
     "test-1.snd"
     "test-2.snd"
     "test-macros.scm"
     "test.aiff"
     "test.data"
     "test.rev"
     "test.reverb"
     "test.snd"
     "test.snd.snd"
     "test.wav"
     "test.xpm"
     "test2.snd"
     "test3.snd"
     "tmp.snd"
     "with-mix.snd" ) each ( file )
       file-delete
     end-each
  #( "bad_data_format.snd.snd"
     "ce-c3.w02.snd"
     "hcom-16.snd.snd"
     "ieee-text-16.snd.snd"
     "mus10.snd.snd"
     "nasahal.avi.snd"
     "nist-shortpack.wav.snd"
     "o2_dvi.wave.snd"
     "oboe.g721.snd"
     "oboe.g723_24.snd"
     "oboe.g723_40.snd"
     "oki.wav.snd"
     "trumps22.adp.snd"
     "wood.sds.snd" ) each ( file )
       sf-dir swap $+ file-delete
     end-each
  #t set-show-listener drop
  "test-forth.output" save-listener drop
  original-prompt set-listener-prompt drop
;

SIGINT lambda: { sig -- }
  stack-reset
  backtrace
  "" #f snd-test-message
  "Interrupt received.  Clean up %S." #( *filename* #f file-basename )
    snd-test-message
  "" #f snd-test-message
  finish-snd-test
  0 snd-exit drop
; signal drop

\ snd-test.scm translations
\ ---------------- test 00: constants ----------------

*with-test-motif* [if]
  "6x12"
[else]
  *with-test-gtk* [if]
    "Sans 8"
  [else]
    "9x15"
  [then]
[then] constant tiny-font-string

*with-test-motif* [if]
  "9x15"
[else]
  *with-test-gtk* [if]
    "Monospace 10"
  [else]
    "6x12"
  [then]
[then] constant tiny-font-set-string

\ XXX: temp-dir, save-dir, ladspa-dir, peak-env-dir
\ These variables default to NULL (snd.c/snd-0.h).
\ snd-test.scm checks for #f
\ snd-test.fs  checks for ""

: 00-constants ( -- )
  sounds { snds }
  undef undef mixes { mxs }
  undef undef undef marks { mks }
  regions { rgns }
  snds
  mxs ||
  mks ||
  rgns || if
    "start up sounds: %s, mixes: %s, marks: %s, regions: %s?"
      #( snds mxs mks rgns ) snd-display
  then
  \
  nil nil nil nil { vals sym req res }
  #( #( <'> enved-amplitude 0 )
     #( <'> bartlett-window 4 )
     #( <'> bartlett-hann-window 21 )
     #( <'> blackman2-window 6 )
     #( <'> blackman3-window 7 )
     #( <'> blackman4-window 8 )
     #( <'> blackman5-window 24 )
     #( <'> blackman6-window 25 )
     #( <'> blackman7-window 26 )
     #( <'> blackman8-window 27 )
     #( <'> blackman9-window 28 )
     #( <'> blackman10-window 29 )
     #( <'> bohman-window 22 )
     #( <'> cauchy-window 12 )
     #( <'> mlt-sine-window 33 )
     #( <'> papoulis-window 34 )
     #( <'> dpss-window 35 )
     #( <'> sinc-window 36 )
     #( <'> channels-combined 1 )
     #( <'> channels-separate 0 )
     #( <'> channels-superimposed 2 )
     #( <'> connes-window 18 )
     #( <'> cursor-in-middle 3 )
     #( <'> cursor-in-view 0 )
     #( <'> cursor-on-left 1 )
     #( <'> cursor-on-right 2 )
     #( <'> dolph-chebyshev-window 16 )
     #( <'> exponential-window 9 )
     #( <'> flat-top-window 23 )
     #( <'> sync-none 0 )
     #( <'> sync-all 1 )
     #( <'> sync-by-sound 2 )
     #( <'> zoom-focus-active 2 )
     #( <'> zoom-focus-left 0 )
     #( <'> zoom-focus-middle 3 )
     #( <'> zoom-focus-right 1 )
     #( <'> gaussian-window 14 )
     #( <'> graph-dots 1 )
     #( <'> graph-dots-and-lines 3 )
     #( <'> graph-filled 2 )
     #( <'> graph-lines 0 )
     #( <'> graph-lollipops 4 )
     #( <'> hamming-window 5 )
     #( <'> hann-window 1 )
     #( <'> hann-poisson-window 17 )
     #( <'> kaiser-window 11 )
     #( <'> keyboard-no-action 4 )
     #( <'> graph-once 0 )
     #( <'> parzen-window 3 )
     #( <'> poisson-window 13 )
     #( <'> rectangular-window 0 )
     #( <'> riemann-window 10 )
     #( <'> rv2-window 30 )
     #( <'> rv3-window 31 )
     #( <'> rv4-window 32 )
     #( <'> samaraki-window 19 )
     #( <'> ultraspherical-window 20 )
     #( <'> graph-as-sonogram 1 )
     #( <'> graph-as-spectrogram 2 )
     #( <'> graph-once 0 )
     #( <'> graph-as-wavogram 3 )
     #( <'> enved-spectrum 1 )
     #( <'> speed-control-as-float 0 )
     #( <'> speed-control-as-ratio 1 )
     #( <'> speed-control-as-semitone 2 )
     #( <'> enved-srate 2 )
     #( <'> tukey-window 15 )
     #( <'> welch-window 2 )
     #( <'> cursor-cross 0 )
     #( <'> cursor-line 1 )
     #( <'> dont-normalize 0 )
     #( <'> envelope-linear 0 )
     #( <'> envelope-exponential 1 )
     #( <'> normalize-by-channel 1 )
     #( <'> normalize-by-sound 2 )
     #( <'> normalize-globally 3 )
     #( <'> x-axis-in-samples 1 )
     #( <'> x-axis-in-beats 3 )
     #( <'> x-axis-in-measures 4 )
     #( <'> x-axis-in-seconds 0 )
     #( <'> x-axis-as-clock 5 )
     #( <'> x-axis-as-percentage 2 )
     #( <'> enved-add-point 0 )
     #( <'> enved-delete-point 1 )
     #( <'> enved-move-point 2 )
     #( <'> time-graph 0 )
     #( <'> transform-graph 1 )
     #( <'> lisp-graph 2 )
     #( <'> copy-context 0 )
     #( <'> cursor-context 3 )
     #( <'> selection-context 2 )
     #( <'> mark-context 4 )
     #( <'> show-no-axes 0 )
     #( <'> show-all-axes 1 )
     #( <'> show-x-axis 2 )
     #( <'> show-all-axes-unlabelled 3 )
     #( <'> show-x-axis-unlabelled 4 )
     #( <'> show-bare-x-axis 5 )
     \ sndlib constants
     #( <'> mus-unknown-header 0 )
     #( <'> mus-next 1 )
     #( <'> mus-aifc 2 )
     #( <'> mus-riff 3 )
     #( <'> mus-nist 6 )
     #( <'> mus-raw 12 )
     #( <'> mus-ircam 15 )
     #( <'> mus-aiff 49 )
     #( <'> mus-bicsf 5 )
     #( <'> mus-voc 10 )
     #( <'> mus-svx 9 )
     #( <'> mus-soundfont 26 )
     #( <'> mus-rf64 4 )
     #( <'> mus-caff 60 )
     \
     #( <'> mus-interp-none 0 )
     #( <'> mus-interp-linear 1 )
     #( <'> mus-interp-sinusoidal 2 )
     #( <'> mus-interp-all-pass 3 )
     #( <'> mus-interp-lagrange 4 )
     #( <'> mus-interp-bezier 5 )
     #( <'> mus-interp-hermite 6 )
     \
     #( <'> mus-chebyshev-first-kind 1 )
     #( <'> mus-chebyshev-second-kind 2 )
     \
     #( <'> mus-unknown-sample 0 )
     #( <'> mus-bshort 1 )
     #( <'> mus-lshort 10 )
     #( <'> mus-mulaw 2 )
     #( <'> mus-alaw 6 )
     #( <'> mus-byte 3 )
     #( <'> mus-ubyte 7 )
     #( <'> mus-bfloat 4 )
     #( <'> mus-lfloat 12 )
     #( <'> mus-bint 5 )
     #( <'> mus-lint 11 )
     #( <'> mus-bintn 17 )
     #( <'> mus-lintn 18 )
     #( <'> mus-b24int 8 )
     #( <'> mus-l24int 16 )
     #( <'> mus-bdouble 9 )
     #( <'> mus-ldouble 13 )
     #( <'> mus-ubshort 14 )
     #( <'> mus-ulshort 15 )
     #( <'> mus-bfloat-unscaled 19 )
     #( <'> mus-lfloat-unscaled 20 )
     #( <'> mus-bdouble-unscaled 21 )
     #( <'> mus-ldouble-unscaled 22 ) ) { consts }
  consts each to vals
    vals 0 array-ref to sym
    vals 1 array-ref to req
    sym execute ( res ) req "%s" #( sym ) snd-test-neq
  end-each
  \
  temp-dir { old-dir }
  #f set-temp-dir drop
  #( #( <'> region-graph-style graph-lines )
     #( <'> ask-about-unsaved-edits #f )
     #( <'> show-full-duration #f )
     #( <'> show-full-range #f )
     #( <'> initial-beg 0.0 )
     #( <'> initial-dur 0.1 )
     #( <'> ask-before-overwrite #f )
     #( <'> auto-resize #t )
     #( <'> auto-update #f )
     #( <'> channel-style 1 )
     #( <'> color-cutoff 0.003 )
     #( <'> color-inverted #t )
     #( <'> color-scale 1.0 )
     #( <'> auto-update-interval 60.0 )
     #( <'> cursor-update-interval 0.05 )
     #( <'> cursor-location-offset 0 )
     #( <'> dac-combines-channels #t )
     #( <'> dac-size 256 )
     #( <'> clipping #f )
     #( <'> default-output-chans 1 )
     #( <'> default-output-sample-type mus-lfloat )
     #( <'> default-output-srate 44100 )
     #( <'> default-output-header-type mus-next )
     #( <'> dot-size 1 )
     #( <'> cursor-size 15 )
     #( <'> cursor-style cursor-cross )
     #( <'> tracking-cursor-style cursor-line )
     #( <'> enved-base 1.0 )
     #( <'> enved-clip? #t )
     #( <'> enved-filter #t )
     #( <'> enved-filter-order 40 )
     #( <'> enved-in-dB #f )
     #( <'> enved-style envelope-linear )
     #( <'> enved-power 3.0 )
     #( <'> enved-target 0 )
     #( <'> enved-wave? #f )
     #( <'> enved-envelope nil )
     #( <'> eps-file "snd.eps" )
     #( <'> eps-bottom-margin 0.0 )
     #( <'> eps-left-margin 0.0 )
     #( <'> eps-size 1.0 )
     #( <'> fft-window-alpha 0.0 )
     #( <'> fft-window-beta 0.0 )
     #( <'> fft-log-frequency #f )
     #( <'> fft-log-magnitude #f )
     #( <'> fft-with-phases #f )
     #( <'> transform-size 512 )
     #( <'> transform-graph-type graph-once )
     #( <'> fft-window 6 )
     #( <'> graph-cursor 34 )
     #( <'> graph-style graph-lines )
     #( <'> graphs-horizontal #t )
     #( <'> html-dir "." )
     #( <'> html-program "firefox" )
     #( <'> just-sounds #t )
     #( <'> listener-prompt ">" )
     #( <'> max-transform-peaks 100 )
     #( <'> max-regions 16 )
     #( <'> min-dB -60.0 )
     #( <'> log-freq-start 32.0 )
     #( <'> selection-creates-region #t )
     #( <'> transform-normalization normalize-by-channel )
     #( <'> print-length 12 )
     #( <'> play-arrow-size 10 )
     #( <'> save-state-file "saved-snd.fs" )
     #( <'> show-axes 1 )
     #( <'> show-transform-peaks #f )
     #( <'> show-indices #f )
     #( <'> show-marks #t )
     #( <'> show-mix-waveforms #t )
     #( <'> show-selection-transform #f )
     #( <'> show-y-zero #f )
     #( <'> show-grid #f )
     #( <'> grid-density 1.0 )
     #( <'> show-sonogram-cursor #f )
     #( <'> sinc-width 10 )
     #( <'> spectrum-end 1.0 )
     #( <'> spectro-hop 4 )
     #( <'> spectrum-start 0.0 )
     #( <'> spectro-x-angle *with-test-gl* if 300.0 else 90.0 then )
     #( <'> spectro-x-scale *with-test-gl* if 1.5 else 1.0 then )
     #( <'> spectro-y-angle *with-test-gl* if 320.0 else 0.0 then )
     #( <'> spectro-y-scale 1.0 )
     #( <'> spectro-z-angle *with-test-gl* if 0.0 else 358.0 then )
     #( <'> spectro-z-scale *with-test-gl* if 1.0 else 0.1 then )
     #( <'> temp-dir "" )
     #( <'> ladspa-dir "" )
     #( <'> peak-env-dir "" )
     #( <'> tiny-font tiny-font-string )
     #( <'> transform-type fourier-transform )
     #( <'> with-file-monitor #t )
     #( <'> clm-table-size 512 )
     #( <'> with-verbose-cursor #f )
     #( <'> with-inset-graph #f )
     #( <'> with-interrupts #t )
     #( <'> remember-sound-state #f )
     #( <'> with-smpte-label #f )
     #( <'> with-toolbar *with-test-gtk* if #t else #f then )
     #( <'> with-tooltips #t )
     #( <'> with-menu-icons #f )
     #( <'> save-as-dialog-src #f )
     #( <'> save-as-dialog-auto-comment #f )
     #( <'> with-pointer-focus #f )
     #( <'> wavelet-type 0 )
     #( <'> time-graph-type graph-once )
     #( <'> wavo-hop 3 )
     #( <'> wavo-trace 64 )
     #( <'> x-axis-style 0 )
     #( <'> beats-per-minute 60.0 )
     #( <'> beats-per-measure 4 )
     #( <'> zero-pad 0 )
     #( <'> zoom-focus-style 2 )
     #( <'> sync-style sync-by-sound )
     #( <'> mix-waveform-height 20 )
     #( <'> mix-tag-width 6 )
     #( <'> mix-tag-height 14 )
     #( <'> mark-tag-width 10 )
     #( <'> mark-tag-height 4 ) ) { vars }
  vars each to vals
    vals 0 array-ref to sym
    vals 1 array-ref to req
    sym execute ( val ) sym set-execute ( res ) req "set-%s" #( sym )
      snd-test-neq
  end-each
  old-dir set-temp-dir drop
  \
  -123 set-max-transform-peaks drop
  -123 set-zero-pad drop
  max-transform-peaks set-max-transform-peaks 100 "set-max-transform-peaks" #()
    snd-test-neq
  zero-pad set-zero-pad 0 "set-zero-pad" #() snd-test-neq
  #t #t zero-pad nil "#t #t zero-pad" #() snd-test-neq
  *with-test-motif* if
    #( <'> axis-label-font
       <'> axis-numbers-font
       <'> tiny-font
       <'> peaks-font
       <'> bold-peaks-font ) { fonts }
    fonts each to sym
      sym execute to req
      "8x123" sym set-execute ( res ) req "set-%s to bogus value" #( sym )
        snd-test-neq
    end-each
  then
  #f set-ask-about-unsaved-edits drop
  #f set-remember-sound-state drop
;

\ ---------------- test 01: defaults ----------------

hot-colormap             constant *good-colormap*
black-and-white-colormap constant *better-colormap*

: 01-defaults ( -- )
  nil { res }
  *with-test-gui* if
    *good-colormap* colormap? unless
      #f to *good-colormap*
      21 1 do
        i integer->colormap to res
        res colormap? if
          res to *good-colormap*
          leave
        then
      loop
    then
    *better-colormap* colormap? unless
      #f to *better-colormap*
      21 *good-colormap* colormap->integer do
        i integer->colormap to res
        res colormap? if
          res to *better-colormap*
          leave
        then
      loop
    then
  then
  \
  temp-dir { old-dir }
  #f set-temp-dir drop
  #( #( <'> ask-about-unsaved-edits #f )
     #( <'> ask-before-overwrite #f )
     #( <'> auto-resize #t )
     #( <'> auto-update #f )
     #( <'> auto-update-interval 60.0 )
     #( <'> beats-per-measure 4 )
     #( <'> beats-per-minute 60.0 )
     #( <'> channel-style 1 )
     #( <'> clipping #f )
     #( <'> clm-table-size 512 )
     #( <'> color-cutoff 0.003 )
     #( <'> color-inverted #t )
     #( <'> color-scale 1.0 )
     #( <'> colormap *good-colormap* )
     #( <'> contrast-control-amp 1.0 )
     #( <'> with-tracking-cursor #f )
     #( <'> cursor-location-offset 0 )
     #( <'> cursor-size 15 )
     #( <'> cursor-style cursor-cross )
     #( <'> cursor-update-interval 0.05 )
     #( <'> dac-combines-channels #t )
     #( <'> dac-size 256 )
     #( <'> default-output-chans 1 )
     #( <'> default-output-sample-type mus-lfloat )
     #( <'> default-output-header-type mus-next )
     #( <'> default-output-srate 44100 )
     #( <'> dot-size 1 )
     #( <'> enved-base 1.0 )
     #( <'> enved-clip? #t )
     #( <'> enved-envelope nil )
     #( <'> enved-filter #t )
     #( <'> enved-filter-order 40 )
     #( <'> enved-in-dB #f )
     #( <'> enved-power 3.0 )
     #( <'> enved-style envelope-linear )
     #( <'> enved-target 0 )
     #( <'> enved-wave? #f )
     #( <'> eps-bottom-margin 0.0 )
     #( <'> eps-file "snd.eps" )
     #( <'> eps-left-margin 0.0 )
     #( <'> eps-size 1.0 )
     #( <'> expand-control-hop 0.05 )
     #( <'> expand-control-jitter 0.1 )
     #( <'> expand-control-length 0.15 )
     #( <'> expand-control-ramp 0.4 )
     #( <'> fft-log-frequency #f )
     #( <'> fft-log-magnitude #f )
     #( <'> fft-with-phases #f )
     #( <'> fft-window 6 )
     #( <'> fft-window-alpha 0.0 )
     #( <'> fft-window-beta 0.0 )
     #( <'> filter-control-in-dB #f )
     #( <'> filter-control-in-hz #f )
     #( <'> filter-control-order 20 )
     #( <'> graph-cursor 34 )
     #( <'> graph-style graph-lines )
     #( <'> graphs-horizontal #t )
     #( <'> grid-density 1.0 )
     #( <'> html-dir "." )
     #( <'> html-program "firefox" )
     #( <'> initial-beg 0.0 )
     #( <'> initial-dur 0.1 )
     #( <'> just-sounds #t )
     #( <'> ladspa-dir "" )
     #( <'> peak-env-dir "" )
     #( <'> listener-prompt ">" )
     #( <'> log-freq-start 32.0 )
     #( <'> mark-tag-height 4 )
     #( <'> mark-tag-width 10 )
     #( <'> max-regions 16 )
     #( <'> max-transform-peaks 100 )
     #( <'> min-dB -60.0 )
     #( <'> mix-tag-height 14 )
     #( <'> mix-tag-width 6 )
     #( <'> mix-waveform-height 20 )
     #( <'> mus-array-print-length 8 )
     #( <'> mus-clipping #f )
     #( <'> mus-float-equal-fudge-factor 0.0000001 )
     #( <'> play-arrow-size 10 )
     #( <'> print-length 12 )
     #( <'> region-graph-style graph-lines )
     #( <'> remember-sound-state #f )
     #( <'> reverb-control-feedback 1.09 )
     #( <'> reverb-control-lowpass 0.7 )
     #( <'> save-as-dialog-auto-comment #f )
     #( <'> save-as-dialog-src #f )
     #( <'> save-state-file "saved-snd.fs" )
     #( <'> selection-creates-region #t )
     #( <'> show-axes 1 )
     #( <'> show-controls #f )
     #( <'> show-full-duration #f )
     #( <'> show-full-range #f )
     #( <'> show-grid #f )
     #( <'> show-indices #f )
     #( <'> show-marks #t )
     #( <'> show-mix-waveforms #t )
     #( <'> show-selection-transform #f )
     #( <'> show-sonogram-cursor #f )
     #( <'> show-transform-peaks #f )
     #( <'> show-y-zero #f )
     #( <'> sinc-width 10 )
     #( <'> spectrum-end 1.0 )
     #( <'> spectro-hop 4 )
     #( <'> spectrum-start 0.0 )
     #( <'> spectro-x-angle *with-test-gl* if 300.0 else 90.0 then )
     #( <'> spectro-x-scale *with-test-gl* if 1.5 else 1.0 then )
     #( <'> spectro-y-angle *with-test-gl* if 320.0 else 0.0 then )
     #( <'> spectro-y-scale 1.0 )
     #( <'> spectro-z-angle *with-test-gl* if 0.0 else 358.0 then )
     #( <'> spectro-z-scale *with-test-gl* if 1.0 else 0.1 then )
     #( <'> sync-style sync-by-sound )
     #( <'> temp-dir "" )
     #( <'> time-graph-type graph-once )
     #( <'> tiny-font tiny-font-string )
     #( <'> tracking-cursor-style cursor-line )
     #( <'> transform-graph-type graph-once )
     #( <'> transform-normalization normalize-by-channel )
     #( <'> transform-size 512 )
     #( <'> transform-type fourier-transform )
     #( <'> wavelet-type 0 )
     #( <'> wavo-hop 3 )
     #( <'> wavo-trace 64 )
     #( <'> with-mix-tags #t )
     #( <'> with-relative-panes #t )
     #( <'> with-tracking-cursor #f )
     #( <'> with-verbose-cursor #f )
     #( <'> with-inset-graph #f )
     #( <'> with-interrupts #t )
     #( <'> with-smpte-label #f )
     #( <'> with-toolbar *with-test-gtk* if #t else #f then )
     #( <'> with-tooltips #t )
     #( <'> with-menu-icons #f )
     #( <'> with-pointer-focus #f )
     #( <'> x-axis-style 0 )
     #( <'> zero-pad 0 )
     #( <'> zoom-focus-style 2 ) ) { procs }
  nil nil nil nil { vals sym req res }
  procs each to vals
    vals 0 array-ref to sym
    vals 1 array-ref to req
    sym execute ( res ) req "%s" #( sym ) snd-test-neq
  end-each
  old-dir set-temp-dir drop
  \ without-errors
  #( <'> amp-control
     <'> contrast-control
     <'> contrast-control?
     <'> expand-control
     <'> expand-control?
     <'> filter-control-coeffs
     <'> filter-control-envelope
     <'> filter-control?
     <'> lisp-graph?
     <'> read-only
     <'> reverb-control-length
     <'> reverb-control-scale
     <'> reverb-control?
     <'> speed-control
     <'> sync
     <'> time-graph?
     <'> transform-graph? ) to procs
  'no-such-sound to req
  procs each to sym
    sym snd-test-catch car 'no-such-sound "%s" #( sym ) snd-test-neq
  end-each
  \ 1 array-ref
  #( #( <'> amp-control-bounds 8.0 )
     #( <'> contrast-control-bounds 10.0 )
     #( <'> expand-control-bounds 20.0 )
     #( <'> reverb-control-length-bounds 5.0 )
     #( <'> reverb-control-scale-bounds 4.0 )
     #( <'> speed-control-bounds 20.0 ) ) to procs
  procs each to vals
    vals 0 array-ref to sym
    vals 1 array-ref to req
    sym execute 1 array-ref ( res ) req "%s" #( sym ) snd-test-neq
  end-each
  \
  *snd-opened-sound* if
    "*snd-opened-sound*: %S" #( *snd-opened-sound* ) snd-display
  then
  #f set-ask-about-unsaved-edits drop
  #f set-remember-sound-state drop
;

\ ---------------- test 02: headers ----------------

: test-header-check { res req lno name info -- }
  res req "%s: %s" #( name info ) lno (snd-test-neq)
;

: (test-headers-with-loop) ( name chns sr dur typ frm loop-s loop-e lno -- )
  { name chns sr dur typ frm loop-start loop-end lno -- }
  name check-file-name { file }
  file file-exists? if
    file mus-sound-chans       { fchns }
    file mus-sound-srate       { fsr }
    file mus-sound-duration    { fdur }
    file mus-sound-sample-type { ffrm }
    file mus-sound-header-type { ftyp }
    file mus-sound-framples    { fframples }
    file mus-sound-samples     { fsamps }
    file mus-sound-length      { flen }
    fchns chns lno name "chans"    test-header-check
    fsr   sr   lno name "srate"    test-header-check
    fdur  dur  lno name "duration" test-header-check
    file mus-sound-datum-size fdur f* fsr f* fchns f* floor f>s { fsize }
    ffrm mus-unknown-sample <>
                    ftyp 27 <> && if
      flen 1+ fsize < if
        flen 1+ fsize lno name "length" test-header-check
      then
    then
    fframples fsr f/ fdur lno name "framples" test-header-check
    fframples fsamps fchns f/ f- floor fabs f>s { res }
    res 1 > if
      res 1 lno name "samples" test-header-check
    then
    ftyp mus-header-type-name typ lno name "type"   test-header-check
    ffrm mus-sample-type-name frm lno name "format" test-header-check
    file mus-sound-loop-info { lst }
    loop-start if
      lst nil? if
        "[%d] %s loop info empty?" #( lno name ) snd-test-message
      else
        lst car  loop-start lno name "loop-start" test-header-check
        lst cadr loop-end   lno name "loop-end"   test-header-check
      then
    else
      lst empty? unless
        "[%d] %s thinks it has loop info: %s?"
          #( lno name lst ) snd-test-message
      then
    then
  else                          \ not file-exists?
    *fth-debug* if
      "[%d] %s missing?" #( lno file ) snd-test-message
    then
  then                          \ file-exists?
;

: test-headers-with-loop ( name chns sr dur typ frm loop-start loop-end -- )
  postpone *lineno*
  postpone (test-headers-with-loop)
; immediate

: test-headers ( name chns sr dur typ frm -- )
  postpone #f
  postpone #f
  postpone *lineno*
  postpone (test-headers-with-loop)
; immediate

: 02-headers ( -- )
  "5_secs.aiff" 1 44100 5.303107 "AIFF" "big endian short (16 bits)"
    test-headers
  "8svx-8.snd" 1 22050 1.88766443729401 "SVX8" "signed byte (8 bits)"
    test-headers
  "Fnonull.aif" 1 8000 0.00112499995157123 "AIFC" "mulaw (8 bits)" test-headers
  "Pmiscck.aif" 1 8000 0.00112499995157123 "AIFC" "mulaw (8 bits)" test-headers
  "Pmiscck.wav" 1 8000 0.00112499995157123 "RIFF" "mulaw (8 bits)" test-headers
  "Poffset.aif" 1 8000 0.00112499995157123 "AIFC" "mulaw (8 bits)" test-headers
  "Porder.aif" 1 8000 0.00112499995157123 "AIFC" "mulaw (8 bits)" test-headers
  "Ptjunk.aif" 1 8000 0.00112499995157123 "AIFC" "mulaw (8 bits)" test-headers
  "Ptjunk.wav" 1 8000 0.00112499995157123 "RIFF" "mulaw (8 bits)" test-headers
  "SINE24-S.WAV" 2 44100 2.0 "RIFF" "little endian int (24 bits)" test-headers
  "a1.asf" 1 16000 3.736562 "asf" "unknown" test-headers
  "a2.asf" 1 8000 4.630625 "asf" "unknown" test-headers
  "addf8.afsp" 1 8000 2.9760000705719 "Sun/Next" "big endian short (16 bits)"
    test-headers
  "addf8.d" 1 8000 2.9760000705719 "SPPACK" "big endian short (16 bits)"
    test-headers
  "addf8.dwd" 1 8000 2.976000071 "DiamondWare" "little endian short (16 bits)"
    test-headers
  "addf8.nh" 2 44100 0.269931972 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "addf8.sd" 1 8000 2.9760000705719 "ESPS" "big endian short (16 bits)"
    test-headers
  "addf8.sf_mipseb" 1 8000 2.9760000705719 "IRCAM" "big endian short (16 bits)"
    test-headers
  "addf8.sf_sun" 1 8000 2.9760000705719 "IRCAM" "big endian short (16 bits)"
    test-headers
  "addf8.sf_vax_b" 1 8000 2.9760000705719 "IRCAM" "big endian short (16 bits)"
    test-headers
  "addf8.wav" 1 8000 2.9760000705719 "RIFF" "little endian short (16 bits)"
    test-headers
  "aebass.krz" 1 44100 3.0 "Kurzweil 2000" "big endian short (16 bits)"
    test-headers
  "aiff-16.snd" 2 44100 0.746666669845581 "AIFF" "big endian short (16 bits)"
    test-headers
  "aiff-8.snd" 2 44100 0.746666669845581 "AIFF" "signed byte (8 bits)"
    test-headers
  "alaw.aifc" 1 44100 0.0367800444364548 "AIFC" "alaw (8 bits)" 
    test-headers
  "alaw.wav" 1 11025 8.70666694641113 "RIFF" "alaw (8 bits)"
    test-headers
  "astor_basia.mp2" 2 44100 1.022 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "c.asf" 1 8000 21.368126 "asf" "unknown" test-headers
  "ce-c3.w02" 1 33000 3.88848495483398 "TX-16W" "unknown" test-headers
  "ce-c4.w03" 1 33000 2.91618180274963 "TX-16W" "unknown" test-headers
  "ce-d2.w01" 1 33000 3.46439385414124 "TX-16W" "unknown" test-headers
  "clbonef.wav" 1 22050 2.57832193374634 "RIFF" "little endian float (32 bits)"
    test-headers
  "cranker.krz" 1 44100 3.48267579 "Kurzweil 2000" "big endian short (16 bits)"
    test-headers
  "d40130.aif" 1 10000 0.100000001490116 "AIFF" "big endian short (16 bits)"
    test-headers
  "d40130.au" 1 10000 0.100000001490116 "Sun/Next" "big endian short (16 bits)"
    test-headers
  "d40130.dsf" 1 8000 0.125 "Delusion" "little endian short (16 bits)"
    test-headers
  "d40130.fsm" 1 8000 0.12524999678 "Farandole" "little endian short (16 bits)"
    test-headers
  "d40130.iff" 1 10000 0.100000001490116 "SVX8" "signed byte (8 bits)"
    test-headers
  "d40130.pat" 1 10000 0.100000001490116
    "Gravis Ultrasound patch" "little endian short (16 bits)" test-headers
  "d40130.sds" 1 10000 0.100000001490116 "MIDI sample dump" "unknown"
    test-headers
  "d40130.sdx" 1 10000 0.100000001490116 
    "Sample dump" "unsigned little endian short (16 bits)" test-headers
  "d40130.sf" 1 10000 0.100000001490116
    "IRCAM" "little endian short (16 bits)" test-headers
  "d40130.smp" 1 8000 0.125 "SMP" "little endian short (16 bits)"
    test-headers
  "d40130.sou" 1 8000 0.125 "SBStudioII" "little endian short (16 bits)" 
    test-headers
  "d40130.st3" 1 8000 0.125
    "Digiplayer ST3" "unsigned little endian short (16 bits)" test-headers
  "d40130.uwf" 1 8000 0.1252499 
    "Ultratracker" "little endian short (16 bits)" test-headers
  "d40130.voc" 1 10000 0.100100003182888 "VOC" "unsigned byte (8 bits)"
    test-headers
  "d40130.w00" 1 16000 0.0625 "TX-16W" "unknown" test-headers
  "d40130.wav" 1 10000 0.100000001490116 "RIFF" "little endian short (16 bits)"
    test-headers
  "d43.wav" 1 10000 0.100000001490116 "RIFF" "little endian short (16 bits)"
    test-headers
  "digit0v0.aiff" 1 8000 0.560000002384186 "AIFC" "big endian short (16 bits)" 
    test-headers
  "esps-16.snd" 1 8000 3.09737491607666 "ESPS" "big endian short (16 bits)"
    test-headers
  "forest.aiff" 2 44100 3.907143
    "AIFF" "big endian short (16 bits)" 24981 144332 test-headers-with-loop
  "g721.au" 1 11025 4.35328817367554 "Sun/Next" "unknown" test-headers
  "g722.aifc" 1 44100 0.0184353739023209 "AIFC" "unknown" test-headers
  "gong.wve" 1 8000 3.96799993515015 "PSION" "alaw (8 bits)" test-headers
  "gsm610.wav" 1 11025 1.7687075138092 "RIFF" "unknown" test-headers
  "inrs-16.snd" 1 8000 2.46399998664856 "INRS" "little endian short (16 bits)"
   test-headers
  "kirk.wve" 1 8000 1.40799999237061 "PSION" "alaw (8 bits)" test-headers
  "loop.aiff" 1 44100 0.0367120169103146
    "AIFC" "big endian short (16 bits)" 12 23 test-headers-with-loop
  "m.asf" 1 8000 64.964622 "asf" "unknown" test-headers
  "mary-sun4.sig" 1 8000 4.47612476348877 "Comdisco SPW signal"
    "big endian double (64 bits)" test-headers
  "mocksong.wav" 1 11025 7.869569301605 "RIFF" "little endian short (16 bits)"
    test-headers
  "mono24.wav" 1 22050 1.98997735977173 "RIFF" "little endian int (24 bits)"
    test-headers
  "msadpcm.wav" 1 11025 4.43501138687134 "RIFF" "unknown" test-headers
  "n8.snd" 1 44100 0.0367800444364548 "Sun/Next" "signed byte (8 bits)"
    test-headers
  "nasahal.aif" 1 11025 9.89841270446777 "AIFF" "signed byte (8 bits)" 
    test-headers
  "nasahal.avi" 1 11025 10.432744 "AVI" "little endian short (16 bits)"
    test-headers
  "nasahal.dig" 1 11025 9.8984 "Sound Designer 1" "big endian short (16 bits)"
    test-headers
  "nasahal.ivc" 2 44100 0.449 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "nasahal.pat" 1 11025 3.95410442352295 "Gravis Ultrasound patch"
    "unsigned byte (8 bits)" test-headers
  "nasahal.snd" 1 11025 9.89841270446777 "SNDT" "unsigned byte (8 bits)"
    test-headers
  "nasahal.svx" 1 11025 9.89841270446777 "SVX8" "signed byte (8 bits)"
    test-headers
  "nasahal.v8" 1 8000 13.6412496566772 "Covox V8" "unsigned byte (8 bits)"
    test-headers
  "nasahal.voc" 1 11025 9.89941024780273 "VOC" "unsigned byte (8 bits)" 
    test-headers
  "nasahal.vox" 2 44100 0.22444 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "nasahal8.wav" 1 11025 9.89841270446777 "RIFF" "unsigned byte (8 bits)"
    test-headers
  "nasahalad.smp" 1 11025 4.94920635223389 "Goldwave sample"
    "little endian short (16 bits)" test-headers
  "next-16.snd" 1 22050 1.00004529953003 "Sun/Next"
    "big endian short (16 bits)" test-headers
  "next-8.snd" 1 22050 0.226757362484932 "Sun/Next"
    "signed byte (8 bits)" test-headers
  "next-dbl.snd" 1 22050 0.226757362484932 "Sun/Next"
    "big endian double (64 bits)" test-headers
  "oboe.ldbl" 1 22050 2.30512475967407 "RIFF"
    "little endian double (64 bits)" test-headers
  "next-flt.snd" 1 22050 0.226757362484932 "Sun/Next"
    "big endian float (32 bits)" test-headers
  "aifc-float.snd" 1 22050 0.2267573624849 "AIFC"
    "big endian float (32 bits)" test-headers
  "next-mulaw.snd" 1 8012 2.03295063972473 "Sun/Next"
    "mulaw (8 bits)" test-headers
  "next24.snd" 1 44100 0.0367800444364548 "Sun/Next" 
    "big endian int (24 bits)" test-headers
  "nist-01.wav" 1 16000 2.26912498474121 "NIST" 
    "little endian short (16 bits)" test-headers
  "nist-10.wav" 1 16000 2.26912498474121 "NIST"
    "big endian short (16 bits)" test-headers
  "nist-16.snd" 1 16000 1.02400004863739 "NIST"
    "big endian short (16 bits)" test-headers
  "nist-shortpack.wav" 1 16000 4.53824996948242 "NIST" "unknown" 
    test-headers
  "none.aifc" 1 44100 0.0367800444364548 "AIFC" "big endian short (16 bits)"
    test-headers
  "nylon2.wav" 2 22050 1.14376413822174 "RIFF" "unknown" test-headers
  "o2.adf" 1 44100 0.036780 "CSRE adf" "little endian short (16 bits)"
    test-headers
  "o2.avr" 1 44100 0.0183900222182274 "AVR" "big endian short (16 bits)"
    test-headers
  "o2.bicsf" 1 44100 0.0367800444364548 "IRCAM" "big endian short (16 bits)"
    test-headers
  "o2.mpeg1" 2 44100 0.0070975 "raw (no header)" "big endian short (16 bits)" 
    test-headers
  "o2.sd2" 2 44100 0.0183900222 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "o2.sf2" 1 44100 0.036780044436 "SoundFont" "little endian short (16 bits)"
    test-headers
  "o2.smp" 1 8000 0.202749997377396 "SMP" "little endian short (16 bits)"
    test-headers
  "o2.voc" 1 44100 0.0368934236466885 "VOC" "little endian short (16 bits)"
    test-headers
  "o2.wave" 1 44100 0.0367800444364548 "RIFF" "little endian short (16 bits)"
    test-headers
  "o2_12bit.aiff" 1 44100 0.036780044436 "AIFF" "big endian short (16 bits)"
    test-headers
  "o2_18bit.aiff" 1 44100 0.0367800444364548 "AIFF" "big endian int (24 bits)"
    test-headers
  "o2_711u.wave" 1 44100 0.0367800444364548 "RIFF" "mulaw (8 bits)"
    test-headers
  "o2_722.snd" 1 44100 0.0183900222182274 "Sun/Next" "unknown" test-headers
  "o2_726.aiff" 1 8000 0.0367499999701977 "AIFC" "unknown" test-headers
  "o2_726.snd" 1 44100 0.0230158735066652 "Sun/Next" "unknown" test-headers
  "o2_728.aiff" 1 8000 0.0367499999701977 "AIFC" "unknown" test-headers
  "o2_8.iff" 1 44100 0.0367800444364548 "SVX8" "signed byte (8 bits)"
    test-headers
  "o2_8.voc" 1 44100 0.0370294786989689 "VOC" "unsigned byte (8 bits)"
    test-headers
  "o2_dvi.wave" 1 44100 0.0232199542224407 "RIFF" "unknown" test-headers
  "o2_float.bicsf" 1 44100 0.0367800444 "IRCAM" "big endian float (32 bits)"
    test-headers
  "o2_gsm.aiff" 1 8000 0.0367499999701977 "AIFC" "unknown" test-headers
  "o2_u8.avr" 1 44100 0.0367800444364548 "AVR" "unsigned byte (8 bits)"
    test-headers
  "o2_u8.wave" 1 44100 0.0367800444364548 "RIFF" "unsigned byte (8 bits)"
    test-headers
  "o28.mpc" 1 44100 0.036780 "AKAI 4" "little endian short (16 bits)"
    test-headers
  "oboe.g721" 1 22050 1.15287983417511 "Sun/Next" "unknown" test-headers
  "oboe.g723_24" 1 22050 0.864761888980865 "Sun/Next" "unknown" test-headers
  "oboe.g723_40" 1 22050 1.44126987457275 "Sun/Next" "unknown" test-headers
  "oboe.kts" 1 22050 2.305125 "Korg" "big endian short (16 bits)" test-headers
  "oboe.its" 1 22050 2.305125 "Impulse Tracker" "little endian short (16 bits)"
    test-headers
  "oboe.sf2" 1 22050 2.305124759674 "SoundFont" "little endian short (16 bits)"
    test-headers
  "oboe.paf" 1 22050 2.305125 "Ensoniq Paris" "big endian short (16 bits)"
    test-headers
  "oboe.pf1" 1 22050 2.305125 "Ensoniq Paris" "little endian short (16 bits)"
    test-headers
  "oboe.smp" 1 22050 2.305125 "snack SMP" "little endian short (16 bits)"
    test-headers
  "oboe.rf64" 1 22050 2.305125 "rf64" "little endian short (16 bits)"
    test-headers
  "oboe-be32.caf" 1 22050 2.305125 "caff" "normalized big endian int (32 bits)"
    test-headers
  "oboe-bf64.caf" 1 22050 2.305125 "caff" "big endian double (64 bits)"
    test-headers
  "oboe-lf32.caf" 1 22050 2.305125 "caff" "little endian float (32 bits)"
    test-headers
  "oboe-ulaw.caf" 1 22050 2.305125 "caff" "mulaw (8 bits)" test-headers
  "oboe.nsp" 1 22050 2.305125 "CSL" "little endian short (16 bits)" 
    test-headers
  "oboe-ulaw.voc" 1 22050 2.305669 "VOC" "mulaw (8 bits)" test-headers
  "oboe-lf32.sf" 1 22050 2.305669 "IRCAM" "little endian float (32 bits)"
    test-headers
  "oboe.wfp" 1 22050 2.305125 "Turtle Beach" "little endian short (16 bits)"
    test-headers
  "oboe.sox" 1 22050 2.305125 "Sox" "normalized little endian int (32 bits)"
    test-headers
  "oki.snd" 2 44100 0.004195011 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "oki.wav" 1 44100 0.016780 "RIFF" "unknown" test-headers
  "orv-dvi-adpcm.wav" 1 44100 1.92725622653961 "RIFF" "unknown" test-headers
  "riff-16.snd" 1 22050 1.88766443729401 "RIFF" "little endian short (16 bits)"
    test-headers
  "riff-8-u.snd" 1 11025 0.506848096847534 "RIFF" "unsigned byte (8 bits)" 
    test-headers
  "rooster.wve" 1 8000 2.04800009727478 "PSION" "alaw (8 bits)" test-headers
  "sd1-16.snd" 1 44100 0.40054 "Sound Designer 1" "big endian short (16 bits)"
    test-headers
  "sf-16.snd" 1 22050 1.88766443729401 "IRCAM" "big endian short (16 bits)"
    test-headers
  "si654.adc" 1 16000 6.71362495422363 "ADC/OGI" "big endian short (16 bits)"
    test-headers
  "smp-16.snd" 1 8000 5.2028751373291 "SMP" "little endian short (16 bits)"
    test-headers
  "sound.pat" 1 8000 1.95050001144409 "Gravis Ultrasound patch" 
    "unsigned little endian short (16 bits)" test-headers
  "sound.sap" 1 8000 1.95050001144409 "Goldwave sample"
    "little endian short (16 bits)" test-headers
  "sound.sds" 1 8000 1.95050001144409 "MIDI sample dump" "unknown" test-headers
  "sound.sfr" 1 8000 1.95050001144409 "SRFS" "little endian short (16 bits)"
    test-headers
  "sound.v8" 1 8000 1.95050001144409 "Covox V8" "unsigned byte (8 bits)" 
    test-headers
  "sound.vox" 2 44100 0.0442177 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "step.omf" 1 11025 8.70666694641113 "OMF" "signed byte (8 bits)" test-headers
  "step.qt" 1 11025 8.70630359649658 "Quicktime" "unsigned byte (8 bits)"
    test-headers
  "sun-16-afsp.snd" 1 8000 2.9760000705719 "Sun/Next"
    "big endian short (16 bits)" test-headers
  "sun-mulaw.snd" 1 8000 4.61950016021729 "Sun/Next" "mulaw (8 bits)"
    test-headers
  "sw1038t_short.wav" 2 8000 6.0 "NIST" "mulaw (8 bits)" test-headers
  "swirl.pat" 1 22050 1.0619500875473 "Gravis Ultrasound patch"
    "unsigned little endian short (16 bits)" test-headers
  "sy85.snd" 1 8000 5.05600023269653 "Sy-85" "big endian short (16 bits)"
    test-headers
  "sy99.snd" 1 8000 4.54400014877319 "Sy-99" "big endian short (16 bits)"
    test-headers
  "telephone.wav" 1 16000 2.2788124084 "NIST" "little endian short (16 bits)"
    test-headers
  "trumps22.adp" 1 22050 3.092880 "RIFF" "unknown" test-headers
  "truspech.wav" 1 8000 1.1599999666214 "RIFF" "unknown" test-headers
  "ulaw.aifc" 1 44100 0.0367800444364548 "AIFC" "mulaw (8 bits)" test-headers
  "voc-8-u.snd" 1 8000 1.49937498569489 "VOC" "unsigned byte (8 bits)"
    test-headers
  "o28.voc" 1 44100 0.036893 "VOC" "little endian short (16 bits)" test-headers
  "voxware.wav" 1 8000 0.324000000953674 "RIFF" "unknown" test-headers
  "wd.w00" 1 8000 0.202749997377396 "Sy-99" "big endian short (16 bits)"
    test-headers
  "wd1.smp" 1 8000 0.202749997377396 "SMP" "little endian short (16 bits)"
    test-headers
  "wd1.wav" 1 44100 0.0367800444364548 "RIFF" "little endian short (16 bits)"
    test-headers
  "wheel.mat" 2 44100 0.14564626 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "b8.pvf" 1 44100 0.036803 "Portable Voice Format" "signed byte (8 bits)"
    test-headers
  "b16.pvf" 1 44100 0.0368 "Portable Voice Format" "big endian short (16 bits)"
    test-headers
  "b32.pvf" 1 44100 0.036803 "Portable Voice Format" "big endian int (32 bits)"
    test-headers
  "water.voc" 2 32000 42.3463897705078 "VOC" "little endian short (16 bits)"
    test-headers
  "wood.dsf" 1 8000 0.202749997377 "Delusion" "little endian short (16 bits)"
    test-headers
  "wood.dvi" 1 22100 0.0278733037412167 "RIFF" "unknown" test-headers
  "wood.dwd" 1 22100 0.0733936652541161 "DiamondWare" "signed byte (8 bits)"
    test-headers
  "wood.fsm" 1 8000 0.2029999942 "Farandole" "little endian short (16 bits)"
    test-headers
  "wood.mad" 1 22100 0.0372398197650909 "RIFF" "unknown" test-headers
  "wood.maud" 1 44100 0.0183900222182274 "MAUD" "big endian short (16 bits)"
    test-headers
  "wood.pat" 1 22100 0.0733936652541161 "Gravis Ultrasound patch"
    "little endian short (16 bits)" test-headers
  "wood.riff" 1 44100 0.0367800444364548 "RIFF" "little endian short (16 bits)"
    test-headers
  "wood.rifx" 1 44100 0.0367800444364548 "RIFF" "big endian short (16 bits)"
    test-headers
  "wood.sds" 1 22100 0.0733936652541161 "MIDI sample dump" "unknown" 
    test-headers
  "wood.sdx" 1 22100 0.0733936652541161 "Sample dump"
    "unsigned little endian short (16 bits)" test-headers
  "wood.sf" 1 44100 0.0367800444364548 "IRCAM" "big endian short (16 bits)"
    test-headers
  "wood.sndr" 2 44100 0.009229 "raw (no header)" "big endian short (16 bits)"
    test-headers
  "wood.sndt" 1 44100 0.0367800444364548 "SNDT" "unsigned byte (8 bits)"
    test-headers
  "wood.st3" 1 8000 0.202749997377396 "Digiplayer ST3"
    "unsigned little endian short (16 bits)" test-headers
  "wood.uwf" 1 8000 0.202999994 "Ultratracker" "little endian short (16 bits)"
    test-headers
  "wood.w00" 1 16000 0.101374998688698 "TX-16W" "unknown" test-headers
  "wood12.aiff" 1 44100 0.0367800444364548 "AIFF" "big endian short (16 bits)"
    test-headers
  "wood16.dwd" 2 44100 0.03678004 "DiamondWare" "little endian short (16 bits)"
    test-headers
  "wood16.wav" 2 44100 0.03678004 "RIFF" "little endian short (16 bits)"
    test-headers
  "wood16.nsp" 2 44100 0.03678004 "CSL" "little endian short (16 bits)"
    test-headers
  "wood16.smp" 2 44100 0.03678004 "snack SMP" "little endian short (16 bits)"
    test-headers
  "wood24.aiff" 1 44100 0.0367800444364548 "AIFF" "big endian int (24 bits)"
    test-headers
  "woodblock.aiff" 1 44100 0.03678 "AIFF" "big endian short (16 bits)"
    test-headers
  "woodflt.snd" 1 44100 0.0367800444364548 "Sun/Next"
    "big endian float (32 bits)" test-headers
  "RealDrums.sf2" 1 44100 6.397256 "SoundFont" "little endian short (16 bits)"
    test-headers
  "32bit.sf" 1 44100 4.6 "IRCAM" "little endian float (32 bits, unscaled)"
    test-headers
  "PCM_48_8bit_m.w64" 1 48000 0.375 "SoundForge" "unsigned byte (8 bits)"
    test-headers
  "oboe.sf6" 1 22050 2.305125 "SoundForge" "little endian short (16 bits)"
    test-headers
  "addf8.24we" 1 8000 2.976000 "RIFF" "little endian int (24 bits)"
    test-headers
  "hybrid.snd" 1 44100 4.600000 "BICSF" "big endian float (32 bits)"
    test-headers
  "litmanna.sf" 1 44100 0.533 "IRCAM" "little endian short (16 bits)"
    test-headers
  "M1F1-float64C-AFsp.aif" 2 8000 2.9366 "AIFC" "big endian double (64 bits)"
    test-headers
  "MacBoing.wav" 1 11127 0.696 "RIFF" "unsigned byte (8 bits)" test-headers
  "t15.aiff" 2 44100 135.00 "AIFC" "little endian short (16 bits)" test-headers
  "tomf8.aud" 1 8000 2.016000 "INRS" "little endian short (16 bits)"
    test-headers
  "Xhs001x.nsp" 1 10000 6.017400 "CSL" "little endian short (16 bits)" 
    test-headers
  "zulu_a4.w11" 1 33000 1.21987879276276 "TX-16W" "unknown" 23342 40042
    test-headers-with-loop
;

\ ---------------- test 03: variables ----------------

: y>0.1-cb <{ y -- f }> y 0.1 f> ;
: y<0.0-cb <{ y -- f }> y f0< ;

'( 0 0.0 50 0.5 100 1.0 ) value zero-to-one
'( 0 1.0 50 0.5 100 0.0 ) value mod-down

: rm-ladspa <{ sym reg -- f }> reg sym symbol-name regexp= ;

: 03-variables ( -- )
  "oboe.snd" open-sound { ind }
  *home* "/test" $+ { test-dir }
  test-dir file-exists? if
    temp-dir { old-val }
    test-dir set-temp-dir test-dir "set-temp-dir" #() snd-test-neq
    old-val set-temp-dir drop
  then
  1000 sample 0.0328 "sample 1000" #() snd-test-neq
  \
  #( output-comment-hook
     help-hook
     mark-drag-hook
     mix-drag-hook
     mouse-drag-hook
     mouse-click-hook
     mouse-press-hook
     start-playing-hook
     start-playing-selection-hook
     stop-playing-hook
     key-press-hook
     snd-error-hook
     snd-warning-hook
     name-click-hook
     after-apply-controls-hook
     enved-hook
     mouse-enter-label-hook
     mouse-enter-graph-hook
     mouse-enter-listener-hook
     mouse-leave-label-hook
     mouse-leave-graph-hook
     mouse-leave-listener-hook
     initial-graph-hook
     after-graph-hook
     graph-hook ) each { h }
    h hook? not
    h empty? not || if
      "%d: %s?" #( i h ) snd-display
    then
  end-each
  \
  *with-test-gui* if
    show-controls { old-ctrl }
    #t set-show-controls drop
    enved-dialog { req }
    dialog-widgets 1 array-ref req "enved-dialog" #() snd-test-neq
    '( 0.0 0.0 1.0 1.0 2.0 0.0 ) to req
    req set-enved-envelope drop
    enved-envelope req "set-enved-envelope" #() snd-test-neq
    enved-envelope set-enved-envelope drop
    enved-envelope req "set-enved-envelope to self" #() snd-test-neq
    old-ctrl set-show-controls drop
  then
  \
  #( #( <'> color-cutoff 0.003 0.01 )
     #( <'> color-inverted #t #f )
     #( <'> color-scale 1.0 0.5 )
     #( <'> contrast-control? #f #t )
     #( <'> enved-base 1.0 1.5 )
     #( <'> enved-in-dB #f #t )
     #( <'> enved-target 0 1 )
     #( <'> enved-wave? #f #t )
     #( <'> expand-control? #f #t )
     #( <'> fft-log-frequency #f #t )
     #( <'> fft-log-magnitude #f #t )
     #( <'> fft-with-phases #f #t )
     #( <'> enved-filter-order 40 20 )
     #( <'> filter-control? #f #t )
     #( <'> transform-normalization normalize-by-channel dont-normalize )
     #( <'> reverb-control? #f #t )
     #( <'> show-transform-peaks #f #t )
     #( <'> show-selection-transform #f #t )
     #( <'> spectrum-end 1.0 0.7 )
     #( <'> spectro-hop 4 10 )
     #( <'> spectrum-start 0.0 0.1 )
     #( <'> spectro-x-angle *with-test-gl* if 300.0 else 90.0 then 60.0 )
     #( <'> spectro-x-scale *with-test-gl* if 1.5 else 1.0 then 2.0 )
     #( <'> spectro-y-angle *with-test-gl* if 320.0 else 0.0 then 60.0 )
     #( <'> spectro-y-scale 1.0 2.0 )
     #( <'> spectro-z-angle *with-test-gl* if 0.0 else 358.0 then 60.0 )
     #( <'> spectro-z-scale *with-test-gl* if 1.0 else 0.1 then 0.2 )
       ) { gui-lst }
  #( #( <'> amp-control 1.0 0.5 )
     #( <'> amp-control-bounds '( 0.0 8.0 ) '( 1.0 5.0 ) )
     #( <'> ask-about-unsaved-edits #f #t )
     #( <'> ask-before-overwrite #f #t )
     #( <'> auto-resize #t #f )
     #( <'> auto-update #f #t )
     #( <'> channel-style 0 1 )
     #( <'> colormap *good-colormap* *better-colormap* )
     #( <'> contrast-control 0.0 0.5 )
     #( <'> contrast-control-bounds '( 0.0 10.0 ) '( 1.0 5.0 ) )
     #( <'> contrast-control-amp 1.0 0.5 )
     #( <'> auto-update-interval 60.0 120.0 )
     #( <'> cursor-update-interval 0.05 0.1 )
     #( <'> cursor-location-offset 0 32768 )
     #( <'> with-tracking-cursor #f #t )
     #( <'> cursor-size 15 30 )
     #( <'> cursor-style cursor-cross cursor-line )
     #( <'> tracking-cursor-style cursor-line cursor-cross )
     #( <'> dac-combines-channels #t #f )
     #( <'> dac-size 256 512 )
     #( <'> clipping #f #t )
     #( <'> default-output-chans 1 2 )
     #( <'> default-output-sample-type 1 1 )
     #( <'> default-output-srate 22050 44100 )
     #( <'> default-output-header-type mus-next mus-aifc )
     #( <'> dot-size 1 4 )
     #( <'> enved-clip? #f #t )
     #( <'> enved-style envelope-linear envelope-exponential )
     #( <'> enved-power 3.0 3.5 )
     #( <'> eps-file "snd.eps" "snd-1.eps" )
     #( <'> eps-left-margin 0.0 72.0 )
     #( <'> eps-size 1.0 2.0 )
     #( <'> eps-bottom-margin 0.0 36.0 )
     #( <'> expand-control 1.0 2.0 )
     #( <'> expand-control-bounds '( 0.001 20.0 ) '( 1.0 2.0 ) )
     #( <'> expand-control-hop 0.05 0.1 )
     #( <'> expand-control-jitter 0.1 0.2 )
     #( <'> expand-control-length 0.15 0.2 )
     #( <'> expand-control-ramp 0.4 0.2 )
     #( <'> fft-window-alpha 0.0 1.0 )
     #( <'> fft-window-beta 0.0 0.5 )
     #( <'> transform-size 512 1024 )
     #( <'> transform-graph-type graph-once graph-as-sonogram )
     #( <'> fft-window 6 5 )
     #( <'> transform-graph? #f #t )
     #( <'> filter-control-in-dB #f #t )
     #( <'> filter-control-envelope '( 0.0 1.0 1.0 1.0 ) '( 0.0 1.0 1.0 0.0 ) )
     #( <'> enved-filter #t #f )
     #( <'> filter-control-in-hz #f #t )
     #( <'> filter-control-order 20 40 )
     #( <'> graph-cursor 34 32 )
     #( <'> graph-style 0 1 )
     #( <'> initial-beg 0.0 1.0 )
     #( <'> initial-dur 0.1 1.0 )
     #( <'> just-sounds #f #t )
     #( <'> listener-prompt ">" ":" )
     #( <'> max-transform-peaks 100 10 )
     #( <'> max-regions 16 6 )
     #( <'> min-dB -60.0 -90.0 )
     #( <'> log-freq-start 32.0 10.0 )
     #( <'> mix-waveform-height 20 40 )
     #( <'> mix-tag-height 14 20 )
     #( <'> mix-tag-width 6 20 )
     #( <'> mark-tag-height 4 20 )
     #( <'> mark-tag-width 10 20 )
     #( <'> mus-clipping #f #t )
     #( <'> selection-creates-region #t #f )
     #( <'> play-arrow-size 10 16 )
     #( <'> print-length 12 16 )
     #( <'> region-graph-style graph-lines graph-lollipops )
     #( <'> reverb-control-decay 1.0 2.0 )
     #( <'> reverb-control-feedback 1.09 1.6 )
     #( <'> reverb-control-length 1.0 2.0 )
     #( <'> reverb-control-length-bounds '( 0.0 0.5 ) '( 1.0 2.0 ) )
     #( <'> reverb-control-lowpass 0.7 0.9 )
     #( <'> reverb-control-scale 0.0 0.2 )
     #( <'> reverb-control-scale-bounds '( 0.0 4.0 ) '( 0.0 0.2 ) )
     #( <'> show-axes 1 0 )
     #( <'> show-full-duration #f #t )
     #( <'> show-full-range #f #t )
     #( <'> show-indices #f #t )
     #( <'> show-marks #t #f )
     #( <'> show-mix-waveforms #t #f )
     #( <'> show-y-zero #f #t )
     #( <'> show-grid #f #t )
     #( <'> grid-density 1.0 0.5 )
     #( <'> show-sonogram-cursor #f #t )
     #( <'> sinc-width 10 40 )
     #( <'> speed-control 1.0 0.5 )
     #( <'> speed-control-bounds '( 0.05 20.0 ) '( 1.0 5.0 ) )
     #( <'> speed-control-style 0 1 )
     #( <'> speed-control-tones 12 18 )
     #( <'> sync 0 1 )
     #( <'> sync-style sync-by-sound sync-all )
     #( <'> tiny-font tiny-font-string tiny-font-set-string )
     #( <'> transform-type fourier-transform autocorrelation )
     #( <'> with-verbose-cursor #f #t )
     #( <'> wavelet-type 0 1 )
     #( <'> time-graph? #f #t )
     #( <'> time-graph-type graph-once graph-as-wavogram )
     #( <'> wavo-hop 3 6 )
     #( <'> wavo-trace 64 128 )
     #( <'> with-mix-tags #t #f )
     #( <'> with-relative-panes #t #f )
     #( <'> with-gl *with-test-gl* #f )
     #( <'> x-axis-style 0 1 )
     #( <'> beats-per-minute 30.0 120.0 )
     #( <'> beats-per-measure 1 120 )
     #( <'> zero-pad 0 1 )
     #( <'> zoom-focus-style 2 1 ) ) { lst }
  *with-test-gui* if
    lst gui-lst array-append to lst
  then
  nil nil nil nil nil { vals sym initval newval nowval }
  lst each to vals
    vals 0 array-ref to sym
    vals 1 array-ref to initval
    vals 2 array-ref to newval
    2 0 do
      newval sym set-execute drop
      sym execute to nowval
      nowval newval "set-%s[%d]" #( sym i ) snd-test-neq
      initval sym set-execute drop
    loop
  end-each
  \
  #( *with-test-gui* if
       #( <'> amp-control 1.0 '( -1.0 123.123 ) )
     then
     #( <'> amp-control-bounds '( 0.0 8.0 )
       '( #f '( 0.0 ) '( 1.0 0.0 ) 2.0 ) )
     #( <'> channel-style 0 '( 32 -1 1.0 ) )
     #( <'> colormap *good-colormap* '( 321 -123 ) )
     #( <'> color-cutoff 0.003 '( -1.0 123.123 ) )
     #( <'> color-scale 1.0 '( -32.0 2000.0 ) )
     *with-test-gui* if
       #( <'> contrast-control 0.0 '( -123.123 123.123 ) )
     then
     #( <'> contrast-control-bounds '( 0.0 10.0 )
       '( #f '( 0.0 ) '( 1.0 0.0 ) 2.0 ) )
     #( <'> cursor-size 15 '( 1.123 -2.5 ) )
     #( <'> dac-size 256 '( -1 0 -123 ) )
     #( <'> dot-size 1 '( 0 -1 -123 ) )
     #( <'> enved-target 0 '( 123 -321 ) )
     #( <'> expand-control 1.0 '( -1.0 0.0 ) )
     #( <'> expand-control-bounds '( 0.001 20.0 )
       '( #f '( 0.0 ) '( 1.0 0.0 ) 2.0 ) )
     #( <'> expand-control-hop 0.05 '( -1.0 ) )
     #( <'> expand-control-length 0.15 '( -1.0 0.0 ) )
     #( <'> expand-control-ramp 0.4 '( -1.0 1.0 123.123 ) )
     #( <'> fft-window-alpha 0.0 '( -1.0 123.123 ) )
     #( <'> fft-window-beta 0.0 '( -1.0 123.123 ) )
     #( <'> transform-size 512 '( -1 0 ) )
     #( <'> zero-pad 0 '( -1 -123 ) )
     #( <'> cursor-style cursor-cross '( -1 ) )
     #( <'> cursor-style cursor-line '( 2 123 ) )
     #( <'> tracking-cursor-style cursor-line '( -1 ) )
     #( <'> tracking-cursor-style cursor-line '( 2 123 ) )
     #( <'> transform-graph-type graph-once '( -1 123 ) )
     #( <'> fft-window 6 '( -1 123 ) )
     #( <'> enved-filter-order 40 '( -1 0 ) )
     #( <'> filter-control-order 20 '( -10 -1 0 ) )
     #( <'> max-transform-peaks 100 '( -1 ) )
     #( <'> max-regions 16 '( -1 -123 ) )
     #( <'> reverb-control-length 1.0 '( -1.0 ) )
     #( <'> show-axes 1 '( -1 123 ) )
     #( <'> sinc-width 10 '( -10 ) )
     #( <'> spectrum-end 1.0 '( -1.0 ) )
     #( <'> spectro-hop 4 '( -10 -1 0 ) )
     #( <'> spectrum-start 0.0 '( -1.0 ) )
     #( <'> speed-control 1.0 '( 0.0 ) )
     #( <'> speed-control-bounds '( 0.05 20.0 )
       '( #f '( 0.0 ) '( 1.0 0.0 ) 2.0 ) )
     #( <'> speed-control-style 0 '( -1 10 ) )
     #( <'> sync-style sync-by-sound '( -1 123 ) )
     #( <'> transform-type fourier-transform
       '( -1 integer->transform 123 integer->transform ) )
     #( <'> wavelet-type 0 '( -1 123 ) )
     #( <'> wavo-hop 1 '( 0 -123 ) )
     #( <'> wavo-trace 1 '( 0 -123 ) )
     #( <'> x-axis-style 0 '( -1 123 ) )
     #( <'> zoom-focus-style 2 '( -1 123 ) ) ) to lst
  nil { newvals }
  lst each to vals
    vals 0 array-ref to sym
    vals 1 array-ref to initval
    vals 2 array-ref to newvals
    newvals each to newval
      newval sym <'> set-execute snd-test-catch drop
      sym execute to nowval
      nowval newval "set-%s (bad set)" #( sym ) snd-test-eq
      initval sym set-execute drop
    end-each
  end-each
  \
  *with-test-gui* if
    sync-none set-sync-style drop
    300 set-window-width drop
    300 set-window-height drop
    window-width  300 "window-width"  #() snd-test-neq
    window-height 300 "window-height" #() snd-test-neq
    color-scale { old-val }
    100.0 set-color-scale drop
    color-scale 100.0 "color-scale" #() snd-test-neq
    old-val set-color-scale drop
  then
  \
  search-procedure proc? if
    "global search procedure: %s?" #( search-procedure ) snd-display
  then
  <'> y>0.1-cb set-search-procedure drop
  search-procedure proc? unless
    "set global search procedure: %s?" #( search-procedure ) snd-display
  then
  search-procedure #( 0.2 ) run-proc unless
    "search 0.1 > 0.2?" #() snd-display
  then
  search-procedure #( 0.02 ) run-proc if
    "search 0.1 > 0.02?" #() snd-display
  then
  <'> y<0.0-cb set-search-procedure drop
  search-procedure #( 0.02 ) run-proc if
    "search 0.0 < 0.02?" #() snd-display
  then
  #f set-search-procedure drop
  search-procedure proc? if
    "global search procedure after reset: %s?" #( search-procedure ) 
      snd-display
  then
  <'> y>0.1-cb set-search-procedure drop
  search-procedure proc? unless
    "set global search procedure: %s?" #( search-procedure ) snd-display
  then
  #f set-search-procedure drop
  \
  *with-test-gui* if
    enved-filter-order { old-val }
    5 set-enved-filter-order drop
    enved-filter-order 6 "set-enved-filter-order 5" #() snd-test-neq
    old-val set-enved-filter-order drop
    \ XXX: This works with global variables. [ms]
    'zero-to-one <'> set-enved-envelope snd-test-catch drop
    enved-envelope zero-to-one "set-enved-envelope (symbol)" #() snd-test-neq
    "mod-down"   <'> set-enved-envelope snd-test-catch drop
    enved-envelope mod-down    "set-enved-envelope (string)" #() snd-test-neq
  then
  ind close-sound drop
  dismiss-all-dialogs
  #() { undefined }
  \ XXX: changes from original snd-test.scm list [ms]:
  \
  \ removed (Scheme specific):
  \   'add-clm-field (run.c)
  \   'run           (macro in run.c)
  \   'file->string  (snd-utils.c)
  \
  \ removed (Forth specific):
  \   'abort         (forth word)
  \
  \ added:
  \   'snd-exit      (xen.c; in Forth exit is already in use)
  #( '*snd-opened-sound* 'add-colormap 'add-mark
     'add-player 'add-sound-file-extension 'add-source-file-extension
     'add-to-main-menu 'add-to-menu 'add-transform 'after-apply-controls-hook
     'after-edit-hook 'after-graph-hook 'after-lisp-graph-hook 'after-open-hook
     'after-save-as-hook 'after-save-state-hook 'after-transform-hook
     'all-pass 'all-pass? 'amp-control 'amp-control-bounds 'amplitude-modulate
     'analyse-ladspa 'apply-controls 'apply-ladspa 'array->file 'array-interp
     'as-one-edit 'ask-about-unsaved-edits 'ask-before-overwrite
     'asymmetric-fm 'asymmetric-fm?
     'auto-resize 'auto-update 'auto-update-interval 'autocorrelate 
     'autocorrelation 'axis-color
     'axis-info 'axis-label-font 'axis-numbers-font 'bad-header-hook 
     'bartlett-window 'bartlett-hann-window 'basic-color 'beats-per-measure 
     'beats-per-minute 'before-close-hook 'before-exit-hook 
     'before-save-as-hook 'before-save-state-hook 'before-transform-hook
     'bind-key 'blackman2-window 'blackman3-window 'blackman4-window
     'blackman5-window 'blackman6-window 'blackman7-window 'blackman8-window
     'blackman9-window 'blackman10-window 'bohman-window 'bold-peaks-font
     'cauchy-window 'mlt-sine-window 'cepstrum 'change-samples-with-origin
     'channel->vct 'channel-amp-envs 'channel-data 'channel-properties 
     'channel-property 'channel-style 'channel-widgets 'channels
     'channels-combined 'channels-separate 'channels-superimposed 
     'chans 'clear-listener 'clip-hook
     'clipping 'clm-channel 'clm-table-size 
     'close-hook 'close-sound 'color->list
     'color-cutoff 'color-orientation-dialog 'color-hook 'color-inverted
     'color-scale 'color?  'colormap 'colormap-name 'colormap-ref
     'colormap-size 'colormap? 'comb 'comb?  'combined-data-color
     'comment 'connes-window 'continue-frample->file 'continue-sample->file
     'contrast-control 'contrast-control-amp 'contrast-control-bounds
     'contrast-control? 'contrast-enhancement 'controls->channel
     'convolution 'convolve 'convolve-files 'convolve-selection-with
     'convolve-with 'convolve? 'copy-context 'copy-sampler
     'current-edit-position 'current-font 'cursor 'cursor-color
     'cursor-context 'cursor-cross 'cursor-in-middle 'cursor-in-view
     'cursor-line 'cursor-location-offset 'cursor-on-left 'cursor-on-right
     'cursor-position 'cursor-size 'cursor-style 'cursor-update-interval
     'dac-combines-channels 'dac-size 'data-color 'sample-type
     'data-location 'data-size 'db->linear 'default-output-chans 
     'default-output-sample-type 'default-output-header-type
     'default-output-srate 'define-envelope 'degrees->radians 'delay
     'delay-tick 'delay? 'delete-colormap
     'delete-mark 'delete-marks 'delete-sample 
     'delete-samples 'delete-samples-and-smooth 'delete-selection
     'delete-selection-and-smooth 'delete-transform 'dialog-widgets
     'disk-kspace 'display-edits 'dolph-chebyshev-window 'dont-normalize
     'dot-product 'dot-size 'draw-axes 'draw-dot 'draw-dots 'draw-line
     'draw-lines 'draw-mark-hook 'draw-mix-hook 'draw-string 'drop-hook
     'during-open-hook 'edit-fragment 'edit-header-dialog 'edit-hook
     'edit-list->function 'edit-position 'edit-tree 'edits 'edot-product
     'env 'env-channel 'env-channel-with-base 'env-interp 'env-selection
     'env-sound 'env? 'enved-add-point 'enved-amplitude 'enved-base
     'enved-clip?  'enved-delete-point 'enved-dialog 'enved-envelope 
     'enved-filter 'enved-filter-order 'enved-hook 'enved-in-dB 
     'enved-move-point 'enved-power 'enved-spectrum 'enved-srate 
     'enved-style 'enved-target 'enved-wave? 'enved-waveform-color
     'envelope-exponential 'envelope-linear 'eps-bottom-margin 'eps-file 
     'eps-left-margin 'eps-size 'exit 'exit-hook 'expand-control
     'expand-control-bounds 'expand-control-hop 'expand-control-jitter
     'expand-control-length 'expand-control-ramp 'expand-control?
     'exponential-window 'fft 'fft-log-frequency 'fft-log-magnitude 
     'fft-window 'fft-window-alpha 'fft-window-beta 'fft-with-phases
     'file->array 'file->frample 'file->frample? 'file->sample 'file->sample?
     'file-name 'file-write-date 'fill-polygon 'fill-rectangle 'filter
     'filtered-comb 'filtered-comb? 'filter-channel 'filter-control-coeffs
     'filter-control-envelope 'filter-control-in-dB 'filter-control-in-hz
     'filter-control-order 'filter-control-waveform-color 'filter-control?
     'filter-selection 'filter-sound 'filter? 'find-dialog
     'find-mark 'find-sound 'finish-progress-report 'fir-filter 'fir-filter?
     'flat-top-window 'focus-widget 'foreground-color 'forget-region 
     'formant 'formant-bank 'formant-bank? 'formant? 'firmant 'firmant?
     'comb-bank 'comb-bank? 'all-pass-bank 'all-pass-bank? 'filtered-comb-bank
     'filtered-comb-bank? 'make-comb-bank 'make-all-pass-bank
     'make-filtered-comb-bank 'fourier-transform
     'frample->file 'frample->file? 'frample->frample 'framples 
     'free-player 'free-sampler 'gaussian-window 'gc-off 'gc-on 
     'gl-graph->ps 'glSpectrogram 'goto-listener-end 'granulate 'granulate?
     'graph 'graph->ps 'graph-as-sonogram 'graph-as-spectrogram
     'graph-as-wavogram 'graph-color 'graph-cursor 'graph-data 'graph-dots
     'graph-dots-and-lines 'graph-filled 'graph-hook 'graph-lines
     'graph-lollipops 'graph-once 'graph-style 'graphs-horizontal
     'grid-density 'haar-transform 'hamming-window 'hann-poisson-window
     'hann-window 'header-type 'help-dialog 'help-hook 'hide-widget
     'highlight-color 'html-dir 'html-program 'hz->radians 'iir-filter
     'iir-filter? 'in 'in-any 'ina 'inb 'info-dialog
     'init-ladspa 'initial-graph-hook 'insert-file-dialog 'insert-region
     'insert-sample 'insert-samples 'insert-samples-with-origin
     'insert-selection 'insert-silence 'insert-sound 'just-sounds 
     'kaiser-window 'key 'key-binding 'key-press-hook 'keyboard-no-action
     'ladspa-activate 'ladspa-cleanup 'ladspa-connect-port 'ladspa-deactivate
     'ladspa-descriptor 'ladspa-dir 'peak-env-dir 'ladspa-instantiate 
     'ladspa-run 'ladspa-run-adding 'ladspa-set-run-adding-gain 'left-sample
     'linear->db 'lisp-graph 'lisp-graph-hook 'lisp-graph-style 'lisp-graph?
     'list->vct 'list-ladspa 'listener-click-hook 'listener-color
     'listener-font 'listener-prompt 'listener-selection 
     'listener-text-color 'little-endian? 'locsig 'locsig-ref
     'locsig-reverb-ref 'locsig-reverb-set! 'locsig-set! 'locsig-type
     'locsig? 'log-freq-start 'main-menu 'main-widgets 'make-all-pass
     'make-asymmetric-fm 'make-moving-average 'make-moving-max
     'make-bezier 'make-color 'make-comb 'make-filtered-comb 'make-convolve
     'make-delay 'make-env 'make-fft-window 'make-file->frample
     'make-file->sample 'make-filter 'make-fir-coeffs 'make-fir-filter
     'make-formant 'make-firmant 'make-formant-bank
     'make-frample->file 'make-granulate 'make-graph-data
     'make-iir-filter 'make-locsig 'make-mix-sampler
     'make-move-sound 'make-notch 'make-one-pole
     'make-one-pole-all-pass 'make-one-zero
     'make-oscil 'make-phase-vocoder 'make-player 'make-polyshape 
     'make-polywave 'make-pulse-train 'make-rand 'make-rand-interp
     'make-readin 'make-region 'make-region-sampler 'make-sample->file
     'make-sampler 'make-sawtooth-wave 'make-nrxysin
     'make-nrxycos 'make-rxyk!cos 'make-rxyk!sin
     'make-snd->sample 'make-square-wave 
     'make-src 'make-ssb-am 'make-ncos 'make-nsin 'make-table-lookup
     'make-triangle-wave 'make-two-pole 'make-two-zero
     'make-variable-graph 'make-vct 'make-wave-train 'map-chan
     'map-channel 'mark-click-hook 'mark-color 'mark-context
     'mark-drag-hook 'mark-home 'mark-hook 'mark-name 'mark-properties
     'mark-property 'mark-sample 'mark-sync 'mark-sync-max
     'mark-tag-height 'mark-tag-width 'mark? 'marks 'max-regions
     'max-transform-peaks 'maxamp 'maxamp-position 'menu-widgets
     'min-dB 'mix 'mix-amp 'mix-amp-env 'mix-click-hook 'mix-color
     'mix-dialog-mix 'mix-drag-hook 'mix-file-dialog 'mix-length
     'mix-home 'mix-name 'mix-position 'mix-properties 'mix-property
     'mix-region 'mix-release-hook 'mix-sync 'mix-sync-max 'mix-sampler?
     'mix-selection 'mix-speed 'mix-tag-height 'mix-tag-width 'mix-tag-y
     'mix-vct 'mix-waveform-height 'mix? 'mixes 'mouse-click-hook 
     'mouse-drag-hook 'mouse-enter-graph-hook 'mouse-enter-label-hook
     'mouse-enter-listener-hook 'mouse-enter-text-hook
     'mouse-leave-graph-hook 'mouse-leave-label-hook
     'mouse-leave-listener-hook 'mouse-leave-text-hook 
     'mouse-press-hook 'move-locsig 'move-sound 'move-sound?
     'moving-average 'moving-average? 'moving-max 'moving-max?
     'mus-aifc 'mus-aiff 'mus-alaw 'mus-alsa-buffer-size
     'mus-alsa-buffers 'mus-alsa-capture-device 'mus-alsa-device
     'mus-alsa-playback-device 'mus-alsa-squelch-warning 'mus-apply
     'mus-array-print-length 'mus-float-equal-fudge-factor 'mus-b24int
     'mus-bdouble 'mus-bdouble-unscaled 'mus-bfloat 'mus-bfloat-unscaled
     'mus-bicsf 'mus-bint 'mus-bintn 'mus-bshort 'mus-byte
     'mus-bytes-per-sample 'mus-caff 'mus-channel 'mus-channels
     'mus-chebyshev-first-kind 'mus-chebyshev-second-kind
     'mus-clipping 'mus-close 'mus-data 'mus-sample-type->string
     'mus-sample-type-name 'mus-describe 'mus-error-hook
     'mus-error-type->string 'mus-expand-filename 'mus-feedback
     'mus-feedforward 'mus-fft 'mus-file-buffer-size 'mus-file-clipping
     'mus-file-name 'mus-frequency 'mus-generator?
     'mus-header-raw-defaults 'mus-header-type->string
     'mus-header-type-name 'mus-hop 'mus-increment 'mus-input? 
     'mus-interp-all-pass 'mus-interp-bezier 'mus-interp-hermite 
     'mus-interp-lagrange 'mus-interp-linear 'mus-interp-none
     'mus-interp-sinusoidal 'mus-interp-type 'mus-interpolate
     'mus-ircam 'mus-l24int 'mus-ldouble 'mus-ldouble-unscaled
     'mus-length 'mus-lfloat 'mus-lfloat-unscaled 'mus-lint 'mus-lintn 
     'mus-location 'mus-lshort 'mus-max-malloc 'mus-max-table-size 
     'mus-mulaw 'mus-name 'mus-next 'mus-nist 'mus-offset 
     'mus-order 'mus-oss-set-buffers 'mus-out-format 'mus-output?
     'mus-phase 'mus-ramp 'mus-rand-seed 'mus-random
     'mus-raw 'mus-reset 'mus-riff 'mus-run 'mus-scaler
     'mus-set-formant-radius-and-frequency 'mus-sound-chans
     'mus-sound-comment 
     'mus-sound-sample-type 'mus-sound-data-location
     'mus-sound-datum-size 'mus-sound-duration 'mus-sound-forget 
     'mus-sound-framples 'mus-sound-header-type 'mus-sound-length 
     'mus-sound-loop-info 'mus-sound-mark-info 'mus-sound-maxamp 
     'mus-sound-maxamp-exists?
     'mus-sound-prune
     'mus-sound-report-cache 'mus-sound-samples
     'mus-sound-srate 'mus-sound-type-specifier
     'mus-sound-write-date 'mus-soundfont 'mus-srate 'mus-svx 'mus-ubshort
     'mus-ubyte 'mus-ulshort 'mus-unknown-sample 'mus-unknown-header
     'mus-voc 'mus-width 'mus-xcoeff 'mus-xcoeffs 'mus-ycoeff
     'mus-ycoeffs 'name-click-hook 'new-sound 'new-sound-dialog 'new-sound-hook
     'new-widget-hook 'next-sample 'normalize-by-channel 'normalize-by-sound
     'normalize-channel 'normalize-globally 'notch 'notch? 'one-pole
     'one-pole? 'one-pole-all-pass 'one-pole-all-pass?
     'one-zero 'one-zero? 'open-file-dialog
     'open-file-dialog-directory 'open-hook 'open-raw-sound 
     'open-raw-sound-hook 'open-sound 'orientation-hook 'oscil
     'oscil? 'out-any 'outa 'outb 'outc 'outd 'output-comment-hook
     'override-samples-with-origin 'pad-channel
     'partials->polynomial 'partials->wave 'parzen-window 'pausing
     'peaks 'peaks-font 'phase-partials->wave
     'phase-vocoder 'phase-vocoder-amp-increments 'phase-vocoder-amps
     'phase-vocoder-freqs 'phase-vocoder-phase-increments 
     'phase-vocoder-phases 'phase-vocoder? 'play 'play-arrow-size
     'play-hook 'player-home 'player? 'players 'playing
     'poisson-window 'polar->rectangular 'polynomial 'polyshape 
     'polywave 'polyshape? 'polywave? 'position->x 'position->y
     'position-color 'preferences-dialog 'previous-sample 'print-dialog
     'print-length 'progress-report 'pulse-train 'pulse-train?
     'radians->degrees 'radians->hz 'ramp-channel 'rand 'rand-interp
     'rand-interp? 'rand? 'read-mix-sample 'read-only 
     'read-region-sample 'read-sample 'readin 'readin? 
     'rectangular->magnitudes 'rectangular->polar 'rectangular-window
     'redo 'redo-edit 'region->vct 'region-chans 'region-home 
     'region-framples 'region-graph-style 'region-maxamp 'region-maxamp-position
     'region-position 'region-sample 'region-sampler? 'region-srate 'region?
     'regions 'remember-sound-state 'remove-from-menu 'reset-controls
     'reset-listener-cursor 'restore-controls 'restore-region
     'reverb-control-decay 'reverb-control-feedback 'reverb-control-length
     'reverb-control-length-bounds 'reverb-control-lowpass 'reverb-control-scale
     'reverb-control-scale-bounds 'reverb-control?  'reverse-channel
     'reverse-selection 'reverse-sound 'revert-sound 'riemann-window 
     'right-sample 'ring-modulate 'rv2-window 'rv3-window 'rv4-window
     'samaraki-window 'sample 'sample->file 'sample->file?
     'sampler-at-end? 'sampler-home 'sampler-position 'sampler? 'samples 
     'samples->seconds 'sash-color 'save-controls 'save-dir 'save-edit-history
     'save-envelopes 'save-hook 'save-listener 'save-marks 'save-region
     'save-region-dialog 'save-selection 'save-selection-dialog 'save-sound
     'save-sound-as 'save-sound-dialog 'save-state 'save-state-file
     'save-state-hook 'sawtooth-wave 'sawtooth-wave?  'scale-by 'scale-channel
     'scale-selection-by 'scale-selection-to 'scale-to
     'scan-channel 'script-arg 'script-args 'search-procedure
     'seconds->samples 'select-all 'select-channel 'select-channel-hook
     'select-sound 'select-sound-hook 'selected-channel 'selected-data-color
     'selected-graph-color 'selected-sound 'selection-chans 'selection-color 
     'selection-context 'selection-creates-region 'selection-framples
     'selection-maxamp 'selection-maxamp-position 'selection-member? 
     'selection-position 'selection-srate 'selection? 'short-file-name
     'show-all-axes 'show-all-axes-unlabelled 'show-bare-x-axis 'show-axes
     'show-controls 'show-grid 'show-indices 'show-full-duration
     'show-full-range 'initial-beg 'initial-dur 'show-listener 'show-marks
     'show-mix-waveforms 'show-no-axes 'show-selection
     'show-selection-transform 'show-sonogram-cursor 'show-transform-peaks
     'show-widget 'show-x-axis 'show-x-axis-unlabelled 'show-y-zero
     'sinc-width 'nrxysin 'nrxysin? 'nrxycos 'nrxycos? 'rxyk!cos 'rxyk!cos?
     'rxyk!sin 'rxyk!sin? 'smooth-channel
     'smooth-selection 'smooth-sound 'snd->sample 'snd->sample? 'snd-error
     'snd-error-hook 'snd-exit ( added ) 'snd-gcs 'snd-help 'snd-font
     'snd-color 'snd-print 'snd-spectrum 'snd-tempnam 'snd-url 'snd-urls
     'snd-version 'snd-warning 'snd-warning-hook
     'sound-file-extensions 'sound-file? 
     'sound-files-in-directory 'sound-loop-info 'sound-properties 
     'sound-property 'sound-widgets 'sound? 'soundfont-info 'sounds
     'spectrum-end 'spectro-hop 'spectrum-start 'spectro-x-angle
     'spectro-x-scale 'spectro-y-angle 'spectro-y-scale 'spectro-z-angle
     'spectro-z-scale 'spectrum 'speed-control 'speed-control-as-float
     'speed-control-as-ratio 'speed-control-as-semitone 'speed-control-bounds
     'speed-control-style 'speed-control-tones 'square-wave 'square-wave?
     'squelch-update 'srate 'src 'src-channel 'src-selection 'src-sound
     'src? 'ssb-am 'ssb-am? 'start-playing 'start-playing-hook
     'start-playing-selection-hook 'start-progress-report 'status-report
     'stop-player 'stop-playing 'stop-playing-hook 
     'stop-playing-selection-hook 'ncos 'ncos? 'nsin 'nsin?  'swap-channels
     'sync 'sync-style 'sync-none 'sync-all 'sync-by-sound 'sync-max
     'syncd-marks 'table-lookup 'table-lookup? 'tap 'tap? 'temp-dir
     'text-focus-color 'time-graph 'time-graph-style 'time-graph-type 
     'time-graph? 'tiny-font 'tracking-cursor-style 'transform->vct
     'transform-dialog 'transform-framples 'transform-graph
     'transform-graph-style 'transform-graph-type 'transform-graph?
     'transform-normalization 'transform-sample 'transform-size 'transform-type
     'transform? 'triangle-wave 'triangle-wave? 'tukey-window 
     'two-pole 'two-pole? 'two-zero 'two-zero? 'ultraspherical-window
     'unbind-key  'undo 'undo-edit 'undo-hook 'unselect-all 'update-hook
     'update-lisp-graph 'update-sound 'update-time-graph
     'update-transform-graph 'variable-graph? 'vct 'vct* 'vct+
     'vct->channel 'vct->list 'vct->string 'vct->vector
     'vct-add! 'vct-length 'vct-max 'vct-min 'vct-move!
     'vct-multiply! 'vct-offset! 'vct-peak 'vct-ref 'vct-reverse! 'vct-scale!
     'vct-set! 'vct-subseq 'vct-subtract! 'vct? 'vector->vct 'walsh-transform
     'wave-train 'wave-train? 'wavelet-transform 'wavelet-type 'wavo-hop
     'wavo-trace 'welch-window 'widget-position 'widget-size 'widget-text
     'window-height 'window-width 'window-x 'window-y
     'with-background-processes 'with-file-monitor 'with-gl 'with-mix-tags
     'with-relative-panes 'with-tracking-cursor 'with-verbose-cursor
     'with-inset-graph 'with-interrupts 'with-pointer-focus 'with-smpte-label
     'with-toolbar 'with-tooltips 'with-menu-icons 'save-as-dialog-src
     'save-as-dialog-auto-comment 'x->position 'x-axis-as-clock
     'x-axis-as-percentage 'x-axis-in-beats 'x-axis-in-measures 
     'x-axis-in-samples 'x-axis-in-seconds 'x-axis-label 'x-axis-style
     'x-bounds 'x-position-slider 'x-zoom-slider 'xramp-channel
     'y->position 'y-axis-label 'y-bounds 'y-position-slider 'y-zoom-slider
     'zero-pad 'zoom-color 'zoom-focus-active 'zoom-focus-left
     'zoom-focus-middle 'zoom-focus-right 'zoom-focus-style ) each to sym
    sym symbol-defined? unless
      undefined sym array-push to undefined
    then
  end-each
  *with-test-ladspa* unless
    undefined <'> rm-ladspa #( /ladspa/ ) array-reject! to undefined
  then
  *with-test-gl* unless
    undefined 'glSpectrogram array-delete-key drop
  then
  *with-test-gl2ps* unless
    undefined 'gl-graph->ps array-delete-key drop
  then
  undefined empty? unless
    "undefined[%d]: %s" #( undefined length undefined ) snd-display
  then
;

\ ---------------- test 04: sndlib ----------------

: sndlib-check-it { snd typ fmt samp -- }
  snd header-type typ "save-as %s" #( typ mus-header-type-name ) snd-test-neq
  "test.snd" mus-sound-header-type { ntyp }
  ntyp
  typ
  "save-as %s -> %s"
  #( typ  mus-header-type-name
     ntyp mus-header-type-name ) snd-test-neq
  snd sample-type fmt "save-as %s" #( fmt mus-sample-type-name ) snd-test-neq
  "test.snd" mus-sound-sample-type { nfmt }
  nfmt
  fmt
  "save-as %s -> %s"
  #( fmt  mus-sample-type-name
     nfmt mus-sample-type-name ) snd-test-neq
  1000 snd sample samp "%s[1000]" #( typ mus-header-type-name ) snd-test-neq
;

: sndlib-check-string <{ string -- str }> string " [written by me]" $+ ;

: sndlib-true-cb <{ n -- f }> #t ;

: sndlib-raw-hook-cb <{ file choice -- lst }> '( 1 22050 mus-bshort ) ;

: sndlib-check-bad-file <{ n -- }>
  n open-sound { ind }
  ind number?
  ind sound? && if
    ind close-sound drop
  then
;

: sndlib-test-map-10-times-cb <{ y -- y' }> y 10.0 f* ;

: sndlib-test-map-add-cb { mx -- prc; y self -- y' }
  1 proc-create 1.001 mx f- , ( prc )
 does> { y self -- y' }
  y self @ ( mx ) f+
;

0 value big-file-framples

: sndlib-test-map-x-cb { x incr -- prc; n self -- y }
  1 proc-create x , incr , ( prc )
 does> { n self -- y }
  self @ ( x ) { val }
  val self cell+ @ ( incr ) f+ self ( x ) !
  val
;

: sndlib-test-scan-x-cb { x incr -- prc; n self -- f }
  1 proc-create x , incr , ( prc )
 does> { n self -- f }
  self @ ( x ) { val }
  val self cell+ @ ( incr ) f+ self ( x ) !
  val n fneq
;

: sndlib-test-map-set-1.0 <{ n -- n' }> 1.0 ;

: (04-sndlib-01) ( -- )
  "oboe.snd" { oboe-snd }
  oboe-snd mus-sound-chans { chns }
  oboe-snd mus-sound-data-location { dl }
  oboe-snd mus-sound-framples { fr }
  oboe-snd mus-sound-samples { smps }
  oboe-snd mus-sound-length { len }
  oboe-snd mus-sound-datum-size { size }
  oboe-snd mus-sound-comment { com }
  oboe-snd mus-sound-srate { sr }
  oboe-snd mus-sound-maxamp-exists? { m1 }
  oboe-snd mus-sound-maxamp { mal }
  "z.snd" mus-sound-maxamp { mz }
  oboe-snd mus-sound-sample-type mus-bytes-per-sample { bytes }
  mz car  0   "mus-sound-maxamp z.snd" #() snd-test-neq
  mz cadr 0.0 "mus-sound-maxamp z.snd" #() snd-test-neq
  \
  #( #( mus-bshort 2 )
     #( mus-lshort 2 )
     #( mus-mulaw 1 )
     #( mus-alaw 1 )
     #( mus-byte 1 )
     #( mus-ubyte 1 )
     #( mus-bfloat 4 )
     #( mus-lfloat 4 )
     #( mus-bint 4 )
     #( mus-lint 4 )
     #( mus-bintn 4 )
     #( mus-lintn 4 )
     #( mus-b24int 3 )
     #( mus-l24int 3 )
     #( mus-bdouble 8 )
     #( mus-ldouble 8 )
     #( mus-ubshort 2 )
     #( mus-ulshort 2 )
     #( mus-bdouble-unscaled 8 )
     #( mus-ldouble-unscaled 8 )
     #( mus-bfloat-unscaled 4 )
     #( mus-lfloat-unscaled 4 ) ) { formats }
  nil nil nil { vals frm siz }
  formats each to vals
    vals 0 array-ref to frm
    vals 1 array-ref to siz
    frm mus-bytes-per-sample siz "mus-bytes-per-sample" #() snd-test-neq
  end-each
  mus-bshort mus-sample-type->string "mus-bshort"
    "mus-sample-type->string" #() snd-test-neq
  mus-aifc mus-header-type->string "mus-aifc"
    "mus-header-type->string" #() snd-test-neq
  \
  "hiho.tmp" { hiho }
  hiho mus-sound-report-cache drop
  hiho readlines { res }
  res 0 array-ref string-chomp "sound table:" "print-cache 1" #() snd-test-neq
  hiho file-delete
  10 { req }
  chns  1             "oboe: mus-sound-chans"         #() snd-test-neq
  dl    28            "oboe: mus-sound-data-location" #() snd-test-neq
  fr    50828         "oboe: mus-sound-framples"      #() snd-test-neq
  smps  50828         "oboe: mus-sound-samples"       #() snd-test-neq
  len   50828 2* 28 + "oboe: mus-sound-length"        #() snd-test-neq
  size  2             "oboe: mus-sound-datum-size"    #() snd-test-neq
  bytes 2             "oboe: mus-sound-bytes"         #() snd-test-neq
  sr    22050         "oboe: mus-sound-srate"         #() snd-test-neq
  m1
  *clmtest* 0= && if
    "oboe: mus-sound-maxamp-exists before maxamp: %s" #( m1 ) snd-display
  then
  oboe-snd mus-sound-maxamp-exists? to res
  res unless
    "oboe: not mus-sound-maxamp-exists after maxamp: %s" #( res ) snd-display
  then
  *clmtest* 0= if
    mus-header-raw-defaults to res
    res length 3 = if
      res 0 array-ref ( sr ) 44100 "mus-header-raw-defaults srate"
        #() snd-test-neq
      res 1 array-ref ( chns ) 2 "mus-header-raw-defaults chans"
        #() snd-test-neq
      res 2 array-ref ( frm ) mus-bshort "mus-header-raw-defaults format"
        #() snd-test-neq
    else
      res object-length 3 "mus-header-raw-defaults %s" #( res ) snd-test-neq
    then
  then
  '( 12345 3 mus-bdouble-unscaled ) set-mus-header-raw-defaults drop
  mus-header-raw-defaults to res
  res length 3 = if
    res 0 array-ref ( sr ) 12345 "set-mus-header-raw-defaults srate"
      #() snd-test-neq
    res 1 array-ref ( chns ) 3 "set-mus-header-raw-defaults chans"
      #() snd-test-neq
    res 2 array-ref ( frm ) mus-bdouble-unscaled
      "set-mus-header-raw-defaults format" #() snd-test-neq
  else
    res object-length 3 "set-mus-header-raw-defaults %s" #( res ) snd-test-neq
  then
  '( 44100 2 mus-bshort ) set-mus-header-raw-defaults drop
  \
  "%d-%b %H:%M" oboe-snd mus-sound-write-date strftime
  "15-Oct 04:34"
  "mus-sound-write-date oboe.snd" #() snd-test-neq
  "%d-%b %H:%M" "pistol.snd" mus-sound-write-date strftime
  "01-Jul 22:06"
  "mus-sound-write-date pistol.snd" #() snd-test-neq
  \
  oboe-snd open-sound { ind }
  "test" { long-file-name }
  10 0 do
    long-file-name "-test" string-push drop
  loop
  long-file-name ".snd" string-push drop
  ind variable-graph? if
    "variable-graph thinks anything is a graph..." #() snd-display
  then
  ind player? if
    "player? thinks anything is a player..." #() snd-display
  then
  ind sound? unless
    "%s is not a sound?" #( ind ) snd-display
  then
  #f sound? if
    "sound? #f -> #t?" #() snd-display
  then
  #t sound? if
    "sound? #t -> #f?" #() snd-display
  then
  long-file-name ind save-sound-as drop
  ind close-sound drop
  long-file-name open-sound to ind
  ind sound? unless
    "can't find test...snd" #() snd-display
  then
  long-file-name string-length { lfnlen }
  ind file-name string-length { fnlen }
  ind short-file-name string-length { sfnlen }
  fnlen lfnlen <
  sfnlen lfnlen < || if
    "file-name lengths: long-file-name %d, file-name %d, short-file-name %d"
      #( lfnlen fnlen sfnlen ) snd-display
  then
  ind close-sound drop
  long-file-name mus-sound-forget drop
  long-file-name file-delete
  \
  "forest.aiff" check-file-name { fsnd }
  fsnd file-exists? if
    fsnd "fmv.snd" file-copy
    "fmv.snd" open-sound to ind
    ind sound-loop-info  fsnd mus-sound-loop-info  "loop-info" #() snd-test-neq
    ind '( 12000 14000 1 2 3 4 ) set-sound-loop-info drop
    ind sound-loop-info  '( 12000 14000 1 2 3 4 1 1 )  "set-loop-info"
      #() snd-test-neq
    "fmv1.snd" ind :header-type mus-aifc save-sound-as drop
    ind close-sound drop
    "fmv1.snd" mus-sound-loop-info  '( 12000 14000 1 2 3 4 1 1 )
      "saved-loop-info" #() snd-test-neq
  then
  \
  oboe-snd open-sound to ind
  "fmv.snd" ind :header-type mus-aifc save-sound-as drop
  ind close-sound drop
  "fmv.snd" open-sound to ind
  ind sound-loop-info '() "null loop-info" #() snd-test-neq
  ind '( 1200 1400 4 3 2 1 ) set-sound-loop-info drop
  ind sound-loop-info '( 1200 1400 4 3 2 1 1 1 ) "set null loop-info" #()
    snd-test-neq
  "fmv1.snd" ind :header-type mus-aifc save-sound-as drop
  "fmv1.snd" mus-sound-loop-info '( 1200 1400 4 3 2 1 1 1 )
    "saved null loop-info" #() snd-test-neq
  ind close-sound drop
  "fmv.snd" open-sound to ind
  '( 1200 1400 4 3 2 1 1 0 ) set-sound-loop-info drop
  ind sound-loop-info '( 1200 1400 0 0 2 1 1 0 ) 
    "set null loop-info (no mode1)" #() snd-test-neq
  "fmv1.snd" ind :header-type mus-aifc save-sound-as drop
  ind close-sound drop
  "fmv1.snd" mus-sound-loop-info '( 1200 1400 0 0 2 1 1 0 )
    "saved null loop-info (no mode1)" #() snd-test-neq
  \
  com empty? unless
    "oboe: mus-sound-comment %S?" #( com ) snd-display
  then
  #( #( "nasahal8.wav"
        "ICRD: 1997-02-22\nIENG: Paul R. Roger\nISFT: Sound Forge 4.0\n" )
     #( "8svx-8.snd" "File created by Sound Exchange  " )
     #( "sun-16-afsp.snd" "AFspdate:1981/02/11 23:03:34 UTC" )
     #( "smp-16.snd" "Converted using Sox.                                        " )
     #( "d40130.au" "1994 Jesus Villena" )
     #( "wood.maud" "file written by SOX MAUD-export " )
     #( "addf8.sf_mipseb"
         "date=\"Feb 11 18:03:34 1981\" info=\"Original recorded at 20 kHz, 15-bit D/A, digitally filtered and resampled\" speaker=\"AMK female\" text=\"Add the sum to the product of these three.\" " )
     #( "mary-sun4.sig" "MARY HAD A LITTLE LAMB\n" )
     #( "nasahal.pat" "This patch saved with Sound Forge 3.0." )
     #( "next-16.snd"
        ";Written on Mon 1-Jul-91 at 12:10 PDT  at localhost (NeXT) using Allegro CL and clm of 25-June-91" )
     #( "wood16.nsp" "Created by Snack   " )
     #( "wood.sdx" "1994 Jesus Villena" )
     #( "clmcom.aif" "this is a comment" )
     #( "anno.aif" "1994 Jesus Villena\n" )
     #( "telephone.wav"
        "sample_byte_format -s2 01\nchannel_count -i 1\nsample_count -i 36461\nsample_rate -i 16000\nsample_n_bytes -i 2\nsample_sig_bits -i 16\n" ) ) { comms }
  comms each to vals
    vals 0 array-ref check-file-name to fsnd
    fsnd file-exists? if
      fsnd mus-sound-comment ( res )
      vals 1 array-ref ( req )
      "mus-sound-comment %s" #( fsnd ) snd-test-neq
    then
  end-each
  \
  "traffic.aiff" check-file-name to fsnd
  fsnd file-exists? if
    fsnd mus-sound-comment to res
    res string? unless
      "mus-sound-comment traffic: %s?" #( res ) snd-display
    then
  then
  \
  *clmtest* 0= if
    mal cadr 0.14724 "oboe: mus-sound-maxamp" #() snd-test-neq
    mal car 24971 "oboe: mus-sound-maxamp at" #() snd-test-neq
  then
  \
  oboe-snd mus-sound-type-specifier to res
  \ 0x646e732e little endian reader
  \ 0x2e736e64 big endian reader
  res 0x646e732e d<>
  res 0x2e736e64 d<> && if
    "oboe: mus-sound-type-specifier: %#x?" #( res ) snd-display
  then
  "%d-%b-%Y %H:%M" oboe-snd file-write-date strftime
  "15-Oct-2006 04:34"
  "oboe: file-write-date" #() snd-test-neq
  \
  0 { lasth }
  begin
    lasth 1+ to lasth
    lasth mus-header-type-name "unknown" string=
  until
  lasth 50 < if
    "header-type[%d] = %s?" #( lasth dup mus-header-type-name ) snd-display
  then
  0 to lasth
  begin
    lasth 1+ to lasth
    lasth mus-sample-type-name "unknown" string=
  until
  lasth 10 < if
    "sample-type[%d] = %s?" #( lasth dup mus-sample-type-name ) snd-display
  then
  nil { name }
  #( 'dont-normalize
     'normalize-globally
     'normalize-by-channel ) each symbol-name to name
    name string-eval to req
    req set-transform-normalization drop
    transform-normalization req
      "set-transform-normalization %s" #( name ) snd-test-neq
  end-each
  \
  "fmv.snd" 1 22050 mus-bshort mus-next "set-samples test" 100 new-sound to ind
  10 3 3 0.1 make-vct set-samples drop
  0 20 ind 0 channel->vct
  vct( 0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 )
  "1 set samples 0 for 0.1" #() snd-test-neq
  20 3 3 0.1 make-vct ind 0 set-samples drop
  10 20 ind 0 channel->vct
  vct( 0.1 0.1 0.1 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 )
  "2 set samples 10 for 0.1" #() snd-test-neq
  30 3 3 0.1 make-vct ind 0 #f "a name" set-samples drop
  20 20 ind 0 channel->vct
  vct( 0.1 0.1 0.1 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 )
  "3 set samples 20 for 0.1" #() snd-test-neq
  0 3 3 0.2 make-vct ind 0 #f "a name" 0 1 set-samples drop
  0 20 ind 0 channel->vct
  vct( 0.2 0.2 0.2 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 )
  "4 set samples 0 at 1 for 0.1" #() snd-test-neq
  20 20 ind 0 channel->vct
  20 0.0 make-vct
  "5 set samples 20 at 1 for 0.1" #() snd-test-neq
  "fmv1.snd" :channels 2 new-sound { nd }
  10 0.5 make-vct 0 10 nd 0 vct->channel drop
  10 0.3 make-vct 0 10 nd 1 vct->channel drop
  "fmv1.snd" nd save-sound-as drop
  nd close-sound drop
  "fmv1.snd" file-exists? unless
    "fmv1.snd not saved??" #() snd-display
  then
  0 10 "fmv1.snd" ind 0 #f "another name" 1 set-samples drop
  0 20 ind 0 channel->vct
  vct( 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.1 0.1 0.1 0 0 0 0 0 0 0 )
  "6 set samples 0 at 1 for 0.1" #() snd-test-neq
  5 6 "fmv1.snd" ind 0 #f "another name 7" 0 set-samples drop
  0 20 ind 0 channel->vct
  vct( 0.3 0.3 0.3 0.3 0.3 0.5 0.5 0.5 0.5 0.5 0.5 0.1 0.1 0 0 0 0 0 0 0 )
  "7 set samples 0 at 1 for 0.1" #() snd-test-neq
  0 10 "fmv1.snd" ind 0 #f "another name 8" 1 0 #f set-samples drop
  0 20 ind 0 channel->vct
  vct( 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0 0 0 0 0 0 0 0 0 0 )
  "8 set samples 0 at 1 for 0.1" #() snd-test-neq
  10 10 "fmv1.snd" ind 0 #f "another name 9" 0 0 set-samples drop
  0 20 ind 0 channel->vct
  vct( 0 0 0 0 0 0 0 0 0 0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 )
  "9 set samples 0 at 1 for 0.1" #() snd-test-neq
  20 10 "fmv1.snd" set-samples drop
  10 20 ind 0 channel->vct
  20 0.5 make-vct
  "10 set samples 0 at 1 for 0.1" #() snd-test-neq
  0 10 "fmv1.snd" ind 0 #t "another name" 1 0 #f set-samples drop
  ind 0 framples 10 "11 set samples truncate" #() snd-test-neq
  ind revert-sound drop
  "fmv1.snd" file-delete
  \ ;; now try to confuse it
  0 10 "fmv1.snd" ind 0 <'> set-samples snd-test-catch to res
  res car 'no-such-file "set-samples, no such file" #() snd-test-neq
  "fmv1.snd" :channels 1 new-sound to nd
  "fmv1.snd" nd save-sound-as drop
  nd close-sound drop
  0 10 "fmv1.snd" ind 0 #f "another name" 1 <'> set-samples
    snd-test-catch to res
  res car 'no-such-channel "set-samples no such channel" #() snd-test-neq
  0 10 "fmv1.snd" ind 0 #f "another name" -1 <'> set-samples
    snd-test-catch to res
  res car 'no-such-channel "set-samples no such channel (-1)" #() snd-test-neq
  0 -10 "fmv1.snd" <'> set-samples snd-test-catch to res
  res car 'wrong-type-arg "set-samples (-10)" #() snd-test-neq
  -10 10 "fmv1.snd" <'> set-samples snd-test-catch to res
  res car 'no-such-sample "set-samples (beg -10)" #() snd-test-neq
  ind close-sound drop
  \
  100 { len }
  #( #( mus-bshort  2 -15 f** )
     #( mus-lshort  2 -15 f** )
     #( mus-mulaw   0.02 )
     #( mus-alaw    0.02 )
     #( mus-byte    2 -7 f** )
     #( mus-lfloat  2 -23 f** )
     #( mus-bint    2 -23 f** )
     #( mus-lint    2 -23 f** )
     #( mus-b24int  2 -23 f** )
     #( mus-l24int  2 -23 f** )
     #( mus-ubshort 2 -15 f** )
     #( mus-ulshort 2 -15 f** )
     #( mus-ubyte   2 -7 f** )
     #( mus-bfloat  2 -23 f** )
     #( mus-bdouble 2 -23 f** )
     #( mus-ldouble 2 -23 f** ) ) { types }
  nil nil nil nil nil nil { v v0 v1 diff maxdiff maxpos }
  nil nil nil { typ allowed-diff val }
  types each to vals
    vals 0 array-ref to typ
    vals 1 array-ref to allowed-diff
    "test.snd" 1 22050 mus-bfloat mus-next new-sound to ind
    len 0.0 make-vct to v
    0.0 to maxdiff
    #f to maxpos
    v 0  0.999 vct-set! drop
    v 1 -1.000 vct-set! drop
    v 2  0.100 vct-set! drop
    v 3 -0.100 vct-set! drop
    v 4  0.010 vct-set! drop
    v 5 -0.010 vct-set! drop
    v 6  0.001 vct-set! drop
    v 7 -0.001 vct-set! drop
    v 8  0.000 vct-set! drop
    len 7 do
      1.9999 random to val
      val 2.0 f>
      val 0.0 f< || if
        "random 2.0 -> %s?" #( val ) snd-display
      then
      v i 1.0 val f- vct-set! drop
    loop
    v 0 len ind 0 vct->channel drop
    "test1.snd" ind :header-type mus-next :sample-type typ save-sound-as drop
    ind close-sound drop
    "test1.snd" open-sound to ind
    0 len ind 0 channel->vct to v1
    len 0 do
      v i vct-ref v1 i vct-ref f- fabs to diff
      diff maxdiff f> if
        diff to maxdiff
        i to maxpos
      then
    loop
    maxdiff allowed-diff f> if
      "%s: %s at %d (%s %s)?"
        #( typ mus-sample-type-name
           maxdiff
           maxpos
           v maxpos vct-ref
           v1 maxpos vct-ref ) snd-display
    then
    ind close-sound drop
  end-each
  \
  oboe-snd view-sound { ob }
  1000 ob sample { samp }
  oboe-snd mus-sound-comment { old-comment }
  "written %s" #( "%a %d-%b-%Y %H:%M %Z" current-time strftime )
    string-format { str }
  ob str set-comment drop
  "test.snd" ob :header-type mus-aifc :sample-type mus-bdouble
    <'> save-sound-as snd-test-catch to res
  res car 'cannot-save "save-sound-as test.snd write trouble" #() snd-test-eq
  #t set-filter-control-in-hz drop
  "test.snd" open-sound { ab }
  ab mus-aifc mus-bdouble samp sndlib-check-it
  "test.snd" mus-sound-comment str "output-comment" #() snd-test-neq
  ab comment str "output-comment (comment)" #() snd-test-neq
  ab close-sound drop
  oboe-snd mus-sound-comment old-comment "set-comment overwrote current" #()
    snd-test-neq
  #f set-filter-control-in-hz drop
  \
  "test.snd" ob :header-type mus-raw save-sound-as drop
  "test.snd" 1 22050 mus-bshort open-raw-sound to ab
  ab mus-raw mus-bshort samp sndlib-check-it
  ab close-sound drop
  \
  "test.snd" ob :header-type mus-nist :sample-type mus-bint save-sound-as drop
  "test.snd" open-sound to ab
  ab mus-nist mus-bint samp sndlib-check-it
  ab close-sound drop
  \
  output-comment-hook reset-hook!
  output-comment-hook <'> sndlib-check-string add-hook!
  "test.snd" ob :header-type mus-riff :sample-type mus-lfloat save-sound-as drop
  output-comment-hook reset-hook!
  "test.snd" open-sound to ab
  ab mus-riff mus-lfloat samp sndlib-check-it
  ab comment str " [written by me]" $+ "output-comment-hook" #() snd-test-neq
  ab close-sound drop
  #( #( mus-aiff  mus-b24int )
     #( mus-ircam mus-mulaw )
     #( mus-next  mus-alaw )
     #( mus-next  mus-ldouble ) ) to types
  nil { fmt }
  types each to vals
    vals 0 array-ref to typ
    vals 1 array-ref to fmt
    "test.snd" ob :header-type typ :sample-type fmt save-sound-as drop
    "test.snd" open-sound to ab
    ab typ fmt samp sndlib-check-it
    ab close-sound drop
  end-each
  "test.snd" ob :header-type mus-next :sample-type mus-bshort save-sound-as drop
  "test.snd" open-sound to ab
  ab mus-next mus-bshort samp sndlib-check-it
  update-hook reset-hook!
  '( -3.0 3.0 ) ab 0 set-y-bounds drop
  ab mus-lshort set-sample-type drop
  \ ; these set!'s can change the index via update-sound
  "test.snd" find-sound to ab
  ab sample-type to fmt
  fmt mus-lshort "set-sample-type %s" #( fmt mus-sample-type-name ) snd-test-neq
  ab 0 y-bounds '( -3.0 3.0 ) "set data format y-bounds" #() snd-test-neq
  '( 2.0 ) ab 0 set-y-bounds drop
  ab 0 y-bounds '( -2.0 2.0 ) "set data format y-bounds 1" #() snd-test-neq
  '( -2.0 ) ab 0 set-y-bounds drop
  ab 0 y-bounds '( -2.0 2.0 ) "set data format y-bounds -2" #() snd-test-neq
  ab mus-aifc set-header-type drop
  "test.snd" find-sound to ab
  ab header-type mus-aifc "set-header-type" #() snd-test-neq
  ab 3 set-channels drop
  "test.snd" find-sound to ab
  ab channels 3 "set-channels" #() snd-test-neq
  ab 1234 set-data-location drop
  "test.snd" find-sound to ab
  ab data-location 1234 "set-data-location" #() snd-test-neq
  ab data-size { old-size }
  ab 1234 set-data-size drop
  "test.snd" find-sound to ab
  ab data-size 1234 "set-data-size" #() snd-test-neq
  ab old-size set-data-size drop
  ab 12345 set-srate drop
  "test.snd" find-sound to ab
  ab srate 12345 "set-srate" #() snd-test-neq
  ab close-sound drop
  \
  "test.snd" ob :header-type mus-next :sample-type mus-bfloat save-sound-as drop
  "test.snd" open-sound to ab
  ab mus-next mus-bfloat samp sndlib-check-it
  ab close-sound drop
  \
  "test.snd" ob :header-type mus-next :sample-type mus-bshort save-sound-as drop
  ob close-sound drop
  "test.snd" open-sound to ab
  mus-lshort set-sample-type drop
  "test.snd" find-sound to ab
  sample-type mus-lshort "set-sample-type" #() snd-test-neq
  mus-aifc set-header-type drop
  "test.snd" find-sound to ab
  header-type mus-aifc "set-header-type" #() snd-test-neq
  3 set-channels drop
  "test.snd" find-sound to ab
  channels 3 "set-channels" #() snd-test-neq
  1234 set-data-location drop
  "test.snd" find-sound to ab
  data-location 1234 "set-data-location" #() snd-test-neq
  12345 set-srate drop
  "test.snd" find-sound to ab
  srate 12345 "set-srate" #() snd-test-neq
  ab close-sound drop
  \
  "2a.snd" open-sound to ind
  "test.snd" :sample-type mus-l24int :header-type mus-riff
    :channel 0 save-sound-as drop
  "test.snd" open-sound { ind0 }
  ind0 channels 1 "save-sound-as :channel 0 chans" #() snd-test-neq
  ind0 sample-type mus-l24int "save-sound-as :channel 0 sample-type" #()
    snd-test-neq
  ind0 header-type mus-riff "save-sound-as :channel 0 header-type" #()
    snd-test-neq
  ind0 srate ind srate "save-sound-as :channel 0 srates" #() snd-test-neq
  ind0 framples ind 0 undef framples "save-sound-as :channel 0 framples" #()
    snd-test-neq
  ind0 maxamp ind 0 undef maxamp "save-sound-as :channel 0 maxamps" #()
    snd-test-neq
  ind0 close-sound drop
  \
  "test.snd" :sample-type mus-bfloat :header-type mus-aifc :channel 1
    :srate 12345 save-sound-as drop
  "test.snd" open-sound to ind0
  ind0 channels 1 "save-sound-as :channel 1 chans" #() snd-test-neq
  ind0 sample-type mus-bfloat "save-sound-as :channel 1 sample-type" #()
    snd-test-neq
  ind0 header-type mus-aifc "save-sound-as :channel 1 header-type" #()
    snd-test-neq
  ind0 srate 12345 "save-sound-as :channel 1 srates" #() snd-test-neq
  ind0 framples ind 1 undef framples "save-sound-as :channel 1 framples" #()
    snd-test-neq
  ind0 maxamp ind 1 undef maxamp "save-sound-as :channel 1 maxamps" #()
    snd-test-neq
  ind0 close-sound drop
  \
  "test.snd" :channel 1 :comment "this is a test" save-sound-as drop
  "test.snd" open-sound to ind0
  ind0 channels 1 "save-sound-as :channel 1 (1) chans" #() snd-test-neq
  ind0 sample-type ind sample-type
    "save-sound-as :channel 1 (1) sample-type" #() snd-test-neq
  ind0 header-type ind header-type 
    "save-sound-as :channel 1 (1) header-type" #() snd-test-neq
  ind0 srate ind srate "save-sound-as :channel 1 (1) srates" #() snd-test-neq
  ind0 framples ind 1 undef framples
    "save-sound-as :channel 1 (1) framples" #() snd-test-neq
  ind0 0 maxamp ind 1 undef maxamp 
    "save-sound-as :channel 1 (1) maxamps" #() snd-test-neq
  ind0 comment "this is a test"
    "save-sound-as :channel 1 (1) comment" #() snd-test-neq
  ind0 close-sound drop
  ind close-sound drop
  \
  "t15.aiff" check-file-name to fsnd
  fsnd file-exists? if
    fsnd open-sound to ind
    132300 ind 0 sample 0.148 "aifc sowt trouble (0)" #() snd-test-neq
    132300 ind 1 sample 0.126 "aifc sowt trouble (1)" #() snd-test-neq
    ind close-sound drop
  then
  "M1F1-float64C-AFsp.aif" check-file-name to fsnd
  fsnd file-exists? if
    fsnd open-sound to ind
    8000 ind 0 sample -0.024 "aifc fl64 trouble (0)" #() snd-test-neq
    8000 ind 1 sample 0.021 "aifc fl64 trouble (1)" #() snd-test-neq
    ind close-sound drop
  then
  \
  #( #( "bad_chans.snd"       0 22050 0 )
     #( "bad_srate.snd"       1 0 0 )
     #( "bad_data_format.snd" 1 22050 4411 )
     #( "bad_chans.aifc"      0 22050 0 )
     #( "bad_srate.aifc"      1 0 0 )
     #( "bad_length.aifc"     1 22050 -10 )
     #( "bad_chans.riff"      0 22050 0 )
     #( "bad_srate.riff"      1 0 0 )
     #( "bad_chans.nist"      0 22050 0 )
     #( "bad_srate.nist"      1 0 0 )
     #( "bad_length.nist"     1 22050 -10 ) ) { files }
  nil nil nil { fc fs fr }
  files each to vals
    vals 0 array-ref check-file-name to fsnd
    vals 1 array-ref to fc
    vals 2 array-ref to fs
    vals 3 array-ref to fr
    fsnd file-exists? if
      fsnd <'> mus-sound-chans #t nil fth-catch ?dup-if
        car 'mus-error <> if
          "%s: chans %d (%s)" #( fsnd fc res ) snd-display
        then
      else
        fc <> if
          "%s: chans %d (%s)" #( fsnd fc res ) snd-display
        then
        stack-reset
      then
      \
      fsnd <'> mus-sound-srate #t nil fth-catch ?dup-if
        car 'mus-error <> if
          "%s: srate %d (%s)" #( fsnd fs res ) snd-display
        then
        stack-reset
      else
        fs <> if
          "%s: srate %d (%s)" #( fsnd fs res ) snd-display
        then
      then
      \
      fsnd <'> mus-sound-framples #t nil fth-catch ?dup-if
        car 'mus-error <> if
          "%s: framples %d (%s)" #( fsnd fr res ) snd-display
        then
        stack-reset
      else
        fr <> if
          "%s: framples %d (%s)" #( fsnd fr res ) snd-display
        then
      then
    then                        \ file-exists?
  end-each
  \
  "/usr/include/sys/" file-pwd $+ "/oboe.snd" $+
    <'> open-sound #t nil fth-catch ?dup-if
    1 >list "open-sound with slashes: %s" swap snd-display
    stack-reset
  else                          \ open-sound with slashes
    to ind
    ind sound? if
      ind short-file-name "oboe.snd" string<> if
        "open-sound with slashes: %s" #( ind short-file-name ) snd-display
      then
    else
      "open-sound with slashes: %s" #( ind ) snd-display
    then
    bad-header-hook <'> sndlib-true-cb add-hook!
    #( "bad_chans.snd"
       "bad_srate.snd"
       "bad_chans.aifc"
       "bad_srate.aifc"
       "bad_length.aifc"
       "bad_chans.riff"
       "bad_srate.riff"
       "bad_chans.nist"
       "bad_location.nist"
       "bad_field.nist"
       "bad_srate.nist"
       "bad_length.nist" ) each check-file-name to fsnd
      fsnd file-exists? if
        fsnd <'> insert-sound snd-test-catch drop
        fsnd <'> convolve-with snd-test-catch drop
        fsnd <'> mix snd-test-catch drop
        fsnd <'> sndlib-check-bad-file snd-test-catch drop
      then
    end-each
    ind close-sound drop
  then                          \ open-sound with slashes
  sounds each ( snd )
    close-sound
  end-each
  \
  #( "trunc.snd"
     "trunc.aiff"
     "trunc.wav"
     "trunc.sf"
     "trunc.voc"
     "trunc.nist"
     "bad.wav"
     "trunc1.aiff"
     "badform.aiff" ) each check-file-name to fsnd
    fsnd file-exists? if
      fsnd <'> open-sound snd-test-catch to res
      res car 'mus-error "open-sound" #() snd-test-neq
    then
  end-each
  open-raw-sound-hook <'> sndlib-raw-hook-cb add-hook!
  "empty.snd" check-file-name to fsnd
  fsnd file-exists? if
    fsnd open-sound to ind
    ind sample-type   mus-bshort "open raw sample-type" #() snd-test-neq
    ind chans         1          "open raw chans" #() snd-test-neq
    ind srate         22050      "open raw srate" #() snd-test-neq
    ind data-location 0          "open raw data-location" #() snd-test-neq
    ind framples      0          "open raw framples" #() snd-test-neq
    ind close-sound drop
  then
  open-raw-sound-hook reset-hook!
  \
  #f set-clipping drop
  "test.snd" :sample-type mus-lshort new-sound { snd }
  0 10 pad-channel drop
  1  1.0000 set-sample drop
  2 -1.0000 set-sample drop
  3  0.9999 set-sample drop
  4  2.0000 set-sample drop
  5 -2.0000 set-sample drop
  6  1.3000 set-sample drop
  7 -1.3000 set-sample drop
  8  1.8000 set-sample drop
  9 -1.8000 set-sample drop
  snd save-sound drop
  snd close-sound drop
  "test.snd" open-sound to snd
  0 10 channel->vct
  vct( 0.000 1.000 -1.000 1.000 0.000 0.000 -0.700 0.700 -0.200 0.200 )
  "unclipped 1" #() snd-test-neq
  snd close-sound drop
  "test.snd" mus-sound-forget drop
  \
  #t set-clipping drop
  "test.snd" :sample-type mus-lshort new-sound to snd
  0 10 pad-channel drop
  1  1.0000 set-sample drop
  2 -1.0000 set-sample drop
  3  0.9999 set-sample drop
  4  2.0000 set-sample drop
  5 -2.0000 set-sample drop
  6  1.3000 set-sample drop
  7 -1.3000 set-sample drop
  8  1.8000 set-sample drop
  9 -1.8000 set-sample drop
  snd save-sound drop
  snd close-sound drop
  "test.snd" open-sound to snd
  0 10 channel->vct
  vct( 0.000 1.000 -1.000 1.000 1.000 -1.000 1.000 -1.000 1.000 -1.000 )
  "clipped" #() snd-test-neq
  snd close-sound drop
  \
  #( #( "next-dbl.snd" 10 10
        vct( 0.475 0.491 0.499 0.499 0.492 0.476 0.453 0.423 0.387 0.344 ) )
     #( "oboe.ldbl" 1000 10 
        vct( 0.033 0.035 0.034 0.031 0.026 0.020 0.013 0.009 0.005 0.004 ) )
     #( "next-flt.snd" 10 10
        vct( 0.475 0.491 0.499 0.499 0.492 0.476 0.453 0.423 0.387 0.344 ) )
     #( "clbonef.wav" 1000 10
        vct( 0.111 0.101 0.07 0.032 -0.014 -0.06 -0.085 -0.108 -0.129 -0.152 ) )
     #( "next-8.snd" 10 10
        vct( 0.898 0.945 0.977 0.992 0.992 0.977 0.945 0.906 0.844 0.773 ) )
     #( "o2_u8.wave" 1000 10
        vct( -0.164 -0.219 -0.258 -0.242 -0.18 -0.102 -0.047 0.0 0.039 0.055 ) )
     #( "next-16.snd" 1000 10
        vct( -0.026 -0.022 -0.024 -0.03 -0.041
             -0.048 -0.05 -0.055 -0.048 -0.033 ) )
     #( "o2.wave" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175 
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "o2_18bit.aiff" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "o2_12bit.aiff" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "next24.snd" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "mono24.wav" 1000 10
        vct( 0.005 0.010 0.016 0.008 -0.007
            -0.018 -0.025 -0.021 -0.005 0.001 ) )
     #( "o2_711u.wave" 1000 10
        vct( -0.164 -0.219 -0.254 -0.242 -0.172
             -0.103 -0.042 0.005 0.042 0.060 ) )
     #( "alaw.wav" 1000 10
        vct( -0.024 -0.048 -0.024 0.0 0.008 0.008 0.000 -0.040 -0.064 -0.024 ) )
     \ ;; it is not a bug if these don't match if MUS_SAMPLE_BITS is not 24
     #( "b32.pvf" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175 
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "b32.wave" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "b32.snd" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "32bit.sf" 1000 10
        vct( 0.016 0.014 0.013 0.011 0.010 0.010 0.010 0.010 0.012 0.014 ) )
     #( "nist-shortpack.wav" 10000 10
        vct( 0.021 0.018 0.014 0.009 0.004
            -0.001 -0.004 -0.006 -0.007 -0.008 ) )
     #( "wood.sds" 1000 10
        vct( -0.160 -0.216 -0.254 -0.239 -0.175
             -0.102 -0.042 0.005 0.041 0.059 ) )
     #( "mus10.snd" 10000 10
        vct( 0.004 0.001 0.005 0.009 0.017 0.015 0.008 0.011 0.009 0.012 ) )
     #( "ieee-text-16.snd" 1000 10
        vct( -0.052 -0.056 -0.069 -0.077 -0.065 
             -0.049 -0.054 -0.062 -0.066 -0.074 ) )
     #( "hcom-16.snd" 10000 10
        vct( 0.000 0.000 0.000 0.008 0.000 -0.016 -0.016 -0.016 -0.008 0.000 ) )
     #( "ce-c3.w02" 1000 10
        vct( 0.581 0.598 0.596 0.577 0.552 0.530 0.508 0.479 0.449 0.425 ) )
     #( "nasahal.avi" 20000 10
        vct( 0.390 0.120 -0.399 -0.131 0.464 0.189 -0.458 -0.150 0.593 0.439 ) )
     #( "oki.wav" 100 10
        vct( 0.396 0.564 0.677 0.779 0.761 0.540 0.209 -0.100 -0.301 -0.265 ) )
     #( "trumps22.adp" 5000 10
         vct( 0.267 0.278 0.309 0.360 0.383 0.414 0.464 0.475 0.486 0.495 ) )
      ) to files
  nil nil nil nil { file beg dur data }
  files each to vals
    vals 0 array-ref to file
    vals 1 array-ref to beg
    vals 2 array-ref to dur
    vals 3 array-ref to data
    file check-file-name to fsnd
    fsnd file-exists? if
      fsnd open-sound to ind
      beg dur ind 0 channel->vct data "%s" #( file ) snd-test-neq
      ind close-sound drop
    then
  end-each
  \
  #( "no error"
     "no frequency method"
     "no phase method"
     "null gen arg to method"
     "no length method"
     "no describe method"
     "no data method"
     "no scaler method"
     "memory allocation failed"
     "can't open file"
     "no sample input"
     "no sample output"
     "no such channel"
     "no file name provided"
     "no location method"
     "no channel method"
     "no such fft window"
     "unknown sample type"
     "header read failed"
     "unknown header type"
     "file descriptors not initialized"
     "not a sound file"
     "file closed"
     "write error"
     "header write failed"
     "can't open temp file"
     "interrupted"
     "bad envelope"
     "audio channels not available"
     "audio srate not available"
     "audio sample type not available"
     "no audio input available"
     "audio configuration not available" 
     "audio write error"
     "audio size not available"
     "audio device not available"
     "can't close audio"
     "can't open audio"
     "audio read error"
     "can't write audio"
     "can't read audio"
     "no audio read permission" 
     "can't close file"
     "arg out of range"
     "no channels method"
     "no hop method"
     "no width method"
     "no file-name method"
     "no ramp method"
     "no run method"
     "no increment method"
     "no offset method"
     "no xcoeff method"
     "no ycoeff method"
     "no xcoeffs method"
     "no ycoeffs method"
     "no reset"
     "bad size"
     "can't convert"
     "read error"
     "no feedforward method"
     "no feedback method"
     "no interp-type method"
     "no position method"
     "no order method"
     "no copy method"
     "can't translate" ) each
    ( err ) i mus-error-type->string "mus-error-type->string[%d]" #( i )
      snd-test-neq
  end-each
  \
  "oboe.snd" mus-sound-srate { cur-srate }
  "oboe.snd" mus-sound-chans { cur-chans }
  "oboe.snd" mus-sound-sample-type { cur-format }
  "oboe.snd" mus-sound-header-type { cur-type }
  "oboe.snd" mus-sound-data-location { cur-loc }
  "oboe.snd" mus-sound-samples { cur-samps }
  "oboe.snd" cur-srate 2* set-mus-sound-srate drop
  "oboe.snd" mus-sound-srate cur-srate 2* "set-mus-sound-srate" #()
    snd-test-neq
  "oboe.snd" cur-samps 2* set-mus-sound-samples drop
  "oboe.snd" mus-sound-samples cur-samps 2* "set-mus-sound-samples" #()
    snd-test-neq
  "oboe.snd" cur-chans 2* set-mus-sound-chans drop
  "oboe.snd" mus-sound-chans cur-chans 2* "set-mus-sound-chans" #()
    snd-test-neq
  "oboe.snd" cur-loc 2* set-mus-sound-data-location drop
  "oboe.snd" mus-sound-data-location cur-loc 2*
    "set-mus-sound-data-location" #() snd-test-neq
  "oboe.snd" mus-nist set-mus-sound-header-type drop
  "oboe.snd" mus-sound-header-type mus-nist "set-mus-sound-header-type" #()
    snd-test-neq
  "oboe.snd" mus-lintn set-mus-sound-sample-type drop
  "oboe.snd" mus-sound-sample-type mus-lintn "set-mus-sound-sample-type" #()
    snd-test-neq
  "oboe.snd" cur-srate  set-mus-sound-srate drop
  "oboe.snd" cur-samps  set-mus-sound-samples drop
  "oboe.snd" cur-chans  set-mus-sound-chans drop
  "oboe.snd" cur-loc    set-mus-sound-data-location drop
  "oboe.snd" cur-type   set-mus-sound-header-type drop
  "oboe.snd" cur-format set-mus-sound-sample-type drop
  \
  "oboe.snd" open-sound to ind
  "test.wave" ind :header-type mus-riff save-sound-as drop
  "test.rf64" ind :header-type mus-rf64 save-sound-as drop
  "test.aifc" ind :header-type mus-aifc save-sound-as drop
  ind close-sound drop
  \
  #( "test.wave" "test.rf64" "test.aifc" ) each to file
    file mus-sound-srate to cur-srate
    file mus-sound-chans to cur-chans
    file mus-sound-sample-type to cur-format
    file mus-sound-header-type to cur-type
    file mus-sound-data-location to cur-loc
    file mus-sound-samples to cur-samps
    file cur-srate 2* set-mus-sound-srate drop
    file mus-sound-srate cur-srate 2* "%s set-mus-sound-srate" #( file )
      snd-test-neq
    file cur-samps 2* set-mus-sound-samples drop
    file mus-sound-samples cur-samps 2* "%s set-mus-sound-samples" #( file )
      snd-test-neq
    file cur-chans 2* set-mus-sound-chans drop
    file mus-sound-chans cur-chans 2* "%s set-mus-sound-chans" #( file )
      snd-test-neq
    file cur-loc 2* set-mus-sound-data-location drop
    file mus-sound-data-location cur-loc 2*
      "%s set-mus-sound-data-location" #( file ) snd-test-neq
    file mus-nist set-mus-sound-header-type drop
    file mus-sound-header-type mus-nist
      "%s set-mus-sound-header-type" #( file ) snd-test-neq
    file mus-lintn set-mus-sound-sample-type drop
    file mus-sound-sample-type mus-lintn 
      "%s set-mus-sound-sample-type" #( file ) snd-test-neq
    file cur-srate  set-mus-sound-srate drop
    file cur-samps  set-mus-sound-samples drop
    file cur-chans  set-mus-sound-chans drop
    file cur-loc    set-mus-sound-data-location drop
    file cur-type   set-mus-sound-header-type drop
    file cur-format set-mus-sound-sample-type drop
  end-each
  #( "test.wave" "test.rf64" "test.aifc" ) each to file
    file open-sound to ind
    ind srate to cur-srate
    ind chans to cur-chans
    ind sample-type to cur-format
    ind header-type to cur-type
    ind data-location to cur-loc
    ind framples to cur-samps
    ind cur-srate 2* set-srate drop
    ind srate cur-srate 2* "%s set-srate" #( ind file-name ) snd-test-neq
    cur-samps 2* ind set-framples drop
    ind framples cur-samps 2* "%s set-framples" #( ind file-name ) snd-test-neq
    ind cur-chans 2* set-chans drop  \ ; this can change the index
    file find-sound to ind
    ind chans cur-chans 2* "%s set-chans" #( ind file-name ) snd-test-neq
    ind cur-loc 2* set-data-location drop
    ind data-location cur-loc 2* "%s set-data-location" #( ind file-name )
      snd-test-neq
    ind mus-nist set-header-type drop
    ind header-type mus-nist "%s set-header-type" #( ind file-name )
      snd-test-neq
    ind mus-lintn set-sample-type drop
    ind sample-type mus-lintn "%s set-sample-type" #( ind file-name )
      snd-test-neq
    ind cur-srate  set-srate drop
    cur-samps ind  set-framples drop
    ind cur-chans  set-chans drop
    ind cur-loc    set-data-location drop
    ind cur-type   set-header-type drop
    ind cur-format set-sample-type drop
    ind close-sound drop
    file file-delete
  end-each
  with-big-file if
    bigger-snd file-exists? if
      \ ; silence as last .9 secs, so it probably wasn't written
      44100 71999.1 f* floor f>d { probable-framples }
      3175160310 { our-framples }
      6350320648 { our-length }
      bigger-snd mus-sound-samples our-framples "bigger samples"
        #() snd-test-neq
      bigger-snd mus-sound-framples our-framples "bigger framples"
        #() snd-test-neq
      bigger-snd mus-sound-framples probable-framples <'> d=
        "bigger framples (probable)" #() snd-test-any-neq
      bigger-snd mus-sound-length our-length "bigger bytes" #() snd-test-neq
      bigger-snd mus-sound-duration 71999.1015 "bigger dur" #() snd-test-neq
      bigger-snd open-sound to ind
      ind framples our-framples "bigger framples" #() snd-test-neq
      ind framples to big-file-framples
      big-file-framples probable-framples <'> d= "bigger framples (probable)"
        #() snd-test-any-neq
      big-file-framples ind 0 0 framples "bigger edpos-framples"
        #() snd-test-neq
      44100 50000 d* ind add-mark to m1
      m1 mark-sample 44100 50000 d* "bigger mark at" #() snd-test-neq
      m1 44100 66000 d* set-mark-sample drop
      m1 mark-sample 44100 66000 d* "bigger mark to" #() snd-test-neq
      "oboe.snd" 44100 60000 d* mix-sound car { mx }
      mx mix? if
        mx mix-position 44100 60000 d* "bigger mix at" #() snd-test-neq
        mx 44100 61000 d* set-mix-position drop
        mx mix-position 44100 61000 d* "bigger mix to" #() snd-test-neq
      else
        "no mix tag from mix-sound" #() snd-display
      then
      2 undo drop
      <'> f0<> 1 make-proc find-channel to res
      res false?
      res 100 > || if
        "bigger find not 0.0: %s" #( res ) snd-display
      then
      selection-creates-region { old-select }
      #f set-selection-creates-region drop
      ind select-all drop
      selection-framples ind 0 undef framples "bigger select all"
        #() snd-test-neq
      44100 50000 d* set-selection-position drop
      selection-position 44100 50000 d* "bigger select pos" #() snd-test-neq
      0 set-selection-position drop
      44100 65000 d* set-selection-framples drop
      selection-framples 44100 65000 d* "bigger select len" #() snd-test-neq
      old-select set-selection-creates-region drop
      44100 50000 d* ind set-cursor drop
      ind cursor 44100 50000 d* "bigger cursor" #() snd-test-neq
      44123 51234 d* ind add-mark to m1
      m1 mark-sample 44123 51234 d* "bigger mark at" #() snd-test-neq
      44123 51234 d* find-mark { mid }
      mid m1 "bigger mark seach" #() snd-test-neq
      "oboe.snd" 44123 61234 d* mix-sound car to mx
      44123 61234 d* find-mix { mxd }
      mxd mx "bigger find-mix" #() snd-test-neq
      44123 51234 d* ind set-cursor drop
      ind cursor 44123 51234 d* "bigger cursor 123" #() snd-test-neq
      ind close-sound drop
    else                        \ bigger-snd not file-exists?
      "no such bigger file %s" #( bigger-snd ) snd-display
    then                        \ bigger-snd file-exists?
  then                          \ with-big-file
  \
  "tmp.snd" 1 22050 mus-l24int mus-riff :size 100000 new-sound to ind
  selection-creates-region { old-selection-creates-region }
  #t set-selection-creates-region drop
  undef undef undef framples to len
  len 1/f { incr }
  -0.5 ( x )
  len 0.0 make-vct map!
    ( x ) incr f+ dup
  end-map ( data ) 0 len undef undef undef "" vct->channel drop ( x ) drop
  save-sound drop
  ind close-sound drop
  "tmp.snd" open-sound to ind
  select-all { reg }
  "tmp1.snd" 22050 mus-l24int mus-next save-selection drop
  "tmp1.snd" open-sound { ind1 }
  -0.5 undef undef undef framples 1/f sndlib-test-scan-x-cb 0 100000 ind1
    scan-channel to res
  res #f "l24 (next) selection not saved correctly" #() snd-test-neq
  ind1 close-sound drop
  "tmp1.snd" 22050 mus-l24int mus-aifc save-selection drop
  "tmp1.snd" open-sound to ind1
  -0.5 undef undef undef framples 1/f sndlib-test-scan-x-cb 0 100000 ind1
    scan-channel to res
  res #f "l24 (aifc) selection not saved correctly" #() snd-test-neq
  ind1 close-sound drop
  reg "tmp1.snd" mus-l24int mus-next save-region drop
  "tmp1.snd" open-sound to ind1
  -0.5 undef undef undef framples 1/f sndlib-test-scan-x-cb 0 100000 ind1
    scan-channel to res
  res #f "l24 (next) region not saved correctly" #() snd-test-neq
  ind1 close-sound drop
  "tmp1.snd" file-delete
  ind close-sound drop
  "tmp.snd" file-delete
  old-selection-creates-region set-selection-creates-region drop
  \
  "tmp.snd" 1 22050 mus-bfloat mus-next :size 10 :comment #f new-sound to ind
  <'> sndlib-test-map-set-1.0 map-channel drop
  '( 0 0 0.1 0.1 0.2 0.2 0.3 0.3 0.4 0.4 0.5 0.5
     0.6 0.6 0.7 0.7 0.8 0.8 0.9 0.9 ) env-channel drop
  channel->vct
  vct( 0.000 0.100 0.200 0.300 0.400 0.500 0.600 0.700 0.800 0.900 )
  "ramp env by 0.1" #() snd-test-neq
  ind close-sound drop
;

: sndlib-hook-2-t#-cb <{ a b -- f }> #t ;
: sndlib-hook-1-t#-cb <{ a -- f }> #t ;

: make-aifc-file { frms auth-lo bits -- }
  "test.aif" make-file-output-port { io }
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o146 port-putc \ len
  io "AIFCFVER" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ version chunk size
  io 0o242 port-putc  io 0o200 port-putc  io 0o121 port-putc  io 0o100 port-putc \ version
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io frms  port-putc \ framples
  io 0o000 port-putc  io bits  port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "AUTH" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io auth-lo port-putc \ AUTH chunk size
  io "bil" port-write
  io 0o000 port-putc
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io port-close
;

: (04-sndlib-02) ( -- )
  open-raw-sound-hook reset-hook!
  open-raw-sound-hook <'> sndlib-hook-2-t#-cb add-hook!
  bad-header-hook reset-hook!
  bad-header-hook <'> sndlib-hook-1-t#-cb add-hook!
  open-raw-sound-hook empty? if
    "add-hook open-raw-sound-hook failed??" #() snd-display
  then
  bad-header-hook empty? if
    "add-hook bad-header-hook failed??" #() snd-display
  then
  #( ".snd" "FORM" "AIFF" "AIFC" "COMM" "COMT" "INFO" "INST" "inst" "MARK"
     "SSND" "FVER" "NONE" "ULAW" "ulaw" "ima4" "raw " "sowt" "in32" "in24"
     "ni23" "fl32" "FL32" "fl64" "twos" "ALAW" "alaw" "APPL" "CLM " "RIFF"
     "RIFX" "WAVE" "fmt " "data" "fact" "clm " "NIST" "8SVX" "16SV" "Crea"
     "tive" "SOUN" "D SA" "MPLE" "BODY" "VHDR" "CHAN" "ANNO" "NAME" "2BIT" 
     "HCOM" "FSSD" "%//\n" "%---" "ALaw" "Soun" "MAUD" "MHDR" "MDAT" "mdat"
     "MThd" "sfbk" "sdta" "shdr" "pdta" "LIST" "GF1P" "ATCH" "$SIG" "NAL_"
     "GOLD" " SAM" "SRFS" "Diam" "ondW" "CSRE" "SND " "SNIN" "SNDT" "DDSF"
     "FSMu" "UWFD" "LM89" "SY80" "SY85" "SCRS" "DSPL" "AVI " "strf" "movi"
     "PRAM" " paf" "fap " "DS16" "HEDR" "HDR8" "SDA_" "SDAB" "SD_B" "NOTE"
     "file" "=sam" "SU7M" "SU7R" "PVF1" "PVF2" "AUTH" "riff" "TWIN" "IMPS"
     "SMP1" "Maui" "SDIF" "NVF " ) { magic-words }
  magic-words length { len }
  nil nil nil nil { magic io res ind }
  magic-words each to magic
    open-raw-sound-hook empty? if
      "open-raw-sound-hook cleared??" #() snd-display
    then
    bad-header-hook empty? if
      "bad-header-hook cleared??" #() snd-display
    then
    "test.snd" file-delete
    "test.snd" mus-sound-forget drop
    \ ;; try random garbage
    "test.snd" make-file-output-port to io
    io magic port-write
    128 0 do
      io "%f" #( 1.0 random ) port-write-format
    loop
    io port-close
    "test.snd" <'> open-sound #t nil fth-catch if
      stack-reset
    else
      to res
      res number? if
        res sound? if
          "open-sound garbage: %s %s" #( magic res ) snd-display
          res close-sound drop
        then
      then
    then
    "test.snd" file-delete
    "test.snd" mus-sound-forget drop
    \ ;; try plausible garbage
    "test.snd" make-file-output-port to io
    io magic port-write
    128 0 do
      io "%d" #( 128 random f>s ) port-write-format
    loop
    io port-close
    "test.snd" <'> open-sound #t nil fth-catch if
      stack-reset
    else
      to res
      res number? if
        res sound? if
          "open-sound plausible garbage: %s %s" #( magic res ) snd-display
          res close-sound drop
        then
      then
    then
    "test.snd" file-delete
    "test.snd" mus-sound-forget drop
    \ ;; write very plausible garbage
    "test.snd" make-file-output-port to io
    io magic port-write
    12 1 do
      io magic-words
        j ( ctr ) i + len < if
          j i +
        else
          i
        then array-ref port-write
    loop
    io port-close
    "test.snd" <'> open-sound #t nil fth-catch if
      stack-reset
    else
      to res
      res number? if
        res sound? if
          "open-sound very plausible garbage: %s %s" #( magic res ) snd-display
          res close-sound drop
        then
      then
    then
  end-each                      \ magic-words
  "test.snd" file-delete
  "test.snd" mus-sound-forget drop
  \
  "test.snd" make-file-output-port to io
  io ".snd" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o034 port-putc \ location
  io 0o000 port-putc  io 0o001 port-putc  io 0o215 port-putc  io 0o030 port-putc \ nominal size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o022 port-putc \ format
  io 0o000 port-putc  io 0o000 port-putc  io 0o126 port-putc  io 0o042 port-putc \ srate
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o001 port-putc \ chans
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ comment
  io 0o000 port-putc  io 0o001 port-putc \ samp 1
  io port-close
  "test.snd" mus-sound-sample-type mus-bshort "next 18" #() snd-test-neq
  "test.snd" file-delete
  "test.snd" mus-sound-forget drop
  \
  "test.snd" make-file-output-port to io
  io ".snd" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ location
  io 0o000 port-putc  io 0o001 port-putc  io 0o215 port-putc  io 0o030 port-putc \ nominal size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o022 port-putc \ format
  io 0o000 port-putc  io 0o000 port-putc  io 0o126 port-putc  io 0o042 port-putc \ srate
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o001 port-putc \ chans
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ comment
  io 0o000 port-putc  io 0o001 port-putc \ samp 1
  io port-close
  "test.snd" <'> open-sound #t nil fth-catch if
    stack-reset
  else
    to res
    res number? if
      res sound? if
        "open-sound next bad location %d: %s"
          #( res data-location res ) snd-display
        res close-sound drop
      then
    then
  then
  "test.snd" file-delete
  "test.snd" mus-sound-forget drop
  \
  \ XXX: Opening this file triggers "not quoted: `pwd`/test.snd" from
  \      snd-file.c, function quoted_filename() (via translate).
  \
  "test.snd" make-file-output-port to io
  io ".snd" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o034 port-putc \ location
  io 0o000 port-putc  io 0o001 port-putc  io 0o215 port-putc  io 0o030 port-putc \ nominal size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o122 port-putc \ format
  io 0o000 port-putc  io 0o000 port-putc  io 0o126 port-putc  io 0o042 port-putc \ srate
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o001 port-putc \ chans
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ comment
  io 0o000 port-putc  io 0o001 port-putc \ samp 1
  io port-close
  "test.snd" <'> open-sound #t nil fth-catch if
    stack-reset
  else
    to res
    res sound? if
      "open-sound next bad format %s: %s" #( res sample-type res ) snd-display
      res close-sound drop
    then
  then
  "test.snd" file-delete
  "test.snd" mus-sound-forget drop
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \ ;;correct (make-aifc-file #o002 #o004 #o020)
  0o102 0o004 0o020 make-aifc-file
  "test.aif" open-sound to ind
  ind framples 2 "bad framples in header" #() snd-test-neq
  ind close-sound drop
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  0o002 0o150 0o020 make-aifc-file
  "test.aif" <'> open-sound #t nil fth-catch if
    stack-reset
  else
    to res
    res sound? if
      "open-sound aifc no ssnd chunk %d: %s"
        #( res data-location res ) snd-display
      res close-sound drop
    then
  then
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  0o002 0o000 0o020 make-aifc-file
  "test.aif" <'> open-sound #t nil fth-catch if
    stack-reset
  else
    to res
    res sound? if
      "open-sound aifc 0-len auth chunk %d: %s"
        #( res data-location res ) snd-display
      res close-sound drop
    then
  then
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  0o002 0o150 0o120 make-aifc-file
  "test.aif" <'> open-sound #t nil fth-catch if
    stack-reset
  else
    to res
    res sound? if
      "open-sound bits 80 %s: %s" #( res sample-type res ) snd-display
      res close-sound drop
    then
  then
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o176 port-putc \ len
  io "AIFCFVER" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ version chunk size
  io 0o242 port-putc  io 0o200 port-putc  io 0o121 port-putc  io 0o100 port-putc \ version
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o002 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "AUTH" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ AUTH chunk size
  io "bil" port-write
  io 0o000 port-putc
  io "ANNO" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ AUTH chunk size
  io "cat" port-write
  io 0o000 port-putc
  io "NAME" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ AUTH chunk size
  io "dog" port-write
  io 0o000 port-putc
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io port-close
  "test.aif" mus-sound-comment length 15 "aifc 3 aux comments" #() snd-test-neq
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o142 port-putc \ len
  io "AIFC" port-write
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o002 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "COMT" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  io "bil" port-write
  io 0o000 port-putc
  io port-close
  "test.aif" mus-sound-comment 0 3 string-substring "bil"
    "aifc trailing comt comments" #() snd-test-neq
  "test.aif" mus-sound-framples 2 "aifc trailing comt framples"
    #() snd-test-neq
  "test.aif" open-sound to ind
  0 sample { s0 }
  1 sample { s1 }
  2 sample { s2 }
  3 sample { s3 }
  vct( s0 s1 s2 s3 ) vct( 0.00198 0.00195 0.0 0.0 ) 
    "aifc trailing comt samps" #() snd-test-neq
  ind close-sound drop
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o142 port-putc \ len
  io "AIFC" port-write
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o100 port-putc  io 0o102 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "COMT" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  io "bil" port-write
  io 0o000 port-putc
  io port-close
  "test.aif" mus-sound-comment 0 3 string-substring "bil" 
    "aifc trailing comt comments" #() snd-test-neq
  "test.aif" mus-sound-framples 2 "aifc trailing comt (bogus) framples"
    #() snd-test-neq
  "test.aif" open-sound to ind
  0 sample to s0
  1 sample to s1
  2 sample to s2
  3 sample to s3
  vct( s0 s1 s2 s3 ) vct( 0.00198 0.00195 0.0 0.0 )
    "aifc trailing comt samps (bogus frame setting)" #() snd-test-neq
  ind close-sound drop
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o142 port-putc \ len
  io "AIFC" port-write
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o100 port-putc  io 0o102 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io port-close
  "test.aif" <'> open-sound #t nil fth-catch if
    stack-reset
  else
    to res
    res sound? if
      "open-sound aifc 2 ssnd chunks %d: %s"
        #( res data-location res ) snd-display
      res close-sound drop
    then
  then
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o040 port-putc \ len
  io "AIFC" port-write
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o000 port-putc  io 0o101 port-putc  io 0o000 port-putc  io 0o100 port-putc \ two samples
  io port-close
  "test.aif" <'> open-sound snd-test-catch to res
  res car 'mus-error "open-sound aifc no comm chunk: %s" #( res ) snd-test-neq
  sounds each ( snd )
    close-sound drop
  end-each
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  \ write AIFC with trailing chunks to try to confuse file->sample
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o176 port-putc \ len
  io "AIFCFVER" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ version chunk size
  io 0o242 port-putc  io 0o200 port-putc  io 0o121 port-putc  io 0o100 port-putc \ version
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o002 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o170 port-putc  io 0o101 port-putc  io 0o100 port-putc  io 0o100 port-putc \ two samples
  io "AUTH" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ AUTH chunk size
  io "bil" port-write
  io 0o000 port-putc
  io "ANNO" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ AUTH chunk size
  io "cat" port-write
  io 0o000 port-putc
  io "NAME" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ AUTH chunk size
  io "dog" port-write
  io 0o000 port-putc
  io port-close
  "test.aif" make-file->sample { gen }
  gen #( 0 ) object-apply 0.93948 "file->sample chunked 0" #() snd-test-neq
  gen #( 1 ) object-apply 0.50195 "file->sample chunked 1" #() snd-test-neq
  gen #( 2 ) object-apply 0.0 "file->sample chunked eof" #() snd-test-neq
  gen #( 3 ) object-apply 0.0 "file->sample chunked eof+1" #() snd-test-neq
  "test.aif" open-sound to ind
  ind framples 2 "chunked framples" #() snd-test-neq
  0 sample 0.93948 "file chunked 0" #() snd-test-neq
  1 sample 0.50195 "file chunked 1" #() snd-test-neq
  2 sample 0.0 "file chunked eof" #() snd-test-neq
  3 sample 0.0 "file chunked eof+1" #() snd-test-neq
  ind close-sound drop
  "test.aif" mus-sound-framples 2 "chunked mus-sound-framples" #() snd-test-neq
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  \ write AIFC with trailing chunks to try to confuse file->sample
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o176 port-putc \ len
  io "AIFCFVER" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ version chunk size
  io 0o242 port-putc  io 0o200 port-putc  io 0o121 port-putc  io 0o100 port-putc \ version
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o170 port-putc  io 0o101 port-putc  io 0o100 port-putc  io 0o100 port-putc \ two samples
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o001 port-putc \ 1 chan
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o002 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "APPL" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io <char> h port-putc
  io "CLM ;Written Mon 02-Nov-98 01:44 CST by root at ockeghem (Linux/X86) using Allegro CL, clm of 20-Oct-98" port-write
  io 0o000 port-putc
  io port-close
  "test.aif" make-file->sample to gen
  gen #( 0 ) object-apply 0.93948 "file->sample chunked 0" #() snd-test-neq
  gen #( 1 ) object-apply 0.50195 "file->sample chunked 1" #() snd-test-neq
  gen #( 2 ) object-apply 0.0 "file->sample chunked eof" #() snd-test-neq
  gen #( 3 ) object-apply 0.0 "file->sample chunked eof+1" #() snd-test-neq
  "test.aif" open-sound to ind
  ind framples 2 "chunked framples" #() snd-test-neq
  0 sample 0.93948 "file chunked 0" #() snd-test-neq
  1 sample 0.50195 "file chunked 1" #() snd-test-neq
  2 sample 0.0 "file chunked eof" #() snd-test-neq
  3 sample 0.0 "file chunked eof+1" #() snd-test-neq
  comment ";Written Mon 02-Nov-98 01:44 CST by root at ockeghem (Linux/X86) using Allegro CL, clm of 20-Oct-98"
  "chunked appl comment" #() snd-test-neq
  ind close-sound drop
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  "test.aif" make-file-output-port to io
  \ write AIFC with trailing chunks to try to confuse file->sample
  io "FORM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o176 port-putc \ len
  io "AIFCFVER" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o004 port-putc \ version chunk size
  io 0o242 port-putc  io 0o200 port-putc  io 0o121 port-putc  io 0o100 port-putc \ version
  io "SSND" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o014 port-putc \ SSND chunk size
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ SSND data loc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc \ block size?
  io 0o170 port-putc  io 0o101 port-putc  io 0o100 port-putc  io 0o100 port-putc \ two samples
  io "COMM" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o046 port-putc \ COMM chunk size
  io 0o000 port-putc  io 0o002 port-putc \ 2 chans
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o001 port-putc \ framples
  io 0o000 port-putc  io 0o020 port-putc \ bits
  io 0o100 port-putc  io 0o016 port-putc  io 0o254 port-putc  io 0o104 port-putc  io 0o000 port-putc
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc
  \ srate as 80-bit float (sheesh)
  io "NONE" port-write    \ compression
  io 0o016 port-putc      \ pascal string len
  io "not compressed" port-write
  io 0o000 port-putc
  io "APPL" port-write
  io 0o000 port-putc  io 0o000 port-putc  io 0o000 port-putc  io <char> h port-putc
  io "CLM ;Written Mon 02-Nov-98 01:44 CST by root at ockeghem (Linux/X86) using Allegro CL, clm of 20-Oct-98" port-write
  io 0o000 port-putc
  io port-close
  "test.aif" make-file->sample to gen
  gen #( 0 0 ) object-apply 0.93948 "file->sample chunked 0 0" #() snd-test-neq
  gen #( 0 1 ) object-apply 0.50195 "file->sample chunked 0 1" #() snd-test-neq
  gen #( 1 0 ) object-apply 0.0 "file->sample chunked eof (stereo)" #()
    snd-test-neq
  gen #( 1 1 ) object-apply 0.0 "file->sample chunked eof+1 (stereo)" #()
    snd-test-neq
  "test.aif" open-sound to ind
  ind framples 1 "chunked framples (1)" #() snd-test-neq
  0 ind 0 sample 0.93948 "file chunked 0 0" #() snd-test-neq
  0 ind 1 sample 0.50195 "file chunked 0 1" #() snd-test-neq
  1 ind 0 sample 0.0 "file chunked eof (stereo)" #() snd-test-neq
  1 ind 1 sample 0.0 "file chunked eof+1 (stereo)" #() snd-test-neq
  comment ";Written Mon 02-Nov-98 01:44 CST by root at ockeghem (Linux/X86) using Allegro CL, clm of 20-Oct-98"
  "chunked appl comment (stereo)" #() snd-test-neq
  ind close-sound drop
  "test.aif" file-delete
  "test.aif" mus-sound-forget drop
  \
  file-pwd sound-files-in-directory { files }
  files empty? if
    "no sound files in %s?" #( file-pwd ) snd-display
  then
  sound-files-in-directory { files1 }
  files files1 "different sound files in %s and default" #( file-pwd ) 
    snd-test-neq
  "." sound-files-in-directory { files2 }
  files1 files2 "sound-files-in-directory dot" #() snd-test-neq
  files  files2 "sound-files-in-directory dot" #() snd-test-neq
  \
  bad-header-hook reset-hook!
  open-raw-sound-hook reset-hook!
  sounds each ( snd )
    close-sound drop
  end-each
  \
  :size 0 new-sound to ind
  ind framples 0 "new-sound :size 0 framples" #() snd-test-neq
  0 sample 0.0 "new-sound :size 0 sample 0" #() snd-test-neq
  ind file-name { new-file-name }
  ind close-sound drop
  new-file-name file-delete
  :size 1 new-sound to ind
  ind framples 1 "new-sound :size 1 framples" #() snd-test-neq
  0 sample 0.0 "new-sound :size 1 sample 0" #() snd-test-neq
  ind file-name to new-file-name
  ind close-sound drop
  new-file-name file-delete
  :size -1 <'> new-sound snd-test-catch to res
  res car 'out-of-range "new-sound :size -1: %s" #( res ) snd-test-neq
  \
  "caruso.asc" check-file-name { fsnd }
  fsnd file-exists? if
    fsnd read-ascii to ind
    ind sound? if
      ind 0 maxamp 0.723 "read-ascii maxamp" #() snd-test-neq
      ind 0 framples 50000 "read-ascii framples" #() snd-test-neq
      ind srate    44100 "read-ascii srate"  #() snd-test-neq
      ind 8000 set-srate drop
      ind 0 maxamp 0.723 "set srate clobbered new sound (maxamp)"
        #() snd-test-neq
      ind 0 framples 50000 "set srate clobbered new sound (framples)"
        #() snd-test-neq
      ind close-sound drop
    else
      "read-ascii can't find %s?" #( fsnd ) snd-display
    then
  then
  \
  "oboe.snd" open-sound to ind
  "test space.snd" save-sound-as drop
  ind close-sound drop
  "test space.snd" open-sound to ind
  ind short-file-name "test space.snd" "file name with space" #() snd-test-neq
  ind framples "test space.snd" mus-sound-framples "spaced filename framples"
    #() snd-test-neq
  1234 ind 0 add-mark drop
  ind save-marks drop      \ ; should write "test space.marks"
  ind close-sound drop
  "test space.snd" open-sound to ind
  file-pwd "/" $+ "test space.marks" $+ file-eval
  1234 ind find-mark unless
    "space file name save marks?" #() snd-display
  then
  :file "test space.snd" make-readin { rd }
  rd mus-file-name "test space.snd" "file name with space readin" #()
    snd-test-neq
  ind close-sound drop
  "test space.snd" file-delete
  "test space.marks" file-delete
  \ XXX: S7 specific tests skipped
;

: 04-sndlib ( -- )
  *tests* 0 ?do
    i to *clmtest*
    *snd-test-verbose*
    *tests* 1 > && if
      "clmtest %d of %d" #( *clmtest* 1+ *tests* ) snd-test-message
    then
    clear-listener drop
    (04-sndlib-01)
  loop
  (04-sndlib-02)
;

\ ---------------- test 05: simple overall checks ----------------

: ccvp-01-cb { y data forward -- r }
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  angle fcos y f* { val }
  data 0 angle incr forward if f+ else f- then vct-set! drop
  val
;

half-pi fnegate constant -half-pi

: ccvp-02-cb { frag-beg frag-dur -- v }
  pi frag-dur f/ { incr }
  -half-pi  frag-beg incr f*  f+
  incr 2 >vct
;

0 value a-ctr
0 value g-init-val

: append-sound { fname -- }
  fname undef undef undef framples insert-sound drop
;

: cc-01-cb { incr -- prc; y self -- r }
  1 proc-create incr , -half-pi ( angle ) , ( prc )
 does> { y self -- r }
  self @ { incr }
  self cell+ @ { angle }
  y angle f* { val }
  angle incr f+ self cell+ ! ( angle += incr )
  val
;

: cosine-channel <{ :optional beg 0 dur #f snd #f chn #f edpos #f -- }>
  pi dur if
      dur
    else
      snd chn undef framples
    then  f/ ( incr ) cc-01-cb beg dur snd chn edpos map-channel drop
;

: (05-simple-check-01) ( -- )
  playing if
    "dac is running??" #() snd-display
  then
  "oboe.snd" open-sound { ind }
  #t ind 0 set-transform-graph? drop
  graph-as-sonogram ind 0 set-transform-graph-type drop
  "hiho" ind 0 1 <'> y-axis-label 'no-such-axis nil fth-catch if
    "no fft axis?" #() snd-display
  then
  stack-reset
  #t ind 0 set-fft-log-frequency drop
  ind 0 update-transform-graph drop
  ind close-sound drop
;

: 05-simple-check ( -- )
  *tests* 0 ?do
    i to *clmtest*
    *snd-test-verbose*
    *tests* 1 > && if
      "clmtest %d of %d" #( *clmtest* 1+ *tests* ) snd-test-message
    then
    (05-simple-check-01)
  loop
;

\ ---------------- test 08: clm ----------------

lambda: <{ -- r }>       0.0 ; value 08-clm-lambda-0.0
lambda: <{ dir -- r }>   1.0 ; value 08-clm-lambda-dir-1.0
lambda: <{ a b c -- r }> 1.0 ; value 08-clm-lambda-a-b-c-1.0
32 make-delay constant make-delay-32

\ xen-mus-apply (using mus-run):
\ S7:    (gen arg)
\ Ruby:  gen.call(arg)
\ Forth: gen '( arg ) apply
\
\ mus-apply ( args -- res )
\ mus-run ( gen :optional arg1 0.0 arg2 0.0 -- res )

: random-gen-run ( ?? make-prc random-args -- )
  { make-prc random-args }
  make-prc #t nil fth-catch if
    stack-reset
    nil
  then { gen }
  nil { arg }
  gen mus-generator? if
    random-args each to arg
      \ ~133.630s apply
      \ ~144.490s mus-run
      \ ~122.690s mus-apply
      \ gen '( arg ) <'> apply #t nil fth-catch
      \ gen arg undef <'> mus-run #t nil fth-catch
      gen arg <'> mus-apply #t nil fth-catch
      stack-reset
    end-each
  then
;

: random-gen ( -- )
  #( 2.0 21.5 f**
     2.0 -18.0 f**
     1.5
     "/hiho"
     list( 0 1 )
     1234
     vct( 0 0 0 )
     0.1 0.2 0.3 make-color-with-catch
     #( 0 1 )
     3/4
     0+i
     make-delay-32
     08-clm-lambda-0.0
     08-clm-lambda-dir-1.0
     08-clm-lambda-a-b-c-1.0
     0
     1
     -1
     #f
     #t
     <char> c
     0.0
     1.0
     -1.0
     '()
     32
     '( 1 2 ) ) { random-args }
  \
  \ XXX: make-asymmetric-fm
  \      Starting with 3 args, make-asymmetric-fm makes trouble
  \      resulting in #<undefined-word in interpret: 10> exception
  \      after! finishing the entire 08-clm test.
  \
  #( <'> make-all-pass <'> make-asymmetric-fm <'> make-moving-average
     <'> make-moving-max <'> make-moving-norm <'> make-table-lookup
     <'> make-triangle-wave <'> make-comb <'> make-delay <'> make-env
     <'> make-fft-window <'> make-filter <'> make-filtered-comb
     <'> make-fir-filter <'> make-formant <'> make-iir-filter <'> make-locsig
     <'> make-notch <'> make-one-pole <'> make-one-pole-all-pass
     <'> make-one-zero <'> make-oscil <'> make-pulse-train
     <'> make-rand <'> make-rand-interp <'> make-sawtooth-wave
     <'> make-polyshape <'> make-polywave <'> make-square-wave
     <'> make-two-pole <'> make-two-zero <'> make-wave-train
     <'> make-ssb-am ) { gen-make-procs }
  nil { make-prc }
  nil nil nil nil { arg1 arg2 arg3 arg4 }
  gen-make-procs each to make-prc
    make-prc random-args random-gen-run
  end-each
  random-args each to arg1
    gen-make-procs each to make-prc
      arg1 make-prc random-args random-gen-run
    end-each
    random-args each to arg2
      gen-make-procs each to make-prc
        arg1 arg2 make-prc random-args random-gen-run
      end-each
      random-args each to arg3
        gen-make-procs each to make-prc
          <'> make-asymmetric-fm make-prc <> if 
            arg1 arg2 arg3 make-prc random-args random-gen-run
          then
        end-each
        random-args each to arg4
          gen-make-procs each to make-prc
            <'> make-asymmetric-fm make-prc <> if 
              arg1 arg2 arg3 arg4 make-prc random-args random-gen-run
            then
          end-each
        end-each
      end-each
    end-each
  end-each
;

: 08-clm ( -- )
  all-args if
    random-gen
  then
;

\ ---------------- test 10: marks ----------------

: 10-marks ( -- )
  "oboe.snd" open-sound { ind }
  123 add-mark drop
  234 ind 0 "hiho"     1 add-mark drop
  345 ind 0 #f         1 add-mark drop
  456 ind 0 "a mark" 2 add-mark drop
  567 ind 0 #f         1 add-mark drop
  ind "oboe.marks" save-marks drop
  ind close-sound drop
  "oboe.snd" open-sound to ind
  1 ind 0 "new mark" 1 add-mark drop
  "oboe.marks" file-eval
  123 ind 0 find-mark { m }
  m mark? if
    m mark-name length zero? unless
      "saved mark 123 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync zero? unless
      "saved mark 123 sync: %s?" #( m mark-sync ) snd-display
    then
  else
    "saved marks missed 123: %s?" #( m ) snd-display
  then
  234 ind 0 find-mark to m
  m mark? if
    m mark-name "hiho" string<> if
      "saved mark 234 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync { m2sync }
    m2sync 0= m2sync 1 = || if
      "saved mark 234 sync: %s?" #( m mark-sync ) snd-display
    then
    m mark-sync
  else
    "saved marks missed 234: %s?" #( m ) snd-display
    0
  then { m1-sync }
  345 ind 0 find-mark to m
  m mark? if
    m mark-name length zero? unless
      "saved mark 345 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync m1-sync <> if
      "saved mark 345 sync: %s %s?" #( m mark-sync m1-sync ) snd-display
    then
  else
    "saved marks missed 345: %s?" #( m ) snd-display
  then
  456 ind 0 find-mark to m
  m mark? if
    m mark-name "a mark" string<> if
      "saved mark 456 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync { m4sync }
    m4sync m1-sync = m4sync 0= || m4sync 1 = || if
      "saved mark 456 sync: %s %s?" #( m mark-sync m1-sync ) snd-display
    then
  else
    "saved marks missed 456: %s?" #( m ) snd-display
  then
  567 ind 0 find-mark to m
  m mark? if
    m mark-name length zero? unless
      "saved mark 567 name: %s?" #( m mark-name ) snd-display
    then
    m mark-sync m1-sync <> if
      "saved mark 567 sync: %s %s?" #( m mark-sync m1-sync ) snd-display
    then
  else
    "saved marks missed 567: %s?" #( m ) snd-display
  then
  ind close-sound drop
;

\ ---------------- test 15: chan-local vars ----------------

: interpolated-peak-offset ( r1 r2 r3 -- r4 )
  { la ca ra }
  la ca fmax ra fmax 0.001 f+ { pk }
  la 0.0000001 fmax pk f/ flog 10 flog f/ { logla }
  ca 0.0000001 fmax pk f/ flog 10 flog f/ { logca }
  ra 0.0000001 fmax pk f/ flog 10 flog f/ { logra }
  logla logra f- f2/
  logla logra f+
  logca f2*  f-  f/
;

: freq-peak { beg ind size -- lst }
  beg size ind 0 channel->vct { data }
  data blackman2-window size #t 0.0 #f #t snd-spectrum { spectr }
  0.0 { peak0 }
  0 { pk0loc }
  size 2/ 0 ?do
    spectr i vct-ref peak0 f> if
      spectr i vct-ref to peak0
      i to pk0loc
    then
  loop
  pk0loc 0> if
    spectr pk0loc 1- vct-ref  spectr pk0loc vct-ref  spectr pk0loc 1+ vct-ref
    interpolated-peak-offset
  else
    0.0
  then pk0loc f+  #f srate f*  size f/  peak0 2 >array
;

: src-test15-cb ( os -- proc; y self -- val )
  1 proc-create swap ,
 does> { y self -- val }
  self @ ( os ) 0.0 0.0 oscil f2/
;

: f3neq ( a b -- f ) f- fabs 10.0 f> ;
: f4neq ( a b -- f ) f- fabs  1.0 f> ;
: f5neq ( a b -- f ) { a b } a b f- fabs 10.0  a b fmax 0.05 f*  f> ;

*with-test-complex* [if]
  \ dolph/dolph-1 are only defined if complex numbers available
  : dolph-test ( -- )
    16 1.0 dolph { val1 }
    dolph-chebyshev-window 16 1.0 make-fft-window { val2 }
    val1 val2 vequal? unless
      "dolph/dolph 1: %s %s" #( val1 val2 ) snd-display
    then
    16 1.0 dolph-1 to val1
    val1 val2 vequal? unless
      "dolph-1/dolph 1: %s %s" #( val1 val2 ) snd-display
    then
  ;
[else]
  <'> noop alias dolph-test
[then]

: 15-chan-local-vars ( -- )
  mus-srate f>s { old-srate }
  22050 to *clm-srate*
  \ dsp.fs
  "test.snd" 1 22050 mus-bfloat mus-next "src-* tests" 10000 new-sound { ind }
  \ src-duration tests
  #( 0 1 1 2 )      src-duration { d1 }
  #( 0 2 1 1 )      src-duration { d2 }
  #( 0 1 0.5 2 )    src-duration { d3 }
  #( 0.5 1 0.75 2 ) src-duration { d4 }
  d1 0.693147180559945 fneq
  d2 d1 fneq ||
  d3 d1 fneq ||
  d4 d1 fneq || if
    "src-duration test1: %f %f %f %f" #( d1 d2 d3 d4 ) snd-display
  then
  #( 0 1 1 0.5 )      src-duration to d1
  #( 0 0.5 1 1 )      src-duration to d2
  #( 0 1 0.5 0.5 )    src-duration to d3
  #( 0.5 1 0.75 0.5 ) src-duration to d4
  d1 1.38629436111989 fneq
  d2 d1 fneq ||
  d3 d1 fneq ||
  d4 d1 fneq || if
    "src-duration test2: %f %f %f %f" #( d1 d2 d3 d4 ) snd-display
  then
  #( 0 1 1 1 ) src-duration to d1
  #( 0 2 1 2 ) src-duration to d2
  d1 1.0 fneq
  d2 0.5 fneq || if
    "src-duration test3: %f %f" #( d1 d2 ) snd-display
  then
  #( 0 0.5 0.5 3 0.6 1 0.7 0.1 0.8 1.5 1 1 ) src-duration to d1
  d1 1.02474349685432 fneq if
    "src-duration test4: %f" #( d1 ) snd-display
  then
  #( 0 1 1 2 2 1 ) src-duration to d1
  d1 0.693147180559945 fneq if
    "src-duration test5: %f" #( d1 ) snd-display
  then
  500.0 0.0 make-oscil src-test15-cb map-channel drop
  0 ind 8192 freq-peak { vals }
  500.0 vals 0 array-ref f4neq
  1.0   vals 1 array-ref fneq || if
    "src no-test: %s" #( vals ) snd-display
  then
  ind close-sound drop
  \
  dolph-test
  \ env.fs
  \ envelope-interp
  0.1 #( 0 0 1 1 ) 1.0 envelope-interp dup 0.1 fneq if
    "envelope-interp 0.1: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0 0 1 1 ) 32.0 envelope-interp dup 0.01336172 fneq if
    "envelope-interp 0.013: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0 0 1 1 ) 0.012 envelope-interp dup 0.36177473 fneq if
    "envelope-interp 0.361: %s?" swap snd-display
  else
    drop
  then
  0.3 #( 0 0 0.5 1 1 0 ) 1.0 envelope-interp dup 0.6 fneq if
    "envelope-interp 0.3 #( 0 0 0.5 1 1 0 ): %s?" swap snd-display
  else
    drop
  then
  \ window-envelope
  1.0 3.0 #( 0.0 0.0 5.0 1.0 ) window-envelope dup #( 1.0 0.2 3.0 0.6 ) feql if
    drop
  else
    1 >array "window-envelope: %s?" swap snd-display
  then
  \ multiply-envelopes
  #( 0 0 1 1 ) #( 0 0 1 1 2 0 ) multiply-envelopes dup
  #( 0 0 0.5 0.5 1 0 ) feql if
    drop
  else
    1 >array "multiply-envelopes: %s?" swap snd-display
  then
  \ max-envelope
  #( 0 0 1 1 2 3 4 0 ) max-envelope dup 3.0 fneq if
    "max-envelopes (0): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 ) max-envelope dup 1.0 fneq if
    "max-envelopes (1): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 1 1 2 2 ) max-envelope dup 2.0 fneq if
    "max-envelopes (2): %s?" swap snd-display
  else
    drop
  then
  #( 0 -1 1 -2 ) max-envelope dup -1.0 fneq if
    "max-envelopes (3): %s?" swap snd-display
  else
    drop
  then
  #( 0 -2 1 -1 ) max-envelope dup -1.0 fneq if
    "max-envelopes (4): %s?" swap snd-display
  else
    drop
  then
  \ min-envelope
  #( 0 0 1 1 2 3 4 0 ) min-envelope dup 0.0 fneq if
    "min-envelopes (0): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 ) min-envelope dup 1.0 fneq if
    "min-envelopes (1): %s?" swap snd-display
  else
    drop
  then
  #( 0 1 1 1 2 2 ) min-envelope dup 1.0 fneq if
    "min-envelopes (2): %s?" swap snd-display
  else
    drop
  then
  #( 0 -1 1 -2 ) min-envelope dup -2.0 fneq if
    "min-envelopes (3): %s?" swap snd-display
  else
    drop
  then
  #( 0 -2 1 -1 ) min-envelope dup -2.0 fneq if
    "min-envelopes (4): %s?" swap snd-display
  else
    drop
  then
  \ integrate-envelope
  #(  0 0 1 1 ) integrate-envelope dup 0.5 fneq if
    "integrate-envelopes (0): %s?" swap snd-display
  else
    drop
  then
  #(  0 1 1 1 ) integrate-envelope dup 1.0 fneq if
    "integrate-envelopes (1): %s?" swap snd-display
  else
    drop
  then
  #(  0 0 1 1 2 0.5 ) integrate-envelope dup 1.25 fneq if
    "integrate-envelopes (2): %s?" swap snd-display
  else
    drop
  then
  \ stretch-envelope
  #(  0 0 1 1 ) 0.1 0.2 #f #f stretch-envelope dup #( 0 0 0.2 0.1 1.0 1 )
  feql if
    drop
  else
    1 >array "stretch-envelope att: %s?" swap snd-display
  then
  #( 0 0 1 1 2 0 ) 0.1 0.2 1.5 1.6 stretch-envelope
  dup #( 0 0 0.2 0.1 1.1 1 1.6 0.5 2 0 ) feql if
    drop
  else
    1 >array "stretch-envelope dec: %s?" swap snd-display
  then
  \ add-envelopes
  #( 0 0 1 1 2 0 ) #( 0 0 1 1 ) add-envelopes
  dup #( 0 0 0.5 1.5 1 1 ) feql if
    drop
  else
    1 >array "add-envelopes: %s?" swap snd-display
  then
  \ scale-envelope
  #( 0 0 1 1 ) 2 0 scale-envelope dup #( 0 0 1 2 ) feql if
    drop
  else
    1 >array "scale-envelope: %s?" swap snd-display
  then
  #( 0 0 1 1 ) 2 1 scale-envelope dup #( 0 1 1 3 ) feql if
    drop
  else
    1 >array "scale-envelope off: %s?" swap snd-display
  then
  \ reverse-envelope
  #( 0 0 1 1 ) reverse-envelope dup #( 0 1 1 0 ) feql if
    drop
  else
    1 >array "reverse-envelope ramp: %s?" swap snd-display
  then
  #( 0 0 0.5 1 2 0 ) reverse-envelope dup #( 0 0 1.5 1 2 0 ) feql if
    drop
  else
    1 >array "reverse-envelope ramp 2: %s?" swap snd-display
  then
  #( 0 0 0.5 1 2 1 ) reverse-envelope dup #( 0 1 1.5 1 2 0 ) feql if
    drop
  else
    1 >array "reverse-envelope ramp 2: %s?" swap snd-display
  then
  \ concatenate-envelopes (from snd/env.scm)
  #( 0 0 1 1 ) #( 0 1 1 0 ) 2 concatenate-envelopes
  dup #( 0.0 0 1.0 1 2.0 0 ) feql if
    drop
  else
    1 >array "concatenate-envelopes (0): %s?" swap snd-display
  then
  #( 0 0 1 1.5 ) #( 0 1 1 0 ) 2 concatenate-envelopes
  dup #( 0.0 0 1.0 1.5 1.01 1 2.01 0 ) feql if
    drop
  else
    1 >array "concatenate-envelopes (1): %s?" swap snd-display
  then
  \ envelope-concatenate (from clm/env.lisp)
  #( 0 0 1 1 ) #( 0 1 1 0 ) 2 envelope-concatenate
  dup #( 0.0 0 1.0 1 1.01 1 2.01 0 ) feql if
    drop
  else
    1 >array "envelope-concatenate (0): %s?" swap snd-display
  then
  #( 0 0 1 1.5 ) #( 0 1 1 0 ) 2 envelope-concatenate
  dup #( 0.0 0 1.0 1.5 1.01 1 2.01 0 ) feql if
    drop
  else
    1 >array "envelope-concatenate (1): %s?" swap snd-display
  then
  \ repeat-envelope
  #( 0 0 1 100 ) 2 #f #f repeat-envelope
  dup #( 0 0 1 100 1.01 0 2.01 100 ) feql if
    drop
  else
    1 >array "repeat-envelope (0): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 2 #f #f repeat-envelope 
  dup #( 0 0 1.5 1 2.0 0 3.5 1 4.0 0 ) feql if
    drop
  else
    1 >array "repeat-envelope (1): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 2 #f #t repeat-envelope
  dup #( 0.0 0 0.75 1 1.0 0 1.75 1 2.0 0 ) feql if
    drop
  else
    1 >array "repeat-envelope (2): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 2 #t #f repeat-envelope
  dup #( 0 0 1.5 1 2.0 0 2.5 1 4.0 0 ) feql if
    drop
  else
    1 >array "repeat-envelope (3): %s?" swap snd-display
  then
  #( 0 0 1.5 1 2 0 ) 3 #f #f repeat-envelope dup
  #( 0 0 1.5 1 2.0 0 3.5 1 4.0 0 5.5 1 6.0 0 ) feql if
    drop
  else
    1 >array "repeat-envelope (4): %s?" swap snd-display
  then
  \ normalize-envelope
  #( 0 0 1 1.5 2.0 1.0 ) normalize-envelope dup #( 0 0.0 1 1.0 2.0 0.667 )
  feql if
    drop
  else
    1 >array "normalize-envelope (0): %s?" swap snd-display
  then
  #( 0 0 1 0.5 2 -0.8 ) normalize-envelope dup #( 0 0.0 1 0.625 2 -1.0 )
  feql if
    drop
  else
    1 >array "normalize-envelope (1): %s?" swap snd-display
  then
  \ envelope-exp
  #( 0 0 1 1 ) 2.0 10 envelope-exp dup
  #( 0 0 0.1 0.01 0.2 0.04 0.3 0.09 0.4 0.16 
     0.5 0.25 0.6 0.36 0.7 0.49 0.8 0.64 0.9 0.81 1 1 )
  feql if
    drop
  else
    1 >array "envelope-exp (0): %s?" swap snd-display
  then
  #( 0 0 1 1 2 0 ) 1.0 10 envelope-exp dup
  #( 0 0 0.2 0.2 0.4 0.4 0.6 0.6 0.8 0.8 1 1 
     1.2 0.8 1.4 0.6 1.6 0.4 1.8 0.2 2 0 )
  feql if
    drop
  else
    1 >array "envelope-exp (1): %s?" swap snd-display
  then
  old-srate to *clm-srate*
;

\ ---------------- test 19: save and restore ----------------

: clm-channel-test <{ :optional snd #f chn #f -- gen }>
  1 -1 make-two-zero 0 #f snd chn #f #f get-func-name clm-channel
;

: random-pi-func <{ x -- y }> pi random ;

#( #( lambda: <{ -- val }> vct( 1.0 0.5 ) 0 2 #f #f #f insert-vct ;
      "lambda: <{ snd chn -- val }> vct( 1.000 0.500 ) 0 2 snd chn insert-vct drop ;"
      "insert-vct" )
   #( lambda: <{ -- val }> #f #f clm-channel-test ;
      "lambda: <{ snd chn -- val }>  snd chn clm-channel-test drop ;"
      "clm-channel-test" )
   ( examp.fs )
   #( lambda: <{ -- val }> 1000 3000 #f #f fft-edit ;
      "lambda: <{ snd chn -- val }> 1000 3000 snd chn fft-edit drop ;"
      "fft-edit" )
   #( lambda: <{ -- val }> 0.01 #f #f fft-squelch ;
      "lambda: <{ snd chn -- val }> 0.01 snd chn fft-squelch drop ;"
      "fft-sqelch" )
   #( lambda: <{ -- val }> 1000 3000 #f #f fft-cancel ;
      "lambda: <{ snd chn -- val }> 1000 3000 snd chn fft-cancel drop ;"
      "fft-cancel" )
   #( lambda: <{ -- val }> #f #f squelch-vowels ;
      "lambda: <{ snd chn -- val }>  snd chn squelch-vowels drop ;"
      "squelch-vowels" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) #f #f fft-env-edit ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) snd chn fft-env-edit drop ;"
      "fft-env-edit" )
   #( lambda: <{ -- val }>
        #( 0 0 1 1 2 0 ) #( 0 1 1 0 2 0 ) #( 0 0 1 1 ) #f #f fft-env-interp
      ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) #( 0 1 1 0 2 0 ) #( 0 0 1 1 ) snd chn fft-env-interp drop ;"
      "fft-env-interp" )
   #( lambda: <{ -- val }> 10 0.1 #f #f hello-dentist ;
      "lambda: <{ snd chn -- val }> 10 0.1 snd chn hello-dentist drop ;"
      "hello-dentist" )
   #( lambda: <{ -- val }> 1 0.3 20 #f #f fp ;
      "lambda: <{ snd chn -- val }> 1 0.3 20 snd chn fp drop ;"
      "fp" )
   #( lambda: <{ -- val }> #( 0 1 1 2 ) #f #f expsnd ;
      "lambda: <{ snd chn -- val }> #( 0 1 1 2 ) snd chn expsnd drop ;"
      "expsnd" )
   #( lambda: <{ -- val }> 1 256 2 2 #f #f voiced->unvoiced ;
      "lambda: <{ snd chn -- val }> 1 256 2 2 snd chn voiced->unvoiced drop ;"
      "voiced->unvoiced" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) 2 #f #f env-sound-interp ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) 2 snd chn env-sound-interp drop ;"
      "env-sound-interp" )
   #( lambda: <{ -- val }> #( #( "1a.snd" ) #( "pistol.snd" 1 2 ) ) #f #f add-notes ;
      "lambda: <{ snd chn -- val }> #( #( \"1a.snd\" ) #( \"pistol.snd\" 1 2 ) ) snd chn add-notes drop ;"
      "add-notes" )
   #( lambda: <{ -- val }> #( 0 0 1 1 2 0 ) #f #f filtered-env ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 2 0 ) snd chn filtered-env drop ;"
      "filtered-env" )
   #( lambda: <{ -- val }> 0.1 #f #f reverse-by-blocks ;
      "lambda: <{ snd chn -- val }> 0.1 snd chn reverse-by-blocks drop ;"
      "reverse-by-blocks" )
   #( lambda: <{ -- val }> 0.1 #f #f reverse-within-blocks ;
      "lambda: <{ snd chn -- val }> 0.1 snd chn reverse-within-blocks drop ;"
      "reverse-within-blocks" )
   ( extensions.fs )
   #( lambda: <{ -- val }> "1a.snd" 1200 #f #f #f #f mix-channel ;
      "lambda: <{ snd chn -- val }> \"1a.snd\" 1200 #f snd chn mix-channel drop ;"
      "mix-channel" )
   #( lambda: <{ -- val }> "1a.snd" 1200 #f #f #f #f insert-channel ;
      "lambda: <{ snd chn -- val }> \"1a.snd\" 1200 #f snd chn insert-channel drop ;"
      "insert-channel" )
   #( lambda: <{ -- val }> "1a.snd" 0.5 0.9 0 #f #f #f #f sine-ramp ;
      "lambda: <{ snd chn -- val }> 0.5 0.9 0 #f snd chn sine-ramp drop ;"
      "sine-ramp" )
   #( lambda: <{ -- val }> 
        #( 0 0 1 1 2 -0.5 3 1 ) 0 #f #f #f #f sine-env-channel
      ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 2 -0.5 3 1 ) 0 #f snd chn sine-env-channel drop ;"
      "sine-env-channel" )
   #( lambda: <{ -- val }> 0 1 0 #f #f #f #f blackman4-ramp ;
      "lambda: <{ snd chn -- val }> 0 1 0 #f snd chn blackman4-ramp drop ;"
      "blackman4-ramp" )
   #( lambda: <{ -- val }>
        #( 0 0 1 1 2 -0.5 3 1 ) 0 #f #f #f #f blackman4-env-channel 
      ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 2 -0.5 3 1 ) 0 #f snd chn blackman4-env-channel drop ;"
      "blackman4-env-channel" )
   #( lambda: <{ -- val }> 0.2 0.8 #t 0 #f #f #f #f ramp-squared ;
      "lambda: <{ snd chn -- val }> 0.2 0.8 #t 0 #f snd chn ramp-squared drop ;"
      "ramp-squared" )
   #( lambda: <{ -- val }> #( 0 0 1 1 ) #t 0 #f #f #f #f env-squared-channel ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 ) #t 0 #f snd chn env-squared-channel drop ;"
      "env-squared-channel" )
   #( lambda: <{ -- val }> 0.2 0.8 32 #t 0 #f #f #f #f ramp-expt ;
      "lambda: <{ snd chn -- val }> 0.2 0.8 32 #t 0 #f snd chn ramp-expt drop ;"
      "ramp-expt" )
   #( lambda: <{ -- val }> #( 0 0 1 1 ) 32 #t 0 #f #f #f #f env-expt-channel ;
      "lambda: <{ snd chn -- val }> #( 0 0 1 1 ) 32 #t 0 #f snd chn env-expt-channel drop ;"
      "env-expt-channel" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f #f offset-channel ;
      "lambda: <{ snd chn -- val }> 0.1 0 #f snd chn offset-channel drop ;"
      "offset-channel" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f #f dither-channel ;
      "lambda: <{ snd chn -- val }> 0.1 0 #f snd chn dither-channel drop ;"
      "dither-channel" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f #f contrast-channel ;
      "lambda: <{ snd chn -- val }> 0.1 0 #f snd chn contrast-channel drop ;"
      "contrast-channel" )
   ( dsp.fs )
   #( lambda: <{ -- val }> 550 600 10 40 50 0 #f #f #f #f ssb-bank ;
      "lambda: <{ snd chn -- val }> 550 600 10 40 50 0 #f snd chn ssb-bank drop ;"
      "ssb-bank" )
   #( lambda: <{ -- val }>
        550 600 #( 0 1 1 2 ) 10 40 50 0 #f #f #f #f ssb-bank-env
      ;
      "lambda: <{ snd chn -- val }> 550 600 #( 0 1 1 2 ) 10 40 50 0 #f snd chn ssb-bank-env drop ;"
      "ssb-bank-env" )
   #( lambda: <{ -- val }> 1 #f #f down-oct ;
      "lambda: <{ snd chn -- val }> 1 snd chn down-oct drop ;"
      "donw-oct" )
   #( lambda: <{ -- val }> 8 #f #f freqdiv ;
      "lambda: <{ snd chn -- val }> 8 snd chn freqdiv drop ;"
      "freqdiv" )
   #( lambda: <{ -- val }> 8 0 #f #f #f adsat ;
      "lambda: <{ snd chn -- val }> 8 0 #f snd chn adsat drop ;"
      "adsat" )
   #( lambda: <{ -- val }> #f #f spike ;
      "lambda: <{ snd chn -- val }>  snd chn spike drop ;"
      "spike" )
   #( lambda: <{ -- val }> #f #f zero-phase ;
      "lambda: <{ snd chn -- val }>  snd chn zero-phase drop ;"
      "zero-phase" )
   #( lambda: <{ -- val }> <'> random-pi-func #f #f rotate-phase ;
      "lambda: <{ snd chn -- val }> <'> random-pi-func snd chn rotate-phase drop ;"
      "rotate-phase" )
   #( lambda: <{ -- val }> 0.5 #f #f brighten-slightly ;
      "lambda: <{ snd chn -- val }> 0.5 snd chn brighten-slightly drop ;"
      "brighten-slightly" )
   #( lambda: <{ -- val }> 100 40 0 #f #f #f #f shift-channel-pitch ;
      "lambda: <{ snd chn -- val }> 100 40 0 #f snd chn shift-channel-pitch drop ;"
      "shift-channel-pitch" )
   #( lambda: <{ -- val }> vct( 0.0 0.5 ) #f #f channel-polynomial ;
      "lambda: <{ snd chn -- val }> vct( 0.000 0.500 ) snd chn channel-polynomial drop ;"
      "channel-polynomial" )
   #( lambda: <{ -- val }> vct( 0.0 1.0 ) #f #f spectral-polynomial ;
      "lambda: <{ snd chn -- val }> vct( 0.000 1.000 ) snd chn spectral-polynomial drop ;"
      "spectral-polynomial" )
   #( lambda: <{ -- val }>
        #( 60.0 120.0 240.0 ) #f 0 #f #f #f #f #t 2 notch-channel
      ;
      "lambda: <{ snd chn -- val }> #( 60.0 120.0 240.0 ) #f 0 #f snd chn notch-channel drop ;"
      "notch-channel" )
   ( effects.fs )
   #( lambda: <{ -- val }> 0.1 128 effects-squelch-channel ;
      "lambda: <{ snd chn -- val }> 0.1 128 snd chn effects-squelch-channel drop ;"
      "effects-sqelch-channel" )
   #( lambda: <{ -- val }> #f 0.5 0.1 0 #f #f #f effects-echo ;
      "lambda: <{ snd chn -- val }> #f 0.5 0.1 0 #f snd chn effects-echo drop ;"
      "effects-echo" )
   #( lambda: <{ -- val }> 0.5 0.1 #f 0 #f #f #f effects-flecho ;
      "lambda: <{ snd chn -- val }> 0.5 0.1 #f 0 #f snd chn effects-flecho drop ;"
      "effects-flecho" )
   #( lambda: <{ -- val }> 0.75 0.75 6.0 10.0 #f 0 #f #f #f effects-zecho ;
      "lambda: <{ snd chn -- val }> 0.75 0.75 6.0 10.0 #f 0 #f snd chn effects-zecho drop ;"
      "effects-zecho" )
   #( lambda: <{ -- val }> 0.1 50 0 #f #f #f effects-comb-filter ;
      "lambda: <{ snd chn -- val }> 0.1 50 0 #f snd chn effects-comb-filter drop ;"
      "effects-comb-filter" )
   #( lambda: <{ -- val }> 10000 0.5 0 #f #f #f effects-moog ;
      "lambda: <{ snd chn -- val }> 10000 0.5 0 #f snd chn effects-moog drop ;"
      "effects-moog" )
   #( lambda: <{ -- val }> #f #f effects-remove-dc ;
      "lambda: <{ snd chn -- val }>  snd chn effects-remove-dc drop ;"
      "effects-remove-dc" )
   #( lambda: <{ -- val }> #f #f effects-compand ;
      "lambda: <{ snd chn -- val }>  snd chn effects-compand drop ;"
      "effects-compand" )
   #( lambda: <{ -- val }> 100.0 #f 0 #f #f #f effects-am ;
      "lambda: <{ snd chn -- val }> 100.0 #f 0 #f snd chn effects-am drop ;"
      "effects-am" )
   #( lambda: <{ -- val }> 100.0 #f 0 #f #f #f effects-rm ;
      "lambda: <{ snd chn -- val }> 100.0 #f 0 #f snd chn effects-rm drop ;"
      "effects-rm" )
   #( lambda: <{ -- val }> 1000.0 100.0 0 #f #f #f effects-bbp ;
      "lambda: <{ snd chn -- val }> 1000.0 100.0 0 #f snd chn effects-bbp drop ;"
      "effects-bbp" )
   #( lambda: <{ -- val }> 1000.0 100.0 0 #f #f #f effects-bbr ;
      "lambda: <{ snd chn -- val }> 1000.0 100.0 0 #f snd chn effects-bbr drop ;"
      "effects-bbr" )
   #( lambda: <{ -- val }> 1000.0 0 #f #f #f effects-bhp ;
      "lambda: <{ snd chn -- val }> 1000.0 0 #f snd chn effects-bhp drop ;"
      "effects-bhp" )
   #( lambda: <{ -- val }> 1000.0 0 #f #f #f effects-blp ;
      "lambda: <{ snd chn -- val }> 1000.0 0 #f snd chn effects-blp drop ;"
      "effects-blp" )
   #( lambda: <{ -- val }> 50.0 0.5 0 #f #f #f effects-hello-dentist ;
      "lambda: <{ snd chn -- val }> 50.0 0.5 0 #f snd chn effects-hello-dentist drop ;"
      "effects-hello-dentist" )
   #( lambda: <{ -- val }> 1.0 0.3 20.0 0 #f #f #f effects-fp ;
      "lambda: <{ snd chn -- val }> 1.0 0.3 20.0 0 #f snd chn effects-fp drop ;"
      "effects-fp" )
   #( lambda: <{ -- val }> 5.0 2.0 0.001 0 #f #f #f effects-flange ;
      "lambda: <{ snd chn -- val }> 5.0 2.0 0.001 0 #f snd chn effects-flange drop ;"
      "effects-flange" )
   #( lambda: <{ -- val }> 0.1 0 #f #f #f effects-jc-reverb-1 ;
      "lambda: <{ snd chn -- val }> 0.1 0 #f snd chn effects-jc-reverb-1 drop ;"
      "effects-jc-reverb-1" ) ) value test19-*.fs

: 19-save/restore ( -- )
  "oboe.snd" open-sound { ind }
  nil nil nil nil nil nil { vals func1 descr name func str }
  test19-*.fs each to vals
    vals 0 array-ref to func1
    vals 1 array-ref to descr
    vals 2 array-ref to name
    *snd-test-verbose* if
      name #f snd-test-message
    then
    func1 #() run-proc drop
    ind #f undef undef edit-list->function to func
    func proc-source-ref to str
    str descr "edit-list->function %s [%d]" #( name i ) snd-test-neq
    ind revert-sound drop
    func #( ind 0 ) run-proc drop
    ind revert-sound drop
  end-each
  ind close-sound drop
;

\ ---------------- test 23: with-sound ----------------

: test23-notehook { ins start dur -- }
  "%7.3f %0.3f %s" #( start dur ins ) snd-test-message
;

: test23-balance ( -- )
  make-rmsgain    { rg }
  40 make-rmsgain { rg1 }
  2  make-rmsgain { rg2 }
  #( 0 0 1 1 2 0 )      :length base-length make-env { e }
  #( 0 0 1 1 )          :length base-length make-env { e1 }
  #( 0 0 1 1 2 0 10 0 ) :length base-length make-env { e2 }
  440.0 make-oscil { o }
  *output* sound? if
    *output* channels make-array map!
      base-length 0.0 make-vct
    end-map
  else
    *output*
  then { gen }
  nil { sig }
  base-length 0 do
    e env to sig
    i  rg  sig                    e2 env rmsgain-balance  gen ws-outa
    i  rg1 sig                    e1 env rmsgain-balance  gen ws-outb
    i  rg2 o 0.0 0.0 oscil 0.1 f* e2 env rmsgain-balance  gen ws-outc
  loop
  gen array? if
    gen each ( v )
      0 *output* i #f undef mix-vct drop
    end-each
  then
  *clm-srate* 22050 = if
    0.98402
  else
    1.4227 ( 44100 )
  then { req }
  rg rmsgain-gain-avg { res }
  res req "rmsgain gain-avg" #() snd-test-neq
  base-length to req
  rg2 rmsgain-avgc to res
  res req "rmsgain count" #() snd-test-neq
;

: test23-ssb-fm ( gen mg -- proc; y self -- val )
  1 proc-create { prc } ( mg ) , ( gen ) , prc
 does> { y self -- val }
  self       @ { mg }
  self cell+ @ { gen }
  gen   mg 0.0 0.0 oscil 0.02 f*  ssb-fm
;

\ CLM examples (see clm.html) and their Snd/Forth counterparts:

\ (with-sound () 
\   (mix (with-sound (:output "hiho.snd") 
\          (fm-violin 0 1 440 .1))
\        :amplitude .5))

: test23-clm-examp-1 ( -- )
  0.0 1.0 440 0.1 <'> fm-violin
  :output "hiho.snd" with-sound ws-output { fname }
  fname :scaler 2.0 clm-mix
  fname ws-close-snd
;

\ (with-sound ()
\   (with-mix () "s1" 0
\     (sound-let ((tmp () (fm-violin 0 1 440 .1)))
\       (mix tmp))))

: test23-clm-examp-2 ( -- )
  "
  '( '( '() 0.0 1.0 440 0.1 <'> fm-violin ) )
  lambda: <{ tmp -- }>
    tmp clm-mix
  ; sound-let
  " '() "s1" 0 with-mix
;

\ (with-sound (:verbose t)
\   (with-mix () "s6" 0
\     (sound-let ((tmp () (fm-violin 0 1 440 .1))
\                 (tmp1 (:reverb nrev) (mix "oboe.snd")))
\       (mix tmp1)
\       (mix tmp :amplitude .2 :output-frame *srate*))
\     (fm-violin .5 .1 330 .1)))

: test23-clm-examp-3 ( -- )
  "
  '( '( '() 0.0 1.0 440 0.1 <'> fm-violin )
     '( '( :reverb <'> nrev ) \"oboe.snd\" <'> clm-mix ) )
  lambda: <{ tmp tmp1 -- }>
    tmp1 clm-mix
    tmp :scaler 5.0 :output-frame 1.0 seconds->samples clm-mix
  ; sound-let
  0.5 0.1 330 0.1 fm-violin
  " '() "s6" 0 with-mix
;

\ (with-sound (:verbose t)
\   (sound-let ((tmp () (with-mix () "s7" 0
\                    (sound-let ((tmp () (fm-violin 0 1 440 .1))
\                                (tmp1 () (mix "oboe.snd")))
\                      (mix tmp1)
\                      (mix tmp :output-frame *srate*))
\                    (fm-violin .5 .1 330 .1))))
\      (mix tmp :amplitude .5)))

: test23-clm-4-cb <{ tmp -- }>
  tmp :scaler 2.0 clm-mix
;

: test23-clm-examp-4 ( -- )
  '( '( '() "
    '( '( '() 0.0 1.0 440 0.1 <'> fm-violin )
        '( '()      \"oboe.snd\" <'> clm-mix ) )
    lambda: <{ tmp tmp1 -- }>
      tmp1 clm-mix
      tmp :output-frame 1.0 seconds->samples clm-mix
    ; sound-let
    0.5 0.1 330 0.1 fm-violin
    " '() "s7" 0 <'> with-mix ) )
  <'> test23-clm-4-cb sound-let
;

: test23-sl-cb <{ tmp1 tmp2 tmp3 -- }>
  *snd-test-ws-verbose* if
    tmp1 . cr
    tmp2 . cr
    tmp3 . cr
  then
  tmp1 clm-mix
  tmp2 clm-mix
;

: test23-sound-let ( -- )
  '( '( '( :reverb <'> jc-reverb ) 0.0 1.0 220 0.2 <'> fm-violin )
     '( '()                        0.5 1 440 0.3 <'> fm-violin )
     '( '()                        '( 10 'a ) ) )
  <'> test23-sl-cb sound-let
;

: test23-with-mix ( -- )
  0.0 0.1 440 0.1 fm-violin
  "
  0.0 0.2 550 0.1 fm-violin
  0.1 0.1 660 0.1 fm-violin
  " #() "sec1" 0.5 with-mix
  "
  0.0 0.1  880 0.1 :reverb-amount 0.2 fm-violin
  0.1 0.1 1320 0.1 :reverb-amount 0.2 fm-violin
  " #( :reverb <'> jc-reverb ) "sec2" 1.0 with-mix
  2.0 0.1 220 0.1 fm-violin
;

\ examples from sndclm.html
: sndclm-oscil-test ( -- )
  440.0 make-oscil { gen }
  0 1 nil run-instrument
    gen 0 0 oscil  f2/
  end-run
;

: sndclm-env-test ( -- )
  440.0 make-oscil { gen }
  '( 0 0 0.01 1 0.25 0.1 0.5 0.01 1 0 )
  :scaler 0.5 :length 44100 make-env { ampf }
  0 1 nil run-instrument
    gen 0 0 oscil  ampf env  f*
  end-run
;

: sndclm-table-lookup-test ( -- )
  440.0 :wave '( 1 0.5  2 0.5 ) #f #f partials->wave make-table-lookup { gen }
  0 1 nil run-instrument
    gen 0 table-lookup  f2/
  end-run
;

: sndclm-polywave-test ( -- )
  440.0 :partials '( 1 0.5 2 0.5 ) make-polywave { gen }
  0 1 nil run-instrument
    gen 0 polywave  f2/
  end-run
;

: sndclm-triangle-wave-test ( -- )
  440.0 make-triangle-wave { gen }
  0 1 nil run-instrument
    gen 0 triangle-wave  f2/
  end-run
;

: sndclm-ncos-test ( -- )
  440.0 10 make-ncos { gen }
  0 1 nil run-instrument
    gen 0 ncos  f2/
  end-run
;

: sndclm-nrxycos-test ( -- )
  440.0 :n 10 make-nrxycos { gen }
  0 1 nil run-instrument
    gen 0 nrxycos  f2/
  end-run
;

: sndclm-ssb-am-test ( -- )
  440.0 20 make-ssb-am { shifter }
  440.0 make-oscil { osc }
  0 1 nil run-instrument
    shifter  osc 0 0 oscil  0 ssb-am f2/
  end-run
;

: sndclm-wave-train-test ( -- )
  400 10 make-ncos { g }
  g -0.5 pi f* set-mus-phase drop
  64 make-vct map!
    g 0 ncos
  end-map { v }
  440.0 :wave v make-wave-train { gen }
  0 1 nil run-instrument
    gen 0 wave-train  f2/
  end-run
;

: sndclm-rand-test ( -- )
  5.0 220.0 hz->radians make-rand { ran1 }
  5.0 330.0 hz->radians make-rand-interp { ran2 }
   440.0 make-oscil { osc1 }
  1320.0 make-oscil { osc2 }
  2.0 seconds->samples { len }
  *output* sound? if
    2 make-array map!
      len 0.0 make-vct
    end-map
  else
    *output*
  then { gen }
  len 0 do
    i  osc1  ran1 0 rand         0 oscil  f2/ gen ws-outa
    i  osc2  ran2 0 rand-interp  0 oscil  f2/ gen ws-outb
  loop
  gen array? if
    gen each ( v )
      0 *output* i #f undef mix-vct drop
    end-each
  then
;

: sndclm-two-pole-test ( -- )
  1000.0 0.999 make-two-pole { flt }
  10000.0 0.002 make-rand { ran1 }
  0 1 nil run-instrument
    flt  ran1 0 rand  two-pole  f2/
  end-run
;

: sndclm-firmant-test ( -- )
  1000.0 0.999 make-firmant { flt }
  10000.0 5.0 make-rand { ran1 }
  0 1 nil run-instrument
    flt  ran1 0 rand  #f firmant  f2/
  end-run
;

: sndclm-iir-filter-test ( -- )
  3 vct( 0.0 -1.978 0.998 ) make-iir-filter { flt }
  10000.0 0.002 make-rand { ran1 }
  0 1 nil run-instrument
    flt  ran1 0 rand  iir-filter  f2/
  end-run
;

: sndclm-delay-test ( -- )
  0.5 seconds->samples make-delay { dly }
  440.0 make-oscil { osc1 }
  660.0 make-oscil { osc2 }
  0 1 nil run-instrument
    osc1 0 0 oscil
    dly  osc2 0 0 oscil  0 delay  f+
    f2/
  end-run
;

: sndclm-comb-test ( -- )
  0.4 0.4 seconds->samples make-comb { cmb }
  440.0 make-oscil { osc }
  '( 0 0 1 1 2 1 3 0 ) :length 4410 make-env { ampf }
  0 2 nil run-instrument
    cmb ( gen )
    ampf env  osc 0 0 oscil  f* ( val )
    0 ( pm )  comb f2/
  end-run
;

: sndclm-all-pass-test ( -- )
  -0.4 0.4 0.4 seconds->samples make-all-pass { alp }
  440.0 make-oscil { osc }
  '( 0 0 1 1 2 1 3 0 ) :length 4410 make-env { ampf }
  0 2 nil run-instrument
    alp ( gen )
    ampf env  osc 0 0 oscil  f* ( val )
    0 ( pm )
    all-pass f2/
  end-run
;

: sndclm-moving-average-test ( -- )
  4410 make-moving-average { avg }
  440.0 make-oscil { osc }
  1.0 0.1 f- { stop }
  0.0 { val }
  0 stop nil run-instrument
    osc 0 0 oscil to val
    avg val fabs moving-average  val f*
  end-run
  stop 1.0 nil run-instrument
    avg 0.0 moving-average  osc 0 0 oscil f*
  end-run
;

: sndclm-src1-test ( -- )
  "oboe.snd" make-readin { rd }
  rd 0.5 make-src { sr }
  "oboe.snd" mus-sound-duration f2* { dur }
  0 dur nil run-instrument
    sr 0 #f src
  end-run
;

: make-src-proc { osc -- prc; dir self -- val }
  1 proc-create osc , ( prc )
 does> { dir self -- val }
  self @ ( osc ) 0 0 oscil
;

: sndclm-src2-test ( -- )
  440.0 make-oscil { osc }
  osc make-src-proc { prc }
  :srate 2.0 make-src { sr }
  0 1 nil run-instrument
    sr 0 prc src
  end-run
;

: sndclm-convolve1-test ( -- )
  "pistol.snd" make-readin ( rd )
  "oboe.snd" file->vct ( v ) make-convolve { cnv }
  0 2 nil run-instrument
    cnv #f convolve  0.25 f*
  end-run
;

: sndclm-convolve2-test ( -- )
  "oboe.snd" "pistol.snd" 0.5 "convolved.snd" convolve-files { tempfile }
  tempfile make-readin { reader }
  tempfile mus-sound-duration { dur }
  0 dur nil run-instrument
    reader readin
  end-run
  tempfile file-delete
;

: sndclm-granulate1-test ( -- )
  "oboe.snd" make-readin 2.0 make-granulate { grn }
  0 1 nil run-instrument
    grn #f #f granulate
  end-run
;

: make-granulate-proc { osc sweep -- prc; dir self -- val }
  1 proc-create osc , sweep , ( prc )
 does> { dir self -- val }
  self @ ( osc )  self cell+ @ ( sweep ) env  0 oscil  0.2 f*
;

: sndclm-granulate2-test ( -- )
  440.0 make-oscil { osc }
  '( 0 0 1 1 ) :scaler 440.0 hz->radians :length 44100 make-env { sweep }
  osc sweep make-granulate-proc :expansion 2.0 :length 0.5
    make-granulate { grn }
  0 2 nil run-instrument
    grn #f #f granulate
  end-run
;

: sndclm-phase-vocoder1-test ( -- )
  "oboe.snd" make-readin :pitch 2.0 make-phase-vocoder { pv }
  0 1 nil run-instrument
    pv #f #f #f #f phase-vocoder
  end-run
;

: sndclm-phase-vocoder2-test ( -- )
  "oboe.snd" make-readin :interp 256 make-phase-vocoder { pv }
  "oboe.snd" mus-sound-duration f2* { dur }
  0 dur nil run-instrument
    pv #f #f #f #f phase-vocoder
  end-run
;

: sndclm-asymmetric-fm-test ( -- )
  440.0 0.0 0.9 0.5 make-asymmetric-fm { fm }
  0 1 nil run-instrument
    fm 1.0 0 asymmetric-fm  f2/
  end-run
;

: sndclm-file->frample->file-test ( -- )
  "stereo.snd" make-file->frample { input }
  2 0.0 make-vct { frm }
  "stereo.snd" mus-sound-framples { len }
  *output* channels { chans }
  *output* sound? if
    *output* short-file-name { fname }
    "frample-" fname $+ to fname
    fname chans undef undef undef make-frample->file
  else
    *output*
  then { gen }
  0.0 0.0 { val0 val1 }
  len 0 ?do
    input i frm file->frample to frm
    frm 0 vct-ref to val0
    frm 1 vct-ref to val1
    frm 0 val1 vct-set! drop
    frm 1 val0 vct-set! drop
    gen i frm frample->file drop
  loop
  *output* sound? if
    gen mus-close drop
    chans 0 do
      0 len fname *output* i #f undef i #f #f set-samples drop
    loop
    fname file-delete
  then
;

: sndclm-readin-test ( -- )
  "oboe.snd" make-readin { reader }
  0 1 nil run-instrument
    reader readin  f2/
  end-run
;

: sndclm-in-out-any-test ( -- )
  "oboe.snd" make-file->sample { infile }
  0 1 nil run-instrument
    i 0 infile in-any
  end-run
;

: sndclm-locsig-test ( -- )
  44100 { len }
  *output* channels { chans }
  *output* sound? if
    *output* short-file-name { fname }
    "frample-" fname $+ to fname
    fname chans undef undef undef make-frample->file
  else
    *output*
  then { gen }
  60.0 :output gen make-locsig { loc }
  440.0 make-oscil { osc }
  len 0 do
    loc i  osc 0 0 oscil f2/  locsig drop
  loop
  *output* sound? if
    gen mus-close drop
    chans 0 do
      0 len fname *output* i #f undef i #f #f set-samples drop
    loop
    fname file-delete
  then
;

: sndclm-amplitude-modulate-test ( -- )
  440.0 make-oscil { osc1 }
  220.0 make-oscil { osc2 }
  0 1 nil run-instrument
    0.3            ( car )
    osc1 0 0 oscil ( in1 )
    osc2 0 0 oscil ( in2 ) amplitude-modulate  f2/
  end-run
;

: ws-test-close-sound { ws -- }
  ws ws-output 0 find-sound { ind }
  ind sound? if
    ind close-sound drop
  then
;

: check-maxamp { ind req name lno -- }
  ind 0 #f maxamp { res }
  res req <'> f> "%s (%s)[%s]: maxamp"
    #( name ind short-file-name lno ) snd-test-any-eq
;

: (ws-test-close-sound-check) { ws req lno -- }
  ws ws-output 0 find-sound { ind }
  ind sound? if
    ws :statistics ws-ref unless
      ind req ws :comment ws-ref lno check-maxamp
    then
    ind close-sound drop
  then
;

: ws-test-close-sound-check ( ws req -- )
  postpone *lineno*
  postpone (ws-test-close-sound-check)
; immediate

: (23-with-sound) ( -- )
  1024 1024 * to *clm-file-buffer-size*
  *clm-play*               { old-play }
  *clm-player*             { old-player }
  *clm-statistics*         { old-stats }
  *snd-test-ws-play*       to *clm-play*
  *snd-test-ws-player*     to *clm-player*
  *snd-test-ws-statistics* to *clm-statistics*
  \ from clm.fs
  <'> test23-clm-examp-1
    :comment over object->string with-sound ws-test-close-sound
  <'> test23-clm-examp-2
    :comment over object->string with-sound ws-test-close-sound
  <'> test23-clm-examp-3
    :comment over object->string with-sound ws-test-close-sound
  <'> test23-clm-examp-4
    :comment over object->string with-sound ws-test-close-sound
  <'> test23-sound-let
    :comment over object->string with-sound ws-test-close-sound
  <'> test23-with-mix
    :comment over object->string with-sound ws-test-close-sound
  <'> run-test
    :comment over object->string with-sound ws-test-close-sound
  <'> src-test
    :comment over object->string with-sound ws-test-close-sound
  <'> conv1-test
    :comment over object->string with-sound ws-test-close-sound
  <'> conv2-test
    :comment over object->string with-sound ws-test-close-sound
  <'> inst-test
    :comment over object->string with-sound ws-test-close-sound
  <'> arpeggio-test
    :comment over object->string with-sound ws-test-close-sound
  \ from bird.fsm
  <'> bird-test
  :comment over object->string
  :verbose *snd-test-ws-verbose*
  :channels 2 with-sound ws-test-close-sound
  \ from clm-ins.fs
  0.0 0.3 <'> clm-ins-test
  :comment  over object->string
  :notehook *snd-test-ws-verbose* if
    <'> test23-notehook
  else
    #f
  then
  :channels 2 with-sound 1.2 ws-test-close-sound-check
  <'> test23-balance
  :comment  over object->string
  :srate    22050
  :channels 3 with-sound ws-output 0 find-sound { ind }
  ind sound? if
    ind close-sound drop
  else
    "with-sound balance?" snd-display
  then
  "tmp.snd" 1 22050 mus-bfloat mus-next new-sound to ind
  0 1000 ind 0 pad-channel drop
  100.0 make-oscil { mg }
  1000 make-ssb-fm { gen }
  gen mg test23-ssb-fm <'> map-channel snd-test-catch drop
  ind close-sound drop
  \ examples from sndclm.html
  <'> sndclm-oscil-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-env-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-table-lookup-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-polywave-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-triangle-wave-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-ncos-test 
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-nrxycos-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-ssb-am-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-wave-train-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-rand-test
    :comment over object->string :channels 2 with-sound ws-test-close-sound
  <'> sndclm-two-pole-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-firmant-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-iir-filter-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-delay-test 
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-comb-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-all-pass-test 
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-moving-average-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-src1-test
    :comment over object->string :srate 22050 with-sound ws-test-close-sound
  <'> sndclm-src2-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-convolve1-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-convolve2-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-granulate1-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-granulate2-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-phase-vocoder1-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-phase-vocoder2-test
    :comment over object->string :srate 22050 with-sound ws-test-close-sound
  <'> sndclm-asymmetric-fm-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-file->frample->file-test
   :comment over object->string :channels 2 with-sound ws-test-close-sound
  <'> sndclm-readin-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-in-out-any-test
    :comment over object->string with-sound ws-test-close-sound
  <'> sndclm-locsig-test
   :comment over object->string :channels 2 with-sound ws-test-close-sound
  <'> sndclm-amplitude-modulate-test
    :comment over object->string with-sound ws-test-close-sound
  old-play   to *clm-play*
  old-player to *clm-player*
  old-stats  to *clm-statistics*
;

: 23-with-sound ( -- )
  #f to *clm-to-snd*
  (23-with-sound)
  #t to *clm-to-snd*
  (23-with-sound)
;

\ ---------------- test 27: general ----------------

*with-test-complex* [if]
  : complex-test ( -- )
    \ edot-product (test008)
    0.0 vct( 1.0 ) edot-product dup 1.0 fneq if
      "edot 1.0: %s?" swap snd-display
    else
      drop
    then
    0.0 vct( 0.0 ) edot-product dup 0.0 fneq if
      "edot 0.0: %s?" swap snd-display
    else
      drop
    then
    0.0 #( 1.0 ) edot-product dup 1.0 fneq if
      "edot 1.0: %s?" swap snd-display
    else
      drop
    then
    0.0 #( 0+1i ) edot-product dup 0+1i cneq if
      "edot i: %s?" swap snd-display
    else
      drop
    then
    0.25 two-pi f* vct( 1.0 1.0 1.0 1.0 ) edot-product
    0.00 two-pi f* fexp
    0.25 two-pi f* fexp f+
    0.50 two-pi f* fexp f+
    0.75 two-pi f* fexp f+ over over fneq if
      2 >array "edot 4: %s %s?" swap snd-display
    else
      2drop
    then
    0.25 two-pi f* 0-1i c* #( 1.0 2.0 3.0 4.0 ) edot-product
    0.00 two-pi f* 0-1i c* cexp 1 c* 
    0.25 two-pi f* 0-1i c* cexp 2 c* c+
    0.50 two-pi f* 0-1i c* cexp 3 c* c+
    0.75 two-pi f* 0-1i c* cexp 4 c* c+ over over cneq if
      2 >array "edot 4 -i: %s %s?" swap snd-display
    else
      2drop
    then
    0.25 two-pi f* 0-1i c* #( 1+1i 2+1i 3+1i 4+1i ) edot-product
    0.00 two-pi f* 0-1i c* cexp 1+1i c* 
    0.25 two-pi f* 0-1i c* cexp 2+1i c* c+
    0.50 two-pi f* 0-1i c* cexp 3+1i c* c+
    0.75 two-pi f* 0-1i c* cexp 4+1i c* c+ over over cneq if
      2 >array "edot 4 -i * i: %s %s?" swap snd-display
    else
      2drop
    then
  ;
[else]
  <'> noop alias complex-test
[then]

: print-and-check ( gen name desc -- )
  { gen name desc }
  gen mus-name name string<> if
    "mus-name %s: %s?" #( name gen mus-name ) snd-display
  then
  gen mus-describe desc string<> if
    "mus-describe %s: %s?" #( name gen ) snd-display
  then
  gen { egen }
  gen egen object-equal? unless
    "equal? %s: %s %s?" #( name gen egen ) snd-display
  then
;

: test-gen-equal ( g0 g1 g2 -- )
  { g0 g1 g2 }
  \ g0 g1 =
  \ g0 g2 <> at start
  g0 { g3 }
  g0 g3 object-equal? unless
    "let %s: %s equal? %s?" #( g0 mus-name g0 g3 ) snd-display 
  then
  g0 g1 object-equal? unless
    "%s: %s equal? %s?" #( g0 mus-name g0 g1 ) snd-display
  then
  g0 g2 object-equal? if
    "%s: %s equal? %s?" #( g0 mus-name g0 g2 ) snd-display
  then
  g0 0.0 0.0 mus-apply drop
  g3 #( 0.0 0.0 ) object-apply drop
  g3 0.0 0.0 mus-apply drop
  g0 g3 object-equal? unless
    "run let %s: %s equal? %s?" #( g0 mus-name g0 g3 ) snd-display
  then
  g0 g1 object-equal? if
    "run %s: %s equal? %s?" #( g0 mus-name g0 g1 ) snd-display
  then
  g0 g2 object-equal? if
    "run %s: %s equal? %s?" #( g0 mus-name g0 g2 ) snd-display
  then
;

\ bind-key proc
: C-xC-c <{ -- }> 0 snd-exit ;

\ hooks
: my-test1-proc <{ fname -- f }> #f ;
<'> my-test1-proc alias my-test2-proc
<'> my-test1-proc alias my-test3-proc
<'> my-test1-proc alias my-test4-proc
<'> my-test1-proc alias my-test5-proc

: my-local-thunk <{ -- }>
  open-hook object-length 3 <> if
    "add-hook! local length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook "my-test3-proc" hook-member? unless
    "local3 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook "my-test4-proc" hook-member? unless
    "local4 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook "my-test5-proc" hook-member? unless
    "local5 add-hook!: %s" #( open-hook ) snd-display
  then
;

#f value my-se
#f value my-sw
#f value my-me

: my-error-prc <{ msg -- f }>
  #t to my-se
  #t
;

: my-warning-prc <{ msg -- f }>
  #t to my-sw
  #t
;

: my-mus-error-prc <{ tp msg -- f }>
  #t to my-me
  #t
;

: my-error-warning-hooks ( -- )
  snd-error-hook   reset-hook!
  snd-warning-hook reset-hook!
  mus-error-hook   reset-hook!
  snd-error-hook   <'> my-error-prc     add-hook!
  snd-warning-hook <'> my-warning-prc   add-hook!
  mus-error-hook   <'> my-mus-error-prc add-hook!
  "uhoh" snd-error drop
  "hiho" snd-warning drop
  "/bad/baddy" mus-sound-samples drop
  my-se #t "snd-error-hook not called?"   #f snd-test-neq
  my-sw #t "snd-warning-hook not called?" #f snd-test-neq
  my-me #t "mus-error-hook not called?"   #f snd-test-neq
  snd-error-hook   reset-hook!
  snd-warning-hook reset-hook!
  mus-error-hook   reset-hook!
;

: 27-sel-from-snd ( -- )
  \ play etc (from test 5)
  "oboe.snd" open-sound { ind }
  ind x-bounds { bnds }
  x-position-slider { xp }
  y-position-slider { yp }
  x-zoom-slider { xz }
  y-zoom-slider { yz }
  " open-so" snd-completion " open-sound" "completion (1)" #() snd-test-neq
  " zoom-focus-r" snd-completion " zoom-focus-right" "completion (2)"
    #() snd-test-neq
  "oboe.snd" :wait #t play drop
  "oboe.snd" :start 12000 :wait #t play drop
  "oboe.snd" :start 1200 :end 15000 :wait #t play drop
  ind :edit-position #f #f edit-position 1- :wait #t play drop
  ind close-sound drop
  \ hooks
  open-hook reset-hook!
  open-hook <'> my-test1-proc add-hook!
  open-hook <'> my-test2-proc add-hook!
  open-hook object-length 2 <> if
    "add-hook! global length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook "my-test1-proc" hook-member? unless
    "global1 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook <'> my-test2-proc hook-member? unless
    "global2 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook
  #( <'> my-test3-proc
     <'> my-test4-proc
     <'> my-test5-proc ) <'> my-local-thunk with-local-hook
  open-hook object-length 2 <> if
    "add-hook! reset length: %d?" #( open-hook object-length ) snd-display
  then
  open-hook <'> my-test1-proc hook-member? unless
    "reset1 add-hook!: %s" #( open-hook ) snd-display
  then
  open-hook "my-test2-proc" hook-member? unless
    "reset2 add-hook!: %s" #( open-hook ) snd-display
  then
  my-error-warning-hooks
  \ bind-key
  <'> C-xC-c { prc }
  "c" 4 #t key-binding { old-prc }
  "c" 4 prc #t bind-key { prc1 }
  "c" 4 #t key-binding { prc2 }
  prc prc1 <> if
    "bind-key: %s %s?" #( prc prc1 ) snd-display
  then
  prc prc2 <> if
    "key-binding: %s %s?" #( prc prc2 ) snd-display
  then
  old-prc proc? if
    "c" 4 old-prc #t bind-key drop
  else
    "c" 4 #t unbind-key drop
  then
  "fmv.snd" 1 22050 mus-bshort mus-next "set-samples test" 100 new-sound to ind
  \ new-sound
  10  3  3 0.1 make-vct set-samples drop
  0 20 ind 0 channel->vct { res }
  res vct( 0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1 0 0 0 0 0 0 0 ) vequal? unless
    "1 set samples 0 for 0.1: %s?" #( res ) snd-display
  then
  ind close-sound drop
  \ x-axis-label (test005)
  *with-test-gui* if
    "oboe.snd" open-sound to ind
    #t set-transform-graph? drop
    #t set-time-graph? drop
    x-axis-label to res
    res "time" string<> if
      "get time x-axis-label: %s?" #( res ) snd-display
    then
    "hiho1" ind 0 time-graph set-x-axis-label drop
    x-axis-label to res
    res "hiho1" string<> if
      "set time x-axis-label: %s?" #( res ) snd-display
    then
    update-transform-graph drop
    ind 0 transform-graph x-axis-label to res
    res "frequency" string<> if
      "get fft x-axis-label: %s?" #( res ) snd-display
    then
    "hiho2" ind 0 transform-graph set-x-axis-label drop
    update-transform-graph drop
    ind 0 transform-graph x-axis-label to res
    res "hiho2" string<> if
      "set fft x-axis-label: %s?" #( res ) snd-display
    then
    "frequency" ind 0 transform-graph set-x-axis-label drop
    '( 0 0 1 1 2 0 ) "lisp" graph drop
    update-lisp-graph drop
    ind 0 lisp-graph x-axis-label to res
    res "lisp" string<> if
      "get lisp x-axis-label: %s?" #( res ) snd-display
    then
    "hiho3" ind 0 lisp-graph set-x-axis-label drop
    ind 0 lisp-graph x-axis-label to res
    res "hiho3" string<> if
      "set lisp x-axis-label: %s?" #( res ) snd-display
    then
    "hiho4" ind 0 time-graph set-y-axis-label drop
    y-axis-label to res
    res "hiho4" string<> if
      "set time y-axis-label: %s?" #( res ) snd-display
    then
    "hiho5" ind 0 lisp-graph set-y-axis-label drop
    ind 0 lisp-graph y-axis-label to res
    res "hiho5" string<> if
      "set lisp y-axis-label: %s?" #( res ) snd-display
    then
    #f set-y-axis-label drop
    "hiho6" ind 0 set-y-axis-label drop
    ind 0 y-axis-label to res
    res "hiho6" string<> if
      "set time y-axis-label (time): %s?" #( res ) snd-display
    then
    #f set-y-axis-label drop
    ind close-sound drop
  then                          \ *with-test-gui*
  \ edot-product (test008)
  complex-test
  \ delay (test008)
  3 make-delay { gen }
  3 make-delay { gen2 }
  4 :initial-contents #( 1.0 0.5 0.25 0.0 ) make-delay { gen1 }
  4 :initial-contents vct( 1.0 0.5 0.25 0.0 ) make-delay { gen3 }
  gen "delay" "delay line[3, step]: [0 0 0]" print-and-check
  10 0.0 make-vct map
    gen i 0.0 delay
  end-map { v0 }
  10 0.0 make-vct map
    gen2 delay? if
      gen2 i 0.0 delay
    else
      -1.0
    then
  end-map { v1 }
  v0 v1 vequal? unless
    "map delay: %s %s?" #( v0 v1 ) snd-display
  then
  gen delay? unless
    "%s not a delay?" #( gen ) snd-display
  then
  gen mus-length 3 <> if
    "delay length: %d?" #( gen mus-length ) snd-display
  then
  v0 1 vct-ref 0.0 fneq
  v0 4 vct-ref 1.0 fneq ||
  v0 8 vct-ref 5.0 fneq || if
    "delay output: %s?" #( v0 ) snd-display 
  then
  gen1 0.0 0.0 delay 1.0  fneq
  gen1 0.0 0.0 delay 0.5  fneq ||
  gen1 0.0 0.0 delay 0.25 fneq ||
  gen1 0.0 0.0 delay 0.0  fneq ||
  gen1 0.0 0.0 delay 0.0  fneq || if
    "delay with list initial-contents confused" #f snd-display
  then
  gen3 0.0 0.0 delay 1.0  fneq
  gen3 0.0 0.0 delay 0.5  fneq ||
  gen3 0.0 0.0 delay 0.25 fneq ||
  gen3 0.0 0.0 delay 0.0  fneq ||
  gen3 0.0 0.0 delay 0.0  fneq || if
    "delay with vct initial-contents confused" #f snd-display
  then
  :size #f <'> make-delay snd-test-catch to res
  res 0 array-ref 'wrong-type-arg object-equal? unless
    "make-delay bad size false: %s" #( res ) snd-display
  then
  make-oscil { osc }
  3 :initial-element osc <'> make-delay snd-test-catch to res
  res 0 array-ref 'wrong-type-arg object-equal? unless
    "make-delay bad initial element: %s" #( res ) snd-display
  then
  -3 <'> make-delay snd-test-catch to res
  res 0 array-ref 'out-of-range object-equal? unless
    "make-delay bad size: %s" #( res ) snd-display
  then
  3 make-delay { d1 }
  3 make-delay { d2 }
  4 make-delay { d3 }
  d1 1.0 0.0 delay drop
  d2 1.0 0.0 delay drop
  d3 1.0 0.0 delay drop
  d1 d2 d3 test-gen-equal
  3 :initial-element 1.0 make-delay to d1
  3 :initial-element 1.0 make-delay to d2
  3 :initial-element 0.5 make-delay to d3
  d1 d2 d3 test-gen-equal
  3 :initial-contents #( 1.0 0.0 0.0 ) make-delay to d1
  3 :initial-contents #( 1.0 0.0 0.0 ) make-delay to d2
  3 :initial-contents #( 1.0 1.0 1.0 ) make-delay to d3
  d1 d2 d3 test-gen-equal
  \ mix (test009)
  *with-test-gtk* unless
    \ FIXME: set-mix-amp-env (gtk segfault)
    \ crashes in set-mix-amp-env with gtk
    "hiho.wave" 1 22050 mus-bshort mus-next new-sound { new-index }
    new-index select-sound drop
    0 new-index 0 find-mix to res
    res if
      "found non-existent mix: %s?" #( res ) snd-display
    then
    "pistol.snd" 100 mix car { mix-id }
    mix-id mix? unless
      "%s not mix?" #( mix-id ) snd-display
    then
    view-mixes-dialog drop
    mix-id mix-position  { pos }
    mix-id mix-length    { len }
    mix-id mix-speed     { spd }
    mix-id mix-home      { home-lst }
    home-lst 0 array-ref { snd }
    home-lst 1 array-ref { chn }
    mix-id mix-amp       { amp }
    mix-id make-mix-sampler { mr }
    mr mix-sampler? unless
      "%s is not mix-sampler?" #( mr ) snd-display
    then
    mr region-sampler?  if
      "mix-sampler: region %s?" #( mr ) snd-display
    then
    mr sampler-position to res
    res 0<> if
      "mix sampler-position: %d?" #( res ) snd-display
    then
    mr sampler-at-end? if
      "mix sampler-at-end: %s?" #( mr ) snd-display
    then
    mr sampler-home to res
    mix-id res object-equal? unless
      "mix sampler-home: %d %s?" #( res mr ) snd-display
    then
    mr object->string 0 16 string-substring to res
    res "#<mix-sampler mi" string<> if
      "mix sampler actually got: [%s]?" #( res ) snd-display
    then
    0.0 0.0 { mx sx }
    99 0 do
      \ XXX: i odd? if mr read-mix-sample else mr read-mix-sample then to mx
      mr read-mix-sample to mx
      100 i + sample to sx
      mx sx fneq if
        "read-mix-sample: %s %s?" #( mx sx ) snd-display
      then
    loop
    \ Scheme: (mr)
    \ Ruby:   mr.call
    \ Forth:  mr #() apply
    mr #() object-apply to mx
    199 sample to sx
    mx sx fneq if
      "read-mix-sample 100: %s %s?" #( mx sx ) snd-display
    then
    mr free-sampler drop
    \
    100 pos <> if
      "mix-position: %d?" #( pos ) snd-display
    then
    41623 len <> if
      "mix-length: %d?" #( len ) snd-display
    then
    snd new-index object-equal? unless
      "snd mix-home: %s?" #( snd ) snd-display
    then
    chn 0<> if
      "chn mix-home: %d?" #( chn ) snd-display
    then
    amp 1.0 fneq if
      "mix-amp: %s?" #( amp ) snd-display
    then
    spd 1.0 fneq if
      "mix-speed: %s?" #( spd ) snd-display
    then
    mix-id <'> play #t nil fth-catch if
      drop                      \ on stack: mix-id
      "cannot play mix" #() snd-display
    else
      drop                      \ on stack: play's return value
    then
    stack-reset
    mix-id :start 1000 <'> play #t nil fth-catch if
      stack-reset               \ on stack: mix-id :start 1000
      "cannot play mix from 1000" #() snd-display
    else
      drop                      \ on stack: play's return value
      stack-reset
    then
    \
    mix-id 200 set-mix-position drop
    mix-id 0.5 set-mix-amp drop
    mix-id 2.0 set-mix-speed drop
    mix-id #( 0 0 1 1 ) set-mix-amp-env drop
    mix-id mix-amp-env to res
    mix-id res set-mix-amp-env drop
    mix-id mix-amp-env { res1 }
    res res1 vequal? unless
      "set-mix-amp-env to self: %s %s?" #( res res1 ) snd-display
    then
    mix-id 20 set-mix-tag-y drop
    mix-id mix-position to pos
    mix-id mix-speed    to spd
    mix-id mix-amp      to amp
    mix-id mix-tag-y    { my }
    200 pos <> if
      "set-mix-position: %d?" #( pos ) snd-display
    then
    spd 2.0 fneq if
      "set-mix-speed: %s?" #( spd ) snd-display
    then
    my 20 <> if
      "set-mix-tag-y: %d?" #( my ) snd-display
    then
    amp 0.5 fneq if
      "set-mix-amp: %s?" #( amp ) snd-display
    then
    mix-id mix-amp-env to res
    res #( 0.0 0.0 1.0 1.0 ) array= unless
      "set-mix-amp-env: %s?" #( res ) snd-display
    then
    \
    3 0.1 make-vct 100 #f #f #t "" mix-vct drop
    0 set-cursor drop
    100 #f #f find-mix { nid }
    nid mix? false? unless
      nid mix-position 100 <> if
        new-index 0 mixes map
          *key* mix-position
        end-map { mx-pos }
        "100 find-mix: %s %s %s?" #( nid dup mix-position mx-pos ) snd-display
      then
    else
      "100 find-mix: not a mix %s?" #( nid ) snd-display
    then
    200 #f #f find-mix to nid
    nid mix? false? unless
      nid mix-position 200 <> if
        new-index 0 mixes map
          *key* mix-position
        end-map { mx-pos }
        "200 find-mix: %s %s %s?" #( nid dup mix-position mx-pos ) snd-display
      then
    else
      "200 find-mix: not a mix %s?" #( nid ) snd-display
    then
    \
    "oboe.snd" 100 mix car to mix-id
    40 set-mix-waveform-height drop
    'hiho mix-id 123 set-mix-property
    'hiho mix-id mix-property to res
    res 123 <> if
      "mix-property: %s?" #( res ) snd-display
    then
    'not-here mix-id mix-property to res
    res if
      "mix-property not-here: %s?" #( res ) snd-display
    then
    #f #f update-time-graph drop
    20 set-mix-waveform-height drop
    new-index revert-sound drop
    new-index close-sound drop
  then                          \ !*with-test-gtk*
  \ envelopes (lists, vcts, arrays) (test015)
  1.0 vct( 0.0 0.0 2.0 1.0 ) 1.0 envelope-interp dup 0.5 fneq if
    "envelope-interp 0.5: %s?" swap snd-display
  else
    drop
  then
  1.0 #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 1.0 envelope-interp dup 1.0 fneq if
    "envelope-interp 1.0: %s?" swap snd-display
  else
    drop
  then
  2.0 #( 0.0 0.0 1.0 1.0 ) 1.0 envelope-interp dup 1.0 fneq if
    "envelope-interp 1.0: %s?" swap snd-display
  else
    drop
  then
  0.0 #( 1.0 0.5 2.0 0.0 ) 1.0 envelope-interp dup 0.5 fneq if
    "envelope-interp 0.5: %s?" swap snd-display
  else
    drop
  then
  0.0 #( -1.0 0.0 0.0 1.0 1.0 -1.0 ) 1.0 envelope-interp dup 1.0 fneq if
    "envelope-interp 1.0; %s?" swap snd-display
  else
    drop
  then
  -0.5 #( -1.0 0.0 0.0 1.0 1.0 -1.0 ) 1.0 envelope-interp dup 0.5 fneq if
    "envelope-interp 0.5: %s?" swap snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 0.0 1.0 1.0 -1.0 ) 1.0 envelope-interp dup 0.0 fneq if
    "envelope-interp 0.0: %s?" swap snd-display
  else
    drop
  then
  -0.5 #( -1.0 -1.0 1.0 1.0 ) 1.0 envelope-interp dup -0.5 fneq if
    "envelope-interp -0.5: %s?" swap snd-display
  else
    drop
  then
  -1.5 #( -1.0 -1.0 1.0 1.0 ) 1.0 envelope-interp dup -1.0 fneq if
    "envelope-interp -1.0: %s?" swap snd-display
  else
    drop
  then
  1.5 #( -1.0 -1.0 1.0 1.0 ) 1.0 envelope-interp dup 1.0 fneq if
    "envelope-interp 1.0: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 ) 1.0 envelope-interp dup 0.1 fneq if
    "envelope-interp 0.1: %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 ) 32.0 envelope-interp dup 0.01336172 fneq if
    "envelope-interp (exp 32): %s?" swap snd-display
  else
    drop
  then
  0.1 #( 0.0 0.0 1.0 1.0 ) 0.012 envelope-interp dup 0.36177473 fneq if
    "envelope-interp (exp 0.012): %s?" swap snd-display
  else
    drop
  then
  0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) 1.0 envelope-interp dup 0.6 fneq if
    "envelope-interp 0.6: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.5 ) { v0 }
  #( 0.0 0.0 2.0 0.5 ) #( 0.0 0.0 1.0 2.0 2.0 1.0 )
    multiply-envelopes dup v0 0 fveql unless
    "multiply-envelopes: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 0.5 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) #( 0.0 0.0 1.0 1.0 2.0 0.0 )
    multiply-envelopes dup v0 0 fveql unless
    "multiply-envelopes: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) max-envelope dup 3.0 fneq if
    "0 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 ) max-envelope dup 1.0 fneq if
    "1 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) max-envelope dup 2.0 fneq if
    "2 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) max-envelope dup -1.0 fneq if
    "3 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) max-envelope dup -1.0 fneq if
    "4 max-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 1.0 2.0 3.0 4.0 0.0 ) min-envelope dup 0.0 fneq if
    "0 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 ) min-envelope dup 1.0 fneq if
    "1 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 1.0 2.0 2.0 ) min-envelope dup 1.0 fneq if
    "2 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -1.0 1.0 -2.0 ) min-envelope dup -2.0 fneq if
    "3 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 -2.0 1.0 -1.0 ) min-envelope dup -2.0 fneq if
    "4 min-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 0.1 0.2 #f #f stretch-envelope dup v0 0 fveql unless
    "stretch-envelope att: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6
    stretch-envelope dup v0 0 fveql unless
    "stretch-envelope dec: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 
    stretch-envelope dup v0 0 fveql unless
    "stretch-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 0.5 1.5 1.0 1.0 ) to v0
  #( 0.0 0.0 1.0 1.0 2.0 0.0 ) #( 0.0 0.0 1.0 1.0 )
    add-envelopes dup v0 0 fveql unless
    "add-envelopes: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.0 2.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) 2.0 scale-envelope dup v0 0 fveql unless
    "scale-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.0 0.0 ) to v0
  #( 0.0 0.0 1.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    "reverse-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 0.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 0.0 ) reverse-envelope dup v0 0 fveql unless
    "reverse-envelope: %s?" swap snd-display
  else
    drop
  then
  #( 0.0 1.0 1.5 1.0 2.0 0.0 ) to v0
  #( 0.0 0.0 0.5 1.0 2.0 1.0 ) reverse-envelope dup v0 0 fveql unless
    "reverse-envelope: %s?" swap snd-display
  else
    drop
  then
;

\ ---------------- test 28: errors ----------------

: check-error-tag { xt expected-tag -- }
  xt snd-test-catch { tag }
  tag if                        \ we ignore #f
    tag car expected-tag <> if
      "%s: expected %s from %s, got %s?"
          #( get-func-name expected-tag xt tag ) snd-display
    then
  then
;

*with-test-motif* [if]
  : snd-motif-error-checks ( -- )
    '( 'Widget 0 ) <'> widget-position 'no-such-widget check-error-tag
    '( 'Widget 0 ) <'> widget-size 'no-such-widget check-error-tag
    '( 'Widget 0 ) <'> widget-text 'no-such-widget check-error-tag
    '( 'Widget 0 ) #( 0 0 ) <'> set-widget-position 'no-such-widget
      check-error-tag
    '( 'Widget 0 ) #( 10 10 ) <'> set-widget-size 'no-such-widget
      check-error-tag
    '( 'Widget 0 ) "hiho" <'> set-widget-text 'no-such-widget check-error-tag
    nil nil { prc tag }
    #( <'> widget-position <'> widget-size <'> widget-text
       <'> hide-widget <'> show-widget <'> focus-widget ) each to prc
      '( 'Widget 0 ) prc snd-test-catch to tag
      tag if
        tag car 'no-such-widget <> if
          "%s of null widget -> %s" #( prc tag ) snd-display
        then
      then
    end-each
  ;
[else]
  <'> noop alias snd-motif-error-checks
[then]
[ifundef] mus-audio-reinitialize
  : mus-audio-reinitialize ( -- n ) 0 ;
[then]

: pt-test-1 <{ a -- f }>     #f ;
: pt-test-2 <{ a b -- f }>   #f ;
: pt-test-3 <{ a b c -- f }> #f ;

#( 1 2 3 )     constant color-95
440 make-oscil constant delay-32
0 make-array   constant vector-0
3 0.0 make-vct constant vct-3
5 0.0 make-vct constant vct-5

#( <'> make-all-pass <'> make-asymmetric-fm <'> make-snd->sample
   <'> make-moving-average <'> make-comb <'> make-filtered-comb
   <'> make-convolve <'> make-delay <'> make-env <'> make-fft-window
   <'> make-file->frample <'> make-file->sample <'> make-filter
   <'> make-fir-filter <'> make-formant <'> make-firmant
   <'> make-frample->file <'> make-granulate <'> make-iir-filter 
   <'> make-locsig <'> make-notch <'> make-one-pole
   <'> make-one-zero <'> make-oscil <'> make-pulse-train <'> make-rand
   <'> make-rand-interp <'> make-readin <'> make-sample->file
   <'> make-sawtooth-wave <'> make-nrxysin <'> make-nrxycos
   <'> make-square-wave <'> make-src <'> make-ncos <'> make-nsin 
   <'> make-table-lookup <'> make-triangle-wave <'> make-two-pole
   <'> make-two-zero <'> make-wave-train <'> make-phase-vocoder
   <'> make-ssb-am <'> make-polyshape <'> make-polywave
   <'> make-player <'> make-region ) constant make-procs

#( :frequency :initial-phase :wave :cosines :amplitude :ratio :size
   :a0 :a1 :a2 :b1 :b2 :input :srate :file :channel :start
   :initial-contents :initial-element :scaler :feedforward 
   :feedback :max-size :radius :gain :partials :r :a :n :fill-time 
   :order :xcoeffs :ycoeffs :envelope :base :duration :offset :end
   :direction :degree :distance :reverb :output :fft-size :expansion 
   :length :hop :ramp :jitter :type :format :comment :channels :filter
   :revout :width :edit :synthesize :analyze :interp :overlap :pitch
   :distribution :sines :dur ) constant keyargs

#( <'> add-mark <'> add-sound-file-extension <'> add-source-file-extension
   <'> sound-file-extensions <'> sound-file? <'> add-to-main-menu
   <'> add-to-menu <'> add-transform <'> amp-control
   <'> ask-about-unsaved-edits <'> as-one-edit <'> ask-before-overwrite
   <'> auto-resize
   <'> auto-update <'> autocorrelate <'> axis-info <'> apply-controls
   <'> change-samples-with-origin <'> channel-style <'> channels
   <'> chans <'> close-sound <'> combined-data-color <'> comment
   <'> contrast-control <'> contrast-control-amp <'> contrast-control? 
   <'> convolve-selection-with <'> convolve-with <'> channel-properties
   <'> channel-property <'> amp-control-bounds <'> speed-control-bounds
   <'> expand-control-bounds <'> contrast-control-bounds
   <'> reverb-control-length-bounds <'> reverb-control-scale-bounds
   <'> cursor-update-interval <'> cursor-location-offset 
   <'> auto-update-interval <'> cursor 
   <'> with-tracking-cursor <'> cursor-size <'> cursor-style
   <'> tracking-cursor-style <'> dac-combines-channels <'> dac-size
   <'> clipping <'> sample-type <'> data-location <'> data-size
   <'> default-output-chans <'> default-output-sample-type
   <'> default-output-srate <'> default-output-header-type
   <'> define-envelope <'> delete-mark <'> delete-marks
   <'> forget-region <'> delete-sample <'> delete-samples 
   <'> delete-samples-and-smooth <'> delete-selection 
   <'> delete-selection-and-smooth <'> display-edits
   <'> edit-fragment <'> edit-position <'> edit-tree <'> edits
   <'> env-selection <'> env-sound <'> enved-envelope <'> enved-base
   <'> enved-clip?  <'> enved-in-dB <'> enved-style <'> enved-power
   <'> enved-target <'> enved-wave? <'> eps-file <'> eps-left-margin
   <'> eps-bottom-margin <'> eps-size <'> expand-control
   <'> expand-control-hop <'> expand-control-jitter
   <'> expand-control-length <'> expand-control-ramp
   <'> expand-control? <'> fft <'> fft-window-alpha
   <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
   <'> transform-size <'> disk-kspace <'> transform-graph-type
   <'> fft-window <'> transform-graph? <'> file-name <'> filter-sound
   <'> filter-control-in-dB <'> filter-control-envelope
   <'> enved-filter-order <'> enved-filter <'> filter-control-in-hz
   <'> filter-control-order <'> filter-selection <'> filter-channel
   <'> filter-control? <'> find-mark <'> find-sound
   <'> finish-progress-report <'> framples <'> free-sampler <'> graph
   <'> transform? <'> delete-transform <'> graph-cursor <'> graph->ps
   <'> gl-graph->ps <'> graph-style <'> lisp-graph? <'>  graphs-horizontal
   <'> header-type <'> in <'> insert-region <'> insert-sample
   <'> insert-samples <'> insert-samples-with-origin <'> insert-selection
   <'> insert-silence <'> insert-sound <'> just-sounds <'> left-sample
   <'> listener-prompt <'> make-mix-sampler <'> make-player
   <'> make-region <'> make-region-sampler <'> make-sampler
   <'> map-chan <'> mark-name <'> mark-properties <'> mark-property
   <'> mark-sample <'> mark-sync <'> mark-sync-max <'> mark-home
   <'> marks <'> mark? <'>  max-transform-peaks <'> max-regions
   <'> maxamp <'> maxamp-position <'> min-dB <'> log-freq-start
   <'> mix <'> mixes <'> mix-amp <'> mix-amp-env <'> mix-length
   <'> mix? <'> mix-position <'> mix-properties <'> mix-property
   <'> mix-name <'> mix-region <'> mix-sampler?  <'> mix-selection 
   <'> mix-sound <'> mix-home <'> mix-speed <'> mix-tag-height 
   <'> mix-tag-width <'> mark-tag-height <'> mark-tag-width
   <'> mix-tag-y <'> mix-vct <'> mix-waveform-height <'> time-graph-style
   <'> lisp-graph-style <'> transform-graph-style <'> read-mix-sample
   <'> next-sample <'> show-full-duration <'> show-full-range
   <'> initial-beg <'> initial-dur <'> transform-normalization
   <'> open-raw-sound <'> open-sound <'> previous-sample <'> peaks 
   <'> player? <'> players <'> print-length <'> progress-report
   <'> read-only <'> redo <'> region-chans <'> region-home 
   <'> region-graph-style <'> region-framples <'> region-position
   <'> region-maxamp <'> region-maxamp-position <'> remember-sound-state
   <'> selection-maxamp <'> selection-maxamp-position <'> region-sample
   <'> region->vct <'> region-srate <'> regions <'> region? 
   <'>  remove-from-menu <'> reset-controls <'> restore-controls
   <'> restore-region <'> reverb-control-decay
   <'> reverb-control-feedback <'> reverb-control-length
   <'> reverb-control-lowpass <'> reverb-control-scale
   <'> reverb-control? <'>  reverse-sound <'> reverse-selection
   <'> revert-sound <'> right-sample <'> sample <'> sampler-at-end?
   <'>  sampler?  <'> samples <'> sampler-position <'> save-controls 
   <'> peak-env-dir <'> save-dir <'> save-edit-history
   <'> save-envelopes <'> save-listener <'> save-marks 
   <'> save-region <'> save-selection <'> save-sound <'> save-sound-as
   <'> save-state <'> save-state-file <'> scale-by <'> scale-selection-by
   <'> scale-selection-to <'> scale-to <'> search-procedure 
   <'> select-all <'> select-channel <'> select-sound <'> selected-channel
   <'> selected-sound <'> selection-position <'> selection-creates-region
   <'> selection-framples <'> selection-member? <'> selection?
   <'> short-file-name <'> show-axes <'> show-controls 
   <'> show-transform-peaks <'> show-indices <'> show-listener
   <'> show-selection <'> unselect-all <'> show-marks
   <'> show-mix-waveforms <'> show-selection-transform
   <'> show-y-zero <'> sinc-width <'> show-grid <'> show-sonogram-cursor
   <'> grid-density <'> smooth-sound <'> smooth-selection <'> snd-spectrum 
   <'> snd-tempnam <'> snd-version <'> sound-files-in-directory 
   <'> sound-loop-info <'> sound? <'> sounds <'> spectrum-end
   <'> spectro-hop <'> spectrum-start <'> spectro-x-angle
   <'> spectro-x-scale <'> spectro-y-angle <'> spectro-y-scale
   <'> spectro-z-angle <'> spectro-z-scale <'> speed-control
   <'> speed-control-style <'> speed-control-tones <'> squelch-update
   <'> srate <'> src-sound <'> src-selection <'> start-progress-report 
   <'> stop-player <'> stop-playing <'> swap-channels <'> syncd-marks
   <'> sync <'> sync-max <'> sound-properties <'> sound-property
   <'> temp-dir <'>  region-sampler?  <'> transform-sample 
   <'> transform->vct <'> transform-framples <'> transform-type
   <'> with-file-monitor <'> undo 
   <'> update-transform-graph <'> update-time-graph 
   <'> update-lisp-graph <'> update-sound <'> clm-table-size 
   <'> with-verbose-cursor <'> view-sound <'> wavelet-type
   <'> with-inset-graph <'> with-interrupts <'> with-pointer-focus
   <'> with-smpte-label <'> with-toolbar <'> with-tooltips 
   <'> with-menu-icons <'> save-as-dialog-src
   <'> save-as-dialog-auto-comment <'> time-graph? 
   <'> time-graph-type <'> wavo-hop <'> wavo-trace <'> window-height
   <'> window-width <'> window-x <'> window-y <'> with-mix-tags
   <'> with-relative-panes <'> with-gl <'> x-axis-style
   <'> beats-per-measure <'> beats-per-minute <'> x-bounds
   <'> x-position-slider <'> x-zoom-slider <'> mus-header-type->string 
   <'> mus-sample-type->string <'> y-bounds <'> y-position-slider
   <'> y-zoom-slider <'> zero-pad <'> zoom-focus-style <'> sync-style
   <'> mus-sound-samples <'> mus-sound-framples <'> mus-sound-duration
   <'> mus-sound-datum-size <'> mus-sound-data-location <'> data-size
   <'> mus-sound-chans <'> mus-sound-srate <'> mus-sound-header-type
   <'> mus-sound-sample-type <'> mus-sound-length
   <'> mus-sound-type-specifier <'> mus-header-type-name
   <'> mus-sample-type-name <'> mus-sound-comment
   <'> mus-sound-write-date <'> mus-bytes-per-sample 
   <'> mus-sound-loop-info <'> mus-alsa-squelch-warning 
   <'> mus-sound-maxamp <'> mus-sound-maxamp-exists?
   <'> mus-clipping
   <'> mus-file-clipping <'> mus-header-raw-defaults <'> moving-average
   <'> moving-average? <'> make-moving-average <'> mus-expand-filename
   <'> all-pass <'> all-pass? <'> amplitude-modulate
   <'> array->file <'> array-interp <'> mus-interpolate <'> asymmetric-fm
   <'> asymmetric-fm?  <'> comb
   <'> comb?  <'> filtered-comb <'> filtered-comb? <'> contrast-enhancement
   <'> convolution <'> convolve <'> convolve? <'> db->linear 
   <'> degrees->radians <'> delay <'> delay? <'> dot-product <'> env 
   <'> env-interp <'> env? <'> file->array <'> file->frample <'> file->frample?
   <'>  file->sample <'> file->sample? <'> filter <'> filter? <'> fir-filter
   <'> fir-filter? <'> formant <'> formant-bank <'> formant?
   <'> frample->file <'> frample->file? <'> frample->frample 
   <'> granulate <'> granulate? <'> hz->radians <'> iir-filter
   <'> iir-filter? <'>  in-any <'> ina <'> inb <'> linear->db <'> locsig
   <'> locsig-ref <'> locsig-reverb-ref <'> locsig-reverb-set! 
   <'> locsig-set! <'>  locsig? <'> make-all-pass <'> make-asymmetric-fm
   <'> make-comb <'> make-filtered-comb <'> make-convolve <'> make-delay
   <'> make-env <'> make-fft-window <'> make-file->frample
   <'> make-file->sample <'> make-filter <'> make-fir-filter
   <'> make-formant <'> make-frample->file <'> make-granulate
   <'> make-iir-filter <'> make-locsig <'> move-locsig
   <'> make-notch <'> make-one-pole <'> make-one-zero <'> make-oscil
   <'> make-pulse-train <'> make-rand <'> make-rand-interp <'> make-readin
   <'> make-sample->file <'> make-sawtooth-wave <'> make-square-wave
   <'> make-src <'> make-ssb-am <'> make-table-lookup <'> make-triangle-wave
   <'> make-two-pole <'> make-two-zero <'> make-wave-train <'> move-sound
   <'> make-move-sound <'> move-sound? <'> mus-float-equal-fudge-factor
   <'> mus-array-print-length <'> mus-channel
   <'> mus-channels <'> make-polyshape <'> polyshape?  <'> mus-close 
   <'> mus-data <'> mus-feedback <'> mus-feedforward <'> mus-fft
   <'> mus-frequency <'> mus-hop <'> mus-increment <'> mus-input?
   <'> mus-file-name <'> mus-length <'> mus-location
   <'> mus-order <'> mus-output? <'>  mus-phase <'> mus-ramp <'> mus-random
   <'> mus-scaler <'> mus-srate <'> mus-xcoeffs <'> mus-ycoeffs <'> notch
   <'> notch? <'> one-pole <'> one-pole?  <'> one-zero <'> one-zero? 
   <'> oscil <'> oscil?  <'> out-any <'> outa <'> outb <'> outc <'> outd
   <'> partials->polynomial <'> partials->wave <'> phase-partials->wave
   <'> polynomial <'> pulse-train <'> pulse-train?  <'> radians->degrees
   <'> radians->hz <'> rand <'> rand-interp <'> rand-interp? <'>  rand?
   <'> readin <'> readin?  <'> rectangular->polar <'> rectangular->magnitudes 
   <'> ring-modulate <'> sample->file <'> sample->file?
   <'> sawtooth-wave <'> sawtooth-wave?  <'> spectrum <'> square-wave
   <'> square-wave?  <'> src <'> src? <'> ssb-am <'> ssb-am? 
   <'> table-lookup <'> table-lookup? <'> tap <'> triangle-wave 
   <'> triangle-wave? <'> two-pole <'> two-pole? <'> two-zero
   <'> two-zero? <'> wave-train <'> wave-train?  <'> make-vct 
   <'> vct-add! <'> vct-subtract! <'> vct-length
   <'> vct-multiply! <'> vct-offset!  <'> vct-ref <'> vct-scale!
   <'> vct-set!  <'> vct-peak <'> vct? <'> list->vct
   <'> vct->list <'> vector->vct <'> vct->vector <'> vct-move!
   <'> vct-reverse! <'> vct-subseq <'> vct <'> little-endian? 
   <'> vct->string <'> clm-channel <'> env-channel <'> map-channel
   <'> scan-channel <'> reverse-channel <'> seconds->samples
   <'> samples->seconds <'> smooth-channel <'> vct->channel 
   <'> channel->vct <'> src-channel <'> scale-channel <'> ramp-channel
   <'> pad-channel <'> normalize-channel <'> cursor-position
   <'> show-listener <'> mus-sound-prune <'> mus-sound-forget
   <'> xramp-channel <'> snd->sample <'> snd->sample? <'> make-snd->sample 
   <'> beats-per-minute <'> beats-per-measure 
   <'> channel-amp-envs <'> convolve-files <'> filter-control-coeffs
   <'> locsig-type <'> make-phase-vocoder <'> mus-describe
   <'> mus-error-type->string <'> mus-file-buffer-size <'> mus-name
   <'> mus-offset <'> mus-out-format <'> mus-reset <'> mus-rand-seed
   <'> mus-width <'> phase-vocoder? <'> polar->rectangular
   <'> phase-vocoder-amp-increments <'> phase-vocoder-amps
   <'> phase-vocoder-freqs <'> phase-vocoder-phase-increments
   <'> phase-vocoder-phases <'> mus-generator? <'> read-sample
   <'> reset-listener-cursor <'> goto-listener-end <'> sampler-home
   <'> selection-chans <'> selection-srate <'> snd-warning
   <'> channel-data <'> x-axis-label <'> variable-graph? <'> y-axis-label
   <'> snd-url <'> snd-urls <'> free-player <'> delete-mix <'> delay-tick
   <'> playing <'> pausing <'> copy-sampler <'> html-dir <'> html-program
   <'> make-fir-coeffs <'> mus-interp-type
   <'> mus-run <'> phase-vocoder <'> player-home <'> redo-edit
   <'> undo-edit ) constant procs

#( <'> amp-control <'> ask-before-overwrite <'> auto-update <'> channel-style
   <'> sound-file-extensions <'> show-full-duration <'> show-full-range
   <'> initial-beg <'> initial-dur <'> contrast-control
   <'> contrast-control-amp <'> combined-data-color <'> amp-control-bounds
   <'> speed-control-bounds <'> expand-control-bounds
   <'> contrast-control-bounds <'> reverb-control-length-bounds
   <'> reverb-control-scale-bounds <'> cursor-update-interval
   <'> cursor-location-offset <'> contrast-control? 
   <'> auto-update-interval <'> cursor <'> channel-properties
   <'> channel-property <'> with-tracking-cursor <'> cursor-size
   <'> cursor-style <'> tracking-cursor-style <'> dac-combines-channels
   <'> dac-size <'> clipping <'> default-output-chans
   <'> default-output-sample-type <'> default-output-srate
   <'> default-output-header-type <'> dot-size <'> enved-envelope 
   <'> enved-base <'> enved-clip? <'> enved-in-dB <'> enved-style
   <'> enved-power <'> enved-target <'> enved-wave? <'> eps-file
   <'> eps-left-margin <'> eps-bottom-margin <'> eps-size <'> expand-control
   <'> expand-control-hop <'> expand-control-jitter <'> expand-control-length
   <'> expand-control-ramp <'> expand-control? <'> fft-window-alpha
   <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
   <'> transform-size <'> transform-graph-type <'> fft-window
   <'> transform-graph? <'> filter-control-in-dB <'> filter-control-envelope
   <'> enved-filter-order <'> enved-filter <'> filter-control-in-hz
   <'> filter-control-order <'> filter-control? <'> graph-cursor
   <'> graph-style <'> lisp-graph? <'> graphs-horizontal <'> just-sounds
   <'> left-sample <'> listener-prompt <'> mark-name <'> mark-properties 
   <'> mark-property <'> mark-sample <'> mark-sync <'> max-transform-peaks
   <'> min-dB <'> log-freq-start <'> mix-amp <'> mix-amp-env <'> mix-name
   <'> mix-position <'> mix-properties <'> mix-property <'> mix-speed
   <'> mix-tag-height <'> mix-tag-width <'> mix-tag-y <'> mark-tag-width
   <'> mark-tag-height <'> mix-waveform-height <'> transform-normalization
   <'> print-length <'> play-arrow-size
   <'> region-graph-style <'> reverb-control-decay <'> reverb-control-feedback
   <'> reverb-control-length <'> reverb-control-lowpass
   <'> reverb-control-scale <'> time-graph-style <'> lisp-graph-style
   <'> transform-graph-style <'> reverb-control? <'> ladspa-dir
   <'> peak-env-dir <'> save-dir <'> save-state-file 
   <'> selection-creates-region <'> show-axes <'> show-controls
   <'> show-transform-peaks <'> show-indices <'> show-marks
   <'> show-mix-waveforms <'> show-selection-transform <'> show-y-zero 
   <'> show-grid <'> show-sonogram-cursor <'> sinc-width <'> spectrum-end
   <'> spectro-hop <'> spectrum-start <'> spectro-x-angle <'> grid-density
   <'> spectro-x-scale <'> spectro-y-angle <'> spectro-y-scale
   <'> spectro-z-angle <'> spectro-z-scale <'> speed-control
   <'> speed-control-style <'> speed-control-tones <'> squelch-update
   <'> sync <'> sound-properties <'> sound-property <'> temp-dir
   <'> y-bounds <'> transform-type <'> with-file-monitor 
   <'> with-verbose-cursor <'> with-inset-graph <'> with-interrupts
   <'> with-pointer-focus <'> wavelet-type <'> x-bounds <'> with-smpte-label
   <'> with-toolbar <'> with-tooltips <'> with-menu-icons
   <'> save-as-dialog-src <'> save-as-dialog-auto-comment <'> time-graph?
   <'> wavo-hop <'> wavo-trace <'> with-gl <'> with-mix-tags
   <'> x-axis-style <'> beats-per-minute <'> zero-pad <'> zoom-focus-style
   <'> sync-style <'> with-relative-panes <'>  window-x <'> window-y
   <'> window-width <'> window-height <'> beats-per-measure <'> channels
   <'> chans <'> comment <'> sample-type <'> data-location <'> data-size
   <'> edit-position <'> framples <'> header-type <'> maxamp <'> read-only
   <'> right-sample <'> sample <'> samples <'> selected-channel
   <'> selected-sound <'> selection-position <'> selection-framples 
   <'> selection-member? <'> sound-loop-info <'> srate <'> time-graph-type
   <'> x-position-slider <'> x-zoom-slider <'> y-position-slider
   <'> y-zoom-slider <'> mus-array-print-length
   <'> mus-float-equal-fudge-factor <'> mus-data <'> mus-feedback
   <'> mus-feedforward <'> mus-frequency <'> mus-hop <'> mus-increment
   <'> mus-length <'> mus-location <'> mus-phase <'> mus-ramp
   <'> mus-scaler <'> vct-ref <'> x-axis-label <'> filter-control-coeffs 
   <'> locsig-type <'> mus-file-buffer-size <'> mus-rand-seed
   <'> mus-width <'> clm-table-size <'> mus-offset <'> mus-reset 
   <'> phase-vocoder-amp-increments <'> phase-vocoder-amps
   <'> phase-vocoder-freqs <'> phase-vocoder-phase-increments
   <'> phase-vocoder-phases <'> html-dir <'> html-program
   <'> mus-interp-type <'> locsig-ref
   <'> locsig-reverb-ref <'> mus-clipping <'> mus-file-clipping
   <'> mus-header-raw-defaults ) constant set-procs

: arity-not-ok     <{ prc args -- f }> prc args     arity-ok not ;
: set-arity-not-ok <{ prc args -- f }> prc args set-arity-ok not ;

procs <'> arity-not-ok  0 array-reject constant procs00
procs <'> arity-not-ok  1 array-reject constant procs01
procs <'> arity-not-ok  2 array-reject constant procs02
procs <'> arity-not-ok  3 array-reject constant procs03
procs <'> arity-not-ok  4 array-reject constant procs04
procs <'> arity-not-ok  5 array-reject constant procs05
procs <'> arity-not-ok  6 array-reject constant procs06
procs <'> arity-not-ok  7 array-reject constant procs07
procs <'> arity-not-ok  8 array-reject constant procs08
procs <'> arity-not-ok 10 array-reject constant procs10
set-procs <'> set-arity-not-ok 1 array-reject constant set-procs00
set-procs <'> set-arity-not-ok 2 array-reject constant set-procs01
set-procs <'> set-arity-not-ok 3 array-reject constant set-procs02
set-procs <'> set-arity-not-ok 4 array-reject constant set-procs03
set-procs <'> set-arity-not-ok 5 array-reject constant set-procs04

: close-sound-mc-cb { -- prc; y self -- val }
  1 proc-create "oboe.snd" open-sound , ( prc )
 does> { y self -- val }
  self @ { ind }
  ind sound? if
    ind close-sound drop
  then
  0.0
;

: close-sound-aoe-1-cb { -- prc; self -- val }
  0 proc-create "oboe.snd" open-sound , "pistol.snd" open-sound ,
 does> { self -- val }
  self       @ ( ind1 ) close-sound drop
  self cell+ @ ( ind2 ) close-sound
;

: close-sound-aoe-2b-cb { ind1 ind2 -- prc; self -- val }
  0 proc-create ind1 , ind2 , ( prc )
 does> { self -- val }
  self       @ ( ind1 ) close-sound drop
  self cell+ @ ( ind2 ) close-sound
;

: close-sound-aoe-2a-cb { -- prc; self -- val }
  0 proc-create "oboe.snd" open-sound , "pistol.snd" open-sound ,
 does> { self -- val }
  self       @ { ind1 }
  self cell+ @ { ind2 }
  100 0.1 ind1 0 set-sample drop
  100 0.1 ind2 0 set-sample drop
  ind1 ind2 close-sound-aoe-2b-cb "inner-edit" as-one-edit
;

: close-sound-fc-cb { -- prc; y self -- f }
  1 proc-create "oboe.snd" open-sound , ( prc )
 does> { y self -- f }
  self @ { ind }
  ind sound? if
    ind close-sound drop
  then
  #f
;

: sc-1-cb { mx -- prc; x self -- f }
  1 proc-create mx , ( prc )
 does> { x self -- f }
  x fabs self @ @ ( mx ) fmax self @ !
  #f
;

: mc-2-cb { ind -- prc; y self -- val }
  1 proc-create ind , ( prc )
 does> { y self -- val }
  y 0.4 f> if
    1 self @ ( ind ) 0 set-framples drop
  then
  y
;

*with-test-complex* [if]
  : mc-3-cb <{ y -- val }> y 0.0+1.0i c* ;
[else]
  <'> noop alias mc-3-cb
[then]

: edpos-1-cb { ind -- prc; self -- edpos }
  0 proc-create ind , ( prc )
 does> { self -- edpos }
  self @ ( ind ) close-sound drop
  current-edit-position
;

: edpos-2-cb <{ snd chn -- edpos }>
  snd close-sound drop
  current-edit-position
;

: check-args-progress-info { msg -- }
  *snd-test-verbose* if
    msg #f snd-test-message
  then
;

: 28-errors ( -- )
  #t set-with-background-processes drop
  reset-almost-all-hooks
  nil nil { prc tag }
  #( <'> amp-control <'> comment
     <'> contrast-control <'> amp-control-bounds <'> speed-control-bounds
     <'> expand-control-bounds <'> contrast-control-bounds
     <'> reverb-control-length-bounds <'> reverb-control-scale-bounds
     <'> contrast-control-amp <'> contrast-control? <'> sample-type
     <'> data-location <'> data-size <'> expand-control
     <'> expand-control-hop <'> expand-control-jitter
     <'> expand-control-length <'> expand-control-ramp 
     <'> expand-control? <'> filter-control-in-dB
     <'> filter-control-in-hz <'> filter-control-envelope
     <'> filter-control-order <'> filter-control?
     <'> finish-progress-report <'> header-type <'> read-only
     <'> reset-controls <'> restore-controls <'> reverb-control-decay
     <'> reverb-control-feedback <'> reverb-control-length 
     <'> reverb-control-lowpass <'> reverb-control-scale <'> reverb-control? 
     <'> save-controls <'> select-sound <'> short-file-name
     <'> sound-loop-info <'> soundfont-info
     <'> speed-control <'> speed-control-style 
     <'> speed-control-tones <'> srate <'> channel-style
     <'> start-progress-report <'> sync <'> swap-channels ) { prcs-1 }
  prcs-1 #(
     <'> apply-controls <'> close-sound <'> channels <'> chans
     <'> file-name <'> framples <'> progress-report
     <'> sound-property <'> sound-properties ) array-append each to prc
    123 integer->sound prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'no-such-sound  <> && if
        "snd no-such-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( 1 make-array "hiho" 0+i 1.5 '( 1 0 ) #( 0 1 ) ) { args-1 }
  #( vct-5 1+i "hiho" delay-32 ) { args-2 }
  nil { arg }
  args-1 each to arg
    prcs-1 #( <'> apply-controls
       <'> close-sound <'> sound-properties ) array-append each to prc
      arg prc snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <>
        tag car 'mus-error      <> && if
          "snd wrong-type-arg %s: %s (%s)" #( prc tag arg ) snd-display
        then
      then
    end-each
  end-each
  args-2 each to arg
    prcs-1 #( <'> channels <'> chans <'> framples ) array-append each to prc
      \ XXX: snd/chn val | val snd/chn
      \ snd/chn before value
      \ g_set_channels(snd, val)           arg 0
      \ snd/chn after value
      \ g_set_amp_control(val, snd, chn)   0 arg
      prc <'> channels      =
      prc <'> chans         = ||
      prc <'> sample-type   = ||
      prc <'> data-location = ||
      prc <'> data-size     = ||
      prc <'> header-type   = ||
      prc <'> srate         = ||
      prc <'> comment       = || if
        arg 0
      else
        0 arg
      then prc set-xt snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <>
        tag car 'syntax-error   <> &&
        tag car 'error          <> && if
          "snd set wrong-type-arg set-%s [%s]: %s" #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  "obtest.snd" open-sound { ind }
  #( <'> amp-control <'> contrast-control <'> contrast-control-amp
     <'> contrast-control?  <'> expand-control <'> amp-control-bounds 
     <'> speed-control-bounds <'> expand-control-bounds
     <'> contrast-control-bounds <'> reverb-control-length-bounds
     <'> reverb-control-scale-bounds <'> expand-control-hop
     <'> expand-control-jitter <'> expand-control-length
     <'> expand-control-ramp <'> expand-control? <'> filter-control-in-dB
     <'> filter-control-in-hz <'> filter-control-envelope
     <'> filter-control-order <'> filter-control? <'> reverb-control-decay
     <'> reverb-control-feedback <'> reverb-control-length
     <'> reverb-control-lowpass <'> reverb-control-scale <'> reverb-control?
     <'> speed-control <'> speed-control-style <'> speed-control-tones 
     <'> channel-style <'> sync ) { prcs-2 }
  args-2 each to arg
    prcs-2 each to prc
      arg ind prc set-xt snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <> if
          "snd safe set wrong-type-arg set-%s [%s]: %s"
            #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  ind close-sound drop
  args-1 each to arg
    #( <'> make-vct <'> vct-length <'> vct->list <'> vct-peak ) each to prc
      arg prc snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <> if
          "vct 0 wrong-type-arg %s [%s]: %s" #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  #( <'> vct-add! <'> vct-subtract! <'> vct-multiply!
     <'> vct-ref <'> vct-scale! ) { vct-prcs-2 }
  nil nil { arg1 arg2 }
  args-1 each to arg1
    args-1 each to arg2
      vct-prcs-2 each to prc
        arg1 arg2 prc snd-test-catch to tag
        tag if
          tag car 'wrong-type-arg       <>
          tag car 'wrong-number-of-args <> &&
          tag car 'mus-error            <> && if
            "vct 1 wrong-whatever %s [%s %s]: %s"
              #( prc arg1 arg2 tag ) snd-display
          then
        then
      end-each
    end-each
  end-each
  args-1 each to arg
    vct-prcs-2 each to prc
      arg vct-3 prc snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <> if
          "vct 2 wrong-whatever %s [%s]: %s" #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  -23 <'> make-vct snd-test-catch to tag
  tag car 'out-of-range <> if
    "make-vct -23: %s" #( tag ) snd-display
  then
  vct-3 { v }
  v 12 <'> vct-ref snd-test-catch to tag
  tag car 'out-of-range <> if
    "vct[12]: %s" #( tag ) snd-display
  then
  #( <'> all-pass? <'> asymmetric-fm? <'> comb? <'> filtered-comb?
     <'> convolve? <'> delay? <'> env? <'> file->frample? <'> file->sample?
     <'> snd->sample? <'> filter? <'> fir-filter? <'> formant? <'> firmant?
     <'> frample->file? <'> granulate? <'> iir-filter?
     <'> locsig? <'> move-sound? <'> mus-input? <'> mus-output?
     <'> notch? <'> one-pole? <'> one-zero? <'> oscil? <'> phase-vocoder?
     <'> pulse-train? <'> rand-interp? <'> rand? <'> readin?
     <'> sample->file? <'> sawtooth-wave? <'> nrxysin? <'> nrxycos?
     <'> square-wave? <'> src? <'> ncos? <'> nsin? <'> table-lookup?
     <'> triangle-wave? <'> two-pole? <'> two-zero?  <'> wave-train?
     <'> color? <'> mix-sampler? <'> moving-average? <'> ssb-am?
     <'> sampler? <'> region-sampler? <'> vct? ) { ?prcs }
  args-1 each to arg
    ?prcs each to prc
      arg prc snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <> if
          "?proc %s [%s]: %s" #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  ?prcs each to prc
    440 make-oscil prc snd-test-catch to tag
    tag if
      "oscil?proc %s [440 make-oscil]: %s" #( prc tag ) snd-display
    then
  end-each
  #( <'> reverse-selection <'> selection-position <'> selection-framples 
     <'> smooth-selection <'> scale-selection-to <'> insert-selection
     <'> delete-selection <'> delete-selection-and-smooth
     <'> mix-selection ) each to prc
    prc snd-test-catch to tag
    tag if
      tag car 'no-active-selection <> if
        "0 selection %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> src-selection <'> filter-selection <'> env-selection ) each to prc
    0.0 prc snd-test-catch to tag
    tag if
      tag car 'no-active-selection <> if
        "1 selection %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> all-pass <'> asymmetric-fm <'> comb
     <'> filtered-comb <'> convolve <'> db->linear <'> moving-average 
     <'> degrees->radians <'> delay <'> env <'> formant <'> firmant
     <'> granulate <'> hz->radians <'> linear->db 
     <'> make-all-pass <'> make-asymmetric-fm <'> make-comb
     <'> make-filtered-comb <'> make-convolve <'> make-delay
     <'> make-env <'> make-file->frample <'> make-file->sample
     <'> make-filter <'> make-fir-filter <'> make-formant
     <'> make-firmant <'> make-granulate 
     <'> make-iir-filter <'> make-locsig <'> make-notch 
     <'> make-one-pole <'> make-one-zero <'> make-oscil
     <'> make-pulse-train <'> make-rand <'> make-rand-interp
     <'> make-readin <'> make-sawtooth-wave <'> make-nrxysin
     <'> make-nrxycos <'> make-square-wave <'> make-src <'> make-ncos 
     <'> make-nsin <'> make-table-lookup <'> make-triangle-wave 
     <'> make-two-pole <'> make-two-zero <'> make-wave-train
     <'> make-ssb-am <'> mus-channel <'> mus-channels <'> make-polyshape
     <'> make-polywave <'> mus-data <'> mus-feedback <'> mus-feedforward
     <'> mus-frequency <'> mus-hop <'> mus-increment <'> mus-length
     <'> mus-file-name <'> mus-location <'> mus-name <'> mus-order
     <'> mus-phase <'> mus-ramp <'> mus-random <'> mus-run <'> mus-scaler
     <'> mus-xcoeffs <'> mus-ycoeffs <'> notch <'> one-pole <'> one-zero
     <'> make-moving-average <'> seconds->samples <'> samples->seconds
     <'> oscil <'> partials->polynomial <'> partials->wave
     <'> phase-partials->wave <'> phase-vocoder <'> pulse-train
     <'> radians->degrees <'> radians->hz <'> rand <'> rand-interp
     <'> readin <'> sawtooth-wave <'> nrxysin <'> nrxycos <'> square-wave
     <'> src <'> ncos <'> nsin <'> table-lookup <'> tap <'> triangle-wave
     <'> two-pole <'> two-zero <'> wave-train <'> ssb-am ) { clm-prcs-1 }
  #( 1 make-array color-95 '( 1.0 ) ) each to arg
    clm-prcs-1 each to prc
      arg prc snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg <>
        tag car 'no-data        <> &&
        tag car 'no-such-method <> &&
        tag car 'bad-type       <> &&
        tag car 'error          <> &&
        tag car 'arg-error      <> && if
          "clm %s [%s]: %s" #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  #( <'> all-pass <'> array-interp <'> asymmetric-fm <'> comb
     <'> filtered-comb <'> contrast-enhancement <'> convolution
     <'> convolve <'> moving-average <'> convolve-files
     <'> delay <'> dot-product <'> env-interp <'> file->sample
     <'> snd->sample <'> filter <'> fir-filter <'> formant <'> firmant
     <'> formant-bank <'> granulate <'> iir-filter <'> ina <'> inb
     <'> locsig-ref <'> locsig-reverb-ref <'> make-all-pass
     <'> make-asymmetric-fm <'> make-comb <'> make-filtered-comb
     <'> make-delay <'> make-env <'> make-fft-window <'> make-filter
     <'> make-fir-filter <'> make-formant <'> make-firmant
     <'> make-granulate <'> make-iir-filter <'> make-locsig <'> make-notch 
     <'> make-one-pole <'> make-one-zero <'> make-oscil <'> make-phase-vocoder
     <'> make-pulse-train <'> make-rand <'> make-rand-interp
     <'> make-readin <'> make-sawtooth-wave <'> make-moving-average
     <'> make-nrxysin <'> make-nrxycos <'> make-square-wave <'> make-src
     <'> make-ncos <'> make-nsin <'> make-table-lookup <'> make-triangle-wave 
     <'> make-two-pole <'> make-two-zero <'> make-wave-train
     <'> notch <'> one-pole <'> one-zero
     <'> oscil <'> partials->polynomial <'> partials->wave
     <'> make-polyshape <'> make-polywave <'> phase-partials->wave
     <'> phase-vocoder <'> polynomial <'> pulse-train <'> rand <'> rand-interp
     <'> rectangular->polar <'> rectangular->magnitudes <'> ring-modulate
     <'> sawtooth-wave <'> nrxysin <'> nrxycos
     <'> square-wave <'> src <'> ncos <'> nsin <'> table-lookup <'> tap
     <'> triangle-wave <'> two-pole <'> two-zero <'> wave-train <'> ssb-am
     <'> make-ssb-am ) each to prc
    make-oscil vct-5 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'bad-arity      <> &&
      tag car 'error          <> &&
      tag car 'mus-error      <> && if
        "clm-1 %s [make-oscil]: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mus-channel <'> mus-channels <'> mus-data <'> mus-feedback
     <'> mus-feedforward <'> mus-frequency <'> mus-hop <'> mus-increment
     <'> mus-length <'> mus-location <'> mus-name <'> mus-order
     <'> mus-phase <'> mus-ramp <'> mus-random <'> mus-run <'> mus-scaler
     <'> mus-xcoeffs <'> mus-ycoeffs ) each to prc
    make-oscil vector-0 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'syntax-error   <> &&
      tag car 'error          <> && if
        "mus-gen %s [make-oscil]: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mus-sound-samples <'> mus-sound-framples <'> mus-sound-duration 
     <'> mus-sound-datum-size <'> mus-sound-data-location <'> mus-sound-chans
     <'> mus-sound-srate <'> mus-sound-header-type <'> mus-sound-sample-type
     <'> mus-sound-length <'> mus-sound-type-specifier
     <'> mus-header-type-name <'> mus-sample-type-name <'> mus-sound-comment
     <'> mus-sound-write-date <'> mus-bytes-per-sample
     <'> mus-sound-loop-info <'> mus-sound-mark-info
     <'> mus-sound-maxamp <'> mus-sound-maxamp-exists?
     <'> mus-header-type->string
     <'> mus-sample-type->string ) { mus-snd-prcs-1 }
  mus-snd-prcs-1 each to prc
    vct-5 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <> if
        "mus-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  mus-snd-prcs-1 each to prc
    prc snd-test-catch to tag
    tag if
      tag car 'wrong-number-of-args <>
      tag car 'error                <> && if
        "no arg mus-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mus-sound-samples <'> mus-sound-framples <'> mus-sound-duration
     <'> mus-sound-datum-size <'> mus-sound-data-location
     <'> mus-sound-chans <'> mus-sound-srate <'> mus-sound-header-type
     <'> mus-sound-sample-type <'> mus-sound-length
     <'> mus-sound-type-specifier <'> mus-sound-comment 
     <'> mus-sound-write-date <'> mus-sound-maxamp 
     <'> mus-sound-maxamp-exists? ) each to prc
    "/bad/baddy" prc snd-test-catch to tag
    tag if
      tag car 'mus-error <> if
        "bad file mus-sound %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "/bad/baddy" mus-sound-forget drop
  #( <'> channel-widgets <'> cursor <'> channel-properties
     <'> channel-property <'> cursor-position
     <'> cursor-size <'> cursor-style <'> tracking-cursor-style
     <'> delete-sample <'> display-edits <'> dot-size <'> edit-fragment 
     <'> edit-position <'> edit-tree <'> edits <'> fft-window-alpha
     <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
     <'> fft-with-phases <'> transform-size <'> transform-graph-type
     <'> fft-window <'> transform-graph? <'> graph
     <'> graph-style <'> lisp-graph? <'> insert-sound <'> time-graph-style
     <'> lisp-graph-style <'> transform-graph-style
     <'> left-sample <'> make-graph-data <'> map-chan <'> max-transform-peaks
     <'> maxamp-position <'> min-dB <'> mix-region
     <'> transform-normalization <'> peaks <'> position->x <'> position->y
     <'> reverse-sound <'> revert-sound <'> right-sample <'> sample
     <'> save-sound <'> save-sound-as <'> select-channel
     <'> show-axes <'> show-transform-peaks <'> show-marks
     <'> show-mix-waveforms <'> show-y-zero <'> show-grid
     <'> show-sonogram-cursor <'> spectrum-end <'> spectro-hop
     <'> spectrum-start <'> spectro-x-angle <'> spectro-x-scale
     <'> spectro-y-angle <'> grid-density <'> spectro-y-scale
     <'> spectro-z-angle <'> spectro-z-scale <'> squelch-update
     <'> transform-sample <'> transform->vct <'> transform-framples
     <'> transform-type <'> update-transform-graph <'> update-time-graph
     <'> update-lisp-graph <'> update-sound <'> wavelet-type <'> time-graph?
     <'> time-graph-type <'> wavo-hop <'> wavo-trace <'> x-bounds
     <'> x-position-slider <'> x-zoom-slider <'> x-axis-label <'> y-axis-label
     <'> y-bounds <'> y-position-slider <'> y-zoom-slider
     <'> zero-pad ) { chn-prcs }
  chn-prcs each to prc
    vct-5 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'error          <> &&
      tag car 'no-such-sound  <> && if
        "chn (no snd) procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  chn-prcs <'> combined-data-color array-push each to prc
    0 vct-5 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <> if
        "chn (no chn) procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> channel-widgets <'> cursor <'> channel-properties
     <'> cursor-position <'> cursor-size <'> cursor-style
     <'> tracking-cursor-style <'> display-edits <'> dot-size <'> edit-position
     <'> edit-tree <'> edits <'> env-sound <'> fft-window-alpha
     <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
     <'> fft-with-phases <'> transform-size <'> transform-graph-type
     <'> fft-window <'> transform-graph? <'> filter-sound <'> graph-data
     <'> graph-style <'> lisp-graph? <'> left-sample <'> time-graph-style
     <'> lisp-graph-style <'> transform-graph-style <'> make-graph-data
     <'> max-transform-peaks <'> maxamp <'> maxamp-position <'> min-dB
     <'> transform-normalization <'> reverse-sound <'> revert-sound
     <'> right-sample <'> save-sound <'> scale-by <'> scale-to
     <'> show-axes <'> show-transform-peaks <'> show-marks
     <'> show-mix-waveforms <'> show-y-zero <'> show-grid
     <'> show-sonogram-cursor <'> spectrum-end <'> spectro-hop
     <'> spectrum-start <'> spectro-x-angle <'> spectro-x-scale
     <'> spectro-y-angle <'> spectro-y-scale <'> spectro-z-angle
     <'> spectro-z-scale <'> squelch-update <'> grid-density <'> src-sound
     <'> transform->vct <'> transform-framples <'> transform-type
     <'> update-transform-graph <'> update-time-graph <'> update-lisp-graph
     <'> update-sound <'> wavelet-type <'> time-graph? <'> time-graph-type
     <'> wavo-hop <'> wavo-trace <'> x-bounds <'> x-position-slider
     <'> x-zoom-slider <'> y-bounds <'> y-position-slider <'> x-axis-label
     <'> y-axis-label <'> y-zoom-slider <'> zero-pad ) each to prc
    1234 integer->sound prc snd-test-catch to tag
    tag if
      tag car 'no-such-sound <> if
        "chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> delete-sample <'> edit-fragment <'> graph-data <'> graph-style
     <'> play <'> position->x <'> position->y <'> redo
     <'> time-graph-style <'> lisp-graph-style <'> transform-graph-style
     <'> scale-by <'> scale-to <'> undo <'> x->position <'> y->position
     <'> x-axis-label ) each to prc
    0 1234 prc snd-test-catch to tag
    tag if
      tag car 'no-such-sound <> if
        "snd(1) chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "oboe.snd" open-sound to ind
  #( <'> delete-sample <'> edit-fragment <'> graph-data <'> position->x
     <'> position->y <'> redo <'> scale-by <'> scale-to <'> undo
     <'> x->position <'> y->position ) each to prc
    prc proc-name "x-axis-label" string<> if
      0 ind 1234 prc snd-test-catch to tag
      tag if
        tag car 'no-such-channel <> if
          "snd(1 1234) chn procs %s: %s" #( prc tag ) snd-display
        then
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" find-sound sound? if
    "oboe.snd is still open?" #() snd-display
  then
  "oboe.snd" open-sound to ind
  #( <'> channel-widgets <'> cursor <'> cursor-position <'> cursor-size
     <'> cursor-style <'> tracking-cursor-style <'> display-edits <'> dot-size
     <'> edit-position <'> edit-tree <'> edits <'> fft-window-alpha
     <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
     <'> fft-with-phases <'> transform-size <'> transform-graph-type
     <'> fft-window <'> transform-graph? <'> graph-style <'> lisp-graph?
     <'> left-sample <'> time-graph-style <'> lisp-graph-style
     <'> transform-graph-style <'> combined-data-color <'> make-graph-data
     <'> max-transform-peaks <'> maxamp <'> maxamp-position <'> min-dB
     <'> transform-normalization <'> reverse-sound <'> right-sample 
     <'> show-axes <'> show-transform-peaks <'> show-marks
     <'> show-mix-waveforms <'> show-y-zero <'> show-grid
     <'> show-sonogram-cursor <'>  grid-density <'> spectrum-end
     <'> spectro-hop <'> spectrum-start <'> spectro-x-angle
     <'> spectro-x-scale <'> spectro-y-angle <'> spectro-y-scale 
     <'> spectro-z-angle <'> spectro-z-scale <'> squelch-update
     <'> transform->vct <'> transform-framples <'> transform-type
     <'> update-transform-graph <'> update-time-graph <'> update-lisp-graph
     <'> wavelet-type <'> time-graph?  <'> time-graph-type <'> wavo-hop
     <'> wavo-trace <'> x-bounds <'> x-position-slider <'> x-axis-label
     <'> x-zoom-slider <'> y-bounds <'> y-position-slider <'> y-zoom-slider
     <'> zero-pad <'> channel-properties <'> channel-property ) each to prc
    ind 1234 prc snd-test-catch to tag
    tag if
      tag car 'no-such-channel <>
      tag car 'no-such-sound   <> && if
        "chn (2) procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" find-sound sound? if
    "oboe.snd is still open?" #() snd-display
  then
  "oboe.snd" open-sound to ind
  #( <'> channel-widgets <'> cursor <'> cursor-position <'> display-edits
     <'> dot-size <'> edit-tree <'> edits <'> fft-window-alpha
     <'> fft-window-beta <'> fft-log-frequency <'> fft-log-magnitude
     <'> fft-with-phases <'> transform-size <'> transform-graph-type
     <'> fft-window <'> transform-graph? <'> graph-style <'> lisp-graph?
     <'> left-sample <'> make-graph-data <'> max-transform-peaks <'> maxamp
     <'> maxamp-position <'> lisp-graph-style <'> transform-graph-style
     <'> make-graph-data <'> combined-data-color <'> min-dB
     <'> transform-normalization <'> reverse-sound <'> right-sample 
     <'> show-axes <'> grid-density <'> show-transform-peaks <'> show-marks
     <'> show-mix-waveforms <'> show-y-zero <'> show-grid
     <'> show-sonogram-cursor <'> spectrum-end <'> spectro-hop
     <'> spectrum-start <'> spectro-x-angle <'> spectro-x-scale
     <'> spectro-y-angle <'> spectro-y-scale <'> spectro-z-angle
     <'> spectro-z-scale <'> squelch-update <'> transform->vct
     <'> transform-framples <'> transform-type <'> update-transform-graph
     <'> update-time-graph <'> update-lisp-graph <'> wavelet-type
     <'> time-graph? <'> time-graph-type <'> wavo-hop <'> wavo-trace
     <'> x-bounds <'> x-position-slider <'> x-zoom-slider <'> y-bounds
     <'> y-position-slider <'> y-zoom-slider <'> zero-pad
     <'> x-axis-label ) each to prc
    vct-5 ind 0 prc set-xt snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'syntax-error   <> &&
      tag car 'error          <> && if
        "set chn procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" find-sound sound? if
    "oboe.snd is still open?" #() snd-display
  then
  #( <'> mix-amp <'> mix-length <'> mix-name
     <'> mix-position <'> mix-home <'> mix-speed <'> mix-tag-y ) { mix-prcs }
  mix-prcs #( <'> mix-amp-env ) array-append each to prc
    vct-5 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'syntax-error   <> &&
      tag car 'error          <> && if
        "[0] mix procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  #( <'> mix-name <'> mix-position <'> mix-home <'> mix-speed
     <'> mix-tag-y ) { mix-set-prcs }
  mix-set-prcs each to prc
    1234 integer->mix vct-5 prc set-xt snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'syntax-error   <> &&
      tag car 'error          <> &&
      tag car 'no-such-mix    <> && if
        "[2] set mix procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "oboe.snd" open-sound to ind
  "oboe.snd" 10 mix-sound { id }
  mix-set-prcs each to prc
    id vct-5 prc set-xt snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'syntax-error   <> &&
      tag car 'error          <> && if
        "[3] set mix procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" find-sound sound? if
    "oboe.snd is still open?" #() snd-display
  then
  #( <'> add-mark <'> mark-name <'> mark-sample <'> mark-sync
     <'> mark-home <'> delete-mark <'> delete-marks <'> find-mark ) each to prc
    vct-5 prc snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <> if
        "mark procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  "oboe.snd" open-sound to ind
  0 ind 0 add-mark to id
  #( <'> mark-name <'> mark-sample <'> mark-sync ) each to prc
    id vct-5 prc set-xt snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <> if
        "set mark procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  ind close-sound drop
  "oboe.snd" find-sound sound? if
    "oboe.snd is still open?" #() snd-display
  then
  #( <'> region-chans <'> region-home <'> region-framples
     <'> region-position <'> region-maxamp <'> region-maxamp-position
     <'> region-srate <'> forget-region ) { reg-prcs-1 }
  #( vct-5 #( 0 1 ) 0+i "hiho" '( 0 1 ) ) each to arg
    reg-prcs-1 #( <'> region-sample <'> region->vct ) array-append each to prc
      arg prc snd-test-catch to tag
      tag if
        tag car 'wrong-type-arg       <>
        tag car 'wrong-number-of-args <> && if
          "region procs %s [%s]: %s" #( prc arg tag ) snd-display
        then
      then
    end-each
  end-each
  #( <'> axis-color <'> enved-filter-order <'> enved-filter
     <'> filter-control-waveform-color <'> ask-before-overwrite 
     <'> ask-about-unsaved-edits <'> auto-resize <'> auto-update
     <'> axis-label-font <'> axis-numbers-font <'> basic-color <'> bind-key
     <'> show-full-duration <'> show-full-range <'> initial-beg <'> initial-dur
     <'> channel-style <'> color-cutoff <'> color-orientation-dialog
     <'> color-inverted <'> color-scale <'> cursor-color
     <'> dac-combines-channels <'> dac-size <'> clipping <'> data-color
     <'> default-output-chans <'> default-output-sample-type 
     <'> default-output-srate <'> default-output-header-type
     <'> enved-envelope <'> enved-base <'> enved-clip? <'> enved-in-dB
     <'> enved-dialog <'> enved-style <'> enved-power <'> enved-target
     <'> enved-waveform-color <'> enved-wave? <'> eps-file <'> eps-left-margin
     <'> eps-bottom-margin <'> eps-size <'> foreground-color <'> graph-color
     <'> graph-cursor <'> highlight-color <'> just-sounds <'> key-binding
     <'> listener-color <'> listener-font <'> listener-prompt
     <'> listener-text-color <'> max-regions <'> mix-waveform-height
     <'> region-graph-style <'> position-color <'> time-graph-style
     <'> lisp-graph-style <'> transform-graph-style <'> peaks-font
     <'> bold-peaks-font <'> print-length
     <'> play-arrow-size <'> sash-color <'> ladspa-dir <'> peak-env-dir
     <'> save-dir <'> save-state-file <'> selected-channel
     <'> selected-data-color <'> selected-graph-color <'> selected-sound 
     <'> selection-creates-region <'> show-controls <'> show-indices
     <'> show-listener <'> show-selection-transform <'> sinc-width <'> temp-dir
     <'> text-focus-color <'> tiny-font <'> with-file-monitor
     <'> unbind-key <'> with-verbose-cursor
     <'> with-inset-graph <'> with-interrupts <'> with-pointer-focus
     <'> window-height <'> beats-per-measure <'> with-smpte-label 
     <'> with-toolbar <'> with-tooltips <'> with-menu-icons 
     <'> remember-sound-state <'> save-as-dialog-src
     <'> save-as-dialog-auto-comment <'> window-width <'> window-x
     <'> window-y <'> with-gl <'> with-mix-tags <'> x-axis-style
     <'> beats-per-minute <'> zoom-color <'> mix-tag-height <'> mix-tag-width
     <'> with-relative-panes <'> clm-table-size
     <'> mark-tag-width <'> mark-tag-height ) each to prc
    vct-5 prc set-xt snd-test-catch to tag
    tag if
      tag car 'wrong-type-arg <>
      tag car 'syntax-error   <> &&
      tag car 'error          <> && if
        "misc procs %s: %s" #( prc tag ) snd-display
      then
    then
  end-each
  nil { hook }
  snd-hooks each to hook
    hook <'> noop 0 make-proc <'> add-hook! snd-test-catch to tag
    tag if
      \ XXX: FTH special 'bad-arity (add-hook!) [ms]
      tag car 'bad-arity      <>
      tag car 'wrong-type-arg <> && if
        "[0] hooks %s: %s" #( hook hook-name tag ) snd-display
      then
    then
  end-each
  reset-almost-all-hooks
  #( <'> exit-hook <'> stop-playing-selection-hook
     <'> color-hook <'> orientation-hook
     <'> start-playing-selection-hook ) each to hook
    hook <'> noop 3 make-proc <'> add-hook! #t nil fth-catch to tag
    stack-reset
    tag if
      \ XXX: FTH special 'bad-arity (add-hook!) [ms]
      tag car 'bad-arity      <>
      tag car 'wrong-type-arg <> && if
        "[1] hooks %s: %s" #( hook hook-name tag ) snd-display
      then
    then
  end-each
  reset-almost-all-hooks
  #f set-ask-about-unsaved-edits drop
  #f set-remember-sound-state drop
  "not-an-env" <'> set-enved-envelope 'no-such-envelope check-error-tag
  "/bad/baddy" <'> save-envelopes 'cannot-save check-error-tag
  "/bad/baddy" <'> mus-sound-report-cache 'cannot-save check-error-tag
  <'> noop 3 make-proc <'> set-search-procedure 'bad-arity check-error-tag
  0 "oboe.snd" 1 <'> make-sampler 'no-such-channel check-error-tag
  0 "oboe.snd" -1 <'> make-sampler 'no-such-channel check-error-tag
  <char> p 0 <'> noop 2 make-proc <'> bind-key 'bad-arity check-error-tag
  <'> noop 1 make-proc <'> set-zoom-focus-style 'bad-arity check-error-tag
  123 #( 0 0 1 1 ) <'> set-sound-loop-info 'no-such-sound check-error-tag
  "fmv.snd" 2 22050 mus-bfloat mus-nist "comment" <'> new-sound 'bad-header
    check-error-tag
  123 <'> player-home 'wrong-type-arg check-error-tag
  "/hiho" <'> set-temp-dir 'no-such-file check-error-tag
  "/hiho" <'> set-save-dir 'no-such-file check-error-tag
  sf-dir "bad_chans.snd" $+ <'> mus-sound-maxamp 'bad-header check-error-tag
  :order 32 :ycoeffs 4 0.0 make-vct <'> make-iir-filter 'mus-error 
    check-error-tag
  :coeffs 4 0 make-vct :ycoeffs 4 0 make-vct <'> make-iir-filter 'mus-error
    check-error-tag
  :coeffs 4 0 make-vct :xcoeffs 4 0 make-vct <'> make-fir-filter 'mus-error
    check-error-tag
  :size 123456789 <'> make-table-lookup 'out-of-range check-error-tag
  :ramp -0.5 <'> make-granulate 'out-of-range check-error-tag
  :ramp 1.5 <'> make-granulate 'out-of-range check-error-tag
  :expansion 32000.0 <'> make-granulate 'mus-error check-error-tag
  "test.snd" :channels 0 <'> new-sound 'out-of-range check-error-tag
  "test.snd" :srate 0 <'> new-sound 'out-of-range check-error-tag
  "test.snd" :size -1 <'> new-sound 'out-of-range check-error-tag
  "test.snd" :size 0 <'> make-readin 'out-of-range check-error-tag
  "test.snd" :size -1 <'> make-readin 'out-of-range check-error-tag
  "oboe.snd" 0 <'> make-file->sample 'out-of-range check-error-tag
  "oboe.snd" -1 <'> make-file->sample 'out-of-range check-error-tag
  "oboe.snd" 0 <'> make-file->frample 'out-of-range check-error-tag
  "oboe.snd" -1 <'> make-file->frample 'out-of-range check-error-tag
  -1 <'> set-default-output-sample-type 'out-of-range check-error-tag
  mus-soundfont <'> set-default-output-header-type 'out-of-range check-error-tag
  sf-dir "bad_location.nist" $+ <'> mus-sound-chans 'mus-error check-error-tag
  sf-dir "bad_field.nist" $+ <'> mus-sound-chans 'mus-error check-error-tag
  snd-motif-error-checks
  -1 <'> main-menu 'no-such-menu check-error-tag
  111 <'> main-menu 'no-such-menu check-error-tag
  "hiho" :header-type 123 <'> new-sound 'out-of-range check-error-tag
  "hiho" :header-type mus-nist :sample-type 123 <'> new-sound 'out-of-range check-error-tag
  "hiho" :header-type mus-nist :sample-type mus-bfloat <'> new-sound 'bad-header check-error-tag
  -1 <'> set-mus-array-print-length 'out-of-range check-error-tag
  -1 <'> set-print-length 'out-of-range check-error-tag
  -1 <'> set-play-arrow-size 'out-of-range check-error-tag
  12 <'> set-enved-style 'out-of-range check-error-tag
  1.5 0.0 0.0 <'> make-color 'out-of-range check-error-tag
  -0.5 0.0 0.0 <'> make-color 'out-of-range check-error-tag
  #f <'> make-variable-graph 'wrong-type-arg check-error-tag
  <'> graph->ps 'cannot-print check-error-tag
  "oboe.snd" open-sound to ind
  #t set-selection-creates-region drop
  select-all drop
  "sel0.snd" :not-a-key 3 <'> save-selection 'mus-error check-error-tag
  '( ind ) <'> read-only 'wrong-type-arg check-error-tag
  ind '( 0 ) <'> framples 'wrong-type-arg check-error-tag
  0 -10 <'> smooth-sound 'wrong-type-arg check-error-tag
  0 ind 123 <'> mix-selection 'no-such-channel check-error-tag
  0 ind 123 <'> insert-selection 'no-such-channel check-error-tag
  ind 0 <'> set-channels 'out-of-range check-error-tag
  ind -1 <'> set-channels 'wrong-type-arg check-error-tag
  ind 12340 <'> set-channels 'out-of-range check-error-tag
  ind 12340 <'> set-sample-type 'out-of-range check-error-tag
  ind 12340 <'> set-header-type 'out-of-range check-error-tag
  ind 0 <'> set-srate 'out-of-range check-error-tag
  ind -1 <'> set-data-location 'wrong-type-arg check-error-tag
  ind -1 <'> set-data-size 'wrong-type-arg check-error-tag
  -1 -1 <'> set-sample 'no-such-sample check-error-tag
  -1 <'> sample 'no-such-sample check-error-tag
  -10 <'> set-framples 'out-of-range check-error-tag
  0.0 <'> set-min-dB 'out-of-range check-error-tag
  0.0 ind 0 <'> set-min-dB 'out-of-range check-error-tag
  1 -22 <'> start-playing 'out-of-range check-error-tag
  1 0 <'> start-playing 'out-of-range check-error-tag
  #( 0.0 1.0 0.1 -0.1 1.0 0.0 ) ind <'> set-filter-control-envelope
    'out-of-range check-error-tag
  #( 0.0 1.0 0.1  1.1 1.0 0.0 ) ind  <'> set-filter-control-envelope
    'out-of-range check-error-tag
  #( 0 0 0.1 0.1 0.05 0.1 1 1 ) 32 <'> filter-sound 'env-error check-error-tag
  ind 123 <'> apply-controls 'out-of-range check-error-tag
  #( 0.0 2.0 ) <'> set-speed-control-bounds 'out-of-range check-error-tag
  #( 0.0 2.0 ) <'> set-expand-control-bounds 'out-of-range check-error-tag
  #( 2.0 0.0 ) <'> set-speed-control-bounds 'out-of-range check-error-tag
  #( 2.0 0.0 ) <'> set-expand-control-bounds 'out-of-range check-error-tag
  sf-dir "bad_chans.snd" $+ <'> insert-sound 'bad-header check-error-tag
  sf-dir "bad_chans.snd" $+ <'> convolve-with 'IO-error check-error-tag
  "hiho.snd" ind -12 <'> save-sound-as 'cannot-save check-error-tag
  "hiho.snd" ind :header-type mus-next :sample-type -12
    <'> save-sound-as 'cannot-save check-error-tag
  "test.snd" ind :header-type mus-nist :sample-type mus-bdouble
    <'> save-sound-as 'cannot-save check-error-tag
  "test.snd" ind :header-type mus-aifc :sample-type mus-lfloat
    <'> save-sound-as 'cannot-save check-error-tag
  "test.snd" ind :header-type mus-riff :sample-type mus-bshort
    <'> save-sound-as 'cannot-save check-error-tag
  "test.snd" ind :header-type mus-voc :sample-type mus-bshort
    <'> save-sound-as 'cannot-save check-error-tag
  "test.snd" 22050 mus-bshort mus-riff
    <'> save-selection 'cannot-save check-error-tag
  "test.snd" 22050 mus-bshort mus-voc
    <'> save-selection 'cannot-save check-error-tag
  #( 0 0 1 1 ) :length 11 make-env <'> src-channel 'out-of-range check-error-tag
  #( 0 1 1 0 ) :length 11 make-env <'> src-channel 'out-of-range check-error-tag
  #( 0 1 1 -1 ) :length 11 make-env <'> src-channel 'out-of-range 
    check-error-tag
  #( 0 -1 1 1 ) :length 11 make-env <'> src-channel 'out-of-range
    check-error-tag
  #( 0 0 1 1 ) :length 11 make-env <'> src-sound 'out-of-range check-error-tag
  #( 0 1 1 0 ) :length 11 make-env <'> src-sound 'out-of-range check-error-tag
  #( 0 1 1 -1 ) :length 11 make-env <'> src-sound 'out-of-range check-error-tag
  #( 0 -1 1 1 ) :length 11 make-env <'> src-sound 'out-of-range check-error-tag
  0.0 0.0 0.0 0.0 0.0 0.0 0.0 <'> make-readin 'mus-error check-error-tag
  vct-3 32 <'> filter-sound 'out-of-range check-error-tag
  #( 0 0 1 1 ) 0 <'> filter-sound 'out-of-range check-error-tag
  ind 0 12345 0 <'> swap-channels 'no-such-sound check-error-tag
  vct( 0.1 0.2 0.3 ) -1 ind 0 #t "" <'> mix-vct 'no-such-sample check-error-tag
  8 0.0 make-vct 0 -123 <'> snd-spectrum 'out-of-range check-error-tag
  8 0.0 make-vct 0 0 <'> snd-spectrum 'out-of-range check-error-tag
  "/baddy/hiho" <'> play 'no-such-file check-error-tag
  sf-dir "nist-shortpack.wav" $+ <'> play 'bad-sample-type check-error-tag
  ind 123 <'> make-player 'no-such-channel check-error-tag
  "/baddy/hiho" <'> mix 'no-such-file check-error-tag
  "oboe.snd" 0 2 <'> mix 'no-such-channel check-error-tag
  "/baddy/hiho" 0 <'> mix-sound 'no-such-file check-error-tag
  "/baddy/hiho.snd" <'> insert-sound 'no-such-file check-error-tag
  0 10 "/baddy/hiho.snd" <'> insert-samples 'no-such-file check-error-tag
  '() ind <'> set-filter-control-envelope 'no-data check-error-tag
  ind 123 <'> set-sample-type 'out-of-range check-error-tag
  ind 123 <'> set-header-type 'out-of-range check-error-tag
  ind 123 <'> set-selected-channel 'no-such-channel check-error-tag
  <'> noop 3 make-proc <'> set-search-procedure 'bad-arity check-error-tag
  <'> noop 3 make-proc <'> map-chan 'bad-arity check-error-tag
  <'> noop 1 make-proc ind 0 <'> set-cursor-style 'bad-arity check-error-tag
  0 0 1 1 ind 0 1234 <'> draw-line 'no-such-graphics-context check-error-tag
  ind 0 1234 <'> foreground-color 'no-such-graphics-context check-error-tag
  ind 0 1234 <'> current-font 'no-such-graphics-context check-error-tag
  '( vct-3 vct-3 ) ind 0 1234 0 1 0 <'> graph-data 'no-such-graphics-context
    check-error-tag
  100 ind 0 1234 <'> position->x 'no-such-axis check-error-tag
  100 ind 0 1234 <'> position->y 'no-such-axis check-error-tag
  100 ind 0 1234 <'> x->position 'no-such-axis check-error-tag
  100 ind 0 1234 <'> y->position 'no-such-axis check-error-tag
  ind 0 1234 <'> axis-info 'no-such-axis check-error-tag
  *with-test-gui* if
    channel-widgets car snd-gcs car "hiho" 0.0 1.0 -1.0 1.0 x-axis-in-seconds
      1234 <'> draw-axes 'out-of-range check-error-tag
    channel-widgets car snd-gcs car "hiho" 0.0 1.0 -1.0 1.0
      1234 <'> draw-axes 'out-of-range check-error-tag
    ind 1234 <'> axis-info 'no-such-channel check-error-tag
    1234 <'> axis-info 'no-such-sound check-error-tag
  then
  graph-once set-time-graph-type drop
  #( 0.1 -0.1 ) <'> set-x-bounds 'out-of-range check-error-tag
  100 0 <'> make-region 'out-of-range check-error-tag
  -1 <'> delete-sample 'no-such-sample check-error-tag
  ind framples 2* <'> delete-sample 'no-such-sample check-error-tag
  "/bad/baddy.snd" <'> play 'no-such-file check-error-tag
  1234 0 <'> play 'no-such-sound check-error-tag
  regions empty? if
    0 100 make-region
  then
  regions car 0 1234 <'> region-sample 'no-such-channel check-error-tag
  regions car 1234 <'> region-framples 'no-such-channel check-error-tag
  regions car 1234 <'> region-position 'no-such-channel check-error-tag
  "/bad/baddy.snd" <'> save-sound-as 'cannot-save check-error-tag
  0 1 1234 <'> transform-sample 'no-such-sound check-error-tag
  0 1 ind 1234 <'> transform-sample 'no-such-channel check-error-tag
  vct( 0 1 ) "hi" 0 1 0 1 1234 <'> graph 'no-such-sound check-error-tag
  vct( 0 1 ) "hi" 0 1 0 1 ind 1234 <'> graph 'no-such-channel check-error-tag
  #f #t set-selection-member? drop
  vct( 0 0 1 1 ) 4 <'> filter-selection 'no-active-selection check-error-tag
  "/bad/baddy.snd" <'> save-selection 'no-active-selection check-error-tag
  #( 0 0 1 1 ) <'> env-selection 'no-active-selection check-error-tag
  0 100 ind 0 make-region drop
  "/bad/baddy.snd" <'> save-selection 'cannot-save check-error-tag
  regions car "/bad/baddy.snd" <'> save-region 'cannot-save check-error-tag
  0 12 1234 #t <'> make-region 'no-such-sound check-error-tag
  #t ind set-read-only drop
  \ XXX: snd-nogui and set-read-only
  \ Snd-nogui has no widget and therefore the sound will not be
  \ write-protected according to snd-snd.c.
  ind read-only if
    ind '( 0 0 1 1 ) <'> set-sound-loop-info 'cannot-save check-error-tag
  else
    *with-test-nogui* unless
      "%s is not read-only?" #( ind ) snd-display
    then
  then
  0 ind 0 123 <'> make-sampler 'no-such-direction check-error-tag
  0 ind 0 0 <'> make-sampler 'no-such-direction check-error-tag
  0 ind 0 -2 <'> make-sampler 'no-such-direction check-error-tag
  '() <'> scale-by 'no-data check-error-tag
  '() <'> scale-to 'no-data check-error-tag
  -999 ind 0 <'> set-selection-position 'no-such-sample check-error-tag
  -999 ind 0 <'> set-selection-framples 'wrong-type-arg check-error-tag
  0 ind 0 <'> set-selection-framples 'wrong-type-arg check-error-tag
  -1 <'> edit-fragment 'no-such-edit check-error-tag
  101 ind 0 <'> edit-fragment 'no-such-edit check-error-tag
  ind 0 -2 <'> edit-tree 'no-such-edit check-error-tag
  ind 0 101 <'> edit-tree 'no-such-edit check-error-tag
  -1 <'> add-mark 'no-such-sample check-error-tag
  framples 2* <'> add-mark 'no-such-sample check-error-tag
  "/bad/baddy" <'> convolve-with 'no-such-file check-error-tag
  "/bad/baddy" <'> mix 'no-such-file check-error-tag
  ind 0 123 <'> swap-channels 'no-such-sound check-error-tag
  123 ind 0 <'> set-show-axes 'out-of-range check-error-tag
  -123 ind 0 <'> set-show-axes 'out-of-range check-error-tag
  123 ind 0 <'> set-x-axis-style 'out-of-range check-error-tag
  -123 ind 0 <'> set-x-axis-style 'out-of-range check-error-tag
  123 ind 0 <'> set-graph-style 'out-of-range check-error-tag
  -123 ind 0 <'> set-graph-style 'out-of-range check-error-tag
  '( 0 0 1 1 ) 0 #f -1.5 <'> env-sound 'out-of-range check-error-tag
  0.0 1.0 -1.6 <'> xramp-channel 'out-of-range check-error-tag
  0 2 -1 <'> set-samples 'wrong-type-arg check-error-tag
  '( 0 ) <'> left-sample 'wrong-type-arg check-error-tag
  '( 0 ) <'> amp-control 'wrong-type-arg check-error-tag
  '( 0 ) <'> sound-loop-info 'wrong-type-arg check-error-tag
  123 '( 0 ) <'> add-mark 'wrong-type-arg check-error-tag
  '( 0 0 1 1 ) 100 #f #f 1234 0 <'> filter-channel 'no-such-sound
    check-error-tag
  '( 0 0 1 1 ) 100 #f #f ind 1  <'> filter-channel 'no-such-channel 
    check-error-tag
  vct( 0 0 1 1 ) 4 #f #f ind 1 <'> filter-channel 'no-such-channel
    check-error-tag
  vct( 0 0 1 1 ) 0 <'> filter-sound 'out-of-range check-error-tag
  vct( 0 0 1 1 ) 10 <'> filter-sound 'out-of-range check-error-tag
  selected-sound 0 :stop <'> noop 0 make-proc <'> play 'bad-arity
    check-error-tag
  '( 0.1 0.01 ) ind <'> set-reverb-control-length-bounds 'out-of-range
    check-error-tag
  '( 0.1 0.01 ) ind <'> set-reverb-control-scale-bounds 'out-of-range
    check-error-tag
  #f <'> scale-by 'wrong-type-arg check-error-tag
  3.0 1.0 #t <'> src-sound 'wrong-type-arg check-error-tag
  3.0 1.0 ind #t <'> src-sound 'wrong-type-arg check-error-tag
  ind 0 123 <'> display-edits 'no-such-edit check-error-tag
  ind 0 123 <'> marks 'no-such-edit check-error-tag
  "test.snd" :edit-position 123 <'> save-sound-as 'no-such-edit check-error-tag
  "1a.snd" 0 0 ind 0 0 123 <'> insert-sound 'no-such-auto-delete-choice
    check-error-tag
  ind close-sound drop
  "hiho" "time" 0 1 <'> noop 0 make-proc <'> add-transform 'bad-arity 
    check-error-tag
  "/bad/baddy" <'> save-state 'cannot-save check-error-tag
  1234 "hi" <'> noop 0 make-proc <'> add-to-menu 'no-such-menu check-error-tag
  "hi" <'> noop 2 make-proc <'> add-to-main-menu 'bad-arity check-error-tag
  1 "hi" <'> noop 2 make-proc <'> add-to-menu 'bad-arity check-error-tag
  '( 0 1 ) "hiho" <'> help-dialog 'wrong-type-arg check-error-tag
  '( 0 1 ) "hiho" <'> info-dialog 'wrong-type-arg check-error-tag
  1234 <'> edit-header-dialog 'no-such-sound check-error-tag
  "/bad/baddy.snd" <'> open-sound 'no-such-file check-error-tag
  "/bad/baddy.snd" 1 22050 mus-lshort <'> open-raw-sound 'no-such-file
    check-error-tag
  "/bad/baddy.snd" <'> view-sound 'no-such-file check-error-tag
  0 "/bad/baddy.snd" <'> make-sampler 'no-such-file check-error-tag
  -1 0 #f <'> bind-key 'no-such-key check-error-tag
  12 17 #f <'> bind-key 'no-such-key check-error-tag
  12 -1 #f <'> bind-key 'no-such-key check-error-tag
  12345678 0 <'> key-binding 'no-such-key check-error-tag
  -1 0 <'> key-binding 'no-such-key check-error-tag
  12 17 <'> key-binding 'no-such-key check-error-tag
  12 -1 <'> key-binding 'no-such-key check-error-tag
  sf-dir "bad_chans.snd" $+ 0 0 123 123 0.0 make-vct <'> file->array 
    'bad-header check-error-tag
  sf-dir "bad_chans.snd" $+ <'> make-readin 'bad-header check-error-tag
  30 3 0 make-vct <'> make-iir-filter 'mus-error check-error-tag
  :size 2 30 f** f>s <'> make-wave-train 'out-of-range check-error-tag
  0.0 <'> set-mus-srate 'out-of-range check-error-tag
  -1000 <'> set-mus-srate 'out-of-range check-error-tag
  3 0 make-vct 3 0 make-vct -1 <'> dot-product 'out-of-range check-error-tag
  3 :initial-element 0.0 :initial-contents vct( 0.1 0.2 0.3 ) <'> make-delay
    'out-of-range check-error-tag
  3 :max-size 100 :initial-contents vct( 0.1 0.2 0.3 ) <'> make-delay
    'out-of-range check-error-tag
  :size 100 :wave 3 0 make-vct <'> make-table-lookup 'out-of-range
    check-error-tag
  :size 100 :wave 3 0 make-vct <'> make-wave-train 'out-of-range check-error-tag
  100 12345678 <'> make-ssb-am 'out-of-range check-error-tag
  :envelope '( 0 0 1 1 ) :distribution 10 0 make-vct <'> make-rand 
    'mus-error check-error-tag
  :envelope '( 0 0 1 ) <'> make-rand 'mus-error check-error-tag
  :envelope '( 0 0 1 1 ) :size -2 <'> make-rand 'out-of-range check-error-tag
  :envelope '( 0 0 1 1 ) :size 1234567890 <'> make-rand 'out-of-range 
    check-error-tag
  make-granulate #f <'> noop 3 make-proc <'> granulate 'bad-arity
    check-error-tag
  make-phase-vocoder #f <'> noop 0 make-proc <'> phase-vocoder 'bad-arity
    check-error-tag
  make-phase-vocoder #f #f <'> noop 0 make-proc <'> phase-vocoder 'bad-arity
    check-error-tag
  make-phase-vocoder #f #f #f <'> noop 0 make-proc <'> phase-vocoder 'bad-arity
    check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 <'> mus-xcoeff 'mus-error 
    check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 <'> mus-ycoeff 'mus-error
    check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 1.0 <'> set-mus-xcoeff
    'mus-error check-error-tag
  3 :xcoeffs vct-3 :ycoeffs vct-3 make-filter 4 1.0 <'> set-mus-ycoeff
    'mus-error check-error-tag
  :ycoeffs 4 0 make-vct :order 12 <'> make-filter 'mus-error check-error-tag
  \ XXX: Switch to float here okay according to clm2xen.c!
  \ was: make-oscil 1 ==> wrong-type-arg fixnum, wanted a float
  make-oscil 1.0 <'> set-mus-offset 'mus-error check-error-tag
  make-oscil 1 <'> set-mus-offset 'wrong-type-arg check-error-tag
  :channels 2 30 f** f>s <'> make-locsig 'out-of-range check-error-tag
  :width 3000 <'> make-src 'out-of-range check-error-tag
  *with-test-gui* if
    "baddy" <'> noop 0 make-proc <'> add-colormap 'bad-arity check-error-tag
    "baddy" <'> noop 3 make-proc <'> add-colormap 'bad-arity check-error-tag
  then
  :input <'> noop 1 make-proc make-src 2000000.0 <'> src 'out-of-range
    check-error-tag
  '( 1 1 ) -1 <'> partials->polynomial 'out-of-range check-error-tag
  '( 1 1 ) 3 <'> partials->polynomial 'out-of-range check-error-tag
  :partials '( 1 1 ) :kind -1 <'> make-polyshape 'out-of-range check-error-tag
  :partials '( 1 1 ) :kind 3 <'> make-polyshape 'out-of-range check-error-tag
  32 <'> normalize-partials 'wrong-type-arg check-error-tag
  '() <'> normalize-partials 'wrong-type-arg check-error-tag
  '( 1 2 3 ) <'> normalize-partials 'bad-type check-error-tag
  vct( 3 ) <'> normalize-partials 'bad-type check-error-tag
  440.0 :partials vct( 1 1 -2 1 ) <'> make-polyshape 'no-data check-error-tag
  440.0 :partials list( 1 1 -2 1 ) <'> make-polyshape 'no-data check-error-tag
  440.0 :partials '() <'> make-polyshape 'no-data check-error-tag
  1234 <'> set-mus-header-raw-defaults 'wrong-type-arg check-error-tag
  '( 44100 2.123 "hi" ) <'> set-mus-header-raw-defaults 'wrong-type-arg
    check-error-tag
  123 <'> set-with-toolbar 'wrong-type-arg check-error-tag
  123 <'> set-with-tooltips 'wrong-type-arg check-error-tag
  123 <'> set-with-menu-icons 'wrong-type-arg check-error-tag
  123 <'> set-save-as-dialog-src 'wrong-type-arg check-error-tag
  123 <'> set-save-as-dialog-auto-comment 'wrong-type-arg check-error-tag
  123 <'> set-with-smpte-label 'wrong-type-arg check-error-tag
  123 <'> set-ask-about-unsaved-edits 'wrong-type-arg check-error-tag
  sounds empty? unless
    "sounds after error checks: %s" #( sounds ) snd-display
  then
  mus-audio-reinitialize drop
  10 set-window-y drop
  dismiss-all-dialogs
  "test.snd" file-exists? if
    "test.snd" 0o644 file-chmod
    "test.snd" file-delete
  then
  "oboe.snd" "test.snd" file-copy
  "test.snd" open-sound to ind
  "test.snd" file-delete
  ind <'> update-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'cant-update-file <> if
      "update-sound after deletion: %s" #( tag ) snd-display
    then
  then
  ind <'> save-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'cannot-save <> if
      "save file deleted: %s" #( tag ) snd-display
    then
  then
  ind close-sound drop
  \
  "oboe.snd" "test.snd" file-copy
  "test.snd" open-sound to ind
  select-all drop
  "test.snd" file-delete
  view-regions-dialog drop
  ind close-sound drop
  dismiss-all-dialogs
  \
  "oboe.snd" "test.snd" file-copy
  "test.snd" open-sound to ind
  "test.snd" 0o400 file-chmod
  10 delete-sample drop
  ind <'> save-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'cannot-save <> if
      "save protected sound msg: %s" #( tag ) snd-display
    then
  then
  ind close-sound drop
  \
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  "oboe.snd" "test.snd" file-copy
  "test.snd" 0o200 file-chmod
  "test.snd" <'> open-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'no-such-file =
    tag car 'mus-error    = || unless
      "open read-protected sound worked!: %s" #( tag ) snd-display
    then
  then
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  ind sound? if
    ind close-sound drop
  then
  \
  "oboe.snd" "test.snd" file-copy
  "test.snd" 0o400 file-chmod
  "test.snd" open-sound to ind
  10 delete-sample drop
  "test.snd" <'> save-sound-as #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'cannot-save <> if
      "save-as write-protected sound msg: %s" #( tag ) snd-display
    then
  then
  ind close-sound drop
  "test.snd" 0o644 file-chmod
  "test.snd" file-delete
  \
  close-sound-mc-cb <'> map-channel #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'no-such-channel <> if
      "map-channel closing own chan: %s" #( tag ) snd-display
    then
  then
  \
  close-sound-aoe-1-cb  "snd-test"   as-one-edit drop
  close-sound-aoe-2a-cb "outer-edit" as-one-edit drop
  close-sound-fc-cb find-channel drop
  \
  "oboe.snd" open-sound to ind
  0 make-sampler { rd }
  ind close-sound drop
  10 0 do
    rd read-sample drop
  loop
  rd sampler-home { home }
  home array-length 0> if
    home 0 array-ref sound? if
      "reader-home of closed sound: %s %s?" #( home sounds ) snd-display
    then
  then
  rd sampler-position { loc }
  loc 0<> if
    "closed reader position: %s?" #( loc ) snd-display
  then
  rd sampler-at-end? { at-end }
  at-end false? if
    "closed sampler at end: %s?" #( at-end ) snd-display
  then
  \
  "oboe.snd" open-sound to ind
  vct( 0.1 0.2 0.3 ) mix-vct { mx }
  mx make-mix-sampler to rd
  ind close-sound drop
  10 0 do
    rd read-mix-sample drop
  loop
  \
  8 max-regions max set-max-regions drop
  "oboe.snd" open-sound to ind
  0 100 ind 0 make-region { reg }
  reg 0 make-region-sampler to rd
  ind close-sound drop
  reg forget-region drop
  10 0 do
    rd read-sample drop
  loop
  \
  "oboe.snd" open-sound to ind
  100 0.5 ind 0 set-sample drop
  100 ind 0 sample { s100 }
  ind mc-2-cb map-channel drop
  100 ind 0 sample to s100
  s100 0.5 fneq if
    "map + reset framples: %s" #( s100 ) snd-display
  then
  ind 0 framples { frms }
  frms 50828 <> if
    "map + reset framples, framples: %s" #( frms ) snd-display
  then
  1 ind 0 undo drop
  ind 0 framples to frms
  frms 1 <> if
    "map + reset framples, undo framples: %s" #( frms ) snd-display
  then
  ind revert-sound drop
  \
  100 0.5 ind 0 set-sample drop
  \ XXX: 'wrong-type-arg instead of 'wrong-number-of-args
  \
  \ (set! (framples ind 0 1) 1) => too many arguments
  \ 1 ind 0 1 set-framples => wrong type arg 1 (<sound 0>)
  \
  \ With Fth this doesn't work as expected.  If stack has more values than
  \ needed by the next word, no 'wrong-number-of-args exception can be
  \ raised because no one knows who will take the other values.  That's
  \ why the first exception is 'wrong-type-arg because sound IND is not
  \ a number.
  \
  \ framples ( snd chn edpos -- frms ) /* g_framples(snd, chn, edpos) */
  \ set-framples ( on snd chn -- val ) /* g_set_framples(on, snd, chn) */
  1 ind 0 1 <'> set-framples #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'wrong-number-of-args <>
    tag car 'wrong-type-arg       <> && if
      "set framples + edpos: %s" #( tag ) snd-display
    then
  then
  ind revert-sound drop
  *with-test-complex* if
    <'> mc-3-cb <'> map-channel #t nil fth-catch to tag
  then
  stack-reset
  tag if
    tag car 'bad-type <> if
      "map-channel rtn complex: %s" #( tag ) snd-display
    then
  then
  0 make-sampler to rd
  10 0 do
    rd #() apply drop
  loop
  rd copy-sampler { crd }
  ind close-sound drop
  10 0 do
    crd read-sample drop
  loop
  crd sampler-home to home
  home array-length 0> if
    home 0 array-ref sound? if
      "copy reader-home of closed sound: %s %s?" #( home sounds ) snd-display
    then
  then
  crd sampler-position to loc
  loc 0<> if
    "closed copy reader position: %s?" #( loc ) snd-display
  then
  crd sampler-at-end? to at-end
  at-end false? if
    "closed copy sampler at end: %s?" #( at-end ) snd-display 
  then
  \
  ind <'> revert-sound #t nil fth-catch to tag
  stack-reset
  tag if
    tag car 'no-such-sound <> if
      "revert-sound of closed sound: %s" #( tag ) snd-display
    then
  then
  \
  set-procs04 length 2 = if
    "(%s %s)" set-procs04
  else
    "%s" #( set-procs04 )
  then string-format { set04fncs }
  procs10 length 2 = if
    "(%s %s)" procs10
  else
    "%s" #( procs10 )
  then string-format { 10fncs }
  *snd-test-verbose* if
    "procs   prcs/set-prcs" #f snd-test-message
    "=====================" #f snd-test-message
    "procs00: %3d/%3d" #( procs00 length set-procs00 length ) snd-test-message
    "procs01: %3d/%3d" #( procs01 length set-procs01 length ) snd-test-message
    "procs02: %3d/%3d" #( procs02 length set-procs02 length ) snd-test-message
    "procs03: %3d/%3d" #( procs03 length set-procs03 length ) snd-test-message
    set-procs04 length 10 <= if
      "procs04: %3d/%3d %s"
        #( procs04 length set-procs04 length set04fncs ) snd-test-message
    else
      "procs04: %3d/%3d" #( procs04 length set-procs04 length ) snd-test-message
    then
    "procs05: %3d" #( procs05 length ) snd-test-message
    "procs06: %3d" #( procs06 length ) snd-test-message
    "procs07: %3d" #( procs07 length ) snd-test-message
    "procs08: %3d" #( procs08 length ) snd-test-message
    procs10 length 10 <= if
      "procs10: %3d %s"  #( procs10 length 10fncs ) snd-test-message
    else
      "procs10: %3d"  #( procs10 length ) snd-test-message
    then
  then
  #( 1.5 #( 0 1 ) 1234 #t ) { random-args }
  #( 1.5 #( 0 1 ) 1234 vct-3 color-95 0+i delay-32 :feedback #f ) { main-args }
  #( 1.5 #( 0 1 ) 1234 0+i delay-32 #t ) { few-args }
  #( 1.5 vct-3 0+i ) { fewer-args }
  all-args if
    main-args
  else
    few-args
  then { less-args }
  nil nil nil nil nil nil nil { arg1 arg2 arg3 arg4 tm prc tag }
  gc-run
  "keyargs-2-args" check-args-progress-info
  keyargs each to arg1
    random-args each to arg2
      make-procs each to prc
        arg1 arg2 prc #t nil fth-catch stack-reset
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  all-args if
    "keyargs-3-args" check-args-progress-info
    random-args each to arg1
      keyargs each to arg2
        random-args each to arg3
          make-procs each to prc
            arg1 arg2 arg3 prc #t nil fth-catch stack-reset
          end-each
        end-each
      end-each
    end-each
    dismiss-all-dialogs
    gc-run
    "keyargs-4-args" check-args-progress-info
    keyargs each to arg1
      random-args each to arg2
        keyargs each to arg3
          random-args each to arg4
            make-procs each to prc
              arg1 arg2 arg3 arg4 prc #t nil fth-catch stack-reset
            end-each
          end-each
        end-each
      end-each
    end-each
    dismiss-all-dialogs
    gc-run
  then
  "no-args" check-args-progress-info
  procs00 each to prc
    prc #t nil fth-catch to tag
    stack-reset
    tag car 'wrong-number-of-args = if
      "procs00: %s %s" #( prc tag ) snd-display
    then
  end-each
  dismiss-all-dialogs
  gc-run
  "set-no-args" check-args-progress-info
  main-args each to arg
    set-procs00 each to prc
      arg prc set-xt #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-number-of-args = if
        "set-procs00: (%s) %s %s" #( arg prc tag ) snd-display
      then
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "1-arg" check-args-progress-info
  nil { arg }
  main-args each to arg
    procs01 each to prc
      arg prc #t nil fth-catch to tag
      stack-reset
      tag car 'wrong-number-of-args = if
        "procs01 wna: (%s) %s %s" #( arg prc tag ) snd-display
      then
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "set-1-arg" check-args-progress-info
  main-args each to arg1
    main-args each to arg2
      set-procs01 each to prc
        prc proc-name "widget-size" string<> if
          arg1 arg2 prc set-xt #t nil fth-catch to tag
          stack-reset
          tag car 'wrong-number-of-args = if
            "set-procs01: (%s %s) %s %s" #( arg1 arg2 prc tag ) snd-display
          then
        then
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "2-args" check-args-progress-info
  main-args each to arg1
    main-args each to arg2
      procs02 each to prc
        arg1 arg2 prc #t nil fth-catch to tag
        stack-reset
        tag car 'wrong-number-of-args = if
          "procs02: (%s %s) %s %s" #( arg1 arg2 prc tag ) snd-display
        then
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "set-2-args" check-args-progress-info
  less-args each to arg1
    less-args each to arg2
      less-args each to arg3
        set-procs02 each to prc
          arg1 arg2 arg3 prc set-xt #t nil fth-catch to tag
          stack-reset
          tag car 'wrong-number-of-args = if
            "set-procs02: (%s %s %s) %s %s"
              #( arg1 arg2 arg3 prc tag ) snd-display
          then
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  nil nil nil nil nil nil { arg5 arg6 arg7 arg8 arg9 arg0 }
  "3-args" check-args-progress-info
  less-args each to arg1
    less-args each to arg2
      less-args each to arg3
        procs03 each to prc
          arg1 arg2 arg3 prc #t nil fth-catch to tag
          stack-reset
          tag car 'wrong-number-of-args = if
            "procs03: (%s %s %s) %s %s" #( arg1 arg2 arg3 prc tag ) snd-display
          then
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "set-3-args" check-args-progress-info
  less-args each to arg1
    less-args each to arg2
      less-args each to arg3
        less-args each to arg4
          set-procs03 each to prc
            arg1 arg2 arg3 arg4 prc #t nil fth-catch to tag
            stack-reset
            tag car 'wrong-number-of-args = if
              "set-procs03: (%s %s %s %s) %s %s"
                #( arg1 arg2 arg3 arg4 prc tag ) snd-display
            then
          end-each
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "4-args" check-args-progress-info
  few-args each to arg1
    few-args each to arg2
      few-args each to arg3
        few-args each to arg4
          procs04 each to prc
            arg1 arg2 arg3 arg4 prc #t nil fth-catch to tag
            stack-reset
            tag car 'wrong-number-of-args = if
              "procs04: (%s %s %s %s) %s %s"
                #( arg1 arg2 arg3 arg4 prc tag ) snd-display
            then
          end-each
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "set-4-args" check-args-progress-info
  few-args each to arg1
    few-args each to arg2
      few-args each to arg3
        few-args each to arg4
          few-args each to arg5
            set-procs04 each to prc
              arg1 arg2 arg3 arg4 arg5 prc #t nil fth-catch to tag
              stack-reset
              tag car 'wrong-number-of-args = if
                "set-procs04: (%s %s %s %s %s) %s %s"
                  #( arg1 arg2 arg3 arg4 arg5 prc tag ) snd-display
              then
            end-each
          end-each
        end-each
      end-each
    end-each
  end-each
  stop-playing drop
  dismiss-all-dialogs
  gc-run
  "5-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
        fewer-args each to arg4
          fewer-args each to arg5
            procs05 each to prc
              arg1 arg2 arg3 arg4 arg5 prc #t nil fth-catch to tag
              stack-reset
              tag car 'wrong-number-of-args = if
                "procs05: (%s %s %s %s %s) %s %s"
                  #( arg1 arg2 arg3 arg4 arg5 prc tag ) snd-display
              then
            end-each
          end-each
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "6-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
        fewer-args each to arg4
          fewer-args each to arg5
            fewer-args each to arg6
                procs06 each to prc
                  arg1 arg2 arg3 arg4 arg5 arg6 prc #t nil fth-catch to tag
                  stack-reset
                  tag car 'wrong-number-of-args = if
                    "procs06: (%s %s %s %s %s %s) %s %s"
                      #( arg1 arg2 arg3 arg4 arg5 arg6 prc tag ) snd-display
                  then
              end-each
            end-each
          end-each
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "8-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
        fewer-args each to arg4
          fewer-args each to arg5
            fewer-args each to arg6
              fewer-args each to arg7
                fewer-args each to arg8
                  procs08 each to prc
                    arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 prc #t nil
                      fth-catch to tag
                    stack-reset
                    tag car 'wrong-number-of-args = if
                      "procs08: (%s %s %s %s %s %s %s %s) %s %s"
                        #( arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 prc tag )
                        snd-display
                    then
                  end-each
                end-each
              end-each
            end-each
          end-each
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  "10-args" check-args-progress-info
  fewer-args each to arg1
    fewer-args each to arg2
      fewer-args each to arg3
        fewer-args each to arg4
          fewer-args each to arg5
            fewer-args each to arg6
              fewer-args each to arg7
                fewer-args each to arg8
                  fewer-args each to arg9
                    fewer-args each to arg0
                      procs10 each to prc
                        arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg0
                          prc #t nil fth-catch
                        to tag
                        stack-reset
                        tag car 'wrong-number-of-args = if
                          "procs10: (%s %s %s %s %s %s %s %s %s %s) %s %s"
                          #( arg1 arg2 arg3 arg4 arg5 arg6 arg7 
                             arg8 arg9 arg0 prc tag ) snd-display
                        then
                      end-each
                    end-each
                  end-each
                end-each
              end-each
            end-each
          end-each
        end-each
      end-each
    end-each
  end-each
  dismiss-all-dialogs
  gc-run
  #f set-ask-about-unsaved-edits drop
;

: 30-test
  \ from clm-ins.fs
  <'> test23-balance
  :comment  over object->string
  :srate    22050
  :channels 3 with-sound ws-output 0 find-sound { ind }
  ind sound? if
    ind close-sound drop
  else
    "with-sound balance?" snd-display
  then
  \ 0.0 0.3 <'> clm-ins-test
  \ :comment  over object->string
  \ :notehook *snd-test-ws-verbose* if <'> test23-notehook else #f then
  \ :channels 2 with-sound ws-test-close-sound
;

let: ( -- )
  #() { numbs }
  script-arg 0> if
    script-args length script-arg 1+ ?do
      script-args i list-ref string->number { n }
      script-arg 1+ set-script-arg drop
      n 0< if
        numbs n array-push to numbs \ negative number means exclude this test
      else
        test-numbers n array-push to test-numbers
      then
    loop
  then
  test-numbers empty? if
    29 -1 do
      test-numbers i array-push to test-numbers
    loop
  then
  numbs each abs { n }
    test-numbers test-numbers n array-index array-delete! drop
  end-each
  .stack
  start-snd-test
  <'> 00-constants       run-fth-test
  <'> 01-defaults        run-fth-test
  mus-ldouble set-default-output-sample-type drop
  <'> 02-headers         run-fth-test
  <'> 03-variables       run-fth-test
  <'> 04-sndlib          run-fth-test
  <'> 05-simple-check    run-fth-test
  <'> 08-clm             run-fth-test
  <'> 10-marks           run-fth-test
  <'> 15-chan-local-vars run-fth-test
  <'> 19-save/restore    run-fth-test
  <'> 23-with-sound      run-fth-test
  <'> 27-sel-from-snd    run-fth-test
  <'> 28-errors          run-fth-test
  <'> 30-test            run-fth-test  \ local fragment test
  .stack
  finish-snd-test
  0 snd-exit drop
;let

\ snd-test.fs ends here
