\ snd-forth-docs.fs -- examples from sndclm.html

\ Usage: snd-forth-nogui -noinit snd-forth-docs.fs

require clm
require examp

\ OSCIL
lambda: ( -- )
  440.0 make-oscil { gen }
  44100 0 do
    i  gen 0 0 oscil  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ ENV
lambda: ( -- )
  440.0 make-oscil { gen }
  '( 0 0 0.01 1 0.25 0.1 0.5 0.01 1 0 )
  :scaler 0.5 :length 44100 make-env { ampf }
  44100 0 do
    i  gen 0 0 oscil  ampf env  f* *output*  outa drop
  loop
; :play #t with-sound drop

\ TABLE-LOOKUP
lambda: ( -- )
  440.0 :wave '( 1 0.5  2 0.5 ) #f #f partials->wave make-table-lookup { gen }
  44100 0 do
    i  gen 0 table-lookup  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ POLYWAVE
lambda: ( -- )
  440.0 :partials '( 1 0.5 2 0.5 ) make-polywave { gen }
  44100 0 do
    i  gen 0 polywave  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ TRIANBLE-WAVE
lambda: ( -- )
  440.0 make-triangle-wave { gen }
  44100 0 do
    i  gen 0 triangle-wave  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ NCOS
lambda: ( -- )
  440.0 10 make-ncos { gen }
  44100 0 do
    i  gen 0 ncos  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ NRXYCOS
lambda: ( -- )
  440.0 :n 10 make-nrxycos { gen }
  44100 0 ?do
    i  gen 0 nrxycos  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ SSB-AM
lambda: ( -- )
  440.0 20 make-ssb-am { shifter }
  440.0 make-oscil { osc }
  44100 0 ?do
    i  shifter  osc 0 0 oscil  0 ssb-am f2/ *output* outa drop
  loop
; :play #t :statistics #t :srate 44100 with-sound drop

\ WAVE-TRAIN
lambda: ( -- )
  400 10 make-ncos { g }
  g -0.5 pi f* set-mus-phase drop
  64 make-vct map! g 0 ncos end-map { v }
  440.0 :wave v make-wave-train { gen }
  44100 0 do
    i  gen 0 wave-train  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ RAND
lambda: ( -- )
  5.0 220.0 hz->radians make-rand { ran1 }
  5.0 330.0 hz->radians make-rand-interp { ran2 }
   440.0 make-oscil { osc1 }
  1320.0 make-oscil { osc2 }
  88200 0 do
    i  osc1  ran1 0 rand         0 oscil  f2/ *output* outa drop
    i  osc2  ran2 0 rand-interp  0 oscil  f2/ *output* outb drop
  loop
; :channels 2 :play #t with-sound drop

\ TWO-POLE
lambda: ( -- )
  1000.0 0.999 make-two-pole { flt }
  10000.0 0.002 make-rand { ran1 }
  44100 0 do
    i  flt  ran1 0 rand  two-pole  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ FIRMANT
lambda: ( -- )
  1000.0 0.999 make-firmant { flt }
  10000.0 5.0 make-rand { ran1 }
  44100 0 do
    i  flt  ran1 0 rand  #f firmant  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ IIR-FILTER
lambda: ( -- )
  3 vct( 0.0 -1.978 0.998 ) make-iir-filter { flt }
  10000.0 0.002 make-rand { ran1 }
  44100 0 do
    i  flt  ran1 0 rand  iir-filter  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ DELAY
lambda: ( -- )
  0.5 seconds->samples make-delay { dly }
  440.0 make-oscil { osc1 }
  660.0 make-oscil { osc2 }
  44100 0 do
    i
    osc1 0 0 oscil
    dly  osc2 0 0 oscil  0 delay f+
    f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ COMB
lambda: ( -- )
  0.4 0.4 seconds->samples make-comb { cmb }
  440.0 make-oscil { osc }
  '( 0 0 1 1 2 1 3 0 ) :length 4410 make-env { ampf }
  88200 0 do
    i
    cmb ( gen )
    ampf env  osc 0 0 oscil  f* ( val )
    0 ( pm )
    comb f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ ALL-PASS
lambda: ( -- )
  -0.4 0.4 0.4 seconds->samples make-all-pass { alp }
  440.0 make-oscil { osc }
  '( 0 0 1 1 2 1 3 0 ) :length 4410 make-env { ampf }
  88200 0 do
    i
    alp ( gen )
    ampf env  osc 0 0 oscil  f* ( val )
    0 ( pm )
    all-pass f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ MOVING-AVERAGE
lambda: ( -- )
  4410 make-moving-average { avg }
  440.0 make-oscil { osc }
  44100 4410 - { stop }
  0.0 { val }
  stop 0 do
    osc 0 0 oscil to val
    i  avg val fabs moving-average  val f* *output* outa drop
  loop
  44100 stop do
    i  avg 0.0 moving-average  osc 0 0 oscil f*  *output* outa drop
  loop
; :play #t with-sound drop

\ SRC1
lambda: ( -- )
  "oboe.snd" make-readin { rd }
  rd 0.5 make-src { sr }
  "oboe.snd" mus-sound-framples 2* ( len ) 0 do
    i  sr 0 #f src  *output* outa drop
  loop
; :play #t :srate 22050 with-sound drop

\ SRC2
: make-src-proc { osc -- prc; dir self -- val }
  1 proc-create osc , ( prc )
 does> { dir self -- val }
  self @ ( osc ) 0 0 oscil
;

lambda: ( -- )
  440.0 make-oscil { osc }
  osc make-src-proc { prc }
  :srate 2.0 make-src { sr }
  44100 0 do
    i  sr 0 prc src  *output* outa drop
  loop
; :play #t with-sound drop

\ CONVOLVE1
lambda: ( -- )
  "pistol.snd" make-readin ( rd )
  "oboe.snd" file->vct ( v ) make-convolve { cnv }
  88200 0 do
    i  cnv #f convolve  0.25 f* *output* outa drop
  loop
; :play #t :statistics #t with-sound drop

\ CONVOLVE2
lambda: ( -- )
  "oboe.snd" "pistol.snd" 0.5 "convolved.snd" convolve-files { tempfile }
  tempfile make-readin { reader }
  tempfile mus-sound-framples ( len ) 0 do
    i  reader readin  *output* outa drop
  loop
  tempfile file-delete
; :play #t with-sound drop

\ GRANULATE1
lambda: ( -- )
  "oboe.snd" make-readin 2.0 make-granulate { grn }
  44100 0 do
    i  grn #f #f granulate  *output* outa drop
  loop
; :play #t with-sound drop

\ GRANULATE2
: make-granulate-proc { osc sweep -- prc; dir self -- val }
  1 proc-create osc , sweep , ( prc )
 does> { dir self -- val }
  self @ ( osc )  self cell+ @ ( sweep ) env  0 oscil  0.2 f*
;

lambda: ( -- )
  440.0 make-oscil { osc }
  '( 0 0 1 1 ) :scaler 440.0 hz->radians :length 44100 make-env { sweep }
  osc sweep make-granulate-proc :expansion 2.0 :length 0.5 make-granulate { grn }
  88200 0 do
    i  grn #f #f granulate  *output* outa drop
  loop
; :play #t with-sound drop

\ PHASE-VOCODER1
lambda: ( -- )
  "oboe.snd" make-readin :pitch 2.0 make-phase-vocoder { pv }
  44100 0 do
    i  pv #f #f #f #f phase-vocoder  *output* outa drop
  loop
; :play #t with-sound drop

\ PHASE-VOCODER2
lambda: ( -- )
  "oboe.snd" make-readin :interp 256 make-phase-vocoder { pv }
  "oboe.snd" mus-sound-framples 2* ( samps ) 0 do
    i  pv #f #f #f #f phase-vocoder  *output* outa drop
  loop
; :play #t :srate 22050 with-sound drop

\ ASYMMETRIC-FM
lambda: ( -- )
  440.0 0.0 0.9 0.5 make-asymmetric-fm { fm }
  44100 0 do
    i  fm 1.0 0 asymmetric-fm  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ FILE->FRAME->FILE
lambda: ( -- )
  "stereo.snd" make-file->frame { input }
  2 make-frame { frm }
  "stereo.snd" mus-sound-framples ( len ) 0 do
    input i frm file->frame ( frm ) 1 frame-ref ( val1 )
    frm 0 frame-ref ( val0 ) frm 1 rot frame-set! drop
    ( val1 ) frm 0 rot frame-set! drop
    *output* i frm frame->file drop
  loop
; :channels 2 :play #t with-sound drop

\ READIN
lambda: ( -- )
  "oboe.snd" make-readin { reader }
  44100 0 do
    i  reader readin  f2/ *output* outa drop
  loop
; :play #t with-sound drop

\ IN-OUT-ANY
lambda: ( -- )
  "oboe.snd" make-file->sample { infile }
  44100 0 do
    i  i 0 infile in-any  0 *output* out-any drop
  loop
; :play #t with-sound drop

\ LOCSIG
lambda: ( -- )
  60.0 make-locsig { loc }
  440.0 make-oscil { osc }
  44100 0 do
    loc i  osc 0 0 oscil f2/  locsig drop
  loop
; :play #t :channels 2 with-sound drop

\ AMPLITUDE-MODULATE
lambda: ( -- )
  440.0 make-oscil { osc1 }
  220.0 make-oscil { osc2 }
  44100 0 do
    i
    0.3            ( car )
    osc1 0 0 oscil ( in1 )
    osc2 0 0 oscil ( in2 ) amplitude-modulate  f2/ *output* outa drop
  loop
; :play #t with-sound drop

bye

\ snd-forth-docs.fs ends here
