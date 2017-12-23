\ effects.fs -- *effects*.scm -> effects.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/10/16 23:04:30
\ Changed: 17/12/15 06:30:08
\
\ @(#)effects.fs	1.59 12/15/17

\ General (nogui/motif/gtk)
\
\ effects-squelch-channel	( amount gate-size :optional snd chn -- )
\ effects-echo			( is dtime eamt :optional beg dur snd chn -- )
\ effects-flecho		( scl secs ismps :optional beg dur snd chn -- )
\ effects-zecho			( scl secs freq amp ismps :optional ... -- )
\ effects-bbp			( freq bw :optional beg dur snd chn -- res )
\ effects-bbr			( freq bw :optional beg dur snd chn -- res )
\ effects-bhp			( freq :optional beg dur snd chn -- res )
\ effects-blp			( freq :optional beg dur snd chn -- res )
\ effects-comb-filter		( scl size :optional beg dur snd chn -- res )
\ effects-comb-chord		( scl si amp :optional beg dur snd chn -- res )
\ effects-moog			( freq Q :optional beg dur snd chn -- res )
\ moog				( freq Q -- prc; inval self -- res )
\ effects-am			( freq en :optional beg dur snd chn -- res )
\ effects-rm			( freq en :optional beg dur snd chn -- res )
\ effects-jc-reverb		( samps volume -- prc; inval self -- res )
\ effects-jc-reverb-1		( volume :optional beg dur snd chn -- res )
\ effects-cnv			( snd0 amp snd chn -- res )
\ effects-position-sound	( mono-snd pos :optional snd chn -- res )
\ effects-place-sound		( mono-snd stereo-snd pan-env -- res )
\ effects-flange		( at spd ti :optional beg dur snd chn -- res )
\ effects-cross-synthesis	( snd amp fftsize r -- prc; inval self -- res )
\ effects-cross-synthesis-1	( csnd amp fftsize r :optional beg dur ... -- )
\ effects-fp			( sf amp frq :optional beg dur snd chn -- vct )
\ effects-hello-dentist		( freq amp :optional beg dur snd chn -- res )
\ effects-remove-clicks		( :optional snd chn -- res )
\ effects-remove-dc		( :optional snd chn -- res )
\ effects-compand		( :optional snd chn -- res )
\
\ Motif/Gtk specific
\
\ Requires --with-motif|gtk
\
\ Tested with Snd 18.x
\             Fth 1.3.x
\             Motif 2.3.3 X11R6
\
\ make-menu			( name parent -- gen )
\ menu-entry			( gen prc disp-prc -- )
\ make-main-menu		( name -- widget )
\ add-to-effects-menu		( name prc -- )
\
\ make-gain-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-normalize-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-gate-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ 
\ make-echo-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-flecho-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-zecho-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\
\ make-band-pass-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-notch-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-high-pass-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-low-pass-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-comb-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-comb-chord-dialog	( name -- pc1 pc2; child self -- prc; self -- )
\ make-moog-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\
\ make-adsat-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-src-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-expsrc-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-src-timevar-dialog	( name -- pc1 pc2; child self -- prc; self -- )
\
\ make-am-effect-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-rm-effect-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\
\ make-reverb-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-jc-reverb-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-convolve-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\
\ make-place-sound-dialog	( name -- pc1 pc2; child self -- prc; self -- )
\ make-silence-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-contrast-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-cross-synth-dialog	( name -- pc1 pc2; child self -- prc; self -- )
\ make-flange-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-random-phase-dialog	( name -- pc1 pc2; child self -- prc; self -- )
\ make-robotize-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-rubber-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\ make-wobble-dialog		( name -- pc1 pc2; child self -- prc; self -- )
\
\ make-effects-menu		( -- )
\ init-effects-menu		( -- )

require clm
require examp
require env
require dsp

-1 value effects-menu			\ for prefs
#f value use-combo-box-for-fft-size

\ effects-squelch-channel ( amount gate-size :optional snd chn -- )

hide
: squelch-cb { f0 f1 amount -- prc; y self -- val }
	1 proc-create amount , f1 , f0 , ( prc )
  does> { y self -- val }
	self @ { amp }
	self 1 cells + @ { f1 }
	self 2 cells + @ { f0 }
	f1  f0 y y f* moving-average amp f< if
		0.0
	else
		1.0
	then  moving-average y f*
;
set-current

: effects-squelch-channel <{ amount gate-size :optional snd #f chn #f -- val }>
	:size gate-size make-moving-average { f0 }
	:size gate-size :initial-element 1.0 make-moving-average { f1 }
	"%s %s %s" #( amount gate-size get-func-name ) string-format { origin }
	f0 f1 amount squelch-cb 0 #f snd chn #f origin map-channel
;
previous

\ effects-echo ( in-samps delay-time echo-amount :optional beg dur snd chn -- )
\ effects-flecho ( scl secs input-samps :optional beg dur snd chn -- )
\ effects-zecho ( scl secs freq amp input-samps :optional beg dur snd chn -- )

hide
: effects-echo-cb { samps amp del -- prc; inval self -- res }
	1 proc-create 0 , del , amp , samps , ( prc )
  does> { inval self -- res }
	self @ 1+ dup self ! { samp }
	self cell+ @ { del }
	self 2 cells + @ { amp }
	self 3 cells + @ { samps }
	del dup 0.0 tap samp samps <= if
		inval f+
	then amp f* 0.0 delay inval f+
;

: effects-flecho-cb ( amp samps flt del -- prc; inval self -- res )
	{ amp samps flt del }
	1 proc-create 0 , samps , flt , del , amp , ( prc )
  does> { inval self -- res }
	self @ 1+ dup self ! { samp }
	self cell+ @ { samps }
	self 2 cells + @ { flt }
	self 3 cells + @ { del }
	self 4 cells + @ { scl }
	del flt del 0.0 tap samp samps <= if
		inval f+ 
	then scl f* fir-filter delay inval f+
;

: effects-zecho-cb ( scaler amp samps os del -- prc; inval self -- res )
	{ scaler amp samps os del }
	1 proc-create 0 , samps , os , del , scaler , amp , ( prc )
  does> { inval self -- res }
	self @ 1+ dup self ! { samp }
	self cell+ @ { samps }
	self 2 cells + @ { os }
	self 3 cells + @ { del }
	self 4 cells + @ { scl }
	self 5 cells + @ { amp }
	del
	del 0.0 tap samp samps <= if
		inval f+
	then scl f*
	os 0.0 0.0 oscil amp f*
	delay inval f+
;
set-current

: effects-echo
  <{ input-samps del-time amp :optional beg 0 dur #f snd #f chn #f -- res }>
	del-time snd srate f* fround->s make-delay { del }
	input-samps number? if
		input-samps
	else
		dur number? if
			dur
		else
			snd chn undef framples
		then
	then { samps }
	"%s %s %s %s %s %s" #( input-samps del-time amp beg dur get-func-name )
	    string-format { orig }
	samps amp del effects-echo-cb beg dur snd chn #f orig map-channel
;

: effects-flecho
  <{ amp secs input-samps :optional beg 0 dur #f snd #f chn #f -- res }>
	:order 4 :xcoeffs vct( 0.125 0.25 0.25 0.125 ) make-fir-filter { flt }
	secs snd srate f* fround->s make-delay { del }
	input-samps number? if
		input-samps
	else
		dur number? if
			dur
		else
			snd chn undef framples
		then
	then { samps }
	"%s %s %s %s %s %s" #( amp secs input-samps beg dur get-func-name )
	    string-format { origin }
	amp samps flt del effects-flecho-cb beg dur snd chn #f origin
	    map-channel
;

: effects-zecho
  <{ scl secs freq amp input-samps :optional beg 0 dur #f snd #f chn #f -- r }>
	freq make-oscil { os }
	secs snd srate f* fround->s { len }
	:size len :max-size len amp f>s 1 + + make-delay { del }
	input-samps number? if
		input-samps
	else
		dur number? if
			dur
		else
			snd chn undef framples
		then
	then { samps }
	"%s %s %s %s %s %s %s %s"
	    #( scl secs freq amp input-samps beg dur get-func-name )
	    string-format { origin }
	scl amp samps os del effects-zecho-cb beg dur snd chn #f origin 
	    map-channel
;
previous

\ effects-bbp ( freq bw :optional beg dur snd chn -- res )
\ effects-bbr ( freq bw :optional beg dur snd chn -- res )
\ effects-bhp ( freq :optional beg dur snd chn -- res )
\ effects-blp ( freq :optional beg dur snd chn -- res )
\ effects-comb-filter ( scl size :optional beg dur snd chn -- res )
\ effects-comb-chord ( scl size amp :optional beg dur snd chn -- res )

: effects-bbp <{ freq bw :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s %s" #( freq bw beg dur get-func-name )
	    string-format { origin }
	freq bw make-butter-band-pass beg dur snd chn #f #f origin clm-channel
;

: effects-bbr <{ freq bw :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s %s" #( freq bw beg dur get-func-name )
	    string-format { origin }
	freq bw make-butter-band-reject beg dur snd chn #f #f origin clm-channel
;

: effects-bhp <{ freq :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s" #( freq beg dur get-func-name ) string-format { origin }
	freq make-butter-high-pass beg dur snd chn #f #f origin clm-channel
;

: effects-blp <{ freq :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s" #( freq beg dur get-func-name ) string-format { origin }
	freq make-butter-low-pass beg dur snd chn #f #f origin clm-channel
;

: effects-comb-filter <{ scl size :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s %s" #( scl size beg dur get-func-name )
	    string-format { origin }
	scl size comb-filter beg dur snd chn #f origin map-channel
;

: effects-comb-chord
  <{ scl size amp :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s %s %s" #( scl size amp beg dur get-func-name )
	    string-format { origin }
	scl size amp comb-chord beg dur snd chn #f origin map-channel
;

\ effects-moog ( freq Q :optional beg dur snd chn -- res )

hide
: moog-cb ( gen -- prc; inval self -- res )
	1 proc-create swap , ( prc )
  does> { inval self -- res }
	self @ ( gen ) inval moog-filter
;
set-current

: effects-moog <{ freq Q :optional beg 0 dur #f snd #f chn #f -- res }>
	"%s %s %s %s %s" #( freq Q beg dur get-func-name )
	    string-format { origin }
	freq Q make-moog-filter moog-cb beg dur snd chn #f origin map-channel
;
previous

\ moog ( freq Q -- prc; inval self -- res )

: moog ( freq Q -- prc; inval self -- res )
	make-moog-filter { gen }
	1 proc-create gen , ( prc )
  does> { inval self -- res }
	self @ ( gen ) inval moog-filter
;

\ effects-am ( freq en :optional beg dur snd chn -- res )
\ effects-rm ( freq en :optional beg dur snd chn -- res )

hide
: effects-am-env-cb { os e -- prc; x self -- res }
	1 proc-create e , os , ( prc )
  does> { inval self -- res }
	self @ { e }
	self cell+ @ { os }
	1.0 inval e env os 0.0 0.0 oscil f* amplitude-modulate
;

: effects-am-cb ( os -- prc; x self -- res )
	1 proc-create swap , ( prc )
  does> { inval self -- res }
	self @ { os }
	os 0.0 0.0 oscil inval f*
;

: effects-rm-env-cb { os e -- prc; x self -- res }
	1 proc-create e , os , ( prc )
  does> { inval self -- res }
	self @ { e }
	self cell+ @ { os }
	os 0.0 0.0 oscil e env f* inval f*
;

: effects-rm-cb ( os -- prc; x self -- res )
	1 proc-create swap , ( prc )
  does> { inval self -- res }
	self @ { os }
	1.0 inval os 0.0 0.0 oscil amplitude-modulate
;
set-current

: effects-am <{ freq en :optional beg 0 dur #f snd #f chn #f -- res }>
	freq make-oscil { os }
	en array? if
		:envelope en :length dur 1- make-env
	else
		#f
	then { e }
	"%s %s %s %s %s" #( freq en beg dur get-func-name )
	    string-format { origin }
	e if
		os e effects-am-env-cb
	else
		os effects-am-cb
	then beg dur snd chn #f origin map-channel
;

: effects-rm <{ freq en :optional beg 0 dur #f snd #f chn #f -- res }>
	freq make-oscil { os }
	en array? if
		:envelope en :length dur 1- make-env
	else
		#f
	then { e }
	"%s %s %s %s %s" #( freq en beg dur get-func-name )
	    string-format { origin }
	e if
		os e effects-rm-env-cb
	else
		os effects-rm-cb
	then beg dur snd chn #f origin map-channel
;
previous

\ effects-jc-reverb ( samps volume -- prc; inval self -- res )
\ effects-jc-reverb-1 ( volume :optional beg dur snd chn -- res )

: effects-jc-reverb ( samps volume -- prc; inval self -- res )
	{ samps vol }
	-0.7 0.7 1051 make-all-pass { all1 }
	-0.7 0.7  337 make-all-pass { all2 }
	-0.7 0.7  113 make-all-pass { all3 }
	0.742 4799 make-comb { c1 }
	0.733 4999 make-comb { c2 }
	0.715 5399 make-comb { c3 }
	0.697 5801 make-comb { c4 }
	#f srate 0.013 f* fround->s make-delay { outdel }
	1 proc-create
	0 ( samp ),
	samps ,
	vol ,
	0.0 ( comb-sum ) ,
	0.0 ( comb-sum-1 ) ,
	0.0 ( comb-sum-2 ) ,
	all1 , all2 , all3 ,
	c1 , c2 , c3 , c4 ,
	outdel , ( prc )
  does> { inval self -- res }
	self @ ( samp++ ) 1+ self !
	self @ { samp }
	self  1 cells + @ { samps }
	self  2 cells + @ { volume }
	self  3 cells + @ { comb-sum }
	self  4 cells + @ { comb-sum-1 }
	self  5 cells + @ { comb-sum-2 }
	self  6 cells + @ { allpass1 }
	self  7 cells + @ { allpass2 }
	self  8 cells + @ { allpass3 }
	self  9 cells + @ { comb1 }
	self 10 cells + @ { comb2 }
	self 11 cells + @ { comb3 }
	self 12 cells + @ { comb4 }
	self 13 cells + @ { outdel }
	allpass3 allpass2 allpass1
	samp samps <= if
		inval
	else
		0.0
	then 0.0 all-pass 0.0 all-pass 0.0 all-pass { allpass-sum }
	comb-sum-1 self 5 cells + ! ( comb-sum-2 )
	comb-sum   self 4 cells + ! ( comb-sum-1 )
	comb1 allpass-sum 0.0 comb
	comb2 allpass-sum 0.0 comb f+
	comb3 allpass-sum 0.0 comb f+
	comb4 allpass-sum 0.0 comb f+ self 3 cells + ! ( comb-sum )
	outdel comb-sum 0.0 delay volume f* inval f+
;

: effects-jc-reverb-1 <{ vol :optional beg 0 dur #f snd #f  chn #f -- res }>
	dur if
		dur
	else
		snd chn #f framples
	then { samps }
	"%s %s %s %s" #( vol beg dur get-func-name ) string-format { origin }
	samps vol effects-jc-reverb beg dur snd chn #f origin map-channel
;

\ effects-cnv ( snd0 amp snd chn -- res )

hide
: cnv-cb ( sf -- prc; dir self -- res )
	1 proc-create swap , ( prc )
  does> { dir self -- res }
	self @ ( sf ) next-sample
;
set-current

: effects-cnv <{ snd0 amp :optional snd #f chn #f -- res }>
	snd0 sound? unless
		sounds 0 array-ref to snd0
	then
	snd0 #f #f framples { flt-len }
	snd chn #f framples flt-len + { total-len }
	:filter 0 flt-len snd0 #f #f channel->vct make-convolve { cnv }
	0 snd chn 1 #f make-sampler { sf }
	sf cnv-cb { cnv-func }
	total-len 0.0 make-vct map!
		cnv cnv-func convolve
	end-map { out-data }
	sf free-sampler drop
	out-data amp vct-scale! drop
	out-data vct-peak { max-samp }
	out-data 0 total-len snd chn #f
	"%s %s %s" #( snd0 amp get-func-name ) string-format
	vct->channel drop
	max-samp 1.0 f> if
		#( max-samp fnegate max-samp ) snd chn set-y-bounds drop
	then
	max-samp
;
previous

\ effects-position-sound ( mono-snd pos :optional snd chn -- res )
\ effects-place-sound ( mono-snd stereo-snd pan-env -- res )

hide
: numb-cb { rd pos -- prc; y self -- res }
	1 proc-create pos , rd , ( prc )
  does> { y self -- res }
	self cell+ @ ( rd ) read-sample self @ ( pos ) f* y f+
;

: env-numb-cb { rd en -- prc; y self -- res }
	1 proc-create en , rd , ( prc )
  does> { y self -- res }
	self cell+ @ ( rd ) read-sample self @ ( en ) env f* y f+
;

: env-cb { rd en -- prc; y self -- res }
	1 proc-create en , rd , ( prc )
  does> { y self -- res }
	self cell+ @ ( rd ) read-sample  1.0 self @ ( en ) env f-  f* y f+
;
set-current

: effects-position-sound <{ mono pos :optional snd #f chn #f -- res }>
	mono #f #f framples { len }
	0 mono #f 1 #f make-sampler { rd }
	"%s %s %s" #( mono pos get-func-name ) string-format { origin }
	pos number? if
		rd pos numb-cb 0 len snd chn #f origin map-channel
	else
		:envelope pos :length len 1- make-env { e }
		chn integer?
		chn 1 = && if
			rd e env-numb-cb 0 len snd chn #f origin map-channel
		else
			rd e env-cb 0 len snd chn #f origin map-channel
		then
	then
;

: effects-place-sound ( mono stereo pan -- res )
	doc" Mixes a mono sound into a stereo sound, \
splitting it into two copies whose amplitudes depend on the envelope PAN-ENV.  \
If PAN-ENV is a number, the sound is split such that 0 is all in channel 0 \
and 90 is all in channel 1."
	{ mono stereo pan }
	pan number? if
		pan 90.0 f/ { pos }
		mono pos        stereo 1 effects-position-sound drop
		mono 1.0 pos f- stereo 0 effects-position-sound
	else
		mono pan stereo 1 effects-position-sound drop
		mono pan stereo 0 effects-position-sound
	then
;
previous

\ effects-flange ( amount speed time :optional beg dur snd chn -- res )

hide
: flange-cb { ri del -- prc; inval self -- res }
	1 proc-create del , ri , ( prc )
  does> { inval self -- res }
	self @ ( del ) inval  self cell+ @ ( ri ) 0.0 rand-interp
	delay inval f+ 0.75 f*
;
set-current

: effects-flange
  <{ amnt speed time :optional beg 0 dur #f snd #f  chn #f  -- res}>
	:frequency speed :amplitude amnt make-rand-interp { ri }
	time snd srate f* fround->s { len }
	:size len :max-size amnt f>s len 1 + + make-delay { del }
	"%s %s %s %s %s %s"
	    #( amnt speed time beg
	       dur number?
	       snd chn #f framples dur <> && if
		       dur
	       else
		       #f
	       then get-func-name ) string-format { origin }
	ri del flange-cb  beg dur snd chn #f origin map-channel
;
previous

\ effects-cross-synthesis ( snd amp fftsize r -- prc; inval self -- res )
\ effects-cross-synthesis-1 ( snd amp fft r :optional beg dur snd chn -- res )

\ cross-synthesis from examp.fs
<'> cross-synthesis
    alias effects-cross-synthesis ( snd amp fftsize r -- prc; y self -- res )

: effects-cross-synthesis-1
  <{ csnd amp fftsize r :optional beg 0 dur #f snd #f  chn #f -- res }>
	{ csnd amp fftsize r beg dur snd chn }
	"%s %s %s %s %s %s %s" #( csnd amp fftsize r beg dur get-func-name )
	    string-format { origin }
	csnd sound? unless
		sounds 0 array-ref to csnd
	then
	csnd amp fftsize r effects-cross-synthesis beg dur snd chn #f origin
	    map-channel
;

\ effects-fp ( srf amp freq :optional beg dur snd chn -- vct )

hide
: src-fp-read-cb ( sf -- prc; dir self -- samp )
	1 proc-create swap , ( prc )
  does> { dir self -- samp }
	self @ ( sf ) dir 0> if
		next-sample
	else
		previous-sample
	then
;
set-current

: effects-fp <{ srf amp freq :optional beg 0 dur #f snd #f  chn #f -- vct }>
	freq make-oscil { os }
	:srate srf make-src { sr }
	beg snd chn 1 #f make-sampler { sf }
	dur if
		dur
	else
		snd chn #f framples
	then { len }
	sf src-fp-read-cb { src-cb }
	len 0.0 make-vct map!
		sr  os 0.0 0.0 oscil amp f*  src-cb  src
	end-map ( out-data ) beg len snd chn #f
	"%s %s %s %s %s %s" #( srf amp freq beg dur get-func-name )
	    string-format vct->channel
;
previous

\ effects-hello-dentist	( freq amp :optional beg dur snd chn -- res )

hide
: hello-src-cb { in-data idx -- prc; dir self -- samp }
	1 proc-create idx , in-data , ( prc )
  does> { dir self -- samp }
	self @ { idx }
	self cell+ @ { in-data }
	in-data idx range? if
		in-data idx vct-ref
	else
		0.0
	then ( val )
	idx dir + self ! ( idx )
;
set-current

: effects-hello-dentist
  <{ freq amp :optional beg 0 dur #f snd #f  chn #f -- res }>
	:frequency freq :amplitude amp make-rand-interp { rn }
	0 { idx }
	dur if
		dur
	else
		snd chn #f framples
	then { len }
	beg len snd chn #f channel->vct { in-data }
	amp f2* 1.0 f+ len f* fround->s ( out-len ) 0.0 make-vct { out-data }
	:srate 1.0 :input in-data idx hello-src-cb make-src { rd }
	out-data map!
		idx len = ?leave
		rd  rn  0.0 rand-interp  #f src
	end-map to out-data
	"%s %s %s %s %s" #( freq amp beg dur get-func-name )
	    string-format { origin }
	out-data beg out-data vct-length snd chn #f origin vct->channel
;
previous

\ effects-remove-clicks ( :optional snd chn -- res )
\ effects-remove-dc ( :optional snd chn -- res )
\ effects-compand ( :optional snd chn -- res )

hide
: find-click { loc snd chn -- pos|#f }
	loc snd chn 1 #f make-sampler { rd }
	0.0 0.0 0.0 { samp0 samp1 samp2 }
	10 0.0 make-vct { samps }
	#f 					\ flag
	snd chn #f framples loc ?do
		samp1 to samp0
		samp2 to samp1
		rd next-sample to samp2
		samps samp0 cycle-set!
		samps vct-peak 0.1 fmax { local-max }
		samp0 samp1 f- fabs local-max f>
		samp1 samp2 f- fabs local-max f> &&
		samp0 samp2 f- fabs local-max f2/ f< && if
			drop ( flag ) i leave
		then
	loop
;

: remove-click { loc snd chn -- }
	loc snd chn find-click { click }
	click if
		click 2 - 4 snd chn smooth-sound drop
		click 2 + snd chn recurse
	then
;

: effects-remove-dc-cb ( -- prc; inval self -- res )
	1 proc-create 0.0 ( lastx ) , 0.0 ( lasty ) , ( prc )
  does> { inval self -- res }
	self @ { lastx }
	self cell+ @ { lasty }
	0.999 lasty f* lastx f- inval f+ self cell+ ! ( lasty )
	inval self ! ( lastx )
	self cell+ @ ( lasty )
;

: effects-compand-cb ( tbl -- prc; inval self -- res )
	1 proc-create swap , ( prc )
  does> { inval self -- res }
	self @ { tbl }
	tbl inval 8.0 f* 8.0 f+ tbl length array-interp
;
set-current

: effects-remove-clicks <{ :optional snd #f chn #f -- res }>
	0 snd chn remove-click
	#f
;

: effects-remove-dc <{ :optional snd #f chn #f -- res }>
	effects-remove-dc-cb 0 #f snd chn #f get-func-name map-channel
;

: effects-compand <{ :optional snd #f chn #f -- res }>
	vct( -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250
	     0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000 ) { tbl }
	tbl effects-compand-cb 0 #f snd chn #f get-func-name map-channel
;
previous

'snd-nogui provided? [if] skip-file [then]

'snd-gtk provided? [if]
	'gtk3 provided? not [if]
		.( snd-gtk: gtk3 required -- skipping effects.fs ) cr
		skip-file
	[then]
[then]

require xm-enved
require snd-xm
require rubber

\ === SND MENU ===

hide
#( "menu-children"
   "menu-parent"
   "menu-name"
   "menu-menu"
   "menu-cascade"
   "menu-display-cb" ) create-struct make-snd-menu-struct

: menu-display ( gen -- )
	menu-display-cb@ #() run-proc drop
;

#( "eff_label"
   "eff_dialog"
   "eff_target"
   "eff_target_widget"
   "eff_trunc"
   "eff_sliders"
   "eff_scl"
   "eff_freq"
   "eff_amp"
   "eff_delay"
   "eff_amnt"
   "eff_enved"
   "eff_size"
   "eff_omit_silence"
   "eff_bp_bw"
   "eff_notch_bw"
   "eff_moog_reson"
   "eff_time_scale"
   "eff_hop_size"
   "eff_ramp_scl"
   "eff_pitch_scl"
   "eff_seg_len"
   "eff_rev_filter"
   "eff_rev_fb"
   "eff_rev_decay"
   "eff_rev_vol"
   "eff_conv_one"
   "eff_conv_two"
   "eff_m_snd"
   "eff_s_snd"
   "eff_pan_pos"
   "eff_cs_snd"
   "eff_cs_radius"
   "eff_cs_wid"
   "eff_fl_speed"
   "eff_fl_time"
   "eff_sr"
   "eff_factor" ) create-struct make-effects-menu-struct
set-current

: make-base-effects { label -- gen }
	make-effects-menu-struct { gen }
	gen label eff_label!
	gen #f eff_dialog!
	gen 'sound eff_target!
	gen #t eff_trunc!
	gen #f eff_sliders!
	gen
;

<'> noop 0 make-proc constant effects-noop

"Go Away" constant eff-dismiss-string
"Help"    constant eff-help-string
"DoIt"    constant eff-okay-string
"Reset"   constant eff-reset-string

\ log scaler widget

500.0 constant log-scale-ticks

: scale-log->linear ( lo val hi -- lin )
	{ lo val hi }
	2.0 flog { log2 }
	lo 1.0 fmax flog log2 f/ { log-lo }
	hi flog          log2 f/ { log-hi }
	val flog log2 f/  log-lo f-  log-hi log-lo f-  f/ log-scale-ticks
	    f* floor->s
;

: scale-linear->log ( lo val hi -- log )
	{ lo val hi }
	2.0 flog { log2 }
	lo 1.0 fmax flog log2 f/ { log-lo }
	hi flog          log2 f/ { log-hi }
	2.0  log-lo val log-scale-ticks f/ log-hi log-lo f- f* f+  f**
;

: scale-log-label ( lo val hi -- str )
	scale-linear->log "%.2f" swap 1 >array string-format
;

\ semitone scaler widget

24 value semi-range

: semi-scale-label ( val -- str )
	"semitones: %s" swap semi-range - 1 >array string-format
;

: semitones->ratio ( val -- r )
	2.0 swap 12.0 f/ f**
;

: ratio->semitones ( ratio -- n )
	12.0 swap flog 2.0 flog f/ f* fround->s
;

: marks-sort ( a b -- -1|0|1 )
	{ a b }
	a b < if
		-1
	else
		a b = if
			0
		else
			1
		then
	then
;

\ returns a list of points
: plausible-mark-samples ( -- pts )
	selected-sound { snd }
	snd selected-channel { chn }
	#() { ms }
	snd chn #f marks each
		undef mark-sample ms swap array-push drop
	end-each
	ms length 2 < if
		#f
	else
		ms <'> marks-sort array-sort! drop
		ms length 2 = if
			ms array->array
		else
			snd chn left-sample  { lw }
			snd chn right-sample { rw }
			snd chn undef cursor { cw }
			cw lw >=
			cw rw <= && if
				cw
			else
				lw rw + 2/
			then { favor }
			#( ms first-ref ms second-ref ) { res }
			ms each { p1 }
				i ms length 2 - = if
					#( p1 ms last-ref ) to res
					leave
				then
				ms i 1+  array-ref { p2 }
				ms i 2 + array-ref { p3 }
				p1 favor - abs p3 favor - abs < if
					#( p1 p2 ) to res
					leave
				then
			end-each
			res
		then
	then
;

: effect-frames { target -- frms }
	target 'sound = if
		#f #f #f framples 1-
	else
		target 'selection = if
			#f #f selection-framples
		else
			plausible-mark-samples { pts }
			pts if
				pts 0 array-ref pts 1 nil array-subarray each
					-
				end-each abs 1+
			else
				0
			then
		then
	then
;

: effect-target-ok <{ target -- f }>
	sounds empty? if
		#f
	else
		target 'sound = if
			#t
		else
			target 'selection = if
				undef selection?
			else
				target 'marks = if
					selected-sound dup
					selected-channel #f marks length 2 >=
				else
					#f
				then
			then
		then
	then
;

: general-target-cb ( gen -- prc; self -- f )
	0 proc-create swap , ( prc )
  does> { self -- f }
	self @ ( gen ) eff_target@ effect-target-ok
;

: set-default-target-cb { okay-button -- prc; self -- }
	0 proc-create okay-button , ( prc )
  does> { self -- }
	self @ ( okay-button ) sounds empty? not set-sensitive
;

: set-target-cb { okay-button target-prc -- prc; self -- }
	0 proc-create okay-button , target-prc , ( prc )
  does> { self -- }
	self @ ( okay ) self cell+ @ ( target ) #() run-proc set-sensitive
;

: help-cb { label message -- prc; w c i self -- x }
	3 proc-create label , message , ( prc )
  does> { w c info self -- x }
	self @ ( label ) self cell+ @ ( message ) help-dialog
;

: target-cb ( gen -- prc; target self -- )
	1 proc-create swap , ( prc )
  does> { target self -- }
	self @ { gen }
	gen target eff_target!
	gen eff_target_widget@  target effect-target-ok  set-sensitive
;

: truncate-cb ( gen -- prc; trunc self -- )
	1 proc-create swap , ( prc )
  does> { trunc self }
	self @ ( gen ) trunc eff_trunc!
;

: map-chan-over-target-with-sync { func target origin-func decay -- }
	sounds empty? if
		"no sound" undef status-report drop
	else
		target 'selection =
		undef selection? not && if
			"no selection" undef status-report drop
		else
			#f sync { snc }
			target 'marks = if
				plausible-mark-samples
			else
				#()
			then { pts }
			target 'sound = if
				0
			else
				target 'selection = if
					#f #f selection-position
				else
					pts 0 array-ref
				then
			then { beg }
			decay number? if
				#f srate decay f* fround->s
			else
				0
			then { overlap }
			snc 0> if
				all-chans
			else
				#( #( selected-sound dup selected-channel ) )
			then each { lst }
				lst 0 array-ref { snd }
				lst 1 array-ref { chn }
				snd sync snc = if
					target 'sound = if
						snd chn undef framples 1-
					else
						target 'selection = if
							#f #f selection-position
							#f #f selection-framples
							    +
						else
							pts 1 array-ref
						then
					then { end }
					end beg - { dur }
					origin-func #( target dur )
					    run-proc { name-and-orig }
					"%s %s %s %s"
					    #( name-and-orig 0 array-ref
					       beg
					       target 'sound = if
						       #f
					       else
						       dur 1+
					       then
					       name-and-orig 1 array-ref )
					       string-format { origin }
					func dur run-proc beg end overlap + 1+
					    snd chn #f origin map-channel drop
				then
			end-each
		then
	then
;

'snd-motif provided? [if]
	: cascade-cb <{ w c i -- }>
		c each
			#() run-proc drop
		end-each
	;

	: make-menu { name parent -- gen }
		make-snd-menu-struct { gen }
		parent name #( FXmNbackground basic-color ) undef
		    FXmCreatePulldownMenu { menu }
		#() { lst }
		name FxmCascadeButtonWidgetClass parent
		    #( FXmNsubMenuId menu FXmNbackground basic-color ) undef
		    FXtCreateManagedWidget { cas }
		cas FXmNcascadingCallback <'> cascade-cb lst FXtAddCallback drop
		gen parent menu-parent!
		gen name menu-name!
		gen menu menu-menu!
		gen cas menu-cascade!
		gen lst menu-children!
		gen
	;

	: menu-entry { gen prc disp-prc -- }
		gen menu-children@ { lst }
		lst array? lst 1 "an array" assert-type
		gen menu-name@ FxmPushButtonWidgetClass gen menu-menu@
		    #( FXmNbackground basic-color ) undef
		    FXtCreateManagedWidget { child }
		child FXmNactivateCallback prc undef FXtAddCallback drop
		lst disp-prc #( child ) run-proc array-push drop
	;

	: unmanage-cb <{ w c i -- f }>
		c FXtUnmanageChild
	;

	[undefined] F_XEditResCheckMessages [if]
		: F_XEditResCheckMessages <{ w c i f -- x }> #f ;
	[then]

	: make-effect-dialog { label ok-prc help-prc reset-prc target-prc -- d }
		eff-dismiss-string FXmStringCreateLocalized { xdismiss }
		eff-help-string    FXmStringCreateLocalized { xhelp }
		eff-okay-string    FXmStringCreateLocalized { xok }
		label              FXmStringCreateLocalized { titlestr }
		main-widgets 1 array-ref label
		#( FXmNcancelLabelString xdismiss
		   FXmNhelpLabelString   xhelp
		   FXmNokLabelString     xok
		   FXmNautoUnmanage      #f
		   FXmNdialogTitle       titlestr
		   FXmNresizePolicy      FXmRESIZE_GROW
		   FXmNnoResize          #f
		   FXmNbackground        basic-color
		   FXmNtransient         #f ) undef
		    FXmCreateTemplateDialog { d }
		xhelp    FXmStringFree drop
		xok      FXmStringFree drop
		xdismiss FXmStringFree drop
		titlestr FXmStringFree drop
		d 0 #t <'> F_XEditResCheckMessages #f
		    FXtAddEventHandler drop
		#( #( FXmDIALOG_HELP_BUTTON   highlight-color )
		   #( FXmDIALOG_CANCEL_BUTTON highlight-color )
		   #( FXmDIALOG_OK_BUTTON     highlight-color ) ) each { lst }
			lst 0 array-ref { button }
			lst 1 array-ref { color }
			d button FXmMessageBoxGetChild
			    #( FXmNarmColor   selection-color
			       FXmNbackground color ) FXtVaSetValues drop
		end-each
		d FXmNcancelCallback <'> unmanage-cb d FXtAddCallback drop
		d FXmNhelpCallback help-prc undef FXtAddCallback drop
		d FXmNokCallback ok-prc undef FXtAddCallback drop
		reset-prc if
			eff-reset-string FxmPushButtonWidgetClass d
			    #( FXmNbackground highlight-color
			       FXmNforeground black-pixel
			       FXmNarmColor   selection-color ) undef
			    FXtCreateManagedWidget ( reset )
			FXmNactivateCallback reset-prc undef FXtAddCallback drop
		then
		d FXmDIALOG_OK_BUTTON FXmMessageBoxGetChild { okay-button }
		effects-hook okay-button target-prc ?dup-if
			set-target-cb
		else
			set-default-target-cb
		then add-hook!
		d
	;

	: scale-log-cb <{ w c info -- }>
		c 0 array-ref { label }
		c 1 array-ref { low }
		c 2 array-ref { high }
		label low info Fvalue high scale-log-label change-label
	;

	: create-log-scale-widget { parent title low init high cb -- scale }
		"%.2f" #( init ) string-format FxmLabelWidgetClass parent
		    #( FXmNbackground basic-color ) undef
		    FXtCreateManagedWidget { label }
		"scale" FxmScaleWidgetClass parent
		    #( FXmNorientation   FXmHORIZONTAL
		       FXmNshowValue     #f
		       FXmNminimum       0
		       FXmNmaximum       log-scale-ticks f>s
		       FXmNvalue         low init high scale-log->linear
		       FXmNdecimalPoints 0
		       FXmNtitleString   title
		       FXmNbackground    basic-color ) undef
		    FXtCreateManagedWidget { scale }
		#( label low high ) { data }
		scale FXmNvalueChangedCallback <'> scale-log-cb data
		    FXtAddCallback drop
		scale FXmNvalueChangedCallback cb undef FXtAddCallback drop
		scale FXmNdragCallback <'> scale-log-cb data FXtAddCallback drop
		scale FXmNdragCallback cb undef FXtAddCallback drop
		scale
	;

	: scale-semi-cb <{ w c info -- }>
		c  info Fvalue semi-scale-label  change-label
	;

	: create-semi-scale-widget { parent title init cb -- scale }
		"semitones: %s" #( init ratio->semitones ) string-format { str }
		str FxmLabelWidgetClass parent
		    #( FXmNbackground  basic-color ) undef
		    FXtCreateManagedWidget { label }
		"scale" FxmScaleWidgetClass parent
		    #( FXmNorientation   FXmHORIZONTAL
		       FXmNshowValue     #f
		       FXmNminimum       0
		       FXmNmaximum       semi-range 2*
		       FXmNvalue         semi-range init ratio->semitones +
		       FXmNdecimalPoints 0
		       FXmNtitleString   title
		       FXmNbackground    basic-color ) undef
		    FXtCreateManagedWidget { scale }
		scale FXmNvalueChangedCallback <'> scale-semi-cb label
		    FXtAddCallback drop
		scale FXmNvalueChangedCallback cb undef FXtAddCallback drop
		scale FXmNdragCallback <'> scale-semi-cb label
		    FXtAddCallback drop
		scale FXmNdragCallback cb undef FXtAddCallback drop
		scale
	;

	\ sliders: #( #( label low init high func scale [log] ) ... )
	: add-sliders { dialog sliders -- sliders-array }
		"formd" FxmFormWidgetClass dialog
		    #( FXmNleftAttachment   FXmATTACH_FORM
		       FXmNrightAttachment  FXmATTACH_FORM
		       FXmNtopAttachment    FXmATTACH_FORM
		       FXmNbottomAttachment FXmATTACH_WIDGET
		       FXmNbottomWidget
		       dialog FXmDIALOG_SEPARATOR FXmMessageBoxGetChild
		       FXmNbackground       highlight-color ) undef
		    FXtCreateManagedWidget { mainfrm }
		"rcd" FxmRowColumnWidgetClass mainfrm
		    #( FXmNleftAttachment   FXmATTACH_FORM
		       FXmNrightAttachment  FXmATTACH_FORM
		       FXmNbackground       highlight-color
		       FXmNorientation      FXmVERTICAL ) undef
		    FXtCreateManagedWidget { mainform }
		sliders map
			*key* 0 array-ref FXmStringCreateLocalized { title }
			*key* 1 array-ref { low }
			*key* 2 array-ref { init }
			*key* 3 array-ref { high }
			*key* 4 array-ref { func }
			*key* 5 array-ref { scale }
			*key* length 7 = if
				*key* 6 array-ref 'log = if
					mainform title low init high func
					    create-log-scale-widget
				else
					mainform title init func
					    create-semi-scale-widget
				then ( scale )
			else
				*key* 0 array-ref FxmScaleWidgetClass mainform
				    #( FXmNorientation FXmHORIZONTAL
				       FXmNshowValue   #t
				       FXmNminimum     low  scale f* fround->s
				       FXmNmaximum     high scale f* fround->s
				       FXmNvalue       init scale f* fround->s
				       FXmNdecimalPoints
				       scale 10000 = if
					       4
				       else
					       scale 1000 = if
						       3
					       else
						       scale 100 = if
							       2
						       else
							       scale 10 = if
								       1
							       else
								       0
							       then
						       then
					       then
				       then
				       FXmNtitleString     title
				       FXmNleftAttachment  FXmATTACH_FORM
				       FXmNrightAttachment FXmATTACH_FORM
				       FXmNbackground      basic-color ) undef
				    FXtCreateManagedWidget ( sc )
			then { new-slider }
			title FXmStringFree drop
			new-slider FXmNvalueChangedCallback func undef
			    FXtAddCallback drop
			new-slider
		end-map
	;

	: color->pixel ( color-str "name" --; self -- pixel )
		{ color-str }
		create #f , color-str ,
 	  does> { self -- pixel }
		self @ ( color ) unless
			main-widgets 1 array-ref { shell }
			shell FXtDisplay { dpy }
			dpy FDefaultScreen { scr }
			dpy scr FDefaultColormap { cmap }
			undef undef undef undef undef undef FXColor { col }
			dpy cmap
			    self cell+ @ ( color-str )
			    col col FXAllocNamedColor 0= if
				"can't allocate color!" snd-error drop
			else
				col Fpixel self !
			then
		then
		self @ ( color )
	;

	"yellow" color->pixel yellow-pixel

	\ c == #( prc type )
	: target-arm-cb <{ w c info -- f }>
		c 0 array-ref #( c 1 array-ref ) run-proc
	;

	: target-truncate-cb <{ w c info -- f }>
		c #( info Fset ) run-proc
	;

	: add-target-main { mainform target-prc truncate-prc -- rc-wid }
		"sep" FxmSeparatorWidgetClass mainform
		    #( FXmNorientation      FXmHORIZONTAL
		       FXmNseparatorType    FXmSHADOW_ETCHED_OUT
		       FXmNbackground       basic-color ) undef
		    FXtCreateManagedWidget drop
		"rc" FxmRowColumnWidgetClass mainform
		    #( FXmNorientation      FXmHORIZONTAL
		       FXmNbackground       basic-color
		       FXmNradioBehavior    #t
		       FXmNradioAlwaysOne   #t
		       FXmNbottomAttachment FXmATTACH_FORM
		       FXmNleftAttachment   FXmATTACH_FORM
		       FXmNrightAttachment  FXmATTACH_FORM
		       FXmNentryClass       FxmToggleButtonWidgetClass
		       FXmNisHomogeneous    #t ) undef
		    FXtCreateManagedWidget { rc }
		#( #( "entire sound"  'sound     #t )
		   #( "selection"     'selection #f )
		   #( "between marks" 'marks     #f ) ) each { lst }
			lst 0 array-ref { name }
			lst 1 array-ref { typ }
			lst 2 array-ref { on }
			name FxmToggleButtonWidgetClass rc
			    #( FXmNbackground     basic-color
			       FXmNselectColor    yellow-pixel
			       FXmNSet            on
			       FXmNindicatorType  FXmONE_OF_MANY_ROUND
			       FXmNarmCallback
			       #( <'> target-arm-cb #( target-prc typ ) ) )
			    undef FXtCreateManagedWidget drop
		end-each
		truncate-prc if
			"trsep" FxmSeparatorWidgetClass mainform
			    #( FXmNorientation FXmHORIZONTAL ) undef
			    FXtCreateManagedWidget drop
			"truncate at end" FxmToggleButtonWidgetClass mainform
			    #( FXmNbackground  basic-color
			       FXmNset         #t
			       FXmNselectColor yellow-pixel ) undef
			    FXtCreateManagedWidget ( trbut )
			FXmNvalueChangedCallback <'> target-truncate-cb
			    truncate-prc FXtAddCallback drop
		then
		rc
	;

	: add-target { gen truncate-prc -- }
		gen eff_dialog@ FXmDIALOG_OK_BUTTON FXmMessageBoxGetChild ( mb )
		gen swap eff_target_widget!
		gen eff_sliders@ 0 array-ref FXtParent { mainform }
		truncate-prc if
			gen truncate-prc to truncate-prc
		then
		mainform gen target-cb truncate-prc add-target-main drop
	;

	: get-slider-value { w info corr -- val }
		info Fvalue corr f/
	;

	: set-slider-value { w val corr -- }
		w #( FXmNvalue val corr f* f>s ) FXtVaSetValues drop
	;
[else]				\ !HAVE_MOTIF
	: motif->gtk-cb ( prc-3-arg -- prc-2-arg; w d self -- x )
		2 proc-create swap , ( prc )
	  does> { w d self -- x }
		self @ ( prc ) #( w d #f ) run-proc
	;

	\ We use existing motif callbacks.
	: wrap-motif-cb ( prc -- prc' )
		dup proc-arity 0 array-ref 3 = if
			motif->gtk-cb
		then
	;

	: cascade-cb <{ w d -- f }>
		d each
			#() run-proc drop
		end-each
		#f
	;

	: make-menu { name parent -- gen }
		make-snd-menu-struct { gen }
		name Fgtk_menu_item_new_with_label { menu }
		#() { lst }
		Fgtk_menu_new { cas }
		parent FGTK_MENU_ITEM Fgtk_menu_item_get_submenu FGTK_MENU_SHELL
		menu Fgtk_menu_shell_append drop
		menu Fgtk_widget_show drop
		menu FGTK_MENU_ITEM cas Fgtk_menu_item_set_submenu drop
		menu "activate" <'> cascade-cb lst Fg_signal_connect drop
		gen parent menu-parent!
		gen name menu-name!
		gen menu menu-menu!
		gen cas menu-cascade!
		gen lst menu-children!
		gen
	;

	: menu-entry { gen prc disp-prc -- }
		gen menu-children@ { lst }
		lst array? lst 1 "an array" assert-type
		gen menu-name@ Fgtk_menu_item_new_with_label { child }
		gen menu-cascade@ FGTK_MENU_SHELL child
		    Fgtk_menu_shell_append drop
		child Fgtk_widget_show drop
		child "activate" prc wrap-motif-cb #f Fg_signal_connect drop
		lst disp-prc #( child ) run-proc array-push drop
	;

	: unmanage-ev-cb <{ w ev d -- f }>
		d Fgtk_widget_hide drop #t
	;

	: unmanage-cb <{ w d -- f }>
		d Fgtk_widget_hide
	;

	: make-effect-dialog { label ok-prc help-prc reset-prc target-prc -- d }
		eff-dismiss-string Fgtk_button_new_with_label { dismiss-button }
		eff-help-string    Fgtk_button_new_with_label { help-button }
		eff-okay-string    Fgtk_button_new_with_label { okay-button }
		Fgtk_dialog_new { d }
		dismiss-button "quit_button" Fgtk_widget_set_name drop
		help-button    "help_button" Fgtk_widget_set_name drop
		okay-button    "doit_button" Fgtk_widget_set_name drop
		d FGTK_CONTAINER 10 Fgtk_container_set_border_width drop
		d FGTK_WINDOW { window }
		window label Fgtk_window_set_title drop
		window -1 -1 Fgtk_window_set_default_size drop
		window #t Fgtk_window_set_resizable drop
		d FGTK_DIALOG Fgtk_dialog_get_action_area FGTK_BOX { box }
		d "delete_event" <'> unmanage-ev-cb d Fg_signal_connect drop
		box dismiss-button #t #t 20 Fgtk_box_pack_start drop
		dismiss-button "clicked" <'> unmanage-cb d
		    Fg_signal_connect drop
		dismiss-button Fgtk_widget_show drop
		box okay-button #t #t 20 Fgtk_box_pack_start drop
		okay-button "clicked" ok-prc wrap-motif-cb #f
		    Fg_signal_connect drop
		okay-button Fgtk_widget_show drop
		reset-prc if
			eff-reset-string Fgtk_button_new_with_label { reset }
			reset "reset_button" Fgtk_widget_set_name drop
			box reset #t #t 20 Fgtk_box_pack_start drop
			reset "clicked" reset-prc wrap-motif-cb #f
			    Fg_signal_connect drop
			reset Fgtk_widget_show drop
		then
		box help-button #t #t 20 Fgtk_box_pack_end drop
		help-button "clicked" help-prc wrap-motif-cb #f
		    Fg_signal_connect drop
		help-button Fgtk_widget_show drop
		effects-hook  okay-button  target-prc ?dup-if
			set-target-cb
		else
			set-default-target-cb
		then add-hook!
		d FG_OBJECT "ok-button" okay-button FGPOINTER
		Fg_object_set_data drop
		d
	;

	: scale-log-cb <{ w d -- f }>
		d 0 array-ref { label }
		d 1 array-ref { title }
		d 2 array-ref { low }
		d 3 array-ref { high }
		d 4 array-ref { func }
		func #( w d ) run-proc drop
		label title ": " $+ low
		    w FGTK_ADJUSTMENT Fgtk_adjustment_get_value
		    high scale-log-label $+ change-label
		#f
	;

	'gtk3 provided? [if]
		<'> noop alias effects-range-set-update-policy ( w -- f )
	[else]
		: effects-range-set-update-policy ( w -- f )
			FGTK_RANGE FGTK_UPDATE_CONTINUOUS
			    Fgtk_range_set_update_policy
		;
	[then]

	\ sliders: #( #( label low init high func scale [log] ) ... )
	: add-sliders { dialog sliders -- sliders-array }
		FGTK_ORIENTATION_VERTICAL 2 Fgtk_box_new { mainform }
		sliders length 1 = if
			#f #f
		else
			Fgtk_grid_new dup FGTK_GRID
		then { table tabtab }
		0 { slider }
		dialog FGTK_DIALOG Fgtk_dialog_get_content_area FGTK_BOX { box }
		box mainform #f #f 4 Fgtk_box_pack_start drop
		mainform Fgtk_widget_show drop
		table if
			mainform FGTK_BOX table #f #f 4 Fgtk_box_pack_start drop
			tabtab 4 Fgtk_grid_set_row_spacing drop
			tabtab 4 Fgtk_grid_set_column_spacing drop
			table Fgtk_widget_show drop
		then
		sliders map
			*key* 0 array-ref { title }
			*key* 1 array-ref { low }
			*key* 2 array-ref { init }
			*key* 3 array-ref { high }
			*key* 4 array-ref { func }
			*key* 5 array-ref { scaler }
			*key* length 7 = if
				*key* 6 array-ref 'log =
			else
				#f
			then { use-log }
			table if
				#f
			else
				FGTK_ORIENTATION_HORIZONTAL 0 Fgtk_box_new
			then { hbox }
			table if
				use-log if
					"%s (%.2f)" #( title init )
				else
					"%s" #( title )
				then
			else
				use-log if
					"%s: %.2f" #( title init )
				else
					"%s:" #( title )
				then
			then string-format Fgtk_label_new { label }
			use-log if
				low init high scale-log->linear
				    0 log-scale-ticks f>s 1 10 1
			else
				init low high 0.0 0.0 0.0
			then Fgtk_adjustment_new { adj }
			FGTK_ORIENTATION_HORIZONTAL adj FGTK_ADJUSTMENT
			    Fgtk_scale_new { scale }
			table if
				tabtab label 0 slider 1 1 Fgtk_grid_attach drop
			else
				mainform FGTK_BOX hbox #f #f 2
				    Fgtk_box_pack_start drop
				hbox Fgtk_widget_show drop
				hbox FGTK_BOX label #f #f 6
				    Fgtk_box_pack_start drop
			then
			label Fgtk_widget_show drop
			scale FGTK_SCALE { sclscl }
			sclscl effects-range-set-update-policy drop
			sclscl use-log if
				0
			else
				scaler 1000 = if
					3
				else
					scaler 100 = if
						2
					else
						scaler 10 = if
							1
						else
							0
						then
					then
				then
			then Fgtk_scale_set_digits drop
			sclscl use-log not Fgtk_scale_set_draw_value drop
			table if
				scale FGTK_WIDGET #t
				    Fgtk_widget_set_hexpand drop
				tabtab scale 1 slider 1 1 Fgtk_grid_attach drop
				slider 1+ to slider
			else
				hbox FGTK_BOX scale #t #t 0
				    Fgtk_box_pack_start drop
			then
			scale Fgtk_widget_show drop
			adj "value_changed"
			use-log if
				<'> scale-log-cb
				    #( label title low high func wrap-motif-cb )
			else
				func wrap-motif-cb #f
			then Fg_signal_connect drop
			adj
		end-map
	;

	\ d: #( func type )
	: target-arm-cb <{ w d -- f }>
		d 0 array-ref { func }
		d 1 array-ref { typ }
		func #( typ ) run-proc
	;

	\ d: func
	: target-truncate-cb <{ w d -- f }>
		w FGTK_TOGGLE_BUTTON Fgtk_toggle_button_get_active { wid }
		d #( wid ) run-proc
	;

	: add-target-main { mainform target-prc truncate-prc -- rc-wid }
		FGTK_ORIENTATION_HORIZONTAL 2 Fgtk_box_new { rc }
		mainform FGTK_BOX rc #f #f 4 Fgtk_box_pack_start drop
		rc Fgtk_widget_show drop
		rc FGTK_BOX { rcbox }
		#f { group }
		#( #( "entire sound"  'sound     #t )
		   #( "selection"     'selection #f )
		   #( "between marks" 'marks     #f ) ) each { lst }
			lst 0 array-ref { name }
			lst 1 array-ref { typ }
			lst 2 array-ref { on }
			group name Fgtk_radio_button_new_with_label { button }
			button FGTK_RADIO_BUTTON Fgtk_radio_button_get_group
			    to group
			rcbox button #f #f 4 Fgtk_box_pack_start drop
			button FGTK_TOGGLE_BUTTON on
			    Fgtk_toggle_button_set_active drop
			button Fgtk_widget_show drop
			button "clicked" <'> target-arm-cb
			    #( target-prc typ ) Fg_signal_connect drop
		end-each
		truncate-prc if
			FGTK_ORIENTATION_HORIZONTAL Fgtk_separator_new { sep }
			rcbox sep #t #t 4 Fgtk_box_pack_start drop
			sep Fgtk_widget_show drop
			"truncate at end" Fgtk_check_button_new_with_label
			    to button
			rcbox button #t #t 4 Fgtk_box_pack_start drop
			button FGTK_TOGGLE_BUTTON #t
			    Fgtk_toggle_button_set_active drop
			button Fgtk_widget_show drop
			button "clicked" <'> target-truncate-cb truncate-prc
			    Fg_signal_connect drop
		then
		rc
	;

	: add-target { gen truncate-prc -- }
		gen eff_dialog@ FG_OBJECT "ok-button"
		    Fg_object_get_data FGTK_WIDGET ( mb )
		gen swap eff_target_widget!
		gen eff_dialog@ FGTK_DIALOG Fgtk_dialog_get_content_area { d }
		truncate-prc if
			gen truncate-prc to truncate-prc
		then
		d gen target-cb truncate-prc add-target-main drop
	;

	: get-slider-value { w info corr -- val }
		w FGTK_ADJUSTMENT Fgtk_adjustment_get_value
	;

	: set-slider-value { w val corr -- }
		w FGTK_ADJUSTMENT val Fgtk_adjustment_set_value drop
	;
[then]				\ HAVE_MOTIF

: make-main-menu ( name -- wid )
	effects-noop add-to-main-menu dup to effects-menu main-menu
;

: add-to-effects-menu ( name prc -- )
	effects-menu -rot undef add-to-menu drop
;
previous

hide
\ reusable callbacks
: amplitude-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amp!
;

: frequency-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_freq!
;

: log-freq-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	20.0 w info 1.0 get-slider-value 22050.0 scale-linear->log { val }
	self @ ( gen ) val eff_freq!
;

: scaler-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_scl!
;

: size-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_size!
;

\ === Effects Entries ===

\ === AMPLITUDE EFFECTS ===

\ === Gain (gain set by gain-amount) ===

'snd-motif provided? [if]
	: make-enved-widget { gen -- }
		gen eff_dialog@ FXmDIALOG_OK_BUTTON FXmMessageBoxGetChild ( mb )
		gen swap eff_target_widget!
		gen eff_sliders@ 0 array-ref FXtParent FXtParent { mainform }
		"fr" FxmFrameWidgetClass mainform
		    #( FXmNheight           200
		       FXmNleftAttachment   FXmATTACH_FORM
		       FXmNrightAttachment  FXmATTACH_FORM
		       FXmNtopAttachment    FXmATTACH_WIDGET
		       FXmNtopWidget        gen eff_sliders@ last-ref
		       FXmNshadowThickness  4
		       FXmNshadowType       FXmSHADOW_ETCHED_OUT ) undef
		    FXtCreateManagedWidget { fr }
		mainform gen target-cb #f add-target-main { target-row }
		gen eff_dialog@ activate-dialog
		gen eff_label@ string-downcase fr
		    :envelope #( 0.0 1.0 1.0 1.0 )
		    :axis-bounds #( 0.0 1.0 0.0 1.0 )
		    :args #( FXmNheight 200 ) make-xenved { en }
		gen en eff_enved!
		fr #( FXmNbottomAttachment FXmATTACH_WIDGET
		      FXmNbottomWidget target-row ) FXtVaSetValues drop
	;
[else]
	: make-enved-widget { gen -- }
		gen #f add-target
		gen eff_dialog@ Fgtk_widget_show drop
		gen eff_label@ string-downcase
		gen eff_dialog@ FGTK_DIALOG Fgtk_dialog_get_content_area
		    :envelope #( 0.0 1.0 1.0 1.0 )
		    :axis-bounds #( 0.0 1.0 0.0 1.0 ) make-xenved { en }
		gen en eff_enved!
		gen eff_dialog@ activate-dialog
	;
[then]

: gain-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_enved@ xe-envelope #( 0.0 1.0 1.0 1.0 ) equal? if
		#f
	else
		gen eff_enved@ xe-envelope gen eff_amnt@ scale-envelope
	then { with-env }
	gen eff_target@ 'sound = if
		with-env array? if
			with-env 0 undef 1.0 #f #f #f env-sound
		else
			gen eff_amnt@ #f #f scale-by
		then
	else
		gen eff_target@ 'selection = if
			undef selection? if
				with-env array? if
					with-env 1.0 env-selection
				else
					gen eff_amnt@ scale-selection-by
				then
			else
				"no selection" undef status-report
			then
		else
			plausible-mark-samples { pts }
			pts if
				with-env array? if
					with-env
					    pts 0 array-ref
					    pts 1 array-ref
					    pts 0 array-ref - 
					    1.0 #f #f #f env-sound
				else
					gen eff_amnt@
					    pts 0 array-ref
					    pts 1 array-ref
					    pts 0 array-ref -
					    #f #f #f normalize-channel
				then
			else
				"no marks" undef status-report
			then
		then
	then
;

: gain-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init }
	gen init eff_amnt!
	gen eff_enved@ #( 0.0 1.0 1.0 1.0 ) set-xe-envelope
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: gain-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-gain-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen gain-ok-cb
		    gen eff_label@ "\
Move the slider to change the gain scaling amount." help-cb
		    gen gain-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "gain" 0.0 gen eff_amnt@ 5.0
			gen gain-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen make-enved-widget
	else
		gen eff_dialog@ activate-dialog
	then
;
set-current
  
: make-gain-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_amnt!
	gen #f eff_enved!
	gen post-gain-dialog ( prc1 )
	1 proc-create gen ,  ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === Normalize ===

hide
: normalize-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_target@ 'sound = if
		gen eff_amnt@ #f #f scale-to drop
	else
		gen eff_target@ 'selection = if
			undef selection? if
				gen eff_amnt@ scale-selection-to drop
			else
				"no selection" undef status-report drop
			then
		else
			plausible-mark-samples { pts }
			pts if
				gen eff_amnt@
				    pts 0 array-ref
				    pts 1 array-ref
				    pts 0 array-ref -
				    #f #f #f normalize-channel drop
			else
				"no marks" undef status-report drop
			then
		then
	then
;

: normalize-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init }
	gen init eff_amnt!
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: normalize-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-normalize-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen normalize-ok-cb
		    gen eff_label@ "\
Normalize scales amplitude to the normalize amount.  \
Move the slider to change the scaling amount." help-cb
		    gen normalize-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "normalize" 0.0 gen eff_amnt@ 1.0
			gen normalize-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-normalize-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_amnt!
	gen post-normalize-dialog ( prc1 )
	1 proc-create gen ,       ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === Gate (gate set by gate-amount) ===

hide
: gate-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	selected-sound sync { snc }
	snc 0> if
		all-chans each { lst }
			lst 0 array-ref { snd }
			snd sync snc = if
				lst 1 array-ref { chn }
				gen eff_amnt@ dup f* gen eff_size@
				    snd chn effects-squelch-channel drop
			then
		end-each
	else
		gen eff_amnt@ dup f* gen eff_size@ #f #f
		    effects-squelch-channel drop
	then
;

: gate-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init }
	gen init eff_amnt!
	gen eff_sliders@ 0 array-ref init 1000.0 set-slider-value
;

: gate-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1000.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

'snd-motif provided? [if]
	: gate-omit-cb <{ w gen info -- }>
		gen info Fset eff_omit_silence!
	;

	: post-gate-dialog ( gen -- prc; w c i self -- )
		3 proc-create swap , ( prc )
  	  does> { w c info self -- }
		self @ { gen }
		gen eff_dialog@ widget? unless
			gen eff_label@ gen gate-ok-cb gen
			    eff_label@ "\
Move the slider to change the gate intensity.  \
Higher values gate more of the sound." help-cb
			    gen gate-reset-cb #f make-effect-dialog { d }
			gen d eff_dialog!
			d #( #( "gate" 0.0 gen eff_amnt@ 0.1
				gen gate-slider-cb 1000 ) ) add-sliders ( sl )
			gen swap eff_sliders!
			"Omit silence" FXmStringCreateLocalized { s1 }
			"Omit silence" FxmToggleButtonWidgetClass
			    gen eff_sliders@ 0 array-ref FXtParent
			    #( FXmNbackground basic-color
			       FXmNvalue gen eff_omit_silence@ if 1 else 0 then
			       FXmNlabelString s1 ) undef
			    FXtCreateManagedWidget ( toggle )
			FXmNvalueChangedCallback <'> gate-omit-cb gen
			    FXtAddCallback drop
			s1 FXmStringFree drop
		then
		gen eff_dialog@ activate-dialog
	;
[else]
	: gate-omit-cb <{ w gen -- }>
		w FGTK_TOGGLE_BUTTON Fgtk_toggle_button_get_active ( sl )
		gen swap eff_omit_silence!
	;

	: post-gate-dialog ( gen -- prc; w d self -- )
		2 proc-create swap , ( prc )
	  does> { w d self -- }
		self @ { gen }
		gen eff_dialog@ widget? unless
			gen eff_label@ gen gate-ok-cb gen eff_label@ "
Move the slider to change the gate intensity.  \
Higher values gate more of the sound." help-cb
			    gen gate-reset-cb #f make-effect-dialog { d }
			gen d eff_dialog!
			d #( #( "gate" 0.0 gen eff_amnt@ 0.1
				gen gate-slider-cb 1000 ) ) add-sliders ( sl )
			gen swap eff_sliders!
			"Omit silence" Fgtk_check_button_new_with_label { tog }
			gen eff_dialog@ FGTK_DIALOG Fgtk_dialog_get_content_area
			    FGTK_BOX tog #f #f 4 Fgtk_box_pack_start drop
			tog Fgtk_widget_show drop
			tog "clicked" <'> gate-omit-cb gen
			    Fg_signal_connect drop
		then
		gen eff_dialog@ activate-dialog
	;
[then]
set-current

: make-gate-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.01 eff_amnt!
	gen 128 eff_size!
	gen #f eff_omit_silence!
	gen post-gate-dialog ( prc1 )
	1 proc-create gen ,  ( prc2 )
  does> ( child self -- prc; self -- )
	{ child self }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.4f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === DELAY EFFECTS ===

\ === Echo (controlled by delay-time and echo-amount) ===

hide
: echo-func-cb ( gen -- prc; samps self -- prc; inval self -- res )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_delay@ #f srate f* f>s make-delay { del }
	1 proc-create 0 , samps , del , gen , ( prc )
  does> { inval self -- res }
	self @ 1+ dup self ! { samp }
	self 1 cells + @ { samps }
	self 2 cells + @ { del }
	self 3 cells + @ { gen }
	del dup 0.0 tap samp samps <= if
		inval f+
	then gen eff_amnt@ f* 0.0 delay inval f+
;

: echo-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-echo"
	"%s %s %s"
	    #( target 'sound = if
		       #f
	       else
		       samps
	       then gen eff_delay@ gen eff_amnt@ ) string-format
;

: echo-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen echo-func-cb gen eff_target@ gen echo-origin-cb gen eff_trunc@ if
		#f
	else
		4.0 gen eff_delay@ f*
	then map-chan-over-target-with-sync
;

: echo-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , gen eff_delay@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-echo }
	self 2 cells + @ { init-delay }
	gen init-echo eff_amnt!
	gen init-delay eff_delay!
	gen eff_sliders@ 0 array-ref init-delay 100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-echo  100.0 set-slider-value
;

: echo-delay-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_delay!
;

: echo-amount-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-echo-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen echo-ok-cb gen eff_label@ "\
The sliders change the delay time and echo amount." help-cb
		    gen echo-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "delay time" 0.0 gen eff_delay@ 2.0
			gen echo-delay-slider-cb  100 )
		     #( "echo amount" 0.0 gen eff_amnt@ 1.0
			gen echo-amount-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen <'> truncate-cb add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-echo-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.5 eff_delay!
	gen 0.2 eff_amnt!
	gen post-echo-dialog ( prc1 )
	1 proc-create gen ,  ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_delay@ gen eff_amnt@ )
	    string-format change-label
;
previous

\ === Filtered Echo ===

hide
: flecho-func-cb ( gen -- prc; samps self -- prc; inval self -- res )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	:order 4 :xcoeffs vct( 0.125 0.25 0.25 0.125 ) make-fir-filter { flt }
	gen eff_delay@ #f srate f* fround->s make-delay { del }
	1 proc-create 0 , samps , flt , del , gen eff_amnt@ , ( prc )
  does> { inval self -- res }
	self @ 1+ dup self ! { samp }
	self 1 cells + @ { samps }
	self 2 cells + @ { flt }
	self 3 cells + @ { del }
	self 4 cells + @ { scl }
	del flt del 0.0 tap samp samps <= if
		inval f+
	then scl f* fir-filter delay inval f+
;

: flecho-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-flecho"
	"%s %s %s"
	    #( gen eff_amnt@
	       gen eff_delay@
	       target 'sound = if
		       #f
	       else
		       samps
	       then ) string-format
;

: flecho-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen flecho-func-cb gen eff_target@ gen flecho-origin-cb
	    gen eff_trunc@ if
		#f
	else
		4.0 gen eff_delay@ f*
	then map-chan-over-target-with-sync
;

: flecho-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , gen eff_delay@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-scaler }
	self 2 cells + @ { init-delay }
	gen init-scaler eff_amnt!
	gen init-delay eff_delay!
	gen eff_sliders@ 0 array-ref init-scaler 100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-delay  100.0 set-slider-value
;

: post-flecho-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen flecho-ok-cb gen eff_label@ "\
Move the sliders to set the filter scaler \
and the delay time in seconds." help-cb
		    gen flecho-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "filter scaler" 0.0 gen eff_amnt@ 1.0
			gen echo-amount-slider-cb 100 )
		     #( "delay time (secs)" 0.0 gen eff_delay@ 3.0
			gen echo-delay-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen <'> truncate-cb add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-flecho-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.9 eff_delay!
	gen 0.5 eff_amnt!
	gen post-flecho-dialog ( prc1 )
	1 proc-create gen ,    ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_amnt@ gen eff_delay@ )
	    string-format change-label
;
previous

\ === Modulated Echo ===

hide
: zecho-func-cb ( gen -- prc; samps self -- prc; inval self -- res )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_freq@ make-oscil { os }
	gen eff_delay@ #f srate f* fround->s { len }
	:size len :max-size len gen eff_amp@ f>s 1+ + make-delay { del }
	1 proc-create ( prc )
	0 , samps , os , del , gen eff_scl@ , gen eff_amp@ ,
  does> { inval self -- res }
	self @ { samp }
	1 self +! ( samp++ )
	self 1 cells + @ { samps }
	self 2 cells + @ { os }
	self 3 cells + @ { del }
	self 4 cells + @ { scl }
	self 5 cells + @ { amp }
	del ( del-gen ) del 0.0 tap  samp samps < if
		inval f+
	then  scl f* ( input ) os 0.0 0.0 oscil amp f* ( pm ) delay inval f+
;

: zecho-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-zecho"
	"%s %s %s %s %s"
	    #( gen eff_scl@
	       gen eff_delay@
	       gen eff_freq@
	       gen eff_amp@
	       target 'sound = if
		       #f
	       else
		       samps
	       then ) string-format
;

: zecho-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen zecho-func-cb gen eff_target@ gen zecho-origin-cb gen eff_trunc@ if
		#f
	else
		4.0 gen eff_delay@ f*
	then map-chan-over-target-with-sync
;

: zecho-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen ,
	gen eff_scl@ ,
	gen eff_delay@ ,
	gen eff_freq@ ,
	gen eff_amp@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-scaler }
	self 2 cells + @ { init-delay }
	self 3 cells + @ { init-freq }
	self 4 cells + @ { init-amp }
	gen init-scaler eff_scl!
	gen init-delay eff_delay!
	gen init-freq eff_freq!
	gen init-amp eff_amp!
	gen eff_sliders@ 0 array-ref init-scaler 100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-delay  100.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-freq   100.0 set-slider-value
	gen eff_sliders@ 3 array-ref init-amp    100.0 set-slider-value
;

: zecho-del-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value self { val }
	@ ( gen ) val eff_delay!
;

: post-zecho-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen zecho-ok-cb gen eff_label@ "
Move the sliders to set the echo scaler, \
the delay time in seconds, the modulation frequency, \
and the echo amplitude." help-cb gen
		    zecho-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "echo scaler" 0.0 gen eff_scl@ 1.0
			gen scaler-slider-cb 100 )
		     #( "delay time (secs)" 0.0 gen eff_delay@ 3.0
			gen zecho-del-slider-cb 100 )
		     #( "modulatio frequency" 0.0 gen eff_freq@ 100.0
			gen frequency-slider-cb 100 )
		     #( "modulatio amplitude" 0.0 gen eff_amp@ 100.0
			gen amplitude-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen <'> truncate-cb add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-zecho-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.50 eff_scl!
	gen 0.75 eff_delay!
	gen 6.00 eff_freq!
	gen 10.0 eff_amp!
	gen post-zecho-dialog ( prc1 )
	1 proc-create gen ,   ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f %.2f %.2f)"
	    #( gen eff_label@
	       gen eff_scl@
	       gen eff_delay@
	       gen eff_freq@
	       gen eff_amp@ ) string-format change-label
;
previous

\ === FILTER EFFECTS ===

\ === Butterworth band-pass filter ===

hide
: bp-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_freq@ gen eff_bp_bw@ make-butter-band-pass { flt }
	gen eff_target@ 'sound = if
		"%s %s 0 #f effects-bbp"
		    #( gen eff_freq@ gen eff_bp_bw@ )
		    string-format { origin }
		flt #f #f #f #f origin filter-sound
	else
		gen eff_target@ 'selection = if
			flt #f #f filter-selection
		else
			plausible-mark-samples { pts }
			pts 0 array-ref { bg }
			pts 1 array-ref bg - 1+ { nd }
			"%s %s %s %s effects-bbp"
			    #( gen eff_freq@ gen eff_bp_bw@ bg nd )
			    string-format { origin }
			flt bg nd #f #f #f #f origin clm-channel
		then
	then
;

: bp-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , gen eff_bp_bw@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-freq }
	self 2 cells + @ { init-bw }
	gen init-freq eff_freq!
	gen init-bw eff_bp_bw!
	gen eff_sliders@ 0 array-ref 20.0 init-freq 22050.0
	    scale-log->linear 1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-bw 1.0 set-slider-value
;

: bp-bw-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_bp_bw!
;

: post-band-pass-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> ( w c i self -- )
	{ w c info self }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen bp-ok-cb gen eff_label@ "\
Butterworth band-pass filter.  \
Move the sliders to change the center frequency and bandwidth." help-cb
		    gen bp-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "center frequency" 20.0 gen eff_freq@ 22050.0
			gen log-freq-slider-cb 1 'log )
		     #( "bandwidth" 0 gen eff_bp_bw@ 1000
			gen bp-bw-slider-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-band-pass-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name) make-base-effects { gen }
	gen 1000.0 eff_freq!
	gen 100 eff_bp_bw!
	gen post-band-pass-dialog ( prc1 )
	1 proc-create gen ,       ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %d)"
	    #( gen eff_label@ gen eff_freq@ gen eff_bp_bw@ )
	    string-format change-label
;
previous

\ === Butterworth band-reject (notch) filter ===

hide
: br-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_freq@ gen eff_notch_bw@ make-butter-band-reject { flt }
	gen eff_target@ 'sound = if
		"%s %s 0 #f effects-bbr" #( gen eff_freq@ gen eff_notch_bw@ )
		    string-format { origin }
		flt #f #f #f #f origin filter-sound
	else
		gen eff_target@ 'selection = if
			flt #f #f filter-selection
		else
			plausible-mark-samples { pts }
			pts 0 array-ref { bg }
			pts 1 array-ref bg - 1+ { nd }
			"%s %s %s %s effects-bbp"
			    #( gen eff_freq@ gen eff_notch_bw@ bg nd )
			    string-format { orig }
			flt bg nd #f #f #f #f orig clm-channel
		then
	then
;

: br-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , gen eff_notch_bw@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-freq }
	self 2 cells + @ { init-bw }
	gen init-freq eff_freq!
	gen init-bw eff_notch_bw!
	gen eff_sliders@ 0 array-ref 20.0 init-freq 22050.0
	    scale-log->linear 1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-bw 1.0 set-slider-value
;

: br-bw-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_notch_bw!
;

: post-notch-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen br-ok-cb gen eff_label@ "\
Butterworth band-reject filter.  \
Move the sliders to change the center frequency and bandwidth." help-cb
		    gen br-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "center frequency" 20.0 gen eff_freq@ 22050.0
			gen log-freq-slider-cb 1 'log )
		     #( "bandwidth" 0 gen eff_notch_bw@ 1000
			gen br-bw-slider-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-notch-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 100.0 eff_freq!
	gen 100 eff_notch_bw!
	gen post-notch-dialog ( prc1 )
	1 proc-create gen ,   ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %d)"
	    #( gen eff_label@ gen eff_freq@ gen eff_notch_bw@ )
	    string-format change-label
;
previous

\ === Butterworth high-pass filter ===

hide
: hp-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_freq@ make-butter-high-pass { flt }
	gen eff_target@ 'sound = if
			"%s 0 #f effects-bhp"
			    #( gen eff_freq@ ) string-format { origin }
			flt #f #f #f #f origin filter-sound
		else
			gen eff_target@ 'selection = if
				flt #f #f filter-selection
			else
				plausible-mark-samples { pts }
				pts 0 array-ref { bg }
				pts 1 array-ref bg - 1+ { nd }
				"%s %s %s effects-bhp"
				    #( gen eff_freq@ bg nd )
				    string-format { origin }
				flt bg nd #f #f #f #f origin clm-channel
		then
	then
;

: hp-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , ( prc )
  does> { w c info self -- }
	self @ { gen  }
	self cell+ @ { init-freq }
	gen init-freq eff_freq!
	gen eff_sliders@ 0 array-ref 20.0 init-freq 22050.0
	    scale-log->linear 1.0 set-slider-value
;

: post-high-pass-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen hp-ok-cb gen eff_label@ "\
Butterworth high-pass filter.  \
Move the slider to change the high-pass cutoff frequency." help-cb
		    gen hp-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "high-pass cutoff frequency" 20.0 gen eff_freq@ 22050.0
			gen log-freq-slider-cb 1 'log ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-high-pass-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 100.0 eff_freq!
	gen post-high-pass-dialog ( prc1 )
	1 proc-create gen ,       ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_freq@ ) string-format change-label
;
previous

\ === Butterworth low-pass filter ===

hide
: lp-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_freq@ make-butter-low-pass { flt }
	gen eff_target@ 'sound = if
		"%s 0 #f effects-blp" gen eff_freq@ string-format { origin }
		flt #f #f #f #f origin filter-sound
	else
		gen eff_target@ 'selection = if
			flt #f #f filter-selection
		else
			plausible-mark-samples { pts }
			pts 0 array-ref { bg }
			pts 1 array-ref bg - 1+ { nd }
			"%s %s %s effects-blp"
			    #( gen eff_freq@ bg nd ) string-format { origin }
			flt bg nd #f #f #f #f origin clm-channel
		then
	then
;

: lp-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init-freq }
	gen init-freq eff_freq!
	gen eff_sliders@ 0 array-ref 20.0 init-freq 22050.0
	    scale-log->linear 1.0 set-slider-value
;

: post-low-pass-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen lp-ok-cb gen eff_label@ "\
Butterworth low-pass filter.  \
Move the slider to change the low-pass cutoff frequency." help-cb
		    gen lp-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "low-pass cutoff frequency" 20.0 gen eff_freq@ 22050.0
			gen log-freq-slider-cb 1 'log ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-low-pass-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1000.0 eff_freq!
	gen post-low-pass-dialog ( prc1 )
	1 proc-create gen ,      ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_freq@ ) string-format change-label
;
previous

\ === Comb filter ===

hide
: comb-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_scl@ gen eff_size@ comb-filter
;

: comb-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-comb-filter"
	"%s %s" #( gen eff_scl@ gen eff_size@ ) string-format
;

: comb-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen comb-func-cb gen eff_target@ gen comb-origin-cb #f
	    map-chan-over-target-with-sync
;

: comb-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_scl@ , gen eff_size@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-scaler }
	self 2 cells + @ { init-size }
	gen init-scaler eff_scl!
	gen init-size eff_size!
	gen eff_sliders@ 0 array-ref init-scaler 100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-size     1.0 set-slider-value
;

: post-comb-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen comb-ok-cb gen eff_label@ "\
Move the slider to change the comb scaler and size." help-cb gen comb-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "scaler" 0.0 gen eff_scl@ 1.0
			gen scaler-slider-cb 100 )
		     #( "size" 0 gen eff_size@ 100
			gen size-slider-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-comb-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.1 eff_scl!
	gen 50 eff_size!
	gen post-comb-dialog ( prc1 )
	1 proc-create gen ,  ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %d)"
	    #( gen eff_label@ gen eff_scl@ gen eff_size@ )
	    string-format change-label
;
previous

\ === Comb-chord filter ===

hide
: cc-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_scl@ gen eff_size@ gen eff_amp@ comb-chord
;

: cc-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-comb-chord"
	"%s %s %s" #( gen eff_scl@ gen eff_size@ gen eff_amp@ ) string-format
;

: cc-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen cc-func-cb gen eff_target@ gen cc-origin-cb #f
	    map-chan-over-target-with-sync
;

: cc-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_scl@ , gen eff_size@ , gen eff_amp@ ,
  does> { w c info self -- }
	self           @ { gen }
	self 1 cells + @ { init-scaler }
	self 2 cells + @ { init-size }
	self 3 cells + @ { init-amp }
	gen init-scaler eff_scl!
	gen init-size eff_size!
	gen init-amp eff_amp!
	gen eff_sliders@ 0 array-ref init-scaler 100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-size     1.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-amp    100.0 set-slider-value
;

: post-cc-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen cc-ok-cb gen eff_label@ "\
Creates chords by using filters at harmonically related sizes.  \
Move the sliders to set the comb chord parameters." help-cb gen cc-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "chord scaler" 0.0 gen eff_scl@ 1.0
			gen scaler-slider-cb 100 )
		     #( "chord size" 0 gen eff_size@ 100
			gen size-slider-cb 1 )
		     #( "amplitude" 0.0 gen eff_amp@ 1.0
			gen amplitude-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-comb-chord-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.95 eff_scl!
	gen 60 eff_size!
	gen 0.3  eff_amp!
	gen post-cc-dialog  ( prc1 )
	1 proc-create gen , ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %d %.2f)"
	    #( gen eff_label@ gen eff_scl@ gen eff_size@ gen eff_amp@ )
	    string-format change-label
;
previous

\ === Moog filter ===

hide
: moog-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_freq@ gen eff_moog_reson@ moog
;

: moog-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-moog"
	"%s %s" #( gen eff_freq@ gen eff_moog_reson@ ) string-format
;

: moog-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen moog-func-cb gen eff_target@ gen moog-origin-cb #f
	    map-chan-over-target-with-sync
;

: moog-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , gen eff_moog_reson@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-freq }
	self 2 cells + @ { init-res }
	gen init-freq eff_freq!
	gen init-res eff_moog_reson!
	gen eff_sliders@ 0 array-ref 20.0 init-freq 22050.0
	    scale-log->linear 1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-res 100.0 set-slider-value
;

: moog-res-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_moog_reson!
;

: post-moog-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen moog-ok-cb gen eff_label@ "\
Moog-style 4-pole lowpass filter \
with 24db/oct rolloff and variable resonance.  \
Move the sliders to set the filter \
cutoff frequency and resonance." help-cb gen moog-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "cutoff frequency" 20.0 gen eff_freq@ 22050.0
			gen log-freq-slider-cb 1 'log )
		     #( "resonanze" 0.0 gen eff_moog_reson@ 1.0
			gen moog-res-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-moog-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 10000.0 eff_freq!
	gen 0.5 eff_moog_reson!
	gen post-moog-dialog ( prc1 )
	1 proc-create gen ,  ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_freq@ gen eff_moog_reson@ )
	    string-format change-label
;
previous

\ === FREQUENCY EFFECTS ===

\ === Adaptive saturation ===

hide
: adsat-func-cb ( gen -- prc; samps self -- prc; val self -- res )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	1 proc-create ( prc )
	gen , gen eff_size@ 0.0 make-vct , 0.0 , 0.0 , 0 ,
  does> { val self -- res }
	self @ { gen }
	self 1 cells + @ { vals }
	self 2 cells + @ { mn }
	self 3 cells + @ { mx }
	self 4 cells + @ { n }
	gen eff_size@ n = if
		vals each { x }
			vals i  x f0>= if
				mx
			else
				mn
			then  vct-set! drop
			0.0 self 2 cells + ! ( mn )
			0.0 self 3 cells + ! ( mx )
			0   self 4 cells + ! ( n )
		end-each
		vals
	else
		vals n val vct-set! drop
		val mx f> if
			val self 3 cells + ! ( mx )
		then
		val mn f< if
			val self 2 cells + ! ( mn )
		then
		n 1+ self 4 cells + ! ( n++ )
		#f
	then
;

: adsat-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"adsat"
	gen eff_size@ number->string
;

: adsat-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen adsat-func-cb gen eff_target@ gen adsat-origin-cb #f
	    map-chan-over-target-with-sync
;

: adsat-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_size@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init-size }
	gen init-size eff_size!
	gen eff_sliders@ 0 array-ref init-size 1.0 set-slider-value
;

: post-adsat-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen adsat-ok-cb gen eff_label@ "\
Move the slider to change the saturation scaling factor." help-cb
		    gen adsat-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "adaptive saturation size" 0 gen eff_size@ 10
			gen size-slider-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-adsat-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 4 eff_size!
	gen post-adsat-dialog ( prc1 )
	1 proc-create gen ,   ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%d)"
	    #( gen eff_label@ gen eff_size@ ) string-format change-label
;
previous

\ === Sample rate conversion (resample) ===

hide
: src-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_target@ 'sound = if
		gen eff_amnt@ 1.0 undef undef undef src-sound
	else
		gen eff_target@ 'selection = if
			undef selection? if
				gen eff_amnt@ 1.0 src-selection
			else
				"no selection" undef status-report
			then
		else
			"can't apply src between marks yet" undef status-report
		then
	then
;

: src-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init-amount }
	gen init-amount eff_amnt!
	gen eff_sliders@ 0 array-ref init-amount 100.0 set-slider-value
;

: src-amount-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-src-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen src-ok-cb gen eff_label@ "\
Move the slider to change the sample rate.  \
Values greater than 1.0 speed up file play, \
negative values reverse it." help-cb gen src-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "sample rate" -2.0 gen eff_amnt@ 2.0
			gen src-amount-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-src-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.0 eff_amnt!
	gen post-src-dialog ( prc1 )
	1 proc-create gen , ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === Time and pitch scaling by granular synthesis and sampling rate conversion

hide
: expsrc-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	selected-sound { snd }
	snd save-controls drop
	snd reset-controls drop
	gen eff_pitch_scl@ snd set-speed-control drop
	gen eff_pitch_scl@ gen eff_time_scale@ f* { new-time }
	new-time 1.0 f<> if
		#t                  snd set-expand-control?       drop
		new-time            snd set-expand-control        drop
		gen eff_hop_size@       snd set-expand-control-hop    drop
		gen eff_seg_len@ snd set-expand-control-length drop
		gen eff_ramp_scl@     snd set-expand-control-ramp   drop
	then
	gen eff_target@ 'marks = if
		plausible-mark-samples { pts }
		pts if
			snd 0
			    pts 0 array-ref
			    pts 1 array-ref
			    pts 0 array-ref - 1+ apply-controls
		else
			"no marks" undef status-report
		then
	else
		snd gen eff_target@ 'sound = if
			0
		else
			2
		then undef undef apply-controls
	then drop
	snd restore-controls
;

: expsrc-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen ,
	gen eff_time_scale@ ,
	gen eff_hop_size@ ,
	gen eff_seg_len@ ,
	gen eff_ramp_scl@ ,
	gen eff_pitch_scl@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-time-scale }
	self 2 cells + @ { init-size }
	self 3 cells + @ { init-seg-len }
	self 4 cells + @ { init-ramp-scale }
	self 5 cells + @ { init-pitch-scale }
	gen init-time-scale eff_time_scale!
	gen init-size eff_hop_size!
	gen init-seg-len eff_seg_len!
	gen init-ramp-scale eff_ramp_scl!
	gen init-pitch-scale eff_pitch_scl!
	gen eff_sliders@ 0 array-ref gen init-time-scale  100.0 set-slider-value
	gen eff_sliders@ 1 array-ref gen init-size        100.0 set-slider-value
	gen eff_sliders@ 2 array-ref gen init-seg-len     100.0 set-slider-value
	gen eff_sliders@ 3 array-ref gen init-ramp-scale  100.0 set-slider-value
	gen eff_sliders@ 4 array-ref gen init-pitch-scale 100.0 set-slider-value
;

: expsrc-ts-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_time_scale!
;

: expsrc-hs-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_hop_size!
;

: expsrc-sl-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_seg_len!
;

: expsrc-rs-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_ramp_scl!
;

: expsrc-ps-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_pitch_scl!
;

: post-expsrc-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen expsrc-ok-cb gen eff_label@ "\
Move the slider to change the time/pitch scaling parameter." help-cb
		    gen expsrc-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "time scale" 0.0 gen eff_time_scale@ 5.0
			gen expsrc-ts-cb 100 )
		     #( "hop size" 0.0 gen eff_hop_size@ 1.0
			gen expsrc-hs-cb 100 )
		     #( "segment-length" 0.0 gen eff_seg_len@ 0.5
			gen expsrc-sl-cb 100 )
		     #( "ramp scale" 0.0 gen eff_ramp_scl@ 0.5
			gen expsrc-rs-cb 100 )
		     #( "pitch scale" 0.0 gen eff_pitch_scl@ 5.0
			gen expsrc-ps-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-expsrc-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.00 eff_time_scale!
	gen 0.05 eff_hop_size!
	gen 0.15 eff_seg_len!
	gen 0.50 eff_ramp_scl!
	gen 1.00 eff_pitch_scl!
	gen post-expsrc-dialog ( prc1 )
	1 proc-create gen ,    ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_time_scale@ gen eff_pitch_scl@ )
	    string-format change-label
;
previous

\ === Time-varying sample rate conversion (resample) ===
\ (KSM)

hide
: src-timevar-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_enved@ xe-envelope gen eff_scl@ scale-envelope { en }
	gen eff_target@ 'sound = if
		en 1.0 #f #f #f src-sound drop
	else
		gen eff_target@ 'selection = if
			selected-sound #f selection-member? if
				en 1.0 src-selection drop
			else
				"no selection" undef status-report drop
			then
		else
			plausible-mark-samples { pts }
			pts if
				pts 0 array-ref { beg }
				pts 1 array-ref { end }
				end beg - { len }
				:envelope en :length len make-env beg len
				    selected-sound #f #f src-channel drop
			else
				"no marks" undef status-report drop
			then
		then
	then
;

: src-timevar-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_scl@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init }
	gen init eff_scl!
	gen eff_enved@ #( 0.0 1.0 1.0 1.0 ) set-xe-envelope
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: post-src-timevar-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen src-timevar-ok-cb gen eff_label@ "\
Move the slider to change the src-timevar scaling amount." help-cb
		    gen src-timevar-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "Resample factor" 0.0 gen eff_scl@ 10.0
			gen scaler-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen make-enved-widget
	else
		gen eff_dialog@ activate-dialog
	then
;
set-current

: make-src-timevar-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_scl!
	gen #f eff_enved!
	gen post-src-timevar-dialog ( prc1 )
	1 proc-create gen ,         ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "Time-varying sample rate scaling" change-label
;
previous

\ === MODULATION EFFECTS ===

\ === Amplitude modulation ===

hide
: am-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_amnt@ make-oscil { os }
	gen eff_enved@ xe-envelope #( 0.0 1.0 1.0 1.0 ) equal? if
		#f
	else
		:envelope gen eff_enved@ xe-envelope :length gen eff_target@
		    effect-frames 1- make-env
	then { e }
	e if
		os e effects-am-env-cb
	else
		os effects-am-cb
	then
;

: am-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-am"
	"%s %s"
	    #( gen eff_amnt@
	       gen eff_enved@
	       xe-envelope #( 0.0 1.0 1.0 1.0 ) equal? if
		       #f
	       else
		       gen eff_enved@ xe-envelope
	       then ) string-format
;

: am-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen am-func-cb gen eff_target@ gen am-origin-cb #f
	    map-chan-over-target-with-sync
;

: am-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self cell+ @ { init }
	gen init eff_amnt!
	gen eff_enved@ #( 0.0 1.0 1.0 1.0 ) set-xe-envelope
	gen eff_sliders@ 0 array-ref init 1.0 set-slider-value
;

: am-slider-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-am-effect-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen am-ok-cb gen eff_label@ "\
Move the slider to change the modulation amount." help-cb gen am-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "amplitude modulation" 0.0 gen eff_amnt@ 1000.0
		        gen am-slider-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen make-enved-widget
	else
		gen eff_dialog@ activate-dialog
	then
;
set-current

: make-am-effect-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 100.0 eff_amnt!
	gen #f eff_enved!
	gen post-am-effect-dialog ( prc1 )
	1 proc-create gen ,       ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === Ring modulation ===

hide
: rm-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_freq@ make-oscil { os }
	gen eff_enved@ xe-envelope #( 0.0 1.0 1.0 1.0 ) equal? if
		#f
	else
		:envelope gen eff_enved@ xe-envelope
		    :length gen eff_target@ effect-frames 1- make-env
	then { e }
	e if
		os e effects-rm-env-cb
	else
		os effects-rm-cb
	then
;

: rm-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-rm"
	"%s %s"
	    #( gen eff_freq@
	       gen eff_enved@
	       xe-envelope #( 0.0 1.0 1.0 1.0 ) equal? if
		       #f
	       else
		       gen eff_enved@ xe-envelope
	       then ) string-format
;

: rm-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen rm-func-cb gen eff_target@ gen rm-origin-cb #f
	    map-chan-over-target-with-sync
;

: rm-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , gen eff_scl@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-freq }
	self 2 cells + @ { init-radians }
	gen init-freq eff_freq!
	gen init-radians eff_scl!
	gen eff_enved@ #( 0.0 1.0 1.0 1.0 ) set-xe-envelope
	gen eff_sliders@ 0 array-ref init-freq    1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-radians 1.0 set-slider-value
;

: rm-freq-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_freq!
;

: rm-radians-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_scl!
;

: post-rm-effect-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen rm-ok-cb gen eff_label@ "\
Move the slider to change ring modulation parameters." help-cb gen rm-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "modulation frequency" 0 gen eff_freq@ 1000
			gen rm-freq-cb 1 )
		     #( "modulation radians" 0 gen eff_scl@ 360
			gen rm-radians-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen make-enved-widget
	else
		gen eff_dialog@ activate-dialog
	then
;
set-current

: make-rm-effect-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 100.0 eff_freq!
	gen 100.0 eff_scl!
	gen #f eff_enved!
	gen post-rm-effect-dialog ( prc1 )
	1 proc-create gen ,       ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_freq@ gen eff_scl@ )
	    string-format change-label
;
previous

\ === REVERBS ===

\ === Reverb from Michael McNabb's Nrev ===

hide
: nrev-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	selected-sound { snd }
	snd save-controls drop
	snd reset-controls drop
	#t                  snd set-reverb-control?         drop
	gen eff_amnt@       snd set-reverb-control-scale    drop
	gen eff_rev_filter@ snd set-reverb-control-lowpass  drop
	gen eff_rev_fb@ snd set-reverb-control-feedback drop
	gen eff_target@ 'marks = if
		plausible-mark-samples { pts }
		pts array? if
		snd 0
		    pts 0 array-ref
		    pts 1 array-ref
		    pts 0 array-ref - 1+ apply-controls drop
		else
			"no marks" undef status-report drop
		then
	else
		snd gen eff_target@ 'sound = if
			0
		else
			2
		then undef undef apply-controls drop
	then
	snd restore-controls
;

: nrev-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_amnt@ , gen eff_rev_filter@ , gen eff_rev_fb@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-amount }
	self 2 cells + @ { init-filter }
	self 3 cells + @ { init-feedback }
	gen init-amount eff_amnt!
	gen init-filter eff_rev_filter!
	gen init-feedback eff_rev_fb!
	gen eff_sliders@ 0 array-ref init-amount   100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-filter   100.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-feedback 100.0 set-slider-value
;

: nrev-amount-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: nrev-filter-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_rev_filter!
;

: nrev-feedback-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_rev_fb!
;

: post-reverb-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen nrev-ok-cb gen eff_label@ "\
Reverberator from Michael McNabb.  \
Adds reverberation scaled by reverb amount, lowpass filtering, and feedback.  \
Move the sliders to change the reverb parameters." help-cb gen nrev-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "reverb amount" 0.0 gen eff_amnt@ 1.00
			gen nrev-amount-cb 100 )
		     #( "reverb filter" 0.0 gen eff_rev_filter@ 1.00
			gen nrev-filter-cb 100 )
		     #( "reverb feedback" 0.0 gen eff_rev_fb@ 1.25
			gen nrev-feedback-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-reverb-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0.10 eff_amnt!
	gen 0.50 eff_rev_filter!
	gen 1.09 eff_rev_fb!
	gen post-reverb-dialog ( prc1 )
	1 proc-create gen ,    ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f %.2f)"
	    #( gen eff_label@
	       gen eff_amnt@
	       gen eff_rev_filter@
	       gen eff_rev_fb@ ) string-format change-label
;
previous

\ === Chowning reverb ===

hide
: jc-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	samps gen eff_rev_vol@ effects-jc-reverb
;

: jc-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-jc-reverb-1"
	gen eff_rev_vol@ number->string
;

: jc-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen jc-func-cb gen eff_target@ gen jc-origin-cb gen eff_trunc@ if
		#f
	else
		gen eff_rev_decay@
	then map-chan-over-target-with-sync
;

: jc-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_rev_decay@ , gen eff_rev_vol@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-decay }
	self 2 cells + @ { init-volume }
	gen init-decay eff_rev_decay!
	gen init-volume eff_rev_vol!
	gen eff_sliders@ 0 array-ref init-decay  100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-volume 100.0 set-slider-value
;

: jc-decay-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_rev_decay!
;

: jc-volume-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_rev_vol!
;

: post-jc-reverb-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen jc-ok-cb gen eff_label@ "\
Nice reverb from John Chowning.  \
Move the sliders to set the reverb parameters." help-cb gen jc-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "decay duration" 0.0 gen eff_rev_decay@ 10.0
			gen jc-decay-cb 100 )
		     #( "reverb volume" 0.0 gen eff_rev_vol@ 1.00
			gen jc-volume-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen <'> truncate-cb add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-jc-reverb-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 2.0 eff_rev_decay!
	gen 0.1 eff_rev_vol!
	gen post-jc-reverb-dialog ( prc1 )
	1 proc-create gen ,       ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_rev_decay@ gen eff_rev_vol@ )
	    string-format change-label
;
previous

\ === Convolution ===

hide
: cnv-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_conv_one@ { snd1 }
	gen eff_conv_two@ { snd2 }
	snd1 sound? if
		snd2 sound? if
			snd1 gen eff_amp@ snd2 #f effects-cnv
		else
			"no such sound two: %S" #( snd2 )
			    string-format undef status-report
		then
	else
		"no such sound one: %S"
		    #( snd1 ) string-format undef status-report
	then
;

: cnv-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_conv_one@ , gen eff_conv_two@ , gen eff_amp@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-one }
	self 2 cells + @ { init-two }
	self 3 cells + @ { init-amp }
	gen init-one eff_conv_one!
	gen init-two eff_conv_two!
	gen init-amp eff_amp!
	gen eff_sliders@ 0 array-ref init-one   1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-two   1.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-amp 100.0 set-slider-value
;

: cnv-one-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_conv_one!
;

: cnv-two-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_conv_two!
;

: post-convolve-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen cnv-ok-cb gen eff_label@ "\
Very simple convolution.  \
Move the sliders to set the reverb parameters the numbers of the soundfiles \
to be convolved and the amount for the amplitude scaler.  \
Output will be scaled to floating-point values, \
resulting in very large (but not clipped) amplitudes.  \
Use the Normalize amplitude effect to rescale the output.  \
The convolution data file typically defines a natural reverberation source, \
and the output from this effect can provide very striking reverb effects.  \
You can find convolution data files on sites listed at \
http://www.bright.net/~dlphilp/linux_csound.html under Impulse Response Data."
		    help-cb gen cnv-reset-cb #f make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "impulse response file" 0 gen eff_conv_one@ 24
			gen cnv-one-cb 1 )
		     #( "sound file" 0 gen eff_conv_two@ 24
			gen cnv-two-cb 1 )
		     #( "amplitude" 0.0 gen eff_amp@ 0.10
			gen amplitude-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-convolve-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0 eff_conv_one!
	gen 1 eff_conv_two!
	gen 0.01 eff_amp!
	gen post-convolve-dialog ( prc1 )
	1 proc-create gen ,      ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%d %d %.2f)"
	    #( gen eff_label@ gen eff_conv_one@ gen eff_conv_two@ gen eff_amp@ )
	    string-format change-label
;
previous

\ === VARIOUS AND MISCELLANEOUS ===

\ === Place sound ===

hide
: ps-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	gen eff_enved@ xe-envelope { e }
	e #( 0.0 1.0 1.0 1.0 ) equal? if
		gen eff_m_snd@ gen eff_s_snd@ gen eff_pan_pos@
	else
		gen eff_m_snd@ gen eff_s_snd@ e
	then effects-place-sound
;

: ps-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_m_snd@ , gen eff_s_snd@ , gen eff_pan_pos@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-mono }
	self 2 cells + @ { init-stereo }
	self 3 cells + @ { init-pos }
	gen init-mono eff_m_snd!
	gen init-stereo eff_s_snd!
	gen init-pos eff_pan_pos!
	gen eff_enved@ #( 0.0 1.0 1.0 1.0 ) set-xe-envelope
	gen eff_sliders@ 0 array-ref init-mono   1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-stereo 1.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-pos    1.0 set-slider-value
;

: ps-mono-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_m_snd!
;

: ps-stereo-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_s_snd!
;

: ps-pos-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_pan_pos!
;

: post-place-sound-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen ps-ok-cb gen eff_label@ "\
Mixes mono sound into stereo sound field." help-cb gen ps-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "mono sound" 0 gen eff_m_snd@ 50
			gen ps-mono-cb 1 )
		     #( "stereo sound" 0 gen eff_s_snd@ 50
			gen ps-stereo-cb 1 )
		     #( "pan position" 0 gen eff_pan_pos@ 90
			gen ps-pos-cb 1 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen make-enved-widget
	else
		gen eff_dialog@ activate-dialog
	then
;
set-current

: make-place-sound-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 0 eff_m_snd!
	gen 1 eff_s_snd!
	gen 45 eff_pan_pos!
	gen post-place-sound-dialog ( prc1 )
	1 proc-create gen ,         ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%d %d %d)"
	    #( gen eff_label@ gen eff_m_snd@ gen eff_s_snd@ gen eff_pan_pos@ )
	    string-format change-label
;
previous

\ === Insert silence (at cursor, silence-amount in secs) ===

hide
: silence-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	#f #f #f cursor #f srate gen eff_amnt@ f* f>s #f #f insert-silence drop
;

: silence-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init }
	gen init eff_amnt!
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: silence-amount-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-silence-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen silence-ok-cb gen eff_label@ "\
Move the slider to change the number of seconds \
of silence added at the cursor position." help-cb gen silence-reset-cb
		    #f make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "silence" 0.0 gen eff_amnt@ 5.0
			gen silence-amount-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-silence-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_amnt!
	gen post-silence-dialog ( prc1 )
	1 proc-create gen ,     ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === Contrast (brightness control) ===

hide
: contrast-ok-cb ( gen -- prc; w c i self -- x )
	3 proc-create swap , ( prc )
  does> { w c info self -- x }
	self @ { gen }
	#f #f #f maxamp { peak }
	selected-sound { snd }
	snd save-controls drop
	snd reset-controls drop
	#t          snd set-contrast-control? drop
	gen eff_amnt@ snd set-contrast-control drop
	peak 1/f    snd set-contrast-control-amp drop
	peak snd #f set-amp-control drop
	gen eff_target@ 'marks = if
		plausible-mark-samples { pts }
		pts if
			snd 0
			    pts 0 array-ref
			    pts 1 array-ref
			    pts 0 array-ref - 1+ apply-controls drop
		else
			"no marks" undef status-report drop
		then
	else
		snd gen eff_target@ 'sound = if
			0
		else
			2
		then 0 undef apply-controls drop
	then
	snd restore-controls
;

: contrast-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_amnt@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init }
	gen init eff_amnt!
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: contrast-amount-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: post-contrast-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen contrast-ok-cb gen eff_label@ "\
Move the slider to change the contrast intensity." help-cb
		    gen contrast-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "contrast enhancement" 0.0 gen eff_amnt@ 10.0
			gen contrast-amount-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-contrast-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_amnt!
	gen post-contrast-dialog ( prc1 )
	1 proc-create gen ,      ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_amnt@ ) string-format change-label
;
previous

\ === Cross synthesis ===

hide
: cs-func-cb ( gen -- prc; samps self -- prc )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	gen eff_cs_snd@ gen eff_amp@ gen eff_size@ gen eff_cs_radius@
	    effects-cross-synthesis
;

: cs-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-cross-synthesis-1"
	"%s %s %s %s"
	    #( gen eff_cs_snd@ gen eff_amp@ gen eff_size@ gen eff_cs_radius@ )
	    string-format
;

: cs-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen cs-func-cb gen eff_target@ gen cs-origin-cb #f
	    map-chan-over-target-with-sync
;

'snd-motif provided? [if]
	: cs-set-state ( wid -- )
		use-combo-box-for-fft-size if
			#( FXmNselectedPosition 1 ) FXtVaSetValues
		else
			#t #t FXmToggleButtonSetState
		then drop
	;
[else]
	: cs-set-state ( wid -- ) drop ;
[then]

: cs-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_cs_snd@ , gen eff_amp@ ,
	gen eff_size@ , gen eff_cs_radius@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-snd }
	self 2 cells + @ { init-amp }
	self 3 cells + @ { init-size }
	self 4 cells + @ { init-rad }
	gen init-snd eff_cs_snd!
	gen init-amp eff_amp!
	gen init-size eff_size!
	gen init-rad eff_cs_radius!
	gen eff_sliders@ 0 array-ref init-snd   1.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-amp 100.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-rad 100.0 set-slider-value
	gen eff_cs_wid@ cs-set-state
;

: cs-snd-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 1.0 get-slider-value { val }
	self @ ( gen ) val eff_cs_snd!
;

: cs-rad-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_cs_radius!
;

'snd-motif provided? [if]
	: cs-sel-cb ( gen -- prc; w c i self -- )
		3 proc-create swap , ( prc )
	  does> { w c info self -- }
		info Fitem_or_text ( selected ) #f FXmCHARSET_TEXT
		    FXmCHARSET_TEXT #f 0 FXmOUTPUT_ALL
		    FXmStringUnparse ( size-as-str ) string->number { val }
		self @ ( gen ) val eff_size!
	;

	: cs-sel-changed-cb ( gen -- prc; w c i self -- )
		3 proc-create swap , ( prc )
  	  does> { w size info self -- }
		info Fset if
			self @ ( gen ) size eff_size!
		then
	;

	: cs-sel-create-sel { gen -- }
		#( 64 128 256 512 1024 4096 ) { sizes }
		"FFT size" FXmStringCreateLocalized { s1 }
		"frame" FxmFrameWidgetClass gen eff_sliders@ 0 array-ref
		    FXtParent
		    #( FXmNborderWidth   1 FXmNshadowType
		       FXmSHADOW_ETCHED_IN FXmNpositionIndex 2 )
		    undef FXtCreateManagedWidget { frame }
		"frm" FxmFormWidgetClass frame
		    #( FXmNleftAttachment   FXmATTACH_FORM
		       FXmNrightAttachment  FXmATTACH_FORM
		       FXmNtopAttachment    FXmATTACH_FORM
		       FXmNbottomAttachment FXmATTACH_FORM
		       FXmNbackground       basic-color )
		    undef FXtCreateManagedWidget { frm }
		use-combo-box-for-fft-size if
			"FFT size" FxmLabelWidgetClass frm
			    #( FXmNleftAttachment   FXmATTACH_FORM
			       FXmNrightAttachment  FXmATTACH_NONE
			       FXmNtopAttachment    FXmATTACH_FORM
			       FXmNbottomAttachment FXmATTACH_FORM
			       FXmNlabelString      s1
			       FXmNbackground       basic-color )
			    undef FXtCreateManagedWidget { lab }
			sizes map!
				*key* number->string FXmStringCreateLocalized
			end-map { fft-labels }
			"fftsize" FxmComboBoxWidgetClass frm
			    #( FXmNleftAttachment   FXmATTACH_WIDGET
			       FXmNleftWidget       lab
			       FXmNrightAttachment  FXmATTACH_FORM
			       FXmNtopAttachment    FXmATTACH_FORM
			       FXmNbottomAttachment FXmATTACH_FORM
			       FXmNitems            fft-labels
			       FXmNitemCount        fft-labels length
			       FXmNcomboBoxType     FXmDROP_DOWN_COMBO_BOX
			       FXmNbackground       basic-color )
			    undef FXtCreateManagedWidget { combo }
			gen combo eff_cs_wid!
			fft-labels each ( s )
				FXmStringFree drop
			end-each
			combo #( FXmNselectedPosition 1 ) FXtVaSetValues drop
			combo FXmNselectionCallback gen cs-sel-cb undef
			    FXtAddCallback drop
		else
			"rc" FxmRowColumnWidgetClass frm
			    #( FXmNorientation      FXmHORIZONTAL
			       FXmNradioBehavior    #t
			       FXmNradioAlwaysOne   #t
			       FXmNentryClass       FxmToggleButtonWidgetClass
			       FXmNisHomogeneous    #t
			       FXmNleftAttachment   FXmATTACH_FORM
			       FXmNrightAttachment  FXmATTACH_FORM
			       FXmNtopAttachment    FXmATTACH_FORM
			       FXmNbottomAttachment FXmATTACH_NONE
			       FXmNbackground       basic-color )
			    undef FXtCreateManagedWidget { rc }
			"FFT size" FxmLabelWidgetClass frm
			    #( FXmNleftAttachment   FXmATTACH_FORM
			       FXmNrightAttachment  FXmATTACH_FORM
			       FXmNtopAttachment    FXmATTACH_WIDGET
			       FXmNtopWidget        rc
			       FXmNbottomAttachment FXmATTACH_FORM
			       FXmNlabelString      s1
			       FXmNalignment        FXmALIGNMENT_BEGINNING
			       FXmNbackground       basic-color )
			    undef FXtCreateManagedWidget { lab }
			sizes each { size }
				size number->string
				    FxmToggleButtonWidgetClass rc
				    #( FXmNbackground basic-color
				       FXmNvalueChangedCallback
				       #( gen cs-sel-changed-cb size )
				       FXmNset        size gen eff_size@ = )
				    undef FXtCreateManagedWidget { button }
				size gen eff_size@ = if
					gen button eff_cs_wid!
				then
			end-each
		then
		s1 FXmStringFree drop
	;
[else]
	: cs-sel-create-sel ( gen -- ) drop ;
[then]

: post-cross-synth-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen cs-ok-cb gen eff_label@ "\
The sliders set the number of the soundfile to be cross-synthesized, \
the synthesis amplitude, the FFT size, and the radius value." help-cb
		    gen cs-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "input sound" 0 gen eff_cs_snd@ 20
			gen cs-snd-cb 1 )
		     #( "amplitude" 0.0 gen eff_amp@ 1.0
			gen amplitude-slider-cb 100 )
		     #( "radius" 0.0 gen eff_cs_radius@ 360.0
			gen cs-rad-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen cs-sel-create-sel
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-cross-synth-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1 eff_cs_snd!
	gen 0.5 eff_amp!
	gen 128 eff_size!
	gen 6.0 eff_cs_radius!
	gen #f eff_cs_wid!
	gen post-cross-synth-dialog ( prc1 )
	1 proc-create gen ,         ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%d %.2f %d %.2f)"
	    #( gen eff_label@
	       gen eff_cs_snd@
	       gen eff_amp@
	       gen eff_size@
	       gen eff_cs_radius@ ) string-format change-label
;
previous

\ === Flange and phasing ===

hide
: flange-func-cb ( gen -- prc; samps self -- prc; self -- )
	1 proc-create swap , ( prc )
  does> { samps self -- prc }
	self @ { gen }
	:frequency gen eff_fl_speed@
	    :amplitude gen eff_amnt@ make-rand-interp { ri }
	gen eff_fl_time@ #f srate f* fround->s { len }
	:size len :max-size gen eff_amnt@ 1.0 len f+ f+ f>s make-delay { del }
	1 proc-create del , ri , ( prc )
  does> { inval self -- res }
	self @ ( del ) inval self cell+ @ ( ri ) 0.0 rand-interp
	    delay inval f+ 0.75 f*
;

: flange-origin-cb ( gen -- prc; target samps self -- name origin )
	2 proc-create swap , ( prc )
  does> { target samps self -- name origin }
	self @ { gen }
	"effects-flange"
	"%s %s %s"
	    #( gen eff_amnt@ gen eff_fl_speed@ gen eff_fl_time@ ) string-format
;

: flange-ok-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen flange-func-cb gen eff_target@ gen flange-origin-cb #f
	    map-chan-over-target-with-sync
;

: flange-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_fl_speed@ , gen eff_amnt@ , gen eff_fl_time@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-speed }
	self 2 cells + @ { init-amount }
	self 3 cells + @ { init-time }
	gen init-speed eff_fl_speed!
	gen init-amount eff_amnt!
	gen init-time eff_fl_time!
	gen eff_sliders@ 0 array-ref init-speed  10.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-amount 10.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-time  100.0 set-slider-value
;

: flange-speed-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 10.0 get-slider-value { val }
	self @ ( gen ) val eff_fl_speed!
;

: flange-amount-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 10.0 get-slider-value { val }
	self @ ( gen ) val eff_amnt!
;

: flange-time-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_fl_time!
;

: post-flange-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen flange-ok-cb gen eff_label@ "\
Move the slider to change the flange speed, amount, and time." help-cb
		    gen flange-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "flange speed" 0.0 gen eff_fl_speed@ 100.0
			gen flange-speed-cb 10 )
		     #( "flange amount" 0.0 gen eff_amnt@ 100.0
			gen flange-amount-cb 10 )
		     #( "flange time" 0.0 gen eff_fl_time@ 1.0
			gen flange-time-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-flange-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 2.000 eff_fl_speed!
	gen 5.000 eff_amnt!
	gen 0.001 eff_fl_time!
	gen post-flange-dialog ( prc1 )
	1 proc-create gen ,    ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f %.2f)"
	    #( gen eff_label@ gen eff_fl_speed@ gen eff_amnt@ gen eff_fl_time@ )
	    string-format change-label
;
previous

\ === Randomize phase ===

hide
: random-phase-cb ( scl -- prc; x self -- res )
	1 proc-create swap , ( prc )
  does> { x self -- res }
	self @ ( scl ) random
;

: rp-ok-cb ( gen -- prc; w c i self -- res )
	3 proc-create swap , ( prc )
  does> { w c info self -- res }
	self @ { gen }
	gen eff_scl@ random-phase-cb { prc }
	\ edit-list->function needs a usable proc-source-string
	prc "%s random-phase-cb" gen eff_scl@ string-format proc-source-set!
	prc #f #f rotate-phase
;

: rp-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_scl@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init }
	gen init eff_scl!
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: post-random-phase-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen rp-ok-cb gen eff_label@ "\
Move the slider to change the randomization amplitude scaler." help-cb
		    gen rp-reset-cb #f make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "amplitude scaler" 0.0 gen eff_scl@ 100.0
			gen scaler-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-random-phase-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 3.14 eff_scl!
	gen post-random-phase-dialog ( prc1 )
	1 proc-create gen ,          ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_scl@ ) string-format change-label
;
previous

\ === Robotize ===

hide
: robotize-ok-cb ( gen -- prc; w c i self -- res )
	3 proc-create swap , ( prc )
  does> { w c info self -- res }
	self @ { gen }
	gen eff_sr@ gen eff_amp@ gen eff_freq@ \ beg dur follows
	gen eff_target@ 'sound = if
		0 #f #f #f framples
	else
		gen eff_target@ 'selection = if
			#f #f selection-position  #f #f selection-framples
		else
			plausible-mark-samples { pts }
			pts if
				pts 0 array-ref
				pts 1 array-ref
				pts 0 array-ref -
			else
				'no-such-mark
				    #( "%s: %s" get-func-name pts ) fth-throw
			then
		then
	then #f #f effects-fp
;

: robotize-reset-cb { gen -- prc; w c i self -- }
	3 proc-create ( prc )
	gen , gen eff_sr@ , gen eff_amp@ , gen eff_freq@ ,
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-sr }
	self 2 cells + @ { init-amp }
	self 3 cells + @ { init-frq }
	gen init-sr eff_sr!
	gen init-amp eff_amp!
	gen init-frq eff_freq!
	gen eff_sliders@ 0 array-ref init-sr  100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-amp 100.0 set-slider-value
	gen eff_sliders@ 2 array-ref init-frq 100.0 set-slider-value
;

: robotize-sam-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_sr!
;

: post-robotize-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen robotize-ok-cb gen eff_label@ "\
Move the sliders to set the sample rate, \
oscillator amplitude, and oscillator frequency." help-cb gen robotize-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "sample rate" 0.0 gen eff_sr@ 2.0
			gen robotize-sam-cb 100 )
		     #( "oscillator amplitude" 0.0 gen eff_amp@ 1.0
			gen amplitude-slider-cb 100 )
		     #( "oscillator frequency" 0.0 gen eff_freq@ 60.0
			gen frequency-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-robotize-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_sr!
	gen 0.3 eff_amp!
	gen 20.0 eff_freq!
	gen post-robotize-dialog ( prc1 )
	1 proc-create gen ,      ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f %.2f)"
	    #( gen eff_label@ gen eff_sr@ gen eff_amp@ gen eff_freq@ )
	    string-format change-label
;
previous

\ === Rubber sound ===

hide
: rubber-ok-cb ( gen -- prc; w c i self -- res )
	3 proc-create swap , ( prc )
  does> { w c info self -- res }
	self @ ( gen ) eff_factor@ #f #f rubber-sound
;

: rubber-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_factor@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init }
	gen init eff_factor!
	gen eff_sliders@ 0 array-ref init 100.0 set-slider-value
;

: rubber-factor-cb ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	w info 100.0 get-slider-value { val }
	self @ ( gen ) val eff_factor!
;

: post-rubber-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen rubber-ok-cb gen eff_label@ "\
Stretches or contracts the time of a sound.  \
Move the slider to change the stretch factor." help-cb gen rubber-reset-cb
		    gen general-target-cb make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "stretch factor" 0.0 gen eff_factor@ 5.0
			gen rubber-factor-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-rubber-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 1.0 eff_factor!
	gen post-rubber-dialog ( prc1 )
	1 proc-create gen ,    ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f)"
	    #( gen eff_label@ gen eff_factor@ ) string-format change-label
;
previous

\ === Wobble ===

hide
: wobble-ok-cb ( gen -- prc; w c i self -- res )
	3 proc-create swap , ( prc )
  does> { w c info self -- res }
	self @ { gen }
	gen eff_freq@ gen eff_amp@		\ beg dur follows
	gen eff_target@ 'sound = if
		0  #f #f #f framples
	else
		gen eff_target@ 'selection = if
			#f #f selection-position  #f #f selection-framples
		else
			plausible-mark-samples { pts }
			pts if
				pts 0 array-ref
				pts 1 array-ref
				pts 0 array-ref -
			else
				'no-such-mark
				    #( "%s: %s" get-func-name pts ) fth-throw
			then
		then
	then #f #f effects-hello-dentist
;

: wobble-reset-cb { gen -- prc; w c i self -- }
	3 proc-create gen , gen eff_freq@ , gen eff_amp@ , ( prc )
  does> { w c info self -- }
	self @ { gen }
	self 1 cells + @ { init-frq }
	self 2 cells + @ { init-amp }
	gen init-frq eff_freq!
	gen init-amp eff_amp!
	gen eff_sliders@ 0 array-ref init-frq 100.0 set-slider-value
	gen eff_sliders@ 1 array-ref init-amp 100.0 set-slider-value
;

: post-wobble-dialog ( gen -- prc; w c i self -- )
	3 proc-create swap , ( prc )
  does> { w c info self -- }
	self @ { gen }
	gen eff_dialog@ widget? unless
		gen eff_label@ gen wobble-ok-cb gen eff_label@ "\
Move the sliders to set the wobble frequency and amplitude." help-cb
		    gen wobble-reset-cb gen general-target-cb
		    make-effect-dialog { d }
		gen d eff_dialog!
		d #( #( "wobble frequency" 0.0 gen eff_freq@ 100.0
			gen frequency-slider-cb 100 )
		     #( "wobble amplitude" 0.0 gen eff_amp@ 1.0
			gen amplitude-slider-cb 100 ) ) add-sliders ( sl )
		gen swap eff_sliders!
		gen #f add-target
	then
	gen eff_dialog@ activate-dialog
;
set-current

: make-wobble-dialog ( name -- prc1 prc2; child self -- prc; self -- )
	( name ) make-base-effects { gen }
	gen 50.0 eff_freq!
	gen 0.5 eff_amp!
	gen post-wobble-dialog ( prc1 )
	1 proc-create gen ,    ( prc2 )
  does> { child self -- prc; self -- }
	0 proc-create self @ ( gen ) , child , ( prc )
  does> { self -- }
	self @ { gen }
	self cell+ @ ( child ) "%s (%.2f %.2f)"
	    #( gen eff_label@ gen eff_freq@ gen eff_amp@ )
	    string-format change-label
;
previous

: init-effects-menu ( name -- )
	make-main-menu { main }
	"Amplitude Effects"           main make-menu { menu }
	menu "Gain"                   make-gain-dialog         menu-entry
	menu "Normalize"              make-normalize-dialog    menu-entry
	menu "Gate"                   make-gate-dialog         menu-entry
	"Delay Effects"               main make-menu to menu
	menu "Echo"                   make-echo-dialog         menu-entry
	menu "Filtered echo"          make-flecho-dialog       menu-entry
	menu "Modulated echo"         make-zecho-dialog        menu-entry
	"Filter Effects"              main make-menu to menu
	menu "Band-pass filter"       make-band-pass-dialog    menu-entry
	menu "Band-reject filter"     make-notch-dialog        menu-entry
	menu "High-pass filter"       make-high-pass-dialog    menu-entry
	menu "Low-pass filter"        make-low-pass-dialog     menu-entry
	menu "Comb filter"            make-comb-dialog         menu-entry
	menu "Comb chord filter"      make-comb-chord-dialog   menu-entry
	menu "Moog filter"            make-moog-dialog         menu-entry
	"Frequency Effects"           main make-menu to menu
	menu "Adaptive saturation"    make-adsat-dialog        menu-entry
	menu "Sample rate conversion" make-src-dialog          menu-entry
	menu "Time/pitch scaling"     make-expsrc-dialog       menu-entry
	menu "Src-Timevar"            make-src-timevar-dialog  menu-entry
	"Modulation Effects"          main make-menu to menu
	menu "Amplitude modulation"   make-am-effect-dialog    menu-entry
	menu "Ring modulation"        make-rm-effect-dialog    menu-entry
	"Reverbs"                     main make-menu to menu
	menu "McNabb reverb"          make-reverb-dialog       menu-entry
	menu "Chowning reverb"        make-jc-reverb-dialog    menu-entry
	menu "Convolution"            make-convolve-dialog     menu-entry
	"Various"                     main make-menu to menu
	menu "Place sound"            make-place-sound-dialog  menu-entry
	menu "Add silence"            make-silence-dialog      menu-entry
	menu "Contrast enhancement"   make-contrast-dialog     menu-entry
	menu "Cross synthesis"        make-cross-synth-dialog  menu-entry
	menu "Flange"                 make-flange-dialog       menu-entry
	menu "Randomize phase"        make-random-phase-dialog menu-entry
	menu "Robotize"               make-robotize-dialog     menu-entry
	menu "Rubber sound"           make-rubber-dialog       menu-entry
	menu "Wobble"                 make-wobble-dialog       menu-entry
;

\ === Effects Menu ===
"Effects" value effects-menu-label

[undefined] effects-menu-exists? [if]
	#t value effects-menu-exists?
	effects-menu-label init-effects-menu
	#f effects-noop  add-to-effects-menu	\ separator

	"Octave-down" lambda: <{ -- }>
		2 #f #f down-oct
	; add-to-effects-menu

	"Remove clicks" lambda: <{ -- }>
		#f #f effects-remove-clicks
	; add-to-effects-menu

	"Remove DC" lambda: <{ -- }>
		#f #f effects-remove-dc
	; add-to-effects-menu

	"Spiker" lambda: <{ -- }>
		#f #f spike
	; add-to-effects-menu

	"Compand" lambda: <{ -- }>
		#f #f effects-compand
	; add-to-effects-menu

	"Invert" lambda: <{ -- }>
		-1 #f #f scale-by
	; add-to-effects-menu

	"Reverse" lambda: <{ -- }>
		#f #f #f reverse-sound
	; add-to-effects-menu

	"Null phase" lambda: <{ -- }>
		#f #f zero-phase
	; add-to-effects-menu
[then]

\ effects.fs ends here
