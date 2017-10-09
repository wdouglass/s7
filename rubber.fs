\ rubber.fs -- rubber.scm --> rubber.fs

\ Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 06/01/06 05:32:57
\ Changed: 14/04/28 03:52:17
\
\ @(#)rubber.fs	1.15 4/28/14

\ original comments are taken from rubber.scm [ms]
\
\ ;;; rubber.scm: rubber-sound stretches or contracts a sound (in time)
\ ;;;   (rubber-sound 1.5) makes it 50% longer
\ ;;;   rubber-sound looks for stable portions and either inserts or deletes
\ ;;;     periods period length is determined via autocorrelation

require clm
require marks

8             value zeros-checked
10.0          value extension
*fth-verbose* value show-details

hide
\ ;;; remove anything below 16Hz
\ ;;; extend (src by 1/extension)
\ ;;; collect upward zero-crossings
\ ;;;   collect weights for each across next zeros-checked crossings
\ ;;;   sort by least weight
\ ;;;   ramp (out or in) and check if done

: derumble-sound { snd chn -- }
	\ ;; remove rumbles and DC etc (since we're using zero crossings
	\ ;;   to find period starts)
	snd chn #f framples { old-len }
	snd srate { sr }
	#( 0.0 0.0
	   16.0 f2* sr f/ 0.0
	   20.0 f2* sr f/ 1.0
	   1.0 1.0 ) ( flt-lst )
	    2.0 old-len sr min flog 2.0 flog f/ fceil ( pow2 )
	    f** f>s ( fftlen ) snd chn #f undef filter-sound drop
	old-len snd chn set-framples drop
;

: sample-sound { snd chn -- }
	\ ;; prepare sound for analysis by interpolating samples
	extension 1.0 f<> if
		extension 1/f 1.0 snd chn #f src-sound drop
	then
;

: unsample-sound { snd chn -- }
	\ ;; undo earlier interpolation
	extension 1.0 f<> if
		extension 1.0 snd chn #f src-sound drop
	then
;

: crossings { snd chn -- n }
	\ ;; return number of upward zero crossings that don't look like silence
	0 0 { crosses last-cross }
	0 snd chn 1 #f make-sampler { sr0 }
	sr0 next-sample { samp0 }
	0.0 { sum }
	extension 0.001 f* { silence }
	snd chn #f framples 0 ?do
		sr0 next-sample { samp1 }
		samp0 f0<=
		samp1 f0>          &&
		i last-cross - 4 > &&
		sum silence f>     && if
			crosses 1+ to crosses
			i to last-cross
			0.0 to sum
		then
		samp0 fabs sum f+ to sum
		samp1 to samp0
	loop
	crosses
;

: env-add { s0 s1 samps snd chn -- vct }
	1.0 { x }
	samps 1/f { xinc }
	s0 snd chn 1 #f make-sampler { sr0 }
	s1 snd chn 1 #f make-sampler { sr1 }
	samps 0.0 make-vct map!
		sr0 next-sample x f*
		    sr1 next-sample  1.0 x f-  f*  f+ ( val )
		x xinc f+ to x
		( val )
	end-map ( data )
;

: rubber-cb { stretch snd chn -- prc; self -- }
	0 proc-create ( prc ) chn , snd , stretch ,
 does> { self -- }
	self @ { chn }
	self cell+ @ { snd }
	self 2 cells + @ { stretch }
	\ ;; prepare sound (get rid of low freqs, resample)
	snd chn derumble-sound
	snd chn sample-sound
	snd chn crossings { crosses }
	crosses :initial-element 0 make-array { cross-samples }
	crosses :initial-element 0 make-array { cross-weights }
	crosses :initial-element 0 make-array { cross-marks }
	crosses :initial-element 0 make-array { cross-periods }
	\ ;; get cross points (sample numbers)
	0 snd chn 1 #f make-sampler { sr0 }
	sr0 next-sample { samp0 }
	0.0 { sum }
	0 { last-cross }
	extension 0.001 f* { silence }
	snd chn #f framples 0 ?do
		sr0 next-sample { samp1 }
		samp0 f0<=
		samp1 f0>           &&
		i last-cross - 40 > &&
		sum silence f>      && if
			i to last-cross
			0.0 to sum
			cross-samples i cycle-set!
		then
		samp0 fabs sum f+ to sum
		samp1 to samp0
	loop
	\ ;; now run through crosses getting period match info
	crosses 1- 0 ?do
		cross-samples i array-ref { start }
		0 { autolen }
		2.0 extension snd srate 40.0 f/ f*
		flog 2.0 flog f/ fceil ( pow2 ) f** f>s { fftlen }
		fftlen 4 / { len4 }
		start snd chn 1 #f make-sampler { rd }
		fftlen 0.0 make-vct map! rd next-sample end-map { data }
		data autocorrelate drop
		len4 1 ?do
			data i    vct-ref data i 1+  vct-ref f<
			data i 1+ vct-ref data i 2 + vct-ref f> && if
				i 2* to autolen
				leave
			then
		loop
		start autolen + { next-start }
		i 1+ { min-i }
		cross-samples min-i array-ref next-start - abs { min-samps }
		crosses i zeros-checked + min i 2 + ?do
			cross-samples i array-ref next-start - abs { dist }
			dist min-samps < if
				dist to min-samps
				i to min-i
			then
		loop
		min-i { current-mark }
		0.0 { current-min }
		cross-samples current-mark array-ref { s1 }
		start snd chn 1 #f make-sampler { sr0 }
		s1 snd chn 1 #f make-sampler { sr1 }
		0.0 0.0 { ampsum diffsum }
		autolen 0 ?do
			sr0 next-sample { samp0 }
			sr1 next-sample { samp1 }
			samp0 fabs ampsum f+ to ampsum
			samp1 samp0 f- fabs diffsum f+ to diffsum
		loop
		diffsum f0= if
			0.0
		else
			diffsum ampsum f/
		then to current-min
		current-min f2/ fround->s to min-samps
		current-mark zeros-checked i + min crosses 1- min { top }
		top i 1+ ?do
			0.0 { wgt }
			cross-samples i array-ref { s1 }
			start snd chn 1 #f make-sampler { sr0 }
			s1 snd chn 1 #f make-sampler { sr1 }
			0.0 0.0 { ampsum diffsum }
			autolen 0 ?do
				sr0 next-sample { samp0 }
				sr1 next-sample { samp1 }
				samp0 fabs ampsum f+ to ampsum
				samp1 samp0 f- fabs diffsum f+ to diffsum
			loop
			diffsum f0= if
				0.0
			else
				diffsum ampsum f/
			then to wgt
			wgt min-samps f< if
				wgt f>s to min-samps
				i to min-i
			then
		loop
		current-mark min-i <> if
			\ ;; these are confused, so effectively erase them
			cross-weights i 1000.0 array-set!
		else
			cross-weights i current-min array-set!
			cross-marks i current-mark array-set!
			cross-periods i cross-samples current-mark array-ref
			cross-samples i array-ref -  array-set!
		then
	loop
	\ ;; now sort weights to scatter the changes as evenly as possible
	snd chn #f framples { len }
	stretch 1.0 f> { adding }
	stretch 1.0 f- fabs len f* floor f>s { samps }
	adding if
		samps
	else
		len samps 2* min
	then { needed-samps }
	0 { handled }
	#() { edits }
	begin
		\ ;; need to find (more than) enough splice pts to delete samps
		-1      { best-mark }
		handled { old-handled }
		0       { cur }
		cross-weights 0 array-ref { curmin }
		cross-weights each { val }
			val curmin f< if
				i to cur
				val to curmin
			then
		end-each
		cur to best-mark
		cross-periods best-mark array-ref handled + to handled
		handled needed-samps <
		handled needed-samps - needed-samps old-handled - < || if
			edits best-mark array-push drop
		then
		cross-weights best-mark 1000.0 array-set!
		edits length crosses =
		handled needed-samps >= ||
	until
	edits length crosses >= if
		needed-samps handled f/ fceil f>s
	else
		1
	then { mult }
	0 { changed-len }
	edits each { best-mark }
		changed-len samps > ?leave
		cross-samples best-mark array-ref { beg }
		cross-samples 
		    cross-marks best-mark array-ref
		    array-ref { next-beg }
		cross-periods best-mark array-ref { len }
		len 0> if
			adding if
				beg next-beg len snd chn env-add { new-samps }
				show-details if
					beg "%d:%d" #( i len extension f/ f>s )
					    string-format
					    snd chn add-named-mark drop
				then
				beg len new-samps snd chn #f insert-samples drop
				mult 1 ?do
					beg len i * + len new-samps
					    snd chn #f insert-samples drop
				loop
				len mult * changed-len + to changed-len
				cross-samples each { curbeg }
					curbeg beg > if
						cross-samples i
						    curbeg len + array-set!
					then
				end-each
			else
				beg snd chn #f framples >= if
					"trouble at %d: %d of %d"
					    #( i beg snd chn #f framples )
					    clm-message
				then
				show-details if
					beg 1- "%d:%d"
					    #( i len extension f/ f>s )
					    string-format
					    snd chn add-named-mark drop
				then
				beg len snd chn #f delete-samples drop
				changed-len len + to changed-len
				beg len + { end }
				cross-samples each { curbeg }
					curbeg beg > if
						curbeg end < if
							cross-periods i 0
							    array-set!
						else
							cross-samples i
							    curbeg len -
							    array-set!
						then
					then
				end-each
			then
		then
	end-each
	show-details if
		"wanted: %d, got %d" #( samps changed-len ) clm-message
	then
	\ ;; and return to original srate
	snd chn unsample-sound
	show-details if
		snd chn 0 framples { frms0 }
		snd chn undef framples { frms }
		"%d -> %d (%d)"
		    #( frms0  frms  frms0 stretch f* floor f>s ) clm-message
	then
;
set-current

: rubber-sound { stretch snd chn -- res }
	"%s %s" #( stretch get-func-name ) string-format { origin }
	stretch snd chn rubber-cb origin as-one-edit
;
previous

\ rubber.fs ends here
