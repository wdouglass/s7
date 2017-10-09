\ draw.fs -- draw.scm -> draw.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: 05/12/18 23:36:09
\ Changed: 14/04/28 03:52:17
\
\ @(#)draw.fs	1.19 4/28/14

\ make-current-window-display  ( -- )
\ close-current-window-display ( -- )

require extensions

\ --- inset overall waveform; if click, move to that location ---

#f value current-window-display-is-running \ for prefs

hide
0.20 constant inset-width
0.25 constant inset-height

: update-current-window-location <{ snd -- #f }>
	current-window-display-is-running if
		snd channels 0 ?do
			'inset-envelope snd i channel-property { vals }
			\ set edit-position to impossible value
			vals array? if
				vals 'edit-position -2 array-assoc-set! to vals
			then
		loop
	then
	#f
;

: display-current-window-location <{ snd chn -- }>
	current-window-display-is-running
	snd chn time-graph? && if
		snd chn undef axis-info { axinf }
		axinf 12 array-ref { grf-width }
		inset-width grf-width f* fround->s { width }
		grf-width width - { x-offset }
		axinf 11 array-ref axinf 13 array-ref - { grf-height }
		inset-height grf-height f* fround->s { height }
		axinf 13 array-ref 10 - { chan-offset }
		chan-offset height 2/ + { y-offset }
		snd channel-style channels-separate = if
			chn
		else
			0
		then { grf-chn }
		axinf 19 array-ref { new-peaks }
		snd chn #f framples { frms }
		#f { data0 }
		#f { data1 }
		width  10 >
		height 10 > &&
		frms   0>   &&
		chn 0= snd channel-style channels-superimposed <> || && if
			x-offset chan-offset height + width 2
			snd grf-chn undef #f fill-rectangle drop
			x-offset chan-offset 2 height
			snd grf-chn undef #f fill-rectangle drop
			snd chn right-sample frms f/ width f* fround->s { rx }
			snd chn left-sample  frms f/ width f* fround->s { lx }
			x-offset lx + chan-offset rx lx - 1 max height
			snd grf-chn selection-context #f fill-rectangle drop
			'inset-envelope snd chn channel-property { old-env }
			old-env array? if
				new-peaks not
				old-env 'width array-assoc-ref width = &&
				old-env 'height array-assoc-ref height = &&
				old-env 'y-offset array-assoc-ref y-offset = &&
				old-env 'edit-position array-assoc-ref
				    snd chn edit-position = && if
					old-env 'data0 array-assoc-ref to data0
					old-env 'data1 array-assoc-ref to data1
					#t
				else
					#f
				then
			else
				#f
			then unless	\ else (old-env == #f)
				snd chn current-edit-position 0
				    frms make-graph-data { data }
				\ data may be a vct or a list of two vcts
				data vct? if
					data vct-peak
				else
					data 0 array-ref vct-peak
					data 1 array-ref vct-peak fmax
				then { data-max }
				data-max f0> if
					height data-max f2* f/
				else
					0.0
				then { data-scaler }
				width 2* { new-len }
				data vct? if
					data
				else
					data 0 array-ref
				then length { data-len }
				data-len width f/ fround->s { step }
				data-len width > if
					new-len make-array to data0
					data array? if
						new-len make-array to data1
					then
					0 { idxi }
					0 { idxj }
					data-max fnegate { max-y }
					data-max { min-y }
					0 { stepper }
					begin
						idxi data-len <
						idxj new-len < &&
					while
						data1 if
							max-y data 1 array-ref
							    idxi vct-ref fmax
							    to max-y
							min-y data 0 array-ref
							    idxi vct-ref fmin
							    to min-y
						else
							max-y data idxi vct-ref
							    fmax to max-y
						then
						stepper 1+ to stepper
						stepper step >= if
							data0 idxj x-offset
							    array-set!
							data0 idxj 1+ y-offset
							    max-y data-scaler
							    f* f- fround->s
							    array-set!
							data-max fnegate
							    to max-y
							data1 if
								data1 idxj
								    x-offset
								    array-set!
								data1 idxj 1+
								    y-offset
								    min-y
								    data-scaler
								    f* f-
								    fround->s
								    array-set!
								data-max
								    to min-y
							then
							x-offset 1+ to x-offset
							stepper step -
							    to stepper
							idxj 2 + to idxj
						then
						idxi 1+ to idxi
					repeat
					begin idxj new-len < while
						data0 idxj
						    data0 idxj 2 - array-ref
						    array-set!
						data0 idxj 1+
						    data0 idxj 1 - array-ref
						    array-set!
						data1 if
							data1 idxj 
							    data1 idxj 2 -
							    array-ref
							    array-set!
							data1 idxj 1+
							    data1 idxj 1 -
							    array-ref
							    array-set!
						then
						idxj 2 + to idxj
					repeat
				else
					width data-len f/ fround->s { xstep }
					data-len 2* make-array to data0
					data array? if
						new-len 2* make-array to data1
					then
					0 { idxj }
					x-offset { xj }
					data-len 0 ?do
						data0 idxj xj array-set!
						data1 if
							data0 idxj 1+
							    y-offset
							    data 1 array-ref
							    i vct-ref
							    data-scaler
							    f* f- fround->s
							    array-set!
							data1 idxj
							    xj array-set!
							data1 idxj 1+
							    y-offset
							    data 0 array-ref
							    i vct-ref
							    data-scaler
							    f* f- fround->s
							    array-set!
						else
							data0 idxj 1+
							    y-offset
							    data i vct-ref
							    data-scaler f* f-
							    fround->s array-set!
						then
						idxj 2 + to idxj
						xj xstep + to xj
					loop
				then
				#() 'width width array-assoc-set!
				( vals ) 'height height array-assoc-set!
				( vals ) 'edit-position
				    snd chn edit-position array-assoc-set!
				( vals ) 'data0 data0 array-assoc-set!
				( vals ) 'data1 data1 array-assoc-set!
				( vals ) 'y-offset
				    y-offset array-assoc-set! { vals }
				'inset-envelope vals
				    snd chn set-channel-property drop
			then
			data1 length 2 mod if
				data1 array-pop drop
			then
			data0 snd grf-chn time-graph draw-lines drop
			data1 if
				data1 snd grf-chn time-graph draw-lines drop
			then
		then
	then
;

: click-current-window-location <{ snd chn button state x y axis -- f }>
	current-window-display-is-running
	axis time-graph = && if
		snd chn undef axis-info            { axinf }
		axinf 12 array-ref                 { grf-width }
		inset-width grf-width f* fround->s { width }
		grf-width width -                  { x-offset }
		axinf 11 array-ref axinf 13 array-ref
		    - inset-height f* fround->s    { height }
		axinf 13 array-ref 10 -            { chan-offset }
		width         0>
		x x-offset    >= &&
		x grf-width   <= &&
		y chan-offset >= &&
		y chan-offset height + <= && if
			snd chn #f framples x x-offset f- width f/ f*
			    fround->s { samp }
			snd chn left-sample { ls }
			snd chn right-sample { rs }
			samp snd chn #f set-cursor drop
			samp ls < samp rs > || if
				samp ls rs - 2/ - 0 max
				snd chn #f framples 1- min
				snd chn set-right-sample drop
			then
			snd chn update-time-graph drop
			#t
		else
			#f
		then
	else
		#f
	then
;

: undo-cb { snd chn -- proc; self -- }
	0 proc-create snd , chn ,
 does> { self -- }
	'inset-envelope #f self @ ( snd ) self cell+ @ ( chn )
	    set-channel-property drop
;

: install-current-window-location <{ snd -- }>
	snd channels 0 ?do
		'inset-envelope snd i
		    set-channel-property-save-state-ignore drop
		snd i undo-hook snd i undo-cb add-hook!
	loop
;
set-current

: make-current-window-display ( -- )
	doc" Display in upper right corner the overall current sound \
and where the current window fits in it."
	current-window-display-is-running unless
		#t to current-window-display-is-running
		after-open-hook  <'> install-current-window-location add-hook!
		after-graph-hook <'> display-current-window-location add-hook!
		mouse-click-hook <'> click-current-window-location   add-hook!
		update-hook      <'> update-current-window-location  add-hook!
	then
;

: close-current-window-display ( -- )
	current-window-display-is-running if
		#f to current-window-display-is-running
		after-open-hook <'> install-current-window-location
		    remove-hook! drop
		after-graph-hook <'> display-current-window-location
		    remove-hook! drop
		mouse-click-hook <'> click-current-window-location
		    remove-hook! drop
		update-hook <'> update-current-window-location
		    remove-hook! drop
		sounds each { snd }
			snd channels 0 ?do
				snd i undo-hook <'> undo-cb remove-hook! drop
			loop
		end-each
	then
;

previous

\ draw.fs ends here
