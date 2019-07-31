;=============================================================================
;		AD1m.ORC
; ADDITIVE SYNTHESIS INSTRUMENT N. 11 / COMPLETE / MONO
;=============================================================================
; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;         avoids printing small values with exponential notation

; Replaced oscili with poscil (precise oscillator), ms 8/08
; Default SR = 96000, recommended precision: 24 bits
;-----------------------------------------------------------------------------
; SIMPLIFIED PERCEPTUAL COMPENSATION: SINCE THE COMPUTATION IS FLOATING POINT
;    NO NEED TO REDUCE THE AMPLITUDES IF MAXIMUM IS > 1 AFTER THE COMPENSATION
; ALSO NO MODIFICATION OF THE GLOBAL AMPLITUDE TO A RELATIVE AMPLITUDE
;-----------------------------------------------------------------------------
;	p1	= instrument number [12]
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= frequency [Hz]
;	p6	= attack duration [sec]
;	p7	= decay duration [sec], with internal readjustment if needed
;	p8	= amplitude envelope [GEN]
;	p9	= jitter amplitude, with internal perceptual compensation [0-1]
;	p10	= tremolo amplitude [0-1]
;	p11	= tremolo frequency [Hz]
;	p12	= balance freq.jitter/vibrato [1 = jitter / 0 = vibrato]
;	p13	= vibrato frequency [Hz]
;	p14	= frequency deviation [semitones]
;	p15	= frequency envelope [GEN]
;	p16	= lower interval for portamento [semitones]
;	p17	= upper interval for portamento [semitones]
;	p18	= portamento envelope [GEN]
;	p19	= portamento duration [sec]
;	p20	= audio fun [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f2	vibrato function (sine)
;	f3	tremolo function (sine)
;_____________________________________________________________________________

; GEN functions **********************************************************
; audio, vibrato, tremolo
f1  0   65537  9  1  1  0
f2  0   65537  9  1  1  0
f3  0   65537  9  1  1  0

; interpolating sinus-based bell-like shape
f20	0	65537	19	1	0.5	-90	0.5
; ascending half sine
f21	0	65537	19	0.5	0.5	-90	0.5
; descending half sine
f22	0	65537	19	0.5	0.5	 90	0.5

; envelopes
; triangle segment
f6  0   513   7  0 256 1 256 0
; all 1
f7  0   513   7  1 512 1
f51  0   513   7  0 1 1 510 1 1 0
f52  0   513   7  0 150 1 150 1 212 0


; score ******************************************************************
; neutral note, att=dec
;  at  dur   amp fq	   att/dec/aenv	jta/tra/trf		jvfq/vfq/vdev/fqenv	plow/up/env/dur	afun
i11	0	2	-6.0 440.0	0.5 0.5	51	0.0 0.0 5.0		0.5 6.0 0.0	20		0.0 0.0 21 0.5	1
s 3
; triangle aenv, no att/dec
i11	0	2	-6.0 440.0	0.0 0.0	6	0.0 0.0 5.0		0.5 6.0 0.0	20		0.0 0.0 21 0.5	1
s 3
; triangle aenv + att/dec
i11	0	2	-6.0 440.0	1.0 1.0	6	0.0 0.0 5.0		0.5 6.0 0.0	20		0.0 0.0 21 0.5	1
s 3
; only tremolo
i11	0	2	-6.0 440.0	0.01 0.01 51	0.0 0.1 5.0	0.5 6.0 0.0	20		0.0 0.0 21 0.5	1
s 3
; only jitter (amp), appx same perceptual amount
i11	0	2	-6.0 440.0	0.01 0.01 51	0.1 0.0 5.0	0.5 6.0 0.0	20		0.0 0.0 21 0.5	1
s 3
; tremolo+jitter (amp)
i11	0	2	-6.0 440.0	0.01 0.01 51	0.1 0.1 5.0	0.5 6.0 0.0	20		0.0 0.0 21 0.5	1
s 3
; only vibrato, 1 Hz, 2+, no envelope
i11	0	2	-6.0 440.0	0.01 0.01 51	0.0 0.0 5.0	0.0 0.5 1.0	7		0.0 0.0 21 0.5	1
s 3
; only vibrato, 6 Hz, 2-, bell-shaped envelope
i11	0	2	-6.0 440.0	0.01 0.01 51	0.0 0.0 5.0	0.0 6.0 1.0	20		0.0 0.0 21 0.5	1
s 3
; only jitter (freq), 2-, bell-shaped envelope
i11	0	2	-6.0 440.0	0.01 0.01 51	0.0 0.0 5.0	1.0 6.0 1.0	20		0.0 0.0 21 0.5	1
s 3
; only portamento (from -2- to 0) with straight notes
i11	0	5	-12.0 440.0	0.01 0.01 51	0.0 0.0 5.0	0.5 6.0 0.0	20		-1.0 0.0 21 3.0	1
i11	0	3	-12.0 415.0	0.01 0.01 51	0.0 0.0 5.0	0.5 6.0 0.0	20		0.0 0.0 21 1.0	1
i11	2	3	-12.0 440.0	0.01 0.01 51	0.0 0.0 5.0	0.5 6.0 0.0	20		0.0 0.0 21 1.0	1
s 6
; only portamento (from 2+ to 0) with straight notes
i11	0	5	-12.0 440.0	0.01 0.01 51	0.0 0.0 5.0	0.5 6.0 0.0	20		1.0 0.0 21 1.0	1
i11	0	3	-12.0 493.9	0.01 0.01 51	0.0 0.0 5.0	0.5 6.0 0.0	20		0.0 0.0 21 0.0	1
i11	2	3	-12.0 440.0	0.01 0.01 51	0.0 0.0 5.0	0.5 6.0 0.0	20		0.0 0.0 21 0.0	1
s 6
; only portamento (from -4th to +5th)
i11	0	5	-8.0 440.0	0.5 0.5	51	0.0 0.0 5.0		0.5 6.0 0.0	20		-5.0 7.0 21 2.0	1
i11	3	2	-8.0 660.0	0.5 0.5	51	0.0 0.0 5.0		0.5 6.0 0.0	20		0.0 0.0 21 1.0	1
s 6
; descending GEN
i11	0	5	-8.0 440.0	0.5 0.5	51	0.0 0.0 5.0		0.5 6.0 0.0	20		-5.0 7.0 22 2.0	1
e
