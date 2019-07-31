;=============================================================================
;		AD1t.ORC
; ADDITIVE SYNTHESIS INSTRUMENT N. 11 / COMPLETE / TRIAPHONIC (no ch. 1)
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
; p1	= instrument number [12]
; p2	= action time [sec]
; p3	= duration [sec]
; p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
; p5	= frequency [Hz]
; p6	= attack duration [sec]
; p7	= decay duration [sec], with internal readjustment if needed
; p8	= amplitude envelope [GEN]
; p9	= jitter amplitude, with internal perceptual compensation [0-1]
; p10	= tremolo amplitude [0-1]
; p11	= tremolo frequency [Hz]
; p12	= balance freq.jitter/vibrato [1 = jitter / 0 = vibrato]
; p13	= vibrato frequency [Hz]
; p14	= frequency deviation [semitones]
; p15	= frequency envelope [GEN]
; p16	= lower interval for portamento [semitones]
; p17	= upper interval for portamento [semitones]
; p18	= portamento envelope [GEN]
; p19	= portamento duration [sec]
; p20	= min azimuth [degrees]
; p21	= max azimuth [degrees]
; p22	= azimuth envelope [GEN]
; p23	= duration of azimuth envelope [sec]
; p24	= distance [0-1, 0=all LP active, 1=normal pan]
; p25	= audio fun [GEN]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f1	audio wave (sine)
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
; ascending
f8  0   513   7  0 512 1
; descending
f9  0   513   7  1 512 0
f51  0   513   7  0 1 1 510 1 1 0
f52  0   513   7  0 150 1 150 1 212 0


; score ******************************************************************
;  at  dur   amp fq	   att/dec/aenv	jta/tra/trf	jvfq/vfq/vdev/ev plow/up/ev/dur	azmn/mx/ev/dr/dst afil
i11	0	1	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 0 8 0 1	1
s 2
i11	0	1	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	120 120 8 0 1	1
s 2
i11	0	1	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	240 240 8 0 1	1
s 2
i11	0	1	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	360 360 8 0 1	1
s 2
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 120 8 0 1	1
s 3
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 120 8 0 0.5	1
s 3
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 120 8 0 0	1
s 3
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 240 8 0 1	1
s 3
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 360 8 0 1	1
s 3
; backwards (descending segment)
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 360 9 0 1	1
s 3
; backwards (descending segment, circle)
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 360 9 0.5 1	1
s 3
; from the middle to 0 backwards
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 120 9 0 1	1
s 3
; from the middle to 0 backwards (circle) CLICKS?
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	0 240 9 1.0 1	1
s 3
; from the middle to 0 forwards
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	120 360 8 0 1	1
s 3
; from the middle to 0 forwards (circle) CLICKS
i11	0	2	-3.0 440.0 0.01 0.05 51	0.0 0.0 5.0	0.0 6.0 0.5	20	-1.0 0.0 21 0.1	120 360 8 1.0 1	1
e
