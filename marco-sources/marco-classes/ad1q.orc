;=============================================================================
;		AD1q.ORC
; ADDITIVE SYNTHESIS INSTRUMENT N. 11 / COMPLETE / QUADRAPHONIC
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

; CONTROL KEYWORDS FOR CHROMA2001 (CTL1)

;  GLOBAL KEYWORDS:
; ACTION-TIME	: start time of the whole event (0.0)
; DURTOT		: total duration of the event (longer components are clipped) (1.0)
; AMPTOT		: amplitude scaler (linear, >0.0-1000.0 or dB, <= 0.0)
; NUMROWS		: amount of rows (components) in the event (1)
;=============================================================================

;  LOCAL KEYWORDS:
;  GENERAL
; E-DELS: entry delays [sec] (0.0)
; DURS	: duration [0-1, scaler of DURTOT] (1.0)
; AMP 	: max amp [0-1000 or dB, scaler of AMPTOT] (-6.0)
; FREQ	: frequency [Hz] (440.0)

;  AMPLITUDE ENVELOPE
; ATK 	: attack time [sec] (0.01)
; DEC 	: decay time [sec] (0.05)
; AENV	: amplitude envelope [GEN] (triangle)

;  AMPLITUDE MODULATION
; JTA 	: jitter amp [0-1, % of max amp+compensation] (0.1)
; TRA 	: tremolo amp [0-1, % of max amp] (0.1)
; TRF 	: tremolo freq [Hz] (5.0)

;  FREQUENCY MODULATION
; JTV 	: vibrato/jitter panpot [0 = all vibrato, 1 = all jitter] (0.5)
; VFQ 	: vibrato freq [Hz] (6.0)
; FDEV	: freq deviation [semitones] (1.0)
; FENV	: frequency envelope [GEN] (triangle)

;  PORTAMENTO
; PLOW	: lowest (minimum) portamento [semitones] (-1.0)
; PUP 	: upper (maximum) portamento amplitude [semitones] (0.0)
; PENV	: portamento envelope [GEN] (asc line)
; PDUR	: portamento duration [sec] (0.1)

;  SPATIAL PROJECTION
; AZ   	: min azimuth [degrees] (0.0)
; AZMAX	: max azimuth [degrees] (360.0)
; AZENV	: envelope for the azimuth [GEN] (asc line)
; AZDUR	: duration of azimuth's envelope [sec, 0=take global dur] (0.0)
; DIST	: distance [0-1, 0=all active, 1=normal pan] (1.0)

; AFIL	: audio function [GEN] (1=sine)
;=============================================================================
;  SUB-COMPONENTS
; NPART	: number of sub-components (3)
; STON	: aleatoric frequency distribution of each sub-component (0.06)
;     	POSITIVE = distribution is linear
;     	NEGATIVE = distribution is logarithmic
; ED2 	: entry delay of each sub-compoment [sec, cumulative] (0.01)
; DUR2	: duration of each sub-component [0-1, nil = use main dur] (nil)
; AMP2	: amplitude of each sub-component [0-1, nil = use main amp] (nil)
;=============================================================================
; COMPULSORY FUNCTIONS :
; f1	= audio sine wave
; f2	= vibrato (sine wave)
; f3	= tremolo (sine wave)
;************************************************************************

; Globals

sr     = 96000
kr     = 96000
ksmps  = 1
nchnls = 4
	
0dbfs = 8388607 ; 24 bits

instr 11

; MAIN INITIALIZATIONS
iramp = 1.0 / 3.0	; Some constants for a good jitter
irfq1 = 20.394		; from a model described in Chant
irfq2 = 9.248
irfq3 = 0.871
irfq4 = 21.714
irfq5 = 9.337
irfq6 = 0.801

ipi 	= 3.14159265358979323846
ipi2	= (ipi * 0.5)
irmsjt	= 2.13	; Factor to balance rms of jit/vib(trem)

idur  	= p3
idurosc = 1.0/p3
iamp 	= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs (p4)))
ifroct	= octcps(p5)	; convert freq into oct for the modulation

iatt	= p6
idec	= p7
iaenv	= p8

; correction on jitter amplitude ONLY, to compensate perceptual energy
iajit	= p9 * irmsjt * iamp
iatrm	= p10 * iamp
itrfq	= p11

ijtvib	= p12
; constrain jitvib between 0 and 1 and assign frequency jitter and vibrato amplitudes
ijtvib	= (ijtvib < 0.0 ? 0.0 : ijtvib)
ijtvib	= (ijtvib > 1.0 ? 1.0 : ijtvib)
ivibfq	= p13
ifqenv	= p15
iprtenv	= p18
iprtdur	= p19
iprtdur	= ( iprtdur <= 0.0 ? 0.01 : iprtdur)	; a duration of 0 cannot be used

; change freq deviation and portamento into octave representation
ifqdev	= p14 / 12	; octave representation (1 = octave)
iprtlow	= p16 / 12
iprtup	= p17 / 12
iprtdev	= (iprtup - iprtlow)

; default signal functions
isigfun	= p25
ivibfun	= 2
itrmfun	= 3

iseed	= 2.0		; seed randi from system time

; INITIALIZATION TESTS
ifjit	= ifqdev * ijtvib * irmsjt	; compensate freq jitter
ifvib	= ifqdev * (1.0 - ijtvib)	
	
; do your best to eliminate notes with potential clicks
; if atk or dec < 0, set it to 0
iatt	= ( iatt < 0.0 ? 0.0 : iatt )
idec	= ( idec < 0.0 ? 0.0 : idec )

; if att+dec > dur, reajust dec, but keep att the same
idec	= ( iatt+idec <= idur ? idec : idur-iatt )
; if att > dur (idec<0), keep att = dur, and set dec to 0
if	(idec>=0.0) goto goon
	iatt = idur
	idec = 0.0
goon:
;OUTBEG
iazmin	= p20
iazmax	= p21
iazdev	= iazmax-iazmin
;iazdev	= (iazmax >= iazmin ? iazmax-iazmin : 360.0+iazmax-iazmin)
	; in case iazmax lies beyond the origin of the circle
iazenv	= p22
iazdur	= (p23 = 0.0 ? idurosc : 1/p23)
idist	= p24
ipanmode= 0	; equal power
;OUTEND

		;    ---------
		;--- Amplitude ---
		;    ---------
; Jitter module
kaj1	randi	iramp,irfq1,iseed
kaj2	randi	iramp,irfq2,iseed
kaj3	randi	iramp,irfq3,iseed
kajit     =	(kaj1 + kaj2 + kaj3) * iajit

; Tremolo module
katrm	poscil3	iatrm,itrfq,itrmfun

; Amplitude modulation
kamod	  =	(kajit + katrm + iamp)

; Amplitude envelope [GEN]
kaenv   poscil3 	kamod,idurosc,iaenv

; Complete amplitude envelope
kamp 	linen 	kaenv,iatt,idur,idec

		;    ---------
		;--- Frequency ---
		;    ---------
; Jitter module
kfj1	randi	iramp,irfq4,iseed
kfj2	randi	iramp,irfq5,iseed
kfj3	randi	iramp,irfq6,iseed
kfjit     =	(kfj1 + kfj2 + kfj3) * ifjit

; Vibrato module
kfvib	poscil	ifvib,ivibfq,ivibfun

; Frequency "modulation"
kfmod	  =	kfjit + kfvib

; Frequency envelope
kfqenv   poscil 	kfmod,idurosc,ifqenv

; Portamento module
kfprt1	oscil1i	0,iprtdev,iprtdur,iprtenv
kfprt	= kfprt1 + iprtlow

; Global frequency 
kfrq	  =	cpsoct( kfqenv + ifroct + kfprt)

			;    ---------------  -------
	        ;--- Main DSP module  -------
			;    ---------------  -------

asound 	poscil 	kamp,kfrq,isigfun
				
;OUTBEG
		;    ------- ---
		;--- Panning ---
		;    ------- ---
iazmin	= (iazmin % 360.0) ; azimuth in degrees, normalized 0-360

kpan	poscil	1.0, iazdur, iazenv
kpan	= kpan*iazdev
kpan	= kpan+iazmin
kpan	= kpan % 360.0

if ((kpan >= 0.0) && (kpan < 90.0)) goto quad1
if ((kpan >= 90.0) && (kpan < 180.0)) goto quad2
if ((kpan >= 180.0) && (kpan < 270.0)) goto quad3
aut4, aut1	pan2	asound, ((kpan % 90.0) / 90.0), ipanmode
goto end

quad1:
aut1, aut2	pan2	asound, (kpan / 90.0), ipanmode
goto end

quad2:
aut2, aut3	pan2	asound, ((kpan % 90.0) / 90.0), ipanmode
goto end

quad3:
aut3, aut4	pan2	asound, ((kpan % 90.0) / 90.0), ipanmode

end:
		outc	aut1*(sin (idist))+asound*(sin (1-idist)), \
				aut2*(sin (idist))+asound*(sin (1-idist)), \
				aut3*(sin (idist))+asound*(sin (1-idist)), \
				aut4*(sin (idist))+asound*(sin (1-idist))
;OUTEND

endin
