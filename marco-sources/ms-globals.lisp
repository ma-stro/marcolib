;*******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $MSsrc/my-globals.lisp
;-------| Designed and implemented in LeLisp by Marco Stroppa 
;-------| Ported to Common Lisp by Serge Lemouton
;-------| Version: Nov 28, 1996
;-------| Modified March 2000 (added dynamic functions in csound)
;-------| Adapted to omChroma 050221, Marco Stroppa
;*******************************************************************

(in-package :cr)


;******************************************************************
; GLOBAL VARIABLES FOR WHOLE SYSTEM
;------------------------------------------------------------------

; TRANSPOSITION FACTORS FOR FUNCTION "xpose"
;		UPWARD TRANSPOSITION
(set-gbl 'xp2- (interval 100))
(set-gbl 'xp2+ (interval 200))
(set-gbl 'xp3- (interval 300))
(set-gbl 'xp3+ (interval 400))
(set-gbl 'xp4 (interval 500))
(set-gbl 'xp4+ (interval 600))
(set-gbl 'xp5- (interval 600))
(set-gbl 'xp5 (interval 700))
(set-gbl 'xp6- (interval 800)) 
(set-gbl 'xp6+ (interval 900))
(set-gbl 'xp7- (interval 1000))
(set-gbl 'xp7+ (interval 1100))
(set-gbl 'xp8 (interval 1200))
(set-gbl 'xp9- (interval 1300))
(set-gbl 'xp9+ (interval 1400))
(set-gbl 'xp10- (interval 1500))
(set-gbl 'xp10+ (interval 1600))
(set-gbl 'xp11 (interval 1700))
(set-gbl 'xp11+ (interval 1800))
(set-gbl 'xp12- (interval 1800))
(set-gbl 'xp12 (interval 1900))
(set-gbl 'xp13- (interval 2000)) 
(set-gbl 'xp13+ (interval 2100))
(set-gbl 'xp14- (interval 2200))
(set-gbl 'xp14+ (interval 2300))

;		DOWNWARD TRANSPOSITION
(set-gbl '-xp2- (interval -100))
(set-gbl '-xp2+ (interval -200))
(set-gbl '-xp3- (interval -300))
(set-gbl '-xp3+ (interval -400))
(set-gbl '-xp4 (interval -500))
(set-gbl '-xp4+ (interval -600))
(set-gbl '-xp5- (interval -600))
(set-gbl '-xp5 (interval -700))
(set-gbl '-xp6- (interval -800)) 
(set-gbl '-xp6+ (interval -900))
(set-gbl '-xp7- (interval -1000))
(set-gbl '-xp7+ (interval -1100))
(set-gbl '-xp8 (interval -1200))
(set-gbl '-xp9- (interval -1300))
(set-gbl '-xp9+ (interval -1400))
(set-gbl '-xp10- (interval -1500))
(set-gbl '-xp10+ (interval -1600))
(set-gbl '-xp11 (interval -1700))
(set-gbl '-xp11+ (interval -1800))
(set-gbl '-xp12- (interval -1800))
(set-gbl '-xp12 (interval -1900))
(set-gbl '-xp13- (interval -2000)) 
(set-gbl '-xp13+ (interval -2100))
(set-gbl '-xp14- (interval -2200))
(set-gbl '-xp14+ (interval -2300))

;-----------------------------------------------------------------------------
; values needed by dve
(set-gbl 'GEN-MIN 1000)
(set-gbl 'GEN-MAX 6999)
(set-gbl 'GEN-CURR (get-gbl 'GEN-MIN))
(set-gbl 'GEN-FILE *LLve*)
;-----------------------------------------------------------------------------
;(set-gbl 'WTDIR (catenate "/snd/" (getenv "USER")))
				; DIR OF SOUND FILES ON MARC TO LOAD ON MOON
; Temporary place for the dynamically computed GENS of a WT object
(set-gbl 'WTL ())		; A-LIST OF WT OBJECTS
(set-gbl 'WTIND 0)		; INDEX OF CURRENT WT OBJECT

;		CSOUND STRUCTURE : '(	(sf1 dir1 f1 . n-smpls1)
;					(sf2 dir2 f2 . n-smpls2) ...)
;		MOON STRUCTURE   : '( (sf1) (sf2) ...)
;		WHERE:	sfX = name of sound file 1, 2, etc.
;			f1 = function number associated to that sound file
;			for csound a f{fX} function will be associated to
;			   input sf sndin.{position-in-A-list} [symbolic link]
				; DIR OF SOUND FILES ON MARC TO LOAD ON MOON

(set-gbl 'WTFO 7000)		; OFFSET WHEN WRITING CSOUND GEN 1 TABLES
(set-gbl 'WTFOMAX 9999)		; MAXIMUM OFFSET WHEN WRITING CSOUND GEN 1
                                ; PAY ATTENTION THAT IT IS NOT OVERLAPPING WITH
                                ;  THE AUTOMATIC TABLE NUMBER IN OM PREFERENCES
;(set-gbl 'DEFXFC ())		; DEFAULT CONTROL FOR INTERFACE OF WT SYSTEM

;-----------------------------------------------------------------------------
; CONTROL OF THE FREEZE ALGORITHM FOR WT OBJECTS
;  PARAMETERS ARE RELATIVE TO A BASIC OVERLAP, SET TO 10% OF THE DURATION
;    OF THE CURRENT WT OBJECT
; SEE FILE wt-frz.ll IN $LLctl1 FOR DETAILS
(set-gbl 'DEF-FRZ		;  DEFAULT FREEZE PARAMETERS
   '(let ((ovlp (* (dur_wt (curr_wt) (my-si_wt)) 0.1)) )
       `(
	 (inc-pt (* ,ovlp 4.0))		; INCRUSTATION POINT
	 (dur (ran (* ,ovlp 2.0)	; DURATION OF INCRUSTED SEGMENTS
		   (* ,ovlp 0.5)) )
	 (skip (ran (* ,ovlp 4.0)	; SKIP POINT FOR EACH NEW SEGMENT
		    ,ovlp) )
	 (end- ,ovlp)			; FINAL PORTION OF THE SOUND
	 (ampfac 1.0)			; AMPLITUDE SCALER FOR EACH NEW SEGMENT
	 (xin ,ovlp)			; IN/OUT CROSSFADES
	 (xout ,ovlp)
	 (first-xout ,ovlp)
	 (last-xin ,ovlp)
					; LAST-XOUT SET TO THE WHOLE DURATION
					;    OF THE LAST SEGMENT
	 (last-xout (- (seg-dur_wt) (seg-last-xin_wt)) )
	 (min-xin (* (get-gbl 'DURMIN) 0.5) )	; MIN-XIN / XOUT FOR EXTREME
	 (min-xout (* (get-gbl 'DURMIN) 0.5) )	;  TESTS
;	 (min-xin 0.05)
;	 (min-xout 0.05)
	)
     ))

;TEST
;(set-gbl 'DEF-FRZ		;  DEFAULT FREEZE PARAMETERS
;     ''(
;	 (inc-pt 0.1)
;	 (dur 0.5)
;	 (skip 0.2)
;	 (end- 0.25)
;	 (ampfac 0.5)
;	 (xin 0.1)
;	 (xout 0.15)
;	 (first-xout 0.25)
;	 (last-xin 0.2)
;    ))
(set-gbl 'FRZ-MODE 1)		; DEFAULT FREEZE MODE = RUN FREEZE WHEN NEEDED
;-----------------------------------------------------------------------------
; SPECIFICATIION OF THE DEFAULT INSTRUCTIONS FOR FOF AND FLT OBJECTS
;    DEFAULTS ARE A-LISTS THAT CONTAIN ONLY ONE SAMPLE OF THE VALUE OF THE
;    DEFAULT FIELD. THIS VALUE IS EVALUED EACH TIME IT IS RETRIEVED.
;    NOTA: FIELDS THAT ARE NOT PRESENT IN BOTH THE LISP OBJECT AND THE DEFAULT
;          INSTRUCTION WILL PRODUCE A GLOBAL ERROR MESSAGE
#|
(set-gbl 'DEF-FOF '(
	(DUR 1.0)
	(AMP 1.0)
	(BAL (ran 0.5 0.5) )

	(FQMOD (list 0.0 0.0 0.0 7) )	; amp-ran fq-ran d-fq fq-env

	(CAMOD (list 0.0 0.0 0.0) )	; jitr atrm fqtrm
	(CFMOD (list 0.5 0.0 0.0 7) )	; v/j fqvib d-fq fenv
	(CFPRT (list 0.0 7 0.0 0.0 0.0) ) ; d-prt prt-env durprt modflg offset

	(BWENV 7)
	(BWMOD (list 0.0 0.0) )		; amp-ran fq-ran

	(SKENV 7)
	(SKMOD (list 0.0 0.0) )

	(OCTENV 7)
	(OCTMOD (list 0.0 0.0) )

	(AMPIN 1.0)			; input scalers for amp and fq dep mod
	(FQIN 1.0)

	(TEXFLG 0 )			; value is normalized skirts
	(DEBATTN (list -1 -1) )		; automatic computation in the instr
	(OVLP -1)			; automatic computation in the instr

  )
)
|#
;*****************************************************************************
