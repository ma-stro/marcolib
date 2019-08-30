;*****************************
; COMPLETE TESTS OF MARCOLIB |
;*****************************

(in-package :cl-user)
(let ((lib (om:find-library "OMChroma")))
  (unless (om::loaded? lib)
    (om::load-om-lib lib)))

(let ((lib (om:find-library "marcolib")))
  (unless (om::loaded? lib)
    (om::load-om-lib lib)))

(in-package :cr)

#|
 '(
   "marco-sources/ms-init"
   "marco-sources/back-from-om"
   "marco-sources/ms-globals"
   "marco-sources/ms-utils"

   "marco-sources/pls/dve"

   "marco-sources/dg/ms-utils-dg"
   "marco-sources/dg/DP"
   "marco-sources/dg/DPV"
   "marco-sources/dg/DPF"
   "marco-sources/dg/lkp.lisp"
   "marco-sources/dg/sp-dg.lisp"

   "marco-sources/marco-classes/ad1m"
   "marco-sources/marco-classes/ad1s"
   "marco-sources/marco-classes/fm1m"

   "marco-sources/userfuns/test-user-funs"
|#


; INITIALIZATIONS
;____________________________________________________________________________
; marcolib/marco-sources/ms-init.lisp
;____________________________________________________________________________
(get-gbl '*MSDATA*)
(get-gbl ''*MSdb*)
(get-gbl '*Llwt*)
(get-gbl '*WTdir*)

;(cl-user::decode-local-path "marcolib-tests.lisp")


; SAME FUNCTIONS DEFINED IN OM (utils-from-chroma.lisp)
;____________________________________________________________________________
; marcolib/marco-sources/back-from-om.lisp
;____________________________________________________________________________
;******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/utils-om.lisp
;-------| Implemented by Marco Stroppa
;-------| Version: 000817, updated 050221
;******************************************************************

; THIS FILE CONTAINS THE DEFINITION OF MIXED FUNCTIONS GENERALLY USEFUL FOR
;    FOR THE CHROMA SYSTEM BUT AVAILABLE TO OM USERS AS WELL.

(in-package :cr)

;------------------------------------------------------------------
; AVAILABLE FUNCTIONS :
;       clip
;	beat->secs
;	interval
;	load-files
;	nextl
;	printdate / stringdate
;	sign /invert-sign
;------------------------------------------------------------------


;------------------------------------------------------------------
; clip
;(defun clip (val &optional (min 0.0) (max 1.0))
;" If val is below min, return min,
;  if val is above max, return max,
;  otherwise return val." 
(clip 3.0)
(clip 3.0 0.0 4.0)
(clip 3.0 4.0 5.0)
;------------------------------------------------------------------
; interval
;(defun interval (val)
;"Return the interval of val [cents] as a scaler"
(interval 100)
(interval -100)

;(defun nroot (root base)
;"Compute the nth root of base"
(nroot 3 1000)

;------------------------------------------------------------------
; beat->secs

;(defun beat->secs (list MM)
;" List: one or a list of times (markers); MM: metronome.
;  Return the same list converted into absolute seconds."  
(beat->secs 10 120)
(beat->secs '(0 1 2 3) 30)
;------------------------------------------------------------------
; (sign a [b]) / (invert-sign a)
;	return the sign of a (+1.0 or -1.0)
;	if b is present, return the sign of b-a
(sign 10)
(sign -10)

(invert-sign 10)
(invert-sign -10)
;------------------------------------------------------------------
;(defun closest-pwr2 (val)
;  "Return the closest larger power of two) of val, ex. 3.4 --> 4. Useful for csound audio tables."
(closest-pwr2 3.4)
(closest-pwr2 4098)
;------------------------------------------------------------------


; PERSONAL GLOBAL VARIABLE
;____________________________________________________________________________
; marcolib/marco-sources/ms-globals.lisp
;____________________________________________________________________________
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
; MOVED TO OMCHROMA GLOBALS
; TRANSPOSITION FACTORS FOR FUNCTION "xpose"
;		UPWARD TRANSPOSITION
#|
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
|#

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


; UTILITIES
;____________________________________________________________________________
; marcolib/marco-sources/ms-utils.lisp
;____________________________________________________________________________
;*******************************************************************
;-------| CHROMA SYSTEM / PERSONAL LIBRARY
;-------| This file is: $MSsrc/ms-test-utils.lisp
;-------| Designed and implemented in MCL by Marco Stroppa 
;-------| Version: 050306
;*******************************************************************

(in-package :cr)

;******************************************************************
; FUNCTIONS
;	build-sequence
;------------------------------------------------------------------

;(defun build-sequence (from to steps &optional (ston 0.0) (exp 0.0))
;"Build a sequence of <steps> steps between from and to.
;Ston: aleatoric change added after computing the sequence
;        (1 = 100% of the current value)
;Exp: exponential factor, 0.0 = linear, > 0.0 expon, < 0.0 log"

(build-sequence 0 10.0 11 0.0 0.0)
(build-sequence 0 10.0 11 0.1 0.0)
(build-sequence 0 10.0 11 0.0 1.0)
(build-sequence 0 10.0 11 0.0 -1.0)
(build-sequence 0 10.0 11 0.0 'sin1)
(build-sequence 0 10.0 11 0.0 'sin2)
(build-sequence 0 10.0 11 0.0 'sin3)
;------------------------------------------------------------------

;******************************************************************

; ABSTRACT TYPES
;____________________________________________________________________________
; marcolib/marco-sources/pls/dve.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/dve.lisp
;-------| Version V1.0: Mar 26, 2000
;-------| By Marco Stroppa
;-------| Copyright 2000 IRCAM
;*****************************************************************************
(in-package :cr)

; PACKAGE TO DEAL WITH DYNAMIC VIRTUAL ENVELOPES
;                 ASSOCIATED TYPE NAME: DVE


; DESCRIPTION OF THE DATA STRUCTURE:
;    A Dynamic Virtual Envelope consists of a break-point function of type FUN
;       that has not yet been compiled for a specific synthesizer. This means
;       that when the synthesizer is called, all the dynamic functions will
;       be first compiled for that synthesier, then linked to the functions
;       of the data base.

;    In the case of csound:
;        put the fun object into a "hash table" and assign to it a default
;            GEN number (see default variables: GEN-MIN, GEN-MAX, GEN-CURR,
;            GEN-FILE, DYN-GENS-LIST and the new type "DVE")
;        if the FUN object already exists (use a function with a threshold to
;           decide) return the GEN number associated to it
;        at the end, when loading the functions for csound, compile those
;            contained in DYN-GENS-LIST if it is not empty and write them onto
;            the file GEN-FILE in the directory LLfun.


; CURRENT IMPLEMENTATION:
;	hash-table with a FUN and a number


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_dve

;	SELECTORS:    fun_dve
;                     num_dve

;	MODIFIERS:    add_dve
;		      rm_dve
;		      reset_dve

;	PREDICATES:   is_dve
;                     is-empty_dve

;	INFO:	      print_dve
;		      short-print_dve
;		      sort-keys_dve
;		      count-keys_dve



; DESCRIPTION OF THE PACKAGE:

;------------------------------------------------------------------
;	NAME:		make_dve  (CONSTRUCTOR)
;	TYPE:		Expr with 1 argument
;	CALL:		(make_dve fun)
;	FUNCTION:	define and initialize a structure of type DVE
;	VALUE:		the new structure
;	SOURCE:		$LLpls/dve.ll
(setf ms-test-dve (make_dve))
(setf ms-test-fun (make_fun '(0 0 1 1 0.5 2)))
(setf ms-test-fun1 (make_fun '(0 0 1 1 0.5 2 0 3)))
;------------------------------------------------------------------
;	NAME:		add_/rm_dve  (MODIFIERS)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(add_dve fun [dve]) / (rm_dve key [dve])
;	FUNCTION:	add the current fun to the DVE structure and return a GEN
;                          number; if fun is already there, return the corresponding
;                          number / remove the number and return 'OK
;	VALUE:		a good number of GEN or OK
;	SOURCE:		$LLpls/dve.ll
;(defun add_dve (fun &optional (dve (get-gbl 'DYN-GENS-LIST)))
;"Add a function to the table unless it is already present and return its key
;If the object is not a function, it is supposed to be RAW DATA directly expressed
;in the syntax of the current synthesizer.
;Only its number will be dynamicall allocated"
;(compare-fun '(make_fun '(0 0 1 1 0.5 2)) ms-test-dve)
;(get-free-gen-num)

(add_dve '(make_fun '(0 0 1 1 0.5 2)) ms-test-dve)
(add_dve 'ms-test-fun1 ms-test-dve)
(rm_dve 'ms-test-fun1 ms-test-dve)

(print_dve ms-test-dve)
(short-print_dve ms-test-dve)

;------------------------------------------------------------------
;	NAME:		fun_/num_dve  (SELECTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(fun_dve fun [dve])
;	FUNCTION:	return the function associated to the key or the number
;                         associated to the first function matching the arg [the first
;                         function is the first one found in the structure and not
;                         necessarily the one with the lowest number (if several exist)]
;                         nil if none are found
;	VALUE:		the new data
;	SOURCE:		$LLpls/dve.ll
(fun_dve 1001 ms-test-dve)
(eval (fun_dve 1001 ms-test-dve))
(num_dve 'ms-test-fun ms-test-dve)
(num_dve 'ms-test-fun1 ms-test-dve)
(num_dve '(make_fun '(0 0 1 1 0.5 2)) ms-test-dve)

;------------------------------------------------------------------
;	NAME:		reset_dve  (MODIFIER)
;	TYPE:		Expr with 0 or 1 arguments
;	CALL:		(reset_dve [dve])
;	FUNCTION:	reset the current  DVE structure
;	VALUE:		the reset structure
;	SOURCE:		$LLpls/dve.ll

(reset_dve ms-test-dve)
(print_dve ms-test-dve)

(csgen->fun

;------------------------------------------------------------------
;	NAME:		is_dve, is-empty_dve  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_dve dve)
;	VECTION:	test whether the argument is a structure of type DVE
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/dve.ll
(is_dve ms-test-dve)
(is-empty_dve ms-test-dve)

(add_dve '(make_fun '(0 0 1 1 0.5 2)) ms-test-dve)
(add_dve 'ms-test-fun1 ms-test-dve)
(add_dve 'ms-test-fun ms-test-dve)
;------------------------------------------------------------------
;	NAME:		print_/short-print_dve  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_ve ve)
;			(short-print_dve dve)
;	VECTION:	nicely or shortely print a structure of type DVE
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/dve.ll

(print_dve ms-test-dve)
(short-print_dve ms-test-dve)

;------------------------------------------------------------------
;	NAME:		sort-/count-keys_dve  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(sort-keys_dve dve)
;	FUNCTION:	returns all the entries of a hash table sorted by '< or
;                         their amount
;	VALUE:		the value above
;	SOURCE:		$LLpls/dve.ll

(count-keys_dve ms-test-dve)
(sort-keys_dve ms-test-dve)

;==================================================================
; SUBSIDIARY FUNCTIONS FOR DVE
(get-free-gen-num)
;(reset-free-gen-num)
;(reset-gen-list (contents ms-test-dve))
(compare-fun 'ms-test-fun ms-test-dve)
(compare-fun '(make_fun '(0 0 1 1 0.5 2)) ms-test-dve)
(compare-fun '(make_fun '(0 0 1 1 0.5 3)) ms-test-dve)
;*****************************************************************************

; DG (Data Generation)
; UTILITIES
;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-utils-dg.lisp
;____________________________________________________________________________
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	load-db
;	load-ve
;	build-cwt
;	build-fqwt
;	load-db / load-ve
;-----------------------------------------------------------------------------
; load-db / load-ve
; (load-db name)
;(defun load-db (name)
;"Load a given data base from $LLdb
;The extension of the file to be loaded is : "name"_db.lisp"
(load-db ())
(load-db 'test)
(load-db "test")

;-----------------------------------------------------------------------------
;(defun load-ve (name)
;"Load-ve loads a data base of virtual envelopes that is un LLdb/ve"
(load-ve "test")
;-----------------------------------------------------------------------------
; build-cwt
; (build-cwt L-CWT)
;  EX: (build-cwt wt-a1 (wt-a2 0 1) (wt-a3 (att_wt wt-a3) (dur_wt wt-a3)))
;(defun build-cwt (&rest l-cwt)
;"B(uild) a CWT field for CTL1 level (shortcut)
;where L-CWT: any wt objects (just the name if alone, the list (name val1 val2) if start and end offs are set)"
#|


    (mapcar (lambda (wt)
		(if (symbolp wt)
		    `'(list ',wt)
		    `'(list ',(nextl wt) ,.wt)) )
	    l-cwt) )
|#
;-----------------------------------------------------------------------------
; build-fqwt
; (build-fqwt L-FQ)
;  EX: (build-fqwt (1.0) (1.059 (nth-freq_wt wt-a2 2)) (440 261))
;(defun build-fqwt (&rest l-fqwt)
;"B(uild) a FQ field for CTL1 level WTsys (shortcut).
;where L-FQ: list of (out-fq [ref-fq]) by default, ref-fq = 1.0."
(build-fqwt '(1.0) '(1.059 (nth-freq_wt wt-a2 2)) '(440 261))
;-----------------------------------------------------------------------------

; DATA AS POINTERS (BASIC)
;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-DP.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DP.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

;-----------------------------------------------------------------------------
; DPV AND DPF FILES CONTAIN ALL THE FUNCTIONS THAT GENERATE DATA PASSED
;    AS POINTERS TO A DATA BASE
; THE WAY POINTER INFORMATION IS GENERATED FOLLOWS THE SAME CRITERIA AS DV.ll
;    AND DF.ll (VALUES OR FUNCTIONS CONTROLLING POINTERS, SEE THE FILES DPV.ll
;    AND DPF.ll)
; IN ADDITION, WHEN THE POINTER IS NOT INTEGER, ONE CAN CHOOSE BETWEEN
;    INTERPOLATING BETWEEN EITHER THE TWO ADJACENT VALUES OR TRUNCATING
;    THE POINTER TO THE CLOSEST INTEGER. FOR INSTANCE, IF i1=10 and i2=20,
;    A POINTER OF 1.3 WILL YIELD RESPECTIVELY 13 OR 10.
; THE NAMES OF THE FUNCTIONS ARE THE SAME AS DV AND DF, WITH THE PREFIX
;    "p-" FOR NON INTERPOLATING POINTERS AND "pi-" FOR INTERPOLATING ONES.
;-----------------------------------------------------------------------------
; FOR THE TIME BEING, A DATA BASE IS A STRAIGHT VECTOR. IN THE FUTURE,
;     ANY TRANSACTIONS WITH IT WILL BE MADE MORE ABSTRACT.
; TO FILL A DATA BASE, EITHER INITIALIZE THE VECTOR BY HAND, OR USE SOME
;    OTHER DG PROCEDURES THAT WILL HAVE TO BE CONVERTED INTO VECTORIAL
;    INFORMATION [ EX: (apply 'vector (lkp 20 (make_fun (0 0 1 1)))) ]
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; THIS FILE CONTAINS ONLY SOME USEFUL COMPLEMENTARY FUNCTIONS USED BY DPV AND
;    DPF
;-----------------------------------------------------------------------------
(in-package :cr)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; USEFUL COMPLEMENTARY FUNCTIONS
;	vind
;	vitp
;	ovfl-pr
;	adjust-limits
;	check-stp
;	p-set-result / pi-set-result
;	p-mod-set-result / pi-mod-set-result
;	build-cl
;-----------------------------------------------------------------------------
; (vind VECT VAL [PRECISION])
; 	return the index of the first place = VAL+/-PRECISION in VECT
;	return () if nothing is found
;	PRECISION = precision in the computation (0.0 by default), absolute value
; EX: (setf a #(0 .5 1 2 3 4 5)), (vind a 2.1 0.3) ===> 6

(setf ms-test-dp (apply 'vector (lkp 21 (make_fun '(0 0 1.0 1)))))
(setf ms-test-dp1 #(0 .5 1 2 3 4 5))
(vind ms-test-dp1 2.1 0.3)
(vind ms-test-dp1 2)
(vind ms-test-dp1 2 1.0)
(vind ms-test-dp1 2 3.0)

(length ms-test-dp)

(vind ms-test-dp 0.5)

;-----------------------------------------------------------------------------
; (vitp VECT IND) : VREF WITH FLOAT INDEX
; 	return a value with interpolation
;       if the value is not a number signal an error
; EX: (setf a #(10 11 12 13 14 15)), (vitp a 3.5) ===> 13.5

(vitp ms-test-dp1 0.3)
(vitp ms-test-dp1 0.5)
(vitp ms-test-dp1 1.0)
(setf ms-test-dp2 #(10 11 12 13 14 15))
(vitp ms-test-dp2 3.5)

;(vitp #(10 11 a 13 14 15) 1.2) -> error

;-----------------------------------------------------------------------------
; (ovfl-pr VAL ALT TEST)
; 	protection against indexes beyond the limits of a vector
;	VAL = list with lower (from) or upper (to) limit (can be = ())
;	if VAL = (), choose the ALT limit
;	   else if VAL > TEST choose TEST, else return VAL

(ovfl-pr () 2.2 4.4)
(ovfl-pr 10 1.1 3.3)
(ovfl-pr 10 1.1 13.3)
(ovfl-pr '(3) 1 11.1)
(ovfl-pr '(3 4 5) 4 2)

;-----------------------------------------------------------------------------
; (adjust-limits ())		; USE DYN SCOPING
;	if upper limit > lower limit, swap them
;	return 1.0 if limits have not been swapped, -1.0 if so
;	upper and lower limits are dynamically bound to "from" and "to"
#|
(defun adjust-limits ()
  (declare (special to from))
  (if (>= to from)
    1.0
    (let ((tmp from))
      (setf from to)
      (setf to tmp)
      -1.0)) )
|#
;-----------------------------------------------------------------------------
; (check-stp FUN)			; USE DYN SCOPING
;	if step is negative, reject instruction
;	FUN is the calling function
#|
(defun check-stp (fun)
  (declare (special stp))
  (when (< stp 0)
    (error "CAN'T WORK WITH NEGATIVE STEPS. CHANGE FROM/TO FIELDS INSTEAD!  ~a
CALLED BY: ~a~%"
           stp fun))
  )
|#
;-----------------------------------------------------------------------------
; (p-set-result / pi-set-result)		; DYN SCOPING
;	prepare the final list by getting the values pointed to by a control
;	   list (cl) with test and clipping if they overflow
#|
(defun p-set-result ()
  (declare (special cl db nev to from result))
  (loop while (>= (decf nev) 0)
	do (let ((vi (if
                       (not
                        (numberp (car cl)) )
                       (error "CONTROL LISTS MUST BE ALL NUMERIC, SIR: ~a~%" cl)
                       (truncate (+ (nextl cl) from 0.5)))) )
             (when (> vi to) (setf vi to))
             (when (< vi from) (setf vi from))
             (newl result (svref db vi))) )
  (nreverse result))

(defun pi-set-result ()
  (declare (special cl db nev to from result))
  (loop while (>= (decf nev) 0)
        do (let ((vi (if
                       (not
                        (numberp (car cl)) )
                       (error "CONTROL LISTS MUST BE ALL NUMERIC, SIR: ~a~%" cl)
                       (+ (nextl cl) from))) )
             (when (> vi to) (setf vi to))
             (when (< vi from) (setf vi from))
             (newl result (vitp db vi))) )
  (nreverse result))
|#
;-----------------------------------------------------------------------------
; (p-mod-set-result / pi-mod-set-result)	; DYN SCOPING
;	prepare the final list by getting the values pointed to by a control
;	   list (cl) with test and foldover if they overflow
#|
(defun p-mod-set-result ()
  (declare (special cl db nev to from result))
  (loop while (>= (decf nev) 0)
	do (let* ((size (1+ (abs (- to from))))
	          (vi (mod
                       (truncate (+ (nextl cl) from 0.5))
                       size)) )
	     (newl result (svref db vi))) )
  (nreverse result))

(defun pi-mod-set-result ()
  (declare (special cl db nev to from result))
  (loop while (>= (decf nev) 0)
	do (let* ((size (1+ (abs (- to from))))
	          (vi (mod
                       (+ (nextl cl) from)
                       size)) )
	     (newl result (vitp db vi))) )
  (nreverse result))
|#
;-----------------------------------------------------------------------------
; (build-cl FROM TO) / (build-bkwd-cl FROM TO)
;	prepare a control list (cl) made of integers between FROM and TO
;	bkwd: build a copy straightforward and backward
; gives an infinite list!!
(firstn 100 (build-cl 10 20))
(firstn 100 (build-bkwd-cl 10 20))
(firstn 100 (build-bkwd-cl 20 10))
(firstn 100 (build-bkwd1-cl 10 20))
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-DPV.lisp
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DPV.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS POINTERS
;    TO A DATA BASE

(in-package :cr)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;    FUNCTIONS WITH CONTROL LIST (CL)
;	p-l-val / pi-l-val	: list of vals (out-of-bound vals are clipped)
;	p-mod-l-val / pi-mod-l-val
;				:  "  "  " (out-of-bound vals are folded over)
;	p-lp / pi-lp		: loop (with starting offset)
;	p-bkwd-lp / pi-bkwd-lp	: backward loop
;	p-rept-lp / pi-rept-lp
;				: repetition with loop at the end
;	p-bkwd-rept-lp / pi-bkwd-rept-lp
;				: repetition with backward loop at the end
;    FUNCTIONS WITH STEP (STP)
;	p-rept / pi-rept	: repetition scaled to interval
;	p-mod-rept / pi-mod-rept: repetition with loop modulo the size
;	p-bkwd-mod-rept / pi-bkwd-mod-rept :
;				: repetition with modulo back and forth
;	p-stp-rept / pi-stp-rept
;				: repetition with step and stop at the end
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; FUNCTIONS WITH CONTROL LIST
;-----------------------------------------------------------------------------
; (p-l-val / pi-l-val NEV CL DB [FROM TO])
; 	if (length CL) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els pointed to by CL (control list)
;	values in CL are relative to FROM and clipped by TO and FROM
; EX: (setf db  '#(11 12 13 14 15 16 17 18 19))
;     (setf  a  '(2 3 4)), (p-l-val 5 a db) ===> '(13 14 15 15 15)
; REMARK: FROM and TO fields are always taken as FROM < TO
(p-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(p-l-val 3 '(0 1 2 3 4 5) ms-test-dp)
(p-l-val 5 '(-1 0 1 2 3 4 5) ms-test-dp)
(p-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp)

(pi-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(pi-l-val 3 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-l-val 5 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp)

(p-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 4)
(p-l-val 3 '(0 1 2 3 4 5) ms-test-dp 2 5)
(p-l-val 5 '(-1 0 1 2 3 4 5) ms-test-dp 2 5)
(p-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)

(pi-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 5)
(pi-l-val 3 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-l-val 5 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-mod-l-val / pi-mod-l-val NEV CL DB [FROM TO])
; 	if (length CL) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els pointed to by CL (control list)
;	values in CL are relative to FROM and folded over around TO and FROM
; EX: (setf db #(11 12 13 14 15 16 17 18 19))
;      (setf a '(2 3 4)), (p-mod-l-val 5 a db) ===> '(13 14 15 15 15)
; REMARK: FROM and TO fields are always taken as FROM < TO

(p-mod-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(p-mod-l-val 10 '(0 1 2 3 4 5) ms-test-dp)
(p-mod-l-val 5 '(-1 0 1 2 3 4 5) ms-test-dp)
(p-mod-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp)

(pi-mod-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(pi-mod-l-val 3 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-mod-l-val 5 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-mod-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp)

(p-mod-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 4)
(p-mod-l-val 3 '(0 1 2 3 4 5) ms-test-dp 2 5)
(p-mod-l-val 5 '(-1 0 1 2 3 4 5) ms-test-dp 2 5)
(p-mod-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)

(pi-mod-l-val 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 5)
(pi-mod-l-val 3 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-mod-l-val 5 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-mod-l-val 5 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-lp / pi-lp NEV CL DB [FROM TO OFFS])
; 	loop along the pointers contained in CL (control list)
;	else return the first NEV els pointed to by CL starting loop at OFFS
;	values in CL are relative to FROM and clipped by TO and FROM

; if NEV > (length <control-list>), start again from the beginning

(p-lp 20 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(p-lp 20 '(0 1 2 3 4 5) ms-test-dp)
(p-lp 20 '(-1 0 1 2 3 4 5) ms-test-dp)
(p-lp 20 '(-1 0 1000 2 3 4 5) ms-test-dp)

(pi-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(pi-lp 10 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-lp 10 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp)

(p-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 4)
(p-lp 10 '(0 1 2 3 4 5) ms-test-dp 2 5)
(p-lp 10 '(-1 0 1 2 3 4 5) ms-test-dp 2 5)
(p-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)

(pi-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 5)
(pi-lp 10 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-lp 10 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-bkwd-lp / pi-bkwd-lp NEV CL DB [FROM TO OFFS])
; 	loop along the values contained in CL going backward and forward
;	   between FROM and TO in data base DB starting at OFFS
; EX: (setf a '(1 2 3)), (pi-bkwd-lp 10 a db) ===> '(1 2 3 3 2 1 1 2 3 3)
(p-bkwd-lp 20 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(p-bkwd-lp 20 '(0 1 2 3 4 5) ms-test-dp)
(p-bkwd-lp 20 '(-1 0 1 2 3 4 5) ms-test-dp)
(p-bkwd-lp 20 '(-1 0 1000 2 3 4 5) ms-test-dp)

(pi-bkwd-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp)
(pi-bkwd-lp 10 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-bkwd-lp 10 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp)
(pi-bkwd-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp)

(p-bkwd-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 4)
(p-bkwd-lp 10 '(0 1 2 3 4 5) ms-test-dp 2 5)
(p-bkwd-lp 10 '(-1 0 1 2 3 4 5) ms-test-dp 2 5)
(p-bkwd-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)

(pi-bkwd-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 5)
(pi-bkwd-lp 10 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-bkwd-lp 10 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-bkwd-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-rept-lp / pi-rept-lp NEV CL DB [FROM TO])
; 	return the values pointed to by CL with looping at the end
;       CL must be of the form '(1 2 3 4 (7 8 9))
;	scale reading to interval TO-FROM
; EX: (setf a '(1 2 3 (7 8))), (p-rept-lp 10 a db) ===> '(1 2 3 7 8 7 8 7 8 7)
; NEW 1908
;     if the list has no loop parentheses at the end, NO ERROR IS SIGNALED, and the last
;        value is repeated

(p-rept-lp 20 '(1 1.1 2 2.2 3 3.3 (4 4.4 5 5.5)) ms-test-dp)
(p-rept-lp 20 '(0 1 2 3 (4 5)) ms-test-dp)
(p-rept-lp 20 '(-1 0 1 2 (3 4 5)) ms-test-dp)
(p-rept-lp 20 '(-1 0 1000 2 (3000 4 5)) ms-test-dp)

(pi-rept-lp 20 '(1 1.1 2 2.2 3 (3.3 4 (4.4 5 5.5))) ms-test-dp)
(pi-rept-lp 10 '(0.5 1.5 2.5 3.5 (4.5 5.5)) ms-test-dp)
(pi-rept-lp 10 '(-1.5 0 1.5 2.5 (3.5 4.5 5.5)) ms-test-dp)
(pi-rept-lp 10 '(-1 0 1000 2 3 (4 5)) ms-test-dp)

(p-rept-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 4)
(p-rept-lp 10 '(0 1 2 3 4 5) ms-test-dp 2 5)
(p-rept-lp 10 '(-1 0 1 2 3 4 5) ms-test-dp 2 5)
(p-rept-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)

(pi-rept-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 5)
(pi-rept-lp 10 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-rept-lp 10 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-rept-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-bkwd-rept-lp / pi-bkwd-rept-lp NEV CL DB [FROM TO])
; 	return the values pointed to by CL with backward looping at the end
;	scale reading to interval TO-FROM

(p-bkwd-rept-lp 20 '(1 1.1 2 2.2 3 3.3 (4 4.4 5 5.5)) ms-test-dp)
(p-bkwd-rept-lp 20 '(0 1 2 3 (4 5)) ms-test-dp)
(p-bkwd-rept-lp 20 '(-1 0 1 2 (3 4 5)) ms-test-dp)
(p-bkwd-rept-lp 20 '(-1 0 1000 2 (3000 4 5)) ms-test-dp)

(pi-bkwd-rept-lp 20 '(1 1.1 2 2.2 3 (3.3 4 (4.4 5 5.5))) ms-test-dp)
(pi-bkwd-rept-lp 10 '(0.5 1.5 2.5 3.5 (4.5 5.5)) ms-test-dp)
(pi-bkwd-rept-lp 10 '(-1.5 0 1.5 2.5 (3.5 4.5 5.5)) ms-test-dp)
(pi-bkwd-rept-lp 10 '(-1 0 1000 2 3 (4 5)) ms-test-dp)

(p-bkwd-rept-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 4)
(p-bkwd-rept-lp 10 '(0 1 2 3 4 5) ms-test-dp 2 5)
(p-bkwd-rept-lp 10 '(-1 0 1 2 3 4 5) ms-test-dp 2 5)
(p-bkwd-rept-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)

(pi-bkwd-rept-lp 10 '(1 1.1 2 2.2 3 3.3 4 4.4 5 5.5) ms-test-dp 2 5)
(pi-bkwd-rept-lp 10 '(0.5 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-bkwd-rept-lp 10 '(-1.5 0 1.5 2.5 3.5 4.5 5.5) ms-test-dp 2 5)
(pi-bkwd-rept-lp 10 '(-1 0 1000 2 3 4 5) ms-test-dp 2 5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; FUNCTIONS WITH STEP
;-----------------------------------------------------------------------------
; (p-rept / pi-rept NEV DB [FROM TO])
; 	read into DB with equally spaced steps NEV values
;	scale reading to interval TO-FROM
; EX: (pi-rept 10 db) ===> read all the db step by step for 10 times
(p-rept 30 ms-test-dp)
(p-rept 30 ms-test-dp 2 8)

(pi-rept 30 ms-test-dp)
(pi-rept 30 ms-test-dp 2 8)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-stp-rept / pi-stp-rept NEV STP DB [FROM TO])
; 	read into DB with equally spaced steps of STP NEV values
;	repeat last value until NEV if needed
(p-stp-rept 30 2.1 ms-test-dp)
(pi-stp-rept 30 2.1 ms-test-dp)
;-----------------------------------------------------------------------------
; (p-mod-rept / pi-mod-rept NEV STP DB [FROM TO OFFS])
; 	loop along the DB between FROM and TO at a step STP
;	start loop at OFFS (must always be positive from FROM)
(p-mod-rept 30 2.1 ms-test-dp)
(pi-mod-rept 30 2.1 ms-test-dp)

(p-mod-rept 30 2.1 ms-test-dp 2 9)
(pi-mod-rept 30 2.1 ms-test-dp 2 9)
;-----------------------------------------------------------------------------
; (p-bkwd-mod-rept / pi-bkwd-mod-rept NEV STP DB [FROM TO OFFS])
; 	loop along the DB back and forth between FROM and TO at a step STP
;	start loop at OFFS (must always be positive from FROM)
(p-bkwd-mod-rept 30 2.1 ms-test-dp)
(pi-bkwd-mod-rept 30 2.1 ms-test-dp)

(p-bkwd-mod-rept 30 2.1 ms-test-dp 2 9)
(pi-bkwd-mod-rept 30 2.1 ms-test-dp 2 9)
;-----------------------------------------------------------------------------



;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-DPF.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DPF.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILE CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS POINTERS
;    TO A DATA BASE

(in-package :cr)

;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	p-lkp / pi-lkp		: linear interpolation (fit to db)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-lkp / pi-lkp NEV CFUN DB [FROM TO])
; 	sample CFUN NEV equally spaced times and use result as a ptr into DB
(p-lkp 31 (make_fun '(0 0 1 1 0 2)) ms-test-dp)
(pi-lkp 31 (make_fun '(0 0 1 1 0 2)) ms-test-dp)

(p-lkp 31 (make_fun '(0 0 1 1 0 2)) ms-test-dp 2 8)
(pi-lkp 31 (make_fun '(0 0 1 1 0 2)) ms-test-dp 2 8)
;-----------------------------------------------------------------------------


; LOOK-UP FUNCTIONS
;____________________________________________________________________________
; marcolib/marco-sources/dg/lkp.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/lkp.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS FUNCTIONS TO GENERATE OTHER CONTROL FUNCTIONS (TYPE FUN)
;	IN AN EFFICIENT AND CATEGORICAL WAY

(in-package :cr)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	make-lkp3a
;	make-lkp3b
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (make-lkp3a Y_START Y_END MIDDLE)
;	generate a two-segment function going from Y_START to Y_END and
;	   passing through Y_MIDDLE
;	MIDDLE	= (Y_MIDDLE . X_MIDDLE)
; EX: (make-lkp3a 0.2 '(1.0 0.3) 0.3)
(make-lkp3a 0.2 '(1.0 0.3) 0.3)
(make-lkp3a (ran 0.2 0.05) (list 1.0 (ran 0.3 0.04)) (ran 0.3 0.1))
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (make-lkp3b Y_START Y_END MIDDLE [CTL])
;	generate a six-segment function going from Y_START to Y_END and
;	   passing through Y_MIDDLE
;	each segment Y_START - Y_MIDDLE and Y_MIDDLE - Y_END is subdivided
;	   into three parts, Y_START -> Y1a -> Y1b -> Y_MIDDLE, etc.
;	MIDDLE	= (Y_MIDDLE . X_MIDDLE)
;	CTL	= ((Y1-scl . Y2-scl) (X1-scl . X2-scl))
;	   default: ((0.3 . 0.7) (0.66 . 0.33))
; EX: (make-lkp3b 0.2 '(1.0 0.3) 0.3)
; EX: (make-lkp3b 0.2 '(1.0 0.3) 0.3 '(0.1 . 0.9) '(0.4 . 0.2) )
(make-lkp3b 0.2 '(1.0 0.3) 0.3)
(make-lkp3b 0.2 '(1.0 0.3) 0.3 '(0.1 . 0.9) '(0.2 . 0.4) )
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; marcolib/marco-sources/dg/sp-dg.lisp
;____________________________________________________________________________
;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/sp-dg.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS SPECIAL-PURPOSE FUNCTIONS TO USE WITH DG SYS

(in-package :cr)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	chk-dur
;	p-ve
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (chk-dur DUR AT GBL-DUR)
;	check the DURation of an event starting at time AT, and clip it with
;	GBL-DUR
(chk-dur 1.0 0.0 0.5)
(chk-dur 1.0 0.0 1.5)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-ve VAL DB [FROM TO])
;	extract 1 item from a data-base of virtual envelopes (type ve)
;       argument x is FROM / TO fields of p-l-val
; EX: (setf db #(f-11 f-12 f-13))
;      (p-ve 0 db) ===> (eval f-11)
(setf ms-test-ve #(ve-11 ve-12 ve-13 ve-14 ve-15 ve-16))
(setf ve-11 (make_ve (make_fun '(0 0 1 1)) 101))
(setf ve-12 (make_ve (make_fun '(1 0 0 1)) 102))
(setf ve-13 (make_ve (make_fun '(0 0 1 1 0 2)) 103))
(setf ve-14 (make_ve (make_fun '(0 0 1 1 0 2)) 104))
(setf ve-15 (make_ve (make_fun '(0 0 1 1 0 2)) 105))
(setf ve-16 (make_ve (make_fun '(0 0 1 1 0 2)) 106))

(p-ve 0 ms-test-ve)
(p-ve 1 ms-test-ve)
(p-ve -1 ms-test-ve)
(p-ve 4 ms-test-ve)
(p-ve 0 ms-test-ve 2 4)
;-----------------------------------------------------------------------------


;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-elet=sys.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-ELET-REFERENCE.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-ms-get-xp.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-ms-set-theory-database.lisp
;____________________________________________________________________________




;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-set-theory.lisp
;____________________________________________________________________________




;**********************************************
(print "Finished loading marcolib-tests")
;**********************************************