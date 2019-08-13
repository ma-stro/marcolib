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
;____________________________________________________________________________
; marcolib/marco-sources/ms-init.lisp
;____________________________________________________________________________
(get-gbl '*MSDATA*)
(get-gbl ''*MSdb*)
(get-gbl '*Llwt*)
(get-gbl '*WTdir*)

;(cl-user::decode-local-path "marcolib-tests.lisp")

;____________________________________________________________________________
; marcolib/marco-sources/back-from-om.lisp
; SAME FUNCTIONS DEFINED IN OM (utils-from-chroma.lisp)
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
; works only within a function that defines to / from
;(adjust-limits)
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
;(check-stp (make_fun '(0 0 1 1 0 3)))
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
; gives an infite list
;(build-cl 10 20)
;(build-bkwd-cl 10 20)
#|
(setf ress '(1 2 3))

(firstn 10 (nconc (nreverse ress) ress))

(defun build-cl (from to)
  (let ((cnt from)
        (result (list from)) )
    (if (< from to)
      (loop while (>= (decf to) from)
            do (newl result (incf cnt)) )
      (loop while (<= (incf to ) from)
            do (newl result (decf cnt))) )
    (nconc (setf result (nreverse result)) result)))

(defun build-bkwd-cl (from to)
  (let ((cnt from)
        (result (list from)) )
    (if (< from to)
      (loop while (>= (decf to) from)
            do (newl result (incf cnt)) )
      (loop while (<= (incf to ) from)
            do (newl result (decf cnt))) )
    (setf result (nreverse result))
    (nconc result (reverse result))
    (nconc result result)))
|#
;-----------------------------------------------------------------------------

;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-DPF.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-DV.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/dg/ms-DPV.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/dg/lkp.lisp
;____________________________________________________________________________

;____________________________________________________________________________
; marcolib/marco-sources/dg/sp-dg.lisp
;____________________________________________________________________________



;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-ELET-REFERENCE.lisp
;____________________________________________________________________________


;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-elet=sys.lisp
;____________________________________________________________________________


;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-set-theory.lisp
;____________________________________________________________________________


;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-ms-set-theory-database.lisp
;____________________________________________________________________________


;____________________________________________________________________________
; marcolib/marco-sources/vps/ms-ms-get-xp.lisp
;____________________________________________________________________________



;**********************************************
(print "Finished loading marcolib-tests")
;**********************************************