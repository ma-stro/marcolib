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
; (vind VECT VAL [PREC])
; 	return the index of the first place = VAL+/-PREC in VECT
;	return () if nothing is found
;	PREC = precision in the computation (0.0 by default), absolute value
; EX: (setf a #(0 .5 1 2 3 4 5)), (vind a 2.1 0.3) ===> 6
(defun vind (vect val &rest prec)
  (let ((prec (ifn prec 0.0 (car prec)))
        (cnt 0)
        (l (length vect)) )
    (catch 'eofwhile
      (loop while (< cnt l)
            do (let ((vi (svref vect cnt)) )
                 (when (and (>= vi (- val prec))
                            (<= vi (+ val prec)) )
                   (throw 'eofwhile cnt))
                 (incf cnt)) )
      (throw 'eofwhile nil))))

;-----------------------------------------------------------------------------
; (vitp VECT IND) : VREF WITH FLOAT INDEX
; 	return a value with interpolation
;       if the value is not a number signal an error
; EX: (setf a #(10 11 12 13 14 15)), (vitp a 3.5) ===> 13.5
(defun vitp (vect ind)
  (let* ((ind-low (truncate ind))
         (ind-high (if (= ind-low (1- (length vect)))
                     ind-low
                     (1+ ind-low)))
         (val-low (svref vect ind-low))
         (val-high (svref vect ind-high)) )
    (if (not (and (numberp val-low) (numberp val-high)) )
      (error
       "COME ON, ~a, CAN'T INTERPOLATE IF THE VALUES AREN'T NUMBERS: ~a~%"
       (get-gbl 'USER) (cons val-low val-high))
      (if (= ind-low ind-high)
        val-low
        (+ val-low
           (* (- val-high val-low)
              (/ (- ind ind-low)
                 (- ind-high ind-low))))) ) )
  )

;-----------------------------------------------------------------------------
; (ovfl-pr VAL ALT TEST)
; 	protection against indexes beyond the limits of a vector
;	VAL = list with lower (from) or upper (to) limit (can be = ())
;	if VAL = (), choose the ALT limit
;	   else if VAL > TEST choose TEST, else return VAL
(defun ovfl-pr (val alt test)
  (cond ((null val)
         alt)
        ((numberp val)
         (if (> val test) test val))
        (t
         (if (> (car val) test)
             test
           (car val))) ))

;-----------------------------------------------------------------------------
; (adjust-limits ())		; USE DYN SCOPING
;	if upper limit > lower limit, swap them
;	return 1.0 if limits have not been swapped, -1.0 if so
;	upper and lower limits are dynamically bound to "from" and "to"
(defun adjust-limits ()
  (declare (special to from))
  (if (>= to from)
    1.0
    (let ((tmp from))
      (setf from to)
      (setf to tmp)
      -1.0)) )

;-----------------------------------------------------------------------------
; (check-stp FUN)			; USE DYN SCOPING
;	if step is negative, reject instruction
;	FUN is the calling function
(defun check-stp (fun)
  (declare (special stp))
  (when (< stp 0)
    (error "CAN'T WORK WITH NEGATIVE STEPS. CHANGE FROM/TO FIELDS INSTEAD!  ~a
CALLED BY: ~a~%"
           stp fun))
  )

;-----------------------------------------------------------------------------
; (p-set-result / pi-set-result)		; DYN SCOPING
;	prepare the final list by getting the values pointed to by a control
;	   list (cl) with test and clipping if they overflow
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

;-----------------------------------------------------------------------------
; (p-mod-set-result / pi-mod-set-result)	; DYN SCOPING
;	prepare the final list by getting the values pointed to by a control
;	   list (cl) with test and foldover if they overflow
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

;-----------------------------------------------------------------------------
; (build-cl FROM TO) / (build-bkwd-cl FROM TO)
;	prepare a control list (cl) made of integers between FROM and TO
;	bkwd: build a copy straightforward and backward
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
;-----------------------------------------------------------------------------
