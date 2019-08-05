;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DPF.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILE CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS POINTERS
;    TO A DATA BASE

(in-package chroma)

;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	p-lkp / pi-lkp		: linear interpolation (fit to db)
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-lkp / pi-lkp NEV CFUN DB [FROM TO])
; 	sample CFUN NEV equally spaced times and use result as a ptr into DB
(defun p-lkp (nev cfun db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (cl ())
 ;        (result ())
         (fun (copy-list cfun)) )
	(x-resc_fun fun 0 (1- nev))
	(y-resc_fun fun from to)
	(let ((step 1)
	      (acc 0) )
          (loop while (< acc nev)
            do (newl cl (y-val_fun fun acc))
            do (incf acc step)) )
	(p-l-val nev (nreverse cl) db from to)) )

;-----------------------------------------------------------------------------
(defun pi-lkp (nev cfun db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (cl ())
;         (result ())
         (fun (copy-list cfun)) )
	(x-resc_fun fun 0 (1- nev))
	(y-resc_fun fun from to)
	(let ((step 1)
	      (acc 0) )
          (loop while (< acc nev)
            do (newl cl (y-val_fun fun acc))
            do (incf acc step)) )
	(pi-l-val nev (nreverse cl) db from to)) )
;-----------------------------------------------------------------------------
