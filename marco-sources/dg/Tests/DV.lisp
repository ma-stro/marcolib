;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DV.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS DIRECT
;     VALUES TO BE GENERATED A CERTAIN AMOUNT OF TIMES

(in-package chroma)

;-----------------------------------------------------------------------------
; GLOBALS
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	l-val		: list of values
;	lp		: loop
;	bkwd-lp		: backward loop
;	rept		: repetition
;	rept-lp		: repetition with loop at the end
;	bkwd-rept-lp	: repetition with backward loop et the end
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (l-val NEV LIST)
; 	if (length LIST) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els of LIST
; EX: (setf a '(1 2 3 4 5)), (l-val 10 a) ===> '(1 2 3 4 5 5 5 5 5 5)
(defun l-val (nev l)
  (if (<= nev (length l))
    (firstn nev l)
    (let ((last (copy-list (last l))) )
      (firstn nev
              (append l (nconc last last))))) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (lp NEV LIST)
; 	 loop along the values contained in LIST
; EX: (setf a '(1 2 3)), (lp 10 a) ===> '(1 2 3 1 2 3 1 2 3 1)
(defun lp (nev l)
    (let ((ll (copy-list l)) )
	(nconc ll ll)
	(firstn nev ll)))
;-----------------------------------------------------------------------------
; (bkwd-lp NEV LIST)
; 	loop along the values contained in LIST going backward and forward
; EX: (setf a '(1 2 3)), (bkwd-lp 10 a) ===> '(1 2 3 3 2 1 1 2 3 3)
(defun bkwd-lp (nev l)
    (let ((ll (copy-list l)) )
	(nconc ll (reverse ll))
	(nconc ll ll)
	(firstn nev ll)))
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (rept NEV VAL)
; 	repeat VAL NEV times
; EX: (rept 10 'a) ===> (a a a a a a a a a a)
(defun rept (nev val)
    (let ((result ()) )
	(loop while (>= (decf nev) 0)
	    do(newl result val))
	(nreverse result)) )
;-----------------------------------------------------------------------------
; (rept-lp NEV LIST)
; 	loop along the last values contained in LIST
;	LIST has the structure: (val1 ... valM (l1 ... lN)), loop is between
;	   l1 and lN
; EX: (setf a '(1 2 (3 4 5))), (rept-lp 10 a) ===> '(1 2 3 4 5 3 4 5 3 4)
(defun rept-lp (nev l)
    (let ((last (copy-list (car (last l))))
	  (result (firstn (1- (length l)) l)) )
	(firstn nev
		(append result (nconc last last))) ) )
;-----------------------------------------------------------------------------
; (bkwd-rept-lp NEV LIST)
; 	loop along the last values contained in LIST backward and forward
;	LIST has the structure: (val1 ... valM (l1 ... lN)), loop is between
;	   l1 and lN
; EX: (setf a '(1 2 (3 4 5))), (bkwd-rept-lp 10 a) ===> '(1 2 3 4 5 5 4 3 3 4)
(defun bkwd-rept-lp (nev l)
    (let ((last (copy-list (car (last l))))
	  (result (firstn (1- (length l)) l)) )
	(nconc last (reverse last))
	(firstn nev
		(append result (nconc last last))) ) )

;-----------------------------------------------------------------------------
