;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/sp-dg.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS SPECIAL-PURPOSE FUNCTIONS TO USE WITH DG SYS

(in-package chroma)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	chk-dur
;	p-ve
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (chk-dur DUR AT GBL-DUR)
;	check the DURation of an event starting at time AT, and clip it with
;	GBL-DUR
(defun chk-dur (dur at gbl-dur)
    (if (> (+ at dur) gbl-dur)
	(- gbl-dur at)
	dur) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-ve VAL DB [FROM TO])
;	extract 1 item from a data-base of virtual envelopes (type ve)
;       argument x is FROM / TO fields of p-l-val
; EX: (setf db #(f-11 f-12 f-13))
;      (p-ve 1 db) ===> (eval f-11)
(defun p-ve (val db &rest x)
    (eval
	(car
	    (apply 'p-l-val (append (list 1 (list (1- val)) db) x)) )) )
;-----------------------------------------------------------------------------
