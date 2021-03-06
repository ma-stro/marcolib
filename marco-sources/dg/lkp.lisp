;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/lkp.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS FUNCTIONS TO GENERATE OTHER CONTROL FUNCTIONS (TYPE FUN)
;	IN AN EFFICIENT AND CATEGORICAL WAY

(in-package chroma)
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
(defun make-lkp3a (y-st mdl y-end &rest nothing)
  (when nothing)	; USELESS LINE (ELIMINATES WARNING WHEN LOADING)
  (let ((y-mdl (car mdl))
        (x-mdl (cadr mdl)) )
    (make_fun `(,y-st 0.0   ,y-mdl ,x-mdl   ,y-end 1.0)) ))

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
(defun make-lkp3b (y-st mid y-end &rest ctl)
    (let ((y-mid (car mid))
	  (x-mid (cadr mid))
	  (y1 (ifn ctl 0.75 (ifn (car ctl) 0.75 (caar ctl))) )
	  (y2 (ifn ctl 0.75 (ifn (car ctl) 0.75 (cdar ctl))) )
	  (x1 (ifn ctl 0.3 (ifn (cadr ctl) 0.3 (caadr ctl))) )
	  (x2 (ifn ctl 0.3 (ifn (cadr ctl) 0.3 (cdadr ctl))) ) )

	(let* ((x1a (* x-mid x1) )
	       (x1b (+ (/ (- x-mid x1a) 2) x1a) )
	       (x2b (- 1.0 (* (- 1.0 x-mid) x2)) )
	       (x2a (+ (/ (- x2b x-mid) 2) x-mid) )

	       (y1a (+ (* (- y-mid y-st) y1) y-st) )
	       (y1b (+ (* (- y-mid y-st) (- 1 y1)) y-st) )
	       (y2a (+ (* (- y-end y-mid) y2) y-mid) )
	       (y2b (+ (* (- y-end y-mid) (- 1 y2)) y-mid) ) )

	    (make_fun `(,y-st 0.0
			,y1a ,x1a
			,y1b ,x1b
			,y-mid ,x-mid
			,y2a ,x2a
			,y2b ,x2b
			,y-end 1.0) )
   )) )
;-----------------------------------------------------------------------------
