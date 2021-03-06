;*******************************************************************
;-------| CHROMA SYSTEM / PERSONAL LIBRARY
;-------| This file is: $MSsrc/my-utils.lisp
;-------| Designed and implemented in MCL by Marco Stroppa 
;-------| Version: 050306
;*******************************************************************

(in-package :cr)

;******************************************************************
; FUNCTIONS
;	build-sequence
;------------------------------------------------------------------

(defun build-sequence (from to steps &optional (ston 0.0) (exp 0.0))
"Build a sequence of <steps> steps between from and to.
Ston: aleatoric change added after computing the sequence
        (1 = 100% of the current value)
Exp: exponential factor, 0.0 = linear, > 0.0 expon, < 0.0 log
"
  (let ((seq (lkp steps (make_fun `(,from 0 ,to 1000)) exp) ))
    (loop for val in seq
          collect (clip (ran% val ston) from to))))

; (build-sequence 0 10.0 11 0.0 0.0)
;------------------------------------------------------------------

;******************************************************************
