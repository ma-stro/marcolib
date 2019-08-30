;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/gen2fun.lisp
;-------| Version V1.0: Aug 7, 2019
;-------| By Marco Stroppa
;-------| Copyright 2019 IRCAM
;*****************************************************************************
(in-package :cr)

; THIS FILE PROVIDES A CONVERSION FROM ABSOLUTE CSOUND GEN FUNCTIONS
;   INTO A CHROMA FUN OBJECT.

; GEN FUNCTIONS SHOULD BE STRINGS OF LISTS (WITH THE EXACT SYNTAX AS IN CSOUND)



;-------------------------------------------------------------------
(defun csgen->fun (csgen)
  (let ((str (string-to-list csgen)))
    (if (or (= (fourth str) 7) (= (fourth str) 5))
        (make-crfun (fifth str) (cdr (cddddr str)) (third str))
    (progn (print (format () "I can only have GEN 5 or 7. You gave me a GEN ~a~%" (fourth str)))
      str))))

;-------------------------------------------------------------------
(defun make-crfun (begval othervals gensize)
  (let ((l-vals (list begval 0.0))
        (numels (1- (length othervals)))
        (cnt 0)
        (result ()))
    (make_fun
     (append l-vals
             (loop for i from 0 to numels by 2 do
                   append (list (nth (1+ i) othervals) (setf cnt (+ cnt (nth i othervals)))))))))
;
#|
(streamp "f11 0 513 7  -1 513 0")
(stringp "f11 0 513 7  -1 513 0")
(listp '(1 2 3))
(consp "f11 0 513 7  -1 513 0")
(make-string-input-stream "f11 0 513 7  -1 513 0")
(defun test-gen2fun ()
  '(
   "f10 0 4097 7  -1 2048 0 2048 1"
   "f11 0 513 7  -1 256 0 256 1"
   "f12 0 513 7   1 512 0"
   "f13 0 513 7   0 512 1"
   "f14 0 513 5   0 512 1"
   "f15 0 513 1   0 512 1"
   "f16 0 513 7   0 512 -1") )
(csgen->fun (nth 0 (test-gen2fun))  )
(first (string-to-list "f11 0 7 513 0 2 3 4 5"))
(fun->gen-cs-table (make_fun '(-1 0  0 2048  1 4097)) )
(fun-points (make_fun '(-1 0  0 2048  1 4096)) 4097)
|#

;-------------------------------------------------------------------
; if str is a string, convert it to a list, otherwise do nothing
(defun string-to-list (str)
  (cond ((consp str)
         str)
        ((stringp str)
         (internal-string-to-list str))
        (t (error "Need a string, please: ~a:" str))))

(defun internal-string-to-list (str)
         (if (not (streamp str))
             (internal-string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (internal-string-to-list str))
;*********************************************************************