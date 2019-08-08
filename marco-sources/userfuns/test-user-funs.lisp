;******************************************************************
(in-package :om)
;------------------------------------------------------------------

; THIS FILE CONTAINS SOME USEFUL METHODS USED
;    TO TEST THE GENERALIZED USER FUN

;******************************************************************


#|
BEHAVIOUR OF GEN-USER-FUN
;test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fails, the string can explains the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fails. All elements of the list will be written in the scr file.
- anything                           - the test fails

;sub-comp
a list of functions of 1 argument (component)
outputs
- a list of component or strings (the list can be empty NIL)
All elements of the list will be written in the scr file

;sub-test
a list of functions of 1 argument (component)
outputs
- a component (modified or the same) - the test is successful
- a string                           - the test fail, the string can explain the reason
- a list                             - If the car of the list is a component the test is successful else
                                     - the test fail. All elements of the list will be written in the scr file
- anything                           - the test fail

;remove-fields
a list of strings for fields that will not be written in the scr file
|#


;------------------------------------------------------------------
; LOCAL SLOTS / ONLY FOR TESTING
;------------------------------------------------------------------

(defmethod test-dur ((c component))
  "
FOR TESTING PURPOSES ONLY!!
Correct the component if its duration is beyond durtot.
(e-dels[i]+durs[i] > durtot[e]	durs[i] => durtot[e] - e-dels[i])

Return a list whose first element is NOT a component.
All the elements of the list are written, including the component
as a second element.
  "
  (let ((curr-dur (comp-field c "durs"))
        (curr-ed (comp-field c "e-dels"))
        (durtot (durtot (event c))))
    (if (<= (+ curr-ed curr-dur) durtot)
      c
      (list 
         (format () ";---> WARNING / Reduced DUR: old-dur = ~a, new-dur = ~a~% "
              curr-dur (- durtot curr-ed))
         (comp-field c "durs" (- durtot curr-ed))
         ))))


(defmethod test-dur2 ((c component))
  "
FOR TESTING PURPOSES ONLY!!
Correct the component if its duration is beyond durtot.
(e-dels[i]+durs[i] > durtot[e]	durs[i] => durtot[e] - e-dels[i])

Return a symbol. Nothing is written in the score
"
  (let ((curr-dur (comp-field c "durs"))
        (curr-ed (comp-field c "e-dels"))
        (durtot (durtot (event c))))
    (if (<= (+ curr-ed curr-dur) durtot)
      c
      'hello-world
       )))



(defmethod fq-sr1? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => Invert sign of freq and print it as is
"
(declare (special cr::sr/2 index))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "freq") sr2)
     (list
      (format () ";*****fq-sr1? - ERROR: FQ > Nyquist: ~a~%;    Inverted freq's sign of component ~a~%"
              (comp-field c "freq") (index c))
      (progn (comp-field c "freq" (* -1.0 (comp-field c "freq")))
             "CHANGED SIGN TO FREQ")
      c) ; TEST FOR TESTING: RETURN AN ERROR AND THE NEGATIVE FREQUENCY (MUST RETURN THE WHOLE COMPONENT)
       c)))


(defmethod fq-sr2? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => test successful (car of the list is a component), component printed
"
(declare (special cr::sr/2 INDEX))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "freq") sr2)
     (list
      (comp-field c "freq" (* -1.0 (comp-field c "freq")))
      (format () ";*****fq-sr2? - ERROR: FQ > Nyquist: ~a~%;    Component n. ~a discarded~%"
              (comp-field c "freq") (index c))
;      (comp-field c "freq" (* -1.0 (comp-field c "freq")))
      ) ; HERE ONLY PRINT THE ERROR MESSAGE, BUT DO NOT RETURN THE COMPONENT
       c)))

; IF THE FIRST ELEMENT OF THE LIST IS A COMPONENT, WHICH IS WHAT IS RETURNED BY COMP-FIELD
; THE TEST IS SUCCESSFUL AND THE COMPONENT IS PRINTED IN THE SCORE WITH THE FOLLOWING COMMENT


(defmethod fq-sr3? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => test failed, component printed (because it is in the list)
"
(declare (special cr::sr/2 INDEX))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "freq") sr2)
     (list
;      (comp-field c "freq" (* -1.0 (comp-field c "freq")))
      (format () ";*****fq-sr3? - ERROR: FQ > Nyquist: ~a~%;    Component n. ~a NOT discarded~%"
              (comp-field c "freq") (index c))
      (comp-field c "freq" (* -1.0 (comp-field c "freq")))
      ) ; HERE ONLY PRINT THE ERROR MESSAGE, BUT DO NOT RETURN THE COMPONENT
       c)))

; THE TEST FAILS, BUT THE COMPONENT IS STILL WRITTEN IN THE SCORE BECAUSE IT IS IN THE RETURNED LIST



(defmethod fq-sr4? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => test failed, component printed (because it is in the list)
"
(declare (special cr::sr/2 INDEX))
   (let ((sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "freq") sr2)
     (list
;      (comp-field c "freq" (* -1.0 (comp-field c "freq")))
      (format () ";*****fq-sr4? - ERROR: FQ > Nyquist: ~a~%;    Component n. ~a discarded~%"
              (comp-field c "freq") (index c))
;      (comp-field c "freq" (* -1.0 (comp-field c "freq")))
      ) ; HERE ONLY PRINT THE ERROR MESSAGE, BUT DO NOT RETURN THE COMPONENT
       c)))

; THE TEST FAILS, AND THE COMPONENT IS NOT WRITTEN IN THE SCORE (JUST THE ERROR MESSAGE)




(defmethod fq-sr5? ((c component))
"
FOR TESTING PURPOSES ONLY
fq[i] > SR/2 => discard component
"
(declare (special cr::sr/2 index)
         )		; modified in the gen-user-fun  
   (let ((index (index c))
         (sr2 (if (find-package 'cr)
                  (cr::get-gbl 'cr::sr/2)
                  22050)))
   (if (> (comp-field c "freq") sr2)
     'fq-sr3-HELLO-WORLD 
       c)))

; TEST FOR TESTING: RETURN A SYMBOL (ANYTHING) -> DOES NOTHING, TEST FAILS, NOTHING IS PRINTED

#|
not (0 <= bal[i] <= 1)	set to 0 or 1 / DONE WITHIN THE ORCHESTRA FILE
	
atk[i] / dec[i] < 0	set to 0 / DONE WITHIN THE ORCHESTRA FILE
	
not (0 <= jtv[i] <= 1)	set to 0 or 1 / DONE WITHIN THE ORCHESTRA FILE
	
nosc[i] < 0	nosc[i] = 0 / NOT NEEDED
|#

;******************************************************************
