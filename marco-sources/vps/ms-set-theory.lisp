;******************************************************************
;-------| VPS SYSTEM
;-------| This file is: $LLvps/set-theory.lisp
;-------| By Marco Stroppa
;-------| Version 1.0, September 1999
;-------| Copyright 1999 IRCAM
;******************************************************************

(in-package chroma)


;;;;;;;;;;;;;; SET THEORY

;;;;;;;;;;;;;; GENERAL METHODS (applied to VPS's)

;REDUCE-TO-PITCH-CLASS
; (reduce-to-pitch-class spl)
; Returns another SPL in the format of a pitch class (see Set Theory).
; Operations: remove octaves, transpose all the pitches within one octave,
;                reorder and start from DO4 (= 0)
(defmethod reduce-to-pitch-class ((x spl))
;(om::defmethod! reduce-to-pitch-class ((x spl))
;  :icon 130
  (let* ((midi (pch->midi (spl_vps (remove-octaves (make_spl (spl_vps x t))))))
         (first (first midi))
         (beg-at-0 (mapcar (lambda (x) (- x first)) midi))
         (sorted (sort (mapcar (lambda (x) (mod x 12)) beg-at-0) #'<)))
    (make_vps (pch-class->pch sorted))))

(defmethod reduce-to-pitch-class ((x t))
;(om::defmethod! reduce-to-pitch-class ((x t))
  (error "YOU THINK IT'S REALLY USEFUL THAT I LEARN TO REDUCE TO A PITCH CLASS
    THIS: ~a ?~%" x))
;__________________________________________________________________

;PRIME-FORM-SET
; (prime-form-set spl)
; Returns the prime form of its argument as another SPL.
; A prime form tends to begin with the smallest intervals.
; If a prime form is not found in the list of sets, then it is an inversion
;    of a prime form.
(defmethod prime-form-set ((x spl))
;(om::defmethod! prime-form-set ((x spl))
;  :icon 130
  (let* ((pch-cls (pitch-class x)))
    (make_vps
     (pch-class->pch
      (start-at-zero
       (find-good-set
        (find-good-sets (build-matrix-list pch-cls))))))))

;---------------------------------------------------------------------------------
; LOCAL METHODS
; build a list of lists containing all the possible forms of a set
; Ex: if list = (0 1 2 3 4),
;  Result: ((0 1 2 3 4) (1 2 3 4 12) (2 3 4 12 13) (3 4 12 13 14) (4 12 13 14 15))
(defmethod build-matrix-list ((l list))
  (let ((list l))
    (loop while l
          collect list
          do (setf list (append (cdr list) (list (+ (pop l) 12))))
          )))

; Check the difference between the first and the last pitch class of a list of sets
;    and return the smallest one (one or several).
(defmethod find-good-sets ((sets list))
  (let* ((differences (mapcar (lambda (l) (- (car (last l)) (first l))) sets))
         (min (apply #'min differences)))
      (loop for set in sets
              when (= (- (car (last set)) (car set)) min)
                collect set)))

; Receives a list of at least one good set and returns the one in which the
;    difference between the first and second pitch class is smallest or try
;    with the following pitch-classes (difference between 2nd and 3rd, and
;    so on).
(defmethod find-good-set ((sets list))
  (let ((diff 12) ; a large difference will trigger the first collection
        (curr-diff ())
        (result ()))
                            ; if the set has at least two elements
    (if (cdar sets)         ;    look for the difference
      (loop for set in sets
            do (setf curr-diff (- (cadr set) (car set)))
            if (< curr-diff diff)
              do (setf diff curr-diff)
              and do (setf result set)
            else if (= curr-diff diff)
              do (setf result
                      (find-subset
                       (find-good-set (mapcar #'cdr sets))
                       sets)))
                                 ; otherwise the two sets are the same
      (setf result (car sets)))  ;   and return the first one
    result))

; Return the set in the list of sets whose cdr is equal to the argument
(defmethod find-subset ((set list) (sets list))
  (loop for el in sets
        when (equal set (cdr el))
          return el))

; Take any set (list of numbers) and return it starting from 0
(defmethod start-at-zero ((set list))
  (let* ((beg (car set)))
    (mapcar (lambda (x) (- x beg)) set)))
;__________________________________________________________________

;INVERT-SET
; (invert-set spl)
; Returns the prime form of the inversion of the prime form of its argument

(defmethod invert-set ((x spl))
;(om::defmethod! invert-set ((x spl))
;  :icon 130
  (let* ((set (pc-set x))
         (inv-set (inv-set set)))
    (prime-form-set
     (make_vps (pch-class->pch inv-set)))))

(defmethod invert-set ((x t))
;(om::defmethod! invert-set ((x t))
  (error "CAN'T INVERT THIS SET, SIR, SORRY, SIR: ~a~%" x))
;------------------------------------------------------------------------------
; LOCAL METHODS
; Invert a set and reduce it to its prime form
; Ex: if set = (0 1 2 3 6 8 9), inversion = (0 1 2 3 6 7 9)

(defmethod inv-set ((set list))
  (let ((inv (mapcar (lambda (el) (- 12 el)) (reverse set))))
    (start-at-zero inv)))

;__________________________________________________________________

;COMPLEMENT-SET
; (complement-set spl)
; Returns the prime form of the complement of the prime form of its argument
; The complement may itself be the inversion of the prime form found in the
;     table with the Pitch-Class Sets.

(defmethod complement-set ((x spl))
;(om::defmethod! complement-set ((x spl))
;  :icon 130
  (let* ((set (pc-set x))
         (complement (comp-set set)))
    (prime-form-set
     (make_vps (pch-class->pch complement)))))

(defmethod complement-set ((x t))
;(om::defmethod! complement-set ((x t))
  (error "CAN'T FIND THE COMPLEMENT OF THIS SET, SIR: ~a~%
    SORRY, SIR" x))
;------------------------------------------------------------------------------
; LOCAL METHODS
; Find the complement of a set and reduce it to its prime form
; Ex: if set = (0 1 2 3 6 8 9), complement = (4 5 7 10 11)
;                               prime form = (0 1 3 6 7)

(defmethod comp-set ((set list))
  (let ((inv
         (loop for i from 0 to 11
               unless (member i set)
                 collect i)))
    (start-at-zero inv)))

;__________________________________________________________________

;;;;;;;;;;;;;; DOCUMENTATION


;GET-NAME
; (get-name prime-form)
; Returns a list containing the name and vector of "prime-form"

(defmethod get-name ((vps vps))
  (declare (special *SET-NAMES*))
  (let* ((form (pc-set vps)) ; IF FORM IS NIL, FIND THE INVERSION
         (i-form (ipc-set vps))
         (found (assoc form *SET-NAMES* :test 'equal)))
    (ifn found
         (assoc i-form *SET-NAMES* :test 'equal)
      found)))

(defmethod get-name ((vps list))
; argument is a list of pitch classes (existing as a prime form)
  (declare (special *SET-NAMES*))
  (assoc vps *SET-NAMES* :test 'equal))


;PRINT-NAME
; (print-name prime-form)
; Verbose version of GET-NAME

(defmethod print-name (list)
  (let ((name (get-name list)))
    (let ((pch-class (first name))
          (prime-form (second name))
          (vector (third name))
          (C-pch-class (fourth name))
          (C-prime-form (fifth name))
          (C-vector (sixth name)))
      (if (member (class-name (class-of list))
                  '(spl rpl cil ail fql crl arl) :test 'equal)
           (format t "~%SET THEORY~%ANALYSIS OF ~a~%" (the-list list))
           (format t "~%SET THEORY~%ANALYSIS OF ~a~%" pch-class))
      (format t "~%   PRIME FORM
         NAME: ~a
         PITCHES: ~a
         VECTOR: ~a~%"
              prime-form pch-class vector)
      (if C-pch-class
        (format t "~%   COMPLEMENT
         NAME: ~a
         PITCHES: ~a
         VECTOR: ~a~%"
              C-prime-form C-pch-class C-vector)
        (format t "~%   NO COMPLEMENT~%")))))

         
;__________________________________________________________________

;;;;;;;;;;;;;; UFM SELECTORS

;PITCH-CLASS
; (pitch-class vps)
;  Return the list of pitch classes in a numeric format starting from 0
;  VPS = if not chromatic SPL, convert
;        the deviation will not be taken into account
;NB: the pitches will contain no octaves and will be sorted

(defmethod pitch-class ((vps spl))
;(om::defmethod! pitch-class ((vps spl))
;  :icon 130
  (pch->pch-class (spl_vps (reduce-to-pitch-class vps))))

(defmethod pitch-class ((vps rpl))
;(om::defmethod! pitch-class ((vps rpl))
  (pitch-class (make_vps (spl_vps vps 4))))

(defmethod pitch-class ((vps cil))
;(om::defmethod! pitch-class ((vps cil))
  (pitch-class (make_vps (spl_vps vps "DO4" t))))

(defmethod pitch-class ((vps ail))
;(om::defmethod! pitch-class ((vps ail))
  (pitch-class (make_vps (spl_vps vps t))))

(defmethod pitch-class ((vps fql))
;(om::defmethod! pitch-class ((vps fql))
  (pitch-class (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod pitch-class ((vps crl))
;(om::defmethod! pitch-class ((vps crl))
  (pitch-class (make_vps (spl_vps vps "DO4" t))))

(defmethod pitch-class ((vps arl))
;(om::defmethod! pitch-class ((vps arl))
  (pitch-class (make_vps (spl_vps vps t))))

(defmethod pitch-class ((vps t))
;(om::defmethod! pitch-class ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________


;PCH-CLASS
; (pch-class vps)
;  After computing the list of pitches in a numeric format, convert it into
;     an RPL
;  VPS = if not chromatic SPL, convert
;        the deviation will not be taken into account
;NB: the pitches will start at DO

(defmethod pch-class ((vps spl))
;(om::defmethod! pch-class ((vps spl))
;  :icon 130
  (rpl_vps (make_vps (pch-class->pch (pitch-class vps) t))))

(defmethod pch-class ((vps rpl))
;(om::defmethod! pch-class ((vps rpl))
  (pch-class (make_vps (spl_vps vps 4))))

(defmethod pch-class ((vps cil))
;(om::defmethod! pch-class ((vps cil))
  (pch-class (make_vps (spl_vps vps "DO4" t))))

(defmethod pch-class ((vps ail))
;(om::defmethod! pch-class ((vps ail))
  (pch-class (make_vps (spl_vps vps t))))

(defmethod pch-class ((vps fql))
;(om::defmethod! pch-class ((vps fql))
  (pch-class (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod pch-class ((vps crl))
;(om::defmethod! pch-class ((vps crl))
  (pch-class (make_vps (spl_vps vps "DO4" t))))

(defmethod pch-class ((vps arl))
;(om::defmethod! pch-class ((vps arl))
  (pch-class (make_vps (spl_vps vps t))))

(defmethod pch-class ((vps t))
;(om::defmethod! pch-class ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________


;PC-SET
; (pc-set vps)
; Return the pitch list of the prime form in a numeric format starting at 0

(defmethod pc-set ((vps spl))
;(om::defmethod! pc-set ((vps spl))
;  :icon 130
  (pch->pch-class (spl_vps (prime-form-set vps) t)))

(defmethod pc-set ((vps rpl))
;(om::defmethod! pc-set ((vps rpl))
  (pc-set (make_vps (spl_vps vps 4))))

(defmethod pc-set ((vps cil))
;(om::defmethod! pc-set ((vps cil))
  (pc-set (make_vps (spl_vps vps "DO4" t))))

(defmethod pc-set ((vps ail))
;(om::defmethod! pc-set ((vps ail))
  (pc-set (make_vps (spl_vps vps t))))

(defmethod pc-set ((vps fql))
;(om::defmethod! pc-set ((vps fql))
  (pc-set (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod pc-set ((vps crl))
;(om::defmethod! pc-set ((vps crl))
  (pc-set (make_vps (spl_vps vps "DO4" t))))

(defmethod pc-set ((vps arl))
;(om::defmethod! pc-set ((vps arl))
  (pc-set (make_vps (spl_vps vps t))))

(defmethod pc-set ((vps t))
;(om::defmethod! pc-set ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________


;PCH-SET
; (pch-set vps)
; Return the pitch list of the prime form as an RPL

(defmethod pch-set ((vps spl))
;(om::defmethod! pch-set ((vps spl))
;  :icon 130
  (rpl_vps (prime-form-set vps) t))

(defmethod pch-set ((vps rpl))
;(om::defmethod! pch-set ((vps rpl))
  (pch-set (make_vps (spl_vps vps 4))))

(defmethod pch-set ((vps cil))
;(om::defmethod! pch-set ((vps cil))
  (pch-set (make_vps (spl_vps vps "DO4" t))))

(defmethod pch-set ((vps ail))
;(om::defmethod! pch-set ((vps ail))
  (pch-set (make_vps (spl_vps vps t))))

(defmethod pch-set ((vps fql))
;(om::defmethod! pch-set ((vps fql))
  (pch-set (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod pch-set ((vps crl))
;(om::defmethod! pch-set ((vps crl))
  (pch-set (make_vps (spl_vps vps "DO4" t))))

(defmethod pch-set ((vps arl))
;(om::defmethod! pch-set ((vps arl))
  (pch-set (make_vps (spl_vps vps t))))

(defmethod pch-set ((vps t))
;(om::defmethod! pch-set ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________

;IPC-SET
; (ipc-set vps)
; Return the pitch list of the inversion of the prime form
;    in a numeric format starting at 0

(defmethod ipc-set ((vps spl))
;(om::defmethod! ipc-set ((vps spl))
;  :icon 130
  (pch->pch-class (spl_vps (invert-set vps) t)))

(defmethod ipc-set ((vps rpl))
;(om::defmethod! ipc-set ((vps rpl))
  (ipc-set (make_vps (spl_vps vps 4))))

(defmethod ipc-set ((vps cil))
;(om::defmethod! ipc-set ((vps cil))
  (ipc-set (make_vps (spl_vps vps "DO4" t))))

(defmethod ipc-set ((vps ail))
;(om::defmethod! ipc-set ((vps ail))
  (ipc-set (make_vps (spl_vps vps t))))

(defmethod ipc-set ((vps fql))
;(om::defmethod! ipc-set ((vps fql))
  (ipc-set (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod ipc-set ((vps crl))
;(om::defmethod! ipc-set ((vps crl))
  (ipc-set (make_vps (spl_vps vps "DO4" t))))

(defmethod ipc-set ((vps arl))
;(om::defmethod! ipc-set ((vps arl))
  (ipc-set (make_vps (spl_vps vps t))))

(defmethod ipc-set ((vps t))
;(om::defmethod! ipc-set ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________


;IPCH-SET
; (ipch-set vps)
; Return the pitch list of the inversion of the prime form as an RPL

(defmethod ipch-set ((vps spl))
;(om::defmethod! ipch-set ((vps spl))
;  :icon 130
  (rpl_vps (invert-set vps) t))

(defmethod ipch-set ((vps rpl))
;(om::defmethod! ipch-set ((vps rpl))
  (ipch-set (make_vps (spl_vps vps 4))))

(defmethod ipch-set ((vps cil))
;(om::defmethod! ipch-set ((vps cil))
  (ipch-set (make_vps (spl_vps vps "DO4" t))))

(defmethod ipch-set ((vps ail))
;(om::defmethod! ipch-set ((vps ail))
  (ipch-set (make_vps (spl_vps vps t))))

(defmethod ipch-set ((vps fql))
;(om::defmethod! ipch-set ((vps fql))
  (ipch-set (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod ipch-set ((vps crl))
;(om::defmethod! ipch-set ((vps crl))
  (ipch-set (make_vps (spl_vps vps "DO4" t))))

(defmethod ipch-set ((vps arl))
;(om::defmethod! ipch-set ((vps arl))
  (ipch-set (make_vps (spl_vps vps t))))

(defmethod ipch-set ((vps t))
;(om::defmethod! ipch-set ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________
;C-PC-SET
; (c-pc-set vps)
; Return the pitch list of the inversion of the prime form
;    in a numeric format starting at 0

(defmethod c-pc-set ((vps spl))
;(om::defmethod! c-pc-set ((vps spl))
;  :icon 130
  (pch->pch-class (spl_vps (complement-set vps) t)))

(defmethod c-pc-set ((vps rpl))
;(om::defmethod! c-pc-set ((vps rpl))
  (c-pc-set (make_vps (spl_vps vps 4))))

(defmethod c-pc-set ((vps cil))
;(om::defmethod! c-pc-set ((vps cil))
  (c-pc-set (make_vps (spl_vps vps "DO4" t))))

(defmethod c-pc-set ((vps ail))
;(om::defmethod! c-pc-set ((vps ail))
  (c-pc-set (make_vps (spl_vps vps t))))

(defmethod c-pc-set ((vps fql))
;(om::defmethod! c-pc-set ((vps fql))
  (c-pc-set (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod c-pc-set ((vps crl))
;(om::defmethod! c-pc-set ((vps crl))
  (c-pc-set (make_vps (spl_vps vps "DO4" t))))

(defmethod c-pc-set ((vps arl))
;(om::defmethod! c-pc-set ((vps arl))
  (c-pc-set (make_vps (spl_vps vps t))))

(defmethod c-pc-set ((vps t))
;(om::defmethod! c-pc-set ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________


;C-PCH-SET
; (c-pch-set vps)
; Return the pitch list of the inversion of the prime form as an RPL

(defmethod c-pch-set ((vps spl))
;(om::defmethod! c-pch-set ((vps spl))
;  :icon 130
  (rpl_vps (complement-set vps) t))

(defmethod c-pch-set ((vps rpl))
;(om::defmethod! c-pch-set ((vps rpl))
  (c-pch-set (make_vps (spl_vps vps 4))))

(defmethod c-pch-set ((vps cil))
;(om::defmethod! c-pch-set ((vps cil))
  (c-pch-set (make_vps (spl_vps vps "DO4" t))))

(defmethod c-pch-set ((vps ail))
;(om::defmethod! c-pch-set ((vps ail))
  (c-pch-set (make_vps (spl_vps vps t))))

(defmethod c-pch-set ((vps fql))
;(om::defmethod! c-pch-set ((vps fql))
  (c-pch-set (make_vps (spl_vps vps (length (fql_vps vps)) t))))

(defmethod c-pch-set ((vps crl))
;(om::defmethod! c-pch-set ((vps crl))
  (c-pch-set (make_vps (spl_vps vps "DO4" t))))

(defmethod c-pch-set ((vps arl))
;(om::defmethod! c-pch-set ((vps arl))
  (c-pch-set (make_vps (spl_vps vps t))))

(defmethod c-pch-set ((vps t))
;(om::defmethod! c-pch-set ((vps t))
  (error "TRIED TO DO MY BEST, SIR,~%   BUT REALLY COULDN'T COPE WITH: ~a~%" vps)
  )
;__________________________________________________________________
