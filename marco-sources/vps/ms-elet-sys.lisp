;******************************************************************
;-------| ELET SYSTEM
;-------| This file is: $VPS/elet-sys.lisp
;-------| Version: Nov 22, 1999
;-------| Ported into MCL by Marco Stroppa
;******************************************************************
;	E L E T   P A C K A G E
;******************************************************************
;	GENERATION ALONE
;------------------------------------------------------------------

(in-package :cr)

;-----------------------------------------------------------------------------
; MAIN VARIABLE WITH ALL THE INTERVALLIC POSSIBILITIES FOR ELET

(defvar *ELET-INT-FIELD* '(
		(2 1 2)
		(3 3 4)
		(4 5 6)
		(5 6 7)
		(6 8 9)
		(7 10 11)

		(9 13 14)
		(10 15 16)
		(11 17 18)
		(12 18 19)
		(13 20 21)
		(14 22 23)))

;-----------------------------------------------------------------------------

; MAIN FUNCTION PERFORMING THE EXHAUSTIVE SEARCH

; SUPER THANKS TO CARLOS AGON FOR THIS EXCELLENT SOLUTION....

(defun elet (chord choices)
"Exhaustive search of the first argument according to the choices of the second"
  (let ((domaines (cons-dom chord choices)))
    (combine-domaines domaines)))

(defun cons-dom (chord choices)
  (let ((result nil))
    (loop for el in chord do
          (loop for note in choices do
                (if (= (car note) el)
                  (push (cdr note) result))))
  (reverse result)))

(defun combine-domaines (dom)
  (cond ((null dom) nil)
        ((null (cdr dom)) (car dom))
        (t  (loop for a in (car dom)
            append
            (let ((other-domaines (combine-domaines (cdr dom))))
              (loop for item in other-domaines
                    collect (cons a (if (listp item)
                                  item (list item)))))))))

; (elet '(1 2 3) '((1 1 2) (2 3 4) (3 5 6)))
;-> ((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6))

;-----------------------------------------------------------------------------
; GENERATION WITH OCTAVE REMOVAL
; (generate-elet '(2 3 4 2 5 2 6) 'DO4 *ELET-INT-FIELD*)
; Return a list of good SPL's

(defun generate-elet (input-is beg-pch
                              &optional (itvl-field *ELET-INT-FIELD*))

"Returns a list of acceptable SPL's starting at beg-pch"
  (unless (pitch-with-octave-p beg-pch)
  (error "SORRY, ~a, I KNOW I'M VERY DUMB, BUT IF YOU DUNNO GIVE ME A
PITCH WITH OCTAVE, HOW SHALL I KNOW WHERE TO START FROM?~%
WHAT THE HELL IS THIS:  ~a ????"
         (getenv 'USER) beg-pch))

  (let* ((itvl-solutions (mapcar 'semitones->itvl
                                 (elet input-is itvl-field)))
         (symb-solutions (mapcar (lambda (x) (itvl->pch x beg-pch))
                                 itvl-solutions))
         (final-solutions
          (mapcar 'make_vps symb-solutions)))

    (loop for spl in final-solutions
          unless (octave-p_vps spl) collect spl))
  )


;-----------------------------------------------------------------------------
; GENERATION WITH OCTAVE REMOVAL AND PRINTING IN FILE

; (show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4 [*ELET-INT-FIELD*])
;            [:file filename])
; (show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4))
; (show-elet (generate-elet '(2 3 4 2 5 2 6) 'DO4
;     '((2 1 2) (3 3) (4 4) (5 5) (6 6))))


(defun show-elet (vps-list
                 &key (file "*Listener*") (approx t)
                 (anal-flag nil))

"Print on the listener or on a file in directoru ELET a list of pitches
usually coming from generate-elet"

  ;anal-flag: internal keyword to optimize the choice between printing only
  ;              and printing with analysis

  (declare (special ELET))
  
  (let ((ofile (if (string-equal file "*Listener*")
                 t
                 (open
                  (make-pathname :directory (namestring (getenv ELET))
                                 :name file)
                  :direction :output
                  :if-does-not-exist :create
                  :if-exists :supersede))))

    (let ((namefile (if (streamp ofile)
                      (namestring (pathname ofile))
                      "Listener")))

      (when (streamp ofile)
        (format t "~%WRITING ~d SOLUTION(S)~%     ON ~a~%"
                (length vps-list) namefile))
      (format ofile "OPENING ~a~%     ON ~a" namefile (stringdate))

      (format ofile "~%   ~D SOLUTION(S)~%" (length vps-list))

      (print-vps-elet vps-list ofile approx anal-flag)

      (when (streamp ofile)
            (format ofile "~%CLOSING ~a~%     ON ~a" namefile (stringdate))
            (close ofile))
      (format t "CLOSING ~a~%" namefile)

      )))


(defun print-vps-elet (vps-list ofile approx anal-flag)
  (let ((cnt -1))
    (if anal-flag
      (loop for spl in vps-list   ; VERSION WITH ANALYSIS

            do
            (format ofile "~%    ~3D: ~a~%" (incf cnt) (spl_vps spl approx))
            (format ofile "          S  = ~a~%" (surf_vps spl))
            (format ofile "          D  = ~5F~%"(dens_vps spl))
            (format ofile "          H  = ~a [~a semitones] made of ~a~%"
                    (hom_vps spl) (hom-s_vps spl) (hom-e_vps spl))
            (format ofile "          SD = ~5F~%" (sd_vps spl))
            (format ofile "          CS = ~5F~%" (cs_vps spl))
            )

      (loop for spl in vps-list
            do (format ofile "    ~3D: ~a~%" (incf cnt) (spl_vps spl approx))
            ))))
;-----------------------------------------------------------------------------
; GENERATION WITH OCTAVE REMOVAL, PRINTING AND ANALYSIS

; (anal-elet '(2 3 4 2 5 2 6) 'DO4 [*ELET-INT-FIELD*] [:file filename])
; (anal-elet '(2 3 4 2 5 2 6) 'DO4)
; (anal-elet '(2 3 4 2 5 2 6) 'DO4 :choices *ELET-INT-FIELD*)
; (show-elet '(2 3 4 2 5 2 6) 'DO4 :choices '((2 1 2) (3 3) (4 4) (5 5) (6 6)))
; (show-elet '(2 3 4 2 5 2 6) 'DO4 :approx 50.0)
; (show-elet '(2 3 4 2 5 2 6) 'DO4 :file "pippo.vps")
; (show-elet '(2 3 4 2 5 2 6) 'DO4 :file "pippo.vps" :approx 50.0)
; (show-elet '(2 3 4 2 5 2 6) 'DO4 :file "pippo.vps" :choices '((2 1 2) (3 3) (4 4) (5 5) (6 6)))

; Print on the listener or on a file in directoru ELET a list of pitches
; corresponding to possible solutions and starting with beg-pch

(defun anal-elet (vps-list
                 &key (file "*Listener*") (approx t))

"Print on the listener or on a file in directoru ELET a list of pitches
usually coming from generate-elet and some analysis data"

  ;anal-flag: internal keyword to optimize the choice between printing only
  ;              and printing with analysis

  (show-elet vps-list :file file :approx approx :anal-flag t))

;-----------------------------------------------------------------------------
; GENERATION OF A DATA BASE OF SEVERAL FILES

; (run-show-elet list-of-matches pitch [:itvl-field] [:approx])

; (run-show-elet '((2 3 4 2 5 2 6)
;                  (2 3 4 2 5 2 6))'DO4)

(defun run-show-elet (input-is-list pitch
                     &key (itvl-field *ELET-INT-FIELD*) (approx t)
                     (anal-flag nil))

"Generates a data base of files containing the VPS's satisfying the constraints"

  ;anal-flag: internal keyword to optimize the choice between printing only
  ;              and printing with analysis

  (mapcar (lambda (vps)
            (show-elet (generate-elet vps pitch itvl-field)
                       :file (make-filename-dispatch vps pitch)
                       :approx approx :anal-flag anal-flag)) input-is-list))

(defun run-anal-elet (input-is-list pitch
                     &key (itvl-field *ELET-INT-FIELD*) (approx t)
                     (anal-flag nil))

"Generates a data base of files containing the VPS's satisfying the constraints
and some analytical data"
   (declare (ignore anal-flag))
   (run-show-elet input-is-list pitch :itvl-field itvl-field
                  :approx approx :anal-flag t))


(defun make-filename-dispatch (list pitch)
"Calls the corresponding function that builds the appropriate filename"
  (cond
   ((and (= (length list) 7)
         (= (first list) (fourth list) (sixth list)))
         (make-filename-elet list pitch))
    (t (make-filename list pitch))))

(defun make-filename (list pitch)
"Generates a filename called <first 7 elements of list>-<pitch>.vps.
If the elements > 7 the name is <first 7 elements of list>+<pitch>.vps"

; SHOULD BE DONE BETTER AND MORE GENERALLY!

  (case (length list)
    (1 (format () "~a-~a.vps"
               (first list) pitch))
    (2 (format () "~a~a-~a.vps"
               (first list) (second list) pitch))
    (3 (format () "~a~a~a-~a.vps"
               (first list) (second list) (third list) pitch))
    (4 (format () "~a~a~a~a-~a.vps"
               (first list) (second list) (third list) (fourth list) pitch))
    (5 (format () "~a~a~a~a~a-~a.vps"
               (first list) (second list) (third list) (fourth list)
               (fifth list) pitch))
    (6 (format () "~a~a~a~a~a~a-~a.vps"
               (first list) (second list) (third list) (fourth list)
               (fifth list) (sixth list) pitch))
    (7 (format () "~a~a~a~a~a~a~a-~a.vps"
               (first list) (second list) (third list) (fourth list)
               (fifth list) (sixth list) (seventh list) pitch))
    (t (format () "~a~a~a~a~a~a~a+~a.vps"
               (first list) (second list) (third list) (fourth list)
               (fifth list) (sixth list) (seventh list) pitch))))


(defun make-filename-elet (list pitch)
"Generates the a filename of the format used in elet...fogytiglan"
  (let ((main-itvl (first list))
        (other-itvls (list (second list) (third list) (fifth list) (seventh list))))
    (format () "~a_~a~a~a~a-~a.vps"
            main-itvl
            (first other-itvls)
            (second other-itvls)
            (third other-itvls)
            (fourth other-itvls)
            pitch)))

;******************************************************************
