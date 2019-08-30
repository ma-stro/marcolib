;*****************************************************************************
;-------| DG SYSTEM
;-------| This file is: $LLdg/DPV.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;*****************************************************************************

; THIS FILES CONTAINS ALL THE FUNCTIONS THAT GENERATE DATA PASSED AS POINTERS
;    TO A DATA BASE

(in-package :cr)
;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;    FUNCTIONS WITH CONTROL LIST (CL)
;	p-l-val / pi-l-val	: list of vals (out-of-bound vals are clipped)
;	p-mod-l-val / pi-mod-l-val
;				:  "  "  " (out-of-bound vals are folded over)
;	p-lp / pi-lp		: loop (with starting offset)
;	p-bkwd-lp / pi-bkwd-lp	: backward loop
;	p-rept-lp / pi-rept-lp
;				: repetition with loop at the end
;	p-bkwd-rept-lp / pi-bkwd-rept-lp
;				: repetition with backward loop at the end
;    FUNCTIONS WITH STEP (STP)
;	p-rept / pi-rept	: repetition scaled to interval
;	p-mod-rept / pi-mod-rept: repetition with loop modulo the size
;	p-bkwd-mod-rept / pi-bkwd-mod-rept :
;				: repetition with modulo back and forth
;	p-stp-rept / pi-stp-rept
;				: repetition with step and stop at the end
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; FUNCTIONS WITH CONTROL LIST
;-----------------------------------------------------------------------------
; (p-l-val / pi-l-val NEV CL DB [FROM TO])
; 	if (length CL) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els pointed to by CL (control list)
;	values in CL are relative to FROM and clipped by TO and FROM
; EX: (setf db  '#(11 12 13 14 15 16 17 18 19))
;     (setf  a  '(2 3 4)), (p-l-val 5 a db) ===> '(13 14 15 15 15)
; REMARK: FROM and TO fields are always taken as FROM < TO
(defun p-l-val (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (when (> nev (length cl))	; PREPARE CL
      (let ((last (copy-list (last cl))) )
        (setq cl (firstn nev
                         (append cl (nconc last last)))) ) )
    (p-set-result)))

;-----------------------------------------------------------------------------
(defun pi-l-val (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    
    (adjust-limits)
    (when (> nev (length cl))	; PREPARE CL
      (let ((last (copy-list (last cl))) )
        (setq cl (firstn nev
                         (append cl (nconc last last)))) ) )
    (pi-set-result)) )

;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-mod-l-val / pi-mod-l-val NEV CL DB [FROM TO])
; 	if (length CL) < NEV, then repeat the last value until NEV
;	   else give back the first NEV els pointed to by CL (control list)
;	values in CL are relative to FROM and folded over around TO and FROM
; EX: (setf db #(11 12 13 14 15 16 17 18 19))
;      (setf a '(2 3 4)), (p-mod-l-val 5 a db) ===> '(13 14 15 15 15)
; REMARK: FROM and TO fields are always taken as FROM < TO
(defun p-mod-l-val (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (when (> nev (length cl))	; PREPARE CL
      (let ((last (copy-list (last cl))) )
        (setq cl (firstn nev
                         (append cl (nconc last last)))) ) )
    (p-mod-set-result)) )

;-----------------------------------------------------------------------------
(defun pi-mod-l-val (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (when (> nev (length cl))	; PREPARE CL
      (let ((last (copy-list (last cl))) )
        (setq cl (firstn nev
                         (append cl (nconc last last)))) ) )
    (pi-mod-set-result)) )

;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-lp / pi-lp NEV CL DB [FROM TO OFFS])
; 	loop along the pointers contained in CL (control list)
;	else return the first NEV els pointed to by CL starting loop at OFFS
;	values in CL are relative to FROM and clipped by TO and FROM
(defun p-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (offs (ifn (cddr x) 0 (caddr x)))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((ll (copy-list cl)) )
      (nconc ll ll)
      (setq cl (firstn nev (advance offs ll))))
    (p-set-result)) )

;-----------------------------------------------------------------------------
(defun pi-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (offs (ifn (cddr x) 0 (caddr x)))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((ll (copy-list cl)) )
      (nconc ll ll)
      (setq cl (firstn nev (advance offs ll))))
    (pi-set-result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-bkwd-lp / pi-bkwd-lp NEV CL DB [FROM TO OFFS])
; 	loop along the values contained in CL going backward and forward
;	   between FROM and TO in data base DB starting at OFFS
; EX: (setf a '(1 2 3)), (pi-bkwd-lp 10 a db) ===> '(1 2 3 3 2 1 1 2 3 3)
(defun p-bkwd-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (offs (ifn (cddr x) 0 (caddr x)))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((ll (copy-list cl)) )
      (nconc ll (reverse ll))
      (nconc ll ll)
      (setq cl (firstn nev (advance offs ll))))
    (p-set-result)) )

;-----------------------------------------------------------------------------
(defun pi-bkwd-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (offs (ifn (cddr x) 0 (caddr x)))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((ll (copy-list cl)) )
      (nconc ll (reverse ll))
      (nconc ll ll)
      (setq cl (firstn nev (advance offs ll))))
    (pi-set-result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-rept-lp / pi-rept-lp NEV CL DB [FROM TO])
; 	return the values pointed to by CL with looping at the end
;       CL must be of the form '(1 2 3 4 (7 8 9))
;	scale reading to interval TO-FROM
; EX: (setf a '(1 2 3 (7 8))), (p-rept-lp 10 a db) ===> '(1 2 3 7 8 7 8 7 8 7)
;     if the list has no loop parentheses at the end, an error is signalled
(defun p-rept-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)

;    (let ((rest (list (copy-tree (car (last cl)))))
; corrected by Marco, 961203

    (let ((rest
           (if (listp (car (last cl)) )
             (copy-tree (car (last cl)))
             (copy-tree (last cl))))
; corrected, Marco, 190830, more flexible behaviour
;            (error "IRREGULAR CONTROL LIST FOR REPEATED LOOPS, SIR: ~a~%"
;                   cl)) )
          (start (firstn (1- (length cl)) cl)) )
      (setq cl (firstn nev
                       (append start (nconc rest rest)))) )
    (p-set-result)) )

;-----------------------------------------------------------------------------
(defun pi-rept-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((rest
           (if (listp (car (last cl)) )
             (copy-tree (car (last cl)))
             (copy-tree (last cl))))
; corrected, Marco, 190830, more flexible behaviour
;            (error "IRREGULAR CONTROL LIST FOR REPEATED LOOPS, SIR: ~a~%"
;                   cl)) )
          (start (firstn (1- (length cl)) cl)) )
      (setq cl (firstn nev
                       (append start (nconc rest rest)))) )
    (pi-set-result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-bkwd-rept-lp / pi-bkwd-rept-lp NEV CL DB [FROM TO])
; 	return the values pointed to by CL with backward looping at the end
;	scale reading to interval TO-FROM
(defun p-bkwd-rept-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((rest
           (if (listp (car (last cl)) )
             (copy-tree (car (last cl)))
             (copy-tree (last cl))))
; corrected, Marco, 190830, more flexible behaviour
;            (error "IRREGULAR CONTROL LIST FOR REPEATED LOOPS, SIR: ~a~%"
;                   cl)) )
          (start (firstn (1- (length cl)) cl)) )
      (nconc rest (reverse rest))
      (setq cl (firstn nev
                       (append start (nconc rest rest)))) )
    (p-set-result)) )

;-----------------------------------------------------------------------------
(defun pi-bkwd-rept-lp (nev cl db &rest x)
  (declare (special nev cl db))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (result ()) )
    (declare (special to from result))
    (adjust-limits)
    (let ((rest
           (if (listp (car (last cl)) )
             (copy-tree (car (last cl)))
             (copy-tree (last cl))))
; corrected, Marco, 190830, more flexible behaviour
;            (error "IRREGULAR CONTROL LIST FOR REPEATED LOOPS, SIR: ~a~%"
;                   cl)) )
          (start (firstn (1- (length cl)) cl)) )
      (nconc rest (reverse rest))
      (setq cl (firstn nev
                       (append start (nconc rest rest)))) )
    (pi-set-result)) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; FUNCTIONS WITH STEP
;-----------------------------------------------------------------------------
; (p-rept / pi-rept NEV DB [FROM TO])
; 	read into DB with equally spaced steps NEV values
;	scale reading to interval TO-FROM
; EX: (pi-rept 10 db) ===> read all the db step by step for 10 times
(defun p-rept (nev db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l)) )
    (declare (special to from))
    (let* ((cnt from)		; MUST FIRST GRAB THE CORRECT FROM, THEN
           (sign (adjust-limits))	 ; SWAP FROM AND TO IF FROM > TO
           (step (* (/	(- to from)
			(1- nev))
                    sign))
           (result ()) )
      (loop while (>= (decf nev) 0)
            do (newl result (svref db (truncate (+ cnt 0.5))))
            do (incf cnt step))
      (nreverse result))) )

;-----------------------------------------------------------------------------
(defun pi-rept (nev db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l)) )
    (declare (special to from))
    (let* ((cnt from)
           (sign (adjust-limits))
           (step (* (/	(- to from)
			(1- nev))
                    sign))
           (result ()) )
      (loop while (>= (decf nev) 0)
            do (newl result (vitp db cnt))
            do (incf cnt step) )
      (nreverse result))) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-stp-rept / pi-stp-rept NEV STP DB [FROM TO])
; 	read into DB with equally spaced steps of STP NEV values
;	repeat last value until NEV if needed
(defun p-stp-rept (nev stp db &rest x)
  (declare (special stp))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l)) )
    (declare (special to from))
    (let* ((cnt from)
           (sign (adjust-limits))
           (step (* stp sign))
           (result nil) )
      (check-stp 'p-stp-rept)		; ELIMINATE NEGATIVE STEPS
      
      (loop while (>= (decf nev) 0)
            do (cond ((< cnt from)
                      (newl result (svref db from)))
                     ((> cnt to)
                      (newl result (svref db to)))
                     (t
                      (if  (> step 0)
                        (newl result (svref db (truncate cnt)))
                        (newl result (svref db (truncate (+ cnt 0.5)))) )) )
            do (incf cnt step) )
      (nreverse result))) )

;-----------------------------------------------------------------------------
(defun pi-stp-rept (nev stp db &rest x)
  (declare (special stp))
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l)) )
    (declare (special to from))
    (let* ((cnt from)
           (sign (adjust-limits))
           (step (* stp sign))
           (result ()) )
      (check-stp 'pi-stp-rept)		; ELIMINATE NEGATIVE STEPS   
      (loop while (>= (decf nev) 0)
            do (cond ((< cnt from)
                      (newl result (svref db from)))
                     ((> cnt to)
                      (newl result (svref db to)))
                     (t
                      (newl result (vitp db cnt))) )
            do (incf cnt step) )
      (nreverse result))) )
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; (p-mod-rept / pi-mod-rept NEV STP DB [FROM TO OFFS])
; 	loop along the DB between FROM and TO at a step STP
;	start loop at OFFS (must always be positive from FROM)
(defun p-mod-rept (nev stp db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (cl (build-cl from to))
         (offs (ifn (cddr x) 0 (caddr x)))
         (cnt offs)
         (result ()) )
    (loop while (>= (decf nev) 0)
          do (let ((vi (truncate
                        (+ (car (advance (truncate cnt) cl))
                           0.5))) )
               (newl result (svref db vi)) )
          do (incf cnt stp))
    (nreverse result)) )

(defun pi-mod-rept (nev stp db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (cl (build-cl from to))
         (offs (ifn (cddr x) 0 (caddr x)))
         (cnt offs)
         (result ()) )
    (loop while (>= (decf nev) 0)
          do (let* ((fix-cnt (truncate cnt))
                    (rest (- cnt fix-cnt))
                    (vi (+ (car (advance fix-cnt cl)) rest)) )
               (newl result (vitp db vi)) )
          do (incf cnt stp))
    (nreverse result)) )

;-----------------------------------------------------------------------------
; (p-bkwd-mod-rept / pi-bkwd-mod-rept NEV STP DB [FROM TO OFFS])
; 	loop along the DB back and forth between FROM and TO at a step STP
;	start loop at OFFS (must always be positive from FROM)
(defun p-bkwd-mod-rept (nev stp db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (cl (build-bkwd-cl from to))
         (offs (ifn (cddr x) 0 (caddr x)))
         (cnt offs)
         (result ()) )
    (loop while (>= (decf nev) 0)
          do (let ((vi (truncate
                        (+ (car (advance (truncate cnt) cl))
                           0.5))) )
               (newl result (svref db vi)) )
          do (incf cnt stp))
    (nreverse result)) )

(defun pi-bkwd-mod-rept (nev stp db &rest x)
  (let* ((ind-l (1- (length db)))
         (from (ovfl-pr x 0 ind-l))
         (to (ovfl-pr (cdr x) ind-l ind-l))
         (cl (build-bkwd-cl from to))
         (offs (ifn (cddr x) 0 (caddr x)))
         (cnt offs)
         (result ()) )
    (loop while (>= (decf nev) 0)
          do (let* ((fix-cnt (truncate cnt))
                    (rest (- cnt fix-cnt))
                    (vi (+ (car (advance fix-cnt cl)) rest)) )
               (newl result (vitp db vi)) )
          do (incf cnt stp))
    (nreverse result)) )
;-----------------------------------------------------------------------------
