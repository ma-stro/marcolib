;******************************************************************
;-------| CTL1 SYSTEM
;-------| This file is: $LLdg/utils.ll
;-------| Version V1.0: Jan 22, 1990
;-------| By Marco Stroppa
;-------| Copyright 1990 IRCAM
;******************************************************************

; THIS FILE CONTAINS SOME USEFUL GENERAL-PURPOSE FUNCTIONS FOR DG SYSTEM

(in-package cr)


;-----------------------------------------------------------------------------
; AVAILABLE FUNCTIONS
;	build-cwt
;	build-fqwt
;	load-db / load-ve
;-----------------------------------------------------------------------------
; load-db / load-ve
; (load-db name)

(defun load-db (name)
(declare (special *MSdb*))
"Load a given data base from $LLdb
The extension of the file to be loaded is : <name>_db.lisp"
  (cond
   ((null name) 'done)
   ((or (symbolp name) (stringp name))
    (let* ((name (format () "~a_db.lisp" name))
           (path (merge-pathnames
                  (make-pathname
                   :directory (append
                               (pathname-directory (get-gbl *MSdb*))
                               ))
                  name)))
      (print (format () "Loading ~a~%" path))
      (load path)) )
   ((listp name)
    (load-db (car name))
    (load-db (cdr name)))
   (t (error "ILLEGAL ARGUMENT: ~a" name))))
  
;-----------------------------------------------------------------------------
(defun load-ve (name)
(declare (special *LLve*)) 
"Load-ve loads a data base of virtual envelopes that is in LLve/ve"
  (cond
   ((null name) 'done)
   ((or (symbolp name) (stringp name))
    (let* ((name (format () "~a_ve.lisp" name))
           (path (merge-pathnames
                  (make-pathname
                   :directory (append
                               (pathname-directory (get-gbl *LLve*))
                               ))
                  name)))
      (print (format () "Loading ~a~%" path))
      (load path)) )
   ((listp name)
    (load-ve (car name))
    (load-ve (cdr name)))
   (t (error "ILLEGAL ARGUMENT: ~a" name))))

;-----------------------------------------------------------------------------
; build-cwt
; (build-cwt L-CWT)
;  EX: (build-cwt wt-a1 (wt-a2 0 1) (wt-a3 (att_wt wt-a3) (dur_wt wt-a3)))
(defun build-cwt (&rest l-cwt)
"B(uild) a CWT field for CTL1 level (shortcut)
where L-CWT: any wt objects (just the name if alone, the list (name val1 val2) if start and end offs are set)
"
    (mapcar (lambda (wt)
		(if (symbolp wt)
		    `'(list ',wt)
		    `'(list ',(nextl wt) ,.wt)) )
	    l-cwt) )

;-----------------------------------------------------------------------------
; build-fqwt
; (build-fqwt L-FQ)
;  EX: (build-fqwt (1.0) (1.059 (nth-freq_wt wt-a2 2)) (440 261))
(defun build-fqwt (&rest l-fqwt)
"B(uild) a FQ field for CTL1 level WTsys (shortcut).
where L-FQ: list of (out-fq [ref-fq]) by default, ref-fq = 1.0.
"
    (mapcar (lambda (fq)
		(let ((out-fq (nextl fq))
		      (ref-fq (ifn fq 1.0 (nextl fq))) )
		    `'(list ,out-fq ,ref-fq)) )
	    l-fqwt) )
;-----------------------------------------------------------------------------
