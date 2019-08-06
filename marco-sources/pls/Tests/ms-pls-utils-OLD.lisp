;******************************************************************
;-------| PULS SYSTEM
;-------| This file is: $MSLpls/basic.ll
;-------| Version V1.2: Jan 12, 1990
;-------| By Marco Stroppa
;-------| Copyright 1986 MIT
;-------| ADAPTED to omChroma, 050221
;-------| LAST ADAPTED to omChroma 3.0, 050414
;******************************************************************

; THIS FILE CONTAINS THE BASIC FUNCTIONS FOR THE PULS SYSTEM
;      IT SHOULD BE LOADED AT THE BEGINNING

(in-package :cr)

;-----------------------------------------------------------------------------
; HISTORICAL AND IN-OUT FUNCTIONS (in alphabetical order):
;	build-cwt / build-fqwt
;	load-wt
;       load-comp-wt
;	save-wt
;	out-wt
;	out-wt-cs
;-----------------------------------------------------------------------------
; WRITE A FILE CONTAING THE CORRECT INSTRUCTION TO LOAD THE WAVE TABLES
; THE LIST OF THE WT OBJECTS TO WRITE DOWN IS INN THE GLOBAL "WTL"
(defun out-wt (file-name &rest dir)
   (let ((synthi (get-gbl 'SYNTH)) )
	(case synthi
	    (csound
		(out-wt-cs file-name (car dir)) )
;	    (moon
;		(out-wt-moon file-name (car dir)) )
	    (t
		(error-synth 'out-wt synthi)))) )

;-----------------------------------------------------------------------------
(defun out-wt-cs (file-name &optional (dir (get-gbl 'CSfun)))
"
New version ONLY working with omChroma.
Older versions are below (grayed).
filename: name of file, without extension.
          the file will have the automatic extensions <.fun>.
dir: directory where the file is to be written (default: value of CSfun).
"  

  (unless (equal (get-gbl 'MACHINE) 'om)
    (error "~%Mr. ~a, did you forget that for the time being I can only work with om?
    Why did you give me this machine, eh: ~a!~%" (getenv 'USER) (get-gbl 'MACHINE)))

  (let ((complete-filename (merge-pathnames dir (format () "~a.fun" file-name)))
        (wt-list (get-gbl 'WTL))
        )
    (when wt-list
      (format t "~%WRITING WT TABLES FOR CSOUND IN~%   ~a~%" complete-filename)
      (format t "   ON ")
      (printdate)
      (format t "~%~%")
      (with-open-file (out-stream complete-filename :direction :output 
                                :if-does-not-exist :create 
                                :if-exists :supersede)
        (format out-stream "~%;  WRITING WT TABLES FOR CSOUND ON ")
        (printdate out-stream)
        (format out-stream "~%~%")

        (mapc (lambda (wt)
                (let* ((name (car wt))
                       (dir (cadr wt))
                       (f-num (caddr wt))
                       (n-smpls (cadddr wt))
                       (f-size (1+ (pwr2 n-smpls)))
                       (sndfilein (merge-pathnames dir name))
                       )
                     
                     (format t "    f~a 0 ~a 1 ~%" f-num f-size)
                     (format t "        \"~a\" 0 4 1~%" sndfilein )
                     (when (> f-num (get-gbl 'WTFOMAX))
                       (format t "            WARNING: function number > upper limit of ~a~%" (get-gbl 'WTFOMAX)))
                     (format out-stream ";  Number of samples: ~a~%" n-smpls)
                     (format out-stream "(om::ScSt \"f ~a 0 ~a 1 " f-num f-size)
                     (format out-stream "\\\"~a\\\" 0 4 1\")~%"sndfilein ))) wt-list))))
  "done")
;-----------------------------------------------------------------------------
