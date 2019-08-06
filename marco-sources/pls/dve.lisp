;*****************************************************************************
;-------| PULS SYSTEM
;-------| This file is: $LLpls/dve.lisp
;-------| Version V1.0: Mar 26, 2000
;-------| By Marco Stroppa
;-------| Copyright 2000 IRCAM
;*****************************************************************************
(in-package :cr)

; PACKAGE TO DEAL WITH DYNAMIC VIRTUAL ENVELOPES
;                 ASSOCIATED TYPE NAME: DVE


; DESCRIPTION OF THE DATA STRUCTURE:
;    A Dynamic Virtual Envelope consists of a break-point function of type FUN
;       that has not yet been compiled for a specific synthesizer. This means
;       that when the synthesizer is called, all the dynamic functions will
;       be first compiled for that synthesier, then linked to the functions
;       of the data base.

;    In the case of csound:
;        put the fun object into a "hash table" and assign to it a default
;            GEN number (see default variables: GEN-MIN, GEN-MAX, GEN-CURR,
;            GEN-FILE, DYN-GENS-LIST and the new type "DVE")
;        if the FUN object already exists (use a function with a threshold to
;           decide) return the GEN number associated to it
;        at the end, when loading the functions for csound, compile those
;            contained in DYN-GENS-LIST if it is not empty and write them onto
;            the file GEN-FILE in the directory LLfun.

; STILL NEEDED?

; CURRENT IMPLEMENTATION:
;	hash-table with a FUN and a number


; AVAILABLE FUNCTIONS:

;	CONSTRUCTORS: make_dve

;	SELECTORS:    fun_dve
;                     num_dve

;	MODIFIERS:    add_dve
;		      rm_dve
;		      reset_dve

;	PREDICATES:   is_dve
;                     is-empty_dve

;	INFO:	      print_dve
;		      short-print_dve
;		      sort-keys_dve
;		      count-keys_dve



; DESCRIPTION OF THE PACKAGE:

;	NAME:		make_dve  (CONSTRUCTOR)
;	TYPE:		Expr with 1 argument
;	CALL:		(make_dve fun)
;	FUNCTION:	define and initialize a structure of type DVE
;	VALUE:		the new structure
;	SOURCE:		$LLpls/dve.ll

(defun make_dve ()
  "Create a structure of type Dynamic Virtual Envelope"
  (attach-type 'DVE (make-hash-table
                     :test #'equalp     ; to allow for 3 = 3.0
                     :size 500          ; average size
                     )) )



;	NAME:		fun_/num_dve  (SELECTOR)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(fun_dve fun [dve])
;	FUNCTION:	return the function associated to the key or the number
;                         associated to the first function matching the arg [the first
;                         function is the first one found in the structure and not
;                         necessarily the one with the lowest number (if several exist)]
;                         nil if none are found
;	VALUE:		the new data
;	SOURCE:		$LLpls/dve.ll

(defun fun_dve (key &optional (dve (get-gbl 'DYN-GENS-LIST)))
    (pls-check-type 'DVE dve 'fun_dve)
    (gethash key (contents dve)) )


(defun num_dve (fun &optional (dve (get-gbl 'DYN-GENS-LIST)))
"Fun can be a fun or synthesizer-specific raw data"
    (pls-check-type 'DVE dve 'num_dve)

    (with-hash-table-iterator (next-entry (contents dve))
      (loop
        (multiple-value-bind (more? num val) (next-entry)
          (unless more? (return))
          (when (if (is_fun fun)
                  (equal (contents fun) (contents val))
                  (equal fun val))
            (return num))))))



;	NAME:		add_/rm_dve  (MODIFIERS)
;	TYPE:		Expr with 1 or 2 arguments
;	CALL:		(add_dve fun [dve]) / (rm_dve key [dve])
;	FUNCTION:	add the current fun to the DVE structure and return a GEN
;                          number; if fun is already there, return the corresponding
;                          number / remove the number and return 'OK
;	VALUE:		a good number of GEN or OK
;	SOURCE:		$LLpls/dve.ll

(defun add_dve (fun &optional (dve (get-gbl 'DYN-GENS-LIST)))
"Add a function to the table unless it is already present and return its key
If the object is not a function, it is supposed to be RAW DATA directly expressed
in the syntax of the current synthesizer.
Only its number will be dynamicall allocated"
    (pls-check-type 'DVE dve 'add_dve)
    (let ((curr-num (compare-fun fun dve)))   ; if no num, add new fun to structure
      (if curr-num
        curr-num
        (let ((curr-num (get-free-gen-num)))
          (setf (gethash curr-num (contents dve)) fun)
          (if (is_fun fun)
            (print (format () "DVE n. ~a~%" curr-num))
            (print (format t "RAW n. ~a~%" curr-num)))
          curr-num))))

(defun rm_dve (key &optional (dve (get-gbl 'DYN-GENS-LIST)))
"Remove a function from the table"
    (pls-check-type 'DVE dve 'rm_dve)
    (remhash key (contents dve))
    dve)



;	NAME:		reset_dve  (MODIFIER)
;	TYPE:		Expr with 0 or 1 arguments
;	CALL:		(reset_dve [dve])
;	FUNCTION:	reset the current  DVE structure
;	VALUE:		the reset structure
;	SOURCE:		$LLpls/dve.ll

(defun reset_dve (&optional (dve (get-gbl 'DYN-GENS-LIST)))
    (pls-check-type 'DVE dve 'reset_dve)
    (reset-gen-list (contents dve))
    (reset-free-gen-num)
    dve)



;	NAME:		is_dve, is-empty_dve  (PREDICATE)
;	TYPE:		Expr with 1 argument
;	CALL:		(is_dve dve)
;	VECTION:	test whether the argument is a structure of type DVE
;	VALUE:		t or nil according to the test
;	SOURCE:		$LLpls/dve.ll

(defun is_dve (dve)
  (when (is-tagged dve)
   (eq (pls-type dve) 'DVE)))

(defun is-empty_dve (dve)
  (when (is-tagged dve)
   (= (hash-table-count (contents dve)) 0)))



;	NAME:		print_/short-print_dve  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(print_ve ve)
;			(short-print_dve dve)
;	VECTION:	nicely or shortely print a structure of type DVE
;	VALUE:		the string 'done
;	SOURCE:		$LLpls/dve.ll

(defun print_dve (dve)
  (let ((hash-table (contents dve)))

    (print (format () "DYNAMIC VIRTUAL ENVELOPE - STRUCTURE OF TYPE : ~a~%" (pls-type dve)))
    (print (format () "   ~a ENTRIES~%" (hash-table-count hash-table)))

    (with-hash-table-iterator (next-entry hash-table)
      (loop
        (multiple-value-bind (more? num fun) (next-entry)
          (unless more? (return))
          (print (format () "     KEY: ~a~%     VALUE: ~a~%~%" num (contents (eval fun)))))))))


(defun short-print_dve (dve)
  (let ((hash-table (contents dve)))
    (print (format () "DVE: ~a ENTRIES~%" (hash-table-count hash-table)))

    (with-hash-table-iterator (next-entry hash-table)
      (loop
        (multiple-value-bind (more? num fun) (next-entry)
          (unless more? (return))
          (print (format () "     ~a | ~a~%" num (contents (eval fun)))))))))



;	NAME:		sort-/count-keys_dve  (INFO)
;	TYPE:		Expr with 1 argument
;	CALL:		(sort-keys_dve dve)
;	FUNCTION:	returns all the entries of a hash table sorted by '< or
;                         their amount
;	VALUE:		the value above
;	SOURCE:		$LLpls/dve.ll

(defun sort-keys_dve (dve)
  (let ((hash-table (contents dve)) 
        (result ()))
    (with-hash-table-iterator (next-entry hash-table)
      (loop
        (multiple-value-bind (more? num fun) (next-entry)
          (unless more? (return))
          (push (cons num fun) result))))
    (sort result #'< :key #'car)))


(defun count-keys_dve (dve)
  (hash-table-count (contents dve)))

;==================================================================
; SUBSIDIARY FUNCTIONS FOR DVE


(defun get-free-gen-num ()
  "Return the number associated to a csound GEN instruction and updates the DYN-GENS-LIST"
  (declare (special GEN-MAX GEN-CURR USER))
  (let ((old-num (get-gbl 'GEN-CURR)))
    (if GEN-MAX
      (if (< GEN-CURR GEN-MAX)
        GEN-CURR
        (error "~%SPACE FOR FUNCTIONS OVERFLOWN, ~a~%MAX IS ~a... DUNNO WHAT TO DO, SORRY...~%"
               (getenv USER) GEN-MAX))
      GEN-CURR)
    (incf GEN-CURR)
    old-num))


(defun reset-free-gen-num ()
  "Set the first free gen number to its lowest value. Usually done after compilation"
  (declare (special GEN-MIN GEN-CURR))
  (set-gbl 'GEN-CURR (get-gbl GEN-MIN)))


(defun reset-gen-list (hashtbl)
  "Resets the hash table. Usually done after compilation"
  (clrhash hashtbl))


(defun compare-fun (fun dve)
  "Compares the current fun to all the funs in the DVE structure.
   If no fun is found, return (), otherwise return the key of the fun"
  (num_dve fun dve))  ; for the time being, no exotic tests are performed

;*****************************************************************************
