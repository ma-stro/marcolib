(in-package :cr)

;--------------------------------------------------
; PERSONAL ENVIRONMENT 
;--------------------------------------------------

(defvar *ms-data* (ensure-directories-exist (merge-pathnames "Documents/ms-dir/" (user-homedir-pathname))) )

;(defun make-ms-path (dir file)
;  (make-pathname :directory (append (pathname-directory *ms-dir*) dir) :name file :type "lisp"))

;(defparameter *ms-out-dir* (ensure-directories-exist (merge-pathnames "ms-out/" *ms-dir*)))



;(defun init-ms-env ()
  
; MAIN ROOT DIRECTORIES:
; MSDATA: ROOT DIRECTORY FOR MY DATA WHEN NOT IN THE LIBRARY
(set-gbl '*MSDATA* *ms-data*)

; MSROOT: ROOT DIRECTORY OF MY LIBRARY
;(cr::setenv	*MSROOT* (make-pathname :directory
;                                       (pathname-directory (om::om-namestring "cl:Userlibrary;marco_lib;"))) )
  
  ;-----------------------------------------------------------------------------
  ;LISP-BASED KERNEL SYSTEM
  
  ; ROOT DIRECTORY FOR LISP-SYSTEM
;  (cr::setenv	MSsrc	(make-pathname :directory (append
;                                                   (pathname-directory MSROOT)
;                                                   (list "marco-sources"))))
  
  ; PLS TAGGED-ARCHITECTURE SYSTEM (PERSONAL TYPES)
;  (cr::setenv	MSpls	(make-pathname :directory (append
;                                                   (pathname-directory MSsrc)
;                                                   (list "pls"))))
  
  ; DATA GENERATION AND PROCESSING
;  (cr::setenv	MSdg	(make-pathname :directory (append
;                                                   (pathname-directory MSsrc)
;                                                   (list "dg"))))
  
  ; VPS (elet + set theory)
;  (cr::setenv	MSvps	(make-pathname :directory (append
;                                                   (pathname-directory MSsrc)
;                                                   (list "vps"))))
  
  ; MODELS
;  (cr::setenv	MSmod	(make-pathname :directory (append
;                                                   (pathname-directory MSsrc)
;                                                   (list "models"))))
  
  ; LISP CLASSES
;  (cr::setenv	MScls	(make-pathname :directory (append
;                                                   (pathname-directory MSsrc)
;                                                   (list "marco-classes"))))
  
  ; CSOUND ORC
;  (cr::setenv	MSorc	(make-pathname :directory (append
;                                                   (pathname-directory MSROOT)
;                                                   (list "marco-orc"))))
  ; CSOUND FUNs
;  (cr::setenv	MSfun	(make-pathname :directory (append
;                                                   (pathname-directory MSROOT)
;                                                   (list "marco-fun"))))
  
  ;-----------------------------------------------------------------------------
  ;PERSONAL DATA
  ; DATA BASES
(set-gbl '*MSdb*
         (ensure-directories-exist
          (make-pathname :directory (append
                                     (pathname-directory (get-gbl '*MSDATA*))
                                     (list "Data-Bases")))))

  ; LIST WT-OBJECTS (LL FOR HISTORICAL REASONS)
(set-gbl '*LLwt*
         (ensure-directories-exist
          (make-pathname :directory (append
                                     (pathname-directory (get-gbl '*MSdb*))
                                     (list "wt")))))
  
  ; SOUNDS FOR WT_OBJECTS (WT FOR HISTORICAL REASONS)
(set-gbl '*WTdir*
         (ensure-directories-exist
          (make-pathname :directory (append
                                     (pathname-directory (get-gbl '*MSdb*))
                                     (list "wt-snd")))))
  
;-----------------------------------------------------------------------------
#|
  (defvar *marco-lib-files* nil)
  (setf *marco-lib-files* (list
                        
                           (cr::make-cr-path '("marco-sources") "my-globals")
                           (cr::make-cr-path '("marco-sources") "my-utils")
                           
                           (cr::make-cr-path '("marco-sources" "pls") "wt")
                           (cr::make-cr-path '("marco-sources" "pls") "wt-frz")
                           (cr::make-cr-path '("marco-sources" "pls") "dve")
                           (cr::make-cr-path '("marco-sources" "pls") "ve")
                           (cr::make-cr-path '("marco-sources" "pls") "my-pls-utils")
                           
                           (cr::make-cr-path MSdg "DP")
                           (cr::make-cr-path MSdg "DPF")
                           (cr::make-cr-path MSdg "DPV")
                           (cr::make-cr-path MSdg "DV")
                           (cr::make-cr-path MSdg "lkp")
                           (cr::make-cr-path MSdg "sp-dg")
                           (cr::make-cr-path MSdg "utils-dg")
                           
                           (cr::make-cr-path MSvps "elet-sys")
                           (cr::make-cr-path MSvps "set-theory-database")
                           (cr::make-cr-path MSvps "set-theory")
                           
                           (cr::make-cr-path MSmod "synthetic-model")
                           (cr::make-cr-path MSmod "my-methods")
                           (cr::make-cr-path MSmod "utils-models")
                           
                           ; my classes
                           (cr::make-cr-path MScls "wt1s-root")
                           (cr::make-cr-path MScls "wt1m-root")
                           (cr::make-cr-path MScls "ad1m-root")
                           (cr::make-cr-path MScls "fltn1s-root")
                           (cr::make-cr-path MScls "fltn1m-root")
                           (cr::make-cr-path MScls "fm1m-root")
                           (cr::make-cr-path MScls "fof1s-root")
                           (cr::make-cr-path MScls "fof1m-root")
                           ))
|#  

; if the current workspace has already been loaded,
; load the library
; otherwise load it only once the workspace has been loaded
;(if om::*current-workspace*
;  (init-cr-env)
;  (om::add-init-user-func 'init-cr-env))