(in-package :om)

(let ((lib (om:find-library "OMChroma")  ))
  (unless (om::loaded? lib)
    (om::load-om-lib lib)))

;(let ((lib (om:find-library "marcolib")  ))
;  (unless (om::loaded? lib)
;    (om::load-om-lib lib)))

;-----------------------------------------------------------
; MARCOLIB: what wasn't documented in Chroma and OMChroma! |
;-----------------------------------------------------------

(mapc 
 #'(lambda (file)
     (compile&load (merge-pathnames file *load-pathname*)))
 
 '(
   "marco-sources/ms-init"
   "marco-sources/back-from-om"
   "marco-sources/ms-globals"
   "marco-sources/ms-utils"

   "marco-sources/pls/dve"

   "marco-sources/dg/ms-utils-dg"
   "marco-sources/dg/DP"
   "marco-sources/dg/DPV"
   "marco-sources/dg/DPF"
   "marco-sources/dg/lkp.lisp"
   "marco-sources/dg/sp-dg.lisp"



;   "marco-sources/models/synthetic-model.lisp"
;   "marco-sources/models/utils-models.lisp"
;   "marco-sources/models/ms-methods.lisp" - EMPTY AT THE MOMENT

   "marco-sources/vps/ms-elet-sys.lisp"
   "marco-sources/vps/ms-ELET-REFERENCE.lisp"
   "marco-sources/vps/ms-get-xp.lisp"
   "marco-sources/vps/ms-set-theory-database.lisp"
   "marco-sources/vps/ms-set-theory.lisp"

   "marco-sources/marco-classes/ad1m"
   "marco-sources/marco-classes/ad1s"
   "marco-sources/marco-classes/fm1m"

   "marco-sources/userfuns/test-user-funs"

 ))

;(cl-user::clean-sources (make-pathname :directory (append (pathname-directory *load-pathname*) '("marco-sources"))))


; if the current workspace has already been loaded,
; load the library
; otherwise load it only once the workspace has been loaded
;(if om::*current-workspace*
;  (init-cr-env)
;  (om::add-init-user-func 'init-cr-env))