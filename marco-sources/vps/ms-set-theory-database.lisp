;******************************************************************
;-------| VPS SYSTEM
;-------| This file is: $LLvps/set-theory-data base.lisp
;-------| By Marco Stroppa
;-------| Version 1.0, September 1999
;-------| Copyright 1999 IRCAM
;******************************************************************

(in-package chroma)


;;;;;;;;;;;;;; SET THEORY

;;;;;;;;;;;;;; DATA BASE CONTAINING NAMES AND VECTORS

;*SET-NAMES*
(defvar *SET-NAMES*
      ; ((Prime-form) (Name) (Vector)
      ;  (COMPL prime-form) (COMPL Name) (COMPL Vector))
      
      ; ((COMPL prime-form) (COMPL Name) (COMPL Vector)
      ;  (Prime-form) (Name) (Vector))
      '(
        
        ; 3 / 9
        
        ((0 1 2) (3-1 (12)) (2 1 0 0 0 0)
         (0 1 2 3 4 5 6 7 8) (9-1) (8 7 6 6 6 3))
        ((0 1 2 3 4 5 6 7 8) (9-1) (8 7 6 6 6 3)
         (0 1 2) (3-1 (12)) (2 1 0 0 0 0))
        
        ((0 1 3) (3-2) (1 1 1 0 0 0)
         (0 1 2 3 4 5 6 7 9) (9-2) (7 7 7 6 6 3))
        ((0 1 2 3 4 5 6 7 9) (9-2) (7 7 7 6 6 3)
         (0 1 3) (3-2) (1 1 1 0 0 0))
        
        ((0 1 4) (3-3) (1 0 1 1 0 0)
         (0 1 2 3 4 5 6 8 9) (9-3) (7 6 7 7 6 3))
        ((0 1 2 3 4 5 6 8 9) (9-3) (7 6 7 7 6 3)
         (0 1 4) (3-3) (1 0 1 1 0 0))
        
        ((0 1 5) (3-4) (1 0 0 1 1 0)
         (0 1 2 3 4 5 7 8 9) (9-4) (7 6 6 7 7 3))
        ((0 1 2 3 4 5 7 8 9) (9-4) (7 6 6 7 7 3)
         (0 1 5) (3-4) (1 0 0 1 1 0))
        
        ((0 1 6) (3-5) (1 0 0 0 1 1)
         (0 1 2 3 4 6 7 8 9) (9-5) (7 6 6 6 7 4))
        ((0 1 2 3 4 6 7 8 9) (9-5) (7 6 6 6 7 4)
         (0 1 6) (3-5) (1 0 0 0 1 1))
        
        ((0 2 4) (3-6 (12)) (0 2 0 1 0 0)
         (0 1 2 3 4 5 6 8 10) (9-6) (6 8 6 7 6 3))
        ((0 1 2 3 4 5 6 8 10) (9-6) (6 8 6 7 6 3)
         (0 2 4) (3-6 (12)) (0 2 0 1 0 0))
        
        ((0 2 5) (3-7) (0 1 1 0 1 0)
         (0 1 2 3 4 5 7 8 10) (9-7) (6 7 7 6 7 3))
        ((0 1 2 3 4 5 7 8 10) (9-7) (6 7 7 6 7 3)
         (0 2 5) (3-7) (0 1 1 0 1 0))
        
        ((0 2 6) (3-8) (0 1 0 1 0 1)
         (0 1 2 3 4 6 7 8 10) (9-8) (6 7 6 7 6 4))
        ((0 1 2 3 4 6 7 8 10) (9-8) (6 7 6 7 6 4)
         (0 2 6) (3-8) (0 1 0 1 0 1))
        
        ((0 2 7) (3-9 (12)) (0 1 0 0 2 0)
         (0 1 2 3 5 6 7 8 10) (9-9) (6 7 6 6 8 3))
        ((0 1 2 3 5 6 7 8 10) (9-9) (6 7 6 6 8 3)
         (0 2 7) (3-9 (12)) (0 1 0 0 2 0))
        
        ((0 3 6) (3-10 (12)) (0 0 2 0 0 1)
         (0 1 2 3 4 6 7 9 10) (9-10) (6 6 8 6 6 4))
        ((0 1 2 3 4 6 7 9 10) (9-10) (6 6 8 6 6 4)
         (0 3 6) (3-10 (12)) (0 0 2 0 0 1))
        
        ((0 3 7) (3-11) (0 0 1 1 1 0)
         (0 1 2 3 5 6 7 9 10) (9-11) (6 6 7 7 7 3))
        ((0 1 2 3 5 6 7 9 10) (9-11) (6 6 7 7 7 3)
         (0 3 7) (3-11) (0 0 1 1 1 0))
        
        ((0 4 8) (3-12 (4)) (0 0 0 3 0 0)
         (0 1 2 4 5 6 8 9 10) (9-12) (6 6 6 9 6 3))
        ((0 1 2 4 5 6 8 9 10) (9-12) (6 6 6 9 6 3)
         (0 4 8) (3-12 (4)) (0 0 0 3 0 0))
        
        ; 4 / 8
        
        ((0 1 2 3) (4-1 (12)) (3 2 1 0 0 0)
         (0 1 2 3 4 5 6 7) (8-1) (7 6 5 4 4 2))
        ((0 1 2 3 4 5 6 7) (8-1) (7 6 5 4 4 2)
         (0 1 2 3) (4-1 (12)) (3 2 1 0 0 0))
        
        ((0 1 2 4) (4-2) (2 2 1 1 0 0)
         (0 1 2 3 4 5 6 8) (8-2) (6 6 5 5 4 2))
        ((0 1 2 3 4 5 6 8) (8-2) (6 6 5 5 4 2)
         (0 1 2 4) (4-2) (2 2 1 1 0 0))
        
        ((0 1 3 4) (4-3 (12)) (2 1 2 1 0 0)
         (0 1 2 3 4 5 6 9) (8-3) (6 5 6 5 4 2))
        ((0 1 2 3 4 5 6 9) (8-3) (6 5 6 5 4 2)
         (0 1 3 4) (4-3 (12)) (2 1 2 1 0 0))
        
        ((0 1 2 5) (4-4) (2 1 1 1 1 0)
         (0 1 2 3 4 5 7 8) (8-4) (6 5 5 5 5 2))
        ((0 1 2 3 4 5 7 8) (8-4) (6 5 5 5 5 2)
         (0 1 2 5) (4-4) (2 1 1 1 1 0))
        
        ((0 1 2 6) (4-5) (2 1 0 1 1 1)
         (0 1 2 3 4 6 7 8) (8-5) (6 5 4 5 5 3))
        ((0 1 2 3 4 6 7 8) (8-5) (6 5 4 5 5 3)
         (0 1 2 6) (4-5) (2 1 0 1 1 1))
        
        ((0 1 2 7) (4-6 (12)) (2 1 0 0 2 1)
         (0 1 2 3 5 6 7 8) (8-6) (6 5 4 4 6 3))
        ((0 1 2 3 5 6 7 8) (8-6) (6 5 4 4 6 3)
         (0 1 2 7) (4-6 (12)) (2 1 0 0 2 1))
        
        ((0 1 4 5) (4-7 (12)) (2 0 1 2 1 0)
         (0 1 2 3 4 5 8 9) (8-7) (6 4 5 6 5 2))
        ((0 1 2 3 4 5 8 9) (8-7) (6 4 5 6 5 2)
         (0 1 4 5) (4-7 (12)) (2 0 1 2 1 0))
        
        ((0 1 5 6) (4-8 (12)) (2 0 0 1 2 1)
         (0 1 2 3 4 7 8 9) (8-8) (6 4 4 5 6 3))
        ((0 1 2 3 4 7 8 9) (8-8) (6 4 4 5 6 3)
         (0 1 5 6) (4-8 (12)) (2 0 0 1 2 1))
        
        ((0 1 6 7) (4-9 (6)) (2 0 0 0 2 2)
         (0 1 2 3 6 7 8 9) (8-9) (6 4 4 4 6 4))
        ((0 1 2 3 6 7 8 9) (8-9) (6 4 4 4 6 4)
         (0 1 6 7) (4-9 (6)) (2 0 0 0 2 2))
        
        ((0 2 3 5) (4-10 (12)) (1 2 2 0 1 0)
         (0 2 3 4 5 6 7 9) (8-10) (5 6 6 4 5 2))
        ((0 2 3 4 5 6 7 9) (8-10) (5 6 6 4 5 2)
         (0 2 3 5) (4-10 (12)) (1 2 2 0 1 0))
        
        ((0 1 3 5) (4-11) (1 2 1 1 1 0)
         (0 1 2 3 4 5 7 9) (8-11) (5 6 5 5 5 2))
        ((0 1 2 3 4 5 7 9) (8-11) (5 6 5 5 5 2)
         (0 1 3 5) (4-11) (1 2 1 1 1 0))
        
        ((0 2 3 6) (4-12) (1 1 2 1 0 1)
         (0 1 3 4 5 6 7 9) (8-12) (5 5 6 5 4 3))
        ((0 1 3 4 5 6 7 9) (8-12) (5 5 6 5 4 3)
         (0 2 3 6) (4-12) (1 1 2 1 0 1))
        
        ((0 1 3 6) (4-13) (1 1 2 0 1 1)
         (0 1 2 3 4 6 7 9) (8-13) (5 5 6 4 5 3))
        ((0 1 2 3 4 6 7 9) (8-13) (5 5 6 4 5 3)
         (0 1 3 6) (4-13) (1 1 2 0 1 1))
        
        ((0 2 3 7) (4-14) (1 1 1 1 2 0)
         (0 1 2 4 5 6 7 9) (8-14) (5 5 5 5 6 2))
        ((0 1 2 4 5 6 7 9) (8-14) (5 5 5 5 6 2)
         (0 2 3 7) (4-14) (1 1 1 1 2 0))
        
        ((0 1 4 6) (4-Z15) (1 1 1 1 1 1)
         (0 1 2 3 4 6 8 9) (8-Z15) (5 5 5 5 5 3))
        ((0 1 2 3 4 6 8 9) (8-Z15) (5 5 5 5 5 3)
         (0 1 4 6) (4-Z15) (1 1 1 1 1 1))
        
        ((0 1 5 7) (4-16) (1 1 0 1 2 1)
         (0 1 2 3 5 7 8 9) (8-16) (5 5 4 5 6 3))
        ((0 1 2 3 5 7 8 9) (8-16) (5 5 4 5 6 3)
         (0 1 5 7) (4-16) (1 1 0 1 2 1))
        
        ((0 3 4 7) (4-17 (12)) (1 0 2 2 1 0)
         (0 1 3 4 5 6 8 9) (8-17) (5 4 6 6 5 2))
        ((0 1 3 4 5 6 8 9) (8-17) (5 4 6 6 5 2)
         (0 3 4 7) (4-17 (12)) (1 0 2 2 1 0))
        
        ((0 1 4 7) (4-18) (1 0 2 1 1 1)
         (0 1 2 3 5 6 8 9) (8-18) (5 4 6 5 5 3))
        ((0 1 2 3 5 6 8 9) (8-18) (5 4 6 5 5 3)
         (0 1 4 7) (4-18) (1 0 2 1 1 1))
        
        ((0 1 4 8) (4-19) (1 0 1 3 1 0)
         (0 1 2 4 5 6 8 9) (8-19) (5 4 5 7 5 2))
        ((0 1 2 4 5 6 8 9) (8-19) (5 4 5 7 5 2)
         (0 1 4 8) (4-19) (1 0 1 3 1 0))
        
        ((0 1 5 8) (4-20 (12)) (1 0 1 2 2 0)
         (0 1 2 4 5 7 8 9) (8-20) (5 4 5 6 6 2))
        ((0 1 2 4 5 7 8 9) (8-20) (5 4 5 6 6 2)
         (0 1 5 8) (4-20 (12)) (1 0 1 2 2 0))
        
        ((0 2 4 6) (4-21 (12)) (0 3 0 2 0 1)
         (0 1 2 3 4 6 8 10) (8-21) (4 7 4 6 4 3))
        ((0 1 2 3 4 6 8 10) (8-21) (4 7 4 6 4 3)
         (0 2 4 6) (4-21 (12)) (0 3 0 2 0 1))
        
        ((0 2 4 7) (4-22) (0 2 1 1 2 0)
         (0 1 2 3 5 6 8 10) (8-22) (4 6 5 5 6 2))
        ((0 1 2 3 5 6 8 10) (8-22) (4 6 5 5 6 2)
         (0 2 4 7) (4-22) (0 2 1 1 2 0))
        
        ((0 2 5 7) (4-23 (12)) (0 2 1 0 3 0)
         (0 1 2 3 5 7 8 10) (8-23) (4 6 5 4 7 2))
        ((0 1 2 3 5 7 8 10) (8-23) (4 6 5 4 7 2)
         (0 2 5 7) (4-23 (12)) (0 2 1 0 3 0))
        
        ((0 2 4 8) (4-24 (12)) (0 2 0 3 0 1)
         (0 1 2 4 5 6 8 10) (8-24) (4 6 4 7 4 3))
        ((0 1 2 4 5 6 8 10) (8-24) (4 6 4 7 4 3)
         (0 2 4 8) (4-24 (12)) (0 2 0 3 0 1))
        
        ((0 2 6 8) (4-25 (6)) (0 2 0 2 0 2)
         (0 1 2 4 6 7 8 10) (8-25) (4 6 4 6 4 4))
        ((0 1 2 4 6 7 8 10) (8-25) (4 6 4 6 4 4)
         (0 2 6 8) (4-25 (6)) (0 2 0 2 0 2))
        
        ((0 3 5 8) (4-26 (12)) (0 1 2 1 2 0)
         (0 1 2 4 5 7 9 10) (8-26) (4 5 6 5 6 2))
        ((0 1 2 4 5 7 9 10) (8-26) (4 5 6 5 6 2)
         (0 3 5 8) (4-26 (12)) (0 1 2 1 2 0))
        
        ((0 2 5 8) (4-27) (0 1 2 1 1 1)
         (0 1 2 4 5 7 8 10) (8-27) (4 5 6 5 5 3))
        ((0 1 2 4 5 7 8 10) (8-27) (4 5 6 5 5 3)
         (0 2 5 8) (4-27) (0 1 2 1 1 1))
        
        ((0 3 6 9) (4-28 (3)) (0 0 4 0 0 2)
         (0 1 3 5 7 8 9 10) (8-28) (4 4 8 4 4 4))
        ((0 1 3 5 7 8 9 10) (8-28) (4 4 8 4 4 4)
         (0 3 6 9) (4-28 (3)) (0 0 4 0 0 2))
        
        ((0 1 3 7) (4-Z29) (1 1 1 1 1 1)
         (0 1 2 3 5 6 7 9) (8-Z29) (5 5 5 5 5 3))
        ((0 1 2 3 5 6 7 9) (8-Z29) (5 5 5 5 5 3)
         (0 1 3 7) (4-Z29) (1 1 1 1 1 1))
        
        
        ; 5 / 7
        
        ((0 1 2 3 4) (5-1 (12)) (4 3 2 1 0 0)
         (0 1 2 3 4 5 6) (7-1) (6 5 4 3 2 1))
        ((0 1 2 3 4 5 6) (7-1) (6 5 4 3 2 1)
         (0 1 2 3 4) (5-1 (12)) (4 3 2 1 0 0))
        
        ((0 1 2 3 5) (5-2) (3 3 2 1 1 0)
         (0 1 2 3 4 5 7) (7-2) (5 5 4 3 3 1))
        ((0 1 2 3 4 5 7) (7-2) (5 5 4 3 3 1)
         (0 1 2 3 5) (5-2) (3 3 2 1 1 0))
        
        ((0 1 2 4 5) (5-3) (3 2 2 2 1 0)
         (0 1 2 3 4 5 8) (7-3) (5 4 4 4 3 1))
        ((0 1 2 3 4 5 8) (7-3) (5 4 4 4 3 1)
         (0 1 2 4 5) (5-3) (3 2 2 2 1 0))
        
        ((0 1 2 3 6) (5-4) (3 2 2 1 1 1)
         (0 1 2 3 4 6 7) (7-4) (5 4 4 3 3 2))
        ((0 1 2 3 4 6 7) (7-4) (5 4 4 3 3 2)
         (0 1 2 3 6) (5-4) (3 2 2 1 1 1))
        
        ((0 1 2 3 7) (5-5) (3 2 1 1 2 1)
         (0 1 2 3 5 6 7) (7-5) (5 4 3 3 4 2))
        ((0 1 2 3 5 6 7) (7-5) (5 4 3 3 4 2)
         (0 1 2 3 7) (5-5) (3 2 1 1 2 1))
        
        ((0 1 2 5 6) (5-6) (3 1 1 2 2 1)
         (0 1 2 3 4 7 8) (7-6) (5 3 3 4 4 2))
        ((0 1 2 3 4 7 8) (7-6) (5 3 3 4 4 2)
         (0 1 2 5 6) (5-6) (3 1 1 2 2 1))
        
        ((0 1 2 6 7) (5-7) (3 1 0 1 3 2)
         (0 1 2 3 6 7 8) (7-7) (5 3 2 3 5 3))
        ((0 1 2 3 6 7 8) (7-7) (5 3 2 3 5 3)
         (0 1 2 6 7) (5-7) (3 1 0 1 3 2))
        
        ((0 2 3 4 6) (5-8 (12)) (2 3 2 2 0 1)
         (0 2 3 4 5 6 8) (7-8) (4 5 4 4 2 2))
        ((0 2 3 4 5 6 8) (7-8) (4 5 4 4 2 2)
         (0 2 3 4 6) (5-8 (12)) (2 3 2 2 0 1))
        
        ((0 1 2 4 6) (5-9) (2 3 1 2 1 1)
         (0 1 2 3 4 6 8) (7-9) (4 5 3 4 3 2))
        ((0 1 2 3 4 6 8) (7-9) (4 5 3 4 3 2)
         (0 1 2 4 6) (5-9) (2 3 1 2 1 1))
        
        ((0 1 3 4 6) (5-10) (2 2 3 1 1 1)
         (0 1 2 3 4 6 9) (7-10) (4 4 5 3 3 2))
        ((0 1 2 3 4 6 9) (7-10) (4 4 5 3 3 2)
         (0 1 3 4 6) (5-10) (2 2 3 1 1 1))
        
        ((0 2 3 4 7) (5-11) (2 2 2 2 2 0)
         (0 1 3 4 5 6 8) (7-11) (4 4 4 4 4 1))
        ((0 1 3 4 5 6 8) (7-11) (4 4 4 4 4 1)
         (0 2 3 4 7) (5-11) (2 2 2 2 2 0))
        
        ((0 1 3 5 6) (5-Z12 (12)) (2 2 2 1 2 1)
         (0 1 2 3 4 7 9) (7-Z12) (4 4 4 3 4 2))
        ((0 1 2 3 4 7 9) (7-Z12) (4 4 4 3 4 2)
         (0 1 3 5 6) (5-Z12 (12)) (2 2 2 1 2 1))
        
        ((0 1 2 4 8) (5-13) (2 2 1 3 1 1)
         (0 1 2 4 5 6 8) (7-13) (4 4 3 5 3 2))
        ((0 1 2 4 5 6 8) (7-13) (4 4 3 5 3 2)
         (0 1 2 4 8) (5-13) (2 2 1 3 1 1))
        
        ((0 1 2 5 7) (5-14) (2 2 1 1 3 1)
         (0 1 2 3 5 7 8) (7-14) (4 4 3 3 5 2))
        ((0 1 2 3 5 7 8) (7-14) (4 4 3 3 5 2)
         (0 1 2 5 7) (5-14) (2 2 1 1 3 1))
        
        ((0 1 2 6 8) (5-15 (12)) (2 2 0 2 2 2)
         (0 1 2 4 6 7 8) (7-15) (4 4 2 4 4 3))
        ((0 1 2 4 6 7 8) (7-15) (4 4 2 4 4 3)
         (0 1 2 6 8) (5-15 (12)) (2 2 0 2 2 2))
        
        ((0 1 3 4 7) (5-16) (2 1 3 2 1 1)
         (0 1 2 3 5 6 9) (7-16) (4 3 5 4 3 2))
        ((0 1 2 3 5 6 9) (7-16) (4 3 5 4 3 2)
         (0 1 3 4 7) (5-16) (2 1 3 2 1 1))
        
        ((0 1 3 4 8) (5-Z17 (12)) (2 1 2 3 2 0)
         (0 1 2 4 5 6 9) (7-Z17) (4 3 4 5 4 1))
        ((0 1 2 4 5 6 9) (7-Z17) (4 3 4 5 4 1)
         (0 1 3 4 8) (5-Z17 (12)) (2 1 2 3 2 0))
        
        ((0 1 4 5 7) (5-Z18) (2 1 2 2 2 1)
         (0 1 2 3 5 8 9) (7-Z18) (4 3 4 4 4 2))
        ((0 1 2 3 5 8 9) (7-Z18) (4 3 4 4 4 2)
         (0 1 4 5 7) (5-Z18) (2 1 2 2 2 1))
        
        ((0 1 3 6 7) (5-19) (2 1 2 1 2 2)
         (0 1 2 3 6 7 9) (7-19) (4 3 4 3 4 3))
        ((0 1 2 3 6 7 9) (7-19) (4 3 4 3 4 3)
         (0 1 3 6 7) (5-19) (2 1 2 1 2 2))
        
        ((0 1 3 7 8) (5-20) (2 1 1 2 3 1)
         (0 1 2 4 7 8 9) (7-20) (4 3 3 4 5 2))
        ((0 1 2 4 7 8 9) (7-20) (4 3 3 4 5 2)
         (0 1 3 7 8) (5-20) (2 1 1 2 3 1))
        
        ((0 1 4 5 8) (5-21) (2 0 2 4 2 0)
         (0 1 2 4 5 8 9) (7-21) (4 2 4 6 4 1))
        ((0 1 2 4 5 8 9) (7-21) (4 2 4 6 4 1)
         (0 1 4 5 8) (5-21) (2 0 2 4 2 0))
        
        ((0 1 4 7 8) (5-22 (12)) (2 0 2 3 2 1)
         (0 1 2 5 6 8 9) (7-22) (4 2 4 5 4 2))
        ((0 1 2 5 6 8 9) (7-22) (4 2 4 5 4 2)
         (0 1 4 7 8) (5-22 (12)) (2 0 2 3 2 1))
        
        ((0 2 3 5 7) (5-23) (1 3 2 1 3 0)
         (0 2 3 4 5 7 9) (7-23) (3 5 4 3 5 1))
        ((0 2 3 4 5 7 9) (7-23) (3 5 4 3 5 1)
         (0 2 3 5 7) (5-23) (1 3 2 1 3 0))
        
        ((0 1 3 5 7) (5-24) (1 3 1 2 2 1)
         (0 1 2 3 5 7 9) (7-24) (3 5 3 4 4 2))
        ((0 1 2 3 5 7 9) (7-24) (3 5 3 4 4 2)
         (0 1 3 5 7) (5-24) (1 3 1 2 2 1))
        
        ((0 2 3 5 8) (5-25) (1 2 3 1 2 1)
         (0 2 3 4 6 7 9) (7-25) (3 4 5 3 4 2))
        ((0 2 3 4 6 7 9) (7-25) (3 4 5 3 4 2)
         (0 2 3 5 8) (5-25) (1 2 3 1 2 1))
        
        ((0 2 4 5 8) (5-26) (1 2 2 3 1 1)
         (0 1 3 4 5 7 9) (7-26) (3 4 4 5 3 2))
        ((0 1 3 4 5 7 9) (7-26) (3 4 4 5 3 2)
         (0 2 4 5 8) (5-26) (1 2 2 3 1 1))
        
        ((0 1 3 5 8) (5-27) (1 2 2 2 3 0)
         (0 1 2 4 5 7 9) (7-27) (3 4 4 4 5 1))
        ((0 1 2 4 5 7 9) (7-27) (3 4 4 4 5 1)
         (0 1 3 5 8) (5-27) (1 2 2 2 3 0))
        
        ((0 2 3 6 8) (5-28) (1 2 2 2 1 2)
         (0 1 3 5 6 7 9) (7-28) (3 4 4 4 3 3))
        ((0 1 3 5 6 7 9) (7-28) (3 4 4 4 3 3)
         (0 2 3 6 8) (5-28) (1 2 2 2 1 2))
        
        ((0 1 3 6 8) (5-29) (1 2 2 1 3 1)
         (0 1 2 4 6 7 9) (7-29) (3 4 4 3 5 2))
        ((0 1 2 4 6 7 9) (7-29) (3 4 4 3 5 2)
         (0 1 3 6 8) (5-29) (1 2 2 1 3 1))
        
        ((0 1 4 6 8) (5-30) (1 2 1 3 2 1)
         (0 1 2 4 6 8 9) (7-30) (3 4 3 5 4 2))
        ((0 1 2 4 6 8 9) (7-30) (3 4 3 5 4 2)
         (0 1 4 6 8) (5-30) (1 2 1 3 2 1))
        
        ((0 1 3 6 9) (5-31) (1 1 4 1 1 2)
         (0 1 3 4 6 7 9) (7-31) (3 3 6 3 3 3))
        ((0 1 3 4 6 7 9) (7-31) (3 3 6 3 3 3)
         (0 1 3 6 9) (5-31) (1 1 4 1 1 2))
        
        ((0 1 4 6 9) (5-32) (1 1 3 2 2 1)
         (0 1 3 4 6 8 9) (7-32) (3 3 5 4 4 2))
        ((0 1 3 4 6 8 9) (7-32) (3 3 5 4 4 2)
         (0 1 4 6 9) (5-32) (1 1 3 2 2 1))
        
        ((0 2 4 6 8) (5-33 (12)) (0 4 0 4 0 2)
         (0 1 2 4 6 8 10) (7-33) (2 6 2 6 2 3))
        ((0 1 2 4 6 8 10) (7-33) (2 6 2 6 2 3)
         (0 2 4 6 8) (5-33 (12)) (0 4 0 4 0 2))
        
        ((0 2 4 6 9) (5-34 (12)) (0 3 2 2 2 1)
         (0 1 3 4 6 8 10) (7-34) (2 5 4 4 4 2))
        ((0 1 3 4 6 8 10) (7-34) (2 5 4 4 4 2)
         (0 2 4 6 9) (5-34 (12)) (0 3 2 2 2 1))
        
        ((0 2 4 7 9) (5-35 (12)) (0 3 2 1 4 0)
         (0 1 3 5 6 8 10) (7-35) (2 5 4 3 6 1))
        ((0 1 3 5 6 8 10) (7-35) (2 5 4 3 6 1)
         (0 2 4 7 9) (5-35 (12)) (0 3 2 1 4 0))
        
        ((0 1 2 4 7) (5-Z36) (2 2 2 1 2 1)
         (0 1 2 3 5 6 8) (7-Z36) (4 4 4 3 4 2))
        ((0 1 2 3 5 6 8) (7-Z36) (4 4 4 3 4 2)
         (0 1 2 4 7) (5-Z36) (2 2 2 1 2 1))
        
        ((0 3 4 5 8) (5-Z37 (12)) (2 1 2 3 2 0)
         (0 1 3 4 5 7 8) (7-Z37) (4 3 4 5 4 1))
        ((0 1 3 4 5 7 8) (7-Z37) (4 3 4 5 4 1)
         (0 3 4 5 8) (5-Z37 (12)) (2 1 2 3 2 0))
        
        ((0 1 2 5 8) (5-Z38) (2 1 2 2 2 1)
         (0 1 2 4 5 7 8) (7-Z38) (4 3 4 4 4 2))
        ((0 1 2 4 5 7 8) (7-Z38) (4 3 4 4 4 2)
         (0 1 2 5 8) (5-Z38) (2 1 2 2 2 1))
        
        ; 6
        
        ((0 1 2 3 4 5) (6-1 (12)) (5 4 3 2 1 0))
        
        ((0 1 2 3 4 6) (6-2) (4 4 3 2 1 1))
        
        ((0 1 2 3 5 6) (6-Z3) (4 3 3 2 2 1)
         (0 1 2 3 4 7) (6-Z36) (4 3 3 2 2 1))
        ((0 1 2 3 4 7) (6-Z36) (4 3 3 2 2 1)
         (0 1 2 3 5 6) (6-Z3) (4 3 3 2 2 1))
        
        ((0 1 2 4 5 6) (6-Z4 (12)) (4 3 2 3 2 1)
         (0 1 2 3 4 8) (6-Z37 (12)) (4 3 2 3 2 1))
        ((0 1 2 3 4 8) (6-Z37 (12)) (4 3 2 3 2 1)
         (0 1 2 4 5 6) (6-Z4 (12)) (4 3 2 3 2 1))
        
        ((0 1 2 3 6 7) (6-5) (4 2 2 2 3 2))
        
        ((0 1 2 5 6 7) (6-Z6 (12)) (4 2 1 2 4 2)
         (0 1 2 3 7 8) (6-Z38 (12)) (4 2 1 2 4 2))
        ((0 1 2 3 7 8) (6-Z38 (12)) (4 2 1 2 4 2)
         (0 1 2 5 6 7) (6-Z6 (12)) (4 2 1 2 4 2))
        
        ((0 1 2 6 7 8) (6-7 (6)) (4 2 0 2 4 3))
        
        ((0 2 3 4 5 7) (6-8 (12)) (3 4 3 2 3 0))
        
        ((0 1 2 3 5 7) (6-9) (3 4 2 2 3 1))
        
        ((0 1 3 4 5 7) (6-Z10) (3 3 3 3 2 1)
         (0 2 3 4 5 8) (6-Z39) (3 3 3 3 2 1))
        ((0 2 3 4 5 8) (6-Z39) (3 3 3 3 2 1)
         (0 1 3 4 5 7) (6-Z10) (3 3 3 3 2 1))
        
        ((0 1 2 4 5 7) (6-Z11) (3 3 3 2 3 1)
         (0 1 2 3 5 8) (6-Z40) (3 3 3 2 3 1))
        ((0 1 2 3 5 8) (6-Z40) (3 3 3 2 3 1)
         (0 1 2 4 5 7) (6-Z11) (3 3 3 2 3 1))
        
        ((0 1 2 4 6 7) (6-Z12) (3 3 2 2 3 2)
         (0 1 2 3 6 8) (6-Z41) (3 3 2 2 3 2))
        ((0 1 2 3 6 8) (6-Z41) (3 3 2 2 3 2)
         (0 1 2 4 6 7) (6-Z12) (3 3 2 2 3 2))
        
        ((0 1 3 4 6 7) (6-Z13 (12)) (3 2 4 2 2 2)
         (0 1 2 3 6 9) (6-Z42 (12)) (3 2 4 2 2 2))
        ((0 1 2 3 6 9) (6-Z42 (12)) (3 2 4 2 2 2)
         (0 1 3 4 6 7) (6-Z13 (12)) (3 2 4 2 2 2))
        
        ((0 1 3 4 5 8) (6-14) (3 2 3 4 3 0))
        
        ((0 1 2 4 5 8) (6-15) (3 2 3 4 2 1))
        
        ((0 1 4 5 6 8) (6-16) (3 2 2 4 3 1))
        
        ((0 1 2 4 7 8) (6-Z17) (3 2 2 3 3 2)
         (0 1 2 5 6 8) (6-Z43) (3 2 2 3 3 2))
        ((0 1 2 5 6 8) (6-Z43) (3 2 2 3 3 2)
         (0 1 2 4 7 8) (6-Z17) (3 2 2 3 3 2))
        
        ((0 1 2 5 7 8) (6-18) (3 2 2 2 4 2))
        
        ((0 1 3 4 7 8) (6-Z19) (3 1 3 4 3 1)
         (0 1 2 5 6 9) (6-Z44) (3 1 3 4 3 1))
        ((0 1 2 5 6 9) (6-Z44) (3 1 3 4 3 1)
         (0 1 3 4 7 8) (6-Z19) (3 1 3 4 3 1))
        
        ((0 1 4 5 8 9) (6-20 (4)) (3 0 3 6 3 0))
        
        ((0 2 3 4 6 8) (6-21) (2 4 2 2 1 2))
        
        ((0 1 2 4 6 8) (6-22) (2 4 1 4 2 2))
        
        ((0 2 3 5 6 8) (6-Z23 (12)) (2 3 4 2 2 2)
         (0 2 3 4 6 9) (6-Z45 (12)) (2 3 4 2 2 2))
        ((0 2 3 4 6 9) (6-Z45 (12)) (2 3 4 2 2 2)
         (0 2 3 5 6 8) (6-Z23 (12)) (2 3 4 2 2 2))
        
        ((0 1 3 4 6 8) (6-Z24) (2 3 3 3 3 1)
         (0 1 2 4 6 9) (6-Z46) (2 3 3 3 3 1))
        ((0 1 2 4 6 9) (6-Z46) (2 3 3 3 3 1)
         (0 1 3 4 6 8) (6-Z24) (2 3 3 3 3 1))
        
        ((0 1 3 5 6 8) (6-Z25) (2 3 3 2 4 1)
         (0 1 2 4 7 9) (6-Z47) (2 3 3 2 4 1))
        ((0 1 2 4 7 9) (6-Z47) (2 3 3 2 4 1)
         (0 1 3 5 6 8) (6-Z25) (2 3 3 2 4 1))
        
        ((0 1 3 5 7 8) (6-Z26 (12)) (2 3 2 3 4 1)
         (0 1 2 5 7 9) (6-Z48 (12)) (2 3 2 3 4 1))
        ((0 1 2 5 7 9) (6-Z48 (12)) (2 3 2 3 4 1)
         (0 1 3 5 7 8) (6-Z26 (12)) (2 3 2 3 4 1))
        
        ((0 1 3 4 6 9) (6-27) (2 2 5 2 2 2))
        
        ((0 1 3 5 6 9) (6-Z28 (12)) (2 2 4 3 2 2)
         (0 1 3 4 7 9) (6-Z49 (12)) (2 2 4 3 2 2))
        ((0 1 3 4 7 9) (6-Z49 (12)) (2 2 4 3 2 2)
         (0 1 3 5 6 9) (6-Z28 (12)) (2 2 4 3 2 2))
        
        ((0 1 3 6 8 9) (6-Z29 (12)) (2 2 4 2 3 2)
         (0 1 4 6 7 9) (6-Z50 (12)) (2 2 4 2 3 2))
        ((0 1 4 6 7 9) (6-Z50 (12)) (2 2 4 2 3 2)
         (0 1 3 6 8 9) (6-Z29 (12)) (2 2 4 2 3 2))
        
        ((0 1 3 6 7 9) (6-30 (12)) (2 2 4 2 2 3))
        
        ((0 1 3 5 8 9) (6-31) (2 2 3 4 3 1))
        
        ((0 2 4 5 7 9) (6-32 (12)) (1 4 3 2 5 0))
        
        ((0 2 3 5 7 9) (6-33) (1 4 3 2 4 1))
        
        ((0 1 3 5 7 9) (6-34) (1 4 2 4 2 2))
        
        ((0 2 4 6 8 10) (6-35 (2)) (0 6 0 6 0 3))
        
        ))
;__________________________________________________________________