;**************************************************************************
;-------| MARCO STROPPA: TRAIETTORIA
;-------| This file is: infiles/Funs/Lisp/C-funs.lisp
;-------| Version V1.0: Feb 26, 2010
;-------| By Marco Stroppa
;**************************************************************************
; NB: original file in LeLisp lost
;     new structure of the data bases

; THIS FILE CONTAINS THE DATA BASE OF FUNCTIONS USED IN TRAIETTORIA
; BASED ON THE ORIGINAL 1982 DATA

; DATA BASE OF FUNCTIONS:
;   CONTAINER: A (f-A-db)
;   KNOWLEDGE BASE: VECTOR kb-A-#, ETC.
; DATA: VIRTUAL ENVELOPE WITH A FUN AND A GEN NUMBER

; TO GENERATE A CSOUND GEN FILE READABLE IN OMCHROMA DO:
;   (save_ve <container-name> <full-path> [<synth>])

(in-package cr)

;--------------------------------------------------------------------------
; INTERPOLATING ENVELOPES: 4 FORMANTS
; ASSOCIATED CONTAINER: f-C-db
; FUNCTION NAME: F-C#-x, REGION: 3001-3500 (from 0)
; FUNCTION NAME: F1-C#-x, REGION: 3501-4000 (from 1)
;--------------------------------------------------------------------------
(setf f-C-db (make_tbl))

;--------------------------------------------------------------------------
; REGION: 3001-3500 (from 0)
; PEAKS AT 0.1 / 0.3 / 0.6 / 0.8
(insert_tbl f-C-db
            'f-c1-A
            '(make_ve
              (make_fun '(0 0  1 .09  .2 .3  0 1))
              3001))
(insert_tbl f-C-db
            'f-c1-B
            '(make_ve
              (make_fun '(0 0  .2 .2  1 .3  .2 .4  0 1))
              3002))
(insert_tbl f-C-db
            'f-c1-C
            '(make_ve
              (make_fun '(0 0  .17 .15  1 .6  .2 .71  0 1))
              3003))
(insert_tbl f-C-db
            'f-c1-D
            '(make_ve
              (make_fun '(0 0  .2 .7  1 .8  .2 .9  0 1))
              3004))

(insert_tbl f-C-db
            'f-c2-A
            '(make_ve
              (make_fun '(0 0  1 .09  .2 .4  0 1))
              3005))
(insert_tbl f-C-db
            'f-c2-B
            '(make_ve
              (make_fun '(0 0  .2 .2  1 .3  .4 .4  .2 .6  0 1))
              3006))
(insert_tbl f-C-db
            'f-c2-C
            '(make_ve
              (make_fun '(0 0  .2 .4  1 .6  .2 .8  0 1))
              3007))
(insert_tbl f-C-db
            'f-c2-D
            '(make_ve
              (make_fun '(0 0  .2 .6  1 .8  0 1))
              3008))
;--------------------------------------------------------------------------
; PEAKS AT 0.1 / 0.4 / 0.6 / 0.8
(insert_tbl f-C-db
            'f-c3-A
            '(make_ve
              (make_fun '(0 0  1 .09  .4 .2  .13 .5  0 1))
              3009))
(insert_tbl f-C-db
            'f-c3-B
            '(make_ve
              (make_fun '(0 0  .2 .3  1 .4  .2 .5  0 1))
              3010))
(insert_tbl f-C-db
            'f-c3-C
            '(make_ve
              (make_fun '(0 0  .18 .3  .4 .5  1 .6  .2 .7  0 1))
              3011))
(insert_tbl f-C-db
            'f-c3-D
            '(make_ve
              (make_fun '(0 0  .2 .5  .4 .7  1 .8  .3 .9  0 1))
              3012))

(insert_tbl f-C-db
            'f-c4-A
            '(make_ve
              (make_fun '(0 0  1 .09  .2 .2  .072 .5  0 1))
              3013))
(insert_tbl f-C-db
            'f-c4-B
            '(make_ve
              (make_fun '(0 0  .2 .23  .4 .31  1 .4 .2 .5  .05 .75  0 1))
              3014))
(insert_tbl f-C-db
            'f-c4-C
            '(make_ve
              (make_fun '(0 0  .2 .4  .4 .5  1 .6 .2 .68  .05 .85  0 1))
              3015))
(insert_tbl f-C-db
            'f-c4-D
            '(make_ve
              (make_fun '(0 0  .2 .6  .3 .7  1 .8  .5 .85  0 1))
              3016))
;--------------------------------------------------------------------------
; PEAKS AT 0.2 / 0.4 / 0.7 / 0.9
(insert_tbl f-C-db
            'f-c5-A
            '(make_ve
              (make_fun '(0 0  .2 .11  1 .2  .2 .3  0 1))
              3017))
(insert_tbl f-C-db
            'f-c5-B
            '(make_ve
              (make_fun '(0 0  .2 .3  1 .4  .4 .5  .15 .67  .03 .88  0 1))
              3018))
(insert_tbl f-C-db
            'f-c5-C
            '(make_ve
              (make_fun '(0 0  .2 .6  1 .71  .2 .8  0 1))
              3019))
(insert_tbl f-C-db
            'f-c5-D
            '(make_ve
              (make_fun '(0 0  .2 .8  1 .9  0 1))
              3020))

;--------------------------------------------------------------------------
; PEAKS AT 0.2 / 0.5 / 0.7 / 0.9
(insert_tbl f-C-db
            'f-c6-A
            '(make_ve
              (make_fun '(0 0  .2 .11  1 .2  .4 .3  .2 .5  0 1))
              3021))
(insert_tbl f-C-db
            'f-c6-B
            '(make_ve
              (make_fun '(0 0  .2 .4  1 .5  .3 .6  0 1))
              3022))
(insert_tbl f-C-db
            'f-c6-C
            '(make_ve
              (make_fun '(0 0  .2 .52  1 .71  .2 .79  0 1))
              3023))
(insert_tbl f-C-db
            'f-c6-D
            '(make_ve
              (make_fun '(0 0  .2 .69  1 .9  0 1))
              3024))


(insert_tbl f-C-db
            'f-c7-A
            '(make_ve
              (make_fun '(0 0  1 .2  .2 .3  0 1))
              3025))
(insert_tbl f-C-db
            'f-c7-B
            '(make_ve
              (make_fun '(0 0  .2 .3  .4 .4  1 .5  .2 .6  .061 .79  0 1))
              3026))
(insert_tbl f-C-db
            'f-c7-C
            '(make_ve
              (make_fun '(0 0  .2 .4  .4 .6  1 .701  0 1))
              3027))
(insert_tbl f-C-db
            'f-c7-D
            '(make_ve
              (make_fun '(0 0  .2 .5  .4 .8 1 .9  0 1))
              3028))


(insert_tbl f-C-db
            'f-c8-A
            '(make_ve
              (make_fun '(0 0  1 .2  .4 .3  0 1))
              3029))
(insert_tbl f-C-db
            'f-c8-B
            '(make_ve
              (make_fun '(0 0  .4 .4  1 .5  .2 .6  .05 .8  0 1))
              3030))
(insert_tbl f-C-db
            'f-c8-C
            '(make_ve
              (make_fun '(0 0  .18 .4  .4 .6  1 .701  .4 .8  .074 .94  0 1))
              3031))
(insert_tbl f-C-db
            'f-c8-D
            '(make_ve
              (make_fun '(0 0  .2 .6  .4 .8  1 .9  0 1))
              3032))

;**************************************************************************
;--------------------------------------------------------------------------
; REGION: 3501-4000 (from 1)
;**************************************************************************
