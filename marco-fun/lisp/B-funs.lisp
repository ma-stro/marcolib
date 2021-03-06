;**************************************************************************
;-------| MARCO STROPPA: TRAIETTORIA
;-------| This file is: infiles/Funs/Lisp/B-funs.lisp
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
; INTERPOLATING ENVELOPES: 3 FORMANTS
; ASSOCIATED CONTAINER: f-B-db
; FUNCTION NAME: F-B#-x, REGION: 2001-2500 (from 0)
; FUNCTION NAME: F1-B#-x, REGION: 2501-3000 (from 1)
;--------------------------------------------------------------------------
(setf f-B-db (make_tbl))

;--------------------------------------------------------------------------
; REGION: 2001-2500 (from 0)
; PEAKS AT 0.1 / 0.5 / 0.9
(insert_tbl f-B-db
            'f-b1-A
            '(make_ve
              (make_fun '(0 0  1 .11  .4 .3  0 1))
              2001))
(insert_tbl f-B-db
            'f-b1-B
            '(make_ve
              (make_fun '(0 0  .4 .3  1 .5  .4 .7  0 1))
              2002))
(insert_tbl f-B-db
            'f-b1-C
            '(make_ve
              (make_fun '(0 0  .4 .68  1 .9  0 1))
              2003))

(insert_tbl f-B-db
            'f-b2-A
            '(make_ve
              (make_fun '(0 0  1 .101  .4 .5  0 1))
              2004))
(insert_tbl f-B-db
            'f-b2-B
            '(make_ve
              (make_fun '(0 0  .4 .11  1 .5  .4 .9  0 1))
              2005))
(insert_tbl f-B-db
            'f-b2-C
            '(make_ve
              (make_fun '(0 0  .4 .5  1 .9  0 1))
              2006))

(insert_tbl f-B-db
            'f-b3-A
            '(make_ve
              (make_fun '(0 0  1 .101  .4 .2  0 1))
              2007))
(insert_tbl f-B-db
            'f-b3-B
            '(make_ve
              (make_fun '(0 0  .4 .4  1 .5  .4 .6  0 1))
              2008))
(insert_tbl f-B-db
            'f-b3-C
            '(make_ve
              (make_fun '(0 0  .4 .8  1 .9  0 1))
              2009))

(insert_tbl f-B-db
            'f-b4-A
            '(make_ve
              (make_fun '(0 0  1 .09  .4 .4  0 1))
              2010))
(insert_tbl f-B-db
            'f-b4-B
            '(make_ve
              (make_fun '(0 0  .2 .3  1 .5  .2 .7  0 1))
              2011))
(insert_tbl f-B-db
            'f-b4-C
            '(make_ve
              (make_fun '(0 0  .4 .6  1 .9  0 1))
              2012))
;-----------------------------------------------------------------------------
; PEAKS AT 0.2 / 0.4 / 0.8
(insert_tbl f-B-db
            'f-b5-A
            '(make_ve
              (make_fun '(0 0  1 .2  .4 .3  0 1))
              2013))
(insert_tbl f-B-db
            'f-b5-B
            '(make_ve
              (make_fun '(0 0  .4 .3  1 .4  .4 .5  0 1))
              2014))
(insert_tbl f-B-db
            'f-b5-C
            '(make_ve
              (make_fun '(0 0  .4 .72  1 .8  0 1))
              2015))

(insert_tbl f-B-db
            'f-b6-A
            '(make_ve
              (make_fun '(0 0  1 .2  .4 .4  0 1))
              2016))
(insert_tbl f-B-db
            'f-b6-B
            '(make_ve
              (make_fun '(0 0  .4 .3  1 .4 .4 .6  0 1))
              2017))
(insert_tbl f-B-db
            'f-b6-C
            '(make_ve
              (make_fun '(0 0  .4 .6  1 .8  0 1))
              2018))
;-----------------------------------------------------------------------------
; PEAKS AT 0.2 / 0.6 / 0.8
(insert_tbl f-B-db
            'f-b7-A
            '(make_ve
              (make_fun '(0 0  1 .2  .4 .5  0 1))
              2019))
(insert_tbl f-B-db
            'f-b7-B
            '(make_ve
              (make_fun '(0 0  .4 .4  1 .6  .2 .8  0 1))
              2020))
(insert_tbl f-B-db
            'f-b7-C
            '(make_ve
              (make_fun '(0 0  .4 .5  1 .8  0 1))
              2021))

(insert_tbl f-B-db
            'f-b8-A
            '(make_ve
              (make_fun '(0 0  1 .2  .4 .6  0 1))
              2022))
(insert_tbl f-B-db
            'f-b8-B
            '(make_ve
              (make_fun '(0 0  .2 .3  1 .6  .4 .71  0 1))
              2023))
(insert_tbl f-B-db
            'f-b8-C
            '(make_ve
              (make_fun '(0 0  .4 .4  1 .8  0 1))
              2024))
;-----------------------------------------------------------------------------
; PEAKS AT 0.3 / 0.6 / 0.7
(insert_tbl f-B-db
            'f-b9-A
            '(make_ve
              (make_fun '(0 0  .4 .2  1 .3  .4 .4  0 1))
              2025))
(insert_tbl f-B-db
            'f-b9-B
            '(make_ve
              (make_fun '(0 0  .4 .52  1 .6  .4 .71  0 1))
              2026))
(insert_tbl f-B-db
            'f-b9-C
            '(make_ve
              (make_fun '(0 0  .4 .6  1 .71  .4 .8  0 1))
              2027))
;-----------------------------------------------------------------------------
; PEAKS AT 0.3 / 0.5 / 0.7
(insert_tbl f-B-db
            'f-b10-A
            '(make_ve
              (make_fun '(0 0  .4 .11  1 .3  .4 .4  0 1))
              2028))
(insert_tbl f-B-db
            'f-b10-B
            '(make_ve
              (make_fun '(0 0  .4 .3  1 .5  .4 .701  0 1))
              2029))
(insert_tbl f-B-db
            'f-b10-C
            '(make_ve
              (make_fun '(0 0  .4 .5  1 .71  0 1))
              2030))
;-----------------------------------------------------------------------------
; PEAKS AT 0.3 / 0.4 / 0.7
(insert_tbl f-B-db
            'f-b11-A
            '(make_ve
              (make_fun '(0 0  .4 .2  1 .3  .4 .5  0 1))
              2031))
(insert_tbl f-B-db
            'f-b11-B
            '(make_ve
              (make_fun '(0 0  .4 .2  1 .4  .4 .5  0 1))
              2032))
(insert_tbl f-B-db
            'f-b11-C
            '(make_ve
              (make_fun '(0 0  .17 .3  .41 .62  1 .69  0 1))
              2033))
;-----------------------------------------------------------------------------
; PEAKS AT 0.4 / 0.5 / 0.6
(insert_tbl f-B-db
            'f-b12-A
            '(make_ve
              (make_fun '(0 0  .4 .3  1 .4  .4 .6  0 1))
              2034))
(insert_tbl f-B-db
            'f-b12-B
            '(make_ve
              (make_fun '(0 0  .4 .4  1 .5  .4 .6  0 1))
              2035))
(insert_tbl f-B-db
            'f-b12-C
            '(make_ve
              (make_fun '(0 0  .2 .4  .4 .5  1 .6  .4 .69  .11 .83  0 1))
              2036))
;*****************************************************************************
;-----------------------------------------------------------------------------
; REGION: 2501-3000 (from 1)
;*****************************************************************************
