;**************************************************************************
;-------| MARCO STROPPA: TRAIETTORIA
;-------| This file is: infiles/Funs/Lisp/D-funs.lisp
;-------| Version V1.0: Feb 26, 2010
;-------| By Marco Stroppa
;**************************************************************************
; NB: original file in LeLisp lost
;     new structure of the data bases


(in-package cr)

;--------------------------------------------------------------------------
; INTERPOLATING ENVELOPES: 5 FORMANTS
; ASSOCIATED CONTAINER: f-D-db
; FUNCTION NAME: F-D#-x, REGION: 4001-4500 (from 0)
; FUNCTION NAME: F1-D#-x, REGION: 4501-5000 (from 1)
;--------------------------------------------------------------------------
(setf f-D-db (make_tbl))

;--------------------------------------------------------------------------
; REGION: 4001-4500 (from 0)
; PEAKS AT 0.05 / 0.25 / 0.45 / 0.65 / 0.85
(insert_tbl f-D-db
            'f-d1-A
            '(make_ve
              (make_fun '(0 0  1000 50  200 110  0 1000.0))
              4001))
(insert_tbl f-D-db
            'f-d1-B
            '(make_ve
              (make_fun '(0 0  180 200  1000 250  200 300  0 1000))
              4002))
(insert_tbl f-D-db
            'f-d1-C
            '(make_ve
              (make_fun '(0 0  170 400  1000 450  200 500  0 1000))
              4003))
(insert_tbl f-D-db
            'f-d1-D
            '(make_ve
              (make_fun '(0 0  150 600  1 650  200 710  0 1000))
              4004))
(insert_tbl f-D-db
            'f-d1-E
            '(make_ve
              (make_fun '(0 0  200 800  1000 850  200 900  0 1000))
              4005))

(insert_tbl f-D-db
            'f-d2-A
            '(make_ve
              (make_fun '(0 0  1000 61  200 200  0 1000))
              4006))
(insert_tbl f-D-db
            'f-d2-B
            '(make_ve
              (make_fun '(0 0  200 150  1000 250  200 350  0 1000))
              4007))
(insert_tbl f-D-db
            'f-d2-C
            '(make_ve
              (make_fun '(0 0  200 300  1000 450  200 600  0 1000))
              4008))
(insert_tbl f-D-db
            'f-d2-D
            '(make_ve
              (make_fun '(0 0  200 500  1000 650  200 800  0 1000))
              4009))
(insert_tbl f-D-db
            'f-d2-E
            '(make_ve
              (make_fun '(0 0  200 710  1000 850  0 1000))
              4010))

(insert_tbl f-D-db
            'f-d3-A
            '(make_ve
              (make_fun '(0 0  1000 50  400 110  90 300  0 1000))
              4011))
(insert_tbl f-D-db
            'f-d3-B
            '(make_ve
              (make_fun '(0 0  .072 .11  .2 .2  1 .25  .4 .3  .11 .5  0 1))
              4012))
(insert_tbl f-D-db
            'f-d3-C
            '(make_ve
              (make_fun '(0 0  .08 .2  .4 .4  1 .45  .4 .5  .11 .69  0 1))
              4013))
(insert_tbl f-D-db
            'f-d3-D
            '(make_ve
              (make_fun '(0 0  .09 .4  .4 .6  1 .65  .27 .73  .061 .87  0 1))
              4014))
(insert_tbl f-D-db
            'f-d3-E
            '(make_ve
              (make_fun '(0 0  .08  3  .2 .6  .4 .8  1 .85  .4 .9  0 1))
              4015))

(insert_tbl f-D-db
            'f-d4-A
            '(make_ve
              (make_fun '(0 0  1 .04  .4 .16  .11 .5  0 1))
              4016))
(insert_tbl f-D-db
            'f-d4-B
            '(make_ve
              (make_fun '(0 0  .4 .2  1 .25  .4 .35  .11 .6  0 1))
              4017))
(insert_tbl f-D-db
            'f-d4-C
            '(make_ve
              (make_fun '(0 0  .08 .31  1 .45  .6 .5  .2 .67  0 1))
              4018))
(insert_tbl f-D-db
            'f-d4-D
            '(make_ve
              (make_fun '(0 0  .11 .3  .28 .5  .52 .6  1 .64  .6 .71  .21 .83  0 1))
              4019))
(insert_tbl f-D-db
            'f-d4-E
            '(make_ve
              (make_fun '(0 0  .11 .37  .21 .62  .4 .73  1 .85  0 1))
              4020))


;--------------------------------------------------------------------------
; PEAKS AT 0.15 / 0.35 / 0.55 / 0.75 / 0.95
(insert_tbl f-D-db
            'f-d5-A
            '(make_ve
              (make_fun '(0 0  .2 .09  1 .15  .2 .2  0 1))
              4021))
(insert_tbl f-D-db
            'f-d5-B
            '(make_ve
              (make_fun '(0 0  .2 .3  1 .35  .2 .4  0 1))
              4022))
(insert_tbl f-D-db
            'f-d5-C
            '(make_ve
              (make_fun '(0 0  .2 .5  1 .55  .2 .6  0 1))
              4023))
(insert_tbl f-D-db
            'f-d5-D
            '(make_ve
              (make_fun '(0 0  .2 .7  1 .75  .2 .8  0 1))
              4024))
(insert_tbl f-D-db
            'f-d5-E
            '(make_ve
              (make_fun '(0 0  .2 .9  1 .95  0 1))
              4025))

(insert_tbl f-D-db
            'f-d6-A
            '(make_ve
              (make_fun '(0 0  1 .15  .2 .3  0 1))
              4026))
(insert_tbl f-D-db
            'f-d6-B
            '(make_ve
              (make_fun '(0 0  .2 .2  1 .35  .2 .5  0 1))
              4027))
(insert_tbl f-D-db
            'f-d6-C
            '(make_ve
              (make_fun '(0 0  .2 .4  1 .55  .2 .71  0 1))
              4028))
(insert_tbl f-D-db
            'f-d6-D
            '(make_ve
              (make_fun '(0 0  .2 .6  1 .75  .2 .9  0 1))
              4029))
(insert_tbl f-D-db
            'f-d6-E
            '(make_ve
              (make_fun '(0 0  .2 .8  1 .95  0 1))
              4030))

(insert_tbl f-D-db
            'f-d7-A
            '(make_ve
              (make_fun '(0 0  .4 .11  1 .15  .4 .25  .13 .5  0 1))
              4031))
(insert_tbl f-D-db
            'f-d7-B
            '(make_ve
              (make_fun '(0 0  .08 .2  .4 .3  1 .35  .2 .54  .061 .71  0 1))
              4032))
(insert_tbl f-D-db
            'f-d7-C
            '(make_ve
              (make_fun '(0 0   0 1))
              4033))
(insert_tbl f-D-db
            'f-d7-D
            '(make_ve
              (make_fun '(0 0   0 1))
              4034))
(insert_tbl f-D-db
            'f-d7-E
            '(make_ve
              (make_fun '(0 0   0 1))
              4035))

(insert_tbl f-D-db
            'f-d8-A
            '(make_ve
              (make_fun '(0 0   0 1))
              4036))
(insert_tbl f-D-db
            'f-d8-B
            '(make_ve
              (make_fun '(0 0   0 1))
              4037))
(insert_tbl f-D-db
            'f-d8-C
            '(make_ve
              (make_fun '(0 0   0 1))
              4038))
(insert_tbl f-D-db
            'f-d8-D
            '(make_ve
              (make_fun '(0 0   0 1))
              4039))
(insert_tbl f-D-db
            'f-d8-E
            '(make_ve
              (make_fun '(0 0   0 1))
              4040))

;**************************************************************************
;--------------------------------------------------------------------------
; REGION: 4501-5000 (from 1)
;**************************************************************************
