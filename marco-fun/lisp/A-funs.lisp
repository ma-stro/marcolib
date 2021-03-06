;**************************************************************************
;-------| MARCO STROPPA: TRAIETTORIA
;-------| This file is: infiles/Funs/Lisp/A-funs.lisp
;-------| Version V1.0: Feb 24, 2010
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

(in-package :cr)

;--------------------------------------------------------------------------
; INTERPOLATING ENVELOPES: 2 FORMANTS
; ASSOCIATED CONTAINER: f-A-db
; FUNCTION NAME: F-A#-x, REGION: 1001-1500 (from 0)
; FUNCTION NAME: F1-A#-x, REGION: 1501-2000 (from 1)
;--------------------------------------------------------------------------
(setf f-A-db (make_tbl))

;--------------------------------------------------------------------------
; REGION: 1001-1500 (from 0)
; PEAKS AT .2 AND .7
(insert_tbl f-A-db
            'f-a1-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  1 .18  .2 .51  0 1)))
            1001))
(insert_tbl f-A-db
            'f-a1-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .18 .54  1 .711  0 1)))
            1002))

(insert_tbl f-A-db
            'f-a2-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .581 .05  .17 .09  1 .2  .39 .3 .73 .4
                            .22 .571  .47 .681  .13 .8  0 1)))
            1003))
(insert_tbl f-A-db
            'f-a2-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .08 .2  .41 .25  .15 .38  .5 .43
                            .33 .55  1 .701  .21 .79  .46 .841  .11 .92  0 1)))
            1004))

(insert_tbl f-A-db
            'f-a3-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .61 .03  .19 .085  .78 .12  .37 .16
                            1 .2  .31 .32  .55 .46  .18 .571  0 1)))
            1005))
(insert_tbl f-A-db
            'f-a3-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .38 .101  .11 .18  .25 .26  .08 .4
                                   .21 .6  1 .701  .101 .85  .25 .91  0 1)))
            1006))
;--------------------------------------------------------------------------
; PEAKS AT .3 AND .6
(insert_tbl f-A-db
            'f-a4-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  1 .3  .22 .63  0 1)))
            1007))
(insert_tbl f-A-db
            'f-a4-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .24 .38  1 .6  .28 .76  0 1)))
            1008))

(insert_tbl f-A-db
            'f-a5-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .55 .072  .23 .185  1 .3  .21 .53
                            .33 .681  .05 .841  0 1)))
            1009))
(insert_tbl f-A-db
            'f-a5-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .23 .22  .17 .31  .28 .43  .25 .5
                            1 .63  .37 .74  .55 .78  .101 .86  0 1)))
            1010))

(insert_tbl f-A-db
            'f-a6-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0   .681 .061  .21 .12  .55 .18  .2 .24  1 .31
                            .28 .48  .49 .53  .11 .701  0 1)))
            1011))
(insert_tbl f-A-db
            'f-a6-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .08 .14  .33 .22  .12 .31  .26 .4  .17 .45
                            1 .63  .28 .76  .44 .811  .101 .88  0 1)))
            1012))
;--------------------------------------------------------------------------
; PEAKS AT .4 AND .5
(insert_tbl f-A-db
            'f-a7-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .3 .25  1 .4  .4 .55  .08 .72  0 1)))
            1013))
(insert_tbl f-A-db
            'f-a7-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .25 .33  1 .5  .3 .691  0 1)))
            1014))

(insert_tbl f-A-db
            'f-a8-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .701 .053  .2 .137  .48 .2  .2 .243  .8 .288
                            .35 .34  1 .4  .19 .62  .44 .711  .101 .811  0 1)))
            1015))
(insert_tbl f-A-db
            'f-a8-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0 .33 .05  .101 .14  .21 .22  .072 .3  .38 .37
                            .11 .42  1 .5  .15 .66  .73 .701  .101 .79
                            .43 .874  .15 .903  0 1)))
            1016))
;--------------------------------------------------------------------------
; PEAKS AT .1 AND .9
(insert_tbl f-A-db
            'f-a9-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  1 .12  .2 .34  0 1)))
            1017))
(insert_tbl f-A-db
            'f-a9-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .2 .74  1 .9  0 1)))
            1018))

(insert_tbl f-A-db
            'f-a10-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .8 .029  .4 .081  1 .107 .2 .231
                            .581 .287  .12 .37  .23 .48  .09 .59  0 1)))
            1019))
(insert_tbl f-A-db
            'f-a10-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(0 0  .2 .3  .53 .337  .15 .42  .681 .517
                            .18 .5718  .4 .622  .17 .73  1 .87  .15 .914  .37 .9411  0 1)))
            1020))
;*****************************************************************************
; REGION: 1501-2000 (from 1)
; PEAKS AT .2 AND .7
(insert_tbl f-A-db
            'f1-a1-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  1 .18  .2 .51  0 1)))
            1501))
(insert_tbl f-A-db
            'f1-a1-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .18 .54  1 .711  0 1)))
            1502))

(insert_tbl f-A-db
            'f1-a2-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .581 .05  .17 .09  1 .2  .39 .3 .73 .4
                            .22 .571  .47 .681  .13 .8  0 1)))
            1503))
(insert_tbl f-A-db
            'f1-a2-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .08 .2  .41 .25  .15 .38  .5 .43
                            .33 .55  1 .701  .21 .79  .46 .841  .11 .92  0 1)))
            1504))

(insert_tbl f-A-db
            'f1-a3-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .61 .03  .19 .085  .78 .12  .37 .16
                            1 .2  .31 .32  .55 .46  .18 .571  0 1)))
            1505))
(insert_tbl f-A-db
            'f1-a3-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .38 .101  .11 .18  .25 .26  .08 .4
                                   .21 .6  1 .701  .101 .85  .25 .91  0 1)))
            1506))
;--------------------------------------------------------------------------
; PEAKS AT .3 AND .6
(insert_tbl f-A-db
            'f1-a4-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  1 .3  .22 .63  0 1)))
            1507))
(insert_tbl f-A-db
            'f1-a4-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .24 .38  1 .6  .28 .76  0 1)))
            1508))

(insert_tbl f-A-db
            'f1-a5-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .55 .072  .23 .185  1 .3  .21 .53
                            .33 .681  .05 .841  0 1)))
            1509))
(insert_tbl f-A-db
            'f1-a5-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .23 .22  .17 .31  .28 .43  .25 .5
                            1 .63  .37 .74  .55 .78  .101 .86  0 1)))
            1510))

(insert_tbl f-A-db
            'f1-a6-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0   .681 .061  .21 .12  .55 .18  .2 .24  1 .31
                            .28 .48  .49 .53  .11 .701  0 1)))
            1511))
(insert_tbl f-A-db
            'f1-a6-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .08 .14  .33 .22  .12 .31  .26 .4  .17 .45
                            1 .63  .28 .76  .44 .811  .101 .88  0 1)))
            1512))
;--------------------------------------------------------------------------
; PEAKS AT .4 AND .5
(insert_tbl f-A-db
            'f1-a7-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .3 .25  1 .4  .4 .55  .08 .72  0 1)))
            1513))
(insert_tbl f-A-db
            'f1-a7-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .25 .33  1 .5  .3 .691  0 1)))
            1514))

(insert_tbl f-A-db
            'f1-a8-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .701 .053  .2 .137  .48 .2  .2 .243  .8 .288
                            .35 .34  1 .4  .19 .62  .44 .711  .101 .811  0 1)))
            1515))
(insert_tbl f-A-db
            'f1-a8-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0 .33 .05  .101 .14  .21 .22  .072 .3  .38 .37
                            .11 .42  1 .5  .15 .66  .73 .701  .101 .79
                            .43 .874  .15 .903  0 1)))
            1516))
;--------------------------------------------------------------------------
; PEAKS AT .1 AND .9
(insert_tbl f-A-db
            'f1-a9-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  1 .12  .2 .34  0 1)))
            1517))
(insert_tbl f-A-db
            'f1-a9-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .2 .74  1 .9  0 1)))
            1518))

(insert_tbl f-A-db
            'f1-a10-A
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .8 .029  .4 .081  1 .107 .2 .231
                            .581 .287  .12 .37  .23 .48  .09 .59  0 1)))
            1519))
(insert_tbl f-A-db
            'f1-a10-B
            '(make_ve
              (make_fun (om::om* 1000.0 '(1 0  .2 .3  .53 .337  .15 .42  .681 .517
                            .18 .5718  .4 .622  .17 .73  1 .87  .15 .914  .37 .9411  0 1)))
            1520))
;**************************************************************************
;(mapcar 'eval (lels_tbl f-A-db))

