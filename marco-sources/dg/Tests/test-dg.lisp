; test DV
(setf lp 20)

(l-val lp '(1 2 3))

(lp lp '(1 2 3 4 5))

(bkwd-lp lp '(1 2 3 4 5))

(rept lp 'hello)

(rept-lp lp '(1 2 3 (hello serge)))

(bkwd-rept-lp lp '(1 2 3 (hello serge)))

; test DF
(setf lpp (make_fun '(0 0 1 1)))
(setf lppp (make_fun '(0 0  1 1  -1 2  .5 4  3 5  -2 7)))

(lkp lp lppp)

(fix-lkp lp lpp)

(lkpr lp lpp 10 20)
(lkpr lp lppp 10 20)

(lkpr lp lpp 10 20 .111111)

(fix-lkpr lp lpp 10 20 1000)
(fix-lkpr lp lpp 10 20)
(setf lp 21)
(fix-lkpr lp lpp 10 30)
(lkpr lp lpp 10.0 30.0)

; test DPV
(setf lp 11)
(setf pl #(10 11 12 13 14 15 16 17 18 19))
(setf pl1 #(a b c d e f g h i))

(p-l-val lp '(-1 0 1 2 3 4 5 123) pl)
(pi-l-val lp '(-1 0 1.3 2.6 3 4.1 5 123) pl)

(pi-mod-l-val lp '(0 1 -1 2 3 19 -13) pl)

(p-lp lp '(0 1 -1 2 3 19 -13 0 1 2 3 4 5) pl)
(setf ccl '(0 1 -1 2 3 19 -13 0 1 2 3 4 5))
(setf ccl1 '(-2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13))
(setf ccl2 '(0 1.1 -1.2 2.3 3.4 19.5 -13.6 0.7 1.8 2.9 3.8 4.7 5.6))
(setf ccl3 '(0 1 2 3))
(setf ccl3a '(0 1 2 3 (6 7 8)))
(p-lp lp ccl3 pl)

(setf lp 20)
(setf ppp1 '(0.9 1.1 2.2 3.3))
(setf ppp1a '(0.9 1.1 2.2 3.3 (4.4 5.5 6.6)))
(p-lp lp ccl3 pl 3 8)
(pi-lp lp ppp1 pl)

(p-bkwd-lp lp ccl3 pl)
(p-bkwd-lp lp ccl pl 2 5)
(p-bkwd-lp lp ccl1 pl 2 5 1)

(pi-bkwd-lp lp ppp1 pl)
(setf lp 13)
(pi-bkwd-lp lp ccl2 pl)
(pi-bkwd-lp lp ccl2 pl 2 5)


(p-rept-lp lp ccl3a pl)
(p-rept-lp lp ccl3a pl 2 5)

(pi-rept-lp lp ppp1a pl)
(pi-rept-lp lp ccl3a pl 2 5)


(p-bkwd-rept-lp lp ccl3a pl)
(pi-bkwd-rept-lp lp ppp1a pl)


; DPV: step functions
(setf lp 20)
(p-rept lp pl)
(p-rept lp pl1)
(p-rept lp pl 4 8)
(p-rept lp pl 8 4)
(p-rept lp pl 8 8)
(p-rept lp pl 40 23)

(setf lp 50)
(pi-rept lp pl)
(pi-rept lp pl1)

(p-stp-rept lp 2 pl)
(p-stp-rept lp 0.5 pl)
(p-stp-rept lp -2 pl)
(p-stp-rept lp 0.5 pl 2 6)

(pi-stp-rept lp 0.7 pl)
(pi-stp-rept lp 0.5 pl 2 6)

(p-mod-rept lp 0.5 pl)
(p-mod-rept lp 0.5 pl 2 5)
(p-mod-rept lp 0.5 pl 2 5 3)

(pi-mod-rept lp 0.5 pl)
(pi-mod-rept lp 0.5 pl 2 5)
(pi-mod-rept lp 0.5 pl 2 5 3)


(p-bkwd-mod-rept lp 0.5 pl)
(p-bkwd-mod-rept lp 0.5 pl 2 5)
(p-bkwd-mod-rept lp 0.5 pl 2 5 3)

(pi-bkwd-mod-rept lp 0.5 pl)
(pi-bkwd-mod-rept lp 0.5 pl 2 5)
(pi-bkwd-mod-rept lp 0.5 pl 2 5 3)



; test DPF
(setf lp 10)
(setf f1 (make_fun '(0 0  1 1)) )
(setf f2 (make_fun '(1 0  0 1)) )

(p-lkp lp f1 pl)
(p-lkp lp f1 pl 3 8)

(pi-lkp 21.0 f1 pl)
(pi-lkp 21.0 f1 pl 2 6)

; test SP & Co.
(setf ko '(1 3 5 6 7 11))

(sp (pch 'la4) 10)
(sp (pch 'la4) 10 0.01)
(sp (pch '100) ko 0.1)

(spsh 100 10 (xp-int-1 7))

(spst 100 10 2.0)
(spst 100 10 1.0)
(spst 100 10 1.7)
(spst 100 10 0.99)
(spst 100 10 2.06)
(spst 100 10 2.06 0.1)

(xp-int 12)
(xp-int-1 7)
(xp-int-1 12)

(spsht 100 10 0.1 2.0)
(spsht 100 10 0.1 2.2)
(spsht 100 10 0.1 1.01)



; test sp-dg.lisp

(chk-dur 10 1 14)
(chk-dur 10 1 4)
(chk-dur 10 10 14)

(setf f-11 11)
(setf f-12 12)
(setf f-13 13)

(setf q1 #(f-11 f-12 f-13))
(setf q2 #('f-11 'f-12 'f-13))
(p-ve 4 q1)
(p-ve 1 q1 2 3)