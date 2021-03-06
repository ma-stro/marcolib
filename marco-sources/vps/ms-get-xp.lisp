(in-package :cr)
(defun get-xp (wanted-fq actual-fq)
"return the transposition interval in midicents to achieve the wanted frequency from the actual frequency"
  (round
   (* (first
       (ratio->semitones
        (fq->ratio (list actual-fq wanted-fq)))) 100.0)))

;(get-xp 440.0 880.0)
