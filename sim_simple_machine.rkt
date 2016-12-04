#lang racket
(require "TM.rkt")
(machine '(@@A_R_R_A_R_R_B_R_R_A_L_L_A_: 0 start)
         '(1000 0 1)
         '((start Any (R R R Px) setup-y)
           (setup-y !: R setup-y)
           (setup-y : (R R Py) goto-first-op)

           (goto-first-op !@ L goto-first-op)
           (goto-first-op @ R do-op)

           (do-op A N print-0)
           (do-op B N print-1)
           (do-op L N move-L)
           (do-op R N move-R)
           
           (print-0 !y R print-0)
           (print-0 y (L P0) goto-next-op)
           (print-1 !y R print-1)
           (print-1 y (L P1) goto-next-op)

           (move-L !y R move-L)
           (move-L y (P_ L L Py) goto-next-op)
           (move-R !y R move-R)
           (move-R y (P_ R R Py) goto-next-op)

           (goto-next-op !x L goto-next-op)
           (goto-next-op x (P_ R) goto-next-op2)
           ;; tests if we are at the end of the intput program
           (goto-next-op2 !: (R Px L) do-op)
           (goto-next-op2 : N halt)))