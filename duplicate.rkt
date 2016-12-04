#lang racket
(require "TM.rkt")
(machine '(@@1_0_1_1_1_1_1_0_1_0_: 0 start)
         '(1000 0 1)
         '((start Any (R R R Px) setup-y)
           (setup-y !: R setup-y)
           (setup-y : (R R Py) goto-first-bit)

           (goto-first-bit !@ L goto-first-bit)
           (goto-first-bit @ R copy-bit)

           (copy-bit 0 N copy-bit-0)
           (copy-bit 1 N copy-bit-1)
           (copy-bit-0 !y R copy-bit-0)
           (copy-bit-0 y (P_ L P0 R R R Py) goto-next-bit)
           (copy-bit-1 !y R copy-bit-1)
           (copy-bit-1 y (P_ L P1 R R R Py) goto-next-bit)

           (goto-next-bit !x L goto-next-bit)
           (goto-next-bit x (P_ R) goto-next-bit2)
           ;; tests if we are at the end of the string
           (goto-next-bit2 !: (R Px L) copy-bit)
           (goto-next-bit2 : N halt)))