#lang racket
(require "TM.rkt")
(machine '(@@1_0_1_0_1_+1_1_1_1_1_: 0 start)
         '(1000 0 1)
         '((start Any (R R R Px) setup-y)
           (setup-y !+ R setup-y)
           (setup-y + (R R Py) setup-z)
           (setup-z !: R setup-z)
           (setup-z : (R R Pz) goto-first-bit)

           (goto-first-bit !@ L goto-first-bit)
           (goto-first-bit @ R add-bit)

           (add-bit 0 N add-bit-0)
           (add-bit 1 N add-bit-1)
           
           (add-bit-0 !y R add-bit-0)
           (add-bit-0 y L add-bit-0+)
           (add-bit-0+ 0 N added-bit-0)
           (add-bit-0+ 1 N added-bit-1)

           (add-bit-1 !y R add-bit-1)
           (add-bit-1 y L add-bit-1+)
           (add-bit-1+ 0 N added-bit-1)
           (add-bit-1+ 1 N added-bit-2)

           (added-bit-0 !z R added-bit-0)
           (added-bit-0 z (P_ R R Pz L L L) added-bit-0+)
           (added-bit-0+ !1 P0 goto-next-bit)
           (added-bit-0+ 1 P1 goto-next-bit)

           (added-bit-1 !z R added-bit-1)
           (added-bit-1 z (P_ R R Pz L L L) added-bit-1+)
           (added-bit-1+ !1 P1 goto-next-bit)
           (added-bit-1+ 1 (P0 R R P1) goto-next-bit)

           (added-bit-2 !z R added-bit-2)
           (added-bit-2 z (P_ R R Pz L L L) added-bit-2+)
           (added-bit-2+ !1 (P0 R R P1) goto-next-bit)
           (added-bit-2+ 1 (P1 R R P1) goto-next-bit)

           (goto-next-bit !y L goto-next-bit)
           (goto-next-bit y (P_ R) goto-next-bit-move-y)
           ;; tests if we are at the end of the string
           (goto-next-bit-move-y !: (R Py L) goto-next-bit-move-x)
           (goto-next-bit-move-y : N halt)
           (goto-next-bit-move-x !x L goto-next-bit-move-x)
           (goto-next-bit-move-x x (P_ R R Px L) add-bit)))