#lang racket
(require "TM.rkt")
(machine '(@@0_1_0 0 start)
         '(1000 0 1000)
         '((start Any (R R R Px R R R R Py) add-two-1s)
           
           (add-two-1s y (P_ R P1 R R P1 R Py) check-x)
           (add-two-1s !y R add-two-1s)

           (check-x x (P_ R R R) check-x2)
           (check-x !x L check-x)
           (check-x2 1 (L Px) add-two-1s)
           (check-x2 0 (R Px) set-up-y)

           (set-up-y y (P_ R P0 R Py) add-two-1s)
           (set-up-y !y R set-up-y)))