#lang racket
(require "TM.rkt")

(machine '(@@1x:_1_:__y 3 copy_first_seq)
         '(1000 0 1)
         '(
           (copy_first_seq x R copy_first_seq2)
           (copy_first_seq !x L copy_first_seq)
           (copy_first_seq2 : (L P_ R R R R Px) copy_second_seq)
           (copy_first_seq2 !: (L P_ R R Px) first_seq_add_1)
           
           (first_seq_add_1 !y R first_seq_add_1)
           (first_seq_add_1 y (P_ L P1 R R R Py) copy_first_seq)
           
           (copy_second_seq x R copy_second_seq2)
           (copy_second_seq !x L copy_second_seq)
           (copy_second_seq2 : L add_colon)
           (copy_second_seq2 !: (L P_ R R Px) second_seq_add_1)
           
           (second_seq_add_1 !y R second_seq_add_1)
           (second_seq_add_1 y (P_ L P1 R R R Py) copy_second_seq)

           ;; erase x
           (add_colon Any P_ add_colon1)
           ;; print new x at beginning of current seq
           (add_colon1 !: L add_colon1)
           (add_colon1 : (R R R Px) add_colon2)
           ;; add the next colon and shift over y
           (add_colon2 !y R add_colon2)
           (add_colon2 y (P_ L P1 R R P1 R R P: R R R Py) copy_first_seq)
                       ))
