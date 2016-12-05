#lang racket
(require "TM.rkt")

(define (append_symbols a b)
  (string->symbol (string-join (map symbol->string (list a b)) "")))

;; adds int at x to int at y and puts result at z
;;(define (add x y z spacing prefix)

;; will copy the last number in sequence of 's|int2|'s|int1|'s to int2|'s|int2|'s|int1
;; note prefix is used as an m-config, it's what starts it off
;; finishes on the leftmost 's (before the copied int)
(define (copy_to_left prefix final_state)
  (let ([pre (lambda (a) (append prefix a))])
    (list (list prefix 'Any 'N (pre 'copy_left_find_start))
          ;; go to one position before final 's
          (list (pre 'copy_left_find_start) None (make-list 10 'R) (pre 'copy_left_find_start2))
          (list (pre 'copy_left_find_start) Any (make-list 5 'L) (pre 'copy_left_find_start))
          ;; go to 's before this and then go to first digit to the left of that
          (list (pre 'copy_left_find_start2) '!s (make-list 5 'R) (pre 'copy_left_find_start2))
          (list (pre 'copy_left_find_start2) 's (make-list 5 'L) (pre 'copy_bit))
          ;; start copying
          (list (pre 'copy_bit) 0 'Pz (pre 'copy0))
          (list (pre 'copy_bit) 1 Po (pre 'copy1))
          ;; if we hit an 's, we're done
          (list (pre 'copy_bit) 's N final_state)
          (list (pre 'copy0) None 'P0 (pre 'copy_next_bit))
          (list (pre 'copy1) None 'P1 (pre 'copy_next_bit))
          (list (pre 'copy_next_bit) 'o (append '(P1) (make-list 5 L)) (pre 'copy_bit))
          (list (pre 'copy_next_bit) 'z (append '(P0) (make-list 5 L)) (pre 'copy_bit))
          (list (pre 'copy_next_bit) Any (make-list 5 'R) (pre 'copy_next_bit)))))

;; adds 1 to previous number for creating sequence a
;; then goes to sequence_add_2 for b
(define (sequence_add_1)
  (append (copy_to_left 'sequence_add_1_ 'sequence_add_1_add_1)
    (list (list 'sequence_add_1 Any 'L 'sequence_add_1_)
          (list 'sequence_add_1_add_1 's (make-list 5 'L))

  
(machine '(@ 0 begin)
         '(1000 0 1)
         '(
           (next_a Any N next_a_copy)
           (next_a_copy None 


