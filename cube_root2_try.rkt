#lang racket
(require "TM.rkt")

(machine '(@ 0 start)
         '(1000 0 1)
         '(
           (start Any (R P1 R R P1 R R P:) print_out_digits)

           (print_out_digits Any N print_out_digits_print_x)
               ;; print xs next to all the known digits
               (print_out_digits_print_x !@ L print_out_digits_print_x)
               (print_out_digits_print_x @ (R) print_out_digits_print_x2)
               (print_out_digits_print_x2 !: (R Px R) print_out_digits_print_x2)
               (print_out_digits_print_x2 : N placeholder_digits)

               ;; put placeholder 0s where the result of multiplication will go
               (placeholder_digits Any N placeholder_digits_find_x)
                   ;; if we hit @, print one 0 and go to multiplication
                   (placeholder_digits_find_x @ R placeholder_digits_last_0)
                   (placeholder_digits_find_x !x L placeholder_digits_find_x)
                   (placeholder_digits_find_x x (P_ R) placeholder_digits_put_0)
                   (placeholder_digits_find_x !x L placeholder_digits_find_x)
                   (placeholder_digits_put_0 Any (R R) placeholder_digits_put_0)
                   ;; print three 0s for every x
                   (placeholder_digits_put_0 None (P0 R R P0 R R P0) placeholder_digits_find_x)

                   (placeholder_digits_last_0 Any (R R) placeholder_digits_last_0)
                   ;; print one extra 0 on the end
                   (placeholder_digits_last_0 None (P0 R R Pz) begin_multi)

            ;; a, b, c are pointers (a moves first, then b, then c)
            ;; d is a and b, e is b and c, f is a and c
            ;; g is a, b, c
            (begin_multi Any N begin_multi_find_delimiter)
                (begin_multi_find_delimiter !: L begin_multi_find_delimiter)
                ;; print first f
                (begin_multi_find_delimiter : (R Ph R R Pp L L L L Pg) multiply)

            (multiply Any N find_a)
                ;; finds the a pointer, and adds that digit
                (find_a None (L L) find_a)
                (find_a g L add)
                (find_a d L add)
                (find_a f L add)
                (find_a a L add)
                (find_a Any (L L) find_a)

            ;; adds digit currently under head
            (add 0 R move_p)
            (add 1 R add_1)
            (add_1 p L carry)
            (add_1 !p R add_1)

            ;; carry just makes sure addition is done in full (wherever it starts,
            ;; it assumes it's adding 1)
            (carry 0 (P1 R) move_p)
            (carry 1 (P0 R R) carry)

            ;; moves the p pointer one to the right
            (move_p !p (R R) move_p)
            (move_p p (P_ R R Pp L) move_a)

            ;; moves a to the left. if we hit @, go to move_b
            ;; otherwise, add again
            (move_a g (Pe L L) print_a)
            (move_a f (Pc L L) print_a)
            (move_a d (Pb L L) print_a)
            (move_a a (P_ L L) print_a)
            (move_a None (L L) move_a)
            (move_a Any (L L) move_a)

            ;; prints an a under the head, goes to move_b if at @
            (print_a @ (R R) move_b)
            (print_a b (Pd L) add)
            (print_a c (Pf L) add)
            (print_a e (Pg L) add)

            (move_b g (Pf L L) print_b)
            (move_b d (Pa L L) print_b)
            (move_b e (Pc L L) print_b)
            (move_b b (P_ L L) print_b)
            (move_b None (L L) move_b)
            (move_b Any (L L) move_b)

            ;; j is where the last reset-b happened
            ;; h is where the last reset-c happened
            ;; k is both of them

            ;; prints a b under the head, goes to move_c if at @
            ;; then resets j and p
            (print_b @ (R R) move_c)
            (print_b a (Pd L) reset_j)
            (print_b c (Pe L) reset_j)
            (print_b f (Pg L) reset_j)

            ;; resets j by removing p, shifting j to the right,
            ;; and putting a new p to the right of that
            ;; then calls reset_a
            (reset_j !p R reset_j)
            (reset_j p P_ reset_j2)
            (reset_j2 j (R R Pj R R Pp) reset_a)
            (reset_j2 k (Ph R R Pj R R Pp) reset_a)
            (reset_j2 Any L reset_j2)
            (reset_j2 None L reset_j2)

            (move_c g (Pd L L) print_c)
            (move_c e (Pb L L) print_c)
            (move_c f (Pa L L) print_c)
            (move_c c (P_ L L) print_c)
            (move_c Any (L L) move_c)
            (move_c None (L L) move_c)

            (print_c @ N check_bit)
            (print_c a (Pf L) reset_h)
            (print_c b (Pe L) reset_h)
            (print_c d (Pg L) reset_h)

            (reset_h !p R reset_h)
            (reset_h p P_ reset_h2)
            (reset_h2 !j L reset_h2)
            (reset_h2 j P_ reset_h3)
            (reset_h3 !h L reset_h3)
            (reset_h3 h (P_ R R Pk R R Pp) multiply)

            ;;(check_bit


             ))
