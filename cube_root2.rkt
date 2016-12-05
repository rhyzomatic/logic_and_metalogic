#lang racket
(require "TM.rkt")

(machine '(@ 0 start)
         '(100000 0 10000)
         '(
           (start Any (R P1 R R P:) print_out_digits)

           (print_out_digits Any N print_out_digits_print_x)
           (print_out_digits None N print_out_digits_print_x)
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
                   (placeholder_digits_last_0 None (Pz) begin_multi)

            ;; a, b, c are pointers (a moves first, then b, then c)
            ;; d is a and b, e is b and c, f is a and c
            ;; g is a, b, c
            (begin_multi Any N begin_multi_find_delimiter)
                (begin_multi_find_delimiter !: L begin_multi_find_delimiter)
                ;; print first f
                (begin_multi_find_delimiter : (R Ph R R Pp L L L L Pc) multiply)

            #|(multiply Any N find_a)
                ;; finds the a pointer, and adds that digit
                (find_a None (L L) find_a)
                (find_a g L add)
                (find_a d L add)
                (find_a f L add)
                (find_a a L add)
                (find_a Any (L L) find_a)
            |#
            (multiply Any N check_c_digit)

            ;; adds digit currently under head
            (add 0 R go_right_move_p)
            (add 1 R add_1)
            (add_1 p L carry)
            (add_1 !p R add_1)

            ;; carry just makes sure addition is done in full (wherever it starts,
            ;; it assumes it's adding 1)
            (carry 0 (P1 R) move_p)
            (carry 1 (P0 R R) carry)

            ;; moves the p pointer one to the right
            (move_p !p (L L) move_p)
            (move_p p (P_ R R Pp) move_a)
            ;; same thing as move_p, just when p happens to be to the right of the head
            (go_right_move_p !p R go_right_move_p)
            (go_right_move_p p N move_p)

            ;; moves a to the left. if we hit @, go to move_b
            ;; otherwise, add again
            (move_a g (Pe L L) print_a)
            (move_a f (Pc L L) print_a)
            (move_a d (Pb L L) print_a)
            (move_a a (P_ L L) print_a)
            (move_a None (L L) move_a)
            (move_a Any (L L) move_a)

            ;; prints an a under the head, goes to move_b if at @
            (print_a @ (R R) goto_move_b)
            (print_a None (Pa L) add)
            (print_a b (Pd L) add)
            (print_a c (Pf L) add)
            (print_a e (Pg L) add)

            ;; just goes to the :, then calls move b
            (goto_move_b !: R goto_move_b)
            (goto_move_b : L move_b)
            (move_b g (Pf L L) print_b)
            (move_b d (Pa L L) print_b)
            (move_b e (Pc L L) print_b)
            (move_b b (P_ L L) print_b)
            (move_b None (L L) move_b)
            (move_b Any (L L) move_b)

            ;; j is where the last reset-b happened
            ;; k is where the last reset-c happened
            ;; h is both of them

            ;; prints a b under the head, goes to move_c if at @
            ;; then resets j and p
            (print_b @ (R R) goto_move_c)
            (print_b None (Pb L) reset_j)
            (print_b a (Pd L) reset_j)
            (print_b c (Pe L) reset_j)
            (print_b f (Pg L) reset_j)

            ;; resets j by removing p, shifting j to the right,
            ;; and putting a new p to the right of that
            ;; then calls check_digit
            (reset_j !p R reset_j)
            (reset_j p P_ reset_j2)
            (reset_j2 j (P_ R R Pj R R Pp) check_b_digit)
            (reset_j2 h (Pk R R Pj R R Pp) check_b_digit)
            (reset_j2 Any L reset_j2)
            (reset_j2 None L reset_j2)

            (check_b_digit b L check_b_digit2)
            (check_b_digit d L check_b_digit2)
            (check_b_digit e L check_b_digit2)
            (check_b_digit g L check_b_digit2)
            (check_b_digit Any L check_b_digit)
            (check_b_digit None L check_b_digit)
            ;; if the b digit is zero, we can skip and reset_j again
            (check_b_digit2 0 R move_b)
            ;; otherwise, reset a and start adding
            (check_b_digit2 1 N reset_a)

            (reset_a !: R reset_a)
            (reset_a : L print_a)

            ;; just goes to the :, then calls move_c
            (goto_move_c !: R goto_move_c)
            (goto_move_c : L move_c)
            (move_c g (Pd L L) print_c)
            (move_c e (Pb L L) print_c)
            (move_c f (Pa L L) print_c)
            (move_c c (P_ L L) print_c)
            (move_c Any (L L) move_c)
            (move_c None (L L) move_c)

            (print_c @ N check_new_bit)
            (print_c None (Pc L) reset_k)
            (print_c a (Pf L) reset_k)
            (print_c b (Pe L) reset_k)
            (print_c d (Pg L) reset_k)

            (reset_k !p R reset_k)
            (reset_k p P_ reset_k2)
            (reset_k2 j P_ reset_k3)
            (reset_k2 h Pk reset_k3)
            (reset_k2 Any L reset_k2)
            (reset_k2 None L reset_k2)
            (reset_k3 !k L reset_k3)
            (reset_k3 k (P_ R R Ph R R Pp) check_c_digit)

            (check_c_digit c L check_c_digit2)
            (check_c_digit e L check_c_digit2)
            (check_c_digit f L check_c_digit2)
            (check_c_digit g L check_c_digit2)
            (check_c_digit Any L check_c_digit)
            (check_c_digit None L check_c_digit)
            ;; if the c digit is zero, we can skip and reset_k again
            (check_c_digit2 0 R move_c)
            ;; otherwise, reset a and b and multiply again
            (check_c_digit2 1 N reset_a_and_b)

            (reset_a_and_b !: R reset_a_and_b)
            (reset_a_and_b : L print_a_and_b)
            (print_a_and_b None Pb check_b_digit)
            (print_a_and_b c Pe check_b_digit)

            (check_new_bit !z R check_new_bit)
            (check_new_bit z (L L) check_first_result)
            (check_first_result 0 (L L) check_second_result)
            (check_first_result 1 N new_bit_is_0)
            (check_second_result 0 N reset_all)
            (check_second_result 1 N new_bit_is_0)

            (new_bit_is_0 !: L new_bit_is_0)
            (new_bit_is_0 : (L L P0 R R) reset_all)

            (reset_all !: L reset_all)
            (reset_all : (P1 R R P: R) reset_all2)
            (reset_all2 !z (P_ R) reset_all2)
            (reset_all2 z P_ print_out_digits)


             ))
