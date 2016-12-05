#lang racket
(require "TM.rkt")

(machine '(@ 0 begin)
         '(10000 0 1)
         '(
           (begin Any (R P1 R R P0 R P1 R P: R R R R R R R P1 R R R R P1 R R R R Pw) i)

           (a s Pt b)
           (a p N c)
           (a None (L L L L) a)
           (a Any (L L L L) a)

           (b q Px a)
           (b None (L L L L) d)
           ;;(b None (R R R R) b)
           (b Any (R R R R) b)

           (c t (Ps R R R R) c)
           (c q N a)
           (c None (Pq L L L L) e)
           ;;(c None (R R R R) c)
           (c Any (R R R R) c)

           (d x (Pq L L L L) d)
           (d t (P1 L L L L) d)
           (d s (P1 L L L L) d)
           (d p (Pw L L L L) d)
           (d None Pp f)
           (d @ N h)
           (d Any (L L L L) d)

           (e x (Pq L L L L) e)
           (e s (P1 L L L L) e)
           (e w (P_ L L L L) e)
           (e p P_ g)
           (e Any (L L L L) e)
           (e None (L L L L) e)

           (f w N a)
           (f 1 (Ps R R R R) f)
           (f None (R R R R) f)
           (f Any (R R R R) f)

           (g q (L L L L Pw L L L L) j)
           (g Any (R R R R) g)
           (g None (R R R R) g)

           (h w (P_ R R R R) h)
           (h q (P1 R R R R) h)
           (h None (Pw L L L L) convert_to_bin)
           ;;(h None (R R R R) h)
           (h Any (R R R R) h)

           (i None Pp k)
           ;;(i None (L L L L) i)
           (i Any (L L L L) i)

           (j None Pp f)
           ;;(j None (L L L L) j)
           (j Any (L L L L) j)

           (k None Pq m)
           ;;(k None (R R R R) k)
           (k Any (R R R R) k)

           (m 1 Ps n)
           (m p N f)
           (m None (L L L L) m)
           (m Any (L L L L) m)

           (n None Pq m)
           (n Any (R R R R) n)


           ;; converts the last unary prime to binary after the colon
           ;; goes to state i after :)
           (convert_to_bin Any (R R Ps L L) add_1)

           (add_1 1 (L L Pd) add_1_go_right)
           (add_1 None (R R P_) copy_bin_string)

           (add_1_go_right !s (R R R R) add_1_go_right)
           (add_1_go_right s (R R R R) carry)

           (carry !1 P1 add_next_1)
           (carry 1 (P0 R R R R) carry)

           (add_next_1 !d (L L L L) add_next_1)
           (add_next_1 d (P_ L L) add_1)

           (copy_bin_string None N goto_s)
           (goto_s !s (R R R R) goto_s)
           (goto_s s (R R R R) find_string_end)

           (find_string_end Any (R R R R) find_string_end)
           (find_string_end None (L L L L) copy_bit)

           (copy_bit 1 (Pe L) copy_bit_1)
           (copy_bit 0 (Pe L) copy_bit_0)
           ;; reached the end of the copying string
           (copy_bit s (P_ L) add_colon)

           (add_colon None (L L) add_colon)
           (add_colon Any (R R P:) next_prime)

           (copy_bit_1 None (L L) copy_bit_1)
           (copy_bit_1 Any (R R P1) copy_next_bit)

           (copy_bit_0 None (L L) copy_bit_0)
           (copy_bit_0 Any (R R P0) copy_next_bit)

           (copy_next_bit !e R copy_next_bit)
           (copy_next_bit e (P_ L L L L) copy_bit)

           (next_prime !w R next_prime)
           (next_prime w (L L L L) i)

           ))


