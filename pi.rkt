#lang racket
(require "TM.rkt")


(define (append_symbols . symbols)
  (string->symbol (string-join (map symbol->string symbols) "")))

;; each skeleton should return '(list_of_configs config_to_call)

(define (L next_command)
  (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols 'L-> next_config)])
    (list (cons (list this_config 'Else 'L next_config) configs)
          this_config)))

(define (gen_skeleton name f)
  (lambda (next_command)
    (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols name '-> next_config)])
      (f this_config next_config))))

(define R2 (gen_skeleton 'R
  (lambda (this_config next_config)
    (list (list (list this_config 'Else 'R (append_symbols this_config '_second_R))
                (list (append_symbols this_config '_second_R) 'Else 'R next_config))
          this_config))))

(define (goto_symbol_constr dir)
  (lambda (symbol next_command)
    (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols 'goto_symbol_ dir '_ symbol '-> next_config)])
      (list (append (list (list this_config symbol 'N next_config)
                          (list this_config 'Else dir this_config))
                    configs)
            this_config))))

;; goes left and stops on symbol
(define goto_symbol_L (goto_symbol_constr 'L))
;; goes right and stops on symbol
(define goto_symbol_R (goto_symbol_constr 'R))

;; goes right to the sentinel, then goes left and stops on first instance of symbol
(define (find_symbol_L symbol next_command)
  (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols 'find_symbol_L_ symbol '-> next_config)])
    (goto_symbol_R '@
    (goto_symbol_L symbol (list '() next_config)))))

(define (find_symbol_R symbol next_command)
  (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols 'find_symbol_R_ symbol '-> next_config)])
    (goto_symbol_L '@
    (goto_symbol_R symbol (list '() next_config)))))

(define num_vars 2)
(define (var_distance var)
  (cond [(equal? 'a var) 1]
        [(equal? 'b var) 2]))

(define (goto_var var next_command)
  (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols 'goto_var_ var '-> next_config)] 
                                      [config (lambda (w) (append_symbols this_config w))])
    (goto_symbol_R '@
      (list (list (list (config '_move_left) 'Else (make-list (var_distance var) 'L) next_config))
            (config '_move_left)))))

(define (get_binary n)
  (map (lambda (s) (if (equal? '|0| s) '0 '1))
       (map string->symbol 
            (filter (lambda (c) (not (equal? "" c)))
                    (string-split (number->string n 2) "")))))

(define _move_var_L (make-list num_vars 'L))

(define (print_for_var symbols)
  (cond [(null? symbols) '()]
        [else (cons (append_symbols 'P (car symbols)) (append _move_var_L (print_for_var (cdr symbols))))]))

(displayln (print_for_var '(0 1 0)))

(define (print_at_head_L symbols next_command)
  (let* ([configs (car next_command)] [next_config (cadr next_command)] [this_config (append_symbols 'print_at_head_L (append_symbols symbols) '-> next_config)] 
                                      [config (lambda (w) (append_symbols this_config w))])
    (list (append (list (list this_config 'Else (print_for_var symbols) next_config)) configs)
          this_config)))

;;(displayln (goto_symbol_L 'x '(() poop)))
;;(displayln (find_symbol_L 'x '(() poop)))
(displayln (goto_var 'b '(() poop)))

#|
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


|#
