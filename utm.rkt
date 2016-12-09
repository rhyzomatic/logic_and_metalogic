#lang racket
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(struct sk (mconfig_to_call mtable) #:transparent)
(struct instr (mconfig read_symbol ops final_mconfig) #:transparent)

(define (printsk s)
  (displayln (sk-mconfig_to_call s))
  (for-each displayln (sk-mtable s)))

;; builds the new m-config name from a list of symbols or sk's (as nesting
;; skeleton tables in one instruction line will thus return sk type
;; instead of a symbol itself)
#|
(define (build_mconfig . symbols)
  (string->symbol (string-join (map symbol->string
         (map (lambda (s) (cond [(sk? s) (sk-mconfig_to_call s)]
                                [else s]))
              symbols)) "")))
|#


(define (expand_table mconfigs)
  (cond [(null? mconfigs) '()]
        [(list? (car mconfigs))
           (append (expand_table (car mconfigs))
                   (expand_table (cdr mconfigs)))]
        [(symbol? (instr-final_mconfig (car mconfigs)))
           (cons (car mconfigs) (expand_table (cdr mconfigs)))]
        ;; if the final_mconfig of the instr is itself a skeleton table,
        ;; we need to expand it and ensure that its table gets properly appended
        [(sk? (instr-final_mconfig (car mconfigs)))
           (cons (struct-copy instr (car mconfigs) 
                              [final_mconfig (sk-mconfig_to_call (instr-final_mconfig (car mconfigs)))])
                 (append (expand_table (sk-mtable (instr-final_mconfig (car mconfigs))))
                         (expand_table (cdr mconfigs))))]
        [else (raise (list "expand_table encountered a poorly formed instr in mconfigs" (car mconfigs)))]))

(define (append_symbols . symbols)
  (string->symbol (string-join (map symbol->string symbols) "")))


;; creates a lil function that simply appends the passed in symbol
;; onto base returning a new symbol
;; note: since you can't have number symbols in racket (wtf?) we
;; have to do that weird string thing
#|
(define (mconfig_builder base)
  (lambda (postfix) 
    (append_symbols base 
        (cond [(symbol? postfix) postfix]
              [(number? postfix) (string->symbol (number->string postfix))]
              [else (raise "mconfig_builder got not a symbol or number")]))))
|#

(define (s+ . symbols)
  (string->symbol (string-join
                    (map (lambda (s)
                           (cond [(number? s) (number->string s)]
                                 [else (symbol->string s)]))
                         symbols) "")))

;;(define build_mconfig s+)
(define (build_mconfig . symbols)
  (apply s+ (map (lambda (s) (cond [(sk? s) (sk-mconfig_to_call s)]
                             [else s]))
              symbols)))

(define (mconfig_builder base)
  (lambda (postfix) (s+ base postfix)))

;; t stands for turing, idk
(define (tnot symbol)
  (s+ '! symbol))

;; f(C, B, alpha)
(define (find C B alpha)
  (let* ([this_mconfig (build_mconfig 'find< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig '@ 'L (m 1))
                (instr this_mconfig '!@ 'L this_mconfig)

                (instr (m 1) alpha 'N C)
                (instr (m 1) (s+ '! alpha) 'R (m 1))
                (instr (m 1) 'None 'R (m 2))

                (instr (m 2) alpha 'N C)
                (instr (m 2) (tnot alpha) 'R (m 1))
                (instr (m 2) 'None 'R B))))))

;; e(C, B, alpha)
(define (erase C B alpha)
  (let* ([this_mconfig (build_mconfig 'erase< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find (m 1) B alpha))
                (instr (m 1) 'Else 'P_ C))))))

;; e(B, alpha) (i.e. overloaded e)
(define (erase_all B alpha)
  (let* ([this_mconfig (build_mconfig 'erase_all< B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (erase this_mconfig B alpha)))))))

;; pe(C, beta)
(define (print_end C beta)
  (let* ([this_mconfig (build_mconfig 'print_end< C '~ beta '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find (m 1) C '@))
                (instr (m 1) 'Any '(R R) (m 1))
                (instr (m 1) 'None (s+ 'P beta) C))))))

;; l(C)
(define (left C)
  (let* ([this_mconfig (build_mconfig 'left< C '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'L C))))))

;; r(C)
(define (right C)
  (let* ([this_mconfig (s+ 'right< C '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'R C))))))

;; f'(C, B, alpha)
(define (find_left C B alpha)
  (let* ([this_mconfig (build_mconfig 'find_left< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find (left C) B alpha)))))))

;; f''(C, B, alpha)
(define (find_right C B alpha)
  (let* ([this_mconfig (build_mconfig 'find_right< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find (right C) B alpha)))))))

;; instr_pattern: '(instr this_mconfig beta 'N (find B C beta))
(define (sub_symbol symbols_left instr_pattern)
  (cond [(null? symbols_left) '()]
        [else
          (letrec ([placeholder (caddr instr_pattern)]
                   [sub (lambda (p sym)
                          (cond [(null? p) '()]
                                [(equal? placeholder (car p))
                                  (cons sym (sub (cdr p) sym))]
                                [(list? (car p))
                                  (cons (sub (car p) sym) (sub (cdr p) sym))]
                                [else
                                  (cons (car p) (sub (cdr p) sym))]))])
                  (cons (eval (sub instr_pattern (car symbols_left)) ns)
                        (sub_symbol (cdr symbols_left) instr_pattern)))]))

(define symbol_list '(0 1 'A 'C 'D))
(define negated_symbol_list '('!0 '!1 '!A '!C '!D))
(define match_sugar ((curry sub_symbol) symbol_list))
(define not_match_sugar ((curry sub_symbol) negated_symbol_list))

;;(let ([mconfig 'm] [B 'b] [C 'c])
  ;;(sub_symbol '('s 'h 0) `(instr ',mconfig beta 'N (find ',B ',C beta))))

;; c(B, A, alpha)
(define (copy C B alpha)
  (let* ([this_mconfig (build_mconfig 'copy< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find_left (m 1) B alpha))
                (match_sugar
                  `(instr ',(m 1) beta 'N (print_end ',C beta))))))))

;; ce(C, B, alpha)
(define (copy_and_erase C B alpha)
  (let* ([this_mconfig (build_mconfig 'copy_and_erase< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (copy (erase C B alpha) B alpha)))))))

;; ce(B, alpha) (i.e. overloaded version)
(define (copy_and_erase_all B alpha)
  (let* ([this_mconfig (build_mconfig 'copy_and_erase_all< B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (copy_and_erase this_mconfig B alpha)))))))

;; re(C, B, alpha)
(define (replace C B alpha beta)
  (let* ([this_mconfig (build_mconfig 'replace< C '~ B '~ alpha '~ beta '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find (m 1) B alpha))
                (instr (m 1) 'Else (list 'P_ (s+ 'P beta)) C))))))

;; re(B, alpha) (i.e. overloaded version)
(define (replace_all B alpha beta)
  (let* ([this_mconfig (build_mconfig 'replace_all< B '~ alpha '~ beta '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (replace this_mconfig B alpha beta)))))))

;; cr(C, B, alpha)
(define (copy_and_replace C B alpha)
  (let* ([this_mconfig (build_mconfig 'copy_and_replace< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (copy (replace C B alpha alpha) B alpha)))))))

;; cr(B, alpha) (i.e. overloaded version)
(define (copy_and_replace_all B alpha)
  (let* ([this_mconfig (build_mconfig 'copy_and_replace_all< B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (copy_and_replace
                                               this_mconfig
                                               (replace_all B alpha alpha)
                                               alpha)))))))
;; cp(C, A, E, alpha, beta)
(define (compare C A E alpha beta)
  (let* ([this_mconfig (build_mconfig 'compare< C '~ A '~ E '~ alpha '~ beta '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find_left (m 1) (find A E beta) alpha))
                (match_sugar
                  `(instr ',(m 1) gamma 'N (find_left (s+ ',(m 2) '-> gamma) ',A ',beta)))
                (match_sugar
                  `(instr ',(m 2) gamma 'N ',C))
                (not_match_sugar
                  `(instr ',(m 2) gamma 'N ',A)))))))

;; cpe(C. A. E. alpha, beta)
(define (compare_and_erase C A E alpha beta)
  (let* ([this_mconfig (build_mconfig 'compare_and_erase< C '~ A '~ E '~ alpha '~ beta '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (compare (erase (erase C C beta) C alpha) A E alpha beta)))))))

;; cpe(A, E, alpha, beta) (i.e. overloaded version)
(define (compare_and_erase_all A E alpha beta)
  (let* ([this_mconfig (build_mconfig 'compare_and_erase_all< A '~ E '~ alpha '~ beta '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (compare_and_erase this_mconfig A E alpha beta)))))))

;; g(C)
(define (find_end C)
  (let* ([this_mconfig (build_mconfig 'find_end< C '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Any 'R this_mconfig)
                (instr this_mconfig 'None 'R (m 1))

                (instr (m 1) 'Any 'R this_mconfig)
                (instr (m 1) 'None 'N C))))))

;; g(C, alpha)
(define (find_last C alpha)
  (let* ([this_mconfig (build_mconfig 'find_last< C '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find_end (m 1)))
                (instr (m 1) alpha 'N C)
                (instr (m 1) (tnot alpha) 'L (m 1)))))))

;; pe2(C, alpha, beta)
(define (print_end2 C alpha beta)
  (let ([this_mconfig (build_mconfig 'print_end2< C '~ alpha '~ beta '>)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (print_end (print_end C beta) alpha)))))))

;; ce2(C, alpha, beta)
(define (copy_and_erase2 B alpha beta)
  (let ([this_mconfig (build_mconfig 'copy_and_erase2< B '~ alpha '~ beta '>)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (copy_and_erase (copy_and_erase B beta) alpha)))))))

(define (join_with_symbol symbol l)
  (cond [(null? (cdr l)) l]
        [else
          (cons (car l) (cons symbol (join_with_symbol symbol (cdr l))))]))

(define (copy_and_erase_n . args)
  (letrec ([B (car args)]
           [markers (cdr args)]
           [n (length markers)]
           [this_mconfig (build_mconfig 'copy_and_erase n '< (apply s+ (join_with_symbol '~ markers)) '>)]
           [ce_helper (lambda (m)
                        (cond [(null? (cdr m))
                                (copy_and_erase_all B (car m))]
                              [else
                                (copy_and_erase_all (ce_helper (cdr m)) (car m))]))])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (ce_helper markers)))))))

;; e(C)
(define (erase_all_markers C)
  (let* ([this_mconfig (build_mconfig 'erase_all_markers< C '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig '@ 'R (m 1))
                (instr this_mconfig '!@ 'L this_mconfig)
                (instr (m 1) 'Any '(R P_ R) (m 1))
                (instr (m 1) 'None 'N C))))))

;; con(C, alpha)
(define (con C alpha)
  (let* ([this_mconfig (build_mconfig 'con< C '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig '!A '(R R) this_mconfig)
                (instr this_mconfig 'A (list 'L (s+ 'P alpha) 'R) (m 1))
                (instr (m 1) 'A (list 'R (s+ 'P alpha) 'R) (m 1))
                (instr (m 1) 'D (list 'R (s+ 'P alpha) 'R) (m 2))
                (instr (m 2) 'C (list 'R (s+ 'P alpha) 'R) (m 2))
                (instr (m 2) '!C '(R R) C))))))


(define kom (expand_table
      (list (instr 'kom '^ '(R Pz L) (con 'kmp 'x))
            (instr 'kom 'z '(L L) 'kom)
            (instr 'kom 'Else 'L 'kom))))

(define begin  (expand_table
    (list (instr 'begin 'Else 'N (find 'begin1 'begin1 '$))
          (instr 'begin1 'Else '(R R P: R R PD R R PA) 'anf))))

(define anf  (expand_table
    (list (instr 'anf 'Else 'N (find_last 'anf1 ':))
          (instr 'anf1 'Else 'N (con 'kom 'y)))))

(define kmp  (expand_table
    (list (instr 'kmp 'Else 'N (compare_and_erase_all (erase_all (erase_all 'anf 'x) 'y) 'sim 'x 'y)))))

(define sim (expand_table
        (list (instr 'sim 'Else 'N (find_left 'sim1 'sim1 'z))
              (instr 'sim1 'Else 'N (con 'sim2 '_))

              (instr 'sim2 'A 'N 'sim3)
              (instr 'sim2 '!A '(L Pu R R R) 'sim2)

              (instr 'sim3 '!A '(L Py) (erase_all 'mk 'z))
              (instr 'sim3 'A '(L Py R R R) 'sim3))))

(define mk (expand_table
        (list (instr 'mk 'Else 'N (find_last 'mk1 ':))

              (instr 'mk1 '!A '(R R) 'mk1)
              (instr 'mk1 'A '(L L L L) 'mk2)

              (instr 'mk2 'C '(R Px L L L) 'mk2)
              (instr 'mk2 ': 'N 'mk4)
              (instr 'mk2 'D '(R Px L L L) 'mk3)

              (instr 'mk3 '!: '(R Pv L L L) 'mk3)
              (instr 'mk3 ': 'N 'mk4)

              (instr 'mk4 'Else 'None (con (left (left 'mk5)) '_))

              (instr 'mk5 'Any '(R Pw R) 'mk5)
              (instr 'mk5 'None 'P: 'sh))))

(define sh (expand_table
        (list (instr 'sh 'Else 'None (find 'sh1 'inst 'u))
              (instr 'sh1 'Else '(L L L) 'sh2)

              (instr 'sh2 'D '(R R R R) 'sh3)
              (instr 'sh2 '!D 'N 'inst)

              (instr 'sh3 'C '(R R) 'sh4)
              (instr 'sh3 '!C 'N 'inst)

              (instr 'sh4 'C '(R R) 'sh5)
              (instr 'sh4 '!C 'N (print_end2 'inst 0 ':))

              (instr 'sh5 'C 'N 'inst)
              (instr 'sh5 '!C 'N 'inst))))

(define inst (expand_table
    (list (instr 'inst 'Else 'N (find_last (left 'inst1) 'u))
          (match_sugar
            `(instr 'inst1 alpha '(R P_) (s+ 'inst1< alpha '>)))
          ;;(instr 'inst<L> 'Else 'N (copy_and_erase_n 'ov 'v 'y 'x 'u 'w))
          (instr 'inst<L> 'Else 'N (copy_and_erase_all 
                                     (copy_and_erase_all
                                       (copy_and_erase_all
                                         (copy_and_erase_all
                                           (copy_and_erase_all 'ov 'w)
                                           'u)
                                         'x)
                                       'y)
                                     'v))
          ;;(instr 'inst<R> 'Else 'N (copy_and_erase_n 'ov 'v 'x 'u 'y 'w))
          ;;(instr 'inst<N> 'Else 'N (copy_and_erase_n 'ov 'v 'x 'y 'u 'w))
          (instr 'ov 'Else 'N (erase_all_markers 'anf)))))

(length (append kom begin anf kmp sim mk sh inst))
