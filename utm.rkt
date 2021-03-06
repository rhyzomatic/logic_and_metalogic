#lang racket
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(require "TM.rkt")

;;;; borrowed from http://stackoverflow.com/a/15871126/3810418
(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) (displayln ele) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))
;;;; ----------------

(struct sk (mconfig_to_call mtable) #:transparent)
(struct instr (mconfig read_symbol ops final_mconfig) #:transparent)
(define (instr->list i)
  (list (instr-mconfig i) (instr-read_symbol i) (instr-ops i) (instr-final_mconfig i)))

(define (printsk s)
  (displayln (sk-mconfig_to_call s))
  (for-each displayln (set->list (sk-mtable s))))

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
  (cond [(null? mconfigs) (set)]
        [(list? (car mconfigs))
           (set-union (expand_table (car mconfigs))
                      (expand_table (cdr mconfigs)))]
        [(symbol? (instr-final_mconfig (car mconfigs)))
           (set-add (expand_table (cdr mconfigs)) (car mconfigs))]
        ;; if the final_mconfig of the instr is itself a skeleton table,
        ;; we need to expand it and ensure that its table gets properly appended
        [(sk? (instr-final_mconfig (car mconfigs)))
           (set-add (set-union (expand_table (set->list (sk-mtable (instr-final_mconfig (car mconfigs)))))
                               (expand_table (cdr mconfigs)))
                    (struct-copy instr (car mconfigs) 
                                 [final_mconfig (sk-mconfig_to_call (instr-final_mconfig (car mconfigs)))]))]
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
                (instr (m 1) 'None 'R (m 2))
                (instr (m 1) (tnot alpha) 'R (m 1))

                (instr (m 2) alpha 'N C)
                (instr (m 2) 'None 'R B)
                (instr (m 2) (tnot alpha) 'R (m 1)))))))

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
(define (sub_symbol symbols_left placeholder instr_pattern)
  (cond [(null? symbols_left) '()]
        [else
          (letrec ([sub (lambda (p sym)
                          (cond [(null? p) '()]
                                [(equal? placeholder (car p))
                                  (cons sym (sub (cdr p) sym))]
                                [(list? (car p))
                                  (cons (sub (car p) sym) (sub (cdr p) sym))]
                                [else
                                  (cons (car p) (sub (cdr p) sym))]))])
                  (cons (eval (sub instr_pattern (car symbols_left)) ns)
                        (sub_symbol (cdr symbols_left) placeholder instr_pattern)))]))

(define symbol_list '('\0 '\1 'A 'C 'D 'L 'R 'N))
(define negated_symbol_list '('!0 '!1 '!A '!C '!D '!L '!R '!N))
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
                (match_sugar 'beta
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
                (match_sugar 'gamma
                  `(instr ',(m 1) gamma 'N (find_left (s+ ',(m 2) '-> gamma) ',A ',beta)))
                (match_sugar 'gamma
                  `(instr (s+ ',(m 2) '-> gamma) gamma 'N ',C))
                (match_sugar 'gamma
                  `(instr (s+ ',(m 2) '-> gamma) (s+ '! gamma) 'N ',A)))))))

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
                (instr (m 1) 'None (list 'PD 'R (s+ 'P alpha) 'R 'R 'R) C)

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

              (instr 'mk4 'Else 'N (con (left (left 'mk5)) '_))

              (instr 'mk5 'Any '(R Pw R) 'mk5)
              (instr 'mk5 'None 'P: 'sh))))

(define sh (expand_table
        (list (instr 'sh 'Else 'N (find 'sh1 'inst 'u))
              (instr 'sh1 'Else '(L L L) 'sh2)

              (instr 'sh2 'D '(R R R R) 'sh3)
              (instr 'sh2 '!D 'N 'inst)

              (instr 'sh3 'C '(R R) 'sh4)
              (instr 'sh3 '!C 'N 'inst)

              (instr 'sh4 'C '(R R) 'sh5)
              (instr 'sh4 '!C 'N (print_end2 'inst '\0 ':))

              (instr 'sh5 'C 'N 'inst)
              (instr 'sh5 '!C 'N (print_end2 'inst '\1 ':)))))

;;(length (sk-mtable (copy 'c 'b 'x)))
;;(length (sk-mtable (copy_and_erase 'c 'b 'x)))
;;(printsk (copy_and_erase 'c 'b 'x))
;;(printsk (copy_and_erase_all 'ov 'w))

(define inst (expand_table
    (list (instr 'inst 'Else 'N (find_last (left 'inst1) 'u))
          (match_sugar 'alpha
            `(instr 'inst1 alpha '(R P_) (s+ 'inst1< alpha '>)))
          (instr 'inst1<L> 'Else 'N (copy_and_erase_n 'ov 'v 'y 'x 'u 'w))
          (instr 'inst1<R> 'Else 'N (copy_and_erase_n 'ov 'v 'x 'u 'y 'w))
          (instr 'inst1<N> 'Else 'N (copy_and_erase_n 'ov 'v 'x 'y 'u 'w))
          (instr 'ov 'Else 'N (erase_all_markers 'anf)))))


(define (instr_set->list instr_set)
  (cond [(set-empty? instr_set) '()]
        [(eq? (instr-read_symbol (set-first instr_set)) 'Else)
           (append (list
                      (instr->list (struct-copy instr (set-first instr_set)
                                                [read_symbol 'Any]))
                      (instr->list (struct-copy instr (set-first instr_set)
                                                [read_symbol 'None])))
                   (instr_set->list (set-rest instr_set)))]
        [else
          (cons (instr->list (set-first instr_set))
                (instr_set->list (set-rest instr_set)))]))

(define (sort_instr_list instr_list)
  (sort instr_list
        (lambda (x y)
          (cond [(eq? (car x) (car y))
                   (cond [(eq? (cadr x) 'None) #t]
                         [(eq? (cadr y) 'None) #f]
                         [(eq? (cadr x) 'Any) #f]
                         [(eq? (cadr y) 'Any) #t]
                         [else (symbol<? (cadr x) (cadr y))])]
                [else
                  (symbol<? (car x) (car y))]))))

(define utm (sort_instr_list (instr_set->list (set-union kom begin anf kmp sim mk sh inst))))

#|
(machine '(@@^_D_A_D_D_C_C_N_D_A_A_^_D_A_A_D_C_C_D_C_C_L_D_A_$
            0 begin)
         '(100000 0 1000)
         utm)
|#

;;(for-each displayln utm)

(define directions (set 'L 'R 'N))

(define (stripP s)
  (if (eq? s 'P_) 'None 
      (string->symbol (string-trim (symbol->string s) "P" #:right? #f))))

(define (single_to_double_op op)
  (cond [(set-member? directions op) (list 'None op)]
        [else (list (stripP op) 'N)]))

(define (convert_to_double_op i)
  (let ([ops (caddr i)])
       (cond [(null? ops) '()]
             [(symbol? ops) (list (list-set i 2 (single_to_double_op ops)))]
             [(= (length ops) 1) (list (list-set i 2 (single_to_double_op (car ops))))]
             [else
               (let ([new_mconfig (gensym)])
                   (cond [(and (not (set-member? directions (car ops))) (set-member? directions (cadr ops)))
                            (cons
                              (append (take i 2) (list (list (stripP (car ops)) (cadr ops)) 
                                                             (if (= (length ops) 2) 
                                                                 (cadddr i)
                                                                 new_mconfig)))
                              (convert_to_double_op (list new_mconfig 'Any (cddr ops) (cadddr i))))]
                         [else
                           (cons
                             (append (take i 2) (list (single_to_double_op (car ops)) new_mconfig))
                             (convert_to_double_op (list new_mconfig 'Any (cdr ops) (cadddr i))))]))])))


;;(convert_to_double_op '(wut a (R R R Pg L Pe R) wut2))
;;(convert_to_double_op '(wut a N wut2))

(define (expand_ops instr_list)
  (cond [(null? instr_list) '()]
        [else
          (append (convert_to_double_op (car instr_list))
                  (expand_ops (cdr instr_list)))]))


;;(for-each displayln (expand_ops utm))

(define (find_mconfig_scanned_symbols mconfig instr_list)
   (cond [(null? instr_list) (set)]
         [(and (eq? mconfig (caar instr_list)) (not (eq? (cadar instr_list) 'Any)))
            (set-add (find_mconfig_scanned_symbols mconfig (cdr instr_list))
                     (cadar instr_list))]
         [else (find_mconfig_scanned_symbols mconfig (cdr instr_list))]))

(define test
  (expand_ops '((wut x (R R R Px L PA R) wut2)
              (wut D (R L Py) wut2)
              (wut2 Any (R Pw R Pz R R Py) wut))))

;;(find_mconfig_scanned_symbols 'wut test)

(define (enum_sym_read_print i symbols)
  (cond [(set-empty? symbols) '()]
        [else
          (cons (list-set (list-set i 1 (set-first symbols)) 2 (cons (set-first symbols) (cdaddr i))) 
                (enum_sym_read_print i (set-rest symbols)))]))

;;(enum_sym_read_print '(wut a (x R) next) (set 'f 'u 'c 'k 'm 'e))

(define (enum_sym_read i symbols)
  (cond [(set-empty? symbols) '()]
        [else
          (cons (list-set i 1 (set-first symbols))
                (enum_sym_read i (set-rest symbols)))]))

;;(enum_sym_read '(wut a (x R) next) (set 'f 'u 'c 'k 'm 'e))


(define all_symbols '(None 0 1 @ : $ ^ D A C L R N v u w x y z))
(define all_symbols_set (list->set all_symbols))

(define (is_not_scan? symbol)
  (eq? #\! (string-ref (symbol->string symbol) 0)))

(define (number_symbol->number symbol)
  (cond [(eq? symbol '\0) 0]
        [(eq? symbol '\1) 1]
        [else symbol]))

(define (get_banged_symbol symbol)
  (number_symbol->number (string->symbol (substring (symbol->string symbol) 1 2))))

(define (fix_print instr_list)
  (letrec ([fix_instr 
             (lambda (ins)
                  (cond [(and (eq? (cadr ins) 'Any) (eq? (caaddr ins) 'None))
                           (enum_sym_read_print ins 
                                                (set-subtract all_symbols_set 
                                                              (find_mconfig_scanned_symbols 
                                                                (car ins) instr_list)))]
                        [(eq? (cadr ins) 'Any)
                           (enum_sym_read 
                             ins (set-subtract all_symbols_set 
                                               (find_mconfig_scanned_symbols 
                                                 (car ins) instr_list)))]
                        [(is_not_scan? (cadr ins))
                           (enum_sym_read
                             ins (set-subtract all_symbols_set
                                               (set (get_banged_symbol (cadr ins)))))]
                        [(eq? (caaddr ins) 'None)
                           (list (list-set ins 2 (cons (cadr ins) (cdaddr ins))))]
                        [else
                          (list ins)]))]
           [fix_all (lambda (i_list)
                      (if (null? i_list) '()
                          (append (fix_instr (car i_list))
                                  (fix_all (cdr i_list)))))])
    (fix_all instr_list)))

;;(for-each displayln (fix_print test))

(define (get_new_mconfig mconfig mconfig_map)
  (if (hash-has-key? mconfig_map mconfig)
      (list (hash-ref mconfig_map mconfig) mconfig_map)
      (list (hash-count mconfig_map) 
            (hash-set mconfig_map mconfig 
                      (hash-count mconfig_map)))))

(define (normalize_mconfigs instr_list)
  (letrec ([helper
             (lambda (mconfig_map i_list)
               (if (null? i_list) '()
                   (let* ([m0 (get_new_mconfig (caar i_list) mconfig_map)]
                          ;; have to use the updated mconfig_map from m0
                          ;; just in case it's different
                          [m1 (get_new_mconfig (cadddr (car i_list)) (cadr m0))])
                     (cons (append (list (car m0)) 
                                   (flatten (take (cdar i_list) 2)) 
                                   (list (car m1)))
                           (helper (cadr m1) (cdr i_list))))))])
    ;; make sure 'begin mconfig gets the 0, so it is called first by the UTM
    (helper (hash 'begin 0) instr_list)))

;(for-each displayln (normalize_mconfigs (fix_print (expand_ops utm))))

(define (insert_blanks str)
  (string-append (string-join (map string (string->list str)) "_") "_"))

(define (mconfig->sd mconfig_n)
  (insert_blanks (string-append "D" (make-string (+ mconfig_n 1) #\A))))

(define (symbol->sd symbol)
  (insert_blanks (string-append "D" (make-string (index-of all_symbols (number_symbol->number symbol)) #\C))))

(define (instr->sd i)
  (string-append "^_" (mconfig->sd (car i)) 
                      (symbol->sd (cadr i)) 
                      (symbol->sd (caddr i)) 
                      (symbol->string (cadddr i)) "_"
                      (mconfig->sd (last i))))

(define (instr_list->sd instr_list)
  (string-append "@@" (apply string-append (map instr->sd instr_list)) "$"))


;; this line will print out a SD of the UTM that can be fed back to itself.
;; 1. expand_ops turns instructions with multiple operations into a string
;;    of several double operation instructions
;; 2. fix_print then fixes all the "scan Any symbol" and "print nothing"
;;    cases by replacing them with instructions for every possible scanned
;;    symbol
;; 3. normalize_mconfigs converts the named mconfigs to integers
;; 4. instr_list->sd turns the list of instructions into a SD string
;;    that can be read by the UTM (i.e. it produces a tape to be fed
;;    into the UTM
(instr_list->sd (normalize_mconfigs (fix_print (expand_ops utm))))

