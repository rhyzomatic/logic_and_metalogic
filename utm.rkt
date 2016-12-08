#lang racket

(struct sk (mconfig_to_call mtable) #:transparent)
(struct instr (mconfig read_symbol ops final_mconfig) #:transparent)

(define (printsk s)
  (displayln (sk-mconfig_to_call s))
  (for-each displayln (sk-mtable s)))

;; builds the new m-config name from a list of symbols or sk's (as nesting
;; skeleton tables in one instruction line will thus return sk type
;; instead of a symbol itself)
(define (build_mconfig . symbols)
  (string->symbol (string-join (map symbol->string
         (map (lambda (s) (cond [(sk? s) (sk-mconfig_to_call s)]
                                [else s]))
              symbols)) "")))

(define (expand_table mconfigs)
  (cond [(null? mconfigs) '()]
        [(symbol? (instr-final_mconfig (car mconfigs)))
           (cons (car mconfigs) (expand_table (cdr mconfigs)))]
        [(instr? (car mconfigs)) (expand_table (append (car mconfigs) (cdr mconfigs)))]
        ;; if the final_mconfig of the instr is itself a skeleton table,
        ;; we need to expand it and ensure that its table gets properly appended
        [(sk? (instr-final_mconfig (car mconfigs)))
           (cons (struct-copy instr (car mconfigs) 
                              [final_mconfig (sk-mconfig_to_call (instr-final_mconfig (car mconfigs)))])
                 (append (expand_table (sk-mtable (instr-final_mconfig (car mconfigs))))
                         (expand_table (cdr mconfigs))))]
        [else (raise "expand_table encountered found a poorly formed instr in mconfigs")]))

(define (append_symbols . symbols)
  (string->symbol (string-join (map symbol->string symbols) "")))
(define s+ append_symbols)

;; creates a lil function that simply appends the passed in symbol
;; onto base returning a new symbol
;; note: since you can't have number symbols in racket (wtf?) we
;; have to do that weird string thing
(define (mconfig_builder base)
  (lambda (postfix) 
    (append_symbols base 
        (cond [(symbol? postfix) postfix]
              [(number? postfix) (string->symbol (number->string postfix))]
              [else (raise "mconfig_builder got not a symbol or number")]))))

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
(define (erase_first C B alpha)
  (let* ([this_mconfig (build_mconfig 'erase_first< C '~ B '~ alpha '>)]
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
          (list (instr this_mconfig 'Else 'N (erase_first this_mconfig B alpha)))))))

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

;; c(C, B, alpha)
(define (find_right C B alpha)
  (let* ([this_mconfig (build_mconfig 'find_right< C '~ B '~ alpha '>)]
         [m (mconfig_builder this_mconfig)])
    (sk this_mconfig
        (expand_table
          (list (instr this_mconfig 'Else 'N (find (right C) B alpha)))))))

;; instr_pattern: '(instr this_mconfig beta 'Else 'N (find B C beta))
(define (sub_symbol symbols_left instr_pattern)
  (cond [(null? symbols_left) '()]
        [else
          (letrec 
            [placeholder (caddr instr_pattern)]
            [sub (lambda (p sym)
                   (cond [(equal? placeholder (car p))
                           (cons symbol (sub (cdr p) sym))]
                         [(list? (car p))
                           (cons (sub (car p) sym) (sub (cdr p) sym))]
                         [else
                           (cons (car p) (sub (cdr p) sym))]))])
            (cons (sub instr_pattern (car symbols_left))
                  (sub_symbol (cdr symbols_left) instr_pattern))]))



