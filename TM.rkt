#lang racket
(require (planet dyoo/simply-scheme:2:2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TM, version 1.0 - November 27, 2016
;; by Dr K. Darcy Otto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Readme
;; ======
;; 
;; This Racket Program (in Simply Scheme) is intended to simulate a Turing Machine according to the format offered in Alan Turing's paper "On Computable Numbers, with a Application to the Entscheidungsproblem," _Proceedings of the London Mathematical Society_, Ser 2, Vol. 42, 1937, 230–65.
;; 
;; (machine '(<starting tape> <starting position> <starting config>)
;;          '(<maximum number of steps> <delay between steps> <print every nth step>)
;;          '(<instructions>)) 
;; 
;; <starting tape>: The initial state of the tape.
;; <starting position>: The intiial position of the read/write head.
;; <starting config>: The initial m-config of the machine.
;; 
;; Use "_" to indicate a blank, and 0 to indicate the first square.  If <starting position> is greater than the length of <starting tape>, then blanks will be added to pad out the tape.
;; 
;; Examples of '(<starting tape> <starting position> <starting config>):
;; 
;; '(_ 0 b): Tape has one blank, read/write head starts at position 0, and m-config is "b".
;; '(0_1_0 2 f): Tape is "0_1_0), read/write head starts at position 1 (beneath the "1"), and m-config is "f"
;; 
;; <maximum number of steps>: the maximum number of steps before the program automatically terminates
;; <delay between steps>: the delay, in seconds, after printing each step
;; <print every nth step>: print every nth step; used to speed up operations
;; 
;; Examples of '(<maximum number of steps> <delay between steps> <print every nth step>):
;; 
;; '(1000 0 1): Stop after 1000 steps, no delay after printing each step, print every step.
;; '(10000 1 100): Stop after 10000 steps, 1 second delay after printing each step, print every 100th step.
;; 
;; <instructions>: a finite list of parenthesized instructions, each with four elements: (<m-config> <symbol> <operations> <final m-config>)
;; 
;; <m-config>: the m-config for the instruction
;; <symbol>: the symbol in the scanned square
;; <operations>: one or more operations
;; <final m-config> the m-config after the operations are complete
;; 
;; The <m-config> and <symbol> together are the configuration; the <operations> and <final m-config> are the behaviour.
;; 
;; <symbol> can be None (matches blanks), Any (matches non-blanks), * (matches non-blanks) or an individual character.  The complement of an individual character is expressed by !.
;; 
;; <operations> can be a single operation, or multiple operations separated by spaces; multiple operations must be set in paranthesis and separated by spaces.  N or _ specifies no operation.
;; 
;; <final m-config> may be any valid m-configuration, but may also be "halt" to stop the machine.
;; 
;; Examples of <instructions>:
;; 
;; '((b None P0 c)): For m-configuration b, when scanned square is blank, print 0, change to m-configuration c.
;; '((c Any R d)): For m-configuration b, when scanned square is non-blank, move right one square, change to m-configuration d
;; '((d !a L e)): For m-configuration d, when scanned square is not "a", move left one square, change to m-configuration e
;; '((e @ (R P0) halt): for m-configuration e, whne scanned square is "@", move right one square and print "0", then halt
;; 
;; Machine Examples:
;; 
;; 1. From "On Computable Numbers, with a Application to the Entscheidungsproblem," p. 233.
;; 
;; (machine '(_ 0 b)
;;          '(20 0 1)
;;          '((b None P0 b)
;;            (b 0 (R R P1) b)
;;            (b 1 (R R P0) b)))
;; 
;; Result:
;; (Step: 20 - Config: b)
;; 0_1_0_1_0_1_0_1_0_1_0_1_0_1_0_1_0_1_0_1
;; --------------------------------------^
;; 
;; 2. 2-state Busy Beaver
;; 
;; (machine '("0000000000" 5 a)
;;          '(10 0 1)
;;          '((a 0 (P1 R) b)
;;            (a 1 (P1 L) b)
;;            (b 0 (P1 L) a)
;;            (b 1 (P1 R) halt)))
;; 
;; Result:
;; (Step: 6 - Config: halt)
;; 0001111000
;; -----^----
;; 
;; 3. 4-state Busy Beaver
;; 
;; (machine '(_ 0 0)
;;          '(110 0 1)
;;          '((0 None (P1 R) 1)
;;            (0 0 E 0)
;;            (0 1 (P1 L) 1)
;;            (1 None (P1 L) 0)
;;            (1 0 E 0)
;;            (1 1 (E L) 2)
;;            (2 None (P1 R) halt)
;;            (2 0 E 0)
;;            (2 1 (P1 L) 3)
;;            (3 None (P1 R) 3)
;;            (3 0 E 0)
;;            (3 1 (E R) 0)))
;; 
;; Result:
;; (Step: 107 - Config: halt)
;; 1_111111111111
;; -^------------
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get everything rolling
(define (machine init config instructions)
  (multiplesteps (car init) (car (cdr init)) (car (cdr (cdr init))) instructions 0 (car config) (car (cdr config)) (car (cdr (cdr config)))))

;; Execute multiple instructions
(define (multiplesteps tape position config instructions counter steps delay printdelay)
  (let ((takestep (step tape position config instructions counter printdelay)))
    (cond ((< steps 1) 'end)
          (takestep (begin
                      (sleep delay)
                      (multiplesteps (car takestep) (car (cdr takestep)) (car (cdr (cdr takestep))) instructions (+ counter 1) (- steps 1) delay printdelay)))
          (else (if (equal? config 'halt) #t #f)))))

;; Sets the initial machine state.
(define (init tape position config)
  (list tape position config))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moves head one to the left.  Returns state.
(define (left tape position config)
  (if (= position 0)
      (list (word '_ tape) 0 config)
      (list tape (- position 1) config)))

;; Moves head one to the right.  Returns state.
(define (right tape position config)
  (if (= (+ position 1) (count tape))
      (list (word tape '_) (+ position 1) config)
      (list tape (+ position 1) config)))

;; Print char on tape at postion.  Returns state.
(define (print tape position config char)
  (list (word (lefttape tape position) char (righttape tape position)) position config))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determine whether operation is single or multi
(define (act tape position operation final)
  (cond ((list? operation) (multiact tape position operation final))
        (else (singleact tape position operation final))))

;; Act on a single operation
(define (singleact tape position operation final)
  (cond ((equal? operation 'L) (left tape position final))
        ((equal? operation 'R) (right tape position final))
        ((equal? operation 'E) (erase tape position final))
        ((equal? operation 'N) (list tape position final))
        ((equal? operation '_) (list tape position final))
        ((equal? (first operation) 'P) (print tape position final (last operation)))
        (else #f)))

;; Step through multioperation list so that it becomes a series of single operations
(define (multiact tape position operation final)
  (cond ((empty? operation) (list tape position final))
        (else (let ((result (singleact tape position (car operation) final)))
                (multiact (car result) (car (cdr result)) (cdr operation) final)))))

;; Execute a single instruction
(define (step tape position config instructions counter printdelay)
  (begin
    (printtape tape position config counter printdelay)
    (let ((matchedinstruction (matchinstruction tape position config instructions)))
      (cond (matchedinstruction (act tape position (getoperation matchedinstruction) (getfinal matchedinstruction)))
            (else #f)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finds first instruction in instructions list that matches.  If not found, returns #f.
(define (matchinstruction tape position config instructions)
  (cond ((null? instructions) #f)
        ((instructionmatches? tape position config (car instructions)) (car instructions))
        (else (matchinstruction tape position config (cdr instructions)))))

;; Look at a particular instruction, and determine whether it matches the current config and symbol.
;; "Any" matches any symbol that is not a blank; "None" matches blanks (cf. 234)
(define (instructionmatches? tape position config instruction)
  (and (or (equal? (atposition tape position) (getsymbol instruction))
           (and (equal? (getsymbol instruction) '*) (not (equal? (atposition tape position) '_)))
           (and (equal? (first (getsymbol instruction)) '!) (not (equal? (atposition tape position) (last (getsymbol instruction))))))
       (equal? config (getstart instruction))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is an helper function, returns everything to the left of the position on tape.
(define (lefttape tape position)
  ((repeated butlast (- (count tape) position)) tape))

;; This is an helper function, returns everything to the right of the position on tape.
(define (righttape tape position)
  ((repeated butfirst (+ position 1)) tape))

;; This is an helper function, returns the character at position.
(define (atposition tape position)
  (first ((repeated butfirst position) tape)))

;; This is a helper function, prints a blank at position.
(define (erase tape position config)
  (print tape position config '_))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Isolate the starting state
(define (getstart instruction)
  (car instruction))

;; Isolate the symbol
(define (getsymbol instruction)
  (let ((symbol (car (cdr instruction))))
    (cond ((or (equal? symbol 'None) (equal? symbol 'none)) '_)
          ((or (equal? symbol 'Any) (equal? symbol 'any)) '*)
          (else symbol))))

;; Isolate the operation
(define (getoperation instruction)
  (car (cdr (cdr instruction))))

;; Isolate the final state
(define (getfinal instruction)
  (last instruction))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print tape
(define (printtape tape position config counter printdelay)
  (cond ((equal? (modulo counter printdelay) 0)
         (begin
           (display (se 'Step: counter '- 'Config: config)) (newline)
           (display tape) (newline)
           (cond ((= position 0) (display (word '^ (blanks (- (count tape) 1)))) (newline))
                 ((= position (- (count tape) 1)) (display (word (blanks position) '^)) (newline))
                 ((and (> position 0) (< position (- (count tape) 1))) (display (word (blanks position) '^ (blanks (- (- (count tape) position) 1)))) (newline))
                 ((and (> position 0) (< position (- (count tape) 1))) (display (string->uninterned-symbol (string-append (blanks position) "^" (blanks (- (- (count tape) position) 1))))) (newline))
                 (else #f))))))

;; Add padding
(define (blanks cnt)
  (if (< cnt 2)
      "-"
      (string-append "-" (blanks (- cnt 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enter Machine Code Below This Line
;;

;; (machine '(_ 0 b)
;;          '(20 0 1)
;;          '((b None P0 b)
;;            (b 0 (R R P1) b)
;;            (b 1 (R R P0) b)))

(provide machine)











