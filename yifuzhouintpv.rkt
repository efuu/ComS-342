#lang racket
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out))

;;-------------------------
;;-------------------------
;;The first problom

;;define whether it is the expr
(define (isexpr s)
  (or (number? s) (or (symbol? s) (isopexpr s))))
(trace isexpr)

;;define whether it is opexpr
(define (isopexpr s)
  (or (isarithexpr s) (or (iscondexpr s) (isletexpr s))))
(trace isopexpr)

;;define whether it is arithexpr
(define (isarithexpr s)
  (if (equal? (length s) 3)
      (and (isop (car s)) (and (isexpr (car (cdr s))) (isexpr (car (cdr (cdr s))))))
      false))
(trace isarithexpr)

;;define whether it is condexpr
(define (iscondexpr s)
  (if (equal? (length s) 3)
      (and (isccond (car s)) (and (isexpr (car (cdr s))) (isexpr (car (cdr (cdr s))))))
      false))
(trace iscondexpr)

;;define whether it is letexpr
(define (isletexpr s)
  (if (equal? (length s) 3)
      (and (islet (car s)) (and (isvarassign (car (cdr s))) (isexpr (car (cdr (cdr s))))))
      ;;This can be change for the problem 3
      ;;(and (islet (car s)) (and (or (isvarassignseq (car (cdr s))) (isvarassign (car (cdr s)))) (isexpr (car (cdr (cdr s))))))
      false))
(trace isletexpr)

;;define whether is op symbol
(define (isop s)
  (if (equal? s '+)
      true
      (if (equal? s '-)
          true
          (if (equal? s '*)
              true
              (if (equal? s '/)
                  true
                  false)))))
(trace isop)

;;define whether it is ccond
(define (isccond s)
  (if (equal? (isbcond s) true)
      true
      (if (equal? (length s) 3)
          (and (isorand (car s)) (and (isccond (car (cdr s))) (isccond (car (cdr (cdr s))))))
          (if (equal? (length s) 2)
              (and (isnot (car s)) (isccond (car (cdr s))))
              false))))
(trace isccond)

;;define whether it is symbol let
(define (islet s)
  (if (equal? s 'let)
      true
      false))
(trace islet)

;;define whether it is varassign
(define (isvarassign s)
  (if (equal? (length s) 2)
      (and (symbol? (car s)) (isexpr (car (cdr s))))
      false))
(trace isvarassign)

;;define whether it is bcond
(define (isbcond s)
  (if (equal? (length s) 3)
      (and (isgle (car s)) (and (isexpr (car (cdr s))) (isexpr (car (cdr (cdr s))))))
      false))
(trace isbcond)

;;define whether it is or & and
(define (isorand s)
  (or (equal? s 'or) (equal? s 'and)))
(trace isorand)

;;define whether it is symbol not
(define (isnot s)
  (equal? s 'not))
(trace isnot)

;;define whether it is gt, lt, eq
(define (isgle s)
  (or (equal? s 'gt) (or (equal? s 'lt) (equal? s 'eq))))
(trace isgle)

;;count the length of the list
(define (length lst)
  (if (not (list? lst))
      false
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))

;;check function
(define (synchk prog)
  (isexpr prog))
(trace synchk)

;;-------------------------
;;-------------------------
;;The second problom

;;evaluate whether it is expr
(define (evalexpr s env)
  (if (number? s)
      s
      (if (symbol? s)
          (findvar s env)
          (if (isopexpr s)
              (findopexpr s env)
              '(Cannot Evaluate)))))
(trace evalexpr)

;;evaluate whether it is var
(define (findvar var env)
  (if (null? env)
      '(exception free-variable)
      (if (equal? (car (car env)) var)
          (car (cdr (car env)))
          (findvar var (cdr env)))))
(trace findvar)

;;evaluate whether it opexpr
(define (findopexpr s env)
  (if (isarithexpr s)
      (findarithexpr s env)
      (if (iscondexpr s)
          (findcondexpr s env)
          (if (isletexpr s)
              (findletexpr s env)
              '(Cannot Evaluate)))))
(trace findopexpr)

;;evaluate arithexpr
(define (findarithexpr s env)
  (if (equal? (car s) '+)
      (+ (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
      (if (equal? (car s) '-)
          (- (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
          (if (equal? (car s) '*)
              (* (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
              (if (equal? (car s) '/)
                  (/ (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
                  '(Cannot Evaluate))))))
(trace findarithexpr)

;;evaluate condexpr
(define (findcondexpr s env)
  (if (findccond (car s) env)
      (evalexpr (car (cdr s)) env)
      (evalexpr (car (cdr (cdr s))) env)))
(trace findcondexpr)

;;evaluate ccond
(define (findccond s env)
  (if (isbcond s)
      (findbcond s env)
      (if (equal? (car s) 'and)
          (and (findccond (car (cdr s)) env) (findccond (car (cdr (cdr s))) env))
          (if (equal? (car s) 'or)
              (or (findccond (car (cdr s)) env) (findccond (car (cdr (cdr s))) env))
              (if (equal? (car s) 'not)
                  (not (findccond (car (cdr s)) env))
                  '(Cannot Evaluate))))))
(trace findccond)

;;evaluate bcond
(define (findbcond s env)
  (if (equal? (car s) 'gt)
      (if (> (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
          true
          false)
      (if (equal? (car s) 'lt)
          (if (< (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
          true
          false)
          (if (equal? (car s) 'eq)
              (if (equal? (evalexpr (car (cdr s)) env) (evalexpr (car (cdr (cdr s))) env))
              true
              false)
              '(Cannot Evaluate)))))
(trace findbcond)

;;evaluate letexpr
;;put the varassign to the env
(define (findletexpr s env)
  (if (isvarassign (car (cdr s)))
      (evalexpr (car (cdr (cdr s))) (varassign (car (cdr s)) env))
     '(Cannot Evaluate)))         
(trace findletexpr)

;;This is the change for problem 3 if it want to check and can be combine problem 2 and 3 together
;;(define (findletexpr s env)
;;  (if (isvarassign (car (cdr s)))
;;      (evalexpr (car (cdr (cdr s))) (varassign (car (cdr s)) env))
;;      (if (isvarassignseq (car (cdr s)))
;;          (findvarassignseq s env)
;;          '(Cannot Evaluate))))
          

;;evaluate varassign
;;keep adding the var to env
(define (varassign pair env)
  (cons (list (car pair) (evalexpr (car (cdr pair)) env)) env))
(trace varassign)

;;eval function
(define (eval prog env)
  (evalexpr prog env))
(trace eval)

;;-------------------------
;;-------------------------
;;The third problom
;;In this function, the eval-new works only on the program is correct and base on the extend varassign
;;I have to copy the code both from problem 1 and 2 and change a litte

;;define whether it is the expr
(define (isexpr-new s)
  (or (number? s) (or (symbol? s) (isopexpr-new s))))
(trace isexpr-new)

;;define whether it is opexpr
(define (isopexpr-new s)
  (or (isarithexpr-new s) (or (iscondexpr-new s) (isletexpr-new s))))
(trace isopexpr-new)

;;define whether it is arithexpr
(define (isarithexpr-new s)
  (if (equal? (length s) 3)
      (and (isop (car s)) (and (isexpr-new (car (cdr s))) (isexpr-new (car (cdr (cdr s))))))
      false))
(trace isarithexpr-new)

;;define whether it is condexpr
(define (iscondexpr-new s)
  (if (equal? (length s) 3)
      (and (isccond-new (car s)) (and (isexpr-new (car (cdr s))) (isexpr-new (car (cdr (cdr s))))))
      false))
(trace iscondexpr-new)

;;define whether it is letexpr
(define (isletexpr-new s)
  (if (equal? (length s) 3)
      ;;This can be change for the problem 3
      (and (islet (car s)) (and (or (isvarassignseq (car (cdr s))) (isvarassign-new (car (cdr s)))) (isexpr-new (car (cdr (cdr s))))))
      false))
(trace isletexpr-new)


;;define whether it is ccond
(define (isccond-new s)
  (if (equal? (isbcond-new s) true)
      true
      (if (equal? (length s) 3)
          (and (isorand (car s)) (and (isccond-new (car (cdr s))) (isccond-new (car (cdr (cdr s))))))
          (if (equal? (length s) 2)
              (and (isnot (car s)) (isccond-new (car (cdr s))))
              false))))
(trace isccond-new)

;;define whether it is varassign
(define (isvarassign-new s)
  (if (equal? (length s) 2)
      (and (symbol? (car s)) (isexpr-new (car (cdr s))))
      false))
(trace isvarassign-new)

;;define whether it is bcond
(define (isbcond-new s)
  (if (equal? (length s) 3)
      (and (isgle (car s)) (and (isexpr-new (car (cdr s))) (isexpr-new (car (cdr (cdr s))))))
      false))
(trace isbcond-new)

(define (evalexpr-new s env)
  (if (number? s)
      s
      (if (symbol? s)
          (findvar s env)
          (if (isopexpr-new s)
              (findopexpr-new s env)
              '(Cannot Evaluate)))))
(trace evalexpr-new)

;;evaluate whether it is var
(define (findvar-new var env)
  (if (null? env)
      '(exception free-variable)
      (if (equal? (car (car env)) var)
          (car (cdr (car env)))
          (findvar-new var (cdr env)))))
(trace findvar-new)

;;evaluate whether it opexpr
(define (findopexpr-new s env)
  (if (isarithexpr-new s)
      (findarithexpr-new s env)
      (if (iscondexpr-new s)
          (findcondexpr-new s env)
          (if (isletexpr-new s)
              (findletexpr-new s env)
              '(Cannot Evaluate)))))
(trace findopexpr-new)

;;evaluate arithexpr
(define (findarithexpr-new s env)
  (if (equal? (car s) '+)
      (+ (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
      (if (equal? (car s) '-)
          (- (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
          (if (equal? (car s) '*)
              (* (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
              (if (equal? (car s) '/)
                  (/ (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
                  '(Cannot Evaluate))))))
(trace findarithexpr-new)

;;evaluate condexpr
(define (findcondexpr-new s env)
  (if (findccond-new (car s) env)
      (evalexpr-new (car (cdr s)) env)
      (evalexpr-new (car (cdr (cdr s))) env)))
(trace findcondexpr-new)

;;evaluate ccond
(define (findccond-new s env)
  (if (isbcond-new s)
      (findbcond-new s env)
      (if (equal? (car s) 'and)
          (and (findccond-new (car (cdr s)) env) (findccond-new (car (cdr (cdr s))) env))
          (if (equal? (car s) 'or)
              (or (findccond-new (car (cdr s)) env) (findccond-new (car (cdr (cdr s))) env))
              (if (equal? (car s) 'not)
                  (not (findccond-new (car (cdr s)) env))
                  '(Cannot Evaluate))))))
(trace findccond-new)

;;evaluate bcond
(define (findbcond-new s env)
  (if (equal? (car s) 'gt)
      (if (> (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
          true
          false)
      (if (equal? (car s) 'lt)
          (if (< (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
          true
          false)
          (if (equal? (car s) 'eq)
              (if (equal? (evalexpr-new (car (cdr s)) env) (evalexpr-new (car (cdr (cdr s))) env))
              true
              false)
              '(Cannot Evaluate)))))
(trace findbcond-new)

;;evaluate letexpr
;;put the varassign to the env
(define (findletexpr-new s env)
  (if (isvarassign-new (car (cdr s)))
      (evalexpr-new (car (cdr (cdr s))) (varassign-new (car (cdr s)) env))
      (if (isvarassignseq (car (cdr s)))
          (findvarassignseq s env)
         '(Cannot Evaluate))))       
(trace findletexpr-new)


;;evaluate varassign
;;keep adding the var to env
(define (varassign-new pair env)
  (cons (list (car pair) (evalexpr-new (car (cdr pair)) env)) env))
(trace varassign-new)

;;eval function
(define (eval-new prog env)
  (evalexpr-new prog env))
(trace eval-new)

;;check whether it is varassignseq
(define (isvarassignseq s)
      (if (isvarassign-new (car s))
          (if (equal? (length s) 1)
              true
              (isvarassignseq (cdr s)))
          false))
(trace isvarassignseq)

;;evaluate the function based on the extend varassign
(define (findvarassignseq s env)
  (if (null? (car (cdr s)))
      (evalexpr (car (cdr (cdr s))) env)
      (findvarassignseq (append (append (list (car s)) (list (append '() (cdr (car (cdr s)))))) (cdr (cdr s))) (varassign (car (car (cdr s))) env))))
(trace findvarassignseq)
