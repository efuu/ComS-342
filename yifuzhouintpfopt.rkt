

#lang racket
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out))

;;------------------------------------Question 1------------------------------
;; My solution to the question 1 is to update the production rule
;; as follow:
;;FExpr -> (letp FAssign Expr)
;;FAssign -> ((FName FormalParams) Expr)
;;FormalParams -> (var) | (var FormalParamList)
;;FormalParamList -> Var | Var FormalParamList
;;ApplyF -> (apply (FName Args))
;;Args -> ('static) | ('dynamic) | ('static ArgList) | ('dynamic ArgList)
;;ArgList -> Expr | Expr ArgList


;;------------------------------------Question 2------------------------------
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;
;;;;;;;;
;;;;
;;
;
(define (eval expr env)
;;  (cond
;;    [ (not (nofree expr env)) '(Cannot Evaluate) ]  ;; if the expression contains free variable, do not evaluate
   (myeval expr env))   ;; updated the version from hw4: letexpression and var
;;(trace eval)

(define (myeval expr env)
  (cond
    [ (number? expr)  expr ]
    [ (symbol? expr)  (findvalue expr env) ] ;; new rule
    [ (islet (car expr)) (evallet expr env) ]  ;; new rule
    [ (isletp (car expr)) (evalfexpr expr env) ]
    [ (isapply (car expr)) (evalfapply expr env) ]
    [ (isarith (car expr)) (evalarith expr env) ] ;; old rule with env parameter
    [ (iscond (car expr))  (if (evalcondition (car expr) env)  ;; old rule with env parameter
                               (myeval (cadr expr) env)       
                               (myeval (cadr (cdr expr)) env)) ]
    
    ))
;;(trace myeval)

(define (islet x)
  (if (equal? x 'let)
      true
      false))

(define (isletp x)
  (if (equal? x 'letp)
      true
      false))

(define (isapply x)
  (if (equal? x 'apply)
      true
      false))

;;count the length of the list
(define (length lst)
  (if (not (list? lst))
      false
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))

;; new addition: see the notes
(define (findvalue var env)
  (if (equal? var (car (car env))) ;; We already know expression does not contain free variables
      (cadr (car env))
      (findvalue var (cdr env))))
;;(trace findvalue)

;;eval the fexpr
(define (evalfexpr expr env)
  (myeval (cadr (cdr expr)) (cons (car (cdr expr)) env)))
;;(trace evalfexpr)

;;match the function name and return that function expr and that function arguments
(define (matchfunname name env)
  (if (null? env)
      false
      (if (equal? (length (car (car env))) 2)
          (if (equal? (car (car (car env))) name)
              (cons (cadr (car env)) (list (cadr (car (car env)))))
              (matchfunname name (cdr env)))
          (matchfunname name (cdr env)))))
;;(trace matchfunname)

;;eval the fapply
(define (evalfapply expr env)
  (if (equal? (matchfunname (car (car (cdr expr))) env) false)
      '(Cannot Evaluate)
      (if (equal? (car (cadr (cadr expr))) ''dynamic)
      (myeval (car (matchfunname (car (car (cdr expr))) env)) (evalfapplyenv (cdr (cadr (cadr expr))) (cdr (cadr (matchfunname (car (car (cdr expr))) env))) env))
      (myeval (car (matchfunname (car (car (cdr expr))) env)) (staticfun expr (cadr (matchfunname (car (car (cdr expr))) env)) env))
          )))
;;(trace evalfapply)

;;do if it is static
;;and ruturn the env that the first element it is that function
(define (staticfun exprapply listfun env)
  (if (equal? (length (car (car env))) false)
      (staticfun exprapply listfun (cdr env))
  (if (equal? (car (cadr exprapply)) (car (car (car env))))
      (evalfapplyenv (cdr (cadr (cadr exprapply))) (cdr listfun) env)
      (staticfun exprapply listfun (cdr env)))))
;;(trace staticfun)

;;give the apply function's arguments
;;save the var in env and return env
(define (evalfapplyenv exprapply listfun env)
  (if (null? exprapply)
      env
      (evalfapplyenv (cdr exprapply) (cdr listfun) (cons (list (car listfun) (myeval (car exprapply) env)) env))))
;;(trace evalfapplyenv)  
  
;; new addition: see the notes
(define (evallet expr env)
  (myeval (cadr (cdr expr)) (cons (list (car (cadr expr)) (myeval (cadr (cadr expr)) env)) env)))
;;(trace evallet)

;;whether it arith operator
(define (isarith x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)))
;;(trace isarith)

;; for eval the arith
(define (evalarith expr env)
  (cond
    [ (equal? (car expr) '+) (+ (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '-) (- (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '*) (* (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '/) (/ (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    ))
;;(trace evalarith)

(define (iscond condition)
  (if (not (list? condition))
      false
  (or (equal? (car condition) 'gt)
      (equal? (car condition) 'lt)
      (equal? (car condition) 'eq)
      (equal? (car condition) 'or)
      (equal? (car condition) 'and)
      (equal? (car condition) 'not))))
;;(trace iscond)

(define (evalcondition condition env)
  (cond
    [ (equal? (car condition) 'gt)   (> (myeval (cadr condition) env) (myeval (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'lt)   (< (myeval (cadr condition) env) (myeval (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'eq)   (equal? (myeval (cadr condition) env) (myeval (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'or)   (or (evalcondition (cadr condition) env) (evalcondition (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'and)  (and (evalcondition (cadr condition) env) (evalcondition (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'not)  (not (evalcondition (cadr condition) env)) ]
    ))
;;(trace evalcondition)


