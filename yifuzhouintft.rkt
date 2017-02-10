

#lang racket
(require racket/trace)
;;(require "program.rkt")
(provide (all-defined-out))

;;define whether it is the expr
(define (isexpr s lst)
  (or (number? s) (or (symbol? s) (or (isopexpr s lst) (or (isfexpr s lst) (isapplyf s lst))))))
(trace isexpr)

;;define whether it is opexpr
(define (isopexpr s lst)
  (or (isarithexpr s lst) (or (iscondexpr s lst) (isletexpr s lst))))
(trace isopexpr)

;;define whether it is arithexpr
(define (isarithexpr s lst)
  (if (equal? (length s) 3)
      (and (isop (car s) lst) (and (isexpr (car (cdr s)) lst) (isexpr (car (cdr (cdr s))) lst)))
      false))
(trace isarithexpr)

;;define whether it is condexpr
(define (iscondexpr s lst)
  (if (equal? (length s) 3)
      (and (isccond (car s) lst) (and (isexpr (car (cdr s)) lst) (isexpr (car (cdr (cdr s))) lst)))
      false))
(trace iscondexpr)

;;define whether it is letexpr
(define (isletexpr s lst)
  (if (equal? (length s) 3)
      (and (islet (car s)) (and (isvarassign (car (cdr s)) lst) (isexpr (car (cdr (cdr s))) lst)))
      ;;This can be change for the problem 3
      ;;(and (islet (car s)) (and (or (isvarassignseq (car (cdr s))) (isvarassign (car (cdr s)))) (isexpr (car (cdr (cdr s))))))
      false))
(trace isletexpr)

;;define whether is op symbol
(define (isop s lst)
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
(define (isccond s lst)
  (if (equal? (isbcond s lst) true)
      true
      (if (equal? (length s) 3)
          (and (isorand (car s) lst) (and (isccond (car (cdr s)) lst) (isccond (car (cdr (cdr s))) lst)))
          (if (equal? (length s) 2)
              (and (isnot (car s) lst) (isccond (car (cdr s)) lst))
              false))))
(trace isccond)

;;define whether it is symbol let
(define (islet s)
  (if (equal? s 'let)
      true
      false))
(trace islet)

;;define whether it is varassign
(define (isvarassign s lst)
  (if (equal? (length s) 2)
      (and (symbol? (car s)) (isexpr (car (cdr s)) lst))
      false))
(trace isvarassign)

;;define whether it is bcond
(define (isbcond s lst)
  (if (equal? (length s) 3)
      (and (isgle (car s) lst) (and (isexpr (car (cdr s)) lst) (isexpr (car (cdr (cdr s))) lst)))
      false))
(trace isbcond)

;;define whether it is or & and
(define (isorand s lst)
  (or (equal? s 'or) (equal? s 'and)))
(trace isorand)

;;define whether it is symbol not
(define (isnot s lst)
  (equal? s 'not))
(trace isnot)

;;define whether it is gt, lt, eq
(define (isgle s lst)
  (or (equal? s 'gt) (or (equal? s 'lt) (equal? s 'eq))))
(trace isgle)

;;count the length of the list
(define (length lst)
  (if (not (list? lst))
      false
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))

;;define whether it is fexpr
(define (isfexpr s lst)
  (if (equal? (length s) 3)
  (and (isletp (car s)) (and (isfassign (car (cdr s)) lst) (isexpr (car (cdr (cdr s))) (add (car (cdr s)) lst))))
  false))
(trace isfexpr)

;;define whether it is symbol 'letp
(define (isletp s)
  (equal? s 'letp))
(trace isletp)

;;define whether it is fassign
(define (isfassign s lst)
  (if (equal? (length (car s)) 2)
      (and (isfnamein (car (car s)) lst) (and (isformalparams (car (cdr (car s))) lst) (isexpr (car (cdr s)) (add s lst))))
      false))
(trace isfassign)

;;add the pair to the envirnment
(define (add s lst)
  (cons (list (car (car s)) (length (car (cdr (car s))))) lst))
(trace add)
;;define whether it is symbol 'apply
(define (isapply s)
  (equal? s 'apply))
(trace isapply)

;;save the function name in the envirnment
(define (isfnamein s lst)
  (if (symbol? s)
      true
      false))
(trace isfnamein)


;;define whether it is applyf
(define (isapplyf s lst)
  (if (equal? (length s) 2)
      (if (isapply (car s))
          (if (equal? (length (car (cdr s))) 2)
              (and (isfnamematch s lst) (isargs (car (cdr (car (cdr s)))) lst))
              false)
          false)
      false))
(trace isapplyf)

;;define whether the fname can match the lst
(define (isfnamematch s lst)
  (if (null? lst)
      false
      (if (equal? (car (car (cdr s))) (car (car lst)))
          (if (equal? (length (car (cdr (car (cdr s))))) (car (cdr (car lst))))
              true
              (isfnamematch s (cdr lst)))
          (isfnamematch s (cdr lst)))))
(trace isfnamematch)


;;cut the match element
(define (cut s lst)
  (cdr lst))
(trace cut)
;;define whether it is args
(define (isargs s lst)
  (if (null? s)
      true
      (if (isarglist s lst)
          true
          false)))
(trace isargs)

;;define whether it is arglist
(define (isarglist s lst)
  (if (and (isexpr (car s) lst) (equal? (length s) 1))
      true
      (if (equal? (length s) 2)
          (and (isexpr (car s) lst) (isarglist (cdr s) lst))
          false)))
(trace isarglist)

;;define whether it is formalparams
(define (isformalparams s lst)
  (if (null? s)
      true
      (and (equal? (length s) 1) (isformalparamlist  s lst))))
(trace isformalparams)

;;define whether it is formalparamlist
(define (isformalparamlist s lst)
  (if (and (equal? (length s) 1) (symbol? (car s)))
      true
      (if (equal? (length s) 2)
          (and (symbol? (car s)) (isformalparamlist (cdr s) lst))
          false)))
(trace isformalparamlist)

;;
(define (synchk prog)
  (isexpr prog '())
  )
(trace synchk)




;;;;;;;;;;;;;;;;;;;;;
;;Qustion 2
;; nofree number: true
;;        var: if present in the boundvar list
;;        let (var expr1) expr2:  expr1 does not have free variables and expr2 does not have free variables 
(define (nofree expr boundvars)
  (cond
    [ (number? expr) true ]
    [ (symbol? expr) (if (mymember expr boundvars) true false) ]
    [ (islet (car expr)) (and (nofree (cadr (cadr expr)) boundvars) (nofree (cadr (cdr expr)) (cons (list (car (cadr expr)) (eval (cadr (cadr expr)) boundvars)) boundvars))) ] 
    [ (isarith (car expr)) (and (nofree (cadr expr) boundvars) (nofree (cadr (cdr expr)) boundvars)) ]
    [ (iscond (car expr)) (and (nofreecondition (car expr) boundvars)
                               (nofree (cadr expr) boundvars)
                             (nofree (cadr (cdr expr)) boundvars)) ]
    [ else true ] 
    ))
(trace nofree)
;;(cons (list (car (cadr expr)) (myeval (cadr (cadr expr)) env envf)) env)

(define (mymember x lst)
  (if (null? lst)
      false
      (if (equal? x (car (car lst)))
          true
          (mymember x (cdr lst)))))
(trace mymember)

(define (nofreecondition condition boundvars)
  (cond
    [ (or (equal? (car condition) 'gt)
          (equal? (car condition) 'lt)
          (equal? (car condition) 'eq))  (and (nofree (cadr condition) boundvars) (nofree (cadr (cdr condition)) boundvars)) ]
    [ (or (equal? (car condition) 'or)
          (equal? (car condition) 'and)) (and (nofreecondition (cadr condition) boundvars) (nofreecondition (cadr (cdr condition)) boundvars)) ]
    [ else (nofreecondition (cadr condition)) ]
    ))
(trace nofreecondition)      
;;;;;;;;;;;;;;;;;;;;

(define (eval expr env)
  (cond
    [ (not (nofree expr env)) '(Cannot Evaluate) ]  ;; if the expression contains free variable, do not evaluate
    [ (myeval expr env '()) ]))   ;; updated the version from hw4: letexpression and var
(trace eval)

(define (myeval expr env envf)
  (cond
    [ (number? expr)  expr ]
    [ (symbol? expr)  (findvalue expr env envf) ] ;; new rule
    [ (islet (car expr)) (evallet expr env envf) ]  ;; new rule
    [ (isletp (car expr)) (evalfexpr expr env envf) ]
    [ (isapply (car expr)) (evalfapply expr env envf) ]
    [ (isarith (car expr)) (evalarith expr env envf) ] ;; old rule with env parameter
    [ (iscond (car expr))  (if (evalcondition (car expr) env envf)  ;; old rule with env parameter
                               (myeval (cadr expr) env envf)       
                               (myeval (cadr (cdr expr)) env envf)) ]
    
    ))
(trace myeval)



;; new addition: see the notes
(define (findvalue var env envf)
  (if (equal? var (car (car env))) ;; We already know expression does not contain free variables
      (cadr (car env))
      (findvalue var (cdr env) envf)))
(trace findvalue)

;;eval the fexpr
(define (evalfexpr expr env envf)
  (myeval (cadr (cdr expr)) env (cons (car (cdr expr)) envf)))
(trace evalfexpr)



;;match the function name and return the arg
(define (matchfunname name env envf)
  (if (null? envf)
      false
      (if (equal? (car (car (car envf))) name)
          (cadr (car (car envf)))
          (matchfunname name env (cdr envf)))))
(trace matchfunname)

;;eval the fapply
(define (evalfapply expr env envf)
  (if (null? (matchfunname (car (car (cdr expr))) env envf))
      (myeval expr env)
      (myevalfapply envf (cons (list (car (matchfunname (car (car (cdr expr))) env envf)) (myeval (car (cadr (cadr expr))) env envf)) env) envf)))
(trace evalfapply)

;;define myevalfapply
(define (myevalfapply expr env envf)
  (myeval (cadr (car expr)) env envf))
 (trace myevalfapply)
  
      
(trace myevalfapply)

  
;; new addition: see the notes
(define (evallet expr env envf)
  (myeval (cadr (cdr expr)) (cons (list (car (cadr expr)) (myeval (cadr (cadr expr)) env envf)) env) envf))
(trace evallet)

;;whether it arith operator
(define (isarith x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)))
(trace isarith)

;; for eval the arith
(define (evalarith expr env envf)
  (cond
    [ (equal? (car expr) '+) (+ (myeval (cadr expr) env envf) (myeval (cadr (cdr expr)) env envf)) ]
    [ (equal? (car expr) '-) (- (myeval (cadr expr) env envf) (myeval (cadr (cdr expr)) env envf)) ]
    [ (equal? (car expr) '*) (* (myeval (cadr expr) env envf) (myeval (cadr (cdr expr)) env envf)) ]
    [ (equal? (car expr) '/) (/ (myeval (cadr expr) env envf) (myeval (cadr (cdr expr)) env envf)) ]
    ))
(trace evalarith)

(define (iscond condition)
  (if (not (list? condition))
      false
  (or (equal? (car condition) 'gt)
      (equal? (car condition) 'lt)
      (equal? (car condition) 'eq)
      (equal? (car condition) 'or)
      (equal? (car condition) 'and)
      (equal? (car condition) 'not))))
(trace iscond)

(define (evalcondition condition env envf)
  (cond
    [ (equal? (car condition) 'gt)   (> (myeval (cadr condition) env envf) (myeval (cadr (cdr condition)) env envf)) ]
    [ (equal? (car condition) 'lt)   (< (myeval (cadr condition) env envf) (myeval (cadr (cdr condition)) env envf)) ]
    [ (equal? (car condition) 'eq)   (equal? (myeval (cadr condition) env envf) (myeval (cadr (cdr condition)) env envf)) ]
    [ (equal? (car condition) 'or)   (or (evalcondition (cadr condition) env envf) (evalcondition (cadr (cdr condition)) env envf)) ]
    [ (equal? (car condition) 'and)  (and (evalcondition (cadr condition) env envf) (evalcondition (cadr (cdr condition)) env envf)) ]
    [ (equal? (car condition) 'not)  (not (evalcondition (cadr condition) env envf)) ]
    ))
(trace evalcondition)



;; Meta-syntax rule 1(a) is violated 
(define prog1
  '(letp ((f1 (x)) ((gt (apply (f ((- x 1)))) 0)
                    (* x (apply (f1 ((- x 1)))))
                   1))
         (apply (f1 (x)))))

;; Meta-syntax rule 1(a) is violated 
(define prog2
  '(letp ((f1 (x)) ((or (gt (apply (f ((- x 1)))) 0) (gt 1 2))
                    (* x (apply (f1 ((- x 1)))))
                    1))
         (apply (f1 (x)))))


;; Meta-syntax rule 1(b) is violated 
(define prog3
  '(letp ((f1 (x)) ((gt (apply (f ((- x 1) 2))) 0)
                    (* x (apply (f1 ((- x 1)))))
                   1))
         (apply (f1 (x)))))

;; Meta-syntax rule 1(b) is violated 
(define prog4
  '(letp ((f1 (x)) ((or (gt (apply (f ((- x 1) 2))) 0) (gt 1 2))
                    (* x (apply (f1 ((- x 1)))))
                    1))
         (apply (f1 (x)))))


(define prog5
  '(letp ((f1 (x)) ((gt (apply (f1 ((- x 1)))) 0)
                    (- x 1)
                    100))
         (apply (f1 (x)))))

;; (eval prog6 '()) returns '(cannot evaluate) 
;; (eval prog6 '((x 1)) returns 0              
(define prog6
  '(letp ((f1 (x)) (let (x (- x 1)) x))
         (apply (f1 (x)))))

;; (eval prog7 '()) returns '(cannot evaluate)  
;; (eval prog7 '((x 1))) returns -1             
(define prog7
  '(let (x (- x 1))
     (letp ((f1 (x)) (- x 1))
           (apply (f1 (x))))))

;; dynamic scoping
;; (eval prog8 '()) returns 99  
(define prog8
  '(let (x 1)
     (letp ((f1 ()) (- x 1))
           (letp ((g1 (y)) (let (x y) (apply (f1 ()))))
                 (apply (g1 (100)))))))

;; (eval prog9 '((x 3)) returns 2  
;; (eval prog9 '((x 6)) returns 8  
(define prog9
  '(letp ((f1 (n)) ((gt n 1)
                    (+ (apply (f1 ((- n 1))))
                       (apply (f1 ((- n 2)))))
                    ((gt n 0)
                     1
                     0)))
         (apply (f1 (x)))))
                    
