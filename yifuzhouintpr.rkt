
#lang racket
(require "program.rkt")
(provide (all-defined-out))



;;;;;;;;;;;;;;;;;;;;;
;; nofree number: true
;;        var: if present in the boundvar list
;;        let (var expr1) expr2:  expr1 does not have free variables and expr2 does not have free variables 
(define (nofree expr boundvars)
  (cond
    [ (number? expr) true ]
    [ (symbol? expr) (if (mymember expr boundvars) true false) ]
    [ (islet (car expr)) (and (nofree (cadr (cadr expr)) boundvars) (nofree (cadr (cdr expr)) (cons (car (cadr expr)) boundvars))) ] 
    [ (isarith (car expr)) (and (nofree (cadr expr) boundvars) (nofree (cadr (cdr expr)) boundvars)) ]
    [ (iscond (car expr)) (and (nofreecondition (car expr) boundvars)
                               (nofree (cadr expr) boundvars)
                               (nofree (cadr (cdr expr)) boundvars)) ]
    ))


(define (mymember x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (mymember x (cdr lst)))))

(define (nofreecondition condition boundvars)
  (cond
    [ (or (equal? (car condition) 'gt)
          (equal? (car condition) 'lt)
          (equal? (car condition) 'eq))  (and (nofree (cadr condition) boundvars) (nofree (cadr (cdr condition)) boundvars)) ]
    [ (or (equal? (car condition) 'or)
          (equal? (car condition) 'and)) (and (nofreecondition (cadr condition) boundvars) (nofreecondition (cadr (cdr condition)) boundvars)) ]
    [ else (nofreecondition (cadr condition)) ]
    ))
      
;;;;;;;;;;;;;;;;;;;;

;;the result I want to get is the form (result heap)
;;so when we eval it if we want the result we need to car, else if we want
;;the heap, we need to cadr
;;when we change the heap value, we need to keep it and save it.

;;if we find any of the result has '(exception runtime) we need output '(exception runtime)
(define (eval expr env heap)
  (if (or (equal? (car (myeval expr env heap)) '(exception runtime)) (equal? (cadr (myeval expr env heap)) '(exception runtime)))
      '(exception runtime)
      (myeval expr env heap)))

;;this is the key eval funtion

(define (myeval expr env heap)
  (if (equal? heap '(exception runtime))
      (cons '(exception runtime) (list heap))
      (if (equal? expr '(exception runtime))
          (cons '(exception runtime) (list '(exception runtime)))
  (cond
    [ (number? expr)  (cons expr (list heap)) ]
    [ (symbol? expr)  (findvalue expr env heap) ] ;; new rule
    [ (islet (car expr)) (evallet expr env heap) ]  ;; new rule
    [ (isarith (car expr)) (evalarith expr env heap) ] ;; old rule with env parameter
    [ (isderef (car expr)) (evaldref expr env heap) ]
    [ (iswref (car expr)) (evalwref expr env heap) ]
    [ (isref (car expr))  (evalref expr env heap) ]
    [ (isfree (car expr)) (evalfree expr env heap) ]
    [ (iscond (car expr))  (if (equal? (car (evalcondition (car expr) env heap)) '(exception runtime))
                               (cons '(exception runtime) (list heap))
                            (if (car (evalcondition (car expr) env heap))  ;; old rule with env parameter
                               (myeval (cadr expr) env (cadr (evalcondition (car expr) env heap)))       
                               (myeval (cadr (cdr expr)) env (cadr (evalcondition (car expr) env heap))))) ]
    ))))
;;(trace myeval)
;;new addtion
(define  (isderef x)
  (equal? x 'deref))
;;  (trace isderef)

;;new addition for evaldref
(define (evaldref expr env heap)
  (cons (readfromheap (car (myeval (cadr expr) env heap)) (cadr (myeval (cadr expr) env heap))) (list heap)))
;;(trace evaldref)

;;new addition for wref
(define (iswref x)
  (equal? x 'wref))
;;(trace iswref)

;;new addition for evalwref
(define (evalwref expr env heap)
  (cons (car (myeval (cadr (cdr expr)) env heap)) (list (writetoheap (car (myeval (cadr expr) env heap)) (car (myeval (cadr (cdr expr)) env heap)) heap false))))
;;(trace evalwref)

;;new addition for ref
(define (isref x)
  (equal? x 'ref))
;;(trace isref)

;;new addition for evalref
(define (evalref expr env heap)
  (cons (findfree heap) (list (writetoheap (findfree heap) (car (myeval (cadr expr) env heap)) (cadr (myeval (cadr expr) env heap)) false))))
;;(trace evalref)

;;new addition for free
(define (isfree x)
  (equal? x 'free))
;;(trace isfree)

;;new addition for evalfree
(define (evalfree expr env heap)
  (cons (car (myeval (cadr expr) env heap)) (list (writetoheap (car (myeval (cadr expr) env heap)) 'free heap false))))
;;(trace evalfree)

;; new addition
(define (islet x)
  (equal? x 'let))
;;(trace islet)
;; new addition: see the notes
(define (findvalue var env heap)
  (if (equal? var (car (car env))) ;; We already know expression does not contain free variables
      (cons (cadr (car env)) (list heap))
      (findvalue var (cdr env) heap)))
;;(trace findvalue)
;; new addition: see the notes
(define (evallet expr env heap)
  (myeval (cadr (cdr expr)) (cons (list (car (cadr expr)) (car (myeval (cadr (cadr expr)) env heap))) env) (cadr (myeval (cadr (cadr expr)) env heap))))
;;(trace evallet)
(define (isarith x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)))
;;(trace isarith)
(define (evalarith expr env heap)
  (cond
    [ (equal? (car expr) '+)
      (if (or (equal? (car (myeval (cadr expr) env heap)) '(exception runtime)) (equal? (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))) '(exception runtime)))
          (cons '(exception runtime) (list heap))
      (cons (+ (car (myeval (cadr expr) env heap)) (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))))
                                   (list (cadr (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap))))))) ]
    [ (equal? (car expr) '-)
      (if (or (equal? (car (myeval (cadr expr) env heap)) '(exception runtime)) (equal? (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))) '(exception runtime)))
          (cons '(exception runtime) (list heap))
      (cons (- (car (myeval (cadr expr) env heap)) (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))))
                                   (list (cadr (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap))))))) ]
    [ (equal? (car expr) '*)
      (if (or (equal? (car (myeval (cadr expr) env heap)) '(exception runtime)) (equal? (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))) '(exception runtime)))
          (cons '(exception runtime) (list heap))
      (cons (* (car (myeval (cadr expr) env heap)) (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))))
                                   (list (cadr (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap))))))) ]
    [ (equal? (car expr) '/)
      (if (or (equal? (car (myeval (cadr expr) env heap)) '(exception runtime)) (equal? (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))) '(exception runtime)))
          (cons '(exception runtime) (list heap))
      (cons (/ (car (myeval (cadr expr) env heap)) (car (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap)))))
                                   (list (cadr (myeval (cadr (cdr expr)) env (cadr (myeval (cadr expr) env heap))))))) ]
    ))
;;(trace evalarith)
(define (iscond condition)
  (or (equal? (car condition) 'gt)
      (equal? (car condition) 'lt)
      (equal? (car condition) 'eq)
      (equal? (car condition) 'or)
      (equal? (car condition) 'and)
      (equal? (car condition) 'not)))
;;(trace iscond)

(define (evalcondition condition env heap)
   
  (cond
    [ (equal? (car condition) 'gt)
      (if (or (equal? (car (myeval (cadr condition) env heap)) '(exception runtime)) (equal? (car (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))) '(exception runtime)))
          (cons '(exception runtime) (list heap))
      (cons (> (car (myeval (cadr condition) env heap)) (car (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))))
                                           (list (cadr (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))))))]
    [ (equal? (car condition) 'lt)
      (if (or (equal? (car (myeval (cadr condition) env heap)) '(exception runtime)) (equal? (car (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))) '(exception runtime)))
          (cons '(exception runtime) (list heap))
      (cons (< (car (myeval (cadr condition) env heap)) (car (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))))
                                           (list (cadr (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))))))]
                                           
    [ (equal? (car condition) 'eq)   (cons (equal? (car (myeval (cadr condition) env heap)) (car (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap)))))
                                           (list (cadr (myeval (cadr (cdr condition)) env (cadr (myeval (cadr condition) env heap))))))]
    [ (equal? (car condition) 'or)   (cons (or (car (evalcondition (cadr condition) env heap)) (car (evalcondition (cadr (cdr condition)) env (cadr (evalcondition (cadr condition) env heap)))))
                                           (list (cadr (evalcondition (cadr (cdr condition)) env (cadr (evalcondition (cadr condition) env heap))))))]
    [ (equal? (car condition) 'and)  (cons (and (car (evalcondition (cadr condition) env heap)) (car (evalcondition (cadr (cdr condition)) env (cadr (evalcondition (cadr condition) env heap)))))
                                           (list (cadr (evalcondition (cadr (cdr condition)) env (cadr (evalcondition (cadr condition) env heap))))))]
    [ (equal? (car condition) 'not)  (cons (not (car (evalcondition (cadr condition) env heap)))
                                           (list (cadr (evalcondition (cadr condition) env heap))))]
    ))
;;(trace evalcondition)

;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions;;
;;;;;;;;;;;;;;;;;;;;;


;; writes at the location loc the integer value val
;; Precondition: loc is integer
;;               val is integer
;;               heap is a syntactically correct representation of heap as a
;;                  list if pairs, where each element in the pair is an integer
;;                  and no two pair has the same integer as the first element
;;               flag is boolean: true to disallow writing to free location
;;                                false to allow writing to free location
(define (writetoheap loc val heap flag)
  (if (null? heap)
      '(exception runtime);; Out of memory access exception
      (if (equal? loc '(exception runtime))
          '(exception runtime)
          (if (equal? val '(exception runtime))
              '(exception runtime)
      (if (equal? (car (car heap)) loc)                                   ;; if location found
          (if (and flag (equal? (cadr (car heap)) 'free))                 ;;   if location is free
              '(exception runtime)                                            ;;     free memory access exception
              (cons (list (car (car heap)) val) (cdr heap)))              ;;    else update that value stored at that location
          (if (equal? (writetoheap loc val (cdr heap) flag) '(exception runtime)) ;; else search for location in the rest
              '(exception runtime)                                           ;;    exception pushback
              (if (equal? (writetoheap loc val (cdr heap) flag) '(exception runtime))
                  '(exception runtime)
                  (cons (car heap) (writetoheap loc val (cdr heap) flag))))))))) ;;    construct the new heap and return
;;(trace writetoheap)

;; read the value stored at the location loc
;; Precondition: loc is integer
;;               heap is a syntactically correct representation of heap as a
;;                  list if pairs, where each element in the pair is an integer
;;                  and no two pair has the same integer as the first element
(define (readfromheap loc heap)
  (if (null? heap)
      '(exception runtime) ;; Out of memory access exception
      (if (equal? (car (car heap)) loc)          ;; if location found
          (if (equal? (cadr (car heap)) 'free)   ;;   if location is free location
              '(exception runtime)                   ;;      free memory access exception: nullpointer
              (cadr (car heap)))                 ;;    otherwise return the value
          (readfromheap loc (cdr heap)))))       ;; otherwise continue the search
;;(trace readfromheap)
;; helper functions for references
;; Precondition: 
;;               heap is a syntactically correct representation of heap as a
;;                  list if pairs, where each element in the pair is an integer
;;                  and no two pair has the same integer as the first element
(define (findfree heap)
  (if (null? heap)
      '(exception runtime) ;; Out of memory exception
      (if (equal? (cadr (car heap)) 'free)  ;; if the location is free
          (car (car heap))                  ;;    return the location 
          (findfree (cdr heap)))))          ;; else continue looking for free
;;(trace findfree)
