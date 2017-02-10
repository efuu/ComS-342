#lang racket
(require "database.rkt")
(provide (all-defined-out))
;;CITED
;;Get some instructions from: http://www.htdp.org/2003-09-26/Book/curriculum-Z-H-1.html#node_toc_start
;;Get some instructions from the GitHub website: https://github.com/search?utf8=%E2%9C%93&q=racket



;;get the x line
(define HELPER
  (lambda (x db)
    (if (null? db)
        null
        (if (equal? x (car(car db)))
            (car db)
            (HELPER x (cdr db))
            )
        )
    )
  )
;;find the subparts of the arguement a     
(define HELPER-SUBPARTS-APPEND
  (lambda (ex lst)
    (if (null? lst)
        ex
        (if (list? (car lst))
            (HELPER-SUBPARTS-APPEND (append ex (cdr(car lst))) (append (car lst) (cdr lst)))
            (HELPER-SUBPARTS-APPEND ex (cdr lst))
            )
        )
    )
  )
;;the helper function to the the can-buy
(define HELPER-CAN-BUY
  (lambda (x db ex)
    (if (null? db)
        ex
        (if (or (= (car(cdr(car db))) x) (< (car(cdr(car db))) x))
          (HELPER-CAN-BUY x (cdr db) (append (list (car (car db))) ex))
          (HELPER-CAN-BUY x (cdr db) ex)
          )
        )
    )
  )
;;the helper function to do the can-build
(define HELPER-CAN-BUILD
  (lambda (length i sublst inputlst)
    (if (equal? i length)
        #f
        (if (null? sublst)
            #t
            (if (null? inputlst)
                #f
                (if (equal? (car sublst) (car inputlst))
                    (HELPER-CAN-BUILD length i (cdr sublst) (cdr inputlst))
                    (HELPER-CAN-BUILD length (+ i 1) (append (cdr inputlst) (list (car inputlst))) sublst)
                    )
                )
            )
        )
    )
  )
;;the helper function to display all the subparts of the argumnet
(define HELPER-SUBPARTS-ALL
  (lambda (ex lst i)
    (if(null? lst)
       ex
       (if (list? (car lst))
           (if (equal? (car(car lst)) i)
               (HELPER-SUBPARTS-ALL (append ex (cdr (car lst))) (append (car lst) (cdr lst)) 1)
               (HELPER-SUBPARTS-ALL (append ex (cdr (car lst))) lst (+ i 1))
               )
           (HELPER-SUBPARTS-ALL ex (cdr lst) 1)
           )
       )
    )
  )
;;the function to display all the subparts
(define subparts-all
  (lambda (a)
    (HELPER-SUBPARTS-ALL null (cdr(cdr(HELPER a database-of-components))) 1)
    )
  )
;;the helper function to do can-build-R
(define HELPER-CAN-BUILD-R
  (lambda (l i sublst inputlst)
    (if (and (equal? i l) (not(equal? l 0)))
        (if (null? (subparts (car sublst)))
            #f
            (HELPER-CAN-BUILD-R (length inputlst) 0 (append (subparts (car sublst)) (cdr sublst)) inputlst)
            )
        (if (null? sublst)
            #t
            (if (null? inputlst)
                #f
                (if (equal? (car sublst) (car inputlst))
                    (HELPER-CAN-BUILD-R (length (cdr inputlst)) 0 (cdr sublst) (cdr inputlst))
                    (HELPER-CAN-BUILD-R (length inputlst) (+ i 1) sublst (append (cdr inputlst) (list (car inputlst))))
                    )
                )
            )
        )
    )
  )
;; the function to solve problem a        
 (define cost
   (lambda (a)
     (car(cdr(HELPER a database-of-components)))
     )
   )
 ;; the function to solve problem b   
(define subparts
  (lambda (a)
    (HELPER-SUBPARTS-APPEND null (cdr(cdr(HELPER a database-of-components))))
    )
  )
;; the function to solve problem c
(define is-atom
  (lambda (a)
    (if (null? (cdr(cdr(HELPER a database-of-components))))
      #t
      #f
      )
    )
  )
;; the function to solve problem d
(define can-buy
  (lambda (b)
    (HELPER-CAN-BUY b database-of-components null)
    )
  )

;; the function to solve problem e
 (define can-build
  (lambda (a)
    (lambda (lst)
      (if (and (equal? 0 (length lst)) (null? (subparts a)))
          #t
          (if (equal? (HELPER-CAN-BUILD (length lst) 0 (subparts a) lst) #t)
              #t
              #f
              )
          )
      )
    )
   )
;; the function to solve problem f
(define can-build-R
  (lambda (a)
    (lambda (lst)
      (if (and (equal? 0 (length lst)) (null? (subparts a)))
          #t
          (if (equal? (HELPER-CAN-BUILD-R (length lst) 0 (subparts-all a) lst) #t)
              #t
              #f
              )
          )
      )
    )
  )