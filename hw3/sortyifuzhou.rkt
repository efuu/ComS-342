#lang racket
(provide (all-defined-out))
(require "library.rkt")

;; the helper function to keep recursion below the max value
(define (HELP max i lst)
  (if (equal? i max)
      ((pivot i) lst)
      (HELP max (+ i 1) (append (car ((pivot i) lst)) (car(cdr ((pivot i) lst)))))
      )
  )

;;the function to do sort function
(define (sort lst)
  (if (null? lst)
  null
  (append (car (HELP 100 0 lst)) (car(cdr (HELP 100 0 lst))))
  )
  )
