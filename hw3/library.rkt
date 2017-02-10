#lang racket
(provide (all-defined-out))


;;Specification of pivot function.
;;(a) pivot function has two formal parameters: the first parameter is a number and the second parameter is a list of numbers.
;;(b) pivot function returns a list containing a pair of lists: the first element of the pair is the list of numbers in the input list that are less than or equal to the input number; and the second element of the pair is the list of numbers in the input list that are greater than the input number.
;;(c) Signature. pivot : Z × List → List × List

(define pivot
  (lambda (n)
    (lambda (lst)
      (if (null? lst)
          '(() ())
          (if (> n (car lst))
              (add-to-first (car lst) ((pivot n) (cdr lst))) 
              (add-to-second (car lst) ((pivot n) (cdr lst))))))))




(define (add-to-first x lst)
  (list (cons x (car lst)) (cadr lst)))

(define (add-to-second x lst)
  (list (car lst) (append (list x) (cadr lst))))