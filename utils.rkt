#lang racket

(provide 1d-distance)

; Euclidean distance (absolute value of difference)
(define/contract (1d-distance a b)
  (-> exact-integer? exact-integer? (and/c exact-integer? (not/c negative?)))
  (abs (- a b)))


(module+ test
  (require rackunit)
  
  (define (test/1d-distance)
    (check-equal? (1d-distance 0 0) 0)
    (check-equal? (1d-distance 1 1) 0)
    (check-equal? (1d-distance -1 -1) 0)
    (check-equal? (1d-distance -1 1) 2)
    (check-equal? (1d-distance 1 -1) 2)
    (check-equal? (1d-distance 1 5) 4)
    (check-equal? (1d-distance 1 -5) 6))
  
  (test/1d-distance))