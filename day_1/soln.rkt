#lang racket

(require "../parsing.rkt")
(require "../utils.rkt")

;;; PART 1
(define/contract (solve-a filename)
  (-> string? exact-integer?)
  
  (define INPUT (deep-map string->number (read-columns filename))) ; unsorted
  (define AS (first INPUT))
  (define BS (second INPUT))
  
  (define AS-SORTED (sort AS <))
  (define BS-SORTED (sort BS <))

  ; multiple complex inputs :DDDD
  (foldr + 0 (map 1d-distance AS-SORTED BS-SORTED)))


;;; PART 2
(define/contract (solve-b filename)
  (-> string? exact-integer?)
  
  (define INPUT (deep-map string->number (read-columns filename))) ; unsorted
  (define AS (first INPUT))
  (define BS (second INPUT))

  (foldr + 0
         (map (lambda (a)
                (* a
                   (count (lambda (b) (= a b)) BS)))
              AS)))


(module+ test
  (require rackunit)
  (check-equal? (solve-a "input-a-test.txt") 11)
  (check-equal? (solve-b "input-a-test.txt") 31))

#;(solve-a "input-a.txt")
#;(solve-b "input-a.txt")