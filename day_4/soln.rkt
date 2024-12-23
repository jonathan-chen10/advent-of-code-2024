#lang racket

(require "../parsing.rkt")
(require "../utils.rkt")

;;; PART 1
(define/contract (solve-a filename)
  (-> string? exact-integer?)
  
  (define INPUT (map explode (read-lines filename)))
  (+ (count-recursive/seq (list "X" "M" "A" "S") INPUT)
     (count-recursive/seq (list "X" "M" "A" "S") (rot45 INPUT))
     (count-recursive/seq (list "X" "M" "A" "S") (rot90 INPUT))
     (count-recursive/seq (list "X" "M" "A" "S") (rot45 (rot90 INPUT)))
     (count-recursive/seq (list "X" "M" "A" "S") (rot90 (rot90 INPUT)))
     (count-recursive/seq (list "X" "M" "A" "S") (rot45 (rot90 (rot90 INPUT))))
     (count-recursive/seq (list "X" "M" "A" "S") (rot90 (rot90 (rot90 INPUT))))
     (count-recursive/seq (list "X" "M" "A" "S") (rot45 (rot90 (rot90 (rot90 INPUT)))))))

;;; PART 2
(define/contract (solve-b filename)
  (-> string? exact-integer?)
  
  (define INPUT (read-all filename))
  0)
  


(module+ test
  (require rackunit)

  (check-equal? (solve-a "input-test.txt") 18)
  #;(check-equal? (solve-b "input-test.txt") 48))

(solve-a "input.txt")
#;(solve-b "input.txt")