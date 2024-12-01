#lang racket

#;(provide solve-a solve-b)
(provide 1d-distance)

;;; PART A
(define/contract (solve-a filename)
  (-> string? exact-integer?)
  (with-input-from-file filename
    (lambda ()
      (define-values (AS BS) (read-loclists)) ; unsorted, in reverse order
      (define AS-SORTED (sort AS <))
      (define BS-SORTED (sort BS <))

      ; multiple complex inputs :DDDD
      (foldr + 0 (map 1d-distance AS-SORTED BS-SORTED)))))

; read lines of "a b" and produces lists as and bs
(define/contract (read-loclists)
  (-> (values (listof exact-integer?) (listof exact-integer?)))
  (for/fold ([as '()]
             [bs '()])
            ([line (in-lines)])
    (define line-parts (regexp-split #px"\\s+" line))
    (values (cons (string->number (first line-parts)) as) 
            (cons (string->number (second line-parts)) bs))))

; Euclidean distance (absolute value of difference)
(define/contract (1d-distance a b)
  (-> exact-integer? exact-integer? (and/c exact-integer? (not/c negative?)))
  (abs (- a b)))

(module+ test
  (require rackunit)
  (check-equal? (solve-a "input-a-test.txt") 11)
  #;(check-equal? (solve-b "input-a-test.txt") 31)


  ; 1d-distance
  (check-equal? (1d-distance 0 0) 0)
  (check-equal? (1d-distance 1 1) 0)
  (check-equal? (1d-distance -1 -1) 0)
  (check-equal? (1d-distance -1 1) 2)
  (check-equal? (1d-distance 1 -1) 2)
  (check-equal? (1d-distance 1 5) 4)
  (check-equal? (1d-distance 1 -5) 6))

#;(solve-a "input-a.txt")
