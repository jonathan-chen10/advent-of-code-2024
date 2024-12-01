#lang racket

#;(provide solve-a solve-b)

(define (solve-a filename)
  (with-input-from-file filename
    (lambda ()
      (let-values ([(AS BS) (read-loclists)]) ; unsorted, in reverse order
        (define AS-SORTED (sort AS <))
        (define BS-SORTED (sort BS <))

        ; multiple complex inputs :DDDD
        (foldr + 0 (map 1d-distance AS-SORTED BS-SORTED))
        ))))

(define (read-loclists)
  (for/fold ([as '()]
             [bs '()])
            ([line (in-lines)])
    (define line-parts (regexp-split #px"\\s+" line))
    (values (cons (string->number (first line-parts)) as) 
            (cons (string->number (second line-parts)) bs))))

(define (1d-distance a b)
  (abs (- a b)))

(module+ test
  (require rackunit)
  (check-equal? (solve-a "input-a-test.txt") 11))

#;(solve-a "input-a.txt")
