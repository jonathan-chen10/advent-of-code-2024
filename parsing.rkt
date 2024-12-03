#lang racket

(provide read-columns read-rows read-lines read-all)

; read lines of "a b ..." and produces list of lists
(define/contract (read-columns filename)
  (-> string? (listof (listof string?)))
  (with-input-from-file filename
    (lambda ()
      (for/foldr ([sublists '()]) ([line (in-lines)])
        (define line-parts (regexp-split #px"\\s+" line))
        (if (= (length sublists) 0)
            (map list line-parts)
            (map cons line-parts sublists))))))


; read lines of "a1 a2 ..." and produces list of lists
(define/contract (read-rows filename)
  (-> string? (listof (listof string?)))
  (with-input-from-file filename
    (lambda ()
      (for/foldr ([sublists '()]) ([line (in-lines)])
        (define line-parts (regexp-split #px"\\s+" line))
        (cons line-parts sublists)))))

; raw lines
(define/contract (read-lines filename)
  (-> string? (listof string?))
  (with-input-from-file filename
    (lambda ()
      (for/foldr ([sublists '()]) ([line (in-lines)])
        (cons line sublists)))))

; whole thing as a string
(define/contract (read-all filename)
  (-> string? string?)
  (port->string (open-input-file filename) 
                #:close? #t))


(module+ test
  (require rackunit)
  
  (test-case "read-columns"
    (define COL1 (list "3." "1" "4" "1" "5"))
    (define COL2 (list "9" "2" "6" "5" "3"))
    (define COL3 (list "5" "8" "9" "7" "9"))
    (check-equal? (read-columns "input-formats/column.txt") (list COL1))
    (check-equal? (read-columns "input-formats/two-column.txt")
                  (list COL1 COL2))
    (check-equal? (read-columns "input-formats/three-column.txt")
                  (list COL1 COL2 COL3)))

  (test-case "read-rows"
    (define ROW1 (list "3." "1" "4" "1" "5"))
    (define ROW2 (list "9" "2" "6" "5" "3"))
    (define ROW3 (list "5" "8" "9" "7" "9"))
    (check-equal? (read-rows "input-formats/row.txt") (list ROW1))
    (check-equal? (read-rows "input-formats/two-row.txt")
                  (list ROW1 ROW2))
    (check-equal? (read-rows "input-formats/three-row.txt")
                  (list ROW1 ROW2 ROW3)))

  (test-case "read-lines"
             (define ROW1 "3. 1 4 1 5")
             (define ROW2 "9 2 6 5 3")
             (define ROW3 "5 8 9 7 9")
             (check-equal? (read-lines "input-formats/row.txt") (list ROW1))
             (check-equal? (read-lines "input-formats/two-row.txt")
                           (list ROW1 ROW2))
             (check-equal? (read-lines "input-formats/three-row.txt")
                           (list ROW1 ROW2 ROW3)))

  (test-case "read-lines"
             (define ROW1 "3. 1 4 1 5\n")
             (define ROW2 "9 2 6 5 3\n")
             (check-equal? (read-all "input-formats/row.txt") ROW1)
             (check-equal? (read-all "input-formats/two-row.txt")
                           (string-append ROW1 ROW2))))