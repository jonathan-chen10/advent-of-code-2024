#lang racket

(require "../parsing.rkt")
(require "../utils.rkt")

;;; PART 1
(define/contract (solve-a filename)
  (-> string? exact-integer?)
  
  (define INPUT (deep-map string->number (read-rows filename))) 
  (count safe-report INPUT))

; Safe reports have all asc/desc by difference btw 1 and 3
(define/contract (safe-report report)
  (-> (listof exact-integer?) boolean?)
  ; map to differences? and check theyre all btw 1 and 3 or -1 and -3
  (define DIFFS (differences report))
  (or (andmap (lambda (d) (<= 1 d 3)) DIFFS)
      (andmap (lambda (d) (<= -3 d -1)) DIFFS)))

(module+ test
  (require rackunit)

  (test-case
   "safe-report"
   (check-equal? (safe-report (list 1 1 1)) #f)
   (check-equal? (safe-report (list 1 2 4 7)) #t)
   (check-equal? (safe-report (list 1 2 4 9)) #f)
   (check-equal? (safe-report (list 1 2 4 3)) #f)
   (check-equal? (safe-report (list 1 0 -3)) #t))
  
  (check-equal? (solve-a "input-test.txt") 2))

(solve-a "input.txt")