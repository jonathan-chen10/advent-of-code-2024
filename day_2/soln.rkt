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
  (-> (listof number?) boolean?)
  ; map to differences and check theyre all btw 1 and 3 or -1 and -3
  (define DIFFS (differences report))
  (or (andmap (lambda (d) (<= 1 d 3)) DIFFS)
      (andmap (lambda (d) (<= -3 d -1)) DIFFS)))


;;; PART 2
(define/contract (solve-b filename)
  (-> string? exact-integer?)
  
  (define INPUT (deep-map string->number (read-rows filename))) 
  (count safe-report-allowing-one INPUT))
  

(define/contract (safe-report-allowing-one report)
  (-> (listof number?) boolean?)

  ; brute force: compare (first remaining) and (second remaining)
  ; if difference outside of range, one of them must be removed
  ; note: prev is in reverse order
  (define/contract (safe-report/asc/acc prev remaining)
    (-> (listof number?) (listof number?) boolean?)
    (cond
      [(<= (length remaining) 1) #t]
      [(<= 1 (- (second remaining) (first remaining)) 3)
       (safe-report/asc/acc (cons (first remaining) prev) (rest remaining))]
      [(not (<= 1 (- (second remaining) (first remaining)) 3))
       (or (safe-report-strict/asc (append (reverse prev) (rest remaining)))
           (safe-report-strict/asc (append (reverse prev)
                                           (cons (first remaining)
                                                 (rest (rest remaining))))))]))

  (define/contract (safe-report-strict/asc remaining)
    (-> (listof number?) boolean?)
    (cond
      [(<= (length remaining) 1) #t]
      [(<= 1 (- (second remaining) (first remaining)) 3)
       (safe-report-strict/asc (rest remaining))]
      [else #f]))
  
  (cond
    [(<= (length report) 2) #t]
    [(> (length report) 2)
     (or (safe-report/asc/acc '() report)
         (safe-report/asc/acc '() (map - report)))]))

;; FUTURE: try implementing longest incrasing subsequence

(module+ test
  (require rackunit)

  (test-case
   "safe-report"
   (check-equal? (safe-report (list 1 1 1)) #f)
   (check-equal? (safe-report (list 1 2 4 7)) #t)
   (check-equal? (safe-report (list 1 2 4 9)) #f)
   (check-equal? (safe-report (list 1 2 4 3)) #f)
   (check-equal? (safe-report (list 1 0 -3)) #t))

  (test-case
   "safe-report-allowing-one"
   (check-equal? (safe-report-allowing-one (list 1 1 1)) #f)
   (check-equal? (safe-report-allowing-one (list 1 2 4 7)) #t)
   (check-equal? (safe-report-allowing-one (list 1 2 4 9)) #t)
   (check-equal? (safe-report-allowing-one (list 1 2 4 3)) #t)
   (check-equal? (safe-report-allowing-one (list 1 0 -3)) #t)
   (check-equal? (safe-report-allowing-one (list 1 -5 10)) #f)
   (check-equal? (safe-report-allowing-one (list 1 -5 4)) #t)
   (check-equal? (safe-report-allowing-one (list 1 -5 4 5 8)) #t)
   (check-equal? (safe-report-allowing-one (list 1 -4 5 6 8)) #f)
   (check-equal? (safe-report-allowing-one (list 3 9 8 7)) #t)
   (check-equal? (safe-report-allowing-one (list 1 3 9 8 7)) #f)
   (check-equal? (safe-report-allowing-one (list 3 7 9 8 7)) #f)
   (check-equal? (safe-report-allowing-one (list 3 6 9 8 7)) #f)
   (check-equal? (safe-report-allowing-one (list 12 9 8 7)) #t))
  
  (check-equal? (solve-a "input-test.txt") 2)
  (check-equal? (solve-b "input-test.txt") 4))

#;(solve-a "input.txt")
#;(solve-b "input.txt")