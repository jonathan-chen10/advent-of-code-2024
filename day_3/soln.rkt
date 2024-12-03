#lang racket

(require "../parsing.rkt")
(require "../utils.rkt")

;;; PART 1
(define/contract (solve-a filename)
  (-> string? exact-integer?)
  
  (define INPUT (read-all filename))
  (define MUL-CLAUSES (regexp-match* #px"mul\\(-?\\d+,-?\\d+\\)" INPUT))
  (foldr + 0 (map compute-multiplication MUL-CLAUSES)))

; evaluates mul(x,y)
(define/contract (compute-multiplication mul-clause)
  (-> (and/c string? (lambda (s) regexp-match* #px"mul\\(-?\\d+,-?\\d+\\)" s))
      exact-integer?)
  (define MULTIPLICANDS (map string->number (regexp-match* #px"-?\\d+" mul-clause)))
  (* (first MULTIPLICANDS) (second MULTIPLICANDS)))

;;; PART 2
(define/contract (solve-b filename)
  (-> string? exact-integer?)
  
  (define INPUT (read-all filename))
  (define INSTRUCTIONS 
    (regexp-match* #px"(mul\\(-?\\d+,-?\\d+\\)|do\\(\\)|don't\\(\\))" INPUT))
  (evaluate-instructions #t INSTRUCTIONS))

(define/contract (evaluate-instructions enabled instructions)
  (-> boolean? 
      (listof 
       (and/c string? 
              (lambda (s) 
                (regexp-match* 
                 #px"(mul\\(-?\\d+,-?\\d+\\)|do\\(\\)|don't\\(\\))" 
                 s))))
      exact-integer?)
  (cond
    [(empty? instructions) 0]
    [(string-contains? (first instructions) "don't") 
     (evaluate-instructions #f (rest instructions))]
    [(string-contains? (first instructions) "do") 
     (evaluate-instructions #t (rest instructions))]
    [(string-contains? (first instructions) "mul") 
     (+ (if enabled (compute-multiplication (first instructions)) 0)
        (evaluate-instructions enabled (rest instructions)))]))
  


(module+ test
  (require rackunit)

  (test-case
   "compute-multiplication"
   (check-equal? (compute-multiplication "mul(0,0)") 0)
   (check-equal? (compute-multiplication "mul(1,1)") 1)
   (check-equal? (compute-multiplication "mul(1,-1)") -1)
   (check-equal? (compute-multiplication "mul(-1,1)") -1)
   (check-equal? (compute-multiplication "mul(2,3)") 6)
   (check-equal? (compute-multiplication "mul(100,24)") 2400))

  (test-case
   "evaluate-instructions"
   (check-equal? (evaluate-instructions #t (list "mul(0,0)")) 0)
   (check-equal? (evaluate-instructions #t (list "mul(100,24)")) 2400)
   (check-equal? (evaluate-instructions #f (list "mul(100,24)")) 0)
   (check-equal? (evaluate-instructions #f (list "do()" "mul(100,24)")) 2400)
   (check-equal? (evaluate-instructions #t (list "do()" "mul(100,24)")) 2400)
   (check-equal? (evaluate-instructions #t (list "don't()" "mul(100,24)")) 0)
   (check-equal? (evaluate-instructions #t (list "mul(100,24)" "don't()")) 2400)
   (check-equal? (evaluate-instructions
                  #t (list "don't()" "mul(100,24)" "don't()")) 0)
   (check-equal? (evaluate-instructions
                  #t (list "don't()" "mul(100,24)" "do()")) 0)
   (check-equal? (evaluate-instructions
                  #t (list "don't()" "mul(100,24)" "mul(2,2)" "do()")) 0)
   (check-equal? (evaluate-instructions
                  #t (list "don't()" "mul(100,24)" "do()" "mul(2,2)")) 4))

  (check-equal? (solve-a "input-test-a.txt") 161)
  (check-equal? (solve-b "input-test-b.txt") 48))

#;(solve-a "input.txt")
#;(solve-b "input.txt")