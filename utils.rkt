#lang racket

(provide 1d-distance deep-map differences count-recursive/seq count/seq starts-with rot45 rot90 explode)

; Euclidean distance (absolute value of difference)
(define/contract (1d-distance a b)
  (-> exact-integer? exact-integer? (and/c exact-integer? (not/c negative?)))
  (abs (- a b)))

; Applies mapping function to every atomic element
(define/contract (deep-map fxn l)
  ; no guarantees on function: will let that throw the error
  (-> (-> any/c any/c) list? list?)
  (cond
    [(not (list? l)) (fxn l)]
    [(empty? l) l]
    [(cons? l) (cons (deep-map fxn (first l)) (deep-map fxn (rest l)))]))

; Calculates the difference between values in list (not absolute value)
(define/contract (differences l)
  (-> (non-empty-listof number?) (listof number?))
  (cond
    [(empty? (rest l)) '()]
    [(cons? (rest l)) (cons (- (second l) (first l))
                            (differences (rest l)))]))

; recursively count the number of times a sequence appears in a list of lists
(define/contract (count-recursive/seq seq l)
  (-> list? list? (and/c exact-integer? (not/c negative?)))
  (cond
    [(empty? l) 0]
    [(not (list? (first l))) (count/seq seq l)]
    [(empty? (first l)) (count-recursive/seq seq (rest l))]
    [(cons? (first l)) (+ (count-recursive/seq seq (first l)) (count-recursive/seq seq (rest l)))]))

; checks only the top level
(define/contract (count/seq seq l)
  (-> list? list? (and/c exact-integer? (not/c negative?)))
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (if (starts-with seq l) 1 0)
                  (count/seq seq (rest l)))]))

; starts-with for lists
(define/contract (starts-with seq l)
  (-> list? list? boolean?)
  (cond
    [(empty? seq) #t]
    [(empty? l) #f]
    [(equal? (first seq) (first l)) (starts-with (rest seq) (rest l))]
    [else #f]))

; rotate 45 degrees clockwise to get a triangular shape
(define/contract (rot45 2darr)
  (-> (listof list?) (listof list?))
  (define/contract (rot45/acc curlen 2darr)
    (-> (and/c exact-integer? (not/c negative?)) (listof list?) (listof list?))
    (cond
      [(andmap empty? 2darr) '()]
      [(empty? (first 2darr))
       (cons (reverse (map first (filter cons? 2darr)))
             (rot45/acc curlen (map (lambda (l) (if (empty? l) l (rest l))) 2darr)))]
      [else
       (cons (reverse (n-firsts-rev curlen 2darr))
             (rot45/acc (add1 curlen) (n-rests curlen 2darr)))]))
  (define/contract (n-firsts-rev n arr)
    (-> (and/c exact-integer? (not/c negative?)) list? list?)
    (if (= n 0) '()
        (cons (first (first arr)) (n-firsts-rev (sub1 n) (rest arr)))))
  (define/contract (n-rests n arr)
    (-> (and/c exact-integer? (not/c negative?)) list? list?)
    (if (= n 0) arr
        (cons (rest (first arr)) (n-rests (sub1 n) (rest arr)))))
  (rot45/acc 1 2darr))

; rotate 90 degrees clockwise
(define/contract (rot90 2darr)
  (-> (listof list?) (listof list?))
  (cond
    [(empty? (first 2darr)) '()]
    [else
     (cons (reverse (map first 2darr))
           (rot90 (map rest 2darr)))]))

(define/contract (explode s)
  (-> string? (listof string?))
  (map string (string->list s)))

(module+ test
  (require rackunit)
  
  (test-case
   "1d-distance"
   (check-equal? (1d-distance 0 0) 0)
   (check-equal? (1d-distance 1 1) 0)
   (check-equal? (1d-distance -1 -1) 0)
   (check-equal? (1d-distance -1 1) 2)
   (check-equal? (1d-distance 1 -1) 2)
   (check-equal? (1d-distance 1 5) 4)
   (check-equal? (1d-distance 1 -5) 6))

  (test-case
   "deep-map"
   (define RANDOMS (build-list 5 (lambda (_) (random))))
   (define RANDOMS/2D
     (build-list 5 (lambda (_) (build-list 5 (lambda (_2) (random))))))
   (check-equal? (map sqrt RANDOMS) (deep-map sqrt RANDOMS))
   (check-equal? (map (lambda (sublist) (map sqrt sublist)) RANDOMS/2D)
                 (deep-map sqrt RANDOMS/2D))
   (check-equal? (cons (map sqrt RANDOMS) (deep-map sqrt RANDOMS/2D))
                 (deep-map sqrt (cons RANDOMS RANDOMS/2D)))
   (check-equal? (deep-map string-length
                           (list (list (list "abc" "a")) "ab" (list "")))
                 (list (list (list 3 1)) 2 (list 0)))
   (check-equal? (deep-map add1 '()) '()))

  (test-case
   "differences"
   (check-equal? (differences (list 1 2 3)) (list 1 1))
   (check-equal? (differences (list 1 -1 5)) (list -2 6)))

  (test-case
   "count-recursive/seq"
   (check-equal? (count-recursive/seq (list 1) (list 1 2 1)) 2)
   (check-equal? (count/seq (list 1) (list 1 2 1)) 2)
   (check-equal? (count/seq (list 1 2) (list 1 2 1)) 1)
   (check-equal? (count/seq (list 2 1) (list 1 2 1)) 1)
   (check-equal? (count/seq (list 2 1 2) (list 1 2 1)) 0)
   (check-equal? (count-recursive/seq (list "a" "b") (list 1 2 1)) 0)
   (check-equal? (count-recursive/seq (list "a" "b")
                                      (list (list "a" "b" "c") (list "c" "a" "b") (list "c" "b" "a")))
                 2))

  (test-case
   "rotations"
   (check-equal? (rot45 (list (list 1 2) (list 3 4))) (list (list 1) (list 3 2) (list 4)))
   (check-equal? (rot45 (list (list 1 2 3) 
                              (list 4 5 6)
                              (list 7 8 9)))
                 (list (list 1) 
                       (list 4 2)
                       (list 7 5 3)
                       (list 8 6)
                       (list 9)))
   (check-equal? (rot45 (list (list 1 2 3 4) 
                              (list 5 6 7 8)
                              (list 9 10 11 12)
                              (list 13 14 15 16)))
                 (list (list 1) 
                       (list 5 2)
                       (list 9 6 3)
                       (list 13 10 7 4)
                       (list 14 11 8)
                       (list 15 12)
                       (list 16)))
   (check-equal? (rot90 (list (list 1 2) (list 3 4))) (list (list 3 1) (list 4 2)))
   (check-equal? (rot90 (list (list 1 2 3) 
                              (list 4 5 6)
                              (list 7 8 9)))
                 (list (list 7 4 1) 
                       (list 8 5 2)
                       (list 9 6 3)))
   (check-equal? (rot90 (list (list 1 2 3 4) 
                              (list 5 6 7 8)
                              (list 9 10 11 12)
                              (list 13 14 15 16)))
                 (list (list 13 9 5 1) 
                       (list 14 10 6 2)
                       (list 15 11 7 3)
                       (list 16 12 8 4))))

  (test-case
   "explode"
   (check-equal? (explode "") '())
   (check-equal? (explode "abc") (list "a" "b" "c"))))