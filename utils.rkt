#lang racket

(provide 1d-distance deep-map)

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
  

(module+ test
  (require rackunit)
  
  (test-case "1d-distance"
    (check-equal? (1d-distance 0 0) 0)
    (check-equal? (1d-distance 1 1) 0)
    (check-equal? (1d-distance -1 -1) 0)
    (check-equal? (1d-distance -1 1) 2)
    (check-equal? (1d-distance 1 -1) 2)
    (check-equal? (1d-distance 1 5) 4)
    (check-equal? (1d-distance 1 -5) 6))

  (test-case "deep-map"
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
    (check-equal? (deep-map add1 '()) '())))