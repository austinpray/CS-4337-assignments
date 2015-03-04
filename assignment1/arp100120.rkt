#lang racket

; my-reverse
; @param c list
(define (my-reverse c)
  (foldl cons '() c))

; my-map
; @param iteratee procedure
; @param collection list
(define (my-map iteratee collection )
  (for/list ([i collection])
    (iteratee i)))

; function-3
; @param x procedure
(define (function-3 x)
  (x 3))

; zipper
; @param x list
; @param y list
(define (zipper x y)
  (for/list ([e1 x] [e2 y])
    (list e1 e2)))

; segregate 
; @param ints list
(define (segregate ints)
  (define (my-even? int)
    (eq? (remainder int 2) 0))
  (define (my-odd? int)
    (not (my-even? int)))
  (define (my-filter predicate collection) 
                      (for/list ([e collection]
                           #:when (predicate e))
                        e))
  (list (my-filter my-even? ints) (my-filter my-odd? ints)))

; is-member
; @param el atom
; @param collection list
(define (is-member? el collection)
  (for/or ([i collection]) (eq? i el)))

; my-sorted?
; @param collection list
(define (my-sorted? collection)
  (define (any<? a b)
  (cond [(and (number? a) (number? b)) (< a b)]
        [(and (string? a) (string? b)) (string<? a b)]
        [else (string<? (format "~A" a) (format "~A" b))])) 
  (define (iter c result)
    (cond
      [(< (length c) 2) result]
      [else (and (iter (rest c) result) (any<? (first c) (second c)))]))
  (iter collection #t))

; my-flatten
; @param collection list
(define (my-flatten collection)
  (let iter ([c collection] [result null])
    (cond [(null? c) result]
          [(pair? c) (iter (car c) (iter (cdr c) result))]
          [else (cons c result)])))

; threshold
; @param collection list
; @param atom atom
(define (threshold collection atom)
  (for/list ([i collection] #:when (< i atom)) i))

; my-list-ref
; @param collection list
; @param index int
(define (my-list-ref collection index)
  (let iter ([c collection] [i 0])
    (cond 
      [(>= index (length collection))
       "index out of bounds"]
      [(= i index) (first c)]
      [else (iter (rest c) (+ 1 i))])))
   
; deep-reverse
; @param collection list
(define (deep-reverse collection)
  (define reversed (my-reverse collection))
  (for/list ([i reversed])
    (cond
      [(list? i) (deep-reverse i)]
      [else i])))

(provide 
 my-reverse 
 my-map 
 function-3 
 zipper 
 segregate 
 is-member?
 my-sorted?
 my-flatten
 threshold
 my-list-ref
 deep-reverse)