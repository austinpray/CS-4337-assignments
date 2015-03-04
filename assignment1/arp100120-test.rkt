#lang racket

(require rackunit
         "arp100120.rkt")

(define (double x)
        (* 2 x))

(define (add-one x)
        (+ 1 x))

(define my-reverse-tests
  (test-suite
   "Tests for my-reverse"
   (test-case
    "number collection"
    (check-equal? (my-reverse '(2 6 3 7)) '(7 3 6 2) "Should reverse number collection"))
   (test-case
    "mixed collection"
    (check-equal? (my-reverse '(3 2.71 "foo" 5)) '(5 "foo" 2.71 3) "Should reverse mixed collection"))
   (test-case
    "collection with one member"
    (check-equal? (my-reverse '(7)) '(7) "Should reverse collection with one "))
   (test-case
    "nested collections"
    (check-equal? (my-reverse '((1 2) 3 (4 5))) '((4 5) 3 (1 2))) "Should reverse nested collection")))

(define my-map-tests
  (test-suite
   "Tests for my-map"
   (test-case
    "sqrt collection"
    (check-equal? (my-map sqrt '(9 25 81 49)) '(3 5 9 7) "should transform to sqrt"))
   (test-case
    "double collection"
    (check-equal? (my-map double '(6 4 8 3)) '(12 8 16 6) "should transform to double"))
   (test-case
    "sqr collection"
    (check-equal? (my-map sqr '(5 7)) '(25 49) "should transform to sqr"))))

(define function-3-tests
  (test-suite
   "Tests for function-3"
   (test-case
    "sqrt"
    (check-equal? (function-3 sqrt) 1.7320508075688772 "should apply 3 to sqrt"))
   (test-case
    "sqr"
    (check-equal? (function-3 sqr) 9 "should apply 3 to sqr"))
   (test-case
    "add-one"
    (check-equal? (function-3 add-one) 4 "should apply 3 to add-one"))))

(define zipper-tests
  (test-suite
   "Tests for zipper"
   (test-case
    "same length"
    (check-equal? (zipper '(1 2 3 4) '(a b c d)) '((1 a) (2 b) (3 c) (4 d)) "should zipper same length arrays"))
   (test-case
    "different lengths"
    (check-equal? (zipper '(1 2 3) '(4 9 5 7)) '((1 4) (2 9) (3 5)) "should zipper different length arrays"))
   (test-case
    "mixed types and lengths"
    (check-equal? (zipper '(3 5 6) '("one" 6.18 #t "two")) '((3 "one")(5 6.18)(6 #t)) "should zipper mixed types"))
   (test-case
    "empty list"
    (check-equal? (zipper '(5) '()) '() "should return empty list"))))

(define segregate-tests
  (test-suite
   "Tests for segregate"
   (test-case
    "even from odd"
    (check-equal? (segregate '(7 2 3 5 8)) '((2 8) (7 3 5)) "separate even from odd"))
   (test-case
    "negative numbers"
    (check-equal? (segregate '(3 -5 8 16 99)) '((8 16) (3 -5 99)) "should separate even negative numbers"))
   (test-case
    "empty lists"
    (check-equal? (segregate '()) '(() ()) "should return empty lists"))))

(define is-member?-tests
  (test-suite
   "Tests for is-member?"
   (test-case
    "number is in colleciton"
    (check-equal? (is-member? 6 '(4 8 6 2 1)) #t "should report 6 as a member"))
   (test-case
    "number is not in collection"
    (check-equal? (is-member? 7 '(4 8 6 2 1)) #f "should report 7 as non a member"))
   (test-case
    "string is in mixed collection"
    (check-equal? (is-member? "foo" '(4 5 #f "foo" a)) #t "should report 'foo' as member"))
   (test-case
    "bool is in mixed collection"
    (check-equal? (is-member? #f '(4 5 #f "foo" a)) #t "should report #f as member"))
   (test-case
    "string is not in mixed collection"
    (check-equal? (is-member? "totally bogus" '(4 5 #f "foo" a)) #f "should report 'bogus' as not member"))))
   
(define my-sorted?-tests
  (test-suite
   "Tests for my-sorted?"
   (test-case 
    "numbers already sorted"
    (check-equal? (my-sorted? '(2 5 6 9 11 34)) #t "should report numbers already sorted"))
   (test-case 
    "numbers already sorted 2"
    (check-equal? (my-sorted? '(5 4 3 2 1)) #f "should report reversed numbers as not sorted"))
   (test-case
    "numbers not already sorted"
    (check-equal? (my-sorted? '(7 25 4 15 11 34)) #f "should not report numbers already sorted"))
   (test-case
    "strings already sorted"
    (check-equal? (my-sorted? '("alpha" "beta" "gamma")) #t "should report as already sorted"))
   (test-case
    "strings not already sorted"
    (check-equal? (my-sorted? '("john" "zack" "bob")) #f "should report as not already sorted"))))

(define my-flatten-tests
  (test-suite
   "Tests for my-flatten"
   (test-case
    "single non-nested member"
    (check-equal? (my-flatten '(1)) '(1) "should return the input"))
   (test-case
    "first member is nested collection"
    (check-equal? (my-flatten '((1 2) 3)) '(1 2 3) "should flatten first member"))
   (test-case
    "3 levels of nesting"
    (check-equal? (my-flatten '(((4 3) 6)((7 2 9)(5 1)))) '(4 3 6 7 2 9 5 1)))))

(define threshold-tests
  (test-suite
   "Tests for threshold"
   (test-case
    "decimal numbers"
    (check-equal? (threshold '(3 6.2 7 2 9 5.3 1) 6) '(3 2 5.3 1) "should return some decimals"))
   (test-case
    "integers"
    (check-equal? (threshold '(1 2 3 4 5) 4) '(1 2 3) "should return some ints"))
   (test-case
    "none under threshhold"
    (check-equal? (threshold '(8 3 5 7) 2) '() "should be empty list"))))

(define my-list-ref-tests
  (test-suite
   "Tests for my-list-ref"
   (test-case
    "should get first element"
    (check-equal? (my-list-ref '(4 7 9) 0) 4 "should return the first element"))
   (test-case
    "should get second element"
    (check-equal? (my-list-ref '(4 7 9) 1) 7 "should return the second element"))
   (test-case
    "should get third element"
    (check-equal? (my-list-ref '(4 7 9) 2) 9 "should return third element"))
   (test-case
    "should throw error"
    (check-equal? (my-list-ref '(4 7 9) 3) "index out of bounds" "should return an error"))))

(require rackunit/text-ui)
 
(run-tests my-reverse-tests)
(run-tests my-map-tests)
(run-tests function-3-tests)
(run-tests zipper-tests)
(run-tests segregate-tests)
(run-tests is-member?-tests)
(run-tests my-sorted?-tests)
(run-tests my-flatten-tests)
(run-tests threshold-tests)
(run-tests my-list-ref-tests)