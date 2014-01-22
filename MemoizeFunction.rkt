#lang racket

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows me to use these:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; I re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; I can also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


;Creating the table data structure
(define (make-table)
   (cons '() "table"))

;Test whether an entity is a table by looking for the presence of a tag
(define (table? table)
  (if (equal? table (cons '() "table")) #t (and (pair? (car table)) (table? (cdr table)))))

;Enable us to add new keys and corresponding values to the table
(define (table-put! table key value)
  (define (table-copy)
            (if (equal? table (make-table))
                (make-table)
                (cons (car table)
                      (table-copy (cdr table)))))
  (begin (set-cdr! table (table-copy table)) (set-car! table (cons key value))))

;Check whether key exists in table
(define (table-has-key? table key)
  (if (equal? table (cons '() "table")) #f (or (equal? key (car (car table))) (table-has-key? (cdr table) key))))

;If key exists in table, extract its value, else spit out an error
(define (table-get table key)
  (if (not (table-has-key? table key)) 'ERROR (if (equal? (car (car table)) key) (cdr (car table)) (table-get (cdr table) key))))

;Test case: the fibonnaci recursive definition
(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; We can track how many times fib is called during evaluation to examine efficiency of recursive procedure
;; The following function is abstracted to return a monitored version of any procedure
(define (make-monitored f) 
   (define times-called 0) 
   (define (mf arg) 
     (cond ((eq? arg 'how-many-calls?) times-called) 
           ((eq? arg 'reset-count) (set! times-called 0)) 
           (else (set! times-called (+ times-called 1)) 
                 (f arg)))) 
   mf)

;We apply this "make-monitored" procedure on fib

(define (fib1 n)
  (if (< n 2)
      n
      (+ (fib1 (- n 1)) (fib1 (- n 2)))))

(fib1 8) ;; => 21
(set! fib1 (make-monitored fib1))
(fib1 8) ;; => 21
(fib1 'how-many-calls?) ;; => 67
(fib1 8) ;; => 21
(fib1 'how-many-calls?) ;; => 134
(fib1 'reset-count)
(fib1 'how-many-calls?) ;; => 0

;; We can create a table to keep track of how many times a function is called
(define (make-num-calls-table mf max)
  (define (helper current done)
  (if (> done max)
      current
      (begin 
        done
        current
        (mf 'reset-count)
        (mf done)
        (table-put! current done (mf 'how-many-calls?))
        (helper mf current (+ 1 done) max))))
  (helper mf (make-table) 1 max))
      
(make-num-calls-table fib1 10)
;((1.1) (2.3) (3.5) (4.9) (5.15) (6.25) (7.41) (8.67) (9.109) (10.177) '() "table")


;To improve the efficiency of these recursive functions, we can 'memoize' the function
;In other words, each function is associated with a data structure which allows us to test
;if we have previously evaluated a function for at a particular value to save computation
;time. The method is abstracted to work on any procedure 
(define (memoize f) 
   (define called (make-table)) 
   (define (mf arg) 
     (if (table-has-key? called arg) (table-get called arg)  
           (begin (table-put! called arg (f arg)) (table-get called arg)))) 
   mf)

;We apply it on fib again to test
(define (fib2 n)
  (if (< n 2)
      n
      (+ (fib2 (- n 1)) (fib2 (- n 2)))))

(set! fib2 (memoize fib2))

(fib2 8);21
(fib2 9);34
(fib2 9);34
