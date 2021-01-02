
;----------------------------------+
;CS 4337.502 __ Fall 2020          |
;Professor Davis                   |
;Project 1 _ racket                |
;----------------------------------|
;Name(Last, First): ZIAEI, ARMIN   |
;netid: AXZ172330                  |
;Date: 10/05/2020                  |
;----------------------------------+


;--------------------start---------------------------------------------------

#lang racket
(provide (all-defined-out))


;1)-----------------divisible-by-x-------------------------------------------
(define ((divisible-by-x? x) n)
  (if (= 0 (remainder n x)) #t #f))

(define div-by-5 (divisible-by-x? 5))

;2)-----------------function-9-----------------------------------------------
(define (function-9 x)
  (car (map x '(9))))

;3)-----------------my-map---------------------------------------------------
(define (my-map f lst)
(cond [(empty? lst) empty]
[else (cons (f (first lst)) (my-map f (rest lst)))]))

;4)-----------------pair-up--------------------------------------------------
(define (pair-up lst1 lst2)
(cond ((null? lst1) '())
    ((null? lst2) '())
    (else
      (cons (list (car lst1) (car lst2))
      (pair-up (cdr lst1) (cdr lst2))))))

;5)-----------------classify-------------------------------------------------
(define (classify boolean lst)
  (cond ((null? lst) '())
    ((equal? boolean even?) (list (even lst) (odd lst)))
    ((equal? boolean real?) (list (real lst) (non-real lst)))
    ((equal? boolean integer?) (list (integer lst) (non-integer lst)))))

(define (even lst)
  (cond ((null? lst) '())
    ((even? (car lst)) (cons (car lst) (even (cdr lst))))
    (else (even (cdr lst)))))

(define (odd lst)
  (cond ((null? lst) '())
    ((odd? (car lst)) (cons (car lst) (odd (cdr lst))))
    (else (odd (cdr lst)))))

(define (real lst)
  (cond ((null? lst) '())
    ((real? (car lst)) (cons (car lst) (real (cdr lst))))
    (else (real (cdr lst)))))

(define (non-real lst)
  (cond ((null? lst) '())
    ((real? (car lst)) (non-real (cdr lst)))
    (else (cons (car lst) (non-real (cdr lst))))))

(define (integer lst)
  (cond ((null? lst) '())
    ((integer? (car lst)) (cons (car lst) (integer (cdr lst))))
    (else (integer (cdr lst)))))

(define (non-integer lst)
  (cond ((null? lst) '())
    ((integer? (car lst)) (non-integer (cdr lst)))
    (else (cons (car lst) (non-integer (cdr lst))))))

;6)-----------------is-member?-----------------------------------------------
(define (is-member? atom lst)
    (if (null? lst)
       #f
    (if (equal? atom (car lst))
        #t
        (is-member? atom (cdr lst)))))


;7)-----------------my-sorted?-----------------------------------------------
(define (my-sorted? tmp< list)
  (or (null? list)
      (null? (cdr list))
      (and (tmp< (car list)(cadr list))
          (my-sorted? tmp< (cdr list)))))


;8)-----------------my-flatten-----------------------------------------------
(define (my-flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (my-flatten (car lst)) (my-flatten (cdr lst))))
        (else (list lst))))


;9)-----------------upper-threshold------------------------------------------
(define (upper-threshold lst buf)
(cond ((null? lst) '()) 
((< (car lst) buf) (cons (car lst) (upper-threshold (cdr lst) buf))) 
(else (upper-threshold (cdr lst) buf)))) 


;10)-----------------my-list-ref----------------------------------------------
(define (my-list-ref lst po)
(cond ((null? lst) (display "ERROR: Index out of bounds"))
((= po 0) (car lst))
(else (my-list-ref (cdr lst) (- po 1)))))


;11)-----------------deep-reverse----------------------------------------------
;(define (deep-reverse l)
;  (if (list? l)
;      (reverse (map deep-reverse l))
;      l))


(define (deep-reverse lst)
  (let loop ((x lst)
             (r '()))
    (cond ((null? x) r) 
          ((pair? x) (loop (cdr x) (cons (deep-reverse (car x)) r)))
          (else x))))

;-----------------------END-----------------------------------------------------
