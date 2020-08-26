#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 - Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?

Tamerlan Gudabayev

2) What is your major?

Self-Taught

3) Are you a returning student? (i.e. Did you take 61AS last semester?)

Nope

4) What made you to take 61AS?

To become a better engineer

5) Tell us interesting things about yourself.

I enjoy experimenting with productivity.

|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

;; Exercise 2a - Define can-drive
(define (can-drive age)
  (if (>= age 16) '(Good to go) '(Not yet)))

;; Exercise 2b - Define fizzbuzz
(define (fizzbuzz n)
  (cond
    ((and (= (remainder n 3) 0) (= (remainder n 5) 0)) 'fizzbuzz)
    ((= (remainder n 3) 0) 'fizz)
    ((= (remainder n 5) 0) 'buzz)
    (else n)
  ))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

;; Exercise 4 - new-if vs if

(define (infinite-loop) (infinite-loop))

(if (= 3 6)
  (infinite-loop)
  (/ 4 2))

#|
Your answer here

|#
