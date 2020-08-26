#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
 (cond
   ((>= secs 86400) (se (quotient secs 86400) 'DAYS (describe-time (remainder secs 86400))))
   ((>= secs 3600) (se (quotient secs 3600) 'HOURS (describe-time (remainder secs 3600))))
   ((>= secs 60) (se (quotient secs 60) 'MINUTES (describe-time (remainder secs 60))))
   (else (se secs 'SECONDS))
))


; Exercise 2 - Define remove-once
(define (remove-once wd sent)
 (cond ((empty? sent) '())
           ((equal? wd (first sent)) (bf sent))
           (else
             (se (first sent) (remove-once wd (bf sent))))
))

; Exercise 3 - Define differences
(define (differences nums)
  (cond
    ((= (count nums) 1) '())
    (else (se (- (first nums) (first (bf nums))) (differences (bf nums))))
))

; Exercise 4 - Define location
(define (location small big)
  (cond
    ((empty? big) #f)
    ((equal? small (first big)) 1)
    (else (+ 1 (location small (bf big))))
))

(location 'me '(you never give me your money))
(location 'the '(the fork and the spoon))
(location 'i '(you never give me your money))


; Exercise 5 - Define initials
(define (initials sent)
 (if (empty? sent)
     '()
     (se (first (first sent))
         (initials (bf sent)))
))

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= num 0) '()
      (se wd (copies (- num 1) wd))
))

(define (base-grade grade)
  (cond
    ((equal? (first grade) 'A) 4)
    ((equal? (first grade) 'B) 3)
    ((equal? (first grade) 'C) 2)
    ((equal? (first grade) 'D) 1)
    (else 0)
  )
)

(define (grade-modifier grade)
  (cond
    ((= (count grade) 1) 0)
    ((equal? (bf grade) '+) 0.33)
    (else -0.33)
  )
)

; Exercise 7 - Define gpa
(define (gpa grades)
 (if
   (empty? grades) 0
   (+ (base-grade (first grades)) (grade-modifier (first grades)) (gpa (bf grades)))
  )
)

; (gpa '(A A+ B+ B))

(define (repeat n word)
  (if (= n 0) '()
      (se word (repeat (- n 1) word)))
)

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  (cond
    ((empty? sent) '())
    ((number? (first sent)) (se (repeat (- (first sent) 1) (first (bf sent))) (repeat-words (bf sent))))
    (else (se (first sent) (repeat-words (bf sent))))                        
  ))

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  (cond
    ((and (empty? sent1) (empty? sent2)) #t)
    ((= (count (first sent1)) (count (first sent2))) (same-shape? (bf sent1) (bf sent2)))
    (else #f)
))

; Test your understanding - Define numbers?
(define (numbers sent)
  (cond
    ((empty? sent) '())
    ((number? (first sent))
     (se (first sent) (numbers (bf sent))))
    (else (numbers (bf sent)))
))

