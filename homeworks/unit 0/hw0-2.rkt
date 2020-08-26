#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom

;2. Compound Expression (3 Atoms)

;3. Compound Expression (4 Atoms)

;4. Compound Expression (1 Atom and 2 subexpressions)

;5. Any Other Kind Expression


;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (item 1 wd) (item 2 wd))
)

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first (item 1 sent)) (first (item 2 sent)))
)

;Exercise 2 - Define teen?
(define (teen? num)
  (if (and (<= num 19) (<= 13 num)) true false) 
)

;Exercise 3 - Define indef-article
(define (vowel? letter)
  (member? letter 'aeiou))

(define (indef-article wd)
  (sentence (if (vowel? (first wd)) 'an 'a) wd)  
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (sentence (butlast sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
 (sentence (first (bf sent)) (first sent) (bl (bf (bf sent))) (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (if (equal? (last time) 'am)
      (if (= (first time) 12)
          0
          (first time))
      (if (= (first time) 12)
          (first time)
          (+ (first time) 12))
))

(define (american-time time)
 (cond ((< 12 time) (sentence (- time 12) 'pm))
       ((= 12 time) (sentence 12 'pm))
       ((= 0 time) (sentence 12 'am))
       ((< time 12) (sentence time 'am))
))

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond ((< secs 60) (sentence secs 'seconds))
        ((and (<= 60 secs) (> 3600 secs)) (sentence (/ secs 60.0) 'minutes))
        ((and (<= 3600 secs) (> 86400 secs)) (sentence (/ secs 3600.0) 'hours))
        (else (sentence (/ secs 86400.0) 'days))
))

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

Explanation here.
Because word is a fucking function but you overwrote it in the scope as a variable,

|#

(define (count-ums str)
  (cond
   ((empty? str) 0)
   ((equal? (first str) 'um) (+ 1 (count-ums (bf str))))
   (else (count-ums (bf str))))
   )

(define (countdown n)
  (if (= n 0)
      'blastoff!
      (sentence n (countdown (- n 1)))
  ))

(countdown 3)
   
   