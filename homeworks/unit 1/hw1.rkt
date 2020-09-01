#lang racket

(require berkeley)
(provide (all-defined-out))
; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  (cond
    ((empty? sent) '())
    ((not (member? (first sent) (bf sent))) (se (first sent) (dupls-removed (bf sent))))
    (else (dupls-removed (bf sent)))
    )
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (cond
    ((empty? sent) 0)
    ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
    (else (count-word (bf sent) wd))
    )
)

; Exercise 3

(define (new-if test then-case else-case)
  (if test
    then-case
    else-case))

(define (pigl wd)
  (new-if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here
There would be an infinite loop because the else case does not run.
|#

; Exercise 4 - Define squares

(define (squares sent)
  (if (empty? sent)
    '()
    (se (sqrt (first sent)) (squares (bf sent)))
)
  )


; Replace General
(define (third-person sent)
  (cond
    ((empty? sent) '())
    ((equal? (first sent) 'I) (se 'you (third-person (bf sent))))
    ((equal? (first sent) 'me) (se 'you (third-person (bf sent))))
    ((equal? (first sent) 'you) (se 'me (third-person (bf sent))))
    (else (se (first sent) (third-person (bf sent))))
  )
)

; Exercise 5 - Define switch

(define (switch sent)
  (if (equal? (first (third-person sent)) 'me)
      (se 'I (bf (third-person sent)))
      (third-person sent)
  )
)

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (cond
    ((empty? (bf sent)) #t)
    ((not (equal? (+ (first sent) 1) (first (bf sent)))) #f)
    (else (ordered? (bf sent)))
    )
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (cond
    ((empty? sent) '())
    ((equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent))))
    (else (ends-e (bf sent)))
  )
)
; Exercise 8

#|

To be fair I'm not sure if I should be
writing only an explanation.

Anyways, probably if we can do some sort of loop that checks
how many times did the condition run.

|#


