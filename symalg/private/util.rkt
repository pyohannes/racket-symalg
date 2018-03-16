#lang racket/base

;; Utility functions.

(provide
  negate
  string-startswith
  list-trim-right
  list-trim-left
  )

;; ---------------------------------
;; Import and implementation section

;; ---------------
;; list-trim-right
;; ---------------

(module+ test
  (require rackunit)
  (check-equal? (list-trim-right '(1 2 3 4 0 0 0) 0)
                '(1 2 3 4))
  (check-equal? (list-trim-right '(1 2 3) 0)
                '(1 2 3))
  )

(define (list-trim-right l token)
  (reverse (list-trim-left (reverse l) token)))

;; --------------
;; list-trim-left
;; --------------

(module+ test
  (check-equal? (list-trim-left '(0 0 0 1 2 3 4 0 0 0) 0)
                '(1 2 3 4 0 0 0))
  (check-equal? (list-trim-left '(1 2 3 ) 0)
                '(1 2 3))
  )

(define (list-trim-left l token)
  (cond ((null? l)
         '())
        ((equal? (car l) token)
         (list-trim-left (cdr l) token))
        (else
          l)))

; ------
; negate
; ------

(module+ test
 (check-true  ((negate odd?) 2))
 (check-false ((negate odd?) 3))
 )

(define (negate f)
  (define (negator . args)
    (not (apply f args)))
  negator)

; -----------------
; string-startswith
; -----------------

(module+ test
  (check-true  (string-startswith "abcdefgh" "abc"))
  (check-false (string-startswith "abcdefgh" "bc"))
  )

(define (string-startswith s prefix)
  (string=? (substring s 0 (string-length prefix)) prefix))

