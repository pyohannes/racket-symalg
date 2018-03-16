#lang racket/base

;; Simplifying symbolic algebraic expressions.

(provide simplify)

;; ---------------------------------
;; Import and implementation section

(require multimethod
         racket/list
         racket/match
         "private/data.rkt"
         "private/zero-expr.rkt"
         "private/util.rkt"
         "smaller.rkt")

;; --------
;; simplify
;; --------

(define-generic (simplify e))

;; ------------
;; num-simplify
;; ------------

(module+ test
  (require rackunit
           "parse.rkt")


  ;; ASAE-1: u is an integer.
  (check-equal? (simplify (make-num 3))
                (make-num 3))
  (check-equal? (simplify (make-num (/ 4 5)))
                (make-frac 4 5))
  )

(define-instance ((simplify num) n)
  (define num (num-val n))
  (if (integer? num)
      n
      (make-frac (numerator num)
                 (denominator num))))

;; -------------
;; frac-simplify
;; -------------

(module+ test

  ;; ASAE-2: u is a fraction in standard form.
  (check-equal? (simplify (make-frac 3 4))
                (make-frac 3 4))
  (check-equal? (simplify (make-frac 4 1))
                (make-num 4))
  (check-equal? (simplify (make-frac 8 4))
                (make-num 2))
  (check-equal? (simplify (make-frac 4 8))
                (make-frac 1 2))
  ;; No division by zero
  (check-exn 
    exn:fail?
    (lambda () (simplify (make-frac 3 0))))
  )

(define-instance ((simplify frac) f)
  (apply-simplify f
                  frac?
                  (list frac-simplify/standard
                        frac-simplify/num
                        frac-simplify/zerodiv)))

(define (frac-simplify/standard f)
  (define num (frac-num f))
  (define denom (frac-denom f))
  (define g (gcd num denom))
  (if (> g 1)
      (make-frac (/ num g)
                 (/ denom g))
      f))

(define (frac-simplify/num f)
  (if (= (frac-denom f) 1)
      (make-num (frac-num f))
      f))

(define (frac-simplify/zerodiv f)
  (if (= (frac-denom f) 0)
      (error "Division by zero")
      f))

;; -----------------
;; constant-simplify
;; -----------------

(module+ test

  ;; ASAE-3: u is a constant.
  (check-equal? (simplify (make-constant 'pi))
                (make-constant 'pi))
  )

(define-instance ((simplify constant) c)
  c)

;; ------------
;; sym-simplify
;; ------------

(module+ test

  ;; ASAE-3: u is a symbol.
  (check-equal? (simplify (make-sym 'x))
                (make-sym 'x))
  )

(define-instance ((simplify sym) s)
  s)

;; ------------
;; add-simplify
;; ------------

(module+ test

  ;; ASAE-5.1
  (check-equal? (simplify (make-add (make-num 3) 
                                    (make-add (make-sym 'x) (make-sym 'y))))
                (make-add (make-num 3) (make-sym 'x) (make-sym 'y)))
  (check-equal? (simplify (make-add (make-num 3) 
                                    (make-add (make-sym 'x) (make-num 4))))
                (make-add (make-num 7) (make-sym 'x)))
  (check-equal? (simplify (make-add (make-sym 'x) (make-num 4) (make-num 0)))
                (make-add (make-num 4) (make-sym 'x)))
  (check-equal? (simplify (make-add (make-num 4) (make-num 0)))
                (make-num 4))
  (check-equal? (simplify (parse-sexpr '(+ (* 1 2) (* 3 4))))
                (parse-sexpr 14))

  ;; ASAE-5.2
  (check-equal? (simplify (make-add (make-num 3) (make-num 4)))
                (make-num 7))
  (check-equal? (simplify (make-add (make-num 3) (make-frac 3 4)))
                (make-frac 15 4))
  (check-equal? (simplify (make-add (make-num 3) (make-sym 'x) (make-num 4)))
                (make-add (make-num 7) (make-sym 'x)))
  (check-equal? (simplify (make-add (make-num 1) (make-num -1)))
                (make-num 0))

  ;; ASAE-5.3
  (check-equal? (simplify (make-add (make-sym 'x) (make-sym 'x)))
                (make-mul (make-num 2) (make-sym 'x)))
  (check-equal? (simplify (make-add (make-sym 'x) (make-num 4) (make-sym 'x)))
                (make-add (make-num 4) (make-mul (make-num 2) (make-sym 'x))))

  ;; ASAE-5.4
  (check-equal? (simplify (make-add (make-sym 'y) (make-sym 'x) (make-num 3)))
                (make-add (make-num 3) (make-sym 'x) (make-sym 'y)))
  )

(define-instance ((simplify add) a)
  (apply-simplify a 
                  add?  
                  (list add-simplify/children
                        (flattener-by-pred add? add-addends make-add)
                        add-simplify/constant
                        add-simplify/zero
                        add-simplify/mul
                        add-simplify/unary
                        add-simplify/sort)))

(define (add-simplify/children a)
  (apply make-add 
         (map simplify (add-addends a))))

(define (add-simplify/constant a)
  (define addends (add-addends a))
  (define a/nums (filter constant? addends))
  (cond ((not (null? a/nums))
         (define a/rest (filter (negate constant?) addends))
         (define a/nums/sum 
           (for/fold ([r (make-num 0)])
                     ([n a/nums])
             (constant-+ r n)))
         (add (cons a/nums/sum a/rest)))
        (else
          a)))

(define (add-simplify/zero a)
  (add (filter (negate zero-expr?)
               (add-addends a))))

(define (add-simplify/unary a)
  (define addends (add-addends a))
  (match (length addends)
    [0 (make-num 0)]
    [1 (car addends)]
    [_ a]))

(define (add-simplify/mul a)
  (define (ensure-mul t)
    (cond ((not (mul? t))
           (make-mul (make-num 1) t))
          ((not (constant? (car (mul-factors t))))
           (apply make-mul (cons (make-num 1)
                                 (mul-factors t))))
          (else
            t)))
  (define as (map ensure-mul (add-addends a)))
  (define hash/m
    (for/fold ([r (hash)])
              ([m as])
      (define const (car (mul-factors m)))
      (define term  (cdr (mul-factors m)))
      (hash-set r term (constant-+ const
                                   (hash-ref r term (make-num 0))))))
  (cond ((< (length (hash-keys hash/m))
            (length as))
         (define ms
           (for/list ([k (hash-keys hash/m)])
             (apply make-mul (cons (hash-ref hash/m k) k))))
         ;; Maybe there is a better way to avoid the simplify below. It is just
         ;; used to remove obsolete multiplications here.
         (apply make-add 
                (map simplify ms)))
        (else
          a)))

(define (add-simplify/sort a)
  (add (sort (add-addends a)
             smaller?)))

;; ------------
;; mul-simplify
;; ------------

(module+ test

  ;; ASAE-4.1
  (check-equal? (simplify (make-mul (make-sym 'x)
                                    (make-mul (make-sym 'y) (make-sym 'z))))
                (make-mul (make-sym 'x) (make-sym 'y) (make-sym 'z)))
  (check-equal? (simplify (make-mul 
                            (make-sym 'a)
                            (make-mul 
                              (make-sym 'b) 
                              (make-mul
                                (make-sym 'c)
                                (make-sym 'd)))))
                (make-mul (make-sym 'a) 
                          (make-sym 'b) 
                          (make-sym 'c)
                          (make-sym 'd)))

  ;; ASAE-4.2
  (check-equal? (simplify (make-mul (make-num 3) (make-sym 'x) (make-num 4)))
                (make-mul (make-num 12) (make-sym 'x)))
  (check-equal? (simplify (make-mul (make-num 3) 
                                    (make-sym 'x) 
                                    (make-frac 3 4)))
                (make-mul (make-frac 9 4) (make-sym 'x)))
  (check-equal? (simplify (make-mul (make-num 1) (make-sym 'x)))
                (make-sym 'x))
  (check-equal? (simplify (make-mul (make-num 0) (make-sym 'x)))
                (make-num 0))

  ;; ASAE-4.3
  (check-equal? (simplify (make-mul (make-power (make-sym 'x) (make-num 3))
                                    (make-power (make-sym 'x) (make-num 4))
                                    (make-sym 'z)))
                (make-mul (make-power (make-sym 'x) (make-num 7))
                          (make-sym 'z)))
  (check-equal? (simplify (make-mul (make-power (make-sym 'a) (make-sym 'x))
                                    (make-power (make-sym 'a) (make-sym 'y))
                                    (make-sym 'z)))
                (make-mul (make-power (make-sym 'a) 
                                      (make-add (make-sym 'x) (make-sym 'y)))
                          (make-sym 'z)))
  (check-equal? (simplify (make-mul (make-sym 'x) (make-sym 'x) (make-sym 'y)))
                (make-mul (make-power (make-sym 'x) (make-num 2))
                          (make-sym 'y)))
  (check-equal? (simplify (parse-sexpr '(/ (* 4 (expt x 3)) 5)))
                (make-mul (make-frac 4 5)
                          (make-power (make-sym 'x) (make-num 3))))

  ;; ASAE-4.4
  (check-equal? (simplify (make-mul (make-sym 'y) (make-sym 'x) (make-num 3)))
                (make-mul (make-num 3) (make-sym 'x) (make-sym 'y)))

  ;; No division by zero
  (check-exn 
    exn:fail?
    (lambda () (simplify (parse-sexpr '(/ 3 (- x x))))))
  )

(define-instance ((simplify mul) m)
  (apply-simplify m 
                  mul?  
                  (list mul-simplify/children
                        (flattener-by-pred mul? mul-factors make-mul)
                        mul-simplify/constant
                        mul-simplify/power
                        mul-simplify/unary
                        mul-simplify/sort
                        )))

(define (mul-simplify/children m)
  (apply make-mul
         (map simplify (mul-factors m))))

(define (mul-simplify/constant m)
  (define fs (mul-factors m))
  (define n/fs (filter constant? fs))
  (cond ((not (null? n/fs))
         (define rest/fs (filter (negate constant?) fs))
         (define n
           (for/fold ([r (frac 1 1)])
                     ([n n/fs])
             (constant-* r n)))
         (match (simplify n)
           [(== (make-num 1))
            (apply make-mul rest/fs)]
           [(== (make-num 0))
            (make-num 0)]
           [n
            (apply make-mul (cons (simplify n) 
                                  rest/fs))]))
        (else
          m)))

(define (mul-simplify/power m)
  (define (powerize t)
    (if (not (power? t))
        (make-power t (make-num 1))
        t))
  (define fs (map powerize (mul-factors m)))
  (define hash/p
    (for/fold ([r (hash)])
              ([p fs])
      (define base (power-base p))
      (hash-set r base (cons p
                             (hash-ref r base '())))))
  (cond ((< (length (hash-keys hash/p))
            (length fs))
         (define ps (map power-* (hash-values hash/p)))
         ;; Maybe there is a better way to avoid the simplify below. It is just
         ;; used to remove obsolete exponents here.
         (apply make-mul
                (map simplify ps)))
        (else
          m)))

(define (mul-simplify/unary m)
  (define factors (mul-factors m))
  (if (= 1 (length factors))
      (car factors)
      m))

(define (mul-simplify/sort m)
  (mul (sort (mul-factors m) 
             smaller?)))

;; --------------
;; power-simplify
;; --------------

(module+ test
  ;; ASAE-6.1: children as ASAEs
  (check-equal? (simplify (make-power (make-sym 'x) (make-add (make-num 1)
                                                              (make-num 2))))
                (make-power (make-sym 'x) (make-num 3)))
  (check-equal? (simplify (make-power (make-add (make-num 1) (make-num 1))
                                      (make-sym 'x)))
                (make-power (make-num 2) (make-sym 'x)))

  ;; ASAE-6.2: The exponent is not 0 or 1
  (check-equal? (simplify (make-power (make-sym 'x) (make-num 0)))
                (make-num 1))
  (check-equal? (simplify (make-power (make-sym 'x) (make-num 1)))
                (make-sym 'x))

  ;; ASAE-6.3: no multiplications in the base
  (check-equal? (simplify (make-power (make-mul (make-sym 'x) (make-sym 'y))
                                      (make-num 2)))
                (make-mul (make-power (make-sym 'x) (make-num 2))
                          (make-power (make-sym 'y) (make-num 2))))

  ;; ASAE-6.4: The base is not 0 or 1
  (check-equal? (simplify (make-power (make-num 1) (make-sym 'x)))
                (make-num 1))
  (check-equal? (simplify (make-power (make-num 0) (make-sym 'x)))
                (make-num 0))

  ;; No division by zero
  (check-exn 
    exn:fail?
    (lambda () (simplify (make-power (make-num 0) (make-num -1)))))
  )

(define-instance ((simplify power) p)
  (apply-simplify p 
                  power?  
                  (list power-simplify/children
                        power-simplify/num
                        power-simplify/mul
                        )))

(define (power-simplify/children p)
  (make-power (simplify (power-base p))
              (simplify (power-exponent p))))

(define (power-simplify/num p)
  (match (list (power-base p) (power-exponent p))
    [(list b (== (make-num 1))) 
     b]
    [(list b (== (make-num 0))) 
     (make-num 1)]
    [(list (== (make-num 0)) exponent) 
     (if (and (num? exponent)
              (negative? (num-val exponent)))
         (error "Division by zero")
         (make-num 0))]
    [(list (== (make-num 1)) _) 
     (make-num 1)]
    [(list (? constant? b) (? constant? e))
     (constant-expt b e)]
    [_
     p]))

(define (power-simplify/mul p)
  (define base (power-base p))
  (cond ((mul? base)
         (define e (power-exponent p))
         (define factors
           (for/list ([f (mul-factors base)])
             (power f e)))
         (simplify (apply make-mul factors)))
        (else
          p)))

;; ----------------------
;; polynomial/si-simplify
;; ----------------------

(module+ test
  (check-equal? (simplify (make-polynomial/si 'x '(0 1 2)))
                (make-polynomial/si 'x '(0 1 2)))
  )

(define-instance ((simplify polynomial/si) p)
  p)

;; -------------
;; logn-simplify
;; -------------

(module+ test
  (check-equal? (simplify (parse-sexpr '(logn (+ 1 2) (+ 3 4))))
                (parse-sexpr '(logn 3 7)))
  )

(define-instance ((simplify logn) l)
  (make-logn (simplify (logn-n l))
             (simplify (logn-base l))))

;; ------------
;; cos-simplify
;; ------------

(module+ test
  (check-equal? (simplify (parse-sexpr '(cos (+ 1 2))))
                (parse-sexpr '(cos 3)))
  )

(define-instance ((simplify cos_) c)
  (make-cos (simplify (cos_-n c))))

;; ------------
;; sin-simplify
;; ------------

(module+ test
  (check-equal? (simplify (parse-sexpr '(sin (+ 1 2))))
                (parse-sexpr '(sin 3)))
  )

(define-instance ((simplify sin_) s)
  (make-sin (simplify (sin_-n s))))

;; ------------
;; tan-simplify
;; ------------

(module+ test
  (check-equal? (simplify (parse-sexpr '(tan (+ 1 2))))
                (parse-sexpr '(tan 3)))
  )

(define-instance ((simplify tan_) s)
  (make-tan (simplify (tan_-n s))))

; --------------
; apply-simplify
; --------------

(module+ test
  (check-equal? (apply-simplify
                  3
                  number?
                  (list number->string symbol->string))
                "3"))


(define (apply-simplify expr pred? ops)
  (for/fold ([a expr])
            ([op ops])
            #:break (not (pred? a))
    (op a)))

; -----------------
; flattener-by-pred
; -----------------

(module+ test
  (check-equal? ((flattener-by-pred add? add-addends make-add)
                 (make-add (make-num 1) (make-add (make-num 2) (make-num 3))))
                (make-add (make-num 1) (make-num 2) (make-num 3)))
  )

(define (flattener-by-pred pred? accessor construct)
  (lambda (e)
    (apply 
      construct
      (flatten
        (for/list ([term (accessor e)])
          (if (pred? term)
              (accessor term)
              term))))))


; ----------
; combine-or
; ----------

(module+ test
  (check-true  ((combine-or-pred symbol? number?) 3))
  (check-true  ((combine-or-pred symbol? number?) 'ab))
  (check-false ((combine-or-pred symbol? number?) "ab"))
  (check-false ((combine-or-pred symbol? string?) 3))
  )

(define (combine-or-pred . preds)
  (lambda (x)
    (cond ((null? preds) #f)
          (((car preds) x) #t)
          (else
            ((apply combine-or-pred (cdr preds)) x)))))

; ---------
; constant?
; ---------

(module+ test
  (check-true  (constant? (make-num 3)))
  (check-true  (constant? (make-frac 3 4)))
  (check-false (constant? (make-sym 'x)))
  (check-false (constant? (make-mul (make-sym 'x) (make-num 3))))
  )

(define constant? (combine-or-pred num? frac?))

; ----------
; constant-+
; ----------

(module+ test
  (check-equal? (constant-+ (make-num 3) (make-num 4))
                (make-num 7))
  (check-equal? (constant-+ (make-num 3) (make-frac 3 2))
                (make-frac 9 2))
  (check-equal? (constant-+ (make-frac 2 3) (make-num 3))
                (make-frac 11 3))
  (check-equal? (constant-+ (make-frac 2 3) (make-frac 2 3))
                (make-frac 4 3))
  )

(define (constant-+ f1 f2)
  (match (list f1 f2)
    [(list (? num? f1) (? num? f2))
     (make-num (+ (num-val f1) (num-val f2)))]
    [(list (? num? f1) (? frac? f2))
     (constant-+ (make-frac (num-val f1) 1) f2)]
    [(list (? frac? f1) (? num? f2))
     (constant-+ f2 f1)]
    [(list (? frac? f1) (? frac? f2))
     (simplify (make-frac (+ (* (frac-num f1) (frac-denom f2))
                             (* (frac-num f2) (frac-denom f1)))
                          (* (frac-denom f1) (frac-denom f2))))]))

; ----------
; constant-*
; ----------

(module+ test
  (check-equal? (constant-* (make-num 3) (make-num 4))
                (make-num 12))
  (check-equal? (constant-* (make-num 3) (make-frac 3 2))
                (make-frac 9 2))
  (check-equal? (constant-* (make-frac 2 3) (make-num 3))
                (make-num 2))
  (check-equal? (constant-* (make-frac 2 3) (make-frac 2 3))
                (make-frac 4 9))
  )

(define (constant-* f1 f2)
  (match (list f1 f2)
    [(list (? num? f1) (? num? f2))
     (make-num (* (num-val f1) (num-val f2)))]
    [(list (? num? f1) (? frac? f2))
     (constant-* (make-frac (num-val f1) 1) f2)]
    [(list (? frac? f1) (? num? f2))
     (constant-* f2 f1)]
    [(list (? frac? f1) (? frac? f2))
     (simplify (make-frac (* (frac-num f1) (frac-num f2))
                          (* (frac-denom f1) (frac-denom f2))))]))

; -------------
; constant-expt
; -------------

(module+ test
  (check-equal? (constant-expt (make-num 3) (make-num 2))
                (make-num 9))
  (check-equal? (constant-expt (make-frac 1 2) (make-num 2))
                (make-frac 1 4))
  )

(define (constant-expt b e)
  (match (list b e)
    [(list (? num? b) (? num? e))
     (make-num (expt (num-val b) (num-val e)))]
    [(list (? frac? b) (? num? e))
     (simplify (make-frac (expt (frac-num b) (num-val e))
                          (expt (frac-denom b) (num-val e))))]))

; -------
; power-* 
; -------

(module+ test
  (check-equal? (power-* (list (make-power (make-sym 'x) (make-num 3))
                               (make-power (make-sym 'x) (make-num 4))))
                (make-power (make-sym 'x) (make-num 7)))
  (check-equal? (power-* (list (make-power (make-sym 'x) (make-num 1))
                               (make-power (make-sym 'x) (make-num -1))))
                (make-power (make-sym 'x) (make-num 0)))
  )

(define (power-* ps)
  (make-power (power-base (car ps))
              (simplify (apply make-add
                               (map power-exponent ps)))))
