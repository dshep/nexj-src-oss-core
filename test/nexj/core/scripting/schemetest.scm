; Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
(define test
   (macro (comp expr)
      (let
         (
            (compSym (string->symbol "#c"))
            (exprSym (string->symbol "#e"))
         )
         `(##let ((,compSym ,comp) (,exprSym ,expr))
            (##if (##equal? ,compSym ,exprSym)
               'passed
               (##error "Test failed: expected <{0}>, got <{1}>; expression: {2}" ,compSym ,exprSym ',expr)
            )
          )
      )
   )
)

; Javascript-only tests
(define-macro (when-limited-runtime . testcases)
   `(##when (##eq? (##get-value '*limited*) #t)
      (let ()
         (declare option (redefine-intrinsics #t))
         ,@testcases
      )
   )
)

; Java-only tests
(define-macro (when-full-runtime . testcases)
   `(##unless (##eq? (##get-value '*limited*) #t)
      ,@testcases
   )
)

; The comments indicate the sections in R4RS
; 2.1
(test 23 (length '(a-b a+b a1 + - a. a.b .. ... * / < = > ! ? : $ % _ & ~ ^)))

; 4.1.1
(define x 123)
(test 123 x)

; 4.1.2
(test 'a (quote a))
(test '(+ 1 2 3) (quote (+ 1 2 3)))
(test '(quote a) '(quote a))
(test "abc" '"abc")
(test "abc" "abc")
(test 123 '123)
(test 123 123)
(test #t '#t)
(test #t #t)

; 4.1.3
(test 3 (+ 1 2))
(test 3 ((if #t + -) 1 2))

; 4.1.4
(test #t (procedure? (lambda () 1)))
(test 9 ((lambda (x) (* x x)) 3))
(define (q x) (lambda (y z) (if x y z)))
(test 1 ((q #t) 1 2))
(test 2 ((q #f) 1 2))
(test '(1 2 3) ((lambda x x) 1 2 3))
(test '(2 3) ((lambda (x . y) y) 1 2 3))
(define n 0)
(begin 1
   ((lambda () (set! n (+ n 1))))
   (set! n (+ n 2))
)
(test 3 n)

; 4.1.5
(test 'f (if (> 1 2) 't 'f))
(test 't (if (> 2 1) 't 'f))
(test 3 (if (> 2 1) (+ 1 2) (- 1 2)))
(test 6 (+ 1 (if (< 1 2) 2) 3))
(define n 0)
(test 2 (if (begin (set! n 1) #t) 2 3))
(test 1 n)
(test 3 (if (begin (set! n 0) #f) 2 3))
(test 0 n)

; 4.1.6
(define x 1)
(test 3 (+ x 2))
(set! x 2)
(test 4 (+ x 2))

; 4.2.1
(test '() (cond))
(test #t (cond ((> 1 2))((> 2 1))))
(test 't (cond ((> 1 2) 'f) ((> 2 1) 't)))
(test 't (cond ((> 1 2) (+ 1 2) 'f) ((> 2 1) (+ 2 3) 't)))
(test '() (cond ((> 1 2) 'a) ((> 2 3) 'b)))
(test 'c (cond ((> 1 2) 'a) ((> 2 3) 'b) (else 'c)))
(test 'c (cond ((> 1 2) 'a) ((> 2 3) 'b) (else 1 2 3 'c)))
(test 'a (cond ((member 'a '(b a c)) => car) (else 'z)))
(test 't (case (+ 1 2) ((2) 1 2 'f) ((0 1) 'b) ((4 3) 't)))
(test '() (case (+ 1 5) ((2) 1 2 'f) ((0 1) 'b) ((4 3) 't)))
(test 'q (case (+ 1 5) ((2) 1 2 'f) ((0 1) 'b) ((4 3) 't) (else 1 2 'q)))
(test #t (and))
(test #t (and (> 2 1)))
(test #t (and (> 2 1) (> 3 2)))
(test #f (and (> 2 1) (> 2 3)))
(test #t (and 1 (> 3 2)))
(test 1 (and (> 3 2) 1))
(test #f (and #f (/ 1 0)))
(test #f (and #f ((lambda () (/ 1 0)))))
(test #f (or))
(test #t (or (> 3 2)))
(test #t (or (> 1 2) (> 2 1)))
(test #f (or (> 1 2) (> 2 3)))
(test #t (or #f #f #f #t))
(test #f (or #f #f #f #f))
(test '(2 3) (or (member 2 '(1 2 3)) (/ 1 0)))

; 4.2.2

(test 3  (let ((x 1) (y 2))
            (+ x y)
         )
)

(test 6  (let ((x 1) (y 2))
            (let ((x (+ x y)))
               (* x y)
            )
         )
)

(test 10 (let* ((x 2) (y (+ x 3))) (* x y)))

(test 30 (letrec ((a (lambda (n total) (if (= n 0) total (b (- n 1) (+ total 1)))))
                  (b (lambda (n total) (if (= n 0) total (a (- n 1) (+ total 1))))))
                  (a 10 20)
         )
)

(test 30 (letrec* ((a (lambda (n total) (if (= n 0) total (b (- n 1) (+ total 1)))))
                   (b (lambda (n total) (if (= n 0) total (a (- n 1) (+ total 1))))))
                   (a 10 20)
         )
)

(define x 1)

; 4.2.3
(test 5 (begin (set! x 2) (set! x (+ x x)) (+ x 1)))
(set! x 1)
((lambda () 1 (begin 1 (set! x 3)) 2))
(test 3 x)

; 4.2.4
(test 15 (let ((s 0)) (do ((i 0 (+ i 1))) ((> i 5) s) (set! s (+ s i)))))
(test #(1 2 3) (do ((v (make-vector 3)) (i 0 (+ i 1))) ((= i (vector-length v)) v) (vector-set! v i (+ i 1))))

(test 15 (let loop ((n 5) (total 10))
            (if (= n 0)
               total
               (loop (- n 1) (+ total 1))
            )
         )
)

; 4.2.6
(test (quasiquote 1) `1)

(define x 'abc)
(test '(f abc 'abc) `(f ,x ',x))
(test '(f a b c . abc) `(f ,@'(a b) ,'c . ,x))
(test '(f a b c abc q) `(f ,@'(a b) c ,@`(,x q)))
(test '#(1 2 3 4 5) `#(1 2 ,(+ 1 2) ,@'(4 5)))
(test '(f a `(g ,(zero? 1) ,(q 3 1 2) c)) `(f a `(g ,(zero? 1) ,(q ,(+ 1 2) ,@'(1 2)) c)))

(let ((val 1) (lst '(1 2 3)))
   (test 1 (quasiquote (unquote val)))
   (test '() (quasiquote ((unquote))))
   (test '(1) (quasiquote ((unquote val))))
   (test '(1 1 1) (quasiquote ((unquote val val val))))

   (test '() (quasiquote ((unquote-splicing))))
   (test '(1 2 3) (quasiquote ((unquote-splicing lst))))
   (test '(1 2 3 1 2 3 1 2 3) (quasiquote ((unquote-splicing lst lst lst))))

   (test '(1 0 1 1 1 0 0 1 2 3 0 1 2 3 1 2 3 1 2 3 0 12345)
      (quasiquote
         (,val 0 (unquote val val val) 0 (unquote) 0
         ,@lst 0 (unquote-splicing lst lst lst) 0 (unquote-splicing) 12345)
      )
   )

   (test '(1 2 `(3 ,1)) (quasiquote (1 2 ((global quasiquote) (3 (unquote (unquote val)))))))
   (test '(1 2 `(3 ,1)) `(1 2 (##quasiquote (3 ,,val))))
)

(when-full-runtime
   (test '(1 0 1 1 1 0 0 1 2 3 0 1 2 3 1 2 3 1 2 3 0 12345)
      (eval
         `(let ((val 1) (lst '(1 2 3)))
            (quasiquote
               (,val 0 (unquote val val val) 0 (unquote) 0
               ,@lst 0 (unquote-splicing lst lst lst) 0 (unquote-splicing) 12345)
            )
         )
      )
   )
)

(test '(1 2 3 (4 5)) (quasiquote (1 (unquote-splicing '(2 3)) (unquote '(4 5)))))
(test '(1 2 3 4 5) (quasiquote (1 (unquote 2 3) (unquote-splicing '(4) '(5)))))
(test '(1 2 3 (4) (5)) (quasiquote (1 (unquote-splicing '(2) '(3)) (unquote '(4) '(5)))))

; 5.2.1

(begin
   (begin)
   (define x 123)
   (begin
      (define y 124)
   )
   (begin)
)

(test 247 (+ x y))

(define f (lambda (x) (* x x)))
(test 4 (f 2))

(define (f x) (* x x x))
(test 8 (f 2))

(define (f) 2)
(test 2 (f))

(define (f . x) x)
(test '(1 2 3) (f 1 2 3))

(define (f x y . z) z)
(test '(3 4 5) (f 1 2 3 4 5))

(define f car)
(test 'a (f '(a b)))

; 5.2.2

(test 12 (let ()
            (begin
               (begin
                  (begin)
                  (define (a x) (* x x (b x)))
                  (begin)
               )
               (define (b x) (+ x 1))
            )
            (begin)
            (a 2)
         )
)

; 6.1
(test #f (not #t))
(test #t (not #f))
(test #f (not 0))
(test #f (not '()))
(test #f (not '(1 2 3)))
(test #f (not (list)))
(test #f (not 't))

(test #t (boolean? #t))
(test #t (boolean? #f))
(test #f (boolean? 1))
(test #f (boolean? "abc"))
(test #f (boolean? '()))

; 6.2
(test #t (eqv? 'a 'a))
(test #f (eqv? 'a 'b))
(test #t (eqv? 1 1))
(test #t (eqv? 1.0 1.0))
(test #t (eqv? 1N 1))
(when-full-runtime
   (test #f (eqv? 1 1.0))
   (test #f (eqv? 1N 1.0))
)
(test #f (eqv? 1 2))
(test #t (eqv? '() '()))
(test #t (eqv? #\A #\A))
(test #f (eqv? #\A #\a))
(define x '(1 2 3))
(test #t (eqv? x x))
(define f (lambda () 1))
(test #t (eqv? f f))
(test #f (eqv? x f))
(test #f (eqv? 1 'a))
(test #t (eqv? #t #t))
(test #f (eqv? #t #f))
(test #f (eqv? '(1 2) '()))
(define (f x) (lambda () x))
(test #f (eqv? (f 1) (f 2)))

(test #t (eq? 'a 'a))
(test #t (eq? #t #t))
(test #t (eq? #f #f))
(test #f (eq? #t #f))
(test #t (eq? '() '()))
(test #f (eq? '(a b c) '()))
(test #t (eq? + +))
(test #t (eq? x x))
(test #f (eq? x f))

(test #t (equal? 'a 'a))
(test #t (equal? '(1 2 3) '(1 2 3)))
(test #f (equal? '(1 2 3) '(1 2 4)))
(test #t (equal? "abc" "abc"))
(test #f (equal? "abc" "cde"))
(test #t (equal? 1 1))
(test #t (equal? (make-vector 10 1) (make-vector 10 1)))
(test #t (equal? #t #t))

; 6.3
(test #t (pair? '(a b)))
(test #t (pair? '(a . b)))
(test #f (pair? '()))
(test #f (pair? '#(a b c)))
(test #f (pair? (lambda() 1)))
(test #f (pair? 1))
(test #f (pair? #t))

(test '(a) (cons 'a '()))
(test '((a) b) (cons '(a) '(b)))
(test '(a . b) (cons 'a 'b))

(test 1 (car '(1 2 3)))
(test '(1 2) (car '((1 2) 3)))
(test 1 (car '(1 . 2)))

(test '(2 3) (cdr '(1 2 3)))
(test '(2) (cdr '(1 2)))
(test '2 (cdr '(1 . 2)))

(define x (list 1 2))
(set-car! x 3)
(test '(3 2) x)

(set-cdr! x 4)
(test '(3 . 4) x)

(test 1 (caar '((1))))
(test 2 (cadr '(1 2)))
(test '(3) (cddr '(1 2 3)))
(test 1 (caaaar '((((1))))))
(test '(5) (cddddr '(1 2 3 4 5)))

(test #t (null? '()))
(test #f (null? 1))
(test #f (null? '(a)))

(test #t (list? '()))
(test #t (list? '(1 2 3)))
(test #f (list? '(1 2 . 3)))
(define x (list 1 2))
(set-cdr! (cdr x) x)
(test #f (list? x))

(test '(1 2 3 a) (list 1 2 (+ 1 2) 'a))
(test '() (list))

(test 0 (length '()))
(test 3 (length '(1 2 3)))
(test 3 (length '((1) (2 3) 4)))

(test '() (append))
(test '(1 2 3) (append '(1) '(2 3)))
(test '(1 (2 3)) (append '(1) '((2 3))))
(test '(1 2 3) (append '() '() '(1) '() '(2 3)))

(test '(3 2 1) (reverse '(1 2 3)))
(test '(4 (2 3) 1) (reverse '(1 (2 3) 4)))

(test '(1 2 3) (list-tail '(1 2 3) 0))
(test '(3) (list-tail '(1 2 3) 2))
(test '() (list-tail '(1 2 3) 3))

(test 'b (list-ref '(a b c) 1))
(test 'c (list-ref '(a b c) 2))

(test '(a b c) (memq 'a '(a b c)))
(test '(c) (memq 'c '(a b c)))
(test #f (memq 'd '(a b c)))
(test #f (memq (cons 'a 'b) '(a (a . b) b)))

(test '(1 2 3) (memv 1 '(1 2 3)))
(test '(3) (memv 3 '(1 2 3)))
(test #f (memv 4 '(1 2 3)))
(test '((a . b) b) (member (cons 'a 'b) '(a (a . b) b)))

(test '(a . 1) (assq 'a '((a . 1) (b . 2) (c . 3))))
(test '(c . 3) (assq 'c '((a . 1) (b . 2) (c . 3))))
(test #f (assq (list 'a) '(((a) . 1) ((b) . 2) ((c) . 3))))
(test '(1 . a) (assv 1 '((1 . a) (2 . b) (3 . c))))
(test '(3 . c) (assv 3 '((1 . a) (2 . b) (3 . c))))
(test '((b) . 2) (assoc (list 'b) '(((a) . 1) ((b) . 2) ((c) . 3))))

; 6.4
(test #t (symbol? 'a))
(test #f (symbol? 1))
(test #f (symbol? #t))

(test "abc" (symbol->string 'abc))
(test "Abc" (symbol->string 'Abc))

(test #t (eq? (string->symbol "abc") 'abc))
(test #t (eq? (string->symbol "Abc") 'Abc))
(test "a b c" (symbol->string (string->symbol "a b c")))

; 6.5.5

(test 38.0 (+ #d#e10 #i#xF #b101 #o10))

(test #t (number? 1))
(test #f (number? 'a))
(test #t (complex? 1))
(test #f (complex? 'a))
(test #t (real? 1))
(test #t (real? 1.0))
(test #t (real? #e1))
(test #t (rational? 1))
(test #t (rational? 1.0))
(test #t (rational? 1.3))
(test #t (integer? 1))
(test #t (integer? 1.0))
(test #f (integer? 1.1))
(test #t (integer? 1.0N))

(test #t (exact? 1))
(when-full-runtime
   (test #t (exact? 1.23N))
   (test #f (exact? 1.0))
   (test #f (exact? #i1))
   (test #t (exact? #e1.1))
)

(test #f (inexact? 1))
(when-full-runtime
   (test #f (inexact? 1.23N))
   (test #t (inexact? 1.0))
   (test #t (inexact? #i1))
   (test #f (inexact? #e1.1))
)

(test #t (= 1 1 1))
(test #t (= 1 1.0 1))
(test #f (= 1 1 1.1))
(test #f (= 1.1 1.2 1.3N))
(test #t (!= 1 2 2))
(test #f (!= 1 1 1))
(test #f (!= 1 1 1.0))
(test #t (!= 1 1.0 1.1))
(test #t (< 1 2 3))
(test #f (< 1 3 2))
(test #t (< 1 1.1 1.2))
(test #f (< 1 1.1 1.1))
(test #t (<= 1 2 2))
(test #f (<= 2 1 2))
(test #f (<= 2.1 2.0 2.0))
(test #t (<= 2.0 2 2.1))
(test #t (> 3 2 1))
(test #f (> 3 1 2))
(test #t (> 1 0.9 0.8))
(test #f (> 1.0 0.9 0.9))
(test #t (>= 2 2 1))
(test #f (>= 2 1 2))
(test #t (>= 2 2.0 1.9))
(test #f (>= 2 2 2.01))

(test #t (zero? 0))
(test #t (zero? 0.0))
(test #f (zero? 1))
(test #f (zero? 0.1))
(test #t (positive? 1))
(test #f (positive? 0))
(test #f (positive? -1))
(test #t (positive? 1E3))
(test #f (positive? -1.0))
(test #f (positive? 0.0))
(test #t (negative? -1))
(test #f (negative? 0))
(test #f (negative? 1))
(test #t (negative? -0.01))
(test #f (negative? 0.0))
(test #f (negative? 2.1))
(test #t (odd? 3))
(test #f (odd? 0))
(test #f (odd? -2))
(test #f (odd? 2))
(test #t (odd? 5.0))
(test #f (odd? 5.1))
(test #t (even? 6))
(test #t (even? 0))
(test #f (even? 5))
(test #t (even? 10.0))
(test #f (even? 10.1))

(test 3 (max 1 2 3))
(test 3.0 (max 1.1 2 3 2.1))
(test 1 (min 2 1 3))
(test -2.0 (min 2.0 -2 3))

(test 0 (+))
(test 2 (+ 2))
(test 6.0 (+ 1 2.0 3))
(test 1 (*))
(test 2 (* 2))
(test 6.0 (* 1 2 3.0))

(test -2 (- 2))
(test -4.1 (- 1 2.1 3))
(test 0.5N (/ 2))
(test 0.5 (/ 2.0))
(test 1.5N (/ 3 2 1))
(test -1.5 (/ -3.0 2 1))

(test 3 (abs 3))
(test 3.0 (abs -3.0))
(test 0.0 (abs 0.0))

(test 3 (quotient 11 3))
(test -3 (quotient 11 -3))
(test -3 (quotient -11 3))
(test 3 (quotient -11 -3))
(test 3.0 (quotient 11.0 3))
(test 2 (modulo 11 3))
(test -1 (modulo 11 -3))
(test 1 (modulo -11 3))
(test -2 (modulo -11 -3))
(test 2.0 (modulo 11 3.0))
(test 2 (remainder 11 3))
(test 2 (remainder 11 -3))
(test -2 (remainder -11 3))
(test -2 (remainder -11 -3))
(test 2.0 (remainder 11 3.0))

(test 0 (gcd))
(test 3 (gcd 12 9))
(test 3.0 (gcd 12 9 18.0))
(test 1 (lcm))
(test 36 (lcm 12 9))
(test 36.0 (lcm 12 9.0 3))

(test 2 (floor 2))
(test 3.0 (floor 3.5))
(test -4.0 (floor -3.5))
(test 4.0 (ceiling 3.5))
(test -3.0 (ceiling -3.5))
(test 3.0 (truncate 3.5))
(test -3.0 (truncate -3.5))
(test 4.0 (round 3.5))
(test -4.0 (round -3.5))
(test 4.0 (round 4.5))
(test -4.0 (round -4.5))
(test 4.0 (round-at 3.5 0))
(test -4.0 (round-at -3.5 0))
(test 5.0 (round-at 4.5 0))
(test -5.0 (round-at -4.5 0))
(test 3.5 (round-at 3.5 1))
(test -3.5 (round-at -3.5 1))
(test 4.11 (round-at 4.105 2))
(test -4.11 (round-at -4.105 2))
(test 4.105 (round-at 4.1045 3))
(test -4.105 (round-at -4.1045 3))
(test 4.104 (round-at 4.10445 3))
(test -4.104 (round-at -4.10445 3))

(test #t (< (abs (- (exp 1) 2.71828)) 0.00001))
(test #t (< (abs (- (log 2.71828) 1)) 0.00001))
(test #t (< (abs (sin 3.1415)) 0.0001))
(test #t (< (abs (- (cos 3.1415) -1)) 0.0001))
(test #t (< (abs (tan 3.1415)) 0.0001))
(test #t (< (abs (- (asin 1) 1.5708)) 0.0001))
(test #t (< (abs (- (acos -1) 3.1415)) 0.0001))
(test #t (< (abs (- (* 4 (atan 1)) 3.1415)) 0.0001))
(test #t (< (abs (- (* 4 (atan 1 1)) 3.1415)) 0.0001))
(test #t (< (abs (- (* 4 (atan 1 1)) 3.1415)) 0.0001))
(test #t (< (abs (- (* 2 (atan 1 0)) 3.1415)) 0.0001))
(test #t (< (abs (- (sqrt 100) 10)) 0.00001))

(test 1.0 (exact->inexact 1))
(test 1.0 (exact->inexact 1.0))
(test 1.0 (exact->inexact 1N))
(when-full-runtime
   (test 1.5N (inexact->exact 1.5))
)
(test 1 (inexact->exact 1.0))
(test 1 (inexact->exact 1))

; 6.5.6
(test "255" (number->string 255))
(test "-255" (number->string -255))
(when-full-runtime
   (test "255.0" (number->string 255.0))
   (test "255N" (number->string 255N))
)
(test "11111111" (number->string 255 2))
(test "377" (number->string 255 8))
(test "255" (number->string 255 10))
(test "ff" (number->string 255 16))
(test "-ff" (number->string -255 16))

(test 123 (string->number "123"))
(test 123.0 (string->number "123.0"))
(when-full-runtime
   (test 123.0 (string->number "123.##"))
   (test 123N (string->number "123.00N"))
)
(test -255 (string->number "-11111111" 2))
(test -255 (string->number "-377" 8))
(test -255 (string->number "-255" 10))
(test -255 (string->number "-ff" 16))
(when-full-runtime
   (test -255.0 (string->number "#i#b-11111111" 10))
   (test #f (string->number "abc"))
)

; 6.6
(test #t (char? #\A))
(test #f (char? 1))

(test #t (char=? #\A #\A))
(test #f (char=? #\A #\B))
(test #t (char<? #\A #\B))
(test #f (char<? #\A #\A))
(test #t (char<=? #\A #\B))
(test #t (char<=? #\A #\A))
(test #f (char<=? #\B #\A))
(test #t (char>? #\B #\A))
(test #f (char>? #\A #\A))
(test #t (char>=? #\B #\A))
(test #t (char>=? #\A #\A))
(test #f (char>=? #\A #\B))

(test #t (char-ci=? #\A #\a))
(test #f (char-ci=? #\A #\b))
(test #t (char-ci<? #\a #\B))
(test #f (char-ci<? #\A #\a))
(test #t (char-ci<=? #\a #\B))
(test #t (char-ci<=? #\a #\A))
(test #f (char-ci<=? #\B #\a))
(test #t (char-ci>? #\B #\a))
(test #f (char-ci>? #\a #\A))
(test #t (char-ci>=? #\B #\a))
(test #t (char-ci>=? #\A #\a))
(test #f (char-ci>=? #\a #\B))

(test #t (char-alphabetic? #\a))
(test #f (char-alphabetic? #\1))
(test #f (char-alphabetic? #\,))
(test #f (char-numeric? #\a))
(test #t (char-numeric? #\1))
(test #f (char-numeric? #\,))
(test #t (char-whitespace? #\newline))
(test #f (char-whitespace? #\1))
(test #t (char-upper-case? #\A))
(test #f (char-upper-case? #\a))
(test #f (char-upper-case? #\1))
(test #f (char-lower-case? #\A))
(test #t (char-lower-case? #\a))
(test #f (char-lower-case? #\1))

(test 65 (char->integer #\A))
(test #\A (integer->char 65))

(test #\A (char-upcase #\a))
(test #\A (char-upcase #\A))
(test #\1 (char-upcase #\1))
(test #\a (char-downcase #\A))
(test #\a (char-downcase #\a))
(test #\1 (char-downcase #\1))

; 6.7
(test #t (string? "abc"))
(test #f (string? 1))
(test "   " (make-string 3))
(test "aaa" (make-string 3 #\a))
(test "abc" (string #\a #\b #\c))
(test 3 (string-length "abc"))
(test #\b (string-ref "abc" 1))
(test #t (string=? "abc" "abc"))
(test #f (string=? "abc" "abC"))
(when-limited-runtime
   (test #t (string-eol=? '() '()))
   (test #t (string-eol=? '() ""))
   (test #t (string-eol=? "" '()))
   (test #f (string-eol=? "left" '()))
   (test #f (string-eol=? '() "right"))

   (let ((left "left"))
      (test #f (string-eol=? left "right"))
      (test #t (string-eol=? left "left"))
      (test #t (string-eol=? left left))
   )

   (test #t (string-eol=? "\rone\ntwo\n\r\r\nthree\r" "\rone\r\ntwo\n\r\n\nthree\n"))
   (test #t (string-eol=? "\r\none\ntwo\n\r\r\nthree\r" "\rone\ntwo\n\n\nthree\r\n"))
   (test #f (string-eol=? "one\r\rtwo" "one\r\ntwo"))
   (test #f (string-eol=? "one\n\rtwo" "one\r\ntwo"))
   (test #f (string-eol=? "one\r\ntwo" "one\n\ntwo"))
)
(test #t (string-ci=? "abc" "aBc"))
(test #f (string-ci=? "abc" "aBd"))
(test #t (string<? "abC" "abc"))
(test #f (string<? "abc" "abC"))
(test #t (string-ci<? "abc" "aBd"))
(test #f (string-ci<? "abc" "aBc"))
(test #t (string<=? "abC" "abc"))
(test #t (string<=? "abc" "abc"))
(test #f (string<=? "abc" "abC"))
(test #t (string-ci<=? "abc" "aBd"))
(test #t (string-ci<=? "abc" "aBc"))
(test #f (string-ci<=? "aBd" "abc"))
(test #t (string>? "abc" "abC"))
(test #f (string>? "abC" "abc"))
(test #t (string-ci>? "aBd" "abc"))
(test #f (string-ci>? "aBc" "abc"))
(test #t (string>=? "abc" "abC"))
(test #t (string>=? "abc" "abc"))
(test #f (string>=? "abC" "abc"))
(test #t (string-ci>=? "aBd" "abc"))
(test #t (string-ci>=? "aBc" "abc"))
(test #f (string-ci>=? "abc" "aBd"))

(test "bc" (substring "abcd" 1 3))
(test "abcd" (string-append "ab" "c" "d"))
(test '(#\a #\b #\c) (string->list "abc"))
(test "abc" (list->string '(#\a #\b #\c)))

; 6.8
(test #t (vector? '#(1 2 3)))
(test #f (vector? '(1 2 3)))

(test '#(() () ()) (make-vector 3))
(test '#() (make-vector 0))
(test '#(0 0 0) (make-vector 3 0))
(test '#(1 2 3) (vector 1 2 3))
(test '#() (vector))
(test 3 (vector-length '#(1 2 3)))
(test 2 (vector-ref '#(1 2 3) 1))
(define v (vector 1 2 3))
(vector-set! v 1 "abc")
(test '#(1 "abc" 3) v)
(test '(1 2 3) (vector->list '#(1 2 3)))
(test '#(1 2 3) (list->vector '(1 2 3)))

; 6.9
(test #t (procedure? +))
(test #t (procedure? (lambda () 1)))
(test #f (procedure? 1))
(test #t (call/cc procedure?))
(test #t (call-with-current-continuation procedure?))

(test 6 (apply + '(1 2 3)))
(test 10 (apply + 1 2 '(3 4)))
(test 4.0 (((lambda (a b) (lambda x (a (apply b x)))) sqrt +) 7 9))

(test '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)))
(test '(1.0 2.0 3.0) (map sqrt '(1 4 9)))
(test '(4 4 4) (map + '(1 2 3) '(3 2 1)))

(define v (make-vector 3))
(for-each (lambda (i) (vector-set! v i i)) '(0 1 2))
(test '#(0 1 2) v)
(for-each (lambda (i x) (vector-set! v i x)) '(0 1 2) '(4 5 6))
(test '#(4 5 6) v)

(define (stream n)
   (let tail ((n n))
      (cons n (delay (tail (+ n 1))))
   )
)

(define (tail stream) (force (cdr stream)))

(test 4 (car (tail (tail (tail (stream 1))))))

(test 1 (call/cc 
           (lambda (return)
              (define i 0)
              (let f ((x '(1 2 3)))
                 (if (eqv? (car x) 2)
                    (return i)
                    (begin
                       (set! i (+ i 1))
                       (f (cdr x))
                    )
                 )
              )
           )
        )
)

(define top '())
(define cont '())

(define (ask)
   (call/cc
      (lambda (resume)
         (set! cont resume)
         (top #f)
      )
   )
)

(define (task)
   (let ((x (ask)))
      (if (eqv? x 1) 'a (task))
   )
)

(call/cc
   (lambda (resume)
      (set! top resume)
   )
)

(let () (test 'a (task)))
(cont 1)
(let () (test #t (task)))
(cont 2)

; 6.10.1
(when-full-runtime
   (test 'a
      (call-with-input-file file-url
         (lambda (p)
            (test #t (input-port? p))
            (test #f (output-port? p))
            (test #\# (peek-char p))
            (test #t (char-ready? p))
            (test #\# (read-char p))
            (test '(hello world!) (read p))
            (test #t (eof-object? (read p)))
            (test #t (eof-object? (peek-char p)))
            (test #f (char-ready? p))
            'a
         )
      )
   )

   (let ((p (open-input-file file-url)))
      (test #t (input-port? p))
      (test #f (output-port? p))
;      (close-input-port p)
   )

   (test 'a
      (with-input-from-file file-url
         (lambda ()
            (test #t (input-port? (current-input-port)))
            (test #f (output-port? (current-input-port)))
            (test #\# (peek-char))
            (test #t (char-ready?))
            (test #\# (read-char))
            (test '(hello world!) (read))
            (test #t (eof-object? (read)))
            (test #t (eof-object? (peek-char)))
            (test #f (char-ready?))
            'a
         )
      )
   )
)

; extensions

; continuation barriers
(define cont '())
(define result '())
(let ()
   (set! result (call/cc (lambda (c) (set! cont c))))
   (set! result 123)
)
(test 123 result)
(set! result 0)
(cont 1)
(test 123 result)

(let ()
   (call-with-continuation-barrier
      (lambda ()
         (set! result (call/cc (lambda (c) (set! cont c))))
      )
   )
   (set! result 123)
)

(test 123 result)
(set! result 0)
(cont 1)
(test 1 result)

; data types 
(test #t (binary? #z012A))
(test #f (binary? "012A"))
(test #t (binary? (cast binary "012A")))

(test #t (timestamp? #m1980-01-01T00:00:00))
(test #f (timestamp? "1980-01-01T00:00:00"))
(test #t (timestamp? (cast timestamp "1980-01-01T00:00:00")))
(test #m1980-01-01T01:02:03 (cast timestamp "1980-01-01T01:02:03"))
(test #m1980-01-01 (cast timestamp "1980-01-01"))
(test #m01:02:03 (cast timestamp "01:02:03"))


(when-full-runtime
   (test 6 (eval '(+ 1 2 3)))
   (let ((compiled (eval '(lambda (x) (* x x)))))
      (test 4 (compiled 2))
   )

   (test #t (macro? let))
   (test #f (macro? map))
   (test #f (macro? car))
)

(test #(1 4 9) (map (lambda (x) (* x x)) #(1 2 3)))
(test 9 ((map (lambda (x) (* x x)) (collection 1 2 3)) 2))

(test #t (some > '(1 2 3) '(1 1 3)))
(test #f (some > '(1 2 3) '(1 2 3)))
(test #t (every > '(1 2 3) '(0 1 2)))
(test #f (every > '(1 2 3) '(0 2 2)))

(when-full-runtime
   (test 2 (cast integer "2"))
   (test #t (cast boolean 1))
)

(test 1 (ifnull '() 1))
(test 1 (ifnull 1 2))

(when-full-runtime
   (import 'java.lang.Integer 'java.lang.Number)
   (test #t (instance? 1 java.lang.Integer))
   (test #f (instance? 1.0 java.lang.Integer))
   (test #t (instance? 1.0 java.lang.Number))
   (test #f (instance? "abc" java.lang.Number))
   (let ((u (User'new)))
      (test #t (instance? u Principal))
      (test #t (instance? u User))
      (test #f (instance? u Group))
   )
   (test #t (instance? 1 sys:integer))
)

(when-limited-runtime
   (test #t (instance? 1 sys:double))
)

(test #f (instance? 1.0 sys:integer))
(test #t (instance? "abc" sys:string))
(test #t (instance? "abc" sys:any))
(test #t (instance? 1.0 sys:any))
(test #f (instance? () sys:any))


(test 3 (try 3 (lambda (e) e)))
(test 'z (try (car 1) (lambda (e) 'z)))
(test 'z (try (try (car 1) (lambda (e) (throw e))) (lambda (e) 'z)))
(define x 1)
(test 3 (try 3 (lambda (e) e) (set! x 2)))
(test 2 x)
(test 'z (try (car 1) (lambda (e) 'z) (set! x 3)))
(test 3 x)
(test 'z (try (try (car 1) (lambda (e) (throw e)) (set! x 4)) (lambda (e) 'z)))
(test 4 x)

(when-full-runtime
   (test "a1b2" (string-expand "a${a}b${b}" (lambda (v) (cdr (assoc v `(("a" . 1) ("b" . 2)))))))
)

(define obj '())
(unless (null? (get-value "MyFun"))
   (test 3 (MyFun'add 1 2))
   (set! obj (MyFun'new 0))
   (test #t (obj'zero))
   (obj'value 3)
   (test 3 (obj'value))
   (test 4 (obj'add 1))
   (test #f (obj'zero))
)

(define col (collection "a" "b" "c" "d"))
(test #t (collection? col))
(test #t (vector? col))
(test 4 (vector-length col))
(test "a" (col'get 0))
(test "d" (col'get 3))
(test '("a" "b" "c" "d") (vector->list col))
(test "a" (vector-ref col 0))
(test "e" (vector-set! col 3 "e"))
(test "e" (vector-ref col 3))
(test "e" (col 3))
(test "f" (col 3 "f"))
(test "f" (col 3))
(define vec (vector "a" "b" "c"))
(test "b" (vec 1))
(test "q" (vec 1 "q"))
(test "q" (vec 1))

(define col (make-collection 3))
(test #t (collection? col))
(test 0 (vector-length col))
(test 0 (collection-length col))
(col'add "a")
(col'add "b")
(col'add "c")
(test 3 (vector-length col))
(test 3 (collection-length col))
(sort! col >)
(test "c" (col'get 0))
(test "b" (col'get 1))
(test "a" (col'get 2))
(sort! col <)
(test "c" ((sort col >)'get 0))
(test '(1 1 2 3 4) (sort! (list 3 4 1 2 1) <))
(test '(1 1 2 3 4) (sort '(3 4 1 2 1) <))
(test #(1 1 2 3 4) (sort! (vector 3 4 1 2 1) <))
(test #(1 1 2 3 4) (sort #(3 4 1 2 1) <))

(test 10 (apply + 1 2 (collection 3 4)))
(test 10 (apply + 1 2 #(3 4)))

(test #(1 () 3 5) (unique #(1 () 3 () 5 3)))
(test 3 (count #(1 () 3 () 5)))
(test 6 (sum #(1 2 () 3)))
(test '() (sum '()))
(test 2 (average #(1 2 () 3)))
(test '() (average '()))
(test 1 (minimum #(1 2 () 3)))
(test '() (minimum '()))
(test 3 (maximum #(1 2 () 3)))
(test '() (maximum '()))
(test 1 (collection->value (collection 1 2 3)))
(test 1 (collection->value 1))
(test () (collection->value '()))
(test () (collection->value (collection)))
(test (collection 1) (value->collection 1))
(test (collection 1 2 3) (value->collection (collection 1 2 3)))
(test (collection) (value->collection '()))

(when-full-runtime
   (let ((o (oid "abc" (oid 2))))
      (test "OID:2:S3:abc:I1:2" (o'toString))
      (test 2 (o'count))
      (test "abc" (o'getValue 0))
      (test 2 (o'getValue 1))
   )
)
(when-limited-runtime
   (test #f (null? (oid "abc" 2)))
)

(when-full-runtime
   (let ((o (oid 1 4294967296L 3.1 4.123456789D #f #t #z0102ABCD "test")))
      (test "C100000001C20000000100000000C44008CCCCCCCCCCCDC440107E6B74DCE58DC6C7040102ABCD4474657374"
         (cast string (o'toBinary))
      )
   )
)

(for-each
   (lambda (args)
      (let* ((val (args 0))
             (s (args 1))
             (o (oid val))
           )
         (test s (oid->string o))
         (test o (string->oid s))
      )
   )
   '(
      #(1 "C100000001")
      #(0x100000004 "C20000000100000004")
      #(-0x100000004 "C2FFFFFFFEFFFFFFFC")
      #(1.3e-45 "C4369DAFCEFAC80ABD")
      #(#m1980-01-01T00:00:00 "C50000004977387000")
      #(#f "C6")
      #(#t "C7")
      #(#z0102ABCD "040102ABCD")
      #("Test" "4454657374")
   )
)

(let ((o (oid 1 4294967296L 1.3e-45 #m1980-01-01T00:00:00 #f #t #z0102ABCD "Test"))
      (s "C100000001C20000000100000000C4369DAFCEFAC80ABDC50000004977387000C6C7040102ABCD4454657374")
     )
   (test s (oid->string o))
   (test o (string->oid s))
)

(when-full-runtime
   (test "C340490E56" (oid->string (oid 3.1415F)))
   (test (oid 3.1415F) (string->oid "C340490E56"))

   (test "C20000000000000001" (oid->string (oid 1L)))

   (test "810000000001" (oid->string (oid 1n)))
   (test (oid 1n) (string->oid "810000000001"))

   (test "820000000203E8" (oid->string (string->oid "820000000203E8")))
)

(when-limited-runtime
   (test "C340490E56" (oid->string (oid 3.1415F)))

   (test "C100000001" (oid->string (oid 1L)))
   (test (string->oid "C100000001") (string->oid "C20000000000000001"))

   (test "C100000001" (oid->string (oid 1n)))
   (test (string->oid "C100000001") (string->oid "810000000001"))

   (test (oid 10) (string->oid "820000000203E8"))
)

(test #t (= (oid 1 2) (oid 1 2)))
(test #f (!= (oid 1 2) (oid 1 2)))
(test #f (= (oid 1) (oid 1 2))) 
(test #t (!= (oid 1) (oid 1 2))) 

(test "a" (string-trim " a "))
(test "a" (string-affix "a" ", " ""))
(test "b" (string-affix '() ", " "b"))
(test "a, b" (string-affix "a" ", " "b"))
(test #t (string-empty? ""))
(test #t (string-empty? '()))
(test #f (string-empty? " "))
(test "a|b|c" (string-join col "|"))
(test "abc" (string-join col))
(test "a|b|c" (string-join (col'iterator) "|"))
(test "abc" (string-join (col'iterator)))
(test "a,b,c," (string-join #("a" "b" "c" ()) ","))
(test "abc" (string-join #("a" "b" "c" ())))
(test "a,b,c," (string-join '("a" "b" "c" ()) ","))
(test "abc" (string-join '("a" "b" "c" ())))
(test '("a" "b" "c") (string-split "a  b c" " +"))
(test '("a" "b" "c") (string-split "a  b c" (string-pattern "\\s+")))
(test '("Abba" "bb") (vector->list (string-match "Abba" "(?i)a(.*)a")))
(test '("abba" "bb") (vector->list (string-match "abba" (string-pattern "a(.*)a"))))
(test #f (string-match "Abba" "a(.*)b"))

(test "abccc" (string-replace "abCCC" "C" "c" #t))
(test "abcCC" (string-replace "abCCC" "C" "c" #f))
(test "abccc" (string-replace "abCCC" "C" "c"))

(test "ABC" (string-upcase "abc"))
(test "ABC" (string-upcase "ABC"))
(test "ABC" (string-upcase "aBc"))
(test '() (string-upcase '()))
(test "abc" (string-downcase "abc"))
(test "abc" (string-downcase "ABC"))
(test "abc" (string-downcase "AbC"))
(test '() (string-downcase '()))

(test #t (string=? '() '()))
(test #f (string=? '() ""))
(test #t (string-eol=? "\r\n" "\r"))
(test #f (string-eol=? '() "a"))
(test #t (string-ci=? '() '()))
(test #f (string-ci=? '() ""))
(test #t (string<? '() ""))
(test #f (string<? "" '()))
(test #t (string-ci<? '() ""))
(test #f (string-ci<? "" '()))
(test #t (string<=? '() ""))
(test #t (string<=? '() '()))
(test #f (string<=? "" '()))
(test #t (string-ci<=? '() ""))
(test #t (string-ci<=? '() '()))
(test #f (string-ci<=? "" '()))
(test #t (string>? "" '()))
(test #f (string>? '() ""))
(test #t (string-ci>? "" '()))
(test #f (string-ci>? '() ""))
(test #t (string>=? "" '()))
(test #t (string>=? '() '()))
(test #f (string>=? '() ""))
(test #t (string-ci>=? "" '()))
(test #t (string-ci>=? '() '()))
(test #f (string-ci>=? '() ""))

(call-with-values
   (lambda ()
      (define graph '((a b c) (b c) (c) (d a) (e f) (f e)))
      (topo-sort (map car graph) (lambda (item) (cdr (assq item graph))))
   )
   (lambda (topo circular)
      (test '(d a b c) topo)
      (test '(e f) circular)
   )
)

; logger
(sys:log '() 0 "a basic test" 'with '(some arguments))
(sys:log-localized '() 0 "ids.test" 'with '(some arguments)) 


; assertions
(when-full-runtime
   (test () (assert (= 1 1)))
   (test () (assert (= 1 1) "msg"))
   (test #t (try (assert (= 1 0)) (lambda (e) #t)))
   (test #t (try (assert (= 1 0) "msg") (lambda (e) #t)))
)

(when-full-runtime
   (import 'java.lang.String '(java.lang.Integer java.lang.Double java.lang.Character))
   (let ((s "abc"))
      (test 3 (s'length))
      (test 1 (s'indexOf #\b))
      (test 1 (s'indexOf "bc"))
      (test #\b (s'charAt 1))
      (test "ABC" (s'toUpperCase))
      (test "abcdef" (s'concat "def"))
      (test "" (java.lang.String'new))
      (test "abc" (java.lang.String'new "abc"))
      (test #t (java.lang.String'isAssignableFrom (s'class)))
   )

   (let ((n (java.lang.Integer'new "123")))
      (test 123 n)
      (test #f (java.lang.String'isAssignableFrom (n'class)))
   )

   (import 'nexj.core.util.HashTab)
   (let ((h (nexj.core.util.HashTab'new)))
      (h'put 'a 1)
      (h'put 'b 2)
      (test 2 (h'size))
      (test 2 (h'get 'b))
      (h'remove 'b)
      (let ((itr (h'iterator)))
         (test #t (itr'hasNext))
         (test 'a (itr'next))
         (test #f (itr'hasNext))
         (test 'a (itr'key))
         (test '1 (itr'value))
         (itr'value 3)
      )
      (test 3 (h'get 'a))
      (test #f ((nexj.core.util.HashTab'EMPTY_ITERATOR)'hasNext))
   )

   (import 'java.util.HashMap)
   (let* ((h (java.util.HashMap'new))
          (itr ((h'entrySet)'iterator))
         )
      (test #f (itr'hasNext))
   )

   (let ((c (instance-collection)))
      (test 0 (c'size))
      (test #t (c'empty))
      (c'add (Contact'new))
      (test 1 (c'size))
      (c'remove 0)
      (test 0 (c'size))
      (c'add (Contact'new))
      (c'remove (c'get 0))
      (test 0 (c'size))
   )
)

; ECA tests
(when-full-runtime
   (let ((u (User'new '(name . "testuser"))))
      (test #t (u'isNew))
      (test "testuser" (u'name))
      (test 6 (u'test 1 2 3))
      (test 19 (u'test 12 2 3))
      (test -12 (u'test -1 -2 -3))
      ;Hash for "secret"
      (test "K7gNU3sdo+OL0wNhqoVWhr3g6s1xYv72ol/pe/Unols=" ((u'password)'passwordHash))
      (test -5 (u'test -10 2 3))
      ;Hash for "password"
      (test "XohImNooBHFR0OVvjcYpJ3NgPQ1qq73WKhHvch0VQtg=" ((u'password)'passwordHash))
      (test 6.5 (u'test 1.5 2 3))
   )

   (let ((g (Group'new '(name . "testgroup"))))
      (test "testgroup" (g'name))
      (test 18 (g'test 1 2 3))
      (test 54 (g'test 2 2 3))
      (test -123 (g'test -1 2 3))
      (test 18.0 (g'test 1.4 2 3))
      (test Group (g':class))
      (test 'name (((g':class)'getAttribute 'name)'symbol))
   )
)

; Action groups tests
(when-full-runtime
   (test 'a (User'testActionGroups 0))
   (test 'b (User'testActionGroups 1))
   (test 'c (User'testActionGroups 2))
   (test '? (User'testActionGroups 3))
   (test 'c (User'testActionGroups -2))
)

; Calculated attribute tests
(when-full-runtime
   (test "Public" (Contact'defaultReadPrincipalName))
   (let ((c (Contact'new)))
      (test "" (c'fullName))
   )
   (let
      (
         (c (Contact'new '(firstName . "First") '(lastName . "Last")))
         (t (ContactType'new '(type . "Employee")))
         (u (User'new '(name . "jude")))
      )
      (test "First Last" (c'fullName))
      (c'firstName "Changed")
      (test "Changed Last" (c'fullName))
      (c'type t)
      (test "Changed Last [Employee]" (c'fullName))
      (t'type "Person")
      (test "Changed Last [Person]" (c'fullName))
      (test '() (c'uname))
      (test "U" (u'typeCode))
      (test '() ((c'user)'name))
      (c'user u)
      (u'contact c)
      (test "jude" ((c'user)'name))
      (test "Changed Last [Person] {jude}" (c'fullName))
      (test "jude" (c'uname))
      (u'name "skippy")
      (test "Changed Last [Person] {skippy}" (c'fullName))
      (test "skippy" (c'uname))
   )
   (let
      (
         (col
            (instance-collection
               (Contact'new (: firstName "a")
                  (: addresses
                     (instance-collection
                        (Address'new (: type "Business") (: city "rh"))
                        (Address'new (: type "Home") (: city "sf"))
                     )
                  )
               )
               (Contact'new (: firstName "b")
                  (: addresses (instance-collection (Address'new (: type "Business") (: city "to"))))
               )
            )
         )
      )
      (test 2 ((col 0)'addressCount))
      (test 1 ((col 1)'addressCount))
      (test 2 ((col'select '(firstName))':size))
      (test '("a" "b") ((col'select '(firstName))':list))
      (test 3 ((col'select '(addresses city))':size))
      (test '("rh" "sf" "to") ((col'select '(addresses city))':list))
      (test 3 (((col'select '(addresses))'city)':size))
      (test '("rh" "sf" "to") (((col'select '(addresses))'city)':list))
      (test '("rh" "to") (((col'select '(businessAddress4))'city)':list))
      (test '("rh" "to") (((col'select '(businessAddresses))'city)':list))
      (test '("sf") (let ((this (col 0))) ((@ addresses (= (@ type) "Home") city)':list)))
      ((((col 0)'addresses) 0)'delete)
      (test 1 ((col 0)'addressCount))
      (test 1 ((col 1)'addressCount))
      (test 2 ((col'select '(firstName))':size))
      (test '("a" "b") ((col'select '(firstName))':list))
      (test 2 ((col'select '(addresses city))':size))
      (test '("sf" "to") ((col'select '(addresses city))':list))
      (test 2 (((col'select '(addresses))'city)':size))
      (test '("sf" "to") (((col'select '(addresses))'city)':list))
      (test '(() "to") (((col'select '(businessAddress4))'city)':list))
      (test '("to") (((col'select '(businessAddresses))'city)':list))
   )
)


; Invocation context tests
(when-full-runtime
   (test "en_CA" (locale-name "en_CA_ON"))
)
(test "en_CA" (locale-name "en_CA"))
(test "en" (locale-name "en"))
(when-full-runtime
   (test "en" (locale-name '()))
)

; Query macros tests
(test #f (query?))
(test #f (query-root?))


; Test macro expansion with global symbols overridden by local symbols
(let
   ((and #f)
    (append #f)
    (cons #f)
    (equal? #f)
    (error #f)
    (let #f)
    (list #f)
    (list->vector #f)
    (member #f)
    (not #f)
    (null? #f)
    (or #f)
    (sys:cast #f)
    (sys:delay #f)
    (sys:try #f)
    (unquote #f)
    (unquote-splicing #f)

    (x 0)
    (y 0)
    (done? #f)
    (promise #f)
   )
   (test '1 (quasiquote 1))
   (test '(1 2 3 (4 5)) `(1 ,@`(2 3) ,`(4 5)))
   (test '(1 2 3 (4 5)) (quasiquote (1 (unquote-splicing '(2 3)) (unquote '(4 5)))))

   (test '() (cond))
   (test #t (cond ((< 1 2))))
   (test 'true (cond ((< 2 1)) ((< 1 2) 'true) (else 'false)))
   (test 'false (cond ((< 2 1)) ((< 3 2) 'true) (else 'false)))

   (test 1 (case "a" (("a" "b" "c") 1) (("d" "e" "f") 2) (else 3)))
   (test 'x^2 (case '(* x x) (((* x 1) (* 1 x)) 'x) (((* x x) (sqr x)) 'x^2)))

   (test '(1 1 2 4 3 9 4 16 5 25 6 36 7 49 8 64 9 81 10 100)
      (do ((i 10 (- i 1)) (lst '() (##cons i lst)))
          ((= i 0) lst)
          (set! lst (##cons (* i i) lst))
       )
   )

   (set! n 0)
   (set! x '())
   (test '() (while (< n 10) (set! n (+ n 1)) (set! x (##cons n x))))
   (test '(10 9 8 7 6 5 4 3 2 1) x)

   (set! x '())
   (test '() (for ((i 0)) (< i 10) (set! i (+ i 1)) (set! x (##cons i x))))
   (test '(9 8 7 6 5 4 3 2 1 0) x)

   (set! x 0)
   (test 2 (when (>= x 0) (set! x (+ x 1)) (set! x (+ x x))))
   (test 2 x)

   (set! x 0)
   (test 2 (unless (< x 0) (set! x (+ x 1)) (set! x (+ x x))))
   (test 2 x)

   (set! x 1)
   (set! promise (delay (/ x y)))
   (set! y 2)
   (test 0.5N (force promise))
   (set! y 1)
   (test 0.5N (force promise))

   (test 'mismatch (try
      (car #(0))
      (lambda (e) 'mismatch)
      (set! done? #t)
   ))

   (test #t (cast boolean "1"))

   (when-full-runtime
      (test #zFEED12 (cast binary "feed12"))
      (test "0" (cast string #f))

      (test '((this'addrs)'fullName) (expand-macro '(@ addrs fullName)))
      (test '((this'addrs)'fullName "New Name") (expand-macro '(@ addrs fullName : "New Name")))
   )

   (test '(a . 3) (: a (+ 1 2)))
)

; NexJ extensions

(define a (list 1 2 3))
(define b (list 4 5))
(define c (list 6))
(define d '(7 . 8))
(define e (list))

(test '() (append!))
(test 1 (append! 1))
(test '() (append! e '() '() '()))
(test '() e)
(test 1 (append! e '() '() '() 1))
(test '() e)

(test '(1 2 3 4 5) (append! a b))
(test '(1 2 3 4 5) a)
(test '(4 5) b)

(set! a (list 1 2 3))
(test '(1 2 3 4 5 6) (append! a e b c))
(test '(1 2 3 4 5 6) a)
(test '(4 5 6) b)
(test '(6) c)
(test '() e)

(set! a (list 1 2 3))
(set! b (list 4 5))
(set! c (list 6))
(test '(1 2 3 4 5 6) (append! e a '() b e c e))
(test '(1 2 3 4 5 6) a)
(test '(4 5 6) b)
(test '(6) c)
(test '() e)

(set! a (list 1 2 3))
(set! b (list 4 5))
(set! c (list 6))
(test '(1 2 3 4 5 6 7 . 8) (append! a b c e d))
(test '(1 2 3 4 5 6 7 . 8) a)
(test '(4 5 6 7 . 8) b)
(test '(6 7 . 8) c)
(test '(7 . 8) d)
(test '() e)


; R6RS Base Library
; 11.4.6. Binding constructs

(test '(1) (let-values (((a) 1)) (list a)))
(test '(1) (let-values (((a) (values 1))) (list a)))
(test '(1 2) (let-values (((a b) (values 1 2))) (list a b)))

(test '(1 2 3 (4 5) (6 7) (8) 9 (10))
   (let-values
      (((a b) (values 1 2))
       ((c . d) (values 3 4 5))
       (e (values 6 7))
       (f (values 8))
       ((g) 9)
       (h 10)
      )
      (list a b c d e f g h)
   )
)

(test '(3 4 1 2)
   (let-values
      (((a b c d) (values 1 2 3 4)))
      (let-values
         (((a b) (values c d))
          ((c d) (values a b)))
         (list a b c d)
      )
   )
)

(test '(1) (let*-values (((a) 1)) (list a)))
(test '(1) (let*-values (((a) (values 1))) (list a)))
(test '(1 2) (let*-values (((a b) (values 1 2))) (list a b)))

(test '(1 2 3 (4 5) (6 7) (8) 9 (10))
   (let*-values
      (((a b) (values 1 2))
       ((c . d) (values (+ a b) 4 5))
       (e (values 6 7))
       (f (values 8))
       ((g) (* c 3))
       (h 10)
      )
      (list a b c d e f g h)
   )
)

(test '(3 4 3 4)
   (let-values
      (((a b c d) (values 1 2 3 4)))
      (let*-values
         (((a b) (values c d))
          ((c d) (values a b)))
         (list a b c d)
      )
   )
)

; 11.7.3.1 Integer division

(map
   (lambda (args)
      (let*-values
         (
            ((dividend divisor div-result div0-result mod-result mod0-result) (apply values args))
            ((dividend-decimal) (cast decimal dividend))
         )
         (test div-result  (div  dividend divisor))
         (test div-result  (div  dividend-decimal divisor))
         (test div0-result (div0 dividend divisor))
         (test div0-result (div0 dividend-decimal divisor))

         (test mod-result  (mod  dividend divisor))
         (test mod-result  (mod  dividend-decimal divisor))
         (test mod0-result (mod0 dividend divisor))
         (test mod0-result (mod0 dividend-decimal divisor))

         (call-with-values
            (lambda () (div-and-mod dividend divisor))
            (lambda (d m)
               (test div-result d)
               (test mod-result m)
            )
         )

         (call-with-values
            (lambda () (div-and-mod dividend-decimal divisor))
            (lambda (d m)
               (test div-result d)
               (test mod-result m)
            )
         )

         (call-with-values
            (lambda () (div0-and-mod0 dividend divisor))
            (lambda (d m)
               (test div0-result d)
               (test mod0-result m)
            )
         )

         (call-with-values
            (lambda () (div0-and-mod0 dividend-decimal divisor))
            (lambda (d m)
               (test div0-result d)
               (test mod0-result m)
            )
         )
      )
   )
   '(
      ;(dividend divisor div-result div0-result mod-result mod0-result)
      ( 120  10   12  12   0  0)
      ( 120 -10  -12 -12   0  0)
      (-120  10  -12 -12   0  0)
      (-120 -10   12  12   0  0)
      ( 124  10   12  12   4  4)
      ( 124 -10  -12 -12   4  4)
      (-124  10  -13 -12   6 -4)
      (-124 -10   13  12   6 -4)
      ( 125  10   12  13   5 -5)
      ( 125 -10  -12 -13   5 -5)
      (-125  10  -13 -12   5 -5)
      (-125 -10   13  12   5 -5)
      ( 126  10   12  13   6 -4)
      ( 126 -10  -12 -13   6 -4)
      (-126  10  -13 -13   4  4)
      (-126 -10   13  13   4  4)

      (  90   9   10  10   0  0)
      (  90  -9  -10 -10   0  0)
      ( -90   9  -10 -10   0  0)
      ( -90  -9   10  10   0  0)
      (  93   9   10  10   3  3)
      (  93  -9  -10 -10   3  3)
      ( -93   9  -11 -10   6 -3)
      ( -93  -9   11  10   6 -3)
      (  94   9   10  10   4  4)
      (  94  -9  -10 -10   4  4)
      ( -94   9  -11 -10   5 -4)
      ( -94  -9   11  10   5 -4)
      (  95   9   10  11   5 -4)
      (  95  -9  -10 -11   5 -4)
      ( -95   9  -11 -11   4  4)
      ( -95  -9   11  11   4  4)
   )
)

; 11.7.4 Numerical operations

(test +inf.0 (/ 1.0 0.0))
(test -inf.0 (/ -1.0 0.0))

(test #f (finite? ()))
(test #t (finite? 2))
(test #t (finite? 2.0))
(test #t (finite? (expt 2 10)))
(test #f (finite? +inf.0))
(test #f (finite? -inf.0))
(test #f (finite? +nan.0))

(test #f (infinite? ()))
(test #f (infinite? 2))
(test #f (infinite? 2.0))
(test #f (infinite? (expt 2 10)))
(test #t (infinite? +inf.0))
(test #t (infinite? -inf.0))
(test #f (infinite? +nan.0))

(test #f (nan? ()))
(test #f (nan? 2))
(test #f (nan? 2.0))
(test #f (nan? (expt 2 10)))
(test #f (nan? +inf.0))
(test #f (nan? -inf.0))
(test #t (nan? +nan.0))

(let-values
   (((x y) (exact-integer-sqrt 121)))
   (test 11 x)
   (test 0 y)
)

(let-values
   (((x y) (exact-integer-sqrt 7.89)))
   (test 2 x)
   (test 3 y)
)

(let-values
   (((x y) (exact-integer-sqrt 29N)))
   (test 5 x)
   (test 4 y)
)

(let-values
   (((x y) (exact-integer-sqrt 0)))
   (test 0 x)
   (test 0 y)
)

(let-values
   (((x y) (exact-integer-sqrt '())))
   (test '() x)
   (test '() y)
)

; 11.8 Booleans
(test #t (boolean=? #t #t))
(test #t (boolean=? 1 'a))
(test #t (boolean=? #f #f))
(test #t (boolean=? #t 'a (= 1 1) (< 1 2)))
(test #t (boolean=? #f #f (= 1 2) (< 1 1)))
(test #f (boolean=? #t 'a (= 1 1) (= 1 2)))
(test #f (boolean=? #f #f (= 1 2) (< 1 2)))

; 11.10 Symbols
(test #t (symbol=? 'a 'a))
(test #t (symbol=? 'a 'a (string->symbol "a") 'a))
(test #f (symbol=? 'a 'b))
(test #f (symbol=? 'a 'a 'a 'b 1))

; 11.13 Vectors

(define test-vector '#())
(test '#() (vector-fill! test-vector 0))
(test '#() test-vector)

(define test-vector '#(1 2 3 4 5))
(test '#(0 0 0 0 0) (vector-fill! test-vector 0))
(test '#(0 0 0 0 0) test-vector)

(define test-col (collection))
(test (collection) (vector-fill! test-col 4))
(test (collection) test-col)

(define test-col (collection 1 2 3 4))
(test (collection 4 4 4 4) (vector-fill! test-col 4))
(test (collection 4 4 4 4) test-col)

(test #() (vector-map + #()))
(test #(1 2) (vector-map + #(1 2)))
(test #(4 6) (vector-map + #(1 2) #(3 4)))
(test #(12 15 18) (vector-map + #(1 2 3) #(4 5 6) #(7 8 9) #(0 0 0)))

(test #() (vector-map list #()))
(test (vector '(1) '(2)) (vector-map list #(1 2)))
(test (vector '(1 4 7 0) '(2 5 8 0) '(3 6 9 0)) (vector-map list #(1 2 3) #(4 5 6) #(7 8 9) #(0 0 0)))

; 11.15 Control features

(test 1 (values 1 2 3))
(test 6 (* 2 (values 3 4 5)))
(test 1 (begin (values 2 3 4) 1))
(test -1 (- (values 1 2)))

(test 11 (call-with-values (lambda () (values 10 11)) (lambda (x y) y)))
(test 7 (call-with-values (lambda () 7) (lambda (x) x)))
(test 6 (call-with-values (lambda () (values 1 2 3)) +))
(test -1 (call-with-values * -))
(test '(-1) (call-with-values (lambda () (- (values 1 2))) (lambda args args)))
(test '(-9) (call-with-values (lambda () (- (values 1 2) 10)) (lambda args args)))
(test '(9) (call-with-values (lambda () (- 10 (values 1 2))) (lambda args args)))

; multiple-argument continuation inside call-with-values
(define cont ())
(define result #f)

(set! result
   (call-with-values
      (lambda ()
         (call/cc
            (lambda (c)
               (set! cont c)
               (values 1 2 3 4)
            )
         )
      )
      (lambda args (apply + 1000 args))
   )
)
(test 1010 result)

(cont 20 30 40 10 50)
(test 1150 result)

(cont 101)
(test 1101 result)

(cont (values 200 300))
(test 1200 result)

(cont)
(test 1000 result)

(set! result
   (-
      (call-with-values
         (lambda () (cont 11 22 33))
         (lambda args 'wrong)
      )
   )
)
(test 1066 result)

(set! result
   (- 
      (call-with-values
         (lambda () (cont 111))
         (lambda args 'wrong)
      )
   )
)
(test 1111 result)

(set! result
   (-
      (call-with-values
         (lambda () (cont))
         (lambda args 'wrong)
      )
   )
)
(test 1000 result)

(set! result
   (call-with-values
      (lambda () (cont (values 200 300 500)))
      (lambda args 'wrong)
   )
)
(test 1200 result)

; multiple-argument continuation without call-with-values
(set! result
   (+ 1
      (call/cc
         (lambda (c)
            (set! cont c)
            (values 10 20 30)
         )
      )
   )
)
(test 11 result)

(cont 20 30 40 10 50)
(test 21 result)

(cont 100)
(test 101 result)

(cont (values 200 300))
(test 201 result)

; continuation called within call-with-values block
(set! result
   (call-with-values
      (lambda () (values (cont 100) 200))
      (lambda args (apply + 10 args))
   )
)
(test 101 result)

(set! result
   (call-with-values
      (lambda () (cont 1000))
      (lambda args (apply + 10 args))
   )
)
(test 1001 result)

; NexJ extension: values->list
(test '() (values->list (values)))
(test '(1) (values->list (values 1)))
(test '(1) (values->list 1))
(test '(1 2 (3)) (values->list (values 1 2 '(3))))

(set! result
   (values->list
      (call/cc
         (lambda (c)
            (set! cont c)
            (values 1 2 3 4)
         )
      )
   )
)
(test '(1 2 3 4) result)

(cont 20 30 40 10 50)
(test '(20 30 40 10 50) result)

(cont 101)
(test '(101) result)

(cont (values 200 300))
(test '(200) result)

(cont)
(test '() result)

; Simple dynamic-wind without call/cc
(let ((n 0))
   (test
      '(1 2 3)
      (values->list
         (dynamic-wind
            (lambda () (set! n (+ n 1)))
            (lambda () (set! n (+ n 10)) (values 1 2 3))
            (lambda () (set! n (+ n 100)))
         )
      )
   )
   (test 111 n)
)

; dynamic-wind with call/cc
(define cont '())
(define ls '())
(define result #f)

(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in1 ls)))
      (lambda ()
         (set! ls (cons 'main1 ls))
         (+ 1 (call/cc (lambda (c) (set! cont c) 10)))
      )
      (lambda () (set! ls (cons 'out1 ls)))
   )
)
(test 11 result)
(test '(in1 main1 out1) (reverse ls))

(set! ls '())
(cont 100)
(test 101 result)
(test '(in1 out1) (reverse ls))

(set! ls '())
(cont 200 300)
(test 201 result)
(test '(in1 out1) (reverse ls))

(set! ls '())
(cont (values 400 500))
(test 401 result)
(test '(in1 out1) (reverse ls))

; Nested dynamic-wind
(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in2 ls)))
      (lambda () (set! ls (cons 'main2 ls)) (cont 1000))
      (lambda () (set! ls (cons 'out2 ls)))
   )
)
(test 1001 result)
(test '(in2 main2 out2 in1 out1) (reverse ls))

(define cont2 '())
(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in1 ls)))
      (lambda ()
         (set! ls (cons 'main1 ls))
         (dynamic-wind
            (lambda () (set! ls (cons 'in2 ls)))
            (lambda ()
               (set! ls (cons 'main2 ls))
               (+ 1 (call/cc (lambda (c) (set! cont2 c) 10)))
            )
            (lambda () (set! ls (cons 'out2 ls)))
         )
      )
      (lambda () (set! ls (cons 'out1 ls)))
   )
)
(test 11 result)
(test '(in1 main1 in2 main2 out2 out1) (reverse ls))

(set! ls '())
(cont2 100)
(test 101 result)
(test '(in1 in2 out2 out1) (reverse ls))

(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in3 ls)))
      (lambda ()
         (set! ls (cons 'main3 ls))
         (dynamic-wind
            (lambda () (set! ls (cons 'in2 ls)))
            (lambda ()
               (set! ls (cons 'main2 ls))
               (cont 10000)
            )
            (lambda () (set! ls (cons 'out2 ls)))
         )
      )
      (lambda () (set! ls (cons 'out3 ls)))
   )
)
(test 10001 result)
(test '(in3 main3 in2 main2 out2 out3 in1 out1) (reverse ls))

; Multiple-return-value dynamic-wind
(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in ls)))
      (lambda ()
         (let-values
            (
               (args
                  (call/cc
                     (lambda (k) (set! cont k) (values 1 2 3 4))
                  )
               )
            )
            (set! ls (cons (apply + args) ls))
            args
         )
      )
      (lambda () (set! ls (cons 'out ls))) 
   )
)
(test '(1 2 3 4) result)
(test '(in 10 out) (reverse ls))

(set! ls '())
(cont 10 20 30)
(test '(10 20 30) result)
(test '(in 60 out) (reverse ls))

(set! ls '())
(cont 101)
(test '(101) result)
(test '(in 101 out) (reverse ls))

(set! ls '())
(cont (values 10 20 30))
(test '(10) result)
(test '(in 10 out) (reverse ls))

(set! ls '())
(cont)
(test '() result)
(test '(in 0 out) (reverse ls))

; Nested, multiple-return-value dynamic-wind
(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in2 ls)))
      (lambda ()
         (set! ls (cons 'main2 ls))
         (let-values
            ((args (call/cc (lambda (c) (set! cont2 c) 123))))
            (apply cont args)
         )
      )
      (lambda () (set! ls (cons 'out2 ls))) 
   )
)
(test '(123) result)
(test '(in2 main2 out2 in 123 out) (reverse ls))

(set! ls '())
(cont2 100 200 300)
(test '(100 200 300) result)
(test '(in2 out2 in 600 out) (reverse ls))

(set! ls '())
(cont2 (values 123 456))
(test '(123) result)
(test '(in2 out2 in 123 out) (reverse ls))

(set! ls '())
(cont2)
(test '() result)
(test '(in2 out2 in 0 out) (reverse ls))

(set! ls '())
(set! result
   (call-with-values
      (lambda () (cont2 (values 12 34 56)))
      (lambda args args)
   )
)
(test '(12) result)
(test '(in2 out2 in 12 out) (reverse ls))

(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in1 ls)))
      (lambda ()
         (set! ls (cons 'main1 ls))
         (dynamic-wind
            (lambda () (set! ls (cons 'in2 ls)))
            (lambda ()
               (let-values
                  (
                     (args
                        (call/cc
                           (lambda (k) (set! cont k) (values 1 2 3 4))
                        )
                     )
                  )
                  (set! ls (cons (apply + args) ls))
                  args
               )
            )
            (lambda () (set! ls (cons 'out2 ls))) 
         )
      )
      (lambda () (set! ls (cons 'out1 ls))) 
   )
)
(test '(1 2 3 4) result)
(test '(in1 main1 in2 10 out2 out1) (reverse ls))

(set! ls '())
(cont 10 20 30)
(test '(10 20 30) result)
(test '(in1 in2 60 out2 out1) (reverse ls))

(set! ls '())
(cont 101)
(test '(101) result)
(test '(in1 in2 101 out2 out1) (reverse ls))

(set! ls '())
(cont (values 10 20 30))
(test '(10) result)
(test '(in1 in2 10 out2 out1) (reverse ls))

(set! ls '())
(cont)
(test '() result)
(test '(in1 in2 0 out2 out1) (reverse ls))

(set! ls '())
(set! result
   (call-with-values
      (lambda () (cont (values 12 34 56)))
      (lambda args args)
   )
)
(test '(12) result)
(test '(in1 in2 12 out2 out1) (reverse ls))

; dynamic-wind and try
(set! ls '())
(set! result 0)
(test 'error
   (try
      (set! result
         (dynamic-wind
            (lambda () (set! ls (cons 'in ls)))
            (lambda ()
               (set! ls (cons 'main ls))
               (set! result (+ 1 result))
               (error "err")
               (set! result (+ 10 result))
            )
            (lambda () (set! ls (cons 'out ls)))
         )
      )
      (lambda (e) (set! ls (cons 'handler ls)) 'error)
      (set! result (+ 100 result))
      (set! ls (cons 'finalizer ls))
   )
)
(test 101 result)
(test '(in main out handler finalizer) (reverse ls))

; dynamic-wind with continuation inside try
(set! ls '())
(set! result 0)
(try
   (set! result
      (dynamic-wind
         (lambda () (set! ls (cons 'in ls)))
         (lambda ()
            (set! ls (cons 'main ls))
            (let ((v (call/cc (lambda (c) (set! cont c) 10))))
               (if
                  (equal? 'error v)
                  (error v)
                  (+ 1 v)
               )
            )
         )
         (lambda () (set! ls (cons 'out ls)))
      )
   )
   (lambda (e) (set! ls (cons 'handler ls)))
   (set! result (+ 100 result))
   (set! ls (cons 'finalizer ls))
)
(test 111 result)
(test '(in main out finalizer) (reverse ls))

(set! ls '())
(cont 20)
(test 121 result)
(test '(in out finalizer) (reverse ls))

(set! ls '())
(set! result 0)
(cont 'error)
(test 100 result)
(test '(in out handler finalizer) (reverse ls))

; dynamic-wind with continuation, with embedded try
(set! ls '())
(set! result
   (dynamic-wind
      (lambda () (set! ls (cons 'in ls)))
      (lambda ()
         (set! ls (cons 'main ls))
         (try
            (set! result
               (let ((v (call/cc (lambda (c) (set! cont c) 10))))
                  (if
                     (equal? 'error v)
                     (error v)
                     (+ 1 v)
                  )
               )
            )
            (lambda (e) (set! ls (cons 'handler ls)) 'exception)
            (set! result (+ 100 result))
            (set! ls (cons* result 'finalizer ls))
         )
      )
      (lambda () (set! ls (cons 'out ls)))
   )
)
(test 11 result)
(test '(in main finalizer 111 out) (reverse ls))

(set! ls '())
(cont 20)
(test 21 result)
(test '(in finalizer 121 out) (reverse ls))

(set! ls '())
(set! result 0)
(cont 'error)
(test 'exception result)
(test '(in handler finalizer 100 out) (reverse ls))

; R6RS Standard Libraries #2. Bytevectors

(test #t (symbol? (endianness big)))
(test 'big (endianness big))
(test 'little (endianness little))
(test #t (symbol? (native-endianness)))
(test 'big (native-endianness))

(test #f (bytevector? (vector 1 2 3)))
(test #f (bytevector? '(1 2 3)))
(test #f (bytevector? 'symbol))
(test #t (bytevector? (bytevector)))
(test #t (bytevector? (bytevector-append)))
(test #t (bytevector? #vu8()))
(test #t (bytevector? #vu8(0 1 2 255)))

(test #vu8() (bytevector))
(test #vu8(128) (bytevector -128))
(test #vu8(1 128) (bytevector 1 -128))
(test #vu8(1 128 255) (bytevector 1 -128 -1))
(test #vu8(255 254 128 129) (bytevector -1 -2 -128 -127))
(test #t (bytevector=? #vu8(255 254 128 129 3) (bytevector -1 -2 -128 -127 3)))
(test #f (bytevector=? #vu8(255 254 128 129 3) (bytevector -1 -2 -128 -127 3 123)))
(test #f (bytevector=? #vu8(255 254 128 129 3) (bytevector -1 -2 -128 -12 3)))
(test #vu8(1 2 3 128 255 0 255 254 128 129) (bytevector 1 2 3 128 255 0 -1 -2 -128 -127))

(test #vu8() (bytevector-append))
(test #vu8(128) (bytevector-append #vu8(128)))
(test #vu8(1 2 3 128 255 0 255 254 128 129)
   (bytevector-append #vu8(1 2 3) #vu8(128) #vu8() #vu8(255 0) (bytevector) (bytevector -1 -2 -128) (bytevector -127)))

(define bvector (make-bytevector 10))
(test #t (bytevector? bvector))
(test 10 (bytevector-length bvector))
(test #t (bytevector=? bvector (make-bytevector 10)))
(test #f (bytevector=? bvector (make-bytevector 5)))

(test 0 (bvector 0 0))
(test 1 (bvector 1 1.0))
(test 1 (bvector 1))
(test 255 (bvector 2 0xFF))
(test 255 (bvector 2))
(test 254 (bvector 3 0xFFFE))
(test 254 (bvector 3))
(test 253 (bvector 8 -3))
(test 253 (bvector 8))
(test 200 (bvector 9 2e2))
(test 200 (bvector 9))
(test #vu8(0 1 255 254 0 0 0 0 253 200) bvector)

(test #t (bytevector=? (make-bytevector 10 255) (bytevector-fill! bvector 255)))
(define bcopy (bytevector-copy bvector))
(test #t (bytevector=? bvector bcopy))

(set! bvector (uint-list->bytevector '(0x01234567 0x89ABCDEF 0xFFFFFFFF 0x80000000) 'big 4))
(test 16 (bytevector-length bvector))
(test #vu8(0x01 0x23 0x45 0x67 0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF 0xFF 0xFF 0xFF 0xFF)
   (bytevector-copy! bvector 0 bvector 4 12))
(test #vu8(0x01 0x23 0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF 0xCD 0xEF 0xFF 0xFF 0xFF 0xFF)
   (bytevector-copy! bvector 4 bvector 2 8))

(set! bvector (make-bytevector 3 -127))
(test #vu8(-127 129 0xABC81) bvector)
(test #vu8(127 0x567F -0x6781) (bytevector-fill! bvector 127))
(test #vu8(127 0x567F -0x6781) bvector)
(test #vu8(-120 136 0x8888) (bytevector-fill! bvector 136))
(test #vu8(-120 136 0x8888) bvector)

(set! bvector #vu8(0 0 0 0 0))
(for-each
   (lambda (i setval uref sref)
      (test setval (bytevector-u8-set! bvector i setval))
      (test uref (bytevector-u8-ref bvector i))
      (test sref (bytevector-s8-ref bvector i))
      (test uref (bytevector-uint-ref bvector i 'big 1))
      (test sref (bytevector-sint-ref bvector i 'big 1))
      (test uref (bytevector-uint-ref bvector i 'little 1))
      (test sref (bytevector-sint-ref bvector i 'little 1))

      (test setval (bytevector-s8-set! bvector i setval))
      (test uref (bytevector-u8-ref bvector i))
      (test sref (bytevector-s8-ref bvector i))
      (test uref (bytevector-uint-ref bvector i 'big 1))
      (test sref (bytevector-sint-ref bvector i 'big 1))
      (test uref (bytevector-uint-ref bvector i 'little 1))
      (test sref (bytevector-sint-ref bvector i 'little 1))

      (test setval (bytevector-uint-set! bvector i setval 'big 1))
      (test uref (bytevector-u8-ref bvector i))
      (test sref (bytevector-s8-ref bvector i))
      (test uref (bytevector-uint-ref bvector i 'big 1))
      (test sref (bytevector-sint-ref bvector i 'big 1))
      (test uref (bytevector-uint-ref bvector i 'little 1))
      (test sref (bytevector-sint-ref bvector i 'little 1))

      (test setval (bytevector-sint-set! bvector i setval 'big 1))
      (test uref (bytevector-u8-ref bvector i))
      (test sref (bytevector-s8-ref bvector i))
      (test uref (bytevector-uint-ref bvector i 'big 1))
      (test sref (bytevector-sint-ref bvector i 'big 1))
      (test uref (bytevector-uint-ref bvector i 'little 1))
      (test sref (bytevector-sint-ref bvector i 'little 1))

      (test setval (bytevector-uint-set! bvector i setval 'little 1))
      (test uref (bytevector-u8-ref bvector i))
      (test sref (bytevector-s8-ref bvector i))
      (test uref (bytevector-uint-ref bvector i 'big 1))
      (test sref (bytevector-sint-ref bvector i 'big 1))
      (test uref (bytevector-uint-ref bvector i 'little 1))
      (test sref (bytevector-sint-ref bvector i 'little 1))

      (test setval (bytevector-sint-set! bvector i setval 'little 1))
      (test uref (bytevector-u8-ref bvector i))
      (test sref (bytevector-s8-ref bvector i))
      (test uref (bytevector-uint-ref bvector i 'big 1))
      (test sref (bytevector-sint-ref bvector i 'big 1))
      (test uref (bytevector-uint-ref bvector i 'little 1))
      (test sref (bytevector-sint-ref bvector i 'little 1))
   )
   #(0 1 2.2 3.9 4 0)
   #(-1  254 127 -128  0xABCDEF 0x123456)
   #(255 254 127  128  0xEF     0x56)
   #(-1   -2 127 -128 -17       0x56)
)

(test '(0x56 254 127 128 0xEF) (bytevector->u8-list bvector))
(test '(0x56 254 127 128 0xEF) (bytevector->uint-list bvector 'big 1))
(test '(0x56 -2 127 -128 -0x11) (bytevector->sint-list bvector 'big 1))
(test '(0x56 254 127 128 0xEF) (bytevector->uint-list bvector 'little 1))
(test '(0x56 -2 127 -128 -0x11) (bytevector->sint-list bvector 'little 1))

(test bvector (u8-list->bytevector '(0x123456 -2 0xFF7F 0x6780 -0x3211)))
(test bvector (uint-list->bytevector '(0x123456 -2 0xFF7F 0x6780 -0x3211) 'big 1))
(test bvector (sint-list->bytevector '(0x123456 -2 0xFF7F 0x6780 -0x3211) 'big 1))
(test bvector (uint-list->bytevector '(0x123456 -2 0xFF7F 0x6780 -0x3211) 'little 1))
(test bvector (sint-list->bytevector '(0x123456 -2 0xFF7F 0x6780 -0x3211) 'little 1))


(set! bvector #vu8(0 0 0 0 0 0 0 0 0 0))
(for-each
   (lambda (i setval uref-be sref-be uref-le sref-le)
      (test setval (bytevector-u16-set! bvector i setval 'big))
      (test uref-be (bytevector-u16-native-ref bvector i))
      (test sref-be (bytevector-s16-native-ref bvector i))
      (test uref-be (bytevector-u16-ref bvector i 'big))
      (test sref-be (bytevector-s16-ref bvector i 'big))
      (test uref-le (bytevector-u16-ref bvector i 'little))
      (test sref-le (bytevector-s16-ref bvector i 'little))

      (test setval (bytevector-s16-set! bvector i setval 'big))
      (test uref-be (bytevector-u16-native-ref bvector i))
      (test sref-be (bytevector-s16-native-ref bvector i))
      (test uref-be (bytevector-u16-ref bvector i 'big))
      (test sref-be (bytevector-s16-ref bvector i 'big))
      (test uref-le (bytevector-u16-ref bvector i 'little))
      (test sref-le (bytevector-s16-ref bvector i 'little))

      (test setval (bytevector-u16-native-set! bvector i setval))
      (test uref-be (bytevector-u16-native-ref bvector i))
      (test sref-be (bytevector-s16-native-ref bvector i))
      (test uref-be (bytevector-u16-ref bvector i 'big))
      (test sref-be (bytevector-s16-ref bvector i 'big))
      (test uref-le (bytevector-u16-ref bvector i 'little))
      (test sref-le (bytevector-s16-ref bvector i 'little))

      (test setval (bytevector-s16-native-set! bvector i setval))
      (test uref-be (bytevector-u16-native-ref bvector i))
      (test sref-be (bytevector-s16-native-ref bvector i))
      (test uref-be (bytevector-u16-ref bvector i 'big))
      (test sref-be (bytevector-s16-ref bvector i 'big))
      (test uref-le (bytevector-u16-ref bvector i 'little))
      (test sref-le (bytevector-s16-ref bvector i 'little))

      (test setval (bytevector-u16-set! bvector i setval 'little))
      (test uref-le (bytevector-u16-native-ref bvector i))
      (test sref-le (bytevector-s16-native-ref bvector i))
      (test uref-le (bytevector-u16-ref bvector i 'big))
      (test sref-le (bytevector-s16-ref bvector i 'big))
      (test uref-be (bytevector-u16-ref bvector i 'little))
      (test sref-be (bytevector-s16-ref bvector i 'little))

      (test setval (bytevector-s16-set! bvector i setval 'little))
      (test uref-le (bytevector-u16-native-ref bvector i))
      (test sref-le (bytevector-s16-native-ref bvector i))
      (test uref-le (bytevector-u16-ref bvector i 'big))
      (test sref-le (bytevector-s16-ref bvector i 'big))
      (test uref-be (bytevector-u16-ref bvector i 'little))
      (test sref-be (bytevector-s16-ref bvector i 'little))
   )
   #(0 2 4.2 6.9 8 0)
   #(-1      0xFFFE  0x7FFF -0x8000 -0x543211 0x123456)
   #(0xFFFF  0xFFFE  0x7FFF  0x8000  0xCDEF   0x3456)
   #(-1     -2       0x7FFF -0x8000 -0x3211   0x3456)
   #(0xFFFF  0xFEFF  0xFF7F  0x0080  0xEFCD   0x5634)
   #(-1     -257    -0x0081  0x0080 -0x1033   0x5634)
)

(test '(0x56 0x34 0xFE 0xFF 0xFF 0x7F 0 0x80 0xEF 0xCD) (bytevector->u8-list bvector))
(test '(0x5634 0xFEFF 0xFF7F 0x0080 0xEFCD) (bytevector->uint-list bvector 'big 2))
(test '(0x5634 -0x0101 -0x0081 0x0080 -0x1033) (bytevector->sint-list bvector 'big 2))
(test '(0x3456 0xFFFE 0x7FFF 0x8000 0xCDEF) (bytevector->uint-list bvector 'little 2))
(test '(0x3456 -2 0x7FFF -0x8000 -0x3211) (bytevector->sint-list bvector 'little 2))

(test bvector (u8-list->bytevector '(0x123456 0x34 0xFE 0xFF 0xFF 0x7F 0 0x80 0xEF 0xCD)))
(test bvector (uint-list->bytevector '(0x785634 -0x010101 0xFF7F 0x10080 -0x12341033) 'big 2))
(test bvector (sint-list->bytevector '(0x785634 -0x010101 0xFF7F 0x10080 -0x12341033) 'big 2))
(test bvector (uint-list->bytevector '(0x123456 -2 0x07FFF 0x18000 -0x543211) 'little 2))
(test bvector (sint-list->bytevector '(0x123456 -2 0x07FFF 0x18000 -0x543211) 'little 2))


(set! bvector (make-bytevector 12 0))
(for-each
   (lambda (i setval uref-be sref-be uref-le sref-le)
      (test setval (bytevector-uint-set! bvector i setval 'big 3))
      (test uref-be (bytevector-uint-ref bvector i 'big 3))
      (test sref-be (bytevector-sint-ref bvector i 'big 3))
      (test uref-le (bytevector-uint-ref bvector i 'little 3))
      (test sref-le (bytevector-sint-ref bvector i 'little 3))

      (test setval (bytevector-sint-set! bvector i setval 'big 3))
      (test uref-be (bytevector-uint-ref bvector i 'big 3))
      (test sref-be (bytevector-sint-ref bvector i 'big 3))
      (test uref-le (bytevector-uint-ref bvector i 'little 3))
      (test sref-le (bytevector-sint-ref bvector i 'little 3))

      (test setval (bytevector-uint-set! bvector i setval 'little 3))
      (test uref-le (bytevector-uint-ref bvector i 'big 3))
      (test sref-le (bytevector-sint-ref bvector i 'big 3))
      (test uref-be (bytevector-uint-ref bvector i 'little 3))
      (test sref-be (bytevector-sint-ref bvector i 'little 3))

      (test setval (bytevector-sint-set! bvector i setval 'little 3))
      (test uref-le (bytevector-uint-ref bvector i 'big 3))
      (test sref-le (bytevector-sint-ref bvector i 'big 3))
      (test uref-be (bytevector-uint-ref bvector i 'little 3))
      (test sref-be (bytevector-sint-ref bvector i 'little 3))
   )
   #(0 3 5.2 9.9 0 3)
   #(-1        0xFFFFFE  0x7FFFFF -0x800000 -0x543210543211  0x123456)
   #(0xFFFFFF  0xFFFFFE  0x7FFFFF  0x800000  0xABCDEF        0x123456)
   #(-1       -2         0x7FFFFF -0x800000 -0x543211        0x123456)
   #(0xFFFFFF  0xFEFFFF  0xFFFF7F  0x000080  0xEFCDAB        0x563412)
   #(-1       -65537    -0x000081  0x000080 -0x103255        0x563412)
)

(test '(0xEF 0xCD 0xAB 0x56 0x34 0x12 0xFF 0x7F 0 0 0 0x80) (bytevector->u8-list bvector))
(test '(0xEFCDAB 0x563412 0xFF7F00 0x000080) (bytevector->uint-list bvector 'big 3))
(test '(-0x103255 0x563412 -0x008100 0x000080) (bytevector->sint-list bvector 'big 3))
(test '(0xABCDEF 0x123456 0x007FFF 0x800000) (bytevector->uint-list bvector 'little 3))
(test '(-0x543211 0x123456 0x007FFF -0x800000) (bytevector->sint-list bvector 'little 3))

(test bvector (u8-list->bytevector '(0xEF 0xCD 0xAB 0x56 0x34 0x12 0xFF 0x7F 0 0 0 0x80)))
(test bvector (uint-list->bytevector '(-0x32103255 0x78563412 -0x008100 0xFF000080) 'big 3))
(test bvector (sint-list->bytevector '(-0x32103255 0x78563412 -0x008100 0xFF000080) 'big 3))
(test bvector (uint-list->bytevector '(-0x76543211 0x12123456 0xFF007FFF -0x88800000) 'little 3))
(test bvector (sint-list->bytevector '(-0x76543211 0x12123456 0xFF007FFF -0x88800000) 'little 3))


(set! bvector (make-bytevector 16 0))
(for-each
   (lambda (i setval uref-be sref-be uref-le sref-le)
      (test setval (bytevector-u32-set! bvector i setval 'big))
      (test uref-be (bytevector-u32-native-ref bvector i))
      (test sref-be (bytevector-s32-native-ref bvector i))
      (test uref-be (bytevector-u32-ref bvector i 'big))
      (test sref-be (bytevector-s32-ref bvector i 'big))
      (test uref-le (bytevector-u32-ref bvector i 'little))
      (test sref-le (bytevector-s32-ref bvector i 'little))

      (test setval (bytevector-s32-set! bvector i setval 'big))
      (test uref-be (bytevector-u32-native-ref bvector i))
      (test sref-be (bytevector-s32-native-ref bvector i))
      (test uref-be (bytevector-u32-ref bvector i 'big))
      (test sref-be (bytevector-s32-ref bvector i 'big))
      (test uref-le (bytevector-u32-ref bvector i 'little))
      (test sref-le (bytevector-s32-ref bvector i 'little))

      (test setval (bytevector-u32-native-set! bvector i setval))
      (test uref-be (bytevector-u32-native-ref bvector i))
      (test sref-be (bytevector-s32-native-ref bvector i))
      (test uref-be (bytevector-u32-ref bvector i 'big))
      (test sref-be (bytevector-s32-ref bvector i 'big))
      (test uref-le (bytevector-u32-ref bvector i 'little))
      (test sref-le (bytevector-s32-ref bvector i 'little))

      (test setval (bytevector-s32-native-set! bvector i setval))
      (test uref-be (bytevector-u32-native-ref bvector i))
      (test sref-be (bytevector-s32-native-ref bvector i))
      (test uref-be (bytevector-u32-ref bvector i 'big))
      (test sref-be (bytevector-s32-ref bvector i 'big))
      (test uref-le (bytevector-u32-ref bvector i 'little))
      (test sref-le (bytevector-s32-ref bvector i 'little))

      (test setval (bytevector-u32-set! bvector i setval 'little))
      (test uref-le (bytevector-u32-native-ref bvector i))
      (test sref-le (bytevector-s32-native-ref bvector i))
      (test uref-le (bytevector-u32-ref bvector i 'big))
      (test sref-le (bytevector-s32-ref bvector i 'big))
      (test uref-be (bytevector-u32-ref bvector i 'little))
      (test sref-be (bytevector-s32-ref bvector i 'little))

      (test setval (bytevector-s32-set! bvector i setval 'little))
      (test uref-le (bytevector-u32-native-ref bvector i))
      (test sref-le (bytevector-s32-native-ref bvector i))
      (test uref-le (bytevector-u32-ref bvector i 'big))
      (test sref-le (bytevector-s32-ref bvector i 'big))
      (test uref-be (bytevector-u32-ref bvector i 'little))
      (test sref-be (bytevector-s32-ref bvector i 'little))
   )
   #(0 4 8.2 12.9 0 4)
   #(-1          0xFFFFFFFE  0x7FFFFFFF -0x80000000 -0x543210543211  0x1234567812)
   #(0xFFFFFFFF  0xFFFFFFFE  0x7FFFFFFF  0x80000000  0xEFABCDEF      0x34567812)
   #(-1         -2           0x7FFFFFFF -0x80000000 -0x10543211      0x34567812)
   #(0xFFFFFFFF  0xFEFFFFFF  0xFFFFFF7F  0x00000080  0xEFCDABEF      0x12785634)
   #(-1         -0x01000001 -0x00000081  0x00000080 -0x10325411      0x12785634)
)

(test '(0xEF 0xCD 0xAB 0xEF 0x12 0x78 0x56 0x34 0xFF 0xFF 0xFF 0x7F 0 0 0 0x80) (bytevector->u8-list bvector))
(test '(0xEFCDABEF 0x12785634 0xFFFFFF7F 0x00000080) (bytevector->uint-list bvector 'big 4))
(test '(-0x10325411 0x12785634 -0x00000081 0x00000080) (bytevector->sint-list bvector 'big 4))
(test '(0xEFABCDEF 0x34567812 0x7FFFFFFF 0x80000000) (bytevector->uint-list bvector 'little 4))
(test '(-0x10543211 0x34567812 0x7FFFFFFF -0x80000000) (bytevector->sint-list bvector 'little 4))

(test bvector (u8-list->bytevector '(0xEF 0xCD 0xAB 0xEF 0x12 0x78 0x56 0x34 0xFF 0xFF 0xFF 0x7F 0 0 0 0x80)))
(test bvector (uint-list->bytevector '(-0x3210325411 0xFF12785634 -0x00000081 0x12300000080) 'big 4))
(test bvector (sint-list->bytevector '(-0x3210325411 0xFF12785634 -0x00000081 0x12300000080) 'big 4))
(test bvector (uint-list->bytevector '(-0x3210543211 0xFF34567812 0x7FFFFFFF -0x8880000000) 'little 4))
(test bvector (sint-list->bytevector '(-0x3210543211 0xFF34567812 0x7FFFFFFF -0x8880000000) 'little 4))


(set! bvector (make-bytevector 15 0))
(for-each
   (lambda (i setval uref-be sref-be uref-le sref-le)
      (test setval (bytevector-uint-set! bvector i setval 'big 5))
      (test uref-be (bytevector-uint-ref bvector i 'big 5))
      (test sref-be (bytevector-sint-ref bvector i 'big 5))
      (test uref-le (bytevector-uint-ref bvector i 'little 5))
      (test sref-le (bytevector-sint-ref bvector i 'little 5))

      (test setval (bytevector-sint-set! bvector i setval 'big 5))
      (test uref-be (bytevector-uint-ref bvector i 'big 5))
      (test sref-be (bytevector-sint-ref bvector i 'big 5))
      (test uref-le (bytevector-uint-ref bvector i 'little 5))
      (test sref-le (bytevector-sint-ref bvector i 'little 5))

      (test setval (bytevector-uint-set! bvector i setval 'little 5))
      (test uref-le (bytevector-uint-ref bvector i 'big 5))
      (test sref-le (bytevector-sint-ref bvector i 'big 5))
      (test uref-be (bytevector-uint-ref bvector i 'little 5))
      (test sref-be (bytevector-sint-ref bvector i 'little 5))

      (test setval (bytevector-sint-set! bvector i setval 'little 5))
      (test uref-le (bytevector-uint-ref bvector i 'big 5))
      (test sref-le (bytevector-sint-ref bvector i 'big 5))
      (test uref-be (bytevector-uint-ref bvector i 'little 5))
      (test sref-be (bytevector-sint-ref bvector i 'little 5))
   )
   #(0 4 8 0 4.2 8.9)
   #(-1            0xFFFFFFFFFE  0x7FFFFFFFFF -0x8000000000 -0x543210543211  0x123456781234)
   #(0xFFFFFFFFFF  0xFFFFFFFFFE  0x7FFFFFFFFF  0x8000000000  0xCDEFABCDEF    0x3456781234)
   #(-1           -2             0x7FFFFFFFFF -0x8000000000 -0x3210543211    0x3456781234)
   #(0xFFFFFFFFFF  0xFEFFFFFFFF  0xFFFFFFFF7F  0x0000000080  0xEFCDABEFCD    0x3412785634)
   #(-1           -0x0100000001 -0x0000000081  0x0000000080 -0x1032541033    0x3412785634)
)

(test '(0 0 0 0 0xEF 0xCD 0xAB 0xEF 0x34 0x12 0x78 0x56 0x34 0 0) (bytevector->u8-list bvector))
(test '(0xEF 0xCDABEF3412 0x7856340000) (bytevector->uint-list bvector 'big 5))
(test '(0xEF -0x325410CBEE 0x7856340000) (bytevector->sint-list bvector 'big 5))
(test '(0xEF00000000 0x1234EFABCD 0x345678) (bytevector->uint-list bvector 'little 5))
(test '(-0x1100000000 0x1234EFABCD 0x345678) (bytevector->sint-list bvector 'little 5))

(test bvector (u8-list->bytevector '(0 0 0 0 0xEF 0xCD 0xAB 0xEF 0x34 0x12 0x78 0x56 0x34 0 0)))
(test bvector (uint-list->bytevector '(0xFF00000000EF -0x54325410CBEE 0x567856340000) 'big 5))
(test bvector (sint-list->bytevector '(0xFF00000000EF -0x54325410CBEE 0x567856340000) 'big 5))
(test bvector (uint-list->bytevector '(-0x321100000000 0xFE1234EFABCD 0x120000345678) 'little 5))
(test bvector (sint-list->bytevector '(-0x321100000000 0xFE1234EFABCD 0x120000345678) 'little 5))


(when-full-runtime
   (set! bvector (make-bytevector 16 0))
   (for-each
      (lambda (i setval uref-be sref-be uref-le sref-le)
         (test setval (bytevector-u64-set! bvector i setval 'big))
         (test uref-be (bytevector-u64-native-ref bvector i))
         (test sref-be (bytevector-s64-native-ref bvector i))
         (test uref-be (bytevector-u64-ref bvector i 'big))
         (test sref-be (bytevector-s64-ref bvector i 'big))
         (test uref-le (bytevector-u64-ref bvector i 'little))
         (test sref-le (bytevector-s64-ref bvector i 'little))

         (test setval (bytevector-s64-set! bvector i setval 'big))
         (test uref-be (bytevector-u64-native-ref bvector i))
         (test sref-be (bytevector-s64-native-ref bvector i))
         (test uref-be (bytevector-u64-ref bvector i 'big))
         (test sref-be (bytevector-s64-ref bvector i 'big))
         (test uref-le (bytevector-u64-ref bvector i 'little))
         (test sref-le (bytevector-s64-ref bvector i 'little))

         (test setval (bytevector-u64-native-set! bvector i setval))
         (test uref-be (bytevector-u64-native-ref bvector i))
         (test sref-be (bytevector-s64-native-ref bvector i))
         (test uref-be (bytevector-u64-ref bvector i 'big))
         (test sref-be (bytevector-s64-ref bvector i 'big))
         (test uref-le (bytevector-u64-ref bvector i 'little))
         (test sref-le (bytevector-s64-ref bvector i 'little))

         (test setval (bytevector-s64-native-set! bvector i setval))
         (test uref-be (bytevector-u64-native-ref bvector i))
         (test sref-be (bytevector-s64-native-ref bvector i))
         (test uref-be (bytevector-u64-ref bvector i 'big))
         (test sref-be (bytevector-s64-ref bvector i 'big))
         (test uref-le (bytevector-u64-ref bvector i 'little))
         (test sref-le (bytevector-s64-ref bvector i 'little))

         (test setval (bytevector-u64-set! bvector i setval 'little))
         (test uref-le (bytevector-u64-native-ref bvector i))
         (test sref-le (bytevector-s64-native-ref bvector i))
         (test uref-le (bytevector-u64-ref bvector i 'big))
         (test sref-le (bytevector-s64-ref bvector i 'big))
         (test uref-be (bytevector-u64-ref bvector i 'little))
         (test sref-be (bytevector-s64-ref bvector i 'little))

         (test setval (bytevector-s64-set! bvector i setval 'little))
         (test uref-le (bytevector-u64-native-ref bvector i))
         (test sref-le (bytevector-s64-native-ref bvector i))
         (test uref-le (bytevector-u64-ref bvector i 'big))
         (test sref-le (bytevector-s64-ref bvector i 'big))
         (test uref-be (bytevector-u64-ref bvector i 'little))
         (test sref-be (bytevector-s64-ref bvector i 'little))
      )
      #(0 8 0 8 0.2 8.9)
;     #(-1                    0xFFFFFFFFFFFFFFFE    0x7FFFFFFFFFFFFFFF   -0x8000000000000000  -0x543210543210543211    0x121234567812345678)
      #(-1                    18446744073709551614  0x7FFFFFFFFFFFFFFF   -0x8000000000000000  -1553133978067125154321  333353161791945070200)
      #(18446744073709551615  18446744073709551614  0x7FFFFFFFFFFFFFFF    9223372036854775808  14839268198186733039    0x1234567812345678)
      #(-1                   -2                     0x7FFFFFFFFFFFFFFF   -0x8000000000000000  -0x3210543210543211      0x1234567812345678)
      #(18446744073709551615  18374686479671623679  18446744073709551487  0x0000000000000080   17279656391726591949    0x7856341278563412)
      #(-1                   -0x0100000000000001   -0x0000000000000081    0x0000000000000080  -0x1032541032541033      0x7856341278563412)
   )

   (test '(0xEF 0xCD 0xAB 0xEF 0xCD 0xAB 0xEF 0xCD 0x78 0x56 0x34 0x12 0x78 0x56 0x34 0x12) (bytevector->u8-list bvector))
   (test '(17279656391726591949 0x7856341278563412) (bytevector->uint-list bvector 'big 8))
   (test '(-0x1032541032541033 0x7856341278563412) (bytevector->sint-list bvector 'big 8))
   (test '(14839268198186733039 0x1234567812345678) (bytevector->uint-list bvector 'little 8))
   (test '(-0x3210543210543211 0x1234567812345678) (bytevector->sint-list bvector 'little 8))

   (test bvector (u8-list->bytevector '(0xEF 0xCD 0xAB 0xEF 0xCD 0xAB 0xEF 0xCD 0x78 0x56 0x34 0x12 0x78 0x56 0x34 0x12)))
   (test bvector (uint-list->bytevector '(-5369169613131462479923 1595091165725502878738) 'big 8))
   (test bvector (sint-list->bytevector '(-5369169613131462479923 1595091165725502878738) 'big 8))
   (test bvector (uint-list->bytevector '(-1553133978067125154321 4705231507261108803192) 'little 8))
   (test bvector (sint-list->bytevector '(-1553133978067125154321 4705231507261108803192) 'little 8))


   (set! bvector (make-bytevector 20 -1))
   (for-each
      (lambda (i setval uref-be sref-be uref-le sref-le)
         (test setval (bytevector-uint-set! bvector i setval 'big 10))
         (test uref-be (bytevector-uint-ref bvector i 'big 10))
         (test sref-be (bytevector-sint-ref bvector i 'big 10))
         (test uref-le (bytevector-uint-ref bvector i 'little 10))
         (test sref-le (bytevector-sint-ref bvector i 'little 10))

         (test setval (bytevector-sint-set! bvector i setval 'big 10))
         (test uref-be (bytevector-uint-ref bvector i 'big 10))
         (test sref-be (bytevector-sint-ref bvector i 'big 10))
         (test uref-le (bytevector-uint-ref bvector i 'little 10))
         (test sref-le (bytevector-sint-ref bvector i 'little 10))

         (test setval (bytevector-uint-set! bvector i setval 'little 10))
         (test uref-le (bytevector-uint-ref bvector i 'big 10))
         (test sref-le (bytevector-sint-ref bvector i 'big 10))
         (test uref-be (bytevector-uint-ref bvector i 'little 10))
         (test sref-be (bytevector-sint-ref bvector i 'little 10))

         (test setval (bytevector-sint-set! bvector i setval 'little 10))
         (test uref-le (bytevector-uint-ref bvector i 'big 10))
         (test sref-le (bytevector-sint-ref bvector i 'big 10))
         (test uref-be (bytevector-uint-ref bvector i 'little 10))
         (test sref-be (bytevector-sint-ref bvector i 'little 10))
      )
      #(0 5 0 5 0.2 5.9)
      #(-1  1208925819614629174706174  604462909807314587353087  -604462909807314587353088  -26057264226971421213065490961  5634002657842756053938493048)
      #(1208925819614629174706175  1208925819614629174706174  604462909807314587353087  604462909807314587353088  1131814821910647726132719  408338438584099807712888)
      #(-1  -2  604462909807314587353087  -604462909807314587353088  -77110997703981448573457  408338438584099807712888)
      #(1208925819614629174706175  1204203453131759529492479  1208925819614629174706047  0x80  1132439561288193930013679  568274150128447636273238)
      #(-1  -4722366482869645213697 -0x81  0x80  -76486258326435244692497  568274150128447636273238)
   )

   (test '(0xEF 0xCD 0xAB 0xEF 0xCD 0x78 0x56 0x34 0x12 0x78 0x56 0x34 0x12 0x78 0x56 0xFF 0xFF 0xFF 0xFF 0xFF) (bytevector->u8-list bvector))
   (test '(1132439561287972309701240  407084079127969784659967) (bytevector->uint-list bvector 'big 10))
   (test '(-76486258326656865004936  407084079127969784659967) (bytevector->sint-list bvector 'big 10))
   (test '(567019790672248051584495  1208925819613901044724822) (bytevector->uint-list bvector 'little 10))
   (test '(567019790672248051584495  -0xA987EDCBAA) (bytevector->sint-list bvector 'little 10))

   (test bvector (u8-list->bytevector '(0xEF 0xCD 0xAB 0xEF 0xCD 0x78 0x56 0x34 0x12 0x78 0x56 0x34 0x12 0x78 0x56 0xFF -1 0xFEFF 0xABCDEFF -0x5432101)))
   (test bvector (uint-list->bytevector '(-13611749226709723690614215404936  5634001403483299923915440127) 'big 10))
   (test bvector (sint-list->bytevector '(-13611749226709723690614215404936  5634001403483299923915440127) 'big 10))
   (test bvector (uint-list->bytevector '(-6670659886408414387741849301521  353006339327470990884222038) 'little 10))
   (test bvector (sint-list->bytevector '(-6670659886408414387741849301521  353006339327470990884222038) 'little 10))
)


(let
   ((bvector (make-bytevector 16 0))
    (error-threshold-single 0.00001)
    (error-threshold-double 0.00000000000000001)
   )
   (for-each
      (lambda (value)
         (bytevector-ieee-single-set! bvector 1 value 'big)
         (test #t (> error-threshold-single (abs (- value (bytevector-ieee-single-ref bvector 1 'big)))))

         (bytevector-ieee-single-set! bvector 1 value 'little)
         (test #t (> error-threshold-single (abs (- value (bytevector-ieee-single-ref bvector 1 'little)))))

         (bytevector-ieee-single-native-set! bvector 4 value)
         (test #t (> error-threshold-single (abs (- value (bytevector-ieee-single-native-ref bvector 4)))))

         (bytevector-ieee-double-set! bvector 1 value 'big)
         (test #t (> error-threshold-double (abs (- value (bytevector-ieee-double-ref bvector 1 'big)))))

         (bytevector-ieee-double-set! bvector 1 value 'little)
         (test #t (> error-threshold-double (abs (- value (bytevector-ieee-double-ref bvector 1 'little)))))

         (bytevector-ieee-double-native-set! bvector 8 value)
         (test #t (> error-threshold-double (abs (- value (bytevector-ieee-double-native-ref bvector 8)))))
      )
      (list
         0 1.2345678901234567890123456789 -1.2345678901234567890123456789
         16.2345678901234567890123456789 -16.2345678901234567890123456789
         90 -90 120.01 -120.01
         2e-149 (* (- 1 2e-23) 2e-126) -2e-126 2e10
         2e-1074 (* (- 1 2e-52) 2e-1022) -2e-1022
         7.1746481373430634e-43
      )
   )
)

(define positive-inf (bytevector-ieee-single-native-ref #vu8(0x7F 0x80 0 0) 0))
(test #t (positive? positive-inf))
(test #t (infinite? positive-inf))
(test #f (finite? positive-inf))
(define positive-inf (bytevector-ieee-single-ref #vu8(0 0 0x80 0x7F) 0 'little))
(test #t (positive? positive-inf))
(test #t (infinite? positive-inf))
(test #f (finite? positive-inf))
(define negative-inf (bytevector-ieee-single-native-ref #vu8(0xFF 0x80 0 0) 0))
(test #t (negative? negative-inf))
(test #t (infinite? negative-inf))
(test #f (finite? negative-inf))
(define negative-inf (bytevector-ieee-single-ref #vu8(0 0 0x80 0xFF) 0 'little))
(test #t (negative? negative-inf))
(test #t (infinite? negative-inf))
(test #f (finite? negative-inf))
(define nan (bytevector-ieee-single-ref #vu8(0x7F 0xC0 0 0) 0 'big))
(test #f (finite? nan))
(test #f (infinite? nan))
(test #t (nan? nan))

(define positive-inf (bytevector-ieee-double-native-ref #vu8(0x7F 0xF0 0 0 0 0 0 0) 0))
(test #t (positive? positive-inf))
(test #t (infinite? positive-inf))
(define positive-inf (bytevector-ieee-double-ref #vu8(0 0 0 0 0 0 0xF0 0x7F) 0 'little))
(test #t (positive? positive-inf))
(test #t (infinite? positive-inf))
(define negative-inf (bytevector-ieee-double-native-ref #vu8(0xFF 0xF0 0 0 0 0 0 0) 0))
(test #t (negative? negative-inf))
(test #t (infinite? negative-inf))
(define negative-inf (bytevector-ieee-double-ref #vu8(0 0 0 0 0 0 0xF0 0xFF) 0 'little))
(test #t (negative? negative-inf))
(test #t (infinite? negative-inf))
(define nan (bytevector-ieee-double-ref #vu8(0x7F 0xF8 0 0 0 0 0 0) 0 'big))
(test #f (finite? nan))
(test #f (infinite? nan))
(test #t (nan? nan))


(test #t (bytevector=? #vu8(0x41 0x42 0x43 0x20 0x74 0x65 0x73 0x74 0x20 0x73 0x74 0x72 0x69 0x6E 0x67)
            (string->utf8 "ABC test string")))

(for-each
   (lambda (base-str)
      (let ((bom-str (string-append "\uFEFF" base-str)))
         (test base-str (utf8->string (string->utf8 base-str)))
         (test bom-str (utf8->string (string->utf8 bom-str)))
         (test (string-append "\uFFFE" base-str)
               (utf8->string (string->utf8 (string-append "\uFFFE" base-str))))
         (test (string->utf8 bom-str)
               (bytevector-append #vu8(0xEF 0xBB 0xBF) (string->utf8 base-str)))
         (test (string->utf8 (string-append "\uFFFE" base-str))
               (bytevector-append #vu8(0xEF 0xBF 0xBE) (string->utf8 base-str)))
         (test (string-append base-str "\uFFFD")
               (utf8->string (bytevector-append (string->utf8 base-str) #vu8(0xC0))))
      )
   )
   (list
      "" "ABC test string" "ABC test\uFEFFstring"
      "\u0000\u007F\u0080\u07FF\u0800\u0FFF\u1000\uCFFF\uD000\uD7FF\uE000\uFFFF\u10000\u3FFFF\u40000\uFFFFF\u100000\u10FFFF"
      "\u0012\u0123\u0ABC\u1234\uD123\uEEEE\u12345\u45678\u10ABCD"
   )
)

(when-full-runtime
   (let ((test-str "ABC test string"))
      (test #t (bytevector=? #vu8(0 0x41 0 0x42 0 0x43 0 0x20 0 0x74 0 0x65 0 0x73 0 0x74 0 0x20
                                  0 0x73 0 0x74 0 0x72 0 0x69 0 0x6E 0 0x67)
                  (string->utf16 test-str)))
      (test #t (bytevector=? #vu8(0x41 0 0x42 0 0x43 0 0x20 0 0x74 0 0x65 0 0x73 0 0x74 0 0x20 0
                                  0x73 0 0x74 0 0x72 0 0x69 0 0x6E 0 0x67 0)
                  (string->utf16 test-str 'little)))
      (test (string->utf16 test-str) (string->utf16 test-str 'big))
      (test #t (bytevector=? #vu8(0 0 0 0x41 0 0 0 0x42 0 0 0 0x43 0 0 0 0x20 0 0 0 0x74
                                  0 0 0 0x65 0 0 0 0x73 0 0 0 0x74 0 0 0 0x20 0 0 0 0x73
                                  0 0 0 0x74 0 0 0 0x72 0 0 0 0x69 0 0 0 0x6E 0 0 0 0x67)
                  (string->utf32 test-str)))
      (test #t (bytevector=? #vu8(0x41 0 0 0 0x42 0 0 0 0x43 0 0 0 0x20 0 0 0 0x74 0 0 0
                                  0x65 0 0 0 0x73 0 0 0 0x74 0 0 0 0x20 0 0 0 0x73 0 0 0
                                  0x74 0 0 0 0x72 0 0 0 0x69 0 0 0 0x6E 0 0 0 0x67 0 0 0)
                  (string->utf32 test-str 'little)))
      (test (string->utf32 test-str) (string->utf32 test-str 'big))
   )

   (for-each
      (lambda (base-str)
         (let ((bom-str (string-append "\uFEFF" base-str)))
            (test base-str (utf16->string (string->utf16 base-str 'big) 'big))
            (test base-str (utf16->string (string->utf16 bom-str 'big) 'big))
            (test base-str (utf16->string (string->utf16 bom-str 'big) 'little)) ; actual mode: big endian
            (test base-str (utf16->string (bytevector-append #vu8(0xFE 0xFF) (string->utf16 base-str 'big)) 'little)) ; actual mode: big endian
            (test base-str (utf16->string (string->utf16 base-str 'big) 'big #f))
            (test base-str (utf16->string (string->utf16 bom-str 'big) 'big #f))
            (test base-str (utf16->string (string->utf16 bom-str 'big) 'little #f)) ; actual mode: big endian
            (test base-str (utf16->string (bytevector-append #vu8(0xFE 0xFF) (string->utf16 base-str 'big)) 'little #f)) ; actual mode: big endian
            (test base-str (utf16->string (string->utf16 base-str 'big) 'big #t))
            (test bom-str (utf16->string (string->utf16 bom-str 'big) 'big #t))

            (test base-str (utf16->string (string->utf16 base-str 'little) 'little))
            (test base-str (utf16->string (string->utf16 bom-str 'little) 'little))
            (test base-str (utf16->string (string->utf16 bom-str 'little) 'big)) ; actual mode: little endian
            (test base-str (utf16->string (bytevector-append #vu8(0xFF 0xFE) (string->utf16 base-str 'little)) 'big)) ; actual mode: little endian
            (test base-str (utf16->string (string->utf16 base-str 'little) 'little #f))
            (test base-str (utf16->string (string->utf16 bom-str 'little) 'little #f))
            (test base-str (utf16->string (string->utf16 bom-str 'little) 'big #f)) ; actual mode: little endian
            (test base-str (utf16->string (bytevector-append #vu8(0xFF 0xFE)(string->utf16 base-str 'little)) 'big #f)) ; actual mode: little endian
            (test base-str (utf16->string (string->utf16 base-str 'little) 'little #t))
            (test bom-str (utf16->string (string->utf16 bom-str 'little) 'little #t))

            (test #t (bytevector=? (string->utf16 bom-str 'big) (bytevector-append #vu8(0xFE 0xFF) (string->utf16 base-str 'big))))
            (test #t (bytevector=? (string->utf16 bom-str 'little) (bytevector-append #vu8(0xFF 0xFE) (string->utf16 base-str 'little))))

            (test (string-append base-str "\uFFFD")
                  (utf16->string (bytevector-append (string->utf16 base-str 'big) #vu8(0)) 'big))
            (test (string-append base-str "\uFFFD")
                  (utf16->string (bytevector-append (string->utf16 base-str 'little) #vu8(0)) 'little))
            (test (string-append base-str "\uFFFD\uFFFD")
                  (utf16->string (bytevector-append (string->utf16 base-str 'big) #vu8(0xD8 0 0 0 0xDF 0x34)) 'big))
            (test (string-append base-str "\uFFFD\uFFFD")
                  (utf16->string (bytevector-append (string->utf16 base-str 'little) #vu8(0 0xD8 0 0 0x34 0xDF)) 'little))

            (test base-str (utf32->string (string->utf32 base-str 'big) 'big))
            (test base-str (utf32->string (string->utf32 bom-str 'big) 'big))
            (test base-str (utf32->string (string->utf32 bom-str 'big) 'little)) ; actual mode: big endian
            (test base-str (utf32->string (string->utf32 base-str 'big) 'big #f))
            (test base-str (utf32->string (string->utf32 bom-str 'big) 'big #f))
            (test base-str (utf32->string (string->utf32 bom-str 'big) 'little #f)) ; actual mode: big endian
            (test base-str (utf32->string (string->utf32 base-str 'big) 'big #t))
            (test bom-str (utf32->string (string->utf32 bom-str 'big) 'big #t))

            (test base-str (utf32->string (string->utf32 base-str 'little) 'little))
            (test base-str (utf32->string (string->utf32 bom-str 'little) 'little))
            (test base-str (utf32->string (string->utf32 bom-str 'little) 'big)) ; actual mode: little endian
            (test base-str (utf32->string (string->utf32 base-str 'little) 'little #f))
            (test base-str (utf32->string (string->utf32 bom-str 'little) 'little #f))
            (test base-str (utf32->string (string->utf32 bom-str 'little) 'big #f)) ; actual mode: little endian
            (test base-str (utf32->string (string->utf32 base-str 'little) 'little #t))
            (test bom-str (utf32->string (string->utf32 bom-str 'little) 'little #t))

            (test #t (bytevector=? (string->utf32 bom-str 'big) (bytevector-append #vu8(0 0 0xFE 0xFF) (string->utf32 base-str 'big))))
            (test #t (bytevector=? (string->utf32 bom-str 'little) (bytevector-append #vu8(0xFF 0xFE 0 0) (string->utf32 base-str 'little))))

            (test (string-append base-str "\uFFFD")
                  (utf32->string (bytevector-append (string->utf32 base-str 'big) #vu8(0)) 'big))
            (test (string-append base-str "\uFFFD")
                  (utf32->string (bytevector-append (string->utf32 base-str 'little) #vu8(0)) 'little))
            (test (string-append base-str "\uFFFD\uFFFD\uFFFD\uFFFD")
                  (utf32->string (bytevector-append (string->utf32 base-str 'big) #vu8(0 0 0xD8 0 0 0 0xDF 0x34 0 0x11 0 0 1 0 0 0)) 'big))
            (test (string-append base-str "\uFFFD\uFFFD\uFFFD\uFFFD")
                  (utf32->string (bytevector-append (string->utf32 base-str 'little) #vu8(0 0xD8 0 0 0x34 0xDF 0 0 0 0 0x11 0 0 0 0 1)) 'little))
         )
      )
      (list
         "" "ABC test string" "ABC test\uFEFFstring"
         "\u0000\u007F\u0080\u07FF\u0800\u0FFF\u1000\uCFFF\uD000\uD7FF\uE000\uFFFF\u10000\u3FFFF\u40000\uFFFFF\u100000\u10FFFF"
         "\u0012\u0123\u0ABC\u1234\uD123\uEEEE\u12345\u45678\u10ABCD"
      )
   )
)


; Support for Binary object in bytevector read-only functions

(define bin #zABCDEF0123456789)
(test 2 (bytevector-length #z012A))
(test 8 (bytevector-length bin))
(test 0xAB (bytevector-u8-ref bin 0))
(test -0x55 (bytevector-s8-ref bin 0))
(test 0xCDEF (bytevector-u16-ref bin 1 'big))
(test -0x1033 (bytevector-s16-ref bin 1 'little))
(test 0xEF01 (bytevector-u16-native-ref bin 2))
(test -0x10FF (bytevector-s16-native-ref bin 2))
(test 0xCDEF0123 (bytevector-u32-ref bin 1 'big))
(test 0x2301EFCD (bytevector-s32-ref bin 1 'little))
(test 0xABCDEF01 (bytevector-u32-native-ref bin 0))
(test -0x543210FF (bytevector-s32-native-ref bin 0))
(test 0xCDEF012345 (bytevector-uint-ref bin 1 'big 5))
(test 0x452301EFCD (bytevector-sint-ref bin 1 'little 5))
(when-full-runtime
   (test 12379813738877118345 (bytevector-u64-ref bin 0 'big))
   (test -0x7698BADCFE103255 (bytevector-s64-ref bin 0 'little))
   (test 12379813738877118345 (bytevector-u64-native-ref bin 0))
   (test -0x543210fedcba9877 (bytevector-s64-native-ref bin 0))
)
(let ((vec #vu8(0xCD 0xEF 0x01 0x23)))
   (test (bytevector-ieee-single-ref vec 0 'big) (bytevector-ieee-single-ref bin 1 'big))
   (test (bytevector-ieee-single-ref vec 0 'little) (bytevector-ieee-single-ref bin 1 'little))
)
(test (bytevector-ieee-double-ref #vu8(0xAB 0xCD 0xEF 0x01 0x23 0x45 0x67 0x89) 0 'big)
   (bytevector-ieee-double-ref bin 0 'big))
(test (bytevector-ieee-double-ref #vu8(0xAB 0xCD 0xEF 0x01 0x23 0x45 0x67 0x89) 0 'little)
   (bytevector-ieee-double-ref bin 0 'little))
(test '(0xAB 0xCD 0xEF 0x01 0x23 0x45 0x67 0x89) (bytevector->u8-list bin))
(test '(0xABCD 0xEF01 0x2345 0x6789) (bytevector->uint-list bin 'big 2))
(test '(0x01EFCDAB -0x7698BADD) (bytevector->sint-list bin 'little 4))


(define str "")
(test '() (string-for-each (lambda chars (set! str (string-append str " " (apply string chars)))) "atc" "beh" "csa" "dtr"))
(test " abcd test char" str)
(test '() (string-for-each (lambda chars (set! str (string-append str " " (apply string chars)))) "testing"))
(test " abcd test char t e s t i n g" str)

(define scopy (string-copy str))
(test #t (string? scopy))
(test " abcd test char t e s t i n g" scopy)
(test #t (= str scopy))
(test #t (equal? str scopy))
(test #f (eq? str scopy))
(set! str "test test")
(test " abcd test char t e s t i n g" scopy)

(define lst '())
(test '() (vector-for-each (lambda args (set! lst (append lst args))) #(1 2 3) #(a b c) #(#t #f "z")))
(test '(1 a #t 2 b #f 3 c "z") lst)
(test '() (vector-for-each (lambda args (set! lst (cons (apply + args) lst))) #(6 5 4)))
(test '(4 5 6 1 a #t 2 b #f 3 c "z") lst)

; R6RS Standard Libraries #3. List Utilities
(test 15 (fold-left + 0 '(1 2 3 4 5)))
(test '(5 4 3 2 1) (fold-left (lambda (a e) (cons e a)) '() '(1 2 3 4 5)))
(test '(5 4 3 2 1) (fold-left (lambda (a e) (cons e a)) '() #(1 2 3 4 5)))
(test '(5 4 3 2 1) (fold-left (lambda (a e) (cons e a)) '() (collection 1 2 3 4 5)))
(test 7 (fold-left (lambda (count x) (if (odd? x) (+ count 1) count)) 0 '(3 1 4 1 5 9 2 6 5 3)))
(test 7 (fold-left (lambda (max-len s) (max max-len (string-length s))) 0 '("longest" "long" "longer")))
(test '((((q) . a) . b) . c) (fold-left cons '(q) '(a b c)))
(test '((((q) . a) . b) . c) (fold-left cons '(q) #(a b c)))
(test '((((q) . a) . b) . c) (fold-left cons '(q) (collection 'a 'b 'c)))
(test 21 (fold-left + 0 '(1 2 3) '(4 5 6)))
(test 21 (fold-left + 0 #(1 2 3) #(4 5 6)))
(test 21 (fold-left + 0 (collection 1 2 3) (collection 4 5 6)))

(test 15 (fold-right + 0 '(1 2 3 4 5)))
(test '(1 2 3 4 5) (fold-right cons '() '(1 2 3 4 5)))
(test '(3 1 1 5 9 5)
   (fold-right
      (lambda (x l)
         (if (odd? x) (cons x l) l)
      )
      '()
      '(3 1 4 1 5 9 2 6 5)
   )
)
(test '(a b c q) (fold-right cons '(q) '(a b c)))
(test 21 (fold-right + 0 '(1 2 3) '(4 5 6)))
(test 21 (fold-right + 0 (collection 1 2 3) (collection 4 5 6)))
(test 21 (fold-right + 0 #(1 2 3) #(4 5 6)))
(test '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6)) (fold-right (lambda (a b accum) (cons (cons a b) accum )) '() '(1 2 3 4 5) '(2 3 4 5 6)))
(test '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6)) (fold-right (lambda (a b accum) (cons (cons a b) accum )) '() (collection 1 2 3 4 5) (collection 2 3 4 5 6)))
(test '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6)) (fold-right (lambda (a b accum) (cons (cons a b) accum )) '() #(1 2 3 4 5) #(2 3 4 5 6)))

(test 4 (find even? '(3 1 4 1 5 9)))
(test #f (find even? '(3 1 5 1 5 9)))

(test '(1 2 3) (filter positive? '(-1 1 0 2 3)))
(test #(-2 1 3) (filter (lambda (x) (<= (* x x) 9)) #(-2 1 3 4)))
(test (collection 0 2 4 6 8 2) (filter even? (collection 0 1 2 3 4 5 6 7 8 9 2 1)))

(let-values
   (((pos neg) (partition positive? '(-1 1 0 2 3)))
    ((sq1 sq2) (partition (lambda (x) (<= (* x x) 9)) #(-2 1 3 4)))
    ((even odd) (partition  even? (collection 0 1 2 3 4 5 6 7 8 9 2 1)))
    ((all empty) (partition number? '(1 2 4 2 5 1)))
   )
   (test '(1 2 3) pos)
   (test '(-1 0) neg)
   (test #(-2 1 3) sq1)
   (test #(4) sq2)
   (test (collection 0 2 4 6 8 2) even)
   (test (collection 1 3 5 7 9 1) odd)
   (test '(1 2 4 2 5 1) all)
   (test '() empty)
)

(test #t (for-all even? '()))
(test #t (for-all = '() '()))
(test #t (for-all even? '(2 4 6 8 10)))
(test #f (for-all even? '(2 4 6 8 9)))
(test #f (for-all even? '(2 4 6 8 9 . 10)))
(test 5 (for-all (lambda (n) n) '(1 2 3 4 5)))
(test #f (for-all (lambda (n) n) '(1 2 #f 4 5)))
(test #t (for-all < '(1 2 3 4) '(2 3 4 5) '(3 4 5 6)))
(test #f (for-all < '(1 2 4 4) '(2 3 4 5) '(3 4 5 6)))

(test #f (exists even? '()))
(test #f (exists = '() '()))
(test #t (exists even? '(1 3 5 8 9)))
(test #f (exists even? '(1 3 5 7 9)))
(test #t (exists even? '(1 3 5 8 9 . 10)))
(test 2 (exists (lambda (n) (and (even? n) n)) '(1 2 3 4 5)))
(test #f (exists (lambda (n) (and (even? n) n)) '(1 3 5 7 9)))
(test #f (exists = '(1 2 3 4) '(2 3 4 5) '(3 4 5 6)))
(test #t (exists < '(1 2 4 4) '(2 3 4 5) '(3 4 4 6)))

; remp, remove, remq, remv

(test '() (remp even? '()))
(test '() (remp even? '(2 4 6 8)))
(test '(1 3 5 7) (remp even? '(1 2 3 4 5 6 7)))
(test '(1 2 3) (remp boolean? '(1 2 #f 3 #t)))

(test #() (remp even? #()))
(test #() (remp even? #(2 4 6 8)))
(test #(1 3 5 7) (remp even? #(1 2 3 4 5 6 7)))
(test #(1 2 3) (remp boolean? #(1 2 #f 3 #t)))

(test (collection) (remp even? (collection)))
(test (collection) (remp even? (collection 2 4 6 8)))
(test (collection 1 3 5 7) (remp even? (collection 1 2 3 4 5 6 7)))
(test (collection 1 2 3) (remp boolean? (collection 1 2 #f 3 #t)))

(define a '(1 . 2))
(define b '(1))
(define c '(1 . 2))

(test '() (remove 1 '()))
(test '() (remove 'a '(a a a a)))
(test '(2 3 4 5 6) (remove 1 '(1 2 3 1 4 1 5 6 1)))
(test '(1 2 3 4 5) (remove 0 '(1 2 3 4 5)))
(test (list b) (remove a (list a b c)))

(test #() (remove 1 #()))
(test #() (remove 'a #(a a a a)))
(test #(2 3 4 5 6) (remove 1 #(1 2 3 1 4 1 5 6 1)))
(test #(1 2 3 4 5) (remove 0 #(1 2 3 4 5)))
(test (vector b) (remove a (vector a b c)))

(test (collection) (remove 1 (collection)))
(test (collection) (remove 'a (collection 'a 'a 'a 'a)))
(test (collection 2 3 4 5 6) (remove 1 (collection 1 2 3 1 4 1 5 6 1)))
(test (collection 1 2 3 4 5) (remove 0 (collection 1 2 3 4 5)))
(test (collection b) (remove a (collection a b c)))

(test (list b c) (remq a (list a b c)))
(test (vector b c) (remv a (vector a b c)))
(test (collection b c) (remv a (collection a b c)))

; memp

(test #f (memp even? '()))
(test '(2 4 5 6 7) (memp even? '(1 3 2 4 5 6 7)))
(test '(1 3 2 4 5 6 7) (memp odd? '(1 3 2 4 5 6 7)))
(test #f (memp zero? '(1 3 2 4 5 6 7)))

(test #f (memp even? #()))
(test #(2 4 5 6 7) (memp even? #(1 3 2 4 5 6 7)))
(test #(1 3 2 4 5 6 7) (memp odd? #(1 3 2 4 5 6 7)))
(test #f (memp zero? #(1 3 2 4 5 6 7)))

(test #f (memp even? (collection)))
(test (collection 2 4 5 6 7) (memp even? (collection 1 3 2 4 5 6 7)))
(test (collection 1 3 2 4 5 6 7) (memp odd? (collection 1 3 2 4 5 6 7)))
(test #f (memp zero? (collection 1 3 2 4 5 6 7)))

; assp

(test #f (assp even? '()))
(test '(2 b) (assp even? '((1 a) (2 b) (3 c))))
(test '(1 a) (assp odd? #((1 a) (2 b) (3 c))))
(test #f (assp zero? (collection '(1 a) '(2 b) '(3 c))))

; cons*

(test 1 (cons* 1))
(test '() (cons* '()))
(test '(1) (cons* 1 '()))
(test '(1 2) (cons* 1 2 '()))
(test '(1 . 2) (cons* 1 2))
(test '(1 2 . 3) (cons* 1 2 3))
(test '(1 2 3 4 5) (cons* 1 2 '(3 4 5)))

; R6RS Standard Libraries #1. Unicode
(define chars
   '(#\a #\A #\1 #\  #\. #\\u01C4 #\\u01C5 #\\u01C6 #\\u03C2 #\\u03C3 #\\u03A3
     #\\u01C4 #\\u01C5 #\\u01C6 #\\u01C7 #\\u01C8 #\\u01C9 #\\u01CA #\\u01CB #\\u01CC
     #\\u01F1 #\\u01F2 #\\u01F3 #\\u1F81 #\\u1F89 #\\u1F90 #\\u1F98 #\\u1FA4 #\\u1FAC
     #\\u0130 #\\u0131
     #\\u000D #\\u00AD #\\uE000 #\\u02BA #\\u05D8 #\\u0B40 #\\u20DF #\\u0363 #\\u0666
     #\\u00BD #\\uFE4F #\\u3030 #\\u300F #\\u00BB #\\u00AB #\\u066D #\\u27EA #\\u20AB
     #\\u309C #\\u2907 #\\u00A6 #\\u2028 #\\u2029 #\\u0020
   )
)

(test
   '(#\A #\A #\1 #\  #\. #\\u01C5 #\\u01C5 #\\u01C5 #\\u03A3 #\\u03A3 #\\u03A3
     #\\u01C5 #\\u01C5 #\\u01C5 #\\u01C8 #\\u01C8 #\\u01C8 #\\u01CB #\\u01CB #\\u01CB
     #\\u01F2 #\\u01F2 #\\u01F2 #\\u1F89 #\\u1F89 #\\u1F98 #\\u1F98 #\\u1FAC #\\u1FAC
     #\\u0130 #\\u0049
     #\\u000D #\\u00AD #\\uE000 #\\u02BA #\\u05D8 #\\u0B40 #\\u20DF #\\u0363 #\\u0666
     #\\u00BD #\\uFE4F #\\u3030 #\\u300F #\\u00BB #\\u00AB #\\u066D #\\u27EA #\\u20AB
     #\\u309C #\\u2907 #\\u00A6 #\\u2028 #\\u2029 #\\u0020
   )
   (map char-titlecase chars)
)

(test    
   '(#\a #\a #\1 #\  #\. #\\u01C6 #\\u01C6 #\\u01C6 #\\u03C3 #\\u03C3 #\\u03C3
     #\\u01C6 #\\u01C6 #\\u01C6 #\\u01C9 #\\u01C9 #\\u01C9 #\\u01CC #\\u01CC #\\u01CC
     #\\u01F3 #\\u01F3 #\\u01F3 #\\u1F81 #\\u1F81 #\\u1F90 #\\u1F90 #\\u1FA4 #\\u1FA4
     #\\u0130 #\\u0131
     #\\u000D #\\u00AD #\\uE000 #\\u02BA #\\u05D8 #\\u0B40 #\\u20DF #\\u0363 #\\u0666
     #\\u00BD #\\uFE4F #\\u3030 #\\u300F #\\u00BB #\\u00AB #\\u066D #\\u27EA #\\u20AB
     #\\u309C #\\u2907 #\\u00A6 #\\u2028 #\\u2029 #\\u0020
   )
   (map char-foldcase chars)
)
(when-full-runtime
   (test
      '(Ll Lu Nd Zs Po Lu Lt Ll Ll Ll Lu Lu Lt Ll Lu Lt Ll Lu Lt Ll Lu Lt Ll Ll Lt Ll Lt Ll Lt Lu Ll
        Cc Cf Co Lm Lo Mc Me Mn Nd No Pc Pd Pe Pf Pi Po Ps Sc Sk Sm So Zl Zp Zs
      )
      (map char-general-category chars)
   )
)

(for-each
   (lambda (test-char)
      (test #t (char-title-case? test-char))
   )
   '(#\\u01C5 #\\u01C8 #\\u01CB #\\u01F2 #\\u1F88
     #\\u1F89 #\\u1F8A #\\u1F8B #\\u1F8C #\\u1F8D
     #\\u1F8E #\\u1F8F #\\u1F98 #\\u1F99 #\\u1F9A
     #\\u1F9B #\\u1F9C #\\u1F9D #\\u1F9E #\\u1F9F
     #\\u1FA8 #\\u1FA9 #\\u1FAA #\\u1FAB #\\u1FAC
     #\\u1FAD #\\u1FAE #\\u1FAF #\\u1FBC #\\u1FCC
     #\\u1FFC
   )
)

(for-each
   (lambda (test-char)
      (test #f (char-title-case? test-char))
   )
   '(#\a #\A #\1 #\  #\. 
     #\\u1111 #\\u01C4 #\\u01C6 #\\u01C7 #\\u01C9
     #\\u1F90 #\\u1F95 #\\u1FA0 #\\u1FA5 #\\u1FC3
   )
)

(test "Test String" (string-titlecase "test string"))
(test "Test String" (string-titlecase "TEST STRING"))
(test "Test String" (string-titlecase "teSt sTRiNg"))
(test "Test String\nWith\nNew Line" (string-titlecase "tesT stRing\nwith\nnew line"))
(test "Nexj's R6rs Title-case Test String" (string-titlecase "NexJ'S R6RS title-Case TEst string"))
(test "Nexj's R6rs: Title-case Test String - W1th, L0ts Of ? Inappropriate; Punctuation & Marks."
   (string-titlecase "NexJ'S R6RS: title-Case TEst string - W1th, l0tS oF ? INapproPRIate; pUnCtUaTiOn & maRKS.")
)
(test "A Latin Test: \u03A3\u03C3\u03C3\u03C2 \u01CB\u03B1 \u0391\u03B2 \u03A3\u03C3\u03C2"
   (string-titlecase "a laTin tEst: \u03C2\u03C3\u03A3\u03A3 \u01CB\u03B1 \u03B1\u0392 \u03A3\u03C3\u03A3")
)

(test "nexj's r6rs: foldcase test string - w1th, l0ts of ? inappropriate; punctuation & marks."
   (string-foldcase "NexJ'S R6RS: foldCase TEst string - W1th, l0tS oF ? INapproPRIate; pUnCtUaTiOn & maRKS.")
)
(let ((s (string-foldcase "\u0130\u0131\u0069\u0049 \u03C3\u03A3\u03A3\u03C2 \u01CB\u03B1 \u0391\u0392 \u03A3\u03C3\u03C2")))
   (test #t 
      (or
         (equal? "\u0069\u0069\u0069\u0069 \u03C3\u03C3\u03C3\u03C2 \u01CC\u03B1 \u03B1\u03B2 \u03C3\u03C3\u03C2" s)
         (equal? "\u0069\u0069\u0069\u0069 \u03C3\u03C3\u03C3\u03C3 \u01CC\u03B1 \u03B1\u03B2 \u03C3\u03C3\u03C3" s)
      )
   )
)

; R6RS Standard Libraries #4. Sorting

(define test-list '(9 7 1 4 6 2 3 5 2 8))
(test '(1 2 2 3 4 5 6 7 8 9) (list-sort < test-list))
(test '(9 7 1 4 6 2 3 5 2 8) test-list)

(define test-vector '#(9 7 1 4 6 2 3 5 2 8))
(test '#(9 8 7 6 5 4 3 2 2 1) (vector-sort > test-vector))
(test '#(9 7 1 4 6 2 3 5 2 8) test-vector)

; R6RS Standard Libraries #5. Control Structures
(define vals (list 0 0))
(define case-lambda-test
   (case-lambda
      (() 'Zero)
      ((a) `(One ,a))
      ((a b) (set-car! vals a) (set-cdr! vals b) `(Two ,a ,b))
      ((a b c d . e) (cons* 'Many a b c d e))
      (rest (list 'Sum (apply + rest)))
   )
)
(test 'Zero (case-lambda-test))
(test '(One 1) (case-lambda-test 1))
(test '(Two 1 2) (case-lambda-test 1 2))
(test vals '(1 . 2))
(test '(Sum 6) (case-lambda-test 1 2 3))
(test '(Many 1 2 3 4) (case-lambda-test 1 2 3 4))
(test '(Many 1 2 3 4 5) (case-lambda-test 1 2 3 4 5))
(test '(Many 1 2 3 4 5 6) (case-lambda-test 1 2 3 4 5 6))
(test '(Many 1 2 (3) (4 5 6)) (case-lambda-test (- 5 4) (* 1 2) '(3) '(4 5 6)))

(define case-lambda-test
   (case-lambda
      ((a b c d . e) (cons* 'Many a b c d e))
      (rest (list 'Sum (apply + rest)))
      (() 'Zero)
      ((a) `(One ,a))
      ((a b) `(Two ,a ,b))
   )
)
(test '(Sum 0) (case-lambda-test))
(test '(Sum 1) (case-lambda-test 1))
(test '(Sum 3) (case-lambda-test 1 2))
(test '(Sum 6) (case-lambda-test 1 2 3))
(test '(Many 1 2 3 4) (case-lambda-test 1 2 3 4))
(test '(Many 1 2 3 4 5) (case-lambda-test 1 2 3 4 5))

; R6RS Standard Libraries
; #11.4 Exact Bitwise Arithmetic

(define min-int -2147483648)
(define max-int 2147483647)

; Basic test cases

; not, and, ior, xor, if

(test -1 (bitwise-not 0))
(test -11 (bitwise-not 10))
(test 0 (bitwise-not -1))
(test 9 (bitwise-not -10))

(test -1 (bitwise-and))
(test 1 (bitwise-and 1))
(test 4 (bitwise-and 12 6))
(test 4 (bitwise-and -12 6))
(test 8 (bitwise-and 12 -6))
(test -16 (bitwise-and -12 -6))
(test 0 (bitwise-and 1 2 4 8 15))
(test 25 (bitwise-and -1 25))

(test 0 (bitwise-ior))
(test 14 (bitwise-ior 12 6))
(test -10 (bitwise-ior -12 6))
(test -2 (bitwise-ior 12 -6))
(test -2 (bitwise-ior -12 -6))
(test
   (bitwise-not (bitwise-ior 10 20 30))
   (bitwise-and (bitwise-not 10) (bitwise-not 20) (bitwise-not 30))
)

(test 0 (bitwise-xor))
(test 10 (bitwise-xor 12 6))
(test -14 (bitwise-xor -12 6))
(test -10 (bitwise-xor 12 -6))
(test 14 (bitwise-xor -12 -6))
(test 15 (bitwise-xor 1 2 4 8))

(test 12 (bitwise-if 12 12 12))
(test 4 (bitwise-if 15 4 10))
(test 20 (bitwise-if 15 4 20))
(test 12 (bitwise-if 10 8 4))
(test 127 (bitwise-if 75 75 52))
(test 29 (bitwise-if -8 26 53))

; bitwise-bit-count

(test 0 (bitwise-bit-count 0))
(test 4 (bitwise-bit-count 15))
(test 6 (bitwise-bit-count 365))
(test -4 (bitwise-bit-count -15))
(test -6 (bitwise-bit-count -365))

; bitwise-length

(test 0 (bitwise-length 0))
(test 5 (bitwise-length 16))
(test 9 (bitwise-length 365))
(test 4 (bitwise-length -16))
(test 9 (bitwise-length -365))

; bitwise-first-bit-set

(test -1 (bitwise-first-bit-set 0))
(test 0 (bitwise-first-bit-set 15))
(test 2 (bitwise-first-bit-set 12))

(test 0 (bitwise-first-bit-set -15))
(test 2 (bitwise-first-bit-set -12))

; bitwise-bit-set?

(test #f (bitwise-bit-set? 14 0))
(test #t (bitwise-bit-set? 14 3))
(test #f (bitwise-bit-set? 14 9))

(test #t (bitwise-bit-set? -1 9))
(test #f (bitwise-bit-set? -2 0))
(test #t (bitwise-bit-set? -2 1))
(test #t (bitwise-bit-set? -2 100))

; bitwise-copy-bit

(test 0 (bitwise-copy-bit 4 2 0))
(test 381 (bitwise-copy-bit 365 4 1))
(test 365 (bitwise-copy-bit 365 4 0))
(test 63 (bitwise-copy-bit 31 5 1))
(test 8 (bitwise-copy-bit 8 129 1))

(test -2 (bitwise-copy-bit -1 0 0))
(test -381 (bitwise-copy-bit -365 4 0))
(test -365 (bitwise-copy-bit -365 4 1))
(test -63 (bitwise-copy-bit -31 5 0))
(test -8 (bitwise-copy-bit -8 129 0))

; bitwise-bit-field

(test 0 (bitwise-bit-field 1023 4 4))
(test 1 (bitwise-bit-field 1023 4 5))
(test 15 (bitwise-bit-field 1023 4 8))
(test 0 (bitwise-bit-field 1023 14 18))

(test 0 (bitwise-bit-field -15 4 4))
(test 1 (bitwise-bit-field -15 4 5))
(test 12 (bitwise-bit-field -15 2 6))
(test 15 (bitwise-bit-field -15 14 18))

; bitwise-copy-bit-field

(test 1 (bitwise-copy-bit-field 15 1 4 32))
(test 1008 (bitwise-copy-bit-field 1023 0 4 32))
(test 1025 (bitwise-copy-bit-field 1 10 11 1))
(test -16 (bitwise-copy-bit-field -1 0 4 128))

; bitwise-arithmetic-shift, -shift-left, -shift-right

(test 50 (bitwise-arithmetic-shift-left 25 1))
(test -1024 (bitwise-arithmetic-shift-left -1 10))
(test (bitwise-arithmetic-shift-left 4 8) (bitwise-arithmetic-shift 1 10))

(test -13 (bitwise-arithmetic-shift-right -25 1))
(test 0 (bitwise-arithmetic-shift-right 1 10))
(test (bitwise-arithmetic-shift-right 123456789 10) (bitwise-arithmetic-shift 123456789 -10))

(test 50 (bitwise-arithmetic-shift 25 1))
(test 12 (bitwise-arithmetic-shift 25 -1))
(test 1024 (bitwise-arithmetic-shift 1 10))
(test 2 (bitwise-arithmetic-shift 1024 -9))
(test 0 (bitwise-arithmetic-shift 1234567 min-int))
(test (expt 2 30) (bitwise-arithmetic-shift 1 30))

(test -50 (bitwise-arithmetic-shift -25 1))
(test -13 (bitwise-arithmetic-shift -25 -1))
(test -1024 (bitwise-arithmetic-shift -1 10))
(test -2 (bitwise-arithmetic-shift -1024 -9))
(test -1 (bitwise-arithmetic-shift -1234567 min-int))
(test (- (expt 2 30)) (bitwise-arithmetic-shift -1 30))

; bitwise-rotate-bit-field

(test 4 (bitwise-rotate-bit-field 8 1 5 7))
(test 112 (bitwise-rotate-bit-field 76 1 6 2))
(test 324 (bitwise-rotate-bit-field 324 3 6 2))
(test 324 (bitwise-rotate-bit-field 324 1 6 20))
(test 328 (bitwise-rotate-bit-field 324 1 6 16))

; bitwise-reverse-bit-field

(test 2 (bitwise-reverse-bit-field 8 1 4))
(test 8 (bitwise-reverse-bit-field 8 1 6))
(test 88 (bitwise-reverse-bit-field 76 1 6))
(test 324 (bitwise-reverse-bit-field 324 3 6))
(test 336 (bitwise-reverse-bit-field 324 1 6))
(test -106 (bitwise-reverse-bit-field -76 1 6))
(test -354 (bitwise-reverse-bit-field -324 1 6))


; Tests with long values

(when-full-runtime
   (let ((min-long -9223372036854775808)
         (max-long 9223372036854775807)
        )

      (test min-long (bitwise-not max-long))
      (test max-long (bitwise-not min-long))

      (test 63 (bitwise-bit-count max-long))
      (test -64 (bitwise-bit-count min-long))

      (test 63 (bitwise-length max-long))
      (test 63 (bitwise-length min-long))

      (test 0 (bitwise-first-bit-set max-long))
      (test 63 (bitwise-first-bit-set min-long))

      (test #f (bitwise-bit-set? max-long 129))
      (test #t (bitwise-bit-set? min-long 129))

      (test max-long (bitwise-copy-bit -1 63 0))
      (test min-long (bitwise-copy-bit 0 63 1))

      (test 7 (bitwise-bit-field max-long 60 63))
      (test 3 (bitwise-bit-field max-long 61 64))
      (test max-long (bitwise-bit-field max-long 0 100))
      (test min-long (bitwise-bit-field min-long 0 100))
      (test 248 (bitwise-bit-field min-long 60 68))
      (test -1 (bitwise-bit-field min-long 63 200))
      (test -2 (bitwise-bit-field min-long 62 200))

      (test 7 (bitwise-copy-bit-field max-long 3 100 0))
      (test -1 (bitwise-copy-bit-field max-long 3 100 -1))
      (test -1 (bitwise-copy-bit-field min-long 0 100 -1))
      (test min-long (bitwise-copy-bit-field 0 63 200 -1))
      (test min-long (bitwise-copy-bit-field 0 62 200 2))
      (test max-long (bitwise-copy-bit-field -1 62 200 29))

      (test 0 (bitwise-arithmetic-shift 1 64))
      (test (expt 2 62) (bitwise-arithmetic-shift 5 62))
      (test 0 (bitwise-arithmetic-shift -1 64))
      (test 0 (bitwise-arithmetic-shift -2 63))

      (test min-long (bitwise-arithmetic-shift max-long 63))
      (test 0 (bitwise-arithmetic-shift min-long 1))
      (test min-long (bitwise-arithmetic-shift 1 63))
      (test min-long (bitwise-arithmetic-shift 2 62))
      (test min-long (bitwise-arithmetic-shift 3 63))
      (test min-long (bitwise-arithmetic-shift -1 63))
      (test min-long (bitwise-arithmetic-shift -2 62))
      (test min-long (bitwise-arithmetic-shift -3 63))
      (test (expt 2 62) (bitwise-arithmetic-shift 0x5000000000000000 2)) ; overflow
      (test min-long (bitwise-arithmetic-shift 0x5000000000000000 3)) ; overflow

      (test 0x234567890ABCDEF0 (bitwise-rotate-bit-field 0x1234567890ABCDEF 0 72 4))
      (test 0x001234567890ABCD (bitwise-rotate-bit-field 0x1234567890ABCDEF 0 72 64))
      (test 0x01234567890ABCDE (bitwise-rotate-bit-field 0x1234567890ABCDEF 0 72 68))

      (test 0x123456783D59DEF8 (bitwise-reverse-bit-field 0x123456789ABCDEF8 16 32))
      (test 0x12345673D591DEF8 (bitwise-reverse-bit-field 0x123456789ABCDEF8 16 36))
      (test 0x1F7B3D591E6A2C48 (bitwise-reverse-bit-field 0x123456789ABCDEF8 0 64))
      (test 0x7B3D591E6A2C4800 (bitwise-reverse-bit-field 0x123456789ABCDEF8 0 72))
      (test 0x516240009ABCDEF8 (bitwise-reverse-bit-field 0x123456789ABCDEF8 35 72))
      (test -0x516240009ABCDEF8 (bitwise-reverse-bit-field -0x123456789ABCDEF8 35 72))

      (test (bitwise-arithmetic-shift-left 0xFFFFFFFF 32) (bitwise-reverse-bit-field 0xFFFFFFFF 0 64))
      (test (bitwise-arithmetic-shift-left 0xFFFFFFFF 36) (bitwise-reverse-bit-field 0xFFFFFFFF 0 68))
   )
)

; Tests with BigInteger values

(when-full-runtime
   (let* ((min-long-big-int (- (expt 2 63)))
          (max-long-big-int (- (expt 2 63) 1))
          (big-int (expt 2 100))
          (big-int2 (expt 2 200))
          (big-int3 (expt 2 300))
          (random-big-int 764758101480111169132851889184)
          (random-neg-big-int (- random-big-int))
         )

      (test (- -1 big-int) (bitwise-not big-int))
      (test (- big-int 1) (bitwise-not (- big-int)))
      (test min-long-big-int (bitwise-not max-long-big-int))
      (test max-long-big-int (bitwise-not min-long-big-int))
      (test 0 (bitwise-and big-int big-int2 big-int3))
      (test
         (bitwise-not (bitwise-ior big-int big-int2 big-int3))
         (apply bitwise-and (map bitwise-not (list big-int big-int2 big-int3)))
      )
      (test 12 (bitwise-xor big-int big-int 12))
      (test 0 (bitwise-if big-int big-int2 big-int))
      (test
         (bitwise-ior big-int big-int3)
         (bitwise-if big-int (bitwise-ior big-int big-int2 big-int3) big-int3)
      )

      (test 63 (bitwise-bit-count max-long-big-int))
      (test -64 (bitwise-bit-count min-long-big-int))
      (test 1 (bitwise-bit-count big-int))
      (test (bitwise-bit-count big-int2) (bitwise-bit-count big-int3))
      (test -101 (bitwise-bit-count (- big-int)))

      (test 63 (bitwise-length min-long-big-int))
      (test 63 (bitwise-length max-long-big-int))
      (test 101 (bitwise-length big-int))
      (test 100 (bitwise-length (- big-int 1)))
      (test 100 (bitwise-length (- big-int)))

      (test 0 (bitwise-first-bit-set max-long-big-int))
      (test 63 (bitwise-first-bit-set min-long-big-int))
      (test 100 (bitwise-first-bit-set big-int))
      (test 200 (bitwise-first-bit-set big-int2))
      (test (bitwise-first-bit-set big-int3) (bitwise-first-bit-set (* big-int big-int2)))

      (test #f (bitwise-bit-set? max-long-big-int 129))
      (test #t (bitwise-bit-set? min-long-big-int 129))
      (test #f (bitwise-bit-set? big-int 99))
      (test #t (bitwise-bit-set? big-int 100))
      (test #f (bitwise-bit-set? big-int 101))

      (test (+ (expt 2 129) 8) (bitwise-copy-bit (expt 2 3) 129 1))
      (test (- (+ (expt 2 129) 8)) (bitwise-copy-bit (- (expt 2 3)) 129 0))
      (test (* 2 min-long-big-int) (bitwise-copy-bit min-long-big-int 63 0))
      (test (+ (* 2 max-long-big-int) 1) (bitwise-copy-bit max-long-big-int 63 1))
      (test big-int2 (bitwise-copy-bit (bitwise-copy-bit big-int 100 0) 200 1))

      (test 0 (bitwise-bit-field big-int 90 99))
      (test 4 (bitwise-bit-field big-int 98 108))
      (test 1020 (bitwise-bit-field (- big-int) 98 108))
      (test 7 (bitwise-bit-field max-long-big-int 60 63))
      (test 3 (bitwise-bit-field max-long-big-int 61 64))
      (test 0 (bitwise-bit-field min-long-big-int 60 63))
      (test 4 (bitwise-bit-field min-long-big-int 61 64))
      (test 255 (bitwise-bit-field min-long-big-int 63 71))

      (test (- (expt 2 100) 1) (bitwise-copy-bit-field max-long-big-int 3 100 -1))
      (test (- (expt 2 100)) (bitwise-copy-bit-field min-long-big-int 0 100 0))
      (test 0 (bitwise-copy-bit-field big-int 98 101 8))
      (test big-int (bitwise-copy-bit-field big-int 97 100 8))
      (test big-int2 (bitwise-copy-bit-field (bitwise-copy-bit-field big-int 100 101 0) 200 201 5))

      (test 1 (bitwise-arithmetic-shift big-int -100))
      (test big-int (bitwise-arithmetic-shift big-int2 -100))
      (test big-int2 (bitwise-arithmetic-shift-left big-int 100))
      (test 1 (bitwise-arithmetic-shift-right big-int 100))
      (test 0 (bitwise-arithmetic-shift-right big-int 101))
      (test big-int (bitwise-arithmetic-shift-right big-int2 100))

      (test 0 (bitwise-arithmetic-shift big-int min-int))
      (test -1 (bitwise-arithmetic-shift (- big-int) min-int))

      (test (+ (expt 2 64) (expt 2 62)) (bitwise-arithmetic-shift (+ (expt 2 62) (expt 2 60)) 2)) ; no overflow because expt returns BigDecimal
      (test (+ (expt 2 65) (expt 2 63)) (bitwise-arithmetic-shift (+ (expt 2 62) (expt 2 60)) 3)) ; no overflow

      (test big-int (bitwise-rotate-bit-field big-int2 100 201 1))
      (test big-int (bitwise-rotate-bit-field big-int2 100 201 102))
      (test big-int (bitwise-rotate-bit-field big-int 90 110 100))

      (test big-int (bitwise-reverse-bit-field big-int2 100 201))
      (test big-int (bitwise-reverse-bit-field big-int 100 101))
      (test big-int (bitwise-reverse-bit-field big-int 99 102))

      (test random-big-int (bitwise-reverse-bit-field random-big-int 20 21))
      (test random-big-int (bitwise-reverse-bit-field random-big-int 101 200))
      (test random-big-int (bitwise-reverse-bit-field random-big-int 101 102))

      (test 15887999988969706612459967611936 (bitwise-reverse-bit-field random-big-int 72 104))
      (test 1646625750249080197092831396896 (bitwise-reverse-bit-field random-big-int 64 104))
      (test 839653518866932215615554847776 (bitwise-reverse-bit-field random-big-int 66 99))
      (test 802989161936392611291472069664 (bitwise-reverse-bit-field random-big-int 58 99))
      (test 762772068326291060296421213216 (bitwise-reverse-bit-field random-big-int 64 91))
      (test 762736263283593966812816999456 (bitwise-reverse-bit-field random-big-int 56 91))
      (test 764510517250147279373588235296 (bitwise-reverse-bit-field random-big-int 67 96))
      (test 755344428083661249619385386016 (bitwise-reverse-bit-field random-big-int 59 96))
      (test 5409542298781033636942346978336 (bitwise-reverse-bit-field random-big-int 75 104))
      (test 13173006001924033749718503785504 (bitwise-reverse-bit-field random-big-int 67 104))

      (test -15887999988969706612459967611936 (bitwise-reverse-bit-field random-neg-big-int 72 104))
      (test -1646625750249080197092831396896 (bitwise-reverse-bit-field random-neg-big-int 64 104))
      (test -839653518866932215615554847776 (bitwise-reverse-bit-field random-neg-big-int 66 99))
      (test -802989161936392611291472069664 (bitwise-reverse-bit-field random-neg-big-int 58 99))
      (test -762772068326291060296421213216 (bitwise-reverse-bit-field random-neg-big-int 64 91))
      (test -762736263283593966812816999456 (bitwise-reverse-bit-field random-neg-big-int 56 91))
      (test -764510517250147279373588235296 (bitwise-reverse-bit-field random-neg-big-int 67 96))
      (test -755344428083661249619385386016 (bitwise-reverse-bit-field random-neg-big-int 59 96))
      (test -5409542298781033636942346978336 (bitwise-reverse-bit-field random-neg-big-int 75 104))
      (test -13173006001924033749718503785504 (bitwise-reverse-bit-field random-neg-big-int 67 104))
   )
)

; Tests with int values

(when-limited-runtime
   (test min-int (bitwise-not max-int))
   (test max-int (bitwise-not min-int))

   (test 31 (bitwise-bit-count max-int))
   (test -32 (bitwise-bit-count min-int))

   (test 31 (bitwise-length max-int))
   (test 31 (bitwise-length min-int))

   (test 0 (bitwise-first-bit-set max-int))
   (test 31 (bitwise-first-bit-set min-int))

   (test #f (bitwise-bit-set? max-int 129))
   (test #t (bitwise-bit-set? min-int 129))

   (test max-int (bitwise-copy-bit -1 31 0))
   (test min-int (bitwise-copy-bit 0 31 1))

   (test 7 (bitwise-bit-field max-int 28 31))
   (test 3 (bitwise-bit-field max-int 29 32))
   (test max-int (bitwise-bit-field max-int 0 100))

   (test 248 (bitwise-bit-field min-int 28 36))
   (test -1 (bitwise-bit-field min-int 31 200))
   (test -2 (bitwise-bit-field min-int 30 200))

   (test 7 (bitwise-copy-bit-field max-int 3 100 0))
   (test -1 (bitwise-copy-bit-field max-int 3 100 -1))
   (test -1 (bitwise-copy-bit-field min-int 0 100 -1))

   (test min-int (bitwise-copy-bit-field 0 31 200 -1))
   (test min-int (bitwise-copy-bit-field 0 30 200 2))
   (test max-int (bitwise-copy-bit-field -1 30 200 29))

   (test 0 (bitwise-arithmetic-shift 1 32))
   (test (expt 2 30) (bitwise-arithmetic-shift 5 30))
   (test 0 (bitwise-arithmetic-shift -1 32))
   (test 0 (bitwise-arithmetic-shift -2 31))

   (test min-int (bitwise-arithmetic-shift max-int 31))
   (test min-int (bitwise-arithmetic-shift 1 31))

   (test 0 (bitwise-arithmetic-shift min-int 1))
   (test min-int (bitwise-arithmetic-shift -1 31))

   (test 0 (bitwise-arithmetic-shift 1 32))
   (test min-int (bitwise-arithmetic-shift 1 31))
   (test min-int (bitwise-arithmetic-shift 2 30))
   (test min-int (bitwise-arithmetic-shift 3 31))
   (test (expt 2 30) (bitwise-arithmetic-shift 5 30))

   (test 0 (bitwise-arithmetic-shift -1 32))
   (test min-int (bitwise-arithmetic-shift -1 31))
   (test min-int (bitwise-arithmetic-shift -2 30))
   (test 0 (bitwise-arithmetic-shift -2 31))
   (test min-int (bitwise-arithmetic-shift -3 31))

   (test 0 (bitwise-arithmetic-shift 1234567 min-int))
   (test -1 (bitwise-arithmetic-shift -1234567 min-int))
   (test 0 (bitwise-arithmetic-shift 1 max-int))
   (test 0 (bitwise-arithmetic-shift -1 max-int))

   (test 4 (bitwise-arithmetic-shift-left (+ (expt 2 30) 1) 2))
   (test -2147483646 (bitwise-arithmetic-shift-left (+ (expt 2 30) 1) 1))

   (test 0x23456780 (bitwise-rotate-bit-field 0x12345678 0 40 4))
   (test 0x00123456 (bitwise-rotate-bit-field 0x12345678 0 40 32))
   (test 0x01234567 (bitwise-rotate-bit-field 0x12345678 0 40 36))

   (test 0x2c485678 (bitwise-reverse-bit-field 0x12345678 16 32))
   (test 0x1228b678 (bitwise-reverse-bit-field 0x12345678 11 23))
   (test 0x1E6A2C48 (bitwise-reverse-bit-field 0x12345678 0 32))
   (test -2147199368 (bitwise-reverse-bit-field 0x12345678 20 40))
   (test 0x48005678 (bitwise-reverse-bit-field 0x12345678 18 38))
   (test -0x48005678 (bitwise-reverse-bit-field -0x12345678 18 38))

   (test 0x7FFF8000 (bitwise-reverse-bit-field 0xFFFF 0 31))
   (test (bitwise-arithmetic-shift-left 0xFFFF 16) (bitwise-reverse-bit-field 0xFFFF 0 32))
   (test (bitwise-arithmetic-shift-left 0xFFFF 20) (bitwise-reverse-bit-field 0xFFFF 0 36))
   (test 0 (bitwise-reverse-bit-field max-int 0 64))
)

; #13 Hashtables

(define htabs
   (vector (make-hashtable)
      (make-eq-hashtable)
      (make-eqv-hashtable)
      (make-hashtable string-hash string=?)
   )
)
(define temp '())

(test #f (hashtable? '()))
(test #f (hashtable? a))
(test #f (hashtable? '(1 2 3)))
(test #f (hashtable? #(1 2 3)))

(for-each
   (lambda (htab)
      (let ((hcopy (hashtable-copy htab))
            (hcopy2 '()))
         ; test basic operations
         (test #t (hashtable? htab))
         (test 0 (hashtable-size htab))
         (test #() (hashtable-keys htab))
         (test #() (hashtable-values htab))
         (call-with-values
            (lambda () (hashtable-entries htab))
            (lambda (keys vals) (test #() keys) (test #() vals))
         )
         (test 10 (hashtable-ref htab "one" 10))
         (test '() (hashtable-ref htab "one"))

         (test '() (hashtable-update! htab "one" (lambda (n) (+ n 1)) 100)) ; {"one":101}
         (test 1 (hashtable-size htab))
         (test 101 (hashtable-ref htab "one" 0))
         (test 101 (hashtable-ref htab "one"))
         (test #("one") (hashtable-keys htab))
         (test #(101) (hashtable-values htab))
         (call-with-values
            (lambda () (hashtable-entries htab))
            (lambda (keys vals) (test #("one") keys) (test #(101) vals))
         )

         (test 101 (hashtable-set! htab "one" 1)) ; {"one":1}
         (test '() (hashtable-set! htab "2" 'two)) ; {"one":1, 2:'two}
         (test 2 (hashtable-size htab))

         (test 1 (hashtable-ref htab "one" 0))
         (test 0 (hashtable-ref htab "two" 0))
         (test 1 (hashtable-set! htab "one" "ONE")) ; {"one":"ONE", 2:'two}

         (test #f (hashtable-contains? htab "ONE"))
         (test #t (hashtable-contains? htab "one"))

         (test '() (hashtable-delete! htab "two")) ; {"one":"ONE", 2:'two}
         (test 'two (hashtable-delete! htab "2")) ; {"one":"ONE"}
         (test 1 (hashtable-size htab))

         (test '() (hashtable-set! htab "two" 1)) ; {"one":"ONE", "two":1}
         (test 1 (hashtable-ref htab "two" 0))
         (test 1 (hashtable-update! htab "two" (lambda (n) (+ n 1)) 10)) ; {"one":"ONE", "two":2}
         (test 2 (hashtable-ref htab "two" 0))

         (test #t (hashtable-mutable? htab))
         (test #f (hashtable-mutable? hcopy))
         (test 0 (hashtable-size hcopy))

         ; test immutable copy of original mutable hashtable
         (set! hcopy (hashtable-copy htab #f)) ; {"one":"ONE", "two":2}
         (test #f (hashtable-mutable? hcopy))
         (test 2 (hashtable-size hcopy))
         (test #t (hashtable-contains? hcopy "one"))
         (test #t (hashtable-contains? hcopy "two"))
         (test #f (hashtable-contains? hcopy "three"))
         (test "ONE" (hashtable-ref hcopy "one" 0))
         (test 2 (hashtable-ref hcopy "two" 0))
         (test 0 (hashtable-ref hcopy "three" 0))
         (test (hashtable-equivalence-function htab) (hashtable-equivalence-function hcopy))
         (test (hashtable-hash-function htab) (hashtable-hash-function hcopy))

         ; test immutable copy of an immutable copy
         (set! hcopy2 (hashtable-copy hcopy)) ; {"one":"ONE", "two":2}
         (test #f (hashtable-mutable? hcopy2))
         (test 2 (hashtable-size hcopy2))
         (test #("one" "two") (vector-sort! (hashtable-keys hcopy2) string<?))
         (test (hashtable-equivalence-function htab) (hashtable-equivalence-function hcopy2))
         (test (hashtable-hash-function htab) (hashtable-hash-function hcopy2))

         ; test mutable copy of an immutable copy
         (set! hcopy2 (hashtable-copy hcopy #t)) ; {"one":"ONE", "two":2}
         (test #t (hashtable-mutable? hcopy2))
         (test 2 (hashtable-size hcopy2))
         (test 2 (hashtable-ref hcopy2 "two" 0))
         (test 2 (hashtable-set! hcopy2 "two" 222)) ; {"one":"ONE", "two":222}
         (test 222 (hashtable-ref hcopy2 "two" 0))
         (test 2 (hashtable-ref htab "two" 0))
         (test 2 (hashtable-ref hcopy "two" 0))
         (test 222 (hashtable-delete! hcopy2 "two")) ; {"one":"ONE"}
         (test (hashtable-size htab) (+ 1 (hashtable-size hcopy2)))
         (test (hashtable-contains? hcopy "two") (not (hashtable-contains? hcopy2 "two")))
         (test '() (hashtable-clear! hcopy2)) ; {}
         (test (hashtable-size htab) (+ 2 (hashtable-size hcopy2)))
         (test (hashtable-size hcopy) (+ 2 (hashtable-size hcopy2)))
         (test #() (hashtable-keys hcopy2))
         (test (hashtable-equivalence-function htab) (hashtable-equivalence-function hcopy2))
         (test (hashtable-hash-function htab) (hashtable-hash-function hcopy2))

         (set! hcopy2 (hashtable-copy htab #t)) ; {"one":"ONE", "two":2}
         (test #t (hashtable-mutable? hcopy2))
         (test 2 (hashtable-size hcopy2))
         (test '() (hashtable-set! hcopy2 "One" '())) ; {"one":"ONE", "two":2, "One":()}
         (test 3 (hashtable-size hcopy2))
         (test 2 (hashtable-size htab))
         (test #t (hashtable-contains? hcopy2 "One"))
         (test #f (hashtable-contains? htab "One"))
         (test '() (hashtable-ref hcopy2 "One" "not found"))
         (test '() (hashtable-update! hcopy2 "One" (lambda (ls) (cons 1 ls)) '(2)))
         (test '(1) (hashtable-ref hcopy2 "One" "not found"))

         ; test that -keys and -entries operations return the right data
         (test #("One" "one" "two") (vector-sort! (hashtable-keys hcopy2) string<?))
         (test #((1) "ONE" 2) (vector-sort! (hashtable-values hcopy2)
            (lambda (left right) (or (pair? left) (and (string? left) (number? right))))))
         (test #("one" "two") (vector-sort! (hashtable-keys htab) string<?))
         (test #("ONE" 2) (vector-sort! (hashtable-values htab) (lambda (left right) (string? left))))

         (let-values (((keys vals) (hashtable-entries hcopy2)))
            (test (hashtable-ref hcopy2 (vector-ref keys 0) "not found") (vector-ref vals 0))
            (test (hashtable-ref hcopy2 (vector-ref keys 1) "not found") (vector-ref vals 1))
            (test (hashtable-ref hcopy2 (vector-ref keys 2) "not found") (vector-ref vals 2))
            (test (vector-length keys) (vector-length vals))
            (test #("One" "one" "two") (vector-sort! keys string<?))
         )
         (let-values (((keys vals) (hashtable-entries htab)))
            (test (hashtable-ref hcopy2 (vector-ref keys 0) "not found") (vector-ref vals 0))
            (test (hashtable-ref hcopy2 (vector-ref keys 1) "not found") (vector-ref vals 1))
            (test (vector-length keys) (vector-length vals))
            (test #("one" "two") (vector-sort! keys string<?))
         )

         (test '() (hashtable-clear! hcopy2 8))
         (test (hashtable-size htab) (+ 2 (hashtable-size hcopy2)))
      )
   )
   htabs
)

; test with other datatypes as keys
(define o1 '(1 2))
(define o2 '(1 2))
(define o3 '(1 2 3))
(define b1 #f)
(define b2 #f)
(define n1 1)
(define sy1 (string->symbol "1"))
(define sy2 'a)
(define st1 "1")
(define st2 "a")
(define nil '())

(define htab (htabs 0))
(test equal? (hashtable-equivalence-function htab))
(test #f (hashtable-hash-function htab))
(test '() (hashtable-set! htab o1 "one"))
(test "one" (hashtable-set! htab o2 "two"))
(test 3 (hashtable-size htab))
(test '() (hashtable-set! htab o3 "three"))
(test 4 (hashtable-size htab))

(test "three" (hashtable-set! htab '(1 2 3) "list"))
(test 4 (hashtable-size htab))
(test "list" (hashtable-ref htab '(1 2 3) "not found"))
(test "not found" (hashtable-ref htab #(1 2 3) "not found"))
(test "list" (hashtable-delete! htab '(1 2 3)))
(test 3 (hashtable-size htab))

(test '() (hashtable-set! htab b1 "boolean"))
(test "boolean" (hashtable-ref htab b2 "not found"))
(test "not found" (hashtable-ref htab #t "not found"))
(test 4 (hashtable-size htab))

(test '() (hashtable-set! htab n1 123))
(test '() (hashtable-set! htab sy1 'symbol))
(test '() (hashtable-set! htab st1 "string"))
(test '() (hashtable-set! htab sy2 'symbol2))
(test '() (hashtable-set! htab st2 "string2"))
(test 9 (hashtable-size htab))
(test 123 (hashtable-ref htab n1 "not found"))
(test 'symbol (hashtable-ref htab sy1 "not found"))
(test "string" (hashtable-ref htab st1 "not found"))
(test 'symbol2 (hashtable-ref htab sy2 "not found"))
(test "string2" (hashtable-ref htab st2 "not found"))

(test '() (hashtable-set! htab nil "null key"))
(test 10 (hashtable-size htab))
(test #t (hashtable-contains? htab nil))
(test #t (hashtable-contains? htab '()))
(test "null key" (hashtable-ref htab nil "not found"))
(test "null key" (hashtable-ref htab '() "not found"))
(test #f (equal? #f (member '() (vector->list (hashtable-keys htab)))))
(test "null key" (hashtable-delete! htab '()))
(test "not found" (hashtable-ref htab nil "not found"))


(set! htab (htabs 1))
(test eq? (hashtable-equivalence-function htab))
(test #f (hashtable-hash-function htab))
(test '() (hashtable-set! htab o1 "one"))
(test '() (hashtable-set! htab o2 "two"))
(test '() (hashtable-set! htab o3 "three"))
(test 5 (hashtable-size htab))

(test "new object" (hashtable-ref htab '(1 2 3) "new object"))
(test "three" (hashtable-ref htab o3 "not found"))
(test "not found" (hashtable-ref htab '(1 2 3) "not found"))
(test "not found" (hashtable-ref htab #(1 2 3) "not found"))
(test '() (hashtable-delete! htab '(1 2 3)))
(test "three" (hashtable-delete! htab o3))
(test 4 (hashtable-size htab))

(test '() (hashtable-set! htab b1 "boolean"))
(test "boolean" (hashtable-ref htab b2 "not found"))
(test "not found" (hashtable-ref htab #t "not found"))

(test '() (hashtable-set! htab n1 123))
(test '() (hashtable-set! htab sy1 'symbol))
(test '() (hashtable-set! htab st1 "string"))
(test '() (hashtable-set! htab sy2 'symbol2))
(test '() (hashtable-set! htab st2 "string2"))
(test 10 (hashtable-size htab))
(test 123 (hashtable-ref htab n1 "not found"))
(test 'symbol (hashtable-ref htab sy1 "not found"))
(test "string" (hashtable-ref htab st1 "not found"))
(test 'symbol2 (hashtable-ref htab sy2 "not found"))
(test "string2" (hashtable-ref htab st2 "not found"))

(test '() (hashtable-set! htab nil "null key"))
(test 11 (hashtable-size htab))
(test #t (hashtable-contains? htab nil))
(test #t (hashtable-contains? htab '()))
(test "null key" (hashtable-ref htab nil "not found"))
(test "null key" (hashtable-ref htab '() "not found"))
(test #f (equal? #f (member '() (vector->list (hashtable-keys htab)))))
(test "null key" (hashtable-delete! htab '()))
(test "not found" (hashtable-ref htab nil "not found"))


(set! htab (htabs 2))
(test eqv? (hashtable-equivalence-function htab))
(test #f (hashtable-hash-function htab))
(test '() (hashtable-set! htab o1 "one"))
(test '() (hashtable-set! htab o2 "two"))
(test 4 (hashtable-size htab))

(test '() (hashtable-set! htab b1 "boolean"))
(test "boolean" (hashtable-ref htab b2 "not found"))
(test "not found" (hashtable-ref htab #t "not found"))

(test '() (hashtable-set! htab n1 123))
(test '() (hashtable-set! htab sy1 'symbol))
(test '() (hashtable-set! htab st1 "string"))
(test '() (hashtable-set! htab sy2 'symbol2))
(test '() (hashtable-set! htab st2 "string2"))
(test 10 (hashtable-size htab))
(test 123 (hashtable-ref htab n1 "not found"))
(test 'symbol (hashtable-ref htab sy1 "not found"))
(test "string" (hashtable-ref htab st1 "not found"))
(test 'symbol2 (hashtable-ref htab sy2 "not found"))
(test "string2" (hashtable-ref htab st2 "not found"))

(test '() (hashtable-set! htab nil "null key"))
(test 11 (hashtable-size htab))
(test #t (hashtable-contains? htab nil))
(test #t (hashtable-contains? htab '()))
(test "null key" (hashtable-ref htab nil "not found"))
(test "null key" (hashtable-ref htab '() "not found"))
(test #f (equal? #f (member '() (vector->list (hashtable-keys htab)))))
(test "null key" (hashtable-delete! htab '()))
(test "not found" (hashtable-ref htab nil "not found"))


(define s1 "obj")
(define s2 "obj")
(define s3 "OBJ")

(set! htab (htabs 3))
(test string=? (hashtable-equivalence-function htab))
(test string-hash (hashtable-hash-function htab))
(test '() (hashtable-set! htab s1 "one"))
(test "one" (hashtable-set! htab s2 "two"))
(test '() (hashtable-set! htab s3 "three"))
(test 4 (hashtable-size htab))

(set! htab (make-hashtable string-ci-hash string-ci=?))
(test string-ci=? (hashtable-equivalence-function htab))
(test string-ci-hash (hashtable-hash-function htab))
(test '() (hashtable-set! htab "ONE" 1))
(test 1 (hashtable-ref htab "one" 0))
(test '() (hashtable-set! htab s1 "one"))
(test "one" (hashtable-set! htab s2 "two"))
(test "two" (hashtable-set! htab s3 "three"))
(test 2 (hashtable-size htab))
(test #("obj" "ONE") (vector-sort! (hashtable-keys htab) string-ci<?))

(define symbol-equal? (lambda (left right) (string=? (symbol->string left) (symbol->string right))))
(set! htab (make-hashtable symbol-hash symbol-equal? 12))
(test symbol-equal? (hashtable-equivalence-function htab))
(test symbol-hash (hashtable-hash-function htab))
(test '() (hashtable-set! htab 'one 1))
(test '() (hashtable-set! htab 'two 2))
(test '() (hashtable-set! htab 'One 10))
(test 2 (hashtable-set! htab 'two 20))
(test 3 (hashtable-size htab))


(test #t (number? (equal-hash 1234)))
(test #t (number? (equal-hash 1234.567)))
(test #t (number? (equal-hash "testString")))
(test #t (number? (equal-hash 'testString)))
(test #t (number? (equal-hash #t)))
(test #t (number? (equal-hash (< 3 2))))
(test #t (number? (equal-hash '(1 . 2))))
(test #t (number? (equal-hash '(1 2 3 4))))
(test #t (number? (equal-hash #(1 2 3 4))))
(test #t (number? (equal-hash (collection 1 2 3 4))))
(test #t (number? (equal-hash htab)))
(test #t (number? (equal-hash (hashtable-copy htab))))

(test #t (number? (string-hash "testString")))
(test #t (number? (string-ci-hash "testString")))
(test (string-ci-hash "TESTSTRING") (string-ci-hash "testString"))
(test #t (number? (symbol-hash 'testString)))


; NexJ extension: Hashset

(define hsets
   (vector (make-hashset)
      (make-eq-hashset)
      (make-eqv-hashset)
      (make-hashset equal-hash equal?)
   )
)

(for-each
   (lambda (hset)
      (let ((hcopy '())
            (hcopy2 '())
            (vect '()))
         (test #t (hashset? hset))
         (test #f (hashset? (make-hashtable)))
         (test #f (hashset? '(1 2 3)))
         (test #f (hashset? #(1 2 3)))
         (test #f (hashset? #vu8(1 2 3)))

         (test 0 (hashset-size hset))
         (test #t (hashset-add! hset 'one))
         (test 1 (hashset-size hset))
         (test #t (hashset-add! hset 2))
         (test 2 (hashset-size hset))
         (test #f (hashset-add! hset 'one))
         (test 2 (hashset-size hset))

         (set! hcopy (hashset-copy hset))
         (test #f (hashset-mutable? hcopy))
         (set! hcopy (hashset-copy hset #f))
         (test #f (hashset-mutable? hcopy))
         (test #t (hashset-contains? hcopy 'one))
         (test #f (hashset-contains? hcopy "one"))
         (test #f (hashset-contains? hcopy 1))
         (test #t (hashset-contains? hcopy 2))

         (set! hcopy2 (hashset-copy hcopy))
         (test #f (hashset-mutable? hcopy2))
         (set! hcopy2 (hashset-copy hcopy #f))
         (test #f (hashset-mutable? hcopy2))
         (set! hcopy2 (hashset-copy hcopy #t))
         (test #t (hashset-mutable? hcopy2))

         (set! hcopy (hashset-copy hset #t))
         (test #t (hashset-mutable? hcopy))
         (test #f (hashset-add! hcopy 'one))
         (test #t (hashset-add! hcopy "one"))
         (test 3 (hashset-size hcopy))
         (test 2 (hashset-size hset))
         (test #t (hashset-remove! hcopy 2))
         (test 2 (hashset-size hcopy))
         (test 2 (hashset-size hset))

         (set! vect (hashset-values hset))
         (test #t (or (equal? vect #(one 2)) (equal? vect #(2 one))))

         (test #f (hashset-remove! hset "one"))
         (test 2 (hashset-size hset))
         (test #t (hashset-remove! hset 2))
         (test 1 (hashset-size hset))
         (test '() (hashset-clear! hset 12))
         (test 0 (hashset-size hset))
         (test #f (hashset-remove! hset 'one))
         (test 0 (hashset-size hset))

         (test 2 (hashset-size hcopy))
         (test '() (hashset-clear! hcopy))
         (test 0 (hashset-size hcopy))
         (test #f (hashset-contains? hcopy 'one))
         (test #f (hashset-contains? hcopy 2))
         (test #() (hashset-values hcopy))
      )
   )
   hsets
)

(define hset (hsets 0))
(test equal? (hashset-equivalence-function hset))
(test #f (hashset-hash-function hset))
(test #t (hashset-add! hset o1))
(test 1 (hashset-size hset))
(test #f (hashset-add! hset o2))
(test 1 (hashset-size hset))
(test #t (hashset-add! hset o3))
(test 2 (hashset-size hset))
(test #t (hashset-remove! hset o2))
(test 1 (hashset-size hset))
(test #f (hashset-contains? hset o1))

(set! hset (hsets 1))
(test eq? (hashset-equivalence-function hset))
(test #f (hashset-hash-function hset))
(test #t (hashset-add! hset o1))
(test 1 (hashset-size hset))
(test #t (hashset-add! hset o2))
(test 2 (hashset-size hset))
(test #t (hashset-add! hset o3))
(test 3 (hashset-size hset))
(test #t (hashset-remove! hset o2))
(test 2 (hashset-size hset))
(test #t (hashset-contains? hset o1))

(set! hset (hsets 2))
(test eqv? (hashset-equivalence-function hset))
(test #f (hashset-hash-function hset))
(test #t (hashset-add! hset o1))
(test 1 (hashset-size hset))
(test #t (hashset-add! hset o2))
(test 2 (hashset-size hset))
(test #t (hashset-add! hset o3))
(test 3 (hashset-size hset))
(test #t (hashset-remove! hset o2))
(test 2 (hashset-size hset))
(test #t (hashset-contains? hset o1))

(set! hset (hsets 3))
(test equal? (hashset-equivalence-function hset))
(test equal-hash (hashset-hash-function hset))

(set! hset (make-hashset string-ci-hash string-ci=?))
(test string-ci=? (hashset-equivalence-function hset))
(test string-ci-hash (hashset-hash-function hset))
(test #t (hashset-add! hset s1))
(test 1 (hashset-size hset))
(test #f (hashset-add! hset s2))
(test 1 (hashset-size hset))
(test #f (hashset-add! hset s3))
(test 1 (hashset-size hset))
(test #t (hashset-remove! hset s2))
(test 0 (hashset-size hset))
(test #f (hashset-contains? hset s1))


; #14 Enumerations

(for-each
   (lambda (dupList symList)
      (let*
         (
            (vect (list->vector symList))
            (length (vector-length vect))
            (masterSet (make-enumeration dupList))
            (constructor (enum-set-constructor masterSet))
            (indexer (enum-set-indexer masterSet))
            (emptySet (constructor '()))
            (subset (constructor (list (vect 0) (vect 1))))
            (superset (constructor (list (vect 5) (vect 1) (vect 0))))
            (difference (constructor (list (vect 5))))
            (complement (constructor (cddr symList)))
         )
         (test #t (eq? masterSet (enum-set-complement emptySet)))
         (test emptySet (enum-set-complement masterSet))
         (test complement (enum-set-complement subset))
         (test (enum-set->list complement) (enum-set->list (enum-set-complement subset)))

         (test #t (eq? masterSet (constructor (reverse dupList))))

         (test #t (eq? emptySet (enum-set-difference emptySet subset)))
         (test emptySet (enum-set-difference subset subset))
         (test complement (enum-set-difference masterSet subset))
         (test subset (enum-set-difference masterSet complement))
         (test subset (enum-set-difference subset complement))
         (test emptySet (enum-set-difference subset superset))
         (test difference (enum-set-difference superset subset))

         (test subset (enum-set-intersection subset superset))
         (test difference (enum-set-intersection complement superset))

         (test symList (enum-set->list masterSet))
         (test '() (enum-set->list emptySet))
         (test (list (vect 0) (vect 1)) (enum-set->list subset))
         (test (cddr symList) (enum-set->list complement))

         (test #t (enum-set-subset? subset superset))
         (test #f (enum-set-subset? subset complement))

         (test #t (eq? masterSet (enum-set-union subset complement)))
         (test #t (eq? masterSet (enum-set-union superset complement)))
         (test complement (enum-set-union (enum-set-complement superset) difference))

         (for ((i 0)) (< i length) (set! i (+ i 1))
            (test i (indexer (vect i)))
            (test #t (enum-set-member? (vect i) masterSet))
         )

         (test #t (enum-set-member? (vect 0) subset))
         (test #t (enum-set-member? (vect 1) subset))
         (test #f (enum-set-member? (vect 5) subset))

         (for-each
            (lambda (set)
               (test #f (indexer 'notAnElement))
               (test #f (enum-set-member? 'notAnElement set))
               (test #t (eq? masterSet (enum-set-universe set)))
               (test #t (enum-set-subset? set masterSet))
               (test #t (enum-set-subset? emptySet set))
               (test (enum-set=? set masterSet) (enum-set-subset? masterSet set))
               (test (enum-set=? set emptySet) (enum-set-subset? set emptySet))
               (test #t (eq? set (enum-set-projection set subset)))
               (test #t (eq? masterSet (enum-set-union set masterSet)))
               (test #t (eq? set (enum-set-union set emptySet)))
               (test #t (eq? set (enum-set-intersection set masterSet)))
               (test #t (eq? emptySet (enum-set-intersection set emptySet)))
               (test emptySet (enum-set-difference set masterSet))
               (test (enum-set-complement set) (enum-set-difference masterSet set))
               (test #t (eq? set (enum-set-difference set emptySet)))
               (test #t (eq? emptySet (enum-set-difference emptySet set)))
            )
            (vector masterSet emptySet subset superset complement)
         )
      )
   )
   '(
      (a0 a1 a0 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a9 a15 a16 a17 a18 a19 a20 a21 a9 a22 a23 a24 a21 a25 a26 a12 a24)
      (a0 a1 a0 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a9 a15 a16 a17 a18 a19 a20 a21 a9 a22 a23 a24 a21 a25 a26 a27 a28 a12 a24 a29 a30 a31)
      (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
       b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31
       a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
       b21 b9 b19 b8 b6 b1 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 b21 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c9
      )
      (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
       b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31
       c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31
      )
   )
   '(
      (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26)
      (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31)
      (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
       b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31
       c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21
      )
      (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
       b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31
       c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31
      )
   )
)


(define-enumeration month (jan feb mar feb jan apr may jun jul may aug sep aug sep oct nov dec dec jan) month-constructor)
(test 'aug (month aug))
(test 'sep (month sep))

(define schoolyear (month-constructor sep oct nov dec jan feb mar apr sep oct nov))
(define summer (month-constructor aug jul aug jun aug may aug aug))
(define months (month-constructor jan aug sep mar))
(define year (enum-set-universe summer))

(test '(jan feb mar apr sep oct nov dec) (enum-set->list schoolyear))
(test '(may jun jul aug) (enum-set->list summer))
(test '(jan mar aug sep) (enum-set->list months))

(test summer (enum-set-complement schoolyear))
(test summer (enum-set-complement (enum-set-complement summer)))
(test #t (eq? year (enum-set-union summer schoolyear)))
(test '(jan feb mar apr may jun jul aug sep oct nov dec) (enum-set->list (enum-set-union summer schoolyear)))
(test '(jan mar may jun jul aug sep) (enum-set->list (enum-set-union summer months)))
(test (month-constructor aug) (enum-set-intersection summer months))
(test (month-constructor) (enum-set-intersection summer schoolyear))
(test '(may jun jul) (enum-set->list (enum-set-difference summer months)))
(test '(jan mar sep) (enum-set->list (enum-set-difference months summer)))

(define-enumeration month2 (dec nov oct sep aug jul jun may apr mar feb jan) month-constructor2)
(define-enumeration month3 (jan feb mar apr may jun jul aug sep oct nov dec fak) month-constructor3)
(define months2 (month-constructor2 mar jan sep aug))
(define months3 (month-constructor3 mar jan sep aug))

(test #t (enum-set-subset? months months2))
(test #t (enum-set-subset? months2 months))
(test #t (enum-set-subset? months months3))
(test #f (enum-set-subset? months3 months))
(test #t (enum-set=? months months2))
(test #f (enum-set=? months months3))

(define months2 (month-constructor2 jan feb mar aug sep))
(define months3 (month-constructor3 jan feb mar aug sep))

(test #t (enum-set-subset? months months2))
(test #f (enum-set-subset? months2 months))
(test #t (enum-set-subset? months months3))
(test #f (enum-set-subset? months3 months))
(test #f (enum-set=? months months2))
(test #f (enum-set=? months months3))

(define-enumeration month4 (aug sep oct nov dec jan feb march april may june july) month-constructor4)
(define months4 (enum-set-projection year (month-constructor4)))
(define months (enum-set-projection months4 months))

(test (month-constructor4 jan feb may aug sep oct nov dec) months4)
(test '(aug sep oct nov dec jan feb may) (enum-set->list months4))
(test #f (enum-set=? months months4))
(test '(jan feb may aug sep oct nov dec) (enum-set->list months))
(test (month-constructor4) (enum-set-projection (month-constructor mar apr jun jul) months4))


; Pretty-printing

(when-full-runtime
   (let
      (
         (writer (open-output-formatter '() 40 10 3))
         (exprs
            #(
               12345
               "this string is ....... 41 characters long"
               thisSymbolIs.............41charactersLong
               (a b c d e)
               #(a b c d e)
               #(aaa bbbb ccccc dddddd eeeeeeeeeeeeeeee)
               #(aaa bbbb (ccccccccccccccccc ddddddddddddddddd) eeeeeeeeeeeeeeee)
               #(aaa bbbb (cccccccccccccccccc ddddddddddddddddd) eeeeeeeeeeeeeeee)
               #(aaa bbbb #(ccccccccccccccccc ddddddddddddddddd) eeeeeeeeeeeeeeee)
               `(a b '(c d) (e ,'f ,@'(g ##(h i)) j) k)
               `(a b '(c d) (e ,'f ,@'(gggggggggggggggggggggggggggggggggggg ##(h i)) j) k l)
               `(a b '(c d) (e ,'f ,@'(g ##(hhhhhhhhhhhhhhhhhhhhh i)) j) k l)
               `(a b '(c d) (e ,'f ,@'(g ##(hhhhhhhhhhhhhhhhhhhhhh i)) j) k l)
               `(a b '(c d) (e ,'f ,@'(g ##(((hhhhhhhhhh i)))) j) k l)
               `(a b '(c d) (e ,'f ,@'(g ##(((hhhhhhhhhhh i)))) j) k l)
               (((((((((((((((aaaaaaaaa)))))))))))))))
               ((((((((((((((((aaaaaaaaa))))))))))))))))
               ((((((((((((((((((((aaaaa))))))))))))))))))))
               ((((((((((((((((((((aaaaa bb))))))))))))))))))))
               (((((((((((((((aaaaaaa bb)))))))))))))))
            )
         )
         (strs
            #(
               "12345"
               "\"this string is ....... 41 characters long\""
               "thisSymbolIs.............41charactersLong"
               "(a b c d e)"
               "#(a b c d e)"
               "#(\n   aaa\n   bbbb\n   ccccc\n   dddddd\n   eeeeeeeeeeeeeeee\n)"
               "#(\n   aaa\n   bbbb\n   (ccccccccccccccccc ddddddddddddddddd)\n   eeeeeeeeeeeeeeee\n)"
               "#(\n   aaa\n   bbbb\n   (cccccccccccccccccc\n      ddddddddddddddddd\n   )\n   eeeeeeeeeeeeeeee\n)"
               "#(\n   aaa\n   bbbb\n   #(\n      ccccccccccccccccc\n      ddddddddddddddddd\n   )\n   eeeeeeeeeeeeeeee\n)"
               "`(a b '(c d) (e ,'f ,@'(g ##(h i)) j) k)"
               "`(a\n   b\n   '(c d)\n   (e\n      ,'f\n      ,@'(gggggggggggggggggggggggggggggggggggg\n         ##(h i)\n      )\n      j\n   )\n   k\n   l\n)"
               "`(a\n   b\n   '(c d)\n   (e\n      ,'f\n      ,@'(g ##(hhhhhhhhhhhhhhhhhhhhh i))\n      j\n   )\n   k\n   l\n)"
               "`(a\n   b\n   '(c d)\n   (e\n      ,'f\n      ,@'(g\n         ##(hhhhhhhhhhhhhhhhhhhhhh i)\n      )\n      j\n   )\n   k\n   l\n)"
               "`(a\n   b\n   '(c d)\n   (e ,'f ,@'(g ##(((hhhhhhhhhh i)))) j)\n   k\n   l\n)"
               "`(a\n   b\n   '(c d)\n   (e\n      ,'f\n      ,@'(g ##(((hhhhhhhhhhh i))))\n      j\n   )\n   k\n   l\n)"
               "(((((((((((((((aaaaaaaaa)))))))))))))))"
               "((((((((((((((((aaaaaaaaa))))))))))))))))"
               "((((((((((((((((((((aaaaa))))))))))))))))))))"
               "((((((((((((((((((((aaaaa bb))))))))))))))))))))"
               "(((((((((((((((aaaaaaa bb)))))))))))))))"
            )
         )
      )

      (for-each
         (lambda (expr str)
            (let ((w (open-output-formatter () 40 10 3 "\n")))
               (write expr w)
               (test str (get-string-all w))
            )
         )
         exprs
         strs
      )

      (writer'lineSeparator "\n")
      (write (exprs 0) writer)
      (newline writer)
      (write (exprs 6) writer)
      (newline writer)
      (write (exprs 1) writer)
      (write (exprs 2) writer)
      (newline writer)
      (newline writer)

      (test
         (string-append (strs 0) "\n" (strs 6) "\n" (strs 1) (strs 2) "\n\n")
         (get-string-all writer)
      )

      (write (exprs 5) writer)
      (newline writer)
      (write (exprs 7) writer)

      (test
         (string-append (strs 0) "\n" (strs 6) "\n" (strs 1) (strs 2) "\n\n" (strs 5) "\n" (strs 7))
         (get-string-all writer #t)
      )

      (newline writer)
      (write (exprs 3) writer)
      (newline writer)
      (write (exprs 10) writer)
      (newline writer)

      (writer'lineLength 10)
      (writer'lineBreakThreshold 8)
      (writer'lineSeparator "\r\n")
      (writer'indentation 2)
      (newline writer)
      (write '(aaa bbbb (ccc dd) (eeee ffff) (((((ggggg))))) hhh) writer)

      (test
         (string-append
            "\n" (strs 3) "\n" (strs 10)
            "\n\r\n(aaa\r\n  bbbb\r\n  (ccc dd)\r\n  (eeee\r\n    ffff\r\n  )\r\n  (((((ggggg)))))\r\n  hhh\r\n)"
         )
         (get-string-all writer #t)
      )

      (test "" (get-string-all writer #f))
      (test "" (get-string-all writer #t))

      (set! writer (open-output-formatter '() 40 10 3 "\n" #t))

      (write (exprs 0) writer)
      (write (exprs 6) writer)
      (newline writer)
      (write (exprs 1) writer)
      (write (exprs 2) writer)
      (newline writer)
      (newline writer)
      (write (exprs 5) writer)
      (write (exprs 7) writer)
      (write (exprs 3) writer)
      (write (exprs 10) writer)
      (newline writer)

      (test
         (string-append
            (strs 0) "\n" (strs 6) "\n" (strs 1) "\n" (strs 2) "\n\n" (strs 5)
            "\n" (strs 7) "\n" (strs 3) "\n" (strs 10) "\n"
         )
         (get-string-all writer)
      )
   )
)

; === Tests for R6RS syntactic extension ===

; Tests transformer-related functions called from outside a transformer environment
(when-full-runtime
   (test #t (symbol? (gensym)))
   (test () (generate-temporaries ()))
   (test () (generate-temporaries #'()))
   (let ((lst (generate-temporaries '(1))))
      (test 1 (length lst))
      (test #t (symbol? (car lst)))
   )
   (let ((lst (generate-temporaries #'(1))))
      (test 1 (length lst))
      (test #t (symbol? (car lst)))
   )
   (let ((lst (generate-temporaries '(1 2 3))))
      (test 3 (length lst))
      (test #t (symbol? (car lst)))
      (test #t (symbol? (cadr lst)))
      (test #t (symbol? (caddr lst)))
   )
   (let ((lst (generate-temporaries #'(1 2 3))))
      (test 3 (length lst))
      (test #t (symbol? (car lst)))
      (test #t (symbol? (cadr lst)))
      (test #t (symbol? (caddr lst)))
   )

   (test #f (identifier? 1))
   (test #f (identifier? (list 1 2)))
   (test #f (identifier? (vector 3 4)))
   (test #t (identifier? (syntax a)))
   (test #f (identifier? #'1))
   (test #f (identifier? #''a))
   (test #f (identifier? #'(1 2)))
   (test #f (identifier? #'#(3 4)))
   (test #t (identifier? #'a))
   
   (test #f (free-identifier=? #'a #'b))
   (test #t (free-identifier=? #'a #'a))
   
   (test #f (bound-identifier=? #'a #'b))
   (test #t (bound-identifier=? #'a #'a))
   
   (test () (syntax->datum ()))
   (test #() (syntax->datum #()))
   (test 'a (syntax->datum #'a))
   (test '(1 (syntax 2) 3) (syntax->datum #'(1 #'2 3)))
   (test '(1 (syntax 2) 3) (syntax->datum (datum->syntax #'a #'(1 #'2 3))))
   (let ((a #'2) (b #'(4 5)))
      (test '(1 2 3) (syntax->datum (list 1 a 3)))
      (test #(1 2 3) (syntax->datum (datum->syntax #'a (vector 1 a 3))))
      (test '(1 2 3 #((4 5) 6)) (syntax->datum (list 1 a 3 (vector b 6))))
   )
)

;;; Nested transformer expansion test.
;;; Note: Unquote to run the remaining test cases with let as a syntax transformer
;(define-syntax let
;   (syntax-rules ()
;      ((_ ((var val) ...) body ...)
;         ((lambda (var ...) body ...) val ...)
;      )
;      ((_ name ((var val) ...) body ...)
;         (((lambda (name) (set! name (lambda (var ...) body ...))) ()) val ...)
;      )
;   )
;)

; Tests transformer-related functions called from inside a transformer environment
(define-syntax transformer
   (lambda (x)
      (syntax-case x ()
         ((_ id) (identifier? #'id)
            #`(list
               #,(identifier? (let ((a #'id)) a))
               (unsyntax-splicing
                  (list
                     (free-identifier=? #'id #'and)
                     (free-identifier=? #'id (let ((a #'or)) a))
                  )
                  (list
                     (symbol=? (syntax->datum #'id) (syntax->datum #'xor))
                  )
               )
            )
         )
         ((_ id)
            #`(list
               #,(identifier? (let ((a #'id)) a))
            )
         )
      )
   )
)
(test '(#f) (transformer 1))
(test '(#f) (transformer (1 2)))
(test '(#t #f #f #f) (transformer a))
(test '(#t #t #f #f) (transformer and))
(test '(#t #f #t #f) (transformer or))
(test '(#t #f #f #t) (transformer xor))

; Tests global transformer binding:
(define-syntax global-transformer (lambda (x) (list list 1)))
(test '(1) global-transformer)
(test '(0 1) (cons 0 global-transformer))
(test '(1) (global-transformer))
(test '(1) (global-transformer 12 34))

(let-syntax ((local-transformer (lambda (x) (list list 1))))
   (test '(1) local-transformer)
   (test '(0 1) (cons 0 local-transformer))
   (test '(1) (local-transformer))
   (test '(1) (local-transformer 12 34))
)

; Tests with alterations to syntax-case input
(define-syntax test-transformer
   (lambda (x)
      (let ((x (append (syntax->datum x) '(123))))
         (syntax-case x ()
            ((_ a) #'(list 't1 a))
            ((_ a b) #'(vector 't2 a b))
         )
      )
   )
)
(test '(t1 123) (test-transformer))
(test #(t2 111 123) (test-transformer 111))

(define-syntax test-transformer
   (lambda (x)
      (if (number? (cadr (syntax->datum x)))
         (syntax-case x () ((_ a b) #'(list a b)))
         (syntax-case x () ((_ a b c) #'(vector a b c)))
      )
   )
)
(test '(1 2) (test-transformer 1 2))
(test #(a 2 3) (test-transformer 'a 2 3))

; Tests local transformer binding: let-syntax vs. letrec-syntax
(let ((f (lambda (x) (* x 10))))
   (let-syntax
      (
         (f (syntax-rules () ((f x) x)))
         (g (syntax-rules () ((g x) (f x))))
      )
      (test '(1 10) (list (f 1) (g 1)))
   )
   (letrec-syntax
      (
         (f (syntax-rules () ((f x) x)))
         (g (syntax-rules () ((g x) (f x))))
      )
      (test '(1 1) (list (f 1) (g 1)))
   )
)

(let ((f (lambda (x) (* x 10))))
   (define-syntax f (syntax-rules () ((f x) x)))
   (define-syntax g (syntax-rules () ((g x) (f x))))
   (test '(1 1) (list (f 1) (g 1)))
)

(test '(1 1)
   (let-syntax ((echo (lambda (x) (syntax-case x () ((_ y) #'y)))))
      (echo
         (let ((f (lambda (x) (* x 10))))
            (define-syntax f (syntax-rules () ((f x) x)))
            (define-syntax g (syntax-rules () ((g x) (f x))))
            (list (f 1) (g 1))
         )
      )
   )
)

; Tests hygiene property
(define var 'globalVar)
(define-syntax define-transformer ; does not modify the global var
   (syntax-rules ()
      ((_) (define var 1))
      ((_ a b) (begin (define var 123) (define var a) (list var b)))
      ((_ a b c) (begin (define var a) (set! var c) (list var b)))
   )
)
(define-transformer)
(test 'globalVar var)
(define-transformer 'localVar var)
(test 'globalVar var)
(define-transformer 'localVar var 'newLocalVar)
(test 'globalVar var)

(define-syntax define-transformer ; does modify the global var
   (lambda (x)
      (syntax-case x ()
         ((_ a) #'(begin (define a 'globalVar) (define var 'localVar) (list a var)))
         ((_ a b) #'(define a b))
         ((_ a b c) #'(begin (define var a) (set! b c) (list var a b c)))
      )
   )
)
(define-transformer var 'localVar)
(test 'localVar var)
(define-transformer 'localVar var 'newLocalVar)
(test 'newLocalVar var)
(define-transformer var)
(test 'globalVar var)

(define-syntax local-rename-transformer
   (lambda (x)
      (syntax-case x (t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
         ((_ t0 a b) #'((lambda var (list var b)) a))
         ((_ t1 a b) #'((lambda (var) (list var b)) a))
         ((_ t1 a b c d) #'((lambda (var x . y) (list var b x y)) a c d))
         ((_ t2 a b) #'(let ((var a)) (list var b)))
         ((_ t3 a b) #'(let* ((var a)) (list var b)))
         ((_ t4 a b) #'(letrec ((var a)) (list var b)))
         ((_ t5 a b) #'(let-values (((var) a)) (list var b)))
         ((_ t6 a b) #'(let*-values (((var) (values a))) (list var b)))
         ((_ t7 a b) #'(let () (define var a) (list var b)))
         ((_ t8 a b)
            #'(let loop ((var a) (i 3))
               (if (> i 0) (cons a (loop a (- i 1))) (list b))
            )
         )
         ((_ t9 a b) #'(let ((var a)) (begin (define var b)) (list var a b)))
         ((_ t10 a b) #'(let ((var b)) (list (let ((var a)) var) var)))
         ((_ t11 a b) #'(let () (list var (let ((var a)) (list var b)) var b)))
      )
   )
)
(let ((list vector))
   (test '((localVar) globalVar) (local-rename-transformer t0 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t1 'localVar var))
   (test '(localVar globalVar 123 (456)) (local-rename-transformer t1 'localVar var 123 456))
   (test '(localVar globalVar) (local-rename-transformer t2 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t3 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t4 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t5 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t6 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t7 'localVar var))
   (test
      '(localVar localVar localVar globalVar)
      (local-rename-transformer t8 'localVar var)
   )
   (test '(globalVar localVar globalVar) (local-rename-transformer t9 'localVar var))
   (test '(localVar globalVar) (local-rename-transformer t10 'localVar var))
   (test '(globalVar (localVar globalVar) globalVar globalVar)
      (local-rename-transformer t11 'localVar var)
   )
)

(let ((list vector) (var 123))
   (define-syntax ttt
      (lambda (x)
         (syntax-case x ()
            ((_ a b)
               #'(let ((var a))
                  (list var b (let ((list integer?)) (##list (list b))))
               )
            )
         )
      )
   )
   (test #(#(1 123 (#t)) #(5 123 (#t)))
      (list (ttt 1 var)
         (let ((list +))
            (ttt (list 2 3) var)
         )
      )
   )
)

(define var 123)
(define-syntax ttt
   (lambda (x)
      (syntax-case x ()
         ((_ a b func)
            #'(let ((var a))
               (list var b (let ((list integer?)) (func (list b))))
            )
         )
      )
   )
)
(test '((1 123 (#t)) (5 123 (#t)))
   (list (ttt 1 var list)
      (let ((list +))
         (ttt (list 2 3) var ##list)
      )
   )
)

; Tests quoted templates
(define x 123)
(define y (list 45 6))
(define-syntax test-quoted-template-transformer
   (syntax-rules ()
      ((_) '(... ...))
      ((_ 0) '((lambda (x y) (list x y)) 1 2))
      ((_ z) `((lambda (x y) (list x ,x ,@y)) 1 z))
      ((_ 0 a) #(a b (lambda (x y) (list x y)) 1 2))
      ((_ 1 a) '#(a b (lambda (x y) (list ,x ,@y)) 1 2))
      ((_ 2 a) `#(a b (lambda (x y) (list ,x ,@y)) 1 2))
   )
)
(test '... (test-quoted-template-transformer))
(test '((lambda (x y) (list x y)) 1 2) (test-quoted-template-transformer 0))
(test '((lambda (x y) (list x 123 45 6)) 1 25) (test-quoted-template-transformer 25))
(test #(25 b (lambda (x y) (list x y)) 1 2) (test-quoted-template-transformer 0 25))
(test #(25 b (lambda (x y) (list ,x ,@y)) 1 2) (test-quoted-template-transformer 1 25))
(test #(25 b (lambda (x y) (list 123 45 6)) 1 2) (test-quoted-template-transformer 2 25))

; Tests hygiene property and let-function behaviour
(define-syntax test-let-transformer
   (syntax-rules ()
      ((_ v1 v2)
         (let ((a v1) (b v2))
            (list
               (let ((a b) (b a)) (list a b))
               (let* ((a b) (b a)) (list a b))
               (letrec ((a b) (b a)) (list a b))
               (letrec* ((a b) (b a)) (list a b))
               (let-values
                  (
                     ((a b) (values b a))
                     ((m n) (values 2 3))
                     (k (values a b))
                     ((x y . z) (values 4 5 6 7))
                  )
                  (list a b m n k x y z)
               )
               (let*-values
                  (
                     ((a b) (values b a))
                     ((m n) (values 2 3))
                     (k (values a b))
                     ((x y . z) (values 4 5 6 7))
                  )
                  (list a b m n k x y z)
               )
            )
         )
      )
   )
)
(test
   (let ((a 1) (b 100))
      (list
         (let ((a b) (b a)) (list a b))
         (let* ((a b) (b a)) (list a b))
         (letrec ((a b) (b a)) (list a b))
         (letrec* ((a b) (b a)) (list a b))
         (let-values
            (
               ((a b) (values b a))
               ((m n) (values 2 3))
               (k (values a b))
               ((x y . z) (values 4 5 6 7))
            )
            (list a b m n k x y z)
         )
         (let*-values
            (
               ((a b) (values b a))
               ((m n) (values 2 3))
               (k (values a b))
               ((x y . z) (values 4 5 6 7))
            )
            (list a b m n k x y z)
         )
      )
   )
   (test-let-transformer 1 100)
)

; Tests overriding primitive function
(test 'rightValue
   (let-syntax
      (
         (when-transformer
            (syntax-rules ()
               ((_ test stmt1 stmt2 ...)
                  (if test (begin stmt1 stmt2 ...))
               )
            )
         )
      )
      (let ((if 'wrongValue))
         (when-transformer if (set! if 'rightValue))
         if
      )
   )
)
(test 'rightValue
   (letrec-syntax
      (
         (when-transformer
            (syntax-rules ()
               ((_ test stmt1 stmt2 ...)
                  (if test (begin stmt1 stmt2 ...))
               )
            )
         )
      )
      (let ((if 'wrongValue))
         (when-transformer if (set! if 'rightValue))
         if
      )
   )
)

; Tests overriding macro function
(test 'rightValue
   (let-syntax
      (
         (when-transformer
            (syntax-rules ()
               ((_ test stmt1 stmt2 ...)
                  (cond (test (begin stmt1 stmt2 ...)))
               )
            )
         )
      )
      (let ((cond 'wrongValue))
         (when-transformer cond (set! cond 'rightValue))
         cond
      )
   )
)
(test 'rightValue
   (letrec-syntax
      (
         (when-transformer
            (syntax-rules ()
               ((_ test stmt1 stmt2 ...)
                  (cond (test (begin stmt1 stmt2 ...)))
               )
            )
         )
      )
      (let ((cond 'wrongValue))
         (when-transformer cond (set! cond 'rightValue))
         cond
      )
   )
)

(when-full-runtime
   (let-syntax
      (
         (unless-transformer
            (syntax-rules ()
               ((_ test body ...)
                  (unless test body ...)
               )
            )
         )
      )
      (let ((unless when))
         (test '() (unless-transformer unless 'wrongValue))
         (test 'rightValue (unless-transformer #f 'rightValue))
      )
   )
)

; Tests overriding intrinsic function
(let-syntax
   (
      (vector-transformer
         (syntax-rules ()
            ((_ val ...)
               (vector val ...)
            )
         )
      )
   )
   (let ((vector list))
      (test #(1 2 3) (vector-transformer 1 2 3))
   )
)

(define list-copy (lambda x (apply list x)))
(define-syntax global-list-transformer
   (syntax-rules ()
      ((_ list a b c) (cons (list-copy a b c) (list a b c)))
   )
)
(test '((1 2 3) . #(1 2 3)) (global-list-transformer vector 1 2 3))

(let
   (
      (list-local-copy list)
      (list-three-items (lambda (x) (list 1 2 x)))
   )
   (let-syntax
      (
         (local-list-transformer
            (syntax-rules ()
               ((_ list a b c) (cons (list-local-copy a b c) (list a b c)))
            )
         )
         (local-list3-transformer
            (syntax-rules ()
               ((_ list) (list-three-items list))
            )
         )
      )
      (test '((1 2 3) . #(1 2 3)) (local-list-transformer vector 1 2 3))
      (test '(1 2 123) (local-list3-transformer 123))
      (test '(1 2 (3 4 5)) (local-list3-transformer '(3 4 5)))
   )
)

; Tests syntax matcher and expander
(define-syntax test-all-pattern-transformer
   (syntax-rules (vector)
      ((_ (a b ... c . #(d))) '(improperListEllipsis a (b ...) c d))
      ((_ (a (b c) . d)) '(improperList1 a b c d))
      ((_ (a b ... c d)) '(properListEllipsis a (b ...) c d))
      ((_ (a b . c)) '(improperList2 a b c))
      ((_ (a)) '(properList1 a))
      ((_ (a b)) '(properList2 a b))
      ((_ #(a b)) '(vector a b))
      ((_ #(a b ... c d)) '(vectorEllipsis a #(b ...) c d))
      ((_ a) '(patternVar a))
   )
)
(test '(properList1 1) (test-all-pattern-transformer (1)))
(test '(improperList2 1 2 ()) (test-all-pattern-transformer (1 2)))
(test '(improperList1 1 2 3 (4 5)) (test-all-pattern-transformer (1 (2 3) 4 5)))
(test '(properListEllipsis 1 () 2 3) (test-all-pattern-transformer (1 2 3)))
(test '(properListEllipsis 1 (2 3) 4 5) (test-all-pattern-transformer (1 2 3 4 5)))
(test '(improperList2 1 2 3) (test-all-pattern-transformer (1 2 . 3)))
(test '(improperListEllipsis 1 () 2 3) (test-all-pattern-transformer (1 2 . #(3))))
(test '(improperListEllipsis 1 (2) 3 4) (test-all-pattern-transformer (1 2 3 . #(4))))
(test '(improperListEllipsis 1 (2 3 4) 5 6) (test-all-pattern-transformer (1 2 3 4 5 . #(6))))
(test '(vector 1 2) (test-all-pattern-transformer #(1 2)))
(test '(vectorEllipsis 1 #() 2 3) (test-all-pattern-transformer #(1 2 3)))
(test '(vectorEllipsis 1 #(2 3) 4 5) (test-all-pattern-transformer #(1 2 3 4 5)))
(test '(patternVar 1) (test-all-pattern-transformer 1))
(test '(patternVar ()) (test-all-pattern-transformer ()))

; Tests ambiguous syntax and backing out of wrong choice
(define-syntax test-improper-ellipsis-list-transformer
   (syntax-rules ()
      ((_ (a b ... #(c) #(d) . e)) '(t0 a (b ...) (c d) e))
      ((_ (a b ... #(c) . d)) '(t1 a (b ...) c d))
      ((_ (a b ... c . d)) '(t2 a (b ...) c d))
      ((_ (a b ... #(c) . d) ...) '(t3 (a (b ...) c d) ...))
   )
)
(test '(t2 1 () 2 ()) (test-improper-ellipsis-list-transformer (1 2)))
(test '(t0 1 () (2 3) (4)) (test-improper-ellipsis-list-transformer (1 #(2) #(3) 4)))
(test '(t0 1 (#(2)) (3 4) ()) (test-improper-ellipsis-list-transformer (1 #(2) #(3) #(4))))
(test '(t1 1 () 2 (3 4)) (test-improper-ellipsis-list-transformer (1 #(2) 3 4)))
(test '(t0 1 () (2 3) (4 5)) (test-improper-ellipsis-list-transformer (1 #(2) #(3) 4 5)))
(test '(t1 1 (2 3 4 5) 6 ()) (test-improper-ellipsis-list-transformer (1 2 3 4 5 #(6))))
(test '(t1 1 (#(2) 3 4 5) 6 ()) (test-improper-ellipsis-list-transformer (1 #(2) 3 4 5 #(6))))
(test '(t1 1 () 2 (3 4 5 6)) (test-improper-ellipsis-list-transformer (1 #(2) 3 4 5 6)))
(test '(t2 1 (2 3 4) 5 #(6)) (test-improper-ellipsis-list-transformer (1 2 3 4 5 . #(6))))
(test '(t2 1 (2 3 4 5) 6 ()) (test-improper-ellipsis-list-transformer (1 2 3 4 5 6)))

(test '(t3 (10 (20) 30 (40)) (1 (2 3 4) 5 (6)))
   (test-improper-ellipsis-list-transformer (10 20 #(30) 40) (1 2 3 4 #(5) 6))
)
(test '(t3 (10 (20) 30 40) (1 (2 3 4) 5 6))
   (test-improper-ellipsis-list-transformer (10 20 #(30) . 40) (1 2 3 4 #(5) . 6))
)
(test '(t3 (10 () 30 ()) (1 (2 3 4) 5 6))
   (test-improper-ellipsis-list-transformer (10 #(30)) (1 2 3 4 #(5) . 6))
)

(define-syntax test-nested-improper-ellipsis-list-transformer
   (syntax-rules ()
      ((_ (a ... (x ... #(y) . z) #(b) . c)) '(t0 (a ...) (x ...) (y) (z) (b) (c)))
      ((_ (a (x ... #(y) . z)  ... b . c)) '(t1 (a) (x ... ...) (y ...) (z ...) (b) (c)))
   )
)

(test '(t1 (1) (2) (3) (4) (5) ((6)))
   (test-nested-improper-ellipsis-list-transformer (1 (2 #(3) . 4) 5 6))
)
(test '(t0 (1) (2) (3) (4) (5) ((6)))
   (test-nested-improper-ellipsis-list-transformer (1 (2 #(3) . 4) #(5) 6))
)
(test '(t1 (1) (2 5 6) (3 7 9) (4 (8) (10)) (11) (12))
   (test-nested-improper-ellipsis-list-transformer (1 (2 #(3) . 4) (5 6 #(7) 8) (#(9) 10) 11 . 12))
)
(test '(t0 (1 (2 #(3) . 4) (5 6 #(7) 8)) () (9) ((10)) (11) (12))
   (test-nested-improper-ellipsis-list-transformer (1 (2 #(3) . 4) (5 6 #(7) 8) (#(9) 10) #(11) . 12))
)
(test '(t0 (1 2 3 4) (5 6) (7) ((8)) (9) (10))
   (test-nested-improper-ellipsis-list-transformer (1 2 3 4 (5 6 #(7) 8) #(9) . 10))
)

; Tests ellipsis list expansion without wrap-around
(define-syntax test-list-expansion-transformer
   (syntax-rules ()
      ((_ ((a b ... c) ...) ...)
      '(
         (b ... ... ...)
         (b ... ... ... . 123)
         ((b . b) ... ... ...)
         ((b b) ... ... ...)

         ((b ... ...) ...)
         ((((b)) ... ...) ...)
         ((b ... ... b ... ...) ...)
         ((b ... ... . 123) ...)

         ((b ...) ... ...)
         ((b ... b ...) ... ...)
         ((b ... . 123) ... ...)

         ((b) ... ... ...)
         ((b . 123) ... ... ...)
         ((b 123) ... ... ...)

         (((b ... a) ...) ...)
         ((((b) ...) ...) ...)

         ((a b ... . c) ... ...)
         ((a c . (b ...)) ... ...)
         (((a c . ((b) ...)) ...) ...)
         ((a ... c ... . (b ... ...)) ...)

         ((c a b ...) ... ... c ... ... (a) ... ... (b ... ... ...))
      ))
   )
)
(test
   '(
      (2 7 8)
      (2 7 8 . 123)
      ((2 . 2) (7 . 7) (8 . 8))
      ((2 2) (7 7) (8 8))

      ((2) (7 8))
      ((((2))) (((7)) ((8))))
      ((2 2) (7 8 7 8))
      ((2 . 123) (7 8 . 123))

      ((2) () (7 8))
      ((2 2) () (7 8 7 8))
      ((2 . 123) 123 (7 8 . 123))

      ((2) (7) (8))
      ((2 . 123) (7 . 123) (8 . 123))
      ((2 123) (7 123) (8 123))

      (((2 1) (4)) ((7 8 6)))
      ((((2)) ()) (((7) (8))))

      ((1 2 . 3) (4 . 5) (6 7 8 . 9))
      ((1 3 2) (4 5) (6 9 7 8))
      (((1 3 (2)) (4 5)) ((6 9 (7) (8))))
      ((1 4 3 5 2) (6 9 7 8))

      ((3 1 2) (5 4) (9 6 7 8) 3 5 9 (1) (4) (6) (2 7 8))
   )
   (test-list-expansion-transformer ((1 2 3) (4 5)) ((6 7 8 9)))
)
(test
   '(
      (4 7 8)
      (4 7 8 . 123)
      ((4 . 4) (7 . 7) (8 . 8))
      ((4 4) (7 7) (8 8))
      ((4) (7 8))
      ((((4))) (((7)) ((8))))
      ((4 4) (7 8 7 8))
      ((4 . 123) (7 8 . 123))
      (() (4) (7 8))
      (() (4 4) (7 8 7 8))
      (123 (4 . 123) (7 8 . 123))
      ((4) (7) (8))
      ((4 . 123) (7 . 123) (8 . 123))
      ((4 123) (7 123) (8 123))
      (((1) (4 3)) ((7 8 6)))
      ((() ((4))) (((7) (8))))
      ((1 . 2) (3 4 . 5) (6 7 8 . 9))
      ((1 2) (3 5 4) (6 9 7 8))
      (((1 2) (3 5 (4))) ((6 9 (7) (8))))
      ((1 3 2 5 4) (6 9 7 8))
      ((2 1) (5 3 4) (9 6 7 8) 2 5 9 (1) (3) (6) (4 7 8))
   )
   (test-list-expansion-transformer ((1 2) (3 4 5)) ((6 7 8 9)))
)
(test
   '(
      (7 8)
      (7 8 . 123)
      ((7 . 7) (8 . 8))
      ((7 7) (8 8))
      (() (7 8))
      (() (((7)) ((8))))
      (() (7 8 7 8))
      (123 (7 8 . 123))
      (() () (7 8))
      (() () (7 8 7 8))
      (123 123 (7 8 . 123))
      ((7) (8))
      ((7 . 123) (8 . 123))
      ((7 123) (8 123))
      (((1) (4)) ((7 8 6)))
      ((() ()) (((7) (8))))
      ((1 . 2) (4 . 5) (6 7 8 . 9))
      ((1 2) (4 5) (6 9 7 8))
      (((1 2) (4 5)) ((6 9 (7) (8))))
      ((1 4 2 5) (6 9 7 8))
      ((2 1) (5 4) (9 6 7 8) 2 5 9 (1) (4) (6) (7 8))
   )
   (test-list-expansion-transformer ((1 2) (4 5)) ((6 7 8 9)))
)

; Tests ellipsis vector expansion without wrap-around
(define-syntax test-vector-expansion-transformer
   (syntax-rules ()
      ((_ #(#(a b ... c) ...) ...)
      '(
         #(b ... ... ...)

         #((b ... ...) ...)
         (#(b ... ...) ...)
         #(#(b ... ...) ...)

         #((b ...) ... ...)
         (#(b ...) ... ...)
         #(#(b ...) ... ...)

         #((b) ... ... ...)
         (#(b) ... ... ...)
         #(#(b) ... ... ...)

         #((b ... ... . 123) ...)
         #((b ... . 123) ... ...)
         #((b . 123) ... ... ...)

         #((b 123) ... ... ...)
         (#(b 123) ... ... ...)
         #(#(b 123) ... ... ...)

         #((b . b) ... ... ...)
         #((b b) ... ... ...)
         (#(b b) ... ... ...)
         #(#(b b) ... ... ...)

         #((#((b)) ... ...) ...)
         #(#(#(#(b)) ... ...) ...)
         (#((#(b) ...) ...) ...)
         #(#(#(#(b) ...) ...) ...)

         #((a c . (b ...)) ... ...)
         #((c a b ...) ... ... c ... ... (a) ... ... (b ... ... ...))
         (#(c a b ...) ... ... c ... ... #(a) ... ... #(b ... ... ...))
         #(#(c a b ...) ... ... c ... ... #(a) ... ... #(b ... ... ...))
      ))
   )
)
(test
   '(
      #(2 7 8)

      #((2) (7 8))
      (#(2) #(7 8))
      #(#(2) #(7 8))

      #((2) () (7 8))
      (#(2) #() #(7 8))
      #(#(2) #() #(7 8))

      #((2) (7) (8))
      (#(2) #(7) #(8))
      #(#(2) #(7) #(8))

      #((2 . 123) (7 8 . 123))
      #((2 . 123) 123 (7 8 . 123))
      #((2 . 123) (7 . 123) (8 . 123))

      #((2 123) (7 123) (8 123))
      (#(2 123) #(7 123) #(8 123))
      #(#(2 123) #(7 123) #(8 123))

      #((2 . 2) (7 . 7) (8 . 8))
      #((2 2) (7 7) (8 8))
      (#(2 2) #(7 7) #(8 8))
      #(#(2 2) #(7 7) #(8 8))

      #((#((2))) (#((7)) #((8))))
      #(#(#(#(2))) #(#(#(7)) #(#(8))))
      (#((#(2)) ()) #((#(7) #(8))))
      #(#(#(#(2)) #()) #(#(#(7) #(8))))

      #((1 3 2) (4 5) (6 9 7 8))
      #((3 1 2) (5 4) (9 6 7 8) 3 5 9 (1) (4) (6) (2 7 8))
      (#(3 1 2) #(5 4) #(9 6 7 8) 3 5 9 #(1) #(4) #(6) #(2 7 8))
      #(#(3 1 2) #(5 4) #(9 6 7 8) 3 5 9 #(1) #(4) #(6) #(2 7 8))
   )
   (test-vector-expansion-transformer #(#(1 2 3) #(4 5)) #(#(6 7 8 9)))
)

; Tests syntax
(let-syntax
   (
      (test-constant-transformer (syntax-rules () ((_ id) (syntax (syntax (syntax 123))))))
      (test-identifier-transformer (syntax-rules () ((_ id) (syntax (syntax (syntax id))))))
      (test-sequence-transformer (syntax-rules () ((_ id id2) (list (syntax id) (syntax id2)))))
   )
   (test (syntax (syntax (syntax 123))) (test-constant-transformer 1))
   (test (syntax (syntax (syntax 1))) (test-identifier-transformer 1))
   (test (list (syntax 1) (syntax 2)) (test-sequence-transformer 1 2))
)

(let-syntax
   (
      (test-constant-transformer (lambda (x) (syntax-case x () ((_ id) (syntax (syntax (syntax (syntax 123))))))))
      (test-identifier-transformer (lambda (x) (syntax-case x () ((_ id) (syntax (syntax (syntax (syntax id))))))))
      (test-sequence-transformer (lambda (x) (syntax-case x () ((_ id id2) (syntax (list (syntax id) (syntax id2)))))))
   )
   (test (syntax (syntax (syntax 123))) (test-constant-transformer 1))
   (test (syntax (syntax (syntax 1))) (test-identifier-transformer 1))
   (test (list (syntax 1) (syntax 2)) (test-sequence-transformer 1 2))
)

; Tests quasisyntax
(let ((a 1) (b 2) (c 3) (d 4))
   (test
      (list (syntax a) (syntax d) 2 (syntax b) 3 4 1 2 (syntax c))
      (quasisyntax (a d (unsyntax b) b (unsyntax c d a b) c))
   )
   (test
      (list (syntax a) (syntax d) 2 (syntax b) 3 4 1 2 (syntax c))
      (quasisyntax (a d (unsyntax-splicing (list b)) b (unsyntax-splicing (list c) (list d a) (list b)) c))
   )
   (test
      (list (syntax a) (syntax d) (syntax b) (syntax b) 3 (syntax d) (syntax a) 2 (syntax c))
      (quasisyntax (a (unsyntax-splicing #'(d b)) b (unsyntax-splicing (list c) #'(d a) (list b)) c))
   )
)

(let-syntax
   (
      (test-quasisyntax-transformer1
         (lambda (x)
            (syntax-case x ()
               ((_ a)
                  (quasisyntax (quasisyntax (unsyntax (quasisyntax a))))
               )
            )
         )
      )
      (test-quasisyntax-transformer2
         (lambda (x)
            (syntax-case x ()
               ((_ a)
                  (quasisyntax (quasisyntax (quasisyntax (unsyntax (unsyntax a)))))
               )
            )
         )
      )
      (test-quasisyntax-transformer3
         (lambda (x)
            (syntax-case x ()
               ((_ a)
                  (quasisyntax (quasisyntax (quasisyntax (unsyntax a))))
               )
            )
         )
      )
   )
   (test (syntax 1) (test-quasisyntax-transformer1 1))
   (test `(,(syntax quasisyntax) (,(syntax unsyntax) 1)) (test-quasisyntax-transformer2 1))
   (test (quasisyntax (quasisyntax (unsyntax 1))) (test-quasisyntax-transformer3 1))
)

(let-syntax
   (
      (test-quasisyntax-transformer
         (lambda (x)
            (syntax-case x ()
               ((_ a b)
                  (quasisyntax
                     (list a b
                        (unsyntax
                           (let ((a 123)) a)
                           (quasisyntax #,456)
                           (quasisyntax
                              (list
                                 (unsyntax-splicing)
                                 #,@'(7 8)
                                 (unsyntax-splicing (list 9 #'a) (list #'b))
                              )
                           )
                        )
                        (unsyntax-splicing
                           (let ((a 123) (c 456)) (list a c))
                           (quasisyntax ((unsyntax) (unsyntax 789) (unsyntax 7 8 9 #'a #'b)))
                        )
                     )
                  )
               )
            )
         )
      )
   )
   (test '(1 2 123 456 (7 8 9 1 2) 123 456 789 7 8 9 1 2) (test-quasisyntax-transformer 1 2))
)

; Tests syntax-case and syntax
(let-syntax
   (
      (testSyntaxCaseNoSyntax
         (lambda (x) (syntax-case x () (_ (list list 1))))
      )
      (testSyntaxCaseSyntax
         (lambda (x) (syntax-case x () (_ (syntax (list list 1)))))
      )
      (testSyntaxCaseSyntaxVar
         (lambda (x) (syntax-case x () ((_ a) (list list (syntax a)))))
      )
      (testSyntaxCaseTwoSyntaxVar
         (lambda (x) (syntax-case x () ((_ a) (list list (syntax (syntax a))))))
      )
      (testSyntaxCaseNoSyntaxMacro
         (lambda (x) (syntax-case x () (_ (let ((a 123)) (list list a)))))
      )
   )
   (test '(1) (testSyntaxCaseNoSyntax))
   (test `(,list 1) (testSyntaxCaseSyntax))
   (test '(123) (testSyntaxCaseSyntaxVar 123))
   (test '(456) (let ((k 456)) (testSyntaxCaseSyntaxVar k)))
   (test `(,(syntax 123)) (testSyntaxCaseTwoSyntaxVar 123))
   (test '(123) (testSyntaxCaseNoSyntaxMacro))
)

; Tests syntax-case with fender
(let ((p (list 0 2)))
   (define-syntax pcar
      (lambda (x)
         (syntax-case x ()
            (e (identifier? #'e) #'(car p))
            ((_ e) #'(set-car! p e))
         )
      )
   )
   (let ((a pcar))
      (pcar 1)
      (test '(0 1 (1 2)) (list a pcar p))
   )
)

; Tests identifier? as fender
(let-syntax
   (
      (transformer
         (lambda (x)
            (let ((y #'x))
               (syntax-case x ()
                  (_ (identifier? y) #''first)
                  (_ #''second)
               )
            )
         )
      )
   )
   (test 'first (transformer))
   (test 'first transformer)
)

(let-syntax
   (
      (transformer
         (lambda (x)
            (let ((y x))
               (syntax-case x ()
                  (_ (identifier? y) #''first)
                  (_ #''second)
               )
            )
         )
      )
   )
   (test 'second (transformer))
   (test 'first transformer)
)

; Tests syntax->datum as fender
(let-syntax
   (
      (transformer
         (lambda (x)
            (let ((y 123))
               (syntax-case x ()
                  ((_ a b) (= (syntax->datum #'a) y) #'`(first a))
                  ((_ a b) #'`(second b))
               )
            )
         )
      )
   )
   (test '(second 34) (transformer 12 34))
   (test '(first 123) (transformer 123 34))
)

; Tests recursive transformer
(define-syntax rev
   (lambda (x)
      (syntax-case x ()
         ((_) #'())
         ((_ a ... b) #'(cons b (rev a ...)))
      )
   )
)
(test '() (rev))
(test '(1) (rev 1))
(test '(2 1) (rev 1 2))
(test '(5 4 3 2 1) (rev 1 2 3 4 5))

(letrec-syntax
   ((rev
      (syntax-rules ()
         ((_ ()) '())
         ((_ (a ... b)) (cons b (rev (a ...))))
      )
   ))
   (test '() (rev ()))
   (test '(1) (rev (1)))
   (test '(2 1) (rev (1 2)))
   (test '(5 4 3 2 1) (rev (1 2 3 4 5)))
)

(letrec-syntax
   ((pair-rev
      (syntax-rules ()
         ((_ (a ... b c)) (cons* b c (pair-rev (a ...))))
         ((_ a) 'a)
      )
   ))
   (test '() (pair-rev ()))
   (test '(1) (pair-rev (1)))
   (test '(1 2) (pair-rev (1 2)))
   (test '(4 5 2 3 1) (pair-rev (1 2 3 4 5)))
   (test '(5 6 3 4 1 2) (pair-rev (1 2 3 4 5 6)))
)

; Tests let-syntax and letrec-syntax scopes with recursive transformer
(let-syntax
   ((rev
      (syntax-rules ()
         ((_ '()) '())
         ((_ '(a ... b)) (cons b (rev '(a ...)))) ; this rev resolves to the global rev
      )
   ))
   (test '() (rev '()))
   (test '(1 ()) (rev '(1)))
   (test '(2 (1)) (rev '(1 2)))
   (test '(5 (1 2 3 4)) (rev '(1 2 3 4 5)))
)

(letrec-syntax
   ((rev
      (syntax-rules ()
         ((_ '()) '())
         ((_ '(a ... b)) (cons b (rev '(a ...)))) ; this rev resolves to the letrec-syntax rev
      )
   ))
   (test '() (rev '()))
   (test '(1) (rev '(1)))
   (test '(2 1) (rev '(1 2)))
   (test '(5 4 3 2 1) (rev '(1 2 3 4 5)))
)

(letrec-syntax   
   (
      (list-rev
         (syntax-rules ()
            ((_) '())
            ((_ a ... b) (cons (rev b) (list-rev a ...)))
         )
      )
      (rev
         (syntax-rules ()
            ((_ ()) '())
            ((_ (a ... b)) (cons b (rev (a ...))))
         )
      )
   )
   (test '((5 4 3 2 1) () (1) (2 1)) (list-rev (1 2) (1) () (1 2 3 4 5)))
)

(letrec-syntax
   (
      (list-rev
         (syntax-rules
            ()
            ((_) '())
            ((_ a ... b)
               (letrec-syntax
                  (
                     (rev
                        (syntax-rules
                           ()
                           ((_ ()) '())
                           ((_ (c (... ...) d)) (cons d (rev (c (... ...)))))
                        )
                     )
                  )
                  (cons (rev b) (list-rev a ...))
               )
            )
         )
      )
   )
   (test '((5 4 3 2 1) () (1) (2 1)) (list-rev (1 2) (1) () (1 2 3 4 5)))
)

; Tests variable transformer
(let ((ls (list 0 2 3)))
   (define-syntax testVariable-transformer
      (make-variable-transformer
         (lambda (x)
           (syntax-case x ()
             (id (identifier? #'id) #'(car ls))
             ((set! _ e) #'(set-car! ls e))
             ((_ e ...) #'((car ls e ...)))
           )
         )
      )
   )
   (let ((before testVariable-transformer))
      (test '(0 2 3) ls)
      (set! testVariable-transformer 1)
      (test 0 before)
      (test 1 testVariable-transformer)
      (test '(1 2 3) ls)
   )
)

; Tests call/cc invoked in transformer definition
(let-syntax
   (
      (find-index
         (lambda (x)
            (syntax-case x
               ()
               ((_ val ls)
                  (call/cc
                     (lambda (return)
                        (define i 0)
                        (let f ((x (syntax->datum #'ls)))
                           (if (eqv? (car x) (syntax->datum #'val))
                              (return i)
                              (begin (set! i (+ i 1)) (f (cdr x)))
                           )
                        )
                     )
                  )
               )
            )
         )
      )
   )
   (test 0 (find-index 123 (123 1 0 3 4 5)))
   (test 2 (find-index 123 (0 1 123 3 4 123))) 
   (test 5 (find-index 123 (0 1 2 3 4 123)))
)

; Tests identifier-syntax (nested transformer)
(define p (list 0 2))
(define-syntax p.car (identifier-syntax (car p)))
(test 0 p.car)

(define-syntax p.car
   (identifier-syntax
      (y (car p))
      ((set! _ e) (set-car! p e))
   )
)
(test 0 p.car)
(set! p.car 1)
(test 1 p.car)
(test '(1 2) p)

; Tests nested matcher
(let ((a 123))
   (let-syntax
      (
         (nest
            (lambda (x)
               (syntax-case x
                  ()
                  ((_ a b)
                     (syntax-case #'a ()
                        (b (identifier? #'b) #'b)
                        (c #'(list b 'c))
                     )
                  )
               )
            )
         )
      )
      (test 123 (nest a 3))
      (test '(3 (1 2)) (nest (1 2) 3))
   )
)

; Tests with-syntax (nested expansion)
(letrec-syntax
   (
      (or-transformer
         (lambda (x)
            (syntax-case x ()
               ((_) (syntax #f))
               ((_ e) (syntax e))
               ((_ e1 e2 e3 ...)
                  (with-syntax ((rest (syntax (or-transformer e2 e3 ...))))
                     #'(let ((t e1)) (if t t rest))
                  )
               )
            )
         )
      )
   )
   (test #f (or-transformer))
   (test #t (or-transformer (> 3 2)))
   (test #t (or-transformer (> 1 2) (> 2 1)))
   (test #f (or-transformer (> 1 2) (> 2 3)))
   (test #t (or-transformer #f #f #f #t))
   (test #f (or-transformer #f #f #f #f))
   (test '(2 3) (or-transformer (member 2 '(1 2 3)) (/ 1 0) (/ 1 0)))
   (let ((a '(1)) (b '(2)) (c 3))
      (test '(1) (or-transformer a b c))
   )
)

(define-syntax cond-transformer
   (lambda (x)
      (syntax-case x ()
         ((_ c1 c2 ...)
            (let f ((c1 #'c1) (cmore #'(c2 ...)))
               (if (null? cmore)
                  (syntax-case c1 (else =>)
                     ((else e1 e2 ...) #'(begin e1 e2 ...))
                     ((e0) #'(let ((t e0)) (if t t)))
                     ((e0 => e1) #'(let ((t e0)) (if t (e1 t))))
                     ((test lambda list ...) #'(if test (begin lambda list ...)))
                  )
                  (with-syntax ((rest (f (car cmore) (cdr cmore))))
                     (syntax-case c1 (=>)
                        ((e0) (syntax (let ((t e0)) (if t t rest))))
                        ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t) rest))))
                        ((e0 e1 bitwise-and ...) (syntax (if e0 (begin e1 bitwise-and ...) rest)))
                     )
                  )
               )
            )
         )
      )
   )
)
(test #t (cond-transformer ((> 1 2)) ((> 2 1))))
(test '() (cond-transformer ((> 1 2)) ((> 1 1))))
(test 't (cond-transformer ((> 1 2) 'f) ((> 2 1) 't)))
(test 't (cond-transformer ((> 2 1) 't) ((> 1 2) 'f)))
(test 't (cond-transformer ((> 1 2) 'f) ((> 1 1) 'f) ((> 2 1) 't)))
(test 't (cond-transformer ((> 1 2) 'f) ((> 1 1) 'f) ((> 1 3) 'f) ((> 2 1) 't)))
(test 't (cond-transformer ((> 1 2) (+ 1 2) 'f) ((> 2 1) (+ 2 3) 't)))
(test '() (cond-transformer ((> 1 2) 'f)))
(test '() (cond-transformer ((> 1 2) 'a) ((> 2 3) 'b)))
(test 'c (cond-transformer ((> 1 2) 'a) ((> 2 3) 'b) (else 'c)))
(test 'c (cond-transformer ((> 1 2) 'a) ((> 2 3) 'b) (else 1 2 3 'c)))
(test 'a (cond-transformer ((member 'a '(b a c)) => car) (else 'z)))

; Tests nested transformer with literal ellipses and local renaming
(define-syntax outer-transformer
   (lambda (x)
      (syntax-case x ()
         ((_ a b ...)
            #'(let-syntax
               (
                  (inner-transformer
                     (syntax-rules () ((_ x (... ...) y) '((x (... ...)) y)))
                  )
               )
               (inner-transformer b ... a)
            )
         )
      )
   )
)
(test '(() 1) (outer-transformer 1))
(test '((2) 1) (outer-transformer 1 2))
(test '((2 3 4) 1) (outer-transformer 1 2 3 4))

(let-syntax
   ((outer-transformer
      (lambda (x)
         (syntax-case x ()
            ((_ a b ...)
               #'(letrec-syntax
                  (
                     (inner-transformer
                        (syntax-rules () ((_ x (... ...) y) '((x (... ...)) y)))
                     )
                  )
                  (inner-transformer b ... a)
               )
            )
         )
      )
   ))
   (test '(() 1) (outer-transformer 1))
   (test '((2) 1) (outer-transformer 1 2))
   (test '((2 3 4) 1) (outer-transformer 1 2 3 4))
)

(letrec-syntax
   ((outer-transformer
      (lambda (x)
         (syntax-case x ()
            ((_ a b ...)
               #'(let ()
                  (define-syntax inner-transformer
                     (syntax-rules () ((_ x (... ...) y) '((x (... ...)) y)))
                  )
                  (inner-transformer b ... a)
               )
            )
         )
      )
   ))
   (test '(() 1) (outer-transformer 1))
   (test '((2) 1) (outer-transformer 1 2))
   (test '((2 3 4) 1) (outer-transformer 1 2 3 4))
)

; Tests bound-identifier=?
(define-syntax let-transformer
   (lambda (x)
      (define unique-ids?
         (lambda (ls)
            (or (null? ls)
               (and
                  (let loop ((e (car ls)) (ls (cdr ls)))
                     (or (null? ls)
                        (and (not (bound-identifier=? e (car ls)))
                           (loop e (cdr ls))
                        )
                     )
                  )
                  (unique-ids? (cdr ls))
               )
            )
         )
      )
      (syntax-case x ()
         ((_ ((var val) ...) body1 body2 ...)
            (unique-ids? #'(var ...))
            #'((lambda (var ...) body1 body2 ...) val ...)
         )
      )
   )
)
(test 123 (let-transformer () 123))
(test 123 (let-transformer ((a 123)) a))
(test '(123 456 789) (let-transformer ((a 123) (b 456) (c 789)) (list a b c)))

; Tests free-identifier=?, bound-identifier=?, syntax->datum and datum->syntax in transformer context
(let ((var 123))
   (define-syntax invoke-id-test (syntax-rules () ((_ func id) (func id var))))
   (define-syntax free-id-test
      (lambda (x)
         (syntax-case x ()
            ((_ id1 id2)
               #`(list
                  #,(free-identifier=? #'id1 #'id2)
                  #,(free-identifier=? #'id1 (datum->syntax #'id2 'temp))
                  #,(free-identifier=? (datum->syntax #'id1 'temp) #'id2)
                  #,(free-identifier=? (datum->syntax #'id1 'temp) (datum->syntax #'id2 'temp))
                  #,(free-identifier=? #'id1 (datum->syntax #'id2 'var))
                  #,(free-identifier=? #'temp (datum->syntax #'id1 'temp))
                  #,(free-identifier=? #'temp (datum->syntax #'id2 'temp))
               )
            )
         )
      )
   )
   (define-syntax bound-id-test
      (lambda (x)
         (syntax-case x ()
            ((_ id1 id2)
               (let
                  (
                     (stx-id1 #'id1)
                     (stx-id2 #'id2)
                     (stx-temp #'temp)
                     (temp-id1 (datum->syntax #'id1 'temp))
                     (temp-id2 (datum->syntax #'id2 'temp))
                     (var-id1 (datum->syntax #'id1 'var))
                     (var-id2 (datum->syntax #'id2 'var))
                  )
                  #`(list
                     #,(bound-identifier=? stx-id1 stx-id2)
                     #,(bound-identifier=? stx-id1 var-id2)
                     #,(bound-identifier=? stx-id2 var-id1)
                     #,(bound-identifier=? stx-id1 var-id1)
                     #,(bound-identifier=? stx-id2 var-id2)
                     #,(bound-identifier=? stx-id1 temp-id2)
                     #,(bound-identifier=? temp-id1 stx-id2)
                     #,(bound-identifier=? temp-id1 temp-id2)
                     #,(bound-identifier=? stx-temp temp-id1)
                     #,(bound-identifier=? stx-temp temp-id2)
                  )
               )
            )
         )
      )
   )
   (define-syntax equal-test
      (lambda (x)
         (syntax-case x ()
            ((_ id1 id2)
               (let
                  (
                     (stx-id1 #'id1)
                     (stx-id2 #'id2)
                     (stx-temp #'temp)
                  )
                  #`(list
                     #,(= 'var (syntax->datum stx-id1))
                     #,(eq? 'var (syntax->datum stx-id2))
                     #,(eqv? 'temp (syntax->datum stx-temp))
                     #,(equal? '(var #(temp var) temp (var #(temp (var))))
                        (syntax->datum
                           (datum->syntax stx-id1
                              '(var #(temp var) temp (var #(temp (var))))
                           )
                        )
                     )
                  )
               )
            )
         )
      )
   )
   (test '(#t #f #f #t #t #t #t) (free-id-test var var))
   (test '(#t #f #f #t #t #t #t) (invoke-id-test free-id-test var))
   (test '(#t #t #t #t #t #f #f #t #f #f) (bound-id-test var var))
   (test '(#f #f #f #t #t #f #f #f #f #f) (invoke-id-test bound-id-test var))
   (test '(#t #t #t #t) (equal-test var var))
   (test '(#t #t #t #t) (invoke-id-test equal-test var))
)

(let ((var 123))
   (define-syntax invoke-id-test (syntax-rules () ((_ func id) (func id var))))
   (define-syntax free-id-test
      (lambda (x)
         (syntax-case x ()
            ((_ id1 id2)
               (with-syntax
                  (
                     (a (free-identifier=? (syntax id1) (syntax id2)))
                     (b (free-identifier=? (syntax id1) (datum->syntax #'id2 'temp)))
                     (c (free-identifier=? (datum->syntax #'id1 'temp) (syntax id1)))
                     (d (free-identifier=? (datum->syntax #'id1 'temp) (datum->syntax #'id2 'temp)))
                     (e (free-identifier=? (syntax id1) (datum->syntax #'id2 'var)))
                  )
                  #'(list a b c d e)
               )
            )
         )
      )
   )
   (define-syntax bound-id-test
      (lambda (x)
         (syntax-case x ()
            ((_ id1 id2)
               (with-syntax
                  (
                     ((a ... b)
                        (list
                           (bound-identifier=? #'id1 #'id2)
                           (bound-identifier=? #'id1 (datum->syntax #'id2 'temp))
                           (bound-identifier=? (datum->syntax #'id1 'temp) #'id1)
                           (bound-identifier=? (datum->syntax #'id1 'temp) (datum->syntax #'id2 'temp))
                        )
                     )
                     ((c d)
                        (list
                           (bound-identifier=? #'id1 (datum->syntax #'id2 'var))
                           (bound-identifier=? (datum->syntax #'id1 'var) #'id2)
                        )
                     )
                     (#(e ...)
                        (vector
                           (bound-identifier=? #'id1 (datum->syntax #'id1 'var))
                           (bound-identifier=? (datum->syntax #'id2 'var) #'id2)
                        )
                     )
                  )
                  #'(list a ... b c d e ...)
               )
            )
         )
      )
   )
   (define-syntax equal-test
      (lambda (x)
         (syntax-case x ()
            ((_ id1 id2)
               (with-syntax
                  (
                     (a (= 'var (syntax->datum #'id1)))
                     ((b ...)
                        (list
                           (eq? 'var (syntax->datum #'id2))
                           (eqv? 'temp (syntax->datum #'temp))
                        )
                     )
                     (c
                        (equal? '(var #(temp var) temp (var #(temp (var))))
                           (syntax->datum
                              (datum->syntax #'id1
                                 '(var #(temp var) temp (var #(temp (var))))
                              )
                           )
                        )
                     )
                  )
                  #'(list a b ... c)
               )
            )
         )
      )
   )
   (test '(#t #f #f #t #t) (free-id-test var var))
   (test '(#t #f #f #t #t) (invoke-id-test free-id-test var))
   (test '(#t #f #f #t #t #t #t #t) (bound-id-test var var))
   (test '(#f #f #f #f #f #f #t #t) (invoke-id-test bound-id-test var))
   (test '(#t #t #t #t) (equal-test var var))
   (test '(#t #t #t #t) (invoke-id-test equal-test var))
)

; Test datum->syntax
(define-syntax loop
   (lambda (x)
      (syntax-case x
         ()
         ((k e ...)
            (with-syntax
               ((break (datum->syntax #'k 'break)))
               #'(call/cc (lambda (break) (let f () e ... (f))))
            )
         )
      )
   )
)
(let ((n 3) (ls '()))
   (test '(a a a)
      (loop
         (if (= n 0) (break ls))
         (set! ls (cons 'a ls))
         (set! n (- n 1))
      )
   )
)

; Tests simple variable value wrap-arounds with no alignment constraints
(define-syntax transformer
   (lambda (x)
      (syntax-case x ()
         ((_ a b ...) #'(list '(a b) ... 123))
      )
   )
)
(test '(123) (transformer 1))
(test '((1 2) 123) (transformer 1 2))
(test '((1 2) (1 3) (1 4) 123) (transformer 1 2 3 4))

; Tests variable value wrap-arounds and alignment during ellipsis expansion
(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a (B ...)) ... (c ...))
            '((a B c) ... ...)
         )
     )
   ))
   (test
      '(
         (a1 b11 c1) (a2 b12 c2) (a3 b13 c3)
         (a1 b21 c1) (a2 b22 c2) (a3 b23 c3)
         (a1 b31 c1) (a2 b32 c2) (a3 b33 c3)
      )
      (test-alignment-transformer (a1 (b11 b12 b13)) (a2 (b21 b22 b23)) (a3 (b31 b32 b33)) (c1 c2 c3))
   )
   (test
      '((a1 b11 c1) (a2 b21 c2) (a3 b31 c3))
      (test-alignment-transformer (a1 (b11)) (a2 (b21)) (a3 (b31)) (c1 c2 c3))
   )
)

(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a ... ((B ...) ...) c) ...)
            '((a ... B ... c) ... ...)
         )
     )
   ))
   (test
      '((a1 b1 c1))
      (test-alignment-transformer (a1 ((b1)) c1))
   )
   (test
      '((a1 a2 b1 c1))
      (test-alignment-transformer (a1 a2 ((b1)) c1))
   )
)

(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a (B ...) C ...) ...)
            '((a B C) ... ...)
         )
     )
   ))
   (test
      '((a1 b11 c11) (a2 b12 c12) (a1 b21 c21) (a2 b22 c22))
      (test-alignment-transformer (a1 (b11 b12) c11 c12) (a2 (b21 b22) c21 c22))
   )
)


(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a (B ...)) ... (c ...))
            '((B c) ... ...)
         )
     )
   ))
   (test
      '((b11 c1) (b12 c2) (b21 c1) (b22 c2))
      (test-alignment-transformer (a1 (b11 b12)) (a2 (b21 b22)) (c1 c2))
   )
)

(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a (B ...)) ... (c ...))
            '((c B) ... ...)
         )
     )
   ))
   (test
      '((c1 b11) (c2 b12) (c1 b21) (c2 b22))
      (test-alignment-transformer (a1 (b11 b12)) (a2 (b21 b22)) (c1 c2))
   )
)

(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a ... ((B ...) ...) c) ...)
            '(((a) ... (B ...) (c)) ... ...)
         )
     )
   ))
   (test
      '(((a1) (b1) (c1)))
      (test-alignment-transformer (a1 ((b1)) c1))
   )
)

(let-syntax
   ((test-alignment-transformer
      (syntax-rules ()
         ((_ (a ... (b ...)) (C ...) ...)
            '(((a b) ... (C ...)) ...)
         )
     )
   ))
   (test
      '(((a1 b1) (a2 b2) (c1)) ((a1 b1) (a2 b2) (c2)))
      (test-alignment-transformer (a1 a2 (b1 b2)) (c1) (c2))
   )
)

(let-syntax
   ((test-nested-alignment-transformer
      (syntax-rules ()
         ((_ (a ...) (b ...) (c ...) ...)
            '((a b (a c) ...) ...)
         )
      )
   ))
   (test
      '((a1 b1 (a1 c11) (a2 c12)) (a2 b2 (a1 c21) (a2 c22)))
      (test-nested-alignment-transformer (a1 a2) (b1 b2) (c11 c12) (c21 c22))
   )
   (test
      '((a1 b1 (a1 c11) (a2 c12) (a3 c13))
        (a2 b2 (a1 c21) (a2 c22) (a3 c23))
        (a3 b3 (a1 c31) (a2 c32) (a3 c33))
       )
      (test-nested-alignment-transformer (a1 a2 a3) (b1 b2 b3) (c11 c12 c13) (c21 c22 c23) (c31 c32 c33))
   )
)

(define-syntax echo (lambda (x) (syntax-case x () ((_ y) #'y))))
(test 123 (echo (let ((a 123)) (echo a))))
(test 123 (let ((a 123)) (echo (echo a))))

(let-syntax
   (
      (let-transformer
         (syntax-rules ()
            ((_ ((var val) ...) body ...)
               ((lambda (var ...) body ...) val ...)
            )
            ((_ name ((var val) ...) body ...)
               (((lambda (name) (set! name (lambda (var ...) body ...))) ()) val ...)
            )
         )
      )
   )
   (let-transformer loop ((nnn 5) (total 10))
      (let-transformer ()
         (if (= nnn 0)
            total
            (loop (- nnn 1) (+ total 1))
         )
      )
   )
)
