(import '(nexj.core.testing.unit.UnitTestAssertionException nexj.core.util.UncheckedException nexj.core.testing.unit.TimeBox))

; NOTE: The following assertions are defined as macros so that they are not
; treated as tail-recursive calls, which lose the stack trace information.

; Asserts that the expected and actual values are equal in the sense of equal?.
; In case of mismatch, a UnitTestAssertionException is thrown.
; @arg expected any The expected value from the test case.
; @arg actual any The actual value.
; @see equal?
(define-macro (assert-equal expected actual)
   `(begin (##sys:assert-op ##equal? ,expected ,actual) '())
)
; Asserts that the expected and actual values are equal in the sense of eq?.
; In case of mismatch, a UnitTestAssertionException is thrown.
; @arg expected any The expected value from the test case.
; @arg actual any The actual value.
; @see eq?
(define-macro (assert-eq expected actual)
   `(begin (##sys:assert-op ##eq? ,expected ,actual) '())
)

; Asserts that the expected and actual values are equal in the sense of =.
; In case of mismatch, a UnitTestAssertionException is thrown.
; @arg expected any The expected value from the test case.
; @arg actual any The actual value.
; @see =
(define-macro (assert= expected actual)
   `(begin (##sys:assert-op ##= ,expected ,actual) '())
)

(define (sys:assert-op op expected actual)
   (if (not (op expected actual))
      (throw (nexj.core.testing.unit.UnitTestAssertionException'new
         "err.testing.unit.assert" (vector expected actual))
      )
   )
)

; Asserts that a value is null.
; In case of mismatch, a UnitTestAssertionException is thrown.
; @arg actual any The value that is asserted to be null.
(define-macro (assert-null actual)
   `(begin (##sys:assert-null ,actual) '())
)

(define (sys:assert-null actual)
   (if (not (null? actual))
      (throw (nexj.core.testing.unit.UnitTestAssertionException'new
         "err.testing.unit.assertNull" (vector actual))
      )
   )
)

; Asserts that a value is true.
; In case of mismatch, a UnitTestAssertionException is thrown.
; @arg actual any The value that is asserted to be true.
; @arg msg any The error message and arguments.
(define-macro (assert-true actual . args)
   `(begin (##sys:assert-true ,actual ,@args) '())
)

(define (sys:assert-true actual . args)
   (unless actual
      (if (null? args)
         (set! args (list "err.testing.unit.assertTrue"))
      )
      (throw (apply nexj.core.testing.unit.UnitTestAssertionException'new args))
   )
)

; Fails a unit test throwing a UnitTestAssertionException with a specified message.
; @arg message string The message.
(define-macro (fail message)
   `(begin (##sys:fail ,message) '())
)

(define (sys:fail message)
   (throw (nexj.core.testing.unit.UnitTestAssertionException'new message))
)

; Asserts that an exception is thrown.
; @arg exception Exception The expected exception.
; @arg exprs any The expressions that should cause the exception.
; @example 
; (Entity'createEntity "Contact" (: lastName "test")) ;missing firstName
; (assert-raises nexj.core.runtime.ValidationException (commit))
(define-macro (assert-raises exception . exprs)
   (define e (string->symbol "#e"))
   `(##try
      (begin
         ,@exprs
         (##fail (##format "Exception {0} should be thrown." ,(exception'name)))
      )
      (lambda (,e)
         (##unless (##instance? ,e ,exception)
            (##throw ,e)
         )
      )
   )
)

; Asserts that a framework exception with the given error code is thrown.
; @arg errorCode string The expected error code.
; @arg exprs any The expressions that should throw the exception.
; @example 
; (define c (Entity'createEntity "Company" (: lastName "test")))
; (c'company c) ;circular ref
; (assert-error-code "err.entity.circularParent" (commit))
(define-macro (assert-error-code errorCode . exprs)
   (define e (string->symbol "#e"))
   (define e2 (string->symbol "#e2"))   
   (define loop (string->symbol "#loop"))   
   `(##try
      (begin
         ,@exprs
         (##fail (##format "Exception with error code \"{0}\" should be thrown." ,errorCode))
      )
      (lambda (,e)
         (##let ,loop ((,e2 ,e))
            (##unless 
               (##and
                  (##instance? ,e2 nexj.core.util.UncheckedException)
                  (##= (,e2'errorCode) ,errorCode)
               )
               (if (##not (##null? (,e2'cause)))
                  (,loop (,e2'cause))
                  ;else:
                  (##throw ,e2)
               )
            )
         )
      )
   )
)

; Resets the invocation context keeping the current user logged in.
(define (reset-context)
   (define ctx (invocation-context))
   (define env ((ctx'machine)'globalEnvironment))

   (env'clearState)
   (ctx'initialize (ctx'principal) env)
)

; Evaluates a compound expression with a tester hook.
; @arg tester lambda The tester hook: (lambda (uow commit? tx?) ...)
; uow is the current Unit-of-Work
; commit? is #t for commit and #f for rollback
; tx? is #t when the transaction is completed
; Additional functions are available to the tester hook:
; (original-context?) - returns #t if the tester is in the invocation
;    context which has invoked with-tester.
; (with-original-context fun) - evaluates a function in the invocation
;    context which has invoked with-tester.
; @arg body any The expressions to evaluate, from left to right.
; @ret any The value returned by the last evaluated expression.
; @example
; (with-tester
;    (lambda (uow commit? tx?)
;       (logger'debug "uow:" uow "commit?:" commit? "tx?:" tx? "original-context?:" (original-context?))
;       (unless (original-context?)
;          (logger'debug "original-ctx:" (with-original-context invocation-context) "current-ctx:" (invocation-context))
;       )
;    )
;    (SysQueue'invoke ((Contact'read '() () () 1 () ()) 0) 'testAsync)
;    (((invocation-context)'unitOfWork)'commit)
; )
(define-macro (with-tester tester . body)
   (define ctx-saved (string->symbol "#c"))
   (define tester-saved (string->symbol "#t"))
   (define args (string->symbol "#a"))
   `(##let
      (
         (,tester-saved ((##invocation-context)'tester))
      )
      (##let ((,ctx-saved (##invocation-context)))
         (define (with-original-context fun)
            (define new-ctx-saved (invocation-context))
            (##nexj.core.runtime.InvocationContext'threadInvocationContext ,ctx-saved)
            (##try
               ((,ctx-saved'machine)'invoke fun '())
               ()
               (##nexj.core.runtime.InvocationContext'threadInvocationContext new-ctx-saved)
            )
         )
         (define (original-context?)
            (##eq? (##invocation-context) ,ctx-saved)
         )
         ((##invocation-context)'tester 
            (lambda ,args
               (##import '(nexj.core.testing.unit.UnitTestAssertionException nexj.core.util.UncheckedException))
               (##for-each
                  (lambda (p)
                     ((((##invocation-context)'machine)'globalEnvironment)'defineVariable (##car p) (##cdr p))
                  )
                  '((sys:assert-op . ,##sys:assert-op) (sys:assert-null . ,##sys:assert-null)
                    (sys:assert-true . ,##sys:assert-true) (sys:fail . ,##sys:fail) (sys:async-exception . ()))
               )
               (##apply ,tester ,args)
            )
         )
      )
      (##try
         (begin
            ,@body
         )
         ()
         ((##invocation-context)'tester ,tester-saved)
      )
   )
)

; Global flag specifying whether the "DefaultRelationalDatabase" supports multi-version reads 
(define sys:mv-read-flag (vector '()))

; @ret whether the "DefaultRelationalDatabase" supports multi-version reads
; Multi-version reads means that while one transaction wrote a record but did not commit yet, another transaction can read this record.
; Without multi-version reads the latter transaction would block until the former one commits.
(define (mv-read?)
   (when (null? (sys:mv-read-flag 0))
      (let ((version (read-instance SysVersion () () ())))
         (version'loaded (not (version'loaded)))
         (pre-commit)
         (try
            (begin-transaction
               (let ((timeBox (nexj.core.testing.unit.TimeBox'new 1000 (invocation-context) #f)))
                  (read-instance SysVersion () `(!= loaded ,(version'loaded)) ())
                  (timeBox'interrupt)
               )
               (sys:mv-read-flag 0 #t)
            )
            (lambda(e) (sys:mv-read-flag 0 #f))
            ()
         )
         (rollback)
      )
   )
   (sys:mv-read-flag 0)
)
