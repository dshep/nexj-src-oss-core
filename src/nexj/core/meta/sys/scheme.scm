; Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
; Please note that the section labeled as "sort.scm" which defines sorted?, merge, merge!, sort, sort!
; is in the public domain.  See the "sort.scm" section itself for more detailed authorship information.

; Non-intrinsic Scheme language implementation
(declare scope all)

; Defines a new macro - a special function invoked
; during compilation to transform code.
; @arg name symbol Name of the variable storing the macro.
; @arg body any Macro arguments and body, the same syntax as in function definition.
; @ret macro The defined macro.
; @see define define-syntax
; @example
; (define-macro (: name value) (list 'cons (list 'quote name) value))
; This defines a macro with name ":", which generates code
; creating a pair with the quoted name in the head and the value in the tail.
; (expand-macro '(: a (+ 1 2))) => (cons 'a (+ 1 2))
(define define-macro
   (macro (name . body)
      (if (pair? name)
         (if (symbol? (car name))
            ;`(define ,(car name) (macro ,(cdr name) ,@body))
            (cons* 'define (car name) (list (cons* 'macro (cdr name) body)))
            (error "err.scripting.defineMacroSyntax")
         )
         (if (and (symbol? name) (pair? body) (null? (cdr body)) (pair? (car body)) (eq? 'lambda (caar body)))
            ;`(define ,name (macro ,@(cdar body)))
            (cons* 'define name (list (cons 'macro (cdar body))))
            (error "err.scripting.defineMacroSyntax")
         )
      )
   )
)

; Higher-order function for transforming a sequence using another function.
; The function fun is called for each element in the sequence and the
; resulting value is stored into a compatible sequence with the same
; number of elements, which is returned from map.
; @arg fun lambda Transformation function of one argument.
; @arg arg sequence Collection to transform (list, vector or collection).
; @ret sequence The resulting sequence of function return values.
; @example
; (map car '((a 1) (b 2))) => (a b)
; (map (lambda (s) (string-append "[" s "]")) '("a" "b")) => ("[a]" "[b]")
(define (map fun arg)
   (define out (list '()))
   (define (map1 in out)
      (if (not (null? in))
         (begin
            (set-cdr! out (list (fun (car in))))
            (map1 (cdr in) (cdr out))
         )
      )
   )
   (map1 arg out)
   (cdr out)
)

; Higher-order function for iteration.
; The function fun is called for each element in the sequence.
; @arg fun lambda The function of one argument to call.
; @arg arg sequence The sequence to iterate over (list, vector, collection or iteratable).
; @ret ()
; @example
; ; Invokes the compute event on each object in a sequence.
; (for-each (lambda (obj) (obj'compute)) objs)
(define (for-each fun . arg)
   (define (for1 in)
      (if (not (null? in))
         (begin
            (fun (car in))
            (for1 (cdr in))
         )
      )
   )
   (for1 (car arg))
)

(define (sys:verify-let vars exprs sym)
   (if (list? vars)
      (for-each
         (lambda (bind)
            (if (list? bind)
               (if (= (length bind) 2)
                  (if (not (symbol? (car bind)))
                     (error "err.scripting.letSymbol" sym)
                  )
                  (error "err.scripting.letBinding" sym)
               )
               (error "err.scripting.letBinding" sym)
            )
         )
         vars
      )
      (error "err.scripting.letList" sym)
   )
   (if (not (pair? exprs))
      (error "err.scripting.body" sym)
   )
)

; Special form defining a scope with local variables.
; @arg vars pair Variable list of the form ((name1 <expr1>) ... (nameN <exprN>)).
; The expressions specify the initial values of the variables in the scope
; and can only access variables from the enclosing scopes.
; @arg exprs any Expressions to evaluate within the scope.
; @ret any The value of the last expression in the scope.
; @see let* letrec letrec* let-values let*-values
; @example
; (let ((a 1) (b (* 2 3))) (+ a b)) => 7
(define-macro (let vars . exprs)
   (if (symbol? vars)
      (begin
         (if (null? exprs)
            (error "err.scripting.letList" vars)
         )
         (sys:verify-let (car exprs) (cdr exprs) 'let)
         (cons
            (list 
               (list 'lambda (list vars)
                  (list 'set! vars (cons* 'lambda (map car (car exprs)) (cdr exprs)))
               )
               '()
            )
            (map cadr (car exprs))
         )
      )
      (begin
         (sys:verify-let vars exprs 'let)
         (cons (cons* 'lambda (map car vars) exprs) (map cadr vars))
      )
   )
)

; Special form defining a scope with local variables.
; Similar to let, but the variable initializers can refer to the
; variables defined before them in the same scope.
; @arg vars pair Variable list of the form ((name1 <expr1>) ... (nameN <exprN>)).
; The expressions specify the initial values of the variables in the scope
; and can only access variables from the enclosing scopes or preceding variables
; from the current scope.
; @arg exprs any Expressions to evaluate within the scope.
; @ret any The value of the last expression in the scope.
; @see let letrec letrec* let-values let*-values
; @example
; (let* ((a 2) (b (* a 3))) (+ a b)) => 8
(define-macro (let* vars . exprs)
   (sys:verify-let vars exprs 'let*)
   (let loop ((vars vars) (exprs exprs))
      (if (null? vars)
         (list (cons* 'lambda '() exprs))
         (cons (list 'lambda (list (caar vars)) (loop (cdr vars) exprs)) (cdar vars))
      )
   )
)

; Special form defining a scope with local variables.
; Similar to let*, but the variable initializers can refer to any other
; variables in the same scope, as long as they are not accessed during
; the initialization. Identical to letrec, this special form is useful for
; defining mutually recursive functions.
; @arg vars pair Variable list of the form ((name1 <expr1>) ... (nameN <exprN>)).
; The expressions specify the initial values of the variables in the scope
; and can only access variables from the enclosing scopes or other variables
; in the same if they are not accessed during initialization.
; @arg exprs any Expressions to evaluate within the scope.
; @ret any The value of the last expression in the scope.
; @see let let* letrec let-values let*-values
; @example
; (letrec* (
;    (f (lambda (n) (if (zero? n) '() (cons 'a (g (- n 1))))))
;    (g (lambda (n) (if (zero? n) '() (cons 'b (f (- n 1))))))
;    )
;    (f 7)
; ) => (a b a b a b a)
; (letrec* ((a 2) (b (* a 3))) (+ a b)) => 8
(define-macro (letrec* vars . exprs)
   (sys:verify-let vars exprs 'letrec)
   (cons
      (cons*
         'lambda
         (map car vars)
         (let loop ((vars vars))
            (if (null? vars)
               (list (list (cons* 'lambda () exprs)))
               (cons (list 'set! (caar vars) (cadar vars)) (loop (cdr vars)))
            )
         )
      )
      (map (lambda (bind) '()) vars)
   )
)

; Special form defining a scope with local variables.
; Similar to let*, but the variable initializers can refer to any other
; variables in the same scope, as long as they are not accessed during
; the initialization. Identical to letrec*, this special form is useful for
; defining mutually recursive functions.
; @arg vars pair Variable list of the form ((name1 <expr1>) ... (nameN <exprN>)).
; The expressions specify the initial values of the variables in the scope
; and can only access variables from the enclosing scopes or other variables
; in the same if they are not accessed during initialization.
; @arg exprs any Expressions to evaluate within the scope.
; @ret any The value of the last expression in the scope.
; @see let let* letrec* let-values let*-values
; @example
; (letrec (
;    (f (lambda (n) (if (zero? n) '() (cons 'a (g (- n 1))))))
;    (g (lambda (n) (if (zero? n) '() (cons 'b (f (- n 1))))))
;    )
;    (f 7)
; ) => (a b a b a b a)
; (letrec ((a 2) (b (* a 3))) (+ a b)) => 8
(define letrec letrec*)

; Special form computing a logical conjunction.
; The arguments are evaluated from left to right until 
; a false value (#f) is encountered.
; @arg args any Expressions to evaluate from left to right.
; @ret any #f, or the value of the last expression, if #f is not encountered.
; @example
; (define x 3)
; (and (not (zero? x)) (< (/ 2 x) 1)) => #t
; (and (not (zero? x)) x) => 3
; (define x 0)
; (and (not (zero? x)) (< (/ 2 x) 1)) => #f
; (and (not (zero? x)) x) => #f
(define-macro (and . args)
   (let loop ((args args))
      (if (null? args)
         #t
         (if (null? (cdr args))
            (car args)
            (list 'if (car args) (loop (cdr args)) #f)
         )
      )
   )
)

; Special form computing a logical disjunction.
; The arguments are evaluated from left to right until a non-false
; value is encountered.
; @arg args any Expressions to evaluate from left to right.
; @ret any The value of the first non-false expression, or #f if all are false.
; @example
; (define x 7)
; (or (< x 5) (odd? x)) => #t
; (or (< x 5) x) => 7
; (define x 2)
; (or (< x 5) (odd? x)) => #t
; (or (< x 5) x) => #t
(define-macro (or . args)
   (define b (string->symbol "#b"))
   (let loop ((args args))
      (if (null? args)
         #f
         (if (null? (cdr args))
            (car args)
            (list (list 'lambda (list b) (list 'if b b (loop (cdr args)))) (car args))
         )
      )
   )
)

; Special form defining the quasiquote and quasisyntax macros.
; @param op symbol The symbol of the basic operation, either quote or syntax.
; @param quasi-op symbol Either quasiquote or quasisyntax.
; @param un-op symbol Either unquote or unsyntax.
; @param un-op-splicing symbol Either unquote-splicing or unsyntax-splicing.
; @ret macro The new macro, either quasiquote or quasisyntax, depending on the arguments.
(define-macro (sys:quasi-macro op quasi-op un-op un-op-splicing)
   (macro (arg)
      (let qq ((arg arg) (depth 0))
         (if (pair? arg)
            (if (pair? (car arg))
               (if (eq? (caar arg) un-op-splicing) ; un-op-splicing in splicing context
                  (if (pair? (cdar arg))
                     (let
                        (
                           (val
                              (if (null? (cddar arg))
                                 (cadar arg) ; 1 arg
                                 (cons '##append (cdar arg)) ; 2+ args
                              )
                           )
                        )
                        (if (= depth 0)
                           (if (null? (cdr arg))
                              val
                              (list '##append val (qq (cdr arg) depth))
                           )
                           (list '##cons
                              (list '##list (list op un-op-splicing) (qq val (- depth 1)))
                              (qq (cdr arg) depth)
                           )
                        )
                     )
                     (if (null? (cdar arg))
                        (if (null? (cdr arg)) () (qq (cdr arg) depth)) ; 0 arg
                        (error "err.compiler.invalidArgs" un-op-splicing) ; improper list
                     )
                  )
                  (if (eq? (caar arg) un-op) ; un-op in splicing context
                     (if (pair? (cdar arg)) ; 1+ arg
                        (if (= depth 0)
                           (if (null? (cdr arg))
                              (cons '##list (cdar arg))
                              (if (null? (cddar arg))
                                 (list '##cons (cadar arg) (qq (cdr arg) depth)) ; 1 arg
                                 (list '##append (cons '##list (cdar arg)) (qq (cdr arg) depth)) ; 2+ args
                              )
                           )
                           (list '##cons
                              (list '##cons (list op un-op) (qq (cdar arg) (- depth 1)))
                              (qq (cdr arg) depth)
                           )
                        )
                        (if (null? (cdar arg))
                           (if (null? (cdr arg)) () (qq (cdr arg) depth)) ; 0 arg
                           (error "err.compiler.invalidArgs" un-op) ; improper list
                        )
                     )
                     (if
                        (and
                           (eq? (caar arg) 'global)
                           (pair? (cdar arg))
                           (null? (cddar arg))
                           (eq? (cadar arg) quasi-op)
                        )
                        (list '##list (list op quasi-op) (qq (cadr arg) (+ depth 1)))
                        (list '##cons (qq (car arg) depth) (qq (cdr arg) depth))
                     )
                  )
               )
               (if (eq? (car arg) un-op) ; un-op not in splicing context
                  (if (pair? (cdr arg))
                     (if (null? (cddr arg))
                        (if (= depth 0) ; 1 arg
                           (cadr arg)
                           (list '##list (list op un-op) (qq (cadr arg) (- depth 1)))
                        )
                        (error "err.compiler.invalidSplicing" un-op) ; 2+ args
                     )
                     (if (null? (cdr arg))
                        (error "err.compiler.invalidSplicing" un-op) ; 0 arg
                        (error "err.compiler.invalidArgs" un-op) ; improper list
                     )
                  )
                  (if (eq? (car arg) un-op-splicing) ; un-op-splicing not in splicing context
                     (error "err.compiler.invalidSplicing" un-op-splicing)
                     (if (eq? (car arg) quasi-op)
                        (if (pair? (cdr arg))
                           (if (null? (cddr arg))
                              (list '##list (list op quasi-op) (qq (cadr arg) (+ depth 1)))
                              (error "err.compiler.maxArgCount" quasi-op 1)
                           )
                           (error "err.compiler.minArgCount" quasi-op 1)
                        )
                        (if (null? (cdr arg))
                           (list '##list (qq (car arg) depth))
                           (list '##cons (qq (car arg) depth) (qq (cdr arg) depth))
                        )
                     )
                  )
               )
            )
            (if (or (eq? op 'syntax) (symbol? arg))
               (list op arg)
               (if (vector? arg)
                  (list '##list->vector (qq (vector->list arg) depth))
                  arg
               )
            )
         )
      )
   )
)

; Special form substituting values in an S-expression.
; Every occurrence of ,expr or (unquote expr) is replaced by the value of expr.
; Every occurrence of (unquote expr1 ... exprN) is replaced by the exprX items
; put in-line in consecutive positions.
; Every occurrence of ,@expr or (unquote-splicing expr) is replaced by the items
; from the list expr put in-line in consecutive positions.
; Every occurrence of (unquote-splicing expr1 ... exprN) is replaced by the items
; from the lists exprX put in-line in consecutive positions.
; `arg is the syntactic equivalent of (quasiquote arg).
; @arg arg any The S-expression, in which to substitute the values.
; @ret any A new S-expression with substituted values. If might reuse parts of arg.
; @example
; `(a (b ,(+ 1 2) c) d ,(list 4 5 6) e ,@(list 7 8 9) f) =>
;    (a (b 3 c) d (4 5 6) e 7 8 9 f)
; 
; (expand-macro '`(a (b ,(+ 1 2) c) d ,(list 4 5 6) e ,@(list 7 8 9))) =>
;    (cons 'a (cons (cons 'b (cons (+ 1 2) (cons 'c ()))) (cons 'd (cons (list 4 5 6) (cons 'e (list 7 8 9))))))
;
; (let ((a 1) (b 2) (c 3) (d 4))
;    (quasiquote (a d (unquote b) b (unquote c d a b) c))
; ) => (a d 2 b 3 4 1 2 c)
; 
; (let ((a 1) (b 2) (c 3) (d 4))
;    (quasiquote (a d (unquote-splicing (list b)) b (unquote-splicing (list c) (list d a) (list b)) c))
; ) => (a d 2 b 3 4 1 2 c)
(define quasiquote (sys:quasi-macro quote quasiquote unquote unquote-splicing))

(define (sys:verify-let-values bindings exprs sym)
   (if (list? bindings)
      (for-each
         (lambda (bind)
            (if (list? bind)
               (if (= (length bind) 2)
                  (
                     (lambda (vars)
                        (if (not (symbol? vars))
                           (if (pair? vars)
                              (let loop ((vars vars))
                                 (if (pair? vars)
                                    (if (symbol? (car vars))
                                       (loop (cdr vars))
                                       (if (not (symbol? vars))
                                          (error "err.scripting.letValuesBinding" sym)
                                       )
                                    )
                                    (if (not (null? vars))
                                       (if (not (symbol? vars))
                                          (error "err.scripting.letValuesBinding" sym)
                                       )
                                    )
                                 )
                              )
                              (if (not (null? vars)) (error "err.scripting.letValuesBinding" sym))
                           )
                        )
                     )
                     (car bind)
                  )
                  (error "err.scripting.letBinding" sym)
               )
               (error "err.scripting.letBinding" sym)
            )
         )
         bindings
      )
      (error "err.scripting.letList" sym)
   )
   (if (not (pair? exprs))
      (error "err.scripting.body" sym)
   )
)

; Special form defining a scope with local variables that supports multiple return values.
; @arg vars pair A list of bindings. Each binding is of the form (<formals> <values>).
; The values are evaluated and bound to the symbols in formals in the same way that
; formals are bound to arguments in a lambda expression.
; @arg exprs any Expressions to evaluate within the scope.
; @ret any The value of the last expression in the scope.
; @see let let* letrec letrec* let*-values lambda
; @example
; (let-values
;    (
;       ((a b) (values 1 2))
;       ((c . d) (values 3 4 5))
;       ((e) 6)
;       (f (values 7 8))
;       (g 9)
;    )
;    (list a b c d e f g)
; ) => (1 2 3 (4 5) 6 (7 8) (9))
(define-macro (let-values vars . exprs)
   (sys:verify-let-values vars exprs 'let-values)
   (if (null? (cdr vars))
      (let ((args (caar vars)))
         (if (and (pair? args) (null? (cdr args)))
            `((lambda ,args ,@exprs) ,(cadar vars))
            `(##call-with-values
                (lambda () ,(cadar vars))
                (lambda ,args ,@exprs)
             )
         )
      )
      (let*
         (
            (temp-var ())
            (temps ())
            (syms ())
            (i -1)
            (value-setters
               (map
                  (lambda (binding)
                     (let*
                        (
                           (args (car binding))
                           (binding-syms
                              (let loop ((args args))
                                 (if (pair? args)
                                    (let ((sym (car args)))
                                       (set! syms (cons sym syms))
                                       (cons sym (loop (cdr args)))
                                    )
                                    (if (symbol? args)
                                       (begin
                                          (set! syms (cons args syms))
                                          (list args)
                                       )
                                    )
                                 )
                              )
                           )
                        )
                        (if (and (pair? args) (null? (cdr args)))
                            (begin
                               (set! i (+ i 1))
                               (set! temp-var (string->symbol (string-append "#v" (number->string i))))
                               (set! temps (cons temp-var temps))
                               `(set! ,temp-var ,(cadr binding))
                            )
                           `(##call-with-values
                               (lambda () ,(cadr binding))
                               (lambda ,args
                                  ,@(map
                                       (lambda (sym)
                                          (set! i (+ i 1))
                                          (set! temp-var (string->symbol (string-append "#v" (number->string i))))
                                          (set! temps (cons temp-var temps))
                                          `(set! ,temp-var ,sym)
                                       )
                                       binding-syms
                                    )
                               )
                           )
                        )
                     )
                  )
                  vars
               )
            )
         )
         (set! temps (reverse! temps))
         `((lambda ,temps
             ,@value-setters
             ((lambda ,(reverse! syms) ,@exprs) ,@temps)
           )
           ,@(map (lambda (v) ()) temps)
         )
      )
   )
)

; Special form defining a scope with local variables that supports multiple return values.
; Similar to let-values, but the variable initializers can refer to the variables defined
; before them in the same scope.
; @arg vars pair A list of bindings. Each binding is of the form (<formals> <values>).
; The values are evaluated and bound to the symbols in formals in the same way that
; formals are bound to arguments in a lambda expression.
; @arg exprs any Expressions to evaluate within the scope.
; @ret any The value of the last expression in the scope.
; @see let let* letrec letrec* let-values lambda
; @example
; (let*-values
;    (
;       ((a b) (values 1 2))
;       ((c . d) (values (+ a b) 4 5))
;       ((e) 6)
;       (f (values 7 8))
;       (g (* c c))
;    )
;    (list a b c d e f g)
; ) => (1 2 3 (4 5) 6 (7 8) (9))
(define-macro (let*-values vars . exprs)
   (sys:verify-let-values vars exprs 'let*-values)
   (if (null? (cdr vars))
      (let ((args (caar vars)))
         (if (and (pair? args) (null? (cdr args)))
            `((lambda ,args ,@exprs) ,(cadar vars))
            `(##call-with-values
                (lambda () ,(cadar vars))
                (lambda ,(caar vars) ,@exprs)
             )
         )
      )
      (let loop ((vars vars))
         (if (null? vars)
            `((lambda () ,@exprs))
            `(##call-with-values
               (lambda () ,@(cdar vars))
               (lambda ,(caar vars) ,(loop (cdr vars)))
             )
         )
      )
   )
)

; Special form wrapping the value(s) obtained from evaluating the given expression
; in a list.
; @arg expr any An expression to evaluate. May return any number of values.
; @ret list A list containing the value(s) obtained from evaluating expr, in order.
; @example
; (values->list (partition even? '(0 1 2 3 4 5 6))) => ((0 2 4 6) (1 3 5))
; (values->list (filter odd? #(0 1 2 3 4 5 6))) => (#(1 3 5))
(define-macro (values->list expr)
   `(call-with-values (lambda () ,expr) (lambda result result))
)

; Special form evaluating sequentially conditional clauses until
; a test condition with non-false value is encountered.
; @arg list clauses The conditional clauses, each in one of the following forms:
; (<test>) - if the value of <test> is not #f, it is returned;
; (<test> ... <result>) - if the value of <test> is not #f, the value of <result> is returned;
; (<test> => <fun>) - if the value of <test> is not #f, the value of (<fun> <test>) is returned;
; (else <result>) - the value of <result> is returned (this must be the last clause).
; @ret any The result of the evaluation, or () if no non-false test condition is encountered.
; @see case if
; @example
; (define x 3)
; (cond
;    ((< x 0) 'negative)
;    ((= x 0) 'zero)
;    (else 'positive)
; ) => positive
; (cond ((and (>= x 0) 'non-negative)) (else 'negative)) => non-negative
; (cond ((assoc x '((-1 . a) (1 . b) (3 . c))) => cdr)) => c
(define-macro (cond . clauses)
   (let loop ((clauses clauses))
      (if (null? clauses)
         ''()
         (let ((clause (car clauses)))
            (if (and (pair? clause) (list? clause))
               (if (eq? (car clause) 'else)
                  (if (null? (cdr clauses))
                     ; (cond (else ...) ...)
                     `(begin ,@(cdr clause))
                     (error "err.scripting.condElse")
                  )
                  (if (null? (cdr clause))
                     ; (cond (test) ...)
                     `(##or ,(car clause) ,(loop (cdr clauses)))
                     (if (eq? (cadr clause) '=>)
                        (if (= (length clause) 3)
                           ; (cond (test => recipient) ...)
                           `((lambda (,(string->symbol "#v"))
                              (if ,(string->symbol "#v")
                                 (,(caddr clause) ,(string->symbol "#v"))
                                 ,(loop (cdr clauses))
                              )
                             )
                             ,(car clause)
                            )
                           (error "err.scripting.condRecipient")
                        )
                        ; (cond (test ...) ...)
                        `(if ,(car clause) (begin ,@(cdr clause)) ,(loop (cdr clauses)))
                     )
                  )
               )
               (error "err.scripting.condClause")
            )
         )
      )
   )
)

; Special form selectivaly evaluating one of several expressions
; based on a match of an expression value with a constant using equal? comparison.
; @arg expr any The expression providing the value to compare against the constants.
; @arg clauses list Match clauses, each in one of the following forms:
; ((<const1> ... <constN>) ... <result>) - if the expr value is equal to one of
; the <const> constants, the value of <result> is returned.
; (else <result>) - the value of <result> is returned (this must be the last clause).
; @ret any The result of the evaluation, or () if there is no match.
; @see cond if
; @example
; (case (+ 1 2)
;    ((0 1 2) 'low)
;    ((3 4 5) 'high)
;    (else '?)
; ) => high
; (define s "a")
; (case s (("a" "b" "c") 1) (("d" "e" "f") 2) (else 3)) => 1
; (case '(* x x) (((* x 1) (* 1 x)) 'x) (((* x x) (sqr x)) 'x^2)) => x^2
(define-macro (case expr . clauses)
   (define key (string->symbol "#k"))
   `(##let ((,key ,expr))
      ,(let loop ((clauses clauses))
         (if (null? clauses)
            ''()
            (let ((clause (car clauses)))
               (if (and (list? clause) (not (null? (cdr clause))))
                  (cond
                     ((and (pair? (car clause)) (list? (car clause)))
                        ; (case key ((datum ...) ...) ...)
                        (if (null? (cdar clause))
                           `(if (##equal? ,key ',(caar clause)) (begin ,@(cdr clause)) ,(loop (cdr clauses)))
                           `(if (##member ,key ',(car clause)) (begin ,@(cdr clause)) ,(loop (cdr clauses)))
                        )
                     )
                     ((eq? (car clause) 'else)
                        ; (case key (else ...) ...)
                        (if (null? (cdr clauses))
                           `(begin ,@(cdr clause))
                           (error "err.scripting.caseElse")
                        )
                     )
                     (else
                        (error "err.scripting.caseDatum")
                     )
                  )
                  (error "err.scripting.caseClause")
               )
            )
         )
      )
   )
)

; Special form implementing iteration.
; @arg vars list Iteration variable declaration of the form
; ((name1 <init1> <step1>) ... (nameN <initN> <stepN>)).
; The <init> expressions are the initial values of the iteration variables.
; The optional <step> expressions are evaluated and assigned to the variables
; before each consecutive iteration after the first one.
; @arg test list The test clause of the form (<test> ... <result>).
; The test expression is evaluated before each iteration, and if
; not equal to #f, the value of <result> is returned.
; @arg body any Expressions evaluated on each iteration.
; @ret any The value of <result>, or () if no result expression is specified.
; @example
; (do (
;        (i 10 (- i 1))
;        (lst '() (cons i lst))
;     )
;     ((= i 0) lst)
;     (set! lst (cons (* i i) lst))
; ) => (1 1 2 4 3 9 4 16 5 25 6 36 7 49 8 64 9 81 10 100) 
(define-macro (do vars test . body)
   (define label (string->symbol "#l"))
   (if (list? vars)
      (if (and (list? test) (not (null? test)))
         `(##let ,label
            ,(map
               (lambda (var)
                  (if (and (list? var) (<= 2 (length var) 3))
                     (if (symbol? (car var))
                        (list (car var) (cadr var))
                        (error "err.scripting.doSymbol")
                     )
                     (error "err.scripting.doBinding")
                  )
               )
               vars
             )
            (if ,(car test)
               (begin
                  ,@(if (null? (cdr test))
                     '('())
                     (cdr test)
                    )
                )
                (begin
                   ,@body
                   (,label
                      ,@(map
                         (lambda (var)
                            (if (null? (cddr var))
                               (car var)
                               (caddr var)
                            )
                         )
                         vars
                        )
                   )
                )
            )
          )
         (error "err.scripting.doTest")
      )
      (error "err.scripting.doVars")
   )
)

; Special form implementing iteration.
; @arg test any Expression evaluated before each iteration.
; If equal to #f, the iterations stop and () is returned.
; @arg body any Expressions to evaluate on each iteration.
; @ret ()
; @example
; (define n 0)
; (define lst '())
; (while (< n 10)
;    (set! n (+ n 1))
;    (set! lst (cons n lst))
; ) => ()
; lst => (10 9 8 7 6 5 4 3 2 1)
(define-macro (while test . body)
   (define label (string->symbol "#l"))
   `(##let ,label ()
      (if ,test
         (begin
            ,@body
            (,label)
         )
      )
    )
)

; Special form implementing iteration.
; @arg pre list Iteration variable declaration of the form
; ((name1 <init1>) ... (nameN <initN>)), where <init> are the
; initial values of the iteration variables.
; @arg test any Expression evaluated before each iteration.
; If equal to #f, the iterations stop and () is returned.
; @arg post any Expression evaluated after each iteration.
; @arg body any Expressions evaluated on each iteration.
; @ret ()
; @example
; (define lst '())
; (for ((i 0)) (< i 10) (set! i (+ i 1))
;    (set! lst (cons i lst))
; ) => ()
; lst => (9 8 7 6 5 4 3 2 1 0)
(define-macro (for pre test post . body)
   (define label (string->symbol "#l"))
   `(##let ,pre
      (##let ,label ()
         (if ,test
            (begin
               ,@body
               ,post
               (,label)
            )
         )
      )
    )
)

; Special form conditionally evaluating a list of expressions.
; @arg test any The test expression. If its value is not #f,
; the body is evaluated.
; @arg body any The expressions evaluated if the test condition is not false.
; @ret any The value of the last expression in body, or () if the test condition is false.
; @example
; (define x 0)
; (when (>= x 0)
;    (set! x (+ x 1))
;    (set! x (+ x x))
; ) => 2
; x => 2
(define-macro (when test . body)
   `(if ,test (begin ,@body))
)

; Special form conditionally evaluating a list of expressions.
; @arg test any The test expression. If its value is #f,
; the body is evaluated.
; @arg body any The expressions evaluated if the test condition is false.
; @ret any The value of the last expression in body, or () if the test condition is not false.
; @example
; (define x 0)
; (unless (< x 0)
;    (set! x (+ x 1))
;    (set! x (+ x x))
; ) => 2
; x => 2
(define-macro (unless test . body)
   `(if (##not ,test) (begin ,@body))
)

(define (sys:delay expr)
   (let ((done? #f) (value #f))
      (lambda ()
         (if done?
            value
            (let ((v (expr)))
               (if done?
                  value
                  (begin
                     (set! done? #t)
                     (set! value v)
                     v
                  )
               )
            )
         )
      )
   )   
)

; Special form constructing lazy evaluation objects.
; @arg expr any Expression which evaluation will be delayed.
; @ret any Special object called promise, which will evaluate the 
; expression once and return its value when (force <promise>) is invoked on it.
; @example
; (define x 1)
; (define y 0)
; (define promise (delay (/ x y)))
; (set! y 2)
; (force promise) => 0.5
; (set! y 1)
; (force promise) => 0.5
(define-macro (delay expr)
   `(',sys:delay (lambda () ,expr))
)

; Special form for exception handling.
; @arg expr any Expression to evaluate.
; @arg catch lambda Function of one argument invoked
; when an exception is thrown from expr. () means no handler.
; @arg finally any Expressions to evaluate after expr and catch.
; @ret any The value of expr or the value returned
; by catch if an exception has occured.
; @example
; (define done? #f)
; (try
;    (/ 1 0)
;    (lambda (e) 'inf)
;    (set! done? #t)
; ) => inf
(define-macro (try expr catch . finally)
   (if (null? catch)
      (set! catch '(lambda (e) (throw e)))
   )
   (if (null? finally)
      (set! finally '('()))
   )
   `(##sys:try (lambda () ,expr) ,catch (lambda () ,@finally))
)

; Sets up a function to call before entering and a function to call after exiting
; the dynamic extent of the main function, and calls these functions in order.
; @arg preFun lambda The function to invoke before entering the dynamic extent
; of the call to fun. Takes no arguments and may return any number of values.
; @arg fun lambda The function to invoke. Takes no arguments and may return any
; number of values.
; @arg postFun lambda The function to invoke after exiting the dynamic extent
; of the call to fun. Takes no arguments and may return any number of values.
; @ret any The return value(s) of invoking fun with no arguments.
; @see call/cc call-with-current-continuation
; @example
; (define cont '())
; (dynamic-wind
;    (lambda () (display 'before) (newline))
;    (lambda ()
;       (display 'main) (newline)
;       (+ 1 (call/cc (lambda (c) (set! cont c) 10)))
;    )
;    (lambda () (display 'after))
; )
;    before
;    main
;    after
; => 11
; 
; (cont 100)
;    before
;    after
; => 101
; 
; (dynamic-wind
;    (lambda () (display 'before2) (newline))
;    (lambda () (display 'main2) (newline) (cont 1000))
;    (lambda () (display 'after2) (newline))
; )
;    before2
;    main2
;    after2
;    before
;    after
; => 1001
; 
; (dynamic-wind
;    (lambda () (display 'before) (newline))
;    (lambda () (display 'main) (newline)
;       (dynamic-wind
;          (lambda () (display 'before2) (newline))
;          (lambda ()
;             (display 'main2) (newline)
;             (+ 1 (call/cc (lambda (c) (set! cont c) 10)))
;          )
;          (lambda () (display 'after2) (newline))
;       )
;    )
;    (lambda () (display 'after))
; )
;    before
;    main
;    before2
;    main2
;    after2
;    after
; => 11
; 
; (cont 100)
;    before
;    before2
;    after2
;    after
; => 101
(define-macro (dynamic-wind preFun fun postFun)
   (define vals (string->symbol "#vals"))
   `(begin
      (,preFun)
      (let ((,vals #f))
         (##sys:try
            (lambda () (set! ,vals (values->list (,fun))))
            ,preFun
            ,postFun
            #t
         )
         (apply values ,vals)
      )
   )
)

; Sets up a continuation barrier and calls a function within its context.
; @arg fun lambda Function to invoke.
; @ret any The value returned by the function above.
; @example
; (define cont '())
; (let ((n 0))
;    (call-with-continuation-barrier
;       (lambda () (call/cc (lambda (c) (set! cont c))))
;    )
;    (set! n (+ n 1))
;    n
; ) => 1
; (cont 0) => 0
(define (call-with-continuation-barrier fun)
   (define barrier (sys:set-barrier))
   (try
      (fun)
      ()
      (sys:set-barrier barrier)
   )
)

; Opens an existing file as a character input stream
; with UTF-8 encoding and passes it to a function.
; @arg url string The file name or URL.
; @arg fun lambda Function of one argument receiving the input stream.
; @ret any The value returned by the function above.
; @example
; (call-with-input-file "c:/Work/test.txt"
;    (lambda (in)
;       (cons 'a (read in))
;    )
; ) => (a 1 2 3) ; assuming that the file contains (1 2 3)
; (call-with-input-file "c:/Work/test.txt" read) => (1 2 3)
(define (call-with-input-file url fun)
   (let ((port (open-input-file url)))
      (try
         (fun port)
         ()
         (close-input-port port)
      )
   )
)

; Creates or replaces a file and passes the character
; output stream with UTF-8 encoding to a function.
; @arg url string The file name or URL.
; @arg fun lambda Function of one argument receiving the output stream.
; @ret any The value returned by the function above.
; @example
; (call-with-output-file "c:/Work/test.txt"
;    (lambda (out)
;       (write '(1 2 3) out)
;    )
; ) => ()
(define (call-with-output-file url fun)
   (let ((port (open-output-file url)))
      (try
         (fun port)
         ()
         (close-output-port port)
      )
   )
)

; Opens an existing file as a character input stream with UTF-8
; encoding, sets it temporarily as the default input stream
; and calls a function.
; @arg url string The file name or URL.
; @arg fun lambda Function without arguments.
; @ret any The value returned by the function above.
; @example
; (with-input-from-file "c:/Work/test.txt"
;    (lambda ()
;       (cons 'a (read))
;    )
; ) => (a 1 2 3) ; assuming that the file contains (1 2 3)
; (with-input-from-file "c:/Work/test.txt" read) => (1 2 3)
(define (with-input-from-file url fun)
   (let ((port (set-current-input-port! (open-input-file url))))
      (try
         (fun)
         ()
         (close-input-port (set-current-input-port! port))
      )
   )
)

; Creates or replaces a file, temporarily sets the character output
; stream with UTF-8 encoding as a default and calls a function.
; @arg url string The file name or URL.
; @arg fun lambda Function without arguments.
; @ret any The value returned by the function above.
; @example
; (with-output-to-file "c:/Work/test.txt"
;    (lambda ()
;       (write '(1 2 3))
;    )
; ) => ()
(define (with-output-to-file url fun)
   (let ((port (set-current-output-port! (open-output-file url))))
      (try
         (fun)
         ()
         (close-output-port (set-current-output-port! port))
      )
   )
)

(define (sys:expand-macro expr env recur? once?)
   (let loop ((expr expr))
      (if (pair? expr)
         (if (symbol? (car expr))
            (case (car expr)
               ((quote global declare)
                  expr
               )
               ((set! define lambda macro)
                  (if (and recur? (pair? (cdr expr)) (pair? (cddr expr)))
                     (cons* (car expr) (cadr expr) (map loop (cddr expr)))
                     expr
                  )
               )
               (else
                  (let ((m (env'findVariable (car expr))))
                     (if (macro? m)
                        (begin
                           (set! expr (apply m (cdr expr)))
                           (if recur?
                              (loop expr)
                              (if once?
                                 expr
                                 (loop expr)
                              )
                           )
                        )
                        (if recur?
                           (map loop expr)
                           expr
                        )
                     )
                  )
               )
            )
            (if recur?
               (map loop expr)
               expr
            )
         )
         expr
      )
   )
)

; Expands a macro and all top-level macros returned by its expansion.
; @arg expr any The S-expression to expand.
; @ret any The expanded expression.
; @example
; (expand-macro '(while (@ a) (@ b))) =>
;    (((lambda (#l) (set! #l (lambda () (if (@ a) (begin (@ b) (#l)))))) ()))
(define (expand-macro expr)
   (sys:expand-macro expr (interaction-environment) #f #f)
)

; Expands a macro without expanding the macros returned by it.
; @arg expr any The S-expression to expand.
; @ret any The expanded expression.
; @example
; (expand-macro-1 '(while (@ a) (@ b))) =>
;    (let #l () (if (@ a) (begin (@ b) (#l))))
(define (expand-macro-1 expr)
   (sys:expand-macro expr (interaction-environment) #f #t)
)

; Higher-order function for transforming a sequence using another function.
; The function fun is called for each element in the sequence and the
; resulting value is stored into a compatible sequence with the same
; number of elements, which is returned from map.
; @arg fun lambda The transformation function of as many arguments as there are sequences.
; @arg arg sequence The sequence to transform (list, vector or collection).
; @arg args sequence Additional parallel sequences of the same type to transform.
; @ret sequence The resulting sequence of function return values.
; @example
; (map car '((a 1) (b 2))) => (a b)
; (map (lambda (s) (string-append "[" s "]")) '("a" "b")) => ("[a]" "[b]")
; (map * '(1 2) '(3 4)) => (3 8)
(define (map fun arg . args)
   (if (null? args)
      (cond
         ((pair? arg)
            (let ((out (list '())))
               (let loop ((in arg) (out out))
                  (if (not (null? in))
                     (begin
                        (set-cdr! out (list (fun (car in))))
                        (loop (cdr in) (cdr out))
                     )
                  )
               )
               (cdr out)
            )
         )
         ((null? arg)
            '()
         )
         ((collection? arg)
            (let ((out (make-collection (vector-length arg))) (itr (iterator arg)))
               (let loop ()
                  (if (itr'hasNext)
                     (begin
                        (out'add (fun (itr'next)))
                        (loop)
                     )
                  )
               )
               out
            )
         )
         (else ;; vector
            (let* ((n (vector-length arg)) (out (make-vector n)))
               (do
                  ((i 0 (+ i 1)))
                  ((= i n) out)
                  (vector-set! out i (fun (vector-ref arg i)))
               )
            )
         )
      )
      (cond
         ((pair? arg)
            (let ((out (list '())))
               (let loop ((in arg) (ins args) (out out))
                  (if (not (null? in))
                     (begin
                        (set-cdr! out (list (apply fun (car in) (map car ins))))
                        (loop (cdr in) (map cdr ins) (cdr out))
                     )
                  )
               )
               (cdr out)
            )
         )
         ((null? arg)
            '()
         )
         ((collection? arg)
            (let
               (
                  (out (make-collection (vector-length arg)))
                  (itr (arg'iterator))
                  (itrs (map iterator args))
               )
               (let loop ()
                  (if (itr'hasNext)
                     (begin
                        (out'add (apply fun (itr'next) (map (lambda (a) (a'next)) itrs)))
                        (loop)
                     )
                  )
               )
               out
            )
         )
         (else ;; vector
            (let ((n (vector-length arg)))
               (let
                  (
                     (out (make-vector n))
                     (itrs (map iterator args))
                  )
                  (do
                     ((i 0 (+ i 1)))
                     ((= i n) out)
                     (vector-set! out i (apply fun (vector-ref arg i) (map (lambda (a) (a'next)) itrs)))
                  )
               )
            )
         )
      )
   )
)

; Higher-order function for transforming a sequence using another function.
; The function fun is called for each element in the sequence and the
; resulting value is stored into a compatible sequence with the same
; number of elements, which is returned from map.
; Similar to map. Added for R6RS compliance.
; @arg fun lambda The transformation function of as many arguments as there are sequences.
; @arg arg sequence The sequence to transform (list, vector or collection).
; @arg args sequence Additional parallel sequences to transform.
; @ret sequence The resulting sequence of function return values.
; @see map
; @example
; (vector-map car #((a 1) (b 2))) => #(a b)
; (vector-map (lambda (s) (string-append "[" s "]")) #("a" "b")) => #("[a]" "[b]")
; (vector-map * #(1 2) #(3 4)) => #(3 8)
(define vector-map map)

; Higher-order function for filtering a sequence with a predicate.
; @arg fun lambda The predicate, invoked on each element of the
; sequence. Only the elements for which the predicate returns
; non-#f are returned in the resulting sequence
; @arg arg sequence The sequence to filter (list, vector or collection).
; @ret sequence New sequence containing the filtered out items.
; @example
; (filter positive? '(-1 1 0 2 3)) => (1 2 3)
; (filter (lambda (x) (<= (* x x) 9)) #(-2 1 3 4)) => #(-2 1 3)
(define (filter fun arg)
   (cond
      ((pair? arg)
         (let ((out (list '())))
            (let loop ((in arg) (out out))
               (if (not (null? in))
                  (begin
                     (if (fun (car in))
                        (begin
                           (set-cdr! out (list (car in)))
                           (loop (cdr in) (cdr out))
                        )
                        (loop (cdr in) out)
                     )
                  )
               )
            )
            (cdr out)
         )
      )
      ((null? arg)
         '()
      )
      ((collection? arg)
         (let ((out (make-collection arg)) (itr (iterator arg)))
            (let loop ()
               (if (itr'hasNext)
                  (begin
                     (set! arg (itr'next))
                     (if (fun arg)
                        (out'add arg)
                     )
                     (loop)
                  )
               )
            )
            out
         )
      )
      (else ;; vector
         (let ((n (vector-length arg)))
            (let ((out (make-collection n)) (item '()))
               (do
                  ((i 0 (+ i 1)))
                  ((= i n) (out'toArray))
                  (set! item (vector-ref arg i))
                  (if (fun item)
                     (out'add item)
                  )
               )
            )
         )
      )
   )
)

; Higher-order function for partitioning a sequence with a predicate.
; @arg fun lambda The predicate, invoked on each element of the sequence.
; @arg arg sequence The sequence to partition (list, vector or collection).
; @ret sequence Two sequences, one containing the items for which fun returns
; true values, and one containing the items for which fun returns #f.
; @example
; (values->list (partition positive? '(-1 1 0 2 3))) => ((1 2 3) (-1 0))
; (values->list (partition (lambda (x) (<= (* x x) 9)) #(-2 1 3 4))) => (#(-2 1 3) #(4))
(define (partition fun arg)
   (cond
      ((pair? arg)
         (let ((out1 (list '())) (out2 (list '())))
            (let loop ((in arg) (out1 out1) (out2 out2))
               (if (not (null? in))
                  (begin
                     (if (fun (car in))
                        (begin
                           (set-cdr! out1 (list (car in)))
                           (loop (cdr in) (cdr out1) out2)
                        )
                        (begin
                           (set-cdr! out2 (list (car in)))
                           (loop (cdr in) out1 (cdr out2))
                        )
                     )
                  )
               )
            )
            (values (cdr out1) (cdr out2))
         )
      )
      ((null? arg)
         (values '() '())
      )
      ((collection? arg)
         (let ((out1 (make-collection arg)) (out2 (make-collection arg)) (itr (iterator arg)))
            (let loop ()
               (when (itr'hasNext)
                  (set! arg (itr'next))
                  (if (fun arg)
                     (out1'add arg)
                     (out2'add arg)
                  )
                  (loop)
               )
            )
            (values out1 out2)
         )
      )
      (else ;; vector
         (let ((n (vector-length arg)))
            (let ((out1 (make-collection n)) (out2 (make-collection n)) (item '()))
               (do
                  ((i 0 (+ i 1)))
                  ((= i n) (values (out1'toArray) (out2'toArray)))
                  (set! item (vector-ref arg i))
                  (if (fun item)
                     (out1'add item)
                     (out2'add item)
                  )
               )
            )
         )
      )
   )
)

; Higher-order function for iteration.
; The function fun is called for each element in a sequence.
; @arg fun lambda The function to call, of as many arguments as there are sequences.
; @arg arg sequence The sequence to iterate over.
; @arg args sequence Additional parallel sequences of the same type to iterate over.
; @ret ()
; @example
; ; Invokes the compute event on each object in a sequence.
; (for-each (lambda (obj) (obj'compute)) objs)
; (for-each (lambda (x y) (display (+ x y)) (newline)) (iterator '(1 2 3)) (iterator #(4 5 6))) ; Different sequence types
(define (for-each fun arg . args)
   (if (null? args)
      (if (iteratable? arg)
         (let ((itr (iterator arg)))
            (let loop ()
               (if (itr'hasNext)
                  (begin
                     (fun (itr'next))
                     (loop)
                  )
               )
            )
         )
         (let loop ((in arg))
            (if (not (null? in))
               (begin
                  (fun (car in))
                  (loop (cdr in))
               )
            )
         )
      )
      (if (iteratable? arg)
         (let ((itr (iterator arg)) (itrs (map iterator args)))
            (let loop ()
               (if (itr'hasNext)
                  (begin
                     (apply fun (itr'next) (map (lambda (a) (a'next)) itrs))
                     (loop)
                  )
               )
            )
         )
         (let loop ((in arg) (ins args))
            (if (not (null? in))
               (begin
                  (apply fun (car in) (map car ins))
                  (loop (cdr in) (map cdr ins))
               )
            )
         )
      )
   )
)

; Higher-order function for iteration on one vector or several vectors of the same length.
; The function fun is called on each set of elements at the same index in the vectors.
; @arg fun lambda The function to call, which must accept as many arguments as there are vectors.
; @arg arg vector The vector to iterate over.
; @arg args vector Additional parallel vectors to iterate over.
; @ret ()
; @example
; (define lst '())
; (vector-for-each (lambda args (set! lst (append lst (list args)))) #(1 2 3) #(a b c)) => '()
; lst => ((1 a) (2 b) (3 c))
(define (vector-for-each fun arg . args)
   (let ((count (vector-length arg)))
      (if (null? args)
         (do
            ((i 0 (+ i 1)))
            ((= i count) '())
            (fun (arg i))
         )
         (if (apply = count (map vector-length args))
            (do
               ((i 0 (+ i 1)))
               ((= i count) '())
               (apply fun (arg i) (map (lambda (vec) (vector-ref vec i)) args))
            )
            (error "fail.scripting.argLengthMismatch" 'vector-for-each)
         )
      )
   )
)

; Higher-order function for iteration on one string or several strings of the same length.
; The function fun is called on each set of characters at the same index in the strings.
; @arg fun lambda The function to call, which must accept as many character arguments as there are strings.
; @arg arg string The string to iterate over.
; @arg args string Additional parallel strings to iterate over.
; @ret ()
; @example
; (define lst '())
; (string-for-each (lambda chars (set! lst (cons (apply string chars) lst))) "abcd" "test") => '()
; lst => ("dt" "cs" "be" "at")
(define (string-for-each fun arg . args)
   (let ((count (string-length arg)))
      (if (null? args)
         (do
            ((i 0 (+ i 1)))
            ((= i count) '())
            (fun (string-ref arg i))
         )
         (if (apply = count (map string-length args))
            (do
               ((i 0 (+ i 1)))
               ((= i count) '())
               (apply fun (string-ref arg i) (map (lambda (str) (string-ref str i)) args))
            )
            (error "fail.scripting.argLengthMismatch" 'string-for-each)
         )
      )
   )
)

; Higher-order function for conditional iteration or search.
; The function fun is called for each element in a sequence
; until it returns a value different from #f, which is returned
; as a result.
; @arg fun lambda The function to invoke, of as many arguments as there are sequences.
; @arg arg sequence The sequence to iterate over (list, vector, collection or iteratable).
; @arg args sequence Additional parallel sequences of the same type to iterate over.
; @ret any The value returned from fun, or #f if fun has returned #f
; for all the elements in the sequence.
; @see exists
; @example
; (some negative? '(1 2 3 0 -1 -2)) => #t
; (some negative? '(1 2 3 0)) => #f
; (some 
;    (lambda (x y)
;       (if (> (+ (* x x) (* y y)) 10)
;          (list x y)
;          #f
;       )
;    )
;    #(-1 2 3 4)
;    #(-2 -3 -4 -5)
; ) => (2 -3)
(define (some fun arg . args)
   (if (null? args)
      (if (iteratable? arg)
         (let ((itr (iterator arg)))
            (let loop ()
               (cond
                  ((not (itr'hasNext)) #f)
                  ((fun (itr'next)))
                  (else (loop))
               )
            )
         )
         (let loop ((in arg))
            (cond
               ((null? in) #f)
               ((fun (car in)))
               (else (loop (cdr in)))
            )
         )
      )
      (if (iteratable? arg)
         (let ((itr (iterator arg)) (itrs (map iterator args)))
            (let loop ()
               (cond
                  ((not (itr'hasNext)) #f)
                  ((apply fun (itr'next) (map (lambda (a) (a'next)) itrs)))
                  (else (loop))
               )
            )
         )
         (let loop ((in arg) (ins args))
            (cond
               ((null? in) #f)
               ((apply fun (car in) (map car ins)))
               (else (loop (cdr in) (map cdr ins)))
            )
         )
      )
   )
)

; Higher-order function for conditional iteration or search.
; The function fun is called for each element in a sequence
; until it returns a value different from #f, which is returned
; as a result. Similar to some. Added for R6RS compliance.
; @arg fun lambda The function to invoke, of as many arguments as there are sequences.
; @arg arg sequence The sequence to iterate over (list, vector, collection or iteratable).
; @arg args sequence Additional parallel sequences to iterate over.
; @ret any The value returned from fun, or #f if fun has returned #f
; for all the elements in the sequence.
; @see some
; @example
; (exists negative? '(1 2 3 0 -1 -2)) => #t
; (exists negative? '(1 2 3 0)) => #f
; (exists 
;    (lambda (x y)
;       (if (> (+ (* x x) (* y y)) 10)
;          (list x y)
;          #f
;       )
;    )
;    #(-1 2 3 4)
;    #(-2 -3 -4 -5)
; ) => (2 -3)
(define exists some)

; Higher-order function for conditional iteration or search.
; The function fun is called for each element in a sequence
; until it returns #f.
; @arg fun lambda The function to invoke, of as many arguments as there are sequences.
; @arg arg sequence The sequence to iterate over (list, vector, collection or iteratable).
; @arg args sequence Additional parallel sequences of the same type to iterate over.
; @ret boolean #t if fun has returned non-false for all the elements in the sequence, #f otherwise.
; @example
; (every positive? '(1 2 3 0 4)) => #f
; (every positive? '(1 2 3 4 5)) => #t
; (every < #(1 3 2 4) #(2 5 3 6)) => #t
(define (every fun arg . args)
   (if (null? args)
      (if (iteratable? arg)
         (let ((itr (iterator arg)))
            (let loop ()
               (cond
                  ((not (itr'hasNext)) #t)
                  ((fun (itr'next)) (loop))
                  (else #f)
               )
            )
         )
         (let loop ((in arg))
            (cond
               ((null? in) #t)
               ((fun (car in)) (loop (cdr in)))
               (else #f)
            )
         )
      )
      (if (iteratable? arg)
         (let ((itr (iterator arg)) (itrs (map iterator args)))
            (let loop ()
               (cond
                  ((not (itr'hasNext)) #t)
                  ((apply fun (itr'next) (map (lambda (a) (a'next)) itrs)) (loop))
                  (else #f)
               )
            )
         )
         (let loop ((in arg) (ins args))
            (cond
               ((null? in) #t)
               ((apply fun (car in) (map car ins)) (loop (cdr in) (map cdr ins)))
               (else #f)
            )
         )
      )
   )
)

; Higher-order function for conditional iteration or search.
; The function fun is called for each element in a sequence
; until it returns #f. Unlike every, if fun returns non-false
; values for all sets of elements, the result of applying fun
; to the last set of elements is returned.
; @arg fun lambda The function to invoke, of as many arguments as there are sequences.
; @arg arg sequence The sequence to iterate over (list, vector, collection or iteratable).
; @arg args sequence Additional parallel sequences to iterate over.
; @ret any #t if the arguments are empty sequences; #f if fun returns #f on any set of elements;
; otherwise, the result of applying fun to the last set of elements. 
; @example
; (for-all positive? '(1 2 3 0 4)) => #f
; (for-all positive? '(1 2 3 4 5)) => #t
; (for-all (lambda (n) (and (positive? n) n)) '(1 2 3 4 5)) => 5
; (for-all < #(1 3 2 4) #(2 5 3 6)) => #t
(define (for-all fun arg . args)
   (define out #t)
   (if (null? args)
      (if (iteratable? arg)
         (let ((itr (iterator arg)))
            (let loop ()
               (if (itr'hasNext)
                  (begin
                     (set! out (fun (itr'next)))
                     (if out (loop) #f)
                  )
                  out
               )
            )
         )
         (let loop ((in arg))
            (if (null? in)
               out
               (begin
                  (set! out (fun (car in)))
                  (if out (loop (cdr in)) #f)
               )
            )
         )
      )
      (if (iteratable? arg)
         (let ((itr (iterator arg)) (itrs (map iterator args)))
            (let loop ()
               (if (itr'hasNext)
                  (begin
                     (set! out (apply fun (itr'next) (map (lambda (a) (a'next)) itrs)))
                     (if out (loop) #f)
                  )
                  out
               )
            )
         )
         (let loop ((in arg) (ins args))
            (if (null? in)
               out
               (begin
                  (set! out (apply fun (car in) (map car ins)))
                  (if out (loop (cdr in) (map cdr ins)) #f)
               )
            )
         )
      )
   )
)

; Visits items of a list, collection, or vector from left to right accumulating a value.
; @arg fun lambda A function with 1 + the number of sequence arguments. The first argument 
; is the current accumulated value. The remaining arguments each correspond to a sequence. 
; The returned value becomes the new accumulated value.  
; @arg value any The initial value
; @arg arg sequence The sequence to visit (list, vector or collection).
; @arg args sequence Additional parallel sequences of the same type to iterate over.
; @ret any The accumulated value
; @see fold-right for-each
; @example 
; (fold-left + 0 '(1 2 3 4 5)) => 15
; (fold-left (lambda (a e) (cons e a)) '() '(1 2 3 4 5)) => '(5 4 3 2 1) 
(define (fold-left fun value arg . args)
   (if (null? args)
      (for-each (lambda (item) (set! value (fun value item))) arg) 
      (apply for-each (lambda (item . items) (set! value (apply fun value item items))) arg args)
   )
   value
)

; Visits items of a list, collection, or vector from right to left accumulating a value.
; @arg fun lambda A function with 1 + the number of sequence arguments. The last argument 
; is the current accumulated value. The remaining arguments each correspond to a sequence. 
; The returned value becomes the new accumulated value.  
; @arg value any The initial value
; @arg arg sequence The sequence to iterate over (list, vector or collection).
; @arg args sequence Additional parallel sequences of the same type to iterate over.
; @ret any The accumulated value
; @see fold-left
; @example 
; (fold-right + 0 '(1 2 3 4 5)) => 15
; (fold-right cons '(q) '(a b c)) => '(a b c q) 
(define (fold-right fun value arg . args)
   (if (null? args)
      (cond
         ((pair? arg)
            (let pair-fold-right ((arg arg))
               (if (null? arg)
                  value
                  (fun (car arg) (pair-fold-right (cdr arg)))
               )
            )
         )
         ((null? arg)
            '()
         )
         (else ;; collection or vector
            (for ((i (+ -1 (if (collection? arg) (arg'size) (vector-length arg))))) (>= i 0)
               (set! i (+ -1 i))
               (set! value (fun (arg i) value))
            )
            value
         )
      )
      ; else
      (cond
         ((pair? arg)
            (let pair-fold-right ((argsList (cons arg args)))
               (if (null? (car argsList))
                  value
                  (apply fun
                     (append
                        (map car argsList)
                        (list (pair-fold-right (map cdr argsList)))
                     )
                  )
               )
            )
         )
         ((null? arg)
            '()
         )
         (else ;; collection or vector
            (for ((i (+ -1 (if (collection? arg) (arg'size) (vector-length arg))))) (>= i 0)
               (set! i (+ -1 i))
               (set! value
                  (apply fun
                     (append (map (lambda (item) (item i)) (cons arg args)) (list value))
                  )
               )
            )
            value
         )
      )
   )
)

; Finds an item in a sequence. When fun returns a true value then iteration 
; stops and the current item is returned.  
; @arg fun lambda The function to invoke for each item. Accepts one argument
; @arg arg sequence The sequence to be searched
; @ret any The first item that fun returned a true value for, or #f if no item found
; @example 
; (find even? '(3 1 4 1 5 9)) => 4
; (find even? '(3 1 5 1 5 9)) => #f
; @see some
(define (find fun arg)
   (define found #f)
   (some
      (lambda (item)
         (if (fun item)
            (begin (set! found item) #t)
            #f
         )
      )
      arg
   )
   found
)


; Applies the predicate fun to each element of the sequence arg and
; returns a sequence of elements of arg for which fun returned #f.
; @arg fun lambda The predicate, invoked on each element of the sequence. It
; should accept one argument and return a single value, without mutating arg.
; @arg arg sequence The sequence to operate on (list, vector or collection).
; @ret sequence New sequence containing the items that evaluate to #f under
; predicate fun.
; @example
; (remp even? '(1 2 3 4 5 6 7)) => (1 3 5 7)
(define (remp fun arg)
   (filter
      (lambda (item) (not (fun item)))
      arg
   )
)

; Returns the first subsequence of arg whose car field evaluates to
; true under the predicate fun, or #f if no such subsequence exists.
; @arg fun lambda The predicate, invoked on each element of the sequence
; until a satisfying argument is found. It should accept one argument
; and return a single value, without mutating arg.
; @arg arg sequence The sequence to operate on (list, vector or collection).
; @ret any The subsequence of the original sequence whose car field
; is the first element in the sequence that evaluates to true under fun,
; or #f if no such subsequence exists.
; @example
; (memp even? '(1 3 2 4 5 6 7)) => (2 4 5 6 7)
; (memp odd? '(1 3 2 4 5 6 7)) => (1 3 2 4 5 6 7)
(define (memp fun arg)
   (if (iteratable? arg)
      (let ((out #f))
         (do
            (
               (i 0 (+ i 1))
               (itr (iterator arg))
            )
            ((not (itr'hasNext)) out)
            (let ((item (itr'next)))
               (if (fun item)
                  (if (collection? arg)
                     (begin
                        (set! out (make-collection (- (vector-length arg) i)))
                        (out'add item)
                        (let loop ()
                           (if (itr'hasNext)
                              (begin
                                 (out'add (itr'next))
                                 (loop)
                              )
                           )
                        )
                     )
                     ;; vector
                     (begin
                        (set! out (make-vector (- (vector-length arg) i)))
                        (vector-set! out 0 item)
                        (do
                           ((k 1 (+ k 1)))
                           ((not (itr'hasNext)))
                           (vector-set! out k (itr'next))
                        )
                     )
                  )
               )
            )
         )
      )
      (let loop ((in arg))
         (cond
            ((null? in) #f)
            ((fun (car in)) in)
            (else (loop (cdr in)))
         )
      )
   )
)

; Returns the first pair in seq whose car field evaluates to true under the
; predicate fun, or #f if no such pair exists.
; @arg fun lambda The predicate, invoked on each pair in seq until a
; satisfying argument is found. It should accept one argument and return
; a single value, without mutating seq.
; @arg seq sequence A sequence of pairs (list, vector or collection).
; @ret any The first pair in seq that satisfies the predicate fun, or #f
; is no such pair exists.
; @example
; (assp even? '((1 a) (2 b) (3 c))) => (2 b)
; (assp odd? '((1 a) (2 b) (3 c))) => (1 a)
(define (assp fun seq)
   (some
      (lambda (item)
         (if (fun (car item))
            item
            #f
         )
      )
      seq
   )
)

; Returns a procedure. When applied, this procedure tries to match its argument arity to
; the given case-lambda clauses in order, and returns the value obtained from evaluating
; the first matching clause. An exception is thrown if no matching clause is found.
; @arg clauses list The case-lambda clauses. May be 0 or more. Each clause must be of
; the form (<args> <exprs>), in which <args> and <exprs> follow the syntax and
; semantics specified for the arguments to lambda.
; @see lambda
; @ret any The value returned by the first matching clause.
; @example
; (define fun
;    (case-lambda
;       (() 'Zero)
;       ((a) `(One ,a))
;       ((a b c . d) (cons* 'Many a b c d))
;       (rest (list 'Sum (apply + rest)))
;    )
; )
; (fun) => Zero
; (fun 1) => (One 1)
; (fun 1 2) => (Sum 3)
; (fun 1 2 3) => (Many 1 2 3)
; (fun 1 2 3 4) => (Many 1 2 3 4)
; (fun (- 5 4) (* 1 2) '(3) 4 '(5 6)) => (Many 1 2 (3) 4 (5 6))
(define-macro (case-lambda . clauses)
   (define args (string->symbol "#args"))
   (define arg-count (string->symbol "#arg-count"))
   (define cond-expr (list '##cond))
   `(lambda ,args
      (##let ((,arg-count (##length ,args)))
         ,(do
            ((cases cond-expr))
            ((null? clauses)
               (unless (and (pair? (car cases)) (= (caar cases) 'else))
                  (set-cdr! cases `((else (##error "err.scripting.caseLambdaNoMatch" ,arg-count))))
               )
               cond-expr)
            (let ((vars (caar clauses)))
               (set-cdr! cases
                  `((
                     ,(let loop ((in vars) (count 0))
                        (cond
                           ((null? in) `(##= ,arg-count ,count))
                           ((and (pair? in) (symbol? (car in)))
                              (loop (cdr in) (+ 1 count)))
                           ((symbol? in)
                              (if (zero? count)
                                 (begin (set-cdr! clauses '()) 'else)
                                 `(##>= ,arg-count ,count)
                              )
                           )
                           (else (error "err.scripting.caseLambdaSyntax" vars))
                        )
                     )
                     (##apply ,(cons* 'lambda vars (cdar clauses)) ,args)
                  ))
               )
               (set! clauses (cdr clauses))
               (set! cases (cdr cases))
            )
         )
      )
   )
)

; Creates a new enumeration type whose universe contains the given symbols,
; with duplicates removed, and generates two macros that are bound to the
; identifiers given by enum-element and enum-constructor.
; The first macro takes a single argument and, if this argument exists in
; the universe of the newly created enumeration type, returns the argument
; as a symbol, otherwise throws an exception.
; The second macro takes 0 or more arguments and, if these arguments exist
; in the universe of the newly created enumeration type, returns an
; enumeration set of this enumeration type which contains these arguments
; as symbols, otherwise throws an exception.
; @arg enum-element symbol The identifier for the membership-check macro.
; @arg symbols list The list of symbols that make up the universe of the
; new enumeration type.
; @arg enum-constructor symbol The identifier for the constructor macro.
; @ret ()
; @see enum-set-constructor enum-set-member?
; @example
; (define-enumeration weekday (mon tue wed thu fri) weekday-constructor)
; (weekday mon) => mon
; (weekday moon) ; exception
; (weekday-constructor) => #<enumset()>
; (weekday-constructor thu fri) => #<enumset(thu, fri)>
; (weekday-constructor thu fry) ; exception
(define-macro (define-enumeration enum-element symbols enum-constructor)
   (define enum (string->symbol (string-append "#enum:" (symbol->string enum-element))))
   `(begin
       (define ,enum (##make-enumeration ',symbols))
       (##define-macro (,enum-constructor . elements)
          `((##enum-set-constructor ,'##,enum) ',elements)
       )
       (##define-macro (,enum-element element)
          `(if (##enum-set-member? ',element ,'##,enum)
              ',element
              (##error "err.scripting.enum.valueNotFound" ',element)
           )
       )
       ()
    )
)

; Returns the symbol representing a supported endian mode.
; @arg sym symbol A symbol indicating the name of the endian mode to use.
; The supported endian modes are big and little.
; @ret symbol The symbol representing the endian mode to use.
; @example
; (endianness big) => big
; (endianness little) => little
(define-enumeration endianness (big little) endianness)

; Returns the symbol representing the native endian mode.
; @ret symbol The symbol representing the native endian mode.
; @example
; (native-endianness) => big
(define (native-endianness) 'big)

; Reverses a list destructively, by modifying all its pairs.
; @arg lst list The list to reverse.
; @ret list The reversed list.
; @see reverse reverse2!
; @example
; (reverse! (list 1 2 3 4)) => (4 3 2 1)
(define (reverse! lst)
   (let ((prev '()) (next '()))
      (let loop ()
         (if (null? lst)
            prev
            (begin
               (set! next (cdr lst))
               (set-cdr! lst prev)
               (set! prev lst)
               (set! lst next)
               (loop)
            )
         )
      )
   )
)

; Reverses a list destructively, by modifying all its pairs,
; and appends to it another list, which is not modified.
; @arg lst list The list to reverse.
; @arg prev list The list to append (unchanged).
; @ret list The resulting list.
; @see reverse reverse!
; @example
; (reverse2! (list 4 3 2 1) '(5 6 7)) => (1 2 3 4 5 6 7)
(define (reverse2! lst prev)
   (let ((next '()))
      (let loop ()
         (if (null? lst)
            prev
            (begin
               (set! next (cdr lst))
               (set-cdr! lst prev)
               (set! prev lst)
               (set! lst next)
               (loop)
            )
         )
      )
   )
)

; Copies recursively a list and its sublist.
; @arg arg list The list to copy.
; @ret list The copy of the list.
; @see append
; @example
; (copy-tree '(1 (2 (3 4)) (5 6))) => (1 (2 (3 4)) (5 6)))
(define (copy-tree arg)
   (let loop ((arg arg))
      (if (pair? arg)
         (cons (loop (car arg)) (loop (cdr arg)))
         arg
      )
   )
)

; The following sorting code was taken from Aubrey Jaffer's SLIB sort.scm
; http://swissnet.ai.mit.edu/~jaffer/SLIB.html
; The code has been reformatted and modified to remove SLIB dependencies.
;========================================================================
;;; "sort.scm" Defines: sorted?, merge, merge!, sort, sort!
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;;
;;; This code is in the public domain.

;;; Updated: 11 June 1991
;;; Modified for scheme library: Aubrey Jaffer 19 Sept. 1991
;;; Updated: 19 June 1995

; Determines if a sequence is sorted according to a comparison function.
; @arg seq sequence The sequence to test (list, vector or collection).
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @ret boolean #t if the sequence is sorted according to the comparison function,
; #f otherwise.
; @example
; (sorted? #(1 2 3) <) => #t
; (sorted? '(1 3 2) <) => #f
(define (sorted? seq less?)
   (cond
      ((null? seq)
         #t
      )
      ((vector? seq)
         (let ((n (vector-length seq)))
            (if (<= n 1)
               #t
               (do ((i 1 (+ i 1)))
                  ((or (= i n) (less? (vector-ref seq i) (vector-ref seq (- i 1))))
                     (= i n)
                  )
               )
            )
         )
      )
      (else
         (let loop ((last (car seq)) (next (cdr seq)))
            (or (null? next)
               (and (not (less? (car next) last))
                  (loop (car next) (cdr next))
               )
            )
         )
      )
   )
)

; Merges two lists sorted according to a comparison function into
; a new list sorted according to the same comparison function.
; @arg a list The first list to merge (sorted? a less?). Unchanged by merge.
; @arg b list The second list to merge (sorted? b less?). Unchanged by merge.
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @ret list The new sorted list, (sorted? (merge a b less?) less?).
; @example
; (merge '(1 3 6) '(2 4 5) <) => (1 2 3 4 5 6)
(define (merge a b less?)
   (cond
      ((null? a)
         b
      )
      ((null? b)
         a
      )
      (else
         (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
            ;; The loop handles the merging of non-empty lists.  It has
            ;; been written this way to save testing and car/cdring.
            (if (less? y x)
               (if (null? b)
                  (cons* y x a)
                  (cons y (loop x a (car b) (cdr b)))
               )
               ;; x <= y
               (if (null? a)
                  (cons* x y b)
                  (cons x (loop (car a) (cdr a) y b))
               )
            )
         )
      )
   )
)

; Merges destructively two lists sorted according to a comparison
; function into a new list sorted according to the same comparison function,
; by modifying the cdr of each list element.
; @arg a list The first list to merge (sorted? a less?). Modified by merge!.
; @arg b list The second list to merge (sorted? b less?). Modified by merge!.
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @ret list The new sorted list, (sorted? (merge! a b less?) less?).
; @example
; (merge! (list 1 3 6) (list 2 4 5) <) => (1 2 3 4 5 6)
(define (merge! a b less?)
   (define (loop r a b)
      (if (less? (car b) (car a))
         (begin
            (set-cdr! r b)
            (if (null? (cdr b))
               (set-cdr! b a)
               (loop b a (cdr b))
            )
         )
         ;; (car a) <= (car b)
         (begin
            (set-cdr! r a)
            (if (null? (cdr a))
               (set-cdr! a b)
               (loop a (cdr a) b)
            )
         )
      )
   )
   (cond
      ((null? a)
         b
      )
      ((null? b)
         a
      )
      ((less? (car b) (car a))
         (if (null? (cdr b))
            (set-cdr! b a)
            (loop b a (cdr b))
         )
         b
      )
      (else ; (car a) <= (car b)
         (if (null? (cdr a))
            (set-cdr! a b)
            (loop a (cdr a) b)
         )
         a
      )
   )
)

; Sorts destructively a sequence according to a comparison function.
; @arg seq sequence The sequence to sort (list, vector or collection). Modified by sort!.
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @ret sequence The sorted sequence (if seq is a list, it might not be
; the same object as the one passed it, as the pairs will be shuffled).
; @example
; (sort! (list 3 2 1 4 6 5) <) => (1 2 3 4 5 6)
(define (sort! seq less?)
   (define (step n)
      (cond
         ((> n 2)
            (let*
               (
                  (j (quotient n 2))
                  (a (step j))
                  (k (- n j))
                  (b (step k))
               )
               (merge! a b less?)
            )
         )
         ((= n 2)
            (let
               (
                  (x (car seq))
                  (y (cadr seq))
                  (p seq)
               )
               (set! seq (cddr seq))
               (if (less? y x)
                  (begin
                     (set-car! p y)
                     (set-car! (cdr p) x)
                  )
               )
               (set-cdr! (cdr p) '())
               p
            )
         )
         ((= n 1)
            (let ((p seq))
               (set! seq (cdr seq))
               (set-cdr! p '())
               p
            )
         )
         (else
            '()
         )
      )
   )
   (if (pair? seq)
      (step (length seq))
      (vector-sort! seq less?)
   )
)

; Sorts a sequence according a comparison function.
; @arg seq sequence The sequence to sort (list, vector or collection). Unchanged by sort.
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @ret sequence A newly allocated sorted sequence.
; @example
; (sort '(3 2 1 4 6 5) <) => (1 2 3 4 5 6)
(define (sort seq less?)
   (cond 
      ((pair? seq)
         (sort! (append seq '()) less?)
      )
      ((null? seq)
         '()
      )
      (else
         (vector-sort! (vector-append seq) less?)
      )
   )
)

;===================================
; end of sort.scm

; Sorts a list according to a comparison function.
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @arg list pair The list to sort. Unchanged by list-sort.
; @ret list A newly allocated sorted list.
; @example
; (list-sort < '(9 7 1 4 6 2 3 5 2 8)) => (1 2 2 3 4 5 6 7 8 9)
(define (list-sort less? list)
   (sort list less?)
)

; Sorts a vector according to a comparison function.
; @arg less? The comparison function of two arguments, (lambda (left right) ...),
; returning #f if left should not be sorted before right, or non-#f otherwise.
; @arg vector vector The vector to sort. Unchanged by vector-sort.
; @ret vector A newly allocated sorted vector.
; @example
; (vector-sort < '#(9 7 1 4 6 2 3 5 2 8)) => #(1 2 2 3 4 5 6 7 8 9)
(define (vector-sort less? vector)
   (sort vector less?)
)

; Constructs a comparison function based on a key extraction function.
; @arg less? lambda The value comparison function.
; @arg key lambda The key extraction function of one argument.
; @ret lambda The resulting comparison function, which compares with less?
; the results of applying key on the compared values.
; @example
; (sort '((2 . a) (3 . b) (1 . c)) (sort-key < car)) => ((1 . c) (2 . a) (3 . b))
; (sort '(2 1 -1 -2) (sort-key < positive?)) => (-1 -2 2 1)
(define (sort-key less? key)
   (lambda (a b)
      (less? (key a) (key b))
   )
)

; Constructs a comparison function based on object attribute values.
; @arg less? lambda The value comparison function.
; @arg attribute symbol The attribute symbol.
; @arg attributes symbol Additional attribute symbols.
; @ret lambda The resulting comparison function, which compares with less?
; the attribute values taken from the compared objects.
; @example
; (sort! persons (sort-attribute < 'lastName 'firstName))
; => collection of persons sorted by last name, then first name
(define (sort-attribute less? attribute . attributes)
   (if (null? attributes)
      (lambda (a b)
         (less? (a attribute) (b attribute))
      )
      (begin
         (set! attributes (cons attribute attributes))
         (lambda (a b)
            (let loop ((attributes attributes))
               (or
                  (null? attributes)
                  (less? (a attribute) (b attribute))
                  (if (less? (b attribute) (a attribute))
                     #f
                     (loop (cdr attributes))
                  )
               )
            )
         )
      )
   )
)

; Sorts a sequence topologically.
; @arg items sequence The sequence to sort.
; @arg get-deps lambda Function of one argument returning a sequence of dependent items.
; @return (list list) The sorted items and the items with circular references.
; @example
; (define graph '((a b c) (b c) (c) (d a) (e f) (f e)))
; (topo-sort (map car graph) (lambda (item) (cdr (assq item graph))))
; => (d a b c)
(define (topo-sort items get-deps)
   (define htab (make-eq-hashtable (length items)))
   (define roots '())
   ; populate table with dependency lists
   (for-each
      (lambda (item)
         (hashtable-set! htab item (cons 0 (get-deps item)))
      )
      items
   )
   ; compute predecessor counts
   (for-each
      (lambda (item)
         (for-each
            (lambda (dep)
               (define info (hashtable-ref htab dep #f))
               (if info (set-car! info (+ (car info) 1)))
            )
            (cdr (hashtable-ref htab item))
         )
      )
      items
   )
   ; collect the roots (i.e. zero predecessor count)
   (for-each
      (lambda (item)
         (if (zero? (car (hashtable-ref htab item)))
            (set! roots (cons item roots))
         )
      )
      items
   )
   ; decrease the predecessor count of dependencies and collect roots
   (let loop ((tail roots) (roots (set! roots (reverse! roots))))
      (define deps '())
      (unless (null? tail)
         (for-each
            (lambda (root)
               (for-each
                  (lambda (dep)
                     (define info (hashtable-ref htab dep #f))
                     (if (and info (zero? (set-car! info (- (car info) 1))))
                        (set! deps (cons dep deps))
                     )
                  )
                  (cdr (hashtable-ref htab root))
               )
            )
            roots
         )
         (set-cdr! tail (reverse! deps))
         (loop deps (cdr tail))
      )
   )
   (values
      roots
      (filter
         (lambda (item) (positive? (car (hashtable-ref htab item))))
         items
      )
   )
)

; Special form casting a value to a given type.
; @arg type symbol The target type name: string, binary, integer,
; long, decimal, float, double, timestamp, boolean or a class
; derived from Object.
; A timestamp cast to long gives the number of milliseconds since
; 1-Jan-1970 00:00:00 UTC.
; @arg x any The expression to evaluate and cast.
; @ret any The cast value.
; @example
; (cast binary "feed12") => #<FEED12>
; (cast boolean "1") => #t
; (cast string #f) => "0"
(define-macro (cast type x)
   (if (symbol? type)
      `(##sys:cast
         (global 
            ,(if (member type '(string binary integer long decimal float double timestamp boolean))
               (string->symbol (string-append "sys:" (symbol->string type)))
               type
             )
         )
         ,x
       )
      (error "err.scripting.castSymbol")
   )
)

; Special form accessing a value through associations relative to "this".
; @arg assocs symbol The association path. ":" separates the arguments for the last association.  
; @ret any The value of the association at the end of the path.
; @example
; (expand-macro '(@ addrs fullName)) => ((this'addrs)'fullName)
; (expand-macro '(@ addrs fullName : "New Name") => ((this'addrs)'fullName "New Name")
(define-macro (@ . assocs)
   (define expr 'this)
   (let loop ((assocs assocs))
      (cond
         ((null? assocs)
            expr
         )
         ((eq? (car assocs) ':)
            (if (symbol? expr)
               (cons expr (cdr assocs))
               (append expr (cdr assocs))
            )
         )
         ((symbol? (car assocs))
            (set! expr (list expr (list 'quote (car assocs))))
            (loop (cdr assocs))
         )
         ((pair? (car assocs))
            (set! expr
               `(
                  (lambda (this)
                     (if (##iteratable? this)
                        (##filter (lambda (this) (##ifnull ,(car assocs) #f)) this)
                        (if (##and (##not (##null? this)) (##ifnull ,(car assocs) #f)) this)
                     )
                  )
                  ,expr
               )
            )
            (loop (cdr assocs))
         )
         (else
            (error "err.scripting.assocSymbol")
         )
      )
   )
)

; Removes duplicate items from a sequence (list, vector or collection).
; @arg items sequence The items to filter.
; @ret sequence The sequence without the duplicate items (can be the original unmodified sequence).
; @example
; (unique #(1 () 3 () 5 3)) => #(1 () 3 5)
(define (unique items)
   (if (instance-collection? items)
      items
      (let ((hset (make-hashset)))
         (filter
            (lambda (item) (hashset-add! hset item))
            items
         )
      )
   )
)

; Counts the number of non-null items in a sequence (list, vector or collection).
; @arg items sequence The items to count.
; @ret number The count of non-null items in the sequence.
; @example
; (count #(1 () 3 () 5)) => 3
(define (count items)
   (if (instance-collection? items)
      (vector-length items)
      (let ((cnt 0))
         (for-each
            (lambda (item) (if (not (null? item)) (set! cnt (+ cnt 1))))
            items
         )
         cnt
      )
   )
)

; Computes the sum of the non-null values in a sequence (list, vector or collection).
; @arg values sequence The values to add.
; @ret number The sum of the values.
; @see count average
; @example
; (sum #(1 2 () 3)) => 6
; (sum '()) => ()
(define (sum values)
   (define n ())
   (for-each
      (lambda (value)
         (if (not (null? value))
            (set! n (if (null? n) value (+ n value)))
         )
      )
      values
   )
   n
)

(define sys:sum sum)

; Computes the arithmetic mean (sum of the values divided by their count)
; of the non-null values in a sequence (list, vector or collection).
; @arg values sequence The values to average.
; @ret number The average of the values.
; @see count sum
; @example
; (average #(1 2 () 3)) => 2
; (average '()) => ()
(define (average values)
   (define n ())
   (define cnt 0)
   (for-each
      (lambda (value)
         (unless (null? value)
            (set! n (if (null? n) value (+ n value)))
            (set! cnt (+ cnt 1))
         )
      )
      values
   )
   (/ n cnt)
)

; Computes the minimum of the non-null values in a sequence (list, vector or collection).
; @arg values sequence The values to consider.
; @ret any The minimum value.
; @see maximum min max
; @example
; (minimum #(1 2 () 3)) => 1
; (minimum '()) => ()
(define (minimum values)
   (define m ())
   (for-each
      (lambda (value)
         (if (not (null? value)) (set! m (if (null? m) value (min m value))))
      )
      values
   )
   m
)

; Computes the maximum of the non-null values in a sequence (list, vector or collection).
; @arg values sequence The values to consider.
; @ret any The maximum value.
; @see minimum min max
; @example
; (maximum #(1 2 () 3)) => 3
; (maximum '()) => ()
(define (maximum values)
   (define m ())
   (for-each
      (lambda (value)
         (if (not (null? value)) (set! m (if (null? m) value (max m value))))
      )
      values
   )
   m
)

; Returns the length of a vector or a collection.
; @arg seq sequence Vector or collection, which length to determine.
; @ret integer The length of the vector or the collection.
; @see vector-length
; @example
; (collection-length (collection 1 2 3)) => 3
; (collection-length #(1 2 3)) => 3
(define (collection-length seq) (vector-length seq))

; Extracts the first item from a collection.
; If the argument is not a collection, it is returned by itself.
; @arg value any The value to convert.
; @ret any The first item, or the argument itself if it is not a collection.
; @example
; (collection->value (collection 1 2 3)) => 1
; (collection->value 1) => 1
; (collection->value '()) => ()
; (collection->value (collection)) => ()
(define (collection->value value)
   (cond
      ((not (vector? value)) value)
      ((zero? (vector-length value)) '())
      (else (vector-ref value 0))
   )
)

; Converts its argument to a collection, if it is not already a collection.
; @arg value any The value to convert.
; @ret collection The collection.
; @example
; (value->collection 1) => #<[1]>
; (value->collection (collection 1 2 3)) => #<[1, 2, 3]>
; (value->collection '()) => #<[]>
(define (value->collection value)
   (cond
      ((collection? value) value)
      ((null? value) (collection))
      ((and (not (null? (get-value 'Object))) (instance? value Object)) (instance-collection value))
      (else (collection value))
   )
)

; Wraps a MatchOperator in a boolean >= comparison.
; @arg attribute string The attribute value to use for matching.
; @arg expression any The expression to match.
; @arg score double The minimum score that the value must accumulate (optional, default 0.5d).
; @ret boolean A boolean indicating if the requested score was reached.
; @example
; (match? (@ addresses city) \"Toronto\" 0.0)
(define-macro (match? attribute expression . score)
   `(##>= (##match ,attribute ,expression)
      ,(if (null? score)
         0.5d
         (if (and (pair? score) (null? (cdr score)))
            (car score)
            (error "err.scripting.invalidMatchpArgCount" (+ 2 (length score)))
         )
      )
   )
)

; Special form creating a pair for use as a named argument.
; @arg name symbol Unquoted symbol to put in the head of the pair.
; @arg value any Expression to evaluate and put in the tail of the pair.
; @ret pair The created pair.
; @example
; (: a (+ 1 2)) => (a . 3)
(define-macro (: name value)
   `(##cons ',name ,value)
)

; Special form for logging.
; @arg level symbol Log level, one of 'fatal, 'error, 'warn, 'info, 'debug or 'dump.
; @arg args any Expressions to log. They are evaluated only if the
; log level is enabled and are logged separated by " ".
; @ret boolean If no args are supplied, a boolean indicating
; the enablement of the corresponding log level, otherwise ().
; @example
; (logger'debug) => #t
; (logger'debug "x =" (+ 1 2)) => "x = 3"
(define-macro (logger level . args)
   (define logged (get-value 'sys:current-logger))
   (define log-function 
      (if (and (= level ''localized) (not (null? args)))
         (begin
            (set! level (car args))
            (set! args (cdr args))
            'sys:log-localized
         )
         'sys:log
      )
   )
   (set! level
      (cdr
         (or
            (assoc level '(('fatal . 0) ('error . 1) ('warn . 2) ('info . 3) ('debug . 4) ('dump . 5)))
            (error "err.scripting.logLevel")
         )
      )
   )
   `(if (##sys:log ,logged ,level)
      (##,log-function ,logged ,level ,@args)
    )
)

; Binds an application defined LOGGER instance to the current logger and executes
; body.
; @arg logger-instance instance Instance of a class with the LOGGER aspect.
; @arg body any Expressions to evaluate with logger-instance bound to the current logger.
; @return any The value of the last expression of body.
(define-macro (with-logger logger-instance . body)
   (define logger-instance-var (string->symbol "#logger-instance"))
   (define result-var (string->symbol "#r"))
   (define logger-saved (get-value 'sys:current-logger))
   `(##let ((,logger-instance-var (,logger-instance'logger)))
      (,(macro () ((interaction-environment)'defineVariable 'sys:current-logger logger-instance-var)))
      (##let ((,result-var ()))
         (set! ,result-var (begin ,@body))
         (,(if (null? logger-saved)
            (macro () ((interaction-environment)'removeVariable 'sys:current-logger))
            (macro () ((interaction-environment)'defineVariable 'sys:current-logger logger-saved))
           )
         )
         ,result-var
      )
   )
)

; Asserts an invariant.
; NOTE: For unit test assertions, use assert-true instead.
; @arg invariant boolean The invariant condition.
; If it evaluates to #f, a scripting exception is thrown.
; @arg args any Optional error code and arguments (defaults to "err.assert").
; @ret ()
; @example
; (assert (<= (col'size) 1))
(define-macro (assert invariant . args)
   (if (null? args)
      `(##sys:assert ,invariant ,@args)
      `(if (##not ,invariant) (##error ,@args))
   )
)

; Rounds the number n to the given number of decimal points, rounding half up
; if the number is exactly in the middle.
; @arg n number The number to round. May be ().
; @arg dec number The number of decimal places.
; @ret number The rounded argument.
; @see ceiling floor round truncate
; @example
; (round-at 3.5 0) => 4.0
; (round-at 4.104 2) => 4.1
; (round-at 4.105 2) => 4.11
; (round-at -4.104 2) => -4.1
; (round-at -4.105 2) => -4.11
(define (round-at n dec)
   (define scale (expt 10n dec))
   (/ (truncate ((if (negative? n) - +) (* n scale) 0.5n)) scale)
)

; Returns the principal name of the logged in account.
; This is the same as the user login name, but the character case might differ.
; @ret string The principal name.
; @example
; (principal-name) => "jtest@mydomain.com"
(define (principal-name)
   (((invocation-context)'principal)'name)
)

; Special form creating syntax objects from an S-expression, allowing parts of
; the expression to be evaluated, analogous to quasiquote.
; Every occurrence of #,expr or (unsyntax expr) is replaced by the value of expr.
; Every occurrence of (unsyntax expr1 ... exprN) is replaced by the exprX items
; put in-line in consecutive positions.
; Every occurrence of #,@expr or (unsyntax-splicing expr) is replaced by the items
; from the list expr put in-line in consecutive positions.
; Every occurrence of (unsyntax-splicing expr1 ... exprN) is replaced by the items
; from the lists exprX put in-line in consecutive positions.
; Each exprX in unsyntax-splicing may also be a syntax object whose value is a list,
; in which case the values put in-line in consecutive positions are the syntax objects
; corresponding to the list items.
; Outside of a transformer expansion context, quasisyntax behaves as quasiquote.
; #`arg is the syntactic equivalent of (quasisyntax arg).
; @arg arg any The S-expression, in which to substitute the values.
; @ret any A new S-expression with substituted values. If might reuse parts of arg.
; @see syntax with-syntax
; @example
; (define a 1)
; (define b 2)
; (define c 3)
; (define d 4)
; (quasisyntax (a d (unsyntax b) b (unsyntax c d a b) c)) => (#<#'a> #<#'d> 2 #<#'b> 3 4 1 2 #<#'c>)
; 
; (quasisyntax (a d (unsyntax-splicing (list b)) b (unsyntax-splicing (list c) (list d a) (list b)) c)) =>
;    (#<#'a> #<#'d> 2 #<#'b> 3 4 1 2 #<#'c>)
; 
; (quasisyntax (a d (unsyntax-splicing #'(b)) b (unsyntax-splicing (list c) #'(d a) (list b)) c)) =>
;    (#<#'a> #<#'d> #<#'b> #<#'b> 3 #<#'d> #<#'a> 2 #<#'c>)
; 
; (define-syntax transformer
;    (lambda (x)
;       (syntax-case x ()
;          ((_ id) (identifier? #'id)
;             #`(list
;                #,(free-identifier=? #'id #'and)
;                #,@(list
;                   (free-identifier=? #'id #'or)
;                )
;             )
;          )
;       )
;    )
; )
; (transformer a) => (#f #f)
; (transformer and) => (#t #f)
; (transformer or) => (#f #t)
(define quasisyntax (sys:quasi-macro syntax quasisyntax unsyntax unsyntax-splicing))

;========================================================================
; syntax-case and syntax-rules transformer macros

(define-syntax let-syntax
   (syntax-rules ()
      ; Special form defining a scope with local transformer functions, analogous to let.
      ; @arg vars pair Variable list of the form ((name1 <expr1>) ... (nameN <exprN>)).
      ; The expressions must evaluate to transformer functions (any one-argument functions),
      ; and can only access variables from the enclosing scopes.
      ; @arg exprs any Expressions to evaluate within the scope (0 or more).
      ; @ret any The value of the last expression in the scope.
      ; @see define-syntax letrec-syntax
      ; @example
      ; (let ((f (lambda (x) (* x 10))))
      ;    (let-syntax
      ;       (
      ;          (f (syntax-rules () ((f x) x)))
      ;          (g (syntax-rules () ((g x) (f x)))) ; f refers to the lambda
      ;       )
      ;       (list (f 1) (g 1))
      ;    )
      ; ) => (1 10)
      ((_ ((kwrd expr) ...) body ...)
         ((lambda ()
            (sys:define-let-syntax kwrd expr) ...
            body ...
         ))
      )
   )
)

(define-syntax letrec-syntax
   (syntax-rules ()
      ; Special form defining a scope with local transformer functions, analogous to letrec.
      ; @arg vars pair Variable list of the form ((name1 <expr1>) ... (nameN <exprN>)).
      ; The expressions must evaluate to transformer functions (any one-argument functions),
      ; and can only access variables from the enclosing scopes.
      ; @arg exprs any Expressions to evaluate within the scope (0 or more).
      ; @ret any The value of the last expression in the scope.
      ; @see define-syntax let-syntax
      ; @example
      ; (let ((f (lambda (x) (* x 10))))
      ;    (letrec-syntax
      ;       (
      ;          (f (syntax-rules () ((f x) x)))
      ;          (g (syntax-rules () ((g x) (f x)))) ; f refers to the syntax-rules transformer
      ;       )
      ;       (list (f 1) (g 1))
      ;    )
      ; ) => (1 1)
      ((_ ((kwrd expr) ...) body ...)
         ((lambda ()
            (define-syntax kwrd expr) ...
            body ...
         ))
      )
   )
)

(define-syntax with-syntax
   (syntax-rules ()
      ; Special form used to bind pattern variables, analogous to let being used to bind
      ; variables. This allows a transformer to construct its output in separate pieces,
      ; then put the pieces together. This is particularly useful when some portions of the
      ; transformer output are repeated in the templates of different transformer clauses.
      ; @arg vars pair Pattern variable list of the form ((<pattern1> <expr1>) ... (<patternN> <exprN>)),
      ; in which patternX is identical to syntax-case patterns, and exprX is any Scheme
      ; expression, which is evaluated and bound to the pattern variables in patternX.
      ; @arg expr any The first expression to evaluate.
      ; @arg exprs any The remaining expressions to evaluate.
      ; @ret any The value returned by the last evaluated expression.
      ; @see syntax-rules syntax-case quasisyntax
      ; @example
      ; (with-syntax
      ;    (
      ;       ((a b (c d)) '(1 2 (abc #(3 4))))
      ;       (((e f) ...) '((5 6) (hello world)))
      ;    )
      ;    (list #'d #'c #'b #'a #'(e ...) #'#(f ...))
      ; ) => (#(3 4) abc 2 1 (5 hello) #(6 world))
      ; 
      ; (define-syntax or
      ;    (lambda (x)
      ;       (syntax-case x ()
      ;          ((_) #'#f)
      ;          ((_ e) #'e)
      ;          ((_ e1 e2 e3 ...)
      ;             (with-syntax ((rest #'(or e2 e3 ...)))
      ;                #'(let ((t e1)) (if t t rest))
      ;             )
      ;          )
      ;       )
      ;    )
      ; )
      ; (or) => #f
      ; (or 123) => 123
      ; (or #f #f #f 123) => 123
      ; (or #f #f #f #f) => #f
      ((_ ((pattern val) ...) body1 body2 ...)
         (syntax-case (list val ...) ()
            ((pattern ...) (let () body1 body2 ...))
         )
      )
   )
)

(define-syntax identifier-syntax
   (lambda (x)
      (syntax-case x (set!)
         ; Creates an identifier-syntax transformer function with the given template.
         ; When the keyword bound to this transformer is referenced, the keyword
         ; reference is replaced by the expansion of the template.
         ; @arg template any The template to expand.
         ; @ret lambda A transformer function.
         ; @see syntax-rules syntax-case with-syntax make-variable-transformer
         ; @example
         ; (define p '(0 2))
         ; (define-syntax p.car (identifier-syntax (car p)))
         ; p.car => 0
         ; (set! p.car 1) ; error
         ; (set! p '(1 2))
         ; p.car => 1
         ((_ template)
            #'(lambda (x)
               (syntax-case x ()
                  (id (identifier? #'id) #'template)
                  ((_ x (... ...)) #'(template x (... ...)))
               )
            )
         )

         ; Creates an identifier-syntax transformer function with the given templates.
         ; When the keyword appears as the argument to a set! expression, the set! call
         ; is replaced by the expansion of the template of the set-clause.
         ; When the keyword bound to this transformer is referenced, the keyword
         ; reference is replaced by the expansion of the template of the id-clause.
         ; @arg id-clause The id clause, must have the form (<id> <template>), where id
         ; and template are any Scheme expressions.
         ; @arg set-clause The set! clause, must have the form ((set! <id> <pattern>) <template>),
         ; where id, pattern and template are any Scheme expressions.
         ; @ret lambda A transformer function.
         ; @see syntax-rules syntax-case with-syntax make-variable-transformer
         ; @example
         ; (define p (list 0 2))
         ; (define-syntax p.car
         ;    (identifier-syntax
         ;       (_ (car p))
         ;       ((set! _ e) (set-car! p e))
         ;    )
         ; )
         ; p.car => 0
         ; (set! p.car 1)
         ; p.car => 1
         ; p => (1 2)
         ((_ (id expr1) ((set! var val) expr2))
            (and (identifier? #'id) (identifier? #'var))
            #'(make-variable-transformer
               (lambda (x)
                  (syntax-case x (set!)
                     ((set! var val) #'expr2)
                     ((id x (... ...)) #'(expr1 x (... ...)))
                     (id (identifier? #'id) #'expr1)
                  )
               )
            )
         )
      )
   )
)
