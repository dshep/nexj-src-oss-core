; Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
; NexJ core Scheme extensions
(declare scope server)

; Returns the user object corresponding to the
; currently logged in account.
; @ret User The user object, or () if running under
; the anonymous account.
; @example
; (user) => #<Instance<User, ...>
(define (user)
   ((invocation-context)'user)
)

(define-macro (sys:generate-event event args vars privilege access new-privileged? tx-mode . actions)
   (define result (string->symbol "#r"))
   (define oldtx (string->symbol "#t"))
   ; nexj.core.meta.Event TX_* modes:  TX_DEFAULT  TX_SUPPORTED  TX_REQUIRED     TX_NEW         TX_NONE         TX_MANDATORY    TX_UNSUPPORTED
   (define tx-start (vector-ref #(     ()          ()            sys:tx-require  sys:tx-begin   sys:tx-suspend  sys:tx-mandate  sys:tx-mandate-none) tx-mode))
   (define tx-end (vector-ref   #(     ()          ()            sys:tx-commit   sys:tx-commit  sys:tx-commit   ()              ()                 ) tx-mode))

   (define (find-else expr)
      (if (eq? (car expr) 'if)
         (if (null? (cdddr expr))
            (cddr expr)
            (find-else (cadddr expr))
         )
      )
   )

   (define code
      (let loop ((action (car actions)) (actions (cdr actions)) (befores '()) (afters '()))
         (define body
            (if (eq? (caddr action) #t)
               ; unconditional action
               `((lambda () ,@(cdddr action)))
               ; conditional action
               `(if ,(caddr action)
                  ((lambda () ,@(cdddr action)))
                  ,@(if (eq? (car action) 'around) '((call-next)) '())
                )
            )
         )
         (case (car action)
            ((before)
               (if (cadr action)
                  ; grouped action (only the first with matching condition in the group is executed)
                  (let ((e (find-else (car befores))))
                     (unless (null? e)
                        (set-cdr! e (list body))
                        (loop (car actions) (cdr actions) befores afters)
                     )
                  )
                  ; ungrouped action
                  (loop (car actions) (cdr actions) (cons body befores) afters)
               )
            )
            ((around main)
               (if (eq? (car action) 'around)
                  (set! body
                     `(##let ((call-next (lambda () ,@(loop (car actions) (cdr actions) '() '()))))
                        ,body
                      )
                  )
               )
               (if (null? afters)
                  (if (null? tx-end)
                     (reverse! (cons body befores))
                     (reverse2! (cons `(##set! ,result ,body) befores) `((##,tx-end ,oldtx) ,result))
                  )
                  (reverse2! (cons `(##set! ,result ,body) befores) afters)
               )
            )
            ((after)
               (if (cadr action)
                  ; grouped action (only the first with matching condition in the group is executed)
                  (let ((e (find-else (car afters))))
                     (unless (null? e)
                        (set-cdr! e (list body))
                        (loop (car actions) (cdr actions) befores afters)
                     )
                  )
                  ; ungrouped action
                  (loop (car actions) (cdr actions) befores
                     (cons body
                        (if (null? afters)
                           (if (null? tx-end)
                              (list result)
                              `((##,tx-end ,oldtx) ,result)
                           )
                           afters
                        )
                     )
                  )
               )
            )
         )
      )
   )

   (cond
      ((or (assq 'after actions) (not (null? tx-start)))
         (set! code
            `(
               (##let
                  (
                     (,result '())
                     ,@(if (null? tx-start)
                        '()
                        `((,oldtx (##,tx-start)))
                       )
                     ,@vars
                  )
                  ,@(if (null? tx-end)
                     code
                     `( 
                        (##try
                           (begin ,@code)
                           (lambda (e)
                              (##sys:tx-rollback ,oldtx)
                              (##throw e)
                           )
                        )
                     )
                  )
               )
            )
         )
      )
      ((not (null? vars))
         (set! code `((##let (,@vars) ,@code)))
      )
   )

   (if (eq? (event'audited) #t)
      (set! code (cons `(##sys:audit (lambda () '()) ,event) code))
   )

   (unless (null? access)
      (set! code
         (cons
            (let
               ((access-check
                  (if (negative? access)
                     `(##sys:check-access (this':class) ,(- -1 access))
                     `(##sys:check-access this ,access)
                  )))
               (if new-privileged?
                  (set! access-check `(if (##not (this'isNew)) ,access-check))
               )
               access-check
            ) 
            code
         )
      )
   )

   (unless (null? privilege)
      (set! code
         (cons
            `(##sys:check-privilege ',privilege)
            code
         )
      )
   )

   `(lambda ,args ,@code)
)

(define-macro (sys:generate-flow-function steps)
   `(
      (##cond
         ,@(map
            (lambda (step)
               (define c `(',(car step)'isActive :state))
               (if (not (eq? (cadr step) #t))
                  (set! c `(##and ,c ,(cadr step)))
               )
               `(,c
                  ,(let
                     (
                        (runStep `((lambda () ,@(cddr step) (',(car step)'step :state))))
                     )
                     (if (null? (SysWorkflow'findEvent "handleException" 3))
                        runStep
                        ; else:
                        `(##try
                           ,runStep
                           (lambda (e)
                              (:flow'handleException e ,(car step) :state)
                           )
                        )
                     )
                  )
               )
            )
            (reverse! steps)
         )
      )
      (if (:state'dirty)
         (:flow'run)
      )
   )
)

(define-macro (sys:begin-tx tx-begin tx-commit body)
   (define result (string->symbol "#r"))
   (define oldtx (string->symbol "#t"))
   `(##let
      (
         (,oldtx ,tx-begin)
      )
      (##try
         (##let ((,result (begin ,@body)))
            (##,tx-commit ,oldtx)
            ,result
         )
         (lambda (e)
            (##sys:tx-rollback ,oldtx)
            (##throw e)
         )
      )
   )
)

; Special form evaluating specified expressions in the context of a new transaction.
; 
; Starts a new transaction (suspending the current transaction, if any) and 
; evaluates the expression in this context. If an exception is thrown this new
; transaction is rolled back and the exception is rethrown. Otherwise this new
; transaction is committed after body is executed. If a transaction was in 
; progress before this block was entered, it will be resumed when this block 
; completes. The container default transaction timeout is used.
; 
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-transaction
;    (fun1)
;    (+ 1 2)
; ) => 3
; @see commit
(define-macro (begin-transaction . body)
   (expand-macro-1 `(sys:begin-tx (##sys:tx-begin) sys:tx-commit ,body))
)

; Special form evaluating specified expressions in the context of a new transaction
; with a specified timeout.
; 
; @arg body any The expressions to evaluate.
; @arg timeout number Transaction timeout in seconds. () and 0 mean the container default timeout.
; @ret any The value of the last expression.
; @example
; (begin-transaction-with-timeout 30
;    (fun1)
;    (+ 1 2)
; ) => 3
; @see begin-transaction
(define-macro (begin-transaction-with-timeout timeout . body)
   (expand-macro-1 `(sys:begin-tx (##sys:tx-begin ,timeout) sys:tx-commit ,body))
)

; Special form evaluating specified expressions in a non-transactional context.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (suspend-transaction
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (suspend-transaction . body)
   (expand-macro-1 `(sys:begin-tx (##sys:tx-suspend) sys:tx-commit ,body))
)

; Special form evaluating specified expressions with security off.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-privileged
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-privileged . body)
   (define secure (string->symbol "#secure"))
   `(##let ((,secure ((##invocation-context)'secure)))
      ((##invocation-context)'secure #f)
      (##try (begin ,@body) () ((##invocation-context)'secure ,secure))
    )
)

; Special form evaluating specified expressions with security on.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-unprivileged
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-unprivileged . body)
   (define secure (string->symbol "#secure"))
   `(##let ((,secure ((##invocation-context)'secure)))
      ((##invocation-context)'secure #t)
      (##try (begin ,@body) () ((##invocation-context)'secure ,secure))
    )
)

; Special form evaluating specified expressions with partitioning off.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-unpartitioned
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-unpartitioned . body)
   (define partitioned (string->symbol "#partitioned"))
   `(##let ((,partitioned ((##invocation-context)'partitioned)))
      ((##invocation-context)'partitioned #f)
      (##try (begin ,@body) () ((##invocation-context)'partitioned ,partitioned))
    )
)

; Special form evaluating specified expressions with partitioning on.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-partitioned
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-partitioned . body)
   (define partitioned (string->symbol "#partitioned"))
   `(##let ((,partitioned ((##invocation-context)'partitioned)))
      ((##invocation-context)'partitioned #t)
      (##try (begin ,@body) () ((##invocation-context)'partitioned ,partitioned))
    )
)

; Special form evaluating if any value in a collection association defined by (@ ...)
; satisfies a given boolean expression. 
; @detail
; Used for expressions that are evaluated both by the persistence layer and the 
; VM in the context of the current class.
; @arg expr any The boolean expression to evaluate.
; @ret boolean #t if any value of is true.
; @example
; (any (= (@ addresses type name) "Home"))
(define-macro (any expr)
   (define (expandAssoc expr assocList var)
      (let loop ((e expr))
         (if (pair? e)
            (cond 
               ((equal? e assocList)
                  var
               )
               (else
                  (map loop e) 
               )
            )
            e
         )
      )
   )
   (define assocLists '())
   (define varIndex 0)
   (let pass1 ((expr expr))
      (if (pair? expr)
         (if (eq? '@ (car expr))
            (set! assocLists (cons expr assocLists))
            (for-each pass1 expr)
         )
      )
   )
   (for-each
      (lambda (assocList)
         (set! expr
            (let loop
               (
                  (var 'this)
                  (class (get-value 'sys:current-class))
                  (assocs (cdr assocList))
               )
               (cond
                  ((null? assocs)
                     (set! expr (expandAssoc expr assocList var))
                  )
                  ((symbol? (car assocs))
                     (let
                        (
                           (attr (class'getAttribute (car assocs)))
                           (nextVar (string->symbol (string-append "#a" (number->string varIndex))))
                        )
                        (if (attr'collection)
                           `(##some
                              (lambda (,nextVar) ,(loop nextVar (attr'type) (cdr assocs)))
                              (,var',(car assocs))
                           )
                           (loop `(,var',(car assocs)) (attr'type) (cdr assocs))
                        )
                     )
                  )
                  ((pair? (car assocs))
                     (loop `((lambda (this) (if (##and (##not (##null? this)) ,(car assocs)) this)) ,var)
                        (let find-derived ((class class) (expr (car assocs)))
                           (if (pair? expr)
                              (case (car expr)
                                 ((instance?)
                                    (if (and (list? expr) (= (length expr) 3) (equal? (cadr expr) '(@)) (symbol? (caddr expr)))
                                       (let ((derived ((class'metadata)'getMetaclass (symbol->string (caddr expr)))))
                                          (if (class'isUpcast derived) derived class)
                                       )
                                       class
                                    )
                                 )
                                 ((and)
                                    (for-each (lambda (expr) (set! class (find-derived class expr))) (cdr expr))
                                    class
                                 )
                                 (else
                                    class
                                 )
                              )              
                              class
                           )
                        )
                        (cdr assocs)
                     )
                  )
                  (else
                     (error "err.scripting.assocSymbol")
                  )
               )
            )
         )
         (set! varIndex (+ varIndex 1))
      )
      assocLists
   )
   expr
)

; Computes the value of the current attribute using a specified rule set.
; @detail
; This macro can be used only in an attribute value or initializer.
; @arg name string The rule set name.
; @ret any The computed value.
; @example
; (rules "Default Address")
(define-macro (rules name)
   (if (null? (get-value 'sys:current-attribute))
      (error "err.scripting.misplacedValueSpecialForm" "rules")
      `(##SysRulesEngine'invoke this ,name ',((get-value 'sys:current-attribute)'symbol))
   )
)

; Special form computing a derived association using its where clause
; applied to the base association.
; @arg base-assoc any The base association expression.
; @ret any The filtered value.
; @example
; (derive-from (@ entityPartitipants))
(define-macro (derive-from base-assoc)
   (define attribute (get-value 'sys:current-attribute))
   (define type (attribute'type))
   (define where (ifnull (attribute'where) #t))
   (unless (eq? where #t)
      (set! where `(##ifnull ,where #f))
   )
   (unless (or (type'primitive) (eq? where #t))
      (set! where `(##and (##instance? this ,type) ,where))
   )
   (if (null? attribute)
      (error "err.scripting.misplacedValueSpecialForm" "derive-from")
      (let ((expr `(##filter (lambda (this) ,where) (##value->collection ,base-assoc))))
         (if (attribute'collection) expr `(##collection->value ,expr))
      )
   )
)

; Matches an s-expression against a pattern, similar in concept to string-match.
; Useful for parsing where clauses. Specific parts of the matching clause can be returned if indicated
; by a comma/unquote in the pattern. Note the comma must still be quoted, so either start the pattern with a ' or, use ,', to
; re-quote the unquote character. There are also special matching operators:
; 
; ,(subclass <Parent Class>): matches any subclass
; ,(? <pattern>): if pattern is present it is matched; if absent, the pattern is ignored. 
; ,(or <pattern1> [<pattern2> <pattern3> ...): Matches any of <pattern1>, <pattern2>, ...
; 
; @arg expr list The s-expression to analyze. Normally a where clause.
; @arg pattern list The s-expression representing a pattern.   
; @ret assoc list|#f The matching assoc list, or #f if there is no match. 
; @see expr-case string-match
; @example
; (expr-match `(= (@@ Person addrs) ,(oid #zB4B47BCACA0142A3A65A6EC355758131)) '(= (@@ Person addrs) ,personOid))
; => ((personOid . #<OID:1:V32:B4B47BCACA0142A3A65A6EC355758131>))
; (expr-match `(= (@@ Person addrs) ,(oid #zB4B47BCACA0142A3A65A6EC355758131)) '(= (@@ ,(subclass Entity) addrs) ,personOid))
; => ((personOid . #<OID:1:V32:B4B47BCACA0142A3A65A6EC355758131>))
; (define attr 'addrs)
; (expr-match `(= (@@ Person addrs) ,(oid #zB4B47BCACA0142A3A65A6EC355758131)) `(= (@@ ,',(subclass Entity) ,attr) ,',personOid))
; => ((personOid . #<OID:1:V32:B4B47BCACA0142A3A65A6EC355758131>))
; (expr-match '(in? (@ name) "a" "b" "c") '(in? (@ name) ,@names))
; => ((names "a" "b" "c"))
; (expr-match '(in? (@ name) "a" "b" "c") '(in? (@ name) "a" "b" ,@names))
; => ((names "c"))
; (expr-match  `(= (@) ,(oid #zB4B47BCACA0142A3A65A6EC355758131)) '(,(or in? =) (@) ,@oids))
; => ((oids #<OID:1:V32:B4B47BCACA0142A3A65A6EC355758131>))
; (expr-match  `(in? (@) ,(oid #zB4B47BCACA0142A3A65A6EC355758131) ,(oid #z314541EEA5AC4D338C7A36A6D4440FCE)) '(,(or in? =) (@) ,@oids))
; => ((oids #<OID:1:V32:B4B47BCACA0142A3A65A6EC355758131> #<OID:1:V32:314541EEA5AC4D338C7A36A6D4440FCE>))
(define (expr-match expr pattern)
   (define matches ())
   (and
      (let loop
         (
            (subExpr expr)
            (subPattern pattern)
            (level "")
         )
         (logger'dump level 
            (format "subExpr = '{0}'" subExpr) "," 
            (format "subPattern = '{0}'" subPattern) "," 
            (format "matches = '{0}'" matches)
         )
         (cond
            ((pair? subPattern)
               (logger'dump level "pair subPattern  car is" (car subPattern))
               (cond
                  ;match (,@token) which is '((unquote-splicing . (token . ())) . ())
                  ((and (pair? (car subPattern)) (eq? (caar subPattern) 'unquote-splicing))
                     (logger'dump level "match (,@token)")
                     (cond
                        ((and (pair? (cdar subPattern)) (symbol? (cadar subPattern)))
                           (logger'dump level "found unquote-splicing match: " subExpr)
                           (set! matches (cons (cons (cadar subPattern) subExpr) matches))
                           #t
                        )
                        (else
                           (error "invalid operator pattern, subpattern = {0}, subExpr = {0}" subPattern subExpr)
                        )
                     )
                  )
                  ; match ,<expr> which is '(unquote . (<expr> . ()))
                  ((eq? (car subPattern) 'unquote)
                     (logger'dump level "match ,<expr>")
                     (cond
                        ; match ,token which is '(unquote . (token . ()))
                        ((and (pair? (cdr subPattern)) (symbol? (cadr subPattern)))
                           (logger'dump level "found match: " subExpr)
                           (set! matches (cons (cons (cadr subPattern) subExpr) matches))
                           #t
                        )
                        ; match ,(operator argument) which is '(unquote . ((operator . (argument . ())) . ()))
                        ((and (pair? (cdr subPattern)) (pair? (cadr subPattern)))
                           (let ((operator (caadr subPattern))
                                 (argument (cdadr subPattern)))
                              (logger'dump level "operator = " operator ", argument =" argument)
                              (cond
                                 ((and (eq? operator 'subclass) (pair? argument) (symbol? subExpr))
                                    (let ((parentClass (((invocation-context)'metadata)'getClassMeta (symbol->string (car argument))))
                                           (childClass (((invocation-context)'metadata)'getClassMeta (symbol->string subExpr))))
                                       ;return:
                                       (parentClass'isUpcast childClass)
                                    )
                                 )
                                 ((eq? operator '?)
                                    (if (null? subExpr)
                                       #t
                                       ; else:
                                       (let
                                          (
                                             (result 
                                                (if (loop subExpr (car argument) (string-append level "?  ")) 
                                                    #t 
                                                   ;else:
                                                   'FAIL_COND_MATCH
                                                )
                                             )
                                          )
                                          result
                                       )
                                    )
                                 )
                                 ((eq? operator 'or)
                                    (some
                                       (lambda (subSubPattern)
                                          (loop subExpr subSubPattern (string-append level "or "))
                                       )
                                       argument
                                    )
                                 )
                                 (else
                                    (error "invalid operator pattern, subpattern = {0}, subExpr = {0}" subPattern subExpr)
                                 )
                              )
                           )
                        )
                        (else
                           (error "unrecognized quoted pattern {0}" subPattern)
                        )
                     )
                  )
                  (else
                     (let
                        (
                           (consumedSubPattern subPattern)
                        )
                        (and
                           (pair? subPattern)
                           (let loop2
                              (
                                 (subExpr subExpr)
                                 (subPattern subPattern)
                              )
                              (let
                                 (
                                    (result 
                                       (loop 
                                          (when (pair? subExpr) 
                                             (car subExpr)
                                          ) 
                                          (when (pair? subPattern) 
                                             (car subPattern)
                                          ) 
                                          (string-append level "   ")
                                       )
                                    )
                                 )
                                 (if (equal? result 'FAIL_COND_MATCH)
                                    (loop2 subExpr (cdr subPattern))
                                    ; else:
                                    (begin
                                       (set! consumedSubPattern subPattern)
                                       result
                                    )
                                 )
                              )
                           )
                           (loop (when (pair? subExpr) (cdr subExpr)) (when (pair? consumedSubPattern) (cdr consumedSubPattern)) (string-append level "   "))
                        )
                     )
                  )
               )
            )
            ((symbol? subPattern)
               (eq? subPattern subExpr)
            )
            ((null? subPattern)
               (null? subExpr)
            )
            (else
               (logger'dump "unrecognized type, falling back on equal? comparison")
              (equal? subPattern subExpr)
            )
         )
      )
      (reverse matches) ;reversed to maintain order.
   )
)

; Special form evaluating sequentially conditional clauses until
; a test condition with non-false value is encountered.
; @arg expr The expression to match.
; @arg list clauses The conditional clauses, each in one of the following forms:
; (<const-pattern1> ... <result>) - if the value of (expr-match expr <pattern>) is not #f, each token is made available as a local variable.
; (else <result>) - the value of <result> is returned (this must be the last clause).
; @ret any The result of the evaluation, or () if no non-false test condition is encountered.
; @see expr-match case if
; @example
; (define where `(= (@@ Person addrs) ,(oid #zB4B47BCACA0142A3A65A6EC355758131)))
; (expr-case where
;    ((= (@@ ,(subclass Entity) addrs) ,entityOid)
;        (logger'debug "oid = " entityOid)
;    )
;    (else
;       (error "err.app.demo.invalidWhere")
;    )
; )
(define-macro (expr-case expr . clauses)
   (define (find-variables pattern vars)
      (if (pair? pattern)
         (if 
            (and
               (memq (car pattern) '(unquote unquote-splicing))
               (pair? (cdr pattern))
               (symbol? (cadr pattern))
            )
            (set! vars (cons (cadr pattern) vars))
            ;else:
            (for-each 
               (lambda (subPattern)
                  (set! vars (find-variables subPattern vars))
               )
               pattern
            )
         )
      )
      ;return:
      vars
   )
   (define expr-var (string->symbol "#e"))
   (define vars (string->symbol "#v"))
   `(##let ((,expr-var ,expr))
      (##cond
         ,@(map
            (lambda (statement)
               (define varSymbols (sort! (find-variables (car statement) '()) (sort-key < symbol->string)))
               (if (pair? statement)
                  (if (eq? (car statement) 'else)
                     `(else ,@(cdr statement))
                     `((##expr-match ,expr-var ',(car statement)) =>
                        (lambda (,vars)
                           (##apply
                              (lambda ,varSymbols ,@(cdr statement))
                              (##map
                                 (lambda (sym)
                                    (##cond ((##assoc sym ,vars) => ##cdr) (else 'UNDEFINED))
                                 )
                                 '(,@varSymbols)
                              )
                           )
                        )
                     )
                  )
                  (error "invalid arg")
               )
            )
            clauses
         )
      )
   )
)

; Throws an optimistic locking exception for the given item.
; This function does not return.
;
; @arg oid-holder nexj.core.persistence.OIDHolder The OID holder identifying the item
; on which optimistic lock contention occurred.
(define (throw-optimistic-lock oid-holder)
   (throw (nexj.core.persistence.OptimisticLockException'new oid-holder))
)

; Converts a generator function to an iterator.
; 
; A generator function has one argument. This argument, called yield, is itself
; a function that accepts one argument. The argument to yield is the result of
; the generator on the current iteration. For the next iteration, execution
; continues within the generator, immediately following the yield function. The
; generator terminates by returning normally. The yield function always returns
; null.
; 
; @arg generator any The generator function, takes one argument.
; @ret any The cursor function.
; @example
; (define itr
;    (generator->iterator
;       (lambda (yield)
;          (yield 1)
;          (yield 2)
;          (yield ())
;          (yield 5)
;          (for-each
;             yield  ; Equivalent to: (lambda (item) (yield item)) 
;             '("hello" a 42)
;          )
;       )
;    )
; )
; 
; ; #<nexj.core.util.FunctionIterator@7d2149c5>
; 
; (itr'hasNext)
; 
; ; #t
; 
; (for-each (lambda (item) (logger'info item)) itr)
; 
; ; 16:28:36,349 INFO  [GlobalEnvironment] 1
; ; 16:28:36,353 INFO  [GlobalEnvironment] 2
; ; 16:28:36,353 INFO  [GlobalEnvironment] ()
; ; 16:28:36,353 INFO  [GlobalEnvironment] 5
; ; 16:28:36,353 INFO  [GlobalEnvironment] hello
; ; 16:28:36,354 INFO  [GlobalEnvironment] a
; ; 16:28:36,354 INFO  [GlobalEnvironment] 42
; 
; (itr'hasNext)
; 
; ; #f
; 
; 
; @example
; (define itr (generator->iterator (lambda (yield) ())))
; 
; #<nexj.core.util.FunctionIterator@45ff5109>
; 
; (itr'hasNext)
; 
; ; #f
; 
(define (generator->iterator generator)
   (letrec
      (
         (yield ())
         (hasNext #t)
         (continue
            (lambda ignored-args
               (generator suspend)
               (set! hasNext #f)
            )
         )
         (suspend
            (lambda (value)
               (call/cc
                  (lambda (k-resume-from-yield)
                     (set! continue k-resume-from-yield)
                     ; return the value that was passed to "suspend":
                     (yield value)
                  )
               )
               ; resume the "suspend" statement and return:
               ()
            )
         )
      )
      ; return the cursor:
      (nexj.core.persistence.virtual.FunctionIterator'new
         (lambda ()
            (call-with-continuation-barrier
               (lambda ()
                  (when hasNext
                     (call/cc
                        (lambda (k-main)
                           (set! yield k-main)
                           (continue ())
                           ()
                        )
                     )
                  )
               )
            )
         )
         (lambda () hasNext)
         (invocation-context)
      )
   )
)

; Determines if an expression is being evaluated as part of a query.
; @ret boolean #t if an expression is being evaluated as part of a query.
; @example
; (if (query?) #t (this'updated'readOnly)) ; use in a security filter
(define-macro (query?)
   ; the persistence layer intercepts the operator and can evaluate it to #t
   '(begin this #f) ; mentioning "this" to prevent if-folding
)

; Determines if an expression is being evaluated as part of a root query node.
; @ret boolean #t if an expression is being evaluated as part of a root query node.
; @example
; (if (and (query-root?) (Document'isContactDetailContext)) <optimized-security> <full-security>) ; use in a security filter
(define-macro (query-root?)
   ; the persistence layer intercepts the operator and can evaluate it to #t
   '(begin this #f) ; mentioning "this" to prevent if-folding
)

; Converts an OID to a String.
; @arg o oid The OID object to convert.
; @ret string The converted String.
; @example
; (define o (oid 1 2 3)) => #<OID:3:I1:1:I1:2:I1:3>
; (oid->string o) => "C100000001C100000002C100000003"
(define (oid->string o)
   (cast string (o'toBinary))
)

; Converts a String to an OID.
; @arg s string The String object to convert.
; @ret oid The converted OID.
; @example
; (string->oid "C100000001C100000002C100000003") => #<OID:3:I1:1:I1:2:I1:3>
(define (string->oid s)
   (OID'fromBinary (cast binary s))
)

; Reads a single instance of a class. (Must be a single instance).
; @detail
; IMPORTANT: The where clause must select a single instance, otherwise read-instance will throw an assertion error.
; 
; Associated objects can be retrieved using nested lists in the attributes argument. Simply specify a 
; non-primitive attribute name then a list of one or more attributes.  
; e.g. (read-instance Entity '(firstName lastName (addrs city state zip)) ... 
; 
; These nestings can continue... 
; e.g. (read-instance Entity '(firstName lastName (addrs city state zip (type name))) ...
; 
; Attributes can also be retrieved polymorphically - meaning attributes that only exist on subclasses 
; can be retrieved when reading the base class. (see examples)
; 
; Attribute names in the where clause may take the simple form of <attributeName> or (@ <attributeName>).  
; The second form is recommended.  In this case the "@" represents the current instance and may be used 
; to traverse arbitrarily long association paths.  
; e.g. on User we may specify (= (@ person homeAddress city) "Toronto")
; @arg class metaclass The class object (must be persistent).
; @arg attributes list The list of attributes, or associated objects, to proactively retrieve.  
; '() retrieves no attributes and any attributes accessed after such a read will trigger a lazy 
; load.  See examples for special downcast (@@) syntax.
; @arg where list The where clause selecting a single instance.  See examples for information on associations, reverse associations and qualified associations.
; @arg xlock boolean If #t, an exclusive lock is set in persistent storage on the retrieved top level instance until the transaction is finished.  null '() for the default value of #f.
; @ret Object The retrieved instance or () if not found.
; @see Object'read openCursor for-each-page
; @example
; (read-instance Person '(firstName lastName) '(and ( = (@ firstName) "Joe") (= (@ lastName) "Test")) #f)
; => ; () if Joe doesn't exist
; => ; #<Instance<Person, OID:1:V32:4AA92BA6E6A64CE5BBEB8... if Joe does exist
;
; ; More advanced read
; (define fn "Joe")
; (read-instance Person 
;   '(firstName lastName (addrs city state (type name))) ; read in city and state attributes as well as the type and it's name for all of Joe's addresses
;   `(and ( = (@ firstName) ,fn) (= (@ lastName) "Test")) #f)  ; variable substitution for the value of the firstName
;
; ; Use of the any operator
; (read-instance Person
;   '(firstName lastName)
;   '(any (= (@ addrs city) "Toronto")) ; two part association path
;   '())
;
; ; Attribute downcast syntax (@@) - Polymorphic read
; ; Used to proactively load subclass specific attributes when reading from a base class
; (read-instance Entity
;   '(fullName lastName (@@ Person homePhone)) ; only retrieve the homePhone attribute for the Person subclass of Entity
;   `(= (@ lastName) "Test") 
;   #f)
;
; ; Conditions in where clause association paths
; ; This syntax allows us to restrict parts of association paths to specific subclasses and also to apply arbitrary conditions on association paths
; ; 1) Most common use case: limit an association based on a qualifier e.g. look for all Entities with a user named "Shaw" playing their advisor role
; (read-instance Entity
;   '(fullName)
;   '(= (@ coverage (= (@ coverageRole roleName) "Advisor") userPerson lastName) "Shaw")
;   '())
;
; ; 2) Subclass restriction example: note that homeAddress is an attribute that is only found on the Person subclass of Entity
; (read-instance Telcom
;   '(fullName)
;   '(= (@ entity (instance? (@) Person) homeAddress city) "Toronto")
;   '())
;
; ; 3) Arbitrary condition example: reads all instances of the Telcom class that has an entity with a lastname of "Shaw" that has an address in "Toronto" with and address2 line that is not null.
; (read-instance Telcom
;   '(fullName)
;   '(not (null? (@ entity (= (@ lastName) "Shaw") addrs (= (@ city) "Toronto") address2)))
;   '())
;
; ; Note that conditions may be applied in reverse associations as well as forward associations
;
; ; Where Clause reverse association syntax (@@)
; ; Used to query from the end of an association "back" to the class you are reading.
; ; Forward associations take the form (@ part1 part2 part3 ...).  The @ represents an instance of the class being read.
; ; Revese associations take the form (@@ <ClassName> part3 part2 part1) where part1 represents an attribute with a type of the class being read.
; ; You can usually just navigate the forward association, but you may see this syntax if you are tracing requests from NexJ UI clients
; (read-instance Address '() `(= (@@ User person addrs) ,(user)) '()) ; reverse from Address. note that this is equivalent to (((user)'person)'addrs)
; (read-instance Address '() `(= (@ entity user) ,(user)) '()) ; forward from Address is also equivalent
(define (read-instance class attributes where xlock)
   (let ((col (class'read attributes where '() -1 0 xlock)))
      (assert (<= (col'count) 1) "fail.persistence.multipleReadInstances")
      (if (col'empty) '() (col'get 0))
   )
)

; Opens a cursor over class instances and pages over it.
; @detail
; Associated objects can be retrieved using nested lists in the attributes argument. Simply specify a 
; non-primitive attribute name then a list of one or more attributes.  
; e.g. (for-each-page Entity '(firstName lastName (addrs city state zip)) ... 
; 
; These nestings can continue... 
; e.g. (for-each-page Entity '(firstName lastName (addrs city state zip (type name))) ...
; 
; Attributes can also be retrieved polymorphically - meaning attributes that only exist on subclasses 
; can be retrieved when reading the base class. (see read-instance examples)
; 
; Attribute names in the where clause may take the simple form of <attributeName> or (@ <attributeName>).  
; The second form is recommended.  In this case the "@" represents the current instance and may be used 
; to traverse arbitrarily long association paths.  
; e.g. on User we may specify (= (@ person homeAddress city) "Toronto")
; @arg class metaclass The class object (must be persistent).
; @arg attributes list The list of attributes, or associated objects, to proactively retrieve.
; '() retrieves no attributes and any attributes accessed after such a read will trigger a lazy 
; load.  See examples in (read-instance ... for special downcast (@@) syntax.
; @arg where list The where clause selecting a single instance.  See examples in (read-instance ... for information on associations, reverse associations and qualified associations.
; @arg order-by  list A list of (<expression> . <ascending>) pairs, by which to order the resulting collection. <ascending> is #t or #f (default #t).
; @arg max-count integer The maximum instance count to process (negative for unlimited).
; @arg page-size integer The maximum instance count per page.
; @arg lazy? boolean #t to retrieve the attributes for each page;
; #f to query for the attributes with the cursor (this can cause optimistic
; locking exceptions if related objects are updated and pre-committed while paging);
; () to use #t if any related objects are retrieved.
; @arg fun lambda The function of one argument to call with a collection of instances for each page.
; NOTE: Do not commit the UOW in this function, only pre-commit is allowed;
; cursors do not last past the transaction, depending on the persistence engine.
; @ret ()
; @see Object'read read-instance openCursor
; @example
; ; Output to stdout a password file for the test tool
; (for-each-page User '(loginName fullName) '() '() '() 100 #f
;    (lambda (page)
;       (for-each
;          (lambda (u)
;             (display (u'loginName))
;             (display "=password")
;             (newline)
;          )
;          page
;       )
;    )
; )
(define (for-each-page class attributes where order-by max-count page-size lazy? fun)
   (if (null? lazy?) (set! lazy? (some pair? attributes)))
   (let ((cursor (class'openCursor (if lazy? '() attributes) where order-by max-count '() '())))
      (try
         (do ((page (cursor'next page-size) (cursor'next page-size)))
            ((page'empty))
            (if lazy? (class'read attributes `(in? (@) ,page) '() '() '() '()))
            (fun page)
         )
         ()
         (cursor'close)
      )
   )
)

; Special form defining a session variable with an initial value. The value must be serializable.
; May occur only in a global scope.
; 
; @arg name symbol The variable name.
; @arg value any The initial value.
; @ret any The initial value.
(define-macro (define-session name value)
   `(begin
       ,(((invocation-context)'metadata)'addSessionSymbol name)
       (define ,name ,value)
    )
)

; Suppresses all logging within the block, unless the DUMP level for the category is visible.
; @arg category string Logger category which controls the logging.
; @arg body any The expressions to evaluate.
(define-macro (begin-stealth category . body)
   (define enabled (string->symbol "#enabled?"))
  `(##let ((,enabled (##nexj.core.util.Logger'enabled)))
      (##try
         (begin (##nexj.core.util.Logger'enable ,category) ,@body)
         ()
         (##nexj.core.util.Logger'enabled ,enabled)
      )
   )
)

; Sets up a synchronization barrier and executes the arguments in parallel. The values of the
; evaluated arguments are returned using multi-value return semantics. Subsequent invocations
; return the cached value(s) for each promise.
;
; Parallel invocation of the arguments is an illusion; they are actually executed one-at-a-time,
; using continuations to switch to the other arguments when a blocking I/O operation is
; encountered. Blocking I/O operations are executed in parallel. So is any code that has been
; wrapped with the (dispatch-io ...) special form.
;
; @arg args sequence The promises returned by delay-io. The last argument may be a timeout
; value to return an error if the barrier does not complete promptly. The timeout is specified
; in milliseconds and defaults to 0 (infinite).
; @ret any The value(s) returned by the promise.
(define (force-io . args)
   (define argCount (length args))
   (define lastArg (list-ref args (- argCount 1)))
   (define timeoutSpecified? (number? lastArg))
   (define timeout (if timeoutSpecified? lastArg 0))
   (define endTime (+ (cast long (now)) timeout))
   (define promises (if timeoutSpecified? (remove lastArg args) args))
   (define promiseCount (length promises))
   (define specVec (make-vector promiseCount))
   (define semaphore (java.util.concurrent.Semaphore'new 1 #f))
   (define readySemaphore (java.util.concurrent.Semaphore'new promiseCount #f))

   ; Executes a spec & returns a wait-spec for it.
   ;
   ; @arg spec message Description for spec
   ; @ret message
   (define (execute-spec spec)
      (when (spec'exception)
         (readySemaphore'release promiseCount) ; Prevent unfinished task threads from blocking
         (error "fail.async.barrier.error" (spec'exception))
      )
      (try
         (spec'continuation
            (call/cc ; returns continuation into "reschedule" (the delay-io'd promise)
               (lambda (yield)
                  (if (null? (spec'promise))
                     ((spec'continuation) (cons yield (spec'result)))
                     ; else:
                     ((spec'promise)
                        ; #reschedule
                        (lambda ()
                           (let
                              (
                                 ; Yield to force-io a continuation into "reschedule (the delay-io'd promise).
                                 ; The continuation goes into "continuation" of the corresponding wait spec.
                                 (yieldResult (call/cc yield))
                              )
                              (set! yield (car yieldResult))
                              (cdr yieldResult)
                           )
                        )
                        ; #done
                        (lambda (result)
                           (spec'result result)
                           (spec'done #t)
                           (yield ())
                        )
                        ; #spec
                        spec
                        ; #invokerQueueName
                        "SysAsyncIO"
                     )
                  )
               )
            )
         )
         ; catch:
         (lambda (ex)
            (readySemaphore'release promiseCount) ; Prevent unfinished task threads from blocking
            (error "fail.async.barrier.error" ex)
         )
      )
      (spec'promise '())
      (spec'ready #f)
      ()
   )
   (readySemaphore'acquire promiseCount) ; Make it a binary semaphore.
   (call-with-continuation-barrier
      (lambda ()
         ; Initialize the ready specs -- all promises are ready.
         (let init
            (
               (n 0)
               (p promises)
            )
            (unless (null? p)
               (vector-set! specVec n
                  (message
                     (: id n)
                     (: promise (car p))
                     (: ready #t)
                     (: done #f)
                     (: exception #f)
                     (: semaphore semaphore)
                     (: readySemaphore readySemaphore)
                  )
               )
               (init (+ n 1) (cdr p))
            )
         )
         (let executeMore
            (
               (remainingTimeout timeout)
            )
            (for-each
               execute-spec
               (filter
                  (lambda (spec) (spec'ready))
                  specVec
               )
            )
            (when
               (some
                  (lambda (spec)
                     (not
                        (or (spec'ready) (spec'done))
                     )
                  )
                  specVec
               )
               (readySemaphore'release) ; Allow one async thread to complete
               (unless
                  (if timeoutSpecified?
                     (semaphore'tryAcquire remainingTimeout (java.util.concurrent.TimeUnit'MILLISECONDS))
                     (semaphore'acquire)
                  )
                  (readySemaphore'release promiseCount) ; Prevent unfinished task threads from blocking
                  (error "fail.async.barrier.timeout")
               )
            )
            (when
               (some
                  (lambda (spec) (not (spec'done)))
                  specVec
               )
               (executeMore
                  (if (zero? timeout) 0 (max 1 (- endTime (cast long (now)))))
               )
            )
         )
         ; return:
         (apply
            values
            (map
               (lambda (spec) (spec'result))
               specVec
            )
         )
      )
   )
)

; Special form constructing lazy evaluation objects for execution within a synchronization barrier.
; @arg expr any Expression which evaluation will be delayed.
; @ret any Special object called promise, which will evaluate the expression once
; inside a synchronization barrier and returns its value when (force-io <promise>) is invoked on it.
(define-macro (delay-io . body)
   (define done (string->symbol "#done?"))
   (define value (string->symbol "#value"))
   (define return (string->symbol "#return"))
   `(let ((,done #f) (,value #f))
      (lambda (,(string->symbol "#reschedule") ,return ,(string->symbol "#spec") ,(string->symbol "#invokerQueueName"))
         (if ,done
            (,return ,value)
            (let
               (
                  (v (begin ,@body))
               )
               (set! ,done #t)
               (set! ,value v)
               (,return v)
            )
         )
      )
   )
)

; Special form for dispatching some code on another thread. Must be used within a (delay-io ...)
; form. Returns when the code has finished executing. Yields control to force-io to avoid blocking
; the other parallel tasks.
;
; @arg vars list The list of variables from the current scope to make available to the dispatched code.
; @arg body any The code to execute asynchronously.
; @ret any The return value of body.
(define-macro (dispatch-io vars . body)
   `(begin
      (integration-send-receive
         (message
            (: :class "ObjectQueueMessage")
            (: body
               (message
                  (: spec ,(string->symbol "#spec"))
                  (: code (lambda ,vars ,@body))
                  (: args (text-serialize (list ,@vars)))
               )
            )
            (: receiver
               (((invocation-context)'getComponentInstance "System.ClusterManager")'httpNode)
            )
         )
         ,(string->symbol "#invokerQueueName")
         ()
      )
      (,(string->symbol "#reschedule"))
   )
)


(import '(java.io.File))
(import '(java.lang.Class java.lang.String java.lang.System))
(import '(java.security.MessageDigest))
(import '(java.sql.Timestamp))
(import '(java.text.DateFormat))
(import '(java.util.AbstractList java.util.ArrayList
          java.util.Calendar java.util.Date java.util.GregorianCalendar java.util.HashMap
          java.util.StringTokenizer))
(import '(java.util.concurrent.Semaphore java.util.concurrent.TimeUnit))

(for-each
   (lambda (c)
      (try
         (import c)
         (lambda (e) e)
      )
   )
   '(java.util.AbstractList$Itr java.util.RandomAccessSubList java.util.AbstractList$SimpleListIterator
     nexj.core.reporting.ReportTimeoutException
     nexj.core.rpc.http.session.FileStore nexj.core.rpc.http.session.DataSourceStore)
)

; Import server-only classes
(import '(nexj.core.integration.Transformer))
(import '(nexj.core.meta.Attribute
          nexj.core.meta.Component
          nexj.core.meta.Event
          nexj.core.meta.GenericType
          nexj.core.meta.Metaclass
          nexj.core.meta.PrimitivePrivilege nexj.core.meta.PrivilegeGroup
          nexj.core.meta.PrivilegeSet nexj.core.meta.xml.XMLMetadata))
(import '(nexj.core.meta.integration.channel.mail.Mail))
(import '(nexj.core.meta.integration.service.Case nexj.core.meta.integration.service.Dispatch
          nexj.core.meta.integration.service.Jump nexj.core.meta.integration.service.Persist
          nexj.core.meta.integration.service.Send nexj.core.meta.integration.service.SendReceive
          nexj.core.meta.integration.service.Service nexj.core.meta.integration.service.Transform))
(import '(nexj.core.meta.workflow.Assignment nexj.core.meta.workflow.AutoCompletion
          nexj.core.meta.workflow.Branch nexj.core.meta.workflow.Catch
          nexj.core.meta.workflow.Concurrent nexj.core.meta.workflow.Decision
          nexj.core.meta.workflow.Fork nexj.core.meta.workflow.Join
          nexj.core.meta.workflow.ManualCompletion nexj.core.meta.workflow.Script
          nexj.core.meta.workflow.Semaphore
          nexj.core.meta.workflow.State nexj.core.meta.workflow.Timeout
          nexj.core.meta.workflow.Trigger nexj.core.meta.workflow.TryCatch
          nexj.core.meta.workflow.Wait nexj.core.meta.workflow.Workflow))
(import '(nexj.core.persistence.OID nexj.core.persistence.OIDSet
          nexj.core.persistence.OptimisticLockException nexj.core.persistence.sql.SQLCursor
          nexj.core.persistence.virtual.FunctionIterator))
(import '(nexj.core.rpc.Request nexj.core.rpc.RPCUtil nexj.core.rpc.TransferObject))
(import '(nexj.core.rpc.mail.MailReceiver))
(import '(nexj.core.rpc.queueing.DefaultObjectQueueDispatcher))
(import '(nexj.core.runtime.InstanceArrayList nexj.core.runtime.InstanceArrayList$InstanceIterator
          nexj.core.runtime.InvocationContext nexj.core.runtime.UnitOfWork))
(import '(nexj.core.scripting.ScriptingError nexj.core.scripting.ScriptingException))
(import '(nexj.core.util.ArrayIterator nexj.core.util.Base64Util nexj.core.util.Binary
          nexj.core.util.ErrorCode nexj.core.util.GenericHashHolder$GenericHashHolderIterator
          nexj.core.util.GenericHashTab$GenericHashTabIterator
          nexj.core.util.GenericHashTab$GenericHashTabValueIterator
          nexj.core.util.GUIDUtil
          nexj.core.util.HashHolder nexj.core.util.HashTab nexj.core.util.HashTab2D
          nexj.core.util.LocaleUtil nexj.core.util.SysUtil nexj.core.util.UncheckedException
          nexj.core.util.Logger))

(define OID nexj.core.persistence.OID)

(let ((metadata ((invocation-context)'metadata)))

   ; Export the functions that are safe to call through RPC
   (for-each
      (lambda (symbol) (metadata'addPublicSymbol symbol))
      '(abs acos and any append asin assoc assp assq assv atan
         bitwise-and bitwise-arithmetic-shift bitwise-arithmetic-shift-left
         bitwise-arithmetic-shift-right bitwise-bit-count bitwise-bit-field
         bitwise-bit-set? bitwise-copy-bit bitwise-copy-bit-field
         bitwise-first-bit-set bitwise-if bitwise-ior bitwise-length bitwise-not
         bitwise-reverse-bit-field bitwise-rotate-bit-field bitwise-xor boolean=? boolean?
         bytevector bytevector-append bytevector-copy bytevector=?
         bytevector-ieee-double-native-ref bytevector-ieee-double-ref
         bytevector-ieee-single-native-ref bytevector-ieee-single-ref
         bytevector-length bytevector? bytevector-s16-native-ref bytevector-s16-ref
         bytevector-s32-native-ref bytevector-s32-ref bytevector-s64-native-ref
         bytevector-s64-ref bytevector-s8-ref bytevector->sint-list bytevector-sint-ref
         bytevector-u16-native-ref bytevector-u16-ref bytevector-u32-native-ref
         bytevector-u32-ref bytevector-u64-native-ref bytevector-u64-ref
         bytevector->u8-list bytevector-u8-ref bytevector->uint-list bytevector-uint-ref
         caaaar caaadr caaar caadar caaddr caadr caar cadaar
         cadadr cadar caddar cadddr caddr cadr car cast cdaaar cdaadr
         cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar
         cddddr cdddr cddr cdr ceiling char-alphabetic? char-ci=?
         char-ci>=? char-ci>? char-ci<=? char-ci<? char-downcase char=? char-foldcase
         char>=? char-general-category char>? char->integer char<=? char-lower-case?
         char<? char-numeric? char? char-title-case? char-titlecase char-upcase char-upper-case?
         char-whitespace? complex? cons cons* cos div div-and-mod / div0 div0-and-mod0 endianness
         enum-set-complement enum-set-constructor enum-set-difference enum-set=? enum-set-indexer
         enum-set-intersection enum-set->list enum-set-member? enum-set-projection
         enum-set-subset? enum-set-union enum-set-universe = eq? equal-hash equal? eqv? error
         even? exact->inexact exact-integer-sqrt exact? exp expt finite? floor gcd >= >
         hashset-contains? hashset-copy hashset-equivalence-function
         hashset-hash-function hashset-mutable? hashset? hashset-size hashset-values
         hashtable-contains? hashtable-copy hashtable-entries
         hashtable-equivalence-function hashtable-hash-function
         hashtable-keys hashtable-mutable? hashtable? hashtable-ref
         hashtable-size hashtable-values if ifnull in? in-privilege?
         inexact->exact inexact? infinite? instance? integer->char
         integer? lcm <= length like? list list? list-ref list->string
         list-tail list->vector log < macro? match match? max member memp memq memv min - mod mod0
         modulo * nan? native-endianness != negative? not now null? number? number->string odd?
         oid oid->string or pair? + positive? procedure? quote quotient rational? real? remainder
         reverse round sin sqrt sint-list->bytevector string string-append string-ci=?
         string-ci>=? string-ci>? string-ci<=? string-downcase string-empty? string=?
         string-eol=? string-foldcase string>=? string>? string-hash string-length string<=?         
         string->list string<? string->number string->oid string? string-ref string->symbol
         string-titlecase string->utf16 string->utf32 string->utf8 string-upcase substring
         symbol=? symbol? symbol-hash symbol->string tan truncate u8-list->bytevector
         uint-list->bytevector utf16->string utf32->string utf8->string values vector
         vector-length vector->list vector? vector-ref zero? user locale-name principal-name
      )
   )

   ; Initialize the client symbol list to include all symbols that are supported on all clients
   (for-each
      (lambda (symbol) (metadata'addClientSymbol symbol))
      '(!= * + - / < <= = > >= abs acos append apply asin assoc assp assq assv atan
         binary? bitwise-and bitwise-arithmetic-shift bitwise-arithmetic-shift-left
         bitwise-arithmetic-shift-right bitwise-bit-count bitwise-bit-field
         bitwise-bit-set? bitwise-copy-bit bitwise-copy-bit-field
         bitwise-first-bit-set bitwise-if bitwise-ior bitwise-length bitwise-not
         bitwise-reverse-bit-field bitwise-rotate-bit-field bitwise-xor boolean=? boolean?
         bytevector bytevector-append bytevector-copy bytevector-copy! bytevector=?
         bytevector-fill! bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
         bytevector-ieee-double-ref bytevector-ieee-double-set! bytevector-ieee-single-native-ref
         bytevector-ieee-single-native-set! bytevector-ieee-single-ref bytevector-ieee-single-set!
         bytevector-length bytevector? bytevector-s16-native-ref bytevector-s16-native-set!
         bytevector-s16-ref bytevector-s16-set! bytevector-s32-native-ref bytevector-s32-native-set!
         bytevector-s32-ref bytevector-s32-set! bytevector-s8-ref bytevector-s8-set!
         bytevector->sint-list bytevector-sint-ref bytevector-sint-set! bytevector-u16-native-ref
         bytevector-u16-native-set! bytevector-u16-ref bytevector-u16-set! bytevector-u32-native-ref
         bytevector-u32-native-set! bytevector-u32-ref bytevector-u32-set! bytevector->u8-list
         bytevector-u8-ref bytevector-u8-set! bytevector->uint-list bytevector-uint-ref bytevector-uint-set!
         caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr
         cadr call-with-current-continuation call/cc car cdaaar cdaadr cdaar cdadar cdaddr
         cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling char->integer
         char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-foldcase
         char-lower-case? char-numeric? char-upcase char-upper-case? char-whitespace? char<=?
         char<? char=? char>=? char>? char? collection collection? complex? cons cons* cos
         div div-and-mod div0 div0-and-mod0 endianness enum-set-complement enum-set-constructor
         enum-set-difference enum-set=? enum-set-indexer enum-set-intersection enum-set->list enum-set-member?
         enum-set-projection enum-set-subset? enum-set-union enum-set-universe eq? equal-hash equal?
         eqv? error even? exact->inexact exact-integer-sqrt exact? exp expt finite? floor force
         format gcd get-value hashset-add! hashset-clear! hashset-contains? hashset-copy
         hashset-equivalence-function hashset-hash-function hashset-mutable? hashset? hastset-remove!
         hashset-size hashset-values hashtable-clear! hashtable-contains? hashtable-copy hashtable-delete!
         hashtable-entries hashtable-equivalence-function hashtable-hash-function hashtable-keys
         hashtable-mutable? hashtable? hashtable-ref hashtable-set! hashtable-size hashtable-update!
         hashtable-values ifnull in? inexact->exact inexact? infinite? instance? integer->char integer?
         interaction-environment invocation-context iteratable? iterator lcm length like?
         list list->string list->vector list-ref list-tail list? locale-name log macro? make-bytevector
         make-collection make-enumeration make-eq-hashtable make-eqv-hashtable make-hashtable
         make-string make-vector max member memp memq memv min mod mod0 modulo nan? native-endianness negative?
         not now null? number->string number? object odd? oid oid->string pair? positive? procedure?
         quotient rational? real? remainder remove remp remq remv reverse round set-car! set-cdr! sin
         sint-list->bytevector sqrt string string->list string->number string->symbol string-affix
         string-append string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-copy string-downcase
         string-empty? string-eol=? string-foldcase string-hash string-join string-length string-match string->oid
         string-pattern string-ref string-replace string-split string-trim string-upcase string->utf8
         string<=? string<? string=? string>=? string>? string? substring symbol->string symbol=? symbol?
         symbol-hash sys:assert sys:cast sys:finalize sys:log sys:log-localized sys:set-barrier
         sys:try tan throw timestamp? truncate u8-list->bytevector uint-list->bytevector utf8->string
         values vector vector->list vector-append vector-length vector-ref vector-set! vector-sort!
         vector? zero?
      )
   )
)
