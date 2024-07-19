(declare scope server)
; TODO: This file must be alphabetically early with respect to other library files
; as it contains macros that other library files depend on.  

;Macro to simplify specifying icons using
;using the icon.en.strings file
;expands (icon "name") to (format "icon.name")
(define-macro (icon name)
   (list '##format (string-append "icon." name))
)

; Similar to apply, but automatically expands association paths in the form (@ ...) into inner apply/maps.
; @arg operator lambda The function to be applied.
; @arg col sequence The collection or list to be applied.  Note this form, unlike apply, does not take additional arguments.
; @arg defaultValue any Takes one single additional argument that can be auto applied as a default if the value is null.
; @ret any The result of the expanded apply.
; @example
; (apply-collection + (@ campaignUsers campaignParticipants opportunities expectedRevenue) 0)
; => sum of all expectedRevenue values from the expansion of all collections between @ and expectedRevenue.
; where null is counted as 0. (normally any null values will result in the sum of null)
; The expansion in the above example will be similar to the following: (assuming @ is an instance of Campaign)
; (simplified abit for readability:
; (apply + 
;    (map (lambda (#a0) 
;          (apply + 
;             (map (lambda (#a1) 
;                   (apply + 
;                      (map (lambda (#a2) 
;                            (ifnull (#a2 'expectedRevenue) 0)
;                         ) 
;                         (#a1 'opportunities)
;                      )
;                   )
;                ) 
;                (#a0 'campaignParticipants)
;             )
;          )
;       ) 
;       (this 'campaignUsers)
;    )
; )
;
(define-macro (apply-collection operator col . defaultValue)
   (define varCount 0)
   (define varPrefix "#a")
   (define (getVar)
      (let ((varName (string-append varPrefix (number->string varCount))))
         (set! varCount (+ varCount 1))
         ;return:
         (string->symbol varName)
      )
   )
   (define mainClass (get-value 'sys:current-class));TODO: production mode
   ;(define mainClass (ifnull (get-value 'sys:current-class) (this':class))) ;TODO DEBUG mode
   
   (if (or (not (pair? col)) (not (eq? (car col) '@)) (null? mainClass))
      ;return:
      `(##apply ,operator ,col)
      ;else
      (let loop ((assocs (cdr col)) 
                 (var 'this) 
                 (class mainClass))
         (define expandedExpr '())        
         (logger'dump "apply2: assocs = " assocs ", var = " var ", class = " class)
         (if (null? assocs)
            (begin
               (logger'debug "apply2: returning variable")
               (set! expandedExpr var)
            )
            ;else
            (let ((value `(,var ',(car assocs)))
                  (attrObj (class'getAttribute (car assocs))))
                (logger'dump "apply2: assocation type = " ((attrObj'type)'name))
                (cond 
                   (((attrObj'type)'primitive)
                     (logger'dump "apply2: primitive")
                      (unless (= (length assocs) 1)
                        (error "apply2: invalid association path, primitive must be the last item in the association path")
                      )
                      ;return:
                      (if (null? defaultValue)
                        (set! expandedExpr value)
                        ;else
                        (set! expandedExpr `(##ifnull ,value ,(car defaultValue)));simplify coding by allowing a default value (like 0)
                      )  
                   )
                   ((not (attrObj'collection))
                     (logger'dump "apply2: not collection")
                     (if (> (length assocs) 1)
                        (begin
                           (logger'dump "recalling with new var")
                           ;return:
                           (set! expandedExpr (loop (cdr assocs) value (attrObj'type)))
                        )
                        ;else
                        (begin
                           (logger'dump "apply2: hoping the apply function takes a non primitive!")
                           ;return:
                           (set! expandedExpr value)
                        )
                     )
                   )
                   ((attrObj'collection)
                     (let ((nextVar (getVar)))
                        (logger'dump "apply2: collection")
                        (set! expandedExpr
                           `(##apply ,operator
                              (##map
                                 (lambda (,nextVar)
                                    ,(loop (cdr assocs) nextVar (attrObj'type))
                                 )
                                 ,value
                              )
                           )
                        )
                     )
                  )
               )    
            )
         )
         (logger'dump "apply2: expandedExpression = " expandedExpr)
         ;return:
         expandedExpr
      )
   )
)   


; Special form evaluating specified expressions with partitioning off, with 
; the added condition that the current user is null,
; has no effect if partitioning is not implemented
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-unpartitioned-when-null-user
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-unpartitioned-when-null-user . body) `(begin ,@body))

; Special form evaluating specified expressions with a specified partition and user,
; equivalent to begin-switch-user if partitioning is not implemented 
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-switch-partition-user partition usr
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-switch-partition-user partition usr . body) `(begin-switch-user ,usr ,@body))

; Special form evaluating specified expressions with a specified partition,
; has no effect if partitioning is not implemented
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-switch-partition partition
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-switch-partition partition . body) `(begin ,@body))

; Special form evaluating specified expressions with a specified fragment
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-switch-fragment "fragment1"
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-switch-fragment fragmentName . body)
   (define oldFragment (string->symbol "#oldFragment"))
   `(##let ((,oldFragment ((##invocation-context)'fragmentName)))
      ((##invocation-context)'fragmentName ,fragmentName)
      (##try 
         (begin 
            ,@body
         ) 
         ;catch:
         ()
         ;finally: 
         ((##invocation-context)'fragmentName ,oldFragment)
      )
    )
)

; Special form evaluating specified expressions logged in as the given user.
; @arg usr string The user name to log in.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-switch-user usr
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-switch-user usr . body)
   (define oldUser (string->symbol "#oldUser"))
   `(##let ((,oldUser (##user)))
      ((##invocation-context)'login (##User'getUser ,usr))
      (##try (begin ,@body) () 
         (##unless (##null? ,oldUser)
            ((##invocation-context)'login ,oldUser)
         )
      )
    )
)

; Helper macro to create catch lamba expressions that only catch specific 
; exceptions.
; @arg e symbol Variable name for the exception being caught
; @arg clauses any Expressions for catching the exception in the form (class/code  body)
; @example
; (try 
;     (commit)
;     (catch e 
;         ("err.entity.circularParent"
;            (logger'debug "circular parent caught!")
;         )
;         (nexj.core.scripting.ScriptingException
;           (logger'debug "scripting exception caught!")
;           (do something else)
;         )
;     )
;     (logger'debug "finally expression")
; )
(define-macro (catch e . clauses)
   (define condClauses
      (map
         (lambda (clause)
            (unless (pair? clause)
               (error "catch: incorrect clause ,should be pair")
            )
            `((if (##string? ,(car clause)) 
                  (##and (##instance? ,e nexj.core.util.UncheckedException) (##= (,e'errorCode) ,(car clause)))
                  ;else
                  (##instance? ,e ,(car clause))
              )
                ,@(cdr clause)
            )
         )
         clauses
      )
   )      
   (unless (symbol? e)
      (error "catch: argument e must be a symbol")
   )
   ;return:
   `(lambda (,e)
      (##cond
         ,@ condClauses
         (else
            (##logger'dump "catch: exception did not meet conditions, rethrowing")
            (##throw ,e)
         )
      )
   )
)   

; Helper macro to sum up a collection.  substitues 0 for null values.
; @arg col any Association path to a collection to apply + to.
; @ret any Result of +
(define-macro (sum col)
   `(##apply-collection + ,col 0)
)

; Macro for delete action conditions.  Checks if parent has the deleted attribute set (soft deleted) or is null (normal
; cascade deleted.  Used to differentiate delete cascades from deletes invoked directly on the instance.
; @arg attr symbol The parent attribute to check.  
; @ret #t if this is a cascade delete, #f if this instnace is deleting on its own.
; @example
; ;On Telcom delete action :
; (unless (is-delete-cascade 'entity)
;    ((@ entity)'update) ;trigger an update when deleting from a subcollection
; )
(define-macro (is-delete-cascade attr)
   `(##ifnull ((this ,attr) 'deleted) #t)
)
