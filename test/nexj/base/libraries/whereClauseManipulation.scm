(declare scope all) ;we use some of these in the client.
;function for parsing where clause:
;expr = the subset of the where clause.
;operator = the operator to check for - e.g. =, like
;attribute = the attribute to check for - e.g. (@ firstName)
(define (checkExpression expr operator attribute)
   (and (list? expr) (eq? (car expr) operator) (pair? (cdr expr)) (equal? (cadr expr) attribute))
)

;Function for replacing an attribute condition clause
;where = original where clause to be processed.
;operator = the operator to check for - e.g. =, like
;attribute = the attribute to check for - e.g. (@ firstName)
;replacementTextFunction = lambda function that is invoked to generate the replacement text
;                          the attribute condition clause is passed as a parameter
(define (replaceAttributeClause where operator attribute replacementTextFunction)
   (let ((newWhere #f))
      (set! newWhere
         (let loop ((expr where))
            (if (pair? expr)
               (cond
                  ((checkExpression expr operator attribute)
                     (replacementTextFunction expr)
                  )
                  (else
                     (map loop expr) 
                  )
               )
               expr
            )
         )
      )
      ;return
      newWhere
   )
)
; Adds an additional clause to an existing where clause using 'and.  If 
; the first where clause already starts with an and, it merges the clauses.
; If the existing where is null, returns the clause alone.
(define (where-and-clause where clause)
   (cond
      ((null? where)
         ;return:
         clause
      )
      ((eq? (car where) 'and)
         ;return:
         (append where (list clause))
      )
      (else
         ;return:
          (list 'and  where clause)
      )
   )
)

;Finds the value of a given expression within a where clause:
(define (where-find-value where operator attribute)
   (let loop ((expr where))
      (if (pair? expr)
         (cond
            ((checkExpression expr operator attribute)
               (if (>= (length expr) 3)
                  (list-ref expr 2)
                  ;else
                  #f
               )
            )
            (else
               (some loop expr)
            )
         )
         ;else
         #f
      )
   )
)

;Processes a where clause to convert any instances to OIDs to allow for serialization.
;@arg where list A scheme list describing a where clause
;@ret list A processed version of the where clause.
(define (where-process-instances where)
   (define newWhere ())
   (set! newWhere
      (let loop ((expr where))
         (if (pair? expr)
            (map loop expr) 
            ;else:
            (if (instance? expr Object)
               (expr':oid)
               ;else:
               expr
            )
         )
      )
   )
   ;return
   newWhere
)


  
; Sets the list context for a where clause. 
; Must be set within an "and" block in the where clause. 
;
; @arg context symbol Context symbol. 
; @ret boolean Always returns #t 
; @example
; (Entity'read '(fullName) '(and (= lastName "smith") (list-context? 'MY_BOOK)))
(define (list-context? context)
   (logger'dump "list context: " context)
   #t
)