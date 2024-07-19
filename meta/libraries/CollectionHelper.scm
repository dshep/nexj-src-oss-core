(declare scope server)

; Gets an item out of a collection that meets the comparision conditions specified in the function body fnBody
; @arg col collection/sequence the collection
; @arg fnBody expr the expression to select the correct item, unquoted and uses "this" to refer to the current instance
; @ret any The first item matching the expression specified in fnBody, or #f if none of the items match.
; @example
;(define testModel 
;   (AssignmentModel'new
;      (: name "TestModel")
;      (: description "TestModel description")
;      (: assignmentRuleset "AsgnMgrTest")
;      (: validRoles 
;         (instance-collection
;            (AssignmentModelRole'new
;               (: role (AssignmentRoleEnum'get'CREATOR)))
;            (AssignmentModelRole'new
;               (: role (AssignmentRoleEnum'get'ANAYLST_IND)))
;            (AssignmentModelRole'new
;               (: role (AssignmentRoleEnum'get'ANAYLST_SEC)))
;          )
;       )
;   )
;)
;(collection-find-item (testModel'validRoles) (= (@ role) (AssignmentRoleEnum'Creator))) 
(define-macro (collection-find-item col fnBody)
   (when (and (pair? fnBody) (or (eq? (car fnBody) 'quote) (eq? (car fnBody) 'quasiquote)))
      (error "fail.incorrectFunctionBodyDefinition")
   )
   `(##some
      (lambda (this)
         (if ,fnBody
            this
            #f
         )
      )
      ,col
   )
)

; Filters a collection according to the given expression.
; @arg col Collection The collection to be filtered.   
; @arg fnBody any Expression that filters the collection.  Note that @ is
; redefined to be relative to the instance in the collection, so references 
; to @ in the fnBody expression are not relative to the current class.
; @example
; ;find all home addresses for the current Entity instance:
; (collection-find-items (@ telcoms) (= (@ type name) "Home"))
(define-macro (collection-find-items col fnBody)
   (when (and (pair? fnBody) (or (eq? (car fnBody) 'quote) (eq? (car fnBody) 'quasiquote)))
      (error "fail.incorrectFunctionBodyDefinition")
   )
   `(##filter
      (lambda (this) ,fnBody)
      ,col
   )
)
