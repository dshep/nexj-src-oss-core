(import 'nexj.core.persistence.OIDHolder)
;Helper methods for manipulating order by expressions:

;takes an expression/list in the form of '(@ x) or 'x or '(@ x y) and returns the value of 
;that attribute using obj as "this".
;
(define (eval-assoc obj expr)
   (cond
      ((null? expr)
         obj
      )
      ((symbol? expr)
         (if (eq? '@ expr) obj (obj expr))
      )
      ((eq? '@ (car expr))
         (eval-assoc obj (cdr expr))
      )
      (else
         (eval-assoc (obj (car expr)) (cdr expr))
      )
   )
)

; Constructs a comparison function based on object attribute values and the given order by clause.
; @arg orderBy list Order by expression, must be a valid, standard order by as used by persistence mapping. 
; @ret lambda The resulting comparison function, which compares based on the order by.  
; the attribute values taken from the compared objects.
; @example
; (sort! persons (sort-by '(((@ lastName) . #t) ((@ firstName) . #t) ((@) . #t))))
; @example
; => collection of persons sorted by last name, then first name, then OID
(define (sort-by orderBy)
   ;return:
   (lambda (a b)
      (let loop ((orderByExpr orderBy))
         (cond
            ((null? orderByExpr) #f)
            (else
               (unless (and (pair? (car orderByExpr)) (boolean? (cdr (car orderByExpr))))
                  (error "err.meta.invalidOrderBy" orderBy)
               )
               (let*
                  (
                     (ascending (cdr (car orderByExpr)))
                     (attrExpr (car (car orderByExpr)))
                     (left (eval-assoc a attrExpr))
                     (right (eval-assoc b attrExpr))
                     (less?
                        (if (or (instance? left nexj.core.persistence.OIDHolder) (instance? right nexj.core.persistence.OIDHolder))
                           (if ascending
                              (sort-attribute comparable< ':oid)
                              ;else:
                              (lambda (aObj bObj)
                                 (not ((sort-attribute comparable< ':oid) aObj bObj))
                              )
                           )
                           ; else:
                           (if ascending < >)
                        )
                     )
                  )
                  (cond
                     ((less? left right) #t)
                     ((= left right)
                        (loop (cdr orderByExpr))
                     )
                     (else #f)
                  )
               )
            )
         )
      )
   )
)

(define sort-order-by sort-by)

; Less function for comparing objects with a compareTo method.  
; Accepts nulls, will sort them before non-null objects.
; @arg a any left hand side of comparison
; @arg b any right hand side of comparison
; @ret boolean result of comparison.
; @example
; ;sort a collection of persons by oid (order by (@))
; (sort persons (sort-attribute comparable< ':oid))
(define (comparable< a b)
   (cond
      ((null? a) (not (null? b)))
      ((null? b) #f)
      (else (negative? (a'compareTo b)))
   )
)
