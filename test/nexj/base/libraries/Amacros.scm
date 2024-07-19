(declare scope server)

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

; Special form evaluating specified expressions logged in as the given user.
; @arg body any The expressions to evaluate.
; @ret any The value of the last expression.
; @example
; (begin-switch-user usr
;    (fun1)
;    (+ 1 2)
; ) => 3
(define-macro (begin-switch-user usr . body)
   (define oldUser (string->symbol "#oldUser"))
   `(let ((,oldUser ((global user))))
      (((global invocation-context))'login (User'getUser ,usr))
      (try (begin ,@body) () 
         (unless (null? ,oldUser)
            (((global invocation-context))'login ,oldUser)
         )
      )
   )
)