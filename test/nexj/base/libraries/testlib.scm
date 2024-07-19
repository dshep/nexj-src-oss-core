; Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
(define (testlib-fun) #t)

(define (rollup entity parents reverseAssoc)
   (define collection (instance-collection))
   (let loop ((child entity))
      (let ((currentChild (child reverseAssoc)))
         (if (not (null? currentChild))
            (begin
               (collection'add currentChild)
               (loop currentChild)
            )
         )
      )
   )
   (entity parents collection)
)

(declare scope client)

(define (one)
   1
)

(define (two)
   2
)

(define (one-plus arg)
   (+ (one) arg)
)

(declare scope server)
(define commit-count 0)
(define update-count 0)

; pre-commit - calls commit with the #f parameter to pre-commit the transaction.
(define (pre-commit)
   (((invocation-context)'unitOfWork)'commit #f)
)
