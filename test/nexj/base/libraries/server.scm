(declare scope server)

(import 
   '( java.lang.Integer 
      java.net.URL
   )
)
(import 
   '( nexj.core.rpc.text.TextMarshaller
      nexj.core.rpc.text.TextUnmarshaller
      nexj.core.runtime.GenericSerializablePropertyMap
      nexj.core.runtime.InstanceLogger
   )
)

; Commits all changes in the current transaction. 
; Invokes (((invocation-context)'unitOfWork)'commit) 
; @see begin-transaction
(define (commit) (((invocation-context)'unitOfWork)'commit))

;Invokes ((invocation-context)'unitOfWork)'rollback)
(define (rollback) (((invocation-context)'unitOfWork)'rollback))

; Calls commit with the #f parameter to pre-commit the transaction.
(define (pre-commit)
   (((invocation-context)'unitOfWork)'commit #f)
)

; Helper function to find out if commit flag is set in the current request.
; @ret boolean #t if the commit flag is set.
(define (isCommit)
   (not (((invocation-context)'unitOfWork)'transient))
)   

; Returns the count of the instances matching given where clauses
; @arg class The class object
; @arg where The where clause
(define (instance-count class where)
   (define cursor (class'openCursor '() where '() '() '() '()))
   (try
      (do ((count 0 (+ count 1)))
         ((null? (cursor'next)) count)
      )
      ()
      (cursor'close)
   )
)

; Gets the start time of the current transaction from the invocation context's
; current unit of work.  Useful for auditing as it returns the same time
; for the entire transaction so the audit records are consistent.
; @ret timestamp The start time of the current transaction.
(define (transaction-time)
   (cast timestamp (((invocation-context)'unitOfWork)'time))
)

; Builds a list from 0 to (max - 1).  See partitioned? for an example.
; @arg max integer The top of the range (note, this is never reached)
; @ret list of integers
; @example
; (range 10) -> '(0 1 2 3 4 5 6 7 8 9)  
(define (range max)
   (define r '())
   (for ((i (- max 1)))
        (>= i 0)
        (set! i (- i 1))
      (set! r (cons i r))
   )
   ;return:
   r
)
; Checks if the instance has the PARTITIONED aspect.  Also accepts metaclasses
; @arg inst Metaclass/Instance The Metaclass/instance being checked
; @ret boolean True only if the class has the PARTITIONED aspect.
(define (partitioned? inst)
   (when (instance? inst Object);allow call with instance, not just metaclass:
      (set! inst (inst':class))
   )
   (and 
      (> (inst'aspectCount) 0) 
      (some 
         (lambda (i) 
            (= "PARTITIONED" ((inst'getAspect i)'name))
         ) 
         (range (inst'aspectCount))
      )
   )
)

; Determines if privilege is in privilege set.
; @arg privilegeSet any The privilege set.
; @arg privilege string The privilege in question.
; @ret boolean True if privilege is in privilege set.
(define (in-privilege-set? privilegeSet privilege)
   (privilegeSet'contains (((invocation-context)'metadata)'getPrimitivePrivilege privilege))
)

; Executes the given sql (update/delete/insert queries only) against the given data source.
; (For internal use only. Will be deprecated in the near future.)
; @arg ds string|DataSource The data source.
; @arg sql string The SQL statement to execute – DML or DDL.
; @arg bindParams any (Optional) Bind parameters to be bound to the query.
; @ret integer The number of rows affected by the query.
(define (sql-execute ds sql . bindParams)
   (define connection '())
   (define statement '())
   
   (try
      (let
         (
            (adapter (((if (string? ds) (((invocation-context)'metadata)'getDataSource ds) ds)'component)'getInstance (invocation-context)))
            (ordinal 0)
         )
         (set! connection (adapter'connection))
         (set! statement ((connection'connection)'prepareStatement sql))
         (logger'debug "sql-execute:" sql)

         (for-each
            (lambda (value)
               (logger'debug "Bind[" ordinal "] =" value)

               (if (null? value)
                  (begin
                     (set! ordinal (+ ordinal 1))
                     (statement'setObject ordinal '())
                  )
                  (begin
                     ((adapter'getBind (nexj.core.meta.Primitive'typeOf value))'setValue statement ordinal value adapter)
                     (set! ordinal (+ ordinal 1))
                  )
               )
            )
            bindParams
         )

         (logger'dump (((invocation-context)'machine)'logTrace 5))

         (statement'executeUpdate)
      )
      ()
      (statement'close)
      (connection'decRef)
   )
)

; Executes a given block of code under the given locale
(define-macro (with-locale locale . body)
   (define locale-saved (string->symbol "#locale"))
   `(##let ((,locale-saved ((##invocation-context)'locale)))
      ((##invocation-context)'locale ,locale)
      (##try (begin ,@body) () ((##invocation-context)'locale ,locale-saved))
    )
)

; Serializes the given object with the TextMarshaller
; @arg obj A marshallable object.  Can be a collection or list.
; @return string Serialized text representation of the given obj.
(define (text-serialize obj)
   (define stringWriter (open-output-string))
   (define textMarshaller (nexj.core.rpc.text.TextMarshaller'new (invocation-context)))
   (textMarshaller'serialize obj stringWriter)
   ;return:
   (stringWriter'toString)
)

; Deserializes the given text using the TextUnmarshaller.  If text is null, returns null.
; @arg test string Text serialized using the TextMarshaller
; @return any Deserialized objects.
(define (text-deserialize text)
   (cond
      ((null? text)
         ;return:
         ()
      )
      (else
         (let ((textUnmarshaller (nexj.core.rpc.text.TextUnmarshaller'new (invocation-context))))
            ;return:
            (textUnmarshaller'deserialize 
               (if (string? text)
                  (open-input-string text) 
                  ;else
                  text;assume a reader is passed in.
               )
            )
         )
      )
   )
)

; Recreates serialized data that was serialized using GenericSerializablePropertyMap
; @arg serializedData string/binary The serialized data
; @ret GenericSerializablePropertyMap The recreated property map
(define (recreate-serializable-property-map serializedData)
   (let ((valueMap (serializable-property-map (nexj.core.runtime.GenericSerializablePropertyMap'SKIP))))
      (unless (null? serializedData)
         (valueMap':deserialize  serializedData (invocation-context))
      )
      ;return:
      valueMap
   )
)

; Creates a new instance of GenericSerializablePropertyMap.  
; @arg args sequence Optional arguments passed on to the GenericSerializablePropertyMap constructor
; @ret GenericSerializablePropertyMap The new property map
(define (serializable-property-map . args)
   ;return:
   (apply nexj.core.runtime.GenericSerializablePropertyMap'new args)
)

; Creates a new URL.
; @arg s string The string representation of the URL to create.
; @ret any
(define (url s)
   (java.net.URL'new s)
)

; Determines if an object is a URL.
; @arg obj any The object to check.
; @ret boolean True if obj is an instance of java.net.URL; false otherwise.
(define (url? obj)
   (instance? obj java.net.URL)
)

; Get the current value of the most precise available system timer, in nanoseconds.
; @ret The current value of the most precise available system timer, in nanoseconds.
(define (clock)
   (java.lang.System'nanoTime)
)
