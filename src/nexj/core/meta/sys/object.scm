(declare scope all)

; Extracts options in the format :<name> <value> from
; a list until a non-option is encountered
(define (sys:get-options specs)
   (do ((opts '()))
      ((or
         (null? specs)
         (not (symbol? (car specs)))
         (not (equal? (substring (symbol->string (car specs)) 0 1) ":"))
       )
         (cons (reverse! opts) specs)
      )
      (if (null? (cdr specs))
         (begin
            (set! opts (cons (cons (car specs) '()) opts))
            (set! specs '())
         )
         (begin
            (set! opts (cons (cons (car specs) (cadr specs)) opts))
            (set! specs (cddr specs))
         )
      )
   )
)

; Special form defining a class.
;
; @arg name symbol The name of the class.
; @arg bases list List of base class symbols; null to use sys:Object.
; @arg specs list Additional options in the form :<name> <value>.
; @ret any The class that was defined.
(define-macro (define-class name bases . specs)
   (define class (string->symbol "#class"))
   (define options '())
   (define description "")
   (if (not (symbol? name))
      (error "err.scripting.className")
   )
   (if (or (not (list? bases)) (not (every symbol? bases)))
      (error "err.scripting.classBases")
   )
   (set! specs (sys:get-options specs))
   (set! options (car specs))
   (set! specs (cdr specs)) ; skip the options
   (unless (null? specs)
      (if (not (string? (car specs)))
         (error "err.scripting.classDesc")
      )
      (set! description (car specs))
      (set! specs (cdr specs))
   )
   `(##let ((,class (##sys:Metaclass':define ',name)))
      (,class':reset)
      (##for-each
         (lambda (base) ((##sys:Metaclass':define base)':add-derived ,class))
         ',(ifnull bases '(sys:Object))
      )
      (,class':add-options ',options)
      ,@(map
         (lambda (spec)
            (if (or (null? spec) (not (list? spec)) (not (symbol? (car spec))))
               (error "err.scripting.classMember")
            )
            (case (car spec)
               ((attribute)
                  (if (< (length spec) 2) (error "err.scripting.classAttribute"))
                  `(##define-attribute ,class ,@(cdr spec))
               )
               ((class-attribute)
                  (if (< (length spec) 2) (error "err.scripting.classAttribute"))
                  `(##define-class-attribute ,class ,@(cdr spec))
               )
               ((method)
                  (if (< (length spec) 3) (error "err.scripting.classMethod"))
                  `(##define-method ,class ,@(cdr spec))
               )
               ((class-method)
                  (if (< (length spec) 3) (error "err.scripting.classMethod"))
                  `(##define-class-method ,class ,@(cdr spec))
               )
               (else
                  (error "err.scripting.classMemberKey" (car spec))
               )
            )
         )
         specs
      )
      ,class
   )
)

; Special form defining an instance attribute
(define-macro (define-attribute class name . spec)
   (define options '())
   (if (not (symbol? name))
      (error "err.scripting.attributeName")
   )
   (set! spec (sys:get-options spec))
   (set! options (car spec))
   (set! spec (cdr spec)) ; skip options
   (if (and (not (null? spec)) (or (not (string? (car spec))) (not (null? (cdr spec)))))
      (error "err.scripting.attributeDesc")
   )
   `(,class':add-attribute ',name ',options ,(or (symbol? class) (pair? class)))
)

; Special form defining a class attribute
(define-macro (define-class-attribute class name . spec)
   `(##define-attribute (,class':class) ,name ,@spec)
)

; Special form defining an instance method
(define-macro (define-method class name args . spec)
   (define options '())
   (if (not (symbol? name))
      (error "err.scripting.methodName")
   )
   (set! spec (sys:get-options spec))
   (set! options (car spec))
   (set! spec (cdr spec)) ; skip options
   (unless (null? spec)
      (if (not (string? (car spec)))
         (error "err.scripting.methodDesc")
      )
      (set! spec (cdr spec)) ; skip description
   )
   `(,class':add-method ',name ',options '(lambda (this .,args) ,@(ifnull spec '(())))
      ,(or (symbol? class) (pair? class)))
)

; Special form defining a class method
(define-macro (define-class-method class name args . spec)
   `(##define-method (,class':class) ,name ,args ,@spec)
)

; Special form invoking a base class method
(define-macro (base method . args)
   (define class (get-value 'sys:current-class))
   (if (null? class)
      `(this ,method ,@args)
      `(this ,class ,method ,@args)
   )
)

; Macro for object instantiation.
; @arg class class The class object
; @arg specs any Attribute initial values: :attr1 <value1> ... :attrN <valueN>
; @ret Object The instantiated object
(define-macro (:: class . specs)
   (define this? (and (symbol? class) (equal? (substring (symbol->string class) 0 1) ":")))
   (if this? (set! specs (cons class specs)))
   (if (null? specs)
      `(,class'new)
      (let ((obj (if this? 'this (string->symbol "#obj"))) (opts '()))
         `(,@(if this? '(begin) `(let ((,obj (,class':new)))))
            ,@(map
               (lambda (spec)
                  (define name (symbol->string (car spec)))
                  (set! spec (cdr spec))
                  (if (equal? name ":")
                     (if (pair? spec)
                        `(,obj',(car spec) ,@(cdr spec))
                        `(,obj',spec)
                     )
                     `(,obj',(string->symbol (substring name 1 (string-length name))) ,spec)
                  )
               )
               (do ((opts '()))
                  ((null? specs) (reverse! opts))
                  (if
                     (or
                        (not (symbol? (car specs)))
                        (not (equal? (substring (symbol->string (car specs)) 0 1) ":"))
                        (not (pair? (cdr specs)))
                     )
                     (error "Invalid initializer specification {0}" (car specs))
                  )
                  (set! opts (cons (cons (car specs) (cadr specs)) opts))
                  (set! specs (cddr specs))
               )
            )
            (,obj'initialize)
            ,obj
         )
      )
   )
)


; Bootstrap the object system
(for-each
   (lambda (class)
      (import class)
      ((get-value class)'init)
   )
   '(nexj.core.scripting.object.BasicMetaclassObject
     nexj.core.scripting.object.ExceptionMetaclassObject
     nexj.core.scripting.object.TypedMetaclassObject
     nexj.core.scripting.object.TypedExceptionMetaclassObject)
)

(define-method sys:Attribute :init (expr) "Sets the initializer expression"
   (this':initializer `(lambda (this) ,expr))
)

(define-method sys:Attribute :set (expr) "Sets the set interceptor expression"
   (this':setter `(lambda (this value) ,expr value))
)

(define-class-method sys:Object :for-each-base (fun) "Iterates over direct base classes"
   (do ((i 0 (+ i 1))) ((= i (@ :base-count))) (fun (@ :base : i)))
)

(define-class-method sys:Object :for-each-derived (fun) "Iterates over direct derived classes"
   (do ((i 0 (+ i 1))) ((= i (@ :derived-count))) (fun (@ :derived : i)))
)

(define-class-method sys:Object :for-each-method (fun) "Iterates over direct methods"
   (do ((i 0 (+ i 1))) ((= i (@ :method-count))) (fun (@ :method : i)))
)

(define-class-method sys:Object :for-each-class-method (fun) "Iterates over direct class methods"
   (do ((i 0 (+ i 1))) ((= i (@ :class-method-count))) (fun (@ :class-method : i)))
)

(define-class-method sys:Object :for-each-attribute (fun) "Iterates over direct attributes"
   (do ((i 0 (+ i 1))) ((= i (@ :attribute-count))) (fun (@ :attribute : i)))
)

(define-class-method sys:Object :for-each-class-attribute (fun) "Iterates over direct class attributes"
   (do ((i 0 (+ i 1))) ((= i (@ :class-attribute-count))) (fun (@ :class-attribute : i)))
)
