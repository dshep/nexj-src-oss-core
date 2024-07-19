; Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0

(define-class TypedTest1 (sys:TypedObject) "")

(define-class TypedExceptionTest1 (sys:TypedException)
   "A copy of TypedTest1, except that the base class is the root typed exception."
)

; Build parallel versions of the class
(for-each
   (lambda (clazz)
      (define-class-attribute clazz clsInt :type sys:integer)
      (define-attribute clazz instInt :type sys:integer)
      (define-attribute clazz instAny :type sys:any)
      (define-attribute clazz instObj :type sys:Object)
      (define-attribute clazz instIntArray :type sys:integer :collection #t)
      (define-method clazz instMeth1 (a b c) :arg-types (sys:integer sys:any sys:Object) ""
         ()
      )
      (define-method clazz instMeth2 (a) :arg-types (sys:integer) :arg-collections (#t))
      (define-method clazz instMethGoodReturn () :type sys:integer :collection #f ""
         42
      )
      (define-method clazz instMethBadReturn () :type sys:integer :collection #f ""
         "a string"
      )
      (define-class-method clazz clsMeth1 (a) :arg-types (sys:integer) ""
         ()
      )
      (define-method clazz instMethVarArg (x . args) :arg-types (sys:integer sys:string) ""
         (logger'debug "X:" x)
         (logger'debug "Args:" args)
         ()
      )
      (define-method clazz instMethVarArg2 args :arg-types (sys:string) ""
         (logger'debug "Args:" args)
         ()
      )
   )
   (list TypedTest1 TypedExceptionTest1)
)

(define-class InheritedTypesTest (TypedTest1) "")

(define-class InheritedExceptionTypesTest (TypedExceptionTest1)
   "A copy of InheritedTypesTest, exception that the base class is TypedExceptionTest1."
)

; Build parallel versions of the class
(for-each
   (lambda (clazz)
      (define-class-attribute clazz clsInt :derived #t :init 1337)
      (define-attribute clazz instInt :derived #t :init 1337)
      (define-method clazz instMeth1 (a b c) :derived #t ""
         "override"
      )
      (define-class-method clazz clsMeth1 (a) :derived #t ""
         "override"
      )
   )
   (list InheritedTypesTest InheritedExceptionTypesTest)
)



; If the following two classes fail to load, then forward attribute type definitions are not working
(define-class CircularAttribute1 (sys:TypedObject) ""
   (attribute a :type CircularAttribute2)
   (class-attribute x :type CircularAttribute2)
)

(define-class CircularAttribute2 (sys:TypedObject) ""
   (attribute b :type CircularAttribute1)
   (class-attribute y :type CircularAttribute1)
)

; If the following two classes fail to load, then forward method type definitions are not working
(define-class CircularMethod1 (sys:TypedObject) ""
   (method w (a) :arg-types (CircularMethod2) :type CircularMethod2)
   (class-method x (a) :arg-types (CircularMethod2) :type CircularMethod2)
)

(define-class CircularMethod2 (sys:TypedObject) ""
   (method y (a) :arg-types (CircularMethod1) :type CircularMethod1)
   (class-method z (a) :arg-types (CircularMethod1) :type CircularMethod1)
)

