; Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
(import 'nexj.core.util.RandUtil)


(define (datastore-ts-add-years ts years)
   (cast timestamp
      (+
         (cast long ts)
         (* 1000 60 60 24 365.25 years)
      )
   )
)

(define (datastore-make-ref store-symbol oid)
   (cons (delay (eval store-symbol)) oid)
)

(define (datastore-deref item)
   (unless (null? item)
      (((force (car item))'lookupByOID) (cdr item))
   )
)

(define AnimalStore
   (message
      (: data
         (collection
            (cons
               (oid #z01)
               (message
                  (: commonName "Dog")
                  (: limbCount 4)
                  (: locking 0)
                  (: pen (oid "North" 8))
                  (: :class "DomesticAnimal")
                  (: classCode "DOM")
                  (: petName "Kerberos")
                  (: licenses
                     (collection
                        (datastore-make-ref 'DomesticAnimalLicenseStore (oid #z80))
                        (datastore-make-ref 'DomesticAnimalLicenseStore (oid #z81))
                     )
                  )
                  (: chaserOf (oid #z02))
                  (: fondOf (datastore-make-ref 'AnimalStore (oid #z04)))
                  (: virtFKToRelColl (oid #z00000000000000000000000000008802 "main"))
                  (: virtFKToRelNone (oid #z00000000000000000000000000008802 "main"))
                  (: petFriend (oid "Cleopatra" "Cat"))
               )
            )
            (cons
               (oid #z02)
               (message
                  (: commonName "Cat")
                  (: scientificName "Felis catus")
                  (: limbCount 4)
                  (: locking 0)
                  (: pen (oid "West" 5))
                  (: :class "DomesticAnimal")
                  (: classCode "DOM")
                  (: petName "Cleopatra")
                  (: fondOf (datastore-make-ref 'AnimalStore (oid #z03)))
                  (: virtFKToRelColl (oid #z00000000000000000000000000008802 "main"))
                  (: virtFKToRelNone (oid #z00000000000000000000000000008801 "main"))
               )
            )
            (cons
               (oid #z03)
               (message
                  (: commonName "Spider")
                  (: limbCount 8)
                  (: locking 0)
                  (: pen (oid "North" 8))
                  (: :class "Animal")
                  (: classCode "ANM")
                  (: virtFKToRelAttr (oid #z00000000000000000000000000008801 "main"))
                  (: primitiveFK #z01)
               )
            )
            (cons
               (oid #z04)
               (message
                  (: commonName "Beaver")
                  (: scientificName "Castor canadensis")
                  (: limbCount 4)
                  (: locking 0)
                  (: fondOf (datastore-make-ref 'AnimalStore (oid #z03)))
                  (: :class "Animal")
                  (: classCode "ANM")
                  (: virtFKToRelNone (oid #z00000000000000000000000000008801 "main"))
                  (: virtFKToRelAttr (oid #z00000000000000000000000000008802 "main"))
               )
            )
         )
      )
      (: className "Animal")
      (: attrs '(commonName limbCount scientificName pen locking petName licenses chaserOf fondOf virtFKToRelColl
         virtFKToRelNone virtFKToRelAttr primitiveFK petFriend classCode)
      )
      (: compAttrs '(licenses fondOf))
      (: locking 'locking)
   )
)

(define AnimalPenStore
   (message
      (: data
         (collection
            (cons
               (oid "North" 8)
               (message
                  (: name "Dog pen")
                  (: locking 0)
               )
            )
            (cons
               (oid "West" 5)
               (message
                  (: name "Cat pen")
                  (: locking 0)
               )
            )
         )
      )
      (: className "AnimalPen")
      (: attrs '(name locking))
      (: locking 'locking)
   )
)


(define DomesticAnimalLicenseStore
   (message
      (: data
         (collection
            (cons
               (oid #z80)
               (message
                  (: name "City Dog License")
                  (: issued (datastore-ts-add-years (now) -5))
                  (: expires (datastore-ts-add-years (now) 5))
               )
            )
            (cons
               (oid #z81)
               (message
                  (: name "Vaccination Certificate for Kerberos the dog")
                  (: issued (datastore-ts-add-years (now) -5))
                  (: expires (datastore-ts-add-years (now) 5))
               )
            )
         )
      )
      (: className "DomesticAnimalLicense")
      (: attrs '(name issued expires))
   )
)



(define (addProcedures store)
   (store'makeMessageFromInstance
      (lambda (inst)
         (unless (null? inst)
            (apply
               message
               (map
                  (lambda (attr)
                     (cons attr (inst attr))
                  )
                  (filter
                     (lambda (attr)
                        (not (null? ((((invocation-context)'metadata)'getMetaclass (inst':class))'findAttribute attr) ))
                     )
                     (store'attrs)
                  )
               )
            )
         )
      )
   )
   (store'makeMessageFromDatum
      (lambda (result)
         (unless (null? result)
            (let
               (
                  (data (cdr result))
               )
               (apply
                  message
                  (: :oid (car result))
                  (map
                     (lambda (attr)
                        (if (in? attr (store'compAttrs))
                           (if (collection? (data attr))
                              (cons attr (map datastore-deref (data attr)))
                              ; else:
                              (cons attr (datastore-deref (data attr)))
                           )
                           ; else:
                           (cons attr (data attr))
                        )
                     )
                     (cons ':class
                        (filter
                           (lambda (attr)
                              (data':contains attr)
                           )
                           (store'attrs)
                        )
                     )
                  )
               )
            )
         )
      )
   )
   (store'makeResults
      (lambda (result)
         (when (collection? result)
            (map
               (store'makeMessageFromDatum)
               result
            )
         )
      )
   )
   (store'create
      (lambda (inst)
         (let*
            (
               (key (inst':oid))
               (data (store'data))
               (result
                  (let loop
                     (
                        (i 0)
                        (size (vector-length data))
                     )
                     (if (< i size)
                        (if (= (car (vector-ref data i)) key)
                           (vector-ref data i)
                           (loop (+ i 1) size)
                        )
                        ()
                     )
                  )
               )
            )
            (unless (null? result) (error (string-append "Duplicate key: " (key'toString))))
            (data'add
               (cons
                  key
                  ((store'makeMessageFromInstance) inst)
               )
            )
            ()
         )
      )
   )
   (store'updateByOID
      ; @arg key any Description for key
      ; @arg tobj message Description for tobj
      ; @ret any 
      (lambda (key tobj)
         (let*
            (
               (data (store'data))
               (hasLocking (store':contains 'locking))
               (lockingAttr (when hasLocking (store'locking)))
               (oldLocking (when hasLocking (tobj lockingAttr)))
               (result
                  (let loop
                     (
                        (i 0)
                        (size (vector-length data))
                     )
                     (if (< i size)
                        (if (= (car (vector-ref data i)) key)
                           (vector-ref data i)
                           (loop (+ i 1) size)
                        )
                        ()
                     )
                  )
               )
               (foundTobj (unless (null? result) (cdr result)))
            )
            (if (null? result)
               #f ; Not found, so it is an optimistic locking error (it was deleted)
               ; else:
               (if
                  (=
                     oldLocking
                     (when hasLocking
                        (foundTobj lockingAttr)
                     )
                  )
                  (begin
                     (for-each
                        (lambda (key)
                           (foundTobj key (tobj key))
                        )
                        (tobj':iterator)
                     )
                     (when hasLocking
                        (let
                           (
                              (newLocking ((nexj.core.util.RandUtil'secureRandom)'nextInt) )
                           )
                           (foundTobj lockingAttr newLocking)
                           (tobj lockingAttr newLocking)
                        )
                     )
                  )
                  ; else: Locking attribute mismatch, optimistic locking error
                  #f
               )
            )
         )
      )
   )
   (store'changeOID
      ; @arg oldOID oid The current OID of the object.
      ; @arg newOID oid The new OID for the object.
      ; @arg tobj message The transfer object for the locking attribute.
      (lambda (key newOID tobj)
         (let*
            (
               (data (store'data))
               (hasLocking (store':contains 'locking))
               (lockingAttr (when hasLocking (store'locking)))
               (oldLocking (when hasLocking (tobj lockingAttr)))
               (result
                  (let loop
                     (
                        (i 0)
                        (size (vector-length data))
                     )
                     (if (< i size)
                        (if (= (car (vector-ref data i)) key)
                           (vector-ref data i)
                           (loop (+ i 1) size)
                        )
                        ()
                     )
                  )
               )
               (foundTobj (unless (null? result) (cdr result)))
            )
            (if (null? result)
               #f ; Not found, so it is an optimistic locking error (it was deleted)
               ; else:
               (if
                  (=
                     oldLocking
                     (when hasLocking
                        (foundTobj lockingAttr)
                     )
                  )
                  (begin
                     (set-car! result newOID)
                     (when hasLocking
                        (let
                           (
                              (newLocking ((nexj.core.util.RandUtil'secureRandom)'nextInt) )
                           )
                           (foundTobj lockingAttr newLocking)
                           (tobj lockingAttr newLocking)
                        )
                     )
                  )
                  ; else: Locking attribute mismatch, optimistic locking error
                  #f
               )
            )
         )
      )
   )
   (store'deleteByOID
      (lambda (key . args)
         (let*
            (
               (data (store'data))
               (hasLocking (store':contains 'locking))
               (lockingAttr (when hasLocking (store'locking)))
               (oldLocking (when hasLocking (car args)))
            )
            (logger'info "DELETE_BY_OID: args" args)
            (let loop
               (
                  (i 0)
                  (size (vector-length data))
               )
               (if (< i size)
                  (if (= (car (vector-ref data i)) key)
                     (if
                        (or
                           (not hasLocking)
                           (equal? ((cdr (vector-ref data i)) lockingAttr) oldLocking)
                        )
                        ; then:
                        (begin
                           (data'remove i)
                           #t
                        )
                        ; else:
                        #f ; Optimistic locking error
                     )
                     ; else:
                     (loop (+ i 1) size)
                  )
                  (error "Not found!")
               )
            )
         )
      )
   )
   (store'lookupByOID
      (lambda (key)
         (let*
            (
               (data (store'data))
               (result
                  (let loop
                     (
                        (i 0)
                        (size (vector-length data))
                     )
                     (if (< i size)
                        (if (= (car (vector-ref data i)) key)
                           (vector-ref data i)
                           (loop (+ i 1) size)
                        )
                        ()
                     )
                  )
               )
            )
            (when (null? result) "Not found!")
            ((store'makeMessageFromDatum) result)
         )
      )
   )
   (store'lookup1ByAttribute
      (lambda (attribute key)
         (let*
            (
               (data (store'data))
               (result
                  (let loop
                     (
                        (i 0)
                        (size (vector-length data))
                     )
                     (if (< i size)
                        (if (= ((cdr (vector-ref data i)) attribute) key)
                           (vector-ref data i)
                           (loop (+ i 1) size)
                        )
                        ()
                     )
                  )
               )
            )
            (when (null? result) "Not found!")
            ((store'makeMessageFromDatum) result)
         )
      )
   )
   (store'lookupManyByAttribute
      (lambda (attribute key)
         (let*
            (
               (data (store'data))
               (result
                  (let loop
                     (
                        (i 0)
                        (size (vector-length data))
                        (result (collection))
                     )
                     (if (< i size)
                        (begin
                           (when (= ((cdr (vector-ref data i)) attribute) key)
                              (result'add (vector-ref data i))
                           )
                           (loop (+ i 1) size result)
                        )
                        result
                     )
                  )
               )
            )
            (when (or (null? result) (zero? (vector-length result))) "Not found!")
            ((store'makeResults) result)
         )
      )
   )
   (store'getAll
      (lambda ()
         ((store'makeResults) (store'data))
      )
   )
)

(addProcedures AnimalStore)
(addProcedures AnimalPenStore)
(addProcedures DomesticAnimalLicenseStore)

