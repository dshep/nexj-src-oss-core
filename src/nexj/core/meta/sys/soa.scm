; Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0

(import
   'nexj.core.rpc.Response
   'nexj.core.rpc.RPCUtil
   'nexj.core.util.NetUtil
   'nexj.core.util.RandUtil
   'java.lang.Throwable
   'java.security.SecureRandom
)


(define soa:local-registry (make-hashtable))



; ==================== SOA Support: Implementation Classes ====================

(define-class soa:TypeObject (sys:TypedObject)
"The root of the hierarchy of information model classes.
All information model classes must (transitively) derive from this."
   (class-attribute :marshalled? :type sys:boolean :init #t)
)


(define-class soa:InterfaceObject (sys:TypedObject)
"The root of the hierarchy of SOA interface classes.
All interface classes must derive from this."
   (class-attribute :stateful-method-map :type sys:any "")
   (class-attribute :method-fault-map :type sys:any "")
)


; The metaclass of the interface objects.
(define soa:InterfaceObject:Metaclass (soa:InterfaceObject':class))


(define-class soa:ServiceObject (sys:TypedObject)
"The root of the hierarchy of SOA service classes.
All service classes must derive from this."
   (class-attribute interfaces :type soa:InterfaceObject:Metaclass :collection #t)
   (class-attribute implementations :type soa:InterfaceObject:Metaclass :collection #t)
)

; The metaclass of the service objects.
(define soa:ServiceObject:Metaclass (soa:ServiceObject':class))


(define-class soa:ImplementationObject (sys:TypedObject)
"The root of the hierarchy of implementation classes.
All implementation classes must derive from this."
   (class-attribute :service :type soa:ServiceObject:Metaclass) 
   (class-method :cast (iface . properties) :arg-types (soa:InterfaceObject:Metaclass sys:any)
"Returns a local proxy to another interface on this service. Since the proxy is local, the other interface methods
will be invoked in the current invocation context."
      (define propertyTab (make-hashtable))
      (hashtable-set! propertyTab 'binding 'local)
      (hashtable-set! propertyTab 'current-interface iface)
      (soa:add-properties propertyTab properties)
      (soa:create-proxy-internal (this':service) propertyTab)
   )
)


(define-class soa:Fault (sys:TypedException) "The root SOA fault object."
   (attribute time :init (now) :type sys:timestamp "The time the fault was created.")
   (attribute id :init (soa:Fault'generate-id) :type sys:string "Unique fault id")
   (attribute seq :init 0 :type sys:integer "Sequence number. Arranges faults in order of causation.")

   (class-attribute :marshalled? :type sys:boolean :init #t)

   (class-method new (code . args)
"Creates a new Fault from an error code and error arguments.
If the last argument is a Throwable, then it is the cause. If the cause is a Fault, then the Fault's id is set to
the id of the cause."
      (define obj (apply this (sys:TypedException':class) 'new code args))
      (define cause (obj'cause))
      (when (instance? cause soa:Fault)
         (obj'id (cause'id))
         (obj'seq (+ 1 (cause'seq)))
      )
      obj
   )
   (class-method generate-id () :type sys:string "Generate a unique fault id."
      (number->string (abs ((nexj.core.util.RandUtil'secureRandom)'nextLong)))
   )
)


(define-class soa:SystemFault (soa:Fault)
"Fault object that wraps underlying system exceptions.
Any exception that is not in a method's list of faults will be logged under a reference id and this exception
will be thrown into the consumer."
   (class-method new () ""
      (this'new ())
   )
   (class-method new (cause)
"Creates a new SystemFault.
If the cause is another soa:Fault object, then the reference id is copied from it and the sequence number is
incremented."
      (if (instance? cause soa:Fault)
         (let
            (
               (obj (base'new "err.rpc.reference" (cause'id)))
            )
            (obj'id (cause'id))
            (obj'seq (+ 1 (cause'seq)))
            obj
         )
         (let*
            (
               (id (soa:Fault'generate-id))
               (obj (base'new "err.rpc.reference" id))
            )
            (obj'id id)
            obj
         )
      )
   )
)



; ==================== SOA Support: Runtime utility functions ====================

; Adds the (: name value) property pairs from the properties list to the given hashtable.
;
; @arg propertyTab any The property hash table.
; @arg properties list The properties to add.
; @ret any The hash table.
(define (soa:add-properties propertyTab properties)
   (for-each
      (lambda (property)
         (hashtable-set! propertyTab (car property) (cdr property))
      )
      properties
   )
   propertyTab
)


; Gets the implementation of a given interface of the given service.
;
; @arg service any The service name (string or symbol).
; @arg interface any The interface name (string or symbol).
; @ret any The implementation class.
(define (soa:get-implementation-class service interface)
   (define found-implementation ())
   (define service-class (get-value service))
   (define interface-class (get-value interface))
   (unless (service-class':is-a? soa:ServiceObject)
      (error "fail.soa.undefinedService" service)
   )
   (for-each
      (lambda (interface-implementation)
         (when (interface-implementation':is-a? interface-class)
            (set! found-implementation interface-implementation)
         )
      )
      (service-class'implementations)
   )
   (if (null? found-implementation)
      (error "fail.soa.implementationLookup" interface service)
      found-implementation
   )
)


; Gets the service information from its SOAConnection.
;
; @arg service any The service class or the string name of the service.
; @arg instanceName string The name of the service instance, if any.
; @ret nexj:soa:Registry:1.0:type:ServiceInstance The service information (including connection parameters); null
; if not found.
(define (soa:find-service-in-connections service instanceName)
   (unless (or (string? instanceName) (null? instanceName))
      (error "fail.soa.invalidInstance" instanceName)
   )
   (let
      (
         (instanceMap
            (hashtable-ref soa:local-registry
               (cond
                  (
                     (and
                        (instance? service sys:Object)
                        (eq? (service':base 0) soa:ServiceObject)
                     )
                     (service':name)
                  )
                  ((string? service) service)
                  (else (error "fail.soa.undefinedService" service))
               )
            )
         )
      )
      (unless (null? instanceMap)
         (hashtable-ref instanceMap instanceName)
      )
   )
)

; Gets a proxy to the service registry. Uses the registry's SOAConnection for the connection parameters.
;
; @ret nexj:soa:Registry:1.0 The proxy.
(define (soa:get-registry-proxy)
   (let*
      (
         (regSrv nexj:soa:Registry:1.0)
         (info (soa:find-service-in-connections regSrv ()))
      )
      (if (null? info)
         (error "fail.soa.registry" regSrv)
         ; else:
         (apply soa:create-proxy regSrv
            (: binding (info'binding)) (: address (info'address)) (: credential (info'credential))
            (map
               (lambda (property)
                  (cons
                     (string->symbol (property'name))
                     (property'value)
                  )
               )
               (info'properties)
            )
         )
      )
   )
)

; Gets the service information from its entry in the registry.
;
; @arg service-class any Description for service-class
; @arg instanceName string The name of the service instance, if any.
; @ret nexj:soa:Registry:1.0:type:ServiceInstance The service information (including connection parameters). Throws
; an error if not found.
(define (soa:get-service-from-registry service-class instanceName)
   (unless (or (string? instanceName) (null? instanceName))
      (error "fail.soa.invalidInstance" instanceName)
   )
   (let
      (
         (info
            (if (string-empty? instanceName)
               ((soa:get-registry-proxy)'getService (service-class':name))
               ((soa:get-registry-proxy)'getService (service-class':name) instanceName)
            )
         )
      )
      (when (null? info)
         (if (string-empty? instanceName)
            (error "fail.soa.serviceLookup" (service-class':name))
            (error "fail.soa.serviceInstanceLookup" instanceName (service-class':name))
         )
      )
      info
   )
)

; Merges the connection parameters in localInfo with those in registryInfoPromise. Properties are always taken
; from localInfo, never from the registry.
;
; @arg localInfo nexj:soa:Registry:1.0:type:ServiceInstance The connection parameters from the .connections file.
; @arg registryInfoPromise promise The connection parameters from the registry. A promise to get the
; actual ServiceInstance.
; @ret nexj:soa:Registry:1.0:type:ServiceInstance The merged connection parameters.
(define (soa:merge-service-info localInfo registryInfoPromise)
   (define (get-attr attr)
      (if (string-empty? (localInfo attr)) ((force registryInfoPromise) attr) (localInfo attr))
   )
   (define mergedInfo
      (fold-left
         (lambda (info attr)
            (info attr (get-attr attr))
            info
         )
         (nexj:soa:Registry:1.0:type:ServiceInstance'new)
         '(service binding address credential)
      )
   )
   (mergedInfo'properties (localInfo'properties))
   mergedInfo
)

; Gets the connection parameters for connecting to a service.
;
; @arg service-class any The service to connect.
; @arg instanceName string The name of the service instance, if any.
; @ret nexj:soa:Registry:1.0:type:ServiceInstance The connection parameters.
(define (soa:get-service-info service-class instanceName)
   (define localInfo (soa:find-service-in-connections service-class instanceName))
   (if (equal? (localInfo'binding) "local")
      localInfo
      ; else:
      (let*
         (
            (registry-promise (delay (soa:get-service-from-registry service-class instanceName)))
            (serviceInfo (soa:merge-service-info localInfo registry-promise))
         )
         (assert (string=? (service-class':name) (serviceInfo'service)))
         serviceInfo
      )
   )
)

; Gets the perimeter authentication token for the current user.
;
; @ret string The authentication token. 
(define (soa:get-auth-token)
   (let
      (
         (authProxy (soa:create-proxy nexj:soa:Authentication:1.0))
      )
      (authProxy'generateToken ((user)'loginName))
   )
)

; Creates a hashtable from a list, with keys in the even positions and values in the odd positions.
;
; @arg items list List of keys and values. Even positions keys, odd positions are values.
; @ret hashtable A map of the method keys to state types.
(define (soa:list->hashtable items)
   (define htab (make-hashtable))
   (let next
      (
         (items items)
      )
      (if (null? items)
         htab
         (begin
            (hashtable-set! htab (car items) (cadr items))
            (next (cddr items))
         )
      )
   )
)

; Makes a same-context SOA request to the local machine.
;
; @arg propertyTab hashtable Generic property list for additional connection properties.
; @arg service soa:ServiceObject The service to invoke.
; @arg interface any The interface of the service to invoke.
; @arg state any The current state, if any.
; @arg operation symbol The operation to invoke.
; @arg args list The arguments to the operation.
; @ret any The result of the operation. If the operation is stateful, then a (<result> . <new state>) pair is
; returned.
(define (soa:invoke-local propertyTab service interface state operation args)
   (try
      (SOAGenericRequestHandler'respond (service':symbol) (interface':symbol) state operation args)
      (lambda (ex)
         (when (instance? ex soa:Fault)
            (ex'system #t)
         )
         (throw ex)
      )
   )
)

; Makes an SOA request using TRPC to another machine.
;
; @arg propertyTab hashtable Generic property list for additional connection properties.
; @arg service soa:ServiceObject The service to invoke.
; @arg interface any The interface of the service to invoke.
; @arg state any The current state, if any.
; @arg operation symbol The operation to invoke.
; @arg args list The arguments to the operation.
; @ret any The result of the operation. If the operation is stateful, then a (<result> . <new state>) pair is
; returned.
(define (soa:invoke-trpc propertyTab service interface state operation args)
   (let
      (
         (address (hashtable-ref propertyTab 'address))
         (credential (hashtable-ref propertyTab 'credential))
      )
      (unless (string? address)
         (error "fail.soa.invalidAddress" address)
      )
      (let*
         (
            (req (nexj.core.rpc.Request'new))
            (request
               (cond
                  ((instance? credential nexj:soa:Registry:1.0:type:BasicCredential)
                     (logger'debug "Using basic authentication")
                     (message
                        (: :class "HTTP")
                        (: url address)
                        (: channel "SOAGenericRequestChannel")
                        (: principal (credential'login))
                        (: password (credential'password))
                     )
                  )
                  ((instance? credential nexj:soa:Registry:1.0:type:PerimeterCredential)
                     (logger'debug "Using perimeter authentication")
                     (message
                        (: :class "HTTP")
                        (: url address)
                        (: channel "SOAGenericRequestChannel")
                        (: headers
                           (message
                              (: X-NexJ-SSO (soa:get-auth-token))
                              (: authorization ())
                           )
                        )
                     )
                  )
                  (else
                     (error "fail.soa.invalidCredential" credential)
                  )
               )
            )
         )
         (req'commit #t)
         (req'addInvocation "SOAGenericRequestHandler" "respond" (vector (service':symbol) (interface':symbol) state operation args) ())
         (request'body (text-serialize req))
         (let*
            (
               (rawResponse
                  (integration-send-receive request (request'channel) ())
               )
               (response (text-deserialize (rawResponse'body)))
            )
            (if (instance? response java.lang.Throwable)
               (begin
                  (when (instance? response soa:Fault)
                     (response'system #t)
                  )
                  (throw response)
               )
               (response'getResult 0)
            )
         )
      )
   )
)

; Creates a proxy to a service.
;
; @arg service-class any The service.
; @arg properties list Generic properties 
; @ret any The proxy.
(define (soa:create-proxy service-class . properties)
   (define propertyTab (make-eq-hashtable))
   (define instanceProp (assq 'instance properties))
   (define instanceName (if instanceProp (cdr instanceProp)))
   (define serviceInfo
      (if (or (string? instanceName) (null? instanceName))
         (soa:get-service-info service-class instanceName)
         (error "fail.soa.invalidInstance" instanceName)
      )
   )
   ; Build table from non-generic properties
   (for-each
      (lambda (key)
         (unless (null? (serviceInfo key))
            (hashtable-set! propertyTab key (serviceInfo key))
         )
      )
      '(binding address credential)
   )
   ; Add the generic properties
   (for-each
      (lambda (property)
         (hashtable-set! propertyTab (property'name) (property'value))
      )
      (serviceInfo'properties)
   )
   ; Add the arguments to create-proxy, overiding the settings from the registry
   (soa:add-properties propertyTab properties)
   ; Return the proxy
   (soa:create-proxy-internal service-class propertyTab)
)

; Creates a proxy to a service.
;
; @arg service-class any The service.
; @arg propertyTab hashtable Generic property list for additional properties to the proxy. Each property is a
; (: name value) pair.
; @ret any The proxy.
(define (soa:create-proxy-internal service-class propertyTab)
   (let*
      (
         (binding (hashtable-ref propertyTab 'binding))
         (invokerName
            (string-append "soa:invoke-"
               (cond
                  ((string? binding) binding)
                  ((symbol? binding) (symbol->string binding))
                  (else (error "fail.soa.invalidBinding" binding))
               )
            )
         )
         (invoker (get-value invokerName))
         (current-interface (hashtable-ref propertyTab 'current-interface (vector-ref (service-class'interfaces) 0)))
         (methodStateTab (make-hashtable (current-interface':class-method-count)))
         (explicit-state? (hashtable-ref propertyTab 'explicit-state #f))
      )
      (when (null? invoker)
         (error "fail.soa.unsupportedBinding" binding)
      )
      (lambda (operation . args)
         (logger'debug "Calling" operation args "on" (service-class':name))
         (if (eq? operation ':cast)
            (let
               (
                  (interface (car args))
                  (interface-classes (service-class'interfaces))
                  (propertyTabClone (hashtable-copy propertyTab #t))
                  (properties (cdr args))
               )
               (unless (in? interface interface-classes)
                  (error "fail.soa.interfaceLookup" interface (service-class':name))
               )
               (hashtable-set! propertyTabClone 'current-interface interface)
               (soa:add-properties propertyTabClone properties)
               (soa:create-proxy-internal service-class propertyTabClone)
            )
            ; else:
            (let*
               (
                  (stateArg (if explicit-state? (if (null? args) (error "fail.soa.missingState") (car args))))
                  (nonStateArgs (if explicit-state? (cdr args) args))
                  (argCount (length nonStateArgs))
                  (interface-method (current-interface':find-class-method operation argCount))
               )
               (when (null? interface-method)
                  (error "fail.soa.methodLookup" operation argCount (service-class':name))
               )
               (interface-method':validate-args "proxy" nonStateArgs)
               (let*
                  (
                     (stateKey (cons operation argCount))
                     (stateType (hashtable-ref (current-interface':stateful-method-map) stateKey))
                     (stateful? (not (null? stateType)))
                     (callingState
                        (if stateful?
                           (if explicit-state?
                              (begin
                                 (unless (or (null? stateArg) (instance? stateArg stateType))
                                    (error "fail.soa.stateTypeMismatch" stateType (nexj.core.meta.GenericType'typeOf stateArg))
                                 )
                                 stateArg
                              )
                              (hashtable-ref methodStateTab stateKey)
                           )
                        )
                     )
                     (response (invoker propertyTab service-class current-interface callingState operation nonStateArgs))
                     (result
                        (unless (null? (interface-method':type)) ; suppress returned value from void methods
                           (if stateful? (car response) response)
                        )
                     )
                     (returnedState (if stateful? (cdr response)))
                  )
                  (when stateful?
                     (unless (or (null? returnedState) (instance? returnedState stateType))
                        (error "fail.soa.stateTypeMismatch" stateType (nexj.core.meta.GenericType'typeOf returnedState))
                     )
                  )
                  (interface-method':validate-result "proxy result" result)
                  ; return:
                  (if explicit-state?
                     (values result returnedState)
                     (begin
                        (when stateful?
                           (hashtable-set! methodStateTab stateKey returnedState)
                        )
                        result
                     )
                  )
               )
            )
         )
      )
   )
)

(define soa:stat-invalid-ch-pattern (string-pattern "[:/]"))

; Returns the path for statistics about the specified operation. 
;
; @arg service string The fully qualified service name.
; @arg interface string The fully qualified interface name.
; @arg operation string The operation name.
; @arg arg-count integer The number of arguments of the operation.
; @ret string The path.
(define (soa:get-stat-path service interface operation arg-count)
   (string-append
      (string-replace service soa:stat-invalid-ch-pattern "_")
      "/" (substring interface (+ (interface'lastIndexOf #\:) 1) (string-length interface)) "/" operation "#" (number->string arg-count)
   )
)

(define soa:stat-invocations "Invocation Count")
(define soa:stat-faults "Fault Count")
(define soa:stat-exec-time "Execution Time (ms)")
(define soa:stat-ninetieth-time "90th Percentile (ms)")
