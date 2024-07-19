Example of NexJ Generic Server Interface invocation with typed XML.

Create a Metaclass to invoke events on, e.g.:

<Class aspects="!PARTITIONED">
   <Events>
      <Event args="districtName" compatible="true" name="validate" static="true" visibility="public">
         <Arguments>
            <Argument name="districtName" required="true" type="string"/>
         </Arguments>
         <Result required="true" type="boolean"/>
         <Actions>
            <Action name="main" type="main"><![CDATA[(logger'info "In validate, arg is \"" districtName "\"")]]></Action>
         </Actions>
      </Event>
   </Events>
</Class>

Create an Integration Channel via "NexJ Studio perspective" -> "Integration" -> "Channels", e.g.:

<HTTP contentType="text/xml" receive="false" url="http://localhost:8080/nexj/xml"/>


Invocation through SOAP:

select: File -> Import -> NexJ Studio -> XML Schema/WSDL
click: Next
enter: "http://localhost:8080/nexj/web?mask=DistrictService"
click: Next
enter: "Sample_"
click: Finish
supply username/password
click: OK

Use the following Scheme to execute the event and interpret the response:

(define req
   (message
      (: :class "HTTP")
      (: method "POST")
      (: principal "nexjsa")
      (: password "nexj")
      (: url "http://localhost:8080/nexj/web")
      (: body
         (format-message-pretty
            (message
               (: :class "Sample_validate_1")
               (: districtName "Washington")
            )
         )
      )
   )
)
(define resp
   (integration-send-receive req "Test"
      (cons () (list "Sample_validate_1_Response"))
   )
)


Invocation through REST:

Create an Integration Message "Sample_validate_Response" via "NexJ Studio perspective" -> "Integration" -> "Messages", e.g.:

<Message aggregation="sequential" format="XML">
   <XMLMapping namespace="ns0" node="_boolean" type="element" uri="http://www.nexj.com/xml"/>
   <Parts>
      <Value maxCount="1" minCount="1" name="item" type="boolean">
         <XMLMapping type="value"/>
      </Value>
   </Parts>
</Message>

Use the following Scheme to execute the event and interpret the response:

(define req
   (message
      (: :class "HTTP")
      (: method "POST")
      (: principal "nexjsa")
      (: password "nexj")
      (: url "http://localhost:8080/nexj/xml/DistrictService/validate")
      (: parameters
         (message
            (: districtName "\"Washington\"")
         )
      )
   )
)
(define resp
   (integration-send-receive req "Test"
      (cons () (list "Sample_validate_Response"))
   )
)