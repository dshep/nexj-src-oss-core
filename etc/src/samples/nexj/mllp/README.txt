How to Send and Receive MLLP Messages


1. Define a MLLP message splitter component in metadata.
Example: (file TCPChannel.MLLPMessageSplitter.comp)

<Component type="nexj.core.rpc.tcp.MLLPMessageStreamFactory" activation="singleton"
   description="A splitter for MLLP messages.">
   <Properties>
      <Property name="startBlock">11</Property>
      <Property name="endBlock">28</Property>
      <Property name="messageSeparator">13</Property>
      <Property name="minByte">31</Property>
   </Properties>
</Component>


2. Define a TCP channel to be used for the MLLP messages.
Example: (file TCPTest.channel)

<TCP defaultUser="nexjsa" encoding="utf-8" localPort="3100" readTimeout="60000" splitter="TCPChannel.MLLPMessageSplitter">
   <ServiceBindings>
      <ServiceBinding output="" service="MLLPReceive">
         <Arguments>
            <Argument name="receiver" value="&quot;EchoService&quot;"/>
         </Arguments>
      </ServiceBinding>
   </ServiceBindings>
</TCP>

- Note that the splitter property of the channel refers to the TCPChannel.MLLPMessageSplitter
   component created in step 1.
- The localPort property indicates that the framework will listen for new TCP connections on 
   port 3100.
- The readTimeout property (specified in milliseconds) indicates the maximum amount of time the
   framework will block waiting for an incoming MLLP message to complete.
   
IMPORTANT:
- The service binding with output="" and service="MLLPReceive" should always be used.
- For servers, you must specify a custom service for handling incoming MLLP messages.  The 
   'receiver' argument should be set to the name of this custom service.
   
   
3. Define a custom service (for server functionality)
Example: (file EchoService.service)

<Service args="empty" interface="HL7_23_CML_ORU" layout="startX:169;endY:201;startY:198;endX:637">
   <Script caption="Echo" layout="y:199;x:387" name="script"><![CDATA[(logger'debug "Started Echo Service!" this)
this]]></Script>
</Service>

- This service simply returns (echoes back) whatever messages it receives.
- If an interface is specified (HL7_23_CML_ORU in this case), then the corresponding .interface
   file must exist in the metadata.  All messages will be parsed according to this interface
   before being delivered to the custom service.
   (See example HL7_23_CML_ORU.interface file)
- To send a response message, the custom service should simply return the response.
- To NOT send a response, the custom service should return null.


4. Send MLLP messages (for client functionality)
The following sample code sends a MLLP message to localhost:9000, and waits for a response.
(See example.scm)

(let
   (
      (msg (message (: host "127.0.0.1") (: port 9000) (: channel "TCPTest")
         (: body "MSH|^~\\&|Reports|CML|||201004261628-500||ORU^R01||1|2.3\\rPID|1 ... \\rFTS|1")))
   )
   ((SysService'invoke "MLLPSend" msg '() 3 #t)'result)
)

"MLLPSend" - Indicates you want to invoke the MLLPSend service.
msg - The raw TCP message you want to send.
'() - Indicates a null output channel.
3 - The maximum number of timeout retry attempts.
#t - Indicates we want to wait for a response.  If #f, the service will return null immediately 
   after sending.
   
The sample code will return the raw TCP response message.







