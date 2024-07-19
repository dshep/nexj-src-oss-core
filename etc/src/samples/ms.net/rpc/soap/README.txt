Example of NexJ Generic Server Interface invocation through RPC/encoded SOAP.

This invocation style uses only generic objects and is useful for applications
that favor use of metadata and/or require transparent handling of circular graphs.
It is not convenient if WS-I compliance is required, or XSLT transformations or
other direct manipulation of the serialized XML is needed: in this case,
use the WSDL for the typed document/literal wrapped style available
under the <context-root>/web?wsdl URL.

NOTE: .NET Framework 2.0 features are used in the sample code.

Use the following command to generate the SOAP proxy:

wsdl /n:NexJ /f /protocol:SOAP /u:<user> /p:<password> http://<host>:<port>/<context-root>/soap?wsdl

e.g.

wsdl /n:NexJ /f /protocol:SOAP /u:nexjsa /p:nexj http://localhost:7080/nexj/soap?wsdl

Keep in mind that if the proxy is generated with Visual Studio .NET, it contains extra
logic for initializing its URL, which affects its behavior for local addresses.

For VB, add to the wsdl the option /language:VB, then edit the generated file
GenericServer.vb to add the "Shadows" keyword to the declaration of functions
"invoke", "Begininvoke" and "Endinvoke" and change all the references to "Me"
into references to "MyBase" inside them.

To compile the example, run the following command at the .NET SDK command prompt:

csc GenericSOAPClient.cs GenericObjects.cs GenericServer.cs
