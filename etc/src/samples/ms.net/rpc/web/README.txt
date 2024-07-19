Example of NexJ Generic Server Interface invocation through typed document/literal wrapped SOAP.

This invocation style is suitable for custom development against NexJ domain classes.
It uses WS-I compliant WSDL and simple XML messages that can be validated with XML
schema processors and can be easily transformed with XSLT.
Support for circular graphs is provided despite the limitations of the format,
however it requires some cooperation from the application developer.
If seamless support for circular graphs is needed and WS-I compliance and
strongly typed messages are not required, use the WSDL for the RPC/encoded style
available under the <context-root>/soap?wsdl URL.

NOTE: .NET Framework 2.0 features are used in the sample code.

Use the following command to generate the SOAP proxy:

wsdl /n:NexJ /f /protocol:SOAP /u:<user> /p:<password> http://<host>:<port>/<context-root>/web?wsdl

e.g.

wsdl /n:NexJ /f /protocol:SOAP /u:nexjsa /p:nexj http://localhost:8080/nexj/web?wsdl

Keep in mind that if the proxy is generated with Visual Studio .NET, it contains extra
logic for initializing its URL, which affects its behavior for local addresses.

For VB, add to the wsdl the option /language:VB, then edit the generated file
GenericServer.vb to add the "Shadows" keyword to the declaration of functions
"invoke", "Begininvoke" and "Endinvoke" and change all the references to "Me"
into references to "MyBase" inside them.

To compile the example, run the following:
csc TypedSOAPClient.cs GenericObjects.cs GenericServer.cs
