Example of NexJ Generic Server Interface invocation through typed document/literal wrapped SOAP for Blackberry MDS.

NOTE: Blackberry MDS has multiple limitations to the WSDL features it can support.

1) install "Blackberry MDS Studio"
2) open "MDS Application Development" perspective
3) create new "MDS Studio Project"
4) select "Bottom Up Approach", click "Next"
5) select "WebService Connector Plugin", click "Next"
5) use http://<host>:<port>/<context-root>/web?basic (e.g. "http://localhost:8080/nexj/web?basic") for "WSDL URL", enter username/password, click "Next"
   to generate a WSDL conforming to MDS limitations the "?basic" URL query argument MUST be specified
   Note: "Blackberry MDS Studio" does not support more than one URL query argument, however, it is safe to omit "wsdl" query argument
   Note2: if getting "An unexpected error occurred" after clicking "Next" then increase "-Xmx" value in eclipse.ini



Example of NexJ Generic Server Interface invocation through typed document/literal wrapped SOAP for IBM Websphere wsimport.exe.

Get the NexJ WsDL file from http://<host>:<port>/<context-root>/xml?wsdl
e.g. wget --http-user=nexjsa --http-passwd=nexj "http://localhost:8080/nexj/xml?wsdl" -O nexj.wsdl

Use the following command to generate the SOAP proxy:
e.g. wsimport.exe -extension -b customization.xml nexj.wsdl

To compile the example, run the following:
javac com\nexj\xml\RESTXMLClientForEvents.java

To execute the example, run the following:
java com.nexj.xml.RESTXMLClientForEvents

NOTE: The "-extension" option is required for wsimport.exe to properly escape Java reserved identifiers.

NOTE 2: The "-b customization.xml" is required for wsimport.exe to properly interpret '_' characters.
The customization.xml file should contain the following:

<jaxws:bindings wsdlLocation="nexj.wsdl"
   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
   xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
   xmlns:jaxws="http://java.sun.com/xml/ns/jaxws"
   xmlns:jaxb="http://java.sun.com/xml/ns/jaxb">
   <!-- Interpret underscore as part of the identifier i.e. do not suppress it. -->
   <jaxws:bindings node="wsdl:definitions/wsdl:types/xsd:schema[@targetNamespace='http://www.nexj.com/xml']">
      <jaxb:globalBindings underscoreBinding="asCharInWord"/>
   </jaxws:bindings>
</jaxws:bindings>

NOTE 3: During proxy generation a warning similar to:

[WARNING] src-resolve: Cannot resolve the name '...' to a(n) 'type definition' component.

can be safely ignored. It is caused by wsimport.exe having trouble identifying complexTypes that it
has already defined in another schema. The resulting Java proxy will still be generated correctly.