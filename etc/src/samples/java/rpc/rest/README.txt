Example of NexJ Generic Server Interface invocation through REST interface with typed XML.

Get the NexJ WADL file from http://<host>:<port>/<context-root>/xml?wadl
e.g. wget --http-user=nexjsa --http-passwd=nexj "http://localhost:8080/nexj/xml?wadl" -O nexj.wadl

The Java WADL proxy generator is available at https://wadl.dev.java.net/.

Use the following command to generate the REST proxy from WADL:
set WADL_OPTS=-Xmx512M
wadl2java -o . -p nexj.app.demo.rpc.rest nexj.wadl

To compile the example, run the following:

javac -cp lib\activation-1.1.jar;lib\comresrcgen-1.0.jar;lib\jaxb-api-2.1.jar;
   lib\jaxb-impl-2.1.jar;lib\jaxb-xjc-2.1.jar;lib\localizer-1.0.jar;
   lib\stax-api-1.0-2.jar;lib\wadl-core-1.1-SNAPSHOT.jar
   -J-Xmx1024M nexj\app\demo\rpc\rest\*.java

To execute the example, run the following:

java -cp lib\activation-1.1.jar;lib\comresrcgen-1.0.jar;lib\jaxb-api-2.1.jar;
   lib\jaxb-impl-2.1.jar;lib\jaxb-xjc-2.1.jar;lib\localizer-1.0.jar;
   lib\stax-api-1.0-2.jar;lib\wadl-core-1.1-SNAPSHOT.jar;.
   -Xmx512M nexj.app.demo.rpc.rest.RESTXMLClient


NOTE:
The wadl2java tool will report "Error: Two declarations cause a collision in the ObjectFactory class."
for certain classes that have an attribute, which when prefixed with the current class name, is same as
the name of another class. The error is caused by com.sun.tools.xjc.model.CElementInfo.getSqueezedName()
generating member variable names by prefixing the element name with the complexType name. Thus both the
type and the attribute sometimes generate identically named variables/functions in ObjectFactory.java.
e.g. the class "Act" will cause collisions on the following:
<element name="folder" type="ns:ActFolder" minOccurs="0"/>
<element name="template" type="ns:ActTemplate" minOccurs="0"/>
e.g. the class "EnterpriseComponent" will cause collisions on the following:
<element name="group" type="ns:PortletGroup" minOccurs="0"/>

However, most classes will not cause similar collisions.
e.g. class "Entity" will not cause a collision on:
<element name="coverage" type="ns:EntityCoverage" minOccurs="0"/>

NOTE 2:
The wadl2java utility needs to be patched for it to generate valid Java from a WADL.