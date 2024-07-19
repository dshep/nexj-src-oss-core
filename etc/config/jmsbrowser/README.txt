Using the JMS Browser Tool
==========================


In order to run the tool, java must be installed, and the application server must be running. In order to start the tool you must type:

   RunJMSReader.bat <Server Type> <Server Location> <Optional Properties>

-------------------------------------------------------------
* Server Type

Currently the tool supports 2 types of application servers:

1) JBoss
2) WebSphere

You must type either "JBoss" or "WebSphere" indicating the desired server. The capitalization of the characters must be exactly the same as shown above.

-------------------------------------------------------------
* Server Location

The installation location of the application server. For JBoss it should be the directory in which such directories as bin, lib, and server are located. For WebSphere - the directory in which AppServer (or AppClient) is located.

-------------------------------------------------------------
Optional Properties

If necessary, you can specify some optional properties such as queue name, connection factory name, etc. If you don't specify any of the properties, the default values will be used. Below is the list off all optional properties you can specify with the default values.

The list of the optional properties are:


-Dfactory=<JNDI name of the connection factory of the required queue>

         Default value: 

                  JBoss:	QueueConnectionFactory
                  WebSphere:   nexj/jms/cf/SystemQueue
                        
-Dqueue=<JNDI name of the required queue>

         Default value: 

                  JBoss:	queue/nexj/NEXJ_QUEUE
                  WebSphere:  nexj/jms/queue/NEXJ_QUEUE
                  
-Ddirectory=<the name of the file> -  the directory path where the the file with the results will be located. If the directory does not exist, the system will create it (as well as the entire path).

         Default value:		C:\dumpingPlace
         
-Dheaders=false          -   If you want only the bodies of the messages to be dumped to the file (without headers), use this option

         Default value: 	true

-DXML             -   There are two ways the tool outputs values: 

                                  1) XML format (default) - the format, that will allow you to deserialize messages back
                                       -DXMLFormatOutput = true
                                       
                                  2) Log format - the format, that takes much less space than XML format, but will not allow you to deserialize messages back
                                       -DXMLFormatOutput = false

         Default value:    true
   
-Durl=<host>[:<port>] - If you do not specify anything, or you specify just localhost, the system will automaticly make a full valid url name for you (localhost:1099 for JBoss, and localhost:2809 for WebSphere). Depending on your machine settings, these default values may be incorrect. Be sure to check the exact port number in your server settings. For WebSphere, please make sure to put the number of BOOTSTRAP_ADDRESS, that you can find in the WebSphere administrative console at Servers -> Application Servers -> {server's name} -> Communications -> Ports -> BOOTSTRAP_ADDRESS. For JBoss 

         Default value: 

                  JBoss:	localhost:1099
                  WebSphere: 	localhost:2809

---------------------
Necessary Jars

The following jar files must be present in the current directory (additionally to nexj-jmsbrowser.jar and nexj-server.jar):

log4j-1.2.14.jar
xercesImpl.jar
xalan.jar
serializer.jar (from xalan)

for WebSphere additionally jms.jar should be present
---------------------
Example:

JBOSS:

jmsbrowser.bat JBoss C:\jboss-4.0.5.GA -Dfactory=QueueConnectionFactory -Dqueue=queue/nexj/NEXJ_QUEUE -Ddirectory=C:\dump1\dump2 -Dheaders=false -DXML=true

WebSphere:

jmsbrowser.bat WebSphere C:\AppServers\WebSphere

    