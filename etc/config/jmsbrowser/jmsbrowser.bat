@echo off
If not %1.==[]. (Cmd /V:On /C Call %0 [] %* & GoTo :EOF)
set s=%*
for /f "tokens=1,2,3,4 delims= " %%a in ("%s%") do set discard=%%a&set servertype=%%b&set serverlocation=%%c&set rest=%%d
set #=%servertype%
set length1=0
:loop
if defined # (set #=%#:~1%&set /A length1 += 1&goto loop)
set #=%serverlocation%
set length=0
:loop2
if defined # (set #=%#:~1%&set /A length += 1&goto loop2)
set /A size=%length%+%length1%+1+1+3
set rest=!s:~%size%!
if %servertype%==JBoss goto JB
if %servertype%==WebSphere goto WS
exit
:JB
java -cp nexj-server.jar;nexj-jmsbrowser.jar;%serverlocation%/client/jbossall-client.jar;xercesImpl.jar;xalan.jar;serializer.jar;log4j-1.2.14.jar -Dtype=JBoss -Dfactory=QueueConnectionFactory -Dqueue=queue/nexj/NEXJ_QUEUE -Ddirectory=C:/dumpingPlace -Dnexj.config=/nexj/default.config %rest% nexj.core.tools.JMSBrowserTool
exit
:WS
%serverlocation%/AppClient/java/bin/java -cp %serverlocation%/AppClient/lib/j2ee.jar;%serverlocation%/AppClient/plugins/org.eclipse.osgi_3.2.1.R32x_v20060919.jar;%serverlocation%/AppClient/plugins/org.eclipse.equinox.common_3.2.0.v20060603.jar;%serverlocation%/AppClient/plugins/org.eclipse.core.runtime_3.2.0.v20060603.jar;%serverlocation%/AppClient/plugins/org.eclipse.equinox.registry_3.2.1.R32x_v20060814.jar;%serverlocation%/AppClient/plugins/com.ibm.ws.bootstrap_6.1.0.jar;%serverlocation%/AppClient/plugins/com.ibm.ws.sib.client_2.0.0.jar;%serverlocation%/AppClient/plugins/org.eclipse.emf.commonj.sdo_2.1.0.v200609210005.jar;%serverlocation%/AppClient/plugins/com.ibm.ws.sib.utils_2.0.0.jar;%serverlocation%/AppClient/plugins/com.ibm.ws.emf_2.1.0.jar;nexj-server.jar;nexj-jmsbrowser.jar;%serverlocation%/AppClient/lib/lmproxy.jar;%serverlocation%/AppClient/plugins/com.ibm.ws.wccm_6.1.0.jar;%serverlocation%/AppClient/lib/bootstrap.jar;jms.jar;xalan.jar;serializer.jar;log4j-1.2.14.jar;%serverlocation%/AppClient/plugins/org.eclipse.emf.common_2.2.1.v200609210005.jar;%serverlocation%/AppClient/plugins/com.ibm.wsspi.extension_6.1.0.jar;%serverlocation%/AppClient/plugins/com.ibm.ws.runtime_6.1.0.jar;%serverlocation%/AppClient/plugins/org.eclipse.emf.ecore_2.2.1.v200609210005.jar -Dnexj.logger.factory=nexj.core.util.log.log4j.Log4JLoggerFactory -Dlog4j.appender.console=org.apache.log4j.ConsoleAppender -Dlog4j.appender.console.layout=org.apache.log4j.PatternLayout -Dlog4j.appender.console.layout.ConversionPattern="%%p: %%m%%n" -Dlog4j.rootLogger=INFO,console -Dlog4j.logger.nexj.core.tools=INFO -Djava.util.logging.manager=com.ibm.ws.bootstrap.WsLogManager -Djava.util.logging.configureByServer=true -Dcom.ibm.CORBA.ConfigURL=file:%serverlocation%/AppClient/properties/sas.client.props -Djava.security.auth.login.config=%serverlocation%/AppClient/properties/wsjaas_client.conf -Dcom.ibm.SSL.ConfigURL=file:%serverlocation%/AppClient/properties/ssl.client.props -Dtype=WebSphere -Durl=localhost:2809 -Dfactory=nexj/jms/cf/SystemQueue -Dqueue=nexj/jms/queue/NEXJ_QUEUE -Ddirectory=C:/dumpingPlace -Dnexj.config=/nexj/default.config %rest% nexj.core.tools.JMSBrowserTool
exit
