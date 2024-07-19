package nexj.core.meta.xml;

import java.io.BufferedReader;
import java.io.StringWriter;
import java.net.URL;

import junit.framework.TestCase;

import nexj.core.meta.soa.Argument;
import nexj.core.meta.soa.Definition;
import nexj.core.meta.soa.Interface;
import nexj.core.meta.soa.Method;
import nexj.core.meta.soa.Result;
import nexj.core.meta.xml.XMLSOAMetadataExporter;
import nexj.core.scripting.Symbol;
import nexj.core.util.IOUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;

/**
 * Tests the XMLSOAMetadataExporter
 */
public class XMLSOAMetadataExporterTest extends TestCase
{
   /**
    * Tests that the exporter correctly handles the minimum required for a Definition
    */
   public void testMinimum() throws Exception
   {
      Definition def = new Definition();

      def.setName("definition");
      def.setVersion("");

      // export definition object
      StringWriter sw = new StringWriter();
      XMLSOAMetadataExporter exporter = new XMLSOAMetadataExporter(new XMLWriter(sw));

      exporter.exportDefinition(def);

      // normalize resulting XML
      String sFormattedXML = XMLUtil.formatXML(sw.toString());
      assertEquals("<SOADefinition name=\"definition\">\r\n   <Service/>\r\n</SOADefinition>\r\n", sFormattedXML);
   }

   /**
    * Tests that the exporter correctly handles services and interfaces
    */
   public void testServicesAndInterfaces() throws Exception
   {
      // build definition object
      Definition def = new Definition();
      def.setName("definition1");
      def.setDescription("This is a definition. It can have Services, Interfaces, and Types among many other things. Bacon ipsum dolor sit amet dolor sausage id, anim boudin proident voluptate sirloin quis qui shankle. Consectetur minim pig t-bone cillum non. Deserunt est quis turkey reprehenderit, qui sirloin chuck velit spare ribs laboris shank cupidatat ut. Meatball ball tip id ullamco cupidatat, sausage incididunt exercitation shank. Pig cow ex excepteur adipisicing, fatback sunt drumstick sirloin beef in ullamco eiusmod. Excepteur chicken pork hamburger, sint tail drumstick enim. Rump pancetta qui tempor pastrami.");
      def.setVersion("v2.78.204g");

      Interface registrationInterface = def.defineInterface("Registration");

      registrationInterface.setDescription("This interface allows clients to register and unregister themselves.");

      Method registerMethod = new Method(registrationInterface);
      registerMethod.setName("register");
      registerMethod.setDescription("Registers a client");

      Argument clientArgument = new Argument();
      clientArgument.setName("client");
      clientArgument.setType(Symbol.define("identifier"));
      clientArgument.setDescription("The client name");

      Argument descArgument = new Argument();
      descArgument.setName("description");
      descArgument.setType(Symbol.define("String"));
      descArgument.setDescription("The description of the client");

      Result registerResult = new Result();
      registerResult.setType(Symbol.define("boolean"));
      registerResult.setDescription("True if registration completed successfully, false otherwise.");

      registerMethod.addArgument(clientArgument);
      registerMethod.addArgument(descArgument);
      registerMethod.setResult(registerResult);

      registrationInterface.addMethod(registerMethod);

      Method registerAllMethod = new Method(registrationInterface);
      registerAllMethod.setName("register-all");

      Argument clientsArgument = new Argument();
      clientsArgument.setName("clients");
      clientsArgument.setType(Symbol.define("List"));
      clientsArgument.setCollection(true);

      Result registerAllResult = new Result();
      registerAllResult.setType(Symbol.define("List"));
      registerAllResult.setCollection(true);

      registerAllMethod.addArgument(clientsArgument);
      registerAllMethod.setResult(registerAllResult);

      registrationInterface.addMethod(registerAllMethod);

      def.addServiceInterface(registrationInterface);

      Interface serviceInterface = def.defineInterface("Service");

      Method doWorkMethod = new Method(serviceInterface);
      doWorkMethod.setName("do-work");

      Argument arg0Argument = new Argument();
      arg0Argument.setName("arg0");
      arg0Argument.setType(Symbol.define("Object"));

      Argument arg1Argument = new Argument();
      arg1Argument.setName("arg1");
      arg1Argument.setType(Symbol.define("Object"));

      Result doWorkResult = new Result();
      doWorkResult.setType(Symbol.define("Object"));

      doWorkMethod.addArgument(arg0Argument);
      doWorkMethod.addArgument(arg1Argument);
      doWorkMethod.setResult(doWorkResult);

      Method doAgainMethod = new Method(serviceInterface);
      doAgainMethod.setName("do-again");

      serviceInterface.addMethod(doAgainMethod);
      serviceInterface.addMethod(doWorkMethod);

      def.addServiceInterface(serviceInterface);

      Interface clientInterface = def.defineInterface("Registered Client");

      clientInterface.setDescription("This interface is not used by any services in this SOADefinition.");

      Method isRegisteredMethod = new Method(clientInterface);
      isRegisteredMethod.setName("is-registered");

      Result isRegisteredResult = new Result();
      isRegisteredResult.setType(Symbol.define("boolean"));

      isRegisteredMethod.setResult(isRegisteredResult);

      clientInterface.addMethod(isRegisteredMethod);

      // export definition object
      StringWriter sw = new StringWriter();
      XMLSOAMetadataExporter exporter = new XMLSOAMetadataExporter(new XMLWriter(sw));

      exporter.exportDefinition(def);

      // normalize resulting XML
      String sFormattedXML = XMLUtil.formatXML(sw.toString());

      // compare to expected result
      URL url = XMLSOAMetadataExporterTest.class.getResource("SOADefinitionExample.soadef");
      BufferedReader expected = null;
      
      try
      {
         expected = IOUtil.openBufferedReader(URLUtil.openStream(url), IOUtil.ENCODING);
         sw = new StringWriter();
         
         IOUtil.copy(sw, expected);
         assertEquals(sw.toString(), sFormattedXML);
      }
      finally
      {
         IOUtil.close(expected);
      }
   }
}
