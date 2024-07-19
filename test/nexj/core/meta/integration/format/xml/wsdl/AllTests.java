// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.meta.integration.format.xml.wsdl");

      //$JUnit-BEGIN$
      suite.addTestSuite(ServiceSOAConverterTest.class);
      suite.addTestSuite(WSDLServiceImporterTest.class);
      suite.addTestSuite(XSDSchemaImporterTest.class);
      //$JUnit-END$
      return suite;
   }
}
