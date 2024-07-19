// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.meta.xml");

      if (!suite.addTestSuite("nexj.core.meta.xml.XMLEnterpriseMetadataTest"))
      {
         suite.addTestSuite(XMLMetadataTest.class);
      }

      //$JUnit-BEGIN$
      suite.addTestSuite(XMLMetadataHelperTest.class);
      suite.addTestSuite(XMLSOAMetadataExporterTest.class);
      suite.addTestSuite(XMLSOAMetadataLoaderTest.class);
      //$JUnit-END$
      return suite;
   }
}