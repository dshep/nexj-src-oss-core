// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.StringReader;

import junit.framework.TestCase;

/**
 * Tests the XML parsing and formatting utilities.
 */
public class XMLUtilTest extends TestCase
{
   /**
    * Tests that the parser will not expand entity references to a point where the system could
    * become unstable.
    */
   public void testEntityExpansionDOSAttack()
   {
      StringReader reader = new StringReader("<!DOCTYPE root [" +
            "<!ENTITY ha \"Ha !\">" +
            "<!ENTITY ha2 \"&ha; &ha;\">" +
            "<!ENTITY ha3 \"&ha2; &ha2;\">" +
            "<!ENTITY ha4 \"&ha3; &ha3;\">" +
            "<!ENTITY ha5 \"&ha4; &ha4;\">" +
            "<!ENTITY ha6 \"&ha5; &ha5;\">" +
            "<!ENTITY ha7 \"&ha6; &ha6;\">" +
            "<!ENTITY ha8 \"&ha7; &ha7;\">" +
            "<!ENTITY ha9 \"&ha8; &ha8;\">" +
            "<!ENTITY ha10 \"&ha9; &ha9;\">" +
            "<!ENTITY ha11 \"&ha10; &ha10;\">" +
            "<!ENTITY ha12 \"&ha11; &ha11;\">" +
            "<!ENTITY ha13 \"&ha12; &ha12;\">" +
            "<!ENTITY ha14 \"&ha13; &ha13;\">" +
            "<!ENTITY ha15 \"&ha14; &ha14;\">" +
            "<!ENTITY ha16 \"&ha15; &ha15;\">" +
            "<!ENTITY ha17 \"&ha16; &ha16;\">" +
            "]>" +
            "<root>&ha17;</root>");

      try
      {
         XMLUtil.parse(reader);
         fail("Expected XMLParserException");
      }
      catch (XMLParserException ex)
      {
         assertEquals("err.xml.parser", ex.getErrorCode());

         String sMessage = (String)ex.getErrorArgs()[0];

         assertTrue(sMessage.startsWith("The parser has encountered more than"));
         assertTrue(sMessage.endsWith("entity expansions in this document; this is the limit imposed by the application."));
      }
      finally
      {
         IOUtil.close(reader);
      }
   }
}
