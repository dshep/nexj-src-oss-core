// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.hl7;

import java.io.StringWriter;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.rpc.TransferObject;
import nexj.core.util.UncheckedException;

public class HL7MessageParserTest extends SQLDataTest
{
   protected final static String TIME = "20050326012305.528+0000";

   protected final static String ADT_A04 = "MSH|^~\\&|||||20050326012305.528+0100||ADT^A04|1|P|2.5||||||\r" +
      "EVN|A04||||||\rPID|||1234567890^^^&OHIP||Test^Joe||19800102|M||U|123 45th St^^Toronto^ON^A1B2C3^Canada^H^" +
      "||(416) 123-4567|(416) 456-7890|||||||||||||||||||||||||\r";

   protected Format m_format;
   protected MessageParser m_parser;
   protected MessageFormatter m_formatter;
   protected Message m_adtA04;

   public HL7MessageParserTest(String sName)
   {
      super(sName);
   }

   protected void setUp() throws Exception
   {
      super.setUp();

      m_format = Repository.getMetadata().getFormat("HL7");
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
      m_adtA04 = Repository.getMetadata().getMessage("HL7_25_ADT_A04");
   }

   protected int findField(String sMsg, int nOrdinal)
   {
      int i = 0;

      while (nOrdinal-- > 0)
      {
         i = sMsg.indexOf('|', i);

         if (i < 0)
         {
            break;
         }

         ++i;
      }

      return i;
   }

   protected String changeOrdinal(String sMsg, int nOrdinal)
   {
      int i = findField(sMsg, 9);

      return sMsg.substring(0, i) + String.valueOf(nOrdinal) + sMsg.substring(sMsg.indexOf('|', i + 1)); 
   }

   protected String normalize(String sMsg)
   {
      int i = findField(sMsg, 6);

      return sMsg.substring(0, i) + TIME + sMsg.substring(sMsg.indexOf('|', i + 1));
   }

   protected void testParsingAndFormatting(Message message, String sMsg, int nOrdinal)
   {
      TransferObject tobj = m_parser.parse(new StringInput(sMsg), message);
      StringWriter writer = new StringWriter();
      m_formatter.format(tobj, message, new WriterOutput(writer));

      assertEquals(changeOrdinal(sMsg, nOrdinal), normalize(writer.toString()));
   }

   protected void testParsingAndFormatting(String sName, String sMsg, int nOrdinal)
   {
      testParsingAndFormatting(Repository.getMetadata().getMessage(sName), sMsg, nOrdinal);
   }

   public void testParseInputMessage()
   {
      TransferObject tobj = m_parser.parse(new StringInput(ADT_A04), m_adtA04);

      assertEquals("HL7_25_ADT_A04", tobj.getClassName());
      assertEquals("Test", ((TransferObject)((TransferObject)((List)((TransferObject)tobj.getValue("patientIdentification"))
         .getValue("patientName")).get(0)).getValue("familyName")).getValue("surname"));
      assertEquals(315615600000L, ((Timestamp)((TransferObject)((TransferObject)tobj.getValue("patientIdentification"))
         .getValue("dateTimeOfBirth")).getValue("time")).getTime());

      StringWriter writer = new StringWriter();

      m_formatter.format(tobj, m_adtA04, new WriterOutput(writer));
      
      assertEquals("MSH|^~\\&|||||20050326002305.528+0000||ADT^A04|1|P|2.5||||||\r" +
         "EVN|A04||||||\rPID|||1234567890^^^&OHIP||Test^Joe||19800101230000.000+0000|M||U|" +
         "123 45th St^^Toronto^ON^A1B2C3^Canada^H||(416) 123-4567|(416) 456-7890|||||||||||||||||||||||||\r",
         writer.toString());

      testParsingAndFormatting(m_adtA04, "MSH|^~\\&|||||" + TIME + "||ADT^A04|1|P|2.5||||||\r" +
         "EVN|A04||||||\rPID|||^^^&OHIP||Test^Joe||||||||||||||||||||||||||||||||||\r", 1);

      testParsingAndFormatting("HL7_A_B", "MSH|^~\\&|NexJ||||" + TIME + "||A^B|1|P|2.5||||||\rC|2|\rD||a|\rD||b|\rC|7|\r", 1);
      testParsingAndFormatting("HL7_A_B", "MSH|^~\\&|NexJ||||" + TIME + "||A^B|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r", 2);
      testParsingAndFormatting("HL7_A", "MSH|^~\\&|NexJ||||" + TIME + "||A|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r", 3);

      testParsingAndFormatting("HL7_Group", "MSH|^~\\&|NexJ||||" + TIME + "||MSO^SOUP|1601|P|2.5||||||\rPID||||||||" +
         "HL7_Group PID value1\rGWH|HL7_Group GWHStructure GWH value11|HL7_Group GWHStructure GWH value21||||||" +
         "HL7_Group GWHStructure GWH valuen1\rPOD|HL7_Group GWHStructure PODS POD value31||||" +
         "HL7_Group GWHStructure PODS POD value41\rPOD|HL7_Group GWHStructure PODS POD value31||||" +
         "HL7_Group GWHStructure PODS POD value41\rGWH|HL7_Group GWHStructure GWH value11|HL7_Group GWHStructure GWH value21||||||" +
         "HL7_Group GWHStructure GWH valuen1\rPOD|HL7_Group GWHStructure PODS POD value31||||HL7_Group GWHStructure PODS POD value41\rPOD|" +
         "HL7_Group GWHStructure PODS POD value31||||HL7_Group GWHStructure PODS POD value41\rSRD|||" +
         "HL7_Group SRD srdval311~HL7_Group SRD srdval312||^^^^HL7_Group SRD srdval5 value51~^^^^HL7_Group SRD srdval5 value52\rSRD|||" +
         "HL7_Group SRD srdval321~HL7_Group SRD srdval322||^^^^HL7_Group SRD srdval5 value51~^^^^HL7_Group SRD srdval5 value52\r", 4);

      testParsingAndFormatting("HL7_H", "MSH|^~\\&|NexJ||||" + TIME + "||H|1|P|2.5||||||\rI|C^a&b\r", 5);

      testParsingAndFormatting("HL7_Repeat", "MSH|^~\\&|RM|1N|HMI|1N|" + TIME + "||ORU^R01|1|P|2.3|||AL|NE||||\r" +
         "PID|||ON0209028^^^RM^MR^1N~ON0209028^^^RM^MR^1A||TEST^TERESSA||19881010000000.000+0000|F|||21 LOMA VIST DRIVE^L4J7S5^THORNHILL^ON||" +
         "9057717672|||||||||||||||||\r", 1);
   }

   public void testParseInputMessageTable()
   {
      MessageTable table = new MessageTable();

      table.addMessage(m_adtA04);
      table.addMessage(getMetadata().getMessage("HL7_A_B"));
      table.addMessage(getMetadata().getMessage("HL7_A"));
      m_parser.initializeMessageTable(table);

      TransferObject tobj = m_parser.parse(new StringInput(ADT_A04), table);

      assertEquals("HL7_25_ADT_A04", tobj.getClassName());
      assertEquals("Test", ((TransferObject)((TransferObject)((List)((TransferObject)tobj.getValue("patientIdentification"))
         .getValue("patientName")).get(0)).getValue("familyName")).getValue("surname"));
      assertEquals(315615600000L, ((Timestamp)((TransferObject)((TransferObject)tobj.getValue("patientIdentification"))
         .getValue("dateTimeOfBirth")).getValue("time")).getTime());

      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||A^B|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A_B", tobj.getClassName());
      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||X^Y^A_B|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A_B", tobj.getClassName());
      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||X^Y^A|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A", tobj.getClassName());
      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||A^Y|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A", tobj.getClassName());
      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||A^B^X_Y|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A_B", tobj.getClassName());
      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||A^Y^X_Y|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A", tobj.getClassName());
      tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||A^Y|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      assertEquals("HL7_A", tobj.getClassName());

      try
      {
         tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||X^Y|1|P|2.5||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.hl7.unsupportedMessage", e.getErrorCode());
      }

      try
      {
         tobj = m_parser.parse(new StringInput("MSH|^~\\&|NexJ||||" + TIME + "||X^Y|1|P|2.6||||||\rC|2|\rD||a|q^r^q&r\rD||b|q1\rC|7|\r"), table);
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.hl7.unsupportedMessage", e.getErrorCode());
      }
   }

   /**
    * Tests lax enumerations where enumeration values are not enforced.
    */
   public void testLaxEnumerations()
   {
      Message msg = m_context.getMetadata().getMessage("LaxEnumeration");
      Message msg2 = m_context.getMetadata().getMessage("NonLaxEnumeration");

      m_formatter.format(m_parser.parse(new StringInput(
            "MSH|^~\\&|NexJ||||20110912193128.774+0000||hL7Mapping|261|P|2.5||||||\rB|D"), msg), msg,
            new WriterOutput(new StringWriter()));

      try
      {
         m_formatter.format(m_parser.parse(new StringInput(
               "MSH|^~\\&|NexJ||||20110912193128.774+0000||hL7Mapping|261|P|2.5||||||\rB|D"), msg2), msg2,
               new WriterOutput(new StringWriter()));
         fail("Expected err.meta.integration.enumerationValue");
      }
      catch (IntegrationException e)
      {
         assertEquals(((UncheckedException)e.getCause()).getErrorCode(),
               "err.meta.integration.enumerationValue");
      }
   }

   /**
    * Test "" (i.e. null) enumeration values.
    */
   public void testNullEnumValues()
   {
      Message msg = m_context.getMetadata().getMessage("HL7_Enum");
      m_formatter.format(m_parser.parse(new StringInput(
            "MSH|^~\\&|NexJ||||20110912193128.774+0000||hL7Mapping|261|P|2.5||||||\rB|\"\""), msg), msg,
            new WriterOutput(new StringWriter()));
   }

   /**
    * Test invalid nesting level.
    */
   public void testNestingLevel()
   {
      Message msg2 = m_context.getMetadata().getMessage("HL7_Enum2");

      try
      {
         m_formatter.format(m_parser.parse(new StringInput(
               "MSH|^~\\&|NexJ||||20110912193128.774+0000||hL7Mapping|261|P|2.5||||||\rC"), msg2), msg2,
               new WriterOutput(new StringWriter()));
         fail("Expected err.meta.integration.hl7.refLevel");
      }
      catch (IntegrationException e)
      {
         assertEquals(e.getErrorCode(), "err.meta.integration.hl7.refLevel");
      }
   }

   /**
    * Test for DevCenter bug #102778.
    * The patientName value of "~^^^^^^S" should result in an array of 2 TransferObjects,
    * the first of which is empty.
    */
   public void testBug102778()
   {
      Message msg = m_context.getMetadata().getMessage("HL7_bug_102778");
      TransferObject tobj = m_parser.parse(new StringInput(
         "MSH|^~\\&|NexJ_MPI|NexJ|NIST_Hydra_PIX_Consumer|NIST|||RSP^K23^RSP_K23||P|2.5|||||||||\r" +
         "PID|||||~^^^^^^S|\r"), msg);
      TransferObject query_response = (TransferObject)tobj.getValue("query_response");
      TransferObject pid = (TransferObject)query_response.getValue("PID");
      ArrayList patientName = (ArrayList)pid.getValue("patientName");

      assertEquals(2, patientName.size());

      String nameTypeCode = (String)((TransferObject)patientName.get(1)).getValue("nameTypeCode");

      assertEquals("S", nameTypeCode);
   }
}
