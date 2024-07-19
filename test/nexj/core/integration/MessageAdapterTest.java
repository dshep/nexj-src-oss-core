// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.StringWriter;
import java.io.Writer;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.format.xml.XSDMessageImporterTest;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.SysUtil;
import nexj.core.util.auth.SimplePrincipal;

public class MessageAdapterTest extends TestCase
{
   Metadata m_metadata;
   InvocationContext m_context;   
   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      try
      {
         System.setProperty(SysUtil.PROPERTY_PREFIX + SysUtil.CONFIG_PROPERTY, "/nexj/core/integration/test.server");
         m_metadata = Repository.getMetadata();

         m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
         m_context.initialize(new SimplePrincipal("test"));
      }
      catch (Exception e)
      {
         fail();
      }
   }
   
   public void testHL7Adapter()
   {
      Format format = m_metadata.getFormat("HL7");
      Message msg = m_metadata.getMessage("HL7_25_ADT_A04");
      TransferObject to = ((MessageParser)format.getParser().getInstance(m_context))
         .parse(new StringInput("MSH|^~\\&|||||20050326012305.528+0100||ADT^A04|1|P|2.5||||||\rEVN|A04||||||\rPID|||1234567890^^^&OHIP||Test^Joe||19800102|M||U|123 45th St^^Toronto^ON^A1B2C3^Canada^H^||(416) 123-4567|(416) 456-7890|||||||||||||||||||||||||\r"), msg);
      TransferObject patientID = (TransferObject)to.getValue("patientIdentification");

      Timestamp birthDate = (Timestamp)((TransferObject)patientID.getValue("dateTimeOfBirth")).getValue("time");
      Calendar calendar = Calendar.getInstance();
      calendar.setTime(birthDate);
      assertEquals(calendar.get(Calendar.YEAR), 1980);
      assertEquals(calendar.get(Calendar.MONTH), 1);
      assertEquals(calendar.get(Calendar.DAY_OF_MONTH), 1);

      TransferObject name = (TransferObject)patientID.getValue("patientName");
      assertEquals("Joe", name.getValue("givenName"));
      assertEquals(((TransferObject)name.getValue("familyName")).getValue("surname"), "Test");
   }

   public void testObjectAdapter()
   {
      TransferObject pat = new TransferObject();
      TransferObject pl = new TransferObject();
      pl.setValue("firstName", "My1");
      pl.setValue("lastName", "Test");
      List ids = new ArrayList();
      TransferObject id = new TransferObject();
      id.setValue("type", "OHIP");
      id.setValue("id", "111222333");
      ids.add(id);
      pl.setValue("ids", ids);
      pat.setValue("player", pl);
      Format format = m_metadata.getFormat("Object");
      Message msg = m_metadata.getMessage("Object_PatientDemographics");
      ObjectOutput out = new ObjectOutput();
      
      ((MessageFormatter)format.getFormatter().getInstance(m_context)).format(pat, msg, out);
      
      TransferObject patCompare = ((MessageParser)format.getParser().getInstance(m_context))
         .parse(new ObjectInput(out.getObject()), msg);
      TransferObject plCompare = (TransferObject)patCompare.getValue("player");
      List idsCompare = (List)plCompare.getValue("ids");
      TransferObject idCompare = (TransferObject)idsCompare.get(0);
      assertEquals("OHIP", idCompare.getValue("type"));
      assertEquals("111222333", idCompare.getValue("id"));
      assertEquals(plCompare.getValue("firstName"), "My1");
      assertEquals(plCompare.getValue("lastName"), "Test");
   }
   
   public void testXMLAdapter()
   {
      TransferObject pat = new TransferObject();
      TransferObject pl = new TransferObject();
      pl.setValue("firstName", "My1");
      pl.setValue("lastName", "Test");
      List ids = new ArrayList();
      TransferObject id = new TransferObject();
      id.setValue("type", "OHIP");
      id.setValue("id", "111222333");
      ids.add(id);
      pl.setValue("id", ids);
      pat.setValue("player", pl);
      Format format = m_metadata.getFormat("XML");
      Message msg = m_metadata.getMessage("XML_PatientDemographics");
      Writer writer = new StringWriter();

      ((MessageFormatter)format.getFormatter().getInstance(m_context)).format(pat, msg, new WriterOutput(writer));
      
      TransferObject patCompare = ((MessageParser)format.getParser().getInstance(m_context))
         .parse(new StringInput(writer.toString()), msg);
      TransferObject plCompare = (TransferObject)patCompare.getValue("player");
      List idsCompare = (List)plCompare.getValue("id");
      TransferObject idCompare = (TransferObject)idsCompare.get(0);
      assertEquals(idCompare.getValue("id"), "111222333");
      assertEquals(idCompare.getValue("type"), "OHIP");
      assertEquals(plCompare.getValue("firstName"), "My1");
      assertEquals(plCompare.getValue("lastName"), "Test");
   }

   public void testXMLAdapter2()
   {
      Format format = m_metadata.getFormat("XML");
      Message msg = m_metadata.getMessage("XML_PatientDemographics");
      TransferObject to = ((MessageParser)format.getParser().getInstance(m_context))
         .parse(new StringInput("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<a:Patient xmlns:a=\"http://nexj.com/nexj\" xmlns:b=\"http://nexj.com/nexj/b\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://nexj.com/nexj/integration.xsd\"><player><firstName>My1</firstName><lastName>Test</lastName><id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>"), msg);
      TransferObject player = (TransferObject)to.getValue("player");
      List ids = (List)player.getValue("id");
      TransferObject id = (TransferObject)ids.get(0);
      assertEquals(id.getValue("id"), "111222333");
      assertEquals(id.getValue("type"), "OHIP");
      assertEquals(player.getValue("firstName"), "My1");
      assertEquals(player.getValue("lastName"), "Test");
   }

   public void testXMLAdapter3()
   {
      /*
      Format format = m_metadata.getFormat("XML");
      Sequence seq = m_metadata.getSequence("XMLInput");
      TransferObject to = ((MessageParser)format.getParser().getInstance(m_context))
         .parse(new StringInput("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<a:Patient xmlns:a=\"http://nexj.com/nexj\" xmlns:b=\"http://nexj.com/nexj/b\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://nexj.com/nexj/integration.xsd\"><player><firstName>My1</firstName><lastName>Test</lastName><id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>"), seq.getMessageTable());
      TransferObject player = (TransferObject)to.getValue("player");
      List ids = (List)player.getValue("id");
      TransferObject id = (TransferObject)ids.get(0);
      assertEquals(id.getValue("id"), "111222333");
      assertEquals(id.getValue("type"), "OHIP");
      assertEquals(player.getValue("firstName"), "My1");
      assertEquals(player.getValue("lastName"), "Test");
      */
   }

   public void testXMLMessageAdapterElementContent() throws Exception
   {
      Map messageRegistry = XSDMessageImporterTest.getMessageRegistry(XSDMessageImporterTest.class.getResource("test.xsd"));

      Message tableMessage = (Message)messageRegistry.get("Table");      

      TransferObject table = new TransferObject();
      table.setValue("Association", new TransferObject());
      List rows = new ArrayList();
      TransferObject row = new TransferObject();
      rows.add(row);
      table.setValue("Row", rows);
      row.setValue("name", "row1");
      row.setValue("value", "abcdef");

      Format format = m_metadata.getFormat("XML");
      Writer writer = new StringWriter();
      ((MessageFormatter)format.getFormatter().getInstance(m_context)).format(table, tableMessage, new WriterOutput(writer));

      table = ((MessageParser)format.getParser().getInstance(m_context)).parse(new StringInput(writer.toString()), tableMessage);
      rows = (List)table.getValue("Row");
      row = (TransferObject)rows.get(0);
      assertEquals(row.getValue("value"), "abcdef");
      
      Message recursiveMessage = (Message)messageRegistry.get("recursiveElement");
      
      TransferObject recursiveElement = new TransferObject();
      TransferObject recursiveElement2 = new TransferObject();
      recursiveElement.setValue("recursiveElement2", recursiveElement2);
      recursiveElement2.setValue("recursiveElement", new TransferObject());

      ((MessageFormatter)format.getFormatter().getInstance(m_context))
         .format(recursiveElement, recursiveMessage, new WriterOutput(new StringWriter()));
   }
   
   public void testInvalidMessageNames() throws Exception
   {
      Map messageRegistry = XSDMessageImporterTest.getMessageRegistry(MessageAdapterTest.class.getResource("invalidmessagenames.xsd"));

      Message tableMessage = (Message)messageRegistry.get("Table");      

      StringWriter writer = new StringWriter();
      XMLMetadataExporter exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(tableMessage);
      
      TransferObject table = new TransferObject();
      TransferObject row = new TransferObject();
      table.setValue("a_b", row);
      row.setValue("isConfirmedBySourceOwner", Boolean.FALSE);

      Format format = m_metadata.getFormat("XML");
      
      writer = new StringWriter();
      ((MessageFormatter)format.getFormatter().getInstance(m_context)).format(table, tableMessage, new WriterOutput(writer));
      
      table = ((MessageParser)format.getParser().getInstance(m_context))
         .parse(new StringInput(writer.toString()), tableMessage);
      row = (TransferObject)table.getValue("a_b");
      assertEquals(row.getValue("isConfirmedBySourceOwner"), Boolean.FALSE);
   }
}
