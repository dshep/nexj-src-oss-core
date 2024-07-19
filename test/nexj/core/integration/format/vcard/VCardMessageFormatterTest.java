// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.vcard;

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.util.ArrayList;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.format.vcard.VCardMessageFormatter.WrappingWriter;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Binary;
import nexj.core.util.QuotedPrintableUtil;

import junit.framework.TestCase;

/**
 * Tests the vCard format message formatter.
 */
public class VCardMessageFormatterTest extends TestCase
{
   // attributes

   /**
    * The vCard formatter instance to test.
    */
   protected MessageFormatter m_formatter;

   /**
    * The vCard format definition.
    */
   protected Format m_format;

   /**
    * Writer for formatted result.
    */
   protected StringWriter m_writer;


   // operations

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      m_format = Repository.getMetadata().getFormat("vCard");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(null);
      m_writer = new StringWriter();
   }

   /**
    * Tests formatting of a message with vCard v2.1 settings.
    */
   public void testFormatV21Message() throws Exception
   {
      Message message = Repository.getMetadata().getMessage("VCard_v21");
      TransferObject root = new TransferObject("VCard_v21");
      TransferObject tobj;

      root.setValue("fullName", "Sherlock Holmes");
      root.setValue("END", "vCard");

      ArrayList companyInfo = new ArrayList(2);

      companyInfo.add("NexJ Systems Inc.");
      companyInfo.add("R&D");
      root.setValue("company", companyInfo);

      tobj = new TransferObject();
      root.setValue("telephone", tobj);
      tobj.setValue("number", "555-1234");

      ArrayList valList = new ArrayList();
      ArrayList p1List = new ArrayList();
      ArrayList p2List = new ArrayList();

      tobj = new TransferObject();
      root.setValue("collection", tobj);
      tobj.setValue("value", valList);
      tobj.setValue("p1", p1List);
      tobj.setValue("p2", p2List);
      valList.add("abc");
      valList.add("def");
      valList.add("g,hi");
      p1List.add(Primitive.createInteger(1));
      p1List.add(Primitive.createInteger(1));
      p1List.add(Primitive.createInteger(2));
      p1List.add(Primitive.createInteger(3));
      p1List.add(Primitive.createInteger(5));
      p1List.add(Primitive.createInteger(8));
      p2List.add(Primitive.createInteger(0));
      tobj.setValue("p4", "123;456:789");

      tobj = new TransferObject();
      root.setValue("logicalGrouping", tobj);
      tobj.setValue("TEL", "555-0000");
      tobj.setValue("NOTE", "Evenings and weekends only.");

      ArrayList containedSimpleNameList = new ArrayList();

      tobj = new TransferObject();
      root.setValue("containedVCardSimple", tobj);
      tobj.setValue("N", containedSimpleNameList);
      containedSimpleNameList.add("Watson");
      containedSimpleNameList.add("");
      containedSimpleNameList.add("");
      containedSimpleNameList.add("Dr.");

      tobj = new TransferObject();
      root.setValue("containedVCardRef", tobj);
      tobj.setValue("fullName", "Moriarty");

      ArrayList telephonesList = new ArrayList();

      root.setValue("telephones", telephonesList);
      tobj = new TransferObject();
      telephonesList.add(tobj);
      tobj.setValue("number", "This is a really long line....12345678941234567895123456789612345678971234567 89");
      tobj = new TransferObject();
      telephonesList.add(tobj);
      tobj.setValue("number", "(416) 967-1111");
      tobj.setValue("kind", "PIZZA");

      ArrayList addressList = new ArrayList(1);
      ArrayList addressInfoList = new ArrayList(7);

      root.setValue("addresses", addressList);
      tobj = new TransferObject(2);
      addressList.add(tobj);
      tobj.setValue("address", addressInfoList);
      addressInfoList.add(0, null);
      addressInfoList.add(1, null);
      addressInfoList.add(2, "4101 Yonge Street\r\nUnit 501");
      addressInfoList.add(3, "North York");
      addressInfoList.add(4, "ON");
      addressInfoList.add(5, "111111");
      addressInfoList.add(6, "Canada");
      tobj.setValue("type", "WORK");

      tobj = new TransferObject(1);
      root.setValue("logo", tobj);
      tobj.setValue("data", Binary.parse("313233414243"));

      tobj = new TransferObject(1);
      root.setValue("trueName", tobj);
      tobj.setValue("name", "Pr\u00E6tor");

      tobj = new TransferObject(1);
      root.setValue("assistant1", tobj);
      tobj.setValue("card", tobj = new TransferObject(1));
      tobj.setValue("FN", "Dr. John H. Watson");

      tobj = new TransferObject(1);
      root.setValue("assistant2", tobj);
      tobj.setValue("card", tobj = new TransferObject(1));
      tobj.setValue("fullName", "Sir Arthur Conan Doyle");

      m_formatter.format(root, message, new WriterOutput(m_writer));
      assertEquals("BEGIN:VCARD\r\n" +
            "VERSION:2.1\r\n" +
            "FN:Sherlock Holmes\r\n" +
            "ORG:NexJ Systems Inc.;R&D\r\n" +
            "TEL;TYPE=VOICE:555-1234\r\n" +
            "X-NexJ-Collection;p1=1,1,2,3,5,8;p2=0;p3=4,9,16;p4=123\\;456\\:789:abc,def,g\\,hi\r\n" +
            "Home.TEL:555-0000\r\n" +
            "Home.NOTE:Evenings and weekends only.\r\n" +
            "BEGIN:VCARD_CONTAINED\r\n" +
            "VERSION:2.1\r\n" +
            "N:Watson,,,Dr.\r\n" +
            "END:VCARD_CONTAINED\r\n" +
            "BEGIN:VCARD\r\n" +
            "VERSION:2.1\r\n" +
            "FN:Moriarty\r\n" +
            "END:VCARD\r\n" +
            "TEL;TYPE=VOICE:This is a really long\r\n" +
            " line....12345678941234567895123456789612345678971234567 89\r\n" +
            "TEL;TYPE=PIZZA:(416) 967-1111\r\n" +
            "ADR;TYPE=WORK;ENCODING=QUOTED-PRINTABLE:;;4101 Yonge Street=0D=0AUnit 501;N=\r\n" +
            "orth York;ON;111111;Canada\r\n" +
            "LOGO;TYPE=PNG;ENCODING=BASE64:MTIzQUJD\r\n" +
            "\r\n" +
            "X-NexJ-IntlName;CHARSET=ISO-8859-1;ENCODING=QUOTED-PRINTABLE:Pr=E6tor\r\n" +
            "AGENT;NUM=1:BEGIN:VCARD\r\n" +
            "VERSION:2.1\r\n" +
            "FN:Dr. John H. Watson\r\n" +
            "END:VCARD\r\n" +
            "AGENT;NUM=2:BEGIN:VCARD\r\n" +
            "VERSION:2.1\r\n" +
            "FN:Sir Arthur Conan Doyle\r\n" +
            "END:VCARD\r\n" +
            "END:vCard\r\n",
            m_writer.toString());

      try
      {
         companyInfo.add("extra");
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.maxPartCount", ex.getErrorCode());
         assertEquals("VCard_v21 company", ex.getErrorArgs()[0]);
      }
      finally
      {
         companyInfo.remove(2);
      }

      try
      {
         p2List.add(Primitive.createInteger(42));
         p2List.add(Primitive.createInteger(21));
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.maxPartCount", ex.getErrorCode());
         assertEquals("VCard_v21 collection p2", ex.getErrorArgs()[0]);
      }
      finally
      {
         p2List.remove(2);
         p2List.remove(1);
      }

      try
      {
         root.removeValue("fullName");
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("VCard_v21 fullName", ex.getErrorArgs()[0]);
      }
      finally
      {
         root.setValue("fullName", "Sherlock Holmes");
      }

      try
      {
         valList.remove(2);
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("VCard_v21 collection value", ex.getErrorArgs()[0]);
      }
      finally
      {
         valList.add("ghi");
      }

      try
      {
         tobj = (TransferObject)root.getValue("collection");
         tobj.removeValue("value");
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("VCard_v21 collection value", ex.getErrorArgs()[0]);
      }
      finally
      {
         tobj.setValue("value", valList);
      }

      try
      {
         tobj = (TransferObject)root.getValue("collection");
         tobj.removeValue("p1");
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("VCard_v21 collection p1", ex.getErrorArgs()[0]);
      }
      finally
      {
         tobj.setValue("p1", p1List);
      }
   }

   /**
    * Tests formatting of a message with vCard v3.0 settings.
    */
   public void testFormatV30Message() throws Exception
   {
      Message message = Repository.getMetadata().getMessage("VCard_v30");
      TransferObject root = new TransferObject("VCard_v30");
      TransferObject tobj;

      root.setValue("fullName", "Sherlock Holmes");
      root.setValue("END", "vCard");

      ArrayList companyInfo = new ArrayList(2);

      companyInfo.add("NexJ Systems Inc.");
      companyInfo.add("R&D");
      root.setValue("company", companyInfo);

      ArrayList telephonesList = new ArrayList();

      root.setValue("telephones", telephonesList);
      tobj = new TransferObject();
      telephonesList.add(tobj);
      tobj.setValue("number", "This is a really long line....12345678941234567895123456789612345678971234567 89");
      tobj = new TransferObject();
      telephonesList.add(tobj);
      tobj.setValue("number", "(416) 967-1111");
      tobj.setValue("kind", "PIZZA");

      tobj = new TransferObject(1);
      root.setValue("logo", tobj);
      tobj.setValue("data", Binary.parse("313233414243000000000000000000000000000000000000000000000000000000000000"));

      tobj = new TransferObject(1);
      root.setValue("trueName", tobj);
      tobj.setValue("name", "Pr\u00E6tor");

      root.setValue("assistant1", "BEGIN:VCARD\r\nFN:Joe Friday\r\n" +
            "TEL:+1-919-555-7878\r\nTITLE:Area Administrator, Assistant\r\nEND:VCARD\r\n");

      m_formatter.format(root, message, new WriterOutput(m_writer));
      assertEquals("BEGIN:VCARD\r\n" +
            "VERSION:3.0\r\n" +
            "FN:Sherlock Holmes\r\n" +
            "ORG:NexJ Systems Inc.;R&D\r\n" +
            "TEL;TYPE=VOICE:This is a really long line....1234567894123456789512345678\r\n" +
            " 9612345678971234567 89\r\n" +
            "TEL;TYPE=PIZZA:(416) 967-1111\r\n" +
            "LOGO;TYPE=PNG;ENCODING=BASE64:MTIzQUJDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\r\n" +
               " AAAAA\r\n" +
            "\r\n" +
            "X-NexJ-IntlName;CHARSET=ISO-8859-1;ENCODING=B:UHLmdG9y\r\n" +
            "\r\n" +
            "AGENT:BEGIN\\:VCARD\\nFN\\:Joe Friday\\nTEL\\:+1-919-555-7878\\nTITLE\\:Area Adm\r\n" +
               " inistrator\\, Assistant\\nEND\\:VCARD\\n\r\n" +
            "END:vCard\r\n",
            m_writer.toString());

      try
      {
         companyInfo.add("extra");
         m_writer = new StringWriter();
         m_formatter.format(root, message, new WriterOutput(m_writer));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.maxPartCount", ex.getErrorCode());
         assertEquals("VCard_v30 company", ex.getErrorArgs()[0]);
      }
      finally
      {
         companyInfo.remove(2);
      }
   }

   /**
    * Tests the wrapping writer.
    */
   public void testWrappingWriter() throws Exception
   {
      StringWriter writer;
      WrappingWriter wrapper;

      // Test wrapping anywhere
      writer = new StringWriter();
      wrapper = new WrappingWriter(writer);
      wrapper.setWrappingMode(WrappingWriter.WRAP_ANYWHERE);
      wrapper.setMaxLineLength(10);
      wrapper.write("1234567891123456789212345678931234567894");
      wrapper.close();
      assertEquals("1234567891\r\n" +
            " 123456789\r\n" +
            " 212345678\r\n" +
            " 931234567\r\n" +
            " 894",
            writer.toString());


      // Test wrapping only on whitespace
      writer = new StringWriter();
      wrapper = new WrappingWriter(writer);
      wrapper.setWrappingMode(WrappingWriter.WRAP_REPLACE_WHITESPACE);
      wrapper.setMaxLineLength(10);
      wrapper.write("1234567891 newline here!");
      wrapper.close();
      assertEquals("1234567891\r\n" +
            " newline\r\n" +
            " here!",
            writer.toString());

      writer = new StringWriter();
      wrapper = new WrappingWriter(writer);
      wrapper.setWrappingMode(WrappingWriter.WRAP_REPLACE_WHITESPACE);
      wrapper.setMaxLineLength(10);
      wrapper.write("123456789 1 newline here!");
      wrapper.close();
      assertEquals("123456789\r\n" +
            " 1 newline\r\n" +
            " here!",
            writer.toString());


      // Test switching wrapping modes
      writer = new StringWriter();
      wrapper = new WrappingWriter(writer);
      wrapper.setWrappingMode(WrappingWriter.WRAP_REPLACE_WHITESPACE);
      wrapper.setMaxLineLength(10);
      wrapper.write("1234567891 12345");
      wrapper.setWrappingMode(WrappingWriter.WRAP_ANYWHERE);
      wrapper.write("678921234");
      wrapper.setWrappingMode(WrappingWriter.WRAP_REPLACE_WHITESPACE);
      wrapper.write(" a bc def ghij klmno pqrstu");
      wrapper.close();
      assertEquals("1234567891\r\n" +
            " 123456789\r\n" +
            " 21234 a\r\n" +
            " bc def\r\n" +
            " ghij\r\n" +
            " klmno\r\n" +
            " pqrstu",
            writer.toString());

      // Test line with no spaces, longer than maximum length
      writer = new StringWriter();
      wrapper = new WrappingWriter(writer);
      wrapper.setWrappingMode(WrappingWriter.WRAP_REPLACE_WHITESPACE);
      wrapper.setMaxLineLength(10);
      wrapper.write("123456789112345 1234567891");
      wrapper.close();
      assertEquals("123456789112345\r\n" +
            " 1234567891",
            writer.toString());
   }

   /**
    * Tests the RFC 1521 MIME Quoted-Printable encoder.
    */
   public void testQuotedPrintableEncoder() throws Exception
   {
      StringWriter writer = new StringWriter();
      int nLength;

      nLength = QuotedPrintableUtil.encode(new ByteArrayInputStream(
         "Testing 123 1+1=2 \n 12345678931234567894123456789512345678961234567897123456789".getBytes("ISO-8859-1")
         ), writer, 0);

      assertEquals(8, nLength);
      assertEquals("Testing 123 1+1=3D2 =0A 123456789312345678941234567895123456789612345678971=\r\n23456789",
         writer.toString());
   }
}
