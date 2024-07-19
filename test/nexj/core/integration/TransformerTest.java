// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.StringWriter;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Message;
import nexj.core.persistence.OID;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Pair;
import nexj.core.util.Binary;
import nexj.core.util.Logger;

public class TransformerTest extends TestCase
{
   // associations

   protected Metadata m_metadata;
   protected Transformer m_tf;
   protected InvocationContext m_context;

   // operations

   protected void setUp() throws Exception
   {
      super.setUp();

      m_metadata = Repository.getMetadata();
      m_context = new InvocationContext(m_metadata);
      ThreadContextHolder.setContext(m_context);
      m_tf = new Transformer(m_context);
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_context.complete(false);
      ThreadContextHolder.setContext(null);
      super.tearDown();

      m_metadata = null;
      m_tf = null;
      m_context = null;
   }

   public void testTwoMessagesToCollection()
   {
      TransferObject msg = new TransferObject("MsgCol", 2);
      TransferObject tobj = new TransferObject(2);

      tobj.setValue("s", "q");
      tobj.setValue("n", new Integer(1));

      msg.setValue("a", tobj);

      tobj = new TransferObject(2);

      tobj.setValue("s", "r");
      tobj.setValue("n", new Integer(2));
      
      msg.setValue("b", tobj);

      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("2MsgToCol"));

      assertEquals("MsgCol", res.getClassName());
      assertEquals(1, res.getValueCount());
      
      List list = (List)res.getValue("c");

      assertEquals(2, list.size());

      tobj = (TransferObject)list.get(0);

      assertEquals(2, tobj.getValueCount());
      assertEquals("q", tobj.getValue("s"));
      assertEquals(new Integer(1), tobj.getValue("n"));

      tobj = (TransferObject)list.get(1);

      assertEquals(2, tobj.getValueCount());
      assertEquals("r", tobj.getValue("s"));
      assertEquals(new Integer(2), tobj.getValue("n"));
   }
   
   public void testNullAttributes()
   {
      TransferObject msg = new TransferObject("MsgCol", 2);
      TransferObject tobj = new TransferObject(2);

      tobj.setValue("s", "q");
      tobj.setValue("n", null);
      msg.setValue("a", tobj);

      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("NullAttributes"));

      assertEquals("MsgCol", res.getClassName());
      assertEquals(2, res.getValueCount());

      tobj = (TransferObject)res.getValue("a");

      assertEquals(2, tobj.getValueCount());
      assertEquals("maybe null", tobj.getValue("s"));
      assertEquals(new Integer(0), tobj.getValue("n"));

      tobj = (TransferObject)res.getValue("b");

      assertEquals(1, tobj.getValueCount());
      assertEquals("not null", tobj.getValue("s"));
      assertNull(tobj.findValue("n"));
   }

   public void testDefaultAttributes()
   {
      TransferObject msg = new TransferObject("MsgCol", 2);
      TransferObject tobj = new TransferObject(2);

      msg.setValue("a", tobj);

      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("DefaultAttributes"));

      assertEquals("MsgCol", res.getClassName());
      assertEquals(2, res.getValueCount());

      tobj = (TransferObject)res.getValue("a");

      assertEquals(2, tobj.getValueCount());
      assertNull(tobj.getValue("s"));
      assertEquals(new Integer(1), tobj.getValue("n"));

      List list = (List)res.getValue("c");

      assertEquals(1, list.size());

      tobj = (TransferObject)list.get(0);

      assertEquals(1, tobj.getValueCount());
      assertEquals("q", tobj.getValue("s"));
   }
   
   public void testTransformManyToOne()
   {
      TransferObject msg = new TransferObject("MsgCol", 2);
      TransferObject tobj = new TransferObject(2);

      tobj.setValue("s", "q");
      tobj.setValue("n", new Integer(1));

      msg.setValue("a", tobj);

      tobj = new TransferObject(2);

      tobj.setValue("s", "r");
      tobj.setValue("n", new Integer(2));
      
      msg.setValue("b", tobj);

      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("2MsgToColSimple"));

      assertEquals("MsgCol", res.getClassName());
      assertEquals(1, res.getValueCount());
      
      List list = (List)res.getValue("c");

      assertEquals(2, list.size());

      tobj = (TransferObject)list.get(0);

      assertEquals(2, tobj.getValueCount());
      assertEquals("q", tobj.getValue("s"));
      assertEquals(new Integer(1), tobj.getValue("n"));

      tobj = (TransferObject)list.get(1);

      assertEquals(2, tobj.getValueCount());
      assertEquals("r", tobj.getValue("s"));
      assertEquals(new Integer(2), tobj.getValue("n"));
   }
   
   public void testTransformPrimitives()
   {
      // build source message
      TransferObject msg = new TransferObject("TransformTestMsg", 4);
      TransferObject msg1 = null;
      ArrayList col = null;
      ArrayList col1 = null;
      
      msg.setValue("primitive", new Integer(0));
      col = new ArrayList(2);
      col.add(new Integer(4));
      col.add(new Integer(5));
      msg.setValue("primitiveCollection", col);
      
      msg1 = new TransferObject(2);
      msg1.setValue("primitive", new Integer(10));
      col = new ArrayList(2);
      col.add(new Integer(14));
      col.add(new Integer(15));
      msg1.setValue("primitiveCollection", col);
      
      msg.setValue("message", msg1);
      
      col1 = new ArrayList(2);
      
      msg1 = new TransferObject(2);
      msg1.setValue("primitive", new Integer(20));
      col = new ArrayList(2);
      col.add(new Integer(24));
      col.add(new Integer(25));
      msg1.setValue("primitiveCollection", col);
      
      col1.add(msg1);
      
      msg1 = new TransferObject(2);
      msg1.setValue("primitive", new Integer(30));
      col = new ArrayList(2);
      col.add(new Integer(34));
      col.add(new Integer(35));
      msg1.setValue("primitiveCollection", col);
      
      col1.add(msg1);
      
      msg.setValue("messageCollection", col1);

      // transform message
      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("MapPrimitive"));

      // validate result message
      assertEquals("TransformTestMsg", res.getClassName());
      assertEquals(4, res.getValueCount());
      
      assertEquals(new Integer(0), res.getValue("primitive"));
      col = (ArrayList)res.getValue("primitiveCollection");
      assertEquals(4, col.size());
      assertEquals(new Integer(0), col.get(0));
      assertEquals(new Integer(1), col.get(1));
      assertEquals(new Integer(2), col.get(2));
      assertEquals(new Integer(3), col.get(3));
      
      msg = (TransferObject)res.getValue("message");
      
      assertEquals(2, msg.getValueCount());
      
      assertEquals(new Integer(15), msg.getValue("primitive"));
      col = (ArrayList)msg.getValue("primitiveCollection");
      
      assertEquals(4, col.size());
      assertEquals(new Integer(14), col.get(0));
      assertEquals(new Integer(14), col.get(1));
      assertEquals(new Integer(15), col.get(2));
      assertEquals(new Integer(15), col.get(3));

      col = (ArrayList)res.getValue("messageCollection");
      assertEquals(2, col.size());
      
      msg = (TransferObject)col.get(0);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(new Integer(25), msg.getValue("primitive"));
      col1 = (ArrayList)msg.getValue("primitiveCollection");
      assertEquals(3, col1.size());
      assertEquals(new Integer(24), col1.get(0));
      assertEquals(new Integer(25), col1.get(1));
      assertEquals(new Integer(20), col1.get(2));
      
      msg = (TransferObject)col.get(1);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(new Integer(35), msg.getValue("primitive"));
      col1 = (ArrayList)msg.getValue("primitiveCollection");
      assertEquals(3, col1.size());
      assertEquals(new Integer(34), col1.get(0));
      assertEquals(new Integer(35), col1.get(1));
      assertEquals(new Integer(30), col1.get(2));
   }
   
   public void testTransformWithPrimitiveIndexing()
   {
      // build source message
      TransferObject msg = new TransferObject("TransformTestMsg", 4);
      TransferObject msg1 = null;
      ArrayList col = null;
      
      col = new ArrayList(2);
      
      msg1 = new TransferObject(2);
      msg1.setValue("primitive", Primitive.createInteger(1));
      msg1.setValue("primitive2", Primitive.createInteger(1));
      
      col.add(msg1);
      
      msg1 = new TransferObject(2);
      msg1.setValue("primitive", Primitive.createInteger(1));
      msg1.setValue("primitive2", Primitive.createInteger(2));
      
      col.add(msg1);
      
      msg.setValue("messageCollection", col);

      // transform message
      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("MapWithPrimitiveIndexing"));

      // validate result message
      assertEquals("TransformTestMsg", res.getClassName());
      assertEquals(1, res.getValueCount());

      col = (ArrayList)res.getValue("messageCollection");
      assertEquals(4, col.size());
      
      msg = (TransferObject)col.get(0);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(Primitive.createInteger(1), msg.getValue("primitive"));
      assertEquals(Primitive.createInteger(2), msg.getValue("primitive2"));
      
      msg = (TransferObject)col.get(1);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(Primitive.createInteger(1), msg.getValue("primitive"));
      assertEquals(Primitive.createInteger(3), msg.getValue("primitive2"));
      
      msg = (TransferObject)col.get(2);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(Primitive.createInteger(1), msg.getValue("primitive"));
      assertEquals(Primitive.createInteger(2), msg.getValue("primitive2"));
      
      msg = (TransferObject)col.get(3);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(Primitive.createInteger(2), msg.getValue("primitive"));
      assertEquals(Primitive.createInteger(4), msg.getValue("primitive2"));
   }
   
   public void testTransformSingleMessage()
   {
      // build source message
      TransferObject msg = new TransferObject("TransformTestMsg", 1);
      TransferObject msg1 = null;
      ArrayList col = null;
      ArrayList col1 = null;
      
      msg1 = new TransferObject(2);
      msg1.setValue("primitive", new Integer(0));
      col = new ArrayList(2);
      col.add(new Integer(4));
      col.add(new Integer(5));
      msg1.setValue("primitiveCollection", col);
      
      msg.setValue("message", msg1);
      
      Logger logger = Logger.getLogger("TransformerTest");
      
      logger.info("source: " + msg.toString());
      
      // transform message
      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("MapMessage"));

      logger.info("result: " + res.toString());
      
      // validate result message
      assertEquals("TransformTestMsg", res.getClassName());
      assertEquals(2, res.getValueCount());
      
      msg = (TransferObject)res.getValue("message");
      
      assertEquals(2, msg.getValueCount());
      
      assertEquals(new Integer(0), msg.getValue("primitive"));
      col = (ArrayList)msg.getValue("primitiveCollection");
      
      assertEquals(2, col.size());
      assertEquals(new Integer(4), col.get(0));
      assertEquals(new Integer(5), col.get(1));

      col = (ArrayList)res.getValue("messageCollection");
      assertEquals(4, col.size());
      
      msg = (TransferObject)col.get(0);
      assertEquals(2, msg.getValueCount());
      
      assertEquals(new Integer(0), msg.getValue("primitive"));
      col1 = (ArrayList)msg.getValue("primitiveCollection");
      assertEquals(2, col1.size());
      assertEquals(new Integer(4), col1.get(0));
      assertEquals(new Integer(5), col1.get(1));
      
      msg = (TransferObject)col.get(1);
      assertEquals(1, msg.getValueCount());
      assertEquals(new Integer(1), msg.getValue("primitive"));
      
      msg = (TransferObject)col.get(2);
      assertEquals(1, msg.getValueCount());
      assertEquals(new Integer(2), msg.getValue("primitive"));
      
      msg = (TransferObject)col.get(3);
      assertEquals(1, msg.getValueCount());
      assertEquals(new Integer(3), msg.getValue("primitive"));
   }

   public void testTransformMessageCollection()
   {
      // build source message
      TransferObject msg = new TransferObject("TransformTestMsg", 1);
      TransferObject msg1 = null;
      TransferObject msg2 = null;
      ArrayList col = null;
      ArrayList col1 = null;
      
      msg1 = new TransferObject(1);
      col = new ArrayList(2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(10));
      col.add(msg2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(20));
      col.add(msg2);
      msg1.setValue("messageCollection", col);
      msg.setValue("message", msg1);
      
      col = new ArrayList(2);
      msg1 = new TransferObject(2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(30));
      msg1.setValue("message", msg2);
      col1 = new ArrayList(2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(40));
      col1.add(msg2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(50));
      col1.add(msg2);
      msg1.setValue("messageCollection", col1);
      col.add(msg1);
      
      msg1 = new TransferObject(2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(60));
      msg1.setValue("message", msg2);
      col1 = new ArrayList(2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(70));
      col1.add(msg2);
      msg2 = new TransferObject(1);
      msg2.setValue("primitive", new Integer(80));
      col1.add(msg2);
      msg1.setValue("messageCollection", col1);
      col.add(msg1);
      
      msg.setValue("messageCollection", col);
      
      Logger logger = Logger.getLogger("TransformerTest");
      
      logger.info("source: " + msg.toString());
      
      // transform message
      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("MapMessageCollection"));

      logger.info("result: " + res.toString());
      
      // validate result message
      assertEquals("TransformTestMsg", res.getClassName());
      assertEquals(2, res.getValueCount());
      
      msg = (TransferObject)res.getValue("message");
      assertEquals(2, msg.getValueCount());
      
      msg1 = (TransferObject)msg.getValue("message");
      assertEquals(1, msg1.getValueCount());
      assertEquals(new Integer(20), msg1.getValue("primitive"));
      
      col = (ArrayList)msg.getValue("messageCollection");
      assertEquals(3, col.size());
      msg1 = (TransferObject)col.get(0);
      assertEquals(1, msg1.getValueCount());
      assertEquals(new Integer(10), msg1.getValue("primitive"));
      msg1 = (TransferObject)col.get(1);
      assertEquals(1, msg1.getValueCount());
      assertEquals(new Integer(10), msg1.getValue("primitive"));
      msg1 = (TransferObject)col.get(2);
      assertEquals(1, msg1.getValueCount());
      assertEquals(new Integer(20), msg1.getValue("primitive"));
      
      col = (ArrayList)res.getValue("messageCollection");
      assertEquals(2, col.size());
      msg1 = (TransferObject)col.get(0);
      assertEquals(2, msg1.getValueCount());
      msg2 = (TransferObject)msg1.getValue("message");
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(50), msg2.getValue("primitive"));
      col1 = (ArrayList)msg1.getValue("messageCollection");
      assertEquals(3, col1.size());
      msg2 = (TransferObject)col1.get(0);
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(40), msg2.getValue("primitive"));
      msg2 = (TransferObject)col1.get(1);
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(50), msg2.getValue("primitive"));
      msg2 = (TransferObject)col1.get(2);
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(30), msg2.getValue("primitive"));
      
      msg1 = (TransferObject)col.get(1);
      assertEquals(2, msg1.getValueCount());
      msg2 = (TransferObject)msg1.getValue("message");
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(80), msg2.getValue("primitive"));
      col1 = (ArrayList)msg1.getValue("messageCollection");
      assertEquals(3, col1.size());
      msg2 = (TransferObject)col1.get(0);
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(70), msg2.getValue("primitive"));
      msg2 = (TransferObject)col1.get(1);
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(80), msg2.getValue("primitive"));
      msg2 = (TransferObject)col1.get(2);
      assertEquals(1, msg2.getValueCount());
      assertEquals(new Integer(60), msg2.getValue("primitive"));
   }
   
   public void testTransformMessagesToStrings()
   {
      TransferObject msg = new TransferObject("MsgCol", 2);
      TransferObject tobj = new TransferObject(2);

      tobj.setValue("s", "q");
      tobj.setValue("n", new Integer(1));

      msg.setValue("a", tobj);

      tobj = new TransferObject(2);

      tobj.setValue("s", "r");
      tobj.setValue("n", new Integer(2));
      
      msg.setValue("b", tobj);

      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("2MsgToPrimitiveCol"));

      assertEquals("MsgPrimitiveCol", res.getClassName());
      assertEquals(1, res.getValueCount());
      
      List list = (List)res.getValue("c");

      assertEquals(2, list.size());

      tobj = (TransferObject)list.get(0);

      assertEquals(1, tobj.getValueCount());
      assertEquals("q", tobj.getValue("s"));

      tobj = (TransferObject)list.get(1);

      assertEquals(1, tobj.getValueCount());
      assertEquals("r", tobj.getValue("s"));
   }

   public void testHealthCareUseCase()
   {
      TransferObject msg = new TransferObject("HL7_25_ADT_A04");
      TransferObject pid = new TransferObject(3); 

      msg.setValue("patientIdentification", pid);

      List idList = new ArrayList(2);

      pid.setValue("patientIdentifierList", idList);

      TransferObject id = new TransferObject(2);

      id.setValue("idNumber", "1234567890");
      
      TransferObject auth = new TransferObject(1);

      auth.setValue("universalId", "OHIP");

      id.setValue("assigningAuthority", auth);

      idList.add(id);

      id = new TransferObject(2);
      
      id.setValue("idNumber", "234567890");
      
      auth = new TransferObject(1);

      auth.setValue("universalId", "SIN");

      id.setValue("assigningAuthority", auth);

      idList.add(id);

      List nameList = new ArrayList(1);
      
      pid.setValue("patientName", nameList);
      
      TransferObject name = new TransferObject(4);

      name.setValue("givenName", "Joe");
      name.setValue("initials", "F.");
      name.setValue("degree", "Ph.D.");

      TransferObject family = new TransferObject(1);

      family.setValue("surname", "Test");
      name.setValue("familyName", family);
      
      nameList.add(name);

      TransferObject dob = new TransferObject(1);

      dob.setValue("time", new Timestamp(100000));
      pid.setValue("dateTimeOfBirth", dob);
      pid.setValue("administrativeSex", "N");
      
      List addressList = new ArrayList(2);
      
      pid.setValue("patientAddress", addressList);

      TransferObject address = new TransferObject();
      TransferObject street = new TransferObject(1);

      street.setValue("streetAddress", "123 45th St.");
      address.setValue("streetAddress", street);

      address.setValue("otherDesignation", "#456");
      address.setValue("city", "Toronto");
      address.setValue("state", "ON");
      address.setValue("zip", "A1B 2C3");
      address.setValue("country", "Canada");
      address.setValue("addressType", "B");

      addressList.add(address);

      address = new TransferObject();
      street = new TransferObject(1);

      street.setValue("streetAddress", "789 Geek Blvd.");
      address.setValue("streetAddress", street);

      address.setValue("otherDesignation", "basement");
      address.setValue("city", "Richmond Hill");
      address.setValue("state", "ON");
      address.setValue("zip", "M4N 4H5");
      address.setValue("country", "Canada");
      address.setValue("addressType", "H");

      addressList.add(address);

      List phoneList = new ArrayList(2);
      TransferObject phone = new TransferObject(2);

      phone.setValue("telephoneNumber", "(905) 123-4567");
      phone.setValue("emailAddress", "jtest@yahoo.ca");
      phoneList.add(phone);

      phone = new TransferObject(2);

      phone.setValue("telephoneNumber", "(905) 765-4321");
      phone.setValue("emailAddress", "haxx0r16@yahoo.ca");
      phoneList.add(phone);

      pid.setValue("phoneNumberHome", phoneList);

      phoneList = new ArrayList(2);
      phone = new TransferObject(2);

      phone.setValue("telephoneNumber", "(416) 123-4567");
      phone.setValue("emailAddress", "jtest@dot.com");
      phoneList.add(phone);

      phone = new TransferObject(2);

      phone.setValue("telephoneNumber", "(416) 765-4321");
      phone.setValue("emailAddress", "joe.test@dot.com");
      phoneList.add(phone);

      pid.setValue("phoneNumberBusiness", phoneList);
      
      Logger.getLogger(TransformerTest.class).info("src" + msg.toString());

      TransferObject res = m_tf.transform(msg, m_metadata.getTransformation("HL7_25_ADT_A04_Object_PatientDemographics"));

      Logger.getLogger(TransformerTest.class).info("dst" + res.toString());

      
      assertEquals("Object_PatientDemographics", res.getClassName());
      assertEquals(1, res.getValueCount());

      TransferObject player = (TransferObject)res.getValue("player");

      assertEquals(11, player.getValueCount());

      assertEquals("Joe", player.getValue("firstName"));
      assertEquals("F.", player.getValue("initials"));
      assertEquals("Test", player.getValue("lastName"));
      assertEquals("Test, Joe", player.getValue("fullName"));
      assertEquals("Ph.D.", player.getValue("title"));
      assertEquals("Ph.D.", player.getValue("affix"));
      assertEquals(new Timestamp(100000), player.getValue("birthTime"));

      // Validate ids
      
      List list = (List)player.getValue("ids");

      assertEquals(2, list.size());

      TransferObject tobj = (TransferObject)list.get(0);

      assertEquals(2, tobj.getValueCount());
      assertEquals("OHIP", tobj.getValue("type"));
      assertEquals("1234567890", tobj.getValue("id"));

      tobj = (TransferObject)list.get(1);

      assertEquals(2, tobj.getValueCount());
      assertEquals("SIN", tobj.getValue("type"));
      assertEquals("234567890", tobj.getValue("id"));

      // Validate addrs
      
      list = (List)player.getValue("addrs");

      assertEquals(2, list.size());

      tobj = (TransferObject)list.get(0);

      assertEquals(7, tobj.getValueCount());
      assertEquals("Toronto", tobj.getValue("city"));
      assertEquals("123 45th St.", tobj.getValue("address1"));
      assertEquals("#456", tobj.getValue("address2"));
      assertEquals("ON", tobj.getValue("state"));
      assertEquals("Canada", tobj.getValue("country"));
      assertEquals("A1B 2C3", tobj.getValue("zip"));

      tobj = (TransferObject)tobj.getValue("type");

      assertEquals(1, tobj.getValueCount());
      
      tobj = (TransferObject)tobj.getValue("useCodes");
      
      assertEquals(1, tobj.getValueCount());
      assertEquals("WP", tobj.getValue("code"));

      tobj = (TransferObject)list.get(1);

      assertEquals(7, tobj.getValueCount());
      assertEquals("Richmond Hill", tobj.getValue("city"));
      assertEquals("789 Geek Blvd.", tobj.getValue("address1"));
      assertEquals("basement", tobj.getValue("address2"));
      assertEquals("ON", tobj.getValue("state"));
      assertEquals("Canada", tobj.getValue("country"));
      assertEquals("M4N 4H5", tobj.getValue("zip"));

      tobj = (TransferObject)tobj.getValue("type");

      assertEquals(1, tobj.getValueCount());
      
      tobj = (TransferObject)tobj.getValue("useCodes");
      
      assertEquals(1, tobj.getValueCount());
      assertEquals("HP", tobj.getValue("code"));
      
      // Validate telcoms
      
      list = (List)player.getValue("telcoms");

      assertEquals(6, list.size());

      tobj = (TransferObject)list.get(0);
      assertEquals(2, tobj.getValueCount());
      assertEquals("(905) 123-4567", tobj.getValue("address"));

      tobj = (TransferObject)tobj.getValue("type");
      assertEquals(1, tobj.getValueCount());

      List list2 = (List)tobj.getValue("useCodes");

      assertEquals(1, list2.size());

      tobj = (TransferObject)list2.get(0);
      assertEquals(1, tobj.getValueCount());
      assertEquals("HP", tobj.getValue("code"));


      tobj = (TransferObject)list.get(1);
      assertEquals(2, tobj.getValueCount());
      assertEquals("jtest@yahoo.ca", tobj.getValue("address"));

      tobj = (TransferObject)tobj.getValue("type");
      assertEquals(1, tobj.getValueCount());

      list2 = (List)tobj.getValue("useCodes");
      assertEquals(1, list2.size());

      tobj = (TransferObject)list2.get(0);
      assertEquals(1, tobj.getValueCount());
      assertEquals("EMAIL", tobj.getValue("code"));

      
      tobj = (TransferObject)list.get(2);
      assertEquals(2, tobj.getValueCount());
      assertEquals("(905) 765-4321", tobj.getValue("address"));

      tobj = (TransferObject)tobj.getValue("type");
      assertEquals(1, tobj.getValueCount());

      list2 = (List)tobj.getValue("useCodes");
      assertEquals(1, list2.size());

      tobj = (TransferObject)list2.get(0);
      assertEquals(1, tobj.getValueCount());
      assertEquals("HP", tobj.getValue("code"));
      
      
      tobj = (TransferObject)list.get(3);
      assertEquals(2, tobj.getValueCount());
      assertEquals("haxx0r16@yahoo.ca", tobj.getValue("address"));

      tobj = (TransferObject)tobj.getValue("type");
      assertEquals(1, tobj.getValueCount());

      list2 = (List)tobj.getValue("useCodes");
      assertEquals(1, list2.size());

      tobj = (TransferObject)list2.get(0);
      assertEquals(1, tobj.getValueCount());
      assertEquals("EMAIL", tobj.getValue("code"));
      
      
      tobj = (TransferObject)list.get(4);
      assertEquals(2, tobj.getValueCount());
      assertEquals("(416) 123-4567", tobj.getValue("address"));

      tobj = (TransferObject)tobj.getValue("type");
      assertEquals(1, tobj.getValueCount());

      list2 = (List)tobj.getValue("useCodes");
      assertEquals(1, list2.size());

      tobj = (TransferObject)list2.get(0);
      assertEquals(1, tobj.getValueCount());
      assertEquals("WP", tobj.getValue("code"));
      
      
      tobj = (TransferObject)list.get(5);
      assertEquals(2, tobj.getValueCount());
      assertEquals("(416) 765-4321", tobj.getValue("address"));

      tobj = (TransferObject)tobj.getValue("type");
      assertEquals(1, tobj.getValueCount());

      list2 = (List)tobj.getValue("useCodes");
      assertEquals(1, list2.size());

      tobj = (TransferObject)list2.get(0);
      assertEquals(1, tobj.getValueCount());
      assertEquals("WP", tobj.getValue("code"));

      // Validate genderCode
      
      tobj = (TransferObject)player.getValue("genderCode");

      assertEquals(1, tobj.getValueCount());
      assertEquals("N", tobj.getValue("value"));
   }

   /**
    * Tests parsing from a primitive to a composite, within a transformation.
    */
   public void testTransformParsePrimitive()
   {
      Message csvMessage = m_metadata.getMessage("CSVTestSuiteResult");
      MessageParser csvParser = (MessageParser)csvMessage.getFormat().getParser().getInstance(m_context);
      StringInput in;
      TransferObject root, result, sub;
      List list;

      in = new StringInput("date,status,result\n" +
         "2009-09-23T17:34:00,PASS,\"<XMLTestResult seed=\\\"5\\\"><a>aValue</a></XMLTestResult>\"\n" +
         "2009-09-23T17:36:00,FAIL,\"<XMLTestResult><a>aValue2</a><b>bValue</b></XMLTestResult>\"\n"
         );

      root = csvParser.parse(in, csvMessage);
      assertEquals("CSVTestSuiteResult", root.getClassName());
      assertEquals(2, ((List)root.getValue("rows")).size());

      result = m_tf.transform(root, m_metadata.getTransformation("CSVTestSuiteResultToXMLTestSuiteResult"));

      assertEquals("XMLTestSuiteResult", result.getClassName());
      list = (List)result.getValue("tests");
      assertEquals(2, list.size());

      sub = (TransferObject)list.get(0);
      assertEquals(Primitive.toTimestamp("2009-09-23T17:34:00"), sub.getValue("date"));
      assertEquals("PASS", sub.getValue("status"));
      sub = (TransferObject)sub.getValue("data");
      assertEquals("XMLTestResult", sub.getClassName());
      assertEquals("5", sub.getValue("seed"));
      assertEquals("aValue", sub.getValue("a"));
      assertFalse(sub.hasValue("b"));

      sub = (TransferObject)list.get(1);
      assertEquals(Primitive.toTimestamp("2009-09-23T17:36:00"), sub.getValue("date"));
      assertEquals("FAIL", sub.getValue("status"));
      sub = (TransferObject)sub.getValue("data");
      assertEquals("XMLTestResult", sub.getClassName());
      assertEquals("bValue", sub.getValue("b"));
      assertEquals("aValue2", sub.getValue("a"));
      assertFalse(sub.hasValue("seed"));

      // Try using a message that is referred to by a message with no format
      TransferObject outer = new TransferObject(1);

      outer.setValue("csv", root);
      result = m_tf.transform(outer, m_metadata.getTransformation("TestSuiteResult"));

      assertEquals("MultiFormatReferrer", result.getClassName());
      assertFalse(result.hasValue("csv"));
      result = (TransferObject)result.getValue("xml");
      assertEquals("XMLTestSuiteResult", result.getClassName());
      list = (List)result.getValue("tests");
      assertEquals(2, list.size());
   }

   /**
    * Tests formatting from a composite to a primitive, within a transformation.
    * Additionally, tests transformation arguments.
    * Tests using the service TestTransform.
    */
   public void testTransformFormatPrimitive()
   {
      Metaclass serviceClass = m_metadata.getMetaclass(Metadata.SERVICE_CLASS_NAME);
      Message csvMessage = m_metadata.getMessage("CSVTestSuiteResult");
      MessageFormatter formatter = (MessageFormatter)csvMessage.getFormat().getFormatter().getInstance(m_context);
      TransferObject root, test, testData, result;
      List testList;
      StringWriter writer;
      WriterOutput out;

      root = new TransferObject("XMLTestSuiteResult");
      testList = new ArrayList(2);
      root.setValue("tests", testList);
      test = new TransferObject();
      testList.add(test);
      test.setValue("date", Primitive.toTimestamp("2009-12-07T08:00:00"));
      test.setValue("status", "ERROR");
      testData = new TransferObject("XMLTestResult", 3);
      test.setValue("data", testData);
      testData.setValue("seed", "12345");
      testData.setValue("a", "aValue");
      testData.setValue("b", "bValue");

      test = new TransferObject();
      testList.add(test);
      test.setValue("date", Primitive.toTimestamp("2009-08-17T00:00:00"));
      test.setValue("status", "PASS");
      testData = new TransferObject("XMLTestResult", 3);
      test.setValue("data", testData);
      testData.setValue("b", "bValue2");

      Instance instance = (Instance)serviceClass.invoke("invoke", new Object[]{"TestTransform", root, null, "PRE-", Boolean.TRUE});

      assertEquals(Boolean.TRUE, instance.invoke("done"));
      result = (TransferObject)instance.invoke("result");

      assertEquals("CSVTestSuiteResult", result.getClassName());
      testList = (List)result.getValue("rows");
      assertEquals(2, testList.size());

      writer = new StringWriter();
      out = new WriterOutput(writer);
      formatter.format(result, csvMessage, out);

      assertEquals("date,status,result\n" +
         "2009-12-07 08:00:00.000000000,PRE-ERROR-POST,\"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\n" +
            "<XMLTestResult seed=\\\"12345\\\"><a>aValue</a><b>bValue</b></XMLTestResult>\"\n" +
         "2009-08-17 00:00:00.000000000,PRE-PASS-POST,\"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\n" +
            "<XMLTestResult><b>bValue2</b></XMLTestResult>\"\n",
         writer.toString());

      // Try using a message that is referred to by a message with no format
      TransferObject outer = new TransferObject(1);

      outer.setValue("xml", root);
      result = m_tf.transform(outer, m_metadata.getTransformation("TestSuiteResult"));

      assertEquals("MultiFormatReferrer", result.getClassName());
      assertFalse(result.hasValue("xml"));
      result = (TransferObject)result.getValue("csv");
      assertEquals("CSVTestSuiteResult", result.getClassName());
      testList = (List)result.getValue("rows");
      assertEquals(2, testList.size());

      writer = new StringWriter();
      out = new WriterOutput(writer);
      formatter.format(result, csvMessage, out);

      assertEquals("date,status,result\n" +
         "2009-12-07 08:00:00.000000000,PREFIXERRORAFFIX,\"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\n" +
            "<XMLTestResult seed=\\\"12345\\\"><a>aValue</a><b>bValue</b></XMLTestResult>\"\n" +
         "2009-08-17 00:00:00.000000000,PREFIXPASSAFFIX,\"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\n" +
            "<XMLTestResult><b>bValue2</b></XMLTestResult>\"\n",
         writer.toString());
   }

   /**
    * Tests a recursive transformation with arguments.
    */
   public void testTransformRecursive()
   {
      TransferObject inRoot, in2, in3;
      TransferObject outRoot, outNext;
      List col;

      inRoot = new TransferObject("XML_LinkedList", 2);
      inRoot.setValue("data", "first");
      in2 = new TransferObject("XML_LinkedList_Sub", 3);
      inRoot.setValue("next", in2);
      in2.setValue("data", "second");
      in2.setValue("count", Primitive.createInteger(7));
      in3 = new TransferObject("XML_LinkedList", 1);
      in2.setValue("next", in3);
      in3.setValue("data", "third");

      outRoot = m_tf.transform(inRoot, m_metadata.getTransformation("XMLLinkedListModify"), Pair.list("A", "B"));

      // First item in list
      assertEquals("XML_LinkedList", outRoot.getClassName());
      assertEquals("AfirstB", outRoot.getValue("data"));
      assertEquals("AfirstB", outRoot.getValue("dataCopy_InheritMapping"));
      assertEquals("AfirstB", outRoot.getValue("dataCopy_ReplaceMapping"));
      assertEquals("AfirstB", outRoot.getValue("dataCopy_Polymorphic"));
      assertFalse(outRoot.hasValue("dataCopy_ReferenceParentMapping"));
      assertEquals("AfirstB", outRoot.getValue("dataCopy"));
      col = (List)outRoot.getValue("dataCopy_Collection");
      assertEquals(1, col.size());
      assertEquals("AfirstB", col.get(0));

      // Second item in list
      outNext = (TransferObject)outRoot.getValue("next");
      assertEquals("XML_LinkedList_Sub", outNext.getClassName());
      assertEquals("AAsecondB", outNext.getValue("data"));
      assertEquals("AAsecondB", outNext.getValue("dataCopy_InheritMapping"));
      assertEquals("Extra:AAsecondB", outNext.getValue("dataCopy_ReplaceMapping"));
      assertEquals("Extra:AAsecondB", outNext.getValue("dataCopy_Polymorphic"));
      assertEquals("AAsecondB", outNext.getValue("dataCopy_ReferenceParentMapping"));
      col = (List)outNext.getValue("dataCopy");
      assertEquals(2, col.size());
      assertEquals("AAsecondB", col.get(0));
      assertEquals("Extra:AAsecondB", col.get(1));
      col = (List)outNext.getValue("dataCopy_Collection");
      assertEquals(2, col.size());
      assertEquals("AAsecondB", col.get(0));
      assertEquals("Extra:AAsecondB", col.get(1));
      assertEquals(49, ((Integer)outNext.getValue("count")).intValue());

      // Third item in list
      outNext = (TransferObject)outNext.getValue("next");
      assertEquals("XML_LinkedList", outNext.getClassName());
      assertEquals("AAAAthirdB", outNext.getValue("data"));
      assertEquals("AAAAthirdB", outNext.getValue("dataCopy_InheritMapping"));
      assertEquals("AAAAthirdB", outNext.getValue("dataCopy_ReplaceMapping"));
      assertEquals("AAAAthirdB", outNext.getValue("dataCopy_Polymorphic"));
      assertFalse(outNext.hasValue("dataCopy_ReferenceParentMapping"));
      assertEquals("AAAAthirdB", outNext.getValue("dataCopy"));
      col = (List)outNext.getValue("dataCopy_Collection");
      assertEquals(1, col.size());
      assertEquals("AAAAthirdB", col.get(0));
      assertFalse(outNext.hasValue("next"));
   }

   /**
    * Transformation from a class instance to an integration message.
    */
   public void testTransformClassSource() throws Exception
   {
      TransferObject tobj, out;
      Instance animal = new Instance(m_metadata.getMetaclass("Animal"), m_context);
      Instance animalPen = new Instance(m_metadata.getMetaclass("AnimalPen"), m_context);

      animal.setOID(new OID(new Object[]{Binary.fromBase64("AAAB")}));
      animal.setValue("commonName", "Test Animal");
      animal.setValue("scientificName", "Testus Animalius");
      animal.setValue("pen", animalPen);
      animalPen.setOID(new OID(new Object[]{"pen1", Primitive.createInteger(2)}));
      animalPen.setValue("name", "Test Pen");

      out = m_tf.transform(animal, m_metadata.getTransformation("AnimalClassToXSDExportCircular"));

      assertEquals("XML_XSDExportCircular1", out.getClassName());
      assertEquals(1, out.getOID().getCount());
      assertEquals("000001", ((Binary)out.getOID().getValue(0)).toString());
      assertEquals("Test Animal", out.getValue("a"));
      tobj = (TransferObject)out.getValue("other");
      assertEquals("XML_XSDExportCircular2", tobj.getClassName());
      assertEquals("Test Pen", tobj.getValue("b"));
      tobj = (TransferObject)tobj.getValue("other");
      assertEquals("XML_XSDExportCircular1", tobj.getClassName());
      assertEquals("Testus Animalius", tobj.getValue("a"));
   }

   /**
    * Transformation from an integration message to a class message.
    */
   public void testTransformClassDestination()
   {
      TransferObject tobj, in, out;

      in = new TransferObject("XML_XSDExportCircular1", 2);
      in.setValue("a", "Test Animal");
      tobj = new TransferObject("XML_XSDExportCircular2", 1);
      in.setValue("other", tobj);
      tobj.setValue("b", "Test Pen");

      out = m_tf.transform(in, m_metadata.getTransformation("XSDExportCircularToAnimalClass"));

      assertEquals("Animal", out.getClassName());
      assertEquals("Test Animal", out.getValue("commonName"));
      tobj = (TransferObject)out.getValue("pen");
      assertEquals("AnimalPen", tobj.getClassName());
      assertEquals("Test Pen", tobj.getValue("name"));
   }

   public void testTransformClassMessageSourceAndDestination()
   {
      TransferObject tobj, in, out;
      List list = new ArrayList(3);

      in = new TransferObject("Animal", 2);
      in.setValue("commonName", "Cat");
      tobj = new TransferObject("AnimalPen", 2);
      in.setValue("pen", tobj);
      tobj.setValue("name", "Animal Farm");
      tobj.setValue("animals", list);

      tobj = new TransferObject("Animal", 1);
      tobj.setValue("commonName", "Kitty");
      list.add(tobj);

      tobj = new TransferObject("Animal", 1);
      tobj.setValue("commonName", "Catty");
      list.add(tobj);

      tobj = new TransferObject("Animal", 1);
      tobj.setValue("commonName", "Doggie");
      list.add(tobj);

      out = m_tf.transform(in, m_metadata.getTransformation("AnimalClassToAnimalClass"));

      assertEquals("Animal", out.getClassName());
      assertEquals("Cat", out.getValue("commonName"));
      tobj = (TransferObject)out.getValue("pen");
      assertEquals("AnimalPen", tobj.getClassName());
      list = (List)tobj.getValue("animals");

      tobj = (TransferObject)list.get(0);
      assertEquals("Animal", tobj.getClassName());
      assertEquals("Friend Kitty from Animal Farm", tobj.getValue("commonName"));

      tobj = (TransferObject)list.get(1);
      assertEquals("Animal", tobj.getClassName());
      assertEquals("Friend Catty from Animal Farm", tobj.getValue("commonName"));

      tobj = (TransferObject)list.get(2);
      assertEquals("Animal", tobj.getClassName());
      assertEquals("Friend Doggie from Animal Farm", tobj.getValue("commonName"));
   }

   public void testTopLevelSource() throws Exception
   {
      TransferObject tobj, in, out;

      in = new TransferObject("XMLTestSuiteResult", 2);
      in.setValue("a", Primitive.createInteger(13));
      in.setValue("b", "data");

      out = m_tf.transform(in, m_metadata.getTransformation("FromRoot"));

      assertEquals("MultiFormatReferrer", out.getClassName());
      tobj = (TransferObject)out.getValue("xml");
      assertSame(in, tobj);
   }
}
