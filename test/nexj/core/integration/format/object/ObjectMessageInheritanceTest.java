// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.Metaclass;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.format.object.ObjectMessagePartMapping;
import nexj.core.persistence.Query;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.scripting.Pair;

public class ObjectMessageInheritanceTest extends SQLDataTest
{
   // associations

   protected Format m_format;
   protected MessageFormatter m_formatter;
   protected MessageParser m_parser;
   private Message m_contactMsg;
   private Message m_patientMsg;
   private Message m_surgeonMsg;
   private Message m_phoneMsg;
   private Message m_contactNonPolyMsg;
   private Message m_patient2Msg;
   private Instance m_contact;
   private Instance m_patient;
   private Instance m_doctor;
   private Instance m_surgeon;

   // constructors

   public ObjectMessageInheritanceTest(String sName)
   {
      super(sName);
   }

   // operations

   protected void setUp() throws Exception
   {
      super.setUp();

      m_format = getMetadata().getFormat("Object");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);

      m_contactMsg = getMetadata().getMessage("Object_Inherit_Contact");
      m_patientMsg = getMetadata().getMessage("Object_Inherit_Patient");
      m_surgeonMsg = getMetadata().getMessage("Object_Inherit_Surgeon");
      m_phoneMsg = getMetadata().getMessage("Object_Inherit_Phone");
      m_contactNonPolyMsg = getMetadata().getMessage("Object_Inherit_Contact_NonPoly");
      m_patient2Msg = getMetadata().getMessage("Object_Inherit_Patient2");

      m_contact = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName)"),
         parse("(= (@ firstName) \"Joe\")"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      m_patient = Query.createRead(getMetadata().getMetaclass("Patient"),
         parse("(firstName lastName middleName)"),
         parse("(and (= (@ firstName) \"Sarah\") (= (@ lastName) \"Johnson\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      m_doctor = Query.createRead(getMetadata().getMetaclass("Doctor"),
         parse("(firstName lastName)"),
         parse("(and (= (@ firstName) \"Johan\") (= (@ lastName) \"Bager\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      m_surgeon = Query.createRead(getMetadata().getMetaclass("Surgeon"),
         parse("(firstName lastName speciality)"),
         parse("(and (= (@ firstName) \"Joshua\") (= (@ lastName) \"Zig\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_format = null;
      m_formatter = null;
      m_parser = null;
      m_contactMsg = null;
      m_patientMsg = null;
      m_surgeonMsg = null;
      m_phoneMsg = null;
      m_contactNonPolyMsg = null;
      m_patient2Msg = null;
      m_contact = null;
      m_patient = null;
      m_doctor = null;
      m_surgeon = null;
   }

   public void testFormatRootInheritance()
   {
      ObjectOutput out;
      TransferObject root;
      Instance result;


      // Format the super-message (no polymorphism)
      root = new TransferObject("Object_Inherit_Contact", 3);
      root.setValue("firstName", "Simon");
      root.setValue("lastName", "Lee");
      root.setValue("middleName", "K");

      out = new ObjectOutput();
      m_formatter.format(root, m_contactMsg, out);
      result = (Instance)out.getObject();
      assertEquals("Contact", result.getMetaclass().getName());
      assertEquals("Simon", result.getValue("firstName"));
      assertNull(result.findValue("lastName"));
      assertNull(result.findValue("middleName"));
      assertEquals(Instance.NEW, result.getState());
      rollback();


      // Format the sub-message (no polymorphism)
      root = new TransferObject("Object_Inherit_Patient", 3);
      root.setValue("firstName", "Simon");
      root.setValue("lastName", "Lee");
      root.setValue("middleName", "K");

      out = new ObjectOutput();
      m_formatter.format(root, m_patientMsg, out);
      result = (Instance)out.getObject();
      assertEquals("Patient", result.getMetaclass().getName());
      assertEquals("Simon", result.getValue("firstName"));
      assertEquals("Lee", result.getValue("lastName"));
      assertEquals("K", result.getValue("middleName"));
      assertEquals(Instance.NEW, result.getState());
      rollback();


      // Format the sub-message polymorphically
      root = new TransferObject("Object_Inherit_Patient", 3);
      root.setValue("firstName", "Simon");
      root.setValue("lastName", "Lee");
      root.setValue("middleName", "K");

      out = new ObjectOutput();
      m_formatter.format(root, m_contactMsg, out);
      result = (Instance)out.getObject();
      assertEquals("Patient", result.getMetaclass().getName());
      assertEquals("Simon", result.getValue("firstName"));
      assertEquals("Lee", result.getValue("lastName"));
      assertEquals("K", result.getValue("middleName"));
      assertEquals(Instance.NEW, result.getState());
      rollback();


      // Format Patient2 with Contact_NonPoly (should fail polymorphic check)
      root = new TransferObject("Object_Inherit_Patient2", 3);
      root.setValue("firstName", "Simon");
      root.setValue("lastName", "Lee");
      root.setValue("middleName", "K");

      out = new ObjectOutput();

      try
      {
         m_formatter.format(root, m_contactNonPolyMsg, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         ex = (IntegrationException)ex.getCause();
         assertEquals("err.integration.messageTypeMismatch", ex.getErrorCode());
         assertEquals("Object_Inherit_Patient2", ex.getErrorArgs()[0]);
         assertEquals("Object_Inherit_Contact_NonPoly", ex.getErrorArgs()[1]);
         assertEquals("Object_Inherit_Patient2", ex.getErrorArgs()[2]);
      }

      rollback();
   }

   public void testParseRootInheritance()
   {
      ObjectInput in;
      TransferObject root;

      // Parse Contact as Object_Inherit_Contact
      in = new ObjectInput(m_contact);
      root = m_parser.parse(in, m_contactMsg);

      assertEquals("Object_Inherit_Contact", root.getClassName());
      assertEquals("Joe", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      // Parse Patient as Object_Inherit_Patient
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, m_patientMsg);

      assertEquals("Object_Inherit_Patient", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertEquals("Johnson", root.getValue("lastName"));
      assertTrue(root.hasValue("middleName"));
      assertNull(root.getValue("middleName"));


      // Parse Patient as Object_Inherit_Contact (polymorphically becomes Object_Inherit_Patient)
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, m_contactMsg);

      assertEquals("Object_Inherit_Patient", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertEquals("Johnson", root.getValue("lastName"));
      assertTrue(root.hasValue("middleName"));
      assertNull(root.getValue("middleName"));


      // Parse Surgeon as Object_Inherit_Contact (polymorphically becomes Object_Inherit_Surgeon)
      in = new ObjectInput(m_surgeon);
      root = m_parser.parse(in, m_contactMsg);

      assertEquals("Object_Inherit_Surgeon", root.getClassName());
      assertEquals("Joshua", root.getValue("firstName"));
      assertEquals("Zig", root.getValue("lastName"));
      assertEquals("ECG", root.getValue("speciality"));
      assertFalse(root.hasValue("middleName"));


      // Parse Doctor as Object_Inherit_Contact (stays as Object_Inherit_Contact)
      in = new ObjectInput(m_doctor);
      root = m_parser.parse(in, m_contactMsg);

      assertEquals("Object_Inherit_Contact", root.getClassName());
      assertEquals("Johan", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      // Parse Doctor as Object_Inherit_Patient (fails)
      in = new ObjectInput(m_doctor);

      try
      {
         root = m_parser.parse(in, m_patientMsg);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         ex = (IntegrationException)ex.getCause();
         assertEquals("err.integration.object.missingMapping", ex.getErrorCode());
      }

      // Parse Surgeon as Object_Inherit_Patient (fails)
      in = new ObjectInput(m_surgeon);

      try
      {
         root = m_parser.parse(in, m_patientMsg);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         ex = (IntegrationException)ex.getCause();
         assertEquals("err.integration.object.missingMapping", ex.getErrorCode());
      }


      // Parse Patient as Object_Inherit_Contact_NonPoly (not polymorphic, so does not become Patient2)
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, m_contactNonPolyMsg);

      assertEquals("Object_Inherit_Contact_NonPoly", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      // Parse Patient as Object_Inherit_Patient2
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, m_patient2Msg);

      assertEquals("Object_Inherit_Patient2", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertEquals("Johnson", root.getValue("lastName"));
      assertTrue(root.hasValue("middleName"));
      assertNull(root.getValue("middleName"));
   }

   public void testParseRootInheritanceWithMessageTable()
   {
      MessageTable table = new MessageTable();
      ObjectInput in;
      TransferObject root;

      table.addMessage(m_patientMsg);
      table.addMessage(m_surgeonMsg);
      m_parser.initializeMessageTable(table);


      // Parse Patient (uses Object_Inherit_Patient)
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Patient", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertEquals("Johnson", root.getValue("lastName"));
      assertTrue(root.hasValue("middleName"));
      assertNull(root.getValue("middleName"));


      // Parse Surgeon (uses Object_Inherit_Surgeon)
      in = new ObjectInput(m_surgeon);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Surgeon", root.getClassName());
      assertEquals("Joshua", root.getValue("firstName"));
      assertEquals("Zig", root.getValue("lastName"));
      assertEquals("ECG", root.getValue("speciality"));
      assertFalse(root.hasValue("middleName"));


      // Parse Contact--error
      try
      {
         in = new ObjectInput(m_contact);
         m_parser.parse(in, table);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.object.unsupportedMessage", ex.getErrorCode());
      }


      // Parse Doctor--error
      try
      {
         in = new ObjectInput(m_doctor);
         m_parser.parse(in, table);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.object.unsupportedMessage", ex.getErrorCode());
      }


      // Change table
      table.addMessage(m_contactMsg);
      m_parser.initializeMessageTable(table);


      // Parse Contact (uses Object_Inherit_Contact)
      in = new ObjectInput(m_contact);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Contact", root.getClassName());
      assertEquals("Joe", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      // Parse Patient (uses Object_Inherit_Patient, polymorphic from Object_Inherit_Contact)
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Patient", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertEquals("Johnson", root.getValue("lastName"));
      assertTrue(root.hasValue("middleName"));
      assertNull(root.getValue("middleName"));


      // Parse Doctor (uses Object_Inherit_Contact)
      in = new ObjectInput(m_doctor);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Contact", root.getClassName());
      assertEquals("Johan", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      // Parse Surgeon (uses Object_Inherit_Surgeon, polymorphic from Object_Inherit_Contact)
      in = new ObjectInput(m_surgeon);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Surgeon", root.getClassName());
      assertEquals("Joshua", root.getValue("firstName"));
      assertEquals("Zig", root.getValue("lastName"));
      assertEquals("ECG", root.getValue("speciality"));
      assertFalse(root.hasValue("middleName"));



      table = new MessageTable();
      table.addMessage(m_contactNonPolyMsg);
      table.addMessage(m_patientMsg);
      m_parser.initializeMessageTable(table);


      // Parse Patient (uses Object_Inherit_Patient from the table)
      in = new ObjectInput(m_patient);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Patient", root.getClassName());
      assertEquals("Sarah", root.getValue("firstName"));
      assertEquals("Johnson", root.getValue("lastName"));
      assertTrue(root.hasValue("middleName"));
      assertNull(root.getValue("middleName"));


      // Parse Surgeon (uses Object_Inherit_Contact_NonPoly from the table)
      in = new ObjectInput(m_surgeon);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Contact_NonPoly", root.getClassName());
      assertEquals("Joshua", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      // Parse Contact (uses Object_Inherit_Contact_NonPoly from the table)
      in = new ObjectInput(m_contact);
      root = m_parser.parse(in, table);

      assertEquals("Object_Inherit_Contact_NonPoly", root.getClassName());
      assertEquals("Joe", root.getValue("firstName"));
      assertFalse(root.hasValue("lastName"));


      try
      {
         table = new MessageTable();
         table.addMessage(m_contactMsg);
         table.addMessage(m_contactNonPolyMsg);
         m_parser.initializeMessageTable(table);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.object.ambiguousTable", ex.getErrorCode());
      }
   }

   public void testFormatReferenceInheritance()
   {
      ObjectOutput out;
      TransferObject phoneRoot, contactRoot;
      Instance result;


      // Format reference with super-message (no polymorphism)
      contactRoot = new TransferObject("Object_Inherit_Contact", 3);
      contactRoot.setValue("firstName", "Simon");
      contactRoot.setValue("lastName", "Lee");
      contactRoot.setValue("middleName", "K");
      phoneRoot = new TransferObject("Object_Inherit_Phone", 3);
      phoneRoot.setValue("type", "cell");
      phoneRoot.setValue("number", "967-1111");
      phoneRoot.setValue("CONTACT", contactRoot);

      out = new ObjectOutput();
      m_formatter.format(phoneRoot, m_phoneMsg, out);
      result = (Instance)out.getObject();
      assertEquals("Phone", result.getMetaclass().getName());
      assertEquals("cell", result.getValue("type"));
      assertEquals("967-1111", result.getValue("number"));
      assertEquals(Instance.NEW, result.getState());
      result = (Instance)result.getValue("contact");

      assertEquals("Contact", result.getMetaclass().getName());
      assertEquals("Simon", result.getValue("firstName"));
      assertNull(result.findValue("lastName"));
      assertNull(result.findValue("middleName"));
      assertEquals(Instance.NEW, result.getState());
      rollback();


      // Format the reference with sub-message (polymorphic!)
      contactRoot = new TransferObject("Object_Inherit_Patient", 3);
      contactRoot.setValue("firstName", "Simon");
      contactRoot.setValue("lastName", "Lee");
      contactRoot.setValue("middleName", "K");
      phoneRoot = new TransferObject("Object_Inherit_Phone", 3);
      phoneRoot.setValue("type", "cell");
      phoneRoot.setValue("number", "967-1111");
      phoneRoot.setValue("CONTACT", contactRoot);

      out = new ObjectOutput();
      m_formatter.format(phoneRoot, m_phoneMsg, out);
      result = (Instance)out.getObject();
      assertEquals("Phone", result.getMetaclass().getName());
      assertEquals("cell", result.getValue("type"));
      assertEquals("967-1111", result.getValue("number"));
      assertEquals(Instance.NEW, result.getState());
      result = (Instance)result.getValue("contact");

      assertEquals("Patient", result.getMetaclass().getName());
      assertEquals("Simon", result.getValue("firstName"));
      assertEquals("Lee", result.getValue("lastName"));
      assertEquals("K", result.getValue("middleName"));
      assertEquals(Instance.NEW, result.getState());
      rollback();
   }

   public void testParseReferenceInheritance()
   {
      Metaclass phoneClass = getMetadata().getMetaclass("Phone");
      Instance phone;
      ObjectInput in;
      TransferObject phoneRoot, contactRoot;

      phone = (Instance)phoneClass.invoke("new");
      phone.setValue("type", "cell");
      phone.setValue("number", "439-0000");

      // Parse reference to Contact (parsed as Object_Inherit_Contact)
      phone.setValue("contact", m_contact);
      in = new ObjectInput(phone);
      phoneRoot = m_parser.parse(in, m_phoneMsg);

      assertEquals("Object_Inherit_Phone", phoneRoot.getClassName());
      assertEquals("cell", phoneRoot.getValue("type"));
      assertEquals("439-0000", phoneRoot.getValue("number"));
      contactRoot = (TransferObject)phoneRoot.getValue("CONTACT");
      assertEquals("Object_Inherit_Contact", contactRoot.getClassName());
      assertEquals("Joe", contactRoot.getValue("firstName"));
      assertFalse(contactRoot.hasValue("lastName"));

      // Parse reference to Patient (polymorphically parsed as Object_Inherit_Patient)
      phone.setValue("contact", m_patient);
      in = new ObjectInput(phone);
      phoneRoot = m_parser.parse(in, m_phoneMsg);

      assertEquals("Object_Inherit_Phone", phoneRoot.getClassName());
      assertEquals("cell", phoneRoot.getValue("type"));
      assertEquals("439-0000", phoneRoot.getValue("number"));
      contactRoot = (TransferObject)phoneRoot.getValue("CONTACT");
      assertEquals("Object_Inherit_Patient", contactRoot.getClassName());
      assertEquals("Sarah", contactRoot.getValue("firstName"));
      assertEquals("Johnson", contactRoot.getValue("lastName"));
      assertTrue(contactRoot.hasValue("middleName"));
      assertNull(contactRoot.getValue("middleName"));

      // Parse reference to Doctor (polymorphically parsed as Object_Inherit_Contact)
      phone.setValue("contact", m_doctor);
      in = new ObjectInput(phone);
      phoneRoot = m_parser.parse(in, m_phoneMsg);

      assertEquals("Object_Inherit_Phone", phoneRoot.getClassName());
      assertEquals("cell", phoneRoot.getValue("type"));
      assertEquals("439-0000", phoneRoot.getValue("number"));
      contactRoot = (TransferObject)phoneRoot.getValue("CONTACT");
      assertEquals("Object_Inherit_Contact", contactRoot.getClassName());
      assertEquals("Johan", contactRoot.getValue("firstName"));
      assertFalse(contactRoot.hasValue("lastName"));

      // Parse reference to Surgeon (polymorphically parsed as Object_Inherit_Surgeon)
      phone.setValue("contact", m_surgeon);
      in = new ObjectInput(phone);
      phoneRoot = m_parser.parse(in, m_phoneMsg);

      assertEquals("Object_Inherit_Phone", phoneRoot.getClassName());
      assertEquals("cell", phoneRoot.getValue("type"));
      assertEquals("439-0000", phoneRoot.getValue("number"));
      contactRoot = (TransferObject)phoneRoot.getValue("CONTACT");
      assertEquals("Object_Inherit_Surgeon", contactRoot.getClassName());
      assertEquals("Joshua", contactRoot.getValue("firstName"));
      assertEquals("Zig", contactRoot.getValue("lastName"));
      assertEquals("ECG", contactRoot.getValue("speciality"));
      assertFalse(contactRoot.hasValue("middleName"));

      // Try with null "contact"
      phone.setValue("contact", null);
      in = new ObjectInput(phone);

      try
      {
         m_parser.parse(in, m_phoneMsg);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         ex = (IntegrationException)ex.getCause();
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
      }
   }

   public void testGetAttributes()
   {
      Pair result = ObjectMessagePartMapping.getAttributes(m_phoneMsg.getRoot());

      // Was:      "(type number (contact (@@ Surgeon firstName speciality lastName) (@@ Patient firstName middleName lastName) firstName))"
      // Should be:"(type number (contact (@@ Surgeon speciality lastName) (@@ Patient middleName lastName) firstName))"
      assertEquals("(type number (contact (@@ Surgeon speciality lastName) (@@ Patient middleName lastName) firstName))",
         result.toString());
   }

   public void testSelect()
   {
      Metaclass contactClass = getMetadata().getMetaclass("Contact");
      Metaclass patientClass = getMetadata().getMetaclass("Patient");
      Metaclass surgeonClass = getMetadata().getMetaclass("Surgeon");
      Metaclass doctorClass = getMetadata().getMetaclass("Doctor");
      Message selected;


      assertEquals(m_contactMsg, ((ObjectMessagePartMapping)m_contactMsg.getRoot().getMapping()).findMessage(contactClass));
      assertEquals(m_patientMsg, ((ObjectMessagePartMapping)m_contactMsg.getRoot().getMapping()).findMessage(patientClass));
      assertEquals(m_surgeonMsg, ((ObjectMessagePartMapping)m_contactMsg.getRoot().getMapping()).findMessage(surgeonClass));
      assertNull(((ObjectMessagePartMapping)m_contactMsg.getRoot().getMapping()).findMessage(doctorClass));

      assertNull(((ObjectMessagePartMapping)m_patientMsg.getRoot().getMapping()).findMessage(contactClass));
      assertNull(((ObjectMessagePartMapping)m_patientMsg.getRoot().getMapping()).findMessage(surgeonClass));

      assertNull(((ObjectMessagePartMapping)m_surgeonMsg.getRoot().getMapping()).findMessage(contactClass));
      assertNull(((ObjectMessagePartMapping)m_surgeonMsg.getRoot().getMapping()).findMessage(patientClass));


      selected = ObjectMessageParser.findDerivedMessage(m_contactMsg, contactClass);
      assertEquals(m_contactMsg, selected);

      selected = ObjectMessageParser.findDerivedMessage(m_patientMsg, patientClass);
      assertEquals(m_patientMsg, selected);

      selected = ObjectMessageParser.findDerivedMessage(m_contactMsg, patientClass);
      assertEquals(m_patientMsg, selected);

      selected = ObjectMessageParser.findDerivedMessage(m_contactMsg, doctorClass);
      assertEquals(m_contactMsg, selected);

      selected = ObjectMessageParser.findDerivedMessage(m_contactMsg, surgeonClass);
      assertEquals(m_surgeonMsg, selected);

      selected = ObjectMessageParser.findDerivedMessage(m_phoneMsg, getMetadata().getMetaclass("Phone"));
      assertEquals(m_phoneMsg, selected);
   }

   /**
    *
    *
    * +---------+                 /----------------\   Contact -> ContactMessage
    * | Contact |- - - - - - - - -| ContactMessage |   Doctor  -> DoctorMessage
    * +----^----+                 \--------^-------/   Surgeon -> DoctorMessage
    *      |                               |
    * +---------+                 /----------------\   Surgeon -> DoctorMessage
    * | Doctor  |- - - - - - - - -|  DoctorMessage |   Doctor  -> DoctorMessage
    * +----^----+                 \----^--------^--/
    *      |                           |        |
    * +---------+       /-----------------\   /-----------------\   Surgeon -> SurgeonMessage2
    * | Surgeon |- - - -| SurgeonMessage1 |- -| SurgeonMessage2 |
    * +---------+       \-----------------/   \-----------------/
    *              Surgeon -> SurgeonMessage1
    */
   public void testMapGeneration()
   {
      Metaclass contactClass = getMetadata().getMetaclass("Contact");
      Metaclass doctorClass = getMetadata().getMetaclass("Doctor");
      Metaclass surgeonClass = getMetadata().getMetaclass("Surgeon");
      Metaclass specialClass = getMetadata().getMetaclass("Special");
      Message contactMsg = new Message("ContactMessage");
      Message doctorMsg = new Message("DoctorMessage");
      Message surgeonMsg1 = new Message("SurgeonMessage1");
      Message surgeonMsg2 = new Message("SurgeonMessage2");
      CompositeMessagePartInstance contactPart = new CompositeMessagePartInstance(contactMsg.getName());
      CompositeMessagePartInstance doctorPart = new CompositeMessagePartInstance(doctorMsg.getName());
      CompositeMessagePartInstance surgeonPart1 = new CompositeMessagePartInstance(surgeonMsg1.getName());
      CompositeMessagePartInstance surgeonPart2 = new CompositeMessagePartInstance(surgeonMsg2.getName());
      ObjectMessagePartMapping contactMapping = new ObjectMessagePartMapping();
      ObjectMessagePartMapping doctorMapping = new ObjectMessagePartMapping();
      ObjectMessagePartMapping surgeonMapping1 = new ObjectMessagePartMapping();
      ObjectMessagePartMapping surgeonMapping2 = new ObjectMessagePartMapping();

      contactMsg.setRoot(contactPart);
      doctorMsg.setRoot(doctorPart);
      surgeonMsg1.setRoot(surgeonPart1);
      surgeonMsg2.setRoot(surgeonPart2);

      contactPart.setDeclarator(contactMsg);
      doctorPart.setDeclarator(doctorMsg);
      surgeonPart1.setDeclarator(surgeonMsg1);
      surgeonPart2.setDeclarator(surgeonMsg2);

      doctorMsg.setBaseMessage(contactMsg);
      contactMsg.addDerivedMessage(doctorMsg);
      surgeonMsg1.setBaseMessage(doctorMsg);
      doctorMsg.addDerivedMessage(surgeonMsg1);
      surgeonMsg2.setBaseMessage(doctorMsg);
      doctorMsg.addDerivedMessage(surgeonMsg2);

      contactPart.setMapping(contactMapping);
      doctorPart.setMapping(doctorMapping);
      surgeonPart1.setMapping(surgeonMapping1);
      surgeonPart2.setMapping(surgeonMapping2);

      contactMapping.setMetaclass(contactClass);
      doctorMapping.setMetaclass(doctorClass);
      surgeonMapping1.setMetaclass(surgeonClass);
      surgeonMapping2.setMetaclass(surgeonClass);

      contactMapping.init(contactPart);
      doctorMapping.init(doctorPart);
      surgeonMapping1.init(surgeonPart1);
      surgeonMapping2.init(surgeonPart2);

      contactMapping.finish(contactPart);
      doctorMapping.finish(doctorPart);
      surgeonMapping1.finish(surgeonPart1);
      surgeonMapping2.finish(surgeonPart2);

      assertEquals(contactMsg, contactMapping.findMessage(contactClass));
      assertEquals(doctorMsg, contactMapping.findMessage(doctorClass));
      assertEquals(doctorMsg, contactMapping.findMessage(surgeonClass));
      assertEquals(doctorMsg, doctorMapping.findMessage(surgeonClass));
      assertEquals(surgeonMsg1, surgeonMapping1.findMessage(surgeonClass));
      assertEquals(surgeonMsg2, surgeonMapping2.findMessage(surgeonClass));

      assertNull(doctorMapping.findMessage(contactClass));
      assertNull(surgeonMapping1.findMessage(contactClass));
      assertNull(surgeonMapping1.findMessage(doctorClass));
      assertNull(surgeonMapping2.findMessage(contactClass));
      assertNull(surgeonMapping2.findMessage(doctorClass));

      assertEquals(contactMsg, ObjectMessageParser.findDerivedMessage(contactMsg, specialClass));
      assertNull(ObjectMessageParser.findDerivedMessage(doctorMsg, specialClass));

      assertEquals(doctorMsg, ObjectMessageParser.findDerivedMessage(contactMsg, doctorClass));
      assertNull(ObjectMessageParser.findDerivedMessage(surgeonMsg1, contactClass));
      assertEquals(doctorMsg, ObjectMessageParser.findDerivedMessage(doctorMsg, surgeonClass));
      assertEquals(doctorMsg, ObjectMessageParser.findDerivedMessage(contactMsg, surgeonClass));
   }
}
