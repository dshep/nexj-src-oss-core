// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.Metaclass;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.util.Binary;

public class ObjectMessageFormatterTest extends SQLDataTest
{
   
   private Format m_format;
   protected MessageFormatter m_formatter;
   
   public ObjectMessageFormatterTest(String name)
   {
      super(name);
   }

   protected void setUp() throws Exception
   {
      super.setUp();
      m_format = getMetadata().getFormat("Object");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_format = null;
      m_formatter = null;
   }

   /**
    * @return Message with the given name
    */
   protected Message getMessage(String sName)
   {
      return getMetadata().getMessage(sName);
   }
   
   /**
    *  Bug 25488: formatting complex object messages can cause ArrayIndexOutOfBoundsException.
    */
   public void testSubclassInMessage() throws Exception
   {
      InstanceList instances = Query.createRead(getMetadata().getMetaclass("Contact"), null, null, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      Instance contact = null;
      Instance doctor = null;

      for (Iterator itr = instances.iterator(); itr.hasNext(); )
      {
         Instance instance = (Instance)itr.next();
         
         if (instance.getMetaclass().getName().equals("Contact"))
         {
            contact = instance;
         }
         
         if (instance.getMetaclass().getName().equals("Doctor"))
         {
            doctor = instance;
         }
      }
      
      Metaclass supportGroupMetaclass = getMetadata().getMetaclass("SupportGroup");
      Instance group = new Instance(supportGroupMetaclass, m_context);
      InstanceList participants = new InstanceArrayList(2);
      
      group.setNew();
      group.setValue("name", "A group");
      group.setValue("participants", participants);
      participants.add(contact);
      participants.add(doctor);
      commit();
      
      TransferObject groupTobj = new TransferObject(group.getOID(), "Object_SupportGroup", "update", 3);
      List participantTobjList = new ArrayList();
      TransferObject contactTobj = new TransferObject(contact.getOID(), "Contact", "update", 3);
      TransferObject doctorTobj = new TransferObject(doctor.getOID(), "Doctor", "update", 3);

      groupTobj.setValue("participants", participantTobjList);
      groupTobj.setValue("name", "Updated");
      participantTobjList.add(doctorTobj);
      doctorTobj.setValue("licenseNumber", "license for Doctor");
      m_formatter.format(groupTobj, getMessage("Object_SupportGroup"), new ObjectOutput());
      commit();
   
      group = Query.createRead(supportGroupMetaclass, null, null, null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);
      assertEquals("Updated", group.getValue("name"));
      participants = (InstanceList)group.getValue("participants");
      
      for (Iterator itr = participants.iterator(); itr.hasNext(); )
      {
         Instance participant = (Instance)itr.next();
         
         if (participant.getClassName().equals("Doctor"))
         {
            assertEquals("license for Doctor", participant.getValue("licenseNumber"));
         }
      }

      participantTobjList.add(contactTobj);
      contactTobj.setValue("licenseNumber", "license for Contact");
      
      try
      {
         m_formatter.format(groupTobj, getMessage("Object_SupportGroup"), new ObjectOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertTrue(e.getCause().getMessage().contains("err.integration.object.noMatch"));
      }
   }

   /**
    *  Bug #25494: ObjectMessageFormatter reads the entire graph in one select. This can cause StackOverflowError.
    */
   public void testCircularMessage() throws Exception
   {
      TransferObject account = new TransferObject(OID.fromBinary(Binary.parse("1000000000000000000000000000000006")), "Object_AccountRecursive", "update", 5);
      TransferObject contact = new TransferObject("Doctor", "update");
      
      account.setValue("funds", Double.valueOf(123));
      account.setValue("contact", contact);
      contact.setValue("lastName", "Peters");
      contact.setValue("account", account);
      m_formatter.format(account, getMessage("Object_AccountRecursive"), new ObjectOutput());
      commit();
   
      InstanceList instances = Query.createRead(getMetadata().getMetaclass("Account"), 
         parse("(funds (contact lastName))"), 
         null, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      boolean bMatch = false;
      
      for (Iterator itr = instances.iterator(); itr.hasNext(); )
      {
         Instance accountRead = (Instance)itr.next();
         
         if (((Number)accountRead.getValue("funds")).intValue() == 123)
         {
            bMatch = true;
            
            Instance contactRead = (Instance)accountRead.getValue("contact");
            assertEquals("Peters", contactRead.getValue("lastName"));
         }
      }
      
      assertTrue(bMatch);
   }
}
