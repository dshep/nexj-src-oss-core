// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import java.util.Iterator;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.ObjectInput;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.persistence.Query;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;

public class ObjectMessageParserTest extends SQLDataTest
{
   
   private Format m_format;
   protected MessageParser m_parser;
   
   public ObjectMessageParserTest(String name)
   {
      super(name);
   }

   protected void setUp() throws Exception
   {
      super.setUp();
      m_format = getMetadata().getFormat("Object");
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);
   }
   
   /**
    * @return Message with the given name
    */
   protected Message getMessage(String sName)
   {
      return Repository.getMetadata().getMessage(sName);
   }

   /**
    *  Bug #25488: formatting complex object messages can cause ArrayIndexOutOfBoundsException. 
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
      participants.add(doctor);
      doctor.setValue("licenseNumber", "Lic 01");
      commit();
      
      group = Query.createRead(supportGroupMetaclass, null, null, null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);
      
      TransferObject tobj = m_parser.parse(new ObjectInput(group), getMessage("Object_SupportGroup"));
      assertEquals("A group", tobj.getValue("name"));
      assertEquals(1, ((List)tobj.getValue("participants")).size());
      assertEquals("Lic 01", ((TransferObject)(((List)tobj.getValue("participants")).get(0))).getValue("licenseNumber"));

      ((InstanceList)group.getValue("participants")).add(contact);
      commit();
   
      try
      {
         m_parser.parse(new ObjectInput(group), getMessage("Object_SupportGroup"));
         fail();
      }
      catch (IntegrationException e)
      {
         assertTrue(e.getCause().getMessage().contains("err.integration.object.partClass"));
      }
   }

   /**
    *  Bug #25494: ObjectMessageFormatter reads the entire graph in one select. This can cause StackOverflowError.
    */
   public void testCircularMessage() throws Exception
   {
      InstanceList instances = Query.createRead(getMetadata().getMetaclass("Account"), 
         parse("(funds (contact lastName (@@ Doctor licenseNumber)))"), 
         null, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      
      for (Iterator itr = instances.iterator(); itr.hasNext(); )
      {
         Instance account = (Instance)itr.next();
         TransferObject accountTobj = m_parser.parse(new ObjectInput(account), getMessage("Object_AccountRecursive"));
         TransferObject contactTobj = (TransferObject)accountTobj.getValue("contact");

         assertTrue(contactTobj.getOID() == ((TransferObject)((TransferObject)contactTobj.getValue("accounts")).getValue("contact")).getOID());
      }
   }
}
