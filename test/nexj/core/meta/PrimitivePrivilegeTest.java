// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class PrimitivePrivilegeTest extends TestCase
{
   protected PrimitivePrivilege m_createContact;
   protected PrimitivePrivilege m_readContact;
   
   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_createContact = Repository.getMetadata().getPrimitivePrivilege("createContact");
      m_readContact =  Repository.getMetadata().getPrimitivePrivilege("readContact");
   }

   public void testIsPrimitive()
   {
      assertTrue(m_createContact.isPrimitive());
      assertTrue(m_readContact.isPrimitive());
   }

   public void testGetOrdinal()
   {
      assertTrue(m_createContact.getOrdinal() >= 0);
      assertTrue(m_readContact.getOrdinal() >= 0);
      assertTrue(m_createContact.getOrdinal() != m_readContact.getOrdinal());
   }

   public void testGetName()
   {
      assertEquals("createContact", m_createContact.getName());
      assertEquals("readContact", m_readContact.getName());
   }
}
