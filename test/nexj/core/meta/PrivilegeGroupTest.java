// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class PrivilegeGroupTest extends TestCase
{
   protected PrivilegeGroup m_contactManagement;
   
   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_contactManagement = (PrivilegeGroup)Repository.getMetadata().getPrivilege("contactManagement");
   }

   public void testIsPrimitive()
   {
      assertFalse(m_contactManagement.isPrimitive());
   }

   public void testAddTo()
   {
      Metadata metadata = Repository.getMetadata();
      PrivilegeSet set = metadata.createPrivilegeSet();

      m_contactManagement.addTo(set);

      assertTrue(set.contains(metadata.getPrimitivePrivilege("createContact")));
      assertTrue(set.contains(metadata.getPrimitivePrivilege("readContact")));
      assertTrue(set.contains(metadata.getPrimitivePrivilege("deleteContact")));
   }

   public void testGetPrivilegeCount()
   {
      assertEquals(3, m_contactManagement.getPrivilegeCount());
   }

   public void testGetName()
   {
      assertEquals("contactManagement", m_contactManagement.getName());
   }
}
