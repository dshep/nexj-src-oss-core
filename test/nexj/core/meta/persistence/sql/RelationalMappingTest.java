// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.Repository;

public class RelationalMappingTest extends TestCase
{
   private RelationalMapping m_contact;
   private RelationalMapping m_principal;
   private RelationalMapping m_user;
   private RelationalMapping m_group;
   
   /**
    * Constructor for RelationalMappingTest.
    * @param name
    */
   public RelationalMappingTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_contact = (RelationalMapping)Repository.getMetadata().getMetaclass("Contact").getPersistenceMapping();
      m_principal = (RelationalMapping)Repository.getMetadata().getMetaclass("Principal").getPersistenceMapping();
      m_user = (RelationalMapping)Repository.getMetadata().getMetaclass("User").getPersistenceMapping();
      m_group = (RelationalMapping)Repository.getMetadata().getMetaclass("Group").getPersistenceMapping();

   }

   public void testGetObjectKey()
   {
      assertEquals(1, m_contact.getObjectKey().getPartCount());
      assertEquals("binary", m_contact.getObjectKey().getPartType(0).getName());
      assertEquals(true, m_contact.getObjectKey().isPartAscending(0));
   }

   public void testGetKeyGenerator()
   {
      assertEquals("KeyGenerator.GUIDGen", m_contact.getKeyGenerator().getName());
   }

   public void testGetPrimaryTable()
   {
      assertEquals("test.Contact", m_contact.getPrimaryTable().getName());
   }

   public void testGetAttributeMapping()
   {
      assertEquals("firstName", m_contact.getAttributeMapping(m_contact.getMetaclass().getAttribute("firstName")).getAttribute().getName());
   }

   public void testGetInheritedAspectAttributeMapping()
   {
      Metaclass clazz = Repository.getMetadata().getMetaclass("DS1Class");

      assertTrue(clazz.getPersistenceMapping().getAttributeMapping(clazz.getAttribute("y")) != null);

      clazz = Repository.getMetadata().getMetaclass("DS2Class");
      assertTrue(clazz.getPersistenceMapping().getAttributeMapping(clazz.getAttribute("y")) != null);
   }

   public void testGetTable()
   {
      assertEquals("test.Contact", m_contact.getTable(0).getName());
   }

   public void testGetTableCount()
   {
      assertEquals(1, m_contact.getTableCount());
   }

   public void testGetDataSource()
   {
      assertEquals("DefaultRelationalDatabase", m_contact.getDataSource().getName());
   }

   public void testGetMetaclass()
   {
      assertEquals("Contact", m_contact.getMetaclass().getName());
   }

   public void testGetLockingAttribute()
   {
      assertEquals("version", m_contact.getLockingAttribute().getName());
      assertEquals("updateCount", ((RelationalMapping)Repository.getMetadata().getMetaclass("Principal").getPersistenceMapping()).getLockingAttribute().getName());
   }
   
   public void testGetTypeCodeAttribute()
   {
      assertEquals("classCode", m_contact.getTypeCodeAttribute().getName());
      assertEquals("typeCode", m_principal.getTypeCodeAttribute().getName());
      assertEquals("typeCode", m_user.getTypeCodeAttribute().getName());
      assertEquals("typeCode", m_group.getTypeCodeAttribute().getName());
   }

   public void testGetMetaclassByTypeCode()
   {
      assertEquals("User", m_principal.getMetaclassByTypeCode("U").getName());
      assertEquals("User", m_user.getMetaclassByTypeCode("U").getName());
      assertEquals("Group", m_principal.getMetaclassByTypeCode("G").getName());
      assertEquals("Group", m_group.getMetaclassByTypeCode("G").getName());
      
      try
      {
         assertEquals("Contact", m_contact.getMetaclassByTypeCode("???").getName());
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
      
      try
      {
         assertEquals("Principal", m_principal.getMetaclassByTypeCode(null).getName());
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testIsTypeCodeFiltered()
   {
      assertFalse(m_contact.isTypeCodeFiltered());
      assertTrue(m_user.isTypeCodeFiltered());
      assertTrue(m_group.isTypeCodeFiltered());
      assertFalse(m_principal.isTypeCodeFiltered());
   }

   public void testIsTypeCodeDispatched()
   {
      assertTrue(m_contact.isTypeCodeDispatched());
      assertFalse(m_user.isTypeCodeDispatched());
      assertFalse(m_group.isTypeCodeDispatched());
      assertTrue(m_principal.isTypeCodeDispatched());
   }

   public void testIsTypeCodePrivileged()
   {
      PrivilegeSet privilegeSet = new PrivilegeSet(Repository.getMetadata().getPrimitivePrivilegeCount(), true);

      assertFalse(m_contact.isTypeCodePrivileged(privilegeSet));

      privilegeSet.remove(Repository.getMetadata().getPrimitivePrivilege("readSpecial"));

      assertTrue(m_contact.isTypeCodePrivileged(privilegeSet));
      assertFalse(m_principal.isTypeCodePrivileged(privilegeSet));
   }
   
   public void testGetTypeCodeIterator()
   {
      PrivilegeSet privilegeSet = new PrivilegeSet(Repository.getMetadata().getPrimitivePrivilegeCount(), true);

      Iterator itr = m_contact.getTypeCodeIterator(privilegeSet);

      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();

      assertFalse(itr.hasNext());

      privilegeSet.remove(Repository.getMetadata().getPrimitivePrivilege("readSpecial"));
      itr = m_contact.getTypeCodeIterator(privilegeSet);

      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      
      assertFalse(itr.hasNext());
   }

   public void testToString()
   {
      assertEquals("RelationalMapping DefaultRelationalDatabase for Metaclass Contact", m_contact.toString());
   }
}
