// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Repository;

public class RelationalClassMappingTest extends TestCase
{
   private RelationalClassMapping m_addresses = null;
   private RelationalClassMapping m_primaryAddress = null;

   /**
    * Constructor for RelationalClassMappingTest.
    * @param name
    */
   public RelationalClassMappingTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      Metaclass contact = Repository.getMetadata().getMetaclass("Contact");
      
      m_addresses = (RelationalClassMapping)((RelationalMapping)contact
         .getPersistenceMapping()).getAttributeMapping(contact.getAttribute("addresses"));
      m_primaryAddress = (RelationalClassMapping)((RelationalMapping)contact
      .getPersistenceMapping()).getAttributeMapping(contact.getAttribute("primaryAddress"));
   }

   public void testGetSourceKey()
   {
      assertEquals("Contact.PK", m_addresses.getSourceKey().getName());
      assertEquals("Contact.FK1", m_primaryAddress.getSourceKey().getName());
   }

   public void testGetDestinationKey()
   {
      assertEquals("Address.FK1", ((Index)m_addresses.getDestinationKey()).getName());
      assertEquals("Address.PK", ((Index)m_primaryAddress.getDestinationKey()).getName());
   }

   public void testGetMapping()
   {
      assertEquals("Address", m_addresses.getMapping().getMetaclass().getName());
      assertEquals("Address", m_primaryAddress.getMapping().getMetaclass().getName());
   }

   public void testGetPersistenceMapping()
   {
      assertEquals("Contact", m_addresses.getPersistenceMapping().getMetaclass().getName());
   }

   public void testGetAttribute()
   {
      assertEquals("addresses", m_addresses.getAttribute().getName());
      assertEquals("primaryAddress", m_primaryAddress.getAttribute().getName());
   }
   
   public void testToString()
   {
      assertEquals("RelationalClassMapping for Attribute addresses to RelationalMapping DefaultRelationalDatabase for Metaclass Address", m_addresses.toString());
   }
}
