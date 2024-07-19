// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Repository;

public class RelationalPrimitiveMappingTest extends TestCase
{
   private RelationalPrimitiveMapping m_mapping = null;

   /**
    * Constructor for RelationalPrimitiveMappingTest.
    * @param name
    */
   public RelationalPrimitiveMappingTest(String name)
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
      
      m_mapping = (RelationalPrimitiveMapping)((RelationalMapping)contact
         .getPersistenceMapping()).getAttributeMapping(contact.getAttribute("firstName"));
   }

   public void testGetColumn()
   {
      assertEquals("first_name", m_mapping.getColumn().getName());
   }

   public void testGetPersistenceMapping()
   {
      assertEquals("Contact", m_mapping.getPersistenceMapping().getMetaclass().getName());
   }

   public void testGetAttribute()
   {
      assertEquals("firstName", m_mapping.getAttribute().getName());
   }
   
   public void testToString()
   {
      assertEquals("RelationalPrimitiveMapping for Attribute firstName to Column test.Contact.first_name", m_mapping.toString());
   }
}
