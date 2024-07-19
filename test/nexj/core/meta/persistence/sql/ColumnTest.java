// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import junit.framework.TestCase;

import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;

public class ColumnTest extends TestCase
{
   private Column m_id;
   private Column m_firstName;
   private Column m_businessAddressCount;

   public ColumnTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      Table table = ((RelationalSchema)Repository.getMetadata()
         .getDataSource("DefaultRelationalDatabase").getSchema()).getTable("test.Contact");
      m_id = table.getColumn("id");
      m_firstName = table.getColumn("first_name");
      m_businessAddressCount = table.getColumn("businessAddressCount");
   }

   public void testGetPrecision()
   {
      assertEquals(16, m_id.getPrecision());
      assertEquals(2, m_businessAddressCount.getPrecision());
   }

   public void testGetScale()
   {
      assertEquals(0, m_id.getScale());
      assertEquals(0, m_businessAddressCount.getScale());
   }

   public void testGetAllocation()
   {
      assertEquals(Column.FIXED, m_id.getAllocation());
      assertEquals(Column.VARYING, m_firstName.getAllocation());
      assertEquals(Column.FIXED, m_businessAddressCount.getAllocation());
   }

   public void testGetTable()
   {
      assertEquals("test.Contact", m_id.getTable().getName());
   }

   public void testGetType()
   {
      assertSame(Primitive.BINARY, m_id.getType());
      assertSame(Primitive.INTEGER, m_businessAddressCount.getType());
   }

   public void testGetName()
   {
      assertEquals("id", m_id.getName());
   }

   public void testToString()
   {
      assertEquals("Column test.Contact.id", m_id.toString());
   }
}
