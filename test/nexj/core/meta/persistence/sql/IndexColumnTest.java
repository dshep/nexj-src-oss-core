// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import junit.framework.TestCase;

import nexj.core.meta.Repository;

public class IndexColumnTest extends TestCase
{
   private IndexColumn m_id = null;

   /**
    * Constructor for IndexColumnTest.
    * @param name
    */
   public IndexColumnTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_id = ((RelationalSchema)Repository.getMetadata().getDataSource("DefaultRelationalDatabase")
         .getSchema()).getTable("test.Contact").getIndex("Contact.PK").getIndexColumn(0);
   }

   public void testGetOrdinal()
   {
      assertEquals(0, m_id.getOrdinal());
   }

   public void testIsAscending()
   {
      assertTrue(m_id.isAscending());
   }

   public void testGetIndex()
   {
      assertEquals("Contact.PK", m_id.getIndex().getName());
   }

   public void testGetColumn()
   {
      assertEquals("id", m_id.getColumn().getName());
   }
}
