// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;

public class IndexTest extends TestCase
{
   private Index m_pk;
   private Index m_fk1;
   private Index m_oi1;
   private Index m_aop;
   private Index m_versioned;

   /**
    * Constructor for IndexTest.
    * @param name
    */
   public IndexTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      RelationalSchema schema = (RelationalSchema)Repository.getMetadata().getDataSource("DefaultRelationalDatabase").getSchema();

      m_pk = schema.getIndex("Contact.PK");
      m_fk1 = schema.getIndex("Address.FK1");
      m_oi1 = schema.getIndex("Address.OI1");
      m_aop = schema.getIndex("HRRequest.AOP");
      m_versioned = schema.getIndex("VERSIONED");
   }

   /**
    * Ensure that LOB columns are not added to index (TT #41674).
    * Most DBs do not support indexes with LOB columns.
    */
   public void testAddLOBColumn()
   {
      Column col = new Column("lob", m_pk.getTable());
      IndexColumn iCol = new IndexColumn(col, true);

      col.setType(Primitive.STRING);

      try // precision: 0 (implicit locator allocation)
      {
         m_pk.addIndexColumn(iCol);
         m_pk.removeIndexColumn(iCol); // should not get here, prevent other tests from failing
         fail("LOB columns are not supported in indexes for DB consistency reasons.");
      }
      catch (MetadataException e)
      {}

      col.setPrecision(Column.MAX_NVARCHAR_PRECISION + 1);

      try // precision: MAX_NVARCHAR_PRECISION + 1 (implicit locator allocation)
      {
         m_pk.addIndexColumn(iCol);
         m_pk.removeIndexColumn(iCol); // should not get here, prevent other tests from failing
         fail("LOB columns are not supported in indexes for DB consistency reasons.");
      }
      catch (MetadataException e)
      {}

      col.setPrecision(1); // valid size
      col.setAllocation(Column.LOCATOR);

      try // explicit locator allocation
      {
         m_pk.addIndexColumn(iCol);
         m_pk.removeIndexColumn(iCol); // should not get here, prevent other tests from failing
         fail("LOB columns are not supported in indexes for DB consistency reasons.");
      }
      catch (MetadataException e)
      {}
   }

   public void testGetType()
   {
      assertEquals(Index.VIRTUAL, m_fk1.getType());
      assertEquals(Index.BTREE, m_oi1.getType());
      assertEquals(Index.VIRTUAL, m_aop.getType());
      assertEquals(Index.ASPECT, m_versioned.getType());
   }

   public void testIsUnique()
   {
      assertTrue(m_pk.isUnique());
      assertFalse(m_fk1.isUnique());
      assertFalse(m_aop.isUnique());
      assertFalse(m_versioned.isUnique());
   }

   public void testGetTable()
   {
      assertEquals("test.Contact", m_pk.getTable().getName());
      assertEquals("test.Address", m_fk1.getTable().getName());
      assertEquals("test.HRRequest", m_aop.getTable().getName());
      assertEquals("VERSIONED", m_versioned.getTable().getName());
   }

   public void testGetIndexColumn()
   {
      assertEquals("id", m_pk.getIndexColumn(0).getColumn().getName());
      assertEquals("version", m_aop.getIndexColumn(0).getColumn().getName());
      assertEquals("userId", m_aop.getIndexColumn(1).getColumn().getName());
      assertEquals("version", m_versioned.getIndexColumn(0).getColumn().getName());
   }

   public void testGetIndexColumnCount()
   {
      assertEquals(1, m_pk.getIndexColumnCount());
      assertEquals(2, m_aop.getIndexColumnCount());
      assertEquals(1, m_versioned.getIndexColumnCount());
   }

   public void testGetIndexColumnIterator()
   {
      Iterator itr = m_pk.getIndexColumnIterator();
      itr.next();
      assertFalse(itr.hasNext());
   }

   public void testGetRelatedTable()
   {
      assertNull(m_pk.getRelatedTable());
      assertEquals("test.Contact", m_fk1.getRelatedTable().getName());
   }

   public void testGetName()
   {
      assertEquals("Address.FK1", m_fk1.getName());
      assertEquals("HRRequest.AOP", m_aop.getName());
      assertEquals("VERSIONED", m_versioned.getName());
   }

   public void testGetPointcutPatternCount()
   {
      assertEquals(0, m_versioned.getPointcutPatternCount());
   }

   public void testIsMatching()
   {
      assertFalse(m_versioned.isMatching(m_aop));
      assertFalse(m_versioned.isMatching(m_oi1));
   }
   
   public void testIsAspect()
   {
      assertTrue(m_versioned.isAspect());
      assertFalse(m_aop.isAspect());
   }

   public void testGetAspect()
   {
      assertEquals("VERSIONED", m_aop.getAspect(0).getName());
   }

   public void testHasAspect()
   {
      assertTrue(m_aop.hasAspect(m_versioned));
      assertFalse(m_oi1.hasAspect(m_versioned));
   }

   public void testGetAspectCount()
   {
      assertEquals(0, m_oi1.getAspectCount());
      assertEquals(1, m_aop.getAspectCount());
   }


   public void testFindAspectOverride()
   {
      assertEquals(0, m_aop.findAspectOverride(m_versioned));
      assertEquals(-1, m_oi1.findAspectOverride(m_versioned));
   }

   public void testGetAspectOverride()
   {
      assertEquals("VERSIONED", m_aop.getAspectOverride(0).getName());
   }

   public void testIsAspectOverrideInclusive()
   {
      assertTrue(m_aop.isAspectOverrideInclusive(0));
   }

   public void testGetAspectOverrideCount()
   {
      assertEquals(1, m_aop.getAspectOverrideCount());
      assertEquals(0, m_oi1.getAspectOverrideCount());
   }

   public void testIsPointcut()
   {
      assertTrue(m_aop.isPointcut());
      assertFalse(m_versioned.isPointcut());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("Index Address.FK1", m_fk1.toString());
   }
}
