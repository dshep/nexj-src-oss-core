// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Repository;
import nexj.core.util.SysUtil;

public class TableTest extends TestCase
{
   private Table m_contact;
   private Table m_contactType;
   private Table m_versioned;
   private Table m_locking;
   private Table m_hrRequest;

   /**
    * Constructor for TableTest.
    * @param name
    */
   public TableTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      RelationalSchema schema = (RelationalSchema)Repository.getMetadata()
         .getDataSource("DefaultRelationalDatabase").getSchema();

      m_contact = schema.getTable("test.Contact");
      m_contactType = schema.getTable("ContactType");
      m_versioned = schema.getTable("VERSIONED");
      m_locking = schema.getTable("LOCKING");
      m_hrRequest = schema.getTable("test.HRRequest");
   }

   public void testGetOwnerName()
   {
      assertEquals("test", m_contact.getOwnerName());
      assertEquals("test", m_contactType.getOwnerName());
      assertNull(m_versioned.getOwnerName());
   }

   public void testGetTableName()
   {
      assertEquals("Contact", m_contact.getTableName());
      assertEquals("ContactType", m_contactType.getTableName());
   }

   public void testGetSchema()
   {
      assertEquals("DefaultRelationalDatabase", m_contact.getSchema().getDataSource().getName());
   }

   /*
    * Test for Column getColumn(String)
    */
   public void testGetColumnString()
   {
      assertEquals("id", m_contact.getColumn("id").getName());
      assertEquals("title", m_contact.getColumn("title").getName());
      assertEquals("version", m_hrRequest.getColumn("version").getName());
   }

   /*
    * Test for Column getColumn(int)
    */
   public void testGetColumnint()
   {
      assertEquals("id", m_contact.getColumn(0).getName());
      assertEquals("readPrincipalId", m_contact.getColumn(3).getName());
   }

   public void testGetColumnCount()
   {
      assertEquals(29, m_contact.getColumnCount());
      assertEquals(1, m_contactType.getColumnCount());
      assertEquals(7, m_hrRequest.getColumnCount());
   }

   public void testGetColumnIterator()
   {
      Iterator itr = m_contact.getColumnIterator();
      int nCount = m_contact.getColumnCount();
      
      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   /*
    * Test for Index getIndex(String)
    */
   public void testGetIndexString()
   {
      assertEquals("Contact.PK", m_contact.getIndex("Contact.PK").getName());
      assertEquals("Contact.FK2", m_contact.getIndex("Contact.FK2").getName());
      assertEquals("test.HRRequest.OK_Version", m_hrRequest.getIndex("test.HRRequest.OK_Version").getName());

      try
      {
         m_contact.getIndex("m");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   /*
    * Test for Index getIndex(int)
    */
   public void testGetIndexint()
   {
      assertEquals("Contact.PK", m_contact.getIndex(0).getName());
      assertEquals("Contact.FK2", m_contact.getIndex(5).getName());
   }

   public void testGetIndexCount()
   {
      assertEquals(12, m_contact.getIndexCount());
      assertEquals(1, m_contactType.getIndexCount());
      assertEquals(4, m_hrRequest.getIndexCount());
   }

   public void testGetIndexIterator()
   {
      Iterator itr = m_contact.getIndexIterator();
      
      for (int i = m_contact.getIndexCount(); i > 0; --i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetPrimaryKey()
   {
      assertEquals("Contact.PK", m_contact.getPrimaryKey().getName());
   }

   public void testGetRelatedKey()
   {
      assertEquals("Contact.FK5", m_contact.getRelatedKey(0).getName());
   }

   public void testGetRelatedKeyCount()
   {
      assertEquals((SysUtil.ENTERPRISE) ? 10 : 8, m_contact.getRelatedKeyCount());
   }

   public void testGetRelatedKeyIterator()
   {
      Iterator itr = m_contact.getRelatedKeyIterator();

      int nCount = m_contact.getRelatedKeyCount();
      
      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetName()
   {
      assertEquals("test.Contact", m_contact.getName());
      assertEquals("ContactType", m_contactType.getName());
   }

   public void testGetPrefix()
   {
      assertNull(m_contact.getPrefix());
      assertNull(m_contactType.getPrefix());
   }

   public void testGetType()
   {
      assertEquals(Table.MANAGED, m_contact.getType());
      assertEquals(Table.MANAGED, m_contactType.getType());
      assertEquals(Table.ASPECT, m_versioned.getType());
   }

   public void testGetPointcutPattern()
   {
      assertEquals("Contact*", m_versioned.getPointcutPattern(0));
   }

   public void testIsPointcutPatternInclusive()
   {
      assertTrue(m_versioned.isPointcutPatternInclusive(0));
   }

   public void testGetPointcutPatternCount()
   {
      assertEquals(1, m_versioned.getPointcutPatternCount());
   }

   public void testIsMatching()
   {
      assertFalse(m_versioned.isMatching(m_hrRequest));
      assertFalse(m_versioned.isMatching(m_contactType));
      assertFalse(m_versioned.isMatching(m_contact));
      assertFalse(m_locking.isMatching(m_contactType));
   }
   
   public void testIsAspect()
   {
      assertTrue(m_versioned.isAspect());
      assertFalse(m_hrRequest.isAspect());
   }
   
   public void testGetAspect()
   {
      assertEquals("VERSIONED", m_hrRequest.getAspect(0).getName());
   }

   public void testHasAspect()
   {
      assertTrue(m_hrRequest.hasAspect(m_versioned));
      assertFalse(m_contactType.hasAspect(m_versioned));
      assertFalse(m_contactType.hasAspect(m_locking));
   }

   public void testGetAspectCount()
   {
      assertEquals(0, m_contact.getAspectCount());
      assertEquals(0, m_contactType.getAspectCount());
      assertEquals(1, m_hrRequest.getAspectCount());
   }


   public void testFindAspectOverride()
   {
      assertEquals(0, m_hrRequest.findAspectOverride(m_versioned));
      assertEquals(0, m_contactType.findAspectOverride(m_versioned));
      assertEquals(1, m_contactType.findAspectOverride(m_locking));
      assertEquals(-1, m_contact.findAspectOverride(m_versioned));
   }

   public void testGetAspectOverride()
   {
      assertEquals("VERSIONED", m_hrRequest.getAspectOverride(0).getName());
      assertEquals("VERSIONED", m_contactType.getAspectOverride(0).getName());
      assertEquals("LOCKING", m_contactType.getAspectOverride(1).getName());
   }

   public void testIsAspectOverrideInclusive()
   {
      assertTrue(m_hrRequest.isAspectOverrideInclusive(0));
      assertFalse(m_contactType.isAspectOverrideInclusive(0));
      assertFalse(m_contactType.isAspectOverrideInclusive(1));
   }

   public void testGetAspectOverrideCount()
   {
      assertEquals(2, m_hrRequest.getAspectOverrideCount());
      assertEquals(2, m_contactType.getAspectOverrideCount());
      assertEquals(1, m_contact.getAspectOverrideCount());
   }

   public void testIsPointcut()
   {
      assertTrue(m_hrRequest.isPointcut());
      assertFalse(m_versioned.isPointcut());
   }

   public void testRemoveColumn()
   {
      Metaclass metaclass = Repository.getMetadata().getMetaclass("Contact");
      RelationalSchema schema = new RelationalSchema();

      schema.addTable(m_locking.clone(schema));

      Table clone = m_contact.clone(schema);
      Column mappedCol = clone.getColumn("first_name");
      Column unmappedCol = clone.getColumn("title");
      int nMappedCol = mappedCol.getOrdinal();
      int nUnmappedCol = unmappedCol.getOrdinal();
      RelationalPrimitiveMapping[] tableMapping = m_contact.findMappingArray((RelationalMapping)metaclass.getPersistenceMapping());
      RelationalPrimitiveMapping[] cloneMapping = clone.findMappingArray((RelationalMapping)metaclass.getPersistenceMapping());
      RelationalPrimitiveMapping mappedColMapping = cloneMapping[mappedCol.getOrdinal()];

      clone.removeColumn(mappedCol);
      clone.removeColumn(unmappedCol);

      // validate column removal
      assertNull(clone.findColumn(mappedCol.getName()));
      assertNull(clone.findColumn(unmappedCol.getName()));

      // validate RelationalPrimitiveMapping update
      int nTableMappingNullCount = 0;
      int nCloneMappingNullCount = 0;

      for (int i = 0, nCount = tableMapping.length; i < nCount; ++i)
      {
         nTableMappingNullCount += (tableMapping[i] == null) ? 1 : 0;
      }

      for (int i = 0, nCount = cloneMapping.length; i < nCount; ++i)
      {
         nCloneMappingNullCount += (cloneMapping[i] == null) ? 1 : 0;
      }

      assertTrue(tableMapping != cloneMapping);
      assertEquals(m_contact.getColumn(nMappedCol), tableMapping[nMappedCol].getColumn());
      assertNotSame(mappedColMapping, cloneMapping[nMappedCol]);
      assertEquals(null, tableMapping[nUnmappedCol]);
      assertEquals(nTableMappingNullCount + 1, nCloneMappingNullCount);
   }

   public void testToString()
   {
      assertEquals("Table test.Contact", m_contact.toString());
   }
}
