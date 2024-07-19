// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.Component;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.persistence.NullConverter;

public class RelationalSchemaTest extends TestCase
{
   protected RelationalSchema m_schema = null;

   /**
    * Constructor for RelationalSchemaTest.
    * @param name
    */
   public RelationalSchemaTest(String name)
   {
      super(name);
   }

   /**
    * Add a schema hint that should be supported during testing.
    * This method is used by SQLDataTest which is not in nexj.core.meta.persistence.sql package.
    * @param schema The schema to check for hint support.
    * @param sHint The hint to add (not null).
    */
   public static void addHint(RelationalSchema schema, String sHint)
   {
      if (!schema.isHintSupported(sHint)) // init m_hintSet
      {
         schema.m_hintSet.add(sHint);
      }
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_schema = (RelationalSchema)Repository.getMetadata().getDataSource("DefaultRelationalDatabase").getSchema();
   }

   public void testIsPortable()
   {
      assertTrue(m_schema.isPortable());
   }
   
   public void testGetTable()
   {
      assertEquals("test.Contact", m_schema.getTable("test.Contact").getName());
      assertEquals("test.Address", m_schema.getTable("test.Address").getName());
   }

   public void testGetTableCount()
   {
      assertEquals(70, m_schema.getTableCount());
   }

   public void testGetTableIterator()
   {
      Iterator itr = m_schema.getTableIterator();
      
      int nCount = m_schema.getTableCount();
      
      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetIndex()
   {
      assertEquals("Contact.PK", m_schema.getIndex("Contact.PK").getName());
      assertEquals("Address.PK", m_schema.getIndex("Address.PK").getName());
   }

   public void testGetIndexCount()
   {
      assertEquals(176, m_schema.getIndexCount());
   }

   public void testGetIndexIterator()
   {
      Iterator itr = m_schema.getIndexIterator();

      for (int i = m_schema.getIndexCount(); i > 0; --i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetPrefix()
   {
      assertEquals("test.", m_schema.getPrefix());
   }

   public void testGetTablespaceName()
   {
      assertEquals("nexjtest01", m_schema.getTablespaceName());
   }

   public void testGetRoleName()
   {
      assertEquals("test", m_schema.getRoleName());
   }

   public void testGetIndexFill()
   {
      assertEquals(80, m_schema.getIndexFill());
   }
   
   public void testGetMetadata()
   {
      assertNotNull(m_schema.getMetadata());
   }

   public void testRequiredColumnWithNullConverter()
   {
      RelationalSchema schema = new RelationalSchema();
      RelationalDatabase ds = new RelationalDatabase(null);
      DataSourceType dsType = new DataSourceType(null);
      Table table = new Table(schema);
      Column column = new Column("testColumn", table);
      Component converter = new Component(null, NullConverter.class, Component.SINGLETON);

      converter.addPrimitivePropertyInitializer("type", Primitive.TIMESTAMP);
      converter.addPrimitivePropertyInitializer("value", "1800-01-01 00:00:00");
      column.setType(Primitive.TIMESTAMP); // something that's not a String
      column.setNullable(false);
      column.setConverter(converter);
      table.setName("TestTable");
      table.addColumn(column);
      dsType.setMetadata(new XMLMetadata(null, null, null, null, null));
      ds.setType(dsType);
      schema.addTable(table);
      schema.setDataSource(ds);
      schema.generateMetaclasses(null, null, null);
      assertFalse(schema.getMetaclass("TestTable").getAttribute("testColumn").isRequired());
   }

   public void testToString()
   {
      assertEquals("RelationalSchema DefaultRelationalDatabase", m_schema.toString());
   }
}
