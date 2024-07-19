// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.util.Iterator;
import java.util.Locale;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Repository;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Symbol;

public class XMLMetadataTest extends TestCase
{
   private XMLMetadata m_meta;

   /**
    * Constructor for XMLMetadataTest.
    * @param name
    */
   public XMLMetadataTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_meta = (XMLMetadata)Repository.getMetadata();
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_meta = null;
   }
   
   public void testGetHelper()
   {
      assertNotNull(m_meta.getHelper());
   }

   public void testGetName()
   {
      assertEquals("NexJ Core Test", m_meta.getName());
   }

   public void testGetRevision()
   {
      assertEquals("1.0", m_meta.getRevision());
   }

   public void testGetNamespace()
   {
      assertEquals("http://www.nexjsystems.com/ns/core-test", m_meta.getNamespace());
   }

   public void testGetVersion()
   {
      assertEquals("12345", m_meta.getVersion());
   }

   public void testGetBaseNamespace()
   {
      assertNull(m_meta.getBaseNamespace());
   }

   public void testGetBaseVersion()
   {
      assertEquals("", m_meta.getBaseVersion());
   }

   public void testGetDataSource()
   {
      assertEquals("DefaultRelationalDatabase", m_meta.getDataSource("DefaultRelationalDatabase").getName());

      try
      {
         m_meta.getDataSource("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testGetDataSourceCount()
   {
      assertEquals(4, m_meta.getDataSourceCount());
   }

   public void testGetDataSourceIterator()
   {
      Iterator itr = m_meta.getDataSourceIterator();

      for (int i = 0, n = m_meta.getDataSourceCount(); i < n; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetChannel()
   {
      assertEquals("ObjectSystemQueue", m_meta.getChannel("ObjectSystemQueue").getName());

      try
      {
         m_meta.getChannel("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testGetChannelCount()
   {
      assertEquals(10, m_meta.getChannelCount());
   }

   public void testGetChannelIterator()
   {
      Iterator itr = m_meta.getChannelIterator();

      for (int i = 0, n = m_meta.getChannelCount(); i < n; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetMetaclass()
   {
      assertEquals("Contact", m_meta.getMetaclass("Contact").getName());

      try
      {
         m_meta.getMetaclass("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }

      Metaclass test = m_meta.getMetaclass("mod1:Test");

      assertEquals("mod1:Test", test.getName());
      assertSame(test, test.getAttribute("test").getType());
   }

   public void testGetMetaclassCount()
   {
      assertEquals(129, m_meta.getMetaclassCount()); // 1 for Object
   }

   public void testGetMetaclassIterator()
   {
      Iterator itr = m_meta.getMetaclassIterator();
      int nCount = m_meta.getMetaclassCount();

      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetClassAspect()
   {
      assertEquals("VERSIONED", m_meta.getClassAspect("VERSIONED").getName());

      try
      {
         m_meta.getClassAspect("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testGetClassAspectCount()
   {
      assertEquals(10, m_meta.getClassAspectCount()); // 1 for Object
   }

   public void testGetClassAspectIterator()
   {
      Iterator itr = m_meta.getClassAspectIterator();
      int nCount = m_meta.getClassAspectCount();

      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetFlowMacroCount()
   {
      assertEquals(1, m_meta.getFlowMacroCount());
   }

   public void testGetComponent()
   {
      assertEquals("Server.Generic", m_meta.getComponent("Server.Generic").getName());

      try
      {
         m_meta.getComponent("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testGetComponentCount()
   {
      assertEquals(28, m_meta.getComponentCount());
   }

   public void testGetComponentIterator()
   {
      Iterator itr = m_meta.getComponentIterator();
      int nCount = m_meta.getComponentCount();

      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testLibraries()
   {
      assertTrue(m_meta.getGlobalEnvironment().getVariable(
         Symbol.define("testlib-fun")) instanceof PCodeFunction);
   }

   public void testIsLocaleSupported()
   {
      assertTrue(m_meta.isLocaleSupported("en_CA"));
      assertFalse(m_meta.isLocaleSupported("en_CA_ON"));
      assertFalse(m_meta.isLocaleSupported("tlh"));
   }

   public void testGetLocaleName()
   {
      assertEquals("en_CA", m_meta.getLocaleName("en_CA"));
      assertEquals("en_CA", m_meta.getLocaleName("en_CA_ON"));
      assertEquals("en", m_meta.getLocaleName("en"));
      assertEquals("en", m_meta.getLocaleName("tlh"));
   }

   public void testGetLocale()
   {
      assertEquals(new Locale("en", "CA"), m_meta.getLocale("en_CA"));
      assertEquals(new Locale("en", "CA"), m_meta.getLocale("en_CA_ON"));
      assertEquals(new Locale("en"), m_meta.getLocale("en"));
      assertEquals(new Locale("en"), m_meta.getLocale("tlh"));
   }

   public void testGetLocaleIterator()
   {
      int nCount = 0;

      for (Iterator itr = m_meta.getLocaleIterator(); itr.hasNext();)
      {
         assertNotNull(itr.next());
         ++nCount;
      }

      assertEquals(135, nCount);
   }

   public void testGetLocaleCount()
   {
      assertEquals(135, m_meta.getLocaleCount());
   }

   public void testToString()
   {
      assertEquals("XMLMetadata \"NexJ Core Test\" 1.0", m_meta.toString());
   }
}