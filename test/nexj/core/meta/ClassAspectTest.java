// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class ClassAspectTest extends TestCase
{
   private ClassAspect m_versioned;
   private ClassAspect m_locking2;

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_versioned = Repository.getMetadata().getClassAspect("VERSIONED");
      m_locking2 = Repository.getMetadata().getClassAspect("LOCKING2");
   }

   public void testGetPointcutPattern()
   {
      assertEquals("ContactType", m_versioned.getPointcutPattern(0));
      assertEquals("AccountTypeEnum", m_versioned.getPointcutPattern(1));
   }

   public void testIsPointcutPatternInclusive()
   {
      assertTrue(m_versioned.isPointcutPatternInclusive(0));
      assertTrue(m_versioned.isPointcutPatternInclusive(1));
   }

   public void testGetPointcutPatternCount()
   {
      assertEquals(2, m_versioned.getPointcutPatternCount());
      assertEquals(0, m_locking2.getPointcutPatternCount());
   }

   public void testIsMatching()
   {
      assertFalse(m_versioned.isMatching(Repository.getMetadata().getMetaclass("ContactType")));
      assertFalse(m_versioned.isMatching(Repository.getMetadata().getMetaclass("HRRequest")));
   }
   
   public void testGetPersistenceMappingCount()
   {
      assertEquals(3, m_locking2.getPersistenceMappingCount());
   }

   public void testGetPersistenceMappingDataSource()
   {
      assertNotNull(m_locking2.getPersistenceMapping(Repository.getMetadata().getDataSource("DefaultRelationalDatabase")));
      assertNotNull(m_locking2.getPersistenceMapping(Repository.getMetadata().getDataSource("TestFilePersistenceDataSource")));
   }
}
