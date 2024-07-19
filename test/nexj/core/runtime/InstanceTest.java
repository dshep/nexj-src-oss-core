// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.util.Arrays;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.persistence.Query;
import nexj.core.persistence.sql.ReadCountHook;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.scripting.Symbol;
import nexj.core.util.Invalid;
import nexj.core.util.Undefined;
import nexj.core.util.auth.SimplePrincipal;


public class InstanceTest extends SQLDataTest
{
   public InstanceTest(String sName)
   {
      super(sName);
   }

   public void testCalculatedValue()
   {
      Instance instance = m_context.getUser();

      instance.setValue("names", instance.getValue("names"));
      instance.setValue("names", Arrays.asList(new Object[]{"abc", "def"}));
      assertEquals(Arrays.asList(new Object[]{"abc", "def"}), instance.getValue("names"));
      instance.setValue("names", Arrays.asList(new Object[]{"abc", new Integer(1), "def"}));
      assertEquals(Arrays.asList(new Object[]{"abc", "1", "def"}), instance.getValue("names"));
   }

   /**
    * Tests that updating an instance in a second work unit fails
    * if it has uncommitted changes in the first work unit.
    * 
    * @throws Exception
    */
   public void testUpdateInWrongWorkUnit() throws Exception
   {
      //Initialize InvocationContext
      Metadata metadata = Repository.getMetadata();
      InvocationContext context = new InvocationContext(metadata);

      context.setUserClass(metadata.getMetaclass("User"));
      context.initialize(new SimplePrincipal("jtest"));

      ThreadContextHolder.setContext(context);


      //Start work unit 1
      assertNull(context.beginTransaction(true));


      //Create instance in work unit 1
      Metaclass ContactClass = metadata.getMetaclass("Contact");
      Instance contact = (Instance)ContactClass.invoke("new");

      contact.setValue("firstName", "James");

      
      //Switch to work unit 2
      UnitOfWork creationUOW = context.beginTransaction(false);


      //Update in work unit 2 should fail. 
      try
      {
         contact.setValue("lastName", "Kirk");
         fail();
      }
      catch (TransactionException ex)
      {
         assertEquals("err.runtime.wrongTransaction", ex.getErrorCode());
         assertEquals("Contact", ex.getErrorArgs()[0]);
      }

      //Clean up
      context.rollbackAndResume(creationUOW);
      context.complete(false);
   }


   /**
    * Tests that deleting an instance in a second work unit fails
    * if it has uncommitted changes in the first work unit.
    * 
    * @throws Exception
    */
   public void testDeleteInWrongWorkUnit() throws Exception
   {
      //Initialize InvocationContext
      Metadata metadata = Repository.getMetadata();
      InvocationContext context = new InvocationContext(metadata);

      context.setUserClass(metadata.getMetaclass("User"));
      context.initialize(new SimplePrincipal("jtest"));

      ThreadContextHolder.setContext(context);


      //Start work unit 1
      assertNull(context.beginTransaction(true));

      //Create instance in work unit 1
      Metaclass ContactClass = metadata.getMetaclass("Contact");
      Instance contact = (Instance)ContactClass.invoke("new");
      
      contact.setValue("firstName", "Jean-luc");

      //Switch to work unit 2
      UnitOfWork creationUOW = context.beginTransaction(false);
      
      
      //Delete in work unit 2 should fail. 
      try
      {
         contact.invoke("delete");
         fail();
      }
      catch (TransactionException ex)
      {
         assertEquals("err.runtime.wrongTransaction", ex.getErrorCode());
         assertEquals("Contact", ex.getErrorArgs()[0]);
      }
      
      //Clean up
      context.rollbackAndResume(creationUOW);
      context.complete(false);
   }

   /**
    * Tests an *uncached* calculated attribute that has an initializer.
    */
   public void testUncachedCalculatedValueWithInitializer()
   {
      final Symbol INITIALIZED = Symbol.define("INITIALIZED");
      final Symbol CALCULATED = Symbol.define("CALCULATED");
      final Metaclass userClass = Repository.getMetadata().getMetaclass("User");
      final int attribute = userClass.findAttribute("regionUncached").getOrdinal();
      UnitOfWork oldUOW;
      Instance instance;

      // Get value while new --> run initializer
      // Get value once committed --> run calculated value script
      oldUOW = m_context.beginTransaction();
      instance = (Instance)userClass.invoke("new");
      assertEquals(Undefined.VALUE, instance.getValueDirect(attribute));
      assertEquals(INITIALIZED, instance.getValue(attribute));
      instance.setValue("name", "jack.test");
      instance.setValue("contact", m_context.getUser().getValue("contact"));
      m_context.commitAndResume(oldUOW);
      assertEquals(INITIALIZED, instance.getValueDirect(attribute));
      assertEquals(CALCULATED, instance.getValue(attribute));

      // Ensure that initializer is run by validation
      oldUOW = m_context.beginTransaction();
      instance = (Instance)userClass.invoke("new");
      instance.setValue("name", "jill.test");
      instance.setValue("contact", m_context.getUser().getValue("contact"));
      m_context.commitAndResume(oldUOW);
      assertEquals(INITIALIZED, instance.getValueDirect(attribute));
      assertEquals(CALCULATED, instance.getValue(attribute));
   }

   /**
    * Tests a *cached* calculated attribute that has an initializer.
    */
   public void testCachedCalculatedValueWithInitializer()
   {
      final Symbol INITIALIZED = Symbol.define("INITIALIZED");
      final Symbol CALCULATED = Symbol.define("CALCULATED");
      final Metaclass userClass = Repository.getMetadata().getMetaclass("User");
      final int attribute = userClass.findAttribute("regionCached").getOrdinal();
      UnitOfWork oldUOW;
      Instance instance;

      // Get value while new --> run initializer
      // Get value once committed --> return cached value
      oldUOW = m_context.beginTransaction();
      instance = (Instance)userClass.invoke("new");
      assertEquals(Undefined.VALUE, instance.getValueDirect(attribute));
      assertEquals(INITIALIZED, instance.getValue(attribute));
      instance.setValue("name", "jack.test");
      instance.setValue("contact", m_context.getUser().getValue("contact"));
      m_context.commitAndResume(oldUOW);
      assertEquals(INITIALIZED, instance.getValueDirect(attribute));
      assertEquals(INITIALIZED, instance.getValue(attribute));

      // Invalidate to re-calculate value
      instance.setValueDirect(attribute, Invalid.VALUE);
      assertEquals(CALCULATED, instance.getValue(attribute));
   }

   /**
    * Tests that lazy loads don't cause heterogeneous joins.
    */
   public void testLazyRelationalJoiningRelational()
   {
      Metaclass clazz = getMetadata().getMetaclass("ExternalVisit");
      ReadCountHook readHook = (ReadCountHook)((SQLAdapter)getMetadata().getDataSource("DefaultRelationalDatabase").getComponent().getInstance(m_context)).getSQLHook();
      ReadCountHook extHook = (ReadCountHook)((SQLAdapter)getMetadata().getDataSource("ExternalRelationalDatabase").getComponent().getInstance(m_context)).getSQLHook();

      assertNotNull("JUnit configuration error: default.config not up-to-date", readHook);
      assertNotNull("JUnit configuration error: default.config not up-to-date", extHook);

      InstanceArrayList list;
      Instance inst;
      int nStartReadCount = readHook.getReadCount();
      int nExtReadCount = extHook.getReadCount();

      list = (InstanceArrayList)Query.createRead(clazz,
         parse("(version startDate endDate)"), null,
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(8, list.getCount());
      inst = list.getInstance(0);
      assertEquals(nStartReadCount, readHook.getReadCount());
      assertEquals(nExtReadCount + 1, extHook.getReadCount());
      nExtReadCount = extHook.getReadCount();

      // This causes a lazy load of ExternalVisit, and will instantiate a lazy Patient instance
      assertEquals("Broken foot", inst.getValue("reason"));
      assertEquals(nExtReadCount + 1, extHook.getReadCount());
      assertEquals(nStartReadCount, readHook.getReadCount());
      inst = (Instance)inst.getValue("patient");
      assertEquals(nStartReadCount, readHook.getReadCount());

      // This will cause the read of Patient
      assertEquals("Sarah", inst.getValue("firstName"));
      assertEquals("Johnson", inst.getValue("lastName"));
      assertEquals(nStartReadCount + 1, readHook.getReadCount());
      assertEquals(nExtReadCount + 1, extHook.getReadCount());
   }
}
