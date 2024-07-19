// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.io.StringReader;
import java.util.Collection;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.OID;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.Undefined;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

/**
 * Tests the Virtual Persistence Adapter.
 */
public class VirtualAdapterTest extends TestCase
{
   // associations
   protected InvocationContext m_context;
   protected Metadata m_metadata;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(VirtualAdapterTest.class);

   // operations

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_metadata = Repository.getMetadata();
      m_context = new InvocationContext(m_metadata);

      try
      {
         m_context.initialize(null);
         m_context.getMachine().eval(
            Pair.list(Symbol.LOAD, VirtualAdapterTest.class.getResource("datastore.scm").toString())
         );
      }
      catch (Throwable t)
      {
         ThreadContextHolder.setContext(null);
         ObjUtil.rethrow(t);
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("==================== Running " + getName() + " ====================");
      }
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("==================== Finished " + getName() + " ====================");
      }

      m_context.complete(false);
      ThreadContextHolder.setContext(null);
      super.tearDown();

      m_metadata = null;
      m_context = null;
   }

   /**
    * Parses a Scheme S-expression.
    * @param sExpr The string to parse.
    * @return The parse tree.
    */
   protected Pair parse(String sExpr)
   {
      return (Pair)new SchemeParser(m_context.getMachine().getGlobalEnvironment())
         .parse(new StringReader(sExpr), null);
   }

   /**
    * Tests that the persistence root of derived virtual classes is computed correctly.
    * (Derived & base must use class codes, derived class mapping must have "derived == true",
    * and they must be on the same datasource).
    */
   public void testPersistenceRoot()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass domAnimalClass = m_metadata.getMetaclass("DomesticAnimal");

      assertSame(animalClass, domAnimalClass.getPersistenceRoot());
   }

   /**
    * Tests reads.
    */
   public void testRead()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst = null;
      InstanceArrayList list = null;

      // Read all
      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         null, null, parse("((commonName . #t))"), null, null, null
      });

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      assertEquals("Beaver", inst.getValue("commonName"));
      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      inst = list.getInstance(2);
      assertEquals("Dog", inst.getValue("commonName"));
      inst = list.getInstance(3);
      assertEquals("Spider", inst.getValue("commonName"));

      m_context.initialize(m_context.getPrincipal(), m_context.getMachine().getGlobalEnvironment());

      // Read just 4-legged animals
      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         parse("(commonName)"), Pair.list(Symbol.EQ, Pair.attribute("limbCount"), Primitive.createInteger(4)), null, null, null, null
      });

      assertEquals(3, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      assertEquals("Felis catus", inst.getValue("scientificName"));
      inst = list.getInstance(2);
      assertEquals("Beaver", inst.getValue("commonName"));

      // Read all with a cursor
      Cursor cur = Query.createRead(animalClass, null, null, null, -1, 0, false, Query.SEC_ALL, m_context).openCursor();

      inst = cur.next();
      assertEquals("Dog", inst.getValue("commonName"));
      inst = cur.next();
      assertEquals("Cat", inst.getValue("commonName"));
      inst = cur.next();
      assertEquals("Spider", inst.getValue("commonName"));
      inst = cur.next();
      assertEquals("Beaver", inst.getValue("commonName"));
      assertNull(cur.next());

      // Read all with a cursor, canceling early
      cur = Query.createRead(animalClass, null, null, null, -1, 0, false, Query.SEC_ALL, m_context).openCursor();

      inst = cur.next();
      assertEquals("Dog", inst.getValue("commonName"));
      cur.close();
      assertNull(cur.next());
      assertNull(cur.next());

      // Read class that has 2-part OID
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");

      list = (InstanceArrayList)penClass.invoke("read", new Object[] {
         null, Pair.list(Symbol.EQ, Pair.attribute("name"), "Dog pen"), null, null, null, null
      });

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals(0, ((Integer)inst.getValue("locking")).intValue());
      assertEquals(2, inst.getOID().getCount());
      assertEquals("North", inst.getOID().getValue(0));
      assertEquals(8, ((Integer)inst.getOID().getValue(1)).intValue());
      assertEquals("North", inst.getValue("k1"));
      assertEquals(8, ((Integer)inst.getValue("k2")).intValue());

      // Read parent, getting sub-class attribute.
      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         parse("(commonName (@@ DomesticAnimal petName))"), parse("(= (@ commonName) \"Dog\")"), null, null, null, null
      });

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("DomesticAnimal", inst.getClassName());
      assertEquals("Dog", inst.getValue("commonName"));
      assertEquals("Kerberos", inst.getValue("petName"));

      // Read sub-class directly.
      Metaclass domAnimalClass = m_metadata.getMetaclass("DomesticAnimal");

      list = (InstanceArrayList)domAnimalClass.invoke("read", new Object[]{
         parse("(petName commonName)"), parse("(= (@ petName) \"Cleopatra\")"), null, null, null, null
      });

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("DomesticAnimal", inst.getClassName());
      assertEquals("Cat", inst.getValue("commonName"));
   }

   /**
    * Tests that read result without OID throws an error.
    */
   public void testReadNotReturningOID()
   {
      Metaclass testClass = m_metadata.getMetaclass("TestVirtualClass");

      try
      {
         // Read by a1, which is broken: doesn't set the TransferObject's OID
         Query.createRead(testClass,
            parse("(a1)"), parse("(= (@ a) 1)"), null, -1, 0, false, Query.SEC_ALL, m_context
         ).read();
         fail();
      }
      catch (PersistenceException ex)
      {
         assertEquals("err.persistence.virtual.missingOID", ex.getErrorCode());
      }
   }

   /**
    * Tests that missing requested attributes become null.
    */
   public void testReadNullAssocAttribute()
   {
      Metaclass testClass = m_metadata.getMetaclass("TestVirtualClass");
      Instance inst = Query.createRead(testClass,
         parse("(a1 b1 b2)"), parse("(= (@ b) 1)"), null, -1, 0, false, Query.SEC_ALL, m_context
      ).read().getInstance(0);

      assertEquals("Test", inst.getValue("a1"));
      assertNull(inst.getValue("b1"));
      assertNull(inst.getValue("b2"));
   }

   /**
    * Tests that an exception is thrown if the locking attribute is not updated.
    */
   public void testUpdateWithoutChangingLocking()
   {
      Metaclass testClass = m_metadata.getMetaclass("TestVirtualClass");
      Instance inst = new Instance(testClass, m_context);

      inst.setOID(new OID(new Object[]{Binary.parse("0013")}));
      inst.setValue("locking", Primitive.createLong(17L));
      inst.setClean();

      try
      {
         inst.setValue("a1", "updated!");
         m_context.complete(true);
      }
      catch (PersistenceException ex)
      {
         assertEquals("err.persistence.virtual.lockingNotModified", ex.getErrorCode());
      }
   }

   /**
    * Tests that a read mapping can set attributes not in the attribute read list.
    * Missing unrequested attributes are left undefined.
    */
   public void testReadAttributesNotInList()
   {
      Metaclass testClass = m_metadata.getMetaclass("TestVirtualClass");
      Instance inst = Query.createRead(testClass,
         parse("(c1)"), parse("(= (@ c) 1)"), null, -1, 0, false, Query.SEC_ALL, m_context
      ).read().getInstance(0);

      assertEquals("Requested", inst.getValue("c1"));
      assertEquals("Extra", inst.getValue("b2"));  // This is done without performing a lazy load.
      assertNull(inst.getValue("a1"));
      assertSame(Undefined.VALUE, inst.getValueDirect(testClass.findAttribute("b1").getOrdinal()));

      Instance assoc = (Instance)inst.getValue("b3");

      assertEquals(testClass, assoc.getLazyMetaclass());
      assertTrue(assoc.isLazy());
      assertEquals(parseEval("(oid #z2347)"), assoc.getOID());
   }

   /**
    * Tests that null elements of a collection get skipped.
    */
   public void testReadCollectionWithNullValue()
   {
      Metaclass testClass = m_metadata.getMetaclass("TestVirtualClass");
      InstanceList list = Query.createRead(testClass,
         parse("(a1)"), parse("(= (@ d) 1)"), null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();
      Instance inst;

      assertEquals(2, list.size());
      inst = list.getInstance(0);
      assertEquals("Element1", inst.getValue("a1"));
      inst = list.getInstance(1);
      assertEquals("Element3", inst.getValue("a1"));
   }

   /**
    * Tests that an empty collection means 0 results.
    */
   public void testReadEmptyCollection()
   {
      Metaclass testClass = m_metadata.getMetaclass("TestVirtualClass");

      assertEquals(0, Query.createRead(testClass,
         parse("(a1)"), parse("(= (@ e) 1)"), null, -1, 0, false, Query.SEC_ALL, m_context
      ).read().size());
   }

   public void testWhereClauseOID1()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      InstanceList list = Query.createRead(animalClass,
         null, parseEval("`(= (@ virtFKToRelColl) ,(read-instance PetOwner '() '(= (@ firstName) \"Zoe\") #f))"),
         null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();

      assertEquals(2, list.size());
      assertEquals("Dog", list.getInstance(0).getValue("commonName"));
      assertEquals("Cat", list.getInstance(1).getValue("commonName"));
   }

   /**
    * Tests single instance delete (via a batch-delete mapping) and optimistic locking.
    */
   public void testDelete()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst = null;
      InstanceList list = null;

      // Read all
      list = Query.createRead(animalClass,
         null, null, null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      inst.invoke("delete");
      m_context.getUnitOfWork().commit();

      // Optimistic locking
      list = Query.createRead(animalClass,
         null, null, null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();

      assertEquals(3, list.size());
      m_context.getMachine().eval(parse("((cdr ((AnimalStore'data)'get 0))'locking 10)"));
      inst = list.getInstance(0);
      inst.invoke("delete");

      try
      {
         m_context.getUnitOfWork().commit();
         fail("Expected OptimisticLockException");
      }
      catch (OptimisticLockException ex)
      {
      }

      m_context.getUnitOfWork().rollback();
   }

   /**
    * Tests single instance update and optimistic locking detection when the
    * instance is updated or deleted elsewhere.
    */
   public void testUpdate()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst = null;
      InstanceArrayList list = null;
      Collection data;

      // Read dog.
      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         parse("(scientificName)"), parse("(= (@ commonName) \"Dog\")"), null, null, null, null
      });

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertNull(inst.getValue("scientificName"));
      inst.setValue("scientificName", "Canis lupus familiaris");
      inst.setValue("petName", "Cerberus");  // Update sub-class attribute
      m_context.getUnitOfWork().commit();

      // Read dog again.
      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         null, Pair.list(Symbol.EQ, Pair.attribute("commonName"), "Dog"), null, null, null, null
      });

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Canis lupus familiaris", inst.getValue("scientificName"));
      assertEquals("Cerberus", inst.getValue("petName"));

      // Test optimistic locking detection (when updated elsewhere)
      inst.setValue("scientificName", "Doggie");
      m_context.getMachine().eval(parse("((cdr ((AnimalStore'data)'get 0))'locking " +
         (((Integer)inst.getValue("locking")).intValue() + 1) + ")"));

      try
      {
         m_context.getUnitOfWork().commit();
         fail("Expected OptimisticLockException");
      }
      catch (OptimisticLockException ex)
      {
      }

      m_context.getUnitOfWork().rollback();

      // Test optimistic locking detection (when deleted elsewhere)
      inst.setValue("scientificName", "Doggie");
      m_context.getMachine().eval(parse("((AnimalStore'data)'remove 0)"));
      data = (Collection)m_context.getMachine().eval(parse("(AnimalStore'data)"));
      assertEquals(3, data.size());

      try
      {
         m_context.getUnitOfWork().commit();
         fail("Expected OptimisticLockException");
      }
      catch (OptimisticLockException ex)
      {
      }

      m_context.getUnitOfWork().rollback();
   }

   /**
    * Tests batch update.
    */
   public void testUpdateBatch()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst = null;
      InstanceArrayList list = null;

      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         null, null, null, null, null, null
      });

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      inst.setValue("limbCount", Primitive.ZERO_INTEGER);
      inst = list.getInstance(1);
      inst.setValue("limbCount", Primitive.ZERO_INTEGER);
      inst = list.getInstance(2);
      inst.setValue("limbCount", Primitive.ZERO_INTEGER);
      inst = list.getInstance(3);
      inst.setValue("limbCount", Primitive.ZERO_INTEGER);

      int nStartCallCount = ((Integer)animalClass.getValue("updateMapping2Count")).intValue();

      m_context.getUnitOfWork().commit();

      int nEndCallCount = ((Integer)animalClass.getValue("updateMapping2Count")).intValue();

      try
      {
         assertEquals(nStartCallCount + 2, nEndCallCount);  // Update Animal and DomesticAnimal.
      }
      catch (AssertionFailedError ex)
      {
         assertEquals("Furthermore, the number of calls is not consistent with no batching.",
            nStartCallCount + 3, nEndCallCount);

         throw ex;
      }

      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         null, null, null, null, null, null
      });

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      assertEquals(0, ((Integer)inst.getValue("limbCount")).intValue());
      inst = list.getInstance(1);
      assertEquals(0, ((Integer)inst.getValue("limbCount")).intValue());
      inst = list.getInstance(2);
      assertEquals(0, ((Integer)inst.getValue("limbCount")).intValue());
      inst = list.getInstance(3);
      assertEquals(0, ((Integer)inst.getValue("limbCount")).intValue());
   }

   /**
    * Tests that updating attributes that are mapped to the object key changes the object key.
    */
   public void testUpdateChangeOID()
   {
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      InstanceList list;
      Instance inst;

      list = Query.createRead(penClass,
         parse("(name)"), parseEval("`(= (@) ,(oid \"North\" 8))"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog pen", inst.getValue("name"));

      // Change the OID-mapped attributes
      inst.setValue("k1", "South");
      inst.setValue("k2", Primitive.createInteger(2));
      m_context.getUnitOfWork().commit();

      // Verify that OID has changed.
      list = Query.createRead(penClass,
         parse("(name)"), parseEval("`(= (@) ,(oid \"North\" 8))"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(0, list.size());
      list = Query.createRead(penClass,
         parse("(name)"), parseEval("`(= (@) ,(oid \"South\" 2))"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog pen", inst.getValue("name"));
   }

   /**
    * Tests dependency sorting among work items.
    */
   public void testUpdateWithAssocToCreated()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Instance animal, pen;
      InstanceList list;

      // Create new pen & update existing animal
      pen = (Instance)penClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "Chimpanzee pen")
      });
      list = Query.createRead(animalClass,
         parse("(commonName pen)"), parse("(= (@ commonName) \"Beaver\")"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      animal = list.getInstance(0);
      assertEquals("Beaver", animal.getValue("commonName"));
      assertNull(animal.getValue("pen"));
      animal.setValue("pen", pen);

      m_context.getUnitOfWork().commit();

      // Read the animal, check the assoc to its pen
      list = Query.createRead(animalClass,
         parse("(commonName pen)"), parse("(= (@ commonName) \"Beaver\")"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      animal = list.getInstance(0);
      assertEquals("Beaver", animal.getValue("commonName"));
      pen = (Instance)animal.getValue("pen");
      assertEquals("Chimpanzee pen", pen.getValue("name"));

      // Read the pen, check the collection of its animals
      list = Query.createRead(penClass,
         parse("(name animals)"), parse("(= (@ name) \"Chimpanzee pen\")"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      pen = list.getInstance(0);
      assertEquals("Chimpanzee pen", pen.getValue("name"));
      list = (InstanceList)pen.getValue("animals");
      assertEquals(1, list.size());
      animal = list.getInstance(0);
      assertEquals("Beaver", animal.getValue("commonName"));
   }

   /**
    * Tests creation of a single instance.
    */
   public void testCreate()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst = null;
      InstanceArrayList list = null;
      Collection data;

      data = (Collection)m_context.getMachine().eval(parse("(AnimalStore'data)"));
      assertEquals(4, data.size());

      // Create new animal
      inst = (Instance)animalClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("commonName"), "Chimpanzee"),
         new Pair(Symbol.define("scientificName"), "Pan troglodytes"),
         new Pair(Symbol.define("limbCount"), Primitive.createInteger(4))
      });
      data = (Collection)m_context.getMachine().eval(parse("(AnimalStore'data)"));
      assertEquals(4, data.size());

      m_context.getUnitOfWork().commit();

      data = (Collection)m_context.getMachine().eval(parse("(AnimalStore'data)"));
      assertEquals(5, data.size());

      // Read it
      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         null, Pair.list(Symbol.EQ, Pair.attribute("commonName"), "Chimpanzee"), null, null, null, null
      });
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Pan troglodytes", inst.getValue("scientificName"));
   }

   /**
    * Tests creating an instance where the OID is set by the create mapping.
    */
   public void testCreateWithExternallyGeneratedOID()
   {
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Instance inst;
      Collection data;

      data = (Collection)m_context.getMachine().eval(parse("(AnimalPenStore'data)"));
      assertEquals(2, data.size());

      inst = (Instance)penClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "Monkey pen")
      });

      data = (Collection)m_context.getMachine().eval(parse("(AnimalPenStore'data)"));
      assertEquals(2, data.size());
      assertNull(inst.getOID());

      m_context.getUnitOfWork().commit();

      data = (Collection)m_context.getMachine().eval(parse("(AnimalPenStore'data)"));
      assertEquals(3, data.size());

      assertEquals(2, inst.getOID().getCount());
      assertEquals("AUTO", inst.getOID().getValue(0));
      assertEquals(1, ((Integer)inst.getOID().getValue(1)).intValue());
   }

   /**
    * Tests creating an instance where the OID parts are specified in the instance attributes.
    */
   public void testCreateWithOIDFromAttributes()
   {
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Instance inst;
      Collection data;
      InstanceArrayList list;
      OID oid;

      data = (Collection)m_context.getMachine().eval(parse("(AnimalPenStore'data)"));
      assertEquals(2, data.size());

      inst = (Instance)penClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "Monkey pen"),
         new Pair(Symbol.define("k1"), "ATTR_SRC"),
         new Pair(Symbol.define("k2"), Primitive.createInteger(42))
      });

      data = (Collection)m_context.getMachine().eval(parse("(AnimalPenStore'data)"));
      assertEquals(2, data.size());
      assertNull(inst.getOID());

      m_context.getUnitOfWork().commit();

      data = (Collection)m_context.getMachine().eval(parse("(AnimalPenStore'data)"));
      assertEquals(3, data.size());

      assertEquals(2, inst.getOID().getCount());
      assertEquals("ATTR_SRC", inst.getOID().getValue(0));
      assertEquals(42, ((Integer)inst.getOID().getValue(1)).intValue());
      assertEquals("ATTR_SRC", inst.getValue("k1"));
      assertEquals(42, ((Integer)inst.getValue("k2")).intValue());
      oid = (OID)m_context.getMachine().eval(parse("(car ((AnimalPenStore'data)'get 2))"));
      assertEquals("ATTR_SRC", oid.getValue(0));
      assertEquals(42, ((Integer)oid.getValue(1)).intValue());

      list = (InstanceArrayList)penClass.invoke("read", new Object[] {
         null, Pair.list(Symbol.EQ, Pair.attribute("name"), "Monkey pen"), null, null, null, null
      });

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("ATTR_SRC", inst.getValue("k1"));
      assertEquals(42, ((Integer)inst.getValue("k2")).intValue());
   }

   /**
    * Tests dependency sorting among work items.
    */
   public void testCreateMultipleWithAssoc()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Instance animal, pen;
      InstanceList list;

      // Create new animal & pen
      pen = (Instance)penClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "Chimpanzee pen")
      });
      animal = (Instance)animalClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("commonName"), "Chimpanzee"),
         new Pair(Symbol.define("scientificName"), "Pan troglodytes"),
         new Pair(Symbol.define("limbCount"), Primitive.createInteger(4))
      });
      animal.setValue("pen", pen);

      m_context.getUnitOfWork().commit();

      // Read the animal, check the assoc to its pen
      list = Query.createRead(animalClass,
         parse("(commonName pen)"), parse("(= (@ commonName) \"Chimpanzee\")"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      animal = list.getInstance(0);
      assertEquals("Chimpanzee", animal.getValue("commonName"));
      pen = (Instance)animal.getValue("pen");
      assertEquals("Chimpanzee pen", pen.getValue("name"));

      // Read the pen, check the collection of its animals
      list = Query.createRead(penClass,
         parse("(name animals)"), parse("(= (@ name) \"Chimpanzee pen\")"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      pen = list.getInstance(0);
      assertEquals("Chimpanzee pen", pen.getValue("name"));
      list = (InstanceList)pen.getValue("animals");
      assertEquals(1, list.size());
      animal = list.getInstance(0);
      assertEquals("Pan troglodytes", animal.getValue("scientificName"));
   }

   /**
    * Tests reading an association.
    */
   public void testReadManyToOneHeterogeneousAssoc()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Instance inst, assocInst;
      InstanceList list;
      int nStartCallCount, nEndCallCount;

      list = Query.createRead(animalClass,
         parse("(commonName limbCount pen)"), null, null, -1, 0, false, Query.SEC_NODE, m_context
      ).read();

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      nStartCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Dog pen", assocInst.getValue("name"));
      nEndCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount + 1, nEndCallCount);  // Lazy loaded
      assertEquals(2, ((InstanceList)assocInst.getValue("animals")).size());
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(0));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      nStartCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Cat pen", assocInst.getValue("name"));
      nEndCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount + 1, nEndCallCount);  // Lazy loaded
      assertEquals(1, ((InstanceList)assocInst.getValue("animals")).size());
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(0));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      nStartCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Dog pen", assocInst.getValue("name"));
      nEndCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded by a previous lazy load
      assertEquals(2, ((InstanceList)assocInst.getValue("animals")).size());
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(1));
   }

   /**
    * Tests reading a 1:1 association, using heterogeneous joins,
    * reading the attribute where the FK is stored.
    */
   public void testReadOneToOneHeterogeneousAssocForward()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      InstanceList list;
      Instance inst, assoc;

      list = (InstanceList)Query.createRead(animalClass, parse("(commonName (chaserOf commonName) (assocToPrimitiveFK commonName) (@@ DomesticAnimal petFriend commonName))"),
         parse("(= (@ commonName) \"Dog\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assoc = (Instance)inst.getValue("chaserOf");
      assertEquals("Cat", assoc.getValue("commonName"));
      assoc = (Instance)inst.getValue("assocToPrimitiveFK");
      assertEquals("Spider", assoc.getValue("commonName"));
      assoc = (Instance)inst.getValue("petFriend");
      assertEquals("Cat", assoc.getValue("commonName"));
      

      list = Query.createRead(animalClass, parse("(commonName (chaserOf commonName))"),
         parse("(= (@ commonName) \"Beaver\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Beaver", inst.getValue("commonName"));
      assertNull(inst.getValue("chaserOf"));
      assertNull(inst.getValue("assocToPrimitiveFK"));
   }

   /**
    * Tests reading a 1:1 association, using heterogeneous joins,
    * reading the reverse of the attribute where the FK is stored.
    */
   public void testReadOneToOneHeterogeneousAssocReverse()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      InstanceList list;
      Instance inst, assoc;

      list = Query.createRead(animalClass, parse("(commonName (chasedBy commonName))"),
         parse("(= (@ commonName) \"Cat\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Cat", inst.getValue("commonName"));
      assoc = (Instance)inst.getValue("chasedBy");
      assertEquals("Dog", assoc.getValue("commonName"));

      list = Query.createRead(animalClass, parse("(commonName (chasedBy commonName))"),
         parse("(= (@ commonName) \"Beaver\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Beaver", inst.getValue("commonName"));
      assertNull(inst.getValue("chasedBy"));
   }

   public void testUpdateOneToOneHeterogeneousAssoc() throws Exception
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      InstanceList list, list2;
      Instance inst, inst2, assoc;

      list = Query.createRead(animalClass, parse("(commonName (chaserOf commonName))"),
         parse("(= (@ commonName) \"Cat\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Cat", inst.getValue("commonName"));
      assertNull(inst.getValue("chaserOf"));

      list2 = Query.createRead(animalClass, parse("(commonName chaserOf chasedBy)"),
         parse("(= (@ commonName) \"Spider\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list2.size());
      inst2 = list2.getInstance(0);
      assertEquals("Spider", inst2.getValue("commonName"));
      assertNull(inst2.getValue("chasedBy"));

      inst.setValue("chaserOf", inst2);
      m_context.getUnitOfWork().commit();

      // Verify
      list = Query.createRead(animalClass, parse("(commonName (chaserOf commonName))"),
         parse("(= (@ commonName) \"Cat\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Cat", inst.getValue("commonName"));
      assoc = (Instance)inst.getValue("chaserOf");
      assertEquals("Spider", assoc.getValue("commonName"));

      list2 = Query.createRead(animalClass, parse("(commonName chaserOf chasedBy)"),
         parse("(= (@ commonName) \"Spider\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list2.size());
      inst2 = list2.getInstance(0);
      assertEquals("Spider", inst2.getValue("commonName"));
      assoc = (Instance)inst2.getValue("chasedBy");
      assertEquals("Cat", assoc.getValue("commonName"));
   }

   public void testCreateOneToOneHeterogeneousAssoc()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst, assoc;
      InstanceList list;

      // Read the animal to associate with
      list = Query.createRead(animalClass, parse("(commonName)"),
         parse("(= (@ commonName) \"Cat\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assoc = list.getInstance(0);

      // Create new animal
      inst = (Instance)animalClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("commonName"), "Chimpanzee"),
         new Pair(Symbol.define("scientificName"), "Pan troglodytes"),
         new Pair(Symbol.define("limbCount"), Primitive.createInteger(4)),
         new Pair(Symbol.define("chaserOf"), assoc)
      });

      m_context.getUnitOfWork().commit();

      // Read it
      list = Query.createRead(animalClass, parse("(commonName (chaserOf commonName))"),
         parse("(= (@ commonName) \"Chimpanzee\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Pan troglodytes", inst.getValue("scientificName"));
      assoc = (Instance)inst.getValue("chaserOf");
      assertEquals("Cat", assoc.getValue("commonName"));
   }

   /**
    * Tests reading a 1:1 association, using homogeneous joins (composition mapping),
    * reading the composition attribute itself.
    */
   public void testReadOneToOneHomogeneousAssocForward()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      InstanceList list;
      Instance inst, assoc;

      list = (InstanceList)Query.createRead(animalClass, parse("(commonName (fondOf commonName))"),
         parse("(= (@ commonName) \"Cat\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Cat", inst.getValue("commonName"));
      assoc = (Instance)inst.getValue("fondOf");
      assertEquals("Spider", assoc.getValue("commonName"));

      list = (InstanceList)Query.createRead(animalClass, parse("(commonName (fondOf commonName))"),
         parse("(= (@ commonName) \"Spider\")"), null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("Spider", inst.getValue("commonName"));
      assertNull(inst.getValue("fondOf"));
   }

   /**
    * Tests that multiple associations (and levels of association) can be read
    * in a single query.
    */
   public void testReadMultipleAssocs()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      InstanceList list, col;
      Instance inst, assoc1, assoc2, assoc21, assoc3, assoc31;

      list = (InstanceList)Query.createRead(animalClass,
         parse("(commonName (licenses name) (fondOf commonName (fondOf commonName)) (pen name) (chaserOf commonName (pen name)))"),
         parse("(= (@ commonName) \"Dog\")"),
         null, -1, 0, false, Query.SEC_NODE, m_context).read();

      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));

      assoc1 = (Instance)inst.getValue("pen");
      assertEquals("Dog pen", assoc1.getValue("name"));

      assoc2 = (Instance)inst.getValue("chaserOf");
      assertEquals("Cat", assoc2.getValue("commonName"));
      assoc21 = (Instance)assoc2.getValue("pen");
      assertEquals("Cat pen", assoc21.getValue("name"));

      col = (InstanceList)inst.getValue("licenses");
      assertEquals(2, col.size());
      assertEquals("City Dog License", col.getInstance(0).getValue("name"));

      assoc3 = (Instance)inst.getValue("fondOf");
      assertEquals("Beaver", assoc3.getValue("commonName"));

      assoc31 = (Instance)assoc3.getValue("fondOf");
      assertEquals("Spider", assoc31.getValue("commonName"));
   }

   /**
    * Tests reading an association, with primitive attributes to read specified
    * to avoid lazy loads.
    */
   public void testReadManyToOneHeterogeneousAssocWithNonLazyPrimitivesOnAssociatedInstances()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Instance inst, assocInst;
      InstanceArrayList list;
      int nStartCallCount, nEndCallCount;

      list = (InstanceArrayList)animalClass.invoke("read", new Object[]{
         parse("(commonName limbCount (pen name))"), null, null, null, null, null
      });

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      nStartCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Dog pen", assocInst.getValue("name"));
      nEndCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded during read
      assertEquals(2, ((InstanceList)assocInst.getValue("animals")).size());
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(0));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      nStartCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Cat pen", assocInst.getValue("name"));
      nEndCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded during read
      assertEquals(1, ((InstanceList)assocInst.getValue("animals")).size());
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(0));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      nStartCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Dog pen", assocInst.getValue("name"));
      nEndCallCount = ((Integer)penClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded during read
      assertEquals(2, ((InstanceList)assocInst.getValue("animals")).size());
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(1));
   }

   /**
    * Tests reading an association, with primitive and association attributes
    * specified to avoid lazy loads.
    */
   public void testReadManyToOneHeterogeneousAssocWithNonLazyCollectionsOnAssociatedInstances()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst, assocInst;
      InstanceList list;
      int nStartCallCount, nEndCallCount;

      list = Query.createRead(animalClass, parse("(commonName limbCount (pen name animals))"),
         null, null, -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      assertEquals("Dog pen", assocInst.getValue("name"));
      nStartCallCount = ((Integer)animalClass.getValue("penAttrReadCount")).intValue();
      assertEquals(2, ((InstanceList)assocInst.getValue("animals")).size());
      nEndCallCount = ((Integer)animalClass.getValue("penAttrReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded during read
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(0));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      assertEquals("Cat pen", assocInst.getValue("name"));
      nStartCallCount = ((Integer)animalClass.getValue("penAttrReadCount")).intValue();
      assertEquals(1, ((InstanceList)assocInst.getValue("animals")).size());
      nEndCallCount = ((Integer)animalClass.getValue("penAttrReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded during read
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(0));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      assocInst = (Instance)inst.getValue("pen");
      assertNotNull(assocInst);
      assertEquals("Dog pen", assocInst.getValue("name"));
      nStartCallCount = ((Integer)animalClass.getValue("penAttrReadCount")).intValue();
      assertEquals(2, ((InstanceList)assocInst.getValue("animals")).size());
      nEndCallCount = ((Integer)animalClass.getValue("penAttrReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Loaded during read
      assertSame(inst, ((InstanceList)assocInst.getValue("animals")).getInstance(1));
   }

   /**
    * Tests reading an association sub-collection.
    */
   public void testReadHeterogeneousCollection()
   {
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst, assocInst;
      InstanceArrayList list, collection;
      int nStartCallCount, nEndCallCount;

      list = (InstanceArrayList)penClass.invoke("read", new Object[]{
         parse("(name animals)"), null, null, null, null, null
      });

      assertEquals(2, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog pen", inst.getValue("name"));
      collection = (InstanceArrayList)inst.getValue("animals");
      assertEquals(2, collection.size());
      assocInst = collection.getInstance(0);
      nStartCallCount = ((Integer)animalClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Dog", assocInst.getValue("commonName"));
      nEndCallCount = ((Integer)animalClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);  // Not lazy loaded
      assertSame(inst, assocInst.getValue("pen"));
      assocInst = collection.getInstance(1);
      assertEquals("Spider", assocInst.getValue("commonName"));
      assertSame(inst, assocInst.getValue("pen"));

      inst = list.getInstance(1);
      assertEquals("Cat pen", inst.getValue("name"));
      collection = (InstanceArrayList)inst.getValue("animals");
      assertEquals(1, collection.size());
      assocInst = collection.getInstance(0);
      assertEquals("Cat", assocInst.getValue("commonName"));
      assertSame(inst, assocInst.getValue("pen"));
   }

   /**
    * Same as testReadCollection, except that the attributes list has "commonName" in
    * it, e.g. (name (animals commonName)), so no lazy loads should be executed.
    */
   public void testReadHeterogeneousCollectionWithNonLazyPrimitives()
   {
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Instance inst, assocInst;
      InstanceArrayList list, collection;
      int nStartCallCount, nEndCallCount;

      list = (InstanceArrayList)penClass.invoke("read", new Object[]{
         parse("(name (animals commonName (@@ DomesticAnimal petName)))"), null, null, null, null, null
      });

      assertEquals(2, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog pen", inst.getValue("name"));
      collection = (InstanceArrayList)inst.getValue("animals");
      assertEquals(2, collection.size());
      assocInst = collection.getInstance(0);
      nStartCallCount = ((Integer)animalClass.getValue("objectKeyReadCount")).intValue();
      assertEquals("Dog", assocInst.getValue("commonName"));  // Loaded during read
      assertEquals("Kerberos", assocInst.getValue("petName"));
      nEndCallCount = ((Integer)animalClass.getValue("objectKeyReadCount")).intValue();
      assertEquals(nStartCallCount, nEndCallCount);
      assertSame(inst, assocInst.getValue("pen"));
      assocInst = collection.getInstance(1);
      assertEquals("Spider", assocInst.getValue("commonName"));
      assertFalse(assocInst.hasValue("petName"));
      assertSame(inst, assocInst.getValue("pen"));

      inst = list.getInstance(1);
      assertEquals("Cat pen", inst.getValue("name"));
      collection = (InstanceArrayList)inst.getValue("animals");
      assertEquals(1, collection.size());
      assocInst = collection.getInstance(0);
      assertEquals("Cat", assocInst.getValue("commonName"));
      assertEquals("Cleopatra", assocInst.getValue("petName"));
      assertSame(inst, assocInst.getValue("pen"));
   }

   /**
    * Reads a sub-collection whose values are provided by a composition mapping.
    */
   public void testReadHomogeneousCollection()
   {
      Metaclass domesticAnimalClass = m_metadata.getMetaclass("Animal");
      InstanceArrayList list, collection;
      Instance inst, inst2;

      list = (InstanceArrayList)domesticAnimalClass.invoke("read", new Object[]{
         parse("(commonName (licenses name))"), null, null, null, null, null
      });

      assertEquals(4, list.size());
      inst = list.getInstance(0);

      assertEquals("Dog", inst.getValue("commonName"));
      collection = (InstanceArrayList)inst.getValue("licenses");
      assertEquals(2, collection.size());
      inst2 = collection.getInstance(0);
      assertEquals("City Dog License", inst2.getValue("name"));
      assertEquals(new OID(new Object[]{Binary.parse("80")}), inst2.getOID());
      inst2 = collection.getInstance(1);
      assertEquals("Vaccination Certificate for Kerberos the dog", inst2.getValue("name"));
      assertEquals(new OID(new Object[]{Binary.parse("81")}), inst2.getOID());

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      collection = (InstanceArrayList)inst.getValue("licenses");
      assertEquals(0, collection.size());

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      collection = (InstanceArrayList)inst.getValue("licenses");
      assertEquals(0, collection.size());

      inst = list.getInstance(3);
      assertEquals("Beaver", inst.getValue("commonName"));
      collection = (InstanceArrayList)inst.getValue("licenses");
      assertEquals(0, collection.size());
   }

   public void testGetSortKeys()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");

      assertEquals("(" +
         "(#t ((@) . #t)) " +
         "(#f (commonName . #t)) " +
         "(#f (scientificName . #t) (commonName . #t)) " +
         "(#t (commonName . #t) ((@) . #t)) " +
         "(#t (commonName . #f) ((@) . #t)) " +
         "(#f (limbCount . #t) (commonName . #t) (scientificName . #t)) " +
         "(#f (limbCount . #t) (scientificName . #t) (commonName . #t))" +
         ")", String.valueOf(animalClass.getSortKeys(null, null)));

      assertEquals("(" +
         "(#t ((@) . #t)) " +
         "(#f (commonName . #t))" +
         ")", String.valueOf(animalClass.getSortKeys(new Attribute[]{penClass.getAttribute("animals")}, null)));

      assertEquals("(" +
         "(#t (name . #t) (k1 . #t) (k2 . #t)) " +
         "(#t (k1 . #t) (k2 . #t)) " +
         "(#t (k1 . #t) (name . #t) (k2 . #t))" +
         ")", String.valueOf(penClass.getSortKeys(null, null)));
   }

   /**
    * Tests reading an instance-cached class directly.
    */
   public void testReadCached()
   {
      Metaclass clazz = m_metadata.getMetaclass("CachedVirtual");
      InstanceList list;
      Instance inst;

      m_context.getGlobalCache().clear();
      clazz.setValue("readCount", Primitive.ZERO_INTEGER);

      // This query fills the cache
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 1))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.ONE_INTEGER, clazz.getValue("readCount")); // verify that the read was executed
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("101", inst.getValue("x"));
      assertNull(inst.getValue("assoc"));
      assertEquals(Primitive.ONE_INTEGER, clazz.getValue("readCount")); // no lazy

      m_context.removeInstance(inst);
      inst = null;
      list = null;

      // This query should hit the cache
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 1))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.ONE_INTEGER, clazz.getValue("readCount")); // cache hit
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("101", inst.getValue("x"));
      assertNull(inst.getValue("assoc"));
      assertEquals(Primitive.ONE_INTEGER, clazz.getValue("readCount")); // no lazy

      m_context.removeInstance(inst);
      inst = null;
      list = null;

      // Clear caches
      m_context.getUnitOfWork().rollback();
      m_context.getGlobalCache().clear();

      // This query should miss the cache
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 1))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.createInteger(2), clazz.getValue("readCount")); // cache miss
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("101", inst.getValue("x"));
      assertNull(inst.getValue("assoc"));
      assertEquals(Primitive.createInteger(2), clazz.getValue("readCount")); // no lazy
   }

   /**
    * Tests reading an instance-cached class through an association.
    */
   public void testReadCachedAssoc()
   {
      Metaclass clazz = m_metadata.getMetaclass("CachedVirtual");
      InstanceList list;
      Instance inst;
      Instance assoc;

      m_context.getGlobalCache().clear();
      clazz.setValue("readCount", Primitive.ZERO_INTEGER);

      // This query fills the cache with key (CachedVirtual . OID:1:I1:3)
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 3))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.createInteger(1), clazz.getValue("readCount")); // verify that the read was executed
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("303", inst.getValue("x"));
      assertNull(inst.getValue("assoc"));
      assertEquals(Primitive.createInteger(1), clazz.getValue("readCount")); // no lazy

      m_context.removeInstance(inst);
      inst = null;
      list = null;

      // The association query does not hit the cache because key is (:class CachedVirtual "(= (@ <"<OID_TYPE>">) #<OID:1:I1:3>)")
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 2))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.createInteger(3), clazz.getValue("readCount")); // reads both again
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("202", inst.getValue("x"));
      assoc = (Instance)inst.getValue("assoc");
      assertEquals("303", assoc.getValue("x"));
      assertEquals(Primitive.createInteger(3), clazz.getValue("readCount")); // no lazy

      m_context.removeInstance(inst);
      m_context.removeInstance(assoc);
      assoc = inst = null;
      list = null;

      // The association query does hit the cache because the where clause cache key is now known (from the previous read)
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 4))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.createInteger(4), clazz.getValue("readCount")); // reads (oid 4), but (oid 3) is taken from cache
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("404", inst.getValue("x"));
      assoc = (Instance)inst.getValue("assoc");
      assertEquals("303", assoc.getValue("x"));
      assertEquals(Primitive.createInteger(4), clazz.getValue("readCount")); // no lazy

      m_context.removeInstance(inst);
      m_context.removeInstance(assoc);
      assoc = inst = null;
      list = null;

      // Clear caches
      m_context.getUnitOfWork().rollback();
      m_context.getGlobalCache().clear();

      // This query should miss the cache
      list = Query.createRead(clazz, parse("(x (assoc x))"), parseEval("`(= (@) ,(oid 2))"), null, -1, 0, false, Query.SEC_ALL, m_context).read();

      assertEquals(Primitive.createInteger(6), clazz.getValue("readCount")); // cache miss
      assertEquals(1, list.size());
      inst = list.getInstance(0);
      assertEquals("202", inst.getValue("x"));
      assoc = (Instance)inst.getValue("assoc");
      assertEquals("303", assoc.getValue("x"));
      assertEquals(Primitive.createInteger(6), clazz.getValue("readCount")); // no lazy
   }

   public void testGetUniqueKeys()
   {
      Metaclass animalClass = m_metadata.getMetaclass("Animal");
      Metaclass penClass = m_metadata.getMetaclass("AnimalPen");

      assertEquals("(" +
         "(pen scientificName limbCount commonName)" +
         ")", String.valueOf(animalClass.getUniqueKeys()));

      assertNull(penClass.getUniqueKeys());
   }

   protected Object parseEval(String sScript)
   {
      Pair code = parse(sScript);

      return m_context.getMachine().eval(code);
   }
}
