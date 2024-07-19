// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import nexj.core.meta.Metaclass;
import nexj.core.persistence.Query;
import nexj.core.persistence.sql.ReadCountHook;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Logger;

/**
 * Tests heterogeneous joins between the virtual adapter and the relational adapter.
 */
public class VirtualAdapterSQLDataTest extends SQLDataTest
{
   protected final static Logger s_logger = Logger.getLogger(VirtualAdapterSQLDataTest.class);

   public VirtualAdapterSQLDataTest(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#setUp()
    */
   public void setUp() throws Exception
   {
      super.setUp();

      m_context.getMachine().eval(
         Pair.list(Symbol.LOAD, VirtualAdapterTest.class.getResource("datastore.scm").toString())
      );
   }

   public void testReadAssocRelationalFKToVirtualNone() throws Exception
   {
      Metaclass relationalClass = getMetadata().getMetaclass("PetOwner");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(relationalClass,
         parse("(firstName lastName (relFKToVirtNone commonName))"), null,
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Joe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relFKToVirtNone");
      assertEquals("Beaver", inst2.getValue("commonName"));
      assertEquals("Castor canadensis", inst2.getValue("scientificName"));

      inst = list.getInstance(1);
      assertEquals("Zoe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relFKToVirtNone");
      assertEquals("Spider", inst2.getValue("commonName"));
   }

   public void testLazyRelationalJoiningVirtual() throws Exception
   {
      Metaclass relationalClass = getMetadata().getMetaclass("PetOwner");
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list;
      Instance inst;
      int nStartCallCount;

      nStartCallCount = ((Integer)virtualClass.getValue("objectKeyReadCount")).intValue();
      list = (InstanceArrayList)Query.createRead(relationalClass,
         null, null, parse("(((@) . #t))"), -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.getCount());
      assertEquals(nStartCallCount, ((Integer)virtualClass.getValue("objectKeyReadCount")).intValue());
      inst = list.getInstance(0);
      assertEquals(nStartCallCount, ((Integer)virtualClass.getValue("objectKeyReadCount")).intValue());
      s_logger.info("***** Start lazy load *****");
      assertEquals("Joe", inst.getValue("firstName"));
      s_logger.info("***** End lazy load *****");

      // Ensure no joins on lazy load:
      assertEquals(nStartCallCount, ((Integer)virtualClass.getValue("objectKeyReadCount")).intValue());
   }

   public void testLazyVirtualJoiningRelational() throws Exception
   {
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      ReadCountHook readCountHook = (ReadCountHook)((SQLAdapter)getMetadata().getDataSource("DefaultRelationalDatabase").getComponent().getInstance(m_context)).getSQLHook();

      assertNotNull("JUnit configuration error: default.config not up-to-date", readCountHook);

      InstanceArrayList list;
      Instance inst;
      int nStartReadCount;

      nStartReadCount = readCountHook.getReadCount();
      list = (InstanceArrayList)Query.createRead(virtualClass,
         null,
         null,
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.getCount());
      assertEquals(nStartReadCount, readCountHook.getReadCount());
      inst = list.getInstance(0);
      assertEquals(nStartReadCount, readCountHook.getReadCount());
      assertEquals("Dog", inst.getValue("commonName"));
      assertEquals(nStartReadCount, readCountHook.getReadCount());
   }

   public void testReadAssocRelationalFKToVirtualAttr() throws Exception
   {
      Metaclass relationalClass = getMetadata().getMetaclass("PetOwner");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(relationalClass,
         parse("(firstName lastName (relFKToVirtAttr commonName))"),
         null, parse("(((@) . #t))"), -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Joe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relFKToVirtAttr");
      assertEquals("Spider", inst2.getValue("commonName"));

      inst = list.getInstance(1);
      assertEquals("Zoe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relFKToVirtAttr");
      assertEquals("Cat", inst2.getValue("commonName"));
      assertEquals("Felis catus", inst2.getValue("scientificName"));

      // Read the reverse attribute
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtAttrFromRelFK firstName))"),
         null,
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assertNull(inst.getValue("virtAttrFromRelFK"));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtAttrFromRelFK");
      assertEquals("Zoe", inst2.getValue("firstName"));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtAttrFromRelFK");
      assertEquals("Joe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));
   }

   public void testReadAssocRelationalFKToVirtualCollection() throws Exception
   {
      Metaclass relationalClass = getMetadata().getMetaclass("PetOwner");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(relationalClass,
         parse("(firstName (relFKToVirtColl scientificName))"),
         parse("(= (@ lastName) \"Test\")"),
         parse("((firstName . #f))"),
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Zoe", inst.getValue("firstName"));
      assertNull(inst.getValue("relFKToVirtColl"));
      inst = list.getInstance(1);
      assertEquals("Joe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relFKToVirtColl");
      assertEquals("Felis catus", inst2.getValue("scientificName"));
      assertEquals("Cat", inst2.getValue("commonName"));


      // Read the reverse (the virtual collection)
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list2;

      list = (InstanceArrayList)virtualClass.invoke("read", new Object[]{
         parse("(commonName (virtCollFromRelFK firstName))"), null, null, null, null, null
      });

      assertEquals(4, list.size());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assertEquals(0, ((InstanceArrayList)inst.getValue("virtCollFromRelFK")).size());

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      list2 = (InstanceArrayList)inst.getValue("virtCollFromRelFK");
      assertEquals(1, list2.size());
      inst2 = list2.getInstance(0);
      assertEquals("Joe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));
   }

   public void testReadAssocVirtualFKToRelationalNone() throws Exception
   {
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtFKToRelNone firstName lastName))"),
         null,
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelNone");
      assertEquals("Zoe", inst2.getValue("firstName"));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelNone");
      assertEquals("Joe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));
      
      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      assertNull(inst.getValue("virtFKToRelNone"));
   }

   public void testReadAssocVirtualFKToRelationalAttr() throws Exception
   {
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtFKToRelAttr firstName))"),
         null,
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.getCount());

      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      assertNull(inst.getValue("virtFKToRelAttr"));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelAttr");
      assertEquals("Joe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));

      inst = list.getInstance(3);
      assertEquals("Beaver", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelAttr");
      assertEquals("Zoe", inst2.getValue("firstName"));


      // Read the reverse attribute
      Metaclass relationalClass = getMetadata().getMetaclass("PetOwner");

      list = (InstanceArrayList)Query.createRead(relationalClass,
         parse("(firstName (relAttrFromVirtFK commonName))"), null,
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Joe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relAttrFromVirtFK");
      assertEquals("Spider", inst2.getValue("commonName"));

      inst = list.getInstance(1);
      assertEquals("Zoe", inst.getValue("firstName"));
      inst2 = (Instance)inst.getValue("relAttrFromVirtFK");
      assertEquals("Beaver", inst2.getValue("commonName"));
      assertEquals("Castor canadensis", inst2.getValue("scientificName"));
   }

   public void testReadAssocVirtualFKToRelationalCollection() throws Exception
   {
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtFKToRelColl firstName lastName))"),
         null,
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelColl");
      assertEquals("Zoe", inst2.getValue("firstName"));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelColl");
      assertEquals("Zoe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      assertNull(inst.getValue("virtFKToRelColl"));


      // Read the reverse (the relational collection)
      Metaclass relationalClass = getMetadata().getMetaclass("PetOwner");
      InstanceArrayList list2;

      list = (InstanceArrayList)Query.createRead(relationalClass,
         parse("(firstName (relCollFromVirtFK commonName))"),
         parse("(= (@ lastName) \"Test\")"),
         parse("((firstName . #t))"),
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.size());
      inst = list.getInstance(0);
      assertEquals("Joe", inst.getValue("firstName"));
      assertEquals(0, ((InstanceArrayList)inst.getValue("relCollFromVirtFK")).size());

      inst = list.getInstance(1);
      assertEquals("Zoe", inst.getValue("firstName"));
      list2 = (InstanceArrayList)inst.getValue("relCollFromVirtFK");
      assertEquals(2, list2.size());
      inst2 = list2.getInstance(0);
      assertEquals("Dog", inst2.getValue("commonName"));
      inst2 = list2.getInstance(1);
      assertEquals("Cat", inst2.getValue("commonName"));
      assertEquals("Felis catus", inst2.getValue("scientificName"));
   }

   public void testReadAssocVirtualAttrToRelationalFKNoAttr() throws Exception
   {
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtAttrToRelFKNoAttr firstName lastName))"),
         null,
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(4, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtAttrToRelFKNoAttr");
      assertEquals("Zoe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      assertNull(inst.getValue("virtAttrToRelFKNoAttr"));

      inst = list.getInstance(2);
      assertEquals("Spider", inst.getValue("commonName"));
      assertNull(inst.getValue("virtAttrToRelFKNoAttr"));
   }

   /**
    * Tests that an attribute in a where clause does not get expanded into OID components.
    */
   public void testReadAssocVirtualFKToRelationalCollectionWhereClause() throws Exception
   {
      Metaclass virtualClass = getMetadata().getMetaclass("Animal");
      InstanceArrayList list;
      Instance inst, inst2;

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtFKToRelColl firstName))"),
         parseEval("`(= (@ virtFKToRelColl) ,(oid #z00000000000000000000000000008802 \"main\"))"),
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertEquals(2, list.getCount());
      inst = list.getInstance(0);
      assertEquals("Dog", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelColl");
      assertEquals("Zoe", inst2.getValue("firstName"));

      inst = list.getInstance(1);
      assertEquals("Cat", inst.getValue("commonName"));
      inst2 = (Instance)inst.getValue("virtFKToRelColl");
      assertEquals("Zoe", inst2.getValue("firstName"));
      assertEquals("Test", inst2.getValue("lastName"));

      list = (InstanceArrayList)Query.createRead(virtualClass,
         parse("(commonName (virtFKToRelColl firstName))"),
         parseEval("`(= (@ virtFKToRelColl) ,(oid #z00000000000000000000000000008801))"),
         null,
         -1, 0, false, Query.SEC_NODE, m_context).read();

      assertTrue(list.isEmpty());
   }

   public void testReadAssocMultipartVirtualFKToRelationalPK()
   {
      Metaclass testClass = getMetadata().getMetaclass("TestVirtualClass");
      InstanceList list = Query.createRead(testClass,
         parse("(f1)"), parse("(= (@ f) 1)"), null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();

      assertEquals("Cottage", ((Instance)list.getInstance(0).getValue("f1")).getValue("caption"));
   }

   /**
    * Tests that a relational instance with an FK to a virtual instance gets the correct FK value
    * when the virtual instance's OID is set by the create mapping.
    */
   public void testCreateDepRelationalOnVirtual()
   {
      Metaclass relationalClass = getMetadata().getMetaclass("NoKGenRelational");
      Metaclass virtualClass = getMetadata().getMetaclass("NoKGenVirtual");
      Instance relInst, virtInst;
      InstanceList list;

      virtInst = (Instance)virtualClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "The virtual")
      });
      relInst = (Instance)relationalClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "The relational"),
         new Pair(Symbol.define("virtual"), virtInst)
      });

      m_context.getUnitOfWork().commit();
      m_context.removeInstance(relInst);
      m_context.removeInstance(virtInst);

      // Verify that the relational instance has an FK to the virtual instance
      list = Query.createRead(relationalClass,
         parse("(name (virtual name))"), null, null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();

      assertEquals(1, list.size());
      relInst = list.getInstance(0);
      assertEquals("The relational", relInst.getValue("name"));
      virtInst = (Instance)relInst.getValue("virtual");
      assertNotNull(virtInst);
      assertEquals("The virtual", virtInst.getValue("name"));
   }

   /**
    * Tests that a virtual instance with an FK to a relational instance gets the correct FK value
    * when the relational instance's OID is set by the "insert" work item. (i.e. identity/auto-increment
    * column)
    */
   public void testCreateDepVirtualOnRelational()
   {
      Metaclass relationalClass = getMetadata().getMetaclass("NoKGenRelational");
      Metaclass virtualClass = getMetadata().getMetaclass("NoKGenVirtual");
      Instance relInst, virtInst;
      InstanceList list;

      relInst = (Instance)relationalClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "The relational")
      });

      virtInst = (Instance)virtualClass.invoke("new", new Object[]
      {
         new Pair(Symbol.define("name"), "The virtual"),
         new Pair(Symbol.define("relational"), relInst)
      });

      m_context.getUnitOfWork().commit();
      m_context.removeInstance(relInst);
      m_context.removeInstance(virtInst);

      // Verify that the virtual instance has an FK to the relational instance
      list = Query.createRead(virtualClass,
         parse("(name (relational name))"), null, null, -1, 0, false, Query.SEC_ALL, m_context
      ).read();

      assertEquals(1, list.size());
      virtInst = list.getInstance(0);
      assertEquals("The virtual", virtInst.getValue("name"));
      relInst = (Instance)virtInst.getValue("relational");
      assertNotNull(relInst);
      assertEquals("The relational", relInst.getValue("name"));
   }

   protected Object parseEval(String sScript)
   {
      Pair code = parse(sScript);

      return m_context.getMachine().eval(code);
   }
}
