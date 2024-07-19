// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.sql.Timestamp;
import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.Repository;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;

public class GenericSerializablePropertyMapTest extends TestCase
{
   protected GenericSerializablePropertyMap m_map;
   protected InvocationContext m_context;

   protected void setUp() throws Exception
   {
      super.setUp();
      m_map = new GenericSerializablePropertyMap(GenericSerializablePropertyMap.SKIP);
      m_context = new InvocationContext(Repository.getMetadata());
      m_context.setSecure(false);

      Instance instance = new Instance(m_context.getMetadata().getMetaclass("SysObjectLog"), m_context);

      instance.setNew();
      instance.setValue("event", "a");
      ((PropertyMap)instance.getValue("values")).setValue("skip", new GenericSerializablePropertyMap());
      instance.setValue("createdOn", new Timestamp(1000000000));

      m_map.setValue("log", instance);
      m_map.setValue("name", "q");
   }

   protected void tearDown() throws Exception
   {
      super.tearDown();
      m_map = null;

      if (m_context != null)
      {
         m_context.complete(false);
         ThreadContextHolder.setContext(null);
         m_context = null;
      }
   }

   public void testSerializeValues()
   {
      assertEquals("4v2TUU1I0U3Slog10T12SSysObjectLogU1I0U13SvaluesCaption7sskip={}16SserializedValuesU9ScreatedOn10D100000000013SobjectCaptions3SoidU4SuserU5SclassU8SargCountU5Sevent1sa6SobjectU4Sname1sq", m_map.serializeValues(m_context));
   }

   public void testDeserializeValues()
   {
      GenericSerializablePropertyMap map = new GenericSerializablePropertyMap();

      map.deserializeValues(m_map.serializeValues(m_context), m_context);
      assertEquals(2, map.getValueCount());
      assertEquals("a", ((Instance)map.getValue("log")).getValue("event"));
      assertEquals("q", map.getValue("name"));
   }

   public void testGetClassName()
   {
      assertEquals("var", m_map.getClassName());
   }

   public void testGetValueString()
   {
      assertEquals("q", m_map.getValue("name"));
      assertNotNull(m_map.getValue("log"));
      assertNull(m_map.getValue("a"));
   }

   public void testSetValueStringObject()
   {
      m_map.setValue("b", new Integer(1));
      assertEquals(3, m_map.getValueCount());
      m_map.setValue("name", "z");
      assertEquals(3, m_map.getValueCount());
      assertEquals("z", m_map.getValue("name"));
      assertEquals(new Integer(1), m_map.getValue("b"));
   }

   public void testFindValueString()
   {
      assertEquals("q", m_map.findValue("name"));
      assertNotNull(m_map.findValue("log"));
      assertNull(m_map.findValue("a"));
   }

   public void testFindValueStringObject()
   {
      assertEquals("q", m_map.findValue("name", "s"));
      assertNotNull(m_map.findValue("log", null));
      assertEquals("z", m_map.findValue("name1", "z"));
   }

   public void testRemoveValue()
   {
      assertEquals("q", m_map.removeValue("name"));
      assertNull(m_map.removeValue("q"));
      assertEquals(1, m_map.getValueCount());
   }

   public void testHasValue()
   {
      assertTrue(m_map.hasValue("name"));
      assertFalse(m_map.hasValue("q"));
   }

   public void testGetValueCount()
   {
      assertEquals(2, m_map.getValueCount());
   }

   public void testGetIterator()
   {
      PropertyIterator itr = m_map.getIterator();

      itr.next();
      assertEquals("name", itr.next());
      assertEquals("name", itr.getName());
      assertEquals("q", itr.getValue());
      assertFalse(itr.hasNext());
   }

   public void testIterator()
   {
      Iterator itr = m_map.iterator();

      itr.next();
      assertEquals("name", itr.next());
      assertFalse(itr.hasNext());
   }

   public void testToString()
   {
      assertEquals("{log=Instance<SysObjectLog, null, NEW>(event=\"a\", createdOn=1970-01-12 13:46:40.000000000, serializedValues=(), values={skip={}}), name=\"q\"}",
         m_map.toString());
   }

   public void testGetValueSymbol()
   {
      assertEquals("q", m_map.getValue(Symbol.define("name")));
      assertNotNull(m_map.getValue(Symbol.define("log")));
      assertNull(m_map.getValue(Symbol.define("a")));
   }

   public void testSetValueSymbolObject()
   {
      m_map.setValue(Symbol.define("b"), new Integer(1));
      assertEquals(3, m_map.getValueCount());
      m_map.setValue(Symbol.define("name"), "z");
      assertEquals(3, m_map.getValueCount());
      assertEquals("z", m_map.getValue(Symbol.define("name")));
      assertEquals(new Integer(1), m_map.getValue(Symbol.define("b")));
   }

   public void testInvokeSymbolIntMachine()
   {
      m_context.getMachine().push(Symbol.define("name"));

      try
      {
         m_map.invoke(Symbol.define("name"), 1, m_context.getMachine());
         fail("Expected ScriptingException");
      }
      catch (ScriptingException e)
      {
      }
   }

   public void testInvokeIntMachine()
   {
      m_context.getMachine().push(Symbol.define("name"));
      m_map.invoke(1, m_context.getMachine());
      assertEquals("q", m_context.getMachine().pop());
      m_context.getMachine().push(Symbol.define("name"));
      m_context.getMachine().push("z");
      m_map.invoke(2, m_context.getMachine());
      assertEquals("z", m_context.getMachine().pop());
      assertEquals(2, m_map.getValueCount());
      assertEquals("z", m_map.getValue("name"));
   }
}
