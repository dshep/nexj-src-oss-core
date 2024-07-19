// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.runtime.InvocationContext;

public class ComponentTest extends TestCase
{
   private Component m_comp = null;

   /**
    * Constructor for ComponentTest.
    * @param name
    */
   public ComponentTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_comp = Repository.getMetadata().getComponent("TestComponent");
   }

   /*
    * Test for Object getInstance(InvocationContext)
    */
   public void testGetInstanceInvocationContext()
   {
      InvocationContext context = new InvocationContext(Repository.getMetadata());
      TestComponent inst = (TestComponent)m_comp.getInstance(context);
      
      assertNotNull(inst);
      assertSame(context, inst.getInvocationContext());
      assertEquals(1, inst.getInitializationCount());
      assertEquals(10, inst.getCookie());
      assertEquals(3, inst.getIntegerCount());
      assertEquals(1, inst.getInteger(0).intValue());
      assertEquals(2, inst.getInteger(1).intValue());
      assertEquals(3, inst.getInteger(2).intValue());
      assertEquals(2, inst.getTestComponentCount());
      assertSame(inst, inst.getTestComponent(0));
      assertEquals(1, inst.getTestComponent(1).getCookie());
      assertEquals(7, inst.getTestComponent(1).getInteger());
      assertEquals(new BigDecimal("123.456001"), inst.getBigDecimal());
      assertEquals(new BigInteger("123456789"), inst.getBigInteger());
      assertEquals(true, inst.getBoolean());
      assertEquals(Boolean.TRUE, inst.getBooleanObj());
      assertEquals(123, inst.getByte());
      assertEquals(new Byte((byte)-123), inst.getByteObj());
      assertEquals('c', inst.getCharacter());
      assertEquals(new Character('C'), inst.getCharacterObj());
      assertEquals(Component.SINGLETON, inst.getComponent().getActivation());
      assertEquals(1, inst.getComponent().getPropertyInitializerCount());
      assertEquals(0, inst.getDate().getTime());
      assertEquals(123.625, inst.getDouble(), 0);
      assertEquals(new Double(-123.625), inst.getDoubleObj());
      assertEquals(456.625f, inst.getFloat(), 0);
      assertEquals(new Float(-456.625f), inst.getFloatObj());
      assertEquals(3, inst.getInstance().getCookie());
      assertEquals(123456, inst.getInteger());
      assertEquals(new Integer(-123456), inst.getIntegerObj());
      assertEquals(1234567890123456L, inst.getLong());
      assertEquals(new Long(-1234567890123456L), inst.getLongObj());
      assertEquals("Contact", inst.getMetaclass().getName());
      assertEquals("integer", inst.getPrimitive().getName());
      assertEquals(2, inst.getProperties().size());
      assertEquals("b+c", inst.getProperties().getProperty("a"));
      assertEquals("2+2", inst.getProperties().getProperty("5"));
      assertEquals(12345, inst.getShort());
      assertEquals(new Short((short)-12345), inst.getShortObj());
      assertEquals("abcdef", inst.getString());
      assertEquals(0, inst.getTime().getTime());
      assertEquals(0, inst.getTimestamp().getTime());
      assertEquals(0, inst.getTimestamp().getNanos());
      assertEquals("Address", inst.getType().getName());
      assertEquals(0, inst.getUtilDate().getTime());
   }

   public void testGetType()
   {
      assertEquals(TestComponent.class, m_comp.getType());
   }

   public void testGetActivation()
   {
      assertEquals(Component.NEW, m_comp.getActivation());
   }

   public void testGetFactoryMethod()
   {
      assertEquals("createTestComponent", m_comp.getFactoryMethod().getName());
   }

   public void testGetFactory()
   {
      assertNotNull(m_comp.getFactory());
   }

   public void testGetPropertyInitializer()
   {
      assertEquals("PrimitiveCollectionPropertyInitializer integer", m_comp.getPropertyInitializer(0).toString());
      assertEquals("PrimitivePropertyInitializer bigDecimal", m_comp.getPropertyInitializer(2).toString());
   }

   public void testGetPropertyInitializerCount()
   {
      assertEquals(31, m_comp.getPropertyInitializerCount());
   }

   public void testGetPropertyInitializerIterator()
   {
      Iterator itr = m_comp.getPropertyInitializerIterator();

      for (int i = 0; i < m_comp.getPropertyInitializerCount(); ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetMetadata()
   {
      assertEquals(Repository.getMetadata(), m_comp.getMetadata());
   }

   public void testGetName()
   {
      assertEquals("TestComponent", m_comp.getName());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("Component TestComponent", m_comp.toString());
   }
}
