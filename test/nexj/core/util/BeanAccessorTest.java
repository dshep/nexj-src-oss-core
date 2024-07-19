// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Properties;

import junit.framework.TestCase;

public class BeanAccessorTest extends TestCase
{
   protected BeanAccessor m_accessor;
   protected TestBean m_bean;
   protected Properties m_properties;

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_accessor = new BeanAccessor();
      m_bean = new TestBean();
      m_properties = new Properties();
      m_properties.setProperty("int", "1");
      m_properties.setProperty("boolean", "true");
      m_properties.setProperty("string", "abc");
      m_properties.setProperty("properties", "a=1\nb=2");
   }

   public void testSetPropertyObject() throws Throwable
   {
      m_accessor.setProperty(m_bean, "int", new Integer(1));
      m_accessor.setProperty(m_bean, "boolean", Boolean.TRUE);

      assertEquals(1, m_bean.getInt());
      assertEquals(true, m_bean.getBoolean());
   }
   
   public void testSetProperty() throws Throwable
   {
      m_accessor.setProperty(m_bean, "int", "1");
      m_accessor.setProperty(m_bean, "boolean", "true");
      m_accessor.setProperty(m_bean, "string", "abc");
      m_accessor.setProperty(m_bean, "properties", "a=1\nb=2");

      assertEquals(1, m_bean.getInt());
      assertEquals(true, m_bean.getBoolean());
      assertEquals("abc", m_bean.getString());
      assertEquals(2, m_bean.getProperties().size());
      assertEquals("1", m_bean.getProperties().getProperty("a"));
      assertEquals("2", m_bean.getProperties().getProperty("b"));
   }

   public void testSetProperties() throws Throwable
   {
      m_accessor.setProperties(m_bean, m_properties);

      assertEquals(1, m_bean.getInt());
      assertEquals(true, m_bean.getBoolean());
      assertEquals("abc", m_bean.getString());
      assertEquals(2, m_bean.getProperties().size());
      assertEquals("1", m_bean.getProperties().getProperty("a"));
      assertEquals("2", m_bean.getProperties().getProperty("b"));
   }

   public void testGetProperty() throws Throwable
   {
      m_accessor.setProperties(m_bean, m_properties);

      assertEquals(new Integer(1), m_accessor.getProperty(m_bean, "int"));
      assertEquals(Boolean.TRUE, m_accessor.getProperty(m_bean, "boolean"));
      assertEquals(Boolean.TRUE, m_accessor.getProperty(m_bean, "bool"));
      assertEquals("abc", m_accessor.getProperty(m_bean, "string"));
      assertEquals(2, ((Properties)m_accessor.getProperty(m_bean, "properties")).size());
   }

   // inner classes

   public static class TestBean
   {
      // attributes

      /**
       * The string value.
       */
      protected String m_sString;

      /**
       * The int value.
       */
      protected int m_nInt;

      /**
       * The boolean value.
       */
      protected boolean m_bBoolean;

      /**
       * The properties value.
       */
      protected Properties m_properties;

      // operations

      /**
       * Sets the string value.
       * @param sString The string value to set.
       */
      public void setString(String sString)
      {
         m_sString = sString;
      }

      /**
       * @return The string value.
       */
      public String getString()
      {
         return m_sString;
      }

      /**
       * Sets the int value.
       * @param nInt The int value to set.
       */
      public void setInt(int nInt)
      {
         m_nInt = nInt;
      }

      /**
       * @return The int value.
       */
      public int getInt()
      {
         return m_nInt;
      }

      /**
       * Sets the boolean value.
       * @param bBoolean The boolean value to set.
       */
      public void setBoolean(boolean bBoolean)
      {
         m_bBoolean = bBoolean;
      }

      /**
       * @return The boolean value.
       */
      public boolean getBoolean()
      {
         return m_bBoolean;
      }

      /**
       * @return The boolean value.
       */
      public boolean isBool()
      {
         return m_bBoolean;
      }
      
      /**
       * Sets the properties value.
       * @param properties The properties value to set.
       */
      public void setProperties(Properties properties)
      {
         m_properties = properties;
      }

      /**
       * @return The properties value.
       */
      public Properties getProperties()
      {
         return m_properties;
      }
   }
}
