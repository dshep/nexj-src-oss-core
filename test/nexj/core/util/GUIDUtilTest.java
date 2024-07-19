// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import junit.framework.TestCase;

public class GUIDUtilTest extends TestCase
{
   protected byte[] m_namespace;

   protected void setUp() throws Exception
   {
      super.setUp();
      m_namespace = Binary.parse("6ba7b8109dad11d180b400c04fd430c8").getData();  
   }

   public void testGenerateGUIDByteArrayByteArray() throws Exception
   {
      assertEquals("2D17BCED88703AA381A1BA6F5B522B6D", GUIDUtil.generateGUID(m_namespace, "www.nexj.com".getBytes("UTF-8")).toString());
   }
}
