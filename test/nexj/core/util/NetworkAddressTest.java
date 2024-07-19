// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Set;

import junit.framework.TestCase;

public class NetworkAddressTest extends TestCase
{
   protected InetAddress m_host1;
   protected InetAddress m_host2;
   protected InetAddress m_host3;

   protected NetworkAddress m_full;
   protected NetworkAddress m_a32;
   protected NetworkAddress m_a31;
   protected NetworkAddress m_a24;
   protected NetworkAddress m_a22;
   protected NetworkAddress m_a22v6;

   protected void setUp() throws Exception
   {
      super.setUp();

      m_host1 = Inet4Address.getByName("10.11.11.13");
      m_host2 = Inet4Address.getByName("10.11.8.1");
      m_host3 = Inet4Address.getByName("10.11.1.1");

      m_full = NetworkAddress.parse("10.11.11.13");
      m_a32 = NetworkAddress.parse("10.11.11.13/32");
      m_a31 = NetworkAddress.parse("10.11.11.12/31");
      m_a24 = NetworkAddress.parse("10.11.11.0/24");
      m_a22 = NetworkAddress.parse("10.11.8.0/22");
      m_a22v6 = NetworkAddress.parse("::ffff:10.11.8.0/22");
   }

   public void testContains()
   {
      assertTrue(m_full.contains(m_host1));
      assertTrue(m_a32.contains(m_host1));
      assertTrue(m_a31.contains(m_host1));
      assertTrue(m_a24.contains(m_host1));
      assertTrue(m_a22.contains(m_host1));

      assertFalse(m_full.contains(m_host2));
      assertFalse(m_a32.contains(m_host2));
      assertFalse(m_a31.contains(m_host2));
      assertFalse(m_a24.contains(m_host2));
      assertTrue(m_a22.contains(m_host2));
      
      assertFalse(m_full.contains(m_host3));
      assertFalse(m_a32.contains(m_host3));
      assertFalse(m_a31.contains(m_host3));
      assertFalse(m_a24.contains(m_host3));
      assertFalse(m_a22.contains(m_host3));
   }

   public void testParse() throws Exception
   {
      try
      {
         NetworkAddress.parse("10.10.10.10/33");
         fail("Expected UnknownHostException");
      }
      catch (UnknownHostException e)
      {
      }
   }

   public void testEquals()
   {
      assertTrue(m_full.equals(m_a32));
      assertFalse(m_a22.equals(m_a32));
      assertTrue(m_a22.equals(m_a22v6));
   }

   public void testHashCode()
   {
      Set set = new HashHolder();

      set.add(m_full);

      assertTrue(set.contains(m_full));
      assertTrue(set.contains(m_a32));
      assertFalse(set.contains(m_a31));
   }

   public void testToString()
   {
      assertEquals("10.11.11.13/32", m_a32.toString());
      assertEquals("10.11.11.12/31", m_a31.toString());
      assertEquals("10.11.11.0/24", m_a24.toString());
      assertEquals("10.11.8.0/22", m_a22.toString());
      assertEquals("10.11.8.0/22", m_a22v6.toString());
   }
}
