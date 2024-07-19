// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigDecimal;
import java.util.Date;

import junit.framework.TestCase;

import nexj.core.meta.TypeConversionException;
import nexj.core.util.Binary;


public class OIDTest extends TestCase
{
   private OID m_oid;
   private OID m_oid2;
   private OID m_oid3;
   private OID m_oid9;
   private OID m_oidBad;

   /**
    * Constructor for OIDTest.
    * @param name
    */
   public OIDTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_oid = new OID(new Object[]{"abc"});
      m_oid2 = new OID(new Object[]{"abc"});
      m_oid3 = new OID(new Object[]{"cde", new Integer(123), new Long(456)});
      m_oid9 = new OID(new Object[]{null, "a", new Integer(123), new Long(567),
         new Double(1.234), new BigDecimal("456.789"), new Date(1234567),
         Boolean.FALSE, new Binary(new byte[]{(byte)134, 45, (byte)173})});
      m_oidBad = new OID(new Object[]{"a", new Float(1), new Object()});
   }

   public void testHashCode()
   {
      assertTrue(m_oid.hashCode() != 0);
      assertTrue(m_oid.hashCode() == m_oid2.hashCode());
   }

   public void testGetValue()
   {
      assertEquals("cde", m_oid3.getValue(0));
      assertEquals(123, ((Integer)m_oid3.getValue(1)).intValue());
      assertEquals(456L, ((Long)m_oid3.getValue(2)).longValue());
   }

   public void testGetCount()
   {
      assertEquals(1, m_oid.getCount());
      assertEquals(3, m_oid3.getCount());
      assertEquals(9, m_oid9.getCount());
   }

   /*
    * Test for boolean equals(Object)
    */
   public void testEqualsObject()
   {
      assertTrue(m_oid.equals(m_oid));
      assertTrue(m_oid.equals(m_oid2));
      assertTrue(m_oid.hashCode() == m_oid2.hashCode());
      assertTrue(m_oid.equals(m_oid2));
      assertTrue(m_oid2.equals(m_oid));
      assertFalse(m_oid.equals(m_oid3));
      assertFalse(m_oid3.equals(m_oid));
      assertFalse(m_oid.equals(null));
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("OID:3:S3:cde:I3:123:L3:456", m_oid3.toString());
      assertEquals("OID:9::S1:a:I3:123:L3:567:D5:1.234:N7:456.789:T7:1234567:BF:V6:862DAD", m_oid9.toString());
      assertEquals("OID:3:S1:a:F3:1.0:?0:", m_oidBad.toString());
   }
   
   public void testToFromBinary()
   {
      assertEquals(m_oid3, OID.fromBinary(m_oid3.toBinary()));
      assertEquals(m_oid9, OID.fromBinary(m_oid9.toBinary()));
      
      try
      {
         m_oidBad.toBinary();
         fail("Expected TypeConversionException");
      }
      catch (TypeConversionException e)
      {
      }
   }

   public void testGetOID()
   {
      assertSame(m_oid, m_oid.getOID());
   }

   public void testSetOID()
   {
      try
      {
         m_oid.setOID(m_oid3);
         fail("Expected UnsupportedOperationException");
      }
      catch (UnsupportedOperationException e)
      {
      }
   }

   public void testExternalize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_oid9);
      
      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      
      OID oid = (OID)istream.readObject();

      assertEquals(m_oid9, oid);
   }
}
