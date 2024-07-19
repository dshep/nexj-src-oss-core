// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Locale;

import junit.framework.TestCase;

import nexj.core.scripting.Pair;
import nexj.core.util.IOUtil;
import nexj.core.util.TZ;
import nexj.core.util.URLUtil;
import nexj.test.util.AssertUtil;

public class RequestTest extends TestCase
{
   protected Request m_request;

   protected void setUp() throws Exception
   {
      super.setUp();

      m_request = new Request();
      m_request.setNamespace("namespace");
      m_request.setVersion("version");
      m_request.setLocale(Locale.CANADA);
      m_request.setTimeZone(TZ.UTC);
      m_request.setStealth(true);

      TransferObject tobj = new TransferObject(1);

      tobj.setClassName("C");
      tobj.setEventName("e");
      tobj.setValue("k", "v");

      m_request.addInvocation(tobj, new Object[]{new Integer(1), "p"}, Pair.attributeList("a b c", null));

      tobj = new TransferObject(0);

      tobj.setClassName("F");
      tobj.setEventName("f");

      m_request.addFilter(tobj);

      tobj = new TransferObject(0);

      tobj.setClassName("R");
      tobj.setEventName("r");

      m_request.setCorrelator(tobj);
   }

   protected void tearDown() throws Exception
   {
      m_request = null;
      super.tearDown();
   }

   public void testCompatibility() throws Exception
   {
      InputStream istream = URLUtil.openResource(getClass(), "request1.bin");

      try
      {
         Request req = (Request)new ObjectInputStream(istream).readObject();

         assertEquals(m_request.getNamespace(), req.getNamespace());
         assertEquals(m_request.getVersion(), req.getVersion());
         assertEquals(m_request.getLocale(), req.getLocale());
         assertEquals(m_request.getTimeZone(), req.getTimeZone());
         assertEquals(m_request.isCommit(), req.isCommit());
         assertEquals(m_request.isStealth(), req.isStealth());
         assertEquals(m_request.getInvocationCount(), req.getInvocationCount());

         for (int i = 0; i < m_request.getInvocationCount(); ++i)
         {
            Request.Invocation left = m_request.getInvocation(i);
            Request.Invocation right = req.getInvocation(i);

            AssertUtil.assertEquals(left.getObject(), right.getObject());
            assertEquals(left.getEventName(), right.getEventName());
            assertTrue(Arrays.deepEquals(left.getArguments(), right.getArguments()));
            assertEquals(left.getAttributes(), right.getAttributes());
         }

         assertEquals(m_request.getFilterCount(), req.getFilterCount());

         for (int i = 0; i < m_request.getFilterCount(); ++i)
         {
            AssertUtil.assertEquals(m_request.getFilter(i), req.getFilter(i));
         }

         AssertUtil.assertEquals(m_request.getCorrelator(), req.getCorrelator());
      }
      finally
      {
         IOUtil.close(istream);
      }
   }

   /*
    * Writes out a serialized version of the request.
    */
   public static void main(String[] args) throws Exception
   {
      OutputStream ostream = new FileOutputStream(args[0]);

      try
      {
         ostream = new ObjectOutputStream(ostream);

         RequestTest test = new RequestTest();
   
         test.setUp();
         ((ObjectOutputStream)ostream).writeObject(test.m_request);
      }
      finally
      {
         ostream.close();
      }
   }
}
