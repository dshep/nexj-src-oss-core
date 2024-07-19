// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import junit.framework.TestCase;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.Repository;
import nexj.core.persistence.OID;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.GenericException;

/**
 * Base class for generic marshaller/unmarshaller tests.
 */
public abstract class CharacterStreamMarshallerTest extends TestCase
{
   protected Request m_request;
   protected Response m_response;
   protected Exception m_exception;
   protected CharacterStreamMarshaller m_marshaller;
   protected CharacterStreamUnmarshaller m_unmarshaller;
   protected Writer m_writer;

   protected CharacterStreamMarshallerTest(String sName)
   {
      super(sName);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_request = new Request();

      TransferObject contact = new TransferObject();

      contact.setOID(new OID(new Object[]{"123"}));
      contact.setClassName("Contact");
      contact.setEventName("update");
      contact.setVersion((short)12345);
      contact.setValue("firstName", "Java");
      contact.setValue("lastName", "Kava");
      contact.setValue("null", null);
      contact.setValue("self", contact);
      contact.setValue("integer", Primitive.ONE_INTEGER);
      contact.setValue("long", Primitive.createLong(2));
      contact.setValue("float", Primitive.createFloat(0.625f));
      contact.setValue("double", Primitive.createDouble(1.625));
      contact.setValue("decimal", new BigDecimal("1.2345"));
      contact.setValue("timestamp", new Timestamp(12345));
      contact.setValue("boolean", Boolean.TRUE);
      contact.setValue("binary", new Binary(new byte[]{1, 2, 3, 4, 5}));
      contact.setValue("binary2", new Binary(new byte[]{1, 2, 3, 4, 5, 6, 7}));
      contact.setValue("binary3", new Binary(new byte[]{1, 2}));
      contact.setValue("symbol", Symbol.define("sym"));
      contact.setValue("pair", new Pair("A", new Pair("B")));
      contact.setValue("cvector", new char[]{'a', 'b', 'c'});
      contact.setValue("bvector", new byte[]{0, (byte)0xAB, 0x12});
      contact.setValue("svector", new String[]{"a", "b", "c"});
      contact.setValue("vector", new Object[]{"a", "b", "c"});
      contact.setValue("function", new PCodeFunction(new char[]{0, 1, 2}, new Object[]{"abc"}));
      contact.setValue("macro", new PCodeMacro(new char[]{3, 4, 5}, new Object[]{"cde"}, null));

      PrivilegeSet privilegeSet = Repository.getMetadata().createPrivilegeSet();

      privilegeSet.add(Repository.getMetadata().getPrimitivePrivilege("updateContact"));
      privilegeSet.add(Repository.getMetadata().getPrimitivePrivilege("WorkflowManage"));
      contact.setValue("privilegeSet", privilegeSet);

      List addressList = new ArrayList();

      TransferObject address = new TransferObject();
      address.setOID(new OID(new Object[]{"456"}));
      address.setClassName("Address");
      address.setEventName("update");
      address.setValue("country", "Canada");
      address.setValue("self", address);
      address.setValue("contact", contact);
      address.setValue("symbol", Symbol.define("sym"));
      addressList.add(address);

      address = new TransferObject();
      address.setOID(new OID(new Object[]{"789"}));
      address.setClassName("Address");
      address.setEventName("new");
      address.setValue("country", "USA");
      address.setValue("contact", contact);
      addressList.add(address);

      contact.setValue("addresses", addressList);

      m_request.setNamespace("http://www.nexjsystems.com/ns/test");
      m_request.setVersion("10");
      m_request.setAsync(true);
      m_request.setCommit(true);
      m_request.setCorrelator(address);
      m_request.setLocale(new Locale("bg", "BG", "SF"));
      m_request.setTimeZone(TimeZone.getTimeZone("AST"));
      m_request.addInvocation(contact);
      m_request.addInvocation(contact, "e", new Object[]{"p1", Primitive.ZERO_DOUBLE}, Pair.list("a", "b"));
      m_request.addFilter(contact);

      m_response = new Response();
      m_response.addResult(contact);
      m_response.addResult(address);

      List eventList = new ArrayList();

      eventList.add(address);
      eventList.add(address);

      m_response.addEvent(eventList);

      ValidationException e = new ValidationException("err.validation.requiredAttributes", new Object[]{"Contact"});

      e.addException(new MetadataValidationException("err.meta.x", new Object[]{"a", "b"}));
      e.addException(new QueryTimeoutException());
      e.setClassName("Contact");
      e.setOIDHolder(contact);
      e.setOrdinal(1);
      e.addException("firstName", new ValidationException("err.validation.requiredAttribute", new Object[]{"firstName", "Contact"}));

      m_exception = e;
      m_writer = new StringWriter();
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      ThreadContextHolder.setContext(null);

      m_request = null;
      m_response = null;
      m_exception = null;
      m_marshaller = null;
      m_unmarshaller = null;
      m_writer = null;
   }

   public void testSerializeRequest() throws IOException
   {
      m_marshaller.serialize(m_request, m_writer);
      StringReader reader = new StringReader(m_writer.toString());
      Request req = (Request)m_unmarshaller.deserialize(reader);

      assertEquals("http://www.nexjsystems.com/ns/test", req.getNamespace());
      assertEquals("10", req.getVersion());
      assertEquals(true, req.isAsync());
      assertEquals(true, req.isCommit());
      assertEquals("bg_BG_SF", req.getLocale().toString());
      assertEquals("AST", req.getTimeZone().getID());
      assertEquals(2, req.getInvocationCount());
      assertEquals(1, req.getFilterCount());

      TransferObject contact = req.getObject(0);

      assertSame(contact, req.getFilter(0));
      assertSame(contact, req.getObject(1));

      assertNull(req.getInvocation(0).getEventName());
      assertEquals("e", req.getInvocation(1).getEventName());

      assertNull(req.getInvocation(0).getArguments());

      Object[] params = req.getInvocation(1).getArguments();

      assertNotNull(params);
      assertEquals(2, params.length);
      assertEquals("p1", params[0]);
      assertEquals(Primitive.ZERO_DOUBLE, params[1]);

      assertNull(req.getInvocation(0).getAttributes());

      Pair pair = req.getInvocation(1).getAttributes();

      assertEquals("a", pair.getHead());
      assertEquals("b", pair.getNext().getHead());
      assertNull(pair.getNext().getNext());

      OID oid = contact.getOID();

      assertEquals(1, oid.getCount());
      assertEquals("123", oid.getValue(0));

      assertEquals("Contact", contact.getClassName());
      assertEquals("update", contact.getEventName());
      assertEquals(12345, contact.getVersion());
      assertEquals(24, contact.getValueCount());
      assertEquals("Java", contact.getValue("firstName"));
      assertEquals("Kava", contact.getValue("lastName"));
      assertNull(contact.getValue("null"));
      assertSame(contact, contact.getValue("self"));
      checkEquals(Primitive.ONE_INTEGER, (Number)contact.getValue("integer"));
      checkEquals(Primitive.createLong(2), (Number)contact.getValue("long"));
      checkEquals(Primitive.createFloat(0.625f), (Number)contact.getValue("float"));
      checkEquals(Primitive.createDouble(1.625), (Number)contact.getValue("double"));
      checkEquals(new BigDecimal("1.2345"), (Number)contact.getValue("decimal"));
      assertEquals(new Timestamp(12345), contact.getValue("timestamp"));
      assertEquals(Boolean.TRUE, contact.getValue("boolean"));
      assertEquals(new Binary(new byte[]{1, 2, 3, 4, 5}), contact.getValue("binary"));
      assertEquals(new Binary(new byte[]{1, 2, 3, 4, 5, 6, 7}), contact.getValue("binary2"));
      assertEquals(new Binary(new byte[]{1, 2}), contact.getValue("binary3"));
      assertEquals("sym", ((Symbol)contact.getValue("symbol")).getName());
      assertEquals(Symbol.define("sym"), contact.getValue("symbol"));

      Pair pairA = (Pair)contact.getValue("pair");
      assertEquals("A", pairA.getHead());

      Pair pairB = pairA.getNext();
      assertEquals("B", pairB.getHead());
      assertNull(pairB.getTail());

      char[] cvec = (char[])contact.getValue("cvector");

      assertEquals(3, cvec.length);
      assertEquals('a', cvec[0]);
      assertEquals('b', cvec[1]);
      assertEquals('c', cvec[2]);

      byte[] bvec = (byte[])contact.getValue("bvector");

      assertEquals(3, bvec.length);
      assertEquals(0, bvec[0]);
      assertEquals((byte)0xAB, bvec[1]);
      assertEquals(0x12, bvec[2]);
      
      Object[] svec = (String[])contact.getValue("svector");

      assertEquals(3, svec.length);
      assertEquals("a", svec[0]);
      assertEquals("b", svec[1]);
      assertEquals("c", svec[2]);

      Object[] vec = (Object[])contact.getValue("vector");

      assertEquals(3, vec.length);
      assertEquals("a", vec[0]);
      assertEquals("b", vec[1]);
      assertEquals("c", vec[2]);

      PCodeFunction fun = (PCodeFunction)contact.getValue("function");

      assertEquals(3, fun.code.length);
      assertEquals(0, fun.code[0]);
      assertEquals(1, fun.code[1]);
      assertEquals(2, fun.code[2]);
      assertEquals(1, fun.constants.length);
      assertEquals("abc", fun.constants[0]);

      PCodeMacro mac = (PCodeMacro)contact.getValue("macro");

      assertEquals(3, mac.code.length);
      assertEquals(3, mac.code[0]);
      assertEquals(4, mac.code[1]);
      assertEquals(5, mac.code[2]);
      assertEquals(1, mac.constants.length);
      assertEquals("cde", mac.constants[0]);

      PrivilegeSet privilegeSet = (PrivilegeSet)contact.getValue("privilegeSet");

      assertTrue(privilegeSet.contains(Repository.getMetadata().getPrimitivePrivilege("updateContact")));
      assertTrue(privilegeSet.contains(Repository.getMetadata().getPrimitivePrivilege("WorkflowManage")));
      assertFalse(privilegeSet.contains(Repository.getMetadata().getPrimitivePrivilege("createContact")));

      List addressList = (List)contact.getValue("addresses");

      assertEquals(2, addressList.size());

      TransferObject addr1 = (TransferObject)addressList.get(0);

      oid = addr1.getOID();

      assertEquals(1, oid.getCount());
      assertEquals("456", oid.getValue(0));

      assertEquals("Address", addr1.getClassName());
      assertEquals("update", addr1.getEventName());
      assertEquals(0, addr1.getVersion());
      assertEquals("Canada", addr1.getValue("country"));
      assertSame(addr1, addr1.getValue("self"));
      assertSame(contact, addr1.getValue("contact"));
      assertEquals(contact.getValue("symbol"), addr1.getValue("symbol"));

      TransferObject addr2 = (TransferObject)addressList.get(1);

      assertEquals(2, addr2.getValueCount());
      assertEquals("USA", addr2.getValue("country"));
      assertSame(contact, addr2.getValue("contact"));
      assertSame(req.getCorrelator(), addr2);
   }

   public void testSerializeResponse() throws IOException
   {
      m_marshaller.serialize(m_response, m_writer);
      StringReader reader = new StringReader(m_writer.toString());
      Response resp = (Response)m_unmarshaller.deserialize(reader);

      assertEquals(2, resp.getResultCount());
      assertSame(((List)((TransferObject)resp.getResult(0)).getValue("addresses")).get(1), resp.getResult(1));

      assertEquals(1, resp.getEventCount());
      List eventList = (List)resp.getEvent(0);
      assertSame(eventList.get(0), eventList.get(1));
   }

   public void testSerializeException() throws IOException
   {
      m_marshaller.serialize(m_exception, m_writer);
      StringReader reader = new StringReader(m_writer.toString());
      Throwable t = (Throwable)m_unmarshaller.deserialize(reader);

      assertTrue(t instanceof ValidationException);

      ValidationException e = (ValidationException)t;

      assertEquals("err.validation.requiredAttributes", e.getErrorCode());
      assertEquals(1, e.getErrorArgs().length);
      assertEquals("Contact", e.getClassName());
      assertEquals("123", e.getOIDHolder().getOID().getValue(0));
      assertEquals(1, e.getOrdinal());
      assertEquals(1, e.getExceptionCount());
      assertEquals("err.persistence.queryTimeout", ((GenericException)e.getExceptionIterator().next()).getErrorCode());
      assertEquals("err.validation.requiredAttribute", ((GenericException)e.findException("firstName")).getErrorCode());
   }

   public void checkEquals(Number expected, Number actual)
   {
      assertEquals(expected, actual);
   }
}
