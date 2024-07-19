package nexj.core.rpc.json;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;

import junit.framework.TestCase;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.persistence.OID;
import nexj.core.rpc.CharacterStreamMarshallerTest;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.UnmarshallerException;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.GenericException;
import nexj.core.util.StringId;

/**
 * Tests the JSON generic RPC marshaller and unmarshaller.
 */
public class JSONTest extends CharacterStreamMarshallerTest
{
   public JSONTest(String sName)
   {
      super(sName);
   }

   /**
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_marshaller = new JSONMarshaller(new InvocationContext(Repository.getMetadata()));
      m_unmarshaller = new JSONUnmarshaller(new InvocationContext(Repository.getMetadata()));
   }

   /**
    * @see nexj.core.rpc.CharacterStreamMarshallerTest#checkEquals(java.lang.Number, java.lang.Number)
    */
   public void checkEquals(Number expected, Number actual)
   {
      if (expected.getClass() == actual.getClass())
      {
         assertEquals(expected, actual);

         return;
      }

      assertTrue(Math.abs(actual.doubleValue() - expected.doubleValue()) < 0.0000001);
   }

   /**
    * Verifies the JSON output of the serializer.
    */
   public void testSerializeRequestCheckJSON() throws Exception
   {
      m_marshaller.serialize(m_request, m_writer);

      assertEquals(
         "{" +
            "\":namespace\":\"http://www.nexjsystems.com/ns/test\"," +
            "\":version\":\"10\"," +
            "\":async\":true," +
            "\":locale\":\"bg_BG_SF\"," +
            "\":timezone\":\"AST\"," +
            "\":correlator\":" +
            "{" +
               "\":class\":\"Address\"," +
               "\":oid\":\"Qzc4OQ==\"," +
               "\":event\":\"new\"," +
               "\"contact\":" +
               "{" +
                  "\":class\":\"Contact\"," +
                  "\":oid\":\"QzEyMw==\"," +
                  "\":event\":\"update\"," +
                  "\":version\":12345," +
                  "\"lastName\":\"Kava\"," +
                  "\"binary\":" +
                     "{\":binary\":\"AQIDBAU=\"}," +
                  "\"vector\":" +
                     "{\":object[]\":[\"a\",\"b\",\"c\"]}," +
                  "\"null\":null," +
                  "\"self\":" +
                  "{" +
                     "\":class\":\"Contact\"," +
                     "\":oid\":\"QzEyMw==\"" +
                  "}," +
                  "\"macro\":" +
                  "{" +
                     "\":macro\":true," +
                     "\":code\":\"\\u0003\\u0004\\u0005\"," +
                     "\":constants\":[\"cde\"]" +
                  "}," +
                  "\"double\":1.625," +
                  "\"decimal\":1.2345," +
                  "\"binary2\":" +
                     "{\":binary\":\"AQIDBAUGBw==\"}," +
                  "\"binary3\":" +
                     "{\":binary\":\"AQI=\"}," +
                  "\"timestamp\":" +
                  "{" +
                     "\":date\":12345" +
                  "}," +
                  "\"symbol\":" +
                     "{\":symbol\":\"sym\"}," +
                  "\"function\":" +
                  "{" +
                     "\":code\":\"\\u0000\\u0001\\u0002\"," +
                     "\":constants\":[\"abc\"]" +
                  "}," +
                  "\"pair\":" +
                  "{" +
                     "\":head\":\"A\"," +
                     "\":tail\":" +
                     "{" +
                        "\":head\":\"B\"" +
                     "}" +
                  "}," +
                  "\"firstName\":\"Java\"," +
                  "\"float\":0.625," +
                  "\"addresses\":" +
                  "[" +
                     "{" +
                        "\":class\":\"Address\"," +
                        "\":oid\":\"QzQ1Ng==\"," +
                        "\":event\":\"update\"," +
                        "\"contact\":" +
                        "{" +
                           "\":class\":\"Contact\"," +
                           "\":oid\":\"QzEyMw==\"" +
                        "}," +
                        "\"country\":\"Canada\"," +
                        "\"symbol\":" +
                           "{\":symbol\":\"sym\"}," +
                        "\"self\":" +
                        "{" +
                           "\":class\":\"Address\"," +
                           "\":oid\":\"QzQ1Ng==\"" +
                        "}" +
                     "}," +
                     "{" +
                        "\":class\":\"Address\"," +
                        "\":oid\":\"Qzc4OQ==\"" +
                     "}" +
                  "]," +
                  "\"bvector\":{\":byte[]\":\"AKsS\"}," +
                  "\"cvector\":" +
                     "{\":char[]\":\"abc\"}," +
                  "\"boolean\":true," +
                  "\"privilegeSet\":" +
                     "{\":privilegeSet\":\"48000000\"}," +
                  "\"svector\":" +
                     "{\":string[]\":[\"a\",\"b\",\"c\"]}," +
                  "\"long\":2," +
                  "\"integer\":1" +
               "}," +
               "\"country\":\"USA\"" +
            "}," +
            "\":invocations\":" +
            "[" +
               "{" +
                  "\"object\":{" +
                     "\":class\":\"Contact\"," +
                     "\":oid\":\"QzEyMw==\"" +
                  "}" +
               "}," +
               "{" +
                  "\"object\":{" +
                     "\":class\":\"Contact\"," +
                     "\":oid\":\"QzEyMw==\"" +
                  "}," +
                  "\"event\":\"e\"," +
                  "\"arguments\":[\"p1\",0.0]," +
                  "\"attributes\":" +
                     "{" +
                     "\":head\":\"a\"," +
                     "\":tail\":" +
                     "{" +
                        "\":head\":\"b\"" +
                     "}" +
               "}" +
               "}" +
            "]," +
            "\":filters\":" +
            "[" +
               "{" +
                  "\":class\":\"Contact\"," +
                  "\":oid\":\"QzEyMw==\"" +
               "}" +
            "]" +
         "}",
         m_writer.toString());
   }

   public void testServerPrimitives() throws MarshallerException, IOException
   {
      m_marshaller.serialize(Primitive.createInteger(234), m_writer);
      assertEquals("234", m_writer.toString());

      Integer i = (Integer)m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(Primitive.createInteger(234), i);

      clear();
      m_marshaller.serialize(new Short((short)234), m_writer);
      assertEquals("234", m_writer.toString());

      i = (Integer)m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(234, i.shortValue());

      clear();
      m_marshaller.serialize(Primitive.createLong(Long.MAX_VALUE), m_writer);
      assertEquals("9223372036854775807", m_writer.toString());

      Long l = (Long) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(Primitive.createLong(Long.MAX_VALUE), l);

      clear();
      m_marshaller.serialize(Primitive.createFloat(0.625f), m_writer);
      assertEquals("0.625", m_writer.toString());

      Number number = (Number) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(0.625, number.floatValue(), 0);

      clear();
      m_marshaller.serialize(Primitive.createDouble(Double.MAX_VALUE), m_writer);
      assertEquals("1.7976931348623157E308", m_writer.toString());

      Double d = (Double) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(Primitive.createDouble(Double.MAX_VALUE), d);

      clear();
      m_marshaller.serialize(BigDecimal.valueOf(Double.MAX_VALUE), m_writer);
      assertEquals("1.7976931348623157E308", m_writer.toString());

      d = (Double) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      checkEquals(BigDecimal.valueOf(Double.MAX_VALUE), d);
   }

   public void testServerArrays() throws MarshallerException, IOException
   {
      m_marshaller.serialize(new char[] { 'a', 'b',1,'1' }, m_writer);
      assertEquals("{\":char[]\":\"ab\\u00011\"}", m_writer.toString());

      char[] cArray = (char[]) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(4, cArray.length);
      assertEquals('a', cArray[0]);
      assertEquals('b', cArray[1]);
      assertEquals(1, cArray[2]);
      assertEquals('1', cArray[3]);

      clear();
      m_marshaller.serialize(new String[] { "ab", "b" }, m_writer);
      assertEquals("{\":string[]\":[\"ab\",\"b\"]}", m_writer.toString());

      String[] sArray = (String[]) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(2, sArray.length);
      assertEquals("ab", sArray[0]);
      assertEquals("b", sArray[1]);

      clear();

      m_marshaller.serialize(new Object[] { Primitive.ONE_INTEGER, new String[] { "ab", "b" } }, m_writer);
      assertEquals("{\":object[]\":[1,{\":string[]\":[\"ab\",\"b\"]}]}", m_writer.toString());

      Object[] oArray = (Object[]) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));

      assertEquals(2, oArray.length);
      assertEquals(Primitive.ONE_INTEGER, oArray[0]);

      sArray = (String[]) oArray[1];
      assertEquals(2, sArray.length);
      assertEquals("ab", sArray[0]);
      assertEquals("b", sArray[1]);

      clear();
      m_marshaller.serialize(new byte[] { 1, 2, 'c' }, m_writer);
      assertEquals("{\":byte[]\":\"AQJj\"}", m_writer.toString());

      byte[] b = (byte[]) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(3, b.length);
      assertEquals(1, b[0]);
      assertEquals(2, b[1]);
      assertEquals('c', b[2]);

      clear();
      ArrayList aList = new ArrayList();
      aList.add(Primitive.ONE_INTEGER);
      aList.add(new String[] { "ab", "b" });
      aList.add("ab cd");

      m_marshaller.serialize(aList, m_writer);
      assertEquals("[1,{\":string[]\":[\"ab\",\"b\"]},\"ab cd\"]", m_writer.toString());

      ArrayList aL = (ArrayList) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(3, aL.size());
      assertEquals(Primitive.ONE_INTEGER, aL.get(0));
      assertEquals("ab cd", (String) aL.get(2));

      sArray = (String[]) aL.get(1);
      assertEquals(2, sArray.length);
      assertEquals("ab", sArray[0]);
      assertEquals("b", sArray[1]);
   }

   public void testServerObjects() throws MarshallerException, IOException
   {
      m_marshaller.serialize(new Timestamp(12345), m_writer);
      assertEquals("{\":date\":12345}", m_writer.toString());

      Timestamp ts = (Timestamp) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(new Timestamp(12345), ts);

      clear();
      m_marshaller.serialize(new StringId("Stri,ng\"Id"), m_writer);
      assertEquals("{\":id\":\"Stri,ng\\\"Id\"}", m_writer.toString());

      StringId sID = (StringId) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals((new StringId("Stri,ng\"Id")).toString(), sID.toString());

      clear();
      m_marshaller.serialize(new Character('c'), m_writer);
      assertEquals("{\":char\":\"c\"}", m_writer.toString());

      Character c = (Character) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(new Character('c'), c);

      clear();
      m_marshaller.serialize(new OID(new Object[] { null, "a", Primitive.createInteger(123), Primitive.createLong(567),
               Primitive.createDouble(1.234), new BigDecimal("456.789"), new Date(1234567), Boolean.FALSE,
               new Binary(new byte[] { (byte) 134, 45, (byte) 173 }) }), m_writer);
      assertEquals("{\":OID\":\"wEFhwQAAAHvCAAAAAAAAAjfEP/O+dsi0OViDAAAAAwb4VcUAAAAAABLWh8YDhi2t\"}", m_writer.toString());

      OID oid = (OID) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(new OID(new Object[] { null, "a", Primitive.createInteger(123), Primitive.createLong(567), Primitive.createDouble(1.234),
               new BigDecimal("456.789"), new Date(1234567), Boolean.FALSE,
               new Binary(new byte[] { (byte) 134, 45, (byte) 173 }) }), oid);

      clear();
      m_marshaller.serialize(new Pair("A", new Pair(new Pair("B",Symbol.define("C")))), m_writer);
      assertEquals("{\":head\":\"A\",\":tail\":{\":head\":{\":head\":\"B\",\":tail\":{\":symbol\":\"C\"}}}}", m_writer.toString());

      Pair p = (Pair) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));
      assertEquals(new Pair("A", new Pair(new Pair("B",Symbol.define("C")))), p);
   }

   public void testSerializeException() throws MarshallerException, IOException
   {
      m_marshaller.serialize(m_exception, m_writer);
      assertEquals(
         "{"+
            "\":errorCode\":\"err.validation.requiredAttributes\","+
            "\":errorMessage\":\"err.validation.requiredAttributes\","+
            "\":errorArgs\":[\"Contact\"],"+
            "\":errorClass\":\"Contact\","+
            "\":errorOID\":\"QzEyMw==\","+
            "\":ordinal\":1,"+
            "\":attributes\":[\"firstName\"],"+
            "\":attributeExceptions\":"+
               "["+
                  "{"+
                     "\":errorCode\":\"err.validation.requiredAttribute\","+
                     "\":errorMessage\":\"The firstName of Contact must be specified.\","+
                     "\":errorArgs\":"+
                     "["+
                        "\"firstName\","+
                        "\"Contact\""+
                     "],"+
                     "\":errorClass\":null,"+
                     "\":errorOID\":null,"+
                     "\":ordinal\":-1,"+
                     "\":attributes\":null,"+
                     "\":attributeExceptions\":null,"+
                     "\":exceptions\":null"+
                  "}"+
               "],"+
            "\":exceptions\":"+
               "["+
                  "{"+
                     "\":errorCode\":\"err.persistence.queryTimeout\","+
                     "\":errorMessage\":\"The query has timed out, most likely due to unsupported selection criteria or to system overload.\","+
                     "\":errorArgs\":null,"+
                     "\":errorClass\":null,"+
                     "\":errorOID\":null,"+
                     "\":ordinal\":null,"+
                     "\":attributes\":null,"+
                     "\":attributeExceptions\":null,"+
                     "\":exceptions\":null"+
                  "}"+
               "]"+
            "}",
         m_writer.toString());

      Throwable t = (Throwable) m_unmarshaller.deserialize(new StringReader(m_writer.toString()));

      assertTrue(t instanceof ValidationException);

      ValidationException e = (ValidationException) t;

      assertEquals("err.validation.requiredAttributes", e.getErrorCode());
      assertEquals(1, e.getErrorArgs().length);
      assertEquals("Contact", e.getClassName());
      assertEquals("123", e.getOIDHolder().getOID().getValue(0));
      assertEquals(1, e.getOrdinal());
      assertEquals(1, e.getExceptionCount());
      assertEquals("err.persistence.queryTimeout", ((GenericException) e.getExceptionIterator().next()).getErrorCode());
      assertEquals("err.validation.requiredAttribute", ((GenericException) e.findException("firstName")).getErrorCode());

   }

   public void testSchemeExpression() throws UnmarshallerException, IOException
   {
      m_unmarshaller = new JSONUnmarshaller(new InvocationContext(Repository.getMetadata()));

      Pair pair = (Pair) m_unmarshaller.deserialize(new StringReader("{\":expression\":\"(firstName lastName fullName \\\"loginName\\\" )\"}"));
      assertEquals(Pair.list(Symbol.define("firstName"), Symbol.define("lastName"), Symbol.define("fullName") , "loginName"), pair);
   }

   public void clear()
   {
      ((StringWriter) m_writer).getBuffer().setLength(0);
   }
}