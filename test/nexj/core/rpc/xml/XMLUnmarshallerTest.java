// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import java.util.TimeZone;

import junit.framework.TestCase;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.persistence.OID;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.MockServer;
import nexj.core.rpc.Request;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAPMarshallerException;
import nexj.core.rpc.soap.SOAPUnmarshallerException;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.util.Binary;
import nexj.core.util.SOAPUtil;

public class XMLUnmarshallerTest extends TestCase
{
   public void testTransferObject() throws MarshallerException, IOException
   {
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      Metaclass metaclass = Repository.getMetadata().findMetaclass("User");
      StringReader reader = new StringReader(
         "<User xmlns=\"" + XML.NS_URI_TNS + "\">" +
         "<fullName>fullname</fullName>" +
         "<names>name1</names><names>name2</names><names>name3</names>" +
         "</User>");
      TransferObject tobj = (TransferObject)unmarshaller.deserialize(reader, metaclass);

      assertEquals("User", tobj.getClassName());
      assertNull(tobj.getEventName());
      assertNull(tobj.getOID());
      assertEquals(2, tobj.getValueCount());
      assertEquals("fullname", tobj.findValue("fullName"));

      Object list = tobj.findValue("names");

      assertTrue(list instanceof List);
      assertEquals(3, ((List)list).size());
      assertEquals("name1", ((List)list).get(0));
      assertEquals("name2", ((List)list).get(1));
      assertEquals("name3", ((List)list).get(2));
   }

   public void testDynamicChangeRequest() throws MarshallerException, IOException
   {
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      SchemeParser parser = new SchemeParser(new InvocationContext(Repository.getMetadata())
         .getMachine().getGlobalEnvironment());
      StringReader reader = new StringReader("<Change-Request xmlns=\"" + XML.NS_URI_TNS
         + "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><objects x" + "si:type=\"User\"><"
         + XML.BASE_PREFIX + "oid>10e8359492f25f4be49109b9979e684ff3</" + XML.BASE_PREFIX
         + "oid><" + XML.BASE_PREFIX + "event>welcome</" + XML.BASE_PREFIX
         + "event><fullName>fullname</fullName></objects><attributes>(password)</" + "attributes></Change-Request>");
      XMLChangeRequest changeRequest = (XMLChangeRequest)unmarshaller.deserialize(reader);
      MockServer server = new MockServer();

      changeRequest.invoke(server);

      Request request = server.getRequest();
      Request.Invocation action = request.getInvocation(0);

      assertEquals(1, request.getInvocationCount());
      assertNull(action.getEventName());
      assertEquals("User", request.getObject(0).getClassName());
      assertEquals("welcome", request.getObject(0).getEventName());
      assertEquals(OID.fromBinary(Binary.parse("10e8359492f25f4be49109b9979e684ff3")),
         request.getObject(0).getOID());
      assertEquals(1, request.getObject(0).getValueCount());
      assertEquals("fullname", request.getObject(0).findValue("fullName"));
      assertEquals(parser.parse(new StringReader("(password)"), null), action.getAttributes());
      assertEquals(0, request.getFilterCount());

      assertNull(changeRequest.getMetaclass());
   }
   
   public void testDynamicReadRequest() throws MarshallerException, IOException
   {
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      SchemeParser parser = new SchemeParser(new InvocationContext(Repository.getMetadata())
         .getMachine().getGlobalEnvironment());
      StringReader reader = new StringReader("<Read-Request xmlns=\"" + XML.NS_URI_TNS + "\">"
         + "<class>User-test</class><attributes>(fullName)</attributes>"
         + "<where></where><orderBy></orderBy><count>8</count><offset>0</offset></Read-Request>");
      XMLReadRequest readRequest = (XMLReadRequest)unmarshaller.deserialize(reader);
      MockServer server = new MockServer(new Object[1]);

      readRequest.invoke(server);

      Request request = server.getRequest();
      Request.Invocation action = request.getInvocation(0);

      assertEquals(1, request.getInvocationCount());
      assertEquals("User-test", request.getObject(0).getClassName());
      assertNull(request.getObject(0).getEventName());
      assertNull(request.getObject(0).getOID());
      assertEquals("read", action.getEventName());
      assertEquals(6, action.getArguments().length);
      assertEquals(parser.parse(new StringReader("(fullName)"), null), action.getArguments()[0]);
      assertNull(action.getArguments()[1]);
      assertNull(action.getArguments()[2]);
      assertEquals(8, ((Integer)action.getArguments()[3]).intValue());
      assertEquals(0, ((Integer)action.getArguments()[4]).intValue());
      assertEquals(Boolean.FALSE, action.getArguments()[5]);
      assertEquals(0, request.getFilterCount());

      assertNull(readRequest.getMetaclass());
   }
   
   public void testDynamicRestrictedChangeRequest() throws MarshallerException, IOException
   {
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      SchemeParser parser = new SchemeParser(new InvocationContext(Repository.getMetadata())
         .getMachine().getGlobalEnvironment());
      StringReader reader = new StringReader("<User-Change-Request xmlns=\"" + XML.NS_URI_TNS
         + "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><objects x" + "si:type=\"User\"><"
         + XML.BASE_PREFIX + "oid>10e8359492f25f4be49109b9979e684ff3</" + XML.BASE_PREFIX
         + "oid><" + XML.BASE_PREFIX + "event>welcome</" + XML.BASE_PREFIX
         + "event><fullName>fullname</fullName></objects><attributes>(password)</" + "attributes></User-Change-Request>");
      XMLChangeRequest changeRequest = (XMLChangeRequest)unmarshaller.deserialize(reader);
      MockServer server = new MockServer();

      changeRequest.invoke(server);

      Request request = server.getRequest();
      Request.Invocation action = request.getInvocation(0);

      assertEquals(1, request.getInvocationCount());
      assertEquals("User", request.getObject(0).getClassName());
      assertEquals("welcome", request.getObject(0).getEventName());
      assertEquals(OID.fromBinary(Binary.parse("10e8359492f25f4be49109b9979e684ff3")),
                   request.getObject(0).getOID());
      assertEquals(1, request.getObject(0).getValueCount());
      assertEquals("fullname", request.getObject(0).findValue("fullName"));
      assertEquals(0, request.getFilterCount());
      assertNull(action.getEventName());
      assertEquals(parser.parse(new StringReader("(password)"), null), action.getAttributes());
      assertEquals("User", changeRequest.getMetaclass().getName());
   }

   public void testDynamicRestrictedReadRequest() throws MarshallerException, IOException
   {
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      SchemeParser parser = new SchemeParser(new InvocationContext(Repository.getMetadata())
         .getMachine().getGlobalEnvironment());
      StringReader reader = new StringReader("<User-Read-Request xmlns=\"" + XML.NS_URI_TNS + "\"><class>User-test</class>"
         + "<attributes>(fullName)</attributes><where></where><orderBy></orderBy><count>8</count>"
         + "<offset>0</offset></User-Read-Request>");
      XMLReadRequest readRequest = (XMLReadRequest)unmarshaller.deserialize(reader);
      MockServer server = new MockServer(new Object[1]);

      readRequest.invoke(server);

      Request request = server.getRequest();
      Request.Invocation action = request.getInvocation(0);

      assertEquals(1, request.getInvocationCount());
      assertEquals("User-test", request.getObject(0).getClassName());
      assertNull(request.getObject(0).getEventName());
      assertNull(request.getObject(0).getOID());
      assertEquals("read", action.getEventName());
      assertEquals(6, action.getArguments().length);
      assertEquals(parser.parse(new StringReader("(fullName)"), null), action.getArguments()[0]);
      assertNull(action.getArguments()[1]);
      assertNull(action.getArguments()[2]);
      assertEquals(8, ((Integer)action.getArguments()[3]).intValue());
      assertEquals(0, ((Integer)action.getArguments()[4]).intValue());
      assertEquals(Boolean.FALSE, action.getArguments()[5]);
      assertEquals(0, request.getFilterCount());
      assertEquals("User", readRequest.getMetaclass().getName());
   }

   public void testEvent() throws Exception
   {
      Metadata metadata = new XMLMetadata(null, null, null, null, null);
      Metaclass metaclass = new Metaclass("Test");
      Attribute attr = new Attribute("attr");
      Argument untyped = new Argument("untyped");
      Argument anytyped = new Argument("anytyped");
      Argument inttyped = new Argument("inttyped");
      Argument listtyped = new Argument("listtyped");
      Event protectedEv = new Event("protected");
      Event publicEv = new Event("public");
      Event staticEv = new Event("static");
      MockServer server = new MockServer(new Object[]{null});
      Timestamp ts =
         SOAPUtil.parseDateTime("1234-05-06T07:08:09", true, true, TimeZone.getDefault());

      attr.setType(Primitive.ANY);
      anytyped.setType(Primitive.ANY);
      inttyped.setType(Primitive.INTEGER);
      listtyped.setType(Primitive.STRING);
      listtyped.setCollection(true);
      protectedEv.setVisibility(Metaclass.PROTECTED);
      publicEv.addArgument(untyped);
      publicEv.addArgument(anytyped);
      publicEv.addArgument(inttyped);
      publicEv.addArgument(listtyped);
      staticEv.addArgument(untyped);
      staticEv.addArgument(anytyped);
      staticEv.addArgument(inttyped);
      staticEv.addArgument(listtyped);
      staticEv.setStatic(true);
      staticEv.setVarArg(true);
      metaclass.addAttribute(attr);
      metaclass.addEvent(protectedEv);
      metaclass.addEvent(publicEv);
      metaclass.addEvent(staticEv);
      metadata.addMetaclass(metaclass);

      // test protected event
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(metadata));
      String sElement = XSDGenerator.computeElementName(protectedEv);
      StringReader reader = new StringReader(
         "<" + sElement + " xmlns='" + XML.NS_URI_TNS + '/' + metaclass.getName() + "'/>");

      try
      {
         unmarshaller.deserialize(reader);
         fail(); // SOAPUnmarshallerException expected
      }
      catch (SOAPUnmarshallerException e)
      {
         assertEquals("err.rpc.soap.missingType", e.getErrorCode());
      }

      // test public non-static unbound event
      sElement = XSDGenerator.computeElementName(publicEv);
      reader = new StringReader(
         "<" + sElement + " xmlns:xs='http://www.w3.org/2001/XMLSchema'" +
         " xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'" +
         " xmlns='" + XML.NS_URI_TNS + '/' + metaclass.getName() + "'>" +
         "<untyped xsi:type='xs:decimal'>3.1415926535897932384626433832795</untyped>" +
         "<anytyped xsi:type='xs:dateTime'>1234-05-06T07:08:09</anytyped>" +
         "<inttyped>42</inttyped>" +
         "<listtyped>abc</listtyped><listtyped>def</listtyped>" +
         "</" + sElement + ">");

      try
      {
         unmarshaller.deserialize(reader);
         fail(); // SOAPUnmarshallerException expected
      }
      catch (SOAPUnmarshallerException e)
      {
         assertEquals("err.rpc.soap.unmshComplex", e.getErrorCode());
      }

      // test public non-static event
      sElement = XSDGenerator.computeElementName(publicEv);
      reader = new StringReader(
         "<" + sElement + " xmlns:xs='http://www.w3.org/2001/XMLSchema'" +
         " xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'" +
         " xmlns:tns='" + XML.NS_URI_TNS + "'" +
         " xmlns='" + XML.NS_URI_TNS + '/' + metaclass.getName() + "'>" +
         "<_instance><tns:" + attr.getName() + ">test</tns:" + attr.getName() + "></_instance>" +
         "<untyped xsi:type='xs:decimal'>3.1415926535897932384626433832795</untyped>" +
         "<anytyped xsi:type='xs:dateTime'>1234-05-06T07:08:09</anytyped>" +
         "<inttyped>42</inttyped>" +
         "<listtyped>abc</listtyped><listtyped>def</listtyped>" +
         "</" + sElement + ">");

      ((XMLInvocationRequest)unmarshaller.deserialize(reader)).invoke(server);

      Request request = server.getRequest();

      assertNotNull(request);
      assertEquals(1, request.getInvocationCount());

      Request.Invocation action = request.getInvocation(0);
      TransferObject tobj = action.getObject();
      Object[] argArray = action.getArguments();

      assertEquals(publicEv.getName(), action.getEventName());
      assertEquals(publicEv.getArgumentCount(), argArray.length);
      assertTrue(argArray[0] instanceof BigDecimal);
      assertEquals(3.1415926535897932384626433832795, ((BigDecimal)argArray[0]).doubleValue(), 0);
      assertTrue(argArray[1] instanceof Timestamp);
      assertEquals(ts.getTime(), ((Timestamp)argArray[1]).getTime());
      assertEquals(Primitive.createInteger(42), argArray[2]);
      assertTrue(argArray[3] instanceof List);
      assertEquals(2, ((List)argArray[3]).size());
      assertEquals("abc", ((List)argArray[3]).get(0));
      assertEquals("def", ((List)argArray[3]).get(1));
      assertEquals(metaclass.getName(), tobj.getClassName());
      assertNull(tobj.getEventName());
      assertNull(tobj.getOID());
      assertEquals(1, tobj.getValueCount());
      assertEquals("test", tobj.findValue(attr.getName()));

      // test static bound event
      sElement = XSDGenerator.computeElementName(staticEv);
      reader = new StringReader(
         "<" + sElement + " xmlns:xs='http://www.w3.org/2001/XMLSchema'" +
         " xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'" +
         " xmlns:tns='" + XML.NS_URI_TNS + "'" +
         " xmlns='" + XML.NS_URI_TNS + '/' + metaclass.getName() + "'>" +
         "<_instance><tns:" + attr.getName() + ">test</tns:" + attr.getName() + "></_instance>" +
         "<untyped xsi:type='xs:decimal'>3.1415926535897932384626433832795</untyped>" +
         "<anytyped xsi:type='xs:dateTime'>1234-05-06T07:08:09</anytyped>" +
         "<inttyped>42</inttyped>" +
         "<listtyped>abc</listtyped><listtyped>def</listtyped>" +
         "</" + sElement + ">");

      try
      {
         unmarshaller.deserialize(reader);
         fail(); // SOAPUnmarshallerException expected
      }
      catch (SOAPUnmarshallerException e)
      {
         assertEquals("err.rpc.soap.element", e.getErrorCode());
      }

      // test static event
      sElement = XSDGenerator.computeElementName(staticEv);
      reader = new StringReader(
         "<" + sElement + " xmlns:xs='http://www.w3.org/2001/XMLSchema'" +
         " xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'" +
         " xmlns:tns='" + XML.NS_URI_TNS + "'" +
         " xmlns='" + XML.NS_URI_TNS + '/' + metaclass.getName() + "'>" +
         "<untyped xsi:type='xs:decimal'>3.1415926535897932384626433832795</untyped>" +
         "<anytyped xsi:type='xs:dateTime'>1234-05-06T07:08:09</anytyped>" +
         "<inttyped>42</inttyped>" +
         "<listtyped><tns:item>abc</tns:item><tns:item>def</tns:item></listtyped>" +
         "<listtyped><tns:item>ghi</tns:item><tns:item>jkl</tns:item></listtyped>" +
         "</" + sElement + ">");
      ((XMLInvocationRequest)unmarshaller.deserialize(reader)).invoke(server);
      request = server.getRequest();
      assertNotNull(request);
      assertEquals(1, request.getInvocationCount());
      action = request.getInvocation(0);
      tobj = action.getObject();
      assertEquals(staticEv.getName(), action.getEventName());
      argArray = action.getArguments();
      assertEquals(5, argArray.length); // untyped + anytyped + inttyped + 2*listtyped
      assertTrue(argArray[0] instanceof BigDecimal);
      assertEquals(3.1415926535897932384626433832795, ((BigDecimal)argArray[0]).doubleValue(), 0);
      assertTrue(argArray[1] instanceof Timestamp);
      assertEquals(ts.getTime(), ((Timestamp)argArray[1]).getTime());
      assertEquals(Primitive.createInteger(42), argArray[2]);
      assertTrue(argArray[3] instanceof List);
      assertEquals(2, ((List)argArray[3]).size());
      assertEquals("abc", ((List)argArray[3]).get(0));
      assertEquals("def", ((List)argArray[3]).get(1));
      assertTrue(argArray[3] instanceof List);
      assertEquals("ghi", ((List)argArray[4]).get(0));
      assertEquals("jkl", ((List)argArray[4]).get(1));
      assertEquals(metaclass.getName(), tobj.getClassName());
      assertNull(tobj.getEventName());
      assertNull(tobj.getOID());
      assertEquals(0, tobj.getValueCount());
   }

   public void testCircularRef()
   {
      Object[] array = new Object[1];

      array[0] = Pair.list(array);

      Pair pair = Pair.list(array);

      StringWriter writer = new StringWriter();

      try
      {
         new XMLMarshaller(new InvocationContext(Repository.getMetadata())).serialize(pair, writer);
         fail("Expected exception: err.rpc.xml.circularReference");
      }
      catch (IOException e)
      {
         assertTrue(e.getCause() instanceof SOAPMarshallerException);
         assertEquals("err.rpc.xml.circularReference",
                      ((SOAPMarshallerException)e.getCause()).getErrorCode());
      }
   }
}