// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.xml.transform.dom.DOMSource;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.Repository;
import nexj.core.persistence.OID;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.MockServer;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAPFault;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.GenericException;
import nexj.core.util.IOUtil;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.LookupDeque;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;
import nexj.test.util.TempFileUtil;

public class XMLMarshallerTest extends TestCase
{
   private final static URL SOAP_ENVELOPE_URL =
      XMLMarshallerTest.class.getResource("SOAPEnvelope.xsd"); // hardcoded copy

   private final static URL XML_NAMESPACE_URL =
      XMLMarshallerTest.class.getResource("XMLNamespace.xsd"); // hardcoded copy

   private File m_tmpSOAPXSD;
   private File m_tmpTNSXSD;
   private File m_tmpTNSBasicXSD;
   private Request m_request;
   private Response m_response;
   private Exception m_exception;
   private SOAPFault m_soapFault;
   private XMLMarshaller m_marshaller;
   private XMLUnmarshaller m_unmarshaller;
   private Writer m_writer;
   private LookupDeque m_xsdMap = new LinkedHashTab();
   private LookupDeque m_xsdMapBasic = new LinkedHashTab();

   /**
    * compare string values of valid vs. other without comparing any pointer references
    * and assert their quality
    * @param valid the valid value
    * @param other the other value
    */
   public void assertEqualsPointerless(Object valid, Object other)
   {
      assertEquals((valid == null)
                   ? null : valid.toString().replaceAll("(=[A-Za-z0-9\\.]+@)[a-f0-9]+", "$1"),
                   (other == null)
                   ? null : other.toString().replaceAll("(=[A-Za-z0-9\\.]+@)[a-f0-9]+", "$1"));
   }

   protected Node generateXSD(XSDGenerator gen) throws IOException
   {
      StringWriter xsdOut = new StringWriter();

      gen.setCompatible(false);
      gen.generate(xsdOut, Repository.getMetadata().getMetaclassIterator(), null);

      // extract the schema portion of the generated XML
      Document xsdDoc = XMLUtil.parse(new StringReader(xsdOut.toString()));
      List/*<Node>*/ namespaceList = new ArrayList/*<Node>*/();
      Node xsdNode;

      for (xsdNode = xsdDoc.getFirstChild();
           !XML.XSD_URI.equals(xsdNode.getNamespaceURI()) ||
           !"schema".equals(xsdNode.getLocalName());
           xsdNode = xsdNode.getFirstChild()) // for every ancestor extract its attributes
      {
         NamedNodeMap attr = xsdNode.getAttributes();

         for (int i = 0, nCount = (attr == null) ? 0 : attr.getLength(); i < nCount; ++i)
         {
            if ("http://www.w3.org/2000/xmlns/".equals(attr.item(i).getNamespaceURI()) &&
                !"xmlns".equals(attr.item(i).getLocalName())) // ignore namespace declaration
            {
               namespaceList.add(attr.item(i).cloneNode(false));
            }
         }
      }

      while (!namespaceList.isEmpty()) // add all attributes to XSD schema node
      {
         xsdNode.getAttributes().setNamedItemNS(
            (Node)namespaceList.remove(namespaceList.size() - 1));
      }

      return xsdNode;
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      InputStream istream = URLUtil.openStream(SOAP_ENVELOPE_URL);
      Document xsdSOAP;

      try
      {
         xsdSOAP = XMLUtil.parse(new InputSource(istream));
      }
      finally
      {
         IOUtil.close(istream);
      }

      Element schema = XMLUtil.findChildElement(xsdSOAP, "xs:schema"); // hardcoded in XSD file

      schema.setAttribute("elementFormDefault", "qualified"); // needed for SOAP->Fault->faultcode

      m_tmpSOAPXSD = TempFileUtil.makeTemporaryFile();
      m_tmpTNSXSD = TempFileUtil.makeTemporaryFile();
      m_tmpTNSBasicXSD = TempFileUtil.makeTemporaryFile();

      XMLUtil.formatXML(
         new DOMSource(xsdSOAP),
         true,
         new OutputStreamWriter(new FileOutputStream(m_tmpSOAPXSD), IOUtil.ENCODING));
      XMLUtil.formatXML(
         new DOMSource(generateXSD(new WSDLGenerator(""))),
         true,
         new OutputStreamWriter(new FileOutputStream(m_tmpTNSXSD), IOUtil.ENCODING));
      XMLUtil.formatXML(
         new DOMSource(generateXSD(new WSDLBasicGenerator(""))),
         true,
         new OutputStreamWriter(new FileOutputStream(m_tmpTNSBasicXSD), IOUtil.ENCODING));

      m_xsdMap.put(XML.NS_URI_TNS, m_tmpTNSXSD.toURL());
      m_xsdMap.put(XML.NS_URI_XML, XML_NAMESPACE_URL);
      m_xsdMap.put(XML.ENV_URI, m_tmpSOAPXSD.toURL());

      m_xsdMapBasic.put(XML.NS_URI_TNS, m_tmpTNSBasicXSD.toURL());
      m_xsdMapBasic.put(XML.NS_URI_XML, XML_NAMESPACE_URL);
      m_xsdMapBasic.put(XML.ENV_URI, m_tmpSOAPXSD.toURL());

      m_request = new Request();

      TransferObject contact = new TransferObject();

      contact.setOID(new OID(new Object[]{"123"}));

      // make sure not to be same as an existing metaclass,
      // otherwise only metaclass fields are valid
      contact.setClassName("Contact-test");

      contact.setEventName("update");
      contact.setVersion((short)12345);
      contact.setValue("firstName", "Java");
      contact.setValue("lastName", "Kava");
      contact.setValue("null", null);
      contact.setValue("self", contact);
      contact.setValue("integer", new Integer(1));
      contact.setValue("long", new Long(2));
      contact.setValue("float", new Float(0.625f));
      contact.setValue("double", new Double(1.625));
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

      ArrayList addressList = new ArrayList();
      
      TransferObject address = new TransferObject();
      address.setOID(new OID(new Object[]{"456"}));

      // make sure not to be same as an existing metaclass,
      // otherwise only metaclass fields are valid
      address.setClassName("Address-test");

      address.setEventName("update");
      address.setValue("country", "Canada");
      address.setValue("self", address);
      address.setValue("contact", contact);
      address.setValue("symbol", Symbol.define("sym"));
      addressList.add(address);
      
      address = new TransferObject();
      address.setOID(new OID(new Object[]{"789"}));

      // make sure not to be same as an existing metaclass,
      // otherwise only metaclass fields are valid
      address.setClassName("Address-test");

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
      m_request.addInvocation(contact, Pair.list("a", "b"));
      m_request.addInvocation(contact,
                       new Object[]{ new Integer(42),
                                     new Double(Math.PI),
                                     new String("Hello World!") },
                       Pair.list("c", "d"));
      m_request.addInvocation(
         contact, "invokeEv", new Object[]{new Long(-1), new Double(Math.E)}, Pair.list("e", "f"));
      m_request.addFilter(contact);

      m_response = new Response();
      m_response.addResult(contact);
      m_response.addResult(address);

      ArrayList eventList = new ArrayList();

      eventList.add(address);
      eventList.add(address);

      m_response.addEvent(eventList);
      
      ValidationException e = new ValidationException("err.validation.requiredAttributes",
                                                      new Object[]{"Contact"});

      e.addException(new MetadataValidationException("err.meta.x", new Object[]{"a", "b"}));
      e.addException(new QueryTimeoutException());
      e.setClassName("Contact");
      e.setOIDHolder(contact);
      e.setOrdinal(1);
      e.addException("firstName",
                     new ValidationException("err.validation.requiredAttribute",
                                             new Object[]{"firstName", "Contact"}));

      m_exception = e;

      m_soapFault = new SOAPFault(m_exception);

      m_marshaller = new XMLMarshaller(new InvocationContext(Repository.getMetadata()),
                                       Repository.getMetadata());
      m_unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()),
                                           Repository.getMetadata());
      
      m_writer = new StringWriter();
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_tmpSOAPXSD.delete();
      m_tmpTNSXSD.delete();
      m_tmpTNSBasicXSD.delete();

      super.tearDown();

      m_request = null;
      m_response = null;
      m_exception = null;
      m_soapFault = null;
      m_marshaller = null;
      m_unmarshaller = null;
      m_writer = null;
      m_xsdMap = null;
      m_xsdMapBasic = null;
   }

   public void testSerializeRequest() throws IOException
   {
      m_marshaller.serialize(m_request, m_writer);
      XMLUtil.parse(new StringReader(m_writer.toString()), m_xsdMap); // validate to XSD

      StringReader reader = new StringReader(m_writer.toString());
      Request req = (Request)m_unmarshaller.deserialize(reader);

      assertEquals("http://www.nexjsystems.com/ns/test", req.getNamespace());
      assertEquals("10", req.getVersion());
      assertEquals(true, req.isAsync());
      assertEquals(true, req.isCommit());
      assertEquals("bg_BG_SF", req.getLocale().toString());
      assertEquals("AST", req.getTimeZone().getID());
      assertEquals(4, req.getInvocationCount());
      assertEquals(1, req.getFilterCount());

      TransferObject contact = req.getObject(0);

      // remove all pointer references,
      // objects are going to be different due to being unmarshalled from different branches of the
      // XML tree (i.e. first argument and first filter blocks)
      assertEqualsPointerless(contact, req.getFilter(0));

      // remove all pointer references,
      // objects are going to be different due to being unmarshalled from different branches of the
      // XML tree (i.e. first argument and first filter blocks)
      assertEqualsPointerless(contact, req.getObject(1));

      Request.Invocation action = req.getInvocation(0);

      assertTrue(contact == action.getObject());
      assertNull(action.getEventName());
      assertNull(action.getAttributes());

      Pair pair = req.getInvocation(1).getAttributes();

      assertEquals("a", pair.getHead());
      assertEquals("b", pair.getNext().getHead());
      assertNull(pair.getNext().getNext());
      
      Object[] params = req.getInvocation(2).getArguments();
      assertEquals(3, params.length);
      assertTrue(params[0] instanceof Integer);
      assertEquals(42, ((Integer)params[0]).intValue());
      assertTrue(params[1] instanceof Double);
      assertEquals(Math.PI, ((Double)params[1]).doubleValue(), 0);
      assertTrue(params[2] instanceof String);
      assertEquals("Hello World!", params[2]);

      action = req.getInvocation(3);
      assertEquals("invokeEv", action.getEventName());
      assertEquals("update", action.getObject().getEventName());
      params = action.getArguments();
      assertEquals(2, params.length);
      assertTrue(params[0] instanceof Long);
      assertEquals(-1, ((Long)params[0]).intValue());
      assertTrue(params[1] instanceof Double);
      assertEquals(Math.E, ((Double)params[1]).doubleValue(), 0);
      pair = action.getAttributes();
      assertEquals("e", pair.getHead());
      assertEquals("f", pair.getNext().getHead());
      assertNull(pair.getNext().getNext());

      OID oid = contact.getOID();

      assertEquals(1, oid.getCount());
      assertEquals("123", oid.getValue(0));

      assertEquals("Contact-test", contact.getClassName());
      assertEquals("update", contact.getEventName());
      assertEquals(12345, contact.getVersion());
      assertEquals(24, contact.getValueCount());
      assertEquals("Java", contact.getValue("firstName"));
      assertEquals("Kava", contact.getValue("lastName"));
      assertNull(contact.getValue("null"));
      assertSame(contact, contact.getValue("self"));
      assertEquals(new Integer(1), contact.getValue("integer"));
      assertEquals(new Long(2), contact.getValue("long"));
      assertEquals(new Float(0.625f), contact.getValue("float"));
      assertEquals(new Double(1.625), contact.getValue("double"));
      assertEquals(new BigDecimal("1.2345"), contact.getValue("decimal"));
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

      String[] svec = (String[])contact.getValue("svector");

      assertEquals(3, svec.length);
      assertEquals("a", svec[0]);
      assertEquals("b", svec[1]);
      assertEquals("c", svec[2]);

      // XMLMarshaller is hardcoded to marshal Object[] as List for compatibility with MS.NET
      List vec = (List)contact.getValue("vector");

      assertEquals(3, vec.size());
      assertEquals("a", vec.get(0));
      assertEquals("b", vec.get(1));
      assertEquals("c", vec.get(2));

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

      assertTrue(privilegeSet.contains(Repository
                                          .getMetadata()
                                             .getPrimitivePrivilege("updateContact")));
      assertTrue(privilegeSet.contains(Repository
                                          .getMetadata()
                                             .getPrimitivePrivilege("WorkflowManage")));
      assertFalse(privilegeSet.contains(Repository
                                           .getMetadata()
                                              .getPrimitivePrivilege("createContact")));

      List addressList = (List)contact.getValue("addresses");

      assertEquals(2, addressList.size());

      TransferObject addr1 = (TransferObject)addressList.get(0);

      oid = addr1.getOID();

      assertEquals(1, oid.getCount());
      assertEquals("456", oid.getValue(0));

      assertEquals("Address-test", addr1.getClassName());
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
      
      // remove all pointer references,
      // objects are going to be different due to being unmarshalled from different branches of the
      // XML tree (i.e. correlator and first argument blocks)
      assertEqualsPointerless(req.getCorrelator(), addr2);
   }
   
   public void testSerializeResponse() throws IOException
   {
      m_marshaller.serialize(m_response, m_writer);
      XMLUtil.parse(new StringReader(m_writer.toString()), m_xsdMap); // validate to XSD

      StringReader reader = new StringReader(m_writer.toString());
      Response resp = (Response)m_unmarshaller.deserialize(reader);

      assertEquals(2, resp.getResultCount());

      // remove all pointer references,
      // objects are going to be different due to being unmarshalled from different branches of the
      // XML tree (i.e. first argument and second argument blocks)
      // XMLMarshaller is hardcoded to marshal List as Object[] for compatibility with MS.NET
      assertEqualsPointerless(
         ((List)((TransferObject)resp.getResult(0)).getValue("addresses")).get(1),
         resp.getResult(1));

      assertEquals(1, resp.getEventCount());
      List eventList = (List)resp.getEvent(0);

      // remove all pointer references,
      // objects are going to be different due to being unmarshalled from different branches of the
      // XML tree (i.e. first event and second event blocks)
      assertEqualsPointerless(eventList.get(0), eventList.get(1));
   }
   
   public void testSerializeException() throws IOException
   {
      m_marshaller.serialize(m_exception, m_writer);
      XMLUtil.parse(new StringReader(m_writer.toString()), m_xsdMap); // validate to XSD

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
      assertEquals("err.persistence.queryTimeout",
                   ((GenericException)e.getExceptionIterator().next()).getErrorCode());
      assertEquals("err.validation.requiredAttribute",
                   ((GenericException)e.findException("firstName")).getErrorCode());
   }

   public void testSerializeSOAPFault() throws IOException
   {
      m_marshaller.serialize(m_soapFault, m_writer);
      XMLUtil.parse(new StringReader(m_writer.toString()), m_xsdMap); // validate to XSD

      StringReader reader = new StringReader(m_writer.toString());
      SOAPFault fault = (SOAPFault)m_unmarshaller.deserialize(reader);

      assertEquals("http://schemas.xmlsoap.org/soap/envelope/", fault.getURI());
      assertEquals("Server.nexj.core.runtime.ValidationException", fault.getCode());
      assertEquals("err.validation.requiredAttributes(\"Contact\")", fault.getMessage());
      assertEquals("err.validation.requiredAttributes",
                   ((ValidationException)fault.getException()).getErrorCode());
   }

   public void testSerializeTransferObject() throws IOException
   {
      Request request = new Request();
      TransferObject tobj = new TransferObject();
      List/*<String>*/ list = new ArrayList/*<String>*/(3);

      list.add("name1");
      list.add("name2");
      list.add("name3");
      tobj.setClassName("User");
      tobj.setValue("firstName", "fName");
      tobj.setValue("names", list);
      request.setVersion("0");
      request.addInvocation(tobj);
      m_marshaller.serialize(request, m_writer);
      XMLUtil.parse(new StringReader(m_writer.toString()), m_xsdMap); // validate to XSD

      request = (Request)m_unmarshaller.deserialize(new StringReader(m_writer.toString()));

      TransferObject processed = request.getObject(0);

      assertEquals(tobj.getClassName(), processed.getClassName());
      assertEquals(tobj.getValueCount(), processed.getValueCount());
      assertEquals(tobj.getValue("firstName"), processed.getValue("firstName"));

      Object names = processed.getValue("names");

      assertTrue(names instanceof List);
      assertEquals(list.size(), ((List)names).size());
      assertEquals(list.get(0), ((List)names).get(0));
      assertEquals(list.get(1), ((List)names).get(1));
      assertEquals(list.get(2), ((List)names).get(2));
   }

   public void testBasicResponse() throws IOException
   {
      StringWriter writer = new StringWriter();
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      Response response = new Response();
      TransferObject tobj = new TransferObject("User");

      tobj.setValue("e", new Double(2.71828183));
      tobj.setValue("privilegeSet", new Integer(42));
      tobj.setValue("name", "Name");
      response.addResult(tobj);

      writer.getBuffer().setLength(0);
      XMLChangeRequest request = (XMLChangeRequest)unmarshaller.deserialize(
         new StringReader("<Change-Request xmlns=\"" + XML.NS_URI_TNS
         + "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><objects xsi:type=\"User\"><"
         + XML.BASE_PREFIX + "oid>10e8359492f25f4be49109b9979e684ff3</" + XML.BASE_PREFIX
         + "oid><" + XML.BASE_PREFIX + "event>welcome</" + XML.BASE_PREFIX
         + "event><name>fullname</name></objects><attributes>(e privilegeSet name)</attributes></Change-Request>"));

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMap); // validate to XSD

      Node root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Change-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertEquals(1, root.getChildNodes().item(0).getAttributes().getLength());
      assertEquals("User",
                   root
                      .getChildNodes()
                         .item(0)
                            .getAttributes()
                               .getNamedItem(XML.XSI_NS + ":type")
                                  .getNodeValue());
      assertEquals(6, root.getChildNodes().item(0).getChildNodes().getLength());

      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));
      assertEquals("e",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "keys")
                         .getTextContent());
      assertEquals(XML.XSD_NS + ":double",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getAttributes()
                            .getNamedItem(XML.XSI_NS + ":type")
                               .getNodeValue());
      assertEquals("2.71828183",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), 
                                        XML.BASE_PREFIX + "values")
                         .getTextContent());
      assertEquals(XML.XSD_NS + ":int",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), "privilegeSet")
                         .getAttributes()
                            .getNamedItem(XML.XSI_NS + ":type")
                               .getNodeValue());
      assertEquals("42",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), "privilegeSet")
                         .getTextContent());
      assertEquals("Name",
                   XMLUtil.findChildElement(root.getChildNodes().item(0), "name").getTextContent());

      writer.getBuffer().setLength(0);
      request = (XMLChangeRequest)unmarshaller.deserialize(new StringReader("<User-Change-Request xmlns=\"" + XML.NS_URI_TNS
         + "\"><objects><" + XML.BASE_PREFIX + "oid>10e8359492f25f4be49109b9979e684ff3</"
         + XML.BASE_PREFIX + "oid><" + XML.BASE_PREFIX + "event>welcome</"
         + XML.BASE_PREFIX + "event><name>fullname</name></objects><attributes>(e privilegeSet name)</attributes></User-Change-Request>"));
      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMapBasic); // validate to XSD
      root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "User-Change-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertFalse(root.getChildNodes().item(0).hasAttributes());
      assertEquals(6, root.getChildNodes().item(0).getChildNodes().getLength());

      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));
      assertEquals("e",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "keys")
                         .getTextContent());
      assertEquals(XML.XSD_NS + ":string",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getAttributes()
                            .getNamedItem(XML.XSI_NS + ":type")
                               .getNodeValue());
      assertEquals("2.71828183",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getTextContent());
      assertNull(XMLUtil
                    .findChildElement(root.getChildNodes().item(0), "privilegeSet")
                       .getAttributes()
                          .getNamedItem(XML.XSI_NS + ":type"));
      assertEquals("42",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), "privilegeSet")
                         .getTextContent());
      assertEquals("Name",
                   XMLUtil.findChildElement(root.getChildNodes().item(0), "name").getTextContent());
   }

   public void testDynamicChangeResponse() throws MarshallerException, IOException
   {
      StringWriter writer = new StringWriter();
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      StringReader reader = new StringReader("<Change-Request xmlns=\"" + XML.NS_URI_TNS
         + "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><objects x" + "si:type=\"Principal\"><"
         + XML.BASE_PREFIX + "oid>10e8359492f25f4be49109b9979e684ff3</" + XML.BASE_PREFIX
         + "oid><" + XML.BASE_PREFIX + "event>welcome</" + XML.BASE_PREFIX
         + "event><name>fullname</name></objects><attributes>(firstName name)</attributes></Change-Request>");
      Response response = new Response();
      TransferObject tobj = new TransferObject("User");

      tobj.setValue("firstName", "FName");
      tobj.setValue("name", "fullName");
      response.addResult(tobj);
      XMLChangeRequest request = (XMLChangeRequest)unmarshaller.deserialize(reader);

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMap); // validate to XSD

      Node root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Change-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertEquals(1, root.getChildNodes().item(0).getAttributes().getLength());
      assertEquals("User",
                   root
                      .getChildNodes()
                         .item(0)
                            .getAttributes()
                               .getNamedItem(XML.XSI_NS + ":type")
                                  .getNodeValue());
      assertEquals(5, root.getChildNodes().item(0).getChildNodes().getLength());

      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));

      // this is not an attribute of 'User' hence it goes into _keys/_values
      assertEquals("firstName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "keys")
                         .getTextContent());

      // this is not an attribute of 'User' hence it goes into _keys/_values
      assertEquals("FName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getTextContent());

      assertEquals("fullName",
                   XMLUtil.findChildElement(root.getChildNodes().item(0), "name").getTextContent());
   }

   public void testDynamicReadResponse() throws MarshallerException, IOException
   {
      StringWriter writer = new StringWriter();
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      StringReader reader = new StringReader("<Read-Request xmlns=\"" + XML.NS_URI_TNS + "\"><class>Principal</class>"
         + "<attributes>(fullName name)</attributes><where></where><orderBy></orderBy><count>8</count>"
         + "<offset>0</offset></Read-Request>");
      Response response = new Response();
      TransferObject tobj = new TransferObject("User");

      tobj.setValue("fullName", "FName");
      tobj.setValue("name", null);
      response.addResult(Arrays.asList(new Object[]{tobj}));
      XMLReadRequest request = (XMLReadRequest)unmarshaller.deserialize(reader);

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMap); // validate to XSD

      Node root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Read-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertEquals(1, root.getChildNodes().item(0).getAttributes().getLength());
      assertEquals("User",
                   root
                      .getChildNodes()
                         .item(0)
                            .getAttributes()
                               .getNamedItem(XML.XSI_NS + ":type")
                                  .getNodeValue());
      assertEquals(5, root.getChildNodes().item(0).getChildNodes().getLength());

      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));
      assertEquals("FName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), "fullName")
                         .getTextContent());
      assertEquals("name",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "keys")
                         .getTextContent());
      assertEquals("true",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getAttributes()
                            .getNamedItem(XML.XSI_NS + ":nil")
                               .getNodeValue());
   }

   public void testDynamicRestrictedChangeResponse() throws MarshallerException, IOException
   {
      StringWriter writer = new StringWriter();
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      StringReader reader = new StringReader("<Principal-Change-Request xmlns=\"" + XML.NS_URI_TNS + "\"><objects><"
         + XML.BASE_PREFIX + "oid>10e8359492f25f4be49109b9979e684ff3</" + XML.BASE_PREFIX
         + "oid><" + XML.BASE_PREFIX + "event>welcome</" + XML.BASE_PREFIX
         + "event><name>fullname</name></objects><attributes>(fullName name)</attributes></Principal-Change-Request>");
      Response response = new Response();
      TransferObject tobj = new TransferObject("User");

      tobj.setValue("fullName", "FName");
      tobj.setValue("name", "Name");
      response.addResult(tobj);
      XMLChangeRequest request = (XMLChangeRequest)unmarshaller.deserialize(reader);

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMapBasic); // validate to XSD

      Node root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Principal-Change-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertFalse(root.getChildNodes().item(0).hasAttributes());
      assertEquals(6, root.getChildNodes().item(0).getChildNodes().getLength());

      assertEquals("User",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "class")
                         .getTextContent());
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));
      assertEquals("fullName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "keys")
                         .getTextContent());
      assertEquals("FName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getTextContent());
      assertEquals("Name",
                   XMLUtil.findChildElement(root.getChildNodes().item(0), "name").getTextContent());
   }

   public void testDynamicRestrictedReadResponse() throws MarshallerException, IOException
   {
      StringWriter writer = new StringWriter();
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      StringReader reader = new StringReader("<Principal-Read-Request xmlns=\"" + XML.NS_URI_TNS
         + "\"><class>User-test</class>"
         + "<attributes>(fullName)</attributes><where></where><orderBy></orderBy><count>8</count>"
         + "<offset>0</offset></Principal-Read-Request>");
      Response response = new Response();
      TransferObject tobj = new TransferObject("User");
      
      tobj.setValue("fullName", "FName");
      tobj.setValue("name", "Name");
      response.addResult(Arrays.asList(new Object[]{tobj}));
      XMLReadRequest request = (XMLReadRequest)unmarshaller.deserialize(reader);

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMapBasic); // validate to XSD

      Node root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Principal-Read-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertFalse(root.getChildNodes().item(0).hasAttributes());
      assertEquals(6, root.getChildNodes().item(0).getChildNodes().getLength());

      assertEquals("User",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "class")
                         .getTextContent());
      assertNotNull(XMLUtil
                       .findChildElement(root.getChildNodes().item(0),
                                         XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil
                       .findChildElement(root.getChildNodes().item(0),
                                         XML.BASE_PREFIX + "version"));
      assertEquals("fullName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "keys")
                          .getTextContent());
      assertEquals("FName",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0),
                                        XML.BASE_PREFIX + "values")
                         .getTextContent());
      assertEquals("Name",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), "name")
                         .getTextContent());
   }

   public void testNestedMetaclassPolymorphism() throws MarshallerException, IOException
   {
      StringWriter writer = new StringWriter();
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()));
      StringReader reader = new StringReader("<Read-Request xmlns=\"" + XML.NS_URI_TNS + "\"><class>Patient</class>"
         + "<attributes>(fullName name)</attributes><where></where><orderBy></orderBy><count>8</count>"
         + "<offset>0</offset></Read-Request>");
      Response response = new Response();
      TransferObject tobj = new TransferObject("Patient");
      TransferObject tobj2 = new TransferObject("Doctor");

      tobj2.setOID(OID.fromBinary(Binary.parse("1000000000000010008000BEEF0000000A")));
      tobj.setValue("doctor", tobj2);
      response.addResult(Arrays.asList(new Object[]{tobj}));
      XMLReadRequest request = (XMLReadRequest)unmarshaller.deserialize(reader);

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMap); // validate to XSD

      Node root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Read-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertEquals(1, root.getChildNodes().item(0).getAttributes().getLength());
      assertEquals("Patient",
                   root
                      .getChildNodes()
                         .item(0)
                            .getAttributes()
                               .getNamedItem(XML.XSI_NS + ":type")
                                  .getNodeValue());
      assertEquals(3, root.getChildNodes().item(0).getChildNodes().getLength());

      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));
      assertNull(XMLUtil
                    .findChildElement(root.getChildNodes().item(0), "doctor")
                       .getAttributes()
                          .getNamedItem(XML.XSI_NS + ":type"));

      writer.getBuffer().setLength(0);
      reader = new StringReader("<Read-Request xmlns=\"" + XML.NS_URI_TNS + "\"><class>Patient</class>"
         + "<attributes>(fullName name)</attributes><where></where><orderBy></orderBy><count>8</count>"
         + "<offset>0</offset></Read-Request>");
      response = new Response();
      tobj = new TransferObject("User");
      tobj2 = new TransferObject("Doctor");

      tobj2.setOID(OID.fromBinary(Binary.parse("1000000000000010008000BEEF0000000A")));
      tobj.setValue("contact", tobj2);
      response.addResult(Arrays.asList(new Object[]{tobj}));
      request = (XMLReadRequest)unmarshaller.deserialize(reader);

      m_marshaller.serialize(request.invoke(new MockServer(response)), writer);
      XMLUtil.parse(new StringReader(writer.toString()), m_xsdMap); // validate to XSD

      root = XMLUtil.parse(new StringReader(writer.toString())).getFirstChild();

      assertEquals(XML.BASE_PREFIX + "Read-Response", root.getNodeName());
      assertEquals(1, root.getChildNodes().getLength());
      assertEquals("item", root.getChildNodes().item(0).getNodeName());
      assertEquals(1, root.getChildNodes().item(0).getAttributes().getLength());
      assertEquals("User",
                   root
                      .getChildNodes()
                         .item(0)
                            .getAttributes()
                               .getNamedItem(XML.XSI_NS + ":type")
                                  .getNodeValue());
      assertEquals(3, root.getChildNodes().item(0).getChildNodes().getLength());

      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "oid"));
      assertNotNull(XMLUtil.findChildElement(root.getChildNodes().item(0),
                                             XML.BASE_PREFIX + "version"));
      assertEquals("Doctor",
                   XMLUtil
                      .findChildElement(root.getChildNodes().item(0), "contact")
                         .getAttributes()
                            .getNamedItem(XML.XSI_NS + ":type")
                               .getNodeValue());
   }

   public void testMarshalValidateMetaclass() throws MarshallerException, IOException
   {
      StringWriter writer = new StringWriter();
      Response response = new Response();
      TransferObject tobj = new TransferObject("Contact"); // valid Metaclass object

      tobj.setValue("firstName", "Name First");
      tobj.setValue("lastName", null);
      tobj.setValue("customKey", "Key Custom");
      response.addResult(tobj); // need to marshal inside a Response since need predefined element

      m_marshaller.serialize(response, writer);

      Document rsp = XMLUtil.parse(new StringReader(writer.toString()), m_xsdMap);//validate to XSD
      Node root = rsp.getFirstChild();

      assertEquals(1, root.getChildNodes().getLength());

      Node result = root.getFirstChild();

      assertEquals("results", result.getNodeName());
      assertEquals(1, result.getAttributes().getLength());
      assertEquals(tobj.getClassName(),
                   result.getAttributes().getNamedItem(XML.XSI_NS + ":type").getNodeValue());
      assertEquals(7, result.getChildNodes().getLength());
      assertEquals(Short.toString(tobj.getVersion()),
                   XMLUtil.findChildElement(result, XML.BASE_PREFIX + "version")
                      .getTextContent());
      assertEquals("@b", // temporary ID not in tobj
                   XMLUtil.findChildElement(result, XML.BASE_PREFIX + "oid")
                      .getTextContent());

      final List/*<Element>*/ list = new ArrayList();
      ElementHandler listFiller = new ElementHandler()
      {
         public void handleElement(Element element)
         {
            list.add(element);
         }
      };

      XMLUtil.forEachChildElement(
         (Element)result, XML.BASE_PREFIX + "keys", listFiller);
      assertEquals("lastName", ((Element)list.get(0)).getTextContent());
      assertEquals("customKey", ((Element)list.get(1)).getTextContent());

      list.clear();
      XMLUtil.forEachChildElement(
         (Element)result, XML.BASE_PREFIX + "values", listFiller);
      assertEquals(1, ((Element)list.get(0)).getAttributes().getLength());
      assertEquals("true", ((Element)list.get(0)).getAttributes()
                           .getNamedItem(XML.XSI_NS + ":nil").getNodeValue());
      assertEquals("", ((Element)list.get(0)).getTextContent());
      assertEquals(1, ((Element)list.get(1)).getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":string", ((Element)list.get(1)).getAttributes()
                                           .getNamedItem(XML.XSI_NS + ":type").getNodeValue());
      assertEquals(tobj.getValue("customKey"), ((Element)list.get(1)).getTextContent());

      assertEquals(tobj.getValue("firstName"),
                   XMLUtil.findChildElement(result, "firstName").getTextContent());
   }
}