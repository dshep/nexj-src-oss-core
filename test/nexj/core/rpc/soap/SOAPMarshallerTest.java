// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import java.io.IOException;
import java.io.StringReader;

import nexj.core.meta.Repository;
import nexj.core.rpc.CharacterStreamMarshallerTest;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ValidationException;

public class SOAPMarshallerTest extends CharacterStreamMarshallerTest
{
   private SOAPRequest m_soapRequest;
   private SOAPResponse m_soapResponse;
   private SOAPFault m_soapFault;

   public SOAPMarshallerTest(String sName)
   {
      super(sName);
   }

   /**
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_soapRequest = new SOAPRequest(m_request);
      m_soapResponse = new SOAPResponse(m_response);
      m_soapFault = new SOAPFault(m_exception);

      m_marshaller = new SOAPMarshaller(new InvocationContext(Repository.getMetadata()));
      m_unmarshaller = new SOAPUnmarshaller(new InvocationContext(Repository.getMetadata()));
   }
   
   public void testSerializeSOAPRequest() throws IOException
   {
      m_marshaller.serialize(m_soapRequest, m_writer);
      StringReader reader = new StringReader(m_writer.toString());
      SOAPRequest req = (SOAPRequest)m_unmarshaller.deserialize(reader);
      
      assertEquals("10", req.getRequest().getVersion());
   }
   
   public void testSerializeSOAPResponse() throws IOException
   {
      m_marshaller.serialize(m_soapResponse, m_writer);
      StringReader reader = new StringReader(m_writer.toString());
      SOAPResponse resp = (SOAPResponse)m_unmarshaller.deserialize(reader);
      
      assertEquals(2, resp.getResponse().getResultCount());
   }
   
   public void testSerializeSOAPFault() throws IOException
   {
      m_marshaller.serialize(m_soapFault, m_writer);
      StringReader reader = new StringReader(m_writer.toString());
      SOAPFault fault = (SOAPFault)m_unmarshaller.deserialize(reader);

      assertEquals("http://schemas.xmlsoap.org/soap/envelope/", fault.getURI());
      assertEquals("Server.nexj.core.runtime.ValidationException", fault.getCode());
      assertEquals("err.validation.requiredAttributes(\"Contact\")", fault.getMessage());
      assertEquals("err.validation.requiredAttributes", ((ValidationException)fault.getException()).getErrorCode());
   }
}
