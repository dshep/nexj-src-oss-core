// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import nexj.core.integration.IntegrationException;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLNamespace;
import nexj.core.rpc.TransferObject;

/**
 * Parses a SOAP envelope.
 */
public class SOAPEnvelopeAcceptor implements ParseEventAcceptor
{
   // constants

   /**
    * Parsing the envelope.
    */
   protected final static byte STATUS_PARSING_ENVELOPE = 0;

   /**
    * Message headers are being parsed.
    */
   protected final static byte STATUS_PARSING_HEADERS = 1;

   /**
    * Message is being parsed. If parsing a message with headers, this is the
    * status when the body is being parsed.
    */
   protected final static byte STATUS_PARSING_MESSAGE = 2;

   // attributes

   /**
    * The namespace of the currently-parsed envelope.
    */
   protected String m_sEnvelopeNamespace = "";

   /**
    * The envelope state array.
    */
   protected int[] m_stateArray;

   /**
    * The envelope state array top.
    */
   protected int m_nState;

   /**
    * The envelope currently being parsed.
    */
   protected byte m_nEnvelope;

   /**
    * The bit-map of envelopes that will be accepted.
    */
   protected byte m_nAcceptedEnvelopes;

   /**
    * True if a SOAP fault was encountered.
    */
   protected boolean m_bFault;

   // associations

   /**
    * The parser stack.
    */
   protected XMLMessageParserStack m_stack;

   /**
    * The parent parser.
    */
   protected XMLMessageParser m_parser;

   /**
    * Parse status, one of the STATUS_* constants.
    */
   protected byte m_nParsingStatus = STATUS_PARSING_ENVELOPE;

   // constructors

   /**
    * Creates a new SOAP envelope parser.
    * @param parser The XML message parser instance using this parser.
    * @param stack The XML message parser parse stack.
    */
   public SOAPEnvelopeAcceptor(XMLMessageParser parser, XMLMessageParserStack stack)
   {
      m_parser = parser;
      m_stack = stack;
   }

   // operations

   /**
    * @throws SOAPFaultException If a SOAP fault was encountered during envelope parsing.
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#checkFault(nexj.core.rpc.TransferObject)
    */
   public void checkFault(TransferObject root) throws SOAPFaultException
   {
      if (!m_bFault)
      {
         return;
      }

      m_bFault = false;

      if (m_nEnvelope == RootXMLMessagePartMapping.ENVELOPE_SOAP)
      {
         SOAPFaultException ex = new SOAPFaultException();

         ex.setFaultCode((String)root.getValue("code"));
         ex.setFaultString((String)root.getValue("msg"));
         ex.setFaultActor((String)root.findValue("actor"));

         throw ex;
      }

      if (m_nEnvelope == RootXMLMessagePartMapping.ENVELOPE_SOAP12)
      {
         SOAP12FaultException ex = new SOAP12FaultException();
         TransferObject code = (TransferObject)root.getValue("code");

         ex.setFaultCode((String)code.getValue("value"));

         while ((code = (TransferObject)code.findValue("subcode")) != null)
         {
            ex.appendFaultSubcode((String)code.getValue("value"));
         }

         TransferObject reason = (TransferObject)root.getValue("reason");
         List reasonList = (List)reason.getValue("text");

         for (int i = 0; i < reasonList.size(); i++)
         {
            TransferObject text = (TransferObject)reasonList.get(i);

            ex.setReason((String)text.getValue("lang"), (String)text.getValue("value"));
         }

         ex.setNode((String)root.findValue("node"));
         ex.setRole((String)root.findValue("role"));

         throw ex;
      }
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#reset()
    */
   public void reset()
   {
      m_stateArray = null;
      m_nParsingStatus = STATUS_PARSING_ENVELOPE;
   }

   /**
    * Sets the bit mask of accepted SOAP envelopes.
    * @param nEnvelope A combination of the ENVELOPE_* constants on RootXMLMessagePartMapping.
    */
   public void setAcceptedEnvelopes(byte nEnvelope)
   {
      m_nAcceptedEnvelopes = nEnvelope;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptCharacters(char[], int, int)
    */
   public boolean acceptCharacters(char[] cbuf, int nOffset, int nLength) throws SAXException
   {
      return m_nParsingStatus == STATUS_PARSING_ENVELOPE;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptEndElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public boolean acceptEndElement(String sURI, String sLocalName, String sQualName) throws SAXException
   {
      if (m_stack.isEmpty() || m_nParsingStatus == STATUS_PARSING_ENVELOPE)
      {
         m_nParsingStatus = STATUS_PARSING_ENVELOPE;
         endEnvelope();

         return true;
      }

      return false;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptStartElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public boolean acceptStartElement(String sURI, String sLocalName, String sQualName, Attributes attributes) throws SAXException
   {
      // Finish parsing headers
      if (m_nParsingStatus == STATUS_PARSING_HEADERS && sURI.equals(m_sEnvelopeNamespace))
      {
         m_nParsingStatus = STATUS_PARSING_ENVELOPE;
         m_stack.setTopIndex(0);
      }

      if (m_nParsingStatus == STATUS_PARSING_ENVELOPE)
      {
         return startEnvelope(sURI, sLocalName, attributes);
      }

      return false;
   }

   /**
    * Parses an envelope element end.
    */
   protected void endEnvelope()
   {
      switch (m_nState)
      {
         case 0:
            m_stateArray = null;
            break;

         case 1:
            if (m_stateArray[m_nState]-- == 0)
            {
               --m_nState;
            }

            break;
      }
   }

   /**
    * Parses an envelope element start.
    * @param sURI The namespace URI; empty string if no namespace.
    * @param sLocalName The local name (without prefix).
    * @param attributes The attributes attached to the element.
    * @return True if the element is accepted.
    */
   protected boolean startEnvelope(String sURI, String sLocalName, Attributes attributes)
   {
      if (m_stateArray == null)
      {
         byte nDetectedEnvelope = RootXMLMessagePartMapping.ENVELOPE_NONE;

         m_sEnvelopeNamespace = "";

         if (sLocalName.equals("Envelope"))
         {
            if (sURI.equals(XMLNamespace.SOAP))
            {
               nDetectedEnvelope = RootXMLMessagePartMapping.ENVELOPE_SOAP;
               m_sEnvelopeNamespace = XMLNamespace.SOAP;
            }
            else if (sURI.equals(XMLNamespace.SOAP12))
            {
               nDetectedEnvelope = RootXMLMessagePartMapping.ENVELOPE_SOAP12;
               m_sEnvelopeNamespace = XMLNamespace.SOAP12;
            }
         }

         if ((m_nAcceptedEnvelopes & nDetectedEnvelope) != 0)
         {
            if (nDetectedEnvelope == RootXMLMessagePartMapping.ENVELOPE_NONE)
            {
               m_nParsingStatus = STATUS_PARSING_MESSAGE;

               return false;
            }

            m_nEnvelope = nDetectedEnvelope;
            m_stateArray = new int[2];
            m_nState = 0;

            return true;
         }
      }
      else
      {
         switch (m_nState)
         {
            case 0: // Envelope
               if (sURI.equals(m_sEnvelopeNamespace))
               {
                  if (sLocalName.equals("Header"))
                  {
                     if (m_stateArray[m_nState] == 0)
                     {
                        if (m_parser.acceptHeaderElement(sURI, sLocalName))
                        {
                           m_nParsingStatus = STATUS_PARSING_HEADERS;
                        }
                        else
                        {
                           m_stateArray[m_nState] = 1;
                           m_stateArray[++m_nState] = 0;
                        }

                        return true;
                     }
                  }
                  else if (sLocalName.equals("Body"))
                  {
                     if (m_stateArray[m_nState] <= 1)
                     {
                        m_stateArray[m_nState] = 2;
                        m_stateArray[++m_nState] = 0;

                        return true;
                     }
                  }
               }

               break;

            case 1: // Header or Body
               switch (m_stateArray[m_nState - 1])
               {
                  case 1: // Header
                     ++m_stateArray[m_nState];

                     return true;

                  case 2: // Body
                     if (m_stateArray[m_nState]++ != 0)
                     {
                        throw new IntegrationException("err.integration.xml.invalidEnvelope",
                           new Object[]{sURI, sLocalName});
                     }

                     if (sLocalName.equals("Fault") && sURI.equals(m_sEnvelopeNamespace))
                     {
                        // If the message is mapped for the SOAP fault, let the main parser handle it
                        if (m_parser.acceptRootElement(sURI, sLocalName, attributes, false))
                        {
                           m_nParsingStatus = STATUS_PARSING_MESSAGE;

                           return true;
                        }

                        m_parser.changeRoot(m_parser.getInvocationContext().getMetadata().getMessage(
                           (m_nEnvelope == RootXMLMessagePartMapping.ENVELOPE_SOAP12) ? "SOAP12Fault" : "SOAP11Fault"));

                        if (m_parser.acceptRootElement(sURI, sLocalName, attributes, false))
                        {
                           m_nParsingStatus = STATUS_PARSING_MESSAGE;
                           m_bFault = true;

                           return true;
                        }

                        return false;
                     }

                     m_nParsingStatus = STATUS_PARSING_MESSAGE;
                     m_parser.acceptRootElement(sURI, sLocalName, attributes, true);

                     return true;
               }

               break;
         }
      }

      throw new IntegrationException("err.integration.xml.invalidEnvelope",
         new Object[]{sURI, sLocalName});
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#getValue()
    */
   public Object getValue()
   {
      return null;
   }
}