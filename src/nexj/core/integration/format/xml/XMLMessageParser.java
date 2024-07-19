// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLNamespace;
import nexj.core.meta.integration.service.Interface;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.LookupDeque;
import nexj.core.util.NoCloseReader;
import nexj.core.util.SOAPUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.Undefined;
import nexj.core.util.UnlimitedMarkReader;
import nexj.core.util.XMLException;
import nexj.core.util.XMLUtil;

/**
 * XML message parser.
 */
public class XMLMessageParser extends DefaultHandler implements MessageParser, InvocationContextAware,
   XMLUtil.IncrementalHandler, ParseEventAcceptor, InvocationContextHolder
{
   // constants

   /**
    * Special composite message part for ignoring the child elements.
    */
   protected final static CompositeMessagePartInstance LAX_PART = new CompositeMessagePartInstance(null);
   protected final static PrimitiveMessagePart ANY_TYPE_PART = new PrimitiveMessagePart(null);

   static
   {
      LAX_PART.setLax(true);
      LAX_PART.makeReadOnly();

      ANY_TYPE_PART.makeReadOnly();
   }

   /**
    * The index (in m_acceptorArray) of the any-type acceptor.
    */
   protected final static int ACCEPTOR_ANY_TYPE = 0;

   /**
    * The index (in m_acceptorArray) of the envelope acceptor.
    */
   protected final static int ACCEPTOR_ENVELOPE = 1;

   /**
    * The index (in m_acceptorArray) of the message acceptor.
    */
   protected final static int ACCEPTOR_MESSAGE = 2;

   // attributes

   /**
    * The local part name return value from getURILocalPart(String).
    */
   protected String m_sLocalName;

   /**
    * Flag for restarting the parsing after schema detection.
    */
   protected boolean m_bRestart;

   /**
    * True if the most-recently started element is nil.
    */
   protected boolean m_bNilElement;

   /**
    * True to process the root element.
    */
   protected boolean m_bProcessRoot;

   // associations

   /**
    * The message map for message identification.
    */
   protected Lookup2D m_messageMap;

   /**
    * The parse stack.
    */
   protected XMLMessageParserStack m_stack = new XMLMessageParserStack();

   /**
    * Objects that handle the parse events.
    */
   protected ParseEventAcceptor[] m_acceptorArray = new ParseEventAcceptor[3];

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The header part.
    */
   protected MessagePart m_headerPart;

   /**
    * The base message root part, before xsi:type was processed.
    */
   protected MessagePart m_baseRootPart;

   /**
    * The map of namespace names to URIs, updated before every SAX event.
    */
   protected Lookup m_namespaceNameURIMap = new HashTab(4);

   /**
    * The primitive value formatter.
    */
   protected PrimitiveFormatter m_primitiveFormatter;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(XMLMessageParser.class);

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.runtime.InvocationContextHolder#getInvocationContext()
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.Message)
    */
   public TransferObject parse(Input in, Message message) throws IntegrationException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Parsing XML message \"" + message.getName() + "\"");
         s_logger.dump(in);
      }

      RootXMLMessagePartMapping mapping = (RootXMLMessagePartMapping)message.getRoot().getMapping();
      boolean bRestartable = (message.getDerivation() != Message.DERIVATION_FINAL && message.getDerivedMessageCount() > 0) || // is polymorphic?
         ((mapping.getEnvelope() & RootXMLMessagePartMapping.ENVELOPE_SOAP12) != 0);  // SOAP 1.2 requires restartable parse (Fault headers)

      m_messageMap = null;
      m_headerPart = (bRestartable) ? null : mapping.getHeaderPart();

      return parse(in, message.getRoot(), bRestartable, mapping.getEnvelope(), mapping.getSchemaResourceMap());
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Identifying and parsing an XML message");
         s_logger.dump(in);
      }

      XMLMessageParserTable parserTable = (XMLMessageParserTable)table.getParserTable();

      m_messageMap = parserTable.getMessageMap();
      m_headerPart = null;

      return parse(in, null, parserTable.isRestartable(), parserTable.getAcceptedEnvelopes(), null);
   }

   /**
    * Parses a message.
    * 
    * @param in The message input.
    * @param rootPart The root message part; may be null (if the message to parse is unknown and
    * needs to be determined by reading some of the input).
    * @param bRestartable True to perform a restartable parse. Set true when parse may need to be
    * restarted, for example when a polymorphic message is parsed, when a message table is parsed
    * and schemas are used, or when a SOAP envelope is parsed and it has message headers defined.
    * @param nEnvelope A bit mask of accepted SOAP envelopes; use the ENVELOPE_* constants
    * from RootXMLMessagePartMapping.
    * @param schemaResourceMap The schemas to use.
    * @return The parsed message.
    */
   protected TransferObject parse(Input in, CompositeMessagePart rootPart, boolean bRestartable, byte nEnvelope, LookupDeque schemaResourceMap)
   {
      try
      {
         TransferObject tobj = new TransferObject();
         Reader reader = in.getReader();

         if (bRestartable)
         {
            reader = new UnlimitedMarkReader(reader);
         }

         reader = new NoCloseReader(reader);
         initParse(rootPart, tobj, nEnvelope);

         if (bRestartable)
         {
            reader.mark(1024);
         }

         XMLUtil.parse(reader, this, schemaResourceMap);

         if (m_bRestart)
         {
            assert bRestartable;

            reader.reset();
            resetAcceptors();
            XMLUtil.parse(reader, this, ((RootXMLMessagePartMapping)
               m_stack.getMessagePart(0).getMapping()).getSchemaResourceMap());
         }

         assert !m_bRestart;

         for (int i = 0; i < m_acceptorArray.length; i++)
         {
            if (m_acceptorArray[i] != null)
            {
               m_acceptorArray[i].checkFault(tobj);
            }
         }

         assert !StringUtil.isEmpty(tobj.getClassName());

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("Parse result:");
            s_logger.dump(tobj);
         }

         return tobj;
      }
      catch (SOAPFaultException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         if (e instanceof XMLException && e.getCause() instanceof IntegrationException)
         {
            throw (IntegrationException)e.getCause();
         }

         if (e instanceof IntegrationException &&
            ((IntegrationException)e).getErrorCode().equals("err.integration.xml.unsupportedMessage"))
         {
            throw (IntegrationException)e;
         }

         if (m_stack.getMessagePart(0) == null)
         {
            throw new IntegrationException("err.integration.xml.unparseableMessage", e);
         }

         throw new IntegrationException("err.integration.xml.messageSyntax",
            new Object[]{m_stack.getMessagePart(0).getName()}, e);
      }
   }

   /**
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
      XMLMessageParserTable parserTable = new XMLMessageParserTable();
      Lookup2D map = parserTable.getMessageMap();
      boolean bRestartable = false;
      byte nAcceptedEnvelopes = 0;

      for (Iterator itr = table.getMessageIterator(); itr.hasNext();)
      {
         Message message = (Message)itr.next();
         RootXMLMessagePartMapping mapping = (RootXMLMessagePartMapping)message.getRoot().getMapping();
         Message old = (Message)map.put((mapping.getNamespace() != null) ? mapping.getNamespace().getURI() : "",
            mapping.getNodeName(), message);

         if (old != null)
         {
            if (old.isUpcast(message) && old.getDerivation() != Message.DERIVATION_FINAL)
            {
               // Re-put old
               map.put((mapping.getNamespace() != null) ? mapping.getNamespace().getURI() : "",
                  mapping.getNodeName(), old);
            }
            else if (message.isUpcast(old) && message.getDerivation() != Message.DERIVATION_FINAL)
            {
               // base message "message" is already in table
            }
            else
            {
               throw new MetadataException("err.meta.integration.xml.dupMessageElement",
                  new Object[]{mapping.getNodeName(), old.getName(), message.getName()});
            }
         }

         nAcceptedEnvelopes |= mapping.getEnvelope();

         if (mapping.getSchemaResourceMap() != null || mapping.getHeaderPart() != null)
         {
            bRestartable = true;
         }
      }

      if ((nAcceptedEnvelopes & RootXMLMessagePartMapping.ENVELOPE_SOAP12) != 0)
      {
         bRestartable = true;
      }

      parserTable.setAcceptedEnvelopes(nAcceptedEnvelopes);
      parserTable.setRestartable(bRestartable);
      table.setParserTable(parserTable);
   }

   /**
    * @see nexj.core.util.XMLUtil.IncrementalHandler#isComplete()
    */
   public boolean isComplete()
   {
      return m_bRestart;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptStartElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public boolean acceptStartElement(String sURI, String sLocalName, String sQualName, Attributes attributes) throws SAXException
   {
      if (m_bProcessRoot)
      {
         acceptRootElement(sURI, sLocalName, attributes, true);
         m_bProcessRoot = false;

         return true;
      }

      if (m_stack.isEmpty())
      {
         throw new IntegrationException("err.integration.xml.unexpectedElement",
               new Object[] {sURI, sLocalName});
      }

      MessagePart parentPart = m_stack.getTopMessagePart();

      if (m_bNilElement)
      {
         throw new IntegrationException("err.integration.xml.nilElementNotEmpty",
            new Object[] {parentPart.getFullPath()});
      }

      if (parentPart instanceof CompositeMessagePart)
      {
         CompositeMessagePart parentComposite = (CompositeMessagePart)parentPart;

         for (int nIndex = (parentComposite.getAggregation() == CompositeMessagePart.SEQUENTIAL) ? m_stack.getTopIndex() : 0;
            nIndex < parentComposite.getPartCount(); ++nIndex)
         {
            MessagePart part = parentComposite.getPart(nIndex);
            XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

            if (mapping.getNodeType() == XMLMessagePartMapping.ELEMENT)
            {
               if (matches(part, sURI, sLocalName))
               {
                  TransferObject tobj;
                  MessagePart basePart = part;

                  m_bNilElement = mapping.isNillable() &&
                      (Primitive.toBoolean(attributes.getValue(XMLNamespace.XSI, "nil")) == Boolean.TRUE);

                  if (part instanceof PrimitiveMessagePart)
                  {
                     tobj = null;

                     if (((PrimitiveMessagePart)part).getType() == Primitive.ANY)
                     {
                        if (mapping.getInterface() == null)
                        {
                           startParsingAnyType((mapping.getSubtype() == XMLMessagePartMapping.SUBTYPE_XSI) ?
                              attributes.getValue(XMLNamespace.XSI, "type") : null);
                        }
                     }
                  }
                  else
                  {
                     CompositeMessagePart composite2 = (CompositeMessagePart)part;
                     PrimitiveMessagePart valuePart = mapping.getValuePart();

                     tobj = new TransferObject(composite2.getPartCount());

                     // Process message inheritance
                     if (composite2 instanceof CompositeMessagePartRef)
                     {
                        CompositeMessagePartRef ref = (CompositeMessagePartRef)composite2;
                        CompositeMessagePart referencedPart = ref.getRefPart();

                        tobj.setClassName(referencedPart.getName());

                        RootXMLMessagePartMapping refRootMapping = (RootXMLMessagePartMapping)referencedPart.getMapping();
                        Message derivedMessage = getDerivedMessage(attributes, refRootMapping, part.getFullPath());

                        if (derivedMessage != null)
                        {
                           Message referencedMessage = refRootMapping.getRootMessage();

                           Message.validatePolymorphism(referencedMessage, derivedMessage, part);
                           tobj.setClassName(derivedMessage.getName());
                           part = derivedMessage.getRoot();
                           valuePart = ((XMLMessagePartMapping)part.getMapping()).getValuePart();
                        }
                     }

                     parseAttributes(tobj, (CompositeMessagePart)part, attributes);

                     if (valuePart != null && valuePart.getType() == Primitive.ANY)
                     {
                        XMLMessagePartMapping valuePartMapping = (XMLMessagePartMapping)valuePart.getMapping();

                        if (valuePartMapping.getInterface() == null)
                        {
                           startParsingAnyType(null);  // value part cannot parse to a primitive (attributes not allowed if primitive)
                        }
                     }
                  }

                  m_stack.setTopIndex(nIndex);
                  m_stack.push(part, basePart, tobj, 0, null);

                  return true;
               }
            }
            else if (mapping.getNodeType() == XMLMessagePartMapping.VALUE)
            {
               if (acceptAnyTypeElement(sURI, sLocalName, attributes, (PrimitiveMessagePart)part, nIndex, nIndex >= parentComposite.getPartCount() - 1))
               {
                  return true;
               }
            }
         }

         if (parentComposite.isLax())
         {
            s_logger.debug("Ignoring the element");

            m_stack.push(LAX_PART, null, null, 0, null);

            return true;
         }
      }
      else if (parentPart instanceof PrimitiveMessagePart)
      {
         if (acceptAnyTypeElement(sURI, sLocalName, attributes, (PrimitiveMessagePart)parentPart, 0, true))
         {
            return true;
         }
      }

      throw new IntegrationException("err.integration.xml.invalidElement",
         new Object[]{sURI, sLocalName, parentPart.getFullPath()});
   }

   /**
    * Accepts an envelope header element start tag from an XML document.
    * 
    * @param sURI The namespace URI; empty string if no namespace.
    * @param sLocalName The local name (without prefix).
    * @return True if the header is accepted; false if there is no header.
    */
   public boolean acceptHeaderElement(String sURI, String sLocalName)
   {
      if (m_headerPart == null)
      {
         return false;
      }

      // No need to check element NS:name against header part--it is already correct (enforced by envelope processor)
      m_stack.push(m_headerPart, m_headerPart, new TransferObject(), 0, null);
      m_bProcessRoot = false;

      return true;
   }

   /**
    * Changes the message being parsed to the given message.
    * @param message The new message to parse.
    */
   public void changeRoot(Message message)
   {
      m_stack.setTopMessagePart(message.getRoot());
      m_stack.setTopBaseMessagePart(message.getRoot());
      m_baseRootPart = message.getRoot();
   }

   /**
    * Accepts the root message element start tag from an XML document.
    * 
    * @param sURI The namespace URI; empty string if no namespace.
    * @param sLocalName The local name (without prefix).
    * @param attributes The attributes attached to the element.
    * @param bError True to throw an error if the document root is accepted; false to
    * return false.
    * @return True if the document root is accepted; false if it is not accepted.
    */
   public boolean acceptRootElement(String sURI, String sLocalName, Attributes attributes, boolean bError)
   {
      MessagePart part = m_stack.getTopMessagePart();
      RootXMLMessagePartMapping rootMapping;

      if (part == null)
      {
         // Identify the XML message being parsed
         Message msg = (Message)m_messageMap.get(sURI, sLocalName);

         if (msg != null)
         {
            part = msg.getRoot();
         }

         if (part == null)
         {
            if (bError)
            {
               throw new IntegrationException("err.integration.xml.unsupportedMessage",
                  new Object[]{sURI, sLocalName});
            }

            return false;
         }

         rootMapping = (RootXMLMessagePartMapping)part.getMapping();
         assert m_stack.getSize() == 1;
         m_stack.setTopMessagePart(part);
         m_stack.setTopBaseMessagePart(part);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Identified the XML message as \"" + part.getName() + "\"");
         }

         ((TransferObject)m_stack.getTopObject()).setClassName(part.getName());
         m_headerPart = rootMapping.getHeaderPart();
         m_bRestart = (rootMapping.getSchemaResourceMap() != null) || (m_headerPart != null);
         m_baseRootPart = part;
      }
      else
      {
         // Verify root element of message being parsed
         rootMapping = (RootXMLMessagePartMapping)part.getMapping();

         if (!matches(m_baseRootPart, sURI, sLocalName))
         {
            if (bError)
            {
               throw new IntegrationException("err.integration.xml.invalidDocRoot",
                  new Object[]{sURI, sLocalName});
            }

            return false;
         }

         if (m_headerPart != rootMapping.getHeaderPart())
         {
            m_headerPart = rootMapping.getHeaderPart();
            m_bRestart = true;
         }
      }

      TransferObject tobj = (TransferObject)m_stack.getTopObject();

      if (!m_bRestart)
      {
         parseAttributes(tobj, (CompositeMessagePart)part, attributes);
      }

      Message derivedMessage = getDerivedMessage(attributes, rootMapping, part.getFullPath());
      Message primaryMessage = rootMapping.getRootMessage();

      tobj.setClassName(primaryMessage.getName());

      if (derivedMessage != null)
      {
         CompositeMessagePart derivedMessageRoot = derivedMessage.getRoot();
         RootXMLMessagePartMapping derivedMessageRootMapping = (RootXMLMessagePartMapping)derivedMessageRoot.getMapping();

         Message.validatePolymorphism(primaryMessage, derivedMessage, derivedMessageRoot);
         m_stack.setTopBaseMessagePart(part);
         part = derivedMessageRoot;
         m_stack.setTopMessagePart(part);
         tobj.setClassName(derivedMessage.getName());

         MessagePart headerPart = derivedMessageRootMapping.getHeaderPart();

         if (m_headerPart != headerPart)
         {
            m_headerPart = headerPart;
            m_bRestart = true;
         }
      }
      else
      {
         if (primaryMessage.getDerivation() == Message.DERIVATION_ABSTRACT)
         {
            throw new IntegrationException("err.integration.abstractMessage",
               new Object[]{primaryMessage.getName(), part.getFullPath()});
         }
      }

      m_bProcessRoot = false;

      return true;
   }

   /**
    * Accepts an element start tag for processing as an any-type with an interface. If
    * accepted, the element start tag is treated as the beginning of a new message.
    * Looks up the message to parse in the interface on the XML message part mapping
    * of the supplied message part.
    * 
    * @param sURI The namespace URI; empty string if no namespace.
    * @param sLocalName The local name (without prefix).
    * @param attributes The attributes attached to the element.
    * @param anyPart The part to parse.
    * @param nIndex The index to set the current top-of-stack index if the element
    * is accepted.
    * @param bError True to throw an error if the element cannot be accepted; otherwise false is returned.
    * @return True if the element is accepted; false otherwise.
    */
   protected boolean acceptAnyTypeElement(String sURI, String sLocalName, Attributes attributes,
      PrimitiveMessagePart anyPart, int nIndex, boolean bError)
   {
      if (anyPart.getType() == Primitive.ANY)
      {
         Interface iface = ((XMLMessagePartMapping)anyPart.getMapping()).getInterface();
         XMLMessageParserTable table = (XMLMessageParserTable)iface.getRequestTable().getParserTable();
         Message msg = (Message)table.getMessageMap().get(sURI, sLocalName);
         MessagePart part = null;

         if (msg == null)
         {
            table = (XMLMessageParserTable)iface.getResponseTable().getParserTable();
            msg = (Message)table.getMessageMap().get(sURI, sLocalName);
         }

         if (msg == null && bError)
         {
            throw new IntegrationException("err.integration.xml.interfaceElementLookup",
               new Object[]{sURI, sLocalName, iface.getName()});
         }

         if (msg != null)
         {
            part = msg.getRoot();
         }

         if (part == null)
         {
            return false;
         }

         Message derivedMessage = getDerivedMessage(attributes, (RootXMLMessagePartMapping)part.getMapping(), null);
         MessagePart basePart = part;

         if (derivedMessage != null)
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Identified the XML anyType message as \"" + msg.getName() +
                  "\", using derived message \"" + derivedMessage.getName() + "\"");
            }

            msg = derivedMessage;
            part = derivedMessage.getRoot();
         }
         else
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Identified the XML anyType message as \"" + msg.getName() + "\"");
            }
         }

         CompositeMessagePart composite = (CompositeMessagePart)part;
         TransferObject tobj = new TransferObject(msg.getName(), composite.getPartCount());

         parseAttributes(tobj, composite, attributes);
         m_stack.setTopIndex(nIndex);
         m_stack.push(part, basePart, tobj, 0, null);

         return true;
      }

      return false;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptEndElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public boolean acceptEndElement(String sURI, String sLocalName, String sQualName) throws SAXException
   {
      MessagePart part = m_stack.getTopMessagePart();
      MessagePart basePart = m_stack.getTopBaseMessagePart();
      Object obj;

      assert basePart == null || basePart.getName() != null;

      if (part instanceof PrimitiveMessagePart)
      {
         PrimitiveMessagePart primitivePart = (PrimitiveMessagePart)part;

         if (primitivePart.getType() == Primitive.ANY)
         {
            if ((obj = getAnyTypeValue()) == null)
            {
               // An element-mapped part with an interface
               obj = m_stack.getTopObject();
            }
         }
         else
         {
            String sValue;

            if (m_bNilElement)
            {
               sValue = null;
            }
            else
            {
               obj = m_stack.getTopObject();
               sValue = (obj == null) ? null : obj.toString();
            }

            obj = convert(primitivePart, sValue);
         }
      }
      else if (part == LAX_PART)
      {
         obj = Undefined.VALUE;
      }
      else
      {
         CompositeMessagePart composite = (CompositeMessagePart)part;
         TransferObject tobj = (TransferObject)m_stack.getTopObject();
         int nPartCount = composite.getPartCount();
         boolean bMatch = false;
         boolean bRequired = false;

         if (tobj != null)
         {
            for (int nIndex = 0; nIndex < nPartCount; ++nIndex)
            {
               MessagePart child = composite.getPart(nIndex);
               int nNodeType = ((XMLMessagePartMapping)child.getMapping()).getNodeType();

               if (!m_bNilElement && nNodeType == XMLMessagePartMapping.ELEMENT)  // ignore counts for nil elements
               {
                  if (composite.getAggregation() == CompositeMessagePart.SINGLE)
                  {
                     if (tobj.hasValue(child.getName()))
                     {
                        if (bMatch)
                        {
                           throw new IntegrationException("err.integration.multipleParts",
                              new Object[]{composite.getFullPath()});
                        }

                        validatePartCount(child);
                        bMatch = true;
                     }

                     bRequired |= child.isRequired();
                  }
                  else
                  {
                     validatePartCount(child);
                  }
               }
               else if (nNodeType == XMLMessagePartMapping.VALUE)
               {
                  Object value = null;

                  if ((value = getAnyTypeValue()) != null)
                  {
                     // Value-mapped part for literal XML will have just one element when collection
                     if (child.isCollection())
                     {
                        ArrayList list = new ArrayList(1);

                        list.add(value);
                        value = list;
                     }
                  }
                  else
                  {
                     value = m_stack.getTopValue();

                     if (value instanceof StringBuilder)
                     {
                        value = value.toString();
                     }

                     if (value instanceof String)
                     {
                        value = convert((PrimitiveMessagePart)child, (String)value);
                     }
                  }

                  tobj.setValue(child.getName(), value);
                  validatePartCount(child);
               }
            }

            if (bRequired && !bMatch)
            {
               throw new IntegrationException("err.integration.missingPart",
                  new Object[]{composite.getFullPath()});
            }
         }

         if (!m_bNilElement || tobj.getValueCount() > 0)
         {
            obj = tobj;
         }
         else
         {
            obj = null;
         }
      }

      addResultToParent(basePart, obj);

      return true;
   }

   /**
    * Pops the current item off the stack and adds the result to the new item on top
    * of the stack.
    * 
    * @param childPart The part whose result is being added.
    * @param result The result to add.
    */
   protected void addResultToParent(MessagePart childPart, Object result)
   {
      m_bNilElement = false;
      m_stack.pop();

      if (!m_stack.isEmpty() && result != Undefined.VALUE)
      {
         TransferObject tobj = (TransferObject)m_stack.getTopObject();
         MessagePart parentPart = m_stack.getTopMessagePart();

         /* If parent is a referenced message part: should use mapping from referrer,
          * but value part must be taken from referenced message part mapping.
          */
         PrimitiveMessagePart valuePart = ((XMLMessagePartMapping)parentPart.getMapping()).getValuePart();

         if (valuePart != null && childPart.getParent() == null)
         {
            // Handle type=any, value-mapped, with interface.
            // 
            // ChildPart has not parent -> root part -> parentPart is any/interface
            // Value Parts are never composite, so cannot be a reference.
            if (valuePart.isCollection())
            {
               List list = (List)m_stack.getTopValue();

               if (list == null)
               {
                  list = new ArrayList();
                  m_stack.setTopValue(list);
               }

               list.add(result);
            }
            else
            {
               m_stack.setTopValue(result);
               m_stack.setTopIndex(m_stack.getTopIndex() + 1);
            }
         }
         else if (parentPart instanceof PrimitiveMessagePart)
         {
            // Handle type=any with interface, when mapped on an element.
            m_stack.setTopObject(result);
         }
         else
         {
            String sChildPartName = childPart.getName();
            
            if (childPart.isCollection())
            {
               List list = (List)tobj.findValue(sChildPartName);

               if (list == null)
               {
                  list = new ArrayList();
                  tobj.setValue(sChildPartName, list);
               }

               list.add(result);
            }
            else
            {
               if (((CompositeMessagePart)parentPart).getAggregation() != CompositeMessagePart.SEQUENTIAL)
               {
                  if (tobj.hasValue(sChildPartName))
                  {
                     throw new IntegrationException("err.integration.maxPartCount", new Object[]{childPart.getFullPath()});
                  }
               }

               tobj.setValue(sChildPartName, result);
               m_stack.setTopIndex(m_stack.getTopIndex() + 1);
            }
         }
      }
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptCharacters(char[], int, int)
    */
   public boolean acceptCharacters(char[] cbuf, int nOffset, int nLength) throws SAXException
   {
      if (m_stack.isEmpty())
      {
         return false;
      }

      MessagePart part = m_stack.getTopMessagePart();

      if (m_bNilElement)
      {
         throw new IntegrationException("err.integration.xml.nilElementNotEmpty",
            new Object[] {part.getFullPath()});
      }

      // Do not process contents of a lax part.
      if (part == LAX_PART)
      {
         return true;
      }

      /*
       * Process character data if the part type is not "any". Otherwise, reject the data.
       * e.g. Reject this: <anyTypePartWithInterface>A<x>...</x>B</anyTypePartWithInterface> (because of A and B)
       */
      boolean bVerifyWhitespace = false;

      if (part instanceof CompositeMessagePart)
      {
         PrimitiveMessagePart valuePart = ((XMLMessagePartMapping)part.getMapping()).getValuePart();

         if (valuePart == null || valuePart.getType() == Primitive.ANY)
         {
            if (!((CompositeMessagePart)part).isLax())
            {
               bVerifyWhitespace = true;
            }
         }
         else
         {
            m_stack.setTopValue(concat(m_stack.getTopValue(), cbuf, nOffset, nLength));
         }
      }
      else
      {
         if (((PrimitiveMessagePart)part).getType() == Primitive.ANY)
         {
            bVerifyWhitespace = true;
         }
         else
         {
            m_stack.setTopObject(concat(m_stack.getTopObject(), cbuf, nOffset, nLength));
         }
      }

      if (bVerifyWhitespace)
      {
         while (nLength-- != 0)
         {
            if (!Character.isWhitespace(cbuf[nOffset++]))
            {
               throw new IntegrationException("err.integration.xml.misplacedCharacterData",
                  new Object[]{part.getFullPath()});
            }
         }
      }

      return true;
   }

   /**
    * Concatenates characters from an array on to the end of a prefix.
    * 
    * @param prefix The string prefix. May be null, a String, or a StringBuilder.
    * @param cbuf The characters to append.
    * @param nOffset The character data start position in the array.
    * @param nLength The number of characters to append from the array.
    * @return A String or StringBuffer with the concatenated result. 
    */
   protected static Object concat(Object prefix, char[] cbuf, int nOffset, int nLength)
   {
      if (prefix == null)
      {
         prefix = new String(cbuf, nOffset, nLength);
      }
      else if (prefix instanceof StringBuilder)
      {
         ((StringBuilder)prefix).append(cbuf, nOffset, nLength);
      }
      else
      {
         String sValue = (String)prefix;
         StringBuilder buf = new StringBuilder(sValue.length() + nLength);

         buf.append(sValue);
         buf.append(cbuf, nOffset, nLength);
         prefix = buf;
      }

      return prefix;
   }

   /**
    * Tests the node for a name match.
    * 
    * @param part The message part to match against.
    * @param sURI The namespace URI.
    * @param sName The node name.
    * @return True if the name and the URI match; false otherwise.
    */
   protected static boolean matches(MessagePart part, String sURI, String sName)
   {
      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

      if (!sName.equals(mapping.getNodeName()))
      {
         return false;
      }

      return ((mapping.getNamespace() != null) ? mapping.getNamespace().getURI() : "").equals(sURI);
   }

   /**
    * Parses element attributes and populates a transfer object with them.
    * @param tobj The destination transfer object.
    * @param composite The composite message part containing the attribute parts.
    * @param attributes The attribute map.
    */
   protected void parseAttributes(TransferObject tobj, CompositeMessagePart composite, Attributes attributes)
   {
      int nPartCount = composite.getPartCount();

      for (int i = 0; i < nPartCount; ++i)
      {
         MessagePart part = composite.getPart(i);
         XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

         if (mapping.getNodeType() == XMLMessagePartMapping.ATTRIBUTE)
         {
            String sValue = attributes.getValue((mapping.getNamespace() != null) ? mapping.getNamespace().getURI() : "", mapping.getNodeName());

            if (part.isRequired() && StringUtil.isEmpty(sValue))
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
            }

            if (sValue != null)
            {
               if (sValue.length() == 0)
               {
                  tobj.setValue(part.getName(), null);
               }
               else
               {
                  tobj.setValue(part.getName(), convert((PrimitiveMessagePart)part, sValue));
               }
            }
         }
      }
   }

   /**
    * Validates the part count in the message on the stack top.
    * @param part The message part metadata.
    */
   protected void validatePartCount(MessagePart part)
   {
      if (part.isRequired() || part.isCollection())
      {
         Object value = ((TransferObject)m_stack.getTopObject()).findValue(part.getName());
         int nCount;

         if (value == null)
         {
            nCount = 0;
         }
         else if (part.isCollection())
         {
            nCount = ((List)value).size();
         }
         else
         {
            nCount = 1;
         }

         if (nCount < part.getMinCount())
         {
            throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
         }

         if (nCount > part.getMaxCount())
         {
            throw new IntegrationException("err.integration.maxPartCount", new Object[]{part.getFullPath()});
         }
      }
   }

   /**
    * Converts a string to a primitive type.
    * @param part The message part.
    * @param sValue The value to convert.
    * @return The converted value.
    */
   protected Object convert(PrimitiveMessagePart part, String sValue) throws IntegrationException
   {
      Object value;

      if (sValue == null)
      {
         value = null;
      }
      else
      {
         XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

         if (mapping.getFormat() != null)
         {
            if (m_primitiveFormatter == null)
            {
               m_primitiveFormatter = new PrimitiveFormatter(m_context);
            }

            return m_primitiveFormatter.parse(sValue, part);
         }

         switch (mapping.getSubtype())
         {
            case XMLMessagePartMapping.SUBTYPE_DATE:
               try
               {
                  value = SOAPUtil.parseDateTime(sValue, true, false, m_context.getTimeZone());
               }
               catch (Exception e)
               {
                  throw new IntegrationException("err.integration.xml.dateTime",
                     new Object[]{sValue, part.getFullPath()}, e);
               }

               break;

            case XMLMessagePartMapping.SUBTYPE_TIME:
               try
               {
                  value = SOAPUtil.parseDateTime(sValue, false, true, m_context.getTimeZone());
               }
               catch (Exception e)
               {
                  throw new IntegrationException("err.integration.xml.dateTime",
                     new Object[]{sValue, part.getFullPath()}, e);
               }

               break;

            case XMLMessagePartMapping.SUBTYPE_DATETIME:
               try
               {
                  value = SOAPUtil.parseDateTime(sValue, true, true, m_context.getTimeZone());
               }
               catch (Exception e)
               {
                  throw new IntegrationException("err.integration.xml.dateTime",
                     new Object[]{sValue, part.getFullPath()}, e);
               }

               break;

            case XMLMessagePartMapping.SUBTYPE_BASE64:
               try
               {
                  value = Binary.fromBase64(sValue);
               }
               catch (Exception e)
               {
                  throw new IntegrationException("err.integration.xml.base64",
                     new Object[]{sValue, part.getFullPath()}, e);
               }

               break;

            default:
               value = part.getType().getConverter(Primitive.STRING).invoke(sValue);
               break;
         }
      }

      return part.validateValue(value);
   }

   /**
    * Starts parsing of an any-type part.
    * @param sType The name of the primitive type to parse; null to parse literal XML.
    */
   public void startParsingAnyType(String sType)
   {
      if (sType != null)
      {
         String sURI = getURILocalPart(sType);

         if (XMLNamespace.XSD.equals(sURI))
         {
            sType = m_sLocalName;
         }
      }

      m_acceptorArray[ACCEPTOR_ANY_TYPE] = new AnyTypeAcceptor(sType);
   }

   /**
    * Gets the result of parsing of an any-type part.
    * @return The any-type value.
    */
   public Object getAnyTypeValue()
   {
      Object value = null;

      if (m_acceptorArray[ACCEPTOR_ANY_TYPE] != null)
      {
         value = m_acceptorArray[ACCEPTOR_ANY_TYPE].getValue();
         m_acceptorArray[ACCEPTOR_ANY_TYPE] = null;
      }

      return value;
   }

   /**
    * Gets the derived message indicated by the xsi:type attribute.
    * 
    * @param attributes Attributes holder for xsi:type lookup.
    * @param mapping Mapping for type to message conversion.
    * @param sLocation The part location to use in an error message if no derived message can
    * be found for the given xsi:type. Null to suppress error and return null.
    * @return The derived message indicated by the xsi:type attribute; null if no xsi:type
    * attribute or no derived message found.
    * @throws IntegrationException If sLocation is not null and there is an xsi:type
    * attribute but no derived message corresponding to that type can be found in the mapping.
    */
   public Message getDerivedMessage(Attributes attributes, RootXMLMessagePartMapping mapping, String sLocation)
   {
      String sType = attributes.getValue(XMLUtil.XSI_NAMESPACE, "type");
      Message message = mapping.getRootMessage();

      if (sType != null && (message.getBaseMessage() != null || message.getDerivedMessageCount() != 0))
      {
         String sURI = getURILocalPart(sType);
         Message derivedMessage = mapping.getXSDTypeMessage(sURI, m_sLocalName);

         if (sLocation != null && derivedMessage == null)
         {
            if (message.getDerivation() == Message.DERIVATION_FINAL)
            {
               return null;
            }
            else
            {
               throw new IntegrationException("err.integration.xml.unknownType",
                  new Object[]{sType, sLocation});
            }
         }

         return derivedMessage;
      }

      return null;
   }

   /**
    * Gets the URI and LocalPart of a QName.
    * @param sQName The QName.
    * @return The URI component. m_sLocalName holds the LocalPart component.
    */
   protected String getURILocalPart(String sQName)
   {
      // Convert namespace alias to URI
      int nSep = sQName.indexOf(':');
      String sPrefix = "";  // the no-prefix prefix

      m_sLocalName = sQName;

      if (nSep >= 0)
      {
         sPrefix = sQName.substring(0, nSep);
         m_sLocalName = sQName.substring(nSep + 1);
      }

      return (String)m_namespaceNameURIMap.get(sPrefix);
   }

   /**
    * Ensures that validation errors get thrown.
    * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
    */
   public void error(SAXParseException e) throws SAXException
   {
      throw e;
   }

   /**
    * @see org.xml.sax.ContentHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public void startElement(String sURI, String sLocalName, String sQualName, Attributes attributes) throws SAXException
   {
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Start element uri=" + sURI + ", name=" + sLocalName + ", qname=" + sQualName);
      }

      for (int i = 0; i < m_acceptorArray.length; i++)
      {
         if (m_acceptorArray[i] != null && m_acceptorArray[i].acceptStartElement(sURI, sLocalName, sQualName, attributes))
         {
            break;
         }
      }
   }

   /**
    * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public void endElement(String sURI, String sLocalName, String sQualName) throws SAXException
   {
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("End element uri=" + sURI + ", name=" + sLocalName + ", qname=" + sQualName);
      }

      if (isComplete())
      {
         return;
      }

      for (int i = 0; i < m_acceptorArray.length; i++)
      {
         if (m_acceptorArray[i] != null && m_acceptorArray[i].acceptEndElement(sURI, sLocalName, sQualName))
         {
            break;
         }
      }
   }

   /**
    * @see org.xml.sax.ContentHandler#characters(char[], int, int)
    */
   public void characters(char[] cbuf, int nOffset, int nLength) throws SAXException
   {
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Characters: \"" + new String(cbuf, nOffset, nLength) + "\"");
      }

      for (int i = 0; i < m_acceptorArray.length; i++)
      {
         if (m_acceptorArray[i] != null && m_acceptorArray[i].acceptCharacters(cbuf, nOffset, nLength))
         {
            break;
         }
      }
   }

   /**
    * Maintains the map of namespace names to namespace URIs.
    * @see org.xml.sax.helpers.DefaultHandler#endPrefixMapping(java.lang.String)
    */
   public void endPrefixMapping(String sPrefix) throws SAXException
   {
      m_namespaceNameURIMap.remove(sPrefix);
   }

   /**
    * Maintains the map of namespace names to namespace URIs.
    * @see org.xml.sax.helpers.DefaultHandler#startPrefixMapping(java.lang.String, java.lang.String)
    */
   public void startPrefixMapping(String sPrefix, String sURI) throws SAXException
   {
      m_namespaceNameURIMap.put(sPrefix, sURI);
   }

   /**
    * Initialises the parser.
    * 
    * @param part The root message part, if any.
    * @param tobj The transfer object that is the parse result.
    * @param nEnvelope A bit mask of accepted message envelopes; use the ENVELOPE_* constants
    * on RootXMLMessagePartMapping.
    */
   protected void initParse(CompositeMessagePart part, TransferObject tobj, byte nEnvelope)
   {
      m_stack.clear();
      m_stack.push(part, part, tobj, 0, null);
      m_baseRootPart = part;

      if (nEnvelope != RootXMLMessagePartMapping.ENVELOPE_NONE)
      {
         SOAPEnvelopeAcceptor envelopeParser = new SOAPEnvelopeAcceptor(this, m_stack);

         envelopeParser.setAcceptedEnvelopes(nEnvelope);
         m_acceptorArray[ACCEPTOR_ENVELOPE] = envelopeParser;
      }
      else
      {
         m_acceptorArray[ACCEPTOR_ENVELOPE] = null;
      }

      m_acceptorArray[ACCEPTOR_MESSAGE] = this;
      resetAcceptors();
   }

   /**
    * Resets all of the acceptors.
    */
   protected void resetAcceptors()
   {
      for (int i = 0; i < m_acceptorArray.length; i++)
      {
         if (m_acceptorArray[i] != null)
         {
            m_acceptorArray[i].reset();
         }
      }
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#checkFault(nexj.core.rpc.TransferObject)
    */
   public void checkFault(TransferObject root) throws Exception
   {
      // No fault is possible
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#reset()
    */
   public void reset()
   {
      m_bProcessRoot = true;
      m_bNilElement = false;
      m_bRestart = false;
      m_namespaceNameURIMap.clear();
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#getValue()
    */
   public Object getValue()
   {
      return null;
   }
}
