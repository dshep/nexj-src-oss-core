// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;

import org.apache.xerces.impl.xs.SchemaGrammar;
import org.apache.xerces.xni.XMLResourceIdentifier;
import org.apache.xerces.xni.XNIException;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.apache.xerces.xni.parser.XMLInputSource;
import org.apache.xerces.xs.StringList;
import org.apache.xerces.xs.XSAnnotation;
import org.apache.xerces.xs.XSAttributeDeclaration;
import org.apache.xerces.xs.XSAttributeUse;
import org.apache.xerces.xs.XSComplexTypeDefinition;
import org.apache.xerces.xs.XSConstants;
import org.apache.xerces.xs.XSElementDeclaration;
import org.apache.xerces.xs.XSModel;
import org.apache.xerces.xs.XSModelGroup;
import org.apache.xerces.xs.XSNamedMap;
import org.apache.xerces.xs.XSNamespaceItem;
import org.apache.xerces.xs.XSNamespaceItemList;
import org.apache.xerces.xs.XSObject;
import org.apache.xerces.xs.XSObjectList;
import org.apache.xerces.xs.XSParticle;
import org.apache.xerces.xs.XSSimpleTypeDefinition;
import org.apache.xerces.xs.XSTerm;
import org.apache.xerces.xs.XSTypeDefinition;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.persistence.PersistenceException;
import nexj.core.scripting.Pair;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.ObjUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XSDUtil;
import nexj.core.util.XMLUtil.ElementHandler;
import nexj.core.util.XMLUtil.MappedResolver;

public class XSDMessageImporter
{
   // constants
   
   public final static String NAMESPACE_WSDL = "http://schemas.xmlsoap.org/wsdl/";
   public final static String NAMESPACE_SOAP = "http://schemas.xmlsoap.org/wsdl/soap/";
   public final static String NAMESPACE_SOAP12 = "http://schemas.xmlsoap.org/wsdl/soap12/";
   public final static String XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";
   public final static String BOOLEAN_TYPE = "boolean";
   public final static String STRING_TYPE = "string";
   public final static String ANYURI_TYPE = "anyURI";
   private final static int RECURRENCE_MAX_COUNT = 1;
   public final static XSParticle EMPTY_PARTICLE_ARRAY[] = new XSParticle[0];
   
   // attributes

   private boolean m_bReuseRootMessages = true;
   private boolean m_bAnyTypeFound;
   private int m_nNamespaceSuffix;
   private boolean m_bCreateMessageForSingleReference = false;

   // associations
   
   private static Templates s_extractXSDTemplates;
   private final XSModel m_xsModel;
   private final Metadata m_metadata;
   private RootXMLMessagePartMapping m_rootMapping;
   private final NameResolver m_msgNameResolver;
   private MessageRegistry m_msgRegistry; //key: XSTypeDefinition instance, value: Message instance
   private final List m_msgList = new ArrayList(); //type Message
   private final Lookup m_namespaceRegistry;
   private final Lookup m_occurrenceMap = new HashTab();
   private List m_fixupList;
   private Document m_wsdlDoc;

   private final static Logger s_logger = Logger.getLogger(XSDMessageImporter.class);

   /**
    * @param resolver Set to null for default behaviour
    * @throws IOException 
    */
   private XSDMessageImporter(XSModel xsModel, Document wsdlDoc, Metadata metadata, NameResolver resolver, Lookup namespaceRegistry, MessageRegistry existingRegistry) throws IOException
   {
      m_metadata = metadata;
      m_msgNameResolver = (resolver == null) ? new NameResolver() : resolver;
      m_xsModel = xsModel;
      m_wsdlDoc = wsdlDoc;
      m_namespaceRegistry = namespaceRegistry;
      m_nNamespaceSuffix = namespaceRegistry.size();
      m_msgRegistry = existingRegistry;
      
      if (m_xsModel == null)
      {
         throw new IOException("No schema definition found");
      }
   }
   
   private MessageRegistry getMessageRegistry()
   {
      if (m_msgRegistry == null)
      {
         m_msgRegistry = new MessageRegistry();
      }
      
      return m_msgRegistry;
   }

   private boolean isRootElementDeclaration(XSElementDeclaration decl)
   {
      if (!isComposite(decl, false))
      {
         return false;
      }

      XSComplexTypeDefinition typeDef = XSDUtil.getComplexTypeDef(decl);

      XSNamedMap namedMap = m_xsModel.getComponents(XSConstants.ELEMENT_DECLARATION);

      for (int i = 0; i < namedMap.getLength(); ++i)
      {
         XSElementDeclaration elemDecl = (XSElementDeclaration)namedMap.item(i);
         
         if (elemDecl.getTypeDefinition() == typeDef)
         {
            return true;
         }
      }

      if ("anyType".equals(typeDef.getName()))
      {
         return false;
      }

      if (!typeDef.getAnonymous())
      {
         if (getMessageRegistry().get(typeDef) == null)
         {
            createCompositeMessagePart(null, parseDescription(decl.getAnnotation()), null, typeDef, typeDef.getName(), decl.getNamespace(), decl.getNillable(), null);
         }

         return true;
      }
      
      return false;
   }
   
   private void addFixup(MessageFixup fixup)
   {
      if (m_fixupList == null)
      {
         m_fixupList = new ArrayList();
      }

      m_fixupList.add(fixup);
   }
   
   public static Message[] createMessageParts(URL xsdURL, Metadata metadata, NameResolver msgNameResolver) throws IOException
   {
      return createMessageParts(xsdURL, metadata, msgNameResolver, false);
   }

   public static Message[] createMessageParts(URL xsdURL, final Metadata metadata, final NameResolver msgNameResolver, final boolean createMessagesForSingleReferences) throws IOException
   {
      final List messageList = new ArrayList(); 
      final Lookup namespaceRegistry = new HashTab();
      final MessageRegistry msgRegistry = new MessageRegistry(); 
      
      forEachXSModel(xsdURL, new IXSModelHandler()
      {
         public boolean handle(XSModel xsModel, Document wsdlDoc) throws IOException
         {
            XSDMessageImporter importer = new XSDMessageImporter(xsModel, wsdlDoc, metadata, msgNameResolver, namespaceRegistry, msgRegistry);
            importer.m_bCreateMessageForSingleReference = createMessagesForSingleReferences;
            messageList.addAll(Arrays.asList(importer.createMessageParts()));
            
            return false;
         }
      });
      
      return (Message[])messageList.toArray(new Message[messageList.size()]);
   }

   public static Message createMessagePart(URL xsdURL, final Metadata metadata, final String rootElement, final NameResolver msgNameResolver) throws IOException
   {
      final Message retVal[] = new Message[1];
      final Lookup namespaceRegistry = new HashTab();
      
      forEachXSModel(xsdURL, new IXSModelHandler()
      {
         public boolean handle(XSModel xsModel, Document wsdlDoc) throws IOException
         {
            XSDMessageImporter creator = new XSDMessageImporter(xsModel, wsdlDoc, metadata, msgNameResolver, namespaceRegistry, null);
            
            creator.m_bReuseRootMessages = false;

            XSElementDeclaration elemDecl = creator.m_xsModel.getElementDeclaration(rootElement, null);

            if (elemDecl == null)
            {
               XSNamespaceItemList l = creator.m_xsModel.getNamespaceItems();
               
               for (int i = 0; i < l.getLength(); ++i)
               {
                  XSNamespaceItem nsi = l.item(i);
                  
                  elemDecl = nsi.getElementDeclaration(rootElement);
                  
                  if (elemDecl != null)
                  {
                     break;
                  }
               }
               
               if (elemDecl == null)
               {
                  return false;
               }
            }
            
            retVal[0] = creator.createMessage(elemDecl);
            
            return true;
         }
      });
      
      return retVal[0];
   }

   public Message[] createMessageParts()
   {
      XSNamedMap namedMap = m_xsModel.getComponents(XSConstants.ELEMENT_DECLARATION);
      
      for (int i = 0; i < namedMap.getLength(); ++i)
      {
         XSElementDeclaration elemDecl = (XSElementDeclaration)namedMap.item(i);
         
         if (getMessageRegistry().register(elemDecl))
         {
            createMessagePart(null, elemDecl, null);
         }
      }

      namedMap = m_xsModel.getComponents(XSTypeDefinition.COMPLEX_TYPE);
      
      for (int i = 0; i < namedMap.getLength(); ++i)
      {
         XSComplexTypeDefinition typeDef = (XSComplexTypeDefinition)namedMap.item(i);
         
         if (!isComposite(typeDef, true))
         {
            continue;
         }
         
         if (!isRegistered(typeDef))
         {
            boolean bCreate = m_bAnyTypeFound;

            if (!bCreate)
            {
               // search for base types that have messages
               for (XSTypeDefinition baseType = typeDef.getBaseType(); baseType instanceof XSComplexTypeDefinition; baseType = baseType.getBaseType())
               {
                  if (isRegistered((XSComplexTypeDefinition)baseType))
                  {
                     bCreate = true;
                     break;
                  }
                  
                  if (baseType == baseType.getBaseType())
                  {
                     break;
                  }
               }
            }

            if (bCreate)
            {
               create(typeDef);
            }
         }
      }
      
      if (m_fixupList != null)
      {
         for (int i = 0; i < m_fixupList.size(); ++i)
         {
            ((MessageFixup)m_fixupList.get(i)).fixup();
         }
      }

      XMLMetadataLoader.finishMessagePartMappings(m_msgList.iterator());

      try
      {
         resolveWSDLBindings();
      }
      finally
      {
         m_wsdlDoc = null;
      }
      
      return (Message[])m_msgList.toArray(new Message[m_msgList.size()]);
   }

   protected Message create(XSComplexTypeDefinition typeDef)
   {
      return create(typeDef, typeDef.getName());
   }

   protected Message create(XSComplexTypeDefinition typeDef, String sName)
   {
      Message[] messageArray = new Message[1];

      createCompositeMessagePart(null, null, null, typeDef, sName, typeDef.getNamespace(), false, messageArray);

      return messageArray[0];
   }

   private void resolveWSDLBindings()
   {
      if (m_wsdlDoc == null)
      {
         return;
      }
      
      WSDLBindingsManager manager = new WSDLBindingsManager(m_wsdlDoc.getDocumentElement());
      
      for (Iterator itr = manager.getBindingIterator(); itr.hasNext();)
      {
         Element binding = (Element)itr.next();

         Element portType = manager.getPortType(binding);
         byte nEnvelope = manager.getEnvelope(binding);
         
         if (portType != null)
         {
            for (Iterator operationItr = manager.operationIterator(binding); operationItr.hasNext();)
            {
               Element bindingOperation = (Element)operationItr.next();
               String operationName = bindingOperation.getAttribute("name");
               
               Message messages[] = manager.parseInputOutputMessages(portType, operationName, m_msgList);
               
               if (messages[0] != null)
               {
                  messages[0].setResponse(messages[1]);
                  
                  RootXMLMessagePartMapping mapping = (RootXMLMessagePartMapping)messages[0].getRoot().getMapping();
                  
                  mapping.setEnvelope(nEnvelope);
                  mapping.setAction(manager.getSoapAction(binding, operationName));
               }
               
               if (messages[1] != null)
               {
                  RootXMLMessagePartMapping mapping = (RootXMLMessagePartMapping)messages[1].getRoot().getMapping();
                  
                  mapping.setEnvelope(nEnvelope);
               }
            }
         }
      }
   }
   
   public Message createMessage(XSElementDeclaration childDecl)
   {
      Message msg[] = new Message[1];

      createMessagePart(null, childDecl, msg);

      return msg[0];
   }

   /**
    * @return new Message part or null if recurrence limit has been reached
    */
   private MessagePart createMessagePart(CompositeMessagePartInstance parent, final XSElementDeclaration childDecl,
      Message msg[])
   {
      if (parent == null)
      {
         m_occurrenceMap.clear();
      }
      
      int nOccurrenceCount = 1;
      
      Integer count = (Integer)m_occurrenceMap.get(childDecl);
      
      if (count != null)
      {
         nOccurrenceCount += count.intValue();
         
         if (nOccurrenceCount > RECURRENCE_MAX_COUNT)
         {
            return null;
         }
      }
      
      m_occurrenceMap.put(childDecl, Primitive.createInteger(nOccurrenceCount));

      try
      {
         if (isComposite(childDecl, parent == null))
         {
            return createCompositeMessagePart(parent, childDecl, msg);
         }

         return parent == null ? null : createPrimitiveMessagePart(parent, childDecl);
      }
      finally
      {
         m_occurrenceMap.put(childDecl, Primitive.createInteger(nOccurrenceCount - 1));
      }
   }
   
   private PrimitiveMessagePart createPrimitiveMessagePart(CompositeMessagePartInstance parent, XSElementDeclaration elemDecl)
   {
      assert !isComposite(elemDecl, parent == null);
      
      PrimitiveMessagePart part = new PrimitiveMessagePart(elemDecl.getName());

      part.setParent(parent);
      part.setDescription(parseDescription(elemDecl.getAnnotation()));
      
      setMapping(part, null, elemDecl);
      
      if (elemDecl.getTypeDefinition() instanceof XSSimpleTypeDefinition)
      {
         String[] sEnumerationArray = XSDUtil.getEnumeration((XSSimpleTypeDefinition)elemDecl.getTypeDefinition());

         for (int nEnumIndex = 0; nEnumIndex < sEnumerationArray.length; ++nEnumIndex)
         {
            part.addEnumeration(sEnumerationArray[nEnumIndex]);
         }
      }

      return part;
   }

   private CompositeMessagePart createCompositeMessagePart(CompositeMessagePartInstance parent,
      XSElementDeclaration complexDecl, Message msg[])
   {
      return createCompositeMessagePart(parent, parseDescription(complexDecl.getAnnotation()), complexDecl, XSDUtil.getComplexTypeDef(complexDecl), complexDecl.getName(), 
         complexDecl.getNamespace(), complexDecl.getNillable(), msg);
   }

   /**
    * Creates a composite message part for a complex xsd element.
    * @param parent The parent composite message part or null.
    * @param sElemDesc A description string or null.
    * @param elemDecl Optional.
    * @param complexDef The complex element type definition.
    * @param sNodeName The xsd element's name;
    * @param sNamespace The xsd element's namespace.
    * @param bNillable The xsd element's nillable property.
    * @param msgArray Optional array of size 1 in which to store returning Message instance.
    * @return A new composite message part instance.
    */
   private CompositeMessagePart createCompositeMessagePart(final CompositeMessagePartInstance parent, String sElemDesc,
      XSElementDeclaration elemDecl, final XSComplexTypeDefinition complexDef, String sNodeName, String sNamespace,
      boolean bNillable, Message msgArray[])
   {
      assert isComposite(complexDef, parent == null);

      CompositeMessagePartInstance newInstance;
      Message newMsg = null;
      String sMsgName = sNodeName; 
      XSComplexTypeDefinition base = findBase(complexDef);

      if (parent == null)
      {
         sMsgName = m_msgNameResolver.generateName(sMsgName);
         newMsg = new Message(sMsgName);

         m_msgList.add(newMsg);

         if (msgArray != null)
         {
            msgArray[0] = newMsg;
         }

         if (m_metadata != null)
         {
            newMsg.setMetadata(m_metadata);
            newMsg.setFormat(m_metadata.getFormat("XML"));
         }

         if (isRegistered(complexDef))
         {
            String sName = new NameResolver().generateName(sNodeName);
            CompositeMessagePartRef ref = new CompositeMessagePartRef(sName);

            ref.setDescription(sElemDesc);
            newMsg.setRoot(ref);
            setMapping(ref, newMsg, sNodeName, sNamespace, bNillable, complexDef);

            addFixup(new MessageRefFixup(ref, sNodeName, sNamespace, bNillable, complexDef));

            return ref;
         }
         
         registerMessage(complexDef, newMsg);

         newInstance = new CompositeMessagePartInstance(sMsgName);
         newMsg.setRoot(newInstance);
         
         if (base != null)
         {
            addFixup(new MessageBaseFixup(base, newMsg));
         }
      }
      else
      {
         sMsgName = new NameResolver()
         {
            public boolean isValid(String name)
            {
               return !parent.hasPart(name);
            }
         }.generateName(sNodeName);

         assert base == null || elemDecl != null;
         
         if (base != null && elemDecl != null)
         {
            StringBuilder builder = new StringBuilder(sMsgName);
            
            for (CompositeMessagePartInstance ancestor = parent; ancestor != null; ancestor = ancestor.getParent())
            {
               builder.insert(0, ancestor.getName() + "_");
            }
            
            create(complexDef, builder.toString());
            return createRef(parent, elemDecl, sMsgName);
         }
         
         base = null;
         newInstance = new CompositeMessagePartInstance(sMsgName);
         newInstance.setParent(parent);
      }

      XSObjectList annotationList = complexDef.getAnnotations();
      
      if (annotationList != null && annotationList.getLength() > 0)
      {
         StringBuilder buffer = new StringBuilder();
         
         if (sElemDesc != null)
         {
            buffer.append(sElemDesc);
         }

         for (int i = 0; i < annotationList.getLength(); ++i)
         {
            XSAnnotation annotation = (XSAnnotation)annotationList.item(i);
         
            String sDescription = parseDescription(annotation);
            
            if (sDescription != null)
            {
               if (buffer.length() > 0)
               {
                  buffer.append("\r");
               }
               
               buffer.append(sDescription);
            }
         }

         if (buffer.length() > 0)
         {
            newInstance.setDescription(buffer.toString());
         }
      }
      else
      {
         newInstance.setDescription(sElemDesc);
      }

      setMapping(newInstance, newMsg, sNodeName, sNamespace, bNillable, complexDef);
      addAttributeParts(newInstance, complexDef, base);
      addChildParts(newInstance, EMPTY_PARTICLE_ARRAY, complexDef, base, null);
      addElementContentPart(newInstance, complexDef);
      
      return newInstance;
   }

   /**
    * Parse an annotation for a description string by searching for element text.
    * @param annotation An xsd annotation or null.
    * @return A description string obtained from the annotation or null if no description could be obtained.
    */
   protected static String parseDescription(XSAnnotation annotation)
   {
      if (annotation == null)
      {
         return null;
      }
      
      final StringBuilder descriptionBuffer = new StringBuilder();
   
      try
      {
         XMLUtil.parse(new StringReader(annotation.getAnnotationString()), new DefaultHandler()
         {
            StringBuilder m_buffer = new StringBuilder();
            
            /**
             * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
             */
            public void endElement(String uri, String localName, String name) throws SAXException
            {
               if (m_buffer.length() > 0)
               {
                  String sDescription = m_buffer.toString().trim();
                  m_buffer.setLength(0);
                  
                  if (sDescription.length() > 0)
                  {
                     if (descriptionBuffer.length() > 0)
                     {
                        descriptionBuffer.append("\r");
                     }
                     
                     descriptionBuffer.append(sDescription);
                  }
               }
            }
            
            /**
             * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
             */
            public void characters(char[] ch, int start, int length) throws SAXException
            {
               m_buffer.append(ch, start, length);
            }
         });
      }
      catch (Exception e)
      {
         s_logger.debug("Unable to parse XSD annotation", e);
      }
      
      return (descriptionBuffer.length() == 0) ? null : descriptionBuffer.toString();
   }
   
   private void addElementContentPart(CompositeMessagePartInstance parent, XSComplexTypeDefinition def)
   {
      if (def.getContentType() != XSComplexTypeDefinition.CONTENTTYPE_MIXED
         && !def.derivedFrom(XML_SCHEMA, STRING_TYPE, (short)(XSConstants.DERIVATION_EXTENSION | XSConstants.DERIVATION_RESTRICTION))
         && !def.derivedFrom(XML_SCHEMA, ANYURI_TYPE, (short)(XSConstants.DERIVATION_EXTENSION | XSConstants.DERIVATION_RESTRICTION)))
      {
         return;
      }

      String sPartName = "value";
      
      while (parent.hasPart(sPartName))
      {
         sPartName = "_" + sPartName;
      }

      PrimitiveMessagePart valuePart = new PrimitiveMessagePart(sPartName);

      valuePart.setType(Primitive.STRING);
      valuePart.setMinCount(0);
      valuePart.setMaxCount(1);

      parent.addPart(valuePart);

      XMLMessagePartMapping mapping = new XMLMessagePartMapping();

      valuePart.setMapping(mapping);
      mapping.setNodeType(XMLMessagePartMapping.VALUE);
      mapping.init(valuePart);
   }

   private static boolean contains(XSObjectList list, XSObject object)
   {
      if (list != null)
      {
         for (int i = 0; i < list.getLength(); ++i)
         {
            if (list.item(i) == object)
            {
               return true;
            }
         }
      }

      return false;
   }

   private void addAttributeParts(CompositeMessagePartInstance parent, XSComplexTypeDefinition def, XSComplexTypeDefinition baseType)
   {
      XSObjectList attributeUses = def.getAttributeUses();

      if (attributeUses == null || attributeUses.getLength() == 0)
      {
         return;
      }

      XSObjectList baseAttributeUses = (baseType != null) ? baseType.getAttributeUses() : null;

      for (int i = 0; i < attributeUses.getLength(); ++i)
      {
         XSAttributeUse attr = (XSAttributeUse)attributeUses.item(i);
         
         if (!contains(baseAttributeUses, attr))
         {
            XSAttributeDeclaration attrDecl = attr.getAttrDeclaration();
            String sName = attrDecl.getName();
            PrimitiveMessagePart part = new PrimitiveMessagePart(sName);

            if (!addPart(parent, part))
            {
               if (s_logger.isWarnEnabled())
               {
                  s_logger.warn("Ignoring duplicate XSD attribute definition \"" + attr.getName() +
                     "\" in element type \"" + def.getName() + "\"");
               }

               continue;
            }

            part.setParent(parent);

            if (attr.getRequired())
            {
               part.setMinCount(1);
            }

            part.setMaxCount(1);
            part.setDescription(parseDescription(attrDecl.getAnnotation()));

            XMLMessagePartMapping mapping = new XMLMessagePartMapping();

            mapping.setNodeName(sName);
            initMapping(mapping, part, XMLMessagePartMapping.ATTRIBUTE, attrDecl.getNamespace(), attrDecl.getTypeDefinition()); 

            String[] enumerations = XSDUtil.getEnumeration(attrDecl);

            for (int nEnumIndex = 0; nEnumIndex < enumerations.length; ++nEnumIndex)
            {
               part.addEnumeration(enumerations[nEnumIndex]);
            }
         }
      }
   }

   private void addChildParts(CompositeMessagePartInstance parentPart, XSParticle modelGroupParticles[], XSParticle modelParticle, XSComplexTypeDefinition baseType, XSModelGroup childParts, Byte aggregation)
   {
      assert modelParticle.getTerm() == childParts;
      
      XSObjectList particleList = childParts.getParticles();
      byte nAggregation;
      
      switch (childParts.getCompositor())
      {
         case XSModelGroup.COMPOSITOR_ALL:
            nAggregation = CompositeMessagePart.RANDOM;
            break;

         case XSModelGroup.COMPOSITOR_CHOICE:            
            if (modelParticle.getMaxOccursUnbounded() || modelParticle.getMaxOccurs() > 1)
            {
               nAggregation = CompositeMessagePart.RANDOM;
            }
            else
            {
               nAggregation = CompositeMessagePart.SINGLE;
            }

            break;

         case XSModelGroup.COMPOSITOR_SEQUENCE:
            nAggregation = CompositeMessagePart.SEQUENTIAL;
            
            break;

         default:
            return;
      }

      if (aggregation == null 
         || aggregation.byteValue() == CompositeMessagePart.SINGLE
         || (aggregation.byteValue() == CompositeMessagePart.SEQUENTIAL && nAggregation != CompositeMessagePart.SINGLE))
      {
         parentPart.setAggregation(nAggregation);
         aggregation = new Byte(nAggregation);
      }

      XSParticle newModelGroupParticles[] = new XSParticle[modelGroupParticles.length + 1];

      System.arraycopy(modelGroupParticles, 0, newModelGroupParticles, 0, modelGroupParticles.length);
      newModelGroupParticles[modelGroupParticles.length] = modelParticle;
      
      for (int i = 0; i < particleList.getLength(); ++i)
      {
         XSParticle childParticle = (XSParticle)particleList.item(i);

         if (!contains(getParticleList(baseType), childParticle))
         {
            addChildParts(parentPart, newModelGroupParticles, childParticle, baseType, aggregation);
         }
      }
   }
   
   protected static XSObjectList getParticleList(XSComplexTypeDefinition type)
   {
      if (type != null)
      {
         XSParticle particle = XSDUtil.getParticle(type);
   
         if (particle != null)
         {
            XSTerm term = particle.getTerm();
            
            if (term.getType() == XSConstants.MODEL_GROUP)
            {
               return ((XSModelGroup)term).getParticles();
            }
         }
      }
      
      return null;
   }

   private void setMinMaxCount(XSParticle modelGroupParticles[], MessagePart part, XSParticle particle, XSElementDeclaration particleTerm)
   {
      try
      {
         if (particleTerm.getTypeDefinition() instanceof XSSimpleTypeDefinition)
         {
            XSSimpleTypeDefinition typeDef = (XSSimpleTypeDefinition)particleTerm.getTypeDefinition();
            
            if (typeDef.getBuiltInKind() == XSConstants.STRING_DT && XSDUtil.getEnumeration(typeDef).length == 0)
            {
               int nMinOccurs = 0;
               
               if (particle.getMinOccurs() > 0)
               {
                  if (!particleTerm.getNillable())
                  {
                     nMinOccurs = particle.getMinOccurs();
                  }
               }
               else if (typeDef.isDefinedFacet(XSSimpleTypeDefinition.FACET_MINLENGTH))
               {
                  String sMinLength = typeDef.getLexicalFacetValue(XSSimpleTypeDefinition.FACET_MINLENGTH);
                  int nMinLength = Primitive.toInteger(sMinLength).intValue();
                  
                  if (nMinLength > 0)
                  {
                     nMinOccurs = 1;
                  }
               }
               
               part.setMinCount(nMinOccurs);

               if (particle.getMaxOccursUnbounded())
               {
                  part.setMaxCount(0);
               }
               else
               {
                  part.setMaxCount(1);
               }

               return;
            }
         }
         else
         {
            XSComplexTypeDefinition complexType = (XSComplexTypeDefinition)particleTerm.getTypeDefinition();
            
            if (!isAnyType(complexType) && !isComposite(complexType, false))
            {
               part.setMinCount(0);
               part.setMaxCount(1);

               return;
            }
         }
         
         if (particleTerm.getNillable())
         {
            part.setMinCount(0);
         }
         else
         {
            part.setMinCount(particle.getMinOccurs());
         }
         
         if (particle.getMaxOccursUnbounded())
         {
            part.setMaxCount(0);
         }
         else
         {
            part.setMaxCount(particle.getMaxOccurs());
         }
      }
      finally
      {
         if (modelGroupParticles.length > 0)
         {
            XSParticle intermediateParticle = modelGroupParticles[0];

            assert intermediateParticle.getTerm() instanceof XSModelGroup;
            
            int nMinOccurs = getMinOccurs(modelGroupParticles, 0, intermediateParticle.getMinOccurs(), part);
            int nMaxOccurs = intermediateParticle.getMaxOccursUnbounded() ? 0 : intermediateParticle.getMaxOccurs();

            for (int i = 1; i < modelGroupParticles.length; ++i)
            {
               intermediateParticle = modelGroupParticles[i];

               assert intermediateParticle.getTerm() instanceof XSModelGroup;

               nMinOccurs = getMinOccurs(modelGroupParticles, i, Math.min(nMinOccurs, intermediateParticle.getMinOccurs()), part);
               
               if (nMaxOccurs != 0)
               {
                  nMaxOccurs = intermediateParticle.getMaxOccursUnbounded() ? 0 : Math.max(nMaxOccurs, intermediateParticle.getMaxOccurs());
               }
            }
            
            part.setMinCount(Math.min(nMinOccurs, part.getMinCount()));
            
            if (part.getMaxCount() != Integer.MAX_VALUE)
            {
               part.setMaxCount(nMaxOccurs == 0 ? 0 : Math.max(nMaxOccurs, part.getMaxCount()));
            }
         }
      }
   }

   protected int getMinOccurs(XSParticle modelGroupParticles[], int nIndex, int nDefault, final MessagePart part)
   {
      if (nDefault != 0)
      {
         if (nIndex < modelGroupParticles.length - 1)
         {
            /*
             * In the following example, phone is not required
             * <choice
             *    <sequence
             *       element name="phone"
             *    </sequence
             *    <sequence
             *       ...
             */
            XSParticle p = modelGroupParticles[nIndex];
   
            if (isChoice(p) && ((XSModelGroup)p.getTerm()).getParticles().getLength() > 1)
            {
               return 0;
            }
         }
         else if (nIndex == modelGroupParticles.length - 1 && isChoice(modelGroupParticles[nIndex]))
         {
            if (nIndex > 0)
            {
               /*
                * In the following example, externalAct is not required
                * <sequence>
                *     <element
                *     <choice>
                *         element name="externalAct
                */
               addFixup(new MessageFixup()
               {
                  public void fixup()
                  {
                     if (part.getParent() != null && part.getParent().getAggregation() == CompositeMessagePartInstance.SEQUENTIAL)
                     {
                        part.setMinCount(0);
                     }
                  }
               });
            }

            if (((XSModelGroup)modelGroupParticles[nIndex].getTerm()).getParticles().getLength() > 1 && isMaxOccursGreaterThanOne(modelGroupParticles[nIndex]))
            {
               /*
                * In the following example, when the aggregation is set to random, th and td should not be required.
                * <xs:complexType name="StrucDoc.Tr">
                *    <xs:choice maxOccurs="unbounded">
                *       <xs:element name="th" type="StrucDoc.Th"/>
                *       <xs:element name="td" type="StrucDoc.Td"/>
                *    </xs:choice>
                */
               addFixup(new MessageFixup()
               {
                  public void fixup()
                  {
                     if (part.getParent() != null && part.getParent().getAggregation() == CompositeMessagePartInstance.RANDOM)
                     {
                        part.setMinCount(0);
                     }
                  }
               });
            }
         }
      }

      return nDefault;
   }

   protected static boolean isChoice(XSParticle p)
   {
      return p.getTerm().getType() == XSConstants.MODEL_GROUP
         && ((XSModelGroup)p.getTerm()).getCompositor() == XSModelGroup.COMPOSITOR_CHOICE;
   }

   private void addChildParts(CompositeMessagePartInstance parentPart, XSParticle[] modelGroupParticles, XSComplexTypeDefinition typeDef, XSComplexTypeDefinition baseType, Byte aggregation)
   {
      XSParticle childParticle = XSDUtil.getParticle(typeDef);
      
      if (childParticle == null)
      {
         return;
      }
      
      addChildParts(parentPart, modelGroupParticles, childParticle, baseType, aggregation);
   }
   
   /**
    * This is a helper function to check if a message should have aggregation set to random.
    * It is called in the case that a sequence model group may have a choice model group as a child.
    * 
    * Returns true if the choice model group (if it exists) satisfies all the following conditions:
    *    1. It has maxOccurs greater than 1
    *    2. It has more than 1 descendant element
    *    3. The choice has a sibling (element or model group with descendant element) or an uncle ancestor element
    */
   private static boolean hasChoiceDescendantWithSiblingOrUncleElement(XSParticle particle, boolean bHasUncle)
   {
      XSTerm term = particle.getTerm();

      if (term.getType() == XSConstants.MODEL_GROUP)
      {
         XSModelGroup modelGroup = (XSModelGroup)term;
         XSObjectList particleList = modelGroup.getParticles();
         
         if (particleList.getLength() > 1)
         {
            //inspect children
            boolean bHasElement = hasElement(particle);
            List modelGroupList = getModelGroups(particle);

            //search for children that are model groups of type choice
            for (int i = 0; i < modelGroupList.size(); i++)
            {
               XSParticle childParticle = (XSParticle)modelGroupList.get(i);
               XSTerm childTerm = childParticle.getTerm();

               assert childTerm.getType() == XSConstants.MODEL_GROUP;

               if (bHasElement || modelGroupList.size() > 1 || bHasUncle)
               {
                  XSModelGroup childModelGroup = (XSModelGroup)childTerm;

                  if (childModelGroup.getCompositor() == XSModelGroup.COMPOSITOR_CHOICE
                     && (childParticle.getMaxOccursUnbounded() || childParticle.getMaxOccurs() > 1)
                     && hasDescendantElements(childParticle, 1))
                  {
                     return true;
                  }
                  else if (childModelGroup.getCompositor() == XSModelGroup.COMPOSITOR_SEQUENCE)
                  {
                     //bHasUncle is set true because at least one of the following is true:
                     // 1. childModelGroup has a sibling element
                     // 2. childModelGroup has a sibling model group with a descendant element
                     // 3. childModelGroup has an uncle element or uncle model group with a descendant element
                     if (hasChoiceDescendantWithSiblingOrUncleElement(childParticle, true))
                     {
                        return true;
                     }
                  }
               }
               else
               {
                  //childModelGroup has no sibling element
                  //childModelGroup has no sibling model group with descendant elements
                  //childModelGroup has no uncle
                  
                  return hasChoiceDescendantWithSiblingOrUncleElement(childParticle, bHasUncle);
               }
            }

            return false;
         }
         else if (particleList.getLength() == 1)
         {
            return hasChoiceDescendantWithSiblingOrUncleElement((XSParticle)particleList.item(0), bHasUncle);
         }
         else
         {
            return false;
         }
      }
      else
      {
         return false;
      }
   }

   /**
    * @return the particles of particle that are model groups with at least one descendant element
    */
   private static List getModelGroups(XSParticle particle)
   {
      List retVal = new ArrayList();
      XSTerm term = particle.getTerm();

      if (term.getType() == XSConstants.MODEL_GROUP)
      {
         XSModelGroup modelGroup = (XSModelGroup)term;
         XSObjectList particleList = modelGroup.getParticles();

         //inspect particles and remember the ones with element descendants
         for (int i = 0; i < particleList.getLength(); i++)
         {
            XSParticle childParticle = (XSParticle)particleList.item(i);

            if (hasDescendantElements(childParticle, 0))
            {
               retVal.add(childParticle);
            }
         }
      }
      
      return retVal;
   }

   private static boolean hasElement(XSParticle particle)
   {
      if (particle.getTerm().getType() == XSConstants.MODEL_GROUP)
      {
         XSTerm term = particle.getTerm();
         XSObjectList childParticleList = ((XSModelGroup)term).getParticles();

         for (int i = 0; i < childParticleList.getLength(); i++)
         {
            XSParticle childParticle = (XSParticle)childParticleList.item(i);
            XSTerm childTerm = childParticle.getTerm();

            if (childTerm.getType() == XSConstants.ELEMENT_DECLARATION)
            {
               return true;
            }
         }
      }
      return false;
   }
   
   /**
    * @return true if particle has more than the specified number of descendant elements
    */
   private static boolean hasDescendantElements(XSParticle particle, int nMoreThan)
   {
      if (particle.getTerm().getType() == XSConstants.MODEL_GROUP)
      {
         int nCount = 0;
         List particleList = new ArrayList();

         particleList.add(particle);

         while(!particleList.isEmpty())
         {
            XSTerm term = ((XSParticle)particleList.remove(0)).getTerm();
            XSObjectList childParticleList = ((XSModelGroup)term).getParticles();

            for (int i = 0; i < childParticleList.getLength(); i++)
            {
               XSParticle childParticle = (XSParticle)childParticleList.item(i);
               XSTerm childTerm = (childParticle).getTerm();

               if (childTerm.getType() == XSConstants.ELEMENT_DECLARATION)
               {
                  nCount += 1;

                  if (nCount > nMoreThan)
                  {
                     return true;
                  }
               }
               else if (childTerm.getType() == XSConstants.MODEL_GROUP)
               {
                  particleList.add(childParticle);
               }
            }
         }
      }
      
      return false;
   }

   protected CompositeMessagePartRef createRef(CompositeMessagePartInstance parentPart, XSElementDeclaration elemDecl,
      String sName)
   {
      CompositeMessagePartRef msgRef = new CompositeMessagePartRef(sName);

      msgRef.setParent(parentPart);
      setMapping(msgRef, null, elemDecl);
      addFixup(new MessageRefFixup(msgRef, elemDecl));

      return msgRef;
   }

   protected static boolean isMaxOccursGreaterThanOne(XSParticle childParticle)
   {
      return childParticle.getMaxOccursUnbounded() || childParticle.getMaxOccurs() > 1;
   }

   private void addChildParts(final CompositeMessagePartInstance parentPart, XSParticle[] modelGroupParticles, XSParticle childParticle, XSComplexTypeDefinition baseType, Byte aggregation)
   {
      XSTerm term = childParticle.getTerm();
      
      switch (term.getType())
      {
         case XSConstants.MODEL_GROUP:
            //determine if aggregation should be random
            if (parentPart.getAggregation() != CompositeMessagePart.RANDOM)
            {
               boolean bSetRandom = false;
               XSModelGroup modelGroup = (XSModelGroup)term;

               if (modelGroup.getCompositor() == XSModelGroup.COMPOSITOR_CHOICE)
               {
                  if (isMaxOccursGreaterThanOne(childParticle) && hasDescendantElements(childParticle, 1))
                  {
                     bSetRandom = true;
                  }
               }
               else if (modelGroup.getCompositor() == XSModelGroup.COMPOSITOR_SEQUENCE)
               {
                  //does it have choice as child with sibling or uncle
                  if (hasChoiceDescendantWithSiblingOrUncleElement(childParticle, false))
                  {
                     bSetRandom = true;
                  }
               }

               if (bSetRandom)
               {
                  parentPart.setAggregation(CompositeMessagePart.RANDOM);

                  if (aggregation == null)
                  {
                     aggregation = new Byte(parentPart.getAggregation());
                  }
               }
            }

            addChildParts(parentPart, modelGroupParticles, childParticle, baseType, (XSModelGroup)term, aggregation);
            break;

         case XSConstants.ELEMENT_DECLARATION:
            final XSElementDeclaration chldElemDecl = (XSElementDeclaration)term;
            MessagePart childPart;
            
            if (m_bReuseRootMessages && isRootElementDeclaration(chldElemDecl))
            {
               String sName = new NameResolver()
               {
                  public boolean isValid(String sName)
                  {
                     return !parentPart.hasPart(sName);
                  }
               }.generateName(chldElemDecl.getName());

               childPart = createRef(parentPart, chldElemDecl, sName);
            }
            else
            {
               childPart = createMessagePart(parentPart, chldElemDecl, null);
            }

            if (childPart == null)
            {
               break;
            }
            
            setMinMaxCount(modelGroupParticles, childPart, childParticle, chldElemDecl);
            
            if (!addPart(parentPart, childPart))
            {
               if (parentPart.getAggregation() == CompositeMessagePart.SEQUENTIAL)
               {
                  MessagePart lastPart = parentPart.getPart(parentPart.getPartCount() - 1);

                  if (lastPart.getName().equals(childPart.getName()))
                  {
                     if (lastPart.getMaxCount() != Integer.MAX_VALUE)
                     {
                        if (childPart.getMaxCount() == Integer.MAX_VALUE)
                        {
                           lastPart.setMaxCount(0);
                        }
                        else
                        {
                           lastPart.setMaxCount(lastPart.getMaxCount() + childPart.getMaxCount());
                        }
                     }

                     return;
                  }
               }

               if (s_logger.isWarnEnabled())
               {
                  s_logger.warn("Ignoring duplicate XSD child element definition \"" +
                     chldElemDecl.getName() + "\" in element \"" + parentPart.getName() +
                     "\", setting parent message to random aggregation with unlimited max count.");
               }

               parentPart.setAggregation(CompositeMessagePart.RANDOM);
               parentPart.setMaxCount(0);
            }

            break;

         case XSConstants.WILDCARD:
            parentPart.setLax(true);
            break;
      }
   }

   private void setMapping(MessagePart part, Message msg, XSElementDeclaration elemDecl)
   {
      setMapping(part, msg, elemDecl.getName(), elemDecl.getNamespace(), elemDecl.getNillable(), elemDecl.getTypeDefinition());
   }

   private void setMapping(MessagePart part, Message msg, String sNodeName, String sNamespace, boolean bNillable, XSTypeDefinition typeDef)
   {
      XMLMessagePartMapping mapping;

      if (msg != null)
      {
         mapping = m_rootMapping = new RootXMLMessagePartMapping(msg);
         
         if (typeDef != null)
         {
            m_rootMapping.setXSDType(typeDef.getName());
         }
         
         initNamespaces(m_rootMapping);
      }
      else
      {
         mapping = new XMLMessagePartMapping();
      }

      mapping.setNodeName(sNodeName);
      mapping.setNillable(bNillable);
      
      initMapping(mapping, part, XMLMessagePartMapping.ELEMENT, sNamespace, typeDef);
   }

   private void initNamespaces(RootXMLMessagePartMapping rootMapping)
   {
      XSNamespaceItemList nsList = m_xsModel.getNamespaceItems();
      
      for (int i = 0; i < nsList.getLength(); ++i)
      {
         XSNamespaceItem nsItem = nsList.item(i);
         String sURI = nsItem.getSchemaNamespace();

         if (sURI != null)
         {
            XMLNamespace namespace = getNamespace(sURI);

            rootMapping.addNamespace(namespace);

            StringList schemaList = nsItem.getDocumentLocations();
            
            if (schemaList.getLength() > 0)
            {
               String sDocURI = schemaList.item(0); //assume first item is primary location

               try
               {
                  String sProtocol = new URI(sDocURI).toURL().getProtocol();
                  
                  if (!"file".equalsIgnoreCase(sProtocol))
                  {
                     namespace.setSchema(sDocURI);
                  }
               }
               catch (Exception e)
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Cannot parse document URI: " + sDocURI, e);
                  }
               }
            }
         }
      }
   }

   private XMLNamespace getNamespace(String sURI)
   {
      XMLNamespace ns = (XMLNamespace)m_namespaceRegistry.get(sURI);

      if (ns == null)
      {
         ns = new XMLNamespace("ns" + Integer.toString(m_nNamespaceSuffix++), sURI);
         m_namespaceRegistry.put(sURI, ns);
      }

      return ns;
   }

   private String getNamespaceShortName(String sURI)
   {
      return getNamespace(sURI).getName();
   }

   /**
    * If no simple type can be deduced, return XSConstants.STRING_DT
    */
   private short parsePrimitiveBuildInKind(XSTypeDefinition typeDef, Set processedComplexTypes)
   {
      if (typeDef != null)
      {
         if (typeDef instanceof XSSimpleTypeDefinition)
         {
            return ((XSSimpleTypeDefinition)typeDef).getBuiltInKind();
         }
         else if (typeDef instanceof XSComplexTypeDefinition)
         {
            if (processedComplexTypes != null && processedComplexTypes.contains(typeDef))
            {
               return XSConstants.STRING_DT;
            }
            
            XSTypeDefinition baseType = ((XSComplexTypeDefinition)typeDef).getBaseType();
            
            if (baseType == typeDef)
            {
               return XSConstants.STRING_DT;
            }
            else if (baseType instanceof XSComplexTypeDefinition)
            {
               if (processedComplexTypes == null)
               {
                  processedComplexTypes = new HashSet();
               }
               
               processedComplexTypes.add(typeDef);
            }

            return parsePrimitiveBuildInKind(baseType, processedComplexTypes);
         }
      }
      
      return XSConstants.STRING_DT;
   }
   
   private void initMapping(XMLMessagePartMapping mapping, MessagePart part, byte nNodeType, String sNamespace, XSTypeDefinition typeDef)
   {  
      part.setMapping(mapping);

      if (part instanceof PrimitiveMessagePart)
      {
         PrimitiveMessagePart primitivePart = (PrimitiveMessagePart)part;

         if (isAnyType(typeDef))
         {
            m_bAnyTypeFound = true;
            primitivePart.setType(Primitive.ANY);
         }
         else
         {
            switch (parsePrimitiveBuildInKind(typeDef, null))
            {
               case XSConstants.STRING_DT:
                  primitivePart.setType(Primitive.STRING);
                  break;
   
               case XSConstants.HEXBINARY_DT:
                  primitivePart.setType(Primitive.BINARY);
                  mapping.setSubtype(XMLMessagePartMapping.SUBTYPE_HEX);
                  break;
   
               case XSConstants.BASE64BINARY_DT:
                  primitivePart.setType(Primitive.BINARY);
                  mapping.setSubtype(XMLMessagePartMapping.SUBTYPE_BASE64);
                  break;
   
               case XSConstants.INT_DT:
               case XSConstants.BYTE_DT:
               case XSConstants.UNSIGNEDBYTE_DT:
               case XSConstants.SHORT_DT:
               case XSConstants.UNSIGNEDSHORT_DT:
               case XSConstants.GDAY_DT:
               case XSConstants.GMONTH_DT:
               case XSConstants.GYEAR_DT:
                  primitivePart.setType(Primitive.INTEGER);
                  break;
   
               case XSConstants.LONG_DT:
               case XSConstants.UNSIGNEDINT_DT:
                  primitivePart.setType(Primitive.LONG);
                  break;
   
               case XSConstants.DECIMAL_DT:
               case XSConstants.INTEGER_DT:
               case XSConstants.NEGATIVEINTEGER_DT:
               case XSConstants.POSITIVEINTEGER_DT:
               case XSConstants.NONNEGATIVEINTEGER_DT:
               case XSConstants.NONPOSITIVEINTEGER_DT:
                  primitivePart.setType(Primitive.DECIMAL);
                  break;
   
               case XSConstants.FLOAT_DT:
                  primitivePart.setType(Primitive.FLOAT);
                  break;
   
               case XSConstants.DOUBLE_DT:
                  primitivePart.setType(Primitive.DOUBLE);
                  break;
   
               case XSConstants.DATE_DT:
                  primitivePart.setType(Primitive.TIMESTAMP);
                  mapping.setSubtype(XMLMessagePartMapping.SUBTYPE_DATE);
                  break;
   
               case XSConstants.TIME_DT:
                  primitivePart.setType(Primitive.TIMESTAMP);
                  mapping.setSubtype(XMLMessagePartMapping.SUBTYPE_TIME);
                  break;
   
               case XSConstants.DATETIME_DT:
                  primitivePart.setType(Primitive.TIMESTAMP);
                  mapping.setSubtype(XMLMessagePartMapping.SUBTYPE_DATETIME);
                  break;
   
               case XSConstants.BOOLEAN_DT:
                  primitivePart.setType(Primitive.BOOLEAN);

                  if (typeDef instanceof XSSimpleTypeDefinition && typeDef.getBaseType() != null
                     && typeDef.getBaseType().derivedFrom(XML_SCHEMA, BOOLEAN_TYPE, (short)XSConstants.DERIVATION_RESTRICTION))
                  {
                     XSSimpleTypeDefinition st = (XSSimpleTypeDefinition)typeDef;
                     StringList s = st.getLexicalPattern();

                     if (s.getLength() == 1)
                     {
                        String sPattern = s.item(0);

                        if ("true|false".equals(sPattern) || "false|true".equals(sPattern))
                        {
                           mapping.setFormat("true;false");
                        }
                     }
                  }

                  break;
   
               default:
                  primitivePart.setType(Primitive.STRING);
                  break;
            }
         }
      }

      mapping.setNodeType(nNodeType);

      if (sNamespace != null)
      {
         mapping.setNamespace(m_rootMapping.getNamespace(getNamespaceShortName(sNamespace)));
      }

      mapping.init(part);
   }

   private static Templates getWSDLTemplate()
   {
      if (s_extractXSDTemplates == null)
      {
         synchronized(XSDUtil.class)
         {
            s_extractXSDTemplates = XMLUtil.getTemplate(XSDMessageImporter.class.getResource("extractschema.xsl"));
         }
      }
      
      return s_extractXSDTemplates;
   }

   private boolean isComposite(XSElementDeclaration elemDecl, boolean isRoot)
   {
      return isComposite(XSDUtil.getComplexTypeDef(elemDecl), isRoot);
   }

   /**
    * @param typeDef Can be null
    * @return true if this is the anyType definition.
    */
   private static boolean isAnyType(XSTypeDefinition typeDef)
   {
      return typeDef == SchemaGrammar.fAnyType;
   }
   
   protected static boolean isComposite(XSComplexTypeDefinition def, boolean isRoot)
   {
      if (def == null || isAnyType(def))
      {
         return false;
      }
      
      if (XSDUtil.getParticle(def) != null)
      {
         return true;
      }
      
      XSObjectList attributeUses = def.getAttributeUses();
      
      if (attributeUses == null || attributeUses.getLength() == 0)
      {
         return isRoot;
      }
      
      return true;
   }
   
   private Message getRootMessage(XSComplexTypeDefinition typeDef)
   {
      Message retVal = (Message)getMessageRegistry().get(typeDef);
      
      if (retVal == null)
      {
         throw new PersistenceException("err.persistence.messageNotFoundForElementTypeDefinition", new Object[] {typeDef.getName()});
      }
      
      return retVal;
   }

   private static String[] getSchemaNamespaces(Document doc)
   {
      final List namespaceList = new ArrayList();
      
      XMLUtil.forEachElementRecur(doc.getDocumentElement(), null, new ElementHandler()
      {
         public void handleElement(Element element)
         {
            if (element.getLocalName().equals("schema"))
            {
               namespaceList.add(element.getAttribute("targetNamespace"));
            }
         }
      });
      
      return (String[])namespaceList.toArray(new String[namespaceList.size()]);
   }
   
   private static void forEachXSModel(URL xsdURL, IXSModelHandler handler) throws IOException
   {
      InputStream is = null;
      
      try
      {
         is = URLUtil.openStream(xsdURL);
         final Document doc = XMLUtil.parse(new InputSource(is));
         
         if (NAMESPACE_WSDL.equals(doc.getDocumentElement().getNamespaceURI()) ||
            "definitions".equals(doc.getDocumentElement().getLocalName()))
         {
            final String sSystemId = xsdURL.toExternalForm();
            final String[] sNamespaceArray = getSchemaNamespaces(doc);
            final String[] sContentArray = new String[sNamespaceArray.length];
            
            for (int i = 0; i < sNamespaceArray.length; i++)
            {
               try
               {
                  Transformer transformer = getWSDLTemplate().newTransformer();

                  transformer.setParameter("position", Primitive.createInteger(i + 1));
                  sContentArray[i] = XSDUtil.transform(transformer, new DOMSource(doc));
               }
               catch (TransformerException e)
               {
                  throw ObjUtil.rethrow(e);
               }
            }
            
            XMLEntityResolver resolver = new MappedResolver(null, true)
            {
               private String getContent(String sNamespace)
               {
                  if (sNamespace != null)
                  {
                     for (int i = 0; i < sNamespaceArray.length; ++i)
                     {
                        if (sNamespace.equals(sNamespaceArray[i]))
                        {
                           return sContentArray[i];
                        }
                     }
                  }
                  
                  return null;
               }
               /**
                * @see nexj.core.util.XMLUtil.MappedResolver#resolveEntity(org.apache.xerces.xni.XMLResourceIdentifier)
                */
               public XMLInputSource resolveEntity(XMLResourceIdentifier res) throws XNIException, IOException
               {
                  String sContent;
                  
                  // The presence of the literal system id indicates we are resolving a schema specified in an <include> tag
                  if (res.getLiteralSystemId() == null && (sContent = getContent(res.getNamespace())) != null)
                  {
                     return new XMLInputSource(null, sSystemId, null, new StringReader(sContent), null);
                  }
                  else
                  {
                     return super.resolveEntity(res);
                  }
               }
            };
            
            for (int i = 0; i < sNamespaceArray.length; ++i)
            {
               XSModel xsModel = XSDUtil.getXSModel(new XMLInputSource(null, sSystemId, null, new StringReader(sContentArray[i]), null), true, resolver);
               
               if (handler.handle(xsModel, doc))
               {
                  break;
               }
            }
            
            return;
         }
      }
      finally
      {
         IOUtil.close(is);
      }
      
      handler.handle(XSDUtil.getXSModel(XSDUtil.toXMLInputSource(xsdURL), true), null);
   }

   private boolean addPart(CompositeMessagePart parent, MessagePart child)
   {
      if (parent.hasPart(child.getName()))
      {
         return false;
      }
      
      parent.addPart(child);

      return true;
   }

   private void registerMessage(XSComplexTypeDefinition typeDef, Message msg)
   {
      getMessageRegistry().put(typeDef, msg);
      incrementMessageRefCount(typeDef);
   }
   
   /**
    * @return true if a Message is already defined for the given type definition.
    */
   private boolean isRegistered(XSComplexTypeDefinition typeDef)
   {
      return getMessageRegistry().get(typeDef) != null;  //do not use contains
   }

   /**
    * Increment the reference count against a type definition. 
    */
   private void incrementMessageRefCount(XSComplexTypeDefinition typeDef)
   {
      if (!m_bCreateMessageForSingleReference)
      {
         getMessageRegistry().incrementRefCount(typeDef);
      }
   }

   protected static XSComplexTypeDefinition findBase(XSComplexTypeDefinition complexDef)
   {
      XSTypeDefinition complexDefBaseType = complexDef.getBaseType();
      
      if (complexDefBaseType instanceof XSComplexTypeDefinition)
      {
         XSComplexTypeDefinition typeDef = (XSComplexTypeDefinition)complexDefBaseType;
         
         if (isComposite(typeDef, true))
         {
            return typeDef;
         }
      }
      
      return null;
   }

   // inner classes

   private interface MessageFixup
   {
      public void fixup();
   }

   private final class MessageBaseFixup implements MessageFixup
   {
      private final XSComplexTypeDefinition m_baseType;

      private final Message m_msg;

      public MessageBaseFixup(XSComplexTypeDefinition baseType, Message msg)
      {
         m_baseType = baseType;
         m_msg = msg;
      }

      public void fixup()
      {
         m_msg.setBaseMessage((XSDMessageImporter.this.isRegistered(m_baseType)) ? XSDMessageImporter.this
            .getRootMessage(m_baseType) : create(m_baseType));
      }
   }

   private final class MessageRefFixup implements MessageFixup
   {
      private final CompositeMessagePartRef m_partRef;
      private final XSComplexTypeDefinition m_typeDef;
      private final String m_sNodeName;
      private final String m_sNamespace;
      private final boolean m_bNillable;
      
      public MessageRefFixup(CompositeMessagePartRef part, XSElementDeclaration complexDecl)
      {
         this(part, complexDecl.getName(), complexDecl.getNamespace(), complexDecl.getNillable(), XSDUtil.getComplexTypeDef(complexDecl)); 
      }
      
      public MessageRefFixup(CompositeMessagePartRef part, String sNodeName, String sNamespace, boolean bNillable, XSComplexTypeDefinition typeDef)
      {
         m_partRef = part;
         m_sNodeName = sNodeName;
         m_sNamespace = sNamespace;
         m_typeDef = typeDef;
         m_bNillable = bNillable;

         XSDMessageImporter.this.incrementMessageRefCount(typeDef);
      }
      
      public final void fixup()
      {
         Message msg = XSDMessageImporter.this.getRootMessage(m_typeDef);

         m_partRef.setRefPart(msg.getRoot());

         if (msg.getRoot().getDescription() != null)
         {
            if (m_partRef.getDescription() == null)
            {
               m_partRef.setDescription(msg.getRoot().getDescription());
            }
            else
            {
               m_partRef.setDescription(m_partRef.getDescription() + "\r" + msg.getRoot().getDescription());
            }
         }

         convertToMessageInstance(msg);
      }
      
      private void convertToMessageInstance(Message msg)
      {
         if (!XSDMessageImporter.this.m_bCreateMessageForSingleReference)
         {
            if (((Integer)XSDMessageImporter.this.getMessageRegistry().m_msgRefCountMap.get(MessageRegistry.toKey(m_typeDef))).intValue() == 1)
            {
               if (m_partRef.getParent() != null && !m_partRef.isRefPartAncestor())  //when parent is null, we have several elements deriving off the same complex type
               {
                  CompositeMessagePart part = m_partRef.getRefPart();
                  part.copyAttributesFrom(m_partRef);
                  XSDMessageImporter.this.setMapping(part, null, m_sNodeName, m_sNamespace, m_bNillable, m_typeDef);
                  m_partRef.getParent().replacePart(m_partRef, part);
                  XSDMessageImporter.this.m_msgList.remove(msg);
               }
            }
         }
      }
   }

   private static class WSDLBindingsManager
   {
      private Lookup m_portTypeMap; //Element[String]
      private Lookup2D m_bindingSoapActionMap; //String[Element][String]
      private Lookup m_messageMap; //Element[String]
      
      private final Lookup m_bindingMap = new HashTab(); //Element[Object]
      
      public WSDLBindingsManager(Element wsdlDocElement)
      {
         XMLUtil.forEachElementRecur(wsdlDocElement, null, new ElementHandler()
         {
            public void handleElement(final Element element)
            {
               if (NAMESPACE_WSDL.equals(element.getNamespaceURI()))
               {
                  String localName = element.getLocalName();
                  
                  if ("binding".equals(localName))
                  {
                     XMLUtil.forEachChildElement(element, null, new ElementHandler()
                     {
                        public void handleElement(Element inner)
                        {
                           if ("binding".equals(inner.getLocalName()))
                           {
                              if (NAMESPACE_SOAP.equals(inner.getNamespaceURI()))
                              {
                                 m_bindingMap.put(element, Primitive.createInteger(RootXMLMessagePartMapping.ENVELOPE_SOAP));
                              }
                              else if (NAMESPACE_SOAP12.equals(inner.getNamespaceURI()))
                              {
                                 m_bindingMap.put(element, Primitive.createInteger(RootXMLMessagePartMapping.ENVELOPE_SOAP12)); 
                              }
                           }
                        }
                     });
                     
                     return;
                  }
                  else if ("portType".equals(localName))
                  {
                     if (m_portTypeMap == null)
                     {
                        m_portTypeMap = new HashTab();
                     }
                     
                     m_portTypeMap.put(element.getAttribute("name"), element);
                  }
                  else if ("message".equals(localName))
                  {
                     if (m_messageMap == null)
                     {
                        m_messageMap = new HashTab();
                     }
                     
                     m_messageMap.put(element.getAttribute("name"), element);
                  }
                  
                  return;
               }
               else if ((NAMESPACE_SOAP.equals(element.getNamespaceURI()) || NAMESPACE_SOAP12.equals(element.getNamespaceURI())) &&
                  "operation".equals(element.getLocalName()))
               {
                  if (element.getParentNode() != null && element.getParentNode().getParentNode() != null)
                  {
                     if (m_bindingSoapActionMap == null)
                     {
                        m_bindingSoapActionMap = new HashTab2D();
                     }
                     
                     m_bindingSoapActionMap.put(element.getParentNode().getParentNode(), parentName(element), element.getAttribute("soapAction"));
                  }
                     
               }
               
               return;
            }
         });

      }
      
      public Element getPortType(Element binding)
      {
         Element portType = null;
         
         if (m_portTypeMap != null)
         {
            String type = binding.getAttribute("type");
            
            if (type.length() > 0)
            {
               portType = (Element)m_portTypeMap.get(stripNamespacePrefix(type));
            }
         }
         
         return portType;
      }
      
      public byte getEnvelope(Element binding)
      {
         return ((Integer)m_bindingMap.get(binding)).byteValue();
      }
      
      public Iterator getBindingIterator()
      {
         return m_bindingMap.iterator();
      }
      
      private String stripNamespacePrefix(String value)
      {
         int find = value.indexOf(':');
         
         if (find >= 0)
         {
            return value.substring(find + 1);
         }
         
         return value;
      }

      public String parentName(Element element)
      {
         return ((Element)element.getParentNode()).getAttribute("name");
      }
      
      public Message[] parseInputOutputMessages(Element portType, final String fOperationName, final List msgList)
      {
         final Message fRetValue[] = new Message[2];

         if (m_messageMap != null)
         {
            XMLUtil.forEachElementRecur(portType, null, new ElementHandler()
            {               
               public void handleElement(Element element)
               {
                  if ("input".equals(element.getLocalName()))
                  {
                     if (parentName(element).equals(fOperationName))
                     {
                        fRetValue[0] = parseMessage(element);
                     }
                  }
                  else if ("output".equals(element.getLocalName()))
                  {
                     if (parentName(element).equals(fOperationName))
                     {
                        fRetValue[1] = parseMessage(element);
                     }
                  }
               }
               
               private Message parseMessage(Element element)
               {
                  Element wsdlMessage = (Element)m_messageMap.get(stripNamespacePrefix(element.getAttribute("message")));
                  
                  if (wsdlMessage != null)
                  {
                     final Message fRetVal[] = new Message[1];
                     
                     XMLUtil.forEachChildElement(wsdlMessage, null, new ElementHandler()
                     {
                        public void handleElement(Element element)
                        {
                           if ("part".equals(element.getLocalName()))
                           {
                              String elementName = stripNamespacePrefix(element.getAttribute("element"));
                              
                              if (elementName.length() > 0)
                              {
                                 for (Iterator itr = msgList.iterator(); itr.hasNext();)
                                 {
                                    Message msg = (Message)itr.next();
                                    
                                    if (elementName.equals(((XMLMessagePartMapping)msg.getRoot().getMapping()).getNodeName()))
                                    {
                                       fRetVal[0] = msg;
                                    }
                                 }
                              }
                           }
                        }
                     });
                     
                     return fRetVal[0];
                  }
                  
                  return null;
               }
            });
         }
         
         return fRetValue;
      }

      public String getSoapAction(Element binding, String operationName)
      {
         return (String)m_bindingSoapActionMap.get(binding, operationName);
      }
      
      public Iterator operationIterator(Element parent)
      {
         final List fList = new ArrayList(); 
         
         XMLUtil.forEachChildElement(parent, null, new ElementHandler()
         {
            public void handleElement(Element element)
            {
               if ("operation".equals(element.getLocalName()))
               {
                  fList.add(element);
               }
            }
         });
         
         return fList.iterator();
      }
   }
   
   public interface IXSModelHandler
   {
      /**
       * @return true to stop iteration.
       */
      public boolean handle(XSModel xsModel, Document wsdlDoc) throws IOException;
   }
   
   public static class MessageRegistry
   {
      protected final HashTab m_msgMap = new HashTab();
      protected Set m_elemKeySet = new HashHolder();
      private Lookup m_msgRefCountMap;
      
      public void put(XSComplexTypeDefinition typeDef, Message msg)
      {
         m_msgMap.put(toKey(typeDef), msg);
      }
      
      public Message get(XSComplexTypeDefinition typeDef)
      {
         return (Message)m_msgMap.get(toKey(typeDef));
      }

      protected static Object toKey(XSElementDeclaration elemDecl)
      {
         return new Pair(elemDecl.getNamespace(), elemDecl.getName());
      }
      
      public boolean register(XSElementDeclaration elemDecl)
      {
         return m_elemKeySet.add(toKey(elemDecl));
      }
      
      protected static Object toKey(XSComplexTypeDefinition typeDef)
      {
         if (typeDef.getAnonymous()) 
         {
            return typeDef;
         }
         else
         {
            return new Pair(typeDef.getNamespace(), typeDef.getName());
         }
      }
      
      public void incrementRefCount(XSComplexTypeDefinition typeDef)
      {
         int nRefCount = 1;
         Object key = toKey(typeDef);
         
         if (m_msgRefCountMap == null)
         {
            m_msgRefCountMap = new HashTab();
         }
         else
         {
            Integer store = (Integer)m_msgRefCountMap.get(key);
            
            if (store != null)
            {
               nRefCount = store.intValue() + 1;
            }
         }

         m_msgRefCountMap.put(key, Primitive.createInteger(nRefCount));
      }
   }
}
