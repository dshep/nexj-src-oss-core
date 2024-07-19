// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.net.URL;
import java.util.Iterator;
import java.util.Set;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.rpc.xml.XML;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.LookupDeque;

/**
 * The XML message part mapping for the root message part.
 * Contains the namespace definitions.
 */
public class RootXMLMessagePartMapping extends XMLMessagePartMapping
{
   // constants

   /**
    * The namespace URI representing no namespace.
    */
   protected final static String NO_NAMESPACE = "";

   /**
    * Plain XML message.
    */
   public final static byte ENVELOPE_NONE = 1;

   /**
    * SOAP 1.1 envelope.
    */
   public final static byte ENVELOPE_SOAP = 2;

   /**
    * SOAP 1.2 envelope.
    */
   public final static byte ENVELOPE_SOAP12 = 4;

   // attributes

   /**
    * The SOAP action.
    */
   protected String m_sAction;

   /**
    * The WSDL operation name.
    */
   protected String m_sOperation;

   /**
    * The XSD type name of this message's root element.
    */
   protected String m_sXSDType;

   /**
    * The envelope type, one of the ENVELOPE_* constants.
    */
   protected byte m_nEnvelope = ENVELOPE_NONE;

   // associations

   /**
    * The message of this part.
    */
   protected final Message m_message;

   /**
    * The namespace map: XMLNamespace[String].
    */
   protected Lookup m_namespaceMap = new HashTab(4);

   /**
    * The map of schema URLs to local URLs. When the schemas are being processed,
    * this map will be used to resolve any imported or included schemas to their
    * local URLs.
    */
   protected LookupDeque m_schemaResourceLookupDeque;

   /**
    * The envelope header part.
    */
   protected MessagePart m_headerPart;

   /**
    * xsdType to Message map. Populated only on the mapping on the root base message
    * in an inheritance hierarchy.
    */
   protected Lookup2D m_xsdTypeMessageMap;

   /**
    * Map of reserved namespace name to URI: String[String].
    */
   protected final static Lookup s_reservedNamespaceMap = new HashTab(7); 

   static
   {
      s_reservedNamespaceMap.put(XML.NS_TAG_XML, XML.NS_URI_XML);
      s_reservedNamespaceMap.put(XMLNamespace.XSD_NAME, XMLNamespace.XSD);
      s_reservedNamespaceMap.put(XMLNamespace.XSI_NAME, XMLNamespace.XSI);
      s_reservedNamespaceMap.put(XMLNamespace.SOAP_NAME, XMLNamespace.SOAP);
      s_reservedNamespaceMap.put(XMLNamespace.SOAP12_NAME, XMLNamespace.SOAP12);
      s_reservedNamespaceMap.put(XMLNamespace.WSDL_NAME, XMLNamespace.WSDL);
      s_reservedNamespaceMap.put(XMLNamespace.WSDL_SOAP_NAME, XMLNamespace.WSDL_SOAP);
   }

   // constructors

   public RootXMLMessagePartMapping(Message root)
   {
      m_message = root;
   }
   
   // operations

   /**
    * Adds a namespace to the mapping or looks up an existing one.
    * @param sName The namespace name.
    * @param sURI The namespace URI.
    * @param sSchema The namespace schema document URL, if any.
    * @param bDeclaredByEnvelope True iff the namespace must be marked as being declared by the envelope.
    * @return The XML namespace.
    * @throws MetadataException if a namespace
    * with the same name but with different URL already exists.
    */
   public XMLNamespace addNamespace(String sName, String sURI, String sSchema, boolean bDeclaredByEnvelope)
   {
      verifyNotReadOnly();

      XMLNamespace namespace = (XMLNamespace)m_namespaceMap.get(sName);

      if (namespace != null)
      {
         String sNamespaceURI = namespace.getURI();

         if (sNamespaceURI == null && sURI != null)
         {
            namespace.setURI(sURI);
            namespace.setSchema(sSchema);
         }

         if (sNamespaceURI != null && sURI != null && !sNamespaceURI.equals(sURI))
         {
            throw new MetadataException("err.meta.integration.xml.namespaceDup",
               new Object[]{sName, getRootMessage().getRoot().getFullPath()});
         }

         if (bDeclaredByEnvelope) 
         {
            namespace.setDeclaredByEnvelope(true);
         }

         return namespace;
      }

      String sReservedURI = (String)s_reservedNamespaceMap.get(sName);

      if (sURI != null)
      {
         if (sReservedURI != null && !sURI.equals(sReservedURI))
         {
            throw new MetadataException("err.meta.integration.xml.reservedNamespaceRedefinition",
               new Object[]{sName, getRootMessage().getRoot().getFullPath()});
         }
      }
      else
      {
         sURI = sReservedURI;
      }

      namespace = new XMLNamespace(sName, sURI, sSchema, bDeclaredByEnvelope);
      m_namespaceMap.put(sName, namespace);

      return namespace;
   }

   /**
    * Adds a new namespace to the mapping.
    * @param namespace The namespace to add.
    * @throws MetadataException if a namespace
    * with the same name already exists.
    */
   public void addNamespace(XMLNamespace namespace)
   {
      verifyNotReadOnly();

      Object oldNamespace = m_namespaceMap.put(namespace.getName(), namespace);

      if (oldNamespace != null)
      {
         if (namespace.isDeclaredByEnvelope())
         {
            ((XMLNamespace)oldNamespace).setDeclaredByEnvelope(true);
         }
         
         m_namespaceMap.put(namespace.getName(), oldNamespace);

         throw new MetadataException("err.meta.integration.xml.namespaceDup", new Object[]
         {
            namespace.getName(),
            getRootMessage().getRoot().getFullPath()
         });
      }
   }

   /**
    * Gets the URI of a reserved (well-known) namespace by name.
    * @param sName The namespace name.
    * @return The namespace URI.
    * @throws MetadataLookupException if the namespace does not exist.
    */
   public String getReservedNamespaceURI(String sName)
   {
      String sURI = (String)s_reservedNamespaceMap.get(sName);

      if (sURI != null)
      {
         return sURI;
      }

      throw new MetadataLookupException("err.meta.integration.xml.namespaceLookup", sName, getRootMessage());
   }

   /**
    * Gets a namespace by name.
    * @param sName The namespace name.
    * @return The namespace object.
    * @throws MetadataLookupException if the namespace does not exist.
    */
   public XMLNamespace getNamespace(String sName)
   {
      XMLNamespace namespace = findNamespace(sName);

      if (namespace != null)
      {
         return namespace;
      }

      throw new MetadataLookupException("err.meta.integration.xml.namespaceLookup", sName, getRootMessage());
   }

   /**
    * Gets a namespace by name.
    * @param sName The namespace name.
    * @return The namespace object; null if not found.
    */
   public XMLNamespace findNamespace(String sName)
   {
      return (XMLNamespace)m_namespaceMap.get(sName);
   }

   /**
    * @return The namespace count.
    */
   public int getNamespaceCount()
   {
      return m_namespaceMap.size();
   }

   /**
    * @return An iterator for the contained namespace objects.
    */
   public Iterator getNamespaceIterator()
   {
      return m_namespaceMap.valueIterator();
   }

   /**
    * Adds a mapping from a schema URL to a local URL. When a schema is imported or
    * included, this mapping will be used to take the schema from a local file. If
    * there is already a mapping for a given source URL, then it is not replaced.
    * @param sSourceURL The source URL to match.
    * @param destination The URL to the local file.
    */
   public void addSchemaResourceMapping(String sSourceURL, URL destination)
   {
      verifyNotReadOnly();

      if (m_schemaResourceLookupDeque == null)
      {
         m_schemaResourceLookupDeque = new LinkedHashTab();
      }

      Object oldDestination = m_schemaResourceLookupDeque.put(sSourceURL, destination);

      if (oldDestination != null)
      {
         m_schemaResourceLookupDeque.put(sSourceURL, oldDestination);
      }
   }

   /**
    * @return The map of schema URLs to local URLs.
    */
   public LookupDeque getSchemaResourceMap()
   {
      return m_schemaResourceLookupDeque;
   }

   /**
    * @return the root message
    */
   public Message getRootMessage()
   {
      return m_message;
   }

   /**
    * Sets the SOAP action.
    * @param sAction The SOAP action to set.
    */
   public void setAction(String sAction)
   {
      verifyNotReadOnly();
      m_sAction = sAction;
   }

   /**
    * @return The SOAP action.
    */
   public String getAction()
   {
      return m_sAction;
   }

   /**
    * Sets the WSDL operation name.
    * @param sOperation The WSDL operation name to set.
    */
   public void setOperation(String sOperation)
   {
      verifyNotReadOnly();
      m_sOperation = sOperation;
   }

   /**
    * @return The WSDL operation name; null if not set.
    */
   public String getOperation()
   {
      return m_sOperation;
   }

   /**
    * Sets the XSD type name of this message's root element.
    * @param sXSDType The XSD type name of the root element.
    */
   public void setXSDType(String sXSDType)
   {
      verifyNotReadOnly();
      m_sXSDType = sXSDType;
   }

   /**
    * Gets the XSD type name of this message's root element.
    * @return The XSD type name of the root element.
    */
   public String getXSDType()
   {
      return m_sXSDType;
   }

   /**
    * Adds a mapping for the type "sNamespaceURI:sXSDType" to message, on the
    * root of the message inheritance hierarchy.
    * @param sNamespaceURI The namespace of the type; null if none.
    * @param sXSDType The type name.
    * @param message The message.
    */
   public void addXSDTypeMessage(String sNamespaceURI, String sXSDType, Message message)
   {
      verifyNotReadOnly();
      assert message != null;

      if (sNamespaceURI == null)
      {
         sNamespaceURI = NO_NAMESPACE;
      }

      Message rootBaseMessage = m_message.getRootBaseMessage();
      RootXMLMessagePartMapping rsmMapping = (RootXMLMessagePartMapping)rootBaseMessage.getRoot().getMapping();

      if (rsmMapping.m_xsdTypeMessageMap == null)
      {
         rsmMapping.m_xsdTypeMessageMap = new HashTab2D();
      }

      Message oldMessage = (Message)rsmMapping.m_xsdTypeMessageMap.put(sNamespaceURI, sXSDType, message);

      if (oldMessage != null && message != oldMessage)
      {
         rsmMapping.m_xsdTypeMessageMap.put(sNamespaceURI, sXSDType, oldMessage);

         throw new MetadataException("err.meta.integration.xml.duplicateXSDTypeMapping", new Object[]
         {
            sNamespaceURI, sXSDType, message, oldMessage
         });
      }
   }

   /**
    * Looks in the root of the message inheritance hierarchy for the message that
    * represents the type "sNamespaceURI:sXSDType".
    * @param sNamespaceURI The namespace of the type; null if none.
    * @param sXSDType The type name.
    * @return The message; null if not found.
    */
   public Message getXSDTypeMessage(String sNamespaceURI, String sXSDType)
   {
      if (sNamespaceURI == null)
      {
         sNamespaceURI = NO_NAMESPACE;
      }

      Message rootBaseMessage = m_message.getRootBaseMessage();
      RootXMLMessagePartMapping rsmMapping = (RootXMLMessagePartMapping)rootBaseMessage.getRoot().getMapping();

      if (rsmMapping.m_xsdTypeMessageMap == null)
      {
         return null;
      }

      return (Message)rsmMapping.m_xsdTypeMessageMap.get(sNamespaceURI, sXSDType);
   }

   /**
    * Sets the envelope type, one of the ENVELOPE_* constants.
    * @param nEnvelope The envelope type, one of the ENVELOPE_* constants to set.
    */
   public void setEnvelope(byte nEnvelope)
   {
      verifyNotReadOnly();
      m_nEnvelope = nEnvelope;
      
      switch (m_nEnvelope)
      {
         case ENVELOPE_SOAP:
            addNamespace(XMLNamespace.SOAP_NAME, XMLNamespace.SOAP, null, true);
            addNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI, null, true);
            break;

         case ENVELOPE_SOAP12:
            addNamespace(XMLNamespace.SOAP12_NAME, XMLNamespace.SOAP12, null, true);
            addNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI, null, true);
            break;
      }
   }

   /**
    * @return The envelope type, one of the ENVELOPE_* constants.
    */
   public byte getEnvelope()
   {
      return m_nEnvelope;
   }

   /**
    * Gets the envelope header part.
    * 
    * @return The envelope header part; null if no header specified.
    */
   public MessagePart getHeaderPart()
   {
      return m_headerPart;
   }

   /**
    * Sets the envelope header part.
    * 
    * @param part The envelope header part to set; null for no header.
    */
   public void setHeaderPart(MessagePart part)
   {
      verifyNotReadOnly();
      m_headerPart = part;
   }
   
   /**
    * @see nexj.core.meta.integration.format.xml.XMLMessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      verifyNotReadOnly();

      Message baseMessage = m_message.getBaseMessage();

      for (Iterator itr = m_namespaceMap.valueIterator(); itr.hasNext(); )
      {
         XMLNamespace namespace = (XMLNamespace)itr.next();

         // Resolve namespaces with no URI.
         if (namespace.getURI() == null)
         {
            String sName = namespace.getName();
            XMLNamespace uriNamespace = null;

            if (baseMessage != null)
            {
               uriNamespace = ((RootXMLMessagePartMapping)baseMessage.getRoot().getMapping()).findNamespace(
                     sName);
            }

            if (uriNamespace == null)
            {
               throw new MetadataException("err.meta.integration.xml.missingURI",
                  new Object[]{part.getFullPath()});
            }

            namespace.setURI(uriNamespace.getURI());
            namespace.setSchema(uriNamespace.getSchema());
         }

         if (namespace.getSchema() != null)
         {
            addNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI, null, false);

            break;
         }
      }

      // Propagate namespaces from this mapping to the base recursively
      propagateNamespaces();

      super.finish(part);

      if (m_sXSDType != null)
      {
         String sURI = (m_namespace == null) ? null : m_namespace.getURI();

         addXSDTypeMessage(sURI, m_sXSDType, m_message);
      }

      if (baseMessage != null)
      {
         RootXMLMessagePartMapping baseRootMapping = (RootXMLMessagePartMapping)baseMessage.getRoot().getMapping();

         if (baseRootMapping.m_schemaResourceLookupDeque != null)
         {
            if (m_schemaResourceLookupDeque == null)
            {
               m_schemaResourceLookupDeque = new LinkedHashTab();
            }

            for (Lookup.Iterator itr = baseRootMapping.m_schemaResourceLookupDeque.iterator(); itr.hasNext(); )
            {
               String sSrcURL = (String)itr.next();
               URL dst = (URL)itr.getValue();
               URL oldDst = (URL)m_schemaResourceLookupDeque.put(sSrcURL, dst);

               if (oldDst != null && !oldDst.equals(dst))
               {
                  throw new MetadataException("err.meta.integration.xml.schemaResourceMismatch",
                     new Object[]{sSrcURL, m_message.getName(), baseRootMapping.m_message.getName()});
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.format.xml.XMLMessagePartMapping#refer(nexj.core.meta.integration.CompositeMessagePartRef)
    */
   public void refer(CompositeMessagePartRef ref)
   {
      super.refer(ref);

      // Handle message whose root is a reference to a message with a header
      if (ref.getRoot() == ref)
      {
         setHeaderPart(((RootXMLMessagePartMapping)ref.getRefPart().getMapping()).getHeaderPart());
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      RootXMLMessagePartMapping copy = (RootXMLMessagePartMapping)super.clone();

      copy.m_headerPart = null;
      copy.m_namespaceMap = (Lookup)m_namespaceMap.clone();

      if (m_schemaResourceLookupDeque != null)
      {
         copy.m_schemaResourceLookupDeque = (LookupDeque)m_schemaResourceLookupDeque.clone();
      }

      return copy;
   }

   /**
    * Copies namespaces from base message to derived message.
    * @see nexj.core.meta.integration.format.xml.XMLMessagePartMapping#resolveInheritance(nexj.core.meta.integration.MessagePartMapping)
    */
   public void resolveInheritance(MessagePartMapping baseMapping)
   {
      super.resolveInheritance(baseMapping);

      Message baseMessage = m_message.getBaseMessage();

      if (baseMessage != null)
      {
         RootXMLMessagePartMapping baseRootMapping = (RootXMLMessagePartMapping)baseMessage.getRoot().getMapping();

         for (Iterator itr = baseRootMapping.getNamespaceIterator(); itr.hasNext(); )
         {
            XMLNamespace namespace = (XMLNamespace)itr.next();

            if (findNamespace(namespace.getName()) == null)
            {
               addNamespace(namespace);
            }
         }
      }
   }

   /**
    * Propagate namespaces from this root mapping to root mappings of bases, referrers to bases, bases of
    * referrers and so on.
    */
   protected void propagateNamespaces()
   {
      Message baseMessage = m_message.getBaseMessage();

      if (baseMessage != null && baseMessage.getDerivation() != Message.DERIVATION_FINAL)
      {
         Set visitedSet = new IdentityHashHolder();

         visitedSet.add(this);
         ((RootXMLMessagePartMapping)baseMessage.getRoot().getMapping()).addNamespaces(m_namespaceMap,
               visitedSet);
      }
   }

   /**
    * Recursive step of propagateNamespaces().
    * @param namespaceMap Namespaces to add.
    * @param visitedSet Set of visited root mappings.
    */
   protected void addNamespaces(Lookup namespaceMap, Set visitedSet)
   {
      if (!visitedSet.add(this))
      {
         return;
      }

      for (Iterator itr = namespaceMap.valueIterator(); itr.hasNext(); )
      {
         XMLNamespace namespace = (XMLNamespace)itr.next();

         addNamespace(namespace.getName(), namespace.getURI(), namespace.getSchema(),
               namespace.isDeclaredByEnvelope());
      }

      Message baseMessage = m_message.getBaseMessage();

      if (baseMessage != null && baseMessage.getDerivation() != Message.DERIVATION_FINAL)
      {
         ((RootXMLMessagePartMapping)baseMessage.getRoot().getMapping()).addNamespaces(namespaceMap,
               visitedSet);
      }

      for (Iterator itr = m_message.getReferrerListIterator(); itr.hasNext(); )
      {
         Message message = (Message)itr.next();

         ((RootXMLMessagePartMapping)message.getRoot().getMapping()).addNamespaces(namespaceMap, visitedSet);
      }
   }
}
