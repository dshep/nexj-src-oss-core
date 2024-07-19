// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.net.URL;
import java.util.Iterator;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.FormatHolder;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.service.Interface;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.Lookup;
import nexj.core.util.XMLUtil;

/**
 * XML message part mapping.
 */
public class XMLMessagePartMapping extends MetadataObject implements MessagePartMapping, FormatHolder
{
   // constants

   /**
    * The node is an element.
    */
   public final static byte ELEMENT = 0;

   /**
    * The node is an attribute.
    */
   public final static byte ATTRIBUTE = 1;

   /**
    * The node is the value portion of the element
    */
   public final static byte VALUE = 2;

   /**
    * No specific sub-type.
    */
   public final static byte SUBTYPE_DEFAULT = 0;

   /**
    * Date/Time sub-type of "timestamp".
    */
   public final static byte SUBTYPE_DATETIME = 1;

   /**
    * Date sub-type of "timestamp".
    */
   public final static byte SUBTYPE_DATE = 2;

   /**
    * Time sub-type of "timestamp".
    */
   public final static byte SUBTYPE_TIME = 3;

   /**
    * Base64 sub-type of "binary".
    */
   public final static byte SUBTYPE_BASE64 = 4;

   /**
    * Hex sub-type of "binary".
    */
   public final static byte SUBTYPE_HEX = 5;

   /**
    * XSI sub-type of "any". When selected, outputs the "xsi:type" attribute.
    */
   public final static byte SUBTYPE_XSI = 6;

   /**
    * Sub-type names at indexes corresponding to the sub-type values.
    */
   protected final static String[] SUBTYPE_NAMES = new String[]{"", "dateTime", "date", "time", "base64", "hex", "xsi"};

   // attributes

   /**
    * The XML node name.
    */
   protected String m_sNodeName;

   /**
    * The primitive value format string, if any.
    */
   protected String m_sFormat;

   /**
    * The XML node type.
    */
   protected byte m_nNodeType;
   
   /**
    * The value sub-type.
    */
   protected byte m_nSubtype;

   /**
    * The nillable flag, false by default. True if this element should use
    * XML Schema Instance nil value semantics.
    */
   protected boolean m_bNillable;

   /**
    * Whether the message part has child message parts that are mapped as required attributes.
    */
   protected boolean m_bHasRequiredAttributes;

   // associations

   /**
    * The message part associated with this mapping.
    */
   protected MessagePart m_part;

   /**
    * The sub-part that is mapped to the element value, if any.
    */
   protected PrimitiveMessagePart m_valuePart;

   /**
    * The interface to use for parsing content of type "any". May be null.
    */
   protected Interface m_interface;

   /**
    * The XML node namespace.
    */
   protected XMLNamespace m_namespace;

   // operations

   /**
    * Sets the XML node type.
    * @param nNodeType The XML node type to set.
    */
   public void setNodeType(byte nNodeType)
   {
      verifyNotReadOnly();
      m_nNodeType = nNodeType;
   }

   /**
    * @return The XML node type.
    */
   public byte getNodeType()
   {
      return m_nNodeType;
   }
   
   /**
    * Sets the value sub-type.
    * @param nSubtype The value sub-type to set.
    */
   public void setSubtype(byte nSubtype)
   {
      verifyNotReadOnly();
      m_nSubtype = nSubtype;
   }

   /**
    * @return The value sub-type.
    */
   public byte getSubtype()
   {
      return m_nSubtype;
   }

   /**
    * @return The sub-part that is mapped to the element value; null if no such part.
    */
   public PrimitiveMessagePart getValuePart()
   {
      return m_valuePart;
   }

   /**
    * Sets the nillable flag.
    * @param bNillable True to use XML Schema Instance nil value semantics.
    */
   public void setNillable(boolean bNillable)
   {
      verifyNotReadOnly();
      m_bNillable = bNillable;
   }

   /**
    * Gets the nillable flag.
    * @return True to use XML Schema Instance nil value semantics.
    */
   public boolean isNillable()
   {
      return m_bNillable;
   }

   /**
    * Sets the XML node name.
    * @param sNodeName The XML node name to set.
    */
   public void setNodeName(String sNodeName)
   {
      verifyNotReadOnly();
      m_sNodeName = sNodeName;
   }

   /**
    * @return The XML node name.
    */
   public String getNodeName()
   {
      return m_sNodeName;
   }

   /**
    * Sets the XML node namespace.
    * @param namespace The XML node namespace to set.
    */
   public void setNamespace(XMLNamespace namespace)
   {
      verifyNotReadOnly();
      m_namespace = namespace;
   }

   /**
    * @return The XML node namespace. Can be null.
    */
   public XMLNamespace getNamespace()
   {
      return m_namespace;
   }

   /**
    * @see nexj.core.meta.integration.FormatHolder#setFormat(java.lang.String)
    */
   public void setFormat(String sFormat)
   {
      verifyNotReadOnly();
      m_sFormat = sFormat;
   }

   /**
    * @see nexj.core.meta.integration.FormatHolder#getFormat()
    */
   public String getFormat()
   {
      return m_sFormat;
   }

   /**
    * Sets the interface for parsing content of type "any".
    * @param iface The interface; may be null.
    */
   public void setInterface(Interface iface)
   {
      verifyNotReadOnly();
      m_interface = iface;
   }

   /**
    * Gets the interface for parsing content of type "any".
    * @return The interface; null if unspecified.
    */
   public Interface getInterface()
   {
      return m_interface;
   }

   /**
    * @return The message part associated with this mapping.
    */
   public MessagePart getMessagePart()
   {
      return m_part;
   }
   
   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      String sName = getClass().getName();
      StringBuffer buf = new StringBuffer();

      buf.append(sName.substring(sName.lastIndexOf('.') + 1));
      buf.append("(type=");
      
      switch (m_nNodeType)
      {
         case ELEMENT:
            buf.append("element");
            break;
            
         case ATTRIBUTE:
            buf.append("attribute");
            break;
            
         case VALUE:
            buf.append("value");
            break;
      }

      if (m_sNodeName != null)
      {
         buf.append(", name=");
         buf.append(m_sNodeName);
      }

      if (m_namespace != null)
      {
         buf.append(", ns=");
         buf.append(m_namespace.getName());
         buf.append('<');
         buf.append(m_namespace.getURI());
         buf.append('>');
      }
      
      if (m_nSubtype != SUBTYPE_DEFAULT)
      {
         buf.append(", subtype=");
         buf.append(SUBTYPE_NAMES[m_nSubtype]);
      }

      buf.append(')');

      return buf.toString();
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      verifyNotReadOnly();
      m_part = part;

      CompositeMessagePart parentPart = part.getParent();
      XMLMessagePartMapping parentMapping = (parentPart == null) ? null : (XMLMessagePartMapping)parentPart.getMapping();
      PrimitiveMessagePart primitivePart = null;
      CompositeMessagePart compositePart = null;

      if (part instanceof PrimitiveMessagePart)
      {
         primitivePart = (PrimitiveMessagePart)part;
      }

      if (part instanceof CompositeMessagePart)
      {
         compositePart = (CompositeMessagePart)part;
      }

      switch (m_nNodeType)
      {
         case ATTRIBUTE:
            if (parentPart == null)
            {
               throw new MetadataException("err.meta.integration.xml.rootNotElement",
                  new Object[]{part.getFullPath()});
            }

            if (compositePart != null)
            {
               throw new MetadataException("err.meta.integration.xml.compositeAttribute",
                  new Object[]{part.getFullPath()});
            }

            if (part.isCollection())
            {
               throw new MetadataException("err.meta.integration.xml.collectionAttribute",
                  new Object[]{part.getFullPath()});
            }

            if (m_bNillable)
            {
               throw new MetadataException("err.meta.integration.xml.misplacedNillable",
                  new Object[]{part.getFullPath()});
            }

            break;

         case VALUE:
            if (parentPart == null)
            {
               throw new MetadataException("err.meta.integration.xml.rootNotElement",
                  new Object[]{part.getFullPath()});
            }

            if (compositePart != null)
            {
               throw new MetadataException("err.meta.integration.xml.compositeValue",
                  new Object[]{part.getFullPath()});
            }

            if (part.isCollection() && primitivePart.getType() != Primitive.ANY)
            {
               throw new MetadataException("err.meta.integration.xml.collectionValue",
                  new Object[]{part.getFullPath()});
            }

            if (parentMapping.getValuePart() != null)
            {
               throw new MetadataException("err.meta.integration.xml.duplicateElementValueMapping",
                  new Object[]{part.getFullPath()});
            }

            parentMapping.m_valuePart = primitivePart;

            if (m_bNillable)
            {
               throw new MetadataException("err.meta.integration.xml.misplacedNillable",
                  new Object[]{part.getFullPath()});
            }

            break;

         case ELEMENT:
            if (m_bNillable && part.isRequired())
            {
               throw new MetadataException("err.meta.integration.xml.requiredNillable",
                  new Object[]{part.getFullPath()});
            }

            break;
      }

      if (m_nSubtype != SUBTYPE_DEFAULT && m_sFormat != null)
      {
         throw new MetadataException("err.meta.integration.xml.formatSubtype",
            new Object[]{part.getFullPath()});
      }

      boolean bSubtypeValid;

      if (primitivePart != null)
      {
         switch (primitivePart.getType().getOrdinal())
         {
            case Primitive.BINARY_ORDINAL:
               switch (m_nSubtype)
               {
                  case SUBTYPE_DEFAULT:
                     m_nSubtype = SUBTYPE_BASE64;

                  case SUBTYPE_BASE64:
                  case SUBTYPE_HEX:
                     bSubtypeValid = true;
                     break;

                  default:
                     bSubtypeValid = false;
                     break;
               }

               break;

            case Primitive.TIMESTAMP_ORDINAL:
               switch (m_nSubtype)
               {
                  case SUBTYPE_DEFAULT:
                     m_nSubtype = SUBTYPE_DATETIME;

                  case SUBTYPE_DATE:
                  case SUBTYPE_TIME:
                  case SUBTYPE_DATETIME:
                     bSubtypeValid = true;
                     break;

                  default:
                     bSubtypeValid = false;
                     break;
               }

               break;

            case Primitive.ANY_ORDINAL:
               bSubtypeValid = m_nSubtype == SUBTYPE_DEFAULT || m_nSubtype == SUBTYPE_XSI && m_nNodeType == ELEMENT;
               break;

            default:
               bSubtypeValid = m_nSubtype == SUBTYPE_DEFAULT;
               break;
         }
      }
      else
      {
         bSubtypeValid = m_nSubtype == SUBTYPE_DEFAULT;

         if (m_sFormat != null)
         {
            throw new MetadataException("err.meta.integration.formatComposite",
               new Object[]{part.getFullPath()});
         }
      }

      if (!bSubtypeValid)
      {
         throw new MetadataException("err.meta.integration.xml.inapplicableSubtype",
            new Object[]{SUBTYPE_NAMES[m_nSubtype], part.getFullPath()});
      }

      MessagePart rootPart = part.getRoot();
      RootXMLMessagePartMapping rootMapping = (RootXMLMessagePartMapping)rootPart.getMapping();

      // Ensure XSI namespace is used on message root if nillable elements are used
      if (m_bNillable && rootMapping != null)
      {
         rootMapping.addNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI, null, false);
      }

      // ANY-typed part with polymorphic typing
      if (primitivePart != null && primitivePart.getType() == Primitive.ANY &&
         m_nSubtype == XMLMessagePartMapping.SUBTYPE_XSI)
      {
         rootMapping.addNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI, null, false);
         rootMapping.addNamespace(XMLNamespace.XSD_NAME, XMLNamespace.XSD, null, false);
      }

      // Check to see if this is a SOAP header part
      if (compositePart != null && rootMapping != null)
      {
         if (rootMapping.getEnvelope() != RootXMLMessagePartMapping.ENVELOPE_NONE &&
            getNodeType() == XMLMessagePartMapping.ELEMENT &&
            "Header".equals(getNodeName()) &&
            (part.getParent() == rootPart || part == rootPart))
         {
            XMLNamespace namespace = getNamespace();

            if (namespace != null && namespace.isDeclaredByEnvelope())
            {
               rootMapping.setHeaderPart(part);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#resolveInheritance(nexj.core.meta.integration.MessagePartMapping)
    */
   public void resolveInheritance(MessagePartMapping baseMapping)
   {
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#refer(CompositeMessagePartRef)
    */
   public void refer(CompositeMessagePartRef ref)
   {
      XMLMessagePartMapping mapping = (XMLMessagePartMapping)ref.getRefPart().getMapping();

      m_valuePart = mapping.getValuePart();

      // Copy the namespaces from the referred part mapping to the root mapping.
      RootXMLMessagePartMapping rootMapping = (RootXMLMessagePartMapping)ref.getRoot().getMapping();
      RootXMLMessagePartMapping refRootMapping = (RootXMLMessagePartMapping)ref.getRefPart().getMapping();

      if (rootMapping != null)
      {
         for (Iterator itr = refRootMapping.getNamespaceIterator(); itr.hasNext(); )
         {
            XMLNamespace namespace = (XMLNamespace)itr.next();

            rootMapping.addNamespace(namespace.getName(), namespace.getURI(), namespace.getSchema(), namespace.isDeclaredByEnvelope());
         }

         // Copy namespaces from the referrer to the base recursively
         rootMapping.propagateNamespaces();
      }

      // Copy schema resource mappings from the referent part mapping to the root mapping.
      if (refRootMapping.getSchemaResourceMap() != null)
      {
         for (Lookup.Iterator itr = refRootMapping.getSchemaResourceMap().iterator(); itr.hasNext(); )
         {
            itr.next();
            rootMapping.addSchemaResourceMapping((String)itr.getKey(), (URL)itr.getValue());
         }
      }

      // Handle message where header is a reference
      if (refRootMapping.getHeaderPart() == ref.getRefPart() &&
         ref.getParent() == ref.getRoot() &&
         rootMapping.getHeaderPart() == null)
      {
         rootMapping.setHeaderPart(ref);
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      verifyNotReadOnly();

      assert (part instanceof CompositeMessagePartRef) ||
         ((this instanceof RootXMLMessagePartMapping) == (part.getParent() == null));

      // Add XSI namespace to referrer message if the reference is a polymorphic reference.
      if (part instanceof CompositeMessagePartRef)
      {
         CompositeMessagePartRef ref = (CompositeMessagePartRef)part;
         RootXMLMessagePartMapping referentMapping = (RootXMLMessagePartMapping)ref.getRefPart().getRoot().getMapping();
         Message referentMessage = referentMapping.getRootMessage();

         if (referentMessage.getDerivedMessageCount() > 0 && referentMessage.getDerivation() != Message.DERIVATION_FINAL)
         {
            ((RootXMLMessagePartMapping)part.getRoot().getMapping()).
               addNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI, null, false);
         }
      }

      if (part instanceof CompositeMessagePart)
      {
         boolean bInstance = part instanceof CompositeMessagePartInstance;
         CompositeMessagePart composite = (CompositeMessagePart)part;

         for (int i = 0, nCount = composite.getPartCount(); i < nCount; i++)
         {
            MessagePart child = composite.getPart(i);
            MessagePartMapping mapping = child.getMapping();

            if (mapping != null)
            {
               if (bInstance)
               {
                  mapping.finish(child);
               }

               if (child.isRequired()
                     && ((XMLMessagePartMapping)mapping).getNodeType() == XMLMessagePartMapping.ATTRIBUTE)
               {
                  m_bHasRequiredAttributes = true;
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder, nexj.core.meta.integration.MessagePart)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part)
   {
      if (warnings != null)
      {
         if (!XMLUtil.isValidName(m_sNodeName))
         {
            MetadataValidationException e = new MetadataValidationException("err.meta.integraiton.xml.invalidNodeName",
               new Object[]{m_sNodeName, part.getFullPath()});

            setProperties(e, part);
            warnings.addException(e);
         }
      }
   }

   /**
    * Sets the metadata marker properties.
    * @param marker The destination marker.
    * @param part The message part
    */
   public void setProperties(MetadataMarker marker, MessagePart part)
   {
      marker.setTypeName("Message");
      marker.setProperty("part", part.getName());
      marker.setProperty("message", part.getRoot().getName());
   }

   /**
    * Parses a sub-type name to an ordinal.
    * @param sSubtype The sub-type name.
    * @return The sub-type ordinal; -1 if not found.
    */
   public static byte parseSubtype(String sSubtype)
   {
      for (byte i = 0; i < SUBTYPE_NAMES.length; ++i)
      {
         if (SUBTYPE_NAMES[i].equals(sSubtype))
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * Check if the message part has child message parts that are mapped as required attributes.
    * @return Whether the message part has child message parts that are mapped as required attributes.
    */
   public boolean hasRequiredAttributes()
   {
      return m_bHasRequiredAttributes;
   }
}
