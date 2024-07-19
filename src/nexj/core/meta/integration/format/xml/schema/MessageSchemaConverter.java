// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.util.Iterator;

import nexj.core.integration.IntegrationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLNamespace;
import nexj.core.meta.integration.service.Interface;
import nexj.core.util.Lookup;

/**
 * Converts integration messages to schemas.
 */
public class MessageSchemaConverter
{
   // attributes

   /**
    * The URI of the namespace currently being exported.
    */
   protected String m_sNamespaceURI;

   // associations

   /**
    * The universe of schemas.
    */
   protected SchemaUniverse m_universe;

   /**
    * The envelope header map partName -> headerElement.
    */
   protected Lookup m_envelopeHeaderMap; // of type String -> Element

   // constructors

   /**
    * Constructs a new integration message to schema converter.
    * @param universe The universe in which the schemas will be defined.
    */
   public MessageSchemaConverter(SchemaUniverse universe)
   {
      m_universe = universe;
   }

   // operations

   /**
    * Exports a message, adding it to the universe of schemas. Creates a composite type
    * for the message, as well as an element of that type.
    * @param message The message to export.
    * @return The root element that was created.
    */
   public Element add(Message message)
   {
      if (m_envelopeHeaderMap != null)
      {
         m_envelopeHeaderMap.clear();
      }

      CompositeMessagePart rootPart = message.getRoot();
      CompositeType type = createCompositeType(rootPart, message);
      String sName = ((XMLMessagePartMapping)rootPart.getMapping()).getNodeName();
      Element msgElement = (Element)type.getSchema().findItem(sName, SchemaItem.ELEMENT);

      if (msgElement == null)
      {
         msgElement = new Element(sName);
         msgElement.setType(type);
         type.getSchema().addItem(msgElement);
      }

      return msgElement;
   }

   /**
    * Creates a composite type definition from a root message part and its (optional) message.
    * The message parameter is used to export inheritance hierarchies.
    * @param part The part to create the type.
    * @param message Optional message, if any, whose root is the part.
    * @return The composite type definition.
    */
   protected CompositeType createCompositeType(CompositeMessagePart part, Message message)
   {
      String sOldNamespaceURI = m_sNamespaceURI;
      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();
      String sName = null;
      boolean bEnvelopeHeaders =
            part == ((RootXMLMessagePartMapping)part.getRoot().getMapping()).getHeaderPart();

      if (message != null)
      {
         assert message.getRoot() == part;

         sName = ((RootXMLMessagePartMapping)mapping).getXSDType();

         if (sName == null)
         {
            sName = message.getName();
         }
      }

      XMLNamespace namespace = mapping.getNamespace();
      Schema schema = (namespace == null) ? m_universe.getSchema(m_sNamespaceURI, null) :
         m_universe.getSchema(namespace.getURI(), namespace.getName());
      CompositeType type;

      if (sName != null)
      {
         CompositeType existingType = (CompositeType)schema.findItem(sName, SchemaItem.COMPOSITE_TYPE);

         if (existingType != null)
         {
            return existingType;
         }

         type = new CompositeType(sName);
         schema.addItem(type);
      }
      else
      {
         type = new CompositeType(sName);
      }

      if (message != null)
      {
         type.setDescription(part.getDescription());
      }

      if (namespace != null)
      {
         m_sNamespaceURI = namespace.getURI();
      }

      switch (part.getAggregation())
      {
         case CompositeMessagePart.SEQUENTIAL:
            type.setAggregation(CompositeType.AGGREGATION_SEQUENTIAL);
            break;

         case CompositeMessagePart.RANDOM:
            type.setAggregation(CompositeType.AGGREGATION_RANDOM);
            break;

         case CompositeMessagePart.SINGLE:
            type.setAggregation(CompositeType.AGGREGATION_CHOICE);
            break;

         default:
            throw new IllegalStateException("Unknown aggregation");
      }

      type.setLax(part.isLax());

      for (int i = 0, nCount = part.getPartCount(); i < nCount; i++)
      {
         MessagePart child = part.getPart(i);

         // Omit parts declared in base message
         if (message != null && child.getDeclarator() != message)
         {
            continue;
         }

         boolean bChildEnvelopeHeaders = child.getRoot() == part &&
            child == ((RootXMLMessagePartMapping)mapping).getHeaderPart();
         XMLMessagePartMapping childMapping = (XMLMessagePartMapping)child.getMapping();

         if (child instanceof PrimitiveMessagePart)
         {
            if (bChildEnvelopeHeaders)
            {
               continue;
            }

            if (childMapping.getNodeType() == XMLMessagePartMapping.VALUE)
            {
               type.setValueType(getAtomicType((PrimitiveMessagePart)child));
            }
            else
            {
               Markup markup = createMarkup((PrimitiveMessagePart)child);

               if (!isEnvelopeMarkup(markup))
               {
                  type.addChild(markup);
               }

               if (bEnvelopeHeaders)
               {
                  addEnvelopeHeader(child, (Element)markup, false);
               }
            }
         }
         else if (child instanceof CompositeMessagePart)
         {
            CompositeType childType;
            Element childElement;

            if (child instanceof CompositeMessagePartRef)
            {
               CompositeMessagePart referentPart = ((CompositeMessagePartRef)child).getRefPart();
               Message referentMessage = ((RootXMLMessagePartMapping)referentPart.getMapping()).getRootMessage();

               childType = createCompositeType(referentPart, referentMessage);
               childElement = new Element(childMapping.getNodeName());
               childElement.setType(childType);
               childElement.setDescription(child.getDescription());

               if (bEnvelopeHeaders)
               {
                  addEnvelopeHeader(child, childElement, true);
               }
            }
            else
            {
               XMLNamespace childNamespace = childMapping.getNamespace();
               String sURI = (childNamespace == null) ? null : childNamespace.getURI();

               if (sURI != null && !sURI.equals(m_sNamespaceURI))
               {
                  // Element reference (different namespace)
                  String sCurrentNamespace = m_sNamespaceURI;
                  Schema otherNSSchema = m_universe.getSchema(sURI, childNamespace.getName());

                  m_sNamespaceURI = sURI;
                  childType = createCompositeType((CompositeMessagePart)child, null);

                  Element otherNSElement = new Element(childMapping.getNodeName());

                  setElementProperties(otherNSElement, (CompositeMessagePart)child);
                  otherNSElement.setType(childType);
                  otherNSElement.setDescription(child.getDescription());
                  otherNSSchema.addItem(otherNSElement);

                  childElement = new ElementRef(otherNSElement);
                  m_sNamespaceURI = sCurrentNamespace;

                  if (bEnvelopeHeaders)
                  {
                     addEnvelopeHeader(child, childElement, false);
                  }
               }
               else
               {
                  // Anonymous type (same namespace)
                  childType = createCompositeType((CompositeMessagePart)child, null);
                  childElement = new Element(childMapping.getNodeName());
                  childElement.setType(childType);
                  childElement.setDescription(child.getDescription());

                  if (bEnvelopeHeaders)
                  {
                     addEnvelopeHeader(child, childElement, true);
                  }
               }
            }

            setElementProperties(childElement, (CompositeMessagePart)child);

            if (!bChildEnvelopeHeaders && !isEnvelopeMarkup(childElement))
            {
               type.addChild(childElement);
            }
         }
      }

      m_sNamespaceURI = sOldNamespaceURI;

      // Export inheritance hierarchy
      if (message != null)
      {
         // Export derived messages
         if (message.getDerivation() != Message.DERIVATION_FINAL)
         {
            for (int i = 0, nCount = message.getDerivedMessageCount(); i < nCount; i++)
            {
               Message derived = message.getDerivedMessage(i);
               CompositeType derivedType = createCompositeType(derived.getRoot(), derived);

               derivedType.setBase(type);
            }
         }

         // Export base message
         Message base = message.getBaseMessage();

         if (type.getBase() == null && base != null)
         {
            CompositeType baseType = createCompositeType(base.getRoot(), base);

            type.setBase(baseType);
         }

         type.setAbstract(message.getDerivation() == Message.DERIVATION_ABSTRACT);
         type.setBlocked(message.getDerivation() == Message.DERIVATION_FINAL);
      }

      return type;
   }

   /**
    * Checks whether markup is in an envelope namespace.
    * @param markup Markup.
    * @return Whether markup is in an envelope namespace.
    */
   protected boolean isEnvelopeMarkup(Markup markup)
   {
      while (markup instanceof ElementRef)
      {
         markup = ((ElementRef)markup).getReferent();
      }

      while (markup instanceof AttributeRef)
      {
         markup = ((AttributeRef)markup).getReferent();
      }

      if (markup != null)
      {
         Schema schema = markup.getSchema();

         if (schema != null)
         {
            String sURI = schema.getURI();

            if (sURI.equals(XMLNamespace.SOAP) || sURI.equals(XMLNamespace.SOAP12))
            {
               return true;
            }
         }
      }

      return false;
   }

   /**
    * Add envelope header to the envelope header list.
    * @param part Envelope header part.
    * @param element Envelope header element.
    * @param bAddToSchema Whether to add element to schema.
    */
   protected void addEnvelopeHeader(MessagePart part, Element element, boolean bAddToSchema)
   {
      if (m_envelopeHeaderMap != null)
      {
         if (bAddToSchema)
         {
            Schema schema = element.getType().getSchema();

            if (schema == null)
            {
               throw new IntegrationException("err.integration.format.missingEnvelopeHeaderNS",
                     new Object[]{part});
            }

            schema.addItem(element);
         }

         m_envelopeHeaderMap.put(part.getName(), element);
      }
   }

   /**
    * Set envelope header map.
    * @param headerList Envelope header list.
    */
   public void setEnvelopeHeaderMap(Lookup headerList)
   {
      m_envelopeHeaderMap = headerList;
   }

   /**
    * Creates a markup definition from a primitive part.
    * @param part The part.
    * @return The markup definition.
    */
   protected Markup createMarkup(PrimitiveMessagePart part)
   {
      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();
      XMLNamespace namespace = mapping.getNamespace();
      String sURI = (namespace == null) ? null : namespace.getURI();

      if (sURI != null && !sURI.equals(m_sNamespaceURI))
      {
         String sCurrentNamespace = m_sNamespaceURI;

         try
         {
            m_sNamespaceURI = sURI;

            if (mapping.getNodeType() == XMLMessagePartMapping.VALUE)
            {
               throw new IllegalStateException();
            }

            byte nType = (mapping.getNodeType() == XMLMessagePartMapping.ELEMENT) ? SchemaItem.ELEMENT : SchemaItem.ATTRIBUTE;
            Schema schema = m_universe.getSchema(sURI, namespace.getName());
            Markup composite = (Markup)schema.findItem(mapping.getNodeName(), nType);

            if (composite == null)
            {
               composite = createMarkup(part);
               schema.addItem(composite);
            }

            if (composite instanceof Element)
            {
               ElementRef ref = new ElementRef((Element)composite);

               setElementProperties(ref, part);

               return ref;
            }

            if (composite instanceof Attribute)
            {
               AttributeRef ref = new AttributeRef((Attribute)composite);

               setAttributeProperties(ref, part);

               return ref;
            }
         }
         finally
         {
            m_sNamespaceURI = sCurrentNamespace;
         }
      }

      if (mapping.getNodeType() == XMLMessagePartMapping.ELEMENT)
      {
         Element element = new Element(mapping.getNodeName());

         setElementProperties(element, part);

         return element;
      }

      if (mapping.getNodeType() == XMLMessagePartMapping.ATTRIBUTE)
      {
         Attribute attribute = new Attribute(mapping.getNodeName());

         setAttributeProperties(attribute, part);

         return attribute;
      }

      throw new IllegalStateException();
   }

   /**
    * Sets an element's properties.
    * @param element The element to set the properties.
    * @param part The message part to get the properties.
    */
   protected static void setElementProperties(Element element, CompositeMessagePart part)
   {
      element.setMaxCount((part.getMaxCount() == Integer.MAX_VALUE) ? 0 : part.getMaxCount());
      element.setMinCount(part.getMinCount());

      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

      element.setNillable(mapping.isNillable());
      element.setQualified(mapping.getNamespace() != null);
   }

   /**
    * Sets an element's properties.
    * @param element The element to set the properties.
    * @param part The message part to get the properties.
    */
   protected void setElementProperties(Element element, PrimitiveMessagePart part)
   {
      element.setMaxCount((part.getMaxCount() == Integer.MAX_VALUE) ? 0 : part.getMaxCount());
      element.setMinCount(part.getMinCount());
      element.setDescription(part.getDescription());

      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

      element.setNillable(mapping.isNillable());

      if (element.getItemType() != SchemaItem.ELEMENT_REF)
      {
         Interface iface = mapping.getInterface();

         if (iface == null)
         {
            element.setType(getAtomicType(part));
         }
         else
         {
            CompositeType type = new CompositeType(null);

            type.setAggregation(CompositeType.AGGREGATION_CHOICE);

            for (Iterator itr = iface.getRequestTable().getMessageIterator(); itr.hasNext(); )
            {
               Message msg = (Message)itr.next();
               CompositeMessagePart rootPart = msg.getRoot();
               XMLMessagePartMapping rootPartMapping = (XMLMessagePartMapping)rootPart.getMapping();

               if (rootPartMapping.getNamespace() != null)
               {
                  CompositeType childType = createCompositeType(rootPart, msg);
                  Element child = new Element(rootPartMapping.getNodeName());

                  setElementProperties(child, rootPart);
                  child.setType(childType);
                  childType.getSchema().addItem(child);

                  ElementRef ref = new ElementRef(child);

                  if (!isEnvelopeMarkup(ref))
                  {
                     type.addChild(ref);
                  }
               }
            }

            element.setType(type);
         }
      }

      XMLNamespace namespace = mapping.getNamespace();

      if (namespace != null)
      {
         assert namespace.getURI().equals(m_sNamespaceURI);

         element.setQualified(true);
      }
   }

   /**
    * Sets an attribute's properties.
    * @param attribute The attribute to set the properties.
    * @param part The message part to get the properties.
    */
   protected void setAttributeProperties(Attribute attribute, PrimitiveMessagePart part)
   {
      attribute.setRequired(part.isRequired());
      attribute.setDescription(part.getDescription());

      if (attribute.getItemType() != SchemaItem.ATTRIBUTE_REF)
      {
         attribute.setType(getAtomicType(part));
      }

      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();
      XMLNamespace namespace = mapping.getNamespace();

      if (namespace != null)
      {
         assert namespace.getURI().equals(m_sNamespaceURI);

         attribute.setQualified(true);
      }
   }

   /**
    * Gets the atomic type of a primitive part.
    * @param part The part to get the type.
    * @return An atomic type definition for the part.
    */
   protected static AtomicType getAtomicType(PrimitiveMessagePart part)
   {
      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();
      int nCount = part.getEnumerationCount();
      AtomicType type = getPrimitiveType(part);

      if (mapping.getFormat() != null)
      {
         FormatType fmt = new FormatType(null, type);

         fmt.setFormat(mapping.getFormat());
         type = fmt;
      }

      if (nCount > 0)
      {
         EnumType e = new EnumType(null, type);

         for (Iterator itr = part.getEnumerationIterator(); itr.hasNext(); )
         {
            e.addValue(itr.next());
         }

         type = e;
      }

      return type;
   }

   /**
    * Gets the primitive type representing the base type of the given primitive part.
    * @param part The primitive part to get the type.
    * @return The type.
    */
   protected static PrimitiveType getPrimitiveType(PrimitiveMessagePart part)
   {
      Primitive type = part.getType();
      XMLMessagePartMapping mapping;

      switch (type.getOrdinal())
      {
         case Primitive.TIMESTAMP_ORDINAL:
            mapping = (XMLMessagePartMapping)part.getMapping();

            switch (mapping.getSubtype())
            {
               case XMLMessagePartMapping.SUBTYPE_DATE:
                  return PrimitiveType.DATE;

               case XMLMessagePartMapping.SUBTYPE_TIME:
                  return PrimitiveType.TIME;

               case XMLMessagePartMapping.SUBTYPE_DATETIME:
                  return PrimitiveType.DATETIME;

               default:
                  throw new IllegalStateException("Unknown subtype");
            }

         case Primitive.BINARY_ORDINAL:
            mapping = (XMLMessagePartMapping)part.getMapping();

            switch (mapping.getSubtype())
            {
               case XMLMessagePartMapping.SUBTYPE_BASE64:
                  return PrimitiveType.BASE64;

               case XMLMessagePartMapping.SUBTYPE_HEX:
                  return PrimitiveType.HEX;

               default:
                  throw new IllegalStateException("Unknown subtype");
            }

         case Primitive.ANY_ORDINAL:
            return PrimitiveType.ANY;

         case Primitive.BOOLEAN_ORDINAL:
            return PrimitiveType.BOOLEAN;

         case Primitive.DECIMAL_ORDINAL:
            return PrimitiveType.DECIMAL;

         case Primitive.DOUBLE_ORDINAL:
            return PrimitiveType.DOUBLE;

         case Primitive.FLOAT_ORDINAL:
            return PrimitiveType.FLOAT;

         case Primitive.INTEGER_ORDINAL:
            return PrimitiveType.INTEGER;

         case Primitive.LONG_ORDINAL:
            return PrimitiveType.LONG;

         case Primitive.STRING_ORDINAL:
            return PrimitiveType.STRING;

         default:
            throw new IllegalStateException("Unknown primitive");
      }
   }
}
