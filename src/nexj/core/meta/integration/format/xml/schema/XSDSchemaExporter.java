// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;

import nexj.core.integration.BooleanFormat;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.rpc.xml.XML;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.LookupDeque;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLWriter;

/**
 * Exports a schema as an XSD (XML Schema 1.0 [WS-I Basic Profile 1.1, XML 1.0]).
 */
public class XSDSchemaExporter implements InvocationContextAware
{
   // constants

   /**
    * Key for checking if a type is overridden.
    */
   protected final static String BASE = "BASE";

   /**
    * Key for checking if a type overrides another type.
    */
   protected final static String DERIVED = "DERIVED";

   /**
    * Orders the items in a schema definition.
    */
   protected final static Comparator ITEM_COMPARATOR = new Comparator()
   {
      public int compare(Object arg0, Object arg1)
      {
         SchemaItem left = (SchemaItem)arg0;
         SchemaItem right = (SchemaItem)arg1;

         int nDiff = getSortOrder(left.getItemType()) - getSortOrder(right.getItemType());

         if (nDiff != 0)
         {
            return nDiff;
         }

         return left.getName().compareToIgnoreCase(right.getName());
      }

      public final byte getSortOrder(byte nType)
      {
         switch (nType)
         {
            case SchemaItem.COMPOSITE_TYPE:
               return 0;

            case SchemaItem.PRIMITIVE_TYPE:
            case SchemaItem.ENUM_TYPE:
            case SchemaItem.FORMAT_TYPE:
               return 1;

            case SchemaItem.ELEMENT:
            case SchemaItem.ELEMENT_REF:
               return 2;

            case SchemaItem.ATTRIBUTE:
            case SchemaItem.ATTRIBUTE_REF:
               return 3;

            default:
               return 4;
         }
      }
   };

   // attributes

   /**
    * True if the output will be embedded in another document (e.g. a WSDL); false if the output should be
    * a stand-alone schema document.
    */
   protected boolean m_bEmbedded;

   // associations

   /**
    * The output writer.
    */
   protected XMLWriter m_writer;

   /**
    * The universe holding the schema(s) to export.
    */
   protected SchemaUniverse m_universe;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * Map of types to flags indicating their override behaviour: Boolean[CompositeType, BASE/DERIVED]
    * 
    * Key BASE: If type t is present in this map, then there exists another type that derives it. Otherwise, t is not present in this map.
    *    If the value is false, then no other type overrides a definition from t.
    *    If the value is true, then at least one type (or sub-type, sub-sub-type, etc.) overrides a definition from t.
    * 
    * Key DERIVED: If type t is present in this map, then it derives another type.
    *    If the value is false, then t does not override any definition from its base.
    *    If the value is true, then t overrides a definition from its base (or base's base, or bases's bases's base, etc.).
    */
   protected Lookup2D m_typeOverrideMap = new HashTab2D();

   /**
    * Temporary map for resolving inheritance of a complex type.
    */
   protected LookupDeque m_childMap = new LinkedHashTab();

   /**
    * Map of common primitive types to their XML Schema equivalents.
    */
   protected final static Lookup s_primitiveTypeMap = new IdentityHashTab(13);

   static
   {
      s_primitiveTypeMap.put(PrimitiveType.ANY, XSDSchema.ANY);
      s_primitiveTypeMap.put(PrimitiveType.BASE64, XSDSchema.BASE64_BINARY);
      s_primitiveTypeMap.put(PrimitiveType.BOOLEAN, XSDSchema.BOOLEAN);
      s_primitiveTypeMap.put(PrimitiveType.DATE, XSDSchema.DATE);
      s_primitiveTypeMap.put(PrimitiveType.DATETIME, XSDSchema.DATETIME);
      s_primitiveTypeMap.put(PrimitiveType.DECIMAL, XSDSchema.DECIMAL);
      s_primitiveTypeMap.put(PrimitiveType.DOUBLE, XSDSchema.DOUBLE);
      s_primitiveTypeMap.put(PrimitiveType.FLOAT, XSDSchema.FLOAT);
      s_primitiveTypeMap.put(PrimitiveType.HEX, XSDSchema.HEX);
      s_primitiveTypeMap.put(PrimitiveType.INTEGER, XSDSchema.INTEGER);
      s_primitiveTypeMap.put(PrimitiveType.LONG, XSDSchema.LONG);
      s_primitiveTypeMap.put(PrimitiveType.STRING, XSDSchema.STRING);
      s_primitiveTypeMap.put(PrimitiveType.TIME, XSDSchema.TIME);
   }

   /**
    * Map of well-known primitive types to their format string exporters: FormatStringExporter[PrimitiveType].
    */
   protected static Lookup s_formatExporterMap = new HashTab(1);

   static
   {
      s_formatExporterMap.put(PrimitiveType.BOOLEAN, new BooleanFormatStringExporter());
   }

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(XSDSchemaExporter.class);

   // constructors

   /**
    * Constructs a new exporter.
    * @param universe The universe holding the schema(s) to export.
    */
   public XSDSchemaExporter(SchemaUniverse universe)
   {
      m_universe = universe;
   }

   /**
    * Constructs a new exporter.
    * @param universe The universe holding the schema(s) to export.
    * @param bEmbedded True if the output will be embedded in another document (e.g. WSDL);
    * false to produce stand-alone output.
    */
   public XSDSchemaExporter(SchemaUniverse universe, boolean bEmbedded)
   {
      m_universe = universe;
      m_bEmbedded = bEmbedded;
   }

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Exports a schema.
    * @param schema The schema to export.
    * @param writer The output writer.
    * @throws IOException If an I/O error occurs.
    */
   public void exportSchema(Schema schema, Writer writer) throws IOException
   {
      init(writer);
      m_universe.addSchema(XSDSchema.XSD);
      m_universe.addSchema(schema);
      m_universe.resolvePrefixes();

      calculateInheritance();

      Set schemaRefSet = getReferences(schema);

      m_writer.openElement("schema");
      m_writer.writeAttribute(XML.XML_NS, XML.XSD_URI);

      String[] sPrefixArray = new String[m_universe.getPrefixCount()];

      ObjUtil.copy(sPrefixArray, m_universe.getPrefixIterator());
      Arrays.sort(sPrefixArray);

      if (!m_bEmbedded)
      {
         m_writer.setNamespace(XML.XML_NS);

         for (int i = 0; i < sPrefixArray.length; i++)
         {
            String sPrefix = sPrefixArray[i];
            String sURI = m_universe.findSchema(sPrefix).getURI();

            m_writer.writeAttribute(sPrefix, sURI);
         }

         m_writer.setNamespace(null);
      }

      if (!StringUtil.isEmpty(schema.getURI()))
      {
         m_writer.writeAttribute("targetNamespace", schema.getURI());
      }

      m_writer.closeElement();

      // Write imports of other namespaces
      schemaRefSet.remove(schema);
      schemaRefSet.remove(XSDSchema.XSD);

      for (int i = 0; i < sPrefixArray.length; i++)
      {
         Schema refSchema = m_universe.findSchema(sPrefixArray[i]);

         if (StringUtil.isEmpty(refSchema.getURI()) || !schemaRefSet.contains(refSchema))
         {
            continue;
         }

         m_writer.openElement("import");
         m_writer.writeAttribute("namespace", refSchema.getURI());
         m_writer.closeEmptyElement();
      }

      // Write definitions
      SchemaItem[] itemArray = new SchemaItem[schema.getItemCount()];

      ObjUtil.copy(itemArray, schema.getItemIterator());
      Arrays.sort(itemArray, ITEM_COMPARATOR);

      for (int i = 0; i < itemArray.length; i++)
      {
         writeSchemaItem(itemArray[i]);
      }

      m_writer.endElement("schema");
   }

   /**
    * Initializes the output writer for export.
    * @param writer The output writer.
    * @throws IOException If an I/O error occurs.
    */
   protected void init(Writer writer) throws IOException
   {
      m_writer = (writer instanceof XMLWriter) ? (XMLWriter)writer : new XMLWriter(writer);
      m_writer.setNamespace(null);

      if (!m_bEmbedded)
      {
         m_writer.write(XML.HEADER);
      }
   }

   /**
    * Writes the XSD corresponding to the schema item definition.
    * @param item The item definition to write.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeSchemaItem(SchemaItem item) throws IOException
   {
      boolean bElementContent = false;
      Attribute attr;
      AttributeRef attrRef;
      Element element;
      ElementRef elementRef;
      Type type;
      CompositeType composite;

      switch (item.getItemType())
      {
         case SchemaItem.ELEMENT:
            element = (Element)item;
            type = element.getType();
            m_writer.openElement("element");
            m_writer.writeAttribute("name", element);

            if (!element.isTopLevel())
            {
               writeMultiplicity(element);
            }

            if (element.isNillable())
            {
               m_writer.writeAttribute("nillable", "true");
            }

            if (element.isQualified() && !element.isTopLevel())
            {
               m_writer.writeAttribute("form", "qualified");
            }

            if (type.getItemType() == SchemaItem.COMPOSITE_TYPE)
            {
               if (type.getName() == null)
               {
                  bElementContent = true;
                  m_writer.closeElement();
                  writeDocumentation(element);
                  writeSchemaItem(type);
               }
               else
               {
                  if (isPolymorphic((CompositeType)type))
                  {
                     m_writer.openAttribute("type");
                     writeQualifiedName(type);
                     m_writer.closeAttribute();
                  }

                  bElementContent |= isDocumented(element);

                  if (bElementContent)
                  {
                     m_writer.closeElement();
                     writeDocumentation(element);
                  }
               }
            }
            else
            {
               bElementContent |= !writeAtomicTypeAttribute((AtomicType)type);
               bElementContent |= isDocumented(element);

               if (bElementContent)
               {
                  m_writer.closeElement();
                  writeDocumentation(element);
                  writeAtomicTypeContent((AtomicType)type);
               }
            }

            if (bElementContent)
            {
               m_writer.endElement("element");
            }
            else
            {
               m_writer.closeEmptyElement();
            }

            break;

         case SchemaItem.ELEMENT_REF:
            elementRef = (ElementRef)item;
            element = elementRef.getReferent();
            m_writer.openElement("element");
            writeMultiplicity(elementRef);
            m_writer.openAttribute("ref");
            writeQualifiedName(element);
            m_writer.closeAttribute();

            if (isDocumented(elementRef))
            {
               m_writer.closeElement();
               writeDocumentation(elementRef);
               m_writer.endElement("element");
            }
            else
            {
               m_writer.closeEmptyElement();
            }

            break;

         case SchemaItem.ATTRIBUTE:
            attr = (Attribute)item;
            m_writer.openElement("attribute");
            m_writer.writeAttribute("name", attr);

            if (attr.isRequired() && !attr.isTopLevel())
            {
               m_writer.writeAttribute("use", "required");
            }

            if (attr.isQualified() && !attr.isTopLevel())
            {
               m_writer.writeAttribute("form", "qualified");
            }

            bElementContent = !writeAtomicTypeAttribute(attr.getAttributeType());
            bElementContent |= isDocumented(attr);

            if (bElementContent)
            {
               m_writer.closeElement();
               writeDocumentation(attr);
            }

            writeAtomicTypeContent(attr.getAttributeType());

            if (bElementContent)
            {
               m_writer.endElement("attribute");
            }
            else
            {
               m_writer.closeEmptyElement();
            }

            break;

         case SchemaItem.ATTRIBUTE_REF:
            attrRef = (AttributeRef)item;
            attr = attrRef.getReferent();
            m_writer.openElement("attribute");

            if (attrRef.isRequired())
            {
               m_writer.writeAttribute("use", "required");
            }

            m_writer.openAttribute("ref");
            writeQualifiedName(attr);
            m_writer.closeAttribute();

            if (isDocumented(attrRef))
            {
               m_writer.closeElement();
               writeDocumentation(attrRef);
               m_writer.endElement("attribute");
            }
            else
            {
               m_writer.closeEmptyElement();
            }

            break;

         case SchemaItem.COMPOSITE_TYPE:
            composite = (CompositeType)item;

            boolean bDerived = isDerived(composite);

            /*
             * Split attributes and elements so they can be dealt with separately.
             * Aggregate with base type(s) taking into account overriding.
             */
            m_childMap.clear();
            addChildrenToMap(composite, m_childMap, Schema.getTypeKey(SchemaItem.ATTRIBUTE), !bDerived);

            int nAttributeCount = m_childMap.size();
            Attribute[] attributeArray = new Attribute[nAttributeCount];

            ObjUtil.copy(attributeArray, m_childMap.valueIterator());

            m_childMap.clear();
            addChildrenToMap(composite, m_childMap, Schema.getTypeKey(SchemaItem.ELEMENT), !bDerived);

            int nElementCount = m_childMap.size();
            Element[] elementArray = new Element[nElementCount];

            ObjUtil.copy(elementArray, m_childMap.valueIterator());

            m_writer.openElement("complexType");
            m_writer.writeAttribute("name", composite);

            if (composite.isAbstract())
            {
               m_writer.writeAttribute("abstract", "true");
            }

            if (composite.isBlocked())
            {
               m_writer.writeAttribute("block", "#all");
            }

            AtomicType valueType = composite.getValueType();
            boolean bWroteSimpleContent = false;

            if (valueType == PrimitiveType.ANY)
            {
               m_writer.writeAttribute("mixed", "true");
               m_writer.closeElement();
               writeDocumentation(composite);
               m_writer.startElement("sequence");
               m_writer.openElement("any");
               m_writer.writeAttribute("processContents", "lax");
               m_writer.writeAttribute("minOccurs", "0");
               m_writer.writeAttribute("maxOccurs", "unbounded");
               m_writer.closeEmptyElement();
               m_writer.endElement("sequence");

               for (int i = 0; i < nAttributeCount; i++)
               {
                  writeSchemaItem(attributeArray[i]);
               }

               m_writer.endElement("complexType");

               break;
            }

            if (valueType != null)
            {
               if (nElementCount > 0)
               {
                  m_writer.writeAttribute("mixed", "true");
                  m_writer.closeElement();
                  writeDocumentation(composite);
               }
               else
               {
                  m_writer.closeElement();
                  writeDocumentation(composite);
                  m_writer.startElement("simpleContent");
                  m_writer.openElement("extension");

                  String sType = getXMLSchemaTypeName(valueType);

                  if (sType != null)
                  {
                     m_writer.writeAttribute("base", sType);
                  }

                  bWroteSimpleContent = true;
                  m_writer.closeElement();
               }
            }
            else if (composite.getBase() != null && bDerived)
            {
               m_writer.closeElement();  // close complexType tag
               writeDocumentation(composite);

               m_writer.startElement("complexContent");
               m_writer.openElement("extension");
               m_writer.openAttribute("base");
               writeQualifiedName(composite.getBase());
               m_writer.closeAttribute();
               m_writer.closeElement();
            }
            else
            {
               m_writer.closeElement();  // close complexType tag
               writeDocumentation(composite);
            }

            if (nElementCount > 0 || composite.isLax())
            {
               writeAggregationAndElements(composite, elementArray, nElementCount);
            }

            for (int i = 0; i < nAttributeCount; i++)
            {
               writeSchemaItem(attributeArray[i]);
            }

            if (composite.getBase() != null && bDerived)
            {
               m_writer.endElement("extension");
               m_writer.endElement("complexContent");
            }
            else if (bWroteSimpleContent)
            {
               m_writer.endElement("extension");
               m_writer.endElement("simpleContent");
            }

            m_writer.endElement("complexType");
            break;

         case SchemaItem.ENUM_TYPE:
            writeAtomicTypeDef((EnumType)item);
            break;

         case SchemaItem.PRIMITIVE_TYPE:
            writeAtomicTypeDef((PrimitiveType)item);
            break;

         default:
            throw new IllegalStateException("Unknown item: " + item);
      }
   }

   /**
    * Writes the XSD for the elements in a complex type.
    * @param composite The complex type to output.
    * @param elementArray The array of elements (may be larger than the number of elements).
    * @param nElementCount The number of elements.
    * @throws IOException If an I/O error occurs.
    */
   private void writeAggregationAndElements(CompositeType composite, Element[] elementArray, int nElementCount)
      throws IOException
   {
      if (composite.getAggregation() == CompositeType.AGGREGATION_SEQUENTIAL)
      {
         m_writer.startElement("sequence");
      }
      else if (composite.getAggregation() == CompositeType.AGGREGATION_RANDOM)
      {
         m_writer.openElement("choice");
         m_writer.writeAttribute("maxOccurs", "unbounded");
         m_writer.closeElement();
      }
      else if (composite.getAggregation() == CompositeType.AGGREGATION_CHOICE)
      {
         m_writer.startElement("choice");
      }
      else
      {
         throw new IllegalStateException("Unsupported aggregation: " + composite.getAggregation());
      }

      for (int i = 0; i < nElementCount; i++)
      {
         writeSchemaItem(elementArray[i]);
      }

      if (composite.isLax())
      {
         m_writer.openElement("any");
         m_writer.writeAttribute("processContents", "skip");
         m_writer.writeAttribute("minOccurs", "0");
         m_writer.writeAttribute("maxOccurs", "unbounded");
         m_writer.closeEmptyElement();
      }

      if (composite.getAggregation() == CompositeType.AGGREGATION_SEQUENTIAL)
      {
         m_writer.endElement("sequence");
      }
      else if (composite.getAggregation() == CompositeType.AGGREGATION_RANDOM)
      {
         m_writer.endElement("choice");
      }
      else if (composite.getAggregation() == CompositeType.AGGREGATION_CHOICE)
      {
         m_writer.endElement("choice");
      }
   }

   /**
    * Writes an atomic type attribute (e.g. 'type="xsd:string'). 
    * @param type The type to write.
    * @return True if the type attribute was written; false if an anonymous type
    * definition must be written instead.
    * @throws IOException If an I/O error occurs.
    */
   protected boolean writeAtomicTypeAttribute(AtomicType type) throws IOException
   {
      // Handle anonymous enumerations by outputting element content
      if ((type.getItemType() == SchemaItem.ENUM_TYPE ||
         type.getItemType() == SchemaItem.FORMAT_TYPE) && type.getName() == null)
      {
         return false;
      }

      // Otherwise, write type reference as an attribute
      String sType = getXMLSchemaTypeName(type);

      if (sType != null)
      {
         m_writer.writeAttribute("type", sType);
      }

      return true;
   }

   /**
    * Writes an anonymous type definition (e.g. <simpleType ...) for an atomic type.
    * @param type The type to write.
    * @return True if the type definition was written; false if a type attribute
    * should be written instead.
    * @throws IOException If an I/O error occurs.
    */
   protected boolean writeAtomicTypeContent(AtomicType type) throws IOException
   {
      // Handle anonymous enumerations by outputting element content
      if ((type.getItemType() == SchemaItem.ENUM_TYPE ||
         type.getItemType() == SchemaItem.FORMAT_TYPE) && type.getName() == null)
      {
         writeAtomicTypeDef(type);

         return true;
      }

      return false;
   }

   /**
    * Writes the qualified name (e.g. "prefix:name") of an item.
    * @param item The item to write the qualified name. Must be a top-level item.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeQualifiedName(SchemaItem item) throws IOException
   {
      assert item.isTopLevel();

      if (StringUtil.isEmpty(item.getSchema().getURI()))
      {
         m_writer.write(item.getName());
      }
      else
      {
         m_writer.write(m_universe.getPrefix(item.getSchema()));
         m_writer.write(':');
         m_writer.write(item.getName());
      }
   }

   /**
    * Gets the XML Schema equivalent of an atomic type.
    * @param type The type.
    * @return Its XML Schema equivalent.
    */
   protected PrimitiveType getXMLSchemaPrimitive(AtomicType type)
   {
      PrimitiveType xsdType = (PrimitiveType)s_primitiveTypeMap.get(type);

      return (xsdType != null) ? xsdType : null;
   }

   /**
    * Gets the qualified name (e.g. "prefix:name") of a type, translating to an XML Schema
    * primitive type first if necessary.
    * @param type The type.
    * @return The qualified name string; null if any type.
    */
   protected String getXMLSchemaTypeName(AtomicType type)
   {
      PrimitiveType xsdType = getXMLSchemaPrimitive(type);

      if (xsdType != null)
      {
         if (xsdType == XSDSchema.ANY)
         {
            return null;
         }

         type = xsdType;
      }

      return m_universe.getQualifiedName(type);
   }

   /**
    * Writes an atomic type definition.
    * @param type The atomic type.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeAtomicTypeDef(final AtomicType type) throws IOException
   {
      FormatStringExporter exporter = null;

      if (type.getItemType() == SchemaItem.FORMAT_TYPE)
      {
         exporter = findFormatStringExporter((PrimitiveType)type.getBase());

         if (exporter == null)
         {
            return;
         }
      }

      if (type.getItemType() == SchemaItem.ENUM_TYPE || type.getItemType() == SchemaItem.FORMAT_TYPE || type.getItemType() == SchemaItem.PRIMITIVE_TYPE)
      {
         m_writer.openElement("simpleType");
         m_writer.writeAttribute("name", type);
         m_writer.closeElement();
         m_writer.openElement("restriction");

         AtomicType baseType = (AtomicType)type.getBase();
         String sType = getXMLSchemaTypeName(baseType);

         if (sType != null)
         {
            m_writer.writeAttribute("base", sType);
         }

         m_writer.closeElement();

         if (type.getItemType() == SchemaItem.ENUM_TYPE)
         {
            EnumType en = (EnumType)type;
            int nCount = en.getValueCount();
            Object[] valueArray = new Object[nCount];

            ObjUtil.copy(valueArray, en.getValueIterator());
            Arrays.sort(valueArray);

            writeEnumerationValues(valueArray, m_writer);
         }
         else if (type.getItemType() == SchemaItem.FORMAT_TYPE)
         {
            FormatType fmt = (FormatType)type;
            String sFormat = PrimitiveFormatter.getFormatPattern(fmt.getFormat(), m_context.getStringTable());

            // Export only if not localized (not found in a string table)
            if (sFormat == fmt.getFormat())
            {
               exporter.export(sFormat, m_writer);
            }
         }

         m_writer.endElement("restriction");
         m_writer.endElement("simpleType");
      }
   }

   /**
    * Exports an array of values as enumeration values.
    * @param valueArray The array of enumeration values to export as strings.
    * @param writer The destination writer.
    * @throws IOException If an I/O error occurs.
    */
   protected static void writeEnumerationValues(Object[] valueArray, XMLWriter writer) throws IOException
   {
      if (valueArray != null)
      {
         for (int i = 0, nCount = valueArray.length; i < nCount; i++)
         {
            writer.openElement("enumeration");
            writer.writeAttribute("value", valueArray[i].toString());
            writer.closeEmptyElement();
         }
      }
   }

   /**
    * Finds a format string exporter for the given type.
    * @param type The type.
    * @return The exporter; null if not found.
    */
   protected FormatStringExporter findFormatStringExporter(PrimitiveType type)
   {
      return (FormatStringExporter)s_formatExporterMap.get(type);
   }

   /**
    * Writes the multiplicity for an element. Expects to be called while
    * an XML Schema start tag is being written.
    * @param element The element to write the multiplicity.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeMultiplicity(Element element) throws IOException
   {
      if (element.getMinCount() != 1)
      {
         m_writer.writeAttribute("minOccurs", element.getMinCount());
      }

      if (element.getMaxCount() == 0)
      {
         m_writer.writeAttribute("maxOccurs", "unbounded");
      }
      else if (element.getMaxCount() != 1)
      {
         m_writer.writeAttribute("maxOccurs", element.getMaxCount());
      }
   }

   /**
    * Checks if an item is documented.
    * @param item The item to check.
    * @return True if the item is documented; false otherwise.
    */
   protected boolean isDocumented(SchemaItem item)
   {
      return StringUtil.trimToNull(item.getDescription()) != null;
   }

   /**
    * Writes documentation for the schema item.
    * @param item The schema item to supply the documentation.
    * @return True if documentation was written; false if nothing was written.
    * @throws IOException If an I/O error occurs.
    */
   protected boolean writeDocumentation(SchemaItem item) throws IOException
   {
      String sDoc = StringUtil.trimToNull(item.getDescription());

      if (sDoc != null)
      {
         m_writer.startElement("annotation");
         m_writer.startElement("documentation");
         m_writer.writeValue(sDoc);
         m_writer.endElement("documentation");
         m_writer.endElement("annotation");

         return true;
      }

      return false;
   }

   /**
    * Gets the set of schemas referenced by the given schema.
    * @param schema The schema to find the references.
    * @return A set of schemas referenced by the given schema.
    */
   public static Set getReferences(Schema schema)
   {
      Set schemaRefSet = new HashHolder();

      for (Iterator itemIterator = schema.getItemIterator(); itemIterator.hasNext(); )
      {
         addReferences((SchemaItem)itemIterator.next(), schemaRefSet, schema);
      }

      return schemaRefSet;
   }

   /**
    * Adds the item's schema references to the set.
    * @param item The item that may reference another schema.
    * @param schemaRefSet The set of referenced schemas.
    * @param schema The schema whose references are being found.
    */
   protected static void addReferences(SchemaItem item, Set schemaRefSet, Schema schema)
   {
      if (item.isTopLevel() && item.getSchema() != schema)
      {
         schemaRefSet.add(item.getSchema());

         return;
      }

      switch (item.getItemType())
      {
         case SchemaItem.ATTRIBUTE_REF:
            schemaRefSet.add(((AttributeRef)item).getReferent().getSchema());
            break;

         case SchemaItem.ELEMENT_REF:
            schemaRefSet.add(((ElementRef)item).getReferent().getSchema());
            break;

         case SchemaItem.ENUM_TYPE:
            addReferences(((EnumType)item).getBase(), schemaRefSet, schema);
            break;

         case SchemaItem.ELEMENT:
         case SchemaItem.ATTRIBUTE:
            Type type = ((Markup)item).getType();

            if (type.getName() == null)
            {
               addReferences(type, schemaRefSet, schema);
            }
            else if (type.getSchema() != null)
            {
               schemaRefSet.add(type.getSchema());
            }

            break;

         case SchemaItem.COMPOSITE_TYPE:
            CompositeType comp = (CompositeType)item;

            if (comp.getBase() != null)
            {
               addReferences(comp.getBase(), schemaRefSet, schema);
            }

            if (comp.getValueType() != null)
            {
               addReferences(comp.getValueType(), schemaRefSet, schema);
            }

            for (int i = 0, nCount = comp.getChildCount(); i < nCount; i++)
            {
               addReferences(comp.getChild(i), schemaRefSet, schema);
            }

            break;
      }
   }

   /**
    * Computes whether or not inheritance relationships can be exported for the types.
    * XML Schema does not support inheritance hierarchies where a derived message overrides
    * children of the base message; the <xsd:extension... element only allows adding to
    * the parent's set of children.
    * 
    * Pre-computes the results of two functions:
    *   isDerived(CompositeType)
    *   isPolymorphic(CompositeType)
    * 
    * That is, it pre-computes, for every type, flags indicating if that type overrides
    * definitions from another type, and if that type's definitions are overridden by one
    * of its derived types.
    */
   protected void calculateInheritance()
   {
      // Visit all composite type definitions in all schemas
      for (Iterator schemaIterator = m_universe.getSchemaIterator(); schemaIterator.hasNext(); )
      {
         Schema schema = (Schema)schemaIterator.next();

         for (Iterator itemIterator = schema.getItemIterator(); itemIterator.hasNext(); )
         {
            SchemaItem item = (SchemaItem)itemIterator.next();

            if (item.getItemType() == SchemaItem.COMPOSITE_TYPE)
            {
               CompositeType type = (CompositeType)item;

               // Compute whether type redefines something from one of its bases.
               boolean bOverrider = false;
               Type base = type.getBase();

               while (base != null && base.getItemType() == SchemaItem.COMPOSITE_TYPE && !bOverrider)
               {
                  CompositeType baseComp = (CompositeType)base;

                  for (int i = 0, nCount = type.getChildCount(); i < nCount; i++)
                  {
                     Markup derivedChild = type.getChild(i);

                     if (baseComp.getChild(derivedChild.getName(), derivedChild.getItemType()) != null)
                     {
                        bOverrider = true;

                        break;
                     }
                  }

                  base = base.getBase();
               }

               if (bOverrider)
               {
                  m_typeOverrideMap.put(type, DERIVED, Boolean.TRUE);

                  // Mark all base types up the hierarchy as overridden
                  base = type.getBase();

                  while (base != null)
                  {
                     m_typeOverrideMap.put(base, BASE, Boolean.TRUE);
                     base = base.getBase();
                  }
               }
               else if (type.getBase() != null)
               {
                  m_typeOverrideMap.put(type, DERIVED, Boolean.FALSE);

                  if (m_typeOverrideMap.put(type.getBase(), BASE, Boolean.FALSE) == Boolean.TRUE)
                  {
                     // set it back to true if it was already true
                     m_typeOverrideMap.put(type.getBase(), BASE, Boolean.TRUE);
                  }
               }
            }
         }
      }
   }

   /**
    * Gets whether the type should be output as a derived type.
    * @param type The type to check.
    * @return True if the type should be output as a derived type.
    */
   protected boolean isDerived(CompositeType type)
   {
      if (type.getName() == null || type.getBase() == null)
      {
         return false;
      }

      // choice aggregations do not get merged together; inheritance is unsupported
      if (type.getAggregation() != CompositeType.AGGREGATION_SEQUENTIAL)
      {
         return false;
      }

      return !((Boolean)m_typeOverrideMap.get(type, DERIVED)).booleanValue();
   }

   /**
    * Gets whether the type should be referenced directly.
    * @param type The type to check.
    * @return True if the type should be referenced directly; false if anyType should be referenced.
    */
   protected boolean isPolymorphic(CompositeType type)
   {
      Boolean overridden = (Boolean)m_typeOverrideMap.get(type, BASE);

      if (overridden == Boolean.TRUE)
      {
         return type.isBlocked();
      }

      // polymorphism unsupported for choice
      if (!type.isBlocked() && overridden != null && type.getAggregation() != CompositeType.AGGREGATION_SEQUENTIAL)
      {
         return false;
      }

      return true;
   }

   /**
    * Adds the children of the complex type to the map. Optionally adds the children of the base type.
    * The base children are added starting with the children from the root base and ending with the
    * children from the type being exported. Children with the same name as a child from a base type
    * override the child from the base type.
    * @param current The complex type to get the children.
    * @param childMap The map to add the children.
    * @param namespace The key of the namespace (elements, attributes, or types) to add.
    * @param bAddBase True to add children from the base type(s) as well.
    */
   protected static void addChildrenToMap(CompositeType current, LookupDeque childMap, Object namespace, boolean bAddBase)
   {
      if (bAddBase && current.getBase() != null)
      {
         addChildrenToMap((CompositeType)current.getBase(), childMap, namespace, true);
      }

      for (int i = 0, nCount = current.getChildCount(); i < nCount; i++)
      {
         Markup child = current.getChild(i);

         if (Schema.getTypeKey(child.getItemType()) == namespace)
         {
            childMap.put(child.getName(), child);
         }
      }
   }

   // inner classes

   /**
    * Strategy definition for exporting format strings.
    */
   protected interface FormatStringExporter
   {
      /**
       * Writes the XML Schema definition of a format string.
       * @param sFormat The format string.
       * @param writer The output writer.
       * @throws IOException If an I/O error occurs.
       */
      public void export(String sFormat, XMLWriter writer) throws IOException;
   }

   /**
    * Strategy for exporting Boolean format strings.
    */
   protected static class BooleanFormatStringExporter implements FormatStringExporter
   {
      // operations

      /**
       * @see nexj.core.meta.integration.format.xml.schema.XSDSchemaExporter.FormatStringExporter#export(java.lang.String, nexj.core.util.XMLWriter)
       */
      public void export(String sFormat, XMLWriter writer) throws IOException
      {
         BooleanFormat fmt = new BooleanFormat(sFormat);

         writeEnumerationValues(fmt.getTrueValues(), writer);
         writeEnumerationValues(fmt.getFalseValues(), writer);
      }
   }
}
