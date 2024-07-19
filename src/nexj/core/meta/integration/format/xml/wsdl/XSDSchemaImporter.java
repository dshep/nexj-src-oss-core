package nexj.core.meta.integration.format.xml.wsdl;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import nexj.core.meta.integration.format.xml.XSDImportException;
import nexj.core.meta.integration.format.xml.schema.AtomicType;
import nexj.core.meta.integration.format.xml.schema.Attribute;
import nexj.core.meta.integration.format.xml.schema.AttributeRef;
import nexj.core.meta.integration.format.xml.schema.CompositeType;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.ElementRef;
import nexj.core.meta.integration.format.xml.schema.Markup;
import nexj.core.meta.integration.format.xml.schema.PrimitiveType;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaItem;
import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;
import nexj.core.meta.integration.format.xml.schema.Type;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * Imports an XSD as a schema.
 */
public class XSDSchemaImporter extends GenericWebServiceImporter
{
   // constants

   /**
    * Namespace for XML Schema.
    */
   protected final static String XSD_SCHEMA_NAMESPACE = "http://www.w3.org/2001/XMLSchema";

   // attributes

   /**
    * Index for the resolveref array.
    */
   protected int m_nResolveRefIndex;

   // associations

   /**
    * Parents of the schema item being imported.
    */
   protected Stack m_schemaItemStack = new Stack();

   /**
    * Element and attribute references to resolve after parse is complete.
    */
   protected Object[] m_resolveRefArray = new Object[3 * 16];  // String[n*3] String[n*3+1] ElementRef/AttributeRef[n*3+2]

   /**
    * The schema.
    */
   protected Schema m_schema;

   /**
    * Documentation accumulator.
    */
   protected StringBuilder m_docBuilder;

   /**
    * Set of URLs of schemas that have already been imported.
    */
   protected Set m_importedURLSet; // URL[]

   /**
    * List of fixups to run after parse is complete.
    */
   protected List m_fixupList; // Fixup[]

   /**
    * Map of XML Schema built-in type names to their PrimitiveType objects.
    */
   protected final static Lookup s_builtInTypeMap = new HashTab(44); // PrimitiveType[String]

   static
   {
      // XSD Schema built-in primitive datatypes
      s_builtInTypeMap.put("base64Binary", PrimitiveType.BASE64);
      s_builtInTypeMap.put("boolean", PrimitiveType.BOOLEAN);
      s_builtInTypeMap.put("date", PrimitiveType.DATE);
      s_builtInTypeMap.put("dateTime", PrimitiveType.DATETIME);
      s_builtInTypeMap.put("decimal", PrimitiveType.DECIMAL);
      s_builtInTypeMap.put("double", PrimitiveType.DOUBLE);
      s_builtInTypeMap.put("float", PrimitiveType.FLOAT);
      s_builtInTypeMap.put("hexBinary", PrimitiveType.HEX);
      s_builtInTypeMap.put("string", PrimitiveType.STRING);
      s_builtInTypeMap.put("time", PrimitiveType.TIME);
      s_builtInTypeMap.put("duration", PrimitiveType.STRING);
      s_builtInTypeMap.put("gYearMonth", PrimitiveType.STRING);
      s_builtInTypeMap.put("gYear", PrimitiveType.STRING);
      s_builtInTypeMap.put("gMonthDay", PrimitiveType.STRING);
      s_builtInTypeMap.put("gDay", PrimitiveType.STRING);
      s_builtInTypeMap.put("gMonth", PrimitiveType.STRING);
      s_builtInTypeMap.put("anyURI", PrimitiveType.STRING);
      s_builtInTypeMap.put("QName", PrimitiveType.STRING);
      s_builtInTypeMap.put("NOTATION", PrimitiveType.STRING);

      // XSD Schema built-in derived datatypes
      s_builtInTypeMap.put("normalizedString", PrimitiveType.STRING);
      s_builtInTypeMap.put("token", PrimitiveType.STRING);
      s_builtInTypeMap.put("language", PrimitiveType.STRING);
      s_builtInTypeMap.put("NMTOKEN", PrimitiveType.STRING);
      s_builtInTypeMap.put("NMTOKENS", PrimitiveType.STRING);
      s_builtInTypeMap.put("Name", PrimitiveType.STRING);
      s_builtInTypeMap.put("NCName", PrimitiveType.STRING);
      s_builtInTypeMap.put("ID", PrimitiveType.STRING);
      s_builtInTypeMap.put("IDREF", PrimitiveType.STRING);
      s_builtInTypeMap.put("IDREFS", PrimitiveType.STRING);
      s_builtInTypeMap.put("ENTITY", PrimitiveType.STRING);
      s_builtInTypeMap.put("ENTITIES", PrimitiveType.STRING);
      s_builtInTypeMap.put("integer", PrimitiveType.LONG);
      s_builtInTypeMap.put("nonPositiveInteger", PrimitiveType.LONG);
      s_builtInTypeMap.put("negativeInteger", PrimitiveType.LONG);
      s_builtInTypeMap.put("long", PrimitiveType.LONG);
      s_builtInTypeMap.put("int", PrimitiveType.INTEGER);
      s_builtInTypeMap.put("short", PrimitiveType.INTEGER);
      s_builtInTypeMap.put("byte", PrimitiveType.INTEGER);
      s_builtInTypeMap.put("nonNegativeInteger", PrimitiveType.LONG);
      s_builtInTypeMap.put("unsignedLong", PrimitiveType.LONG);
      s_builtInTypeMap.put("unsignedInt", PrimitiveType.INTEGER);
      s_builtInTypeMap.put("unsignedShort", PrimitiveType.INTEGER);
      s_builtInTypeMap.put("unsignedByte", PrimitiveType.INTEGER);
      s_builtInTypeMap.put("positiveInteger", PrimitiveType.LONG);
   }

   // constructors

   /**
    * Constructor.
    */
   public XSDSchemaImporter()
   {
      super();
      m_importedURLSet = new HashHolder();
      m_fixupList = new ArrayList();
   }

   /**
    * Constructor.
    * @param parent The parent importer.
    * @param bSearchParent True to search parent for alias resolution.
    */
   public XSDSchemaImporter(GenericWebServiceImporter parent, boolean bSearchParent)
   {
      super(parent, bSearchParent);

      if (parent instanceof XSDSchemaImporter)
      {
         XSDSchemaImporter xsdParent = (XSDSchemaImporter)parent;

         m_importedURLSet = xsdParent.m_importedURLSet;
         m_fixupList = xsdParent.m_fixupList;
      }
      else
      {
         m_importedURLSet = new HashHolder();
         m_fixupList = new ArrayList();
      }
   }

   // operations

   /**
    * Takes the location of an XSD and returns a schema.
    * @param xsdURL The location of an XSD file.
    * @return A schema object.
    * @throws IOException if the URL cannot be used.
    */
   public static SchemaUniverse parse(URL xsdURL) throws IOException
   {
      return new XSDSchemaImporter().parseXSD(xsdURL);
   }

   /**
    * Executes all fixup actions.
    */
   public void runFixups()
   {
      for (int i = 0; i < m_fixupList.size(); i++)
      {
         ((Fixup)m_fixupList.get(i)).fixup();
      }
   }

   /**
    * Returns a string representing where in the schema sName is found.
    */
   protected String getParentHierarchyString()
   {
      StringBuilder sb = new StringBuilder();

      for (int i = 0; i < m_schemaItemStack.size(); i++)
      {
         SchemaItem item = (SchemaItem)m_schemaItemStack.get(i);

         switch (item.getItemType())
         {
            case SchemaItem.ATTRIBUTE:
            case SchemaItem.ELEMENT:
               if (item.getName() != null)
               {
                  if (i > 0)
                  {
                     sb.append('.');
                  }

                  sb.append(item.getName());
               }

               break;

            case SchemaItem.COMPOSITE_TYPE:
            case SchemaItem.ENUM_TYPE:
            case SchemaItem.FORMAT_TYPE:
            case SchemaItem.PRIMITIVE_TYPE:
               if (item.isTopLevel() && item.getName() != null)
               {
                  if (i > 0)
                  {
                     sb.append('.');
                  }

                  sb.append(item.getName());
               }

               break;

            default:
               sb.append("Unknown Type[");
               sb.append(item.getClass().getName());
               sb.append("](\"");
               sb.append(item.getName());
               sb.append("\")");
               break;
         }
      }

      return sb.toString();
   }

   /**
    * Returns the last used alias to refer to the given namespace or null.
    * @param sURI The namespace.
    * @return The last used alias for the namespace or null.
    */
   protected String getPreferredPrefix(String sURI)
   {
      GenericWebServiceImporter cur = this;

      while (cur.m_parent != null)
      {
         cur = cur.m_parent;

         Object prefix = cur.m_uriAliasMap.get(sURI);

         if (prefix != null)
         {
            return (String)prefix;
         }
      }

      return null;
   }

   /**
    * Adds information about a reference to the resolution queue.
    * @param sNS The namespace of the referent.
    * @param sLocal The name of the referent.
    * @param referrer The referrer, an ElementRef or AttributeRef.
    */
   protected void addReference(String sNS, String sLocal, Markup referrer)
   {
      // grow array if necessary
      if (m_nResolveRefIndex >= m_resolveRefArray.length)
      {
         Object[] resolveRefArray = new Object[m_resolveRefArray.length << 1];

         System.arraycopy(m_resolveRefArray, 0, resolveRefArray, 0, m_resolveRefArray.length);
         m_resolveRefArray = resolveRefArray;
      }

      m_resolveRefArray[m_nResolveRefIndex++] = sNS;
      m_resolveRefArray[m_nResolveRefIndex++] = sLocal;
      m_resolveRefArray[m_nResolveRefIndex++] = referrer;
   }

   /**
    * Returns the requested type or null if none found.
    * @param sURI The namespace of the type.
    * @param sName The name of the type.
    * @return The type or null if none found.
    */
   protected Type findType(String sURI, String sName)
   {
      Schema schema = m_universe.findSchemaByURI(sURI);

      if (schema != null)
      {
         return (Type)schema.findItem(sName, SchemaItem.COMPOSITE_TYPE);
      }

      return null;
   }

   /**
    * Resolves the references saved during parsing.
    */
   protected void resolveReferences()
   {
      int n = 0;

      while (n < m_nResolveRefIndex)
      {
         Schema schema = m_universe.findSchemaByURI((String)m_resolveRefArray[n++]);

         if (schema != null)
         {
            String sLocal = (String)m_resolveRefArray[n++];
            Markup referrer = (Markup)m_resolveRefArray[n++];

            switch (referrer.getItemType())
            {
               case SchemaItem.ELEMENT_REF:
                  Element referent = (Element)schema.findItem(sLocal, SchemaItem.ELEMENT);

                  if (referent == null)
                  {
                     throw new XSDImportException("err.meta.import.unknownType", new Object[] { m_resolveRefArray[n-2], m_resolveRefArray[n-3] }, null);
                  }

                  ((ElementRef)referrer).setReferent(referent);
                  break;

               case SchemaItem.ATTRIBUTE_REF:
                  Attribute attrReferent = (Attribute)schema.findItem(sLocal, SchemaItem.ATTRIBUTE);

                  if (attrReferent == null)
                  {
                     throw new XSDImportException("err.meta.import.unknownType", new Object[] { m_resolveRefArray[n-2], m_resolveRefArray[n-3] }, null);
                  }

                  ((AttributeRef)referrer).setReferent(attrReferent);
                  break;

               default:
                  throw new IllegalStateException();
            }
         }
         else
         {
            throw new XSDImportException("err.meta.import.unknownSchema", new Object[] { m_resolveRefArray[n-1] }, null);
         }
      }
   }

   /**
    * Returns the built-in type.
    * @param sName The name of the type.
    * @return The specified built-in type.
    */
   protected PrimitiveType getBuiltInType(String sName)
   {
      PrimitiveType primitive = (PrimitiveType)s_builtInTypeMap.get(sName);

      if (primitive == null)
      {
         throw new XSDImportException("err.meta.import.unknownType", new Object[] { getTextPosition(), sName, XSD_SCHEMA_NAMESPACE }, getTextPosition());
      }

      return primitive;
   }

   /**
    * Sets the type on a markup given the type name.
    * @param markup The markup.
    * @param sType The qualified type name.
    */
   protected void setType(final Markup markup, String sType)
   {
      final String sNS = getURI(getAlias(sType));
      final String sLocal = getLocalName(sType);

      if (sNS.equals(XSD_SCHEMA_NAMESPACE))
      {
         markup.setType(getBuiltInType(sLocal));
      }
      else
      {
         m_fixupList.add(new Fixup()
         {
            public void fixup()
            {
               Type type = findType(sNS, sLocal);

               if (type != null)
               {
                  switch (type.getItemType())
                  {
                     case SchemaItem.COMPOSITE_TYPE:
                     case SchemaItem.PRIMITIVE_TYPE:
                        markup.setType(type);
                        break;

                     default:
                        throw new IllegalStateException();
                  }
               }
            }
         });
      }
   }

   /**
    * Receive notification of the start of an element element.
    */
   protected void handleElement(Attributes attributes)
   {
      if (m_schemaItemStack.isEmpty())
      {
         // top level element
         if (attributes.getValue("name") != null)
         {
            Element element = new Element(attributes.getValue("name"));

            if (attributes.getValue("type") != null)
            {
               setType(element, attributes.getValue("type"));
            }

            if (attributes.getValue("nillable") != null)
            {
               element.setNillable(StringUtil.parseBoolean(attributes.getValue("nillable")));
            }

            m_schema.addItem(element);
            m_schemaItemStack.push(element);
         }
      }
      else if (attributes.getValue("ref") == null)
      {
         // local element, not handling ref
         Element element = new Element(attributes.getValue("name"));

         if (attributes.getValue("type") != null)
         {
            setType(element, attributes.getValue("type"));
         }

         if (attributes.getValue("nillable") != null)
         {
            element.setNillable(StringUtil.parseBoolean(attributes.getValue("nillable")));
         }

         setMinMaxCount(element, attributes.getValue("minOccurs"), attributes.getValue("maxOccurs"));
         m_schemaItemStack.push(element);
      }
      else
      {
         String sNS = getURI(getAlias(attributes.getValue("ref")));
         String sLocal = getLocalName(attributes.getValue("ref"));

         if (sNS.equals(XSD_SCHEMA_NAMESPACE) && sLocal.equals("schema"))
         {
            // create element called "schema" add it to the current parent.
            Element element = new Element("schema");

            element.setType(PrimitiveType.ANY);
            setMinMaxCount(element, attributes.getValue("minOccurs"), attributes.getValue("maxOccurs"));
            m_schemaItemStack.push(element);
         }
         else
         {
            ElementRef elementRef = new ElementRef(sLocal);

            setMinMaxCount(elementRef, attributes.getValue("minOccurs"), attributes.getValue("maxOccurs"));
            m_schemaItemStack.push(elementRef);
            addReference(sNS, sLocal, elementRef);
         }
      }
   }

   /**
    * Set the multiplicity on an element.
    * @param element The element.
    * @param sMinOccurs The minimum occurrences.
    * @param sMaxOccurs The maximum occurrences.
    */
   protected void setMinMaxCount(Element element, String sMinOccurs, String sMaxOccurs)
   {
      if (sMinOccurs != null)
      {
         element.setMinCount(Integer.parseInt(sMinOccurs));
      }

      if (sMaxOccurs != null)
      {
         if (sMaxOccurs.equals("unbounded"))
         {
            element.setMaxCount(0);
         }
         else
         {
            element.setMaxCount(Integer.parseInt(sMaxOccurs));
         }
      }
   }

   /**
    * Receive notification of the start of a simple type element.
    */
   protected void handleSimpleType(Attributes attributes)
   {
      if (m_schemaItemStack.isEmpty())
      {
         // top level
         if (attributes.getValue("name") != null)
         {
            PrimitiveType type = new PrimitiveType(attributes.getValue("name"));

            m_schema.addItem(type);
            m_schemaItemStack.push(type);
         }
      }
      else
      {
         PrimitiveType type = new PrimitiveType(attributes.getValue("name"));

         type.setDescription(getParentHierarchyString());

         if (m_schemaItemStack.peek() instanceof PrimitiveType)
         {
            ((PrimitiveType)m_schemaItemStack.peek()).setBase(type);
         }
         else if (m_schemaItemStack.peek() instanceof Markup)
         {
            ((Markup)m_schemaItemStack.peek()).setType(type);
         }

         m_schemaItemStack.push(type);
      }
   }

   /**
    * Receive notification of the start of a restriction element.
    */
   protected void handleRestriction(Attributes attributes)
   {
      String sBase = attributes.getValue("base");

      if (attributes.getValue("base") != null && m_schemaItemStack.peek() instanceof PrimitiveType)
      {
         final PrimitiveType type = (PrimitiveType)m_schemaItemStack.peek();
         final String sLocal = getLocalName(sBase);
         final String sNS = getURI(getAlias(sBase));

         if (sNS.equals(XSD_SCHEMA_NAMESPACE))
         {
            type.setBase(getBuiltInType(sLocal));
         }
         else
         {
            final TextPosition textPos = getTextPosition();

            m_fixupList.add(new Fixup()
            {
               public void fixup()
               {
                  Schema schema = m_universe.findSchemaByURI(sNS);

                  if (schema != null)
                  {
                     SchemaItem item = schema.findItem(sLocal, SchemaItem.PRIMITIVE_TYPE);

                     if (item instanceof AtomicType)
                     {
                        type.setBase((AtomicType)item);

                        return;
                     }

                     throw new XSDImportException("err.meta.import.unknownType", new Object[] { sLocal, sNS }, textPos);
                  }

                  throw new XSDImportException("err.meta.import.unknownSchema", new Object[] { sNS, sLocal }, textPos);
               }
            });
         }
      }
   }

   /**
    * Receive notification of the start of an extension element.
    */
   protected void handleExtension(Attributes attributes)
   {
      if (m_schemaItemStack.peek() instanceof CompositeType && attributes.getValue("base") != null)
      {
         final String sLocal = getLocalName(attributes.getValue("base"));
         final String sNS = getURI(getAlias(attributes.getValue("base")));
         final CompositeType type = (CompositeType)m_schemaItemStack.peek();

         if (sNS.equals(XSD_SCHEMA_NAMESPACE))
         {
            type.setValueType(getBuiltInType(sLocal));
         }
         else
         {
            final TextPosition textPos = getTextPosition();

            m_fixupList.add(new Fixup()
            {
               public void fixup()
               {
                  Type base = findType(sNS, sLocal);

                  if (base instanceof CompositeType)
                  {
                     type.setBase((CompositeType)base);
                  }
                  else if (base instanceof PrimitiveType)
                  {
                     type.setValueType((PrimitiveType)((PrimitiveType)base).getBase());
                  }
                  else
                  {
                     throw new XSDImportException("err.meta.import.unknownType", new Object[] { sLocal, sNS }, textPos);
                  }
               }
            });
         }
      }
   }

   /**
    * Receive notification of the start of a complex type element.
    */
   protected void handleComplexType(Attributes attributes)
   {
      if (m_schemaItemStack.isEmpty())
      {
         // top level
         if (attributes.getValue("name") != null)
         {
            CompositeType type = new CompositeType(attributes.getValue("name"));

            if (attributes.getValue("abstract") != null)
            {
               type.setAbstract(StringUtil.parseBoolean(attributes.getValue("abstract")));
            }

            m_schema.addItem(type);
            m_schemaItemStack.push(type);
         }
      }
      else
      {
         // inline type
         Element parent = (m_schemaItemStack.peek() instanceof Element) ? (Element)m_schemaItemStack.peek() : null;
         CompositeType type = new CompositeType(null);

         type.setDescription(getParentHierarchyString());

         if (parent != null)
         {
            parent.setType(type);
         }

         m_schemaItemStack.push(type);
      }
   }

   /**
    * Receive notification of the start of an attribute element.
    */
   protected void handleAttribute(Attributes attributes)
   {
      if (m_schemaItemStack.isEmpty())
      {
         // top level
         if (attributes.getValue("name") != null)
         {
            Attribute attr = new Attribute(attributes.getValue("name"));

            if (attributes.getValue("type") != null)
            {
               setType(attr, attributes.getValue("type"));
            }

            m_schema.addItem(attr);
            m_schemaItemStack.push(attr);
         }
      }
      else if (attributes.getValue("ref") == null)
      {
         Attribute attr = new Attribute(attributes.getValue("name"));

         if (attributes.getValue("type") != null)
         {
            setType(attr, attributes.getValue("type"));
         }

         if (attributes.getValue("use") != null)
         {
            attr.setRequired(attributes.getValue("use").equalsIgnoreCase("required"));
         }

         m_schemaItemStack.push(attr);
      }
      else
      {
         String sLocal = getLocalName(attributes.getValue("ref"));
         String sNS = getURI(getAlias(attributes.getValue("ref")));
         Attribute attrRef = new AttributeRef(sLocal);

         if (attributes.getValue("use") != null)
         {
            attrRef.setRequired(attributes.getValue("use").equalsIgnoreCase("required"));
         }

         m_schemaItemStack.push(attrRef);
         addReference(sNS, sLocal, attrRef);
      }
   }

   /**
    * Receive notification of the start of an any element.
    */
   protected void handleAny(Attributes attributes)
   {
      Element element = new Element("any");

      element.setType(PrimitiveType.ANY);
      setMinMaxCount(element, attributes.getValue("minOccurs"), attributes.getValue("maxOccurs"));
      ((CompositeType)m_schemaItemStack.peek()).addChild(element);
   }

   /**
    * Handles import and include tags that link to an XSD.
    * @param attributes The attributes attached to the element.
    */
   protected void handleParseXSD(Attributes attributes)
   {
      try
      {
         if (!StringUtil.isEmpty(attributes.getValue("schemaLocation")))
         {
            importSchema(attributes.getValue("schemaLocation"));
         }
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }
   }

   /**
    * Imports an XSD.
    * @param url the XSD location.
    * @throws IOException If a stream cannot be opened.
    */
   public SchemaUniverse parseXSD(URL url) throws IOException
   {
      m_url = url;

      parseStream(url);

      if (m_parent == null)
      {
         resolveReferences();
         runFixups();
      }

      return m_universe;
   }

   protected void importSchema(String sLocation) throws IOException
   {
      URL url = new URL(m_url, sLocation);

      if (!m_importedURLSet.contains(url.toExternalForm()))
      {
         XSDSchemaImporter subImporter = new XSDSchemaImporter(this, false);
         subImporter.m_url = url;
         subImporter.m_nResolveRefIndex = m_nResolveRefIndex;
         subImporter.m_resolveRefArray = m_resolveRefArray;

         subImporter.parseStream(url);

         m_nResolveRefIndex = subImporter.m_nResolveRefIndex;
         m_resolveRefArray = subImporter.m_resolveRefArray;
      }
   }

   /**
    * Parse an XML stream from a URL.
    * @param url The XML stream location.
    * @throws IOException If the stream cannot be opened.
    */
   protected void parseStream(URL url) throws IOException
   {
      InputStream is = null;
      Reader r = null;

      try
      {
         is = URLUtil.openStream(url);
         r = new InputStreamReader(is, "UTF-8");
         
         m_importedURLSet.add(url.toExternalForm());
         XMLUtil.parse(r, this);
      }
      finally
      {
         IOUtil.close(is);
         IOUtil.close(r);
      }
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public void startElement(String sURI, String sLocalName, String sQName, Attributes attributes) throws SAXException
   {
      if (sLocalName.equals("schema"))
      {
         m_sTargetNamespace = attributes.getValue("targetNamespace");

         if (m_sTargetNamespace == null)
         {
            if (m_parent != null)
            {
               m_sTargetNamespace = m_parent.m_sTargetNamespace;
            }
            else
            {
               throw new XSDImportException("err.meta.import.rootMissingTargetNamespace", new Object[] { m_url }, null);
            }
         }

         Schema schema = m_universe.getSchema(m_sTargetNamespace, getPreferredPrefix(m_sTargetNamespace));

         m_schema = schema;
      }
      else if (sLocalName.equals("include"))
      {
         handleParseXSD(attributes);
      }
      else if (sLocalName.equals("import"))
      {
         handleParseXSD(attributes);
      }
      else if (sLocalName.equals("attribute"))
      {
         handleAttribute(attributes);
      }
      else if (sLocalName.equals("element"))
      {
         handleElement(attributes);
      }
      else if (sLocalName.equals("simpleType"))
      {
         handleSimpleType(attributes);
      }
      else if (sLocalName.equals("complexType"))
      {
         handleComplexType(attributes);
      }
      else if (sLocalName.equals("sequence"))
      {
         if (m_schemaItemStack.peek() instanceof CompositeType)
         {
            ((CompositeType)m_schemaItemStack.peek()).setAggregation(CompositeType.AGGREGATION_SEQUENTIAL);
         }
      }
      else if (sLocalName.equals("choice"))
      {
         if (m_schemaItemStack.peek() instanceof CompositeType)
         {
            ((CompositeType)m_schemaItemStack.peek()).setAggregation(CompositeType.AGGREGATION_CHOICE);
         }
      }
      else if (sLocalName.equals("all"))
      {
         if (m_schemaItemStack.peek() instanceof CompositeType)
         {
            ((CompositeType)m_schemaItemStack.peek()).setAggregation(CompositeType.AGGREGATION_RANDOM);
         }
      }
      else if (sLocalName.equals("extension"))
      {
         handleExtension(attributes);
      }
      else if (sLocalName.equals("restriction"))
      {
         handleRestriction(attributes);
      }
      else if (sLocalName.equals("any"))
      {
         handleAny(attributes);
      }
      else if (sLocalName.equals("documentation"))
      {
         m_docBuilder = new StringBuilder();
      }
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public void endElement(String sURI, String sLocalName, String sQName) throws SAXException
   {
      if (sLocalName.equals("schema"))
      {
         m_sTargetNamespace = null;
      }
      else if (sLocalName.equals("attribute") || sLocalName.equals("element"))
      {
         Markup markup = (Markup)m_schemaItemStack.pop();

         if (!m_schemaItemStack.isEmpty() && m_schemaItemStack.peek() instanceof CompositeType)
         {
            ((CompositeType)m_schemaItemStack.peek()).addChild(markup);
         }
      }
      else if (sLocalName.equals("simpleType"))
      {
         m_schemaItemStack.pop();
      }
      else if (sLocalName.equals("complexType"))
      {
         m_schemaItemStack.pop();
      }
      else if (sLocalName.equals("documentation"))
      {
         if (!m_schemaItemStack.isEmpty())
         {
            SchemaItem item = (SchemaItem)m_schemaItemStack.peek();

            if (StringUtil.isEmpty(item.getDescription()))
            {
               item.setDescription(m_docBuilder.toString());
            }
            else
            {
               item.setDescription(item.getDescription() + "\r\n\r\n" + m_docBuilder.toString());
            }
         }
         else
         {
            if (StringUtil.isEmpty(m_schema.getDescription()))
            {
               m_schema.setDescription(m_docBuilder.toString());
            }
            else
            {
               m_schema.setDescription(m_schema.getDescription() + "\r\n\r\n" + m_docBuilder.toString());
            }
         }

         m_docBuilder = null;
      }
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
    */
   public void characters(char[] ch, int nStart, int nLength) throws SAXException
   {
      if (m_docBuilder != null)
      {
         m_docBuilder.append(ch, nStart, nLength);
      }
   }
}
