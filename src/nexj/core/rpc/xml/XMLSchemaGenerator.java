// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.XMLWriter;

/**
 * Abstract class for generating API schema definitions.
 */
public abstract class XMLSchemaGenerator
{

   /**
    * Default base metatype to use for any metatype without a base class. 
    */
   public final static Metaclass DEFAULT_BASE_TYPE = new Metaclass(XML.BASE_PREFIX + "TransferObject");

   static
   {
      // List of attributes visible in the default base type
      // (used to avoid collisions with attributes of extended types).
      // This list should include all attributes from self and parents of the type. 
      DEFAULT_BASE_TYPE.addAttribute(new Attribute(XML.BASE_PREFIX + "class"));
      DEFAULT_BASE_TYPE.addAttribute(new Attribute(XML.BASE_PREFIX + "event"));
      DEFAULT_BASE_TYPE.addAttribute(new Attribute(XML.BASE_PREFIX + "version"));
      DEFAULT_BASE_TYPE.addAttribute(new Attribute(XML.BASE_PREFIX + "oid"));
      DEFAULT_BASE_TYPE.addAttribute(new Attribute(XML.BASE_PREFIX + "keys"));
      DEFAULT_BASE_TYPE.addAttribute(new Attribute(XML.BASE_PREFIX + "values"));
   }

   /**
    * List of predefined types used in the protocol.
    */
   protected final static List/*<XMLMetatype>*/ s_xmlMetatypeList =
      new ArrayList/*<XMLMetatype>*/(20);

   static
   {
      // This element must be identical between "Array" and "ArrayOfAnyType",
      // the former only used by MS.NET C# IDE
      // NOTE: MS.NET console compiler explicitly expects "values" tag and will not unmarshal
      //       properly if any other tag is used with "ArrayOfAnyType",
      //       however the IDE compiler checks the tag of the "Array" type, hence both "Array" and
      //       "ArrayOfAnyType" must have the same child
      // NOTE2: at a later date MS.NET for no apparent reason decided to switch to using "item" tag
      //        although the element below defined "values",
      //        both for the MS.NET C# IDE and the console compiler, so we switch again maybe
      //        someone can figure out how to make MS.NET stick with a constant value TODO
      XMLMetatype.XMLMetatypeElement arrayOfAnyTypeElement =
         new XMLMetatype.XMLMetatypeElement(
            "item", XML.getQualifiedType(Primitive.ANY), 0, -1, true);

      // NOTE: this type is explicitly needed for MS.NET C# IDE, it should _not_ be used by any
      //       other type (the command line C# compiler doesn't even use it)
      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType("Array"))
         .addElement(arrayOfAnyTypeElement));

      // NOTE: this type is used by MS.NET to return object[]
      //       (regardless of if it's defined or not by the NexJ server)
      //       MS.NET also expects any object[] (that contain object[] items) to explicitly have
      //       this type (or it will fail to unmarshall the individual items)
      // NOTE2: MS.NET cannot handle multidimensional arrays unless they are in a separate tag
      //        i.e. Object[][] cannot be in the form: <Objs><Obj/><Obj/></Objs><Objs><Obj/></Objs>
      //             it must be marshalled as: <X><Objs><Obj/><Obj/></Objs><Objs><Obj/></Objs></X>
      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType("ArrayOfAnyType"))
         .addElement(arrayOfAnyTypeElement));

      // Type used for Collection instances.
      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Collection"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "item", XML.getQualifiedType(Primitive.ANY), 0, -1, true)));

      // Generic types
      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Expression"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "text", XML.getQualifiedType(Primitive.STRING), 1, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Function"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "code", new QName(XML.XSD_URI, "unsignedShort", XML.XSD_NS), 0, -1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "constants", XML.getQualifiedType(Primitive.ANY), 0, -1, true)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Exception"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "errorCode", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "errorMessage", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "errorArgs", XML.getQualifiedType(Primitive.ANY), 0, -1, true))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "class", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "oid", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "ordinal", XML.getQualifiedType(Primitive.INTEGER), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "attributes", XML.getQualifiedType(Primitive.STRING), 0, -1, true))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "attributeExceptions", XML.getTNSType(XML.BASE_PREFIX + "Exception"), 0, -1, true))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "exceptions", XML.getTNSType(XML.BASE_PREFIX + "Exception"), 0, -1, true)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Locale"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "name", XML.getQualifiedType(Primitive.STRING), 1, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Macro"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "code", new QName(XML.XSD_URI, "unsignedShort", XML.XSD_NS), 0, -1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "constants", XML.getQualifiedType(Primitive.ANY), 0, -1, true)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Pair"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "head", XML.getQualifiedType(Primitive.ANY), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "tail", XML.getQualifiedType(Primitive.ANY), 0, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "PrivilegeSet"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "mask", XML.getQualifiedType(Primitive.BINARY), 1, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "ByteVector"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "value", XML.getQualifiedType(Primitive.BINARY), 1, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Invocation"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "object", XML.getTNSType(XML.BASE_PREFIX + "TransferObject"), 1, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "event", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "arg", XML.getQualifiedType(Primitive.ANY), 0, -1, true))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "attributes", XML.getTNSType(XML.BASE_PREFIX + "Pair"), 0, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Symbol"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "name", XML.getQualifiedType(Primitive.STRING), 1, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "TimeZone"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "name", XML.getQualifiedType(Primitive.STRING), 1, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "TransferObject"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            XML.BASE_PREFIX + "class", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            XML.BASE_PREFIX + "event", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement( // absent if reference (i.e. class+oid)
            XML.BASE_PREFIX + "version", XML.getQualifiedType(Primitive.INTEGER), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            XML.BASE_PREFIX + "oid", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            XML.BASE_PREFIX + "keys", XML.getQualifiedType(Primitive.STRING), 0, -1, true))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            XML.BASE_PREFIX + "values", XML.getQualifiedType(Primitive.ANY), 0, -1, true)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Request"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "namespace", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "version", XML.getQualifiedType(Primitive.STRING), 1, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "async", XML.getQualifiedType(Primitive.BOOLEAN), 1, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "commit", XML.getQualifiedType(Primitive.BOOLEAN), 1, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "locale", XML.getTNSType(XML.BASE_PREFIX + "Locale"), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "timeZone", XML.getTNSType(XML.BASE_PREFIX + "TimeZone"), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "correlator", XML.getTNSType(XML.BASE_PREFIX + "TransferObject"), 0, 1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "invocation", XML.getTNSType(XML.BASE_PREFIX + "Invocation"), 0, -1, false))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "filters", XML.getTNSType(XML.BASE_PREFIX + "TransferObject"), 0, -1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Response"))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "results", XML.getQualifiedType(Primitive.ANY), 0, -1, true))
         // have to use a 2D array type or MS.NET will not be able to generate valid code
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "events",
            XML.getTNSType(
               XML.getTNSType(XML.BASE_PREFIX + "TransferObject", true).getLocalPart(), true),
            0, 1, false)));

      s_xmlMetatypeList.add(new XMLMetatype(XML.getTNSType("unsignedShort", true))
         .addElement(new XMLMetatype.XMLMetatypeElement(
            "item", new QName(XML.XSD_URI, "unsignedShort", XML.XSD_NS), 0, -1, false)));

      // for use with TransferObject-array-array
      s_xmlMetatypeList.add(
         new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "TransferObject", true))
            .addElement(new XMLMetatype.XMLMetatypeElement(
               "item", XML.getTNSType(XML.BASE_PREFIX + "TransferObject"), 0, -1, true)));

      // 2D array of TransferObject // need this type otherwise MS.NET throws
      // "error CS0030: Cannot convert type 'NexJ.TransferObject[]' to 'NexJ.TransferObject'"
      // when trying to change an array to multiple identical tags, one for each item (in Result)
      s_xmlMetatypeList.add(
         new XMLMetatype(XML.getTNSType(
            XML.getTNSType(XML.BASE_PREFIX + "TransferObject", true).getLocalPart(), true))
            .addElement(new XMLMetatype.XMLMetatypeElement(
               "item", XML.getTNSType(XML.BASE_PREFIX + "TransferObject", true), 0, -1, true)));
   }

   /**
    * The XML writer to which the DL document is output.
    */
   protected XMLWriter m_writer;

   /**
    * Cache of results from getQualifiedType that is shared amongst all generation attempts for
    * this object since cached data is static: XMLQType[Type][Boolean].
    */
   protected Lookup2D/*<Type, Boolean, QName>*/ m_typeMap =
      new HashTab2D/*<Type, Boolean, QName>*/();

   // Generation invocation specific data variables
   // (don't use a ThreadLocal variable due to overhead,
   //  prefer to enforce non-clobbering via synchronization).

   /**
    * Array containing an ordered list of Metaclasses to export.
    * Created after the dependency resolution stage and used for the generation stage.
    */
   protected Metaclass[] m_metaclassArray;

   /**
    * Array containing an ordered list of XMLMetatypes to export.
    * Created after the dependency resolution stage and used for the generation stage.
    */
   protected XMLMetatype[] m_metatypeArray;

   /**
    * Set of optional unqualified Metaclass types that have not been defined.
    * Used during the dependency resolution stage.
    */
   protected Lookup/*<String, Metaclass>*/ m_optionalMetaclassMap;

   /**
    * Set of optional unqualified XMLMetatype types that have not been defined.
    * Used during the dependency resolution stage.
    */
   protected Lookup/*<QName, XMLMetatype>*/ m_optionalXMLMetatypeMap;

   /**
    * Set of Metaclasses for which type definitions must be exported (not necessarily events).
    * Used during the dependency resolution stage.
    */
   protected Set/*<Metaclass>*/ m_requiredMetaclassSet;

   /**
    * Map of XMLMetatypees which require their definitions to be exported (e.g. dependencies).
    * Used during the dependency resolution stage.
    */
   protected Lookup/*<QName, XMLMetatype>*/ m_requiredXMLMetatypeMap;

   /**
    * Add the types for all metaclass dependencies to required sets
    * e.g. attribute types and event argument types.
    * Implementers must call setRequiredType(Type, boolean) to trigger adding required types.
    * @param meta The metaclass which to check for dependency types (not null).
    */
   protected abstract void addDependencies(Metaclass meta);

   /**
    * Add a metaclass to list of metaclasses to be considered for output.
    * @param meta The metaclass to add (not null).
    */
   protected abstract void addMetaclass(Metaclass meta);

   /**
    * @return The data MIME type.
    */
   public abstract String getMIMEType();

   /**
    * Perform the work required to generate the API and output it to the registered writer, for all
    * the metaclasses that have been added via addMetaclass(Metaclass), and any required static
    * types.
    * @param sURI Base URI to use for binding generation.
    */
   protected abstract void generate(String sURI) throws IOException;

   /**
    * Generate API and output it to the specified writer.
    * Do not allow more than one thread at a time to access this method since generation uses 
    * member variables to store state.
    * Synchronization is preferred over using ThreadLocal variables.
    * @param writer The writer with which to create the document (if null then noop).
    * @param metaItr Iterator over classes to generate XML for.
    * @param sURI Base URI to use for binding generation.
    * @throws IOException On IO error.
    */
   public final void generate(Writer writer,
                              Iterator/*<Metaclass>*/ metaItr,
                              String sURI) throws IOException
   {
      reset();

      // go through all metaclasses and pre-process them internally before the final generate() call
      if (metaItr != null)
      {
         while (metaItr.hasNext())
         {
            addMetaclass((Metaclass)metaItr.next());
         }
      }

      m_writer = (writer instanceof XMLWriter) ? (XMLWriter)writer : new XMLWriter(writer);
      m_optionalMetaclassMap = null; // release memory
      m_optionalXMLMetatypeMap = null; // release memory
      m_metaclassArray = (Metaclass[])m_requiredMetaclassSet.toArray(new Metaclass[0]);
      m_requiredMetaclassSet = null; // release memory
      m_metatypeArray = new XMLMetatype[m_requiredXMLMetatypeMap.size()];

      int i = -1;
      for (Iterator/*<XMLMetatype>*/ itr = m_requiredXMLMetatypeMap.valueIterator(); itr.hasNext();)
      {
         m_metatypeArray[++i] = (XMLMetatype)itr.next();
      }

      m_requiredXMLMetatypeMap = null; // release memory

      Arrays.sort(m_metaclassArray, Metaclass.COMPARATOR);
      Arrays.sort(m_metatypeArray, XMLMetatype.COMPARATOR);

      generate(sURI);
   }

   /**
    * Provide caching for qualified types.
    * @see XML#getQualifiedType(nexj.core.meta.Type, boolean)
    */
   protected QName getQualifiedTypeCached(Type type, boolean bArray)
   {
      assert type != null;

      QName qType = (QName)m_typeMap.get(type, Boolean.valueOf(bArray));

      if (qType == null)
      {
         qType = XML.getQualifiedType(type, bArray);
         m_typeMap.put(type, Boolean.valueOf(bArray), qType);
      }

      return qType;
   }

   /**
    * Reset all member caches.
    * Any classes overriding this function will have to call this function as well
    * (i.e. their parent's function).
    */
   protected void reset()
   {
      m_optionalMetaclassMap = new HashTab/*<String, Metaclass>*/();
      m_optionalXMLMetatypeMap = new HashTab/*<QName, XMLMetatype>*/();
      m_requiredMetaclassSet = new HashHolder/*<Metaclass>*/();
      m_requiredXMLMetatypeMap = new HashTab/*<QName, XMLMetatype>*/();

      // define array types for all XSD types since that is how arrays might be marshalled
      for (int i = 0; i <= Primitive.MAX_COUNT; ++i)
      {
         Primitive type = Primitive.get(i);
         QName qType = XML.getQualifiedType(type);

         if (qType.getNamespaceURI() == XML.XSD_URI) // only consider XSD types, identity check
         {
            setRequiredType(type, true);
         }
      }

      // define all static TNS types,
      // overriding classes can replace/remove entries in map in their reset() function
      for (int i = 0, nCount = s_xmlMetatypeList.size(); i < nCount; ++i)
      {
         setRequiredType((XMLMetatype)s_xmlMetatypeList.get(i));
      }
   }

   /**
    * Set a type as required (if type is not already required then generate it).
    * @param type The type that s required.
    * @param bArray This is an array type.
    */
   protected void setRequiredType(Type type, boolean bArray)
   {
      if (type == null || type == DEFAULT_BASE_TYPE) // DEFAULT_BASE_TYPE output via static types
      {
         return; // null type == xsd:anyType which is already defined (valid for DEFAULT_BASE_TYPE)
      }
      else if (type instanceof Metaclass)
      {
         if (m_requiredMetaclassSet.add(type))
         {
            m_optionalMetaclassMap.remove(type.getName());
            addDependencies((Metaclass)type);
         }
      }
      else if (type instanceof Primitive)
      {
         // NOOP, all primitives defined in XML.XSD_URI and do not need to be redefined
      }
      else
      {
         QName qType = getQualifiedTypeCached(type, false);

         if (!m_requiredXMLMetatypeMap.contains(qType))
         {
            Object xmlType = m_optionalXMLMetatypeMap.remove(qType);

            assert xmlType != null; // it is a design/programming error to have a missing type
            m_requiredXMLMetatypeMap.put(qType, xmlType);
         }
      }

      if (bArray)
      {
         QName qType = getQualifiedTypeCached(type, true);

         if (!m_requiredXMLMetatypeMap.contains(qType))
         {
            m_optionalXMLMetatypeMap.remove(qType);
            m_requiredXMLMetatypeMap.put(
               qType,
               new XMLMetatype(qType).addElement(
                  new XMLMetatype.XMLMetatypeElement(
                     "item", getQualifiedTypeCached(type, false), 0, -1, true)));
         }
      }
   }

   /**
    * Set a type as required.
    * @param type The type that s required (not null).
    */
   protected void setRequiredType(XMLMetatype type)
   {
      assert type != null;

      QName qType = type.getType();

      if (m_requiredXMLMetatypeMap.put(qType, type) != null)
      {
         m_optionalXMLMetatypeMap.remove(qType); // remove just in case it's a double define
      }
   }
}