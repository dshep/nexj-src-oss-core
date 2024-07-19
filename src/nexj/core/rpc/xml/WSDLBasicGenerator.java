// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.rpc.xml.XMLMetatype.XMLMetatypeElement;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Class to generate WSDL v1.1 API without relying on xsd:anyType.
 */
public class WSDLBasicGenerator extends WSDLGenerator
{
   /**
    * The default base XML Metatype.
    * Cannot use DEFAULT_BASE_TYPE because it uses xsd:anyType and relies on polymorphism.
    * Since this class is not supposed to output any polymorphic types make sure not to use
    * extends/restricts for XMLMetatype.
    */
   protected final static XMLMetatype DEFAULT_BASE_METATYPE =
      new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "TransferObject"))
        .addElement(new XMLMetatype.XMLMetatypeElement(
           XML.BASE_PREFIX + "class", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
        .addElement(new XMLMetatype.XMLMetatypeElement(
           XML.BASE_PREFIX + "event", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
        .addElement(new XMLMetatype.XMLMetatypeElement(
           XML.BASE_PREFIX + "version", XML.getQualifiedType(Primitive.INTEGER),
           1, 1, false))
        .addElement(new XMLMetatype.XMLMetatypeElement(
           XML.BASE_PREFIX + "oid", XML.getQualifiedType(Primitive.STRING), 0, 1, false))
        .addElement(new XMLMetatype.XMLMetatypeElement(
           XML.BASE_PREFIX + "keys", XML.getQualifiedType(Primitive.STRING), 0, -1, true))
        .addElement(new XMLMetatype.XMLMetatypeElement(
           XML.BASE_PREFIX + "values", XML.getQualifiedType(Primitive.STRING), 0, -1, true));

   /**
    * Cache for known recursive state of metaclasses
    * (since it's a cache there's no need to keep strong references to it).
    * In actual fact because the implementation below uses static constants,
    * this map should not shrink in size since static values are never deallocated once created.
    * true => Deep recursion i.e. Metaclass references an attribute that will reference Metaclass
    *                             somewhere below
    *      => this attribute should be skipped everywhere.
    * false => No deep recursion (shallow recursion should still be checked for i.e. where
    *                             Metaclass has attribute of same type).
    * Possible recursion types:
    *    DEEP_RECURSIVE - Metaclass references an attribute that will reference Metaclass somewhere
    *                     below (all => skip)
    *                     i.e. attributes of this type should be skipped in all metaclasses.
    *    NON_RECURSIVE - Metaclass does not have any loops (matching type => non-recursive,
    *                    non-matching type => check deeper).
    *    SHALLOW_RECURSIVE - Metaclass references an attribute that is of same type
    *                        (matching type => recursive, non-matching type => check deeper)
    *                        i.e. attributes of this type should be skipped by their Metaclass only
    *                        (others must check deeper for attributes of different type,
    *                         m_checkInProgress will be used to break loops).
    */
   public Lookup/*<String, Boolean>*/ m_deepRecursiveMetaclassMap;

   /**
    * Set containing names of metaclasses used to break recursive lookup steps.
    * This set should always be empty once isRecursiveMetaclass(...) exits.
    */
   public Set/*<String>*/ m_inProgressClassNameSet;

   /**
    * Cache for known valid attribute lists for a given metatype
    * (since it's a cache there's no need to keep strong references to it).
    */
   public Lookup/*<String, List<Attribute>>*/ m_metaclassAttributeCacheMap;

   /**
    * Constructor.
    * @param sSoapAction The value for the soapAction header to expect.
    */
   public WSDLBasicGenerator(String sSoapAction)
   {
      super(sSoapAction);

      // poison the cache with xsd:string as the proper return type instead of xsd:anyType
      m_typeMap.put(Primitive.ANY, Boolean.FALSE, XML.getQualifiedType(Primitive.STRING));
      m_typeMap.put(Primitive.ANY, Boolean.TRUE, XML.getQualifiedType(Primitive.STRING, true));
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#addDependencies(nexj.core.meta.Metaclass)
    */
   protected void addDependencies(Metaclass meta)
   {
      List/*<Attribute>*/ attrList = getValidAttributes(meta);

      // WSDL Basic does not support outputting metaclasses that have no valid attributes, must skip
      if (attrList.isEmpty())
      {
         return;
      }

      // add all attribute types
      for (int i = 0, nCount = attrList.size(); i < nCount; ++i)
      {
         Attribute attr = (Attribute)attrList.get(i);

         if (!attr.isStatic() && (!m_bCompatible || attr.isCompatible()))
         {
            setRequiredType(attr.getType(), false); // collections will repeat same element per item
         }
      }

      // skip event dependency resolution for non-exported metaclasses, also skip dependency
      if (isExportedMetaclass(meta))
      {
         setRequiredType(meta, true); // needed for writeElements(...) e.g. Read-Request
         addEventDependencies(meta);
      }
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#generate(java.lang.String)
    */
   protected void generate(String sURI) throws IOException
   {
      m_writer.openElement("definitions");
         m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL);
         m_writer.setNamespace(XML.XML_NS);
            m_writer.writeAttribute(XML.XSD_NS, XML.XSD_URI);
            m_writer.writeAttribute(XML.NS_TAG_WSDL, XML.NS_URI_WSDL);
            m_writer.writeAttribute(XML.NS_TAG_WSDL_SOAP, XML.NS_URI_WSDL_SOAP);
            m_writer.writeAttribute(XML.ENC_NS, XML.ENC_URI);
            m_writer.writeAttribute(XML.TNS_NS, XML.NS_URI_TNS);
         m_writer.setNamespace(null);
         m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
      m_writer.closeElement();

         m_writer.startElement("types");

            m_writer.openElement("schema");
               m_writer.writeAttribute(XML.XML_NS, XML.XSD_URI);
               m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
               m_writer.writeAttribute("elementFormDefault", "qualified");
            m_writer.closeElement();

               writeTNSXSD();

               // type shared by all *-read_request elements
               m_writer.openElement("complexType");
                  m_writer.writeAttribute("name", "Read-Request");
               m_writer.closeElement();

                  m_writer.startElement("sequence");
                     writeElement("attributes", null, Primitive.STRING);
                     writeElement("where", null, Primitive.STRING);
                     writeElement("orderBy", null, Primitive.STRING);
                     writeElement("count", null, Primitive.INTEGER);
                     writeElement("offset", null, Primitive.INTEGER);
                  m_writer.endElement("sequence");

               m_writer.endElement("complexType");

               // output metaclass specific binding operation here
               for (int i = 0; i < m_metaclassArray.length; ++i)
               {
                  Metaclass meta = m_metaclassArray[i];

                  if (isExportedMetaclass(meta))
                  {
                     writeElements(meta);
                  }
               }

            m_writer.endElement("schema");

            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               Metaclass meta = m_metaclassArray[i];

               if (isExportedMetaclass(meta))
               {
                  writeMetaclassSchema(meta); // metaclass specific schema
               }
            }

         m_writer.endElement("types");

         // output metaclass specific binding operation here
         for (int i = 0; i < m_metaclassArray.length; ++i)
         {
            Metaclass meta = m_metaclassArray[i];

            if (isExportedMetaclass(meta))
            {
               writeMessages(meta);
            }
         }

         m_writer.openElement("portType");
            m_writer.writeAttribute("name", "Server");
         m_writer.closeElement();

            // output metaclass specific operation here
            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               Metaclass meta = m_metaclassArray[i];

               if (isExportedMetaclass(meta))
               {
                  writePortTypeOperations(meta);
               }
            }

         m_writer.endElement("portType");

         for (int i = 0; i < m_metaclassArray.length; ++i)
         {
            Metaclass meta = m_metaclassArray[i];

            if (isExportedMetaclass(meta))
            {
               writePortType(meta); // metaclass specific PortType
            }
         }

         m_writer.openElement("binding");
            m_writer.writeAttribute("name", "ServerSoapBinding");
            m_writer.writeAttribute("type", XML.TNS_NS, ":Server");
         m_writer.closeElement();

            m_writer.openElement("binding");
               m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
               m_writer.writeAttribute("style", "document");
               m_writer.writeAttribute("transport", XML.NS_URI_SOAP_HTTP);
            m_writer.closeEmptyElement();

            // output metaclass specific binding operation here
            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               Metaclass meta = m_metaclassArray[i];

               if (isExportedMetaclass(meta))
               {
                  writeBindingOperations(meta);
               }
            }

         m_writer.endElement("binding");

         for (int i = 0; i < m_metaclassArray.length; ++i)
         {
            Metaclass meta = m_metaclassArray[i];

            if (isExportedMetaclass(meta))
            {
               writeBinding(meta); // metaclass specific binding
            }
         }

         m_writer.openElement("service");
            m_writer.writeAttribute("name", "GenericServer");
         m_writer.closeElement();

            m_writer.openElement("port");
               m_writer.writeAttribute("name", "Server");
               m_writer.writeAttribute("binding", XML.TNS_NS, ":ServerSoapBinding");
            m_writer.closeElement();

               m_writer.openElement("address");
                  m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
                  m_writer.writeAttribute("location", sURI);
               m_writer.closeEmptyElement();

            m_writer.endElement("port");

            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               Metaclass meta = m_metaclassArray[i];

               if (isExportedMetaclass(meta))
               {
                  writeServicePort(meta, sURI); // metaclass specific service port
               }
            }

         m_writer.endElement("service");
      m_writer.endElement("definitions");
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#isExportedEvent(nexj.core.meta.Event)
    */
   protected boolean isExportedEvent(Event event)
   {
      // MDS Studio does not allow creating complexTypes with no elements,
      // non-static events have at least <_instance/>.
      // If no valid arguments defined then cannot create <input/> for <operation/>, therefore
      // cannot invoke method.
      if (!super.isExportedEvent(event) || (event.isStatic() && event.getArgumentCount() == 0))
      {
         return false;
      }

      // check that all argument types are supported, otherwise will not have valid event arguments
      for (int i = 0, nCount = event.getArgumentCount(); i < nCount; ++i)
      {
         Type type = event.getArgument(i).getType();

         if (type instanceof Metaclass && !isSupportedMetaclass((Metaclass)type))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see nexj.core.rpc.xml.WSDLGenerator#isExportedEventResult(nexj.core.meta.Event)
    */
   protected boolean isExportedEventResult(Event event)
   {
      Type type = (event.getResult() == null) ? null : event.getResult().getType();

      return type != null && // untyped result not supported
             type != Primitive.ANY && // polymorphic result type not supported
             !(type instanceof Metaclass && !isSupportedMetaclass((Metaclass)type));
   }

   /**
    * @see nexj.core.rpc.xml.WSDLGenerator#isExportedMetaclass(nexj.core.meta.Metaclass)
    */
   protected boolean isExportedMetaclass(Metaclass meta)
   {
      return super.isExportedMetaclass(meta) && isSupportedMetaclass(meta);
   }

   /**
    * Check if the requested metaclass can be exported in basic WSDL.
    * @param meta The metaclass to check (not null).
    * @param Can the requested metaclass be exported in basic WSDL.
    */
   protected boolean isSupportedMetaclass(Metaclass meta)
   {
      return !getValidAttributes(meta).isEmpty();
   }

   /**
    * Output WSDL binding operation XML for a metaclass.
    * @param meta The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writeBindingOperations(Metaclass meta) throws IOException
   {
      writeBindingOperation("change-", meta.getName(), true);
      writeBindingOperation("read-", meta.getName(), true);
   }

   /**
    * Output WSDL element XML for a metaclass.
    * @param meta The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writeElements(Metaclass meta) throws IOException
   {
      m_writer.openElement("element");
         m_writer.writeAttribute("name", XML.BASE_PREFIX, meta.getName(), "-Change-Request");
      m_writer.closeElement();

         m_writer.startElement("complexType");
            m_writer.startElement("sequence");
               writeElement("objects", null, meta, 0, -1, false, null);
               writeElement("attributes", null, Primitive.STRING);
            m_writer.endElement("sequence");
         m_writer.endElement("complexType");

      m_writer.endElement("element");

      writeElement(
         XML.BASE_PREFIX, meta.getName(), "-Change-Response", getQualifiedTypeCached(meta, true));
      writeElement(
         XML.BASE_PREFIX, meta.getName(), "-Read-Request", XML.TNS_NS, "Read-Request", null);
      writeElement(
         XML.BASE_PREFIX, meta.getName(), "-Read-Response", getQualifiedTypeCached(meta, true));
   }

   /**
    * Output WSDL message XML for a metaclass.
    * @param meta The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writeMessages(Metaclass meta) throws IOException
   {
      super.writeMessages(meta); // output event specific messages
      writeMessage(
         meta.getName(), "-Change-Request", null, null,
         null, XML.BASE_PREFIX, meta.getName(), "-Change-Request");
      writeMessage(
         meta.getName(), "-Change-Response", null, null,
         null, XML.BASE_PREFIX, meta.getName(), "-Change-Response");
      writeMessage(
         meta.getName(), "-Read-Request", null, null,
         null, XML.BASE_PREFIX, meta.getName(), "-Read-Request");
      writeMessage(
         meta.getName(), "-Read-Response", null, null,
         null, XML.BASE_PREFIX, meta.getName(), "-Read-Response");
   }

   /**
    * Override parent behaviour by checking ahead to see which metaclasses are actually valid.
    * Only generate metaclasses if they are non-empty.
    * NOTE: It is OK to postprocess m_defferedMetaclasses items through this because they've been
    *       deferred by other metaclass instantiations
    *       which in turn have already checked validity of their own attributes.
    *       (yes this is hackish because any modification to the aforementioned assumption will
    *        break the algorithm and produce unexpected/invalid results)
    * @see nexj.core.rpc.rest.XSDGenerator#writeMetaclass(nexj.core.meta.Metaclass)
    */
   protected void writeMetaclass(Metaclass meta) throws IOException
   {
      List/*<Attribute>*/ attributeList = getValidAttributes(meta);

      if (attributeList.isEmpty())
      {
         return; // not a Metatype that could have a valid definition for BlackBerry MDS
      }

      m_writer.openElement("complexType");
         m_writer.writeAttribute("name", meta.getName());

         // don't allow instantiation of abstract Metaclasses
         // (still need them because some attributes are of their type)
         if (meta.getVisibility() != Metaclass.PUBLIC)
         {
            m_writer.writeAttribute("abstract", "true");
         }

      m_writer.closeElement();

         if (m_bIncludeDocumentation)
         {
            writeDocumentation(meta.getCaption(), meta.getDescription());
         }

         // MS.NET is having problems using "all" within an extension therefore can only use
         // "sequence"
         m_writer.startElement("sequence");

            // all attributes from the base class that all metaclases pseudo-extend in XML schema
            for (Iterator/*<XMLMetatypeElement>*/ itr =
                    DEFAULT_BASE_METATYPE.m_elementList.iterator(); itr.hasNext();)
            {
               ((XMLMetatypeElement)itr.next()).writeXSD(m_writer);
            }

            for (Iterator/*<Attribute>*/ itr = attributeList.iterator(); itr.hasNext();)
            {
               writeAttribute((Attribute)(itr.next()));
            }

         m_writer.endElement("sequence");

      m_writer.endElement("complexType");
   }

   /**
    * Output WSDL port type operation XML for a metaclass.
    * @param metaclass The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writePortTypeOperations(Metaclass metaclass) throws IOException
   {
      writePortTypeOperation(
         "change-", metaclass.getName(),
         metaclass.getName(), "-Change-Request", null,
         metaclass.getName(), "-Change-Response", null, null);
      writePortTypeOperation(
         "read-", metaclass.getName(),
         metaclass.getName(), "-Read-Request", null,
         metaclass.getName(), "-Read-Response", null, null);
   }

   /**
    * Determine list of valid attributes for a given metaclass.
    * @param meta The metaclass to generate attribute list for.
    * @return List of valid metaclass attributes.
    */
   protected List/*<Attribute>*/ getValidAttributes(Metaclass meta)
   {
      List/*<Attribute>*/ attributeList = (List)m_metaclassAttributeCacheMap.get(meta.getName());

      if (attributeList != null)
      {  // return cached results
         return attributeList;
      }

      attributeList = new ArrayList/*<Attribute>*/();

      for (Iterator/*<Attribute>*/ itr = meta.getAttributeIterator(); itr.hasNext();)
      {
         Attribute attribute = (Attribute)(itr.next());

         // ensure only outputting public instance attributes
         if (!attribute.isStatic() &&
             (!m_bCompatible || attribute.isCompatible()) &&
             attribute.getVisibility() == Metaclass.PUBLIC)
         {
            // a Metaclass should not have any attributes of same type as self or any attributes
            // that would lead back to self in Metaclass attribute dependency tree
            // skip all attributes that can have children/grandchildren/etc with same type as
            // current metaclass
            // make sure it's a non-empty Metaclass
            if(!(attribute.getType() instanceof Metaclass) ||
               (!isRecursiveMetaclass((Metaclass)attribute.getType()) &&
                isSupportedMetaclass((Metaclass)attribute.getType())))
            {
               attributeList.add(attribute);
            }
         }
      }

      m_metaclassAttributeCacheMap.put(meta.getName(), attributeList);
      
      return attributeList;
   }
   
   /**
    * Check if requested attribute is recursive.
    * i.e. A Metaclass should not have any attributes of same type as self or any attributes that
    *      would lead back to self in Metaclass attribute dependency tree.
    *      The cache is for the latter situation, since the former is taken care of by not adding
    *      the attribute in the first place.
    * The only reason for having a for-loop in this method is to distinguish between deep/shallow
    * recursion and only cache the former.
    * @param meta The metaclass to check.
    * @return Was a loop definition found for this metaclass.
    */
   protected boolean isRecursiveMetaclass(Metaclass meta)
   {
      Boolean cached = (Boolean)m_deepRecursiveMetaclassMap.get(meta.getName());

      if (cached != null)
      {  
         // return previously cached value
         return cached.booleanValue();
      }

      for (Iterator/*<Attribute>*/ itr = meta.getAttributeIterator(); itr.hasNext();)
      {
         Attribute attribute = (Attribute)itr.next();

         // this is not an attribute we're interested in to check recursion for or type is already
         // known to be deeply recursive then skip it since it'll be removed anyway
         // meta is not known to be deeply recursive since it's checked above
         if (attribute == null
           || attribute.getVisibility() != Metaclass.PUBLIC
           || !(attribute.getType() instanceof Metaclass)
           || m_deepRecursiveMetaclassMap.get(attribute.getType().getName()) == Boolean.TRUE
            )
         {  
            continue;
         }

         // found the attribute type being referenced in the collection
         // (therefore the requested attribute is immediately recursive but its removal might make
         // the metaclass non-recursive, hence don't cache result)
         if (attribute.getType().getName().equals(meta.getName()))
         {  
            return true; // do NOT cache this
         }

         m_inProgressClassNameSet.add(meta.getName()); // track recursion to break infinite loops

            // check for deep recursion of requested metaclass
            boolean bReachable = isRecursivelyReachable(meta, (Metaclass)attribute.getType());

         m_inProgressClassNameSet.remove(meta.getName());

         // Boolean.TRUE is static therefore it'll never get deallocated by GC hence can use a soft
         // reference and be sure the reference will stay
         if (bReachable &&
             m_deepRecursiveMetaclassMap.get(attribute.getType().getName()) != Boolean.TRUE)
         {  
            // found a recursive loop
            // (tmpBool should only be set to true if the attribute checked was not deemed deeply
            //  recursive, otherwise it would be removed anyway) must doublecheck because
            // recursiveness could have been determined during recursive evaluation step        
            m_deepRecursiveMetaclassMap.put(meta.getName(), Boolean.TRUE); // add result to cache

            return true;
         }
      }

      m_deepRecursiveMetaclassMap.put(meta.getName(), Boolean.FALSE); // add result to cache

      return false;
   }

   /**
    * Check if the metaclass can reach its own type via one of its attributes.
    * Check if the sample metaclass has a recursive definition,
    * start check from the attributes of the collection metaclass and continue recursively
    * (meant to be used as recursive step by isRecursiveMetaclass(Metaclass) hence no check for
    *  sample in m_checkedRecursiveMetaclasses).
    * Note: This function caches results for faster retrieval.
    *       A Metaclass should not have any attributes of same type as self or any attributes that
    *       would lead back to self in Metaclass attribute dependency tree.
    *       The cache is for the latter situation, since the former is taken care of by not adding
    *       the attribute in the first place.
    * Note2: If an attribute is known to be recursive then it is assumed the attribute will be
    *        removed and this its branch will not be searched for loops.
    * @param sample The sample metaclass to check
    *               (assumed to already exist once somewhere in the hierarchy above).
    * @param collection The collection to start check in.
    * @return Was a loop definition found for this metaclass.
    */
   protected boolean isRecursivelyReachable(Metaclass sample, Metaclass collection)
   { 
      // the collection metaclass is recursive, cache for future lookup
      if (m_inProgressClassNameSet.contains(collection.getName()))
      {           
         // cache deep recursion 
         // (guaranteed not to be shallow recursion since there's a check for that before adding to
         //  m_checkInProgress)
         m_deepRecursiveMetaclassMap.put(collection.getName(), Boolean.TRUE); 

         // break out of evaluation so as not to do infinite recursion
         // (since went around the loop second time -> sample not reachable via this branch)
         return false;
      }

      for (Iterator/*<Attribute>*/ itr = collection.getAttributeIterator(); itr.hasNext();)
      {
         Attribute attribute = (Attribute)itr.next();

         // this is not an attribute we're interested in
         if (attribute == null ||
             attribute.getVisibility() != Metaclass.PUBLIC ||
             !(attribute.getType() instanceof Metaclass))
         {  
            continue;
         }

         // found the sample being referenced in the collection
         // (therefore the requested sample might immediately recursive hence should not be cached
         //  because removal of the attribute might make the metaclass non-recursive)
         if (attribute.getType().getName().equals(sample.getName()))
         {  
            return true;
         }

         // attribute deeply recursive or attribute shallowly recursive
         if (m_deepRecursiveMetaclassMap.get(attribute.getType().getName()) == Boolean.TRUE ||
             attribute.getType().getName().equals(collection.getName()) 
            )
         {
            // hence skip attribute since it should be removed anyway when the metaclass pointed to
            // by 'collection' is declared
            continue;
         }

         // track recursion to break infinite loops
         m_inProgressClassNameSet.add(collection.getName());

            // can be false if not encountered or encountered inside a loop that should be removed
            boolean bReachable = isRecursivelyReachable(sample, (Metaclass)attribute.getType());

         // if 'collection' found to be recursive then definition of 'collection' should have this
         // 'attribute' removed, and 'sample' is still OK
         m_inProgressClassNameSet.remove(collection.getName()); 

         // Boolean.TRUE is static therefore it'll never get deallocated by GC hence can use a soft
         // reference and be sure the reference will stay
         if (bReachable &&
             m_deepRecursiveMetaclassMap.get(attribute.getType().getName()) != Boolean.TRUE)
         {  
            // found a recursive loop
            // (tmpBool should only be set to true if the attribute checked was not deemed deeply
            //  recursive, otherwise it would be removed anyway) must double check because
            // recursiveness could have been determined during recursive evaluation step
            return true;
         }
      }

      return false;
   }

   /**
    * Reset all member caches.
    * Any classes overriding this function will have to call this function as well
    * (i.e. their parent's function).
    */
   protected void reset()
   {
      super.reset();

      // remove all static types since they potentially rely on polymorphism
      for (int i = 0, nCount = s_xmlMetatypeList.size(); i < nCount; ++i)
      {
         m_requiredXMLMetatypeMap.remove(((XMLMetatype)s_xmlMetatypeList.get(i)).getType());
      }

      m_requiredXMLMetatypeMap.remove(m_arrayDeclarator.getType()); // no need for MS.NET kludge
      setRequiredType(DEFAULT_BASE_METATYPE); // redefined to not use inheritance for MDS compliance
      m_deepRecursiveMetaclassMap = new HashTab/*<String, Boolean>*/();
      m_inProgressClassNameSet = new HashHolder/*<String>*/();
      m_metaclassAttributeCacheMap = new HashTab/*<String, List<Attribute>>*/();
   }
}