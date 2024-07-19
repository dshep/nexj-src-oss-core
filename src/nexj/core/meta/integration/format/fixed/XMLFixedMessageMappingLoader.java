// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.fixed;

import java.io.StringReader;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.scripting.StringParser;
import nexj.core.util.XMLUtil;

/**
 * Responsible for creating mappings between MessagePart objects and
 * the fixed length fields output file ("flat file database").
 * These mappings are all computed from the metadata.
 *
 * @see nexj.core.meta.integration.XMLMessageMappingLoader
 */
public class XMLFixedMessageMappingLoader implements XMLMessageMappingLoader
{
   // constants

   /**
    * The name of the mapping tag attribute that specifies whether to apply
    * padding to the LEFT or RIGHT of the provided data.
    */
   protected final static String ALIGNMENT_ATTRIBUTE = "alignment";

   /**
    * The name of the mapping tag attribute that specifies the name
    * of the fixed length field to which the associated MessagePart should
    * be mapped.
    */
   protected final static String FIELD_NAME_ATTRIBUTE = "field";

   /**
    * The name of the mapping tag attribute that specifies the formatting
    * string for this part.
    */
   protected final static String FORMAT_ATTRIBUTE = "format";

   /**
    * The name of the mapping tag attribute that specifies the character to use
    * for padding the data to make it the proper length of that field.
    */
   protected final static String PADDING_ATTRIBUTE = "padding";

   /**
    * The name of the mapping tag attribute that specifies the number of elements
    * per page (that is swapped to disk) for a message part mapping with
    * multiplicity greater than 1.
    */
   protected final static String PAGE_SIZE_ATTRIBUTE = "pageSize";

   /**
    * The name of the mapping tag attribute that specifies prefix string
    * for this part.  If specified, the prefix string is at the very start
    * of the field (i.e. field character position 0)
    */
   protected final static String PREFIX_ATTRIBUTE = "prefix";

   /**
    * The name of the mapping tag attribute that specifies suffix string
    * for this part.  If specified, the suffix string is at the very end
    * of the field
    */
   protected final static String SUFFIX_ATTRIBUTE = "suffix";

   /**
    * The name of the mapping tag attribute that specifies width
    * for this part.  This includes the length of prefix, suffix, data
    * and padding.
    */
   protected final static String WIDTH_ATTRIBUTE = "width";

   /**
    * The name of the XML tag that is used to specify additional information
    * about the mapping between the Message and the fixed length file.
    */
   protected final static String FIXED_XML_TAG_NAME = "FixedMapping";

   // associations

   /**
    * The parser for interpreting strings with backslash-escaped characters.
    */
   protected StringParser m_stringParser;

   // constructors

   /**
    * Default constructor to provide member initialization.
    */
   public XMLFixedMessageMappingLoader()
   {
      //Pass null because we don't need to resolve any globals.
      m_stringParser = new StringParser(null);
   }


   // operations

   /**
    * Gets the attribute of given name from the XML node, returning
    * null if no node is specified or attribute is not found.
    *
    * @param node  The node to look at.
    * @param sName The name of the attribute to retrieve.
    * @return The value of the attribute, or null if the attribute
    *         is not set or node is null.
    */
   protected String getStringAttr(Node node, String sName)
   {
      if (node == null)
      {
         return null;
      }

      return XMLUtil.getStringAttr(node, sName);
   }

   /**
    * Gets the attribute of given name from the XML node, returning
    * the default value if no node is specified or attribute is not found.
    *
    * @param node  The node to look at, may be null.
    * @param sName The name of the attribute to retrieve.
    * @param sDefaultValue The default value.
    * @return The value of the attribute, or the default value if the
    * attribute is not set or node is null.
    */
   protected String getStringAttr(Node node, String sName, String sDefaultValue)
   {
      if (node == null)
      {
         return sDefaultValue;
      }

      return XMLUtil.getStringAttr(node, sName, sDefaultValue);
   }

   /**
    * Gets the attribute of a given name from the XML node,
    * parsing out backslash-escaped characters.
    *
    * @param node  The node to look at.
    * @param sName The name of the attribute to retrieve.
    * @param sDefault The default attribute value.
    * @return The result of parsing the attribute, or null
    *         if the attribute is not set or node is null.
    */
   protected String parseStringAttr(Node node, String sName, String sDefault)
   {
      String sToParse = getStringAttr(node, sName, sDefault);

      if (sToParse == null)
      {
         return null;
      }

      return (String)m_stringParser.parse(new StringReader(sToParse), null);
   }

   /**
    * Creates a FixedMessagePartMapping for a given MessagePart. This routine will be
    * called automatically by the framework, one time for every MessagePart in the
    * Message definition. Scans the given element from the metadata file for
    * additional, Fixed-specific instructions, found in FixedMapping tags.
    *
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, MessagePart part, Format format, XMLMetadataLoader loader)
   {
      FixedMessagePartMapping parentMapping = null;
      Element mappingElement = XMLUtil.findChildElement(element, FIXED_XML_TAG_NAME);

      //Compute current level
      CompositeMessagePart parentPart = part.getParent();
      int nThisLevel;

      if (parentPart == null)
      {
         nThisLevel = 0;
      }
      else
      {
         //Require sequential aggregation mode
         if (parentPart.getAggregation() != CompositeMessagePart.SEQUENTIAL)
         {
            throw new MetadataException("err.meta.integration.nonSequentialAggregation",
               new Object[]{FIXED_XML_TAG_NAME, msg.getName()});
         }

         parentMapping = (FixedMessagePartMapping)parentPart.getMapping();
         nThisLevel = parentMapping.getLevel() + 1;
      }

      final FixedMessagePartMapping mapping = new FixedMessagePartMapping();

      mapping.setLevel(nThisLevel);
      mapping.setMessagePart(part);

      // the "records" level
      if (nThisLevel == 1)
      {
         if (mappingElement == null)
         {
            return mapping;
         }

         if (XMLUtil.getIntAttr(mappingElement, WIDTH_ATTRIBUTE) != 0)
         {
            throw new MetadataException("err.meta.integration.fixed.mustNotSetRecordWidth",
               new Object[]{msg.getName()});
         }
      }

      // the individual fields level
      if (nThisLevel > 1)
      {
         if (mappingElement == null)
         {
            throw new MetadataException("err.meta.integration.fixed.missingWidth",
               new Object[]{part.getFullPath()});
         }

         mapping.setWidth(XMLUtil.getReqIntAttr(mappingElement, WIDTH_ATTRIBUTE));

         String s = parseStringAttr(mappingElement, PADDING_ATTRIBUTE, " ");

         if (s.length() != 1)
         {
            throw new MetadataException("err.meta.integration.fixed.paddingLengthMustBeOne",
               new Object[]{Primitive.createInteger(s.length()), part.getFullPath()});
         }

         mapping.setPadding(s.charAt(0));

         String sAlignment = getStringAttr(mappingElement, ALIGNMENT_ATTRIBUTE);

         mapping.setLeftAligned(FixedMessagePartMapping.ALIGNMENT_LEFT.equals(sAlignment));
      }

      mapping.setPrefix(parseStringAttr(mappingElement, PREFIX_ATTRIBUTE, null));
      mapping.setSuffix(parseStringAttr(mappingElement, SUFFIX_ATTRIBUTE, null));

      mapping.setPageSize(getStringAttr(mappingElement, PAGE_SIZE_ATTRIBUTE), parentMapping);

      mapping.setFormat(getStringAttr(mappingElement, FORMAT_ATTRIBUTE));

      return mapping;
   }
}
