// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.csv;

import java.io.StringReader;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import nexj.core.meta.MetadataException;
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
 * the CSV output file. These mappings are all computed from the
 * metadata.
 *
 * @see nexj.core.meta.integration.XMLMessageMappingLoader
 */
public class XMLCSVMessageMappingLoader implements XMLMessageMappingLoader
{
   // constants

   /**
    * The name of the mapping tag attribute that specifies the name
    * of the CSV field to which the associated MessagePart should
    * be mapped. Used in the generation of the CSV file header line.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String FIELD_NAME_ATTRIBUTE = "field";

   /**
    * The name of the mapping tag attribute that specifies the formatting
    * string for this part.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String FORMAT_ATTRIBUTE = "format";

   /**
    * The name of the mapping tag attribute that specifies whether
    * or not the first line of the CSV file contains column headers.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String HEADER_FLAG_ATTRIBUTE = "header";

   /**
    * The name of the mapping tag attribute that specifies whether
    * or not to force a CSV field to be quoted.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String QUOTED_FLAG_ATTRIBUTE = "quoted";

   /**
    * The name of the mapping tag attribute that specifies the end of
    * line sequence to use when outputting a CSV file.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String LINE_ENDINGS_ATTRIBUTE = "line";

   /**
    * The name of the mapping tag attribute that specifies the character
    * to use to quote a field that contains one or more special characters.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String QUOTE_CHAR_ATTRIBUTE = "quote";

   /**
    * The name of the mapping tag attribute that specifies the character to
    * use to escape special characters.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String ESCAPE_CHAR_ATTRIBUTE = "escape";

   /**
    * The name of the mapping tag attribute that specifies the character to
    * use as a field delimiter.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String DELIMITER_CHAR_ATTRIBUTE = "delimiter";

   /**
    * The name of the mapping tag attribute that specifies the characters to
    * use as single-line comment initiators.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String COMMENT_CHARS_ATTRIBUTE = "comment";

   /**
    * The name of the mapping tag attribute that specifies the index of the
    * CSV file field (i.e. column) to which a datum should be mapped.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String ORDINAL_ATTRIBUTE = "ordinal";

   /**
    * The name of the mapping tag attrbute that specifies the number of elements
    * per page (that is swapped to disk) for a message part mapping with
    * multiplicity greater than 1.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String PAGE_SIZE_ATTRIBUTE = "pageSize";

   /**
    * The name of the XML tag that is used to specify additional information
    * about the mapping between the Message and the CSV file.
    *
    * @see "nexj.core.meta.metadata.xsd"
    */
   protected final static String CSV_XML_TAG_NAME = "CSVMapping";

   /**
    * The value used for attributes like quote and escape when they need to
    * be disabled. Since they inherit from their parent level, they cannot
    * be disabled by omission. Since the property editor doesn't visually
    * distinguish between blank attributes and attributes not present, a
    * blank string cannot be used for this purpose, either. Therefore, we
    * require the user to specify the magic value "none" to disable the
    * attribute.
    */
   protected final static String BLANK_ATTRIBUTE_VALUE = "none";


   // associations

   /**
    * The parser for interpreting strings with backslash-escaped characters.
    */
   protected StringParser m_stringParser;


   // constructors

   /**
    * Default constructor to provide member initialization.
    */
   public XMLCSVMessageMappingLoader()
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
    * Gets the attribute of given name from the XML node, returning
    * null if no node is specified or attribute is not found. If
    * attribute matches BLANK_ATTRIBUTE_VALUE, then return the
    * empty string.
    *
    * @param node  The node to look at.
    * @param sName The name of the attribute to retrieve.
    * @return The value of the attribute, null if the attribute is
    *         not set or node is null, or "" (empty string) if
    *         attribute value matches BLANK_ATTRIBUTE_VALUE.
    */
   protected String getStringAttrWithNone(Node node, String sName)
   {
      if (node == null)
      {
         return null;
      }

      String sValue = XMLUtil.getStringAttr(node, sName);

      if (sValue != null && BLANK_ATTRIBUTE_VALUE.equalsIgnoreCase(sValue))
      {
         return "";
      }

      return sValue;
   }


   /**
    * Gets the attribute of a given name from the XML node,
    * parsing out backslash-escaped characters.
    *
    * @param node  The node to look at.
    * @param sName The name of the attribute to retrieve.
    * @return The result of parsing the attribute, or null
    *         if the attribute is not set or node is null.
    */
   protected String parseStringAttr(Node node, String sName)
   {
      String sToParse = getStringAttr(node, sName);

      if (sToParse == null)
      {
         return null;
      }

      return (String)m_stringParser.parse(new StringReader(sToParse), null);
   }


   /**
    * Gets the attribute of a given name from the XML node,
    * parsing out backslash-escaped characters. If attribute
    * matches BLANK_ATTRIBUTE_VALUE, then return the empty
    * string.
    *
    * @param node  The node to look at.
    * @param sName The name of the attribute to retrieve.
    * @return The result of parsing the attribute, or null
    *         if the attribute is not set or node is null,
    *         or "" (empty string) if attribute value matches
    *         BLANK_ATTRIBUTE_VALUE.
    */
   protected String parseStringAttrWithNone(Node node, String sName)
   {
      String sToParse = getStringAttrWithNone(node, sName);

      if (sToParse == null)
      {
         return null;
      }

      return (String)m_stringParser.parse(new StringReader(sToParse), null);
   }


   /**
    * Creates a CSVMessagePartMapping for a given MessagePart. This routine will be
    * called automatically by the framework, one time for every MessagePart in the
    * Message definition. Scans the given element from the metadata file for
    * additional, CSV-specific instructions, found in CSVMapping tags.
    *
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, MessagePart part, Format format, XMLMetadataLoader loader)
   {
      CSVMessagePartMapping parentMapping = null;
      Element mappingElement = XMLUtil.findChildElement(element, CSV_XML_TAG_NAME);

      //Compute current level
      int nThisLevel;
      CompositeMessagePart parentPart = part.getParent();

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
               new Object[]{CSV_XML_TAG_NAME, msg.getName()});
         }

         parentMapping = (CSVMessagePartMapping)parentPart.getMapping();
         nThisLevel = parentMapping.getLevel() + 1;
      }

      //Mapping is required on root element
      if (nThisLevel <= 0)
      {
         if (mappingElement == null)
         {
            throw new MetadataException("err.meta.integration.csv.requireRootMapping",
               new Object[]{msg.getName()});
         }
      }

      final CSVMessagePartMapping result = new CSVMessagePartMapping();
      result.setLevel(nThisLevel);

      //Process CSV formatting options, inheriting from parent as necessary.
      result.setQuoted(getStringAttr(mappingElement, QUOTED_FLAG_ATTRIBUTE), parentMapping);
      result.setField(getStringAttr(mappingElement, FIELD_NAME_ATTRIBUTE, part.getName()));
      result.setPageSize(getStringAttr(mappingElement, PAGE_SIZE_ATTRIBUTE), parentMapping);

      //Delimiter, comments, and line ends are parsed, to allow for things like \t
      result.setDelimiter(parseStringAttr(mappingElement, DELIMITER_CHAR_ATTRIBUTE), parentMapping);
      result.setCommentCharacters(parseStringAttr(mappingElement, COMMENT_CHARS_ATTRIBUTE), parentMapping);
      result.setLineEnd(parseStringAttr(mappingElement, LINE_ENDINGS_ATTRIBUTE), parentMapping);

      String sEscape = getStringAttrWithNone(mappingElement, ESCAPE_CHAR_ATTRIBUTE);

      if (sEscape == null || sEscape.length() <= 1)
      {
         result.setEscape(sEscape, parentMapping);
      }
      else
      {
         result.setEscape(parseStringAttrWithNone(mappingElement, ESCAPE_CHAR_ATTRIBUTE), parentMapping);
      }

      String sQuote = getStringAttrWithNone(mappingElement, QUOTE_CHAR_ATTRIBUTE);

      if (sQuote == null || sQuote.length() <= 1)
      {
         result.setQuote(sQuote, parentMapping);
      }
      else
      {
         result.setQuote(parseStringAttrWithNone(mappingElement, QUOTE_CHAR_ATTRIBUTE), parentMapping);
      }

      if (mappingElement != null)
      {
         result.setHeader(XMLUtil.getBooleanAttr(mappingElement, HEADER_FLAG_ATTRIBUTE));

         if (mappingElement.hasAttribute(ORDINAL_ATTRIBUTE))
         {
            result.setOrdinal(XMLUtil.getIntAttr(mappingElement, ORDINAL_ATTRIBUTE));
         }
      }

      result.setFormat(getStringAttr(mappingElement, FORMAT_ATTRIBUTE));

      return result;
   }
}
