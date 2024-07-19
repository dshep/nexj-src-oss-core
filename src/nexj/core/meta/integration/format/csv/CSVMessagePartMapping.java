// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.csv;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.FormatHolder;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Represents a mapping between a MessagePart and a field or set of fields
 * in a CSV file.
 */
public class CSVMessagePartMapping extends MetadataObject implements MessagePartMapping, FormatHolder
{
   // constants
   
   /**
    * The default delimiter for separating CSV fields.
    */
   public final static char DEFAULT_DELIMITER = ',';
   
   /**
    * The default escape character to use for escaping special characters
    * appearing as data in a CSV field.
    */
   public final static char DEFAULT_ESCAPE = '\\';
   
   /**
    * The default quote character to use for quoting fields that contain
    * special characters.
    */
   public final static char DEFAULT_QUOTE = '\"';
   
   /**
    * The default value of the quoted flag.
    */
   public final static boolean DEFAULT_QUOTED = false;
   
   
   // attributes
   
   /**
    * The level number of the associated MessagePart within its message, level 0
    * being the root and increasing from there.
    */
   protected int m_nLevel;
   
   /**
    * The name of the CSV file field to which this mapping maps.
    */
   protected String m_sField;
   
   /**
    * The end of line sequence to use when formatting output.
    */
   protected String m_sLineEnd;

   /**
    * The primitive value format string, if any.
    */
   protected String m_sFormat;

   /**
    * The header flag. Instructs the formatter to output the CSV field names as the first row
    * of CSV output and the parser to ignore the first line of CSV input.
    */
   protected boolean m_bHeader;
   
   /**
    * The character to use as the CSV field delimiter.
    */
   protected Character m_delimiter;
   
   /**
    * A character to use to escape special characters appearing as data
    * in a CSV field.
    */
   protected Character m_escape;
   
   /**
    * A character used to quote fields that contain special characters.
    */
   protected Character m_quote;
   
   /**
    * The quoted flag forces a CSV field to have its data quoted.
    */
   protected boolean m_bQuoted;
   
   /**
    * Each character in the comment characters string is a comment character; lines beginning
    * with a comment character must be ignored by the parser.
    */
   protected String m_sCommentCharacters;
   
   /**
    * The 1-based index of the CSV file field to which this datum should be mapped.
    */
   protected int m_nOrdinal;
   
   /**
    * The highest ordinal of all the mappings on parts below the part associated
    * with this mapping.
    */
   protected int m_nHighestOrdinal;
   
   /**
    * The sequence of keys to use to look up the datum corresponding to
    * this mapping instance in a tree of TransferObjects. The first key
    * in the sequence will be the key of the TransferObject that holds
    * the row data.
    */
   protected String[] m_dataKeyPathArray;

   /**
    * The number of elements per page, for message parts with multiplicity
    * greater than 1.
    */
   protected int m_nPageSize;


   // associations
   
   /**
    * A cache of the message parts in CSV field order for the
    * Message associated with this mapping. This cache will only
    * be set on the mapping instance attached to the root
    * MessagePart of the Message.
    */
   protected PrimitiveMessagePart[] m_cachedPartsArray;
   
   /**
    * A mapping of CSV field names to the message parts associate
    * with those fields. This cache will only be set on the instance
    * of mapping attached to the root MessagePart of the Message.
    */
   protected Lookup m_fieldNamePartMap;
   
   /**
    * The message part to which this mapping belongs.
    */
   protected MessagePart m_messagePart;
   
   
   // operations
   
   /**
    * Sets the level of this mapping within the Message.
    * 
    * @param nLevel The level within the message, must be non-negative.
    */
   public void setLevel(int nLevel)
   {
      verifyNotReadOnly();
      m_nLevel = nLevel;
   }
   
   /**
    * Gets the level of this mapping within the Message.
    * 
    * @return A non-negative value.
    */
   public int getLevel()
   {
      return m_nLevel;
   }
   
   
   /**
    * Sets the name of the CSV file field to which this mapping maps.
    * 
    * @param sField The name of the CSV field.
    */
   public void setField(String sField)
   {
      verifyNotReadOnly();
      m_sField = sField;
   }
   
   /**
    * Gets the name of the CSV file field to which this mapping maps.
    * 
    * @return The name of the CSV field.
    */
   public String getField()
   {
      return m_sField;
   }

   
   /**
    * Sets the end of line sequence to use when formatting output.
    * 
    * @param sLineEnd The end of line sequence to use when formatting output.
    */
   public void setLineEnd(String sLineEnd)
   {
      verifyNotReadOnly();
      m_sLineEnd = sLineEnd;
   }

   /**
    * @return The end of line sequence to use when formatting output.
    */
   public String getLineEnd()
   {
      return m_sLineEnd;
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
    * Sets the end of line sequence to use when formatting output, inheriting
    * end of line sequence from parent if none is specified.
    * 
    * @param sLineEnd The end of line sequence to use when formatting output. 
    * @param parent   Parent of this mapping from which to inherit settings;
    *                 null if this mapping is the root.     
    */ 
   public void setLineEnd(String sLineEnd, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();
      
      if (sLineEnd == null)
      {
         if (parent == null)
         {
            m_sLineEnd = null;
         }
         else
         {
            m_sLineEnd = parent.getLineEnd();
         }
      }
      else
      {
         m_sLineEnd = sLineEnd;
      }
   }
   
   
   /**
    * Sets the header flag.
    * 
    * @param bHeader The new header flag, true to interpret a header when
    *                formatting and parsing the message.
    */
   public void setHeader(boolean bHeader)
   {
      verifyNotReadOnly();
      m_bHeader = bHeader;
   }

   /**
    * Gets the header flag.
    * 
    * @return The header flag, true to interpret a header when formatting
    *         and parsing the message.
    */
   public boolean isHeader()
   {
      return m_bHeader;
   }
   
   
   /**
    * Sets the character to use as the CSV field delimiter.
    * 
    * @param delimiter The character to use as the CSV field delimiter.
    */
   public void setDelimiter(Character delimiter)
   {
      verifyNotReadOnly();
      m_delimiter = delimiter;
   }
   
   /**
    * @return The character to use as the CSV field delimiter.
    */
   public Character getDelimiter()
   {
      return m_delimiter;
   }
   
   /**
    * Sets the character to use to delimit fields when formatting and parsing,
    * inheriting from parent if no delimiter is specified.
    * 
    * @param sDelimiter Character used to delimit fields in CSV output and input; 
    *                   null to inherit from parent.
    * @param parent     Parent of this mapping from which to inherit settings;
    *                   null if this mapping is the root.     
    */ 
   public void setDelimiter(String sDelimiter, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();
      
      if (sDelimiter == null)
      {
         if (parent == null)
         {
            m_delimiter = Primitive.createCharacter(DEFAULT_DELIMITER);
         }
         else
         {
            m_delimiter = parent.getDelimiter();
         }
      }
      else
      {
         if (sDelimiter.length() != 1)
         {
            throw new MetadataException("err.meta.integration.csv.delimiterNotSingleCharacter",
               new Object[]{sDelimiter});
         }
         
         m_delimiter = Primitive.createCharacter(sDelimiter.charAt(0));
      }
   }
   
   
   /**
    * Sets the character to use to escape special characters appearing as data
    * in a CSV field.
    * 
    * @param escape The character to use to escape special characters in the data;
    *               or null to disable escaping of special characters, putting
    *               them in quotes instead (and escaping quotes by repeating
    *               the quote character twice) 
    */
   public void setEscape(Character escape)
   {
      verifyNotReadOnly();
      m_escape = escape;
   }

   /**
    * @return The character to use to escape special characters in the data; or
    *         null if escaping is not to be used.
    */
   public Character getEscape()
   {
      return m_escape;
   }
   
   /**
    * Sets the character to use to escape special characters in the data,
    * inheriting from parent if no escape character is specified.
    * 
    * @param sEscape Character used to escape special characters in the data; 
    *                null to inherit from parent, or "" to disable escaping.
    * @param parent  Parent of this mapping from which to inherit settings;
    *                null if this mapping is the root.     
    */ 
   public void setEscape(String sEscape, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();
      
      if (sEscape == null)
      {
         if (parent == null)
         {
            m_escape = Primitive.createCharacter(DEFAULT_ESCAPE);
         }
         else
         {
            m_escape = parent.getEscape();
         }
      }
      else if ("".equals(sEscape))
      {
         m_escape = null;
      }
      else
      {
         if (sEscape.length() != 1)
         {
            throw new MetadataException("err.meta.integration.csv.escapeNotSingleCharacter",
               new Object[]{sEscape});
         }
         
         m_escape = Primitive.createCharacter(sEscape.charAt(0));
      }
   }
   
   
   /**
    * Sets the character used to quote fields which contain special characters.
    * 
    * @param quote The character to use to quote fields that contain special characters;
    *              or null to disable quoting of fields, escaping all of the special
    *              characters instead. 
    */
   public void setQuote(Character quote)
   {
      verifyNotReadOnly();
      m_quote = quote;
   }

   /**
    * @return The character used to quote fields that contain special characters;
    *         null if the field should not be quoted.
    */
   public Character getQuote()
   {
      return m_quote;
   }
   
   /**
    * Sets the character used to quote fields that contain special characters,
    * inheriting from parent if no quote character is specified.
    * 
    * @param sQuote Character used to quote CSV fields that contain special characters; 
    *               null to inherit from parent, or "" to disable quoting.
    * @param parent Parent of this mapping from which to inherit settings;
    *               null if this mapping is the root.     
    */ 
   public void setQuote(String sQuote, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();
      
      if (sQuote == null)
      {
         if (parent == null)
         {
            m_quote = Primitive.createCharacter(DEFAULT_QUOTE);
         }
         else
         {
            m_quote = parent.getQuote();
         }
      }
      else if ("".equals(sQuote))
      {
         m_quote = null;
      }
      else
      {
         if (sQuote.length() != 1)
         {
            throw new MetadataException("err.meta.integration.csv.quoteNotSingleCharacter",
               new Object[]{sQuote});
         }

         m_quote = Primitive.createCharacter(sQuote.charAt(0));
      }
   }
   
   
   /**
    * Sets the quoted flag.
    * 
    * @param bQuoted The quoted flag to set.
    */
   public void setQuoted(boolean bQuoted)
   {
      verifyNotReadOnly();
      m_bQuoted = bQuoted;
   }

   /**
    * @return The quoted flag.
    */
   public boolean isQuoted()
   {
      return m_bQuoted;
   }
   
   /**
    * Parses the string for a "true" or "false" setting of the quoted flag, inheriting
    * from parent if there is no string to parse.
    * 
    * @param sQuoted Value of "true" or "false" from which to determine the quoted flag; 
    *                null to inherit from parent.
    * @param parent  Parent of this mapping from which to inherit settings;
    *                null if this mapping is the root.     
    */ 
   public void setQuoted(String sQuoted, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();
      
      if (sQuoted == null)
      {
         if (parent == null)
         {
            m_bQuoted = DEFAULT_QUOTED;
         }
         else
         {
            m_bQuoted = parent.isQuoted();
         }
      }
      else
      {
         m_bQuoted = Primitive.toBoolean(sQuoted).booleanValue();
      }
   }
   
   
   /**
    * Sets the comment characters string to the given argument. Lines beginning with a comment
    * character must be ignored by the parser.
    * 
    * @param sCommentCharacterss A string containing all the characters to use as comment
    *                            characters; null or "" if no character should be
    *                            considered a comment.
    */
   public void setCommentCharacters(String sCommentCharacters)
   {
      verifyNotReadOnly();
      
      if (sCommentCharacters != null && sCommentCharacters.length() == 0)
      {
         m_sCommentCharacters = null;
      }
      
      m_sCommentCharacters = sCommentCharacters;
   }
      
   /**
    * Gets the comment characters string which contains all the comment characters.
    * Lines beginning with a comment character must be ignored by the parser, and
    * the formatter will escape a comment character that begins the first field
    * of output on any row.
    * 
    * @return A string containing all the characters to use as comment characters;
    *         null if no character should be considered a comment initiator.
    */
   public String getCommentCharacters()
   {
      return m_sCommentCharacters;
   }
   
   /**
    * Sets the comment characters string to the given argument. Lines beginning with
    * a comment character must be ignored by the parser, and the formatter will
    * escape a comment character that begins the first field of output on any row.
    * 
    * @param sCommentCharacters A string containing all the characters to use as
    *                           comment characters; null to inherit from parent.
    * @param parent             Parent of this mapping from which to inherit settings;
    *                           null if this mapping is the root.
    */
   public void setCommentCharacters(String sCommentCharacters, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();
      
      if (sCommentCharacters == null)
      {
         if (parent == null)
         {
            m_sCommentCharacters = null;
         }
         else
         {
            m_sCommentCharacters = parent.getCommentCharacters();
         }
      }
      else
      {
         m_sCommentCharacters = sCommentCharacters;
      }
   }
   
   
   /**
    * Sets the index of the CSV file field (i.e. column) to which this datum should be mapped.
    * 
    * @param nOrdinal The 1-based index of the CSV file field to which this datum should be mapped.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      
      if (nOrdinal < 1)
      {
         throw new MetadataException("err.meta.integration.csv.ordinalOutOfBounds",
            new Object[] {Primitive.createInteger(nOrdinal)});
      }
      
      m_nOrdinal = nOrdinal;
   }
   
   /**
    * Gets the index of the CSV file field (i.e. column) to which this datum should be mapped.
    * 
    * @return The 1-based index of the CSV file field to which this datum should be mapped.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }
   
   /**
    * Gets whether or not this mapping maps to the first CSV file field.
    * 
    * @return True if this mapping maps to the first column.
    */
   public boolean isFirstField()
   {
      return (m_nOrdinal == 1);
   }
   
   
   /**
    * Gets the sequence of keys to use to look up the corresponding datum
    * in a tree of TransferObjects.
    * 
    * @return The sequence of keys to use to look up the datum
    *         corresponding to this mapping instance.
    */
   public String[] getDataKeyPath()
   {
      return m_dataKeyPathArray;
   }

   /**
    * Sets the message part to which this mapping belongs.
    * 
    * @param messagePart The message part to associate with this mapping.
    */
   public void setMessagePart(MessagePart messagePart)
   {
      verifyNotReadOnly();
      m_messagePart = messagePart;
   }
   
   /**
    * Gets the message part to which this mapping belongs.
    * 
    * @return The message part associated with this mapping.
    */
   public MessagePart getMessagePart()
   {
      return m_messagePart;
   }
   
   
   /**
    * Gets the highest ordinal of all the mappings on parts below the part associated
    * with this mapping.
    * 
    * @return The highest ordinal value to which a sub-part maps.
    */
   public int getHighestOrdinal()
   {
      return m_nHighestOrdinal;
   }


   /**
    * Sets the number of elements per page for message parts with multiplicity
    * greater than 1.
    * 
    * @param nPageSize The number of elements per page.
    */
   public void setPageSize(int nPageSize)
   {
      verifyNotReadOnly();
      m_nPageSize = nPageSize;
   }

   /**
    * Sets the number of elements per page for message parts with multiplicity
    * greater than 1.
    * 
    * @param sPageSize A string containing the page size; null to inherit from
    *                  parent.
    * @param parent The parent from which to inherit if page size not specified;
    *               null to use default of 0.
    */
   public void setPageSize(String sPageSize, CSVMessagePartMapping parent)
   {
      verifyNotReadOnly();

      if (sPageSize == null)
      {
         if (parent == null)
         {
            m_nPageSize = 0;
         }
         else
         {
            m_nPageSize = parent.getPageSize();
         }
      }
      else
      {
         m_nPageSize = Primitive.toInteger(sPageSize).intValue();
      }
   }

   /**
    * Gets the number of elements per page for message parts with multiplicity
    * greater than 1.
    * 
    * @return The number of elements per page.
    */
   public int getPageSize()
   {
      return m_nPageSize;
   }


   /**
    * Gets the message parts in CSV field order for the Message associated with
    * this mapping. This is only valid on the mapping attached to the root
    * message part of a message. 
    * 
    * @return The message parts in CSV field order.
    */
   public PrimitiveMessagePart[] getPartsInOrder()
   {
      return m_cachedPartsArray;
   }
   
   
   /**
    * Gets the message part for a specific CSV field.
    * 
    * @param sFieldName The name of the CSV field.
    * @return The message part that is mapped to the given field name.
    */
   public MessagePart getPartByFieldName(String sFieldName)
   {
      return (MessagePart)m_fieldNamePartMap.get(sFieldName);
   }

   /**
    * This initialization routine must be called on the instance attached to the
    * root part of a Message, but only after the composite message parts have
    * had their children populated. This routine computes the ordinal to part
    * array, the field name to part lookup, verifies correct ordering of
    * required and optional parts, ensures that no message parts map to
    * the same CSV field ordinal, and checks that none of the CSV fields have
    * the same name.
    */
   public void finalizeInitialization(CompositeMessagePart rootPart)
   {
      verifyNotReadOnly();
      
      computeOrdinalValues(rootPart, 1);
      m_cachedPartsArray = makePartsInOrder(rootPart);
      
      /* 
       * Verify that ordinal mapping of parts has all required parts before
       * optional parts begin. Also verify that there are no mappings that
       * map to the same CSV field name.
       */
      boolean bOnRequired = true;
      Lookup fieldNames = new HashTab(m_cachedPartsArray.length);
      
      for (int nPartIndex=0; nPartIndex < m_cachedPartsArray.length; nPartIndex++)
      {
         if (m_cachedPartsArray[nPartIndex] != null)
         {
            if (!bOnRequired && m_cachedPartsArray[nPartIndex].isRequired())
            {
               throw new MetadataException("err.meta.integration.csv.cannotMixRequiredAndOptionalParts",
                  new Object[]{m_cachedPartsArray[nPartIndex].getFullPath()});
            }
            
            bOnRequired = m_cachedPartsArray[nPartIndex].isRequired();
            
            CSVMessagePartMapping partMapping = (CSVMessagePartMapping)m_cachedPartsArray[nPartIndex].getMapping();
            MessagePart partWithSameFieldName = (MessagePart)fieldNames.get(partMapping.getField());
            
            if (partWithSameFieldName != null)
            {
               throw new MetadataException("err.meta.integration.csv.duplicateFieldNames",
                  new Object[]{
                     partWithSameFieldName.getFullPath(),
                     m_cachedPartsArray[nPartIndex].getFullPath(),
                     partMapping.getField()
                  });
            }
            else
            {
               fieldNames.put(partMapping.getField(), m_cachedPartsArray[nPartIndex]);
            }
         }
      }
      
      m_fieldNamePartMap = fieldNames;
   }
   
   
   /**
    * Infer ordinal values for message parts that have not been explicitly
    * initialized with an ordinal value. This routine also computes the
    * highest ordinal field, which is used to optimize several algorithms
    * that work with CSV message parts.
    * 
    * @param nCurrentOrdinal The ordinal to assign to the first mapping found
    *                        which does not have ordinal set.
    * @return The ordinal that should be assigned to the next mapping found.
    */
   protected int computeOrdinalValues(MessagePart part, int nCurrentOrdinal)
   {
      CompositeMessagePart compositePart = (CompositeMessagePart)part;
      int nChildren = compositePart.getPartCount();
      
      for (int nChild = 0; nChild < nChildren; nChild++)
      {
         MessagePart childPart = compositePart.getPart(nChild);
         CSVMessagePartMapping childMapping = (CSVMessagePartMapping)childPart.getMapping();
         
         if (childPart instanceof PrimitiveMessagePart)
         {
            if (childMapping.m_nOrdinal <= 0)
            {
               childMapping.m_nOrdinal = nCurrentOrdinal;
            }
            else
            {
               nCurrentOrdinal = childMapping.m_nOrdinal;
            }
            
            if (nCurrentOrdinal > m_nHighestOrdinal)
            {
               m_nHighestOrdinal = nCurrentOrdinal;
            }
            
            nCurrentOrdinal++;
         }
         else
         {
            nCurrentOrdinal = childMapping.computeOrdinalValues(childPart, nCurrentOrdinal);
            
            if (childMapping.m_nHighestOrdinal > m_nHighestOrdinal)
            {
               m_nHighestOrdinal = childMapping.m_nHighestOrdinal;
            }
         }
      }
      
      return nCurrentOrdinal;
   }
   
   
   /**
    * Computes a list of PrimitiveMessageParts in CSV field order for the
    * children of the CompositeMessagePart with which this mapping
    * is associated.
    * 
    * @return The children of the message part associated with this mapping,
    *         in CSV field order.
    */
   protected PrimitiveMessagePart[] makePartsInOrder(CompositeMessagePart part)
   {      
      PrimitiveMessagePart[] result = new PrimitiveMessagePart[m_nHighestOrdinal];
      int nChildren = part.getPartCount();
      
      for (int nChildIndex = 0; nChildIndex < nChildren; nChildIndex++)
      {
         MessagePart childPart = part.getPart(nChildIndex);

         if (childPart instanceof PrimitiveMessagePart)
         {
            CSVMessagePartMapping childMapping = (CSVMessagePartMapping)childPart.getMapping();

            if (result[childMapping.getOrdinal() - 1] != null)
            {
               throw new MetadataException("err.meta.integration.csv.duplicateOrdinal",
                  new Object[] {
                     childPart.getFullPath(),
                     result[childMapping.getOrdinal() -1].getFullPath(),
                     Primitive.createInteger(childMapping.getOrdinal())
                  });
            }
            
            result[childMapping.getOrdinal() - 1] = (PrimitiveMessagePart)childPart;
         }
         else
         {
            PrimitiveMessagePart[] merge = ((CSVMessagePartMapping)childPart.getMapping()).makePartsInOrder((CompositeMessagePart)childPart);
            
            //merge[i] -> result[i], for all i, with checking
            for (int nMergeIndex = 0; nMergeIndex < merge.length; nMergeIndex++)
            {
               if (merge[nMergeIndex] != null)
               {
                  if (result[nMergeIndex] != null)
                  {
                     throw new MetadataException("err.meta.integration.csv.duplicateOrdinal",
                        new Object[] {
                           result[nMergeIndex].getFullPath(),
                           merge[nMergeIndex].getFullPath(),
                           Primitive.createInteger(nMergeIndex + 1)
                        });
                  }
                  
                  result[nMergeIndex] = merge[nMergeIndex];
               }
            }
         }
      }
      
      return result;
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      m_messagePart = part;

      //Cannot force quoting unless quote character is set.
      if (getQuote() == null && isQuoted())
      {
         throw new MetadataException("err.meta.integration.csv.quotedButMissingQuote",
            new Object[]{part.getFullPath()});
      }
      
      //Verify that there is no overlap between the delimiter, escape, and quote characters.
      if (getQuote() != null)
      {
         if (getQuote().equals(getDelimiter()) ||
             getQuote().equals(getEscape()))
         {
            throw new MetadataException("err.meta.integration.csv.quoteDelimEscapeSimilar",
               new Object[]{part.getFullPath(), getQuote(), getEscape(), getDelimiter()});
         }
      }
      
      if (getEscape() != null && getEscape().equals(getDelimiter()))
      {
         throw new MetadataException("err.meta.integration.csv.quoteDelimEscapeSimilar",
            new Object[]{part.getFullPath(), getQuote(), getEscape(), getDelimiter()});
      }
            
      //Level-specific checks for validity
      if (getLevel() == 1)
      {
         //The second level should have a CompositeMessagePart of 0..* cardinality
         if (!part.isCollection())
         {
            throw new MetadataException("err.meta.integration.csv.mustHaveToplevelCollection",
               new Object[]{part.getFullPath()});
         }
         
         if (part.getParent().getPartCount() != 1)
         {
            throw new MetadataException("err.meta.integration.csv.onlyOneTopLevel",
               new Object[]{part.getFullPath()});
         }
      }
      else if (getLevel() > 1)
      {
         //If beyond second level, and CompositeMessagePart, then enforce 0..1 cardinality.
         if (part.isCollection())
         {
            throw new MetadataException("err.meta.integration.csv.noNestedCollections",
               new Object[]{part.getFullPath()});
         }
      }
      
      
      //Build the path of keys to use for accessing TransferObject data for this part
      if (part instanceof PrimitiveMessagePart)
      {
         String[] keyPath = new String[getLevel() - 1];
         MessagePart keyPathPart = part;
         
         for (int nKeyPathIndex = getLevel() - 2; nKeyPathIndex >= 0; nKeyPathIndex--)
         {
            keyPath[nKeyPathIndex] = keyPathPart.getName();
            keyPathPart = keyPathPart.getParent();            
         }
         
         m_dataKeyPathArray = keyPath;
      }
      else
      {
         m_dataKeyPathArray = null;

         if (m_sFormat != null)
         {
            throw new MetadataException("err.meta.integration.formatComposite",
               new Object[]{part.getFullPath()});
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
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      if (m_nLevel == 0)
      {
         finalizeInitialization((CompositeMessagePart)m_messagePart);
      }

      if (m_messagePart instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePart composite = (CompositeMessagePartInstance)m_messagePart;

         for (int i = 0, nCount = composite.getPartCount(); i < nCount; i++)
         {
            MessagePart child = composite.getPart(i);
            MessagePartMapping mapping = child.getMapping();

            if (mapping != null)
            {
               mapping.finish(child);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      CSVMessagePartMapping copy = (CSVMessagePartMapping)super.clone();

      copy.m_nHighestOrdinal = 0;

      return copy;
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder, nexj.core.meta.integration.MessagePart)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part)
   {
   }
}
