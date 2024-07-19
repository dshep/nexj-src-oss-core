// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.fixed;

import nexj.core.integration.IntegrationException;
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


/**
 * Represents a mapping between a MessagePart and a field or a record (a set of fields) or the
 * file (the entire set of records) in a Fixed Length Field file ("flat file database").
 */
public class FixedMessagePartMapping extends MetadataObject implements MessagePartMapping, FormatHolder
{
   // constants

   /**
    * Pad data to the left side when filling the field to its full length.
    */
   public final static String ALIGNMENT_LEFT = "left";

   /**
    * The default pad character for fixed length fields.
    */
   public final static char DEFAULT_PAD = ' ';

   // associations

   /**
    * The message part to which this mapping belongs.
    */
   protected MessagePart m_messagePart;

   // attributes

   /**
    * The primitive value format string, if any.
    */
   protected String m_sFormat;

   /**
    * The prefix at the beginning of the field to be ignored when parsing or start the field when formatting.
    * This is typically an empty string for most fixed length field files.
    */
   protected String m_sPrefix = "";

   /**
    * The suffix at the very end of the field to be ignored when parsing or end the field when formatting.
    * This is typically an empty string for most fixed length field files.
    */
   protected String m_sSuffix = "";

   /**
    * A character template of the record used when formatting the data
    */
   protected char[] m_chTemplate;

   /**
    * The level number of the associated MessagePart within its message, level 0
    * being the root and increasing from there.
    */
   protected int m_nLevel;

   /**
    * The width (or length) of the fixed length field.
    */
   protected int m_nWidth;

   /**
    * The number of elements per page, for message parts with multiplicity
    * greater than 1.
    */
   protected int m_nPageSize;

   /**
    * The character to use to pad data to the length for its fixed field.
    */
   protected char m_chPadding;

   /**
    * The side on which to pad a field.
    */
   protected boolean m_bLeftAligned;

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
    * The width of the individual field if the part is primitive or the
    * overall length of the record if the part is the root.
    *
    * @return the width (or length) of this part
    */
   public int getWidth()
   {
      return m_nWidth;
   }

   /**
    * @param nWidth the width (or length) of this part
    */
   public void setWidth(int nWidth)
   {
      verifyNotReadOnly();

      m_nWidth = nWidth;
   }

   /**
   * @see nexj.core.meta.integration.FormatHolder#getFormat()
   */
   public String getFormat()
   {
      return m_sFormat;
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
    * @param nDelta the increase(or decrease) in overall record length
    */
   private void addToRecordWidth(int nDelta)
   {
      m_nWidth += nDelta;
   }

   /**
    * Sets the character used to pad data so it is of field length.
    *
    * @param chPad The character to use for padding.
    */
   public void setPadding(char chPad)
   {
      verifyNotReadOnly();
      m_chPadding = chPad;
   }

   /**
    * @return The character used to pad data to fill it to field length.
    */
   public char getPadding()
   {
      return m_chPadding;
   }

   /**
    * Sets the side of the data on which padding will be applied.
    *
    * @param bAlignLeft True if padding is to the left, false if to the right.
    */
   public void setLeftAligned(boolean bAlignLeft)
   {
      verifyNotReadOnly();
      m_bLeftAligned = bAlignLeft;
   }

   /**
    * @return True if the field is left aligned, false if it is right aligned.
    */
   public boolean isLeftAligned()
   {
      return m_bLeftAligned;
   }

   /**
    * @return the prefix for this field
    * @see m_sPrefix
    */
   public String getPrefix()
   {
      return m_sPrefix;
   }

   /**
    * @param sPrefix the prefix to for this field
    */
   public void setPrefix(String sPrefix)
   {
      verifyNotReadOnly();
      m_sPrefix = (sPrefix == null) ? "" : sPrefix;
   }

   /**
    * @return the suffix for this field.  Guaranteed not to be null, defaults to "".
    */
   public String getSuffix()
   {
      return m_sSuffix;
   }

   /**
    * @param sSuffix the suffix for this field
    */
   public void setSuffix(String sSuffix)
   {
      verifyNotReadOnly();
      m_sSuffix = (sSuffix == null) ? "" : sSuffix;
   }

   /**
    * Sets the message part to which this mapping belongs.
    *
    * @param part The message part to associate with this mapping.
    */
   public void setMessagePart(MessagePart part)
   {
      verifyNotReadOnly();
      m_messagePart = part;

      MessagePart rootPart = m_messagePart.getRoot();

      // for the records part, add the root's prefix & suffix lengths to the record length
      if (rootPart != null && rootPart != m_messagePart && m_messagePart instanceof CompositeMessagePart)
      {
         FixedMessagePartMapping map = (FixedMessagePartMapping)m_messagePart.getMapping();

         if (map != null)
         {
            addToRecordWidth(map.getPrefix().length() + map.getSuffix().length());
         }
      }
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
   public void setPageSize(String sPageSize, FixedMessagePartMapping parent)
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
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      m_messagePart = part;
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
         CompositeMessagePart rootPart = (CompositeMessagePartInstance)m_messagePart;
         FixedMessagePartMapping rootMapping = (FixedMessagePartMapping)rootPart.getMapping();

         if (rootMapping != null && rootMapping.getSuffix().length() > 0)
         {
            throw new MetadataException("err.meta.integration.fixed.suffixNotAllowedOnRoot",
               new Object[]{part.getFullPath()});
         }

         // call finish() on the records part
         MessagePart recordsPart = rootPart.getPart(0);
         MessagePartMapping recordsMapping = recordsPart.getMapping();

         if (recordsMapping != null)
         {
            recordsMapping.finish(recordsPart);
         }
      }
      else if (m_nLevel == 1)
      {
         //The second level should have a CompositeMessagePart of 0..* cardinality
         if (!part.isCollection())
         {
            throw new MetadataException("err.meta.integration.fixed.mustHaveTopLevelCollection",
               new Object[]{part.getFullPath()});
         }

         // Call finish() for child mappings
         CompositeMessagePart recordPart = (CompositeMessagePartInstance)m_messagePart;
         for (int i=0; i < recordPart.getPartCount(); i++)
         {
            MessagePart fieldPart = recordPart.getPart(i);
            MessagePartMapping fieldMapping = fieldPart.getMapping();

            fieldMapping.finish(fieldPart);
         }

         createRecordTemplate();
      }
      else
      {
         // Beyond second level, no collections allowed
         if (part.isCollection())
         {
            throw new MetadataException("err.meta.integration.fixed.noNestedCollections",
               new Object[]{part.getFullPath()});
         }

         // fields add their width to the record width
         if (m_messagePart != null && m_messagePart instanceof PrimitiveMessagePart)
         {
            MessagePart recordPart = m_messagePart.getParent();
            FixedMessagePartMapping recordMapping = (FixedMessagePartMapping)recordPart.getMapping();

            recordMapping.addToRecordWidth(m_nWidth);
         }
      }
   }

   /**
    * The character template is a CharArrayWriter that already contains each field's prefix and suffix, and
    * whose data area is fully padded.  Users of this will typically write field data to the appropriate
    * offset (based on data, prefix and suffix lengths and padding alignment). Once filled with data,
    * the .writeTo(Writer) method of this template can stream the data out.
    *
    * @param achTemplate the array to fill with the template of this record
    */
   public void fillRecordTemplate(char[] achTemplate) throws IntegrationException
   {
      assert m_messagePart instanceof CompositeMessagePart && m_nLevel == 1;

      System.arraycopy(m_chTemplate, 0, achTemplate, 0, m_chTemplate.length);
   }

   /**
    * @return a newly instantiated and populated character template.
    */
   private void createRecordTemplate() throws IntegrationException
   {
      CompositeMessagePart recordPart = (CompositeMessagePart)m_messagePart;

      m_chTemplate = new char[m_nWidth];

      int nOffset = 0;

      for (int i = 0; i < recordPart.getPartCount(); i++)
      {
         PrimitiveMessagePart fieldPart = (PrimitiveMessagePart)recordPart.getPart(i);
         FixedMessagePartMapping mapping = (FixedMessagePartMapping)fieldPart.getMapping();

         String sToken = mapping.getPrefix();
         int nTokenLen = sToken.length();

         sToken.getChars(0, nTokenLen, m_chTemplate, nOffset);

         nOffset += nTokenLen;

         // calculate the padding length
         nTokenLen = mapping.getWidth() - nTokenLen - mapping.getSuffix().length();

         char chPad = mapping.getPadding();

         for (int j = nOffset; j < nOffset + nTokenLen; j++)
         {
            m_chTemplate[j] = chPad;
         }

         nOffset += nTokenLen;

         sToken = mapping.getSuffix();
         nTokenLen = sToken.length();

         sToken.getChars(0, nTokenLen, m_chTemplate, nOffset);

         nOffset += nTokenLen;
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder, nexj.core.meta.integration.MessagePart)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part)
   {
   }
}
