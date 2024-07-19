// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.vcard;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.util.ExceptionHolder;

/**
 * Represents the mapping between a message part and a vCard field or group of fields.
 */
public class VCardMessagePartMapping extends MetadataObject implements MessagePartMapping
{
   // constants

   /**
    * The default encoding to use.
    */
   public final static String DEFAULT_ENCODING = "US-ASCII";

   /**
    * Map to the value part of the vCard field.
    */
   public final static byte TYPE_VALUE = 0;

   /**
    * Map to the value of a parameter on the vCard field. Name attribute on mapping
    * is used as the parameter name.
    */
   public final static byte TYPE_PARAMETER = 1;

   /**
    * Logical grouping of child parts. If name attribute on mapping is not empty, it
    * is prefixed to every field name in the group.
    */
   public final static byte TYPE_GROUP = 2;

   /**
    * Wrap lines only on whitespace.
    */
   public final static byte WRAPPING_WHITESPACE = 0;

   /**
    * Wrap lines anywhere.
    */
   public final static byte WRAPPING_ANYWHERE = 1;

   /**
    * Do not encode non-printable data.
    */
   public final static byte QUOTING_NONE = 0;

   /**
    * Encode non-printable data as base64.
    */
   public final static byte QUOTING_BASE64 = 1;

   /**
    * Encode non-printable data using MIME Quoted-Printable encoding.
    */
   public final static byte QUOTING_QP = 2;

   /**
    * Encode non-printable data and delimiters using the vCard (backslash-escaped) encoding.
    */
   public final static byte QUOTING_VCARD = 3;

   /**
    * No specific subtype.
    */
   public final static byte SUBTYPE_DEFAULT = 0;

   /**
    * Date/Time subtype.
    */
   public final static byte SUBTYPE_DATETIME = 1;

   /**
    * Date subtype.
    */
   public final static byte SUBTYPE_DATE = 2;


   // attributes

   /**
    * Name for this part in the vCard message.
    */
   protected String m_sName;

   /**
    * The full name for this part in the vCard message, including all
    * parent group names.
    */
   protected String m_sFullName;

   /**
    * Default value to use when there is no corresponding value in the
    * TransferObject.
    */
   protected String m_sDefault;

   /**
    * The Java character set to use for encoding/decoding the field value.
    */
   protected String m_sEncoding = DEFAULT_ENCODING;

   /**
    * Delimiter for a primitive message part collection.
    */
   protected char m_chDelimiter = ',';

   /**
    * The type of vCard element to which the message part is mapped. One of
    * the TYPE_* constants.
    */
   protected byte m_nType;

   /**
    * The line wrapping mode. One of the WRAPPING_* constants.
    */
   protected byte m_nWrapping;

   /**
    * The quoting mode for non-printable data and delimiters. One of the QUOTING_*
    * constants.
    */
   protected byte m_nQuoting;

   /**
    * The value subtype.
    */
   protected byte m_nSubtype;


   // associations

   /**
    * The value part for this vCard field.
    */
   protected MessagePart m_valuePart;


   // operations

   /**
    * Sets the name for this part in the vCard message.
    * @param sName The name for this part in the vCard message.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = sName;
   }

   /**
    * Gets the name for this part in the vCard message.
    * @return The name for this part in the vCard message.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Gets the full name for this part in the vCard message.
    * @return The full name, prefixed by all parent group names.
    */
   public String getFullName()
   {
      return m_sFullName;
   }

   /**
    * Sets the default value to use when there is no corresponding value
    * in the TransferObject.
    * @param sDefault The default value to use; null to omit.
    */
   public void setDefault(String sDefault)
   {
      verifyNotReadOnly();
      m_sDefault = sDefault;
   }

   /**
    * Gets the default value to use when there is no corresponding value
    * in the TransferObject.
    * @return The default value; null if no default.
    */
   public String getDefault()
   {
      return m_sDefault;
   }

   /**
    * Sets the Java character set to use for encoding/decoding
    * the field value.
    * @param sEncodingName The name of the character set.
    */
   public void setEncoding(String sEncodingName)
   {
      verifyNotReadOnly();
      m_sEncoding = sEncodingName;
   }

   /**
    * Gets the Java character set to use for encoding/decoding
    * the field value.
    * @return The name of the character set.
    */
   public String getEncoding()
   {
      return m_sEncoding;
   }

   /**
    * Sets the delimiter to use for a primitive message part collection.
    * @param chDelimiter The delimiter character to use.
    */
   public void setDelimiter(char chDelimiter)
   {
      verifyNotReadOnly();
      m_chDelimiter = chDelimiter;
   }

   /**
    * Gets the delimiter to use for a primitive message part collection.
    * @return The delimiter character.
    */
   public char getDelimiter()
   {
      return m_chDelimiter;
   }

   /**
    * Sets the type of the vCard element to which the message part is mapped.
    * @param nType One of the TYPE_* constants.
    */
   public void setType(byte nType)
   {
      verifyNotReadOnly();

      if (nType < TYPE_VALUE || nType > TYPE_GROUP)
      {
         throw new IllegalStateException("Unknown \"type\": " + nType);
      }

      m_nType = nType;
   }

   /**
    * Gets the type of the vCard element to which the message part is mapped.
    * @return One of the TYPE_* constants.
    */
   public byte getType()
   {
      return m_nType;
   }

   /**
    * Sets the value subtype.
    * @param nSubtype The value subtype to set, one of the SUBTYPE_* constants.
    */
   public void setSubtype(byte nSubtype)
   {
      verifyNotReadOnly();
      m_nSubtype = nSubtype;
   }

   /**
    * @return The value subtype, one of the SUBTYPE_* constants.
    */
   public byte getSubtype()
   {
      return m_nSubtype;
   }

   /**
    * Sets the line wrapping mode.
    * @param nWrapping One of the WRAPPING_* constants.
    */
   public void setWrapping(byte nWrapping)
   {
      verifyNotReadOnly();

      if (nWrapping < WRAPPING_WHITESPACE || nWrapping > WRAPPING_ANYWHERE)
      {
         throw new IllegalStateException("Unknown \"wrapping\": " + nWrapping);
      }

      m_nWrapping = nWrapping;
   }

   /**
    * Gets the line wrapping mode.
    * @return One of the WRAPPING_* constants.
    */
   public byte getWrapping()
   {
      return m_nWrapping;
   }

   /**
    * Sets the quoting mode for non-printable data and delimiters.
    * @param nQuoting One of the QUOTING_* constants.
    */
   public void setQuoting(byte nQuoting)
   {
      verifyNotReadOnly();

      if (nQuoting < QUOTING_NONE || nQuoting > QUOTING_VCARD)
      {
         throw new IllegalStateException("Unknown \"quoting\": " + nQuoting);
      }

      m_nQuoting = nQuoting;
   }

   /**
    * Gets the quoting mode for non-printable data and delimiters.
    * @return One of the QUOTING_* constants.
    */
   public byte getQuoting()
   {
      return m_nQuoting;
   }

   /**
    * Gets the value part for this vCard field.
    * @return The value part.
    */
   public MessagePart getValuePart()
   {
      return m_valuePart;
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
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
    * Resolves the value-part field of a value-mapped composite message part.
    * 
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      CompositeMessagePart parentPart = part.getParent();

      m_sFullName = m_sName;

      if (parentPart != null)
      {
         VCardMessagePartMapping parentMapping = (VCardMessagePartMapping)parentPart.getMapping();

         if (parentMapping.getType() == TYPE_GROUP)
         {
            if (m_nType == TYPE_PARAMETER)
            {
               throw new MetadataException("err.meta.integration.vcard.invalidParameterPlacement",
                  new Object[]{part.getFullPath()});
            }
         }

         if (parentMapping.getName().length() > 0)
         {
            m_sFullName = parentMapping.getFullName() + '.' + m_sName;
         }
      }

      if (part instanceof PrimitiveMessagePart)
      {
         if (m_nType == TYPE_GROUP)
         {
            throw new MetadataException("err.meta.integration.vcard.groupMappedPrimitive",
               new Object[]{part.getFullPath()});
         }
      }
      else if (part instanceof CompositeMessagePart)
      {
         if (m_nType == TYPE_PARAMETER)
         {
            throw new MetadataException("err.meta.integration.vcard.parameterMappedComposite",
               new Object[]{part.getFullPath()});
         }
      }

      if (part instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePart composite = (CompositeMessagePart)part;

         if (m_nType == VCardMessagePartMapping.TYPE_VALUE)
         {
            int nChildCount = composite.getPartCount();

            for (int i = 0; i < nChildCount; i++)
            {
               MessagePart childPart = (MessagePart)composite.getPart(i);
               VCardMessagePartMapping childMapping = (VCardMessagePartMapping)childPart.getMapping();

               if (childMapping.getType() == TYPE_VALUE || childMapping.getType() == TYPE_GROUP)
               {
                  if (m_valuePart != null)
                  {
                     throw new MetadataException("err.meta.integration.vcard.singleValueChildRequired",
                        new Object[]{part.getFullPath()});
                  }

                  m_valuePart = childPart;
               }
            }

            if (m_valuePart == null)
            {
               throw new MetadataException("err.meta.integration.vcard.singleValueChildRequired",
                  new Object[]{part.getFullPath()});
            }
         }

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
    * @see nexj.core.meta.integration.MessagePartMapping#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder, nexj.core.meta.integration.MessagePart)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part)
   {
   }
}
