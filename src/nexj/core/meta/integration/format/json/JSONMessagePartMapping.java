// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.json;

import java.util.regex.Pattern;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.FormatHolder;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.StringUtil;

/**
 * The message part mapping for JSON message primitive parts.
 */
public class JSONMessagePartMapping extends MetadataObject implements MessagePartMapping, FormatHolder
{
   // constants

   /**
    * No specific sub-type.
    */
   public final static byte SUBTYPE_DEFAULT = 0;

   /**
    * Date/Time sub-type of "timestamp".
    */
   public final static byte SUBTYPE_DATETIME = 1;

   /**
    * Date sub-type of "timestamp".
    */
   public final static byte SUBTYPE_DATE = 2;

   /**
    * Time sub-type of "timestamp".
    */
   public final static byte SUBTYPE_TIME = 3;

   /**
    * Base64 sub-type of "binary".
 */
   public final static byte SUBTYPE_BASE64 = 4;

   /**
    * Hex sub-type of "binary".
    */
   public final static byte SUBTYPE_HEX = 5;

   /**
    * Sub-type names at indexes corresponding to the sub-type values.
    */
   protected final static String[] SUBTYPE_NAMES = new String[]{"", "dateTime", "date", "time", "base64", "hex"};

   /**
    * Pattern to check if format string contains non numeric characters.
    */
   protected final static Pattern NON_NUMERIC_CHARACTERS = Pattern.compile("[^#.0]");

   // attributes

   /**
    * The value format string.
    */
   protected String m_sFormat;

   /**
    * The JSON object key name.
    */
   protected String m_sKeyName;

   /**
    * Whether or not to quote the JSON value.
    */
   protected boolean m_bQuoted;

   /**
    * The value sub-type, one of the SUBTYPE_* constants.
    */
   protected byte m_nSubtype;

   // operations

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
    * Sets the JSON object key name.
    * @param sKeyName The JSON object key name to set.
    */
   public void setKeyName(String sKeyName)
   {
      verifyNotReadOnly();
      m_sKeyName = sKeyName;
   }

   /**
    * @return The JSON object key name.
    */
   public String getKeyName()
   {
      return m_sKeyName;
   }

   /**
    * Sets the value sub-type.
    * @param nSubtype The value sub-type to set.
    */
   public void setSubtype(byte nSubtype)
   {
      verifyNotReadOnly();
      m_nSubtype = nSubtype;
   }

   /**
    * @return The value sub-type.
    */
   public byte getSubtype()
   {
      return m_nSubtype;
   }

   /**
    * @return True to quote the primitive value when generating JSON.
    */
   public boolean isQuoted()
   {
      return m_bQuoted;
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      verifyNotReadOnly();

      if (!StringUtil.isEmpty(m_sFormat))
      {
         // determine if quoting is required
         switch (((PrimitiveMessagePart)part).getType().getOrdinal())
         {
            case Primitive.ANY_ORDINAL:
               throw new MetadataException("err.meta.integration.json.anyFormat", new Object[]{part.getFullPath()});

            case Primitive.INTEGER_ORDINAL:
            case Primitive.LONG_ORDINAL:
            case Primitive.DECIMAL_ORDINAL:
            case Primitive.FLOAT_ORDINAL:
            case Primitive.DOUBLE_ORDINAL:
               //check if format will introduce non numeric characters
               if (NON_NUMERIC_CHARACTERS.matcher(m_sFormat).find())
               {
                  m_bQuoted = true;
               }

               break;

            case Primitive.TIMESTAMP_ORDINAL:
               m_bQuoted = true;

               break;

            case Primitive.BOOLEAN_ORDINAL:
               if (!"true;false".equals(m_sFormat) && !"false;true".equals(m_sFormat))
               {
                  m_bQuoted = true;
               }

               break;
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      // Nothing to do.
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#refer(nexj.core.meta.integration.CompositeMessagePartRef)
    */
   public void refer(CompositeMessagePartRef ref)
   {
      // Nothing to do.
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#resolveInheritance(nexj.core.meta.integration.MessagePartMapping)
    */
   public void resolveInheritance(MessagePartMapping baseMapping)
   {
      // Nothing to do.
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder, nexj.core.meta.integration.MessagePart)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part)
   {
      // Nothing to do.
   }

   /**
    * Parses a sub-type name to an ordinal.
    * @param sSubtype The sub-type name.
    * @return The sub-type ordinal; -1 if not found.
    */
   public static byte parseSubtype(String sSubtype)
   {
      for (byte i = 0; i < SUBTYPE_NAMES.length; ++i)
      {
         if (SUBTYPE_NAMES[i].equals(sSubtype))
         {
            return i;
         }
      }

      return -1;
   }
}