// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.json;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;

/**
 * The message part mapping for JSON message composite parts.
 * Contains the message format mode.
 */
public class CompositeJSONMessagePartMapping extends JSONMessagePartMapping
{
   // constants

   /**
    * Default mode: {}.
    */
   public final static byte SUBTYPE_OBJECT = 0;

   /**
    * Array mode: [].
    */
   public final static  byte SUBTYPE_ARRAY = 1;

   /**
    * Root mode (message must contain only one part): [].
    */
   public final static byte SUBTYPE_ROOT = 2;

   /**
    * Sub-type names at indexes corresponding to the sub-type values.
    */
   protected final static String[] SUBTYPE_NAMES = new String[]{"", "array", "root"};

   // attributes

   /**
    * The sub type, one of SUBTYPE_* constants.
    */
   protected byte m_nSubType = SUBTYPE_OBJECT;

   /**
    * The unique message key-value combination.
    */
   protected String m_sMessageKeyValue;

   /**
    * The unique message key.
    */
   protected String m_sMessageKey;

   /**
    * The unique message value.
    */
   protected String m_sMessageValue;

   // operations

   /**
    * Sets the unique message key-value combination.
    * @param sMessageKeyValue The key-value combination.
    */
   public void setMessageKeyValue(String sMessageKeyValue)
   {
      verifyNotReadOnly();
      m_sMessageKeyValue = sMessageKeyValue;

      if (sMessageKeyValue == null)
      {
         m_sMessageKey = null;
         m_sMessageValue = null;
      }
      else
      {
         int i = sMessageKeyValue.indexOf(':');

         if (i < 0)
         {
            m_sMessageKey = sMessageKeyValue;
            m_sMessageValue = "";
         }
         else
         {
            m_sMessageKey = sMessageKeyValue.substring(0, i);
            m_sMessageValue = sMessageKeyValue.substring(i + 1);
         }
      }
   }

   /**
    * @return The unique message key-value combination.
    */
   public String getMessageKeyValue()
   {
      return m_sMessageKeyValue;
   }

   /**
    * @return The unique message key name.
    */
   public String getMessageKey()
   {
      return m_sMessageKey;
   }

   /**
    * @return The unique message key value.
    */
   public String getMessageValue()
   {
      return m_sMessageValue;
   }

   /**
    * @see nexj.core.meta.integration.format.json.JSONMessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      if (part instanceof CompositeMessagePartRef)
      {
         return; // nothing to do.
      }

      CompositeMessagePart compositePart = (CompositeMessagePart)part;
      CompositeJSONMessagePartMapping compositeMapping = (CompositeJSONMessagePartMapping)compositePart.getMapping();
      int nPartCount = compositePart.getPartCount();
      byte nSubtype = compositeMapping.getSubtype();

      if (compositeMapping.getMessageKey() != null && nSubtype != SUBTYPE_OBJECT)
      {
         throw new MetadataException("err.meta.integration.json.nonObjectKey", new Object[]{part.getFullPath()});
      }

      if (nSubtype == SUBTYPE_ROOT && nPartCount != 1)
      {
         throw new MetadataException("err.meta.integration.json.rootChild", new Object[]{part.getFullPath()});
      }

      // call finish on the child parts of this message part.
      for (int i = 0; i < nPartCount; ++i)
      {
         part = compositePart.getPart(i);

         if (part instanceof CompositeMessagePart)
         {
            finish(part);
         }
         else if (part instanceof PrimitiveMessagePart)
         {
            JSONMessagePartMapping primitiveMapping = (JSONMessagePartMapping)part.getMapping();

            if (primitiveMapping != null)
            {
               primitiveMapping.finish(part);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.format.json.JSONMessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      if (m_sFormat != null)
      {
         throw new MetadataException("err.meta.integration.formatComposite", new Object[]{part.getFullPath()});
      }

      super.init(part);
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