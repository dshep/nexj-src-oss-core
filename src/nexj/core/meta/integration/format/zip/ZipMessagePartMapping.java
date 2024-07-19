// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.zip;


import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.util.ExceptionHolder;

/**
 * Maps between entries in a Zip file and message parts.
 */
public class ZipMessagePartMapping extends MetadataObject implements MessagePartMapping
{
   // constants

   /**
    * The comment for the entry in the Zip file.
    */
   public final static byte VALUE_COMMENT = 0;

   /**
    * The uncompressed contents of the file.
    */
   public final static byte VALUE_CONTENTS = 1;

   /**
    * True to indicate that this entry is a directory.
    */
   public final static byte VALUE_DIRECTORY = 2;

   /**
    * Optional extra byte array associated with this entry.
    */
   public final static byte VALUE_EXTRA = 3;

   /**
    * The name of this entry.
    */
   public final static byte VALUE_NAME = 4;

   /**
    * The size of the file in this entry.
    */
   public final static byte VALUE_SIZE = 5;

   /**
    * The last modified time of this entry.
    */
   public final static byte VALUE_TIME = 6;


   // attributes

   /**
    * The level number of the associated MessagePart within its message, level 0
    * being the root and increasing from there.
    */
   protected int m_nLevel;

   /**
    * The default comment to use when creating a Zip file (in the formatter).
    */
   protected String m_sComment;

   /**
    * The default compression level to use when creating a Zip file (in the formatter).
    * Range: 0 to 9, inclusive.
    */
   protected byte m_nCompression = 7;

   /**
    * The Zip entry field mapped to the associated message part.
    */
   protected byte m_nValue;


   // operations

   /**
    * Sets the Zip entry's field that is mapped to the associated message part.
    * 
    * @param nValue One of the VALUE_* constants.
    */
   public void setValue(byte nValue)
   {
      verifyNotReadOnly();
      m_nValue = nValue;
   }

   /**
    * Gets the Zip entry's field that is mapped to the associated message part.
    * 
    * @return One of the VALUE_* constants.
    */
   public byte getValue()
   {
      return m_nValue;
   }

   /**
    * Sets the default compression level to use when creating a Zip file.
    * 
    * @param nCompression The default compression level; 0 is worst, 9 is best.
    */
   public void setCompression(byte nCompression)
   {
      verifyNotReadOnly();
      m_nCompression = nCompression;
   }

   /**
    * Gets the default compression level to use when creating a Zip file.
    * 
    * @return The default compression level; 0 is worst, 9 is best.
    */
   public byte getCompression()
   {
      return m_nCompression;
   }

   /**
    * Sets the default comment to use for the Zip file.
    * 
    * @param sComment The default comment for the Zip file; null to leave
    *                 comment unspecified.
    */
   public void setComment(String sComment)
   {
      verifyNotReadOnly();
      m_sComment = sComment;
   }

   /**
    * Gets the default comment to use for the Zip file.
    * 
    * @return The default comment for the Zip file; null if no comment.
    */
   public String getComment()
   {
      return m_sComment;
   }

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
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      if (m_nCompression < 0 || m_nCompression > 9)
      {
         throw new MetadataException("err.meta.integration.zip.compressionOutOfRange",
            new Object[]{Primitive.createInteger(m_nCompression), part.getFullPath()});
      }

      if (m_nValue < VALUE_COMMENT || m_nValue > VALUE_TIME)
      {
         throw new IllegalStateException("Unknown value: " + m_nValue);
      }

      if (m_nLevel == 0)
      {
         // Check for a single child part to represent the Zip entries collection
         if (!(part instanceof CompositeMessagePart) ||
             ((CompositeMessagePart)part).getPartCount() != 1 ||
             !((CompositeMessagePart)((CompositeMessagePart)part).getPart(0)).isCollection())
         {
            throw new MetadataException("err.meta.integration.zip.missingCollectionUnderRoot",
               new Object[]{part.getFullPath()});
         }
      }
      else if (m_nLevel == 1)
      {
         // Require a child part with name set
         boolean bFoundNamePart = false;

         if (part instanceof CompositeMessagePart)
         {
            CompositeMessagePart composite = (CompositeMessagePart)part;

            for (int i = 0; i < composite.getPartCount(); i++)
            {
               MessagePart childPart = composite.getPart(i);

               if (((ZipMessagePartMapping)childPart.getMapping()).m_nValue == VALUE_NAME)
               {
                  bFoundNamePart = true;

                  break;
               }
            }
         }

         if (!bFoundNamePart)
         {
            throw new MetadataException("err.meta.integration.zip.missingNameChild",
               new Object[]{part.getFullPath()});
         }
      }
      else if (m_nLevel == 2)
      {
         // Check that the parts here are simple, single-valued
         if (!(part instanceof PrimitiveMessagePart) ||
             ((PrimitiveMessagePart)part).isCollection())
         {
            throw new MetadataException("err.meta.integration.zip.nonPrimitive",
               new Object[]{part.getFullPath()});
         }
      }
      else
      {
         throw new IllegalStateException("Message part depth: " + m_nLevel);
      }

      if (part instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePart composite = (CompositeMessagePartInstance)part;

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
