// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.hl7;

import java.util.Comparator;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.util.ExceptionHolder;

/**
 * HL7 message part mapping.
 */
public class HL7MessagePartMapping extends MetadataObject implements MessagePartMapping
{
   // constants

   /**
    * Top level.
    */
   public final static int LEVEL_MESSAGE = 0;

   /**
    * Segment group level.
    */
   public final static int LEVEL_GROUP = 1;

   /**
    * Segment level.
    */
   public final static int LEVEL_SEGMENT = 2;

   /**
    * Field level.
    */
   public final static int LEVEL_FIELD = 3;

   /**
    * Component level.
    */
   public final static int LEVEL_COMPONENT = 4;

   /**
    * Subcomponent level.
    */
   public final static int LEVEL_SUBCOMPONENT = 5;

   /**
    * No specific subtype.
    */
   public final static int SUBTYPE_DEFAULT = 0;

   /**
    * Date/Time subtype.
    */
   public final static int SUBTYPE_DTM = 1;

   /**
    * Date subtype.
    */
   public final static int SUBTYPE_DT = 2;

   /**
    * Time subtype.
    */
   public final static int SUBTYPE_TM = 3;

   // attributes

   /**
    * The segment or message type name.
    */
   protected String m_sName;

   /**
    * The segment or message type version.
    */
   protected String m_sVersion;

   /**
    * The sequential number, 1-based.
    */
   protected int m_nSeq;

   /**
    * The element level, one of the LEVEL_* constants.
    */
   protected int m_nLevel = -1;

   /**
    * The subtype.
    */
   protected int m_nSubtype;

   // operations

   /**
    * Sets the segment or message type name.
    * @param sName The segment or message type name to set.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = sName;
   }

   /**
    * @return The segment or message type name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Set the segment or message type version.
    * @param sVersion Segment or message type version.
    */
   public void setVersion(String sVersion)
   {
      m_sVersion = sVersion;
   }

   /**
    * @return Segment or message type version.
    */
   public String getVersion()
   {
      return m_sVersion;
   }

   /**
    * Sets the sequential number, 1-based.
    * @param nSeq The sequential number, 1-based. to set.
    */
   public void setSeq(int nSeq)
   {
      verifyNotReadOnly();
      m_nSeq = nSeq;
   }

   /**
    * @return The sequential number, 1-based.
    */
   public int getSeq()
   {
      return m_nSeq;
   }

   /**
    * Sets the element level, one of the LEVEL_* constants.
    * @param nLevel The element level, one of the LEVEL_* constants to set.
    */
   public void setLevel(int nLevel)
   {
      verifyNotReadOnly();
      m_nLevel = nLevel;
   }

   /**
    * @return The element level, one of the LEVEL_* constants.
    */
   public int getLevel()
   {
      return m_nLevel;
   }

   /**
    * Sets the subtype.
    * @param nSubtype The subtype to set.
    */
   public void setSubtype(int nSubtype)
   {
      verifyNotReadOnly();
      m_nSubtype = nSubtype;
   }

   /**
    * @return The subtype.
    */
   public int getSubtype()
   {
      return m_nSubtype;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "HL7MessagePartMapping(" + ((m_sName == null) ? "" : "name=" + m_sName + ", ") +
         "level=" + m_nLevel + ", seq=" + m_nSeq + ")";
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      int nParentLevel = (part.getParent() == null) ? -1 :
         ((HL7MessagePartMapping)part.getParent().getMapping()).m_nLevel;

      if (nParentLevel < 0)
      {
         if (m_nSeq == 0)
         {
            if (m_nLevel < 0)
            {
               m_nLevel = LEVEL_MESSAGE;
            }
         }
         else
         {
            m_nLevel = (m_sName == null) ? LEVEL_FIELD : LEVEL_SEGMENT;
         }
      }

      if (m_nLevel == LEVEL_GROUP && nParentLevel > m_nLevel)
      {
         throw new MetadataException("err.meta.integration.hl7.missingMapping");
      }

      if (m_nLevel < 0)
      {
         if (nParentLevel < LEVEL_SEGMENT)
         {
            m_nLevel = (m_sName == null) ? LEVEL_FIELD : LEVEL_SEGMENT;
         }
         else
         {
            m_nLevel = nParentLevel + 1;
         }
      }

      if (m_nLevel > LEVEL_SUBCOMPONENT)
      {
         throw new MetadataException("err.meta.integration.hl7.levels");
      }

      if (m_sName != null)
      {
         if (m_nLevel == LEVEL_MESSAGE && m_nSeq != 0)
         {
            throw new MetadataException("err.meta.integration.hl7.naSeq");
         }

         if (nParentLevel >= LEVEL_SEGMENT)
         {
            throw new MetadataException("err.meta.integration.hl7.misplacedSeg", new Object[]{m_sName});
         }

         if (m_nLevel != LEVEL_MESSAGE)
         {
            m_nLevel = LEVEL_SEGMENT;
         }
      }
      else
      {
         if (m_nLevel > LEVEL_GROUP && m_nSeq <= 0)
         {
            throw new MetadataException("err.meta.integration.hl7.missingSeq");
         }
      }

      if (m_nLevel > LEVEL_FIELD && part.getMaxCount() > 1)
      {
         throw new MetadataException("err.meta.integration.hl7.maxCount", new Object[]{m_sName});
      }

      if (m_nSubtype != SUBTYPE_DEFAULT)
      {
         if (!(part instanceof PrimitiveMessagePart) ||
            ((PrimitiveMessagePart)part).getType() != Primitive.TIMESTAMP)
         {
            throw new MetadataException("err.meta.integration.hl7.inapplicableSubtype",
               new Object[]{new String[]{"DTM", "DT", "TM"}[m_nSubtype - 1], part.getFullPath()});
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
      CompositeMessagePart parent = ref.getParent();
      MessagePartMapping mapping = (parent == null) ? null : parent.getMapping();

      if (mapping != null && 
         ((HL7MessagePartMapping)mapping).getLevel() >= ((m_nLevel == LEVEL_GROUP) ? LEVEL_SEGMENT : m_nLevel))
      {
         throw new MetadataException("err.meta.integration.hl7.refLevel", new Object[]{ref.getFullPath()});
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      if (part instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePartInstance composite = (CompositeMessagePartInstance)part;

         // Sorts the message parts
         if (m_nLevel >= LEVEL_SEGMENT)
         {
            composite.sort(new Comparator()
            {
               public int compare(Object o1, Object o2)
               {
                  int n = ((HL7MessagePartMapping)((MessagePart)o1).getMapping()).getSeq() -
                     ((HL7MessagePartMapping)((MessagePart)o2).getMapping()).getSeq();

                  if (n == 0)
                  {
                     throw new MetadataException("err.meta.integration.hl7.dupSeq",
                        new Object[]{((MessagePart)o2).getFullPath()});
                  }

                  return n;
               }
            });
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

   /**
    * Finds the MSH header message part.
    * @param message The message to analyze.
    * @return The message part, or null if not found. 
    */
   public static CompositeMessagePart findMSH(Message message)
   {
      CompositeMessagePart root = message.getRoot();

      if (root.getPartCount() == 0)
      {
         return null;
      }

      MessagePart hdr = root.getPart(0);
      HL7MessagePartMapping mapping = (HL7MessagePartMapping)hdr.getMapping();

      return (mapping.getLevel() == HL7MessagePartMapping.LEVEL_SEGMENT &&
         mapping.getName().equals("MSH")) ? (CompositeMessagePart)hdr : null;
   }
}
