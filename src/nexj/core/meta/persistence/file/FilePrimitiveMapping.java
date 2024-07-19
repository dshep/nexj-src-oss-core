// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.file;


import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.util.ExceptionHolder;

/**
 * A metadata object describing the mapping of a single attribute from a metaclass
 * to the file persistence layer.
 * 
 * Since there are only a limited number of data points inside the file persistence
 * layer to which an attribute might be mapped, these data points are enumerated
 * by the SYSID_* constants in this class.
 */
public class FilePrimitiveMapping extends AttributeMapping
{
   // constants

   /**
    * The name of the file, on disk.
    */
   public final static byte SYSID_ID = 0;

   /**
    * The contents of the file.
    */
   public final static byte SYSID_DATA = 1;

   /**
    * The value to use for optimistic locking of the file, currently the
    * file's last modified time.
    */
   public final static byte SYSID_LOCKING = 2;


   // attributes

   /**
    * The field in the file persistence layer to which the attribute is mapped.
    * Must be one of the SYSID_* constants.
    */
   protected byte m_nSysId;


   // operations

   /**
    * Sets the id of the file storage field to which the attribute is mapped.
    * 
    * @param nSysId One of the SYSID_* constants.
    */
   public void setSysId(byte nSysId)
   {
      verifyNotReadOnly();
      m_nSysId = nSysId;
   }


   /**
    * Gets the id of the file storage field to which the attribute is mapped.
    * 
    * @return One of the SYSID_* constants;
    */
   public byte getSysId()
   {
      return m_nSysId;
   }


   /**
    * @see nexj.core.meta.persistence.AttributeMapping#getMaxLength(nexj.core.meta.Primitive)
    */
   public int getMaxLength(Primitive type)
   {
      // Unlimited
      return 0;
   }


   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isAliasOf(nexj.core.meta.persistence.AttributeMapping)
    */
   public boolean isAliasOf(AttributeMapping mapping)
   {
      return (mapping instanceof FilePrimitiveMapping) &&
          ((FilePrimitiveMapping)mapping).m_nSysId == m_nSysId;
   }


   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isMultiplexed()
    */
   public boolean isMultiplexed()
   {
      return false;
   }


   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      Type attributeType = m_attribute.getType();
      
      if (m_nSysId == SYSID_ID)
      {
         if (attributeType != Primitive.STRING)
         {
            throw new MetadataException("err.meta.persistence.file.invalidNameType",
               new Object[]{m_attribute.getName(), m_persistenceMapping.getMetaclass().getName()});
         }
      }
      else if (m_nSysId == SYSID_DATA)
      {
         if (attributeType != Primitive.BINARY && attributeType != Primitive.STRING)
         {
            throw new MetadataException("err.meta.persistence.file.invalidDataType",
               new Object[]{m_attribute.getName(), m_persistenceMapping.getMetaclass().getName()});
         }
      }
   }


   /**
    * A helpful string representation for debugging.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);
      
      buf.append("FilePrimitiveMapping from ");
      buf.append(m_attribute);
      buf.append(" to file adapter field ");
      
      switch (m_nSysId)
      {
         case SYSID_ID:
            buf.append("SYSID_ID");
            break;
            
         case SYSID_DATA:
            buf.append("SYSID_DATA");
            break;
         
         case SYSID_LOCKING:
            buf.append("SYSID_LOCKING");
            break;
            
         default:
            buf.append("UNKNOWN");
      }
      
      return buf.toString();
   }
}
