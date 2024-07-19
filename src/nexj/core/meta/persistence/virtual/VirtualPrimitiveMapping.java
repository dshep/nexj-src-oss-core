// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;

/**
 * Mapping for a primitive attribute in the virtual persistence adapter.
 */
public class VirtualPrimitiveMapping extends AttributeMapping
{
   // attributes

   /**
    * The maximum attribute data length.
    */
   int m_nMaxLength;

   /**
    * The index of this attribute in the OID, if this attribute is mapped to
    * an OID part.
    */
   int m_nObjectKeyPartOrdinal = -1;

   // operations

   /**
    * Sets the maximum attribute data length.
    * @param nMaxLength The maximum attribute data length; 0 for unlimited.
    */
   public void setMaxLength(int nMaxLength)
   {
      verifyNotReadOnly();
      m_nMaxLength = nMaxLength;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#getMaxLength(nexj.core.meta.Primitive)
    */
   public int getMaxLength(Primitive type)
   {
      return m_nMaxLength;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isAliasOf(nexj.core.meta.persistence.AttributeMapping)
    */
   public boolean isAliasOf(AttributeMapping mapping)
   {
      return m_attribute.getName().equals(mapping.getAttribute().getName());
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isMultiplexed()
    */
   public boolean isMultiplexed()
   {
      return false;
   }

   /**
    * Sets the index of this attribute in the OID.
    * @param nOrdinal The index in the OID; -1 if not mapped to the OID.
    */
   public void setObjectKeyPart(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nObjectKeyPartOrdinal = nOrdinal;
   }

   /**
    * Gets the index of this attribute in the OID.
    * @return The index in the OID; -1 if not mapped to the OID.
    */
   public int getObjectKeyPart()
   {
      return m_nObjectKeyPartOrdinal;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder("Virtual primitive mapping for ");

      buf.append(m_attribute.getMetaclass().getName());
      buf.append('.');
      buf.append(m_attribute.getName());

      if (m_nObjectKeyPartOrdinal >= 0)
      {
         buf.append(" (OID part=");
         buf.append(m_nObjectKeyPartOrdinal);
         buf.append(')');
      }

      return buf.toString();
   }
}
