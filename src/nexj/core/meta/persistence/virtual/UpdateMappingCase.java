// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.BitSet;

import nexj.core.meta.Metaclass;
import nexj.core.util.ObjUtil;

/**
 * An update mapping case. There can be multiple update mapping cases in an update
 * mapping, each handling a different set of attributes.
 */
public class UpdateMappingCase extends WorkMapping
{
   // attributes

   /**
    * Put dirty attributes into the transfer object; false to put all
    * attributes.
    */
   protected boolean m_bDirty = true;

   /**
    * Put all instance attributes into the transfer object; false to
    * put only the update mapping case attributes.
    */
   protected boolean m_bFull;

   // associations

   /**
    * The set of attributes handled by this case. Instance attribute ordinal number
    * is used as the bit index.
    */
   protected BitSet m_attributeSet;

   // constructor

   public UpdateMappingCase(Metaclass declarator)
   {
      super(declarator);
   }

   // operations

   /**
    * Sets the dirty attributes flag, which determines the attributes that
    * should be put into the transfer object.
    * @param bDirty True to put only dirty attributes; false to
    * put all attributes.
    */
   public void setDirty(boolean bDirty)
   {
      verifyNotReadOnly();
      m_bDirty = bDirty;
   }

   /**
    * Gets the dirty attributes flag, which determines the attributes that
    * should be put into the transfer object.
    * @return True to put only dirty attributes; false to put all attributes.
    */
   public boolean isDirty()
   {
      return m_bDirty;
   }

   /**
    * Sets the full attributes flag, which determines the attributes that
    * should be put into the transfer object.
    * @param bFull True to put all instance attributes; false to put only attributes
    * from this case.
    */
   public void setFull(boolean bFull)
   {
      verifyNotReadOnly();
      m_bFull = bFull;
   }

   /**
    * Gets the full attributes flag, which determines the attributes that
    * should be put into the transfer object.
    * @return True to put all instance attributes; false to put only attributes
    * from this case.
    */
   public boolean isFull()
   {
      return m_bFull;
   }

   /**
    * Sets the attributes handled by this case.
    * @param attributeSet Bitmap with index corresponding to the ordinal numbers of
    * the instance attributes. A bit is set if attribute is handled by this case.
    */
   public void setAttributeSet(BitSet attributeSet)
   {
      verifyNotReadOnly();
      m_attributeSet = attributeSet;
   }

   /**
    * Gets the attributes handled by this case.
    * @return Bitmap with index corresponding to the ordinal numbers of the
    * instance attributes. A bit is set if attribute is handled by this case.
    */
   public BitSet getAttributeSet()
   {
      return m_attributeSet;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder();

      if (!m_bBatch)
      {
         buf.append("Batchable ");
      }

      buf.append("UpdateMappingCase(");

      if (m_attributeSet != null && !m_attributeSet.isEmpty())
      {
         buf.append(m_attributeSet.toString());
      }

      buf.append(')');

      return buf.toString();
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof UpdateMappingCase)
      {
         return ObjUtil.equal(m_attributeSet, ((UpdateMappingCase)obj).m_attributeSet);
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return (m_attributeSet == null) ? 0 : m_attributeSet.hashCode();
   }
}