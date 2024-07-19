// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Arrays;
import java.util.Iterator;

import nexj.core.util.Holder;

/**
 * Set of primitive privileges.
 */
public final class PrivilegeSet implements java.io.Serializable
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 16681655969042101L;

   // attributes

   /**
    * The bit mask of privileges.
    */
   protected byte[] m_mask;

   // constructors

   /**
    * Constructs the privilege set.
    * @param nCount The maximum set size.
    */
   public PrivilegeSet(int nCount)
   {
      m_mask = new byte[(nCount + 7) >> 3];
   }

   /**
    * Constructs the privilege set.
    * @param nCount The maximum set size.
    * @param bGrantAll True to grant all the privileges.
    */
   public PrivilegeSet(int nCount, boolean bGrantAll)
   {
      this(nCount);

      if (bGrantAll)
      {
         Arrays.fill(m_mask, (byte)~0);
      }
   }

   /**
    * Constructs the privilege set.
    * This method is for INTERNAL USE ONLY.
    * @param mask The bit mask.
    */
   public PrivilegeSet(byte mask[])
   {
      m_mask = mask;
   }

   // operations
   
   /**
    * Checks if a privilege is contained in the set.
    * @param privilege The privilege which membership to check.
    * @return True if the privilege is contained in the set.
    */
   public final boolean contains(PrimitivePrivilege privilege)
   {
      return contains(privilege.m_nOrdinal);
   }

   /**
    * Checks if a privilege is contained in the set.
    * @param nOrdinal The ordinal number of the privilege which membership to check.
    * @return True if the privilege is contained in the set.
    */
   public final boolean contains(int nOrdinal)
   {
      return (m_mask[nOrdinal >> 3] & (1 << (nOrdinal & 7))) != 0;
   }
   
   /**
    * Checks if all the privileges in a collection, identified
    * by their ordinal numbers, are contained in the set.
    * @param ordinals Collection of privilege ordinal numbers.
    * @return True if all the privileges are contained in the set.
    */
   public final boolean contains(Holder ordinals)
   {
      if (ordinals != null)
      {
         for (Iterator itr = ordinals.iterator(); itr.hasNext();)
         {
            if (!contains(((Number)itr.next()).intValue()))
            {
               return false;
            }
         }
      }

      return true;
   }

   /**
    * Adds a privilege to the set.
    * @param privilege The privilege to add.
    */
   public final void add(PrimitivePrivilege privilege)
   {
      int nOrdinal = privilege.m_nOrdinal;
      
      m_mask[nOrdinal >> 3] |= 1 << (nOrdinal & 7);
   }
   
   /**
    * Adds all the members of a privilege set to this set.
    * @param set The source privilege set.
    */
   public final void addAll(PrivilegeSet set)
   {
      for (int i = 0, nCount = m_mask.length; i != nCount; ++i)
      {
         m_mask[i] |= set.m_mask[i];
      }
   }

   /**
    * Removes a privilege from the set.
    * @param privilege The privilege to remove.
    */
   public final void remove(PrimitivePrivilege privilege)
   {
      int nOrdinal = privilege.m_nOrdinal;
      
      m_mask[nOrdinal >> 3] &= ~(1 << (nOrdinal & 7));
   }
   
   /**
    * Removes all the members of a privilege set from this set.
    * @param set The source privilege set.
    */
   public final void removeAll(PrivilegeSet set)
   {
      for (int i = 0, nCount = m_mask.length; i != nCount; ++i)
      {
         m_mask[i] &= ~set.m_mask[i];
      }
   }

   /**
    * This method is for INTERNAL USE ONLY.
    * @return The bit mask.
    */
   public final byte[] getMask()
   {
      return m_mask;
   }
}
