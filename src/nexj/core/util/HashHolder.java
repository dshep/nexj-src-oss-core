// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Fast hash set implementation.
 * The focus is on efficiency. No error checking is provided.
 */
public class HashHolder extends GenericHashHolder
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -599270550004305272L;

   // constructors

   /**
    * Creates a hash holder with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public HashHolder(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    */
   public HashHolder()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashHolder#rehash(int)
    */
   protected void rehash(int nSize2)
   {
      int nSize = m_table.length;

      if (nSize2 != nSize || m_nEmpty != 0)
      {
         int nMask2 = nSize2 - 1;
         Object[] table2 = new Object[nSize2];

         for (int k = 0; k < nSize; ++k)
         {
            Object value = m_table[k];

            if (value != null && value != EMPTY)
            {
               int i = value.hashCode() & nMask2;

               for (;;)
               {
                  if (table2[i] == null)
                  {
                     table2[i] = value;

                     break;
                  }

                  i = (i + 1) & nMask2;
               }
            }
         }

         m_table = table2;
         m_nEmpty = 0;
      }
   }

   /**
    * @see nexj.core.util.Holder#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      int nMask = m_table.length - 1;
      int i = value.hashCode() & nMask;
      int k = -1;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            if (k >= 0)
            {
               i = k;
               --m_nEmpty;
            }

            m_table[i] = value;

            // Max load factor 3/4
            if (((nMask - ++m_nCount - m_nEmpty) << 2) + 3 < nMask)
            {
               rehash();
            }

            return true;
         }

         if (value2 == EMPTY)
         {
            if (k < 0)
            {
               k = i;
            }
         }
         else if (value2.equals(value))
         {
            m_table[i] = value;

            return false;
         }

         i = (i + 1) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Holder#contains(java.lang.Object)
    */
   public boolean contains(Object value)
   {
      if (value == null)
      {
         return false;
      }

      int nMask = m_table.length - 1;
      int i = value.hashCode() & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return false;
         }

         if (value2.equals(value))
         {
            return true;
         }

         i = (i + 1) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Holder#get(java.lang.Object)
    */
   public Object get(Object value)
   {
      int nMask = m_table.length - 1;
      int i = value.hashCode() & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return null;
         }

         if (value2.equals(value))
         {
            return value2;
         }

         i = (i + 1) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Holder#remove(java.lang.Object)
    */
   public boolean remove(Object value)
   {
      int nMask = m_table.length - 1;
      int i = value.hashCode() & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return false;
         }

         if (value2.equals(value))
         {
            --m_nCount;
            ++m_nEmpty;

            if (m_table[(i + 1) & nMask] == null)
            {
               do
               {
                  m_table[i] = null;
                  i = (i - 1) & nMask;
                  --m_nEmpty;
               }
               while (m_table[i] == EMPTY);
            }
            else
            {
               m_table[i] = EMPTY;
            }

            // Load factor [1/4..3/4]
            if ((m_nCount << 2) - 1 < nMask ||
               ((nMask - m_nCount - m_nEmpty) << 2) + 3  < nMask)
            {
               rehash();
            }

            return true;
         }

         i = (i + 1) & nMask;
      }
   }
}
