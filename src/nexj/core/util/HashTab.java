// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Fast hash table implementation.
 * The focus is on efficiency. No error checking is provided.
 */
public class HashTab extends GenericHashTab
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 775591525988624503L;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   public HashTab(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public HashTab()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashTab#rehash(int)
    */
   protected void rehash(int nSize2)
   {
      int nSize = m_table.length;

      if (nSize2 != nSize || m_nEmpty != 0)
      {
         int nMask2 = nSize2 - 1;
         Object[] table2 = new Object[nSize2];

         for (int k = 0; k < nSize; k += 2)
         {
            Object key = m_table[k];

            if (key != null && key != EMPTY)
            {
               int i = (key.hashCode() << 1) & nMask2;

               for (;;)
               {
                  if (table2[i] == null)
                  {
                     table2[i] = key;
                     table2[i + 1] = m_table[k + 1];

                     break;
                  }

                  i = (i + 2) & nMask2;
               }
            }
         }

         m_table = table2;
         m_nEmpty = 0;
      }
   }

   /**
    * @see nexj.core.util.Lookup#put(java.lang.Object, java.lang.Object)
    */
   public Object put(Object key, Object value)
   {
      int nMask = m_table.length - 1;
      int i = (key.hashCode() << 1) & nMask;
      int k = -1;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            if (k >= 0)
            {
               i = k;
               --m_nEmpty;
            }

            m_table[i] = key;
            m_table[i + 1] = value;

            int nMaxCount = (nMask + 1) >> 1;

            // Max load factor 3/4
            if ((nMaxCount - ++m_nCount - m_nEmpty) << 2 < nMaxCount)
            {
               rehash();
            }

            return null;
         }

         if (key2 == EMPTY)
         {
            if (k < 0)
            {
               k = i;
            }
         }
         else if (key2.equals(key))
         {
            Object oldValue = m_table[++i];

            m_table[i] = value;

            return oldValue;
         }

         i = (i + 2) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Lookup#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      int nMask = m_table.length - 1;
      int i = (key.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return null;
         }

         if (key2.equals(key))
         {
            return m_table[i + 1];
         }

         i = (i + 2) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Lookup#contains(java.lang.Object)
    */
   public boolean contains(Object key)
   {
      int nMask = m_table.length - 1;
      int i = (key.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return false;
         }

         if (key2.equals(key))
         {
            return true;
         }

         i = (i + 2) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Lookup#remove(java.lang.Object)
    */
   public Object remove(Object key)
   {
      int nMask = m_table.length - 1;
      int i = (key.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return null;
         }

         if (key2.equals(key))
         {
            Object oldValue = m_table[i + 1];

            m_table[i + 1] = null;
            --m_nCount;
            ++m_nEmpty;

            if (m_table[(i + 2) & nMask] == null)
            {
               do
               {
                  m_table[i] = null;
                  i = (i - 2) & nMask;
                  --m_nEmpty;
               }
               while (m_table[i] == EMPTY);
            }
            else
            {
               m_table[i] = EMPTY;
            }

            int nMaxCount = (nMask + 1) >> 1;

            // Load factor [1/4..3/4]
            if ((m_nCount << 2) < nMaxCount ||
               (nMaxCount - m_nCount - m_nEmpty) << 2 < nMaxCount)
            {
               rehash();
            }

            return oldValue;
         }

         i = (i + 2) & nMask;
      }
   }
}
