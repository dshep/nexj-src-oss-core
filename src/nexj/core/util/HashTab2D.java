// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Fast 2D hash table implementation.
 * The focus is on efficiency. No error checking is provided.
 */
public class HashTab2D extends GenericHashTab2D
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -2724822446427659564L;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value triples.
    * @param nCount The estimated key-value triple count.
    */
   public HashTab2D(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table with an estimated key-value triple count of 8.
    */
   public HashTab2D()
   {
      super();
   }

   // operations

   /**
    * Computes the 2D hash index of a key pair.
    * @param key1 The first key.
    * @param key2 The second key.
    * @param nSize The table size.
    * @return The computed 2D hash code.
    */
   protected static int hashIndex(Object key1, Object key2, int nSize)
   {
      return (((key1.hashCode() << 6 ^ key2.hashCode()) & (Integer.MAX_VALUE >> 2)) * 3) % nSize;
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#rehash(int)
    */
   protected void rehash(int nSize2)
   {
      int nSize = m_table.length;

      if (nSize2 != nSize || m_nEmpty != 0)
      {
         Object[] table2 = new Object[nSize2];

         for (int k = 0; k < nSize; k += 3)
         {
            Object key1 = m_table[k];

            if (key1 != null && key1 != EMPTY)
            {
               int i = hashIndex(key1, m_table[k + 1], nSize2);

               for (;;)
               {
                  if (table2[i] == null)
                  {
                     table2[i] = key1;
                     table2[i + 1] = m_table[k + 1];
                     table2[i + 2] = m_table[k + 2];

                     break;
                  }

                  if ((i += 3) == nSize2)
                  {
                     i = 0;
                  }
               }
            }
         }

         m_table = table2;
         m_nEmpty = 0;
      }
   }

   /**
    * @see nexj.core.util.Lookup2D#put(java.lang.Object, java.lang.Object, java.lang.Object)
    */
   public Object put(Object key1, Object key2, Object value)
   {
      int nSize = m_table.length;
      int i = hashIndex(key1, key2, nSize);
      int k = -1;

      for (;;)
      {
         Object key = m_table[i];

         if (key == null)
         {
            if (k >= 0)
            {
               i = k;
               --m_nEmpty;
            }

            m_table[i] = key1;
            m_table[i + 1] = key2;
            m_table[i + 2] = value;

            // Max load factor 3/4
            if ((++m_nCount + m_nEmpty) << 2 > nSize)
            {
               rehash();
            }

            return null;
         }

         if (key == EMPTY)
         {
            if (k < 0)
            {
               k = i;
            }
         }
         else if (key.equals(key1) && m_table[i + 1].equals(key2))
         {
            Object oldValue = m_table[i += 2];

            m_table[i] = value;

            return oldValue;
         }

         if ((i += 3) == nSize)
         {
            i = 0;
         }
      }
   }

   /**
    * @see nexj.core.util.Lookup2D#get(java.lang.Object, java.lang.Object)
    */
   public Object get(Object key1, Object key2)
   {
      int nSize = m_table.length;
      int i = hashIndex(key1, key2, nSize);

      for (;;)
      {
         Object key = m_table[i];

         if (key == null)
         {
            return null;
         }

         if (key.equals(key1) && m_table[i + 1].equals(key2))
         {
            return m_table[i + 2];
         }

         if ((i += 3) == nSize)
         {
            i = 0;
         }
      }
   }

   /**
    * @see nexj.core.util.Lookup2D#contains(java.lang.Object, java.lang.Object)
    */
   public boolean contains(Object key1, Object key2)
   {
      int nSize = m_table.length;
      int i = hashIndex(key1, key2, nSize);

      for (;;)
      {
         Object key = m_table[i];

         if (key == null)
         {
            return false;
         }

         if (key.equals(key1) && m_table[i + 1].equals(key2))
         {
            return true;
         }

         if ((i += 3) == nSize)
         {
            i = 0;
         }
      }
   }

   /**
    * @see nexj.core.util.Lookup2D#remove(java.lang.Object, java.lang.Object)
    */
   public Object remove(Object key1, Object key2)
   {
      int nSize = m_table.length;
      int i = hashIndex(key1, key2, nSize);

      for (;;)
      {
         Object key = m_table[i];

         if (key == null)
         {
            return null;
         }

         if (key.equals(key1) && m_table[i + 1].equals(key2))
         {
            Object oldValue = m_table[i + 2];

            m_table[i + 1] = m_table[i + 2] = null;
            --m_nCount;
            ++m_nEmpty;

            if (m_table[(i + 3) % nSize] == null)
            {
               do
               {
                  m_table[i] = null;
                  i = (i - 3 + nSize) % nSize;
                  --m_nEmpty;
               }
               while (m_table[i] == EMPTY);
            }
            else
            {
               m_table[i] = EMPTY;
            }

            // Load factor [1/4..3/4] 
            if (m_nCount * 12 < nSize || (m_nCount + m_nEmpty) << 2 > nSize)
            {
               rehash();
            }

            return oldValue;
         }

         if ((i += 3) == nSize)
         {
            i = 0;
         }
      }
   }
}
