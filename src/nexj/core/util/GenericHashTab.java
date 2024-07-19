// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * Generic hash table implementation.
 * The focus is on efficiency. No error checking is provided. 
 */
public abstract class GenericHashTab implements Lookup, Cloneable, Serializable, Printable
{
   // constants
   
   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 2045784218593908701L;

   public final static Iterator EMPTY_ITERATOR = new Iterator()
   {
      public Object getKey()
      {
         throw new IllegalStateException();
      }

      public Object getValue()
      {
         throw new IllegalStateException();
      }

      public void setValue(Object value)
      {
         throw new IllegalStateException();
      }

      public void remove()
      {
         throw new IllegalStateException();
      }

      public boolean hasNext()
      {
         return false;
      }

      public Object next()
      {
         throw new java.util.NoSuchElementException();
      }
   };
   
   /**
    * Key value indicating deallocated cells.
    */
   protected final static Object EMPTY = new Object();

   // attributes

   /**
    * The key and value table.
    * The keys are at even indexes, the values are at odd indexes.
    */
   protected transient Object[] m_table;
   
   /**
    * The key-value pair count.
    */
   protected transient int m_nCount;

   /**
    * The empty item count.
    */
   protected transient int m_nEmpty;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   protected GenericHashTab(int nCount)
   {
      m_table = new Object[getTableSize(nCount)];
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   protected GenericHashTab()
   {
      m_table = new Object[32];
   }

   // operations

   /**
    * Calculates the table size as a power of two.
    * @param nCount The count of key-value pairs to store.
    * @return The calculated size.
    */
   protected static int getTableSize(int nCount)
   {
      if (nCount < 1)
      {
         nCount = 1;
      }

      // Initial load factor 2/3
      return MathUtil.ceil2(nCount + (nCount << 1));
   }

   /**
    * Rehashes the table.
    */
   protected final void rehash()
   {
      rehash(getTableSize(m_nCount));
   }

   /**
    * Rehashes the table.
    * @param nSize2 The new hash table size.
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
               int i = (hash(key) << 1) & nMask2;

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
      int i = (hash(key) << 1) & nMask;
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
         else if (equal(key2, key))
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
      int i = (hash(key) << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return null;
         }

         if (equal(key2, key))
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
      int i = (hash(key) << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return false;
         }

         if (equal(key2, key))
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
      int i = (hash(key) << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return null;
         }

         if (equal(key2, key))
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

   /**
    * Invokes the equivalence function.
    * @param left The first object to compare.
    * @param right The second object to compare.
    * @return True if the given arguments are equal.
    */
   protected boolean equal(Object left, Object right)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * Invokes the hash function.
    * @param key The object for which a hash code is generated.
    * @return The hash value of key.
    */
   protected int hash(Object key)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.util.Lookup#clear()
    */
   public void clear()
   {
      java.util.Arrays.fill(m_table, null);
      m_nCount = m_nEmpty = 0;
   }

   /**
    * Clears the table and resets its capacity to the given number.
    * @param nCapacity The estimated key-value pairs count.
    */
   public void clear(int nCapacity)
   {
      nCapacity = getTableSize(nCapacity);

      if (m_table.length != nCapacity)
      {
         m_table = new Object[nCapacity];
         m_nCount = m_nEmpty = 0;
      }
      else
      {
         clear();
      }
   }

   /**
    * @see nexj.core.util.Lookup#size()
    */
   public int size()
   {
      return m_nCount;
   }

   /**
    * @see nexj.core.util.Lookup#iterator()
    */
   public Iterator iterator()
   {
      return new GenericHashTabIterator();
   }

   /**
    * @see nexj.core.util.Lookup#valueIterator()
    */
   public Iterator valueIterator()
   {
      return new GenericHashTabValueIterator();
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         GenericHashTab ht = (GenericHashTab)super.clone();

         ht.m_table = new Object[m_table.length];
         System.arraycopy(m_table, 0, ht.m_table, 0, m_table.length);
         
         return ht;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * Serializes the object.
    */
   private void writeObject(ObjectOutputStream out) throws IOException
   {
      serialize(out);
   }

   /**
    * Template method for serializing the object.
    * @param The serialization stream. 
    */
   protected void serialize(ObjectOutputStream out) throws IOException
   {
      int nCount = m_nCount;

      out.defaultWriteObject();
      out.writeInt(nCount);

      for (int i = 0; nCount != 0; i += 2)
      {
         Object key = m_table[i];
         
         if (key != null && key != EMPTY)
         {
            out.writeObject(key);
            out.writeObject(m_table[i + 1]);
            --nCount;
         }
      }
   }

   /**
    * Deserializes the object.
    * @param in The serialization stream.
    */
   private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      deserialize(in);
   }

   /**
    * Template method for deserializing the object.
    */
   protected void deserialize(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      in.defaultReadObject();
      
      int nCount = in.readInt();

      if (nCount < 0)
      {
         throw new InvalidObjectException("Negative HashTab value count");
      }
      
      m_table = new Object[getTableSize(nCount)];
      
      while (nCount-- != 0)
      {
         Object key = in.readObject();
         put(key, in.readObject());
      }
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('{');

      boolean bFirst = true;

      for (int i = 0, nLen = m_table.length; i < nLen; i += 2)
      {
         Object key = m_table[i];

         if (key != null && key != EMPTY)
         {
            if (bFirst)
            {
               bFirst = false;
            }
            else
            {
               writer.write(", ");
            }

            writer.print(key);
            writer.write('=');
            writer.print(m_table[i + 1]);
         }
      }

      writer.write('}');
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   /**
    * GenericHashTab iterator - can return the current key and the value.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected class GenericHashTabIterator implements Iterator
   {
      /**
       * The next key index.
       */
      protected int m_nNext = -2;
      
      /**
       * The current key index.
       */
      protected int m_nCur;
      
      /**
       * The table for iteration.
       */
      protected Object[] m_table = GenericHashTab.this.m_table;

      /**
       * Creates an iterator.
       */
      protected GenericHashTabIterator()
      {
         incr();
      }
      
      /**
       * Creates an iterator.
       * @param bInit True to initialize the iterator.
       */
      protected GenericHashTabIterator(boolean bInit)
      {
         if (bInit)
         {
            incr();
         }
      }

      /**
       * Advances to the next item.
       * @return True if there is next item.
       */
      protected boolean incr()
      {
         m_nCur = m_nNext;

         int nCount = m_table.length;

         while ((m_nNext += 2) < nCount)
         {
            Object key = m_table[m_nNext];

            if (key != null && key != EMPTY)
            {
               return true;
            }
         }

         return false;
      }
      
      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_nNext < m_table.length;
      }

      /**
       * @return The next available element key.
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext < m_table.length)
         {
            incr();

            return m_table[m_nCur];
         }

         throw new java.util.NoSuchElementException("m_nNext=" + m_nNext + ", m_table.length=" + m_table.length);
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         if (m_table[m_nCur] == null || m_table[m_nCur] == EMPTY)
         {
            return;
         }

         if (m_table == GenericHashTab.this.m_table)
         {
            int nMask = m_table.length - 1;

            --m_nCount;
            ++m_nEmpty;
            m_table[m_nCur + 1] = null;

            if (m_table[(m_nCur + 2) & nMask] == null)
            {
               int i = m_nCur;
               
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
               m_table[m_nCur] = EMPTY;

               int nMaxCount = (nMask + 1) >> 1;

               // Rehash when the load factor exceeds 3/4
               if ((nMaxCount - m_nCount - m_nEmpty) << 2 < nMaxCount)
               {
                  rehash();
               }
            }
         }
         else
         {
            GenericHashTab.this.remove(m_table[m_nCur]);
            m_table[m_nCur] = m_table[m_nCur + 1] = null;
         }
      }
      
      /**
       * @return The key retrieved by the last next() invocation.
       */
      public Object getKey()
      {
         return m_table[m_nCur];
      }

      /**
       * @return The value associated with the last next() invocation.
       */
      public Object getValue()
      {
         return m_table[m_nCur + 1];
      }

      /**
       * Replaces the value associated with the last next() invocation.
       * @param value The value to set.
       */
      public void setValue(Object value)
      {
         m_table[m_nCur + 1] = value;

         if (m_table != GenericHashTab.this.m_table)
         {
            put(m_table[m_nCur], value);
         }
      }
   }

   /**
    * The same as the GenericHashTabIterator, but next() returns the value.
    */
   protected class GenericHashTabValueIterator extends GenericHashTabIterator
   {
      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext < m_table.length)
         {
            incr();

            return m_table[m_nCur + 1];
         }

         throw new java.util.NoSuchElementException("m_nNext=" + m_nNext + ", m_table.length=" + m_table.length);
      }
   }
}
