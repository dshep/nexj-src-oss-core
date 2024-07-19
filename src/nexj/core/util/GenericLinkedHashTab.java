// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * Generic LookupDeque implementation with a hash table and a double-linked list.
 * The focus is on efficiency. No error checking is provided.
 */
public abstract class GenericLinkedHashTab implements LookupDeque, Cloneable, Serializable, Printable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -8161157734551544723L;

   /**
    * Empty iterator implementation.
    */
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
    * The link table: nNext[2*n], nPrev[2*n+1].
    */
   protected transient int[] m_links;

   /**
    * The first key-value index, or -1 if empty.
    */
   protected transient int m_nFirst;

   /**
    * The last key-value index, or -1 if empty.
    */
   protected transient int m_nLast;

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
   protected GenericLinkedHashTab(int nCount)
   {
      int nSize = getTableSize(nCount);

      m_table = new Object[nSize];
      m_links = new int[nSize];
      m_nFirst = m_nLast = -1;
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   protected GenericLinkedHashTab()
   {
      m_table = new Object[32];
      m_links = new int[32];
      m_nFirst = m_nLast = -1;
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
    * Computes the hash code of a key.
    * @param key The key.
    * @return The hash code.
    */
   protected abstract int hash(Object key);

   /**
    * Compares two keys for equality.
    * @param left The left key.
    * @param right The right key.
    * @return True if the keys are equal.
    */
   protected abstract boolean equal(Object left, Object right);

   /**
    * Rehashes the table.
    */
   protected final void rehash()
   {
      rehash(getTableSize(m_nCount));
   }

   /**
    * Rehashes the table.
    * @param nSize2 The new table size.
    */
   protected void rehash(int nSize2)
   {
      if (nSize2 != m_table.length || m_nEmpty != 0)
      {
         Object[] table2 = new Object[nSize2];
         int[] links2 = new int[nSize2];
         int nMask2 = nSize2 - 1;
         int nFirst = -1;
         int nLast = -1;

         for (int k = m_nFirst; k >= 0; k = m_links[k])
         {
            Object key = m_table[k];
            int i = (hash(key) << 1) & nMask2;

            for (;;)
            {
               if (table2[i] == null)
               {
                  table2[i] = key;
                  table2[i + 1] = m_table[k + 1];
                  links2[i] = -1;
                  links2[i + 1] = nLast;

                  if (nLast >= 0)
                  {
                     links2[nLast] = i;
                  }
                  else
                  {
                     nFirst = i;
                  }

                  nLast = i;

                  break;
               }

               i = (i + 2) & nMask2;
            }
         }

         m_table = table2;
         m_links = links2;
         m_nFirst = nFirst;
         m_nLast = nLast;
         m_nEmpty = 0;
      }
   }

   /**
    * Removes a key-value pair from the hash table (but not from the linked list).
    * @param i The index of the key-value pair.
    * @param nMask The table mask (size - 1).
    * @return The old value.
    */
   protected Object remove(int i, int nMask)
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

         int nMaxCount = (nMask + 1) >> 1;

         // Load factor [1/4..3/4]
         if ((m_nCount << 2) < nMaxCount ||
            (nMaxCount - m_nCount - m_nEmpty) << 2 < nMaxCount)
         {
            rehash();
         }
      }

      return oldValue;
   }

   /**
    * Removes a key-value paur from the linked list (but not from the hash table).
    * @param i The index of the key-value pair.
    */
   protected void unlink(int i)
   {
      int nNext = m_links[i];
      int nPrev = m_links[i + 1];

      if (nNext >= 0)
      {
         m_links[nNext + 1] = nPrev;
      }
      else
      {
         m_nLast = nPrev;
      }

      if (nPrev >= 0)
      {
         m_links[nPrev] = nNext;
      }
      else
      {
         m_nFirst = nNext;
      }
   }

   /**
    * Differs from putLast in that, if an entry for key already exists, it
    * is replaced but it is not moved to the end of the deque. Otherwise,
    * the operation is identical.
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
            m_links[i] = -1;
            m_links[i + 1] = m_nLast;

            if (m_nLast >= 0)
            {
               m_links[m_nLast] = i;
            }
            else
            {
               m_nFirst = i;
            }

            m_nLast = i;

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
    * @see nexj.core.util.LookupDeque#putFirst(java.lang.Object, java.lang.Object)
    */
   public Object putFirst(Object key, Object value)
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

            if (m_nFirst >= 0)
            {
               m_links[m_nFirst + 1] = i;
            }
            else
            {
               m_nLast = i;
            }

            m_links[i] = m_nFirst;
            m_links[i + 1] = -1;
            m_nFirst = i;

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
            Object oldValue = m_table[i + 1];

            m_table[i + 1] = value;
            unlink(i);

            if (m_nFirst >= 0)
            {
               m_links[m_nFirst + 1] = i;
            }
            else
            {
               m_nLast = i;
            }

            m_links[i] = m_nFirst;
            m_links[i + 1] = -1;
            m_nFirst = i;

            return oldValue;
         }

         i = (i + 2) & nMask;
      }
   }

   /**
    * @see nexj.core.util.LookupDeque#putLast(java.lang.Object, java.lang.Object)
    */
   public Object putLast(Object key, Object value)
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
            m_links[i] = -1;
            m_links[i + 1] = m_nLast;

            if (m_nLast >= 0)
            {
               m_links[m_nLast] = i;
            }
            else
            {
               m_nFirst = i;
            }

            m_nLast = i;

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
            Object oldValue = m_table[i + 1];

            m_table[i + 1] = value;
            unlink(i);
            m_links[i] = -1;
            m_links[i + 1] = m_nLast;

            if (m_nLast >= 0)
            {
               m_links[m_nLast] = i;
            }
            else
            {
               m_nFirst = i;
            }

            m_nLast = i;

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
      int i = (key.hashCode() << 1) & nMask;

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
            unlink(i);

            return remove(i, nMask);
         }

         i = (i + 2) & nMask;
      }
   }

   /**
    * @see nexj.core.util.LookupDeque#removeFirst()
    */
   public Object removeFirst()
   {
      int i = m_nFirst;

      if (i < 0)
      {
         return null;
      }

      unlink(i);

      return remove(i, m_table.length - 1);
   }

   /**
    * @see nexj.core.util.LookupDeque#removeLast()
    */
   public Object removeLast()
   {
      int i = m_nLast;

      if (i < 0)
      {
         return null;
      }

      unlink(i);

      return remove(i, m_table.length - 1);
   }

   /**
    * @see nexj.core.util.LookupDeque#firstKey()
    */
   public Object firstKey()
   {
      if (m_nFirst >= 0)
      {
         return m_table[m_nFirst];
      }

      return null;
   }

   /**
    * @see nexj.core.util.LookupDeque#lastKey()
    */
   public Object lastKey()
   {
      if (m_nLast >= 0)
      {
         return m_table[m_nLast];
      }

      return null;
   }

   /**
    * @see nexj.core.util.LookupDeque#firstValue()
    */
   public Object firstValue()
   {
      if (m_nFirst >= 0)
      {
         return m_table[m_nFirst + 1];
      }

      return null;
   }

   /**
    * @see nexj.core.util.LookupDeque#lastValue()
    */
   public Object lastValue()
   {
      if (m_nLast >= 0)
      {
         return m_table[m_nLast + 1];
      }

      return null;
   }

   /**
    * @see nexj.core.util.Lookup#clear()
    */
   public void clear()
   {
      java.util.Arrays.fill(m_table, null);
      m_nFirst = m_nLast = -1;
      m_nCount = m_nEmpty = 0;
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
      return new GenericLinkedHashTabIterator();
   }

   /**
    * @see nexj.core.util.Lookup#valueIterator()
    */
   public Iterator valueIterator()
   {
      return new GenericLinkedHashTabValueIterator();
   }

   /**
    * @see nexj.core.util.Lookup#iterator()
    */
   public Iterator reverseIterator()
   {
      return new GenericLinkedHashTabReverseIterator();
   }

   /**
    * @see nexj.core.util.Lookup#valueIterator()
    */
   public Iterator reverseValueIterator()
   {
      return new GenericLinkedHashTabReverseValueIterator();
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         GenericLinkedHashTab h = (GenericLinkedHashTab)super.clone();

         h.m_table = new Object[m_table.length];
         System.arraycopy(m_table, 0, h.m_table, 0, m_table.length);
         h.m_links = new int[m_links.length];
         System.arraycopy(m_links, 0, h.m_links, 0, m_links.length);

         return h;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('{');

      for (int i = m_nFirst; i >= 0; i = m_links[i])
      {
         if (i != m_nFirst)
         {
            writer.write(", ");
         }

         writer.print(m_table[i]);
         writer.write('=');
         writer.print(m_table[i + 1]);
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
      out.defaultWriteObject();
      out.writeInt(m_nCount);

      for (int i = m_nFirst; i >= 0; i = m_links[i])
      {
         out.writeObject(m_table[i]);
         out.writeObject(m_table[i + 1]);
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
         throw new InvalidObjectException("Negative LinkedHashTab value count");
      }

      int nSize = getTableSize(nCount);

      m_table = new Object[nSize];
      m_links = new int[nSize];
      m_nFirst = m_nLast = -1;

      while (nCount-- != 0)
      {
         Object key = in.readObject();
         put(key, in.readObject());
      }
   }

   // inner classes

   /**
    * GenericLinkedHashTab iterator.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected class GenericLinkedHashTabIterator implements Iterator
   {
      /**
       * The current value index.
       */
      protected int m_nCur = -1;

      /**
       * The next value index.
       */
      protected int m_nNext = m_nFirst;

      /**
       * The value table for iteration.
       */
      protected Object[] m_table = GenericLinkedHashTab.this.m_table;

      /**
       * The link table for iteration.
       */
      protected int[] m_links = GenericLinkedHashTab.this.m_links;

      // constructors

      /**
       * Creates an iterator from first item to last item.
       */
      public GenericLinkedHashTabIterator()
      {
      }

      // operations

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_nNext >= 0;
      }

      /**
       * @return The next available value.
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext >= 0)
         {
            m_nCur = m_nNext;
            m_nNext = m_links[m_nNext];

            return m_table[m_nCur];
         }

         throw new java.util.NoSuchElementException();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         if (m_nCur < 0)
         {
            throw new IllegalStateException();
         }

         if (m_table[m_nCur] == null || m_table[m_nCur] == EMPTY)
         {
            return;
         }

         if (m_table == GenericLinkedHashTab.this.m_table)
         {
            int nMask = m_table.length - 1;

            --m_nCount;
            ++m_nEmpty;
            unlink(m_nCur);
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
            GenericLinkedHashTab.this.remove(m_table[m_nCur]);
            m_table[m_nCur] = m_table[m_nCur + 1] = null;
         }
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getKey()
       */
      public Object getKey()
      {
         return m_table[m_nCur];
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getValue()
       */
      public Object getValue()
      {
         return m_table[m_nCur + 1];
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         m_table[m_nCur + 1] = value;

         if (m_table != GenericLinkedHashTab.this.m_table)
         {
            put(m_table[m_nCur], value);
         }
      }
   }

   /**
    * The same as GenericLinkedHashTabIterator, but direction of iteration is from last item to first item.
    */
   protected class GenericLinkedHashTabReverseIterator extends GenericLinkedHashTabIterator
   {
      // constructors

      /**
       * Creates an iterator from last item to first item.
       */
      public GenericLinkedHashTabReverseIterator()
      {
         m_nNext = m_nLast;
      }

      // operations

      /**
       * @see nexj.core.util.GenericLinkedHashTab.GenericLinkedHashTabIterator#next()
       */
      public Object next()
      {
         if (m_nNext >= 0)
         {
            m_nCur = m_nNext;
            m_nNext = m_links[m_nNext + 1];

            return m_table[m_nCur];
         }

         throw new java.util.NoSuchElementException();
      }
   }

   /**
    * The same as the GenericLinkedHashTabIterator, but next() returns the value.
    */
   protected class GenericLinkedHashTabValueIterator extends GenericLinkedHashTabIterator
   {
      // constructors

      /**
       * Creates a value iterator from first item to last item.
       */
      public GenericLinkedHashTabValueIterator()
      {
      }

      // operations

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext >= 0)
         {
            m_nCur = m_nNext;
            m_nNext = m_links[m_nNext];

            return m_table[m_nCur + 1];
         }

         throw new java.util.NoSuchElementException();
      }
   }

   /**
    * The same as GenericLinkedHashTabValueIterator, but direction of iteration is from last item to first item.
    */
   protected class GenericLinkedHashTabReverseValueIterator extends GenericLinkedHashTabValueIterator
   {
      // constructors

      /**
       * Creates a value iterator from last item to first item.
       */
      public GenericLinkedHashTabReverseValueIterator()
      {
         m_nNext = m_nLast;
      }

      // operations

      /**
       * @see nexj.core.util.GenericLinkedHashTab.GenericLinkedHashTabValueIterator#next()
       */
      public Object next()
      {
         if (m_nNext >= 0)
         {
            m_nCur = m_nNext;
            m_nNext = m_links[m_nNext + 1];

            return m_table[m_nCur + 1];
         }

         throw new java.util.NoSuchElementException();
      }
   }
}
