// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Iterator;

/**
 * Generic hash set/deque implementation.
 * The focus is on efficiency. No error checking is provided.
 */
public abstract class GenericHashDeque extends GenericCollection implements HolderDeque, Cloneable, Serializable, Printable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -1229279381615305062L;

   /**
    * Value indicating deallocated cells.
    */
   protected final static Object EMPTY = new Object();

   // attributes

   /**
    * The value table.
    */
   protected transient Object[] m_table;

   /**
    * The link table: nNext[2*n], nPrev[2*n+1].
    */
   protected transient int[] m_links;

   /**
    * The first value index, or -1 if empty.
    */
   protected transient int m_nFirst;

   /**
    * The last value index, or -1 if empty.
    */
   protected transient int m_nLast;

   /**
    * The value count.
    */
   protected transient int m_nCount;

   /**
    * The empty item count.
    */
   protected transient int m_nEmpty;

   // constructors

   /**
    * Creates a hash deque with an estimated number of values.
    * @param nCount The estimated value count.
    */
   protected GenericHashDeque(int nCount)
   {
      int nSize = getTableSize(nCount);

      m_table = new Object[nSize];
      m_links = new int[nSize << 1];
      m_nFirst = m_nLast = -1;
   }

   /**
    * Creates a hash deque with an estimated value count of 8.
    */
   protected GenericHashDeque()
   {
      m_table = new Object[16];
      m_links = new int[32];
      m_nFirst = m_nLast = -1;
   }

   // operations

   /**
    * Calculates the table size as a power of two.
    * @param nCount The count of values to store.
    * @return The calculated size.
    */
   protected static int getTableSize(int nCount)
   {
      if (nCount < 1)
      {
         nCount = 1;
      }

      // Initial load factor 2/3
      return MathUtil.ceil2(nCount + ((nCount + 1) >> 1));
   }

   /**
    * Computes the hash code of a value.
    * @param value The value.
    * @return The hash code.
    */
   protected abstract int hash(Object value);

   /**
    * Compares two values for equality.
    * @param left The left value.
    * @param right The right value.
    * @return True if the values are equal.
    */
   protected abstract boolean equal(Object left, Object right);

   /**
    * Rehashes the hash table.
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
         int[] links2 = new int[nSize2 << 1];
         int nMask2 = nSize2 - 1;
         int nFirst = -1;
         int nLast = -1;

         for (int k = m_nFirst; k >= 0; k = m_links[k << 1])
         {
            Object value = m_table[k];
            int i = hash(value) & nMask2;

            for (;;)
            {
               if (table2[i] == null)
               {
                  table2[i] = value;
                  links2[i << 1] = -1;
                  links2[(i << 1) + 1] = nLast;

                  if (nLast >= 0)
                  {
                     links2[nLast << 1] = i;
                  }
                  else
                  {
                     nFirst = i;
                  }

                  nLast = i;

                  break;
               }

               i = (i + 1) & nMask2;
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
    * Removes a value from the hash table (but not from the linked list).
    * @param i The index of the value.
    * @param nMask The table mask (size - 1).
    */
   protected void remove(int i, int nMask)
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

         // Load factor [1/4..3/4]
         if ((m_nCount << 2) - 1 < nMask ||
            ((nMask - m_nCount - m_nEmpty) << 2) + 3  < nMask)
         {
            rehash();
         }
      }
   }

   /**
    * Removes a value from the linked list (but not from the hash table).
    * @param i The index of the value.
    */
   protected void unlink(int i)
   {
      int nNext = m_links[i << 1];
      int nPrev = m_links[(i << 1) + 1];

      if (nNext >= 0)
      {
         m_links[(nNext << 1) + 1] = nPrev;
      }
      else
      {
         m_nLast = nPrev;
      }

      if (nPrev >= 0)
      {
         m_links[nPrev << 1] = nNext;
      }
      else
      {
         m_nFirst = nNext;
      }
   }

   /**
    * @see nexj.core.util.Holder#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      int nMask = m_table.length - 1;
      int i = hash(value) & nMask;
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
            m_links[i << 1] = -1;
            m_links[(i << 1) + 1] = m_nLast;

            if (m_nLast >= 0)
            {
               m_links[m_nLast << 1] = i;
            }
            else
            {
               m_nFirst = i;
            }

            m_nLast = i;

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
         else if (equal(value2, value))
         {
            m_table[i] = value;

            return false;
         }

         i = (i + 1) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Deque#addFirst(java.lang.Object)
    */
   public boolean addFirst(Object value)
   {
      int nMask = m_table.length - 1;
      int i = hash(value) & nMask;
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

            if (m_nFirst >= 0)
            {
               m_links[(m_nFirst << 1) + 1] = i;
            }
            else
            {
               m_nLast = i;
            }

            m_links[i << 1] = m_nFirst;
            m_links[(i << 1) + 1] = -1;
            m_nFirst = i;

            // Max load factor 3/4
            if (((nMask - ++m_nCount - m_nEmpty) << 2) + 3 < nMask)
            {
               rehash();
            }

            return true;
         }

         if (value2 == EMPTY)
         {
            k = i;
         }
         else if (equal(value2, value))
         {
            m_table[i] = value;
            unlink(i);

            if (m_nFirst >= 0)
            {
               m_links[(m_nFirst << 1) + 1] = i;
            }
            else
            {
               m_nLast = i;
            }

            m_links[i << 1] = m_nFirst;
            m_links[(i << 1) + 1] = -1;
            m_nFirst = i;

            return false;
         }

         i = (i + 1) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Deque#addLast(java.lang.Object)
    */
   public boolean addLast(Object value)
   {
      int nMask = m_table.length - 1;
      int i = hash(value) & nMask;
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
            m_links[i << 1] = -1;
            m_links[(i << 1) + 1] = m_nLast;

            if (m_nLast >= 0)
            {
               m_links[m_nLast << 1] = i;
            }
            else
            {
               m_nFirst = i;
            }

            m_nLast = i;

            // Max load factor 3/4
            if (((nMask - ++m_nCount - m_nEmpty) << 2) + 3 < nMask)
            {
               rehash();
            }

            return true;
         }

         if (value2 == EMPTY)
         {
            k = i;
         }
         else if (equal(value2, value))
         {
            m_table[i] = value;
            unlink(i);
            m_links[i << 1] = -1;
            m_links[(i << 1) + 1] = m_nLast;

            if (m_nLast >= 0)
            {
               m_links[m_nLast << 1] = i;
            }
            else
            {
               m_nFirst = i;
            }

            m_nLast = i;

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
      int i = hash(value) & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return false;
         }

         if (equal(value2, value))
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
      int i = hash(value) & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return null;
         }

         if (equal(value2, value))
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
      int i = hash(value) & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return false;
         }

         if (equal(value2, value))
         {
            unlink(i);
            remove(i, nMask);

            return true;
         }

         i = (i + 1) & nMask;
      }
   }

   /**
    * @see nexj.core.util.Deque#removeFirst()
    */
   public Object removeFirst()
   {
      Object value = null;
      int i = m_nFirst;

      if (i >= 0)
      {
         value = m_table[i];
         unlink(i);
         remove(i, m_table.length - 1);
      }

      return value;
   }

   /**
    * @see nexj.core.util.Deque#removeLast()
    */
   public Object removeLast()
   {
      Object value = null;
      int i = m_nLast;

      if (i >= 0)
      {
         value = m_table[i];
         unlink(i);
         remove(i, m_table.length - 1);
      }

      return value;
   }

   /**
    * @see nexj.core.util.Deque#first()
    */
   public Object first()
   {
      if (m_nFirst >= 0)
      {
         return m_table[m_nFirst];
      }

      return null;
   }

   /**
    * @see nexj.core.util.Deque#last()
    */
   public Object last()
   {
      if (m_nLast >= 0)
      {
         return m_table[m_nLast];
      }

      return null;
   }

   /**
    * @see Holder#clear()
    * @see Deque#clear();
    */
   public void clear()
   {
      java.util.Arrays.fill(m_table, null);
      m_nFirst = m_nLast = -1;
      m_nCount = m_nEmpty = 0;
   }

   /**
    * @see Holder#size()
    * @see Deque#size()
    */
   public int size()
   {
      return m_nCount;
   }

   /**
    * @see Holder#iterator()
    * @see Deque#iterator()
    */
   public Iterator iterator()
   {
      return new GenericHashDequeIterator();
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         GenericHashDeque hdq = (GenericHashDeque)super.clone();

         hdq.m_table = new Object[m_table.length];
         System.arraycopy(m_table, 0, hdq.m_table, 0, m_table.length);
         hdq.m_links = new int[m_links.length];
         System.arraycopy(m_links, 0, hdq.m_links, 0, m_links.length);

         return hdq;
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
      writer.write('[');

      for (int i = m_nFirst; i >= 0; i = m_links[i << 1])
      {
         if (i != m_nFirst)
         {
            writer.write(", ");
         }

         writer.print(m_table[i]);
      }

      writer.write(']');
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

      for (int i = m_nFirst; i >= 0; i = m_links[i << 1])
      {
         out.writeObject(m_table[i]);
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
         throw new InvalidObjectException("Negative HashDeque value count");
      }

      int nSize = getTableSize(nCount);

      m_table = new Object[nSize];
      m_links = new int[nSize << 1];
      m_nFirst = m_nLast = -1;

      while (nCount-- != 0)
      {
         add(in.readObject());
      }
   }

   // inner classes

   /**
    * GenericHashDeque iterator.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected class GenericHashDequeIterator implements Iterator
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
      protected Object[] m_table = GenericHashDeque.this.m_table;

      /**
       * The link table for iteration.
       */
      protected int[] m_links = GenericHashDeque.this.m_links;

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
            m_nNext = m_links[m_nNext << 1];

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

         if (m_table == GenericHashDeque.this.m_table)
         {
            int nMask = m_table.length - 1;

            --m_nCount;
            ++m_nEmpty;
            unlink(m_nCur);

            if (m_table[(m_nCur + 1) & nMask] == null)
            {
               int i = m_nCur;

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
               m_table[m_nCur] = EMPTY;

               // Rehash when the load factor exceeds 3/4
               if (((nMask - m_nCount - m_nEmpty) << 2) + 3  < nMask)
               {
                  rehash();
               }
            }
         }
         else
         {
            GenericHashDeque.this.remove(m_table[m_nCur]);
            m_table[m_nCur] = null;
         }
      }
   }
}
