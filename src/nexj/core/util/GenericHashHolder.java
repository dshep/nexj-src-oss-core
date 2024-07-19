// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Iterator;

/**
 * Generic hash set implementation.
 * The focus is on efficiency. No error checking is provided. 
 */
public abstract class GenericHashHolder extends GenericCollection implements Holder, Cloneable, Serializable, Printable
{
   // constants
   
   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 5360286262970503908L;

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
    * The value count.
    */
   protected transient int m_nCount;

   /**
    * The empty item count.
    */
   protected transient int m_nEmpty;

   // constructors

   /**
    * Creates a hash holder with an estimated number of values.
    * @param nCount The estimated value count.
    */
   protected GenericHashHolder(int nCount)
   {
      m_table = new Object[getTableSize(nCount)];
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    */
   protected GenericHashHolder()
   {
      m_table = new Object[16];
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
    * Rehashes the table.
    */
   protected final void rehash()
   {
      rehash(getTableSize(m_nCount));
   }

   /**
    * Rehashes the table.
    * @param nSize The new table size.
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
               int i = hash(value) & nMask2;

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
         else if (equal(value, value2))
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
      int i = hash(value) & nMask;

      for (;;)
      {
         Object value2 = m_table[i];

         if (value2 == null)
         {
            return false;
         }

         if (equal(value, value2))
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

         if (equal(value, value2))
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

         if (equal(value, value2))
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
    * @see nexj.core.util.Holder#clear()
    */
   public void clear()
   {
      java.util.Arrays.fill(m_table, null);
      m_nCount = m_nEmpty = 0;
   }

   /**
    * Removes all values from the holder and resets its capacity
    * to the given number.
    * @param nCapacity The estimated value count.
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
    * @see nexj.core.util.Holder#size()
    */
   public int size()
   {
      return m_nCount;
   }

   /**
    * @see nexj.core.util.Holder#iterator()
    */
   public Iterator iterator()
   {
      return new GenericHashHolderIterator();
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         GenericHashHolder h = (GenericHashHolder)super.clone();

         h.m_table = new Object[m_table.length];
         System.arraycopy(m_table, 0, h.m_table, 0, m_table.length);

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
      writer.write('[');

      boolean bFirst = true;

      for (int i = 0, nLen = m_table.length; i < nLen; ++i)
      {
         Object value = m_table[i];

         if (value != null && value != EMPTY)
         {
            if (bFirst)
            {
               bFirst = false;
            }
            else
            {
               writer.write(", ");
            }

            writer.print(value);
         }
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
      int nCount = m_nCount;

      out.defaultWriteObject();
      out.writeInt(nCount);

      for (int i = 0; nCount != 0; ++i)
      {
         Object value = m_table[i];
         
         if (value != null && value != EMPTY)
         {
            out.writeObject(value);
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
         throw new InvalidObjectException("Negative HashHolder value count");
      }
      
      m_table = new Object[getTableSize(nCount)];
      
      while (nCount-- != 0)
      {
         add(in.readObject());
      }
   }
   
   // inner classes
   
   /**
    * GenericHashHolder iterator.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected class GenericHashHolderIterator implements Iterator
   {
      /**
       * The next value index.
       */
      protected int m_nNext = -1;
      
      /**
       * The current value index.
       */
      protected int m_nCur;
      
      /**
       * The table for iteration.
       */
      protected Object[] m_table = GenericHashHolder.this.m_table;

      /**
       * Creates an iterator.
       */
      protected GenericHashHolderIterator()
      {
         incr();
      }

      /**
       * Advances to the next item.
       * @return True if there is next item.
       */
      protected boolean incr()
      {
         m_nCur = m_nNext++;
         
         while (m_nNext < m_table.length)
         {
            if (m_table[m_nNext] != null && m_table[m_nNext] != EMPTY)
            {
               return true;
            }
            
            ++m_nNext;
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
       * @return The next available value.
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext < m_table.length)
         {
            incr();
            
            return m_table[m_nCur];
         }
         
         throw new java.util.NoSuchElementException();
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

         if (m_table == GenericHashHolder.this.m_table)
         {
            int nMask = m_table.length - 1;

            --m_nCount;
            ++m_nEmpty;

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
            GenericHashHolder.this.remove(m_table[m_nCur]);
            m_table[m_nCur] = null;
         }
      }
   }
}
