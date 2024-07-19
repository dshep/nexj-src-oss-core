// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * Generic 2D hash table implementation.
 * The focus is on efficiency. No error checking is provided.
 */
public abstract class GenericHashTab2D implements Lookup2D, Cloneable, Serializable, Printable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 155949742547387287L;

   public final static Iterator EMPTY_ITERATOR = new Iterator()
   {
      public Object getKey1()
      {
         throw new IllegalStateException();
      }

      public Object getKey2()
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
    * The key-value table: key1[3*n], key2[3*n+1], value[3*n+2].
    */
   protected transient Object[] m_table;

   /**
    * The key-value triple count.
    */
   protected transient int m_nCount;

   /**
    * The empty items count.
    */
   protected transient int m_nEmpty;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value triples.
    * @param nCount The estimated key-value triple count.
    */
   protected GenericHashTab2D(int nCount)
   {
      m_table = new Object[getTableSize(nCount)];
   }

   /**
    * Creates a hash table with an estimated key-value triple count of 8.
    */
   protected GenericHashTab2D()
   {
      m_table = new Object[36];
   }

   // operations

   /**
    * Calculates the table size.
    * @param nCount The count of key-value triples to store.
    * @return The calculated size.
    */
   protected static int getTableSize(int nCount)
   {
      if (nCount < 1)
      {
         nCount = 1;
      }

      // Initial load factor 2/3
      return MathUtil.ceil2(nCount + ((nCount + 1) >> 1)) * 3;
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
    * @param nSize The new hash table size.
    */
   protected abstract void rehash(int nSize);

   /**
    * @see nexj.core.util.Lookup#clear()
    */
   public void clear()
   {
      java.util.Arrays.fill(m_table, null);
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
    * @see nexj.core.util.Lookup#valueIterator()
    */
   public Iterator valueIterator()
   {
      return new GenericHashTab2DValueIterator();
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         GenericHashTab2D ht = (GenericHashTab2D)super.clone();

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
      out.defaultWriteObject();
      out.writeInt(m_nCount);

      int nCount = m_nCount;

      for (int i = 0; nCount != 0; i += 3)
      {
         Object key1 = m_table[i];

         if (key1 != null && key1 != EMPTY)
         {
            out.writeObject(key1);
            out.writeObject(m_table[i + 1]);
            out.writeObject(m_table[i + 2]);
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
    * @param in The serialization stream.
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
         Object key1 = in.readObject();
         Object key2 = in.readObject();

         put(key1, key2, in.readObject());
      }
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('{');

      boolean bFirst = true;

      for (int i = 0, nLen = m_table.length; i < nLen; i += 3)
      {
         Object key1 = m_table[i];

         if (key1 != null && key1 != EMPTY)
         {
            if (bFirst)
            {
               bFirst = false;
            }
            else
            {
               writer.write(", ");
            }

            writer.write('(');
            writer.print(key1);
            writer.write(',');
            writer.print(m_table[i + 1]);
            writer.write(")=");
            writer.print(m_table[i + 2]);
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
   protected class GenericHashTab2DValueIterator implements Iterator
   {
      /**
       * The next key index.
       */
      protected int m_nNext = -3;

      /**
       * The current key index.
       */
      protected int m_nCur;

      /**
       * The table for iteration.
       */
      protected Object[] m_table = GenericHashTab2D.this.m_table;

      /**
       * Creates an iterator.
       */
      protected GenericHashTab2DValueIterator()
      {
         incr();
      }

      /**
       * Advances to the next item.
       * @return True if there is next item.
       */
      protected boolean incr()
      {
         m_nCur = m_nNext;
         m_nNext += 3;

         while (m_nNext < m_table.length)
         {
            if (m_table[m_nNext] != null && m_table[m_nNext] != EMPTY)
            {
               return true;
            }

            m_nNext += 3;
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
       * @return The next available element value.
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext < m_table.length)
         {
            incr();

            return m_table[m_nCur + 2];
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

         if (m_table == GenericHashTab2D.this.m_table)
         {
            int nSize = m_table.length;

            --m_nCount;
            ++m_nEmpty;
            m_table[m_nCur + 1] = m_table[m_nCur + 2] = null;

            if (m_table[(m_nCur + 3) % nSize] == null)
            {
               int i = m_nCur;

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
               m_table[m_nCur] = EMPTY;

               // Max load factor 3/4
               if ((m_nCount + m_nEmpty) << 2 > nSize)
               {
                  rehash();
               }
            }
         }
         else
         {
            GenericHashTab2D.this.remove(m_table[m_nCur], m_table[m_nCur + 1]);
            m_table[m_nCur] = m_table[m_nCur + 1] = m_table[m_nCur + 2] = null;
         }
      }

      /**
       * @return The first key retrieved by the last next() invocation.
       */
      public Object getKey1()
      {
         return m_table[m_nCur];
      }

      /**
       * @return The second key retrieved by the last next() invocation.
       */
      public Object getKey2()
      {
         return m_table[m_nCur + 1];
      }

      /**
       * @return The value associated with the last next() invocation.
       */
      public Object getValue()
      {
         return m_table[m_nCur + 2];
      }

      /**
       * Replaces the value associated with the last next() invocation.
       * @param value The value to set.
       */
      public void setValue(Object value)
      {
         m_table[m_nCur + 2] = value;

         if (m_table != GenericHashTab2D.this.m_table)
         {
            put(m_table[m_nCur], m_table[m_nCur + 1], value);
         }
      }
   }
}
