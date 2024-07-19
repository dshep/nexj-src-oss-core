// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;

/**
 * Binary heap implementation.
 */
public class BinaryHeap implements Heap, Cloneable, java.io.Serializable
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 4955235494314693399L;

   // attributes

   /**
    * The current element count.
    */
   protected transient int m_nCount;

   // associations

   /**
    * Item array.
    */
   protected transient Object[] m_items;
   
   /**
    * Item comparator.
    */
   protected Comparator m_comparator;
   
   // constructors
   
   /**
    * Constructs the heap.
    * @param nCount The estimated item count.
    * @param comparator The item comparator.
    */
   public BinaryHeap(int nCount, Comparator comparator)
   {
      m_items = new Object[nCount];
      m_comparator = comparator;
   }

   /**
    * Constructs the heap with an estimated item count of 8.
    */
   public BinaryHeap(Comparator comparator)
   {
      this(8, comparator);
   }

   // operations

   /**
    * @see nexj.core.util.Heap#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      int i = m_nCount++;
      
      if (i == m_items.length)
      {
         Object[] array = new Object[i << 1];

         System.arraycopy(m_items, 0, array, 0, i);
         m_items = array;
      }
      
      m_items[i] = value;

      while (i != 0)
      {
         int k = ((i + 1) >> 1) - 1;
         Object left = m_items[k];
         Object right = m_items[i];

         if (m_comparator.compare(left, right) < 0)
         {
            break;
         }
         
         m_items[k] = right;
         m_items[i] = left;
         i = k;
      }

      return true;
   }

   /**
    * @see nexj.core.util.Heap#clear()
    */
   public void clear()
   {
      Arrays.fill(m_items, 0, m_nCount, null);
      m_nCount = 0;
   }

   /**
    * @see nexj.core.util.Heap#contains(java.lang.Object)
    */
   public boolean contains(Object value)
   {
      for (int i = 0; i < m_nCount; ++i)
      {
         if (m_items[i].equals(value))
         {
            return true;
         }
      }
      
      return false;
   }

   /**
    * @see nexj.core.util.Heap#first()
    */
   public Object first()
   {
      if (m_nCount != 0)
      {
         return m_items[0];
      }

      return null;
   }

   /**
    * @see nexj.core.util.Heap#removeFirst()
    */
   public Object removeFirst()
   {
      if (m_nCount != 0)
      {
         Object value = m_items[0];

         m_items[0] = m_items[--m_nCount];
         m_items[m_nCount] = null;

         for (int i = 0, k; (k = ((i + 1) << 1)) <= m_nCount; i = k)
         {
            if (k == m_nCount || m_comparator.compare(m_items[k - 1], m_items[k]) <= 0)
            {
               --k;
            }

            Object left = m_items[i];
            Object right = m_items[k];

            if (m_comparator.compare(left, right) <= 0)
            {
               break;
            }

            m_items[i] = right;
            m_items[k] = left;
         }

         return value;
      }

      return null;
   }

   /**
    * @see nexj.core.util.Heap#remove(java.lang.Object)
    */
   public boolean remove(Object value)
   {
      for (int i = 0; i < m_nCount; ++i)
      {
         if (m_items[i].equals(value))
         {
            remove(i);

            return true;
         }
      }
      
      return false;
   }

   /**
    * Removes i-th item from the heap.
    * @param i The item index.
    */
   protected void remove(int i)
   {
      while (i != 0)
      {
         int k = ((i + 1) >> 1) - 1;

         m_items[i] = m_items[k];
         i = k;
      }

      removeFirst();
   }
   
   /**
    * @see nexj.core.util.Heap#size()
    */
   public int size()
   {
      return m_nCount;
   }

   /**
    * @see nexj.core.util.Heap#iterator()
    */
   public Iterator iterator()
   {
      return new Iterator()
      {
         private int m_nCur;

         public boolean hasNext()
         {
            return m_nCur < m_nCount;
         }

         public Object next()
         {
            if (m_nCur < m_nCount)
            {
               return m_items[m_nCur++];
            }
            
            throw new java.util.NoSuchElementException();
         }

         public void remove()
         {
            if (m_nCur > m_nCount || m_nCur == 0)
            {
               throw new IllegalStateException();
            }

            BinaryHeap.this.remove(--m_nCur);
         }
      };
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         BinaryHeap heap = (BinaryHeap)super.clone();

         heap.m_items = new Object[Math.max(m_nCount, 4)];
         System.arraycopy(m_items, 0, heap.m_items, 0, m_nCount);

         return heap;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(m_nCount << 3);
      
      buf.append('[');
      
      for (int i = 0; i < m_nCount; ++i)
      {
         if (i != 0)
         {
            buf.append(", ");
         }

         buf.append(m_items[i]);
      }
      
      buf.append(']');
      
      return buf.toString();
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

      for (int i = 0; i < m_nCount; ++i)
      {
         out.writeObject(m_items[i]);
      }
   }

   /**
    * Deserializes the object.
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
      
      m_nCount = in.readInt();

      if (m_nCount < 0)
      {
         throw new InvalidObjectException("Negative BinaryHeap value count");
      }
      
      m_items = new Object[Math.max(m_nCount, 4)];

      for (int i = 0; i < m_nCount; ++i)
      {
         m_items[i] = in.readObject();
      }
   }
}
