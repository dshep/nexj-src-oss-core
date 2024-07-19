// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

/**
 * Hash table with weak keys.
 */
public class WeakKeyHashTab extends HashTab
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -8463528896477140250L;

   // associations

   /**
    * The reference queue with stale references, populated by the garbage collector.
    */
   protected transient ReferenceQueue m_refq = new ReferenceQueue();

   // constructors

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public WeakKeyHashTab()
   {
      super();
   }

   /**
    * Creates a hash table with an estimated key-value pair count.
    * 
    * @param nCount The estimated key-value pair count.
    */
   public WeakKeyHashTab(int nCount)
   {
      super(nCount);
   }

   // operations

   /**
    * Removes the stale references from the queue and the hash table
    */
   public void removeStaleReferences()
   {
      Ref ref;

      while ((ref = (Ref)m_refq.poll()) != null)
      {
         int nMask = m_table.length - 1;
         int i = (ref.hashCode() << 1) & nMask;

         for (;;)
         {
            Object key = m_table[i];

            if (key == ref)
            {
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

               break;
            }

            if (key == null)
            {
               break;
            }

            i = (i + 2) & nMask;
         }
      }
   }

   /**
    * @see nexj.core.util.Lookup#contains(java.lang.Object)
    */
   public boolean contains(Object key)
   {
      removeStaleReferences();

      return super.contains(key);
   }

   /**
    * @see nexj.core.util.Lookup#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      removeStaleReferences();

      return super.get(key);
   }

   /**
    * @see nexj.core.util.Lookup#put(java.lang.Object, java.lang.Object)
    */
   public Object put(Object key, Object value)
   {
      removeStaleReferences();

      return super.put(new Ref(key, m_refq), value);
   }

   /**
    * @see nexj.core.util.Lookup#remove(java.lang.Object)
    */
   public Object remove(Object key)
   {
      removeStaleReferences();

      return super.remove(key);
   }

   /**
    * @see nexj.core.util.Lookup#clear()
    */
   public void clear()
   {
      super.clear();

      while (m_refq.poll() != null);
   }

   /**
    * @see nexj.core.util.Lookup#size()
    */
   public int size()
   {
      removeStaleReferences();

      return super.size();
   }

   /**
    * @see nexj.core.util.GenericHashTab#serialize(java.io.ObjectOutputStream)
    */
   protected void serialize(ObjectOutputStream out) throws IOException
   {
      Object[] table = new Object[m_table.length];
      
      for (int i = 0; i < table.length; i += 2)
      {
         if (m_table[i] != null)
         {
            table[i] = ((Ref)m_table[i]).get();
         }
      }

      removeStaleReferences();
      out.defaultWriteObject();
      out.writeInt(m_nCount);

      int nCount = m_nCount;

      for (int i = 0; nCount != 0; i += 2)
      {
         Object key = m_table[i];

         if (key != null && key != EMPTY)
         {
            out.writeObject(((Ref)key).get());
            out.writeObject(m_table[i + 1]);
            --nCount;
         }
      }
   }

   /**
    * @see nexj.core.util.GenericHashTab#deserialize(java.io.ObjectInputStream)
    */
   protected void deserialize(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      m_refq = new ReferenceQueue();

      super.deserialize(in);
   }

   /**
    * @see nexj.core.util.Lookup#iterator()
    * getValue() may return null.
    */
   public Iterator iterator()
   {
      removeStaleReferences();

      return new WeakKeyHashTabIterator();
   }

   /**
    * Weak instance reference.
    */
   protected final static class Ref extends WeakReference implements Printable
   {
      // attributes

      /**
       * The hash code.
       */
      protected int m_nHashCode;

      // constructors

      /**
       * Constructs the reference.
       * @param key The instance.
       * @param q The reference queue.
       */
      public Ref(Object key, ReferenceQueue q)
      {
         super(key, q);

         if (key != null)
         {
            m_nHashCode = key.hashCode();
         }
      }

      // operations

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_nHashCode;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         Object ref = get();

         if (obj instanceof Ref)
         {
            Object ref2 = ((Ref)obj).get();

            if (ref2 != null)
            {
               return ref2.equals(ref);
            }

            if (ref == null)
            {
               return obj == this;
            }

            return false;
         }

         if (obj != null)
         {
            return obj.equals(ref);
         }

         return false;
      }

      /**
       * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
       */
      public void printOn(PrintWriter writer) throws IOException
      {
         Object obj = get();
         
         if (obj != null)
         {
            writer.print(obj);
         }
         else
         {
            writer.write(super.toString());
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         Object obj = get();

         return (obj != null) ? obj.toString() : super.toString();
      }
   }   

   // The key iterator

   protected class WeakKeyHashTabIterator extends GenericHashTabIterator
   {
      /**
       * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#next()
       */
      public Object next()
      {
         return ((Ref)super.next()).get();
      }

      /**
       * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#getKey()
       */
      public Object getKey()
      {
         return ((Ref)super.getKey()).get();
      }
   }
}