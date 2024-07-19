// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

/**
 * 2D hash table using weak references to store the values.
 */
public class WeakHashTab2D extends HashTab2D
{
   // constants
   
   /**
    * Serialization serial version UID. 
    */
   private final static long serialVersionUID = -2562146084347496558L;
   
   // associations

   /**
    * The reference queue with stale references populated by the garbage collector. 
    */
   protected transient ReferenceQueue m_refq = new ReferenceQueue();

   // constructors
   
   /**
    * Creates a hash table with an estimated key-value triple count of 8.
    */
   public WeakHashTab2D()
   {
      super();
   }

   /**
    * Creates a hash table with an estimated number of key-value triples.
    * @param nCount The estimated key-value triple count.
    */
   public WeakHashTab2D(int nCount)
   {
      super(nCount);
   }

   // operations
   

   /**
    * Removes the stale references from the queue and the hash table.
    */
   public void removeStaleReferences()
   {
      Ref ref;
      
      while ((ref = (Ref)m_refq.poll()) != null)
      {
         Ref oldRef = (Ref)super.remove(ref.key1, ref.key2);

         if (oldRef != null && oldRef.get() != null)
         {
            super.put(oldRef.key1, oldRef.key2, oldRef);
         }
      }
   }

   /**
    * @see nexj.core.util.HashTab2D#contains(java.lang.Object, java.lang.Object)
    */
   public boolean contains(Object key1, Object key2)
   {
      removeStaleReferences();
      
      return super.contains(key1, key2);
   }

   /**
    * @see nexj.core.util.HashTab2D#get(java.lang.Object, java.lang.Object)
    */
   public Object get(Object key1, Object key2)
   {
      removeStaleReferences();
      
      Object obj = super.get(key1, key2);
      
      if (obj != null)
      {
         obj = ((Ref)obj).get();
      }
      
      return obj;
   }

   /**
    * @see nexj.core.util.HashTab2D#put(java.lang.Object, java.lang.Object, java.lang.Object)
    */
   public Object put(Object key1, Object key2, Object value)
   {
      removeStaleReferences();

      Object obj = super.put(key1, key2, new Ref(key1, key2, value, m_refq));
      
      if (obj != null)
      {
         obj = ((Ref)obj).get();
      }
      
      return obj;
   }

   /**
    * @see nexj.core.util.HashTab2D#remove(java.lang.Object, java.lang.Object)
    */
   public Object remove(Object key1, Object key2)
   {
      removeStaleReferences();

      Object obj = super.remove(key1, key2);

      if (obj != null)
      {
         obj = ((Ref)obj).get();
      }
      
      return obj;
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#clear()
    */
   public void clear()
   {
      super.clear();

      while (m_refq.poll() != null);
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#size()
    */
   public int size()
   {
      removeStaleReferences();

      return super.size();
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#valueIterator()
    * next() and getValue() may return null.
    */
   public Iterator valueIterator()
   {
      removeStaleReferences();

      return new WeakHashTab2DValueIterator();
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#clone()
    */
   public Object clone()
   {
      removeStaleReferences();

      WeakHashTab2D htab = (WeakHashTab2D)super.clone();
      Ref ref;

      htab.m_refq = new ReferenceQueue();

      for (int i = 2; i < m_table.length; i += 3)
      {
         ref = (Ref)m_table[i];

         if (ref != null)
         {
            htab.m_table[i] = new Ref(ref.key1, ref.key2, ref.get(), htab.m_refq);
         }
      }

      while ((ref = (Ref)m_refq.poll()) != null)
      {
         Ref oldRef = (Ref)super.remove(ref.key1, ref.key2);

         if (oldRef != null && oldRef.get() != null)
         {
            super.put(oldRef.key1, oldRef.key2, oldRef);
         }
         else
         {
            htab.remove(ref.key1, ref.key2);
         }
      }

      return htab;
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      removeStaleReferences();
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
            writer.print(((Ref)m_table[i + 2]).get());
         }
      }

      writer.write('}');
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#serialize(java.io.ObjectOutputStream)
    */
   protected void serialize(ObjectOutputStream out) throws IOException
   {
      Object[] table = new Object[m_table.length];
      
      for (int i = 2; i < table.length; i += 3)
      {
         Object obj = m_table[i];

         if (obj != null)
         {
            table[i] = ((Ref)obj).get();
         }
      }

      removeStaleReferences();
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
            out.writeObject(((Ref)m_table[i + 2]).get());
            --nCount;
         }
      }
   }

   /**
    * @see nexj.core.util.GenericHashTab2D#deserialize(java.io.ObjectInputStream)
    */
   protected void deserialize(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      m_refq = new ReferenceQueue();

      super.deserialize(in);
   }

   // The weak reference

   protected static class Ref extends WeakReference
   {
      // associations

      /**
       * The first object key.
       */
      public Object key1;

      /**
       * The second object key.
       */
      public Object key2;

      // constructor

      /**
       * Constructs the weak reference.
       * @param key1 The first object key.
       * @param key2 The second object key.
       * @param referent The referred object.
       * @param q The reference queue.
       */
      public Ref(Object key1, Object key2, Object referent, ReferenceQueue q)
      {
         super(referent, q);
         this.key1 = key1;
         this.key2 = key2;
      }
   }

   // The value iterator.

   protected class WeakHashTab2DValueIterator extends GenericHashTab2DValueIterator
   {
      /**
       * @see java.util.Iterator#next()
       * May return null.
       */
      public Object next()
      {
         return ((Ref)super.next()).get();
      }

      /**
       * @see nexj.core.util.GenericHashTab2D.GenericHashTab2DValueIterator#getValue()
       * May return null.
       */
      public Object getValue()
      {
         return ((Ref)super.getValue()).get();
      }

      /**
       * @see nexj.core.util.GenericHashTab2D.GenericHashTab2DValueIterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         m_table[m_nCur + 2] = new Ref(m_table[m_nCur], m_table[m_nCur + 1], value, m_refq);

         if (m_table != WeakHashTab2D.this.m_table)
         {
            put(m_table[m_nCur], m_table[m_nCur + 1], value);
         }
      }
   }
}
