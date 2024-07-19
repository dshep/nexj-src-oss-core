// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

/**
 * Hash table using weak references to store the values.
 */
public class WeakHashTab extends HashTab
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -4485235201522255440L;

   // associations

   /**
    * The reference queue with stale references populated by the garbage collector. 
    */
   protected transient ReferenceQueue m_refq = new ReferenceQueue();

   // constructors

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public WeakHashTab()
   {
      super();
   }

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   public WeakHashTab(int nCount)
   {
      super(nCount);
   }
   
   // operations

   /**
    * Removes the stale references from the queue and the hash table.
    */
   protected void removeStaleReferences()
   {
      Reference ref;
      
      while ((ref = m_refq.poll()) != null)
      {
         Ref oldRef = (Ref)super.remove(((Ref)ref).key);
         
         if (oldRef != null && oldRef.get() != null)
         {
            super.put(oldRef.key, oldRef);
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
      
      Object obj = super.get(key);
      
      if (obj != null)
      {
         obj = ((Ref)obj).get();
      }
      
      return obj;
   }

   /**
    * @see nexj.core.util.Lookup#put(java.lang.Object, java.lang.Object)
    */
   public Object put(Object key, Object value)
   {
      removeStaleReferences();

      Object obj = super.put(key, new Ref(key, value, m_refq));
      
      if (obj != null)
      {
         obj = ((Ref)obj).get();
      }
      
      return obj;
   }

   /**
    * @see nexj.core.util.Lookup#remove(java.lang.Object)
    */
   public Object remove(Object key)
   {
      removeStaleReferences();

      Object obj = super.remove(key);

      if (obj != null)
      {
         obj = ((Ref)obj).get();
      }
      
      return obj;
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
    * @see nexj.core.util.GenericHashTab2D#serialize(java.io.ObjectOutputStream)
    */
   protected void serialize(ObjectOutputStream out) throws IOException
   {
      Object[] table = new Object[m_table.length];
      
      for (int i = 1; i < table.length; i += 2)
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

      for (int i = 0; nCount != 0; i += 2)
      {
         Object key = m_table[i];

         if (key != null && key != EMPTY)
         {
            out.writeObject(key);
            out.writeObject(((Ref)m_table[i + 1]).get());
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

   /**
    * @see nexj.core.util.GenericHashTab#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      removeStaleReferences();
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
            writer.print(((Ref)m_table[i + 1]).get());
         }
      }

      writer.write('}');
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      removeStaleReferences();

      WeakHashTab htab = (WeakHashTab)super.clone();
      Ref ref;

      htab.m_refq = new ReferenceQueue();

      for (int i = 1; i < m_table.length; i += 2)
      {
         ref = (Ref)m_table[i];

         if (ref != null)
         {
            htab.m_table[i] = new Ref(ref.key, ref.get(), htab.m_refq);
         }
      }

      while ((ref = (Ref)m_refq.poll()) != null)
      {
         Ref oldRef = (Ref)super.remove(ref.key);

         if (oldRef != null && oldRef.get() != null)
         {
            super.put(oldRef.key, oldRef);
         }
         else
         {
            htab.remove(ref.key);
         }
      }

      return htab;
   }

   /**
    * @see nexj.core.util.Lookup#iterator()
    * getValue() may return null. 
    */
   public Iterator iterator()
   {
      removeStaleReferences();

      return new WeakHashTabIterator();
   }

   /**
    * @see nexj.core.util.Lookup#valueIterator()
    * next() and getValue() may return null.
    */
   public Iterator valueIterator()
   {
      removeStaleReferences();

      return new WeakHashTabValueIterator();
   }

   // The weak reference
   
   protected static class Ref extends WeakReference
   {
      // associations

      /**
       * The object key.
       */
      public Object key;

      // constructor

      /**
       * Constructs the weak reference.
       * @param key The object key.
       * @param referent The referred object.
       * @param q The reference queue.
       */
      public Ref(Object key, Object referent, ReferenceQueue q)
      {
         super(referent, q);
         this.key = key;
      }
   }

   // The key iterator

   protected class WeakHashTabIterator extends GenericHashTabIterator
   {
      /**
       * @see nexj.core.util.Lookup.Iterator#getValue()
       * May return null.
       */
      public Object getValue()
      {
         return ((Ref)super.getValue()).get();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         m_table[m_nCur + 1] = new Ref(m_table[m_nCur], value, m_refq);

         if (m_table != WeakHashTab.this.m_table)
         {
            put(m_table[m_nCur], value);
         }
      }
   }

   // The value iterator.

   protected class WeakHashTabValueIterator extends WeakHashTabIterator
   {
      /**
       * @see java.util.Iterator#next()
       * May return null.
       */
      public Object next()
      {
         if (m_nNext < m_table.length)
         {
            incr();

            return ((Ref)m_table[m_nCur + 1]).get();
         }

         throw new java.util.NoSuchElementException();
      }
   }
}
