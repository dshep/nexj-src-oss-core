// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.Iterator;

/**
 * Hash set using weak references to store the elements of the set. This class
 * subclasses HashHolder, wrapping the objects being stored in weak references
 * and unwrapping them when they are retrieved.
 */
public class WeakHashHolder extends HashHolder
{
   // constants

   /**
    * The serialization version.
    */
   private final static long serialVersionUID = 5589247031286027889L;


   // associations

   /**
    * The reference queue. Stale references enqueued by garbage collector.
    */
   protected transient ReferenceQueue m_refq = new ReferenceQueue();


   // constructors

   /**
    * Creates a hash holder with an estimated item count of 8.
    */
   public WeakHashHolder()
   {
      super();
   }


   /**
    * Creates a hash holder with the given estimated item count.
    * 
    * @param nCount The estimated item count.
    */
   public WeakHashHolder(int nCount)
   {
      super(nCount);
   }


   // operations

   /**
    * Removes the stale references from the table.
    */
   protected void removeStaleReferences()
   {
      Ref ref;

      while ((ref = (Ref)m_refq.poll()) != null)
      {
         int nMask = m_table.length - 1;
         int i = ref.hashCode() & nMask;

         for (;;)
         {
            Object obj = m_table[i];

            if (obj == ref)
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

               break;
            }

            if (obj == null)
            {
               break;
            }

            i = (i + 1) & nMask;
         }
      }
   }

   /**
    * @see nexj.core.util.HashHolder#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      removeStaleReferences();

      return super.add(new Ref(value, m_refq));
   }

   /**
    * @see nexj.core.util.HashHolder#contains(java.lang.Object)
    */
   public boolean contains(Object value)
   {
      removeStaleReferences();

      return super.contains(value);
   }

   /**
    * @see nexj.core.util.HashHolder#get(java.lang.Object)
    */
   public Object get(Object value)
   {
      removeStaleReferences();

      Ref ref = (Ref)super.get(value);

      if (ref != null)
      {
         return ref.get();
      }

      return null;
   }

   /**
    * @see nexj.core.util.HashHolder#remove(java.lang.Object)
    */
   public boolean remove(Object value)
   {
      removeStaleReferences();

      return super.remove(value);
   }

   /**
    * @see nexj.core.util.GenericHashHolder#iterator()
    */
   public Iterator iterator()
   {
      removeStaleReferences();

      return new WeakHashHolderIterator();
   }

   /**
    * @see nexj.core.util.GenericHashHolder#size()
    */
   public int size()
   {
      removeStaleReferences();

      return super.size();
   }

   /**
    * @see nexj.core.util.GenericHashHolder#serialize(java.io.ObjectOutputStream)
    */
   protected void serialize(ObjectOutputStream out) throws IOException
   {
      // Reference objects strongly to prevent GC during serialization
      Object[] table = new Object[m_table.length];

      for (int i = 0; i < table.length; i++)
      {
         Object obj = m_table[i];

         if (obj != null && obj != EMPTY)
         {
            table[i] = ((Ref)obj).get();
         }
      }

      removeStaleReferences();

      // Output all the objects in the table
      int nCount = m_nCount;

      out.defaultWriteObject();
      out.writeInt(nCount);

      for (int i = 0; nCount != 0; ++i)
      {
         Object value = m_table[i];

         if (value != null && value != EMPTY)
         {
            Ref ref = (Ref)value;
            Object referent = ref.get();

            if (referent != null)
            {
               out.writeObject(referent);
               --nCount;
            }
         }
      }
   }

   /**
    * @see nexj.core.util.GenericHashHolder#deserialize(java.io.ObjectInputStream)
    */
   protected void deserialize(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      m_refq = new ReferenceQueue();

      super.deserialize(in);
   }


   // inner classes

   /**
    * The weak reference used in the weak hash holder. All objects put in the
    * hash table are first wrapped in an instance of this class. This class
    * defines equals and hashCode methods that forward to the wrapped object
    * so that the hash table code can be used without modification.
    */
   protected static class Ref extends WeakReference implements Printable
   {
      // attributes

      /**
       * The hash code of the referent.
       */
      protected int m_nHashCode;

      // constructors

      /**
       * Constructs the weak reference.
       * @param referent The referred object.
       * @param q The reference queue.
       */
      public Ref(Object referent, ReferenceQueue q)
      {
         super(referent, q);

         if (referent != null)
         {
            m_nHashCode = referent.hashCode();
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

   /**
    * The weak hash holder iterator.
    */
   protected class WeakHashHolderIterator extends GenericHashHolderIterator
   {
      /**
       * May return null.
       * @see nexj.core.util.GenericHashHolder.GenericHashHolderIterator#next()
       */
      public Object next()
      {
         Ref ref = (Ref)super.next();

         return ref.get();
      }
   }
}
