// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 * Like a lookup but multiple values can be stored for each key (in a List).
 */
public class MultiMap implements Cloneable, Serializable, Printable
{
   private static final long serialVersionUID = -4726285766048990225L;

   // associations

   /**
    * Invariant: If there is a key then the value collection is non-empty.
    */
   private final Lookup m_lookup;

   // constructors

   /**
    * Create a multimap backed by a {@link HashTab}.
    */
   public MultiMap()
   {
      this(new HashTab());
   }

   /**
    * Create a multimap backed by the given lookup.
    * 
    * @param backing A lookup to use, either empty or with all values as Lists
    */
   public MultiMap(Lookup backing)
   {
      assert backing != null;

      m_lookup = backing;
   }

   // operations

   /**
    * Add a value to the list of values for the given key
    * 
    * @param key The key to use
    * @param value The value to be added to the list of values for the given key
    */
   public boolean add(Object key, Object value)
   {
      assert key != null;

      Collection l = (Collection)m_lookup.get(key);

      if (l == null)
      {
         m_lookup.put(key, l = createCollection());
      }

      return l.add(value);
   }

   /**
    * Remove an item from the list of values for the given key.
    * 
    * @param key The key to use
    * @param value The value to be added to the list of values for the given key
    * @return True if value removed from the list
    */
   public boolean remove(Object key, Object value)
   {
      Collection l = (Collection)m_lookup.get(key);

      if (l == null)
      {
         return false;
      }

      boolean bRemoved = l.remove(value);

      if (l.isEmpty())
      {
         m_lookup.remove(key);
      }

      return bRemoved;
   }

   /**
    * Removes a key-value pair from the table.
    * 
    * @param key The key.
    * @return The removed value (or null if none).
    */
   public Collection remove(Object key)
   {
      return (Collection)m_lookup.remove(key);
   }

   /**
    * The list of all elements for the given key.
    * 
    * @param key The key
    * @return An immutable collection. Iterators on this collection may be
    *         invalidated by operations on the multimap.
    */
   public Collection get(Object key)
   {
      Collection r = (Collection)m_lookup.get(key);

      if (r == null)
      {
         return Collections.EMPTY_LIST;
      }

      return Collections.unmodifiableCollection(r);
   }

   /**
    * @return true if the key is present in the table.
    */
   public boolean contains(Object key)
   {
      return m_lookup.contains(key);
   }

   /**
    * Removes all the key-value pairs from the table.
    */
   public void clear()
   {
      m_lookup.clear();
   }

   /**
    * @return The current value count.
    */
   public int size()
   {
      int nSize = 0;

      for (Lookup.Iterator it = m_lookup.valueIterator(); it.hasNext();)
      {
         nSize += ((Collection)it.next()).size();
      }

      return nSize;
   }

   /**
    * @return The number of keys
    */
   public int keyCount()
   {
      return m_lookup.size();
   }

   /**
    * @return Whether this map is empty
    */
   public boolean isEmpty()
   {
      // There are no empty lists in the lookup
      return m_lookup.size() == 0;
   }

   /**
    * @return An iterator over values.
    */
   public Lookup.Iterator valueIterator()
   {
      return new ValueIterator(m_lookup.valueIterator());
   }

   /**
    * @return An iterator over keys. The value is a non-empty immutable
    *         collection. Setting the value is not supported.
    */
   public Lookup.Iterator iterator()
   {
      return new Iterator(m_lookup.iterator());
   }

   /**
    * @return A shallow copy of the container.
    */
   public Object clone()
   {
      try
      {
         return super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         assert false;
         return null;
      }
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.print(m_lookup);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   /**
    * Template method to construct an inner collection
    * 
    * @return A newly created collection
    */
   protected Collection createCollection()
   {
      return new ArrayList(2);
   }

   /**
    * Template method to get the iterator for a collection
    */
   protected java.util.Iterator getIterator(Collection collection)
   {
      return collection.iterator();
   }

   // inner classes

   private static class Iterator implements Lookup.Iterator
   {
      private final Lookup.Iterator m_outer;

      // constructor

      public Iterator(Lookup.Iterator outer)
      {
         m_outer = outer;
      }

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_outer.hasNext();
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         return m_outer.next();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         m_outer.remove();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getKey()
       */
      public Object getKey()
      {
         return m_outer.getKey();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getValue()
       */
      public Object getValue()
      {
         return Collections.unmodifiableCollection((Collection)m_outer.getValue());
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         throw new UnsupportedOperationException();
      }
   }

   /**
    * An iterator over the values this multi map
    */
   private static class ValueIterator implements Lookup.Iterator
   {
      // associations

      private final Lookup.Iterator m_outer;

      private java.util.Iterator m_inner;

      private Object m_lastValue;

      // constructor

      public ValueIterator(Lookup.Iterator outer)
      {
         m_outer = outer;
      }

      // operations

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return (m_inner != null && m_inner.hasNext()) || m_outer.hasNext();
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_inner == null || !m_inner.hasNext())
         {
            m_inner = ((Collection)m_outer.next()).iterator();
         }

         return (m_lastValue = m_inner.next());
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         m_inner.remove();

         if (((Collection)m_outer.getValue()).isEmpty())
         {
            m_outer.remove();
         }
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getKey()
       */
      public Object getKey()
      {
         return m_outer.getKey();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getValue()
       */
      public Object getValue()
      {
         return m_lastValue;
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         throw new UnsupportedOperationException();
      }
   }
}
