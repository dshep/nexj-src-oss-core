// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 */
public class LookupHashMap extends HashMap implements Lookup
{

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 2794007691496288943L;

   /**
    * @see nexj.core.util.Lookup#contains(java.lang.Object)
    */
   public boolean contains(Object key)
   {
      return containsKey(key);
   }

   /**
    * @see nexj.core.util.Lookup#iterator()
    */
   public Iterator iterator()
   {
      return new LookupHashMapIterator();
   }

   /**
    * @see nexj.core.util.Lookup#valueIterator()
    */
   public Iterator valueIterator()
   {
      return new LookupHashMapValueIterator();
   }

   /**
    * LookupHashMap iterator - can return the current key and the value.
    */
   protected class LookupHashMapIterator implements Iterator
   {
      
      protected java.util.Iterator m_itr;
      protected Map.Entry m_entry;

      /**
       * Creates an iterator.
       */
      protected LookupHashMapIterator()
      {
         m_itr = entrySet().iterator();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getKey()
       */
      public Object getKey()
      {
         return m_entry.getKey();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getValue()
       */
      public Object getValue()
      {
         return m_entry.getValue();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         m_entry.setValue(value);
      }

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_itr.hasNext();
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         m_entry = (Map.Entry)m_itr.next();
         
         return m_entry.getKey();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         m_itr.remove();
      }
   }
   
   /**
    * The same as the LookupHashMapIterator, but next() returns the value.
    */
   protected class LookupHashMapValueIterator extends LookupHashMapIterator
   {
      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         super.next();
         
         return m_entry.getValue();
      }
   }
}
