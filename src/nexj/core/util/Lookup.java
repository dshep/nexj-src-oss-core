// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Simple lookup table (map) interface.
 */
public interface Lookup
{
   /**
    * Inserts a key-value pair into the lookup table.
    * @param key The key. Cannot be null.
    * @param value The value.
    * @return The previous value corresponding to this key (or null if none). 
    */
   Object put(Object key, Object value);
   
   /**
    * Gets a value by key.
    * @param key The key. Cannot be null.
    * @return The value corresponding to this key (or null if none).
    */
   Object get(Object key);

   /**
    * @return true if the key is present in the table. 
    */
   boolean contains(Object key);
   
   /**
    * Removes a key-value pair from the table.
    * @param key The key.
    * @return The removed value (or null if none).
    */
   Object remove(Object key);
   
   /**
    * Removes all the key-value pairs from the table.
    */
   void clear();
   
   /**
    * @return The current key-value pair count.
    */
   int size();
   
   /**
    * @return A new Lookup.Iterator instance for this lookup object.
    */
   Iterator iterator();
   
   /**
    * @return A new Lookup.Iterator instance for this lookup object.
    * This iterator returns the values instead of keys from its next() method.
    */
   Iterator valueIterator();

   /**
    * @return A shallow copy of the container.
    */
   Object clone();
   
   /**
    * A lookup object iterator - can return the current key and value.
    */
   interface Iterator extends java.util.Iterator
   {
      /**
       * @return The key retrieved by the last next() invocation.
       */
      Object getKey();

      /**
       * @return The value associated with the last next() invocation.
       */
      Object getValue();
      
      /**
       * Replaces the value associated with the last next() invocation.
       * @param value The value to set.
       */
      void setValue(Object value);
   }
}
