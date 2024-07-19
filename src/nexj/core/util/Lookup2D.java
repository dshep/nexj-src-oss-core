// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Two-dimensional (2 keys) lookup table interface.
 */
public interface Lookup2D
{
   /**
    * Inserts a key-value triple into the lookup table.
    * @param key1 The first key. Cannot be null.
    * @param key2 The second key. Cannot be null.
    * @param value The value.
    * @return The previous value corresponding to the key pair (or null if none). 
    */
   Object put(Object key1, Object key2, Object value);
   
   /**
    * Gets a value by key pair.
    * @param key1 The first key. Cannot be null.
    * @param key1 The scond key. Cannot be null.
    * @return The value corresponding to the key pair (or null if none).
    */
   Object get(Object key1, Object key2);

   /**
    * @return true if the key pair is present in the table. 
    */
   boolean contains(Object key1, Object key2);
   
   /**
    * Removes a key-value triple from the table.
    * @param key1 The first key.
    * @param key2 The second key.
    * @return The removed value (or null if none).
    */
   Object remove(Object key1, Object key2);
   
   /**
    * Removes all the key-value triples from the table.
    */
   void clear();
   
   /**
    * @return The current key-value pair count.
    */
   int size();

   /**
    * @return A new Lookup2D.Iterator instance for this lookup object.
    * This iterator returns values from its next() method.
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
       * @return The first key retrieved by the last next() invocation.
       */
      Object getKey1();

      /**
       * @return The second key retrieved by the last next() invocation.
       */
      Object getKey2();

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
