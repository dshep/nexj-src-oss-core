// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Combined Lookup/Deque interface.
 */
public interface LookupDeque extends Lookup
{
   /**
    * Inserts a key-value pair at the head.
    * @param key The key. Cannot be null.
    * @param value The value.
    * @return The old value, or null if none.
    */
   Object putFirst(Object key, Object value);

   /**
    * Inserts a key-value pair at the tail.
    * @param key The key. Cannot be null.
    * @param value The value.
    * @return The old value, or null if none.
    */
   Object putLast(Object key, Object value);

   /**
    * @return The first key, or null if none.
    */
   Object firstKey();

   /**
    * @return The last key, or null if none.
    */
   Object lastKey();

   /**
    * @return The first value, or null if none.
    */
   Object firstValue();

   /**
    * @return The last value, or null if none.
    */
   Object lastValue();

   /**
    * Removes and returns the first value.
    * @return The removed value, or null if none.
    */
   Object removeFirst();

   /**
    * Removes and returns the last value.
    * @return The removed value, or null if none.
    */
   Object removeLast();

   /**
    * Gets an iterator for the reverse ordering of this deque.
    * @return A new Lookup.Iterator instance for this lookup object.
    */
   Iterator reverseIterator();

   /**
    * Gets a value iterator for the reverse ordering of this deque.
    * This iterator returns the values instead of keys from its next() method.
    * @return A new Lookup.Iterator instance for this lookup object.
    */
   Iterator reverseValueIterator();
}
