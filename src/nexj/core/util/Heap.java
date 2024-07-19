// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Heap interface.
 */
public interface Heap
{
   /**
    * Inserts a value into the heap.
    * @param value The value to insert. Cannot be null.
    * @return True, so that the implementor could also implement the Collection interface.
    */
   boolean add(Object value);

   /**
    * @return true if the value is present in the heap. 
    */
   boolean contains(Object value);

   /**
    * Removes a value from the heap.
    * @param value The value to remove. Cannot be null.
    */
   boolean remove(Object value);
   
   /**
    * @return The first value, or null if none.
    */
   Object first();

   /**
    * Removes and returns the first value.
    * @return The first value, or null if none.
    */
   Object removeFirst();
   
   /**
    * @return A heap iterator.
    */
   Iterator iterator();
   
   /**
    * Removes all the values from the heap.
    */
   void clear();
   
   /**
    * @return The count of values in the heap.
    */
   int size();
   
   /**
    * @return A shallow copy of the heap.
    */
   Object clone();
}
