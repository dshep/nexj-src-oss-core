// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Deque interface.
 */
public interface Deque
{
   /**
    * Adds a value at the head.
    * @param value The value to add.
    * @return True if added, false if moved.
    */
   boolean addFirst(Object value);

   /**
    * Adds a value at the tail.
    * @param value The value to add.
    * @return True if added, false if moved.
    */
   boolean addLast(Object value);

   /**
    * @return The first value, or null if none.
    */
   Object first();

   /**
    * @return The last value, or null if none.
    */
   Object last();

   /**
    * Removes and returns the first value.
    * @return The first value, or null if none.
    */
   Object removeFirst();
   
   /**
    * Removes and returns the last value.
    * @return The last value, or null if none.
    */
   Object removeLast();
   
   /**
    * @return A queue iterator, from head to tail.
    */
   Iterator iterator();
   
   /**
    * Removes all the values from the queue.
    */
   void clear();
   
   /**
    * @return The count of values in the queue.
    */
   int size();
}
