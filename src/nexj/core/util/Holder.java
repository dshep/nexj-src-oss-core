// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;
import java.util.Set;

/**
 * Simple holder (set) interface.
 */
public interface Holder extends Set
{
   /**
    * Inserts a value into the holder.
    * @param value The value to insert. Cannot be null.
    * @return True if this the value was not already in the holder.
    */
   boolean add(Object value);
   
   /**
    * @return true if the value is present in the holder. 
    */
   boolean contains(Object value);

   /**
    * Removes the value from the holder.
    * @param value The value to remove. Cannot be null.
    * @return True if the value was removed; false if the value was not found.
    */
   boolean remove(Object value);

   /**
    * Gets the object from the holder that is equivalent to value.
    * @param value The value to get.
    * @return The object that is equivalent to value; null if not present.
    */
   Object get(Object value);

   /**
    * Removes all the values from the holder.
    */
   void clear();
   
   /**
    * @return The current value count.
    */
   int size();
   
   /**
    * @return A new Iterator instance for the value in the holder.
    */
   Iterator iterator();
   
   /**
    * @return A shallow copy of the holder.
    */
   Object clone();
}
