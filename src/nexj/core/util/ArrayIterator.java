// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An Iterator that iterates an array.
 */
public class ArrayIterator implements Iterator
{
   // attributes

   /**
    * The number of elements in the array.
    */
   protected int m_nCount;

   /**
    * The current index into the array.
    */
   protected int m_nIndex = 0;

   // associations

   /**
    * The array to iterate.
    */
   protected Object m_array;

   // constructors

   /**
    * Constructs an iterator that iterates the given array.
    * 
    * @param array The array to iterate.
    * @throws IllegalArgumentException If the parameter is not an array.
    */
   public ArrayIterator(Object array)
   {
      m_nCount = Array.getLength(array);
      m_array = array;
   }

   // operations

   /**
    * @see java.util.Iterator#hasNext()
    */
   public boolean hasNext()
   {
      return m_nIndex != m_nCount;
   }

   /**
    * @see java.util.Iterator#next()
    */
   public Object next()
   {
      if (m_nIndex == m_nCount)
      {
         throw new NoSuchElementException();
      }

      return Array.get(m_array, m_nIndex++);
   }

   /**
    * @see java.util.Iterator#remove()
    */
   public void remove()
   {
      throw new UnsupportedOperationException();
   }
}
