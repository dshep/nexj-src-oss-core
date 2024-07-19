// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Helper class to iterate over a single Object.
 */
public class SingletonIterator implements Iterator
{
   // constants

   /**
    * The end-of-iteration token.
    */
   protected final static Object END = new Object();

   // associations

   /**
    * The entry to iterate over.
    */
   protected Object m_obj;

   // constructors

   /**
    * The constructor.
    * @param obj The object to iterate over.
    */
   public SingletonIterator(Object obj)
   {
      m_obj = obj;
   }

   // operations

   /**
    * @see java.util.Iterator#remove()
    */
   public void remove()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.util.Iterator#next()
    */
   public Object next()
   {
      if (m_obj == END)
      {
         throw new NoSuchElementException();
      }

      try
      {
         return m_obj;
      }
      finally
      {
         m_obj = END;
      }
   }

   /**
    * @see java.util.Iterator#hasNext()
    */
   public boolean hasNext()
   {
      return m_obj != END;
   }
}
