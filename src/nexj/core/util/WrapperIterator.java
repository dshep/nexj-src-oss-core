// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
/**
 * 
 */
package nexj.core.util;

import java.util.Iterator;

/**
 * Wraps an iterator to simplify implementing the decorator pattern.
 */
public abstract class WrapperIterator implements Iterator
{
   // associations

   /**
    * The wrapped iterator.
    */
   protected Iterator m_itr;

   // constructors

   /**
    * Constructs the wrapper iterator.
    * @param itr The wrapped iterator.
    */
   protected WrapperIterator(Iterator itr)
   {
      m_itr = itr;
   }

   // operations

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
      return m_itr.next();
   }

   /**
    * @see java.util.Iterator#remove()
    */
   public void remove()
   {
      m_itr.remove();
   }
}
