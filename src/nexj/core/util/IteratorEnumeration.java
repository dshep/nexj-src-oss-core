// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Enumeration;
import java.util.Iterator;

/**
 * Enumeration adapter for an iterator.
 */
public class IteratorEnumeration implements Enumeration
{
   // associations

   /**
    * The wrapped iterator.
    */
   protected Iterator m_itr;

   // constructors

   /**
    * Constructs the enumeration.
    * @param itr The iterator.
    */
   public IteratorEnumeration(Iterator itr)
   {
      m_itr = itr;
   }

   // operations
   
   /**
    * @see java.util.Enumeration#hasMoreElements()
    */
   public boolean hasMoreElements()
   {
      return m_itr.hasNext();
   }

   /**
    * @see java.util.Enumeration#nextElement()
    */
   public Object nextElement()
   {
      return m_itr.next();
   }
}
