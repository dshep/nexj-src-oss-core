// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Provides empty iterator instances.
 */
public class EmptyIterator implements Iterator
{
   // constants
   
   private final static Iterator s_instance = new EmptyIterator();
   
   // constructors
   
   /**
    * Cannot create instances explicitly.
    */
   private EmptyIterator()
   {
   }

   // operations
   
   /**
    * @returns The empty iterator instance.
    */
   public static Iterator getInstance()
   {
      return s_instance;
   }

   /**
    * @see java.util.Iterator#hasNext()
    */
   public boolean hasNext()
   {
      return false;
   }

   /**
    * @see java.util.Iterator#next()
    */
   public Object next()
   {
      throw new NoSuchElementException("Cannot iterate with an EmptyIterator");
   }

   /**
    * @see java.util.Iterator#remove()
    */
   public void remove()
   {
      throw new IllegalStateException("Cannot remove from an EmptyIterator");
   }
}
