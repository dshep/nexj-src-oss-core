// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.Iterator;
import java.util.NoSuchElementException;

import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;

/**
 * An Iterator that calls next and hasNext Functions to perform the iteration.
 */
public class FunctionIterator implements Iterator
{
   // associations

   /**
    * The VM for function evaluation.
    */
   protected Machine m_machine;

   /**
    * The "next" function.
    */
   protected Function m_nextFunction;

   /**
    * The "hasNext" function.
    */
   protected Function m_hasNextFunction;

   /**
    * The next item in the iteration.
    */
   protected Object m_next;

   // constructors

   /**
    * Creates a new Iterator from "next" and "hasNext" functions.
    * @param nextFunction The "next" function.
    * @param hasNextFunction The "hasNext" function.
    * @param context The invocation context.
    */
   public FunctionIterator(Function nextFunction, Function hasNextFunction, InvocationContext context)
   {
      m_nextFunction = nextFunction;
      m_hasNextFunction = hasNextFunction;
      m_machine = context.getMachine();
      next();
   }

   // operations

   /**
    * @see java.util.Iterator#hasNext()
    */
   public boolean hasNext()
   {
      if (m_hasNextFunction == null)
      {
         m_nextFunction = null;

         return false;
      }

      Boolean hasNext = (Boolean)m_machine.invoke(m_hasNextFunction, (Object[])null);

      if (hasNext == null || hasNext.booleanValue())
      {
         return true;
      }

      m_nextFunction = null;
      m_hasNextFunction = null;

      return false;
   }

   /**
    * @see java.util.Iterator#next()
    */
   public Object next()
   {
      if (m_nextFunction == null)
      {
         throw new NoSuchElementException();
      }

      Object result = m_next;

      m_next = m_machine.invoke(m_nextFunction, (Object[])null);

      return result;
   }

   /**
    * Not supported.
    * @see java.util.Iterator#remove()
    */
   public void remove()
   {
      throw new UnsupportedOperationException();
   }
}