// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;

/**
 * Exception that contains metadata validation exceptions.
 */
public class MetadataCompoundValidationException
   extends MetadataException
   implements ExceptionHolder
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 6431761821672644637L;
   
   // associations

   /**
    * Nested exception collection.
    */
   private List m_exceptionList = null;
   
   // constructors

   public MetadataCompoundValidationException()
   {
      super("err.meta.validation");
   }

   // operations

   /**
    * Adds the exceptions from the specified exception holder.
    * @param eh The exception holder.
    */
   public void addExceptions(ExceptionHolder eh)
   {
      for (Iterator itr = eh.getExceptionIterator(); itr.hasNext();)
      {
         addException((Throwable)itr.next());
      }
   }

   /**
    * @see nexj.core.util.ExceptionHolder#addException(java.lang.Throwable)
    */
   public void addException(Throwable e)
   {
      if (m_exceptionList == null)
      {
         m_exceptionList = new ArrayList();
      }
      
      m_exceptionList.add(e);
      
      if (getCause() == null)
      {
         initCause(e);
      }
   }

   /**
    * @see nexj.core.util.ExceptionHolder#getExceptionCount()
    */
   public int getExceptionCount()
   {
      if (m_exceptionList == null)
      {
         return 0;
      }
      
      return m_exceptionList.size();
   }

   /**
    * @see nexj.core.util.ExceptionHolder#getExceptionIterator()
    */
   public Iterator getExceptionIterator()
   {
      if (m_exceptionList == null)
      {
         return EmptyIterator.getInstance();
      }
      
      return m_exceptionList.iterator();
   }
}
