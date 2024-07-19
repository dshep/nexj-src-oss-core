// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * This class extends UncheckedException and provides an implementation of ExceptionHolder
 */
public class GenericException extends UncheckedException implements ExceptionHolder
{   
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 7277225080390543630L;
   
   // associations
   
   /**
    * Contained exceptions.
    */
   private List m_exceptionList;
   
   // constructors

   public GenericException(String sErrCode)
   {
      super(sErrCode);
   }

   public GenericException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public GenericException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public GenericException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return false;
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
      return (m_exceptionList == null) ? 0 : m_exceptionList.size();
   }

   /**
    * @see nexj.core.util.ExceptionHolder#getExceptionIterator()
    */
   public Iterator getExceptionIterator()
   {
      return (m_exceptionList == null) ? EmptyIterator.getInstance() : m_exceptionList.iterator();
   }
   
   public void setErrorCode(String sErrCode)
   {
      m_sErrCode = sErrCode;
   }

   public void setErrorArgs(Object[] argArray)
   {
      m_argArray = argArray;
   }
}
