// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Iterator;

import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 *  Exception that contains integration exceptions.
 */
public class CompoundIntegrationException extends IntegrationException implements ExceptionHolder
{
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 7400817349521484447L;

   /**
    * Nested exception map.
    */
   private Lookup m_exceptionMap = null;
   
   public CompoundIntegrationException(String sErrCode)
   {
      super(sErrCode);
   }

   public CompoundIntegrationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public CompoundIntegrationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public CompoundIntegrationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   // operations
   
   /**
    * @see nexj.core.util.ExceptionHolder#addException(java.lang.Throwable)
    */
   public void addException(Throwable e)
   {
      addException(e, Boolean.TRUE);
   }

   /**
    * Adds a new contained exception to the holder.
    * @param e The exception to add.
    * @param obj an object to be associated with the exception
    */
   public void addException(Throwable e, Serializable obj)
   {
      if (m_exceptionMap == null)
      {
         m_exceptionMap = new HashTab();
      }
      
      m_exceptionMap.put(e, obj);
      
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
      if (m_exceptionMap == null)
      {
         return 0;
      }
      
      return m_exceptionMap.size();
   }

   /**
    * @see nexj.core.util.ExceptionHolder#getExceptionIterator()
    */
   public Iterator getExceptionIterator()
   {
      if (m_exceptionMap == null)
      {
         return EmptyIterator.getInstance();
      }
      
      return m_exceptionMap.iterator();
   }
   
   /**
    * @param e The exception
    * @return an object associated with the given exception
    */
   public Object getValue(Throwable e)
   {
      return (m_exceptionMap == null) ? null : m_exceptionMap.get(e);
   }
   
   /**
    * @see java.lang.Throwable#printStackTrace(PrintStream)
    */
   public void printStackTrace(PrintStream ps)
   {
      ps.println(toString());
      
      if (m_exceptionMap != null)
      {
         ps.print("Caused  by ");
         ps.print(m_exceptionMap.size());
         ps.println(" nested exception(s):");
         
         for (Lookup.Iterator itr = m_exceptionMap.iterator(); itr.hasNext(); )
         {
            ps.print("Exception ");
            ((Throwable)itr.next()).printStackTrace(ps);
            ps.println();
            ps.print("for ");
            ps.println(itr.getValue());
         }
      }
   }
   
   /**
    * @see java.lang.Throwable#printStackTrace(PrintWriter)
    */
   public void printStackTrace(PrintWriter pw)
   {
      pw.println(toString());
      
      if (m_exceptionMap != null)
      {
         pw.print("Caused  by ");
         pw.print(m_exceptionMap.size());
         pw.println(" nested exception(s):");
         
         for (Lookup.Iterator itr = m_exceptionMap.iterator(); itr.hasNext(); )
         {
            pw.print("Exception ");
            ((Throwable)itr.next()).printStackTrace(pw);
            pw.println();
            pw.print("for ");
            pw.println(itr.getValue());
         }
      }
   }
}
