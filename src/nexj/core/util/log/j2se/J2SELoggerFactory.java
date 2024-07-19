// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log.j2se;

import java.util.logging.Logger;

import nexj.core.util.log.LoggerFactory;

/**
 * Logger factory for J2SE logging API wrappers.
 */
public class J2SELoggerFactory implements LoggerFactory
{
   // associations
   
   /**
    * The thread invocation context for context logging.
    */
   protected final static ThreadLocal s_threadContext = new ThreadLocal();

   // operations
   
   /**
    * @see nexj.core.util.log.LoggerFactory#createLogger(java.lang.String)
    */
   public nexj.core.util.Logger createLogger(String sName)
   {
      return new J2SELogger(Logger.getLogger(sName));
   }

   /**
    * @see nexj.core.util.log.LoggerFactory#pushContext(java.lang.String)
    */
   public int pushContext(String sToken)
   {
      StringBuffer buf = (StringBuffer)s_threadContext.get();
      
      if (buf == null)
      {
         if (sToken != null && sToken.length() != 0)
         {
            buf = new StringBuffer(64);
            buf.append(sToken);
            s_threadContext.set(buf);
         }
         
         return 0;
      }

      int n = buf.length();

      if (sToken != null && sToken.length() != 0)
      {
         if (n > 0)
         {
            buf.append(' ');
         }
         
         buf.append(sToken);
      }
      
      return n;
   }

   /**
    * @see nexj.core.util.log.LoggerFactory#resetContext(int)
    */
   public void resetContext(int nCookie)
   {
      if (nCookie <= 0)
      {
         s_threadContext.set(null);
      }
      else
      {
         StringBuffer buf = (StringBuffer)s_threadContext.get();
         
         if (nCookie < buf.length())
         {
            buf.setLength(nCookie);
         }
      }
   }
   
   /**
    * Adds a context string to a message.
    * @param sMsg The message to which to add a context string.
    * @return The resulting message.
    */
   public static String addContext(String sMsg)
   {
      if (sMsg == null)
      {
         sMsg = "";
      }
      
      StringBuffer buf = (StringBuffer)s_threadContext.get();

      if (buf == null || buf.length() == 0)
      {
         return sMsg;
      }
      
      StringBuffer msgBuf = new StringBuffer(buf.length() + 3 + sMsg.length());
      
      msgBuf.append('[');
      msgBuf.append(buf);
      msgBuf.append("] ");
      msgBuf.append(sMsg);

      return msgBuf.toString();
   }
}
