// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log.log4j;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Log4J logger wrapper.
 */
public class Log4JLogger extends nexj.core.util.Logger
{
   // constants
   
   protected final static Level[] s_levelArray =
   {
      Level.FATAL,
      Level.ERROR,
      Level.WARN,
      Level.INFO,
      Level.DEBUG,
      new Level(5000, "DUMP", 7)
      {
         private final static long serialVersionUID = 3487163159235007428L;
      }
   };

   // attributes

   private Logger m_logger;

   // constructors

   protected Log4JLogger(Logger logger)
   {
      m_logger = logger;
   }

   // operations

   /**
    * @see nexj.core.util.Logger#platformLog(int, java.lang.Object)
    */
   protected void platformLog(int nLevel, Object message)
   {
      m_logger.log(s_levelArray[nLevel], message);
   }

   /**
    * @see nexj.core.util.Logger#platformLog(int, java.lang.Object, java.lang.Throwable)
    */
   protected void platformLog(int nLevel, Object message, Throwable e)
   {
      m_logger.log(s_levelArray[nLevel], message, e);
   }

   /**
    * @see nexj.core.util.Logger#isPlatformLevelEnabled(int)
    */
   protected boolean isPlatformLevelEnabled(int nLevel)
   {
      return m_logger.isEnabledFor(s_levelArray[nLevel]);
   }
}
