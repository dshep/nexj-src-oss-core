// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log.j2se;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * J2SE logger wrapper. 
 */
public class J2SELogger extends nexj.core.util.Logger
{
   // constants
   
   /**
    * Maps our log level to the J2SE log level.
    */
   protected final static Level[] s_levelArray =
   {
      FatalLevel.FATAL,
      Level.SEVERE,
      Level.WARNING,
      Level.INFO,
      Level.FINE,
      Level.FINEST
   };
   
   // associations
   
   /**
    * The wrapped logger.
    */
   private Logger m_logger;
   
   // constructors
   
   /**
    * Constructs the wrapper.
    */
   public J2SELogger(Logger logger)
   {
      m_logger = logger;
   }

   // operations
   
   /**
    * @see nexj.core.util.Logger#platformLog(int, java.lang.Object)
    */
   protected void platformLog(int nLevel, Object message)
   {
      Level level = s_levelArray[nLevel];

      if (m_logger.isLoggable(level))
      {
         m_logger.log(level, J2SELoggerFactory.addContext(String.valueOf(message)));
      }
   }

   /**
    * @see nexj.core.util.Logger#platformLog(int, java.lang.Object, java.lang.Throwable)
    */
   protected void platformLog(int nLevel, Object message, Throwable e)
   {
      Level level = s_levelArray[nLevel];

      if (m_logger.isLoggable(level))
      {
         m_logger.log(level, J2SELoggerFactory.addContext(String.valueOf(message)), e);
      }
   }

   /**
    * @see nexj.core.util.Logger#isPlatformLevelEnabled(int)
    */
   protected boolean isPlatformLevelEnabled(int nLevel)
   {
      return m_logger.isLoggable(s_levelArray[nLevel]);
   }
}
