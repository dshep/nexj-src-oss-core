// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.TimeZone;

import nexj.core.util.log.LoggerFactory;

/**
 * Generic logger object.
 * The following configuration properties affect the logger initialization:
 * logger.factory=[Logger factory class name (nexj.core.util.log.log4j.Log4JLoggerFactory)]
 */
public abstract class Logger implements LoggerHolder
{
   // constants

   /**
    * FATAL (highest priority) logging level.
    * @see Logger#fatal(Object)
    */
   public final static int FATAL = 0;

   /**
    * ERROR logging level.
    * @see Logger#error(Object)
    */
   public final static int ERROR = 1;

   /**
    * WARN logging level.
    * @see Logger#warn(Object)
    */
   public final static int WARN = 2;

   /**
    * INFO logging level.
    * @see Logger#info(Object)
    */
   public final static int INFO = 3;

   /**
    * DEBUG logging level.
    * @see Logger#debug(Object)
    */
   public final static int DEBUG = 4;

   /**
    * DUMP (lowest priority) logging level.
    * @see Logger#dump(Object)
    */
   public final static int DUMP = 5;

   /**
    * The logger factory property name.
    */
   protected final static String LOGGER_FACTORY_PROPERTY = "logger.factory";

   /**
    * Whether debug and dump logging is enabled for each thread. Boolean.FALSE for disabling and null for
    * enabling.
    */
   protected final static ThreadLocal s_threadEnabled = new ThreadLocal();

   /**
    * The default logger factory class name.
    */
   protected final static String DEFAULT_LOGGER_FACTORY_CLASS;

   static
   {
      String sLoggerFactory;

      try
      {
         // sLoggerFactory =  SysConfig.getDefaultLoggerFactory();
         sLoggerFactory = (String)Class.forName("nexj.core.util.LoggerConfig").getMethod("getDefaultLoggerFactory", null).invoke(null, null);
      }
      catch (ClassNotFoundException e)
      {
         // Default factory
         sLoggerFactory = "nexj.core.util.log.j2se.J2SELoggerFactory";
      }
      catch (Exception e)
      {
         sLoggerFactory = null;
         ObjUtil.rethrow(e);
      }

      DEFAULT_LOGGER_FACTORY_CLASS = sLoggerFactory;
   }

   /**
    * The factory object for instantiating a particular logger implementation.
    */
   protected final static LoggerFactory s_factory;

   static
   {
      try
      {
         s_factory = (LoggerFactory)Class.forName(
            SysUtil.getConfigProperties().getProperty(
               LOGGER_FACTORY_PROPERTY,
               DEFAULT_LOGGER_FACTORY_CLASS)).newInstance();

         Logger logger = getLogger(SysUtil.class);

         if (logger.isInfoEnabled())
         {
            String sCfgFile = SysUtil.getConfigFileName();

            if (sCfgFile == null)
            {
               logger.info("Using system configuration properties");
            }
            else
            {
               logger.info("Using configuration file \"" + sCfgFile + "\"");
            }
         }
      }
      catch (Exception e)
      {
         throw new UncheckedException("err.logger.initFailed", null, e);
      }
   }

   // operations

   /**
    * Returns a logger for a particular class. Same as Logger.getLogger(class.getName()).
    * Typical usage:
    * class MyClass
    * {
    *    private final static Logger s_logger = Logger.getLogger(MyClass.class);
    *    ...
    * }
    *
    * @param clazz The class object for which to return a logger.
    * @return Logger instance.
    */
   public static Logger getLogger(Class clazz)
   {
      return getLogger(clazz.getName());
   }

   /**
    * Returns a logger for a particular named subject.
    * Typically, Logger.getLogger(Class) is used instead.
    * @param sName The subject for which to return a logger.
    * @return Logger instance.
    */
   public static Logger getLogger(String sName)
   {
      return s_factory.createLogger(sName);
   }

   /**
    * Gets a system logger for a particular class.
    * Typically, Logger.getSystemLogger(Class) is used instead.
    * @param clazz The class object for which to return a logger.
    * @return Logger instance.
    */
   public static Logger getSystemLogger(Class clazz)
   {
      return getSystemLogger(clazz.getName());
   }

   /**
    * Gets a system logger for a particular named subject.
    * @param sName The subject for which to return a logger.
    * @return Logger instance.
    */
   public static Logger getSystemLogger(String sName)
   {
      int i = sName.indexOf('.');

      return getLogger(sName.substring(0, i + 1) + "system." + sName.substring(i + 1));
   }

   /**
    * Set whether debug and dump logging is enabled for the current thread.
    * @param bEnabled Whether debug and dump logging is enabled for the current thread.
    */
   public static void setEnabled(boolean bEnabled)
   {
      s_threadEnabled.set((bEnabled) ? null : Boolean.FALSE);
   }

   /**
    * Get whether debug and dump logging is enabled for the current thread.
    * @return Whether debug and dump logging is enabled for the current thread.
    */
   public static boolean isEnabled()
   {
      return s_threadEnabled.get() == null;
   }

   /**
    * Enable debug and dump logging for the current thread if the logger for the category has dump enabled.
    * @param sCategory Logger category. Can be null to disable logging.
    */
   public static void enable(String sCategory)
   {
      if (sCategory == null)
      {
         setEnabled(false);
      }
      else
      {
         getLogger(sCategory).enable();
      }
   }

   /**
    * Enable debug and dump logging for the current thread if dump logging is enabled.
    * @param sCategory Logger category.
    */
   public void enable()
   {
      setEnabled(isDumpEnabled());
   }

   /**
    * Pushes a thread diagnostic context token.
    * The logger outputs the entire context stack when configured to do so.
    * This is useful for matching interleaved log messages resulting from
    * multiple parallel activities.
    * @param sToken The token to push and print in the log messages.
    * @return A cookie for resetting the context to its previous level.
    */
   public static int pushContext(String sToken)
   {
      return s_factory.pushContext(sToken);
   }

   /**
    * Resets the thread diagnostic context to a previous level.
    * @param nCookie A cookie returned previously by pushContext.
    */
   public static void resetContext(int nCookie)
   {
      s_factory.resetContext(nCookie);
   }

   /**
    * Application-customized logging extension.
    * @param nLevel The logging level corresponding to the message.
    * @param sCode The string identifier of the message.
    * @param args The message arguments.
    * @param e The exception to log.
    */
   public void log(int nLevel, String sCode, Object[] args, Throwable e)
   {
      if (isLevelEnabled(nLevel))
      {
         StringTable table = StringTable.getInstance();
         String sMsg;

         if (table == null)
         {
            StringBuilder buf = new StringBuilder(128);

            buf.append(sCode);

            if (args != null && args.length != 0)
            {
               buf.append('(');

               for (int i = 0; i < args.length; ++i)
               {
                  if (i != 0)
                  {
                     buf.append(", ");
                  }

                  buf.append(args[i]);
               }

               buf.append(')');
            }

            sMsg = buf.toString();
         }
         else
         {
            TimeZone tz = StringTable.getTimeZone();

            if (tz == null)
            {
               tz = TZ.UTC;
            }

            sMsg = table.format(sCode, args, tz);
         }

         log(nLevel, sMsg, e);
      }
   }

   /**
    * Generic logging method.
    * @param nLevel The logging level corresponding to the message.
    * @param message The message to log.
    */
   public final void log(int nLevel, Object message)
   {
      if (isLevelEnabled(nLevel))
      {
         platformLog(nLevel, message);
      }
   }

   /**
    * Generic logging method.
    * @param nLevel The logging level corresponding to the message.
    * @param message The message to log.
    * @param e The exception to log.
    */
   public final void log(int nLevel, Object message, Throwable e)
   {
      if (isLevelEnabled(nLevel))
      {
         if (e == null)
         {
            platformLog(nLevel, message);
         }
         else
         {
            platformLog(nLevel, message, e);
         }
      }
   }

   /**
    * Checks if a particular logging level is enabled.
    * It is used to skip unnecessary computations.
    * @param nLevel The level to check - one of the Logger level constants.
    * @return True if the logging level is enabled.
    */
   public final boolean isLevelEnabled(int nLevel)
   {
      return isPlatformLevelEnabled(nLevel) && (isEnabled() || nLevel <= INFO);
   }

   /**
    * Logs a fatal error message - the application cannot execute any request,
    * e.g. data source cannot be found, message queue is unavailable,
    * memory overflow. A sysadmin is usually paged to address
    * the problem immediately.
    * @param message The message to log.
    */
   public final void fatal(Object message)
   {
      log(FATAL, message);
   }

   /**
    * Logs a fatal error message.
    * @param message The log message.
    * @param e The exception to log.
    */
   public final void fatal(Object message, Throwable e)
   {
      log(FATAL, message, e);
   }

   /**
    * Logs an error message - the application cannot continue
    * executing a small class of requests, e.g. an incorrect data
    * has been passed from another layer, service configuration problems.
    * User errors and optimistic locking errors are reported
    * at the Debug level instead.
    * @param message The message to log.
    */
   public final void error(Object message)
   {
      log(ERROR, message);
   }

   /**
    * Logs an error message.
    * @param message The log message.
    * @param e The exception to log.
    */
   public final void error(Object message, Throwable e)
   {
      log(ERROR, message, e);
   }

   /**
    * Logs a warning message - potential application problem,
    * e.g. a query executes longer than a predefined threshold,
    * hacker alert, maximum resource pool size reached.
    * The production systems are configured to output this level
    * and the higher-priority levels.
    * @param message The message to log.
    */
   public final void warn(Object message)
   {
      log(WARN, message);
   }

   /**
    * Logs a warning message.
    * @param message The log message.
    * @param e The exception to log.
    */
   public final void warn(Object message, Throwable e)
   {
      log(WARN, message, e);
   }

   /**
    * Must be called always if the log message is computed.
    * @return True if WARN logging level is enabled.
    */
   public final boolean isWarnEnabled()
   {
      return isLevelEnabled(WARN);
   }

   /**
    * Logs an informational message - information about application
    * progress, e.g. configuration values, service startup/shutdown.
    * The amount of the logged information is not proportional to
    * the number of handled requests. If it is proportional,
    * use Debug level instead.
    * @param message The message to log.
    */
   public final void info(Object message)
   {
      log(INFO, message);
   }

   /**
    * Logs an informational message.
    * @param message The log message.
    * @param e The exception to log.
    */
   public final void info(Object message, Throwable e)
   {
      log(INFO, message, e);
   }

   /**
    * Must be called always if the log message is computed.
    * @return True if INFO logging level is enabled.
    */
   public final boolean isInfoEnabled()
   {
      return isLevelEnabled(INFO);
   }

   /**
    * Logs a debugging message - detailed debugging information
    * that can be used for identifying problems without a debugger,
    * e.g. entry/exit points, user errors, recoverable errors,
    * algorithm steps. The logged messages usually do not exceed
    * 255 characters. The application performance is significantly
    * reduced when this logging level is enabled. Not for production use.
    * @param message The message to log.
    */
   public final void debug(Object message)
   {
      log(DEBUG, message);
   }

   /**
    * Logs a debugging message.
    * @param message The log message.
    * @param e The exception to log.
    */
   public final void debug(Object message, Throwable e)
   {
      log(DEBUG, message, e);
   }

   /**
    * Must be called always if the log message is computed.
    * @return True if DEBUG logging level is enabled.
    */
   public final boolean isDebugEnabled()
   {
      return isLevelEnabled(DEBUG);
   }

   /**
    * Logs an object dump message - dump of object state for
    * very detailed debugging. The log files will grow rapidly.
    * @param message The message to log.
    */
   public final void dump(Object message)
   {
      log(DUMP, message);
   }

   /**
    * Logs an object dump message.
    * @param message The log message.
    * @param e The exception to log.
    */
   public final void dump(Object message, Throwable e)
   {
      log(DUMP, message, e);
   }

   /**
    * Must be called always if the log message is computed.
    * @return True if DUMP logging level is enabled.
    */
   public final boolean isDumpEnabled()
   {
      return isLevelEnabled(DUMP);
   }

   /**
    * @see nexj.core.util.LoggerHolder#getLogger()
    */
   public Logger getLogger()
   {
      return this;
   }

   /**
    * Generic logging template method.
    * @param nLevel The logging level corresponding to the message.
    * @param message The message to log.
    */
   protected abstract void platformLog(int nLevel, Object message);

   /**
    * Generic logging template method.
    * @param nLevel The logging level corresponding to the message.
    * @param message The message to log.
    * @param e The exception to log.
    */
   protected abstract void platformLog(int nLevel, Object message, Throwable e);

   /**
    * Template method to check if a particular logging level is enabled.
    * It is used to skip unnecessary computations.
    * @param nLevel The level to check - one of the Logger level constants.
    * @return True if the logging level is enabled.
    */
   protected abstract boolean isPlatformLevelEnabled(int nLevel);
}
