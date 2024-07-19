// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.meta.Metaclass;
import nexj.core.util.Logger;

/**
 * Wrapper for a LOGGER instance, providing the nexj.core.util.Logger interface.
 */
public class InstanceLogger extends Logger
{
   /* Constants */
   
   /**
    * Level symbols
    */
   public static final String LOG_LEVELS[] = 
      {
         "F", // fatal
         "E", // error
         "W", // warn
         "I", // info
         "D", // debug
         "U" // dump
      };
   
   /* Associations */
   
   /**
    * The LOGGER instance.
    */
   protected Instance m_instance;

   /**
    * The associated J2EE logger.
    */
   protected Logger m_logger;
   
   /* Constructors */
   
   /**
    * @param instance The LOGGER instance to wrap.
    */
   public InstanceLogger(Instance instance)
   {
      m_instance = instance;
      
      String sLoggerCategory = (String)instance.getValue("logCategory");
      
      if (sLoggerCategory == null)
      {
         m_logger = instance.getMetaclass().getLogger();
      }
      else
      {
         m_logger = Logger.getLogger(sLoggerCategory);
      }
   }
   
   /* Methods */
   
   /**
    * @param nLevel The logging level to test.
    * @return True if the instance associated with this logs at level nLevel.
    */
   public boolean isInstanceLevelEnabled(int nLevel)
   {
      Instance logLevel = (Instance)m_instance.getValue("logLevel");
      String sLogLevel = (String)logLevel.getValue("value");

      // iterate over levels until we reach nLevel or until we encounter the logLevel threshold.
      for (int i = 0; i < nLevel && i < LOG_LEVELS.length; i++)
      {
         if (sLogLevel.equals(LOG_LEVELS[i]))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see nexj.core.util.Logger#log(int, java.lang.String, java.lang.Object[], java.lang.Throwable)
    */
   public void log(int nLevel, String sCode, Object[] args, Throwable e)
   {
      if (m_logger.isLevelEnabled(nLevel))
      {
         m_logger.log(Math.max(nLevel, Logger.DEBUG), sCode, args, e);
      }

      if (isInstanceLevelEnabled(nLevel))
      {
         InvocationContext context = m_instance.getInvocationContext();
         UnitOfWork uowSaved = (m_instance.getState() == Instance.NEW) ?
            context.getUnitOfWork() : context.setLoggerUnitOfWork();
         boolean bSecureSaved = context.isSecure();
         
         try
         {
            context.setSecure(false);
            
            Object level = ((Metaclass)m_instance.getMetaclass().getAttribute("logLevel").getType())
               .invoke("get", new Object[]{LOG_LEVELS[nLevel]});
            
            m_instance.invoke("log", new Object[] {level, sCode, args, e});
         }
         finally
         {
            context.setSecure(bSecureSaved);
            context.setUnitOfWork(uowSaved);
         }
      }
   }

   /**
    * @see nexj.core.util.Logger#isPlatformLevelEnabled(int)
    */
   protected boolean isPlatformLevelEnabled(int nLevel)
   {
      return m_logger.isLevelEnabled(nLevel) || isInstanceLevelEnabled(nLevel);
   }

   /**
    * @see nexj.core.util.Logger#platformLog(int, java.lang.Object)
    */
   protected void platformLog(int nLevel, Object message)
   {
      platformLog(nLevel, message, null);
   }

   /**
    * @see nexj.core.util.Logger#platformLog(int, java.lang.Object, java.lang.Throwable)
    */
   protected void platformLog(int nLevel, Object message, Throwable e)
   {  
      log(nLevel, "ids.logger.defaultMessage", new Object[]{message}, e);
   }
}
