// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log.log4j;

import java.util.Enumeration;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.apache.log4j.NDC;
import org.apache.log4j.PropertyConfigurator;

import nexj.core.util.SysUtil;

/**
 * Logger factory for log4j wrappers.
 */
public class Log4JLoggerFactory implements nexj.core.util.log.LoggerFactory
{
   /**
    * Constructor using the configuration properties from SysUtil.
    */
   public Log4JLoggerFactory()
   {
      this(getLog4JProperties());
   }

   /**
    * Constructor using specified configuration properties for log4j.
    * @param properties Configuration properties for log4j.
    */
   protected Log4JLoggerFactory(Properties properties)
   {
      PropertyConfigurator.configure(properties);
   }
   
   /**
    * @see nexj.core.util.log.LoggerFactory#createLogger(java.lang.String)
    */
   public nexj.core.util.Logger createLogger(String sName)
   {
      return new Log4JLogger(Logger.getLogger(sName));
   }
   
   /**
    * @see nexj.core.util.log.LoggerFactory#pushContext(java.lang.String)
    */
   public int pushContext(String sToken)
   {
      int i = NDC.getDepth();

      NDC.push(sToken);

      return i;
   }

   /**
    * @see nexj.core.util.log.LoggerFactory#resetContext(int)
    */
   public void resetContext(int nCookie)
   {
      NDC.setMaxDepth(nCookie);
   }

   /**
    * @return The log4j configuration properties.
    */
   protected static Properties getLog4JProperties()
   {
      Properties properties = new Properties();

      addLog4JProperties(properties, SysUtil.getConfigProperties());
      
      try
      {
         addLog4JProperties(properties, System.getProperties());
      }
      catch (SecurityException e)
      {
      }

      return properties;
   }
   
   /**
    * Adds the properties starting with log4j.* from the source to the destination property map.
    * @param dst The destination property map. 
    * @param src The source property map.
    */
   protected static void addLog4JProperties(Properties dst, Properties src)
   {
      for (Enumeration enm = src.propertyNames(); enm.hasMoreElements();)
      {
         String sName = (String)enm.nextElement();
         
         if (sName.startsWith("log4j."))
         {
            dst.setProperty(sName, src.getProperty(sName));
         }
      }
   }
}
