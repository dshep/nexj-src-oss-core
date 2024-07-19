// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Handles platform dependent logger functionality.
 */
public class LoggerConfig
{
   /**
    * Get the fully qualified class name of a logger factory to use.
    * 
    * Warning: called from static initializer
    * 
    * @return Fully qualified java class of the logger factory to use.
    */
   public static String getDefaultLoggerFactory()
   {
      switch (J2EEUtil.CONTAINER)
      {
         case J2EEUtil.WEBSPHERE:
            return "nexj.core.util.log.j2se.J2SELoggerFactory";

         default:
            return "nexj.core.util.log.log4j.Log4JLoggerFactory";
      }
   }
}
