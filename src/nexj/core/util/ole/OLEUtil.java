// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.ole;

import nexj.core.util.Logger;
import nexj.core.util.OS;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * OLE utilities.
 */
public class OLEUtil
{
   // attributes

   /**
    * Flag indicating the availability of the OLE helper library.
    */
   private static boolean s_bAvailable;
   
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(OLEUtil.class);

   static
   {
      try
      {
         if (OS.isWindows() &&
            StringUtil.parseBoolean(SysUtil.getConfigProperties().getProperty("ole.enabled", "true")))
         {
            System.loadLibrary("nexjole");
            s_bAvailable = true;
         }
      }
      catch (UnsatisfiedLinkError e)
      {
         s_logger.debug("OLE helper library not found", e); 
      }
      catch (Exception e)
      {
         s_logger.debug("Error loading the OLE helper library", e);
      }
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected OLEUtil()
   {
   }

   // operations
   
   /**
    * @return True if OLE is available, false otherwise.
    */
   public static boolean isAvailable()
   {
      return s_bAvailable;
   }
}
