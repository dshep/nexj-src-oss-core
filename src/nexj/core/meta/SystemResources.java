// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.net.URL;

/**
 * Builds a MetadataFactory.
 */
public class SystemResources
{  
   // constants

   /**
    * System resource path.
    */
   public final static String SYSTEM_METADATA_PATH = "sys/";
   
   /**
    * Loads a system resource.
    * @param sResourceName The name of the resource.
    * @return URL The resource URL, null if the resource is not found.
    */
   public static URL find(String sResourceName)
   {
      return SystemResources.class.getResource(SYSTEM_METADATA_PATH + sResourceName);
   }
}
