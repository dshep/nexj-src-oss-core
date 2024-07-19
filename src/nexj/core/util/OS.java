// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Locale;

/**
 * Operating system detection class.
 */
public class OS
{
   // constants

   /**
    * Operating system name.
    */
   public final static String NAME = SysUtil.getSystemProperty("os.name", "");

   /**
    * The API name: [win32|linux|mac|...]-[x86|x64|...].
    */
   public final static String API = getAPIName();

   // constructors

   /**
    * Prevents construction.
    */
   protected OS()
   {
   }

   // operations

   /**
    * @return The API name.
    */
   private static String getAPIName()
   {
      String sAPI;

      if (isWindows())
      {
         sAPI = "win32";
      }
      else
      {
         sAPI = NAME.replace("/", "").toLowerCase(Locale.ENGLISH);
      }

      String sArch = SysUtil.getSystemProperty("os.arch", "").toLowerCase(Locale.ENGLISH);

      if (sArch.equals("i386") || sArch.equals("i686"))
      {
         sArch = "x86";
      }
      else if (sArch.equals("x86_64") || sArch.equals("amd64"))
      {
         sArch = "x64";
      }

      if (sArch.length() != 0)
      {
         return sAPI + '-' + sArch;
      }

      return sAPI;
   }

   /**
    * @return True if the OS is Windows.
    */
   public static boolean isWindows()
   {
      return NAME.startsWith("Windows");
   }
}
