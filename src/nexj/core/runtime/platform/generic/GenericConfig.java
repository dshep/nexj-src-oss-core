// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic;

import java.io.File;

import nexj.core.runtime.platform.PlatformConfig;
import nexj.core.util.J2EEUtil;
import nexj.core.util.SysUtil;

/**
 * Platform configuration values when not running in J2EE contained mode.
 */
public class GenericConfig extends PlatformConfig
{
   /**
    * Creates a new platform configuration instance for non-J2EE-contained mode.
    * The data directory when running uncontained is ${java.io.tmpdir}\nexj
    */
   public GenericConfig()
   {
      assert (J2EEUtil.CONTAINER == J2EEUtil.NONE) || (J2EEUtil.CONTAINER == J2EEUtil.TEEE);

      m_dataDirectory = new File(System.getProperty("java.io.tmpdir"), SysUtil.NAMESPACE);
   }
}
