// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform;

import nexj.core.runtime.AuthenticationCache;
import nexj.core.util.SysUtil;

/**
 * Base class for authentication caches.
 */
public abstract class PlatformAuthenticationCache implements AuthenticationCache
{
   /**
    * The security domain to use.
    */
   protected String m_sDomain = SysUtil.NAMESPACE;

   /**
    * Sets the security domain to use.
    * @param sDomain The security domain to use to set.
    */
   public void setDomain(String sDomain)
   {
      m_sDomain = sDomain;
   }

   /**
    * @return The security domain to use.
    */
   public String getDomain()
   {
      return m_sDomain;
   }
}
