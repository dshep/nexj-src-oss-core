// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.jboss;

import java.security.Principal;

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import org.jboss.security.SimplePrincipal;

import nexj.core.runtime.platform.PlatformAuthenticationCache;
import nexj.core.util.Logger;

/**
 * JBoss-specific authentication cache.
 */
public class JBossAuthenticationCache extends PlatformAuthenticationCache
{
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(JBossAuthenticationCache.class);

   /**
    * @see nexj.core.runtime.AuthenticationCache#flushPrincipal(java.lang.String)
    */
   public void flushPrincipal(String sName)
   {
      try
      {
         Principal principal = new SimplePrincipal(sName);
         ObjectName jaasMgr = new ObjectName("jboss.security:service=JaasSecurityManager");
         Object[] args = {m_sDomain, principal};
         String[] types = {"java.lang.String", "java.security.Principal"};
         MBeanServer mbsvr = (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
         mbsvr.invoke(jaasMgr, "flushAuthenticationCache", args, types);
      }
      catch (Exception e)
      {
         s_logger.error("Error flushing the authentication cache for principal\"" + sName + "\"", e);
      }
   }
}
