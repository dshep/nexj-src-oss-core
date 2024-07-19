// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic;

import javax.naming.InitialContext;

import nexj.core.runtime.AuthenticationCache;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.SysUtil;

/**
 * Generic authentication cache. Implements cache flushing when running Teee.
 */
public class GenericAuthenticationCache implements AuthenticationCache
{
   // constants

   /**
    * The ENC name of the authentication cache to flush.
    */
   private final static String JNDI_NAME_CONTAINER = J2EEUtil.JNDI_ENV_PREFIX + 
      SysUtil.NAMESPACE + "/" + J2EEUtil.TEEE_NAMESPACE + "/Container";

   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(GenericAuthenticationCache.class);

   // operations

   /**
    * @see nexj.core.runtime.AuthenticationCache#flushPrincipal(java.lang.String)
    */
   public void flushPrincipal(String sName)
   {
      if (J2EEUtil.CONTAINER == J2EEUtil.TEEE)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Flushing authentication cache for " + sName);
         }

         try
         {
            InitialContext ctx = new InitialContext();
            AuthenticationCache cache = (AuthenticationCache)ctx.lookup(JNDI_NAME_CONTAINER);

            cache.flushPrincipal(sName);
         }
         catch (Exception ex)
         {
            s_logger.warn("Cannot flush authentication cache", ex);
         }
      }
   }
}
