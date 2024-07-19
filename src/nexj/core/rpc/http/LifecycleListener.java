// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import nexj.core.runtime.LifecycleManager;
import nexj.core.util.ObjUtil;

/**
 * Application lifecycle listener.
 */
public class LifecycleListener implements ServletContextListener
{
   /**
    * @see javax.servlet.ServletContextListener#contextInitialized(javax.servlet.ServletContextEvent)
    */
   public void contextInitialized(ServletContextEvent event)
   {
      try
      {
         LifecycleManager.getLifecycle().startup();
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
   }

   /**
    * @see javax.servlet.ServletContextListener#contextDestroyed(javax.servlet.ServletContextEvent)
    */
   public void contextDestroyed(ServletContextEvent event)
   {
      LifecycleManager.getLifecycle().shutdown();
   }
}
