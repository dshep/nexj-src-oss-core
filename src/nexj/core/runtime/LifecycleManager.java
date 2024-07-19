// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Repository;
import nexj.core.util.ClassProperty;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.core.version.Version;

/**
 * System component life cycle manager.
 */
public class LifecycleManager implements Lifecycle
{
   // associations

   /**
    * The list of managed components.
    */
   protected List m_componentList = new ArrayList(4);

   /**
    * The system lifecycle singleton.
    */
   protected final static Lifecycle s_lifecycle = new Lifecycle()
   {
      // constants

      /**
       * The application has not yet been started.
       */
      protected final static byte STATE_INITIAL = 0;

      /**
       * The application is running.
       */
      protected final static byte STATE_RUNNING = 1;

      /**
       * The application is suspended.
       */
      protected final static byte STATE_SUSPENDED = 2;

      /**
       * The application has been shut down.
       */
      protected final static byte STATE_SHUTDOWN = 3;

      // attributes

      /**
       * The current state of the application; one of the STATE_* constants.
       */
      protected byte m_nState = STATE_INITIAL;

      // associations

      /**
       * An error that occurred while starting or resuming the application. Error
       * is saved here so that subsequent start/resume invocations can re-throw it.
       */
      private Exception m_exception;

      // operations

      public synchronized void startup() throws Exception
      {
         if (m_nState == STATE_INITIAL)
         {
            m_nState = STATE_RUNNING;

            if (s_logger.isInfoEnabled())
            {
               s_logger.info("Starting up the application (build number " +
                  SysUtil.getConfigProperties().getProperty("build.number", "unknown") +
                  ", build date " + SysUtil.getConfigProperties().getProperty("build.date", "unknown") +
                  ", core " + Version.RELEASE + ")");
            }

            try
            {
               ((Lifecycle)Repository.getMetadata().getComponent("System.Lifecycle").getInstance(null)).startup();
               s_logger.info("Startup complete");
            }
            catch (Throwable t)
            {
               s_logger.fatal("Unable to start up the application", t);

               m_exception = new LifecycleException("err.runtime.startup", t);
            }
         }

         if (m_exception != null)
         {
            throw m_exception;
         }
      }

      public synchronized void shutdown()
      {
         if (m_nState == STATE_RUNNING)
         {
            m_nState = STATE_SHUTDOWN;
            s_logger.info("Shutting down the application");

            try
            {
               ((Lifecycle)Repository.getMetadata().getComponent("System.Lifecycle").getInstance(null)).shutdown();
            }
            catch (Throwable t)
            {
               s_logger.warn("Unexpected exception when shutting down the application", t);
            }

            switch (J2EEUtil.CONTAINER)
            {
               // Custom module loading order cannot be enforced reliably
               // in 3rd-party containers
               case J2EEUtil.NONE:
               case J2EEUtil.TEEE:
                  Repository.unload();
                  XMLUtil.cleanup();
                  ClassProperty.cleanup();

                  break;
            }
         }
      }

      public synchronized void suspend() throws Exception
      {
         if (m_nState == STATE_RUNNING)
         {
            m_nState = STATE_SUSPENDED;
            s_logger.debug("Suspending the application");

            try
            {
               ((Lifecycle)Repository.getMetadata().getComponent("System.Lifecycle").getInstance(null)).suspend();
            }
            catch (Throwable t)
            {
               s_logger.fatal("Unable to suspend the application", t);

               m_exception = new LifecycleException("err.runtime.suspend", t);
            }
         }
      }

      public synchronized void resume() throws Exception
      {
         if (m_nState == STATE_SUSPENDED && m_exception == null)
         {
            m_nState = STATE_RUNNING;
            s_logger.debug("Resuming the application");

            try
            {
               ((Lifecycle)Repository.getMetadata().getComponent("System.Lifecycle").getInstance(null)).resume();
            }
            catch (Throwable t)
            {
               s_logger.fatal("Unable to resume the application", t);

               m_exception = new LifecycleException("err.runtime.resume", t);
            }
         }

         if (m_exception != null)
         {
            throw m_exception;
         }
      }
   };

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(LifecycleManager.class);

   // operations

   /**
    * @return The system lifecycle manager.
    */
   public static Lifecycle getLifecycle()
   {
      return s_lifecycle;
   }

   /**
    * Adds a managed component.
    * @param component The component to manage.
    */
   public void addComponent(Lifecycle component)
   {
      m_componentList.add(component);
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public void startup() throws Exception
   {
      for (int i = 0, nCount = m_componentList.size(); i < nCount; ++i)
      {
         ((Lifecycle)m_componentList.get(i)).startup();
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public void shutdown()
   {
      for (int i = m_componentList.size() - 1; i >= 0; --i)
      {
         ((Lifecycle)m_componentList.get(i)).shutdown();
      }
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
      for (int i = m_componentList.size() - 1; i >= 0; --i)
      {
         ((Lifecycle)m_componentList.get(i)).suspend();
      }
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      for (int i = 0, nCount = m_componentList.size(); i < nCount; ++i)
      {
         ((Lifecycle)m_componentList.get(i)).resume();
      }
   }
}
