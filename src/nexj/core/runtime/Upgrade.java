// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.util.Iterator;

import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.persistence.LockTimeoutException;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.SchemaVersion;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Checks the versions of the persistent storages and upgrades the system.
 */
public class Upgrade implements Initializable, Lifecycle
{
   // attributes

   /**
    * Is the Lifecycle object in a running state as opposed to shuting-down state.
    */
   protected boolean m_bStartup;

   /**
    * The user account for executing the upgrade.
    */
   protected String m_sUser;
   
   // associations
   
   /**
    * The invocation context component.
    */
   protected Component m_contextComponent;

   /**
    * SystemPartition metaclass.
    */
   private Metaclass m_systemPartitionClass;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(Upgrade.class);
   
   // operations

   /**
    * Sets the invocation context component.
    * @param contextComponent The invocation context component to set.
    */
   public void setContextComponent(Component contextComponent)
   {
      m_contextComponent = contextComponent;
   }

   /**
    * Sets the user account for executing the upgrade.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }
   
   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_contextComponent == null)
      {
         m_contextComponent = Repository.getMetadata().getComponent("System.InvocationContext");
      }

      m_systemPartitionClass = m_contextComponent.getMetadata().findMetaclass("SystemPartition");
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public void startup() throws Exception
   {
      synchronized (this)
      {
         m_bStartup = true;
      }

      while (m_bStartup) // startup retry loop
      {
         try
         {
            upgrade();
            break;
         }
         catch (LockTimeoutException e)
         {
            s_logger.debug("Database lock has timed out - retrying");
         }
      }
   }

   /**
    * Check metadata version and upgrade metadata.
    * @throws Exception On error.
    */
   protected void upgrade() throws Exception
   {
      Context contextSaved = ThreadContextHolder.getContext();
      InvocationContext context = (InvocationContext)m_contextComponent.getInstance(null);
      int nCookie = -1;

      try
      {
         s_logger.debug("Checking persistent storage versions");

         context.initialize(null);
         context.setSecure(false);
         context.setPartitioned(false);

         if (m_systemPartitionClass != null)
         {
            context.login((Instance)Query.createRead(m_systemPartitionClass, null, null, null, 1, 0, false,
                  Query.SEC_NONE, context).read().getInstance(0).getValue("systemUser"));
            context.setPartitioned(true);
         }

         nCookie = Logger.pushContext(context.getPrincipal().getName());

         Metadata metadata = context.getMetadata();

         context.getTransactionManager().setTransactionTimeout(86400);
         context.beginTransaction();

         for (Iterator dataSourceItr = metadata.getDataSourceIterator(); dataSourceItr.hasNext();)
         {
            DataSource dataSource = (DataSource)dataSourceItr.next();

            if (dataSource.isEnabled() && dataSource.getComponent() != null)
            {
               for (Iterator fragmentItr = dataSource.getFragmentIterator(); fragmentItr.hasNext();)
               {
                  DataSourceFragment fragment = (DataSourceFragment)fragmentItr.next();
                  String sFragment = (fragment.getName() == null) ? "" : fragment.getName();

                  context.setFragmentName(sFragment);

                  SchemaVersion version = ((PersistenceAdapter)dataSource.getComponent()
                     .getInstance(context)).getVersion(dataSource.getSchema());

                  if (version != null)
                  {
                     if (!metadata.getNamespace().equals(version.getNamespace()))
                     {
                        throw new PersistenceException("err.persistence.storageNamespace",
                           new Object[]{version.getNamespace(), dataSource.getName(),
                              sFragment, metadata.getNamespace()});
                     }

                     if (!metadata.getVersion().equals(version.getVersion()))
                     {
                        throw new PersistenceException("err.persistence.storageVersion",
                           new Object[]{version.getVersion(), dataSource.getName(),
                           sFragment, metadata.getVersion(), metadata.getNamespace()});
                     }

                     int nStep = version.getStep();

                     if (nStep != -1)
                     {
                        throw new PersistenceException("err.persistence.failedStep",
                           new Object[]{version.getVersion(), Primitive.createInteger(nStep), dataSource.getName()});
                     }
                  }
               }
            }
         }

         context.setFragmentName(null);

         s_logger.debug("Persistent storage versions OK");

         Metaclass upgrade = metadata.findMetaclass("SysUpgrade");

         if (upgrade != null)
         {
            if (m_sUser != null && m_sUser.length() != 0)
            {
               context.login(new SimplePrincipal(m_sUser));
               Logger.resetContext(nCookie);
               nCookie = Logger.pushContext(context.getPrincipal().getName());
            }

            upgrade.invoke("upgrade");
            context.getUnitOfWork().commit(false);
            context.setFragmentName(null);
         }

         context.complete(true);
      }
      catch (Throwable t)
      {
         context.complete(false);

         if (t instanceof Exception)
         {
            throw (Exception)t;
         }

         ObjUtil.rethrow(t);
      }
      finally
      {
         context.getTransactionManager().setTransactionTimeout(0);

         if (nCookie != -1)
         {
            Logger.resetContext(nCookie);
         }

         ThreadContextHolder.setContext(contextSaved);
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public synchronized void shutdown()
   {
      m_bStartup = false;
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
      shutdown();
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      startup();
   }
}
