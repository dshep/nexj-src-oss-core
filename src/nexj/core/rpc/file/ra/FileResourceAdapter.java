// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.util.Iterator;
import java.util.Locale;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.BootstrapContext;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ResourceAdapterInternalException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.xa.XAResource;

import nexj.core.rpc.ra.GenericConnectionManager;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.rpc.ra.TransactionalResourceAdapter;
import nexj.core.runtime.platform.PlatformConfig;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;

/**
 * The file resource adapter.
 */
public class FileResourceAdapter extends TransactionalResourceAdapter
{
   // associations

   /**
    * The default connection manager.
    */
   private static GenericConnectionManager s_defaultConnectionManager;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(FileResourceAdapter.class);

   /**
    * The platform configuration.
    */
   protected PlatformConfig m_platformConfig;

   /**
    * Holds ManagedConnection instances created for recovery by getXAResources().
    */
   protected Holder m_recoveryConnectionHolder;

   // constructors

   /**
    * Creates a new file resource adapter.
    */
   public FileResourceAdapter()
   {
      super(s_logger);
   }


   // operations

   /**
    * Gets the platform configuration. Will not be valid until after start()
    * has been called.
    * 
    * @return The platform configuration.
    */
   public PlatformConfig getPlatformConfig()
   {
      return m_platformConfig;
   }


   /**
    * @return The default connection manager.
    */
   public static synchronized ConnectionManager getDefaultConnectionManager()
   {
      if (s_defaultConnectionManager == null)
      {
         s_defaultConnectionManager = new GenericConnectionManager(GenericConnectionManager.ASSOCIATED);
      }

      return s_defaultConnectionManager;
   }


   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#stop()
    */
   public void stop()
   {
      synchronized (FileResourceAdapter.class)
      {
         if (s_defaultConnectionManager != null)
         {
            s_defaultConnectionManager.clear();
            s_defaultConnectionManager = null;
         }

         if (m_recoveryConnectionHolder != null)
         {
            for (Iterator itr = m_recoveryConnectionHolder.iterator(); itr.hasNext(); )
            {
               FileManagedConnection mconn = (FileManagedConnection)itr.next();

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("stop() is destroying: " + mconn);
               }

               try
               {
                  mconn.destroy();
               }
               catch (ResourceException ex)
               {
                  s_logger.error("Error destroying recovery managed connection", ex);

                  throw ObjUtil.rethrow(ex);
               }
            }

            m_recoveryConnectionHolder.clear();
         }
      }

      super.stop();
   }


   /**
    * @see javax.resource.spi.ResourceAdapter#getXAResources(javax.resource.spi.ActivationSpec[])
    */
   public XAResource[] getXAResources(ActivationSpec[] specs) throws ResourceException
   {
      XAResource[] resourceArray = new XAResource[specs.length];

      for (int i = 0; i < specs.length; i++)
      {
         FileConsumerConfig config = (FileConsumerConfig)specs[i];
         FileManagedConnectionFactory mcf = new FileManagedConnectionFactory();

         //Configure this temporary managed connection according to the ActivationSpec
         mcf.setResourceAdapter(this);
         mcf.setInputConnection(true);
         mcf.setIncomingDirectory(config.getIncomingDirectory());
         mcf.setProcessedDirectory(config.getProcessedDirectory());         
         mcf.setJournalDirectory(config.getJournalDirectory());

         FileManagedConnection mconn = (FileManagedConnection)mcf.createManagedConnection(null, null);

         resourceArray[i] = mconn.getXAResource();
         mconn.setRecovery(true);

         // Cannot mconn.destroy() as managed connection is needed for XAResource operation
         if (m_recoveryConnectionHolder == null)
         {
            m_recoveryConnectionHolder = new HashHolder(specs.length);
         }

         m_recoveryConnectionHolder.add(mconn);
      }

      return resourceArray;
   }


   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#createConsumerPool(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   protected GenericConsumerPool createConsumerPool(MessageEndpointFactory factory, ActivationSpec cfg)
      throws ResourceException
   {
      return new FileConsumerPool(this, factory, (FileConsumerConfig)cfg);
   }


   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#endpointActivation(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   public void endpointActivation(MessageEndpointFactory factory, ActivationSpec cfg) throws ResourceException
   {
      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Activating endpoint with configuration: " + cfg);
      }
      
      super.endpointActivation(factory, cfg);
   }


   /**
    * @see nexj.core.rpc.ra.TransactionalResourceAdapter#start(javax.resource.spi.BootstrapContext)
    */
   public void start(BootstrapContext context) throws ResourceAdapterInternalException
   {
      if (!J2EEUtil.isContained())
      {
         throw new ResourceAdapterInternalException("Unknown J2EE container");
      }

      String sContainer = J2EEUtil.getPlatformName();

      try
      {
         m_platformConfig = (PlatformConfig)Class.forName(
            SysUtil.PACKAGE + ".core.runtime.platform." + sContainer.toLowerCase(Locale.ENGLISH) + '.' +
            sContainer + "Config").newInstance();
      }
      catch (Throwable e)
      {
         throw new ResourceAdapterInternalException("Unable to locate the platform configuration", e);
      }
      
      super.start(context);
   }
}
