// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import java.io.File;
import java.io.IOException;

import javax.resource.ResourceException;
import javax.transaction.TransactionManager;

import nexj.core.meta.persistence.file.FileDataSource;
import nexj.core.meta.persistence.file.FileDataSourceFragment;
import nexj.core.rpc.file.FileConnection;
import nexj.core.rpc.file.FileConnectionFactory;
import nexj.core.rpc.file.ra.FileManagedConnectionFactory;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.platform.generic.GenericConfig;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Creates FileStorageConnection objects with an underlying FileConnection handle
 * originating from a connection factory that is held within this object,
 * configured by the current fragment of the the associated data source.
 * 
 * This is the connection factory used when nexj is running in the scheme
 * console or in unit tests.
 */
public class XAFileStorageConnectionFactory implements FileStorageConnectionFactory, Initializable
{
   // associations

   /**
    * The file fragment.
    */
   protected FileDataSourceFragment m_fragment;

   /**
    * The factory for creating file connections.
    */
   protected FileConnectionFactory m_factory;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(XAFileStorageConnectionFactory.class);

   /**
    * The transaction manager, so that connections issued by this factory can
    * be enlisted in the current transaction.
    */
   protected TransactionManager m_transactionManager;

   // operations

   /**
    * Sets the file fragment.
    * @param fragment The file fragment to set.
    */
   public void setFragment(FileDataSourceFragment fragment)
   {
      m_fragment = fragment;
   }

   /**
    * @return The file fragment.
    */
   public FileDataSourceFragment getFragment()
   {
      return m_fragment;
   }

   /**
    * @see nexj.core.persistence.file.FileStorageConnectionFactory#getDataSource()
    */
   public FileDataSource getDataSource()
   {
      return (FileDataSource)m_fragment.getDataSource();
   }

   /**
    * @see nexj.core.persistence.file.FileStorageConnectionFactory#getConnection(nexj.core.persistence.file.FileAdapter)
    */
   public FileConnection getConnection(FileAdapter adapter)
   {
      FileConnectionFactory factory = makeFactory(adapter);

      try
      {
         return factory.getConnection();
      }
      catch (ResourceException ex)
      {
         throw ObjUtil.rethrow(ex);
      }
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Initialized.");
      }
   }

   /**
    * Creates and configures a connection factory and underlying managed connection
    * factory. The data source member is used to get the fragments, which contain
    * the configuration data.
    * 
    * The correct fragment to use is identified from the invocation context on
    * the adapter.
    * 
    * @param adapter The adapter for which the file connection factory is being created.
    * @return A factory object for creating file connections.
    */
   protected synchronized FileConnectionFactory makeFactory(FileAdapter adapter)
   {
      if (m_factory != null)
      {
         return m_factory;
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating connection factory for file data source \"" +
            m_fragment.getDataSource().getName() + "\"" +
            ((m_fragment.isDefault()) ? "" : ", fragment \"" + m_fragment.getName() + "\""));
      }

      if (J2EEUtil.isContained())
      {
         throw new IllegalStateException("XAFileStorageConnectionFactory is only for J2EE non-contained mode");
      }

      FileManagedConnectionFactory mcf = new FileManagedConnectionFactory();

      mcf.setPersistenceConnection(true);
      mcf.setInputConnection(false);
      mcf.setOutgoingDirectory(m_fragment.getDataDirectory());
      mcf.setOutgoingTempDirectory(m_fragment.getTemporaryDirectory());
      mcf.setMaxNameSplits(m_fragment.getMaxNameSplits());
      mcf.setNameSplitSize(m_fragment.getNameSplitSize());

      File journalPath = new File(m_fragment.getJournalPath());

      if (!journalPath.isAbsolute())
      {
         try
         {
            journalPath = new File(new GenericConfig().createDataDirectory(),
               m_fragment.getJournalPath());
         }
         catch (IOException e)
         {
            return null;
         }
      }

      mcf.setJournalDirectory(journalPath.getAbsolutePath());

      try
      {
         m_factory = (FileConnectionFactory)mcf.createConnectionFactory();
         return m_factory;
      }
      catch (ResourceException ex)
      {
         return null;
      }
   }

   /**
    * Sets the transaction manager.
    * @param transactionManager The transaction manager to set.
    */
   public void setTransactionManager(TransactionManager transactionManager)
   {
      m_transactionManager = transactionManager;
   }

   /**
    * Provides a string representation for debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "XAFileStorageConnectionFactory(" + ((m_factory != null) ? m_factory.toString() : "N/A") + ")";
   }
}
