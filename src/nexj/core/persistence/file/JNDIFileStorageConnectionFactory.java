// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.resource.ResourceException;

import nexj.core.meta.persistence.file.FileDataSource;
import nexj.core.rpc.file.FileConnection;
import nexj.core.rpc.file.FileConnectionFactory;
import nexj.core.runtime.Initializable;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Creates FileStorageConnection objects with an underlying FileConnection handle
 * originating from a connection factory bound in the approparite JNDI namespace.
 * 
 * This is the connection factory used when nexj is running in a J2EE server.
 */
public class JNDIFileStorageConnectionFactory implements FileStorageConnectionFactory, Initializable
{
   // attributes

   /**
    * The JNDI name where the FileConnection factory is bound.
    */
   protected String m_sFactoryName;


   // associations

   /**
    * The factory for creating file connections, bound on initialization and kept
    * throughout the lifetime of this object.
    */
   protected FileConnectionFactory m_factory;

   /**
    * The data source from which to get the connection parameters.
    */
   protected FileDataSource m_dataSource;

   /**
    * A cache of the JNDI naming context.
    */
   protected Context m_context;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JNDIFileStorageConnectionFactory.class);


   // operations

   /**
    * Sets the JNDI name where the FileConnection factory is bound.
    * 
    * @param sFactoryName The JNDI name where the FileConnection factory
    *                     is bound.
    */
   public synchronized void setFactoryName(String sFactoryName)
   {
      m_sFactoryName = sFactoryName;
   }


   /**
    * Sets the data source from which this factory will retrieve the connection
    * parameters.
    * 
    * @param ds The data source to set.
    */
   public void setDataSource(FileDataSource ds)
   {
      m_dataSource = ds;
   }


   /**
    * @see nexj.core.persistence.file.FileStorageConnectionFactory#getDataSource()
    */
   public FileDataSource getDataSource()
   {
      return m_dataSource;
   }


   /**
    * @see nexj.core.persistence.file.FileStorageConnectionFactory#getConnection(nexj.core.persistence.file.FileAdapter)
    */
   public FileConnection getConnection(FileAdapter adapter)
   {
      FileConnectionFactory factory;
      
      synchronized (this)
      {
         factory = m_factory;
      }
      
      try
      {
         return factory.getConnection();
      }
      catch (ResourceException ex)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Rebinding to the stale factory \"" + m_sFactoryName + "\"");
         }
         
         try
         {
            synchronized (this)
            {
               // Check not already rebound
               if (factory == m_factory)
               {
                  initFactory();
               }
               
               factory = m_factory;
            }
            
            return factory.getConnection();
         }
         catch (NamingException nx)
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Failed to re-bind to factory \"" + m_sFactoryName + "\"", nx);
            }

            ObjUtil.rethrow(ex);

            return null;
         }
         catch (ResourceException rx)
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Unable to get connection from factory \"" + m_sFactoryName + "\"", rx);
            }

            ObjUtil.rethrow(ex);

            return null;
         }
      }
   }


   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Binding to factory \"" + m_sFactoryName + "\"");
      }
      
      m_context = new InitialContext();
      initFactory();
   }


   /**
    * Looks up the FileConnectionFactory in the JNDI namespace.
    * 
    * @throws NamingException
    */
   private synchronized void initFactory() throws NamingException
   {
      m_factory = (FileConnectionFactory)m_context.lookup(m_sFactoryName);
   }


   /**
    * Provides a string representation for debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "JNDIFileStorageConnectionFactory(" + ((m_factory != null) ? m_factory.toString() : "N/A") + ")";
   }
}
