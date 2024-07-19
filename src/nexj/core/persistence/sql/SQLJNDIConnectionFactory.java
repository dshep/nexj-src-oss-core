// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.resource.ResourceException;
import javax.sql.DataSource;

import nexj.core.persistence.PersistenceException;
import nexj.core.runtime.Initializable;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;

/**
 * Data Source connection factory with JNDI lookup (for J2EE containers)
 */
public class SQLJNDIConnectionFactory implements SQLConnectionFactory, Initializable
{
   // attributes
   
   /**
    * The data source name.
    */
   private String m_sDataSource;

   // associations

   /**
    * Naming context.
    */
   private Context m_context;
   
   /**
    * The data source object.
    */
   private DataSource m_dataSource;
   
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(SQLJNDIConnectionFactory.class);
   
   // operations

   /**
    * Sets the data source name.
    * @param sDataSource The data source name to set.
    */
   public void setDataSource(String sDataSource)
   {
      m_sDataSource = sDataSource;
   }

   /**
    * @return The data source name.
    */
   public String getDataSource()
   {
      return m_sDataSource;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_sDataSource == null)
      {
         throw new PersistenceException("err.persistence.sqlConnectionProperties");
      }
      
      if (m_sDataSource.indexOf(':') < 0)
      {
         if (m_sDataSource.length() > 0 && m_sDataSource.charAt(0) == '/')
         {
            m_sDataSource = m_sDataSource.substring(1);
         }
         else
         {
            m_sDataSource = J2EEUtil.JNDI_ENV_PREFIX + "jdbc/" + m_sDataSource;
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Binding to data source \"" + m_sDataSource + "\"");
      }

      m_context = new InitialContext();
      initDataSource();
   }

   /**
    * Looks up and sets the data source in the initial context.
    */
   private synchronized void initDataSource() throws NamingException
   {
      m_dataSource = (DataSource)m_context.lookup(m_sDataSource);
   }

   /**
    * @see nexj.core.persistence.sql.SQLConnectionFactory#getConnection(nexj.core.persistence.sql.SQLAdapter)
    */
   public Connection getConnection(SQLAdapter adapter) throws SQLException
   {
      DataSource dataSource; 
      
      synchronized (this)
      {
         dataSource = m_dataSource;
      }

      try
      {
         return dataSource.getConnection();
      }
      catch (SQLException e)
      {
         for (Throwable x = e.getCause(); x != null; x = x.getCause())
         {
            if (x instanceof ResourceException || x instanceof IOException)
            {
               // The connection factory is stale, get a new one

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Rebinding the stale data source \"" + m_sDataSource + "\"");
               }

               try
               {
                  synchronized (this)
                  {
                     if (dataSource == m_dataSource)
                     {
                        initDataSource();
                     }

                     dataSource = m_dataSource;
                  }
               }
               catch (NamingException nx)
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Failed to bind to data source \"" + m_sDataSource + "\"", nx);
                  }

                  break;
               }

               return dataSource.getConnection();
            }
         }

         SQLException x =
            new SQLException("Unable to connect to data source \"" + getDataSource() + '"',
                             e.getSQLState(),
                             e.getErrorCode());

         x.initCause(e);

         throw x;
      }
   }
}
