// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.sql.ra;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.sql.DataSource;

import nexj.core.rpc.ra.GenericConnectionFactory;

/**
 * SQL connection factory.
 */
public class SQLConnectionFactory extends GenericConnectionFactory implements DataSource
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -6322561831513751754L;

   /**
    * The provider of managed connections.
    */
   protected SQLManagedConnectionFactory m_factory;

   /**
    * Constructor.
    * @param manager The connection manager (e.g. managing pooling).
    */
   protected SQLConnectionFactory(ConnectionManager manager, SQLManagedConnectionFactory factory)
   {
      super(manager);

      assert factory != null; // required for proper functionality

      m_factory = factory;
   }

   /**
    * @see javax.sql.DataSource#getConnection()
    */
   public Connection getConnection() throws SQLException
   {
      try
      {
         return (Connection)m_manager.allocateConnection(m_factory, m_factory.getDefaultConnectionRequestInfo());
      }
      catch (ResourceException e)
      {
         if (e.getCause() instanceof SQLException)
         {
            throw (SQLException)e.getCause();
         }

         SQLException x = new SQLException(e.getLocalizedMessage());

         x.initCause(e);

         throw x;
      }
   }

   /**
    * @see javax.sql.DataSource#getConnection(java.lang.String, java.lang.String)
    */
   public Connection getConnection(String sUser, String sPassword) throws SQLException
   {
      try
      {
         return (Connection)m_manager.allocateConnection(m_factory, new SQLConnectionRequestInfo(sUser, sPassword));
      }
      catch (ResourceException e)
      {
         if (e.getCause() instanceof SQLException)
         {
            throw (SQLException)e.getCause();
         }

         SQLException x = new SQLException(e.getLocalizedMessage());

         x.initCause(e);

         throw x;
      }
   }

   /**
    * @see javax.sql.DataSource#getLogWriter()
    */
   public PrintWriter getLogWriter() throws SQLException
   {
      return m_factory.m_dataSource.getLogWriter();
   }

   /**
    * @see javax.sql.DataSource#getLoginTimeout()
    */
   public int getLoginTimeout() throws SQLException
   {
      return m_factory.m_dataSource.getLoginTimeout();
   }

   /**
    * @see javax.sql.DataSource#setLogWriter(java.io.PrintWriter)
    */
   public void setLogWriter(PrintWriter out) throws SQLException
   {
      // NOOP
   }

   /**
    * @see javax.sql.DataSource#setLoginTimeout(int)
    */
   public void setLoginTimeout(int nSeconds) throws SQLException
   {
      // NOOP
   }
}