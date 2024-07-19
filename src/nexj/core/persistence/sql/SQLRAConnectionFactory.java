// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Connection;
import java.sql.SQLException;

import javax.resource.ResourceException;
import javax.sql.DataSource;

import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.persistence.PersistenceException;
import nexj.core.rpc.sql.ra.SQLManagedConnectionFactory;
import nexj.core.runtime.Initializable;
import nexj.core.util.Logger;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;

/**
 * JCA wrapper for XA Data Source connection factory.
 */
public class SQLRAConnectionFactory implements SQLConnectionFactory, Initializable
{
   // constants

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SQLRAConnectionFactory.class);

   // associations

   /**
    * JCA ConnectionFactory to query for new Connection handles.
    */
   protected DataSource m_factory;

   /**
    * The configuration fragment to query factory settings from.
    */
   protected RelationalDatabaseFragment m_fragment;

   /**
    * JCA ManagedConnectionFactory to query for new ConnectionFactories.
    */
   protected SQLManagedConnectionFactory m_managedFactory;

   /**
    * The initialization SQL used by the connection factory for new connections.
    */
   protected String m_sInitialSQL;

   /**
    * The testing SQL used by the connection factory for existing connections.
    */
   protected String m_sTestSQL;

   // operations

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_fragment == null || !m_fragment.getDataSource().isEnabled())
      {
         throw new PersistenceException("err.persistence.sqlConnectionProperties");
      }

      m_managedFactory = (SQLManagedConnectionFactory)Class.forName(
            SysUtil.PACKAGE + ".core.rpc.sql.ra.SQLManagedConnectionFactory").newInstance();

      RelationalDatabase ds = (RelationalDatabase)m_fragment.getDataSource();

      m_managedFactory.setStatementCacheSize(m_fragment.getStatementCacheSize());
      m_managedFactory.setName(ds.getName());
      m_managedFactory.setDataSource(ds.getDriver());
      m_managedFactory.setPassword(m_fragment.getPassword());
      m_managedFactory.setProperties(
         PropertyUtil.toString(m_fragment.getPropertyHolder().getProperties()));
      m_managedFactory.setUser(m_fragment.getUser());
      m_managedFactory.setInitialSQL(m_sInitialSQL);
      m_managedFactory.setTestSQL(m_sTestSQL);

      m_factory = (DataSource)m_managedFactory.createConnectionFactory();
   }

   /**
    * @see nexj.core.persistence.sql.SQLConnectionFactory#getConnection(nexj.core.persistence.sql.SQLAdapter)
    */
   public Connection getConnection(SQLAdapter adapter) throws SQLException
   {
      DataSource factory;

      synchronized (this)
      {
         factory = m_factory;
      }

      try
      {
         return factory.getConnection(); // get a handle from the factory
      }
      catch (SQLException e)
      {
         SQLException x =
            new SQLException(
               "Unable to connect to data source \"" + m_fragment.getDataSource().getName() + '"',
               e.getSQLState(),
               e.getErrorCode());

         x.initCause(e);

         throw x;
      }
   }

   /**
    * Sets the database fragment.
    * @param fragment The fragment to set (not null).
    * @throws ResourceException  On factory instantiation/configuration failure.
    */
   public void setFragment(RelationalDatabaseFragment fragment) throws ResourceException
   {
      m_fragment = fragment;
   }

   /**
    * Set the initialization SQL used by the connection factory for new connections.
    * @param sSQL The SQL string to initialize with (null == none).
    */
   public void setInitialSQL(String sSQL)
   {
      m_sInitialSQL = sSQL;
   }

   /**
    * Set the testing SQL used by the connection factory for existing connections.
    * @param sSQL The SQL string to test with (null == none).
    */
   public void setTestSQL(String sSQL)
   {
      m_sTestSQL = sSQL;
   }
}