// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.sql.ra;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.security.auth.Subject;
import javax.sql.XADataSource;

import nexj.core.rpc.ra.GenericManagedConnectionFactory;
import nexj.core.util.BeanAccessor;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * SQL managed connection factory.
 */
public class SQLManagedConnectionFactory extends GenericManagedConnectionFactory
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -7934292382786073677L;

   // attributes

   /**
    * Flag this connection as requiring rollback if it's AutoCommit is true.
    * This is an Oracle hack, see SQLManagedConnectionFactory.getXAConnection(String, String).
    */
   protected boolean m_bAutoCommitRollbackRequired = false;

   /**
    * The XAResourceWrapper should use the return value of isSameRM of the wrapped XAResource.
    * If false then XAResourceWrapper should always return false to isSameRM().
    */
   protected boolean m_bSameRMUsed = true;

   /**
    * Flag this connection as being usable by multiple handles.
    * This is a MySQL kluge, see SQLManagedConnection.matches(Subject, GenericConnectionRequestInfo)
    */
   protected boolean m_bShared = true;

   // associations

   /**
    * Default connection credentials.
    */
   protected SQLConnectionRequestInfo m_conInfo = new SQLConnectionRequestInfo();

   /**
    * The wrapped SQL XA DataSource.
    */
   protected XADataSource m_dataSource;

   /**
    * The properties to initialize new XADataSource with.
    */
   protected Properties m_dataSourceProperties;

   /**
    * The transaction isolation level.
    * One of the java.sql.Connection.TRANSACTION_* constants.
    */
   protected int m_nIsolationLevel = Connection.TRANSACTION_READ_COMMITTED;

   /**
    * The maximum number of statements to cache per connection (negative value == unlimited).
    */
   protected int m_nStatementCacheSize = 1000;

   /**
    * The initial SQL statement to execute when the SQL connection is first created.
    */
   protected String m_sInitialSQL;

   /**
    * The name of this factory, used in error messages.
    */
   protected String m_sName;

    /**
    * The connection testing statement.
    */
   protected String m_sTestSQL;

   // operations

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory(javax.resource.spi.ConnectionManager)
    */
   public Object createConnectionFactory(ConnectionManager manager) throws ResourceException
   {
      return new SQLConnectionFactory(manager, this);
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createManagedConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection createManagedConnection(Subject subject, ConnectionRequestInfo cri)
      throws ResourceException
   {
      assert m_dataSource != null; // need DataSource initialized

      SQLConnectionRequestInfo info = (SQLConnectionRequestInfo)cri;

      try
      {
         String sPassword = info.getPassword(); // the actual password for this connection
         String sUser = info.getUser(); // the actual user for this connection

         return new SQLManagedConnection(
            (sUser == null && sPassword == null)
            ? m_dataSource.getXAConnection() : m_dataSource.getXAConnection(sUser, sPassword),
            this,
            info);
      }
      catch (SQLException e)
      {
         throw new ResourceException(e.getLocalizedMessage(), e);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnectionFactory#getDefaultConnectionManager()
    */
   protected ConnectionManager getDefaultConnectionManager()
   {
      return SQLResourceAdapter.getDefaultConnectionManager();
   }

   /**
    * @return The default connection request info object. Do not modify.
    */
   public SQLConnectionRequestInfo getDefaultConnectionRequestInfo()
   {
      return m_conInfo;
   }

   /**
    * @return True if rollback should be invoked when an auto-commit connection is closed (Oracle kluge).
    */
   public boolean isAutoCommitRollbackRequired()
   {
      return m_bAutoCommitRollbackRequired;
   }

   /**
    * @return True if the XA resource isSameRM() should be used. False to make that return false (Oracle/jTDS kluge).
    */
   public boolean isSameRMUsed()
   {
      return m_bSameRMUsed;
   }

   /**
    * @return True to reuse the same database connection in multiple handles (false for MySQL kluge).
    */
   public boolean isShared()
   {
      return m_bShared;
   }

   /**
    * @param nSize The maximum number of statements to cache per connection (negative == unlimited).
    */
   public void setStatementCacheSize(int nSize)
   {
      m_nStatementCacheSize = nSize;
   }

   /**
    * @return The maximum number of statements to cache per connection (negative == unlimited).
    */
   public int getStatementCacheSize()
   {
      return m_nStatementCacheSize;
   }

   /**
    * Instantiate an XADataSource for the specified class name.
    * @param sDataSource The class name of the XADataSource to instantiate.
    * @throws ResourceException If an instantiation error occurs.
    */
   public void setDataSource(String sDataSource) throws ResourceException
   {
      assert m_dataSource == null; // do not support changing DataSources

      try
      {
         m_dataSource = (XADataSource)Class.forName(sDataSource).newInstance();

         if (m_dataSourceProperties != null)
         {
            new BeanAccessor().setProperties(m_dataSource, m_dataSourceProperties);
         }
      }
      catch (Throwable e)
      {
         throw new ResourceException(e);
      }

      // Oracle kluge
      m_bAutoCommitRollbackRequired = sDataSource.indexOf("Oracle") >= 0;

      // Oracle/JTDS kluge
      m_bSameRMUsed = sDataSource.indexOf("Jtds") < 0 && sDataSource.indexOf("Oracle") < 0;

      // MySQL connections do not allow executing statements if a connection already has a
      // streaming ResultSet in progress. Hence set connections to not be sharable.
      m_bShared = sDataSource.indexOf("Mysql") < 0;
   }

   /**
    * Sets the initial SQL statement to execute when a connection is first established.
    * @param sTestSQL The initial SQL statement to set; null to omit initial statement.
    */
   public void setInitialSQL(String sInitialSQL)
   {
      m_sInitialSQL = sInitialSQL;
   }

   /**
    * @return The initial SQL statement.
    */
   public String getInitialSQL()
   {
      return m_sInitialSQL;
   }

   /**
    * Sets the transaction isolation level.
    * @param nIsolationLevel The transaction isolation level to set,
    *                        one of java.sql.Connection.TRANSACTION_* constants.
    */
   public void setIsolationLevel(int nIsolationLevel)
   {
      m_nIsolationLevel = nIsolationLevel;
   }

   /**
    * Set the name of this factory.
    * @param sName The name of this factory.
    */
   public void setName(String sName)
   {
      m_sName = sName;
   }

   /**
    * Set the default user to use for new connections.
    * @param sUser The default user to use for new connections.
    */
   public void setUser(String sUser)
   {
      m_conInfo.setUser(sUser);
   }

   /**
    * Set the default password to use for new connections.
    * @param sPassword The default password to use for new connections.
    */
   public void setPassword(String sPassword)
   {
      CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();

      dispatcher.init(SysUtil.getConfigProperties());
      m_conInfo.setPassword(dispatcher.decrypt(sPassword));
   }

   /**
    * Set XADataSource initialization properties (marshalled via PropertyUtil.toString()).
    * @param sProperties The properties to initialize the XADataSource with.
    * @throws ResourceException if properties are unusable.
    */
   public void setProperties(String sProperties) throws ResourceException
   {
      try
      {
         m_dataSourceProperties = PropertyUtil.fromString(sProperties);

         if (m_dataSource != null)
         {
            new BeanAccessor().setProperties(m_dataSource, m_dataSourceProperties);
         }
      }
      catch (Throwable e)
      {
         throw new ResourceException("err.meta.dataSourceConnectionProperty", e);
      }
   }

    /**
    * Sets the connection test SQL statement.
    * @param sTestSQL The SQL statement to set.
    */
   public void setTestSQL(String sTestSQL)
   {
      m_sTestSQL = sTestSQL;
   }

   /**
    * @return The connection test SQL statement.
    */
   public String getTestSQL()
   {
      return m_sTestSQL;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(ObjUtil.getShortClassName(this));

      if (m_dataSourceProperties != null)
      {
         buf.append('(');
         buf.append(m_dataSourceProperties);
         buf.append(')');
      }

      return buf.toString();
   }
}