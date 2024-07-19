// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.sql.ra;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.Statement;
import java.util.Map;

import javax.resource.ResourceException;

import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericManagedConnection;

/**
 * This is a connection handle for working with a single SQL XA connection.
 * It can be reassociated to a different XAConnection, but should not be unless it is closed first.
 *
 * This object is used in the  persistence framework for the SQL persistence
 * adapter.
 */
public class SQLConnection extends GenericConnection implements Connection
{
   /**
    * The managed connection this handle uses.
    */
   protected SQLManagedConnection m_managedConnection;

   /**
    * The wrapped java.sql.Connection (null == closed).
    */
   protected Connection m_connection;

   /**
    * @see java.sql.Connection#clearWarnings()
    */
   public void clearWarnings() throws SQLException
   {
      assert m_managedConnection != null;
      assert m_managedConnection.m_sqlConnection != null;

      m_managedConnection.m_sqlConnection.clearWarnings();
   }

    /**
    * @see java.sql.Connection#close()
    */
   public void close() throws SQLException
   {
      try
      {
         closeHandle();
      }
      catch (ResourceException e)
      {
         if (e.getCause() instanceof SQLException)
         {
            throw (SQLException)e.getCause();
         }

         SQLException sqlE = new SQLException(e.getLocalizedMessage());

         sqlE.initCause(e);

         throw sqlE;
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConnection#closeHandle()
    */
   protected synchronized void closeHandle() throws ResourceException
   {
      m_connection = null; // ensure assert in setManagedConnection() passes
      m_managedConnection = null;

      // Do not close the java.sql.Connection, let the SQLManagedConnection do that
      // Since some XADatasources (e.g. DB2) will not close handles properly
      super.closeHandle();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConnection#setManagedConnection(nexj.core.rpc.ra.GenericManagedConnection)
    */
   protected synchronized void setManagedConnection(GenericManagedConnection managedConnection)
      throws ResourceException
   {
      assert m_connection == null;

      super.setManagedConnection(managedConnection);

      // check required for jUnit tests to pass
      if (super.m_managedConnection instanceof SQLManagedConnection)
      {
         m_managedConnection = (SQLManagedConnection)super.m_managedConnection;
         m_connection = m_managedConnection.m_sqlConnection;
      }
   }

   /**
    * @see java.sql.Connection#commit()
    */
   public void commit() throws SQLException
   {
      throw new UnsupportedOperationException(); // not supported
   }

   /**
    * @see java.sql.Connection#createStatement()
    */
   public Statement createStatement() throws SQLException
   {
      return m_managedConnection.createStatement(this);
   }

   /**
    * @see java.sql.Connection#createStatement(int, int)
    */
   public Statement createStatement(int nResultSetType, int nResultSetConcurrency)
      throws SQLException
   {
      return m_managedConnection.createStatement(this, nResultSetType, nResultSetConcurrency);
   }

   /**
    * @see java.sql.Connection#createStatement(int, int, int)
    */
   public Statement createStatement(int nResultSetType, int nResultSetConcurrency,
      int nResultSetHoldability) throws SQLException
   {
      return m_managedConnection.createStatement(
         this, nResultSetType, nResultSetConcurrency, nResultSetHoldability);
   }

   /**
    * @see java.sql.Connection#getAutoCommit()
    */
   public boolean getAutoCommit() throws SQLException
   {
      return m_connection.getAutoCommit();
   }

   /**
    * @see java.sql.Connection#getCatalog()
    */
   public String getCatalog() throws SQLException
   {
      return m_connection.getCatalog();
   }

   /**
    * @return The underlying wrapped connection.
    */
   public Connection getConnection()
   {
      return m_connection;
   }

   /**
    * @see java.sql.Connection#getHoldability()
    */
   public int getHoldability() throws SQLException
   {
      return m_connection.getHoldability();
   }

   /**
    * @see java.sql.Connection#getMetaData()
    */
   public DatabaseMetaData getMetaData() throws SQLException
   {
      return m_connection.getMetaData();
   }

   /**
    * @see java.sql.Connection#getTransactionIsolation()
    */
   public int getTransactionIsolation() throws SQLException
   {
      return m_connection.getTransactionIsolation();
   }

   /**
    * @see java.sql.Connection#getTypeMap()
    */
   public Map getTypeMap() throws SQLException
   {
      return m_connection.getTypeMap();
   }

   /**
    * @see java.sql.Connection#getWarnings()
    */
   public SQLWarning getWarnings() throws SQLException
   {
      return m_connection.getWarnings();
   }

   /**
    * @see java.sql.Connection#isClosed()
    */
   public boolean isClosed() throws SQLException
   {
      return m_connection == null || m_connection.isClosed();
   }

   /**
    * @see java.sql.Connection#isReadOnly()
    */
   public boolean isReadOnly() throws SQLException
   {
      return m_connection.isReadOnly();
   }

   /**
    * @see java.sql.Connection#nativeSQL(java.lang.String)
    */
   public String nativeSQL(String sSQL) throws SQLException
   {
      return m_connection.nativeSQL(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String)
    */
   public CallableStatement prepareCall(String sSQL) throws SQLException
   {
      return m_managedConnection.prepareCall(this, sSQL);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
    */
   public CallableStatement prepareCall(String sSQL, int nResultSetType, int nResultSetConcurrency)
      throws SQLException
   {
      return m_managedConnection.prepareCall(this, sSQL, nResultSetType, nResultSetConcurrency);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
    */
   public CallableStatement prepareCall(String sSQL, int nResultSetType, int nResultSetConcurrency,
      int resultSetHoldability) throws SQLException
   {
      return m_managedConnection.prepareCall(
         this, sSQL, nResultSetType, nResultSetConcurrency, resultSetHoldability);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String)
    */
   public PreparedStatement prepareStatement(String sSQL) throws SQLException
   {
      return m_managedConnection.prepareStatement(this, sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int nAutoGeneratedKeys)
      throws SQLException
   {
      return m_managedConnection.prepareStatement(this, sSQL, nAutoGeneratedKeys);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
    */
   public PreparedStatement prepareStatement(String sSQL, int[] columnIndexArray)
      throws SQLException
   {
      return m_managedConnection.prepareStatement(this, sSQL, columnIndexArray);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
    */
   public PreparedStatement prepareStatement(String sSQL, String[] columnNameArray)
      throws SQLException
   {
      return m_managedConnection.prepareStatement(this, sSQL, columnNameArray);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
    */
   public PreparedStatement prepareStatement(
      String sSQL, int nResultSetType, int nResultSetConcurrency)
      throws SQLException
   {
      return m_managedConnection.prepareStatement(
         this, sSQL, nResultSetType, nResultSetConcurrency);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int nResultSetType,
      int nResultSetConcurrency, int nResultSetHoldability) throws SQLException
   {
      return m_managedConnection.prepareStatement(
         this, sSQL, nResultSetType, nResultSetConcurrency, nResultSetHoldability);
   }

   /**
    * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
    */
   public void releaseSavepoint(Savepoint savepoint) throws SQLException
   {
      throw new UnsupportedOperationException(); // not supported
   }

   /**
    * @see java.sql.Connection#rollback()
    */
   public void rollback() throws SQLException
   {
      throw new UnsupportedOperationException(); // not supported
   }

   /**
    * @see java.sql.Connection#rollback(java.sql.Savepoint)
    */
   public void rollback(Savepoint savepoint) throws SQLException
   {
      throw new UnsupportedOperationException(); // not supported
   }

   /**
    * @see java.sql.Connection#setAutoCommit(boolean)
    */
   public void setAutoCommit(boolean bAutoCommit) throws SQLException
   {
      m_connection.setAutoCommit(bAutoCommit);
   }

   /**
    * @see java.sql.Connection#setCatalog(java.lang.String)
    */
   public void setCatalog(String sCatalog) throws SQLException
   {
      m_connection.setCatalog(sCatalog);
   }

   /**
    * @see java.sql.Connection#setHoldability(int)
    */
   public void setHoldability(int nHoldability) throws SQLException
   {
      m_connection.setHoldability(nHoldability);
   }

   /**
    * @see java.sql.Connection#setReadOnly(boolean)
    */
   public void setReadOnly(boolean bReadOnly) throws SQLException
   {
      m_connection.setReadOnly(bReadOnly);
   }

   /**
    * @see java.sql.Connection#setSavepoint()
    */
   public Savepoint setSavepoint() throws SQLException
   {
      throw new UnsupportedOperationException(); // not supported
   }

   /**
    * @see java.sql.Connection#setSavepoint(java.lang.String)
    */
   public Savepoint setSavepoint(String sName) throws SQLException
   {
      throw new UnsupportedOperationException(); // not supported
   }

   /**
    * @see java.sql.Connection#setTransactionIsolation(int)
    */
   public void setTransactionIsolation(int nLevel) throws SQLException
   {
      m_connection.setTransactionIsolation(nLevel);
   }

   /**
    * @see java.sql.Connection#setTypeMap(java.util.Map)
    */
   public void setTypeMap(Map map) throws SQLException
   {
      m_connection.setTypeMap(map);
   }
}