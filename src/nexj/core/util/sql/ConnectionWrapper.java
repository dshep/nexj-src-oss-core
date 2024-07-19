// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.sql;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.Statement;
import java.util.Map;

/**
 * A wrapper class for a connection.
 */
public abstract class ConnectionWrapper implements Connection
{
   // associations

   /**
    * The wrapped connection.
    */
   protected Connection m_connection;

   // constructors

   /**
    * Constructs the wrapper.
    * @param connection The connection to wrap.
    */
   protected ConnectionWrapper(Connection connection)
   {
      m_connection = connection;
   }

   // operations

   /**
    * @return The wrapped connection.
    */
   public Connection getConnection()
   {
      return m_connection;
   }

   /**
    * @see java.sql.Connection#clearWarnings()
    */
   public void clearWarnings() throws SQLException
   {
      m_connection.clearWarnings();
   }

   /**
    * @see java.sql.Connection#close()
    */
   public void close() throws SQLException
   {
      m_connection.close();
   }

   /**
    * @see java.sql.Connection#commit()
    */
   public void commit() throws SQLException
   {
      m_connection.commit();
   }

   /**
    * @see java.sql.Connection#createStatement()
    */
   public Statement createStatement() throws SQLException
   {
      return m_connection.createStatement();
   }

   /**
    * @see java.sql.Connection#createStatement(int, int)
    */
   public Statement createStatement(
      int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      return m_connection.createStatement(nResultSetType, nResultSetConcurrency);
   }

   /**
    * @see java.sql.Connection#createStatement(int, int, int)
    */
   public Statement createStatement(
      int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability) throws SQLException
   {
      return m_connection.createStatement(
                nResultSetType, nResultSetConcurrency, nResultSetHoldability);
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
      return m_connection.isClosed();
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
      return m_connection.prepareCall(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
    */
   public CallableStatement prepareCall(
      String sSQL, int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      return m_connection.prepareCall(sSQL, nResultSetType, nResultSetConcurrency);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
    */
   public CallableStatement prepareCall(
      String sSQL, int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability)
      throws SQLException
   {
      return m_connection.prepareCall(
                sSQL, nResultSetType, nResultSetConcurrency,nResultSetHoldability);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String)
    */
   public PreparedStatement prepareStatement(String sSQL) throws SQLException
   {
      return m_connection.prepareStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int nAutoGeneratedKeys)
      throws SQLException
   {
      return m_connection.prepareStatement(sSQL, nAutoGeneratedKeys);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
    */
   public PreparedStatement prepareStatement(String sSQL, int[] nColumnIndexArray)
      throws SQLException
   {
      return m_connection.prepareStatement(sSQL, nColumnIndexArray);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
    */
   public PreparedStatement prepareStatement(
      String sSQL, String[] sColumnNameArray) throws SQLException
   {
      return m_connection.prepareStatement(sSQL, sColumnNameArray);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
    */
   public PreparedStatement prepareStatement(
      String sSQL, int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      return m_connection.prepareStatement(sSQL, nResultSetType, nResultSetConcurrency);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
    */
   public PreparedStatement prepareStatement(
      String sSQL, int nResultSetType, int nRresultSetConcurrency, int nRresultSetHoldability)
      throws SQLException
   {
      return m_connection.prepareStatement(
                sSQL, nResultSetType, nRresultSetConcurrency, nRresultSetHoldability);
   }

   /**
    * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
    */
   public void releaseSavepoint(Savepoint savepoint) throws SQLException
   {
      m_connection.releaseSavepoint(savepoint);
   }

   /**
    * @see java.sql.Connection#rollback()
    */
   public void rollback() throws SQLException
   {
      m_connection.rollback();
   }

   /**
    * @see java.sql.Connection#rollback(java.sql.Savepoint)
    */
   public void rollback(Savepoint savepoint) throws SQLException
   {
      m_connection.rollback(savepoint);
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
      return m_connection.setSavepoint();
   }

   /**
    * @see java.sql.Connection#setSavepoint(java.lang.String)
    */
   public Savepoint setSavepoint(String sName) throws SQLException
   {
      return m_connection.setSavepoint(sName);
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