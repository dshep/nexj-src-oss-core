// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.Statement;
import java.util.Map;

import nexj.core.meta.Primitive;

/**
 * Connection decorator for intercepting SQL statements.
 */
public final class SQLHookConnection implements Connection
{
   // constants

   /**
    * Prefix for logging transformed SQL.
    */
   public final static String LOG_PREFIX = "/* SQLHook */ ";

   // associations

   /**
    * The decorated connection.
    */
   protected Connection m_con;

   /**
    * The SQL hook.
    */
   protected SQLHook m_hook;

   /**
    * The SQL logger.
    */
   protected SQLLogger m_logger;

   // constructors

   /**
    * Constructs the connection.
    * @param con The connection to decorate.
    * @param hook The SQL hook.
    */
   public SQLHookConnection(Connection con, SQLHook hook, SQLLogger logger)
   {
      m_con = con;
      m_hook = hook;
      m_logger = logger;
   }

   // operations
   
   /**
    * @return The decorated connection.
    */
   public Connection getConnection()
   {
      return m_con;
   }

   /**
    * @return The SQL hook.
    */
   public SQLHook getHook()
   {
      return m_hook;
   }

   /**
    * @return The SQL logger.
    */
   public SQLLogger getLogger()
   {
      return m_logger;
   }
   
   /**
    * Inspects the SQL statement through the hook and logs the new statement.
    * @param sSQL The original SQL statement.
    * @return The new SQL statement, or null.
    */
   protected String inspect(String sSQL)
   {
      String sNewSQL = m_hook.inspect(sSQL);

      if (m_logger.isLogging() && sNewSQL != null && !sNewSQL.equals(sSQL))
      {
         m_logger.log(LOG_PREFIX + sNewSQL);
      }

      return sNewSQL;
   }

   /**
    * @see java.sql.Connection#clearWarnings()
    */
   public void clearWarnings() throws SQLException
   {
      m_con.clearWarnings();
   }

   /**
    * @see java.sql.Connection#close()
    */
   public void close() throws SQLException
   {
      m_con.close();
   }

   /**
    * @see java.sql.Connection#commit()
    */
   public void commit() throws SQLException
   {
      m_con.commit();
   }

   /**
    * @see java.sql.Connection#createStatement()
    */
   public Statement createStatement() throws SQLException
   {
      return new SQLHookStatement(this, m_con.createStatement());
   }

   /**
    * @see java.sql.Connection#createStatement(int, int, int)
    */
   public Statement createStatement(int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability) throws SQLException
   {
      return new SQLHookStatement(this, m_con.createStatement(nResultSetType, nResultSetConcurrency, nResultSetHoldability));
   }

   /**
    * @see java.sql.Connection#createStatement(int, int)
    */
   public Statement createStatement(int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      return new SQLHookStatement(this, m_con.createStatement(nResultSetType, nResultSetConcurrency));
   }

   /**
    * @see java.sql.Connection#getAutoCommit()
    */
   public boolean getAutoCommit() throws SQLException
   {
      return m_con.getAutoCommit();
   }

   /**
    * @see java.sql.Connection#getCatalog()
    */
   public String getCatalog() throws SQLException
   {
      return m_con.getCatalog();
   }

   /**
    * @see java.sql.Connection#getHoldability()
    */
   public int getHoldability() throws SQLException
   {
      return m_con.getHoldability();
   }

   /**
    * @see java.sql.Connection#getMetaData()
    */
   public DatabaseMetaData getMetaData() throws SQLException
   {
      return m_con.getMetaData();
   }

   /**
    * @see java.sql.Connection#getTransactionIsolation()
    */
   public int getTransactionIsolation() throws SQLException
   {
      return m_con.getTransactionIsolation();
   }

   /**
    * @see java.sql.Connection#getTypeMap()
    */
   public Map getTypeMap() throws SQLException
   {
      return m_con.getTypeMap();
   }

   /**
    * @see java.sql.Connection#getWarnings()
    */
   public SQLWarning getWarnings() throws SQLException
   {
      return m_con.getWarnings();
   }

   /**
    * @see java.sql.Connection#isClosed()
    */
   public boolean isClosed() throws SQLException
   {
      return m_con.isClosed();
   }

   /**
    * @see java.sql.Connection#isReadOnly()
    */
   public boolean isReadOnly() throws SQLException
   {
      return m_con.isReadOnly();
   }

   /**
    * @see java.sql.Connection#nativeSQL(java.lang.String)
    */
   public String nativeSQL(String sSQL) throws SQLException
   {
      return m_con.nativeSQL(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
    */
   public CallableStatement prepareCall(String sSQL, int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareCall(s, nResultSetType, nResultSetConcurrency, nResultSetHoldability);
      }

      throw new SQLException("SQLHook.inspect() must not return null for callable statements");
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
    */
   public CallableStatement prepareCall(String sSQL, int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareCall(s, nResultSetType, nResultSetConcurrency);
      }

      throw new SQLException("SQLHook.inspect() must not return null for callable statements");
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String)
    */
   public CallableStatement prepareCall(String sSQL) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareCall(s);
      }

      throw new SQLException("SQLHook.inspect() must not return null for callable statements");
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareStatement(s, nResultSetType, nResultSetConcurrency, nResultSetHoldability);
      }

      return new SQLHookStatement(this, sSQL, nResultSetType, nResultSetConcurrency, nResultSetHoldability);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareStatement(s, nResultSetType, nResultSetConcurrency);
      }

      return new SQLHookStatement(this, sSQL, nResultSetType, nResultSetConcurrency, 0);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int nAutoGeneratedKeys) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareStatement(s, nAutoGeneratedKeys);
      }

      return new SQLHookStatement(this, sSQL, Primitive.createInteger(nAutoGeneratedKeys)); 
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
    */
   public PreparedStatement prepareStatement(String sSQL, int[] nColumnIndexArray) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareStatement(s, nColumnIndexArray);
      }

      return new SQLHookStatement(this, sSQL, nColumnIndexArray); 
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
    */
   public PreparedStatement prepareStatement(String sSQL, String[] sColumnNameArray) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareStatement(s, sColumnNameArray);
      }

      return new SQLHookStatement(this, sSQL, sColumnNameArray); 
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String)
    */
   public PreparedStatement prepareStatement(String sSQL) throws SQLException
   {
      String s = inspect(sSQL);

      if (s != null)
      {
         return m_con.prepareStatement(s);
      }

      return new SQLHookStatement(this, sSQL, 0, 0, 0);
   }

   /**
    * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
    */
   public void releaseSavepoint(Savepoint sSavepoint) throws SQLException
   {
      m_con.releaseSavepoint(sSavepoint);
   }

   /**
    * @see java.sql.Connection#rollback()
    */
   public void rollback() throws SQLException
   {
      m_con.rollback();
   }

   /**
    * @see java.sql.Connection#rollback(java.sql.Savepoint)
    */
   public void rollback(Savepoint sSavepoint) throws SQLException
   {
      m_con.rollback(sSavepoint);
   }

   /**
    * @see java.sql.Connection#setAutoCommit(boolean)
    */
   public void setAutoCommit(boolean bAutoCommit) throws SQLException
   {
      m_con.setAutoCommit(bAutoCommit);
   }

   /**
    * @see java.sql.Connection#setCatalog(java.lang.String)
    */
   public void setCatalog(String sCatalog) throws SQLException
   {
      m_con.setCatalog(sCatalog);
   }

   /**
    * @see java.sql.Connection#setHoldability(int)
    */
   public void setHoldability(int nHoldability) throws SQLException
   {
      m_con.setHoldability(nHoldability);
   }

   /**
    * @see java.sql.Connection#setReadOnly(boolean)
    */
   public void setReadOnly(boolean nReadOnly) throws SQLException
   {
      m_con.setReadOnly(nReadOnly);
   }

   /**
    * @see java.sql.Connection#setSavepoint()
    */
   public Savepoint setSavepoint() throws SQLException
   {
      return m_con.setSavepoint();
   }

   /**
    * @see java.sql.Connection#setSavepoint(java.lang.String)
    */
   public Savepoint setSavepoint(String sName) throws SQLException
   {
      return m_con.setSavepoint(sName);
   }

   /**
    * @see java.sql.Connection#setTransactionIsolation(int)
    */
   public void setTransactionIsolation(int nLevel) throws SQLException
   {
      m_con.setTransactionIsolation(nLevel);
   }

   /**
    * @see java.sql.Connection#setTypeMap(java.util.Map)
    */
   public void setTypeMap(Map map) throws SQLException
   {
      m_con.setTypeMap(map);
   }
}
