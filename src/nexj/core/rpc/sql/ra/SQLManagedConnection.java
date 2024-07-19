// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.sql.ra;

import java.io.InputStream;
import java.io.Reader;
import java.lang.ref.SoftReference;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.Date;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.resource.ResourceException;
import javax.resource.spi.CommException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.security.auth.Subject;
import javax.sql.XAConnection;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.rpc.ra.SharedManagedConnection;
import nexj.core.util.HashHolder;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Logger;
import nexj.core.util.LookupDeque;
import nexj.core.util.ObjUtil;
import nexj.core.util.sql.StatementWrapper;

/**
 * An SQL XA Connection wrapper.
 */
public class SQLManagedConnection extends SharedManagedConnection
{
   // constants

   /**
    * The percent amount of cache to clear every time GC requests memory (0..100].
    * Default to every GC event causes 50% of the cached statements to be closed.
    */
   protected final static int CACHE_FREE_THRESHOLD = 50;

   // attributes

   /**
    * Total number of active statements (should be kept in sync with m_cacheStatementMap counts).
    * Count is decremented when statements are put back into the statement cache.
    */
   protected int m_nStatementActiveCount = 0;

   /**
    * Total number of cached statements (should be kept in sync with m_cacheStatementMap counts).
    */
   protected int m_nStatementCachedCount = 0;

   /**
    * Garbage collection event listener used for clearing a portion of the cache.
    */
   protected SoftReference m_cacheFreeTrigger;

   // associations

   /**
    * Cache of SQL to unused previously created non-wrapped PreparedStatements (without arguments).
    * Only cache a statement once it's deemed closed otherwise can potentially set parameters into
    * the same statement from different references.
    * The value is either CacheDecrementer<Statement>
    * or List<CacheDecrementer<Statement>>.
    */
   protected LookupDeque/*<String, Object>*/ m_cachedStatementMap =
      new LinkedHashTab/*<String, Object>*/();

   /**
    * The connection request information used for connection identification, e.g. matches().
    */
   protected SQLConnectionRequestInfo m_conInfo;

   /**
    * The connection Factory containing the configuration parameters.
    */
   protected SQLManagedConnectionFactory m_factory;

   /**
    * Wrapped XAResource that can tell if it has a transaction. (lazy init)
    */
   protected XAResourceWrapper m_resource;

   /**
    * The connection handle for the XAConnection (referenced by SQLConnection). (lazy init)
    * Use single java.sql.Connection handle because some DBs (e.g. DB2) have problems dealing with
    * multiple handles to the same connection.
    */
   protected Connection m_sqlConnection;

   /**
    * The XA connection to wrap.
    * This will be set to null when destroy() is called and connection closed.
    */
   protected XAConnection m_xaConnection;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SQLManagedConnection.class);

   // constructors

   /**
    * Constructor.
    * @param connection The managed connection that will provide handles (not null).
    * @param cfg The configuration used for connection creation, for SQL init/test and isolation.
    * @param info The connection request information used for connection identification, matches().
    * @throws SQLException If an error occurs in establishing a connection handle.
    */
   public SQLManagedConnection(XAConnection connection, SQLManagedConnectionFactory factory,
      SQLConnectionRequestInfo info) throws SQLException
   {
      assert connection != null;
      assert factory != null;
      assert info != null;

      m_xaConnection = connection;
      m_factory = factory;
      m_conInfo = info;
      m_sqlConnection = m_xaConnection.getConnection();
      m_sqlConnection.setAutoCommit(true); // same as SQLXAConnectionFactory for non-tx connections
      m_sqlConnection.setTransactionIsolation(m_factory.m_nIsolationLevel);

      if (m_factory.getInitialSQL() != null) // execute initialization SQL if provided
      {
         Statement stmt = null;

         try
         {
            stmt = m_sqlConnection.createStatement();
            stmt.execute(m_factory.m_sInitialSQL);
            stmt.close();
         }
         finally
         {
            if (stmt != null)
            {
               stmt.close();
            }
         }
      }
   }

   /**
    * Cache a statement.
    * @param sKey The key to cache the statement with (null == close statement).
    * @param stmt The statement to cache (not null).
    * @throws SQLException On LRU update failure.
    */
   protected void cacheStatement(String sKey, Statement stmt) throws SQLException
   {
      assert stmt != null; // simplification invariant

      synchronized (m_cachedStatementMap)
      {
         cancelStatementCapacity(1, null); // decrement active statement count

         if (sKey == null) // non-cacheable statement
         {
            stmt.close();

            return;
         }

         stmt.clearBatch(); // free no longer relevant memory
         stmt.clearWarnings(); // free no longer relevant memory

         if (stmt instanceof PreparedStatement)
         {
            ((PreparedStatement)stmt).clearParameters(); // free no longer relevant memory
         }

         Object cached = m_cachedStatementMap.remove(sKey);

         if (cached instanceof Statement)
         {
            List/*<Statement>*/ list = new ArrayList/*<Statement>*/(2);

            list.add(cached);
            list.add(stmt);
            cached = list;
         }
         else if (cached instanceof List)
         {
            ((List)cached).add(stmt);
         }
         else
         {
            cached = stmt;
         }

         m_cachedStatementMap.putFirst(sKey, cached);
         ++m_nStatementCachedCount;
         freeCacheLRU(m_nStatementActiveCount + m_nStatementCachedCount -
                      m_factory.getStatementCacheSize()); // clear overflow

         // this is required invariant condition
         assert m_nStatementActiveCount >= 0 && m_nStatementCachedCount >= 0 &&
            m_nStatementActiveCount + m_nStatementCachedCount < m_factory.getStatementCacheSize();

         if (m_nStatementCachedCount > 0 && m_cacheFreeTrigger == null)
         {
            m_cacheFreeTrigger = new SoftReference(new CacheCleanupTrigger());
         }
         else if (m_cacheFreeTrigger != null && m_cacheFreeTrigger.get() == null)
         {
            // GC requested memory release which has not closed any statements from cache yet
            freeCacheLRU((int)(((long)m_nStatementCachedCount * CACHE_FREE_THRESHOLD + 99) / 100));
            m_cacheFreeTrigger = new SoftReference(new CacheCleanupTrigger());
         }
      }
   }

   /**
    * Release previously statement capacity of nDelta and rethrow the error as SQLException or
    * RuntimeException.
    * @param nDelta The number of reserved statements for which to cancel the reservation.
    * @param t The error to rethrow (null == do not rethrow and return null).
    * @return t IFF t instanceof SQLException || t==null, else t is rethrown via ObjUtil.rethrow(t).
    */
   protected SQLException cancelStatementCapacity (int nDelta, Throwable t)
   {
      m_nStatementActiveCount -= Math.max(0, nDelta);
      assert m_nStatementActiveCount >= 0;

      if (t == null || t instanceof SQLException)
      {
         return (SQLException)t;
      }

      throw ObjUtil.rethrow(t);
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#cleanup()
    */
   public synchronized void cleanup() throws ResourceException
   {
      try
      {
         super.cleanup();
      }
      finally
      {
         assert m_nStatementActiveCount == 0; // there must be no statements on a free connection

         try
         {
            if (m_factory.isAutoCommitRollbackRequired() && m_sqlConnection.getAutoCommit())
            {
               m_sqlConnection.rollback();
            }
         }
         catch (SQLException e)
         {
            throw (e.getCause() instanceof ResourceException)
               ? (ResourceException)e.getCause()
               : new ResourceException(e.getLocalizedMessage(), e);
         }
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#createConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   protected GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri)
      throws ResourceException
   {
      try
      {
         // DB2 seems to close connection if SQL Error encountered, but doesn't notify
         if (m_xaConnection == null || m_sqlConnection.isClosed()) // closed connection
         {
            throw new CommException("RDBMS connection is already closed");
         }
      }
      catch (SQLException e)
      {
         throw new CommException("Unexpected RDBMS connection error", e);
      }

      if (m_handleSet.isEmpty() && // no active handles or XA transactions, safe to test
          (m_resource == null || !m_resource.isBusy()))
      {
         Statement stmt = null;

         try
         {
            if (m_factory.isAutoCommitRollbackRequired() || !m_sqlConnection.getAutoCommit())
            {
               m_sqlConnection.rollback(); // roll back any local transactions
            }

            if (m_factory.getTestSQL() != null) // execute test SQL if provided
            {
               m_sqlConnection.setAutoCommit(true);
               stmt = m_sqlConnection.createStatement();
               stmt.execute(m_factory.getTestSQL());
               stmt.close();
            }
         }
         catch (SQLException e)
         {
            if (stmt != null)
            {
               try
               {
                  stmt.close();
               }
               catch (SQLException f)
               {
                  // ignore since closing connection
               }
            }

            try
            {
               destroy();
            }
            catch (ResourceException f)
            {
               // ignore since looping around
            }

            throw new CommException("Connection test failed", e);
         }
      }

      return new SQLConnection(); // the empty handle will be told its managed connection
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#createStatement()
    */
   protected Statement createStatement(Connection con) throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLStatement(m_sqlConnection.createStatement(), con);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#createStatement(int, int)
    */
   protected Statement createStatement(
      Connection con, int nResultSetType, int nResultSetConcurrency) throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLStatement(
            m_sqlConnection.createStatement(nResultSetType, nResultSetConcurrency), con);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#createStatement(int, int, int)
    */
   protected Statement createStatement(
      Connection con, int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability)
      throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLStatement(
            m_sqlConnection.createStatement(
               nResultSetType, nResultSetConcurrency, nResultSetHoldability),
            con);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#destroy()
    */
   public void destroy() throws ResourceException
   {
      try
      {
         super.destroy();
      }
      finally
      {
         // double-close happens when GenericConnectionManager.clear() clears its pool and then the
         // SQLManagedConnectionFactory.dispose() destroys all of its created connections
         if (m_xaConnection == null)
         {
            return; // NOOP, prevent double-close for drivers that cannot handle it, e.g. JTDS
         }

         synchronized (m_cachedStatementMap)
         {
            assert m_nStatementActiveCount == 0; //there must be no statements on a free connection
            freeCacheLRU(m_nStatementCachedCount);
         }

         try
         {
            m_sqlConnection.close();
         }
         catch (SQLException e) // ignore exception because physical connection will be closed next
         {
         }

         try
         {
            m_xaConnection.close();
            m_xaConnection = null; // mark as closed since JTDS cannot handle double-close
         }
         catch (SQLException e)
         {
            throw (e.getCause() instanceof ResourceException)
               ? (ResourceException)e.getCause()
               : new ResourceException(e.getLocalizedMessage(), e);
         }
      }
   }

   /**
    * Close/remove at least nCount statements from cache, up to maximum cached.
    * This function should be synchronized around m_cacheStatementMap.
    * Close errors are ignored.
    * @param nCount The minimum number of statements to free from the cache.
    */
   protected void freeCacheLRU(int nCount)
   {
      // clear a full key at a time for efficiency
      while (nCount > 0 && m_cachedStatementMap.lastKey() != null)
      {
         Object cached = m_cachedStatementMap.removeLast(); // remove value for retrieved key

         if (cached instanceof Statement)
         {
            try
            {
               ((Statement)cached).close(); // it is an error for a null statement to be cached
            }
            catch (SQLException e)
            {
            }

            --m_nStatementCachedCount;
            --nCount;
         }
         else if (cached instanceof List)
         {
            for (int i = ((List)cached).size() - 1; i >= 0; --i)
            {
               Statement stmt = (Statement)((List)cached).remove(i);

               if (stmt != null) // null statements were removed from cache on getCachedStatement()
               {
                  try
                  {
                     stmt.close();
                  }
                  catch (SQLException e)
                  {
                  }

                  --m_nStatementCachedCount;
                  --nCount;
               }
            }
         }
      }
   }

   /**
    * Retrieve/remove a Statement from the cache (reserveStatementCapacity() must have been called before).
    * For internal use only.
    * @param sKey The statement key to use for finding the appropriate statement (not null).
    * @param type The class the statement must match (not null).
    * @return A cached statement or null if none available.
    */
   protected Statement getCachedStatement(String sKey, Class type)
   {
      synchronized (m_cachedStatementMap)
      {
         Object cached = m_cachedStatementMap.get(sKey);

         if (type.isInstance(cached))
         {
            m_cachedStatementMap.remove(sKey);
            --m_nStatementCachedCount;

            return (Statement)cached;
         }

         if (cached instanceof List) // empty lists fill up once statements return to cache
         {
            for (int i = ((List)cached).size() - 1; i >= 0; --i)
            {
               if (type.isInstance(((List)cached).get(i)))
               {
                  --m_nStatementCachedCount;

                  return (Statement)((List)cached).remove(i);
               }
            }
         }
      }

      return null; // nothing in cache
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#getXAResource()
    */
   public XAResource getXAResource() throws ResourceException
   {
      if (m_resource == null)
      {
         try
         {
            m_resource = new XAResourceWrapper(m_xaConnection.getXAResource());
         }
         catch (SQLException e)
         {
            throw new ResourceException(e);
         }
      }

      return m_resource;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#matches(javax.security.auth.Subject, nexj.core.rpc.ra.GenericConnectionRequestInfo)
    */
   public boolean matches(Subject subject, GenericConnectionRequestInfo cri)
   {
      // check if should reject due to being non-sharable and having to share
      if (!(cri instanceof SQLConnectionRequestInfo) ||
          (!m_factory.isShared() && !m_handleSet.isEmpty()))
      {
         return false; // one of the configurations is not sharable and this connection has handles
      }

      // compare credentials
      return ObjUtil.equal(m_conInfo.getUser(), ((SQLConnectionRequestInfo)cri).getUser()) &&
         ObjUtil.equal(m_conInfo.getPassword(), ((SQLConnectionRequestInfo)cri).getPassword());
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareCall(java.lang.String)
    */
   protected CallableStatement prepareCall(Connection con, String sSQL) throws SQLException
   {
      CallableStatement stmt =
         (CallableStatement)getCachedStatement(sSQL, CallableStatement.class);

      reserveStatementCapacity(1);

      try
      {
         return new SQLCallableStatement(
            (stmt == null) ? m_sqlConnection.prepareCall(sSQL) : stmt, con, sSQL);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
    */
   protected CallableStatement prepareCall(
      Connection con, String sSQL, int nResultSetType, int nResultSetConcurrency)
      throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLCallableStatement(
            m_sqlConnection.prepareCall(sSQL, nResultSetType, nResultSetConcurrency), con, null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
    */
   protected CallableStatement prepareCall(Connection con, String sSQL,
      int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability)
      throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLCallableStatement(
            m_sqlConnection.prepareCall(
               sSQL, nResultSetType, nResultSetConcurrency, nResultSetHoldability),
            con,
            null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareStatement(java.lang.String)
    */
   protected PreparedStatement prepareStatement(Connection con, String sSQL) throws SQLException
   {
      PreparedStatement stmt =
         (PreparedStatement)getCachedStatement(sSQL, PreparedStatement.class);

      reserveStatementCapacity(1);

      try
      {
         return new SQLPreparedStatement(
            (stmt == null) ? m_sqlConnection.prepareStatement(sSQL) : stmt, con, sSQL);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareStatement(java.lang.String, int)
    */
   protected PreparedStatement prepareStatement(
      Connection con, String sSQL, int nAutoGeneratedKeys) throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLPreparedStatement(
            m_sqlConnection.prepareStatement(sSQL, nAutoGeneratedKeys), con, null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
    */
   protected PreparedStatement prepareStatement(
      Connection con, String sSQL, int[] columnIndexArray) throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLPreparedStatement(
            m_sqlConnection.prepareStatement(sSQL, columnIndexArray), con, null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
    */
   protected PreparedStatement prepareStatement(
      Connection con, String sSQL, String[] columnNameArray) throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLPreparedStatement(
            m_sqlConnection.prepareStatement(sSQL, columnNameArray), con, null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
    */
   protected PreparedStatement prepareStatement(
      Connection con, String sSQL, int nResultSetType, int nResultSetConcurrency)
      throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLPreparedStatement(
            m_sqlConnection.prepareStatement(sSQL, nResultSetType, nResultSetConcurrency),
            con,
            null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * @param con The connection handle that requested this statement.
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
    */
   protected PreparedStatement prepareStatement(Connection con, String sSQL,
      int nResultSetType, int nResultSetConcurrency, int nResultSetHoldability)
      throws SQLException
   {
      reserveStatementCapacity(1);

      try
      {
         return new SQLPreparedStatement(
            m_sqlConnection.prepareStatement(
               sSQL, nResultSetType, nResultSetConcurrency, nResultSetHoldability),
            con,
            null);
      }
      catch (Throwable t)
      {
         throw cancelStatementCapacity(1, t); // reservation no longer required due to exception
      }
   }

   /**
    * Ensure that the connection can hold an extra nDelta statements.
    * For internal use only.
    * The requested number of statements is reserved in the cache.
    * The reservation is freed by returning statements to cache.
    * @param nDelta The number of extra statements that this connections should be able to hold.
    */
   protected void reserveStatementCapacity(int nDelta)
   {
      m_nStatementActiveCount += Math.max(0, nDelta);

      synchronized (m_cachedStatementMap)
      {
         if (m_nStatementActiveCount + m_nStatementCachedCount + nDelta >
             m_factory.getStatementCacheSize())
         {
            freeCacheLRU(nDelta);
         }
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(ObjUtil.getShortClassName(this));
      buf.append('@');
      buf.append(System.identityHashCode(this));
      buf.append("(user=");
      buf.append((m_conInfo == null) ? null : m_conInfo.getUser());
      buf.append(", factory=");
      buf.append(m_factory);
      buf.append(')');

      return buf.toString();
   }

   // inner classes

   /**
    * Class to trigger cache cleanup
    */
   protected class CacheCleanupTrigger
   {
      /**
       * @see java.lang.Object#finalize()
       */
      protected void finalize() throws Throwable
      {
         synchronized(m_cachedStatementMap)
         {
            freeCacheLRU((int)(((long)m_nStatementCachedCount * CACHE_FREE_THRESHOLD + 99) / 100));
            m_cacheFreeTrigger = (m_nStatementCachedCount <= 0)
                               ? null : new SoftReference(new CacheCleanupTrigger());
         }

         super.finalize();
      }
   }

   /**
    * CallableStatement Wrapper.
    */
   protected class SQLCallableStatement extends SQLPreparedStatement implements CallableStatement
   {
      /**
       * The wrapped statement.
       */
      protected CallableStatement m_stmt;

      /**
       * Constructor.
       * @param stmt The statement to wrap (not null).
       * @param connection The connection handle that requested this statement.
       * @param sKey This statement should be cached with this key on close (null == no caching).
       */
      public SQLCallableStatement(CallableStatement stmt, Connection connection, String sKey)
      {
         super(stmt, connection, sKey);

         m_stmt = stmt;
      }

      /**
       * @see nexj.core.rpc.sql.ra.SQLConnection.SQLPreparedStatement#close()
       */
      public void close() throws SQLException
      {
         super.close();
         m_stmt = null;
      }

      /**
       * @see java.sql.CallableStatement#getArray(int)
       */
      public Array getArray(int nParameterIndex) throws SQLException
      {
         return m_stmt.getArray(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getArray(java.lang.String)
       */
      public Array getArray(String sParameterName) throws SQLException
      {
         return m_stmt.getArray(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getBigDecimal(int)
       */
      public BigDecimal getBigDecimal(int nParameterIndex) throws SQLException
      {
         return m_stmt.getBigDecimal(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getBigDecimal(java.lang.String)
       */
      public BigDecimal getBigDecimal(String sParameterName) throws SQLException
      {
         return m_stmt.getBigDecimal(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getBigDecimal(int, int)
       * @deprecated
       */
      public BigDecimal getBigDecimal(int nParameterIndex, int nScale) throws SQLException
      {
         return m_stmt.getBigDecimal(nParameterIndex, nScale);
      }

      /**
       * @see java.sql.CallableStatement#getBlob(int)
       */
      public Blob getBlob(int nParameterIndex) throws SQLException
      {
         return m_stmt.getBlob(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getBlob(java.lang.String)
       */
      public Blob getBlob(String sParameterName) throws SQLException
      {
         return m_stmt.getBlob(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getBoolean(int)
       */
      public boolean getBoolean(int nParameterIndex) throws SQLException
      {
         return m_stmt.getBoolean(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getBoolean(java.lang.String)
       */
      public boolean getBoolean(String sParameterName) throws SQLException
      {
         return m_stmt.getBoolean(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getByte(int)
       */
      public byte getByte(int nParameterIndex) throws SQLException
      {
         return m_stmt.getByte(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getByte(java.lang.String)
       */
      public byte getByte(String sParameterName) throws SQLException
      {
         return m_stmt.getByte(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getBytes(int)
       */
      public byte[] getBytes(int nParameterIndex) throws SQLException
      {
         return m_stmt.getBytes(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getBytes(java.lang.String)
       */
      public byte[] getBytes(String sParameterName) throws SQLException
      {
         return m_stmt.getBytes(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getClob(int)
       */
      public Clob getClob(int nParameterIndex) throws SQLException
      {
         return m_stmt.getClob(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getClob(java.lang.String)
       */
      public Clob getClob(String sParameterName) throws SQLException
      {
         return m_stmt.getClob(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getDate(int)
       */
      public Date getDate(int nParameterIndex) throws SQLException
      {
         return m_stmt.getDate(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getDate(java.lang.String)
       */
      public Date getDate(String sParameterName) throws SQLException
      {
         return m_stmt.getDate(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getDate(int, java.util.Calendar)
       */
      public Date getDate(int nParameterIndex, Calendar cal) throws SQLException
      {
         return m_stmt.getDate(nParameterIndex, cal);
      }

      /**
       * @see java.sql.CallableStatement#getDate(java.lang.String, java.util.Calendar)
       */
      public Date getDate(String sParameterName, Calendar cal) throws SQLException
      {
         return m_stmt.getDate(sParameterName, cal);
      }

      /**
       * @see java.sql.CallableStatement#getDouble(int)
       */
      public double getDouble(int nParameterIndex) throws SQLException
      {
         return m_stmt.getDouble(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getDouble(java.lang.String)
       */
      public double getDouble(String sParameterName) throws SQLException
      {
         return m_stmt.getDouble(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getFloat(int)
       */
      public float getFloat(int nParameterIndex) throws SQLException
      {
         return m_stmt.getFloat(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getFloat(java.lang.String)
       */
      public float getFloat(String sParameterName) throws SQLException
      {
         return m_stmt.getFloat(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getInt(int)
       */
      public int getInt(int nParameterIndex) throws SQLException
      {
         return m_stmt.getInt(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getInt(java.lang.String)
       */
      public int getInt(String sParameterName) throws SQLException
      {
         return m_stmt.getInt(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getLong(int)
       */
      public long getLong(int nParameterIndex) throws SQLException
      {
         return m_stmt.getLong(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getLong(java.lang.String)
       */
      public long getLong(String sParameterName) throws SQLException
      {
         return m_stmt.getLong(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getObject(int)
       */
      public Object getObject(int nParameterIndex) throws SQLException
      {
         return m_stmt.getObject(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getObject(java.lang.String)
       */
      public Object getObject(String sParameterName) throws SQLException
      {
         return m_stmt.getObject(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getObject(int, java.util.Map)
       */
      public Object getObject(int nParameterIndex, Map arg1) throws SQLException
      {
         return m_stmt.getObject(nParameterIndex, arg1);
      }

      /**
       * @see java.sql.CallableStatement#getObject(java.lang.String, java.util.Map)
       */
      public Object getObject(String sParameterName, Map arg1) throws SQLException
      {
         return m_stmt.getObject(sParameterName, arg1);
      }

      /**
       * @see java.sql.CallableStatement#getRef(int)
       */
      public Ref getRef(int nParameterIndex) throws SQLException
      {
         return m_stmt.getRef(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getRef(java.lang.String)
       */
      public Ref getRef(String sParameterName) throws SQLException
      {
         return m_stmt.getRef(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getShort(int)
       */
      public short getShort(int nParameterIndex) throws SQLException
      {
         return m_stmt.getShort(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getShort(java.lang.String)
       */
      public short getShort(String sParameterName) throws SQLException
      {
         return m_stmt.getShort(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getString(int)
       */
      public String getString(int nParameterIndex) throws SQLException
      {
         return m_stmt.getString(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getString(java.lang.String)
       */
      public String getString(String sParameterName) throws SQLException
      {
         return m_stmt.getString(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getTime(int)
       */
      public Time getTime(int nParameterIndex) throws SQLException
      {
         return m_stmt.getTime(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getTime(java.lang.String)
       */
      public Time getTime(String sParameterName) throws SQLException
      {
         return m_stmt.getTime(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getTime(int, java.util.Calendar)
       */
      public Time getTime(int nParameterIndex, Calendar cal) throws SQLException
      {
         return m_stmt.getTime(nParameterIndex, cal);
      }

      /**
       * @see java.sql.CallableStatement#getTime(java.lang.String, java.util.Calendar)
       */
      public Time getTime(String sParameterName, Calendar cal) throws SQLException
      {
         return m_stmt.getTime(sParameterName, cal);
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(int)
       */
      public Timestamp getTimestamp(int nParameterIndex) throws SQLException
      {
         return m_stmt.getTimestamp(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(java.lang.String)
       */
      public Timestamp getTimestamp(String sParameterName) throws SQLException
      {
         return m_stmt.getTimestamp(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(int, java.util.Calendar)
       */
      public Timestamp getTimestamp(int nParameterIndex, Calendar cal) throws SQLException
      {
         return m_stmt.getTimestamp(nParameterIndex, cal);
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(java.lang.String, java.util.Calendar)
       */
      public Timestamp getTimestamp(String sParameterName, Calendar cal) throws SQLException
      {
         return m_stmt.getTimestamp(sParameterName, cal);
      }

      /**
       * @see java.sql.CallableStatement#getURL(int)
       */
      public URL getURL(int nParameterIndex) throws SQLException
      {
         return m_stmt.getURL(nParameterIndex);
      }

      /**
       * @see java.sql.CallableStatement#getURL(java.lang.String)
       */
      public URL getURL(String sParameterName) throws SQLException
      {
         return m_stmt.getURL(sParameterName);
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(int, int)
       */
      public void registerOutParameter(int nParameterIndex, int nSQLType) throws SQLException
      {
         m_stmt.registerOutParameter(nParameterIndex, nSQLType);
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(java.lang.String, int)
       */
      public void registerOutParameter(String sParameterName, int nSQLType) throws SQLException
      {
         m_stmt.registerOutParameter(sParameterName, nSQLType);
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(int, int, int)
       */
      public void registerOutParameter(int nParameterIndex, int nSQLType, int nScale) throws SQLException
      {
         m_stmt.registerOutParameter(nParameterIndex, nSQLType, nScale);
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(int, int, java.lang.String)
       */
      public void registerOutParameter(int nParameterIndex, int nSQLType, String sTypeName) throws SQLException
      {
         m_stmt.registerOutParameter(nParameterIndex, nSQLType, sTypeName);
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(java.lang.String, int, int)
       */
      public void registerOutParameter(String sParameterName, int nSQLType, int nScale) throws SQLException
      {
         m_stmt.registerOutParameter(sParameterName, nSQLType, nScale);
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(java.lang.String, int, java.lang.String)
       */
      public void registerOutParameter(String sParameterName, int nSQLType, String sTypeName) throws SQLException
      {
         m_stmt.registerOutParameter(sParameterName, nSQLType, sTypeName);
      }

      /**
       * @see java.sql.CallableStatement#setAsciiStream(java.lang.String, java.io.InputStream, int)
       */
      public void setAsciiStream(String sParameterName, InputStream x, int nLength) throws SQLException
      {
         m_stmt.setAsciiStream(sParameterName, x, nLength);
      }

      /**
       * @see java.sql.CallableStatement#setBigDecimal(java.lang.String, java.math.BigDecimal)
       */
      public void setBigDecimal(String sParameterName, BigDecimal x) throws SQLException
      {
         m_stmt.setBigDecimal(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setBinaryStream(java.lang.String, java.io.InputStream, int)
       */
      public void setBinaryStream(String sParameterName, InputStream x, int nLength) throws SQLException
      {
         m_stmt.setBinaryStream(sParameterName, x, nLength);
      }

      /**
       * @see java.sql.CallableStatement#setBoolean(java.lang.String, boolean)
       */
      public void setBoolean(String sParameterName, boolean x) throws SQLException
      {
         m_stmt.setBoolean(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setByte(java.lang.String, byte)
       */
      public void setByte(String sParameterName, byte x) throws SQLException
      {
         m_stmt.setByte(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setBytes(java.lang.String, byte[])
       */
      public void setBytes(String sParameterName, byte[] x) throws SQLException
      {
         m_stmt.setBytes(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setCharacterStream(java.lang.String, java.io.Reader, int)
       */
      public void setCharacterStream(String sParameterName, Reader reader, int nLength) throws SQLException
      {
         m_stmt.setCharacterStream(sParameterName, reader, nLength);
      }

      /**
       * @see java.sql.CallableStatement#setDate(java.lang.String, java.sql.Date)
       */
      public void setDate(String sParameterName, Date x) throws SQLException
      {
         m_stmt.setDate(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setDate(java.lang.String, java.sql.Date, java.util.Calendar)
       */
      public void setDate(String sParameterName, Date x, Calendar cal) throws SQLException
      {
         m_stmt.setDate(sParameterName, x, cal);
      }

      /**
       * @see java.sql.CallableStatement#setDouble(java.lang.String, double)
       */
      public void setDouble(String sParameterName, double x) throws SQLException
      {
         m_stmt.setDouble(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setFloat(java.lang.String, float)
       */
      public void setFloat(String sParameterName, float x) throws SQLException
      {
         m_stmt.setFloat(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setInt(java.lang.String, int)
       */
      public void setInt(String sParameterName, int x) throws SQLException
      {
         m_stmt.setInt(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setLong(java.lang.String, long)
       */
      public void setLong(String sParameterName, long x) throws SQLException
      {
         m_stmt.setLong(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setNull(java.lang.String, int)
       */
      public void setNull(String sParameterName, int nSQLType) throws SQLException
      {
         m_stmt.setNull(sParameterName, nSQLType);
      }

      /**
       * @see java.sql.CallableStatement#setNull(java.lang.String, int, java.lang.String)
       */
      public void setNull(String sParameterName, int nSQLType, String sTypeName) throws SQLException
      {
         m_stmt.setNull(sParameterName, nSQLType, sTypeName);
      }

      /**
       * @see java.sql.CallableStatement#setObject(java.lang.String, java.lang.Object)
       */
      public void setObject(String sParameterName, Object x) throws SQLException
      {
         m_stmt.setObject(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setObject(java.lang.String, java.lang.Object, int)
       */
      public void setObject(String sParameterName, Object x, int nTargetSQLType) throws SQLException
      {
         m_stmt.setObject(sParameterName, x, nTargetSQLType);
      }

      /**
       * @see java.sql.CallableStatement#setObject(java.lang.String, java.lang.Object, int, int)
       */
      public void setObject(String sParameterName, Object x, int nTargetSQLType, int nScale) throws SQLException
      {
         m_stmt.setObject(sParameterName, x, nTargetSQLType, nScale);
      }

      /**
       * @see java.sql.CallableStatement#setShort(java.lang.String, short)
       */
      public void setShort(String sParameterName, short x) throws SQLException
      {
         m_stmt.setShort(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setString(java.lang.String, java.lang.String)
       */
      public void setString(String sParameterName, String x) throws SQLException
      {
         m_stmt.setString(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setTime(java.lang.String, java.sql.Time)
       */
      public void setTime(String sParameterName, Time x) throws SQLException
      {
         m_stmt.setTime(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setTime(java.lang.String, java.sql.Time, java.util.Calendar)
       */
      public void setTime(String sParameterName, Time x, Calendar cal) throws SQLException
      {
         m_stmt.setTime(sParameterName, x, cal);
      }

      /**
       * @see java.sql.CallableStatement#setTimestamp(java.lang.String, java.sql.Timestamp)
       */
      public void setTimestamp(String sParameterName, Timestamp x) throws SQLException
      {
         m_stmt.setTimestamp(sParameterName, x);
      }

      /**
       * @see java.sql.CallableStatement#setTimestamp(java.lang.String, java.sql.Timestamp, java.util.Calendar)
       */
      public void setTimestamp(String sParameterName, Timestamp x, Calendar cal) throws SQLException
      {
         m_stmt.setTimestamp(sParameterName, x, cal);
      }

      /**
       * @see java.sql.CallableStatement#setURL(java.lang.String, java.net.URL)
       */
      public void setURL(String sParameterName, URL val) throws SQLException
      {
         m_stmt.setURL(sParameterName, val);
      }

      /**
       * @see java.sql.CallableStatement#wasNull()
       */
      public boolean wasNull() throws SQLException
      {
         return m_stmt.wasNull();
      }
   }

   /**
    * PreparedStatement Wrapper.
    */
   protected class SQLPreparedStatement extends SQLStatement implements PreparedStatement
   {
      /**
       * The key to cache the statement with upon close (null == no caching).
       */
      protected String m_sKey;

      /**
       * The wrapped statement.
       */
      protected PreparedStatement m_stmt;

      /**
       * Constructor.
       * @param stmt The statement to wrap (not null).
       * @param connection The connection handle that requested this statement.
       * @param sKey This statement should be cached with this key on close (null == no caching).
       */
      public SQLPreparedStatement(PreparedStatement stmt, Connection connection, String sKey)
      {
         super(stmt, connection);

         m_sKey = sKey;
         m_stmt = stmt;
      }

      /**
       * @see java.sql.PreparedStatement#addBatch()
       */
      public void addBatch() throws SQLException
      {
         m_stmt.addBatch();
      }

      /**
       * @see java.sql.PreparedStatement#clearParameters()
       */
      public void clearParameters() throws SQLException
      {
         m_stmt.clearParameters();
      }

      /**
       * @see nexj.core.rpc.sql.ra.SQLConnection.SQLStatement#close()
       */
      public void close() throws SQLException
      {
         if (m_stmt != null) // allow multiple close() calls on statement as per specification
         {
            cacheStatement(m_sKey, m_stmt);
         }

         m_stmt = null;
         m_connection = null;
      }

      /**
       * @see java.sql.PreparedStatement#execute()
       */
      public boolean execute() throws SQLException
      {
         return m_stmt.execute();
      }

      /**
       * @see java.sql.PreparedStatement#executeQuery()
       */
      public ResultSet executeQuery() throws SQLException
      {
         return new SQLResultSet(m_stmt.executeQuery());
      }

      /**
       * @see java.sql.PreparedStatement#executeUpdate()
       */
      public int executeUpdate() throws SQLException
      {
         return m_stmt.executeUpdate();
      }

      /**
       * @see java.sql.PreparedStatement#getMetaData()
       */
      public ResultSetMetaData getMetaData() throws SQLException
      {
         return m_stmt.getMetaData();
      }

      /**
       * @see java.sql.PreparedStatement#getParameterMetaData()
       */
      public ParameterMetaData getParameterMetaData() throws SQLException
      {
         return m_stmt.getParameterMetaData();
      }

      /**
       * @see java.sql.PreparedStatement#setArray(int, java.sql.Array)
       */
      public void setArray(int nParameterIndex, Array x) throws SQLException
      {
         m_stmt.setArray(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setAsciiStream(int, java.io.InputStream, int)
       */
      public void setAsciiStream(int nParameterIndex, InputStream x, int nLength) throws SQLException
      {
         m_stmt.setAsciiStream(nParameterIndex, x, nLength);
      }

      /**
       * @see java.sql.PreparedStatement#setBigDecimal(int, java.math.BigDecimal)
       */
      public void setBigDecimal(int nParameterIndex, BigDecimal x) throws SQLException
      {
         m_stmt.setBigDecimal(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setBinaryStream(int, java.io.InputStream, int)
       */
      public void setBinaryStream(int nParameterIndex, InputStream x, int nLength) throws SQLException
      {
         m_stmt.setBinaryStream(nParameterIndex, x, nLength);
      }

      /**
       * @see java.sql.PreparedStatement#setBlob(int, java.sql.Blob)
       */
      public void setBlob(int nParameterIndex, Blob x) throws SQLException
      {
         m_stmt.setBlob(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setBoolean(int, boolean)
       */
      public void setBoolean(int nParameterIndex, boolean x) throws SQLException
      {
         m_stmt.setBoolean(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setByte(int, byte)
       */
      public void setByte(int nParameterIndex, byte x) throws SQLException
      {
         m_stmt.setByte(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setBytes(int, byte[])
       */
      public void setBytes(int nParameterIndex, byte[] x) throws SQLException
      {
         m_stmt.setBytes(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setCharacterStream(int, java.io.Reader, int)
       */
      public void setCharacterStream(int nParameterIndex, Reader reader, int nLength) throws SQLException
      {
         m_stmt.setCharacterStream(nParameterIndex, reader, nLength);
      }

      /**
       * @see java.sql.PreparedStatement#setClob(int, java.sql.Clob)
       */
      public void setClob(int nParameterIndex, Clob x) throws SQLException
      {
         m_stmt.setClob(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setDate(int, java.sql.Date)
       */
      public void setDate(int nParameterIndex, Date x) throws SQLException
      {
         m_stmt.setDate(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setDate(int, java.sql.Date, java.util.Calendar)
       */
      public void setDate(int nParameterIndex, Date x, Calendar cal) throws SQLException
      {
         m_stmt.setDate(nParameterIndex, x, cal);
      }

      /**
       * @see java.sql.PreparedStatement#setDouble(int, double)
       */
      public void setDouble(int nParameterIndex, double x) throws SQLException
      {
         m_stmt.setDouble(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setFloat(int, float)
       */
      public void setFloat(int nParameterIndex, float x) throws SQLException
      {
         m_stmt.setFloat(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setInt(int, int)
       */
      public void setInt(int nParameterIndex, int x) throws SQLException
      {
         m_stmt.setInt(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setLong(int, long)
       */
      public void setLong(int nParameterIndex, long x) throws SQLException
      {
         m_stmt.setLong(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setNull(int, int)
       */
      public void setNull(int nParameterIndex, int nSQLType) throws SQLException
      {
         m_stmt.setNull(nParameterIndex, nSQLType);
      }

      /**
       * @see java.sql.PreparedStatement#setNull(int, int, java.lang.String)
       */
      public void setNull(int nParameterIndex, int nSQLType, String sTypeName) throws SQLException
      {
         m_stmt.setNull(nParameterIndex, nSQLType,sTypeName);
      }

      /**
       * @see java.sql.PreparedStatement#setObject(int, java.lang.Object)
       */
      public void setObject(int nParameterIndex, Object x) throws SQLException
      {
         m_stmt.setObject(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setObject(int, java.lang.Object, int)
       */
      public void setObject(int nParameterIndex, Object x, int nTargetSQLType) throws SQLException
      {
         m_stmt.setObject(nParameterIndex, x, nTargetSQLType);
      }

      /**
       * @see java.sql.PreparedStatement#setObject(int, java.lang.Object, int, int)
       */
      public void setObject(int nParameterIndex, Object x, int nTargetSQLType, int nScale) throws SQLException
      {
         m_stmt.setObject(nParameterIndex, x, nTargetSQLType, nScale);
      }

      /**
       * @see java.sql.PreparedStatement#setRef(int, java.sql.Ref)
       */
      public void setRef(int nParameterIndex, Ref x) throws SQLException
      {
         m_stmt.setRef(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setShort(int, short)
       */
      public void setShort(int nParameterIndex, short x) throws SQLException
      {
         m_stmt.setShort(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setString(int, java.lang.String)
       */
      public void setString(int nParameterIndex, String x) throws SQLException
      {
         m_stmt.setString(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setTime(int, java.sql.Time)
       */
      public void setTime(int nParameterIndex, Time x) throws SQLException
      {
         m_stmt.setTime(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setTime(int, java.sql.Time, java.util.Calendar)
       */
      public void setTime(int nParameterIndex, Time x, Calendar cal) throws SQLException
      {
         m_stmt.setTime(nParameterIndex, x, cal);
      }

      /**
       * @see java.sql.PreparedStatement#setTimestamp(int, java.sql.Timestamp)
       */
      public void setTimestamp(int nParameterIndex, Timestamp x) throws SQLException
      {
         m_stmt.setTimestamp(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setTimestamp(int, java.sql.Timestamp, java.util.Calendar)
       */
      public void setTimestamp(int nParameterIndex, Timestamp x, Calendar cal) throws SQLException
      {
         m_stmt.setTimestamp(nParameterIndex, x, cal);
      }

      /**
       * @see java.sql.PreparedStatement#setURL(int, java.net.URL)
       */
      public void setURL(int nParameterIndex, URL x) throws SQLException
      {
         m_stmt.setURL(nParameterIndex, x);
      }

      /**
       * @see java.sql.PreparedStatement#setUnicodeStream(int, java.io.InputStream, int)
       * @deprecated
       */
      public void setUnicodeStream(int nParameterIndex, InputStream x, int nLength) throws SQLException
      {
         m_stmt.setUnicodeStream(nParameterIndex, x, nLength);
      }
   }

   /**
    * Statement Wrapper.
    */
   protected class SQLStatement extends StatementWrapper
   {
      // associations

      /**
       * The connection handle that requested this statement.
       */
      protected Connection m_connection;

      // constructors

      /**
       * Constructor.
       * @param stmt The statement to wrap (not null).
       * @param connection The connection handle that requested this statement.
       */
      public SQLStatement(Statement stmt, Connection connection)
      {
         super(stmt);

         m_connection = connection;
      }

      // operations

      /**
       * @see nexj.core.util.sql.StatementWrapper#close()
       */
      public void close() throws SQLException
      {
         if (m_stmt != null) // allow multiple close() calls on statement as per specification
         {
            cacheStatement(null, m_stmt);
         }

         m_stmt = null;
         m_connection = null;
      }

      /**
       * @see nexj.core.util.sql.StatementWrapper#getConnection()
       */
      public Connection getConnection() throws SQLException
      {
         return m_connection; // return the wrapped connection
      }
   }

   /**
    * Wrapper around XAResource that can track transaction start/end.
    */
   protected class XAResourceWrapper implements XAResource
   {
      /**
       * The wrapped resource.
       */
      protected XAResource m_resource;

      /**
       * A set of currently active transactions.
       */
      protected Set/*<Xid>*/ m_transactionSet = new HashHolder/*<Xid>*/(2);

      /**
       * Constructor.
       * @param resource The wrapped resource.
       */
      public XAResourceWrapper(XAResource resource)
      {
         assert resource != null;

         m_resource = resource;
      }

      /**
       * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
       */
      public void commit(Xid xid, boolean bOnePhase) throws XAException
      {
         m_transactionSet.remove(xid); // either committed or rolled back
         m_resource.commit(xid, bOnePhase);
      }

      /**
       * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
       */
      public void end(Xid xid, int nFlags) throws XAException
      {
         if ((nFlags & TMSUSPEND) != 0)
         {
            m_transactionSet.remove(xid); // dissociated from transaction
         }

         m_resource.end(xid, nFlags);
      }

      /**
       * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
       */
      public void forget(Xid xid) throws XAException
      {
         m_transactionSet.remove(xid); // dissociated from transaction
         m_resource.forget(xid);
      }

      /**
       * @see javax.transaction.xa.XAResource#getTransactionTimeout()
       */
      public int getTransactionTimeout() throws XAException
      {
         return m_resource.getTransactionTimeout();
      }

      /**
       * @return If there are still transactions that are tracked by this XAResource.
       */
      protected boolean isBusy()
      {
         return !m_transactionSet.isEmpty();
      }

      /**
       * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
       */
      public boolean isSameRM(XAResource xar) throws XAException
      {
         return m_factory.isSameRMUsed() && m_resource.isSameRM(xar);
      }

      /**
       * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
       */
      public int prepare(Xid xid) throws XAException
      {
         return m_resource.prepare(xid);
      }

      /**
       * @see javax.transaction.xa.XAResource#recover(int)
       */
      public Xid[] recover(int nFlag) throws XAException
      {
         return m_resource.recover(nFlag);
      }

      /**
       * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
       */
      public void rollback(Xid xid) throws XAException
      {
         m_transactionSet.remove(xid); // rolled back
         m_resource.rollback(xid);
      }

      /**
       * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
       */
      public boolean setTransactionTimeout(int nSeconds) throws XAException
      {
         return m_resource.setTransactionTimeout(nSeconds);
      }

      /**
       * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
       */
      public void start(Xid xid, int nFlag) throws XAException
      {
         if ((nFlag & TMJOIN) != 0)
         {
            m_transactionSet.remove(xid); // joining another transaction (should be >0 left)
         }

         m_resource.start(xid, nFlag);

         if ((nFlag & TMJOIN) == 0)
         {
            m_transactionSet.add(xid); // starting or resuming transaction
         }
      }
   }
}