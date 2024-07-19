// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;

import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.persistence.Field;
import nexj.core.persistence.GenericCursor;
import nexj.core.persistence.OID;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.persistence.sql.SQLAdapter.Bind;
import nexj.core.util.Logger;

/**
 * Cursor for the SQL adapter.
 */
public class SQLCursor extends GenericCursor
{
   // associations

   /**
    * The SQL adapter that has opened the cursor.
    */
   protected SQLAdapter m_adapter;

   /**
    * The SQL statement generator.
    */
   protected SQLGenerator m_generator;

   /**
    * The SQL connection resource.
    */
   protected SQLConnection m_connection;

   /**
    * The open result set.
    */
   protected ResultSet m_rs;

   /**
    * The temporary result set value array needed for working around buggy
    * drivers requiring specific order of value retrieval.
    */
   protected Object[] m_valueArray;

   /**
    * The deferred log list.
    */
   protected List m_logList;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(SQLAdapter.class);

   /**
    * The long-running query logger.
    */
   private final static Logger s_lrqLogger = Logger.getLogger(SQLAdapter.class.getName() + ".LRQ");

   // constructors

   /**
    * Constructs the cursor and allocates the necessary resources.
    * @param adapter The SQL adapter.
    * @param query The query.
    */
   public SQLCursor(Query query)
   {
      super(query);
   }

   // operations

   /**
    * @see nexj.core.persistence.GenericCursor#getType()
    */
   protected String getType()
   {
      return "SQL";
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getLogger()
    */
   protected Logger getLogger()
   {
      return s_logger;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getLRQLogger()
    */
   protected Logger getLRQLogger()
   {
      return s_lrqLogger;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#init(nexj.core.meta.persistence.DataSourceFragment)
    */
   protected void init(DataSourceFragment fragment)
   {
      RelationalDatabaseFragment dbf = (RelationalDatabaseFragment)fragment;

      if (m_nTimeout < 0)
      {
         m_nTimeout = dbf.getQueryTimeout();
      }

      m_lWarningTimeout = (m_query.isLocking()) ? 0 : dbf.getWarningTimeout();
      m_adapter = (SQLAdapter)m_query.getAdapter();
      m_logList = null;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#logQuery(int)
    */
   protected void logQuery(int nLevel)
   {
      m_adapter.log(m_logger, nLevel, m_logList);
   }

   /**
    * @see nexj.core.persistence.GenericCursor#query()
    */
   protected void query() throws PersistenceException
   {
      m_generator = (SQLGenerator)m_query.getGenerator();
      m_generator.clearBinds(); // Reset existing bind count as we're generating a new statement
      m_generator.generateReadSQL();

      boolean bLogDeferredSaved = m_adapter.isLogDeferred();

      m_adapter.clearLog();
      m_adapter.setLogDeferred(true);
      m_generator.log();

      PreparedStatement stmt = null;
      long lStartTime = 0;

      try
      {
         // for grouping queries then there might already be a ResultSet with an unclosed statement
         closeStatement();

         // We might already have a connection from previous ResultSet
         if (m_connection == null)
         {
            m_connection = m_adapter.getConnection(
               (RelationalDatabaseFragment)m_query.getFragment(),
               m_adapter.isSharedConnection(m_query));
         }

         lStartTime = getCurrentTime();
         stmt = m_adapter.prepareStatement(m_connection.getConnection(), m_generator.getSQL(), m_query);

         for (int i = 0, n = m_generator.getBindCount(); i != n; ++i)
         {
            m_generator.getBind(i).setValue(stmt, i, m_generator.getBindValue(i), m_adapter);
         }

         m_adapter.setFetchSize(stmt, m_query);

         if (m_nTimeout > 0)
         {
            stmt.setQueryTimeout(m_nTimeout);
         }

         m_rs = m_adapter.executeQuery(stmt);
         m_logList = m_adapter.getLog();
         m_adapter.setLogDeferred(bLogDeferredSaved);

         logDuration(lStartTime, true);

         if (m_adapter.isOrderedColumnFetch())
         {
            m_valueArray = new Object[m_generator.getOutputFieldCount()];
         }
         else
         {
            m_valueArray = null;
         }
      }
      catch (SQLException e)
      {
         m_adapter.setLogDeferred(bLogDeferredSaved);
         m_adapter.flushLog(e);

         PersistenceException x = m_adapter.getException(e, null, 0, 0);

         if (x instanceof QueryTimeoutException)
         {
            logTimeout(lStartTime, !m_adapter.isLogging());
         }

         throw x;
      }
      finally
      {
         m_adapter.setLogDeferred(bLogDeferredSaved);

         if (m_rs == null && m_connection != null)
         {
            m_adapter.close(stmt);
            m_connection.decRef();
            m_connection = null;
         }

         m_adapter.clearLog();
      }
   }

   /**
    * @see nexj.core.persistence.GenericCursor#fetch()
    */
   protected boolean fetch() throws PersistenceException
   {
      try
      {
         if (m_rs.next())
         {
            if (m_valueArray != null)
            {
               for (int i = 0, n = m_generator.getOutputFieldCount(); i != n; ++i)
               {
                  m_valueArray[i] = ((Bind)m_generator.getOutputField(i).getBind()).getValue(m_rs, i, m_adapter);
               }
            }

            return true;
         }

         return false;
      }
      catch (SQLException e)
      {
         throw m_adapter.getException(e, null, 0, 0);
      }
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getRawValue(nexj.core.persistence.Field)
    */
   protected Object getRawValue(Field field) throws PersistenceException
   {
      if (m_valueArray != null)
      {
         return m_valueArray[field.getOrdinal()];
      }

      try
      {
         return ((Bind)field.getBind()).getValue(m_rs, field.getOrdinal(), m_adapter);
      }
      catch (SQLException e)
      {
         throw m_adapter.getException(e, null, 0, 0);
      }
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getKey(java.lang.Object)
    */
   protected OID getKey(Object item)
   {
      return getKey((Field[])item);
   }

   /**
    * Release the existing open ResultSet and its Statement.
    */
   protected void closeStatement()
   {
      if (m_rs == null)
      {
         return; // no statement to release
      }

      Statement stmt = null;

      try
      {
         stmt = m_rs.getStatement();
      }
      catch (SQLException e)
      {
      }

      m_adapter.close(m_rs);
      m_adapter.close(stmt);
      m_rs = null;
   }

   /**
    * Releases the SQL connection.
    */
   protected void disconnect()
   {
      if (m_rs != null)
      {
         closeStatement();

         if (m_connection != null)
         {
            m_connection.decRef();
            m_connection = null;
         }

         m_adapter = null;
         m_generator = null;
         m_valueArray = null;
      }
   }
}
