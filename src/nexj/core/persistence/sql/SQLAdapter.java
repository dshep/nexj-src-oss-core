// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DataTruncation;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.TypeConversionException;
import nexj.core.meta.UnaryFunction;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.Schema;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalClassDenorm;
import nexj.core.meta.persistence.sql.RelationalClassMapping;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.RelationalObject;
import nexj.core.meta.persistence.sql.RelationalPrimitiveDenorm;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgrade;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.persistence.AvailabilityException;
import nexj.core.persistence.CalendarFactory;
import nexj.core.persistence.Converter;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.DeadlockException;
import nexj.core.persistence.DuplicateKeyException;
import nexj.core.persistence.Field;
import nexj.core.persistence.GenericPersistenceAdapter;
import nexj.core.persistence.LockTimeoutException;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDGenerator;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.Operator;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.Source;
import nexj.core.persistence.ValueRangeException;
import nexj.core.persistence.Work;
import nexj.core.persistence.operator.AggregateOperator;
import nexj.core.persistence.operator.AttributeOperator;
import nexj.core.persistence.operator.FunctionOperator;
import nexj.core.persistence.operator.InOperator;
import nexj.core.persistence.sql.SQLGenerator.OperatorAppender;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.UnitOfWork;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.Pair;
import nexj.core.util.Binary;
import nexj.core.util.Cancellable;
import nexj.core.util.HashHolder;
import nexj.core.util.KeywordTab;
import nexj.core.util.Logger;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringId;
import nexj.core.util.StringUtil;
import nexj.core.util.TZ;
import nexj.core.util.Undefined;
import nexj.core.util.sql.ConnectionWrapper;

/**
 * SQL persistence adapter.
 */
public abstract class SQLAdapter extends GenericPersistenceAdapter implements Cancellable, SQLLogger
{
   // constants

   /**
    * SQL keywords that have to be quoted (ANSI SQL 2003).
    */
   protected final static KeywordTab KEYWORDS = new KeywordTab(new String[]
   {
      "add", "all", "allocate", "alter", "and", "any", "are", "array", "as",
      "asensitive", "asymmetric", "at", "atomic", "authorization", "begin",
      "between", "bigint", "binary", "blob", "boolean", "both", "by", "call",
      "called", "cascaded", "case", "cast", "char", "character", "check", "clob",
      "close", "collate", "column", "commit", "connect", "constraint", "continue",
      "corresponding", "create", "cross", "cube", "current", "current_date",
      "current_default_transform_group", "current_path", "current_role",
      "current_time", "current_timestamp", "current_transform_group_for_type",
      "current_user", "cursor", "cycle", "date", "day", "deallocate", "dec",
      "decimal", "declare", "default", "delete", "deref", "describe",
      "deterministic", "disconnect", "distinct", "double", "drop", "dynamic", "each",
      "element", "else", "end", "end-exec", "escape", "except", "exec", "execute",
      "exists", "external", "false", "fetch", "filter", "float", "for", "foreign",
      "free", "from", "full", "function", "get", "global", "grant", "group",
      "grouping", "having", "hold", "hour", "identity", "immediate", "in",
      "indicator", "inner", "inout", "input", "insensitive", "insert", "int",
      "integer", "intersect", "interval", "into", "is", "isolation", "join",
      "language", "large", "lateral", "leading", "left", "like", "local",
      "localtime", "localtimestamp", "match", "member", "merge", "method", "minute",
      "modifies", "module", "month", "multiset", "national", "natural", "nchar",
      "nclob", "new", "no", "none", "not", "null", "numeric", "of", "old", "on",
      "only", "open", "or", "order", "out", "outer", "output", "over", "overlaps",
      "parameter", "partition", "precision", "prepare", "primary", "procedure",
      "range", "reads", "real", "recursive", "ref", "references", "referencing",
      "release", "return", "returns", "revoke", "right", "rollback", "rollup", "row",
      "rows", "savepoint", "scroll", "search", "second", "select", "sensitive",
      "session_user", "set", "similar", "smallint", "some", "specific",
      "specifictype", "sql", "sqlexception", "sqlstate", "sqlwarning", "start",
      "static", "submultiset", "symmetric", "system", "system_user", "table", "then",
      "time", "timestamp", "timezone_hour", "timezone_minute", "to", "trailing",
      "translation", "treat", "trigger", "true", "union", "unique", "unknown",
      "unnest", "update", "user", "using", "value", "values", "varchar", "varying",
      "when", "whenever", "where", "window", "with", "within", "without", "year"
   });

   protected final static Bind BIND_STRING = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.VARCHAR);
         }
         else
         {
            stmt.setString(nOrdinal + 1, (String)value);
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         return rs.getString(nOrdinal + 1);
      }
   };

   protected final static Bind BIND_CHAR = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.CHAR);
         }
         else
         {
            stmt.setString(nOrdinal + 1, (String)value);
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         String s = rs.getString(nOrdinal + 1);

         if (s != null)
         {
            int i = s.length();

            while (i > 0 && s.charAt(i - 1) == ' ')
            {
               --i;
            }

            if (i < s.length())
            {
               s = s.substring(0, i);
            }
         }

         return s;
      }
   };

   protected final static Bind BIND_CLOB = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.CLOB);
         }
         else
         {
            stmt.setString(nOrdinal + 1, (String)value);
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         Clob clob = rs.getClob(nOrdinal + 1);

         if (clob == null)
         {
            return null;
         }

         return clob.getSubString(1, (int)clob.length());
      }
   };

   protected final static Bind BIND_BINARY = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.VARBINARY);
         }
         else
         {
            stmt.setBytes(nOrdinal + 1, ((Binary)value).getData());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         byte[] data = rs.getBytes(nOrdinal + 1);

         if (data == null)
         {
            return null;
         }

         return new Binary(data);
      }
   };

   protected final static Bind BIND_BLOB = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.BLOB);
         }
         else
         {
            stmt.setBytes(nOrdinal + 1, ((Binary)value).getData());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         Blob blob = rs.getBlob(nOrdinal + 1);

         if (blob == null)
         {
            return null;
         }

         return new Binary(blob.getBytes(1L, (int)blob.length()));
      }
   };

   protected final static Bind BIND_INTEGER = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.INTEGER);
         }
         else
         {
            stmt.setInt(nOrdinal + 1, ((Number)value).intValue());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         int n = rs.getInt(nOrdinal + 1);

         if (rs.wasNull())
         {
            return null;
         }

         return Primitive.createInteger(n);
      }
   };

   protected final static Bind BIND_LONG = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.BIGINT);
         }
         else
         {
            stmt.setLong(nOrdinal + 1, ((Number)value).longValue());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         long l = rs.getLong(nOrdinal + 1);

         if (rs.wasNull())
         {
            return null;
         }

         return Primitive.createLong(l);
      }
   };

   protected final static Bind BIND_DECIMAL = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.DECIMAL);
         }
         else if (value instanceof BigDecimal)
         {
            stmt.setBigDecimal(nOrdinal + 1, (BigDecimal)value);
         }
         else
         {
            stmt.setBigDecimal(nOrdinal + 1, Primitive.toDecimal(value));
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         BigDecimal dec = rs.getBigDecimal(nOrdinal + 1);

         return (dec == null) ? null : dec.stripTrailingZeros();
      }
   };

   protected final static Bind BIND_FLOAT = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.REAL);
         }
         else
         {
            stmt.setFloat(nOrdinal + 1, ((Number)value).floatValue());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         float f = rs.getFloat(nOrdinal + 1);

         if (rs.wasNull())
         {
            return null;
         }

         return Primitive.createFloat(f);
      }
   };

   protected final static Bind BIND_DOUBLE = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.DOUBLE);
         }
         else
         {
            stmt.setDouble(nOrdinal + 1, ((Number)value).doubleValue());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         double d = rs.getDouble(nOrdinal + 1);

         if (rs.wasNull())
         {
            return null;
         }

         return Primitive.createDouble(d);
      }
   };

   protected final static Bind BIND_TIMESTAMP = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.TIMESTAMP);
         }
         else
         {
            stmt.setTimestamp(nOrdinal + 1, (Timestamp)value, adapter.getCalendar());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         return rs.getTimestamp(nOrdinal + 1, adapter.getCalendar());
      }
   };

   protected final static Bind BIND_BOOLEAN = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         if (value == null)
         {
            stmt.setNull(nOrdinal + 1, Types.BIT);
         }
         else
         {
            stmt.setBoolean(nOrdinal + 1, ((Boolean)value).booleanValue());
         }
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         boolean b = rs.getBoolean(nOrdinal + 1);

         if (rs.wasNull())
         {
            return null;
         }

         return Boolean.valueOf(b);
      }
   };

   protected static Method s_unwrap;
   protected static Method s_getUnderlyingConnection;
   protected static Class s_wsJDBCConnection;
   protected static Method s_getNativeConnection;

   static
   {
      try
      {
         s_unwrap = // Java 1.6 java.sql.Wrapper
            Class.forName("java.sql.Wrapper").getDeclaredMethod("unwrap", new Class[]{Class.class});
      }
      catch (Throwable t)
      {
      }

      try
      {
         s_getUnderlyingConnection =
            Class.forName("org.jboss.resource.adapter.jdbc.WrappedConnection")
               .getDeclaredMethod("getUnderlyingConnection", null);
      }
      catch (Throwable t)
      {
      }

      try
      {
         s_wsJDBCConnection = Class.forName("com.ibm.ws.rsadapter.jdbc.WSJdbcConnection");
         s_getNativeConnection =
            Class.forName("com.ibm.ws.rsadapter.jdbc.WSJdbcUtil")
               .getDeclaredMethod("getNativeConnection", new Class[]{s_wsJDBCConnection});
      }
      catch (Throwable t)
      {
      }
   }

   // attributes

   /**
    * The maximum batch data size in bytes (non-positive for unlimited).
    */
   protected int m_nMaxBatchDataSize = 65536;

   /**
    * The unicode string flag.
    */
   protected boolean m_bUnicode = true;

   /**
    * The literal bind flag. Set to true to disable bind parameters and use
    * literal binds in all SQL select queries.
    */
   protected boolean m_bLiteral;

   /**
    * True if debug logging is enabled.
    */
   protected boolean m_bDebug;

   // associations

   /**
    * The connection factory.
    */
   protected SQLConnectionFactory m_connectionFactory;

   /**
    * The SQL hook.
    */
   protected SQLHook m_sqlHook;

   /**
    * The SQL work item lookup key.
    */
   protected SQLWork m_workKey;

   /**
    * The calendar for timestamp conversion.
    */
   protected Calendar m_calendar;

   /**
    * The statement being executed.
    */
   protected Statement m_statement;

   /**
    * Log statement list for the deferred exception logger.
    */
   protected ArrayList m_logList;

   /**
    * Cached supported operator visitor.
    */
   protected SupportedOperatorVisitor m_sopVisitor;

   /**
    * Bind factory array: BindFactory[nPrimitiveOrdinal].
    */
   protected final static BindFactory[] s_bindFactoryArray = new BindFactory[Primitive.MAX_COUNT];

   static
   {
      s_bindFactoryArray[Primitive.STRING_ORDINAL] = new BindFactory()
      {
         public Bind create(Column column)
         {
            if (column.getAllocation() == Column.LOCATOR)
            {
               return BIND_CLOB;
            }

            if (column.getAllocation() == Column.FIXED)
            {
               return BIND_CHAR;
            }

            return BIND_STRING;
         }

         public Bind create(Primitive type)
         {
            return BIND_STRING;
         }
      };

      s_bindFactoryArray[Primitive.BINARY_ORDINAL] = new BindFactory()
      {
         public Bind create(Column column)
         {
            if (column.getAllocation() == Column.LOCATOR)
            {
               return BIND_BLOB;
            }

            return BIND_BINARY;
         }

         public Bind create(Primitive type)
         {
            return BIND_BINARY;
         }
      };

      s_bindFactoryArray[Primitive.INTEGER_ORDINAL] = new SimpleBindFactory(BIND_INTEGER);
      s_bindFactoryArray[Primitive.LONG_ORDINAL] = new SimpleBindFactory(BIND_LONG);
      s_bindFactoryArray[Primitive.DECIMAL_ORDINAL] = new SimpleBindFactory(BIND_DECIMAL);
      s_bindFactoryArray[Primitive.FLOAT_ORDINAL] = new SimpleBindFactory(BIND_FLOAT);
      s_bindFactoryArray[Primitive.DOUBLE_ORDINAL] = new SimpleBindFactory(BIND_DOUBLE);

      s_bindFactoryArray[Primitive.TIMESTAMP_ORDINAL] = new BindFactory()
      {
         public Bind create(final Column column)
         {
            if (column.isTimeZoned())
            {
               return new Bind()
               {
                  protected Calendar m_calendar = ((CalendarFactory)column.getConverter().getInstance(null)).createCalendar();

                  public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
                  {
                     if (value == null)
                     {
                        stmt.setNull(nOrdinal + 1, Types.TIMESTAMP);
                     }
                     else
                     {
                        stmt.setTimestamp(nOrdinal + 1, (Timestamp)value, m_calendar);
                     }
                  }

                  public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
                  {
                     return rs.getTimestamp(nOrdinal + 1, m_calendar);
                  }
               };
            }

            return BIND_TIMESTAMP;
         }

         public Bind create(Primitive type)
         {
            return BIND_TIMESTAMP;
         }
      };

      s_bindFactoryArray[Primitive.BOOLEAN_ORDINAL] = new SimpleBindFactory(BIND_BOOLEAN);

      s_bindFactoryArray[Primitive.ANY_ORDINAL] = new BindFactory()
      {
         public Bind create(Column column)
         {
            throw new MetadataException("err.meta.columnBindType", new Object[]{Primitive.ANY.getName()});
         }

         public Bind create(Primitive type)
         {
            throw new MetadataException("err.meta.columnBindType", new Object[]{Primitive.ANY.getName()});
         }
      };
   }

   /**
    * 2d array of converters: Converter[nFromTypeOrdinal * Primitive.MAX_COUNT + nToTypeOrdinal].
    */
   protected final static Converter[] s_converterArray = new Converter[Primitive.MAX_COUNT * Primitive.MAX_COUNT];

   static
   {
      for (int i = 0; i < Primitive.MAX_COUNT; ++i)
      {
         for (int k = 0; k < Primitive.MAX_COUNT; ++k)
         {
            if (i != k)
            {
               Primitive fromType = Primitive.get(i);
               Primitive toType = Primitive.get(k);
               UnaryFunction forward = toType.findConverter(fromType);

               if (forward != null)
               {
                  UnaryFunction inverse = fromType.findConverter(toType);

                  if (inverse != null)
                  {
                     s_converterArray[i * Primitive.MAX_COUNT + k] =
                        new SQLGenericConverter(fromType, toType, forward, inverse,
                        Primitive.isOrderPreserved(fromType, toType));
                  }
               }
            }
         }
      }
   }

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(SQLAdapter.class);

   /**
    * The deferred exception SQL logger.
    */
   private final static Logger s_xlogger = Logger.getLogger(SQLAdapter.class.getName() + ".exception");

   // constructors

   /**
    * Constructs the adapter.
    */
   protected SQLAdapter()
   {
      m_bDebug = s_logger.isDebugEnabled();

      if (!m_bDebug && s_xlogger.isDebugEnabled())
      {
         m_logList = new ArrayList();
      }
   }

   // operations

   /**
    * Sets the connection factory.
    * @param connectionFactory The connection factory to set.
    */
   public void setConnectionFactory(SQLConnectionFactory connectionFactory)
   {
      m_connectionFactory = connectionFactory;
   }

   /**
    * @return The connection factory.
    */
   public SQLConnectionFactory getConnectionFactory()
   {
      return m_connectionFactory;
   }

   /**
    * Sets the SQL hook.
    * @param sqlHook The SQL hook to set.
    */
   public void setSQLHook(SQLHook sqlHook)
   {
      m_sqlHook = sqlHook;
   }

   /**
    * @return The SQL hook.
    */
   public SQLHook getSQLHook()
   {
      return m_sqlHook;
   }

   /**
    * Sets the maximum batch data size in bytes.
    * @param nMaxBatchDataSize The maximum batch data size in bytes to set (non-positive for unlimited).
    */
   public void setMaxBatchDataSize(int nMaxBatchDataSize)
   {
      m_nMaxBatchDataSize = nMaxBatchDataSize;
   }

   /**
    * @return The maximum batch data size in bytes (non-positive for unlimited).
    */
   public int getMaxBatchDataSize()
   {
      return m_nMaxBatchDataSize;
   }

   /**
    * Sets the unicode string flag.
    * @param bUnicode The unicode string flag to set.
    */
   public void setUnicode(boolean bUnicode)
   {
      m_bUnicode = bUnicode;
   }

   /**
    * @return The unicode string flag.
    */
   public boolean isUnicode()
   {
      return m_bUnicode;
   }

   /**
    * Sets the literal bind flag.
    * @param bLiteral True to disable bind parameters and use literal binds
    * in all SQL select queries.
    */
   public void setLiteral(boolean bLiteral)
   {
      m_bLiteral = bLiteral;
   }

   /**
    * Gets the literal bind flag.
    *
    * @return The literal bind flag; true to disable bind parameters and use
    * literal binds in all SQL select queries.
    */
   public boolean isLiteral()
   {
      return m_bLiteral;
   }

   /**
    * Can the specified query use a shared SQL connection.
    * @param query The query to check.
    * @return The specific query can use a share an SQL connection with other queries.
    */
   protected boolean isSharedConnection(Query query)
   {
      return true;
   }

   /**
    * Gets an SQL connection resource.
    * Each invocation of this method must be paired with a decRef() on the resource.
    * @return An SQL connection resource for the current UOW.
    * @throws PersistenceException if an error occurs.
    */
   public SQLConnection getConnection() throws PersistenceException
   {
      return getConnection(null, true);
   }

   /**
    * Gets an SQL connection resource.
    * Each invocation of this method must be paired with a decRef() on the resource.
    * @param fragment The data source fragment. Can be null.
    * @param bShareable True if the connection is shareable.
    * @return An SQL connection resource for the current UOW.
    * @throws PersistenceException if an error occurs.
    */
   protected SQLConnection getConnection(RelationalDatabaseFragment fragment, boolean bShareable) throws PersistenceException
   {
      String sFragmentName = (fragment != null) ? fragment.getName() :
         m_context.getUnitOfWork().getFragmentName(m_bFragmented);
      SQLConnection sqlcon = (SQLConnection)m_context.getUnitOfWork()
         .findResource(this, sFragmentName, null, bShareable);

      if (sqlcon == null)
      {
         Connection con;

         try
         {
            if (m_connectionFactory instanceof SQLConnectionInterceptor) // connection factory override
            {
               con = ((SQLConnectionInterceptor)m_connectionFactory).getConnection(this, fragment);
            }
            else if (fragment != null)
            {
               con = ((SQLConnectionFactory)fragment.getConnectionFactory().getInstance(m_context))
                        .getConnection(this);
            }
            else
            {
               con = m_connectionFactory.getConnection(this);
            }
         }
         catch (Throwable t)
         {
            throw new AvailabilityException(fragment, t);
         }

         if (m_sqlHook != null)
         {
            con = new SQLHookConnection(con, m_sqlHook, this);
         }

         sqlcon = createConnection(con);
         sqlcon.setFragmentName(sFragmentName);
         sqlcon.setShareable(bShareable);
         m_context.getUnitOfWork().addResource(sqlcon);
      }

      sqlcon.incRef();

      return sqlcon;
   }

   /**
    * Creates a new SQL connection resource.
    * @param connection The SQL connection for the resource.
    * @return The SQLConnection.
    */
   protected SQLConnection createConnection(Connection connection)
   {
      return new SQLConnection(this, connection);
   }

   /**
    * Closes an SQL connection.
    * @param connection The connection to close. Can be null.
    */
   public void close(Connection connection)
   {
      if (connection != null)
      {
         try
         {
            connection.close();
         }
         catch (SQLException e)
         {
            s_logger.debug("Unable to close the SQL connection", e);
         }
      }
   }

   /**
    * Closes an SQL statement.
    * @param stmt The statement to close. Can be null.
    */
   public void close(Statement stmt)
   {
      if (stmt != null)
      {
         try
         {
            stmt.close();
         }
         catch (SQLException e)
         {
            s_logger.debug("Unable to close the SQL statement", e);
         }
      }
   }

   /**
    * Closes a result set.
    * @param rs The result set to close. Can be null.
    */
   public void close(ResultSet rs)
   {
      if (rs != null)
      {
         try
         {
            rs.close();
         }
         catch (SQLException e)
         {
            s_logger.debug("Unable to close the result set", e);
         }
      }
   }

   /**
    * Cancels an SQL statement.
    * @param stmt The statement to cancel. Can be null.
    */
   public void cancel(Statement stmt)
   {
      if (stmt != null)
      {
         try
         {
            stmt.cancel();
         }
         catch (SQLException e)
         {
            s_logger.debug("Unable to cancel the SQL statement", e);
         }
      }
   }

   /**
    * Cancels the currently executing statement.
    * @see Cancellable#cancel()
    */
   public void cancel()
   {
      Statement stmt;

      synchronized (this)
      {
         stmt = m_statement;
      }

      cancel(stmt);
   }

   /**
    * Creates and initializes a Statement.
    * @param connection The Connection on which the Statement should be created.
    * @param sSQL The SQL to execute.
    * @param query The Query being executed.
    * @return The Statement to execute.
    * @throws SQLException If an error occurs.
    */
   public PreparedStatement prepareStatement(Connection connection, String sSQL, Query query) throws SQLException
   {
      return connection.prepareStatement(sSQL);
   }

   /**
    * Creates and initializes a Statement.
    * @param connection The Connection on which the Statement should be created.
    * @param sSQL The SQL to execute.
    * @param work The Work being executed.
    * @return The Statement to execute.
    * @throws SQLException If an error occurs.
    */
   public PreparedStatement prepareStatement(Connection connection, String sSQL, Work work)
      throws SQLException
   {
      return connection.prepareStatement(sSQL);
   }

   /**
    * Executes an SQL query.
    * @param stmt The statement to execute.
    * @see PreparedStatement#executeQuery();
    */
   public ResultSet executeQuery(PreparedStatement stmt) throws SQLException
   {
      Cancellable cancellableSaved = null;

      try
      {
         synchronized (this)
         {
            m_statement = stmt;
         }

         cancellableSaved = m_context.setCancellable(this);

         return stmt.executeQuery();
      }
      finally
      {
         m_context.setCancellable(cancellableSaved);
         m_context.addRPCCount(1);

         synchronized (this)
         {
            m_statement = null;
         }
      }
   }

   /**
    * Executes an SQL update.
    * @param stmt The statement to execute.
    * @return The affected record count.
    * @see PreparedStatement#executeUpdate();
    */
   public int executeUpdate(PreparedStatement stmt) throws SQLException
   {
      Cancellable cancellableSaved = null;

      try
      {
         synchronized (this)
         {
            m_statement = stmt;
         }

         cancellableSaved = m_context.setCancellable(this);

         return stmt.executeUpdate();
      }
      finally
      {
         m_context.setCancellable(cancellableSaved);
         m_context.addRPCCount(1);

         synchronized (this)
         {
            m_statement = null;
         }
      }
   }

   /**
    * Executes an SQL batch.
    * @param stmt The statement to execute.
    * @return Affected record counts and status information.
    * @see Statement#executeBatch();
    */
   public int[] executeBatch(Statement stmt) throws SQLException
   {
      Cancellable cancellableSaved = null;

      try
      {
         synchronized (this)
         {
            m_statement = stmt;
         }

         cancellableSaved = m_context.setCancellable(this);

         return stmt.executeBatch();
      }
      finally
      {
         m_context.setCancellable(cancellableSaved);
         m_context.addRPCCount(1);

         synchronized (this)
         {
            m_statement = null;
         }
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceMapper#mapQuery(nexj.core.persistence.Query)
    */
   public void mapQuery(Query query)
   {
      if (query.isRoot())
      {
         query.optimizeJoins();
      }

      SQLGenerator generator = new SQLGenerator(query, this);

      query.setGenerator(generator);
      generator.addMappings();
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#openCursor(nexj.core.persistence.Query)
    */
   public Cursor openCursor(Query query)
   {
      return new SQLCursor(query);
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#execute(nexj.core.persistence.Work[], int, int)
    */
   public void execute(Work[] workArray, int nStart, int nEnd)
   {
      SQLWork work = null;
      SQLConnection connection = null;
      PreparedStatement stmt = null;
      long lStartTime = 0;
      long lEndTime = 0;
      int nBatch = nStart;
      int nDataSize = 0;
      int nTotalSize = 0;
      boolean bBatchable = true;

      try
      {
         clearLog();

         for (;;)
         {
            if (stmt != null)
            {
               boolean bExecute = (nStart >= nEnd || !isBatchSupported());

               if (!bExecute)
               {
                  bExecute = !bBatchable;
                  bBatchable = ((SQLWork)workArray[nStart]).isBatchable();

                  if (bBatchable)
                  {
                     nDataSize = ((SQLWork)workArray[nStart]).getDataSize();
                  }

                  bExecute |= !bBatchable;

                  if (!bExecute)
                  {
                     bExecute = (workArray[nStart].compareTo(workArray[nBatch]) != 0);

                     if (!bExecute)
                     {
                        bExecute |= m_nMaxBatchDataSize >= 0 && m_nMaxBatchDataSize - nTotalSize < nDataSize;
                     }
                  }
               }

               if (bExecute)
               {
                  if (nStart - nBatch > 1)
                  {
                     stmt.addBatch();
                  }

                  if (m_bDebug)
                  {
                     lStartTime = System.currentTimeMillis();
                  }

                  work.execute(stmt, workArray, nBatch, nStart);

                  if (m_bDebug)
                  {
                     lEndTime = System.currentTimeMillis();
                     s_logger.debug("SQL execution time: " + (lEndTime - lStartTime) + " ms");
                  }

                  nBatch = nStart;
                  nTotalSize = 0;
                  close(stmt);
                  stmt = null;
               }

               nTotalSize += nDataSize;
            }

            if (nStart >= nEnd)
            {
               break;
            }

            if (work == null && isBatchSupported())
            {
               work = (SQLWork)workArray[nStart];
               bBatchable = work.isBatchable();

               if (bBatchable)
               {
                  nDataSize = work.getDataSize();
               }
            }

            work = (SQLWork)workArray[nStart++];

            if (stmt == null)
            {
               String sSQL = work.getSQL();

               log(sSQL);

               if (connection == null)
               {
                  connection = getConnection((RelationalDatabaseFragment)work.getFragment(), true);
               }

               if (work.isCallable())
               {
                  stmt = connection.getConnection().prepareCall(sSQL);
               }
               else
               {
                  stmt = prepareStatement(connection.getConnection(), sSQL, work);
               }
            }
            else
            {
               stmt.addBatch();
            }

            logBatch(nStart - nBatch - 1);

            work.prepare();
            work.bind(stmt, (SQLWork)workArray[nBatch]);

            if (isBatchSupported() && bBatchable)
            {
               nDataSize = work.getDataSize();
            }
         }
      }
      catch (SQLException e)
      {
         flushLog(e);

         throw getException(e, workArray, nBatch, nStart);
      }
      finally
      {
         close(stmt);

         if (connection != null)
         {
            connection.decRef();
         }

         clearLog();
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addDependency(nexj.core.runtime.UnitOfWork, nexj.core.persistence.Work, nexj.core.runtime.Instance, nexj.core.meta.persistence.Key, nexj.core.meta.persistence.Key, boolean)
    */
   public void addDependency(UnitOfWork uow, Work work, Instance instance, Key dstKey, Key srcKey, boolean bSuccessor)
   {
      if (instance.getUnitOfWork() != uow)
      {
         return;
      }

      int nType;

      switch (instance.getState())
      {
         case Instance.NEW:
            nType = SQLWork.INSERT;
            break;

         case Instance.DIRTY:
            nType = SQLWork.UPDATE;
            break;

         case Instance.DELETED:
            if (instance.getOID() != null)
            {
               nType = SQLWork.DELETE;
               break;
            }

         default:
            return;
      }

      Table table;

      if (bSuccessor)
      {
         table = ((Index)dstKey).getTable();
      }
      else
      {
         table = ((RelationalMapping)instance.getPersistenceMapping()).getPrimaryTable();
      }

      SQLWork sqlWork = findWork(uow, instance, table);

      if (sqlWork == null)
      {
         sqlWork = addWork(uow, nType, instance, table);

         if (bSuccessor)
         {
            Table primaryTable = ((RelationalMapping)instance.getPersistenceMapping()).getPrimaryTable();

            // Add a secondary table as successor to the primary one
            if (table != primaryTable)
            {
               if (instance.getOID() != null)
               {
                  sqlWork.setOID();
               }
               else
               {
                  getWork(uow, nType, instance, primaryTable).addSuccessor(sqlWork, table.getPrimaryKey(), primaryTable.getPrimaryKey());
               }
            }
         }
      }

      if (bSuccessor)
      {
         if (nType == SQLWork.INSERT)
         {
            work.addSuccessor(sqlWork, dstKey, srcKey);
         }
      }
      else
      {
         sqlWork.addSuccessor(work, dstKey, srcKey);
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance)
    */
   public void addWork(UnitOfWork uow, Instance instance)
   {
      switch (instance.getState())
      {
         case Instance.NEW:
            addInsert(uow, instance);
            break;

         case Instance.DIRTY:
            addUpdate(uow, instance);
            break;

         case Instance.DELETED:
            addDelete(uow, instance);
            break;

         default:
            throw new IllegalStateException();
      }
   }

   /**
    * @see nexj.core.persistence.GenericPersistenceAdapter#addPrimitiveWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance, nexj.core.persistence.Work, nexj.core.meta.persistence.AttributeMapping, java.lang.Object)
    */
   protected Work addPrimitiveWork(UnitOfWork uow, Instance instance,
      Work primaryWork, AttributeMapping attributeMapping, Object value)
   {
      RelationalMapping mapping = (RelationalMapping)instance.getPersistenceMapping();
      RelationalPrimitiveMapping primitiveMapping = (RelationalPrimitiveMapping)attributeMapping;
      Column column = primitiveMapping.getColumn();
      int nType = primaryWork.getType();
      SQLWork work = null;

      if (nType == SQLWork.UPDATE || value != null || column.getTable() == mapping.getPrimaryTable())
      {
         work = findWork(uow, instance, column.getTable());

         if (work == null)
         {
            if (nType == SQLWork.UPDATE)
            {
               ((SQLUpdate)primaryWork).touch();
            }

            work = addWork(uow, nType, instance, column.getTable());
            primaryWork.addSuccessor(work, column.getTable().getPrimaryKey(), mapping.getObjectKey());
         }

         work.setValue(column, column.getValueType().convert(value));
      }

      return work;
   }

   /**
    * @see nexj.core.persistence.GenericPersistenceAdapter#addClassWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance, nexj.core.persistence.Work, nexj.core.meta.persistence.ClassMapping)
    */
   protected Work addClassWork(UnitOfWork uow, Instance instance,
      Work primaryWork, ClassMapping assocMapping)
   {
      PersistenceMapping mapping = instance.getPersistenceMapping();
      RelationalClassMapping relAssocMapping = (RelationalClassMapping)assocMapping;
      SQLWork work = findWork(uow, instance, relAssocMapping.getSourceKey().getTable());

      if (work == null)
      {
         if (primaryWork.getType() == SQLWork.UPDATE)
         {
            ((SQLUpdate)primaryWork).touch();
         }

         work = addWork(uow, primaryWork.getType(), instance, relAssocMapping.getSourceKey().getTable());
         primaryWork.addSuccessor(work, relAssocMapping.getSourceKey().getTable().getPrimaryKey(), mapping.getObjectKey());
      }

      return work;
   }

   /**
    * @see nexj.core.persistence.GenericPersistenceAdapter#isCreateSuccessor(nexj.core.persistence.Work, nexj.core.meta.persistence.ClassMapping, boolean)
    */
   protected boolean isCreateSuccessor(Work work, ClassMapping assocMapping, boolean bOID)
   {
      SQLWork primaryWork = (SQLWork)work;

      if (assocMapping.getKey(false).isObjectKeyPart())
      {
         if (bOID)
         {
            return true;
         }

         if (primaryWork.isIdentity())
         {
            return true;
         }

         if (assocMapping.getKey(true).isObjectKeyPart())
         {
            return false;
         }

         throw new PersistenceException("err.persistence.requiredOID",
            new Object[]{assocMapping.getPersistenceMapping().getMetaclass().getName()});
      }

      return false;
   }

   /**
    * Adds insert work items to the unit of work.
    * @param uow The unit of work.
    * @param instance The instance for which to add the work items.
    */
   protected void addInsert(UnitOfWork uow, Instance instance)
   {
      RelationalMapping relMapping = (RelationalMapping)instance.getPersistenceMapping();
      Table primaryTable = relMapping.getPrimaryTable();
      SQLWork primaryWork = getWork(uow, SQLWork.INSERT, instance, primaryTable);
      OID oid = instance.getOID();

      if (oid != null)
      {
         primaryWork.setOID();
      }
      else
      {
         Component component = relMapping.getKeyGenerator();

         if (component == RelationalMapping.KEY_GEN_IDENTITY)
         {
            primaryWork.setIdentity(true);
         }
         else if (component != null)
         {
            oid = ((OIDGenerator)component.getInstance(uow.getInvocationContext())).generateOID(instance, this);

            Object[] values = oid.getValueArray();
            Index pk = primaryTable.getPrimaryKey();
            int nCount = pk.getIndexColumnCount();

            if (nCount != values.length)
            {
               throw new PersistenceException("err.persistence.oidValueCount",
                  new Object[]{component.getName(), Primitive.createInteger(nCount),
                     Primitive.createInteger(values.length)});
            }

            for (int i = 0; i < nCount; ++i)
            {
               values[i] = pk.getIndexColumn(i).getColumn().getValueType().convert(values[i]);
            }

            instance.setOID(oid);
            primaryWork.setOID();
         }
         else
         {
            Metaclass metaclass = instance.getMetaclass();

            for (int i = 0, n = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; ++i)
            {
               Attribute attribute = metaclass.getInstanceAttribute(i);
               AttributeMapping mapping = relMapping.getAttributeMapping(attribute);
               int nPrimaryColCount = primaryTable.getPrimaryKey().getIndexColumnCount();

               if (mapping instanceof RelationalPrimitiveMapping)
               {
                  Column column = ((RelationalPrimitiveMapping)mapping).getColumn();

                  if (column.isPrimary())
                  {
                     primaryWork.setValue(column, column.getValueType().convert(instance.getValue(i)));

                     if (++n >= nPrimaryColCount)
                     {
                        break;
                     }
                  }
               }
               else if (mapping instanceof RelationalClassMapping)
               {
                  Index index = ((RelationalClassMapping)mapping).getSourceKey();

                  if (index.isObjectKeyPart())
                  {
                     Object value = instance.getValue(i);

                     if (value instanceof OIDHolder)
                     {
                        oid = ((OIDHolder)value).getOID();

                        if (oid != null )
                        {
                           int nIndexColCount = index.getIndexColumnCount();

                           if (nIndexColCount == oid.getCount())
                           {
                              for (int k = 0; k < nIndexColCount; ++k)
                              {
                                 Column column = index.getIndexColumn(k).getColumn();

                                 if (column.isPrimary())
                                 {
                                    primaryWork.setValue(column, column.getValueType().convert(oid.getValue(k)));

                                    if (++n >= nPrimaryColCount)
                                    {
                                       break;
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
            }

            oid = primaryWork.computeOID();

            if (oid != null)
            {
               instance.setOID(oid);
            }
         }
      }

      addCreateDependencies(uow, instance, primaryWork);
   }

   /**
    * Adds update work items to the unit of work.
    * @param uow The unit of work.
    * @param instance The instance for which to add the work items.
    */
   protected void addUpdate(UnitOfWork uow, Instance instance)
   {
      RelationalMapping relMapping = (RelationalMapping)instance.getPersistenceMapping();
      Table primaryTable = relMapping.getPrimaryTable();
      SQLUpdate work = (SQLUpdate)getWork(uow, SQLWork.UPDATE, instance, primaryTable);

      work.setupLocking();

      addUpdateDependencies(uow, instance, work);
   }

   /**
    * Adds deletion work items to the unit of work.
    * @param uow The unit of work.
    * @param instance The instance for which to add the work items.
    */
   protected void addDelete(UnitOfWork uow, Instance instance)
   {
      assert instance.getOID() != null;

      RelationalMapping relMapping = (RelationalMapping)instance.getPersistenceMapping();
      SQLWork primaryWork = getWork(uow, SQLWork.DELETE, instance, relMapping.getPrimaryTable());

      primaryWork.setOID();
      primaryWork.setupLocking();

      int nCount = relMapping.getTableCount();

      for (int i = 1; i < nCount; ++i)
      {
         SQLWork work = getWork(uow, SQLWork.DELETE, instance, relMapping.getTable(i));

         work.setOID();
         primaryWork.addSuccessor(work, null, null);
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addDenorm(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance, nexj.core.meta.persistence.AttributeMapping)
    */
   public void addDenorm(UnitOfWork uow, Instance instance, AttributeMapping mapping) throws PersistenceException
   {
      Attribute attribute = mapping.getAttribute();
      Object value = instance.getValueDirect(attribute.getOrdinal());

      if (mapping instanceof RelationalPrimitiveMapping)
      {
         for (int i = 0, n = mapping.getDenormCount(); i < n; ++i)
         {
            RelationalPrimitiveDenorm denorm = (RelationalPrimitiveDenorm)mapping.getDenorm(i);
            Column column = denorm.getColumn();
            SQLWork work = findWork(uow, instance, column.getTable());

            if (work != null)
            {
               work.setValue(column, value);
            }
         }
      }
      else
      {
         Instance assoc = null;

         if (value instanceof InstanceList)
         {
            InstanceList list = (InstanceList)value;

            if (!list.isEmpty())
            {
               assoc = list.getInstance(0);
            }
         }
         else
         {
            assoc = (Instance)value;
         }

         for (int i = 0, n = mapping.getDenormCount(); i < n; ++i)
         {
            RelationalClassDenorm denorm = (RelationalClassDenorm)mapping.getDenorm(i);
            Key dstKey = ((RelationalClassMapping)denorm.getMapping()).getDestinationKey();

            SQLWork work = findWork(uow, instance, denorm.getSourceKey().getTable());

            if (work != null)
            {
               if (assoc != null && assoc.getOID() == null)
               {
                  addDependency(uow, work, assoc, denorm.getSourceKey(), dstKey, true);
               }
               else
               {
                  work.setKeyValue(denorm.getSourceKey(), dstKey, assoc);
               }
            }
         }
      }
   }

   /**
    * Finds an SQL work item in the unit of work.
    * @param uow The unit of work.
    * @param instance The instance for which to find the work item.
    * @param table The table for which to find the work item.
    * @return The found work item, or null if not found.
    */
   public SQLWork findWork(UnitOfWork uow, Instance instance, Table table)
   {
      if (m_workKey == null)
      {
         m_workKey = new SQLWork(instance, table, this)
         {
            public int getType()
            {
               return -1;
            }

            public String getSQL()
            {
               return null;
            }

            public void bind(PreparedStatement stmt, SQLWork proto)
            {
            }

            public boolean isBatchable()
            {
               return false;
            }

            public void execute(PreparedStatement stmt, Work[] workArray, int nStart, int nEnd) throws SQLException
            {
            }
         };
      }
      else
      {
         m_workKey.setData(instance, table);
      }

      return (SQLWork)uow.findWork(m_workKey);
   }

   /**
    * Gets an SQL work item in the unit of work, or creates a new one of not found.
    * @param uow The unit of work.
    * @param nType The work item type.
    * @param instance The instance for which to find the work item.
    * @param table The table for which to find the work item.
    * @return The work item.
    */
   public SQLWork getWork(UnitOfWork uow, int nType, Instance instance, Table table)
   {
      SQLWork work = findWork(uow, instance, table);

      if (work == null)
      {
         work = addWork(uow, nType, instance, table);
      }

      return work;
   }

   /**
    * Factory method to create a work item of a given type and add it to the UOW.
    * @param uow The unit of work.
    * @param nType The work item type.
    * @param instance The instance for which to find the work item.
    * @param table The table for which to find the work item.
    * @return The created work item.
    */
   protected SQLWork addWork(UnitOfWork uow, int nType, Instance instance, Table table)
   {
      SQLWork work;

      switch (nType)
      {
         case SQLWork.INSERT:
            work = new SQLInsert(instance, table, this);
            break;

         case SQLWork.UPDATE:
            work = new SQLUpdate(instance, table, this);
            break;

         case SQLWork.DELETE:
            work = new SQLDelete(instance, table, this);
            break;

         default:
            throw new IllegalArgumentException();
      }

      uow.addWork(work);

      return work;
   }

   /**
    * Gets a default type converter.
    * @param fromType The type from which to convert.
    * @param toType The type to which to convert.
    * @throws TypeConversionException if the conversion is not supported.
    */
   public Converter getDefaultConverter(Primitive fromType, Primitive toType) throws TypeConversionException
   {
      Converter converter = s_converterArray[fromType.getOrdinal() * Primitive.MAX_COUNT + toType.getOrdinal()];

      if (converter == null)
      {
         throw new TypeConversionException(toType);
      }

      return converter;
   }

   /**
    * Gets a type converter for the specified column.
    * @param type The type to which to convert.
    * @param column The column
    * @return The converter instance, or null if no conversion is needed.
    */
   protected final Converter getConverter(Primitive type, Column column)
   {
      if (column.getConverter() == null)
      {
         type = getConversionMapper().getType(type);
         Primitive colType = getConversionMapper().getType(column.getType());

         if (type != colType)
         {
            return getDefaultConverter(colType, type);
         }

         return null;
      }

      return (Converter)column.getConverter().getInstance(null);
   }

   /**
    * Adds the field columns to a set.
    * @param field The field.
    * @param columnSet The destination column set.
    */
   protected void addColumns(Field field, Set columnSet)
   {
      RelationalPrimitiveMapping mapping = (RelationalPrimitiveMapping)field.getAttributeMapping();

      if (mapping != null)
      {
         columnSet.add(mapping.getColumn());
      }
      else
      {
         Object item = field.getItem();

         if (item instanceof Column)
         {
            columnSet.add(item);
         }
         else if (item instanceof Field[])
         {
            Field[] fieldArray = (Field[])item;

            for (int i = 0; i < fieldArray.length; ++i)
            {
               addColumns(fieldArray[i], columnSet);
            }
         }
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#isUnique(nexj.core.persistence.Query, java.util.List)
    */
   public boolean isUnique(Query query, List sourceList)
   {
      int nCount = sourceList.size();
      Set columnSet = new HashHolder(nCount);

      for (int nSource = 0; nSource < nCount; ++nSource)
      {
         Source source = (Source)sourceList.get(nSource);

         if (source instanceof Field)
         {
            addColumns((Field)source, columnSet);
         }
         else
         {
            Query assoc = source.getQuery();

            if (assoc.getParent() == query)
            {
               Index index = (Index)query.getKey(false);

               for (int i = 0, n = index.getIndexColumnCount(); i < n; ++i)
               {
                  columnSet.add(index.getIndexColumn(i).getColumn());
               }
            }
         }
      }

      if (!columnSet.isEmpty())
      {
         Set tableSet = new HashHolder(columnSet.size() >> 2);

         for (Iterator itr = columnSet.iterator(); itr.hasNext();)
         {
            tableSet.add(((Column)itr.next()).getTable());
         }

         for (Iterator itr = tableSet.iterator(); itr.hasNext();)
         {
            Table table = (Table)itr.next();

         loop:
            for (int nIndex = 0, nIndexCount = table.getIndexCount(); nIndex < nIndexCount; ++nIndex)
            {
               Index index = table.getIndex(nIndex);

               if (index.isUnique() &&
                  (index.getType() == Index.BTREE || index.getType() == Index.CLUSTER))
               {
                  for (int i = 0, n = index.getIndexColumnCount(); i < n; ++i)
                  {
                     if (!columnSet.contains(index.getIndexColumn(i).getColumn()))
                     {
                        continue loop;
                     }
                  }

                  return true;
               }
            }
         }
      }

      return false;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getQuery(nexj.core.persistence.Source)
    */
   public Query getQuery(Source source)
   {
      return ((SQLJoin)source.getMapping()).query;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getFields(Source)
    */
   public final Field[] getFields(Source source)
   {
      if (source instanceof Query)
      {
         return (Field[])((Query)source).getFieldItem();
      }

      return (Field[])source.getItem();
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValues(nexj.core.persistence.OID, Source)
    */
   public Object[] getValues(OID oid, Source source) throws PersistenceException
   {
      if (source instanceof Query)
      {
         Query query = (Query)source;

         if (query.getAttribute() != null)
         {
            Key key = query.getKey(true);

            if (key.isObjectKeyPart())
            {
               Field[] fieldArray = getFields(source);
               int nCount = fieldArray.length;

               if (nCount != oid.getCount() && nCount <= key.getPartCount())
               {
                  Object[] valueArray = new Object[nCount];

                  for (int i = 0; i < nCount; ++i)
                  {
                     int nOrdinal = key.getObjectKeyPartOrdinal(i);

                     if (nOrdinal >= oid.getCount())
                     {
                        return oid.getValueArray();
                     }

                     valueArray[i] = oid.getValue(nOrdinal);
                  }

                  return valueArray;
               }
            }
         }
      }

      return oid.getValueArray();
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValue(OID, nexj.core.persistence.Source)
    */
   public Object getValue(OID oid, Source source)
   {
      if (source instanceof Field)
      {
         Column column = null;
         Field field = (Field)source;
         RelationalPrimitiveMapping mapping = (RelationalPrimitiveMapping)field.getAttributeMapping();

         if (mapping != null)
         {
            column = mapping.getColumn();
         }
         else if (field.getItem() instanceof Column)
         {
            column = (Column)field.getItem();
         }

         if (column != null && column.isPrimary())
         {
            IndexColumn indexColumn = ((Index)((RelationalMapping)source.getQuery()
               .getPersistenceMapping()).getObjectKey()).findIndexColumn(column);

            if (indexColumn != null && indexColumn.getOrdinal() < oid.getCount())
            {
               return oid.getValue(indexColumn.getOrdinal());
            }
         }
      }
      else
      {
         Query query = source.getQuery();

         if (query.getParent() != null)
         {
            Key key = query.getKey(false);

            if (key.isObjectKeyPart() && query.getKey(true).isObjectKeyPart())
            {
               int nCount = key.getPartCount();
               Object[] valueArray = new Object[nCount];

               for (int i = 0; i < nCount; ++i)
               {
                  int nOrdinal = key.getObjectKeyPartOrdinal(i);

                  if (nOrdinal >= oid.getCount())
                  {
                     return Undefined.VALUE;
                  }

                  valueArray[i] = oid.getValue(nOrdinal);
               }

               return new OID(valueArray);
            }
         }
      }

      return Undefined.VALUE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValue(PropertyMap, nexj.core.persistence.Source)
    */
   public Object getValue(PropertyMap instance, Source source)
   {
      if (instance instanceof OIDHolder)
      {
         OID oid = ((OIDHolder)instance).getOID();

         if (oid == null)
         {
            return Undefined.VALUE;
         }

         Object value = getValue(oid, source);

         if (value != Undefined.VALUE)
         {
            return value;
         }
      }

      Attribute attribute = source.getAttribute();

      if (attribute != null && !source.isInverse())
      {
         Object value = instance.findValue(attribute.getName(), Undefined.VALUE);

         if (value != Undefined.VALUE)
         {
            return value;
         }
      }

      if (source instanceof Field)
      {
         Column column = null;
         RelationalPrimitiveMapping primitiveMapping = (RelationalPrimitiveMapping)source.getAttributeMapping();

         if (primitiveMapping != null)
         {
            column = primitiveMapping.getColumn();
         }
         else
         {
            Object item = source.getItem();

            if (item instanceof Column)
            {
               column = (Column)item;
            }
            else if (item instanceof Field[])
            {
               Object[] valueArray = null;
               Field[] fieldArray = (Field[])item;

               for (int i = 0; i < fieldArray.length; ++i)
               {
                  Object value = getValue(instance, fieldArray[i]);

                  if (value == Undefined.VALUE)
                  {
                     valueArray = null;

                     break;
                  }

                  if (valueArray == null)
                  {
                     valueArray = new Object[fieldArray.length];
                  }

                  valueArray[i] = value;
               }

               if (valueArray != null)
               {
                  return new OID(valueArray);
               }
            }
         }

         if (column != null)
         {
            PersistenceMapping persistenceMapping = ((SQLJoin)source.getMapping()).query.getPersistenceMapping();

            if (persistenceMapping != null)
            {
               Metaclass metaclass = persistenceMapping.getMetaclass();

               assert metaclass.isUpcast(m_context.getMetadata().getMetaclass(instance.getClassName()));

               // TODO: Optimize with precomputed metadata

               for (int nAttrOrdinal = 0, nAttrCount = metaclass.getInstanceAttributeCount();
                  nAttrOrdinal < nAttrCount; ++nAttrOrdinal)
               {
                  attribute = metaclass.getInstanceAttribute(nAttrOrdinal);

                  AttributeMapping mapping = persistenceMapping.getAttributeMapping(attribute);

                  if (mapping instanceof RelationalPrimitiveMapping)
                  {
                     if (column == ((RelationalPrimitiveMapping)mapping).getColumn())
                     {
                        Object value = instance.findValue(attribute.getName(), Undefined.VALUE);

                        if (value != Undefined.VALUE)
                        {
                           return column.getValueType().convert(value);
                        }
                     }
                  }
                  else if (mapping instanceof RelationalClassMapping)
                  {
                     RelationalClassMapping classMapping = (RelationalClassMapping)mapping;

                     if (classMapping.isInner())
                     {
                        IndexColumn indexColumn = classMapping.getSourceKey().findIndexColumn(column);

                        if (indexColumn != null)
                        {
                           Object value = instance.findValue(attribute.getName(), Undefined.VALUE);

                           if (value == null)
                           {
                              return null;
                           }

                           if (value instanceof OIDHolder)
                           {
                              OID oid = ((OIDHolder)value).getOID();

                              if (oid != null && indexColumn.getOrdinal() < oid.getCount())
                              {
                                 return oid.getValue(indexColumn.getOrdinal());
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }

      return Undefined.VALUE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getOID(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public OID getOID(PropertyMap instance, Object item)
   {
      Field[] fieldArray = (Field[])item;
      Object[] valueArray = new Object[fieldArray.length];

      for (int i = 0; i < fieldArray.length; ++i)
      {
         if ((valueArray[i] = getValue(instance, fieldArray[i])) == Undefined.VALUE)
         {
            return null;
         }
      }

      return new OID(valueArray);
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#validate(nexj.core.meta.persistence.AttributeMapping, java.lang.Object)
    */
   public void validate(AttributeMapping mapping, Object value) throws ValidationException
   {
      if (mapping instanceof RelationalPrimitiveMapping && value != null)
      {
         Column column = ((RelationalPrimitiveMapping)mapping).getColumn();
         int nPrecision;

         switch (column.getType().getOrdinal())
         {
            case Primitive.INTEGER_ORDINAL:
               nPrecision = column.getPrecision();

               switch (nPrecision)
               {
                  case 1:
                     int nValue = ((Number)toBind(column, column.getValueType().convert(value))).intValue();

                     if (nValue < 0 || nValue > 255)
                     {
                        throw new ValidationException("err.validation.numberRange",
                           new Object[]
                           {
                              new StringId(mapping.getAttribute().getCaption()),
                              new StringId(mapping.getAttribute().getMetaclass().getCaption()),
                              Primitive.ZERO_INTEGER,
                              Primitive.createInteger(255)
                           });
                     }

                     break;

                  case 2:
                     nValue = ((Number)toBind(column, column.getValueType().convert(value))).intValue();

                     if (nValue < Short.MIN_VALUE || nValue > Short.MAX_VALUE)
                     {
                        throw new ValidationException("err.validation.numberRange",
                           new Object[]
                           {
                              new StringId(mapping.getAttribute().getCaption()),
                              new StringId(mapping.getAttribute().getMetaclass().getCaption()),
                              Primitive.createInteger(Short.MIN_VALUE),
                              Primitive.createInteger(Short.MAX_VALUE)
                           });
                     }

                     break;
               }

               break;

            case Primitive.DECIMAL_ORDINAL:
               nPrecision = column.getPrecision(getMaxDecimalPrecision());

               int nScale = column.getScale(getMaxDecimalPrecision());
               BigDecimal decMax = BigDecimal.TEN.pow(nPrecision - nScale);

               if (((BigDecimal)toBind(column, column.getValueType().convert(value))).abs().setScale(nScale,
                  RoundingMode.HALF_UP).compareTo(decMax) >= 0)
               {
                  if (nScale > 0)
                  {
                     decMax = decMax.subtract(BigDecimal.ONE.divide(BigDecimal.TEN.pow(nScale)));
                  }

                  throw new ValidationException("err.validation.numberRange",
                     new Object[]
                     {
                        new StringId(mapping.getAttribute().getCaption()),
                        new StringId(mapping.getAttribute().getMetaclass().getCaption()),
                        decMax.negate(),
                        decMax
                     });
               }

               break;

            case Primitive.TIMESTAMP_ORDINAL:
               long lTime = ((Timestamp)toBind(column, column.getValueType().convert(value))).getTime();

               if (lTime < getMinTime() || lTime > getMaxTime())
               {
                  throw new ValidationException("err.validation.dateRange",
                     new Object[]
                     {
                        new StringId(mapping.getAttribute().getCaption()),
                        new StringId(mapping.getAttribute().getMetaclass().getCaption()),
                        new Timestamp(getMinTime()),
                        new Timestamp(getMaxTime())
                     });
               }

               break;
         }
      }
   }

   /**
    * Validate schema Unicode flag matches DB state (if available).
    * @param schema the Schema to validate.
    * @param rs The resultset to validate.
    * @param nColumn The column number (1 based) in resultset to validate.
    * @return TRUE == is a unicode column
    *         FALSE == is not a unicode column
    *         null == unknown (e.g. number column or column not present)
    * @throws SQLException On DB IO error.
    */
   protected abstract Boolean isUnicode(RelationalSchema schema, ResultSet rs, int nColumn)
      throws SQLException;

   /**
    * @return The maximum precision of decimal numbers.
    */
   protected int getMaxDecimalPrecision()
   {
      return 38;
   }

   /**
    * @return The minimum supported timestamp value.
    */
   protected abstract long getMinTime();

   /**
    * @return The minimum supported timestamp value.
    */
   protected abstract long getMaxTime();

   /**
    * Check if the argument types of the specified operator are supported for that operator with
    * this RDBMS.
    * @param op The operator and argument types to check for support by the adapter.
    * @return Are operator arguments supported by the RDMS for this operator.
    */
   public boolean isSupported(InOperator op)
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#isSupported(nexj.core.persistence.Operator, int)
    */
   public boolean isSupported(Operator op, int nFlags)
   {
      if (SQLGenerator.isSupported(op))
      {
         if ((nFlags & Operator.NORMALIZE_EXPRESSION) != 0)
         {
            return true;
         }

         if (m_sopVisitor == null)
         {
            m_sopVisitor = new SupportedOperatorVisitor();
         }

         return m_sopVisitor.isSupported(op);
      }

      return false;
   }

   /**
    * Determines if an operator is supported in a table join.
    * This is restriction is in addition to isSupported().
    * @param query The joined query.
    * @param op The operator to test.
    * @return True if the operator is supported.
    */
   public boolean isJoinSupported(Query query, Operator op)
   {
      if (op instanceof AttributeOperator)
      {
         Source source = ((AttributeOperator)op).getSource();

         return !(source instanceof Field) || source.getMapping() == query.getMapping();
      }

      return true;
   }

   /**
    * Determines if the adapter is compatible with the connection.
    * @param con The JDBC connection.
    * @param db The relational database. Can be null.
    * @return True if the adapter is compatible with the connection.
    */
   protected boolean isCompatible(Connection con, RelationalDatabase db) throws SQLException
   {
      if (db != null && !getClass().equals(db.getAdapter().getClassObject()))
      {
         return false;
      }

      String sDriver = (db == null) ? null : db.getDriver();

      if (StringUtil.isEmpty(sDriver))
      {
         return true;
      }

      int i = sDriver.indexOf('.');

      if (i < 0)
      {
         return true;
      }

      int n = 1;

      if (i <= 2)
      {
         n = 3;
      }
      else if ("biz com edu net org gov mil int".indexOf(sDriver.substring(0, i)) >= 0)
      {
         n = 2;
      }

      for (++i; n != 0; --n)
      {
         int k = sDriver.indexOf('.', i);

         if (k < 0)
         {
            i = sDriver.length();

            break;
         }

         i = k + 1;
      }

      return sDriver.regionMatches(0, unwrap(con).getClass().getName(), 0, i);
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getVersion(nexj.core.meta.persistence.Schema)
    */
   public SchemaVersion getVersion(Schema schema) throws PersistenceException
   {
      if (schema == null)
      {
         return null;
      }

      RelationalDatabase db = (RelationalDatabase)schema.getDataSource();
      Table table = ((RelationalSchema)schema).getVersionTable();
      Metadata metadata = m_context.getMetadata();

      if (metadata.getNamespace() == null || table == null)
      {
         return null;
      }

      Column namespaceColumn = table.getColumn("namespace");
      Column versionColumn = table.getColumn("version");
      Column stepColumn = table.getColumn("step");
      Column upgradableColumn = table.getColumn("upgradable");
      Column testColumn = table.getColumn("test");
      StringBuffer buf = new StringBuffer(128);

      buf.append("select ");
      appendColumn(buf, namespaceColumn);
      buf.append(", ");
      appendColumn(buf, versionColumn);
      buf.append(", ");
      appendColumn(buf, stepColumn);
      buf.append(", ");
      appendColumn(buf, upgradableColumn);
      buf.append(", ");
      appendColumn(buf, testColumn);
      buf.append(" from ");
      appendTable(buf, table);

      String sSQL = buf.toString();
      SQLConnection connection = null;
      PreparedStatement stmt = null;
      ResultSet rs = null;

      try
      {
         connection = getConnection();

         log(sSQL);

         Connection con = connection.getConnection();

         if (!isCompatible(con, db))
         {
            String sDBName = con.getMetaData().getDatabaseProductName();
            String sDBVersion = con.getMetaData().getDatabaseProductVersion();

            throw new PersistenceException("err.persistence.adapterMismatch",
               new Object[]{db.getName(), db.getAdapter().getName(),
                  (sDBVersion.contains(sDBName)) ? sDBVersion : (sDBName + " " + sDBVersion)});
         }

         stmt = con.prepareStatement(sSQL);
         rs = executeQuery(stmt);

         if (!rs.next())
         {
            throw new PersistenceException("err.persistence.noStorageVersion",
               new Object[]{db.getName()});
         }

         SchemaVersion version = new SchemaVersion();

         version.setNamespace(Primitive.toString(getBind(namespaceColumn).getValue(rs, 0, this)));
         version.setVersion(Primitive.toString(getBind(versionColumn).getValue(rs, 1, this)));

         Integer step = Primitive.toInteger(getBind(stepColumn).getValue(rs, 2, this));

         version.setStep((step == null) ? -1 : step.intValue());

         Boolean upgradable = Primitive.toBoolean(getBind(upgradableColumn).getValue(rs, 3, this));

         version.setUpgradable((upgradable == null) ? false : upgradable.booleanValue());

         Boolean test = Primitive.toBoolean(getBind(testColumn).getValue(rs, 4, this));

         version.setTest((test == null) ? false : test.booleanValue());

         // has to be done while still have data in ResultSet on DB2
         Boolean unicode = isUnicode((RelationalSchema)schema, rs, 1); // 1 == "namespace" column
         boolean bUnicode = db.isUnicode();

         if (unicode == null)
         {
            throw new PersistenceException(
               "err.persistence.sql.unicodeUnknown",
               new Object[]{db.getName(), Boolean.valueOf(bUnicode)});
         }

         if (unicode.booleanValue() != bUnicode)
         {
            throw new PersistenceException(
               "err.persistence.sql.unicodeMismatch",
               new Object[]{db.getName(), Boolean.valueOf(bUnicode)});
         }

         if (rs.next())
         {
            throw new PersistenceException("err.persistence.ambiguousStorageVersion",
               new Object[]{db.getName()});
         }

         return version;
      }
      catch (SQLException e)
      {
         throw new PersistenceException("err.persistence.checkVersion",
            new Object[]{db.getName()}, e);
      }
      finally
      {
         close(rs);
         close(stmt);

         if (connection != null)
         {
            connection.decRef();
         }
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#setVersion(nexj.core.meta.persistence.Schema, nexj.core.persistence.SchemaVersion)
    */
   public void setVersion(Schema schema, SchemaVersion version) throws PersistenceException
   {
      if (version.getNamespace() == null ||
         version.getVersion() == null)
      {
         throw new NullPointerException();
      }

      if (!version.isUpgradable())
      {
         throw new IllegalStateException();
      }

      if (schema == null)
      {
         return;
      }

      Table table = ((RelationalSchema)schema).getVersionTable();

      if (table == null)
      {
         return;
      }

      Column namespaceColumn = table.getColumn("namespace");
      Column versionColumn = table.getColumn("version");
      Column stepColumn = table.getColumn("step");
      StringBuffer buf = new StringBuffer(128);

      buf.append("update ");
      appendTable(buf, table);
      buf.append(" set ");
      appendColumn(buf, versionColumn);
      buf.append(" = ");
      appendBind(buf, 0);
      buf.append(", ");
      appendColumn(buf, stepColumn);
      buf.append(" = ");
      appendBind(buf, 1);
      buf.append(" where ");
      appendColumn(buf, namespaceColumn);
      buf.append(" = ");
      appendBind(buf, 2);

      String sSQL = buf.toString();
      SQLConnection connection = null;
      PreparedStatement stmt = null;

      try
      {
         connection = getConnection();

         if (isLogging())
         {
            log(sSQL);
            logBindValue(0, version.getVersion());
            logBindValue(1, Primitive.createInteger(version.getStep()));
            logBindValue(2, version.getNamespace());
         }

         stmt = connection.getConnection().prepareStatement(sSQL);
         getBind(versionColumn).setValue(stmt, 0, version.getVersion(), this);
         getBind(stepColumn).setValue(stmt, 1, Primitive.createInteger(version.getStep()), this);
         getBind(namespaceColumn).setValue(stmt, 2, version.getNamespace(), this);

         if (executeUpdate(stmt) != 1)
         {
            throw new PersistenceException("err.persistence.noStorageNamespace",
               new Object[]{schema.getDataSource().getName(), version.getNamespace()});
         }
      }
      catch (SQLException e)
      {
         throw new PersistenceException("err.persistence.setVersion",
            new Object[]{schema.getDataSource().getName()}, e);
      }
      finally
      {
         close(stmt);

         if (connection != null)
         {
            connection.decRef();
         }
      }
   }

   /**
    * Obtains the original SQL connection.
    * @param connection The connection wrapper.
    * @return The original SQL connection.
    */
   protected static Connection unwrap(Connection connection) throws SQLException
   {
      if (connection == null)
      {
         return null;
      }

      if (connection instanceof ConnectionWrapper)
      {
         connection = ((ConnectionWrapper)connection).getConnection();
      }

      if (connection instanceof SQLHookConnection)
      {
         return unwrap(((SQLHookConnection)connection).getConnection());
      }

      if (connection instanceof nexj.core.rpc.sql.ra.SQLConnection)
      {
         return ((nexj.core.rpc.sql.ra.SQLConnection)connection).getConnection();
      }

      try
      {
         if (s_getUnderlyingConnection != null &&
             s_getUnderlyingConnection.getDeclaringClass().isAssignableFrom(connection.getClass()))
         {
            return (Connection)s_getUnderlyingConnection.invoke(connection, null);
         }

         if (s_getNativeConnection != null &&
             s_wsJDBCConnection.isAssignableFrom(connection.getClass()))
         {
            return (Connection)s_getNativeConnection.invoke(null, new Object[]{connection});
         }

         // Note: Class.isAssignableFrom(...) returns incorrect results under Sun JVM
         // i.e. class compiled under Java 1.5 not implementing a Java 1.6 interface will be marked
         //      as implementing the Java 1.6 interface when run via a Sun Java 1.6 JVM
         // e.g. java.sql.Wrapper from JRE1.6 not implemented on jBoss4 WrappedStatement
         if (s_unwrap != null && // Java 1.6 java.sql.Wrapper
             s_unwrap.getDeclaringClass().isAssignableFrom(connection.getClass()))
         {
            return (Connection)s_unwrap.invoke(connection, new Object[]{Connection.class});
         }
      }
      catch (Exception e)
      {
         SQLException x;

         if (e instanceof InvocationTargetException &&
            ((InvocationTargetException)e).getTargetException() instanceof SQLException)
         {
            x = (SQLException)((InvocationTargetException)e).getTargetException();
         }
         else
         {
            x = new SQLException("Unable to unwrap connection");
            x.initCause(e);
         }

         throw x;
      }

      return connection;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#upgrade(nexj.core.meta.persistence.SchemaUpgrade, nexj.core.meta.upgrade.UpgradeState, nexj.core.persistence.SchemaVersion)
    */
   public void upgrade(SchemaUpgrade upgrade, UpgradeState state, SchemaVersion version) throws PersistenceException
   {
      SQLConnection connection = null;

      try
      {
         connection = getConnection();

         SQLSchemaManager manager = createSchemaManager((RelationalDatabase)upgrade.getDataSource());
         RelationalSchemaUpgradeState rstate = (RelationalSchemaUpgradeState)state;

         manager.setSQLAppender(new SQLSchemaManager.SQLConnectionAppender(connection.getConnection()));

         for (;;)
         {
            int nStep = version.getStep();

            if (nStep < 0)
            {
               break;
            }

            manager.upgrade((RelationalSchemaUpgrade)upgrade, rstate, version);

            if (version.getStep() == nStep)
            {
               throw new IllegalStateException();
            }

            setVersion(rstate.getSchema(), version);
         }
      }
      catch (RuntimeException e)
      {
         throw new PersistenceException(
            "err.persistence.upgrade.apply",
            new Object[]{
               version.getNamespace(),
               version.getVersion(),
               Primitive.createInteger(version.getStep())},
            e);
      }
      finally
      {
         if (connection != null)
         {
            connection.decRef();
         }
      }
   }

   /**
    * Gets SQL statement to execute when a connection is first established.
    * @return The initial SQL string; null to issue no initial statement.
    */
   public String getInitialSQL()
   {
      return null;
   }

   /**
    * Gets SQL statement to execute to test that the connection is still valid
    * after it has been retrieved out of a connection pool.
    * @return The test SQL string.
    */
   public abstract String getTestSQL();

   /**
    * @return The calendar for timestamp conversion.
    */
   protected Calendar getCalendar()
   {
      if (m_calendar == null)
      {
         m_calendar = Calendar.getInstance(TZ.UTC, Locale.ENGLISH);
      }

      return m_calendar;
   }

   /**
    * Template method to return a bind factory from a primitive type.
    * @param type The primitive type.
    * @return The bind factory.
    */
   protected BindFactory getBindFactory(Primitive type)
   {
      return s_bindFactoryArray[type.getOrdinal()];
   }

   /**
    * Gets the bind for a given column.
    * @param column The column for which to get the bind.
    * @return The bind.
    */
   public Bind getBind(Column column)
   {
      return getBindFactory(getConversionMapper().getType(column.getType())).create(column);
   }

   /**
    * Gets the bind for a given primitive type.
    * @param type The primitive type for which to get the bind.
    * @return The bind.
    */
   public Bind getBind(Primitive type)
   {
      return getBindFactory(getConversionMapper().getType(type)).create(type);
   }

   /**
    * Gets the internal representation of a given constant.
    * @param type The constant type.
    * @param value The constant value.
    * @return The converted constant.
    */
   public Object getInternal(Primitive type, Object value)
   {
      Primitive toType = getConversionMapper().getType(type);

      if (toType == type)
      {
         return value;
      }

      return toType.getConverter(type).invoke(value);
   }

   /**
    * Converts a primitive value to a column value type.
    * @param column The table column.
    * @param value The value to convert.
    * @return The converted value.
    */
   public final Object toBind(Column column, Object value)
   {
      Converter converter = getConverter(column.getValueType(), column);

      if (converter == null)
      {
         return value;
      }

      return converter.getInverseFunction().invoke(value);
   }

   /**
    * Converts a bind primitive value to a column value type.
    * @param column The table column.
    * @param value The value to convert.
    * @return The converted value.
    */
   public final Object toValue(Column column, Object value)
   {
      Converter converter = getConverter(column.getValueType(), column);

      if (converter == null)
      {
         return value;
      }

      return converter.getForwardFunction().invoke(value);
   }

   /**
    * Retrieve OperatorAppender suitable for use of the specific operator with this adapter.
    * @param operator The operator to retrieve the appender for.
    * @return The preferred operator appender on null of no override required.
    */
   public OperatorAppender findOperatorAppender(Operator operator)
   {
      return null;
   }

   /**
    * Determines if the output columns should be aliased to ensure their uniqueness.
    * @param The top level query.
    * @return True if the columns should be aliased.
    */
   public boolean isColumnAliased(Query query)
   {
      return false;
   }

   /**
    * Appends any additional output fields to a string buffer.
    * @param buf The string buffer.
    * @param query The query providing the output fields.
    * @param gen The SQL generator.
    */
   public void appendExtraOutputFields(StringBuffer buf, Query query, SQLGenerator gen)
   {
   }

   /**
    * @return The keyword table.
    */
   protected Set getKeywords()
   {
      return KEYWORDS;
   }

   /**
    * Determines if a given identifier is a keyword.
    * @param sName The identifier.
    * @return True if the identifier is a keyword.
    */
   public boolean isKeyword(String sName)
   {
      return getKeywords().contains(sName);
   }

   /**
    * Appends a quoted identifier to a string buffer.
    * @param buf The destination buffer.
    * @param sName The identifier to append.
    */
   public void appendQuoted(StringBuffer buf, String sName)
   {
      buf.append('"');
      StringUtil.appendUpperCase(buf, sName);
      buf.append('"');
   }

   /**
    * Appends a column name, possibly quoted, to a string buffer.
    * @param buf The destination buffer.
    * @param column The column to append.
    */
   public void appendColumn(StringBuffer buf, Column column)
   {
      String sName = column.getName();

      if (isKeyword(sName))
      {
         appendQuoted(buf, sName);
      }
      else
      {
         buf.append(sName);
      }
   }

   /**
    * Appends a relational object name, possibly quoted, to a string buffer.
    * @param buf The destination buffer.
    * @param Object The object to append.
    * @param sOwner The default owner. Can be null.
    * @param sSuffix The table name suffix. Can be null.
    * @param bQuote True to quote the keywords.
    */
   public void appendRelationalObject(StringBuffer buf, RelationalObject object,
      String sOwner, String sSuffix, boolean bQuote)
   {
      if (object.getOwnerName() != null)
      {
         sOwner = object.getOwnerName();
      }

      if (!StringUtil.isEmpty(sOwner))
      {
         if (bQuote && isKeyword(sOwner))
         {
            appendQuoted(buf, sOwner);
         }
         else
         {
            buf.append(sOwner);
         }

         buf.append('.');
      }

      String sName = object.getObjectName();

      if (sSuffix == null && bQuote && isKeyword(sName))
      {
         appendQuoted(buf, sName);
      }
      else
      {
         buf.append(sName);
      }

      if (sSuffix != null)
      {
         buf.append(sSuffix);
      }
   }

   /**
    * Appends a table name, possibly quoted, to a string buffer.
    * @param buf The destination buffer.
    * @param table The table to append.
    * @param sOwner The default owner. Can be null.
    * @param sSuffix The table name suffix. Can be null.
    * @param bQuote True to quote the keywords.
    */
   public void appendTable(StringBuffer buf, Table table, String sOwner, String sSuffix, boolean bQuote)
   {
      if (table.getAlias() != null)
      {
         assert sSuffix == null;

         buf.append(table.getAlias());
      }
      else
      {
         appendRelationalObject(buf, (RelationalObject)table, sOwner, sSuffix, bQuote);
      }
   }

   /**
    * Appends a table name, possibly quoted, to a string buffer.
    * @param buf The destination buffer.
    * @param table The table to append.
    * @param sOwner The default owner. Can be null.
    */
   public final void appendTable(StringBuffer buf, Table table, String sOwner)
   {
      appendTable(buf, table, sOwner, null, true);
   }

   /**
    * Appends a table name, possibly quoted, to a string buffer.
    * @param buf The destination buffer.
    * @param table The table to append.
    */
   public final void appendTable(StringBuffer buf, Table table)
   {
      appendTable(buf, table, null, null, true);
   }

   /**
    * Gets a full table name.
    * @param table The table.
    * @param sOwner The default owner. Can be null.
    * @param sSuffix The table name suffix. Can be null.
    * @param bQuote True to quote the keywords.
    */
   public final String getTableName(Table table, String sOwner, String sSuffix, boolean bQuote)
   {
      StringBuffer buf = new StringBuffer(table.getName().length() + 16);

      appendTable(buf, table, sOwner, sSuffix, bQuote);

      return buf.toString();
   }

   /**
    * Gets a full table name.
    * @param table The table.
    * @param sOwner The default owner. Can be null.
    */
   public final String getTableName(Table table, String sOwner)
   {
      return getTableName(table, sOwner, null, true); 
   }

   /**
    * Appends a bind placeholder to a string buffer.
    * @param buf The destination buffer.
    * @param nOrdinal The placeholder ordinal number (0-based).
    */
   public void appendBind(StringBuffer buf, int nOrdinal)
   {
      buf.append('?');
   }

   /**
    * Appends a bind placeholder to a string buffer.
    * Used where column type is unknown from immediate query context.
    * Override in adapters that need to know the bind type.
    * @param buf The destination buffer.
    * @param nOrdinal The placeholder ordinal number (0-based).
    * @param type The value type.
    * @param value The actual value to be bound to the statement (null == unknown)
    */
   public void appendBind(StringBuffer buf, int nOrdinal, Primitive type, Object value)
   {
      appendBind(buf, nOrdinal);
   }

   /**
    * Appends a bind placeholder to a string buffer.
    * Used where column type is unknown from immediate query context.
    * Override in adapters that need to know the bind type.
    * @param buf The destination buffer.
    * @param nOrdinal The placeholder ordinal number (0-based).
    * @param column The column the value is meant for.
    */
   public void appendBind(StringBuffer buf, int nOrdinal, Column column)
   {
      appendBind(buf, nOrdinal);
   }

   /**
    * Appends a case-converted bind placeholder to a string buffer.
    * @param buf The destination buffer.
    * @param nOrdinal The placeholder ordinal number (0-based).
    * @param column The column the value is meant for.
    */
   public void appendCaseConvertedBind(StringBuffer buf, int nOrdinal, Column column)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * Appends a case-converted literal to a string buffer.
    * @param buf The destination buffer.
    * @param sLiteral The literal to append.
    */
   public void appendCaseConvertedLiteral(StringBuffer buf, String sLiteral)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * Converts a wildcard expression to a database like pattern.
    * @param sPattern The wildcard expression (with * and ?).
    */
   public String getLikePattern(String sPattern)
   {
      int nCount = sPattern.length();
      StringBuffer patBuf = new StringBuffer(nCount + 8);

      for (int i = 0; i < nCount; ++i)
      {
         char ch = sPattern.charAt(i);

         switch (ch)
         {
            case '*':
               patBuf.append('%');
               break;

            case '?':
               patBuf.append('_');
               break;

            case '%':
            case '_':
            case '\\':
               patBuf.append('\\');
               patBuf.append(ch);
               break;

            default:
               if (isLikeReservedChar(ch))
               {
                  patBuf.append('\\'); // char defined in appendLikeEscape(StringBuffer)
               }

               patBuf.append(ch);
               break;
         }
      }

      return patBuf.toString();
   }

   /**
    * Does the argument represent a reserved character in a 'LIKE' statement pattern.
    * The characters '%', '_', and '/' are assumed to be reserved.
    * @param ch The character to examine.
    * @return Is this character reserved.
    */
   protected boolean isLikeReservedChar(char ch)
   {
      return '[' == ch || ']' == ch;
   }

   /**
    * Appends a like escape statement to a string buffer.
    * @param buf The destination buffer.
    */
   public void appendLikeEscape(StringBuffer buf)
   {
      buf.append(" escape '\\'");
   }

   /**
    * Appends the beginning of a like statement to a string buffer.
    * @param buf The destination buffer.
    */
   public void appendLikeStatement(StringBuffer buf)
   {
      buf.append(" like ");
   }

   /**
    * Appends the full-text search statement (e.g. "contains") that returns a
    * double precision floating point score [0..1] of each result,
    * or null for a non-matching record.
    * @param buf The buffer to append to.
    * @param sAlias The alias of the table containing the column.
    * @param column The column to do the full-text search on (same as for getMatchJoin()).
    * @param join The joined table SQLJoin struct (null if return from getMatchJoin() == null).
    * @param expression The full-text search expression to search for (same as for getMatchJoin()).
    */
   public abstract void appendMatchStatement(StringBuffer buf, String sAlias, Column column,
                                             SQLJoin join, Pair expression);

   /**
    * Gets the columns that should be prepended to an ORDER BY clause
    * to trick the database server into using an index in lieu of
    * performing a sort.
    *
    * @return The columns to prepend; null to prepend nothing.
    */
   public OrderByPrefix getOrderByPrefix(Query query)
   {
      return null;
   }

   /**
    * Appends the sort direction (asc/desc) to a string buffer.
    * @param buf The destination buffer.
    * @param op The sort operand.
    * @param bAscending True if the sort direction is ascending.
    */
   public void appendSortDirection(StringBuffer buf, Operator op, boolean bAscending)
   {
      if (!bAscending)
      {
         buf.append(" desc");
      }
   }

   /**
    * Appends a sort order conversion function prefix.
    * @param buf The destination buffer.
    * @param op The operand to convert.
    * @return The suffix to append or null.
    */
   public String appendSortPrefix(StringBuffer buf, Operator op)
   {
      return null;
   }

   /**
    * Appends a group order conversion function prefix.
    * @param buf The destination buffer.
    * @param op The operand to convert.
    * @return The suffix to append or null.
    */
   public String appendGroupPrefix(StringBuffer buf, Operator op)
   {
      return null;
   }

   /**
    * Appends a comparison conversion function prefix.
    * @param buf The destination buffer.
    * @param op The operand to convert.
    * @return The suffix to append or null.
    */
   public String appendComparisonPrefix(StringBuffer buf, Operator op)
   {
      return null;
   }

   /**
    * Appends a boolean-to-numerical expression conversion prefix.
    * @param buf The destination buffer.
    * @param op The operand to convert.
    * @return The suffix to append or null.
    */
   public String appendBooleanPrefix(StringBuffer buf, Operator op)
   {
      buf.append("(case when ");

      return " then 1 else 0 end)";
   }

   /**
    * Appends a numerical-to-boolean expression conversion prefix.
    * @param buf The destination buffer.
    * @param op The operand to convert.
    * @return The suffix to append or null.
    */
   public String appendLogicalPrefix(StringBuffer buf, Operator op)
   {
      buf.append('(');

      int i = buf.length();

      buf.append(" <> ");
      appendLiteral(buf, Primitive.BOOLEAN, Boolean.FALSE);
      buf.append(')');

      String sSuffix = buf.substring(i);

      buf.setLength(i);

      return sSuffix;
   }

   /**
    * Appends a string-length function prefix.
    * @param buf The destination buffer.
    * @param op The string-length operator.
    * @return The suffix to append.
    */
   public abstract String appendStringLengthPrefix(StringBuffer buf, FunctionOperator op);

   /**
    * Appends a substring function prefix.
    * @param buf The destination buffer.
    * @param op The substring operator.
    * @return The suffix to append.
    */
   public abstract String appendSubstringPrefix(StringBuffer buf, FunctionOperator op);

   /**
    * Appends a count aggregate function prefix.
    * @param buf The destination buffer.
    * @param op The count operator.
    * @return The suffix to append.
    */
   public String appendCountPrefix(StringBuffer buf, AggregateOperator op)
   {
      buf.append("count(");

      return ")";
   }

   /**
    * Appends a sum aggregate function prefix.
    * @param buf The destination buffer.
    * @param op The sum operator.
    * @return The suffix to append.
    */
   public String appendSumPrefix(StringBuffer buf, AggregateOperator op)
   {
      buf.append("sum(");

      return ")";
   }

   /**
    * Appends an average aggregate function prefix.
    * @param buf The destination buffer.
    * @param op The average operator.
    * @return The suffix to append.
    */
   public String appendAveragePrefix(StringBuffer buf, AggregateOperator op)
   {
      buf.append("avg(");

      return ")";
   }

   /**
    * Appends a minimum aggregate function prefix.
    * @param buf The destination buffer.
    * @param op The minimum operator.
    * @return The suffix to append.
    */
   public String appendMinimumPrefix(StringBuffer buf, AggregateOperator op)
   {
      buf.append("min(");

      return ")";
   }

   /**
    * Appends a maximum aggregate function prefix.
    * @param buf The destination buffer.
    * @param op The maximum operator.
    * @return The suffix to append.
    */
   public String appendMaximumPrefix(StringBuffer buf, AggregateOperator op)
   {
      buf.append("max(");

      return ")";
   }

   /**
    * Appends an ungroup aggregate function prefix.
    * @param buf The destination buffer.
    * @param type The expression type.
    * @return The suffix to append.
    */
   public String appendUngroupPrefix(StringBuffer buf, Primitive type)
   {
      buf.append("min(");

      return ")";
   }

   /**
    * Appends a sort, comparison etc suffix to a string buffer.
    * @param buf The destination buffer.
    * @param sSuffix The suffix to append.
    */
   public void appendSuffix(StringBuffer buf, String sSuffix)
   {
      if (sSuffix != null)
      {
         buf.append(sSuffix);
      }
   }

   /**
    * Determines if the operator is case-converted.
    * @param op The operator to test.
    * @return True if the operator is case-converted.
    */
   public boolean isCaseConverted(Operator op)
   {
      return false;
   }

   /**
    * Determines if the field is case-converted.
    * @param field The field to test.
    * @return True if the field is case-converted.
    */
   public boolean isCaseConverted(Field field)
   {
      return false;
   }

   /**
    * Determines if the column is case-converted.
    * @param column The column to test.
    * @return True if the column is case-converted.
    */
   public boolean isCaseConverted(Column column)
   {
      return false;
   }

   /**
    * Appends a case-converted column name.
    * @param buf The destination buffer.
    * @param column The column to append.
    */
   public void appendCaseConvertedColumn(StringBuffer buf, Column column)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * Determines if a value can be used as literal.
    * Some databases have limitations on the maximum length of a literal.
    * @param type The value primitive type.
    * @param value The value.
    * @return True if the value can be used as a literal.
    */
   public abstract boolean isLiteral(Primitive type, Object value);

   /**
    * Append a literal SQL sanitized string to the buffer (i.e. all quotes escaped)
    * @param buf The buffer to append to.
    * @param sValue The value to sanitize and append.
    */
   protected void appendLiteral(StringBuffer buf, String sValue)
   {
      buf.append('\'');

      for (int i = 0, nLength = sValue.length(); i < nLength; ++i)
      {
         char ch = sValue.charAt(i);

         if (ch == '\'')
         {
            buf.append('\'');
         }

         buf.append(ch);
      }

      buf.append('\'');
   }

   /**
    * Appends an SQL literal to a string buffer.
    * Should be overridden for Timestamp value when column.isTimeZoned() and outputting explicit TZ.
    * @param buf The output buffer.
    * @param column The column the value is meant to be used with.
    * @param value The value to append.
    */
   public void appendLiteral(StringBuffer buf, Column column, Object value)
   {
      // take timezone into account when outputting a literal Timestamp value
      // NOTE: value will have wrong TZ when outputting the value with an explicit timezone
      if (column.isTimeZoned())
      {
         value = rebase(
            (Timestamp)value,
            ((CalendarFactory)column.getConverter().getInstance(null))
               .createCalendar().getTimeZone());
      }

      Primitive type = column.getType();

      appendLiteral(buf, type, type.convert(value));
   }

   /**
    * Appends an SQL literal to a string buffer.
    * @param buf The output buffer.
    * @param type The value type.
    * @param value The value to append.
    */
   public abstract void appendLiteral(StringBuffer buf, Primitive type, Object value);

   /**
    * Appends the specified SQL type conversion expression to a string buffer.
    * @param buf The destination buffer.
    * @param op The operand to convert - a Field or Operator.
    * @param fromType The type from which to convert.
    * @param type The type to which to convert.
    * @param gen The SQLGenerator instance.
    */
   public abstract void appendTypeConversion(StringBuffer buf, Object op,
      Primitive fromType, Primitive type, SQLGenerator gen);

   /**
    * Appends a prefix hint to the SQL clause: HINT select ...
    * @param buf The query output buffer.
    * @param query The root query node.
    */
   public abstract void appendPrefixHint(StringBuffer buf, Query query);

   /**
    * Appends an infix hint to the SQL clause: select HINT ...
    * @param buf The query output buffer.
    * @param query The root query node.
    */
   public abstract void appendInfixHint(StringBuffer buf, Query query);

   /**
    * Appends a suffix hint to the SQL clause: select ... where ... order by ... HINT
    * @param buf The query output buffer.
    * @param query The root query node.
    */
   public abstract void appendSuffixHint(StringBuffer buf, Query query);

   /**
    * Appends a table hint to the SQL clause: select ... from table_1 as alias_1 HINT ...
    * @param buf The query output buffer.
    * @param join The table join.
    * @param query The join query node.
    */
   public abstract void appendTableHint(StringBuffer buf, SQLJoin join, Query query);

   /**
    * Appends an identity output prefix.
    * @param buf The statement output buffer.
    * @param work The work item.
    */
   public abstract void appendIdentityPrefix(StringBuffer buf, SQLInsert work);

   /**
    * Appends an identity column to the SQL insert statement column list.
    * @param buf The statement output buffer.
    * @param work The work item.
    * @return True if anything has been appended.
    */
   public abstract boolean appendIdentityColumn(StringBuffer buf, SQLInsert work);

   /**
    * Appends an identity value to the SQL insert statement value list.
    * @param buf The statement output buffer.
    * @param work The work item.
    * @return True if anything has been appended.
    */
   public abstract boolean appendIdentityValue(StringBuffer buf, SQLInsert work);

   /**
    * Appends an SQL statement outputting the last generated identity value for a given table.
    * @param buf The statement output buffer.
    * @param work The work item.
    * @return True if the resulting statement should be prepared as callable.
    */
   public abstract boolean appendIdentitySuffix(StringBuffer buf, SQLInsert work);

   /**
    * Binds an identity output parameter.
    * @param stmt The prepared statement.
    * @param work The SQL insert work item.
    * @throws SQLException if a JDBC error occurs.
    */
   public abstract void bindIdentity(PreparedStatement stmt, SQLInsert work) throws SQLException;

   /**
    * Gets the identity value from an executed statement.
    * @param stmt The prepared statement.
    * @param column The column storing the value.
    * @param work The SQL insert work item.
    * @throws SQLException if a JDBC error occurs.
    */
   public abstract Object getIdentityValue(PreparedStatement stmt, Column column, SQLInsert work) throws SQLException;

   /**
    * Appends an SQL block start for an update statement with a conditional check.
    * @param buf The statement output buffer.
    * @return False if conditional statements are not supported.
    */
   public abstract boolean appendNoRowsBlock(StringBuffer buf);

   /**
    * Appends the beginning of an SQL conditional that checks if no rows have been updated.
    * @param buf The statement output buffer.
    */
   public abstract void appendNoRowsStart(StringBuffer buf);

   /**
    * Appends the end of an SQL conditional that checks if no rows have been updated.
    * @param buf The statement output buffer.
    */
   public abstract void appendNoRowsEnd(StringBuffer buf);

   /**
    * @return True if batch mode is supported by the driver.
    */
   public abstract boolean isBatchSupported();

   /**
    * @return True if the driver returns real batch update counts,
    * as opposed to just success status.
    */
   public abstract boolean isBatchUpdateCountSupported();

   /**
    * Determines if a work item is batchable.
    * @param work The work item.
    * @return True if the work item is batchable.
    */
   public abstract boolean isBatchable(SQLWork work);

   /**
    * @return True if the driver requires fetching column values
    * in the order of query column declaration.
    * The official SQL Server driver has this bug.
    */
   public boolean isOrderedColumnFetch()
   {
      return false;
   }

   /**
    * @return The join syntax, one of the SQLGenerator.JOIN_* constants.
    */
   public int getJoinSyntax()
   {
      return SQLGenerator.JOIN_ANSI;
   }

   /**
    * @return The maximum count of elements in a list (e.g. in (expr1, ..., exprN)).
    */
   public int getMaxListSize()
   {
      return Integer.MAX_VALUE;
   }

   /**
    * @return The maximum supported count of bind parameters.
    */
   public int getMaxBindCount()
   {
      return Integer.MAX_VALUE;
   }

   /**
    * Rebase a timestamp from UTC to the specified TimeZone, i.e. the returned object will report
    * a TimeZone of GMT but will be offset from the original by the offset of the TimeZone.
    * NOTE: Unless the specified TimeZone is GMT, the TimeZone of the resulting timestamp will have
    *       the wrong value when output via a DateFormat object with an explicit TimeZone as part of
    *       the output.
    * @param ts The timestamp to rebase (null == return null).
    * @param tz The TimeZone to rebase to (null == GMT).
    * @return The timestamp rebased to the specified TimeZone.
    */
   protected Timestamp rebase(Timestamp ts, TimeZone tz)
   {
      if (ts == null || tz == null || tz.equals(TZ.UTC))
      {
         return ts;
      }

      long lTime = ts.getTime();
      Timestamp tsRebased = new Timestamp(lTime + tz.getOffset(lTime));

      tsRebased.setNanos(ts.getNanos());

      return tsRebased;
   }

   /**
    * Rounds up a list size to facilitate SQL statement caching.
    * @param nSize The list size.
    * @param nBind The bind parameter count.
    * @return The rounded up list size.
    */
   protected int roundUpListSize(int nSize, int nBindCount)
   {
      int nMask = 1;
      int nRoundedSize = nSize;

      while (nRoundedSize > 0)
      {
         nRoundedSize >>>= 1;
         nMask <<= 1;
      }

      nMask = (nMask - 1) >>> 4;
      nRoundedSize = nSize + nMask & ~nMask;

      return (getMaxBindCount() - nRoundedSize < nBindCount) ? nSize : nRoundedSize;
   }

   /**
    * Rounds up the maximum row count to facilitate SQL statement caching.
    * @param The maximum row count.
    * @return The rounded up count.
    */
   protected int roundUpMaxCount(int nMaxCount)
   {
      if (nMaxCount <= 8)
      {
         return nMaxCount;
      }

      return (nMaxCount + 0x07) & ~0x07;
   }

   /**
    * Converts an SQLException to a subclass of PersistenceException.
    * @param e The SQLException to convert.
    * @param workArray The array containing the work items. Can be null.
    * @param nStart The start index.
    * @param nEnd The end index (exclusive).
    * @return The converted exception.
    */
   public PersistenceException getException(SQLException e, Work[] workArray, int nStart, int nEnd)
   {
      if (isQueryTimeoutException(e))
      {
         return new QueryTimeoutException(e);
      }

      if (isDuplicateKeyException(e))
      {
         Instance firstInstance = null;
         Metaclass commonMetaclass = null;

         if (workArray != null)
         {
            for (int i = nStart; i < nEnd; i++)
            {
               Instance instance = workArray[i].getInstance();
               Metaclass metaclass = instance.getLazyMetaclass();

               if (commonMetaclass == null || metaclass.isUpcast(commonMetaclass))
               {
                  commonMetaclass = metaclass;
                  firstInstance = instance;
               }
               else if (!commonMetaclass.isUpcast(metaclass)) // Metaclasses of instances do not have a common parent; just use root for the first one as a guess.
               {
                  commonMetaclass = instance.getLazyMetaclass().getPersistenceRoot();

                  break;
               }
            }
         }

         return new DuplicateKeyException(firstInstance,
            (firstInstance == null) ? null :
            getUniqueKeys(getDuplicateKeyName(e),
            (RelationalMapping)firstInstance.getPersistenceMapping()), e);
      }

      if (isDateRangeException(e))
      {
         throw new ValueRangeException("err.persistence.dateRange",
            new Object[]{new Timestamp(getMinTime()), new Timestamp(getMaxTime())}, e);
      }

      if (e instanceof DataTruncation)
      {
         throw new ValueRangeException("err.persistence.valueRange", e);
      }

      if(isDeadlockException(e))
      {
         return new DeadlockException(e);
      }

      if (isLockTimeoutException(e))
      {
         return new LockTimeoutException(e);
      }

      if (isAvailabilityException(e))
      {
         return new AvailabilityException(null, e);
      }

      return new PersistenceException("err.persistence.sql", e);
   }

   /**
    * Determines the unique keys for a given index in a relational mapping.
    * @param sIndexName The name of the index for which to determine the unique keys.
    * @param mapping The relational mapping.
    * @return A list of unique keys.
    */
   private Pair getUniqueKeys(String sIndexName, RelationalMapping mapping)
   {
      if (sIndexName != null)
      {
         for (int nTable = 0; nTable < mapping.getTableCount(); ++nTable)
         {
            Table table = mapping.getTable(nTable);

            for (int nIndex = 0, nIndexCount = table.getIndexCount(); nIndex < nIndexCount; ++nIndex)
            {
               Index index = table.getIndex(nIndex);

               if (indexNameMatches(index, sIndexName))
               {
                  if (index.isUnique() && index.getIndexColumnCount() != 0)
                  {
                     Pair keys = mapping.getUniqueKeys(index);

                     if (keys != null)
                     {
                        return (Pair)keys.getHead();
                     }
                  }

                  return null;
               }
            }
         }
      }

      return null;
   }

   /**
    * Checks if a given vendor-specific SQL exception is a persistence store availability exception.
    * @param e The SQL exception.
    * @return True if this is an availability exception.
    */
   protected boolean isAvailabilityException(SQLException e)
   {
      String sCode = e.getSQLState();

      if (sCode == null)
      {
         return false;
      }

      return sCode.equals("01002") || sCode.equals("08001") || sCode.equals("08004") || 
         sCode.equals("08007") || sCode.equals("08900");
   }

   /**
    * Checks if a given vendor-specific SQL exception is a data range exception.
    * @param e The SQL exception.
    * @return True if this is a data range exception.
    */
   protected abstract boolean isDateRangeException(SQLException e);

   /**
    * Checks if a given vendor-specific SQL exception is a query timeout exception.
    * @param e The SQL exception.
    * @return True if this is a query timeout exception.
    */
   protected abstract boolean isQueryTimeoutException(SQLException e);

   /**
    * Checks if a given vendor-specific SQL exception is a duplicate key exception.
    * @param e The SQL exception.
    * @return true If this is a duplicate key exception.
    */
   protected abstract boolean isDuplicateKeyException(SQLException e);

   /**
    * Extracts a key name from a vendor-specific SQL exception; its message could be localized.
    * @param e An exception for which isDuplicateKeyException() is true.
    * @return The duplicate key name, or null if not found.
    */
   protected abstract String getDuplicateKeyName(SQLException e);

   /**
    * Matches a given index to its name in the persistent store.
    * This name is the one generated in SQLSchemaManager.getIndexName() class.
    * @param index The index to match.
    * @param sPhysicalName The physical index name.
    * @return True if the index name matches.
    * @see nexj.core.persistence.sql.SQLSchemaManager#getIndexName(String, String, String, boolean, boolean)
    */
   protected abstract boolean indexNameMatches(Index index, String sPhysicalName);

   /**
    * Checks if a given vendor-specific SQL exception is a deadlock exception.
    * @param e The SQL exception.
    * @return true If this is a deadlock exception.
    */
   protected abstract boolean isDeadlockException(SQLException e);

   /**
    * Checks if a given vendor-specific SQL exception is a lock timeout exception.
    * @param e The SQL exception.
    * @return true If this is a lock timeout exception.
    */
   protected abstract boolean isLockTimeoutException(SQLException e);

   /**
    * Matches a given index to its name in the persistent store.
    * @param sMetadataName Name of the key in metadata.
    * @param nMetaStart Starting position of the metadata name to be compared to the persistent key name.
    * @param sPhysicalName Name of the key in a persistent store.
    * @param sPrefix Physical name prefix. May be null.
    * @param bCaseInsensitive Whether a case insensitive comparison is required.
    * @return True if the index names in metadata and in a persistent store match.
    */
   protected boolean indexNameMatches(String sMetadataName, int nMetaStart, String sPhysicalName, String sPrefix, boolean bCaseInsensitive)
   {
      int i = 0;

      if (sPrefix != null)
      {
         if (!sPhysicalName.regionMatches(bCaseInsensitive, 0, sPrefix, 0, sPrefix.length()))
         {
            return false;
         }

         i += sPrefix.length();
      }

      if (sMetadataName.length() - nMetaStart != sPhysicalName.length() - i)
      {
         return false;
      }

      for (int k = 0; i < sPhysicalName.length(); ++i, ++k)
      {
         char chMeta = sMetadataName.charAt(k + nMetaStart);
         char chPhys = sPhysicalName.charAt(i);

         if (chPhys == '_')
         {
            if (chMeta != '_' && chMeta != '.')
            {
               return false;
            }
         }
         else
         {
            if (bCaseInsensitive)
            {
               if (Character.toLowerCase(chMeta) != Character.toLowerCase(chPhys))
               {
                  return false;
               }
            }
            else
            {
               if (chMeta != chPhys)
               {
                  return false;
               }
            }
         }
      }

      return true;
   }

   /**
    * Creates an SQLSchemaManager instance.
    * @return The schema manager instance.
    */
   public abstract SQLSchemaManager createSchemaManager();

   /**
    * Creates an SQLSchemaManager instance and initializes it with
    * the current fragment of the specified data source.
    * @param database The database. Can be null.
    * @return The schema manager instance.
    */
   public SQLSchemaManager createSchemaManager(RelationalDatabase database)
   {
      SQLSchemaManager manager = createSchemaManager();

      if (database != null)
      {
         manager.setFragment((RelationalDatabaseFragment)database
            .getFragment((m_context == null) ? null : m_context.getFragmentName()));
      }

      return manager;
   }

   /**
    * @return True if logging is enabled.
    */
   public final boolean isLogging()
   {
      return m_bDebug || m_logList != null;
   }

   /**
    * Sets the deferred logging flag.
    * @param bEnabled True to enable deferred logging.
    */
   public final void setLogDeferred(boolean bEnabled)
   {
      if (bEnabled)
      {
         if (m_logList == null)
         {
            m_logList = new ArrayList();
         }
      }
      else
      {
         m_logList = null;
      }
   }

   /**
    * @return True if logging is deferred.
    */
   public final boolean isLogDeferred()
   {
      return m_logList != null;
   }

   /**
    * @return A snapshot of the current deferred log. Can be null.
    */
   public final List getLog()
   {
      if (m_logList == null)
      {
         return null;
      }

      return (List)m_logList.clone();
   }

   /**
    * Logs a message.
    * @param message The message to log.
    */
   public final void log(Object message)
   {
      if (m_bDebug)
      {
         s_logger.debug(message);
      }

      if (m_logList != null)
      {
         m_logList.add(message);
      }
   }

   /**
    * Logs a bind value.
    * @param nOrdinal The bind value ordinal number.
    * @param value The bind value.
    */
   public final void logBindValue(int nOrdinal, Object value)
   {
      if (m_bDebug)
      {
         StringBuffer buf = new StringBuffer(64);

         appendBindValueLog(buf, nOrdinal, value);
         s_logger.debug(buf);
      }

      if (m_logList != null)
      {
         m_logList.add(Primitive.createInteger(nOrdinal));
         m_logList.add(value);
      }
   }

   /**
    * Logs a batch start.
    */
   public final void logBatch(int nOrdinal)
   {
      if (m_bDebug && nOrdinal > 0)
      {
         s_logger.debug("--- Batch " + nOrdinal + " ---");
      }
   }

   /**
    * Logs the deferred log statements and resets the log.
    * @param t Optional exception to log. Can be null.
    */
   public final void flushLog(Throwable t)
   {
      if (m_logList != null)
      {
         log(s_xlogger, Logger.DEBUG, m_logList);
         m_logList.clear();
      }

      if (t != null && s_xlogger.isDebugEnabled())
      {
         m_context.getMachine().updateStackTrace(t);
         s_xlogger.debug("Database error", t);
      }
   }

   /**
    * Resets the deferred exception log.
    */
   public final void clearLog()
   {
      if (m_logList != null)
      {
         m_logList.clear();
      }
   }

   /**
    * Appends a bind value log to a string buffer.
    * @param logger The destination logger.
    * @param nOrdinal The bind value ordinal number.
    * @param value The bind value.
    */
   protected void appendBindValueLog(StringBuffer buf, int nOrdinal, Object value)
   {
      buf.append("Bind[");
      buf.append(nOrdinal);
      buf.append("] = ");

      if (value instanceof String)
      {
         buf.append('\'');
         buf.append(value);
         buf.append('\'');
      }
      else if (value instanceof Binary)
      {
         ((Binary)value).appendTo(buf);
      }
      else
      {
         buf.append(Primitive.toString(value));
      }
   }

   /**
    * Logs deferred log statements.
    * @param logger The destination logger.
    * @param nLevel The log level, one of the Logger.* level constants.
    * @param logList The deferred log statements. Can be null.
    */
   public void log(Logger logger, int nLevel, List logList)
   {
      if (logList != null)
      {
         StringBuffer buf = null;
         int nLastBind = -1;
         int nBatch = 0;

         for (int i = 0, nCount = logList.size(); i < nCount; ++i)
         {
            Object message = logList.get(i);

            if (message instanceof Integer && i < nCount - 1)
            {
               if (buf == null)
               {
                  buf = new StringBuffer(64);
               }

               int nBind = ((Integer)message).intValue();

               if (nBind <= nLastBind)
               {
                  buf.setLength(0);
                  buf.append("--- Batch ");
                  buf.append(++nBatch);
                  buf.append("---");
                  logger.log(nLevel, buf);
               }

               buf.setLength(0);
               appendBindValueLog(buf, nBind, logList.get(++i));
               logger.log(nLevel, buf);
               nLastBind = nBind;
            }
            else
            {
               logger.log(nLevel, message);
               nLastBind = -1;
               nBatch = 0;
            }
         }
      }
   }

   // inner classes

   /**
    * SQL bind adapter.
    */
   public interface Bind
   {
      /**
       * Sets a bind value on a prepared statement.
       * @param stmt The prepared statement.
       * @param nOrdinal The bind variable number (0-based).
       * @param value The value to set. Can be null.
       * @param adapter The persistence adapter.
       */
      void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException;

      /**
       * Gets a bind value from a result set.
       * @param rs The result set.
       * @param nOrdinal The column number (0-based).
       * @param adapter The persistence adapter.
       * @return The value. Can be null.
       */
      Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException;
   }

   /**
    * Factory for creating binds for a particular data type.
    */
   protected interface BindFactory
   {
      /**
       * Creates a bind for a given column.
       * @param column The column.
       * @return The created bind.
       */
      Bind create(Column column);

      /**
       * Creates a bind for a primitive type.
       * @param type The primitive type.
       * @return The created bind.
       */
      Bind create(Primitive type);
   }

   /**
    * BindFactory that does not contain any logic to create the bind.
    */
   protected final static class SimpleBindFactory implements BindFactory
   {
      /**
       * The bind to return from the factory methods.
       */
      private Bind m_bind;

      /**
       * Creates a bind factory.
       * @param bind The bind to return from the factory methods.
       */
      public SimpleBindFactory(Bind bind)
      {
         m_bind = bind;
      }

      /**
       * @see nexj.core.meta.persistence.sql.Column.BindFactory#create(nexj.core.meta.persistence.sql.Column)
       */
      public Bind create(Column column)
      {
         return m_bind;
      }

      /**
       * @see nexj.core.meta.persistence.sql.Column.BindFactory#create(nexj.core.meta.Primitive)
       */
      public Bind create(Primitive type)
      {
         return m_bind;
      }
   }

   /**
    * Encapsulates columns that should be prepended to an ORDER BY clause
    * to trick the database server into using an index in lieu of
    * performing a sort.
    */
   protected static class OrderByPrefix
   {
      // attributes

      /**
       * The number of columns to prepend, starting with the first
       * column in the index.
       */
      protected int m_nColumnCount;

      /**
       * Indicates whether or not the sort order of the columns to
       * be prepended should be reversed to match the sort order
       * of the first column of the ORDER BY clause.
       *
       * E.g. if index is on (A ASC, B DESC, C ASC) and the
       * existing ORDER BY clause is "C DESC", then the new
       * clause should be "A DESC, B ASC, C DESC".
       */
      protected boolean m_bSortDirectionReversed;

      // associations

      /**
       * The index from which the columns to prepend should be taken.
       */
      protected Index m_index;

      /**
       * The query mapping for the specified index.
       */
      protected Object m_mapping;

      // constructors

      /**
       * Creates a new encapsulation of the columns that should be prepended to
       * an ORDER BY clause to trick the database server into using an
       * index in lieu of performing a sort.
       *
       * @param index The index from which the columns to prepend should be taken.
       * @param nColumnCount The number of columns to prepend.
       * @param bSortDirectionReversed True to invert the sort orders of the columns
       * being prepended.
       * @param mapping The index mapping.
       */
      public OrderByPrefix(
         Index index, int nColumnCount, boolean bSortDirectionReversed, Object mapping)
      {
         m_index = index;
         m_mapping = mapping;
         m_nColumnCount = nColumnCount;
         m_bSortDirectionReversed = bSortDirectionReversed;
      }

      // operations

      /**
       * Gets the number of columns to prepend. Columns will be prepended
       * starting from the first column in the index.
       *
       * @return The number of columns to prepend to the ORDER BY clause.
       */
      protected int getColumnCount()
      {
         return m_nColumnCount;
      }

      /**
       * Indicates whether or not the sort order of the columns to
       * be prepended should be reversed to match the sort order
       * of the first column of the ORDER BY clause.
       *
       * @return True to invert the sort orders of the columns being prepended;
       * false to leave them as-is.
       */
      protected boolean isSortDirectionReversed()
      {
         return m_bSortDirectionReversed;
      }

      /**
       * Gets the index from which the columns to prepend will be taken.
       *
       * @return The index from which the columns to prepend will be taken.
       */
      protected Index getIndex()
      {
         return m_index;
      }

     /**
      * @return The query mapping for the specified index.
      */
      protected Object getMapping()
      {
         return m_mapping;
      }
   }

   /**
    * Return a table representing a join required for the match (e.g. CONTAINS) operation.
    * The table must have its primary index set to the appropriate column names.
    * @param column The column to do the full-text search on.
    * @param expression The full-text search expression to search for.
    * @return The table required in join or null if none required.
    */
   public Table getMatchJoin(Column column, Pair expression)
   {
      return null;
   }

   /**
    * Sets proper fetch size for a prepared statement
    * @param stmt The statement to modify
    * @param query Query in use for statement
    * @throws SQLException if statement modification fails
    */
   public void setFetchSize(PreparedStatement stmt, Query query) throws SQLException
   {
      if (query.getMaxCount() >= 0)
      {
         stmt.setFetchSize(Math.min(256, query.getMaxCount() + query.getOffset()));
      }
   }

   /**
    * Visitor for determining whether a given operator is supported.
    */
   protected static class SupportedOperatorVisitor implements Operator.Visitor
   {
      // attributes

      /**
       * The operator parent.
       */
      protected Operator m_parent;

      // operations

      /**
       * Determines if an operator is supported by the database.
       * @param op The operator to test.
       * @return True if the operator is supported.
       */
      public boolean isSupported(Operator op)
      {
         m_parent = op;

         try
         {
            return op.visit(this, Operator.VISIT_PREORDER);
         }
         finally
         {
            m_parent = null;
         }
      }

      /**
       * @see nexj.core.persistence.Operator.Visitor#isEligible(nexj.core.persistence.Operator)
       */
      public boolean isEligible(Operator op)
      {
         return op.getParent() == m_parent;
      }

      /**
       * @see nexj.core.persistence.Operator.Visitor#visit(nexj.core.persistence.Operator)
       */
      public boolean visit(Operator op)
      {
         if (op instanceof AttributeOperator && op.getParent() == m_parent)
         {
            AttributeOperator aop = (AttributeOperator)op;

            if (!aop.isNoConversion() && aop.getSource() instanceof Field)
            {
               Field field = (Field)aop.getSource();

               if (field.getAttribute() != null)
               {
                  AttributeMapping mapping = field.getAttributeMapping();

                  if (mapping instanceof RelationalPrimitiveMapping &&
                     ((RelationalPrimitiveMapping)mapping).getColumn().isPostConverted())
                  {
                     return false;
                  }
               }
            }
         }

         return true;
      }
   }
}