// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Properties;

import nexj.core.meta.Component;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.PropertyDataSourceFragment;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;

/**
 * Relational database fragment metadata.
 */
public class RelationalDatabaseFragment extends PropertyDataSourceFragment
{
   // attributes

   /**
    * The warning timeout in milliseconds (0 for unlimited).
    * Queries taking longer than that will be logged as warnings.
    */
   protected long m_lWarningTimeout = 500;

   /**
    * The JNDI alias.
    */
   protected String m_sAlias;

   /**
    * The database server host.
    */
   protected String m_sHost;

   /**
    * The instance name.
    */
   protected String m_sInstance;

   /**
    * The database name.
    */
   protected String m_sDatabase;

   /**
    * The server user name.
    */
   protected String m_sUser;

   /**
    * The server password.
    */
   protected String m_sPassword;

   /**
    * The database server port.
    */
   protected int m_nPort = -1;

   /**
    * The minimum connection pool size.
    */
   protected int m_nMinPoolSize = 3;

   /**
    * The maximum connection pool size.
    */
   protected int m_nMaxPoolSize = 33;

   /**
    * The prepared SQL statement cache size.
    */
   protected int m_nStatementCacheSize = 1000;

   /**
    * The query timeout in seconds (0 for unlimited).
    */
   protected int m_nQueryTimeout = 30;

   /**
    * True if this fragment is the first fragment with a given alias.
    */
   protected boolean m_bFirst = true;

   // associations

   /**
    * The connection factory component.
    */
   protected Component m_component;

   // constructors

   /**
    * Constructs a default (nameless) fragment.
    */
   public RelationalDatabaseFragment()
   {
   }

   /**
    * Constructs a fragment.
    * @param sName The fragment name.
    */
   public RelationalDatabaseFragment(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Sets the JNDI alias.
    * @param sAlias The JNDI alias to set.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();
      m_sAlias = sAlias;
   }

   /**
    * @return The JNDI alias.
    */
   public String getAlias()
   {
      return m_sAlias;
   }

   /**
    * Sets the database server host.
    * @param sHost The database server host to set.
    */
   public void setHost(String sHost)
   {
      verifyNotReadOnly();
      m_sHost = sHost;
   }

   /**
    * @return The database server host.
    */
   public String getHost()
   {
      return m_sHost;
   }

   /**
    * Sets the database server port.
    * @param nPort The database server port to set.
    */
   public void setPort(int nPort)
   {
      verifyNotReadOnly();
      m_nPort = nPort;
   }

   /**
    * @return The database server port.
    */
   public int getPort()
   {
      return m_nPort;
   }

   /**
    * Sets the instance name.
    * @param sInstance The instance name to set.
    */
   public void setInstance(String sInstance)
   {
      verifyNotReadOnly();
      m_sInstance = sInstance;
   }

   /**
    * @return The instance name.
    */
   public String getInstance()
   {
      return m_sInstance;
   }

   /**
    * Sets the database name.
    * @param sDatabase The database name to set.
    */
   public void setDatabase(String sDatabase)
   {
      verifyNotReadOnly();
      m_sDatabase = sDatabase;
   }

   /**
    * @return The database name.
    */
   public String getDatabase()
   {
      return m_sDatabase;
   }
   /**
    * Sets the server user name.
    * @param sUser The server user name to set.
    */
   public void setUser(String sUser)
   {
      verifyNotReadOnly();
      m_sUser = sUser;
   }

   /**
    * @return The server user name.
    */
   public String getUser()
   {
      return m_sUser;
   }

   /**
    * Sets the server password.
    * @param sPassword The server password to set.
    */
   public void setPassword(String sPassword)
   {
      verifyNotReadOnly();
      m_sPassword = sPassword;
   }

   /**
    * @return The server password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * Sets the minimum connection pool size.
    * @param nMinPoolSize The minimum connection pool size to set.
    */
   public void setMinPoolSize(int nMinPoolSize)
   {
      verifyNotReadOnly();
      m_nMinPoolSize = nMinPoolSize;
   }

   /**
    * @return The minimum connection pool size.
    */
   public int getMinPoolSize()
   {
      return m_nMinPoolSize;
   }

   /**
    * Sets the maximum connection pool size.
    * @param nMaxPoolSize The maximum connection pool size to set.
    */
   public void setMaxPoolSize(int nMaxPoolSize)
   {
      verifyNotReadOnly();

      if (nMaxPoolSize < m_nMinPoolSize)
      {
         throw new MetadataException("err.meta.persistence.maxPoolSize",
            new Object[]{Primitive.createInteger(m_nMinPoolSize),
               Primitive.createInteger(m_nMaxPoolSize), getName()});
      }

      m_nMaxPoolSize = nMaxPoolSize;
   }

   /**
    * @return The maximum connection pool size.
    */
   public int getMaxPoolSize()
   {
      return m_nMaxPoolSize;
   }

   /**
    * Sets the prepared SQL statement cache size.
    * @param nStatementCacheSize The prepared SQL statement cache size to set.
    */
   public void setStatementCacheSize(int nStatementCacheSize)
   {
      verifyNotReadOnly();
      m_nStatementCacheSize = nStatementCacheSize;
   }

   /**
    * @return The prepared SQL statement cache size.
    */
   public int getStatementCacheSize()
   {
      return m_nStatementCacheSize;
   }

   /**
    * Sets the query timeout in seconds.
    * @param nQueryTimeout The query timeout to set.
    */
   public void setQueryTimeout(int nQueryTimeout)
   {
      verifyNotReadOnly();

      if (nQueryTimeout == Integer.MAX_VALUE)
      {
         nQueryTimeout = 0;
      }

      m_nQueryTimeout = nQueryTimeout;
   }

   /**
    * @return The query timeout in seconds.
    */
   public int getQueryTimeout()
   {
      return m_nQueryTimeout;
   }

   /**
    * Sets whether or not this fragment is the first fragment with a given alias.
    * @param bFirst True if this fragment is the first fragment with a given alias.
    */
   public void setFirst(boolean bFirst)
   {
      verifyNotReadOnly();
      m_bFirst = bFirst;
   }

   /**
    * Gets whether or not this fragment is the first fragment with a given alias.
    * @return True if this fragment is the first fragment with a given alias.
    */
   public boolean isFirst()
   {
      return m_bFirst;
   }

   /**
    * Sets the warning timeout in milliseconds.
    * @param lWarningTimeout The warning timeout in milliseconds to set (0 for unlimited).
    */
   public void setWarningTimeout(long lWarningTimeout)
   {
      verifyNotReadOnly();

      if (lWarningTimeout == Long.MAX_VALUE)
      {
         lWarningTimeout = 0;
      }

      m_lWarningTimeout = lWarningTimeout;
   }

   /**
    * @return The warning timeout in milliseconds.
    */
   public long getWarningTimeout()
   {
      return m_lWarningTimeout;
   }

   /**
    * Sets the connection factory component.
    * @param component The connection factory component to set.
    */
   public void setConnectionFactory(Component component)
   {
      verifyNotReadOnly();
      m_component = component;
   }

   /**
    * @return The connection factory component.
    */
   public Component getConnectionFactory()
   {
      return m_component;
   }

   /**
    * Sets the default properties for the supported drivers.
    * @param nContainer The container type, one of the J2EEUtil constants.
    */
   public void setDefaultProperties(int nContainer)
   {
      verifyNotReadOnly();

      RelationalDatabase db = (RelationalDatabase)m_dataSource;
      String sDriver = db.getDriver();

      if (sDriver != null)
      {
         if (sDriver.equals("com.microsoft.sqlserver.jdbc.SQLServerXADataSource"))
         {
            m_props.addDefaultProperty("applicationName", SysUtil.CAPTION);
            m_props.addDefaultProperty("sendStringParametersAsUnicode", String.valueOf(db.isUnicode()));
            m_props.addDefaultProperty("packetSize", "32767");
            m_props.addDefaultProperty("responseBuffering", "adaptive");

            if (m_sHost != null)
            {
               m_props.addDefaultProperty("serverName", m_sHost + ((m_sInstance == null) ? "" : "\\" + m_sInstance));

            }

            if (m_nPort >= 0)
            {
               m_props.addDefaultProperty("portNumber", String.valueOf(m_nPort));
            }

            if (m_sInstance != null)
            {
               m_props.addDefaultProperty("instance", m_sInstance);
            }

            if (m_sDatabase != null)
            {
               m_props.addDefaultProperty("databaseName", m_sDatabase);
            }
         }
         else if (sDriver.equals("net.sourceforge.jtds.jdbcx.JtdsDataSource"))
         {
            m_props.addDefaultProperty("sendStringParametersAsUnicode", String.valueOf(db.isUnicode()));
            m_props.addDefaultProperty("appName", SysUtil.CAPTION);
            m_props.addDefaultProperty("lobBuffer", "10485760");
            m_props.addDefaultProperty("bufferMaxMemory", "65536");
            m_props.addDefaultProperty("maxStatements", "0");

            if (m_dataSource.getAdapter().getName().startsWith("Sybase"))
            {
               m_props.addDefaultProperty("prepareSql", "1");
               m_props.addDefaultProperty("serverType", "2");
            }
            else
            {
               m_props.addDefaultProperty("prepareSql", "3");
            }

            m_props.addDefaultProperty("xaEmulation", "true"); // TODO: Change when JTDS is fixed

            if (m_sHost != null)
            {
               m_props.addDefaultProperty("serverName", m_sHost);
            }

            if (m_nPort >= 0)
            {
               m_props.addDefaultProperty("portNumber", String.valueOf(m_nPort));
            }

            if (m_sInstance != null)
            {
               m_props.addDefaultProperty("instance", m_sInstance);
            }

            if (m_sDatabase != null)
            {
               m_props.addDefaultProperty("databaseName", m_sDatabase);
            }
         }
         else if (sDriver.equals("oracle.jdbc.xa.client.OracleXADataSource"))
         {
            Properties props = new Properties();

            // The default value of defaultNChar is "false" and since the framework now uses varchar2
            // for Unicode databases, it is not set explicitly, which would have been needed
            // for nvarchar2 to avoid character conversion
            // (indicated by SYS_OP_C2C("FIRSTNAME$")=:<bind> in the query plan)
            props.setProperty("defaultNChar", String.valueOf(db.isUnicode()));

            props.setProperty("processEscapes", "false");

            m_props.addDefaultProperty("connectionProperties", PropertyUtil.toString(props));

            if (m_props.findProperty("URL") == null)
            {
               m_props.addDefaultProperty("driverType", "thin");

               if (m_sHost != null)
               {
                  m_props.addDefaultProperty("serverName", m_sHost);
               }

               int nPort = m_nPort;

               if (nPort < 0)
               {
                  nPort = 1521;
               }

               m_props.addDefaultProperty("portNumber", String.valueOf(nPort));

               if (m_sDatabase != null)
               {
                  m_props.addDefaultProperty("serviceName", m_sDatabase);
               }
            }
         }
         else if (sDriver.equals("com.sybase.jdbc3.jdbc.SybXADataSource"))
         {
            if (m_sHost != null)
            {
               m_props.addDefaultProperty("serverName", m_sHost);
            }

            if (m_nPort >= 0)
            {
               m_props.addDefaultProperty("portNumber", String.valueOf(m_nPort));
            }

            if (m_sDatabase != null)
            {
               m_props.addDefaultProperty("databaseName", m_sDatabase);
            }

            // Unichar sending should be disabled if not using unicode.
            m_props.addDefaultProperty("DISABLE_UNICHAR_SENDING", (db.isUnicode()) ? "false" : "true");
            m_props.addDefaultProperty("ESCAPE_PROCESSING_DEFAULT", "false");
            m_props.addDefaultProperty("APPLICATIONNAME", SysUtil.CAPTION);
         }
         else if (sDriver.equals("com.mysql.jdbc.jdbc2.optional.MysqlXADataSource"))
         {
            if (m_sHost != null)
            {
               m_props.addDefaultProperty("serverName", m_sHost);
            }

            if (m_nPort >= 0)
            {
               m_props.addDefaultProperty("portNumber", String.valueOf(m_nPort));
            }

            if (m_sDatabase != null)
            {
               m_props.addDefaultProperty("databaseName", m_sDatabase);
            }

            // allow use of multiple ; separated queries (e.g. select last_insert_id() after insert)
            m_props.addDefaultProperty("allowMultiQueries", "true");

            // enable use of server side cursors (necessary for setFetchSize() to work)
            m_props.addDefaultProperty("useCursorFetch", "true");

            // enable use of server side prepared statements (necessary for setFetchSize() to work)
            m_props.addDefaultProperty("useServerPrepStmts", "true");
         }
         else if (sDriver.equals("com.ibm.db2.jcc.DB2XADataSource"))
         {
            if (m_sHost != null)
            {
               m_props.addDefaultProperty("serverName", m_sHost);
            }

            if (m_nPort >= 0)
            {
               m_props.addDefaultProperty("portNumber", String.valueOf(m_nPort));
            }

            if (m_sDatabase != null)
            {
               m_props.addDefaultProperty("databaseName", m_sDatabase);
            }

            // The JDBC connectivity type of a data source
            m_props.addDefaultProperty("driverType", "4");

            // do not retrieve message text because errors within transaction will attempt to
            // execute a new statement which will cause rollback exceptions
            m_props.addDefaultProperty("retrieveMessagesFromServerOnGetMessage", "false");

            // required for multiple concurrent ResultSets on same physical connection
            // otherwise closing one ResultSet will close all other ResultSets on same connection
            m_props.addDefaultProperty("downgradeHoldCursorsUnderXa", "true");
            m_props.addDefaultProperty("resultSetHoldability", "1");
         }
         else if (sDriver.equals("nexj.core.persistence.sql.TeradataDataSourceWrapper") &&
                  "Teradata".equals(db.getAdapter().getName()))
         {
            Properties props = new Properties();

            if (m_sHost != null)
            {
               props.setProperty("DSName", m_sHost);
            }

            if (m_nPort >= 0)
            {
               props.setProperty("DBS_PORT", String.valueOf(m_nPort));
            }

            if (m_sDatabase != null)
            {
               props.setProperty("DATABASE", m_sDatabase);
            }

            props.setProperty("CHARSET", (db.isUnicode()) ? "UTF16" : "UTF8");
            props.setProperty("TMODE", "ANSI"); // required for transaction isolation to work

            m_props.addDefaultProperty("DataSource", "com.teradata.jdbc.TeraDataSource");
            m_props.addDefaultProperty("Properties", PropertyUtil.toString(props));
         }
         else if (sDriver.equals("org.postgresql.xa.PGXADataSource"))
         {
            if (m_sHost != null) // Defaults to localhost if not set, http://www.postgresql.org/docs/7.3/static/jdbc-use.html
            {
               m_props.addDefaultProperty("serverName", m_sHost);
            }

            if (m_nPort >= 0) //Port defaults to 5432 if not set, http://www.postgresql.org/docs/7.3/static/jdbc-use.html
            {
               m_props.addDefaultProperty("portNumber", String.valueOf(m_nPort));
            }

            if (m_sDatabase != null)
            {
               m_props.addDefaultProperty("databaseName", m_sDatabase);
            }
         }
      }
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof RelationalDatabaseFragment))
      {
         return false;
      }

      RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)obj;

      return ObjUtil.equal(m_sDatabase, fragment.getDatabase()) &&
         ObjUtil.equal(m_sInstance, fragment.getInstance()) &&
         ObjUtil.equal(m_sHost, fragment.getHost()) &&
         m_nPort == fragment.getPort() &&
         ObjUtil.equal(m_sUser, fragment.getUser()) &&
         (m_dataSource != null) == (fragment.getDataSource() != null) &&
         (m_dataSource == null || m_dataSource.getAdapter() == fragment.getDataSource().getAdapter()) &&
         (m_sHost != null && (m_sDatabase != null || m_sInstance != null) ||
            m_props.equals(fragment.getPropertyHolder()));
   }

   /**
    * Determines whether or not this fragment can share the same alias as the given fragment.
    * @param other The other fragment.
    * @return True if the fragments may have the same alias; false otherwise.
    */
   public boolean isCompatible(RelationalDatabaseFragment other)
   {
      RelationalDatabase db = (RelationalDatabase)getDataSource();
      RelationalDatabase otherDb = (RelationalDatabase)other.getDataSource();

      return ObjUtil.equal(m_sHost, other.getHost()) &&
         ObjUtil.equal(m_sInstance, other.getInstance()) &&
         ObjUtil.equal(m_sDatabase, other.getDatabase()) &&
         ObjUtil.equal(m_sUser, other.getUser()) &&
         ObjUtil.equal(m_sPassword, other.getPassword()) &&
         m_nPort == other.getPort() &&
         m_nMinPoolSize == other.getMinPoolSize() &&
         m_nMaxPoolSize == other.getMaxPoolSize() &&
         m_nStatementCacheSize == other.getStatementCacheSize() &&
         ObjUtil.equal(db.getDriver(), otherDb.getDriver()) &&
         db.getPageSize() == otherDb.getPageSize() &&
         db.isUnicode() == otherDb.isUnicode();
   }
}
