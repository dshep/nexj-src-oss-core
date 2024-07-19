// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Locale;

/**
 * Utility class that holds various JDBC information.
 * This class can be used to map datasource and URL based connection info.
 */
public class JDBCInfo
{
   // constants

   /**
    * Unknown database type.
    */
   public static final byte DB_TYPE_UNKNOWN = 0;

   /**
    * MS SQL Server database.
    */
   public static final byte DB_TYPE_MSSQL = 1;

   /**
    * Oracle database.
    */
   public static final byte DB_TYPE_ORACLE = 2;

   /**
    * DB2 database.
    */
   public static final byte DB_TYPE_DB2 = 3;

   /**
    * MySQL database.
    */
   public static final byte DB_TYPE_MYSQL = 4;

   /**
    * Sybase database.
    */
   public static final byte DB_TYPE_SYBASE = 5;

   /**
    * PostgreSQL database.
    */
   public static final byte DB_TYPE_POSTGRESQL = 6;

   /**
    * The JTDS driver name.
    */
   protected static final String DRIVER_JTDS = "net.sourceforge.jtds.jdbcx.JtdsDataSource";

   /**
    * The MSSQL driver name.
    */
   protected static final String DRIVER_MSSQL = "com.microsoft.sqlserver.jdbc.SQLServerXADataSource";

   /**
    * The Oracle driver name.
    */
   protected static final String DRIVER_ORACLE = "oracle.jdbc.xa.client.OracleXADataSource";

   /**
    * The DB2 driver name.
    */
   protected static final String DRIVER_DB2 = "com.ibm.db2.jcc.DB2XADataSource";

   /**
    * The MySQL driver name.
    */
   protected static final String DRIVER_MYSQL = "com.mysql.jdbc.jdbc2.optional.MysqlXADataSource";

   /**
    * The Syabse driver name.
    */
   protected static final String DRIVER_SYBASE = "com.sybase.jdbc3.jdbc.SybXADataSource";

   /**
    * The PostgreSQL driver name.
    */
   protected static final String DRIVER_POSTGRESQL = "org.postgresql.xa.PGXADataSource";

   /**
    * The JDBC connection URL prefix.
    */
   protected static final String URL_PREFIX = "jdbc:";

   /**
    * The JDBC connection URL template hostPort substitution variable placeholder.
    */
   protected static final String URL_HOSTPORT_PLACEHOLDER = "${hostPort}";

   /**
    * The JDBC connection URL template dbName substitution variable placeholder.
    */
   protected static final String URL_DBNAME_PLACEHOLDER = "${dbName}";

   /**
    * The test SQL prefix.
    */
   protected static final String TEST_SQL_PREFIX = "select 1";

   /**
    * The database names corresponding to each database type.
    */
   protected static final String[] DB_TYPE_NAMES = new String[]
   {
      null,
      "MSSQL",
      "Oracle",
      "DB2",
      "MySQL",
      "Sybase",
      "PostgreSQL"
   };


   // attributes

   /**
    * The database type.
    */
   protected byte m_nDBType;

   /**
    * The database name.
    */
   protected String m_sDBTypeName;

   /**
    * The The database driver for datasource based connections.
    */
   protected String m_sDriver;

   /**
    * The database driver for URL based connections.
    */
   protected String m_sURLDriver;

   /**
    * The connection URL.
    */
   protected String m_sURL;

   /**
    * The connection URL placeholder template used to construct the connection URL.
    */
   protected String m_sURLTemplate;

   /**
    * The test SQL.
    */
   protected String m_sTestSQL;


   // constructors

   /**
    * Constructs JDBCInfo from XA datasource driver and optional database type hint.
    *
    * @param sDriver The XA datasource based driver.
    * @param sHint A hint to help determine the database type if the driver provided is null
    *              and to help distinguish between MSSQL and Sybase database types if the driver is JTDS.
    *              The hint can be either null or a string containing the database type name such as "Sybase" or "MySQL", etc.
    *              If the driver is null and the hint contains "MSSQL" or "Sybase", the driver will be assumed to be JTDS.
    */
   protected JDBCInfo(String sDriver, String sHint)
   {
      if (sDriver == null)
      {
         sHint = sHint.toLowerCase(Locale.ENGLISH);

         if (sHint.contains("mssql"))
         {
            m_nDBType = DB_TYPE_MSSQL;
            m_sDriver = DRIVER_JTDS;
         }
         else if (sHint.contains("oracle"))
         {
            m_nDBType = DB_TYPE_ORACLE;
            m_sDriver = DRIVER_ORACLE;
         }
         else if (sHint.contains("db2"))
         {
            m_nDBType = DB_TYPE_DB2;
            m_sDriver = DRIVER_DB2;
         }
         else if (sHint.contains("mysql"))
         {
            m_nDBType = DB_TYPE_MYSQL;
            m_sDriver = DRIVER_MYSQL;
         }
         else if (sHint.contains("sybase"))
         {
            m_nDBType = DB_TYPE_SYBASE;
            m_sDriver = DRIVER_JTDS;
         }
         else if (sHint.contains("postgresql"))
         {
            m_nDBType = DB_TYPE_POSTGRESQL;
            m_sDriver = DRIVER_POSTGRESQL;
         }
         else
         {
            m_nDBType = DB_TYPE_UNKNOWN;
         }
      }
      else
      {
         m_sDriver = sDriver;

         if (sDriver.equals(DRIVER_JTDS))
         {
            if (sHint != null && sHint.toLowerCase(Locale.ENGLISH).contains("sybase"))
            {
               m_nDBType = DB_TYPE_SYBASE;
            }
            else
            {
               m_nDBType = DB_TYPE_MSSQL;
            }
         }
         else if (sDriver.equals(DRIVER_MSSQL))
         {
            m_nDBType = DB_TYPE_MSSQL;
         }
         else if (sDriver.equals(DRIVER_ORACLE))
         {
            m_nDBType = DB_TYPE_ORACLE;
         }
         else if (sDriver.equals(DRIVER_DB2))
         {
            m_nDBType = DB_TYPE_DB2;
         }
         else if (sDriver.equals(DRIVER_MYSQL))
         {
            m_nDBType = DB_TYPE_MYSQL;
         }
         else if (sDriver.equals(DRIVER_SYBASE))
         {
            m_nDBType = DB_TYPE_SYBASE;
         }
         else if (sDriver.equals(DRIVER_POSTGRESQL))
         {
            m_nDBType = DB_TYPE_POSTGRESQL;
         }
         else
         {
            m_nDBType = DB_TYPE_UNKNOWN;
         }
      }

      setAttributes();
   }

   /**
    * Constructs JDBCInfo from a connection URL.
    * @param sJDBCConnectionURL The connection URL.
    */
   protected JDBCInfo(String sJDBCConnectionURL)
   {
      if (sJDBCConnectionURL.contains("jtds:"))
      {
         if (sJDBCConnectionURL.contains("sybase"))
         {
            m_nDBType = DB_TYPE_SYBASE;
         }
         else
         {
            m_nDBType = DB_TYPE_MSSQL;
         }

         m_sDriver = DRIVER_JTDS;
      }
      else if (sJDBCConnectionURL.contains("sqlserver"))
      {
         m_nDBType = DB_TYPE_MSSQL;
         m_sDriver = DRIVER_MSSQL;
      }
      else if (sJDBCConnectionURL.contains("oracle"))
      {
         m_nDBType = DB_TYPE_ORACLE;
         m_sDriver = DRIVER_ORACLE;
      }
      else if (sJDBCConnectionURL.contains("db2"))
      {
         m_nDBType = DB_TYPE_DB2;
         m_sDriver = DRIVER_DB2;
      }
      else if (sJDBCConnectionURL.contains("mysql"))
      {
         m_nDBType = DB_TYPE_MYSQL;
         m_sDriver = DRIVER_MYSQL;
      }
      else if (sJDBCConnectionURL.contains("sybase"))
      {
         m_nDBType = DB_TYPE_SYBASE;
         m_sDriver = DRIVER_SYBASE;
      }
      else if (sJDBCConnectionURL.contains("postgresql"))
      {
         m_nDBType = DB_TYPE_POSTGRESQL;
         m_sDriver = DRIVER_POSTGRESQL;
      }
      else
      {
         m_nDBType = DB_TYPE_UNKNOWN;
      }

      setAttributes();

      m_sURL = sJDBCConnectionURL;
   }


   // operations

   /**
    * Sets additional JDBCInfo attributes once m_nDBType and m_sDriver have been set by the constructor.
    */
   protected void setAttributes()
   {
      m_sDBTypeName = DB_TYPE_NAMES[m_nDBType];

      if (m_nDBType == DB_TYPE_MSSQL)
      {
         if (m_sDriver.equals("net.sourceforge.jtds.jdbcx.JtdsDataSource"))
         {
            m_sURLDriver = "net.sourceforge.jtds.jdbc.Driver";
            m_sURLTemplate = URL_PREFIX + "jtds:sqlserver://" + URL_HOSTPORT_PLACEHOLDER + ";DatabaseName=" + URL_DBNAME_PLACEHOLDER;
         }
         else if (m_sDriver.equals("com.microsoft.sqlserver.jdbc.SQLServerXADataSource"))
         {
            m_sURLDriver = "com.microsoft.sqlserver.jdbc.SQLServerDriver";
            m_sURLTemplate = URL_PREFIX + "sqlserver://" + URL_HOSTPORT_PLACEHOLDER + ";DatabaseName=" + URL_DBNAME_PLACEHOLDER;
         }

         m_sTestSQL = TEST_SQL_PREFIX;
      }
      else if (m_nDBType == DB_TYPE_ORACLE)
      {
         m_sURLDriver = "oracle.jdbc.OracleDriver";
         m_sURLTemplate = URL_PREFIX + "oracle:thin:@" + URL_HOSTPORT_PLACEHOLDER + "/" + URL_DBNAME_PLACEHOLDER;
         m_sTestSQL = TEST_SQL_PREFIX + " from dual";
      }
      else if (m_nDBType == DB_TYPE_DB2)
      {
         m_sURLDriver = "com.ibm.db2.jcc.DB2Driver";
         m_sURLTemplate = URL_PREFIX + "db2://" + URL_HOSTPORT_PLACEHOLDER + "/" + URL_DBNAME_PLACEHOLDER;
         m_sTestSQL = TEST_SQL_PREFIX + " from sysibm.sysdummy1";
      }
      else if (m_nDBType == DB_TYPE_MYSQL)
      {
         m_sURLDriver = "com.mysql.jdbc.Driver";
         m_sURLTemplate = URL_PREFIX + "mysql://" + URL_HOSTPORT_PLACEHOLDER + "/" + URL_DBNAME_PLACEHOLDER;
         m_sTestSQL = TEST_SQL_PREFIX;
      }
      else if (m_nDBType == DB_TYPE_SYBASE)
      {
         if (m_sDriver.equals("net.sourceforge.jtds.jdbcx.JtdsDataSource"))
         {
            m_sURLDriver = "net.sourceforge.jtds.jdbc.Driver";
            m_sURLTemplate = URL_PREFIX + "jtds:sybase://" + URL_HOSTPORT_PLACEHOLDER + ";DatabaseName=" + URL_DBNAME_PLACEHOLDER;
         }
         else if (m_sDriver.equals("com.sybase.jdbc3.jdbc.SybXADataSource"))
         {
            m_sURLDriver = "com.sybase.jdbc3.jdbc.SybDriver";
            m_sURLTemplate = URL_PREFIX + "sybase:Tds:" + URL_HOSTPORT_PLACEHOLDER + "/" + URL_DBNAME_PLACEHOLDER;
         }

         m_sTestSQL = TEST_SQL_PREFIX;
      }
      else if (m_nDBType == DB_TYPE_POSTGRESQL)
      {
         m_sURLDriver = "org.postgresql.Driver";
         m_sURLTemplate = URL_PREFIX + "postgresql://" + URL_HOSTPORT_PLACEHOLDER + "/" + URL_DBNAME_PLACEHOLDER;
         m_sTestSQL = TEST_SQL_PREFIX;
      }
   }

   /**
    * @see #JDBCInfo(String, String)
    */
   public static JDBCInfo getInstance(String sDriver, String sHint)
   {
      return new JDBCInfo(sDriver, sHint);
   }

   /**
    * @see #JDBCInfo(String)
    */
   public static JDBCInfo getInstance(String sJDBCConnectionURL)
   {
      return new JDBCInfo(sJDBCConnectionURL);
   }

   /**
    * @return The database type.
    */
   public byte getDBType()
   {
      return m_nDBType;
   }

   /**
    * @return The database type name. Null for an unknown database type.
    */
   public String getDBTypeName()
   {
      return m_sDBTypeName;
   }

   /**
    * @return The database driver for XA datasource based connections. Null for an unrecognized connection URL.
    */
   public String getDriver()
   {
      return m_sDriver;
   }

   /**
    * @return The database driver for URL based connections. Null for an unrecognized driver or connection URL.
    */
   public String getURLDriver()
   {
      return m_sURLDriver;
   }

   /**
    * @return The connection URL if this JDBCInfo was instantiated from a connection URL. Null otherwise.
    */
   public String getURL()
   {
      return m_sURL;
   }

   /**
    * Construcs a connection URL for a specific url driver based on the parameters supplied.
    *
    * @param sDBHost The database host name.
    * @param sDBPort The database port number.
    * @param sDBName The database name.
    *
    * @return The connection URL. Null for an unrecognized driver or connection URL.
    */
   public String getURL(String sDBHost, String sDBPort, String sDBName)
   {
      if (m_sURLTemplate == null)
      {
         return null;
      }

      return m_sURLTemplate.replaceAll("\\$\\{hostPort\\}", sDBHost + ":" + sDBPort).replaceAll("\\$\\{dbName\\}", sDBName);
   }

   /**
    * @return The test SQL. Null for an unknown database type.
    */
   public String getTestSQL()
   {
      return m_sTestSQL;
   }
}