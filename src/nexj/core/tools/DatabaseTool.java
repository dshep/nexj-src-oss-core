// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.Locale;
import java.util.Properties;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.persistence.sql.DefaultSQLSchemaManagerFactory;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLConnectionFactory;
import nexj.core.persistence.sql.SQLSchemaManager;
import nexj.core.runtime.Context;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.Logger;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;

/**
 * Database command-line tool shell.
 */
public abstract class DatabaseTool extends GenericTool
{
   // associations

   /**
    * The SQL connection used by the tool.
    */
   private Connection m_connection;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(DatabaseTool.class);

   // operations

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected final String[] getOptionUsage()
   {
      String[] usageArray = new String[]
      {
         "-Dmeta.datasource=<metadata data source name>",
         "-Ddb.driver=<driver class name>",
         "-Ddb.url=<URL>",
         "-Ddb.user=<user>",
         "-Ddb.password=<password>",
         "-Ddb.prop#=<value#>",
      };

      String[] usageArray2 = getAdditionalOptionUsage();

      if (usageArray2 != null)
      {
         String[] usageArray3 = new String[usageArray.length + usageArray2.length];
         
         System.arraycopy(usageArray, 0, usageArray3, 0, usageArray.length);
         System.arraycopy(usageArray2, 0, usageArray3, usageArray.length, usageArray2.length);
         
         return usageArray3;
      }

      return usageArray;
   }
   
   /**
    * The same as getOptionUsage(), but for additional options.
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getAdditionalOptionUsage()
   {
      return null;
   }
   
   /**
    * Gets an SQL connection based on the configuration properties.
    * @return The SQL connection.
    * @throws IllegalArgumentException if a required property is missing.
    * @throws SQLException if a connection opening error occurs.
    */
   protected final Connection getConnection() throws SQLException, IllegalArgumentException
   {
      if (m_connection == null)
      {
         String sDriver = getProperty("db.driver");
         String sURL = getProperty("db.url");
         
         if (sDriver != null && sURL != null)
         {
            String sUser = getRequiredProperty("db.user");
            String sPassword = getProperty("db.password", "");
            StringBuffer buf = new StringBuffer(128);
   
            for (int i = 1; ; ++i)
            {
               String sValue = getProperty("db.prop" + i);
               
               if (sValue == null)
               {
                  break;
               }
               
               buf.append(sValue);
               buf.append(SysUtil.LINE_SEP);
            }
            
            Properties connectionProperties;
            
            try
            {
               connectionProperties = PropertyUtil.fromString(buf.toString());
            }
            catch (IOException e)
            {
               connectionProperties = new Properties();
            }
            
            try
            {
               Class.forName(sDriver);
            }
            catch (ClassNotFoundException e)
            {
               throw new IllegalArgumentException("Cannot find the driver \"" + sDriver + "\""); 
            }
   
            connectionProperties.setProperty("user", sUser);
            connectionProperties.setProperty("password", sPassword);
   
            m_connection = DriverManager.getConnection(sURL, connectionProperties);
         }
         else
         {
            SQLAdapter adapter = (SQLAdapter)getDatabase().getComponent().getInstance(null);
            
            m_connection = adapter.getConnectionFactory().getConnection(adapter);
         }
      }

      return m_connection;
   }
   
   /**
    * Closes the SQL connection, if it is still open.
    */
   protected final void closeConnection()
   {
      if (m_connection != null)
      {
         try
         {
            m_connection.close();
         }
         catch (SQLException e)
         {
            s_logger.debug("Error closing the SQL connection", e);
         }
         
         m_connection = null;
      }
   }

   /**
    * @return The relational database.
    */
   protected RelationalDatabase getDatabase()
   {
      return getDatabase(null);
   }

   /**
    * @return The relational database.
    * @param metatada The Metadata object to query (null == Repository.getMetadata()).
    * @throws IllegalArgumentException If no RelationalDatabase by that name was found.
    */
   protected RelationalDatabase getDatabase(Metadata metadata) throws IllegalArgumentException
   {
      metadata = (metadata == null) ? Repository.getMetadata() : metadata;

      DataSource ds = metadata.getDataSource(getRequiredProperty("meta.datasource"));

      if (ds instanceof RelationalDatabase)
      {
         return (RelationalDatabase)ds;
      }

      throw new IllegalArgumentException("Data source \"" + ds.getName() +
                                         "\" is not a relational database");
   }

   /**
    * @return The relational schema.
    */
   protected RelationalSchema getSchema()
   {
      return (RelationalSchema)getDatabase().getSchema();
   }

   /**
    * Instantiates a schema manager based on a property value or on a connection metadata.
    * @param connection The SQL connection. Can be null.
    */
   protected SQLSchemaManager getSchemaManager(Connection connection) throws SQLException
   {
      String sDataSource = (connection == null) ? getRequiredProperty("meta.datasource") : getProperty("meta.datasource") ;
      SQLSchemaManager manager;

      if (sDataSource == null)
      {
         manager = DefaultSQLSchemaManagerFactory.create(connection);
      }
      else
      {
         manager = ((SQLAdapter)getDatabase().getComponent().getInstance(null)).createSchemaManager(getDatabase());

         if (connection != null)
         {
            manager.setConnection(connection);
         }
      }

      String sOwner = getProperty("meta.owner");

      if (sOwner != null)
      {
         manager.setOwner((sOwner.equals(".")) ? "" : sOwner);
      }

      return manager;
   }

   /**
    * @see nexj.core.tools.GenericTool#dispose()
    */
   protected void dispose()
   {
      closeConnection();
      super.dispose();
   }

   /**
    * Test a data source connection.
    * 
    * @param metadata The metadata.
    * @param sDataSource The data source name.
    * @return false if a SQL connection could not be obtained
    */
   public static void testConnection(Metadata metadata, String sDataSource) throws SQLException
   {
      InvocationContext context = null;
      Context contextSaved = ThreadContextHolder.getContext();

      try
      {
         DataSource db = metadata.getDataSource(sDataSource);
         Component comp = metadata.findComponent("System.InvocationContext");
         context = (comp != null) ? (InvocationContext)comp.getInstance(null) : new InvocationContext(metadata);

         context.initialize(null);
         context.setLocale(Locale.getDefault());
         context.setProtected(false);
         context.setSecure(false);
         context.setPartitioned(false);

         for (Iterator itr = db.getFragmentIterator(); itr.hasNext();)
         {
            Object o = itr.next();

            if (o instanceof RelationalDatabaseFragment)
            {
               ((SQLConnectionFactory)((RelationalDatabaseFragment)o).getConnectionFactory().getInstance(context))
                  .getConnection(null).close();
            }
         }
      }
      finally
      {
         if (context != null)
         {
            context.complete(false);
         }

         ThreadContextHolder.setContext(contextSaved);
      }
   }
}
