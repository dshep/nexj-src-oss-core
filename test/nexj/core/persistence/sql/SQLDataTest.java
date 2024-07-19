// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Locale;
import java.util.Properties;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import junit.framework.TestCase;

import nexj.core.admin.etl.sql.SQLUtil;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.Repository;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.RelationalSchemaTest;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.persistence.sql.SQLSchemaManager.SQLConnectionAppender;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Base class for tests, which require database data.
 */
public abstract class SQLDataTest extends TestCase
{
   // constants

   /**
    * Script for dropping the schema objects that are not the the schema metadata.
    */
   protected final static String ST_DROP = "drop";

   /**
    * Script for inserting data in the schema objects.
    */
   protected final static String ST_INSERT = "insert";

   // associations

   /**
    * The adapter-specific metadata.
    */
   protected Metadata m_metadata;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The data source.
    */
   protected RelationalDatabase m_database;

   /**
    * The persistence adapter.
    */
   protected SQLAdapter m_adapter;

   /**
    * The logger.
    */
   protected Logger m_logger;

   /**
    * Map of connection URL names to their corresponding metadata: Metadata[String].
    */
   private final static Lookup s_metadataMap = new HashTab();

   /**
    *  Map of adapter names, for which schemas have been created, to their locking transaction: Transaction[String].
    */
   private final static Lookup s_initMap = new HashTab();

   // constructors

   /**
    * Constructor for SQLDataTest.
    * @param sName The name of the test.
    */
   public SQLDataTest(String sName)
   {
      this(sName, true);
   }

   /**
    * Constructor for SQLDataTest.
    * @param sName The name of the test.
    * @param bInitialize True to initialize the test.
    */
   protected SQLDataTest(String sName, boolean bInitialize)
   {
      super(sName);

      if (bInitialize)
      {
         initialize();
      }
   }

   // operations

   /**
    * Template method to load metadata, typically called from the constructor.
    */
   protected void initialize()
   {
      if (isEnabled())
      {
         loadMetadata();
      }
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      if (!isEnabled())
      {
         throw new IllegalStateException("Test " + getClass().getName() + " is not enabled");
      }

      boolean bLocked = lock();
      Connection con = null;

      try
      {
         con = m_adapter.getConnectionFactory().getConnection(m_adapter);

         if (!bLocked) // lock acquired previously, therefore schema created, only truncate
         {
            RelationalSchema schema = (RelationalSchema)m_database.getSchema();
            SQLSchemaManager manager = m_adapter.createSchemaManager(m_database);
            SQLConnectionAppender appender = new SQLSchemaManager.SQLConnectionAppender(con, true);

            appender.setLogLevel(Logger.DUMP);
            manager.setSQLAppender(appender);
            manager.setOwner("test");
            appender.setSafe(false);
            manager.truncateSchema(schema);
         }

         SQLUtil.execute(con, getSQLScript(ST_INSERT));
      }
      finally
      {
         if (con != null)
         {
            try
            {
               con.close();
            }
            catch (SQLException e)
            {
            }
         }
      }

      try
      {
         m_context.setUserClass(getMetadata().getMetaclass("User"));
         m_context.initialize(new SimplePrincipal(getUser()));
      }
      catch (Throwable t)
      {
         ThreadContextHolder.setContext(null);
         ObjUtil.rethrow(t);
      }
   }

   /*
    * @see TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_context.complete(false);
      ThreadContextHolder.setContext(null);
      m_context = null;
      m_database = null;
      m_adapter = null;
      m_logger = null;
      super.tearDown();
   }

   /**
    * @return The logger for this class.
    */
   protected Logger getLogger()
   {
      if (m_logger == null)
      {
         m_logger = Logger.getLogger(getClass());
      }

      return m_logger;
   }

   /**
    * @return True if the engine is enabled.
    */
   public boolean isEnabled()
   {
      return StringUtil.parseBoolean(SysUtil.getConfigProperties()
         .getProperty(getDataSourceName().toLowerCase(Locale.ENGLISH) + '.' +
            getAdapterName().toLowerCase(Locale.ENGLISH) + ".enabled", "false"));
   }

   /**
    * @return The context user.
    */
   protected String getUser()
   {
      return "jtest";
   }

   /**
    * @return The data source name.
    */
   protected String getDataSourceName()
   {
      return Repository.getMetadata().getMetaclass("Contact").getPersistenceMapping().getDataSource().getName();
   }

   /**
    * @return The adapter name for the default metadata.
    */
   protected static String getDefaultAdapterName()
   {
      return Repository.getMetadata().getMetaclass("Contact").getPersistenceMapping().getDataSource().getAdapter().getName();
   }

   /**
    * @return The adapter name.
    */
   protected String getAdapterName()
   {
      return getDefaultAdapterName();
   }

   /**
    * @return The connections URL. Null for the default URL.
    */
   protected String getConnectionsURL()
   {
      String sAdapter = getAdapterName();

      if (!sAdapter.equals(getDefaultAdapterName()))
      {
         return '/' + SysUtil.PACKAGE + '/' + sAdapter.toLowerCase(Locale.ENGLISH) + ".connections";
      }

      return null;
   }

   /**
    * Loads the test metadata.
    */
   protected void loadMetadata()
   {
      String sConnectionsURL = getConnectionsURL();

      if (sConnectionsURL != null)
      {
         synchronized (s_metadataMap)
         {
            m_metadata = (Metadata)s_metadataMap.get(sConnectionsURL);
         }

         if (m_metadata == null)
         {
            m_metadata = loadMetadata(sConnectionsURL);

            synchronized (s_metadataMap)
            {
               Object oldMetadata = s_metadataMap.put(sConnectionsURL, m_metadata);

               if (oldMetadata != null)
               {
                  s_metadataMap.put(sConnectionsURL, oldMetadata);
                  m_metadata = (Metadata)oldMetadata;
               }
            }
         }
      }
   }

   /**
    * Loads a repository configured for a given persistence engine.
    * @param sConnectionsURL The connections URL to use.
    * @return The loaded metadata object.
    */
   protected static Metadata loadMetadata(String sConnectionsURL)
   {
      Properties props = SysUtil.getConfigProperties();

      props = new Properties(props);
      props.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, sConnectionsURL);

      return new MetadataLoaderDispatcher().load(
         props.getProperty(MetadataLoader.METADATA_URL_PROPERTY,
            MetadataLoader.DEFAULT_METADATA_URL), props, 0, null);
   }

   /**
    * @return The metadata.
    */
   protected Metadata getMetadata()
   {
      if (m_metadata == null)
      {
         m_metadata = Repository.getMetadata();
      }

      return m_metadata;
   }

   /**
    * @return The URL of the setup SQL script.
    * @param sType The script type, one of the ST_* constants.
    */
   protected URL getSQLScript(String sType)
   {
      return SQLDataTest.class.getResource("script/" + getSQLScriptName(sType) + ".sql");
   }

   /**
    * @return The name of the setup SQL script.  
    * @param sType The script type, one of the ST_* constants.
    */
   protected String getSQLScriptName(String sType)
   {
      return getAdapterName().toLowerCase(Locale.ENGLISH) + '_' + sType;
   }

   /**
    * Parses a Scheme S-expression.
    * @param sExpr The string to parse.
    * @return The parse tree.
    */
   protected Pair parse(String sExpr)
   {
      return (Pair)new SchemeParser(m_context.getMachine().getGlobalEnvironment())
         .parse(new StringReader(sExpr), null);
   }
   
   /**
    * Returns a file name URL relative to the class.
    * @param sFileName The relative file name.
    */
   protected URL getURL(String sFileName)
   {
      try
      {
         return new URL(getClass().getResource("data/"), sFileName);
      }
      catch (MalformedURLException e)
      {
         throw new UncheckedException("err.test.url", new Object[]{sFileName}, e);
      } 
   }
   
   /**
    * Commits the current unit of work.
    */
   protected void commit()
   {
      m_context.getUnitOfWork().commit();
   }
   
   /**
    * Rolls back the current unit of work.
    */
   protected void rollback()
   {
      m_context.getUnitOfWork().rollback();
   }
   
   /**
    * Resets the invocation context.
    */
   protected void reset()
   {
      m_context.initialize(m_context.getPrincipal());
      m_adapter = (SQLAdapter)getMetadata().getDataSource(getDataSourceName()).getComponent().getInstance(m_context);
   }

   /**
    * Executes a named SQL script.
    * @param sName The script name.
    */
   protected void execute(String sName)
   {
      TransactionManager txManager = m_context.getTransactionManager();
      Transaction tx = null;
      Connection con = null;

      try
      {
         tx = txManager.getTransaction();
         
         if (tx != null)
         {
            txManager.suspend();
         }

         con = m_adapter.getConnectionFactory().getConnection(null);
         SQLUtil.execute(con, getSQLScript(sName));
      }
      catch (Throwable t)
      {
         throw new UncheckedException("err.test.sql", new Object[]{sName}, t);
      }
      finally
      {
         if (con != null)
         {
            try
            {
               con.close();
            }
            catch (SQLException e)
            {
            }
         }

         if (tx != null)
         {
            try
            {
               txManager.resume(tx);
            }
            catch (Throwable t)
            {
               ObjUtil.rethrow(t);
            }
         }
      }
   }

   /**
    * Perform an exclusive resource lock.
    * @return The lock was acquired.
    */
   public boolean lock()
   {
      // reinitialize member variables for every call (same as setUp() did before)
      m_context = new InvocationContext(getMetadata());
      m_database = (RelationalDatabase)getMetadata().getDataSource(getDataSourceName());
      m_adapter = (SQLAdapter)m_database.getComponent().getInstance(m_context);

      // hints declared/used by core/test/nexj/base/upgrades/Main.upgrade, need for dropSchema()
      RelationalSchemaTest.addHint((RelationalSchema)m_database.getSchema(), "test1");
      RelationalSchemaTest.addHint((RelationalSchema)m_database.getSchema(), "test2");
      RelationalSchemaTest.addHint((RelationalSchema)m_database.getSchema(), "test3");

      String sAdapter = getAdapterName();
      TransactionManager txManager = m_context.getTransactionManager();

      synchronized (s_initMap)
      {
         if (s_initMap.contains(sAdapter))
         {
            return false; // acquiring a second time will case an early release
         }

         Transaction tx = null;
         Connection con = null;

         try
         {
            txManager.setTransactionTimeout(86400);
            txManager.begin();
            tx = txManager.getTransaction();
            con = m_adapter.getConnectionFactory().getConnection(m_adapter);

            if (getLogger().isDebugEnabled())
            {
               getLogger().debug("Locking " + m_database + " for adapter \"" + sAdapter + "\"");
            }

            SQLUtil.execute(con, "update test.Mutex set id=1");

            getLogger().debug("Lock acquired");

            con.close();
            con = null;
            txManager.suspend();
            txManager.setTransactionTimeout(0);

            // use separate connection for creating schema since DDL operations might issue commit
            con = m_adapter.getConnectionFactory().getConnection(m_adapter);

            RelationalSchema schema = (RelationalSchema)m_database.getSchema();
            SQLSchemaManager manager = m_adapter.createSchemaManager(m_database);
            SQLConnectionAppender appender = new SQLSchemaManager.SQLConnectionAppender(con, true);

            // recreate test schema
            appender.setLogLevel(Logger.DUMP);
            manager.setSQLAppender(appender);
            manager.setOwner("test");
            manager.dropSchema(schema);
            SQLUtil.execute(con, getSQLScript(ST_DROP));
            appender.setSafe(false);
            manager.createSchema(schema);
            con.close();
            con = null;

            s_initMap.put(sAdapter, tx);

            return true;
         }
         catch (Throwable t)
         {
            if (con != null)
            {
               try
               {
                  con.close();
               }
               catch (SQLException e)
               {
               }
            }

            if (tx != null)
            {
               try
               {
                  Transaction curTx = txManager.getTransaction();

                  if (curTx != tx)
                  {
                     assert curTx == null; // it is invalid for another TX to be active here
                     txManager.resume(tx); // lock TX needs to be rolled back
                  }

                  txManager.rollback();
               }
               catch (Throwable u)
               {
               }
            }

            throw new RuntimeException(
               "Unable to lock the persistent store for adapter \"" + getAdapterName() + '"', t);
         }
      }
   }

   /**
    * Release an exclusive resource lock.
    */
   public void unlock()
   {
      String sAdapter = getAdapterName();

      if (getDefaultAdapterName().equals(sAdapter)) // valid null if never locked by caller
      {
         return; // do not unlock primary DB since it's used by other jUnit tests
      }

      try
      {
         synchronized (s_initMap)
         {
            ((Transaction)s_initMap.remove(sAdapter)).rollback(); // release DB lock
         }
      }
      catch (Exception e)
      {
      }
   }
}