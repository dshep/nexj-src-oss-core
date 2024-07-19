// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URI;
import java.net.URL;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import nexj.core.admin.etl.DataLoader;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.Repository;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.XMLRelationalMetadataExporter;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLSchemaManager;
import nexj.core.persistence.sql.SQLSchemaManager.SQLFileAppender;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.HashHolder;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;

/**
 * Tool for managing the SQL database schemas.
 */
public final class DatabaseSchemaTool extends DatabaseTool
{
   // attributes

   /**
    * The current command.
    */
   private String m_sCommand;

   // associations

   /**
    * The exported database.
    */
   private RelationalDatabase m_database;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(DatabaseSchemaTool.class);

   // operations
   
   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      m_sCommand = sCommand;

      SQLSchemaManager manager = null;

      if (sCommand.equals("create"))
      {
         manager = getSchemaManager();
         manager.createSchema(getSchema());
      }
      else if (sCommand.equals("drop"))
      {
         manager = getSchemaManager();
         manager.dropSchema(getSchema());
      }
      else if (sCommand.equals("insert"))
      {
         generateInsertScript();
      }
      else if (sCommand.equals("truncate"))
      {
         manager = getSchemaManager();
         manager.truncateSchema(getSchema());
      }
      else if (sCommand.equals("upgrade"))
      {
         manager = getSchemaManager();
         manager.upgrade(getSchema(), getProperty("meta.start"));
      }
      else if (sCommand.equals("analyze"))
      {
         manager = getSchemaManager();
         manager.analyzeSchema(getSchema());
      }
      else if (sCommand.equals("export"))
      {
         exportSchema();
      }
      else if (sCommand.equals("setup"))
      {
         Metadata metadata = new MetadataLoaderDispatcher().load(null, null,
            MetadataLoader.DATASOURCE_ONLY | MetadataLoader.INTEGRATION_EXCLUDED, null);
         RelationalDatabase database = getDatabase(metadata);

         manager = getSchemaManager(database);
         manager.createDatabase((RelationalSchema)database.getSchema(), new PropertyMap()
         {
            public Object findValue(String sName, Object defaultValue)
            {
               Object value = getValue(sName);

               return (value == null) ? defaultValue : value;
            }

            public Object getValue(String sName)
            {
               Object value = getProperty(sName);

               return (value == null) ? getProperty("setup." + sName) : value; // also check CMD args
            }

            // thin wrapper around corresponding getProperty(...) methods
            public Object findValue(String sName) { return getValue(sName); }
            public String getClassName() { return getClass().getName(); }
            public PropertyIterator getIterator() { throw new UnsupportedOperationException(); }
            public int getValueCount() { throw new UnsupportedOperationException(); }
            public boolean hasValue(String sName) { return getValue(sName) != null; }
            public void setValue(String sName, Object value)
            {
               throw new UnsupportedOperationException();
            }
         });
      }
      else if (sCommand.equals("wrap"))
      {
         exportSchema();
         exportClasses();
      }
      else
      {
         throw new IllegalArgumentException("Invalid command \"" + sCommand + "\"");
      }

      if (manager != null && manager.getSQLAppender() instanceof SQLFileAppender)
      {
         ((SQLFileAppender)manager.getSQLAppender()).close();
      }
   }

   /**
    * Exports the schema specified in the options.
    */
   private void exportSchema() throws IOException, SQLException
   {
      if (m_database != null)
      {
         return;
      }
      
      String sDataSourceName = getProperty("meta.datasource", "NewDatabase");
      File dir = new File(new File(getRequiredProperty("meta.dir")), "datasources");
         
      if (!dir.exists())
      {
         dir.mkdirs();
      }

      File file = new File(dir, sDataSourceName + ".datasource");
      SQLSchemaManager manager = null;
      Writer writer = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file)), XMLUtil.ENCODING);

      try
      {
         if (m_database == null)
         {
            m_database = new RelationalDatabase(sDataSourceName);
            m_database.setSchema(new RelationalSchema());

            manager = getSchemaManager(getConnection());
            manager.readSchema(
               (RelationalSchema)m_database.getSchema(),
               getProperty("db.catalog"),
               getProperty("db.schema"),
               getProperty("db.table"),
               null,
               null);
         }

         StringWriter swriter = new StringWriter(0x8000);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Writing relational database metadata \"" + m_database.getName() + 
               "\" to file \"" + file.toString() + "\"");
         }

         new XMLRelationalMetadataExporter(new XMLMetadataExporter(swriter)).exportDatabase(m_database);
         writer.write(XMLUtil.formatXML(swriter.toString()));
      }
      finally
      {
         if (manager != null)
         {
            manager.setConnection(null);
         }

         writer.close();
      }
   }
   
   /**
    * Exports the classes specified in the options.
    */
   private void exportClasses() throws IOException
   {
      File root = new File(getRequiredProperty("meta.dir"));
      File dir = new File(root, "classes");

      if (!dir.exists())
      {
         dir.mkdirs();
      }
      
      Properties properties = null;
      String sMappingFile = getProperty("meta.mapfile");
      
      if (sMappingFile != null && sMappingFile.length() != 0)
      {
         InputStream istream = new FileInputStream(sMappingFile);
         
         try
         {
            properties = new Properties();
            properties.load(istream);
         }
         finally
         {
            istream.close();
         }
      }

      XMLMetadata metadata = new XMLMetadata("new", root.toURL(), null, null, null);
      DataSourceType dstype = new DataSourceType("RelationalDatabase");

      dstype.setMetadata(metadata);
      dstype.setExporter(XMLRelationalMetadataExporter.class);
      m_database.setType(dstype);
      metadata.addDataSource(m_database);
      ((RelationalSchema)m_database.getSchema()).generateMetaclasses(getProperty("meta.prefix"), properties, null);

      for (Iterator itr = metadata.getMetaclassIterator(); itr.hasNext();)
      {
         Metaclass metaclass = (Metaclass)itr.next();

         File file = new File(dir, metaclass.getName() + ".meta");
         Writer writer = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file)), XMLUtil.ENCODING);

         try
         {
            StringWriter swriter = new StringWriter(0x2000);

            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Writing class \"" + metaclass.getName() + 
                  "\" to file \"" + file.toString() + "\"");
            }

            new XMLMetadataExporter(swriter).exportMetaclass(metaclass);
            writer.write(XMLUtil.formatXML(swriter.toString()));
         }
         finally
         {
            writer.close();
         }
      }

      String sDescriptorFile = getProperty("meta.descfile");

      if (sDescriptorFile != null && sDescriptorFile.length() != 0)
      {
         StringWriter swriter = new StringWriter(0x2000);
         XMLWriter writer = new XMLWriter(swriter);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Writing the repository descriptor to file \"" + sDescriptorFile + "\"");
         }

         writer.openElement("Metadata");
         writer.writeAttribute("version", "0");
         writer.closeElement();

         writer.startElement("Classes");

         List metaclassList = new ArrayList(metadata.getMetaclassCount());

         for (Iterator itr = metadata.getMetaclassIterator(); itr.hasNext();)
         {
            metaclassList.add(itr.next());
         }

         Collections.sort(metaclassList, new Comparator()
         {
            public int compare(Object o1, Object o2)
            {
               return ((Metaclass)o1).getName().compareToIgnoreCase(((Metaclass)o2).getName());
            }
         });

         for (int i = 0; i < metaclassList.size(); ++i)
         {
            writer.openElement("ClassRef");
            writer.writeAttribute("resource", "classes/" +
               ((Metaclass)metaclassList.get(i)).getName() + ".meta");
            writer.closeEmptyElement();
         }

         writer.endElement("Classes");
         writer.startElement("DataSources");
         writer.openElement("DataSourceRef");
         writer.writeAttribute("resource", "datasources/" + m_database.getName() + ".datasource");
         writer.closeEmptyElement();
         writer.endElement("DataSources");
         writer.endElement("Metadata");

         Writer fwriter = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(sDescriptorFile)), XMLUtil.ENCODING);

         try
         {
            fwriter.write(XMLUtil.formatXML(swriter.toString()));
         }
         finally
         {
            fwriter.close();
         }
      }
   }

   /**
    * Converts a dump file into an SQL script.
    * @throws Exception On script generation error.
    */
   private void generateInsertScript() throws Exception
   {
      Metadata metadata = Repository.getMetadata();
      String sDataSources = getProperty("meta.datasource", "DefaultRelationalDatabase");
      String sInputURL = getRequiredProperty("dump.file");
      String sOutputURL = getRequiredProperty("sql.file");
      InputStream in = URLUtil.openStream(new URL(URLUtil.toURL(sInputURL)));
      Writer out = // for requested DataSources
         IOUtil.openBufferedWriter((URLUtil.isURL(sOutputURL)) ?
            new File(new URI(sOutputURL)) : new File(sOutputURL), XMLUtil.ENCODING);
      Set dataSourceSet = null;
      InvocationContext context =
         (InvocationContext)metadata.getComponent("System.InvocationContext").getInstance(null);

      context.initialize(null);
      context.setLocale(Locale.getDefault());
      context.setProtected(false);
      context.setSecure(false);
      context.setPartitioned(false);

      if (sDataSources != null && !"*".equals(sDataSources))
      {
         dataSourceSet = new HashHolder();

         for (StringTokenizer tokenizer = new StringTokenizer(sDataSources, ",");
              tokenizer.hasMoreTokens();)
         {
            dataSourceSet.add(metadata.getDataSource(tokenizer.nextToken().trim()));
         }
      }

      try
      {
         DataLoader dataLoader = new DataLoader(context);
         
         dataLoader.setVersionMaintained(false);
         dataLoader.generateScript(out, in, dataSourceSet);
      }
      finally
      {
         IOUtil.close(in);
         out.close();
      }
   }

   /**
    * @return The SQL schema manager.
    */
   private SQLSchemaManager getSchemaManager()
   {
      return getSchemaManager((RelationalDatabase)null);
   }

   /**
    * @param ds The relational database to query for configuration (null == call getDatabase()).
    * @return The SQL schema manager.
    */
   private SQLSchemaManager getSchemaManager(RelationalDatabase database)
   {
      database = (database == null) ? getDatabase() : database;

      String sFile = getRequiredProperty("sql.file")
         .replace("${cmd}", m_sCommand)
         .replace("${ds}", database.getName());
      String sOwner = getProperty("meta.owner");
      SQLAdapter adapter = (SQLAdapter)database.getComponent().getInstance(null);
      SQLSchemaManager manager = adapter.createSchemaManager(database);

      if (sOwner != null) // some SchemaManagers (e.g. DB2) will have a different default for null
      {
         manager.setOwner((sOwner.equals(".")) ? "" : sOwner);
      }

      manager.setSQLAppender(manager.new SQLFileAppender((sFile.equals("-")) ? null : new File(sFile)));

      return manager;
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "create - generate an SQL script for creating the schema",
         "drop - generate an SQL script for dropping the schema",
         "insert - generate an SQL script for inserting data from a dump file", 
         "truncate - generate an SQL script for truncating the tables",
         "upgrade - generate an SQL script for upgrading the schema",
         "analyze - generate an SQL script for updating the statistics",
         "setup - generate an SQL script for setting up the database",
         "export - export metadata schema from the database",
         "wrap - export metadata schema and generate classes",
      };
   }

   /**
    * @see nexj.core.tools.DatabaseTool#getAdditionalOptionUsage()
    */
   protected String[] getAdditionalOptionUsage()
   {
      return new String[]
      {
         "-Ddb.catalog=<catalog name>",
         "-Ddb.schema=<schema name pattern>",
         "-Ddb.table=<table name pattern>",
         "-Ddump.file=<input dump file>",
         "-Dmeta.datasource=<datasource name> - Can be a comma separated list or * for all",
         "-Dmeta.dir=<metadata directory name>",
         "-Dmeta.mapfile=<table to class name map file>",
         "-Dmeta.owner=<metadata table owner>",
         "-Dmeta.prefix=<class name prefix>",
         "-Dmeta.start=<metadata version to start upgrade from>",
         "-Dmeta.descfile=<descriptor file name>",
         "-Dsetup.collation=<database collation to use>",
         "-Dsetup.datapath=<path to data files>",
         "-Dsetup.indexpath=<path to index data files>",
         "-Dsetup.indexspacesize=<initial size of index tablespace>",
         "-Dsetup.indexspaceincrement=<size increment of index tablespace>",
         "-Dsetup.longpath=<path to LOB data files>",
         "-Dsetup.longspacesize=<initial size of LOB tablespace>",
         "-Dsetup.longspaceincrement=<size increment of LOB tablespace>",
         "-Dsetup.mempercent=<percent of total system memory to allocate>",
         "-Dsetup.memsizeperm=<long-term memory size allocation (e.g. cache/pools)>",
         "-Dsetup.memsizetemp=<short-term memory size allocation (e.g. sorting)>",
         "-Dsetup.tablespacesize=<initial size of data tablespace>",
         "-Dsetup.tablespaceincrement=<size increment of data tablespace>",
         "-Dsetup.tempspace=<name of temporary tablespace>",
         "-Dsetup.tempspacesize=<initial size of temporary tablespace>",
         "-Dsetup.tempspaceincrement=<size increment of temporary tablespace>",
         "-Dsetup.undopath=<path to undo data files>",
         "-Dsetup.undospace=<name of undo/log tablespace>",
         "-Dsetup.undospacesize=<initial size of undo/log tablespace>",
         "-Dsetup.undospaceincrement=<size increment of undo/log tablespace>",
         "-Dsql.file=<sql output file name>",
      };
   }

   public static void main(String[] args)
   {
      new DatabaseSchemaTool().run(args);
   }
}