// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.net.URI;
import java.net.URL;
import java.util.Iterator;
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
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.persistence.SchemaVersion;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.util.HashDeque;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;

/**
 * Data import/export tool.
 */
public class DataLoadTool extends GenericTool
{
   // associations

   /**
    * The logger.
    */
   private final static Logger s_logger = Logger.getLogger(DataLoadTool.class);
   
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;
   
   // operations
   
   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      if (sCommand.equals("export"))
      {
         init();
         exportData();
      }
      else if (sCommand.equals("extract"))
      {
         init();
         extractData();
      }
      else if (sCommand.equals("import"))
      {
         init();
         importData(false);
      }
      else if (sCommand.equals("recreate"))
      {
         init();
         new DataLoader(m_context).recreateSchema(getDataSourceSet());
         importData(true);
      }
      else if (sCommand.equals("reset"))
      {
         init();
         new DataLoader(m_context).deleteData(getDataSourceSet());
         importData(true);
      }
      else if (sCommand.equals("recreateschema"))
      {
         init();
         new DataLoader(m_context).recreateSchema(getDataSourceSet());
      }
      else if (sCommand.equals("upgrade"))
      {
         init();
         upgradeData(false);
      }
      else if (sCommand.equals("upgradedump"))
      {
         String sDumpURLs = getProperty("dump.urls");
         String sDumpURLArray[];

         if (sDumpURLs != null)
         {
            if (hasProperty("dump.url") || hasProperty("old.dump.url"))
            {
               throw new IllegalArgumentException(
                  "Properties \"dump.url\" and \"old.dump.url\" cannot be specified with \"dump.urls\"");
            }

            sDumpURLArray = StringUtil.split(sDumpURLs);
         }
         else
         {
            sDumpURLArray = new String[] {getRequiredProperty("dump.url")};
         }

         String sPreviousMetaURL = getRequiredProperty("old.meta.url");
         String sBaseURLProperty = getProperty("old.meta.base.url");
         Properties properties = new Properties(SysUtil.getConfigProperties())
         {
            private static final long serialVersionUID = -6961684990382643373L;

            /**
             * @see java.util.Properties#getProperty(java.lang.String,
             *      java.lang.String)
             */
            public String getProperty(String sKey, String sDefaultValue)
            {
               String sValue = getOverride(sKey);

               return (sValue != null) ? sValue : super.getProperty(sKey, sDefaultValue);
            }

            /**
             * @see java.util.Properties#getProperty(java.lang.String)
             */
            public String getProperty(String sKey)
            {
               String sValue = getOverride(sKey);

               return (sValue != null) ? sValue : super.getProperty(sKey);
            }

            protected String getOverride(String sKey)
            {
               if (sKey.startsWith("meta.") && !sKey.equals(XMLMetadataLoader.BASE_URL_PROPERTY))
               {
                  return DataLoadTool.this.getProperty("old." + sKey);
               }

               return null;
            }
         };

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, sBaseURLProperty == null ? "" : sBaseURLProperty);

         Metadata oldMetadata = null;

         for (int nDumpIndex = 0; nDumpIndex < sDumpURLArray.length; ++nDumpIndex)
         {
            String sPreviousDumpURL;

            if (sDumpURLs == null)
            {
               sPreviousDumpURL = getProperty("old.dump.url", sDumpURLArray[nDumpIndex]);
            }
            else
            {
               sPreviousDumpURL = sDumpURLArray[nDumpIndex];
               setProperty("dump.url", sDumpURLArray[nDumpIndex]);
            }

            if (s_logger.isInfoEnabled())
            {
               s_logger.info("Upgrading dump " + sDumpURLArray[nDumpIndex] + "...");
            }

            s_logger.info("Loading current repository");
            init();

            // drop any tables that may exist for the current schema
            new DataLoader(m_context).dropSchema(getDataSourceSet());

            s_logger.info("Loading old repository");

            if (oldMetadata == null)
            {
               oldMetadata = new MetadataLoaderDispatcher().load(sPreviousMetaURL, properties, MetadataLoader.DEFAULT, null);
            }

            init(oldMetadata);

            s_logger.info("Recreating the old data");
            new DataLoader(m_context).recreateSchema(getDataSourceSet());
            importData(true, sPreviousDumpURL);

            s_logger.info("Upgrading the data");
            init();
            upgradeData(true);

            s_logger.info("Exporting the data");
            init();
            exportData();
         }

         s_logger.info("Upgrade dump completed");
      }
      else if (sCommand.equals("version"))
      {
         init();

         DataLoader loader = new DataLoader(m_context);
         
         for (Iterator itr = loader.getDataSourceIterator(getDataSourceSet()); itr.hasNext();)
         {
            DataSource ds = (DataSource)itr.next();
            SchemaVersion version = loader.getSchemaVersion(ds);

            if (version != null)
            {
               printVersion(version.getNamespace(), version.getVersion());

               if (!ObjUtil.equal(m_context.getMetadata().getNamespace(), version.getNamespace()))
               {
                  if (s_logger.isWarnEnabled())
                  {
                     s_logger.warn("Namespace mismatch in " + ds + " (expected \"" +
                        m_context.getMetadata().getNamespace() + "\", got \"" +
                        version.getNamespace() + "\")");
                  }

                  setExitCode(2);
               }
            }
            else
            {
               s_logger.error("Unable to determine the version of " + ds);
               setExitCode(3);
            }
         }
      }
      else if (sCommand.equals("dumpversion"))
      {
         init();
         printDumpVersion();
      }
      else
      {
         throw new IllegalArgumentException("Invalid command \"" + sCommand + "\"");
      }
   }

   /**
    * Outputs the repository URL with version.
    * @param sNamespace The repository namespace.
    * @param sVersion The repository version.
    */
   protected static void printVersion(String sNamespace, String sVersion)
   {
      System.out.println(sNamespace + '#' + sVersion);
   }
   
   /**
    * Outputs the repository URL with version using the dump URL.
    */
   protected void printDumpVersion() throws Exception
   {
      InputStream in = new BufferedInputStream(URLUtil.openStream(
         new URL(URLUtil.toURL(getRequiredProperty("dump.url")))));

      try
      {
         Pair p = new DataLoader(m_context).getDumpVersion(in);
         
         if (p != null)
         {
            printVersion((String)p.getHead(), (String)p.getTail());
         }
         else
         {
            s_logger.info("Unable to determine the dump version");
            setExitCode(3);
         }
      }
      finally
      {
         IOUtil.close(in);
      }
   }
   
   /**
    * Initializes the invocation context.
    */
   protected void init()
   {
      init(Repository.getMetadata());
   }
   
   /**
    * Initializes the invocation context given a metadata object.
    * @param metadata The metadata object.
    */
   protected void init(Metadata metadata)
   {
      m_context = (InvocationContext)metadata.getComponent("System.InvocationContext").getInstance(null);
      m_context.initialize(null);
      m_context.setLocale(Locale.getDefault());
      m_context.setProtected(false);
      m_context.setSecure(false);
      m_context.setPartitioned(false);
      m_context.getGlobalCache().clear();
   }

   /**
    * @return The data source set. Can be null for all.
    */
   protected Set getDataSourceSet()
   {
      String sDataSources = getProperty("meta.datasource", "DefaultRelationalDatabase");

      if ("*".equals(sDataSources))
      {
         return null;
      }

      StringTokenizer tokenizer = new StringTokenizer(sDataSources, ",");
      Set dataSourceSet = new HashDeque(1);

      while (tokenizer.hasMoreTokens())
      {
         dataSourceSet.add(m_context.getMetadata().getDataSource(tokenizer.nextToken().trim()));
      }

      return dataSourceSet;
   }

   /**
    * Exports data through a specified interface.
    * @param exporter The data exporter.
    */
   protected void exportData(Exporter exporter) throws Exception
   {
      String sDataURL = getRequiredProperty("dump.url");
      File file = (URLUtil.isURL(sDataURL)) ? new File(new URI(sDataURL)) : new File(sDataURL);
      File tmpFile = File.createTempFile(SysUtil.NAMESPACE + '-', ".dump", file.getParentFile());
      OutputStream ostream = null;
      boolean bFailed = false;

      try
      {
         ostream = new BufferedOutputStream(new FileOutputStream(tmpFile));
         exporter.export(ostream);
         ostream.close();
         ostream = null;
      }
      catch (Throwable t)
      {
         bFailed = true;
         ObjUtil.rethrow(t);
      }
      finally
      {
         if (ostream != null)
         {
            try
            {
               ostream.close();
            }
            catch (IOException e)
            {
            }
         }

         if (bFailed)
         {
            tmpFile.delete();
         }
      }

      if (!bFailed)
      {
         if (file.exists() && !file.delete() || !tmpFile.renameTo(file))
         {
            throw new IOException("Unable to rename \"" + tmpFile.toString() +
               "\" to \"" + file.toString() + "\"");
         }
      }
   }

   /**
    * @return A new initialized data loader instance.
    */
   protected DataLoader createDataLoader()
   {
      DataLoader loader = new DataLoader(m_context);

      loader.setCompressed(StringUtil.parseBoolean(getProperty("dump.compressed", "true")));

      String sMarshaller = getProperty("dump.format");

      if (sMarshaller != null)
      {
         if (sMarshaller.indexOf('.') < 0)
         {
            sMarshaller = SysUtil.PACKAGE + ".core.rpc." + sMarshaller.toLowerCase(Locale.ENGLISH) +
               '.' + sMarshaller + "Marshaller";
         }

         loader.setMarshallerClassName(sMarshaller);
      }

      return loader;
   }

   /**
    * Exports the data from the system.
    */
   protected void exportData() throws Exception
   {
      final Set dataSourceSet = getDataSourceSet();
      final Lookup whereMap;

      if (!StringUtil.parseBoolean(getProperty("enums.enabled", "true")))
      {
         String[] names =
         {
            "EnumTypeDisplay", "EnumUserPickList", "EnumType", "EnumCode",
            "EnumDisplay", "EnumBase", "EnumDependency", "EnumDependencyType"
         };

         whereMap = new HashTab(names.length);

         for (int i = 0; i < names.length; ++i)
         {
            Metaclass metaclass = m_context.getMetadata().findMetaclass(names[i]);

            if (metaclass != null)
            {
               whereMap.put(metaclass, Boolean.FALSE);
            }
         }
      }
      else
      {
         whereMap = null;
      }

      exportData(new Exporter()
      {
         public void export(OutputStream ostream) throws Exception
         {
            createDataLoader().exportData(ostream, dataSourceSet, whereMap, false);
         }
      });
   }

   /**
    * Selectively exports the data from the system.
    */
   protected void extractData() throws Exception
   {
      String sQuery = getRequiredProperty("query");
      String sScript = getProperty("script.url");
      Machine machine = m_context.getMachine();

      if (sScript != null)
      {
         Intrinsic.load(sScript, machine);
      }

      final Pair spec = (Pair)machine.invoke(new Compiler().compile(
         new SchemeParser(machine.getGlobalEnvironment()).parse(new StringReader(sQuery), null),
         null, machine, true), (Pair)null);

      exportData(new Exporter()
      {
         public void export(OutputStream ostream) throws Exception
         {
            createDataLoader().exportData(ostream, Pair.getIterator(spec));
         }
      });
   }

   /**
    * Upgrades the data in the system.
    * @param bForce True to ignore the persistent store upgradability flag.
    */
   protected void upgradeData(boolean bForce)
   {
      Metadata metadata = m_context.getMetadata();
      boolean bCommit = false;

      try
      {
         Upgrade upgrade = metadata.getUpgrade("Main");

         upgrade.validate(metadata, null);
         new DataLoader(m_context).upgrade(getDataSourceSet(), upgrade, bForce);
         metadata.getMetaclass("SysUpgrade").invoke("upgrade");
         bCommit = true;
      }
      finally
      {
         m_context.complete(bCommit);
      }
   }

   /**
    * @see DataLoadTool#importData(boolean, String)
    */
   protected void importData(boolean bSkipRead) throws Exception
   {
      importData(bSkipRead, getRequiredProperty("dump.url")); 
   }
   
   /**
    * Imports the data into the system.
    * @param bSkipRead True to skip reading the instances during import.
    * @param sDumpURL The dump URL string.
    */
   protected void importData(boolean bSkipRead, String sDumpURL) throws Exception
   {
      InputStream in = URLUtil.openStream(new URL(URLUtil.toURL(sDumpURL)));

      if (!(in instanceof BufferedInputStream))
      {
         in = new BufferedInputStream(in); 
      }

      try
      {
         new DataLoader(m_context).importData(in, getDataSourceSet(), bSkipRead);
      }
      finally
      {
         IOUtil.close(in);
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Dnexj.meta.connections.url=<connections URL>",
         "-Dnexj.meta.url=<metadata URL>",
         "-Dnexj.meta.base.url=<base URL>",
         "-Dmeta.datasource=<datasource name> - Can be a comma separated list or * for all",
         "-Ddump.url=<dump file URL> - Must be a file when exporting",
         "-Ddump.format=JSON|Text|SOAP|XML|<MarshallerClass> - Dump file format, defaults to Text",
         "-Ddump.compressed=true|false - True to compress the dump file (default)",
         "-Denums.enabled=true|false - True to export the enums",
         "-Dold.meta.url=<old metadata URL>",
         "-Dold.meta.base.url=<old base URL>",
         "-Dold.dump.url=<old dump file URL> - Defaults to dump.url",
         "-Dquery=<query expression> - Must evaluate to '((<class> <attributes> <where>) ...)",
         "-Dscript.url=<additional script URL>"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "export - Exports the data",
         "extract - Selectively extracts a data set described by a query",
         "import - Imports the data",
         "reset - Deletes and then imports the data",
         "recreate - Drops and recreates all tables, then imports the data",
         "recreateschema - Drops and recreates all tables, but does not import the data",
         "upgrade - Upgrades the data",
         "upgradedump - Recreates using old.meta.url and old.dump.url, upgrades, then exports the data",
         "version - Prints the current repository version in the system",
         "dumpversion - Prints the version from the dump url"
      };
   }

   public static void main(String[] args)
   {   
      new DataLoadTool().run(args);
   }

   // inner classes

   /**
    * Interface implemented by data exporters.
    */
   protected interface Exporter
   {
      /**
       * Exports the data to the specified output stream.
       * @param ostream The output stream.
       */
      void export(OutputStream ostream) throws Exception;
   }
}