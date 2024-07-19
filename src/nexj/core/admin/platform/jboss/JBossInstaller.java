// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.admin.platform.jboss;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.transform.dom.DOMSource;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.HostKey;
import com.jcraft.jsch.HostKeyRepository;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpATTRS;
import com.jcraft.jsch.SftpException;
import com.jcraft.jsch.UserInfo;

import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;

import nexj.core.admin.Installer;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.http.HTTPChannel;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.meta.integration.channel.mail.Mail;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLSchemaManager;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.J2EEUtil;
import nexj.core.util.JDBCInfo;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SubstReader;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.auth.PropertiesLoginModule;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * JBoss-specific installer
 */
public class JBossInstaller implements Installer
{
   // constants

   /**
    * Special token to work around a JBoss property setter bug.
    * @see nexj.core.container.platform.jboss.JBossPropertiesEditor
    */
   protected final static String PROPERTY_LINE_SEP = "##CR-LF##";

   protected final static String[] DATASOURCE_ELEMENTS =
   {
      "jndi-name", "connection-url", "use-java-context", "track-connection-by-tx", "xa-datasource-class", "driver-class",
      "xa-datasource-property", "connection-property", "isSameRM-override-value", "transaction-isolation",
      "user-name", "password", "application-managed-security", "security-domain",
      "security-domain-and-application", "min-pool-size", "max-pool-size",
      "blocking-timeout-millis", "background-validation", "background-validation-minutes",
      "idle-timeout-minutes", "validate-on-match", "no-tx-separate-pools", "xa-resource-timeout",
      "new-connection-sql", "check-valid-connection-sql", "valid-connection-checker-class-name",
      "exception-sorter-class-name", "track-statements", "prefill", "prepared-statement-cache-size",
      "share-prepared-statements", "set-tx-query-timeout", "query-timeout", "metadata",
      "type-mapping", "depends"
   };

   protected final static String[] CONNECTION_FACTORY_ELEMENTS =
   {
      "jndi-name", "local-transaction", "xa-transaction",
      "track-connection-by-tx", "rar-name", "connection-definition", "config-property",
      "application-managed-security", "security-domain", "security-domain-and-application",
      "min-pool-size", "max-pool-size", "blocking-timeout-millis", "idle-timeout-minutes",
      "no-tx-separate-pools", "xa-resource-timeout", "metadata", "type-mapping", "depends"
   };

   protected final static String[] SERVICE_ELEMENTS =
   {
      "Connector", "Engine"
   };

   protected final static String[] LOG4J_ELEMENTS =
   {
      "renderer", "appender", "category", "logger", "root", "categoryFactory"
   };

   protected final static String[] ROOT_ELEMENTS =
   {
      "param", "priority", "level", "appender-ref"
   };

   /**
    * The trust store password.
    */
   protected final static String TRUST_STORE_PASSWORD = "trustpass";

   /**
    * The JDBC prepared statement bind variables regexp pattern.
    */
   protected final static String BIND_VAR_PATTERN = "(\\?|:\\d+)";

   /**
    * Implements a comparator to sort DDL statements into 2 alphabetically sorted groups: CREATE TABLE and CREATE INDEX statements.
    * This is used to make sure that CREATE INDEX statements always appear after CREATE TABLE statements in an DDL output file.
    */
   protected final static Comparator DDL_COMPARATOR = new Comparator()
   {
      /**
       * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
       */
      public int compare(Object sDDLStatement1, Object sDDLStatement2)
      {
         if (!(sDDLStatement1 instanceof String && sDDLStatement2 instanceof String))
         {
            throw new IllegalArgumentException("Only arguments of type String supported: " + "\"" + sDDLStatement1 + "\", " + "\"" + sDDLStatement2 + "\"");
         }

         if (sDDLStatement1 == null || sDDLStatement2 == null)
         {
            throw new IllegalArgumentException("Only non null arguments supported: " + "\"" + sDDLStatement1 + "\", " + "\"" + sDDLStatement2 + "\"");
         }

         String sDDL1 = ((String)sDDLStatement1).trim().toLowerCase(Locale.ENGLISH);
         String sDDL2 = ((String)sDDLStatement2).trim().toLowerCase(Locale.ENGLISH);

         if (sDDL1.startsWith("create table") && sDDL2.startsWith("create index"))
         {
            return -1;
         }
         else if(sDDL2.startsWith("create table") && sDDL1.startsWith("create index"))
         {
            return 1;
         }
         else
         {
            return sDDL1.compareTo(sDDL2);
         }
      }
   };

   // associations

   /**
    * SQL Adapter cache: SQLAdapter[RelationalDatabase].
    */
   protected Lookup m_adapterMap = new HashTab();

   /**
    * SQL schema manager cache: SQLSchemaManager[RelationalDatabase].
    */
   protected Lookup m_schemaManagerMap = new HashTab();

   /**
    * The JBoss locations.
    */
   protected Location[] m_locationArray;

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The environment name.
    */
   protected String m_sEnvironmentName;
   
   /**
    * Installer run-time properties.
    */
   protected Properties m_properties;

   /**
    * Version specific JBossInstallerStrategy
    */
   protected JBossInstallerStrategy m_installerStrategy;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JBossInstaller.class);

   // operations

   /**
    * @see nexj.core.admin.Installer#setMetadata(nexj.core.meta.Metadata)
    */
   public void setMetadata(Metadata metadata)
   {
      m_metadata = metadata;
      m_sEnvironmentName = m_metadata.getEnvironment();
   }

   /**
    * @see nexj.core.admin.Installer#setProperties(java.util.Properties)
    */
   public void setProperties(Properties properties)
   {
      m_properties = properties;
   }

   /**
    * Sets the JBoss server directories.
    * Can be a single directory or multiple ones with a choice pattern: ...{dir1,...,dirN}...
    * Examples:
    *    C:\java\jboss-4.0.3SP1\server\default
    *    \\{server1,server2,server3}\jboss\server\all
    * @see nexj.core.admin.Installer#setLocation(java.lang.String, java.lang.String, java.lang.String)
    */
   public void setLocation(String sLocation, String sUser, String sPassword) throws Exception
   {
      int nStart = sLocation.indexOf('{');
      int nEnd = sLocation.indexOf('}', nStart + 1);
      String sPrefix;
      String sSuffix;
      String sChoiceArray[];

      if (nStart < 0 || nEnd < 0)
      {
         sPrefix = "";
         sSuffix = "";
         sChoiceArray = StringUtil.split(sLocation, ',');
      }
      else
      {
         sPrefix = sLocation.substring(0, nStart);
         sSuffix = sLocation.substring(nEnd + 1);
         sChoiceArray = StringUtil.split(sLocation.substring(nStart + 1, nEnd), ',');
      }

      m_locationArray = new Location[sChoiceArray.length];

      for (int i = 0; i < sChoiceArray.length; ++i)
      {
         m_locationArray[i] = new Location(sPrefix + sChoiceArray[i].trim() + sSuffix,
            m_metadata.isDistributed(), sUser, sPassword);
      }

      m_installerStrategy = initInstallerStrategy(m_locationArray[0].getResourceConnection(m_properties), m_locationArray[0].getConfigDir());
   }

   /**
    * @param resourceConn The resource connection.
    * @param configDir The server conf directory.
    * @return Version specific JBossInstalationStrategy
    */
   protected JBossInstallerStrategy initInstallerStrategy(ResourceConnection resourceConn, Resource configDir)
   {
      Resource aopConfigFile = resourceConn.getResource(configDir, "bootstrap" + SysUtil.FILE_SEP + "aop.xml");

      try
      {
         if (aopConfigFile.exists())
         {
            return new JBoss5InstallerStrategy();
         }
         else
         {
            return new JBoss4InstallerStrategy();
         }
      }
      catch (Throwable t)
      {
         throw new RuntimeException("Unable to determine JBoss version", t);
      }
   }

   /**
    * @see nexj.core.admin.Installer#deploy(java.lang.String)
    */
   public void deploy(String sEARName) throws Exception
   {
      for (int i = 0; i < m_locationArray.length; ++i)
      {
         Location location = m_locationArray[i];
         ResourceConnection connection = location.getResourceConnection(m_properties);

         try
         {
            connection.getResource(location.getFarmDir()).put(new File(sEARName));
         }
         finally
         {
            connection.close();
         }

         if (m_metadata.isDistributed())
         {
            break;
         }
      }
   }

   /**
    * @see nexj.core.admin.Installer#undeploy(java.lang.String)
    */
   public void undeploy(String sEARName) throws Exception
   {
      for (int i = 0; i < m_locationArray.length; ++i)
      {
         Location location = m_locationArray[i];
         ResourceConnection connection = location.getResourceConnection(m_properties);

         try
         {
            connection.getResource(m_locationArray[i].getFarmDir(), sEARName).delete();
         }
         finally
         {
            connection.close();
         }

         if (m_metadata.isDistributed())
         {
            break;
         }
      }
   }

   /**
    * @see nexj.core.admin.Installer#install()
    */
   public void install() throws Exception
   {
      for (int i = 0; i < m_locationArray.length; ++i)
      {
         Location location = m_locationArray[i];
         ResourceConnection connection = location.getResourceConnection(m_properties);

         try
         {
            m_installerStrategy.install(connection, location, i);
         }
         finally
         {
            connection.close();
         }
      }
   }

   /**
    * @see nexj.core.admin.Installer#uninstall()
    */
   public void uninstall() throws Exception
   {
      for (int i = 0; i < m_locationArray.length; ++i)
      {
         Location location = m_locationArray[i];
         ResourceConnection connection = location.getResourceConnection(m_properties);

         try
         {
            m_installerStrategy.uninstall(connection, location, i);
         }
         finally
         {
            connection.close();
         }
      }
   }

   /**
    * Installs properties-service.xml.
    * @param file The file.
    */
   protected void installPropertiesService(Resource file) throws IOException
   {
      Document doc = parse(file, "<server><mbean code=\"org.jboss.varia.property.SystemPropertiesService\"" +
         " name=\"jboss:type=Service,name=SystemProperties\"/></server>");
      String sOriginalContents = format(doc);
      Element server = doc.getDocumentElement();

      Element mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss:type=Service,name=PropertyEditorManager");

      if (mbean == null)
      {
         mbean = XMLUtil.setChildElement(server, null, "mbean", "name", "jboss:type=Service,name=PropertyEditorManager", null, false);
         XMLUtil.setAttribute(mbean, "code", "org.jboss.varia.property.PropertyEditorManagerService", false);
      }

      Element editors = XMLUtil.findChildElement(mbean, "attribute", "name", "Editors");

      if (SysUtil.ENTERPRISE)
      {
         if (editors == null)
         {
            editors = XMLUtil.setChildElement(mbean, null, "attribute", "name", "Editors", null, false);
         }

         Properties properties = PropertyUtil.fromString(XMLUtil.getElementValue(editors));

         properties.setProperty("java.util.Properties", SysUtil.PACKAGE + ".core.container.platform.jboss.JBossPropertiesEditor");
         XMLUtil.setElementValue(editors, PropertyUtil.toString(properties));
      }
      else
      {
         if (editors != null)
         {
            XMLUtil.removeNode(editors);
         }
      }

      mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss:type=Service,name=SystemProperties");

      if (mbean == null)
      {
         mbean = XMLUtil.setChildElement(server, null, "mbean", "name", "jboss:type=Service,name=SystemProperties", null, false);
         XMLUtil.setAttribute(mbean, "code", "org.jboss.varia.property.SystemPropertiesService", false);
      }

      Element propsAttrib = XMLUtil.setChildElement(mbean, null, "attribute", "name", "Properties", null, false);
      m_installerStrategy.addCustomProperties(propsAttrib);

      XMLUtil.normalize(server);
      format(file, doc, sOriginalContents);
   }

   /**
    * Uninstalls properties-service.xml.
    * @param file The file.
    */
   protected void uninstallPropertiesService(Resource file) throws IOException
   {
      if (file.exists())
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element mbean = XMLUtil.findChildElement(server, "mbean", "name",
            "jboss:type=Service,name=PropertyEditorManager");

         if (mbean != null)
         {
            Element editors = XMLUtil.findChildElement(mbean, "attribute", "name", "Editors");

            if (editors != null)
            {
               Properties properties = PropertyUtil.fromString(XMLUtil.getElementValue(editors));
               String sEditor = properties.getProperty("java.util.Properties");

               if (sEditor != null && sEditor.startsWith(SysUtil.PACKAGE + '.'))
               {
                  properties.remove("java.util.Properties");

                  XMLUtil.setElementValue(editors, PropertyUtil.toString(properties));
                  format(file, doc, sOriginalContents);
               }
            }
         }
      }
   }

   /**
    * Installs nexj-ds.xml.
    * @param file The file.
    */
   protected void installDataSource(Resource file) throws IOException
   {
      Document doc = parse(file, "<datasources/>");
      String sOriginalContents = format(doc);
      Element datasources = doc.getDocumentElement();

      for (Iterator dsItr = m_metadata.getDataSourceIterator(); dsItr.hasNext();)
      {
         DataSource ds = (DataSource)dsItr.next();

         if (ds.isEnabled() && ds instanceof RelationalDatabase)
         {
            RelationalDatabase db = (RelationalDatabase)ds;

            for (Iterator fragmentItr = db.getFragmentIterator(); fragmentItr.hasNext();)
            {
               RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)fragmentItr.next();

               addDataSource(doc, db, fragment, null, SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/");
            }
         }
      }

      JDBCConnectionInfo jmsDBConnInfo = m_installerStrategy.getJMSDBConnectionInfo();
      m_installerStrategy.addJMSDataSource(doc, jmsDBConnInfo, SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jms/jdbc/");

      XMLUtil.normalize(datasources);
      format(file, doc, sOriginalContents);
   }

   /**
    * Add a datasource configuration
    * @param document The document representing the contents of the file under which to add the datasource
    * @param db The database.
    * @param dbFragment The database fragment.
    * @param jdbcConnInfo Non-XA data source additional connection info. Null if XA data source.
    * @param sJNDIPrefix The prefix to use for JNDI name.
    */
   protected void addDataSource(Document document, RelationalDatabase db, RelationalDatabaseFragment dbFragment,
      JDBCConnectionInfo jdbcConnInfo, String sJNDIPrefix)
   {
      if (!dbFragment.isFirst())
      {
         return;
      }

      String sDSElementName = (jdbcConnInfo == null) ? "xa-datasource" : "local-tx-datasource";
      String sDriverClassElementName = (jdbcConnInfo == null) ? "xa-datasource-class" : "driver-class";
      String sConnPropElementName = (jdbcConnInfo == null) ? "xa-datasource-property" : "connection-property";
      String sDriver = (jdbcConnInfo == null) ? db.getDriver() : jdbcConnInfo.getDriver();
      String sJNDIName = sJNDIPrefix + dbFragment.getAlias();
      byte nDBType = JDBCInfo.getInstance(sDriver, db.getAdapter().getName()).getDBType();

      Element datasources = document.getDocumentElement();
      Element datasource = XMLUtil.findChildElementBySubelement(datasources, null, "jndi-name", sJNDIName);

      if (datasource != null && !datasource.getNodeName().endsWith(sDSElementName))
      {
         XMLUtil.removeNode(datasource);
         datasource = null;
      }

      if (datasource == null)
      {
         datasource = document.createElement(sDSElementName);
         datasources.appendChild(datasource);
      }

      Element driverClass = XMLUtil.findChildElement(datasource, sDriverClassElementName);
      boolean bDriverChanged = (driverClass == null ||
         !ObjUtil.equal(XMLUtil.getElementValue(driverClass), sDriver));

      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "jndi-name", sJNDIName, true);

      if (jdbcConnInfo == null)
      {
         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "track-connection-by-tx", "true", true);
      }
      else
      {
         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "connection-url", jdbcConnInfo.getConnectionURL(), true);
      }

      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, sDriverClassElementName, sDriver, true);
      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "transaction-isolation", "TRANSACTION_READ_COMMITTED", true);
      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "security-domain", getJAASDomain(dbFragment), true);
      XMLUtil.removeNode(XMLUtil.findChildElement(datasource, "user-name"));
      XMLUtil.removeNode(XMLUtil.findChildElement(datasource, "password"));
      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "min-pool-size", String.valueOf(dbFragment.getMinPoolSize()), true);
      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "max-pool-size", String.valueOf(dbFragment.getMaxPoolSize()), true);
      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "blocking-timeout-millis", "10000", false);
      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "idle-timeout-minutes", "2", false);

      String sInitSQL = null;
      String sTestSQL;

      if (jdbcConnInfo == null)
      {
         SQLAdapter adapter = getAdapter(db);
         sInitSQL = adapter.getInitialSQL();
         sTestSQL = adapter.getTestSQL();
      }
      else
      {
         sTestSQL = jdbcConnInfo.getTestSQL();
      }

      if (sInitSQL != null)
      {
         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "new-connection-sql", sInitSQL, true);
      }
      else
      {
         XMLUtil.removeNode(XMLUtil.findChildElement(datasource, "new-connection-sql"));
      }

      if (sTestSQL != null)
      {
         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "check-valid-connection-sql", sTestSQL, true);
      }
      else
      {
         XMLUtil.removeNode(XMLUtil.findChildElement(datasource, "check-valid-connection-sql"));
      }

      if (nDBType == JDBCInfo.DB_TYPE_ORACLE)
      {
         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "exception-sorter-class-name",
            "org.jboss.resource.adapter.jdbc.vendor.OracleExceptionSorter", bDriverChanged);
      }
      else if (nDBType == JDBCInfo.DB_TYPE_SYBASE)
      {
         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "exception-sorter-class-name",
            "org.jboss.resource.adapter.jdbc.vendor.SybaseExceptionSorter", bDriverChanged);
      }
      else if (bDriverChanged)
      {
         XMLUtil.removeNode(XMLUtil.findChildElement(datasource, "exception-sorter-class-name"));
      }

      //Buggy/incomplete XA support: jTDS with XA Emulation, all Oracle versions.
      if (sDriver.equals("net.sourceforge.jtds.jdbcx.JtdsDataSource") ||
          nDBType == JDBCInfo.DB_TYPE_ORACLE ||
          nDBType == JDBCInfo.DB_TYPE_DB2)
      {
         if (jdbcConnInfo == null)
         {
            //Prevents joining existing branches; always creates a new branch.
            XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "isSameRM-override-value", "false", true);
         }
      }
      else
      {
         //Known not buggy: Microsoft SQL Server JDBC driver v1.2
         XMLUtil.removeNode(XMLUtil.findChildElement(datasource, "isSameRM-override-value"));
      }

      XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, "prepared-statement-cache-size",
         String.valueOf(dbFragment.getStatementCacheSize()), true);

      final List removalList = new ArrayList();

      XMLUtil.forEachChildElement(datasource, sConnPropElementName, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element element)
         {
            removalList.add(element);
         }
      });

      for (int i = 0, n = removalList.size(); i < n; ++i)
      {
         XMLUtil.removeNode((Node)removalList.get(i));
      }

      for (Lookup.Iterator propItr = dbFragment.getPropertyHolder().getPropertyIterator(); propItr.hasNext();)
      {
         String sName = (String)propItr.next();

         if (jdbcConnInfo == null && sName.length() != 0)
         {
            sName = sName.substring(0, 1).toUpperCase(Locale.ENGLISH) + sName.substring(1);
         }

         String sValue = (String)propItr.getValue();

         if (sValue != null)
         {
            sValue = sValue.replace("\n", PROPERTY_LINE_SEP);
            sValue = sValue.replace("\r", PROPERTY_LINE_SEP);
            sValue = sValue.replace(PROPERTY_LINE_SEP + PROPERTY_LINE_SEP, PROPERTY_LINE_SEP);
         }

         XMLUtil.setChildElement(datasource, DATASOURCE_ELEMENTS, sConnPropElementName,
            "name", sName, sValue, true);
      }
   }

   /**
    * Frameworks prior to v7 use mail service from container and require mail-service.xml.
    * Frameworks v7+ have their own Mail RA implementation and mail-service.xml will conflict with
    * JNDI mapping of the Mail RA.
    * @param file The file.
    */
   protected void installMailService(Resource file) throws IOException
   {
      uninstallMailService(file);
   }

   /**
    * Uninstalls mail-service.xml.
    * @param file The file.
    */
   protected void uninstallMailService(Resource file) throws IOException
   {
      if (file.exists())
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();

         for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
         {
            Channel channel = (Channel)itr.next();

            if (channel instanceof Mail && channel.isEnabled())
            {
               XMLUtil.removeNode(XMLUtil.findChildElement(server, "mbean", "name", "jboss:service=" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + '/' + ((Mail)channel).getAlias()));
            }
         }

         format(file, doc, sOriginalContents);
      }
   }

   /**
    * Installs server.xml.
    * @param file The file.
    * @param nNode The node number.
    */
   protected void installServer(Resource file, int nNode) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element server = doc.getDocumentElement();
      Element service = XMLUtil.findChildElement(server, "Service", "name", "jboss.web");

      if (service != null)
      {
         XMLUtil.forEachChildElement(service, "Connector", new XMLUtil.ElementHandler()
         {
            public void handleElement(Element connector)
            {
               XMLUtil.setAttribute(connector, "address", "${jboss.bind.address}", false);
               XMLUtil.setAttribute(connector, "emptySessionPath", "true", false);

               if (connector.getAttribute("protocol").startsWith("AJP"))
               {
                  String sAJPRedirectPort = "443";

                  try
                  {
                     URI httpServerURI = new URI(m_metadata.getHTTPRoot());

                     if (httpServerURI.getScheme() != null && httpServerURI.getScheme().equals("https") && httpServerURI.getPort() != -1)
                     {
                        sAJPRedirectPort = String.valueOf(httpServerURI.getPort());
                     }

                  }
                  catch (URISyntaxException e)
                  {
                     // leave sAJPRedirectPort as 443
                  }

                  XMLUtil.setAttribute(connector, "redirectPort", sAJPRedirectPort, true);
                  XMLUtil.setAttribute(connector, "enableLookups", "false", false);
                  XMLUtil.setAttribute(connector, "maxThreads", "256", false);
                  XMLUtil.setAttribute(connector, "minSpareThreads", "3", false);
                  XMLUtil.setAttribute(connector, "maxSpareThreads", "256", false);
                  XMLUtil.setAttribute(connector, "minProcessors", "3", false);
                  XMLUtil.setAttribute(connector, "maxProcessors", "256", false);
                  XMLUtil.setAttribute(connector, "bufferSize", "8192", false);
                  XMLUtil.setAttribute(connector, "backlog", "25", false);
               }
               else
               {
                  XMLUtil.setAttribute(connector, "acceptCount", "2000", false);
                  XMLUtil.setAttribute(connector, "backlog", "200", false);
                  XMLUtil.setAttribute(connector, "connectionTimeout", "120000", false);
                  XMLUtil.setAttribute(connector, "maxThreads", "33", false);
                  XMLUtil.setAttribute(connector, "minSpareThreads", "3", false);
                  XMLUtil.setAttribute(connector, "maxSpareThreads", "33", false);
                  XMLUtil.setAttribute(connector, "maxKeepAliveRequests", "-1", false);
                  XMLUtil.setAttribute(connector, "bufferSize", "8192", false);
                  XMLUtil.setAttribute(connector, "maxHttpHeaderSize", "8192", false);
                  XMLUtil.setAttribute(connector, "enableLookups", "false", false);
                  XMLUtil.setAttribute(connector, "strategy", "ms", false);
                  XMLUtil.setAttribute(connector, "threadPriority", "7", false);
                  XMLUtil.setAttribute(connector, "restrictedUserAgents", "^.*MS Web Services Client Protocol .*$", false);

                  if (connector.getAttribute("scheme").equals("https"))
                  {
                     XMLUtil.setAttribute(connector, "secure", "true", false);
                     XMLUtil.setAttribute(connector, "clientAuth", "false", false);
                     XMLUtil.setAttribute(connector, "sslProtocol", "TLS", false);

                     XMLUtil.setAttribute(connector, "securityDomain", "java:/jaas/" + SysUtil.NAMESPACE + "-cert", false);
                     XMLUtil.setAttribute(connector, "SSLImplementation", "org.jboss.net.ssl.JBossImplementation", false);
                     connector.removeAttribute("keystoreFile");
                     connector.removeAttribute("keystorePass");
                  }
               }
            }
         });

         Element engine = XMLUtil.findChildElement(service, "Engine", "name", "jboss.web");

         if (engine != null)
         {
            engine.setAttribute("jvmRoute", "node" + String.valueOf(nNode));

            Element authRealm = XMLUtil.findChildElement(engine, "Realm");

            if (authRealm != null)
            {
               XMLUtil.setAttribute(authRealm, "certificatePrincipal", "org.jboss.security.auth.certs.SubjectCNMapping", true);
            }
         }

         format(file, doc, sOriginalContents);
      }
   }

   /**
    * Installs web.xml.
    * @param file The file.
    */
   protected void installWeb(Resource file) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element webapp = doc.getDocumentElement();

      XMLUtil.removeNode(XMLUtil.findChildElementBySubelement(webapp,
         "filter", "filter-name", "CommonHeadersFilter"));
      XMLUtil.removeNode(XMLUtil.findChildElementBySubelement(webapp,
         "filter-mapping", "filter-name", "CommonHeadersFilter"));

      Element servlet = XMLUtil.findChildElementBySubelement(webapp, "servlet", "servlet-name", "default");

      if (servlet != null)
      {
         Element param = XMLUtil.findChildElementBySubelement(servlet, "init-param", "param-name", "listings");

         if (param != null)
         {
            Element value = XMLUtil.findChildElement(param, "param-value");

            if (value != null)
            {
               XMLUtil.setElementValue(value, "false");
            }
         }

         param = XMLUtil.findChildElementBySubelement(servlet, "init-param", "param-name", "output");

         if (param != null)
         {
            Element value = XMLUtil.findChildElement(param, "param-value");

            if (value != null)
            {
               boolean bReplace = false;

               try
               {
                  String sValue = XMLUtil.getElementValue(value);

                  bReplace = (sValue == null || Integer.parseInt(sValue, 10) < 8192);
               }
               catch (Exception e)
               {
                  bReplace = true;
               }

               if (bReplace)
               {
                  XMLUtil.setElementValue(value, "8192");
               }
            }
         }
      }

      format(file, doc, sOriginalContents);
   }

   /**
    * Installs jms-ds.xml.
    * @param file The file.
    */
   protected void installJMSConnectionFactories(Resource file) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element connectionFactories = doc.getDocumentElement();

      for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
      {
         Channel channel = (Channel)itr.next();

         if (channel instanceof MessageQueue && channel.isEnabled())
         {
            MessageQueue mq = (MessageQueue)channel;

            String sConnFactoryType = (channel.isTransactional()) ? "tx-connection-factory" : "no-tx-connection-factory";

            if (mq.isConnectionFactoryManaged())
            {
               String sCFJNDIName = getCFJNDIName(mq);
               Element connectionFactory = XMLUtil.findChildElementBySubelement(
                  connectionFactories, "tx-connection-factory", "jndi-name", sCFJNDIName);

               /*
                * If we change from tx-connection-factory to no-tx-connection-factory or the other way around
                * we delete the old definitions and create new ones from scratch
                */
               if (connectionFactory != null)
               {
                  if (!channel.isTransactional())
                  {
                     XMLUtil.removeNode(connectionFactory);

                     connectionFactory = null;
                  }
               }
               else
               {
                  connectionFactory = XMLUtil.findChildElementBySubelement(
                     connectionFactories, "no-tx-connection-factory", "jndi-name", sCFJNDIName);

                  if (connectionFactory != null)
                  {
                     if (channel.isTransactional())
                     {
                        XMLUtil.removeNode(connectionFactory);

                        connectionFactory = null;
                     }
                  }
               }

               if (mq.getConnectionFactory() == null)
               {
                  if (connectionFactory == null)
                  {
                     connectionFactory = doc.createElement(sConnFactoryType);
                     connectionFactories.appendChild(connectionFactory);
                  }

                  XMLUtil.setChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                     "jndi-name", sCFJNDIName, false);

                  XMLUtil.removeNode(XMLUtil.findChildElement(connectionFactory, "local-transaction"));

                  if (channel.isTransactional())
                  {
                     XMLUtil.setChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                        "xa-transaction", null, false);
                  }

                  XMLUtil.setChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                     "rar-name", "jms-ra.rar", false);
                  XMLUtil.setChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                     "connection-definition", "org.jboss.resource.adapter.jms.JmsConnectionFactory", false);

                  Element element = XMLUtil.findChildElement(connectionFactory, "config-property", "name", "SessionDefaultType");

                  if (element == null)
                  {
                     element = XMLUtil.addChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                        "config-property", "javax.jms.Topic");
                     XMLUtil.setAttribute(element, "name", "SessionDefaultType", true);
                     XMLUtil.setAttribute(element, "type", "java.lang.String", true);
                  }

                  element = XMLUtil.findChildElement(connectionFactory, "config-property", "name", "JmsProviderAdapterJNDI");

                  if (element == null)
                  {
                     element = XMLUtil.addChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                        "config-property", "java:/DefaultJMSProvider");
                     XMLUtil.setAttribute(element, "name", "JmsProviderAdapterJNDI", true);
                     XMLUtil.setAttribute(element, "type", "java.lang.String", true);
                  }

                  XMLUtil.setChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                     "max-pool-size", String.valueOf(mq.getMaxSenders() + ((mq.isReceivable()) ? 1 : 0)), true);
                  XMLUtil.setChildElement(connectionFactory, CONNECTION_FACTORY_ELEMENTS,
                     "security-domain-and-application", getJAASDomain(mq), true);
               }
               else
               {
                  XMLUtil.removeNode(connectionFactory);
               }
            }
         }
      }

      format(file, doc, sOriginalContents);
   }

   /**
    * Uninstalls jms-ds.xml.
    * @param file The file.
    */
   protected void uninstallJMSConnectionFactories(Resource file) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element connectionFactories = doc.getDocumentElement();

      for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
      {
         Channel channel = (Channel)itr.next();

         if (channel instanceof MessageQueue && channel.isEnabled())
         {
            MessageQueue mq = (MessageQueue)channel;

            if (mq.isConnectionFactoryManaged())
            {
               Element connectionFactory = XMLUtil.findChildElementBySubelement(
                  connectionFactories, "tx-connection-factory", "jndi-name", getCFJNDIName(mq));

               if (connectionFactory == null)
               {
                  connectionFactory = XMLUtil.findChildElementBySubelement(
                     connectionFactories, "no-tx-connection-factory", "jndi-name", getCFJNDIName(mq));
               }

               if (connectionFactory != null)
               {
                  XMLUtil.removeNode(connectionFactory);
               }
            }
         }
      }

      format(file, doc, sOriginalContents);
   }

   /**
    * Installs log4j.xml.
    * @param file The file.
    */
   protected void installLog4J(Resource file) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element configuration = doc.getDocumentElement();

      addFileAppender(configuration, SysUtil.CAPTION + "File", SysUtil.NAMESPACE + ".log");
      addFileAppender(configuration, SysUtil.CAPTION + "SQLFile", SysUtil.NAMESPACE + "-sql.log");

      Element appender = addAppender(configuration, SysUtil.CAPTION + "Console", "org.apache.log4j.ConsoleAppender");

      if (appender != null)
      {
         addAppenderParam(appender, "Target", "System.out");

         Element layout = XMLUtil.addChildElement(appender, null, "layout");

         layout.setAttribute("class", "org.apache.log4j.PatternLayout");
         addAppenderParam(layout, "ConversionPattern", "%d{ABSOLUTE} %-5p [%c{1}] %m%n");
      }

      Element category = XMLUtil.findChildElement(configuration, "category", "name", SysUtil.NAMESPACE);

      if (category == null)
      {
         category = XMLUtil.addChildElement(configuration, LOG4J_ELEMENTS, "category");
         category.setAttribute("name", SysUtil.PACKAGE);
         category.setAttribute("additivity", "false");
         XMLUtil.addChildElement(category, null, "priority").setAttribute("value", "INFO");
         XMLUtil.addChildElement(category, null, "appender-ref").setAttribute("ref", SysUtil.CAPTION + "File");

         configuration.insertBefore(doc.createComment(" development settings \r\n" +
            "   <category name=\"" + SysUtil.PACKAGE +  "\" additivity=\"false\">\r\n" +
            "      <priority value=\"DEBUG\"/>\r\n" +
            "      <appender-ref ref=\"" + SysUtil.CAPTION + "File\"/>\r\n" +
            "      <appender-ref ref=\"" + SysUtil.CAPTION + "Console\"/>\r\n" +
            "   </category>\r\n" +
            "   <root>\r\n" +
            "      <appender-ref ref=\"CONSOLE\"/>\r\n" +
            "      <appender-ref ref=\"FILE\"/>\r\n" +
            "   </root>\r\n" +
            "   end of development settings "), category);

         configuration.insertBefore(doc.createTextNode("\n\n   "), category);
         configuration.insertBefore(doc.createComment(" production settings "), category);
         configuration.insertBefore(doc.createTextNode("\n   "), category);
         configuration.insertBefore(doc.createComment(" end of production settings "), category.getNextSibling().getNextSibling());

         Element root = XMLUtil.findChildElement(configuration, "root");

         if (root != null)
         {
            XMLUtil.removeNode(XMLUtil.findChildElement(root, "appender-ref", "ref", "CONSOLE"));

            if (XMLUtil.findChildElement(root, "priority") == null &&
               XMLUtil.findChildElement(root, "level") == null)
            {
               XMLUtil.addChildElement(root, ROOT_ELEMENTS, "priority").setAttribute("value", "INFO");
            }
         }
      }

      format(file, doc, sOriginalContents);
   }

   /**
    * Adds an appender to a log4j configuration.
    * @param configuration The log4j configuration element.
    * @param sName The appender name.
    * @param sClass The appender class name.
    * @return The appender element, if added, or null.
    */
   protected Element addAppender(Element configuration, String sName, String sClass)
   {
      Element appender = XMLUtil.findChildElement(configuration, "appender", "name", sName);

      if (appender != null)
      {
         return null;
      }

      appender = XMLUtil.addChildElement(configuration, LOG4J_ELEMENTS, "appender");
      appender.setAttribute("name", sName);
      appender.setAttribute("class", sClass);
      XMLUtil.addChildElement(appender, null, "errorHandler").setAttribute("class", "org.jboss.logging.util.OnlyOnceErrorHandler");

      return appender;
   }

   /**
    * Adds a file appender to a log4j configuration.
    * @param configuration The log4j configuration element.
    * @param sName The appender name.
    * @param sFile The file name.
    */
   protected void addFileAppender(Element configuration, String sName, String sFile)
   {
      Element appender = addAppender(configuration, sName, "org.jboss.logging.appender.DailyRollingFileAppender");

      if (appender != null)
      {
         addAppenderParam(appender, "File", "${jboss.server.home.dir}/log/" + sFile);
         addAppenderParam(appender, "Append", "true");
         addAppenderParam(appender, "DatePattern", "'.'yyyy-MM-dd");

         Element layout = XMLUtil.addChildElement(appender, null, "layout");

         layout.setAttribute("class", "org.apache.log4j.PatternLayout");
         addAppenderParam(layout, "ConversionPattern", "%d %-5p [%c] (%t:%x) %m%n");
      }
   }

   /**
    * Adds a log4j appender parameter.
    * @param appender The appender element.
    * @param sName The parameter name.
    * @param sValue The parameter value.
    */
   protected void addAppenderParam(Element appender, String sName, String sValue)
   {
      Element param = XMLUtil.addChildElement(appender, null, "param");

      param.setAttribute("name", sName);
      param.setAttribute("value", sValue);
   }

   /**
    * Uninstalls log4j.xml.
    * @param file The file.
    */
   protected void uninstallLog4J(Resource file) throws IOException
   {
      if (file.exists())
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element configuration = doc.getDocumentElement();

         XMLUtil.removeNode(XMLUtil.findChildElement(configuration, "appender", "name", SysUtil.CAPTION + "File"));
         XMLUtil.removeNode(XMLUtil.findChildElement(configuration, "appender", "name", SysUtil.CAPTION + "SQLFile"));
         XMLUtil.removeNode(XMLUtil.findChildElement(configuration, "appender", "name", SysUtil.CAPTION + "Console"));

         Element category = XMLUtil.findChildElement(configuration, "category", "name", SysUtil.PACKAGE);

         if (category != null)
         {
            List removalList = new ArrayList();

            for (Node node = category.getPreviousSibling();
               enqueueComment(node, removalList);
               node = node.getPreviousSibling()) ;

            for (Node node = category.getNextSibling();
               enqueueComment(node, removalList);
               node = node.getNextSibling()) ;

            for (int i = 0, n = removalList.size(); i < n; ++i)
            {
               XMLUtil.removeNode((Node)removalList.get(i));
            }

            XMLUtil.removeNode(category);
         }

         format(file, doc, sOriginalContents);
      }
   }

   /**
    * Optionally enqueues a comment for removal.
    * @param node The comment to enqueue.
    * @param removalList The list for equeueing the nodes.
    * @return True if the node was enqueued, false if not.
    */
   protected boolean enqueueComment(Node node, List removalList)
   {
      if (node instanceof Comment)
      {
         String sData = ((Comment)node).getData();

         if (sData != null)
         {
            if (sData.indexOf("production settings") >= 0 ||
               sData.indexOf("development settings") >= 0)
            {
               removalList.add(node);
               return true;
            }
         }
      }
      else if (node instanceof Text)
      {
         removalList.add(node);
         return true;
      }

      return false;
   }

   /**
    * Installs login-config.xml.
    * @param file The file.
    * @param db The default database.
    */
   protected void installLoginConfig(Resource file, RelationalDatabase db) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element policies = doc.getDocumentElement();
      Element policy = XMLUtil.setChildElement(policies, null, "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName, null, true);
      Element authentication = XMLUtil.setChildElement(policy, null, "authentication", null, true);
      Element module = XMLUtil.setChildElement(authentication, null, "login-module", null, true);
      SQLAdapter sqlAdapter = getAdapter(db);
      StringBuffer buf = new StringBuffer(128);

      module.setAttribute("flag", "required");

      switch (m_metadata.getAuthenticationProtocol())
      {
         case Metadata.AUTH_PROTOCOL_BASIC:
            module.setAttribute("code", "org.jboss.security.auth.spi.DatabaseServerLoginModule");

            if (db != null)
            {
               XMLUtil.setChildElement(module, null, "module-option", "name", "dsJndiName",
                  "java:/" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/" + ((RelationalDatabaseFragment)db.getDefaultFragment()).getAlias(), true);
               XMLUtil.setChildElement(module, null, "module-option", "name", "hashAlgorithm", "SHA-256", true);
               XMLUtil.setChildElement(module, null, "module-option", "name", "hashCharset", "UTF-8", true);
               XMLUtil.setChildElement(module, null, "module-option", "name", "hashEncoding", "base64", true);

               buf.append("select P.passwordHash from NJUser U inner join NJUserPassword P on U.id=P.userId where U.loginName=? and U.status=1 and P.activeFlag=");
               sqlAdapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.TRUE);

               XMLUtil.setChildElement(module, null, "module-option", "name", "principalsQuery", fixSQL(buf.toString(), db), true);
               XMLUtil.setChildElement(module, null, "module-option", "name", "rolesQuery",
                  fixSQL("select '" + SysUtil.NAMESPACE + "users', 'Roles' from NJUser where loginName=? and status=1", db), true);
            }

            break;

         case Metadata.AUTH_PROTOCOL_SPNEGO:
            module.setAttribute("code", SysUtil.PACKAGE + ".core.container.platform.jboss.JBossLoginModule");
            XMLUtil.setChildElement(module, null, "module-option", "name", "domain", m_metadata.getAuthenticationDomain(), true);
            XMLUtil.setChildElement(module, null, "module-option", "name", "service", m_metadata.getAuthenticationService(), true);
            break;

         case Metadata.AUTH_PROTOCOL_CERTIFICATE:
            module.setAttribute("code", "org.jboss.security.auth.spi.DatabaseCertLoginModule");

            if (db != null)
            {
               XMLUtil.setChildElement(module, null, "module-option", "name", "dsJndiName",
                  "java:/" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/" + ((RelationalDatabaseFragment)db.getDefaultFragment()).getAlias(), true);
               XMLUtil.setChildElement(module, null, "module-option", "name", "rolesQuery",
                  fixSQL("select '" + SysUtil.NAMESPACE + "users', 'Roles' from NJUser where loginName=? and status=1", db), true);
               XMLUtil.setChildElement(module, null, "module-option", "name", "verifier", "org.jboss.security.auth.certs.AnyCertVerifier", false);
               XMLUtil.setChildElement(module, null, "module-option", "name", "securityDomain", "java:/jaas/" + SysUtil.NAMESPACE + '-' + m_sEnvironmentName, false);
            }

            break;
      }

      // Kerberos SSO policy
      policy = XMLUtil.setChildElement(policies, null, "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-kerberos", null, false);
      authentication = XMLUtil.setChildElement(policy, null, "authentication", null, false);
      module = XMLUtil.setChildElement(authentication, null, "login-module", null, false);
      module.setAttribute("flag", "required");
      module.setAttribute("code", "com.sun.security.auth.module.Krb5LoginModule");
      XMLUtil.setChildElement(module, null, "module-option", "name", "useTicketCache", "false", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "doNotPrompt", "false", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "storeKey", "false", true);

      // Kerberos Silent SSO policy
      policy = XMLUtil.setChildElement(policies, null, "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-kerberos-silent", null, false);
      authentication = XMLUtil.setChildElement(policy, null, "authentication", null, false);
      module = XMLUtil.setChildElement(authentication, null, "login-module", null, false);
      module.setAttribute("flag", "required");
      module.setAttribute("code", "com.sun.security.auth.module.Krb5LoginModule");
      XMLUtil.setChildElement(module, null, "module-option", "name", "useTicketCache", "true", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "renewTGT", "true", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "doNotPrompt", "false", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "storeKey", "false", true);

      // Kerberos Server SSO policy
      policy = XMLUtil.setChildElement(policies, null, "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-server-kerberos", null, false);
      authentication = XMLUtil.setChildElement(policy, null, "authentication", null, false);

      Properties authProperties = m_metadata.getAuthenticationProperties();

      if (!authProperties.isEmpty())
      {
         module = XMLUtil.setChildElement(authentication, null, "login-module", "code", PropertiesLoginModule.class.getCanonicalName(), null, true);
         module.setAttribute("flag", "required");

         for (Enumeration itr = authProperties.keys(); itr.hasMoreElements(); )
         {
            String sKey = (String)itr.nextElement();

            XMLUtil.setChildElement(module, null, "module-option", "name", sKey, authProperties.getProperty(sKey), true);
         }
      }
      else
      {
         module = XMLUtil.findChildElement(authentication, "login-module", "code", PropertiesLoginModule.class.getCanonicalName());

         if (module != null)
         {
            XMLUtil.removeNode(module);
         }
      }

      module = XMLUtil.setChildElement(authentication, null, "login-module", "code", "com.sun.security.auth.module.Krb5LoginModule", null, false);
      module.setAttribute("flag", "required");
      XMLUtil.setChildElement(module, null, "module-option", "name", "useTicketCache", "false", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "doNotPrompt", "false", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "isInitiator", "false", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "storeKey", "true", true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "useKeyTab", "true", true);

      // Configures client certificate authentication policy
      if (db != null)
      {
         policy = XMLUtil.setChildElement(policies, null, "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-cert", null, true);
         authentication = XMLUtil.setChildElement(policy, null, "authentication", null, true);
         module = XMLUtil.setChildElement(authentication, null, "login-module", null, true);
         module.setAttribute("flag", "required");
         module.setAttribute("code", "org.jboss.security.auth.spi.DatabaseCertLoginModule");
         XMLUtil.setChildElement(module, null, "module-option", "name", "dsJndiName",
            "java:/" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/" + ((RelationalDatabaseFragment)db.getDefaultFragment()).getAlias(), true);
         XMLUtil.setChildElement(module, null, "module-option", "name", "rolesQuery",
            fixSQL("select '" + SysUtil.NAMESPACE + "users', 'Roles' from NJUser where loginName=? and status=1", db), true);
         XMLUtil.setChildElement(module, null, "module-option", "name", "verifier", "org.jboss.security.auth.certs.AnyCertVerifier", false);
         XMLUtil.setChildElement(module, null, "module-option", "name", "securityDomain", "java:/jaas/" + SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-cert", false);
      }

      for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
      {
         Channel channel = (Channel)itr.next();

         if (channel instanceof MessageQueue && channel.isEnabled())
         {
            MessageQueue mq = (MessageQueue)channel;

            addApplicationPolicy(policies, getJAASDomain(channel), (channel.isTransactional()) ? "TxCM" : "NoTxCM", getCFJNDIName(mq), mq.getUser(), mq.getPassword());
         }
      }

      // Database connections require user and password
      for (Iterator dsItr = m_metadata.getDataSourceIterator(); dsItr.hasNext();)
      {
         DataSource ds = (DataSource)dsItr.next();

         if (ds.isEnabled() && ds instanceof RelationalDatabase)
         {
            RelationalDatabase dataBase = (RelationalDatabase)ds;

            for (Iterator fragmentItr = dataBase.getFragmentIterator(); fragmentItr.hasNext();)
            {
               RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)fragmentItr.next();

               if (fragment.isFirst())
               {
                  addApplicationPolicy(policies, getJAASDomain(fragment), SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/" + fragment.getAlias(), fragment.getUser(), fragment.getPassword());
               }
            }
         }
      }

      m_installerStrategy.addJMSDBApplicationPolicy(policies, m_installerStrategy.getJMSDBConnectionInfo());

      format(file, doc, sOriginalContents);
   }

   /**
    * @see #addApplicationPolicy(Element, String, String, String, String, String)
    */
   protected void addApplicationPolicy(Element policies, String sPolicyName, String sJNDIName, String sUser, String sPassword)
   {
      addApplicationPolicy(policies, sPolicyName, "XATxCM", sJNDIName, sUser, sPassword);
   }

   /**
    * Adds an application-policy
    * @param policies The policies element under which to add the policy.
    * @param sPolicyName The policy name.
    * @param sJCAServiceName The jca service part of the managedConnectionFactoryName
    * @param sDBAlias The database alias.
    * @param sUser The username
    * @param sPassword The password
    */
   protected void addApplicationPolicy(Element policies, String sPolicyName, String sJCAServiceName, String sJNDIName, String sUser, String sPassword)
   {
      //Create the XML configuration policy
      Element policy = XMLUtil.setChildElement(policies, null, "application-policy", "name", sPolicyName, null, true);
      Element authentication = XMLUtil.setChildElement(policy, null, "authentication", null, true);
      Element module = XMLUtil.setChildElement(authentication, null, "login-module", null, true);
      module.setAttribute("flag", "required");

      module.setAttribute("code", (SysUtil.ENTERPRISE) ?
         SysUtil.PACKAGE + ".core.container.platform.jboss.JBossStreamCipherLoginModule" :
            "org.jboss.resource.security.ConfiguredIdentityLoginModule");

      XMLUtil.setChildElement(module, null, "module-option", "name", "managedConnectionFactoryName",
         "jboss.jca:service=" + sJCAServiceName + ",name=" + sJNDIName, true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "principal", sUser, true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "userName", sUser, true);
      XMLUtil.setChildElement(module, null, "module-option", "name", "password", encrypt(sPassword), true);
   }

   /**
    * Installs a login config application-policy for the JMS engine
    * @param file The config file.
    * @param policyName The policy name.
    * @param db The database to use for authentication.
    * @throws IOException
    */
   protected void installMQLoginConfig(Resource file, String policyName, RelationalDatabase db) throws IOException
   {
      if (db != null)
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element policies = doc.getDocumentElement();
         Element policy = XMLUtil.findChildElement(policies, "application-policy", "name", policyName);

         if (policy != null)
         {
            SQLAdapter sqlAdapter = getAdapter(db);
            StringBuffer buf = new StringBuffer(128);
            Element authentication = XMLUtil.setChildElement(policy, null, "authentication", null, true);
            Element module = XMLUtil.setChildElement(authentication, null, "login-module", null, true);

            module.setAttribute("flag", "required");
            module.setAttribute("code", "org.jboss.security.auth.spi.DatabaseServerLoginModule");
            XMLUtil.setChildElement(module, null, "module-option", "name", "dsJndiName",
               "java:/" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/" + ((RelationalDatabaseFragment)db.getDefaultFragment()).getAlias(), true);
            XMLUtil.setChildElement(module, null, "module-option", "name", "hashAlgorithm", "SHA-256", true);
            XMLUtil.setChildElement(module, null, "module-option", "name", "hashCharset", "UTF-8", true);
            XMLUtil.setChildElement(module, null, "module-option", "name", "hashEncoding", "base64", true);

            buf.append("select P.passwordHash from NJUser U inner join NJUserPassword P on U.id=P.userId where U.loginName=? and U.status=1 and P.activeFlag=");
            sqlAdapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.TRUE);

            XMLUtil.setChildElement(module, null, "module-option", "name", "principalsQuery", fixSQL(buf.toString(), db), true);
            XMLUtil.setChildElement(module, null, "module-option", "name", "rolesQuery",
               fixSQL("select loginName, 'Roles' from NJUser where loginName=? and status=1", db), true);
         }

         format(file, doc, sOriginalContents);
      }
   }

   /**
    * Uninstalls login-config.xml.
    * @param file The file.
    */
   protected void uninstallLoginConfig(Resource file) throws IOException
   {
      if (file.exists())
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element policies = doc.getDocumentElement();

         XMLUtil.removeNode(XMLUtil.findChildElement(policies,
            "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName));

         XMLUtil.removeNode(XMLUtil.findChildElement(policies,
            "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-kerberos"));

         XMLUtil.removeNode(XMLUtil.findChildElement(policies,
            "application-policy", "name", SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-kerberos-silent"));

         for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
         {
            Channel channel = (Channel)itr.next();

            if (channel instanceof MessageQueue && channel.isEnabled())
            {
               MessageQueue mq = (MessageQueue)channel;

               if (mq.isConnectionFactoryManaged())
               {
                  XMLUtil.removeNode(XMLUtil.findChildElement(doc.getDocumentElement(),
                     "application-policy", "name", getJAASDomain(mq)));
               }
            }
         }

         format(file, doc, sOriginalContents);
      }
   }

   /**
    * Installs the conf/jboss-service.xml file.
    * @param file The file.
    * @throws IOException If an I/O error occurs.
    */
   protected void installJBossService(Resource file, String sTrustStoreFileName) throws IOException
   {
      // J2EE Descriptor password encryption
      String sEncryptedKeyStorePassword = m_metadata.getKeyStorePassword();
      CharacterStreamCipherDispatcher dispatcher = null;

      if (!m_metadata.isEncrypted() && SysUtil.ENTERPRISE)
      {
         dispatcher = new CharacterStreamCipherDispatcher();

         if (m_properties.getProperty("cipher.scheme") == null)
         {
            m_properties.setProperty("cipher.scheme", m_metadata.getEncryptionScheme());
         }

         dispatcher.init(m_properties);
         sEncryptedKeyStorePassword = dispatcher.encrypt(sEncryptedKeyStorePassword);
      }

      String sNullTagFixupRegEx = "(?s)<parameter>\\s*<null/>\\s*</parameter>";
      String sNullTagFixup = "<parameter><null/></parameter>";

      Document doc = parse(file, null);
      String sOriginalContents = format(doc).replaceAll(sNullTagFixupRegEx, sNullTagFixup);
      Element server = doc.getDocumentElement();
      Element mbean = XMLUtil.setChildElement(server, null, "mbean", "name", "jboss.security:service=SecurityDomainFor" + SysUtil.CAPTION + "Cert", null, true);

      writeCertificateSecurityDomain(mbean, "-cert", sEncryptedKeyStorePassword, sTrustStoreFileName);

      // All web authentication handled by client certificate authentication
      if (m_metadata.getAuthenticationProtocol() == Metadata.AUTH_PROTOCOL_CERTIFICATE)
      {
         mbean = XMLUtil.setChildElement(server, null, "mbean", "name", "jboss.security:service=SecurityDomainFor" + SysUtil.CAPTION, null, true);
         writeCertificateSecurityDomain(mbean, "", sEncryptedKeyStorePassword, sTrustStoreFileName);
      }
      else
      {
         mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss.security:service=SecurityDomainFor" + SysUtil.CAPTION, false);
         XMLUtil.removeNode(mbean);
      }

      format(file, format(doc).replaceAll(sNullTagFixupRegEx, sNullTagFixup), sOriginalContents);
   }

   /**
    * Writes the XML for a certificate authentication security domain.
    *
    * @param mbean The MBean element in jboss-service.xml.
    * @param sSuffix The security domain suffix. Empty string for none.
    * @param sKeyStorePassword The password for the key store.
    */
   protected void writeCertificateSecurityDomain(Element mbean, String sSuffix, String sKeyStorePassword, String sTrustStoreFileName)
   {
      Element constructor = XMLUtil.setChildElement(mbean, null, "constructor", null, true);
      Element arg = XMLUtil.setChildElement(constructor, null, "arg", null, true);

      XMLUtil.setAttribute(arg, "type", "java.lang.String", true);
      XMLUtil.setAttribute(arg, "value", SysUtil.PACKAGE + sSuffix, true);

      XMLUtil.setChildElement(mbean, null, "attribute", "name", "KeyStoreURL", "resource:ssl.keystore", false);

      if (SysUtil.ENTERPRISE)
      {
         XMLUtil.setChildElement(mbean, null, "attribute", "name", "KeyStorePass",
            "{CLASS}" + SysUtil.PACKAGE + ".core.container.platform.jboss.JBossKeyStorePassword:" +
            sKeyStorePassword, false);
      }
      else
      {
         XMLUtil.setChildElement(mbean, null, "attribute", "name", "KeyStorePass", sKeyStorePassword, false);
      }

      XMLUtil.setChildElement(mbean, null, "attribute", "name", "TrustStoreURL", "resource:" + sTrustStoreFileName, true);
      // JBoss 4.0.5.GA does not support encrypted trust store password
      XMLUtil.setChildElement(mbean, null, "attribute", "name", "TrustStorePass", TRUST_STORE_PASSWORD, true);
      XMLUtil.setChildElement(mbean, null, "depends", "jboss.security:service=JaasSecurityManager", true);
      XMLUtil.setAttribute(mbean, "code", "org.jboss.security.plugins.JaasSecurityDomain", true);
   }

   /**
    * Un-installs the conf/jboss-service.xml file.
    * @param file The file.
    * @throws IOException If an I/O error occurs.
    */
   protected void uninstallJBossService(Resource file) throws IOException
   {
      Document doc = parse(file, null);
      String sOriginalContents = format(doc);
      Element server = doc.getDocumentElement();
      Element mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss.security:service=SecurityDomainFor" + SysUtil.CAPTION + "Cert", false);

      XMLUtil.removeNode(mbean);

      format(file, doc, sOriginalContents);
   }

   /**
    * Installs the trust store file.
    * @param file The file.
    * @throws IOException If an I/O error occurs.
    */
   protected void installTrustStore(Resource file) throws IOException
   {
      // TODO: The new trust store is not used until the keystore is reloaded (mbean operation) and Tomcat is restarted
      InputStream fis = null;
      OutputStream fos = null;

      try
      {
         KeyStore keyStore = KeyStore.getInstance("JKS");

         // Read existing trust store
         fis = file.getInputStream();

         if (fis != null)
         {
            keyStore.load(fis, TRUST_STORE_PASSWORD.toCharArray());
            fis.close();
         }
         else
         {
            keyStore.load(null, null);
         }

         // Remove all nexj certificates, leaving others
         for (Enumeration e = keyStore.aliases(); e.hasMoreElements(); )
         {
            String sAlias = (String)e.nextElement();

            if (sAlias.startsWith(SysUtil.NAMESPACE + "-"))
            {
               keyStore.deleteEntry(sAlias);
            }
         }

         // Add the nexj elements.
         for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
         {
            Channel channel = (Channel)itr.next();

            if (channel instanceof HTTPChannel && channel.isEnabled())
            {
               HTTPChannel http = (HTTPChannel)channel;

               if (http.isSecure() && http.getTrustedCertificate() != null)
               {
                  keyStore.setCertificateEntry(SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-channel-" +
                     http.getName().toLowerCase(Locale.ENGLISH), http.getTrustedCertificate());
               }
            }
         }

         // Add Generic RPC trusted certificate
         Certificate genericCertificate = m_metadata.getTrustedCertificate();

         if (genericCertificate != null)
         {
            keyStore.setCertificateEntry(SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-rpc-generic", genericCertificate);
         }

         fos = file.getOutputStream();
         keyStore.store(fos, TRUST_STORE_PASSWORD.toCharArray());
      }
      catch (Exception ex)
      {
         ObjUtil.rethrow(ex);
      }
      finally
      {
         IOUtil.close(fis);

         if (fos != null)
         {
            fos.close();
         }
      }
   }

   /**
    * Parses an XML file.
    * @param resource The resource holding the XML.
    * @param sDefaultXML The default XML if the file does not exist.
    * @return The parsed DOM.
    * @throws IOException if an I/O error occurs.
    */
   protected Document parse(Resource resource, String sDefaultXML) throws IOException
   {
      InputStream istream = resource.getInputStream();

      try
      {
         if (istream != null)
         {
            istream = new BufferedInputStream(istream);

            return XMLUtil.parse(new InputSource(istream), (URL)null);
         }
         else if (sDefaultXML != null)
         {
            return XMLUtil.parse(new StringReader(sDefaultXML));
         }
         else
         {
            throw new IOException("Missing resource \"" + resource + "\"");
         }
      }
      finally
      {
         IOUtil.close(istream);
      }
   }

   /**
    * Parses an XML resource, optionally taking data from an "old resource" and
    * deleting it, if the resource does not exist.
    *
    * @param resource The resource holding the XML.
    * @param oldResource The old resource to use and then delete if resource is not found.
    * @param bDeleteOldResource True to delete the old resource.
    * @return The parsed DOM.
    * @throws IOException if an I/O error occurs.
    */
   protected Document parse(Resource resource, Resource oldResource, boolean bDeleteOldResource) throws IOException
   {
      InputStream istream = resource.getInputStream();
      boolean bParsedOldResource = false;

      try
      {
         if (istream == null)
         {
            if (oldResource == null)
            {
               throw new IllegalArgumentException("Old resource unspecified");
            }

            istream = oldResource.getInputStream();

            if (istream == null)
            {
               throw new IOException("Missing resource \"" + oldResource + "\"");
            }

            bParsedOldResource = true;
         }

         istream = new BufferedInputStream(istream);

         return XMLUtil.parse(new InputSource(istream), (URL)null);
      }
      finally
      {
         IOUtil.close(istream);

         if (bParsedOldResource && bDeleteOldResource)
         {
            oldResource.delete();
         }
      }
   }

   /**
    * Formats and writes out SQL to a resource.
    * @param resource The resource.
    * @param sqlList The list with SQL statements, without statement separators.
    * @param db The relational DB metadata object.
    */
   protected void formatSQL(Resource resource, List sqlList, RelationalDatabase db) throws IOException
   {
      Collections.sort(sqlList, DDL_COMPARATOR);

      StringBuffer buf = new StringBuffer(256);
      String sSeparator = getAdapter(db).createSchemaManager().getSeparator();

      sSeparator = sSeparator.replaceAll("\\s*/", ";");

      for (Iterator iterator = sqlList.iterator(); iterator.hasNext();)
      {
         buf.append((String)iterator.next()).append(sSeparator);
      }

      format(resource, buf.toString(), null);
   }

   /**
    * Formats and writes out XML to a resource, but only if it has changed from
    * its original contents.
    * @param resource The resource.
    * @param node The DOM node.
    * @param sOriginal The original contents of the resource.
    */
   protected void format(Resource resource, Node node, String sOriginal) throws IOException
   {
      format(resource, format(node), sOriginal);
   }

   /**
    * Formats and writes out XML to a resource, but only if it has changed from
    * its original contents.
    * @param resource The resource.
    * @param sToFormat The string to be formatted.
    * @param sOriginal The original contents of the resource.
    */
   protected void format(Resource resource, String sToFormat, String sOriginal) throws IOException
   {
      if (sToFormat.equals(sOriginal))
      {
         return;
      }

      // Write XML String to file
      OutputStream ostream = resource.getOutputStream();
      Writer fileWriter = null;

      try
      {
         fileWriter = new OutputStreamWriter(new BufferedOutputStream(ostream), XMLUtil.ENCODING);
         fileWriter.write(sToFormat);
      }
      finally
      {
         if (fileWriter != null)
         {
            fileWriter.close();
            fileWriter = null;
            ostream = null;
         }

         if (ostream != null)
         {
            ostream.close();
            ostream = null;
         }
      }
   }

   /**
    * Formats an XML file, returning its contents as a String.
    * @param node The DOM node to format.
    * @return The String representation of node.
    */
   protected String format(Node node)
   {
      return format(node, true);
   }

   /**
    * Formats an XML file, returning its contents as a String.
    * @param node The DOM node to format.
    * @param bHeader True to output the XML declaration header.
    * @return The String representation of node.
    */
   protected String format(Node node, boolean bHeader)
   {
      StringWriter writer = new StringWriter();

      writer.write(XMLUtil.formatXML(new DOMSource(node), bHeader));

      return writer.toString();
   }

   /**
    * Fixes an SQL statement for a given database.
    * @param sSQL The SQL statement.
    * @param db The relational database.
    * @return The resulting SQL string.
    */
   protected String fixSQL(String sSQL, RelationalDatabase db)
   {
      sSQL = m_installerStrategy.formatSQL(sSQL, db);

      // if custom JMS DB was specified
      if (!db.isEnabled())
      {
         return sSQL;
      }

      StringBuffer buf = new StringBuffer(sSQL.length());
      Pattern pattern = Pattern.compile(BIND_VAR_PATTERN, Pattern.CASE_INSENSITIVE);
      Matcher matcher = pattern.matcher(sSQL);
      int i = 0;

      while (matcher.find())
      {
         if (db == null)
         {
            matcher.appendReplacement(buf, "?");
         }
         else
         {
            matcher.appendReplacement(buf, "");
            getAdapter(db).appendBind(buf, i++);
         }
      }

      matcher.appendTail(buf);

      return buf.toString();
   }

   /**
    * Gets a database schema owner prefix.
    * @param db The database.
    * @return The owner prefix (owner.) or an empty string.
    */
   protected String getOwnerPrefix(RelationalDatabase db)
   {
      String sOwner = null;

      // if custom JMS DB was specified
      if (!db.isEnabled())
      {
         sOwner = m_properties.getProperty("jms.jdbc.owner");

         if (sOwner == null && JDBCInfo.DB_TYPE_MSSQL ==
            JDBCInfo.getInstance(db.getDriver(), db.getAdapter().getName()).getDBType())
         {
            sOwner = "dbo";
         }
      }
      else
      {
         sOwner = getSchemaManager(db).getOwner();
      }

      return (StringUtil.isEmpty(sOwner)) ? "" : sOwner + '.';
   }

   /**
    * Gets the schema manager for a given database.
    * @param db The relational database.
    * @return The schema manager.
    */
   protected SQLSchemaManager getSchemaManager(RelationalDatabase db)
   {
      SQLSchemaManager manager = (SQLSchemaManager)m_schemaManagerMap.get(db);

      if (manager == null)
      {
         manager = getAdapter(db).createSchemaManager(db);
         m_schemaManagerMap.put(db, manager);
      }

      return manager;
   }

   /**
    * Gets an SQL adapter instance for a given database.
    * @param db The database.
    * @return The SQL adapter.
    */
   protected SQLAdapter getAdapter(RelationalDatabase db)
   {
      SQLAdapter adapter = (SQLAdapter)m_adapterMap.get(db);

      if (adapter == null)
      {
         adapter = (SQLAdapter)db.getComponent().getInstance(null);
         m_adapterMap.put(db, adapter);
      }

      return adapter;
   }

   /**
    * @return The default relational database.
    */
   protected RelationalDatabase getDefaultDatabase()
   {
      DataSource ds = m_metadata.getDataSource("DefaultRelationalDatabase");

      if (ds instanceof RelationalDatabase && ds.isEnabled())
      {
         return (RelationalDatabase)ds;
      }

      return null;
   }

   /**
    * Gets a message queue JMS connection factory JNDI name.
    * @param mq The message queue.
    * @param sContext Additional context.
    * @return The JMS connection factory JNDI name.
    */
   protected String getCFJNDIName(MessageQueue mq)
   {
      return SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jms/cf/" + mq.getName();
   }

   /**
    * Gets a channel JAAS domain name.
    * @param channel The channel.
    * @return The JAAS domain name.
    */
   protected String getJAASDomain(Channel channel)
   {
      return SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-channel-" + channel.getName();
   }

   /**
    * Gets a data source JAAS domain name.
    * @param fragment The data source fragment.
    * @return The JAAS domain name.
    */
   protected String getJAASDomain(RelationalDatabaseFragment fragment)
   {
      return SysUtil.NAMESPACE + '-' + m_sEnvironmentName + "-datasource-" + fragment.getAlias();
   }


   // inner classes

   /**
    * Specifies a JBoss server directory and associated subdirectories.
    */
   protected static class Location
   {
      // constants

      /**
       * Scheme for resources on the local filesystem.
       */
      public final static String FILE_SCHEME = "file";

      /**
       * Scheme for resources accessed via ssh.
       */
      public final static String SSH_SCHEME = "ssh";

      // attributes

      /**
       * The network port of this location.
       */
      private int m_nPort;

      // associations

      /**
       * Deployment server base directory, e.g. .../jboss/server/default
       */
      private Resource m_baseDir;

      /**
       * Deployment directory, e.g. .../jboss/server/default/deploy
       */
      private Resource m_deploymentDir;

      /**
       * Farm directory, e.g. .../jboss/server/all/farm
       */
      private Resource m_farmDir;

      /**
       * Singleton deployment directory, e.g. .../jboss/server/all/deploy-hasingleton
       */
      private Resource m_singletonDir;

      /**
       * Configuration directory, e.g. .../jboss/server/default/conf
       */
      private Resource m_configDir;

      /**
       * The scheme for connecting to the remote location.
       */
      private String m_sScheme;

      /**
       * The host of this deployment location.
       */
      private String m_sHost;

      /**
       * The user id for authenticating to the remote location.
       */
      private String m_sUser;

      /**
       * The password for authenticating to the remote location.
       */
      private String m_sPassword;

      // constructors

      /**
       * Constructs the location.
       * @param sDir The server directory.
       * @param bCluster True if installing a cluster.
       * @throws IOException if the directory does not exist.
       */
      public Location(String sDir, boolean bCluster, String sUser, String sPassword) throws IOException
      {
         if (sDir.startsWith(SSH_SCHEME + ":") || sDir.startsWith(FILE_SCHEME + ":"))
         {
            URI uri = null;

            try
            {
               uri = new URI(sDir);
            }
            catch (URISyntaxException e)
            {
               ObjUtil.rethrow(e);
            }

            m_baseDir = new Resource(uri.getPath());
            m_sHost = uri.getHost();
            m_nPort = uri.getPort();
            m_sScheme = uri.getScheme();

            String sUserInfo = uri.getUserInfo();

            if (sUserInfo == null)
            {
               m_sUser = sUser;
               m_sPassword = sPassword;
            }
            else
            {
               String[] sCredentialArray = sUserInfo.split(":");

               if (sCredentialArray.length != 2)
               {
                  throw new IllegalArgumentException("Invalid user/password in deployment location \"" + uri + "\"");
               }

               m_sUser = sCredentialArray[0];
               m_sPassword = sCredentialArray[1];
            }
         }
         else
         {
            m_baseDir = new Resource(sDir);
            m_sScheme = FILE_SCHEME;
         }

         m_deploymentDir = new Resource(m_baseDir, "deploy");
         m_configDir = new Resource(m_baseDir, "conf");

         if (bCluster)
         {
            m_farmDir = new Resource(m_baseDir, "farm");
            m_singletonDir = new Resource(m_baseDir, "deploy-hasingleton");
         }
         else
         {
            m_farmDir = m_deploymentDir;
            m_singletonDir = m_deploymentDir;
         }
      }

      // operations

      /**
       * @return The JBoss server base directory, .../
       */
      public Resource getBaseDir()
      {
         return m_baseDir;
      }

      /**
       * @return The JBoss deployment directory, .../deploy
       */
      public Resource getDeploymentDir()
      {
         return m_deploymentDir;
      }

      /**
       * @return The JBoss farm deployment directory, either .../farm or .../deploy
       */
      public Resource getFarmDir()
      {
         return m_farmDir;
      }

      /**
       * @return The JBoss singleton deployment directory, either .../deploy-hasingleton or .../deploy
       */
      public Resource getSingletonDir()
      {
         return m_singletonDir;
      }

      /**
       * @return The JBoss configuration directory.
       */
      public Resource getConfigDir()
      {
         return m_configDir;
      }

      /**
       * @return The JBoss configuration bootstrap directory.
       */
      public Resource getBootstrapDir()
      {
         return new Resource(m_configDir, "bootstrap");
      }

      /**
       * @return The JBoss configuration props directory.
       */
      public Resource getPropsDir()
      {
         return new Resource(m_configDir, "props");
      }

      /**
       * Gets a resource connection for this location.
       *
       * @param initialProperties Properties to use when creating a new resource connection.
       * @return The resource connection for this location.
       */
      public ResourceConnection getResourceConnection(Properties initialProperties)
      {
         if (m_sScheme.equals(FILE_SCHEME))
         {
            return new LocalResourceConnection();
         }
         else if (m_sScheme.equals(SSH_SCHEME))
         {
            if (!initialProperties.getProperty("installer.cmd.ssh.get", "").equals(""))
            {
               return new RemoteResourceConnection(m_sHost, m_nPort, m_sUser, m_sPassword, initialProperties);
            }
            else
            {
               return new JSchResourceConnection(m_sHost, m_nPort, m_sUser, m_sPassword, initialProperties);
            }
         }
         else
         {
            throw new IllegalArgumentException("Unknown deployment URL scheme \"" + m_sScheme + "\"");
         }
      }
   }

   /**
    * A connection used to configure and deploy resources.
    */
   protected static abstract class ResourceConnection
   {
      // operations

      /**
       * Gets a resource on this connection.
       *
       * @param resource The
       * @return A resource on this connection.
       */
      public Resource getResource(Resource resource)
      {
         return new Resource(this, resource.getPath());
      }

      /**
       * Gets a resource on this connection.
       *
       * @param baseResource
       * @param sRelativePath
       * @return A resource on this connection.
       */
      public Resource getResource(Resource baseResource, String sRelativePath)
      {
         return new Resource(this, baseResource, sRelativePath);
      }

      /**
       * Gets the resource and writes it to a local file.
       *
       * @param dst The local file to write.
       * @param src The resource to get.
       * @throws IOException If an I/O error occurs.
       */
      public abstract void get(File dst, Resource src) throws IOException;

      /**
       * Puts the local file to a resource.
       *
       * @param dst The resource to write.
       * @param src The local file to read.
       * @throws IOException If an I/O error occurs.
       */
      public abstract void put(Resource dst, File src) throws IOException;

      /**
       * Deletes a resource.
       *
       * @param resource The resource to delete.
       * @throws IOException If an I/O error occurs.
       */
      public abstract void delete(Resource resource) throws IOException;

      /**
       * Checks that the resource exists.
       *
       * @param resource The resource to check.
       * @return True if the resource exists; false otherwise.
       * @throws IOException If an I/O error occurs.
       */
      public abstract boolean exists(Resource resource) throws IOException;

      /**
       * Opens a resource for reading as an input stream.
       *
       * @param resource The resource to open for reading.
       * @return An input stream.
       * @throws IOException If an I/O error occurs.
       */
      public abstract InputStream getInputStream(Resource resource) throws IOException;

      /**
       * Opens a resource for writing as an output stream.
       *
       * @param sFile The resource to open for writing.
       * @return An output stream.
       * @throws IOException If an I/O error occurs.
       */
      public abstract OutputStream getOutputStream(Resource resource) throws IOException;

      /**
       * Cleans up the connection.
       *
       * @throws IOException If an I/O error occurs.
       */
      public abstract void close() throws IOException;
   }

   /**
    * A resource to configure or deploy.
    */
   protected static class Resource
   {
      // attributes

      /**
       * The resource path.
       */
      protected String m_sPath;

      // associations

      /**
       * The connection to use for performing operations on this resource; may be null.
       */
      protected ResourceConnection m_connection;

      // constructors

      /**
       * Creates a new, unconnected resource.
       *
       * @param sPath The path of the resource.
       */
      public Resource(String sPath)
      {
         m_sPath = makeResourcePath(sPath);
      }

      /**
       * Creates a new, unconnected resource.
       *
       * @param base The base resource.
       * @param sRelativePath The path of the resource, relative to the base.
       */
      public Resource(Resource base, String sRelativePath)
      {
         m_sPath = concat(base.m_sPath, makeResourcePath(sRelativePath));
      }

      /**
       * Creates a new, connected resource.
       *
       * @param connection The connection to use for resource operations.
       * @param sPath The path of the resource.
       */
      public Resource(ResourceConnection connection, String sPath)
      {
         m_connection = connection;
         m_sPath = makeResourcePath(sPath);
      }

      /**
       * Creates a new, connected resource.
       *
       * @param connection The connection to use for resource operations.
       * @param base The base resource.
       * @param sRelativePath The path of the resource, relative to the base.
       */
      public Resource(ResourceConnection connection, Resource base, String sRelativePath)
      {
         m_connection = connection;
         m_sPath = concat(base.m_sPath, makeResourcePath(sRelativePath));
      }

      // operations

      /**
       * Opens the resource for reading.
       *
       * @return An input stream to the resource.
       * @throws IOException If an I/O error occurs.
       */
      public InputStream getInputStream() throws IOException
      {
         checkConnected();

         return m_connection.getInputStream(this);
      }

      /**
       * Opens the resource for writing.
       *
       * @return An output stream to the resource.
       * @throws IOException If an I/O error occurs.
       */
      public OutputStream getOutputStream() throws IOException
      {
         checkConnected();

         return m_connection.getOutputStream(this);
      }

      /**
       * Deletes the resource.
       *
       * @throws IOException If an I/O error occurs.
       */
      public void delete() throws IOException
      {
         checkConnected();

         m_connection.delete(this);
      }

      /**
       * Checks that the resource exists.
       *
       * @return True if the resource exists; false otherwise.
       * @throws IOException If an I/O error occurs.
       */
      public boolean exists() throws IOException
      {
         checkConnected();

         return m_connection.exists(this);
      }

      /**
       * Gets the resource to the given local file.
       *
       * @param localFile A file on a local filesystem.
       * @throws IOException If an I/O error occurs.
       */
      public void get(File localFile) throws IOException
      {
         checkConnected();

         m_connection.get(localFile, this);
      }

      /**
       * Puts the given local file to the resource.
       *
       * @param localFile A file on a local filesystem to send.
       * @throws IOException If an I/O error occurs.
       */
      public void put(File localFile) throws IOException
      {
         checkConnected();

         m_connection.put(this, localFile);
      }

      /**
       * Gets the path to the resource.
       *
       * @return The path to the resource.
       */
      public String getPath()
      {
         return m_sPath;
      }

      /**
       * Checks that this resource is connected.
       * @throws IllegalStateException If not connected.
       */
      private void checkConnected()
      {
         if (m_connection == null)
         {
            throw new IllegalStateException("Not connected");
         }
      }

      /**
       * Converts a path using the system default file separator to
       * use forward slashes.
       *
       * @param sPath The path to convert.
       * @return Forward-slash-delimited path.
       */
      private static String makeResourcePath(String sPath)
      {
         if (!SysUtil.FILE_SEP.equals("/"))
         {
            return sPath.replaceAll(Pattern.quote(SysUtil.FILE_SEP), "/");
         }

         return sPath;
      }

      /**
       * Concatenates two path strings.
       *
       * @param sPath1 The root, forward-slash delimited path.
       * @param sPath2 The sub path, forward-slash delimited.
       * @return The concatenated path.
       */
      private static String concat(String sPath1, String sPath2)
      {
         if (sPath2.startsWith("/"))
         {
            sPath2 = sPath2.substring(1);
         }

         if (sPath1.endsWith("/"))
         {
            return sPath1 + sPath2;
         }
         else
         {
            return sPath1 + '/' + sPath2;
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_sPath;
      }
   }

   /**
    * A connection to a remote server through SSH/SFTP, using the JSch pure-Java SSH implementation
    * that comes with Eclipse.
    *
    * Properties:
    *    installer.cmd.jsch.knownHostsFile
    *       The list of trusted hosts.
    *       Path to an SSH known_hosts file, e.g. ~/.ssh/known_hosts. "<trust-all>" to trust all hosts.
    *    installer.cmd.jsch.identityFile
    *       Private key for password-less login.
    *       Path to an SSH identity file, e.g. ~/.ssh/id_rsa. Unspecified to use basic authentication.
    *       If specified, "password" attribute in .server is used as passphrase to the identity file.
    */
   protected static class JSchResourceConnection extends ResourceConnection
   {
      // constants

      /**
       * The property that has the path to the identity file.
       */
      public final static String IDENTITY_FILE_PROPERTY = "installer.cmd.jsch.identityFile";

      /**
       * The property that has the path to the known hosts file.
       */
      public final static String KNOWN_HOSTS_FILE_PROPERTY = "installer.cmd.jsch.knownHostsFile";

      // associations

      /**
       * The JSch control object.
       */
      protected JSch m_jsch;

      /**
       * The JSch session.
       */
      protected Session m_session;

      /**
       * The SFTP channel.
       */
      protected ChannelSftp m_channel;

      // constructors

      /**
       * Creates a connection using JSch to a remote host.
       *
       * @param sHost The host to connect.
       * @param nPort The port on which to connect, -1 for default port.
       * @param sUser The user id to connect as; null or "" to use identity file.
       * @param sPassword The password for the user id, may be null.
       * @param properties The installer properties.
       */
      public JSchResourceConnection(String sHost, int nPort, String sUser, String sPassword, Properties properties)
      {
         try
         {
            m_jsch = new JSch();

            String sHomeDir = System.getProperty("user.home");
            String sKnownHostsPath = properties.getProperty(KNOWN_HOSTS_FILE_PROPERTY,
               sHomeDir + SysUtil.FILE_SEP + ".ssh" + SysUtil.FILE_SEP + "known_hosts");

            if ("<trust-all>".equals(sKnownHostsPath))
            {
               // Trust ALL
               m_jsch.setHostKeyRepository(new HostKeyRepository()
               {
                  // operations

                  public int check(String host, byte[] key)
                  {
                     return HostKeyRepository.OK;
                  }

                  public void add(HostKey hostkey, UserInfo ui)
                  {
                     throw new UnsupportedOperationException();
                  }

                  public HostKey[] getHostKey()
                  {
                     throw new UnsupportedOperationException();
                  }

                  public HostKey[] getHostKey(String host, String type)
                  {
                     throw new UnsupportedOperationException();
                  }

                  public String getKnownHostsRepositoryID()
                  {
                     throw new UnsupportedOperationException();
                  }

                  public void remove(String host, String type)
                  {
                     throw new UnsupportedOperationException();
                  }

                  public void remove(String host, String type, byte[] key)
                  {
                     throw new UnsupportedOperationException();
                  }
               });
            }
            else if (sKnownHostsPath != null)
            {
               if (!(new File(sKnownHostsPath)).canRead())
               {
                  throw new IllegalStateException("Cannot read known hosts file: \"" + sKnownHostsPath + "\". Set \"" + KNOWN_HOSTS_FILE_PROPERTY + "\"");
               }

               m_jsch.setKnownHosts(sKnownHostsPath);
            }
            else
            {
               throw new IllegalStateException("Set \"" + KNOWN_HOSTS_FILE_PROPERTY + "\" for JSch deployment");
            }

            // If no identity file specified, try for default in user's home directory
            String sIdentityFile = properties.getProperty(IDENTITY_FILE_PROPERTY);
            boolean bLoadIdentityFile = false;

            if (sIdentityFile != null && !sIdentityFile.equals(""))
            {
               if (!(new File(sIdentityFile)).canRead())
               {
                  throw new IllegalStateException("Cannot read identity file: \"" + sIdentityFile + "\"");
               }

               bLoadIdentityFile = true;
            }
            else
            {
               sIdentityFile = sHomeDir + SysUtil.FILE_SEP + ".ssh" + SysUtil.FILE_SEP + "id_rsa";
               bLoadIdentityFile = (new File(sIdentityFile)).canRead();
            }

            // Uses identity file for authentication. If this fails, then falls back to basic authentication.
            if (bLoadIdentityFile)
            {
               if (sPassword == null)
               {
                  m_jsch.addIdentity(sIdentityFile);
               }
               else
               {
                  m_jsch.addIdentity(sIdentityFile, sPassword);
               }
            }

            m_session = m_jsch.getSession(sUser, sHost);
            m_session.setPassword(sPassword);

            if (nPort > 0)
            {
               m_session.setPort(nPort);
            }

            m_session.connect();
            m_channel = (ChannelSftp)m_session.openChannel("sftp");
            m_channel.connect();
         }
         catch (JSchException e)
         {
            s_logger.error("SFTP Error", e);
            ObjUtil.rethrow(e);
         }
      }

      // operations

      /**
       * @see JBossInstaller.ResourceConnection#close()
       */
      public void close() throws IOException
      {
         if (m_channel != null)
         {
            m_channel.disconnect();
            m_channel = null;
         }

         if (m_session != null)
         {
            m_session.disconnect();
            m_session = null;
         }

         m_jsch = null;
      }

      /**
       * @see JBossInstaller.ResourceConnection#delete(JBossInstaller.Resource)
       */
      public void delete(Resource resource) throws IOException
      {
         try
         {
            m_channel.rm(resource.getPath());
         }
         catch (SftpException e)
         {
            s_logger.error("SFTP Error", e);
            ObjUtil.rethrow(e);
         }
      }

      /**
       * @see JBossInstaller.ResourceConnection#exists(JBossInstaller.Resource)
       */
      public boolean exists(Resource resource) throws IOException
      {
         try
         {
            SftpATTRS attrs = m_channel.lstat(resource.getPath());

            return (attrs != null && !attrs.isDir());
         }
         catch (SftpException e)
         {
            // No independent "exists" check, so resort to catching error
            if (e.id == ChannelSftp.SSH_FX_NO_SUCH_FILE)
            {
               return false;
            }

            s_logger.error("SFTP Error", e);
            throw ObjUtil.rethrow(e);
         }
      }

      /**
       * @see JBossInstaller.ResourceConnection#get(java.io.File, JBossInstaller.Resource)
       */
      public void get(File dst, Resource src) throws IOException
      {
         try
         {
            m_channel.get(src.getPath(), dst.getAbsolutePath());
         }
         catch (SftpException e)
         {
            s_logger.error("SFTP Error", e);
            ObjUtil.rethrow(e);
         }
      }

      /**
       * @see JBossInstaller.ResourceConnection#getInputStream(JBossInstaller.Resource)
       */
      public InputStream getInputStream(Resource resource) throws IOException
      {
         File tempFile = File.createTempFile(SysUtil.NAMESPACE, null);

         if (!exists(resource))
         {
            return null;
         }

         get(tempFile, resource);

         return new DeleteFileInputStream(tempFile);
      }

      /**
       * @see JBossInstaller.ResourceConnection#getOutputStream(JBossInstaller.Resource)
       */
      public OutputStream getOutputStream(Resource resource) throws IOException
      {
         File tempFile = File.createTempFile(SysUtil.NAMESPACE, null);

         return new PutFileOutputStream(tempFile, resource);
      }

      /**
       * @see JBossInstaller.ResourceConnection#put(JBossInstaller.Resource, java.io.File)
       */
      public void put(Resource dst, File src) throws IOException
      {
         try
         {
            m_channel.put(src.getAbsolutePath(), dst.getPath());
         }
         catch (SftpException e)
         {
            s_logger.error("SFTP Error", e);
            ObjUtil.rethrow(e);
         }
      }
   }

   /**
    * A connection to a remote filesystem. Executes user-defined commands to access the
    * remote filesystem.
    */
   protected static class RemoteResourceConnection extends ResourceConnection
   {
      // associations

      /**
       * Properties from which the substitutions will be read.
       */
      protected Properties m_properties;

      /**
       * The GET command specification.
       */
      protected CommandSpec m_getCommand;

      /**
       * The PUT command specification.
       */
      protected CommandSpec m_putCommand;

      /**
       * The DEL command specification.
       */
      protected CommandSpec m_delCommand;

      /**
       * The input stream saved by an exists check operation for use
       * by getInputStream(). Saving it prevents two RPCs.
       */
      protected InputStream m_savedInputStream;

      // constructors

      /**
       * Creates a connection to a remote filesystem.
       *
       * @param sHost The host to connect.
       * @param nPort The port on which to connect.
       * @param sUser The user id to connect as.
       * @param sPassword The password for the user id.
       * @param properties The installer properties.
       */
      public RemoteResourceConnection(String sHost, int nPort, String sUser, String sPassword, Properties properties)
      {
         m_properties = new Properties(properties);
         m_properties.setProperty("user", sUser);
         m_properties.setProperty("password", sPassword);
         m_properties.setProperty("host", sHost);
         m_properties.setProperty("port", Integer.toString(nPort));

         m_getCommand = new CommandSpec(properties.getProperty("installer.cmd.ssh.get"));
         m_putCommand = new CommandSpec(properties.getProperty("installer.cmd.ssh.put"));
         m_delCommand = new CommandSpec(properties.getProperty("installer.cmd.ssh.del"));
      }

      // operations

      /**
       * @see JBossInstaller.ResourceConnection#close()
       */
      public void close() throws IOException
      {
      }

      /**
       * Runs the command in the command specification, giving responses to
       * interactive prompts based on the triggers in the specification.
       *
       * @param command The command specification to execute.
       * @param properties The properties to use for substitution values.
       * @throws IOException If an I/O error occurs.
       */
      protected void runCommand(CommandSpec command, Properties properties) throws IOException
      {
         ProcessHelper helper = new ProcessHelper(command.getCommand(properties));

         try
         {
            String sChunk;

            while ((sChunk = helper.readNextChunk()) != null)
            {
               String sResponse = command.trigger(sChunk, properties);

               if (sResponse != null)
               {
                  helper.writeLine(sResponse);
               }
            }
         }
         catch (Exception ex)
         {
            throw new IllegalStateException(helper.getSessionLog(), ex);
         }

         if (helper.getReturnCode() != 0)
         {
            throw new IllegalStateException("Process failed with code = " +
               helper.getReturnCode() + SysUtil.LINE_SEP + helper.getSessionLog());
         }
      }

      /**
       * @see JBossInstaller.ResourceConnection#get(java.io.File, java.lang.String)
       */
      public void get(File dst, Resource src) throws IOException
      {
         if (m_savedInputStream != null)
         {
            m_savedInputStream.close();
            m_savedInputStream = null;
         }

         Properties properties = new Properties(m_properties);

         properties.setProperty("local", dst.getAbsolutePath());
         properties.setProperty("remote", src.getPath());

         runCommand(m_getCommand, properties);
      }

      /**
       * @see JBossInstaller.ResourceConnection#get(java.lang.String, java.io.File)
       */
      public void put(Resource dst, File src) throws IOException
      {
         if (m_savedInputStream != null)
         {
            m_savedInputStream.close();
            m_savedInputStream = null;
         }

         Properties properties = new Properties(m_properties);

         properties.setProperty("local", src.getAbsolutePath());
         properties.setProperty("remote", dst.getPath());

         runCommand(m_putCommand, properties);
      }

      /**
       * @see JBossInstaller.ResourceConnection#delete(java.lang.String)
       */
      public void delete(Resource resource) throws IOException
      {
         if (m_savedInputStream != null)
         {
            m_savedInputStream.close();
            m_savedInputStream = null;
         }

         Properties properties = new Properties(m_properties);

         properties.setProperty("remote", resource.getPath());

         runCommand(m_delCommand, properties);
      }

      /**
       * @see JBossInstaller.ResourceConnection#exists(JBossInstaller.Resource)
       */
      public boolean exists(Resource resource) throws IOException
      {
         if (m_savedInputStream != null)
         {
            return true;
         }

         InputStream istream = getInputStream(resource);

         if (istream == null)
         {
            return false;
         }
         else
         {
            m_savedInputStream = istream;

            return true;
         }
      }

      /**
       * @see JBossInstaller.ResourceConnection#getInputStream(java.lang.String)
       */
      public InputStream getInputStream(Resource resource) throws IOException
      {
         if (m_savedInputStream != null)
         {
            InputStream temp = m_savedInputStream;

            m_savedInputStream = null;

            return temp;
         }

         File tempFile = File.createTempFile(SysUtil.NAMESPACE, null);

         try
         {
            get(tempFile, resource);
         }
         catch (Exception ex)
         {
            return null;
         }

         return new DeleteFileInputStream(tempFile);
      }

      /**
       * @see JBossInstaller.ResourceConnection#getOutputStream(java.lang.String)
       */
      public OutputStream getOutputStream(Resource resource) throws IOException
      {
         if (m_savedInputStream != null)
         {
            m_savedInputStream.close();
            m_savedInputStream = null;
         }

         File tempFile = File.createTempFile(SysUtil.NAMESPACE, null);

         return new PutFileOutputStream(tempFile, resource);
      }

      // inner classes

      /**
       * A Command Specification that allows this Deployer to use user-defined commands
       * for deployment.
       *
       * Commands are specified in the following format:
       * <command> <arg1> <arg2> ... <argN>;<trigger_pattern1>,<trigger_result1>,<trigger_pattern2>,<trigger_result2>
       *
       * Command and arguments are separated by spaces. Double quotes can be used
       * to quote spaces. Backslash is used to escape double quotes and itself.
       */
      protected static class CommandSpec
      {
         // associations

         /**
          * The command array. The first location is the full path to the executable; the
          * subsequent locations are the arguments to the command.
          */
         protected String[] m_sCommandArray;

         /**
          * The trigger array.
          */
         protected Pattern[] m_triggerArray;

         /**
          * The response array. Must be same length as trigger array.
          */
         protected String[] m_sResponseArray;

         // constructors

         /**
          * Creates a new command specification from the given specification string.
          *
          * @param sSpec The specification string.
          */
         public CommandSpec(String sSpec)
         {
            ArrayList commandList = new ArrayList();
            ArrayList triggerList = new ArrayList();
            List currentList = commandList;
            int i = 0;
            StringBuilder buf = new StringBuilder();
            boolean bQuoted = false;
            boolean bLastCharEscape = false;

            while (i < sSpec.length())
            {
               char ch = sSpec.charAt(i);

               if (ch == ' ' && !bQuoted && !bLastCharEscape)
               {
                  currentList.add(buf.toString());
                  buf.setLength(0);
               }
               else if (ch == ';' && !bQuoted && !bLastCharEscape)
               {
                  currentList.add(buf.toString());
                  currentList = triggerList;
                  buf.setLength(0);
               }
               else if (ch == '"')
               {
                  if (bLastCharEscape)
                  {
                     buf.append('"');
                     bLastCharEscape = false;
                  }
                  else
                  {
                     bQuoted = !bQuoted;
                  }
               }
               else if (ch == '\\')
               {
                  if (bLastCharEscape)
                  {
                     buf.append('\\');
                     bLastCharEscape = false;
                  }
                  else
                  {
                     bLastCharEscape = true;
                  }
               }
               else
               {
                  if (bLastCharEscape)
                  {
                     buf.append('\\');
                     bLastCharEscape = false;
                  }

                  buf.append(ch);
               }

               i++;
            }

            currentList.add(buf.toString());

            m_sCommandArray = (String[])commandList.toArray(new String[commandList.size()]);

            if ((triggerList.size() & 0x01) == 1)
            {
               throw new IllegalStateException("Invalid trigger-response specification.");
            }

            m_triggerArray = new Pattern[triggerList.size() >> 1];
            m_sResponseArray = new String[triggerList.size() >> 1];

            for (i = 0; i < triggerList.size(); i += 2)
            {
               m_triggerArray[i >> 1] = Pattern.compile((String)triggerList.get(i));
               m_sResponseArray[i >> 1] = (String)triggerList.get(i + 1);
            }
         }

         // operations

         /**
          * Replaces named-substitution parameters (of the form ${name}) with
          * their values from the given property hash.
          *
          * @param sData The string on which to perform substitution.
          * @param properties The substitution name-value map.
          * @return The input string with values from properties substituted.
          */
         protected static String replaceVariables(String sData, final Properties properties)
         {
            StringWriter writer = new StringWriter();
            Reader reader = new SubstReader(new StringReader(sData))
            {
               protected String getValue(String sName) throws IOException
               {
                  if (sName.startsWith("cygwin:"))
                  {
                     String sCygwinPath = properties.getProperty(sName.substring("cygwin:".length()), "");

                     if (sCygwinPath.startsWith(":\\", 1))
                     {
                        sCygwinPath = sCygwinPath.replaceAll("\\\\", "/");
                        sCygwinPath = "/cygdrive/" + sCygwinPath.charAt(0) + sCygwinPath.substring(2);

                        return sCygwinPath;
                     }
                     else
                     {
                        return "";
                     }
                  }
                  else
                  {
                     return properties.getProperty(sName, "");
                  }
               }
            };

            try
            {
               IOUtil.copy(writer, reader);
            }
            catch (IOException ex)
            {
               ObjUtil.rethrow(ex);
            }

            return writer.toString();
         }

         /**
          * Finds the first trigger that matches within the given data chunk
          * and returns the trigger's response string.
          *
          * @param sData The data chunk on which triggers should be matched.
          * @param properties The substitution name-value map.
          * @return The response for the first matched trigger; null if no
          * trigger was matched.
          */
         public String trigger(String sData, Properties properties)
         {
            for (int i = 0; i < m_triggerArray.length; i++)
            {
               Pattern trigger = m_triggerArray[i];
               Matcher matcher = trigger.matcher(sData);

               if (matcher.find())
               {
                  return replaceVariables(m_sResponseArray[i], properties);
               }
            }

            return null;
         }

         /**
          * Gets the command array using the substitution values provided in
          * the argument.
          *
          * @param properties The substitution name-value map.
          * @return The command array (first element is path to the command's
          * executable, subsequent elements are arguments to the command).
          */
         public String[] getCommand(Properties properties)
         {
            String[] sOptionsArray = new String[m_sCommandArray.length];

            for (int i = 0; i < sOptionsArray.length; i++)
            {
               sOptionsArray[i] = replaceVariables(m_sCommandArray[i], properties);
            }

            return sOptionsArray;
         }
      }
   }

   /**
    * A connection to the local filesystem.
    */
   protected static class LocalResourceConnection extends ResourceConnection
   {
      // operations

      /**
       * @see JBossInstaller.ResourceConnection#get(java.io.File, java.lang.String)
       */
      public void get(File dst, Resource src) throws IOException
      {
         IOUtil.copy(dst, getFile(src.getPath()), null);
      }

      /**
       * @see JBossInstaller.ResourceConnection#get(java.lang.String, java.io.File)
       */
      public void put(Resource dst, File src) throws IOException
      {
         IOUtil.copy(getFile(dst.getPath()), src, null);
      }

      /**
       * @see JBossInstaller.ResourceConnection#delete(java.lang.String)
       */
      public void delete(Resource resource) throws IOException
      {
         IOUtil.delete(getFile(resource.getPath()));
      }

      /**
       * @see JBossInstaller.ResourceConnection#exists(JBossInstaller.Resource)
       */
      public boolean exists(Resource resource) throws IOException
      {
         return getFile(resource.getPath()).exists();
      }

      /**
       * @see JBossInstaller.ResourceConnection#getInputStream(java.lang.String)
       */
      public InputStream getInputStream(Resource resource) throws IOException
      {
         File file = getFile(resource.getPath());

         if (file.exists())
         {
            return new FileInputStream(file);
         }
         else
         {
            return null;
         }
      }

      /**
       * @see JBossInstaller.ResourceConnection#getOutputStream(java.lang.String)
       */
      public OutputStream getOutputStream(Resource resource) throws IOException
      {
         return new FileOutputStream(getFile(resource.getPath()));
      }

      /**
       * @see JBossInstaller.ResourceConnection#close()
       */
      public void close() throws IOException
      {
      }

      /**
       * Converts an abstract deployment location to a File on the local filesystem.
       *
       * @param sFile The abstract deployment location.
       * @return A File on the local filesystem corresponding to the given parameter.
       */
      protected static File getFile(String sFile)
      {
         if (!SysUtil.FILE_SEP.equals("/"))
         {
            sFile = sFile.replaceAll("/", Matcher.quoteReplacement(SysUtil.FILE_SEP));
         }

         return new File(sFile);
      }
   }

   /**
    * Helper class for running a subprocess.
    */
   protected static class ProcessHelper
   {
      // constants

      /**
       * The timeout. If the subprocess does not produce output or terminate within
       * the timeout, it is presumed to have hung. The timeout counter is reset every time
       * new output is retrieved.
       */
      public final static long PROCESS_TIMEOUT = 60 * 1000;

      // attributes

      /**
       * The subprocess return code.
       */
      protected int m_nReturnCode;

      // associations

      /**
       * The subprocess being run.
       */
      protected Process m_process;

      /**
       * Holds the output of the subprocess.
       */
      protected StringBuffer m_sessionBuffer;

      /**
       * Output stream to the subprocess.
       */
      protected OutputStream m_ostream;

      /**
       * Input stream from the subprocess.
       */
      protected InputStream m_istream;

      /**
       * Reader for reading from the subprocess.
       */
      protected Reader m_reader;

      /**
       * Writer for writing to the subprocess.
       */
      protected Writer m_writer;

      // constructors

      /**
       * Creates and starts a new subprocess.
       *
       * @param sCommandArray The command to execute and its arguments.
       * @throws IOException If an I/O error occurs.
       */
      public ProcessHelper(String[] sCommandArray) throws IOException
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Executing command: " + Arrays.toString(sCommandArray));
         }

         ProcessBuilder pb = new ProcessBuilder(sCommandArray);

         pb.redirectErrorStream(true);
         m_process = pb.start();

         m_istream = m_process.getInputStream();
         m_ostream = new BufferedOutputStream(m_process.getOutputStream());
         m_reader = new InputStreamReader(m_istream);
         m_writer = new OutputStreamWriter(m_ostream);
         m_sessionBuffer = new StringBuffer();
      }

      // operations

      /**
       * Reads the next chunk of output from the subprocess.
       *
       * @return A chunk of output from the subprocess; null if the subprocess has terminated.
       * @throws IOException If an I/O error occurs.
       */
      public String readNextChunk() throws IOException
      {
         int nErrCode = 0;

         try
         {
            long lStartTime = System.currentTimeMillis();
            boolean bDone = false;

            while (!bDone)
            {
               try
               {
                  nErrCode = m_process.exitValue();
                  bDone = true;
               }
               catch (IllegalThreadStateException ex)
               {
                  // Yield CPU to other threads
                  Thread.sleep(100);

                  int nPos = m_sessionBuffer.length();

                  while (m_reader.ready())
                  {
                     m_sessionBuffer.appendCodePoint(m_reader.read());
                  }

                  if (nPos < m_sessionBuffer.length())
                  {
                     return m_sessionBuffer.substring(nPos);
                  }
               }

               if (System.currentTimeMillis() - lStartTime > PROCESS_TIMEOUT)
               {
                  throw new IOException("Subprocess timeout");
               }
            }

            m_nReturnCode = nErrCode;
         }
         catch (InterruptedException ex)
         {
            ObjUtil.rethrow(ex);
         }

         while (m_reader.ready())
         {
            m_sessionBuffer.appendCodePoint(m_reader.read());
         }

         return null;
      }

      /**
       * Writes a string to the process and then writes a newline.
       *
       * @param sLine The string to write.
       */
      public void writeLine(String sLine)
      {
         try
         {
            m_writer.write(sLine);
            m_writer.write(SysUtil.LINE_SEP);
            m_writer.flush();
         }
         catch (IOException ex)
         {
            throw new IllegalStateException(m_sessionBuffer.toString(), ex);
         }
      }

      /**
       * Gets the return code of the subprocess. Valid only when the subprocess
       * has exited.
       *
       * @return The subprocess return code.
       */
      public int getReturnCode()
      {
         return m_nReturnCode;
      }

      /**
       * Gets the complete output of the subprocess.
       *
       * @return The complete output of the subprocess.
       */
      public String getSessionLog()
      {
         return m_sessionBuffer.toString();
      }
   }

   /**
    * An input stream that deletes the file when closed.
    */
   protected static class DeleteFileInputStream extends FilterInputStream
   {
      // associations

      /**
       * The file being read.
       */
      protected File m_file;

      // constructors

      /**
       * Creates a new instance that reads the given file and then deletes it.
       *
       * @param file The file to read and delete.
       * @throws IOException If an I/O error occurs.
       */
      public DeleteFileInputStream(File file) throws IOException
      {
         super(new FileInputStream(file));
         m_file = file;
      }

      // operations

      /**
       * @see java.io.FilterInputStream#close()
       */
      public void close() throws IOException
      {
         super.close();
         IOUtil.delete(m_file);
      }
   }

   /**
    * An output stream that transfers the file to a destination when closed and
    * then deletes the local copy of the file.
    */
   protected static class PutFileOutputStream extends FilterOutputStream
   {
      // associations

      /**
       * The file to write, transfer, and then delete.
       */
      protected File m_file;

      /**
       * The transfer destination.
       */
      protected Resource m_dst;

      // constructors

      /**
       * Creates a new instance that allows writing to the given file. When
       * closed, the file will be transferred to the destination and then
       * deleted.
       *
       * @param file The file to write, transfer, and then delete.
       * @param dst The destination resource.
       * @throws IOException If an I/O error occurs.
       */
      public PutFileOutputStream(File file, Resource dst) throws IOException
      {
         super(new FileOutputStream(file));
         m_file = file;
         m_dst = dst;
      }

      // operations

      /**
       * @see java.io.FilterOutputStream#close()
       */
      public void close() throws IOException
      {
         super.close();
         m_dst.put(m_file);
         IOUtil.delete(m_file);
      }
   }

   /**
    * Strategy to use for installing defferent JBoss versions
    */
   protected abstract class JBossInstallerStrategy
   {
      // constants

      protected final static String LOGIN_CONFIG_FILE_NAME = "login-config.xml";
      protected final static String PROPERTIES_SERVICE_FILE_NAME = "properties-service.xml";
      protected final static String DATASOURCE_FILE_NAME = SysUtil.NAMESPACE + "-ds.xml";
      protected final static String MAIL_SERVICE_FILE_NAME = "mail-service.xml";
      protected final static String JBOSS_SERVICE_FILE_NAME = "jboss-service.xml";
      protected final static String TRUST_STORE_FILE_NAME = "ssl.truststore";
      protected final String PERSISTENCE_SERVICE_TEMPLATE_HOME_DIR_NAME = ".." + SysUtil.FILE_SEP + ".." + SysUtil.FILE_SEP + "docs" + SysUtil.FILE_SEP + "examples" + SysUtil.FILE_SEP;
      protected final String PERSISTENCE_SERVICE_TEMPLATE_DIR_NAME = "jms" + SysUtil.FILE_SEP;
      protected final static String LOCAL_TX_JCA_SERVICE_NAME = "LocalTxCM";

      // associations

      /**
       * The JMS data source connection info.
       */
      protected JDBCConnectionInfo m_jmsDBConnectionInfo;

      // operations

      /**
       * @return log4j config file name.
       */
      public abstract String getLog4JFileName();

      /**
       * @return jbossweb (tomcat) config file name
       */
      public abstract String getServerFileName();

      /**
       * @return jbossweb (tomcat) web.xml file
       */
      public abstract String getWebFileName();

      /**
       * @return jbossweb (tomcat) deployer service config file name
       */
      public abstract String getTomcatFileName();

      /**
       * @return JMS connection factories config file name
       */
      public abstract String getJMSDSFileName();

      /**
       * @return HAJNDI JMS connection factories config file name
       */
      public abstract String getHAJNDIJMSDSFileName();

      /**
       * @return The name to use for the JMS application-policy
       */
      public abstract String getMQLoginPolicyName();

      /**
       * @return The suffix of the persistence service template file
       */
      public abstract String getPersistenceServiceTemplateFileSuffix();

      /**
       * @param connection The resource connection
       * @param location The server location
       * @return A resource for the file used to config JMS application-policy
       */
      public abstract Resource getMQLoginConfigResource(ResourceConnection connection, Location location);

      /**
       * @return The data source connection info to be used for JMS persistence.
       */
      public abstract JDBCConnectionInfo getJMSDBConnectionInfo() throws IOException;

      /**
       * Adds a new login DB based application-policy
       * @param policies The <policy> element of the login config file
       * @param jmsJDBCConnInfo The JDBC connection info
       */
      public abstract void addJMSDBApplicationPolicy(Element policies, JDBCConnectionInfo jmsJDBCConnInfo);

      /**
       * Adds and configures a data source
       * @param document The document object representing the parsed data source config file
       * @param jmsDBConnectionInfo The JMS data source connection info
       * @param sJNDIPrefix The prefix to use for the JNDI binding
       * @param fragment The DB fragment
       */
      public abstract void addJMSDataSource(Document document, JDBCConnectionInfo jmsDBConnectionInfo, String sJNDIPrefix);

      /**
       * Adds custom system properties for the JBoss runtime environment
       * @param propsAttribute The properties XMl element from the properties-service.xml file that contains property name value pairs
       * @throws IllegalArgumentException
       * @throws IOException
       */
      public abstract void addCustomProperties(Element propsAttribute) throws IllegalArgumentException, IOException;

      /**
       * Configures cluster settings
       * @param connection The resource connection
       * @param location The server location
       * @throws IOException
       */
      public abstract void installClusterService(ResourceConnection connection, Location location) throws IOException;

      /**
       * Installs and configures the persistence service for the JMS engine
       * @param connection The resource connection
       * @param location The server location
       * @param jmsDB The DB to use as the data source
       * @param sqlFile The file to output DDL commands to. Can be null to not do this.
       * @param templateFile The persistence service template file
       * @throws IOException
       */
      public abstract void installPersistenceService(ResourceConnection connection, Location location, Resource templateFile, Resource sqlFile, RelationalDatabase jmsDB) throws IOException;

      /**
       * Configures the jbossweb (tomcat) serice
       * @param file The jboss web config file
       */
      public abstract void installTomcat(Resource file) throws IOException;

      /**
       * Installs an configures the nexj JMS destinations
       * @param connection The resource connection
       * @param location The server location
       * @param nNodeIdx The server cluster node index
       * @throws IOException
       */
      public abstract void installMQService(ResourceConnection connection, Location location, int nNodeIdx) throws IOException;

      /**
       * Fixes an SQL statement for a given database
       * @param sSQL The SQL statement
       * @param db The relational database for which the SQL is to be formatted
       * @return The resulting SQL string
       */
      public abstract String formatSQL(String sSQL, RelationalDatabase db);

      /**
       * Uninstalls the nexj JMS destinations
       * @param connection The resource connection
       * @param location The server location
       * @throws IOException
       */
      public abstract void uninstallMQService(ResourceConnection connection, Location location) throws IOException;

      /**
       * Installs jboss application server resources
       * @param connection The resource connection
       * @param location The server location
       */
      public void install(ResourceConnection connection, Location location, int nNodeIdx) throws Exception
      {
         RelationalDatabase defaultDatabase = getDefaultDatabase();

         installTrustStore(connection.getResource(location.getConfigDir(), TRUST_STORE_FILE_NAME));

         installPropertiesService(connection.getResource(location.getDeploymentDir(), PROPERTIES_SERVICE_FILE_NAME));
         installDataSource(connection.getResource(location.getDeploymentDir(), DATASOURCE_FILE_NAME));
         installMailService(connection.getResource(location.getDeploymentDir(), MAIL_SERVICE_FILE_NAME));
         installClusterService(connection, location);
         installServer(connection.getResource(location.getDeploymentDir(), getServerFileName()), nNodeIdx + 1);
         installWeb(connection.getResource(location.getDeploymentDir(), getWebFileName()));
         installTomcat(connection.getResource(location.getDeploymentDir(), getTomcatFileName()));
         installPersistenceService(connection, location, getJMSDBConnectionInfo());

         installJMSConnectionFactories(connection.getResource(location.getDeploymentDir(),
            (m_metadata.isDistributed()) ? getHAJNDIJMSDSFileName() : getJMSDSFileName()));

         installMQService(connection, location, nNodeIdx);
         installLog4J(connection.getResource(location.getConfigDir(), getLog4JFileName()));
         installLoginConfig(connection.getResource(location.getConfigDir(), LOGIN_CONFIG_FILE_NAME), defaultDatabase);
         installMQLoginConfig(getMQLoginConfigResource(connection, location), getMQLoginPolicyName(), defaultDatabase);
         installJBossService(connection.getResource(location.getConfigDir(), JBOSS_SERVICE_FILE_NAME), TRUST_STORE_FILE_NAME);
      }

      /**
       * Uninstalls jboss application server resources
       * @param connection The resource connection
       * @param location The server location
       */
      public void uninstall(ResourceConnection connection, Location location, int nNodeIdx) throws Exception
      {
         connection.getResource(location.getDeploymentDir(), DATASOURCE_FILE_NAME).delete();
         uninstallMailService(connection.getResource(location.getDeploymentDir(), MAIL_SERVICE_FILE_NAME));
         uninstallMQService(connection, location);
         uninstallJMSConnectionFactories(connection.getResource(location.getDeploymentDir(),
            (m_metadata.isDistributed()) ? getHAJNDIJMSDSFileName() : getJMSDSFileName()));
         uninstallLog4J(connection.getResource(location.getConfigDir(), getLog4JFileName()));
         uninstallLoginConfig(connection.getResource(location.getConfigDir(), LOGIN_CONFIG_FILE_NAME));
         uninstallPropertiesService(connection.getResource(location.getDeploymentDir(), PROPERTIES_SERVICE_FILE_NAME));
         uninstallJBossService(connection.getResource(location.getConfigDir(), JBOSS_SERVICE_FILE_NAME));
      }

      /**
       * Installs and configures the persistence service for the JMS engine
       * @param connection The resource connection
       * @param location The server location
       * @param jmsDBConnectionInfo The JMS data source connection info
       * @throws IOException
       */
      public void installPersistenceService(ResourceConnection connection, Location location, JDBCConnectionInfo jmsDBConnectionInfo) throws IOException
      {
         RelationalDatabase jmsDB = (jmsDBConnectionInfo != null) ? jmsDBConnectionInfo.getBackingDB() : getDefaultDatabase();

         if (jmsDB != null)
         {
            String sPersistenceFilenamePrefix = JDBCInfo.getInstance(jmsDB.getDriver(), jmsDB.getAdapter().getName()).getDBTypeName().toLowerCase(Locale.ENGLISH);
            String sTemplateFileName = PERSISTENCE_SERVICE_TEMPLATE_DIR_NAME + sPersistenceFilenamePrefix + getPersistenceServiceTemplateFileSuffix();
            Resource templateFile =  connection.getResource(location.getBaseDir(), PERSISTENCE_SERVICE_TEMPLATE_HOME_DIR_NAME + sTemplateFileName);

            if (!templateFile.exists())
            {
               templateFile =  connection.getResource(location.getBaseDir(), sTemplateFileName);

               if (!templateFile.exists())
               {
                  templateFile = null;
               }
            }

            String sSQLFileName = m_properties.getProperty("jms.sql.file");
            Resource sqlFile = (sSQLFileName == null) ? null : new Resource(new LocalResourceConnection(), sSQLFileName);

            installPersistenceService(connection, location, templateFile, sqlFile, jmsDB);
         }
      }
   }

   /**
    * JBoss 4 Installer Strategy
    */
   protected class JBoss4InstallerStrategy extends JBossInstallerStrategy
   {
      protected final String CLUSTER_SERVICE_FILE_NAME = "cluster-service.xml";
      protected final String JDBC_SERVICE_FILE_NAME = "jms" + SysUtil.FILE_SEP + SysUtil.NAMESPACE + "-jdbc2-service.xml";
      protected final String JDBC_SERVICE_OLD_FILE_NAME = "jms" + SysUtil.FILE_SEP + "hsqldb-jdbc2-service.xml";
      protected final String JDBC_STATE_SERVICE_FILE_NAME = "jms" + SysUtil.FILE_SEP + SysUtil.NAMESPACE + "-jdbc-state-service.xml";
      protected final String JDBC_STATE_SERVICE_OLD_FILE_NAME = "jms" + SysUtil.FILE_SEP + "hsqldb-jdbc-state-service.xml";
      protected final String MQ_SERVICE_FILE_NAME = "jms" + SysUtil.FILE_SEP + SysUtil.NAMESPACE + "-mq-service.xml";

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getLog4JFileName()
       */
      public String getLog4JFileName()
      {
         return "log4j.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getServerFileName()
       */
      public String getServerFileName()
      {
         return "jbossweb-tomcat55.sar" + SysUtil.FILE_SEP + "server.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getWebFileName()
       */
      public String getWebFileName()
      {
         return "jbossweb-tomcat55.sar" + SysUtil.FILE_SEP + "conf" + SysUtil.FILE_SEP + "web.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getTomcatFileName()
       */
      public String getTomcatFileName()
      {
         return "jbossweb-tomcat55.sar" + SysUtil.FILE_SEP + "META-INF" + SysUtil.FILE_SEP + "jboss-service.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getJMSDSFileName()
       */
      public String getJMSDSFileName()
      {
         return "jms" + SysUtil.FILE_SEP + "jms-ds.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getHAJNDIJMSDSFileName()
       */
      public String getHAJNDIJMSDSFileName()
      {
         return "jms" + SysUtil.FILE_SEP + "hajndi-jms-ds.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getMQLoginPolicyName()
       */
      public String getMQLoginPolicyName()
      {
         return "jbossmq";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getPersistenceServiceTemplateFileSuffix()
       */
      public String getPersistenceServiceTemplateFileSuffix()
      {
         return "-jdbc2-service.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getMQLoginConfigResource(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location)
       */
      public Resource getMQLoginConfigResource(ResourceConnection connection, Location location)
      {
         return connection.getResource(location.getConfigDir(), LOGIN_CONFIG_FILE_NAME);
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getJMSDBConnectionInfo()
       */
      public JDBCConnectionInfo getJMSDBConnectionInfo() throws IOException
      {
         if (m_jmsDBConnectionInfo == null)
         {
            m_jmsDBConnectionInfo = JBossInstaller.this.getJMSDBConnectionInfo();
         }

         return m_jmsDBConnectionInfo;
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#addJMSDBApplicationPolicy(org.w3c.dom.Element, java.lang.String, RelationalDatabase)
       */
      public void addJMSDBApplicationPolicy(Element policies, JDBCConnectionInfo jmsJDBCConnInfo)
      {
         if (jmsJDBCConnInfo != null)
         {
            RelationalDatabase db = jmsJDBCConnInfo.getBackingDB();
            RelationalDatabaseFragment dbFragment = (RelationalDatabaseFragment)db.getDefaultFragment();

            addApplicationPolicy(policies, getJAASDomain(dbFragment), LOCAL_TX_JCA_SERVICE_NAME,
               SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jms/jdbc/" + dbFragment.getAlias(), dbFragment.getUser(), dbFragment.getPassword());
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#addJMSDataSource(org.w3c.dom.Document, nexj.core.meta.persistence.sql.RelationalDatabase, nexj.core.meta.persistence.sql.RelationalDatabaseFragment, java.lang.String)
       */
      public void addJMSDataSource(Document document, JDBCConnectionInfo jmsJDBCConnInfo, String sJNDIPrefix)
      {
         if (jmsJDBCConnInfo != null)
         {
            addDataSource(document, jmsJDBCConnInfo.getBackingDB(),
               (RelationalDatabaseFragment)jmsJDBCConnInfo.getBackingDB().getDefaultFragment(), jmsJDBCConnInfo, sJNDIPrefix);
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#addCustomProperties(org.w3c.dom.Element)
       */
      public void addCustomProperties(Element propsAttribute) throws IllegalArgumentException, IOException
      {
         Properties properties = PropertyUtil.fromString(XMLUtil.getElementValue(propsAttribute));

         properties.setProperty("org.apache.catalina.STRICT_SERVLET_COMPLIANCE", "true");
         XMLUtil.setElementValue(propsAttribute, PropertyUtil.toString(properties));
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installClusterService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location)
       */
      public void installClusterService(ResourceConnection connection, Location location) throws IOException
      {
         if (m_metadata.isDistributed())
         {
            Resource file = connection.getResource(location.getDeploymentDir(), CLUSTER_SERVICE_FILE_NAME);
            Document doc = parse(file, null);
            String sOriginalContents = format(doc);
            Element server = doc.getDocumentElement();
            Element mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss:service=${jboss.partition.name:DefaultPartition}");

            if (mbean != null)
            {
               Element timeout = XMLUtil.findChildElement(mbean, "attribute", "name", "StateTransferTimeout");

               if (timeout != null)
               {
                  long lTimeout = 0;

                  try
                  {
                     lTimeout = Long.parseLong(XMLUtil.getElementValue(timeout));
                  }
                  catch (Exception e)
                  {
                  }

                  if (lTimeout < 300000)
                  {
                     XMLUtil.setElementValue(timeout, "300000");
                  }
               }

               Element config  = XMLUtil.findChildElement(mbean, "attribute", "name", "PartitionConfig");

               if (config != null)
               {
                  config = XMLUtil.findChildElement(config, "Config");
               }

               if (config != null)
               {
                  Element stable = XMLUtil.findChildElement(config, "pbcast.STABLE");

                  if (stable != null)
                  {
                     long lGossip = Long.MAX_VALUE;

                     try
                     {
                        lGossip = XMLUtil.getLongAttr(stable, "desired_avg_gossip", Long.MAX_VALUE);
                     }
                     catch (Exception e)
                     {
                     }

                     if (lGossip > 256)
                     {
                        stable.setAttribute("desired_avg_gossip", "256");
                     }

                     long lMaxBytes = 0;

                     try
                     {
                        lMaxBytes = XMLUtil.getLongAttr(stable, "max_bytes", 0);
                     }
                     catch (Exception e)
                     {
                     }

                     if (lMaxBytes < 524288)
                     {
                        stable.setAttribute("max_bytes", "524288");
                     }
                  }
               }
            }

            format(file, doc, sOriginalContents);
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installPersistenceService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location, nexj.core.admin.platform.jboss.JBossInstaller.Resource, nexj.core.admin.platform.jboss.JBossInstaller.Resource, nexj.core.meta.persistence.sql.RelationalDatabase)
       */
      public void installPersistenceService(ResourceConnection connection, Location location, Resource templateFile, Resource sqlFile, RelationalDatabase jmsDB) throws IOException
      {
         String sJNDIName = SysUtil.NAMESPACE + '/' + m_sEnvironmentName + ((jmsDB != getDefaultDatabase()) ? "/jms" : "") + "/jdbc/" +
            ((RelationalDatabaseFragment)jmsDB.getDefaultFragment()).getAlias();

         List sqlList = (sqlFile == null) ? null : new ArrayList();

         installJDBCService(connection.getResource(location.getSingletonDir(), JDBC_SERVICE_FILE_NAME),
            connection.getResource(location.getSingletonDir(), JDBC_SERVICE_OLD_FILE_NAME),
            templateFile, jmsDB, sJNDIName, sqlList);

         installJDBCStateService(connection.getResource(location.getSingletonDir(), JDBC_STATE_SERVICE_FILE_NAME),
            connection.getResource(location.getSingletonDir(), JDBC_STATE_SERVICE_OLD_FILE_NAME),
            jmsDB, sJNDIName, sqlList);

         if (sqlFile != null)
         {
            JBossInstaller.this.formatSQL(sqlFile, sqlList, jmsDB);
         }
      }

      /**
       * Installs nexj-jdbc2-service.xml.
       * @param file The file.
       * @param oldFile The old file, which will be renamed to the new one.
       * @param templateFile the persistence service template file
       * @param jmsDB The default database.
       * @param sJNDIName The jndi name of the data source
       * @param sqlList The output list for SQL statements. Can be null.
       */
      protected void installJDBCService(Resource file, Resource oldFile, Resource templateFile, RelationalDatabase jmsDB, String sJNDIName, List sqlList) throws IOException
      {
         Document doc;
         Document templateDoc = null;

         if (templateFile == null)
         {
            doc = parse(file, oldFile, true);
         }
         else
         {
            doc = parse(file, templateFile, false);
            templateDoc = parse(templateFile, null);

            if (oldFile.exists())
            {
               oldFile.delete();
            }
         }

         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element messageCache = XMLUtil.findChildElementByName(server, "mbean", "jboss.mq:service=MessageCache");

         if (messageCache != null)
         {
            Element highMark = XMLUtil.findChildElementByName(messageCache, "attribute", "HighMemoryMark");

            if (highMark != null)
            {
               XMLUtil.setElementValue(highMark, "200");
            }

            Element maxMark = XMLUtil.findChildElementByName(messageCache, "attribute", "MaxMemoryMark");

            if (maxMark != null)
            {
               XMLUtil.setElementValue(maxMark, "512");
            }
         }

         Element persistenceManager = XMLUtil.findChildElementByName(server, "mbean", "jboss.mq:service=PersistenceManager");

         if (persistenceManager != null)
         {
            Element connectionManager = XMLUtil.findChildElement(persistenceManager,
               "depends", "optional-attribute-name", "ConnectionManager");

            if (connectionManager != null)
            {
               XMLUtil.setElementValue(connectionManager, "jboss.jca:service=DataSourceBinding,name=" + sJNDIName);
            }

            Element sqlPropertiesTo = XMLUtil.findChildElementByName(persistenceManager, "attribute", "SqlProperties");
            Element sqlPropertiesFrom = sqlPropertiesTo;

            if (templateDoc != null)
            {
               Element templateServer = templateDoc.getDocumentElement();

               if (templateServer != null)
               {
                  Element templatePersistenceManager = XMLUtil.findChildElementByName(templateServer, "mbean", "jboss.mq:service=PersistenceManager");

                  if (templatePersistenceManager != null)
                  {
                     sqlPropertiesFrom = XMLUtil.findChildElementByName(templatePersistenceManager, "attribute", "SqlProperties");

                     if (sqlPropertiesTo == null)
                     {
                        XMLUtil.setChildElement(persistenceManager, null, "attribute", "name", "SqlProperties", null, true);
                     }
                  }
               }
            }

            if (sqlPropertiesTo != null)
            {
               String sValue = XMLUtil.getElementValue(sqlPropertiesFrom);

               if (sValue != null)
               {
                  Properties properties = PropertyUtil.fromString(sValue);
                  byte nJMSDBType = JDBCInfo.getInstance(jmsDB.getDriver(), jmsDB.getAdapter().getName()).getDBType();

                  if (templateFile == null)
                  {
                     properties.setProperty("BLOB_TYPE", "BINARYSTREAM_BLOB");
                  }

                  for (Iterator itr = properties.entrySet().iterator(); itr.hasNext();)
                  {
                     Map.Entry entry = (Map.Entry)itr.next();

                     if (entry.getValue() != null)
                     {
                        String sSQL = fixSQL((String)entry.getValue(), jmsDB);

                        if (sSQL.matches("(?i)^\\s*select\\s+max\\s*\\(.*?\\s+from\\s+.*?\\)\\s*$"))
                        {
                           sSQL += " A";
                        }

                        if (nJMSDBType == JDBCInfo.DB_TYPE_DB2)
                        {
                           if (sSQL.matches("(?i)CREATE TABLE.*JMS_TRANSACTIONS.*"))
                           {
                              sSQL = sSQL.replaceAll("(?i)TXID INTEGER", "TXID INTEGER NOT NULL");
                           }
                        }

                        if (((String)entry.getKey()).equals("CREATE_TABLES_ON_STARTUP"))
                        {
                           sSQL = (sqlList == null) ? "TRUE" : "FALSE";
                        }

                        entry.setValue(sSQL);

                        if (sqlList != null && sSQL.startsWith("CREATE"))
                        {
                           sqlList.add(sSQL);
                        }
                     }
                  }

                  XMLUtil.setElementValue(sqlPropertiesTo, PropertyUtil.toString(properties));
               }
            }
         }

         format(file, doc, sOriginalContents);
      }

      /**
       * Installs nexj-jdbc-state-service.xml.
       * @param file The file.
       * @param oldFile The old file, which will be renamed to the new one.
       * @param jmsDB The default database.
       * @param sJNDIName The jndi name of the data source
       * @param sqlList The output list for SQL statements. Can be null.
       */
      protected void installJDBCStateService(Resource file, Resource oldFile, RelationalDatabase jmsDB, String sJNDIName, List sqlList) throws IOException
      {
         Document doc = parse(file, oldFile, true);
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element stateManager = XMLUtil.findChildElementByName(server, "mbean", "jboss.mq:service=StateManager");

         if (stateManager != null)
         {
            Element connectionManager = XMLUtil.findChildElement(stateManager,
               "depends", "optional-attribute-name", "ConnectionManager");

            if (connectionManager != null)
            {
               XMLUtil.setElementValue(connectionManager, "jboss.jca:service=DataSourceBinding,name=" + sJNDIName);
            }

            Element sqlProperties = XMLUtil.findChildElementByName(stateManager, "attribute", "SqlProperties");

            if (sqlProperties != null)
            {
               String sValue = XMLUtil.getElementValue(sqlProperties);

               if (sValue != null)
               {
                  Properties properties = PropertyUtil.fromString(sValue);

                  for (Iterator itr = properties.entrySet().iterator(); itr.hasNext();)
                  {
                     Map.Entry entry = (Map.Entry)itr.next();

                     if (((String)entry.getKey()).startsWith("POPULATE.TABLES."))
                     {
                        itr.remove();
                     }
                     else if (entry.getValue() != null)
                     {
                        String sSQL = fixSQL((String)entry.getValue(), jmsDB);

                        if (((String)entry.getKey()).equals("CREATE_TABLES_ON_STARTUP"))
                        {
                           sSQL = (sqlList == null) ? "TRUE" : "FALSE";
                        }

                        entry.setValue(sSQL);

                        if (sqlList != null && sSQL.startsWith("CREATE"))
                        {
                           sqlList.add(sSQL);
                        }
                     }
                  }

                  XMLUtil.setElementValue(sqlProperties, PropertyUtil.toString(properties));
               }
            }
         }

         format(file, doc, sOriginalContents);
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installTomcat(nexj.core.admin.platform.jboss.JBossInstaller.Resource)
       */
      public void installTomcat(Resource file) throws IOException
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss.web:service=WebServer");

         if (mbean != null)
         {
            Element useJK = XMLUtil.findChildElement(mbean, "attribute", "name", "UseJK");

            if (useJK != null)
            {
               XMLUtil.setElementValue(useJK, String.valueOf(m_metadata.isDistributed()));

               format(file, doc, sOriginalContents);
            }
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installMQService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location, int)
       */
      public void installMQService(ResourceConnection connection, Location location, int nNodeIdx) throws IOException
      {
         Resource file = connection.getResource(location.getSingletonDir(), MQ_SERVICE_FILE_NAME);
         Document doc = parse(file, "<server/>");
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();

         for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
         {
            Channel channel = (Channel)itr.next();

            if (channel instanceof MessageQueue && channel.isEnabled())
            {
               MessageQueue mq = (MessageQueue)channel;

               if (mq.isDestinationManaged() && mq.isFirst())
               {
                  String sMbeanName = "jboss.mq.destination:service=" + ((mq.isBroadcast()) ? "Topic" : "Queue") +
                  ",name=" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + '/' + mq.getAlias();

                  if (mq.getDestination() == null)
                  {
                     Element mbean = XMLUtil.setChildElement(server, null, "mbean", "name", sMbeanName, null, true);

                     mbean.setAttribute("code", (mq.isBroadcast()) ? "org.jboss.mq.server.jmx.Topic" : "org.jboss.mq.server.jmx.Queue");

                     XMLUtil.setChildElement(mbean, null, "depends", "optional-attribute-name",
                        "DestinationManager", "jboss.mq:service=DestinationManager", true);

                     XMLUtil.setChildElement(mbean, null, "depends", "optional-attribute-name",
                        "SecurityManager", "jboss.mq:service=SecurityManager", true);

                     Element securityConf = XMLUtil.setChildElement(mbean, null, "attribute", "name", "SecurityConf", null, true);
                     Element security = XMLUtil.setChildElement(securityConf, null, "security", null, true);

                     addRolesToQueue(mq, security);
                  }
                  else
                  {
                     XMLUtil.removeNode(XMLUtil.findChildElementByName(server, "mbean", sMbeanName));
                  }
               }
            }
         }

         format(file, doc, sOriginalContents);
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#uninstallMQService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location)
       */
      public void uninstallMQService(ResourceConnection connection, Location location) throws IOException
      {
         connection.getResource(location.getSingletonDir(), MQ_SERVICE_FILE_NAME).delete();
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#formatSQL(java.lang.String, nexj.core.meta.persistence.sql.RelationalDatabase)
       */
      public String formatSQL(String sSQL, RelationalDatabase db)
      {
         byte nDBType = JDBCInfo.getInstance(db.getDriver(), db.getAdapter().getName()).getDBType();
         boolean bUnicode = (db == null || db.isUnicode());

         sSQL = sSQL.replaceAll("(?i)\\b((\\w+\\.)?((JMS_|NJ)\\w+))\\b", getOwnerPrefix(db) + "$3");
         sSQL = sSQL.replaceAll("(?i)\\b(n|uni)?varchar2?\\b", ((nDBType == JDBCInfo.DB_TYPE_DB2) ? "" : (((bUnicode) ? ((nDBType == JDBCInfo.DB_TYPE_SYBASE) ? "UNI" : "N") : ""))) + "VARCHAR" + ((nDBType == JDBCInfo.DB_TYPE_ORACLE) ? "2" : ""));
         sSQL = sSQL.replaceAll("(?i)\\b(n|uni)?char\\b", ((nDBType == JDBCInfo.DB_TYPE_DB2) ? "" : (((bUnicode) ? ((nDBType == JDBCInfo.DB_TYPE_SYBASE) ? "UNI" : "N") : ""))) + "CHAR");
         sSQL = sSQL.replaceAll("(?i)\\b(object|image|(long)?blob)\\b", (nDBType == JDBCInfo.DB_TYPE_ORACLE || nDBType == JDBCInfo.DB_TYPE_DB2) ? "BLOB" : (nDBType == JDBCInfo.DB_TYPE_MYSQL) ? "LONGBLOB" : "IMAGE");
         sSQL = sSQL.replaceAll("(?i)\\bcreate\\s+cached\\s+table\\b", "CREATE TABLE");

         return sSQL;
      }
   }

   /**
    * JBoss 5 Installer Strategy
    */
   protected class JBoss5InstallerStrategy extends JBossInstallerStrategy
   {
      protected final static String VFS_FILE_NAME = "vfs.xml";
      protected final static String TRANSACTION_PROPERTIES_FILE_NAME = "jbossts-properties.xml";
      protected final static String JMX_PROPERTIES_ROLES_NAME = "jmx-console-roles.properties";
      protected final static String JMX_PROPERTIES_USERS_NAME = "jmx-console-users.properties";
      protected final String HAPARTITION_SERVICE_FILE_NAME = "cluster" + SysUtil.FILE_SEP + "hapartition-jboss-beans.xml";
      protected final String CHANNEL_FACTORY_SERVICE_FILE_NAME = "cluster" + SysUtil.FILE_SEP + "jgroups-channelfactory.sar" + SysUtil.FILE_SEP + "META-INF" + SysUtil.FILE_SEP + "jgroups-channelfactory-stacks.xml";
      protected final String PERSISTENCE_SERVICE_FILE_NAME = "messaging" + SysUtil.FILE_SEP + SysUtil.NAMESPACE + "-persistence-service.xml";
      protected final String PERSISTENCE_SERVICE_OLD_FILE_NAME = "messaging" + SysUtil.FILE_SEP + "hsqldb-persistence-service.xml";
      protected final String MQ_SERVICE_FILE_NAME = "messaging" + SysUtil.FILE_SEP + SysUtil.NAMESPACE + "-mq-service.xml";
      protected final String MESSAGING_SERVICE_FILE_NAME = "messaging" + SysUtil.FILE_SEP + "messaging-service.xml";
      protected final static String TRANSACTION_SERVICE_FILE_NAME = "jbossts-properties.xml";
      protected final static String sAssumeCompletePropertyName = "com.arjuna.ats.jta.xaAssumeRecoveryComplete";

      /**
       * Installs jboss application server resources
       * @param connection The resource connection
       * @param location The server location
       */
      public void install(ResourceConnection connection, Location location, int nNodeIdx) throws Exception
      {
         super.install(connection, location, nNodeIdx);
         installTransactionProperties(connection.getResource(location.getConfigDir(), TRANSACTION_PROPERTIES_FILE_NAME), connection.getResource(location.getPropsDir(), JMX_PROPERTIES_ROLES_NAME), connection.getResource(location.getPropsDir(), JMX_PROPERTIES_USERS_NAME));
      }

      /**
       * Configures transaction recovery modules for JBossAS 5
       * @param file Resource that represents jbossts-properties.xml file.
       * @param fileJMXRoles Resource that represents jmx-console-roles.properties file.
       * @param fileJMXUsers Resource that represents jmx-console-users.properties file.
       * @throws IOException
       */
      protected void installTransactionProperties(Resource file, Resource fileJMXRoles, Resource fileJMXUsers) throws IOException
      {
         String sJMXUser = "";
         String sJMXPassword = "";
         InputStream istreamJMXRoles = null;
         InputStream istreamJMXUsers = null;

         try
         {
            Properties psJMXRoles = new Properties();

            istreamJMXRoles = fileJMXRoles.getInputStream();

            if (istreamJMXRoles == null)
            {
               throw new IOException("Missing resource \"" + fileJMXRoles + "\"");
            }

            istreamJMXRoles = new BufferedInputStream(istreamJMXRoles);

            psJMXRoles.load(istreamJMXRoles);

            Iterator itrUsers = psJMXRoles.keySet().iterator();

            while (itrUsers.hasNext())
            {
               sJMXUser = (String)itrUsers.next();

               if (((String)psJMXRoles.get(sJMXUser)).contains("JBossAdmin"))
               {
                  // The user with the name sUser is JBossAdmin
                  // we get the password from the jmx console users properties file

                  Properties psJMXUsers = new Properties();

                  istreamJMXUsers = fileJMXUsers.getInputStream();

                  if (istreamJMXUsers == null)
                  {
                     throw new IOException("Missing resource \"" + fileJMXUsers + "\"");
                  }

                  istreamJMXUsers = new BufferedInputStream(istreamJMXUsers);

                  psJMXUsers.load(istreamJMXUsers);

                  Object objectPassword = psJMXUsers.get(sJMXUser);

                  if (objectPassword != null)
                  {
                     sJMXPassword = (String)objectPassword;
                  }
               }
            }
         }
         finally
         {
            if (istreamJMXRoles != null)
            {
               istreamJMXRoles.close();
            }

            if (istreamJMXRoles != null)
            {
               istreamJMXRoles.close();
            }
         }

         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element transactionService = doc.getDocumentElement();
         Element properties = XMLUtil.findChildElement(transactionService, "properties", "name", "jta");

         if (properties == null)
         {
            return;
         }

         // Get all children of the properties element
         // Delete those of children the name of which start with com.arjuna.ats.jta.recovery.XAResourceRecovery.nexj-<envName>

         NodeList children = properties.getChildNodes();

         for (int i = 0;; i++)
         {
            Node node = children.item(i);
            
            if (node == null)
            {
               break;
            }
            
            if (node.getNodeType() == Node.ELEMENT_NODE)
            {
               if (((Element)node).getAttribute("name").startsWith("com.arjuna.ats.jta.recovery.XAResourceRecovery." + SysUtil.PACKAGE + m_sEnvironmentName))
               {
                  XMLUtil.removeNode(node);
                  i--;
               }
            }
         }

         Element assumeCompleteElement = XMLUtil.findChildElementByName(properties, sAssumeCompletePropertyName);

         assumeCompleteElement = XMLUtil.setChildElement(properties, null, "property", "name", sAssumeCompletePropertyName, null, true);
         XMLUtil.setAttribute(assumeCompleteElement, "value", "true", true);

         for (Iterator dsItr = m_metadata.getDataSourceIterator(); dsItr.hasNext();)
         {
            DataSource ds = (DataSource)dsItr.next();

            if (ds.isEnabled() && ds instanceof RelationalDatabase)
            {
               RelationalDatabase db = (RelationalDatabase)ds;

               for (Iterator fragmentItr = db.getFragmentIterator(); fragmentItr.hasNext();)
               {
                  RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)fragmentItr.next();

                  if (fragment.isFirst())
                  {
                     String sDataSourceJNDIName = SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jdbc/" + db.getAlias();
                     String sElementName = "com.arjuna.ats.jta.recovery.XAResourceRecovery." + SysUtil.PACKAGE + '-' + m_sEnvironmentName + "." + sDataSourceJNDIName;
                     Element jdbcRecoveryModuleElement = XMLUtil.setChildElement(properties, null, "property", "name", sElementName, null, true);
                     
                     XMLUtil.setAttribute(jdbcRecoveryModuleElement, "value", SysUtil.PACKAGE + ".core.container.platform.jboss.SQLXAResourceRecovery;jndiname=" + sDataSourceJNDIName + ",username=" + sJMXUser + ",password=" + sJMXPassword + ",dbusername=" + fragment.getUser() + ",dbpassword=" + encrypt(fragment.getPassword()), true);
                  }
               }
            }
         }

         // The list of strings that will contain the names of all JMS adapters already initialized
         // necessary as for each JMS adapter we need to configure recovery module only once.
         List initAdapterList = new ArrayList();

         for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
         {
            Channel channel = (Channel)itr.next();

            if (channel instanceof MessageQueue && channel.isEnabled())
            {
               MessageQueue mq = (MessageQueue)channel;

               if (mq.isFirst() && mq.isTransactional())
               {
                  String sJMSAdapter = null;
                  String sCFJNDIName = getCFJNDIName(mq);

                  String sCurrentConnectionFactory = mq.getConnectionFactory();

                  if (sCurrentConnectionFactory == null)
                  {
                     // for Default JMS engine (JBoss Messaging)
                     sJMSAdapter = SysUtil.PACKAGE + ".core.rpc.jms.ra.platform.jbm.platform.jboss.JBMXARecovery";
                     sCFJNDIName = "XAConnectionFactory";
                  }
                  else
                  {
                     // The relation between JMS adapter class and its recovery module is:
                     // class:<package>.<Class> -> <package>.platform.jboss.<Class>XARecovery
                     sCurrentConnectionFactory = sCurrentConnectionFactory.substring("class:".length());

                     int nLastIndexOfDot = sCurrentConnectionFactory.lastIndexOf('.');

                     String sAdapterName = sCurrentConnectionFactory.substring(nLastIndexOfDot + 1);

                     if (nLastIndexOfDot >= 0)
                     {
                        String sPackage = sCurrentConnectionFactory.substring(0, nLastIndexOfDot);

                        sJMSAdapter = sPackage + "." + "platform.jboss." + sAdapterName + "XARecovery";
                     }
                     else
                     {
                        sJMSAdapter = "platform.jboss." + sAdapterName + "XARecovery";
                     }
                  }

                  // Configure not more than one recovery module per JMS adapter
                  if (initAdapterList.contains(sJMSAdapter))
                  {
                     continue;
                  }

                  Properties props = new Properties();

                  props.setProperty("JMSAdapter", sJMSAdapter);
                  props.setProperty("connectionFactoryName", sCFJNDIName);
                  props.setProperty("waitInMillis", "5000");

                  if (mq.getUser() != null)
                  {
                     props.setProperty("user", mq.getUser());
                  }

                  if (mq.getPassword() != null)
                  {
                     props.setProperty("password", encrypt(mq.getPassword()));
                  }

                  Element jmsRecoveryModuleElement = XMLUtil.setChildElement(properties, null, "property", "name",
                     "com.arjuna.ats.jta.recovery.XAResourceRecovery." + SysUtil.PACKAGE + '-' + m_sEnvironmentName + "." + mq.getAlias(), null, true);
                  
                  XMLUtil.setAttribute(jmsRecoveryModuleElement, "value", SysUtil.PACKAGE +
                     ".core.container.platform.jboss.JMSXAResourceRecovery;" + PropertyUtil.toString(props), true);
                  initAdapterList.add(sJMSAdapter);
               }
            }
         }

         XMLUtil.normalize(transactionService);
         format(file, doc, sOriginalContents);  
      }

      /**
       * Due to JBoss 5 bug in the clustered environment we need to add manually the farm directory
       * to the ExceptionHandler map in the vfs.xml file.
       *
       * http://community.jboss.org/message/7585
       * https://issues.jboss.org/browse/JBAS-7126
       *
       * @param file Resource that represents vfs.xml file.
       * @throws IOException
       */
      protected void installVFS(Resource file) throws IOException
      {
         Document doc = parse(file, "");
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element bean = XMLUtil.findChildElement(server, "bean", "name", "VFSCache");
         
         if (bean == null) return;
         
         Element permanentRoots = XMLUtil.findChildElement(bean, "property", "name", "permanentRoots");
         
         if (permanentRoots == null) return;
         
         Element mapExceptionHandler = XMLUtil.findChildElement(permanentRoots, "map", "valueClass", "org.jboss.virtual.spi.ExceptionHandler");
         
         if (mapExceptionHandler == null) return;

         if (XMLUtil.findChildElementBySubelement(mapExceptionHandler, "entry", "key", "${jboss.server.home.url}farm") == null)
         {
            Element deployElement = (Element)XMLUtil.findChildElementBySubelement(mapExceptionHandler, "entry", "key", "${jboss.server.home.url}deploy").cloneNode(true);
            
            if (deployElement == null) return;
            
            Element keyElement = XMLUtil.findChildElement(deployElement, "key", null, null);

            keyElement.getFirstChild().setNodeValue("${jboss.server.home.url}farm");
            mapExceptionHandler.appendChild(deployElement);
         }

         XMLUtil.normalize(server);
         format(file, doc, sOriginalContents);
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getLog4JFileName()
       */
      public String getLog4JFileName()
      {
         return "jboss-log4j.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getServerFileName()
       */
      public String getServerFileName()
      {
         return "jbossweb.sar" + SysUtil.FILE_SEP + "server.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getWebFileName()
       */
      public String getWebFileName()
      {
         return ".." + SysUtil.FILE_SEP + "deployers" + SysUtil.FILE_SEP + "jbossweb.deployer" + SysUtil.FILE_SEP + "web.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getTomcatFileName()
       */
      public String getTomcatFileName()
      {
         return ".." + SysUtil.FILE_SEP + "deployers" + SysUtil.FILE_SEP + "jbossweb.deployer" + SysUtil.FILE_SEP + "META-INF" + SysUtil.FILE_SEP + "war-deployers-jboss-beans.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getJMSDSFileName()
       */
      public String getJMSDSFileName()
      {
         return "messaging" + SysUtil.FILE_SEP + "jms-ds.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getHAJNDIJMSDSFileName()
       */
      public String getHAJNDIJMSDSFileName()
      {
         return "messaging" + SysUtil.FILE_SEP + "jms-ds.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getMQLoginPolicyName()
       */
      public String getMQLoginPolicyName()
      {
         return "messaging";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getPersistenceServiceTemplateFileSuffix()
       */
      public String getPersistenceServiceTemplateFileSuffix()
      {
         return "-persistence-service.xml";
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getMQLoginConfigResource(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location)
       */
      public Resource getMQLoginConfigResource(ResourceConnection connection, Location location)
      {
         return connection.getResource(location.getDeploymentDir(), "messaging" + SysUtil.FILE_SEP + "messaging-jboss-beans.xml");
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#getJMSDBConnectionInfo()
       */
      public JDBCConnectionInfo getJMSDBConnectionInfo() throws IOException
      {
         if (m_jmsDBConnectionInfo == null)
         {
            m_jmsDBConnectionInfo = JBossInstaller.this.getJMSDBConnectionInfo();

            if (m_jmsDBConnectionInfo == null)
            {
               m_jmsDBConnectionInfo = getDerivedJMSDBConnectionInfo();
            }
         }

         return m_jmsDBConnectionInfo;
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#addJMSDBApplicationPolicy(org.w3c.dom.Element, java.lang.String, RelationalDatabase)
       */
      public void addJMSDBApplicationPolicy(Element policies, JDBCConnectionInfo jmsJDBCConnInfo)
      {
         RelationalDatabase db = jmsJDBCConnInfo.getBackingDB();
         RelationalDatabaseFragment dbFragment = (RelationalDatabaseFragment)db.getDefaultFragment();

         addApplicationPolicy(policies, getJAASDomain(dbFragment), LOCAL_TX_JCA_SERVICE_NAME,
            SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jms/jdbc/" + dbFragment.getAlias(), dbFragment.getUser(), dbFragment.getPassword());
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#addDataSource(org.w3c.dom.Document, nexj.core.meta.persistence.sql.RelationalDatabase, nexj.core.meta.persistence.sql.RelationalDatabaseFragment, java.lang.String)
       */
      public void addJMSDataSource(Document document, JDBCConnectionInfo jmsJDBCConnInfo, String sJNDIPrefix)
      {
         addDataSource(document, jmsJDBCConnInfo.getBackingDB(),
            (RelationalDatabaseFragment)jmsJDBCConnInfo.getBackingDB().getDefaultFragment(), jmsJDBCConnInfo, sJNDIPrefix);
      }

      /**
       * Derives URL bases jdbc connection info from the XA datasource base DefaultRelationalDatabase.
       *
       * @return JDBCConnectionInfo containing the derived RelationalDatabase and additional  connection info.
       */
      protected JDBCConnectionInfo getDerivedJMSDBConnectionInfo()
      {
         String sJMSDBName = "JMS_DATABASE";
         RelationalDatabase defaultDB = getDefaultDatabase();
         RelationalDatabaseFragment defaultDBFragment = (RelationalDatabaseFragment)defaultDB.getDefaultFragment();

         RelationalDatabase jmsDB = new RelationalDatabase(sJMSDBName);
         RelationalDatabaseFragment jmsDBFragment = (RelationalDatabaseFragment)jmsDB.getDefaultFragment();

         jmsDB.setAdapter(defaultDB.getAdapter());
         jmsDB.setComponent(defaultDB.getComponent());
         jmsDB.setDriver(defaultDB.getDriver());
         jmsDB.setAlias(sJMSDBName);
         jmsDB.setUnicode(false);

         jmsDBFragment.setAlias(sJMSDBName + jmsDBFragment.getSuffix());
         jmsDBFragment.setDefaultProperties(J2EEUtil.JBOSS);
         jmsDBFragment.setHost(defaultDBFragment.getHost());
         jmsDBFragment.setPort(defaultDBFragment.getPort());
         jmsDBFragment.setDatabase(defaultDBFragment.getDatabase());
         jmsDBFragment.setUser(defaultDBFragment.getUser());
         jmsDBFragment.setPassword(defaultDBFragment.getPassword());
         jmsDBFragment.setMaxPoolSize(getJMSDSMaxPoolSize());
         jmsDBFragment.setMinPoolSize(Math.min(3, jmsDBFragment.getMaxPoolSize()));

         for (Lookup.Iterator iter = defaultDBFragment.getPropertyHolder().getPropertyIterator(); iter.hasNext();)
         {
            iter.next();
            String sProperty = (String)iter.getKey();

            if (!sProperty.equals("serverName") && !sProperty.equals("portNumber") && !sProperty.equals("databaseName"))
            {
               jmsDBFragment.getPropertyHolder().addDefaultProperty(sProperty, (String)iter.getValue());
            }
         }

         JDBCConnectionInfo jmsDBConnInfo = createJDBCConnectionInfo(jmsDB);

         return jmsDBConnInfo;
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#addCustomProperties(org.w3c.dom.Element)
       */
      public void addCustomProperties(Element propsAttribute) throws IllegalArgumentException, IOException
      {
         Properties properties = PropertyUtil.fromString(XMLUtil.getElementValue(propsAttribute));

         properties.setProperty("org.apache.catalina.connector.Request.SESSION_ID_CHECK", "false");
         XMLUtil.setElementValue(propsAttribute, PropertyUtil.toString(properties));
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installClusterService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location)
       */
      public void installClusterService(ResourceConnection connection, Location location) throws IOException
      {
         if (m_metadata.isDistributed())
         {
            installHAPartitionService(connection.getResource(location.getDeploymentDir(), HAPARTITION_SERVICE_FILE_NAME));
            installVFS(connection.getResource(location.getBootstrapDir(), VFS_FILE_NAME));
         }
      }

      /**
       * Installs custom config for the HA Partition Service
       * @param file The HA Partition Service config file
       * @throws IOException
       */
      public void installHAPartitionService(Resource file) throws IOException
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element deployment = doc.getDocumentElement();
         Element bean = XMLUtil.findChildElement(deployment, "bean", "name", "HAPartition");

         if (bean != null)
         {
            Element timeout = XMLUtil.findChildElement(bean, "property", "name", "stateTransferTimeout");

            if (timeout != null)
            {
               long lTimeout = 0;

               try
               {
                  lTimeout = Long.parseLong(XMLUtil.getElementValue(timeout));
               }
               catch (Exception e)
               {
               }

               if (lTimeout < 300000)
               {
                  XMLUtil.setElementValue(timeout, "300000");
               }
            }

            format(file, doc, sOriginalContents);
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installPersistenceService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location, nexj.core.admin.platform.jboss.JBossInstaller.Resource, nexj.core.admin.platform.jboss.JBossInstaller.Resource, nexj.core.meta.persistence.sql.RelationalDatabase)
       */
      public void installPersistenceService(ResourceConnection connection, Location location, Resource templateFile, Resource sqlFile, RelationalDatabase jmsDB) throws IOException
      {
         if (templateFile == null)
         {
            throw new RuntimeException("Unable to find the JMS template file. " +
               "Please copy the \"jms\" directory from <JBOSS_HOME>/docs/examples to your server root");
         }

         installPersistenceService(connection.getResource(location.getDeploymentDir(), PERSISTENCE_SERVICE_FILE_NAME),
            connection.getResource(location.getDeploymentDir(), PERSISTENCE_SERVICE_OLD_FILE_NAME), templateFile, sqlFile, jmsDB);
      }

      /**
       * Installs the JMS Persistence Serice
       * @param file The JMS Persistence Serice config file
       * @param oldFile The old config file to be replaced by the new one
       * @param templateFile The template file (from docs/examples/jms) with the default config for a specific DB
       * @param sqlFile The file to output DDL commands to. Can be null to not do this.
       * @param jmsDB The DB that is used as the data source for JMS persistence
       * @throws IOException
       */
      protected void installPersistenceService(Resource file, Resource oldFile, Resource templateFile, Resource sqlFile, RelationalDatabase jmsDB) throws IOException
      {
         if (oldFile.exists())
         {
            oldFile.delete();
         }

         Document doc = parse(file, templateFile, false);
         Document templateDoc = parse(templateFile, null);
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element templateServer = templateDoc.getDocumentElement();
         String[] serviceNames = new String[] {"PersistenceManager", "PostOffice", "JMSUserManager"};
         List sqlList = new ArrayList();

         for (int i = 0; i < serviceNames.length; i++)
         {
            String sServiceName = serviceNames[i];

            Element serviceMBean = XMLUtil.findChildElementByName(server, "mbean", "jboss.messaging:service=" + sServiceName);
            Element templateServiceMBean = XMLUtil.findChildElementByName(templateServer, "mbean", "jboss.messaging:service=" + sServiceName);

            if (serviceMBean != null && templateServiceMBean != null)
            {
               String sAlias = ((RelationalDatabaseFragment)jmsDB.getDefaultFragment()).getAlias();
               Element dsBindingDependency = XMLUtil.findChildElementByElementValuePrefix(serviceMBean, "depends", "jboss.jca:service=DataSourceBinding");

               if (dsBindingDependency != null)
               {
                  XMLUtil.setElementValue(dsBindingDependency, "jboss.jca:service=DataSourceBinding,name=" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jms/jdbc/" +
                     sAlias);
               }

               Element dataSourceAttr = XMLUtil.findChildElementByName(serviceMBean, "attribute", "DataSource");

               if (dataSourceAttr != null)
               {
                  XMLUtil.setElementValue(dataSourceAttr, "java:/" + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/jms/jdbc/" + sAlias);
               }

               Element createTablesAttr = XMLUtil.findChildElementByName(serviceMBean, "attribute", "CreateTablesOnStartup");

               if (createTablesAttr != null)
               {
                  XMLUtil.setElementValue(createTablesAttr, (sqlFile == null) ? "true" : "false");
               }

               boolean bIsPostOfficeMBean = sServiceName.equals("PostOffice");
               boolean bIsJMSUserManagerMBean = sServiceName.equals("JMSUserManager");

               if (bIsPostOfficeMBean)
               {
                  Element clusteredAttrib = XMLUtil.findChildElement(serviceMBean, "attribute", "name", "Clustered");
                  Element failoverAttrib = XMLUtil.findChildElement(serviceMBean, "attribute", "name", "FailoverOnNodeLeave");
                  Element channelFactoryDependency = XMLUtil.findChildElement(serviceMBean, "depends", "optional-attribute-name", "ChannelFactoryName");

                  if (m_metadata.isDistributed())
                  {
                     XMLUtil.setElementValue(clusteredAttrib, "true");
                     XMLUtil.setElementValue(failoverAttrib, "true");

                     if (channelFactoryDependency == null)
                     {
                        Comment chFactoryDepComment = XMLUtil.findChildComment(serviceMBean, "<depends optional-attribute-name=\"ChannelFactoryName\">jboss.jgroups:service=ChannelFactory</depends>");

                        if (chFactoryDepComment != null)
                        {
                           XMLUtil.removeNode(chFactoryDepComment);
                        }

                        XMLUtil.setChildElement(serviceMBean, null, "depends", "optional-attribute-name", "ChannelFactoryName", "jboss.jgroups:service=ChannelFactory", false);
                     }
                  }
                  else
                  {
                     XMLUtil.setElementValue(clusteredAttrib, "false");
                     XMLUtil.setElementValue(failoverAttrib, "false");

                     if (channelFactoryDependency != null)
                     {
                        Comment chFactoryDepComment = XMLUtil.findChildComment(serviceMBean, "<depends optional-attribute-name=\"ChannelFactoryName\">jboss.jgroups:service=ChannelFactory</depends>");

                        if (chFactoryDepComment == null)
                        {
                           serviceMBean.insertBefore(doc.createComment("Uncomment for clustered PostOffice:\n" + format(channelFactoryDependency, false)), channelFactoryDependency);
                        }

                        XMLUtil.removeNode(channelFactoryDependency);
                     }
                  }
               }

               Element sqlPropertiesAttr = XMLUtil.findChildElementByName(serviceMBean, "attribute", "SqlProperties");
               Element templateSqlProperties = XMLUtil.findChildElementByName(templateServiceMBean, "attribute", "SqlProperties");

               if (sqlPropertiesAttr != null && templateSqlProperties != null)
               {
                  String sSqlProperties = XMLUtil.getElementValue(templateSqlProperties);

                  if (sSqlProperties != null)
                  {
                     Properties sqlProperties = PropertyUtil.fromString(sSqlProperties);

                     for (Iterator itr = sqlProperties.entrySet().iterator(); itr.hasNext();)
                     {
                        Map.Entry entry = (Map.Entry)itr.next();
                        String sKey = (String)entry.getKey();
                        String sValue = (String)entry.getValue();

                        if (sValue != null)
                        {
                           if (bIsJMSUserManagerMBean && sKey.startsWith("POPULATE.TABLES."))
                           {
                              itr.remove();
                           }
                           else
                           {
                              String sSQL = fixSQL(sValue, jmsDB);

                              entry.setValue(sSQL);

                              if (sqlFile != null && sSQL.startsWith("CREATE"))
                              {
                                 sqlList.add(sSQL);
                              }
                           }
                        }
                     }

                     XMLUtil.setElementValue(sqlPropertiesAttr, PropertyUtil.toString(sqlProperties));
                  }
               }
            }
         }

         if (sqlFile != null)
         {
            JBossInstaller.this.formatSQL(sqlFile, sqlList, jmsDB);
         }

         format(file, doc, sOriginalContents);
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installTomcat(nexj.core.admin.platform.jboss.JBossInstaller.Resource)
       */
      public void installTomcat(Resource file) throws IOException
      {
         Document doc = parse(file, null);
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element mbean = XMLUtil.findChildElement(server, "bean", "name", "WebAppClusteringDefaultsDeployer");

         if (mbean != null)
         {
            Element useJK = XMLUtil.findChildElement(mbean, "property", "name", "useJK");

            if (useJK != null)
            {
               XMLUtil.setElementValue(useJK, String.valueOf(m_metadata.isDistributed()));
            }
            else
            {
               useJK = XMLUtil.addChildElement(mbean, null, "property", String.valueOf(m_metadata.isDistributed()));
               XMLUtil.setAttribute(useJK, "name", "useJK", true);
            }

            format(file, doc, sOriginalContents);
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#installMQService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location, int)
       */
      public void installMQService(ResourceConnection connection, Location location, int nNodeIdx) throws IOException
      {
         Resource file = connection.getResource(location.getDeploymentDir(), MQ_SERVICE_FILE_NAME);
         Document doc = parse(file, "<server/>");
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();

         for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
         {
            Channel channel = (Channel)itr.next();

            if (channel instanceof MessageQueue && channel.isEnabled())
            {
               MessageQueue mq = (MessageQueue)channel;

               if (mq.isDestinationManaged() && mq.isFirst())
               {
                  String sMbeanName = "jboss.messaging.destination:service=" +
                  ((mq.isBroadcast()) ? "Topic" : "Queue") + ",name=" + SysUtil.NAMESPACE + '-' + m_metadata.getEnvironment() + '-' + mq.getAlias();

                  if (mq.getDestination() == null)
                  {
                     Element mbean = XMLUtil.setChildElement(server, null, "mbean", "name", sMbeanName, null, true);

                     mbean.setAttribute("code", "org.jboss.jms.server.destination." + (mq.isBroadcast() ? "Topic" : "Queue") + "Service");
                     mbean.setAttribute("xmbean-dd", "xmdesc/" + (mq.isBroadcast() ? "Topic" : "Queue") + "-xmbean.xml");

                     XMLUtil.setChildElement(mbean, null, "depends", "optional-attribute-name", "ServerPeer", "jboss.messaging:service=ServerPeer", true);
                     XMLUtil.addChildElement(mbean, null, "depends", "jboss.messaging:service=PostOffice");
                     XMLUtil.addChildElement(mbean, null, "depends", "jboss.messaging:service=SecurityStore");
                     XMLUtil.setChildElement(mbean, null, "attribute", "name", "JNDIName", (mq.isBroadcast() ? "topic" : "queue") + '/' + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + '/' + mq.getAlias(), true);

                     if (m_metadata.isDistributed())
                     {
                        XMLUtil.setChildElement(mbean, null, "attribute", "name", "Clustered", "true", true);
                     }
                     else
                     {
                        Element clusteredAttrib = XMLUtil.findChildElement(mbean, "attribute", "name", "Clustered");

                        if (clusteredAttrib != null)
                        {
                           XMLUtil.setElementValue(clusteredAttrib, "false");
                        }
                     }

                     Element securityConf = XMLUtil.setChildElement(mbean, null, "attribute", "name", "SecurityConfig", null, true);
                     Element security = XMLUtil.setChildElement(securityConf, null, "security", null, true);

                     addRolesToQueue(mq, security);
                  }
                  else
                  {
                     XMLUtil.removeNode(XMLUtil.findChildElementByName(server, "mbean", sMbeanName));
                  }
               }
            }
         }

         format(file, doc, sOriginalContents);

         if (m_metadata.isDistributed())
         {
            installMessagingService(connection, location, nNodeIdx);
         }
      }

      /**
       * Configures the messaging service for clustering
       * @param connection The resource connection
       * @param location The server location
       * @param nNodeIdx The server cluster node index
       * @throws IOException
       */
      public void installMessagingService(ResourceConnection connection, Location location, int nNodeIdx) throws IOException
      {
         Resource file = connection.getResource(location.getDeploymentDir(), MESSAGING_SERVICE_FILE_NAME);
         Document doc = parse(file, "<server/>");
         String sOriginalContents = format(doc);
         Element server = doc.getDocumentElement();
         Element mbean = XMLUtil.findChildElement(server, "mbean", "name", "jboss.messaging:service=ServerPeer");

         if (mbean != null)
         {
            XMLUtil.setChildElement(mbean, null, "attribute", "name", "ServerPeerID", "${jboss.messaging.ServerPeerID:" + nNodeIdx + "}", true);

            format(file, doc, sOriginalContents);
         }
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#formatSQL(java.lang.String, nexj.core.meta.persistence.sql.RelationalDatabase)
       */
      public String formatSQL(String sSQL, RelationalDatabase db)
      {
         /* prepends DB owner to all table names starting with "JBM_" or "NJ":
          * - (?i):
          *    case insensitive
          * - (?<!CREATE INDEX ):
          *    not preceded by "CREATE INDEX" anywhere in the line
          *    i.e. ensure only table names are matched and not index names
          * - (JBM_(?!ID)...):
          *    match JBM_* but not if the matching token is exactly JBM_ID
          *    i.e. ensure table names starting with JBM_ are matched but JBM_ID column name is not
          * - ((...|JBM_ID|...)\\w+):
          *    match JBM_ID followed by other chars (i.e. not exactly JBM_ID)
          *    i.e. ensure table names starting with JBM_ID (such as JBM_ID_CACHE) are still matched
          */
         return sSQL.replaceAll("(?i)(?<!CREATE INDEX )\\b((\\w+\\.)?((JBM_(?!ID)|JBM_ID|NJ)\\w+))\\b", getOwnerPrefix(db) + "$3");
      }

      /**
       * @see nexj.core.admin.platform.jboss.JBossInstaller.JBossInstallerStrategy#uninstallMQService(nexj.core.admin.platform.jboss.JBossInstaller.ResourceConnection, nexj.core.admin.platform.jboss.JBossInstaller.Location)
       */
      public void uninstallMQService(ResourceConnection connection, Location location) throws IOException
      {
         connection.getResource(location.getDeploymentDir(), MQ_SERVICE_FILE_NAME).delete();
      }

      /**
       * @param backingDB The backing database
       * @return The database specific jdbc connection info
       */
      protected JDBCConnectionInfo createJDBCConnectionInfo(RelationalDatabase backingDB)
      {
         JDBCInfo jdbcInfo = JDBCInfo.getInstance(backingDB.getDriver(), backingDB.getAdapter().getName());
         RelationalDatabaseFragment backingDBFragment = (RelationalDatabaseFragment)backingDB.getDefaultFragment();
         String sDriver = jdbcInfo.getURLDriver();
         String sConnURL = jdbcInfo.getURL(backingDBFragment.getHost(),
            String.valueOf(backingDBFragment.getPort()), backingDBFragment.getDatabase());

         return new JDBCConnectionInfo(backingDB, sDriver, sConnURL, jdbcInfo.getTestSQL());
      }
   }

   /**
    * This class holds additional JDBC connection info for non-XA data sources as well as XA to non-XA driver mapping.
    */
   protected static class JDBCConnectionInfo
   {
      protected RelationalDatabase m_backingDB;

      protected String m_sDriver;

      protected String m_sConnectionURL;

      protected String m_sTestSQL;

      public JDBCConnectionInfo(RelationalDatabase backingDB, String sDriver, String sConnectionURL, String sTestSQL)
      {
         m_backingDB = backingDB;
         m_sDriver = sDriver;
         m_sConnectionURL = sConnectionURL;
         m_sTestSQL = sTestSQL;

         if (m_backingDB.getDriver() == null)
         {
            m_backingDB.setDriver(JDBCInfo.getInstance(sConnectionURL).getDriver());
         }
      }

      /**
       * @return The backing database
       */
      public RelationalDatabase getBackingDB()
      {
         return m_backingDB;
      }

      /**
       * @return The driver
       */
      public String getDriver()
      {
         return m_sDriver;
      }

      /**
       * @return The connectionURL
       */
      public String getConnectionURL()
      {
         return m_sConnectionURL;
      }

      /**
       * @return The test SQL
       */
      public String getTestSQL()
      {
         return m_sTestSQL;
      }
   }

   /**
    * @return The data source connection info to be used for JMS persistence.
    * @throws IOException
    */
   protected JDBCConnectionInfo getJMSDBConnectionInfo() throws IOException
   {
      String sJMSDBName = "JMS_DATABASE";
      String sJMSDBURL = m_properties.getProperty("jms.jdbc.url");
      String sJMSDBDriver = m_properties.getProperty("jms.jdbc.driver");
      String sJMSDBUser = m_properties.getProperty("jms.jdbc.user");
      String sJMSDBPassword = m_properties.getProperty("jms.jdbc.password");

      if (sJMSDBURL == null && sJMSDBDriver == null && sJMSDBUser == null && sJMSDBPassword == null)
      {
         return null;
      }

      if (sJMSDBURL == null || sJMSDBDriver == null || sJMSDBUser == null || sJMSDBPassword == null)
      {
         throw new RuntimeException("Please ensure that all of the following jms.jdbc.* properties " +
            "are specified for a custom JMS data source: url, driver, user, password");
      }

      JDBCInfo jdbcInfo = JDBCInfo.getInstance(sJMSDBURL);
      RelationalDatabase jmsDB = new RelationalDatabase(sJMSDBName);
      RelationalDatabaseFragment jmsDBFragment = (RelationalDatabaseFragment)jmsDB.getDefaultFragment();

      jmsDB.setAlias(sJMSDBName);
      jmsDB.setUnicode(false);
      jmsDB.setAdapter(new DataSourceAdapter(jdbcInfo.getDBTypeName()));
      jmsDBFragment.setAlias(sJMSDBName + jmsDBFragment.getSuffix());
      jmsDBFragment.setUser(sJMSDBUser);
      jmsDBFragment.setPassword(sJMSDBPassword);

      int nMinPoolSize;
      int nMaxPoolSize;

      try
      {
         String sMaxPoolSize = m_properties.getProperty("jms.jdbc.maxPoolSize");
         nMaxPoolSize = (sMaxPoolSize == null) ? getJMSDSMaxPoolSize() : Integer.parseInt(sMaxPoolSize);
      }
      catch (NumberFormatException e)
      {
         throw new IllegalArgumentException("Invalid JMS DS maxPoolSize \"" + m_properties.getProperty("jms.jdbc.maxPoolSize") + "\"", e);
      }

      try
      {
         String sMinPoolSize = m_properties.getProperty("jms.jdbc.minPoolSize");
         nMinPoolSize = (sMinPoolSize == null) ? Math.min(3, nMaxPoolSize) : Integer.parseInt(sMinPoolSize);
      }
      catch (NumberFormatException e)
      {
         throw new IllegalArgumentException("Invalid JMS DS minPoolSize \"" + m_properties.getProperty("jms.jdbc.minPoolSize") + "\"", e);
      }

      jmsDBFragment.setMinPoolSize(nMinPoolSize);
      jmsDBFragment.setMaxPoolSize(nMaxPoolSize);

      String sCustomProps = m_properties.getProperty("jms.jdbc.properties");

      if (sCustomProps != null)
      {
         Properties properties = PropertyUtil.fromString(sCustomProps);

         for (Iterator itr = properties.entrySet().iterator(); itr.hasNext();)
         {
            Map.Entry entry = (Map.Entry)itr.next();
            jmsDBFragment.getPropertyHolder().addDefaultProperty((String)entry.getKey(), (String)entry.getValue());
         }
      }

      return new JDBCConnectionInfo(jmsDB, sJMSDBDriver, sJMSDBURL, jdbcInfo.getTestSQL());
   }

   /**
    * @param sToEncrypt A string to encrypt
    * @return The encrypted string
    */
   protected String encrypt(String sToEncrypt)
   {
      // J2EE Descriptor password encryption
      CharacterStreamCipherDispatcher dispatcher = null;

      if (!m_metadata.isEncrypted() && SysUtil.ENTERPRISE)
      {
         dispatcher = new CharacterStreamCipherDispatcher();

         if (m_properties.getProperty("cipher.scheme") == null)
         {
            m_properties.setProperty("cipher.scheme", m_metadata.getEncryptionScheme());
         }

         dispatcher.init(m_properties);
      }

      return (dispatcher != null) ? dispatcher.encrypt(sToEncrypt) : sToEncrypt;
   }

   /**
    * Calculates the maximum JDBC connection pool size used by the JMS engine based
    * on the total number of MessageQueues defined in the system.  The size is increased
    * by 1 to allow for an additional connection from the JMS console.
    *
    * @return The max database connection pool size to be used by the JMS engine.
    */
   protected int getJMSDSMaxPoolSize()
   {
      int nMaxPoolSize = 1;

      for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
      {
         Channel channel = (Channel)itr.next();

         if (channel instanceof MessageQueue && channel.isEnabled())
         {
            nMaxPoolSize++;
         }
      }

      return nMaxPoolSize;
   }

   /**
    * Configures the user role access permissions to a given queue. All MQs sharing the same alias
    * as the given queue will have their users added to the ACL.
    * @param mq The message queue.
    * @param security The security element.
    */
   protected void addRolesToQueue(MessageQueue mq, Element security)
   {
      Set receivableUserSet = new HashHolder();
      Set sendableUserSet = new HashHolder();

      for (Iterator roleItr = m_metadata.getChannelIterator(); roleItr.hasNext(); )
      {
         Channel roleChannel = (Channel)roleItr.next();

         if (roleChannel.isEnabled() && roleChannel instanceof MessageQueue)
         {
            MessageQueue roleMQ = (MessageQueue)roleChannel;

            if (roleMQ.getUser() != null && roleMQ.getAlias().equals(mq.getAlias()))
            {
               if (roleMQ.isReceivable())
               {
                  receivableUserSet.add(roleMQ.getUser());
               }

               if (roleMQ.isSendable())
               {
                  sendableUserSet.add(roleMQ.getUser());
               }
            }
         }
      }

      // logical OR of the permissions for a user
      for (Iterator roleItr = receivableUserSet.iterator(); roleItr.hasNext(); )
      {
         String sRole = (String)roleItr.next();
         boolean bSendable = sendableUserSet.remove(sRole);
         Element role = XMLUtil.setChildElement(security, null, "role", "name", sRole, null, true);

         role.setAttribute("read", "true");
         role.setAttribute("write", String.valueOf(bSendable));
         role.setAttribute("create", String.valueOf(bSendable));
      }

      for (Iterator roleItr = sendableUserSet.iterator(); roleItr.hasNext(); )
      {
         String sRole = (String)roleItr.next();
         Element role = XMLUtil.setChildElement(security, null, "role", "name", sRole, null, true);

         role.setAttribute("read", "false");
         role.setAttribute("write", "true");
         role.setAttribute("create", "true");
      }
   }
}