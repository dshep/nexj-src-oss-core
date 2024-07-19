// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.j2ee;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Properties;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.xml.DefaultXMLMetadataExporter;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.IOUtil;
import nexj.core.util.IndentingXMLWriter;
import nexj.core.util.J2EEUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SubstReader;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * J2EE descriptor file exporter.
 */
public class J2EEDescriptorExporter
{
   // constants
   
   /**
    * Array of macro variable names with indexes corresponding
    * to the XMLMetadataExporter.J2EE_* constants.
    */
   protected final static String[] MACRO_NAME_ARRAY = new String[]
   {
      "distributable",
      "confidential",
      "session-timeout",
      "context-root",
      "web-login-config",
      "resource-ref",
      "resource-env-ref",
      "message-driven",
      "container-transaction",
      "test-filter",
      "test-filter-mapping",
      "persistence-filter",
      "persistence-filter-mapping",
      "session-listener",
      "portlets",
      "platform-resource-ref",
      "platform-resource-env-ref",
      "platform-resource-ref-ext",
      "platform-message-driven",
      "platform-message-driven-ext",
      "platform-activation-spec",
      "platform-connection-factory",
      "platform-admin-object",
      "platform-invoker-proxy-binding",
      "platform-replication-config",
      "platform-cluster-config",
      "platform-persistence-interceptor",
      "platform-auth-interceptor",
      "platform-classpath",
      "portlet-instances"
   };

   // operations

   /**
    * Gets a reader for a J2EE descriptor by preprocessing a template.
    *
    * @param templateReader The reader from which the template shall be read.
    * @param properties The environment properties.
    * @param metadata The root metadata object.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the XMLPersistenceMetadataExporter.J2EE_CONTEXT_* constants.
    * @return A reader on the J2EE descriptor.
    * @throws java.io.IOException if an error occurs.
    */
   public Reader getDescriptorReader(Reader templateReader, final Properties properties, final Metadata metadata,
      final int nContainer, final int nContext) throws IOException
   {
      return new DescriptorSubstReader(templateReader, properties, metadata, nContainer, nContext);
   }

   /**
    * Exports a J2EE descriptor by preprocessing a template.
    * @param writer The writer to which the J2EE descriptor shall be written.
    * @param templateReader The reader from which the template shall be read.
    * @param properties The environment properties.
    * @param metadata The root metadata object.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the XMLPersistenceMetadataExporter.J2EE_CONTEXT_* constants.
    * @throws java.io.IOException if an error occurs.
    */
   public void exportDescriptor(Writer writer, Reader templateReader, final Properties properties,
      final Metadata metadata, final int nContainer, final int nContext) throws IOException
   {
      Reader reader = null;

      try
      {
         reader = getDescriptorReader(templateReader, properties, metadata, nContainer, nContext);

         IOUtil.copy(writer, reader);
      }
      finally
      {
         IOUtil.close(reader);
      }
   }

   /**
    * Exports a J2EE descriptor by preprocessing a template.
    * @param dir The destination directory.
    * @param sFile The template and the destination file name.
    * @param properties The environment properties.
    * @param metadata The root metadata object.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the XMLPersistenceMetadataExporter.J2EE_CONTEXT_* constants.
    * @param bOverwrite True to overwrite a file that exists.
    * @throws java.io.IOException if an error occurs.
    */
   protected void exportDescriptor(File dir, String sFile, Properties properties, final Metadata metadata,
      final int nContainer, final int nContext, boolean bOverwrite) throws IOException
   {
      File out = new File(dir, sFile);
      Reader reader = null;
      Writer writer = null;
      boolean bSuccess = false;

      if (!bOverwrite && out.exists())
      {
         return;
      }

      try
      {
         File parent = out.getParentFile();

         if (parent != null && !parent.equals(dir))
         {
            parent.mkdirs();
         }

         InputStream instream = null;

         try
         {
            instream = URLUtil.openResource(getClass(), sFile);
         }
         catch (IOException e)
         {  // don't export if template cannot be found.
            return;
         }

         reader = new InputStreamReader(new BufferedInputStream(instream), XMLUtil.ENCODING);
         writer = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(out)), XMLUtil.ENCODING);
         exportDescriptor(writer, reader, properties, metadata, nContainer, nContext);
         bSuccess = true;
      }
      finally
      {
         IOUtil.close(reader);

         if (writer != null)
         {
            try
            {
               writer.close();

               if (!bSuccess)
               {
                  out.delete();
               }
            }
            catch (IOException e)
            {
            }
         }
      }
   }

   /**
    * Exports all the metadata descriptors to a given directory.
    * @param sDir The destination directory.
    * @param properties The environment properties.
    * @param metadata The root metadata object.
    * @param bOverwrite True to overwrite a file that exists.
    * @throws java.io.IOException if an error occurs.
    */
   public void exportDescriptor(String sDir, final Properties properties, final Metadata metadata, final boolean bOverwrite) throws IOException
   {
      final File dir = new File(sDir);

      export(new Exporter()
      {
         public void export(String sFile, int nContainer, int nContext) throws IOException
         {
            exportDescriptor(dir, sFile, properties, metadata, nContainer, nContext, bOverwrite);
         }
      });
   }

   /**
    * Exports all the metadata descriptors to a given directory.
    * @param sDir The destination directory.
    * @param sRoot The root repository directory or URL.
    * @param sBase The base repository directory or URL.
    * @param sConfig The server config file or URL.
    * @param sConnections The connections file or URL.
    * @param bOverwrite True to overwrite a file that exists.
    * @param properties Metadata loader properties.
    * @throws java.io.IOException if an error occurs.
    */
   public void exportDescriptor(final String sDir, String sRoot, String sBase,
      String sConfig, String sConnections, boolean bOverwrite, Properties properties) throws IOException
   {
      if (!bOverwrite)
      {
         final boolean[] missing = new boolean[1];

         export(new Exporter()
         {
            public void export(String sFile, int nContainer, int nContext) throws IOException
            {
               if (!new File(sDir, sFile).exists())
               {
                  missing[0] = true;
               }
            }
         });

         if (!missing[0])
         {
            return;
         }
      }

      properties = new Properties(properties);

      if (sBase != null)
      {
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, URLUtil.toURL(sBase));
      }

      properties.setProperty(SysUtil.CONFIG_PROPERTY, URLUtil.toURL(sConfig));
      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, URLUtil.toURL(sConnections));

      exportDescriptor(sDir, properties, new MetadataLoaderDispatcher().load(
         URLUtil.toURL(sRoot), properties, MetadataLoader.ENVIRONMENT_ONLY | MetadataLoader.ENCRYPTED |
         MetadataLoader.PROPERTIES, null), bOverwrite);
   }

   /**
    * Exports all the files.
    * @param exporter The exporter implementation.
    * @throws IOException if an error occurs.
    */
   protected void export(Exporter exporter) throws IOException
   {
      // EJBs
      exporter.export("ejb-jar.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_EJB);
      exporter.export("jboss.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_EJB);
      exporter.export("teee.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_EJB);

      exporter.export("ibm-ejb-jar-bnd.xmi", J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_EJB);
      exporter.export("ibm-ejb-jar-ext.xmi", J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_EJB);

      // Servlets
      exporter.export("web.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("jboss-web.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("context.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("ibm-web-bnd.xmi", J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("ibm-web-ext.xmi", J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_WEB);

      // Servlets with SSL client certificate authentication
      exporter.export("cert/web.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_WEB_CERT);
      exporter.export("cert/jboss-web.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB_CERT);
      exporter.export("cert/context.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB_CERT);

      // Anonymous web context (flat web, HTTP integration, generic RPC)
      exporter.export("anon/web.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("anon/jboss-web.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("anon/context.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("anon/ibm-web-ext.xmi", J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_WEB);

      // Forms-Based-Auth web context
      exporter.export("form/web.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("form/jboss-web.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("form/context.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_WEB);
      exporter.export("form/ibm-web-ext.xmi", J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_WEB);

      // Portlets
      exporter.export("portlet.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_PORTLET);
      exporter.export("portlet-instances.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_PORTLET);

      // Embedded RAs

      // File RA
      exporter.export("ra/file/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/file/jboss-ra-ds.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/file/teee-ra.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // JMS RA
      exporter.export("ra/jms/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/jms/jboss-ra-ds.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/jms/teee-ra.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // Mail RA
      exporter.export("ra/mail/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/mail/jboss-ra-ds.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/mail/teee-ra.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // SQL RA
      exporter.export("ra/sql/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/sql/teee-ra.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // Timer RA
      exporter.export("ra/timer/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // Tomcat RA
      exporter.export("ra/tomcat/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // TCP RA
      exporter.export("ra/tcp/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/tcp/jboss-ra-ds.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/tcp/teee-ra.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // Object Queueing RA
      exporter.export("ra/queueing/META-INF/ra.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/queueing/jboss-ra-ds.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_RA);
      exporter.export("ra/queueing/teee-ra.xml", J2EEUtil.TEEE, XMLMetadataExporter.J2EE_CONTEXT_RA);

      // Application
      exporter.export("application.xml", J2EEUtil.NONE, XMLMetadataExporter.J2EE_CONTEXT_APP);
      exporter.export("jboss-app.xml", J2EEUtil.JBOSS, XMLMetadataExporter.J2EE_CONTEXT_APP);
      exporter.export("ibmconfig/cells/defaultCell/applications/defaultApp/deployments/defaultApp/deployment.xml",
         J2EEUtil.WEBSPHERE, XMLMetadataExporter.J2EE_CONTEXT_APP);
   }

   // inner classes

   private interface Exporter
   {
      public void export(String sFile, int nContainer, int nContext) throws IOException;
   }

   /**
    * A SubstReader that processes J2EEDescriptor macros, including the conditional "ifenterprise" macro.
    */
   private class DescriptorSubstReader extends SubstReader
   {
      // attributes

      /**
       * The J2EE container for which to generate descriptors.
       */
      private int m_nContainer;

      /**
       * The context in which the descriptor will be used.
       */
      private int m_nContext;

      // associations

      /**
       * The metadata for which to generate descriptors.
       */
      private Metadata m_metadata;
      
      /**
       * The environment properties.
       */
      private Properties m_properties;
      
      // constructors

      /**
       * @param templateReader Reader of the descriptor template.
       * @param metadata The metadata for which to generate descriptors.
       * @param properties The environment properties.
       * @param nContainer The container for which to generate descriptors.
       * @param nContext The context in which the descriptors will be used.
       * 
       */
      public DescriptorSubstReader(Reader templateReader, Properties properties, Metadata metadata, int nContainer, int nContext)
      {
         super(templateReader);

         m_properties = properties;
         m_metadata = metadata;
         m_nContainer = nContainer;
         m_nContext = nContext;
      }

      private String getNestedValue(String sValue) throws IOException
      {
         Reader nested = new DescriptorSubstReader(new StringReader(sValue), m_properties, m_metadata, m_nContainer, m_nContext);
         StringBuffer buf = new StringBuffer(sValue.length());
         int ch;

         while ((ch = nested.read()) != -1)
         {
            buf.append((char)ch);
         }

         return buf.toString();
      }

      /**
       * Get the value of a macro expansion.
       * @param sName The macro to expand.
       * @return The expansion.
       */
      protected String getValue(String sName) throws IOException
      {
         int j = sName.indexOf(':');
         String sNamespace = "";

         if (j > 0)
         {
            sNamespace = sName.substring(j + 1);
            sName = sName.substring(0, j);
         }

         if (sName.equals("ifenterprise"))
         {
            if (SysUtil.ENTERPRISE)
            {
               return getNestedValue(sNamespace);
            }

            return "";
         }
         
         if (sName.equals("environment"))
         {
            return m_metadata.getEnvironment();
         }

         if (sName.equals("if"))
         {
            j = sNamespace.indexOf(':');

            if (j > 0)
            {
               String sPropertyName = sNamespace.substring(0, j);

               if (StringUtil.parseBoolean(m_properties.getProperty(sPropertyName, "false")))
               {
                  return getNestedValue(sNamespace.substring(j + 1));
               }
            }

            return "";
         }

         if (sName.equals("ifcomponent"))
         {
            j = sNamespace.indexOf(':');

            if (j > 0)
            {
               String sComponentName = sNamespace.substring(0, j);

               if (m_metadata.findComponent(sComponentName) != null)
               {
                  return getNestedValue(sNamespace.substring(j + 1));
               }
            }

            return "";
         }

         if (sName.equals("ifmodule"))
         {
            j = sNamespace.indexOf(':');

            if (j > 0)
            {
               if (!J2EEUtil.MINIMAL)
               {
                  return getNestedValue(sNamespace.substring(j + 1));
               }
            }

            return "";
         }

         if (sName.equals("ifcontainer"))
         {
            j = sNamespace.indexOf(':');

            if (j > 0)
            {
               boolean bNot = sNamespace.length() != 0 && sNamespace.charAt(0) == '!';

               if (sNamespace.substring((bNot) ? 1 : 0, j).equalsIgnoreCase(
                  m_properties.getProperty("type", "Generic")) != bNot)
               {
                  return getNestedValue(sNamespace.substring(j + 1));
               }
            }

            return "";
         }

         for (int i = 0; i < MACRO_NAME_ARRAY.length; ++i)
         {
            if (MACRO_NAME_ARRAY[i].equals(sName))
            {
               StringWriter sw = new StringWriter();
               IndentingXMLWriter writer = new IndentingXMLWriter(sw);
               
               new DefaultXMLMetadataExporter(writer).exportJ2EEDescriptor(m_metadata, i, sNamespace, m_nContainer, m_nContext);
                                             
               writer.close();

               return sw.toString();
            }
         }

         throw new IOException("Invalid macro variable name \"" + sName + "\"");
      }
   }
}
