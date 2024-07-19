// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.Locale;
import java.util.Properties;

import org.w3c.dom.Element;

import nexj.core.admin.Installer;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.IOUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * Application server setup tool.
 */
public class SetupTool extends GenericTool
{
   // constants
   
   /**
    * Custom installer properties. 
    */
   protected final static String[] CUSTOM_PROPERTIES = new String[]
   {
      "jms.bus",
      "installer.cmd.ssh.get",
      "installer.cmd.ssh.put",
      "installer.cmd.ssh.del",
      "installer.cmd.jsch.knownHostsFile",
      "installer.cmd.jsch.identityFile",
   };

   // associations
   
   /**
    * The installer instance.
    */
   protected Installer m_installer;
   
   // operations
   
   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      Properties configProperties = SysUtil.getConfigProperties();
      MetadataEnabledProperties metadataEnabledProperties = new MetadataEnabledProperties(configProperties);
      CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();

      dispatcher.init(metadataEnabledProperties);

      if (m_installer == null)
      {
         String sConfig = URLUtil.toURL(getRequiredProperty("cfg.url"));
         InputStream istream = null;
         InputStreamReader encReader = null;
         Reader decReader = null;
         Element root;

         try
         {
            istream = new BufferedInputStream(URLUtil.openStream(new URL(sConfig)));
            encReader = new InputStreamReader(istream, "UTF-8");
            decReader = dispatcher.createDecryptedReader(encReader);
            root = XMLUtil.parse(decReader, XMLMetadataHelper.DEFAULT_SCHEMA_URL_DEQUE).getDocumentElement();
         }
         finally
         {
            IOUtil.close(decReader);
            IOUtil.close(encReader);
            IOUtil.close(istream);
         }

         String sPlatform = XMLUtil.getReqStringAttr(root, "type");
         String sLocation = XMLUtil.getReqStringAttr(root, "location");
         String sUser = XMLUtil.getStringAttr(root, "user");
         String sPassword = XMLUtil.getStringAttr(root, "password");

         m_installer = (Installer)Class.forName(SysUtil.PACKAGE + ".core.admin.platform." +
            sPlatform.toLowerCase(Locale.ENGLISH) + "." + sPlatform + "Installer").newInstance();

         if (getProperty("base.url") != null)
         {
            configProperties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, URLUtil.toURL(getProperty("base.url")));
         }

         if (getProperty("con.url") != null)
         {
            configProperties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, URLUtil.toURL(getProperty("con.url")));
         }

         configProperties.setProperty(SysUtil.CONFIG_PROPERTY, sConfig);

         MetadataLoaderDispatcher loaderDispatcher = new MetadataLoaderDispatcher();
         
         Metadata metadata = loaderDispatcher.load(
            URLUtil.toURL(getRequiredProperty("root.url")),
            configProperties, MetadataLoader.ENVIRONMENT_ONLY, null);

         metadataEnabledProperties.setMetadataLoaderProperties(metadata.getProperties());
         m_installer.setMetadata(metadata);

         for (int i = 0; i != CUSTOM_PROPERTIES.length; ++i)
         {
            String sName = CUSTOM_PROPERTIES[i];
            String sValue = getProperty(sName);
            
            if (sValue != null)
            {
               metadataEnabledProperties.setProperty(sName, sValue);
            }
         }

         // Decrypt the password
         sPassword = dispatcher.decrypt(sPassword);

         // Prepare for encryption
         if (metadataEnabledProperties.getProperty("cipher.scheme") == null)
         {
            metadataEnabledProperties.setProperty("cipher.scheme", metadata.getEncryptionScheme());
         }

         dispatcher.init(metadataEnabledProperties);

         m_installer.setProperties(metadataEnabledProperties);
         m_installer.setLocation(sLocation, sUser, sPassword);
      }

      if (sCommand.equals("deploy"))
      {
         m_installer.deploy(getRequiredProperty("ear.file"));
      }
      else if (sCommand.equals("undeploy"))
      {
         m_installer.undeploy(getRequiredProperty("ear.file"));
      }
      else if (sCommand.equals("install"))
      {
         m_installer.install();
      }
      else if (sCommand.equals("uninstall"))
      {
         m_installer.uninstall();
      }
      else
      {
         throw new IllegalArgumentException("Invalid command \"" + sCommand + "\"");
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Droot.url=<root repository URL>",
         "-Dbase.url=<base repository URL>",
         "-Dcfg.url=<configuration URL>",
         "-Dcon.url=<connections URL>",
         "-Dear.file=<EAR file name>",
         "-Djms.bus=<integration bus name>",
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "deploy    - deploys an EAR file",
         "undeploy  - undeploys an EAR file",
         "install   - installs the resources in the server",
         "uninstall - uninstalls the resources from the server",
      };
   }

   public static void main(String[] args)
   {
      new SetupTool().run(args);
   }
   
   // inner classes

   protected static class MetadataEnabledProperties extends Properties
   {
      /**
       * Serialization UID.
       */
      private final static long serialVersionUID = -59128242864313548L;

      protected Properties m_loaderProperties;

      public MetadataEnabledProperties(Properties defaults)
      {
         super(defaults);
      }
      
      public void setMetadataLoaderProperties(Properties properties)
      {
         m_loaderProperties = properties;
      }

      public String getProperty(String key)
      {
         Object value = super.get(key);

         if (value == null || !(value instanceof String))
         {
            value = defaults.getProperty(key);
         }

         if (value == null && m_loaderProperties != null)
         {
            value = m_loaderProperties.getProperty(key);

            if (StringUtil.isEmpty((String)value))
            {
               return null;
            }
         }

         return (String)value;
      }
   }
}
