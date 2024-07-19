// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.InputStream;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.text.SimpleDateFormat;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;

/**
 * Various system-related utilities for portability.
 */
public final class SysUtil
{
   // constants

   /**
    * Framework caption name.
    */
   public final static String CAPTION = "NexJ";

   /**
    * Framework namespace for various purposes.
    */
   public final static String NAMESPACE = "nexj";

   /**
    * Framework package name.
    */
   public final static String PACKAGE = NAMESPACE;

   /**
    * Framework property prefix. Only the system properties starting with
    * this prefix end up as configuration properties, without the prefix.
    */
   public final static String PROPERTY_PREFIX = NAMESPACE + '.';

   /**
    * Configuration property name.
    */
   public final static String CONFIG_PROPERTY = "config";

   /**
    * Enterprise property name.
    */
   public final static String ENTERPRISE_PROPERTY = "enterprise";

   /**
    * Default configuration file URL string.
    */
   public final static String DEFAULT_CONFIG_URL = '/' + NAMESPACE + "/default.config";

   /**
    * Build number URL string.
    */
   public final static String BUILD_NUMBER_URL = '/' + NAMESPACE + "/build.number";

   /**
    * The build number.
    */
   public final static long BUILD_NUMBER;

   /**
    * The build time.
    */
   public final static long BUILD_TIME;

   /**
    * The enterprise flag.
    */
   public final static boolean ENTERPRISE;

   /**
    * File separator string.
    */
   public final static String FILE_SEP = getSystemProperty("file.separator", "/");

   /**
    * Path separator string.
    */
   public final static String PATH_SEP = getSystemProperty("path.separator", ":");

   /**
    * Line separator string.
    */
   public final static String LINE_SEP = getSystemProperty("line.separator", "\n");

   /**
    * The application configuration properties.
    */
   private final static Properties s_configProperties = new Properties();

   /**
    * The config file name, for debugging purposes.
    */
   private final static String s_sConfigFileName;

   static
   {
      try
      {
         Properties properties = (Properties)AccessController.doPrivileged(
            new PrivilegedAction()
            {
               public Object run()
               {
                  return System.getProperties();
               }
            });

         for (Enumeration enm = properties.propertyNames(); enm.hasMoreElements();)
         {
            String sName = (String)enm.nextElement();

            if (sName.startsWith(PROPERTY_PREFIX))
            {
               s_configProperties.setProperty(sName.substring(PROPERTY_PREFIX.length()),
                  properties.getProperty(sName));
            }
         }
      }
      catch (SecurityException e)
      {
         try
         {

            String sConfig = getSystemProperty(PROPERTY_PREFIX + CONFIG_PROPERTY, null);

            if (sConfig != null)
            {
               s_configProperties.setProperty(CONFIG_PROPERTY, sConfig);
            }
         }
         catch (SecurityException x)
         {
         }
      }

      String sCfgFileName = s_configProperties.getProperty(CONFIG_PROPERTY, DEFAULT_CONFIG_URL);

      if (sCfgFileName.trim().length() == 0)
      {
         s_sConfigFileName = null;
      }
      else
      {
         try
         {
            // SysConfig.load(sCfgFile, s_configProperties);
            Class.forName("nexj.core.util.SysConfig").getMethod("load", new Class[]
            {
               String.class,
               Properties.class
            }).invoke(null, new Object[]
            {
               sCfgFileName,
               s_configProperties
            });
         }
         catch (ClassNotFoundException e)
         {
            // Failed to load configuration loader
            // Fall through.
         }
         catch (Exception e)
         {
            ObjUtil.rethrow(e);
         }

         s_sConfigFileName = sCfgFileName;
      }

      // Load build number information
      InputStream istream = null;
      long lBuildTime = 0;
      long lBuildNumber = 0;

      try
      {
         Properties properties = new Properties();
         URL url = SysUtil.class.getResource(BUILD_NUMBER_URL);

         if (url != null)
         {
            istream = URLUtil.openStream(url);
            properties.load(istream);

            for (Iterator itr = properties.entrySet().iterator(); itr.hasNext();)
            {
               Map.Entry entry = (Map.Entry)itr.next();

               s_configProperties.put(entry.getKey(), entry.getValue());
            }

            String sBuildNumber = s_configProperties.getProperty("build.number");

            if (sBuildNumber != null)
            {
               lBuildNumber = Long.parseLong(sBuildNumber);
            }

            String sBuildDate = s_configProperties.getProperty("build.date");

            if (sBuildDate != null)
            {
               SimpleDateFormat fmt = new SimpleDateFormat("yyyy'-'MM'-'dd' 'HH':'mm':'ss.SSSZ", Locale.ENGLISH);

               fmt.setTimeZone(TimeZone.getTimeZone("Etc/UTC"));
               lBuildTime = fmt.parse(sBuildDate).getTime();
            }
         }
      }
      catch (Exception e)
      {
         throw new UncheckedException("err.sys.configOpen", new Object[]{BUILD_NUMBER_URL}, e);
      }
      finally
      {
         IOUtil.close(istream);
      }

      boolean bEnterpriseEnabled = Boolean.parseBoolean(s_configProperties.getProperty(ENTERPRISE_PROPERTY, "true"));

      if (bEnterpriseEnabled)
      {
         bEnterpriseEnabled = (SysUtil.class.getClassLoader().getResource(NAMESPACE + "/core/util/cipher/MasterPasswordStreamCipher.class") != null);
      }

      BUILD_NUMBER = lBuildNumber;
      BUILD_TIME = lBuildTime;
      ENTERPRISE = bEnterpriseEnabled;
   }

   // constructors

   /**
    * Prevents instantiation.
    */
   protected SysUtil()
   {
   }

   // operations

   /**
    * Returns the configuration properties -
    * this is used strictly only for bootstrapping the framework,
    * the application components must NOT use this method.
    * The following system property affects the configuration:
    * nexj.server.config=[Config file URL (/nexj/default.server)]
    * If nexj.server.config is specified, but empty,
    * then the system properties are used only.
    * @returns The configuration properties.
    */
   public static Properties getConfigProperties()
   {
      return s_configProperties;
   }

   /**
    * @return The config file name, or null if using system properties.
    */
   protected static String getConfigFileName()
   {
      return s_sConfigFileName;
   }

   /**
    * Gets a system property in a privileged action.
    * @param sName The property name.
    * @param sDefaultValue The default property value.
    * @return The property value.
    */
   protected static String getSystemProperty(final String sName, final String sDefaultValue)
   {
      return (String)AccessController.doPrivileged(
         new PrivilegedAction()
         {
            public Object run()
            {
               return System.getProperty(sName, sDefaultValue);
            }
         });
   }
}
