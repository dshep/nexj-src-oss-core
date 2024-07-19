// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * J2EE container-related utility functions.
 */
public class J2EEUtil
{
   // constants

   /**
    * No J2EE container.
    */
   public final static int NONE = 0;

   /**
    * JBoss J2EE container.
    */
   public final static int JBOSS = 1;

   /**
    * Orion J2EE container.
    */
   public final static int ORION = 2;

   /**
    * WebLogic J2EE container.
    */
   public final static int WEBLOGIC = 3;

   /**
    * WebSphere J2EE container.
    */
   public final static int WEBSPHERE = 4;

   /**
    * Bootstrap J2EE container.
    */
   public final static int TEEE = 5;

   /**
    * The detected J2EE container constant.
    */
   public final static int CONTAINER;

   /**
    * True if the container provides minimal
    * functionality for running a component.
    */
   public final static boolean MINIMAL;

   /**
    * The detected J2EE container name.
    */
   public final static String CONTAINER_NAME;

   /**
    * Name for a node that is not part of a cluster.
    */
   public final static String ISOLATED_NODE_NAME = "node1";

   /**
    * Bootstrap container namespace for various purposes.
    */
   public final static String TEEE_NAMESPACE = "teee";

   /**
    * Bootstrap container package.
    */
   public final static String TEEE_PACKAGE_NAME = TEEE_NAMESPACE;


   protected final static String[] CONTAINER_NAMES = new String[]
   {
      null,
      "JBoss",
      "Orion",
      "WebLogic",
      "WebSphere",
      "Teee"
   };

   static
   {
      int nContainer = NONE;

      String[] sClassNameArray = new String[]
      {
         "org.jboss.Main",
         "com.evermind.server.ejb.SessionContainer",
         "weblogic.Server",
         "com.ibm.ws.runtime.component.EJBContainerImpl",
      };

      for (int i = 0; i < sClassNameArray.length; ++i)
      {
         try
         {
            Class.forName(sClassNameArray[i]);
         }
         catch (Throwable t)
         {
            continue;
         }

         nContainer = i + 1;
      }

      if (nContainer == NONE && Boolean.parseBoolean(SysUtil.getConfigProperties().getProperty("container.enabled", "false")))
      {
         nContainer = TEEE;
      }

      CONTAINER = nContainer;
      CONTAINER_NAME = getContainerName(nContainer);
      MINIMAL = CONTAINER == TEEE && Boolean.parseBoolean(SysUtil.getConfigProperties().getProperty("container.minimal", "false"));
   }

   /**
    * The host identifier, valid during the JVM process lifetime.
    */
   public final static Binary NODE_GUID = GUIDUtil.generateGUID();

   /**
    * The host identifier, valid during the JVM process lifetime.
    * Base64 version of NODE_GUID.
    */
   public final static String NODE_ID = NODE_GUID.toBase64();

   /**
    * The JNDI environment prefix.
    */
   public final static String JNDI_ENV_PREFIX = "java:comp/env/";

   /**
    * The minimal container mode flag.
    */
   protected static boolean s_bMinimal;

   // constructors

   /**
    * Prevents construction.
    */
   protected J2EEUtil()
   {
   }

   /**
    * Gets a container name by ordinal number.
    * @param nContainer The container constant.
    * @return The container name.
    */
   public static String getContainerName(int nContainer)
   {
      return CONTAINER_NAMES[nContainer];
   }

   /**
    * @return True if the code is executed in a container.
    */
   public static boolean isContained()
   {
      return CONTAINER != NONE && (CONTAINER != TEEE || !s_bMinimal);
   }

   /**
    * @return The minimal container mode flag.
    */
   public static boolean isMinimal()
   {
      return s_bMinimal;
   }

   /**
    * Sets the minimal container mode flag.
    * @param bMinimal The minimal container mode flag.
    */
   public static void setMinimal(boolean bMinimal)
   {
      s_bMinimal = bMinimal;
   }

   /**
    * Gets the platform name based on the current J2EE container.
    *
    * @return The platform name, capitalized.
    */
   public static String getPlatformName()
   {
      if (CONTAINER == NONE || CONTAINER == TEEE)
      {
         return "Generic";
      }

      return CONTAINER_NAME;
   }

   /**
    * Gets the absolute path to the config directory of the current J2EE
    * container.
    *
    * @return The absolute path to the config directory.
    */
   public static File getConfigDir()
   {
      switch (CONTAINER)
      {
         case JBOSS:
            try
            {
               return URLUtil.fileFromURL(new URL(System.getProperty("jboss.server.config.url")));
            }
            catch (MalformedURLException ex)
            {
               ObjUtil.rethrow(ex);
            }

            break;

         case WEBSPHERE:
            return new File(System.getProperty("was.install.root"), SysUtil.NAMESPACE);

         case TEEE:
            String sDeployDir = SysUtil.getConfigProperties().getProperty("deploy.dir");

            if (sDeployDir == null)
            {
               return new File(System.getProperty("user.home"), "." + SysUtil.NAMESPACE);
            }
            else
            {
               return new File(sDeployDir, "../conf").getAbsoluteFile();
            }


         default:
            throw new IllegalStateException("Unknown J2EE container");
      }

      return null;
   }

   /**
    * Gets the absolute path to the temporary directory of the current
    * J2EE container.
    *
    * @return The absolute path to the temporary directory.
    */
   public static File getTempDir()
   {
      String sTempDirPath = null;

      switch (CONTAINER)
      {
         case NONE:
         case TEEE:
            sTempDirPath = System.getProperty("java.io.tmpdir");
            break;

         case JBOSS:
            sTempDirPath = System.getProperty("jboss.server.temp.dir");
            break;

         case WEBSPHERE:
            sTempDirPath = System.getProperty("was.repository.temp");
            break;

         default:
            throw new IllegalStateException("Unknown J2EE container");
      }

      return new File(sTempDirPath, SysUtil.NAMESPACE);
   }

   /**
    * Gets the absolute path to the data directory of the current J2EE container.
    * @return The absolute path to the data directory.
    */
   public static File getDataDir()
   {
      File confDir = getConfigDir();

      switch (CONTAINER)
      {
         case JBOSS:
            return new File(confDir, "../data");

         case WEBSPHERE:
            return new File(confDir, "data");

         case TEEE:
            String sDeployDir = SysUtil.getConfigProperties().getProperty("deploy.dir");

            if (sDeployDir == null)
            {
               return new File(System.getProperty("user.home"), '.' + SysUtil.NAMESPACE);
            }
            else
            {
               return new File(sDeployDir, "/work").getAbsoluteFile();
            }

         default:
            throw new IllegalStateException("Unknown J2EE container");
      }
   }
}
