// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Locale;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataHelper.MixinNamespaceHandler;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;

/**
 * Ant task for generating root and base JAR file names.
 */
public class ProjectJarName extends Task
{
   //constants
   
   private final static String NAMESPACE_ATTR = "namespace";
   private final static String BASE_NAMESPACE_ATTR = "baseNamespace";
   private final static String REVISION_ATTR = "revision";
   private final static String VERSION_ATTR = "version";
   private final static String BASE_VERSION_ATTR = "baseVersion";
   private final static String BASE_CHECKSUM_ATTR = "baseChecksum";
   private final static String JAR_PREFIX = "NEXJ-META-";

   //instances

   private String m_sSrcDir; //Workspace directory.
   private String m_sOutDir; //Publish directory.
   private String m_sBaseDir; //Base JAR location directory.
   private String m_sProject; //Name of the project to retrieve Jar file names for.
   private String m_sMetaJarProperty; //Ant property that stores the root Jar file name.
   private String m_sMetaDir; //Ant property that stores the metadata directory name.
   private String m_sMetaPathProperty; // Ant property that stores the root folder URI.
   private String m_sBaseJarProperty; //Ant property that stores the base Jar file name.
   private String m_sBasePathProperty; // Ant property that stores the base folder URI.
   private String m_sMixinJarsProperty; // Ant property that stores the mixin Jar URLs.
   private String m_sChecksumProperty; //Ant property that stores the checksum.
   private String m_sVersionProperty; //Ant property that stores the checksum.
   private String m_sRevisionProperty; //Ant property that stores the project revision.
   private String m_sBaseProjectProperty; //Ant property that stores the base project name.
   private String m_sEnvironmentNameProperty; //Ant property that stores the environment name.
   private boolean m_bProfileEnabled; //Ant property that stores whether to output timing info
   
   //operations

   /**
    * Ant attribute setter for project name.
    */
   public void setProject(String sProject)
   {
      m_sProject = sProject;
   }

   /**
    * Ant attribute setter for property to store generated root Jar file.
    */
   public void setMetaJarProperty(String sMetaJarProperty)
   {
      m_sMetaJarProperty = sMetaJarProperty;
   }

   /**
    * Ant attribute setter for property to store generated base Jar file.
    */
   public void setBaseJarProperty(String sBaseJarProperty)
   {
      m_sBaseJarProperty = sBaseJarProperty;
   }
   
   /**
    * Ant attribute setter for property to store generated root folder URI.
    */
   public void setMetaPathProperty(String sMetaPathProperty)
   {
      m_sMetaPathProperty = sMetaPathProperty;
   }

   /**
    * Ant attribute setter for property to store generated base folder URI.
    */
   public void setBasePathProperty(String sBasePathProperty)
   {
      m_sBasePathProperty = sBasePathProperty;
   }

   /**
    * Ant attribute setter for property to store base project name.
    */
   public void setBaseProjectProperty(String sBaseProjectProperty)
   {
      m_sBaseProjectProperty = sBaseProjectProperty;
   }

   /**
    * Ant attribute setter for property to store the environment name..
    */
   public void setEnvironmentNameProperty(String sEnvironmentNameProperty)
   {
      m_sEnvironmentNameProperty = sEnvironmentNameProperty;
   }

   /**
    * Ant attribute setter for property to store mixin repository URLs.
    */
   public void setMixinJarsProperty(String sMixinJarsProperty)
   {
      m_sMixinJarsProperty = sMixinJarsProperty;
   }

   /**
    * Ant attribute setter for property to store generated checksum.
    */
   public void setChecksumProperty(String sChecksumProperty)
   {
      m_sChecksumProperty = sChecksumProperty;
   }

   /**
    * Ant attribute setter for property to store the parsed version.
    */
   public void setVersionProperty(String sVersionProperty)
   {
      m_sVersionProperty = sVersionProperty;
   }

   /**
    * Ant attribute setter for property to store the parsed revision.
    */
   public void setRevisionProperty(String sRevisionProperty)
   {
      m_sRevisionProperty = sRevisionProperty;
   }

   /**
    * Ant attribute setter for workspace directory.
    */
   public void setSrcDir(String sSrcDir)
   {
      m_sSrcDir = sSrcDir;
   }

   /**
    * Ant attribute setter for publish directory.
    */
   public void setOutDir(String sOutDir)
   {
      m_sOutDir = sOutDir;
   }

   /**
    * Ant attribute setter for base JAR directory.
    */
   public void setBaseDir(String sBaseDir)
   {
      m_sBaseDir = sBaseDir;
   }

   /**
    * Ant attribute setter for base JAR directory.
    */
   public void setProfile(String sProfileEnabled)
   {
      m_bProfileEnabled = StringUtil.parseBoolean(sProfileEnabled);
   }

   /**
    * Ant attribute setter for property to store metadata directory if not default.
    */
   public void setMetaDir(String sMetaDir)
   {
      m_sMetaDir = sMetaDir;
   }

   /**
    * Generate path to the meta data folder.
    *
    * @param sProject name of the project.
    * @return The path to the meta data folder for the specified project.
    */
   private String getMetaDir(String sProject)
   {
      if (m_sMetaDir != null)
      {
         return m_sMetaDir;
      }
      else if (sProject.equals(".tmp"))
      {
         return m_sOutDir + "/nexj/meta";
      }
      else
      {  
         return m_sSrcDir + '/' + sProject + "/meta";
      }
   }

   /**
    * Get the name of the JAR file that has been built already.
    *
    * @param sProject name of the project.
    * @param sChecksum checksum of the meta files.
    * @return Name of the existing JAR file with the specified the version. If the
    *   file does not exist then return null.
    */
   private String getPublishedJARName(String sProject, String sChecksum)
   {
      File publishDir = new File(m_sBaseDir);

      if (publishDir.exists() && publishDir.isDirectory())
      {
         File[] files = publishDir.listFiles();

         sProject = '-' + sProject.toUpperCase(Locale.ENGLISH) + '-';
         sChecksum = '-' + sChecksum.toUpperCase(Locale.ENGLISH);

         for (int i = 0; i < files.length; ++i)
         {
            File file = files[i];

            if (file.isFile())
            {
               String sName = file.getName().toUpperCase(Locale.ENGLISH);
               int nExtStart = sName.lastIndexOf('.');

               if (sName.startsWith(JAR_PREFIX) && nExtStart > JAR_PREFIX.length())
               {
                  sName = sName.substring(JAR_PREFIX.length() - 1, nExtStart);

                  if(sName.startsWith(sProject) &&
                     sName.endsWith(sChecksum))
                  {
                     String sPath = file.getAbsolutePath();

                     log("Found: " + sPath);

                     return sPath;
                  }
               }
            }
         }
      }
      else
      {
         log("Unknown dir: "+ publishDir.getPath());
      }

      return null;
   }

   /**
    * Generate a JAR name based on given version information.
    *
    * @param sProject name of the project.
    * @param sRevision revision info of the project.
    * @param sVersion version info of the project.
    * @param sChecksum checksum of the meta files.
    * @return JAR file name based on the version information.
    */
   private String getJarName(String sProject, String sRevision, String sVersion, String sChecksum)
   {
      StringBuilder sb = new StringBuilder();

      if (m_sOutDir != null)
      {
         sb.append(m_sOutDir);
         sb.append(File.separatorChar);
      }

      sb.append("nexj-meta-");
      int nFileNameIndex = sb.length();
      sb.append(sProject);
      sb.append("-");
      sb.append(sRevision);
      sb.append("-");
      sb.append(sVersion);
      sb.append("-");
      sb.append(sChecksum);

      for (int i = nFileNameIndex; i < sb.length(); i++)
      {
         if (sb.charAt(i) == ' ')
         {
            sb.setCharAt(i, '_');
         }
      }

      sb.append(".jar");
      return sb.toString();
   }

   /**
    * Generate a JAR file names from the Metadata and set them into Ant properties.
    *
    * @param sProject name of the project.
    */
   private void setJarNameProperties(String sProject) throws BuildException
   {
      try
      {
         URL rootURL = XMLMetadataHelper.getURL(URLUtil.toURL(getMetaDir(sProject)), true);
         URL baseURL = null;
         XMLMetadataHelper helper = new XMLMetadataHelper(rootURL, null, null, null);
         Element descriptorElement = helper.getDescriptorElement(true);
         String sRevision = descriptorElement.getAttribute(REVISION_ATTR);
         String sVersion = descriptorElement.getAttribute(VERSION_ATTR);
         String sBaseJarName = null;

         if (!descriptorElement.hasAttribute(BASE_NAMESPACE_ATTR) || sProject.equals(".tmp"))
         {
            if (m_sBaseJarProperty != null)
            {
               getProject().setProperty(m_sBaseJarProperty, "");
            }

            if (m_sBaseProjectProperty != null)
            {
               getProject().setProperty(m_sBaseProjectProperty, "");
            }
            
            if (m_sBasePathProperty != null)
            {
               getProject().setProperty(m_sBasePathProperty, "");
            }
         }
         else
         {
            //Parse the namespace URL to retrieve the base project name.
            String sBaseNamespace = descriptorElement.getAttribute(BASE_NAMESPACE_ATTR);
            String sBaseProject = sBaseNamespace.substring(sBaseNamespace.lastIndexOf('/') + 1);
            String sBaseVersion = descriptorElement.getAttribute(BASE_VERSION_ATTR);
            String sBaseChecksum = descriptorElement.getAttribute(BASE_CHECKSUM_ATTR);
            long lSearchTime, lStartTime = 0;

            if (m_bProfileEnabled)
            {
               lStartTime = System.currentTimeMillis();
            }

            log("Searching for base repository '" + sBaseProject + "' with checksum " + sBaseChecksum);

            if (m_sBaseJarProperty != null)
            {
               sBaseJarName = getProject().getProperty(m_sBaseJarProperty);
            }

            if (StringUtil.isEmpty(sBaseJarName))
            {
               sBaseJarName = getPublishedJARName(sBaseProject, sBaseChecksum);
            }

            if (m_bProfileEnabled)
            {
               lSearchTime = System.currentTimeMillis() - lStartTime;
               log("   Search complete (" + lSearchTime + " ms).");
            }

            if (sBaseJarName == null)
            {
               throw new BuildException("Missing base repository '" + sBaseProject + "' with version " +
                  sBaseVersion + " and checksum " + sBaseChecksum);
            }

            if (m_sBaseJarProperty != null)
            {
               getProject().setProperty(m_sBaseJarProperty, sBaseJarName);
            }

            if (m_sBaseProjectProperty != null)
            {
               getProject().setProperty(m_sBaseProjectProperty, sBaseProject);
            }
            
            String sBaseRepositoryPath = XMLMetadataHelper.getRepositoryPath(sBaseNamespace);

            if (m_sBasePathProperty != null)
            {
               getProject().setProperty(m_sBasePathProperty, sBaseRepositoryPath);
            }

            baseURL = getMetaFolderURL(sBaseJarName, sBaseRepositoryPath);
         }

         String sChecksum = null;
         
         try
         {
            helper = new XMLMetadataHelper(rootURL, baseURL, null, null);
            
            sChecksum = helper.getChecksum(true);
         }
         catch (MetadataException e)
         {
            if (baseURL != null)
            {
               // base may be old-style repository, rooted at /nexj/meta
               if (m_sBasePathProperty != null)
               {
                  getProject().setProperty(m_sBasePathProperty, "nexj/meta");
               }

               baseURL = getMetaFolderURL(sBaseJarName, "nexj/meta/");
               helper = new XMLMetadataHelper(rootURL, baseURL, null, null);
               
               sChecksum = helper.getChecksum(true);
            }
            else
            {
               throw e;
            }
         }

         // mixin JARs
         if (m_sMixinJarsProperty != null)
         {
            if (getProject().getProperty(m_sMixinJarsProperty) == null)
            {
               final StringBuffer mixinJarsBuffer = new StringBuffer();

               XMLMetadataHelper.forEachReferencedMixin(descriptorElement, baseURL, new MixinNamespaceHandler() 
               {
                  public URL handle(String sNamespace, String sVersion, String sChecksum)
                  {
                     String sMixinProject = sNamespace.substring(sNamespace.lastIndexOf('/') + 1);

                     log("Searching for mixin repository '" + sMixinProject + "' with checksum " + sChecksum);

                     String sMixinJAR = getPublishedJARName(sMixinProject, sChecksum);

                     if (sMixinJAR == null)
                     {
                        throw new BuildException("Missing mixin repository '" + sMixinProject + "' with version " +
                           sVersion + " and checksum " + sChecksum);
                     }

                     if (mixinJarsBuffer.length() > 0)
                     {
                        mixinJarsBuffer.append(File.pathSeparator);
                     }

                     mixinJarsBuffer.append(sMixinJAR);

                     try
                     {
                        URL metaFolderURL = getMetaFolderURL(sMixinJAR, XMLMetadataHelper.getRepositoryPath(sNamespace));

                        getProject().setProperty(SysUtil.PROPERTY_PREFIX + XMLMetadataHelper.getRepositoryProperty(sNamespace), metaFolderURL.toString());

                        return metaFolderURL;
                     }
                     catch (IOException ioe)
                     {
                        throw new BuildException("Unable to resolve metadata folder URL for mixin repository'" + sMixinProject + "' with version " +
                           sVersion + " and checksum " + sChecksum, ioe);
                     }
                  }
               });

               getProject().setProperty(m_sMixinJarsProperty, mixinJarsBuffer.toString());
            }
            else
            {
               log("Mixin property '" + m_sMixinJarsProperty + "' already set. Ignoring.", Project.MSG_DEBUG);
            }
         }

         log("Project '" + sProject + "': revision = " + sRevision + ", version = " + sVersion + ", checksum = " + sChecksum);

         if (getProject().getProperty(m_sMetaJarProperty) == null)
         {
            getProject().setProperty(m_sMetaJarProperty, getJarName(sProject, sRevision, sVersion, sChecksum));
         }
            
         if (m_sChecksumProperty != null)
         {
            getProject().setProperty(m_sChecksumProperty, sChecksum);
         }

         if (m_sVersionProperty != null)
         {
            getProject().setProperty(m_sVersionProperty, sVersion);
         }

         if (m_sRevisionProperty != null)
         {
            getProject().setProperty(m_sRevisionProperty, sRevision);
         }
         
         if (m_sMetaPathProperty != null)
         {
            getProject().setProperty(m_sMetaPathProperty, XMLMetadataHelper.getRepositoryPath(descriptorElement.getAttribute(NAMESPACE_ATTR)));
         }
         
         if (m_sEnvironmentNameProperty != null)
         {
            String sEnvName = getProject().getProperty(m_sEnvironmentNameProperty);
            
            getProject().setProperty(m_sEnvironmentNameProperty, (!StringUtil.isEmpty(sEnvName)) ?
               sEnvName : XMLMetadataHelper.getEnvironmentName(descriptorElement.getAttribute(NAMESPACE_ATTR)));
         }
      }
      catch (IOException e)
      {
         throw new BuildException("Cannot get URL for project " + sProject, e);
      }
   }

   /**
    * Check if all the mandatory Ant Task attributes are set.
    */
   private void checkAttributes()
   {
      if (m_sSrcDir == null)
      {
         throw new BuildException("Undefined srcdir attribute of " + getTaskName());
      }

      if (m_sProject == null)
      {
         throw new BuildException("Undefined project attribute of " + getTaskName());
      }

      if (m_sMetaJarProperty == null)
      {
         throw new BuildException("Undefined metajarproperty attribute of " + getTaskName());
      }

      if (m_sBaseDir == null)
      {
         // default to the publish directory.
         m_sBaseDir = m_sOutDir;
      }
   }

   /**
    * Returns the metadata URL for a given JAR file with repository path.
    * 
    * @param sJarFile the path string for the JAR file. 
    * @param sRepositoryPath the metadata path within the JAR file. 
    * @return a meta URL for a published jar.
    * @throws IOException if a problem is encountered creating the URL object.
    */
   public static URL getMetaFolderURL(String sJarFile, String sRepositoryPath) throws IOException
   {
      StringBuffer sb = new StringBuffer("jar:file:");

      sb.append(sJarFile);
      sb.append("!/" + sRepositoryPath);

      return XMLMetadataHelper.getURL(URLUtil.toURL(sb.toString()), true);
   }

   /**
    * Entry point for Ant Task.
    * Get Jar file names and set them in Ant properties.
    */
   public void execute() throws BuildException
   {
      checkAttributes();
      setJarNameProperties(m_sProject);
   }
}
