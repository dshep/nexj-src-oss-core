// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Properties;
import java.util.ArrayList;
import java.util.Set;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.DirSet;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.ResourceCollection;

import nexj.core.util.HashHolder;
import nexj.core.util.IOUtil;
import nexj.core.util.StringUtil;

/**
 * The dependstate task compares a set of sources with a state file. If any of
 * the state file entries, formatted as a .properties file, do not match or
 * have earlier modify times than the specified source files, the specified
 * target files are deleted, and the state file is recreated.
 * 
 * The specific criteria for deleting targets and recreating the state file
 * is:
 *  - No state file exists
 *  - A specified target file does not exist
 *  - A state file entry was not specified as a source file.
 *  - A specified source file has no entry in the state file.
 *  - A specified source file has an entry in the state file, however
 *    it has a later modify time than it's state file entry. 
 *  - The state file is improperly formatted.
 */
public class DependState extends Task
{
   /**
    * An ArrayList of ResourceCollections specifying the source files
    */
   private ArrayList m_srcResourceList  = new ArrayList();
   
   /**
    * An ArrayList of ResourceCollections specifying the target files
    */
   private ArrayList m_trgResourceList  = new ArrayList();
   
   /**
    * A string for the state file path. 
    */
   private String m_sStateFilePath;
   
   /**
    * Sets the state file's absolute path.
    * @param sStateFilePath the state file path.
    */
   public void setStateFile(String sStateFilePath) 
   {
      m_sStateFilePath = sStateFilePath;
   }
   
   /**
    * Add a set of source files.
    * @param fs the FileSet to add.
    */
   public void addSrcfileset(FileSet fs) 
   {
      m_srcResourceList.add(fs);
   }
 
   /**
    * Add a list of source files.
    * @param fl the FileList to add.
    */
   public void addSrcfilelist(FileList fl) 
   {
      m_srcResourceList.add(fl);
   }
 
   /**
    * Add a set of target files.
    * @param fs the FileSet to add.
    */
   public void addTargetfileset(FileSet fs) 
   {
      m_trgResourceList.add(fs);
   }
 
   /**
    * Add a list of target files.
    * @param fl the FileList to add.
    */
   public void addTargetfilelist(FileList fl) 
   {
      m_trgResourceList.add(fl);
   }

   /**
    * Add a set of target directories.
    * @param fl the FileList to add.
    */
   public void addTargetdirset(DirSet ds) 
   {
      m_trgResourceList.add(ds);
   }

   /**
    * @see org.apache.tools.ant.Task#execute()
    */
   public void execute()
   {      
      File stateFile;
      Collection sourceFileCollection; // of type File
      Collection targetFileCollection; // of type File
      Properties stateTable;
      
      // parameter checking
      if (StringUtil.isEmpty(m_sStateFilePath))
      {
         raiseBuildException("Invalid state file path.", null);
      }
      
      // state file
      stateFile = new File(m_sStateFilePath);
      
      if (stateFile.exists())
      {
         if (stateFile.isDirectory())
         {
            raiseBuildException("State file can not be a directory.", null);
         }
         
         if (!stateFile.canRead()) 
         {
            raiseBuildException("Source state file \""+ m_sStateFilePath +"\" cannot be read.", null);
         }
      }
      
      if (m_srcResourceList.isEmpty())
      {
         raiseBuildException("At least one <srcfileset> or <srcfilelist> element must be set", null);
      }
      
      if (m_trgResourceList.isEmpty())
      {
         raiseBuildException("At least one <targetfileset> or <targetfilelist> element must be set", null);
      }
      
      // get state file contents
      stateTable = parseStateFile(stateFile);
      
      // get source and targets
      sourceFileCollection = collectFiles(m_srcResourceList.iterator());
      targetFileCollection = collectFiles(m_trgResourceList.iterator());
      
      // take action
      if (!targetsExist(targetFileCollection))
      {
         log("Target file does not exist. Reset state.", Project.MSG_DEBUG);
         resetState(sourceFileCollection, collectFiles(m_trgResourceList.iterator()), stateFile);
      } 
      else if (stateTable == null)
      {
         log("State file \"" + stateFile.getAbsolutePath() + "\" does not exist. Reset state.", Project.MSG_DEBUG);
         resetState(sourceFileCollection, targetFileCollection, stateFile);
      }
      else if (!isUpToDate(sourceFileCollection, stateTable))
      {
         log("Files out of sync.", Project.MSG_DEBUG);
         resetState(sourceFileCollection, targetFileCollection, stateFile);
      } 
      // else state file exists, up to date
      // no changes
   }
   
   /**
    * Returns true if all specified target files exist in the
    * file system.
    * 
    * @param targetFileCollection A set of File objects, representing the target files.
    * @return if all specified target files exist.
    */
   private boolean targetsExist(Collection targetFileCollection)
   {
      File targetFile;
      
      Iterator fileIterator = targetFileCollection.iterator();
      
      while (fileIterator.hasNext())
      {
         targetFile = (File)fileIterator.next();

         if (!targetFile.exists())
         {
            log("Target file \"" + targetFile.getAbsolutePath() + "\" does not exist in file system.", Project.MSG_DEBUG);
            return false;
         }
      }

      return true;
   }

   /**
    * Resets the state of the source-target-state file(s), by
    * recreating the state file with the current source files and
    * deleting the target files.
    * 
    * @param sourceFileCollection Set of Files objects specifying the source files.
    * @param targetFileCollection Set of Files objects specifying the target files.
    * @param stateFile A File object representing the target file.
    */
   private void resetState(Collection sourceFileCollection, Collection targetFileCollection, File stateFile) 
   {
      deleteFiles(targetFileCollection);
      recreateStateFile(sourceFileCollection, stateFile);
   }
   
   /**
    * Deletes the target files.
    * @param targetFileCollection A Set of File objects specifying the target files.
    */
   private void deleteFiles(Collection targetFileCollection)
   {
      File targetFile;

      log("Deleting target files...", Project.MSG_DEBUG);

      Iterator fileIterator = targetFileCollection.iterator();

      while (fileIterator.hasNext())
      {
         targetFile = (File)fileIterator.next();

         if (targetFile.isDirectory())
         {
            deleteContents(targetFile);
         }
         else
         {
            targetFile.delete();
         }
      }
   }

   /**
    * Deletes a file or non-empty directory and it's contents.
    * @param targetFile File or directory to delete.
    */
   private void deleteContents(File targetDir)
   {
      log("Deleting: " + targetDir.getPath(), Project.MSG_DEBUG);

      if (!(targetDir.exists() && targetDir.isDirectory()))
      {
         return;
      }

      File[] contents = targetDir.listFiles();
      File targetFile;
      int nIndex;

      for (nIndex = 0; nIndex < contents.length; nIndex++) 
      {
         targetFile = contents[nIndex];
         
         if (targetFile.exists()) {
            deleteContents(targetFile);
            contents[nIndex].delete();
         }
      }
   }

   /**
    * Compares the source files to the state file entries and returns false
    * if there is no 1-1 correlation, or the state file does not exist or
    * is poorly formatted.
    * 
    * @param sourceFileCollection Set of Files objects specifying the source files.
    * @param stateTable A Properties object specifying the state file entries.
    * @return true if the source files and only the source files each have an entry in the state file with an older or equal file system modify time stamp to the entry modify time stamp.
    */
   private boolean isUpToDate(Collection sourceFileCollection, Properties stateTable) 
   {
      File srcFile;
      Long stateTime;
      Iterator fileIterator = sourceFileCollection.iterator();
      
      while (fileIterator.hasNext())
      {
         srcFile = (File)fileIterator.next();
         
         if (srcFile.exists())
         {
            String sTime = (String)stateTable.remove(srcFile.getAbsolutePath());
            String sType = srcFile.isFile()? "File" : "Directory";
            
            if (sTime == null)
            {
               log(sType + " \"" + srcFile.getAbsolutePath() + "\" has no entry in the state file.", Project.MSG_DEBUG);
               return false; //new source file
            }
            
            try
            {               
               stateTime = new Long(sTime);
            }
            catch (NumberFormatException nfe)
            {
               log("Invalid time offset \"" + sTime + "\" for " + sType.toLowerCase(Locale.ENGLISH) + " \"" + srcFile.getAbsolutePath() + "\".", Project.MSG_DEBUG);               
               return false;
            }
            
            // check mod times
            if (srcFile.lastModified() != stateTime.longValue())
            {
               log(sType + " \"" + srcFile.getAbsolutePath() + "\" has been modifed.", Project.MSG_DEBUG);
               return false;
            }            
         }
         else
         {
            // source file specified in FileList does not exist
            log("\"" + srcFile.getAbsolutePath() + "\" does not exist in file system.", Project.MSG_DEBUG);
            return false;
         }
      }
     
      if (stateTable.size() != 0) //does the state file have more than sources?
      {
         log("More state entries found than source files/folders.", Project.MSG_DEBUG);
         return false;       
      }
      
      return true;
   }

   /**
    * Parses the state file into a Properties object.
    * 
    * @param stateFile A File object specifying the state file. It does not have to exist.
    * @return A Properties object of the state file entries.
    */
   private Properties parseStateFile(File stateFile)
   {
      Properties sourceTable;
      
      if (!stateFile.exists())
      {
         return null; // no state file to read, so return empty
      }
      
      log("Found state file: " + stateFile.getAbsolutePath(), Project.MSG_DEBUG);

      sourceTable = new Properties();
      FileInputStream fiStream = null;
      
      try 
      {
         sourceTable.load(new BufferedInputStream(fiStream = new FileInputStream(stateFile)));
      }
      catch (IOException ioe)
      {
         raiseBuildException(ioe.getMessage(), ioe);
      }
      finally
      {
         IOUtil.close(fiStream);
      }
      
      return sourceTable;
   }
   
   /**
    * Creates or recreates the statefile using the specified source files,
    * in a key=value properties format.
    * 
    * @param sourceFileCollection Set of Files objects specifying the source files.
    * @param stateFile A File object specifying the state file. It does not have to exist.
    */
   private void recreateStateFile(Collection sourceFileCollection, File stateFile)
   {      
      Properties stateTable = new Properties();
      File srcFile;
      String sParentDirPath;
      OutputStream ostream = null;
      
      stateFile.delete();
      sParentDirPath = stateFile.getParent();
      
      // confirm statefile path
      if (sParentDirPath != null)
      {
         File parentDir = new File(sParentDirPath);
         
         if (!(parentDir.mkdirs() || parentDir.exists()))
         {
            raiseBuildException("Parent directory \"" + sParentDirPath + "\" for state file does not exist.", null);
         }
      }
            
      try
      {
         Iterator fileIterator = sourceFileCollection.iterator();
         
         while (fileIterator.hasNext())
         {
            srcFile = (File)fileIterator.next();
            
            if (srcFile.exists())
            {
               stateTable.setProperty(srcFile.getAbsolutePath(), String.valueOf(srcFile.lastModified()));
            }
            // else filelist entry non-present, no action
         }
         
         stateTable.store(new BufferedOutputStream(ostream = new FileOutputStream(stateFile, false)), "Generated by DependState");

         log("Created state file: " + stateFile.getAbsolutePath(), Project.MSG_DEBUG);
      }
      catch (IOException ioe)
      {
         raiseBuildException("Error creating state file \"" + stateFile.getAbsolutePath() + "\".", ioe);
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
               raiseBuildException("Could not close state file \"" + stateFile.getAbsolutePath() + "\".", e);
            }
         }
      }
      
   }
   
   /**
    * Returns a Vector of File objects specified in a series of 
    * ResourceCollection objects.  
    * 
    * @param resourceColItr An iterator over ResourceCollection instances
    * @return A Set of files
    */
   private Collection collectFiles(Iterator resourceColItr)
   {
      Set fileSet = new HashHolder();
      
      // collect using a TreeSet to weed out duplicates
      while (resourceColItr.hasNext()) 
      {
         ResourceCollection rc = (ResourceCollection)resourceColItr.next();
         File file, dir = null;
         String[] sNameArray = null;
         
         int nIndex;

         if (rc instanceof FileSet)
         {
            FileSet fset = (FileSet)rc;
            sNameArray = fset.getDirectoryScanner(getProject()).getIncludedFiles();
            dir = fset.getDir();
         }
         else if (rc instanceof FileList)
         {
            FileList flist = (FileList)rc;
            sNameArray = flist.getFiles(getProject());
            dir = flist.getDir(getProject());
         }
         else if (rc instanceof DirSet)
         {
            DirSet dset = (DirSet)rc;
            dir = dset.getDir(getProject());

            if (dir.exists())
            {
               DirectoryScanner ds = dset.getDirectoryScanner(getProject());
               sNameArray = ds.getIncludedFiles();
               fileSet.add(dir);
            }
         }
         else
         {
            raiseBuildException("Unknown ResourceCollection: " + rc.getClass().getCanonicalName(), null);
         }

         if (sNameArray == null || dir == null)
         {
            continue;
         }

         for (nIndex = 0; nIndex < sNameArray.length; nIndex++) 
         {
            file = new File(dir, sNameArray[nIndex]);
            fileSet.add(file);

            // get parent dirs between target file and dir (fileset directory)
            for (;;)
            {
               file = file.getParentFile();

               if (file.equals(dir))
               {
                  break;
               }

               fileSet.add(file);
            }
         }
      }

      return fileSet;
   }   
   
   /**
    * From WixWriter.java
    * Throws a BuildException, printing additional details to stdout.
    * 
    * Where a BuildException will only convey the error message to the 
    * ant output log, raiseBuildException will print the stack
    * trace and indicate where in the java class our exception was raised
    * (line number and call flow). 
    * 
    * @param sMessage The build error.
    * @param e The cause exception.
    * @throws BuildException to halt the build.
    */
   private void raiseBuildException(String sMessage, Exception e) throws BuildException
   {
      log(sMessage, Project.MSG_ERR); // log to stdout

      if (e != null) 
      { 
         e.printStackTrace(); 
      }

      throw new BuildException(sMessage, e);
   }
   
}
