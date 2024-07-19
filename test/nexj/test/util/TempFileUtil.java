// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.test.util;

import java.io.File;
import java.io.IOException;

import nexj.core.util.RandUtil;

/**
 * Utility class for the creation of temporary files and directories. 
 */
public class TempFileUtil
{
   // constants

   /**
    * The name of the directory (in the system temporary directory) that will be
    * used to store all temporary files and directories that are created by
    * this class.
    */
   protected final static String NEXJ_DIRECTORY_NAME = "nexj_unittest";


   // associations

   /**
    * The directory in which the temporary files and directories created by
    * this method will be located. It will be named NEXJ_DIRECTORY_NAME and
    * will be located in the system temporary directory specified by the
    * system property "java.io.tmpdir".
    */
   protected static File s_nexjTemporaryDirectory;


   // operations

   /**
    * Generates a unique name prefixed by the given string.
    * 
    * @param sPrefix The string to use as a prefix, or null if no prefix
    *                is desired.
    * @return A unique name.
    */
   public static String generateUniqueName(String sPrefix)
   {
      long lRandom = RandUtil.getSecureRandom().nextLong();
      
      if (sPrefix == null)
      {
         return String.valueOf(Math.abs(lRandom));
      }
      else
      {
         return sPrefix + "_" + String.valueOf(Math.abs(lRandom));
      }
   }

   /**
    * Gets the directory to use to store all temporary files and directories
    * created as part of running the NexJ unit test suite. It will be named
    * NEXJ_DIRECTORY_NAME and will be located in the system temporary
    * directory specified by the system property "java.io.tmpdir".
    * 
    * @return The root temporary directory.
    * @throws IOException
    */
   public synchronized static File getNexjTempDirectory() throws IOException
   {
      if (s_nexjTemporaryDirectory != null)
      {
         return s_nexjTemporaryDirectory;
      }
      
      String sSystemTemp = System.getProperty("java.io.tmpdir");
      File result = new File(sSystemTemp, NEXJ_DIRECTORY_NAME);
      
      if (result.exists())
      {
         deleteRecursive(result, result.getCanonicalPath());
      }
      
      //Might not have been deleted completely, but tests should execute regardless
      if (!result.exists())
      {
         if (!result.mkdir())
         {
            throw new IOException("Could not create temporary directory \"" + result.toString() + "\"");
         }

      }
      
      if (!result.canWrite())
      {
         throw new IOException("Temporary directory \"" + result.toString() + "\" not writable.");
      }
      
      s_nexjTemporaryDirectory = result;
      
      return result;
   }

   /**
    * Creates a uniquely-named temporary file in the root temporary directory.
    * 
    * @return The File object pointing to a newly-created, empty file.
    * @throws IOException
    */
   public static File makeTemporaryFile() throws IOException
   {
      return makeTemporaryFile(null);
   }

   /**
    * Creates a uniquely-named temporary file in the root temporary directory.
    * 
    * @param sPrefix A String to prefix the file name; null if no prefix is
    *                desired.
    * @return The File object pointing to a newly-created, empty file.
    * @throws IOException
    */
   public static File makeTemporaryFile(String sPrefix) throws IOException
   {
      File resultFile = new File(getNexjTempDirectory(), generateUniqueName(sPrefix));
      
      if (!resultFile.createNewFile())
      {
         throw new IOException("Could not create temporary file \"" + resultFile.toString() + "\"");
      }
      
      return resultFile;
   }

   /**
    * Creates a uniquely-named temporary directory inside the root temporary
    * directory.
    * 
    * @return The File object pointing to a newly-created, empty directory.
    * @throws IOException
    */
   public static File makeTemporaryDirectory() throws IOException
   {
      return makeTemporaryDirectory(null);
   }

   /**
    * Creates a uniquely-named temporary directory inside the root temporary
    * directory.
    * 
    * @param sPrefix A String to prefix the directory name; null if no prefix
    *                is desired.
    * @return The File object pointing to a newly-created, empty directory.
    * @throws IOException
    */
   public static File makeTemporaryDirectory(String sPrefix) throws IOException
   {
      File resultFile = new File(getNexjTempDirectory(), generateUniqueName(sPrefix));
      
      if (!resultFile.mkdir())
      {
         throw new IOException("Could not create temporary directory \"" + resultFile.toString() + "\"");
      }
      
      return resultFile;
   }

   
   /**
    * Deletes the given directory by deleting all files and directories in it,
    * recursively.
    * 
    * @param dir The directory to delete.
    * @param sRootPath The canonical path to the root of the deletion tree, used
    *                  to ensure that deletions do not happen outside of the
    *                  desired directory.
    */
   protected static void deleteRecursive(File dir, String sRootPath) throws IOException
   {
      if (!dir.isDirectory())
      {
         return;
      }
      
      //Ensure that all deletions happen underneath the root path.
      if (!dir.getCanonicalPath().startsWith(sRootPath))
      {
         return;
      }
      
      File[] entries = dir.listFiles();
      
      if (entries == null)
      {
         return;
      }
      
      for (int i = 0; i < entries.length; i++)
      {
         File entry = entries[i];
         
         if (entry.isDirectory())
         {
            deleteRecursive(entry, sRootPath);
         }
         
         entry.delete();
      }
   }
}
