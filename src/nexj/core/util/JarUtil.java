// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/**
 * Jar-related utilities. 
 */
public final class JarUtil
{
   // constructors

   /**
    * Prevents construction.
    */
   protected JarUtil()
   {
   }

   // operations

   /**
    * Extracts jar files into a given folder.
    * 
    * @param jarFile The jar file.
    * @param sFilter The regular expression filter.
    * @param extractFolder The extraction folder.
    * @param extractedList Return list of extracted files absolute paths (null to ignore).
    */
   public static void extractFiles(File jarFile, String sFilter, File extractFolder, List extractedList)
   throws ZipException, IOException
   {
      ZipFile zipFile = null;
      
      try
      {
         Pattern filterPattern = Pattern.compile(sFilter);
         zipFile = new ZipFile(jarFile);

         for (Enumeration e = zipFile.entries(); e.hasMoreElements();)
         {
            ZipEntry entry = (ZipEntry)e.nextElement();
            String sEntryName = entry.getName().toLowerCase(Locale.ENGLISH);
            Matcher matcher = filterPattern.matcher(sEntryName);

            if (matcher.find() && matcher.groupCount() > 0)
            {
               File outputFile = new File(extractFolder, new File(sEntryName).getName());

               if (outputFile.createNewFile())
               {
                  OutputStream os = null;
                  InputStream is = null;

                  try
                  {
                     os = new BufferedOutputStream(new FileOutputStream(outputFile));
                     is = new BufferedInputStream(zipFile.getInputStream(entry));

                     IOUtil.copy(os, is);
                  }
                  finally
                  {
                     IOUtil.close(is);

                     if (os != null)
                     {
                        os.close();
                     }
                  }
               }

               if (extractedList != null)
               {
                  extractedList.add(outputFile);
               }
            }
         }
      }
      finally
      {
         if (zipFile != null)
         {
            zipFile.close();
         }
      }
   }
}