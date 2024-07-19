// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

/**
 * I/O utilities.
 */
public class IOUtil
{
   // constants

   /**
    * The character encoding to use with files.
    */
   public final static String ENCODING = "UTF-8";

   /**
    * Default buffer size.
    */
   public final static int BUFFER_SIZE = 2048; 
   
   // constructors
   
   /**
    * Prevents construction.
    */
   protected IOUtil()
   {
   }

   // operations

   /**
    * Opens a buffered reader from a file.
    * @param file The file from which to read.
    * @param sEncoding The file character encoding.
    * @return The buffered reader.
    * @throws IOException if an error occurs.
    */
   public static BufferedReader openBufferedReader(File file, String sEncoding) throws IOException
   {
      FileInputStream istream = new FileInputStream(file);

      try
      {
         BufferedReader reader = openBufferedReader(istream, sEncoding);

         istream = null;

         return reader;
      }
      finally
      {
         close(istream);
      }
   }

   /**
    * Opens a buffered reader from an input stream stream.
    * @param istream The input stream.
    * @param sEncoding The input stream character encoding.
    * @return The buffered reader.
    * @throws IOException if an error occurs.
    */
   public static BufferedReader openBufferedReader(InputStream istream, String sEncoding) throws IOException
   {
      if (!(istream instanceof BufferedInputStream))
      {
         istream = new BufferedInputStream(istream, BUFFER_SIZE);
      }

      return new BufferedReader(new InputStreamReader(istream, sEncoding), BUFFER_SIZE);
   }

   /**
    * Opens a buffered writer to a file.
    * @param file The file into which to write.
    * @param sEncoding The file character encoding.
    * @return The buffered writer.
    * @throws IOException if an error occurs.
    */
   public static BufferedWriter openBufferedWriter(File file, String sEncoding) throws IOException
   {
      FileOutputStream ostream = new FileOutputStream(file);

      try
      {
         BufferedWriter writer = openBufferedWriter(ostream, sEncoding);

         ostream = null;

         return writer;
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

            file.delete();
         }
      }
   }

   /**
    * Opens a buffered writer to an output stream.
    * @param ostream The output stream.
    * @param sEncoding The stream character encoding.
    * @return The buffered writer.
    * @throws IOException if an error occurs.
    */
   public static BufferedWriter openBufferedWriter(OutputStream ostream, String sEncoding) throws IOException
   {
      return openBufferedWriter(ostream, sEncoding, BUFFER_SIZE);
   }
   
   /**
    * Opens a buffered writer to an output stream.
    * @param ostream The output stream.
    * @param sEncoding The stream character encoding.
    * @param nBufferSize The buffer size.
    * @return The buffered writer.
    * @throws IOException if an error occurs.
    */
   public static BufferedWriter openBufferedWriter(OutputStream ostream, String sEncoding, int nBufferSize) throws IOException
   {
      if (!(ostream instanceof BufferedOutputStream))
      {
         ostream = new BufferedOutputStream(ostream, nBufferSize);
      }

      return new BufferedWriter(new OutputStreamWriter(ostream, sEncoding), nBufferSize);
   }

   /**
    * Copies a file to a given destination directory.
    * @param dstDir The destination directory.
    * @param srcFile The file to copy.
    * @param progress The progress listener. Can be null.
    * @throws IOException if an error occurs.
    */
   public static void copy(File dstDir, File srcFile, Progress progress) throws IOException
   {
      String sName = srcFile.getName();
      long lLength = srcFile.length();
      File dstFile = new File(dstDir, sName);
      File tmpFile = new File(dstDir, sName + ".tmp");
      FileOutputStream os = null;
      InputStream is = null;
      boolean bDone = false;

      try
      {
         is = new FileInputStream(srcFile);
         os = new FileOutputStream(tmpFile);
         os.getChannel().lock();
         IOUtil.copy(os, is, new byte[(int)Math.min(Math.max(lLength, 1), 1048576)], progress);
         bDone = true;
      }
      finally
      {
         IOUtil.close(is);

         if (os != null)
         {
            os.close();
         }

         if (!bDone)
         {
            tmpFile.delete();
         }
      }

      dstFile.delete();

      if (!tmpFile.renameTo(dstFile))
      {
         tmpFile.delete();
         
         throw new IOException("Unable to rename file \"" + tmpFile.getPath() + "\" to \"" + dstFile.getPath() + "\"");
      }
   }
   
   /**
    * Deletes a file with error checking.
    * @param file The file to delete.
    * @throws IOException if an error occurs.
    */
   public static void delete(File file) throws IOException
   {
      if (file.exists())
      {
         if (!file.delete())
         {
            throw new IOException("Unable to delete file \"" + file.getPath() + "\"");
         }
      }
   }
   
   /**
    * Deletes a file or recursively deletes a directory.
    * @param sFileName The file to delete.
    * @return True if and only if the file or directory is successfully deleted. False otherwise.
    */
   public static boolean deleteRecursively(File file)
   {
      return deleteRecursively(file, null);
   }

   /**
    * Recursively deletes files and/or directories matched by a file filter. 
    * @param file The file or directory to match and possibly delete. Directories are recursed.
    * @param fileFilter The filter to use for matching. null to match everything.
    * @return True if at least one occurrence of file was found AND deleted. False otherwise.
    */
   public static boolean deleteRecursively(File file, FileFilter fileFilter)
   {
      boolean bDeleted = false;
      
      if (file.isDirectory())
      {
         File[] childFileArray = file.listFiles();
         
         for (int i = 0; i < childFileArray.length; i++)
         {
            bDeleted |= deleteRecursively(childFileArray[i], fileFilter);
         }
      }
      
      return ((fileFilter == null || fileFilter.accept(file)) && file.delete()) | bDeleted;
   }

   /**
    * Copies the contents of the reader to the writer.
    * @param writer The writer.
    * @param reader The reader.
    * @throws IOException if an error occurs.
    */
   public static void copy(Writer writer, Reader reader) throws IOException
   {
      char[] buf = new char[128]; 

      for (;;)
      {
         int nCount = reader.read(buf);

         if (nCount <= 0)
         {
            break;
         }

         writer.write(buf, 0, nCount);
      }
   }

   /**
    * Copies the contents of the reader to the writer.
    * @param writer The writer.
    * @param reader The reader.
    * @param buf The buffer to use. Null to use the default size.
    * @throws IOException if an error occurs.
    */
   public static void copy(Writer writer, Reader reader, char[] buf) throws IOException
   {
      copy(writer, reader, buf, null);
   }
   
   /**
    * Copies the contents of the reader to the writer.
    * @param writer The writer.
    * @param reader The reader.
    * @param buf The buffer to use. Null to use the default size.
    * @param progress The progress listener. Can be null.
    * @throws IOException if an error occurs.
    */
   public static void copy(Writer writer, Reader reader, char[] buf, Progress progress) throws IOException
   {
      if (buf == null)
      {
         buf = new char[BUFFER_SIZE];
      }
      
      long lTotal = 0;
      
      for (;;)
      {
         int nCount = reader.read(buf);

         if (nCount <= 0)
         {
            break;
         }
            
         writer.write(buf, 0, nCount);
         
         if (progress != null)
         {
            lTotal += nCount;
            progress.progress(lTotal);
         }
      }
   }
   
   /**
    * Copies the contents of the input stream to the output stream.
    * @param ostream The output stream.
    * @param istream The input stream.
    * @throws IOException if an error occurs.
    */
   public static void copy(OutputStream ostream, InputStream istream) throws IOException
   {
      byte[] buf = new byte[256];

      for (;;)
      {
         int nCount = istream.read(buf);

         if (nCount <= 0)
         {
            break;
         }

         ostream.write(buf, 0, nCount);
      }
   }

   /**
    * Copies the contents of the input stream to the output stream.
    * @param ostream The output stream.
    * @param istream The input stream.
    * @param buf The buffer to use. Null to use the default size.
    * @throws IOException if an error occurs.
    */
   public static void copy(OutputStream ostream, InputStream istream, byte[] buf) throws IOException
   {
      IOUtil.copy(ostream, istream, buf, null);
   }

   /**
    * Copies the contents of the input stream to the output stream.
    * @param ostream The output stream.
    * @param istream The input stream.
    * @param buf The buffer to use. Null to use the default size.
    * @param progress The progress listener. Can be null.
    * @throws IOException if an error occurs.
    */
   public static void copy(OutputStream ostream, InputStream istream, byte[] buf, Progress progress) throws IOException
   {
      if (buf == null)
      {
         buf = new byte[BUFFER_SIZE];
      }
      
      long lTotal = 0;
      
      for (;;)
      {
         int nCount = istream.read(buf);

         if (nCount <= 0)
         {
            break;
         }

         ostream.write(buf, 0, nCount);
         
         if (progress != null)
         {
            lTotal += nCount;
            progress.progress(lTotal);
         }
      }
   }

   /**
    * Closes a reader safely.
    * @param reader The reader to close. Can be null.
    */
   public static void close(Reader reader)
   {
      if (reader != null)
      {
         try
         {
            reader.close();
         }
         catch (IOException e)
         {
         }
      }
   }
   
   /**
    * Closes an input stream safely.
    * @param istream The input stream to close. Can be null.
    */
   public static void close(InputStream istream)
   {
      if (istream != null)
      {
         try
         {
            istream.close();
         }
         catch (IOException e)
         {
         }
      }
   }

   // inner classes
   
   /**
    * Interface implemented by IO progress listeners
    */
   public static interface Progress
   {
      /**
       * Notifies about the transferred byte/char count.
       * @param lCount The count of bytes/chars transferred so far.
       */
      void progress(long lCount);
   }
}
