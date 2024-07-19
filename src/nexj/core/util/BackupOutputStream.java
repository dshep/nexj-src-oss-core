// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Output stream filter that copies its contents to a backup stream.
 */
public class BackupOutputStream extends OutputStream
{
   // associations
   
   /**
    * The destination output stream.
    */
   protected OutputStream m_outputStream;
   
   /**
    * The backup output stream.
    */
   protected OutputStream m_backupStream;

   // constructors
   
   /**
    * Constructs the output stream.
    * @param outputStream The output stream. Can be null.
    * @param backupStream The backup stream. Can be null.
    */
   public BackupOutputStream(OutputStream outputStream, OutputStream backupStream)
   {
      m_outputStream = outputStream;
      m_backupStream = backupStream;
   }

   // operations

   /**
    * Sets the destination output stream.
    * @param outputStream The destination output stream to set.
    */
   public void setOutputStream(OutputStream outputStream)
   {
      m_outputStream = outputStream;
   }

   /**
    * @return The destination output stream.
    */
   public OutputStream getOutputStream()
   {
      return m_outputStream;
   }
   
   /**
    * Sets the backup output stream.
    * @param backupStream The backup output stream to set.
    */
   public void setBackupStream(OutputStream backupStream)
   {
      m_backupStream = backupStream;
   }

   /**
    * @return The backup output stream.
    */
   public OutputStream getBackupStream()
   {
      return m_backupStream;
   }
   
   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int n) throws IOException
   {
      if (m_outputStream != null)
      {
         m_outputStream.write(n);
      }
      
      if (m_backupStream != null)
      {
         m_backupStream.write(n);
      }
   }

   /**
    * @see java.io.OutputStream#write(byte[])
    */
   public void write(byte[] buf) throws IOException
   {
      if (m_outputStream != null)
      {
         m_outputStream.write(buf);
      }
      
      if (m_backupStream != null)
      {
         m_backupStream.write(buf);
      }
   }

   /**
    * @see java.io.OutputStream#write(byte[], int, int)
    */
   public void write(byte[] buf, int nOff, int nLen) throws IOException
   {
      if (m_outputStream != null)
      {
         m_outputStream.write(buf, nOff, nLen);
      }
      
      if (m_backupStream != null)
      {
         m_backupStream.write(buf, nOff, nLen);
      }
   }

   /**
    * @see java.io.OutputStream#flush()
    */
   public void flush() throws IOException
   {
      if (m_outputStream != null)
      {
         m_outputStream.flush();
      }
      
      if (m_backupStream != null)
      {
         m_backupStream.flush();
      }
   }

   /**
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
      if (m_outputStream != null)
      {
         m_outputStream.close();
      }
      
      if (m_backupStream != null)
      {
         m_backupStream.close();
      }
   }
}
