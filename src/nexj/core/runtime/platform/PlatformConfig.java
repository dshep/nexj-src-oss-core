// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform;

import java.io.File;
import java.io.IOException;
import java.util.Observable;

/**
 * Interface implemented by platform configuration lookup classes.
 */
public abstract class PlatformConfig extends Observable
{
   // attributes

   /**
    * The unique HTTP node name used for session affinity.
    */
   protected String m_sHTTPNode;

   /**
    * The HTTP session cookie name.
    */
   protected String m_sHTTPSessionCookie = "JSESSIONID";

   /**
    * The HTTP session URL field.
    */
   protected String m_sHTTPSessionURLField = "jsessionid";

   /**
    * The HTTP node delimiter in HTTP messages.
    */
   protected char m_chHTTPNodeDelimiter = '.';

   /**
    * True if the data directory has already been created.
    */
   protected boolean m_bDataDirectoryCreated; 

   // associations

   /**
    *  The server data direcory.
    */
   protected File m_dataDirectory;

   // operations

   /**
    * Gets the directory for data files on the currently running process.
    * @return The data file directory.
    */
   public synchronized File getDataDirectory()
   {
      return m_dataDirectory;
   }

   /**
    * Creates the data directory, if it does not exist already.
    * @return The data directory object.
    * @throws IOException if the directory cannot be created.
    */
   public File createDataDirectory() throws IOException
   {
      File dir = getDataDirectory();

      synchronized (this)
      {
         if (!m_bDataDirectoryCreated)
         {
            if (!dir.exists() && !dir.mkdirs())
            {
               throw new IOException("Cannot create data directory: " + dir.toString());
            }

            m_bDataDirectoryCreated = true;
         }
      }

      return dir;
   }

   /**
    * @return The unique HTTP node name used for session affinity.
    */
   public synchronized String getHTTPNode()
   {
      return m_sHTTPNode;
   }

   /**
    * @return The HTTP node delimiter in HTTP messages.
    */
   public synchronized char getHTTPNodeDelimiter()
   {
      return m_chHTTPNodeDelimiter;
   }

   /**
    * @return The HTTP session cookie name.
    */
   public synchronized String getHTTPSessionCookie()
   {
      return m_sHTTPSessionCookie;
   }

   /**
    * @return The HTTP session URL field.
    */
   public synchronized String getHTTPSessionURLField()
   {
      return m_sHTTPSessionURLField;
   }
}
