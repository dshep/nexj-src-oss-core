// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import javax.resource.spi.InvalidPropertyException;

import nexj.core.rpc.ra.TransactionalConsumerConfig;

/**
 * @see nexj.core.rpc.ra.GenericConsumerConfig
 */
public class FileConsumerConfig extends TransactionalConsumerConfig
{
   // attributes

   /**
    * The absolute path to the directory to scan for incoming message files. Path
    * should be specified in the host OS path format.
    */
   protected String m_sIncomingDirectory;

   /**
    * The number of subdirectory levels to which the incoming directory should be
    * scanned. Zero to scan only the incoming directory.
    */
   protected int m_nSubdirectoryLevels;

   /**
    * The absolute path to the directory in which to place incoming message files
    * after they have been processed. Null to delete those files. Path should be
    * specified in the host OS path format.
    */
   protected String m_sProcessedDirectory;

   /**
    * The path to the transactional journal directory used to record transaction
    * progress. If relative, the resource adapter uses the server node's data
    * directory as the root.
    * Path should be specified in the host OS path format.
    */
   protected String m_sJournalDirectory;

   /**
    * The scanning interval (in ms) between successive scans of the incoming directory.
    */
   protected long m_lScanInterval;

   /**
    * The minimum age (in ms) that an incoming message file must exceed before it
    * will be detected by the channel.
    */
   protected long m_lAge;

   /**
    * The like-pattern used to select incoming message files for processing.
    */
   protected String m_sPattern;


   // operations

   /**
    * Sets the absolute path to the directory to scan for incoming message files.
    * 
    * @param sIncoming The absolute path in the host OS path format.
    */
   public void setIncomingDirectory(String sIncoming)
   {
      m_sIncomingDirectory = sIncoming;
   }


   /**
    * Gets the absolute path to the directory to scan for incoming message files.
    * 
    * @return The absolute path in the host OS path format.
    */
   public String getIncomingDirectory()
   {
      return m_sIncomingDirectory;
   }


   /**
    * Sets the number of subdirectory levels to which the incoming directory
    * should be scanned.
    * 
    * @param nLevels The number of subdirectory levels; zero to scan only the
    *                incoming directory.
    */
   public void setSubdirectoryLevels(int nLevels)
   {
      m_nSubdirectoryLevels = nLevels;
   }


   /**
    * Gets the number of subdirectory levels to which the incoming directory
    * should be scanned.
    * 
    * @return The number of subdirectory levles; zero to scan only the
    *         incoming directory.
    */
   public int getSubdirectoryLevels()
   {
      return m_nSubdirectoryLevels;
   }


   /**
    * Sets the absolute path to the directory in which to place incoming message files
    * after they have been processed.
    * 
    * @param sProcessed The absolute path in the host OS path format; null to delete
    *                   message files after they have been processed.
    */
   public void setProcessedDirectory(String sProcessed)
   {
      m_sProcessedDirectory = sProcessed;
   }


   /**
    * Gets the absolute path to the directory in which to place incoming message files
    * after they have been processed.
    * 
    * @return The absolute path in the host OS path format, or null if message files
    *         should be deleted after they have been processed.
    */
   public String getProcessedDirectory()
   {
      return m_sProcessedDirectory;
   }


   /**
    * Sets the path to the transactional journal used by this adapter.
    * 
    * @param sJournal The path in the host OS path format.
    */
   public void setJournalDirectory(String sJournal)
   {
      m_sJournalDirectory = sJournal;
   }


   /**
    * Gets the path to the transactional journal used by this adapter.
    * 
    * @return The path in the host OS path format.
    */
   public String getJournalDirectory()
   {
      return m_sJournalDirectory;
   }


   /**
    * Sets the scanning interval between successive scans of the incoming directory.
    * 
    * @param lInterval The scanning interval, in ms.
    */
   public void setInterval(long lInterval)
   {
      m_lScanInterval = lInterval;
   }


   /**
    * Gets the scanning interval between successive scans of the incoming directory.
    * 
    * @return The scanning interval, in ms.
    */
   public long getInterval()
   {
      return m_lScanInterval;
   }


   /**
    * Sets the minimum age that an incoming file must exceed before it will be
    * detected by the channel.
    * 
    * @param lAge The minimum age, in ms.
    */
   public void setAge(long lAge)
   {
      m_lAge = lAge;
   }


   /**
    * Gets the minimum age that an incoming file must exceed before it will be
    * detected by the channel.
    * 
    * @return The minimum age, in ms.
    */
   public long getAge()
   {
      return m_lAge;
   }


   /**
    * Sets the pattern used to select incoming message files for processing.
    * 
    * @param sPattern The like-pattern used to select incoming message files.
    */
   public void setPattern(String sPattern)
   {
      m_sPattern = sPattern;
   }


   /**
    * Gets the pattern used to select incoming message files for processing.
    * 
    * @return The like-pattern used to select incoming message files.
    */
   public String getPattern()
   {
      return m_sPattern;
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerConfig#validate()
    */
   public void validate() throws InvalidPropertyException
   {
      super.validate();

      if (m_lAge < 0)
      {
         throw new InvalidPropertyException("Age cannot be negative");
      }

      if (m_nSubdirectoryLevels < 0)
      {
         throw new InvalidPropertyException("Subdirectory levels cannot be negative");
      }

      if (m_lScanInterval < 0)
      {
         throw new InvalidPropertyException("Scan interval cannot be negative");
      }
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerConfig#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append(super.toString());
      buf.append("(incoming=\"");
      buf.append(m_sIncomingDirectory);
      buf.append("\", levels=");
      buf.append(m_nSubdirectoryLevels);
      buf.append(", journal=\"");
      buf.append(m_sJournalDirectory);
      buf.append("\", processed=\"");
      buf.append(m_sProcessedDirectory);
      buf.append("\", interval=");
      buf.append(m_lScanInterval);
      buf.append("ms, age=");
      buf.append(m_lAge);
      buf.append(", pattern=\"");
      buf.append(m_sPattern);
      buf.append("\")");

      return buf.toString();
   }
}
