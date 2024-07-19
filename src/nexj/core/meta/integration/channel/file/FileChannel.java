// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.file;

import java.nio.charset.Charset;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.TransactionalChannel;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * The File Channel Adapter is for transactional message delivery and pickup to/from
 * directories on the filesystem.
 */
public class FileChannel extends TransactionalChannel
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
   protected int m_nSubdirectoryLevels = 0;

   /**
    * The absolute path to the directory in which to place incoming message files
    * after they have been processed. Null to delete those files. Path should be
    * specified in the host OS path format.
    */
   protected String m_sProcessedDirectory;

   /**
    * The absolute path to the directory in which to place outgoing message files.
    * Path should be specified in the host OS path format.
    */
   protected String m_sOutgoingDirectory;

   /**
    * The absolute path to the directory used for preparing outgoing messages.
    * Path should be specified in the host OS path format.
    */
   protected String m_sOutgoingTempDirectory;

   /**
    * The path to the transactional journal directory used by this adapter.
    * If this path is a relative path, then the resource adapter uses
    * the server node's data directory as the root.
    * Path should be specified in the host OS path format.
    */
   protected String m_sJournalPath;

   /**
    * A template string used to automatically generate file names for the outgoing
    * message files.
    */
   protected String m_sOutgoingName = "${ts}_${seq}";

   /**
    * The minimum age (in ms) that an incoming message file must exceed before it
    * will be detected by the channel.
    */
   protected long m_lMinimumAge = 15000L;

   /**
    * The scanning interval (in ms) between successive scans of the incoming directory.
    */
   protected long m_lScanInterval = 30000L;

   /**
    * The default user.
    */
   protected String m_sDefaultUser;

   /**
    * The like-pattern used to select incoming message files for processing.
    */
   protected String m_sIncomingPattern = "*";

   /**
    * The character set encoding to use.
    */
   protected String m_sEncoding;

   /**
    * A template string used to generate a new name for incoming files after they have been
    * moved to the processed directory.
    */
   protected String m_sProcessedName = "${orig}_${ts}";
   
   /**
    * The maximum number of sender connections to have open at one time.
    */
   protected int m_nMaxSenders = 16;
   
   /**
    * The maximum number of consumer threads.
    */
   protected int m_nMaxReceivers = 4;

   // constructors

   /**
    * Constructs the file channel metadata.
    * @param sName The file channel name.
    */
   public FileChannel(String sName)
   {
      super(sName);
   }


   // operations

   /**
    * Sets the absolute path to the directory to scan for incoming message files.
    * 
    * @param sIncoming The absolute path in the host OS path format.
    */
   public void setIncomingDirectory(String sIncoming)
   {
      verifyNotReadOnly();
      
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
      verifyNotReadOnly();
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
      verifyNotReadOnly();
      
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
    * Sets the absolute path to the directory in which to place outgoing message files.
    * 
    * @param sOutgoing The absolute path in the host OS path format.
    */
   public void setOutgoingDirectory(String sOutgoing)
   {
      verifyNotReadOnly();
      
      m_sOutgoingDirectory = sOutgoing;
   }


   /**
    * Gets the absolute path to the directory in which to place outgoing message files.
    * 
    * @return The absolute path in the host OS path format.
    */
   public String getOutgoingDirectory()
   {
      return m_sOutgoingDirectory;
   }


   /**
    * Sets the absolute path to the directory used for preparing outgoing messages.
    * 
    * @param sTemp The absolute path in the host OS path format.
    */
   public void setTemporaryDirectory(String sTemp)
   {
      verifyNotReadOnly();
      
      m_sOutgoingTempDirectory = sTemp;
   }


   /**
    * Gets the absolute path to the directory used for preparing outgoing messages.
    * 
    * @return The absolute path in the host OS path format.
    */
   public String getTemporaryDirectory()
   {
      return m_sOutgoingTempDirectory;
   }


   /**
    * Sets the absolute path to the transactional journal directory used by this adapter.
    * 
    * @param sJournal The absolute path in the host OS path format.
    */
   public void setJournalPath(String sJournal)
   {
      verifyNotReadOnly();
      
      m_sJournalPath = sJournal;
   }


   /**
    * Gets the absolute path to the transactional journal directory used by this adapter.
    * 
    * @return The absolute path in the host OS path format; null if unspecified.
    */
   public String getJournalPath()
   {
      return m_sJournalPath;
   }


   /**
    * Sets the pattern used to select incoming message files for processing.
    * 
    * @param sPattern The like-pattern used to select incoming message files.
    */
   public void setPattern(String sPattern)
   {
      verifyNotReadOnly();
      
      m_sIncomingPattern = sPattern;
   }


   /**
    * Gets the pattern used to select incoming message files for processing.
    * 
    * @return The like-pattern used to select incoming message files.
    */
   public String getPattern()
   {
      return m_sIncomingPattern;
   }


   /**
    * Sets the template string used to automatically generate file names for the outgoing
    * message files.
    * 
    * @param sAutoname The template string used to generate outgoing file names.
    */
   public void setOutgoingName(String sAutoname)
   {
      verifyNotReadOnly();
      
      m_sOutgoingName = sAutoname;
   }


   /**
    * Gets the template string used to automatically generate file names for the outgoing
    * message files.
    * 
    * @return The template string used to generate outgoing file names.
    */
   public String getOutgoingName()
   {
      return m_sOutgoingName;
   }


   /**
    * Sets the minimum age that an incoming file must exceed before it will be
    * detected by the channel.
    * 
    * @param lAge The minimum age, in ms.
    */
   public void setAge(long lAge)
   {
      verifyNotReadOnly();
      
      m_lMinimumAge = lAge;
   }


   /**
    * Gets the minimum age that an incoming file must exceed before it will be
    * detected by the channel.
    * 
    * @return The minimum age, in ms.
    */
   public long getAge()
   {
      return m_lMinimumAge;
   }


   /**
    * Sets the scanning interval between successive scans of the incoming directory.
    * 
    * @param lInterval The scanning interval, in ms.
    */
   public void setInterval(long lInterval)
   {
      verifyNotReadOnly();
      
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
    * Sets the default user.
    * 
    * @param sDefaultUser The default user id.
    */
   public void setDefaultUser(String sDefaultUser)
   {
      verifyNotReadOnly();
      
      m_sDefaultUser = sDefaultUser;
   }


   /**
    * Gets the default user.
    * 
    * @return The default user id.
    */
   public String getDefaultUser()
   {
      return m_sDefaultUser;
   }


   /**
    * Sets the character encoding to use when reading/writing files.
    * 
    * @param sEncoding The Java charset name.
    */
   public void setEncoding(String sEncoding)
   {
      verifyNotReadOnly();

      if (sEncoding != null)
      {
         try
         {
            Charset.forName(sEncoding);
         }
         catch (IllegalArgumentException e)
         {
            throw new MetadataException("err.meta.encoding", new Object[]{sEncoding});
         }
      }

      m_sEncoding = sEncoding;
   }


   /**
    * Gets the character encoding to use when reading/writing files.
    * 
    * @return A Java charset name.
    */
   public String getEncoding()
   {
      return m_sEncoding;
   }


   /**
    * Sets the template string to be used to generate a new name for incoming files
    * after they have been moved to the processed directory.
    * 
    * @param sProcessedName The template string used to generate names for processed files.
    */
   public void setProcessedName(String sProcessedName)
   {
      verifyNotReadOnly();
      
      m_sProcessedName = sProcessedName;
   }


   /**
    * Gets the template string to be used to generate a new name for incoming files
    * after they have been moved to the processed directory.
    * 
    * @return The template string used to generate names for processed files.
    */
   public String getProcessedName()
   {
      return m_sProcessedName;
   }


   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return false;
   }


   /**
    * Check metadata configuration for errors and resolves the references.
    * @throws MetadataException if an error is encountered.
    */
   public void resolve() throws MetadataException
   {
      if (m_sIncomingDirectory == null)
      {
         setReceivable(false);
      }

      if (m_sOutgoingDirectory == null)
      {
         setSendable(false);
      }

      if (m_sOutgoingDirectory != null && StringUtil.isEmpty(m_sOutgoingTempDirectory))
      {
         throw new MetadataException("err.meta.integration.file.missingOutgoingTempDirectory",
            new Object[] {getName()});
      }

      if (m_sOutgoingDirectory != null && m_sOutgoingDirectory.equals(m_sOutgoingTempDirectory))
      {
         throw new MetadataException("err.meta.integration.file.outgoingAndTempMustBeDifferent",
            new Object[] {getName()});
      }

      if (m_sProcessedDirectory != null && StringUtil.isEmpty(m_sProcessedName))
      {
         throw new MetadataException("err.meta.integration.file.missingProcessedName",
            new Object[] {getName()});
      }
   }

   /**
    * Gets the directory in which to store the transaction journal for outgoing
    * messages. If the journal path property was specified, then the outgoing
    * journal path is rooted there. If the journal path property was not
    * specified, then a default relative path is returned. This relative path
    * should be rooted at the server node's data directory.
    * 
    * @return The path to the outgoing journal directory. If not absolute, it
    *         is relative to the server node's data directory.
    */
   public String getOutgoingJournalPath()
   {
      if (m_sJournalPath != null)
      {
         return m_sJournalPath + SysUtil.FILE_SEP + "out";
      }
      
      return "channels" + SysUtil.FILE_SEP + m_sName + SysUtil.FILE_SEP + "out";
   }

   /**
    * Gets the directory in which to store the transaction journal for incoming
    * messages. If the journal path property was specified, then the incoming
    * journal path is rooted there. If the journal path property was not
    * specified, then a default relative path is returned. This relative path
    * should be rooted at the server node's data directory.
    * 
    * @return The path to the incoming journal directory. If not absolute, it
    *         is relative to the server node's data directory.
    */
   public String getIncomingJournalPath()
   {
      if (m_sJournalPath != null)
      {
         return m_sJournalPath + SysUtil.FILE_SEP + "in";
      }
      
      return "channels" + SysUtil.FILE_SEP + m_sName + SysUtil.FILE_SEP + "in";
   }
   
   /**
    * Sets the maximum number of open physical connections.
    * @param nMaxSenders The maximum number of sender connections to have open at one time.
    */
   public void setMaxSenders(int nMaxSenders)
   {
      verifyNotReadOnly();
      m_nMaxSenders = nMaxSenders;
   }

   /**
    * @return The maximum number of sender connections to have open at one time.
    */
   public int getMaxSenders()
   {
      return m_nMaxSenders;
   }
   
   /**
    * Sets the maximum number of consumer threads.
    * @param nMaxReceivers The maximum number of consumer threads to set.
    */
   public void setMaxReceivers(int nMaxReceivers)
   {
      verifyNotReadOnly();
      m_nMaxReceivers = nMaxReceivers;
   }

   /**
    * @return The maximum number of consumer threads.
    */
   public int getMaxReceivers()
   {
      return m_nMaxReceivers;
   }
}
