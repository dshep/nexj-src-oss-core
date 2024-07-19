// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.FileFilter;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.util.HashHolder;
import nexj.core.util.Logger;

/**
 * Manages the pool of FileConsumer instances.
 */
public class FileConsumerPool extends GenericConsumerPool
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileConsumerPool.class);

   /**
    * The managed connection factory to give to FileConsumers to allow them
    * to create file managed connections. The factory is configured by the
    * connect() method, using the settings in m_config.
    */
   protected FileManagedConnectionFactory m_managedConnectionFactory;

   /**
    * Only file names matching this pattern will be detected by listen() and
    * dispatched to FileConsumers. Pattern is loaded and compiled only once,
    * by the connect() method, and is re-used throughout the lifetime of
    * the pool.
    */
   protected Pattern m_listenNamePattern;

   /**
    * A set of files that could not be attached by the consumer. This set is used
    * to prevent repeated log messages about the same files.
    */
   protected Set m_problemFileNameSet;


   // constructors

   /**
    * Constructs the consumer pool.
    * 
    * @param adapter The resource adapter.
    * @param factory The endpoint factory.
    * @param config The pool configuration.
    * @throws ResourceException if an initialization error occurs.
    */
   public FileConsumerPool(FileResourceAdapter adapter, MessageEndpointFactory factory,
      FileConsumerConfig config) throws ResourceException
   {
      super(adapter, factory, config, s_logger);
   }


   // operations

   /**
    * Gets the managed connection factory. To be used by FileConsumer objects
    * so that they can create a managed connection for performing file operations.
    * 
    * @return The managed connection factory.
    */
   public FileManagedConnectionFactory getManagedConnectionFactory()
   {
      return m_managedConnectionFactory;
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#close()
    */
   protected void close()
   {
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#connect()
    */
   protected void connect() throws Throwable
   {
      FileConsumerConfig config = (FileConsumerConfig)m_config;

      m_managedConnectionFactory = new FileManagedConnectionFactory();
      m_managedConnectionFactory.setResourceAdapter(m_adapter);

      m_managedConnectionFactory.setJournalDirectory(config.getJournalDirectory());
      m_managedConnectionFactory.setInputConnection(true);
      m_managedConnectionFactory.setIncomingDirectory(config.getIncomingDirectory());
      m_managedConnectionFactory.setProcessedDirectory(config.getProcessedDirectory());

      m_listenNamePattern = Pattern.compile(((FileConsumerConfig)m_config).getPattern());
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      return new FileConsumer(this);
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isTransactional()
    */
   public boolean isTransactional()
   {
      return true;
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isSynchronized()
    */
   protected boolean isSynchronized()
   {
      return false;
   }


   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#stop()
    */
   protected void stop()
   {
   }


   /**
    * Looks for new files in the incoming directory and dispatches FileConsumer
    * instances to deal with them.
    * 
    * Must be as lightweight as possible, performing a minimum amount of operations.
    * 
    * @see nexj.core.rpc.ra.GenericConsumerPool#listen()
    */
   protected void listen()
   {
      final long lListenStartTime = System.currentTimeMillis();
      final FileConsumerConfig config = (FileConsumerConfig)m_config;
      File incomingDirectory = new File(config.getIncomingDirectory());

      //Scan directory for new files
      final Matcher nameMatcher = m_listenNamePattern.matcher("");
      FileFilter filter = new FileFilter()
      {
         public boolean accept(File candidate)
         {
            //Get all subdirectories, regardless of name/age
            if (candidate.isDirectory())
            {
               return true;
            }

            //Only pick up files older than the configured age.
            if ((lListenStartTime - candidate.lastModified()) < config.getAge())
            {
               return false;
            }

            //Only pick up files whose names match the pattern
            nameMatcher.reset(candidate.getName());

            return nameMatcher.matches();
         }
      };

      processDirectory(incomingDirectory, 0, filter);

      //Wait until next scan period start.
      try
      {
         long lListenEndTime = System.currentTimeMillis();
         long lTimeElapsed = lListenEndTime - lListenStartTime;
         long lTimeRemaining = config.getInterval() - lTimeElapsed;

         if (lTimeRemaining > 0)
         {
            synchronized (this)
            {
               wait(lTimeRemaining);
            }
         }
      }
      catch (InterruptedException ex)
      {
         //Return early
      }
   }


   /**
    * Process the given directory, starting consumers on the files and recursing
    * on the subdirectories.
    * 
    * @param directory     The directory to process.
    * @param nCurrentLevel The current recursion depth. Should start at 0.
    * @param filter        The filter to use for selecting files to process.
    */
   protected void processDirectory(File directory, int nCurrentLevel, FileFilter filter)
   {
      FileConsumerConfig config = (FileConsumerConfig)m_config;
      String sIncomingDirectory = config.getIncomingDirectory();

      File[] files = directory.listFiles(filter);

      // Issue error (once) if incoming directory is inaccessible
      if (files == null)
      {
         if (s_logger.isWarnEnabled())
         {
            String sPath = directory.getAbsolutePath();

            if (addProblemFile(sPath))
            {
               s_logger.warn("Unable to access incoming directory \"" + sPath + "\".");
            }
         }

         return;
      }
      else
      {
         removeProblemFile(directory.getAbsolutePath());
      }

      try
      {
         for (int i = 0; i < files.length; i++)
         {
            File incomingFile = files[i];

            //Early exit
            synchronized (this)
            {
               if (m_bShutdown)
               {
                  return;
               }
            }

            //Process subdirectories
            if (incomingFile.isDirectory())
            {
               if (nCurrentLevel < config.getSubdirectoryLevels())
               {
                  processDirectory(incomingFile, nCurrentLevel + 1, filter);
               }
            }

            //Skip anything other than a file.
            if (!incomingFile.isFile())
            {
               continue;
            }

            //Compute path of file relative to the incoming directory.
            String sAbsolutePath = incomingFile.getAbsolutePath();
            String sRelativePath = null;

            if (sAbsolutePath.startsWith(sIncomingDirectory))
            {
               sRelativePath = sAbsolutePath.substring(sIncomingDirectory.length() + 1);
            }

            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Detected file: " + sRelativePath);
            }

            FileConsumer consumer = (FileConsumer)getConsumer();

            consumer.start(sRelativePath);
         }
      }
      catch (Throwable t)
      {
         if (s_logger.isWarnEnabled())
         {
            s_logger.warn("Error in " + this, t);
         }
      }
   }


   /**
    * Marks that the file attachment succeeded. This allows internal cleanup of the
    * list of erroneous files to be done.
    * 
    * @param sFileName The path and name of the file (relative to the incoming directory)
    *                  containing the message being consumed.
    */
   public void removeProblemFile(String sFileName)
   {
      if (m_problemFileNameSet != null)
      {
         m_problemFileNameSet.remove(sFileName);
      }
   }


   /**
    * Determines whether or not an error message should be logged for the
    * given file, setting a flag for this file to indicate that the error
    * message has been logged.
    * 
    * @param sFileName The path and name of the file (relative to the incoming directory)
    *                  containing the message being consumed.
    * @return True if the error message has not previous been logged;
    *         false otherwise.
    */
   public boolean addProblemFile(String sFileName)
   {
      if (m_problemFileNameSet == null)
      {
         m_problemFileNameSet = new HashHolder();
      }

      return m_problemFileNameSet.add(sFileName);
   }
}
