// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.LocalTransaction;
import javax.security.auth.Subject;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyInsert;
import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyUpdate;
import nexj.core.rpc.file.ra.tx.FileJournalRecordDelete;
import nexj.core.rpc.file.ra.tx.FileJournalRecordIncomingDelete;
import nexj.core.rpc.file.ra.tx.FileJournalRecordIncomingMove;
import nexj.core.rpc.file.ra.tx.FileJournalRecordMkTemp;
import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.rpc.ra.SharedManagedConnection;
import nexj.core.rpc.ra.tx.GenericJournalRecord;
import nexj.core.rpc.ra.tx.JournalRecordPrepared;
import nexj.core.rpc.ra.tx.MemoryJournal;
import nexj.core.rpc.ra.tx.PersistentJournal;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;

/**
 * The managed (physical) connection for file messages and file persistence.
 * 
 * A FileManagedConnection maintains a permanent association with a FileXAResource.
 * It may have any number of FileConnections at a given time.
 * 
 * A FileManagedConnection cannot be reused until the transaction has
 * completed.
 * 
 * For the persistence adapter, this class attaches to the target file
 * in the database directory. The file will be locked, but not manipulated.
 * (If the file does not already exist, it is not created or locked.)
 * Instead, when the user performs operations through this class, those
 * operations will be applied to a temporary file. On commit, the temporary
 * file will be copied over the target file, and the target file will be
 * unlocked.
 *  
 * For incoming files,this class attaches to the file in the incoming
 * directory. On commit, the incoming file will either be deleted or
 * moved to a directory for "processed" files.
 * 
 * For outgoing files, this class executes a persistence INSERT operation.
 * The only difference between this and the persistence connection is
 * that the outgoing connection will not attach if the file already exists
 * in the outgoing directory.
 * 
 * 
 * 
 * Attach Method
 * =============
 * 
 * Incoming File Message Connection:
 * - stage 1 file is set (to incoming message file)
 * - stage 1 lockable file handle is set and locked
 * 
 * Persistence Connection and Outgoing File Message Connection:
 * - stage 2 file is set (to the persistence data file)
 * - target lockable file handle is set and locked (if stage 2 exists)
 * - stage 1 file is set (to temp file)
 * - stage 1 lockable file handle is set, locked, and truncated (only if outgoing message)
 *   (prevents other outgoing messages of same name)
 *   
 * 
 * 
 * PATH NAME SPLITTING
 * ===================
 * The persistence adapter can potentially store many more files than the OS
 * supports in a single directory. To solve this problem, files are stored in
 * a directory tree. The naming of the directories in the directory tree is
 * derived from the file name itself, as configured by the name split size
 * and maximum number of name splits.
 * 
 * If a file is named "canadian" and the name split size is 2 and max name splits
 * is 3, then it will be given the full path:
 * 
 *    m_sDataDirectory/ca/na/di/canadian.bin
 * 
 * The file named "canada" is stored at:
 * 
 *    m_sDataDirectory/ca/na/da/canada.bin
 * 
 * And the file named "can" is stored at:
 * 
 *    m_sDataDirectory/ca/can.bin
 * 
 * The file named "ca" is stored at:
 * 
 *    m_sDataDirectory/ca/ca.bin
 * 
 * The file named "caca" is stored at:
 * 
 *    m_sDataDirectory/ca/ca/caca.bin
 *    
 */
public class FileManagedConnection extends SharedManagedConnection implements LocalTransaction
{
   // attributes

   /**
    * The number of characters in each split path component.
    */
   protected int m_nNameSplitSize;

   /**
    * The maximum number of name splits to perform on a file name.
    */
   protected int m_nMaxNameSplits;

   /**
    * A flag indicating that this connection is an input connection. If false,
    * then this connection is an output connection.
    */
   protected boolean m_bInputConnection;

   /**
    * A flag indicating that this connection is a file persistence connection.
    * If false, then this connection is a file message connection.
    */
   protected boolean m_bPersistenceConnection;

   /**
    * Recovery flag. True if this connection was created specifically
    * for recovery operations by FileResourceAdapter.getXAResources().
    */
   protected boolean m_bRecovery;

   /**
    * True if a local transaction is in progress.
    */
   protected boolean m_bLocalTransaction;

   // associations

   /**
    * For input connections, this is the directory in which the incoming file is
    * found. For output connections, this is the temporary directory used for
    * preparing the output message.
    */
   protected File m_primaryDirectory;

   /**
    * For input connections, this is the directory to which the incoming file
    * should be moved after it has been processed, or null to delete the incoming
    * file. For output connections, this is the outgoing directory into which the
    * message should be copied when it is committed.
    */
   protected File m_secondaryDirectory;

   /**
    * The transactional journal for this connection.
    */
   protected PersistentJournal m_journal;

   /**
    * The XAResource for this managed connection. Each managed connection has its own
    * XAResource that lasts for the lifetime of that connection.
    */
   protected FileXAResource m_xar;

   /**
    * The XID of the currently-associated transaction.
    */
   protected Xid m_currentXid;

   /**
    * The parameters for the local transaction; non-null only when a local transaction is
    * in progress.
    */
   protected PhysicalConnectionParameters m_localTxParameters;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileManagedConnection.class);

   /**
    * Lookup table to use to get the connection objects for a given key. The key is used
    * to distinguish between the many FileConnection handles issued on this physical
    * connection. Cannot use the handle instance itself for this purpose because it
    * is possible to have two handles issued on the same underlying file.
    * 
    * Type: PhysicalConnectionParameters[String]
    */
   protected Lookup m_fileLookup;


   // constructors

   /**
    * Creates a new physical connection instance, using the given journal file
    * for recording transactional operations. 
    * 
    * @param journal The journal file to use for recording transactional operations.
    */
   public FileManagedConnection(File journal)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating new FileManagedConnection: " + this);
      }
      
      m_journal = PersistentJournal.makeJournal(journal);
      
      m_fileLookup = new HashTab(1);
      
      m_xar = new FileXAResource(m_journal);
      m_xar.setManagedConnection(this);
   }


   // operations

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#cleanup()
    */
   public synchronized void cleanup() throws ResourceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("cleanup()");
      }

      super.cleanup();
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#destroy()
    */
   public synchronized void destroy() throws ResourceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("destroy()");
      }

      if (m_journal != null)
      {
         super.destroy();

         m_journal.close();
         m_journal = null;
      }
   }


   /**
    * Sets the primary directory. For input connections, this is the directory in
    * which the incoming file is found. For output connections, this is the
    * temporary directory used for preparing the output message.
    * 
    * @param directory The directory to use as the primary directory.
    */
   public void setPrimaryDirectory(File directory)
   {
      m_primaryDirectory = directory;
   }


   /**
    * Gets the primary directory.
    * 
    * @return The primary directory.
    */
   public File getPrimaryDirectory()
   {
      return m_primaryDirectory;
   }


   /**
    * Sets the secondary directory. For input connections, this is the directory to which the incoming file
    * should be moved after it has been processed, or null to delete the incoming
    * file. For output connections, this is the outgoing directory into which the
    * message should be copied when it is committed.
    * 
    * @param directory The directory to use as the secondary directory.
    */
   public void setSecondaryDirectory(File directory)
   {
      m_secondaryDirectory = directory;
   }


   /**
    * Gets the secondary directory.
    * 
    * @return The secondary directory.
    */
   public File getSecondaryDirectory()
   {
      return m_secondaryDirectory;
   }


   /**
    * Sets the flag indicating whether or not this connection is an input connection.
    * 
    * @param bInputConnection True to make this connection an input connection; false
    *                         to make it an output connection.
    */
   public void setInputConnection(boolean bInputConnection)
   {
      m_bInputConnection = bInputConnection;
   }


   /**
    * Gets the flag indicating whether or not this connection is an input connection.
    * 
    * @return True if this connection is an input connection; false if it is an output
    *         connection.
    */
   public boolean isInputConnection()
   {
      return m_bInputConnection;
   }


   /**
    * Sets the flag indicating whether or not this connection is a persistence connection.
    * 
    * @param bPersistenceConnection True to make this connection a file persistence connection;
    *                               false to make it a message connection.
    */
   public void setPersistenceConnection(boolean bPersistenceConnection)
   {
      m_bPersistenceConnection = bPersistenceConnection;
   }


   /**
    * Gets the flag indicating whether or not this connection is a persistence connection.
    * 
    * @return True if this connection is a file persistence connection; false if it is a
    *         file message connection.
    */
   public boolean isPersistenceConnection()
   {
      return m_bPersistenceConnection;
   }


   /**
    * Sets the recovery flag.
    * 
    * @param bRecovery True if this connection has been created specifically
    * for recovery operations by FileResourceAdapter.getXAResources().
    */
   public void setRecovery(boolean bRecovery)
   {
      m_bRecovery = bRecovery;
   }


   /**
    * Gets the recovery flag.
    * 
    * @return True if this connection was created specifically for recovery operations
    * by FileResourceAdapter.getXAResources().
    */
   public boolean isRecovery()
   {
      return m_bRecovery;
   }


   /**
    * Sets the maximum number of splits to perform on a file name.
    * 
    * @param nMaxSplits The maximum number of splits to perform.
    */
   public void setMaxNameSplits(int nMaxSplits)
   {
      m_nMaxNameSplits = nMaxSplits;
   }


   /**
    * Gets the maximum number of splits to perform on a file name.
    * 
    * @return The maximum number of splits to perform.
    */
   public int getMaxNameSplits()
   {
      return m_nMaxNameSplits;
   }


   /**
    * Sets the number of characters in each split path component.
    * 
    * @param nSplitSize The number of characters in each component.
    */
   public void setNameSplitSize(int nSplitSize)
   {
      m_nNameSplitSize = nSplitSize;
   }


   /**
    * Gets the number of characters in each split path component.
    * 
    * @return The number of characters in each component.
    */
   public int getNameSplitSize()
   {
      return m_nNameSplitSize;
   }

   /**
    * Sets the XID of the currently-associated global transaction, if any.
    * @param xid The XID of the current transaction.
    * @throws XAException If a local transaction is already in progress.
    */
   public void setCurrentXid(Xid xid) throws XAException
   {
      if (m_localTxParameters != null && xid != null)
      {
         throw new XAException(XAException.XAER_PROTO);
      }

      m_currentXid = xid;
   }

   /**
    * Gets the XID of the currently-associated global transaction, if any.
    * @return The XID of the current transaction.
    */
   public Xid getCurrentXid()
   {
      return m_currentXid;
   }

   /**
    * @see nexj.core.rpc.ra.UnsharedManagedConnection#createConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   protected GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating new FileConnection on " + this);
      }
      
      return new FileConnection(this);
   }


   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#matches(javax.security.auth.Subject, nexj.core.rpc.ra.GenericConnectionRequestInfo)
    */
   public boolean matches(Subject subject, GenericConnectionRequestInfo cri)
   {
      return true;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#getLocalTransaction()
    */
   public LocalTransaction getLocalTransaction() throws ResourceException
   {
      return this;
   }

   /**
    * @see javax.resource.spi.ManagedConnection#getXAResource()
    */
   public XAResource getXAResource() throws ResourceException
   {
      return m_xar;
   }

   /**
    * Performs as much of the commit as possible, permanently recording the transaction
    * result but not yet releasing any resources.
    * 
    * @return True if a commit can succeed; false if transaction must be rolled back.
    */
   public boolean prepare(Xid xid)
   {
      MemoryJournal journal = m_xar.getJournal(xid);

      for (Iterator itr = m_fileLookup.valueIterator(); itr.hasNext(); )
      {
         PhysicalConnectionParameters info = (PhysicalConnectionParameters)itr.next();

         if (!ObjUtil.equal(xid, info.getXid()))
         {
            continue;
         }

         if (!prepare(info, journal))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Prepares a transaction.
    * @param info The connection parameters.
    * @param journal The transaction journal.
    */
   protected boolean prepare(PhysicalConnectionParameters info, MemoryJournal journal)
   {
      Xid xid = info.getXid();

      if (m_bInputConnection)
      {
         if (m_secondaryDirectory == null)
         {
            if (!info.getStageOneFile().canWrite() || !m_primaryDirectory.canWrite())
            {
               return false;
            }

            journal.addRecord(new FileJournalRecordIncomingDelete(xid,
               info.getStageOneFile().getAbsolutePath(), info.getStageOneLockableFile())
               );
         }
         else
         {
            //Move the incoming file
            info.setStageTwoFile(new File(m_secondaryDirectory, info.getExpandedProcessedName()));

            if (info.getStageTwoFile().exists())
            {
               //Cannot commit: there is already a file with the same processed name
               return false;
            }

            if (!info.getStageTwoFile().getParentFile().exists())
            {
               if (!info.getStageTwoFile().getParentFile().mkdirs())
               {
                  //Cannot commit: unable to create processed directory/subdirectory for file.
                  return false;
               }
            }

            journal.addRecord(new FileJournalRecordIncomingMove(xid,
               info.getStageOneFile().getAbsolutePath(),
               info.getStageTwoFile().getAbsolutePath(),
               info.getStageOneLockableFile())
            );
         }
      }
      else
      {
         if (info.isOperationPerformed())
         {
            GenericJournalRecord operationRecord;

            if (info.isLastOpIsDelete())
            {
               operationRecord = new FileJournalRecordDelete(xid,
                  info.getStageTwoFile().getAbsolutePath(), info.getTargetFile()
                  );
            }
            else
            {
               if (info.getTargetFile() != null)
               {
                  operationRecord = new FileJournalRecordCopyUpdate(xid,
                     info.getStageOneFile().getAbsolutePath(),
                     info.getStageTwoFile().getAbsolutePath(),
                     info.getStageOneLockableFile(),
                     info.getTargetFile()
                     );
               }
               else
               {
                  operationRecord = new FileJournalRecordCopyInsert(xid,
                     info.getStageOneFile().getAbsolutePath(),
                     info.getStageTwoFile().getAbsolutePath(),
                     info.getStageOneLockableFile(),
                     info.getTargetFile()
                     );
               }
            }

            journal.addRecord(operationRecord);

            //Allow the temp file to get cleaned up.
            journal.addRecord(new FileJournalRecordMkTemp(xid,
               info.getStageOneFile().getAbsolutePath(),
               info.getStageOneLockableFile()
               )
            );
         }
      }

      journal.addRecord(new JournalRecordPrepared(xid));
      journal.flush();

      return true;
   }

   /**
    * Complete the transaction by making the operations that were
    * performed permanent.
    * 
    * @param xid The xid is needed for error reporting purposes.
    */
   public void commit(Xid xid) throws XAException
   {
      m_xar.executeCommit(xid);
      
      //Remove all physical connections that deal with this transaction.
      for (Iterator itr = m_fileLookup.valueIterator(); itr.hasNext(); )
      {
         PhysicalConnectionParameters info = (PhysicalConnectionParameters)itr.next();
         LockableFile targetFile = info.getTargetFile();
         
         if (!ObjUtil.equal(xid, info.getXid()))
         {
            continue;
         }

         info.setCommitted(true);
         itr.remove();
         
         //Executed when persistence connection is committed after performing no operations.
         if (targetFile != null && targetFile.isLocked())
         {
            targetFile.close();
         }
      }
   }


   /**
    * Complete the transaction by rolling back the operations that were performed.
    * 
    * This cannot delegate to the executeRollback method in FileXAResource
    * because that method uses the journal to do the rollback. Since this method
    * can be called at any time, even before prepare() has been executed, it
    * cannot rely on a journal having been written. 
    * 
    * @param xid The xid is needed for error reporting purposes.
    */
   public void rollback(Xid xid) throws XAException
   {
      for (Iterator itr = m_fileLookup.valueIterator(); itr.hasNext(); )
      {
         PhysicalConnectionParameters info = (PhysicalConnectionParameters)itr.next();

         if (!ObjUtil.equal(xid, info.getXid()))
         {
            continue;
         }

         itr.remove();
         rollback(info);
      }

      m_xar.executeRollback(xid);
   }

   /**
    * Rolls back a transaction.
    * @param info The connection parameters.
    */
   protected void rollback(PhysicalConnectionParameters info)
   {
      if (m_bInputConnection)
      {
         if (m_secondaryDirectory != null)
         {
            unlockAndCloseStage2(info);
            
            info.setStageTwoFile(null);
         }
         
         //Reset incoming file so that it can be picked up again.
         unlockAndCloseStage1(info);
      }
      else
      {
         /* ********** PERSISTENCE CONNECTION ROLLBACK ********* */
         /* Also rollback for outgoing message connection */
         
         unlockAndCloseStage1(info);
         
         if (info.getStageOneFile() != null && info.getStageOneFile().exists())
         {
            if (!info.getStageOneFile().delete())
            {
               throw new FileConnectionException("err.rpc.file.needsRecovery",
                  new Object[]{String.valueOf(info.getXid()), info.getStageOneFile().getName()});
            }
         }
         
         try
         {
            unlockAndCloseStage2(info);
         }
         catch (FileConnectionException ex)
         {
            throw new FileConnectionException("err.rpc.file.needsRecovery",
               new Object[]{String.valueOf(info.getXid()), info.getStageTwoFile().getName()}, ex);
         }
      }
   }

   /**
    * Helper method that unlocks and closes the stage 1 file. It silently
    * ignores null pointer issues, locks that have already been released,
    * and files that have already been closed.
    */
   protected void unlockAndCloseStage1(PhysicalConnectionParameters info)
   {
      if (info.getStageOneLockableFile() != null)
      {
         info.getStageOneLockableFile().unlock();
         info.getStageOneLockableFile().close();
      }
   }


   /**
    * Helper method that unlocks, closes, and deletes the stage 2 file.
    * 
    * @throws IOException
    */
   protected void unlockAndCloseStage2(PhysicalConnectionParameters info)
   {
      if (info.getTargetFile() != null)
      {
         info.getTargetFile().unlock();
         info.getTargetFile().close();
      }
   }

   /**
    * @see javax.resource.spi.LocalTransaction#begin()
    */
   public void begin() throws ResourceException
   {
      if (m_currentXid != null)
      {
         throw new ResourceException("Cannot start local transaction when XA transaction already in progress");
      }

      m_bLocalTransaction = true;
   }

   /**
    * @see javax.resource.spi.LocalTransaction#commit()
    */
   public void commit() throws ResourceException
   {
      try
      {
         if (!m_bLocalTransaction)
         {
            throw new ResourceException("No local transaction in progress");
         }

         if (m_localTxParameters == null)
         {
            throw new ResourceException("Connection was not closed");
         }

         if (m_localTxParameters.isCommitted())
         {
            return;
         }

         m_localTxParameters.setCommitted(true);

         MemoryJournal journal = m_xar.getJournal(null);

         if (!prepare(m_localTxParameters, journal))
         {
            // TODO: Error code
            throw new ResourceException("Unable to commit local transaction");
         }

         try
         {
            m_xar.executeCommit(journal, null, false);

            for (Iterator itr = m_fileLookup.valueIterator(); itr.hasNext(); )
            {
               PhysicalConnectionParameters info = (PhysicalConnectionParameters)itr.next();

               if (info == m_localTxParameters)
               {
                  itr.remove();

                  break;
               }
            }

            //Executed when persistence connection is committed after performing no operations.
            if (m_localTxParameters.getTargetFile() != null && m_localTxParameters.getTargetFile().isLocked())
            {
               m_localTxParameters.getTargetFile().close();
            }
         }
         catch (XAException ex)
         {
            ObjUtil.rethrow(ex);
         }
      }
      finally
      {
         m_bLocalTransaction = false;
         m_localTxParameters = null;
      }
   }

   /**
    * @see javax.resource.spi.LocalTransaction#rollback()
    */
   public void rollback() throws ResourceException
   {
      try
      {
         if (!m_bLocalTransaction)
         {
            throw new ResourceException("No local transaction in progress");
         }

         if (m_localTxParameters == null)
         {
            throw new ResourceException("Connection was not closed");
         }

         rollback(m_localTxParameters);
      }
      finally
      {
         m_bLocalTransaction = false;
         m_localTxParameters = null;
      }
   }


   // FileConnection methods

   /**
    * Opens the file with the given filename for transacted read or write.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sTargetFile The name of the file to open, relative to the directories
    *                    for this connection.
    * @return A key to be used by the FileConnection handle to identify itself
    *         in all subsequent operations performed by that handle.
    *         
    * @see nexj.core.rpc.file.FileConnection#attachToFile(java.lang.String)
    */
   public PhysicalConnectionParameters attachToFile(String sTargetFile)
   {
      PhysicalConnectionParameters info;
      File fileToAttach = new File(m_primaryDirectory, sTargetFile);
      
      info = (PhysicalConnectionParameters)m_fileLookup.get(sTargetFile);
      
      if (info != null)
      {
         return info;
      }
      else
      {
         info = new PhysicalConnectionParameters();
      }
      
      //Open and lock file.
      if (m_bInputConnection)
      {
         if (!fileToAttach.canRead())
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("File \"" + fileToAttach.getAbsolutePath() + "\" does not exist or is not readable; cannot attach for incoming mode.");
            }
            
            //Unable to attach: cannot read incoming file, or it doesn't exist
            return null;
         }
         
         info.setStageOneLockableFile(new LockableFile(fileToAttach));
         
         if(!info.getStageOneLockableFile().tryLock(false))
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Unable to lock file \"" + fileToAttach.getAbsolutePath() + "\"; cannot attach.");
            }
            
            unlockAndCloseStage1(info);
            
            info.setStageOneLockableFile(null);
            
            //Unable to attach due to locking
            return null;
         }
         
         //Attachment has been successful
         info.setStageOneFile(fileToAttach);
         info.setXid(m_currentXid);
         
         m_fileLookup.put(sTargetFile, info);
         
         return info;
      }
      else
      {
         /* ********** OPEN AS PERSISTENCE CONNECTION ********** */
         /* This is also used for outgoing file messages */
         
         info.setLastOpIsDelete(false);
         
         if (!m_primaryDirectory.canWrite())
         {
            throw new FileConnectionException("err.rpc.file.missingOutgoingTemp",
               new Object[]{m_primaryDirectory.toString()});
         }
         
         if (!m_secondaryDirectory.canWrite())
         {
            throw new FileConnectionException("err.rpc.file.missingDataDirectory",
               new Object[]{m_secondaryDirectory.toString()});
         }
         
         info.setStageTwoFile(FileManagedConnection.splitNameToSubdirs(m_secondaryDirectory, sTargetFile, m_nMaxNameSplits, m_nNameSplitSize, true));
         
         /*
          * Note that target file existence check and the lock operation,
          * which creates the file if it didn't already exist, do not
          * occur atomically. However, this is NOT a race condition,
          * due to how the file persistence connection will be used.
          * Since creation will only happen for a new object, and
          * since every new object is given a unique id, it is
          * statistically unlikely that a second object will be created
          * and persisted with the same id as the first object, during
          * the window between existence check and creation.
          */
         if (info.getStageTwoFile().exists())
         {
            // Outgoing message connection must ensure file doesn't exist already.
            if (!m_bPersistenceConnection)
            {
               return null;
            }
            
            info.setTargetFile(new LockableFile(info.getStageTwoFile()));
            
            //LOCK TARGET (BLOCKING)
            if (!info.getTargetFile().lock(false))
            {
               info.getTargetFile().close();
               return null;
            }
         }
         else
         {
            //Defer locking until commit for outgoing messages and INSERT operations
         }

         
         //Create, truncate, lock temp file.
         info.setStageOneFile(fileToAttach);
         
         //Prevent other outgoing messages of the same name
         if (!m_bPersistenceConnection)
         {
            if (!fileToAttach.getParentFile().exists())
            {
               fileToAttach.getParentFile().mkdirs();
            }

            info.setStageOneLockableFile(new LockableFile(info.getStageOneFile()));

            if (!info.getStageOneLockableFile().tryLock(false))
            {
               if (info.getTargetFile() != null)
               {
                  info.getTargetFile().close();
               }
               
               info.getStageOneLockableFile().close();

               return null;
            }
            else
            {
               try
               {
                  info.getStageOneLockableFile().truncate();
               }
               catch (IOException ex)
               {
                  unlockAndCloseStage1(info);
                  
                  if (info.getTargetFile() != null)
                  {
                     info.getTargetFile().close();
                  }
                  
                  return null;
               }
            }
         }
         
         m_fileLookup.put(sTargetFile, info);
         info.setXid(m_currentXid);
         
         return info;
      }
   }


   /**
    * Gets an input stream to use for reading and writing an incoming file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#getInputStream()
    */
   public InputStream getInputStream(PhysicalConnectionParameters parameters)
   {
      if (m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation not valid on a persistence connection");
      }
      
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      //Check read mode
      if (!m_bInputConnection)
      {
         throw new IllegalStateException("Input stream is only available from an incoming FileConnection");
      }
      
      parameters.setOperationPerformed();
      parameters.setXid(m_currentXid);
      
      try
      {
         return parameters.getStageOneLockableFile().getInputStream();
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
         return null;
      }
   }


   /**
    * Gets an output stream to use for writing to an outgoing file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#getOutputStream()
    */
   public OutputStream getOutputStream(PhysicalConnectionParameters parameters)
   {
      if (m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation not valid on a persistence connection");
      }
      
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      //Check write mode
      if (m_bInputConnection)
      {
         throw new IllegalStateException("Output is only possible on an outgoing FileConnection");
      }
      
      parameters.setOperationPerformed();
      parameters.setLastOpIsDelete(false);
      parameters.setXid(m_currentXid);
      
      return parameters.getStageOneLockableFile().getOutputStream();
   }


   /**
    * Reads data from the persistence file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#readBinary()
    */
   public synchronized Binary readBinary(PhysicalConnectionParameters parameters) throws IOException
   {
      if (!m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation only valid on a persistence connection");
      }
      
      //Check attached.
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      LockableFile temporaryFile = null;
      
      parameters.setXid(m_currentXid);
      
      try
      {
         if (parameters.isLastOpIsDelete())
         {
            return null;
         }
         
         if (parameters.isOperationPerformed() && parameters.getStageOneFile().exists())
         {
            temporaryFile = new LockableFile(parameters.getStageOneFile());
            
            return temporaryFile.getDataAsBinary();
         }
         else
         {
            return parameters.getTargetFile().getDataAsBinary();
         }
      }
      finally
      {
         if (temporaryFile != null)
         {
            temporaryFile.close();
         }
      }
   }


   /**
    * Reads data from the persistence file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#readString()
    */
   public synchronized String readString(PhysicalConnectionParameters parameters) throws IOException
   {
      if (!m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation only valid on a persistence connection");
      }
      
      //Check attached.
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      LockableFile temporaryFile = null;
      
      parameters.setXid(m_currentXid);
      
      try
      {
         if (parameters.isLastOpIsDelete())
         {
            return null;
         }
         
         if (parameters.isOperationPerformed() && parameters.getStageOneFile().exists())
         {
            temporaryFile = new LockableFile(parameters.getStageOneFile());
            
            return temporaryFile.getDataAsString();
         }
         else
         {
            return parameters.getTargetFile().getDataAsString();
         }
      }
      finally
      {
         if (temporaryFile != null)
         {
            temporaryFile.close();
         }
      }
   }


   /**
    * Writes data to the persistence file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#write(nexj.core.util.Binary)
    */
   public synchronized void write(PhysicalConnectionParameters parameters, Binary data) throws IOException
   {
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      if (m_bPersistenceConnection)
      {
         LockableFile temporaryFile = new LockableFile(parameters.getStageOneFile());
         
         try
         {
            temporaryFile.writeData(data);
            temporaryFile.force();
         }
         finally
         {
            temporaryFile.close();
         }
      }
      else if (!m_bInputConnection)
      {
         parameters.getStageOneLockableFile().writeData(data);
      }
      else
      {
         throw new IllegalStateException("Output is only possible on an outgoing or persistence FileConnection");
      }
      
      parameters.setXid(m_currentXid);
      parameters.setOperationPerformed();
      parameters.setLastOpIsDelete(false);
   }


   /**
    * Writes data to the persistence file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#write(java.lang.String)
    */
   public synchronized void write(PhysicalConnectionParameters parameters, String sData) throws IOException
   {
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      if (m_bPersistenceConnection)
      {
         LockableFile temporaryFile = new LockableFile(parameters.getStageOneFile());
         
         try
         {
            temporaryFile.writeData(sData);
            temporaryFile.force();
         }
         finally
         {
            temporaryFile.close();
         }
      }
      else if (!m_bInputConnection)
      {
         parameters.getStageOneLockableFile().writeData(sData);
      }
      else
      {
         throw new IllegalStateException("Output is only possible on an outgoing or persistence FileConnection");
      }
      
      parameters.setXid(m_currentXid);
      parameters.setOperationPerformed();
      parameters.setLastOpIsDelete(false);
   }


   /**
    * Schedules the data file for deletion on commit.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#delete()
    */
   public void delete(PhysicalConnectionParameters parameters) throws IOException
   {
      if (!m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation only valid on a persistence connection");
      }
      
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      parameters.setXid(m_currentXid);
      parameters.setOperationPerformed();
      parameters.setLastOpIsDelete(true);
   }

   /**
    * Completes a local transaction, if any.
    * @param parameters The connection parameters.
    * @throws IOException If an I/O error occurs.
    */
   public void finish(PhysicalConnectionParameters parameters) throws IOException
   {
      if (parameters.getXid() == null)
      {
         // not enlisted in an XA transaction
         m_localTxParameters = parameters;

         // If begin() never called, commit implicit transaction on close; otherwise, wait for commit() to be called
         if (!m_bLocalTransaction)
         {
            m_bLocalTransaction = true;

            try
            {
               commit();
            }
            catch (ResourceException ex)
            {
               ObjUtil.rethrow(ex);
            }
         }
      }
   }

   /**
    * Gets the last modified time of the data file from the persistence
    * file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#getLastModified()
    */
   public long getLastModified(PhysicalConnectionParameters parameters)
   {
      if (!m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation only valid on a persistence connection");
      }
      
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      parameters.setXid(m_currentXid);

      return parameters.getStageTwoFile().lastModified();
   }


   /**
    * Gets the last modified time of the temporary file from the persistence
    * file connection.
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#getLastModifiedThisTxn()
    */
   public long getLastModifiedThisTxn(PhysicalConnectionParameters parameters)
   {
      if (!m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation only valid on a persistence connection");
      }
      
      if (parameters == null)
      {
         throw new IllegalStateException("Operation not valid on un-attached FileConnection");
      }
      
      if (!parameters.getStageOneFile().exists())
      {
         throw new IllegalStateException("Operation not valid when temporary file doesn't exist");
      }
      
      parameters.setXid(m_currentXid);
      
      return parameters.getStageOneFile().lastModified();
   }


   /**
    * Sets the file name to use for an imcoing message file when it is moved
    * to the "processed" directory (after it has been successfully processed).
    * For use only by a FileConnection handle issued on this physical connection.
    * 
    * @param sFile A key used to identify the FileConnection handle. Should be the
    *              value that was returned when the handle called attachToFile().
    * 
    * @see nexj.core.rpc.file.FileConnection#setExpandedProcessedName(java.lang.String)
    */
   public void setExpandedProcessedName(PhysicalConnectionParameters parameters, String sExpandedName)
   {
      if (m_bPersistenceConnection)
      {
         throw new IllegalStateException("Operation not valid on a persistence connection");
      }
      
      parameters.setExpandedProcessedName(sExpandedName);
      parameters.setXid(m_currentXid);
   }


   /**
    * Based on the file name and the integer parameters, create a chain of
    * subdirectories under rootDir so that the names of the subdirectories,
    * when concatenated, form a prefix to the file name.
    * 
    * e.g. splitNameToSubdirs("/", "elephant", 3, 2) => "/el/ep/ha/elephant"
    * 
    * Note that the file name is padded with '_' characters if it is less than
    * the length required to reach a leaf directory. This prevents name collisions
    * between the files and the directories.
    * 
    * If nMaxSplits is less than 1 then name splitting is disabled: the file
    * is created as a direct child of the root directory, and no padding is
    * performed.
    * 
    * @param rootDir    The base directory in which the file should be stored.
    * @param sFileName  The name of the file.
    * @param nMaxSplits The maximum number of subdirectories to go to.
    * @param nSplitSize The number of characters to use in each path component.
    * @param bCreateDir True to create the subdirectory tree.
    * @return The file.
    */
   public static File splitNameToSubdirs(File rootDir, String sFileName, int nMaxSplits, int nSplitSize, boolean bCreateDir)
   {
      int j = 0;
      StringBuilder buf = new StringBuilder(nMaxSplits * (nSplitSize + SysUtil.FILE_SEP.length()));
      
      for (int i = 0; i < nMaxSplits; ++i)
      {
         for (int k = 0; k < nSplitSize; ++k)
         {
            buf.append((j < sFileName.length()) ? sFileName.charAt(j) : '_');
            ++j;
         }

         buf.append(SysUtil.FILE_SEP);
      }
      
      File subdir = new File(rootDir, buf.toString());
      
      //Create the tree
      if (bCreateDir && !subdir.exists())
      {
         if (!subdir.mkdirs())
         {
            throw new FileConnectionException("err.rpc.file.cannotCreatePersistenceTree",
               new Object[]{subdir.toString(), sFileName});
         }
      }
      
      return new File(subdir, sFileName);
   }


   // inner classes

   /**
    * Holds all the information and file handles for a connection to a single
    * file.
    */
   public static class PhysicalConnectionParameters
   {
      // attributes

      /**
       * The expanded processed name to use as the new name for an incoming file, after
       * it has been processed and is being moved to the processed directory (stage 2 directory).
       */
      protected String m_sExpandedProcessedName;

      /**
       * True if at least one write operation has been performed on this connection.
       */
      protected boolean m_bOperationPerformed;

      /**
       * True if the last operation performed on this connection was a delete.
       */
      protected boolean m_bLastOpIsDelete;

      /**
       * True if the changes have been committed.
       */
      protected boolean m_bCommitted;

      // associations

      /**
       * Path information for the stage one file.
       */
      protected File m_stageOneFile;

      /**
       * Path information for the stage two file, if any.
       */
      protected File m_stageTwoFile;

      /**
       * The locked stage one file for message file connections. Null for persistence
       * file connections.
       */
      protected LockableFile m_stageOneLockableFile;

      /**
       * The file persistence connection data file.
       */
      protected LockableFile m_targetFile;

      /**
       * The transaction id with which this connection is associated.
       */
      protected Xid m_xid;


      // operations

      /**
       * Gets the path for the stage one file.
       * 
       * @return The path to the file.
       */
      public File getStageOneFile()
      {
         return m_stageOneFile;
      }

      /**
       * Sets the path for the stage one file.
       * 
       * @param stageOneFile The path to the stage one file.
       */
      public void setStageOneFile(File stageOneFile)
      {
         m_stageOneFile = stageOneFile;
      }

      /**
       * Gets the path for the stage two file.
       * 
       * @return The path to the file; null if there is no stage two file.
       */
      public File getStageTwoFile()
      {
         return m_stageTwoFile;
      }

      /**
       * Sets the path for the stage two file.
       * 
       * @param stageTwoFile The path to the file.
       */
      public void setStageTwoFile(File stageTwoFile)
      {
         m_stageTwoFile = stageTwoFile;
      }

      /**
       * Gets the file handle for the stage one file.
       * 
       * @return The file handle for the stage one file.
       */
      public LockableFile getStageOneLockableFile()
      {
         return m_stageOneLockableFile;
      }

      /**
       * Sets the file handle for accessing the stage one file.
       * 
       * @param stageOneLockableFile The file handle for the stage one file.
       */
      public void setStageOneLockableFile(LockableFile stageOneLockableFile)
      {
         m_stageOneLockableFile = stageOneLockableFile;
      }

      /**
       * Gets the file handle for the data file (persistence connection).
       * 
       * @return The file handle for the data file (persistence connection).
       */
      public LockableFile getTargetFile()
      {
         return m_targetFile;
      }

      /**
       * Sets the file handle for the data file.
       * 
       * @param targetFile The file handle for the data file.
       */
      public void setTargetFile(LockableFile targetFile)
      {
         m_targetFile = targetFile;
      }

      /**
       * Gets whether or not a write operation has been performed on this
       * connection.
       * 
       * @return True if a write operation has been performed; false otherwise.
       */
      public boolean isOperationPerformed()
      {
         return m_bOperationPerformed;
      }

      /**
       * Sets a flag to indicate that a write operation has been performed
       * on this connection.
       */
      public void setOperationPerformed()
      {
         m_bOperationPerformed = true;
      }

      /**
       * Gets whether or not the last operation performed on this connection
       * was a delete operation.
       * 
       * @return True if the last operation performed was a delete; false otherwise.
       */
      public boolean isLastOpIsDelete()
      {
         return m_bLastOpIsDelete;
      }

      /**
       * Sets a flag indicating whether or not the last operation performed on
       * this connection was a delete operation.
       * 
       * @param bLastOpIsDelete True if the last operation performed was a delete.
       */
      public void setLastOpIsDelete(boolean bLastOpIsDelete)
      {
         m_bLastOpIsDelete = bLastOpIsDelete;
      }

      /**
       * Gets whether or not the changes have been committed.
       * @return True if the changes have been committed.
       */
      public boolean isCommitted()
      {
         return m_bCommitted;
      }

      /**
       * Sets whether or not the changes have been committed.
       * @param bCommitted True if the changes have been committed.
       */
      public void setCommitted(boolean bCommitted)
      {
         m_bCommitted = bCommitted;
      }

      /**
       * Gets the name to use as a new name for an incoming file.
       * 
       * @return The new name to use for an incoming file, after it has been processed.
       */
      public String getExpandedProcessedName()
      {
         return m_sExpandedProcessedName;
      }

      /**
       * Sets the name to use as the processed name for an incoming file.
       * 
       * @param sExpandedProcessedName The name to use for an incoming file, after it
       *                               has been processed.
       */
      public void setExpandedProcessedName(String sExpandedProcessedName)
      {
         m_sExpandedProcessedName = sExpandedProcessedName;
      }

      /**
       * Gets the transaction id with which this connection is associated.
       * 
       * @return The Xid with which this connection is associated.
       */
      public Xid getXid()
      {
         return m_xid;
      }

      /**
       * Sets the transaction id with which this connection is associated.
       * 
       * @param xid The transaction id to associate this connection with.
       */
      public void setXid(Xid xid)
      {
         m_xid = xid;
      }
   }
}
