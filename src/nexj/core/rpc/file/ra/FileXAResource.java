// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.resource.ResourceException;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyInsert;
import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyUpdate;
import nexj.core.rpc.file.ra.tx.FileJournalRecordDelete;
import nexj.core.rpc.file.ra.tx.FileJournalRecordIncomingDelete;
import nexj.core.rpc.file.ra.tx.FileJournalRecordIncomingMove;
import nexj.core.rpc.file.ra.tx.FileJournalRecordMkTemp;
import nexj.core.rpc.ra.tx.GenericJournalRecord;
import nexj.core.rpc.ra.tx.JournalRecordCompleted;
import nexj.core.rpc.ra.tx.JournalRecordPrepared;
import nexj.core.rpc.ra.tx.MemoryJournal;
import nexj.core.rpc.ra.tx.PersistentJournal;
import nexj.core.rpc.ra.tx.RecoveredXid;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * The XAResource for file connections. Allows a file connection to be used
 * in a distributed transactional environment.
 */
public class FileXAResource implements XAResource
{
   // constants

   /**
    * State representing that this resource is not associated with any transaction.
    */
   public final static int STATE_UNASSOCIATED = 0;

   /**
    * State representing that this resource is currently associated with a transaction.
    */
   public final static int STATE_ASSOCIATED = 1;

   /**
    * State representing that this resource's association is currently suspended.
    */
   public final static int STATE_SUSPENDED = 2;


   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileXAResource.class);

   /**
    * XIDs of all transactions that have been encountered so far, but have not
    * yet been completed.
    */
   protected Lookup m_knownXids;

   /**
    * The transactional journal instance used to record progress of processing
    * various transactions.
    */
   protected PersistentJournal m_journal;

   /**
    * The managed connection. (This is needed to obtain configuration settings
    * for recovery operations)
    */
   protected FileManagedConnection m_managedConnection;


   // constructors

   /**
    * Creates a new instance that uses the given journal as its underlying
    * transactional log.
    * 
    * @param journal
    */
   public FileXAResource(PersistentJournal journal)
   {
      m_knownXids = new HashTab(1);
      
      m_journal = journal;
   }


   // operations

   /**
    * Sets the managed connection associated with this XAResource. It is used
    * to obtain configuration settings for recovery operations.
    * 
    * @param managedConnection
    */
   public void setManagedConnection(FileManagedConnection managedConnection)
   {
      m_managedConnection = managedConnection;
   }

   /**
    * Gets a handle to the journal holding transactional operations for the
    * given transaction id. The handle is created if it doesn't already exist.
    * 
    * @param xid The transaction id.
    * @return A handle to the transaction journal.
    */
   public MemoryJournal getJournal(Xid xid)
   {
      if (xid == null)
      {
         return m_journal.getJournal(null);
      }

      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      MemoryJournal result = tInfo.getJournal();
      
      if (result == null)
      {
         result = m_journal.getJournal(xid);
         tInfo.setJournal(result);
      }
      
      return result;
   }


   /**
    * Rolls back the transactional operations performed for the given
    * transaction id.
    * 
    * @param xid The id of the transaction to roll back.
    * @throws XAException
    */
   public void executeRollback(Xid xid) throws XAException
   {
      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      
      MemoryJournal journal = tInfo.getJournal();
      
      //It is only necessary to record completion if the journal has been made
      //(Completion will not be recorded if it has not been prepared)
      if (journal != null)
      {
         journal.addRecord(new JournalRecordCompleted(xid));
         journal.flush();
         journal.completed();
      }
   }


   /**
    * Rolls back the transactional operations performed for the given
    * transaction id.
    * 
    * @param journal   The journal handle containing the operations to roll back.
    * @param xid       The id of the transaction to roll back.
    * @throws XAException
    */
   public void executeRollback(MemoryJournal journal, Xid xid) throws XAException
   {
      List opsList = journal.getRecords();
      int nOpsCount = opsList.size();
      
      for (int i = 0; i < nOpsCount; i++)
      {
         GenericJournalRecord opRecord = (GenericJournalRecord)opsList.get(i);
         int nOpCode = opRecord.getOpCode();
         
         switch (nOpCode)
         {
            case FileJournalRecordCopyInsert.OPCODE:
               //CREATE and OUTGOING
               String sDstFileName = ((FileJournalRecordCopyInsert)opRecord).getDestinationFilePath();
               File dstFile = new File(sDstFileName);
               
               /*
                * The target file could have been created and might even have
                * been filled with partial data. If so, delete it.
                * 
                * Do not need to worry about another process coming along and
                * creating the file between the time that this txn crashed and
                * the present moment (recovery of this txn), because the naming
                * of the files should be statistically unique.
                */
               if (dstFile.exists())
               {
                  if (!dstFile.delete())
                  {
                     throw new FileConnectionException("err.rpc.file.needsRecovery",
                        new Object[]{xid.toString(), sDstFileName});
                  }
               }
               
               break;


            case FileJournalRecordMkTemp.OPCODE:
               String sTempFileName = ((FileJournalRecordMkTemp)opRecord).getFilePath();
               File tempFile = new File(sTempFileName);
               
               if (tempFile.exists())
               {
                  if (!tempFile.delete())
                  {
                     throw new FileConnectionException("err.rpc.file.needsRecovery",
                        new Object[]{xid.toString(), sTempFileName});
                  }
               }
               
               break;


            case FileJournalRecordCopyUpdate.OPCODE:
            case FileJournalRecordIncomingMove.OPCODE:
               //Do not delete processed file, because it might have been made by another
               //incoming message later on.
            case FileJournalRecordIncomingDelete.OPCODE:
            case FileJournalRecordDelete.OPCODE:
            case JournalRecordPrepared.OPCODE:
               break;
               
            default:
               throw new IllegalArgumentException("Unknown opcode: " + nOpCode);
         }
      }
      
      //Record completion
      journal.addRecord(new JournalRecordCompleted(xid));
      journal.flush();
      journal.forget(xid);
   }


   /**
    * Commits the transactional operations for the given transaction id.
    * 
    * @param xid The id of the transaction to commit.
    * @throws XAException
    */
   public void executeCommit(Xid xid) throws XAException
   {
      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      
      executeCommit(tInfo.getJournal(), xid, false);
   }


   /**
    * Commits the transactional operations for the given transaction id.
    * 
    * @param journal   The journal handle containing the operations to commit.
    * @param xid       The id of the transaction to commit.
    * @param bRecovery True if this is a recovery commit; false otherwise.
    * @throws XAException
    */
   public void executeCommit(MemoryJournal journal, Xid xid, boolean bRecovery) throws XAException
   {
      List opsList = journal.getRecords();
      int nOpsCount = opsList.size();
      String sErrorResourceName = null;
      
      try
      {
         for (int i = 0; i < nOpsCount; i++)
         {
            GenericJournalRecord opRecord = (GenericJournalRecord)opsList.get(i);
            int nOpCode = opRecord.getOpCode();
            
            sErrorResourceName = opRecord.toString();
            
            switch (nOpCode)
            {
               case FileJournalRecordCopyUpdate.OPCODE:
                  //UPDATE
                  FileJournalRecordCopyUpdate copyUpdate = (FileJournalRecordCopyUpdate)opRecord;
                  File srcFile = new File(copyUpdate.getSourceFilePath());
                  File dstFile = new File(copyUpdate.getDestinationFilePath());
                  LockableFile src = copyUpdate.getSourceFileHandle();
                  LockableFile dst = copyUpdate.getDestinationFileHandle();
                  
                  if (!bRecovery)
                  {
                     assert dst != null;
                     assert dst.isLocked();
                     
                     dst.copyFromFile(srcFile);
                     dst.force();
                     dst.close();
                     dstFile.setLastModified(srcFile.lastModified());
                  }
                  else
                  {
                     //UPDATE cannot be recovered.
                     throw new XAException(XAException.XA_HEURRB);
                  }
                  
                  break;
                  
                  
               case FileJournalRecordCopyInsert.OPCODE:
                  //CREATE and OUTGOING
                  FileJournalRecordCopyInsert copyInsert = (FileJournalRecordCopyInsert)opRecord;
                  srcFile = new File(copyInsert.getSourceFilePath());
                  dstFile = new File(copyInsert.getDestinationFilePath());
                  src = copyInsert.getSourceFileHandle();
                  dst = copyInsert.getDestinationFileHandle();
                  
                  if (!bRecovery)
                  {
                     if (src != null)
                     {
                        //OUTGOING MESSAGE (src is outgoing temp and is locked)
                        assert dst == null;
                        assert src.isLocked();

                        //Build the path to the destination file, if it doesn't exist.
                        if (!dstFile.getParentFile().equals(
                           m_managedConnection.getSecondaryDirectory()) &&
                           !dstFile.getParentFile().exists())
                        {
                           dstFile.getParentFile().mkdirs();
                        }

                        src.copyToFile(dstFile);
                     }
                     else
                     {
                        //CREATE (persistence)
                        if (srcFile.exists() && !dstFile.exists())
                        {
                           if (!srcFile.renameTo(dstFile))
                           {
                              throw new FileConnectionException("err.rpc.file.needsRecovery",
                                 new Object[]{xid.toString(), srcFile.getAbsolutePath()});
                           }
                        }
                     }
                  }
                  else
                  {
                     if (srcFile.exists() && !dstFile.exists())
                     {
                        if (!srcFile.renameTo(dstFile))
                        {
                           throw new FileConnectionException("err.rpc.file.needsRecovery",
                              new Object[]{xid.toString(), srcFile.getAbsolutePath()});
                        }
                     }
                  }
                  
                  break;
                  
                  
               case FileJournalRecordDelete.OPCODE:
                  if (bRecovery)
                  {
                     throw new XAException(XAException.XA_HEURRB);
                  }
                  
                  FileJournalRecordDelete delete = (FileJournalRecordDelete)opRecord;
                  LockableFile target = delete.getFileHandle();
                  
                  if (target != null)
                  {
                     target.closeAndDelete();
                  }
                  
                  break;
                  
                  
               case FileJournalRecordMkTemp.OPCODE:
                  FileJournalRecordMkTemp mktemp = (FileJournalRecordMkTemp)opRecord;
                  LockableFile temp = mktemp.getFileHandle();

                  if (temp != null)
                  {
                     temp.close();
                  }
                  
                  String sTempFileName = mktemp.getFilePath();
                  File tempFile = new File(sTempFileName);
                  
                  if (tempFile.exists())
                  {
                     if (!tempFile.delete())
                     {
                        throw new FileConnectionException("err.rpc.file.needsRecovery",
                           new Object[]{xid.toString(), sTempFileName});
                     }
                  }
                  
                  break;
                  
                  
               case FileJournalRecordIncomingMove.OPCODE:
                  if (bRecovery)
                  {
                     throw new XAException(XAException.XA_HEURRB);
                  }
                  
                  FileJournalRecordIncomingMove incomingMove = (FileJournalRecordIncomingMove)opRecord;
                  String sProcessedName = incomingMove.getDestinationFilePath();
                  File processedFile = new File(sProcessedName);
                  LockableFile incoming = incomingMove.getSourceFileHandle();
                  
                  assert incoming != null;
                  
                  incoming.copyToFile(processedFile);
                  incoming.truncate();
                  incoming.closeAndDelete();
                  
                  break;


               case FileJournalRecordIncomingDelete.OPCODE:
                  if (bRecovery)
                  {
                     throw new XAException(XAException.XA_HEURRB);
                  }
                  
                  FileJournalRecordIncomingDelete incomingDelete = (FileJournalRecordIncomingDelete)opRecord;
                  incoming = incomingDelete.getFileHandle();
                  
                  assert incoming != null;
                  
                  incoming.truncate();
                  incoming.closeAndDelete();
                  
                  break;
               
               
               case JournalRecordPrepared.OPCODE:
                  break;
                  
               default:
                  throw new IllegalArgumentException("Unknown opcode: " + nOpCode);
            }
         }
      }
      catch (IOException ex)
      {
         throw new FileConnectionException("err.rpc.file.needsRecovery",
            new Object[]{xid.toString(), sErrorResourceName}, ex);
      }

      
      //Record completion.
      journal.addRecord(new JournalRecordCompleted(xid));
      journal.flush();
      
      if (!bRecovery)
      {
         journal.completed();
      }
      else
      {
         journal.forget(xid);
      }
   }


   /* ******************** XAResource Implementation ******************** */

   /**
    * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
    */
   public synchronized void commit(Xid xid, boolean bOnePhase) throws XAException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Committing Transaction: " + xid + " (in " + this + ")");
      }
      
      if (!m_knownXids.contains(xid))
      {
         MemoryJournal recovered = m_journal.recoverJournal(xid);

         executeCommit(recovered, xid, true);
         completeRecovery();

         return;
      }
      
      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      
      if (tInfo == null)
      {
         throw new XAException(XAException.XAER_NOTA);
      }
      
      boolean bCommit = true;
      
      //Handle 1PC
      if (bOnePhase)
      {
         //Transaction managed forgets this XID, so no point in recording READY in log
         
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Performing one-phase commit on XID: " + xid);
         }
         
         if (!m_managedConnection.prepare(xid))
         {
            bCommit = false;
         }
      }
      
      
      //Actually commit
      if (bCommit)
      {
         try
         {
            m_managedConnection.commit(xid);
         }
         catch (Exception ex)
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Got exception during FileConnection.commit()", ex);
            }
            
            throw new XAException(XAException.XAER_RMERR);
         }
      }
      
      m_knownXids.remove(xid);
   }


   /**
    * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
    */
   public synchronized void end(Xid xid, int nFlags) throws XAException
   {
      if (xid == null)
      {
         throw new XAException(XAException.XAER_NOTA);
      }
      
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Ending Transaction: " + xid + " (in " + this + ")");
      }
      
      if (!ObjUtil.equal(m_managedConnection.getCurrentXid(), xid))
      {
         throw new XAException(XAException.XAER_PROTO);
      }
      
      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      
      assert tInfo != null;
      
      switch (nFlags)
      {
         case XAResource.TMSUSPEND:
            if (tInfo.getState() != STATE_ASSOCIATED)
            {
               throw new XAException(XAException.XAER_PROTO);
            }
            
            tInfo.setState(STATE_SUSPENDED);
            break;
            
         case XAResource.TMFAIL:
            if (tInfo.getState() == STATE_UNASSOCIATED)
            {
               throw new XAException(XAException.XAER_PROTO);
            }
            
            tInfo.setState(STATE_UNASSOCIATED);
            break;
            
         case XAResource.TMSUCCESS:
            if (tInfo.getState() == STATE_UNASSOCIATED)
            {
               throw new XAException(XAException.XAER_PROTO);
            }
            
            tInfo.setState(STATE_UNASSOCIATED);
            break;
            
         default:
            throw new XAException(XAException.XAER_INVAL);
      }
      
      m_managedConnection.setCurrentXid(null);
   }


   /**
    * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
    */
   public synchronized void forget(Xid xid) throws XAException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Forgetting Transaction: " + xid + " (in " + this + ")");
      }
      
      //Only asked to forget records that are for recovery.
      if (m_knownXids.contains(xid))
      {
         throw new XAException(XAException.XAER_NOTA);
      }

      // allow equals/hashCode to work correctly
      xid = new RecoveredXid(xid);

      if (m_journal.forget(xid))
      {
         m_journal.writeCompletion(xid);
      }

      completeRecovery();
   }


   /**
    * @see javax.transaction.xa.XAResource#getTransactionTimeout()
    */
   public int getTransactionTimeout() throws XAException
   {
      return 0;
   }


   /**
    * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
    */
   public synchronized boolean isSameRM(XAResource xar) throws XAException
   {
      return (this == xar);
   }


   /**
    * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
    */
   public synchronized int prepare(Xid xid) throws XAException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Preparing Transaction: " + xid + " (in " + this + ")");
      }
      
      if (!m_knownXids.contains(xid))
      {
         throw new XAException(XAException.XAER_NOTA);
      }
      
      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      
      if (tInfo == null)
      {
         throw new XAException(XAException.XAER_NOTA);
      }
      
      //Check to see that commit is possible
      if (m_managedConnection.prepare(xid))
      {
         return XA_OK;
      }
      else
      {
         //Discard all knowledge of branch
         m_managedConnection.rollback(xid);
         m_knownXids.remove(xid);
         
         throw new XAException(XAException.XA_RBROLLBACK);
      }
   }


   /**
    * @see javax.transaction.xa.XAResource#recover(int)
    */
   public synchronized Xid[] recover(int nFlag) throws XAException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("recover(0x" + Integer.toHexString(nFlag) + ") (in " + this + ")");
      }

      if ((nFlag & XAResource.TMSTARTRSCAN) != 0)
      {
         try
         {
            Xid[] indoubtXidArray = m_journal.recover();

            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("In-doubt transaction list: " + Arrays.toString(indoubtXidArray));
            }

            return indoubtXidArray;
         }
         catch (Throwable t)
         {
            s_logger.error("Error recovering in-doubt transactions", t);

            throw ObjUtil.rethrow(t);
         }
      }

      return null;
   }


   /**
    * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
    */
   public synchronized void rollback(Xid xid) throws XAException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Rolling-back Transaction: " + xid + " (in " + this + ")");
      }
      
      if (!m_knownXids.contains(xid))
      {
         //Recovery rollback
         MemoryJournal recovered = m_journal.recoverJournal(xid);

         executeRollback(recovered, xid);
         completeRecovery();

         return;
      }
      
      TransactionInfo tInfo = (TransactionInfo)m_knownXids.get(xid);
      
      if (tInfo == null)
      {
         throw new XAException(XAException.XAER_NOTA);
      }
      
      try
      {
         m_managedConnection.rollback(xid);
      }
      catch (Exception ex)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Got exception during FileConnection.rollback()", ex);
         }
         
         throw new XAException(XAException.XAER_RMERR);
      }
      
      
      m_knownXids.remove(xid);
   }


   /**
    * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
    */
   public boolean setTransactionTimeout(int nSeconds) throws XAException
   {
      if (nSeconds < 0)
      {
         throw new XAException(XAException.XAER_INVAL);
      }

      return true;
   }


   /**
    * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
    */
   public synchronized void start(Xid xid, int nFlag) throws XAException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Starting Transaction: " + xid + " (in " + this + ")");
      }
      
      if (m_managedConnection.getCurrentXid() != null)
      {
         throw new XAException(XAException.XAER_PROTO);
      }
      
      TransactionInfo tInfo = null;
      
      switch (nFlag)
      {
         case XAResource.TMNOFLAGS:
            if (m_knownXids.contains(xid))
            {
               throw new XAException(XAException.XAER_DUPID);
            }
            
            tInfo = new TransactionInfo();
            
            m_knownXids.put(xid, tInfo);
            
            break;


         case XAResource.TMJOIN:
            //If XID not recognized, return XAER_NOTA
            if (!m_knownXids.contains(xid))
            {
               throw new XAException(XAException.XAER_NOTA);
            }
            
            tInfo = (TransactionInfo)m_knownXids.get(xid);
            
            if (tInfo.getState() != STATE_UNASSOCIATED)
            {
               throw new XAException(XAException.XAER_PROTO);
            }
            
            break;


         case XAResource.TMRESUME:
            //If XID not recognized, return XAER_NOTA
            if (!m_knownXids.contains(xid))
            {
               throw new XAException(XAException.XAER_NOTA);
            }
            
            tInfo = (TransactionInfo)m_knownXids.get(xid);
            
            if (tInfo.getState() != STATE_SUSPENDED)
            {
               throw new XAException(XAException.XAER_PROTO);
            }
            
            break;


         default:
            throw new XAException(XAException.XAER_INVAL);
      }
      
      tInfo.setState(STATE_ASSOCIATED);
      m_managedConnection.setCurrentXid(xid);
   }


   /**
    * Closes resources when recovery is finished.
    * 
    * @throws XAException If an error occurs.
    */
   protected void completeRecovery() throws XAException
   {
      if (m_managedConnection.isRecovery() && !m_journal.isInDoubt())
      {
         try
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Nothing more to recover. Destroying managed connection: " +
                  m_managedConnection);
            }

            m_managedConnection.destroy();
         }
         catch (ResourceException ex)
         {
            s_logger.error("Error destroying managed connection after recovery", ex);

            throw new XAException(XAException.XAER_RMERR);
         }
      }
   }


   // inner classes

   /**
    * Holds data relevant to a single transaction.
    */
   protected static class TransactionInfo
   {
      // attributes

      /**
       * The state of this resource manager instance with respect
       * to the transaction, one of the STATE_* constants.
       */
      protected int m_nState;


      // associations

      /**
       * The journal handle for this transaction; or null if no handle
       * has been issued.
       */
      protected MemoryJournal m_journal;


      // operations

      /**
       * Gets the handle into the transactional journal for this transaction.
       * 
       * @return A handle to the transactional journal.
       */
      public MemoryJournal getJournal()
      {
         return m_journal;
      }

      /**
       * Sets the handle into the transactional journal for this transaction.
       * 
       * @param journal A handle to the transactional journal.
       */
      public void setJournal(MemoryJournal journal)
      {
         m_journal = journal;
      }

      /**
       * Gets the state of this resource manager with respect to this transaction.
       * 
       * @return One of the STATE_* constants.
       */
      public int getState()
      {
         return m_nState;
      }

      /**
       * Sets the state of this resource manager with respect to this transaction.
       * 
       * @param nState One of the STATE_* constants.
       */
      public void setState(int nState)
      {
         m_nState = nState;
      }
   }
}
