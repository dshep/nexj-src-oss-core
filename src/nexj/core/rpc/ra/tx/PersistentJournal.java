// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra.tx;

import java.io.ByteArrayOutputStream;
import java.io.CharConversionException;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OptionalDataException;
import java.io.OutputStream;
import java.io.StreamCorruptedException;
import java.io.UTFDataFormatException;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.JournalManagementException;
import nexj.core.rpc.file.ra.LockableFile;
import nexj.core.util.Binary;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.WeakHashTab;

/**
 * A disk-based transactional journal. There can only be one instance
 * reading/writing a journal file in a given process: this guaranteed by
 * using the static factory method makeJournal. The journal cannot be
 * shared by more than one process. 
 */
public class PersistentJournal
{
   // constants

   /**
    * Default size at which journal should be compacted.
    */
   public final static long DEFAULT_MAX_JOURNAL_SIZE = 64 * 1024;

   /**
    * The two-byte version code for journal files created by this class.
    */
   protected final static byte[] JOURNAL_VERSION = {'A', '4'};

   /**
    * The name of the first journal file.
    */
   protected final static String JOURNAL_FILE1_NAME = "journal1.bin";

   /**
    * The name of the second journal file.
    */
   protected final static String JOURNAL_FILE2_NAME = "journal2.bin";

   /**
    * The name of the file that contains the pointer to the active
    * journal.
    */
   protected final static String JOURNAL_POINTER_FILE_NAME = "active.bin";

   // attributes

   /**
    * The size at which the journal should be compacted.
    */
   protected long m_lMaxSize = DEFAULT_MAX_JOURNAL_SIZE;

   /**
    * The count of references to a persistent journal instance.
    */
   protected int m_nRefCount;

   // associations

   /**
    * The file used to store the transactional operations.
    */
   protected LockableFile m_journal;

   /**
    * The file used for compaction.
    */
   protected LockableFile m_compactionJournal;

   /**
    * The path to the file used to store the transactional operations.
    */
   protected File m_journalFile;

   /**
    * The path to the file used for compaction. 
    */
   protected File m_compactionJournalFile;

   /**
    * The path to the file that contains information about which journal file is
    * the active journal file.
    */
   protected File m_activeJournalPointerFile;

   /**
    * Mapping of journal file paths to their PersistentJournal instances.
    * 
    * Type: JournalInfo[File]
    */
   protected static Lookup s_journalRegistry = new WeakHashTab();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(PersistentJournal.class);

   /**
    * The set of in-doubt transaction ids.
    */
   protected Holder m_indoubtXids;

   /**
    * The set of active transaction ids.
    */
   protected Holder m_activeXids;


   // constructors

   /**
    * Creates a new persistent journal instance.
    * 
    * @param journalDirectory The directory to use for storing/retrieving the journalled
    *                    transactional operations.
    */
   private PersistentJournal(File journalDirectory)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Opening journal at: " + journalDirectory.getAbsolutePath() + " (instance: " + this + ")");
      }

      if (!journalDirectory.exists())
      {
         if (!journalDirectory.mkdirs())
         {
            throw new JournalManagementException("err.rpc.file.journalIoErr",
               new Object[] {journalDirectory.getAbsolutePath()});
         }
      }

      m_journalFile = new File(journalDirectory, JOURNAL_FILE1_NAME);
      m_compactionJournalFile = new File(journalDirectory, JOURNAL_FILE2_NAME);
      m_activeJournalPointerFile = new File(journalDirectory, JOURNAL_POINTER_FILE_NAME);
      m_journal = new LockableFile(m_journalFile);
      m_compactionJournal = new LockableFile(m_compactionJournalFile);

      boolean bReady = false;

      try
      {
         if (!m_journal.tryLock(false))
         {
            throw new IllegalStateException("Journal \"" + m_journalFile.getAbsolutePath() + "\" is already locked.");
         }

         if (!m_compactionJournal.tryLock(false))
         {
            throw new IllegalStateException("Journal \"" + m_compactionJournalFile.getAbsolutePath() + "\" is already locked.");
         }

         boolean bSecondFileIsActive = true;

         if (m_activeJournalPointerFile.exists())
         {
            //Read the active journal pointer.
            try
            {
               FileInputStream activePointerStream = new FileInputStream(m_activeJournalPointerFile);

               try
               {
                  bSecondFileIsActive = activePointerStream.read() == 2;
               }
               finally
               {
                  IOUtil.close(activePointerStream);
               }
            }
            catch (IOException ex)
            {
               throw new JournalManagementException("err.rpc.file.journalIoErr",
                  new Object[] {m_activeJournalPointerFile.getAbsolutePath()}, ex);
            }
         }
         else
         {
            writeActiveJournalPointer((byte)2);
         }

         if (bSecondFileIsActive)
         {
            swapJournalFiles();
         }

         //Check/write version code
         try
         {
            if (m_journalFile.length() >= JOURNAL_VERSION.length)
            {
               InputStream istream = m_journal.getInputStream();
               byte[] nVersionCodeArray = new byte[JOURNAL_VERSION.length];

               istream.read(nVersionCodeArray);

               if (Binary.compare(nVersionCodeArray, JOURNAL_VERSION) != 0)
               {
                  throw new JournalManagementException("err.rpc.file.versionMismatch",
                     new Object[]{new Binary(JOURNAL_VERSION).toString(),
                        new Binary(nVersionCodeArray).toString(),
                        journalDirectory.getAbsolutePath()});
               }
            }
            else
            {
               m_journal.writeData(new Binary(JOURNAL_VERSION));
            }
         }
         catch (IOException ex)
         {
            throw new JournalManagementException("err.rpc.file.journalIoErr",
               new Object[] {m_journalFile.getAbsolutePath()}, ex);
         }

         m_indoubtXids = recoverInternal();
         m_activeXids = new HashHolder();
         forceCompact();
         bReady = true;
      }
      finally
      {
         if (!bReady)
         {
            //Close handles so that admin can correct error without restarting app
            if (m_journal != null)
            {
               m_journal.close();
            }

            if (m_compactionJournal != null)
            {
               m_compactionJournal.close();
            }
         }
      }
   }


   /**
    * Warns about instances that were not closed properly.
    * 
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      if (m_journalFile != null)
      {
         if (s_logger.isWarnEnabled())
         {
            s_logger.warn("PersistentJournal(\"" + m_journalFile.getAbsolutePath() + "\") was never closed (instance: " + super.toString() + ")");
         }
      }
      
      super.finalize();
   }


   // operations

   /**
    * Informs a journal instance that it is no longer in use by a particular user.
    * When a journal is no longer in use by anyone, this method will close it.
    * 
    * @param journalDirectory The journal directory.
    */
   public void close()
   {
      assert m_journalFile != null;

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Decrementing reference to journal instance: " + this + " on file: " + m_journalFile.getAbsolutePath());
      }

      File journalDirectory = m_journalFile.getParentFile();
      
      synchronized (s_journalRegistry)
      {
         PersistentJournal journal = (PersistentJournal)s_journalRegistry.get(journalDirectory);
         
         journal.decRefCount();
         
         if (journal.getRefCount() == 0)
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Closing journal on \"" + m_journalFile.getAbsolutePath() + "\"");
            }

            s_journalRegistry.remove(journalDirectory);
            
            m_journal.unlock();
            m_journal.close();
            m_journalFile = null;
            m_journal = null;
            
            m_compactionJournal.unlock();
            m_compactionJournal.close();
            m_compactionJournalFile = null;
            m_compactionJournal = null;
         }
      }
   }


   /**
    * Static factory method to create or retrieve a PersistentJournal instance
    * for the given file path. Enforces only one persistent journal instance
    * per journal file.
    * 
    * @param journalDirectory The journal directory.
    * @return A persistent journal backed by the given journal file.
    */
   public static PersistentJournal makeJournal(File journalDirectory)
   {
      synchronized (s_journalRegistry)
      {
         PersistentJournal result = (PersistentJournal)s_journalRegistry.get(journalDirectory);
         
         if (result == null)
         {
            result = new PersistentJournal(journalDirectory);
            result.incRefCount();
            
            s_journalRegistry.put(journalDirectory, result);
         }
         else
         {
            result.incRefCount();
         }
         
         return result;
      }
   }


   /**
    * Gets a handle to use on this journal.
    * 
    * @return A handle to use for recording the operations of a single transaction.
    */
   public synchronized MemoryJournal getJournal(Xid xid)
   {
      compact();
      
      if (xid != null && !m_activeXids.add(xid))
      {
         throw new IllegalStateException("Cannot have more than one handle for same xid = \"" + xid + "\"");
      }
      
      return new MemoryJournal(this, xid);
   }

   /**
    * Compacts the journal, if journal size exceeds the maximum journal size.
    */
   protected synchronized void compact()
   {
      if (m_journalFile.length() <= m_lMaxSize)
      {
         return;
      }

      forceCompact();
   }

   /**
    * Compacts the journal.
    * 
    * Compaction is performed by scanning the journal, writing the active and
    * in-doubt records to the compaction journal file, and then using the
    * compaction journal as the active journal from that point forward.
    */
   protected synchronized void forceCompact()
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Compacting journal... (\"" + m_journalFile.getAbsolutePath() + "\")");
      }
      
      ObjectOutputStream oos = null;

      try
      {
         m_compactionJournal.truncate();
         m_compactionJournal.writeData(new Binary(JOURNAL_VERSION));
         
         OutputStream ostream = m_compactionJournal.getOutputStream();
         oos = new ObjectOutputStream(ostream);
         int nPreservedCount = 0;
         int nDiscardedCount = 0;
         RecordIterator itr = new RecordIterator(m_journal);
         
         while (itr.hasNext())
         {
            GenericJournalRecord currentRec = (GenericJournalRecord)itr.next();
            Xid currentXid = currentRec.getXid();
            
            if (m_activeXids.contains(currentXid) || m_indoubtXids.contains(currentXid))
            {
               nPreservedCount++;
               oos.writeObject(currentRec);
            }
            else
            {
               nDiscardedCount++;
            }
         }
         
         itr.close();
         
         //Finish writing new journal, ensuring that records have been written to disk
         oos.writeObject(null);
         oos.flush();
         
         //Swap the journal pointer on-disk
         writeActiveJournalPointer((m_compactionJournalFile.getName().equals(JOURNAL_FILE1_NAME)) ? (byte)1 : (byte)2);
         
         //Clears the old journal (must be done *after* swapping journal pointer on-disk)
         m_journal.writeData(new Binary(JOURNAL_VERSION));
         
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Compaction done: " + nDiscardedCount + " records discarded, " + nPreservedCount + " preserved.");
         }
         
         swapJournalFiles();

         oos.close();
      }
      catch (IOException ex)
      {
         if (oos != null)
         {
            try
            {
               oos.close();
            }
            catch (IOException ex2)
            {
               // Suppress it because an I/O error will already be thrown.
            }
         }

         throw new JournalManagementException("err.rpc.file.journalIoErr",
            new Object[] {m_journalFile}, ex);
      }
   }


   /**
    * Called by a MemoryJournal handle into this journal when it is destroyed.
    */
   public synchronized void completed(Xid xid)
   {
      if (xid != null && !m_activeXids.remove(xid))
      {
         throw new IllegalStateException("Corrupted in-memory journal state: Unknown xid = \"" + xid + "\"");
      }
      
      compact();
   }


   /**
    * Forgets the in-doubt transaction with the given transaction id.
    * 
    * @param xid The id of the transaction to forget.
    * @return    True if there is an in-doubt transaction with the given id;
    *            false otherwise.
    */
   public synchronized boolean forget(Xid xid)
   {
      // ensure equals/hashCode work correctly
      if (!(xid instanceof RecoveredXid))
      {
         xid = new RecoveredXid(xid);
      }

      if (m_indoubtXids.remove(xid))
      {
         compact();
         return true;
      }
      
      return false;
   }


   /**
    * Writes a completion record to the journal for the given transaction id,
    * indication that the transaction has completed (and will not require
    * recovery).
    * 
    * @param xid The id of the transaction for which completion should be
    *            recorded.
    */
   public synchronized void writeCompletion(Xid xid)
   {
      assert xid != null;

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("writeCompletion (xid = " + xid + ")");
      }

      JournalRecordCompleted completionRecord = new JournalRecordCompleted(xid);
      
      try
      {
         ByteArrayOutputStream byteOStream = new ByteArrayOutputStream();
         ObjectOutputStream oos = new ObjectOutputStream(byteOStream);

         oos.writeObject(completionRecord);
         oos.writeObject(null);
         oos.close();

         m_journal.appendData(new Binary(byteOStream.toByteArray()));
         m_journal.force();
      }
      catch (IOException ex)
      {
         throw new JournalManagementException("err.rpc.file.journalIoErr",
            new Object[] {m_journalFile.getAbsolutePath()}, ex);
      }
   }


   /**
    * Writes the given records of transactional operations to the end of
    * the journal file.
    * 
    * @param recordArray The transactional operations to write.
    */
   public synchronized void write(GenericJournalRecord[] recordArray)
   {
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("write: " + Arrays.toString(recordArray));
      }

      if (recordArray == null || recordArray.length == 0)
      {
         return;
      }
      
      try
      {
         ByteArrayOutputStream byteOStream = new ByteArrayOutputStream();
         ObjectOutputStream oos = new ObjectOutputStream(byteOStream);
         
         for (int i = 0; i < recordArray.length; i++)
         {
            recordArray[i].initChecksum();
            oos.writeObject(recordArray[i]);
         }
         
         oos.writeObject(null);
         oos.close();
         m_journal.appendData(new Binary(byteOStream.toByteArray()));
         m_journal.force();
      }
      catch (IOException ex)
      {
         throw new JournalManagementException("err.rpc.file.journalIoErr",
            new Object[] {m_journalFile.getAbsolutePath()}, ex);
      }
   }


   /**
    * Writes a single byte to the activeJournalPointerFile to indicate which
    * journal file is the active journal file.
    */
   protected void writeActiveJournalPointer(byte nJournalId)
   {
      assert nJournalId == 1 || nJournalId == 2;

      try
      {
         FileOutputStream activePointerStream = new FileOutputStream(m_activeJournalPointerFile);
   
         try
         {
            activePointerStream.write(nJournalId);
         }
         finally
         {
            activePointerStream.close();
         }
      }
      catch (IOException ex)
      {
         throw new JournalManagementException("err.rpc.file.journalIoErr",
            new Object[] {m_activeJournalPointerFile}, ex);
      }
   }


   /**
    * Gets whether or not there are any in-doubt transactions found in the journal.
    * 
    * @return True if journal contains in-doubt transactions; false otherwise.
    */
   public boolean isInDoubt()
   {
      return (m_indoubtXids != null && !m_indoubtXids.isEmpty());
   }


   /**
    * Gets the ids of all of the in-doubt transactions found in the journal.
    * 
    * @return A list of in-doubt transaction ids.
    */
   public RecoveredXid[] recover()
   {
      if (!isInDoubt())
      {
         return new RecoveredXid[0];
      }

      //Construct result array
      RecoveredXid[] result = new RecoveredXid[m_indoubtXids.size()];
      int i = 0;

      for (Iterator itr = m_indoubtXids.iterator(); itr.hasNext(); )
      {
         result[i++] = (RecoveredXid)itr.next();
      }

      return result;
   }


   /**
    * Scans the entire journal file, looking for in-doubt transactions. (Transactions
    * that have been PREPARED but not COMPLETED).
    * 
    * @return The set of in-doubt transaction Xids.
    */
   protected synchronized Holder recoverInternal()
   {
      Holder indoubtSet = new HashHolder();
      RecordIterator itr = new RecordIterator(m_journal);

      while (itr.hasNext())
      {
         GenericJournalRecord currentRec = (GenericJournalRecord)itr.next();
         Xid currentXid = currentRec.getXid();

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("Recovering record: " + currentRec);
         }

         if (currentRec.getOpCode() == JournalRecordPrepared.OPCODE)
         {
            indoubtSet.add(currentXid);
         }
         else if (currentRec.getOpCode() == JournalRecordCompleted.OPCODE)
         {
            indoubtSet.remove(currentXid);
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Recovered journal, in-doubt set: " + indoubtSet);
      }

      return indoubtSet;
   }


   /**
    * Gets a MemoryJournal handle for the given transaction id, loading
    * the list of transactional operations for that id from the journal
    * file.
    * 
    * @param xid The id of the transaction whose operations should be
    *            loaded into the handle.
    * @return A MemoryJournal handle containing the transactional
    *         operations for the given XID.
    */
   public synchronized MemoryJournal recoverJournal(Xid xid)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("recoverJournal (xid =" + xid + ") (instance: " + this + ")");
      }

      List operationsList = new ArrayList();
      RecordIterator itr = new RecordIterator(m_journal);
      
      while (itr.hasNext())
      {
         GenericJournalRecord currentRec = (GenericJournalRecord)itr.next();
         Xid currentXid = currentRec.getXid();
         
         if (currentXid.equals(xid))
         {
            operationsList.add(currentRec);
         }
      }
      
      itr.close();
      
      MemoryJournal result = null;
      
      if (operationsList.size() > 0)
      {
         result = new MemoryJournal(this, xid);
         
         result.m_operationsList = operationsList;
         result.m_firstDirtyIndex = operationsList.size();
      }
      
      return result;
   }


   /**
    * Used for TESTING PURPOSES ONLY. Closes all PersistentJournals in this
    * process. Simulates the effect of restarting the process.
    */
   public static void closeAll()
   {
      synchronized (s_journalRegistry)
      {
         for (Iterator itr = s_journalRegistry.valueIterator(); itr.hasNext(); )
         {
            PersistentJournal journal = (PersistentJournal)itr.next();

            if (journal == null)
            {
               continue;
            }

            if (journal.m_journal != null)
            {
               journal.m_journal.unlock();
               journal.m_journal.close();
            }

            journal.m_journal = null;
            journal.m_journalFile = null;

            if (journal.m_compactionJournal != null)
            {
               journal.m_compactionJournal.unlock();
               journal.m_compactionJournal.close();
            }

            journal.m_compactionJournal = null;
            journal.m_compactionJournalFile = null;
         }
         
         s_journalRegistry.clear();
      }
   }


   /**
    * Sets the journal file size threshold at which compaction should occur.
    * 
    * @param lMaxSize The size which, if exceeded, triggers journal file compaction.
    */
   public void setCompactionThreshold(long lMaxSize)
   {
      m_lMaxSize = Math.max(lMaxSize, JOURNAL_VERSION.length);
   }


   /**
    * Swaps the handles 
    */
   protected void swapJournalFiles()
   {
      LockableFile handle;
      File file;
      
      handle = m_journal;
      m_journal = m_compactionJournal;
      m_compactionJournal = handle;
      
      file = m_journalFile;
      m_journalFile = m_compactionJournalFile;
      m_compactionJournalFile = file;
   }


   /**
    * Gets the path to the active journal file. FOR TESTING PURPOSES ONLY.
    * 
    * @return The active journal.
    */
   public File getActiveJournal()
   {
      return m_journalFile;
   }


   /**
    * @return The number of active transactions in the journal.
    */
   public int getActiveTransactions()
   {
      return m_activeXids.size();
   }


   /**
    * @return The number of in-doubt transactions in the journal.
    */
   public int getIndoubtTransactions()
   {
      return m_indoubtXids.size();
   }


   /**
    * Gets the reference count.
    * 
    * @return The number of references to this journal instance.
    */
   public int getRefCount()
   {
      return m_nRefCount;
   }


   /**
    * Increments the reference count.
    */
   public void incRefCount()
   {
      m_nRefCount++;
   }


   /**
    * Decrements the reference count.
    */
   public void decRefCount()
   {
      assert m_nRefCount > 0;
      m_nRefCount--;
   }


   // inner classes

   /**
    * This is used internally to iterate through the records in the
    * journal file.
    */
   protected static class RecordIterator implements Iterator
   {
      // attributes

      boolean m_bHasNext;


      // associations

      /**
       * The underlying input stream for reading from the journal file.
       */
      InputStream m_istream;

      /**
       * The object input stream for deserialization.
       */
      ObjectInputStream m_ois;

      /**
       * The next record.
       */
      GenericJournalRecord m_nextRecord;


      // constructors

      /**
       * Creates a record iterator on the given file handle.
       * 
       * @param journal The journal file to iterate over.
       */
      public RecordIterator(LockableFile journal)
      {
         try
         {
            m_istream = journal.getInputStream();
            m_ois = null;
            m_bHasNext = true;
            
            //Consume journal version code
            m_istream.skip(JOURNAL_VERSION.length);
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
         }
         
         m_nextRecord = internalNext();
      }


      // operations

      /**
       * Cleans up the streams associated with this iterator.
       */
      public void close()
      {
         IOUtil.close(m_ois);
         IOUtil.close(m_istream);
         m_nextRecord = null;
      }


      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_bHasNext;
      }


      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_bHasNext)
         {
            GenericJournalRecord current = m_nextRecord;
            
            m_nextRecord = internalNext();
            return current;
         }
         else
         {
            throw new NoSuchElementException();
         }
      }


      /**
       * Advances to the next journal record.
       * 
       * @return The next journal record; null if EOF.
       */
      private GenericJournalRecord internalNext()
      {
         try
         {
            GenericJournalRecord currentRec = null;
            
            while (currentRec == null)
            {
               if (m_ois == null)
               {
                  m_ois = new ObjectInputStream(m_istream);
               }
               
               currentRec = (GenericJournalRecord)m_ois.readObject();
               
               if (currentRec == null)
               {
                  m_ois.close();
                  m_ois = null;
               }
            }

            // Invalid record is end-of-journal
            if (currentRec != null && !currentRec.isValid())
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Detected EOF on journal via record checksum for: " + currentRec);
               }

               m_bHasNext = false;
               currentRec = null;
            }

            return currentRec;
         }
         catch (EOFException ex)
         {
            m_bHasNext = false;
         }
         catch (OptionalDataException ex)
         {
            m_bHasNext = false;
         }
         catch (StreamCorruptedException ex)
         {
            m_bHasNext = false;
         }
         catch (UTFDataFormatException ex)
         {
            m_bHasNext = false;
         }
         catch (CharConversionException ex)
         {
            m_bHasNext = false;
         }
         catch (CharacterCodingException ex)
         {
            m_bHasNext = false;
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
         }
         catch (ClassNotFoundException ex)
         {
            m_bHasNext = false;
         }

         return null;
      }


      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         throw new UnsupportedOperationException();
      }
   }
}
