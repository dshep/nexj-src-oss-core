// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra.tx;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.transaction.xa.Xid;

import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;


/**
 * The in-memory transactional journal, holding an ordered list of transactional
 * operations for a single transaction. It acts as a handle into a persistent
 * journal. Supports adding new records, getting the list of records, and
 * flushing the most-recently added records into the associated persistent
 * journal.
 */
public class MemoryJournal
{
   // attributes

   /**
    * The index of the first operation in the list that has not yet been written
    * out to the persistent journal. All previous entries in the list will have
    * been written; it and all subsequent entries will not have.
    */
   protected int m_firstDirtyIndex;

   /**
    * True if this transaction has been completed; false otherwise.
    */
   protected boolean m_bCompleted;


   // associations

   /**
    * The persistent journal backing this instance.
    */
   protected PersistentJournal m_journal;

   /**
    * An ordered list of the operations being performed by this transaction.
    * 
    * Type: GenericJournalRecord[]
    */
   protected List m_operationsList;

   /**
    * The id of the transaction associated with these operations.
    */
   protected Xid m_xid;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MemoryJournal.class);


   // constructors

   /**
    * Creates a new instance, backed by the given persistent journal.
    * 
    * @param journal The persistent journal backing this instance.
    * @param xid     The transaction id for the operations in this journal.
    */
   public MemoryJournal(PersistentJournal journal, Xid xid)
   {
      m_journal = journal;
      m_xid = xid;
      m_operationsList = new ArrayList(4);
   }


   // operations

   /**
    * Adds a transactional operation to the end of the ordered list.
    * 
    * @param record The transactional operation to add.
    */
   public void addRecord(GenericJournalRecord record)
   {
      assert !m_bCompleted;
      assert ObjUtil.equal(record.getXid(), m_xid);

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("addRecord: " + record);
      }

      m_operationsList.add(record);
   }


   /**
    * Writes as-yet unwritten transactional operations to disk.
    */
   public void flush()
   {
      assert !m_bCompleted;

      if (m_xid == null)
      {
         return;
      }

      int nTotalOpsCount = m_operationsList.size();

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("flush (xid=" + m_xid + ")");
      }

      //Check that flush is required
      if (nTotalOpsCount <= m_firstDirtyIndex)
      {
         return;
      }
      
      int nRecordCount = nTotalOpsCount - m_firstDirtyIndex;
      GenericJournalRecord[] recordArray = new GenericJournalRecord[nRecordCount];
      
      for (int c = 0; c < nRecordCount; c++)
      {
         recordArray[c] = (GenericJournalRecord)m_operationsList.get(m_firstDirtyIndex + c);
      }
      
      m_journal.write(recordArray);
      
      m_firstDirtyIndex = nTotalOpsCount;
   }


   /**
    * Gets the ordered list of transactional operations stored in this instance.
    * 
    * @return
    */
   public List getRecords()
   {
      assert !m_bCompleted;
      return Collections.unmodifiableList(m_operationsList);
   }


   /**
    * Marks this journal instance as completed, destroying this handle into the
    * persistent journal.
    */
   public void completed()
   {
      assert !m_bCompleted;

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("completed (xid=" + m_xid + ")");
      }

      m_journal.completed(m_xid);
      m_bCompleted = true;
   }


   /**
    * Marks this journal instance as completed, destroying this handle into the
    * persistent journal. Used for completing journal handles for transactions
    * needing recovery.
    * 
    * @param xid The transaction id to forget.
    */
   public void forget(Xid xid)
   {
      assert !m_bCompleted;
      assert m_xid.equals(xid);

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("forget (xid=" + xid + ")");
      }

      m_journal.forget(xid);
      m_bCompleted = true;
   }
}
