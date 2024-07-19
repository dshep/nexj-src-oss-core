// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyInsert;
import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyUpdate;
import nexj.core.rpc.file.ra.tx.FileJournalRecordDelete;
import nexj.core.rpc.file.ra.tx.FileJournalRecordIncomingMove;
import nexj.core.rpc.ra.tx.GenericJournalRecord;
import nexj.core.rpc.ra.tx.JournalRecordCompleted;
import nexj.core.rpc.ra.tx.JournalRecordPrepared;
import nexj.core.rpc.ra.tx.MemoryJournal;
import nexj.core.rpc.ra.tx.PersistentJournal;
import nexj.core.rpc.ra.tx.RecoveredXid;
import nexj.test.util.TempFileUtil;

import junit.framework.TestCase; 


public class AppendingJournalTest extends TestCase
{
   public void setUp() throws Exception
   {
      PersistentJournal.closeAll();
      JournalRecordTesting.setSetChecksum(true);
   }
   
   /**
    * Tests the recover method of the persistent journal, which scans the journal to
    * find in-doubt transactions.
    * 
    * 1) Tests that in-doubt transactions can be identified
    * 2) Tests that transactions can be retrieved from the journal.
    */
   public void testRecover() throws Exception
   {
      for (int i = 0; i < 2; i++)
      {
         JournalRecordTesting.setSetChecksum(i == 0);

         File tempDirectory = TempFileUtil.makeTemporaryDirectory(getClass().getName() + "." + getName());
         PersistentJournal perJournal = PersistentJournal.makeJournal(tempDirectory);
         
         //A record that needs to be recovered. (XID1)
         Xid xid1 = makeXid(1);
         MemoryJournal journal1 = perJournal.getJournal(xid1);
         
         journal1.addRecord(new JournalRecordTesting(xid1, FileJournalRecordCopyInsert.OPCODE));
         journal1.addRecord(new JournalRecordTesting(xid1, JournalRecordPrepared.OPCODE));
         journal1.flush();
         journal1.completed();
         journal1 = null;
         
         
         //Completed record (XID2) interleaved with record needing recovery (XID3)
         Xid xid2 = makeXid(2);
         Xid xid3 = makeXid(3);
         MemoryJournal journal2a = perJournal.getJournal(xid2);
         MemoryJournal journal2b = perJournal.getJournal(xid3);
         
         journal2a.addRecord(new JournalRecordTesting(xid2, FileJournalRecordCopyUpdate.OPCODE));
         journal2a.flush();
         journal2b.addRecord(new JournalRecordTesting(xid3, FileJournalRecordCopyUpdate.OPCODE));
         journal2b.flush();
         journal2a.addRecord(new JournalRecordTesting(xid2, JournalRecordPrepared.OPCODE));
         journal2a.flush();
         journal2b.addRecord(new JournalRecordTesting(xid3, JournalRecordPrepared.OPCODE));
         journal2b.flush();
         journal2a.addRecord(new JournalRecordTesting(xid2, JournalRecordCompleted.OPCODE));
         journal2a.flush();
         journal2a.completed();
         journal2b.completed();
         journal2a = null;
         journal2b = null;
         
         
         //Repeated instructions; completed (XID4)
         Xid xid4 = makeXid(4);
         MemoryJournal journal3 = perJournal.getJournal(xid4);
         
         journal3.addRecord(new JournalRecordTesting(xid4, FileJournalRecordDelete.OPCODE));
         journal3.addRecord(new JournalRecordTesting(xid4, JournalRecordPrepared.OPCODE));
         journal3.addRecord(new JournalRecordTesting(xid4, JournalRecordPrepared.OPCODE));
         journal3.addRecord(new JournalRecordTesting(xid4, JournalRecordCompleted.OPCODE));
         journal3.addRecord(new JournalRecordTesting(xid4, JournalRecordPrepared.OPCODE));
         journal3.addRecord(new JournalRecordTesting(xid4, JournalRecordCompleted.OPCODE));
         journal3.flush();
         journal3.completed();
         journal3 = null;
         
         
         //Extraneous instructions; un-prepared (XID5), completed (XID6)
         Xid xid5 = makeXid(5);
         Xid xid6 = makeXid(6);
         MemoryJournal journal4a = perJournal.getJournal(xid5);
         MemoryJournal journal4b = perJournal.getJournal(xid6);
         
         journal4a.addRecord(new JournalRecordTesting(xid5, FileJournalRecordCopyInsert.OPCODE));
         journal4a.flush();
         journal4b.addRecord(new JournalRecordTesting(xid6, FileJournalRecordIncomingMove.OPCODE));
         journal4b.flush();
         journal4a.addRecord(new JournalRecordTesting(xid5, FileJournalRecordCopyUpdate.OPCODE));
         journal4a.flush();
         journal4b.addRecord(new JournalRecordTesting(xid6, JournalRecordPrepared.OPCODE));
         journal4b.flush();
         journal4a.addRecord(new JournalRecordTesting(xid5, FileJournalRecordDelete.OPCODE));
         journal4a.flush();
         journal4b.addRecord(new JournalRecordTesting(xid6, JournalRecordCompleted.OPCODE));
         journal4b.flush();
         journal4a.completed();
         journal4b.completed();
         journal4a = null;
         journal4b = null;
         
         
         //Just a PREPARED record by itself; recover (XID7)
         Xid xid7 = makeXid(7);
         MemoryJournal journal5 = perJournal.getJournal(xid7);
         
         journal5.addRecord(new JournalRecordTesting(xid7, JournalRecordPrepared.OPCODE));
         journal5.flush();
         journal5.completed();
         journal5 = null;
         
         
         //Just PREPARED and COMPLETED; completed (XID8)
         Xid xid8 = makeXid(8);
         MemoryJournal journal6 = perJournal.getJournal(xid8);
         
         journal6.addRecord(new JournalRecordTesting(xid8, JournalRecordPrepared.OPCODE));
         journal6.addRecord(new JournalRecordTesting(xid8, JournalRecordCompleted.OPCODE));
         journal6.flush();
         journal6.completed();
         journal6 = null;
         
         
         //Just COMPLETED; completed (XID9)
         Xid xid9 = makeXid(9);
         MemoryJournal journal7 = perJournal.getJournal(xid9);
         
         journal7.addRecord(new JournalRecordTesting(xid9, JournalRecordCompleted.OPCODE));
         journal7.flush();
         journal7.completed();
         journal7 = null;
         
         
         
         /* ********** CRASH ********** */
         PersistentJournal.closeAll();
         perJournal = null;
         
         
         
         //Recovery
         PersistentJournal recoverJournal = PersistentJournal.makeJournal(tempDirectory);
         Xid[] recoveredXids = recoverJournal.recover();
         
         assertEquals(3, recoveredXids.length);
         Arrays.sort(recoveredXids);
         assertEquals(makeXid(1), recoveredXids[0]);
         assertEquals(makeXid(3), recoveredXids[1]);
         assertEquals(makeXid(7), recoveredXids[2]);
         
         
         //Test getting some operations
         MemoryJournal recovered = recoverJournal.recoverJournal(makeXid(1));
         List recoveredOps = recovered.getRecords();
         
         //A record that needs to be recovered. (XID1)
         assertEquals(2, recoveredOps.size());
         assertEquals(new JournalRecordTesting(makeXid(1), FileJournalRecordCopyInsert.OPCODE), recoveredOps.get(0));
         assertEquals(new JournalRecordTesting(makeXid(1), JournalRecordPrepared.OPCODE), recoveredOps.get(1));
         recovered.forget(makeXid(1));
         
         //Completed record (XID2) interleaved with record needing recovery (XID3)
         recovered = recoverJournal.recoverJournal(makeXid(3));
         recoveredOps = recovered.getRecords();
         assertEquals(2, recoveredOps.size());
         assertEquals(new JournalRecordTesting(makeXid(3), FileJournalRecordCopyUpdate.OPCODE), recoveredOps.get(0));
         assertEquals(new JournalRecordTesting(makeXid(3), JournalRecordPrepared.OPCODE), recoveredOps.get(1));
         recovered.forget(makeXid(3));
         
         recoverJournal.close();
         PersistentJournal.closeAll();
      }
   }


   public void testJournalCompaction() throws Exception
   {
      File tempDirectory = TempFileUtil.makeTemporaryDirectory(getClass().getName() + "." + getName());
      PersistentJournal perJournal = PersistentJournal.makeJournal(tempDirectory);
      MemoryJournal j1, j2, j3;
      File tempFile1 = new File(tempDirectory, "journal1.bin");
      File tempFile2 = new File(tempDirectory, "journal2.bin");
      final int nHeaderLength = 2;
      final int nZeroObjectsLength = 7;

      assertEquals(nZeroObjectsLength, tempFile1.length());
      assertEquals(nHeaderLength, tempFile2.length());
      perJournal.setCompactionThreshold(nZeroObjectsLength);
      
      //Just one journal handle: should compact quickly!
      j1 = perJournal.getJournal(makeXid(1));  //NO COMPACT
      assertEquals(1, perJournal.getActiveTransactions());
      assertEquals(tempFile1, perJournal.getActiveJournal());
      j1.addRecord(new JournalRecordTesting(makeXid(1), FileJournalRecordCopyInsert.OPCODE));
      j1.addRecord(new JournalRecordTesting(makeXid(1), JournalRecordPrepared.OPCODE));
      j1.addRecord(new JournalRecordTesting(makeXid(1), JournalRecordCompleted.OPCODE));
      assertEquals(nZeroObjectsLength, tempFile1.length());
      j1.flush();
      assertTrue(tempFile1.length() > nZeroObjectsLength);
      j1.completed();  //COMPACT
      assertEquals(0, perJournal.getActiveTransactions());
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nZeroObjectsLength, tempFile2.length());
      assertEquals(nHeaderLength, tempFile1.length());
      j1 = null;
      
      
      //Test handles overlapping in time:
      //---j1---COMPACT!
      //     ---j2---COMPACT!
      //          ---j3--->COMPACT!
      j1 = perJournal.getJournal(makeXid(2));  //NO COMPACT
      assertEquals(1, perJournal.getActiveTransactions());
      assertEquals(tempFile2, perJournal.getActiveJournal());
      j1.addRecord(new JournalRecordTesting(makeXid(2), FileJournalRecordCopyInsert.OPCODE));
      j1.flush();
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertTrue(tempFile2.length() > nZeroObjectsLength);
      assertEquals(nHeaderLength, tempFile1.length());
      
      j2 = perJournal.getJournal(makeXid(3));  //COMPACT
      assertEquals(tempFile1, perJournal.getActiveJournal());
      assertTrue(tempFile1.length() > nZeroObjectsLength);  //first record still active!
      assertEquals(nHeaderLength, tempFile2.length());
      j2.addRecord(new JournalRecordTesting(makeXid(3), FileJournalRecordDelete.OPCODE));
      j2.flush();
      j1.completed();  //COMPACT
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile1.length());
      assertTrue(tempFile2.length() > nZeroObjectsLength);  //second record still active!
      j1 = null;
      
      j3 = perJournal.getJournal(makeXid(4));  //COMPACT
      assertEquals(tempFile1, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile2.length());
      assertTrue(tempFile1.length() > nZeroObjectsLength);  //second record still active!
      j2.completed();  //COMPACT
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile1.length());
      assertEquals(nZeroObjectsLength, tempFile2.length());  //third record still active, but is null!
      j2 = null;
      j3.completed();  //NO COMPACT
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile1.length());
      assertEquals(nZeroObjectsLength, tempFile2.length());
      j3 = null;
      
      
      //Test no compaction until crash recovered
      //---j1--- CRASH
      //               RESTART ---j2---
      //                                forget(j1) COMPACT
      j1 = perJournal.getJournal(makeXid(5));  //NO COMPACT
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile1.length());
      assertEquals(nZeroObjectsLength, tempFile2.length());
      
      j1.addRecord(new JournalRecordTesting(makeXid(5), FileJournalRecordCopyInsert.OPCODE));
      j1.addRecord(new JournalRecordTesting(makeXid(5), JournalRecordPrepared.OPCODE));
      j1.flush();
      assertTrue(tempFile2.length() > nZeroObjectsLength);

      //CRASH
      PersistentJournal.closeAll();
      j1 = null;
      perJournal = null;
      
      perJournal = PersistentJournal.makeJournal(tempDirectory);
      perJournal.setCompactionThreshold(nZeroObjectsLength);
      assertEquals(tempFile1, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile2.length());
      assertTrue(tempFile1.length() > nZeroObjectsLength);
      
      j2 = perJournal.getJournal(makeXid(6));  //COMPACT
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile1.length());
      assertTrue(tempFile2.length() > nZeroObjectsLength);
      j2.addRecord(new JournalRecordTesting(makeXid(6), FileJournalRecordCopyUpdate.OPCODE));
      j2.addRecord(new JournalRecordTesting(makeXid(6), JournalRecordPrepared.OPCODE));
      j2.addRecord(new JournalRecordTesting(makeXid(6), JournalRecordCompleted.OPCODE));
      j2.flush();
      assertEquals(tempFile2, perJournal.getActiveJournal());
      j2.completed();  //COMPACT
      assertEquals(tempFile1, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile2.length());
      assertTrue(tempFile1.length() > nZeroObjectsLength);
      j2 = null;
      
      j1 = perJournal.recoverJournal(makeXid(5));
      assertNotNull(j1);
      
      List opsList = j1.getRecords();
      
      assertEquals(2, opsList.size());
      assertEquals(new JournalRecordTesting(makeXid(5), FileJournalRecordCopyInsert.OPCODE), opsList.get(0));
      assertEquals(new JournalRecordTesting(makeXid(5), JournalRecordPrepared.OPCODE), opsList.get(1));
      
      //FORGET
      assertEquals(tempFile1, perJournal.getActiveJournal());
      j1.addRecord(new JournalRecordTesting(makeXid(5), JournalRecordCompleted.OPCODE));
      j1.flush();
      assertEquals(tempFile1, perJournal.getActiveJournal());
      j1.forget(makeXid(5));  //COMPACT
      assertEquals(tempFile2, perJournal.getActiveJournal());
      assertEquals(nHeaderLength, tempFile1.length());
      assertEquals(nZeroObjectsLength, tempFile2.length());
      j1 = null;
      
      perJournal.close();
   }


   // helper methods

   /**
    * Creates a transactional id that can be used for testing purposes.
    * 
    * @param nXidNumber A number to distinguish transactional ids created for testing.
    *                   Passing the same number to two different invocations of this
    *                   method will create equivalent Xids.
    * @return An xid.
    */
   protected Xid makeXid(int nXidNumber)
   {
      byte[] nTxIdArray = new byte[RecoveredXid.MAXGTRIDSIZE];
      byte[] nBrQArray = new byte[RecoveredXid.MAXBQUALSIZE];
      
      Arrays.fill(nTxIdArray, (byte)'G');
      nTxIdArray[0] = (byte)((nXidNumber >> 24) & 0xff);
      nTxIdArray[1] = (byte)((nXidNumber >> 16) & 0xff);
      nTxIdArray[2] = (byte)((nXidNumber >> 8) & 0xff);
      nTxIdArray[3] = (byte)(nXidNumber & 0xff);
      
      Arrays.fill(nBrQArray, (byte)'B');
      
      return new RecoveredXid(nTxIdArray, nBrQArray, 0xcafecafe);
   }


   // inner classes

   /**
    * A journal record class to use for testing the transactional journal.
    */
   public static class JournalRecordTesting extends GenericJournalRecord
   {
      // constants

      /**
       * The serialization version.
       * @see java.io.Serializable
       */
      private static final long serialVersionUID = -9105163450027534724L;

      // attributes

      /**
       * The operation code, for testing purposes.
       */
      protected int m_nOpCode;

      /**
       * Whether to set checksum.
       */
      protected static boolean s_bSetChecksum = true;

      // constructors

      /**
       * Constructs a journal record that has a configurable operation
       * code for testing purposes.
       * 
       * @param xid The xid for this record.
       * @param nOpCode The operation code for this record.
       */
      public JournalRecordTesting(Xid xid, int nOpCode)
      {
         setXid(xid);
         m_nOpCode = nOpCode;
      }

      // operations

      /**
       * Set whether to set checksum.
       * @param bSetChecksum Whether to set checksum.
       */
      public static void setSetChecksum(boolean bSetChecksum)
      {
         s_bSetChecksum = bSetChecksum;
      }

      /**
       * @see nexj.core.rpc.ra.tx.GenericJournalRecord#initChecksum()
       */
      public void initChecksum()
      {
         if (s_bSetChecksum)
         {
            super.initChecksum();
         }
      }

      /**
       * @see nexj.core.rpc.ra.tx.GenericJournalRecord#getOpCode()
       */
      public int getOpCode()
      {
         return m_nOpCode;
      }


      /**
       * For debugging purposes.
       * 
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuilder buf = new StringBuilder();
         
         buf.append("JournalRecordTesting(xid=");
         buf.append((getXid() != null) ? getXid().toString() : "null");
         buf.append(", opcode=");
         buf.append(m_nOpCode);
         buf.append(")");
         
         return buf.toString();
      }


      /**
       * For testing purposes. (The unit test needs to be able to compare
       * GenericJournalRecord instances).
       * 
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof JournalRecordTesting))
         {
            return false;
         }
         
         JournalRecordTesting other = (JournalRecordTesting)obj;
         
         if (other.m_nOpCode != m_nOpCode)
         {
            return false;
         }
         
         return super.equals(obj);
      }
   }
}
