// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.channels.FileLock;
import java.util.Arrays;
import java.util.List;

import javax.transaction.RollbackException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.rpc.file.FileNameExpander;
import nexj.core.rpc.file.ra.tx.FileJournalRecordCopyInsert;
import nexj.core.rpc.file.ra.tx.FileJournalRecordMkTemp;
import nexj.core.rpc.ra.tx.GenericJournalRecord;
import nexj.core.rpc.ra.tx.JournalRecordCompleted;
import nexj.core.rpc.ra.tx.JournalRecordPrepared;
import nexj.core.rpc.ra.tx.MemoryJournal;
import nexj.core.rpc.ra.tx.PersistentJournal;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.platform.generic.tx.GenericTransactionManager;
import nexj.core.util.IOUtil;
import nexj.core.util.XMLUtil;

/**
 * Tests the FileXAResource
 */
public class FileXAResourceTest extends MessageDirectoryTestCase
{
   // operations

   /**
    * Tests that an outgoing file connection can be created, prepared, and
    * committed, driving the transactional flow directly from the test instead
    * of using a transaction manager.
    */
   public void testProcessOutgoingFile() throws Exception
   {
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();

      writeData(fConn, data);
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);

      
      //Test that the file is in the correct directory
      verifyData(new File(m_outgoingDirectory, "out1.txt"), data);
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests sequence number generation using the SysCounter metaclass.
    * 
    * Tests that two outgoing file connections can be enlisted in the
    * same distributed transaction and committed successfully. The
    * GenericTransactionManager is used to managed the transaction.
    */
   public void test2OutgoingWithManagerAndSuccessful() throws Exception
   {
      String sFileNameTemplate = "out.${seq}.txt";
      
      //Initialize DB
      DbSetup sqlTest = new DbSetup("");
      sqlTest.doSetUp();
      InvocationContext context = sqlTest.getInvocationContext();
      
      try
      {
         //Connection #1
         ConnectionObjects outgoing1 = createOutgoingConnection();
         FileConnection fConn1 = outgoing1.getConnection();
         XAResource xar1 = outgoing1.getXAR();
         
         
         //Connection #2
         ConnectionObjects outgoing2 = createOutgoingConnection();
         FileConnection fConn2 = outgoing2.getConnection();
         XAResource xar2 = outgoing2.getXAR();
         
         
         //Transaction Manager Setup
         Transaction txT;
         TransactionManager txM = new GenericTransactionManager();
         
         txM.begin();
         txT = txM.getTransaction();
         
         txT.enlistResource(xar1);
         txT.enlistResource(xar2);
         
         
         //Write on connection #1
         checkAttach(fConn1, FileNameExpander.expandString(sFileNameTemplate, context, null));
         
         //Do file ops...
         char[] data1 = "Data for the first file.\n".toCharArray();
         
         fConn1.write(String.valueOf(data1));
         
         //Write on connection #2
         checkAttach(fConn2, FileNameExpander.expandString(sFileNameTemplate, context, null));
         
         //Do file ops...
         char[] data2 = "Data for the second file.\n".toCharArray();
         
         fConn2.write(String.valueOf(data2));
         
         //Wind up
         fConn1.close();
         fConn2.close();
         
         txM.commit();
         
         
         //Check Result #1
         verifyData(new File(m_outgoingDirectory, "out.1.txt"), data1);
         
         //Check Result #2
         verifyData(new File(m_outgoingDirectory, "out.2.txt"), data2);
         
         //There should be no temporary files left over
         assertEquals(0, m_outgoingTempDirectory.listFiles().length);
      }
      finally
      {
         sqlTest.doTearDown();
      }
   }


   /**
    * Tests that two outgoing file connections can be enlisted in the
    * same distributed transaction and that they are rolled back
    * successfully when the application asks the transaction manager
    * to do a rollback. The GenericTransactionManager is used to
    * manage the transaction.
    */
   public void test2OutgoingWithManagerAndRollback() throws Exception
   {
      //Connection #1
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn1 = outgoing1.getConnection();
      XAResource xar1 = outgoing1.getXAR();
      
      
      //Connection #2
      ConnectionObjects outgoing2 = createOutgoingConnection();
      FileConnection fConn2 = outgoing2.getConnection();
      XAResource xar2 = outgoing2.getXAR();
      
      
      //Transaction Manager Setup
      Transaction txT;
      TransactionManager txM = new GenericTransactionManager();
      
      txM.begin();
      txT = txM.getTransaction();
      
      txT.enlistResource(xar1);
      txT.enlistResource(xar2);
      
      
      //Write on connection #1
      checkAttach(fConn1, "out1.txt");
      
      //Do file ops...
      OutputStream ostream1 = fConn1.getOutputStream();
      OutputStreamWriter writer1 = new OutputStreamWriter(ostream1, "UTF-8");
      
      char[] data1 = "Data for the first file.\n".toCharArray();
      
      writer1.write(data1);
      writer1.close();
      writer1 = null;
      
      
      //Write on connection #2
      checkAttach(fConn2, "out2.txt");
      
      //Do file ops...
      char[] data2 = "Data for the second file.\n".toCharArray();

      fConn2.write(String.valueOf(data2));
      
      //Wind up
      fConn1.close();
      fConn2.close();
      
      txM.rollback();

      
      //There should be no output
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      
      //There should be no temporary files left over
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }
   
   
   /**
    * Tests that two outgoing file connections can be enlisted in the
    * same distributed transaction and that they are rolled back
    * successfully when another participant in the transaction fails
    * to prepare.
    */
   public void test2OutgoingWithManagerAndErrorRollbackAutomatic() throws Exception
   {
      //Connection #1
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn1 = outgoing1.getConnection();
      XAResource xar1 = outgoing1.getXAR();
      
      
      //Connection #2
      ConnectionObjects outgoing2 = createOutgoingConnection();
      FileConnection fConn2 = outgoing2.getConnection();
      XAResource xar2 = outgoing2.getXAR();
      
      
      //The failing XAR
      PrepareFailsXAResource failingXar = new PrepareFailsXAResource();
      
      
      //Transaction Manager Setup
      Transaction txT;
      TransactionManager txM = new GenericTransactionManager();
      
      txM.begin();
      txT = txM.getTransaction();
      
      txT.enlistResource(xar1);
      txT.enlistResource(xar2);
      txT.enlistResource(failingXar);
      
      
      //Write on connection #1
      checkAttach(fConn1, "out1.txt");
      
      //Do file ops...
      OutputStream ostream1 = fConn1.getOutputStream();
      OutputStreamWriter writer1 = new OutputStreamWriter(ostream1, "UTF-8");
      
      char[] data1 = "Data for the first file.\n".toCharArray();

      writer1.write(data1);
      writer1.close();
      writer1 = null;
      
      
      //Write on connection #2
      checkAttach(fConn2, "out2.txt");
      
      //Do file ops...
      char[] data2 = "Data for the second file.\n".toCharArray();
      
      fConn2.write(String.valueOf(data2));
      
      //Wind up
      fConn1.close();
      fConn2.close();
      
      
      //This should not succeed (prepare for 3rd XAR will fail)
      try
      {
         txM.commit();
         fail();
      }
      catch (RollbackException ex)
      {
         //Good
      }
      
      
      //There should be no output
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      
      //There should be no temporary files left over
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * An outgoing file that cannot be attached because the outgoing temp
    * directory doesn't exist.
    */
   public void testOutgoingWithManagerAndErrorRollbackInAttach() throws Exception
   {
      //Connection #1
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn1 = outgoing1.getConnection();
      XAResource xar1 = outgoing1.getXAR();
      
      
      //Transaction Manager Setup
      Transaction txT;
      TransactionManager txM = new GenericTransactionManager();
      
      txM.begin();
      txT = txM.getTransaction();
      
      txT.enlistResource(xar1);
      
      //Create attachment failure
      assertTrue(m_outgoingTempDirectory.delete());
      
      //Open connection #1 - should fail
      try
      {
         fConn1.attachToFile("out1.txt");
         fail();
      }
      catch (FileConnectionException ex)
      {
      }
      
      txM.rollback();
      
      //There should be no output
      assertEquals(0, m_outgoingDirectory.listFiles().length);
   }


   /**
    * Tests interleaving of two connections, both using the same XAResource.
    */
   public void testProcess2OutgoingOnSameXAR() throws Exception
   {
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();
      FileConnection fConn2 = outgoing1.makeNewConnectionHandle();
      FileManagedConnection mConn = outgoing1.getManagedConnection();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      char[] data1 = "This is a test, using test outgoing data.\n".toCharArray();

      writeData(fConn, data1);
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      //assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      assertTrue(mConn.prepare(makeXid(1)));
      
      //Start new between PREPARE and COMMIT
      xar.start(makeXid(2), XAResource.TMNOFLAGS);
      
      //Finish second txn
      char[] data2 = "This is test data for second file.\n".toCharArray();
      
      checkAttach(fConn2, "out2.txt");
      writeData(fConn2, data2);
      xar.end(makeXid(2), XAResource.TMSUCCESS);
      
      //assertEquals(XAResource.XA_OK, xar.prepare(makeXid(2)));
      assertTrue(mConn.prepare(makeXid(2)));
      mConn.commit(makeXid(2));
      
      
      //Resume first txn
      mConn.commit(makeXid(1));

      
      //Test that the file is in the correct directory
      verifyData(new File(m_outgoingDirectory, "out1.txt"), data1);
      
      //Test that the file is in the correct directory
      verifyData(new File(m_outgoingDirectory, "out2.txt"), data2);
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests interleaving of two connections, both using the same XAResource,
    * with SUSPEND and RESUME.
    */
   public void testProcess2OutgoingOnSameXAR2() throws Exception
   {
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();
      FileConnection fConn2 = outgoing1.makeNewConnectionHandle();
      FileManagedConnection mConn = outgoing1.getManagedConnection();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      char[] data1 = "This is a test, using test outgoing data.\n".toCharArray();

      writeData(fConn, data1);
      xar.end(makeXid(1), XAResource.TMSUSPEND);
      
      //Start new between PREPARE and COMMIT
      xar.start(makeXid(2), XAResource.TMNOFLAGS);
      
      //Finish second txn
      char[] data2 = "This is test data for second file.\n".toCharArray();
      
      checkAttach(fConn2, "out2.txt");
      writeData(fConn2, data2);
      xar.end(makeXid(2), XAResource.TMSUCCESS);
      
      assertTrue(mConn.prepare(makeXid(2)));
      mConn.commit(makeXid(2));
      
      
      //Resume first txn
      xar.start(makeXid(1), XAResource.TMRESUME);
      writeData(fConn, "ADDITIONAL".toCharArray());
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));

      
      //Test that the file is in the correct directory
      verifyData(new File(m_outgoingDirectory, "out1.txt"), new String(data1).concat("ADDITIONAL").toCharArray());
      
      //Test that the file is in the correct directory
      verifyData(new File(m_outgoingDirectory, "out2.txt"), data2);
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that an outgoing file can be attached and committed using
    * the one-phase commit optimization.
    */
   public void testProcessOutgoingFile1PC() throws Exception
   {
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      writeData(fConn, data);
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      xar.commit(makeXid(1), true);
      
      
      //Test that the file is in the correct directory
      verifyData(new File(m_outgoingDirectory, "out1.txt"), data);
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that recovery can find those transactions requiring recovery,
    * even if they were executed by different threads in the same process
    * as that in which recovery is being executed.
    */
   public void testRecoveryOutgoingFindsXids() throws Throwable
   {
      //***** First Transaction *****
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn1 = outgoing1.getConnection();
      XAResource xar1 = outgoing1.getXAR();

      
      xar1.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn1, "out1.txt");
      
      //Do file ops...
      char[] data1 = "This is a test 1, using test outgoing data.\n".toCharArray();
      
      writeData(fConn1, data1);
      
      //Finish up...
      fConn1.close();
      xar1.end(makeXid(1), XAResource.TMSUCCESS);
      assertEquals(XAResource.XA_OK, xar1.prepare(makeXid(1)));

      
      
      //***** Second Transaction *****
      ConnectionObjects outgoing2 = createOutgoingConnection();
      FileConnection fConn2 = outgoing2.getConnection();
      XAResource xar2 = outgoing2.getXAR();

      xar2.start(makeXid(2), XAResource.TMNOFLAGS);
      
      checkAttach(fConn2, "out2.txt");
      
      //Do file ops...
      char[] data2 = "This is a test 2, using test outgoing data.\n".toCharArray();
      
      writeData(fConn2, data2);
      
      //Finish up...
      fConn2.close();
      xar2.end(makeXid(2), XAResource.TMSUCCESS);
      assertEquals(XAResource.XA_OK, xar2.prepare(makeXid(2)));
      
      
      
      //***** CRASH HAPPENS HERE *****
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out2.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out2.txt"));
      outgoing1 = null;
      xar1 = null;
      fConn1 = null;
      outgoing2 = null;
      xar2 = null;
      fConn2 = null;
      
      
      //Find out what needs to be recovered
      ConnectionObjects outgoing = createOutgoingConnection();
      XAResource xar = outgoing.getXAR();
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(2, xidArray.length);
      Arrays.sort(xidArray);       
      assertTrue(xidArray[0].equals(makeXid(1)));
      assertTrue(xidArray[1].equals(makeXid(2)));
   }


   /**
    * Tests that an outgoing file txn that has been prepared but was never
    * committed can be recovered successfully.
    */
   public void testOutgoingHandleRecoveryCommit() throws Throwable
   {
      final char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      //Test it with a short file name (shorter than FileConnectionJournal.FILE_NAME_PREFIX_LENGTH)
      checkAttach(fConn, "o1");
      
      //Do file ops...
      writeData(fConn, data);
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      //********** CRASH HAPPENS HERE **********
      
      LockableFile.closeChannel(new File(m_outgoingDirectory, "o1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "o1"));
      PersistentJournal.closeAll();
      
      
      //Recover.
      ConnectionObjects outgoing2 = createOutgoingConnection();
      XAResource xar2 = outgoing2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      //Do the COMMIT.
      xar2.commit(recoveredXid, false);
      
      
      //Test that the file got written after all
      verifyData(new File(m_outgoingDirectory, "o1"), data);
      
      FileOutputStream lockStream = new FileOutputStream(new File(m_outgoingDirectory, "o1"));
      FileLock flock = lockStream.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock.isValid());
      flock.release();
      
      //Temp directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
      
      
      //The transaction should be forgotten automatically
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      //Reload journal from disk, to ensure that the transaction was actually forgotten
      PersistentJournal.closeAll();
      
      ConnectionObjects outgoing3 = createOutgoingConnection();
      XAResource xar3 = outgoing3.getXAR();
      
      xidArray = xar3.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar3.recover(XAResource.TMNOFLAGS));
      assertNull(xar3.recover(XAResource.TMENDRSCAN));
   }


   /**
    * Tests that an outgoing file txn that has been prepared but experienced
    * a failure during commit can be recovered successfully. 
    */
   public void testOutgoingHandleRecoveryCommit2() throws Throwable
   {
      final char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      writeData(fConn, data);
      
      
      //Finish up
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      
      //Cause an error!
      assertTrue(m_outgoingDirectory.delete());
      
      try
      {
         xar.commit(makeXid(1), false);
         fail();
      }
      catch (XAException ex)
      {
         //This is the result of temporary disconnection.
      }

      
      //***** CRASH *****
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out1.txt"));
      PersistentJournal.closeAll();
      
      //Repair the error
      assertTrue(m_outgoingDirectory.mkdir());
      
      
      //Recover.
      ConnectionObjects outgoing2 = createOutgoingConnection();
      XAResource xar2 = outgoing2.getXAR();
      

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      //Do the COMMIT.
      xar2.commit(recoveredXid, false);
      
      
      //Test that the file got written after all
      verifyData(new File(m_outgoingDirectory, "out1.txt"), data);
      
      FileOutputStream lockStream = new FileOutputStream(new File(m_outgoingDirectory, "out1.txt"));
      FileLock flock = lockStream.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock.isValid());
      flock.release();
      
      
      //Temp directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Create a situation on-disk that simulates the state of an outgoing
    * file txn that has prepared successfully but has not committed. This
    * approximates the state when recovery is being done for a failed
    * txn from another process.
    */
   public void testOutgoingHandleOtherProcessRecoveryCommit() throws Throwable
   {
      final char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      //Make temporary output file
      this.writeData(new File(m_outgoingTempDirectory, "outgoing.txt"), data);
      
      //Make appropriate journal entry. (File, STATUS_READY)
      PersistentJournal pJournal = PersistentJournal.makeJournal(m_outgoingJournal);
      MemoryJournal journal = pJournal.getJournal(makeXid(7));
      
      journal.addRecord(new FileJournalRecordCopyInsert(makeXid(7),
         new File(m_outgoingTempDirectory, "outgoing.txt").getAbsolutePath(),
         new File(m_outgoingDirectory, "outgoing.txt").getAbsolutePath(),
         null, null));
      
      journal.addRecord(new FileJournalRecordMkTemp(makeXid(7),
         new File(m_outgoingTempDirectory, "outgoing.txt").getAbsolutePath(),
         null));
      
      journal.addRecord(new JournalRecordPrepared(makeXid(7)));
      
      journal.flush();
      
      
      //********** PROCESS CRASH RECOVERY **********
      PersistentJournal.closeAll();
      
      //Recover.
      ConnectionObjects outgoing2 = createOutgoingConnection();
      XAResource xar2 = outgoing2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(7)));
      
      //Do the COMMIT.
      xar2.commit(recoveredXid, false);
      
      
      //Test that the file got written after all
      verifyData(new File(m_outgoingDirectory, "outgoing.txt"), data);
      
      FileOutputStream lockStream = new FileOutputStream(new File(m_outgoingDirectory, "outgoing.txt"));
      FileLock flock = lockStream.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock.isValid());
      flock.release();
      
      
      //Temp directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
      
      //Journal must mark transaction as completed.
      pJournal = PersistentJournal.makeJournal(m_outgoingJournal);
      journal = pJournal.recoverJournal(makeXid(7));
      
      List recordsList = journal.getRecords();
      GenericJournalRecord lastRecord = (GenericJournalRecord)recordsList.get(recordsList.size() - 1);
      
      assertEquals(JournalRecordCompleted.OPCODE, lastRecord.getOpCode());
      assertEquals(recoveredXid, lastRecord.getXid());
      
      pJournal.close();
   }


   /**
    * Tests that an outgoing file txn that has been prepared successfully but was never
    * rolled back can be recovered successfully.
    * 
    * This situation happens if the file resource manager can commit successfully, but
    * some other participant in the distributed transaction decides to roll back, and
    * a crash occurrs that prevents the file resource manager from receiving or
    * executing the rollback.
    */
   public void testOutgoingHandleRecoveryRollback() throws Throwable
   {
      final char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();

      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      writeData(fConn, data);
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      //********** CRASH HAPPENS HERE **********
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out1.txt"));
      outgoing1 = null;
      fConn = null;
      xar = null;
      
      
      
      //Recover
      ConnectionObjects outgoing2 = createOutgoingConnection();
      XAResource xar2 = outgoing2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      
      //Assume one of the other distributed transactions failed, so recovery operation is to rollback.
      xar2.rollback(recoveredXid);
      
      
      //Both files should be gone.
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);


      //The transaction should be forgotten automatically
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      //Reload journal from disk, to ensure that the transaction was actually forgotten
      PersistentJournal.closeAll();
      
      ConnectionObjects outgoing3 = createOutgoingConnection();
      XAResource xar3 = outgoing3.getXAR();
      
      xidArray = xar3.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar3.recover(XAResource.TMNOFLAGS));
      assertNull(xar3.recover(XAResource.TMENDRSCAN));
   }


   /**
    * Tests that an outgoing file txn that has been prepared successfully and was
    * rolled back (but rollback failed) can be recovered.
    * 
    * This situation happens if the file resource manager can commit successfully, but
    * some other participant in the distributed transaction decides to roll back, and
    * a failure occurs that prevents the file resource manager from completing the
    * roll back.
    */
   public void testOutgoingHandleRecoveryRollback2() throws Throwable
   {
      final char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn = outgoing1.getConnection();
      XAResource xar = outgoing1.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      writeData(fConn, data);
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      
      //Simulate temporary disconnection from shared drive. (Make delete of stage 1 file fail)
      FileOutputStream causeFailureOutputStream = new FileOutputStream(new File(m_outgoingTempDirectory, "out1.txt"));
      
      try
      {
         //Assume one of the other distributed transactions failed, so ROLLBACK
         xar.rollback(makeXid(1));
         fail();
      }
      catch (XAException ex)
      {
         //This is the result of temporary disconnection.
      }
      
      //Must close this now, or it will remain open and affect the recovery procedure.
      causeFailureOutputStream.close();
      
      
      //***** CRASH HAPPENS HERE *****
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out1.txt"));
      outgoing1 = null;
      fConn = null;
      xar = null;
      
      
      //Recover
      ConnectionObjects outgoing2 = createOutgoingConnection();
      XAResource xar2 = outgoing2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      
      //Assume one of the other distributed transactions failed, so ROLLBACK
      xar2.rollback(recoveredXid);
      
      
      //Both files should be gone.
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file connection can be created, prepared, and
    * committed, driving the transactional flow directly from the test instead
    * of using a transaction manager.
    * 
    * Tests the move to processed directory mode.
    */
   public void testProcessIncomingFileMove() throws Exception
   {
      ConnectionObjects incoming1 = createIncomingConnection(false);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      fConn.setExpandedProcessedName("incoming1.txt");
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);
      
      
      //Check that the file was moved.
      verifyData(new File(m_processedDirectory, "incoming1.txt"), data);
      
      //Incoming directory should now be empty
      assertEquals(0, m_incomingDirectory.listFiles().length);
   }


   /**
    * XAResource.start() called after file connection has attached to file.
    */
   public void testProcessIncomingFileMoveWithPreAssociate1() throws Exception
   {
      ConnectionObjects incoming1 = createIncomingConnection(false);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      checkAttach(fConn, "incoming1.txt");
      
      fConn.setExpandedProcessedName("incoming1.txt");
      
      //NOW START XAResource
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);
      
      
      //Check that the file was moved.
      verifyData(new File(m_processedDirectory, "incoming1.txt"), data);
      
      //Incoming directory should now be empty
      assertEquals(0, m_incomingDirectory.listFiles().length);
   }


   /**
    * XAResource.start() called after obtaining input stream from file connection.
    */
   public void testProcessIncomingFileMoveWithPreAssociate2() throws Exception
   {
      ConnectionObjects incoming1 = createIncomingConnection(false);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      checkAttach(fConn, "incoming1.txt");
      
      fConn.setExpandedProcessedName("incoming1.txt");
      
      //NOW START XAResource
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      //at least one operation must be performed after start()
      InputStream istream = fConn.getInputStream();
      
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);
      
      
      //Check that the file was moved.
      verifyData(new File(m_processedDirectory, "incoming1.txt"), data);
      
      //Incoming directory should now be empty
      assertEquals(0, m_incomingDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file connection can be created, prepared, and
    * committed, driving the transactional flow directly from the test instead
    * of using a transaction manager.
    * 
    * Tests the delete incoming file mode.
    */
   public void testProcessIncomingFileDelete() throws Exception
   {
      ConnectionObjects incoming1 = createIncomingConnection(true);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      //File operations
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming2.txt");
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);
      
      
      //Check deletion
      assertEquals(0, m_incomingDirectory.listFiles().length);
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * XAResource.start() called after file connection has attached to file.
    */
   public void testProcessIncomingFileDeleteWithPreAssociate1() throws Exception
   {
      ConnectionObjects incoming1 = createIncomingConnection(true);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      //File operations
      
      checkAttach(fConn, "incoming2.txt");
      
      //NOW START XAResource
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);
      
      
      //Check deletion
      assertEquals(0, m_incomingDirectory.listFiles().length);
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * XAResource.start() called after obtaining input stream from file connection.
    */
   public void testProcessIncomingFileDeleteWithPreAssociate2() throws Exception
   {
      ConnectionObjects incoming1 = createIncomingConnection(true);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      //File operations
      
      checkAttach(fConn, "incoming2.txt");
      
      //NOW START XAResource
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      //at least one operation must be performed after start().
      InputStream istream = fConn.getInputStream();
      
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      xar.commit(makeXid(1), false);
      
      
      //Check deletion
      assertEquals(0, m_incomingDirectory.listFiles().length);
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Incoming file recovery: COMMIT must fail with HEURISTIC ROLLBACK.
    */
   public void testIncomingMoveFileHandleRecoveryCommit() throws Throwable
   {
      char[] data = "This is a test, using test incoming data.\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();

      //Do incoming.
      ConnectionObjects incoming1 = createIncomingConnection(false);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();

                     
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      fConn.setExpandedProcessedName("incoming1.txt");
      
      //Do file ops...
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      //********** CRASH HAPPENS HERE **********
      LockableFile.closeChannel(new File(m_incomingDirectory, "incoming1.txt"));
      LockableFile.closeChannel(new File(m_processedDirectory, "incoming1.txt"));
      PersistentJournal.closeAll();
      incoming1 = null;
      fConn = null;
      xar = null;
      
      
      //Recover.
      ConnectionObjects incoming2 = createIncomingConnection(false);
      XAResource xar2 = incoming2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      //COMMIT must fail with HEURISTIC ROLLBACK
      try
      {
         xar2.commit(recoveredXid, false);
         fail();
      }
      catch (XAException ex)
      {
         assertEquals(XAException.XA_HEURRB, ex.errorCode);
      }
      
      //The transaction should NOT be forgotten automatically (HEURISTIC COMPLETION)
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      recoveredXid = xidArray[0];
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      xar2.forget(recoveredXid);
      
      
      //Now test to see that it has been forgotten.
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      //Test again, reloading the journal
      PersistentJournal.closeAll();
      
      ConnectionObjects outgoing3 = createOutgoingConnection();
      XAResource xar3 = outgoing3.getXAR();
      
      xidArray = xar3.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar3.recover(XAResource.TMNOFLAGS));
      assertNull(xar3.recover(XAResource.TMENDRSCAN));
   }


   /**
    * Incoming file recovery: COMMIT must fail with HEURISTIC ROLLBACK.
    */
   public void testIncomingDeleteFileHandleRecoveryCommit() throws Throwable
   {
      char[] data = "This is a test, using test incoming data.\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();

      //Execute incoming message
      ConnectionObjects incoming1 = createIncomingConnection(true);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      //Do file ops...
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      //********** CRASH HAPPENS HERE **********
      LockableFile.closeChannel(new File(m_incomingDirectory, "incoming1.txt"));
      PersistentJournal.closeAll();
      incoming1 = null;
      fConn = null;
      xar = null;
      
      
      //Recover.
      ConnectionObjects incoming2 = createIncomingConnection(true);
      XAResource xar2 = incoming2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      //COMMIT must fail with HEURISTIC ROLLBACK
      try
      {
         xar2.commit(recoveredXid, false);
         fail();
      }
      catch (XAException ex)
      {
         assertEquals(XAException.XA_HEURRB, ex.errorCode);
      }

      //The transaction should NOT be forgotten automatically (HEURISTIC COMPLETION)
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      recoveredXid = xidArray[0];
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      xar2.forget(recoveredXid);
      
      
      //Now test to see that it has been forgotten.
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      //Test again, reloading the journal
      PersistentJournal.closeAll();
      
      ConnectionObjects outgoing3 = createOutgoingConnection();
      XAResource xar3 = outgoing3.getXAR();
      
      xidArray = xar3.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar3.recover(XAResource.TMNOFLAGS));
      assertNull(xar3.recover(XAResource.TMENDRSCAN));
   }


   /**
    * Tests that an incoming file txn that has been prepared successfully but was never
    * rolled back can be recovered successfully.
    * 
    * This situation happens if the file resource manager can commit successfully, but
    * some other participant in the distributed transaction decides to roll back, and
    * a crash occurrs that prevents the file resource manager from receiving or
    * executing the rollback.
    */
   public void testIncomingMoveFileHandleRecoveryRollback() throws Throwable
   {
      final char[] data = "This is a test, using test incoming data.\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();

      ConnectionObjects incoming1 = createIncomingConnection(false);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      fConn.setExpandedProcessedName("incoming1.txt");
      
      //Do file ops...
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      //***** CRASH HAPPENS HERE *****
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_incomingDirectory, "incoming1.txt"));
      LockableFile.closeChannel(new File(m_processedDirectory, "incoming1.txt"));
      
      
      //Recover.
      ConnectionObjects incoming2 = createIncomingConnection(false);
      XAResource xar2 = incoming2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
  
      
      //Assume one of the other distributed transactions failed, so ROLLBACK
      xar2.rollback(recoveredXid);
      
      
      //Test that file is still in incoming directory, and is UNLOCKED
      File result = new File(m_incomingDirectory, "incoming1.txt");
      
      verifyData(result, data);
      
      FileOutputStream lockStream = new FileOutputStream(result);
      FileLock flock = lockStream.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock.isValid());
      flock.release();
      
      //Processed directory should be empty
      assertEquals(0, m_processedDirectory.listFiles().length);
      
      
      //Now test to see that it has been forgotten.
      xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      //Test again, reloading the journal
      PersistentJournal.closeAll();
      
      ConnectionObjects outgoing3 = createOutgoingConnection();
      XAResource xar3 = outgoing3.getXAR();
      
      xidArray = xar3.recover(XAResource.TMSTARTRSCAN);
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      assertNull(xar3.recover(XAResource.TMNOFLAGS));
      assertNull(xar3.recover(XAResource.TMENDRSCAN));
   }


   /**
    * Tests that an incoming file txn that has been prepared successfully but was never
    * rolled back can be recovered successfully.
    * 
    * This situation happens if the file resource manager can commit successfully, but
    * some other participant in the distributed transaction decides to roll back, and
    * a crash occurrs that prevents the file resource manager from receiving or
    * executing the rollback.
    */
   public void testIncomingDeleteFileHandleRecoveryRollback() throws Throwable
   {
      char[] data = "This is a test, using test incoming data.\n".toCharArray();
      writeData(new File(m_incomingDirectory, "incoming1.txt"), data);

      
      //BEGIN
      ConnectionObjects incoming1 = createIncomingConnection(true);
      FileConnection fConn = incoming1.getConnection();
      XAResource xar = incoming1.getXAR();


      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      //Do file ops...
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      
      
      //Finish up...
      fConn.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar.prepare(makeXid(1)));
      
      
      //***** CRASH *****
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_incomingDirectory, "incoming1.txt"));
      incoming1 = null;
      fConn = null;
      xar = null;
      istream = null;
      reader = null;
      
      
      
      //Recover.
      ConnectionObjects incoming2 = createIncomingConnection(true);
      XAResource xar2 = incoming2.getXAR();

      Xid[] xidArray = xar2.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      assertNull(xar2.recover(XAResource.TMNOFLAGS));
      assertNull(xar2.recover(XAResource.TMENDRSCAN));
      
      Xid recoveredXid = xidArray[0];
      
      assertTrue(recoveredXid.equals(makeXid(1)));
      
      //Assume one of the other distributed transactions failed, so ROLLBACK
      xar2.rollback(recoveredXid);
      
      
      //Test that file is still in incoming directory, and is UNLOCKED
      File result = new File(m_incomingDirectory, "incoming1.txt");
      
      verifyData(result, data);
      
      FileOutputStream lockStream = new FileOutputStream(result);
      FileLock flock = lockStream.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock.isValid());
      flock.release();
      
      
      //Processed directory should be empty
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that two outgoing file txns that were both prepared but never
    * committed can be recovered successfully.
    */
   public void test2OutgoingHandleRecoveryCommit() throws Throwable
   {
      char[] data1 = "[FILE 1] This is a test, using test outgoing data.\n".toCharArray();
      char[] data2 = "[FILE 2] This is a test, using test outgoing data.\n".toCharArray();
      
      //BEGIN 1
      ConnectionObjects outgoing1 = createOutgoingConnection();
      FileConnection fConn1 = outgoing1.getConnection();
      XAResource xar1 = outgoing1.getXAR();

      
      xar1.start(makeXid(5), XAResource.TMNOFLAGS);
      
      checkAttach(fConn1, "out1.txt");
      
      //Do file ops...
      writeData(fConn1, data1);
      
      
      //Finish up...
      fConn1.close();
      
      xar1.end(makeXid(5), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar1.prepare(makeXid(5)));
      
      //********** CRASH HAPPENS HERE **********
      
      
      //BEGIN 2
      ConnectionObjects outgoing2 = createOutgoingConnection();
      FileConnection fConn2 = outgoing2.getConnection();
      XAResource xar2 = outgoing2.getXAR();

      
      xar2.start(makeXid(6), XAResource.TMNOFLAGS);
      
      checkAttach(fConn2, "out2.txt");
      
      //Do file ops...
      writeData(fConn2, data2);
      
      
      //Finish up...
      fConn2.close();
      
      xar2.end(makeXid(6), XAResource.TMSUCCESS);
      
      assertEquals(XAResource.XA_OK, xar2.prepare(makeXid(6)));
      
      
      
      //********** CRASH HAPPENS HERE **********
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out1.txt"));
      LockableFile.closeChannel(new File(m_outgoingDirectory, "out2.txt"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "out2.txt"));
      outgoing1 = null;
      fConn1 = null;
      xar1 = null;
      outgoing2 = null;
      fConn2 = null;
      xar2 = null;
      
      
      
      //Recover.
      ConnectionObjects outgoing3 = createOutgoingConnection();
      XAResource xar3 = outgoing3.getXAR();

      Xid[] xidArray = xar3.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(2, xidArray.length);
      assertNull(xar3.recover(XAResource.TMNOFLAGS));
      assertNull(xar3.recover(XAResource.TMENDRSCAN));
      
      Arrays.sort(xidArray);
      
      Xid recoveredXid1 = xidArray[0];
      Xid recoveredXid2 = xidArray[1];
      
      assertTrue(recoveredXid1.equals(makeXid(5)));
      assertTrue(recoveredXid2.equals(makeXid(6)));
      
      //Do the COMMIT.
      xar3.commit(recoveredXid1, false);
      xar3.commit(recoveredXid2, false);
      
      
      //Test that the first file got written after all
      File result1 = new File(m_outgoingDirectory, "out1.txt");
      
      verifyData(result1, data1);
            
      FileOutputStream lockStream1 = new FileOutputStream(result1);
      FileLock flock1 = lockStream1.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock1.isValid());
      flock1.release();
      
      
      //Test that the second file got written after all
      File result2 = new File(m_outgoingDirectory, "out2.txt");
      
      verifyData(result2, data2);
      
      FileOutputStream lockStream2 = new FileOutputStream(result2);
      FileLock flock2 = lockStream2.getChannel().tryLock(0L, Long.MAX_VALUE, false);
      
      assertTrue(flock2.isValid());
      flock2.release();
            
      
      //Temp directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   // inner classes

   /**
    * Set up the database for unit testing so that SysCounter
    * can be tested.
    */
   protected static class DbSetup extends SQLDataTest
   {
      // constructors

      /**
       * Creates a new setup instance.
       * 
       * @param sName The name of the test.
       */
      public DbSetup(String sName)
      {
         super(sName);
      }


      // operations

      /**
       * Perform database setup.
       */
      public void doSetUp() throws Exception
      {
         setUp();
      }

      /**
       * Cleanup after ourselves.
       */
      public void doTearDown() throws Exception
      {
         tearDown();
      }

      /**
       * Gets the invocation context.
       */
      public InvocationContext getInvocationContext()
      {
         return m_context;
      }
   }


   /**
    * This is an XAResource that fails when prepare() is called. It may be enlisted
    * in a distributed transaction to force that transaction to be rolled back.
    */
   protected class PrepareFailsXAResource implements XAResource
   {
      /**
       * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
       */
      public void commit(Xid xid, boolean bOnePhase) throws XAException
      {
      }

      /**
       * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
       */
      public void end(Xid xid, int nFlags) throws XAException
      {
      }

      /**
       * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
       */
      public void forget(Xid xid) throws XAException
      {
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
      public boolean isSameRM(XAResource xar) throws XAException
      {
         return false;
      }

      /**
       * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
       */
      public int prepare(Xid xid) throws XAException
      {
         throw new XAException(XAException.XA_RBROLLBACK);
      }

      /**
       * @see javax.transaction.xa.XAResource#recover(int)
       */
      public Xid[] recover(int nFlag) throws XAException
      {
         return null;
      }

      /**
       * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
       */
      public void rollback(Xid xid) throws XAException
      {
      }

      /**
       * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
       */
      public boolean setTransactionTimeout(int nSeconds) throws XAException
      {
         return false;
      }

      /**
       * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
       */
      public void start(Xid xid, int nFlag) throws XAException
      {
      }
   }
}
