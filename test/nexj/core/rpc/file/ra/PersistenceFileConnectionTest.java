// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.util.Arrays;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.rpc.ra.tx.PersistentJournal;
import nexj.core.runtime.platform.generic.tx.GenericTransactionManager;
import nexj.core.util.Binary;

/**
 * Tests a file connection that is configured for persistence.
 */
public class PersistenceFileConnectionTest extends MessageDirectoryTestCase
{
   // operations
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Commit
    * 
    * @throws Exception
    */
   public void testAttachToUnexistingButNoOperation() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Check result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Rollback
    * 
    * @throws Exception
    */
   public void testAttachToUnexistingButNoOperationRollback() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      //Now unattach
      fConn.close();
      
      txM.rollback();
      
      //Check result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   
   /**
    * Tests:
    * - Attach (exists)
    * - Commit
    * 
    * @throws Exception
    */
   public void testAttachToExistingButNoOperation() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Do attach
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));
      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Check result
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
   }
   
   
   /**
    * Tests:
    * - Attach (exists)
    * - Rollback
    * 
    * @throws Exception
    */
   public void testAttachToExistingButNoOperationRollback() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Do attach
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));
      
      //Now unattach
      fConn.close();
      
      txM.rollback();
      
      //Check result
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
   }
   
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - Commit
    * 
    * @throws Exception
    */
   public void testSingleWrite() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertTrue(new File(m_outgoingTempDirectory, "data1").exists());

      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), data);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   /**
    * Tests (with no transaction enlistment):
    * - Attach (!exists)
    * - Write
    * - Commit
    */
   public void testSingleWriteNoTransaction() throws Exception
   {
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();

      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());

      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));

      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertTrue(new File(m_outgoingTempDirectory, "data1").exists());


      //Now unattach (no transaction case commits on close)
      fConn.close();

      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), data);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   /**
    * Tests:
    * - Attach (exists)
    * - Write
    * - Commit
    * 
    * @throws Exception
    */
   public void testSingleWriteExisting() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);


      //Do the ops
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      assertTrue(new File(m_outgoingTempDirectory, "data1").exists());

      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), data);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   /**
    * Tests (with no transaction enlistment):
    * - Attach (exists)
    * - Write
    * - Commit
    */
   public void testSingleWriteExistingNoTransaction() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};

      writeData(new File(m_outgoingDirectory, "data1"), existingData);


      //Do the ops
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();

      checkAttach(fConn, "data1");

      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));

      assertTrue(new File(m_outgoingTempDirectory, "data1").exists());


      //Now unattach (no transaction case does commit here)
      fConn.close();

      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), data);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   /**
    * Tests that the correct modification time can be retrieved from the
    * file connection.
    * 
    * Tests:
    * - Create and get last modified
    * - Attach (exists)
    * - Get last modified (should be same as existing)
    * - Write
    * - Get last modified (should be same as existing)
    * - Commit
    * - Get last modified (should be different now)
    * 
    * @throws Exception
    */
   public void testLastModified() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "dfile1"), existingData);

      Transaction txT;
      TransactionManager txM;
      
      txM = new GenericTransactionManager();      
      long lPreModificationTime = new File(m_outgoingDirectory, "dfile1").lastModified();      
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "dfile1");
      
      assertEquals(lPreModificationTime, fConn.getLastModified());
      
      /*
       * Windows system timer resolution seems to be about 16ms. This is the resolution to
       * which NTFS last modified time will be written. Must wait 16ms before trying
       * to write file so that write will be done in the next timer tick.
       * 
       * (NTFS itself stores time as the number of 100ns intervals since midnight UTC,
       * January 1, 1601 -- ntfs-3g project) (This is also the context for interpreting
       * the FILETIME structure, part of Win32 API)
       */
      Thread.sleep(20);
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      assertEquals(lPreModificationTime, fConn.getLastModified());
      
      long lUpdatedTime = fConn.getLastModifiedThisTxn();  //temp file time
      
      assertEquals(lPreModificationTime, fConn.getLastModified());
      
      //Finish up
      fConn.close();
      
      txM.commit();
      
      File theFile = new File(m_outgoingDirectory, "dfile1");
      
      assertFalse(lPreModificationTime == lUpdatedTime);
      assertFalse(lPreModificationTime == theFile.lastModified());
      assertEquals(lUpdatedTime, theFile.lastModified());
      
      //Test result
      verifyData(theFile, data);
      assertFalse(new File(m_outgoingTempDirectory, "dfile1").exists());
   }
   
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - Rollback
    * 
    * @throws Exception
    */
   public void testSingleWriteRollback() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      //Now unattach
      fConn.close();
      
      txM.rollback();
      
      //Test result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   
   /**
    * Tests:
    * - Attach file (!exists)
    * - Write data
    * - Delete
    * - Commit
    * 
    * @throws Exception
    */
   public void testWriteDeleteCommit() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      fConn.delete();
      
      Binary readData = fConn.readBinary();
      
      assertNull(readData);
      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Test result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   
   /**
    * Tests:
    * - Attach file (exists)
    * - Delete
    * - Commit
    * 
    * @throws Exception
    */
   public void testDeleteCommit() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);

      
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      assertTrue(new File(m_outgoingDirectory, "data1").exists());
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));
      
      Binary read1 = fConn.readBinary();
      
      assertEquals(new Binary(existingData), read1);
      
      fConn.delete();
      
      Binary read2 = fConn.readBinary();
      
      assertNull(read2);
      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Test result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   /**
    * Tests (with no transaction enlistment):
    * - Attach file (exists)
    * - Delete
    * - Commit
    */
   public void testDeleteNoTransaction() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};

      writeData(new File(m_outgoingDirectory, "data1"), existingData);

      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();

      checkAttach(fConn, "data1");
      assertTrue(new File(m_outgoingDirectory, "data1").exists());
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));

      Binary read1 = fConn.readBinary();

      assertEquals(new Binary(existingData), read1);

      fConn.delete();

      Binary read2 = fConn.readBinary();

      assertNull(read2);

      //Now unattach (no transaction case commits here too)
      fConn.close();

      //Test result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   /**
    * Tests:
    * - Attach file (exists)
    * - Delete
    * - Rollback
    * 
    * @throws Exception
    */
   public void testDeleteRollback() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);

      
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      assertTrue(new File(m_outgoingDirectory, "data1").exists());
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));
      
      Binary read1 = fConn.readBinary();
      
      assertEquals(new Binary(existingData), read1);
      
      fConn.delete();
      
      Binary read2 = fConn.readBinary();
      
      assertNull(read2);
      
      //Now unattach
      fConn.close();
      
      txM.rollback();
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - Read (verify gets written)
    * - Commit
    * 
    * @throws Exception
    */
   public void testWriteReadCommit() throws Exception
   {
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      Binary readData = fConn.readBinary();
      
      assertTrue(readData.equals(new Binary(data)));
      
      //Now unattach
      fConn.close();
      
      txM.commit();
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), data);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }


   /**
    * Tests:
    * - Attach (!exists)
    * - Prepare
    * - Crash
    * - Commit
    * 
    * @throws Exception
    */
   public void testRecoveryAttachToUnexistingButNoOperation() throws Exception
   {
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      connection = null;
      fConn = null;
      xar = null;
      
      //Process crashed
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.commit(xidArray[0], false);
      
      //Check result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Prepare
    * - Crash
    * - Rollback
    * 
    * @throws Exception
    */
   public void testRecoveryAttachToUnexistingButNoOperationRollback() throws Exception
   {
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      connection = null;
      fConn = null;
      xar = null;
      PersistentJournal.closeAll();
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.rollback(xidArray[0]);
      
      //Check result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   /**
    * Tests:
    * - Attach (exists)
    * - Prepare
    * - Crash
    * - Commit
    * 
    * @throws Exception
    */
   public void testRecoveryAttachToExistingButNoOperation() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Do attach
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      connection = null;
      fConn = null;
      xar = null;
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.commit(xidArray[0], false);
      
      //Check result
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
   }
   
   
   /**
    * Tests:
    * - Attach (exists)
    * - Prepare
    * - Crash
    * - Rollback
    * 
    * @throws Exception
    */
   public void testRecoveryAttachToExistingButNoOperationRollback() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Do attach
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      checkCannotExclusiveLockFile(new File(m_outgoingDirectory, "data1"));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      connection = null;
      fConn = null;
      xar = null;
      
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.rollback(xidArray[0]);
      
      //Check result
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
   }
   
   
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - Prepare
    * - Crash
    * - Commit (Can succeed because not existing--INSERT op)
    * 
    * @throws Exception
    */
   public void testRecoveryInsertCommit() throws Exception
   {
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      connection = null;
      fConn = null;
      xar = null;
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.commit(xidArray[0], false);
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), data);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }
   
   
   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - Prepare
    * - Crash
    * - Rollback
    * 
    * @throws Exception
    */
   public void testRecoveryInsertRollback() throws Exception
   {
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      connection = null;
      fConn = null;
      xar = null;
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.rollback(xidArray[0]);
      
      //Test result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }


   /**
    * Tests:
    * - Attach (exists)
    * - Write
    * - Prepare
    * - Crash
    * - Commit (HEURISTIC ROLLBACK because existing)
    * 
    * @throws Exception
    */
   public void testRecoveryUpdateCommit() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Go
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      connection = null;
      fConn = null;
      xar = null;
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      try
      {
         xar.commit(xidArray[0], false);
         fail();
      }
      catch (XAException ex)
      {
         assertEquals(XAException.XA_HEURRB, ex.errorCode);
      }
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
   }


   /**
    * Tests:
    * - Attach (exists)
    * - Write
    * - Prepare
    * - Crash
    * - Rollback
    * 
    * @throws Exception
    */
   public void testRecoveryUpdateRollback() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Go
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      connection = null;
      fConn = null;
      xar = null;
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.rollback(xidArray[0]);
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }

   
   /**
    * Tests:
    * - Attach (exists)
    * - Delete
    * - Prepare
    * - Crash
    * - Commit (HEURISTIC ROLLBACK because existing)
    * 
    * @throws Exception
    */
   public void testRecoveryDeleteCommit() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Go
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      fConn.delete();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      connection = null;
      fConn = null;
      xar = null;
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      try
      {
         xar.commit(xidArray[0], false);
         fail();
      }
      catch (XAException ex)
      {
         assertEquals(XAException.XA_HEURRB, ex.errorCode);
      }
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
   }


   /**
    * Tests:
    * - Attach (exists)
    * - Delete
    * - Prepare
    * - Crash
    * - Rollback
    * 
    * @throws Exception
    */
   public void testRecoveryDeleteRollback() throws Exception
   {
      //Make existing file
      byte[] existingData = new byte[] {0x41, 0x42, 0x43, 0x31, 0x32, 0x33};
      
      writeData(new File(m_outgoingDirectory, "data1"), existingData);
      
      //Go
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      
      fConn.delete();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      xar.prepare(makeXid(1));
      
      //--- CRASH ---
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      connection = null;
      fConn = null;
      xar = null;
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(1, xidArray.length);
      Arrays.sort(xidArray); 
      assertTrue(xidArray[0].equals(makeXid(1)));
      
      xar.rollback(xidArray[0]);
      
      //Test result
      verifyData(new File(m_outgoingDirectory, "data1"), existingData);
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }


   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - NO PREPARE
    * - Crash
    * - Verify not in recovery list
    * 
    * @throws Exception
    */
   public void testRecoverySingleWriteRollback() throws Exception
   {
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "data1");
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      byte[] data = new byte[] {0x41, 0x42, 0x43, 0x00, (byte)0xFF, 0x0d, 0x0a};
      fConn.write(new Binary(data));
      
      //NO PREPARE
      
      //--- CRASH ---
      PersistentJournal.closeAll();
      LockableFile.closeChannel(new File(m_outgoingDirectory, "data1"));
      LockableFile.closeChannel(new File(m_outgoingTempDirectory, "data1"));
      connection = null;
      fConn = null;
      xar = null;
      
      
      //Recover
      connection = createPersistenceConnection();
      xar = connection.getXAR();
      
      Xid[] xidArray = xar.recover(XAResource.TMSTARTRSCAN);
      
      assertNotNull(xidArray);
      assertEquals(0, xidArray.length);
      
      //Test result
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
   }


   /**
    * Tests:
    * - Attach1 (!exists)
    * - Attach2 (exists)
    * - Attach3 (exists)
    * - Attach4 (exists)
    * - Read3
    * - Write3
    * - Read3
    * - Delete4
    * - Commit
    * 
    * @throws Exception
    */
   public void testSharing1() throws Exception
   {
      //Make existing files
      char[] existingData2 = "This is file number two.".toCharArray();
      char[] existingData3 = "This is file number three.".toCharArray();
      char[] existingData4 = "This is file number four.".toCharArray();
      
      writeData(new File(m_outgoingDirectory, "data2"), existingData2);
      writeData(new File(m_outgoingDirectory, "data3"), existingData3);
      writeData(new File(m_outgoingDirectory, "data4"), existingData4);
      
      
      //Begin
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn1 = connection.getConnection();
      FileConnection fConn2 = connection.makeNewConnectionHandle();
      FileConnection fConn3 = connection.makeNewConnectionHandle();
      FileConnection fConn4 = connection.makeNewConnectionHandle();
      FileConnection fConn5 = connection.makeNewConnectionHandle();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn1, "data1");
      checkAttach(fConn2, "data2");
      checkAttach(fConn3, "data3");
      checkAttach(fConn4, "data4");
      checkAttach(fConn5, "data3");
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      //Operations
      
      //File #1: do nothing.
      
      //File #2: do nothing.
      
      //File #3: read, write, read.
      assertEquals("This is file number three.", fConn3.readString());
      assertEquals("This is file number three.", fConn5.readString());
      fConn3.write("MODIFIED");
      assertEquals("MODIFIED", fConn3.readString());
      assertEquals("MODIFIED", fConn5.readString());
      
      //File #4: delete.
      fConn4.delete();
      
      /* ********** DETACH ********** */
      fConn1.close();
      fConn2.close();
      fConn3.close();
      fConn4.close();
      fConn5.close();
      
      txM.commit();
      
      //Check result
      
      //File #1: not present
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
      
      //File #2: still present
      assertTrue(new File(m_outgoingDirectory, "data2").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data2").exists());
      
      //File #3: data changed
      verifyData(new File(m_outgoingDirectory, "data3"), "MODIFIED".toCharArray());
      assertFalse(new File(m_outgoingTempDirectory, "data3").exists());
      
      //File #4: no longer present
      assertFalse(new File(m_outgoingDirectory, "data4").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data4").exists());
   }

   /**
    * Tests (without using any transaction):
    * - Attach1 (!exists)
    * - Attach2 (exists)
    * - Attach3 (exists)
    * - Attach4 (exists)
    * - Read3
    * - Write3
    * - Read3
    * - Delete4
    * - Commit
    * 
    * @throws Exception
    */
   public void testSharing1NoTransaction() throws Exception
   {
      // Make existing files
      char[] existingData2 = "This is file number two.".toCharArray();
      char[] existingData3 = "This is file number three.".toCharArray();
      char[] existingData4 = "This is file number four.".toCharArray();

      writeData(new File(m_outgoingDirectory, "data2"), existingData2);
      writeData(new File(m_outgoingDirectory, "data3"), existingData3);
      writeData(new File(m_outgoingDirectory, "data4"), existingData4);


      // Begin
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn1 = connection.getConnection();
      FileConnection fConn2 = connection.makeNewConnectionHandle();
      FileConnection fConn3 = connection.makeNewConnectionHandle();
      FileConnection fConn4 = connection.makeNewConnectionHandle();
      FileConnection fConn5 = connection.makeNewConnectionHandle();

      checkAttach(fConn1, "data1");
      checkAttach(fConn2, "data2");
      checkAttach(fConn3, "data3");
      checkAttach(fConn4, "data4");
      checkAttach(fConn5, "data3");

      assertFalse(new File(m_outgoingDirectory, "data1").exists());

      // Operations

      // File #1: do nothing.

      // File #2: do nothing.

      // File #3: read, write, read.
      assertEquals("This is file number three.", fConn3.readString());
      assertEquals("This is file number three.", fConn5.readString());
      fConn3.write("MODIFIED");
      assertEquals("MODIFIED", fConn3.readString());
      assertEquals("MODIFIED", fConn5.readString());

      // File #4: delete.
      fConn4.delete();

      /* ********** DETACH ********** */
      fConn1.close();
      fConn2.close();
      fConn3.close();
      fConn4.close();
      fConn5.close();

      // Check result

      // File #1: not present
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());

      // File #2: still present
      assertTrue(new File(m_outgoingDirectory, "data2").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data2").exists());

      // Verify: File #3: data changed immediately
      verifyData(new File(m_outgoingDirectory, "data3"), "MODIFIED".toCharArray());
      assertFalse(new File(m_outgoingTempDirectory, "data3").exists());

      // Verify: File #4: no longer present
      assertFalse(new File(m_outgoingDirectory, "data4").exists());
      assertFalse(new File(m_outgoingTempDirectory, "data4").exists());
   }

   /**
    * Tests:
    * - Attach (!exists)
    * - Write
    * - Commit
    * - Attach (exists)
    * - Write
    * - Commit
    */
   public void testSequentialAttach() throws Exception
   {
      //Begin
      Transaction txT;
      TransactionManager txM;
      ConnectionObjects connection = createPersistenceConnection();
      FileConnection fConn1 = connection.getConnection();
      XAResource xar = connection.getXAR();
      
      txM = new GenericTransactionManager();
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      checkAttach(fConn1, "data1");
      
      assertFalse(new File(m_outgoingDirectory, "data1").exists());
      
      //Operations
      fConn1.write("CREATE");
      fConn1.close();
      txM.commit();
      
      verifyData(new File(m_outgoingDirectory, "data1"), "CREATE".toCharArray());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
      
      
      //Second attach
      txM.begin();
      txT = txM.getTransaction();
      txT.enlistResource(xar);
      
      FileConnection fConn2 = connection.makeNewConnectionHandle();
      
      checkAttach(fConn2, "data1");
      
      //Operations
      fConn2.write("UPDATE");
      fConn2.close();
      txM.commit();
      
      verifyData(new File(m_outgoingDirectory, "data1"), "UPDATE".toCharArray());
      assertFalse(new File(m_outgoingTempDirectory, "data1").exists());
   }


   // helper methods
   
   public void checkCannotExclusiveLockFile(File theFile) throws IOException
   {
      RandomAccessFile f = new RandomAccessFile(theFile, "rw");
      
      try
      {
         FileLock flock = f.getChannel().tryLock(0L, Long.MAX_VALUE, false);
         
         assertNull(flock);
      }
      catch (OverlappingFileLockException ex)
      {
         //No problem.
      }
      
      f.close();
   }
}
