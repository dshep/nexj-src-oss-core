// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.channels.FileLock;
import java.util.Arrays;

import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;

import nexj.core.util.IOUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;


/**
 * Tests a file connection directly, without driving it from a
 * FileXAResource.
 */
public class FileConnectionTest extends MessageDirectoryTestCase
{
   // operations

   /**
    * Tests connecting to an outgoing file.
    */
   public void testProcessOutgoingFile() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Verify that another thread/process can't get a lock on the temp file
      File sameFile = new File(m_outgoingTempDirectory, "out1.txt");
      FileInputStream sameFileOutputStream = new FileInputStream(sameFile);
      FileLock theLock = sameFileOutputStream.getChannel().tryLock(0L, Long.MAX_VALUE, true);
      
      assertNull(theLock);
      sameFileOutputStream.close();
      
      
      //Do file ops...
      OutputStream ostream = fConn.getOutputStream();
      OutputStreamWriter writer = new OutputStreamWriter(ostream, "UTF-8");
      
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      writer.write(data);
      writer.close();
      
      
      //Do some more file ops (to prove that we can get the output stream again)
      ostream = fConn.getOutputStream();
      writer = new OutputStreamWriter(ostream, "UTF-8");
      
      writer.write(data);
      writer.close();
      
      
      //Verify again that another thread/process can't get a lock on the temp file
      File sameFile2 = new File(m_outgoingTempDirectory, "out1.txt");
      FileInputStream sameFileOutputStream2 = new FileInputStream(sameFile2);
      FileLock theLock2 = sameFileOutputStream2.getChannel().tryLock(0L, Long.MAX_VALUE, true);
      
      assertNull(theLock2);
      sameFileOutputStream2.close();
      
      
      
      //Finish up...
      fConn.close();
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));
      
      
      //Test that the file is in the correct directory
      File result = new File(m_outgoingDirectory, "out1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());

      Reader reader = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData = new char[data.length];
      char[] resultData2 = new char[data.length];

      assertEquals(data.length, reader.read(resultData));
      assertTrue(Arrays.equals(data, resultData));
      assertEquals(data.length, reader.read(resultData2));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader.close();
      
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
      
      
      //Test that connection cannot be reused
      try
      {
         fConn.attachToFile("out2.txt");
         fail();
      }
      catch (FileConnectionException ex)
      {
         assertEquals("err.rpc.file.duplicateAttach", ex.getErrorCode());
         assertEquals("out2.txt", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Tests connecting to an outgoing file, with no transaction enlistment.
    */
   public void testProcessOutgoingFileNoTransaction() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();

      checkAttach(fConn, "out1.txt");

      //Verify that another thread/process can't get a lock on the temp file
      File sameFile = new File(m_outgoingTempDirectory, "out1.txt");
      FileInputStream sameFileOutputStream = new FileInputStream(sameFile);
      FileLock theLock = sameFileOutputStream.getChannel().tryLock(0L, Long.MAX_VALUE, true);

      assertNull(theLock);
      sameFileOutputStream.close();


      //Do file ops...
      OutputStream ostream = fConn.getOutputStream();
      OutputStreamWriter writer = new OutputStreamWriter(ostream, "UTF-8");

      char[] data = "This is a test, using test outgoing data.\n".toCharArray();

      writer.write(data);
      writer.close();


      //Do some more file ops (to prove that we can get the output stream again)
      ostream = fConn.getOutputStream();
      writer = new OutputStreamWriter(ostream, "UTF-8");

      writer.write(data);
      writer.close();


      //Verify again that another thread/process can't get a lock on the temp file
      File sameFile2 = new File(m_outgoingTempDirectory, "out1.txt");
      FileInputStream sameFileOutputStream2 = new FileInputStream(sameFile2);
      FileLock theLock2 = sameFileOutputStream2.getChannel().tryLock(0L, Long.MAX_VALUE, true);

      assertNull(theLock2);
      sameFileOutputStream2.close();



      //Finish up... (no transaction, so this will do the commit)
      fConn.close();


      //Test that the file is in the correct directory
      File result = new File(m_outgoingDirectory, "out1.txt");

      assertTrue(result.exists());
      assertTrue(result.canRead());

      Reader reader = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData = new char[data.length];
      char[] resultData2 = new char[data.length];

      assertEquals(data.length, reader.read(resultData));
      assertTrue(Arrays.equals(data, resultData));
      assertEquals(data.length, reader.read(resultData2));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData2));

      reader.close();


      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);


      //Test that connection cannot be reused
      try
      {
         fConn.attachToFile("out2.txt");
         fail();
      }
      catch (FileConnectionException ex)
      {
         assertEquals("err.rpc.file.duplicateAttach", ex.getErrorCode());
         assertEquals("out2.txt", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Tests that no operation outgoing connections get cleaned up, with
    * only close() and rollback().
    */
   public void testNoOpsDoesCleanupOutgoing() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      fConn.close();
      
      mConn.rollback(makeXid(1));
      
      
      //Second connection
      outgoing = createOutgoingConnection();
      fConn = outgoing.getConnection();
      mConn = outgoing.getManagedConnection();
      xar = outgoing.getXAR();
      
      xar.start(makeXid(2), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      OutputStream ostream = fConn.getOutputStream();
      OutputStreamWriter writer = new OutputStreamWriter(ostream, "UTF-8");
      
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      writer.write(data);
      writer.close();
            
      //Finish up...
      fConn.close();
      xar.end(makeXid(2), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(2)));
      mConn.commit(makeXid(2));
      
      
      //Test that the file is in the correct directory
      File result = new File(m_outgoingDirectory, "out1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      
      Reader reader = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertTrue(Arrays.equals(data, resultData));
      assertEquals(-1, reader.read());
      
      reader.close();
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests connecting to an outgoing file when the temporary directory (used
    * for preparing outgoing messages) is a subdirectory of the outgoing
    * directory.
    */
   public void testProcessOutgoingFileWithTempInOutgoingDirectory() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnectionWithTempInOutgoing();
      FileConnection fConn = outgoing.getConnection();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();

      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      
      //Do file ops...
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      fConn.write(String.valueOf(data));

      
      //Finish up...
      fConn.close();
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));
      
      
      //Test that the file is in the correct directory
      File result = new File(m_outgoingDirectory, "out1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      
      Reader reader = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertTrue(Arrays.equals(data, resultData));
      assertEquals(-1, reader.read());
      
      reader.close();
      
      //Temporary directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests connecting to an incoming file and that the file is moved
    * to the processed directory when it is committed.
    */
   public void testProcessIncomingFileMove() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(false);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      File sameFile;
      FileInputStream sameFileInputStream;
      FileLock theLock;
      
      
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
      
      
      //Verify that another thread/process can't get a lock on the same file
      sameFile = new File(m_incomingDirectory, "incoming1.txt");
      sameFileInputStream = new FileInputStream(sameFile);
      theLock = sameFileInputStream.getChannel().tryLock(0L, Long.MAX_VALUE, true);
      
      assertNull(theLock);
      sameFileInputStream.close();
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));

      
      // Now to check that the file was moved.
      File result = new File(m_processedDirectory, "incoming1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Incoming directory should now be empty
      assertEquals(0, m_incomingDirectory.listFiles().length);
   }
   
   
   /**
    * Tests connecting to an incoming file and that the file is deleted
    * when it is committed.
    */
   public void testProcessIncomingFileDelete() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(true);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
            
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
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));
      
      
      //Check that the file was deleted.
      assertEquals(0, m_incomingDirectory.listFiles().length);
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming connection without any operations performed
    * gets cleaned up when close() is called, even if it hasn't been
    * enlisted in a transaction (and rollback is getting called).
    */
   public void testNoOpsDoesCleanupIncoming() throws Exception
   {
      //Create incoming file.
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      
      ConnectionObjects incoming = createIncomingConnection(true);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      checkAttach(fConn, "incoming2.txt");
      fConn.close();
      mConn.rollback(makeXid(1));
      
      
      
      //Second connection
      incoming = createIncomingConnection(true);
      fConn = incoming.getConnection();
      mConn = incoming.getManagedConnection();
      xar = incoming.getXAR();
            
      checkAttach(fConn, "incoming2.txt");
      
      xar.start(makeXid(2), XAResource.TMNOFLAGS);
      
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      fConn.close();
      xar.end(makeXid(2), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(2)));
      mConn.commit(makeXid(2));
      
      
      // Now to check that the file was moved.
      assertEquals(0, m_incomingDirectory.listFiles().length);
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that a second connection cannot attach to an incoming file
    * that is already being processed.
    */
   public void testProcessIncomingFileMoveWithSecondConnection() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(false);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      //Make second connection
      ConnectionObjects incoming2 = createIncomingConnection(false);
      FileConnection fConn2 = incoming2.getConnection();
      XAResource xar2 = incoming2.getXAR();
      
      
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
            
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      xar2.start(makeXid(2), XAResource.TMNOFLAGS);
      
      
      checkAttach(fConn, "incoming1.txt");
      
      fConn.setExpandedProcessedName("incoming1.txt");
      
      assertFalse(fConn2.attachToFile("incoming1.txt"));
            
      
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      assertFalse(fConn2.attachToFile("incoming1.txt"));
      
      reader.close();
      fConn.close();
      
      assertFalse(fConn2.attachToFile("incoming1.txt"));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      
      assertFalse(fConn2.attachToFile("incoming1.txt"));
      
      assertTrue(mConn.prepare(makeXid(1)));
      
      assertFalse(fConn2.attachToFile("incoming1.txt"));
      
      mConn.commit(makeXid(1));
      
      assertFalse(fConn2.attachToFile("incoming1.txt"));
      
      
      // Now to check that the file was moved.
      File result = new File(m_processedDirectory, "incoming1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Incoming directory should now be empty
      assertEquals(0, m_incomingDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file may be rolled back after prepare has
    * already executed successfully. (This is an expected condition in
    * distributed transaction processing, and happens if another
    * participant in the transaction cannot prepare successfully).
    * 
    * Tests for the move to processed directory case for incoming files.
    */
   public void testProcessIncomingFileMoveWithRollbackAfterPrepare() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(false);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      long lOriginalTime = dataFile.lastModified();
      
            
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
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.rollback(makeXid(1));

      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      assertEquals(lOriginalTime, result.lastModified());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Processed directory empty
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file may be rolled back before prepare
    * has even been executed.
    * 
    * Tests for the move to processed directory case for incoming files.
    */
   public void testProcessIncomingFileMoveWithRollbackBeforePrepare() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(false);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      long lOriginalTime = dataFile.lastModified();
      
            
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      reader.close();
      fConn.close();
            
      mConn.rollback(makeXid(1));

      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      assertEquals(lOriginalTime, result.lastModified());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Processed directory empty
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file may be rolled back even before the
    * return input stream has been closed.
    * 
    * Tests for the move to processed directory case for incoming files.
    */
   public void testProcessIncomingFileMoveWithRollbackBeforeClose() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(false);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      long lOriginalTime = dataFile.lastModified();
      
            
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming1.txt");
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      mConn.rollback(makeXid(1));

      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      assertEquals(lOriginalTime, result.lastModified());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Processed directory empty
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file can be rolled back by the prepare()
    * method itself, should prepare() fail.
    * 
    * (Prepare will fail if the processed file already exists)
    */
   public void testProcessIncomingFileMoveWithRollbackCausedByPrepare() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(false);
      FileConnection fConn = incoming.getConnection();
      //FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      
      writeData(dataFile, data);
      
      //This ensures prepare() will fail
      writeData(new File(m_processedDirectory, "incoming1.txt"), "blocker".toCharArray());
      
      
      //BEGIN
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
      
      try
      {
         xar.prepare(makeXid(1));
         fail();
      }
      catch (XAException ex)
      {
         assertEquals(XAException.XA_RBROLLBACK, ex.errorCode);
      }
      
      //This is not necessary
      //mConn.rollback(makeXid(1));
      

      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming1.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Processed directory has the blocker file
      assertEquals(1, m_processedDirectory.listFiles().length);
      verifyData(new File(m_processedDirectory, "incoming1.txt"), "blocker".toCharArray());
   }


   /**
    * Tests that an incoming file may be rolled back after prepare has
    * already executed successfully. (This is an expected condition in
    * distributed transaction processing, and happens if another
    * participant in the transaction cannot prepare successfully).
    * 
    * Tests for the delete case for incoming files.
    */
   public void testProcessIncomingFileDeleteWithRollbackAfterPrepare() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(true);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      long lOriginalTime = dataFile.lastModified();
      
            
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
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.rollback(makeXid(1));
      
      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming2.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      assertEquals(lOriginalTime, result.lastModified());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Verify file not in processed
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file may be rolled back before prepare
    * has even been executed.
    * 
    * Tests for the delete case for incoming files.
    */
   public void testProcessIncomingFileDeleteWithRollbackBeforePrepare() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(true);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      long lOriginalTime = dataFile.lastModified();
      
            
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
      
      mConn.rollback(makeXid(1));
      
      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming2.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      assertEquals(lOriginalTime, result.lastModified());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Verify file not in processed
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that an incoming file may be rolled back even before the
    * return input stream has been closed.
    * 
    * Tests for the delete case for incoming files.
    */
   public void testProcessIncomingFileDeleteWithRollbackBeforeClose() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(true);
      FileConnection fConn = incoming.getConnection();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming2.txt");
      Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
      
      writer.write(data);
      writer.close();
      
      long lOriginalTime = dataFile.lastModified();
      
            
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "incoming2.txt");
      
      
      InputStream istream = fConn.getInputStream();
      InputStreamReader reader = new InputStreamReader(istream, "UTF-8");
      
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertEquals(-1, reader.read());
      assertTrue(Arrays.equals(data, resultData));
      
      mConn.rollback(makeXid(1));
      
      //Check that file is still in incoming directory.
      File result = new File(m_incomingDirectory, "incoming2.txt");
      
      assertTrue(result.exists());
      assertTrue(result.canRead());
      assertEquals(lOriginalTime, result.lastModified());
      
      Reader reader2 = IOUtil.openBufferedReader(result, XMLUtil.ENCODING);
      char[] resultData2 = new char[data.length];
      
      assertEquals(data.length, reader2.read(resultData2));
      assertEquals(-1, reader2.read());
      assertTrue(Arrays.equals(data, resultData2));
      
      reader2.close();
      
      //Verify file not in processed
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Tests that a second outgoing connection may not be made to an outgoing
    * file that is already being processed.
    */
   public void testProcessOutgoingFileWithSecondConnection() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();
      
      ConnectionObjects outgoing2 = createOutgoingConnection();
      FileConnection fConn2 = outgoing2.getConnection();
      XAResource xar2 = outgoing2.getXAR();
      
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      xar2.start(makeXid(2), XAResource.TMNOFLAGS);
      
      
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      
      assertTrue(fConn.attachToFile("out3.txt"));
      
      assertFalse(fConn2.attachToFile("out3.txt"));
      
      
      //Do file ops...
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();

      fConn.write(String.valueOf(data));

      assertFalse(fConn2.attachToFile("out3.txt"));
     
      //Finish up...
      fConn.close();
      
      assertFalse(fConn2.attachToFile("out3.txt"));
      
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      
      //COMMIT
      mConn.commit(makeXid(1));
      
      
      //Check output data.
      File result = new File(m_outgoingDirectory, "out3.txt");
      FileInputStream istream = new FileInputStream(result);
      FileLock flock = istream.getChannel().tryLock(0L, Long.MAX_VALUE, true);
      assertTrue(flock.isValid());
      
      Reader reader = new InputStreamReader(istream, "UTF-8");
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertTrue(Arrays.equals(data, resultData));
      assertEquals(-1, reader.read());
      
      flock.release();
      reader.close();
      istream.close();
      
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that an outgoing file may be rolled back after prepare has
    * already executed successfully. (This is an expected condition in
    * distributed transaction processing, and happens if another
    * participant in the transaction cannot prepare successfully).
    */
   public void testProcessOutgoingFileWithRollbackAfterPrepare() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();
      
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      fConn.write(String.valueOf(data));
      

      //Finish up...
      fConn.close();
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.rollback(makeXid(1));
      
      
      //Both directories should be empty
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that an outgoing file can be rolled back even before prepare
    * has executed successfully.
    */
   public void testProcessOutgoingFileWithRollbackBeforePrepare() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();
      
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn, "out1.txt");
      
      //Do file ops...
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      fConn.write(String.valueOf(data));

      //Finish up...
      fConn.close();
      mConn.rollback(makeXid(1));
      
      
      //Both directories should be empty
      assertEquals(0, m_outgoingDirectory.listFiles().length);
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that correct exception is thrown when the outgoing file
    * cannot be attached (due to a missing outgoing directory).
    */
   public void testProcessOutgoingFileWithFailure1() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      XAResource xar = outgoing.getXAR();
      
      
      //Make it fail by ensuring there is no outgoing directory
      assertTrue(m_outgoingDirectory.delete());
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      try
      {
         fConn.attachToFile("out2.txt");
         fail();
      }
      catch (FileConnectionException ex)
      {
         assertEquals("err.rpc.file.missingDataDirectory", ex.getErrorCode());
      }
   }


   /**
    * Tests that the outgoing connection cannot attach if an outgoing file
    * of the given name already exists in the outgoing directory.
    */
   public void testProcessOutgoingFileWithFailure2() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn = outgoing.getConnection();
      XAResource xar = outgoing.getXAR();
      
      
      //Make it fail by having a file of same name already in outgoing directory
      File conflictFile = new File(m_outgoingDirectory, "out2.txt");
      assertTrue(conflictFile.createNewFile());
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      assertFalse(fConn.attachToFile("out2.txt"));
      
      assertTrue(conflictFile.exists());
   }


   /**
    * Tests that multiple outgoing handles (some to different files, some to the same
    * files) can be created on the physical (managed) connection.
    */
   public void testSharing3OutgoingFiles() throws Exception
   {
      ConnectionObjects outgoing = createOutgoingConnection();
      FileConnection fConn1 = outgoing.getConnection();
      FileConnection fConn2 = outgoing.makeNewConnectionHandle();
      FileConnection fConn3 = outgoing.makeNewConnectionHandle();
      FileConnection fConn4 = outgoing.makeNewConnectionHandle();
      FileManagedConnection mConn = outgoing.getManagedConnection();
      XAResource xar = outgoing.getXAR();
      
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn1, "out1.txt");
      checkAttach(fConn2, "out2.txt");
      checkAttach(fConn3, "out3.txt");
      checkAttach(fConn4, "out1.txt");

      
      //Do file ops...
      fConn4.write("This is handle 4");
      
      OutputStream ostream = fConn1.getOutputStream();
      OutputStreamWriter writer = new OutputStreamWriter(ostream, "UTF-8");
      
      char[] data = "This is a test, using test outgoing data.\n".toCharArray();
      
      writer.write(data);
      writer.close();
      
      fConn2.write("Second output file.");
      fConn3.write("Third output file.");
      
      
      //Finish up...
      fConn1.close();
      fConn2.close();
      fConn3.close();
      fConn4.close();
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));
      
      
      //Test that writes were successful
      verifyData(new File(m_outgoingDirectory, "out1.txt"), ("This is handle 4" + new String(data)).toCharArray());
      verifyData(new File(m_outgoingDirectory, "out2.txt"), "Second output file.".toCharArray());
      verifyData(new File(m_outgoingDirectory, "out3.txt"), "Third output file.".toCharArray());
      
      
      //Other directory should be empty
      assertEquals(0, m_outgoingTempDirectory.listFiles().length);
   }


   /**
    * Tests that multiple incoming handles (some to different files, some to the same
    * files) can be created on the physical (managed) connection.
    */
   public void testSharing3IncomingFiles() throws Exception
   {
      ConnectionObjects incoming = createIncomingConnection(true);
      FileConnection fConn1 = incoming.getConnection();
      FileConnection fConn2 = incoming.makeNewConnectionHandle();
      FileConnection fConn3 = incoming.makeNewConnectionHandle();
      FileConnection fConn4 = incoming.makeNewConnectionHandle();
      FileManagedConnection mConn = incoming.getManagedConnection();
      XAResource xar = incoming.getXAR();
      
      
      char[] data = "This is some test incoming data.\n\nHi there!\n".toCharArray();
      File dataFile = new File(m_incomingDirectory, "incoming1.txt");
      
      writeData(dataFile, data);
      
      writeData(new File(m_incomingDirectory, "incoming2.txt"), "Second file.".toCharArray());
      writeData(new File(m_incomingDirectory, "incoming3.txt"), "Third file.".toCharArray());
            
      xar.start(makeXid(1), XAResource.TMNOFLAGS);
      
      checkAttach(fConn1, "incoming1.txt");
      checkAttach(fConn2, "incoming2.txt");
      checkAttach(fConn3, "incoming3.txt");
      checkAttach(fConn4, "incoming1.txt");
      
      verifyData(fConn1.getInputStream(), data);
      
      verifyData(fConn2.getInputStream(), "Second file.".toCharArray());
      verifyData(fConn3.getInputStream(), "Third file.".toCharArray());
      
      InputStream istream4 = fConn4.getInputStream();
      
      istream4.reset();
      verifyData(istream4, data);
      
      
      fConn1.close();
      fConn2.close();
      fConn3.close();
      fConn4.close();
      xar.end(makeXid(1), XAResource.TMSUCCESS);
      assertTrue(mConn.prepare(makeXid(1)));
      mConn.commit(makeXid(1));
      
      //Check that the file was deleted.
      assertEquals(0, m_incomingDirectory.listFiles().length);
      assertEquals(0, m_processedDirectory.listFiles().length);
   }


   /**
    * Checks the directory tree creation routine.
    * 
    * @throws Exception
    */
   public void testDirectoryNameSplit() throws Exception
   {
      File baseDir = m_tempDirectory;
      File result;
      
      result = FileManagedConnection.splitNameToSubdirs(baseDir, "a", 3, 2, false);
      assertEquals(new File(m_tempDirectory, "a_" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "a"), result);
      
      result = FileManagedConnection.splitNameToSubdirs(baseDir, "ab", 3, 2, false);
      assertEquals(new File(m_tempDirectory, "ab" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "ab"), result);
      
      result = FileManagedConnection.splitNameToSubdirs(baseDir, "abacadaeaf", 3, 2, false);
      assertEquals(new File(m_tempDirectory, "ab" + SysUtil.FILE_SEP + "ac" + SysUtil.FILE_SEP + "ad" + SysUtil.FILE_SEP + "abacadaeaf"), result);
   }
}
