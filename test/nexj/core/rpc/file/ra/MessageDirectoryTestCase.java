// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Arrays;

import javax.resource.ResourceException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.runtime.platform.generic.tx.GenericXid;
import nexj.core.util.IOUtil;
import nexj.test.util.TempFileUtil;

import junit.framework.TestCase;

/**
 * A helper test case class for making tests of the transactional file
 * adapter easier. For each test in a subclass, this class sets up a
 * temporary directory specifically for that test, and also creates
 * message directories within that temporary directory.
 */
public abstract class MessageDirectoryTestCase extends TestCase
{
   // associations
   
   /**
    * The temporary directory specific to the currently-running test.
    */
   protected File m_tempDirectory;

   /**
    * The incoming directory for the currently-running test.
    */
   protected File m_incomingDirectory;

   /**
    * The processed directory for the currently-running test.
    */
   protected File m_processedDirectory;

   /**
    * The outgoing message directory for the currently-running test.
    */
   protected File m_outgoingDirectory;

   /**
    * The temporary directory for the currently-running test.
    */
   protected File m_outgoingTempDirectory;

   /**
    * The journal for incoming operations for the currently-running test.
    */
   protected File m_incomingJournal;

   /**
    * The journal for outgoing operations for the currently-running test.
    */
   protected File m_outgoingJournal;


   // operations

   /**
    * Asserts that attaching to the file sName on connection fConn succeeds.
    * 
    * @param fConn The connection to use.
    * @param sName The file name to attach.
    */
   public void checkAttach(FileConnection fConn, String sName) throws IOException
   {
      assertTrue(fConn.attachToFile(sName));
   }


   /**
    * Creates a persistence connection.
    * 
    * @return The (ManagedConnection, Connection, XAResource) tuple that
    *         was created.
    * @throws ResourceException
    */
   public ConnectionObjects createPersistenceConnection() throws ResourceException
   {
      FileManagedConnectionFactory factory = new FileManagedConnectionFactory();
      
      factory.setJournalDirectory(m_outgoingJournal.getAbsolutePath());
      factory.setOutgoingDirectory(m_outgoingDirectory.getAbsolutePath());
      factory.setOutgoingTempDirectory(m_outgoingTempDirectory.getAbsolutePath());
      factory.setInputConnection(false);
      factory.setPersistenceConnection(true);
      
      FileManagedConnection mConn = (FileManagedConnection)factory.createManagedConnection(null, null);

      mConn.setMaxNameSplits(0);
      mConn.setNameSplitSize(0);
      
      FileXAResource xar = (FileXAResource)mConn.getXAResource();
      FileConnection fConn = (FileConnection)mConn.createConnection(null, null);
      
      return new ConnectionObjects(mConn, fConn, xar);
   }


   /**
    * Creates an outgoing connection.
    * 
    * @return The (ManagedConnection, Connection, XAResource) tuple that
    *         was created.
    * @throws ResourceException
    */
   public ConnectionObjects createOutgoingConnection() throws ResourceException
   {
      FileManagedConnectionFactory factory = new FileManagedConnectionFactory();
      
      factory.setJournalDirectory(m_outgoingJournal.getAbsolutePath());
      factory.setOutgoingDirectory(m_outgoingDirectory.getAbsolutePath());
      factory.setOutgoingTempDirectory(m_outgoingTempDirectory.getAbsolutePath());
      factory.setInputConnection(false);
      factory.setPersistenceConnection(false);
      
      FileManagedConnection mConn = (FileManagedConnection)factory.createManagedConnection(null, null);

      FileXAResource xar = (FileXAResource)mConn.getXAResource();
      FileConnection fConn = (FileConnection)mConn.createConnection(null, null);
      
      return new ConnectionObjects(mConn, fConn, xar);
   }


   /**
    * Creates an outgoing connection with the temporary directory (used for
    * preparing outgoing messages) located inside the outgoing directory.
    * 
    * @return The (ManagedConnection, Connection, XAResource) tuple that
    *         was created.
    * @throws ResourceException
    */
   public ConnectionObjects createOutgoingConnectionWithTempInOutgoing() throws ResourceException
   {
      m_outgoingTempDirectory = new File(m_outgoingDirectory, "tmp");
      assertTrue(m_outgoingTempDirectory.mkdir());
      
      return createOutgoingConnection();
   }


   /**
    * Creates an incoming connection.
    * 
    * @param bDelete True to delete the file after it has been processed;
    *                false to move it to the processed directory (under
    *                its original name).
    * @return The (ManagedConnection, Connection, XAResource) tuple that
    *         was created.
    * @throws ResourceException
    */
   public ConnectionObjects createIncomingConnection(boolean bDelete) throws ResourceException
   {
      FileManagedConnectionFactory factory = new FileManagedConnectionFactory();
      
      factory.setJournalDirectory(m_incomingJournal.getAbsolutePath());
      factory.setIncomingDirectory(m_incomingDirectory.getAbsolutePath());
      factory.setProcessedDirectory(bDelete ? null : m_processedDirectory.getAbsolutePath());
      factory.setInputConnection(true);
      factory.setPersistenceConnection(false);
      
      FileManagedConnection mConn = (FileManagedConnection)factory.createManagedConnection(null, null);
      
      FileXAResource xar = (FileXAResource)mConn.getXAResource();
      FileConnection fConn = (FileConnection)mConn.createConnection(null, null);
      
      return new ConnectionObjects(mConn, fConn, xar);
   }

   /**
    * @see junit.framework.TestCase#setUp()
    */
   public void setUp() throws IOException
   {
      m_tempDirectory = TempFileUtil.makeTemporaryDirectory(getClass().getName() + "." + getName());
      
      m_incomingDirectory = new File(m_tempDirectory, "incoming");
      m_processedDirectory = new File(m_tempDirectory, "processed");
      m_outgoingDirectory = new File(m_tempDirectory, "outgoing");
      m_outgoingTempDirectory = new File(m_tempDirectory, "outtemp");
      
      assertTrue(m_incomingDirectory.mkdir());
      assertTrue(m_processedDirectory.mkdir());
      assertTrue(m_outgoingDirectory.mkdir());
      assertTrue(m_outgoingTempDirectory.mkdir());
      
      m_incomingJournal = new File(m_tempDirectory, "injournal");
      m_outgoingJournal = new File(m_tempDirectory, "outjournal");
      
      assertTrue(m_incomingJournal.mkdir());
      assertTrue(m_outgoingJournal.mkdir());
   }


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
      byte[] nTxIdArray = new byte[GenericXid.MAXGTRIDSIZE];
      byte[] nBrQArray = new byte[GenericXid.MAXBQUALSIZE];
      
      Arrays.fill(nTxIdArray, (byte)'G');
      nTxIdArray[0] = (byte)((nXidNumber >> 24) & 0xff);
      nTxIdArray[1] = (byte)((nXidNumber >> 16) & 0xff);
      nTxIdArray[2] = (byte)((nXidNumber >> 8) & 0xff);
      nTxIdArray[3] = (byte)(nXidNumber & 0xff);
      
      Arrays.fill(nBrQArray, (byte)'B');
      
      return new GenericXid(nTxIdArray, nBrQArray);
   }


   /**
    * Writes data to an OutputStream on fConn.
    * 
    * @param fConn The connection to use to write data.
    * @param data  The data to write.
    * @throws IOException
    */
   protected void writeData(FileConnection fConn, char[] data) throws IOException
   {
      OutputStream ostream = fConn.getOutputStream();
      OutputStreamWriter writer = new OutputStreamWriter(ostream, "UTF-8");
      
      writer.write(data);
      writer.close();
      ostream.close();
   }


   /**
    * Writes data to file theFile, destroying the previous contents of
    * the file, if any.
    * 
    * @param theFile The file to which data will be written.
    * @param data    The data to write.
    * @throws IOException
    */
   protected void writeData(File theFile, byte[] data) throws IOException
   {
      FileOutputStream ostream = new FileOutputStream(theFile);
      ByteArrayInputStream istream = new ByteArrayInputStream(data);
      
      IOUtil.copy(ostream, istream);
      ostream.close();
   }


   /**
    * Writes data to file theFile, destroying the previous contents of
    * the file, if any. Characters are written with UTF-8 encoding.
    * 
    * @param theFile The file to which data will be written.
    * @param data    The data to write.
    * @throws IOException
    */
   protected void writeData(File theFile, char[] data) throws IOException
   {
      FileOutputStream ostream = new FileOutputStream(theFile);
      OutputStreamWriter writer = new OutputStreamWriter(ostream, "utf-8");
      
      writer.write(data);
      writer.close();
      ostream.close();
   }


   /**
    * Asserts that file theFile has contents equal to data, encoded with
    * UTF-8 encoding.
    * 
    * @param theFile The file whose contents should be verified.
    * @param data    The expected contents of the file.
    * @throws IOException
    */
   protected void verifyData(File theFile, char[] data) throws IOException
   {
      assertTrue(theFile.exists());
      assertTrue(theFile.canRead());
      
      FileInputStream istream = new FileInputStream(theFile);
      
      verifyData(istream, data);
      istream.close();
   }


   /**
    * Asserts that the next bytes read from istream match the UTF-8
    * encoded characters in data, and that the stream ends immediately.
    * 
    * @param istream The stream to read.
    * @param data    The expected values for the next bytes in the stream.
    * @throws IOException
    */
   protected void verifyData(InputStream istream, char[] data) throws IOException
   {
      InputStreamReader reader = new InputStreamReader(istream, "utf-8");
      char[] resultData = new char[data.length];
      
      assertEquals(data.length, reader.read(resultData));
      assertTrue(Arrays.equals(data, resultData));
      assertEquals(-1, reader.read());
      
      reader.close();
   }


   /**
    * Asserts that file theFile has binary contents equal to data.
    * 
    * @param theFile The file whose contents should be verified.
    * @param data    The expected contents of the file.
    * @throws IOException
    */
   protected void verifyData(File theFile, byte[] data) throws IOException
   {
      assertTrue(theFile.exists());
      assertTrue(theFile.canRead());
      
      FileInputStream istream = new FileInputStream(theFile);
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      
      IOUtil.copy(ostream, istream);
      istream.close();
      
      byte[] resultData = ostream.toByteArray();
      
      assertEquals(data.length, resultData.length);
      assertTrue(Arrays.equals(data, resultData));
   }


   // inner classes

   /**
    * Holds a set of connection objects that have been configured to use test configuration.
    */
   public static class ConnectionObjects
   {
      // associations
      protected FileManagedConnection m_managedConnection;
      
      protected FileConnection m_connection;
      
      protected FileXAResource m_xar;
      
      
      // operations
      
      public ConnectionObjects(FileManagedConnection manConn, FileConnection conn, FileXAResource xar)
      {
         m_managedConnection = manConn;
         m_connection = conn;
         m_xar = xar;
      }
      
      public FileManagedConnection getManagedConnection()
      {
         return m_managedConnection;
      }
      
      public FileConnection getConnection()
      {
         return m_connection;
      }
      
      public XAResource getXAR()
      {
         return m_xar;
      }

      public FileConnection makeNewConnectionHandle() throws ResourceException
      {
         return (FileConnection)m_managedConnection.createConnection(null, null);
      }
   }
}
