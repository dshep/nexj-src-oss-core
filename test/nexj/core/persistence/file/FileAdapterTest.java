// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.Writer;
import java.util.Locale;
import java.util.Properties;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.file.FileDataSourceFragment;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.persistence.OID;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.Query;
import nexj.core.rpc.file.ra.FileManagedConnection;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.SchemeParser;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.test.util.TempFileUtil;


/**
 * Test the file persistence adapter.
 */
public class FileAdapterTest extends TestCase
{
   // associations

   /**
    * Cached environment.
    */
   private GlobalEnvironment m_env;


   // operations

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      m_env = new GlobalEnvironment();
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_env = null;
   }

   /**
    * Tests fragment replication of creates, updates, and deletes
    * with the file persistence adapter.
    */
   public void testFragmentReplicationUpdateDelete() throws Exception
   {
      Metadata metadata = loadMetadata("filestoragefragment");
      InvocationContext context = new InvocationContext(metadata);
      
      try
      {
         ThreadContextHolder.setContext(context);
      
         File mainDir = TempFileUtil.makeTemporaryDirectory(getName());
         File tempDir = new File(mainDir, "temp");
         File dataDir = new File(mainDir, "data");
         File jrnlDir = new File(mainDir, "journal");
         
         File tempDir1 = new File(mainDir, "temp1");
         File dataDir1 = new File(mainDir, "data1");
         File jrnlDir1 = new File(mainDir, "journal1");
         
         assertTrue(tempDir.mkdir());
         assertTrue(dataDir.mkdir());
         assertTrue(jrnlDir.mkdir());
         
         assertTrue(tempDir1.mkdir());
         assertTrue(dataDir1.mkdir());
         assertTrue(jrnlDir1.mkdir());
         
         DataSource ds = metadata.getDataSource("TestFilePersistenceDataSource");
         
         assertEquals(2, ds.getFragmentCount());
         
         FileDataSourceFragment defaultFrag = (FileDataSourceFragment)ds.getDefaultFragment();
         FileDataSourceFragment fragment1 = (FileDataSourceFragment)ds.getFragment("fragment1");
         
         defaultFrag.debugSettings(dataDir.getAbsolutePath(),
            tempDir.getAbsolutePath(),
            jrnlDir.getAbsolutePath());
         
         fragment1.debugSettings(dataDir1.getAbsolutePath(),
            tempDir1.getAbsolutePath(),
            jrnlDir1.getAbsolutePath());
         
         // Create it
         assertNull(context.beginTransaction());
         
         Metaclass testFPA = metadata.getMetaclass("TestFPA");
         Instance a = (Instance)testFPA.invoke("new");
         
         a.setValue("data", new Binary("Creation (\u4e2d\u6587) data.".getBytes("utf-8")));
         
         // Replicate
         context.getUnitOfWork().addReplicationFragment(a, "fragment1");
         
         commit(context);
   
         OID oidA = a.getOID();
         String sOidName = (String)oidA.getValue(0);
         
         // Verify
         File defaultFile = FileManagedConnection.splitNameToSubdirs(dataDir, sOidName, 3, 2, false);
         
         assertTrue(defaultFile.exists());
         assertEquals("Creation (\u4e2d\u6587) data.", readFileToString(defaultFile));
         
         File fragment1File = FileManagedConnection.splitNameToSubdirs(dataDir1, sOidName, 3, 2, false);
         
         assertTrue(fragment1File.exists());
         assertEquals("Creation (\u4e2d\u6587) data.", readFileToString(fragment1File));
         
         // Update it
         assertNull(context.beginTransaction());
         
         a.setValue("data", new Binary("Modified data.\nModified.".getBytes("utf-8")));
         
         // Replication
         context.getUnitOfWork().addReplicationFragment(a, "fragment1");
         
         commit(context);
         
         // Verify
         assertTrue(defaultFile.exists());
         assertEquals("Modified data.\nModified.", readFileToString(defaultFile));
         assertTrue(fragment1File.exists());
         assertEquals("Modified data.\nModified.", readFileToString(fragment1File));
         
         // Delete it
         assertNull(context.beginTransaction());
         
         a.invoke("delete");
         
         // Replication
         context.getUnitOfWork().addReplicationFragment(a, "fragment1");
         
         commit(context);
         
         //Verify
         assertFalse(defaultFile.exists());
         assertFalse(fragment1File.exists());
      }
      finally
      {
         context.complete(false);
         ThreadContextHolder.setContext(null);
      }
   }


   /**
    * Tests basic functionality of adapter by executing create,
    * read, update, and delete statements. Uses adapter to
    * verify that the operations are performed successfully.
    */
   public void testCreateReadUpdateDelete() throws Exception
   {
      Metadata metadata = loadMetadata("filestorage");
      InvocationContext context = new InvocationContext(metadata);
      
      try
      {
         ThreadContextHolder.setContext(context);
      
         /* ***** Configure the data source connection with temporary directories ***** */
         File mainDir = TempFileUtil.makeTemporaryDirectory(getName());
         File tempDir = new File(mainDir, "temp");
         File dataDir = new File(mainDir, "data");
         File jrnlDir = new File(mainDir, "journal");
         
         assertTrue(tempDir.mkdir());
         assertTrue(dataDir.mkdir());
         assertTrue(jrnlDir.mkdir());
         
         
         DataSource ds = metadata.getDataSource("TestFilePersistenceDataSource");
         
         assertEquals(1, ds.getFragmentCount());
         
         FileDataSourceFragment defaultFrag = (FileDataSourceFragment)ds.getDefaultFragment();
         
         defaultFrag.debugSettings(dataDir.getAbsolutePath(),
            tempDir.getAbsolutePath(),
            jrnlDir.getAbsolutePath());
         
         
         // Create it
         assertNull(context.beginTransaction());
         
         Metaclass testFPA = metadata.getMetaclass("TestFPA");
         Instance a = (Instance)testFPA.invoke("new");
         
         a.setValue("data", new Binary("Creation data.".getBytes("utf-8")));
         
         commit(context);
        
         OID oidA = a.getOID();
         String sOidName = (String)oidA.getValue(0);
         
         // Read
         assertNull(context.beginTransaction());
         a.invoke("read", new Object[]{
            parse("(data)"),
            parse("(= (@ id) \"" + sOidName + "\")"), null, null, null, null
         });
         assertEquals(new Binary("Creation data.".getBytes("utf-8")), a.getValue("data"));
   
         // Update
         a.setValue("data", new Binary("Update data.".getBytes("utf-8")));
         
         commit(context);
         
         // Read
         assertNull(context.beginTransaction());
         a.invoke("read", new Object[]{
            parse("(data)"),
            parse("(= (@ id) \"" + sOidName + "\")"), null, null, null, null
         });
         assertEquals(new Binary("Update data.".getBytes("utf-8")), a.getValue("data"));
         
         // Delete
         a.invoke("delete");
         
         commit(context);
         
         // Read (NOT FOUND)
         assertNull(context.beginTransaction());
            
         Query query = Query.createRead(testFPA, null, parse("(= (@ id) \"" + sOidName + "\")"), null, 10, 0, false, Query.SEC_NONE, context);
         InstanceList resultList = query.read();
            
         assertEquals(0, resultList.getCount());
         
         commit(context);

         // Create it (test that the read operation doesn't add the not-found instance to the invocation context)
         assertNull(context.beginTransaction());

         a = (Instance)testFPA.invoke("new");
         a.setOID(oidA);
         a.setValue("data", new Binary("Creation data #2.".getBytes("utf-8")));

         commit(context);
      }
      finally
      {
         context.complete(false);
         ThreadContextHolder.setContext(null);
      }
   }


   /**
    * Tests that the adapter can create multiple files in one transaction.
    */
   public void testCreateMultiple() throws Exception
   {
      Metadata metadata = loadMetadata("filestorage");
      InvocationContext context = new InvocationContext(metadata);
      
      try
      {
         ThreadContextHolder.setContext(context);
      
         /* ***** Configure the data source connection with temporary directories ***** */
         File mainDir = TempFileUtil.makeTemporaryDirectory(getName());
         File tempDir = new File(mainDir, "temp");
         File dataDir = new File(mainDir, "data");
         File jrnlDir = new File(mainDir, "journal");
         
         assertTrue(tempDir.mkdir());
         assertTrue(dataDir.mkdir());
         assertTrue(jrnlDir.mkdir());
         
         
         DataSource ds = metadata.getDataSource("TestFilePersistenceDataSource");
         
         assertEquals(1, ds.getFragmentCount());
         
         FileDataSourceFragment defaultFrag = (FileDataSourceFragment)ds.getDefaultFragment();
         
         defaultFrag.debugSettings(dataDir.getAbsolutePath(),
            tempDir.getAbsolutePath(),
            jrnlDir.getAbsolutePath());
         
         
         // Create it
         assertNull(context.beginTransaction());
         
         Metaclass testFPA = metadata.getMetaclass("TestFPA");
         Instance a = (Instance)testFPA.invoke("new");
         
         a.setValue("data", new Binary("Creation data for 'a'.".getBytes("utf-8")));
         
         Instance b = (Instance)testFPA.invoke("new");
         
         b.setValue("data", new Binary("Creation data for 'b'.".getBytes("utf-8")));
         
         commit(context);
        
         OID oidA = a.getOID();
         String sOidNameA = (String)oidA.getValue(0);
         OID oidB = b.getOID();
         String sOidNameB = (String)oidB.getValue(0);

         // Read
         assertNull(context.beginTransaction());
         a.invoke("read", new Object[]{
            parse("(data)"),
            parse("(= (@ id) \"" + sOidNameA + "\")"), null, null, null, null
         });
         assertEquals(new Binary("Creation data for 'a'.".getBytes("utf-8")), a.getValue("data"));
   
         b.invoke("read", new Object[]{
            parse("(data)"),
            parse("(= (@ id) \"" + sOidNameB + "\")"), null, null, null, null
         });
         assertEquals(new Binary("Creation data for 'b'.".getBytes("utf-8")), b.getValue("data"));

         commit(context);
      }
      finally
      {
         context.complete(false);
         ThreadContextHolder.setContext(null);
      }
   }


   /**
    * Tests that optimistic locking prevents UPDATE if file is modified
    * externally.
    */
   public void testOptimisticLockingUpdateFail() throws Exception
   {
      Metadata metadata = loadMetadata("filestorage");
      InvocationContext context = new InvocationContext(metadata);

      try
      {
         /* ***** Configure the data source connection with temporary directories ***** */
         File mainDir = TempFileUtil.makeTemporaryDirectory(getName());
         File tempDir = new File(mainDir, "temp");
         File dataDir = new File(mainDir, "data");
         File jrnlDir = new File(mainDir, "journal");
         
         assertTrue(tempDir.mkdir());
         assertTrue(dataDir.mkdir());
         assertTrue(jrnlDir.mkdir());
         
         
         DataSource ds = metadata.getDataSource("TestFilePersistenceDataSource");
         
         assertEquals(1, ds.getFragmentCount());
         
         FileDataSourceFragment defaultFrag = (FileDataSourceFragment)ds.getDefaultFragment();
         
         defaultFrag.debugSettings(dataDir.getAbsolutePath(),
            tempDir.getAbsolutePath(),
            jrnlDir.getAbsolutePath());
         
         //Write a data file
         File dataFile = new File(dataDir, "u_" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "u");
         
         assertTrue(dataFile.getParentFile().mkdirs());

         Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);

         writer.write("Original contents");
         writer.close();
         
         long lOriginalModified = dataFile.lastModified();
         
         //Now try to do a query.
         assertNull(context.beginTransaction());
         
         Metaclass testFPA = metadata.getMetaclass("TestFPA");
         Object where = parse("(= (@ id) \"u\")");
         Query query = Query.createRead(testFPA, null, where, null, 10, 0, false, Query.SEC_NONE, context);
         
         InstanceList resultList = query.read();
         
         assertEquals(1, resultList.size());
         
         Instance result = resultList.getInstance(0);
         
         assertEquals(new Binary("Original contents".getBytes("utf-8")), result.getValue("data"));
         
         result.setValue("data", new Binary("Modified through adapter.".getBytes("utf-8")));
         
         //Edit the data file outside the adapter
         writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
         writer.write("Modified directly.");
         writer.close();
         dataFile.setLastModified(lOriginalModified + 1);
         
         //Try to commit
         try
         {
            commit(context);
            fail("Expected OptimisticLockException");
         }
         catch (OptimisticLockException ex)
         {
            assertEquals("TestFPA", ex.getClassName());
         }
         
         context.getUnitOfWork().rollback(true);
         
         assertNull(context.beginTransaction());
         
         //Re-read
         result.invoke("read", new Object[]{
            parse("(data)"),
            parse("(= (@ id) \"u\")"), null, null, null, null
         });
         assertEquals(new Binary("Modified directly.".getBytes("utf-8")), result.getValue("data"));
      }
      finally
      {
         context.complete(false);
         ThreadContextHolder.setContext(null);
      }
   }


   /**
    * Tests that optimistic locking prevents DELETE if file is modified
    * externally.
    */
   public void testOptimisticLockingDeleteFail() throws Exception
   {
      Metadata metadata = loadMetadata("filestorage");
      InvocationContext context = new InvocationContext(metadata);
      
      try
      {
         /* ***** Configure the data source connection with temporary directories ***** */
         File mainDir = TempFileUtil.makeTemporaryDirectory(getName());
         File tempDir = new File(mainDir, "temp");
         File dataDir = new File(mainDir, "data");
         File jrnlDir = new File(mainDir, "journal");
         
         assertTrue(tempDir.mkdir());
         assertTrue(dataDir.mkdir());
         assertTrue(jrnlDir.mkdir());
         
         
         DataSource ds = metadata.getDataSource("TestFilePersistenceDataSource");
         
         assertEquals(1, ds.getFragmentCount());
         
         FileDataSourceFragment defaultFrag = (FileDataSourceFragment)ds.getDefaultFragment();
         
         defaultFrag.debugSettings(dataDir.getAbsolutePath(),
            tempDir.getAbsolutePath(),
            jrnlDir.getAbsolutePath());
         
         //Write a data file
         File dataFile = new File(dataDir, "d_" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "__" + SysUtil.FILE_SEP + "d");
         
         assertTrue(dataFile.getParentFile().mkdirs());
         
         Writer writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
         
         writer.write("Original contents");
         writer.close();
         
         long lOriginalModified = dataFile.lastModified();
         
         //Now try to do a query.
         assertNull(context.beginTransaction());
         
         Metaclass testFPA = metadata.getMetaclass("TestFPA");
         Object where = parse("(= (@ id) \"d\")");
         Query query = Query.createRead(testFPA, null, where, null, 10, 0, false, Query.SEC_NONE, context);
         
         InstanceList resultList = query.read();
         
         assertEquals(1, resultList.size());
         
         Instance result = resultList.getInstance(0);
         
         assertEquals(new Binary("Original contents".getBytes("utf-8")), result.getValue("data"));
         
         //DELETE it
         result.invoke("delete");
         
         //Edit the data file outside the adapter
         writer = IOUtil.openBufferedWriter(dataFile, XMLUtil.ENCODING);
         writer.write("Modified directly.");
         writer.close();
         dataFile.setLastModified(lOriginalModified + 1);
         
         //Try to commit
         try
         {
            commit(context);
            fail("Expected OptimisticLockException");
         }
         catch (OptimisticLockException ex)
         {
            assertEquals("TestFPA", ex.getClassName());
         }
         
         context.getUnitOfWork().rollback(true);
         
         assertNull(context.beginTransaction());
         
         //Re-read
         result.invoke("read", new Object[]{
            parse("(data)"),
            parse("(= (@ id) \"d\")"), null, null, null, null
         });

         assertEquals(new Binary("Modified directly.".getBytes("utf-8")), result.getValue("data"));
      }
      finally
      {
         context.complete(false);
         ThreadContextHolder.setContext(null);
      }
   }


   /**
    * Parse the given scheme string.
    * 
    * @param sExpr Some scheme code.
    * @return The result of running the scheme parser on the parameter.
    */
   protected Object parse(String sExpr)
   {
      return new SchemeParser(m_env).parse(new StringReader(sExpr), null);
   }

   /**
    * Loads a repository configured for a given persistence engine.
    * @param sAdapter The engine to use.
    * @return The loaded metadata object.
    */
   protected static Metadata loadMetadata(String sAdapter)
   {
      Properties props = SysUtil.getConfigProperties();

      props = new Properties(props);
      props.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/" + sAdapter.toLowerCase(Locale.ENGLISH) + ".connections");

      return new MetadataLoaderDispatcher().load(null, props, 0, null);
   }


   /**
    * Reads a file as a string, using utf-8 encoding.
    * 
    * @param f The file to read.
    * @return The string representing the contents of the file, decoded using utf-8.
    */
   protected static String readFileToString(File f) throws Exception
   {
      FileInputStream istream = new FileInputStream(f);
      InputStreamReader reader = new InputStreamReader(istream, "utf-8");
      BufferedReader reader2 = new BufferedReader(reader);
      
      int nRead = reader2.read();
      StringBuilder buf = new StringBuilder();
      
      while (nRead >= 0)
      {
         buf.appendCodePoint(nRead);
         nRead = reader2.read();
      }
      
      reader2.close();
      reader.close();
      istream.close();
      
      return buf.toString();
   }

   /**
    * Commits the current unit of work and the distributed transaction.
    * @param context The context to get the current unit of work.
    */
   protected static void commit(InvocationContext context)
   {
      context.getUnitOfWork().setRaw(true);
      context.getUnitOfWork().commit(true);
   }
}
