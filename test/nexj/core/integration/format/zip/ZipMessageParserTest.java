// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.zip;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.StreamInput;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.SysUtil;

import junit.framework.TestCase;

/**
 * Tests the Zip format message parser.
 * 
 * Note that, due to deficiencies in the Java standard zip implementation,
 * the Zip file written by ZipOutputStream doesn't write the size information
 * before each Zip entry, but after those entries, instead. When read by a
 * ZipInputStream, the file size information will not be retrieved (though
 * most other software capable of reading Zip files will be able to retrieve
 * it).
 */
public class ZipMessageParserTest extends TestCase
{
   // attributes

   /**
    * The parser instance.
    */
   protected MessageParser m_parser;

   /**
    * The Zip message instance--only one is needed.
    */
   protected Message m_message;


   // operations

   /**
    * Sets up a parser and message instance for use by test cases.
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      Format format = Repository.getMetadata().getFormat("Zip");
      m_parser = (MessageParser)format.getParser().getInstance(null);
      m_message = Repository.getMetadata().getMessage("Zip_AllMapped");
   }

   /**
    * Tests parsing of a file created by 7Zip.
    */
   public void testFrom7ZipFile() throws Exception
   {
      Binary zipBin = Binary.parse("504b03040a0000000000756f44380000" +
                   "00000000000000000000090000007365" +
                   "696e66656c642f504b03040a00000000" +
                   "00756f44380000000000000000000000" +
                   "00140000007365696e66656c642f6368" +
                   "61726163746572732f504b03040a0000" +
                   "000000656f4438d49ad5361300000013" +
                   "0000001e0000007365696e66656c642f" +
                   "636861726163746572732f67656f7267" +
                   "652e747874436f6e74656e7473206f66" +
                   "2067656f7267652e504b03040a000000" +
                   "0000676f4438f96918f2120000001200" +
                   "00001d0000007365696e66656c642f63" +
                   "6861726163746572732f6a657272792e" +
                   "747874436f6e74656e7473206f66206a" +
                   "657272792e504b010214000a00000000" +
                   "00756f44380000000000000000000000" +
                   "00090000000000000000001000000000" +
                   "0000007365696e66656c642f504b0102" +
                   "14000a0000000000756f443800000000" +
                   "00000000000000001400000000000000" +
                   "000010000000270000007365696e6665" +
                   "6c642f636861726163746572732f504b" +
                   "010214000a0000000000656f4438d49a" +
                   "d53613000000130000001e0000000000" +
                   "0000000020000000590000007365696e" +
                   "66656c642f636861726163746572732f" +
                   "67656f7267652e747874504b01021400" +
                   "0a0000000000676f4438f96918f21200" +
                   "0000120000001d000000000000000000" +
                   "20000000a80000007365696e66656c64" +
                   "2f636861726163746572732f6a657272" +
                   "792e747874504b050600000000040004" +
                   "0010010000f500000000000a");

      TransferObject root = m_parser.parse(new ObjectInput(zipBin), m_message);
      List filesList = (List)root.getValue("files");

      assertEquals("Zip_AllMapped", root.getClassName());
      assertNotNull(filesList);
      assertEquals(4, filesList.size());

      TransferObject file;

      file = (TransferObject)filesList.get(0);
      assertEquals("seinfeld" + SysUtil.FILE_SEP, file.getValue("fileName"));
      assertTrue(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertEquals(0, ((Long)file.getValue("fileSize")).longValue());
      assertNull(file.getValue("data"));

      file = (TransferObject)filesList.get(1);
      assertEquals("seinfeld" + SysUtil.FILE_SEP + "characters"  + SysUtil.FILE_SEP, file.getValue("fileName"));
      assertTrue(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertEquals(0, ((Long)file.getValue("fileSize")).longValue());
      assertNull(file.getValue("data"));

      file = (TransferObject)filesList.get(2);
      assertEquals("seinfeld" + SysUtil.FILE_SEP + "characters" + SysUtil.FILE_SEP + "george.txt", file.getValue("fileName"));
      assertFalse(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertEquals(19, ((Long)file.getValue("fileSize")).longValue());
      assertEquals(new Binary("Contents of george.".getBytes("UTF-8")), file.getValue("data"));

      file = (TransferObject)filesList.get(3);
      assertEquals("seinfeld" + SysUtil.FILE_SEP + "characters" + SysUtil.FILE_SEP + "jerry.txt", file.getValue("fileName"));
      assertFalse(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertEquals(18, ((Long)file.getValue("fileSize")).longValue());
      assertEquals(new Binary("Contents of jerry.".getBytes("UTF-8")), file.getValue("data"));
   }


   /**
    * Tests parsing of a file created by a java ZipOutputStream.
    * Note that the ZipOutputStream writes size and other information
    * in such a way that it cannot be retrieved by the input stream, so
    * some data fields will be blank. 
    */
   public void testParseJavaGeneratedZip() throws Exception
   {
      // Create a zip file in memory
      ZipMaker maker = new ZipMaker();

      maker.addFile("a", "Contents of file a.");
      maker.addDirectory("b/");
      maker.addFile("b/c", "Contents of file c in directory b."); 

      // Parse it
      TransferObject root = m_parser.parse(
         new StreamInput(maker.getInputStream()), m_message);
      List filesList = (List)root.getValue("files");

      assertEquals("Zip_AllMapped", root.getClassName());
      assertNotNull(filesList);
      assertEquals(3, filesList.size());

      TransferObject file;

      file = (TransferObject)filesList.get(0);
      assertEquals("a", file.getValue("fileName"));
      assertEquals(new Binary("Contents of file a.".getBytes("UTF-8")), file.getValue("data"));
      assertEquals(19, ((Long)file.getValue("fileSize")).longValue());
      assertFalse(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertNull(file.getValue("extra"));

      file = (TransferObject)filesList.get(1);
      assertEquals("b" + SysUtil.FILE_SEP, file.getValue("fileName"));
      assertTrue(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertNull(file.getValue("data"));

      file = (TransferObject)filesList.get(2);
      assertEquals("b" + SysUtil.FILE_SEP + "c", file.getValue("fileName"));
      assertEquals(new Binary("Contents of file c in directory b.".getBytes("UTF-8")), file.getValue("data"));
      assertEquals(34, ((Long)file.getValue("fileSize")).longValue());
      assertFalse(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertNull(file.getValue("extra"));
   }

   /**
    * Tests parsing of a message table containing a single
    * Zip format message.
    */
   public void testParseMessageTable() throws Exception
   {
      // Create a zip file in memory
      ZipMaker maker = new ZipMaker();

      maker.addFile("a", "Contents of file a.");
      maker.addDirectory("b/");
      maker.addFile("b/c", "Contents of file c in directory b."); 

      // Parse it
      MessageTable table = new MessageTable();

      table.addMessage(m_message);
      m_parser.initializeMessageTable(table);

      TransferObject root = m_parser.parse(
         new StreamInput(maker.getInputStream()), table);
      List filesList = (List)root.getValue("files");

      assertEquals("Zip_AllMapped", root.getClassName());
      assertNotNull(filesList);
      assertEquals(3, filesList.size());

      TransferObject file;

      file = (TransferObject)filesList.get(0);
      assertEquals("a", file.getValue("fileName"));
      assertEquals(new Binary("Contents of file a.".getBytes("UTF-8")), file.getValue("data"));
      assertFalse(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertNull(file.getValue("extra"));

      file = (TransferObject)filesList.get(1);
      assertEquals("b" + SysUtil.FILE_SEP, file.getValue("fileName"));
      assertTrue(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertNull(file.getValue("data"));

      file = (TransferObject)filesList.get(2);
      assertEquals("b" + SysUtil.FILE_SEP + "c", file.getValue("fileName"));
      assertEquals(new Binary("Contents of file c in directory b.".getBytes("UTF-8")), file.getValue("data"));
      assertFalse(((Boolean)file.getValue("isDirectory")).booleanValue());
      assertNull(file.getValue("extra"));
   }

   /**
    * Tests two invalid message tables.
    */
   public void testInvalidMessageTable() throws Exception
   {
      try
      {
         MessageTable table = new MessageTable();

         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException ex)
      {
      }

      try
      {
         // Create a zip file in memory
         ZipMaker maker = new ZipMaker();

         maker.addFile("a", "Contents of file a.");
         maker.addDirectory("b/");
         maker.addFile("b/c", "Contents of file c in directory b."); 


         MessageTable table = new MessageTable();

         m_parser.parse(new StreamInput(maker.getInputStream()), table);
         fail();
      }
      catch (IntegrationException ex)
      {
      }
   }


   // inner classes

   /**
    * A utility class for creating a Zip file in memory and getting
    * an input stream from that file.
    */
   protected static class ZipMaker
   {
      // associations

      /**
       * The stream where the Zip-compressed bytes will be written.
       */
      protected ByteArrayOutputStream m_ostream;

      /**
       * The Zip stream.
       */
      protected ZipOutputStream m_zipStream;


      // constructors

      /**
       * Constructs a new in-memory Zip file creator.
       */
      public ZipMaker()
      {
         m_ostream = new ByteArrayOutputStream();
         m_zipStream = new ZipOutputStream(m_ostream);
      }


      // operations

      /**
       * Adds a directory entry to the Zip file.
       * 
       * @param sName The name of the directory entry, using "/" as the separator.
       * @throws IOException If an I/O error occurs.
       */
      public void addDirectory(String sName) throws IOException
      {
         addFile(sName, null, null, null, 0, System.currentTimeMillis());
      }

      /**
       * Adds a file entry to the Zip file.
       * 
       * @param sName The name of the file entry, using "/" as the separator.
       * @param sContents The string to write with UTF-8 encoding.
       * @throws IOException If an I/O error occurs.
       */
      public void addFile(String sName, String sContents) throws IOException
      {
         addFile(sName, sContents.getBytes("UTF-8"));
      }

      /**
       * Adds a file entry to the Zip file.
       * 
       * @param sName The name of the file entry, using "/" as the separator.
       * @param nContentsArray The raw (uncompressed) data to write.
       * @throws IOException If an I/O error occurs.
       */
      public void addFile(String sName, byte[] nContentsArray) throws IOException
      {
         ByteArrayInputStream contentsStream = new ByteArrayInputStream(nContentsArray);

         addFile(sName, contentsStream, null, null, nContentsArray.length, System.currentTimeMillis());
      }

      /**
       * Adds a file entry to the Zip file.
       * 
       * @param sName The name of the file entry, using "/" as the separator.
       * @param contentsStream A stream containing the raw (uncompressed) data
       *                       to write.
       * @param sComment The comment to use for the entry.
       * @param nExtraArray Extra byte data to associate with the entry.
       * @param lSize The value to use for the file size.
       * @param lTime The value to use for the file last modification time.
       * @throws IOException If an I/O error occurs.
       */
      public void addFile(String sName, InputStream contentsStream, String sComment,
         byte[] nExtraArray, long lSize, long lTime) throws IOException
      {
         ZipEntry entry = new ZipEntry(sName);

         entry.setComment(sComment);
         entry.setExtra(nExtraArray);
         entry.setSize(lSize);
         entry.setTime(lTime);
         m_zipStream.putNextEntry(entry);

         if (contentsStream != null)
         {
            IOUtil.copy(m_zipStream, contentsStream);
         }

         m_zipStream.closeEntry();
      }

      /**
       * Gets the result of the ZipMaker as an input stream that sources
       * the binary data for the Zip file.
       * 
       * @return An input stream for getting the Zip file.
       * @throws IOException If an I/O error occurs.
       */
      public InputStream getInputStream() throws IOException
      {
         m_zipStream.close();
         m_zipStream = null;
         m_ostream.close();

         return new ByteArrayInputStream(m_ostream.toByteArray());
      }
   }
}
