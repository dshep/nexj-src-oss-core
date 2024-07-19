// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.zip;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import nexj.core.integration.MessageFormatter;
import nexj.core.integration.io.StreamOutput;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Binary;

import junit.framework.TestCase;

/**
 * Tests the Zip format message formatter.
 */
public class ZipMessageFormatterTest extends TestCase
{
   // attributes

   protected MessageFormatter m_formatter;

   protected Format m_format;

   protected Message m_message;


   // operations

   protected void setUp() throws Exception
   {
      m_format = Repository.getMetadata().getFormat("Zip");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(null);
      m_message = Repository.getMetadata().getMessage("Zip_AllMapped");
   }

   public void testFormat() throws Exception
   {
      TransferObject root = new TransferObject("Zip_AllMapped", 1);
      List entryList = new ArrayList(4);
      TransferObject entry;

      root.setValue("files", entryList);

      entry = new TransferObject(2);
      entry.setValue("fileName", "cat.txt");
      entry.setValue("data", new Binary("This is what should go in CAT.".getBytes("UTF-8")));
      entryList.add(entry);

      entry = new TransferObject(1);
      entry.setValue("fileName", "toy/");
      entryList.add(entry);

      entry = new TransferObject(2);
      entry.setValue("data", new Binary("This file has no name.".getBytes("UTF-8")));
      entry.setValue("comments", "This entry will not be written.");
      entryList.add(entry);

      entry = new TransferObject(3);
      entry.setValue("fileName", "toy/ball.txt");
      entry.setValue("data", new Binary("This is the cat's ball-toy.".getBytes("UTF-8")));
      entry.setValue("comments", "A toy");
      entryList.add(entry);

      // Format and check result
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();

      m_formatter.format(root, m_message, new StreamOutput(ostream));
      ostream.close();

      ByteArrayInputStream istream = new ByteArrayInputStream(ostream.toByteArray());
      ZipInputStream zipStream = new ZipInputStream(istream);
      ZipEntry zipEntry;
      StringWriter contentsWriter;

      zipEntry = zipStream.getNextEntry();
      assertEquals("cat.txt", zipEntry.getName());
      contentsWriter = new StringWriter();
      copy(contentsWriter, zipStream, 1024, "UTF-8");
      assertEquals("This is what should go in CAT.", contentsWriter.toString());

      zipEntry = zipStream.getNextEntry();
      assertEquals("toy/", zipEntry.getName());
      assertTrue(zipEntry.isDirectory());
      assertEquals(-1, zipStream.read());

      zipEntry = zipStream.getNextEntry();
      assertEquals("toy/ball.txt", zipEntry.getName());
      contentsWriter = new StringWriter();
      copy(contentsWriter, zipStream, 1024, "UTF-8");
      assertEquals("This is the cat's ball-toy.", contentsWriter.toString());

      assertNull(zipStream.getNextEntry());
      zipStream.close();
      istream.close();
   }

   /**
    * Copies the contents of the input stream to the output character stream.
    * 
    * @param writer    The output character stream.
    * @param istream   The input byte stream.
    * @param nBufSize  The buffer size to use.
    * @param sEncoding The name of the encoding to use to decode the bytes.
    * @throws IOException
    */
   public static void copy(Writer writer, InputStream istream, int nBufSize, String sEncoding) throws IOException
   {
      Charset cs = Charset.forName(sEncoding);
      CharsetDecoder decoder = cs.newDecoder();

      decoder.onMalformedInput(CodingErrorAction.REPLACE);
      decoder.onUnmappableCharacter(CodingErrorAction.REPLACE);

      //Set up buffers
      byte[] bytes = new byte[nBufSize];
      ByteBuffer bytesIn = ByteBuffer.wrap(bytes);
      char[] chars = new char[nBufSize];
      CharBuffer charsOut = CharBuffer.wrap(chars);


      //Decode
      int nBytesRead = 0;

      while (nBytesRead > -1)
      {
         nBytesRead = istream.read(bytes, bytesIn.position(), bytesIn.limit() - bytesIn.position());

         if (nBytesRead < 0)
         {
            break;
         }

         bytesIn.position(nBytesRead);

         //Decode
         bytesIn.flip();
         decoder.decode(bytesIn, charsOut, false);

         //Append only characters that were decoded
         charsOut.flip();
         writer.write(chars, 0, charsOut.limit());

         //Reset buffers. Input buffer must save unprocessed bytes.
         charsOut.clear();
         bytesIn.compact();
      }

      //Final decoding
      bytesIn.flip();
      decoder.decode(bytesIn, charsOut, true);
      decoder.flush(charsOut);

      //Append characters produced by decoder dumping internal state
      charsOut.flip();
      writer.write(chars, 0, charsOut.limit());
   }
}
