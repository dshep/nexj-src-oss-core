package nexj.core.integration.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import nexj.core.integration.IntegrationException;
import nexj.core.util.IOUtil;

import junit.framework.TestCase;

/**
 * Tests the DebugInput class.
 */
public class DebugInputTest extends TestCase
{
   // constants

   protected static String s_testString = "abcdefghijklmnopqrstuvwxyz0123456789";

   protected static byte[] s_testBytes = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x10,
      0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x20,
      0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x30,
      0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x40 };

   // operations

   public void testReaderComplete() throws IntegrationException, IOException
   {
      int prefixSize = 20;
      StringInput input = new StringInput(s_testString);
      DebugInput debugInput = new DebugInput(input, prefixSize);
      ObjectOutput output = new ObjectOutput();

      IOUtil.copy(output.getWriter(), debugInput.getReader());

      assertEquals(s_testString.substring(0, prefixSize), debugInput.getPrefix());
   }

   public void testInputStreamComplete() throws IntegrationException, IOException
   {
      int prefixSize = 20;
      StreamInput input = new StreamInput(new ByteArrayInputStream(s_testBytes));
      DebugInput debugInput = new DebugInput(input, prefixSize);
      ByteArrayOutputStream os = new ByteArrayOutputStream();

      IOUtil.copy(os, debugInput.getInputStream());

      assertEquals(new String(s_testBytes, 0, prefixSize), debugInput.getPrefix());
   }

   public void testReaderIncomplete1() throws IntegrationException, IOException
   {
      int prefixSize = 20;
      StringInput input = new StringInput(s_testString);
      DebugInput debugInput = new DebugInput(input, prefixSize);

      debugInput.getReader().read();
      debugInput.getReader().read();

      assertEquals(s_testString.substring(0, prefixSize), debugInput.getPrefix());
   }

   public void testInputStreamIncomplete1() throws IntegrationException, IOException
   {
      int prefixSize = 20;
      StreamInput input = new StreamInput(new ByteArrayInputStream(s_testBytes));
      DebugInput debugInput = new DebugInput(input, prefixSize);

      debugInput.getInputStream().read();
      debugInput.getInputStream().read();

      assertEquals(new String(s_testBytes, 0, prefixSize), debugInput.getPrefix());
   }

   public void testReaderIncomplete2() throws IntegrationException, IOException
   {
      int prefixSize = 20;
      StringInput input = new StringInput(s_testString);
      DebugInput debugInput = new DebugInput(input, prefixSize);
      char[] chars = new char[prefixSize];

      debugInput.getReader().read(chars, 0, 5);
      debugInput.getReader().read(chars, 0, 5);

      assertEquals(s_testString.substring(0, prefixSize), debugInput.getPrefix());
   }

   public void testInputStreamIncomplete2() throws IntegrationException, IOException
   {
      int prefixSize = 20;
      StreamInput input = new StreamInput(new ByteArrayInputStream(s_testBytes));
      DebugInput debugInput = new DebugInput(input, prefixSize);
      byte[] bytes = new byte[prefixSize];

      debugInput.getInputStream().read(bytes, 0, 5);
      debugInput.getInputStream().read(bytes, 0, 5);

      assertEquals(new String(s_testBytes, 0, prefixSize), debugInput.getPrefix());
   }
}
