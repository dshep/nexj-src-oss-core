package nexj.core.rpc.json;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;

import nexj.core.meta.PrivilegeSet;
import nexj.core.util.Base64Util;
import nexj.core.util.Binary;
import nexj.core.util.StringUtil;

/**
 * Filter writer with additional methods for writing JSON syntax.
 */
public class JSONWriter extends FilterWriter
{
   // associations

   /**
    * The working string builder.
    */
   protected StringBuilder m_buffer;

   // constructors

   /**
    * @see FilterWriter
    */
   public JSONWriter(Writer writer)
   {
      super(writer);
   }

   // operations

   /**
    * Writes JSON object opening token (an left brace).
    */
   public void openObject() throws IOException
   {
      write('{');
   }

   /**
    * Writes JSON object closing token (a right brace).
    */
   public void closeObject() throws IOException
   {
      write('}');
   }

   /**
    * Writes JSON array opening token (a left bracket).
    */
   public void openArray() throws IOException
   {
      write('[');
   }

   /**
    * Writes JSON array closing token (a right bracket).
    */
   public void closeArray() throws IOException
   {
      write(']');
   }

   /**
    * Writes a JSON separator (a comma).
    */
   public void writeSeparator() throws IOException
   {
      write(',');
   }

   /**
    * Writes JSON boolean value
    *
    * @param bValue boolean value to write.
    */
   public void writeBoolean(boolean bValue) throws IOException
   {
      write(Boolean.toString(bValue));
   }

   /**
    * Writes JSON null token.
    */
   public void writeNull() throws IOException
   {
      write("null");
   }

   /**
    * Writes char to character stream and escapes special characters.
    *
    * @param ch The character to write.
    */
   protected void writeEscapedChar(char ch) throws IOException
   {
      switch (ch)
      {
         case '"':
            write("\\\"");
            break;

         case '\\':
            write("\\\\");
            break;

         case '\b':
            write("\\b");
            break;

         case '\f':
            write("\\f");
            break;

         case '\n':
            write("\\n");
            break;

         case '\r':
            write("\\r");
            break;

         case '\t':
            write("\\t");
            break;

         default:
            if (ch >= ' ' && ch <= '~')
            {
               write(ch);
            }
            else
            {
               write("\\u");
               StringUtil.writeHex(this, ch);
            }
      }
   }

   /**
    * Writes a JSON number
    *
    * @param nValue Integer to write
    */
   public void writeNumber(Integer nValue) throws IOException
   {
      writeNumber(nValue.intValue());
   }

   /**
    * Writes a JSON number
    *
    * @param nValue Integer to write
    */
   public void writeNumber(int nValue) throws IOException
   {
      getBuffer().append(nValue);

      writeBuffer();
   }

   /**
    * Writes a JSON number
    *
    * @param fValue Float to write
    */
   public void writeNumber(Float fValue) throws IOException
   {
      getBuffer().append(fValue.floatValue());

      writeBuffer();
   }

   /**
    * Writes a JSON number
    *
    * @param lValue Long to write
    */
   public void writeNumber(Long lValue) throws IOException
   {
      writeNumber(lValue.longValue());
   }

   /**
    * Writes a JSON number
    *
    * @param lValue Long to write
    */
   public void writeNumber(long lValue) throws IOException
   {
      getBuffer().append(lValue);

      writeBuffer();
   }

   /**
    * Writes a JSON number
    *
    * @param dValue Double to write
    */
   public void writeNumber(Double dValue) throws IOException
   {
      getBuffer().append(dValue.doubleValue());

      writeBuffer();
   }

   /**
    * Writes a JSON number
    *
    * @param decValue BigDecimal to write
    */
   public void writeNumber(BigDecimal decValue) throws IOException
   {
      getBuffer().append(decValue.doubleValue());

      writeBuffer();
   }

   /**
    * Writes a JSON number
    *
    * @param shortValue Short to write
    */
   public void writeNumber(Short shortValue) throws IOException
   {
      getBuffer().append(shortValue.shortValue());

      writeBuffer();
   }

   /**
    * Writes a JSON string
    *
    * @param sValue String to write
    */
   public void writeString(String sValue) throws IOException
   {
      if (sValue == null)
      {
         writeNull();

         return;
      }

      write('"');

      for (int i = 0, nCount = sValue.length(); i < nCount; i++)
      {
         writeEscapedChar(sValue.charAt(i));
      }

      write('"');
   }

   /**
    * Writes a single character as a JSON string.
    *
    * @param ch The character to write.
    */
   public void writeString(char ch) throws IOException
   {
      write('"');
      writeEscapedChar(ch);
      write('"');
   }

   /**
    * Writes the Object key followed by :
    *
    * @param sKey The Key
    */
   public void writeObjectKey(String sKey) throws IOException
   {
      writeString(sKey);
      write(':');
   }

   /**
    * Writes a JSON object for the given key and value
    *
    * @param sKey The object's key
    * @param sValue String value
    */
   public void writeObject(String sKey, String sValue) throws IOException
   {
      openObject();
      writeObjectKeyValue(sKey, sValue);
      closeObject();
   }

   /**
    * Write a key and value
    * @param sKey The key to write
    * @param sValue The value to write
    */
   public void writeObjectKeyValue(String sKey, String sValue) throws IOException
   {
      writeObjectKey(sKey);
      writeString(sValue);      
   }

   /**
    * Write a key and value
    * @param sKey The key to write
    * @param bValue The value to write
    */
   public void writeObjectKeyValue(String sKey, boolean bValue) throws IOException
   {
      writeObjectKey(sKey);
      writeBoolean(bValue);      
   }
   
   /**
    * Write a key and value
    * @param sKey The key to write
    * @param nValue The value to write
    */
   public void writeObjectKeyValue(String sKey, int nValue) throws IOException
   {
      writeObjectKey(sKey);
      writeNumber(nValue);      
   }

   /**
    * Writes a JSON object for the given key and value
    *
    * @param sKey The object's key
    * @param ch Character value
    */
   public void writeObject(String sKey, char ch) throws IOException
   {
      openObject();
      writeObjectKey(sKey);
      writeString(ch);
      closeObject();
   }

   /**
    * Writes a JSON object for the given key and value
    *
    * @param sKey The object's key
    * @param privilegeSet PrivilegeSet value
    */
   public void writeObject(String sKey, PrivilegeSet privilegeSet) throws IOException
   {
      byte[] mask = privilegeSet.getMask();

      openObject();
      writeObjectKey(sKey);
      write('"');
      Binary.write(this, mask, 0, mask.length);
      write('"');
      closeObject();
   }

   /**
    * Writes a JSON object for the given key and value
    *
    * @param sKey The object's key
    * @param binary Binary value
    */
   public void writeObject(String sKey, Binary binary) throws IOException
   {
      openObject();
      writeObjectKey(sKey);
      writeBase64(binary);
      closeObject();
   }

   /**
    * Writes the Binary value encoded in base 64
    *
    * @param binary Binary value
    */
   public void writeBase64(Binary binary) throws IOException
   {
      write('"');
      Base64Util.encode(binary.getInputStream(), this, -1, false);
      write('"');
   }

   /**
    * Writes the work buffer contents to the output stream.
    */
   protected void writeBuffer() throws IOException
   {
      int n = m_buffer.length();

      for (int i = 0; i < n; ++i)
      {
         write(m_buffer.charAt(i));
      }

      m_buffer.setLength(0);
   }

   /**
    * @return The work buffer
    */
   protected StringBuilder getBuffer()
   {
      if (m_buffer == null)
      {
         m_buffer = new StringBuilder(32);
      }

      return m_buffer;
   }
}