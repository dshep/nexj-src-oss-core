// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

/**
 * Filter writer with additional methods for writing escaped values.
 */
public class XMLWriter extends FilterWriter
{
   /**
    * Hex digits used in URL encoding.
    */
   private final static char[] HEX_DIGITS = "0123456789ABCDEF".toCharArray();

   /**
    * Set of ASCII chars skipped during URL encoding.
    */
   private final static int[] s_urlUnencodedBitArray = new int[8];

   static
   {
      int ch;

      for (ch = '0'; ch <= '9'; ++ch)
      {
         setURLUnencoded(ch);
      }

      for (ch = 'A'; ch <= 'Z'; ++ch)
      {
         setURLUnencoded(ch);
      }

      for (ch = 'a'; ch <= 'z'; ++ch)
      {
         setURLUnencoded(ch);
      }

      setURLUnencoded('-');
      setURLUnencoded('_');
      setURLUnencoded('.');
      setURLUnencoded('*');
   }

   // attributes
   
   /**
    * The current namespace.
    */
   protected String m_sNamespace;
   
   /**
    * The integer conversion buffer.
    */
   protected char[] m_cbuf;

   // constructors
   
   /**
    * @see FilterWriter
    */
   public XMLWriter(Writer writer)
   {
      super(writer);
   }

   // operations

   /**
    * Sets the current namespace.
    * @param sNamespace The current namespace to set.
    */
   public void setNamespace(String sNamespace)
   {
      if (sNamespace != null && sNamespace.length() == 0)
      {
         sNamespace = null;
      }

      m_sNamespace = sNamespace;
   }

   /**
    * @return The current namespace.
    */
   public String getNamespace()
   {
      return m_sNamespace;
   }

   /**
    * Writes the current namespace prefix. 
    */
   protected void writeNamespace() throws IOException
   {
      if (m_sNamespace != null)
      {
         write(m_sNamespace);
         write(':');
      }
   }
   
   /**
    * Writes an escaped character, according to the XML spec.
    * Surrogate block characters 0xD800-0xDFFF must be accepted since this function takes both characters and codepoints.
    * Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
    * from http://www.w3.org/TR/xml/#charsets
    * @param ch The character to write.
    */
   public void writeEscaped(int ch) throws IOException
   {
      switch (ch)
      {
         case 0:
         case 1:
         case 2:
         case 3:
         case 4:
         case 5:
         case 6:
         case 7:
         case 8:
         case 11:
         case 12:
         case 14:
         case 15:
         case 16:
         case 17:
         case 18:
         case 19:
         case 20:
         case 21:
         case 22:
         case 23:
         case 24:
         case 25:
         case 26:
         case 27:
         case 28:
         case 29:
         case 30:
         case 31:
            write(' ');
            break;

         case '\r':
            write("&#13;");
            break;

         case '\n':
            write("&#10;");
            break;

         case '\t':
            write("&#09;");
            break;

         case '<':
            write("&lt;");
            break;

         case '>':
            write("&gt;");
            break;

         case '&':
            write("&amp;");
            break;

         case '"':
            write("&quot;");
            break;

         default:
            if (ch <= 0xfffd)
            {
               write(ch);
            }

            break;
      }
   }

   /**
    * Prints a decimal string representation of an integer.
    * @param n The integer to print.
    */
   public void writeInt(int n) throws IOException
   {
      if (m_cbuf == null)
      {
         m_cbuf = new char[10];
      }

      if (n < 0)
      {
         n = -n;
         
         if (n < 0)
         {
            write("-2147483648");
            return;
         }
         
         write('-');
      }

      int i = 0;

      do
      {
         m_cbuf[i++] = (char)('0' + n % 10);
         n /= 10;
      }
      while (n != 0);

      do
      {
         write(m_cbuf[--i]);
      }
      while (i != 0);
   }

   /**
    * Prints a boolean value. 
    * @param bValue The value to print.
    */
   public void writeBoolean(boolean bValue) throws IOException
   {
      write((bValue) ? "true" : "false");
   }
   
   /**
    * Prints an empty XML element.
    * @param sName The element name.
    */
   public void writeElement(String sName) throws IOException
   {
      openElement(sName);
      closeEmptyElement();
   }
   
   /**
    * Prints an XML element with a simple value.
    * @param sName The element name.
    * @param sValue The element value.
    */
   public void writeElement(String sName, CharSequence sValue) throws IOException
   {
      startElement(sName);
      writeValue(sValue);
      endElement(sName);
   }

   /**
    * Prints an XML element with a concatenated value.
    * @param sName The element name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    */
   public void writeElement(String sName, CharSequence s1, CharSequence s2) throws IOException
   {
      startElement(sName);
      writeValue(s1);
      writeValue(s2);
      endElement(sName);
   }

   /**
    * Prints an XML element with a concatenated value.
    * @param sName The element name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    */
   public void writeElement(String sName, CharSequence s1, CharSequence s2, CharSequence s3) throws IOException
   {
      startElement(sName);
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      endElement(sName);
   }

   /**
    * Prints an XML element with a concatenated value.
    * @param sName The element name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    */
   public void writeElement(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4) throws IOException
   {
      startElement(sName);
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      endElement(sName);
   }

   /**
    * Prints an XML element with a concatenated value.
    * @param sName The element name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    * @param s5 The fifth part of the value.
    */
   public void writeElement(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4, CharSequence s5) throws IOException
   {
      startElement(sName);
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      writeValue(s5);
      endElement(sName);
   }

   /**
    * Prints an XML element with a concatenated value.
    * @param sName The element name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    * @param s5 The fifth part of the value.
    * @param s6 The sixth part of the value.
    */
   public void writeElement(String sName, CharSequence s1, CharSequence s2, CharSequence s3,
      CharSequence s4, CharSequence s5, CharSequence s6) throws IOException
   {
      startElement(sName);
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      writeValue(s5);
      writeValue(s6);
      endElement(sName);
   }

   /**
    * Prints an XML element with a concatenated value.
    * @param sName The element name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    * @param s5 The fifth part of the value.
    * @param s6 The sixth part of the value.
    * @param s7 The sixth part of the value.
    */
   public void writeElement(String sName, CharSequence s1, CharSequence s2, CharSequence s3,
      CharSequence s4, CharSequence s5, CharSequence s6, CharSequence s7) throws IOException
   {
      startElement(sName);
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      writeValue(s5);
      writeValue(s6);
      writeValue(s7);
      endElement(sName);
   }
   
   /**
    * Opens an Attribute.
    * @param sName The attribute name.
    */
   public void openAttribute(String sName) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
   }

   /**
    * Closes an Attribute opened with openAttribute.
    */
   public void closeAttribute() throws IOException
   {
      write('"');
   }

   /**
    * Prints an XML attribute name and value.
    * @param sName The attribute name.
    * @param sValue The attribute value.
    */
   public void writeAttribute(String sName, CharSequence sValue) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(sValue);
      write('"');
   }

   /**
    * Prints an XML attribute name and value. If the named object or its name is null,
    * then no attribute is output.
    * @param sName The attribute name.
    * @param named The named object.
    */
   public void writeAttribute(String sName, Named named) throws IOException
   {
      if (named != null && named.getName() != null)
      {
         writeAttribute(sName, named.getName());
      }
   }

   /**
    * Prints an XML attribute name and value.
    * @param sName The attribute name.
    * @param nValue The attribute value.
    */
   public void writeAttribute(String sName, int nValue) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeInt(nValue);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2, CharSequence s3) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1,
      CharSequence s2, CharSequence s3, CharSequence s4) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param n The integer part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1,
      CharSequence s2, CharSequence s3, int n) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeInt(n);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    * @param s5 The fifth part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1,
      CharSequence s2, CharSequence s3, CharSequence s4, CharSequence s5) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      writeValue(s5);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    * @param s5 The fifth part of the value.
    * @param s6 The sixth part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1,
      CharSequence s2, CharSequence s3, CharSequence s4,
      CharSequence s5, CharSequence s6) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      writeValue(s5);
      writeValue(s6);
      write('"');
   }

   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param s1 The first part of the value.
    * @param s2 The second part of the value.
    * @param s3 The third part of the value.
    * @param s4 The fourth part of the value.
    * @param s5 The fifth part of the value.
    * @param s6 The sixth part of the value.
    * @param s7 The seventh part of the value.
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4, CharSequence s5, CharSequence s6,
      CharSequence s7) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeValue(s1);
      writeValue(s2);
      writeValue(s3);
      writeValue(s4);
      writeValue(s5);
      writeValue(s6);
      writeValue(s7);
      write('"');
   }
   
   /**
    * Prints an XML attribute name and a concatenated value.
    * @param sName The attribute name.
    * @param n The integer part of value.
    * @param s The string part of value.
    */
   public void writeAttribute(String sName, int n, CharSequence s) throws IOException
   {
      write(' ');
      writeNamespace();
      write(sName);
      write('=');
      write('"');
      writeInt(n);
      writeValue(s);
      write('"');
   }

   /**
    * Prints an XML boolean attribute name and value.
    * @param sName The attribute name.
    * @param bValue The attribute value.
    */
   public void writeAttribute(String sName, boolean bValue) throws IOException
   {
      writeAttribute(sName, (bValue) ? "true" : "false");
   }

   /**
    * Prints an element value as CDATA or as escaped characters.
    * @param sValue The element value.
    */
   public void writeCDATA(String sValue) throws IOException
   {
      if (sValue == null)
      {
         return;
      }
      
      if (sValue.indexOf("]]>") >= 0)
      {
         int nCount = sValue.length();
         
         for (int i = 0; i < nCount; ++i)
         {
            writeEscaped(sValue.charAt(i));
         }
      }
      else
      {
         write("<![CDATA[");
         write(sValue);
         write("]]>");
      }
   }

   /**
    * Prints an element value.
    * @param sValue The element value.
    */
   public void writeValue(CharSequence sValue) throws IOException
   {
      if (sValue == null)
      {
         return;
      }

      int nCount = sValue.length();

      for (int i = 0; i < nCount; ++i)
      {
         writeEscaped(sValue.charAt(i));
      }
   }

   /**
    * Prints an element value.
    * @param cbuf Buffer of characters to be written.
    * @param nOffset Offset from which to start reading characters.
    * @param nLength Number of characters to be written.
    * @throws IOException If an I/O error occurs.
    */
   public void writeValue(char[] cbuf, int nOffset, int nLength) throws IOException
   {
      for (int i = nOffset, nEnd = nOffset + nLength; i < nEnd; ++i)
      {
         writeEscaped(cbuf[i]);
      }
   }

   /**
    * Prints an element value and replaces spaces
    * with &amp;nbsp; character.
    * @param sValue The element value.
    */
   public void writeValueNBSP(CharSequence sValue) throws IOException
   {
      if (sValue == null)
      {
         return;
      }

      int nCount = sValue.length();

      for (int i = 0; i < nCount; ++i)
      {
         char c = sValue.charAt(i);
         
         if (c == ' ')
         {
            write("&nbsp;");
         }
         else
         {
            writeEscaped(c);
         }
      }
   }

   /**
    * Prints an element value and replace HTML specific characters.
    * 
    * @param sValue The element value.
    */
   public void writeValueHTML(CharSequence sValue) throws IOException
   {
      if (sValue == null)
      {
         return;
      }

      int nCount = sValue.length();

      for (int i = 0; i < nCount; ++i)
      {
         char c = sValue.charAt(i);
         
         if (c == '\n')
         {
            write("<br/>");
         }
         else
         {
            writeEscaped(c);
         }
      }
   }

   /**
    * Opens and closes an element.
    */
   public void startElement(String sName) throws IOException
   {
      openElement(sName);
      closeElement();
   }
   
   /**
    * Opens an element.
    * @param sName The element name.
    */
   public void openElement(String sName) throws IOException
   {
      write('<');
      writeNamespace();
      write(sName);
   }

   /**
    * Closes an element.
    */
   public void closeElement() throws IOException
   {
      write('>');
   }
   
   /**
    * Closes an empty element.
    */
   public void closeEmptyElement() throws IOException
   {
      write('/');
      write('>');
   }
   
   /**
    * Prints an end-tag.
    * @param sName The element name.
    */
   public void endElement(String sName) throws IOException
   {
      write('<');
      write('/');
      writeNamespace();
      write(sName);
      write('>');
   }

   /**
    * Prints a hexadecimal RGB value.
    * @param nRBG The integer to print.
    */
   public void writeRGB(int nRBG) throws IOException
   {
      write(HEX_DIGITS[nRBG >> 20 & 0x0f]);               
      write(HEX_DIGITS[nRBG >> 16 & 0x0f]);
      write(HEX_DIGITS[nRBG >> 12 & 0x0f]);               
      write(HEX_DIGITS[nRBG >> 8 & 0x0f]);
      write(HEX_DIGITS[nRBG >> 4 & 0x0f]);               
      write(HEX_DIGITS[nRBG & 0x0f]);
   }

   /**
    * Prints URL-encoded string.
    * @param s The string to encode.
    */
   public void writeURL(CharSequence s) throws IOException
   {
      if (s == null)
      {
         return;
      }

      for (int i = 0, nCount = s.length(); i < nCount; ++i)
      {
         int ch = s.charAt(i);

         if (ch >= 0xD800 && ch <= 0xDBFF && i + 1 < nCount)
         {
            // Unicode surrogate
            // four bytes: 11110www 10xxxxxx 10yyyyyy 10zzzzzz
            // represented as: 110110wwwwzzzzyy, 110111yyyyxxxxxx

            writeURL(((ch - Character.MIN_HIGH_SURROGATE) << 10) +
               (s.charAt(++i) - Character.MIN_LOW_SURROGATE) +
               Character.MIN_SUPPLEMENTARY_CODE_POINT);
         }
         else
         {
            writeURL(ch);
         }
      }
   }

   /**
    * Prints a UTF-8 URL-encoded character.
    * @param ch The character to encode. Can be UTF-32. 
    */
   public void writeURL(int ch) throws IOException
   {
      if (ch <= 0x7f)
      {
         // one byte

         if ((s_urlUnencodedBitArray[ch >> 5] & 1 << (ch & 0x1f)) != 0)
         {
            write(ch);
         }
         else
         {
            writeURLHex(ch);
         }
      }
      else if (ch <= 0x7ff)
      {
         // two bytes: 110yyyyy 10zzzzzz
         writeURLHex((ch >> 6) | 0xc0);
         writeURLHex((ch & 0x3f) | 0x80);
      }
      else if (ch <= 0xFFFF)
      {
         // three bytes: 1110xxx 10yyyyyy 10zzzzzz
         writeURLHex((ch >> 12) | 0xe0);
         writeURLHex(((ch & 0xfc0) >> 6) | 0x80);
         writeURLHex((ch & 0x3f) | 0x80);
      }
      else
      {
         writeURLHex((ch >> 18) | 0xf0);
         writeURLHex(((ch & 0x3f000) >> 12) | 0x80);
         writeURLHex(((ch & 0xfc0) >> 6) | 0x80);
         writeURLHex((ch & 0x3f) | 0x80);
      }
   }

   /**
    * Writes a URL hex byte.
    * @param n The byte to write.
    */
   private void writeURLHex(int n) throws IOException
   {
      write('%');

      write(HEX_DIGITS[n >> 4 & 0x0f]);               
      write(HEX_DIGITS[n & 0x0f]);
   }

   /**
    * Specifies an ASCII character that should not be URL-encoded.
    * @param ch The character.
    */
   private static void setURLUnencoded(int ch)
   {
      s_urlUnencodedBitArray[ch >> 5] |= 1 << (ch & 0x1f);
   }

   /**
    * Prints a JavaScript-encoded string.
    * @param s The string to encode.
    */
   public void writeJSString(CharSequence s) throws IOException
   {
      write('\'');
      
      if (s != null)
      {
         for (int i = 0, c = s.length(); i < c; i++)
         {
            int ch = s.charAt(i);
            
            switch (ch)
            {
               case '\r':
                  write("\\r");
                  break;

               case '\n':
                  write("\\n");
                  break;

               case '\t':
                  write("\\t");
                  break;

               case '\\':
                  write("\\\\");
                  break;

               case '\'':
                  write("\\'");
                  break;

               case '"':
                  write("\\x22");
                  break;

               case '&':
                  write("\\x26");
                  break;

               default:
                  write(ch);
                  break;
            }
         }
      }
      
      write('\'');
   }
}
