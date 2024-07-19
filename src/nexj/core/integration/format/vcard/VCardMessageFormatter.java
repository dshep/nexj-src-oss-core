// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.vcard;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.vcard.VCardMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Base64Util;
import nexj.core.util.Binary;
import nexj.core.util.QuotedPrintableUtil;
import nexj.core.util.ISO8601Util;
import nexj.core.util.StringUtil;

/**
 * vCard (and related formats) message formatter.
 */
public class VCardMessageFormatter implements MessageFormatter
{
   // attributes

   /**
    * True if commas and semicolons should be escaped in text fields.
    */
   protected boolean m_bTextEscaped = false;

   /**
    * The default line wrapping mode.
    */
   protected byte m_nDefaultWrappingMode;


   // associations

   /**
    * The writer.
    */
   protected WrappingWriter m_writer;

   // operations

   /**
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      CompositeMessagePart msgRoot = message.getRoot();

      m_writer = new WrappingWriter(out.getWriter());

      try
      {
         switch (((VCardMessagePartMapping)msgRoot.getMapping()).getWrapping())
         {
            case VCardMessagePartMapping.WRAPPING_WHITESPACE:
               m_nDefaultWrappingMode = WrappingWriter.WRAP_REPLACE_WHITESPACE;
               m_writer.setMaxLineLength(72);
               break;

            case VCardMessagePartMapping.WRAPPING_ANYWHERE:
               m_nDefaultWrappingMode = WrappingWriter.WRAP_ANYWHERE;
               m_writer.setMaxLineLength(73);   // 75 chars per line, including CRLF
               break;
         }

         m_writer.setWrappingMode(m_nDefaultWrappingMode);
         format(tobj, msgRoot);
      }
      catch (IOException ex)
      {
         throw new IntegrationException("err.integration.format",
            new Object[]{message.getName()}, ex);
      }
   }

   /**
    * Formats a transfer object for a given composite message part.
    * 
    * @param tobj The transfer object containing the data to format.
    * @param part The composite message part to format.
    * @throws IOException If an I/O error occurs.
    */
   protected void format(TransferObject tobj, CompositeMessagePart part) throws IOException
   {
      if (tobj == null)
      {
         if (part.isRequired())
         {
            throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
         }

         return;
      }

      VCardMessagePartMapping mapping = (VCardMessagePartMapping)part.getMapping();

      if (part instanceof CompositeMessagePartRef)
      {
         mapping = (VCardMessagePartMapping)((CompositeMessagePartRef)part).getRefPart().getMapping();
      }

      if (mapping.getType() == VCardMessagePartMapping.TYPE_GROUP)
      {
         for (int i = 0, nChildCount = part.getPartCount(); i < nChildCount; i++)
         {
            MessagePart childPart = part.getPart(i);
            Object childObj = tobj.findValue(childPart.getName());

            if (childPart instanceof CompositeMessagePart)
            {
               if (childPart.isCollection())
               {
                  // composite sub-part of group, and it is a collection (multiplicity * group or field w/ parameters)
                  List list = (List)childObj;
                  int nCount = (list != null) ? list.size() : 0;

                  checkMultiplicity(nCount, childPart);

                  for (int k = 0; k < nCount; k++)
                  {
                     format((TransferObject)list.get(k), (CompositeMessagePart)childPart);
                  }
               }
               else
               {
                  // composite sub-part of group (could be another group, a field with parameters, etc.)
                  format((TransferObject)childObj, (CompositeMessagePart)childPart);
               }
            }
            else
            {
               // primitive parts in the group
               writeField(childObj, childPart, (VCardMessagePartMapping)childPart.getMapping());
            }
         }
      }
      else
      {
         // a value-mapped part, composite
         writeField(tobj, part, mapping);
      }
   }

   /**
    * Writes a single vCard field, of the form:
    *    [fieldName];[param1]=[p1val1],[p1val2];[param2]=[p2val]:[value1][delimiter][value2]
    * 
    * @param obj The object containing the data to format.
    * @param part The message part to format. If it is a composite message part, then parameters
    * will be written. If it is a primitive message part, then a simple name/value pair
    * will be written.
    * @param mapping The vCard mapping of the part.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeField(Object obj, MessagePart part, VCardMessagePartMapping mapping) throws IOException
   {
      boolean bParameters = (part instanceof CompositeMessagePart);
      CompositeMessagePart parentPart = null;
      MessagePart valuePart;
      Object valueObj;

      if (bParameters)
      {
         valuePart = mapping.getValuePart();
         valueObj = ((TransferObject)obj).findValue(valuePart.getName());
         parentPart = (CompositeMessagePart)part;
      }
      else
      {
         valuePart = part;
         valueObj = obj;
      }

      VCardMessagePartMapping valuePartMapping = (VCardMessagePartMapping)valuePart.getMapping();

      if (valueObj == null && valuePartMapping.getDefault() == null)
      {
         if (valuePart.isRequired())
         {
            throw new IntegrationException("err.integration.minPartCount", new Object[]{valuePart.getFullPath()});
         }

         return;
      }

      m_writer.write((bParameters) ? mapping.getFullName() : valuePartMapping.getFullName());

      int nChildCount = (bParameters) ? parentPart.getPartCount() : 0;

      for (int i = 0; i < nChildCount; i++)
      {
         MessagePart childPart = (MessagePart)parentPart.getPart(i);
         VCardMessagePartMapping childMapping = (VCardMessagePartMapping)childPart.getMapping();

         if (childMapping.getType() == VCardMessagePartMapping.TYPE_PARAMETER)
         {
            Object parameterObj = ((TransferObject)obj).findValue(childPart.getName());

            if (parameterObj != null || childMapping.getDefault() != null)
            {
               m_writer.write(';');
               m_writer.write(childMapping.getName());
               m_writer.write('=');
               writeValue((PrimitiveMessagePart)childPart, childMapping, parameterObj);
            }
            else
            {
               if (childPart.isRequired())
               {
                  throw new IntegrationException("err.integration.minPartCount", new Object[]{childPart.getFullPath()});
               }
            }
         }
      }

      m_writer.write(':');

      if (valuePart instanceof PrimitiveMessagePart)
      {
         writeValue((PrimitiveMessagePart)valuePart, valuePartMapping, valueObj);
         m_writer.writeLineBreak();
      }
      else
      {
         // agent
         format((TransferObject)valueObj, (CompositeMessagePart)valuePart);
      }
   }


   /**
    * Writes a value. If the value is a list, then write it as a list of delimited strings.
    * Otherwise, the value is assumed to be a primitive that will be quoted according to
    * one of the vCard quoting mechanisms before it is written out.
    * 
    * @param part The primitive part to format.
    * @param mapping The vCard mapping of the part.
    * @param obj The object containing the data to format.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeValue(PrimitiveMessagePart part, VCardMessagePartMapping mapping, Object obj) throws IOException
   {
      boolean bCollection = (obj instanceof List);

      if (bCollection)
      {
         List list = (List)obj;
         int nCount = list.size();

         checkMultiplicity(nCount, part);

         StringWriter collectionWriter = new StringWriter();

         for (int i = 0; i < nCount; i++)
         {
            if (i != 0)
            {
               collectionWriter.write(mapping.getDelimiter());
            }

            Object item = part.convertValue(list.get(i));
            String sField = (item instanceof String) ? (String)item : toString(part, item);

            writeEscaped(collectionWriter, sField, String.valueOf(mapping.getDelimiter()), '\\');
         }

         obj = collectionWriter.toString();
      }
      else if (obj == null)
      {
         obj = mapping.getDefault();
      }
      else
      {
         obj = part.convertValue(obj);
      }

      if (mapping.getName().equals("VERSION"))
      {
         m_bTextEscaped = StringUtil.compareVersions((String)obj, "3.0") >= 0;
      }

      switch (mapping.getQuoting())
      {
         case VCardMessagePartMapping.QUOTING_QP:

            if (!(obj instanceof String))
            {
               obj = toString(part, obj);
            }

            if (m_bTextEscaped)
            {
               StringWriter writer = new StringWriter();

               writeEscaped(writer, (String)obj, ",;", '\\');
               obj = writer.toString();
            }

            m_writer.setWrappingMode(WrappingWriter.WRAP_NONE);
            m_writer.setCurrentLineLength(QuotedPrintableUtil.encode(
               new ByteArrayInputStream(((String)obj).getBytes(mapping.getEncoding())),
               m_writer, m_writer.getCurrentLineLength()));
            m_writer.setWrappingMode(m_nDefaultWrappingMode);
            break;

         case VCardMessagePartMapping.QUOTING_NONE:

            if (!(obj instanceof String))
            {
               obj = toString(part, obj);
            }

            if (bCollection)
            {
               m_writer.write((String)obj);
            }
            else
            {
               writeEscaped(m_writer, (String)obj, ";:", '\\');
            }

            break;


         case VCardMessagePartMapping.QUOTING_BASE64:

            m_writer.setWrappingMode(WrappingWriter.WRAP_ANYWHERE);

            if (obj instanceof Binary)
            {
               Base64Util.encode(((Binary)obj).getInputStream(), m_writer, -1, false);
            }
            else
            {
               obj = toString(part, obj);

               if (m_bTextEscaped)
               {
                  StringWriter writer = new StringWriter();

                  writeEscaped(writer, (String)obj, ",;", '\\');
                  obj = writer.toString();
               }

               Base64Util.encode(new ByteArrayInputStream(
                  ((String)obj).getBytes(mapping.getEncoding())),
                  m_writer, -1, false);
            }

            m_writer.setWrappingMode(m_nDefaultWrappingMode);
            m_writer.writeLineBreak();

            break;


         case VCardMessagePartMapping.QUOTING_VCARD:

            String sField = (obj instanceof String) ? (String)obj : toString(part, obj);
            int i = 0;
            int nLength = sField.length();

            while (i < nLength)
            {
               char ch = sField.charAt(i);

               switch (ch)
               {
                  case ',':
                  case ';':
                  case ':':
                  case '\\':
                     m_writer.write('\\');
                     m_writer.write(ch);
                     break;

                  case '\n':
                     m_writer.write("\\n");
                     break;

                  case '\r':
                     if (sField.charAt(i + 1) == '\n')
                     {
                        m_writer.write("\\n");
                     }

                     i++;
                     break;

                  default:
                     m_writer.write(ch);
                     break;
               }

               i++;
            }

            break;
      }
   }

   /**
    * Converts a value to string.
    * 
    * @param part The primitive message part.
    * @param value The value to convert.
    * @return The string representation of the value.
    */
   protected static String toString(PrimitiveMessagePart part, Object value)
   {
      switch (((VCardMessagePartMapping)part.getMapping()).getSubtype())
      {
         case VCardMessagePartMapping.SUBTYPE_DATE:
            return ISO8601Util.formatDate(Primitive.toTimestamp(value));

         case VCardMessagePartMapping.SUBTYPE_DATETIME:
            return ISO8601Util.formatDateTime(Primitive.toTimestamp(value));

         default:
            return Primitive.toString(value);
      }
   }

   /**
    * Writes out a string, escaping special characters.
    * 
    * @param writer The writer to write to.
    * @param sValue The string to write.
    * @param sToEscape Characters in the string that should be escaped.
    * @param chEscape The escape character to use.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeEscaped(Writer writer, String sValue, String sToEscape, char chEscape) throws IOException
   {
      int nLength = (sValue == null) ? 0 : sValue.length();

      for (int i = 0; i < nLength; i++)
      {
         char ch = sValue.charAt(i);

         if (ch == chEscape || sToEscape.indexOf(ch) >= 0)
         {
            writer.write(chEscape);
         }

         writer.write(ch);
      }
   }


   /**
    * Checks that a part appears with the correct multiplicity.
    * 
    * @param nCount The actual number of occurrences in the TransferObject.
    * @param part The part to check.
    * @throws IntegrationException If the multiplicity is incorrect.
    */
   protected static void checkMultiplicity(int nCount, MessagePart part)
   {
      if (nCount < part.getMinCount())
      {
         throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
      }

      if (nCount > part.getMaxCount())
      {
         throw new IntegrationException("err.integration.maxPartCount", new Object[]{part.getFullPath()});
      }
   }


   // inner classes

   /**
    * A writer that inserts newline sequences in text to prevent lines from getting too long.
    */
   public static class WrappingWriter extends Writer
   {
      // constants

      /**
       * The highest value to which the maximum line length may be set.
       */
      public final static int MAX_MAX_LINE_LENGTH = 80;

      /**
       * Wrap lines only at whitespace, replacing the whitespace
       * character with the newline sequence.
       */
      public final static byte WRAP_REPLACE_WHITESPACE = 0;

      /**
       * Wrap lines at any location.
       */
      public final static byte WRAP_ANYWHERE = 1;

      /**
       * Perform no automatic newline insertion.
       */
      public final static byte WRAP_NONE = 2;


      // attributes

      /**
       * The number of characters written on the current line.
       */
      protected int m_nCurrentLineLength;

      /**
       * The maximum number of characters per line.
       */
      protected int m_nMaxLineLength = 72;

      /**
       * The number of characters in the line-wrapping buffer.
       */
      protected int m_nBufferSize;

      /**
       * The start of character data in the line-wrapping buffer.
       */
      protected int m_nBufferOffset;

      /**
       * The folding mode; one of the WRAP_* constants.
       */
      protected byte m_nWrappingMode;


      // associations

      /**
       * The writer to which the wrapped text shall be written.
       */
      protected Writer m_out;

      /**
       * The line buffer to use for wrapping text at whitespace points.
       */
      protected char[] m_chLineBuffer = new char[MAX_MAX_LINE_LENGTH + 1];


      // constructors

      /**
       * Creates a new wrapping writer.
       * @param out The writer to which the wrapped text will be written.
       */
      public WrappingWriter(Writer out)
      {
         m_out = out;
      }

      // operations

      /**
       * Sets the wrapping mode to use. The wrapping mode may be changed at any time.
       * Changing the wrapping mode only has an effect on the characters subsequently
       * written to this writer.
       * 
       * @param nMode The wrapping mode to use; one of the WRAP_* constants.
       * @throws IOException If an I/O error occurs.
       */
      public void setWrappingMode(byte nMode) throws IOException
      {
         checkOpen();

         if (m_nWrappingMode == WRAP_REPLACE_WHITESPACE)
         {
            dumpBuffer();
         }

         if (nMode == WRAP_REPLACE_WHITESPACE)
         {
            m_nBufferOffset = m_nCurrentLineLength;
         }

         m_nWrappingMode = nMode;
      }

      /**
       * Sets the maximum line length for wrapped lines.
       * 
       * @param nMaxLength The new maximum line length. Does not include newline character
       * for end of line.
       */
      public void setMaxLineLength(int nMaxLength)
      {
         if (nMaxLength > MAX_MAX_LINE_LENGTH)
         {
            throw new IllegalArgumentException("Line length " + nMaxLength + " too long.");
         }

         m_nMaxLineLength = nMaxLength;
      }

      /**
       * Sets the length of the current line.
       * 
       * @param nCurrentLength The length of the current line.
       */
      public void setCurrentLineLength(int nCurrentLength)
      {
         m_nCurrentLineLength = nCurrentLength;
      }

      /**
       * Gets the length of the current line.
       * 
       * @return The length of the current line.
       */
      public int getCurrentLineLength()
      {
         return m_nCurrentLineLength;
      }

      /**
       * Writes a hard line break.
       * 
       * @throws IOException If an I/O error occurs.
       */
      public void writeLineBreak() throws IOException
      {
         checkOpen();
         dumpBuffer();
         m_out.write("\r\n");
         m_nCurrentLineLength = 0;
      }

      /**
       * Dumps the contents of the whitespace wrapping buffer, if any.
       * 
       * @throws IOException If am I/O error occurs.
       */
      protected void dumpBuffer() throws IOException
      {
         m_out.write(m_chLineBuffer, m_nBufferOffset, m_nBufferSize);
         m_nBufferSize = 0;
         m_nBufferOffset = 0;
      }

      /**
       * @see java.io.Writer#close()
       */
      public void close() throws IOException
      {
         if (m_out != null)
         {
            dumpBuffer();
            m_out.close();
            m_out = null;
         }
      }

      /**
       * @see java.io.Writer#flush()
       */
      public void flush() throws IOException
      {
         checkOpen();
         dumpBuffer();
         m_out.flush();
      }

      /**
       * @see java.io.Writer#write(char[], int, int)
       */
      public void write(char[] chDataArray, int nOffset, int nLength) throws IOException
      {
         checkOpen();

         switch (m_nWrappingMode)
         {
            case WRAP_REPLACE_WHITESPACE:
               wrapWhitespace(chDataArray, nOffset, nLength);
               break;

            case WRAP_ANYWHERE:
               wrapAnywhere(chDataArray, nOffset, nLength);
               break;

            case WRAP_NONE:
               m_out.write(chDataArray, nOffset, nLength);
               m_nCurrentLineLength += nLength;
               break;
         }
      }

      /**
       * Writes the character data specified in the parameter, wrapping at existing
       * whitespace locations.
       * 
       * @param chDataArray The array of data to write.
       * @param nOffset The beginning index of the data to write in the array.
       * @param nLength The number of characters to write.
       * @throws IOException If an I/O error occurs.
       */
      public void wrapWhitespace(char[] chDataArray, int nOffset, int nLength) throws IOException
      {
         while (nLength > 0)
         {
            int nCount = Math.min(m_nMaxLineLength - (m_nBufferSize + m_nBufferOffset) + 1, nLength);

            System.arraycopy(chDataArray, nOffset, m_chLineBuffer, m_nBufferSize + m_nBufferOffset, nCount);

            m_nBufferSize += nCount;
            nLength -= nCount;
            nOffset += nCount;
            m_nCurrentLineLength += nCount;

            if ((m_nBufferSize + m_nBufferOffset) == m_nMaxLineLength + 1)
            {
               int nLastSpaceIndex = m_nBufferSize + m_nBufferOffset - 1;

               while (nLastSpaceIndex >= m_nBufferOffset)
               {
                  if (m_chLineBuffer[nLastSpaceIndex] == ' ' || m_chLineBuffer[nLastSpaceIndex] == '\t')
                  {
                     break;
                  }

                  nLastSpaceIndex--;
               }

               if (nLastSpaceIndex > 0)
               {
                  m_out.write(m_chLineBuffer, m_nBufferOffset, nLastSpaceIndex - m_nBufferOffset);
                  m_out.write("\r\n");
                  m_nBufferSize = m_nBufferSize - (nLastSpaceIndex - m_nBufferOffset);
                  System.arraycopy(m_chLineBuffer, nLastSpaceIndex, m_chLineBuffer, 0, m_nBufferSize);
                  m_nCurrentLineLength = m_nBufferSize;
                  m_nBufferOffset = 0;
               }
               else
               {
                  m_out.write(m_chLineBuffer, m_nBufferOffset, m_nBufferSize - 1);
                  m_chLineBuffer[0] = m_chLineBuffer[m_nBufferOffset + m_nBufferSize - 1];
                  m_nBufferSize = 1;
                  m_nBufferOffset = 0;
               }
            }
         }
      }

      /**
       * Writes the character data specified in the parameter, wrapping at
       * any location.
       * 
       * @param chDataArray The array of data to write.
       * @param nOffset The beginning index of the data to write in the array.
       * @param nLength The number of characters to write.
       * @throws IOException If an I/O error occurs.
       */
      public void wrapAnywhere(char[] chDataArray, int nOffset, int nLength) throws IOException
      {
         while (nLength > 0)
         {
            if (m_nCurrentLineLength >= m_nMaxLineLength)
            {
               m_out.write("\r\n ");
               m_nCurrentLineLength = 1;
            }
            else
            {
               int nCount = Math.min(m_nMaxLineLength - m_nCurrentLineLength, nLength);

               m_out.write(chDataArray, nOffset, nCount);
               m_nCurrentLineLength += nCount;
               nLength -= nCount;
               nOffset += nCount;
            }
         }
      }

      /**
       * Checks that this writer has not been closed.
       * @throws IOException If not open.
       */
      protected void checkOpen() throws IOException
      {
         if (m_out == null)
         {
            throw new IOException("WrappingWriter closed");
         }
      }
   }
}
