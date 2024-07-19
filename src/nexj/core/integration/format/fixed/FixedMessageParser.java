// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.fixed;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.fixed.FixedMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Logger;
import nexj.core.util.PagedArrayList;
import nexj.core.util.StringUtil;

/**
 * Parser for fixed length (flat file database) messages. Responsible for taking
 * an Input stream of data and parsing it into a TransferObject graph, according
 * to the fixed length format configured by a given Message object.
 *
 * It may also accept a MessageTable, but assumes that there is only one
 * Message object in the table to be used to parse the Input stream.
 */
public class FixedMessageParser implements MessageParser, InvocationContextAware
{
   // associations

   /**
    * The reader object from which input is taken.
    */
   protected Reader m_reader;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FixedMessageParser.class);

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The primitive value formatter.
    */
   protected PrimitiveFormatter m_primitiveFormatter;

   /**
    * The buffer for reading field data into.  Initialized to record width.
    */
   protected char[] m_chRecord;

   // attributes

   /**
    * Track parse progress.
    */
   protected int m_nCurrentRecord;

   /**
    * Flag for whether we've hit EOF on m_reader.
    */
   protected boolean m_bEOF;

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.Message)
    */
   public TransferObject parse(Input in, Message msg) throws IntegrationException
   {
      m_nCurrentRecord = 0;

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Parsing Fixed message \"" + msg.getName() + "\"");
         s_logger.dump(msg);
      }

      CompositeMessagePart rootPart = msg.getRoot();

      // initialize reader
      m_reader = in.getReader();
      m_bEOF = false;

      // parse message
      CompositeMessagePart recordPart = (CompositeMessagePart)rootPart.getPart(0);
      FixedMessagePartMapping recordMapping = (FixedMessagePartMapping)recordPart.getMapping();

      if (m_chRecord == null)
      {
         m_chRecord = new char[recordMapping.getWidth()];
      }

      TransferObject result = null;
      List listResult;

      try
      {
         FixedMessagePartMapping rootMapping = (FixedMessagePartMapping)rootPart.getMapping();

         if (rootMapping != null)
         {
            m_reader.skip(rootMapping.getPrefix().length());
         }

         listResult = parseRecords(recordMapping);
      }
      catch (IOException innerEx)
      {
         throw new IntegrationException("err.integration.parseRecord",
            new Object[]{msg.getName(), Primitive.createInteger(m_nCurrentRecord)}, innerEx);
      }

      result = new TransferObject(msg.getName(), 1);
      result.setValue(recordPart.getName(), listResult);

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Parsed result: ");
         s_logger.dump(result);
      }

      return result;
   }


   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      if (!(table.getParserTable() instanceof Message))
      {
         throw new IntegrationException("err.integration.messageTableUninitialized");
      }

      return parse(in, (Message)table.getParserTable());
   }


   /**
    * Parses fixed length data into a list of TransferObjects, each one corresponding to
    * a single record.
    *
    * @param recordMapping   The mapping for the entire records part, used for parser settings.
    * @return A list of TransferObjects containing the data for each row of the fixed length file.
    * @throws IOException
    */
   protected List parseRecords(FixedMessagePartMapping recordMapping) throws IOException, IntegrationException
   {
      List listResult;

      if (recordMapping.getPageSize() > 0)
      {
         listResult = new PagedArrayList(recordMapping.getPageSize());
      }
      else
      {
         listResult = new ArrayList();
      }

      String[] sRawArray;

      while (!m_bEOF && ((sRawArray = rawReadRecord(recordMapping)) != null))
      {
         listResult.add(convertRecord(recordMapping, sRawArray));
      }

      return listResult;
   }


   /**
    * Reads a record of data, returning it as an array that holds the data (as strings) in field order.
    *
    * @param recordMapping Used to obtain parser settings for fields that have no corresponding message part.
    * @return A record of data with a (possibly empty) value for every defined field in the message.
    * @throws IOException If an I/O error occurs.
    */
   protected String[] rawReadRecord(FixedMessagePartMapping recordMapping) throws IOException, IntegrationException
   {
      CompositeMessagePart recordPart = (CompositeMessagePart)recordMapping.getMessagePart();
      String[] sResultArray = new String[recordPart.getPartCount()];

      m_nCurrentRecord += 1;

      if (recordMapping.getPrefix() != null)
      {
         m_reader.skip(recordMapping.getPrefix().length());
      }

      for (int i = 0; i < recordPart.getPartCount(); i++)
      {
         PrimitiveMessagePart fieldPart = (PrimitiveMessagePart)recordPart.getPart(i);

         if ((sResultArray[i] = readField(fieldPart)) == null && m_bEOF)
         {
            return null;
         }
      }

      if (recordMapping.getSuffix() != null)
      {
         m_reader.skip(recordMapping.getSuffix().length());
      }

      return sResultArray;
   }


   /**
    * Converts the raw data strings from a parsed record into their
    * correct primitives and places them into a new TransferObject graph
    * representing the record.
    *
    * @param recordMapping The mapping for this record.
    * @param sRawRecordArray A single row of data, one element per fixed length field.
    * @return A TransferObject graph representing the processed data row.
    */
   protected TransferObject convertRecord(FixedMessagePartMapping recordMapping, String[] sRawRecordArray)
   {
      CompositeMessagePart recordPart = (CompositeMessagePart)recordMapping.getMessagePart();

      if (recordPart.getPartCount() == 0)
      {
         return null;
      }

      TransferObject result = new TransferObject(recordPart.getPartCount());

      for (int i = 0; i < recordPart.getPartCount(); i++)
      {
         PrimitiveMessagePart fieldPart = (PrimitiveMessagePart)recordPart.getPart(i);
         Object datum = sRawRecordArray[i];
         FixedMessagePartMapping mapping = (FixedMessagePartMapping)fieldPart.getMapping();

         if (mapping.getFormat() == null)
         {
            datum = fieldPart.convertValue(datum);
         }
         else
         {
            if (m_primitiveFormatter == null)
            {
               m_primitiveFormatter = new PrimitiveFormatter(m_context);
            }

            datum = m_primitiveFormatter.parse((String)datum, fieldPart);
         }

         result.setValue(fieldPart.getName(), datum);
      }

      return result;
   }


   /**
    * Read a single field of input according to the settings in the supplied message
    * part. Assumes that the current character m_cCurrent is pointing at the first
    * character of the field. When this method returns, the current character
    * m_cCurrent will be left pointing at the first character of the next field or EOF.
    *
    * @param fieldMapping The message part mapping that holds parser settings for this field.
    * @return The textual value of the field, null if field has no data or EOF (while setting m_bEOF).
    * @throws IOException If an I/O error occurs.
    * @throws IntegrationException If the parsed data does not match field requirements
    */
   protected String readField(PrimitiveMessagePart fieldPart) throws IOException, IntegrationException
   {
      FixedMessagePartMapping fieldMapping = (FixedMessagePartMapping)fieldPart.getMapping();
      int nReadLen = readNumChars(m_reader, m_chRecord, fieldMapping.getWidth());

      if (nReadLen < fieldMapping.getWidth())
      {
         m_bEOF = true;

         if (nReadLen == 0)
         {
            return null;
         }

         Arrays.fill(m_chRecord, nReadLen, m_chRecord.length, fieldMapping.getPadding());
      }

      String sField = trimToNull(m_chRecord, fieldMapping.getPadding(), fieldMapping.getPrefix().length(), (nReadLen - fieldMapping.getSuffix().length()));

      if (StringUtil.isEmpty(sField))
      {
         if (fieldPart.isRequired())
         {
            throw new IntegrationException("err.integration.parse.noDataForRequiredPart",
               new Object[]{fieldPart.getFullPath(), Primitive.createInteger(m_nCurrentRecord)});
         }
         else
         {
            return null;  // no data in field
         }
      }

      return sField;
   }

   /**
    * Calls reader.read() in a loop filling achRecord until either EOF or nCharsToRead chars have been read.
    * 
    * @return the number of characters read; if return < nCharsToRead then EOF was reached.
    */
   private static int readNumChars(Reader reader, char[] achRecord, int nCharsToRead) throws IOException
   {
      int nTotalRead = 0;
      int nCurrRead;

      while (nCharsToRead > 0)
      {
         nCurrRead = reader.read(achRecord, nTotalRead, nCharsToRead);

         if (nCurrRead == -1)
         {
            return nTotalRead;
         }

         nTotalRead += nCurrRead;
         nCharsToRead -= nCurrRead;
      }

      return nTotalRead;
   }

   /**
    * Examine the given MessageTable, verifying that the Message objects in the
    * table meet a set of requirements that will make them distinguishable
    * to the parser.
    *
    * For fixed length, we only support a single message, so set the parser
    * table to that message.
    *
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
      if (table.getMessageCount() > 1)
      {
         throw new IntegrationException("err.integration.fixed.onlyOneMessageAllowedInTable");
      }

      // only supporting one message, so stick it in the parser table
      table.setParserTable((table.getMessageCount() == 0) ? null : table.getMessage(0));
   }


   /**
    * Trims the data of the char array as a String after removing all padding
    * characters from the left and the right ends of the array.
    *
    * @param buf The char array to trim. Can be null or empty.
    * @param chPad the padding character to trim from the given character array.
    *
    * @return The trimmed string. Null if the given character array is empty or
    *         null, the empty string if after trimming all padding no data is
    *         left.
    *
    * @see java.lang.String#trim()
    */
   public static String trimToNull(char[] buf, char chPad)
   {
      return trimToNull(buf, chPad, 0, buf.length);
   }

   /**
    * Trims the data of the char sub-array (data[nFromIndex]..data[nToIndex]) as
    * a String after removing all padding characters from the left and the right
    * ends of the array.
    *
    * @param buf The char array to trim. Can be null or empty.
    * @param chPad the padding character to trim from the given character array.
    * @param nFromIndex index of the first character (inclusive) of the
    *           sub-array to be trimmed.
    * @param nToIndex the index of the last character (exclusive) of the
    *           sub-array to be trimmed.
    *
    * @return The trimmed string. Null if the given character array is empty or
    *         null, the empty string if after trimming all padding no data is
    *         left.
    *
    * @see java.lang.String#trim()
    */
   public static String trimToNull(char[] buf, char chPad, int nFromIndex, int nToIndex)
   {
      if (buf == null || buf.length == 0)
      {
         return null;
      }

      while(nFromIndex < (nToIndex - 1))
      {
         if (buf[nFromIndex] != chPad)
         {
            break;
         }

         nFromIndex++;
      }

      while((nToIndex - 1) >= nFromIndex)
      {
         if (buf[nToIndex - 1] != chPad)
         {
            break;
         }

         nToIndex--;
      }

      return new String(buf, nFromIndex, (nToIndex - nFromIndex));
   }
}
