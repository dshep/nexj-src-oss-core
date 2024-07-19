// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.csv;

import java.io.EOFException;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.csv.CSVMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Invalid;
import nexj.core.util.ObjUtil;
import nexj.core.util.PagedArrayList;

/**
 * Parser for CSV messages. Responsible for taking an Input stream of
 * data and parsing it into a TransferObject graph, according to the
 * CSV format configured by a given Message object.
 *
 * It may also accept a MessageTable and determine which of the
 * Message objects in the table should be used to parse the Input
 * stream. Certain restrictions apply on the Messages that may be
 * put in the MessageTable; see the initializeMessageTable method
 * below for more information.
 *
 * @see nexj.core.integration.MessageParser
 */
public class CSVMessageParser implements MessageParser, InvocationContextAware
{
   // constants

   /**
    * The state that corresponds with reading the first character
    * of a field, from the state machine used by the readField method.
    */
   protected final static int STATE_FIELD_FIRST_CHAR = 0;

   /**
    * The state that corresponds with reading a quoted field, from the
    * state machine used by the readField method.
    */
   protected final static int STATE_READ_QUOTED_FIELD = 1;

   /**
    * The state that corresponds with reading an unquoted field, from
    * the state machine used by the readField method.
    */
   protected final static int STATE_READ_UNQUOTED_FIELD = 2;


   // attributes

   /**
    * The current character of input.
    */
   protected char m_chCurrent;

   /**
    * A flag which, when true, indicates that End Of File has been reached.
    */
   protected boolean m_bEOF;


   // associations

   /**
    * The reader object from which input is taken.
    */
   protected Reader m_reader;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The primitive value formatter.
    */
   protected PrimitiveFormatter m_primitiveFormatter;

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
      CompositeMessagePart rootPart = msg.getRoot();
      CSVMessagePartMapping rootMapping = (CSVMessagePartMapping)rootPart.getMapping();

      initializeParse(in, rootMapping);

      return parseMessage(msg, null);
   }


   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      if (table.getParserTable() == null || !(table.getParserTable() instanceof CSVMessageParserTable))
      {
         throw new IntegrationException("err.integration.messageTableUninitialized");
      }

      CSVMessageParserTable parserTable = (CSVMessageParserTable)table.getParserTable();

      if (parserTable.getMessageList().size() == 0)
      {
         return null;
      }

      Object[] rawRow = null;
      Message detectedMessage = null;

      Message longestMessage = parserTable.getMessageWithMostFields();
      CSVMessagePartMapping rootMapping = (CSVMessagePartMapping)longestMessage.getRoot().getMapping();
      CompositeMessagePart rowPart = (CompositeMessagePart)longestMessage.getRoot().getPart(0);
      CSVMessagePartMapping rowMapping = (CSVMessagePartMapping)rowPart.getMapping();

      initializeParse(in, rootMapping);

      //Count the number of fields in the first row of input, and use this number
      //to guess the message.
      try
      {
         rawRow = rawReadRow(null, rowMapping);

         detectedMessage = parserTable.getMessageForFieldCount(rawRow.length);
      }
      catch (IOException ioEx)
      {
         throw new IntegrationException("err.integration.csv.undetectableMessage", ioEx);
      }

      if (detectedMessage == null)
      {
         throw new IntegrationException("err.integration.csv.undetectableMessage");
      }

      //Parse detected Message, starting with first row of data already read.
      return parseMessage(detectedMessage, rawRow);
   }


   /**
    * Perform initialization of the parser, setting up the input reader
    * and positioning the current character so that it points to the
    * first character of header or body data.
    *
    * @param in          The input source.
    * @param rootMapping The mapping to use as a source of parser settings.
    */
   protected void initializeParse(Input in, CSVMessagePartMapping rootMapping)
   {
      m_reader = in.getReader();
      m_bEOF = false;

      try
      {
         readNext();

         //Consume leading line ends
         while((m_chCurrent == '\n' || m_chCurrent == '\r') && !m_bEOF)
         {
            readNext();
         }

         consumeComments(rootMapping);
      }
      catch (IOException ioEx)
      {
         throw new IntegrationException("err.integration.parse",
            new Object[]{null}, ioEx);
      }
   }


   /**
    * Read data and parse it according to the given message. The first row
    * of data, if it has already been read, may be supplied in the optional
    * argument firstRow.
    *
    * @param msg      The message being read.
    * @param firstRow First row of input (either header data or body data);
    *                 null to read first row directly.
    * @return A list of TransferObjects containing the data for each row of
    *         the CSV file.
    */
   protected TransferObject parseMessage(Message msg, Object[] firstRow)
   {
      CompositeMessagePart rootPart = msg.getRoot();
      CSVMessagePartMapping rootMapping = (CSVMessagePartMapping)rootPart.getMapping();
      CompositeMessagePart rowPart = (CompositeMessagePart)rootPart.getPart(0);
      CSVMessagePartMapping rowMapping = (CSVMessagePartMapping)rowPart.getMapping();
      MessagePart[] partsInOrder = rootMapping.getPartsInOrder();

      TransferObject result = null;
      List listResult;

      try
      {
         if (rootMapping.isHeader())
         {
            if (firstRow == null)
            {
               firstRow = rawReadRow(null, rowMapping);
            }

            consumeComments(rootMapping);
            partsInOrder = makePartsInHeaderOrder(firstRow, rootMapping);
            firstRow = null;
         }

         listResult = parseBody(partsInOrder, rowMapping, firstRow);
      }
      catch (Exception innerEx)
      {
         throw new IntegrationException("err.integration.parse",
            new Object[]{msg.getName()}, innerEx);
      }

      result = new TransferObject(msg.getName());
      result.setValue(rowPart.getName(), listResult);

      return result;
   }


   /**
    * Parses CSV data into a list of TransferObjects, each one corresponding to
    * a single record of CSV data. Optionally takes data that have already been
    * read from the first row.
    *
    * @param partsInOrder The message parts for each field, in CSV field order.
    * @param rowMapping   The mapping for the entire row, used for CSV parser settings.
    * @param firstRowData Optional source for first row of data; set to null to
    *                     read first row directly from input.
    * @return A list of TransferObjects containing the data for each row of the CSV file.
    * @throws IOException
    */
   protected List parseBody(MessagePart[] partsInOrder, CSVMessagePartMapping rowMapping, Object[] firstRowData) throws IOException
   {
      Object[] rawRow = firstRowData;
      TransferObject rowResult;
      List listResult;

      if (rowMapping.getPageSize() > 0)
      {
         listResult = new PagedArrayList(rowMapping.getPageSize());
      }
      else
      {
         listResult = new ArrayList();
      }

      while ((firstRowData != null && firstRowData.length > 0) || !m_bEOF)
      {
         if (rawRow == null)
         {
            rawRow = rawReadRow(partsInOrder, rowMapping);
         }

         rowResult = convertRow(rawRow, partsInOrder);
         rawRow = null;
         firstRowData = null;
         listResult.add(rowResult);

         if (!m_bEOF)
         {
            consumeComments(rowMapping);
         }
      }

      return listResult;
   }


   /**
    * Reads a row of data, returning it in as an array that holds the data
    * in column order. Can pass null as first parameter, causing this routine
    * to return an array whose length is exactly equal to the actual number
    * of fields encountered on one line. Otherwise, when first parameter
    * is non-null, the returned array is of length equivalent to the length
    * of the first argument, empty fields filled with null and missing fields
    * set to Invalid.VALUE.
    *
    * @param partsInOrder The message parts for this row, in CSV field order.
    *                     If null, then the settings on the rowMapping parameter
    *                     are used to parse each field.
    * @param rowMapping   Used to obtain parser settings for fields that have
    *                     no corresponding message part.
    * @return A row of data, padded to the size of partsInOrder with Invalid.VALUE,
    *         or if partsInOrder is null then exactly equal in size to the number
    *         of fields found in the input stream.
    * @throws IOException
    */
   protected Object[] rawReadRow(MessagePart[] partsInOrder, CSVMessagePartMapping rowMapping) throws IOException
   {
      Object[] result = new Object[rowMapping.getHighestOrdinal()];
      int nResultIndex = 0;
      char chDelimiter = rowMapping.getDelimiter().charValue();
      CSVMessagePartMapping partMapping;

      while (!m_bEOF && (m_chCurrent != '\n' && m_chCurrent != '\r'))
      {
         //Do not get more fields than necessary, if number of fields is known.
         if (partsInOrder != null && nResultIndex >= result.length)
         {
            break;
         }

         if (partsInOrder != null && partsInOrder[nResultIndex] != null)
         {
            partMapping = (CSVMessagePartMapping)partsInOrder[nResultIndex].getMapping();
         }
         else
         {
            partMapping = rowMapping;
         }

         //Infer presence of data if this field starts with a delimiter
         if (nResultIndex > 0 && m_chCurrent == chDelimiter)
         {
            //Advance to first character of data field.
            if (!m_bEOF)
            {
               readNext();
            }
         }

         chDelimiter = partMapping.getDelimiter().charValue();

         // Resize array to accommodate data, if getting unknown number of fields
         if (partsInOrder == null && nResultIndex >= result.length)
         {
            Object[] tmp = new Object[result.length << 1];
            System.arraycopy(result, 0, tmp, 0, result.length);
            result = tmp;
         }

         result[nResultIndex] = readField(partMapping);
         nResultIndex++;
      }

      //If number of columns is known, fill remaining spots in array with INVALID. Otherwise,
      //trim array down to data size.
      if (partsInOrder != null)
      {
         while (nResultIndex < result.length)
         {
            result[nResultIndex] = Invalid.VALUE;
            nResultIndex++;
         }
      }
      else
      {
         if (result.length > nResultIndex)
         {
            Object[] tmp = new Object[nResultIndex];
            System.arraycopy(result, 0, tmp, 0, nResultIndex);
            result = tmp;
         }
      }

      //Consume extra data on end of line. (e.g. extra fields)
      while (!m_bEOF && (m_chCurrent != '\n' && m_chCurrent != '\r'))
      {
         readNext();
      }

      //Consume multiple line ends (parser should be forgiving)
      while ((m_chCurrent == '\n' || m_chCurrent == '\r') && !m_bEOF)
      {
         readNext();
      }

      return result;
   }


   /**
    * Converts the raw data strings from a parsed row of data into their
    * correct primitives and places them into a new TransferObject graph
    * representing one row of data.
    *
    * @param rawRow       A single row of data, one element per CSV field.
    * @param partsInOrder The message parts, in CSV field order.
    * @return A TransferObject graph representing the processed data row.
    */
   protected TransferObject convertRow(Object[] rawRow, MessagePart[] partsInOrder)
   {
      TransferObject result = new TransferObject(partsInOrder.length);  //over-allocates
      boolean bGotData = false;

      for (int nDataIndex = 0; nDataIndex < partsInOrder.length; nDataIndex++)
      {
         PrimitiveMessagePart part = (PrimitiveMessagePart)partsInOrder[nDataIndex];

         //Check for missing required parts.
         if (nDataIndex >= rawRow.length || rawRow[nDataIndex] == Invalid.VALUE)
         {
            for (int nPartScanIndex = nDataIndex; nPartScanIndex < partsInOrder.length; nPartScanIndex++)
            {
               MessagePart scanPart = partsInOrder[nPartScanIndex];

               if (scanPart != null && scanPart.isRequired())
               {
                  throw new IntegrationException("err.integration.minPartCount",
                     new Object[]{scanPart.getFullPath()});
               }
            }

            continue;
         }

         //Skip data that are not mapped to a MessagePart
         if (part == null)
         {
            continue;
         }

         Object datum = rawRow[nDataIndex];

         //Traverse graph of TransferObjects to find the object to which this
         //datum should be attached, creating the graph as necessary.
         if ((datum != null && datum != Invalid.VALUE) || part.isRequired())
         {
            CSVMessagePartMapping mapping = (CSVMessagePartMapping)part.getMapping();

            if (mapping.getFormat() == null)
            {
               datum = part.convertValue(datum);
            }
            else
            {
               if (m_primitiveFormatter == null)
               {
                  m_primitiveFormatter = new PrimitiveFormatter(m_context);
               }

               datum = m_primitiveFormatter.parse((String)datum, part);
            }

            TransferObject nestedResult = result;
            TransferObject nextNestedResult = null;
            String[] dataKeyPath = mapping.getDataKeyPath();

            for (int nKeyPathIndex=0; nKeyPathIndex < dataKeyPath.length - 1; nKeyPathIndex++)
            {
               nextNestedResult = (TransferObject)nestedResult.findValue(dataKeyPath[nKeyPathIndex]);

               if (nextNestedResult == null)
               {
                  nextNestedResult = new TransferObject();
                  nestedResult.setValue(dataKeyPath[nKeyPathIndex], nextNestedResult);
               }

               nestedResult = nextNestedResult;
            }

            nestedResult.setValue(part.getName(), datum);
            bGotData = true;
         }
      }

      return (bGotData) ? result : null;
   }


   /**
    * Create an array that maps CSV column ordinal to message parts, in the
    * order specified by the CSV field headers in rawRow.
    *
    * @param rawRow      The CSV field headers in the desired ordering.
    * @param rootMapping Provides translation from field name to message part.
    * @return An array of message parts, organized in the order specified by the
    *         CSV field headers in rawRow.
    * @throws IntegrationException If two of the header fields are the same name.
    */
   protected MessagePart[] makePartsInHeaderOrder(Object[] rawRow, CSVMessagePartMapping rootMapping)
   {
      MessagePart[] result = new MessagePart[rawRow.length];
      boolean[] partEncounteredArray = new boolean[rootMapping.getHighestOrdinal()];

      for (int nNameIndex = 0; nNameIndex < rawRow.length; nNameIndex++)
      {
         if (rawRow[nNameIndex] == Invalid.VALUE)
         {
            continue;
         }

         MessagePart part = rootMapping.getPartByFieldName((String)rawRow[nNameIndex]);

         result[nNameIndex] = part;

         if (part == null)
         {
            continue;
         }

         CSVMessagePartMapping partMapping = (CSVMessagePartMapping)part.getMapping();

         if (!partEncounteredArray[partMapping.getOrdinal()-1])
         {
            partEncounteredArray[partMapping.getOrdinal()-1] = true;
         }
         else
         {
            throw new IntegrationException("err.integration.csv.duplicateFieldInHeader",
               new Object[]{rawRow[nNameIndex], new Integer(nNameIndex+1)});
         }
      }

      return result;
   }


   /**
    * Advance the reader past a comment or group of comments in the
    * input.
    *
    * @param map The mapping that holds the comment characters to use.
    * @throws IOException
    */
   protected void consumeComments(CSVMessagePartMapping map) throws IOException
   {
      if (map.getCommentCharacters() == null)
      {
         return;
      }

      boolean bDoneAllComments = false;

      while (!bDoneAllComments)
      {
         //Line must start with a comment character.
         if (map.getCommentCharacters().indexOf(m_chCurrent) >= 0)
         {
            boolean bDoneCommentLine = false;

            //Consume entire comment line
            while (!bDoneCommentLine)
            {
               if (!m_bEOF)
               {
                  readNext();

                  if (m_chCurrent == '\n' || m_chCurrent == '\r')
                  {
                     bDoneCommentLine = true;
                  }
               }
               else
               {
                  bDoneCommentLine = true;
               }
            }

            //Consume multiple line ends
            while((m_chCurrent == '\n' || m_chCurrent == '\r') && !m_bEOF)
            {
               readNext();
            }

            if (m_bEOF)
            {
               bDoneAllComments = true;
            }
         }
         else
         {
            bDoneAllComments = true;
         }
      }
   }


   /**
    * Read a single field of input according to the settings in the supplied message
    * part. Assumes that the current character m_cCurrent is pointing at the first
    * character of the field. When this method returns, the current character
    * m_cCurrent will be left pointing at either a delimiter or a newline.
    *
    * @param fieldMapping The message part mapping that holds parser settings for this field.
    * @return The textual value of the field, or null if the field is blank.
    * @throws IOException
    */
   protected String readField(CSVMessagePartMapping fieldMapping) throws IOException
   {
      StringBuilder buf = new StringBuilder();
      char chDelimiter = fieldMapping.getDelimiter().charValue();

      boolean bDone = false;
      int nState = STATE_FIELD_FIRST_CHAR;

      while (!bDone)
      {
         switch (nState)
         {
            case STATE_FIELD_FIRST_CHAR:

               if (fieldMapping.getQuote() != null && fieldMapping.getQuote().charValue() == m_chCurrent)
               {
                  nState = STATE_READ_QUOTED_FIELD;

                  if (!m_bEOF)
                  {
                     readNext();
                  }
                  else
                  {
                     //EOF in quoted field, automatically close quote
                     bDone = true;
                     break;
                  }
               }
               else
               {
                  nState = STATE_READ_UNQUOTED_FIELD;
               }
               break;


            case STATE_READ_UNQUOTED_FIELD:

               if (fieldMapping.getEscape() != null && m_chCurrent == fieldMapping.getEscape().charValue())
               {
                  //skip to next, and add next char literally
                  readNext();

                  if (m_bEOF)
                  {
                     //EOF right after escape, assume field ends
                     bDone = true;
                     break;
                  }
               }
               else if (m_chCurrent == chDelimiter || m_chCurrent == '\r' || m_chCurrent == '\n')
               {
                  bDone = true;
                  break;
               }

               buf.append(m_chCurrent);

               readNext();

               if(m_bEOF)
               {
                  bDone = true;
               }

               break;


            case STATE_READ_QUOTED_FIELD:

               if (fieldMapping.getEscape() != null && fieldMapping.getEscape().charValue() == m_chCurrent)
               {
                  //skip to next, and add next char literally
                  readNext();

                  if (m_bEOF)
                  {
                     //EOF right after escape, assume field ends
                     bDone = true;
                     break;
                  }
               }
               else if(m_chCurrent == fieldMapping.getQuote().charValue())
               {
                  //Lookahead to next
                  if (!m_bEOF)
                  {
                     readNext();

                     if (m_chCurrent == '\r' || m_chCurrent == '\n' || m_chCurrent == chDelimiter || m_bEOF)
                     {
                        bDone = true;
                        break;
                     }
                  }
                  else
                  {
                     bDone = true;
                     break;
                  }
               }

               buf.append(m_chCurrent);

               readNext();

               if(m_bEOF)
               {
                  bDone = true;
               }

               break;
         }
      }

      if (buf.length() == 0)
      {
         return null;
      }

      return buf.toString();
   }


   /**
    * Read next character of input, storing it in instance variable
    * m_cCurrent. If reader hits EOF, first set the instance flag
    * m_bEOF to true, throwing an EOFException on subsequent calls.
    *
    * @throws IOException
    */
   protected void readNext() throws IOException
   {
      int nCh = m_reader.read();

      if (nCh >= 0)
      {
         m_chCurrent = (char)nCh;
      }
      else
      {
         if (m_bEOF)
         {
            throw new EOFException();
         }

         m_bEOF = true;
      }
   }


   /**
    * Examine the given MessageTable, verifying that the Message objects in the
    * table meet a set of requirements that will make them distinguishable
    * to the parser. If the requirements are met, an internal CSVMessageParserTable
    * object will be created and attached to the MessageTable instance; this
    * will be used by the parser to determine which Message should be used
    * to parse the Input.
    *
    * These are the requirements that the Message objects must satisfy:
    *
    * 1) All messages must use the same delimiter/quote/escape.
    * 2) The messages must have different numbers of fields, taking
    *    into account the required and optional fields. For example,
    *    a message with three required fields and two optional fields
    *    could parse a row with either three, four, or five data fields,
    *    so it may only coexist in a MessageTable with messages that
    *    can parse zero, one, two, six or more fields.
    *
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
      int nMessages = table.getMessageCount();
      List messages = new ArrayList();

      //Consider all pairs of Message objects in the table.
      for (int nMessageIndex1 = 0; nMessageIndex1 < nMessages; nMessageIndex1++)
      {
         Message message1 = table.getMessage(nMessageIndex1);
         CSVMessagePartMapping message1Mapping = (CSVMessagePartMapping)message1.getRoot().getMapping();

         for (int nMessageIndex2 = 0; nMessageIndex2 < nMessages; nMessageIndex2++)
         {
            Message message2 = table.getMessage(nMessageIndex2);

            if (message2 == message1 || message2.getName() == message1.getName())
            {
               continue;
            }

            CSVMessagePartMapping message2Mapping = (CSVMessagePartMapping)message2.getRoot().getMapping();

            //Perform verification on the message pair (message1, message2)
            if (!haveSameConfiguration(message1Mapping, message2Mapping) || !verifyLengthRestriction(message1, message2))
            {
               throw new IntegrationException("err.integration.csv.messageTableConflict",
                  new Object[]{message1.getName(), message2.getName()});
            }
         }

         messages.add(message1);
      }

      //If all checks passed, construct a table to speed up lookup
      CSVMessageParserTable parserTable = new CSVMessageParserTable();

      parserTable.setMessageList(messages);

      int nMessageCount = messages.size();

      for (int nMessageIndex = 0; nMessageIndex < nMessageCount; nMessageIndex++)
      {
         Message msg = (Message)messages.get(nMessageIndex);

         computeMessageLengthLookup(msg, parserTable);
      }

      if (parserTable.getMessageForFieldCount(0) == null && !messages.isEmpty())
      {
         parserTable.setMessageForFieldCount(0, (Message)messages.get(0));
      }

      table.setParserTable(parserTable);
   }


   /**
    * For the given message, compute all row lengths (i.e. number of CSV fields)
    * that can be parsed into that message, and register that message for those
    * field counts with the parser table.
    *
    * @param msg The message for which all possible field counts should be computed.
    * @param table The parser table with which this message will be registered.
    */
   protected static void computeMessageLengthLookup(Message msg, CSVMessageParserTable table)
   {
      MessagePart[] parts = ((CSVMessagePartMapping)msg.getRoot().getMapping()).getPartsInOrder();

      int nFieldsMin = computeRequired(parts);
      int nFieldsMax = parts.length;

      for (int nFields = nFieldsMin; nFields <= nFieldsMax; nFields++)
      {
         table.setMessageForFieldCount(nFields, msg);
      }
   }


   /**
    * Given two messages, check that they may be distinguished by
    * the parser (and thus allowed to coexist in a MessageTable)
    * based on their count of required and optional fields. In a
    * nutshell, the algorithm ensures that the number of required
    * fields in one message exceeds the total number of fields in
    * the other message.
    *
    * @param m1 First Message for which to verify length restriction holds.
    * @param m2 Second Message for which to verify length restriction holds.
    * @return True if the given Message objects may be distinguished by
    *         the parser based solely on number of fields; false otherwise.
    */
   protected static boolean verifyLengthRestriction(Message m1, Message m2)
   {
      CSVMessagePartMapping map1 = (CSVMessagePartMapping)m1.getRoot().getMapping();
      CSVMessagePartMapping map2 = (CSVMessagePartMapping)m2.getRoot().getMapping();

      MessagePart[] m1parts = map1.getPartsInOrder();
      MessagePart[] m2parts = map2.getPartsInOrder();

      int nLength1 = m1parts.length;
      int nRequired1 = computeRequired(m1parts);
      int nLength2 = m2parts.length;
      int nRequired2 = computeRequired(m2parts);

      if (nRequired1 > nRequired2)
      {
         return (nLength2 < nRequired1);
      }
      else if (nRequired2 > nRequired1)
      {
         return (nLength1 < nRequired2);
      }

      return false;
   }


   /**
    * Given the list of message parts comprising a Message,
    * count the number of message parts that are required.
    *
    * @param parts The list of message parts that make up a
    *              message, in order.
    * @return The number or required message parts.
    */
   protected static int computeRequired(MessagePart[] parts)
   {
      for (int nPartIndex = 0; nPartIndex < parts.length; nPartIndex++)
      {
         MessagePart part = parts[nPartIndex];

         if (part != null && !part.isRequired())
         {
            return nPartIndex;
         }
      }

      return parts.length;
   }


   /**
    * Checks that the given CSV message part mappings have an equivalent
    * configuration, necessary to satisfy the requirements of allowing
    * their corresponding messages to coexist in a MessageTable. This
    * method is defined here because it is specific to the algorithm
    * used to distinguish messages in the CSV parser--it does not in
    * any way indicate that the given two mappings are equivalent.
    *
    * @param map1 The first mapping to verify.
    * @param map2 The second mapping to verify.
    * @return True if the given mappings have equivalent configurations,
    *         as defined by the algorithm used in the CSV parser to
    *         distinguish input messages; false otherwise.
    */
   protected static boolean haveSameConfiguration(CSVMessagePartMapping map1, CSVMessagePartMapping map2)
   {
      if (!map1.getDelimiter().equals(map2.getDelimiter()))
      {
         return false;
      }
      else if (!ObjUtil.equal(map1.getCommentCharacters(), map2.getCommentCharacters()))
      {
         return false;
      }
      else if (!ObjUtil.equal(map1.getEscape(), map2.getEscape()))
      {
         return false;
      }
      else if (!ObjUtil.equal(map1.getQuote(), map2.getQuote()))
      {
         return false;
      }

      return true;
   }

   // inner classes

   /**
    * This is used exclusively by the parser to organize the Message
    * objects it can accept.
    */
   protected static class CSVMessageParserTable
   {
      /**
       * The Message objects.
       */
      protected List m_messageList;

      /**
       * A mapping from the number of fields in a CSV message
       * to a Message object that supports this number of fields.
       */
      protected Message[] m_fieldToMessageArray;

      /**
       * Sets the list of Message objects.
       *
       * @param messages The list of Message objects.
       */
      public void setMessageList(List messages)
      {
         m_messageList = messages;
      }

      /**
       * Gets the list of Message objects.
       *
       * @return The list of Message objects.
       */
      public List getMessageList()
      {
         return m_messageList;
      }


      /**
       * Create a mapping from the number of CSV fields accepted
       * by a Message to the Message that accepts that number of
       * fields.
       *
       * @param nFields The number of fields the given message
       *                can accept.
       * @param msg     The Message object accepting the given
       *                number of fields.
       */
      public void setMessageForFieldCount(int nFields, Message msg)
      {
         if (m_fieldToMessageArray == null)
         {
            m_fieldToMessageArray = new Message[nFields+1];
         }

         //Grow if necessary
         if (nFields >= m_fieldToMessageArray.length)
         {
            Message[] tmp = new Message[nFields+1];
            System.arraycopy(m_fieldToMessageArray, 0, tmp, 0, m_fieldToMessageArray.length);
            m_fieldToMessageArray = tmp;
         }

         if (m_fieldToMessageArray[nFields] == null)
         {
            m_fieldToMessageArray[nFields] = msg;
         }
         else
         {
            throw new IntegrationException("err.integration.csv.sameNumberOfFields",
               new Object[]{m_fieldToMessageArray[nFields], msg.getName(), Integer.toString(nFields)});
         }
      }

      /**
       * Gets the Message that accepts the given number of CSV fields.
       *
       * @param nFields The number of fields in a row of CSV data.
       * @return The message that accepts the given number of fields.
       */
      public Message getMessageForFieldCount(int nFields)
      {
         if (m_fieldToMessageArray == null || nFields >= m_fieldToMessageArray.length)
         {
            return null;
         }

         return m_fieldToMessageArray[nFields];
      }

      /**
       * Gets the Message in this table that has the highest number of CSV fields.
       *
       * @return The message that has the largest number of CSV fields.
       */
      public Message getMessageWithMostFields()
      {
         if (m_fieldToMessageArray == null || m_fieldToMessageArray.length == 0)
         {
            return null;
         }

         return m_fieldToMessageArray[m_fieldToMessageArray.length - 1];
      }
   }

}
