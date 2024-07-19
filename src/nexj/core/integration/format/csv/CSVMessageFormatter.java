// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.csv;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.csv.CSVMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Undefined;

/**
 * Formatter for CSV messages. Responsible for taking a TransferObject containing the
 * data and writing it in CSV format to the Output, as configured by the Message.
 * 
 * @see nexj.core.integration.MessageFormatter
 */
public class CSVMessageFormatter implements MessageFormatter, InvocationContextAware
{
   // associations
   
   /**
    * The writer instance to use for CSV output streaming.
    */
   protected Writer m_writer;

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
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {  
      CompositeMessagePart rootPart = message.getRoot();
      CSVMessagePartMapping rootMapping = (CSVMessagePartMapping)rootPart.getMapping();
      
      CompositeMessagePart rowPart = (CompositeMessagePart)rootPart.getPart(0);
      
      //Get the MessageParts in the order specified by their ordinal tags.
      PrimitiveMessagePart[] partsInOrder = rootMapping.getPartsInOrder();
      
      try
      {
         m_writer = out.getWriter();
         
         //First line should be the column headers.
         if (rootMapping.isHeader())
         {
            formatHeader(rootMapping);
         }
         
         //The one and only sub-datum will be a List of TransferObjects if there
         //are many rows or a single TransferObject if there is just one row.
         Object toFormat = tobj.findValue(rowPart.getName());

         if (toFormat instanceof List)
         {
            formatRows((List)toFormat, partsInOrder, rowPart);
         }
         else
         {
            throw new IntegrationException("err.integration.invalidRowCollection",
               new Object[]{(toFormat != null) ? toFormat.getClass().getName() : null});
         }
      }
      catch (Exception innerEx)
      {
         throw new IntegrationException("err.integration.format",
            new Object[]{message.getName()}, innerEx);
      }
   }
   
   
   /**
    * Output the column headers in CSV field order, using the part ordering
    * and formatter settings supplied by rootMapping.
    * 
    * @param rootMapping  The mapping object attached to the root of the
    *                     Message, to be used as a source of message parts
    *                     in CSV field order and formatter settings.
    * @throws IOException
    */
   protected void formatHeader(CSVMessagePartMapping rootMapping) throws IOException
   {
      MessagePart[] partsInOrder = rootMapping.getPartsInOrder();

      for (int nPartIndex=0; nPartIndex < partsInOrder.length; nPartIndex++)
      {
         if (partsInOrder[nPartIndex] != null)
         {
            formatField(((CSVMessagePartMapping)partsInOrder[nPartIndex].getMapping()).getField(), rootMapping, null);
         }
         
         if (nPartIndex < partsInOrder.length - 1)
         {
            m_writer.write(rootMapping.getDelimiter().charValue());
         }
      }
      
      m_writer.write(rootMapping.getLineEnd());
   }
   
   
   /**
    * Outputs a set of rows (CSV records) of data.
    * 
    * @param rows         A List of TransferObjects containing the data to output.
    * @param partsInOrder PrimitiveMessageParts corresponding to CSV fields in ordinal order.
    * @param rowPart      Holds all of the MessageParts that comprise the main
    *                     level of the CSV message.
    * @throws IOException If the writer encounters a problem.
    */
   protected void formatRows(List rows, PrimitiveMessagePart[] partsInOrder, CompositeMessagePart rowPart) throws IOException
   {
      int nRowCount = rows.size();
      
      for (int nRowIndex = 0; nRowIndex < nRowCount; nRowIndex++)
      {
         formatRow((TransferObject)rows.get(nRowIndex), partsInOrder, rowPart);
         m_writer.write(((CSVMessagePartMapping)rowPart.getMapping()).getLineEnd());
      }
   }
   
   
   /**
    * Outputs an entire row (a single CSV file record) of data.
    * 
    * @param rowTransferObj The TransferObject which contains the data for this row, keyed
    *                       on the name of each MessagePart in the row.
    * @param partsInOrder   The list of message parts for a data row, in ordinal order.
    * @param rowPart        Holds all of the PrimitiveMessageParts that comprise this level of the
    *                       Message.
    * @throws IOException If the writer encounters a problem.
    */
   protected void formatRow(TransferObject rowTransferObj, PrimitiveMessagePart[] partsInOrder,
      CompositeMessagePart rowPart) throws IOException
   {      
      //No delimiter before first field on line
      int nDelimiterAccumulator = 0;
      
      //Null row datum may only be ignored if the first field (and so all of the fields) is optional
      if (rowTransferObj == null)
      {
         if (partsInOrder != null && partsInOrder.length >= 1 && partsInOrder[0].isRequired())
         {
            throw new IntegrationException("err.integration.minPartCount",
               new Object[]{partsInOrder[0].getFullPath()});
         }
         
         return;
      }
      
      MessagePart previousPart = rowPart;
      
      //Format fields in ordinal order
      for (int nPartIndex=0; nPartIndex < partsInOrder.length; nPartIndex++)
      {
         PrimitiveMessagePart part = partsInOrder[nPartIndex];
         
         if (part == null)
         {
            nDelimiterAccumulator += 1;
            continue;
         }
         
         CSVMessagePartMapping partMapping = (CSVMessagePartMapping)part.getMapping();
         
         //Get the transfer object for this part's datum.
         String[] dataKeyPath = partMapping.getDataKeyPath();
         Object datum = rowTransferObj;

         for (int nKeyPathIndex=0; nKeyPathIndex < dataKeyPath.length; nKeyPathIndex++)
         {
            datum = ((TransferObject)datum).findValue(dataKeyPath[nKeyPathIndex], Undefined.VALUE);
            
            if (datum == Undefined.VALUE)
            {
               break;
            }
         }
         
         //Always write the absolute minimum number of delimiters: if there is no data
         //for the last fields on the line, then no delimiter is written.
         if (datum != Undefined.VALUE)
         {
            for (int nDelim = 0; nDelim < nDelimiterAccumulator; nDelim++)
            {
               m_writer.write(((CSVMessagePartMapping)previousPart.getMapping()).getDelimiter().charValue());
            }
            
            nDelimiterAccumulator = 1;
            formatField(datum, partMapping, part);
         }
         else
         {
            nDelimiterAccumulator++;
            
            if (part.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount",
                  new Object[]{part.getFullPath()});
            }
         }
         
         previousPart = part;
      }
   }
   
   
   /**
    * Outputs a value from a single field to the writer, using the formatting rules
    * given in mapping. The value object is expected to be either a String or a
    * Primitive.
    * 
    * @param value   The object, either a String or a Primitive, to output.
    * @param mapping The formatting rules for this CSV field.
    * @throws IOException If the writer encounters a problem.
    */
   protected void formatField(Object value, CSVMessagePartMapping mapping, PrimitiveMessagePart part) throws IOException
   {
      String sValue;

      if (mapping.getFormat() != null)
      {
         if (m_primitiveFormatter == null)
         {
            m_primitiveFormatter = new PrimitiveFormatter(m_context);
         }

         sValue = m_primitiveFormatter.format(value, part);
      }
      else
      {
         if (part != null)
         {
            value = part.convertValue(value);
         }

         sValue = Primitive.toString(value);
      }

      formatString(sValue, mapping);
   }
   
   
   /**
    * Outputs a single field on the writer, using the formatting rules given in
    * mapping.
    * 
    * @param sToFormat  The text value of the CSV field to output. If
    *                   null, write nothing.
    * @param mapping    The formatting rules for this CSV field.
    * @throws IOException If the writer encounters a problem.
    */
   protected void formatString(String sToFormat, CSVMessagePartMapping mapping) throws IOException
   {      
      boolean bQuoteIt = false;
      
      if (sToFormat == null)
      {
         return;
      }
      
      int nLength = sToFormat.length();
      
      //Configuration parameters for the output filter
      char chMatch1, chMatch2, chOut;
      
      //Configure output filter according to the mapping and the data value being formatted.
      if (mapping.getQuote() == null)
      {
         if (mapping.getEscape() == null)
         {
            m_writer.write(sToFormat);
            return;
         }
         
         //Prefix delimiters and escapes with an escape
         chOut = chMatch1 = mapping.getEscape().charValue(); 
         chMatch2 = mapping.getDelimiter().charValue();
         
         //If this is the first field in the row and its datum starts with a comment character,
         //then escape the comment character.
         if (mapping.isFirstField() && mapping.getCommentCharacters() != null && mapping.getCommentCharacters().indexOf(sToFormat.charAt(0)) >= 0)
         {
            m_writer.write(chOut);
         }
         
         /* 
          * Output Filter: this is the code from the end of this method, with
          *    an additional check to ensure that there are no newline characters
          *    in the field data, as newlines can't be handled when there is no
          *    quoting.
          * 
          * Perhaps this check is not strictly necessary, though without it, the
          * output will be garbage.
          */
         for (int nChIndex = 0; nChIndex < nLength; nChIndex++)
         {
            char chIn = sToFormat.charAt(nChIndex);
            
            if (chIn == chMatch1 || chIn == chMatch2)
            {
               m_writer.write(chOut);
            }
            else if (chIn == '\n' || chIn == '\r')
            {
               throw new IntegrationException("err.integration.csv.fieldHasNewlineButCantQuote",
                  new Object[]{mapping.getMessagePart().getFullPath()});
            }
            
            m_writer.write(chIn);
         }

         return;
      }
      else
      {
         //The needsQuoting check is expensive, making this algorithm O(2*nLength)
         //For maximum efficiency, metadata should set the "quoted" flag as often as possible.
         bQuoteIt = (mapping.isQuoted() || needsQuoting(sToFormat, mapping));
         
         //If this is the first field in the row and its datum starts with a comment character,
         //then the whole field should be quoted.
         bQuoteIt = bQuoteIt || (mapping.isFirstField() && mapping.getCommentCharacters() != null && mapping.getCommentCharacters().indexOf(sToFormat.charAt(0)) >= 0);         
         
         if (bQuoteIt)
         {
            m_writer.write(mapping.getQuote().charValue());
            
            if (mapping.getEscape() == null)
            {
               //Prefix quotes with a quote
               chOut = chMatch1 = chMatch2 = mapping.getQuote().charValue();
            }
            else
            {
               //Prefix quotes and escapes with an escape
               chOut = chMatch1 = mapping.getEscape().charValue(); 
               chMatch2 = mapping.getQuote().charValue();
            }
         }
         else
         {
            //Don't quote it, but still need to escape any escapes
            if (mapping.getEscape() != null)
            {               
               chOut = chMatch1 = chMatch2 = mapping.getEscape().charValue();
            }
            else
            {
               //Doesn't contain any quotes, delimiters, or newlines, and no escape
               //character is set, so just give raw output.
               m_writer.write(sToFormat);
               return;
            }
         }
      }
      
      //Output Filter: write string, escaping special characters according to configuration.
      for (int nChIndex = 0; nChIndex < nLength; nChIndex++)
      {
         char chIn = sToFormat.charAt(nChIndex);
         
         if (chIn == chMatch1 || chIn == chMatch2)
         {
            m_writer.write(chOut);
         }
         
         m_writer.write(chIn);
      }
      
      if (bQuoteIt)
      {
         m_writer.write(mapping.getQuote().charValue());
      }
   }
   
   
   /**
    * Scans the given String to see if it needs to be quoted. This is determined by
    * looking for newline whitespace, the delimiter character, and the quote
    * character.
    * 
    * @param sPotential The string to scan
    * @param mapping    Used to determine the quote and delimiter characters
    * @return True if the string will need to be quoted; false otherwise.
    */
   protected boolean needsQuoting(String sPotential, CSVMessagePartMapping mapping)
   {
      char chQuote = mapping.getQuote().charValue();
      char chDelim = mapping.getDelimiter().charValue();
      int nLength = sPotential.length();
      
      for (int nChIndex = 0; nChIndex < nLength; nChIndex++)
      {
         char chIn = sPotential.charAt(nChIndex);
                 
         if (chIn == chQuote || chIn == chDelim || chIn == '\n' || chIn == '\r')
         {
            return true;
         }
      }
      
      return false;
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }
}
