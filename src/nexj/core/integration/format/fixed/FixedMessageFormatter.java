// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.fixed;

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
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.fixed.FixedMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Logger;

/**
 * Formatter for Fixed Length (flat file database) messages. Responsible for
 * taking a TransferObject containing the data and writing it in Fixed Length
 * format to the Output, as configured by the Message.
 *
 * @see nexj.core.integration.MessageFormatter
 */
public class FixedMessageFormatter implements MessageFormatter, InvocationContextAware
{
   // associations

   /**
    * The writer instance to use for Fixed Length output streaming.
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

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FixedMessageFormatter.class);


   // attributes

   /**
    * The template of the record
    */
   protected char[] m_achTemplate;


   // operations

   /**
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Formatting Fixed Length message \"" + message.getName() + "\"");
         s_logger.dump(tobj);
      }

      CompositeMessagePart rootPart = message.getRoot();
      CompositeMessagePart recordPart = (CompositeMessagePart)rootPart.getPart(0);

      m_writer = out.getWriter();

      FixedMessagePartMapping rootMapping = (FixedMessagePartMapping)rootPart.getMapping();

      try
      {
         m_writer.write(rootMapping.getPrefix());

         Object toFormat = tobj.findValue(recordPart.getName());

         if (toFormat instanceof List)
         {
            formatRecords((List)toFormat, recordPart);
         }
         else
         {
            throw new IntegrationException("err.integration.invalidRowCollection",
               new Object[]{(toFormat != null) ? toFormat.getClass().getName() : null});
         }

         m_writer.write(rootMapping.getSuffix());
      }
      catch (IOException innerEx)
      {
         throw new IntegrationException("err.integration.format",
            new Object[]{message.getName()}, innerEx);
      }
   }


   /**
    * Process the list of record transfer objects
    *
    * @param recordList the transfer object containing the list of records
    * @param recordPart the message part for the records node
    * @throws IOException
    */
   private void formatRecords(List recordList, CompositeMessagePart recordPart) throws IOException, IntegrationException
   {
      FixedMessagePartMapping recordMapping = (FixedMessagePartMapping)recordPart.getMapping();

      ensureTemplateSize(recordMapping.getWidth());

      for (int nRowIndex = 0; nRowIndex < recordList.size(); nRowIndex++)
      {
         formatRecord((TransferObject)recordList.get(nRowIndex), recordPart);
      }
   }


   /**
    * Process a single record transfer object, iterating over each of its fields
    *
    * @param recordTO the transfer object containing the fields of this one record
    * @param recordPart the message part for the record node
    * @throws IOException
    */
   private void formatRecord(TransferObject recordTO, CompositeMessagePart recordPart) throws IOException, IntegrationException
   {
      FixedMessagePartMapping recordMapping = (FixedMessagePartMapping)recordPart.getMapping();

      recordMapping.fillRecordTemplate(m_achTemplate);

      int nOffset = 0;

      for (int i = 0; i < recordPart.getPartCount(); i++)
      {
         PrimitiveMessagePart fieldPart = (PrimitiveMessagePart)recordPart.getPart(i);
         FixedMessagePartMapping fieldMapping = (FixedMessagePartMapping)fieldPart.getMapping();
         Object fieldValue = recordTO.findValue(fieldPart.getName());

         if (fieldValue != null)
         {
            String sFieldValue = formatField(fieldMapping, fieldValue, fieldPart);
            int nFieldOffset = nOffset;

            if (fieldMapping.isLeftAligned())
            {
               nFieldOffset += fieldMapping.getPrefix().length();
            }
            else
            {
               nFieldOffset += fieldMapping.getWidth() - fieldMapping.getSuffix().length() - sFieldValue.length();
            }

            sFieldValue.getChars(0, sFieldValue.length(), m_achTemplate, nFieldOffset);
         }
         else
         {
            if (fieldPart.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount",
                  new Object[]{fieldPart.getFullPath()});
            }
         }

         nOffset += fieldMapping.getWidth();
      }

      m_writer.write(m_achTemplate, 0, recordMapping.getWidth());
   }


   /**
    * Converts a value to string.
    *
    * @param part The primitive message part
    * @param value The value to convert
    * @return the value as a String
    */
   protected String formatField(FixedMessagePartMapping mapping, Object value, PrimitiveMessagePart part)
   {
      if (mapping.getFormat() != null)
      {
         if (m_primitiveFormatter == null)
         {
            m_primitiveFormatter = new PrimitiveFormatter(m_context);
         }

         return m_primitiveFormatter.format(value, part);
      }
      else
      {
         if (part != null)
         {
            value = part.convertValue(value);
         }

         return Primitive.toString(value);
      }
   }

   /**
    * Makes sure that m_template has at least width size
    *
    * @param nWidth the minimum width
    */
   private void ensureTemplateSize(int nWidth)
   {
      if (m_achTemplate == null || m_achTemplate.length < nWidth)
      {
         m_achTemplate = new char[nWidth];
      }
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }
}
