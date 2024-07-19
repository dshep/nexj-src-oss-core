// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.hl7;

import java.io.IOException;
import java.io.Writer;
import java.sql.Timestamp;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.hl7.HL7MessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.HL7Util;
import nexj.core.util.Logger;
import nexj.core.util.SysUtil;
import nexj.core.util.Undefined;

/**
 * HL7 message formatter.
 */
public class HL7MessageFormatter implements MessageFormatter, InvocationContextAware
{
   // constants

   /**
    * Default HL7 standard version. 
    */
   public final static String DEFAULT_VERSION = "2.5"; 

   /**
    * Default HL7 MSH processing id (Production).
    */
   public final static String DEFAULT_PROCESSING = "P";

   // attributes

   /**
    * The last field sequential number.
    */
   protected int m_nSeq;

   // associations

   /**
    * The output writer.
    */
   protected Writer m_writer;

   /**
    * Temporary buffer.
    */
   protected StringBuffer m_buf;

   /**
    * The formatted message..
    */
   protected Message m_message;

   /**
    * The MSH header, or null if none.
    */
   protected CompositeMessagePart m_msh;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(HL7MessageFormatter.class);

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Formatting HL7 message \"" + message.getName() + "\"");
            s_logger.dump(tobj);
         }

         m_writer = out.getWriter();
         m_buf = new StringBuffer(32);
         m_message = message;
         m_msh = HL7MessagePartMapping.findMSH(message);

         // Append the message header

         m_writer.write("MSH|^~\\&|");

         if (m_msh == null)
         {
            HL7MessagePartMapping mapping = (HL7MessagePartMapping)message.getRoot().getMapping();

            m_writer.write(SysUtil.CAPTION);
            m_writer.write("||||");
            HL7Util.appendDateTime(m_buf, new Timestamp(System.currentTimeMillis()));
            m_writer.append(m_buf);
            m_writer.write("||");
            m_writer.write(mapping.getName());
            m_writer.write('|');
            m_writer.write(getControlId());
            m_writer.write('|');
            m_writer.write(DEFAULT_PROCESSING);
            m_writer.write('|');

            String sVersion = mapping.getVersion();

            m_writer.write((sVersion == null) ? DEFAULT_VERSION : sVersion);
            m_writer.write("||||||\r");
         }

         append(tobj, message.getRoot(), null);
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.format",
            new Object[]{message.getName()}, e);
      }
   }

   /**
    * Generates an MSH control id.
    */
   protected String getControlId()
   {
      return String.valueOf(m_context.getMetadata().getMetaclass("SysCounter")
         .invoke("next", new Object[]{"hl7.message"}));
   }

   /**
    * Sets a value corresponding to a given message part.
    * @param tobj The destination transfer object.
    * @param composite The composite part corresponding to the transfer object.
    * @param nSeq The sequence number of a child part.
    * @param value The value to set.
    */
   protected void setValue(TransferObject tobj, CompositeMessagePart composite, int nSeq, Object value)
   {
      for (int i = 0, n = composite.getPartCount(); i < n; ++i)
      {
         MessagePart part = composite.getPart(i);

         if (((HL7MessagePartMapping)part.getMapping()).getSeq() == nSeq)
         {
            tobj.setValue(part.getName(), value);

            break;
         }
      }
   }

   /**
    * Gets a default MSH value.
    */
   protected Object getDefaultMSHValue(MessagePart part)
   {
      Primitive primitive = null;
      TransferObject tobj;

      if (part instanceof PrimitiveMessagePart)
      {
         primitive = ((PrimitiveMessagePart)part).getType();
      }

      switch (((HL7MessagePartMapping)part.getMapping()).getSeq())
      {
         case 7: // Message time
            if (primitive != null)
            {
               return primitive.convert(new Timestamp(System.currentTimeMillis()));
            }

            break;

         case 9: // Message type
            String sType = ((HL7MessagePartMapping)m_message.getRoot().getMapping()).getName();
            String sEvent = null;

            int i = sType.indexOf('^');

            if (i >= 0)
            {
               sEvent = sType.substring(i + 1);
               sType = sType.substring(0, i);
            }

            if (primitive != null)
            {
               return sType;
            }

            tobj = new TransferObject(2);
            setValue(tobj, (CompositeMessagePart)part, 1, sType);
            setValue(tobj, (CompositeMessagePart)part, 2, sEvent);

            return tobj;

         case 10: // Control Id
            if (primitive != null)
            {
               return primitive.convert(getControlId());
            }

            break;

         case 11: // Processing Id
            if (primitive != null)
            {
               return DEFAULT_PROCESSING;
            }

            tobj = new TransferObject(1);
            setValue(tobj, (CompositeMessagePart)part, 1, DEFAULT_PROCESSING);

            return tobj;

         case 12: // Version
            if (primitive != null)
            {
               return DEFAULT_VERSION;
            }

            break;
      }

      return Undefined.VALUE;
   }

   /**
    * Appends a composite message part to the message.
    * @param tobj The transfer object representing the message.
    * @param composite The message part.
    * @param parentComposite The parent message part.
    */
   protected void append(TransferObject tobj, CompositeMessagePart composite,
         CompositeMessagePart parentComposite) throws IOException
   {
      HL7MessagePartMapping mapping = (HL7MessagePartMapping)composite.getMapping();
      int nLevel = mapping.getLevel();
      int nSeqSaved = m_nSeq;

      if (nLevel >= HL7MessagePartMapping.LEVEL_FIELD)
      {
         nLevel = Math.max(nLevel, ((HL7MessagePartMapping)parentComposite.getMapping()).getLevel() + 1);
      }

      m_nSeq = 1;

      // Open a segment

      if (nLevel == HL7MessagePartMapping.LEVEL_SEGMENT && composite != m_msh)
      {
         m_writer.write(mapping.getName());
         m_writer.write('|');
      }

      for (int i = 0, n = composite.getPartCount(); i < n; ++i)
      {
         MessagePart part = composite.getPart(i);
         Object value = tobj.findValue(part.getName(), Undefined.VALUE);

         if (composite == m_msh)
         {
            if (((HL7MessagePartMapping)part.getMapping()).getSeq() <= 2)
            {
               m_nSeq = 3;

               continue;
            }

            if (value == Undefined.VALUE)
            {
               value = getDefaultMSHValue(part);
            }
         }

         if (value != Undefined.VALUE)
         {
            HL7MessagePartMapping partMapping = (HL7MessagePartMapping)part.getMapping();

            if (value == null)
            {
               if (part.isRequired())
               {
                  throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
               }

               if (partMapping.getLevel() >= HL7MessagePartMapping.LEVEL_FIELD)
               {
                  appendSeparators(partMapping, nLevel);
                  m_writer.write("\"\"");
               }
            }
            else
            {
               appendSeparators(partMapping, nLevel);

               if (part.isCollection())
               {
                  List list = (List)value;

                  if (list.size() < part.getMinCount())
                  {
                     throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
                  }

                  if (list.size() > part.getMaxCount())
                  {
                     throw new IntegrationException("err.integration.maxPartCount", new Object[]{part.getFullPath()});
                  }

                  for (int k = 0, m = list.size(); k < m; ++k)
                  {
                     if (k != 0 && partMapping.getLevel() >= HL7MessagePartMapping.LEVEL_FIELD)
                     {
                        m_writer.write('~');
                     }

                     if (part instanceof CompositeMessagePart)
                     {
                        append((TransferObject)list.get(k), (CompositeMessagePart)part, composite);
                     }
                     else
                     {
                        append(list.get(k), (PrimitiveMessagePart)part);
                     }
                  }
               }
               else
               {
                  if (part instanceof CompositeMessagePart)
                  {
                     append((TransferObject)value, (CompositeMessagePart)part, composite);
                  }
                  else
                  {
                     append(value, (PrimitiveMessagePart)part);
                  }
               }
            }
         }
         else
         {
            if (part.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
            }
         }
      }

      // Close a segment
      if (nLevel == HL7MessagePartMapping.LEVEL_SEGMENT)
      {
         while (m_nSeq < mapping.getSeq())
         {
            m_writer.write('|');
            ++m_nSeq;
         }

         m_writer.write('\r');
      }

      m_nSeq = nSeqSaved;
   }

   /**
    * Appends separators before the current message part.
    * @param mapping The message part mapping.
    * @param nParentLevel The level of the parent part.
    */
   protected void appendSeparators(HL7MessagePartMapping mapping, int nParentLevel) throws IOException
   {
      char ch;

      switch (Math.max(mapping.getLevel(), nParentLevel + 1))
      {
         case HL7MessagePartMapping.LEVEL_FIELD:
            ch = '|';
            break;

         case HL7MessagePartMapping.LEVEL_COMPONENT:
            ch = '^';
            break;

         case HL7MessagePartMapping.LEVEL_SUBCOMPONENT:
            ch = '&';
            break;

         default:
            return;
      }

      while (m_nSeq < mapping.getSeq())
      {
         m_writer.write(ch);
         ++m_nSeq;
      }
   }

   /**
    * Appends a primitive value to the message.
    * @param value The value to append.
    * @param part The message part metadata.
    */
   protected void append(Object value, PrimitiveMessagePart part) throws IOException
   {
      value = part.convertValue(value);

      if (value == null)
      {
         value = "\"\"";
      }

      switch (part.getType().getOrdinal())
      {
         case Primitive.STRING_ORDINAL:
            String s = (String)value;

            for (int i = 0, n = s.length(); i != n; ++i)
            {
               char ch = s.charAt(i);

               switch (ch)
               {
                  case '|':
                     m_writer.write("\\F\\");
                     break;

                  case '^':
                     m_writer.write("\\S\\");
                     break;

                  case '&':
                     m_writer.write("\\T\\");
                     break;

                  case '~':
                     m_writer.write("\\R\\");
                     break;

                  case '\r':
                     m_writer.write("\\X0D\\");
                     break;

                  case '\\':
                     m_writer.write("\\E\\");
                     break;

                  default:
                     m_writer.write(ch);
                     break;
               }
            }

            break;

         case Primitive.TIMESTAMP_ORDINAL:
            m_buf.setLength(0);

            switch (((HL7MessagePartMapping)part.getMapping()).getSubtype())
            {
               case HL7MessagePartMapping.SUBTYPE_DT:
                  HL7Util.appendDate(m_buf, (Timestamp)value);
                  break;

               case HL7MessagePartMapping.SUBTYPE_TM:
                  HL7Util.appendTime(m_buf, (Timestamp)value);
                  break;

               default:
                  HL7Util.appendDateTime(m_buf, (Timestamp)value);
                  break;
            }

            m_writer.append(m_buf);

            break;

         case Primitive.FLOAT_ORDINAL:
            m_writer.write(Primitive.toString(Primitive.toDecimal((Float)value)));
            break;

         case Primitive.DOUBLE_ORDINAL:
            m_writer.write(Primitive.toString(Primitive.toDecimal((Double)value)));
            break;

         default:
            m_writer.write(Primitive.toString(value));
            break;
      }
   }
}
