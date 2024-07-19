package nexj.core.integration.format.json;

import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.sql.Timestamp;
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
import nexj.core.meta.integration.format.json.CompositeJSONMessagePartMapping;
import nexj.core.meta.integration.format.json.JSONMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.json.JSONWriter;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.LookupException;
import nexj.core.util.SOAPUtil;

/**
 * Formats a message into JSON.
 */
public class JSONMessageFormatter implements MessageFormatter, InvocationContextAware
{
   // associations

   /**
    * The JSON writer instance to use.
    */
   protected JSONWriter m_writer;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * Formatter used to format primitives.
    */
   protected PrimitiveFormatter m_primitiveFormatter;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JSONMessageFormatter.class);

   // operations

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
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Formatting JSON message \"" + message.getName() + "\"");
         s_logger.dump(tobj);
      }

      Writer writer = out.getWriter();

      m_writer = (writer instanceof JSONWriter) ? (JSONWriter)writer : new JSONWriter(writer);

      try
      {
         formatCompositeMessagePart(tobj, message.getRoot(), false);
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.format", new Object[]{message.getName()}, e);
      }
   }

   /**
    * Formats the a composite message part of a transfer object.
    * @param tobj The transfer object.
    * @param part The composite message part.
    * @param bArrayElement Whether currently formatting an array element.
    * @throws IntegrationException If format failed.
    * @throws IOException If error reading from the stream.
    */
   protected void formatCompositeMessagePart(TransferObject tobj, CompositeMessagePart part, boolean bArrayElement) throws IntegrationException, IOException
   {
      boolean bCloseObject = false;
      boolean bCloseArray = false;
      boolean bWriteKeys;
      boolean bArray;

      switch (((CompositeJSONMessagePartMapping)part.getMapping()).getSubtype())
      {
         case CompositeJSONMessagePartMapping.SUBTYPE_ARRAY:
            bCloseArray = true;
            bWriteKeys = false;
            bArray = true;
            m_writer.openArray();

            break;

         case CompositeJSONMessagePartMapping.SUBTYPE_ROOT:
            bWriteKeys = false;
            bArray = false;

            break;

         default:
            bCloseObject = true;
            bWriteKeys = true;
            bArray = false;

            m_writer.openObject();

            break;
      }

      boolean bWroteValue = false;

      for (int i = 0, nCount = part.getPartCount(); i < nCount; ++i)
      {
         MessagePart innerPart = part.getPart(i);
         String sKey = ((JSONMessagePartMapping)innerPart.getMapping()).getKeyName();
         Object obj;

         try
         {
            obj = tobj.getValue(sKey);
         }
         catch (LookupException e)
         {
            if (innerPart.isRequired())
            {
               throw new IntegrationException("err.integration.missingPart", new Object[]{innerPart.getFullPath()});
            }

            if (bArray)
            {
               if (bWroteValue)
               {
                  m_writer.writeSeparator();
               }

               m_writer.writeNull();

               bWroteValue = true;
            }

            continue;
         }

         if (bWroteValue)
         {
            m_writer.writeSeparator();
         }

         if (bWriteKeys)
         {
            m_writer.writeObjectKey(sKey);
         }

         if (obj == null)
         {
            if (innerPart.isRequired())
            {
               throw new IntegrationException("err.integration.missingPart", new Object[]{innerPart.getFullPath()});
            }

            m_writer.writeNull();

            bWroteValue = true;

            continue;
         }

         // object
         if (innerPart instanceof CompositeMessagePart)
         {
            if (!innerPart.isCollection() || bArrayElement)
            {
               formatCompositeMessagePart((TransferObject)obj, (CompositeMessagePart)innerPart, false);
            }
            else
            {
               // array
               CompositeMessagePart compositePart = (CompositeMessagePart)innerPart;
               List list = (List)obj;
               int nValueCount = list.size();

               validateArrayValueCount(innerPart, list);

               m_writer.openArray();
               formatCompositeMessagePart((TransferObject)list.get(0), compositePart, true);

               for (int k = 1; k < nValueCount; ++k)
               {
                  m_writer.writeSeparator();
                  formatCompositeMessagePart((TransferObject)list.get(k), compositePart, true);
               }

               m_writer.closeArray();
            }

            bWroteValue = true;

            continue;
         }

         // primitive
         PrimitiveMessagePart primitivePart = (PrimitiveMessagePart)innerPart;

         if (innerPart.isCollection())
         {
            // array
            List list = (List)obj;

            validateArrayValueCount(innerPart, list);
            m_writer.openArray();
            writePrimitive(primitivePart, list.get(0));

            for (int k = 1, nValueCount = list.size(); k < nValueCount; ++k)
            {
               m_writer.writeSeparator();
               writePrimitive(primitivePart, list.get(k));
            }

            m_writer.closeArray();
         }
         else
         {
            Primitive msgType = primitivePart.getType();

            if (msgType != Primitive.ANY && msgType != Primitive.primitiveOf(obj.getClass()))
            {
               throw new IntegrationException("err.integration.messageTypeMismatch", new Object[]{innerPart.getFullPath(), msgType, obj.getClass().getName()});
            }

            writePrimitive(primitivePart, obj);
         }

         bWroteValue = true;
      }

      if (bCloseArray)
      {
         m_writer.closeArray();
      }
      else if (bCloseObject)
      {
         m_writer.closeObject();
      }
   }

   /**
    * Checks if the number of values for a collection is valid for the given message part.
    * @param part The message part.
    * @param list The collection object.
    * @throws IntegrationException If number of values is invalid.
    */
   protected void validateArrayValueCount(MessagePart part, List list) throws IntegrationException
   {
      int nCount = list.size();

      if (nCount < part.getMinCount())
      {
         throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
      }

      if (nCount > part.getMaxCount())
      {
         throw new IntegrationException("err.integration.maxPartCount", new Object[]{part.getFullPath()});
      }
   }

   /**
    * Writes the JSON for a value for a given primitive message part.
    * @param part The primitive message part.
    * @param value The value to write.
    * @throws IOException If failure occurs when writing to the output stream.
    */
   protected void writePrimitive(PrimitiveMessagePart part, Object value) throws IOException
   {
      value = part.validateValue(value);

      if (value == null)
      {
         m_writer.writeNull();

         return;
      }

      JSONMessagePartMapping mapping = (JSONMessagePartMapping)part.getMapping();

      if (mapping.getFormat() != null)
      {
         if (m_primitiveFormatter == null)
         {
            m_primitiveFormatter = new PrimitiveFormatter(m_context);
         }

         if (mapping.isQuoted())
         {
            m_writer.writeString(m_primitiveFormatter.format(value, part));
         }
         else
         {
            m_writer.write(m_primitiveFormatter.format(value, part));
         }

         return;
      }

      Primitive partType = part.getType();

      if (partType == Primitive.ANY)
      {
         partType = Primitive.primitiveOf(value.getClass());
      }

      switch (partType.getOrdinal())
      {
         case Primitive.BINARY_ORDINAL:
            switch (mapping.getSubtype())
            {
               case XMLMessagePartMapping.SUBTYPE_BASE64:
                  m_writer.writeBase64((Binary)value);

                  break;

               default: // hex
                  m_writer.writeString(Primitive.toString((Binary)value));

                  break;
            }

            break;

         case Primitive.DECIMAL_ORDINAL:
            m_writer.writeNumber((BigDecimal)value);

            break;

         case Primitive.DOUBLE_ORDINAL:
            m_writer.writeNumber((Double)value);

            break;

         case Primitive.FLOAT_ORDINAL:
            m_writer.writeNumber((Float)value);

            break;

         case Primitive.INTEGER_ORDINAL:
            m_writer.writeNumber((Integer)value);

            break;

         case Primitive.LONG_ORDINAL:
            m_writer.writeNumber((Long)value);

            break;

         case Primitive.STRING_ORDINAL:
            m_writer.writeString((String)value);

            break;

         case Primitive.TIMESTAMP_ORDINAL:
            switch (mapping.getSubtype())
            {
               case XMLMessagePartMapping.SUBTYPE_DATE:
                  m_writer.writeString(SOAPUtil.formatDate((Timestamp)part.validateValue(Primitive.toTimestamp(value))));

                  break;

               case XMLMessagePartMapping.SUBTYPE_TIME:
                  m_writer.writeString(SOAPUtil.formatTime((Timestamp)part.validateValue(Primitive.toTimestamp(value))));

                  break;

               case XMLMessagePartMapping.SUBTYPE_DATETIME:
                  m_writer.writeString(SOAPUtil.formatDateTime((Timestamp)part.validateValue(Primitive.toTimestamp(value))));

                  break;

               default:
                  m_writer.writeNumber(Primitive.toLong(value));

                  break;
            }

            break;

         case Primitive.BOOLEAN_ORDINAL:
            m_writer.writeBoolean(((Boolean)value).booleanValue());

            break;
      }
   }
}
