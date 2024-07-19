package nexj.core.integration.format.json;

import java.io.IOException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.json.CompositeJSONMessagePartMapping;
import nexj.core.meta.integration.format.json.JSONMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.json.JSONParser;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.ParserException;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SOAPUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionReader;

/**
 * Parser for JSON messages. Responsible for taking an Input stream of
 * data and parsing it into a TransferObject graph, according to JSON
 * format configured by a given Message object.
 *
 * It may also accept a MessageTable, but assumes that there is only one
 * Message object in the table to be used to parse the Input stream.
 */
public class JSONMessageParser extends JSONParser implements MessageParser, InvocationContextAware
{
   // associations

   /**
    * The current message part.
    */
   protected MessagePart m_currentMessagePart;

   /**
    * The current array subtype message part.
    */
   protected CompositeMessagePart m_currentArraySubtypeMessagePart;

   /**
    * The current array object.
    */
   protected Object m_array;

   /**
    * The value name to message part map.
    */
   protected Lookup /*<valueName,messagePart>*/ m_currentPartMap;

   /**
    * The primitive formatter.
    */
   protected PrimitiveFormatter m_primitiveFormatter;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // constructors

   /**
    * Default constructor for JSONMessageParser.
    */
   public JSONMessageParser()
   {
      this(null);
   }

   /**
    * Constructor for JSONMessageparser which takes a global environment.
    * @param env The global environment variable.
    */
   public JSONMessageParser(GlobalEnvironment env)
   {
      super(env);
   }

   // operations

   /**
    * Parses a JSON input given a message.
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.Message)
    */
   public TransferObject parse(Input in, Message msg) throws IntegrationException
   {
      m_reader = new TextPositionReader(in.getReader());
      m_currentMessagePart = msg.getRoot();

      try
      {
         return (TransferObject)parse(m_reader, null);
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.parse", new Object[]{msg.getName()}, e);
      }
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#parseElement()
    */
   protected Object parseElement()
   {
      if (m_currentMessagePart instanceof CompositeMessagePart &&
         ((CompositeJSONMessagePartMapping)m_currentMessagePart.getMapping()).getSubtype() == CompositeJSONMessagePartMapping.SUBTYPE_ROOT)
      {
         return parseKeyValue(((JSONMessagePartMapping)m_currentMessagePart.getMapping()).getKeyName());
      }

      return super.parseElement();
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#createObject()
    */
   protected Object createObject()
   {
      return new TransferObject();
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#parseObjectKey(java.lang.Object)
    */
   protected String parseObjectKey(Object obj)
   {
      String sKey = super.parseObjectKey(obj);

      if (((TransferObject)obj).hasValue(sKey))
      {
         throw new IntegrationException("err.integration.json.parse.duplicateKey", new Object[]{sKey, m_currentMessagePart.getFullPath()});
      }

      m_currentMessagePart = (MessagePart)m_currentPartMap.get(sKey);

      if (m_currentMessagePart == null)
      {
         throw new IntegrationException("err.integration.json.parse.invalidKey",
                  new Object[]{sKey, ((MessagePart)m_currentPartMap.get(Primitive.createInteger(1))).getFullPath()});
      }

      return sKey;
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#parseKeyValue(java.lang.String)
    */
   protected Object parseKeyValue(String sKey)
   {
      int nToken = getCurToken();

      if (m_tokenValue == null)
      {
         if (m_currentMessagePart.isRequired())
         {
            throw new IntegrationException("err.integration.json.parse.requiredNull", new Object[]{m_currentMessagePart.getFullPath()});
         }

         return null;
      }

      if (m_currentMessagePart.isCollection())
      {
         if (nToken != TOKEN_OBRACKET)
         {
            throw new IntegrationException("err.integration.json.parse.unexpectedToken", new Object[]{m_currentMessagePart.getFullPath(), "["});
         }

         return super.parseKeyValue(sKey);
      }

      if (m_currentMessagePart instanceof PrimitiveMessagePart)
      {
         return super.parseKeyValue(sKey);
      }

      switch (((CompositeJSONMessagePartMapping)m_currentMessagePart.getMapping()).getSubtype())
      {
         case CompositeJSONMessagePartMapping.SUBTYPE_OBJECT:
            if (nToken != TOKEN_OBRACE)
            {
               throw new IntegrationException("err.integration.json.parse.unexpectedToken", new Object[]{m_currentMessagePart.getFullPath(), "{"});
            }

            break;

         case CompositeJSONMessagePartMapping.SUBTYPE_ARRAY:
            if (nToken != TOKEN_OBRACKET)
            {
               throw new IntegrationException("err.integration.json.parse.unexpectedToken", new Object[]{m_currentMessagePart.getFullPath(), "["});
            }

            break;

         case CompositeJSONMessagePartMapping.SUBTYPE_ROOT:
            CompositeMessagePart parentPart = (CompositeMessagePart)m_currentMessagePart;

            m_currentMessagePart = parentPart.getPart(0);

            String sRootKey = ((JSONMessagePartMapping)m_currentMessagePart.getMapping()).getKeyName();
            TransferObject tobj = new TransferObject();

            tobj.setValue(sRootKey, parseKeyValue(sRootKey));
            m_currentMessagePart = parentPart;
            validateCompositeMessagePart(tobj);

            return tobj;
      }

      return super.parseKeyValue(sKey);
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#addObjectValue(java.lang.Object, java.lang.String, java.lang.Object)
    */
   protected void addObjectValue(Object obj, String sKey, Object value)
   {
      TransferObject tobj = (TransferObject)obj;

      if (m_currentMessagePart.isRequired() && value == null)
      {
         throw new IntegrationException("err.integration.json.parse.requiredNull", new Object[]{m_currentMessagePart.getFullPath()});
      }

      if (m_currentMessagePart instanceof PrimitiveMessagePart && !m_currentMessagePart.isCollection())
      {
         value = convertValue(value);
      }

      tobj.setValue(sKey, value);
   }

   /**
    * Parses the buffer for an object for the current composite message part.
    * @see nexj.core.rpc.json.JSONParser#parseObject()
    */
   protected Object parseObject()
   {
      if (getCurToken() == TOKEN_CBRACE)
      {
         fail("err.parser.unexpectedToken", new Object[]{m_tokenValue}, getCurTokenPos());
      }

      CompositeMessagePart parentPart = (CompositeMessagePart)m_currentMessagePart;
      Lookup parentPartMap = m_currentPartMap;

      m_currentPartMap = new HashTab();

      // insert parent part
      m_currentPartMap.put(Primitive.createInteger(1), parentPart);

      // associate parts to their names.
      for (int i = 0, nCount = parentPart.getPartCount(); i < nCount; ++i)
      {
         MessagePart part = parentPart.getPart(i);

         m_currentPartMap.put(((JSONMessagePartMapping)part.getMapping()).getKeyName(), part);
      }

      TransferObject tobj = (TransferObject)super.parseObject();

      m_currentMessagePart = parentPart;
      m_currentPartMap = parentPartMap;
      validateCompositeMessagePart(tobj);

      return tobj;
   }

   /**
    * Validates a value against the current composite message part.
    * @param tobj The value to validate.
    */
   protected void validateCompositeMessagePart(TransferObject tobj)
   {
      CompositeMessagePart messagePart = (CompositeMessagePart)m_currentMessagePart;

      for (int i = 0, nCount = messagePart.getPartCount(); i < nCount; ++i)
      {
         MessagePart part = messagePart.getPart(i);

         if (part.isRequired() && !(tobj.hasValue(((JSONMessagePartMapping)part.getMapping()).getKeyName())))
         {
            throw new IntegrationException("err.integration.missingPart", new Object[]{part.getFullPath()});
         }
      }
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#parseArray()
    */
   protected Object parseArray()
   {
      if (m_currentMessagePart instanceof CompositeMessagePart &&
          ((CompositeJSONMessagePartMapping)m_currentMessagePart.getMapping()).getSubtype() == CompositeJSONMessagePartMapping.SUBTYPE_ARRAY)
      {
         CompositeMessagePart previousArraySubtypeMessagePart = m_currentArraySubtypeMessagePart;

         m_currentArraySubtypeMessagePart = (CompositeMessagePart)m_currentMessagePart;

         TransferObject tobj = (TransferObject)super.parseArray();

         m_currentMessagePart = m_currentArraySubtypeMessagePart;
         m_currentArraySubtypeMessagePart = previousArraySubtypeMessagePart;

         validateCompositeMessagePart(tobj);

         return tobj;
      }

      List list = (List)super.parseArray();

      validateCollection(list);

      return list;
   }

   /**
    * Validates a collection against the current message part.
    * @param list The collection to validate.
    */
   protected void validateCollection(List list)
   {
      if (list == null)
      {
         if (m_currentMessagePart.isRequired())
         {
            throw new IntegrationException("err.integration.missingPart", new Object[]{m_currentMessagePart.getFullPath()});
         }

         return;
      }

      if (list.size() > m_currentMessagePart.getMaxCount())
      {
         throw new IntegrationException("err.integration.maxPartCount", new Object[]{m_currentMessagePart.getFullPath()});
      }

      if (list.size() < m_currentMessagePart.getMinCount())
      {
         throw new IntegrationException("err.integration.minPartCount", new Object[]{m_currentMessagePart.getFullPath()});
      }
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#createArray()
    */
   protected Object createArray()
   {
      if (m_currentArraySubtypeMessagePart != null)
      {
         m_array = createObject();
      }
      else
      {
         m_array = super.createArray();
      }

      return m_array;
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#parseIndexValue(int)
    */
   protected Object parseIndexValue(int nIndex)
   {
      if (m_array instanceof TransferObject)
      {
         m_currentMessagePart = m_currentArraySubtypeMessagePart.getPart(nIndex);
      }

      return super.parseIndexValue(nIndex);
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#addArrayValue(java.lang.Object, int, java.lang.Object)
    */
   protected void addArrayValue(Object array, int nIndex, Object value)
   {
      if (m_currentMessagePart instanceof PrimitiveMessagePart)
      {
         value = convertValue(value);
      }

      if (array instanceof TransferObject)
      {
         ((TransferObject)array).setValue(((CompositeJSONMessagePartMapping)m_currentMessagePart.getMapping()).getKeyName(), value);

         return;
      }

      super.addArrayValue(array, nIndex, value);
   }

   /**
    * Converts and validates a value for the current primitive message part.
    * @param value The value to convert and validate.
    * @return The converted value.
    */
   protected Object convertValue(Object value)
   {
      JSONMessagePartMapping mapping = (JSONMessagePartMapping)m_currentMessagePart.getMapping();

      if (mapping.getFormat() != null)
      {
         if (m_primitiveFormatter == null)
         {
            m_primitiveFormatter = new PrimitiveFormatter(m_context);
         }

         value = (value instanceof String) ? value : value.toString();
         value = m_primitiveFormatter.parse((String)value, (PrimitiveMessagePart)m_currentMessagePart);
      }

      Primitive partType = ((PrimitiveMessagePart)m_currentMessagePart).getType();

      if (partType == Primitive.ANY || partType == Primitive.primitiveOf(value))
      {
         return value;
      }

      switch(partType.getOrdinal())
      {
         case Primitive.BINARY_ORDINAL:
            if (!(value instanceof String))
            {
               throw new IntegrationException("err.integration.json.parse.binary", new Object[]{m_currentMessagePart.getFullPath()});
            }

            switch (mapping.getSubtype())
            {
               case JSONMessagePartMapping.SUBTYPE_BASE64:
                  try
                  {
                     return Binary.fromBase64((String)value);
                  }
                  catch (IOException e)
                  {
                     throw new IntegrationException("err.integration.json.parse.base64", new Object[]{m_currentMessagePart.getFullPath()}, e);
                  }

               default:
                  try
                  {
                     return Primitive.BINARY.getConverter(Primitive.STRING).invoke(value);
                  }
                  catch (Exception e)
                  {
                     throw new IntegrationException("err.integration.json.parse.hex", new Object[]{m_currentMessagePart.getFullPath()}, e);
                  }
            }

         case Primitive.TIMESTAMP_ORDINAL:
            byte nSubtype = mapping.getSubtype();

            if (nSubtype == JSONMessagePartMapping.SUBTYPE_DEFAULT)
            {
               if (!(value instanceof Number))
               {
                  throw new IntegrationException("err.integration.json.parse.timestamp", new Object[]{m_currentMessagePart.getFullPath()});
               }

               return new Timestamp(((Number)value).longValue());
            }

            if (!(value instanceof String))
            {
               throw new IntegrationException("err.integration.json.parse.typeMismatch", new Object[]{m_currentMessagePart.getFullPath(), partType});
            }

            String sValue = (String)value;

            switch (nSubtype)
            {
               case XMLMessagePartMapping.SUBTYPE_DATE:
                  try
                  {
                     return SOAPUtil.parseDateTime(sValue, true, false, m_context.getTimeZone());
                  }
                  catch (Exception e)
                  {
                     throw new IntegrationException("err.integration.json.dateTime",
                        new Object[]{m_currentMessagePart.getFullPath(), "Gyyyy'-'MM'-'dd'Z'"}, e);
                  }

               case XMLMessagePartMapping.SUBTYPE_TIME:
                  try
                  {
                     return SOAPUtil.parseDateTime(sValue, false, true, m_context.getTimeZone());
                  }
                  catch (Exception e)
                  {
                     throw new IntegrationException("err.integration.json.dateTime",
                        new Object[]{m_currentMessagePart.getFullPath(), "HH':'mm':'ss.SSS'Z'"}, e);
                  }

               case XMLMessagePartMapping.SUBTYPE_DATETIME:
                  try
                  {
                     return SOAPUtil.parseDateTime(sValue, true, true, m_context.getTimeZone());
                  }
                  catch (Exception e)
                  {
                     throw new IntegrationException("err.integration.json.dateTime",
                        new Object[]{m_currentMessagePart.getFullPath(), "Gyyyy'-'MM'-'dd'T'HH':'mm':'ss.SSS'Z'"}, e);
                  }
            }

         case Primitive.DECIMAL_ORDINAL:
            if (!(value instanceof Number))
            {
               throw new IntegrationException("err.integration.json.parse.typeMismatch", new Object[]{m_currentMessagePart.getFullPath(), partType});
            }

            return Primitive.toDecimal(value);

         case Primitive.FLOAT_ORDINAL:
            if (value instanceof Number)
            {
               return Primitive.createFloat(((Number)value).floatValue());
            }
            // fall through
         default:
            throw new IntegrationException("err.integration.json.parse.typeMismatch", new Object[]{m_currentMessagePart.getFullPath(), partType});
      }
   }

   /**
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
      ArrayList messageList = new ArrayList(table.getMessageCount());
      Lookup keyMessageMap = new HashTab();

      for (int i = 0, nCount = table.getMessageCount(); i < nCount; i++)
      {
         Message message = table.getMessage(i);
         CompositeJSONMessagePartMapping mapping = (CompositeJSONMessagePartMapping)message.getRoot().getMapping();
         String sMessageKeyValue = mapping.getMessageKeyValue();

         if (sMessageKeyValue == null)
         {
            throw new IntegrationException("err.integration.json.parse.missingMessageKey", new Object[]{message});
         }

         Object oldMessage = keyMessageMap.put(sMessageKeyValue, message);

         if (oldMessage != null)
         {
            throw new IntegrationException("err.integration.json.parse.messageTableConflict", new Object[]{oldMessage, message});
         }

         messageList.add(message);
      }

      table.setParserTable(messageList);
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      int nCount = table.getMessageCount();

      if (nCount == 1)
      {
         return parse(in, table.getMessage(0));
      }

      Object obj = new JSONParser(getGlobalEnvironment()).parse(new TextPositionReader(in.getReader()), null);

      if (!(obj instanceof Lookup))
      {
         throw new IntegrationException("err.integration.json.parse.tableObject");
      }

      Lookup objMap = (Lookup)obj;

      for (int i = 0; i < nCount; ++i)
      {
         CompositeMessagePart root = table.getMessage(i).getRoot();
         CompositeJSONMessagePartMapping mapping = (CompositeJSONMessagePartMapping)root.getMapping();

         if (mapping.getMessageValue().equals(objMap.get(mapping.getMessageKey())))
         {
            return convertMessage(root, objMap);
         }
      }

      throw new IntegrationException("err.integration.json.parse.invalidObject", new Object[]{table.getParserTable()});
   }

   /**
    * Converts and validates an object into a message corresponding to the given composite message part.
    * @param compositePart The composite message part.
    * @param obj The object to convert.
    * @return The converted message.
    */
   protected TransferObject convertMessage(CompositeMessagePart compositePart, Object obj) throws IntegrationException
   {
      TransferObject tobj = null;
      JSONMessagePartMapping mapping = (JSONMessagePartMapping)compositePart.getMapping();

      switch(((CompositeJSONMessagePartMapping)mapping).getSubtype())
      {
         case CompositeJSONMessagePartMapping.SUBTYPE_ARRAY:
            if (!(obj instanceof List))
            {
               throw new IntegrationException("err.integration.json.parse.arrayMismatch", new Object[]{compositePart.getFullPath()});
            }

            List list = (List)obj;
            int nPartCount = compositePart.getPartCount();

            if (list.size() != nPartCount)
            {
               throw new IntegrationException("err.integration.json.parse.subtypeArray", new Object[]{compositePart.getFullPath()});
            }

            tobj = (TransferObject)createObject();

            for (int i = 0; i < nPartCount; ++i)
            {
               Object element = list.get(i);
               MessagePart elementPart = compositePart.getPart(i);
               String sElementKey = ((JSONMessagePartMapping)elementPart.getMapping()).getKeyName();

               if (element == null)
               {
                  if (elementPart.isRequired())
                  {
                     throw new IntegrationException("err.integration.json.parse.requiredNull", new Object[]{elementPart.getFullPath()});
                  }

                  tobj.setValue(sElementKey, element);

                  continue;
               }

               m_currentMessagePart = elementPart;

               tobj.setValue(sElementKey, (elementPart.isCollection()) ? convertCollection(elementPart, element) :
                  (elementPart instanceof PrimitiveMessagePart) ? convertValue(element) : convertMessage((CompositeMessagePart)elementPart, element));
            }

            break;

         case CompositeJSONMessagePartMapping.SUBTYPE_ROOT:
            m_currentMessagePart = compositePart.getPart(0);

            if (!(m_currentMessagePart.isCollection()))
            {
               tobj = (TransferObject)createObject();
               tobj.setValue(mapping.getKeyName(), (m_currentMessagePart instanceof PrimitiveMessagePart) ?
                     convertValue(obj) : convertMessage((CompositeMessagePart)m_currentMessagePart, obj));

               break;
            }

            if (!(obj instanceof List))
            {
               throw new IntegrationException("err.integration.json.parse.arrayMismatch", new Object[]{m_currentMessagePart.getFullPath()});
            }

            tobj = (TransferObject)createObject();
            tobj.setValue(mapping.getKeyName(), convertCollection(m_currentMessagePart, obj));

            break;

         default:
            if (compositePart instanceof CompositeMessagePartRef)
            {
               tobj = convertMessage(((CompositeMessagePartRef)compositePart).getRefPart(), obj);

               break;
            }

            if (!(obj instanceof Lookup))
            {
               throw new IntegrationException("err.integration.json.parse.objectMismatch", new Object[]{compositePart.getFullPath()});
            }

            tobj = (TransferObject)createObject();
            Lookup objMap = (Lookup)obj;

            for (int i = 0, n = compositePart.getPartCount(); i < n; ++i)
            {
               m_currentMessagePart = compositePart.getPart(i);

               JSONMessagePartMapping partMapping = (JSONMessagePartMapping)m_currentMessagePart.getMapping();
               String sKey = partMapping.getKeyName();
               Object value = objMap.get(sKey);

               if (value == null)
               {
                  if (m_currentMessagePart.isRequired())
                  {
                     throw new IntegrationException("err.integration.json.parse.requiredNull", new Object[]{m_currentMessagePart.getFullPath()});
                  }

                  if (objMap.contains(sKey))
                  {
                     tobj.setValue(sKey, null);
                  }

                  continue;
               }

               tobj.setValue(sKey, m_currentMessagePart.isCollection() ? convertCollection(m_currentMessagePart, value) :
                  (m_currentMessagePart instanceof PrimitiveMessagePart) ? convertValue(value) : convertMessage((CompositeMessagePart)m_currentMessagePart, value));
            }

            break;
      }

      return tobj;
   }

   /**
    * Converts and validates an object into a collection corresponding to the given message part.
    * @param part The message part.
    * @param obj The object.
    * @return The converted collection.
    */
   protected List convertCollection(MessagePart part, Object obj)
   {
      if (obj == null)
      {
         if (part.isRequired())
         {
            throw new IntegrationException("err.integration.json.parse.requiredNull", new Object[]{part.getFullPath()});
         }

         return null;
      }

      if (!(obj instanceof List))
      {
         throw new IntegrationException("err.integration.json.parse.arrayMismatch", new Object[]{part.getFullPath()});
      }

      m_currentMessagePart = part;

      List list = (List)obj;

      validateCollection(list);

      if (part instanceof PrimitiveMessagePart)
      {
         for (int i = 0, nCount = list.size(); i < nCount; ++i)
         {
            list.add(i, convertValue(list.remove(i)));
         }
      }
      else
      {
         CompositeMessagePart compositePart = (CompositeMessagePart)part;

         for (int i = 0, nCount = list.size(); i < nCount; ++i)
         {
            list.add(i, convertMessage(compositePart, list.remove(i)));
         }
      }

      return list;
   }

   /**
    * Sets the invocation context.
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Throws parser exception immediately.
    * @see nexj.core.scripting.GenericParser#fail(java.lang.String, java.lang.Object[], java.lang.Throwable, nexj.core.util.TextPosition)
    */
   protected void fail(String sErrCode, Object[] errArgs, Throwable cause, TextPosition pos) throws ParserException
   {
      throw new ParserException(sErrCode, errArgs, cause, pos);
   }
}