package nexj.core.rpc.json;

import java.io.IOException;
import java.io.Reader;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Iterator;
import java.util.TimeZone;

import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.persistence.OID;
import nexj.core.rpc.CharacterStreamUnmarshaller;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.UnmarshallerException;
import nexj.core.runtime.Context;
import nexj.core.runtime.ContextAware;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.Base64Util;
import nexj.core.util.Binary;
import nexj.core.util.GenericException;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.LocaleUtil;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.StringId;
import nexj.core.util.TextPositionReader;

/**
 * JSON unmarshaller.
 */
public class JSONUnmarshaller extends JSONParser implements CharacterStreamUnmarshaller, ContextAware
{
   // associations

   /**
    * The object reference map: TransferObject[sClassName,sOID].
    */
   protected Lookup2D m_objectMap;

   /**
    * The runtime context.
    */
   protected Context m_context;

   /**
    * The JSON map
    */
   protected Lookup m_valueMap;

   /**
    * The unmarshaller array, indexed by type character.
    */
   protected final static Lookup s_unmshMap = new HashTab(20);

   protected final static Unmarshaller TRANSFER_OBJECT_UNMSH = new Unmarshaller()
   {
      public Object unmarshal(JSONUnmarshaller unmsh)
      {
         String sClassName = unmsh.removeString(JSON.TRANSFER_OBJECT);
         String sOID = unmsh.removeString(JSON.TRANSFER_OBJECT_OID);
         TransferObject tobj = (sClassName == null || sOID == null) ? null :
            (TransferObject)unmsh.m_objectMap.get(sOID, sClassName);

         // check if TO was found in object map
         if (tobj == null)
         {
            tobj = new TransferObject();

            if (sOID != null)
            {
               if (sClassName != null)
               {
                  unmsh.m_objectMap.put(sOID, sClassName, tobj);
               }

               if (sOID.charAt(0) == '@') // is a placeholder OID
               {
                  sOID = null;
               }
            }

            tobj.setClassName(sClassName);

            try
            {
               tobj.setOID((sOID == null) ? null : OID.fromBinary(new Binary(Base64Util.decode(sOID))));
            }
            catch (IOException e)
            {
               throw new JSONUnmarshallerException("err.rpc.base64", e);
            }
         }

         String sEventName = unmsh.removeString(JSON.TRANSFER_OBJECT_EVENT);

         if (sEventName != null)
         {
            tobj.setEventName(sEventName);
         }

         Number version = unmsh.removeNumber(JSON.TRANSFER_OBJECT_VERSION);

         if (version != null)
         {
            tobj.setVersion(version.shortValue());
         }

         for (Lookup.Iterator itr = unmsh.m_valueMap.iterator(); itr.hasNext();)
         {
            String sName = (String)itr.next();

            tobj.setValue(sName, itr.getValue());
         }

         return tobj;
      }
   };

   static
   {
      s_unmshMap.put(JSON.FUNCTION, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            PCodeFunction fun;
            Boolean macro = unmsh.getBoolean(JSON.MACRO);

            if (macro != null && macro.booleanValue())
            {
               fun = new PCodeMacro();
            }
            else
            {
               fun = new PCodeFunction();
            }

            String sCode = unmsh.getString(JSON.FUNCTION);

            if (sCode != null)
            {
               fun.code = sCode.toCharArray();
            }

            Collection constants = unmsh.getCollection(JSON.FUNCTION_CONSTANTS);

            if (constants != null)
            {
               fun.constants = constants.toArray();
            }

            return fun;
         }
      });

      s_unmshMap.put(JSON.PAIR, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            Pair p = new Pair(unmsh.m_valueMap.get(JSON.PAIR));

            p.setTail(unmsh.m_valueMap.get(JSON.PAIR_TAIL));

            return p;
         }
      });

      s_unmshMap.put(JSON.TIMESTAMP, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            Number time = unmsh.getNumber(JSON.TIMESTAMP);

            if (time != null)
            {
               return new Timestamp(time.longValue());
            }

            return null;
         }
      });

      s_unmshMap.put(JSON.RESPONSE_RESULTS, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            Response response = new Response();

            Collection collection = unmsh.getCollection(JSON.RESPONSE_RESULTS);

            if (collection != null)
            {
               for (Iterator itrResults = collection.iterator(); itrResults.hasNext();)
               {
                  response.addResult(itrResults.next());
               }
            }

            collection = unmsh.getCollection(JSON.RESPONSE_EVENTS);

            if (collection != null)
            {
               for (Iterator itrEvents = collection.iterator(); itrEvents.hasNext();)
               {
                  response.addEvent((Collection)itrEvents.next());
               }
            }

            return response;
         }
      });

      s_unmshMap.put(JSON.REQUEST, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            Boolean value;
            Request req = new Request();

            req.setNamespace(unmsh.getString(JSON.REQUEST_NAMESPACE));
            req.setVersion(unmsh.getString(JSON.REQUEST_VERSION));

            if ((value = unmsh.getBoolean(JSON.REQUEST_ASYNC)) != null)
            {
               req.setAsync(value.booleanValue());
            }

            if ((value = unmsh.getBoolean(JSON.REQUEST_COMMIT)) != null)
            {
               req.setCommit(value.booleanValue());
            }

            String sValue = unmsh.getString(JSON.REQUEST_LOCALE);

            if (sValue != null)
            {
               req.setLocale(LocaleUtil.parse(sValue));
            }

            sValue = unmsh.getString(JSON.REQUEST_TIMEZONE);

            if (sValue != null)
            {
               req.setTimeZone(TimeZone.getTimeZone(sValue));
            }

            req.setCorrelator(unmsh.getTransferObject(JSON.REQUEST_CORRELATOR));

            Collection collection = unmsh.getCollection(JSON.REQUEST);

            if (collection != null)
            {
               for (Iterator itr = collection.iterator(); itr.hasNext();)
               {
                  TransferObject tobj = (TransferObject)itr.next();

                  if (tobj.getClassName() != null)
                  {
                     req.addInvocation(tobj);
                  }
                  else
                  {
                     Collection argCollection = (Collection)tobj.findValue(JSON.INVOCATION_ARGUMENTS);

                     req.addInvocation((TransferObject)tobj.findValue(JSON.INVOCATION_OBJECT),
                        (String)tobj.findValue(JSON.INVOCATION_EVENT),
                        (argCollection == null) ? null : argCollection.toArray(),
                           (Pair)tobj.findValue(JSON.INVOCATION_ATTRIBUTES));
                  }
               }
            }

            collection = unmsh.getCollection(JSON.REQUEST_FILTERS);

            if (collection != null)
            {
               for (Iterator itr = collection.iterator(); itr.hasNext();)
               {
                  req.addFilter((TransferObject)itr.next());
               }
            }

            return req;
         }
      });

      s_unmshMap.put(JSON.EXCEPTION, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            String sErrCode = unmsh.getString(JSON.EXCEPTION);
            String sMessage = unmsh.getString(JSON.EXCEPTION_ERR_MSG);
            Collection argList = unmsh.getCollection(JSON.EXCEPTION_ERR_ARGS);
            String sClassName = unmsh.getString(JSON.EXCEPTION_CLASS);

            GenericException exception;

            if (sClassName != null)
            {
               exception = new ValidationException(sErrCode, argList.toArray());
            }
            else if (argList != null)
            {
               exception = new GenericException(sErrCode, argList.toArray());
            }
            else
            {
               exception = new GenericException(sErrCode, new Object[] { sMessage });
            }

            OID oid = null;

            String sOID = unmsh.getString(JSON.EXCEPTION_OID);

            if (sOID != null)
            {
               try
               {
                  oid = OID.fromBinary(new Binary(Base64Util.decode(sOID)));
               }
               catch (IOException e)
               {
                  throw new JSONUnmarshallerException("err.rpc.base64", e);
               }
            }

            Number nOrdinal = unmsh.getNumber(JSON.EXCEPTION_ORDINAL);

            if (sClassName != null)
            {
               ValidationException validationExcep = (ValidationException)exception;

               validationExcep.setClassName(sClassName);
               validationExcep.setOIDHolder(oid);
               validationExcep.setOrdinal(nOrdinal.intValue());
            }

            Collection attrList = unmsh.getCollection(JSON.EXCEPTION_ATTRS);
            Collection attrExceps = unmsh.getCollection(JSON.EXCEPTION_ATTR_EXCEPTIONS);

            if (attrList != null)
            {
               String[] sAttrsArray = (String[])attrList.toArray(new String[attrList.size()]);
               Throwable[] excepsArray = (Throwable[])attrExceps.toArray(new Throwable[attrExceps.size()]);
               int nCount = sAttrsArray.length;

               for (int i = 0; i < nCount; i++)
               {
                  ((ValidationException)exception).addException(sAttrsArray[i], excepsArray[i]);
               }
            }

            Collection causeList = unmsh.getCollection(JSON.EXCEPTION_EXCEPTIONS);

            if (causeList != null)
            {
               for (Iterator itrExceptions = causeList.iterator(); itrExceptions.hasNext();)
               {
                  exception.addException((Throwable)itrExceptions.next());
               }
            }

            return exception;
         }
      });

      s_unmshMap.put(JSON.TRANSFER_OBJECT, TRANSFER_OBJECT_UNMSH);

      s_unmshMap.put(JSON.OID, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            String sOID = unmsh.getString(JSON.OID);

            if (sOID == null)
            {
               return null;
            }

            try
            {
               return OID.fromBinary(new Binary(Base64Util.decode(sOID)));
            }
            catch (IOException e)
            {
               throw new JSONUnmarshallerException("err.rpc.base64", e);
            }
         }
      });

      s_unmshMap.put(JSON.STRING_ID, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return new StringId(unmsh.getString(JSON.STRING_ID));
         }
      });

      s_unmshMap.put(JSON.CHARACTER, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return Primitive.createCharacter(unmsh.getString(JSON.CHARACTER).charAt(0));
         }
      });

      s_unmshMap.put(JSON.BINARY, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            try
            {
               return new Binary(Base64Util.decode(unmsh.getString(JSON.BINARY)));
            }
            catch (IOException e)
            {
               throw new JSONUnmarshallerException("err.rpc.base64", e);
            }
         }
      });

      s_unmshMap.put(JSON.LOCALE, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return LocaleUtil.parse(unmsh.getString(JSON.LOCALE));
         }
      });

      s_unmshMap.put(JSON.TIMEZONE, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return TimeZone.getTimeZone(unmsh.getString(JSON.TIMEZONE));
         }
      });

      s_unmshMap.put(JSON.SYMBOL, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return Symbol.define(unmsh.getString(JSON.SYMBOL));
         }
      });

      s_unmshMap.put(JSON.PRIVILEGE_SET, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            try
            {
               return new PrivilegeSet(Binary.parse(unmsh.getString(JSON.PRIVILEGE_SET)).getData());
            }
            catch (Exception e)
            {
               throw new JSONUnmarshallerException("err.rpc.base64", e);
            }
         }
      });

      s_unmshMap.put(JSON.CHAR_ARRAY, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return unmsh.getString(JSON.CHAR_ARRAY).toCharArray();
         }
      });

      s_unmshMap.put(JSON.STRING_ARRAY, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            Collection collection = unmsh.getCollection(JSON.STRING_ARRAY);

            return collection.toArray(new String[collection.size()]);
         }
      });

      s_unmshMap.put(JSON.OBJECT_ARRAY, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return unmsh.getCollection(JSON.OBJECT_ARRAY).toArray();
         }
      });

      s_unmshMap.put(JSON.BYTE_ARRAY, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            try
            {
               return Base64Util.decode(unmsh.getString(JSON.BYTE_ARRAY));
            }
            catch (IOException e)
            {
               throw new JSONUnmarshallerException("err.rpc.base64", e);
            }
         }
      });

      s_unmshMap.put(JSON.EXPRESSION, new Unmarshaller()
      {
         public Object unmarshal(JSONUnmarshaller unmsh)
         {
            return RPCUtil.parse(new SchemeParser(unmsh.m_context.getMachine().getGlobalEnvironment()),
               unmsh.getString(JSON.EXPRESSION), null);
         }
      });
   }

   // constructors

   /**
    * Creates the unmarshaller with a given runtime context.
    *
    * @param context The runtime context.
    */
   public JSONUnmarshaller(Context context)
   {
      super(context.getMachine().getGlobalEnvironment());

      m_context = context;
   }

   // operations

   /**
    * Sets the runtime context.
    *
    * @param context The runtime context to set.
    */
   public void setContext(Context context)
   {
      m_context = context;
   }

   /**
    * @return The runtime context.
    */
   public Context getContext()
   {
      return m_context;
   }

   /**
    * Deserializes an object from a character stream containing a message.
    *
    * @param reader The character stream reader.
    * @return The deserialized object.
    */
   public Object deserialize(Reader reader) throws IOException, UnmarshallerException
   {
      m_objectMap = new HashTab2D();

      return parse(new TextPositionReader(reader), null);
   }

   /**
    * @see nexj.core.rpc.json.JSONParser#parseObject()
    */
   protected Object parseObject()
   {
      m_valueMap = (Lookup)super.parseObject();

      for (Lookup.Iterator itr = m_valueMap.iterator(); itr.hasNext();)
      {
         String sKey = (String)itr.next();

         if (sKey.length() != 0 && sKey.charAt(0) == ':')
         {
            Unmarshaller unmsh = (Unmarshaller)s_unmshMap.get(sKey);

            if (unmsh != null)
            {
               return unmsh.unmarshal(this);
            }
         }
      }

      return TRANSFER_OBJECT_UNMSH.unmarshal(this);
   }

   /**
    * Retrieves an object from the value map and casts it to a Boolean
    * @param sKey The Key for the object in the value map
    * @return The retrieved Boolean object
    */
   protected Boolean getBoolean(String sKey)
   {
      try
      {
         return (Boolean)m_valueMap.get(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "Boolean", e.getMessage() });
      }
   }

   /**
    * Retrieves an object from the value map and casts it to a String
    * @param sKey The Key for the object in the value map
    * @return The retrieved String object
    */
   protected String getString(String sKey)
   {
      try
      {
         return (String)m_valueMap.get(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "String", e.getMessage() });
      }
   }

   /**
    * Retrieves an object from the value map and casts it to a String
    * @param sKey The Key for the object in the value map
    * @return The retrieved String object
    */
   protected String removeString(String sKey)
   {
      try
      {
         return (String)m_valueMap.remove(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "String", e.getMessage() });
      }
   }

   /**
    * Retrieves an object from the value map and casts it to a Collection
    * @param sKey The Key for the object in the value map
    * @return The retrieved Collection object
    */
   protected Collection getCollection(String sKey)
   {
      try
      {
         return (Collection)m_valueMap.get(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "Collection", e.getMessage() });
      }
   }

   /**
    * Retrieves an object from the value map and casts it to a Number
    * @param sKey The Key for the object in the value map
    * @return The retrieved Number object
    */
   protected Number getNumber(String sKey)
   {
      try
      {
         return (Number)m_valueMap.get(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "Number", e.getMessage() });
      }
   }

   /**
    * Retrieves an object from the value map and casts it to a Number
    * @param sKey The Key for the object in the value map
    * @return The retrieved Number object
    */
   protected Number removeNumber(String sKey)
   {
      try
      {
         return (Number)m_valueMap.remove(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "Number", e.getMessage() });
      }
   }

   /**
    * Retrieves an object from the value map and casts it to a TransferObject
    * @param sKey The Key for the object in the value map
    * @return The retrieved TransferObject object
    */
   protected TransferObject getTransferObject(String sKey)
   {
      try
      {
         return (TransferObject)m_valueMap.get(sKey);
      }
      catch (ClassCastException e)
      {
         throw new JSONUnmarshallerException("err.rpc.json.unmsh.unexpectedObjectType", new Object[] { sKey, "TransferObject", e.getMessage() });
      }
   }

   // inner classes

   /**
    * Interface implemented by JSON unmarshallers.
    */
   private interface Unmarshaller
   {
      /**
       * Unmarshals an object of a specific type from the input stream.
       *
       * @param msh The JSON unmarshaller.
       * @return The unmarshalled object.
       */
      Object unmarshal(JSONUnmarshaller unmsh);
   }
}