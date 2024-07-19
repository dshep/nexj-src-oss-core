package nexj.core.rpc.json;

import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Locale;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.ErrorLocationHolder;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Context;
import nexj.core.runtime.ContextAware;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.ErrorCode;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PagedArrayList;
import nexj.core.util.PagedBinary;
import nexj.core.util.PropertyIterator;
import nexj.core.util.StringId;
import nexj.core.util.TZ;

/**
 * JSON format object marshaller.
 */
public class JSONMarshaller implements CharacterStreamMarshaller, ContextAware
{
   // constants

   /**
    * The exception marshaller.
    */
   protected final static Marshaller EXCEPTION_MSH = new Marshaller()
   {
      public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
      {
         Throwable t = (Throwable)obj;

         writer.openObject();

         if (obj instanceof ErrorCode)
         {
            ErrorCode e = (ErrorCode)obj;

            writer.writeObjectKey(JSON.EXCEPTION);
            writer.writeString(e.getErrorCode());

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ERR_MSG);
            writer.writeString((msh.getContext() == null) ? t.getLocalizedMessage() :
               msh.getContext().formatString(e.getErrorCode(), e.getErrorArgs()));

            Object[] argArray = e.getErrorArgs();
            int nLen = (argArray == null) ? 0 : argArray.length;

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ERR_ARGS);

            if (nLen == 0)
            {
               writer.writeNull();
            }
            else
            {
               writer.openArray();
               msh.marshal(argArray[0]);

               for (int i = 1; i < nLen; i++)
               {
                  writer.writeSeparator();
                  msh.marshal(argArray[i]);
               }

               writer.closeArray();
            }
         }
         else
         {
            writer.writeObjectKey(JSON.EXCEPTION);
            writer.writeString("err." + t.getClass().getName());

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ERR_MSG);
            writer.writeString(t.getLocalizedMessage());

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ERR_ARGS);
            writer.writeNull();
         }

         if (obj instanceof ErrorLocationHolder)
         {
            ErrorLocationHolder h = (ErrorLocationHolder)obj;
            String sName = h.getClassName();

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_CLASS);

            if (sName == null)
            {
               writer.writeNull();
            }
            else
            {
               writer.writeString(sName);
            }

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_OID);

            OIDHolder oidHolder = h.getOIDHolder();

            if (oidHolder == null || oidHolder.getOID() == null)
            {
               writer.writeNull();
            }
            else
            {
               writer.writeBase64(oidHolder.getOID().toBinary());
            }

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ORDINAL);
            writer.write(String.valueOf(h.getOrdinal()));

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ATTRS);

            int nCount = h.getAttributeCount();

            if (nCount == 0)
            {
               writer.writeNull();
               writer.writeSeparator();
               writer.writeObjectKey(JSON.EXCEPTION_ATTR_EXCEPTIONS);
               writer.writeNull();
            }
            else
            {
               writer.openArray();

               for (Iterator itr = h.getAttributeIterator(); itr.hasNext();)
               {
                  writer.writeString((String)itr.next());

                  if (itr.hasNext())
                  {
                     writer.writeSeparator();
                  }
               }

               writer.closeArray();

               writer.writeSeparator();
               writer.writeObjectKey(JSON.EXCEPTION_ATTR_EXCEPTIONS);

               writer.openArray();

               for (Iterator itr = h.getAttributeIterator(); itr.hasNext();)
               {
                  msh.marshal(h.findException((String)itr.next()));

                  if (itr.hasNext())
                  {
                     writer.writeSeparator();
                  }
               }

               writer.closeArray();
            }
         }
         else
         {
            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_CLASS);
            writer.writeNull();

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_OID);
            writer.writeNull();

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ORDINAL);
            writer.writeNull();

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ATTRS);
            writer.writeNull();

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_ATTR_EXCEPTIONS);
            writer.writeNull();
         }

         if (obj instanceof ExceptionHolder)
         {
            ExceptionHolder h = (ExceptionHolder)obj;

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_EXCEPTIONS);

            boolean bNull = true;

            for (Iterator itr = h.getExceptionIterator(); itr.hasNext();)
            {
               Throwable cause = (Throwable)itr.next();

               if (!ObjUtil.isSystem(cause))
               {
                  if (bNull)
                  {
                     bNull = false;
                     writer.openArray();
                  }
                  else
                  {
                     writer.writeSeparator();
                  }

                  marshal(cause, msh, writer);
               }
            }

            if (bNull)
            {
               writer.writeNull();
            }
            else
            {
               writer.closeArray();
            }
         }
         else
         {
            Throwable cause = t.getCause();

            writer.writeSeparator();
            writer.writeObjectKey(JSON.EXCEPTION_EXCEPTIONS);

            if (cause == null || ObjUtil.isSystem(cause))
            {
               writer.writeNull();
            }
            else
            {
               marshal(cause, msh, writer);
            }
         }

         writer.closeObject();
      }
   };

   /**
    * The Symbol Marshaller
    */
   protected final static Marshaller SYMBOL_MSH = new Marshaller()
   {
      public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
      {
         writer.writeObject(JSON.SYMBOL, ((Symbol)obj).getName());
      }
   };

   /**
    * The marshaller map, class to marshaller implementation.
    */
   protected final static Lookup s_mshMap = new HashTab(35); // of type Marshaller[Class]

   static
   {
      // BigDecimal, Byte, Double, Float, Integer, Long, Short
      s_mshMap.put(Integer.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeNumber((Integer)obj);
         }
      });

      s_mshMap.put(Long.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeNumber((Long)obj);
         }
      });

      s_mshMap.put(Float.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeNumber((Float)obj);
         }
      });

      s_mshMap.put(Double.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeNumber((Double)obj);
         }
      });

      s_mshMap.put(BigDecimal.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeNumber((BigDecimal)obj);
         }
      });

      s_mshMap.put(Short.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeNumber((Short)obj);
         }
      });

      // java.util.Date, sql.Time, sql.Date, sql.Timestamp
      s_mshMap.put(java.sql.Timestamp.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.openObject();
            writer.writeObjectKey(JSON.TIMESTAMP);
            writer.write(String.valueOf(((java.util.Date)obj).getTime()));
            writer.closeObject();
         }
      });

      s_mshMap.put(java.util.Date.class, s_mshMap.get(java.sql.Timestamp.class));
      s_mshMap.put(java.sql.Date.class, s_mshMap.get(java.sql.Timestamp.class));
      s_mshMap.put(java.sql.Time.class, s_mshMap.get(java.sql.Timestamp.class));

      s_mshMap.put(Boolean.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeBoolean(((Boolean)obj).booleanValue());
         }
      });

      s_mshMap.put(String.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeString((String)obj);
         }
      });

      s_mshMap.put(StringId.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            String sStringID = ((StringId)obj).toString();

            if (sStringID == null)
            {
               sStringID = "";
            }

            writer.writeObject(JSON.STRING_ID, sStringID);
         }
      });

      s_mshMap.put(Character.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.CHARACTER, ((Character)obj).charValue());
         }
      });

      // Binary, PagedBinary
      s_mshMap.put(Binary.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.BINARY, (Binary)obj);
         }
      });
      s_mshMap.put(PagedBinary.class, s_mshMap.get(Binary.class));

      s_mshMap.put(Locale.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.LOCALE, obj.toString());
         }
      });

      // TimeZone, TZ, SimpleTimeZone
      s_mshMap.put(TimeZone.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.TIMEZONE, ((TimeZone)obj).getID());
         }
      });
      s_mshMap.put(TZ.UTC.getClass(), s_mshMap.get(TimeZone.class));
      s_mshMap.put(SimpleTimeZone.class, s_mshMap.get(TimeZone.class));

      //ArrayList, PagedArrayList
      s_mshMap.put(ArrayList.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.openArray();

            ArrayList list = (ArrayList)obj;
            int nCount = list.size();

            if (nCount == 0)
            {
               writer.closeArray();

               return;
            }

            msh.marshal(list.get(0));

            for (int i = 1; i < nCount; i++)
            {
               writer.writeSeparator();
               msh.marshal(list.get(i));
            }

            writer.closeArray();
         }
      });

      s_mshMap.put(PagedArrayList.class, s_mshMap.get(ArrayList.class));

      s_mshMap.put(TransferObject.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            TransferObject tobj = (TransferObject)obj;

            writer.openObject();

            writer.writeObjectKey(JSON.TRANSFER_OBJECT);
            writer.writeString(tobj.getClassName());

            writer.writeSeparator();
            writer.writeObjectKey(JSON.TRANSFER_OBJECT_OID);

            String sOID = (String)msh.m_objectMap.get(obj);

            if (sOID != null)
            {
               // TO found in marshalled object map
               writer.writeString(sOID);
               writer.closeObject();

               return;
            }

            OID oid = tobj.getOID();

            if (oid == null)
            {
               sOID = "@" + msh.m_objectMap.size();
            }
            else
            {
               sOID = oid.toBinary().toBase64();
            }

            msh.m_objectMap.put(obj, sOID);

            writer.writeString(sOID);

            if (tobj.getEventName() != null)
            {
               writer.writeSeparator();
               writer.writeObjectKey(JSON.TRANSFER_OBJECT_EVENT);
               writer.writeString(tobj.getEventName());
            }

            if (tobj.getVersion() != 0)
            {
               writer.writeSeparator();
               writer.writeObjectKey(JSON.TRANSFER_OBJECT_VERSION);
               writer.writeNumber(Primitive.createInteger(tobj.getVersion()));
            }

            for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
            {
               writer.writeSeparator();
               writer.writeObjectKey((String)itr.next());
               msh.marshal(itr.getValue());
            }

            writer.closeObject();
         }
      });

      s_mshMap.put(OID.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.openObject();
            writer.writeObjectKey(JSON.OID);

            writer.writeBase64(((OID)obj).toBinary());

            writer.closeObject();
         }
      });

      s_mshMap.put(Symbol.class, SYMBOL_MSH);

      //Pair, ConstPair
      s_mshMap.put(Pair.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            Pair pair = (Pair)obj;
            int nCount = 1;

            for (;;)
            {
               Object head = pair.getHead();

               writer.openObject();
               writer.writeObjectKey(JSON.PAIR);

               // optimized for nested Pair and Symbol
               if (head instanceof Pair)
               {
                  marshal(head, msh, writer);
               }
               else if (head instanceof Symbol)
               {
                  SYMBOL_MSH.marshal(head, msh, writer);
               }
               else if (head != null)
               {
                  msh.marshal(head);
               }
               else
               {
                  writer.writeNull();
               }

               Object tail = pair.getTail();

               if (tail == null)
               {
                  break;
               }

               writer.writeSeparator();
               writer.writeObjectKey(JSON.PAIR_TAIL);

               if (tail instanceof Pair)
               {
                  pair = (Pair)tail;
                  nCount++;
               }
               else
               {
                  msh.marshal(tail);

                  break;
               }
            }

            for (; nCount > 0; nCount--)
            {
               writer.closeObject();
            }
         }
      });

      s_mshMap.put(ConstPair.class, s_mshMap.get(Pair.class));

      s_mshMap.put(byte[].class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.BYTE_ARRAY, new Binary((byte[])obj));
         }
      });

      s_mshMap.put(char[].class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.CHAR_ARRAY, new String((char[])obj));
         }
      });

      s_mshMap.put(String[].class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            String[] stringArray = (String[])obj;
            int nCount = stringArray.length;

            writer.openObject();
            writer.writeObjectKey(JSON.STRING_ARRAY);

            writer.openArray();

            if (nCount == 0)
            {
               writer.closeArray();

               return;
            }

            writer.writeString(stringArray[0]);

            for (int i = 1; i < nCount; i++)
            {
               writer.writeSeparator();
               writer.writeString(stringArray[i]);
            }

            writer.closeArray();
            writer.closeObject();
         }
      });

      s_mshMap.put(Object[].class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            Object[] objArray = (Object[])obj;
            int nCount = objArray.length;

            writer.openObject();
            writer.writeObjectKey(JSON.OBJECT_ARRAY);

            writer.openArray();

            if (nCount == 0)
            {
               writer.closeArray();

               return;
            }

            msh.marshal(objArray[0]);

            for (int i = 1; i < nCount; i++)
            {
               writer.writeSeparator();
               msh.marshal(objArray[i]);
            }

            writer.closeArray();
            writer.closeObject();
         }
      });

      s_mshMap.put(PrivilegeSet.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            writer.writeObject(JSON.PRIVILEGE_SET, (PrivilegeSet)obj);
         }
      });

      //PCodeFunction, PCodeMacro
      s_mshMap.put(PCodeFunction.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            PCodeFunction fun = ((PCodeFunction)obj).getCleanCopy();

            writer.openObject();

            if (obj instanceof PCodeMacro)
            {
               writer.writeObjectKey(JSON.MACRO);
               writer.writeBoolean(true);
               writer.writeSeparator();
            }

            writer.writeObjectKey(JSON.FUNCTION);

            if (fun.code == null)
            {
               writer.writeNull();
            }
            else
            {
               writer.writeString(new String(fun.code));
            }

            writer.writeSeparator();
            writer.writeObjectKey(JSON.FUNCTION_CONSTANTS);

            if (fun.constants == null)
            {
               writer.writeNull();
            }
            else
            {
               writer.openArray();
               msh.marshal(fun.constants[0]);

               int nCount = fun.constants.length;

               for (int i = 1; i < nCount; i++)
               {
                  writer.writeSeparator();
                  msh.marshal(fun.constants[i]);
               }

               writer.closeArray();
            }

            writer.closeObject();
         }
      });

      s_mshMap.put(PCodeMacro.class, s_mshMap.get(PCodeFunction.class));

      s_mshMap.put(Request.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            Request req = (Request)obj;

            writer.openObject();

            if (req.getNamespace() != null)
            {
               writer.writeObjectKey(JSON.REQUEST_NAMESPACE);
               writer.writeString(req.getNamespace());
               writer.writeSeparator();
            }

            if (req.getVersion() != null)
            {
               writer.writeObjectKey(JSON.REQUEST_VERSION);
               writer.writeString(req.getVersion());
               writer.writeSeparator();
            }

            if (req.isAsync())
            {
               writer.writeObjectKey(JSON.REQUEST_ASYNC);
               writer.writeBoolean(req.isAsync());
               writer.writeSeparator();
            }

            if (!req.isCommit())
            {
               writer.writeObjectKey(JSON.REQUEST_COMMIT);
               writer.writeBoolean(req.isCommit());
               writer.writeSeparator();
            }

            if (req.getLocale() != null)
            {
               writer.writeObjectKey(JSON.REQUEST_LOCALE);
               writer.writeString(req.getLocale().toString());
               writer.writeSeparator();
            }

            if (req.getTimeZone() != null)
            {
               writer.writeObjectKey(JSON.REQUEST_TIMEZONE);
               writer.writeString(req.getTimeZone().getID());
               writer.writeSeparator();
            }

            if (req.getCorrelator() != null)
            {
               writer.writeObjectKey(JSON.REQUEST_CORRELATOR);
               msh.marshal(req.getCorrelator());
               writer.writeSeparator();
            }

            writer.writeObjectKey(JSON.REQUEST);
            writer.openArray();

            for (int i = 0, n = req.getInvocationCount(); i < n; ++i)
            {
               Request.Invocation invocation = req.getInvocation(i);

               if (i != 0)
               {
                  writer.writeSeparator();
               }

               writer.openObject();
               writer.writeObjectKey(JSON.INVOCATION_OBJECT);
               msh.marshal(invocation.getObject());

               if (invocation.getEventName() != null)
               {
                  writer.writeSeparator();
                  writer.writeObjectKey(JSON.INVOCATION_EVENT);
                  msh.marshal(invocation.getEventName());
               }

               Object[] argArray = invocation.getArguments();

               if (argArray != null)
               {
                  writer.writeSeparator();
                  writer.writeObjectKey(JSON.INVOCATION_ARGUMENTS);
                  writer.openArray();

                  for (int k = 0; k < argArray.length; ++k)
                  {
                     if (k != 0)
                     {
                        writer.writeSeparator();
                     }

                     msh.marshal(argArray[k]);
                  }

                  writer.closeArray();
               }

               if (invocation.getAttributes() != null)
               {
                  writer.writeSeparator();
                  writer.writeObjectKey(JSON.INVOCATION_ATTRIBUTES);
                  msh.marshal(invocation.getAttributes());
               }

               writer.closeObject();
            }

            writer.closeArray();

            int n = req.getFilterCount();

            if (n != 0)
            {
               writer.writeSeparator();
               writer.writeObjectKey(JSON.REQUEST_FILTERS);
               writer.openArray();

               for (int i = 0; i < n; ++i)
               {
                  if (i != 0)
                  {
                     writer.writeSeparator();
                  }

                  msh.marshal(req.getFilter(i));
               }

               writer.closeArray();
            }

            writer.closeObject();
         }
      });

      s_mshMap.put(Response.class, new Marshaller()
      {
         public void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException
         {
            Response resp = (Response)obj;

            writer.openObject();
            writer.writeObjectKey(JSON.RESPONSE_RESULTS);

            int n = resp.getResultCount();

            if (n == 0)
            {
               writer.writeNull();
            }
            else
            {
               writer.openArray();
               msh.marshal(resp.getResult(0));

               for (int i = 1; i < n; ++i)
               {
                  writer.writeSeparator();
                  msh.marshal(resp.getResult(i));
               }

               writer.closeArray();
            }

            writer.writeSeparator();

            writer.writeObjectKey(JSON.RESPONSE_EVENTS);
            n = resp.getEventCount();

            if (n == 0)
            {
               writer.writeNull();
            }
            else
            {
               writer.openArray();
               msh.marshal(resp.getEvent(0));

               for (int i = 1; i < n; ++i)
               {
                  writer.writeSeparator();
                  msh.marshal(resp.getEvent(i));
               }

               writer.closeArray();
            }

            writer.closeObject();
         }
      });
   }

   // associations

   /**
    * The object reference map.
    */
   protected Lookup m_objectMap;

   /**
    * The character stream writer.
    */
   protected JSONWriter m_writer;

   /**
    * The runtime context.
    */
   protected Context m_context;

   // constructors

   /**
    * Creates a JSON marshaller with a given runtime context.
    *
    * @param context The runtime context.
    */
   public JSONMarshaller(Context context)
   {
      m_context = context;
   }

   // operations

   /**
    * Sets the runtime context.
    *
    * @param context The context to set.
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
    * Marshals an object to the output stream.
    *
    * @param obj The object to marshal.
    */
   protected void marshal(Object obj) throws IOException
   {
      if (obj == null)
      {
         m_writer.writeNull();

         return;
      }

      Marshaller msh = (Marshaller)s_mshMap.get(obj.getClass());

      if (msh == null)
      {
         if (obj instanceof Throwable)
         {
            msh = EXCEPTION_MSH;
         }
         else
         {
            throw new JSONMarshallerException("err.rpc.mshType", new Object[] { obj.getClass().getName() });
         }
      }

      msh.marshal(obj, this, m_writer);
   }

   /**
    * Serializes an object to a character stream.
    *
    * @param obj The object to serialize.
    * @param writer The character stream writer.
    */
   public void serialize(Object obj, Writer writer) throws IOException, MarshallerException
   {
      m_writer = (writer instanceof JSONWriter) ? (JSONWriter)writer : new JSONWriter(writer);
      m_objectMap = new IdentityHashTab();

      marshal(obj);

      // clear memory
      m_objectMap = null;
   }

   /**
    * Interface implemented by JSON marshallers.
    */
   protected interface Marshaller
   {
      /**
       * Marshals an object of a specific type to the output stream.
       *
       * @param obj The object to marshal.
       * @param msh The JSON marshaller.
       * @param writer The JSON writer
       * @throws IOException If an I/O error occurs.
       */
      void marshal(Object obj, JSONMarshaller msh, JSONWriter writer) throws IOException;
   }
}