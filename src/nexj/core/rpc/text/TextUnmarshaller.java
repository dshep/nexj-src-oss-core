// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import java.io.IOException;
import java.io.Reader;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
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
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.AttributeObject;
import nexj.core.scripting.object.BasicMetaclassObject;
import nexj.core.scripting.object.ClassObject;
import nexj.core.scripting.object.MemberObject;
import nexj.core.scripting.object.ObjectException;
import nexj.core.scripting.object.ObjectOriented;
import nexj.core.scripting.object.TypedAttributeObject;
import nexj.core.util.Base64Util;
import nexj.core.util.Binary;
import nexj.core.util.DetachableByteArrayOutputStream;
import nexj.core.util.GenericException;
import nexj.core.util.LocaleUtil;
import nexj.core.util.StringId;

/**
 * Text format unmarshaller.
 */
public class TextUnmarshaller implements CharacterStreamUnmarshaller, ContextAware
{
   // attributes

   /**
    * The serialization format version.
    */
   protected int m_nVersion;

   // associations

   /**
    * The unmarshaller array, indexed by type character.
    */
   private final static Unmarshaller[] s_unmshArray = new Unmarshaller[128];

   static
   {
      s_unmshArray[Text.NULL] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return null;
         }
      };

      s_unmshArray[Text.REFERENCE] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return unmsh.m_objectList.get(nCount);
         }
      };

      s_unmshArray[Text.INTEGER] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return Primitive.createInteger(Integer.parseInt(unmsh.read(nCount)));
         }
      };

      s_unmshArray[Text.LONG] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return Primitive.createLong(Long.parseLong(unmsh.read(nCount)));
         }
      };

      s_unmshArray[Text.FLOAT] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return Primitive.createFloat(Float.parseFloat(unmsh.read(nCount)));
         }
      };

      s_unmshArray[Text.DOUBLE] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return Primitive.createDouble(Double.parseDouble(unmsh.read(nCount)));
         }
      };

      s_unmshArray[Text.DECIMAL] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return new BigDecimal(unmsh.read(nCount));
         }
      };

      s_unmshArray[Text.TIMESTAMP] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return new Timestamp(Long.parseLong(unmsh.read(nCount)));
         }
      };

      s_unmshArray[Text.BOOLEAN] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return Boolean.valueOf(nCount != 0);
         }
      };

      s_unmshArray[Text.STRING] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return unmsh.read(nCount);
         }
      };

      s_unmshArray[Text.STRING_OBJECT] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            String s = unmsh.read(nCount);

            unmsh.addObj(s);

            return s;
         }
      };

      s_unmshArray[Text.STRING_ID] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller msh) throws IOException
         {
            StringId id = new StringId(msh.read(nCount));

            msh.addObj(id);

            return id;
         }
      };

      s_unmshArray[Text.CHARACTER] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            return Primitive.createCharacter(unmsh.read());
         }
      };

      s_unmshArray[Text.BINARY] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Binary bin = new Binary(unmsh.readBase64(nCount));

            unmsh.addObj(bin);

            return bin;
         }
      };

      s_unmshArray[Text.LOCALE] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Locale locale = LocaleUtil.parse(unmsh.read(nCount));

            unmsh.addObj(locale);

            return locale;
         }
      };

      s_unmshArray[Text.TIME_ZONE] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            TimeZone timeZone = TimeZone.getTimeZone(unmsh.read(nCount));

            unmsh.addObj(timeZone);

            return timeZone;
         }
      };

      s_unmshArray[Text.ARRAY] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            ArrayList list = new ArrayList(nCount);

            unmsh.addObj(list);

            for (int i = 0; i < nCount; ++i)
            {
               list.add(unmsh.unmarshal());
            }

            return list;
         }
      };

      s_unmshArray[Text.SEQUENCE] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller msh) throws IOException
         {
            ArrayList list = new ArrayList(nCount);

            for (int i = 0; i < nCount; ++i)
            {
               list.add(msh.unmarshal());
            }

            return list;
         }
      };

      s_unmshArray[Text.TRANSFER_OBJECT] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            TransferObject tobj = new TransferObject(nCount - 4); // -4 for non-setValue() objects

            unmsh.addObj(tobj);

            tobj.setClassName((String)unmsh.unmarshal());
            tobj.setEventName((String)unmsh.unmarshal());
            tobj.setVersion((short)unmsh.unmarshalLong());

            OID oid = (OID)unmsh.unmarshal();

            if (oid != null)
            {
               tobj.setOID(oid);
            }

            String sKey;

            for (int i = 0; i < nCount; ++i)
            {
               sKey = (String)unmsh.unmarshal();
               tobj.setValue(sKey, unmsh.unmarshal());
            }

            return tobj;
         }
      };

      s_unmshArray[Text.BASIC_OBJECT] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            int nObjIndex = unmsh.m_objectList.size();

            unmsh.addObj(null);

            Object datum = unmsh.unmarshal();
            boolean bException = false;
            Symbol className;

            if (datum instanceof Boolean)
            {
               bException = ((Boolean)datum).booleanValue();
               className = (Symbol)unmsh.unmarshal();
            }
            else
            {
               className = (Symbol)datum;
            }

            Machine machine = unmsh.m_context.getMachine();
            ClassObject classObj = machine.getGlobalEnvironment().findClass(className);

            if (classObj == null)
            {
               throw new TextMarshallerException("err.rpc.mshClass",
                  new Object[]{className});
            }

            MemberObject member = classObj.getMetaclass().resolveMember(BasicMetaclassObject.MARSHALLED_P, 0);

            if (member == null || !Intrinsic.isTrue(machine.invoke(classObj, BasicMetaclassObject.MARSHALLED_P, (Object[])null)))
            {
               throw new TextUnmarshallerException("err.rpc.mshObject",
                  new Object[]{classObj, BasicMetaclassObject.MARSHALLED_P});
            }

            ObjectOriented bobj = classObj.createObject();

            if (bException)
            {
               String sErrorCode = (String)unmsh.unmarshal();
               int nArgCount = unmsh.readPrefix(Text.SEQUENCE);
               Object[] argArray = (nArgCount == 0) ? null : new Object[nArgCount];

               for (int i = 0; i < nArgCount; ++i)
               {
                  argArray[i] = unmsh.unmarshal();
               }

               ((ObjectException)bobj).init(sErrorCode, argArray, null);
               ((ObjectException)bobj).setStackTrace(new StackTraceElement[0]);
            }

            unmsh.m_objectList.set(nObjIndex, bobj);

            Object[] symArray = (Object[])unmsh.unmarshal();

            for (int i = 0; i < nCount; i++)
            {
               Object value = unmsh.unmarshal();
               Symbol attrSym = (Symbol)symArray[i];
               int nAttr = classObj.resolveAttributeOffset(attrSym);

               if (nAttr < 0)
               {
                  throw new TextUnmarshallerException("err.rpc.mshAttribute",
                     new Object[]{attrSym, classObj});
               }

               if (value == null)
               {
                  AttributeObject attr = classObj.resolveAttribute(nAttr);

                  if (attr instanceof TypedAttributeObject && ((TypedAttributeObject)attr).isRequired())
                  {
                     throw new TextMarshallerException("err.rpc.mshRequiredAttribute",
                        new Object[]{attr.getName(), classObj});
                  }
               }

               bobj.setValue(nAttr, value, machine);
            }

            return bobj;
         }
      };

      s_unmshArray[Text.OID] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            OID oid = new OID(null);

            unmsh.addObj(oid);
            nCount = unmsh.readPrefix(Text.SEQUENCE);

            Object[] valueArray = new Object[nCount];

            for (int i = 0; i < nCount; ++i)
            {
               valueArray[i] = unmsh.unmarshal();
            }

            oid.setValueArray(valueArray);

            return oid;
         }
      };

      s_unmshArray[Text.SYMBOL] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Symbol sym = Symbol.define(unmsh.read(nCount));

            unmsh.addObj(sym);

            return sym;
         }
      };

      s_unmshArray[Text.PAIR] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Pair pair = new Pair(null);

            unmsh.addObj(pair);

            pair.setHead(unmsh.unmarshal());
            pair.setTail(unmsh.unmarshal());

            return pair;
         }
      };

      s_unmshArray[Text.BVECTOR] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            byte[] nArray = unmsh.readBase64(nCount);

            unmsh.addObj(nArray);

            return nArray;
         }
      };

      s_unmshArray[Text.CVECTOR] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            char[] cbuf = new char[nCount];

            unmsh.addObj(cbuf);
            unmsh.read(cbuf, 0, cbuf.length);

            for (int i = 0; i < nCount; ++i)
            {
               if (cbuf[i] == '\uFFFF')
               {
                  cbuf[i] = 0;
               }
            }

            return cbuf;
         }
      };

      s_unmshArray[Text.SVECTOR] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            String[] array = new String[nCount];

            unmsh.addObj(array);

            for (int i = 0; i < nCount; ++i)
            {
               array[i] = (String)unmsh.unmarshal();
            }

            return array;
         }
      };

      s_unmshArray[Text.VECTOR] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Object[] array = new Object[nCount];

            unmsh.addObj(array);

            for (int i = 0; i < nCount; ++i)
            {
               array[i] = unmsh.unmarshal();
            }

            return array;
         }
      };

      s_unmshArray[Text.PRIVILEGE_SET] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            byte[] mask = new byte[(nCount + 1) >> 1];

            if (Binary.read(unmsh.m_reader, mask, 0, nCount) != nCount)
            {
               throw new TextUnmarshallerException("err.rpc.unmshEOF");
            }

            PrivilegeSet privilegeSet = new PrivilegeSet(mask);

            unmsh.addObj(privilegeSet);

            return privilegeSet;
         }
      };

      s_unmshArray[Text.FUNCTION] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            PCodeFunction fun = new PCodeFunction();

            unmsh.addObj(fun);
            fun.code = (char[])unmsh.unmarshal();
            fun.constants = (Object[])unmsh.unmarshal();

            return fun;
         }
      };

      s_unmshArray[Text.MACRO] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            PCodeFunction fun = new PCodeMacro();

            unmsh.addObj(fun);
            fun.code = (char[])unmsh.unmarshal();
            fun.constants = (Object[])unmsh.unmarshal();

            return fun;
         }
      };

      s_unmshArray[Text.REQUEST] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Request req = new Request();
            int nVersion = unmsh.m_nVersion;

            unmsh.addObj(req);

            req.setNamespace((String)unmsh.unmarshal());
            req.setVersion((String)unmsh.unmarshal());
            req.setAsync(unmsh.unmarshalBoolean());
            req.setCommit(unmsh.unmarshalBoolean());
            req.setLocale((Locale)unmsh.unmarshal());

            if (nVersion >= 2)
            {
               req.setTimeZone((TimeZone)unmsh.unmarshal());
            }

            req.setCorrelator((TransferObject)unmsh.unmarshal());

            if (nVersion <= 3)
            {
               nCount = unmsh.readPrefix(Text.SEQUENCE);
            }

            for (int i = 0; i < nCount; ++i)
            {
               TransferObject tobj = (TransferObject)unmsh.unmarshal();

               if (nVersion <= 3)
               {
                  req.addInvocation(tobj);
               }
               else
               {
                  String sEventName = (String)unmsh.unmarshal();
                  Object args = unmsh.unmarshal();
                  Pair attributes = (Pair)unmsh.unmarshal();

                  req.addInvocation(tobj, sEventName, (args instanceof Collection) ?
                     ((Collection)args).toArray() : (Object[])args, attributes);
               }
            }

            if (nVersion <= 3)
            {
               if (nVersion == 3)
               {
                  nCount = unmsh.readPrefix(Text.SEQUENCE);

                  for (int i = 0; i < nCount; ++i)
                  {
                     Object args = unmsh.unmarshal();

                     req.getInvocation(i).setArguments((args instanceof Collection) ?
                        ((Collection)args).toArray() : (Object[])args);
                  }
               }

               nCount = unmsh.readPrefix(Text.SEQUENCE);

               for (int i = 0; i < nCount; ++i)
               {
                  req.getInvocation(i).setAttributes((Pair)unmsh.unmarshal());
               }
            }

            nCount = unmsh.readPrefix(Text.SEQUENCE);

            for (int i = 0; i < nCount; ++i)
            {
               req.addFilter((TransferObject)unmsh.unmarshal());
            }

            return req;
         }
      };

      s_unmshArray[Text.RESPONSE] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Response resp = new Response();

            unmsh.addObj(resp);

            nCount = unmsh.readPrefix(Text.SEQUENCE);

            for (int i = 0; i < nCount; ++i)
            {
               resp.addResult(unmsh.unmarshal());
            }

            nCount = unmsh.readPrefix(Text.SEQUENCE);

            for (int i = 0; i < nCount; ++i)
            {
               resp.addEvent((Collection)unmsh.unmarshal());
            }

            return resp;
         }
      };

      s_unmshArray[Text.EXCEPTION] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            int nObjIndex = unmsh.m_objectList.size();

            unmsh.addObj(null);

            String sErrCode = (String)unmsh.unmarshal();
            String sMessage = (String)unmsh.unmarshal();
            int nArgCount = unmsh.readPrefix(Text.SEQUENCE);
            Object[] argArray = (nArgCount == 0) ? null : new Object[nArgCount];

            for (int i = 0; i < nArgCount; ++i)
            {
               argArray[i] = unmsh.unmarshal();
            }

            String sClassName = (String)unmsh.unmarshal();

            GenericException e;

            if (sClassName != null)
            {
               e = new ValidationException(sErrCode, argArray);
            }
            else if (nArgCount != 0)
            {
               e = new GenericException(sErrCode, argArray);
            }
            else
            {
               e = new GenericException(sErrCode, new Object[]{sMessage});
            }

            e.setStackTrace(new StackTraceElement[0]);
            unmsh.m_objectList.set(nObjIndex, e);

            OID oid = (OID)unmsh.unmarshal();
            int nOrdinal = (int)unmsh.unmarshalLong();
            int nAttrCount = unmsh.readPrefix(Text.SEQUENCE);

            if (sClassName != null)
            {
               ValidationException x = (ValidationException)e;

               x.setClassName(sClassName);
               x.setOIDHolder(oid);
               x.setOrdinal(nOrdinal);
            }

            for (int i = 0; i < nAttrCount; i += 2)
            {
               String sName = (String)unmsh.unmarshal();

               ((ValidationException)e).addException(sName, (Throwable)unmsh.unmarshal());
            }

            int nExcCount = unmsh.readPrefix(Text.SEQUENCE);

            for (int i = 0; i < nExcCount; ++i)
            {
               e.addException((Throwable)unmsh.unmarshal());
            }

            return e;
         }
      };

      s_unmshArray[Text.EXPRESSION] = new Unmarshaller()
      {
         public Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException
         {
            Object expr = RPCUtil.parse(new SchemeParser(unmsh.getContext().getMachine().getGlobalEnvironment()),
               unmsh.read(nCount), null);

            unmsh.addObj(expr);

            return expr;
         }
      };
   };

   /**
    * The character stream reader.
    */
   private Reader m_reader;

   /**
    * The working character buffer.
    */
   private char[] m_buffer = new char[32];

   /**
    * The Base64 output stream.
    */
   private DetachableByteArrayOutputStream m_ostream64;

   /**
    * The object reference list.
    */
   private List m_objectList;

   /**
    * The runtime context.
    */
   private Context m_context;

   // constructors

   /**
    * Creates the unmarshaller with a given runtime context.
    * @param context The runtime context.
    */
   public TextUnmarshaller(Context context)
   {
      m_context = context;
   }

   // operations

   /**
    * Sets the runtime context.
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
    * @return The detected serialization format version.
    */
   public int getVersion()
   {
      return m_nVersion;
   }

   /**
    * Reads the specified number of characters from the input stream.
    * @param nCount The number of characters to read.
    * @return The read string.
    */
   protected String read(int nCount) throws IOException
   {
      if (nCount > m_buffer.length)
      {
         m_buffer = new char[Math.max(nCount, m_buffer.length << 1)];
      }

      read(m_buffer, 0, nCount);

      return new String(m_buffer, 0, nCount);
   }

   /**
    * Reads characters until a character buffer is filled.
    * @param cbuf The character buffer, which on return contains the read characters.
    * @param nStart The start offset of the buffer.
    * @param nEnd The end offset of the buffer.
    */
   protected void read(char[] cbuf, int nStart, int nEnd) throws IOException
   {
      while (nStart < nEnd)
      {
         int nCount = m_reader.read(cbuf, nStart, nEnd - nStart);

         if (nCount < 0)
         {
            if (nStart < nEnd)
            {
               throw new TextUnmarshallerException("err.rpc.unmshEOF");
            }

            break;
         }

         nStart += nCount;
      }
   }

   /**
    * Reads a single character.
    * @return The read character.
    */
   protected char read() throws IOException
   {
      int ch = m_reader.read();

      if (ch < 0)
      {
         throw new TextUnmarshallerException("err.rpc.unmshEOF");
      }

      return (char)ch;
   }

   /**
    * Reads the specified number of Base64-encoded characters from the input stream.
    * @param int nCount The number of characters to read.
    * @return The read byte array.
    */
   protected byte[] readBase64(int nCount) throws IOException
   {
      int nLength = ((nCount + 3) >> 2) * 3;

      if (m_ostream64 == null)
      {
         m_ostream64 = new DetachableByteArrayOutputStream(nLength);
      }
      else
      {
         m_ostream64.reset(nLength);
      }

      long lReadCount = Base64Util.decode(m_reader, m_ostream64, nCount, false);

      if (lReadCount != nCount)
      {
         throw new TextUnmarshallerException("err.rpc.base64");
      }

      if (m_ostream64.size() == m_ostream64.length())
      {
         return m_ostream64.detach();
      }

      return m_ostream64.toByteArray();
   }

   /**
    * Reads a prefix count from the input stream.
    * @param chType The prefix data type. Must match this.
    * @return The prefix count value.
    * @throws TextUnmarshallerException if the prefix data type does not match chType.
    */
   protected int readPrefix(char chType) throws IOException, TextUnmarshallerException
   {
      int nCount = 0;
      int ch = m_reader.read();

      while (ch >= '0' && ch <= '9')
      {
         nCount = nCount * 10 + (ch - '0');
         ch = m_reader.read();
      }

      if (ch != chType &&
         (chType != Text.LONG || ch != Text.INTEGER && ch != Text.DOUBLE && ch != Text.DECIMAL && ch != Text.STRING) &&
         (chType != Text.SEQUENCE || ch != Text.NULL || nCount != 0) &&
         (chType != Text.VERSION || ch != Text.VECTOR && ch != Text.EXCEPTION))
      {
         if (ch < 0)
         {
            throw new TextUnmarshallerException("err.rpc.mshEOF");
         }

         if (chType == Text.VERSION)
         {
            throw new TextUnmarshallerException("err.rpc.mshHeader", new Object[]{Primitive.createCharacter(ch)});
         }

         throw new TextUnmarshallerException("err.rpc.text.unexpectedUnmshType",
            new Object[]{Primitive.createCharacter(ch), Primitive.createCharacter(chType)});
      }

      return nCount;
   }

   /**
    * Adds an object at the end of the object list.
    * @param obj The object to add.
    */
   protected void addObj(Object obj)
   {
      m_objectList.add(obj);
   }

   /**
    * Unmarshals a long value from the input stream.
    * @return The unmarshalled long value.
    */
   protected long unmarshalLong() throws IOException
   {
      return Long.parseLong(read(readPrefix(Text.LONG)));
   }

   /**
    * Unmarshals a boolean value from the input stream.
    * @return The unmarshalled boolean value.
    */
   protected boolean unmarshalBoolean() throws IOException
   {
      return readPrefix(Text.BOOLEAN) != 0;
   }

   /**
    * Unmarshals the next available object from the input stream.
    * @return The unmarshalled object.
    */
   protected Object unmarshal() throws IOException
   {
      int nCount = 0;
      int ch = m_reader.read();

      while (ch >= '0' && ch <= '9')
      {
         nCount = nCount * 10 + (ch - '0');
         ch = m_reader.read();
      }

      if (ch < 0 || ch > 127 || s_unmshArray[ch] == null)
      {
         throw new TextUnmarshallerException("err.rpc.unmshType",
            new Object[]{Primitive.createCharacter(ch)});
      }

      return s_unmshArray[ch].unmarshal(nCount, this);
   }

   /**
    * Deserializes an object from a character stream containing a message.
    * @param reader The character stream reader.
    * @return The deserialized object.
    */
   public Object deserialize(Reader reader) throws IOException, UnmarshallerException
   {
      m_reader = reader;
      m_objectList = new ArrayList(256);

      int nVersion = readPrefix(Text.VERSION);

      if (nVersion < 1 || nVersion > 4)
      {
         throw new TextUnmarshallerException("err.rpc.mshVersion",
            new Object[]{Primitive.createInteger(nVersion)});
      }

      // Only set the detected version if it is supported
      m_nVersion = nVersion;

      Object obj = unmarshal();

      m_reader = null; // free memory not used after unmarshal()
      m_objectList = null; // free memory not used after unmarshal()

      return obj;
   }

   // inner classes

   /**
    * Interface implemented by text unmarshallers.
    */
   private interface Unmarshaller
   {
      /**
       * Unmarshals an object of a specific type from the input stream.
       * @param nCount The count parameter of the text to be unmarshalled.
       * @param msh The text unmarshaller.
       * @return The unmarshalled object.
       */
      Object unmarshal(int nCount, TextUnmarshaller unmsh) throws IOException;
   }
}
