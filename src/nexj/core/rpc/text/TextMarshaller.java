// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import java.io.ByteArrayInputStream;
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
import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.ErrorLocationHolder;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Context;
import nexj.core.runtime.ContextAware;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.AttributeObject;
import nexj.core.scripting.object.BasicMetaclassObject;
import nexj.core.scripting.object.BasicObject;
import nexj.core.scripting.object.ClassObject;
import nexj.core.scripting.object.ObjectException;
import nexj.core.scripting.object.ObjectOriented;
import nexj.core.scripting.object.TypedAttributeObject;
import nexj.core.scripting.object.TypedBasicObject;
import nexj.core.scripting.object.TypedObjectException;
import nexj.core.util.Base64Util;
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
 * Text format object marshaller.
 */
public class TextMarshaller implements CharacterStreamMarshaller, ContextAware
{
   // constants

   /**
    * The exception marshaller.
    */
   protected final static Marshaller EXCEPTION_MSH = new Marshaller()
   {
      public void marshal(Object obj, TextMarshaller msh) throws IOException
      {
         Throwable t = (Throwable)obj;

         msh.write(Text.EXCEPTION);

         if (obj instanceof ErrorCode)
         {
            ErrorCode e = (ErrorCode)obj;

            msh.marshal(e.getErrorCode());
            msh.marshal((msh.getContext() != null)
               ? msh.getContext().formatString(e.getErrorCode(), e.getErrorArgs())
               : t.getLocalizedMessage());

            marshalErrorArgs(msh, e);
         }
         else
         {
            msh.marshal("err." + t.getClass().getName());
            msh.marshal(t.getLocalizedMessage());
            msh.write(Text.NULL);
         }

         if (obj instanceof ErrorLocationHolder)
         {
            ErrorLocationHolder h = (ErrorLocationHolder)obj;

            msh.marshal(h.getClassName());
            msh.marshal((h.getOIDHolder() != null) ? h.getOIDHolder().getOID() : null);
            msh.marshal(h.getOrdinal());
            msh.writePrefix(h.getAttributeCount() << 1, Text.SEQUENCE);

            for (Iterator itr = h.getAttributeIterator(); itr.hasNext();)
            {
               String sAttribute = (String)itr.next();

               msh.marshalObj(sAttribute);
               msh.marshal(h.findException(sAttribute));
            }
         }
         else
         {
            msh.write(Text.NULL);
            msh.write(Text.NULL);
            msh.marshal(-1);
            msh.write(Text.NULL);
         }

         if (obj instanceof ExceptionHolder)
         {
            ExceptionHolder h = (ExceptionHolder)obj;
            int nCount = 0;

            for (Iterator itr = h.getExceptionIterator(); itr.hasNext();)
            {
               if (!ObjUtil.isSystem((Throwable)itr.next()))
               {
                  ++nCount;
               }
            }

            msh.writePrefix(nCount, Text.SEQUENCE);

            for (Iterator itr = h.getExceptionIterator(); nCount != 0;)
            {
               Throwable cause = (Throwable)itr.next();

               if (!ObjUtil.isSystem(cause))
               {
                  msh.marshal(cause);
                  --nCount;
               }
            }
         }
         else
         {
            Throwable cause = t.getCause();

            if (cause == null || ObjUtil.isSystem(cause))
            {
               msh.write(Text.NULL);
            }
            else
            {
               msh.writePrefix(1, Text.SEQUENCE);
               msh.marshal(cause);
            }
         }
      }

      public boolean isPrimitive()
      {
         return false;
      }
   };

   /**
    * The marshaller map, class to marshaller implementation.
    */
   protected final static Lookup s_mshMap = new HashTab(64); // of type Marshaller[Class]

   static
   {
      s_mshMap.put(Integer.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.m_buffer.append(((Integer)obj).intValue());
            msh.writePrefix(msh.m_buffer.length(), Text.INTEGER);
            msh.writeBuffer();
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(Long.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.m_buffer.append(((Long)obj).longValue());
            msh.writePrefix(msh.m_buffer.length(), Text.LONG);
            msh.writeBuffer();
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(Float.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.m_buffer.append(((Float)obj).floatValue());
            msh.writePrefix(msh.m_buffer.length(), Text.FLOAT);
            msh.writeBuffer();
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(Double.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.m_buffer.append(((Double)obj).doubleValue());
            msh.writePrefix(msh.m_buffer.length(), Text.DOUBLE);
            msh.writeBuffer();
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(BigDecimal.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = obj.toString();
            msh.writePrefix(s.length(), Text.DECIMAL);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(java.util.Date.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.m_buffer.append(((java.util.Date)obj).getTime());
            msh.writePrefix(msh.m_buffer.length(), Text.TIMESTAMP);
            msh.writeBuffer();
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(java.sql.Date.class, s_mshMap.get(java.util.Date.class));
      s_mshMap.put(java.sql.Time.class, s_mshMap.get(java.util.Date.class));
      s_mshMap.put(java.sql.Timestamp.class, s_mshMap.get(java.util.Date.class));

      s_mshMap.put(Boolean.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.writePrefix((((Boolean)obj).booleanValue()) ? 1 : 0, Text.BOOLEAN);
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(String.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = (String)obj;
            msh.writePrefix(s.length(), Text.STRING);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(StringId.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = ((StringId)obj).toString();

            if (s == null)
            {
               s = "";
            }

            msh.writePrefix(s.length(), Text.STRING_ID);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(Character.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            msh.writePrefix(1, Text.CHARACTER);
            msh.write(((Character)obj).charValue());
         }

         public boolean isPrimitive()
         {
            return true;
         }
      });

      s_mshMap.put(Binary.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            Binary bin = (Binary)obj;

            msh.writePrefix(((bin.getData().length + 2) / 3) << 2, Text.BINARY);
            msh.write(bin.getData());
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(PagedBinary.class, s_mshMap.get(Binary.class));

      s_mshMap.put(Locale.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = obj.toString();
            msh.writePrefix(s.length(), Text.LOCALE);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(TimeZone.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = ((TimeZone)obj).getID();
            msh.writePrefix(s.length(), Text.TIME_ZONE);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(TZ.UTC.getClass(), s_mshMap.get(TimeZone.class));
      s_mshMap.put(SimpleTimeZone.class, s_mshMap.get(TimeZone.class));

      s_mshMap.put(ArrayList.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            ArrayList list = (ArrayList)obj;
            int n = list.size();

            msh.writePrefix(n, Text.ARRAY);

            for (int i = 0; i < n; ++i)
            {
               msh.marshal(list.get(i));
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(PagedArrayList.class, s_mshMap.get(ArrayList.class));

      s_mshMap.put(TransferObject.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            TransferObject tobj = (TransferObject)obj;

            msh.writePrefix(tobj.getValueCount(), Text.TRANSFER_OBJECT);
            msh.marshalObj(tobj.getClassName());
            msh.marshalObj(tobj.getEventName());
            msh.marshal(tobj.getVersion());
            msh.marshal(tobj.getOID());

            for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
            {
               msh.marshalObj((String)itr.next());
               msh.marshal(itr.getValue());
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(BasicObject.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            ObjectOriented bobj = (ObjectOriented)obj;
            ClassObject classObj = bobj.getClassObject();
            Machine machine = (msh.m_context == null) ? null : msh.m_context.getMachine();
            int nAttrOffset = classObj.getMetaclass().resolveAttributeOffset(BasicMetaclassObject.MARSHALLED_P);

            if (nAttrOffset < 0 || classObj.getValue(nAttrOffset, machine) != Boolean.TRUE)
            {
               throw new TextMarshallerException("err.rpc.mshObject",
                  new Object[]{classObj, BasicMetaclassObject.MARSHALLED_P});
            }

            AttributeObject[] attributeArray = classObj.resolveAttributeArray();
            int nCount = attributeArray.length;

            msh.writePrefix(nCount, Text.BASIC_OBJECT);

            if (bobj instanceof ObjectException)
            {
               msh.marshal(Boolean.TRUE);
            }

            msh.marshal(classObj.getSymbol());

            if (bobj instanceof ObjectException)
            {
               ObjectException e = (ObjectException)bobj;

               msh.marshal(e.getErrorCode());
               marshalErrorArgs(msh, e);
            }

            msh.marshal(attributeArray);

            for (int i = 0; i < nCount; i++)
            {
               Object value = bobj.getValue(i, machine);

               if (value == null)
               {
                  AttributeObject attr = attributeArray[i];

                  if (attr instanceof TypedAttributeObject && ((TypedAttributeObject)attr).isRequired())
                  {
                     throw new TextMarshallerException("err.rpc.mshRequiredAttribute",
                        new Object[]{attr.getName(), classObj});
                  }
               }

               msh.marshal(value);
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(TypedBasicObject.class, s_mshMap.get(BasicObject.class));
      s_mshMap.put(ObjectException.class, s_mshMap.get(BasicObject.class));
      s_mshMap.put(TypedObjectException.class, s_mshMap.get(BasicObject.class));

      s_mshMap.put(AttributeObject[].class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            AttributeObject[] attrArray = (AttributeObject[])obj;
            int n = attrArray.length;

            msh.writePrefix(n, Text.VECTOR);

            for (int i = 0; i < attrArray.length; i++)
            {
               msh.marshal(attrArray[i].getSymbol());
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(OID.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            OID oid = (OID)obj;

            msh.write(Text.OID);

            int n = oid.getCount();

            msh.writePrefix(n, Text.SEQUENCE);

            for (int i = 0; i < n; ++i)
            {
               msh.marshal(oid.getValue(i));
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(Symbol.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = ((Symbol)obj).getName();

            msh.writePrefix(s.length(), Text.SYMBOL);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(Pair.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            Pair pair = (Pair)obj;

            msh.write(Text.PAIR);
            msh.marshal(pair.getHead());
            msh.marshal(pair.getTail());
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(ConstPair.class, s_mshMap.get(Pair.class));

      s_mshMap.put(byte[].class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            byte[] nArray = (byte[])obj;

            msh.writePrefix(((nArray.length + 2) / 3) << 2, Text.BVECTOR);
            msh.write(nArray);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(char[].class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            char[] cbuf = (char[])obj;
            int n = cbuf.length;

            msh.writePrefix(n, Text.CVECTOR);

            // Replace NUL with \uFFFF to work around the IE XMLHTTP zstring bug
            for (int i = 0; i < n; ++i)
            {
               char ch = cbuf[i];

               msh.write((ch == 0) ? '\uFFFF' : ch);
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(String[].class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String[] array = (String[])obj;
            int n = array.length;

            msh.writePrefix(n, Text.SVECTOR);

            for (int i = 0; i < n; ++i)
            {
               msh.marshal(array[i]);
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(Object[].class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            Object[] array = (Object[])obj;
            int n = array.length;

            msh.writePrefix(n, Text.VECTOR);

            for (int i = 0; i < n; ++i)
            {
               msh.marshal(array[i]);
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(PrivilegeSet.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            byte[] mask = ((PrivilegeSet)obj).getMask();

            msh.writePrefix(mask.length << 1, Text.PRIVILEGE_SET);
            Binary.write(msh.m_writer, mask, 0, mask.length);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(PCodeFunction.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            PCodeFunction fun = ((PCodeFunction)obj).getCleanCopy();

            msh.write(Text.FUNCTION);
            msh.marshal(fun.code);
            msh.marshal(fun.constants);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(PCodeMacro.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            PCodeFunction fun = ((PCodeFunction)obj).getCleanCopy();

            msh.write(Text.MACRO);
            msh.marshal(fun.code);
            msh.marshal(fun.constants);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(Request.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            Request req = (Request)obj;
            int nVersion = msh.m_nVersion;
            int nCount = req.getInvocationCount();

            msh.writePrefix((nVersion >= 4) ? nCount : 0, Text.REQUEST);
            msh.marshal(req.getNamespace());
            msh.marshal(req.getVersion());
            msh.marshal(req.isAsync());
            msh.marshal(req.isCommit());
            msh.marshal(req.getLocale());

            if (nVersion >= 2)
            {
               msh.marshal(req.getTimeZone());
            }

            msh.marshal(req.getCorrelator());

            boolean bParameters = false;
            boolean bAttributes = false;

            if (nVersion <= 3)
            {
               msh.writePrefix(nCount, Text.SEQUENCE);
            }

            for (int i = 0; i < nCount; ++i)
            {
               Request.Invocation invocation = req.getInvocation(i);

               msh.marshal(invocation.getObject());
               
               if (nVersion <= 3)
               {
                  if (!bParameters)
                  {
                     bParameters = (invocation.getArguments() != null);
                  }

                  if (!bAttributes)
                  {
                     bAttributes = (invocation.getAttributes() != null);
                  }
               }
               else
               {
                  msh.marshalObj(invocation.getEventName());
                  msh.marshal(invocation.getArguments());
                  msh.marshal(invocation.getAttributes());
               }
            }

            if (nVersion <= 3)
            {
               if (nVersion == 3)
               {
                  if (bParameters)
                  {
                     msh.writePrefix(nCount, Text.SEQUENCE);

                     for (int i = 0; i < nCount; ++i)
                     {
                        msh.marshal(req.getInvocation(i).getArguments());
                     }
                  }
                  else
                  {
                     msh.writePrefix(0, Text.NULL);
                  }
               }

               if (bAttributes)
               {
                  msh.writePrefix(nCount, Text.SEQUENCE);

                  for (int i = 0; i < nCount; ++i)
                  {
                     msh.marshal(req.getInvocation(i).getAttributes());
                  }
               }
               else
               {
                  msh.writePrefix(0, Text.NULL);
               }
            }

            nCount = req.getFilterCount();
            msh.writePrefix(nCount, Text.SEQUENCE);

            for (int i = 0; i < nCount; ++i)
            {
               msh.marshal(req.getFilter(i));
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });

      s_mshMap.put(Response.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            Response resp = (Response)obj;

            msh.write(Text.RESPONSE);

            int n = resp.getResultCount();

            msh.writePrefix(n, Text.SEQUENCE);

            for (int i = 0; i < n; ++i)
            {
               msh.marshal(resp.getResult(i));
            }

            n = resp.getEventCount();
            msh.writePrefix(n, Text.SEQUENCE);

            for (int i = 0; i < n; ++i)
            {
               msh.marshal(resp.getEvent(i));
            }
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });
   }

   // attributes

   /**
    * The serialization format version.
    */
   protected int m_nVersion = 4;

   // associations

   /**
    * The working string buffer.
    */
   protected StringBuilder m_buffer = new StringBuilder(32);

   /**
    * The number conversion buffer.
    */
   protected char[] m_cvtBuf = new char[10];

   /**
    * The object class to marshaller map: Marshaller[Class].
    */
   protected Lookup m_mshMap;

   /**
    * The object reference map.
    */
   protected Lookup m_objectMap;

   /**
    * The character stream writer.
    */
   protected Writer m_writer;

   /**
    * The Base64 input stream.
    */
   protected Base64InputStream m_istream64;

   /**
    * The runtime context.
    */
   protected Context m_context;

   // constructors

   /**
    * Creates a text marshaller with a given runtime context.
    * @param context The runtime context.
    */
   public TextMarshaller(Context context)
   {
      m_context = context;
   }

   // operations

   /**
    * Sets the runtime context.
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
    * Sets the serialization format version.
    * @param nVersion The serialization format version to set.
    */
   public void setVersion(int nVersion)
   {
      m_nVersion = nVersion;
   }

   /**
    * @return The serialization format version.
    */
   public int getVersion()
   {
      return m_nVersion;
   }

   /**
    * Writes a character to the output stream.
    * @param ch The character to write.
    */
   protected void write(char ch) throws IOException
   {
      m_writer.write(ch);
   }

   /**
    * Writes a string to the output stream.
    * @param s The string to write.
    */
   protected void write(String s) throws IOException
   {
      m_writer.write(s);
   }

   /**
    * Writes a Base64-encoded byte array to the output stream.
    * @param data The array to write.
    */
   protected void write(byte[] data) throws IOException
   {
      if (m_istream64 == null)
      {
         m_istream64 = new Base64InputStream(data);
      }
      else
      {
         m_istream64.setData(data);
      }

      Base64Util.encode(m_istream64, m_writer, -1, false);
      m_istream64.detach();
   }

   /**
    * Writes a count and type prefix to the output stream.
    * @param nCount The count prefix. Must be non-negative.
    * @param chType The type prefix.
    */
   protected void writePrefix(int nCount, char chType) throws IOException
   {
      String s;

      switch (nCount)
      {
         case 0:
            m_writer.write(chType);
            return;

         case 1:
            s = "1";
            break;

         case 2:
            s = "2";
            break;

         case 3:
            s = "3";
            break;

         case 4:
            s = "4";
            break;

         case 5:
            s = "5";
            break;

         case 6:
            s = "6";
            break;

         case 7:
            s = "7";
            break;

         case 8:
            s = "8";
            break;

         case 9:
            s = "9";
            break;

         case 10:
            s = "10";
            break;

         case 11:
            s = "11";
            break;

         case 12:
            s = "12";
            break;

         case 13:
            s = "13";
            break;

         case 14:
            s = "14";
            break;

         case 15:
            s = "15";
            break;

         case 16:
            s = "16";
            break;

         case 17:
            s = "17";
            break;

         case 18:
            s = "18";
            break;

         case 19:
            s = "19";
            break;

         case 20:
            s = "20";
            break;

         case 21:
            s = "21";
            break;

         case 22:
            s = "22";
            break;

         case 23:
            s = "23";
            break;

         case 24:
            s = "24";
            break;

         case 25:
            s = "25";
            break;

         case 26:
            s = "26";
            break;

         case 27:
            s = "27";
            break;

         case 28:
            s = "28";
            break;

         case 29:
            s = "29";
            break;

         case 30:
            s = "30";
            break;

         case 31:
            s = "31";
            break;

         case 32:
            s = "32";
            break;

         default:
            int i = 0;

            do
            {
               m_cvtBuf[i++] = (char)(nCount % 10 + '0');
               nCount /= 10;
            }
            while (nCount != 0);

            while (i != 0)
            {
               m_writer.write(m_cvtBuf[--i]);
            }

            m_writer.write(chType);

            return;
      }

      m_writer.write(s);
      m_writer.write(chType);
   }

   /**
    * Writes the work buffer contents to the output stream.
    */
   protected void writeBuffer() throws IOException
   {
      int n = m_buffer.length();

      for (int i = 0; i < n; ++i)
      {
         m_writer.write(m_buffer.charAt(i));
      }

      m_buffer.setLength(0);
   }

   /**
    * Marshals an integer value.
    * @param n The integer value to marshal.
    */
   protected void marshal(int n) throws IOException
   {
      m_buffer.append(n);
      writePrefix(m_buffer.length(), Text.INTEGER);
      writeBuffer();
   }

   /**
    * Marshals a long value.
    * @param l The long value to marshal.
    */
   protected void marshal(long l) throws IOException
   {
      m_buffer.append(l);
      writePrefix(m_buffer.length(), Text.LONG);
      writeBuffer();
   }

   /**
    * Marshals a boolean value.
    * @param b The boolean value to marshal.
    */
   protected void marshal(boolean b) throws IOException
   {
      writePrefix((b) ? 1 : 0, Text.BOOLEAN);
   }

   /**
    * Marshals a string as an object.
    * @param s The string to marshal.
    */
   protected void marshalObj(String s) throws IOException
   {
      if (s == null)
      {
         write(Text.NULL);
         return;
      }

      Integer index = (Integer)m_objectMap.get(s);

      if (index != null)
      {
         writePrefix(index.intValue(), Text.REFERENCE);
         return;
      }

      m_objectMap.put(s, Primitive.createInteger(m_objectMap.size()));

      writePrefix(s.length(), Text.STRING_OBJECT);
      write(s);
   }

   /**
    * Marshals an object to the output stream.
    * @param obj The object to marshal.
    */
   protected void marshal(Object obj) throws IOException
   {
      if (obj == null)
      {
         write(Text.NULL);
         return;
      }

      Marshaller msh = (Marshaller)m_mshMap.get(obj.getClass());

      if (msh == null)
      {
         if (obj instanceof Throwable)
         {
            msh = EXCEPTION_MSH;
         }
         else
         {
            throw new TextMarshallerException("err.rpc.mshType",
               new Object[]{obj.getClass().getName()});
         }
      }

      if (!msh.isPrimitive())
      {
         Integer index = (Integer)m_objectMap.get(obj);

         if (index != null)
         {
            writePrefix(index.intValue(), Text.REFERENCE);
            return;
         }

         m_objectMap.put(obj, Primitive.createInteger(m_objectMap.size()));
      }

      msh.marshal(obj, this);
   }

   /**
    * Serializes an object to a character stream.
    * @param obj The object to serialize.
    * @param writer The character stream writer.
    */
   public void serialize(Object obj, Writer writer) throws IOException, MarshallerException
   {
      m_writer = writer;
      m_mshMap = getMarshallerMap();
      m_objectMap = new IdentityHashTab();
      m_buffer.setLength(0);

      writePrefix(m_nVersion, Text.VERSION);
      marshal(obj);
   }

   /**
    * @return The marshaller map.
    */
   protected Lookup getMarshallerMap()
   {
      return s_mshMap;
   }

   /**
    * Determines if an object is supported by the marshaller.
    */
   public static boolean isSerializable(Object obj)
   {
      return obj == null || obj instanceof Throwable || s_mshMap.contains(obj.getClass());
   }

   /**
    * Marshals the error arguments from the given exception.
    * @param msh The marshaller.
    * @param e The exception containing the arguments to marshal.
    * @throws IOException If an I/O error occurs.
    */
   protected static void marshalErrorArgs(TextMarshaller msh, ErrorCode e) throws IOException
   {
      Object[] args = e.getErrorArgs();

      if (args == null)
      {
         msh.write(Text.NULL);
      }
      else
      {
         msh.writePrefix(args.length, Text.SEQUENCE);

         for (int i = 0; i < args.length; ++i)
         {
            Object arg = args[i];

            if (arg != null)
            {
               if (!msh.m_mshMap.contains(arg.getClass()))
               {
                  if (msh.m_context != null)
                  {
                     arg = msh.m_context.getStringTable().toString(arg);
                  }
                  else
                  {
                     arg = String.valueOf(arg);
                  }
               }
            }

            msh.marshal(arg);
         }
      }
   }

   // inner classes

   /**
    * Interface implemented by text marshallers.
    */
   protected interface Marshaller
   {
      /**
       * Marshals an object of a specific type to the output stream.
       * @param obj The object to marshal.
       * @param msh The text marshaller.
       */
      void marshal(Object obj, TextMarshaller msh) throws IOException;

      /**
       * @return True if the object should not be put in the object table
       */
      boolean isPrimitive();
   }

   /**
    * Binary input stream based on a byte array.
    */
   protected static class Base64InputStream extends ByteArrayInputStream
   {
      /**
       * Creates the input stream.
       * @param data The array that will be used as the stream source.
       */
      public Base64InputStream(byte[] data)
      {
         super(data);
      }

      /**
       * Sets the stream source array.
       * @param data The array to set.
       */
      public void setData(byte[] data)
      {
         this.buf = data;
         this.pos = 0;
         this.count = data.length;
      }

      /**
       * Detaches the source array.
       * @return The stream source array.
       */
      public byte[] detach()
      {
         byte[] data = this.buf;

         this.buf = null;

         return data;
      }
   }
}
