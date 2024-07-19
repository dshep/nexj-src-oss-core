// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import org.xml.sax.helpers.DefaultHandler;

import nexj.core.meta.PrivilegeSet;
import nexj.core.persistence.OID;
import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.ErrorLocationHolder;
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
import nexj.core.util.SOAPUtil;
import nexj.core.util.StringId;
import nexj.core.util.XMLWriter;

/**
 * SOAP format marshaller.
 */
public class SOAPMarshaller extends DefaultHandler implements CharacterStreamMarshaller, ContextAware
{
   // constants

   /**
    * Explicitly marshal a null value instead of skipping it.
    */
   public final static int MF_NIL = 0x0001;

   /**
    * Provide type information for the value.
    */
   public final static int MF_TYPE = 0x0002;

   /**
    * Marshals an object as a literal type.
    */
   public final static int MF_LITERAL = 0x0004;

   /**
    * Id conversion buffer length, sufficient to hold max int base 52.
    */
   protected final static int ID_BUF_LENGTH = 6;

   /**
    * The size of the Id fixup entry.
    */
   protected final static int ID_FIXUP_SIZE = 3;

   /**
    * Static id lookup array.
    */
   protected final static String[] s_idArray = new String[1024];

   static
   {
      char buf[] = new char[ID_BUF_LENGTH];

      for (int i = 0; i < s_idArray.length; ++i)
      {
         s_idArray[i] = toId(i, buf);
      }
   }

   protected final static Marshaller STRING_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.m_writer.writeValue((String)obj);
      }

      public String getType()
      {
         return "string";
      }
   };

   protected final static Marshaller BINARY_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         Base64Util.encode(new ByteArrayInputStream(((Binary)obj).getData()), msh.m_writer, -1, false);
      }

      public String getType()
      {
         return "base64Binary";
      }
   };

   protected final static Marshaller INTEGER_MSH = new PrimitiveMarshaller()
   {
      public String getType()
      {
         return "int";
      }
   };

   protected final static Marshaller LONG_MSH = new PrimitiveMarshaller()
   {
      public String getType()
      {
         return "long";
      }
   };

   protected final static Marshaller DECIMAL_MSH = new PrimitiveMarshaller()
   {
      public String getType()
      {
         return "decimal";
      }
   };

   protected final static Marshaller FLOAT_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         String s = obj.toString();

         if (s.charAt(s.length() - 1) == 'y')
         {
            s = (s.charAt(0) == '-') ? "-INF" : "INF";
         }

         msh.m_writer.write(s);
      }

      public String getType()
      {
         return "float";
      }
   };

   protected final static Marshaller DOUBLE_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         String s = obj.toString();

         if (s.charAt(s.length() - 1) == 'y')
         {
            s = (s.charAt(0) == '-') ? "-INF" : "INF";
         }

         msh.m_writer.write(s);
      }

      public String getType()
      {
         return "double";
      }
   };

   protected final static Marshaller TIMESTAMP_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.m_writer.write(SOAPUtil.formatDateTime((Date)obj));
      }

      public String getType()
      {
         return "dateTime";
      }
   };

   protected final static Marshaller BOOLEAN_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.m_writer.write((((Boolean)obj).booleanValue()) ? "true" : "false");
      }

      public String getType()
      {
         return "boolean";
      }
   };

   protected final static Marshaller CHARACTER_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.m_writer.writeInt(((Character)obj).charValue());
      }

      public String getType()
      {
         return "unsignedShort";
      }
   };

   protected final static Marshaller OBJECT_ARRAY_MSH = new ObjectArrayMarshaller(null, 0);

   protected final static Marshaller STRING_ARRAY_MSH = new ObjectArrayMarshaller(STRING_MSH, MF_LITERAL);

   protected final static Marshaller CHAR_ARRAY_MSH = new ArrayMarshaller(CHARACTER_MSH, MF_LITERAL)
   {
      protected int getSize(Object obj)
      {
         return ((char[])obj).length;
      }

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         char[] cbuf = (char[])obj;

         for (int i = 0; i != cbuf.length; ++i)
         {
            msh.marshal(cbuf[i], "", "Item");
         }
      }
   };

   protected final static Marshaller LIST_MSH = new ObjectMarshaller()
   {
      private final Marshaller ITEMS_MSH = new ListMarshaller(null, 0);

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(obj, null, "items", ITEMS_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "Array";
      }

   };

   protected final static Marshaller STRINGID_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((StringId)obj).toString(), null, "id", STRING_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "StringId";
      }
   };

   protected final static Marshaller SYMBOL_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((Symbol)obj).getName(), null, "name", STRING_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "Symbol";
      }
   };

   protected final static Marshaller LOCALE_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((Locale)obj).toString(), null, "name", STRING_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "Locale";
      }
   };

   protected final static Marshaller TIME_ZONE_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((TimeZone)obj).getID(), null, "name", STRING_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "TimeZone";
      }
   };

   protected final static Marshaller PAIR_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         Pair pair = (Pair)obj;

         msh.marshal(pair.getHead(), null, "head", null, MF_TYPE);
         msh.marshal(pair.getTail(), null, "tail", null, MF_TYPE);
      }

      public String getType()
      {
         return "Pair";
      }
   };

   protected final static Marshaller PRIVILEGE_SET_MSH = new ObjectMarshaller()
   {
      private final Marshaller MASK_MSH = new PrimitiveMarshaller()
      {
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Base64Util.encode(new ByteArrayInputStream(((PrivilegeSet)obj).getMask()), msh.m_writer, -1, false);
         }

         public String getType()
         {
            return "base64Binary";
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(obj, null, "mask", MASK_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "PrivilegeSet";
      }
   };

   protected final static Marshaller BYTE_VECTOR_MSH = new ObjectMarshaller()
   {
      private final Marshaller MASK_MSH = new PrimitiveMarshaller()
      {
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Base64Util.encode(new ByteArrayInputStream((byte[])obj), msh.m_writer, -1, false);
         }

         public String getType()
         {
            return "base64Binary";
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(obj, null, "value", MASK_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "ByteVector";
      }
   };

   protected final static Marshaller FUNCTION_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         PCodeFunction fun = ((PCodeFunction)obj).getCleanCopy();

         msh.marshal(fun.code, null, "code", CHAR_ARRAY_MSH, MF_LITERAL);
         msh.marshal(fun.constants, null, "constants", OBJECT_ARRAY_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "Function";
      }
   };

   protected final static Marshaller MACRO_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         PCodeMacro fun = (PCodeMacro)((PCodeMacro)obj).getCleanCopy();

         msh.marshal(fun.code, null, "code", CHAR_ARRAY_MSH, MF_LITERAL);
         msh.marshal(fun.constants, null, "constants", OBJECT_ARRAY_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "Macro";
      }
   };

   protected final static Marshaller OID_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((OID)obj).getValueArray(), null, "values", OBJECT_ARRAY_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "OID";
      }
   };

   protected final static Marshaller TRANSFER_OBJECT_MSH = new ObjectMarshaller()
   {
      private final Marshaller KEYS_MSH = new ArrayMarshaller(STRING_MSH, MF_LITERAL)
      {
         protected int getSize(Object obj)
         {
            return ((TransferObject)obj).getValueCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            TransferObject tobj = (TransferObject)obj;

            for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
            {
               marshalItem(itr.next(), msh);
            }
         }
      };

      private final Marshaller VALUES_MSH = new ArrayMarshaller(null, 0)
      {
         protected int getSize(Object obj)
         {
            return ((TransferObject)obj).getValueCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            TransferObject tobj = (TransferObject)obj;

            for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
            {
               itr.next();
               marshalItem(itr.getValue(), msh);
            }
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         TransferObject tobj = (TransferObject)obj;

         msh.marshal(tobj.getClassName(), null, "class", STRING_MSH, MF_LITERAL);
         msh.marshal(tobj.getEventName(), null, "event", STRING_MSH, MF_LITERAL);
         msh.marshal(tobj.getVersion(), null, "version");
         msh.marshal(tobj.getOID(), null, "oid", OID_MSH, MF_LITERAL);

         if (tobj.getValueCount() != 0)
         {
            msh.marshal(tobj, null, "keys", KEYS_MSH, MF_LITERAL);
            msh.marshal(tobj, null, "values", VALUES_MSH, MF_LITERAL);
         }
      }

      public String getType()
      {
         return "TransferObject";
      }
   };

   protected final static Marshaller REQUEST_MSH = new ObjectMarshaller()
   {
      private final Marshaller INVOCATION_MSH = new ObjectMarshaller()
      {
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Request.Invocation invocation = (Request.Invocation)obj;

            msh.marshal(invocation.getObject(), null, "object", TRANSFER_OBJECT_MSH, 0);
            msh.marshal(invocation.getEventName(), null, "event", STRING_MSH, MF_LITERAL);
            msh.marshal(invocation.getArguments(), null, "arguments", OBJECT_ARRAY_MSH, MF_LITERAL);
            msh.marshal(invocation.getAttributes(), null, "attributes", PAIR_MSH, 0);
         }

         public String getType()
         {
            return "Invocation";
         }
      };

      private final Marshaller INVOCATIONS_MSH = new ArrayMarshaller(INVOCATION_MSH, MF_LITERAL)
      {
         protected int getSize(Object obj)
         {
            return ((Request)obj).getInvocationCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Request req = (Request)obj;

            for (int i = 0, n = req.getInvocationCount(); i != n; ++i)
            {
               marshalItem(req.getInvocation(i), msh);
            }
         }
      };

      private final Marshaller FILTERS_MSH = new ArrayMarshaller(TRANSFER_OBJECT_MSH, 0)
      {
         protected int getSize(Object obj)
         {
            return ((Request)obj).getFilterCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Request req = (Request)obj;

            for (int i = 0, n = req.getFilterCount(); i != n; ++i)
            {
               marshalItem(req.getFilter(i), msh);
            }
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         Request req = (Request)obj;

         msh.marshal(req.getNamespace(), null, "namespace", STRING_MSH, MF_LITERAL);
         msh.marshal(req.getVersion(), null, "version", STRING_MSH, MF_LITERAL);
         msh.marshal(Boolean.valueOf(req.isAsync()), null, "async", BOOLEAN_MSH, MF_LITERAL);
         msh.marshal(Boolean.valueOf(req.isCommit()), null, "commit", BOOLEAN_MSH, MF_LITERAL);
         msh.marshal(req.getLocale(), null, "locale", LOCALE_MSH, MF_LITERAL);
         msh.marshal(req.getTimeZone(), null, "timeZone", TIME_ZONE_MSH, MF_LITERAL);
         msh.marshal(req.getCorrelator(), null, "correlator", TRANSFER_OBJECT_MSH, 0);

         if (req.getInvocationCount() != 0)
         {
            msh.marshal(req, null, "invocations", INVOCATIONS_MSH, MF_LITERAL);
         }

         if (req.getFilterCount() != 0)
         {
            msh.marshal(req, null, "filters", FILTERS_MSH, MF_LITERAL);
         }
      }

      public String getType()
      {
         return "Request";
      }
   };

   protected final static Marshaller RESPONSE_MSH = new ObjectMarshaller()
   {
      private final Marshaller RESULTS_MSH = new ArrayMarshaller(null, 0)
      {
         protected int getSize(Object obj)
         {
            return ((Response)obj).getResultCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Response resp = (Response)obj;

            for (int i = 0, n = resp.getResultCount(); i != n; ++i)
            {
               marshalItem(resp.getResult(i), msh);
            }
         }
      };

      private final Marshaller EVENTS_MSH = new ArrayMarshaller(
         new CollectionMarshaller(TRANSFER_OBJECT_MSH, 0), 0)
      {
         protected int getSize(Object obj)
         {
            return ((Response)obj).getEventCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            Response resp = (Response)obj;

            for (int i = 0, n = resp.getEventCount(); i != n; ++i)
            {
               marshalItem(resp.getEvent(i), msh);
            }
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         Response resp = (Response)obj;

         msh.marshal(resp, null, "results", RESULTS_MSH, MF_LITERAL);

         if (resp.getEventCount() != 0)
         {
            msh.marshal(obj, null, "events", EVENTS_MSH, MF_LITERAL);
         }
      }

      public String getType()
      {
         return "Response";
      }
   };

   protected final static Marshaller EXCEPTION_MSH = new ObjectMarshaller()
   {
      private final Marshaller ARGS_MSH = new ObjectArrayMarshaller(null, 0)
      {
         protected void marshalItem(Object item, SOAPMarshaller msh) throws IOException
         {
            if (item != null && !s_mshMap.contains(item.getClass()))
            {
               if (msh.m_context != null)
               {
                  item = msh.m_context.getStringTable().toString(item);
               }
               else
               {
                  item = String.valueOf(item);
               }
            }

            super.marshalItem(item, msh);
         }

      };

      private final Marshaller ATTRIBUTES_MSH = new ArrayMarshaller(STRING_MSH, 0)
      {
         protected int getSize(Object obj)
         {
            return ((ErrorLocationHolder)obj).getAttributeCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            ErrorLocationHolder h = (ErrorLocationHolder)obj;

            for (Iterator itr = h.getAttributeIterator(); itr.hasNext();)
            {
               marshalItem(itr.next(), msh);
            }
         }
      };

      private final Marshaller ATTRIBUTE_EXCEPTIONS_MSH = new ArrayMarshaller(EXCEPTION_MSH, 0)
      {
         protected int getSize(Object obj)
         {
            return ((ErrorLocationHolder)obj).getAttributeCount();
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            ErrorLocationHolder h = (ErrorLocationHolder)obj;

            for (Iterator itr = h.getAttributeIterator(); itr.hasNext();)
            {
               marshalItem(h.findException((String)itr.next()), msh);
            }
         }
      };

      private final Marshaller EXCEPTIONS_MSH = new ArrayMarshaller(EXCEPTION_MSH, 0)
      {
         protected int getSize(Object obj)
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

            return nCount;
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            ExceptionHolder h = (ExceptionHolder)obj;

            for (Iterator itr = h.getExceptionIterator(); itr.hasNext();)
            {
               Throwable cause = (Throwable)itr.next();

               if (!ObjUtil.isSystem(cause))
               {
                  marshalItem(cause, msh);
               }
            }
         }
      };

      private final Marshaller CAUSE_MSH = new ArrayMarshaller(EXCEPTION_MSH, 0)
      {
         protected int getSize(Object obj)
         {
            return 1;
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            marshalItem(obj, msh);
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         Throwable t = (Throwable)obj;

         if (obj instanceof ErrorCode)
         {
            ErrorCode e = (ErrorCode)t;

            msh.marshal(e.getErrorCode(), null, "errorCode", STRING_MSH, MF_LITERAL);
            msh.marshal((msh.getContext() != null)
               ? msh.getContext().formatString(e.getErrorCode(), e.getErrorArgs())
               : t.getLocalizedMessage(), null, "errorMessage", STRING_MSH, MF_LITERAL);
            msh.marshal(e.getErrorArgs(), null, "errorArgs", ARGS_MSH, MF_LITERAL);
         }
         else
         {
            msh.marshal("err." + t.getClass().getName(), null, "errorCode", STRING_MSH, MF_LITERAL);
            msh.marshal(t.getLocalizedMessage(), null, "errorMessage", STRING_MSH, MF_LITERAL);
         }

         if (obj instanceof ErrorLocationHolder)
         {
            ErrorLocationHolder h = (ErrorLocationHolder)obj;

            msh.marshal(h.getClassName(), null, "class", STRING_MSH, MF_LITERAL);

            if (h.getOIDHolder() != null)
            {
               msh.marshal(h.getOIDHolder().getOID(), null, "oid", OID_MSH, MF_LITERAL);
            }

            msh.marshal(h.getOrdinal(), null, "ordinal");

            if (h.getAttributeCount() != 0)
            {
               msh.marshal(h, null, "attributes", ATTRIBUTES_MSH, MF_LITERAL);
               msh.marshal(h, null, "attributeExceptions", ATTRIBUTE_EXCEPTIONS_MSH, MF_LITERAL);
            }
         }
         else
         {
            msh.marshal(-1, null, "ordinal");
         }

         if (obj instanceof ExceptionHolder)
         {
            if (((ExceptionHolder)obj).getExceptionCount() != 0)
            {
               msh.marshal(obj, null, "exceptions", EXCEPTIONS_MSH, MF_LITERAL);
            }
         }
         else
         {
            Throwable cause = t.getCause();

            if (cause != null && ObjUtil.isSystem(cause))
            {
               cause = null;
            }

            msh.marshal(cause, null, "exceptions", CAUSE_MSH, MF_LITERAL);
         }
      }

      public String getType()
      {
         return "Exception";
      }
   };

   protected final static Marshaller SOAP_REQUEST_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((SOAPRequest)obj).getRequest(), null, "request", REQUEST_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "invoke";
      }
   };

   protected final static Marshaller SOAP_RESPONSE_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(((SOAPResponse)obj).getResponse(), null, "response", RESPONSE_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return "invoke-response";
      }
   };

   protected final static Marshaller SOAP_FAULT_MSH = new ObjectMarshaller()
   {
      private final Marshaller DETAIL_EXCEPTION_MSH = new ObjectMarshaller()
      {
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            msh.marshal(obj, EXCEPTION_MSH.getNamespace(), EXCEPTION_MSH.getType(), EXCEPTION_MSH, MF_LITERAL);
         }

         public String getType()
         {
            return null;
         }

      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         SOAPFault fault = (SOAPFault)obj;

         if (fault.getCode() != null)
         {
            msh.marshal(((SOAP.ENV_URI.equals(fault.getURI())) ? SOAP.ENV_NS + ":" : "") +
               fault.getCode(), null, "faultcode", STRING_MSH, MF_LITERAL);
         }

         if (fault.getMessage() != null)
         {
            msh.marshal(fault.getMessage(), null, "faultstring", STRING_MSH, MF_LITERAL);
         }

         if (fault.getException() != null)
         {
            msh.marshal(fault.getException(), null, "detail", DETAIL_EXCEPTION_MSH, MF_LITERAL);
         }
      }

      public String getNamespace()
      {
         return SOAP.ENV_NS;
      }

      public String getType()
      {
         return "Fault";
      }
   };

   /**
    * Map of class objects to marshallers: Marshaller[Class].
    */
   protected final static Lookup s_mshMap = new HashTab(64);

   static
   {
      s_mshMap.put(String.class, STRING_MSH);
      s_mshMap.put(Binary.class, BINARY_MSH);
      s_mshMap.put(PagedBinary.class, BINARY_MSH);
      s_mshMap.put(Integer.class, INTEGER_MSH);
      s_mshMap.put(Long.class, LONG_MSH);
      s_mshMap.put(BigDecimal.class, DECIMAL_MSH);
      s_mshMap.put(Float.class, FLOAT_MSH);
      s_mshMap.put(Double.class, DOUBLE_MSH);
      s_mshMap.put(java.sql.Date.class, TIMESTAMP_MSH);
      s_mshMap.put(java.sql.Time.class, TIMESTAMP_MSH);
      s_mshMap.put(java.sql.Timestamp.class, TIMESTAMP_MSH);
      s_mshMap.put(Boolean.class, BOOLEAN_MSH);
      s_mshMap.put(ArrayList.class, LIST_MSH);
      s_mshMap.put(PagedArrayList.class, LIST_MSH);
      s_mshMap.put(Object[].class, OBJECT_ARRAY_MSH);
      s_mshMap.put(String[].class, STRING_ARRAY_MSH);
      s_mshMap.put(Character.class, CHARACTER_MSH);
      s_mshMap.put(char[].class, CHAR_ARRAY_MSH);
      s_mshMap.put(StringId.class, STRINGID_MSH);
      s_mshMap.put(Symbol.class, SYMBOL_MSH);
      s_mshMap.put(Locale.class, LOCALE_MSH);
      s_mshMap.put(TimeZone.class, TIME_ZONE_MSH);
      s_mshMap.put(Pair.class, PAIR_MSH);
      s_mshMap.put(ConstPair.class, PAIR_MSH);
      s_mshMap.put(PrivilegeSet.class, PRIVILEGE_SET_MSH);
      s_mshMap.put(byte[].class, BYTE_VECTOR_MSH);
      s_mshMap.put(PCodeFunction.class, FUNCTION_MSH);
      s_mshMap.put(PCodeMacro.class, MACRO_MSH);
      s_mshMap.put(OID.class, OID_MSH);
      s_mshMap.put(TransferObject.class, TRANSFER_OBJECT_MSH);
      s_mshMap.put(Request.class, REQUEST_MSH);
      s_mshMap.put(Response.class, RESPONSE_MSH);
      s_mshMap.put(SOAPRequest.class, SOAP_REQUEST_MSH);
      s_mshMap.put(SOAPResponse.class, SOAP_RESPONSE_MSH);
      s_mshMap.put(SOAPFault.class, SOAP_FAULT_MSH);
   }

   // attributes

   /**
    * Count of fixup entries.
    */
   protected int m_nFixupCount;

   // associations

   /**
    * The output stream writer.
    */
   protected XMLWriter m_writer;

   /**
    * The id conversion buffer.
    */
   protected char[] m_idBuf;

   /**
    * Map of object to id: id[Object].
    */
   protected Lookup m_idMap;

   /**
    * The fixup array: Marshaller[n*3], id[n*3+1], Object[n*3+2]
    */
   protected Object[] m_fixupArray;

   /**
    * The runtime context.
    */
   protected Context m_context;

   // constructors

   /**
    * Constructs the SOAP marshaller.
    * @param context The runtime context.
    */
   public SOAPMarshaller(Context context)
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
    * Marshals a character as an unsigned short.
    * @param ch The character to marshal.
    * @param sNamespace The element namespace name (not URI).
    * @param sElement The element name.
    */
   protected void marshal(char ch, String sNamespace, String sElement) throws IOException
   {
      m_writer.setNamespace(sNamespace);
      m_writer.startElement(sElement);
      m_writer.writeInt(ch);
      m_writer.endElement(sElement);
   }

   /**
    * Marshals an integer literally.
    * @param n The integer to marshal.
    * @param sNamespace The element namespace name (not URI).
    * @param sElement The element name.
    */
   protected void marshal(int n, String sNamespace, String sElement) throws IOException
   {
      m_writer.setNamespace(sNamespace);
      m_writer.startElement(sElement);
      m_writer.writeInt(n);
      m_writer.endElement(sElement);
   }

   /**
    * Marshals a long integer literally.
    * @param l The long integer to marshal.
    * @param sNamespace The element namespace name (not URI).
    * @param sElement The element name.
    */
   protected void marshal(long l, String sNamespace, String sElement) throws IOException
   {
      m_writer.setNamespace(sNamespace);
      m_writer.startElement(sElement);
      m_writer.write(Long.toString(l));
      m_writer.endElement(sElement);
   }

   /**
    * Marshals an object as xsd:anyType.
    * @param obj The object to marshal.
    * @param sNamespace The XML element namespace name (not URI).
    * @param sElement The XML element name.
    * @param msh The marshaller to use, or null to determine automatically.
    * @param nFlags Combination of MF_* constants.
    */
   protected void marshal(Object obj, String sNamespace, String sElement, Marshaller msh, int nFlags) throws IOException
   {
      if (obj == null)
      {
         if ((nFlags & MF_NIL) != 0)
         {
            m_writer.setNamespace(sNamespace);
            m_writer.openElement(sElement);
            m_writer.setNamespace(SOAP.XSI_NS);
            m_writer.writeAttribute("nil", true);
            m_writer.closeEmptyElement();
         }
      }
      else
      {
         if (msh == null)
         {
            msh = getMarshaller(obj);
         }

         m_writer.setNamespace(sNamespace);
         m_writer.openElement(sElement);

         if (msh.isPrimitive() || (nFlags & MF_LITERAL) != 0)
         {
            if ((nFlags & MF_TYPE) != 0)
            {
               m_writer.setNamespace(SOAP.XSI_NS);
               m_writer.writeAttribute("type", msh.getNamespace(), ":", msh.getType());
            }

            msh.marshalAttributes(obj, this);
            m_writer.closeElement();
            msh.marshalContents(obj, this);
            m_writer.setNamespace(sNamespace);
            m_writer.endElement(sElement);
         }
         else
         {
            m_writer.writeAttribute("href", "#", addObj(obj, msh));
            m_writer.closeEmptyElement();
         }
      }
   }

   /**
    * Serializes an object to a character stream.
    * @param obj The object to serialize.
    * @param writer The character stream writer.
    */
   public void serialize(Object obj, Writer writer) throws IOException
   {
      // Initialize
      m_nFixupCount = 0;
      m_fixupArray = new Object[48];
      m_idMap = new IdentityHashTab();
      m_writer = (writer instanceof XMLWriter) ? (XMLWriter)writer : new XMLWriter(writer);

      // Write out the header
      m_writer.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
      m_writer.setNamespace(SOAP.ENV_NS);
      m_writer.openElement("Envelope");
      m_writer.setNamespace(SOAP.XML_NS);
      m_writer.writeAttribute(SOAP.XSD_NS, SOAP.XSD_URI);
      m_writer.writeAttribute(SOAP.XSI_NS, SOAP.XSI_URI);
      m_writer.writeAttribute(SOAP.ENV_NS, SOAP.ENV_URI);
      m_writer.writeAttribute(SOAP.ENC_NS, SOAP.ENC_URI);
      m_writer.closeElement();
      m_writer.setNamespace(SOAP.ENV_NS);
      m_writer.openElement("Body");
      m_writer.writeAttribute("encodingStyle", SOAP.ENC_URI);
      m_writer.setNamespace(SOAP.XML_NS);
      m_writer.writeAttribute(SOAP.TNS_NS, SOAP.TNS_URI);
      m_writer.closeElement();

      Marshaller msh  = (obj != null) ? getMarshaller(obj) : STRING_MSH;

      marshal(obj, msh.getNamespace(), msh.getType(), msh, MF_LITERAL);

      // Marshal the enqueued object while adding new ones if needed
      for (int i = 0; i < m_nFixupCount; i += ID_FIXUP_SIZE)
      {
         msh = (Marshaller)m_fixupArray[i];
         m_writer.setNamespace(msh.getNamespace());
         m_writer.openElement(msh.getType());
         m_writer.setNamespace(null);
         m_writer.writeAttribute("id", (String)m_fixupArray[i + 1]);
         obj = m_fixupArray[i + 2];
         msh.marshalAttributes(obj, this);
         m_writer.closeElement();
         msh.marshalContents(obj, this);
         m_writer.setNamespace(msh.getNamespace());
         m_writer.endElement(msh.getType());
      }

      m_writer.setNamespace(SOAP.ENV_NS);
      m_writer.endElement("Body");
      m_writer.endElement("Envelope");

      // Cleanup the resources
      m_fixupArray = null;
      m_idMap = null;
      m_writer = null;
   }

   /**
    * Gets a marshaller for an object.
    * @param obj The object for which to get the marshaller. Cannot be null.
    * @return The marshaller.
    * @throws SOAPMarshallerException if a marshaller cannot be found.
    */
   protected static Marshaller getMarshaller(Object obj) throws SOAPMarshallerException
   {
      Marshaller msh = (Marshaller)s_mshMap.get(obj.getClass());

      if (msh == null)
      {
         if (obj instanceof Throwable)
         {
            msh = EXCEPTION_MSH;
         }
         else
         {
            throw new SOAPMarshallerException("err.rpc.mshType",
               new Object[]{obj.getClass().getName()});
         }
      }

      return msh;
   }

   /**
    * Adds a fixup to the array.
    * @param fixup The fixup object.
    * @param nSize The fixup size, i.e. how many consecutive array elements to allocate.
    * @return The index of the added fixup.
    */
   protected int addFixup(int nSize)
   {
      if (m_nFixupCount + nSize > m_fixupArray.length)
      {
         Object [] fixupArray = new Object[(m_nFixupCount + nSize) << 1];

         System.arraycopy(m_fixupArray, 0, fixupArray, 0, m_nFixupCount);
         m_fixupArray = fixupArray;
      }

      int i = m_nFixupCount;

      m_nFixupCount += nSize;

      return i;
   }

   /**
    * Sets a fixup value at a given index.
    * @param i The fixup index.
    * @param value The value to set.
    */
   protected void setFixup(int i, Object value)
   {
      m_fixupArray[i] = value;
   }

   /**
    * Gets a fixup value at a given index.
    * @param i The fixup index.
    * @return The fixup value.
    */
   protected Object getFixup(int i)
   {
      return m_fixupArray[i];
   }

   /**
    * Enqueues an object for marshalling.
    * @param obj The object to enqueue.
    * @param msh The object marshaller.
    * @return The object element id.
    */
   protected String addObj(Object obj, Marshaller msh)
   {
      String sId = (String)m_idMap.get(obj);

      if (sId == null)
      {
         sId = toId(m_nFixupCount / ID_FIXUP_SIZE);
         m_idMap.put(obj, sId);
         int i = addFixup(ID_FIXUP_SIZE);

         setFixup(i, msh);
         setFixup(i + 1, sId);
         setFixup(i + 2, obj);
      }

      return sId;
   }

   /**
    * Computes a string element id from an integer.
    * @param n The integer from which to compute the id.
    * @return The element id.
    */
   protected String toId(int n)
   {
      if (n < s_idArray.length)
      {
         return s_idArray[n];
      }

      if (m_idBuf == null)
      {
         m_idBuf = new char[ID_BUF_LENGTH];
      }

      return toId(n, m_idBuf);
   }

   /**
    * Computes a string element id from an integer.
    * @param n The integer from which to compute the id.
    * @param buf The work buffer.
    * @return The element id.
    */
   protected static String toId(int n, char[] buf)
   {
      assert n >= 0;

      int i = 0;
      int k;

      do
      {
         k = n % 52;
         buf[i++] = (char)(k + ((k < 26) ? 'a' : (int)'A' - 26));
         n /= 52;
      }
      while (n != 0);

      return new String(buf, 0, i);
   }

   // inner classes

   /**
    * Interface implemented by SOAP marshallers.
    */
   protected interface Marshaller
   {
      /**
       * Marshals the attributes of an object of specific type to the output stream.
       * @param obj The object to marshal.
       * @param msh The SOAP marshaller.
       */
      void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException;

      /**
       * Marshals an object of a specific type to the output stream as the
       * contents of a containing element, which has been output by the caller.
       * @param obj The object to marshal.
       * @param msh The SOAP marshaller.
       */
      void marshalContents(Object obj, SOAPMarshaller msh) throws IOException;

      /**
       * @return The type namespace name (not URI).
       */
      String getNamespace();

      /**
       * @return The type name.
       */
      String getType();

      /**
       * @return True if the type is primitive.
       */
      boolean isPrimitive();
   }

   /**
    * Base class of primitive type marshallers.
    */
   protected abstract static class PrimitiveMarshaller implements Marshaller
   {
      public void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException
      {
      }

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.m_writer.write(obj.toString());
      }

      public String getNamespace()
      {
         return SOAP.XSD_NS;
      }

      public final boolean isPrimitive()
      {
         return true;
      }
   }

   /**
    * Base class for object type marshallers.
    */
   protected abstract static class ObjectMarshaller implements Marshaller
   {
      public void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException
      {
      }

      public String getNamespace()
      {
         return SOAP.TNS_NS;
      }

      public final boolean isPrimitive()
      {
         return false;
      }
   }

   /**
    * Generic array marshaller.
    */
   protected abstract static class ArrayMarshaller extends ObjectMarshaller
   {
      // attributes

      /**
       * The item marshalling flags.
       */
      protected int m_nFlags;

      /**
       * The array item type.
       */
      protected String m_sItemType;

      // associations

      /**
       * The item marshaller. Can be null.
       */
      protected Marshaller m_itemMsh;

      // constructors

      /**
       * Constructs the marshaller.
       * @param itemMsh The item marshaller. Can be null to determine dynamically.
       * @param nFlags Combination of MF_* bits for marshalling the items.
       */
      public ArrayMarshaller(Marshaller itemMsh, int nFlags)
      {
         m_itemMsh = itemMsh;
         m_nFlags = nFlags | MF_NIL;

         if (itemMsh == null)
         {
            m_nFlags |= MF_TYPE;
            m_sItemType = SOAP.XSD_NS + ":anyType";
         }
         else
         {
            int n;

            for (n = 0; itemMsh instanceof ArrayMarshaller;
               ++n, itemMsh = ((ArrayMarshaller)itemMsh).m_itemMsh);

            StringBuffer buf = new StringBuffer();

            if (itemMsh == null)
            {
               buf.append(SOAP.XSD_NS);
               buf.append(":anyType");
            }
            else
            {
               buf.append(itemMsh.getNamespace());
               buf.append(':');
               buf.append(itemMsh.getType());
            }

            while (n-- != 0)
            {
               buf.append("[]");
            }

            m_sItemType = buf.toString();
         }
      }

      // operations

      public void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException
      {
         msh.m_writer.setNamespace(SOAP.ENC_NS);
         msh.m_writer.writeAttribute("arrayType", m_sItemType, "[", Integer.toString(getSize(obj)), "]");
      }

      /**
       * Marshals a single item.
       * @param item The item to marshal.
       * @param msh The SOAP marshaller.
       */
      protected void marshalItem(Object item, SOAPMarshaller msh) throws IOException
      {
         msh.marshal(item, null, "Item", m_itemMsh, m_nFlags);
      }

      /**
       * Template method for retieving the collection size.
       * @param obj The collection object.
       * @return The array size.
       */
      protected abstract int getSize(Object obj);

      public final String getNamespace()
      {
         return SOAP.ENC_NS;
      }

      public final String getType()
      {
         return "Array";
      }
   }

   /**
    * Object[] marshaller.
    */
   protected static class ObjectArrayMarshaller extends ArrayMarshaller
   {
      /**
       * Constructs the marshaller.
       * @param itemMsh The item marshaller. Can be null to determine dynamically.
       * @param nFlags Combination of MF_* bits for marshalling the items.
       */
      public ObjectArrayMarshaller(Marshaller itemMsh, int nFlags)
      {
         super(itemMsh, nFlags);
      }

      protected int getSize(Object obj)
      {
         return ((Object[])obj).length;
      }

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         Object[] array = (Object[])obj;

         for (int i = 0; i != array.length; ++i)
         {
            marshalItem(array[i], msh);
         }
      }
   }

   /**
    * Collection marshaller.
    */
   protected static class CollectionMarshaller extends ArrayMarshaller
   {
      /**
       * Constructs the marshaller.
       * @param itemMsh The item marshaller. Can be null to determine dynamically.
       * @param nFlags Combination of MF_* bits for marshalling the items.
       */
      public CollectionMarshaller(Marshaller itemMsh, int nFlags)
      {
         super(itemMsh, nFlags);
      }

      protected int getSize(Object obj)
      {
         return ((Collection)obj).size();
      }

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         for (Iterator itr = ((Collection)obj).iterator(); itr.hasNext();)
         {
            marshalItem(itr.next(), msh);
         }
      }
   };

   /**
    * List marshaller.
    */
   protected static class ListMarshaller extends ArrayMarshaller
   {
      /**
       * Constructs the marshaller.
       * @param itemMsh The item marshaller. Can be null to determine dynamically.
       * @param nFlags Combination of MF_* bits for marshalling the items.
       */
      public ListMarshaller(Marshaller itemMsh, int nFlags)
      {
         super(itemMsh, nFlags);
      }

      protected int getSize(Object obj)
      {
         return ((List)obj).size();
      }

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         List list = (List)obj;

         for (int i = 0, n = list.size(); i != n; ++i)
         {
            marshalItem(list.get(i), msh);
         }
      }
   }
}
