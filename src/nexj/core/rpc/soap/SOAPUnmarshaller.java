// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import java.io.IOException;
import java.io.Reader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.Locale;
import java.util.TimeZone;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.persistence.OID;
import nexj.core.rpc.CharacterStreamUnmarshaller;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
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
import nexj.core.util.SOAPUtil;
import nexj.core.util.StringId;
import nexj.core.util.XMLException;
import nexj.core.util.XMLParserException;
import nexj.core.util.XMLUtil;

/**
 * SOAP format unmarshaller.
 */
public class SOAPUnmarshaller extends DefaultHandler implements CharacterStreamUnmarshaller, ContextAware
{
   // constants

   /**
    * The offset of the first reference fixup index in the index array relative to the object fixup index.
    */
   protected final static int FIRST_FIXUP_OFS = 1;
   
   /**
    * The offset of the unmarshaller in the fixup array relative to the object fixup index.
    */
   protected final static int UNMSH_OFS = 1;
   
   /**
    * The offset of the first temporary value relative to the object fixup index.
    */
   protected final static int TEMP_OFS = 2;
   
   /**
    * The offset of the href value in the fixup array relative to the reference fixup index.
    */
   protected final static int REF_OFS = 0;
   
   /**
    * The offset of the accessor in the fixup array relative to the reference fixup index.
    */
   protected final static int ACCESSOR_OFS = 1;
   
   /**
    * The offset of the cookie in the index array relative to the reference fixup index.
    */
   protected final static int COOKIE_OFS = 1;

   /**
    * The offset of the object reference in the parser stack.
    */
   protected final static int STK_REF_OFS = 0;
   
   /**
    * The offset of the unmarshaller in the parser stack.
    */
   protected final static int STK_UNMSH_OFS = 1;
   
   /**
    * The offset of the accessor in the parser stack.
    */
   protected final static int STK_ACCESSOR_OFS = 2;

   /**
    * The size of the stack state entry.
    */
   protected final static int STK_STATE_SIZE = 3;
   
   /**
    * Special value to indicate a non-constructed object.
    */
   protected final static Object EMPTY = new Object();
   
   /**
    * The unmarshallers.
    */
   protected final static Unmarshaller STRING_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "string")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh)
      {
         return sValue;
      }

      public boolean isMultiRef()
      {
         return true;
      }
   };

   protected final static Unmarshaller BINARY_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "base64Binary")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws IOException
      {
         return new Binary(Base64Util.decode(sValue));
      }

      public boolean isMultiRef()
      {
         return true;
      }
   };
   
   protected final static Unmarshaller INT_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "int")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return Primitive.createInteger(Integer.parseInt(sValue));
      }
   };

   protected final static Unmarshaller LONG_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "long")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return Primitive.createLong(Long.parseLong(sValue));
      }
   };

   protected final static Unmarshaller INTEGER_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "integer")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return new BigDecimal(new BigInteger(sValue));
      }
   };

   protected final static Unmarshaller DECIMAL_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "decimal")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return new BigDecimal(sValue);
      }
   };

   protected final static Unmarshaller FLOAT_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "float")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         if (sValue.length() > 0 && sValue.charAt(sValue.length() - 1) == 'F')
         {
            if (sValue.equals("INF"))
            {
               sValue = "Infinity";
            }
            else if (sValue.equals("-INF"))
            {
               sValue = "-Infinity";
            }
         }
         
         return Primitive.createFloat(Float.parseFloat(sValue));
      }
   };

   protected final static Unmarshaller DOUBLE_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "double")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         if (sValue.length() > 0 && sValue.charAt(sValue.length() - 1) == 'F')
         {
            if (sValue.equals("INF"))
            {
               sValue = "Infinity";
            }
            else if (sValue.equals("-INF"))
            {
               sValue = "-Infinity";
            }
         }
         
         return Primitive.createDouble(Double.parseDouble(sValue));
      }
   };
   
   protected final static Unmarshaller TIMESTAMP_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "dateTime")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return SOAPUtil.parseDateTime(sValue, true, true, unmsh.getContext().getTimeZone());
      }
   };

   protected final static Unmarshaller BOOLEAN_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "boolean")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         if (sValue.equals("true") || sValue.equals("1"))
         {
            return Boolean.TRUE;
         }
         
         if (sValue.equals("false") || sValue.equals("0"))
         {
            return Boolean.FALSE;
         }
         
         throw new SOAPUnmarshallerException("err.rpc.soap.boolean", new Object[]{sValue});
      }
   };

   protected final static Unmarshaller CHARACTER_UNMSH = new SimpleUnmarshaller(SOAP.XSD_URI, "unsignedShort")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return Primitive.createCharacter(Integer.parseInt(sValue));
      }
   };

   protected final static Unmarshaller CHAR_ARRAY_UNMSH = new ArrayUnmarshaller(
      new SimpleUnmarshaller(SOAP.XSD_URI, "unsignedShort")
      {
         public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
         {
            return sValue;
         }
      })
   {
      protected int init(int nSize, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         int nObj = unmsh.addObjectFixup(new char[nSize], this, 1);

         unmsh.setTempIndex(nObj, 0, 0);

         return nObj;
      }

      protected int addValue(int nObj, SOAPUnmarshaller unmsh)
      {
         int i = unmsh.getTempIndex(nObj, 0);
         
         unmsh.setTempIndex(nObj, 0, i + 1);
         
         return i;
      }

      protected char parse(String s) throws SOAPUnmarshallerException
      {
         try
         {
            return (char)Integer.parseInt(s);
         }
         catch (Exception e)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.charCode", new Object[]{s, getURI(), getType()});
         }
      }

      public void setValue(int nObj, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((char[])unmsh.getFixup(nObj))[addValue(nObj, unmsh)] = parse((String)value);
      }

      public void setValue(int nObj, int nCookie, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((char[])unmsh.getTempValue(nObj, 0))[nCookie] = parse((String)value);
      }
   };

   protected final static Unmarshaller STRING_ARRAY_UNMSH = new ObjectArrayUnmarshaller(STRING_UNMSH)
   {
      protected Object[] construct(int nSize)
      {
         return new String[nSize];
      }
   };

   /**
    * Unmarshaller for xsd:anyType[].
    */
   protected final static Unmarshaller OBJECT_ARRAY_UNMSH = new ObjectArrayUnmarshaller(null);

   protected final static Unmarshaller LIST_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Array",
      new ObjectAccessor[]{new StaticObjectAccessor("", "items", false, false, OBJECT_ARRAY_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Object[] array = (Object[])unmsh.getTempValue(nObj, 0);
         
         unmsh.setFixup(nObj, (array == null) ? Collections.EMPTY_LIST : Arrays.asList(array));
      }
   };

   protected final static Unmarshaller STRINGID_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "StringId",
      new ObjectAccessor[]{new StaticObjectAccessor("", "id", true, true, STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, new StringId((String)unmsh.getTempValue(nObj, 0)));
      }
   };
   
   protected final static Unmarshaller SYMBOL_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Symbol",
      new ObjectAccessor[]{new StaticObjectAccessor("", "name", true, true, STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, Symbol.define((String)unmsh.getTempValue(nObj, 0)));
      }
   };

   protected final static Unmarshaller LOCALE_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Locale",
      new ObjectAccessor[]{new StaticObjectAccessor("", "name", true, true, STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, LocaleUtil.parse((String)unmsh.getTempValue(nObj, 0)));
      }
   };

   protected final static Unmarshaller TIME_ZONE_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "TimeZone",
      new ObjectAccessor[]{new StaticObjectAccessor("", "name", true, true, STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, TimeZone.getTimeZone((String)unmsh.getTempValue(nObj, 0)));
      }
   };

   /**
    * String expression parsed into an S-expression.
    * Can be used to eliminate S-expression builder calls on the client.
    */
   protected final static Unmarshaller EXPRESSION_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Expression",
      new ObjectAccessor[]{new StaticObjectAccessor("", "text", true, true, STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, RPCUtil.parse(new SchemeParser(unmsh.getContext().getMachine().getGlobalEnvironment()),
            (String)unmsh.getTempValue(nObj, 0), null));
      }
   };

   protected final static Unmarshaller PAIR_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Pair",
      new ObjectAccessor[]
      {
         new DynamicObjectAccessor("", "head", false, false),
         new DynamicObjectAccessor("", "tail", false, false)
      })
   {
      protected Object construct()
      {
         return new Pair(null);
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Pair pair = (Pair)unmsh.getFixup(nObj);
         
         pair.setHead(unmsh.getTempValue(nObj, 0));
         pair.setTail(unmsh.getTempValue(nObj, 1));
      }
   };
   
   protected final static Unmarshaller PRIVILEGE_SET_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "PrivilegeSet",
      new ObjectAccessor[]{new StaticObjectAccessor("", "mask", false, true, BINARY_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Binary mask = (Binary)unmsh.getTempValue(nObj, 0);
         
         unmsh.setFixup(nObj, (mask == null) ? new PrivilegeSet(0) : new PrivilegeSet(mask.getData()));
      }
   };

   protected final static Unmarshaller BYTE_VECTOR_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "ByteVector",
      new ObjectAccessor[]{new StaticObjectAccessor("", "value", false, true, BINARY_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Binary value = (Binary)unmsh.getTempValue(nObj, 0);
         
         unmsh.setFixup(nObj, (value == null) ? null : value.getData());
      }
   };

   protected final static Unmarshaller FUNCTION_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Function",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "code", true, false, CHAR_ARRAY_UNMSH),
         new ObjectArrayAccessor("", "constants", false, false)
      })
   {
      protected Object construct()
      {
         return new PCodeFunction();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         PCodeFunction fun = (PCodeFunction)unmsh.getFixup(nObj);
         
         fun.code = (char[])unmsh.getTempValue(nObj, 0);
         fun.constants = (Object[])unmsh.getTempValue(nObj, 1);
      }
   };
   
   protected final static Unmarshaller MACRO_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Macro",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "code", true, false, CHAR_ARRAY_UNMSH),
         new ObjectArrayAccessor("", "constants", false, false)
      })
   {
      protected Object construct()
      {
         return new PCodeMacro();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         PCodeMacro fun = (PCodeMacro)unmsh.getFixup(nObj);
         
         fun.code = (char[])unmsh.getTempValue(nObj, 0);
         fun.constants = (Object[])unmsh.getTempValue(nObj, 1);
      }
   };
   
   protected final static Unmarshaller OID_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "OID",
      new ObjectAccessor[]{new ObjectArrayAccessor("", "values", true, false)})
   {
      protected Object construct()
      {
         return new OID();
      }
      
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((OID)unmsh.getFixup(nObj)).setValueArray((Object[])unmsh.getTempValue(nObj, 0));
      }
   };

   protected final static Unmarshaller TRANSFER_OBJECT_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "TransferObject",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "class", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "event", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "version", false, true, INT_UNMSH),
         new StaticObjectAccessor("", "oid", false, false, OID_UNMSH),
         new StaticObjectAccessor("", "keys", false, true, STRING_ARRAY_UNMSH),
         new ObjectArrayAccessor("", "values", false, true)
      })
   {
      protected Object construct()
      {
         return new TransferObject();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         TransferObject tobj = (TransferObject)unmsh.getFixup(nObj);

         tobj.setClassName((String)unmsh.getTempValue(nObj, 0));
         tobj.setEventName((String)unmsh.getTempValue(nObj, 1));

         Integer version = (Integer)unmsh.getTempValue(nObj, 2);

         if (version != null)
         {
            tobj.setVersion(version.shortValue());
         }

         tobj.setOID((OID)unmsh.getTempValue(nObj, 3));

         String[] keys = (String[])unmsh.getTempValue(nObj, 4);
         Object[] values = (Object[])unmsh.getTempValue(nObj, 5);

         if (((keys == null) ? 0 : keys.length) != ((values == null) ? 0 : values.length)) 
         {
            throw new SOAPMarshallerException("err.rpc.soap.keyValueMismatch", new Object[]{m_sURI, m_sType});
         }

         if (keys != null && values != null)
         {
            for (int i = 0; i != keys.length; ++i)
            {
               tobj.setValue(keys[i], values[i]);
            }
         }
      }
   };
   
   protected final static Unmarshaller TRANSFER_OBJECT_ARRAY_UNMSH = new ObjectArrayUnmarshaller(TRANSFER_OBJECT_UNMSH);
   protected final static Unmarshaller TRANSFER_OBJECT_ARRAY_ARRAY_UNMSH = new ObjectArrayUnmarshaller(TRANSFER_OBJECT_ARRAY_UNMSH);

   protected final static Unmarshaller INVOCATION_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Invocation",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "object", true, false, TRANSFER_OBJECT_UNMSH),
         new StaticObjectAccessor("", "event", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "arguments", false, false, OBJECT_ARRAY_UNMSH),
         new StaticObjectAccessor("", "attributes", false, false, PAIR_UNMSH)
      })
   {
      protected Object construct()
      {
         return new Request.Invocation(null);
      }

      public void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Request.Invocation invocation = (Request.Invocation)unmsh.getFixup(nObj);

         invocation.setObject((TransferObject)unmsh.getTempValue(nObj, 0));
         invocation.setEventName((String)unmsh.getTempValue(nObj, 1));
         invocation.setArguments((Object[])unmsh.getTempValue(nObj, 2));
         invocation.setAttributes((Pair)unmsh.getTempValue(nObj, 3));
      }
   };

   protected final static Unmarshaller REQUEST_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Request",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "namespace", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "version", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "async", false, true, BOOLEAN_UNMSH),
         new StaticObjectAccessor("", "commit", false, true, BOOLEAN_UNMSH),
         new StaticObjectAccessor("", "locale", false, false, LOCALE_UNMSH),
         new StaticObjectAccessor("", "timeZone", false, false, TIME_ZONE_UNMSH),
         new StaticObjectAccessor("", "correlator", false, false, TRANSFER_OBJECT_UNMSH),
         new StaticObjectAccessor("", "invocations", false, true, new ObjectArrayUnmarshaller(INVOCATION_UNMSH)),
         new StaticObjectAccessor("", "args", false, true, TRANSFER_OBJECT_ARRAY_UNMSH), // deprecated
         new StaticObjectAccessor("", "parameters", false, true, new ObjectArrayUnmarshaller(OBJECT_ARRAY_UNMSH)), // deprecated
         new StaticObjectAccessor("", "attributes", false, true, new ObjectArrayUnmarshaller(PAIR_UNMSH)), // deprecated
         new StaticObjectAccessor("", "filters", false, true, TRANSFER_OBJECT_ARRAY_UNMSH)
      })
   {
      protected Object construct()
      {
         return new Request();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Request req = (Request)unmsh.getFixup(nObj);

         req.setNamespace((String)unmsh.getTempValue(nObj, 0));
         req.setVersion((String)unmsh.getTempValue(nObj, 1));

         Boolean async = (Boolean)unmsh.getTempValue(nObj, 2);

         if (async != null)
         {
            req.setAsync(async.booleanValue());
         }

         Boolean commit = (Boolean)unmsh.getTempValue(nObj, 3);

         if (commit != null)
         {
            req.setCommit(commit.booleanValue());
         }

         req.setLocale((Locale)unmsh.getTempValue(nObj, 4));
         req.setTimeZone((TimeZone)unmsh.getTempValue(nObj, 5));
         req.setCorrelator((TransferObject)unmsh.getTempValue(nObj, 6));

         Object[] invocations = (Object[])unmsh.getTempValue(nObj, 7);

         if (invocations != null)
         {
            for (int i = 0; i != invocations.length; ++i)
            {
               req.addInvocation((Request.Invocation)invocations[i]);
            }
         }
         else
         {
            Object[] args = (Object[])unmsh.getTempValue(nObj, 8);
            Object[] parameters = (Object[])unmsh.getTempValue(nObj, 9);
            Object[] attributes = (Object[])unmsh.getTempValue(nObj, 10);

            if (args != null)
            {
               for (int i = 0; i != args.length; ++i)
               {
                  req.addInvocation((TransferObject)args[i],
                     (parameters == null || i >= parameters.length) ? null : (Object[])parameters[i],
                     (attributes == null || i >= attributes.length) ? null : (Pair)attributes[i]);
               }
            }
         }

         Object[] filters = (Object[])unmsh.getTempValue(nObj, 11);

         if (filters != null)
         {
            for (int i = 0; i != filters.length; ++i)
            {
               req.addFilter((TransferObject)filters[i]);
            }
         }
      }
   };
   
   protected final static Unmarshaller RESPONSE_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Response",
      new ObjectAccessor[]
      {
         new ObjectArrayAccessor("", "results", false, true),
         new StaticObjectAccessor("", "events", false, true, TRANSFER_OBJECT_ARRAY_ARRAY_UNMSH)
      })
   {
      protected Object construct()
      {
         return new Response();
      }
      
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Response resp = (Response)unmsh.getFixup(nObj);
         Object[] results = (Object[])unmsh.getTempValue(nObj, 0);
         
         if (results != null)
         {
            for (int i = 0; i != results.length; ++i)
            {
               resp.addResult(results[i]);
            }
         }
         
         Object[] events = (Object[])unmsh.getTempValue(nObj, 1);
         
         if (events != null)
         {
            for (int i = 0; i != events.length; ++i)
            {
               Object[] event = (Object[])events[i];
               
               resp.addEvent((event == null) ? Collections.EMPTY_LIST : Arrays.asList(event));
            }
         }
      }
   };

   protected final static ObjectUnmarshaller EXCEPTION_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "Exception",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "errorCode", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "errorMessage", false, true, STRING_UNMSH),
         new ObjectArrayAccessor("", "errorArgs", false, false),
         new StaticObjectAccessor("", "class", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "oid", false, false, OID_UNMSH),
         new StaticObjectAccessor("", "ordinal", false, true, INT_UNMSH),
         new StaticObjectAccessor("", "attributes", false, true, STRING_ARRAY_UNMSH),
         new StaticObjectAccessor("", "attributeExceptions", false, true, null),
         new StaticObjectAccessor("", "exceptions", false, true, null)
      })
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         String sErrCode = (String)unmsh.getTempValue(nObj, 0);
         String sMessage = (String)unmsh.getTempValue(nObj, 1);
         Object[] argArray = (Object[])unmsh.getTempValue(nObj, 2);
         String sClassName = (String)unmsh.getTempValue(nObj, 3);
         GenericException e;

         if (argArray != null && argArray.length == 0)
         {
            argArray = null;
         }

         if (sClassName != null)
         {
            ValidationException x = new ValidationException(sErrCode, argArray);
            
            x.setClassName(sClassName);
            x.setOIDHolder((OID)unmsh.getTempValue(nObj, 4));
            
            Integer ordinal = (Integer)unmsh.getTempValue(nObj, 5);
            
            if (ordinal != null)
            {
               x.setOrdinal(ordinal.intValue());
            }
            
            String[] attributeArray = (String[])unmsh.getTempValue(nObj, 6);
            Object[] exceptionArray = (Object[])unmsh.getTempValue(nObj, 7);
            
            if (((attributeArray == null) ? 0 : attributeArray.length) !=
               ((exceptionArray == null) ? 0 : exceptionArray.length))
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.attributeExceptionMismatch",
                  new Object[]{m_sURI, m_sType});
            }
            
            if (attributeArray != null && exceptionArray != null)
            {
               for (int i = 0; i < attributeArray.length; ++i)
               {
                  x.addException(attributeArray[i], (Throwable)exceptionArray[i]);
               }
            }

            e = x;
         }
         else if (argArray != null)
         {
            e = new GenericException(sErrCode, argArray);
         }
         else
         {
            e = new GenericException(sErrCode, new Object[]{sMessage});
         }

         e.setStackTrace(new StackTraceElement[0]);

         Object[] exceptionArray = (Object[])unmsh.getTempValue(nObj, 8);
         
         if (exceptionArray != null)
         {
            for (int i = 0; i < exceptionArray.length; ++i)
            {
               e.addException((Throwable)exceptionArray[i]);
            }
         }
         
         unmsh.setFixup(nObj, e);
      }
   };

   protected final static Unmarshaller EXCEPTION_ARRAY_UNMSH = new ObjectArrayUnmarshaller(EXCEPTION_UNMSH);

   static
   {
      ((StaticObjectAccessor)EXCEPTION_UNMSH.getAccessor(7)).setUnmarshaller(EXCEPTION_ARRAY_UNMSH);
      ((StaticObjectAccessor)EXCEPTION_UNMSH.getAccessor(8)).setUnmarshaller(EXCEPTION_ARRAY_UNMSH);
   }

   protected final static Unmarshaller SOAP_REQUEST_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "invoke",
      new ObjectAccessor[]{new StaticObjectAccessor("", "request", true, false, REQUEST_UNMSH)})
   {
      protected Object construct()
      {
         return new SOAPRequest(null);
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((SOAPRequest)unmsh.getFixup(nObj)).setRequest((Request)unmsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };

   protected final static Unmarshaller SOAP_RESPONSE_UNMSH = new ObjectUnmarshaller(SOAP.TNS_URI, "invoke-response",
      new ObjectAccessor[]{new StaticObjectAccessor("", "response", true, false, RESPONSE_UNMSH)})
   {
      protected Object construct()
      {
         return new SOAPResponse(null);
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((SOAPResponse)unmsh.getFixup(nObj)).setResponse((Response)unmsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };

   protected final static Unmarshaller SOAP_FAULT_UNMSH = new ObjectSubsetUnmarshaller(SOAP.ENV_URI, "Fault",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor("", "faultcode", false, true,
            new SimpleUnmarshaller(SOAP.XSD_URI, "string")
            {
            public Object unmarshal(String sValue, SOAPUnmarshaller unmsh)
            {
               String sCode = getTypeName(sValue);
               String sURI = unmsh.getTypeURI(sValue, sCode);
               
               return new String[]{sURI, sCode};
            }

            public boolean isMultiRef()
            {
               return false;
            }
         }),
         new StaticObjectAccessor("", "faultstring", false, true, STRING_UNMSH),
         new StaticObjectAccessor("", "detail", false, false,
            new ObjectSubsetUnmarshaller(SOAP.TNS_URI, "Exception",
               new ObjectAccessor[]{new StaticObjectAccessor(SOAP.TNS_URI, "Exception", false, false, EXCEPTION_UNMSH)})
            {
               protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
               {
                  unmsh.setFixup(nObj, unmsh.getTempValue(nObj, 0));
               }
            })
      })
   {
      protected Object construct()
      {
         return new SOAPFault(null);
      }
      
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         SOAPFault fault = (SOAPFault)unmsh.getFixup(nObj);
         String[] code = (String[])unmsh.getTempValue(nObj, 0);
         
         fault.setURI(code[0]);
         fault.setCode(code[1]);
         fault.setMessage((String)unmsh.getTempValue(nObj, 1));
         fault.setException((Throwable)unmsh.getTempValue(nObj, 2));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };
   
   protected final static Unmarshaller SOAP_ENVELOPE_UNMSH = new ObjectSubsetUnmarshaller(SOAP.ENV_URI, "Envelope",
      new ObjectAccessor[]{new StaticObjectAccessor(SOAP.ENV_URI, "Body", true, false, new SOAPBodyUnmarshaller())})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, unmsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };

   protected final static ObjectUnmarshaller SOAP_MESSAGE_UNMSH = new ObjectUnmarshaller("", "SOAP-Message",
      new ObjectAccessor[]{new StaticObjectAccessor(SOAP.ENV_URI, "Envelope", true, false, SOAP_ENVELOPE_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setFixup(nObj, unmsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };

   /**
    * Special accessor that consumes all the elements.
    */
   protected final static Accessor NULL_ACCESSOR = new NullAccessor();

   /**
    * Special unmarshaller that skips all the elements.
    */
   protected final static Unmarshaller NULL_UNMSH = new ComplexUnmarshaller(null, null)
   {
      public int init(Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return 0;
      }

      public Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return NULL_ACCESSOR;
      }
   };
   
   /**
    * Special unmarshaller instance indicating an array type.
    */
   protected final static Unmarshaller ARRAY_UNMSH = new Unmarshaller(SOAP.ENC_URI, "Array")
   {
      public boolean isMultiRef()
      {
         return true;
      }
   };

   /**
    * Map of URI and type name to unmarshaller: Unmarshaller[String][String].
    */
   protected final static Lookup2D s_unmshMap = new HashTab2D(32);

   static
   {
      Unmarshaller[] unmshArray = new Unmarshaller[]
      {
         STRING_UNMSH,
         BINARY_UNMSH,
         INT_UNMSH,
         LONG_UNMSH,
         INTEGER_UNMSH,
         DECIMAL_UNMSH,
         FLOAT_UNMSH,
         DOUBLE_UNMSH,
         TIMESTAMP_UNMSH,
         BOOLEAN_UNMSH,
         CHARACTER_UNMSH,
         LIST_UNMSH,
         STRINGID_UNMSH,
         SYMBOL_UNMSH,
         LOCALE_UNMSH,
         TIME_ZONE_UNMSH,
         EXPRESSION_UNMSH,
         PAIR_UNMSH,
         PRIVILEGE_SET_UNMSH,
         BYTE_VECTOR_UNMSH,
         FUNCTION_UNMSH,
         MACRO_UNMSH,
         OID_UNMSH,
         TRANSFER_OBJECT_UNMSH,
         INVOCATION_UNMSH,
         REQUEST_UNMSH,
         RESPONSE_UNMSH,
         EXCEPTION_UNMSH,
         SOAP_REQUEST_UNMSH,
         SOAP_RESPONSE_UNMSH,
         SOAP_FAULT_UNMSH,
         ARRAY_UNMSH
      };

      for (int i = 0; i < unmshArray.length; ++i)
      {
         Unmarshaller unmsh = unmshArray[i];

         s_unmshMap.put(unmsh.getURI(), unmsh.getType(), unmsh);
      }

      s_unmshMap.put(SOAP.ENC_NS, "base64", BINARY_UNMSH);
   }

   // attributes

   /**
    * Count of fixup entries.
    */
   protected int m_nFixupCount;
   
   /**
    * The stack top.
    */
   protected int m_nTop;
   
   /**
    * True if the current element should be empty.
    */
   protected boolean m_bEmpty;
   
   /**
    * True if the current element should be ignored.
    * Used by the env:Body unmarshaller.
    */
   protected boolean m_bIgnoreRoot;

   // associations

   /**
    * The fixup array - contains linked lists of object and reference fixups.
    * Object Fixup:
    *                 0          1                 nObj        nObj+1     nObj+2    
    * m_fixupArray: [null     ][NULL_UNMSH]...[Object/null][Unmarshaller][Temp+0][Temp+1]...[Temp+n] ...
    *                 0                            nObj        nObj+1     nObj+2
    * m_indexArray: [nFirstObj][0         ]...[nNextObj   ][nFirstRFixup][Flag+0][Flag+0]...[Flag+n] ...
    * 
    * Reference Fixup:
    *                      nFixup    nFixup+1
    * m_fixupArray: ... [href/ref  ][accessor] ...
    *                      nFixup    nFixup+1
    * m_indexArray: ... [nNextFixup][nCookie ] ...
    * 
    * NOTE: All the drudgery with linked lists and offsets in arrays is done
    * for efficiency reasons, to avoid excessive memory allocation.
    */
   protected Object[] m_fixupArray;
   
   /**
    * The index array - links the fixup elements into a list.
    * Used during unmarshalling.
    * @see SOAPUnmarshaller#m_fixupArray
    */
   protected int[] m_indexArray;
   
   /**
    * Map of object to id: id[Object].
    */
   protected Lookup m_idMap;

   /**
    * The parser state stack.
    */
   protected Object[] m_stack;

   /**
    * Buffer for element values.
    */
   protected StringBuffer m_valueBuf;
   
   /**
    * Map of namespace name to a list of URI elements,
    * the first element containing the current URI: Namespace[String].
    */
   protected Lookup m_nsMap;
   
   /**
    * Map of array item unmarshaller to array unmarshaller: CollectionUnmarshaller[Unmarshaller].
    */
   protected Lookup m_arrayUnmshMap;

   /**
    * The runtime context.
    */
   protected Context m_context;
   
   // constructors

   /**
    * Constructs the SOAP unmarshaller.
    * @param context The runtime context.
    */
   public SOAPUnmarshaller(Context context)
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
    * @see nexj.core.rpc.CharacterStreamMarshaller#deserialize(java.io.Reader)
    */
   public Object deserialize(Reader reader) throws IOException, MarshallerException
   {
      // Initialize
      m_nFixupCount = 2;
      m_nTop = 0;
      m_bEmpty = false;
      m_bIgnoreRoot = false;
      m_fixupArray = new Object[48];
      m_indexArray = new int[48];
      m_stack = new Object[48];
      m_idMap = new HashTab();
      m_nsMap = new HashTab();
      m_arrayUnmshMap = new HashTab();
      m_arrayUnmshMap.put(CHARACTER_UNMSH, CHAR_ARRAY_UNMSH);
      m_arrayUnmshMap.put(STRING_UNMSH, STRING_ARRAY_UNMSH);
      m_arrayUnmshMap.put(TRANSFER_OBJECT_UNMSH, TRANSFER_OBJECT_ARRAY_UNMSH);
      m_arrayUnmshMap.put(TRANSFER_OBJECT_ARRAY_UNMSH, TRANSFER_OBJECT_ARRAY_ARRAY_UNMSH);
      m_valueBuf = new StringBuffer(32);

      int nRoot = SOAP_MESSAGE_UNMSH.init(null, this);
      
      linkObjectFixup(nRoot);
      push(Primitive.createInteger(nRoot), SOAP_MESSAGE_UNMSH, SOAP_MESSAGE_UNMSH.getAccessor(0));

      try
      {
         XMLUtil.parse(reader, this);
         fixup();
         SOAP_MESSAGE_UNMSH.complete(nRoot, this);

         return getFixup(nRoot);
      }
      catch (SOAPUnmarshallerException e)
      {
         throw e;
      }
      catch (XMLParserException e)
      {
         throw new SOAPUnmarshallerException("err.rpc.soap.xml", e);
      }
      catch (XMLException e)
      {
         if (e.getCause() instanceof SOAPUnmarshallerException)
         {
            throw (SOAPUnmarshallerException)e.getCause();
         }

         throw new SOAPUnmarshallerException("err.rpc.soap.failed", e.getCause());
      }
      finally
      {
         m_fixupArray = null;
         m_indexArray = null;
         m_stack = null;
         m_idMap = null;
         m_nsMap = null;
         m_arrayUnmshMap = null;
         m_valueBuf = null;
      }
   };

   /**
    * Fixes up the outstanding object references.
    */
   protected void fixup()
   {
      // Fixup the object references in multiple passes until done
      int nPrevObj, nObj;
      boolean bDirty = true;

      while ((nObj = m_indexArray[nPrevObj = 0]) != 0)
      {
         if (!bDirty)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.invalidCircularReference");
         }
         
         bDirty = false;

         do
         {
            int nPrevFixup = nObj + FIRST_FIXUP_OFS;
            int nFixup = m_indexArray[nPrevFixup];

            while (nFixup != 0)
            {
               Object ref = m_fixupArray[nFixup + REF_OFS];

               // If string href, lookup the object index
               if (!(ref instanceof Integer))
               {
                  ref = m_idMap.get(ref);

                  if (ref == null)
                  {
                     throw new SOAPUnmarshallerException("err.rpc.soap.idLookup",
                        new Object[]{m_fixupArray[nFixup + REF_OFS]});
                  }

                  m_fixupArray[nFixup + REF_OFS] = ref;
               }

               int nRefObj = ((Integer)ref).intValue();
               Accessor accessor = (Accessor)m_fixupArray[nFixup + ACCESSOR_OFS];

               // If the referenced object is already fully constructed,
               // i.e. it has no more reference fixups, or the accessor
               // is not deferred and the object is partially constructed, 
               // resolve and remove this reference fixup
               if (m_indexArray[nRefObj + FIRST_FIXUP_OFS] == 0 ||
                  m_fixupArray[nRefObj] != EMPTY &&
                  !accessor.isDeferred())
               {
                  // if accessor then set value and set accessor to null
                  // if child complete remove from the list

                  if (accessor != null)
                  {
                     accessor.setValue(nObj, m_indexArray[nFixup + COOKIE_OFS], m_fixupArray[nRefObj], this);
                  }

                  nFixup = m_indexArray[nFixup];
                  m_indexArray[nPrevFixup] = nFixup;
                  bDirty = true;
               }
               else
               {
                  // Process the next fixup
                  nPrevFixup = nFixup;
                  nFixup = m_indexArray[nFixup];
               }
            }

            // If no more fixups are left, construct
            // the object and remove it from the list
            if (m_indexArray[nObj + FIRST_FIXUP_OFS] == 0)
            {
               try
               {
                  ((ComplexUnmarshaller)m_fixupArray[nObj + UNMSH_OFS]).complete(nObj, this);
               }
               catch (Exception e)
               {
                  String sId = findId(nObj);
                  
                  throw new SOAPUnmarshallerException("err.rpc.soap.unmshFixup",
                     new Object[]{(sId == null) ? "" : sId}, e);
               }

               // Remove the object from the list
               nObj = m_indexArray[nObj];
               m_indexArray[nPrevObj] = nObj;
               bDirty = true;
            }
            else
            {
               // Process the next object
               nPrevObj = nObj;
               nObj = m_indexArray[nObj];
            }
         }
         while (nObj != 0);
      }
   }
   
   /**
    * @see org.xml.sax.helpers.DefaultHandler#startPrefixMapping(java.lang.String, java.lang.String)
    */
   public void startPrefixMapping(String sNamespace, String sURI) throws SAXException
   {
      super.startPrefixMapping(sNamespace, sURI);

      // Push the URI 
      m_nsMap.put(sNamespace, new Namespace(sURI, (Namespace)m_nsMap.get(sNamespace)));
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#endPrefixMapping(java.lang.String)
    */
   public void endPrefixMapping(String sNamespace) throws SAXException
   {
      Namespace ns = (Namespace)m_nsMap.get(sNamespace);

      // Pop the URI
      if (ns != null)
      {
         m_nsMap.put(sNamespace, ns.next);
      }

      super.endPrefixMapping(sNamespace);
   }
   
   /**
    * Gets the href attribute.
    * @param attributes The attribute collection.
    * @return The value of href attribute or null if not available.
    */
   protected String getHRefAttributeValue(Attributes attributes)
   {
      return attributes.getValue("href");
   }

   /**
    * Gets the id attribute.
    * @param attributes The attribute collection.
    * @return The value of id attribute or null if not available.
    */
   protected String getIdAttributeValue(Attributes attributes)
   {
      return attributes.getValue("id");
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public void startElement(String sURI, String sName, String sQName, Attributes attributes) throws SAXException
   {
      Object u = getUnmarshaller();

      if (!(u instanceof ComplexUnmarshaller))
      {
         throw new SOAPUnmarshallerException("err.rpc.soap.notSimpleElement", new Object[]{sURI, sName});
      }

      if (m_bEmpty)
      {
         throw new SOAPUnmarshallerException("err.rpc.soap.notEmptyElement", new Object[]{sURI, sName});
      }

      ComplexUnmarshaller parentUnmsh = (ComplexUnmarshaller)u;
      Accessor accessor = parentUnmsh.getAccessor(sURI, sName, this);
      Unmarshaller unmsh = accessor.getUnmarshaller(sURI, sName, attributes, this);
      int nParentObj = getObjectIndex().intValue();
      String sNil = attributes.getValue(SOAP.XSI_URI, "nil");

      if (sNil != null)
      {
         if (!sNil.equals("true") && !sNil.equals("1"))
         {
            if (!sNil.equals("false") && !sNil.equals("0"))
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.nil", new Object[]{sNil, sURI, sName});
            }

            sNil = null;
         }
      }

      if (sNil != null) 
      {
         accessor.setValue(nParentObj, null, this);
         m_bEmpty = true;
      }
      else
      {
         String sHRef = getHRefAttributeValue(attributes);
         
         if (sHRef != null)
         {
            if (sHRef.length() < 2 || sHRef.charAt(0) != '#')
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.href", new Object[]{sHRef, sURI, sName});
            }
            
            if (unmsh != null && !unmsh.isMultiRef())
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.multiRef", new Object[]{sURI, sName});
            }
            
            accessor.setFixup(nParentObj, sHRef.substring(1), this);
            m_bEmpty = true;
         }
         else
         {
            if (unmsh == null)
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.missingType", new Object[]{sURI, sName});
            }

            String sId = (unmsh.isMultiRef()) ? getIdAttributeValue(attributes) : null;
            Integer ref = null;
   
            if (unmsh instanceof ComplexUnmarshaller)
            {
               ref = Primitive.createInteger(((ComplexUnmarshaller)unmsh).init(attributes, this));
            }
            else
            {
               if (sId != null)
               {
                  ref = Primitive.createInteger(addObjectFixup(EMPTY, unmsh, 0));
               }
            }

            if (sId != null)
            {
               if (m_idMap.put(sId, ref) != null)
               {
                  throw new SOAPUnmarshallerException("err.rpc.soap.dupId", new Object[]{sId, sURI, sName});
               }
            }
   
            push(ref, unmsh, accessor);
         }
      }

      m_valueBuf.setLength(0);
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
    */
   public void characters(char[] cbuf, int nOffset, int nCount) throws SAXException
   {
      m_valueBuf.append(cbuf, nOffset, nCount);
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public void endElement(String sURI, String sName, String sQName) throws SAXException
   {
      if (m_bEmpty)
      {
         // An empty element was expected (e.g. xsi:nil or xml:href)
         m_bEmpty = false;
      }
      else
      {
         Object u = getUnmarshaller();
         Accessor accessor = getAccessor();
         Integer ref = getObjectIndex();

         pop();

         if (u instanceof ComplexUnmarshaller)
         {
            // Complex type
            int nObj = ref.intValue();

            if (m_indexArray[nObj + FIRST_FIXUP_OFS] == 0)
            {
               // This object does not depend on any incomplete children (i.e. all children are complete).
               // No fixups, create the object and set the accessor value.
               try
               {
                  completeComplexObject(accessor, (ComplexUnmarshaller)u, ref);
               }
               catch (Exception e)
               {
                  String sId = findId(nObj);
                  
                  throw new SOAPUnmarshallerException("err.rpc.soap.unmshComplex",
                     new Object[]{sURI, sName, (sId == null) ? "" : sId}, e);
               }
            }
            else
            {
               deferComplexObject(accessor, (ComplexUnmarshaller)u, ref);
            }
         }
         else
         {
            // Simple type

            try
            {
               completeSimpleObject(accessor, (SimpleUnmarshaller)u, ref);
            }
            catch (Exception e)
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.unmshSimple",
                  new Object[]{sURI, sName, (m_valueBuf.length() < 128)
                     ? m_valueBuf.toString()
                     : m_valueBuf.substring(0, 128) + "..."}, e);
            }
         }
      }
   }

   /**
    * Completes the instantiation of a complex type.
    * @param accessor The destination accessor.
    * @param unmsh The object unmarshaller.
    * @param ref The source object reference.
    */
   protected void completeComplexObject(Accessor accessor,
      ComplexUnmarshaller unmsh, Integer ref)
   {
      int nObj = ref.intValue();

      unmsh.complete(nObj, this);
      accessor.setValue(getObjectIndex().intValue(), getFixup(nObj), this);
   }

   /**
    * Defers the instantiation of a complex type and sets up the fixups.
    * @param accessor The destination accessor.
    * @param unmsh The object unmarshaller.
    * @param ref The source object reference.
    */
   protected void deferComplexObject(Accessor accessor,
      ComplexUnmarshaller unmsh, Integer ref)
   {
      // Add a reference fixup and an object fixup
      accessor.setFixup(getObjectIndex().intValue(), ref, this);
      linkObjectFixup(ref.intValue());
   }

   /**
    * Completes the instantiation of a simple type.
    * @param accessor The destination accessor.
    * @param unmsh The object unmarshaller.
    * @param ref The source object reference.
    */
   protected void completeSimpleObject(Accessor accessor,
      SimpleUnmarshaller unmsh, Integer ref) throws Exception
   {
      accessor.setValue(getObjectIndex().intValue(),
         unmsh.unmarshal(m_valueBuf.toString(), this), this);
   }

   /**
    * Finds the object id by object index.
    * @param nObj The object index.
    * @return The object id, or null if not found.
    */
   protected String findId(int nObj)
   {
      for (Lookup.Iterator itr = m_idMap.valueIterator(); itr.hasNext();)
      {
         if (((Integer)itr.next()).intValue() == nObj)
         {
            return (String)itr.getKey();
         }
      }
      
      return null;
   }
   
   /**
    * Pushes the current parser state on the parser stack.
    * @param ref The object index.
    * @param unmsh The object unmarshaller.
    * @param accessor The object accessor.
    */
   protected void push(Integer ref, Unmarshaller unmsh, Accessor accessor)
   {
      if (m_nTop + STK_STATE_SIZE > m_stack.length)
      {
         Object[] stack = new Object[(m_nTop + STK_STATE_SIZE) << 1];
         
         System.arraycopy(m_stack, 0, stack, 0, m_nTop);
         m_stack = stack;
      }

      m_stack[m_nTop + STK_REF_OFS] = ref;
      m_stack[m_nTop + STK_UNMSH_OFS] = unmsh;
      m_stack[m_nTop + STK_ACCESSOR_OFS] = accessor;
      
      m_nTop += STK_STATE_SIZE;
   }

   /**
    * Pops the parser state from the stack.
    */
   protected void pop()
   {
      m_nTop -= STK_STATE_SIZE;
   }
   
   /**
    * @return The index of the current object from the top of the stack.
    */
   protected Integer getObjectIndex()
   {
      return (Integer)m_stack[m_nTop + (STK_REF_OFS - STK_STATE_SIZE)];
   }
   
   /**
    * @return The current object unmarshaller from the top of the stack.
    */
   protected Object getUnmarshaller()
   {
      return m_stack[m_nTop + (STK_UNMSH_OFS - STK_STATE_SIZE)];
   }
   
   /**
    * @return The current accessor from the top of the stack.
    */
   protected Accessor getAccessor()
   {
      return (Accessor)m_stack[m_nTop + (STK_ACCESSOR_OFS - STK_STATE_SIZE)];
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
         Object[] fixupArray = new Object[(m_nFixupCount + nSize) << 1];
         
         System.arraycopy(m_fixupArray, 0, fixupArray, 0, m_nFixupCount);
         m_fixupArray = fixupArray;

         int[] indexArray = new int[fixupArray.length];

         System.arraycopy(m_indexArray, 0, indexArray, 0, m_nFixupCount);
         m_indexArray = indexArray;
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
    * Adds an object fixup to the fixup array.
    * @param obj The constructed object.
    * @param unmsh The object unmarshaller.
    * @param nTempSize Temporary variable count.
    */
   protected int addObjectFixup(Object obj, Unmarshaller unmsh, int nTempSize)
   {
      int i = addFixup(2 + nTempSize);

      setFixup(i, obj);
      setFixup(i + UNMSH_OFS, unmsh);

      return i;
   }

   /**
    * Links the object fixup into the fixup list.
    * @param nObj The index of the object in the fixup array.
    */
   protected void linkObjectFixup(int nObj)
   {
      assert m_indexArray[nObj] == 0;
      
      m_indexArray[nObj] = m_indexArray[0];
      m_indexArray[0] = nObj;
   }

   /**
    * Adds a reference fixup to an object fixup.
    * @param nObj The index of the object in the fixup array.
    * @param ref The reference value - string id or object index.
    * @param accessor The member accessor.
    * @param nCookie The fixup cookie.
    * @return The fixup index.
    */
   protected int addReferenceFixup(int nObj, Object ref, Accessor accessor, int nCookie)
   {
      int i = addFixup(2);
      
      setFixup(i + REF_OFS, ref);
      setFixup(i + ACCESSOR_OFS, accessor);

      m_indexArray[i] = m_indexArray[nObj + FIRST_FIXUP_OFS];
      m_indexArray[nObj + FIRST_FIXUP_OFS] = i;
      m_indexArray[i + COOKIE_OFS] = nCookie;

      return i;
   }

   /**
    * Sets a temporary variable for a given unmarshalled object.
    * @param nObj The index of the object in the fixup array.
    * @param nTemp The index of the temporary variable.
    * @param value The value to set.
    */
   protected void setTempValue(int nObj, int nTemp, Object value)
   {
      m_fixupArray[nObj + TEMP_OFS + nTemp] = value;
   }

   /**
    * Gets a temporary variable for a given unmarshalled object.
    * @param nObj The index of the object in the fixup array.
    * @param nTemp The index of the temporary variable.
    * @return The variable value.
    */
   protected Object getTempValue(int nObj, int nTemp)
   {
      return m_fixupArray[nObj + TEMP_OFS + nTemp];
   }

   /**
    * Sets a temporary index for a given unmarshalled object.
    * @param nObj The index of the object in the fixup array.
    * @param nTemp The index of the temporary variable.
    * @param i The value to set.
    */
   protected void setTempIndex(int nObj, int nTemp, int i)
   {
      m_indexArray[nObj + TEMP_OFS + nTemp] = i;
   }

   /**
    * Gets a temporary index for a given unmarshalled object.
    * @param nObj The index of the object in the fixup array.
    * @param nTemp The index of the temporary variable.
    * @return The index value.
    */
   protected int getTempIndex(int nObj, int nTemp)
   {
      return m_indexArray[nObj + TEMP_OFS + nTemp];
   }

   /**
    * Gets an array unmarshaller by array type name.
    * @param sArrayQType The array qualified type name (ns:type[]...[n]).
    * @param nLength The length of the sArrayQType to take into account.
    * @return The array unmarshaller.
    * @throws SOAPUnmarshallerException if no unmarshaller is found.
    */
   protected Unmarshaller getArrayUnmarshaller(String sArrayQType, int nLength) throws SOAPUnmarshallerException
   {
      int i = sArrayQType.lastIndexOf('[', nLength - 1);
      
      if (i < 0)
      {
         throw new SOAPUnmarshallerException("err.rcp.soap.arrayType", new Object[]{sArrayQType});
      }

      Unmarshaller unmsh;

      if (sArrayQType.lastIndexOf(']', i - 1) < 0)
      {
         int k = sArrayQType.indexOf(']', i + 1);

         if (k < 0 || k >= nLength || k == nLength - 1 && i == k)
         {
            throw new SOAPUnmarshallerException("err.rcp.soap.arrayType", new Object[]{sArrayQType});
         }

         String sQType = sArrayQType.substring(0, i);
         String sType =  getTypeName(sQType);
         String sURI = getTypeURI(sQType, sType);
         
         if (sType.equals("anyType") && sURI.equals(SOAP.XSD_URI))
         {
            return OBJECT_ARRAY_UNMSH;
         }
         
         unmsh = getUnmarshaller(sURI, sType, (Attributes)null);
      }
      else
      {
         unmsh = getArrayUnmarshaller(sArrayQType, i);
      }

      Unmarshaller arrayUnmsh = (Unmarshaller)m_arrayUnmshMap.get(unmsh);

      if (arrayUnmsh == null)
      {
         arrayUnmsh = new ObjectArrayUnmarshaller(unmsh);
         m_arrayUnmshMap.put(unmsh, arrayUnmsh);
      }

      return arrayUnmsh;
   }

   /**
    * Gets an array unmarshaller by element attributes.
    * @param attributes The element attributes.
    * @return The array unmarshaller, or null if the type is not specified
    * @throws SOAPUnmarshallerException if the type is unsupported.
    */
   protected Unmarshaller getArrayUnmarshaller(Attributes attributes) throws SOAPUnmarshallerException
   {
      String sArrayQType = attributes.getValue(SOAP.ENC_URI, "arrayType");

      if (sArrayQType != null)
      {
         return getArrayUnmarshaller(sArrayQType, sArrayQType.length());
      }

      return null;
   }
   
   /**
    * Gets an unmarshaller by type namespace URI and name.
    * @param sURI The namespace URI.
    * @param sType The type name.
    * @param attributes The additional attributes for type lookup. Can be null.
    * @return The unmarshaller.
    * @throws SOAPUnmarshallerException if no unmarshaller is found.
    */
   protected Unmarshaller getUnmarshaller(String sURI, String sType, Attributes attributes) throws SOAPUnmarshallerException
   {
      Unmarshaller unmsh = (Unmarshaller)s_unmshMap.get(sURI, sType);

      if (unmsh == null)
      {
         throw new SOAPUnmarshallerException("err.rpc.soap.unmshType", new Object[]{sURI, sType});
      }

      if (unmsh == ARRAY_UNMSH)
      {
         if (attributes == null)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.itemType");
         }
         
         unmsh = getArrayUnmarshaller(attributes);
         
         if (unmsh == null)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.missingArrayType");
         }
      }

      return unmsh;
   }

   /**
    * Gets an unmarshaller by qualified type name.
    * @param sQType The qualified type name (ns:type).
    * @param attributes The additional attributes for type lookup. Can be null.
    * @return The unmarshaller.
    * @throws SOAPUnmarshallerException if no unmarshaller is found.
    */
   protected Unmarshaller getUnmarshaller(String sQType, Attributes attributes) throws SOAPUnmarshallerException
   {
      String sType = getTypeName(sQType);
      
      return getUnmarshaller(getTypeURI(sQType, sType), sType, attributes);
   }

   /**
    * Gets an unmarshaller by element attributes.
    * @param attributes The attributes for type lookup.
    * @return The unmarshaller, or null if no type is specified.
    * @throws SOAPUnmarshallerException if type is unsupported.
    */
   protected Unmarshaller getUnmarshaller(Attributes attributes) throws SOAPUnmarshallerException
   {
      String sQType = attributes.getValue(SOAP.XSI_URI, "type");
      
      if (sQType != null)
      {
         return getUnmarshaller(sQType, attributes);
      }
      
      return getArrayUnmarshaller(attributes);
   }

   /**
    * Gets the local type name from the qualified type name.
    * @param sQType The qualified type name (ns:type).
    * @return The local type name (type).
    */
   protected static String getTypeName(String sQType)
   {
      int i = sQType.indexOf(':');
      
      if (i < 0)
      {
         return sQType;
      }
      
      return sQType.substring(i + 1);
   }
   
   /**
    * Gets the type URI from the qualified type name.
    * @param sQType The qualified type name (ns:type).
    * @param sName The local type name returned by getTypeName().
    * @return The type URI.
    * @throws SOAPUnmarshallerException if the namespace cannot be found.
    */
   protected String getTypeURI(String sQType, String sName) throws SOAPUnmarshallerException
   {
      if (sQType == sName)
      {
         return "";
      }

      return getURI(sQType.substring(0, sQType.length() - sName.length() - 1));
   }

   /**
    * Gets a URI by namespace name.
    * @param sNamespace The namespace name.
    * @return The namespace URI.
    * @throws SOAPUnmarshallerException if the namespace cannot be found.
    */
   protected String getURI(String sNamespace) throws SOAPUnmarshallerException
   {
      Namespace ns = (Namespace)m_nsMap.get(sNamespace);
      
      if (ns == null)
      {
         throw new SOAPUnmarshallerException("err.rpc.soap.namespaceLookup", new Object[]{sNamespace});
      }

      return ns.uri;
   }

   /**
    * Namespace list element.
    */
   protected static class Namespace
   {
      public String uri;
      public Namespace next;
      
      public Namespace(String sURI, Namespace next)
      {
         this.uri = sURI;
         this.next = next;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return uri;
      }
   };

   /**
    * Interface implemented by unmarshallers.
    */
   protected abstract static class Unmarshaller
   {
      // attributes

      /**
       * The type namespace URI. 
       */
      protected final String m_sURI;
      
      /**
       * The type name.
       */
      protected final String m_sType;

      // constructor

      /**
       * Constructs the unmarshaller.
       * @param sURI The type namespace URI.
       * @param sType The type name.
       */
      protected Unmarshaller(String sURI, String sType)
      {
         m_sURI = sURI;
         m_sType = sType;
      }

      // operations

      /**
       * @return The type namespace URI.
       */
      public String getURI()
      {
         return m_sURI;
      }

      /**
       * @return The type name.
       */
      public String getType()
      {
         return m_sType;
      }

      /**
       * @return True if the unmarshalled object can be referenced multiple times.
       */
      public abstract boolean isMultiRef();

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         String sName = getClass().getName();

         return sName.substring(sName.lastIndexOf('.') + 1) + '(' + m_sURI + ", " + m_sType + ')';
      }
   };

   /**
    * Interface implemented by simple type unmarshallers.
    */
   protected abstract static class SimpleUnmarshaller extends Unmarshaller
   {
      /**
       * Constructs the unmarshaller.
       * @param sURI The type namespace URI.
       * @param sType The type name.
       */
      protected SimpleUnmarshaller(String sURI, String sType)
      {
         super(sURI, sType);
      }

      /**
       * Unmarshals the object.
       * @param sValue The element value.
       * @param unmsh The SOAP unmarshaller.
       * @return The unmarshalled object.
       * @throws Exception if an unmarshalling error occurs.
       */
      public abstract Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception;

      public boolean isMultiRef()
      {
         return false;
      }
   };

   /**
    * Interface implemented by the complex type unmarshallers.
    */
   protected abstract static class ComplexUnmarshaller extends Unmarshaller
   {
      /**
       * Constructs the unmarshaller.
       * @param sURI The type namespace URI.
       * @param sType The type name.
       */
      protected ComplexUnmarshaller(String sURI, String sType)
      {
         super(sURI, sType);
      }

      /**
       * Initializes the unmarshalling structures for a new object, possibly constructing an empty instance.
       * @param attributes The containing element attributes. 
       * @param unmsh The SOAP unmarshaller.
       * @return The index of the object in the fixup array.
       * @throws SOAPUnmarshallerException if an error occurs.
       */
      public abstract int init(Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;

      /**
       * Gets an accessor by namespace URI and type name.
       * @param sURI The namespace URI.
       * @param sType The type name.
       * @param unmsh The SOAP unmarshaller.
       * @return The accessor.
       * @throws SOAPUnmarshallerException if the accessor cannot be found.
       */
      public abstract Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;

      /**
       * Completes the object initialization from temporary data.
       * @param nObj The index of the object in the fixup array.
       * @param unmsh The SOAP unmarshaller.
       * @throws SOAPUnmarshallerException if an error occurs.
       */
      public void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
      }

      public boolean isMultiRef()
      {
         return true;
      }
   };

   /**
    * Interface implemented by complex unmarshaller accessors.
    */
   protected interface Accessor
   {
      /**
       * Sets the (possibly temporary) value of the member of the object.
       * @param nObj The index of the object in the fixup array.
       * @param value The value to set.
       * @param unmsh The SOAP unmarshaller.
       */
      void setValue(int nObj, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;
      
      /**
       * Sets the (possibly temporary) value of the member of the object.
       * @param nObj The index of the object in the fixup array.
       * @param nCookie The fixup cookie.
       * @param value The value to set.
       * @param unmsh The SOAP unmarshaller.
       */
      void setValue(int nObj, int nCookie, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;

      /**
       * Sets a reference fixup for the member of the object.
       * @param nObj The index of the object in the fixup array.
       * @param ref The object reference - string id or integer object index.
       * @param unmsh The SOAP unmarshaller.
       */
      void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh);

      /**
       * Gets the unmarshaller for the member value type.
       * @param sURI The element URI.
       * @param sName The element name.
       * @param attributes The element attributes.
       * @param unmsh The SOAP unmarshaller.
       * @return The unmarshaller, or null if the type could not be determined.
       * @throws SOAPUnmarshallerException if a marshaller for the specified type does not exist.
       */
      Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;
      
      /**
       * @return True if the referenced object must be fully
       * constructed in order to complete the type construction.
       */
      boolean isDeferred();
   };

   /**
    * Generic array unmarshaller.
    */
   protected abstract static class ArrayUnmarshaller extends ComplexUnmarshaller implements Accessor
   {
      protected Unmarshaller m_itemUnmsh;
      
      protected ArrayUnmarshaller(Unmarshaller itemUnmsh)
      {
         super((itemUnmsh == null) ? SOAP.XSD_URI : itemUnmsh.getURI(), 
            (itemUnmsh == null) ? "anyType[]" : itemUnmsh.getType() + "[]");
         m_itemUnmsh = itemUnmsh;
      }
      
      public int init(Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         String sType = attributes.getValue(SOAP.ENC_URI, "arrayType");
         
         if (sType == null)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.missingArrayType");
         }
         
         int i = sType.lastIndexOf('[');
         int k = sType.indexOf(']', i + 1);

         if (i > 0 && k > 0)
         {
            int nSize;
            
            try
            {
               nSize = Integer.parseInt(sType.substring(i + 1, k));
            }
            catch (Throwable t)
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.arrayType", new Object[]{sType}, t);
            }
            
            if (nSize >= 0)
            {
               return init(nSize, unmsh);
            }
         }
         
         throw new SOAPUnmarshallerException("err.rpc.soap.arrayType", new Object[]{sType});
      }
      
      public Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if ("".equals(sURI) && "Item".equals(sType))
         {
            return this;
         }
         
         throw new SOAPUnmarshallerException("err.rpc.soap.arrayItem", new Object[]{sURI, sType, m_sURI, m_sType});
      }

      public void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh)
      {
         unmsh.addReferenceFixup(nObj, ref, this, addValue(nObj, unmsh));
      }

      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (m_itemUnmsh != null)
         {
            return m_itemUnmsh;
         }

         return unmsh.getUnmarshaller(attributes);
      }

      public boolean isDeferred()
      {
         return false;
      }

      /**
       * Initializes the array.
       * @param nSize The array size.
       * @param unmsh The SOAP unmarshaller.
       * @return The index of the object in the fixup array.
       */
      protected abstract int init(int nSize, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;

      /**
       * Reserves an empty item in the array.
       * @param nObj The index of the object in the fixup array.
       * @param unmsh The SOAP unmarshaller.
       * @return The reserved index.
       */
      protected abstract int addValue(int nObj, SOAPUnmarshaller unmsh);
   };

   /**
    * Array unmarshaller into Object[].
    */
   protected static class ObjectArrayUnmarshaller extends ArrayUnmarshaller
   {
      protected ObjectArrayUnmarshaller(Unmarshaller itemUnmsh)
      {
         super(itemUnmsh);
      }

      protected final int init(int nSize, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         int nObj = unmsh.addObjectFixup(construct(nSize), this, 1);

         unmsh.setTempIndex(nObj, 0, 0);

         return nObj;
      }

      protected Object[] construct(int nSize)
      {
         return new Object[nSize];
      }
      
      protected final int addValue(int nObj, SOAPUnmarshaller unmsh)
      {
         int i = unmsh.getTempIndex(nObj, 0);
         
         unmsh.setTempIndex(nObj, 0, i + 1);
         
         return i;
      }

      public final void setValue(int nObj, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((Object[])unmsh.getFixup(nObj))[addValue(nObj, unmsh)] = value;
      }

      public final void setValue(int nObj, int nCookie, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ((Object[])unmsh.getFixup(nObj))[nCookie] = value;
      }

      public boolean isDeferred()
      {
         return false;
      }
   };
   
   /**
    * Generic object accessor.
    */
   protected abstract static class ObjectAccessor implements Accessor
   {
      // constants
      
      /**
       * Constant to indicate that the value has been encountered.
       */
      protected final static int ENCOUNTERED = 1;
      
      // attributes

      /**
       * The namespace URI.
       */
      protected final String m_sURI;
      
      /**
       * The element name.
       */
      protected final String m_sElement;
      
      /**
       * The value ordinal number.
       */
      protected int m_nOrdinal;

      /**
       * True to indicate that the value is required.
       */
      protected final boolean m_bRequired;
      
      /**
       * Deferral flag.
       */
      protected final boolean m_bDeferred;
      
      // associations
      
      /**
       * The parent type unmarshaller.
       */
      protected ObjectUnmarshaller m_parentUnmsh;

      // constructors

      /**
       * Constructs the accessor.
       * @param sURI The namespace URI.
       * @param sElement The element name.
       * @param bRequired True if the value is required.
       * @param bDeferred True if the accessor is deferred.
       */
      public ObjectAccessor(String sURI, String sElement, boolean bRequired, boolean bDeferred)
      {
         m_sURI = sURI;
         m_sElement = sElement;
         m_bRequired = bRequired;
         m_bDeferred = bDeferred;
      }

      // operations

      /**
       * @return The element namespace URI.
       */
      public String getURI()
      {
         return m_sURI;
      }
      
      /**
       * @return The element name.
       */
      public String getElement()
      {
         return m_sElement;
      }
      
      /**
       * Sets the parent type unmarshaller.
       * @param unmsh The parent type unmarshaller.
       */
      public void setParentUnmarshaller(ObjectUnmarshaller unmsh)
      {
         m_parentUnmsh = unmsh;
      }
      
      /**
       * Sets the ordinal number of the accessor.
       * @param nOrdinal The accessor ordinal number.
       */
      public void setOrdinal(int nOrdinal)
      {
         m_nOrdinal = nOrdinal;
      }
      
      public void setValue(int nObj, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         encountered(nObj, unmsh);
         unmsh.setTempValue(nObj, m_nOrdinal, value);
      }

      public void setValue(int nObj, int nCookie, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         unmsh.setTempValue(nObj, m_nOrdinal, value);
      }

      public void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh)
      {
         encountered(nObj, unmsh);
         unmsh.addReferenceFixup(nObj, ref, this, 0);
      }

      public boolean isDeferred()
      {
         return m_bDeferred;
      }

      /**
       * Invoked when the value has been encountered.
       * @param nObj The index of the object fixup in the fixup array.
       * @param unmsh The SOAP unmarshaller.
       * @throws SOAPUnmarshallerException if the value has been encountered too many times.
       */
      protected void encountered(int nObj, SOAPUnmarshaller unmsh) throws SOAPMarshallerException 
      {
         if (unmsh.getTempIndex(nObj, m_nOrdinal) == ENCOUNTERED)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.dupElement",
               new Object[]{m_sURI, m_sElement, m_parentUnmsh.getURI(), m_parentUnmsh.getType()});
         }

         unmsh.setTempIndex(nObj, m_nOrdinal, ENCOUNTERED);
      }

      /**
       * Validates the value.
       * @param nObj The index of the object fixup in the fixup array.
       * @param unmsh The SOAP unmarshaller.
       * @throws SOAPUnmarshallerException if the value is invalid.
       */
      protected void validate(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (m_bRequired && unmsh.getTempValue(nObj, m_nOrdinal) == null)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.required",
               new Object[]{m_sURI, m_sElement, m_parentUnmsh.getURI(), m_parentUnmsh.getType()});
         }
      }
   };

   /**
    * Object accessor for a static type.
    */
   protected static class StaticObjectAccessor extends ObjectAccessor
   {
      /**
       * The value unmarshaller.
       */
      protected Unmarshaller m_unmsh;
      
      public StaticObjectAccessor(String sURI, String sElement, boolean bRequired, boolean bDeferred, Unmarshaller unmsh)
      {
         super(sURI, sElement, bRequired, bDeferred);
         m_unmsh = unmsh;
      }
      
      public void setUnmarshaller(Unmarshaller unmsh)
      {
         m_unmsh = unmsh;
      }

      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return m_unmsh;
      }
   };
   
   /**
    * Object accessor for xsd:anyType.
    */
   protected static class DynamicObjectAccessor extends ObjectAccessor
   {
      public DynamicObjectAccessor(String sURI, String sElement, boolean bRequired, boolean bDeferred)
      {
         super(sURI, sElement, bRequired, bDeferred);
      }

      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return unmsh.getUnmarshaller(attributes);
      }
   };
   
   /**
    * Object accessor for an object array.
    */
   protected static class ObjectArrayAccessor extends ObjectAccessor
   {
      public ObjectArrayAccessor(String sURI, String sElement, boolean bRequired, boolean bDeferred)
      {
         super(sURI, sElement, bRequired, bDeferred);
      }

      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return unmsh.getArrayUnmarshaller(attributes);
      }
   };

   /**
    * Accessor that consumes all the elements.
    */
   protected final static class NullAccessor implements Accessor
   {
      public void setValue(int nObj, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
      }

      public void setValue(int nObj, int nCookie, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
      }

      public void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh)
      {
      }

      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return NULL_UNMSH;
      }

      public boolean isDeferred()
      {
         return false;
      }
   };

   /**
    * Object/structure unmarshaller.
    */
   protected abstract static class ObjectUnmarshaller extends ComplexUnmarshaller
   {
      // associations

      /**
       * The object accessors.
       */
      protected final ObjectAccessor[] m_accessorArray;

      /**
       * Map of URI:Element to accessor.
       */
      protected final Lookup2D m_accessorMap;

      // constructors

      /**
       * Constructs the unmarshaller.
       * @param sURI The type namespace URI.
       * @param sType The type name.
       */
      public ObjectUnmarshaller(String sURI, String sType, ObjectAccessor[] accessorArray)
      {
         super(sURI, sType);
         m_accessorArray = accessorArray;
         m_accessorMap = new HashTab2D(accessorArray.length);

         for (int i = 0; i < accessorArray.length; ++i)
         {
            ObjectAccessor accessor = m_accessorArray[i];

            accessor.setParentUnmarshaller(this);
            accessor.setOrdinal(i);
            m_accessorMap.put(accessor.getURI(), accessor.getElement(), accessor);
         }
      }

      // operations

      public final ObjectAccessor getAccessor(int i)
      {
         return m_accessorArray[i];
      }

      public final int init(Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return unmsh.addObjectFixup(construct(), this, m_accessorArray.length);
      }

      public Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Accessor accessor = (Accessor)m_accessorMap.get(sURI, sType);

         if (accessor == null)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.element", new Object[]{sURI, sType, m_sURI, m_sType});
         }

         return accessor;
      }

      public final void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         for (int i = 0; i != m_accessorArray.length; ++i)
         {
            m_accessorArray[i].validate(nObj, unmsh);
         }

         finish(nObj, unmsh);
      }

      /**
       * @return An empty object.
       */
      protected Object construct()
      {
         return EMPTY;
      }

      /**
       * Template method to complete the object creation.
       */
      protected abstract void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException;
   };
   
   /**
    * Object/structure unmarshaller that skips unrecognized elements.
    */
   protected abstract static class ObjectSubsetUnmarshaller extends ObjectUnmarshaller
   {
      public ObjectSubsetUnmarshaller(String sURI, String sType, ObjectAccessor[] accessorArray)
      {
         super(sURI, sType, accessorArray);
      }

      public Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Accessor accessor = (Accessor)m_accessorMap.get(sURI, sType);

         if (accessor == null)
         {
            return NULL_ACCESSOR;
         }

         return accessor;
      }
   };
   
   /**
    * env:Body unmarshaller.
    */
   protected final static class SOAPBodyUnmarshaller extends ComplexUnmarshaller implements Accessor
   {
      public SOAPBodyUnmarshaller()
      {
         super(SOAP.ENV_URI, "Body");
      }

      public int init(Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         String sEncoding = attributes.getValue(SOAP.ENV_URI, "encodingStyle");
         
         if (!SOAP.ENC_URI.equals(sEncoding))
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.encodingStyle", new Object[]{sEncoding});
         }

         return unmsh.addObjectFixup(EMPTY, this, 1);
      }

      public Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return this;
      }

      public void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (unmsh.getTempIndex(nObj, 0) == 0)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.root");
         }

         unmsh.setFixup(nObj, unmsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }

      public void setValue(int nObj, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (!unmsh.m_bIgnoreRoot)
         {
            setValue(nObj, 0, value, unmsh);
         }
         
         setIgnoreRoot(nObj, unmsh);
      }

      public void setValue(int nObj, int nCookie, Object value, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (unmsh.getTempValue(nObj, 0) instanceof SOAPFault)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.dupFault", new Object[]{m_sURI, m_sType});
         }

         unmsh.setTempValue(nObj, 0, value);
         unmsh.setTempIndex(nObj, 0, 1);
      }

      public void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh)
      {
         if (ref instanceof String)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.hrefTop", new Object[]{m_sURI, m_sType});
         }
         
         if (!unmsh.m_bIgnoreRoot)
         {
            unmsh.addReferenceFixup(nObj, ref, this, 0);
         }
         
         setIgnoreRoot(nObj, unmsh);
      }

      protected void setIgnoreRoot(int nObj, SOAPUnmarshaller unmsh)
      {
         if (unmsh.m_bIgnoreRoot)
         {
            if (unmsh.getTempIndex(nObj, 0) == 0)
            {
               unmsh.m_bIgnoreRoot = false;
            }
         }
         else
         {
            unmsh.setTempIndex(nObj, 0, 1);
            unmsh.m_bIgnoreRoot = true;
         }
      }
      
      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Unmarshaller u = unmsh.getUnmarshaller(attributes);
         
         if (u == null)
         {
            u = unmsh.getUnmarshaller(sURI, sName, attributes);
         }

         if (unmsh.m_bIgnoreRoot)
         {
            if (u == SOAP_FAULT_UNMSH)
            {
               unmsh.m_bIgnoreRoot = false;
            }
         }
         else
         {
            if ("0".equals(attributes.getValue(SOAP.ENV_URI, "root")) && u != SOAP_FAULT_UNMSH)
            {
               unmsh.m_bIgnoreRoot = true;
            }
         }

         return u;
      }

      public boolean isDeferred()
      {
         return false;
      }
   };
}
