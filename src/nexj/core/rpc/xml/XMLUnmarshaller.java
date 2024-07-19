// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.Type;
import nexj.core.persistence.OID;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAP;
import nexj.core.rpc.soap.SOAPFault;
import nexj.core.rpc.soap.SOAPMarshallerException;
import nexj.core.rpc.soap.SOAPUnmarshaller;
import nexj.core.rpc.soap.SOAPUnmarshallerException;
import nexj.core.runtime.Context;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeMacro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.EmptyIterator;
import nexj.core.util.GenericException;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.LocaleUtil;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.XMLException;
import nexj.core.util.XMLParserException;
import nexj.core.util.XMLUtil;

/**
 * Document/literal SOAP/XML unmarshaller.
 * Shares most basic objects with SOAPUnmarshaller hence extends it to use them.
 * Needs to extend SOAPUnmarshaller to gain access to protected unmarshalling types/objects.
 */
public class XMLUnmarshaller extends SOAPUnmarshaller
{
   /**
    * For any array types MS.NET just prefixes the type with "ArrayOf" and will camel-case the first
    * letter of the type. MS.NET will refuse to use any array types defined in the XSD, even if an
    * exact match is available.
    */
   protected final static String ARRAY_PREFIX = "ArrayOf";

   /**
    * The prefix used for distinguishing a regular OID from a temporary one in the marshaled XML
    * stream.
    * Note: The temporary OID is not used/passed outside the marshaller/unmarshaller.
    */
   protected final static String TEMPORARY_OID_PREFIX = "@";

   /**
    * Unmarshaller for a char-array XML type to char[].
    */
   protected final static Unmarshaller CHAR_ARRAY_UNMSH = new ElementListUnmarshaller(
         new CharacterListAccessor(XML.getTNSType("item")), XML.NS_URI_TNS, "unsignedShort-array")
   {
      /**
       * Need to move constructed list into final place.
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ComplexUnmarshaller
       *      #complete(int, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, ((CharacterListAccessor)m_accessor).getCharArray(nObj, unmrsh));
      }
   };
   
   /**
    * Unmarshaller for Change-Request objects without a metaclass.
    */
   protected final static Unmarshaller DYNAMIC_CHANGE_REQUEST_UNMSH = new DynamicChangeRequestUnmarshaller(null);
   
   /**
    * Unmarshaller for Read-Request objects without a metaclass.
    */
   protected final static Unmarshaller DYNAMIC_READ_REQUEST_UNMSH = new DynamicReadRequestUnmarshaller(null);

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#EXPRESSION_UNMSH
    */
   protected final static Unmarshaller EXPRESSION_UNMSH =
      new ObjectUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "Expression",
      new ObjectAccessor[]
      {
         new StaticObjectAccessor(XML.NS_URI_TNS, "text", true, true, STRING_UNMSH)
      })
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, new SchemeParser(unmsh.getContext().getMachine().getGlobalEnvironment())
            .parse(new StringReader((String)unmrsh.getTempValue(nObj, 0)), null));
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#MACRO_UNMSH
    */
   protected final static Unmarshaller FUNCTION_UNMSH = new ObjectUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "Function",
      new ObjectAccessor[]
      {
         new CharacterListAccessor(XML.getTNSType("code")),
         new ObjectListAccessor(XML.getTNSType("constants"), false, false, null, true)
      })
   {
      protected Object construct()
      {
         return new PCodeFunction();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         PCodeFunction fun = (PCodeFunction)unmrsh.getFixup(nObj);

         fun.code = ((CharacterListAccessor)getAccessor(0)).getCharArray(nObj, unmrsh);
         fun.constants = ((List)unmrsh.getTempValue(nObj, 1)).toArray();
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#LOCALE_UNMSH
    */
   protected final static Unmarshaller LOCALE_UNMSH = new ObjectUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "Locale",
      new ObjectAccessor[]{new StaticObjectAccessor(XML.NS_URI_TNS, "name", true, true, STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, LocaleUtil.parse((String)unmrsh.getTempValue(nObj, 0)));
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#MACRO_UNMSH
    */
   protected final static Unmarshaller MACRO_UNMSH = new ObjectUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "Macro",
      new ObjectAccessor[]
      {
         new CharacterListAccessor(XML.getTNSType("code")),
         new ObjectListAccessor(XML.getTNSType("constants"), false, false, null, true)
      })
   {
      protected Object construct()
      {
         return new PCodeMacro();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         PCodeMacro fun = (PCodeMacro)unmrsh.getFixup(nObj);

         fun.code = ((CharacterListAccessor)getAccessor(0)).getCharArray(nObj, unmrsh);
         fun.constants = ((List)unmrsh.getTempValue(nObj, 1)).toArray();
      }
   };

   /**
    * Unmarshaller for Collection<Object>.
    */
   protected final static Unmarshaller OBJECT_LIST_MSH = new ElementListUnmarshaller(null);

   /**
    * Unmarshaller for OID as a HEX string.
    * Will return either a string representing a temporary OID or a valid OID object.
    */
   protected final static Unmarshaller OID_UNMSH = new SimpleUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "OID")
   {
      public Object unmarshal(String value, SOAPUnmarshaller unmsh) throws Exception
      {
         return (value != null && 
                 value.startsWith(TEMPORARY_OID_PREFIX)) ? (Object)value
                                                         : OID.fromBinary(Binary.parse(value));
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#PAIR_UNMSH
    */
   protected final static Unmarshaller PAIR_UNMSH = new ObjectUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "Pair",
      new ObjectAccessor[]
      {
         new DynamicObjectAccessor(XML.NS_URI_TNS, "head", false, false),
         new DynamicObjectAccessor(XML.NS_URI_TNS, "tail", false, false)
      })
   {
      protected Object construct()
      {
         return new Pair(null);
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         Pair pair = (Pair)unmrsh.getFixup(nObj);
         
         pair.setHead(unmrsh.getTempValue(nObj, 0));
         pair.setTail(unmrsh.getTempValue(nObj, 1));
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#PRIVILEGE_SET_UNMSH
    */
   protected final static Unmarshaller PRIVILEGE_SET_UNMSH = new ObjectUnmarshaller(XML.NS_URI_TNS,
      XML.BASE_PREFIX + "PrivilegeSet",
      new ObjectAccessor[]{new StaticObjectAccessor(XML.NS_URI_TNS,
                                                    "mask",
                                                    false,
                                                    true,
                                                    BINARY_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         Binary mask = (Binary)unmrsh.getTempValue(nObj, 0);
         
         unmrsh.setFixup(nObj,
                         (mask == null) ? new PrivilegeSet(0): new PrivilegeSet(mask.getData()));
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#BYTE_VECTOR_UNMSH
    */
   protected final static Unmarshaller BYTE_VECTOR_UNMSH = new ObjectUnmarshaller(XML.NS_URI_TNS,
      XML.BASE_PREFIX + "ByteVector",
      new ObjectAccessor[]{new StaticObjectAccessor(XML.NS_URI_TNS,
                                                    "value",
                                                    false,
                                                    true,
                                                    BINARY_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         Binary binary = (Binary)unmrsh.getTempValue(nObj, 0);
         
         unmrsh.setFixup(nObj,
                         (binary == null) ? null : binary.getData());
      }
   };

   /**
    * Unmarshal a string into a qualified type.
    */
   protected final static Unmarshaller QUALIFIED_STRING_UNMSH =
      new SimpleUnmarshaller(XML.XSD_URI, "string")
   {
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         String sCode = getTypeName(sValue);
         String sURI = ((XMLUnmarshaller)unmsh).getTypeURI(sValue, sCode);

         return new QName(sURI, sCode, sValue.substring(0, sCode.length()));
      }
   };

   /**
    * Unmarshaller for SOAP envelope
    * (need to redefine because SOAPBodyUnmarshaller can't be extended and need access to protected
    *  methods of SOAPUnmarshaller).
    */
   protected final static Unmarshaller SOAP_ENVELOPE_UNMSH =
      new ObjectSubsetUnmarshaller(XML.ENV_URI, "Envelope",
      new ObjectAccessor[]{new StaticObjectAccessor(XML.ENV_URI,
                                                    "Body",
                                                    true,
                                                    false,
                                                    new SOAPBodyUnmarshaller())})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, unmrsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };

   /**
    * Unmarshaller to decode a string array, used by jUnit tests.
    */
   protected final static Unmarshaller STRING_ARRAY_UNMSH =
      new ElementListUnmarshaller(STRING_UNMSH, XML.NS_URI_TNS, "string" + XML.ARRAY_SUFFIX)
   {
      /**
       * @see nexj.core.rpc.rest.XMLUnmarshaller.ListUnmarshaller#finalize(java.util.List)
       */
      protected Object complete(List list)
      {
         return (list == null) ? null : list.toArray(new String[list.size()]);
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#SYMBOL_UNMSH
    */
   protected final static Unmarshaller SYMBOL_UNMSH =
      new ObjectUnmarshaller(XML.NS_URI_TNS,
                             XML.BASE_PREFIX + "Symbol",
                             new ObjectAccessor[]{new StaticObjectAccessor(XML.NS_URI_TNS,
                                                                           "name",
                                                                           true,
                                                                           true,
                                                                           STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, Symbol.define((String)unmrsh.getTempValue(nObj, 0)));
      }
   };

   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#TIME_ZONE_UNMSH
    */
   protected final static Unmarshaller TIME_ZONE_UNMSH =
      new ObjectUnmarshaller(XML.NS_URI_TNS,
                             XML.BASE_PREFIX + "TimeZone",
                             new ObjectAccessor[]{new StaticObjectAccessor(XML.NS_URI_TNS,
                                                                           "name",
                                                                           true,
                                                                           true,
                                                                           STRING_UNMSH)})
   {
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, TimeZone.getTimeZone((String)unmrsh.getTempValue(nObj, 0)));
      }
   };

   /**
    * Unmarshaller for TransferObject.
    */
   protected final static Unmarshaller TRANSFER_OBJECT_UNMSH =
      new MetaclassUnmarshaller(XML.NS_URI_TNS, XML.BASE_PREFIX + "TransferObject", null);

   // declare after the rest as it needs OID_UNMSH declared
   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#EXCEPTION_UNMSH
    */
   protected final static ObjectUnmarshaller EXCEPTION_UNMSH =
      new ObjectUnmarshaller(XML.NS_URI_TNS,
                             XML.BASE_PREFIX + "Exception",
                             new ObjectAccessor[]
      {
         new StaticObjectAccessor(XML.NS_URI_TNS, "errorCode", false, true, STRING_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "errorMessage", false, true, STRING_UNMSH),
         new ObjectListAccessor(XML.getTNSType("errorArgs"), false, false, null, true),
         new StaticObjectAccessor(XML.NS_URI_TNS, "class", false, true,  STRING_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "oid", false, false, OID_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "ordinal", false, true,  INT_UNMSH),
         new ObjectListAccessor(XML.getTNSType("attributes"), false, true,  STRING_UNMSH, false),
         // unmarshaller == EXCEPTION_UNMSH set in construct()
         new ObjectListAccessor(XML.getTNSType("attributeExceptions"), false, true, null, true),
         new ObjectListAccessor(XML.getTNSType("exceptions"), false, true, null, true)
      })
   {
      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectUnmarshaller#construct()
       */
      protected Object construct()
      {  // Have to set self as unmarshaller for sub-exceptions.
         // Can't set in declaration because self has not been declared yet.
         ((ObjectListAccessor)getAccessor(7)).setUnmarshaller(EXCEPTION_UNMSH);
         ((ObjectListAccessor)getAccessor(8)).setUnmarshaller(EXCEPTION_UNMSH);

         return super.construct();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         String sErrCode = (String)unmrsh.getTempValue(nObj, 0);
         String sMessage = (String)unmrsh.getTempValue(nObj, 1);
         
         List argList = (List)unmrsh.getTempValue(nObj, 2);
         Object[] argArray = (argList == null || argList.isEmpty()) ? null : argList.toArray();

         String sClassName = (String)unmrsh.getTempValue(nObj, 3);
         GenericException e;

         if (sClassName != null)
         {
            ValidationException x = new ValidationException(sErrCode, argArray);

            x.setClassName(sClassName);
            x.setOIDHolder((OID)unmrsh.getTempValue(nObj, 4));

            Integer ordinal = (Integer)unmrsh.getTempValue(nObj, 5);

            if (ordinal != null)
            {
               x.setOrdinal(ordinal.intValue());
            }

            List attributeList = (List)unmrsh.getTempValue(nObj, 6);
            List exceptionList = (List)unmrsh.getTempValue(nObj, 7);

            if (((attributeList == null) ? 0 : attributeList.size()) != 
                ((exceptionList == null) ? 0 : exceptionList.size()))
            {
               throw new SOAPUnmarshallerException("err.rpc.soap.attributeExceptionMismatch",
                                                   new Object[]{m_sURI, m_sType});
            }

            if (attributeList != null)
            {
               for (int i = 0, n = attributeList.size(); i < n; ++i)
               {
                  x.addException((String)attributeList.get(i), (Throwable)exceptionList.get(i));
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

         List exceptionList = (List)unmrsh.getTempValue(nObj, 8);

         if (exceptionList != null)
         {
            for (int i = 0, n = exceptionList.size(); i < n; ++i)
            {
               e.addException((Throwable)exceptionList.get(i));
            }
         }

         unmrsh.setFixup(nObj, e);
      }
   };

   // declare after the rest as it needs TRANSFER_OBJECT_UNMSH declared
   protected final static Unmarshaller REQUEST_UNMSH =
      new ObjectUnmarshaller(XML.NS_URI_TNS,
                             XML.BASE_PREFIX + "Request",
                             new ObjectAccessor[]
      {
         new StaticObjectAccessor(XML.NS_URI_TNS, "namespace", false, true, STRING_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "version", false, true, STRING_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "async", false, true, BOOLEAN_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "commit", false, true, BOOLEAN_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "locale", false, false, LOCALE_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS, "timeZone", false, false, TIME_ZONE_UNMSH),
         new StaticObjectAccessor(XML.NS_URI_TNS,
                                  "correlator",
                                  false,
                                  false,
                                  TRANSFER_OBJECT_UNMSH),
         // unmarshaller == INVOCATION_UNMSH set in construct()
         new ObjectListAccessor(XML.getTNSType("invocation"), false, true, null, false),
         new ObjectListAccessor(
            XML.getTNSType("filters"), false, true, TRANSFER_OBJECT_UNMSH, false)
      })
   {
      private final Unmarshaller INVOCATION_UNMSH =
         new ObjectUnmarshaller(
            XML.NS_URI_TNS, "invocation", new ObjectAccessor[]
            {
               new ObjectInstanceAccessor( // use TransferObject Unmarshaller if no type specified
                      XML.getTNSType("object"), false, true, TRANSFER_OBJECT_UNMSH, true),
               new StaticObjectAccessor(XML.NS_URI_TNS, "event", false, true, STRING_UNMSH),
               new ObjectListAccessor(XML.getTNSType("arg"), false, true, null, true),
               new StaticObjectAccessor(XML.NS_URI_TNS, "attributes", false, true, PAIR_UNMSH)
            })
      {
         protected Object construct()
         {
            return new Request.Invocation(null);
         }

         protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
         {
            XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
            Request.Invocation invocation = (Request.Invocation)unmrsh.getFixup(nObj);
            List argList = (List)unmrsh.getTempValue(nObj, 2);

            invocation.setObject((TransferObject)unmrsh.getTempValue(nObj, 0));
            invocation.setEventName((String)unmrsh.getTempValue(nObj, 1));
            invocation.setArguments((argList == null) ? null : argList.toArray());
            invocation.setAttributes((Pair)unmrsh.getTempValue(nObj, 3));
         }
      };

      protected Object construct()
      {
         // Can't set in declaration because private variables have not been declared yet.
         ((ObjectListAccessor)getAccessor(7)).setUnmarshaller(INVOCATION_UNMSH);

         return new Request();
      }

      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         Request req = (Request)unmrsh.getFixup(nObj);

         req.setNamespace((String)unmrsh.getTempValue(nObj, 0));
         req.setVersion((String)unmrsh.getTempValue(nObj, 1));

         Boolean async = (Boolean)unmrsh.getTempValue(nObj, 2);
         
         if (async != null)
         {
            req.setAsync(async.booleanValue());
         }
         
         Boolean commit = (Boolean)unmrsh.getTempValue(nObj, 3);
         
         if (commit != null)
         {
            req.setCommit(commit.booleanValue());
         }
         
         req.setLocale((Locale)unmrsh.getTempValue(nObj, 4));
         req.setTimeZone((TimeZone)unmrsh.getTempValue(nObj, 5));
         req.setCorrelator((TransferObject)unmrsh.getTempValue(nObj, 6));

         List/*<Request.Invocation>*/ invocationList = (List)unmrsh.getTempValue(nObj, 7);
         Lookup2D/*<String, Object, TransferObject>*/ argIdentityMap = null;

         if (invocationList != null)
         {
            int nArgCount = invocationList.size();

            argIdentityMap = new HashTab2D(nArgCount);

            for (int i = 0; i < nArgCount; ++i)
            {
               Request.Invocation invocation = (Request.Invocation)invocationList.get(i);
               TransferObject tobj = invocation.getObject();

               req.addInvocation(invocation);

               if (tobj != null)
               {
                  String sClass = tobj.getClassName();
                  Object obj = tobj.getOID();

                  if (obj == null)
                  {
                     obj = unmrsh.getObjectRef(tobj);
                  }

                  if (sClass != null && obj != null)
                  {
                     argIdentityMap.put(sClass, obj, tobj);
                  }
               }
            }
         }

         List/*<TransferObject>*/ filterList = (List)unmrsh.getTempValue(nObj, 8);

         if (filterList != null)
         {
            for (int nFilter = 0, nFilterCount = filterList.size();
                 nFilter < nFilterCount;
                 ++nFilter)
            {  
               // walk through filters searching for a filter with "instances" key and update all
               // its values to objects from arguments
               TransferObject filter = (TransferObject)filterList.get(nFilter);

               req.addFilter(filter);

               if (filter == null)
               {
                  continue;
               }

               // this value is defined in nexj.core.rpc.Request
               Object instances = filter.findValue("instances");

               if (instances == null)
               {
                  continue;
               }

               List instanceList;

               // GenericServer.invoke(Request) expects instances to be a List
               if (instances instanceof Object[])
               {
                  instanceList = Arrays.asList((Object[])instances);
                  filter.setValue("instances", instanceList);
               }
               else
               {
                  instanceList = (List)instances;
               }

               if (argIdentityMap != null)
               {
                  for (int i = 0, n = instanceList.size(); i < n; ++i)
                  {
                     TransferObject tobj = (TransferObject)instanceList.get(i);

                     if (tobj == null)
                     {
                        continue;
                     }

                     String sClass = tobj.getClassName();
                     Object obj = tobj.getOID();

                     if (obj == null)
                     {
                        obj = unmrsh.getObjectRef(tobj);
                     }

                     if (sClass != null && obj != null)
                     {
                        tobj = (TransferObject)argIdentityMap.get(sClass, obj);
   
                        if (tobj != null)
                        {
                           // replace the object copy with the one from args
                           instanceList.set(i, tobj);
                        }
                     }
                  }
               }
            }
         }
      }
   };
   
   /**
    * Response unmarshaller.
    */
   protected final static Unmarshaller RESPONSE_UNMSH =
      new ObjectUnmarshaller(XML.NS_URI_TNS,
                             XML.BASE_PREFIX + "Response",
                             new ObjectAccessor[]
      {
         new ObjectListAccessor(XML.getTNSType("results"), false, true, null, true),
         // fallback on TransferObject Unmarshaller if no type specified
         new StaticObjectAccessor(XML.NS_URI_TNS, "events",  false, true,
                new ElementListUnmarshaller(new ElementListUnmarshaller(TRANSFER_OBJECT_UNMSH)))
      })
   {
      protected Object construct()
      {
         return new Response();
      }
      
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         Response resp = (Response)unmrsh.getFixup(nObj);
         List resultList = (List)unmrsh.getTempValue(nObj, 0);
         
         if (resultList != null)
         {
            for (int i = 0, n = resultList.size(); i < n; ++i)
            {
               resp.addResult(resultList.get(i));
            }
         }

         List eventList = (List)unmrsh.getTempValue(nObj, 1);

         if (eventList != null)
         {
            for (int i = 0, n = eventList.size(); i < n; ++i)
            {
               Collection eventCollection = (Collection)eventList.get(i);

               resp.addEvent((eventCollection == null) ? Collections.EMPTY_LIST : eventCollection);
            }
         }
      }
   };

   // declare after the rest as it needs EXCEPTION_UNMSH declared
   /**
    * Added URI to Accessors.
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#SOAP_FAULT_UNMSH
    */
   protected final static Unmarshaller SOAP_FAULT_UNMSH =
      new ObjectSubsetUnmarshaller(XML.ENV_URI,
                                   "Fault",
                                   new ObjectAccessor[]
      {
         new StaticObjectAccessor(XML.ENV_URI, "faultcode", false, true, QUALIFIED_STRING_UNMSH),
         new StaticObjectAccessor(XML.ENV_URI, "faultstring", false, true, STRING_UNMSH),
         new StaticObjectAccessor(XML.ENV_URI, "detail", false, false, EXCEPTION_UNMSH)            
      })
   {
      protected Object construct()
      {
         return new SOAPFault(null);
      }
      
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         SOAPFault fault = (SOAPFault)unmrsh.getFixup(nObj);
         QName code = (QName)unmrsh.getTempValue(nObj, 0);
         
         fault.setURI(code.getNamespaceURI());
         fault.setCode(code.getLocalPart());
         fault.setMessage((String)unmrsh.getTempValue(nObj, 1));
         fault.setException((Throwable)unmrsh.getTempValue(nObj, 2));
      }

      public boolean isMultiRef()
      {
         return false;
      }
   };

   /**
    * Map of URI and type name to unmarshaller: <URI, Type> -> Unmarshaller.
    */
   protected final static Lookup2D/*<String, String, Unmarshaller>*/ s_unmarshallerMap =
      new HashTab2D/*<String, String, Unmarshaller>*/(32);
   static
   {
      // add all PrimitiveUnmarshallers from parent to map and redefine their namespace URI
      // ComplexUnmarshallers already redefined due to having components in different XML namespace.
      for (Iterator/*<Unmarshaller>*/ itr = s_unmshMap.valueIterator(); itr.hasNext();)
      {  
         Unmarshaller unmshaller = (Unmarshaller)itr.next();

         if (unmshaller instanceof SimpleUnmarshaller)
         {
            unmshaller = new SimpleUnmarshallerWrapper(SOAP.TNS_URI.equals(unmshaller.getURI())
                                                       ? XML.NS_URI_TNS : unmshaller.getURI(),
                                                       unmshaller.getType(),
                                                       (SimpleUnmarshaller)unmshaller);
            s_unmarshallerMap.put(unmshaller.getURI(), unmshaller.getType(), unmshaller);
         }
      }

      s_unmarshallerMap.put(CHAR_ARRAY_UNMSH.getURI(),
                            CHAR_ARRAY_UNMSH.getType(),
                            CHAR_ARRAY_UNMSH);
      s_unmarshallerMap.put(DYNAMIC_CHANGE_REQUEST_UNMSH.getURI(),
                            DYNAMIC_CHANGE_REQUEST_UNMSH.getType(),
                            DYNAMIC_CHANGE_REQUEST_UNMSH);
      s_unmarshallerMap.put(DYNAMIC_READ_REQUEST_UNMSH.getURI(),
                            DYNAMIC_READ_REQUEST_UNMSH.getType(),
                            DYNAMIC_READ_REQUEST_UNMSH);
      s_unmarshallerMap.put(EXCEPTION_UNMSH.getURI(), EXCEPTION_UNMSH.getType(), EXCEPTION_UNMSH);
      s_unmarshallerMap.put(EXPRESSION_UNMSH.getURI(),
                            EXPRESSION_UNMSH.getType(),
                            EXPRESSION_UNMSH);
      s_unmarshallerMap.put(FUNCTION_UNMSH.getURI(), FUNCTION_UNMSH.getType(), FUNCTION_UNMSH);
      s_unmarshallerMap.put(LOCALE_UNMSH.getURI(), LOCALE_UNMSH.getType(), LOCALE_UNMSH);
      s_unmarshallerMap.put(MACRO_UNMSH.getURI(), MACRO_UNMSH.getType(), MACRO_UNMSH);
      s_unmarshallerMap.put(OBJECT_LIST_MSH.getURI(), OBJECT_LIST_MSH.getType(), OBJECT_LIST_MSH);
      s_unmarshallerMap.put(OID_UNMSH.getURI(), OID_UNMSH.getType(), OID_UNMSH);
      s_unmarshallerMap.put(PAIR_UNMSH.getURI(), PAIR_UNMSH.getType(), PAIR_UNMSH);
      s_unmarshallerMap.put(PRIVILEGE_SET_UNMSH.getURI(),
                            PRIVILEGE_SET_UNMSH.getType(),
                            PRIVILEGE_SET_UNMSH);
      s_unmarshallerMap.put(BYTE_VECTOR_UNMSH.getURI(),
                            BYTE_VECTOR_UNMSH.getType(),
                            BYTE_VECTOR_UNMSH);
      s_unmarshallerMap.put(REQUEST_UNMSH.getURI(), REQUEST_UNMSH.getType(), REQUEST_UNMSH);
      s_unmarshallerMap.put(RESPONSE_UNMSH.getURI(), RESPONSE_UNMSH.getType(), RESPONSE_UNMSH);
      s_unmarshallerMap.put(SOAP_FAULT_UNMSH.getURI(),
                            SOAP_FAULT_UNMSH.getType(),
                            SOAP_FAULT_UNMSH);
      s_unmarshallerMap.put(STRING_ARRAY_UNMSH.getURI(),
                            STRING_ARRAY_UNMSH.getType(),
                            STRING_ARRAY_UNMSH);
      s_unmarshallerMap.put(SYMBOL_UNMSH.getURI(), SYMBOL_UNMSH.getType(), SYMBOL_UNMSH);
      s_unmarshallerMap.put(TIME_ZONE_UNMSH.getURI(), TIME_ZONE_UNMSH.getType(), TIME_ZONE_UNMSH);
      s_unmarshallerMap.put(TRANSFER_OBJECT_UNMSH.getURI(),
                            TRANSFER_OBJECT_UNMSH.getType(),
                            TRANSFER_OBJECT_UNMSH);
   }
   
   /**
    * Map of URI and type name to collection unmarshaller: <URI, Type> -> ElementListUnmarshaller.
    */
   protected final static Lookup2D/*<String, String, ElementListUnmarshaller>*/
      s_collectionUnmarshallerMap = new HashTab2D/*<String, String, ElementListUnmarshaller>*/(1);

   static
   {
      s_collectionUnmarshallerMap.put(XML.XSD_URI, "anyType", new ElementListUnmarshaller(null));
   }

   /**
    * Set of XML Schema built-in types.
    * Use set to speed up lookup.
    * @see http://www.w3.org/TR/xmlschema-2/#built-in-datatypes
    */
   protected final static Set/*<String>*/ s_XMLSchemaTypeSet = new HashHolder/*<String>*/(46);

   static
   {
      s_XMLSchemaTypeSet.add("anySimpleType");
      s_XMLSchemaTypeSet.add("anyType");
      s_XMLSchemaTypeSet.add("anyURI");
      s_XMLSchemaTypeSet.add("base64Binary");
      s_XMLSchemaTypeSet.add("boolean");
      s_XMLSchemaTypeSet.add("byte");
      s_XMLSchemaTypeSet.add("date");
      s_XMLSchemaTypeSet.add("dateTime");
      s_XMLSchemaTypeSet.add("decimal");
      s_XMLSchemaTypeSet.add("double");
      s_XMLSchemaTypeSet.add("duration");
      s_XMLSchemaTypeSet.add("ENTITIES");
      s_XMLSchemaTypeSet.add("ENTITY");
      s_XMLSchemaTypeSet.add("float");
      s_XMLSchemaTypeSet.add("gDay");
      s_XMLSchemaTypeSet.add("gMonth");
      s_XMLSchemaTypeSet.add("gMonthDay");
      s_XMLSchemaTypeSet.add("gYear");
      s_XMLSchemaTypeSet.add("gYearMonth");
      s_XMLSchemaTypeSet.add("hexBinary");
      s_XMLSchemaTypeSet.add("ID");
      s_XMLSchemaTypeSet.add("IDREF");
      s_XMLSchemaTypeSet.add("IDREFS");
      s_XMLSchemaTypeSet.add("int");
      s_XMLSchemaTypeSet.add("integer");
      s_XMLSchemaTypeSet.add("language");
      s_XMLSchemaTypeSet.add("long");
      s_XMLSchemaTypeSet.add("Name");
      s_XMLSchemaTypeSet.add("NCName");
      s_XMLSchemaTypeSet.add("negativeInteger");
      s_XMLSchemaTypeSet.add("NMTOKEN");
      s_XMLSchemaTypeSet.add("NMTOKENS");
      s_XMLSchemaTypeSet.add("nonNegativeInteger");
      s_XMLSchemaTypeSet.add("nonPositiveInteger");
      s_XMLSchemaTypeSet.add("normalizedString");
      s_XMLSchemaTypeSet.add("NOTATION");
      s_XMLSchemaTypeSet.add("positiveInteger");
      s_XMLSchemaTypeSet.add("QName");
      s_XMLSchemaTypeSet.add("short");
      s_XMLSchemaTypeSet.add("string");
      s_XMLSchemaTypeSet.add("time");
      s_XMLSchemaTypeSet.add("token");
      s_XMLSchemaTypeSet.add("unsignedByte");
      s_XMLSchemaTypeSet.add("unsignedInt");
      s_XMLSchemaTypeSet.add("unsignedLong");
      s_XMLSchemaTypeSet.add("unsignedShort");
   }

   // attributes

   /**
    * Counter holding the last Epoch used
    * (in actual fact in current implementation it is the number of nodes processed).
    * Metaclass references are resolved in a bottom-up fashion when returning from nodes in a
    * depth-first unmarshaller traversal.
    * The algoritm used for resolving Metaclass style references (i.e. only sType + OID):
    * 1) When opening element set m_indexArray[nObj] to incrememnted epoch
    *    (nObj == current object position in m_fixupArray).
    *    This works because m_indexArray[nObj] (used by linkObjectFixup()) is only called at
    *    endElement(...) by which point we already dont need the value.
    * 2) When closing an element:
    *    a) If only have sType + OID:
    *       i) Find tail of linked list for unresolved metarefererences matching this object from
    *          m_metaRefMap.
    *       ii) Set current object as the tail of the linked list for matching metareferences in
    *           m_metaRefMap.
    *       iii) Put metareference pointing to self into m_idMap (just in case this is really a
    *            finished object containing only a type and OID).
    *       iv) setTempIndex(nObj, 0) to previous tail
    *           (garanteed to have 2 positions, one for type and one for OID).
    *           It's ok to reuse these spots in m_indexArray because construction of object has
    *           been completed and tempValue offsets will no longer be used.
    *       v) setTempIndex(nObj, 1) to the epoch of this metareference.
    *       vi) setTempValue(nObj, 1) to the metaRef object used in m_idMap.
    *       vii) setFixup() for parent's pointer to metaRef object used in m_idMap.
    *    b) If have a complete Metaclass:
    *       This will always be an un-completable object because it depends on references,
    *       otherwise there would be no references to update in it.
    *       i) Find tail of linked list for unresolved metarefererences matching this object from
    *          m_metaRefMap.
    *       ii) Reserve a spot in m_fixupArray pointing to current object.
    *           Have to use a different spot to let metareferences not depend on an un-completed
    *           metaclass during fixup().
    *       iii) For each metareference with epoch > current object update m_idMap at
    *            metareference's metaRef with reserved spot.
    *       iv) Either update m_metaRefMap with next metareference to be processed or remove list
    *           if no more references remain.
    *  Note: All this drudgery instead of using DependencyStack to forego allocation of two extra
    *        arrays.
    */
   protected int m_nEpochCounter = -1;

   /**
    * The position of the value in fixup array.
    */
   protected int m_nFixupValue;

   // associations

   /**
    * Static unmarshaller to use for the outer metaclass contents of the body.
    */
   protected Unmarshaller m_metaclassStaticUnmarshaller;
  
   /**
    * Map holding the head of the linked list for Metaclass references
    * <sType, OID> -> <Integer object offset in m_indexArray containing head of list> 
    */
   protected Lookup2D/*<String, OID, Integer>*/ m_metaRefMap;

   /**
    * Map of URI and type name to unmarshaller: <sURI, sType> -> Unmarshaller.
    * Used to store metadata unmarshallers instantiated based on current invocation context.
    */
   protected final Lookup2D/*<String, String, Unmarshaller>*/ m_unmarshallerMap =
      new HashTab2D/*<String, String, Unmarshaller>*/(32);
   
   /**
    * Map of URI and type name to collection unmarshaller: <sURI, sType> -> ElementListUnmarshaller.
    * Used to store metadata unmarshallers instantiated based on current invocation context.
    */
   protected final Lookup2D/*<String, String, ElementListUnmarshaller>*/
      m_collectionUnmarshallerMap = new HashTab2D/*<String, String, ElementListUnmarshaller>*/(32);

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   // constructors
   
   /**
    * Constructs the unmarshaller.
    * @param context The application context.
    * @param metadata The root metadata object.
    */
   public XMLUnmarshaller(Context context, Metadata metadata)
   {
      super(context);
      m_metadata = metadata;
   }

   /**
    * Constructs the unmarshaller.
    * @param context The invocation context.
    */
   public XMLUnmarshaller(InvocationContext context)
   {
      this(context, context.getMetadata());
   }

   /**
    * Constructs the unmarshaller.
    * @param context The invocation context.
    */
   public XMLUnmarshaller(Context context)
   {
      this((InvocationContext)context);
   }

   // operations

   /**
    * Do steps to complete instantiation of a complex object.
    * @param destination The accessor for the destination location
    *        (destination reference avaiable via call to getObjectIndex()) (null == dont't store).
    * @param unmarshaller The unmarshaller to use for completion of the object
    *        (null == use unmarshaller from fixup array).
    * @param reference The reference of the source object (null == null object).
    */
   protected void completeComplexObject(Accessor destination,
                                        ComplexUnmarshaller unmarshaller,
                                        Integer reference)
   {
      Object value = null;

      if (reference != null) // see method javadoc for reasons behind this condition
      {
         int nObj = reference.intValue();
         
         // get the object epoch before it has a chance to be updated by linkObjectFixup() or
         // addReferenceFixup().
         int nObjEpoch = m_indexArray[nObj];
         
         if (unmarshaller == null)
         {
            unmarshaller = (ComplexUnmarshaller)getFixup(nObj + UNMSH_OFS);
         }
      
         if (unmarshaller != null) // see method javadoc for reasons behind this condition
         {
            unmarshaller.complete(nObj, this);
         }
      
         if (unmarshaller instanceof MetaclassUnmarshaller)
         {
            MetaclassUnmarshaller.MetaclassRef metaRef = ((MetaclassUnmarshaller)unmarshaller).getRef(nObj, this);

            // delay setting the "attribute" value in parent until child has a chance to be
            // overridden (this approach has a chance of failing if fixup done before real object
            // created)
            if (metaRef != null &&
                metaRef.sType != null && // Lookup2D can't handle null keys
                metaRef.oid != null && // Lookup2D can't handle null keys
                metaRef.isRef != null &&
                metaRef.isRef.booleanValue())
            {
               Integer tail = (Integer)m_metaRefMap.get(metaRef.sType, metaRef.oid);

               // set this object as the tail position of for this metaRef
               m_metaRefMap.put(metaRef.sType, metaRef.oid, reference);

               // point reference to self just in case this is really a finished object containing
               // only a type and OID (always use current metaRef because don't know how epochs
               // will map
               m_idMap.put(metaRef, reference);

               // set to previous position of linked list of objects
               // (garanteed to have 2 positions, one for type and one for OID)
               setTempIndex(nObj, 0, (tail == null) ? -1 : tail.intValue());

               // remember this objects epoch value
               // (garanteed to have 2 positions, one for type and one for OID)
               setTempIndex(nObj, 1, nObjEpoch);

               // remember the key used in m_idMap for the unresolved reference
               // (garanteed to have 2 positions, one for type and one for OID)
               setTempValue(nObj, 1, metaRef);
               
               // add a fixup request for a fixup
               // (i.e. when cleanup started this would move the getFixup(nObj) value into accessor)
               // use the metaRef as a unique identifier since it will be recreated for a different
               // branch
               if (destination != null)
               {
                  destination.setFixup(getObjectIndex().intValue(), metaRef, this);
               }
      
               // all done, don't set value just yet for parent because the value might change and
               // the parent shouldn't try to initialize from it
               return;
            }
            // if this is not a reference then the only way can get to here is if there are no
            // reference children because otherwise they would be unfinished and this object would
            // be unfinished, hence they'd go to the 'else' section
         }
         
         value = getFixup(nObj);
      }

      if (destination != null) // see method javadoc for reasons behind this condition
      {
         destination.setValue(getObjectIndex().intValue(), value, this);
      }
   }

   /**
    * Do steps to complete instantiation of a simple object.
    * @param destination The accessor for the destination location
    *        (destination reference avaiable via call to getObjectIndex()) (null == dont't store).
    * @param unmarshaller The unmarshaller to use for completion of the object
    *        (null == use unmarshaller from fixup array implies reference != null).
    * @param reference The reference of the source object (null == no need to update any fixups).
    * @throws Exception If unmarshalling error occurs.
    */
   protected void completeSimpleObject(Accessor destination,
                                       SimpleUnmarshaller unmarshaller,
                                       Integer reference) throws Exception
   {
      Object value = null;

      // see method javadoc for reasons behind this condition
      if (unmarshaller == null && reference != null)
      {
         unmarshaller = (SimpleUnmarshaller)getFixup(reference.intValue() + UNMSH_OFS);
      }
   
      if (unmarshaller != null) // see method javadoc for reasons behind this condition
      {
         value = unmarshaller.unmarshal(m_valueBuf.toString(), this);
      }
     
      // this section is similar to ComplexUnmarshaller.complete() since any reserved spots have to
      // be updated as well
      if (reference != null)
      {
         // if there was a reference created for this object (something others might depend on)
         setFixup(reference.intValue(), value);
      }

      if (destination != null) // see method javadoc for reasons behind this condition
      {
         destination.setValue(getObjectIndex().intValue(), value, this);
      }
   }

   /**
    * Creates a proper unmarshaller for the specified qualified type.
    * @param qType The qualified type of the unmarshaller to create (not null).
    * @return The unmarshaller or null if no unmarshaller known for specified qType.
    */
   protected Unmarshaller createDynamicUnmarshaller(QName qType)
   {
      String sURI = qType.getNamespaceURI();

      if (!sURI.startsWith(XML.NS_URI_TNS))
      {
         return null; // qType not under dynamic namespace, i.e. not <XML.NS_URI_TNS>
      }

      // events for WSDL are under <XML.NS_URI_TNS>/<Metaclass> hence find proper metaclass
      if (sURI.length() > XML.NS_URI_TNS.length())
      {
         int nPos = XML.NS_URI_TNS.length();

         if (sURI.charAt(nPos) != '/')
         {
            return null; // sURI doesn't start with <XML.NS_URI_TNS>/
         }

         int nEnd = sURI.indexOf('/', nPos + 1);
         String sMetaclass = (nEnd < 0) ? sURI.substring(nPos + 1) : sURI.substring(nPos + 1, nEnd);
         Unmarshaller unmsh = getUnmarshaller(XML.NS_URI_TNS, sMetaclass, null);

         return (unmsh instanceof MetaclassUnmarshaller)
                ? ((MetaclassUnmarshaller)unmsh).findUnmarshaller(qType) : null;
      }

      String sType = qType.getLocalPart();
      Metaclass metaclass = m_metadata.findMetaclass(sType);

      // Unmarshaller for actual Metaclass
      if (metaclass != null)
      {
         return new MetaclassUnmarshaller(sURI, sType, metaclass);
      }

      // Unmarshaller for dynamic change request constrained to a specific metaclass
      if (sType.endsWith(DYNAMIC_CHANGE_REQUEST_UNMSH.getType()) &&
          sType.charAt(sType.length() - DYNAMIC_CHANGE_REQUEST_UNMSH.getType().length() - 1) == '-')
      {
         sType = sType.substring(0, sType.length() -
                                    DYNAMIC_CHANGE_REQUEST_UNMSH.getType().length() - 1);

         Unmarshaller unmsh = getUnmarshaller(sURI, sType, null);

         return (unmsh instanceof MetaclassUnmarshaller)
                ? new DynamicChangeRequestUnmarshaller((MetaclassUnmarshaller)unmsh) : null;
      }

      // Unmarshaller for dynamic read request constrained to a specific metaclass
      if (sType.endsWith(DYNAMIC_READ_REQUEST_UNMSH.getType()) &&
          sType.charAt(sType.length() - DYNAMIC_READ_REQUEST_UNMSH.getType().length() - 1) == '-')
      {
         sType =
            sType.substring(0, sType.length() - DYNAMIC_READ_REQUEST_UNMSH.getType().length() - 1);

         Unmarshaller unmsh = getUnmarshaller(sURI, sType, null);

         return (unmsh instanceof MetaclassUnmarshaller)
                ? new DynamicReadRequestUnmarshaller((MetaclassUnmarshaller)unmsh) : null;
      }

      return null; // Unknown Metaclass
   }

   /**
    * Do steps to defer completion of a complex object.
    * @param destination The accessor for the destination location
    *        (destination reference avaiable via call to getObjectIndex()) (null == dont't store).
    * @param unmarshaller The unmarshaller to use for completion of the object
    *        (null == use unmarshaller from fixup array).
    * @param reference The reference of the source object (null == null object).
    */
   protected void deferComplexObject(Accessor destination,
                                     ComplexUnmarshaller unmarshaller,
                                     Integer reference)
   {
      // no object to defer so complete as usual,
      // see method javadoc for reasons behind this condition
      if (reference == null)
      {
         completeComplexObject(destination, unmarshaller, null);
         
         return;
      }

      int nObj = reference.intValue();

      // get the object epoch before it has a chance to be updated by linkObjectFixup() or
      // addReferenceFixup().
      int nObjEpoch = m_indexArray[nObj];

      m_indexArray[nObj] = 0;

      // Add a reference fixup and an object fixup
      if (destination != null) // see method javadoc for reasons behind this condition
      {
         destination.setFixup(getObjectIndex().intValue(), reference, this);
      }

      // object must be fixed up since it reserves a spot that needs to be filled with by the
      // accessor's value
      linkObjectFixup(nObj);

      if (unmarshaller == null) // see method javadoc for reasons behind this condition
      {
         unmarshaller = (ComplexUnmarshaller)getFixup(nObj + UNMSH_OFS);
      }

      // NOTE: this approach will always resolve a Metaclass reference to the most inner parent
      //       with same class+OID because once the most inner parent reches the end tag it will
      //       remove from the m_metaRefMap any children that require it and update m_idMap to
      //       point to self   
      if (unmarshaller instanceof MetaclassUnmarshaller)
      {
         // process an unfinished TransferObject that might be referenced by children
         // (it should have metaRef because class and OID are PrimitiveUnmarshaller types and hence
         //  will be unmarshalled by now)
         MetaclassUnmarshaller.MetaclassRef metaRef = ((MetaclassUnmarshaller)unmarshaller).getRef(nObj, this);

         // find the reference object used by referring children
         // (if no children then we don't need to do anything)
         // Lookup2D can't handle null keys
         if (metaRef != null && metaRef.sType != null && metaRef.oid != null)
         {  
            Integer tail = (Integer)m_metaRefMap.get(metaRef.sType, metaRef.oid);
            
            // if have metaclass references referencing this object
            if (tail != null)
            {
               Integer objRef = null;
               int nTail;
               
               // while have not reached end of objects we are responsible for
               // i.e. current tail is valid (>-1) && reference's epoch is after this object's epoch
               for (nTail = tail.intValue();
                    nTail > -1 && nObjEpoch < getTempIndex(nTail, 1);
                    nTail = getTempIndex(nTail, 0))
               {
                  // allocate only one position and only when know that there is something to
                  // resolve
                  if (objRef == null)
                  {
                     // copy the unfinished object into a new place so it can be used for linking
                     // by others (don't need an unmarshaller as that's handled by another
                     // unmarshaller pointing to same object)
                     objRef = Primitive.createInteger(addObjectFixup(getFixup(nObj), null, 0));
                  }

                  // update reference with the location of this object
                  m_idMap.put(getTempValue(nTail, 1), objRef);
               }
               
               // update m_metaRefMap after resolved as many references as possible
               if (nTail < 0)
               {
                  m_metaRefMap.remove(metaRef.sType, metaRef.oid); // remove stale entry from map
               }
               else if (nTail != tail.intValue())
               {
                  // set map to new tail
                  m_metaRefMap.put(metaRef.sType, metaRef.oid, Primitive.createInteger(nTail));
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#deserialize(java.io.Reader)
    */
   public Object deserialize(Reader reader) throws IOException, MarshallerException
   {
      return deserialize(reader, null);
   }

   /**
    * @param reader The reader to get source input from.
    * @param metaclass The metaclass to expect as the outermost object, can be null.
    * @return The unmarshalled object.
    * @throws IOException On error reading from input.
    * @throws MarshallerException On unmarshalling error.
    */
   public Object deserialize(Reader reader, Metaclass metaclass) throws IOException, MarshallerException
   {
      init(metaclass);

      try
      {
         XMLUtil.parse(reader, this);
         fixup();

         return getFixup(m_nFixupValue);
      }
      catch (SOAPUnmarshallerException e)
      {
         throw e;
      }
      catch (XMLParserException e)
      {
         throw new SOAPUnmarshallerException("err.rpc.xml.request", e);
      }
      catch (XMLException e)
      {
         throw (e.getCause() instanceof SOAPUnmarshallerException)
                ? (SOAPUnmarshallerException)e.getCause() 
                : new SOAPUnmarshallerException("err.rpc.xml.request", e.getCause());
      }
   }

   /**
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#findId(int)
    */
   protected String findId(int nObj)
   {
      return null; // object indexes not used in XML literal style
   }

   /**
    * Finds a URI by namespace name.
    * @param sNamespace The namespace name.
    * @return The namespace URI or null if the namespace cannot be found.
    */
   protected String findURI(String sNamespace) throws SOAPUnmarshallerException
   {
      Namespace ns = (Namespace)m_nsMap.get(sNamespace);

      return (ns == null) ? null : ns.uri;
   }

   /**
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#getHRefAttributeValue(org.xml.sax.Attributes)
    */
   protected String getHRefAttributeValue(Attributes attributes)
   {
      return attributes.getValue(XML.NS_URI_XLINK, "href");
   }

   /**
    * @see nexj.core.rpc.soap.SOAPUnmarshaller#getIdAttributeValue(org.xml.sax.Attributes)
    */
   protected String getIdAttributeValue(Attributes attributes)
   {
      return attributes.getValue(XML.NS_URI_XML, "id");
   }

   /**
    * Gets a list unmarshaller for a given type.
    * @param sURI The default URI to use if sType is not qualified.
    * @param sType The (possibly) qualified type of the list elements
    *              (if name is qualified then its namespace will be used to decipher type).
    * @param attributes The additional attributes for type lookup. Can be null.
    * @return The array unmarshaller or null if base type unmarshaller not found.
    */
   protected ElementListUnmarshaller getListUnmarshaller(String sURI,
                                                         String sType,
                                                         Attributes attributes)
   {
      QName qType = resolveType(sURI, sType);

      // check with locally registered unmarshallers first (static context)
      ElementListUnmarshaller arrayUnmsh =
         (ElementListUnmarshaller)s_collectionUnmarshallerMap.get(qType.getNamespaceURI(),
                                                                  qType.getLocalPart());

      if (arrayUnmsh == null)
      {
         // check with locally registered unmarshallers first (invocation context sensitive)
         arrayUnmsh =
            (ElementListUnmarshaller)m_collectionUnmarshallerMap.get(qType.getNamespaceURI(),
                                                                     qType.getLocalPart());
      }

      // if not found then create it below
      if (arrayUnmsh == null)
      {
         Unmarshaller unmsh = getUnmarshaller(sURI, sType, attributes); // base type unmarshaller

         if (unmsh != null)
         {
            arrayUnmsh = new ElementListUnmarshaller(unmsh);

            // put it in map so can reuse same unmarshaller later
            m_collectionUnmarshallerMap.put(qType.getNamespaceURI(),
                                            qType.getLocalPart(),
                                            arrayUnmsh);
         }
      }

      return arrayUnmsh;
   }

   /**
    * Retrieves a tracked an object reference.
    * @param key The key to retrieve value for (null will not be tracked).
    * @return The corresponding value or null == not found.
    */
   protected Object getObjectRef(Object key)
   {
      return (key == null) ? null : m_idMap.get(key); // Lookup maps cannot handle nulls
   }

   /**
    * @param sURI The default URI to use if sType is not qualified.
    * @param sType The (possibly) qualified type
    *              (if name is qualified then its namespace will be used to decipher type).
    * @param attributes The additional attributes for type lookup. Can be null.
    * @returns The unmarshaller or null if not found.
    */
   protected Unmarshaller getUnmarshaller(String sURI, String sType, Attributes attributes)
   {
      if (attributes != null)
      {
         String sValue = attributes.getValue(XML.XSI_URI, "nil");

         // if got a nil value then return proper unmarshaller
         if ("true".equals(sValue) || "1".equals(sValue))
         {
            return NULL_UNMSH;
         }
      }

      if (sType == null)
      {
         return null;
      }
      
      // if got a qualified type then determine its proper namespace URI
      QName qType = resolveType(sURI, sType);
      sURI = qType.getNamespaceURI();
      sType = qType.getLocalPart();

      // check with locally registered unmarshallers first (static context)
      Unmarshaller unmsh = (Unmarshaller)s_unmarshallerMap.get(sURI, sType);
      
      // check with locally registered unmarshallers first (invocation context sensitive)
      if (unmsh == null)
      {
         unmsh = (Unmarshaller)m_unmarshallerMap.get(sURI, sType);
      }
      
      // SOAPUnmarshaller will sometimes not qualify a type
      // (e.g. tag qualified and type not qualified will produce "" == sURI)
      // so use NexJ type by default
      if (unmsh == null && "".equals(sURI))
      {
         sURI = XML.NS_URI_TNS;

         // check with locally registered unmarshallers first (static context)
         unmsh = (Unmarshaller)s_unmarshallerMap.get(sURI, sType);

         // check with locally registered unmarshallers first (invocation context sensitive)
         if (unmsh == null)
         {
            unmsh = (Unmarshaller)m_unmarshallerMap.get(sURI, sType);
         }
      }

      if (unmsh == null)
      {
         unmsh = createDynamicUnmarshaller(qType);

         // put it in map so can reuse same unmarshaller later
         if (unmsh != null)
         {
            m_unmarshallerMap.put(sURI, sType, unmsh);
         }
         else if (sType.endsWith(XML.ARRAY_SUFFIX))
         {
            sType = sType.substring(0, sType.length() - XML.ARRAY_SUFFIX.length());

            // get sType as array type
            return getListUnmarshaller((!s_XMLSchemaTypeSet.contains(sType))
                                       ? sURI : XML.XSD_URI, sType, attributes);
         }
         else if (sType.startsWith(ARRAY_PREFIX) && sType.length() > ARRAY_PREFIX.length())
         {
            // This is an MS.NET kluge, for any array types MS.NET just prefixes the type with
            // "ArrayOf" and will camel-case the first letter of the type. MS.NET will refuse to use
            // any array types defined in the XSD, even if an exact match is available.
            sType = sType.substring(ARRAY_PREFIX.length());
            unmsh = getListUnmarshaller((!s_XMLSchemaTypeSet.contains(sType))
                                        ? sURI : XML.XSD_URI, sType, attributes);

            // undo the camel-caseing that MS.NET might have added and try resolving the type again
            if (unmsh == null)
            {
               sType = sType.substring(0, 1).toLowerCase(Locale.ENGLISH) + sType.substring(1);
               unmsh = getListUnmarshaller((!s_XMLSchemaTypeSet.contains(sType))
                                           ? sURI : XML.XSD_URI, sType, attributes);
            }
         }
      }

      return unmsh;
   }

   /**
    * Initializer to initialize parse state.
    * @param metadata The collection of available metadata.
    * @param metaclass The metaclass to expect as the outermost object, can be null.
    * @param oid The object ID to unmarshal, can be null (ignored if meta == null).
    * @param event The event to invoke, can be null (ignored if meta == null).
    * @param argsMap Map of URL arguments for the event.
    */
   protected void init(Metaclass metaclass)
   {
      // consume first 2 indexes of m_fixupArray
      // (otherwise first object created @ 0 and linkObjectFixup will mark object as not in list
      //  because '0' is end-of-list marker) @see nexj.core.rpc.soap.SOAPUnmarshaller#m_fixupArray
      m_nFixupCount = 2;
      m_nTop = 0;
      m_bEmpty = false;
      m_bIgnoreRoot = false;
      m_metaRefMap = new HashTab2D/*<String, OID, Integer>*/();
      m_fixupArray = new Object[48];
      m_indexArray = new int[48];
      m_arrayUnmshMap = new HashTab();
      m_idMap = new HashTab(); // object to map an xml:id -> id in m_fixupArray
      m_nsMap = new HashTab();
      m_stack = new Object[48];
      m_valueBuf = new StringBuffer(32);
      m_metaclassStaticUnmarshaller = (metaclass == null) ? null :
         new MetaclassUnmarshaller(XML.NS_URI_TNS, metaclass.getName(), metaclass);        

      // would like to dynamically identify top-most unmarshaller
      ComplexUnmarshaller rootUnmsh = new ComplexUnmarshaller(null, null)
      { 
         private final ObjectAccessor SOAP_ENVELOPE_ACESSOR = 
            new StaticObjectAccessor(XMLUnmarshaller.SOAP_ENVELOPE_UNMSH.getURI(),
                                     XMLUnmarshaller.SOAP_ENVELOPE_UNMSH.getType(),
                                     true,
                                     true,
                                     XMLUnmarshaller.SOAP_ENVELOPE_UNMSH);

         private final ObjectAccessor REQUEST_ACCESSOR = 
            new StaticObjectAccessor(XMLUnmarshaller.REQUEST_UNMSH.getURI(),
                                     XMLUnmarshaller.REQUEST_UNMSH.getType(),
                                     true,
                                     true,
                                     XMLUnmarshaller.REQUEST_UNMSH);

         private final ObjectAccessor METACLASS_ACCESSOR =
            (m_metaclassStaticUnmarshaller == null)
            ? null
            : new StaticObjectAccessor(m_metaclassStaticUnmarshaller.getURI(),
                                       m_metaclassStaticUnmarshaller.getType(),
                                       true,
                                       true,
                                       m_metaclassStaticUnmarshaller);

         private final ObjectAccessor DYNAMIC_ACCESSOR = new ObjectAccessor(null, null, true, true)
         {
            public Unmarshaller getUnmarshaller(String sURI,
                                                String sName,
                                                Attributes attributes,
                                                SOAPUnmarshaller unmsh
                                               ) throws SOAPUnmarshallerException
            {
               // can't use DynamicObjectAccessor as it doesn't take sURI/sName into account
               return ((XMLUnmarshaller)unmsh).getUnmarshaller(sURI, sName, attributes);
            }
         };

         public void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
         {
            XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
            unmrsh.setFixup(nObj, unmrsh.getTempValue(nObj, 0));  
         }
      
         /**
          * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectUnmarshaller
          *      #getAccessor(java.lang.String,
          *                   java.lang.String,
          *                   nexj.core.rpc.soap.SOAPUnmarshaller)
          */
         public Accessor getAccessor(String sURI,
                                     String sType,
                                     SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
         {
            if (SOAP_ENVELOPE_ACESSOR.getURI().equals(sURI) &&
                SOAP_ENVELOPE_ACESSOR.getElement().equals(sType))
            {
               return SOAP_ENVELOPE_ACESSOR;
            }

            if (REQUEST_ACCESSOR.getURI().equals(sURI) &&
                REQUEST_ACCESSOR.getElement().equals(sType))
            {
               return REQUEST_ACCESSOR;
            }

            if (METACLASS_ACCESSOR != null &&
                METACLASS_ACCESSOR.getURI().equals(sURI) &&
                METACLASS_ACCESSOR.getElement().equals(sType))
            {
               return METACLASS_ACCESSOR;
            }
            else if (METACLASS_ACCESSOR == null)
            {
               // if no metaclass was explicitly requested then do a dynamic lookup
               // (used in jUnit testcases and for Dynamic-Request,
               //  if not unmarshalling Request then call will fail in production because
               //  XMLHTTPServer does a cast to Request)
               // same is done in SOAPBodyUnmarshaller.getUnmarshaller(...)
               return DYNAMIC_ACCESSOR;
            }

            // should never get here on valid XML,
            // invalid XML will cause a NullPointerException down the line
            return null;
         }

         public int init(Attributes attributes,
                         SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
         {
            // reserve a spot with a placeholder
            // (so that nothing uses the unfinished value before it's done)
            return ((XMLUnmarshaller)unmsh).addObjectFixup(EMPTY, this, 1);
         }         
      };

      m_nFixupValue = rootUnmsh.init(null, this);
      Integer fixupValue = Primitive.createInteger(m_nFixupValue);

      // object must be deferred since it reserves a spot that needs to be filled with by the
      // accessor's value
      deferComplexObject(null, rootUnmsh, fixupValue);

      push(fixupValue, rootUnmsh, null); // accessor is not actually used for the outer element
   }

   /**
    * Resolve a type to the proper URI and unqualified type.
    * @param sURI The default URI to use if sType is not qualified.
    * @param sType The (possibly) qualified type
    *              (if name is qualified then its namespace will be used to decipher type).
    * @return The resolved type with uri/name filled.
    */
   protected QName resolveType(String sURI, String sType)
   {
      int i = sType.indexOf(':');
      String sPrefix = (i < 0) ? XMLConstants.DEFAULT_NS_PREFIX : sType.substring(0, i);
      
      return new QName((i < 0) ? sURI : getURI(sPrefix),
                       (i < 0) ? sType : sType.substring(i + 1),
                       sPrefix);
   }

   /**
    * Tracks an object reference.
    * @param key The key to track (nulls will not be tracked).
    * @param ref The reference to return on key lookup.
    * @return Previous value for this key.
    */
   protected Object setObjectRef(Object key, Object ref)
   {
      return (key == null) ? null : m_idMap.put(key, ref); // Lookup maps cannot handle nulls
   }

   /**
    * @see nexj.core.rpc.soap.SOAPUnmarshaller
    *      #startElement(java.lang.String,
    *                    java.lang.String,
    *                    java.lang.String,
    *                    org.xml.sax.Attributes)
    */
   public void startElement(String sURI,
                            String sName,
                            String sQName,
                            Attributes attributes) throws SAXException
   {
      super.startElement(sURI, sName, sQName, attributes);

      // store the incrememnted epoch counter for this element in the position used by
      // linkObjectFixup()
      // this works because linkObjectFixup() is only called at endElement(...) by which point we
      // already dont need the value
      // super.startElement(...) tracks object position only for ComplexUnmarshaller objects
      if (getUnmarshaller() instanceof ComplexUnmarshaller)
      {
         m_indexArray[getObjectIndex().intValue()] = ++m_nEpochCounter;
      }
   }

   // inner classes

   /**
    * Accessor for char[] expressed as a list of unsignedInt
    */
   protected static class CharacterListAccessor extends ObjectListAccessor
   {
      /**
       * @see nexj.core.rpc.xml.XMLUnmarshaller.ObjectListAccessor#ObjectListAccessor(javax.xml.namespace.QName, boolean, boolean, Unmarshaller, boolean)
       */
      public CharacterListAccessor(QName type)
      {
         super(type, false, false, STRING_UNMSH, false);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectAccessor
       *      #setValue(int, int, java.lang.Object, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void setValue(int nObj,
                           int nCookie,
                           Object value,
                           SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         StringBuilder buf = getStringBuilder(nObj, ((XMLUnmarshaller)unmsh));

         buf.setLength(Math.max(buf.length(), nCookie + 1)); // add empty positions
         buf.setCharAt(nCookie, (char)Integer.parseInt((String)value));
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectAccessor
       *      #setValue(int, java.lang.Object, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void setValue(int nObj,
                           Object value,
                           SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         getStringBuilder(nObj,
                          ((XMLUnmarshaller)unmsh)).append((char)Integer.parseInt((String)value));
      }

      /**
       * Retrieve/create ArrayList associated with this accessor.
       * @param nObj The index of the object in the fixup array.
       * @param unmarshaller The unmarshaller to query for the ArrayList.
       * @return ArrayList associated with this accessor.
       */
      private StringBuilder getStringBuilder(int nObj, XMLUnmarshaller unmarshaller)
      {
         StringBuilder buf = (StringBuilder)unmarshaller.getTempValue(nObj, m_nOrdinal);

         if (buf == null)
         {
            buf = new StringBuilder();
            unmarshaller.setTempValue(nObj, m_nOrdinal, buf);
         }

         return buf;
      }
      
      /**
       * Convert a StringBuilder to a char[]
       * @param nObj The index of the object in the fixup array.
       * @param unmarshaller The unmarshaller to query for the StringBuilder.
       * @return The char[] representation.
       */
      public char[] getCharArray(int nObj, XMLUnmarshaller unmarshaller)
      {
         StringBuilder buf = (StringBuilder)unmarshaller.getTempValue(nObj, m_nOrdinal);
         char[] chArray = null;

         if (buf != null)
         {
            chArray = new char[buf.length()];
            buf.getChars(0, buf.length(), chArray, 0);
         }

         return chArray;
      }
   }

   /**
    * Complex Object unmarshaller with delayed initialization of Accessors.
    * @see SOAPUnmarshaller#ObjectUnmarshaller
    */
   protected abstract static class ComplexObjectUnmarshaller extends ComplexUnmarshaller
   {
      /**
       * A list of object assesors in the order they were added.
       * Note: Run init(XNLUnmarshaller) before accessing this object.
       */
      private List/*<ObjectInstanceAccessor>*/ m_accessorList =
         new ArrayList/*<ObjectInstanceAccessor>*/();

      /**
       * Map of <accessor namespace>+<accessor element> => accessor.
       * Note: Run init(XNLUnmarshaller) before accessing this object.
       */
      private Lookup2D/*<String, String, ObjectInstanceAccessor>*/ m_accessorMap =
         new HashTab2D/*<String, String, ObjectInstanceAccessor>*/();

      /**
       * Was init(XMLUnmarshaller) called successfully.
       */
      private boolean m_bInitialized = false;

      /**
       * Object for supplying sURI/type for exception
       * at SOAPUnmarshaller.ObjectAccessor#validate(int, SOAPUnmarshaller):2363
       * (so don't get NullPointerException),
       * yes this is a hack but can't use 'this' because
       * ComplexObjectUnmarshaller !instanceof ObjectUnmarshaller due to init(...) being final
       */
      private ObjectUnmarshaller m_unmarshallerType = new UnmarshallerTypeWrapper();

      /**
       * @param sNamespaceURI The type namespace URI.
       * @param sType The type name.
       */
      protected ComplexObjectUnmarshaller(String sNamespaceURI, String sType)
      {
         super(sNamespaceURI, sType);
      }

      /**
       * Add an accessor to list of resolvable accessors.
       * Note: getAccessor(...) will not return accessors with null namespace or element.
       * @param accessor The assesor to add.
       */
      protected void addAccessor(ObjectInstanceAccessor accessor)
      {
         assert accessor.getURI() == null || accessor.getElement() == null ||
                !m_accessorMap.contains(accessor.getURI(), accessor.getElement());

         accessor.setParentUnmarshaller(m_unmarshallerType);
         accessor.setOrdinal(m_accessorList.size());
         m_accessorList.add(accessor);

         // ignore "hidden" accessors i.e. not expected to exist e.g. MetaclassUnmarshaller.m_aRef
         if (accessor.getURI() != null && accessor.getElement() != null)
         {
            m_accessorMap.put(accessor.getURI(), accessor.getElement(), accessor);
         }
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ComplexUnmarshaller#complete(int, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public final void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         for (int i = 0, nCount = m_accessorList.size(); i < nCount; ++i)
         {
            ((ObjectInstanceAccessor)m_accessorList.get(i)).validate(nObj, unmsh);
         }

         finish(nObj, (XMLUnmarshaller)unmsh);
      }

      /**
       * Template method to complete the object creation.
       * @param nObj The index of the object in the fixup array.
       * @param unmsh The XML Unmarshaller.
       */
      protected abstract void finish(int nObj, XMLUnmarshaller unmsh)
         throws SOAPUnmarshallerException;

      /**
       * Convenience method for retrieving i'th accessor.
       * @param i The accessor ID to retrieve.
       */
      protected final ObjectInstanceAccessor getAccessor(int i)
      {
         return (ObjectInstanceAccessor)m_accessorList.get(i);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ComplexUnmarshaller#getAccessor(java.lang.String, java.lang.String, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public Accessor getAccessor(String sURI, String sType, SOAPUnmarshaller unmsh)
         throws SOAPUnmarshallerException
      {
         if (!m_bInitialized)
         {
            m_accessorList.clear();
            m_accessorMap.clear();
            init((XMLUnmarshaller)unmsh); // must do init first
            m_bInitialized = true;
         }

         Accessor accessor = (Accessor)m_accessorMap.get(sURI, sType);

         if (accessor != null)
         {
            return accessor;
         }

         throw new SOAPUnmarshallerException(
            "err.rpc.soap.element", new Object[]{sURI, sType, m_sURI, m_sType});
      }

      /**
       * Initialize all accessors here and not in the constructor since can easily blow stack in
       * the constructor on circular definitions.
       * This function should _not_ be called from the constructor to avoid circular unmarshaller
       * creation.
       * Implement by calling addAccessor(Accessor) for every accessor of this unmarshaller.
       * @param unmsh The object to use to resolve unmarshallers.
       */
      protected abstract void init(XMLUnmarshaller unmsh);

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ComplexUnmarshaller#init(org.xml.sax.Attributes, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public int init(Attributes attributes, SOAPUnmarshaller unmsh)
         throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         if (!m_bInitialized)
         {
            m_accessorList.clear();
            m_accessorMap.clear();
            init((XMLUnmarshaller)unmsh); // must do init first
            m_bInitialized = true;
         }

         return unmrsh.addObjectFixup(EMPTY, this, m_accessorList.size());
      }

      /**
       * Object for suplying sURI/type for exception
       * at SOAPUnmarshaller.ObjectAccessor#validate(int, SOAPUnmarshaller):2363
       * (so don't get NullPointerException),
       * yes this is a hack but can't use 'this' because
       * ComplexObjectUnmarshaller !instanceof ObjectUnmarshaller due to init(...) being final
       */
      protected class UnmarshallerTypeWrapper extends ObjectUnmarshaller
      {
         public UnmarshallerTypeWrapper() { super(null, null, new ObjectAccessor[0]); }
         protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException {}
         public String getType() { return ComplexObjectUnmarshaller.this.getType(); }
         public String getURI() { return ComplexObjectUnmarshaller.this.getURI(); }
      }
   }

   /**
    * Class to unmarshal basic type class-Change-Request.
    */
   protected static class DynamicChangeRequestUnmarshaller extends ObjectUnmarshaller
   {
      /**
       * The default unmarshaller used.
       */
      protected MetaclassUnmarshaller m_unmarshaller;

      /**
       * Constructor.
       * @param unmarshaller The unmarshaller to use (null == dynamic).
       */
      public DynamicChangeRequestUnmarshaller(MetaclassUnmarshaller unmarshaller)
      {
         super((unmarshaller == null) ? XML.NS_URI_TNS : unmarshaller.getURI(),
               (unmarshaller == null) ? "Change-Request" : unmarshaller.getType(),
               new ObjectAccessor[]
               {
                  new ObjectListAccessor(
                     XML.getTNSType("objects"), false, false, unmarshaller, true),
                  new StaticObjectAccessor(XML.NS_URI_TNS, "attributes", false, false, STRING_UNMSH)
               });

         m_unmarshaller = unmarshaller;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectUnmarshaller#construct()
       */
      protected Object construct()
      {
         return new XMLChangeRequest();
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectUnmarshaller#finish(int, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         XMLChangeRequest request = (XMLChangeRequest)unmrsh.getFixup(nObj);

         if (m_unmarshaller != null)
         {
            request.setMetaclass(m_unmarshaller.getMetaclass());
         }

         String sAttributes = (String)unmrsh.getTempValue(nObj, 1);

         if (sAttributes != null)
         {
            request.setAttributes((Pair)new SchemeParser(unmsh.getContext().getMachine().getGlobalEnvironment())
               .parse(new StringReader(sAttributes), null));
         }

         request.addTransferObjects((Collection)unmrsh.getTempValue(nObj, 0));
         request.setResponseType("Change-Response");
      }
   }

   /**
    * Class to unmarshal basic type class-Read-Request.
    */
   protected static class DynamicReadRequestUnmarshaller extends ObjectUnmarshaller
   {
      /**
       * The default unmarshaller used.
       */
      protected MetaclassUnmarshaller m_unmarshaller;

      /**
       * Constructor.
       * @param unmarshaller The unmarshaller to use by default (null == dynamic).
       */
      public DynamicReadRequestUnmarshaller(MetaclassUnmarshaller unmarshaller)
      {
         super((unmarshaller == null) ? XML.NS_URI_TNS : unmarshaller.getURI(),
               (unmarshaller == null) ? "Read-Request" : unmarshaller.getType(),
               new ObjectAccessor[]
               {
                  new StaticObjectAccessor(XML.NS_URI_TNS, "class", false, false, STRING_UNMSH),
                  new StaticObjectAccessor(XML.NS_URI_TNS, "attributes", false, false, STRING_UNMSH),
                  new StaticObjectAccessor(XML.NS_URI_TNS, "where", false, false, STRING_UNMSH),
                  new StaticObjectAccessor(XML.NS_URI_TNS, "orderBy", false, false, STRING_UNMSH),
                  new StaticObjectAccessor(XML.NS_URI_TNS, "count", false, false, INTEGER_UNMSH),
                  new StaticObjectAccessor(XML.NS_URI_TNS, "offset", false, false, INTEGER_UNMSH)
               });

         m_unmarshaller = unmarshaller;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectUnmarshaller#construct()
       */
      protected Object construct()
      {
         return new XMLReadRequest();
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectUnmarshaller
       *      #finish(int, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      protected void finish(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         XMLReadRequest request = (XMLReadRequest)unmrsh.getFixup(nObj);

         if (m_unmarshaller != null)
         {
            request.setMetaclass(m_unmarshaller.getMetaclass());
            request.setClassName(m_unmarshaller.getType());
         }

         String sClassName = (String)unmrsh.getTempValue(nObj, 0);

         if (sClassName != null)
         {
            request.setClassName(sClassName);
         }

         SchemeParser parser = new SchemeParser(unmsh.getContext().getMachine().getGlobalEnvironment());

         request.setAttributes((Pair)RPCUtil.parse(parser, (String)unmrsh.getTempValue(nObj, 1), null));
         request.setWhere(RPCUtil.parse(parser, (String)unmrsh.getTempValue(nObj, 2), null));
         request.setOrderBy((Pair)RPCUtil.parse(parser, (String)unmrsh.getTempValue(nObj, 3), null));
         request.setCount((Number)unmrsh.getTempValue(nObj, 4));
         request.setOffset((Number)unmrsh.getTempValue(nObj, 5));
         request.setResponseType("Read-Response");
      }
   }

   /**
    * List unmarshaller into java.util.List.
    */
   protected static class ElementListUnmarshaller extends ComplexUnmarshaller
   {
      /**
       * The accessor to use for elements.
       */
      protected ObjectAccessor m_accessor;

      /**
       * Constructor.
       * @param defaultUnmarshaller The unmarshaller to use if no type specified in incoming XML
       *                            (MS.NET doesn't specify types for some lists).
       */
      protected ElementListUnmarshaller(Unmarshaller defaultUnmarshaller)
      {
         this(defaultUnmarshaller, null, null);
      }

      /**
       * Constructs the unmarshaller.
       * @param defaultUnmarshaller The unmarshaller to use if no type specified in incoming XML
       *                            (MS.NET doesn't specify types for some lists).
       * @param sURI The type namespace URI (null == DLGenerator.NS_URI_NEXJ_XML).
       * @param sType The type name (null means "Collection").
       */
      protected ElementListUnmarshaller(Unmarshaller defaultUnmarshaller, String sURI, String sType)
      {
         this(defaultUnmarshaller, sURI, sType, XML.NS_URI_TNS, "item");
      }
      

      /**
       * Constructs the unmarshaller.
       * @param defaultUnmarshaller The unmarshaller to use if no type specified in incoming XML
       *                            (MS.NET doesn't specify types for some lists).
       * @param sURI The type namespace URI (null == DLGenerator.NS_URI_NEXJ_XML).
       * @param sType The type name (null means "Collection").
       * @param sElementURI The namespace URI of sElement to match
       *                    (if sElement then default will be used).
       * @param sElement The tag to use for sub elements (null == all tags match).
       */
      protected ElementListUnmarshaller(Unmarshaller defaultUnmarshaller,
                                        String sURI,
                                        String sType,
                                        String sElementURI,
                                        String sElement)
      {
         this(new ObjectListAccessor(new QName(sElementURI, sElement),
                                     false,
                                     false,
                                     defaultUnmarshaller,
                                     true),
              sURI,
              sType);
      }
      
      /**
       * Constructs the unmarshaller.
       * @param accessor The accessor to use.
       * @param sURI The type namespace URI (null == DLGenerator.NS_URI_NEXJ_XML).
       * @param sType The type name (null means "Collection").
       */
      protected ElementListUnmarshaller(ObjectAccessor accessor, String sURI, String sType)
      {
         super((sType != null) ? sURI : XML.NS_URI_TNS, (sType != null) ? sType : "Collection");
         
         assert accessor != null;
         
         m_accessor = accessor;

         // this is the only accessor in this object (init() reserves one spot)
         m_accessor.setOrdinal(0);
      }

      /**
       * Need to move constructed list into final place.
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ComplexUnmarshaller
       *      #complete(int, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, complete((ArrayList)unmrsh.getTempValue(nObj, 0)));
      }

      /**
       * Abstract out completion of object so that overriding instances need only override this.
       * @param list The list of values.
       * @return The object to be used as the final valu of this unmarshaller.
       */
      protected Object complete(List list)
      {
         return list;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ArrayUnmarshaller
       *      #getAccessor(java.lang.String, java.lang.String, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public Accessor getAccessor(String sURI,
                                  String sType,
                                  SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (m_accessor.getElement() != null &&
             (!m_accessor.getElement().equals(sType) ||
              (m_accessor.getURI() == null && sURI != null) ||
              !m_accessor.getURI().equals(sURI)))
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.arrayItem",
                                                new Object[]{ sURI, sType, m_sURI, m_sType });
         }
         
         return m_accessor; // have to return self to construct object item
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ComplexUnmarshaller
       *      #init(org.xml.sax.Attributes, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public int init(Attributes attributes,
                      SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         // reserve a spot with a placeholder
         // (so that nothing uses the unfinished value before it's done)
         return ((XMLUnmarshaller)unmsh).addObjectFixup(EMPTY, this, 1);
      }
   }

   /**
    * Metaclass event unmarshaller.
    */
   protected final static class EventUnmarshaller extends ComplexObjectUnmarshaller
   {
      // associations

      /**
       * Accessor for <_instance> element of non-static events.
       */
      protected ObjectInstanceAccessor m_instanceAccessor;

      /**
       * The Event that can be unmarshalled.
       */
      protected Event m_event;

      /**
       * The result tag type.
       */
      protected QName m_resultType;

      // constructors

      /**
       * @param type The event element type (not null).
       * @param event The event to unmarshal for (not null).
       */
      public EventUnmarshaller(QName type, Event event)
      {
         super(type.getNamespaceURI(), type.getLocalPart());

         assert event != null;

         m_event = event;
         m_resultType = new QName(m_sURI, m_sType + "_Response"); // as defined in XSDGenerator.java
      }

      // operations

      /**
       * @see nexj.core.rpc.xml.XMLUnmarshaller.ComplexObjectUnmarshaller#finish(int, nexj.core.rpc.xml.XMLUnmarshaller)
       */
      protected void finish(int nObj, XMLUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         TransferObject instance = null;

         // get the instance this event call is supposed to reference (last accessor see init(...))
         if (m_instanceAccessor != null)
         {
            instance = (TransferObject)m_instanceAccessor.getValue(nObj, unmsh);

            if (instance == null)
            {
               throw new SOAPUnmarshallerException(
                  "err.rpc.xml.unboundEvent", new Object[]{m_event.toString()});
            }
         }

         XMLInvocationRequest request = // WSDL events always return a list for MS.NET compatibility
            new XMLInvocationRequest(m_event, instance, m_resultType, true);

         unmsh.setFixup(nObj, request); // replace the 'EMPTY' value vith a valid value

         // set the non varArg arguments (Event argument ordinal == i @see init(XMLUnmarshaller))
         for (int i = 0, nCount = m_event.getArgumentCount() - ((m_event.isVarArg()) ? 1 : 0);
              i < nCount;
              ++i)
         {
            request.addArgument(getAccessor(i).getValue(nObj, unmsh));
         }

         // last arg is a list that needs to be expanded @see init(XMLUnmarshaller) for reasons why
         if (m_event.isVarArg() && m_event.getArgumentCount() > 0)
         {
            List list = (List)getAccessor(m_event.getArgumentCount() - 1).getValue(nObj, unmsh);

            if (list == null || list.isEmpty()) // null/empty varArg value
            {
               request.addArgument(null); // last value will be null
            }
            else
            {
               // expand varArg list to argArray starting at varArg possition
               for (int i = 0, nCount = list.size(); i < nCount; ++i)
               {
                  request.addArgument(list.get(i));
               }
            }
         }
      }

      /**
       * @see nexj.core.rpc.xml.XMLUnmarshaller.ComplexObjectUnmarshaller#init(nexj.core.rpc.xml.XMLUnmarshaller)
       */
      protected void init(XMLUnmarshaller unmsh)
      {
         // for every argument add an accessor (do first so arg ID's match accessor ordinal)
         for (int i = 0, nCount = m_event.getArgumentCount(); i < nCount; ++i)
         {
            Argument arg = m_event.getArgument(i);
            QName element = new QName(m_sURI, arg.getName());
            Unmarshaller unmarshaller = null;
            boolean bVarArg = m_event.isVarArg() && i == nCount - 1;
            boolean bTyped = false;

            // find unmarshaller for typed argument
            if (arg.getType() != Primitive.ANY)
            {
               QName qType = XML.getQualifiedType(arg.getType());

               bTyped = true;
               unmarshaller = (bVarArg && arg.isCollection())
                  ? unmsh.getListUnmarshaller(qType.getNamespaceURI(), qType.getLocalPart(), null)
                  : unmsh.getUnmarshaller(qType.getNamespaceURI(), qType.getLocalPart(), null);
            }

            assert unmarshaller != null || !bTyped; // ensure can unmarshall every argument

            addAccessor((bVarArg || (bTyped && arg.isCollection()))
                        ? new ObjectListAccessor(element, false, true, unmarshaller, !bTyped)
                        : new ObjectInstanceAccessor(element, false, true, unmarshaller, !bTyped));
         }

         // non-static events have an extra <_instance> element
         if (!m_event.isStatic())
         {
            MetaclassUnmarshaller unmarshaller = (MetaclassUnmarshaller)unmsh.getUnmarshaller(
               XML.NS_URI_TNS, m_event.getMetaclass().getName(), null);

            m_instanceAccessor = new ObjectInstanceAccessor(
               new QName(m_sURI, XML.BASE_PREFIX  + "instance"), true, true, unmarshaller, false);
            addAccessor(m_instanceAccessor);
         }
      }
   }

   /**
    * Metaclass unmarshaller.
    */
   protected final static class MetaclassUnmarshaller extends ComplexObjectUnmarshaller
   {
      /**
       * This will be used if type is unspecified in the object model and in the XML.
       */
      protected final static QName ANY_TYPE = XML.getQualifiedType(Primitive.STRING);

      // Accessors for elements of TransferObject
      protected ObjectInstanceAccessor m_classAccessor = new ObjectInstanceAccessor(
         XML.getTNSType(XML.BASE_PREFIX + "class"), false, true, STRING_UNMSH, false);

      protected ObjectInstanceAccessor m_eventAccessor = new ObjectInstanceAccessor(
         XML.getTNSType(XML.BASE_PREFIX  + "event"), false, true, STRING_UNMSH, false);

      protected ObjectInstanceAccessor m_versionAccessor = new ObjectInstanceAccessor(
         XML.getTNSType(XML.BASE_PREFIX + "version"), false, true, INT_UNMSH, false);

      protected ObjectInstanceAccessor m_oidAccessor = new ObjectInstanceAccessor(
         XML.getTNSType(XML.BASE_PREFIX + "oid"), false, false, OID_UNMSH, false);

      protected ObjectListAccessor m_keysAccessor = new ObjectListAccessor(
         XML.getTNSType(XML.BASE_PREFIX + "keys"), false, true, STRING_UNMSH, false);

      protected ObjectListAccessor m_valuesAccessor = new ObjectListAccessor(
         XML.getTNSType(XML.BASE_PREFIX + "values"), false, true, null, true);

      /**
       * An accessor to object denoting the reference properties of this TransferObject.
       * This accessor is never actually used for unmarshalling,
       * only storing/retrieving once OID is unmarshaled.
       */
      protected ObjectInstanceAccessor m_refAccessor =
         new ObjectInstanceAccessor(null, false, true, STRING_UNMSH, false);

      /**
       * Map of attributes to their corresponding accessor array (Attribute -> ObjectAccessor).
       * Note: Run init(XMLUnmarshaller) before accessing this object.
       * m_accessorMap.size() >= m_attributeMap.size() due to m_accessorMap containing
       * TransferObject elements as well.
       */
      private Lookup/*<Attribute, ObjectInstanceAccessor>*/ m_attributeMap;

      /**
       * The metaclass that can be unmarshalled.
       */
      protected Metaclass m_metaclass;

      /**
       * Map of <Event QName> => unmarshaller.
       */
      private Lookup/*<QName, Unmarshaller>*/ m_eventMap = new HashTab/*<QName, Unmarshaller>*/();

      // constructors

      /**
       * @param sURI The Type namespace URI.
       * @param sType The type name.
       * @param metaclass The metaclass we are unmarshalling.
       * @throws MetadataLookupException If metaclass not found.
       */
      protected MetaclassUnmarshaller(String sURI, String sType,
         Metaclass metaclass) throws MetadataLookupException
      {
         super(sURI, sType);
         m_metaclass = metaclass;

         if (m_metaclass == null)
         {
            return; // no extra configuration required for generic TransferObject unmarshaller
         }

         String sEvURI = m_sURI + '/' + m_sType;

         // create unmarshallers for all WSDL exported events
         for (int i = 0, nCount = m_metaclass.getEventCount(); i < nCount; ++i)
         {
            Event event = m_metaclass.getEvent(i);

            if (event.getVisibility() == Metaclass.PUBLIC)
            {
               QName type = new QName(sEvURI, XSDGenerator.computeElementName(event));

               m_eventMap.put(type, new EventUnmarshaller(type, event));
            }
         }
      }

      // operations

      /**
       * @return The metaclass.
       */
      public Metaclass getMetaclass()
      {
         return m_metaclass;
      }

      /**
       * @see nexj.core.rpc.xml.XMLUnmarshaller.ComplexObjectUnmarshaller#finish(int, nexj.core.rpc.xml.XMLUnmarshaller)
       */
      protected void finish(int nObj, XMLUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         boolean bRef = true;
         Object value = m_classAccessor.getValue(nObj, unmsh);
         TransferObject instance = (TransferObject)unmsh.getFixup(nObj);

         instance.setClassName((m_metaclass == null) ? null : m_metaclass.getName());

         // value in '_class' takes precedence
         if (value != null)
         {
            instance.setClassName((String)value);
            
            // ref can only have class & oid (only one OID)
            bRef = (m_metaclass == null || m_metaclass.getName().equals(value) && bRef);
         }

         value = m_eventAccessor.getValue(nObj, unmsh);
         bRef = (value == null && bRef); // ref can only have class & oid
         instance.setEventName((String)value);

         value = m_versionAccessor.getValue(nObj, unmsh);

         if (value != null)
         {  // if no version provided then leave at default
            instance.setVersion(((Integer)value).shortValue());
            bRef = false; // ref can only have class & oid
         }

         value = m_oidAccessor.getValue(nObj, unmsh);

         if (value instanceof OID)
         {
            instance.setOID((OID)value);
         }
         else if (value != null)
         {  // don't store null OIDs since they can map to anything
            unmsh.setObjectRef(instance, value);
         }

         value = m_valuesAccessor.getValue(nObj, unmsh);
         bRef = (value == null && bRef); // ref can only have class & oid
         Iterator/*<Object>*/ valueItr = (value == null)
                                       ? EmptyIterator.getInstance()
                                       : ((List/*<Object>*/)value).iterator();

         value = m_keysAccessor.getValue(nObj, unmsh);
         bRef = (value == null && bRef); // ref can only have class & oid

         for(Iterator/*<String>*/ keyItr = (value == null)
                                         ? EmptyIterator.getInstance()
                                         : ((List/*<Object>*/)value).iterator();
             // it's valid for there to be more keys then values,
             // the remaining keys are assigned 'null' values
             keyItr.hasNext();
             instance.setValue((String)keyItr.next(),
                               (!valueItr.hasNext()) ? null : valueItr.next()));
         
         // more values then keys
         if (valueItr.hasNext())
         {
            throw new SOAPMarshallerException("err.rpc.soap.keyValueMismatch",
                                              new Object[]{m_sURI, m_sType});
         }

         for (Lookup.Iterator/*<Attribute, ObjectInstanceAccessor>*/ itr =
                 m_attributeMap.iterator();
              itr.hasNext();)
         {
            Attribute attribute = (Attribute)itr.next();
            ObjectInstanceAccessor accessor = (ObjectInstanceAccessor)itr.getValue();

            value = accessor.getValue(nObj, unmsh);

            // ObjectListAccessor use null to indicate there are no elements present
            value = (accessor instanceof ObjectListAccessor && value == null) ? EMPTY : value;

            if (value != EMPTY)
            {  // if have an explicitly set value
               instance.setValue(attribute.getName(), value);
               bRef = false; // ref can only have class & oid
            }
         }

         // remember valid compleated value
         m_refAccessor.setValue(nObj,
            new MetaclassRef((m_metaclass == null) ? (String)m_classAccessor.getValue(nObj, unmsh)
                                                   : m_metaclass.getName(),
                             m_oidAccessor.getValue(nObj, unmsh),
                             Boolean.valueOf(bRef)),
            unmsh);
      }

      /**
       * Finds the appropriate unmarshaller for the specific qType.
       * @param qType The qalified type of the unmarshaller to create (not null).
       * @return the proper unmarshaller for qType or null if no proper unmarshaller known.
       */
      public Unmarshaller findUnmarshaller(QName qType)
      {
         assert qType != null;

         return (Unmarshaller)m_eventMap.get(qType);
      }

      /**
       * Returns corrected qualified type for a specific type.
       * @param type The type to examine.
       * @return Corrected qualified type (or null if type = null).
       */
      protected QName getQualifiedType(Type type)
      {
         if (type == null)
         {
            return null;
         }

         return (type == Primitive.ANY) ? ANY_TYPE : XML.getQualifiedType(type);
      }

      /**
       * Return the reference information for this object.
       * @param nObj The object to get the value for.
       * @param state The object containing the unmarshalling state.
       * @return The reference information for this object
       *         (or as much as available up to this point in object's construction)
       *         temporary valies have isRef == null.
       */
      public MetaclassRef getRef(int nObj, XMLUnmarshaller state)
      {
         Object value = m_refAccessor.getValue(nObj, state);

         // always regenerate temporary value because m_aClass and m_aOID might start returning
         // valid values once they've been unmarshalled
         return (value != null && value != EMPTY)
              ? (MetaclassRef)value
              : new MetaclassRef((m_metaclass == null)
                                 ? (String)m_classAccessor.getValue(nObj, state)
                                 : m_metaclass.getName(),
                                 m_oidAccessor.getValue(nObj, state),
                                 null);
      }

      /**
       * @see nexj.core.rpc.xml.XMLUnmarshaller.ComplexObjectUnmarshaller#init(nexj.core.rpc.xml.XMLUnmarshaller)
       */
      protected void init(XMLUnmarshaller unmsh)
      {
         addAccessor(m_refAccessor);
         addAccessor(m_classAccessor);
         addAccessor(m_eventAccessor);
         addAccessor(m_versionAccessor);
         addAccessor(m_oidAccessor);
         addAccessor(m_keysAccessor);
         addAccessor(m_valuesAccessor);

         // use a temporary pointer in the event that an exception causes an early exit from
         // function, the actual m_attributeMap isn't left half initialized (yes it happend)
         Lookup attributeMap = new HashTab/*<Attribute, ObjectAccessor>*/();

         if (m_metaclass != null)
         {
            for (Iterator/*<Attribute>*/ itr = m_metaclass.getAttributeIterator(); itr.hasNext();)
            {
               Attribute attribute = (Attribute)itr.next(); 
   
               if (attribute.getVisibility() != Metaclass.PUBLIC)
               {
                  continue; // ignore non-publicly visible attributes
               }

               String sTmpName = XSDGenerator.computeElementName(attribute);
               QName qType = getQualifiedType(attribute.getType());
               Unmarshaller unmarshaller =
                  unmsh.getUnmarshaller(qType.getNamespaceURI(), qType.getLocalPart(), null);

               if (unmarshaller != null)
               {
                  QName type = XML.getTNSType(sTmpName);
                  ObjectInstanceAccessor accessor = (attribute.isCollection())
                     ? new ObjectListAccessor(type, false, true, unmarshaller, false)
                     : new ObjectInstanceAccessor(type, false, true, unmarshaller, false);

                  addAccessor(accessor);
                  attributeMap.put(attribute, accessor);
               }
            }
         }

         m_attributeMap = attributeMap;
      }

      /**
       * @see nexj.core.rpc.xml.XMLUnmarshaller.ComplexObjectUnmarshaller#init(org.xml.sax.Attributes, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public int init(Attributes attributes, SOAPUnmarshaller unmsh)
         throws SOAPUnmarshallerException
      {
         int nObj = super.init(attributes, unmsh);

         // must replace EMPTY with placeholder instacne for proper recursive metaclass linking
         ((XMLUnmarshaller)unmsh).setFixup(nObj, new TransferObject());

         // mark all objects representing individual attribues as unset so that can easily tell
         // when a 'null' value is set
         // (leave TransferObject accessors alone since there null == unset)
         for (Iterator/*<ObjectInstanceAccessor>*/ itr = m_attributeMap.valueIterator();
              itr.hasNext();)
         {
            ObjectInstanceAccessor accessor = (ObjectInstanceAccessor)itr.next();

            // ObjectListAccessor use null to indicate there are no elements present
            if (!(accessor instanceof ObjectListAccessor))
            {
               // use the nCookie capable function since it does a strait store and doesn't check
               // for duplicates
               accessor.setValue(nObj, 0, EMPTY, unmsh);
            }
         }

         return nObj;
      }

      /**
       * Class representing the reference information for this metaclass.
       */
      public class MetaclassRef
      {
         // if this object is a reference (i.e. only contains class and OID)
         // null == unfinished initialization -> unknown
         public final Boolean isRef;

         // either an OID type or some other type that signifies a temporary OID
         public final Object oid;

         // the metaclass type of this object
         // (for Metaclass this is the tag name and for TransferObject this is the _-class value)
         public final String sType;
         
         public MetaclassRef(String sTypeValue, Object oidValue, Boolean isRefValue)
         {
            isRef = isRefValue;
            sType = sTypeValue;
            oid = oidValue;
         }

         /**
          * @see java.lang.Object#toString()
          */
         public String toString()
         {
            return ((isRef == null) ? "?" : ((isRef.booleanValue()) ? "#" : "")) +
                    sType +
                    "->" +
                    oid;
         }
      }
   }

   /**
    * Accessor capable of returning the value set at the initialized ordinal and
    * determining object type dynamically based on xsd:type attribute or the supplied unmarshaller.
    */
   protected static class ObjectInstanceAccessor extends StaticObjectAccessor
   {
      /**
       * Override unmarshaller with proper type if "xsi:type" attribute available.
       */
      protected boolean m_bPolymorphic;

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.StaticObjectAccessor#StaticObjectAccessor(String, String, boolean, boolean, nexj.core.rpc.soap.SOAPUnmarshaller)
       * @param type The qualified type of the element.
       * @param bRequired True if the value is required.
       * @param bDeferred True if the accessor is deferred.
       * @param unmsh The unmarshaller to use if no type specified in incoming XML or !bPolymorphic.
       * @param bPolymorphic Check the xsi:type attribute and use that to choose unmarshaller.
       *                     If !bPolymorphic && unmsh == null
       *                       => use tag name to determine unmarshaller.
       *                     If !bPolymorphic && unmsh != null
       *                       => use unmsh (i.e. fallback on unmsh if it's available).
       *                     If bPolymorphic && XSD:type != null
       *                       => use XSD:type name to determine unmarshaller.
       *                     If bPolymorphic && XSD:type == null && unmsh == null
       *                       => use tag name to determine unmarshaller.
       *                     If bPolymorphic && XSD:type == null && unmsh != null
       *                       => use unmsh (i.e. fallback on unmsh if it's available).
       */
      public ObjectInstanceAccessor(
         QName type, boolean bRequired, boolean bDeferred, Unmarshaller unmsh, boolean bPolymorphic)
      {
         super((type == null) ? null : type.getNamespaceURI(),
               (type == null) ? null : type.getLocalPart(),
               bRequired,
               bDeferred,
               unmsh);

         m_bPolymorphic = bPolymorphic;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.StaticObjectAccessor#getUnmarshaller(java.lang.String, java.lang.String, org.xml.sax.Attributes, nexj.core.rpc.soap.SOAPUnmarshaller)
       * MS.NET provides a type for some arrays and none for others.
       * If there is a type provided then use that.
       */
      public Unmarshaller getUnmarshaller(String sURI, String sName, Attributes attributes,
         SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         Unmarshaller unmarshaller = super.getUnmarshaller(sURI, sName, attributes, unmsh);
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         if (m_bPolymorphic)
         {
            String sType = attributes.getValue(XML.XSI_URI, "type");

            if (sType != null)
            {
               QName qType = // might be a qualified type
                  unmrsh.resolveType(unmrsh.findURI(XMLConstants.DEFAULT_NS_PREFIX), sType);

               unmarshaller =
                  unmrsh.getUnmarshaller(qType.getNamespaceURI(), qType.getLocalPart(), attributes);
            }
         }

         // each element corresponds to an object to be unmarshalled
         return (unmarshaller != null)
                ? unmarshaller : unmrsh.getUnmarshaller(sURI, sName, attributes);
      }

      /**
       * Gets the value of the member of the object.
       * @param nObj The index of the object in the fixup array.
       * @return The value stored at that index taking into consideration
       *         m_nOrdinal (set via setOrdinal(int)).
       */
      public Object getValue(int nObj, XMLUnmarshaller unmsh)
      {
         return unmsh.getTempValue(nObj, m_nOrdinal);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectAccessor#validate(int, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void validate(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         // This method is needed for changing the visibility
         super.validate(nObj, unmsh);
      }
   }

   /**
    * Object accessor for an object list.
    * i.e. Same tag repeating multiple times.
    */
   protected static class ObjectListAccessor extends ObjectInstanceAccessor
   {
      /**
       * @see ObjectInstanceAccessor#ObjectInstanceAccessor(javax.xml.namespace.QName, boolean, boolean, Unmarshaller, boolean)
       */
      public ObjectListAccessor(
         QName type, boolean bRequired, boolean bDeferred, Unmarshaller unmsh, boolean bPolymorphic)
      {
         super(type, bRequired, bDeferred, unmsh, bPolymorphic);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectAccessor
       *      #setFixup(int, java.lang.Object, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh)
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         ArrayList list = getList(nObj, m_nOrdinal, unmrsh);

         setValue(nObj, EMPTY, unmsh); // reserve a spot in the array for the value

         // nCookie == reserved spot in the array
         unmrsh.addReferenceFixup(nObj, ref, this, list.size() - 1);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectAccessor
       *      #setValue(int, int, java.lang.Object, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void setValue(int nObj,
                           int nCookie,
                           Object value,
                           SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         ArrayList list = getList(nObj, m_nOrdinal, ((XMLUnmarshaller)unmsh));

         // add empty positions
         for(list.ensureCapacity(nCookie + 1); list.size() <= nCookie; list.add(null));

         list.set(nCookie, value);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.ObjectAccessor
       *      #setValue(int, java.lang.Object, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public void setValue(int nObj,
                           Object value,
                           SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         getList(nObj, m_nOrdinal, ((XMLUnmarshaller)unmsh)).add(value);
      }
      
      /**
       * Retrieve/create ArrayList associated with this accessor.
       * @param nObj The index of the object in the fixup array.
       * @param nOrdinal The accessor ordinal number.
       * @param unmarshaller The unmarshaller to query for the ArrayList.
       * @return ArrayList associated with this accessor.
       */
      private ArrayList getList(int nObj, int nOrdinal, XMLUnmarshaller unmarshaller)
      {
         ArrayList list = (ArrayList)unmarshaller.getTempValue(nObj, m_nOrdinal);
         
         if (list == null)
         {
            list = new ArrayList/*<Object>*/();
            unmarshaller.setTempValue(nObj, m_nOrdinal, list);
         }
         
         return list;
      }
   }

   /**
    * Class to wrap a SimpleUnmarshaller allowing to change the apprent URI/type and still reuse
    * the original implementation.
    */
   protected static class SimpleUnmarshallerWrapper extends SimpleUnmarshaller
   {
      /**
       * The wrapped unmarshaller.
       */
      protected SimpleUnmarshaller m_unmarshaller;

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.SimpleUnmarshaller
       *      #SimpleUnmarshaller(java.lang.String, java.lang.String)
       */
      protected SimpleUnmarshallerWrapper(String sURI, String sType, SimpleUnmarshaller wrapped)
      {
         super(sURI, sType);
         
         m_unmarshaller = wrapped;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPUnmarshaller.SimpleUnmarshaller
       *      #unmarshal(java.lang.String, nexj.core.rpc.soap.SOAPUnmarshaller)
       */
      public Object unmarshal(String sValue, SOAPUnmarshaller unmsh) throws Exception
      {
         return (m_unmarshaller == null) ? null : m_unmarshaller.unmarshal(sValue, unmsh);
      }
   }

   /**
    * env:Body unmarshaller.
    * The valid object is the either the first SOAPFault or the first element not having the
    * attribute 'root="0"'.
    * It is invalid to have multiple SOAPFaults.
    * A valid object must have been set by the time complete() is executed.
    */
   protected static class SOAPBodyUnmarshaller extends ComplexUnmarshaller implements Accessor
   {
      public SOAPBodyUnmarshaller()
      {
         super(XML.ENV_URI, "Body");
      }

      public int init(Attributes attributes,
                      SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return ((XMLUnmarshaller)unmsh).addObjectFixup(EMPTY, this, 1);
      }

      public Accessor getAccessor(String sURI,
                                  String sType,
                                  SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         return this;
      }

      public void complete(int nObj, SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         if (((XMLUnmarshaller)unmsh).getTempIndex(nObj, 0) == 0)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.root");
         }
         
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         unmrsh.setFixup(nObj, unmrsh.getTempValue(nObj, 0));
      }

      public boolean isMultiRef()
      {
         return false;
      }

      public void setValue(int nObj,
                           Object value,
                           SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         if (!unmrsh.m_bIgnoreRoot)
         {  // don't store values that were explicitly marked as not the value i.e. root="0"
            setValue(nObj, 0, value, unmsh);
         }
         
         setIgnoreRoot(nObj, unmrsh);
      }

      public void setValue(int nObj,
                           int nCookie,
                           Object value,
                           SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         // this is the second fault in the input
         if (unmrsh.getTempValue(nObj, 0) instanceof SOAPFault)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.dupFault",
                                                new Object[]{m_sURI, m_sType});
         }

         unmrsh.setTempValue(nObj, 0, value);
         unmrsh.setTempIndex(nObj, 0, 1); // mark value as having been set
      }

      public void setFixup(int nObj, Object ref, SOAPUnmarshaller unmsh)
      {
         if (ref instanceof String)
         {
            throw new SOAPUnmarshallerException("err.rpc.soap.hrefTop",
                                                new Object[]{m_sURI, m_sType});
         }

         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;

         if (!unmrsh.m_bIgnoreRoot)
         {  // don't store values that were explicitly marked as not the value i.e. root="0"
            unmrsh.addReferenceFixup(nObj, ref, this, 0);
         }

         setIgnoreRoot(nObj, unmrsh);
      }

      protected void setIgnoreRoot(int nObj, XMLUnmarshaller unmsh)
      {  // current object was explicitly marked as not the value i.e. root="0"
         if (unmsh.m_bIgnoreRoot)
         {
            if (unmsh.getTempIndex(nObj, 0) == 0)
            {  // allow next value to be considered as the body element
               unmsh.m_bIgnoreRoot = false;
            }
         }
         else
         {  
            unmsh.setTempIndex(nObj, 0, 1); // mark value as having been set
            unmsh.m_bIgnoreRoot = true;
         }
      }
      
      public Unmarshaller getUnmarshaller(String sURI,
                                          String sName,
                                          Attributes attributes,
                                          SOAPUnmarshaller unmsh) throws SOAPUnmarshallerException
      {
         XMLUnmarshaller unmrsh = (XMLUnmarshaller)unmsh;
         
         // check if we are supposed to have a static outermost unmarshaller
         // (to prevent incorrect XML from triggering wrong Metatype)
         Unmarshaller u = (unmrsh.m_metaclassStaticUnmarshaller != null)
                        ? unmrsh.m_metaclassStaticUnmarshaller
                        : unmrsh.getUnmarshaller(attributes);
         
         if (u == null)
         {  
            u = unmrsh.getUnmarshaller(sURI, sName, attributes);
         }

         if (unmrsh.m_bIgnoreRoot)
         {
            if (u == SOAP_FAULT_UNMSH)
            {
               unmrsh.m_bIgnoreRoot = false;
            }
         }
         else
         {
            // root="0" is an element that should be ignored
            // (but still unmarshalled as it can contain objects for "href" resolution)
            // as it's not part of the message (hence ignore next element)
            if ("0".equals(attributes.getValue(XML.ENV_URI, "root")) && u != SOAP_FAULT_UNMSH)
            {
               unmrsh.m_bIgnoreRoot = true;
            }
         }

         return u;
      }

      public boolean isDeferred()
      {
         return false;
      }
   }
}