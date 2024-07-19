// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.Type;
import nexj.core.persistence.OID;
import nexj.core.rpc.ErrorLocationHolder;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAPFault;
import nexj.core.rpc.soap.SOAPMarshaller;
import nexj.core.rpc.soap.SOAPMarshallerException;
import nexj.core.runtime.Context;
import nexj.core.runtime.InvocationContext;
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
import nexj.core.util.Iteratable;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.LookupDeque;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.Undefined;
import nexj.core.util.WrapperIterator;
import nexj.core.util.XMLWriter;

/**
 * Document/literal SOAP/XML marshaller.
 * shares most basic objects with SOAPMarshaller hence extends it to use them.
 * Needs to extend SOAPMarshaller to gain access to protected marshalling types/objects.
 */
public class XMLMarshaller extends SOAPMarshaller
{
   /**
    * Character marshaller.
    */
   protected final static Marshaller CHAR_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         ((XMLMarshaller)msh).m_writer.writeInt(((Character)obj).charValue());
      }

      public String getType()
      {
         return "unsignedShort";
      }
   };

   /**
    * char[] marshaller.
    */
   protected final static Marshaller CHAR_ARRAY_MSH = new ElementListMarshaller(CHAR_MSH)
   {
      /**
       * @see nexj.core.rpc.rest.XMLMarshaller.ElementListMarshaller
       *      #marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         ((XMLMarshaller)msh).marshalInline(obj, m_sElementNS, m_sElement, m_marshaller, MF_NIL);
      }
   };

   /**
    * Throwable marshaller.
    */
   protected final static Marshaller EXCEPTION_MSH = new ObjectMarshaller()
   {
      public void marshalContents(final Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         Throwable t = (Throwable)obj;
         
         if (obj instanceof ErrorCode)
         {
            ErrorCode e = (ErrorCode)t;
            
            mrsh.marshal(e.getErrorCode(), XML.TNS_NS, "errorCode", STRING_MSH, MF_LITERAL);
            mrsh.marshal((msh.getContext() != null) ? msh.getContext().formatString(e.getErrorCode(), e.getErrorArgs()) :
               t.getLocalizedMessage(), XML.TNS_NS, "errorMessage", STRING_MSH, MF_LITERAL);
            mrsh.marshalInline(e.getErrorArgs(), XML.TNS_NS, "errorArgs", null, MF_NIL | MF_TYPE);
         }
         else
         {
            mrsh.marshal("err." + t.getClass().getName(), XML.TNS_NS, "errorCode", STRING_MSH, MF_LITERAL);
            mrsh.marshal(t.getLocalizedMessage(), XML.TNS_NS, "errorMessage", STRING_MSH, MF_LITERAL);
         }

         if (obj instanceof ErrorLocationHolder)
         {
            final ErrorLocationHolder h = (ErrorLocationHolder)obj;

            mrsh.marshal(h.getClassName(), XML.TNS_NS, "class", STRING_MSH, MF_LITERAL);

            if (h.getOIDHolder() != null)
            {
               mrsh.marshal(h.getOIDHolder().getOID(), XML.TNS_NS, "oid", OID_MSH, MF_LITERAL);
            }

            mrsh.marshal(Primitive.createInteger(h.getOrdinal()), XML.TNS_NS, "ordinal", INTEGER_MSH, 0);

            if (h.getAttributeCount() != 0)
            {
               mrsh.marshalInline(h.getAttributeIterator(), XML.TNS_NS, "attributes", STRING_MSH, MF_LITERAL);
               mrsh.marshalInline(new WrapperIterator(h.getAttributeIterator())
               {
                  public Object next()
                  {
                     return h.findException((String)m_itr.next());
                  }
               }, XML.TNS_NS, "attributeExceptions", EXCEPTION_MSH, MF_LITERAL);
            }
         }
         else
         {
            mrsh.marshal(Primitive.createInteger(-1), null, "ordinal", INTEGER_MSH, 0);
         }

         if (obj instanceof ExceptionHolder)
         {
            if (((ExceptionHolder)obj).getExceptionCount() != 0)
            {
               Iterator exceptionsItr = 
                  new WrapperIterator(((ExceptionHolder)obj).getExceptionIterator())
               {
                  private Throwable m_next = null;

                  public boolean hasNext()
                  {
                     for (m_next = (m_next != null || !m_itr.hasNext()) ? m_next : (Throwable)m_itr.next();
                        m_next != null && ObjUtil.isSystem(m_next);
                        m_next = (!m_itr.hasNext()) ? null : (Throwable)m_itr.next());

                     return m_next != null;
                  }

                  public Object next()
                  {
                     hasNext();

                     Object next = m_next;

                     m_next = null;

                     return next;
                  }
               };

               mrsh.marshalInline(exceptionsItr, XML.TNS_NS, "exceptions", EXCEPTION_MSH, MF_LITERAL);
            }
         }
         else
         {
            Throwable cause = t.getCause();

            if (cause != null && ObjUtil.isSystem(cause))
            {
               cause = null;
            }

            mrsh.marshal(cause, XML.TNS_NS, "exceptions", EXCEPTION_MSH, MF_LITERAL);
         }
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Exception";
      }
   };

   /**
    * Function marshaller.
    */
   protected final static Marshaller FUNCTION_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         PCodeFunction fun = ((PCodeFunction)obj).getCleanCopy();

         mrsh.marshalInline(fun.code, XML.TNS_NS, "code", CHAR_MSH, MF_NIL);
         mrsh.marshalInline(fun.constants, XML.TNS_NS, "constants", null, MF_NIL | MF_TYPE);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Function";
      }
   };

   /**
    * Macro marshaller.
    */
   protected final static Marshaller MACRO_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         PCodeMacro fun = (PCodeMacro)((PCodeMacro)obj).getCleanCopy();

         mrsh.marshalInline(fun.code, XML.TNS_NS, "code", CHAR_MSH, MF_NIL);
         mrsh.marshalInline(fun.constants, XML.TNS_NS, "constants", null, MF_NIL | MF_TYPE);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Macro";
      }
   };

   /**
    * MS.NET generates it's own type for an object array (called "ArrayOfAnyType"), 
    * then puts it into NexJ's namespace.
    * MS.NET can only accept this type for Object[].
    * Our Marshaller therefore defines the type and uses it to marshal Object[].
    * The child tag that MS.NET expects is sometimes "values" and other times it's "item" even
    * though it might be defined opposite in XSD for both "ArrayOfAnyType" and "Array".
    * TODO If anyone can figure why the child tag expected by MS.NET keeps changing then go ahead 
    *      and make it pick one or the other, both here and in DLGenerator.
    */
   protected final static Marshaller OBJECT_ARRAY_MSH = 
      new ElementListMarshaller(null, "ArrayOfAnyType", XML.TNS_NS, "item");

   /**
    * Object collection marshaller.
    */
   protected final static Marshaller OBJECT_LIST_MSH = new ElementListMarshaller(null);
 
   /**
    * OID marshaller.
    */
   protected final static Marshaller OID_MSH = new PrimitiveMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         byte[] oid = ((OID)obj).toBinary().getData();

         Binary.write(((XMLMarshaller)msh).m_writer, oid, 0, oid.length);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "OID";
      }
   };

   /**
    * Pair marshaller.
    * Copy from SOAPMarshaller with overridden type.
    */
   protected final static Marshaller PAIR_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         Pair pair = (Pair)obj;

         mrsh.marshal(pair.getHead(), null, "head", null, MF_TYPE);
         mrsh.marshal(pair.getTail(), null, "tail", null, MF_TYPE);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Pair";
      }
   };

   /**
    * PrivilegeSet marshaller.
    * Copy from SOAPMarshaller with overridden type.
    */
   protected final static Marshaller PRIVILEGE_SET_MSH = new ObjectMarshaller()
   {
      private final Marshaller MASK_MSH = new PrimitiveMarshaller()
      {
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            XMLMarshaller mrsh = (XMLMarshaller)msh;

            Base64Util.encode(new ByteArrayInputStream(((PrivilegeSet)obj).getMask()), mrsh.m_writer, -1, false);
         }

         public String getType()
         {
            return "base64Binary";
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;

         mrsh.marshal(obj, null, "mask", MASK_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "PrivilegeSet";
      }
   };

   /**
    * byte[] marshaller.
    * Copy from SOAPMarshaller with overridden type.
    */
   protected final static Marshaller BYTE_VECTOR_MSH = new ObjectMarshaller()
   {
      private final Marshaller MASK_MSH = new PrimitiveMarshaller()
      {
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            XMLMarshaller mrsh = (XMLMarshaller)msh;

            Base64Util.encode(new ByteArrayInputStream((byte[])obj), mrsh.m_writer, -1, false);
         }

         public String getType()
         {
            return "base64Binary";
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;

         mrsh.marshal(obj, null, "value", MASK_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "ByteVector";
      }
   };

   /**
    * SOAP Fault marshaller.
    */
   protected final static Marshaller SOAP_FAULT_MSH = new ObjectMarshaller()
   {
      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.ObjectMarshaller
       *      #marshalAttributes(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException
      {
          // add namespace for SOAP Fault
         ((XMLMarshaller)msh).writeAttribute("xmlns", XML.ENV_NS, null, XML.ENV_URI, null);
      }

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         SOAPFault fault = (SOAPFault)obj;
         
         if (fault.getCode() != null)
         {
            mrsh.marshal(((XML.ENV_URI.equals(fault.getURI())) ? XML.ENV_NS + ":" : "") + fault.getCode(),
               XML.ENV_NS, "faultcode", STRING_MSH, MF_LITERAL);
         }

         if (fault.getMessage() != null)
         {
            mrsh.marshal(fault.getMessage(), XML.ENV_NS, "faultstring", STRING_MSH, MF_LITERAL);
         }
         
         if (fault.getException() != null)
         {
            mrsh.marshal(fault.getException(), XML.ENV_NS, "detail", EXCEPTION_MSH, MF_LITERAL);
         }
      }

      public String getNamespace()
      {
         return XML.ENV_NS;
      }

      public String getType()
      {
         return "Fault";
      }
   };

   /**
    * Symbol marshaller.
    * Copy from SOAPMarshaller with overridden type.
    */
   protected final static Marshaller SYMBOL_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;

         mrsh.marshal(((Symbol)obj).getName(), null, "name", STRING_MSH, MF_LITERAL);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Symbol";
      }
   };

   /**
    * XMLElement marshaller.
    */
   protected final static Marshaller XML_ELEMENT_MSH = new ElementMarshaller(null, null)
   {
      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementMarshaller#getElementType(java.lang.Object)
       */
      protected QName getElementType(Object obj)
      {
         return ((XMLElement)obj).getElement();
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.ObjectMarshaller#marshalAttributes(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         XMLElement element = (XMLElement)obj;

         obj = element.getObject();

         if (obj == null)
         {
            mrsh.writeAttribute(XML.XSI_NS, "nil", true); // required if XMLElement holds a null

            return;
         }

         Marshaller marshaller = mrsh.findMarshaller(obj, element.getType()); // suppress xsi:type

         if (marshaller == null)
         {
            marshaller = mrsh.findMarshaller(obj);
            mrsh.writeAttribute(
               XML.XSI_NS, "type", marshaller.getNamespace(), marshaller.getType(), null);
         }

         marshaller.marshalAttributes(obj, msh);
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementMarshaller#marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         if (obj != null) // omit body if XMLElement holds a null
         {
            super.marshalContents(((XMLElement)obj).getObject(), msh);
         }
      }
   };

   /**
    * XMLElementList marshaller.
    */
   protected final static Marshaller XML_ELEMENT_LIST_MSH = new ElementListMarshaller(null)
   {
      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementListMarshaller#getElement(java.lang.Object)
       */
      public String getElement(Object obj)
      {
         return ((XMLElementList)obj).getElement().getLocalPart();
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementListMarshaller#getNamespace(java.lang.Object)
       */
      public String getNamespace(Object obj)
      {
         return ((XMLElementList)obj).getElement().getNamespaceURI();
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementListMarshaller#marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         XMLElementList element = (XMLElementList)obj;

         obj = element.getObject();

         String sPrefix = mrsh.getNamespacePrefix(element.getElement().getNamespaceURI());
         Marshaller marshaller = mrsh.findMarshaller(obj, element.getType()); // suppress xsi:type

         mrsh.marshalInline(obj, sPrefix, m_sElement, marshaller, MF_NIL);
      }
   };

   /**
    * XMLCollection marshaller with support for type restriction.
    * i.e. Marshaller for metaclass collections that are restricted to a certain type
    * (i.e. don't support polymorhism).
    */
   protected final static Marshaller XML_RESTRICTED_COLLECTION_MSH = new ElementListMarshaller(null)
   {
      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementListMarshaller#getElement(java.lang.Object)
       */
      public String getElement(Object obj)
      {
         XMLCollection response = (XMLCollection)obj;
         String sElement = XML.BASE_PREFIX;

         if (response.getMetaclass() != null)
         {
            sElement += response.getMetaclass().getName() + '-';
         }

         sElement += (response.getType() == null) ? "Collection" : response.getType();

         return sElement;
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.ElementListMarshaller#marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         if (obj == null)
         {
            return;
         }

         XMLMarshaller mrsh = (XMLMarshaller)msh;
         XMLCollection collection = (XMLCollection)obj;
         Metaclass restriction = collection.getMetaclass();
         Iterator iterator = collection.getCollection().iterator();

         // marshal as an unrestricted list
         if (restriction == null)
         {
            mrsh.marshalInline(iterator, m_sElementNS, m_sElement, null, MF_NIL);

            return;
         }

         boolean bDynamicTypeEnabled = mrsh.m_bDynamicTypeEnabled; // remember original value

         mrsh.m_bDynamicTypeEnabled = false;

         // go through all elements and see if can marshal them as the restricted metatype
         // (or null otherwise)
         while (iterator.hasNext())
         {
            Object tmpObj = iterator.next();
            String sClassName = (tmpObj instanceof TransferObject)
                              ? ((TransferObject)tmpObj).getClassName() : null;

            // Check if 'meta' derives from 'restriction'
            if (sClassName == null ||
                !restriction.isUpcast(mrsh.m_metadata.findMetaclass(sClassName)))
            {
               mrsh.marshal(null, m_sElementNS, m_sElement, null, MF_NIL); // not a supported class
            }
            else // select a marshaller with the 'restriction' in mind i.e. at most 'restriction'
            {
               Marshaller marshaller = mrsh.findMetaclassMarshaller(restriction.getName());

               mrsh.marshal(tmpObj, m_sElementNS, m_sElement, marshaller, MF_NIL);
            }
         }

         mrsh.m_bDynamicTypeEnabled = bDynamicTypeEnabled;
      }
   };

   /**
    * TransferObject marshaller.
    * The value for bDynamicMetaclassTypes doesn't matter since TransferObject has no attributes.
    */
   protected final static Marshaller TRANSFER_OBJECT_MSH = new MetaclassMarshaller(null, true);

   // declare after the rest as it needs TRANSFER_OBJECT_UNMSH declared

   /**
    * Request marshaller used by jUnit tests.
    */
   protected final static Marshaller REQUEST_MSH = new ObjectMarshaller()
   {
      // Request.Invocation marshaller
      private final Marshaller INVOCATION_MSH = new ObjectMarshaller()
      {
         public String getType()
         {
            return XML.BASE_PREFIX + "Invocation";
         }

         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            XMLMarshaller mrsh = (XMLMarshaller)msh;
            Request.Invocation invocation = (Request.Invocation)obj;

            mrsh.marshal(invocation.getObject(), XML.TNS_NS, "object", null, MF_NIL);
            mrsh.marshal(invocation.getEventName(), XML.TNS_NS, "event", STRING_MSH, 0);
            mrsh.marshalInline(invocation.getArguments(), XML.TNS_NS, "arg", null, MF_NIL);
            mrsh.marshal(invocation.getAttributes(), XML.TNS_NS, "attributes", PAIR_MSH, 0);
         }
      };

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         final Request req = (Request)obj;

         mrsh.marshal(req.getNamespace(), XML.TNS_NS, "namespace", STRING_MSH, MF_LITERAL);
         mrsh.marshal(req.getVersion(), XML.TNS_NS, "version", STRING_MSH, MF_LITERAL);
         mrsh.marshal(Boolean.valueOf(req.isAsync()), XML.TNS_NS, "async", BOOLEAN_MSH, MF_LITERAL);
         mrsh.marshal(Boolean.valueOf(req.isCommit()), XML.TNS_NS, "commit", BOOLEAN_MSH, MF_LITERAL);
         mrsh.marshal(req.getLocale(), XML.TNS_NS, "locale", LOCALE_MSH, MF_LITERAL);
         mrsh.marshal(req.getTimeZone(), XML.TNS_NS, "timeZone", TIME_ZONE_MSH, MF_LITERAL);
         mrsh.marshal(req.getCorrelator(), XML.TNS_NS, "correlator", TRANSFER_OBJECT_MSH, 0);
         mrsh.marshalInline(
            req.getInvocationIterator(), XML.TNS_NS, "invocation", INVOCATION_MSH, MF_NIL);
         mrsh.marshalInline(req.getFilterIterator(), XML.TNS_NS, "filters", TRANSFER_OBJECT_MSH, MF_NIL);
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Request";
      }
   };

   /**
    * Response marshaller.
    */
   protected final static Marshaller RESPONSE_MSH = new ObjectMarshaller()
   {
      protected final Marshaller EVENTS_MSH = new ElementListMarshaller(
            new ElementListMarshaller(null, XML.TNS_NS, "item"), XML.TNS_NS, "item");

      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         Response resp = (Response)obj;

         mrsh.marshalInline(resp.getResultIterator(), XML.TNS_NS, "results", null, MF_NIL);

         if (resp.getEventCount() != 0)
         {
            mrsh.marshal(resp.getEventIterator(), XML.TNS_NS, "events", EVENTS_MSH, MF_NIL);
         }
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Response";
      }
   };

   /**
    * Null response marshaller.
    */
   protected final static Marshaller NIL_MSH = new ObjectMarshaller()
   {
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
      }

      public String getType()
      {
         return XML.BASE_PREFIX + "Nil";
      }
   };

   /**
    * Map of class objects to marshallers: Marshaller[Class].
    */
   protected final static Lookup s_marshallerMap = new HashTab(64);

   static
   {
      // can't use OBJECT_LIST_MSH because MS.NET can only unmarshal lists as Object[],
      // it will fail to unmarshal any type other than "ArrayOfAnyType"
      s_marshallerMap.put(ArrayList.class, OBJECT_ARRAY_MSH);
      s_marshallerMap.put(char[].class, CHAR_ARRAY_MSH);
      s_marshallerMap.put(Object[].class, OBJECT_ARRAY_MSH);
      s_marshallerMap.put(OID.class, OID_MSH);
      s_marshallerMap.put(Pair.class, PAIR_MSH);
      s_marshallerMap.put(PCodeFunction.class, FUNCTION_MSH);
      s_marshallerMap.put(PCodeMacro.class, MACRO_MSH);
      s_marshallerMap.put(PrivilegeSet.class, PRIVILEGE_SET_MSH);
      s_marshallerMap.put(byte[].class, BYTE_VECTOR_MSH);
      s_marshallerMap.put(Request.class, REQUEST_MSH);
      s_marshallerMap.put(Response.class, RESPONSE_MSH);
      s_marshallerMap.put(SOAPFault.class, SOAP_FAULT_MSH);
      s_marshallerMap.put(Symbol.class, SYMBOL_MSH);
      s_marshallerMap.put(String[].class, new ElementListMarshaller(STRING_MSH));
      s_marshallerMap.put(TransferObject.class, TRANSFER_OBJECT_MSH);
      s_marshallerMap.put(XMLCollection.class, XML_RESTRICTED_COLLECTION_MSH);
      s_marshallerMap.put(XMLElement.class, XML_ELEMENT_MSH);
      s_marshallerMap.put(XMLElementList.class, XML_ELEMENT_LIST_MSH);
   }

   // attributes

   /**
    * ID counter for untyped marshalled objects.
    */
   protected int m_nNextMarshalledObjectId;

   /**
    * If dynamic marshaller type detection should be enabled or STRING_MSH used instead.
    */
   protected boolean m_bDynamicTypeEnabled;

   /**
    * Preserve element namespace prefix definitions after close of element, to be reused by next
    * element.
    */
   protected boolean m_bPreserveNS;

    /**
    * The default namespace URI for current XML element level.
    */
   protected String m_sDefaultNS;

   /**
    * Map containing currently defined namespace prefix for a given namespace URI key.
    * Default map size of 4 == number of elements added in serialize(...).
    * Any key instanceof String is a namespace URI to namespace prefix mapping.
    * Any key instanceof Integer is delimiter and contains default namespace defined for parent.
    * Newest definitions always at the front since no reverseIterator available for LookupDeque.
    * Upon openElement all namespaces from start of map up to the first Integer key will be defined.
    * Upon closeElement a new Integer will be added with the default namespace for this element.
    * Upon closeEmptyElement/endElement all namespaces from start of map up to but excluding the
    * second Integer key will be removed, with the value of the first Integer key set as default
    * namespace.
    * e.g. post openElement {0->"", URI0->"ns", 1->URI0, URI1->"", URI2->"q0" ... URIx->"qY"}
    * e.g. post closeElement/endElement
    *      {0->"", URI0->"ns", 1->URI0, URI1->"", URI2->"q0" ... URIj->"qK", N->URI1}
    * Note: Redefining default namespace between openElement and closeElement may cause the current
    *       element prefix to be invalid in the generated XML.
    */
   protected LookupDeque/*<Object, String>*/ m_nsMap = new LinkedHashTab/*<Object, String>*/(4);

   // associations

   /**
    * Map of Object (Metaclass) name Marshaller (String->Marshaller).
    * Used to store metadata marshallers instantiated based on current invocation context.
    */
   protected final Lookup/*<String, Marshaller>*/ m_marshallerMap = new HashTab();
   
   /**
    * The metadata collection to query for descriptions.
    */
   protected Metadata m_metadata;

   // constructors

   /**
    * Constructs the marshaller.
    * @param context The invocation context.
    * @param metadata The root metadata object for type information.
    */
   public XMLMarshaller(Context context, Metadata metadata)
   {
      super(context);
      m_metadata = metadata;
   }

   /**
    * Constructs the marshaller.
    * @param context The invocation context.
    */
   public XMLMarshaller(InvocationContext context)
   {
      this(context, context.getMetadata());
   }

   /**
    * Constructs the marshaller.
    * @param context The invocation context.
    */
   public XMLMarshaller(Context context)
   {
      this((InvocationContext)context);
   }

   // operations

   /**
    * Close an XML element header tag without closing the actual XML element.
    * @throws IOException  On output error.
    */
   protected void closeElement() throws IOException
   {
      m_nsMap.putFirst(Primitive.createInteger(m_nsMap.size()), m_sDefaultNS);
      m_writer.closeElement();
   }

   /**
    * Close an XML element without a defined body.
    * @throws IOException On output error.
    */
   protected void closeEmptyElement() throws IOException
   {
      popNamespacePrefix();
      m_writer.closeEmptyElement();
   }

   /**
    * End an XML element.
    * @param sPrefix The element prefix.
    * @param sElement The name of the element.
    * @throws IOException On output error.
    */
   protected void endElement(String sPrefix, String sElement) throws IOException
   {
      // close of parent element after NS preserve specified in child
      if (m_nsMap.firstKey() instanceof String)
      {
         boolean bPreserveNS = m_bPreserveNS;

         bPreserveNS = false;
         popNamespacePrefix();
         m_bPreserveNS = bPreserveNS;
      }

      m_nsMap.removeFirst(); // first element is delimiter for current
      popNamespacePrefix();

      //second condition is SOAP backwards compatibility kluge since SOAP assumes default XML.TNS_NS
      if (sPrefix == XMLConstants.DEFAULT_NS_PREFIX || sPrefix == XML.TNS_NS)
      {
         sPrefix = null;
      }

      m_writer.setNamespace(sPrefix);
         m_writer.endElement(sElement);
      m_writer.setNamespace(null);
   }

   /**
    * Gets a marshaller for an object.
    * @param obj The object for which to get the marshaller. Cannot be null.
    * @return The marshaller to use for the specified object.
    * @throws SOAPMarshallerException If a marshaller cannot be found.
    */
   protected Marshaller findMarshaller(Object obj) throws SOAPMarshallerException
   {
      Marshaller msh = null;

      if (obj instanceof TransferObject)
      {
         msh = findMetaclassMarshaller(((TransferObject)obj).getClassName());
      }

      return (msh != null) ? msh : XMLMarshaller.getMarshaller(obj);
   }

   /**
    * Get the marshaller for the object only if the object is of the expected type.
    * @param obj The object for which to get the marshaller.
    * @param type The expected object type to get the marshaller for.
    * @return The marshaller for the type or null if type does not match object's type.
    * @throws SOAPMarshallerException If a marshaller cannot be found for type.
    */
   protected Marshaller findMarshaller(Object obj, Type type)
   {
      // expected type equal to type of obj
      if ((type instanceof Primitive && type != Primitive.ANY && type == Primitive.typeOf(obj)) ||
          (type instanceof Metaclass &&
           obj instanceof TransferObject &&
           type.getName().equals(((TransferObject)obj).getClassName())))
      {
         return findMarshaller(obj);
      }

      return null;
   }

   /**
    * Get a marshaller for a specific metaclass
    * @param sType The type of the marshaller to get. Can be null.
    * @return The marshaller or null if not found
    */
   protected Marshaller findMetaclassMarshaller(String sType)
   {
      // check with locally registered marshallers (invocation context sensitive)
      Marshaller msh = (sType == null) ? null : (Marshaller)m_marshallerMap.get(sType); 
      
      if (msh != null || sType == null) // getMetaclass() does not like null type
      {
         return msh;
      }

      Metaclass metaclass = m_metadata.findMetaclass(sType);

      if (metaclass != null)
      {
         msh = new MetaclassMarshaller(metaclass, m_bDynamicTypeEnabled);
         m_marshallerMap.put(sType, msh); // put it in map so can reuse same unmarshaller later
      }

      return msh;
   }

   /**
    * Gets a marshaller for an object.
    * @param obj The object for which to get the marshaller. Cannot be null.
    * @return The marshaller to use for the specified object.
    * @throws SOAPMarshallerException If a marshaller cannot be found.
    */
   protected static Marshaller getMarshaller(Object obj) throws SOAPMarshallerException
   {
      Marshaller msh = (obj == null) ? null : (Marshaller)s_marshallerMap.get(obj.getClass());

      if (msh == null && obj instanceof Throwable)
      {
         return EXCEPTION_MSH;
      }

      return (msh != null) ? msh : SOAPMarshaller.getMarshaller(obj);
   }

   /**
    * Gets a marshaller for a specific java.lang.Class.
    * @param type The class to retrieve the object for.
    * @return The marshaller to use for a specific object or null if not found.
    */
   protected static Marshaller getMarshallerByClass(Class type)
   {
      // Lookup cannot handle null keys
      Marshaller msh = (type == null) ? null : (Marshaller)s_marshallerMap.get(type); 

      if (msh == null && Throwable.class.isAssignableFrom(type))
      {
         return EXCEPTION_MSH;
      }

      // check with parent class
      return (msh != null) ? msh : (Marshaller)SOAPMarshaller.s_mshMap.get(type);
   }

   /**
    * Generate or return unique prefix for an XML namespace.
    * @param sURI The XML namespace URI (not null).
    * @return Prefix that will be valid for the URI on next openElement(String, String) call.
    */
   protected String getNamespacePrefix(String sURI)
   {
      assert sURI != null;

      if (sURI == m_sDefaultNS || sURI.equals(m_sDefaultNS)) // first condition is optimization
      {
         return XMLConstants.DEFAULT_NS_PREFIX;
      }

      String sPrefix = (String)m_nsMap.get(sURI);

      if (sPrefix == null)
      {
         // use m_nsMap.size() as reasonable approximation of upper bound of possible NS definitions
         sPrefix = "q" + m_nsMap.size(); // 'q' for qualified type/name
         m_nsMap.putFirst(sURI, sPrefix);
      }

      return sPrefix;
   }

   /**
    * Generate or return unique ID for an object
    * (same object will always get same ID no matter how many times it's asked for).
    * @param obj The object for which to find the ID.
    * @return ID for the object.
    */
   protected String getObjectId(Object obj)
   {
      return (String)m_idMap.get(obj);
   }

   /**
    * @see nexj.core.rpc.soap.SOAPMarshaller#marshal(char, java.lang.String, java.lang.String)
    */
   protected void marshal(char ch, String sNamespace, String sElement) throws IOException
   {
      startElement(sNamespace, sElement);
         m_writer.writeInt(ch);
      endElement(sNamespace, sElement);
   }

   /**
    * @see nexj.core.rpc.soap.SOAPMarshaller#marshal(int, java.lang.String, java.lang.String)
    */
   protected void marshal(int n, String sNamespace, String sElement) throws IOException
   {
      startElement(sNamespace, sElement);
         m_writer.writeInt(n);
      endElement(sNamespace, sElement);
   }

   /**
    * @see nexj.core.rpc.soap.SOAPMarshaller#marshal(long, java.lang.String, java.lang.String)
    */
   protected void marshal(long l, String sNamespace, String sElement) throws IOException
   {
      startElement(sNamespace, sElement);
         m_writer.write(Long.toString(l));
      endElement(sNamespace, sElement);
   }

   /**
    * Marshals an object as xsd:anyType.
    * @param obj The object to marshal. Can be null.
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
            openElement(sNamespace, sElement);
               writeAttribute(XML.XSI_NS, "nil", true);
            closeEmptyElement();
         }

         return;
      }

      // force String type when marshalling without dynamic marshaller detection
      // (no need to output type because this is explicitly expected)
      if (msh == null && !m_bDynamicTypeEnabled)
      {  
         msh = STRING_MSH;
         obj = obj.toString();
      }

      Marshaller marshaller = (msh != null) ? msh : findMarshaller(obj); // actual Marshaller to use

      // non-primitive marshallers should not output same objects
      // primitive marshallers do some strange things that cause this algorithm to break anyways
      // e.g. PrimitiveSet->mask
      if (!marshaller.isPrimitive())
      {
         String sId = getObjectId(obj);

         // don't output object again if it was already output once
         if (sId != null)
         {
            if (marshaller instanceof MetaclassMarshaller)
            {
               // MetaclassMarshallers know how to do shallow copies of themselves
               marshaller = ((MetaclassMarshaller)marshaller).REF_MARSHALLER;
            }
            else
            {
               IOException e = new IOException("Circular references are not supported");

               e.initCause(new SOAPMarshallerException("err.rpc.xml.circularReference",
                                                       new Object[]{obj}));

               throw e;
            }
         }
      }

      String sId = null;

      openElement(sNamespace, sElement);

         // force type specification if a dynamic lookup was done for a non-collection
         if ((nFlags & MF_TYPE) != 0 || msh == null)
         {
            writeAttribute(XML.XSI_NS, "type", marshaller.getNamespace(), marshaller.getType(), null);
         }

         // mark/track untyped objects with a unique ID
         if (!marshaller.isPrimitive())
         {
            sId = toId(m_nNextMarshalledObjectId++);
            m_idMap.put(obj, sId);
         }

         marshaller.marshalAttributes(obj, this);
      closeElement();
         marshaller.marshalContents(obj, this);
      endElement(sNamespace, sElement);

      // remove object from recursion tracking as we're done with this branch
      // (used to leave one object copy per branch)
      if (sId != null)
      {
         m_idMap.remove(obj);
      }
   }

   /**
    * If value is actually a collection of individual values then marshal each individual
    * separately, otherwise marshal as a single object.
    * @param obj The object to marshal.
    * @param sPrefix The prefix of the element to marshal into (not null).
    * @param sElement The element name to marshal into (not null).
    * @param marshaller The marshaller to marshal the object with (null == dynamic).
    * @param nFlags Combination of MF_* constants to set marshaling options.
    * @throws IOException On output IO error.
    */
   protected void marshalInline(
      Object obj, String sPrefix, String sElement, Marshaller marshaller, int nFlags)
      throws IOException
   {
      if (obj == null)
      {
         marshal(obj, sPrefix, sElement, marshaller, MF_NIL);

         return;
      }

      if (obj instanceof Iterable)
      {
         obj = ((Iterable)obj).iterator();
      }
      else if (obj instanceof Iteratable)
      {
         obj = ((Iteratable)obj).iterator();
      }

      boolean bPreserveNS = m_bPreserveNS;

      m_bPreserveNS = true;

      if (obj instanceof Iterator)
      {
         for (Iterator itr = (Iterator)obj; itr.hasNext();)
         {
            marshal(itr.next(), sPrefix, sElement, marshaller, nFlags);
         }
      }
      else if (obj instanceof Object[])
      {
         Object[] array = (Object[])obj;

         for (int i = 0; i < array.length; ++i)
         {
            marshal(array[i], sPrefix, sElement, marshaller, nFlags);
         }
      }
      else if (obj instanceof char[])
      {
         char[] chArray = (char[])obj;

         for (int i = 0; i < chArray.length; ++i)
         {
            marshal(chArray[i], sPrefix, sElement);
         }
      }
      else
      {
         marshal(obj, sPrefix, sElement, marshaller, nFlags);
      }

      m_bPreserveNS = bPreserveNS;
      popNamespacePrefix();
   }

   /**
    * Open an XML element and output initial attributes.
    * @param sPrefix The element prefix.
    * @param sElement The name of the element.
    * @throws IOException On output error.
    */
   protected void openElement(String sPrefix, String sElement) throws IOException
   {
      //second condition is SOAP backwards compatibility kluge since SOAP assumes default XML.TNS_NS
      if (sPrefix == XMLConstants.DEFAULT_NS_PREFIX || sPrefix == XML.TNS_NS)
      {
         sPrefix = null;
      }

      m_writer.setNamespace(sPrefix);
         m_writer.openElement(sElement);
      m_writer.setNamespace(null);

      String sDefaultNS;

      if (m_nsMap.firstKey() instanceof String) // optimization to avoid creating Iterator
      {
         m_writer.setNamespace(XML.XML_NS);

            Lookup.Iterator/*<Object, String>*/ itr = m_nsMap.iterator();

            while (itr.hasNext() && itr.next() instanceof String)
            {
               m_writer.writeAttribute((String)itr.getValue(), (String)itr.getKey());
            }

         m_writer.setNamespace(null);

         sDefaultNS = (String)itr.getValue();
      }
      else
      {
         sDefaultNS = (String)m_nsMap.firstValue();
      }

      if (sDefaultNS != m_sDefaultNS) // default namespace has changed from last tag
      {
         m_writer.writeAttribute(XML.XML_NS, m_sDefaultNS); // output default namespace
      }
   }

   /**
    * Remove all namespace prefixes defined for the current element level.
    */
   protected void popNamespacePrefix()
   {
      if (!m_bPreserveNS)
      {
         while(m_nsMap.firstKey() instanceof String) // delimiter for current not in map
         {
            m_nsMap.removeFirst();
         }

         m_sDefaultNS = (String)m_nsMap.firstValue();
      }
   }

   /**
    * Start an XML element and output initial attributes.
    * @param sPrefix The element prefix.
    * @param sElement The name of the element.
    * @throws IOException On output error.
    */
   protected void startElement(String sPrefix, String sElement) throws IOException
   {
      openElement(sPrefix, sElement);
      closeElement();
   }

   /**
    * @see nexj.core.rpc.CharacterStreamMarshaller#serialize(java.lang.Object, java.io.Writer)
    */
   public void serialize(Object obj, Writer writer) throws IOException, MarshallerException
   {
      m_writer = (writer instanceof XMLWriter) ? (XMLWriter)writer : new XMLWriter(writer);
      m_idMap = new IdentityHashTab();
      m_nNextMarshalledObjectId = 0;
      m_bDynamicTypeEnabled = true;
      m_sDefaultNS = XML.NS_URI_TNS;

      // load all header attributes for first element
      m_nsMap.putFirst(Primitive.createInteger(m_nsMap.size()), XMLConstants.NULL_NS_URI);
      m_nsMap.putFirst(XML.XSD_URI, XML.XSD_NS);
      m_nsMap.putFirst(XML.XSI_URI, XML.XSI_NS);
      m_nsMap.putFirst(XML.NS_URI_TNS, XML.TNS_NS);

      int nFlags = MF_LITERAL;
      Marshaller msh;

      if (obj == null)
      {
         msh = NIL_MSH;
         nFlags |= MF_NIL;
      }
      else
      {
         msh = findMarshaller(obj);
      }

      String sPrefix;
      String sElement;

      if (msh instanceof TypedElementMarshaller)
      {
         TypedElementMarshaller mrsh = (TypedElementMarshaller)msh;

         sPrefix = getNamespacePrefix(mrsh.getNamespace(obj));
         sElement = mrsh.getElement(obj);
      }
      else
      {
         sPrefix = msh.getNamespace();
         sElement = msh.getType();
      }

      marshal(obj, sPrefix, sElement, msh, nFlags);
   }

   /**
    * Output an attribute.
    * @param sPrefix The element prefix.
    * @param sName The element name.
    * @param bValue The value.
    * @throws IOException On output error.
    */
   protected void writeAttribute(String sPrefix, String sName, boolean bValue) throws IOException
   {
      m_writer.setNamespace((sPrefix != null && sPrefix != XML.TNS_NS) ? sPrefix : null);
         m_writer.writeAttribute(sName, bValue);
      m_writer.setNamespace(null);
   }

   /**
    * Output an attribute.
    * @param sPrefix The element prefix.
    * @param sName The element name.
    * @param sValuePrefix The value prefix.
    * @param sValue0 The first part of the value (null == skip).
    * @param sValue1 The second part of the value (null == skip).
    * @throws IOException On output error.
    */
   protected void writeAttribute(String sPrefix, String sName, String sValuePrefix, String sValue0, String sValue1)
      throws IOException
   {
      m_writer.setNamespace((sPrefix != null && sPrefix != XML.TNS_NS) ? sPrefix : null);

         if (sValuePrefix != null && sValuePrefix != XML.TNS_NS)
         {
            m_writer.writeAttribute(sName, sValuePrefix, ":", sValue0, sValue1);
         }
         else
         {
            m_writer.writeAttribute(sName, sValue0, sValue1);
         }

      m_writer.setNamespace(null);
   }

   // inner classes

   /**
    * Marshaller for lists producing a "values" tag for each element.
    * The object to marshal must either be an Iterator, Iterable or Object[].
    * This class will not work from primitive arrays
    * (override marshalContents(Object, SOAPMarshaller) for that).
    */
   protected static class ElementListMarshaller
      extends ObjectMarshaller implements TypedElementMarshaller
   {
      // attributes

      /**
       * The collection type to use.
       */
      protected String m_sType;

      /**
       * The tag namespace to use for sub elements.
       */
      protected String m_sElementNS;
      
      /**
       * The tag to use for sub elements.
       */
      protected String m_sElement;

      // associations

      /**
       * The marshaller to use for individual elements.
       */
      protected Marshaller m_marshaller;

      // constructors
      
      /**
       * Constructor.
       * @param marshaller The marshaller to use (null means dynamic).
       * @param sType The marshaller type name (null for default). 
       * @param sElementNS The namespace (tag not URI) of sElement to use
       *                   (if sElement then default will be used).
       * @param sElement The tag to use for sub elements (null for default).
       */
      public ElementListMarshaller(Marshaller marshaller, String sType, String sElementNS, String sElement)
      {         
         m_marshaller = marshaller;

         if (sType == null)
         {
            if (marshaller != null)
            {
               sType = marshaller.getType() + XML.ARRAY_SUFFIX;
            }
            else
            {
               sType = XML.BASE_PREFIX + "Collection";
            }
         }

         m_sType = sType;
         m_sElementNS = (sElementNS == null) ?  XML.TNS_NS : sElementNS;
         m_sElement = (sElement == null) ? "item" : sElement;
      }

      /**
       * Constructor.
       * @param marshaller The marshaller to use (null means dynamic).
       * @param sElementNS The namespace (tag not URI) of sElement to use
       *                   (if sElement then default will be used).
       * @param sElement The tag to use for sub elements (null for default).
       */
      public ElementListMarshaller(Marshaller marshaller, String sElementNS, String sElement)
      {
         this(marshaller, null, sElementNS, sElement);
      }

      /**
       * Convenience constructor providing default values.
       * @param marshaller The marshaller to use (null means dynamic).
       */
      public ElementListMarshaller(Marshaller marshaller)
      {
         this(marshaller, null, null, null);
      }

      // operations

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.TypedElementMarshaller#getElement(java.lang.Object)
       */
      public String getElement(Object obj)
      {
         return getType();
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.TypedElementMarshaller#getNamespace(java.lang.Object)
       */
      public String getNamespace(Object obj)
      {
         return XML.NS_URI_TNS;
      }
      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller#getType()
       */
      public String getType()
      {
         return m_sType;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller
       *      #marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         ((XMLMarshaller)msh).marshalInline(obj, m_sElementNS, m_sElement, m_marshaller, MF_NIL);
      }
   }

   /**
    * Marshaller for individual objects.
    */
   protected static class ElementMarshaller
      extends ObjectMarshaller implements TypedElementMarshaller
   {
      /**
       * Default element type.
       */
      protected static QName s_defaultType = WADLGenerator.computeElementType(Primitive.ANY, false);

      /**
       * The marshaller to use for individual elements.
       */
      protected Marshaller m_marshaller;

      /**
       * The element/tag type to use.
       */
      protected QName m_type;

      /**
       * Constructor.
       * @param marshaller The marshaller to use (null means dynamic).
       * @param type The marshaller element/tag type (null for dynamic).
       */
      public ElementMarshaller(Marshaller marshaller, QName type)
      {
         m_marshaller = marshaller;
         m_type = type;
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.TypedElementMarshaller#getElement(java.lang.Object)
       */
      public String getElement(Object obj)
      {
         return getElementType(obj).getLocalPart();
      }

      /**
       * @see nexj.core.rpc.xml.XMLMarshaller.TypedElementMarshaller#getNamespace(java.lang.Object)
       */
      public String getNamespace(Object obj)
      {
         return getElementType(obj).getNamespaceURI();
      }

      /**
       * Return the qualified type of the element representing the marshalled object.
       * @param The object to be marshalled that requires a namespace URI.
       * @return The element/tag qualified type to use when marshalling this object.
       */
      protected QName getElementType(Object obj)
      {
         if (m_type != null)
         {
            return m_type;
         }

         return (obj instanceof Type)
                ? WADLGenerator.computeElementType((Type)obj, false) : s_defaultType;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller#getType()
       */
      public String getType()
      {
         return getElement(null);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.ObjectMarshaller#marshalAttributes(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalAttributes(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         Marshaller marshaller = m_marshaller;

         if (marshaller == null)
         {
            marshaller = mrsh.findMarshaller(obj);
            mrsh.writeAttribute(
               XML.XSI_NS, "type", marshaller.getNamespace(), marshaller.getType(), null);
         }

         marshaller.marshalAttributes(obj, msh);
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller#marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         Marshaller marshaller = (m_marshaller == null) ? mrsh.findMarshaller(obj) : m_marshaller;

         marshaller.marshalContents(obj, mrsh);
      }
   }

   /**
    * Class to marshal TransferObjects into metaclass instances. 
    */
   protected static class MetaclassMarshaller extends ObjectMarshaller
   {
      /**
       * The type of this object.
       */
      protected String m_sType = XML.BASE_PREFIX + "TransferObject";

      /**
       * A map of attributes to corrsponding marshallers.
       */
      protected Lookup/*<String, AttributeInfo>*/ m_attributeMap =
         new LinkedHashTab /*<String, AttributeInfo>*/();
      
      /**
       * The ref marshaller for this object instance (need instance so have access to m_sType).
       */
      public final Marshaller REF_MARSHALLER = new MetaclassRefMarshaller(); 

      /**
       * @param sName The name of the Metaclass to query.
       * @param meta The metaclass this object marshals for
       *             (any key/value pairs not in this class will go under _keys/_values).
       * @param bDynamicMetaclassTypes enable dynamic detection of attributes containing instances
       */
      public MetaclassMarshaller(Metaclass meta, boolean bDynamicMetaclassTypes)
      {
         if (meta == null)
         {
            // done with initialization
            // (this is valid for TransferObject without an existing Metaclass, e.g. jUnit tests)
            return;
         }

         m_sType = meta.getName();

         for (Iterator itr = meta.getAttributeIterator(); itr.hasNext();)
         {
            Attribute attribute = (Attribute)itr.next();

            if (attribute.getVisibility() != Metaclass.PUBLIC)
            {
               continue;
            }

            AttributeInfo attrInfo;

            if (attribute.getType().isPrimitive())
            {
               // can resolve primitive Marshallers immediately since this is not a primitive
               // Marshaller hence no danger of recursion loop
               attrInfo = (Primitive.ANY.equals(attribute.getType()))
                        ? new AttributeInfo(attribute, null, true)
                        : new AttributeInfo(attribute,
                                            getMarshallerByClass(((Primitive)attribute.getType())
                                                                    .getClassObject()),
                                            false);
            }
            else if (attribute.getType() instanceof Metaclass)
            {
               // don't initialize marshaller to avoid recursive creation of self
               attrInfo = new AttributeInfo(attribute, null, bDynamicMetaclassTypes);
            }
            else
            {
               // need value to determine proper marshaller to use (assume it has a static type)
               attrInfo = new AttributeInfo(attribute, null, false);
            }

            m_attributeMap.put(attribute.getName(), attrInfo);
         }
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller#getType()
       */
      public String getType()
      {
         return m_sType;
      }

      /**
       * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller
       *      #marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
       */
      public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
      {
         XMLMarshaller mrsh = (XMLMarshaller)msh;
         TransferObject tobj = (TransferObject)obj;

         // only marshal the class if it's different from the class we are supposed to marshal
         if (m_sType == null || !m_sType.equals(tobj.getClassName()))
         {
            mrsh.marshal(tobj.getClassName(),
                         null,
                         XML.BASE_PREFIX + "class",
                         STRING_MSH,
                         MF_LITERAL);
         }

         mrsh.marshal(tobj.getEventName(),
                      null, XML.BASE_PREFIX + "event", STRING_MSH, MF_LITERAL);
         mrsh.marshal(tobj.getVersion(), null, XML.BASE_PREFIX + "version");

         OID oid = tobj.getOID();

         if (oid == null)
         {
            mrsh.marshal("@" + mrsh.getObjectId(obj),
                         null, XML.BASE_PREFIX + "oid", STRING_MSH, MF_LITERAL);
         }
         else
         {
            mrsh.marshal(oid, null, XML.BASE_PREFIX + "oid", OID_MSH, MF_LITERAL);
         }

         List valueList = null;

         for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
         {
            String sName = (String)itr.next();
            AttributeInfo attrInfo = (AttributeInfo)m_attributeMap.get(sName);

            // this is not attribute of this metaclass or value is null hence problem for MS.NET
            if (attrInfo == null || itr.getValue() == null)
            {
               mrsh.marshal(sName, XML.TNS_NS, XML.BASE_PREFIX + "keys", STRING_MSH, MF_NIL);

               if (valueList == null)
               {
                  valueList = new ArrayList();
               }

               valueList.add(itr.getValue());
            }
         }

         if (valueList != null)
         {
            for (int i = 0, n = valueList.size(); i < n; ++i)
            {
               mrsh.marshal(valueList.get(i), XML.TNS_NS, XML.BASE_PREFIX + "values", null, MF_NIL | MF_TYPE);
            }
         }

         for (Lookup.Iterator itr = m_attributeMap.valueIterator(); itr.hasNext();)
         {
            AttributeInfo attrInfo = (AttributeInfo)itr.next();
            Attribute attribute = attrInfo.m_attribute;

            if (attribute.getVisibility() != Metaclass.PUBLIC)
            {
               continue;
            }

            Object value = tobj.findValue(attribute.getName(), Undefined.VALUE);

            if (value == Undefined.VALUE)
            {
               continue;
            }

            Marshaller attrMsh = attrInfo.m_marshaller; // get cached marshaller

            // if this is the first time through marshaller then initialize metaclass marshaller
            if (attrMsh == null)
            {
               if (attribute.getType() instanceof Metaclass)
               {
                  // determine proper marshaller based on attribute to guarantee valid type output
                  attrMsh = mrsh.findMetaclassMarshaller(attribute.getType().getName());
               }
               else if (!attrInfo.m_bDynamicMarshaller && value != null)
               {
                  // might is reachable for non-Metaclass attributes e.g. Throwable
                  attrMsh = mrsh.findMarshaller(value);
               }

               if (attrMsh == null && !attrInfo.m_bDynamicMarshaller && value != null)
               {
                  throw new SOAPMarshallerException("err.rpc.mshType",
                                                    new Object[]{attribute.getType().getName()});
               }

               // cache the marshaller for future reference
               if (attrMsh != null)
               {
                  synchronized(this)
                  {
                     // guarantee all instances will use same marshaller
                     if (attrInfo.m_marshaller == null)
                     {  // store for future lookups
                        attrInfo.m_marshaller = attrMsh;
                     }
                     else
                     {
                        attrMsh = attrInfo.m_marshaller;
                     }
                  }
               }
            }

            // if this is a dynamic marshaller for a metaclass that is not the same as attribute
            if (attrInfo.m_bDynamicMarshaller &&
                attrMsh instanceof MetaclassMarshaller &&
                value instanceof TransferObject &&
                ((TransferObject)value).getClassName() != null && // if unspecified then == default
                !((TransferObject)value).getClassName().equals(attribute.getType().getName()))
            {
               attrMsh = null;
            }

            if (value != null) // do not marshal null values since they will not work with MS.NET
            {
               if (attribute.isCollection())
               {
                  mrsh.marshalInline(((Iterable)value).iterator(), XML.TNS_NS, attrInfo.m_sName, attrMsh, MF_NIL);
               }
               else
               {
                  mrsh.marshal(value, XML.TNS_NS, attrInfo.m_sName, attrMsh, MF_NIL);
               }
            }
         }
      }

      /**
       * Marshaller for ref of this metaclass (non static)
       */
      protected class MetaclassRefMarshaller extends PrimitiveMarshaller
      {
         /**
          * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller#getType()
          */
         public String getType()
         {
            return m_sType;
         }

         /**
          * @see nexj.core.rpc.soap.SOAPMarshaller.PrimitiveMarshaller#getNamespace()
          */
         public String getNamespace()
         {
            return XML.TNS_NS;
         }

         /**
          * @see nexj.core.rpc.soap.SOAPMarshaller.Marshaller#marshalContents(java.lang.Object, nexj.core.rpc.soap.SOAPMarshaller)
          */
         public void marshalContents(Object obj, SOAPMarshaller msh) throws IOException
         {
            XMLMarshaller mrsh = (XMLMarshaller)msh; 
            TransferObject tobj = (TransferObject)obj;
            OID oid = tobj.getOID();

            mrsh.marshal(tobj.getClassName(), null, XML.BASE_PREFIX + "class", STRING_MSH, MF_LITERAL);

            mrsh.marshal((oid != null) ? (Object)oid : ("@" + mrsh.getObjectId(obj)), null,
               XML.BASE_PREFIX + "oid", (oid != null) ? OID_MSH : STRING_MSH, MF_LITERAL);
         }
      }

      /**
       * Bean storing attribute information.
       */
      protected static class AttributeInfo
      {
         /**
          * Name of the tag to use.
          */
         public String m_sName;

         /**
          * The attribute in being referenced
          * (final because m_sName value generation depends on it).
          */
         public final Attribute m_attribute;

         /**
          * If the marshaller should be dynamically determined when its type differs from attribute
          */
         public boolean m_bDynamicMarshaller;

         /**
          * The marshaller being used for the attribute.
          */
         public Marshaller m_marshaller;
         
         /**
          * Convinience constructor.
          * @param attribute The attribute being referenced.
          * @param marshaller The Marshaller for the attribute.
          * @param bDynamicMarshaller Determine marshaller dynamically if it doesn't exactly match.
          */
         public AttributeInfo(Attribute attribute, Marshaller marshaller, boolean bDynamicMarshaller)
         {
            m_attribute = attribute;
            m_bDynamicMarshaller = bDynamicMarshaller;
            m_marshaller = marshaller;
            m_sName = XSDGenerator.computeElementName(m_attribute);
         }

         /**
          * @see java.lang.Object#toString()
          */
         public String toString()
         {
            return m_sName + " = (" + m_attribute + ") => (" + m_marshaller + ")";
         }
      }
   }

   /**
    * Interface to distinguish marshallers capable of providing customized element type information.
    */
   protected static interface TypedElementMarshaller extends Marshaller
   {
      /**
       * Return the Namespace URI for the element representing the marshalled object.
       * @param The object to be marshalled that requires a namespace URI.
       * @return The XML namespace URI to use when marshalling this object.
       */
      public String getNamespace(Object obj);

      /**
       * Return the element name for the element representing the marshalled object.
       * @param The object to be marshalled that requires an element name.
       * @return The XML untyped element name to use when marshalling this object.
       */
      public String getElement(Object obj);
   }
}