// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.xml.namespace.QName;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Selector;
import nexj.core.meta.Type;
import nexj.core.meta.TypeConversionException;
import nexj.core.persistence.OID;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.RequestException;
import nexj.core.rpc.Server;
import nexj.core.rpc.ServerException;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.http.GenericHTTPServer;
import nexj.core.rpc.soap.SOAPFault;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Parser;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.ErrorCode;
import nexj.core.util.HTTP;
import nexj.core.util.IndentingXMLWriter;
import nexj.core.util.IteratorEnumeration;
import nexj.core.util.StringUtil;

/**
 * Class for serving REST like requests.
 */
public class XMLHTTPServer extends GenericHTTPServer
{
   // constants
   
   /**
    * Supported SOAPAction headers
    */
   protected final static String SOAP_ACTION_BASIC = "Server#basic";
   protected final static String SOAP_ACTION_DEFAULT = "Server#invoke";
   
   /**
    * Standard XML/SOAP strings
    */
   protected final static String XML_HEADER = XML.HEADER;

   protected final static String SOAP_WRAP_PREFIX = "<Envelope xmlns=\"" + XML.ENV_URI +
      "\" encodingStyle=\"" + XML.ENC_URI + "\"><Body>";

   protected final static String SOAP_WRAP_SUFFIX = "</Body></Envelope>";

   // attributes

   /**
    * The content MIME type and encoding.
    */
   protected String m_sContentType = "text/xml; charset=UTF-8";

   /**
    * Should a SOAP envelope be used for responses.
    */
   protected boolean m_bWrapped;

   /**
    * The generic server component.
    */
   protected Component m_server;

   /**
    * The Scheme parser.
    */
   protected Parser m_parser;

   // operations
   
   /**
    * Sets the content MIME type and encoding.
    * @param sContentType The content MIME type and encoding to set.
    */
   public void setContentType(String sContentType)
   {
      m_sContentType = sContentType;
   }

   /**
    * @return The content MIME type and encoding.
    */
   public String getContentType()
   {
      return m_sContentType;
   }

   /**
    * Sets the generic server component.
    * @param server The generic server component to set.
    */
   public void setServer(Component server)
   {
      m_server = server;
   }

   /**
    * Sets if this server should output SOAP wrapped responses.
    * @param bWrapped Should the responses be wrapped in a soap envelope.
    */
   public void setWrapped(boolean bWrapped)
   {
      m_bWrapped = bWrapped;
   }

   /**
    * @return The response writer.
    */
   protected Writer getWriter() throws IOException
   {
      Writer writer = m_response.getWriter();

      if (getParameter("indent-xml") != null)
      {
         writer = new IndentingXMLWriter(writer);
      }

      return writer;
   }
   
   /**
    * @see nexj.core.rpc.http.GenericHTTPServer#invoke()
    */
   protected void invoke() throws ServletException, IOException
   {
      RPCUtil.checkPrivilege(m_context);

      // In case of error track if we need to open the envelope again
      boolean bEnvelopeOpen = false;

      try
      {
         String sContext = m_request.getPathInfo();

         if (StringUtil.isEmpty(sContext))
         {
            sContext = "/";
         }

         Metaclass metaclass = null;
         int nClassEndIndex = sContext.indexOf('/', 1);

         if (nClassEndIndex < 0)
         {
            nClassEndIndex = sContext.length();
         }

         if (nClassEndIndex > 2)
         {
            String sClassName = sContext.substring(1, nClassEndIndex);

            metaclass = m_context.getMetadata().findMetaclass(sClassName);

            if (metaclass == null)
            {
               throw new RequestException("err.rpc.xml.class", new Object[]{sClassName});
            }
         }

         sContext = (nClassEndIndex < sContext.length()) ? sContext.substring(nClassEndIndex + 1) : "";

         String sMethod = getMethod();

         if (metaclass == null && sMethod.equals(HTTP.METHOD_GET))
         {
            // Generate schema

            XSDGenerator generator;

            if (getParameter("xsd") != null)
            {
               generator = new XSDGenerator();
            }
            else if (m_bWrapped)
            {
               if (getParameter("basic") != null)
               {
                  generator = new WSDLBasicGenerator(SOAP_ACTION_BASIC);
               }
               else
               {
                  generator = new WSDLGenerator(SOAP_ACTION_DEFAULT);
               }
            }
            else
            {
               generator = new WADLGenerator();
            }

            generator.setCompatible(StringUtil.isEmpty(getParameter("compatible")) ||
                                    StringUtil.parseBoolean(getParameter("compatible")));
            generator.setIncludeDocumentation(getParameter("doc") != null);

            String sMask = getParameter("mask");

            if (sMask != null)
            {
               generator.setMask(sMask.split("\\s+"));
            }

            String sScope = getParameter("scope");

            if ("none".equals(sScope) || "generic".equals(sScope))
            {
               generator.setScope(XSDGenerator.SCOPE_GENERIC);
            }
            else if ("state".equals(sScope))
            {
               generator.setScope(XSDGenerator.SCOPE_STATE);
            }
            else if ("behavior".equals(sScope) || "behaviour".equals(sScope))
            {
               generator.setScope(XSDGenerator.SCOPE_BEHAVIOUR);
            }
            else if (sScope != null)
            {
               throw new IllegalArgumentException("Invalid SCOPE value.");
            }

            List metaclassList = Collections.list(new IteratorEnumeration(m_context.getMetadata().getMetaclassIterator()));

            Collections.sort(metaclassList, Metaclass.COMPARATOR);

            setResponseContentType(generator.getMIMEType());

            Writer writer = getWriter();

            writer.write(XML_HEADER);
            generator.generate(writer, metaclassList.iterator(), m_request.getRequestURL().toString());
         }
         else
         {
            // Invoke the generic server

            Object request;

            try
            {
               request = unmarshal(metaclass, sContext);
            }
            catch (Exception e)
            {
               if (e instanceof ClassCastException ||
                  e instanceof NumberFormatException ||
                  e instanceof TypeConversionException)
               {
                  throw new RequestException("err.rpc.xml.request", e);
               }

               throw e;
            }

            Server server = (Server)m_server.getInstance(m_context);
            Object response;

            if (request instanceof Invoker)
            {
               Invoker invoker = (Invoker)request;

               invoker.setLocale(m_request.getLocale());
               response = invoker.invoke(server);
            }
            else if (request instanceof Request)
            {
               response = server.invoke((Request)request);
            }
            else
            {
               throw new RequestException("err.rpc.xml.request");
            }

            if (sMethod.equals(HTTP.METHOD_DELETE))
            {
               return;
            }

            setResponseContentType(m_sContentType);

            Writer writer = getWriter();

            writer.write(XML_HEADER);

            if (m_bWrapped)
            {
               // begin SOAP envelope wrapping
               writer.write(SOAP_WRAP_PREFIX);
            }

            bEnvelopeOpen = true;

            new XMLMarshaller(m_context).serialize(response, writer);

            if (m_bWrapped)
            {
               // end SOAP envelope wrapping
               writer.write(SOAP_WRAP_SUFFIX);
            }
         }
      }
      catch (Throwable t)
      {
         try
         {
             // prepare and log exception (might not get a chance to log if resetBuffer() fails
            t = RPCUtil.handleException(t, RPCUtil.isSystem(t), "XML", m_logger);

            resetResponseBuffer();
            setResponseContentType(m_sContentType);

            if (t instanceof RequestException)
            {
               if ("err.rpc.xml.method".equals(((ErrorCode)t).getErrorCode()))
               {
                  m_response.setStatus(HTTP.STATUS_METHOD_NOT_ALLOWED);
               }
               else
               {
                  m_response.setStatus(HTTP.STATUS_BAD_REQUEST);
               }
            }
            else
            {
               m_response.setStatus(HTTP.STATUS_INTERNAL_SERVER_ERROR);
            }

            Writer writer = getWriter();
            XMLMarshaller marshaller = new XMLMarshaller(m_context);

            if (!bEnvelopeOpen)
            {
               writer.write(XML_HEADER);
            }

            if (!m_bWrapped)
            {
               // this will throw IllegalStateException if the response headers were already sent
               // out (causing invalid XML output)
               marshaller.serialize(t, writer);
            }
            else
            {  
               if (!bEnvelopeOpen)
               {
                  writer.write(SOAP_WRAP_PREFIX);
               }

               marshaller.serialize(new SOAPFault(t), writer);

               writer.write(SOAP_WRAP_SUFFIX);
            }
         }
         catch (RPCException e)
         {
            throw e;
         }
         catch (Throwable e)
         {
            throw new ServerException("err.rpc.xml.serialize", e);
         }
      }
   }

   /**
    * Process a SOAP/REST request.
    * @param metaclass The metaclass from the URL context. Can be null.
    * @param sContext The URL context after the class name (/ separated, OID hex encoded).
    * @return The unmarshalled request or null if no request available.
    * @throws IOException If an input error occurs.
    * @throws RequestException On unsupported request method for a specified metaclass.
    */
   protected Object unmarshal(Metaclass metaclass, String sContext) throws IOException, RequestException
   {
      XMLUnmarshaller unmarshaller = new XMLUnmarshaller(m_context);
      String sMethod = getMethod();
      OID oid = null;
      Event event = null;

      if (sContext.length() != 0)
      {
         int i = sContext.indexOf('/');
         String sOID;

         if (i < 0)
         {
            sOID = sContext;
            sContext = "";
         }
         else
         {
            sOID = sContext.substring(0, i);
            sContext = sContext.substring(i + 1);
         }

         if (HTTP.METHOD_POST.equals(sMethod)) // static event invocation could be part of sContext
         {
            event = findEvent(metaclass, sOID, true); // static event name in URI
         }

         if (event == null) // if first part of sContext is not event, then interpret it as OID
         {
            oid = OID.fromBinary(Binary.parse(sOID));

            if (sContext.length() > 0 && HTTP.METHOD_POST.equals(sMethod)) // non-static event
            {
               event = findEvent(metaclass, sContext, false); // non-static event name in URI
            }
            else if (sContext.length() == 0 && sMethod.startsWith(XML.BASE_PREFIX))
            {
               // non-static event name in HTTP method
               event = findEvent(metaclass, sMethod.substring(XML.BASE_PREFIX.length()), false);
            }
         }

         if (event == null && sContext.length() > 0)
         {
            throw createMethodException(); // unknown event
         }
      }
      else if (sMethod.startsWith(XML.BASE_PREFIX)) // static event name in HTTP method
      {
         event = findEvent(metaclass, sMethod.substring(XML.BASE_PREFIX.length()), true);
      }

      if (event != null)
      {
         return unmarshal(event, oid); // event invocation request
      }

      // read request
      if (sMethod.equals(HTTP.METHOD_GET))
      {
         assert metaclass != null;

         XMLReadRequest request = new XMLReadRequest();

         request.setClassName(metaclass.getName());
         request.setAttributes(getAttributes(metaclass));
         request.setWhere(RPCUtil.parse(getParser(), getParameter("where"), null));

         if (oid != null)
         {
            request.setWhere(Pair.commutative(Symbol.AND, Pair.list(Symbol.EQ, Pair.attribute(""), oid), request.getWhere()));
         }

         request.setOrderBy((Pair)RPCUtil.parse(getParser(), getParameter("orderBy"), null));
         request.setCount((Number)RPCUtil.parse(getParser(), getParameter("count"), Primitive.createInteger(8)));
         request.setOffset((Number)RPCUtil.parse(getParser(), getParameter("offset"), null));

         return request;
      }

      if (oid == null && !sMethod.equals(HTTP.METHOD_POST))
      {
         throw createMethodException();
      }

      Reader reader = getReader();

      if (sMethod.equals(HTTP.METHOD_DELETE))
      {
         reader.mark(1);

         if (reader.read() < 0)
         {
            return new XMLChange1Request(new TransferObject(oid, metaclass.getName(), "delete", 0), null);
         }

         reader.reset();
      }

      Object obj = unmarshaller.deserialize(reader);

      if (obj instanceof TransferObject)
      {
         TransferObject tobj = (TransferObject)obj;

         if (oid != null)
         {
            tobj.setOID(oid);
         }
         else if (!sMethod.equals(HTTP.METHOD_POST))
         {
            throw createMethodException();
         }

         if (tobj.getClassName() == null && metaclass != null)
         {
            tobj.setClassName(metaclass.getName());
         }

         if (sMethod.equals(HTTP.METHOD_POST))
         {
            if (tobj.getEventName() == null)
            {
               tobj.setEventName("create");
            }
         }
         else if (sMethod.equals(HTTP.METHOD_PUT))
         {
            tobj.setEventName("update");
         }
         else if (sMethod.equals(HTTP.METHOD_DELETE))
         {
            tobj.setEventName("delete");
         }
         else
         {
            throw createMethodException();
         }

         return new XMLChange1Request(tobj, getAttributes(null));
      }

      if (obj instanceof Request ||
         obj instanceof XMLChangeRequest ||
         obj instanceof XMLInvocationRequest ||
         obj instanceof XMLReadRequest)
      {
         if (sMethod.equals(HTTP.METHOD_POST) && metaclass == null)
         {
            return obj;
         }
      }

      throw createMethodException();
   }

   /**
    * Unmarshal an event, taking event argument from request parameters.
    * @param event The event to unmarshal (not null).
    * @param oid The instance OID (null for static events, not null otherwise).
    * @throws IOException If an input error occurs.
    * @throws RequestException On unsupported request method for a specified metaclass.
    */
   protected Object unmarshal(Event event, OID oid) throws IOException, RequestException
   {
      assert (event.isStatic() && oid == null) || (!event.isStatic() && oid != null);

      if (getReader().read() >= 0)
      {
         throw createMethodException(); // event invocations should not have a body
      }

      TransferObject instance = null;

      if (oid != null)
      {
         instance = new TransferObject(event.getMetaclass().getName(), 0);
         instance.setOID(oid);
      }

      Argument result = event.getResult();
      XMLInvocationRequest request;

      if (result == null) // untyped event
      {
         QName element = WADLGenerator.computeElementType(Primitive.ANY, false);

         request = new XMLInvocationRequest(event, instance, element, false);
      }
      else
      {
         QName element = WADLGenerator.computeElementType(result.getType(), result.isCollection());

         request = new XMLInvocationRequest(event, instance, element, result.isCollection());
      }

      for (int i = 0, nCount = event.getArgumentCount(); i < nCount; ++i)
      {
         request.addArgument(unmarshal(event.getArgument(i))); // unmarshal each argument
      }

      return request;
   }

   /**
    * Unmarshal an argument value, taking marshalled value from request parameters.
    * @param arg The argument for which to unmarshal the value (not null).
    * @throws IOException If an input error occurs.
    */
   protected Object unmarshal(Argument arg)
   {
      Type type = arg.getType();

      // Primitive types know how to convert from String
      if (type instanceof Primitive && type != Primitive.ANY)
      {
         if (!arg.isCollection())
         {
            return type.convert(getParameter(arg.getName()));
         }

         String[] sValueArray = getParameters(arg.getName()); // get all values of collection

         if (sValueArray == null)
         {
            return null;
         }

         Object[] valueArray = new Object[sValueArray.length];

         for (int i = 0; i < valueArray.length; ++i)
         {
            valueArray[i] = type.convert(sValueArray[i]);
         }

         return valueArray;
      }
      else if (type instanceof Metaclass) // Metaclass types use OID to pass instance
      {
         if (!arg.isCollection())
         {
            String sValue = getParameter(arg.getName());

            if (sValue == null)
            {
               return null;
            }

            TransferObject value = new TransferObject(type.getName(), 0);

            value.setOID(OID.fromBinary(Binary.parse(sValue)));

            return value;
         }

         String[] sValueArray = getParameters(arg.getName()); // get all values of collection

         if (sValueArray == null)
         {
            return null;
         }

         String sMetaclass = type.getName();
         TransferObject[] valueArray = new TransferObject[sValueArray.length];

         for (int i = 0; i < valueArray.length; ++i)
         {
            valueArray[i] = new TransferObject(sMetaclass, 0);
            valueArray[i].setOID(OID.fromBinary(Binary.parse(sValueArray[i])));
         }

         return valueArray;
      }
      else if (arg.isCollection()) // collection types scheme parsed return Object[]
      {
         String[] sValueArray = getParameters(arg.getName()); // get all values of collection

         if (sValueArray == null)
         {
            return null;
         }

         Object[] valueArray = new Object[sValueArray.length];

         for (int i = 0; i < valueArray.length; ++i)
         {
            valueArray[i] = RPCUtil.parse(getParser(), sValueArray[i], null);
         }

         return valueArray;
      }
      else // everything else scheme parsed as a single object
      {
         return RPCUtil.parse(getParser(), getParameter(arg.getName()), null);
      }
   }

   /**
    * Return an array of values associated with the requested parameter name.
    * @see nexj.core.rpc.http.GenericHTTPServer#getParameter(String)
    * @param sName The parameter name to get values for.
    * @return The array of values associated with the requested parameter name.
    */
   protected String[] getParameters(String sName)
   {
      String[] sValueArray = (String[])getParameterMap().get(sName);

      return (sValueArray == null) ? m_request.getParameterValues(sName) : sValueArray;
   }

   /**
    * @return The Scheme parser.
    */
   protected Parser getParser()
   {
      if (m_parser == null)
      {
         m_parser = new SchemeParser(m_context.getMachine().getGlobalEnvironment());
      }

      return m_parser;
   }

   /**
    * Gets the attribute list from the request.
    * @param metaclass The metaclass to use as a default value. Can be null.
    * @return The request attributes.
    */
   protected Pair getAttributes(Metaclass metaclass)
   {
      String sAttributes = getParameter("attributes");

      if (sAttributes != null)
      {
         return (Pair)RPCUtil.parse(getParser(), sAttributes, null);
      }

      Pair attributes = null;

      if (metaclass != null)
      {
         for (Iterator/*<Attribute>*/ itr = metaclass.getAttributeIterator(); itr.hasNext();)
         {
            Attribute attribute = (Attribute)itr.next();
   
            if (!attribute.isStatic() && !attribute.isCalculated() &&
               attribute.getVisibility() == Metaclass.PUBLIC && !attribute.isCollection())
            {
               attributes = new Pair(attribute.getSymbol(), attributes);
            }
         }
      }

      return attributes;
   }

   /**
    * Find the metaclass event with the given name and best match for parameters supplied,
    * parameters taken from getParameterMap().
    * @param meta The metaclass to search for corresponding event (not null).
    * @param sEvent The event name to search for (not null).
    * @param bStatic The event being searched for must be static (false == must be non-static).
    * @return The best matching event or null if no valid match.
    */
   protected Event findEvent(Metaclass meta, String sEvent, boolean bStatic)
   {
      Selector selector = meta.getSelector(sEvent);

      if (selector == null)
      {
         return null;
      }

      for (int i = selector.getMaxArgCount(); i >= 0; --i)
      {
         Member member = selector.findMember(i);

         if (!(member instanceof Event) || member.isStatic() != bStatic)
         {
            continue;
         }

         Event event = (Event)member;
         boolean bMatch = true;

         // go through all event arguments and check that they were specified
         for (int k = 0, nCount = event.getArgumentCount(); k < nCount && bMatch; ++k)
         {
            Argument arg = event.getArgument(k);

            bMatch = !arg.isRequired() || getParameter(arg.getName()) != null;
         }

         if (bMatch) // ensure all required arguments for this event exist
         {
            return event;
         }
      }

      return null;
   }

   /**
    * @return An invalid method exception.
    */
   protected RequestException createMethodException()
   {
      throw new RequestException("err.rpc.xml.method",
         new Object[]{getMethod(), getRequestURI()});
   }
}