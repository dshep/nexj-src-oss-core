// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import nexj.core.scripting.Pair;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ObjUtil;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.SysUtil;

/**
 * The object containing the incoming request to the server.
 */
public class Request implements Serializable, Printable, Cloneable
{
   // constants

   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 1565196989796276205L;

   /**
    * The product namespace URL.
    */
   private String m_sNamespace;

   /**
    * The release version of the product, which has issued the request. The
    * server converts the request and the transfer objects based on this
    * version.
    */
   private String m_sVersion;

   /**
    * True if the request is asynchronous, i.e. the server enqueues the
    * request and immediately returns null to the client.
    */
   private boolean m_bAsync;

   /**
    * True if the results of the request execution should be committed to the
    * persistent storage. Use false to submit a request (generally from UI)
    * for retrieving derived attributes of new and edit-in-progress instances.
    */
   private boolean m_bCommit = true;

   /**
    * Whether this request should be invoked in stealth mode: debug and dump
    * level logging is suppressed for this request.
    */
   private boolean m_bStealth;

   // associations

   /**
    * The locale used for processing the request.
    */
   private Locale m_locale;
   
   /**
    * The time zone used for processing the request.
    */
   private TimeZone m_timeZone;

   /**
    * A transfer object identifying the instance which receives the response of
    * an asynchronous request. It must contain at least an OID, a class name
    * and an event name. The object receives the Response object in an optional
    * value "request", and/or the result array in an optional value "results".
    * The values must be null in the transfer object.
    */
   private TransferObject m_correlator;

   /**
    * The list of object invocations.
    */
   private List m_invocationList = new ArrayList(4); // of type Invocation

   /**
    * A list of transfer object specifying filters (as read requests,
    * with an additional optional attribute "instances", which specifies a list
    * of transfer objects), which retrieve any matching objects
    * affected by the request.
    */
   private List m_filterList; // of type TransferObject

   // operations

   /**
    * Sets the product namespace URL.
    * @param sNamespace The product namespace URL.
    */
   public void setNamespace(String sNamespace)
   {
      m_sNamespace = sNamespace;
   }
   
   /**
    * @return The product namespace URL.
    */
   public String getNamespace()
   {
      return m_sNamespace;
   }
   
   /**
    * Sets the product release version.
    * @param sVersion The product release version to set.
    */
   public void setVersion(String sVersion)
   {
      m_sVersion = sVersion;
   }

   /**
    * @return The product release version.
    */
   public String getVersion()
   {
      return m_sVersion;
   }

   /**
    * Sets the asynchronous request flag.
    * @param bAsync The asynchronous request flag to set.
    */
   public void setAsync(boolean bAsync)
   {
      m_bAsync = bAsync;
   }

   /**
    * @return The asynchronous request flag.
    */
   public boolean isAsync()
   {
      return m_bAsync;
   }
   
   /**
    * Sets the commit transaction flag.
    * @param bCommit The commit transaction flag to set.
    */
   public void setCommit(boolean bCommit)
   {
      m_bCommit = bCommit;
   }

   /**
    * @return The commit transaction flag.
    */
   public boolean isCommit()
   {
      return m_bCommit;
   }

   /**
    * @return Whether this request should be invoked in stealth mode: debug and
    * dump level logging is suppressed for this request.
    */
   public boolean isStealth()
   {
      return m_bStealth;
   }

   /**
    * Whether this request should be invoked in stealth mode: debug and dump
    * level logging is suppressed for this request.
    * @param bStealth True to suppress this logging
    */
   public void setStealth(boolean bStealth)
   {
      m_bStealth = bStealth;
   }
   
   /**
    * Sets the request locale.
    * @param locale The request locale to set.
    */
   public void setLocale(Locale locale)
   {
      m_locale = locale;
   }

   /**
    * @return The request locale.
    */
   public Locale getLocale()
   {
      return m_locale;
   }

   /**
    * Sets the request time zone.
    * @param timeZone The request time zone to set.
    */
   public void setTimeZone(TimeZone timeZone)
   {
      m_timeZone = timeZone;
   }
   
   /**
    * @return The request time zone.
    */
   public TimeZone getTimeZone()
   {
      return m_timeZone;
   }

   /**
    * Sets the correlation transfer object.
    * @param correlator The correlation transfer object to set.
    */
   public void setCorrelator(TransferObject correlator)
   {
      m_correlator = correlator;
   }

   /**
    * @return The correlation transfer object.
    */
   public TransferObject getCorrelator()
   {
      return m_correlator;
   }

   /**
    * Adds a new invocation to the request.
    * @param invocation The invocation to add.
    */
   public void addInvocation(Invocation invocation)
   {
      assert invocation != null;

      m_invocationList.add(invocation);
   }

   /**
    * Adds a new invocation to the request.
    * @param tobj The invocation transfer object.
    * @return The invocation object.
    */
   public Invocation addInvocation(TransferObject tobj)
   {
      assert tobj != null;

      Invocation invocation = new Invocation(tobj);

      addInvocation(invocation);

      return invocation;
   }

   /**
    * Adds a new invocation to the request.
    * @param tobj The invocation transfer object.
    * @param argArray The event argument array.
    * @return The invocation object.
    */
   public Invocation addInvocation(TransferObject tobj, Object[] argArray)
   {
      return addInvocation(tobj, null, argArray, null);
   }

   /**
    * Adds a new invocation to the request.
    * @param tobj The invocation transfer object.
    * @param argArray The event argument array.
    * @param attributes The result attribute specification.
    * @return The invocation object.
    */
   public Invocation addInvocation(TransferObject tobj, Pair attributes)
   {
      return addInvocation(tobj, null, null, attributes);
   }

   /**
    * Adds a new invocation to the request.
    * @param tobj The invocation transfer object.
    * @param argArray The event argument array.
    * @param attributes The result attribute specification.
    * @return The invocation object.
    */
   public Invocation addInvocation(TransferObject tobj, Object[] argArray, Pair attributes)
   {
      return addInvocation(tobj, null, argArray, attributes);
   }

   /**
    * Adds a new invocation to the request.
    * @param tobj The invocation transfer object.
    * @param sEvent The event name. Can be null.
    * @param argArray The event argument array.
    * @param attributes The result attribute specification.
    * @return The invocation object.
    */
   public Invocation addInvocation(TransferObject tobj, String sEvent, Object[] argArray, Pair attributes)
   {
      assert tobj != null;

      Invocation invocation = new Invocation(tobj, sEvent, argArray, attributes);

      addInvocation(invocation);

      return invocation;
   }

   /**
    * Adds a new invocation to the request.
    * @param sClass The class name.
    * @param sEvent The event name.
    * @param argArray The event argument array.
    * @param attributes The result attribute specification.
    * @return The invocation object.
    */
   public Invocation addInvocation(String sClass, String sEvent, Object[] argArray, Pair attributes)
   {
      assert sClass != null;
      assert sEvent != null;

      Invocation invocation = new Invocation(new TransferObject(sClass, 0), sEvent, argArray, attributes);

      addInvocation(invocation);

      return invocation;
   }

   /**
    * Gets an invocation by ordinal number.
    * @param nOrdinal The invocation ordinal number.
    * @return The invocation.
    */
   public Invocation getInvocation(int nOrdinal)
   {
      return (Invocation)m_invocationList.get(nOrdinal);
   }

   /**
    * @return The invocation count.
    */
   public int getInvocationCount()
   {
      return m_invocationList.size();
   }

   /**
    * @return The invocation iterator.
    */
   public Iterator getInvocationIterator()
   {
      return m_invocationList.iterator();
   }

   /**
    * Gets a transfer object by invocation ordinal number.
    * @param nOrdinal The invocation ordinal number.
    * @return The transfer object.
    */
   public TransferObject getObject(int nOrdinal)
   {
      return getInvocation(nOrdinal).getObject();
   }

   /**
    * @return Transfer object iterator.
    */
   public Iterator getObjectIterator()
   {
      return new Iterator()
      {
         private Iterator m_itr = getInvocationIterator();

         public boolean hasNext()
         {
            return m_itr.hasNext();
         }

         public Object next()
         {
            return ((Request.Invocation)m_itr.next()).getObject();
         }

         public void remove()
         {
            m_itr.remove();
         }
      };
   }

   /**
    * Same as addInvocation(tobj).
    * @deprecated Used for script backward compatibility.
    */
   public void addArg(TransferObject tobj)
   {
      addInvocation(tobj);
   }

   /**
    * Same as addInvocation(tobj, null, null, attributes).
    * @deprecated Used for script backward compatibility.
    */
   public void addArg(TransferObject tobj, Pair attributes)
   {
      addInvocation(tobj, null, null, attributes);
   }

   /**
    * Adds a new filter transfer object to the request.
    * @param filter The filter transfer object to add.
    */
   public void addFilter(TransferObject filter)
   {
      assert filter != null;

      if (m_filterList == null)
      {
         m_filterList = new ArrayList(4);
      }

      m_filterList.add(filter);
   }

   /**
    * Gets a filter transfer object by ordinal number.
    * @param nOrdinal The filter transfer object ordinal number (0-based).
    * @return The filter transfer object object.
    */
   public TransferObject getFilter(int nOrdinal)
   {
      return (TransferObject)m_filterList.get(nOrdinal);
   }

   /**
    * @return The filter transfer object count.
    */
   public int getFilterCount()
   {
      if (m_filterList == null)
      {
         return 0;
      }
      
      return m_filterList.size();
   }

   /**
    * @return An iterator for the contained filter transfer object objects.
    */
   public Iterator getFilterIterator()
   {
      if (m_filterList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_filterList.iterator();
   }

   /**
    * Determines if this request can be merged with another one.
    * @param request The other request.
    * @return True if the requests are compatible.
    */
   public boolean isCompatibleWith(Request request)
   {
      return m_bCommit == request.isCommit() &&
         m_bAsync == request.isAsync() &&
         ObjUtil.equal(m_correlator, request.getCorrelator()) &&
         ObjUtil.equal(m_locale, request.getLocale()) &&
         ObjUtil.equal(m_timeZone, request.getTimeZone()) &&
         ObjUtil.equal(m_sNamespace, request.getNamespace()) &&
         ObjUtil.equal(m_sVersion, request.getVersion());
   }

   /**
    * Appends a given request to this request.
    * @param request The request to append.
    */
   public void append(Request request)
   {
      for (int i = 0, n = request.getInvocationCount(); i < n; ++i)
      {
         addInvocation(request.getInvocation(i));
      }

      for (int i = 0, n = request.getFilterCount(); i < n; ++i)
      {
         addFilter(request.getFilter(i));
      }
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         Request request = (Request)super.clone();

         request.m_invocationList = (List)((ArrayList)m_invocationList).clone();

         if (m_filterList != null)
         {
            request.m_filterList = (List)((ArrayList)m_filterList).clone();
         }

         return request;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * Deserializes the object.
    * @param in The serialization stream.
    */
   private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      ObjectInputStream.GetField accessor = in.readFields();

      m_bAsync = accessor.get("m_bAsync", false);
      m_bCommit = accessor.get("m_bCommit", true);
      m_bStealth = accessor.get("m_bStealth", false);
      m_sNamespace = (String)accessor.get("m_sNamespace", null);
      m_sVersion = (String)accessor.get("m_sVersion", null);
      m_locale = (Locale)accessor.get("m_locale", null);
      m_timeZone = (TimeZone)accessor.get("m_timeZone", null);

      List invocationList = (List)accessor.get("m_invocationList", null);

      if (invocationList != null)
      {
         m_invocationList = invocationList;
      }
      else
      {
         // Deserialize legacy format, for backward compatibility
         List argList = (List)accessor.get("m_argList", null);
         int nCount = argList.size();
         List attributeList = null;
         List parameterList = null;

         try
         {
            attributeList = (List)accessor.get("m_attributeList", null);
            parameterList = (List)accessor.get("m_parameterList", null);
         }
         catch (IllegalArgumentException e)
         {
         }

         m_invocationList = invocationList = new ArrayList(nCount);

         for (int i = 0; i < nCount; ++i)
         {
            invocationList.add(new Invocation((TransferObject)argList.get(i), null,
               (parameterList == null) ? null : (Object[])parameterList.get(i),
               (attributeList == null) ? null : (Pair)attributeList.get(i)));
         }
      }

      m_filterList = (List)accessor.get("m_filterList", null);
      m_correlator = (TransferObject)accessor.get("m_correlator", null);
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      if (writer.addObject(this))
      {
         writer.addIndent(1);

         writer.write("Request(namespace=");
         writer.write(m_sNamespace);
         writer.write(", version=");
         writer.write(m_sVersion);
         writer.write(", async=");
         writer.write(String.valueOf(m_bAsync));
         writer.write(", commit=");
         writer.write(String.valueOf(m_bCommit));

         if (m_locale != null)
         {
            writer.write(", locale=");
            writer.write(m_locale.toString());
         }

         if (m_timeZone != null)
         {
            writer.write(", timeZone=");
            writer.write(m_timeZone.getID());
         }

         writer.write(')');
         writer.indent(-1);
         writer.write("invocations(");
         writer.print(getInvocationCount());
         writer.write("):");

         for (int i = 0; i < getInvocationCount(); ++i)
         {
            writer.print(getInvocation(i));
         }

         writer.indent(-1);
         writer.write("filters(");
         writer.print(getFilterCount());
         writer.write("):");

         for (int i = 0; i < getFilterCount(); ++i)
         {
            writer.indent();
            writer.print(getFilter(i));
         }

         writer.indent(-1);
         writer.write("correlator=");
         writer.print(m_correlator);
         writer.addIndent(-1);
         writer.write(SysUtil.LINE_SEP);
      }
      else
      {
         writer.printRef(this);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   // inner classes

   /**
    * Invocation of an event on a particular object instance.
    */
   public static class Invocation implements Serializable, Printable, Cloneable
   {
      // constant

      /**
       * Serialization UID
       */
      private static final long serialVersionUID = -6066090587641169880L;

      // attributes

      /**
       * The transfer object.
       */
      private TransferObject m_object;

      /**
       * The event name. Can be null.
       */
      private String m_sEvent;
      
      /**
       * The event arguments. Can be null.
       */
      private Object[] m_argumentArray;

      /**
       * The result attribute specification, in read format (attr1 (attr2 ... attr2_N) ... attrN). Can be null. 
       */
      private Pair m_attributes;

      // constructors

      /**
       * Constructs the invocation.
       * @param tobj The transfer object.
       */
      public Invocation(TransferObject tobj)
      {
         m_object = tobj;
      }

      /**
       * Constructs the invocation.
       * @param tobj The transfer object.
       * @param sEvent The event name. Can be null.
       * @param argumentArray The event arguments. Can be null.
       * @param attributes The result attribute specification. Can be null.
       */
      public Invocation(TransferObject tobj, String sEvent, Object[] argumentArray, Pair attributes)
      {
         m_object = tobj;
         m_sEvent = sEvent;
         m_argumentArray = argumentArray;
         m_attributes = attributes;
      }

      // operations

      /**
       * Sets the transfer object.
       * @param object The transfer object to set.
       */
      public void setObject(TransferObject object)
      {
         m_object = object;
      }

      /**
       * @return The transfer object.
       */
      public TransferObject getObject()
      {
         return m_object;
      }

      /**   
       * Sets the event name.
       * @param sEvent The event name to set.
       */
      public void setEventName(String sEvent)
      {
         m_sEvent = sEvent;
      }

      /**
       * @return The event name.
       */
      public String getEventName()
      {
         return m_sEvent;
      }

      /**
       * Sets the event arguments.
       * @param argumentArray The event arguments to set.
       */
      public void setArguments(Object[] argumentArray)
      {
         m_argumentArray = argumentArray;
      }

      /**
       * @return The event arguments.
       */
      public Object[] getArguments()
      {
         return m_argumentArray;
      }

      /**
       * Sets the result attribute spec.
       * @param attributes The result attribute spec to set.
       */
      public void setAttributes(Pair attributes)
      {
         m_attributes = attributes;
      }

      /**
       * @return The result attribute spec.
       */
      public Pair getAttributes()
      {
         return m_attributes;
      }

      /**
       * @see java.lang.Object#clone()
       */
      public Object clone() throws CloneNotSupportedException
      {
         try
         {
            return super.clone();
         }
         catch (CloneNotSupportedException e)
         {
            return null;
         }
      }

      /**
       * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
       */
      public void printOn(PrintWriter writer) throws IOException
      {
         writer.indent();
         writer.print(m_object);

         if (m_sEvent != null)
         {
            writer.indent();
            writer.write("event: ");
            writer.write(m_sEvent);
         }

         if (m_argumentArray != null)
         {
            writer.indent();
            writer.write("arguments: ");
            writer.addIndent(1);
            writer.print(m_argumentArray);
            writer.addIndent(-1);
         }

         if (m_attributes != null)
         {
            writer.indent();
            writer.write("attributes: ");
            writer.addIndent(1);
            writer.print(m_attributes);
            writer.addIndent(-1);
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return PrintWriter.toString(this);
      }
   }
}
