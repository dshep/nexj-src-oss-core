// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.soa;

import java.util.Collections;
import java.util.Iterator;

import nexj.core.meta.Primitive;
import nexj.core.meta.xml.XMLSOAMetadataLoader;
import nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject;
import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;

/**
 * An interface. Consists of one or more methods.
 */
public class Interface extends SOAObject implements GlobalObject
{
   // associations

   /**
    * The definition where this interface is defined.
    */
   protected Definition m_definition;

   /**
    * The methods defined on this interface keyed by method name and number of arguments.
    */
   protected Lookup2D m_methodMap = new HashTab2D();  // Method[String, Integer]

   // constructors

   /**
    * USE Definition.defineInterface(String) TO CREATE AN INTERFACE.
    * Creates a new interface.
    * @param definition The definition where this interface is defined.
    */
   Interface(Definition definition)
   {
      m_definition = definition;
   }

   // operations

   /**
    * Gets the parent definition.
    * @return The definition where this interface is defined.
    */
   public Definition getDefinition()
   {
      return m_definition;
   }

   /**
    * Adds a method to this interface.
    * @param method The method to add.
    * @return True if the method was added; false if a method with the same signature has already
    * been added.
    */
   public boolean addMethod(Method method)
   {
      return m_methodMap.put(method.getName(), Primitive.createInteger(method.getArgCount()), method) == null;
   }

   /**
    * Given a method signature (name and argument count), finds the method.
    * @param sName The method name.
    * @param nArgCount The method argument count.
    * @return The method; null if not found.
    */
   public Method getMethod(String sName, int nArgCount)
   {
      return (Method)m_methodMap.get(sName, Primitive.createInteger(nArgCount));
   }

   /**
    * Gets the number of methods on the interface.
    * @return The number of methods.
    */
   public int getMethodCount()
   {
      return m_methodMap.size();
   }

   /**
    * Gets an iterator over this interface's methods.
    * @return An iterator over the Methods.
    */
   public Iterator getMethodIterator()
   {
      return m_methodMap.valueIterator();
   }

   /**
    * Generates the following code:
    *
    *    (define-class <definition QName>:interface:<name> (soa:InterfaceObject) "Description"
    *       (class-attribute :stateful-method-map :init (soa:list->hashtable (list '(<method symbol> . <arg count>) <state type> ...)))
    *       (class-attribute :method-fault-map :init (soa:list->hashtable (list '(<method symbol> . <arg count>) (list <fault type> ...) ...)))
    *       (class-method ...)
    *       ...
    *    )
    *
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
    */
   public Object getCode()
   {
      Pair methods = null;
      Pair mapInit = null;
      Pair faultMapInit = null;

      for (Iterator itr = m_methodMap.valueIterator(); itr.hasNext(); )
      {
         Method method = (Method)itr.next();
         Pair methodKey = Pair.quote(new ConstPair(Symbol.define(method.getName()), Primitive.createInteger(method.getArgCount())));

         methods = new ConstPair(method.getCode(), methods);

         if (method.getStateType() != null)
         {
            mapInit = ConstPair.cons(methodKey, method.getStateType(), mapInit);
         }

         Pair faults = Pair.fromIterator(method.getFaultIterator());

         if (faults != null)
         {
            faultMapInit = ConstPair.cons(methodKey, new ConstPair(Symbol.LIST, faults), faultMapInit);
         }
      }

      Pair body = ConstPair.cons(
         Pair.list(XMLSOAMetadataLoader.CLASS_ATTRIBUTE, XMLSOAMetadataLoader.STATEFUL_METHOD_MAP,
            XMLSOAMetadataLoader.INIT, toHashtable(mapInit)),
         Pair.list(XMLSOAMetadataLoader.CLASS_ATTRIBUTE, XMLSOAMetadataLoader.METHOD_FAULT_MAP,
            XMLSOAMetadataLoader.INIT, toHashtable(faultMapInit)),
         methods
      );

      return ConstPair.cons(
         XMLSOAMetadataLoader.DEFINE_CLASS,
         Symbol.define(getGlobalName()),
         Pair.list(XMLSOAMetadataLoader.INTERFACE_OBJECT),
         getDescription(),
         body
      );
   }

   /**
    * Generates code to convert the list argument to a hashtable, or returns an empty hashtable constant
    * if list is null.
    * @param list The list to convert to a hashtable.
    * @return Code to generate the hashtable, or a hashtable constant.
    */
   protected static Object toHashtable(Pair list)
   {
      if (list == null)
      {
         return Collections.emptyMap();
      }

      return ConstPair.list(XMLSOAMetadataLoader.LIST_HASHTABLE, new ConstPair(Symbol.LIST, list));
   }

   /**
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject#getGlobalName()
    */
   public String getGlobalName()
   {
      StringBuilder buf = new StringBuilder(m_definition.getGlobalName().length() + 11 + m_sName.length());

      buf.append(m_definition.getGlobalName());
      buf.append(":interface:");
      buf.append(m_sName);

      return buf.toString();
   }
}

