// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.soa;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.xml.XMLSOAMetadataLoader;
import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.LookupDeque;

/**
 * A method from an interface. Declares zero or more arguments, an optional return value, and zero
 * or more faults.
 */
public class Method extends SOAObject
{
   // associations

   /**
    * The interface holding this method.
    */
   protected Interface m_parentInterface;

   /**
    * The ordered set of arguments.
    */
   protected LookupDeque m_argumentLookupDeque = new LinkedHashTab(); // of type Argument[]

   /**
    * The return value type & information. Null for no return value (void).
    */
   protected Result m_result;

   /**
    * The faults that this method can raise.
    */
   protected List m_faultList = new ArrayList(); // of type Symbol[]

   /**
    * The type, if any, of the method's implicit state parameter.
    */
   protected Symbol m_stateType;

   // constructors

   /**
    * Creates a new interface method.
    * @param parent The interface holding this method.
    * @param sName The name of this method.
    */
   public Method(Interface parent)
   {
      m_parentInterface = parent;
   }

   // operations

   /**
    * Adds an argument to this method.
    * @param argument The argument to add.
    * @return The previous argument with the same name, null if none existed.
    */
   public Argument addArgument(Argument argument)
   {
      return (Argument)m_argumentLookupDeque.put(argument.getName(), argument);
   }

   /**
    * @param sName The name of the argument to find.
    * @return An argument with the specified name or null if none found.
    */
   public Argument findArgument(String sName)
   {
      return (Argument)m_argumentLookupDeque.get(sName);
   }

   /**
    * Gets a list of the arguments.
    * @return A string representation of the method arguments.
    */
   public String getArgString()
   {
      StringBuilder buf = new StringBuilder();

      buf.append('(');

      boolean bFirst = true;

      for (Iterator it = m_argumentLookupDeque.valueIterator(); it.hasNext();)
      {
         Argument arg = (Argument)it.next();

         if (!bFirst)
         {
            buf.append(' ');
         }

         buf.append(arg.getName());
         bFirst = false;
      }

      buf.append(')');

      return buf.toString();
   }

   /**
    * Gets the number of arguments for this method.
    * @return The method argument count.
    */
   public int getArgCount()
   {
      return m_argumentLookupDeque.size();
   }

   /**
    * Gets an iterator over this method's arguments.
    * @return An iterator over the Arguments.
    */
   public Iterator getArgIterator()
   {
      return m_argumentLookupDeque.valueIterator();
   }

   /**
    * Sets the return value type/etc. of this method.
    * @param result The method result.
    */
   public void setResult(Result result)
   {
      m_result = result;
   }

   /**
    * Gets the number of faults for this method.
    * @return The method fault count.
    */
   public int getFaultCount()
   {
      return m_faultList.size();
   }

   /**
    * Gets an iterator over this method's faults.
    * @return An iterator over the Faults.
    */
   public Iterator getFaultIterator()
   {
      return m_faultList.iterator();
   }

   /**
    * Gets the return value type/etc. of this method.
    * @return The method result.
    */
   public Result getResult()
   {
      return m_result;
   }

   /**
    * Adds a fault to this method.
    * @param fault The fault to add.
    */
   public void addFault(Symbol fault)
   {
      m_faultList.add(fault);
   }

   /**
    * Sets the stateful method type.
    * @param type The type of the method's implicit state parameter. May be null.
    */
   public void setStateType(Symbol type)
   {
      m_stateType = type;
   }

   /**
    * Gets the stateful method type.
    * @return The type of the method's implicit state parameter. May be null.
    */
   public Symbol getStateType()
   {
      return m_stateType;
   }

   /**
    * Generates the following code:
    *
    *    (class-method <name> (<args>) :type <return-type> :collection <return-collection> "Description"
    *       (throw (java.lang.UnsupportedOperationException'new <name>))
    *    )
    *
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
    */
   public Object getCode()
   {
      Symbol type = (m_result == null) ? null : m_result.getType();
      Boolean collection = (m_result == null) ? Boolean.FALSE : Boolean.valueOf(m_result.isCollection());

      Pair argList = null;
      Pair argTypeList = null;
      Pair argCollectionList = null;

      for (Iterator it = m_argumentLookupDeque.reverseValueIterator(); it.hasNext();)
      {
         Argument arg = (Argument)it.next();

         argList = new Pair(arg.getCode(), argList);
         argTypeList = new Pair(arg.getType(), argTypeList);
         argCollectionList = new Pair(Boolean.valueOf(arg.isCollection()), argCollectionList);
      }

      Pair code = Pair.list(XMLSOAMetadataLoader.CLASS_METHOD,
         Symbol.define(m_sName),
         argList);

      if (type != null)
      {
         Pair.nconc(code, Pair.list(XMLSOAMetadataLoader.TYPE, type, XMLSOAMetadataLoader.COLLECTION, collection));
      }

      if (argTypeList != null)
      {
         Pair.nconc(code, Pair.list(XMLSOAMetadataLoader.ARG_TYPES, argTypeList, XMLSOAMetadataLoader.ARG_COLLECTIONS, argCollectionList));
      }

      return Pair.nconc(code, Pair.list(
            getDescription(),
            Pair.list(
               Symbol.THROW,
               Pair.list(
                  UnsupportedOperationException.class,
                  Pair.quote(Symbol.NEW),
                  m_sName + '/' + getArgCount()
               )
            )
         )
      );
   }

   /**
    * Ensures two methods with equivalent signatures cannot be added to the same interface.
    * Signatures are equivalent if the method name matches and the number of arguments is
    * the same.
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof Method))
      {
         return false;
      }

      Method other = (Method)obj;

      if (!m_sName.equals(other.m_sName))
      {
         return false;
      }

      if (m_argumentLookupDeque.size() != other.m_argumentLookupDeque.size())
      {
         return false;
      }

      return true;
   }

   /**
    * Ensures two methods with equivalent signatures cannot be added to the same interface.
    * Signatures are equivalent if the method name matches and the number of arguments is
    * the same.
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_sName.hashCode() ^ m_argumentLookupDeque.size();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return m_sName + getArgString();
   }
}

