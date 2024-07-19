// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;

/**
 * Named object.
 */
public abstract class NamedObject extends GenericObject implements Named
{
   // attributes

   /**
    * The object name symbol.
    */
   protected Symbol m_symbol;

   // constructors

   /**
    * Constructs the named object.
    * @param classObject The class object.
    */
   protected NamedObject(ClassObject classObject)
   {
      super(classObject);
   }

   /**
    * Constructs the named object.
    * @param classObject The class object.
    * @param sName The object name. Can be null.
    */
   protected NamedObject(ClassObject classObject, String sName)
   {
      super(classObject);
      setName(sName);
   }

   /**
    * Constructs the named object.
    * @param classObject The class object.
    * @param symbol The object symbol. Can be null.
    */
   protected NamedObject(ClassObject classObject, Symbol symbol)
   {
      super(classObject);
      setSymbol(symbol);
   }

   // operations

   /**
    * Sets the object symbol.
    * @param symbol The object symbol. Can be null.
    */
   protected void setSymbol(Symbol symbol)
   {
      m_symbol = symbol;
   }

   /**
    * @return The object symbol.
    */
   public Symbol getSymbol()
   {
      return m_symbol;
   }

   /**
    * Sets the object name.
    * @param sName The name to set. Can be null.
    */
   protected void setName(String sName)
   {
      setSymbol((sName == null) ? null : Symbol.define(sName));
   }

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return (m_symbol == null) ? null : m_symbol.getName();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + ':' +
         ((m_symbol == null) ? String.valueOf(System.identityHashCode(this)) : m_symbol.getName());
   }

   /**
    * Exposes members from NamedObject.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      classObject.addMethod(":name", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((NamedObject)machine.getArg(0, nArgCount)).getName(), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":symbol", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((NamedObject)machine.getArg(0, nArgCount)).getSymbol(), nArgCount);

            return false;
         }
      });
   }
}
