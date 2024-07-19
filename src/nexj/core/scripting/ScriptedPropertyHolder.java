// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.meta.Primitive;

/**
 * Generic scripted property holder.
 */
public abstract class ScriptedPropertyHolder implements Function
{
   // operations

   /**
    * @return The holder class name.
    */
   public abstract String getClassName();
   
   /**
    * Gets a property from the map.
    * @param sym The property name.
    * @return The property value. 
    */
   public abstract Object getValue(String sName);

   /**
    * Gets a property from the map.
    * @param sym The property symbol.
    * @return The property value. 
    */
   public Object getValue(Symbol sym)
   {
      return getValue(sym.getName());
   }

   /**
    * Puts a property value.
    * @param sym The property symbol.
    * @param value The property value.
    */
   public void setValue(Symbol sym, Object value)
   {
      setValue(sym.getName(), value);
   }

   /**
    * Puts a property value.
    * @param sName The property name.
    * @param value The property value.
    */
   public void setValue(String sName, Object value)
   {
      throw new ScriptingException("err.scripting.readOnlyPropertyMap",
         new Object[]{getClassName()});
   }

   /**
    * Invokes a method with a specified argument count greater than 2.
    * @param sym The method symbol.
    * @param nArgCount The argument count, including the method symbol.
    * @param machine The machine.
    * @return The method return value.
    */
   protected Object invoke(Symbol sym, int nArgCount, Machine machine)
   {
      throw new ScriptingException("err.scripting.maxArgCount",
         new Object[]{getClassName(),
            Primitive.createInteger(2),
            Primitive.createInteger(nArgCount)});
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount < 1)
      {
         throw new ScriptingException("err.scripting.minArgCount",
               new Object[]{getClassName(),
                  Primitive.createInteger(1),
                  Primitive.createInteger(nArgCount)});
      }

      Object sym = machine.getArg(0, nArgCount);
      String sName;

      if (sym instanceof Symbol)
      {
         sName = ((Symbol)sym).getName();
      }
      else if (sym instanceof String && nArgCount <= 2)
      {
         sName = (String)sym;
      }
      else
      {
         throw new ScriptingException("err.scripting.variableSymbol",
            new Object[]{getClassName()});
      }

      if (nArgCount == 1)
      {
         machine.returnValue(getValue(sName), nArgCount);
      }
      else if (nArgCount == 2)
      {
         Object value = machine.getArg(1, nArgCount);

         setValue(sName, value);
         machine.returnValue(value, nArgCount);
      }
      else
      {
         machine.returnValue(invoke((Symbol)sym, nArgCount, machine), nArgCount);
      }

      return false;
   }
}
