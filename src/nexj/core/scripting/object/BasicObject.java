// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;


/**
 * Object with state values indexed by attribute offset.
 */
public class BasicObject extends GenericObject
{
   // constants

   /**
    * The symbol of the root class in the hierarchy.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:Object");

   // attributes

   /**
    * The object attributes indexed by attribute offset: Object[].
    */
   protected Object[] m_valueArray;

   // constructors

   /**
    * Constructs the object.
    * @param classObject The class object.
    */
   public BasicObject(ClassObject classObject)
   {
      super(classObject);
      m_valueArray = new Object[classObject.resolveAttributeCount()];
   }

   // operations

   /**
    * @see nexj.core.scripting.object.GenericObject#setValue(int, java.lang.Object, Machine)
    */
   public void setValue(int nOffset, Object value, Machine machine)
   {
      m_valueArray[nOffset] = value;
   }

   /**
    * @see nexj.core.scripting.object.GenericObject#getValue(int, Machine)
    */
   public Object getValue(int nOffset, Machine machine)
   {
      return m_valueArray[nOffset];
   }
}
