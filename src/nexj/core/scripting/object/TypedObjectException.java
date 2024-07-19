// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Exception object with type-checked state and method arguments.
 */
public class TypedObjectException extends ObjectException
{
   // constants 

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 1245126438072659955L;

   /**
    * The symbol of the root typed class in the hierarchy.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:TypedException");

   // constructors

   /**
    * Constructs the object.
    * @param classObject The object's class.
    */
   public TypedObjectException(ClassObject classObject)
   {
      super(classObject);
   }

   // operations

   /**
    * Verifies instance attribute assignments.
    * @see nexj.core.scripting.object.BasicObject#setValue(int, java.lang.Object, nexj.core.scripting.Machine)
    */
   public void setValue(int nOffset, Object value, Machine machine)
   {
      TypedAttributeObject attribute = (TypedAttributeObject)m_class.resolveAttribute(nOffset);

      TypedMetaclassObject.validateValue(value, attribute.getValueType(), attribute.isCollection(), this, attribute);

      super.setValue(nOffset, value, machine);
   }
}
