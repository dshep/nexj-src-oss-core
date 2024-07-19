// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * A metaclass to extend the dynamic object system with typed attributes and methods for exception objects.
 */
public class TypedExceptionMetaclassObject extends ExceptionMetaclassObject
{
   // constants

   /**
    * The symbol of the root typed exception metaclass in the hierarchy.
    */
   public final static Symbol METACLASS_SYMBOL = Symbol.define("sys:TypedExceptionMetaclass");

   // constructors

   /**
    * @see BasicMetaclassObject#BasicMetaclassObject(Symbol)
    */
   public TypedExceptionMetaclassObject(Symbol symbol)
   {
      super(TypedGrandMetaclassObject.INSTANCE, symbol);
   }

   /**
    * Constructs the root typed exception metaclass. 
    */
   public TypedExceptionMetaclassObject()
   {
      super(TypedGrandMetaclassObject.INSTANCE, METACLASS_SYMBOL);
      ObjectException.addClassMembers(this);
   }

   // operations

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#createAttribute(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol)
    */
   protected AttributeObject createAttribute(ClassObject classObject, Symbol symbol)
   {
      return new TypedAttributeObject(classObject, symbol);
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#createInstance(nexj.core.scripting.object.ClassObject)
    */
   protected ObjectOriented createInstance(ClassObject classObject)
   {
      return new TypedObjectException(classObject);
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#createMetaclass(nexj.core.scripting.Symbol)
    */
   protected MetaclassObject createMetaclass(Symbol symbol)
   {
      return new TypedExceptionMetaclassObject(symbol);
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#createMethod(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol, int, boolean, nexj.core.scripting.Function)
    */
   protected MethodObject createMethod(ClassObject classObject, Symbol symbol, int nArgCount, boolean bVarArg, Function function)
   {
      return new TypedMethodObject(classObject, symbol, nArgCount, bVarArg, function);
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#getObjectClassSymbol()
    */
   protected Symbol getObjectClassSymbol()
   {
      return TypedObjectException.CLASS_SYMBOL;
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#getObjectBaseClassSymbol()
    */
   protected Symbol getObjectBaseClassSymbol()
   {
      return ObjectException.CLASS_SYMBOL;
   }

   /**
    * Verifies class attribute assignments.
    * @see nexj.core.scripting.object.BasicMetaclassObject#setValue(nexj.core.scripting.object.ClassObject, int, java.lang.Object, nexj.core.scripting.Machine)
    */
   protected void setValue(ClassObject classObject, int nOffset, Object value, Machine machine)
   {
      TypedAttributeObject attribute = (TypedAttributeObject)resolveAttribute(nOffset);

      TypedMetaclassObject.validateValue(value, attribute.getValueType(), attribute.isCollection(), classObject, attribute);

      super.setValue(classObject, nOffset, value, machine);
   }

   /**
    * Initializes the typed object system.
    */
   public static void init()
   {
      ClassEnvironment env = getEnvironment();

      if (!(env.findClass(METACLASS_SYMBOL) instanceof TypedExceptionMetaclassObject))
      {
         TypedExceptionMetaclassObject metaclass = new TypedExceptionMetaclassObject();

         ClassObject.addMembers(metaclass);
         metaclass.define();

         ClassObject typedObjectClass = metaclass.defineClass(TypedObjectException.CLASS_SYMBOL);

         TypedGrandMetaclassObject.addMethodValidation(typedObjectClass.getMetaclass());
      }
   }
}
