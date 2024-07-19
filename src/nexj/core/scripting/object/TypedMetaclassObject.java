// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import java.util.List;

import nexj.core.meta.GenericType;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.scripting.EnumSet;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * A metaclass to extend the dynamic object system with typed attributes and methods.
 */
public class TypedMetaclassObject extends BasicMetaclassObject
{
   // constants

   /**
    * The symbol of the root typed metaclass in the hierarchy.
    */
   public final static Symbol TYPED_METACLASS_SYMBOL = Symbol.define("sys:TypedMetaclass");

   // constructors

   /**
    * @see MetaclassObject#MetaclassObject(Symbol)
    */
   public TypedMetaclassObject(Symbol symbol)
   {
      super(TypedGrandMetaclassObject.INSTANCE, symbol);
   }

   /**
    * Constructs the root typed metaclass.
    */
   protected TypedMetaclassObject()
   {
      super(TypedGrandMetaclassObject.INSTANCE, TYPED_METACLASS_SYMBOL);
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
      return new TypedBasicObject(classObject);
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#createMetaclass(nexj.core.scripting.Symbol)
    */
   protected MetaclassObject createMetaclass(Symbol symbol)
   {
      return new TypedMetaclassObject(symbol);
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
      return TypedBasicObject.CLASS_SYMBOL;
   }

   /**
    * @see nexj.core.scripting.object.BasicMetaclassObject#getObjectBaseClassSymbol()
    */
   protected Symbol getObjectBaseClassSymbol()
   {
      return BasicObject.CLASS_SYMBOL;
   }

   /**
    * Gets the type known by the given symbol. If no such type is known, then it is forward-defined.
    * @param sym The type symbol.
    * @param member The member on which the type is being set.
    * @param machine The scheme VM.
    * @return The type.
    */
   public static Type getForwardType(Symbol sym, MemberObject member, Machine machine)
   {
      Object definedVar = machine.getGlobalEnvironment().findVariable(sym);

      if (definedVar instanceof Type)
      {
         return (Type)definedVar;
      }

      return member.getHolder().getMetaclass().defineClass(sym);
   }

   /**
    * Verifies class attribute assignments.
    * @see nexj.core.scripting.object.BasicMetaclassObject#setValue(nexj.core.scripting.object.ClassObject, int, java.lang.Object, nexj.core.scripting.Machine)
    */
   protected void setValue(ClassObject classObject, int nOffset, Object value, Machine machine)
   {
      TypedAttributeObject attribute = (TypedAttributeObject)resolveAttribute(nOffset);

      validateValue(value, attribute.getValueType(), attribute.isCollection(), classObject, attribute);

      super.setValue(classObject, nOffset, value, machine);
   }

   /**
    * Validates a value against a type and calls the error method on the handler if the value is of the wrong type.
    * @param value The value to check.
    * @param type The expected type.
    * @param bCollection The expected collection flag.
    * @param instance The instance where the check is happening.
    * @param handler The error handler.
    */
   public static void validateValue(Object value, Type type, boolean bCollection, Object instance, TypeErrorHandler handler)
   {
      if (value == null || type == null)
      {
         return;
      }

      if (bCollection)
      {
         if (!(value instanceof List))
         {
            handler.error(Symbol.COLLECTION.getName(), GenericType.typeOf(value).toString(), instance);
         }
   
         if (type != Primitive.ANY)
         {
            List list = (List)value;
   
            for (int i = 0, nSize = list.size(); i < nSize; i++)
            {
               validateValue(list.get(i), type, instance, handler);
            }
         }
      }
      else if (type != Primitive.ANY)
      {
         if (value instanceof List)
         {
            handler.error(type.toString(), Symbol.COLLECTION.getName(), instance);
         }
   
         validateValue(value, type, instance, handler);
      }
   }

   /**
    * Validates a value against a type & calls the error handler if the value is the wrong type.
    * @param value The value to check.
    * @param expectedType The expected type.
    * @param instance The instance where the check is happening.
    * @param handler The error handler.
    */
   private static void validateValue(Object value, Type expectedType, Object instance, TypeErrorHandler handler)
   {
      if (expectedType instanceof EnumSet)
      {
         EnumSet enumeration = (EnumSet)expectedType;

         if (!enumeration.contains(value))
         {
            handler.error(enumeration.toString(), value.toString(), instance);
         }
      }
      else
      {
         Type valueType = GenericType.typeOf(value);

         if (!expectedType.isUpcast(valueType))
         {
            handler.error(expectedType.toString(), valueType.toString(), instance);
         }
      }
   }

   /**
    * Initializes the typed object system.
    */
   public static void init()
   {
      ClassEnvironment env = getEnvironment();

      if (!(env.findClass(TYPED_METACLASS_SYMBOL) instanceof TypedMetaclassObject))
      {
         TypedMetaclassObject metaclass = new TypedMetaclassObject();

         ClassObject.addMembers(metaclass);
         metaclass.define();

         ClassObject typedObjectClass = metaclass.defineClass(TypedBasicObject.CLASS_SYMBOL);

         TypedGrandMetaclassObject.addMethodValidation(typedObjectClass.getMetaclass());

         // The attribute and method objects are not typed.
         BasicMetaclassObject basicMetaclass = (BasicMetaclassObject)env.findClass(METACLASS_SYMBOL);
         ClassObject attribute = basicMetaclass.defineClass(TypedAttributeObject.CLASS_SYMBOL);
         ClassObject method = basicMetaclass.defineClass(TypedMethodObject.CLASS_SYMBOL);

         ClassObject baseAttribute = env.findClass(AttributeObject.CLASS_SYMBOL);
         ClassObject baseMethod = env.findClass(MethodObject.CLASS_SYMBOL);

         assert baseAttribute != null;
         assert baseMethod != null;

         attribute.addBase(baseAttribute);
         attribute.removeBase(BasicObject.CLASS_SYMBOL);
         method.addBase(baseMethod);
         method.removeBase(BasicObject.CLASS_SYMBOL);

         if (attribute.isForward())
         {
            attribute.setForward(false);
            TypedAttributeObject.addMembers(attribute);
         }

         if (method.isForward())
         {
            method.setForward(false);
            TypedMethodObject.addMembers(method);
         }
      }
   }

   // inner classes

   /**
    * Allows different objects to throw different type mismatch errors.
    */
   public interface TypeErrorHandler
   {
      /**
       * Throws a type mismatch exception.
       * @param sExpected The expected type name.
       * @param sActual The actual type name.
       * @param location The instance (or other) where the error happened.
       */
      public void error(String sExpected, String sActual, Object location);
   }
}
