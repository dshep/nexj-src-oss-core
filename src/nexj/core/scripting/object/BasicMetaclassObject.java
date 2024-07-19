// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Metaclass for a class object which state is indexed by attribute offset
 * and stored in the global environment. 
 */
public class BasicMetaclassObject extends MetaclassObject
{
   // constants

   /**
    * The symbol of the root metaclass in the hierarchy.
    */
   public final static Symbol METACLASS_SYMBOL = Symbol.define("sys:Metaclass");

   /**
    * The :marshalled? symbol for the class attribute that specifies whether or not instances should be marshalled.
    */
   public final static Symbol MARSHALLED_P = Symbol.define(":marshalled?");

   // constructors

   /**
    * @see MetaclassObject#MetaclassObject(Symbol)
    */
   public BasicMetaclassObject(Symbol symbol)
   {
      super(symbol);
   }

   /**
    * @see MetaclassObject#MetaclassObject(MetaclassObject, Symbol)
    */
   public BasicMetaclassObject(MetaclassObject grandMetaclass, Symbol symbol)
   {
      super(grandMetaclass, symbol);
   }

   /**
    * Constructs the root metaclass.
    */
   protected BasicMetaclassObject()
   {
      this(METACLASS_SYMBOL);
   }

   // operations

   /**
    * @see nexj.core.scripting.object.MetaclassObject#setValue(nexj.core.scripting.object.ClassObject, int, java.lang.Object, Machine)
    */
   protected void setValue(ClassObject classObject, int nOffset, Object value, Machine machine)
   {
      ClassEnvironment env = machine.getGlobalEnvironment();
      Object[] valueArray = (Object[])env.getState(classObject);

      if (valueArray == null)
      {
         if (value == null)
         {
            return;
         }

         valueArray = new Object[classObject.getClassObject().resolveAttributeCount()];
         env.setState(classObject, valueArray);
         classObject.initialize(machine);
      }

      valueArray[nOffset] = value;
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getValue(nexj.core.scripting.object.ClassObject, int, Machine)
    */
   protected Object getValue(ClassObject classObject, int nOffset, Machine machine)
   {
      ClassEnvironment env = machine.getGlobalEnvironment();
      Object[] valueArray = (Object[])env.getState(classObject);

      if (valueArray == null)
      {
         ClassObject metaclass = classObject.getClassObject();

         if (metaclass.getInitializerCount() == 0)
         {
            return null;
         }

         valueArray = new Object[metaclass.resolveAttributeCount()];
         env.setState(classObject, valueArray);
         classObject.initialize(machine);
      }

      return valueArray[nOffset];
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createInstance(ClassObject)
    */
   protected ObjectOriented createInstance(ClassObject classObject)
   {
      return new BasicObject(classObject);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createMetaclass(nexj.core.scripting.Symbol)
    */
   protected MetaclassObject createMetaclass(Symbol symbol)
   {
      return new BasicMetaclassObject(symbol);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createAttribute(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol)
    */
   protected AttributeObject createAttribute(ClassObject classObject, Symbol symbol)
   {
      return new AttributeObject(classObject, symbol);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createMethod(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol, int, boolean, nexj.core.scripting.Function)
    */
   protected MethodObject createMethod(ClassObject classObject, Symbol symbol, int nArgCount, boolean bVarArg, Function function)
   {
      return new MethodObject(classObject, symbol, nArgCount, bVarArg, function);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#validateBase(nexj.core.scripting.object.ClassObject)
    */
   protected void validateBase(ClassObject base)
   {
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#validateAttribute(nexj.core.scripting.object.ClassObject, nexj.core.scripting.object.AttributeObject)
    */
   protected void validateAttribute(ClassObject classObject, AttributeObject attribute)
   {
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#validateMethod(nexj.core.scripting.object.ClassObject, nexj.core.scripting.object.MethodObject)
    */
   protected void validateMethod(ClassObject classObject, MethodObject method)
   {
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#sortInitializers(ClassObject, int[])
    */
   protected void sortInitializers(ClassObject classObject, int[] offsetArray)
   {
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#resolve(nexj.core.scripting.object.ClassObject)
    */
   protected void resolve(ClassObject classObject)
   {
      if (!(classObject instanceof MetaclassObject) &&
         !classObject.getSymbol().equals(getObjectClassSymbol()) &&
         classObject.getBaseCount() != 0)
      {
         ClassObject old = classObject.getClassObject();
         ClassObject base = classObject.getBase(0);
         MetaclassObject metaclass = base.getMetaclass();

         if (metaclass.getClass() != old.getClass())
         {
            classObject.setClassObject(metaclass.createMetaclass(classObject.getSymbol()));
            old.remove();
         }
      }
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createClass(nexj.core.scripting.Symbol)
    */
   public ClassObject createClass(Symbol symbol)
   {
      ClassEnvironment env = getEnvironment();
      ClassObject classObject = super.createClass(symbol);
      ClassObject object = env.findClass(getObjectClassSymbol());

      if (object != null && object != classObject)
      {
         classObject.addBase(object);
      }

      return classObject;
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#addObjectMembers(nexj.core.scripting.object.ClassObject)
    */
   protected void addObjectMembers(ClassObject classObject)
   {
      GenericObject.addMembers(classObject);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getObjectClassSymbol()
    */
   protected Symbol getObjectClassSymbol()
   {
      return BasicObject.CLASS_SYMBOL;
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getObjectBaseClassSymbol()
    */
   protected Symbol getObjectBaseClassSymbol()
   {
      return null;
   }

   /**
    * Initializes the object system.
    */
   public static void init()
   {
      if (!(getEnvironment().findClass(METACLASS_SYMBOL) instanceof BasicMetaclassObject))
      {
         BasicMetaclassObject metaclass = new BasicMetaclassObject();

         ClassObject.addMembers(metaclass);
         metaclass.define();

         ClassObject object = metaclass.defineClass(BasicObject.CLASS_SYMBOL);
         ClassObject attribute = metaclass.defineClass(AttributeObject.CLASS_SYMBOL);
         ClassObject method = metaclass.defineClass(MethodObject.CLASS_SYMBOL);

         metaclass.addBase(object);

         if (attribute.isForward())
         {
            attribute.setForward(false);
            AttributeObject.addMembers(attribute);
         }

         if (method.isForward())
         {
            method.setForward(false);
            MethodObject.addMembers(method);
         }
      }
   }
}
